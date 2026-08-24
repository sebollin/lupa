.es_clave <- function(datos, indices) {
  columnas <- lapply(indices, function(i) datos[[i]])
  if (any(vapply(columnas, anyNA, logical(1L)))) {
    return(FALSE)
  }
  if (length(indices) == 1L) {
    return(anyDuplicated(columnas[[1L]]) == 0L)
  }
  combinado <- as.data.frame(columnas, optional = TRUE, stringsAsFactors = FALSE)
  anyDuplicated(combinado) == 0L
}

.umbral_unicidad_casi_clave <- 0.9
.umbral_concentracion_casi_clave <- 0.5
.min_filas_casi_clave <- 100L

.resumen_tipo_candidato_clave <- function(x) {
  es_doble_fraccionable <- is.double(x) && !inherits(x, "integer64")
  n_fraccionarios_finitos <- if (es_doble_fraccionable) {
    valores_dobles <- tryCatch(as.double(x), error = function(e) NULL)
    if (is.null(valores_dobles)) NA_integer_ else {
      as.integer(sum(
        is.finite(valores_dobles) & valores_dobles != trunc(valores_dobles)
      ))
    }
  } else 0L
  list(
    tipo_almacenamiento = typeof(x),
    n_valores_fraccionarios_finitos = n_fraccionarios_finitos,
    es_candidato = !is.na(n_fraccionarios_finitos) &&
      n_fraccionarios_finitos == 0L
  )
}

# `disfrazados` es la mascara de los valores que dicen ausencia sin ser `NA`.
# Sin ella, una clave sana con treinta filas en `"SIN DATO"` se informaba como
# clave rota: esas treinta colisionan entre si, la concentracion da 1 y el
# hallazgo dice que la columna no sirve como identificador. La lectura correcta
# es la contraria -la clave esta bien y faltan treinta documentos-, y el error
# va en los dos sentidos, porque una clave con colisiones **reales** mas unos
# `"SIN DATO"` ve su concentracion diluida por debajo del umbral y se calla.
#
# La mascara ya estaba calculada seis lineas antes de esta llamada, y el propio
# paquete la usa para lo mismo en el detector de vocabulario.
.resumen_casi_clave <- function(
    x, umbral_unicidad = .umbral_unicidad_casi_clave,
    umbral_concentracion = .umbral_concentracion_casi_clave,
    min_filas = .min_filas_casi_clave, rol = NULL,
    tipo_implicito = NULL, disfrazados = NULL) {
  if (!is.null(disfrazados) && length(disfrazados) == length(x) &&
      is.logical(disfrazados) && any(disfrazados, na.rm = TRUE)) {
    x <- x[!(!is.na(disfrazados) & disfrazados)]
  }
  tipo_candidato <- .resumen_tipo_candidato_clave(x)
  if (is.null(tipo_implicito)) {
    tipo_implicito <- if (is.character(x) || is.factor(x)) {
      inferir_tipo(x)$tipo
    } else {
      .tipo_declarado(x)
    }
  }
  if (is.null(rol)) {
    rol <- .propuesta_escala(x, tipo_implicito)$rol
  }
  rol <- as.character(rol[[1L]])
  vacio <- list(
    es_casi_clave = FALSE, n_filas = length(x), n_distintos = NA_integer_,
    tasa_distintos = NA_real_, n_valores_colisionados = NA_integer_,
    n_filas_en_colision = NA_integer_, n_duplicados_excedentes = NA_integer_,
    concentracion_colisiones = NA_real_, valores = character(),
    frecuencias = integer(), indices = integer(),
    tipo_almacenamiento = tipo_candidato$tipo_almacenamiento,
    n_valores_fraccionarios_finitos =
      tipo_candidato$n_valores_fraccionarios_finitos,
    rol = rol, tipo_implicito = tipo_implicito,
    min_filas = min_filas,
    umbral_unicidad = umbral_unicidad,
    umbral_concentracion = umbral_concentracion
  )
  if (is.matrix(x) || is.list(x) || !length(x)) return(vacio)
  if (!isTRUE(tipo_candidato$es_candidato)) return(vacio)
  ausentes <- tryCatch(is.na(x), error = function(e) NULL)
  if (is.null(ausentes) || length(ausentes) != length(x) || any(ausentes)) {
    return(vacio)
  }
  indices_valor <- tryCatch(match(x, unique(x)), error = function(e) NULL)
  if (is.null(indices_valor) || anyNA(indices_valor)) return(vacio)
  frecuencias <- tabulate(indices_valor)
  n_distintos <- length(frecuencias)
  colisionados <- which(frecuencias > 1L)
  n_excedentes <- sum(frecuencias[colisionados] - 1L)
  concentracion <- if (n_excedentes > 0L) {
    max(frecuencias[colisionados] - 1L) / n_excedentes
  } else NA_real_
  tasa <- n_distintos / length(x)
  orden <- if (length(colisionados)) {
    order(-frecuencias[colisionados], colisionados)
  } else integer()
  colisionados <- colisionados[orden]
  valores_unicos <- unique(x)
  valores <- tryCatch(
    suppressWarnings(as.character(valores_unicos[colisionados])),
    error = function(e) rep(NA_character_, length(colisionados))
  )
  indices <- if (length(colisionados)) {
    which(indices_valor %in% colisionados)
  } else integer()
  list(
    es_casi_clave = length(x) >= min_filas &&
      !rol %in% c("fecha", "fecha-hora") &&
      n_excedentes > 0L && tasa >= umbral_unicidad &&
      concentracion >= umbral_concentracion,
    n_filas = length(x), n_distintos = n_distintos,
    tasa_distintos = tasa,
    n_valores_colisionados = length(colisionados),
    n_filas_en_colision = sum(frecuencias[colisionados]),
    n_duplicados_excedentes = n_excedentes,
    concentracion_colisiones = concentracion,
    valores = valores,
    frecuencias = as.integer(frecuencias[colisionados]),
    indices = as.integer(indices),
    tipo_almacenamiento = tipo_candidato$tipo_almacenamiento,
    n_valores_fraccionarios_finitos =
      tipo_candidato$n_valores_fraccionarios_finitos,
    rol = rol, tipo_implicito = tipo_implicito,
    min_filas = min_filas,
    umbral_unicidad = umbral_unicidad,
    umbral_concentracion = umbral_concentracion
  )
}

.evidencia_colisiones_casi_clave <- function(resumen, max_valores = 20L) {
  if (!length(resumen$valores)) return("")
  indices <- seq_len(min(length(resumen$valores), max_valores))
  evidencia <- paste(vapply(indices, function(i) {
    valor <- resumen$valores[[i]]
    if (is.na(valor)) valor <- "<no representable>"
    paste0(.escapar_texto_visible(valor), " (", resumen$frecuencias[[i]], ")")
  }, character(1L)), collapse = "; ")
  if (length(resumen$valores) > max_valores) {
    evidencia <- paste0(
      evidencia, "; ... ", length(resumen$valores) - max_valores,
      " valores no mostrados"
    )
  }
  evidencia
}

.resumen_clave_normalizada <- function(datos, indices, nombres, normalizacion) {
  valores <- lapply(indices, function(i) {
    x <- suppressWarnings(as.character(.texto_analizable(datos[[i]])$valores))
    x[is.na(x)] <- ""
    .normalizacion_aplicar(
      x, .normalizacion_para_columna(normalizacion, nombres[[i]])
    )
  })
  if (!length(valores)) return(list(unicidad = NA, distintos = NA_integer_))
  completos <- Reduce(`&`, lapply(indices, function(i) !is.na(datos[[i]])),
                      init = rep(TRUE, nrow(datos)))
  if (!any(completos)) return(list(unicidad = FALSE, distintos = 0L))
  combinado <- do.call(paste, c(lapply(valores, `[`, completos), sep = "\u001f"))
  list(
    unicidad = anyDuplicated(combinado) == 0L,
    distintos = length(unique(combinado))
  )
}

.pares_redundantes <- function(datos, indices_clave, nombres) {
  .pares_de_columnas_identicas(datos, indices_clave, nombres)
}

#' Detectar claves candidatas
#'
#' Busca primero claves simples y luego combinaciones mínimas de dos o tres
#' columnas. No prueba una combinación si ya contiene una clave candidata más
#' pequeña. Una clave exige ausencia de `NA` y unicidad en todas las filas.
#' Además informa columnas simples casi-clave cuando tienen al menos 100 filas,
#' al menos el 90 % de sus valores son distintos y un único valor concentra al
#' menos la mitad de los duplicados excedentes. La concentración evita confundir
#' texto libre de alta cardinalidad, con muchas colisiones dispersas, con una
#' clave dañada. Las variables con rol propuesto `fecha`, incluidas fecha-hora,
#' no se consideran casi-claves.
#' Los vectores `double` sólo son candidatos si ninguno de sus valores finitos
#' tiene parte fraccionaria. Esto conserva identificadores enteros importados
#' desde archivos de texto y excluye importes, coordenadas y otras medidas. Los
#' vectores `integer64` se tratan como enteros semánticos.
#'
#' Dos claves simples se marcan como redundantes cuando sus contenidos son
#' idénticos —incluidas clase, atributos, ausencias y representación exacta—,
#' aunque tengan nombres distintos. Las columnas matriciales o de lista no se
#' interpretan como claves. Los pares también quedan en el atributo
#' `claves_redundantes`.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param max_combinacion Máximo de columnas por combinación, entre 1 y 3.
#' @param normalizar Perfil de comparación. `NULL` hereda el perfil de
#'   `perfil`, pero las claves se siguen descubriendo por identidad exacta.
#' @param perfil Perfil producido por [perfilar()] para heredar la comparación.
#'
#' @return Data frame de claves candidatas y casi-claves con las columnas
#'   combinadas, cantidad de columnas, marcas de redundancia, `casi_clave`,
#'   `unicidad_exacta` y `unicidad_normalizada`. Las columnas de colisiones
#'   publican sus valores, frecuencias y la concentración observada. Las claves
#'   exactas conservan `casi_clave = FALSE`; una fila con `casi_clave = TRUE`
#'   es un diagnóstico que requiere corregir o confirmar, no una clave válida.
#' @export
#' @seealso [detectar_dependencias()], [detectar_relaciones()]
#'
#' @examples
#' detectar_claves(data.frame(id = 1:4, grupo = c("a", "a", "b", "b")))
detectar_claves <- function(datos, max_combinacion = 3, normalizar = NULL,
                            perfil = NULL) {
  .validar_datos_tabla(datos)
  .validar_perfil_de(perfil, datos)
  normalizacion_resuelta <- .resolver_normalizacion(normalizar, perfil)
  if (length(max_combinacion) != 1L || is.na(max_combinacion) ||
      max_combinacion < 1L || max_combinacion > 3L) {
    stop("`max_combinacion` debe ser un entero entre 1 y 3.", call. = FALSE)
  }
  nombres <- make.unique(names(datos))
  encontradas <- list()
  casi_encontradas <- list()
  k <- 0L
  analizables <- which(!vapply(datos, function(x) {
    is.list(x) || is.matrix(x) ||
      !isTRUE(.resumen_tipo_candidato_clave(x)$es_candidato)
  }, logical(1L)))
  limite <- min(floor(max_combinacion), length(analizables))

  if (nrow(datos) > 0L && limite > 0L) {
    for (tamano in seq_len(limite)) {
      combinaciones <- utils::combn(analizables, tamano, simplify = FALSE)
      for (combinacion in combinaciones) {
        contiene_clave <- any(vapply(encontradas, function(clave) {
          all(clave %in% combinacion)
        }, logical(1L)))
        if (!contiene_clave && .es_clave(datos, combinacion)) {
          k <- k + 1L
          encontradas[[k]] <- combinacion
        }
      }
    }
    casi_encontradas <- lapply(analizables, function(i) {
      resumen <- .resumen_casi_clave(datos[[i]])
      if (isTRUE(resumen$es_casi_clave)) {
        list(indices = i, resumen = resumen)
      } else NULL
    })
    casi_encontradas <- Filter(Negate(is.null), casi_encontradas)
  }

  indices_simples <- unlist(encontradas[lengths(encontradas) == 1L], use.names = FALSE)
  redundantes <- .pares_redundantes(datos, indices_simples, nombres)
  entradas <- c(
    lapply(encontradas, function(indices) {
      list(indices = indices, casi_clave = FALSE, resumen = NULL)
    }),
    lapply(casi_encontradas, function(x) {
      list(indices = x$indices, casi_clave = TRUE, resumen = x$resumen)
    })
  )
  if (!length(entradas)) {
    resultado <- data.frame(
      columnas = character(), n_columnas = integer(), n_filas = integer(),
      redundante = logical(), equivalente_a = character(),
      casi_clave = logical(),
      unicidad_exacta = logical(), unicidad_normalizada = logical(),
      n_distintos_exactos = integer(), n_distintos_normalizados = integer(),
      n_valores_colisionados = integer(), n_filas_en_colision = integer(),
      n_duplicados_excedentes = integer(),
      concentracion_colisiones = numeric(), colisiones = character(),
      stringsAsFactors = FALSE
    )
  } else {
    resultado <- do.call(rbind, lapply(entradas, function(entrada) {
      indices <- entrada$indices
      es_casi_clave <- entrada$casi_clave
      resumen <- entrada$resumen
      nombres_clave <- nombres[indices]
      relacionadas <- character()
      if (!es_casi_clave && length(indices) == 1L && nrow(redundantes)) {
        relacionadas <- c(
          redundantes$columna_2[redundantes$columna_1 == nombres_clave],
          redundantes$columna_1[redundantes$columna_2 == nombres_clave]
        )
      }
      normalizada <- .resumen_clave_normalizada(
        datos, indices, nombres, normalizacion_resuelta
      )
      exactos <- if (es_casi_clave) {
        resumen$n_distintos
      } else if (nrow(datos)) {
        completos <- !apply(is.na(as.data.frame(datos[indices])), 1L, any)
        if (length(indices) == 1L) {
          length(unique(datos[[indices[[1L]]]][completos]))
        } else {
          combinado <- do.call(paste, c(lapply(datos[indices], `[`, completos),
                                         sep = "\u001f"))
          length(unique(combinado))
        }
      } else 0L
      data.frame(
        columnas = .pegar_nombres(nombres_clave),
        n_columnas = length(indices),
        n_filas = nrow(datos),
        redundante = length(relacionadas) > 0L,
        equivalente_a = paste(relacionadas, collapse = ", "),
        casi_clave = es_casi_clave,
        unicidad_exacta = !es_casi_clave,
        unicidad_normalizada = normalizada$unicidad,
        n_distintos_exactos = exactos,
        n_distintos_normalizados = normalizada$distintos,
        n_valores_colisionados = if (es_casi_clave) {
          resumen$n_valores_colisionados
        } else 0L,
        n_filas_en_colision = if (es_casi_clave) {
          resumen$n_filas_en_colision
        } else 0L,
        n_duplicados_excedentes = if (es_casi_clave) {
          resumen$n_duplicados_excedentes
        } else 0L,
        concentracion_colisiones = if (es_casi_clave) {
          resumen$concentracion_colisiones
        } else NA_real_,
        colisiones = if (es_casi_clave) {
          .evidencia_colisiones_casi_clave(resumen)
        } else "",
        stringsAsFactors = FALSE
      )
    }))
    rownames(resultado) <- NULL
  }
  class(resultado) <- c("claves_candidatas", "data.frame")
  attr(resultado, "claves_redundantes") <- redundantes
  attr(resultado, "normalizacion") <- .normalizacion_resumen(normalizacion_resuelta)
  resultado
}

.valores_relacion <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  .texto_analizable(x)$valores
}

.familia_relacion <- function(x) {
  if (inherits(x, "POSIXt")) return("fecha-hora")
  if (inherits(x, "Date")) return("fecha")
  if (inherits(x, "integer64") || is.integer(x) || is.double(x)) {
    return("numerica")
  }
  if (is.factor(x) || is.character(x)) return("texto")
  if (is.logical(x)) return("logica")
  .tipo_declarado(x)
}

.rango_relacion <- function(x, familia) {
  if (!familia %in% c("numerica", "fecha", "fecha-hora")) {
    return(c(minimo = NA_real_, maximo = NA_real_))
  }
  valores <- tryCatch(as.numeric(x[!is.na(x)]), error = function(e) NULL)
  if (is.null(valores) || !length(valores) || any(!is.finite(valores))) {
    return(c(minimo = NA_real_, maximo = NA_real_))
  }
  c(minimo = min(valores), maximo = max(valores))
}

.resumir_columna_relacion <- function(x, muestra) {
  valores <- .valores_relacion(x)
  valores_muestra <- .muestrear_vector(valores, muestra)$valores
  valores_completos <- valores[!is.na(valores)]
  familia <- .familia_relacion(x)
  # `unique()` se calculaba dos veces sobre la columna entera, una por cada
  # campo, y esta funcion corre una vez por columna de cada una de las dos
  # tablas, asi que el sobrecosto se multiplica por el ancho. Medido sobre
  # 500.000 filas con 200.000 distintos: 0,028 s contra 0,014 s por columna.
  unicos <- unique(valores_completos)
  list(
    muestra = valores_muestra[!is.na(valores_muestra)],
    unicos = unicos,
    unico = anyDuplicated(valores_completos) == 0L,
    n_distintos = length(unicos),
    familia = familia,
    rango = .rango_relacion(x, familia)
  )
}

.validar_columnas_candidatas_relacion <- function(datos, columnas, lado) {
  nombres <- if (is.character(datos)) datos else make.unique(names(datos))
  if (is.null(columnas)) return(nombres)
  if (!is.character(columnas) || !length(columnas) || anyNA(columnas) ||
      any(!nzchar(columnas))) {
    stop(
      "`columnas_candidatas` para ", lado,
      " debe ser un vector de nombres no vacios.", call. = FALSE
    )
  }
  desconocidas <- setdiff(columnas, nombres)
  if (length(desconocidas)) {
    stop(
      "`columnas_candidatas` nombra columnas inexistentes en ", lado, ": ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  unique(columnas)
}

.resolver_columnas_candidatas_relacion <- function(columnas, n1, n2) {
  if (is.null(columnas)) return(list(tabla1 = n1, tabla2 = n2))
  if (!is.list(columnas) || length(columnas) != 2L) {
    stop(
      "`columnas_candidatas` debe ser una lista con una entrada para cada tabla,",
      " en el orden `tabla1`, `tabla2`.", call. = FALSE
    )
  }
  nombres <- names(columnas)
  if (!is.null(nombres) && all(c("tabla1", "tabla2") %in% nombres)) {
    columnas <- columnas[c("tabla1", "tabla2")]
  }
  list(
    tabla1 = .validar_columnas_candidatas_relacion(
      n1, columnas[[1L]], "tabla1"
    ),
    tabla2 = .validar_columnas_candidatas_relacion(
      n2, columnas[[2L]], "tabla2"
    )
  )
}

# Hay dos clases de poda y no se pueden tratar igual.
#
# Una es **cierta**: dos columnas de la misma familia con rangos numericos
# disjuntos no comparten ningun valor, y eso se sabe sin comparar. La respuesta
# es la misma que daria la comparacion —cero comunes, cobertura cero—, asi que
# saltearla es puro ahorro y la fila sale igual que siempre.
#
# La otra **no lo es**. Familias distintas parece decisivo y no lo es: una
# columna de texto puede guardar `"2020-01-05"` y coincidir con una de fecha, y
# una logica coincide con `"TRUE"` guardado como texto. Y la cardinalidad
# imposible no dice que no haya coincidencias: dice que no alcanzan el umbral,
# que es otra cosa. Podar por esas dos cambia lo que el objeto informa, asi que
# no se hace por omision, y cuando se pide, la fila sale como no comparada con
# su motivo en vez de desaparecer.
.poda_cierta_relacion <- function(x, y) {
  if (!identical(x$familia, y$familia)) return(NULL)
  rango_x <- x$rango
  rango_y <- y$rango
  if (!all(is.finite(c(rango_x, rango_y)))) return(NULL)
  if (rango_x[["maximo"]] >= rango_y[["minimo"]] &&
      rango_y[["maximo"]] >= rango_x[["minimo"]]) {
    return(NULL)
  }
  list(
    motivo = "rangos_disjuntos",
    detalle = paste0(
      "[", rango_x[["minimo"]], ", ", rango_x[["maximo"]], "] y [",
      rango_y[["minimo"]], ", ", rango_y[["maximo"]], "]"
    )
  )
}

.poda_relacion <- function(x, y, umbral_cobertura) {
  if (!identical(x$familia, y$familia) &&
      !all(c(x$familia, y$familia) %in% "numerica")) {
    return(list(
      motivo = "tipos_incompatibles",
      detalle = paste0("familias ", x$familia, " y ", y$familia)
    ))
  }
  if (umbral_cobertura > 0 && x$n_distintos > 0L && y$n_distintos > 0L &&
      y$n_distintos * umbral_cobertura > x$n_distintos) {
    return(list(
      motivo = "cardinalidades_imposibles",
      detalle = paste0(
        y$n_distintos, " distintos en tabla2 contra ", x$n_distintos,
        " en tabla1 con umbral ", umbral_cobertura
      )
    ))
  }
  NULL
}

.podas_relacion_vacias <- function() {
  data.frame(
    columna_tabla1 = character(), columna_tabla2 = character(),
    motivo = character(), detalle = character(), stringsAsFactors = FALSE
  )
}

#' Detectar relaciones entre dos tablas
#'
#' Examina los pares de columnas declarados y describe su cardinalidad a partir
#' de la unicidad completa de cada lado. La cobertura `tabla1_en_tabla2` es la
#' proporción de valores no ausentes de la primera columna que existe en la
#' segunda; la cobertura inversa se informa de forma simétrica. Así se puede
#' escoger la dirección PK/FK sin imponerla de antemano.
#'
#' `columnas_candidatas` permite evitar la exploración de columnas que el usuario
#' sabe que no pueden participar. El costo crece con el producto de anchos: dos
#' tablas de treinta columnas son novecientas combinaciones por par de tablas, y
#' declarar cuáles pueden participar es lo que lo vuelve manejable.
#'
#' **Hay dos clases de poda y el paquete no las trata igual.** Dos columnas de
#' la misma familia con rangos numéricos disjuntos no comparten ningún valor, y
#' eso se sabe sin comparar: la fila sale como siempre —`sin_coincidencias`, con
#' cobertura cero— y la comparación se ahorra. Esa poda está siempre activa
#' porque no cambia lo que el objeto informa.
#'
#' Las otras dos sí lo cambiarían. Familias distintas parece decisivo y no lo
#' es: una columna de texto puede guardar `"2020-01-05"` y coincidir con una de
#' fecha. Y una cardinalidad imposible no dice que no haya coincidencias, dice
#' que no alcanzan `umbral_cobertura`, que es otra cosa. Por eso van detrás de
#' `podar = TRUE`, y cuando se aplican **el par no desaparece**: sale con
#' `cardinalidad = "sin_comparar"`, coberturas `NA` y su motivo en `motivo_poda`.
#' Un par que no se evaluó no es un par sin relación.
#'
#' Todas las podas, de las dos clases, quedan además en el atributo `podas` con
#' su motivo y su detalle.
#'
#' Cuando una tabla supera `muestra`, la función estima cada cobertura con una
#' muestra sistemática del lado que se verifica y conserva completo el conjunto
#' de referencia. La cardinalidad y la cantidad de valores comunes siempre se
#' calculan con ambas columnas completas.
#'
#' @param tabla1,tabla2 Objetos que heredan de `data.frame`.
#' @param muestra Máximo de filas del lado verificado que se usan para estimar
#'   cada cobertura. El muestreo es sistemático y reproducible; el lado de
#'   referencia no se muestrea. Use `Inf` para calcular todo sin muestreo.
#' @param columnas_candidatas Lista de dos vectores de nombres, para `tabla1` y
#'   `tabla2`, que declara las columnas que pueden participar. `NULL` conserva
#'   la exploración completa por compatibilidad.
#' @param umbral_cobertura Umbral usado por la poda de cardinalidades imposibles.
#' @param podar Si se aplican las podas que cambiarían lo informado —tipos
#'   incompatibles y cardinalidades imposibles—. `FALSE` por omisión: sólo se
#'   aplica la poda cierta, que no cambia ninguna fila.
#' @param tope_memoria_mb Presupuesto de memoria para las filas comparadas, en
#'   megabytes. Las combinaciones pendientes se declaran como podas cuando se
#'   alcanza; `Inf` no limita el procesamiento.
#'
#' @return Data frame con columnas comparadas, cardinalidad, coincidencias,
#'   coberturas de integridad referencial en ambas direcciones y `motivo_poda`,
#'   que sólo tiene valor en los pares no comparados. Los atributos
#'   `filas_totales`, `filas_analizadas` y `muestreado` documentan el muestreo;
#'   `podas`, `n_pares_totales`, `n_pares_comparados` y `n_pares_podados`
#'   documentan qué se comparó y qué no.
#' @export
#' @seealso [detectar_claves()], [referencial()], [proponer_modelo()]
#'
#' @examples
#' personas <- data.frame(id = 1:3)
#' tramites <- data.frame(persona_id = c(1, 1, 3, 4))
#' detectar_relaciones(personas, tramites)
detectar_relaciones <- function(tabla1, tabla2, muestra = 1e5,
                                columnas_candidatas = NULL,
                                umbral_cobertura = 0.9,
                                podar = FALSE,
                                tope_memoria_mb = Inf) {
  if (!inherits(tabla1, "data.frame") || !inherits(tabla2, "data.frame")) {
    stop("`tabla1` y `tabla2` deben heredar de data.frame.", call. = FALSE)
  }
  if (!is.logical(podar) || length(podar) != 1L || is.na(podar)) {
    stop("`podar` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.numeric(umbral_cobertura) || length(umbral_cobertura) != 1L ||
      is.na(umbral_cobertura) || umbral_cobertura < 0 ||
      umbral_cobertura > 1) {
    stop("`umbral_cobertura` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(tope_memoria_mb) || length(tope_memoria_mb) != 1L ||
      is.na(tope_memoria_mb) || tope_memoria_mb < 0) {
    stop("`tope_memoria_mb` debe ser un numero no negativo.", call. = FALSE)
  }
  limite_muestra <- .validar_muestra(muestra)
  nombres_1 <- make.unique(names(tabla1))
  nombres_2 <- make.unique(names(tabla2))
  candidatas <- .resolver_columnas_candidatas_relacion(
    columnas_candidatas, nombres_1, nombres_2
  )
  indices_1 <- match(candidatas$tabla1, nombres_1)
  indices_2 <- match(candidatas$tabla2, nombres_2)
  columnas_1 <- lapply(indices_1, function(i) {
    .resumir_columna_relacion(tabla1[[i]], limite_muestra)
  })
  columnas_2 <- lapply(indices_2, function(i) {
    .resumir_columna_relacion(tabla2[[i]], limite_muestra)
  })
  filas <- list()
  podas <- list()
  memoria_mb <- 0
  comparadas <- 0L
  presupuesto_agotado <- FALSE

  for (i in seq_along(indices_1)) {
    x <- columnas_1[[i]]
    for (j in seq_along(indices_2)) {
      y <- columnas_2[[j]]
      nombre_1 <- candidatas$tabla1[[i]]
      nombre_2 <- candidatas$tabla2[[j]]
      poda <- if (isTRUE(podar)) {
        .poda_relacion(x, y, umbral_cobertura)
      } else NULL
      if (!is.null(poda)) {
        podas[[length(podas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          motivo = poda$motivo, detalle = poda$detalle,
          stringsAsFactors = FALSE
        )
        # El par no desaparece: sale declarado como no comparado. Un par que no
        # se evaluo no es un par sin relacion.
        filas[[length(filas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          cardinalidad = "sin_comparar", n_valores_comunes = NA_integer_,
          cobertura_tabla1_en_tabla2 = NA_real_,
          cobertura_tabla2_en_tabla1 = NA_real_,
          motivo_poda = poda$motivo, stringsAsFactors = FALSE
        )
        next
      }
      cierta <- .poda_cierta_relacion(x, y)
      if (!is.null(cierta)) {
        podas[[length(podas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          motivo = cierta$motivo, detalle = cierta$detalle,
          stringsAsFactors = FALSE
        )
        # Rangos disjuntos de la misma familia: la respuesta se conoce sin
        # comparar, y es la misma. Se informa igual que siempre.
        filas[[length(filas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          cardinalidad = "sin_coincidencias", n_valores_comunes = 0L,
          cobertura_tabla1_en_tabla2 = if (length(x$muestra)) 0 else NA_real_,
          cobertura_tabla2_en_tabla1 = if (length(y$muestra)) 0 else NA_real_,
          motivo_poda = NA_character_, stringsAsFactors = FALSE
        )
        comparadas <- comparadas + 1L
        next
      }
      if (is.finite(tope_memoria_mb) && memoria_mb >= tope_memoria_mb) {
        presupuesto_agotado <- TRUE
        podas[[length(podas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          motivo = "presupuesto_memoria_agotado",
          detalle = paste0("tope_memoria_mb = ", tope_memoria_mb),
          stringsAsFactors = FALSE
        )
        next
      }
      n_valores_comunes <- length(intersect(x$unicos, y$unicos))
      unico_x <- x$unico
      unico_y <- y$unico
      cardinalidad <- if (!n_valores_comunes) {
        "sin_coincidencias"
      } else if (unico_x && unico_y) {
        "1:1"
      } else if (unico_x && !unico_y) {
        "1:m"
      } else if (!unico_x && unico_y) {
        "m:1"
      } else {
        "m:m"
      }
      fila <- data.frame(
        columna_tabla1 = nombre_1,
        columna_tabla2 = nombre_2,
        cardinalidad = cardinalidad,
        n_valores_comunes = n_valores_comunes,
        cobertura_tabla1_en_tabla2 = if (length(x$muestra)) {
          mean(x$muestra %in% y$unicos)
        } else {
          NA_real_
        },
        cobertura_tabla2_en_tabla1 = if (length(y$muestra)) {
          mean(y$muestra %in% x$unicos)
        } else {
          NA_real_
        },
        motivo_poda = NA_character_,
        stringsAsFactors = FALSE
      )
      filas[[length(filas) + 1L]] <- fila
      comparadas <- comparadas + 1L
      memoria_mb <- memoria_mb + as.numeric(utils::object.size(fila)) / 1024^2
    }
  }

  if (length(filas)) {
    resultado <- do.call(rbind, filas)
    rownames(resultado) <- NULL
  } else {
    resultado <- data.frame(
      columna_tabla1 = character(), columna_tabla2 = character(),
      cardinalidad = character(), n_valores_comunes = integer(),
      cobertura_tabla1_en_tabla2 = numeric(),
      cobertura_tabla2_en_tabla1 = numeric(), motivo_poda = character(),
      stringsAsFactors = FALSE
    )
  }
  class(resultado) <- c("relaciones_detectadas", "data.frame")
  attr(resultado, "filas_totales") <- c(
    tabla1 = nrow(tabla1), tabla2 = nrow(tabla2)
  )
  attr(resultado, "filas_analizadas") <- c(
    tabla1 = min(nrow(tabla1), limite_muestra),
    tabla2 = min(nrow(tabla2), limite_muestra)
  )
  attr(resultado, "muestreado") <- c(
    tabla1 = nrow(tabla1) > limite_muestra,
    tabla2 = nrow(tabla2) > limite_muestra
  )
  attr(resultado, "podas") <- if (length(podas)) {
    do.call(rbind, podas)
  } else {
    .podas_relacion_vacias()
  }
  attr(resultado, "n_pares_totales") <- length(indices_1) * length(indices_2)
  attr(resultado, "n_pares_comparados") <- comparadas
  attr(resultado, "n_pares_podados") <- length(podas)
  attr(resultado, "presupuesto_memoria_agotado") <- presupuesto_agotado
  attr(resultado, "memoria_resultado_mb") <- memoria_mb
  resultado
}
