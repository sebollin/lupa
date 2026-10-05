# `integer64` guarda cada entero en los bits de un doble, y dos operaciones de
# base no lo saben: bit64 no registra `anyDuplicated()`, y
# `stats::complete.cases()` no despacha. Las dos leen los dobles crudos, y el
# patron de bits de todo entero de -1 a -4503599627370495 -y de los de
# 9218868437227405313 para arriba- es un NaN: `complete.cases()` lo lee ausente
# y `anyDuplicated()` iguala dos cualesquiera. Medido en la ronda 25:
# `detectar_claves()` no proponia `id = -1, -2, -3` -y `sugerir_clave()` si-,
# `referencial()` lo rechazaba por "valores ausentes", la correctitud borraba
# en silencio la fila que FALLABA, y `perfilar()` publicaba `filas_completas =
# 0` y dos columnas distintas como identicas.
#
# Se compara su texto, que es exacto: es lo que ya hacen `.texto_identidad()` y
# `.codigos_filas_exactos()` de la remediacion. `is.na()` y `anyNA()` si
# despachan, y el texto conserva el ausente.
.integer64_como_texto <- function(datos) {
  for (i in seq_along(datos)) {
    if (inherits(datos[[i]], "integer64")) {
      datos[[i]] <- .texto_identidad(datos[[i]])
    }
  }
  datos
}

.filas_completas <- function(datos) {
  stats::complete.cases(.integer64_como_texto(datos))
}

.es_clave <- function(datos, indices) {
  columnas <- lapply(indices, function(i) datos[[i]])
  if (any(vapply(columnas, .es_columna_compuesta, logical(1L)))) {
    return(FALSE)
  }
  if (any(vapply(columnas, anyNA, logical(1L)))) {
    return(FALSE)
  }
  columnas <- .integer64_como_texto(columnas)
  if (length(indices) == 1L) {
    return(anyDuplicated(columnas[[1L]]) == 0L)
  }
  combinado <- as.data.frame(columnas, optional = TRUE, stringsAsFactors = FALSE)
  anyDuplicated(combinado) == 0L
}

# La casi-clave de `detectar_claves()` con la MISMA mascara de faltantes
# disfrazados que `perfilar()` usa por omision -sus sentinelas numericos y sin
# cadenas declaradas-, calculada por la misma funcion. Ver
# `.faltantes_disfrazados_columna()`. Devuelve tambien el tipo inferido, que
# `.resumen_casi_clave()` calculaba por su cuenta.
.contexto_casi_clave <- function(x, perfil = NULL) {
  preparacion <- .texto_analizable(x)
  x_analisis <- preparacion$valores
  n <- length(x_analisis)
  if (!n) return(list(tipo = NULL, disfrazados = NULL))
  muestra <- min(n, 1e5)
  inferencia <- .inferir_tipo_interno(
    preparacion$valores_identidad, muestra = muestra, conservar_cache = TRUE
  )
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) {
    formatos <- detectar_formatos_fecha(x_analisis, muestra = muestra)
  }
  secuencia <- .resumen_secuencia_entera(x_analisis, inferencia, formatos)
  # Con un perfil, la politica que ese perfil declaro; sin el, la de
  # `perfilar()` por omision.
  sentinelas <- .numeros_na_locales
  cadenas <- NULL
  if (inherits(perfil, "perfil") && is.list(perfil$meta)) {
    if (!is.null(perfil$meta$sentinelas_numericos)) {
      sentinelas <- perfil$meta$sentinelas_numericos
    }
    cadenas <- perfil$meta$cadenas_ausencia
  }
  disfrazados <- .faltantes_disfrazados_columna(
    x_analisis, inferencia, formatos, secuencia, sentinelas,
    .sentinelas_numericos_declarados(sentinelas), cadenas, rep(TRUE, n)
  )
  list(tipo = inferencia$tipo, disfrazados = disfrazados$mascara)
}

.umbral_unicidad_casi_clave <- 0.9
.umbral_concentracion_casi_clave <- 0.5
.min_filas_casi_clave <- 100L

.resumen_tipo_candidato_clave <- function(x) {
  if (.es_columna_compuesta(x)) {
    return(list(
      tipo_almacenamiento = typeof(x),
      n_valores_fraccionarios_finitos = NA_integer_,
      es_candidato = FALSE
    ))
  }
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
    es_casi_clave = FALSE,
    n_filas = if (.es_columna_compuesta(x)) NROW(x) else length(x),
    n_distintos = NA_integer_,
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
  if (.es_columna_compuesta(x) || is.list(x) || !length(x)) return(vacio)
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

# Una clave compuesta se pega con un separador que ningun valor puede producir.
# `.clave_bytes()` deja pasar U+001F tal cual, asi que un valor que lo traia
# fingia un separador: con `c1 = "a<US>b", c2 = "c"` y `c1 = "a", c2 = "b<US>c"`,
# cuatro filas distintas se contaban tres. Se escapa como `\x1f`, que no se
# confunde con un valor porque `.clave_bytes()` ya duplico las barras invertidas.
# Es la receta de `duplicados-aproximados.R`, escrita una vez para los dos sitios
# de esta funcion.
.pegar_clave_compuesta <- function(valores) {
  .clave_bytes(do.call(paste, c(
    unname(lapply(valores, function(x) {
      gsub("\u001f", "\\x1f", .clave_bytes(x), fixed = TRUE)
    })),
    sep = "\u001f"
  )))
}

.resumen_clave_normalizada <- function(datos, indices, nombres, normalizacion) {
  valores <- lapply(indices, function(i) {
    columna <- datos[[i]]
    # Dos textos con bytes invalidos DISTINTOS -lo que deja un `read.csv()` de un
    # archivo latin1 sin declarar- se volvian `NA` y despues `""`: se contaban
    # como uno solo, e igual a un vacio. Se escapan antes, como en
    # `duplicados-aproximados.R`.
    if (is.character(columna)) columna <- .clave_bytes(columna)
    x <- suppressWarnings(as.character(.texto_analizable(columna)$valores))
    x[is.na(x)] <- ""
    .normalizacion_aplicar(
      x, .normalizacion_para_columna(normalizacion, nombres[[i]])
    )
  })
  if (!length(valores)) return(list(unicidad = NA, distintos = NA_integer_))
  completos <- Reduce(`&`, lapply(indices, function(i) !is.na(datos[[i]])),
                      init = rep(TRUE, nrow(datos)))
  if (!any(completos)) return(list(unicidad = FALSE, distintos = 0L))
  # `unname()`: `lapply` conserva los nombres, y un nombre de columna que
  # coincida con un formal de `paste` -`sep`, `recycle0`- aborta la corrida.
  combinado <- .pegar_clave_compuesta(lapply(valores, function(x) x[completos]))
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
#' Los faltantes disfrazados —`"SIN DATO"`, `-999`— no cuentan como colisiones:
#' se reconocen con la misma máscara que [perfilar()], con su política por
#' omisión o con la del `perfil` que se pase, y la casi-clave se mide sin ellos
#' —sus distintos, exactos y normalizados, y sus colisiones—. Una clave sana con
#' treinta documentos en `"SIN DATO"` no es una clave rota: le faltan treinta
#' documentos. `n_filas` sigue siendo el de la tabla.
#' Los vectores `double` sólo son candidatos si ninguno de sus valores finitos
#' tiene parte fraccionaria. Esto conserva identificadores enteros importados
#' desde archivos de texto y excluye importes, coordenadas y otras medidas. Los
#' vectores `integer64` se tratan como enteros semánticos.
#'
#' Dos claves simples se marcan como redundantes cuando sus contenidos son
#' idénticos —incluidas clase, atributos, ausencias y representación exacta—,
#' aunque tengan nombres distintos. Las columnas compuestas —matrices o
#' arreglos de más de una dimensión— y las de lista no se interpretan como
#' claves. Los pares también quedan en el atributo
#' `claves_redundantes`.
#'
#' Una columna cuyo nombre se repite en la tabla no se analiza: una clave se
#' publica por el nombre de sus columnas, y ese nombre no diría cuál de las dos
#' identifica. Se avisa, y los nombres quedan en el atributo
#' `columnas_nombre_repetido`.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param max_combinacion Máximo de columnas por combinación, entre 1 y 3.
#' @param normalizar Perfil de comparación. `NULL` hereda el perfil de
#'   `perfil`, pero las claves se siguen descubriendo por identidad exacta.
#' @param perfil Perfil producido por [perfilar()] para heredar la comparación
#'   y la política de faltantes disfrazados —`sentinelas_numericos` y
#'   `cadenas_ausencia`—.
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
  datos <- .tabla_base(datos)
  normalizacion_resuelta <- .resolver_normalizacion(normalizar, perfil)
  if (length(max_combinacion) != 1L || is.na(max_combinacion) ||
      max_combinacion < 1L || max_combinacion > 3L) {
    stop("`max_combinacion` debe ser un entero entre 1 y 3.", call. = FALSE)
  }
  # Los nombres publicados son los de la tabla. La forma unica queda reservada
  # para nombres de listas internas; agregar el sufijo de `make.unique()` a una
  # clave publica puede inventar una columna que no existe.
  nombres <- names(datos)
  encontradas <- list()
  casi_encontradas <- list()
  k <- 0L
  # Una columna cuyo nombre se repite no entra: la clave se publica por nombre, y
  # ese nombre no dice cual de las dos identifica. Se declara y se avisa.
  repetidos <- .nombres_repetidos(nombres)
  analizables <- which(!repetidos & !vapply(datos, function(x) {
    is.list(x) || .es_columna_compuesta(x) ||
      !isTRUE(.resumen_tipo_candidato_clave(x)$es_candidato)
  }, logical(1L)))
  limite <- min(floor(max_combinacion), length(analizables))

  if (nrow(datos) > 0L && limite > 0L) {
    for (tamano in seq_len(limite)) {
      # `combn()` sobre un solo numero `k` combina `seq_len(k)`: con una sola
      # columna analizable en la posicion 3, probaba como clave las columnas 1 y
      # 2, que el filtro habia excluido -un importe con decimales, una lista-, y
      # la respuesta dependia del orden de las columnas.
      combinaciones <- if (length(analizables) == 1L) {
        list(analizables)
      } else {
        utils::combn(analizables, tamano, simplify = FALSE)
      }
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
    simples <- unlist(encontradas[lengths(encontradas) == 1L])
    casi_encontradas <- lapply(analizables, function(i) {
      # La mascara cuesta una inferencia de tipo: no se paga donde la
      # casi-clave no puede salir -una clave exacta no tiene colisiones, y con
      # menos de `.min_filas_casi_clave` filas no se evalua-.
      columna <- if (i %in% simples || NROW(datos[[i]]) < .min_filas_casi_clave) {
        list(tipo = NULL, disfrazados = NULL)
      } else {
        .contexto_casi_clave(datos[[i]], perfil)
      }
      resumen <- .resumen_casi_clave(
        datos[[i]], tipo_implicito = columna$tipo,
        disfrazados = columna$disfrazados
      )
      if (isTRUE(resumen$es_casi_clave)) {
        list(indices = i, resumen = resumen, disfrazados = columna$disfrazados)
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
      list(
        indices = x$indices, casi_clave = TRUE, resumen = x$resumen,
        disfrazados = x$disfrazados
      )
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
          redundantes$columna_2[
            .nombres_para_operar(redundantes$columna_1) %in%
              .nombres_para_operar(nombres_clave)
          ],
          redundantes$columna_1[
            .nombres_para_operar(redundantes$columna_2) %in%
              .nombres_para_operar(nombres_clave)
          ]
        )
      }
      # Una casi-clave se mide sin sus faltantes disfrazados, y la unicidad
      # normalizada de la misma fila tambien: contada sobre la columna entera
      # daba mas distintos normalizados que exactos.
      filas_medidas <- if (es_casi_clave && length(entrada$disfrazados)) {
        !(!is.na(entrada$disfrazados) & entrada$disfrazados)
      } else rep(TRUE, nrow(datos))
      normalizada <- .resumen_clave_normalizada(
        datos[filas_medidas, , drop = FALSE], indices, nombres,
        normalizacion_resuelta
      )
      exactos <- if (es_casi_clave) {
        resumen$n_distintos
      } else if (nrow(datos)) {
        seleccion <- .seleccionar_columnas(datos, indices)
        completos <- !apply(.matriz_ausentes(seleccion), 1L, any)
        if (length(indices) == 1L) {
          length(unique(datos[[indices[[1L]]]][completos]))
        } else {
          combinado <- .pegar_clave_compuesta(
            lapply(seleccion, function(x) x[completos])
          )
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
  sin_analizar <- nombres[repetidos]
  sin_analizar <- sin_analizar[!duplicated(.nombres_para_operar(sin_analizar))]
  attr(resultado, "columnas_nombre_repetido") <- sin_analizar
  if (length(sin_analizar)) {
    warning(
      "No se buscaron claves en ", length(sin_analizar), " nombre",
      if (length(sin_analizar) > 1L) "s" else "", " de columna repetido",
      if (length(sin_analizar) > 1L) "s" else "", " (",
      paste0("`", .marcar_para_exhibir(sin_analizar), "`", collapse = ", "),
      "): una clave se nombra por sus columnas, y ese nombre no dice cual es. ",
      "Renombrarlas -por ejemplo con `make.unique()`- y volver a buscar.",
      call. = FALSE
    )
  }
  resultado
}

.valores_relacion <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(.escritura_exacta_temporal(
      format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"), as.numeric(x)
    ))
  }
  if (inherits(x, "Date")) {
    return(.escritura_exacta_temporal(format(x, "%Y-%m-%d"), as.numeric(x)))
  }
  analisis <- .texto_analizable(x)
  valores <- analisis$valores
  if (!is.character(valores)) return(valores)
  claves <- .nombres_para_operar(valores)
  # Un texto con bytes invalidos SIN declarar -lo que deja un `read.csv()` de un
  # archivo latin1- sale de `.texto_analizable()` como `NA`, y aca se compara:
  # la relacion lo sacaba del denominador, el referencial hacia del `NA` un
  # nivel -todo invalido del objetivo casaba con cualquier invalido del padron-
  # y las dependencias juntaban los invalidos en un solo grupo. Medido en la
  # ronda 25: el padron {Ana, Mar<ed>a} declaraba conforme a `Jos<e9>`, y dos
  # tablas con una sola fila en comun publicaban coberturas 1 y 1.
  #
  # Es el mismo arreglo que `.resumen_clave_normalizada()`: lo que no se puede
  # leer como texto se compara por sus bytes, que es lo que hacen `%in%` y
  # `unique()` de R. `.nombres_para_operar()` ya tiene esa clave -inyectiva y
  # reservada, `<lupa-byte:...>`- y se le pasa el valor ORIGINAL.
  invalidos <- which(analisis$invalidos)
  if (length(invalidos)) {
    claves[invalidos] <- .nombres_para_operar(as.character(x)[invalidos])
  }
  claves
}

# Un instante se escribia al segundo y una fecha al dia: dos instantes del mismo
# segundo eran el mismo valor. Y la poda por rangos disjuntos usa el numero sin
# truncar, asi que la respuesta dependia de los vecinos: medido en la ronda 25,
# `a = t0 + 0.2` contra `b = t0 + 0.7` salia `sin_coincidencias` por la poda, y
# agregando `t0 + 0.9` a `a` -el mismo valor de `b`- salia `m:1` con cobertura
# 1. Las dependencias agrupaban los dos instantes de cada segundo y no omitian
# una columna que es clave, y el referencial declaraba conforme un instante que
# el padron no tenia.
#
# Se conserva la escritura al segundo -o al dia- cuando no hay fraccion, que es
# como un texto puede coincidir con una fecha, y si la hay se le agrega la
# fraccion EXACTA: `x - floor(x)` no redondea, y `format()` escribe el piso, asi
# que el segundo mas su fraccion vuelve al valor. `%OS6` no alcanza: funde dos
# instantes a un microsegundo -medido, `t0` y `t0 + 1e-6` dan la misma cadena-.
.escritura_exacta_temporal <- function(texto, numero) {
  fraccion <- numero - floor(numero)
  con_fraccion <- !is.na(texto) & is.finite(numero) & fraccion != 0
  if (any(con_fraccion)) {
    etiqueta <- .etiqueta_numero_reversible(fraccion[con_fraccion])
    # Una fraccion chica sale con exponente -`9.5367431640625e-07`-; se escribe
    # en decimal corriendo la coma, sin redondear ninguna cifra.
    con_exponente <- grepl("e", etiqueta, fixed = TRUE)
    if (any(con_exponente)) {
      etiqueta[con_exponente] <- vapply(
        strsplit(etiqueta[con_exponente], "e", fixed = TRUE),
        function(partes) {
          cifras <- sub(".", "", partes[[1L]], fixed = TRUE)
          ceros <- -as.integer(partes[[2L]]) - 1L
          paste0("0.", strrep("0", max(ceros, 0L)), cifras)
        }, character(1L)
      )
    }
    texto[con_fraccion] <- paste0(
      texto[con_fraccion], sub("^0[.]", ".", etiqueta)
    )
  }
  texto
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

.rango_relacion_sin_evidencia <- function() {
  c(minimo = NA_real_, maximo = NA_real_)
}

.resumir_columna_relacion <- function(x, muestra, rango = NULL,
                                      origen_rango = NULL) {
  # Por su valor, como el camino referencial: el mismo numero guardado como
  # `integer64` en una tabla y como doble en la otra -100000 y 1e5- no se
  # encontraba, y la cobertura de una clave foranea daba 0 sobre 2 de 3.
  # Medido en la ronda 22.
  valores <- .texto_identidad(.valores_relacion(x))
  valores_muestra <- .muestrear_vector(valores, muestra)$valores
  valores_completos <- valores[!is.na(valores)]
  familia <- .familia_relacion(x)
  if (is.null(origen_rango)) {
    # En memoria `x` es la columna completa; en una coleccion el rango llega
    # separado y declara si proviene del universo o si no pudo medirse.
    rango <- .rango_relacion(x, familia)
    origen_rango <- "columna_completa"
  }
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
    rango = rango,
    origen_rango = origen_rango
  )
}

.validar_columnas_candidatas_relacion <- function(datos, columnas, lado) {
  nombres <- if (is.character(datos)) datos else names(datos)
  if (is.null(columnas)) return(nombres)
  if (!is.character(columnas) || !length(columnas) || anyNA(columnas) ||
      any(!nzchar(columnas))) {
    stop(
      "`columnas_candidatas` para ", lado,
      " debe ser un vector de nombres no vacios.", call. = FALSE
    )
  }
  indices <- .indice_nombre(columnas, nombres)
  desconocidas <- columnas[is.na(indices)]
  if (length(desconocidas)) {
    stop(
      "`columnas_candidatas` nombra columnas inexistentes en ", lado, ": ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  nombres[unique(indices)]
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
# Una es **cierta**: dos columnas de la misma familia, con rangos numericos
# disjuntos y medidos sobre el mismo universo, no comparten ningun valor, y eso
# se sabe sin comparar. La respuesta es la misma que daria la comparacion
# -cero comunes, cobertura cero-, asi que saltearla es puro ahorro y la fila
# sale igual que siempre.
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
  if (!all(c(x$origen_rango, y$origen_rango) %in%
           c("columna_completa", "universo_db"))) {
    return(NULL)
  }
  rango_x <- x$rango
  rango_y <- y$rango
  if (!all(is.finite(c(rango_x, rango_y)))) return(NULL)
  if (rango_x[["maximo"]] >= rango_y[["minimo"]] &&
      rango_y[["maximo"]] >= rango_x[["minimo"]]) {
    return(NULL)
  }
  list(
    motivo = "rangos_disjuntos",
    detalle = .detalle_rangos_disjuntos(rango_x, rango_y)
  )
}

# El motivo de una poda es una EXPLICACION, y hasta el 2026-09-07 la explicacion
# publicaba los cuatro extremos crudos. Sobre una columna de documentos eso es
# una fuga: `perfilar()` y `perfilar_dbi()` enmascaran el minimo y el maximo de
# esa misma columna -salen `NA`- y por esta puerta salian literales, en texto,
# dentro del objeto devuelto. El minimo de una columna de documentos es el
# documento de una persona real, y el par minimo-maximo acota a los demas.
#
# Se aplica EL PISO, que es la definicion que el paquete ya tiene en un solo
# lugar: un valor de seis caracteres o mas no se publica. Es deliberadamente mas
# ancho que la regla completa -que ademas exige que la columna este clasificada
# como personal-, y la razon es que aca no hay clasificacion disponible:
# `relaciones_coleccion()` trabaja sobre un registro de tablas que no perfila, y
# clasificar por columna dentro de este bucle costaria mas que la poda que
# ahorra. El costo de ser mas ancho lo paga un monto de seis o mas caracteres,
# que sale como protegido en un texto explicativo; el par real sigue publicado
# en `cobertura_pares`. Enmascarar de mas en una explicacion es barato;
# publicar un documento de menos no lo es.
.detalle_rangos_disjuntos <- function(rango_x, rango_y) {
  # Se protege el RANGO entero, no cada extremo por separado. Enmascarar uno
  # solo publica el otro, y el par acota igual; ademas
  # `[[valor protegido], [valor protegido]]` no se lee.
  lado <- function(rango) {
    textos <- format(c(rango[["minimo"]], rango[["maximo"]]),
                     scientific = FALSE, trim = TRUE)
    if (length(.valores_identificantes(textos))) {
      "[valor protegido]"
    } else {
      paste0("[", textos[[1L]], ", ", textos[[2L]], "]")
    }
  }
  paste0(lado(rango_x), " y ", lado(rango_y))
}

.poda_relacion <- function(x, y, umbral_cobertura) {
  if (!identical(x$familia, y$familia) &&
      !all(c(x$familia, y$familia) %in% "numerica")) {
    # El motivo dice lo que se midio -que las familias son distintas- y no lo
    # que no se midio. Se llamaba `tipos_incompatibles`, y esa etiqueta afirma
    # una incompatibilidad que el propio `.Rd` niega: "Familias distintas
    # parece decisivo y no lo es: una columna de texto puede guardar
    # '2020-01-05' y coincidir con una de fecha". Medido: ese mismo par da
    # `1:1` con un valor comun por el camino por omision, y con `podar = TRUE`
    # salia declarado incompatible. La poda es correcta y esta documentada; lo
    # que sobraba era la afirmacion del nombre.
    return(list(
      motivo = "familias_distintas",
      detalle = paste0("familias ", x$familia, " y ", y$familia)
    ))
  }
  # Misma cautela que en la poda de dependencias: la cobertura maxima posible
  # es `x$n_distintos / y$n_distintos`, y se compara dividiendo. Escrita como
  # `y * umbral > x`, con 25 distintos contra 7 y umbral 0,28, el producto da
  # 7.0000000000000009 y declara imposible un par cuya cobertura maxima vale
  # exactamente el umbral -o sea alcanzable-.
  #
  # Pero esa cota es de VALORES DISTINTOS, y la cobertura que se compara con el
  # umbral es por FILAS: `mean(y$muestra %in% x$unicos)`. Un solo valor comun
  # puede cubrir casi todas las filas. Medido en la ronda 25: `a = {1}` contra
  # `b` con 91 filas en 1 y nueve valores mas daba cobertura 0,91 y la poda la
  # declaraba "imposible" con umbral 0,9 -la FK se perdia con un motivo falso-.
  #
  # La cota por filas: de `y` pueden estar en `x` a lo sumo `x$n_distintos`
  # valores, asi que las filas cubiertas no superan las de sus
  # `x$n_distintos` valores MAS FRECUENTES. La cota de distintos queda como
  # filtro previo -es condicion necesaria: los k mas frecuentes de d valores
  # cubren al menos k/d de las filas- y la de filas, que cuesta una tabla de
  # frecuencias, solo se calcula cuando aquella dispara.
  if (umbral_cobertura > 0 && x$n_distintos > 0L && y$n_distintos > 0L &&
      length(y$muestra) &&
      x$n_distintos / y$n_distintos < umbral_cobertura) {
    frecuencias <- sort(
      tabulate(match(y$muestra, unique(y$muestra))), decreasing = TRUE
    )
    k <- min(x$n_distintos, length(frecuencias))
    cota <- sum(frecuencias[seq_len(k)]) / length(y$muestra)
    if (cota < umbral_cobertura) {
      return(list(
        motivo = "cardinalidades_imposibles",
        detalle = paste0(
          "con ", x$n_distintos, " distintos en tabla1, los valores mas ",
          "frecuentes de tabla2 cubren a lo sumo ", signif(cota, 4),
          " de sus filas, por debajo del umbral ", umbral_cobertura
        )
      ))
    }
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
#' Los valores se comparan por su identidad. Los números, por su valor; los
#' instantes y las fechas también, con su fracción: dos instantes del mismo
#' segundo no coinciden, y un instante sin fracción se escribe al segundo
#' —`2020-01-01 10:00:00`— para que un texto con esa escritura lo encuentre.
#' Un texto con bytes que no son UTF-8 válido se compara por sus bytes, como
#' `%in%`: no es un ausente, y dos valores así distintos no coinciden.
#'
#' `columnas_candidatas` permite evitar la exploración de columnas que el usuario
#' sabe que no pueden participar. El costo crece con el producto de anchos: dos
#' tablas de treinta columnas son novecientas combinaciones por par de tablas, y
#' declarar cuáles pueden participar es lo que lo vuelve manejable.
#'
#' **Hay dos clases de poda y el paquete no las trata igual.** Dos columnas de
#' la misma familia con rangos numéricos disjuntos, medidos sobre la columna
#' completa o sobre el universo DBI, no comparten ningún valor, y eso se sabe sin
#' comparar: la fila sale como siempre —`sin_coincidencias`, con cobertura
#' cero— y la comparación se ahorra. Esa poda está siempre activa porque no
#' cambia lo que el objeto informa.
#'
#' Las otras dos sí lo cambiarían. Familias distintas parece decisivo y no lo
#' es: una columna de texto puede guardar `"2020-01-05"` y coincidir con una de
#' fecha. Y una cardinalidad imposible no dice que no haya coincidencias, dice
#' que no alcanzan `umbral_cobertura`, que es otra cosa. La cota es por filas,
#' como la cobertura: con `k` valores distintos en `tabla1`, las filas cubiertas
#' de `tabla2` no superan las de sus `k` valores más frecuentes. Por eso van detrás de
#' `podar = TRUE`, y cuando se aplican **el par no desaparece**: sale con
#' `cardinalidad = "sin_comparar"`, coberturas `NA` y su motivo en `motivo_poda`.
#' Un par que no se evaluó no es un par sin relación.
#'
#' Dos pares no se comparan nunca, con `podar` o sin él, porque la comparación
#' no daría una respuesta sino una falsa. Si una tabla tiene dos columnas con el
#' mismo nombre, el nombre no dice cuál de las dos es: sus pares salen
#' `sin_comparar` con `motivo_poda = "nombre_repetido"`. Y una fecha contra una
#' fecha-hora no comparte escritura —`2020-01-01` contra `2020-01-01 00:00:00`—, así
#' que ningún valor coincidiría aunque fueran los mismos días: el par sale con
#' `motivo_poda = "fecha_contra_instante"`. Convertir una de las dos a la clase de
#' la otra, con `tz` explícito, y volver a comparar.
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
#' @param .rangos Uso interno de [relaciones_coleccion()]. Lista nombrada por
#'   `tabla1` y `tabla2`, con rangos y su origen para cada columna. La poda por
#'   rangos solo acepta rangos de la columna completa o del universo DBI. Una
#'   columna cuya lectura no permite comparar exacto lo declara en
#'   `no_comparable`, y sus pares salen `sin_comparar` con ese motivo.
#'
#' @return Data frame con `columna_tabla1`, `columna_tabla2`, `cardinalidad`
#'   —`1:1`, `1:m`, `m:1`, `m:m`, `sin_coincidencias` o `sin_comparar`—,
#'   `n_valores_comunes`,
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
                                tope_memoria_mb = Inf, .rangos = NULL) {
  if (!inherits(tabla1, "data.frame") || !inherits(tabla2, "data.frame")) {
    stop("`tabla1` y `tabla2` deben heredar de data.frame.", call. = FALSE)
  }
  tabla1 <- .tabla_base(tabla1)
  tabla2 <- .tabla_base(tabla2)
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
  nombres_1 <- names(tabla1)
  nombres_2 <- names(tabla2)
  candidatas <- .resolver_columnas_candidatas_relacion(
    columnas_candidatas, nombres_1, nombres_2
  )
  # Un nombre que la tabla repite aparece una sola vez entre las candidatas: sus
  # pares no se comparan -ver abajo- y dos filas iguales no dirian mas.
  candidatas <- lapply(candidatas, function(nombres) {
    nombres[!duplicated(.nombres_para_operar(nombres))]
  })
  if (!is.null(.rangos) && (!is.list(.rangos) ||
      !all(c("tabla1", "tabla2") %in% names(.rangos)))) {
    stop(
      "`.rangos` debe declarar entradas `tabla1` y `tabla2`.",
      call. = FALSE
    )
  }
  rango_de <- function(lado, nombre) {
    if (is.null(.rangos)) return(NULL)
    disponibles <- .rangos[[lado]]
    especificacion <- if (is.list(disponibles)) {
      disponibles[[nombre]]
    } else NULL
    if (is.list(especificacion) &&
        !is.null(especificacion$rango) &&
        !is.null(especificacion$origen)) {
      return(especificacion)
    }
    list(
      rango = .rango_relacion_sin_evidencia(),
      origen = "no_disponible"
    )
  }
  indices_1 <- .indice_nombre(candidatas$tabla1, nombres_1)
  indices_2 <- .indice_nombre(candidatas$tabla2, nombres_2)
  columnas_1 <- lapply(indices_1, function(i) {
    especificacion <- rango_de("tabla1", nombres_1[[i]])
    if (is.null(especificacion)) {
      .resumir_columna_relacion(tabla1[[i]], limite_muestra)
    } else {
      .resumir_columna_relacion(
        tabla1[[i]], limite_muestra, especificacion$rango,
        especificacion$origen
      )
    }
  })
  columnas_2 <- lapply(indices_2, function(i) {
    especificacion <- rango_de("tabla2", nombres_2[[i]])
    if (is.null(especificacion)) {
      .resumir_columna_relacion(tabla2[[i]], limite_muestra)
    } else {
      .resumir_columna_relacion(
        tabla2[[i]], limite_muestra, especificacion$rango,
        especificacion$origen
      )
    }
  })
  filas <- list()
  podas <- list()
  memoria_mb <- 0
  comparadas <- 0L
  presupuesto_agotado <- FALSE

  repetido_1 <- .nombres_repetidos(nombres_1)[indices_1]
  repetido_2 <- .nombres_repetidos(nombres_2)[indices_2]
  # Lo que la LECTURA no permite comparar exacto lo declara quien leyo
  # -`relaciones_coleccion()`, en `.rangos`-: un BIGINT que el driver entrego
  # como doble por encima de 2^53 ya no distingue dos enteros consecutivos.
  lectura_de <- function(lado, nombre) {
    if (is.null(.rangos) || !is.list(.rangos[[lado]])) return(NULL)
    especificacion <- .rangos[[lado]][[nombre]]
    if (is.list(especificacion) && is.list(especificacion$no_comparable)) {
      especificacion$no_comparable
    } else NULL
  }
  lectura_1 <- lapply(nombres_1[indices_1], function(n) lectura_de("tabla1", n))
  lectura_2 <- lapply(nombres_2[indices_2], function(n) lectura_de("tabla2", n))
  for (i in seq_along(indices_1)) {
    x <- columnas_1[[i]]
    for (j in seq_along(indices_2)) {
      y <- columnas_2[[j]]
      nombre_1 <- candidatas$tabla1[[i]]
      nombre_2 <- candidatas$tabla2[[j]]
      # Dos pares que no se pueden comparar, con `podar` o sin el, porque la
      # respuesta de la comparacion seria falsa y no solo cara:
      #
      # - un nombre que su tabla repite: la columna se buscaba por nombre y
      #   siempre salia la primera, asi que la fila de la segunda publicaba la
      #   relacion de la primera -`1:1` con un valor comun donde el cruce real
      #   es vacio-;
      # - una fecha contra una fecha-hora: `Date` se compara como `2020-01-01` y
      #   `POSIXct` como `2020-01-01 00:00:00`, ningun valor coincide nunca, y la
      #   fila decia `sin_coincidencias` con cobertura 0 sobre los mismos dias.
      #   `CorrectitudSemFuerte` ya avisa esta mezcla; aca se declara en la fila.
      no_comparable <- if (repetido_1[[i]] || repetido_2[[j]]) {
        list(
          motivo = "nombre_repetido",
          detalle = paste0(
            "la tabla tiene mas de una columna con ese nombre y no se puede ",
            "decir cual se compara; renombrarlas con `make.unique()`"
          )
        )
      } else if (setequal(c(x$familia, y$familia), c("fecha", "fecha-hora"))) {
        list(
          motivo = "fecha_contra_instante",
          detalle = paste0(
            "una fecha de calendario y un instante no comparten escritura; ",
            "convertir una de las dos a la clase de la otra, con `tz` ",
            "explicito, y volver a comparar"
          )
        )
      } else if (!is.null(lectura_1[[i]])) {
        lectura_1[[i]]
      } else if (!is.null(lectura_2[[j]])) {
        lectura_2[[j]]
      } else NULL
      if (!is.null(no_comparable)) {
        podas[[length(podas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          motivo = no_comparable$motivo, detalle = no_comparable$detalle,
          stringsAsFactors = FALSE
        )
        filas[[length(filas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          cardinalidad = "sin_comparar", n_valores_comunes = NA_integer_,
          cobertura_tabla1_en_tabla2 = NA_real_,
          cobertura_tabla2_en_tabla1 = NA_real_,
          motivo_poda = no_comparable$motivo, stringsAsFactors = FALSE
        )
        next
      }
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
        # Rangos disjuntos de la misma familia y del mismo universo: la
        # respuesta se conoce sin comparar, y es la misma.
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
        # El par sale declarado como no comparado, igual que en las podas de
        # arriba. Hasta el 2026-09-08 esta rama guardaba el par SOLO en el
        # atributo `podas`, asi que con el presupuesto agotado la funcion
        # devolvia una tabla de CERO filas: quien la imprime veia encabezados y
        # nada. Una tabla vacia afirma —se lee como "no hay relaciones"— y lo que
        # paso fue "no se comparo ninguna". La forma correcta ya estaba dos
        # ramas mas arriba; esta era la unica que se salia de la convencion.
        filas[[length(filas) + 1L]] <- data.frame(
          columna_tabla1 = nombre_1, columna_tabla2 = nombre_2,
          cardinalidad = "sin_comparar", n_valores_comunes = NA_integer_,
          cobertura_tabla1_en_tabla2 = NA_real_,
          cobertura_tabla2_en_tabla1 = NA_real_,
          motivo_poda = "presupuesto_memoria_agotado",
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
