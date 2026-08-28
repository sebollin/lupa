.copiar_data_frame <- function(x) {
  if (inherits(x, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::copy(x), stringsAsFactors = FALSE))
  }
  as.data.frame(x, stringsAsFactors = FALSE)
}

.validar_nombres_referencial <- function(datos, columnas, argumento,
                                         permitir_vacio = FALSE) {
  if (!is.character(columnas) || anyNA(columnas) ||
      any(!nzchar(columnas)) || anyDuplicated(columnas) ||
      (!permitir_vacio && !length(columnas))) {
    stop("`", argumento, "` debe contener nombres de columna \u00fanicos y no vac\u00edos.",
         call. = FALSE)
  }
  faltantes <- setdiff(columnas, names(datos))
  if (length(faltantes)) {
    stop(
      "No se encontraron columnas de `", argumento, "`: ",
      paste(faltantes, collapse = ", "), ".", call. = FALSE
    )
  }
  columnas
}

.codigos_filas <- function(datos) {
  if (!nrow(datos)) return(integer())
  if (!ncol(datos)) return(rep.int(1L, nrow(datos)))
  factores <- lapply(datos, function(x) {
    factor(.valores_relacion(x), exclude = NULL)
  })
  as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
}

.filas_en_referencial <- function(objetivo, referencia) {
  if (ncol(objetivo) != ncol(referencia)) {
    stop("El objeto y el referencial deben tener la misma cantidad de columnas.",
         call. = FALSE)
  }
  nombres <- paste0("V", seq_len(ncol(objetivo)))
  objetivo <- as.data.frame(objetivo, stringsAsFactors = FALSE)
  referencia <- as.data.frame(referencia, stringsAsFactors = FALSE)
  names(objetivo) <- names(referencia) <- nombres
  combinado <- rbind(referencia, objetivo)
  codigos <- .codigos_filas(combinado)
  n_ref <- nrow(referencia)
  if (!n_ref) return(rep(FALSE, nrow(objetivo)))
  codigos_objetivo <- codigos[n_ref + seq_len(nrow(objetivo))]
  codigos_objetivo %in% codigos[seq_len(n_ref)]
}

# Configuracion comun de las metricas referenciales. `normalizar = NULL` es
# deliberado: permite heredar el perfil declarado por referencial().
.validar_config_referencial <- function(configuracion) {
  permitidas <- c("normalizar", "proximidad", "metodo", "p", "umbral",
                  "max_pares", "nucleos")
  desconocidas <- setdiff(names(configuracion), permitidas)
  if (length(desconocidas)) {
    stop("Las metricas referenciales no aceptan: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  normalizar <- configuracion[["normalizar"]]
  if (!is.null(normalizar)) {
    tryCatch(.resolver_normalizacion(normalizar), error = function(e) {
      stop("`normalizar` no describe un perfil valido: ",
           conditionMessage(e), call. = FALSE)
    })
  }
  proximidad <- configuracion[["proximidad"]]
  if (is.null(proximidad)) proximidad <- TRUE
  if (!is.logical(proximidad) || length(proximidad) != 1L || is.na(proximidad)) {
    stop("`proximidad` debe ser un logico escalar sin NA.", call. = FALSE)
  }
  metodo <- configuracion[["metodo"]]
  if (is.null(metodo)) metodo <- "jw"
  metodos <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
               "jaccard", "jw", "soundex")
  if (!is.character(metodo) || length(metodo) != 1L || is.na(metodo) ||
      !metodo %in% metodos) {
    stop("`metodo` debe ser una medida admitida: ",
         paste(metodos, collapse = ", "), ".", call. = FALSE)
  }
  p <- configuracion[["p"]]
  if (is.null(p)) p <- 0.1
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) ||
      p < 0 || p > 0.25) {
    stop("`p` debe ser un numero finito entre 0 y 0.25.", call. = FALSE)
  }
  umbral <- configuracion[["umbral"]]
  if (is.null(umbral)) umbral <- 0.10
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral < 0) {
    stop("`umbral` debe ser un numero finito no negativo.", call. = FALSE)
  }
  max_pares <- configuracion[["max_pares"]]
  if (is.null(max_pares)) max_pares <- 500000L
  if (!is.numeric(max_pares) || length(max_pares) != 1L || is.na(max_pares) ||
      max_pares < 1 || (!is.infinite(max_pares) && max_pares != floor(max_pares))) {
    stop("`max_pares` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  list(
    normalizar = normalizar, proximidad = proximidad, metodo = metodo, p = p,
    umbral = umbral,
    max_pares = if (is.infinite(max_pares)) Inf else as.integer(max_pares),
    nucleos = .resolver_nucleos_lupa(configuracion[["nucleos"]])
  )
}

.referencial_normalizacion <- function(instancia, referencia) {
  normalizar <- instancia$configuracion$normalizar
  if (is.null(normalizar)) normalizar <- referencia$normalizar
  if (is.null(normalizar)) normalizar <- TRUE
  .resolver_normalizacion(normalizar)
}

.referencial_tabla_normalizada <- function(tabla, columnas, perfil) {
  salida <- lapply(columnas, function(columna) {
    texto <- suppressWarnings(as.character(.valores_relacion(tabla[[columna]])))
    .normalizacion_aplicar(
      texto, .normalizacion_para_columna(perfil, columna)
    )
  })
  names(salida) <- columnas
  as.data.frame(salida, stringsAsFactors = FALSE)
}

.referencial_filas_texto <- function(tabla, columnas, perfil) {
  normalizada <- .referencial_tabla_normalizada(tabla, columnas, perfil)
  if (!nrow(normalizada)) return(character())
  normalizada[] <- lapply(normalizada, function(x) {
    x[is.na(x)] <- ""
    x
  })
  do.call(paste, c(unname(normalizada), sep = " | "))
}

.referencial_filas_original_texto <- function(tabla, columnas) {
  if (!nrow(tabla)) return(character())
  valores <- lapply(columnas, function(columna) {
    texto <- suppressWarnings(as.character(.valores_relacion(tabla[[columna]])))
    texto[is.na(texto)] <- ""
    texto
  })
  do.call(paste, c(valores, sep = " | "))
}

.referencial_proximidad <- function(filas_fallidas, texto_objetivo,
                                    texto_referencia, referencia_original,
                                    config) {
  n <- length(texto_objetivo)
  valores_fallidos <- unique(texto_objetivo[filas_fallidas])
  n_valores <- length(valores_fallidos)
  evidencia <- rep("", n)
  base <- list(
    solicitada = isTRUE(config$proximidad), disponible = FALSE,
    motivo = "No se calculo la proximidad.", n_fallos = length(filas_fallidas),
    n_valores_fallidos_distintos = n_valores,
    n_valores_fallidos_comparados = 0L,
    n_fallos_comparados = 0L, n_referencial = length(texto_referencia),
    n_pares_comparados = 0, n_pares_sin_comparar = 0,
    umbral = config$umbral, metodo = config$metodo, p = config$p,
    max_pares = config$max_pares, truncado = FALSE
  )
  if (!isTRUE(config$proximidad)) {
    base$motivo <- "La proximidad fue desactivada por configuracion."
    return(list(evidencia = evidencia, alcance = base))
  }
  if (!.stringdist_disponible()) {
    base$motivo <- "No esta instalado el paquete opcional 'stringdist'."
    base$n_pares_sin_comparar <- as.numeric(n_valores) *
      length(texto_referencia)
    return(list(evidencia = evidencia, alcance = base))
  }
  if (!length(filas_fallidas) || !length(texto_referencia)) {
    base$disponible <- TRUE
    base$motivo <- "No hubo fallos o el referencial esta vacio."
    return(list(evidencia = evidencia, alcance = base))
  }
  nref <- length(texto_referencia)
  ncomparar <- if (is.infinite(config$max_pares)) {
    n_valores
  } else min(n_valores, floor(config$max_pares / nref))
  if (ncomparar < 1L) {
    base$motivo <- "El limite de pares no alcanza para comparar un fallo con el referencial."
    base$n_pares_sin_comparar <- as.numeric(n_valores) * nref
    base$truncado <- TRUE
    return(list(evidencia = evidencia, alcance = base))
  }
  elegidas <- seq_len(ncomparar)
  distancias <- .matriz_distancias_duplicados(
    valores_fallidos[elegidas], texto_referencia,
    metodo = config$metodo, p = config$p, nucleos = config$nucleos
  )
  if (is.null(dim(distancias))) distancias <- matrix(distancias, nrow = ncomparar)
  etiquetas <- vapply(seq_len(nrow(referencia_original)), function(i) {
    paste(as.character(referencia_original[i, , drop = TRUE]), collapse = " | ")
  }, character(1L))
  evidencia_valores <- rep("", n_valores)
  for (i in seq_len(ncomparar)) {
    minimo <- min(distancias[i, ])
    if (is.finite(minimo) && minimo <= config$umbral) {
      cerca <- which(abs(distancias[i, ] - minimo) <= 1e-12)
      evidencia_valores[elegidas[[i]]] <- paste0(
        "candidato_referencial=", paste(etiquetas[cerca], collapse = " / "),
        "; distancia=", formatC(minimo, format = "f", digits = 4)
      )
    }
  }
  indices_fallidos <- match(texto_objetivo[filas_fallidas], valores_fallidos)
  evidencia[filas_fallidas] <- evidencia_valores[indices_fallidos]
  base$disponible <- TRUE
  base$motivo <- ""
  base$n_valores_fallidos_comparados <- ncomparar
  base$n_fallos_comparados <- sum(indices_fallidos <= ncomparar)
  base$n_pares_comparados <- as.numeric(ncomparar) * nref
  base$n_pares_sin_comparar <- as.numeric(n_valores - ncomparar) * nref
  base$truncado <- ncomparar < n_valores
  list(evidencia = evidencia, alcance = base)
}

#' Declarar un conjunto de datos referencial
#'
#' Un referencial representa conocimiento externo mediante una clave y, de
#' forma opcional, valores asociados a ella. `normalizar` controla la
#' representación usada para emparejar referenciales; no modifica los datos
#' guardados. La declaración de completitud es explícita: `RatioCobertura`
#' exige `completo = TRUE` y un `alcance` explícito; un referencial parcial sólo
#' puede usarse para correctitud.
#'
#' La clave no admite ausentes y debe identificar cada fila de forma única.
#' `valor` no puede repetir columnas de `clave` y representa atributos que se
#' contrastan en correctitud semántica débil. El constructor copia la tabla y
#' no consulta fuentes externas.
#'
#' @param datos Tabla de referencia. Se conserva una copia ordinaria de R.
#' @param clave Columnas que identifican unívocamente cada fila. Puede
#'   contener varias columnas.
#' @param valor Columnas asociadas que pueden contrastarse en correctitud débil.
#'   Es opcional.
#' @param completo Si la tabla cubre todo el universo declarado. Es `FALSE` por
#'   omisión.
#' @param alcance Descripción obligatoria cuando `completo = TRUE`.
#' @param nombre Nombre legible del referencial. Si se omite, usa el nombre del
#'   objeto de entrada o `"referencial"`.
#' @param normalizar `TRUE`, `FALSE`, `"amplio"`, un perfil de
#'   [normalizacion()] o una lista nombrada por columna. `TRUE` es el valor
#'   predeterminado.
#' @return Un objeto de clase `referencial`.
#' @export
#'
#' @seealso [metricas_referencial()], [instanciar()], [detectar_relaciones()]
#'
#' @examples
#' padron <- referencial(
#'   data.frame(codigo = c("01", "02"), departamento = c("Artigas", "Canelones")),
#'   clave = "codigo", valor = "departamento",
#'   completo = TRUE, alcance = "departamentos del Uruguay"
#' )
#' padron
referencial <- function(datos, clave, valor = character(), completo = FALSE,
                        alcance = NULL, nombre = NULL, normalizar = TRUE) {
  expresion_datos <- substitute(datos)
  if (is.null(nombre)) {
    nombre <- if (is.symbol(expresion_datos)) {
      as.character(expresion_datos)
    } else {
      "referencial"
    }
  }
  .validar_datos_tabla(datos)
  if (is.null(names(datos)) || anyNA(names(datos)) || any(!nzchar(names(datos))) ||
      anyDuplicated(names(datos))) {
    stop("El referencial requiere nombres de columna \u00fanicos y no vac\u00edos.",
         call. = FALSE)
  }
  tabla <- .copiar_data_frame(datos)
  clave <- .validar_nombres_referencial(tabla, clave, "clave")
  valor <- .validar_nombres_referencial(
    tabla, valor, "valor", permitir_vacio = TRUE
  )
  if (length(intersect(clave, valor))) {
    stop("`clave` y `valor` no pueden compartir columnas.", call. = FALSE)
  }
  if (any(vapply(tabla[c(clave, valor)], is.list, logical(1L)))) {
    stop("Las columnas del referencial deben ser vectores at\u00f3micos.", call. = FALSE)
  }
  if (any(!stats::complete.cases(tabla[clave]))) {
    stop("La clave del referencial no puede contener valores ausentes.",
         call. = FALSE)
  }
  if (anyDuplicated(tabla[clave])) {
    stop("La clave del referencial debe identificar un\u00edvocamente cada fila.",
         call. = FALSE)
  }
  if (!is.logical(completo) || length(completo) != 1L || is.na(completo)) {
    stop("`completo` debe ser un l\u00f3gico escalar sin NA.", call. = FALSE)
  }
  if (completo && !.es_texto_escalar(alcance)) {
    stop("Un referencial completo requiere describir su `alcance`.",
         call. = FALSE)
  }
  if (!is.null(alcance) && !.es_texto_escalar(alcance)) {
    stop("`alcance` debe ser NULL o una cadena no vac\u00eda.", call. = FALSE)
  }
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  normalizar <- tryCatch(
    .resolver_normalizacion(normalizar),
    error = function(e) {
      stop("`normalizar` no describe un perfil valido: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  estructura <- list(
    datos = tabla, clave = clave, valor = valor, completo = completo,
    alcance = if (is.null(alcance)) NA_character_ else alcance,
    nombre = nombre, normalizar = normalizar
  )
  class(estructura) <- "referencial"
  estructura
}

#' @export
print.referencial <- function(x, ...) {
  cat("Referencial:", x$nombre, "\n")
  cat("  Filas:", nrow(x$datos), "\n")
  cat("  Clave:", paste(x$clave, collapse = " + "), "\n")
  if (length(x$valor)) {
    cat("  Valores:", paste(x$valor, collapse = " + "), "\n")
  }
  cat("  Completo:", if (x$completo) "s\u00ed" else "no", "\n")
  if (x$completo) cat("  Alcance:", x$alcance, "\n")
  invisible(x)
}

.exigir_referencial <- function(instancia, valores = FALSE,
                                completo = FALSE) {
  referencia <- instancia$referencial
  if (!inherits(referencia, "referencial")) {
    stop(
      "La m\u00e9trica ", instancia$declaracion$nombre,
      " requiere un objeto creado por referencial().", call. = FALSE
    )
  }
  if (valores && !length(referencia$valor)) {
    stop("CorrectitudSemDebil requiere valores asociados en el referencial.",
         call. = FALSE)
  }
  if (completo && !isTRUE(referencia$completo)) {
    stop(
      "RatioCobertura exige un referencial declarado completo y con alcance.",
      call. = FALSE
    )
  }
  referencia
}

# Los dos metodos de correctitud referencial -el fuerte, que exige que la clave
# exista en el referencial, y el debil, que ademas compara los valores- eran la
# misma funcion escrita dos veces: 48 de sus 68 lineas coincidian y lo unico que
# cambiaba era QUE COLUMNAS de la referencia se usan. Se parametriza eso y el
# resto es uno solo, que ademas garantiza que los dos midan igual lo que miden
# igual.
.metodo_correctitud_comun <- function(tablas, instancia, referencia,
                                     valores_referencia, columnas_referencia) {
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop("No se encontraron atributos ligados: ", paste(faltantes, collapse = ", "), ".",
         call. = FALSE)
  }
  objetivo <- tabla[, instancia$atributos, drop = FALSE]
  presentes <- stats::complete.cases(objetivo)
  filas <- which(presentes)
  perfil <- .referencial_normalizacion(instancia, referencia)
  objetivo_presente <- objetivo[presentes, , drop = FALSE]
  usa_normalizacion <- !is.null(instancia$configuracion$normalizar) ||
    !is.null(referencia$normalizar)
  if (usa_normalizacion) {
    objetivo_comparable <- .referencial_tabla_normalizada(
      objetivo_presente, instancia$atributos, perfil
    )
    referencia_comparable <- .referencial_tabla_normalizada(
      valores_referencia, columnas_referencia, perfil
    )
    resultado <- .filas_en_referencial(objetivo_comparable, referencia_comparable)
    texto_objetivo <- .referencial_filas_texto(
      objetivo_presente, instancia$atributos, perfil
    )
    texto_referencia <- .referencial_filas_texto(
      valores_referencia, columnas_referencia, perfil
    )
  } else {
    resultado <- .filas_en_referencial(objetivo_presente, valores_referencia)
    texto_objetivo <- .referencial_filas_original_texto(
      objetivo_presente, instancia$atributos
    )
    texto_referencia <- .referencial_filas_original_texto(
      valores_referencia, columnas_referencia
    )
  }
  config <- instancia$configuracion
  fallos <- which(!resultado)
  proximidad <- .referencial_proximidad(
    fallos, texto_objetivo, texto_referencia, valores_referencia, config
  )
  objetos <- paste0(entidad, "[", filas, ",",
                    paste(instancia$atributos, collapse = "+"), "]")
  objetos[fallos] <- ifelse(
    nzchar(proximidad$evidencia[fallos]),
    paste0(objetos[fallos], " {", proximidad$evidencia[fallos], "}"),
    objetos[fallos]
  )
  salida <- .salida_metodo(
    resultado, entidad, paste(instancia$atributos, collapse = "+"), filas, objetos
  )
  attr(salida, "alcance") <- list(
    normalizar = if (is.null(instancia$configuracion$normalizar)) {
      "heredado_del_referencial"
    } else instancia$configuracion$normalizar,
    normalizacion = .normalizacion_resumen(perfil),
    n_evaluados = length(filas), n_presentes = length(filas),
    n_afectados = sum(!resultado),
    proximidad = proximidad$alcance
  )
  salida
}


.metodo_correctitud_fuerte <- function(tablas, instancia) {
  referencia <- .exigir_referencial(instancia)
  .validar_vinculo(instancia, 1L, length(referencia$clave))
  .metodo_correctitud_comun(
    tablas, instancia, referencia, referencia$datos[referencia$clave],
    referencia$clave
  )
}

.metodo_correctitud_debil <- function(tablas, instancia) {
  referencia <- .exigir_referencial(instancia, valores = TRUE)
  columnas_ref <- c(referencia$clave, referencia$valor)
  .validar_vinculo(instancia, 1L, length(columnas_ref))
  .metodo_correctitud_comun(
    tablas, instancia, referencia, referencia$datos[columnas_ref], columnas_ref
  )
}

.metodo_ratio_cobertura <- function(tablas, instancia) {
  referencia <- .exigir_referencial(instancia, completo = TRUE)
  .validar_vinculo(instancia, 1L, length(referencia$clave))
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop("No se encontraron atributos ligados: ", paste(faltantes, collapse = ", "), ".",
         call. = FALSE)
  }
  objetivo <- unique(tabla[stats::complete.cases(tabla[instancia$atributos]),
                           instancia$atributos, drop = FALSE])
  referencia_clave <- referencia$datos[referencia$clave]
  perfil <- .referencial_normalizacion(instancia, referencia)
  usa_normalizacion <- !is.null(instancia$configuracion$normalizar) ||
    !is.null(referencia$normalizar)
  if (usa_normalizacion) {
    cubiertas <- .filas_en_referencial(
      .referencial_tabla_normalizada(referencia_clave, referencia$clave, perfil),
      .referencial_tabla_normalizada(objetivo, instancia$atributos, perfil)
    )
  } else {
    cubiertas <- .filas_en_referencial(referencia_clave, objetivo)
  }
  resultado <- if (nrow(referencia_clave)) mean(cubiertas) else 1
  salida <- .salida_metodo(
    resultado, entidad, paste(instancia$atributos, collapse = "+"), NA_integer_,
    paste0(entidad, " respecto de ", referencia$nombre)
  )
  attr(salida, "alcance") <- list(
    normalizacion = .normalizacion_resumen(perfil),
    n_referencial = nrow(referencia_clave),
    n_valores_objetivo = nrow(objetivo),
    proximidad = list(solicitada = FALSE, motivo =
                        "La proximidad no participa en la cobertura.")
  )
  salida
}

#' Métricas que consumen un referencial tabular
#'
#' Devuelve las tres métricas base que pueden medirse con el contrato de
#' [referencial()]. `CorrectitudSemFuerte` verifica que la identificación exista;
#' `CorrectitudSemDebil` comprueba el par identificación–valor; `RatioCobertura`
#' mide qué proporción del universo completo de claves aparece en la entidad.
#' Los ratios de correctitud se obtienen mediante [agregar()] con `"ratio"`.
#' Las tres métricas aceptan `normalizar`, `proximidad`, `metodo`, `p`, `umbral`,
#' `max_pares` y `nucleos`. `normalizar = NULL` hereda el perfil declarado por
#' [referencial()]. La normalización sólo cambia la representación usada para
#' emparejar: no modifica los datos. La proximidad es evidencia para los
#' valores ausentes y nunca cambia su veredicto; si el paquete opcional
#' [stringdist](https://cran.r-project.org/package=stringdist) no está
#' instalado, se declara que no se calculó. Se calcula una sola vez por valor
#' fallido distinto y la evidencia se reparte a las filas repetidas. El alcance
#' conserva por separado `n_fallos` (filas),
#' `n_valores_fallidos_distintos`, `n_valores_fallidos_comparados` y los pares
#' comparados; así el límite no depende del orden ni de la frecuencia de las
#' filas.
#'
#' Los valores ausentes no generan medidas de correctitud: corresponden a la
#' dimensión Completitud. La cobertura ignora claves ausentes en el objetivo y
#' no permite que duplicados inflen el resultado.
#'
#' @return Lista con tres objetos `metrica_generica`.
#' @export
#'
#' @seealso [referencial()], [metricas_nucleo()], [agregar()]
#'
#' @examples
#' ref <- referencial(
#'   data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla")),
#'   "id", "nombre", completo = TRUE, alcance = "padrón de ejemplo"
#' )
#' m <- metricas_referencial()
#' fuerte <- instanciar(especializar(m$CorrectitudSemFuerte),
#'   "personas", "id", referencial = ref)
#' medir(modelo(fuerte), data.frame(id = c(1, 4)))
metricas_referencial <- function() {
  list(
    CorrectitudSemFuerte = metrica(
      "CorrectitudSemFuerte",
      "Indica si la identificaci\u00f3n de una entidad existe en un referencial.",
      "instanciaAtributo", "booleano", dimension = "Exactitud",
      factor = "Correctitud sem\u00e1ntica", propiedades = c(
        "normalizar", "proximidad", "metodo", "p", "umbral", "max_pares", "nucleos"
      ), metodo = .metodo_correctitud_fuerte,
      validar_propiedades = .validar_config_referencial,
      orientacion = "conformidad"
    ),
    CorrectitudSemDebil = metrica(
      "CorrectitudSemDebil",
      "Indica si un valor est\u00e1 asociado a la identificaci\u00f3n correcta en un referencial.",
      "instanciaAtributo", "booleano", dimension = "Exactitud",
      factor = "Correctitud sem\u00e1ntica", propiedades = c(
        "normalizar", "proximidad", "metodo", "p", "umbral", "max_pares", "nucleos"
      ), metodo = .metodo_correctitud_debil,
      validar_propiedades = .validar_config_referencial,
      orientacion = "conformidad"
    ),
    RatioCobertura = metrica(
      "RatioCobertura",
      "Mide la cobertura de una entidad respecto de un referencial completo.",
      "entidad", "real", dimension = "Completitud", factor = "Cobertura",
      propiedades = c(
        "normalizar", "proximidad", "metodo", "p", "umbral", "max_pares", "nucleos"
      ), metodo = .metodo_ratio_cobertura,
      validar_propiedades = .validar_config_referencial,
      orientacion = "conformidad"
    )
  )
}
