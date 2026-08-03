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

#' Declarar un conjunto de datos referencial
#'
#' Un referencial representa conocimiento externo mediante una clave y, de
#' forma opcional, valores asociados a ella. Se diferencia de un diccionario:
#' el diccionario sólo enumera valores sintácticamente válidos, mientras que
#' el referencial permite comprobar que una entidad existe y que sus atributos
#' están asociados a la clave correcta.
#'
#' La declaración de completitud es explícita. `RatioCobertura` sólo tiene
#' sentido bajo una asunción de mundo cerrado y exige `completo = TRUE`; una
#' lista parcial puede usarse para correctitud, pero no como denominador de
#' cobertura.
#'
#' `clave` no admite ausentes y debe identificar cada fila de `datos` de forma
#' única; puede contener varias columnas. `valor` es opcional, no puede repetir
#' columnas de `clave` y representa los atributos asociados que se contrastan
#' en correctitud semántica fuerte. `completo = FALSE` es el valor
#' predeterminado y permite omitir `alcance`. Al declarar `completo = TRUE`,
#' `alcance` pasa a ser obligatorio y debe nombrar el universo que la tabla dice
#' cubrir. El constructor copia la tabla y no consulta fuentes externas.
#'
#' @param datos Tabla de referencia. Se conserva una copia ordinaria de R.
#' @param clave Columnas que identifican unívocamente cada fila.
#' @param valor Columnas cuyos valores se contrastan junto con la clave.
#' @param completo Si el referencial declara contener todo el universo del
#'   alcance indicado. Es `FALSE` por omisión.
#' @param alcance Descripción explícita de aquello de lo que el referencial se
#'   declara completo. Es obligatoria cuando `completo = TRUE`.
#' @param nombre Nombre legible del referencial. Si se omite, usa el nombre del
#'   objeto de entrada o `"referencial"` cuando la tabla se construye en línea.
#'
#' @return Objeto de clase `referencial` con `datos`, `clave`, `valor`,
#'   `completo`, `alcance` y `nombre`.
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
                        alcance = NULL,
                        nombre = NULL) {
  expresion_datos <- substitute(datos)
  if (is.null(nombre)) {
    nombre <- if (is.symbol(expresion_datos)) {
      as.character(expresion_datos)
    } else {
      "referencial"
    }
  }
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
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
  estructura <- list(
    datos = tabla, clave = clave, valor = valor, completo = completo,
    alcance = if (is.null(alcance)) NA_character_ else alcance,
    nombre = nombre
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

.metodo_correctitud_fuerte <- function(tablas, instancia) {
  referencia <- .exigir_referencial(instancia)
  .validar_vinculo(instancia, 1L, length(referencia$clave))
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop("No se encontraron atributos ligados: ", paste(faltantes, collapse = ", "), ".",
         call. = FALSE)
  }
  objetivo <- tabla[instancia$atributos]
  presentes <- stats::complete.cases(objetivo)
  filas <- which(presentes)
  resultado <- .filas_en_referencial(
    objetivo[presentes, , drop = FALSE],
    referencia$datos[referencia$clave]
  )
  .salida_metodo(
    resultado, entidad, paste(instancia$atributos, collapse = "+"), filas,
    paste0(entidad, "[", filas, ",", paste(instancia$atributos, collapse = "+"), "]")
  )
}

.metodo_correctitud_debil <- function(tablas, instancia) {
  referencia <- .exigir_referencial(instancia, valores = TRUE)
  columnas_ref <- c(referencia$clave, referencia$valor)
  .validar_vinculo(instancia, 1L, length(columnas_ref))
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop("No se encontraron atributos ligados: ", paste(faltantes, collapse = ", "), ".",
         call. = FALSE)
  }
  objetivo <- tabla[instancia$atributos]
  presentes <- stats::complete.cases(objetivo)
  filas <- which(presentes)
  resultado <- .filas_en_referencial(
    objetivo[presentes, , drop = FALSE], referencia$datos[columnas_ref]
  )
  .salida_metodo(
    resultado, entidad, paste(instancia$atributos, collapse = "+"), filas,
    paste0(entidad, "[", filas, ",", paste(instancia$atributos, collapse = "+"), "]")
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
  cubiertas <- .filas_en_referencial(referencia_clave, objetivo)
  resultado <- if (nrow(referencia_clave)) mean(cubiertas) else 1
  .salida_metodo(
    resultado, entidad, paste(instancia$atributos, collapse = "+"), NA_integer_,
    paste0(entidad, " respecto de ", referencia$nombre)
  )
}

#' Métricas que consumen un referencial tabular
#'
#' Devuelve las tres métricas base que pueden medirse con el contrato de
#' [referencial()]. `CorrectitudSemFuerte` verifica que la identificación exista;
#' `CorrectitudSemDebil` comprueba el par identificación–valor; `RatioCobertura`
#' mide qué proporción del universo completo de claves aparece en la entidad.
#' Los ratios de correctitud se obtienen mediante [agregar()] con `"ratio"`.
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
      factor = "Correctitud sem\u00e1ntica", metodo = .metodo_correctitud_fuerte
    ),
    CorrectitudSemDebil = metrica(
      "CorrectitudSemDebil",
      "Indica si un valor est\u00e1 asociado a la identificaci\u00f3n correcta en un referencial.",
      "instanciaAtributo", "booleano", dimension = "Exactitud",
      factor = "Correctitud sem\u00e1ntica", metodo = .metodo_correctitud_debil
    ),
    RatioCobertura = metrica(
      "RatioCobertura",
      "Mide la cobertura de una entidad respecto de un referencial completo.",
      "entidad", "real", dimension = "Completitud", factor = "Cobertura",
      metodo = .metodo_ratio_cobertura
    )
  )
}
