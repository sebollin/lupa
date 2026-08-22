# `muestra = 1.5` se aceptaba y se perfilaba UNA fila, en silencio. Un usuario
# que pide una muestra y medio recibe un perfil sobre una fila sin enterarse, y
# la misma llamada por la via DBI da error. Ahora las dos rechazan el no entero.
#
# La unica diferencia que queda entre las dos vias es deliberada y esta
# documentada: `Inf` vale en memoria -significa "todas las filas"- y no vale
# contra un motor, donde hay que decir cuantas filas traer.
.validar_muestra <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      muestra < 1) {
    stop("`muestra` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  if (is.finite(muestra) && muestra != floor(muestra)) {
    stop(
      "`muestra` debe ser un entero positivo, o `Inf` para no muestrear.",
      call. = FALSE
    )
  }
  floor(muestra)
}

.muestrear_vector <- function(x, muestra) {
  limite_solicitado <- .validar_muestra(muestra)

  total <- length(x)
  limite <- min(total, limite_solicitado)
  if (total <= limite) {
    return(list(
      valores = x,
      total = total,
      analizados = total,
      muestreado = FALSE
    ))
  }

  indices <- unique(as.integer(round(seq.int(1, total, length.out = limite))))
  list(
    valores = x[indices],
    total = total,
    analizados = length(indices),
    muestreado = TRUE
  )
}

.tipo_declarado <- function(x) {
  if (inherits(x, "sfc")) {
    return(class(x)[[1L]])
  }
  if (is.matrix(x)) {
    return("matriz")
  }
  if (inherits(x, "integer64")) {
    return("integer64")
  }
  if (inherits(x, "POSIXt")) {
    return("fecha-hora")
  }
  if (inherits(x, "Date")) {
    return("fecha")
  }
  if (is.ordered(x)) {
    return("factor-ordenado")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.logical(x)) {
    return("logico")
  }
  if (is.integer(x)) {
    return("entero")
  }
  if (is.double(x)) {
    return("doble")
  }
  if (is.character(x)) {
    return("texto")
  }
  if (is.list(x)) {
    return("lista")
  }
  class(x)[[1L]]
}

.zona_horaria_origen <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_character_)
  zona <- attr(x, "tzone", exact = TRUE)
  if (length(zona) && !is.na(zona[[1L]]) && nzchar(zona[[1L]])) {
    as.character(zona[[1L]])
  } else {
    "sin_declarar"
  }
}

.fechas_civiles_distintas_utc <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_integer_)
  zona <- .zona_horaria_origen(x)
  if (identical(zona, "sin_declarar")) return(NA_integer_)
  presentes <- !is.na(x)
  if (!any(presentes)) return(0L)
  civil_origen <- format(x[presentes], "%Y-%m-%d", tz = zona)
  civil_utc <- format(x[presentes], "%Y-%m-%d", tz = "UTC")
  as.integer(sum(civil_origen != civil_utc, na.rm = TRUE))
}

.texto_valor <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) {
    return(NA_character_)
  }
  if (inherits(x, "POSIXt")) {
    return(format(x[[1L]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x[[1L]], "%Y-%m-%d"))
  }
  as.character(x[[1L]])
}

# ¿La clase de `x` sabe convertirse a texto por sí misma? Si define un método
# de `as.character()`, la conversión produce los valores del dato. Si no, la
# coerción cae en `as.character.default()`.
.tiene_metodo_as_character <- function(x) {
  for (clase in class(x)) {
    metodo <- tryCatch(
      utils::getS3method("as.character", clase, optional = TRUE),
      error = function(e) NULL
    )
    if (!is.null(metodo)) return(TRUE)
  }
  FALSE
}

# Una columna se puede analizar como texto cuando aplicarle una primitiva de
# texto devuelve sus valores. Sobre una lista de objetos —una `sfc`, una columna
# de WKB crudo, una lista de vectores— `as.character.default()` no devuelve
# valores: deparsa cada elemento a código fuente de R. Ese texto no describe el
# dato, es más largo que el dato mismo y se rehace una vez por cada etapa que lo
# toca, así que el costo del perfilado queda dominado por una conversión que
# nadie pidió y que nadie puede leer.
#
# Las listas de escalares atómicos sí se coercionan a sus valores y siguen
# siendo analizables: el corte separa lo que se convierte de lo que se deparsa,
# no lo que es lista de lo que no lo es.
.analizable_como_texto <- function(x) {
  if (!is.list(x)) return(TRUE)
  if (inherits(x, "sfc") || inherits(x, "sfg")) return(FALSE)
  if (.tiene_metodo_as_character(x)) return(TRUE)
  n <- length(x)
  if (!n) return(TRUE)
  largos <- tryCatch(lengths(x), error = function(e) NULL)
  if (is.null(largos) || length(largos) != n || anyNA(largos) ||
      !all(largos == 1L)) {
    return(FALSE)
  }
  all(vapply(x, is.atomic, logical(1L)))
}

.motivo_no_analizable_texto <- function(x) {
  paste0(
    "La columna es una lista de objetos (", .tipo_declarado(x),
    "); las etapas de texto no la evaluaron porque convertirla no produce sus ",
    "valores sino su representaci\u00f3n como c\u00f3digo."
  )
}

#' Preparar una columna para las etapas de texto
#'
#' Separa dos usos que hasta ahora viajaban juntos y que no son el mismo:
#' aplicar una primitiva de texto sobre los valores, y compararlos por
#' igualdad.
#'
#' @param x Columna que se va a analizar.
#'
#' @return Lista con cinco elementos.
#'   `valores` es lo que puede recibir una primitiva de texto. Cuando la
#'   columna no es analizable como texto, son ausentes declarados: la no
#'   evaluación se declara en vez de dejar pasar la columna intacta para que
#'   cada etapa la deparse por su cuenta.
#'   `valores_identidad` es lo que puede recibir una comparación por igualdad
#'   —`unique()`, `match()`, la moda, la inferencia de tipo—. Para toda columna
#'   analizable es el mismo vector que `valores`; para las demás es la columna
#'   original, de modo que contar sus distintos siga siendo posible sin
#'   convertirla a texto.
#'   `invalidos` y `posiciones` marcan los bytes UTF-8 inválidos aislados.
#'   `analizable` declara si la columna pasa por las etapas de texto y `motivo`
#'   dice por qué no, cuando corresponde.
#' @noRd
.texto_analizable <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    if (!.analizable_como_texto(x)) {
      n <- length(x)
      return(list(
        valores = rep(NA_character_, n),
        invalidos = rep(FALSE, n),
        posiciones = integer(),
        analizable = FALSE,
        valores_identidad = x,
        motivo = .motivo_no_analizable_texto(x)
      ))
    }
    return(list(
      valores = x, invalidos = rep(FALSE, length(x)), posiciones = integer(),
      analizable = TRUE, valores_identidad = x, motivo = NA_character_
    ))
  }
  valores <- as.character(x)
  # Lo que R sabe convertir se convierte, no se descarta.
  #
  # `validUTF8()` mira los bytes, y los de un texto marcado `latin1` no son UTF-8
  # validos. Sin este paso, una columna de un CSV viejo en espanol -el caso mas
  # comun que hay en datos publicos de la region- perdia todos sus valores
  # acentuados: `CAFE`, `ANO` y `NUMERO` sobrevivian y `CAFE con tilde`, `ANO con
  # tilde` y `NUMERO con tilde` se volvian NA. El perfil informaba entonces
  # `n_distintos = 2` sobre cinco valores distintos y `n_faltantes = 0`, sin
  # declarar nada: informar como medido lo que se descarto, que es exactamente lo
  # que el paquete promete no hacer.
  #
  # `Encoding()` dice `latin1` cuando R conoce la codificacion, y ahi `enc2utf8()`
  # convierte sin perder nada. Lo que queda invalido despues de eso es texto cuya
  # codificacion nadie declaro y no se puede adivinar; eso si se descarta, y se
  # informa en `invalidos` y `posiciones`.
  declarados <- Encoding(valores) %in% c("latin1", "UTF-8")
  if (any(declarados)) {
    convertidos <- suppressWarnings(
      tryCatch(enc2utf8(valores[declarados]), error = function(e) NULL)
    )
    if (!is.null(convertidos) && length(convertidos) == sum(declarados)) {
      valores[declarados] <- convertidos
    }
  }
  invalidos <- !is.na(valores) & !validUTF8(valores)
  posiciones <- which(invalidos)
  if (length(posiciones)) valores[posiciones] <- NA_character_
  list(
    valores = valores, invalidos = invalidos, posiciones = posiciones,
    analizable = TRUE, valores_identidad = valores, motivo = NA_character_
  )
}

# Copia operativa de una tabla: los factores se interpretan como texto para
# que los métodos no dependan de la versión de R ni de stringsAsFactors.
# El perfil conserva la columna original; esta función sólo se usa al ejecutar
# contratos que leen valores.
.normalizar_columnas_texto <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(tabla)
  factores <- vapply(tabla, is.factor, logical(1L))
  if (!any(factores)) return(tabla)
  salida <- tabla
  for (nombre in names(salida)[factores]) {
    salida[[nombre]] <- .texto_analizable(salida[[nombre]])$valores
  }
  salida
}

.columnas_identicas <- function(x, y) {
  identical(class(x), class(y)) &&
    identical(is.na(x), is.na(y)) &&
    identical(x, y)
}

.nombre_paquete <- function() {
  nombre <- utils::packageName()
  if (is.null(nombre)) "lupa" else nombre
}

.version_paquete <- function() {
  nombre <- .nombre_paquete()
  tryCatch(
    as.character(utils::packageVersion(nombre)),
    error = function(e) "desarrollo"
  )
}

.data_frame_vacio <- function() {
  data.frame(stringsAsFactors = FALSE)
}

.pegar_nombres <- function(x) {
  paste(x, collapse = " + ")
}
