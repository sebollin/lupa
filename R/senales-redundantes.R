# Señales redundantes: varias columnas que codifican el mismo hecho y se
# contradicen dentro de la misma fila.
#
# Es un defecto que **ninguna columna por separado muestra**: el año de la
# fecha, el año fiscal y el año del archivo pueden ser los tres valores
# perfectamente plausibles y aun así uno contradecir a los otros.
#
# El grupo lo declara quien conoce los datos. `lupa` no adivina qué columnas
# codifican el mismo hecho: dos columnas de año pueden ser el año de nacimiento
# y el año de ingreso, y no tienen por qué coincidir. Suponerlo sería inventar
# conocimiento del dominio, que es justo lo que este paquete no hace.

.validar_transformacion_senal <- function(transformacion, columnas) {
  if (is.null(transformacion)) return(NULL)
  if (!is.list(transformacion) || is.null(names(transformacion)) ||
      anyNA(names(transformacion)) || !all(nzchar(names(transformacion)))) {
    stop("`transformacion` debe ser una lista con nombres.", call. = FALSE)
  }
  sobrantes <- setdiff(names(transformacion), columnas)
  if (length(sobrantes)) {
    stop(
      "`transformacion` nombra columnas que no estan en la senal: ",
      paste(sobrantes, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!all(vapply(transformacion, is.function, logical(1L)))) {
    stop("Cada elemento de `transformacion` debe ser una funcion.",
         call. = FALSE)
  }
  transformacion
}

#' Declarar una señal redundante entre columnas
#'
#' Declara que varias columnas de la misma tabla codifican **el mismo hecho**,
#' de modo que [detectar_discordancias()] pueda informar las filas donde no
#' concuerdan. El grupo lo declara quien conoce los datos: `lupa` no lo adivina,
#' porque dos columnas de año pueden ser el año de nacimiento y el año de
#' ingreso y no tienen por qué coincidir.
#'
#' `transformacion` permite comparar columnas que guardan el hecho de formas
#' distintas —por ejemplo extraer el año de una fecha para compararlo con una
#' columna de año—. Cada función recibe la columna entera y devuelve un vector
#' de la misma longitud.
#'
#' `ventana` es la tolerancia, **en las unidades del valor transformado**: con
#' años, `ventana = 1` acepta un año de diferencia. La unidad no se adivina; es
#' la del resultado de la transformación.
#'
#' @param columnas Nombres de al menos dos columnas que codifican el mismo
#'   hecho.
#' @param ventana Tolerancia máxima admitida entre los valores, en las unidades
#'   del valor comparado. Por omisión `0`: coincidencia exacta.
#' @param transformacion Lista opcional con nombres de columna y funciones que
#'   las llevan a una escala comparable.
#' @param nombre Etiqueta de la señal. Por omisión, las columnas unidas.
#'
#' @return Objeto de clase `senal_redundante`: una lista con `nombre` -el
#'   declarado, o las columnas unidas por `=`-, el vector `columnas` que la
#'   señal relaciona, la `ventana` numérica y la `transformacion` validada.
#'   Describe la relación esperada; no la evalúa.
#' @export
#' @seealso [detectar_discordancias()], [detectar_dependencias()]
#'
#' @examples
#' senal_redundante(c("anio_fiscal", "anio_archivo"))
#' senal_redundante(
#'   c("fecha", "anio_fiscal"),
#'   transformacion = list(fecha = function(x) as.integer(format(x, "%Y")))
#' )
senal_redundante <- function(columnas, ventana = 0, transformacion = NULL,
                             nombre = NULL) {
  if (!is.character(columnas) || length(columnas) < 2L ||
      anyNA(columnas) || !all(nzchar(columnas))) {
    stop("`columnas` debe nombrar al menos dos columnas.", call. = FALSE)
  }
  if (anyDuplicated(columnas)) {
    stop("`columnas` repite una columna.", call. = FALSE)
  }
  if (!is.numeric(ventana) || length(ventana) != 1L || is.na(ventana) ||
      !is.finite(ventana) || ventana < 0) {
    stop("`ventana` debe ser un numero finito no negativo.", call. = FALSE)
  }
  transformacion <- .validar_transformacion_senal(transformacion, columnas)
  if (!is.null(nombre) && !.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser NULL o una cadena no vacia.", call. = FALSE)
  }
  estructura <- list(
    nombre = if (is.null(nombre)) paste(columnas, collapse = " = ") else nombre,
    columnas = columnas,
    ventana = as.numeric(ventana),
    transformacion = transformacion
  )
  class(estructura) <- "senal_redundante"
  estructura
}

#' @export
print.senal_redundante <- function(x, ...) {
  cli::cli_text("Se\u00f1al redundante: {.strong {x$nombre}}")
  cli::cli_text("Columnas: {paste(x$columnas, collapse = ', ')}")
  cli::cli_text("Ventana: {x$ventana}")
  if (length(x$transformacion)) {
    cli::cli_text(
      "Transformadas: {paste(names(x$transformacion), collapse = ', ')}"
    )
  }
  invisible(x)
}

.valores_senal <- function(datos, senal) {
  lapply(senal$columnas, function(columna) {
    valores <- datos[[columna]]
    funcion <- senal$transformacion[[columna]]
    if (!is.null(funcion)) valores <- funcion(valores)
    if (length(valores) != nrow(datos)) {
      stop(
        "La transformacion de `", columna, "` devolvio ", length(valores),
        " valores para ", nrow(datos), " filas.", call. = FALSE
      )
    }
    valores
  })
}

.discordancia_vacia <- function() {
  data.frame(
    senal = character(), columnas = character(), n_filas = numeric(),
    n_evaluadas = numeric(), n_discordantes = numeric(),
    proporcion = numeric(), ventana = numeric(), evidencia = character(),
    stringsAsFactors = FALSE
  )
}

#' Detectar filas donde señales redundantes se contradicen
#'
#' Recorre las señales declaradas con [senal_redundante()] e informa las filas
#' donde las columnas que deberían codificar el mismo hecho no concuerdan dentro
#' de la ventana declarada.
#'
#' El valor de este diagnóstico es que **ninguna columna por separado lo
#' muestra**: los tres años de una fila pueden ser todos plausibles y aun así
#' contradecirse entre sí.
#'
#' Una fila donde alguna de las columnas comparadas está ausente **no se cuenta
#' como discordante ni como concordante**: se excluye del universo evaluado, y
#' `n_evaluadas` lo declara. Ausencia no es desacuerdo.
#'
#' @param datos Tabla que se desea examinar.
#' @param senales Una señal creada por [senal_redundante()] o una lista de
#'   señales.
#' @param max_ejemplos Máximo de filas concretas que se citan como evidencia.
#'
#' @return Data frame con una fila por señal: `senal`, `columnas`, `n_filas`,
#'   `n_evaluadas`, `n_discordantes`, `proporcion`, `ventana` y `evidencia`.
#' @export
#' @seealso [senal_redundante()], [detectar_dependencias()],
#'   [detectar_relaciones()]
#'
#' @examples
#' d <- data.frame(
#'   anio_fiscal = c(2023L, 2023L, 2022L),
#'   anio_archivo = c(2023L, 2023L, 2023L)
#' )
#' detectar_discordancias(d, senal_redundante(c("anio_fiscal", "anio_archivo")))
detectar_discordancias <- function(datos, senales, max_ejemplos = 5L) {
  .validar_datos_tabla(datos)
  datos <- .tabla_base(datos)
  if (inherits(senales, "senal_redundante")) senales <- list(senales)
  if (!is.list(senales) || !length(senales) ||
      !all(vapply(senales, inherits, logical(1L), "senal_redundante"))) {
    stop(
      "`senales` debe ser una senal_redundante o una lista de ellas.",
      call. = FALSE
    )
  }
  if (!is.numeric(max_ejemplos) || length(max_ejemplos) != 1L ||
      is.na(max_ejemplos) || max_ejemplos < 0) {
    stop("`max_ejemplos` debe ser un entero no negativo.", call. = FALSE)
  }
  max_ejemplos <- as.integer(max_ejemplos)

  filas <- lapply(senales, function(senal) {
    faltantes <- setdiff(senal$columnas, names(datos))
    if (length(faltantes)) {
      stop(
        "La senal '", senal$nombre, "' nombra columnas que no estan en los ",
        "datos: ", paste(faltantes, collapse = ", "),
        ". Disponibles: ", paste(names(datos), collapse = ", "), ".",
        call. = FALSE
      )
    }
    valores <- .valores_senal(datos, senal)
    completos <- Reduce(`&`, lapply(valores, function(x) !is.na(x)))
    n_evaluadas <- sum(completos)
    if (!n_evaluadas) {
      return(data.frame(
        senal = senal$nombre,
        columnas = paste(senal$columnas, collapse = ", "),
        n_filas = as.numeric(nrow(datos)), n_evaluadas = 0,
        n_discordantes = NA_real_, proporcion = NA_real_,
        ventana = senal$ventana,
        evidencia = paste(
          "No hubo filas con todas las columnas presentes: la senal no se",
          "evaluo."
        ),
        stringsAsFactors = FALSE
      ))
    }
    comparables <- lapply(valores, function(x) x[completos])
    numericos <- all(vapply(comparables, is.numeric, logical(1L)))
    discordante <- if (numericos) {
      extremos <- do.call(pmax, comparables) - do.call(pmin, comparables)
      extremos > senal$ventana
    } else {
      referencia <- as.character(comparables[[1L]])
      Reduce(`|`, lapply(comparables[-1L], function(x) {
        as.character(x) != referencia
      }))
    }
    indices <- which(completos)[discordante]
    ejemplos <- utils::head(indices, max_ejemplos)
    detalle <- if (length(ejemplos)) {
      paste(vapply(ejemplos, function(fila) {
        partes <- vapply(seq_along(senal$columnas), function(k) {
          paste0(senal$columnas[[k]], "=", .texto_valor(valores[[k]][fila]))
        }, character(1L))
        paste0("fila ", fila, ": ", paste(partes, collapse = "; "))
      }, character(1L)), collapse = " | ")
    } else {
      "sin filas discordantes"
    }
    data.frame(
      senal = senal$nombre,
      columnas = paste(senal$columnas, collapse = ", "),
      n_filas = as.numeric(nrow(datos)),
      n_evaluadas = as.numeric(n_evaluadas),
      n_discordantes = as.numeric(length(indices)),
      proporcion = length(indices) / n_evaluadas,
      ventana = senal$ventana,
      evidencia = paste0(
        detalle,
        "; universo: ", n_evaluadas, " de ", nrow(datos),
        " filas con todas las columnas presentes",
        if (!numericos) "; comparacion textual exacta" else "",
        if (senal$ventana > 0 && numericos) {
          paste0("; ventana declarada: ", senal$ventana)
        } else ""
      ),
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  if (is.null(salida)) salida <- .discordancia_vacia()
  rownames(salida) <- NULL
  class(salida) <- c("discordancias_senales", "data.frame")
  salida
}
