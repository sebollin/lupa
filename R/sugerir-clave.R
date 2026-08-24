# Sugerir la clave en vez de adivinarla.
#
# Adivinar si una columna es clave mirando solo sus valores no funciona -un
# monto tambien es casi unico- y ese camino ya se probo y se retiro. Preguntar
# si funciona, y preguntar bien es ofrecer las candidatas ordenadas en vez de
# una casilla en blanco: quien conoce la tabla reconoce su clave de un vistazo,
# pero no tiene por que acordarse de como se llama la columna.

# Nombres que en un padron o un registro administrativo casi siempre nombran la
# clave. Se comparan sobre el nombre normalizado -sin acentos, sin separadores y
# en minusculas- para que `ID_Persona`, `idpersona` y `id.persona` pesen igual.
.NOMBRES_CLAVE_PROBABLE <- c(
  "id", "clave", "codigo", "cod", "documento", "doc", "cedula", "ci",
  "nro", "numero", "num", "folio", "expediente", "matricula", "padron",
  "ruc", "rut", "nif", "dni", "legajo", "registro"
)

.normalizar_nombre_columna <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "", iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  x
}

# Cuanto se parece el nombre a un nombre de clave. Tres niveles, no un continuo:
# el nombre entero, el nombre que empieza o termina con la palabra, y el que la
# contiene en el medio. Un `id_persona` pesa mas que un `partido_id` y los dos
# mas que un `validador`.
.puntaje_nombre_clave <- function(nombre) {
  n <- .normalizar_nombre_columna(nombre)
  if (!nzchar(n)) return(0)
  if (n %in% .NOMBRES_CLAVE_PROBABLE) return(3)
  empieza <- any(vapply(
    .NOMBRES_CLAVE_PROBABLE, function(p) startsWith(n, p), logical(1L)
  ))
  termina <- any(vapply(
    .NOMBRES_CLAVE_PROBABLE, function(p) endsWith(n, p), logical(1L)
  ))
  if (empieza || termina) return(2)
  contiene <- any(vapply(
    .NOMBRES_CLAVE_PROBABLE, function(p) grepl(p, n, fixed = TRUE), logical(1L)
  ))
  if (contiene) return(1)
  0
}

#' Sugerir qué columnas podrían ser la clave
#'
#' Ordena las columnas que **podrían** identificar una fila, para que quien
#' conoce la tabla elija. No decide: una columna única puede ser una clave o
#' puede ser una magnitud que no repite, y esa diferencia no está en los datos.
#'
#' El orden combina tres señales, y las tres se publican para que se pueda
#' discutir el orden en vez de aceptarlo: si la columna **identifica** cada fila
#' sin repetir, si **no tiene ausentes** —una clave con ausentes no identifica—,
#' y cuánto se **parece su nombre** al de una clave (`id_persona`, `documento`,
#' `expediente`). El nombre se compara sin acentos ni separadores, así que
#' `ID_Persona` e `idpersona` pesan igual.
#'
#' Las columnas que repiten valores no se ofrecen como clave de una sola
#' columna, pero sí se informan cuando estuvieron cerca: una columna que
#' identifica al 99 % suele ser una clave con duplicados de carga, que es
#' exactamente lo que conviene mirar.
#'
#' @param datos Tabla a examinar.
#' @param maximo Cuántas sugerencias devolver como máximo.
#' @param umbral_casi Proporción de valores distintos a partir de la cual una
#'   columna que repite se informa igual, como candidata con duplicados.
#'
#' @return Un `data.frame` con una fila por columna candidata, ordenado de más a
#'   menos probable, con las tres señales por separado y el motivo en texto.
#'   Cero filas si ninguna columna se acerca.
#'
#' @examples
#' personas <- data.frame(
#'   id_persona = 1:4,
#'   documento = c(11111111, 22222222, 33333333, 33333333),
#'   edad = c(30, 41, 25, 25)
#' )
#' sugerir_clave(personas)
#'
#' @seealso [detectar_claves()] para las combinaciones de varias columnas, y el
#'   argumento `clave` de [perfilar()] para declarar la que se elija.
#' @export
sugerir_clave <- function(datos, maximo = 5L, umbral_casi = 0.95) {
  .validar_datos_tabla(datos)
  if (!is.numeric(maximo) || length(maximo) != 1L || is.na(maximo) ||
        maximo < 1) {
    stop("`maximo` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.numeric(umbral_casi) || length(umbral_casi) != 1L ||
        is.na(umbral_casi) || umbral_casi <= 0 || umbral_casi > 1) {
    stop("`umbral_casi` debe estar en (0, 1].", call. = FALSE)
  }
  vacio <- data.frame(
    columna = character(), identifica = logical(), sin_faltantes = logical(),
    tasa_distintos = numeric(), parecido_nombre = integer(),
    motivo = character(), stringsAsFactors = FALSE
  )
  if (!ncol(datos) || !nrow(datos)) return(vacio)

  filas <- lapply(names(datos), function(nombre) {
    x <- datos[[nombre]]
    if (is.list(x) && !inherits(x, "POSIXlt")) return(NULL)
    n_validos <- sum(!is.na(x))
    if (!n_validos) return(NULL)
    distintos <- length(unique(x[!is.na(x)]))
    tasa <- distintos / n_validos
    identifica <- distintos == nrow(datos) && n_validos == nrow(datos)
    if (!identifica && tasa < umbral_casi) return(NULL)
    data.frame(
      columna = nombre, identifica = identifica,
      sin_faltantes = n_validos == nrow(datos), tasa_distintos = tasa,
      parecido_nombre = as.integer(.puntaje_nombre_clave(nombre)),
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  if (!length(filas)) return(vacio)
  salida <- do.call(rbind, filas)

  # El orden: primero las que identifican, despues las que no tienen ausentes,
  # despues el parecido del nombre, y a igualdad la que aparece antes en la
  # tabla -la clave suele ser la primera columna-.
  posicion <- match(salida$columna, names(datos))
  salida <- salida[order(
    -salida$identifica, -salida$sin_faltantes, -salida$parecido_nombre,
    -salida$tasa_distintos, posicion
  ), , drop = FALSE]

  salida$motivo <- vapply(seq_len(nrow(salida)), function(i) {
    partes <- character()
    # Se separan las dos razones por las que una columna puede no identificar,
    # porque piden arreglos distintos: si repite hay duplicados de carga o la
    # clave es otra; si tiene ausentes, la columna esta incompleta. Decir "el
    # resto repite" sobre una columna que no repite nada y solo tiene un vacio
    # manda a buscar lo que no esta.
    repite <- salida$tasa_distintos[[i]] < 1
    partes <- c(partes, if (salida$identifica[[i]]) {
      "identifica cada fila sin repetir"
    } else if (repite) {
      sprintf(
        "no identifica: %.1f%% de valores distintos, el resto repite",
        100 * salida$tasa_distintos[[i]]
      )
    } else {
      "no identifica: sus valores no repiten, pero no estan en todas las filas"
    })
    if (!salida$sin_faltantes[[i]]) {
      partes <- c(partes, "una clave con ausentes no identifica")
    }
    if (salida$parecido_nombre[[i]] >= 2L) {
      partes <- c(partes, "su nombre es el de una clave")
    } else if (salida$parecido_nombre[[i]] == 1L) {
      partes <- c(partes, "su nombre contiene una palabra de clave")
    }
    paste(partes, collapse = "; ")
  }, character(1L))

  salida <- utils::head(salida, as.integer(maximo))
  rownames(salida) <- NULL
  salida[, c(
    "columna", "identifica", "sin_faltantes", "tasa_distintos",
    "parecido_nombre", "motivo"
  )]
}

#' Elegir la clave entre las sugeridas
#'
#' Muestra las candidatas de [sugerir_clave()] numeradas, con el motivo de cada
#' una, y devuelve la que se elija para pasarla al argumento `clave` de
#' [perfilar()]. La última opción permite escribir un nombre que no esté en la
#' lista, o varios separados por coma para una clave compuesta.
#'
#' **En una sesión no interactiva no pregunta**: devuelve `NULL` y avisa qué
#' habría ofrecido. Un guion que corre solo no puede quedarse esperando una
#' respuesta, y elegir por su cuenta sería exactamente lo que esta función
#' existe para no hacer.
#'
#' @inheritParams sugerir_clave
#'
#' @return El nombre —o los nombres— de la clave elegida, o `NULL` si no se
#'   eligió ninguna o la sesión no es interactiva.
#'
#' @examples
#' personas <- data.frame(id_persona = 1:3, edad = c(30, 41, 25))
#' # En una sesion interactiva pregunta; aqui devuelve NULL y dice que ofreceria.
#' elegir_clave(personas)
#'
#' @seealso [sugerir_clave()], y el argumento `clave` de [perfilar()].
#' @export
elegir_clave <- function(datos, maximo = 5L, umbral_casi = 0.95) {
  sugerencias <- sugerir_clave(datos, maximo = maximo, umbral_casi = umbral_casi)
  if (!interactive()) {
    if (nrow(sugerencias)) {
      cli::cli_alert_info(paste0(
        "Sesi\u00f3n no interactiva: no se pregunta. Se habr\u00eda ofrecido ",
        paste0("`", sugerencias$columna, "`", collapse = ", "),
        ". Usar `sugerir_clave()` y pasar la elegida a `perfilar(clave = ...)`."
      ))
    } else {
      cli::cli_alert_info(
        "Sesi\u00f3n no interactiva, y ninguna columna se acerca a ser clave."
      )
    }
    return(NULL)
  }
  if (!nrow(sugerencias)) {
    cli::cli_alert_warning(
      "Ninguna columna identifica una fila ni se le acerca."
    )
    escrita <- readline("Nombre de la clave (vacio para ninguna): ")
    return(.clave_escrita(escrita, datos))
  }
  cli::cli_h2("Cual es la clave de esta tabla?")
  for (i in seq_len(nrow(sugerencias))) {
    cli::cli_text("{i}. {.field {sugerencias$columna[[i]]}} - {sugerencias$motivo[[i]]}")
  }
  otra <- nrow(sugerencias) + 1L
  cli::cli_text("{otra}. Otra (escribirla; separar con coma si son varias)")
  cli::cli_text("0. Ninguna")
  respuesta <- readline("Numero: ")
  numero <- suppressWarnings(as.integer(trimws(respuesta)))
  if (is.na(numero) || numero < 0L || numero > otra) {
    cli::cli_alert_warning("Respuesta no reconocida; no se declara clave.")
    return(NULL)
  }
  if (numero == 0L) return(NULL)
  if (numero == otra) {
    return(.clave_escrita(readline("Nombre o nombres: "), datos))
  }
  sugerencias$columna[[numero]]
}

.clave_escrita <- function(texto, datos) {
  nombres <- trimws(strsplit(as.character(texto), ",", fixed = TRUE)[[1L]])
  nombres <- nombres[nzchar(nombres)]
  if (!length(nombres)) return(NULL)
  faltan <- setdiff(nombres, names(datos))
  if (length(faltan)) {
    cli::cli_alert_danger(paste0(
      "No existe en la tabla: ", paste0("`", faltan, "`", collapse = ", "), "."
    ))
    return(NULL)
  }
  nombres
}
