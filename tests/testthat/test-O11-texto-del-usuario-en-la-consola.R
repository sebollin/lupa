# O11. El texto del usuario no se ejecuta ni aborta al imprimirse.
#
# cli lee su primer argumento como plantilla de glue: todo `{...}` adentro es
# una expresion que se EVALUA. El paquete armaba esa plantilla con `paste()` e
# incluia texto del usuario. Medido antes del arreglo:
#
#   * `perfilar(d, nombre = "tabla{1+1}")` se imprimia `tabla2`;
#   * una columna llamada `tasa{Sys.getenv("USER")}` se imprimia con el nombre
#     del usuario al imprimir el plan: el encabezado de un CSV ejecutaba codigo;
#   * un nombre de columna declarado `bytes` abortaba `print()` del plan;
#   * `reportar()` abortaba sobre un perfil con texto sin marca que no es
#     UTF-8 -lo que deja `read.csv()` sin `fileEncoding`-, perfil que el propio
#     paquete habia producido.
#
# Las cadenas con bytes altos se construyen con `rawToChar()`: la fuente es ASCII.

.o11_salida <- function(expr) {
  mensajes <- character()
  salida <- withCallingHandlers(
    utils::capture.output(expr),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  paste(c(mensajes, salida), collapse = "\n")
}

.o11_marcar <- function(bytes, codificacion) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- codificacion
  s
}

test_that("el nombre de la tabla se imprime tal cual, sin evaluarse", {
  datos <- data.frame(a = c("x", "y", "x"), n = 1:3)
  perfil <- perfilar(datos, nombre = "tabla{1+1}")
  salida <- .o11_salida(print(perfil))
  expect_match(salida, "tabla{1+1}", fixed = TRUE)
  expect_false(grepl("tabla2", salida, fixed = TRUE))
})

test_that("un nombre de columna y un valor con llaves no se evaluan en la guia", {
  datos <- data.frame(
    x = c("Salto ", " r{1+1}s ", " r{1+1}s ", "Rivera", "Salto", "Rivera"),
    n = 1:6, stringsAsFactors = FALSE
  )
  names(datos)[1] <- "c{2+2}"
  plan <- planificar_limpieza(perfilar(datos), datos = datos)
  expect_true(any(as.character(plan$estrategia) == "recortar_espacios"))
  salida <- .o11_salida(guiar_limpieza(plan, datos, selector = function(...) 0))
  # La premisa: la guia publica la columna. Si deja de hacerlo, no mide.
  expect_match(salida, "c{2+2}", fixed = TRUE)
  expect_false(grepl("c4", salida, fixed = TRUE))
  expect_false(grepl("r2s", salida, fixed = TRUE))
})

test_that("un nombre de columna declarado bytes no aborta la impresion del plan", {
  roto <- .o11_marcar(c(0x63, 0x61, 0x66, 0xe9), "bytes")
  datos <- data.frame(t = c(rep(roto, 6), rep("te", 6), "cafe", "cafes"),
                      n = 1:14, stringsAsFactors = FALSE)
  names(datos)[1] <- roto
  plan <- planificar_limpieza(perfilar(datos), datos = datos)
  # La premisa: el plan avisa de diagnosticos no evaluados sobre esa columna,
  # que es la linea que abortaba.
  expect_true(nrow(attr(plan, "cobertura_diagnosticos")) > 0L)
  salida <- expect_no_error(.o11_salida(print(plan)))
  expect_match(salida, "caf\\xe9", fixed = TRUE)
  # Y el nombre no arrastra a la frase del paquete: `paste()` marcaba `bytes`
  # la frase entera y salia `diagn\\xc3\\xb3sticos`, escapada con el nombre.
  expect_false(grepl("\\xc3", salida, fixed = TRUE))
})

test_that("reportar publica un perfil con texto sin marca que no es UTF-8", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  roto <- .o11_marcar(c(0x63, 0x61, 0x66, 0xe9), "unknown")
  expect_false(validUTF8(roto))
  datos <- data.frame(t = c("cafe", roto, "cafe", roto), stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_no_error(reportar(perfil, archivo = archivo))
  html <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  expect_true(all(validUTF8(html)))
  # Se publica como lo muestra la consola, igual que lo declarado `bytes`.
  expect_true(any(grepl("caf\\xe9", html, fixed = TRUE)))
})

test_that("un nombre sin marca y el mismo declarado bytes se citan igual", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  evidencia <- function(marca) {
    datos <- data.frame(a = 1:3, n = 1:3)
    names(datos)[1] <- .o11_marcar(c(0x63, 0x61, 0x66, 0xe9), marca)
    h <- as.data.frame(hallazgos(perfilar(datos)))
    h$evidencia[h$tipo_hallazgo == "nombres_columnas_problematicos"]
  }
  sin_marca <- evidencia("unknown")
  expect_length(sin_marca, 1L)
  expect_identical(sin_marca, evidencia("bytes"))
  expect_match(sin_marca, "\"caf\\xe9\"", fixed = TRUE)
})

test_that("ninguna llamada a cli recibe una plantilla armada sin neutralizar", {
  # La guarda que corre sola: recorre el cuerpo de cada funcion del paquete y
  # exige que la plantilla de toda llamada a cli sea un literal o pase por
  # `.cli_literal()`. Una llamada nueva con `paste()` falla aca, no en la
  # consola de un usuario.
  ns <- asNamespace("lupa")
  malas <- character()
  revisadas <- 0L
  recorrer <- function(e, donde) {
    if (!is.call(e)) return(invisible())
    f <- e[[1L]]
    if (is.call(f) && identical(f[[1L]], as.name("::")) &&
        identical(as.character(f[[2L]]), "cli") &&
        grepl("^cli_", as.character(f[[3L]])) &&
        !grepl("^cli_progress", as.character(f[[3L]])) &&
        length(e) >= 2L && (is.null(names(e)) || !nzchar(names(e)[2L]))) {
      revisadas <<- revisadas + 1L
      primero <- e[[2L]]
      literal <- is.character(primero)
      neutralizado <- is.call(primero) &&
        identical(primero[[1L]], as.name(".cli_literal"))
      if (!literal && !neutralizado) {
        malas <<- c(malas, paste0(donde, ": ", paste(deparse(primero), collapse = " ")))
      }
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]], donde)
    }
  }
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto)) recorrer(body(objeto), nombre)
  }
  # Si el recorrido deja de encontrar llamadas, no mide nada: que falle.
  expect_gt(revisadas, 100L)
  expect_identical(malas, character())
})
