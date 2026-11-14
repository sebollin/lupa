# Un nombre de columna sin marca que no es UTF-8 valido -lo que deja
# `data.table::fread()` sobre un CSV en latin-1- viaja adentro de las
# columnas-lista de las tablas publicadas: las claves de la traza son un
# `data.frame` con ese nombre, y la cobertura y la propuesta guardan los nombres
# de las columnas. `print.data.frame()` de base no puede formatear esas listas en
# una sesion UTF-8 y abortaba al escribir `hallazgos(p)` en la consola. Bajo un
# locale que no es UTF-8, "sin marca" es la codificacion nativa y el caso no
# existe.

testthat::skip_on_cran()

.datos_nombre_no_utf8 <- function() {
  byte <- rawToChar(as.raw(0xA0))
  datos <- data.frame(id = c(1:12, NA))
  datos[[paste0("ciudad", byte)]] <- c(
    "Montevideo", "montevideo", "Montevido", "Canelones", "canelones",
    "Canelones", "Maldonado", "Maldonado", "Salto", "Salto", "Montevideo",
    "Canelones", NA
  )
  datos[[paste0("importe", byte)]] <- c(
    "12,5", "13,1", "14,2", "15,0", "16,3", "2,2", "3,3", "4,4", "5,5", "6,6",
    "7,7", "8,8", NA
  )
  datos
}

.se_imprime <- function(x) {
  salida <- utils::capture.output(print(x))
  length(salida) > 0L
}

test_that("hallazgos() y cobertura() se imprimen con un nombre de columna que no es UTF-8", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  datos <- .datos_nombre_no_utf8()
  clave <- names(datos)[[2L]]
  expect_false(validUTF8(clave))
  # La clave del fixture repite valores y tiene un ausente: el aviso de la
  # clave declarada es el de siempre y no es lo que se prueba aca.
  expect_warning(
    perfil <- perfilar(datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
                       clave = clave),
    "no es \u00fanica"
  )

  h <- hallazgos(perfil)
  # La tabla trae lo que abortaba: una clave de la traza con ese nombre.
  claves <- Filter(is.data.frame, lapply(h$trazabilidad, `[[`, "claves"))
  expect_true(length(claves) > 0L)
  expect_true(any(vapply(claves, function(k) any(!validUTF8(names(k))), TRUE)))
  expect_true(.se_imprime(h))
  expect_true(.se_imprime(h[, "trazabilidad", drop = FALSE]))
  # La impresion nombra la columna con su byte escapado, no la calla.
  expect_true(any(grepl("ciudad\\\\240", utils::capture.output(print(h)),
                        fixed = TRUE)))

  cb <- cobertura(perfil)
  expect_true(any(vapply(cb$columnas, function(n) any(!validUTF8(n)), TRUE)))
  expect_true(.se_imprime(cb))
  expect_true(.se_imprime(cb[, "columnas", drop = FALSE]))

  propuesta <- proponer_modelo(perfil, datos)
  expect_true(.se_imprime(propuesta[, "atributos_ligados", drop = FALSE]))
})

test_that("la clase de impresion no cambia ningun valor", {
  datos <- .datos_nombre_no_utf8()
  expect_warning(
    perfil <- perfilar(datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
                       clave = names(datos)[[2L]]),
    "no es \u00fanica"
  )
  h <- hallazgos(perfil)
  cb <- cobertura(perfil)
  expect_s3_class(h, "data.frame")
  expect_s3_class(cb, "data.frame")
  # Es la tabla del perfil, con otra impresion: `as.data.frame()` la devuelve.
  expect_identical(as.data.frame(h), perfil$hallazgos)
  expect_identical(as.data.frame(cb), perfil$cobertura_diagnosticos)
  # La propuesta sigue siendo una propuesta, y su subconjunto sin el contrato
  # sigue sin serlo.
  propuesta <- proponer_modelo(perfil, datos)
  expect_s3_class(propuesta, "propuesta_modelo")
  expect_false(inherits(propuesta[, "atributos_ligados", drop = FALSE],
                        "propuesta_modelo"))
})
