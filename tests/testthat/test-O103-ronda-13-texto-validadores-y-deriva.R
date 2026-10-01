# Ronda 13: reparacion de texto, validadores y deriva entre perfiles.

test_that("los validadores de URL rechazan tres formas que no son sintaxis", {
  # Antes: TRUE las tres.
  expect_false(validar_url("http://[:::]/"))
  expect_false(validar_url("http://ejemplo.uy.."))
  expect_false(validar_url("http://a@b@ejemplo.uy/"))
  # Control: las formas legitimas vecinas siguen pasando.
  expect_true(all(validar_url(c(
    "http://[::]/", "http://[::1]/", "http://[1::]/", "http://ejemplo.uy./",
    "http://a@ejemplo.uy/", "https://ejemplo.uy/ruta?x=1"
  ))))
})

test_that("un documento con un separador al borde no es un documento", {
  # La documentacion promete que no se le quita el signo para hacerlo pasar.
  expect_false(any(validar_ci_uy(c("-12345672", "12345672-", ".1234567.2."))))
  expect_false(validar_rut_uy("-211003420017"))
  # Control: las escrituras con separadores entre digitos siguen pasando.
  expect_true(all(validar_ci_uy(c("1.234.567-2", "12345672", "1234567-2"))))
})

test_that("un texto mal convertido que el motor no puede reparar produce hallazgo", {
  perfil <- suppressWarnings(perfilar(
    data.frame(x = c("\u0101\u20ac", "ok", "otro"), stringsAsFactors = FALSE)
  ))
  # Antes: `n_codificacion_no_se_pudo = 1` y ningun hallazgo ni accion.
  expect_equal(perfil$columnas$n_codificacion_rota, 1)
  expect_true("codificacion_rota" %in% as.character(hallazgos(perfil)$tipo_hallazgo))
  # Control: un texto sano no produce el hallazgo.
  sano <- suppressWarnings(perfilar(data.frame(x = c("caf\u00e9", "ok"))))
  expect_false("codificacion_rota" %in% as.character(hallazgos(sano)$tipo_hallazgo))
})

test_that("la deriva declara un cambio de cadenas de ausencia y de tamano", {
  set.seed(1)
  d <- data.frame(id = 1:200, telefono = sample(c("555-0100", "zzz"), 200, TRUE,
                                                prob = c(0.7, 0.3)),
                  stringsAsFactors = FALSE)
  f1 <- as.POSIXct("2026-01-01", tz = "UTC")
  f2 <- as.POSIXct("2026-02-01", tz = "UTC")
  con <- suppressWarnings(perfilar(d, nombre = "t", fecha = f1,
                                   cadenas_ausencia = "zzz"))
  sin <- suppressWarnings(perfilar(d, nombre = "t", fecha = f2))
  deriva <- as.data.frame(comparar_perfiles(con, sin))
  # Antes: ninguna fila decia que la politica habia cambiado.
  expect_true("configuracion_cadenas_ausencia" %in% deriva$aspecto)
  # Control: la misma politica en las dos no publica esa fila.
  igual <- as.data.frame(comparar_perfiles(
    con, suppressWarnings(perfilar(d, nombre = "t", fecha = f2,
                                   cadenas_ausencia = "zzz"))
  ))
  expect_false("configuracion_cadenas_ausencia" %in% igual$aspecto)

  # El tamano: la tabla duplica sus filas con valores nuevos.
  chica <- suppressWarnings(perfilar(data.frame(id = 1:300), nombre = "t", fecha = f1))
  grande <- suppressWarnings(perfilar(data.frame(id = 1:600), nombre = "t", fecha = f2))
  crece <- as.data.frame(comparar_perfiles(chica, grande))
  fila <- crece[crece$aspecto == "filas", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "ok")
  # El anterior tiene que ser el mas antiguo: se rehacen con las fechas al reves.
  grande_antes <- suppressWarnings(perfilar(data.frame(id = 1:600), nombre = "t",
                                            fecha = f1))
  chica_despues <- suppressWarnings(perfilar(data.frame(id = 1:300), nombre = "t",
                                             fecha = f2))
  achica <- as.data.frame(comparar_perfiles(grande_antes, chica_despues))
  expect_identical(
    as.character(achica$severidad[achica$aspecto == "filas"]), "sospechoso"
  )
})
