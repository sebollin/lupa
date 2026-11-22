# Bateria de conformidad en relaciones derivadas y uso optativo en medir_dbi().

.conectar_conformidad_dbi <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

test_that("conformidad_dbi coincide en SQLite y DuckDB", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"),
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "E",
                           valores = c("A", "B")), "d", "t"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "R",
                           minimo = -1, maximo = 1), "d", "n"),
    instanciar(especializar(nuc$AtributoDuplicado, "D"), "d", "t"),
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "P",
                           regla = ~ !is.na(n) & n > 1), "d", "n"),
    instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"),
               c("r", "d"), c("id", "fk"))
  )
  for (motor in c("sqlite", "duckdb")) {
    con <- .conectar_conformidad_dbi(motor)
    antes <- DBI::dbListTables(con)
    z <- conformidad_dbi(con, mod)
    despues <- DBI::dbListTables(con)
    expect_s3_class(z, "conformidad_dbi")
    expect_identical(attr(z, "motor", exact = TRUE), motor)
    expect_identical(antes, despues)
    expect_true(any(z$conformidad == "conforme"))
    expect_true(any(z$coincide[z$conformidad == "conforme"]))
    if (identical(motor, "sqlite")) {
      expect_true(any(z$conformidad == "abstencion"))
      expect_true(any(grepl("UTF-8", z$motivo, fixed = TRUE)))
    } else {
      expect_true(all(z$conformidad == "conforme"))
      expect_true(all(z$coincide))
    }
    if (identical(motor, "duckdb")) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }
})

test_that("una funcion ausente deja la regla no_medible y se aplica a medir_dbi", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  mala <- modelo(instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "Mala",
                 regla = ~ nchar(t) > 1),
    "d", "t"
  ))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  bateria <- conformidad_dbi(con, mala)
  expect_true(all(bateria$conformidad == "abstencion"))
  expect_true(any(grepl("nchar", bateria$motivo, fixed = TRUE)))
  expect_true(any(bateria$accion == "medir por bloques en R"))

  buena <- modelo(instanciar(
    especializar(nuc$ValoresPosiblesPorExtension, "Dominio",
                 valores = c("A", "B")),
    "d", "t"
  ))
  DBI::dbWriteTable(con, "d", data.frame(t = c("A", "B"),
                                          stringsAsFactors = FALSE))
  normal <- medir_dbi(buena, con, id_medicion = "normal",
                      fecha = .mdt_fecha, proteger_datos_personales = TRUE)
  bateria_mala <- conformidad_dbi(con, buena)
  con_bateria <- suppressWarnings(medir_dbi(
    buena, con, id_medicion = "bateria", fecha = .mdt_fecha,
    proteger_datos_personales = TRUE, conformidad = bateria_mala
  ))
  expect_gt(nrow(normal), 0L)
  # Los acentos que la bateria inventa disparan la guarda de SQLite: eso es una
  # abstencion con esos valores y no le quita la regla de texto ASCII.
  expect_identical(con_bateria$numerador, normal$numerador)
  expect_identical(con_bateria$denominador, normal$denominador)
})
