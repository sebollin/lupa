# Paso 6: lectura por bloques (`estrategia_no_sql = "bloques"`) con proyeccion.

.conectar_bloques_prueba <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

test_that("estrategia_no_sql = 'bloques' mide metricas separables en varios bloques y coincide con memoria", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  # 12 filas -> con bloque_filas = 3 son 4 bloques; bloque 1 tiene NA en cod
  d <- data.frame(
    n = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    m = c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    t = c("a", "b", "c", "d", "e", NA, "b", "c", "d", "z", "a", "c"),
    cod = c(NA, NA, NA, "AB1", "CD2", "bad", "EF3", NA, "GH4", "xx", "IJ5", "KL6"),
    f = as.Date(c("2026-01-01", "2026-01-10", NA, "2026-01-20",
                  "2026-01-05", "2026-01-15", "2026-01-25", "2026-02-01",
                  "2026-02-10", "2026-02-15", "2026-02-20", NA)),
    relleno = paste0("extra_", seq_len(12L)),
    stringsAsFactors = FALSE
  )
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^[A-Z]{2}[0-9]$"), "d", "cod"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "RT", minimo = "b", maximo = "d"), "d", "t"),
    instanciar(especializar(nuc$GradoOportunidadAtributoPorFecha, "Gr",
                            fecha_solicitud = as.Date("2026-01-01"),
                            fecha_fin_utilidad = as.Date("2026-03-01")), "d", "f"),
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "Arit",
                            regla = ~ n + m >= 10), "d", c("n", "m")),
    instanciar(especializar(nuc$ErrorEstandar, "SE"), "d", "n"),
    instanciar(especializar(nuc$EntidadDuplicada, "ED"), "d")
  )

  for (motor in c("sqlite", "duckdb")) {
    con <- .conectar_bloques_prueba(motor)
    DBI::dbWriteTable(con, "d", d)
    leida <- DBI::dbReadTable(con, "d")
    mem <- suppressWarnings(medir(mod, list(d = leida), id_medicion = "bl",
                                  fecha = fecha, proteger_datos_personales = TRUE,
                                  detalle = "resumen"))
    bl <- suppressWarnings(medir_dbi(mod, con, id_medicion = "bl", fecha = fecha,
                                     estrategia_no_sql = "bloques",
                                     bloque_filas = 3L,
                                     proteger_datos_personales = TRUE))

    # Las 5 metricas (1 SQL + 4 bloques) coinciden con memoria
    esperadas <- c("NN@d.n", "Rx@d.cod", "RT@d.t", "Gr@d.f", "Arit@d.n.m")
    expect_identical(bl$metrica_instanciada, esperadas, info = motor)
    mem_sub <- mem[match(esperadas, mem$metrica_instanciada), , drop = FALSE]
    rownames(mem_sub) <- NULL
    expect_equal(bl$resultado, mem_sub$resultado, tolerance = 1e-12, info = motor)
    expect_identical(bl$numerador, mem_sub$numerador, info = motor)
    expect_identical(bl$denominador, mem_sub$denominador, info = motor)
    expect_identical(bl$n_universo, mem_sub$n_universo, info = motor)
    expect_identical(bl$n_no_evaluados, mem_sub$n_no_evaluados, info = motor)
    expect_identical(bl$motivo_no_evaluados, mem_sub$motivo_no_evaluados, info = motor)

    # ErrorEstandar y EntidadDuplicada (no separables) siguen no_medible
    cob <- attr(bl, "cobertura_metricas", exact = TRUE)
    expect_true(all(c("SE@d.n", "ED@d") %in% cob$metrica_instanciada[cob$estado == "no_medible"]),
                info = motor)

    # Ninguna consulta de bloques usa SELECT * ni proyecta `relleno`
    sqls <- attr(bl, "ejecucion_dbi", exact = TRUE)$sql
    expect_false(any(grepl("SELECT \\*", sqls)), info = motor)
    expect_false(any(grepl("relleno", sqls, fixed = TRUE)), info = motor)

    DBI::dbDisconnect(con)
  }
})

test_that("SQLite guarda UTF-8 con estrategia_no_sql = 'bloques' deriva dominio a bloques", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  e_agudo <- intToUtf8(0x00e9L)
  DBI::dbWriteTable(con, "d", data.frame(
    t = c("a", e_agudo, "b", NA, e_agudo),
    extra = 1:5,
    stringsAsFactors = FALSE
  ))
  mod <- modelo(instanciar(
    especializar(nuc$ValoresPosiblesPorExtension, "Dom", valores = c("a", e_agudo)),
    "d", "t"
  ))
  # Sin bloques: la guarda UTF-8 declina
  sin_b <- suppressWarnings(medir_dbi(mod, con, id_medicion = "g", fecha = fecha,
                                      proteger_datos_personales = TRUE))
  expect_identical(nrow(sin_b), 0L)
  expect_identical(attr(sin_b, "cobertura_metricas")$estado, "no_medible")

  # Con bloques: se deriva a bloques_r y coincide exactamente con memoria
  con_b <- medir_dbi(mod, con, id_medicion = "g", fecha = fecha,
                     estrategia_no_sql = "bloques", bloque_filas = 2L,
                     proteger_datos_personales = TRUE)
  mem <- medir(mod, list(d = DBI::dbReadTable(con, "d")), id_medicion = "g",
               fecha = fecha, proteger_datos_personales = TRUE,
               detalle = "resumen")
  expect_identical(as.data.frame(unclass(con_b), stringsAsFactors = FALSE),
                   as.data.frame(unclass(mem), stringsAsFactors = FALSE))
  expect_identical(unname(attr(con_b, "ejecucion_dbi")$vias), "bloques_r")
})
