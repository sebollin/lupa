# Paso 7: politica motor por regla para PostgreSQL, MySQL, MariaDB y SQL Server.

test_that("la politica de motores declara los 6 motores para todas las reglas", {
  skip_on_cran()
  pol <- lupa:::.POLITICA_MOTORES_MEDICION
  expect_true(is.data.frame(pol))
  expect_identical(
    sort(unique(pol$motor)),
    sort(c("sqlite", "duckdb", "postgresql", "mysql", "mariadb", "sqlserver"))
  )
  n_reglas <- length(unique(pol$regla))
  expect_gte(n_reglas, 15L)
  expect_identical(nrow(pol), n_reglas * 6L)
})

test_that("PostgreSQL, MySQL, MariaDB y SQL Server quedan no_medible con su motivo declarado", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(
    n = c(1, 2, NA), t = c("a", "b", "a"), stringsAsFactors = FALSE
  ))
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "Dom", valores = c("a", "b")), "d", "t"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "n")
  )
  for (motor in c("postgresql", "mysql", "mariadb", "sqlserver")) {
    p <- plan_medicion_dbi(mod, con, dialecto = motor)
    expect_identical(attr(p, "motor", exact = TRUE), motor)
    expect_true(all(p$via == "no_medible"), info = motor)
    expect_true(all(grepl(motor, p$motivo, fixed = TRUE)), info = motor)

    r <- suppressWarnings(medir_dbi(mod, con, id_medicion = "m7", fecha = fecha,
                                    dialecto = motor,
                                    proteger_datos_personales = TRUE))
    expect_identical(nrow(r), 0L, info = motor)
    cob <- attr(r, "cobertura_metricas", exact = TRUE)
    expect_identical(nrow(cob), 3L, info = motor)
    expect_true(all(cob$estado == "no_medible"), info = motor)
    expect_true(all(grepl(motor, cob$motivo, fixed = TRUE)), info = motor)
  }
})
