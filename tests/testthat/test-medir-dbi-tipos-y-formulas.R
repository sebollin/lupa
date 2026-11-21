test_that("medir_dbi reproduce la lectura doble de DECIMAL y FLOAT", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")

  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  instancia <- function(metrica, nombre, atributo, ...) {
    instanciar(
      especializar(metrica, nombre, ...), "t", atributo,
      nombre_instancia = nombre
    )
  }
  instancias <- list(
    instancia(nuc$ValoresPosiblesPorExtension, "VE_dec", "dec",
              valores = c(0.3, 0.1, 0.5)),
    instancia(nuc$ValoresPosiblesPorComprension, "RC_dec", "dec",
              minimo = 0.1, maximo = 0.3),
    instancia(nuc$NoNulo, "NN_dec", "dec", valores_nulos = 0.3),
    instancia(nuc$ReglaIntegridadIntraEntidad, "F_dec", "dec",
              regla = ~ dec == 0.3),
    instancia(nuc$ValoresPosiblesPorExtension, "VE_fl", "fl",
              valores = c(0.3, 0.1, 0.2)),
    instancia(nuc$ValoresPosiblesPorComprension, "RC_fl", "fl",
              minimo = 0.1, maximo = 0.3)
  )
  mod <- modelo(instancias)
  sql_filas <- paste0(
    "INSERT INTO t VALUES ",
    paste(sprintf("(%.1f, %.1f)", (1:60) %% 7 / 10, (1:60) %% 5 / 10),
          collapse = ", ")
  )
  for (motor in c("sqlite", "duckdb")) {
    con <- if (motor == "sqlite") {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    } else {
      DBI::dbConnect(
        .duckdb_prueba(dbdir = ":memory:")
      )
    }
    cerrar <- local({
      cc <- con
      function() suppressWarnings(
        try(DBI::dbDisconnect(cc, shutdown = TRUE), silent = TRUE)
      )
    })
    on.exit(cerrar(), add = TRUE)
    tipos <- if (motor == "sqlite") {
      "CREATE TABLE t (dec NUMERIC, fl REAL)"
    } else {
      "CREATE TABLE t (dec DECIMAL(10,2), fl FLOAT)"
    }
    DBI::dbExecute(con, tipos)
    DBI::dbExecute(con, sql_filas)
    leido <- list(t = DBI::dbReadTable(con, "t"))
    esperado <- suppressWarnings(medir(
      mod, leido, id_medicion = "x", fecha = fecha, detalle = "resumen"
    ))
    obtenido <- suppressWarnings(medir_dbi(
      mod, con, id_medicion = "x", fecha = fecha
    ))
    campos <- c("metrica_instanciada", "numerador", "denominador", "resultado")
    for (campo in campos) {
      expect_identical(esperado[[campo]], obtenido[[campo]], info = motor)
    }
  }
})

test_that("las formulas DBI no consultan ni fuerzan simbolos externos", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(a = c(1, 2, NA, 4)))
  nuc <- metricas_nucleo()
  marca <- new.env(parent = globalenv())
  marca$n <- 0L
  marca$boom <- function() {
    marca$n <- marca$n + 1L
    TRUE
  }
  delayedAssign("v", boom(), eval.env = marca, assign.env = marca)
  regla <- stats::as.formula("~ a > v", env = marca)
  inst <- instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "R", regla = regla),
    "d", "a", nombre_instancia = "R"
  )
  mod <- modelo(inst)
  p <- suppressWarnings(plan_medicion_dbi(mod, con))
  md <- suppressWarnings(medir_dbi(mod, con))
  mm <- suppressWarnings(medir(mod, list(d = DBI::dbReadTable(con, "d"))))
  expect_equal(marca$n, 0L)
  expect_identical(p$via, "no_medible_metodo")
  expect_identical(attr(md, "cobertura_metricas")$estado, "no_medible")
  expect_identical(attr(mm, "cobertura_metricas")$estado, "no_medible")
  expect_match(
    attr(md, "cobertura_metricas")$motivo,
    "s\u00edmbolo", fixed = TRUE
  )
  expect_match(
    attr(md, "cobertura_metricas")$motivo,
    "f\u00f3rmula", fixed = TRUE
  )

  marca$n <- 0L
  marca$v <- structure(1, class = "evil")
  marca$`Ops.evil` <- function(e1, e2) {
    marca$n <- marca$n + 1L
    TRUE
  }
  regla_s3 <- stats::as.formula("~ a > v", env = marca)
  inst_s3 <- instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "S", regla = regla_s3),
    "d", "a", nombre_instancia = "S"
  )
  md_s3 <- suppressWarnings(medir_dbi(modelo(inst_s3), con))
  expect_equal(marca$n, 0L)
  expect_identical(attr(md_s3, "cobertura_metricas")$estado, "no_medible")
})

test_that("el resumen resuelve `aplicable` una sola vez", {
  skip_on_cran()
  marca <- new.env(parent = globalenv())
  marca$n <- 0L
  marca$boom <- function() {
    marca$n <- marca$n + 1L
    TRUE
  }
  regla <- stats::as.formula("~ a > 1 & boom()", env = marca)
  nuc <- metricas_nucleo()
  inst <- instanciar(
    especializar(nuc$NoNulo, "N", aplicable = regla), "d", "a",
    nombre_instancia = "N"
  )
  datos <- list(d = data.frame(a = c(1, 2, NA, 4)))
  completo <- suppressWarnings(medir(modelo(inst), datos))
  expect_equal(marca$n, 2L)
  marca$n <- 0L
  resumen <- suppressWarnings(medir(modelo(inst), datos, detalle = "resumen"))
  expect_equal(marca$n, 1L)
  expect_equal(resumen$denominador, nrow(completo), tolerance = 0)
})

test_that("lim no se resuelve desde el entorno de una formula", {
  skip_on_cran()
  marca <- new.env(parent = globalenv())
  marca$n <- 0L
  delayedAssign(
    "lim", {
      marca$n <- marca$n + 1L
      2
    }, eval.env = marca, assign.env = marca
  )
  regla <- stats::as.formula("~ a > lim", env = marca)
  nuc <- metricas_nucleo()
  inst <- instanciar(
    especializar(nuc$ReglaIntegridadIntraEntidad, "R", regla = regla),
    "d", "a", nombre_instancia = "R"
  )
  md <- suppressWarnings(medir(
    modelo(inst), list(d = data.frame(a = c(1, 2, 3))), detalle = "resumen"
  ))
  cob <- attr(md, "cobertura_metricas")
  expect_identical(marca$n, 0L)
  expect_identical(cob$estado, "no_medible")
  expect_match(cob$motivo, "s\u00edmbolo", fixed = TRUE)
})

test_that("DuckDB compara como doble BIGINT, UINTEGER y DECIMAL entero", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(
    .duckdb_prueba(dbdir = ":memory:")
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE d (big BIGINT, dec DECIMAL(18,0), ui UINTEGER)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO d VALUES ",
    paste(sprintf("(%d, %d, %d)", 1:60, 1:60, 1:60), collapse = ", ")
  ))
  leido <- list(d = DBI::dbReadTable(con, "d"))
  nuc <- metricas_nucleo()
  instancias <- lapply(c("big", "dec", "ui"), function(columna) {
    instanciar(
      especializar(nuc$NoNulo, paste0("NN_", columna)),
      "d", columna, nombre_instancia = paste0("NN_", columna)
    )
  })
  mod <- modelo(instancias)
  esperado <- suppressWarnings(medir(
    mod, leido, id_medicion = "x", detalle = "resumen"
  ))
  obtenido <- suppressWarnings(medir_dbi(mod, con, id_medicion = "x"))
  campos <- c("metrica_instanciada", "numerador", "denominador", "resultado")
  expect_identical(
    as.data.frame(unclass(esperado[campos]), stringsAsFactors = FALSE),
    as.data.frame(unclass(obtenido[campos]), stringsAsFactors = FALSE)
  )

  DBI::dbExecute(con, "CREATE TABLE h (big BIGINT, dec DECIMAL(18,0))")
  DBI::dbExecute(con, paste0(
    "INSERT INTO h VALUES ",
    paste(c(
      "(9007199254740992, 9007199254740992)",
      "(9007199254740993, 9007199254740993)",
      "(9007199254740994, 9007199254740994)"
    ), collapse = ", ")
  ))
  leido_h <- list(h = DBI::dbReadTable(con, "h"))
  instancias_h <- lapply(c("big", "dec"), function(columna) {
    instanciar(
      especializar(
        nuc$ValoresPosiblesPorExtension,
        paste0("VE_", columna), valores = 9007199254740992
      ),
      "h", columna, nombre_instancia = paste0("VE_", columna)
    )
  })
  mod_h <- modelo(instancias_h)
  esperado_h <- suppressWarnings(medir(
    mod_h, leido_h, id_medicion = "h", detalle = "resumen"
  ))
  obtenido_h <- suppressWarnings(medir_dbi(mod_h, con, tablas = c(h = "h"),
                                           id_medicion = "h"))
  expect_identical(
    as.data.frame(unclass(esperado_h[campos]), stringsAsFactors = FALSE),
    as.data.frame(unclass(obtenido_h[campos]), stringsAsFactors = FALSE)
  )
})

test_that("BIGINT leido como integer64 queda no_medible con su tipo", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(
    .duckdb_prueba(bigint = "integer64", dbdir = ":memory:")
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (big BIGINT)")
  DBI::dbExecute(con, "INSERT INTO d VALUES (1), (2), (3)")
  nuc <- metricas_nucleo()
  inst <- instanciar(
    especializar(nuc$NoNulo, "NN"), "d", "big", nombre_instancia = "NN"
  )
  md <- suppressWarnings(medir_dbi(modelo(inst), con))
  cob <- attr(md, "cobertura_metricas")
  expect_identical(nrow(md), 0L)
  expect_identical(cob$estado, "no_medible")
  expect_match(cob$motivo, "entero de 64 bits", fixed = TRUE)
  expect_match(cob$motivo, "BIGINT", fixed = TRUE)
})

test_that("tablas rechaza una entidad desconocida", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "otra", data.frame(x = 1:2))
  nuc <- metricas_nucleo()
  inst <- instanciar(especializar(nuc$NoNulo, "N"), "d", "x")
  expect_error(
    medir_dbi(modelo(inst), con, tablas = c(zzz = "otra")),
    regexp = "zzz"
  )
})

test_that("tabla fisica inexistente tiene un motivo propio y su nombre", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nuc <- metricas_nucleo()
  inst <- instanciar(especializar(nuc$NoNulo, "N"), "d", "x")
  md <- suppressWarnings(medir_dbi(
    modelo(inst), con, tablas = c(d = "no_existe")
  ))
  cob <- attr(md, "cobertura_metricas")
  expect_identical(cob$estado, "no_medible")
  expect_match(cob$motivo, "no_existe", fixed = TRUE)
  expect_match(cob$motivo, "esquema", fixed = TRUE)
  expect_false(grepl("no such table", cob$motivo, fixed = TRUE))
})

test_that("las columnas LIST no se miden como escalares", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(
    .duckdb_prueba(dbdir = ":memory:")
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (xs INTEGER[], k INTEGER)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ([1, 2], 1), ([1, 2], 2), (NULL, 3)")
  nuc <- metricas_nucleo()
  inst <- instanciar(especializar(nuc$NoNulo, "N"), "d", "xs")
  md <- suppressWarnings(medir_dbi(modelo(inst), con))
  cob <- attr(md, "cobertura_metricas")
  expect_identical(nrow(md), 0L)
  expect_identical(cob$estado, "no_medible")
  expect_match(cob$motivo, "tipo escalar", fixed = TRUE)
})

test_that("max_consultas cuenta las consultas de esquema", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(x = 1:3))
  nuc <- metricas_nucleo()
  inst <- instanciar(especializar(nuc$NoNulo, "N"), "d", "x")
  md <- suppressWarnings(medir_dbi(modelo(inst), con, max_consultas = 1))
  ejecucion <- attr(md, "ejecucion_dbi")
  expect_identical(ejecucion$consultas, 1L)
  expect_true(any(attr(md, "cobertura_metricas")$estado == "no_medible"))
})
