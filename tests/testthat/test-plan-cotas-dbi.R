testthat::skip_on_cran()

.cotas_conectar <- function(motor) {
  if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    drv_args <- list()
    if ("shared_home" %in% names(formals(duckdb::duckdb))) {
      drv_args$shared_home <- FALSE
    }
    drv <- do.call(duckdb::duckdb, drv_args)
    DBI::dbConnect(drv, dbdir = ":memory:")
  }
}

.cotas_formas <- function(motor) {
  infinito <- if (identical(motor, "sqlite")) {
    "9e999"
  } else {
    "CAST('Infinity' AS DOUBLE)"
  }
  list(
    vacia = list(
      ddl = "id INTEGER, x DOUBLE, d DATE, s VARCHAR(20)",
      filas = character()
    ),
    una_numerica_nula = list(
      ddl = "id INTEGER, x DOUBLE, s VARCHAR(20)",
      filas = paste0("(", 1:12, ", NULL, 'v", 1:12 %% 3L, "')")
    ),
    todas_nulas_menos_texto = list(
      ddl = "a DOUBLE, b BIGINT, d DATE, s VARCHAR(20)",
      filas = paste0("(NULL, NULL, NULL, 'v", 1:12 %% 2L, "')")
    ),
    toda_nula = list(
      ddl = "x DOUBLE",
      filas = rep("(NULL)", 12)
    ),
    entero_ancho = list(
      ddl = "c1 BIGINT",
      filas = paste0("(", sprintf("%.0f", 9007199254740992 + 2 * 1:12), ")")
    ),
    fecha = list(
      ddl = "d DATE",
      filas = paste0("('", format(as.Date("2024-01-01") + 1:12), "')")
    ),
    con_infinito = list(
      ddl = "id INTEGER, x DOUBLE",
      filas = paste0("(", 1:12, ", ", c(infinito, 2:12 / 4), ")")
    )
  )
}

.cotas_metricas <- list(
  por_omision = NULL,
  validos = "validos",
  distintos = "distintos",
  moda = "moda",
  basicos = "basicos",
  mediana = "mediana",
  desvio = "desvio",
  validos_basicos_desvio = c("validos", "basicos", "desvio"),
  basicos_mediana = c("basicos", "mediana"),
  basicos_desvio = c("basicos", "desvio"),
  validos_distintos_moda = c("validos", "distintos", "moda")
)

.cotas_medir <- function(con, nombre, forma, metricas = NULL, ...) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_plan_cotas")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE lupa_plan_cotas (", forma$ddl, ")"
  ))
  if (length(forma$filas)) DBI::dbExecute(con, paste0(
    "INSERT INTO lupa_plan_cotas VALUES ", paste(forma$filas, collapse = ", ")
  ))
  argumentos <- list(
    conexion = con, tabla = "lupa_plan_cotas", ...
  )
  if (!is.null(metricas)) argumentos$metricas <- metricas
  plan <- suppressWarnings(suppressMessages(do.call(
    plan_perfilado_dbi, argumentos
  )))
  perfil <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(argumentos, list(fecha = as.POSIXct(
      "2024-01-01", tz = "UTC"
    )))
  )))
  emitidas <- as.numeric(perfil$resumen_tabla$meta$consultas$emitidas)
  list(
    motor = nombre,
    emitidas = emitidas,
    minimo = as.numeric(attr(plan, "total_minimo")),
    maximo = as.numeric(attr(plan, "total_maximo"))
  )
}

.cotas_comprobar <- function(con, motor, formas, configuracion) {
  resultados <- list()
  for (forma_nombre in names(formas)) {
    for (metrica_nombre in names(.cotas_metricas)) {
      resultado <- do.call(.cotas_medir, c(
        list(
          con = con,
          nombre = paste(motor, forma_nombre, metrica_nombre, sep = "/"),
          forma = formas[[forma_nombre]],
          metricas = .cotas_metricas[[metrica_nombre]]
        ),
        configuracion
      ))
      testthat::expect_gte(
        resultado[["emitidas"]], resultado[["minimo"]],
        label = paste(resultado$motor, forma_nombre, metrica_nombre)
      )
      testthat::expect_lte(
        resultado[["emitidas"]], resultado[["maximo"]],
        label = paste(resultado$motor, forma_nombre, metrica_nombre)
      )
      resultados[[length(resultados) + 1L]] <- resultado
    }
  }
  resultados
}

testthat::test_that(
  "las cotas encierran la grilla de metricas en SQLite y DuckDB", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    testthat::skip_if_not_installed("duckdb")
    for (motor in c("sqlite", "duckdb")) {
      con <- .cotas_conectar(motor)
      .cotas_comprobar(con, motor, .cotas_formas(motor), list(
        bloque_muestra = "con_muestra"
      ))
      .cotas_comprobar(con, motor, .cotas_formas(motor), list(
        bloque_muestra = "solo_agregados"
      ))
      try(DBI::dbDisconnect(
        con, shutdown = identical(motor, "duckdb")
      ), silent = TRUE)
    }
  }
)

testthat::test_that(
  "las cotas cubren bloques y muestra del motor", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    testthat::skip_if_not_installed("duckdb")
    formas <- .cotas_formas("sqlite")[c("vacia", "entero_ancho", "con_infinito")]
    for (motor in c("sqlite", "duckdb")) {
      con <- .cotas_conectar(motor)
      .cotas_comprobar(con, motor, formas, list(
        bloque_muestra = "solo_agregados", bloque_filas = 4L
      ))
      .cotas_comprobar(con, motor, formas, list(
        universo = "muestra_motor", muestra_motor = 6L,
        bloque_muestra = "solo_agregados"
      ))
      .cotas_comprobar(con, motor, formas, list(
        universo = "muestra_motor", muestra_motor = 6L,
        bloque_muestra = "con_muestra", bloque_filas = 4L
      ))
      try(DBI::dbDisconnect(
        con, shutdown = identical(motor, "duckdb")
      ), silent = TRUE)
    }
  }
)

testthat::test_that(
  "la holgura por omision no crece en una tabla comun", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "CREATE TABLE lupa_plan_cotas (id INTEGER, x DOUBLE, s VARCHAR(20))")
    DBI::dbExecute(con, paste0(
      "INSERT INTO lupa_plan_cotas VALUES ", paste0(
        "(", 1:12, ", ", 1:12, ", 'v", 1:12 %% 3L, "')",
        collapse = ", "
      )
    ))
    plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
      con, "lupa_plan_cotas"
    )))
    testthat::expect_equal(attr(plan, "total_maximo"), 30)
  }
)
