testthat::skip_on_cran()

.apr_conexion_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
      port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
      password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
    ), error = function(e) NULL
  )
  skip_if(is.null(conexion), paste0("PostgreSQL no disponible en ", puerto, "."))
  conexion
}

.apr_conexion_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  conexion <- tryCatch(
    DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
      Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ), error = function(e) NULL
  )
  skip_if(is.null(conexion), "SQL Server no disponible.")
  conexion
}

.apr_crear_tabla_comun <- function(conexion, motor, tabla) {
  try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  if (motor == "postgres") {
    DBI::dbExecute(conexion, paste(
      "CREATE TABLE", tabla, "(id integer, n1 double precision,",
      "n2 numeric(18,3), txt text, txt_corto varchar(100),",
      "fecha date, bin bytea)"
    ))
    DBI::dbExecute(conexion, paste(
      "INSERT INTO", tabla,
      "SELECT g, g * 0.25, g::numeric / 3, 'texto-' || g,",
      "repeat('x', 50), DATE '2020-01-01' + g,",
      "decode(repeat('ab', 20), 'hex')",
      "FROM generate_series(1, 3000) AS g"
    ))
    DBI::dbExecute(conexion, paste("ANALYZE", tabla))
  } else {
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla,
      " (id int, n1 float, n2 decimal(18,3), txt varchar(max),",
      "txt_corto varchar(100), fecha date, bin varbinary(max))"
    ))
    DBI::dbExecute(conexion, paste0(
      ";WITH n AS (SELECT TOP (3000) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS id ",
      "FROM sys.all_objects AS a CROSS JOIN sys.all_objects AS b) ",
      "INSERT INTO ", tabla,
      " SELECT CONVERT(int, id), CONVERT(float, id) * 0.25,",
      " CONVERT(decimal(18,3), id) / 3,",
      " CONCAT('texto-', id), REPLICATE(CAST('x' AS varchar(max)), 50),",
      " DATEADD(day, id, CONVERT(date, '2020-01-01')),",
      " CONVERT(varbinary(max), REPLICATE(CAST('ab' AS varchar(max)), 20))",
      " FROM n"
    ))
  }
  invisible(tabla)
}

.apr_crear_tabla_bytes <- function(conexion, motor, tabla) {
  try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  if (motor == "postgres") {
    DBI::dbExecute(conexion, paste(
      "CREATE TABLE", tabla, "(id integer, texto text, bin bytea)"
    ))
    DBI::dbExecute(conexion, paste(
      "INSERT INTO", tabla,
      "SELECT g, repeat('x', 3000), decode(repeat('ab', 300), 'hex')",
      "FROM generate_series(1, 200) AS g"
    ))
    DBI::dbExecute(conexion, paste("ANALYZE", tabla))
  } else {
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla,
      " (id int, texto varchar(max), bin varbinary(max))"
    ))
    DBI::dbExecute(conexion, paste0(
      ";WITH n AS (SELECT TOP (200) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS id ",
      "FROM sys.all_objects AS a CROSS JOIN sys.all_objects AS b) ",
      "INSERT INTO ", tabla,
      " SELECT CONVERT(int, id), REPLICATE(CAST('x' AS varchar(max)), 3000),",
      " CONVERT(varbinary(max), REPLICATE(CAST('ab' AS varchar(max)), 300))",
      " FROM n"
    ))
  }
  invisible(tabla)
}

.apr_capturar <- function(conexion, tabla, max_consultas) {
  aviso <- list()
  resultado <- tryCatch(withCallingHandlers(
    suppressMessages(perfilar_dbi(
      conexion, tabla, universo = "muestra_motor", muestra_motor = 500L,
      muestra = 500L, orden_muestra = "id", max_consultas = max_consultas,
      proteger_datos_personales = FALSE
    )), warning = function(w) {
      aviso[[length(aviso) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  ), error = function(e) e)
  list(resultado = resultado, avisos = aviso)
}

.apr_comparar_publicado <- function(chico, referencia) {
  columnas <- chico$resumen_tabla$columnas
  ref <- referencia$resumen_tabla$columnas
  expect_equal(as.character(columnas$columna), as.character(ref$columna))
  for (nombre in intersect(names(columnas), names(ref))) {
    for (i in seq_len(nrow(columnas))) {
      actual <- columnas[[nombre]][[i]]
      esperado <- ref[[nombre]][[i]]
      if (!is.na(actual) && !is.na(esperado)) {
        expect_true(isTRUE(all.equal(actual, esperado)),
                    info = paste(nombre, columnas$columna[[i]]))
      }
    }
  }
}

.apr_probar_presupuesto <- function(conexion, tabla, casos) {
  referencia <- suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, tabla, universo = "muestra_motor", muestra_motor = 500L,
    muestra = 500L, orden_muestra = "id", max_consultas = Inf,
    proteger_datos_personales = FALSE
  )))
  for (k in casos) {
    capturado <- .apr_capturar(conexion, tabla, k)
    resultado <- capturado$resultado
    expect_false(inherits(resultado, "condition"), info = paste("k", k))
    expect_true(is.list(resultado$resumen_tabla), info = paste("k", k))
    .apr_comparar_publicado(resultado, referencia)
    consultas <- resultado$resumen_tabla$meta$consultas
    expect_true(isTRUE(consultas$agotado), info = paste("k", k))
    expect_true(any(vapply(
      capturado$avisos, inherits, logical(1L), "lupa_aviso_muestra_motor"
    )), info = paste("k", k))
    sql <- resultado$resumen_tabla$sql
    expect_true(any(sql$estado == "no_disponible"), info = paste("k", k))
    cobertura <- resultado$resumen_tabla$cobertura
    expect_true(any(
      cobertura$estado %in% c("degradado", "presupuesto_agotado") |
        grepl("presupuesto declarado", cobertura$motivo, fixed = TRUE),
      na.rm = TRUE
    ), info = paste("k", k))
    muestreo <- resultado$resumen_tabla$meta$muestreo
    expect_true(is.list(muestreo), info = paste("k", k))
    if (!isTRUE(muestreo$disponible)) expect_null(resultado$perfil_muestra)
  }
}

test_that("max_consultas degrada muestra_motor en PostgreSQL", {
  skip_on_cran()
  conexion <- .apr_conexion_pg(55432L)
  tabla <- "apr_presupuesto_pg16"
  on.exit({
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .apr_crear_tabla_comun(conexion, "postgres", tabla)
  .apr_probar_presupuesto(conexion, tabla, c(1L, 6L, 5L))
})

test_that("max_consultas degrada muestra_motor en SQL Server", {
  skip_on_cran()
  conexion <- .apr_conexion_mssql()
  tabla <- "apr_presupuesto_mssql"
  on.exit({
    try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .apr_crear_tabla_comun(conexion, "mssql", tabla)
  .apr_probar_presupuesto(conexion, tabla, c(1L, 7L, 6L))
})

test_that("muestra_motor mide el tope de bytes en PostgreSQL y SQL Server", {
  skip_on_cran()
  conexiones <- list(
    postgres = .apr_conexion_pg(55432L),
    mssql = .apr_conexion_mssql()
  )
  on.exit(for (conexion in conexiones) DBI::dbDisconnect(conexion), add = TRUE)
  for (motor in names(conexiones)) {
    conexion <- conexiones[[motor]]
    tabla <- paste0("apr_bytes_", motor)
    on.exit(
      try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE),
      add = TRUE
    )
    .apr_crear_tabla_bytes(conexion, motor, tabla)
    for (tope in c(100000L, 20000L)) {
      avisos <- list()
      resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
        conexion, tabla, universo = "muestra_motor", muestra_motor = 100L,
        muestra = 100L, orden_muestra = "id", metricas = "validos",
        max_bytes_muestra = tope, max_bytes_materializacion = tope,
        proteger_datos_personales = FALSE
      )), warning = function(w) {
        avisos[[length(avisos) + 1L]] <<- w
        invokeRestart("muffleWarning")
      })
      muestreo <- resultado$resumen_tabla$meta$muestreo
      materializacion <- resultado$resumen_tabla$meta$materializacion
      expect_true(any(vapply(
        avisos, inherits, logical(1L), "lupa_aviso_muestra_motor"
      )), info = paste(motor, tope))
      expect_true(is.finite(muestreo$filas_obtenidas), info = paste(motor, tope))
      expect_gte(muestreo$filas_obtenidas, if (tope == 100000L) 10 else 3,
                 label = paste(motor, tope))
      expect_lte(muestreo$filas_obtenidas, 100,
                 label = paste(motor, tope))
      expect_true(isTRUE(muestreo$filas_por_bytes_fuente ==
                         "medicion_materializacion"), info = paste(motor, tope))
      expect_equal(muestreo$filas_por_bytes, muestreo$filas_obtenidas,
                   info = paste(motor, tope))
      expect_lte(materializacion$bytes, tope, label = paste(motor, tope))
      expect_true(is.finite(materializacion$bytes), info = paste(motor, tope))
      expect_false(is.null(resultado$perfil_muestra), info = paste(motor, tope))
      expect_lte(resultado$perfil_muestra$meta$bytes_muestra, tope,
                 label = paste(motor, tope))
    }
  }
})

