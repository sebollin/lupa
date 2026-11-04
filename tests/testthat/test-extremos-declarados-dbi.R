.a59_con_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  con
}

.a59_con_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.a59_literal <- function(x) sprintf("%.17g", x)

.a59_perfil <- function(con, tabla, metricas) {
  avisos <- character()
  resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa",
    bloque_muestra = "solo_agregados", metricas = metricas,
    proteger_datos_personales = FALSE
  )), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(resultado = resultado, avisos = avisos)
}

.a59_crear_extremos <- function(con, tabla, tipo, valores) {
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x ", tipo, ")"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", .a59_literal(valores), ")", collapse = ",")
  ))
}

.a59_mediana_exacta <- function(x) {
  x <- sort(as.numeric(x))
  mitad <- length(x) %/% 2L
  if (length(x) %% 2L) x[[mitad + 1L]] else {
    x[[mitad]] / 2 + x[[mitad + 1L]] / 2
  }
}

test_that("SQL Server escala la media extrema sin declararla ausente", {
  skip_on_cran()
  con <- .a59_con_mssql()
  tabla <- "a59_media_sqlserver"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .a59_crear_extremos(con, tabla, "FLOAT", rep(6.02214076e306, 37L))
  corrida <- .a59_perfil(con, tabla, "basicos")
  fila <- corrida$resultado$resumen_tabla$columnas
  fila <- fila[fila$columna == "x", , drop = FALSE]
  expect_equal(as.numeric(fila$media), 6.02214076e306, tolerance = 1e-12)
  expect_length(corrida$avisos, 0L)
  expect_equal(nrow(corrida$resultado$resumen_tabla$meta$consultas$fallas), 0L)
})

test_that("la mediana extrema usa dos mitades en PostgreSQL y SQL Server", {
  skip_on_cran()
  set.seed(2559L)
  par <- sort(runif(200L, 1e307, 1.7e308))
  impar <- sort(c(runif(200L, 1e307, 1.7e308), 1.1e307))
  probar <- function(con, tablas, tipo, valores) {
    # La conexion llega como promesa: forzarla antes de registrar `on.exit`
    # hace que un `skip()` por motor ausente ocurra aca y no dentro del
    # `try()` de la limpieza, donde R avisa "restarting interrupted promise".
    force(con)
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    for (i in seq_along(tablas)) {
      .a59_crear_extremos(con, tablas[[i]], tipo, valores[[i]])
      corrida <- .a59_perfil(
        con, tablas[[i]], c("validos", "basicos", "mediana")
      )
      fila <- corrida$resultado$resumen_tabla$columnas
      fila <- fila[fila$columna == "x", , drop = FALSE]
      expect_equal(as.numeric(fila$mediana), .a59_mediana_exacta(
        valores[[i]]
      ), tolerance = 1e-12)
      expect_length(corrida$avisos, 0L)
      expect_equal(nrow(corrida$resultado$resumen_tabla$meta$consultas$fallas), 0L)
    }
  }
  for (puerto in c(55432L, 55493L)) {
    probar(
      .a59_con_pg(puerto),
      c(
        paste0("a59_mediana_pg_even_", puerto),
        paste0("a59_mediana_pg_odd_", puerto)
      ),
      "DOUBLE PRECISION", list(par, impar)
    )
  }
  probar(
    .a59_con_mssql(),
    c("a59_mediana_mssql_even", "a59_mediana_mssql_odd"),
    "FLOAT", list(par, impar)
  )
})

test_that("la moda binaria de SQL Server conserva la regla del prefijo", {
  skip_on_cran()
  con <- .a59_con_mssql()
  tabla <- "a59_moda_binaria_sqlserver"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INT, bin VARBINARY(MAX))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    "(1,CONVERT(varbinary(max),REPLICATE(CONVERT(varchar(max),'01'),9000),2)),",
    "(2,CONVERT(varbinary(max),REPLICATE(CONVERT(varchar(max),'01'),9000),2)),",
    "(3,CONVERT(varbinary(max),REPLICATE(CONVERT(varchar(max),'02'),9000),2))"
  ))
  corrida <- .a59_perfil(con, tabla, c("validos", "moda"))
  fila <- corrida$resultado$resumen_tabla$columnas
  fila <- fila[fila$columna == "bin", , drop = FALSE]
  expect_match(as.character(fila$moda), "^9000:")
  expect_length(corrida$avisos, 0L)
  expect_equal(nrow(corrida$resultado$resumen_tabla$meta$consultas$fallas), 0L)

  version <- .version_servidor_sqlserver_dbi(con)
  muestra <- function(version_sqlserver) .campos_sql_muestra_dbi(
    con, "bin", '"bin"', list(raw(1L)), list("varbinary"),
    .dialectos_dbi()$top, tipos_motor = "varbinary",
    version_sqlserver = version_sqlserver
  )
  es_prefijo <- function(x) grepl("SUBSTRING", x, fixed = TRUE)
  real_muestra <- muestra(version)
  real_moda <- .expresion_binaria_moda_dbi(
    con, "bin", "varbinary", version_sqlserver = version
  )
  expect_identical(
    es_prefijo(real_muestra$campos_sql[[1L]]), es_prefijo(real_moda)
  )
  for (version_forzada in list(12, NA_real_)) {
    muestra_forzada <- muestra(version_forzada)
    moda_forzada <- .expresion_binaria_moda_dbi(
      con, "bin", "varbinary", version_sqlserver = version_forzada
    )
    expect_true(es_prefijo(muestra_forzada$campos_sql[[1L]]))
    expect_true(es_prefijo(moda_forzada))
    expect_identical(
      "bin" %in% muestra_forzada$columnas_binarias_prefijo, TRUE
    )
  }
})
