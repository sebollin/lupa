test_that("sin n_distintos, una moda de frecuencia 1 sobre varios valores se calla", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE amsr_moda"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, "DROP TABLE amsr_moda"), silent = TRUE)
  DBI::dbExecute(con, "CREATE TABLE amsr_moda (x int, y int, z int)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO amsr_moda VALUES (5,1,8),(3,1,NULL),(9,2,NULL),(1,3,NULL),(7,4,NULL)"
  ))
  for (metricas in list(c("validos", "moda"), c("validos", "distintos", "moda"))) {
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "amsr_moda", universo = "tabla_completa",
      bloque_muestra = "solo_agregados", metricas = metricas,
      proteger_datos_personales = FALSE
    )))
    columnas <- resultado$resumen_tabla$columnas
    # Cinco valores distintos: la frecuencia 1 dice que ninguno se repite.
    expect_true(is.na(columnas$moda[columnas$columna == "x"]))
    expect_equal(as.numeric(columnas$frecuencia_moda[columnas$columna == "x"]), 1)
    # Un valor que se repite sigue siendo la moda.
    expect_identical(as.character(columnas$moda[columnas$columna == "y"]), "1")
    # Un unico valor valido es su propia moda, aunque aparezca una vez.
    expect_identical(as.character(columnas$moda[columnas$columna == "z"]), "8")
  }
})
