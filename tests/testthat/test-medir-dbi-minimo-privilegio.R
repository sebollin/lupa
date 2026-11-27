# Una credencial de minimo privilegio -solo `SELECT` sobre las tablas, sin
# `pg_read_all_stats` en PostgreSQL ni `VIEW SERVER STATE`/`VIEW DATABASE
# STATE` en SQL Server- mide lo mismo que el dueno con medir_dbi(),
# plan_medicion_dbi(), perfilar_dbi(), conformidad_dbi() y
# relaciones_coleccion(). Una pieza opcional que el motor le puede negar -el
# contador de modificaciones del motor- nunca hace fallar la medicion: se lee
# aparte y, negada, solo baja el control de lecturas, con el motivo. MySQL y
# MariaDB, en `test-local-medir-dbi-minimo-privilegio-mysql.R`. Las
# credenciales nuevas se crean y se borran en cada prueba; se saltean sin el
# motor o sin permiso para crearlas.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.minpg_conectar <- function(puerto, usuario = Sys.getenv("LUPA_PG_USER", "postgres"),
                            clave = Sys.getenv("LUPA_PG_PASSWORD", "lupa")) {
  skip_if_not_installed("RPostgres")
  tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = usuario, password = clave,
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
}

.minpg_probar <- function(puerto) {
  duenio <- .minpg_conectar(puerto)
  skip_if(is.null(duenio), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  on.exit(DBI::dbDisconnect(duenio), add = TRUE)
  rol <- "lupa_mdi_minimo"
  clave <- .minp_clave()
  limpiar <- function() {
    .minp_borrar_tablas(duenio)
    try(DBI::dbExecute(duenio, paste0("DROP ROLE IF EXISTS ", rol)), silent = TRUE)
  }
  limpiar()
  creado <- tryCatch({
    DBI::dbExecute(duenio, paste0("CREATE ROLE ", rol, " LOGIN PASSWORD '", clave, "'"))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear un rol.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  .minp_crear_tablas(duenio)
  for (t in .minp_tablas) DBI::dbExecute(duenio, paste0("GRANT SELECT ON ", t, " TO ", rol))
  minimo <- .minpg_conectar(puerto, rol, clave)
  skip_if(is.null(minimo), "El rol nuevo no puede conectarse.")
  on.exit(DBI::dbDisconnect(minimo), add = TRUE, after = FALSE)
  .minp_comparar(duenio, minimo, paste("postgresql", puerto))
}

test_that("PostgreSQL: una credencial de minimo privilegio mide lo mismo que el dueno", {
  for (puerto in c(Sys.getenv("LUPA_PG_PORT", "55432"),
                   Sys.getenv("LUPA_PG93_PORT", "55493"))) {
    .minpg_probar(puerto)
  }
})

test_that("SQL Server: una credencial de minimo privilegio mide lo mismo que el dueno", {
  skip_if_not_installed("odbc")
  clave_duenio <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave_duenio), "Sin clave de SQL Server.")
  conectar <- function(usuario, clave) {
    tryCatch(DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS",
      Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
      Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
      UID = usuario, PWD = clave,
      Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
      TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
    ), error = function(e) NULL)
  }
  duenio <- conectar(Sys.getenv("LUPA_MSSQL_USER", "sa"), clave_duenio)
  skip_if(is.null(duenio), "SQL Server no disponible.")
  on.exit(DBI::dbDisconnect(duenio), add = TRUE)
  usuario <- "lupa_mdi_minimo"
  clave <- .minp_clave()
  limpiar <- function() {
    .minp_borrar_tablas(duenio)
    try(DBI::dbExecute(duenio, paste0(
      "IF DATABASE_PRINCIPAL_ID(N'", usuario, "') IS NOT NULL DROP USER ", usuario
    )), silent = TRUE)
    try(DBI::dbExecute(duenio, paste0(
      "IF SUSER_ID(N'", usuario, "') IS NOT NULL DROP LOGIN ", usuario
    )), silent = TRUE)
  }
  limpiar()
  creado <- tryCatch({
    DBI::dbExecute(duenio, paste0("CREATE LOGIN ", usuario, " WITH PASSWORD = N'", clave,
                                  "', CHECK_POLICY = OFF"))
    DBI::dbExecute(duenio, paste0("CREATE USER ", usuario, " FOR LOGIN ", usuario))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear un usuario.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  .minp_crear_tablas(duenio)
  for (t in .minp_tablas) DBI::dbExecute(duenio, paste0("GRANT SELECT ON dbo.", t, " TO ", usuario))
  minimo <- conectar(usuario, clave)
  skip_if(is.null(minimo), "El usuario nuevo no puede conectarse.")
  on.exit(DBI::dbDisconnect(minimo), add = TRUE, after = FALSE)
  m <- .minp_comparar(duenio, minimo, "sqlserver")
  # Sin `VIEW DATABASE STATE` el contador del motor no se lee, y se dice por que;
  # el dueno lo lee.
  cm_m <- attr(m$minimo, "ejecucion_dbi", exact = TRUE)$contador_motor
  cm_d <- attr(m$duenio, "ejecucion_dbi", exact = TRUE)$contador_motor
  expect_true(is.data.frame(cm_m) && nrow(cm_m) == 2L)
  expect_false(any(cm_m$leido))
  expect_true(all(nzchar(cm_m$motivo)))
  expect_true(all(cm_d$leido))
  expect_true(all(attr(m$minimo, "lecturas", exact = TRUE)$control ==
                    "solo_filas_agregadas_o_quitadas"))
})
