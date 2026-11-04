# Pruebas contra MySQL, MariaDB y SQL Server por JDBC en motores locales.
#
# Usan controladores que el paquete no declara (RMySQL, RMariaDB, RJDBC) y
# motores que solo existen en la maquina de desarrollo, asi que no viajan en el
# paquete: `.Rbuildignore` excluye `tests/testthat/test-local-*.R`. La suite de
# desarrollo (`devtools::test()`) las corre igual, desde el arbol fuente.

test_that("RJDBC publica el producto del motor cuando esta disponible", {
  skip_on_cran()
  skip_if_not_installed("RJDBC")
  skip_if_not_installed("rJava")
  variables <- c("LUPA_JDBC_DRIVER", "LUPA_JDBC_CLASSPATH", "LUPA_JDBC_URL")
  testthat::skip_if(
    any(!nzchar(Sys.getenv(variables))),
    "se necesitan las variables JDBC de la prueba real"
  )
  driver <- RJDBC::JDBC(
    Sys.getenv("LUPA_JDBC_DRIVER"), Sys.getenv("LUPA_JDBC_CLASSPATH")
  )
  con <- DBI::dbConnect(
    driver, Sys.getenv("LUPA_JDBC_URL"),
    Sys.getenv("LUPA_JDBC_USER", unset = ""),
    Sys.getenv("LUPA_JDBC_PASSWORD", unset = "")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_match(.senas_conexion_dbi(con), "sql server")
})

.arrm_conexion_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  conexion <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_DB_PORT", if (maria) "33306" else "33307")),
        dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
        username = Sys.getenv("LUPA_DB_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_DB_PORT", if (maria) "33306" else "33307")),
        dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
        username = Sys.getenv("LUPA_DB_USER", "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(conexion), paste0(paquete, " no disponible."))
  conexion
}

test_that("una tabla normal no emite avisos en MySQL", {
  skip_on_cran()
  conexion <- .arrm_conexion_mysql()
  tabla <- "arrm_aviso_mysql"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en MariaDB", {
  skip_on_cran()
  conexion <- .arrm_conexion_mysql(maria = TRUE)
  tabla <- "arrm_aviso_maria"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

.arrm_con_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD")
  if (!nzchar(clave)) skip("falta la clave del motor")
  tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) skip("motor local no disponible"))
}

.arrm_prueba_tipos_mysql <- function(maria = FALSE) {
  con <- .arrm_con_mysql(maria)
  nombre <- if (maria) "arrm_maria_tipos" else "arrm_mysql_tipos"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", nombre)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", nombre, " (id INT, txt TEXT, txt_long LONGTEXT, ",
    "bin_fixed BINARY(4), bin_var VARBINARY(16), bin_blob BLOB, ",
    "bin_long LONGBLOB)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", nombre,
    " VALUES (1, 'texto corto', REPEAT('l', 100), UNHEX('DEADBEEF'),",
    " UNHEX('DEADBEEF00'), UNHEX('DEADBEEF00'), UNHEX('DEADBEEF00')),",
    " (2, 'otro texto', REPEAT('m', 100), UNHEX('01020304'),",
    " UNHEX('0102030405'), UNHEX('0102030405'), UNHEX('0102030405')),",
    " (3, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, nombre, universo = "muestra_motor", muestra_motor = 3L,
    muestra = 3L, metricas = "validos", proteger_datos_personales = FALSE
  ))
  binarias <- resultado$perfil_muestra$meta$columnas_binarias
  expect_true(all(c("bin_fixed", "bin_var", "bin_blob", "bin_long") %in% binarias))
  expect_false(any(c("txt", "txt_long") %in% binarias))
  columnas <- resultado$perfil_muestra$columnas
  expect_identical(
    columnas$tipo_declarado[columnas$columna == "txt"], "texto"
  )
  expect_true(is.finite(
    columnas$longitud_media[columnas$columna == "bin_var"]
  ))
}

test_that("MySQL usa data_type del catalogo para texto y binarios", {
  testthat::skip_on_cran()
  .arrm_prueba_tipos_mysql(FALSE)
})

test_that("MariaDB usa data_type del catalogo para texto y binarios", {
  testthat::skip_on_cran()
  .arrm_prueba_tipos_mysql(TRUE)
})
