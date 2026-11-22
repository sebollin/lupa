# Pruebas locales de compilacion para MySQL y MariaDB.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.compilar_prueba_conectar_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  prefijo <- if (maria) "LUPA_MARIA" else "LUPA_MYSQL"
  clave <- Sys.getenv(
    if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD",
    unset = ""
  )
  skip_if(!nzchar(clave), paste0("Sin clave para ", prefijo, "."))
  con <- tryCatch({
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
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(prefijo, " no disponible."))
  con
}

.compilar_prueba_probar_mysql <- function(maria = FALSE) {
  con <- .compilar_prueba_conectar_mysql(maria)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- if (maria) "lupa_compilar_mariadb" else "lupa_compilar_mysql"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "DROP TABLE IF EXISTS ", tabla
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id INTEGER NOT NULL, x DOUBLE NOT NULL, txt VARCHAR(32))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste0(
      "(", seq_len(20L), ", ", seq_len(20L), ", 'v",
      seq_len(20L) %% 4L, "')", collapse = ", "
    )
  ))
  mod <- modelo(instanciar(
    especializar(metricas_nucleo()$NoNulo, "compilar_NN"), tabla, "x"
  ))

  perfil <- plan_perfilado_dbi(
    con, tabla, universo = "tabla_completa", metricas = "validos",
    bloque_muestra = "solo_agregados", incluir_valores = FALSE,
    compilar = TRUE
  )
  perfil_compilacion <- attr(perfil, "compilacion", exact = TRUE)
  expect_true(any(perfil_compilacion$estado == "compilada"))
  expect_false(any(perfil_compilacion$estado == "no_compilada"))
  expect_false(any(grepl(
    "no se pudo construir todas", perfil_compilacion$motivo,
    fixed = TRUE, useBytes = TRUE
  ), na.rm = TRUE))

  medicion <- plan_medicion_dbi(
    mod, con, tablas = stats::setNames(tabla, tabla), compilar = TRUE
  )
  medicion_compilacion <- attr(medicion, "compilacion", exact = TRUE)
  expect_true(any(medicion_compilacion$estado == "compilada"))
  expect_false(any(medicion_compilacion$estado == "no_compilada"))

  casos <- list(
    funcion = paste0("SELECT lupa_funcion_inexistente(x) FROM ", tabla),
    columna = paste0("SELECT lupa_columna_inexistente FROM ", tabla)
  )
  for (nombre in names(casos)) {
    compilado <- lupa:::.with_compilador_dbi(con, motor, {
      lupa:::.compilar_sql_dbi(con, casos[[nombre]], nombre, motor)
    })
    registro <- compilado$compilacion
    expect_identical(registro$estado[[1L]], "no_compilada")
    expect_true(nzchar(registro$motivo[[1L]]))
  }

  subconsulta <- paste0(
    "SELECT * FROM (SELECT SLEEP(0.2) AS lupa_sueno) AS lupa_subconsulta"
  )
  compilado_subconsulta <- lupa:::.with_compilador_dbi(con, motor, {
    lupa:::.compilar_sql_dbi(con, subconsulta, "subconsulta", motor)
  })
  registro_subconsulta <- compilado_subconsulta$compilacion
  expect_identical(registro_subconsulta$estado[[1L]], "compilada")
  expect_identical(registro_subconsulta$metodo[[1L]], "PREPARE")
}

test_that("MySQL compila planes y rechaza consultas invalidas", {
  .compilar_prueba_probar_mysql(FALSE)
})

test_that("MariaDB compila planes y rechaza consultas invalidas", {
  .compilar_prueba_probar_mysql(TRUE)
})
