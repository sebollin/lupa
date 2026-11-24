testthat::skip_on_cran()
testthat::skip_if_not_installed("DBI")

.cotas_local_conectar <- function(maria) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  testthat::skip_if_not_installed(paquete)
  clave <- Sys.getenv(
    if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD", unset = ""
  )
  testthat::skip_if(!nzchar(clave), paste0(
    "Sin clave para ", if (maria) "MariaDB" else "MySQL", "."
  ))
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
  testthat::skip_if(is.null(con), paste0(
    if (maria) "MariaDB" else "MySQL", " no disponible."
  ))
  con
}

.cotas_local_comprobar <- function(con, rotulo) {
  tabla <- "lupa_plan_cotas_local"
  quitar <- function() suppressWarnings(try(
    DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
  ))
  quitar()
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id INTEGER, x DOUBLE, d DATE, s VARCHAR(20))"
  ))
  valores <- paste0(
    "(", 1:12, ", ", 1:12 / 4, ", '2024-01-", sprintf("%02d", 1:12),
    "', 'v", 1:12 %% 3L, "')"
  )
  DBI::dbExecute(con, paste(
    "INSERT INTO", tabla, "VALUES", paste(valores, collapse = ", ")
  ))
  on.exit(quitar(), add = TRUE)
  comprobaciones <- 0L
  for (caso in c("valores", "vacia")) {
    if (identical(caso, "vacia")) DBI::dbExecute(con, paste("DELETE FROM", tabla))
    for (bloque in c("con_muestra", "solo_agregados")) {
      plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
        con, tabla, bloque_muestra = bloque
      )))
      perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, bloque_muestra = bloque,
        fecha = as.POSIXct("2024-01-01", tz = "UTC")
      )))
      emitidas <- as.numeric(perfil$resumen_tabla$meta$consultas$emitidas)
      testthat::expect_gte(
        emitidas, as.numeric(attr(plan, "total_minimo")),
        label = paste(rotulo, caso, bloque, "minimo")
      )
      testthat::expect_lte(
        emitidas, as.numeric(attr(plan, "total_maximo")),
        label = paste(rotulo, caso, bloque, "maximo")
      )
      comprobaciones <- comprobaciones + 2L
    }
    if (identical(caso, "vacia")) DBI::dbExecute(con, paste(
      "INSERT INTO", tabla, "VALUES", paste(valores, collapse = ", ")
    ))
  }
  cat("comprobaciones de cotas ", rotulo, ": ", comprobaciones, "\n", sep = "")
}

testthat::test_that("MariaDB mantiene el emitido dentro de las cotas", {
  motor <- getOption("lupa_plan_cotas_motor")
  if (!is.null(motor)) testthat::skip_if(
    !identical(motor, "mariadb"), "Prueba reservada para MariaDB."
  )
  con <- .cotas_local_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .cotas_local_comprobar(con, "MariaDB")
})

testthat::test_that("MySQL mantiene el emitido dentro de las cotas", {
  motor <- getOption("lupa_plan_cotas_motor")
  if (!is.null(motor)) testthat::skip_if(
    !identical(motor, "mysql"), "Prueba reservada para MySQL."
  )
  con <- .cotas_local_conectar(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .cotas_local_comprobar(con, "MySQL")
})

testthat::test_that("el entero ancho conserva las cotas locales", {
  motor <- getOption("lupa_plan_cotas_motor")
  if (!is.null(motor)) testthat::skip_if(
    !motor %in% c("mariadb", "mysql"), "Prueba reservada para motores locales."
  )
  motores <- if (is.null(motor)) c("mariadb", "mysql") else motor
  comprobar <- function(nombre) {
    con <- .cotas_local_conectar(identical(nombre, "mariadb"))
    tabla <- "lupa_plan_cotas_ancho"
    quitar <- function() suppressWarnings(try(
      DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
    ))
    quitar()
    on.exit({quitar(); DBI::dbDisconnect(con)}, add = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (c1 BIGINT)"))
    valores <- paste0(
      "(", sprintf("%.0f", 9007199254740992 + 2 * 1:12), ")"
    )
    DBI::dbExecute(con, paste(
      "INSERT INTO", tabla, "VALUES", paste(valores, collapse = ", ")
    ))
    plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
      con, tabla, metricas = c("basicos", "mediana")
    )))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = c("basicos", "mediana"),
      fecha = as.POSIXct("2024-01-01", tz = "UTC")
    )))
    emitidas <- as.numeric(perfil$resumen_tabla$meta$consultas$emitidas)
    testthat::expect_gte(
      emitidas, as.numeric(attr(plan, "total_minimo")),
      label = paste(nombre, "entero ancho minimo")
    )
    testthat::expect_lte(
      emitidas, as.numeric(attr(plan, "total_maximo")),
      label = paste(nombre, "entero ancho maximo")
    )
    cat("comprobaciones de cotas ", nombre, " entero ancho: 2\n", sep = "")
  }
  for (nombre in motores) comprobar(nombre)
})
