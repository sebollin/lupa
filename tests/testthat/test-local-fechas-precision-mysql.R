# Periodos y resumen de fechas en MariaDB y MySQL: lo que `test-fechas-precision.R`
# prueba en SQLite y DuckDB imitando al motor, aca con el motor de verdad. Aparte
# porque sus controladores no estan en Suggests (ver
# `test-local-fechas-motor-mysql.R`).
testthat::skip_on_cran()

.fpl_conectar <- function(maria) {
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

.fpl_fecha <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

.fpl_cifras <- c(
  "n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
  "n_fin_abierto_centinela", "n_fin_centinela_declarado",
  "n_fin_centinela_sin_declarar", "n_sin_inicio_con_fin",
  "n_ambos_ausentes_o_centinela"
)

.fpl_par <- c(inicio = "lupa_desde", fin = "lupa_hasta")

# La tabla se escribe con CREATE TABLE e INSERT: MySQL puede tener apagada la
# carga local que usa `dbWriteTable()`.
.fpl_tabla <- function(con, tabla, columnas, filas) {
  try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (", columnas, ")"))
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ", paste0("(", filas, ")", collapse = ", ")))
}

.fpl_filas_contratos <- function() {
  c(
    "1, 'A', '2020-01-01', '2020-06-01'", "2, 'A', '2020-02-01', '2020-01-15'",
    "3, 'B', '2020-03-01', NULL", "4, 'A', NULL, '2020-05-01'",
    "5, 'B', '2021-01-01', '9999-12-31'", "6, 'A', '2021-05-05', '2021-06-01'",
    "7, NULL, '2022-01-01', '2022-02-01'", "8, 'A', '2019-01-01', NULL",
    "9, 'B', '2023-01-01', '2022-01-01'", "10, 'A', '2023-06-01', '2099-12-31'"
  )
}

.fpl_periodo <- function(x) x$resumen_tabla$meta$periodos

test_that("MariaDB: un periodo con universo declarado se declara, y por bloques da lo de la memoria", {
  con <- .fpl_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_precision_maria"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fpl_tabla(con, tabla, "lupa_id INT, lupa_estado VARCHAR(1), lupa_desde DATE, lupa_hasta DATE",
             .fpl_filas_contratos())
  leido <- suppressWarnings(DBI::dbReadTable(con, tabla))
  for (declarado in list(
    list(aplicabilidad = list(lupa_hasta = ~ lupa_estado == "A")),
    list(columnas_opcionales = "lupa_hasta")
  )) {
    memoria <- suppressWarnings(do.call(perfilar, c(list(
      leido, fecha = .fpl_fecha, analizar_dependencias = FALSE, periodos = .fpl_par
    ), declarado)))$general$periodos
    expect_lt(memoria$n, 10)
    sin_bloques <- .fpl_periodo(suppressWarnings(do.call(perfilar_dbi, c(list(
      con, tabla, fecha = .fpl_fecha, periodos = .fpl_par
    ), declarado))))
    expect_identical(sin_bloques$estado, "no_disponible")
    por_bloques <- .fpl_periodo(suppressWarnings(do.call(perfilar_dbi, c(list(
      con, tabla, fecha = .fpl_fecha, periodos = .fpl_par,
      estrategia_no_sql = "bloques"
    ), declarado))))
    expect_identical(por_bloques$estado, "calculado")
    expect_identical(por_bloques[.fpl_cifras], memoria[.fpl_cifras])
  }
})

test_that("MariaDB: en un TIMESTAMP la aplicabilidad manda sobre la lectura por bloques", {
  con <- .fpl_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_precision_maria_ts"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fpl_tabla(con, tabla, "lupa_estado VARCHAR(1), lupa_alta TIMESTAMP NULL", c(
    "'A', '2024-01-01 00:00:00'", "'A', '2024-02-01 00:00:00'",
    "'B', '2027-01-01 00:00:00'", "'B', '2027-02-01 00:00:00'"
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, tabla, fecha = .fpl_fecha, resumen_fechas = TRUE,
    aplicabilidad = list(lupa_alta = ~ lupa_estado == "A"),
    estrategia_no_sql = "bloques"
  ))
  s <- perfil$resumen_tabla$sql
  fila <- s[s$columna == "lupa_alta" & s$metrica == "n_fechas_futuras", , drop = FALSE]
  expect_identical(fila$estado, "no_disponible")
})

test_that("MariaDB: una regla sobre una columna que llega como 0 y 1 no se aplica y se declara", {
  con <- .fpl_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_precision_maria_regla"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fpl_tabla(con, tabla, paste(
    "lupa_id INT, lupa_activo BOOLEAN, lupa_monto DOUBLE,",
    "lupa_desde DATE, lupa_hasta DATE"
  ), c(
    "1, 1, 5, '2020-01-01', '2020-06-01'", "2, 0, 7, '2020-02-01', '2020-01-15'",
    "3, 1, 9, '2020-03-01', NULL", "4, NULL, 11, '2020-04-01', '2020-05-01'",
    "5, 1, 13, '2021-01-01', '2021-02-01'", "6, 0, 15, '2021-05-05', '2021-06-01'"
  ))
  fila <- function(x) {
    columnas <- x$perfil_muestra$columnas
    salida <- columnas[columnas$columna == "lupa_monto", , drop = FALSE]
    rownames(salida) <- NULL
    salida
  }
  con_regla <- suppressWarnings(perfilar_dbi(
    con, tabla, fecha = .fpl_fecha, aplicabilidad = list(lupa_monto = ~ lupa_activo)
  ))
  sin_regla <- suppressWarnings(perfilar_dbi(con, tabla, fecha = .fpl_fecha))
  expect_false(is.null(con_regla$perfil_muestra))
  expect_identical(fila(con_regla), fila(sin_regla))
  cobertura <- con_regla$resumen_tabla$cobertura
  propia <- cobertura[cobertura$bloque == "perfil_muestra" &
                        cobertura$elemento == "lupa_monto", , drop = FALSE]
  expect_identical(nrow(propia), 1L)
  expect_match(propia$como_resolverlo, "~ lupa_activo == 1", fixed = TRUE)
  for (estrategia in c("no_medible", "bloques")) {
    periodo <- .fpl_periodo(suppressWarnings(perfilar_dbi(
      con, tabla, fecha = .fpl_fecha, periodos = .fpl_par,
      aplicabilidad = list(lupa_hasta = ~ lupa_activo),
      estrategia_no_sql = estrategia
    )))
    expect_identical(periodo$estado, "no_disponible", label = estrategia)
    expect_true(all(is.na(unlist(periodo[.fpl_cifras]))), label = estrategia)
  }
  expect_match(periodo$motivo, "~ lupa_activo == 1", fixed = TRUE)
})

test_that("MySQL: con muestra_motor el periodo no se cuenta sobre la tabla entera, tampoco por bloques", {
  con <- .fpl_conectar(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_precision_mysql"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fpl_tabla(con, tabla, "lupa_id INT, lupa_estado VARCHAR(1), lupa_desde DATE, lupa_hasta DATE",
             .fpl_filas_contratos())
  fila <- .fpl_periodo(suppressWarnings(perfilar_dbi(
    con, tabla, fecha = .fpl_fecha, periodos = .fpl_par,
    universo = "muestra_motor", muestra_motor = 4,
    estrategia_no_sql = "bloques"
  )))
  expect_identical(fila$estado, "no_disponible")
  expect_true(all(is.na(unlist(fila[.fpl_cifras]))))
  expect_match(fila$motivo, "muestra_motor", fixed = TRUE)
})
