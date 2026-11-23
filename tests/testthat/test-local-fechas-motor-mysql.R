# El resumen de fechas y los periodos en MariaDB y MySQL (ver
# `test-fechas-motor.R`). Aparte porque sus controladores no estan en Suggests.
# Lo que se promete: cada columna de fecha deja su fila en `resumen_tabla$sql`
# -calculada igual que `perfilar()` o declarada con su motivo-, nunca un `NA`
# sin explicacion.
testthat::skip_on_cran()

.fml_conectar <- function(maria) {
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

.fml_fecha <- as.POSIXct("2026-11-22 12:00:00", tz = "UTC")

.fm_campo_ml <- function(x, columna, campo) {
  columnas <- if (inherits(x, "perfil_dbi")) x$resumen_tabla$columnas else x$columnas
  columnas[[campo]][[match(columna, columnas$columna)]]
}

# La tabla se escribe con CREATE TABLE e INSERT: MySQL puede tener apagada la
# carga local que usa `dbWriteTable()`.
.fml_tabla <- function(con, tabla, columnas, filas) {
  try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (", columnas, ")"))
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ", paste0("(", filas, ")", collapse = ", ")))
}

.fml_filas_contratos <- function() {
  dia <- as.Date("2025-01-01") + (seq_len(40) * 11) %% 500
  hasta <- format(dia + 90)
  hasta[c(3, 9)] <- "9999-12-31"
  hasta[4] <- NA
  hasta[5] <- format(dia[5] - 10)
  nac <- format(as.Date("1950-01-01") + seq_len(40) * 400)
  nac[1:3] <- "1900-01-01"
  alta <- format(as.POSIXct("2026-11-10 08:00:00", tz = "UTC") + seq_len(40) * 30000,
                 "%Y-%m-%d %H:%M:%S")
  q <- function(v) ifelse(is.na(v), "NULL", paste0("'", v, "'"))
  paste(seq_len(40), q(format(dia)), q(hasta), q(nac), q(alta), sep = ", ")
}

.fml_dos_vias <- function(con, tabla, ...) {
  leido <- suppressWarnings(DBI::dbReadTable(con, tabla))
  memoria <- suppressWarnings(perfilar(leido, nombre = tabla, fecha = .fml_fecha,
                                       resumen_fechas = TRUE, analizar_dependencias = FALSE, ...))
  motor <- suppressWarnings(perfilar_dbi(con, tabla, fecha = .fml_fecha, resumen_fechas = TRUE, ...))
  list(memoria = memoria, motor = motor)
}

.fml_diferencias <- function(vias) {
  e <- as.data.frame(comparar_equivalencia(vias$memoria, vias$motor, tolerancia = 0))
  e <- e[e$campo %in% .CAMPOS_RESUMEN_FECHAS, , drop = FALSE]
  e[e$veredicto != "identico" & !(e$motivo %in% "sin_valor_medible_en_ambos"), , drop = FALSE]
}

.fml_estados <- function(perfil_dbi, columna) {
  s <- perfil_dbi$resumen_tabla$sql
  s <- s[s$columna == columna & s$metrica %in% .CAMPOS_RESUMEN_FECHAS, , drop = FALSE]
  stats::setNames(s$estado, s$metrica)
}

test_that("MariaDB: las fechas se cuentan en el motor igual que en memoria, y los periodos tambien", {
  con <- .fml_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_fechas_maria"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fml_tabla(con, tabla, "id INT, desde DATE, hasta DATE, fecha_nacimiento DATE, fecha_alta DATETIME(6)",
             .fml_filas_contratos())
  periodo <- list(inicio = "desde", fin = "hasta", fin_abierto = "9999-12-31")
  vias <- .fml_dos_vias(con, tabla, periodos = periodo, centinelas_fecha = "2025-01-12")
  for (columna in c("desde", "hasta", "fecha_nacimiento", "fecha_alta")) {
    estados <- .fml_estados(vias$motor, columna)
    expect_setequal(names(estados), .CAMPOS_RESUMEN_FECHAS)
    expect_true(all(estados == "calculado"), info = columna)
  }
  expect_equal(nrow(.fml_diferencias(vias)), 0L)
  expect_identical(.fm_campo_ml(vias$motor, "fecha_nacimiento", "n_fechas_centinela"), 3)
  expect_identical(vias$motor$resumen_tabla$meta$periodos, vias$memoria$general$periodos)
})

test_that("MariaDB: un TIMESTAMP y los microsegundos junto a 9999-12-31 se declaran", {
  con <- .fml_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_fechas_maria_bordes"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  otras <- format(as.POSIXct("2015-01-01", tz = "UTC") + (1:40) * 86400 * 30, "%Y-%m-%d %H:%M:%S")
  filas <- c("'9999-12-31 00:00:00', '2020-01-01 00:00:00'",
             "'9999-12-31 00:00:00.000001', '2020-02-01 00:00:00'",
             "'9999-12-31 00:00:00.00001', '2020-03-01 00:00:00'",
             paste0("'", otras, "', '2021-01-01 00:00:00'"))
  .fml_tabla(con, tabla, "ts_dato DATETIME(6), ts_marca TIMESTAMP NULL", filas)
  vias <- .fml_dos_vias(con, tabla)
  expect_true(all(.fml_estados(vias$motor, "ts_dato") == "no_disponible"))
  expect_true(all(.fml_estados(vias$motor, "ts_marca") == "no_disponible"))
  s <- vias$motor$resumen_tabla$sql
  expect_match(s$motivo[s$columna == "ts_marca" & s$metrica == "n_fechas_futuras"], "TIMESTAMP", fixed = TRUE)
  # R lee las tres como 9999-12-31: el motor no las puede contar asi.
  expect_identical(.fm_campo_ml(vias$memoria, "ts_dato", "n_fechas_centinela"), 3)
  bloques <- suppressWarnings(perfilar_dbi(con, tabla, fecha = .fml_fecha, resumen_fechas = TRUE,
                                           estrategia_no_sql = "bloques"))
  expect_identical(.fm_campo_ml(bloques, "ts_dato", "n_fechas_centinela"), 3)
  expect_identical(.fm_campo_ml(bloques, "ts_marca", "n_fechas_futuras"),
                   .fm_campo_ml(vias$memoria, "ts_marca", "n_fechas_futuras"))
})

test_that("MySQL: las fechas que el controlador lee como texto se declaran, y por bloques se cuentan", {
  con <- .fml_conectar(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_fechas_fechas_mysql"
  on.exit(try(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tabla)), silent = TRUE),
          add = TRUE, after = FALSE)
  .fml_tabla(con, tabla, "id INT, desde DATE, hasta DATE, fecha_nacimiento DATE, fecha_alta DATETIME",
             .fml_filas_contratos())
  periodo <- list(inicio = "desde", fin = "hasta", fin_abierto = "9999-12-31")
  vias <- .fml_dos_vias(con, tabla, periodos = periodo)
  for (columna in c("desde", "hasta", "fecha_nacimiento", "fecha_alta")) {
    estados <- .fml_estados(vias$motor, columna)
    expect_setequal(names(estados), .CAMPOS_RESUMEN_FECHAS)
    expect_true(all(estados == "no_disponible"), info = columna)
  }
  expect_true(all(is.na(unlist(vias$motor$resumen_tabla$meta$periodos[c("n", "n_cerrados_validos")]))))
  bloques <- suppressWarnings(perfilar_dbi(con, tabla, fecha = .fml_fecha, resumen_fechas = TRUE,
                                           periodos = periodo, estrategia_no_sql = "bloques"))
  expect_equal(nrow(.fml_diferencias(list(memoria = vias$memoria, motor = bloques))), 0L)
  cifras <- c("n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
              "n_fin_abierto_centinela")
  expect_identical(bloques$resumen_tabla$meta$periodos[cifras], vias$memoria$general$periodos[cifras])
})
