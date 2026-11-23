# El plan de las consultas de fechas contra la corrida en los motores de
# servidor (ver `test-fechas-plan-consultas.R`, que recorre la grilla entera en
# SQLite y DuckDB). En PostgreSQL y en SQL Server el resumen de fechas y los
# periodos no se cuentan por literales: se declaran, o se leen por bloques.
#
# - Una muestra de la grilla: lo emitido cae en el rango del plan y la
#   conciliacion queda `conciliado`. PostgreSQL 9.3 y SQL Server rechazan una
#   capacidad al prepararse, y esa falla no desfasa una lectura de fechas que
#   activaron los datos.
# - Con `universo = "muestra_motor"`, un `TABLESAMPLE` que devuelve menos filas
#   que las pedidas se reintenta: la segunda seleccion es condicional, dentro
#   del maximo de su clase, y no desfasa.
# - Con la sesion de PostgreSQL fuera de UTC, un periodo entre un `TIMESTAMP` y
#   un `TIMESTAMPTZ` leido por bloques da lo que da `perfilar()` sobre lo que
#   el controlador lee, aunque el motor comparando de otra cosa.
#
# Se saltean si el motor no esta disponible en el entorno local.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.fpm_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"), port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.fpm_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin clave de SQL Server.")
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

.fpm_pg_actual <- function() .fpm_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
.fpm_pg_93 <- function() .fpm_conectar_pg(55493L)

# ---- Una muestra de la grilla ------------------------------------------------

.fpm_grilla <- function(con, motor) {
  muestra <- .fpc_muestra_grilla(con, motor, paste0("lupa_fpm_", motor))
  expect_identical(muestra$recorridas, 40L)
  # La tabla sucia activo lecturas condicionales: si no, la muestra no prueba
  # que una condicional concilie en este motor.
  expect_gt(muestra$condicionales, 3L)
}

test_that("PostgreSQL: el plan cuenta las consultas de fechas que la corrida emite", {
  con <- .fpm_pg_actual()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_grilla(con, "pg")
})

test_that("PostgreSQL 9.3: el plan cuenta las consultas de fechas que la corrida emite", {
  con <- .fpm_pg_93()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_grilla(con, "pg93")
})

test_that("SQL Server: el plan cuenta las consultas de fechas que la corrida emite", {
  con <- .fpm_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_grilla(con, "mssql")
})

# ---- Un periodo y un resumen sobre columnas de texto, por bloques -------------

.fpm_texto <- function(con, motor) {
  tabla <- paste0("lupa_fpm_", motor, "_texto")
  .fpc_borrar_tabla(con, tabla)
  on.exit(.fpc_borrar_tabla(con, tabla), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (lupa_id INTEGER, lupa_desde VARCHAR(30), lupa_hasta VARCHAR(30), lupa_nota VARCHAR(30))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (1, '2020-01-01', '2020-06-01', '2020-01-01'), ",
    "(2, '2020-02-01', '2020-07-01', '2020-01-02')"
  ))
  par <- .fpc_par(con, tabla, list(
    metricas = "basicos", resumen_fechas = TRUE, estrategia_no_sql = "bloques",
    bloque_filas = 1L, periodos = c(inicio = "lupa_desde", fin = "lupa_hasta")
  ))
  expect_identical(.fpc_desacuerdos(par, exacto = FALSE), NULL, label = motor)
  # Las tres columnas de texto que la muestra lee como fecha se leen, y es
  # condicional: el plan no sabe que son fechas hasta que la muestra lo dice.
  resumen <- .fpc_clase(par, "resumen")
  expect_identical(c(resumen$seguras, resumen$maximas, resumen$emitidas), c(0, 3, 3),
                   label = motor)
  expect_identical(resumen$direccion, "condicional", label = motor)
  periodos <- .fpc_clase(par, "periodos")
  expect_identical(c(periodos$seguras, periodos$emitidas), c(1, 1), label = motor)
}

test_that("PostgreSQL: las lecturas de texto por bloques son condicionales y concilian", {
  con <- .fpm_pg_actual()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_texto(con, "pg")
})

test_that("PostgreSQL 9.3: las lecturas de texto por bloques son condicionales y concilian", {
  con <- .fpm_pg_93()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_texto(con, "pg93")
})

test_that("SQL Server: las lecturas de texto por bloques son condicionales y concilian", {
  con <- .fpm_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_texto(con, "mssql")
})

# ---- La muestra del motor que se reintenta ------------------------------------

# Sobre una tabla de una sola pagina, `TABLESAMPLE` por bloques la devuelve
# entera o no devuelve nada: con una fraccion de un cuarto, tres de cada cuatro
# selecciones salen cortas y se reintentan. Doce corridas sin ningun reintento
# pasan menos de una vez en diez millones.
.fpm_reintento <- function(con, motor) {
  tabla <- paste0("lupa_fpm_", motor, "_muestra")
  .fpc_borrar_tabla(con, tabla)
  on.exit(.fpc_borrar_tabla(con, tabla), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (lupa_id INTEGER, lupa_valor INTEGER, lupa_nota VARCHAR(30))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", 1:8, ", ", (1:8) * 3L, ", 'a", 1:8, "')", collapse = ", ")
  ))
  argumentos <- list(metricas = "basicos", universo = "muestra_motor", muestra_motor = 2L)
  plan <- suppressWarnings(do.call(plan_perfilado_dbi, c(list(con, tabla), argumentos)))
  seleccion <- plan[plan$clase_consulta == "seleccion de la muestra (spool de la sesion)", ]
  expect_identical(c(seleccion$n_consultas, seleccion$n_consultas_max), c(1, 2), label = motor)
  reintentos <- 0L
  for (corrida in 1:12) {
    perfil <- suppressWarnings(do.call(
      perfilar_dbi, c(list(con, tabla, fecha = .fpc_fecha), argumentos)
    ))
    consultas <- perfil$resumen_tabla$meta$consultas
    expect_identical(consultas$conciliacion$estado, "conciliado", label = motor)
    expect_equal(consultas$conciliacion$emitidas, consultas$emitidas, label = motor)
    expect_lte(consultas$emitidas, attr(plan, "total_maximo"), label = motor)
    detalle <- consultas$conciliacion$detalle
    if (consultas$emitidas > attr(plan, "total")) {
      reintentos <- reintentos + 1L
      expect_identical(detalle$clase_consulta, "seleccion de la muestra (spool de la sesion)",
                       label = motor)
      expect_identical(detalle$direccion, "condicional", label = motor)
      expect_identical(c(detalle$n_anunciadas, detalle$n_emitidas), c(1, 2), label = motor)
    } else {
      expect_identical(nrow(detalle), 0L, label = motor)
    }
  }
  expect_gt(reintentos, 0L, label = motor)
}

test_that("PostgreSQL: la seleccion de la muestra que se reintenta es condicional y concilia", {
  con <- .fpm_pg_actual()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_reintento(con, "pg")
})

test_that("SQL Server: la seleccion de la muestra que se reintenta es condicional y concilia", {
  con <- .fpm_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_reintento(con, "mssql")
})

# ---- PostgreSQL con la sesion fuera de UTC ------------------------------------

# Los instantes estan a menos de tres horas uno del otro: es donde el huso de la
# sesion cambia el orden entre un `TIMESTAMP`, que no lleva huso, y un
# `TIMESTAMPTZ`. El controlador entrega los dos como instantes en UTC, sea cual
# sea el huso de la sesion.
.fpm_huso <- function(con, motor) {
  tabla <- paste0("lupa_fpm_", motor, "_huso")
  cifras <- c(
    "n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
    "n_fin_abierto_centinela", "n_fin_centinela_declarado",
    "n_fin_centinela_sin_declarar", "n_sin_inicio_con_fin",
    "n_ambos_ausentes_o_centinela"
  )
  periodo <- c(inicio = "lupa_ts", fin = "lupa_tz")
  .fpc_borrar_tabla(con, tabla)
  on.exit(.fpc_borrar_tabla(con, tabla), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (lupa_id INTEGER, lupa_ts TIMESTAMP, lupa_tz TIMESTAMPTZ)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    "(1, TIMESTAMP '2020-01-01 01:00:00', TIMESTAMPTZ '2020-01-01 02:00:00+00'), ",
    "(2, TIMESTAMP '2020-06-01 12:00:00', TIMESTAMPTZ '2020-06-01 11:00:00+00'), ",
    "(3, TIMESTAMP '2020-12-01 18:00:00', TIMESTAMPTZ '2020-12-01 23:30:00+00'), ",
    "(4, TIMESTAMP '2021-03-01 00:00:00', TIMESTAMPTZ '2021-02-28 22:00:00+00'), ",
    "(5, TIMESTAMP '2021-07-01 00:00:00', NULL)"
  ))
  en_el_motor <- character()
  for (huso in c("UTC", "America/Montevideo", "Asia/Tokyo")) {
    DBI::dbExecute(con, paste0("SET TIME ZONE '", huso, "'"))
    memoria <- suppressWarnings(perfilar(
      DBI::dbReadTable(con, tabla), fecha = .fpc_fecha, periodos = periodo,
      analizar_dependencias = FALSE
    ))$general$periodos
    expect_identical(c(memoria$n_cerrados_validos, memoria$n_fin_antes_inicio), c(2, 2),
                     label = paste(motor, huso))
    correr <- function(estrategia) {
      suppressWarnings(perfilar_dbi(
        con, tabla, fecha = .fpc_fecha, periodos = periodo, metricas = "basicos",
        estrategia_no_sql = estrategia
      ))$resumen_tabla$meta$periodos
    }
    declarado <- correr("no_medible")
    expect_identical(declarado$estado, "no_disponible", label = paste(motor, huso))
    contado <- correr("bloques")
    expect_identical(contado$estado, "calculado", label = paste(motor, huso))
    expect_identical(contado[cifras], memoria[cifras], label = paste(motor, huso))
    cerrados <- DBI::dbGetQuery(con, paste0(
      "SELECT SUM(CASE WHEN lupa_ts <= lupa_tz THEN 1 ELSE 0 END) AS n FROM ", tabla
    ))$n
    en_el_motor <- c(en_el_motor, as.character(as.numeric(cerrados)))
  }
  # El control: el motor, comparando, da una cuenta distinta en cada huso. Si
  # diera siempre la de la memoria, esta prueba no estaria probando el huso.
  expect_identical(en_el_motor, c("2", "1", "4"), label = motor)
}

test_that("PostgreSQL: con la sesion fuera de UTC, TIMESTAMP contra TIMESTAMPTZ por bloques da lo de la memoria", {
  con <- .fpm_pg_actual()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_huso(con, "pg")
})

test_that("PostgreSQL 9.3: con la sesion fuera de UTC, TIMESTAMP contra TIMESTAMPTZ por bloques da lo de la memoria", {
  con <- .fpm_pg_93()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .fpm_huso(con, "pg93")
})
