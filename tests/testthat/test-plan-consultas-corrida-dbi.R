# `plan_perfilado_dbi()` dice cuantas consultas va a emitir `perfilar_dbi()`,
# y `max_consultas` las cuenta igual. No coincidian: por la muestra del motor
# sin bloque de filas el plan cobraba las consultas de la forma del desvio, que
# la corrida no emite, y no contaba la lectura de comprobacion de la muestra;
# por el spool contaba agregados SQL que no corren; por bloques contaba solo el
# recorrido;
# y en PostgreSQL y SQL Server la tabla completa mandaba una estimacion de
# catalogo sin contarla. Las sentencias se cuentan aca por construccion, en los
# genericos de DBI, y no con lo que el paquete dice de si mismo.

.pcc_genericas <- c(
  "dbGetQuery", "dbSendQuery", "dbExecute", "dbSendStatement",
  "dbExistsTable", "dbListFields", "dbListTables", "dbReadTable",
  "dbRemoveTable"
)

# Solo cuenta la llamada de afuera: la `dbGetQuery()` por omision llama a
# `dbSendQuery()`, y esa no es otra sentencia. Las de la guarda de transaccion
# y las lecturas de `pg_stat_statements` de `instrumentar` quedan aparte: no
# son consultas sobre la tabla y la ayuda las declara fuera de la cuenta.
.pcc_sentencias <- function(codigo) {
  estado <- new.env()
  estado$prof <- 0L
  estado$textos <- character()
  for (generica in .pcc_genericas) {
    suppressMessages(trace(
      generica, where = asNamespace("DBI"), print = FALSE,
      tracer = bquote({
        if (get("prof", envir = .(estado)) == 0L) {
          assign("textos", c(get("textos", envir = .(estado)), tryCatch(
            if (exists("statement", inherits = FALSE)) {
              paste(statement, collapse = " ")
            } else .(generica),
            error = function(e) .(generica)
          )), envir = .(estado))
        }
        assign("prof", get("prof", envir = .(estado)) + 1L, envir = .(estado))
      }),
      exit = bquote(
        assign("prof", get("prof", envir = .(estado)) - 1L, envir = .(estado))
      )
    ))
  }
  on.exit(for (generica in .pcc_genericas) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)
  valor <- force(codigo)
  fuera <- grepl(
    "lupa_trancount|SAVEPOINT|lupa_ahora|pg_stat_statements",
    estado$textos, useBytes = TRUE
  )
  list(valor = valor, n = sum(!fuera))
}

.pcc_vias <- list(
  completa = list(),
  completa_sin_muestra = list(bloque_muestra = "solo_agregados"),
  bloques = list(bloque_filas = 20L),
  bloques_sin_muestra = list(bloque_filas = 20L,
                             bloque_muestra = "solo_agregados"),
  motor_sin_muestra = list(universo = "muestra_motor", muestra_motor = 30L,
                           bloque_muestra = "solo_agregados"),
  motor_spool = list(universo = "muestra_motor", muestra_motor = 30L)
)

.pcc_fecha <- as.POSIXct("2026-11-07 12:00:00", tz = "UTC")

.pcc_medir <- function(con, tabla, via, ...) {
  argumentos <- c(list(con, tabla), .pcc_vias[[via]], list(...))
  plan <- suppressWarnings(suppressMessages(
    do.call(plan_perfilado_dbi, argumentos)
  ))
  corrida <- .pcc_sentencias(suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(argumentos, list(proteger_datos_personales = FALSE,
                                     fecha = .pcc_fecha))
  ))))
  list(plan = plan, perfil = corrida$valor, n = corrida$n)
}

.pcc_comprobar_exacto <- function(con, tabla, vias = names(.pcc_vias)) {
  for (via in vias) {
    medido <- .pcc_medir(con, tabla, via)
    info <- paste(tabla, via)
    expect_identical(
      as.numeric(medido$n), as.numeric(attr(medido$plan, "total")),
      info = info
    )
    expect_identical(
      as.numeric(medido$perfil$resumen_tabla$meta$consultas$emitidas),
      as.numeric(medido$n), info = info
    )
  }
}

test_that("en SQLite el plan anuncia las sentencias de cada via", {
  # Doce perfiles con los genericos de DBI trazados: pesada para CRAN.
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE pcc_datos (id INTEGER PRIMARY KEY, t TEXT, x REAL)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO pcc_datos VALUES ", paste0(
      "(", 1:60, ", 'v", (1:60) %% 7L, "', ", ((1:60) * 37L) %% 101L / 7, ")",
      collapse = ", "
    )
  ))
  # Solo texto y sin clave primaria: la corrida paga las consultas de la forma
  # del desvio aunque ninguna columna sea numerica, y la via por bloques va por el
  # localizador, con su segunda pasada.
  DBI::dbExecute(con, "CREATE TABLE pcc_textos (t TEXT, u TEXT)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO pcc_textos VALUES ", paste0(
      "('a", (1:60) %% 5L, "', 'b", (1:60) %% 11L, "')", collapse = ", "
    )
  ))
  .pcc_comprobar_exacto(con, "pcc_datos")
  .pcc_comprobar_exacto(con, "pcc_textos")
})

test_that("en DuckDB el plan anuncia las sentencias de cada via, con decimales", {
  # Doce perfiles con los genericos de DBI trazados: pesada para CRAN.
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE pcc_datos (id INTEGER PRIMARY KEY, t VARCHAR,",
    "d DECIMAL(12,2), x DOUBLE)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO pcc_datos SELECT i, 'v' || (i % 7),",
    "CAST(((i * 7919) % 10000) AS DECIMAL(12,2)) / 100, i * 0.25",
    "FROM range(1, 61) AS r(i)"
  ))
  # DuckDB no tiene lectura incremental demostrada: la via por bloques paga la
  # preparacion y nada mas, y el plan tiene que decirlo.
  .pcc_comprobar_exacto(con, "pcc_datos")
})

.pcc_pg <- function() {
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
  con
}

.pcc_mssql <- function() {
  skip_on_cran()
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

# Cuarenta filas: cantidad par, y las dos centrales del decimal son dobles
# distintos, asi que su mediana por bloques es una consulta propia.
.pcc_crear_servidor <- function(con, tabla, decimal, doble) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id int PRIMARY KEY, t varchar(20), d ",
    decimal, ", x ", doble, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste0(
      "(", 1:40, ", 'v", (1:40) %% 7L, "', ",
      sprintf("%.2f", ((1:40) * 7919L) %% 10000L / 100), ", ",
      sprintf("%.6f", ((1:40) * 37L) %% 101L / 7), ")",
      collapse = ", "
    )
  ))
}

.pcc_estado <- function(perfil, columna, metrica) {
  sql <- perfil$resumen_tabla$sql
  as.character(sql$estado[sql$columna == columna & sql$metrica == metrica])
}

.pcc_comprobar_servidor <- function(con, tabla) {
  # Tabla completa: la estimacion de filas del catalogo del aviso de costo, y
  # en PostgreSQL `SHOW extra_float_digits` y la mediana del `float8`.
  for (via in c("completa", "completa_sin_muestra")) {
    medido <- .pcc_medir(con, tabla, via)
    expect_identical(as.numeric(medido$n), as.numeric(attr(medido$plan, "total")),
                     info = via)
    expect_identical(
      as.numeric(medido$perfil$resumen_tabla$meta$consultas$emitidas),
      as.numeric(medido$n), info = via
    )
  }
  # La muestra del motor sale de `TABLESAMPLE`, y un sorteo corto se
  # reintenta: la corrida cae dentro del rango, no en un numero.
  for (via in c("motor_sin_muestra", "motor_spool")) {
    medido <- .pcc_medir(con, tabla, via)
    expect_gte(medido$n, attr(medido$plan, "total"), label = via)
    expect_lte(medido$n, attr(medido$plan, "total_maximo"), label = via)
    expect_identical(
      as.numeric(medido$perfil$resumen_tabla$meta$consultas$emitidas),
      as.numeric(medido$n), info = via
    )
  }
  # Bloques: la mediana del decimal la piden sus centrales y va al extremo
  # superior; la media, una para todos los decimales, esta en el total.
  medido <- .pcc_medir(con, tabla, "bloques_sin_muestra")
  expect_gte(medido$n, attr(medido$plan, "total"))
  expect_lte(medido$n, attr(medido$plan, "total_maximo"))
  expect_identical(
    as.numeric(medido$perfil$resumen_tabla$meta$consultas$emitidas),
    as.numeric(medido$n)
  )
  expect_identical(.pcc_estado(medido$perfil, "d", "media"), "calculado")
  expect_identical(.pcc_estado(medido$perfil, "d", "mediana"), "calculado")
  # Con el total del plan como presupuesto, lo que el total anuncia entra: la
  # media del decimal; con el maximo, entra tambien la mediana.
  justo <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, bloque_filas = 20L, bloque_muestra = "solo_agregados",
    max_consultas = attr(medido$plan, "total"),
    proteger_datos_personales = FALSE, fecha = .pcc_fecha
  )))
  expect_identical(.pcc_estado(justo, "d", "media"), "calculado")
  holgado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, bloque_filas = 20L, bloque_muestra = "solo_agregados",
    max_consultas = attr(medido$plan, "total_maximo"),
    proteger_datos_personales = FALSE, fecha = .pcc_fecha
  )))
  expect_identical(.pcc_estado(holgado, "d", "media"), "calculado")
  expect_identical(.pcc_estado(holgado, "d", "mediana"), "calculado")
  # Y con la muestra: la lectura de `perfil_muestra` tiene su lugar reservado.
  medido <- .pcc_medir(con, tabla, "bloques")
  expect_gte(medido$n, attr(medido$plan, "total"))
  expect_lte(medido$n, attr(medido$plan, "total_maximo"))
  justo <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, bloque_filas = 20L, max_consultas = attr(medido$plan, "total"),
    proteger_datos_personales = FALSE, fecha = .pcc_fecha
  )))
  expect_false(is.null(justo$perfil_muestra))
  expect_identical(.pcc_estado(justo, "d", "media"), "calculado")
}

test_that("en PostgreSQL el plan anuncia lo que la corrida manda", {
  con <- .pcc_pg()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE pcc_plan_pg"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .pcc_crear_servidor(con, "pcc_plan_pg", "numeric(12,2)", "double precision")
  .pcc_comprobar_servidor(con, "pcc_plan_pg")
})

test_that("en SQL Server el plan anuncia lo que la corrida manda", {
  con <- .pcc_mssql()
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE pcc_plan_mssql"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .pcc_crear_servidor(con, "pcc_plan_mssql", "decimal(12,2)", "float")
  .pcc_comprobar_servidor(con, "pcc_plan_mssql")
})
