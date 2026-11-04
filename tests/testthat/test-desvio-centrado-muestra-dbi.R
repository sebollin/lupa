.acn_con_pg <- function(puerto) {
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

.acn_con_mssql <- function() {
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

.acn_casos <- function() {
  set.seed(7)
  list(
    e6_m8 = 1e6 + runif(100L) * 1e-8,
    epoch_hora = 1.7e9 + runif(100L) * 3600,
    e6_u = 1e6 + runif(100L)
  )
}

.acn_exactos <- c(
  e6_m8 = 2.8324886649005893e-9,
  epoch_hora = 964.2306713548253,
  e6_u = 0.27123367509526173
)

.acn_cargar <- function(con, tabla, tipo, valores) {
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INT, x ", tipo, ")"
  ))
  filas <- paste0(
    "(", seq_along(valores), ",", sprintf("%.17g", valores), ")"
  )
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ",")
  ))
}

.acn_probar_desvio <- function(con, tabla, tipo) {
  casos <- .acn_casos()
  tablas <- character()
  on.exit({
    for (nombre in tablas) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", nombre)),
          silent = TRUE)
    }
  }, add = TRUE)
  for (nombre in names(casos)) {
    tabla_caso <- paste0(tabla, "_", nombre)
    tablas <- c(tablas, tabla_caso)
    .acn_cargar(con, tabla_caso, tipo, casos[[nombre]])
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla_caso, universo = "muestra_motor",
      muestra_motor = 100000L, bloque_muestra = "solo_agregados",
      metricas = c("basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
    ]
    expect_equal(as.numeric(fila$n), 100)
    expect_true(is.finite(as.numeric(fila$desvio)), info = nombre)
    expect_true(
      abs(as.numeric(fila$desvio) - .acn_exactos[[nombre]]) <= 1e-12,
      info = nombre
    )
  }
}

test_that("PostgreSQL centra el desvio de la muestra del motor", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .acn_con_pg(puerto)
    .acn_probar_desvio(con, paste0("acn_desvio_pg_", puerto),
                      "double precision")
    DBI::dbDisconnect(con)
  }
})

test_that("SQL Server centra el desvio de la muestra del motor", {
  skip_on_cran()
  con <- .acn_con_mssql()
  .acn_probar_desvio(con, "acn_desvio_mssql", "float")
  DBI::dbDisconnect(con)
})

test_that("n y filas obtenidas declaran el presupuesto agotado", {
  skip_on_cran()
  casos_conexion <- list(
    pg16 = function() .acn_con_pg(55432L),
    pg93 = function() .acn_con_pg(55493L),
    mssql = .acn_con_mssql
  )
  for (nombre in names(casos_conexion)) {
    con <- casos_conexion[[nombre]]()
    tabla <- paste0("acn_presupuesto_", nombre)
    .acn_cargar(con, tabla, if (nombre == "mssql") "float" else "double precision",
                seq_len(60L))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 1000L,
      max_consultas = 3L, proteger_datos_personales = FALSE
    )))
    sql_n <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$metrica == "n", , drop = FALSE
    ]
    filas_n <- resultado$resumen_tabla$columnas$n
    expect_equal(nrow(sql_n), length(filas_n))
    for (i in seq_along(filas_n)) {
      if (is.na(filas_n[[i]])) {
        expect_identical(sql_n$estado[[i]], "no_disponible")
        expect_true(nzchar(as.character(sql_n$motivo[[i]])))
      } else {
        expect_identical(sql_n$estado[[i]], "calculado")
      }
    }
    muestreo <- resultado$resumen_tabla$meta$muestreo
    expect_true("motivo_filas_obtenidas" %in% names(muestreo))
    if (is.na(muestreo$filas_obtenidas)) {
      expect_true(nzchar(as.character(muestreo$motivo_filas_obtenidas)))
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("una vista de PostgreSQL centra el desvio sin ctid", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .acn_con_pg(puerto)
    tabla <- paste0("avn_vista_base_", puerto)
    vista <- paste0("avn_vista_muestra_", puerto)
    try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)),
        silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla,
      " AS SELECT g AS id, (g % 97) * 1.5::double precision AS x",
      " FROM generate_series(1, 5000) g"
    ))
    DBI::dbExecute(con, paste0(
      "CREATE VIEW ", vista, " AS SELECT id, x FROM ", tabla
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, vista, universo = "muestra_motor", muestra_motor = 500L,
      bloque_muestra = "solo_agregados",
      metricas = c("validos", "basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
    ]
    expect_true(is.finite(as.numeric(fila$media)))
    expect_true(is.finite(as.numeric(fila$desvio)))
    expect_true(is.finite(as.numeric(fila$n_validos)))
    sql <- resultado$resumen_tabla$sql$sql
    expect_false(any(grepl("ctid|MD5\\(|REPEATABLE", sql,
                           perl = TRUE), na.rm = TRUE))
    try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)),
        silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("una vista de SQL Server conserva NEWID y centra el desvio", {
  skip_on_cran()
  con <- .acn_con_mssql()
  tabla <- "avn_vista_mssql_base"
  vista <- "avn_vista_mssql_muestra"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)),
        silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP VIEW IF EXISTS ", vista)),
      silent = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id int, x float)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " SELECT TOP 500 ROW_NUMBER() OVER (ORDER BY (SELECT 1)),",
    " ROW_NUMBER() OVER (ORDER BY (SELECT 1)) FROM sys.all_objects a CROSS JOIN sys.all_objects b"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT id, x FROM ", tabla
  ))
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, vista, universo = "muestra_motor", muestra_motor = 100L,
    bloque_muestra = "solo_agregados",
    metricas = c("validos", "basicos", "desvio"),
    proteger_datos_personales = FALSE
  )))
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
  ]
  expect_true(is.finite(as.numeric(fila$media)))
  expect_true(is.finite(as.numeric(fila$desvio)))
  expect_true(is.finite(as.numeric(fila$n_validos)))
  expect_identical(
    as.character(resultado$resumen_tabla$meta$muestreo$metodo),
    "random_limit"
  )
  expect_true(grepl(
    "NEWID", resultado$resumen_tabla$meta$muestreo$sql, fixed = TRUE
  ))
  expect_false(any(grepl(
    "TABLESAMPLE", resultado$resumen_tabla$sql$sql,
    ignore.case = TRUE
  ), na.rm = TRUE))
})

test_that("una vista materializada de PostgreSQL usa la muestra comun", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .acn_con_pg(puerto)
    vista <- paste0("avn_vista_mat_muestra_", puerto)
    try(DBI::dbExecute(con, paste0(
      "DROP MATERIALIZED VIEW IF EXISTS ", vista
    )), silent = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE MATERIALIZED VIEW ", vista,
      " AS SELECT g AS id, (g % 97) * 1.5::double precision AS x",
      " FROM generate_series(1, 5000) g WITH DATA"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, vista, universo = "muestra_motor", muestra_motor = 500L,
      bloque_muestra = "solo_agregados",
      metricas = c("validos", "basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
    ]
    expect_true(is.finite(as.numeric(fila$media)))
    expect_true(is.finite(as.numeric(fila$desvio)))
    expect_true(is.finite(as.numeric(fila$n_validos)))
    expect_false(any(grepl(
      "ctid|MD5\\(|REPEATABLE", resultado$resumen_tabla$sql$sql,
      perl = TRUE
    ), na.rm = TRUE))
    try(DBI::dbExecute(con, paste0(
      "DROP MATERIALIZED VIEW IF EXISTS ", vista
    )), silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("una tabla particionada o heredada no repite ctid entre partes", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .acn_con_pg(puerto)
    tabla <- paste0("avn_partes_", puerto)
    izquierda <- paste0(tabla, "_a")
    derecha <- paste0(tabla, "_b")
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla,
                                   " CASCADE")), silent = TRUE)
    if (puerto == 55432L) {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla,
        " (id integer, x double precision) PARTITION BY RANGE (id)"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", izquierda, " PARTITION OF ", tabla,
        " FOR VALUES FROM (1) TO (1001)"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", derecha, " PARTITION OF ", tabla,
        " FOR VALUES FROM (1001) TO (2001)"
      ))
    } else {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla, " (id integer, x double precision)"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", izquierda, " (CHECK (id < 1001)) INHERITS (",
        tabla, ")"
      ))
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", derecha, " (CHECK (id >= 1001)) INHERITS (",
        tabla, ")"
      ))
    }
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", izquierda,
      " SELECT g, g::double precision FROM generate_series(1, 1000) g"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", derecha,
      " SELECT g, g::double precision FROM generate_series(1001, 2000) g"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 1000L,
      bloque_muestra = "solo_agregados",
      metricas = c("validos", "basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
    ]
    expect_true(is.finite(as.numeric(fila$media)))
    expect_true(is.finite(as.numeric(fila$desvio)))
    expect_gte(as.numeric(fila$n_validos), 1)
    sql <- resultado$resumen_tabla$sql$sql
    expect_false(any(grepl("ctid|MD5\\(|REPEATABLE", sql,
                           perl = TRUE), na.rm = TRUE))
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla,
                                   " CASCADE")), silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("el reintento reemplaza la fuente de los agregados", {
  skip_on_cran()
  con <- .acn_con_pg(55432L)
  tabla <- "avn_reintento_fuente"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " AS SELECT g AS id, g::double precision AS x",
    " FROM generate_series(1, 3000) g"
  ))
  resultados <- vector("list", 8L)
  for (i in seq_along(resultados)) {
    resultados[[i]] <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 60L,
      bloque_muestra = "solo_agregados",
      metricas = c("basicos", "desvio"), proteger_datos_personales = FALSE
    )))
    if (isTRUE(resultados[[i]]$resumen_tabla$meta$muestreo$reintentos > 0)) {
      break
    }
  }
  hay_reintento <- vapply(
    resultados[!vapply(resultados, is.null, logical(1L))],
    function(x) isTRUE(x$resumen_tabla$meta$muestreo$reintentos > 0),
    logical(1L)
  )
  expect_true(any(hay_reintento))
  resultado <- resultados[[which(hay_reintento)[[1L]]]]
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
  ]
  expect_true(is.finite(as.numeric(fila$media)))
  expect_true(is.finite(as.numeric(fila$desvio)))
  expect_identical(
    as.character(resultado$resumen_tabla$meta$muestreo$metodo),
    "tablesample_bernoulli"
  )
  sql <- resultado$resumen_tabla$sql$sql
  expect_true(any(grepl("TABLESAMPLE BERNOULLI", sql, fixed = TRUE),
                  na.rm = TRUE))
})

test_that("la cardinalidad publicada es la del agregado de la muestra", {
  skip_on_cran()
  con <- .acn_con_pg(55432L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # La tabla es propia de la prueba. Antes leia `aud_cmp_t`, que no la crea el
  # paquete: con la tabla borrada la prueba daba error en vez de medir.
  tabla <- paste0("acn_cardinalidad_", Sys.getpid())
  set.seed(20261103)
  n <- 5000L
  base <- data.frame(
    id = seq_len(n),
    n_int = sample(-500:500, n, TRUE),
    f_dec = round(rnorm(n, 50, 10), 3),
    f_grande = 1e9 + runif(n),
    f_neg = -abs(rnorm(n, 1e6, 3))
  )
  base$f_dec[sample(n, 200L)] <- NA
  base$n_int[sample(n, 50L)] <- NA
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbWriteTable(con, tabla, base)
  DBI::dbExecute(con, paste0(
    "ALTER TABLE ", tabla, " ADD COLUMN d_dec NUMERIC(12,3)"
  ))
  DBI::dbExecute(con, paste0(
    "UPDATE ", tabla, " SET d_dec = CAST(f_dec AS NUMERIC(12,3))"
  ))
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
              silent = TRUE), add = TRUE, after = FALSE)
  for (i in seq_len(15L)) {
    aviso_muestra <- FALSE
    resultado <- withCallingHandlers(
      suppressMessages(perfilar_dbi(
        con, tabla, universo = "muestra_motor",
        muestra_motor = 300L, muestra = 300L,
        bloque_muestra = "solo_agregados",
        proteger_datos_personales = FALSE
      )),
      warning = function(condicion) {
        if (inherits(condicion, "lupa_aviso_muestra_motor")) {
          aviso_muestra <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    filas <- suppressWarnings(as.numeric(
      resultado$resumen_tabla$meta$muestreo$filas_obtenidas
    ))
    filas_agregadas <- suppressWarnings(as.numeric(
      resultado$resumen_tabla$meta$filas_obtenidas_muestra
    ))
    if (length(filas) == 1L && length(filas_agregadas) == 1L &&
        is.finite(filas) && is.finite(filas_agregadas)) {
      expect_identical(filas, filas_agregadas, info = paste0("corrida ", i))
    }
    sql_media <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$metrica == "media", , drop = FALSE
    ]
    no_disponibles <- is.finite(filas) && filas > 0 && any(
      as.character(sql_media$estado) == "no_disponible", na.rm = TRUE
    )
    expect_false(isTRUE(no_disponibles) && !isTRUE(aviso_muestra),
                 info = paste0("corrida ", i))
  }
})
