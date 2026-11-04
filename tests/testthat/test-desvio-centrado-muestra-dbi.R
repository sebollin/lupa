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
