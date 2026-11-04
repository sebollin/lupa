.abin_con_pg <- function(puerto) {
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

.abin_con_mssql <- function() {
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

# 300 filas: 40 de un valor de 3 bytes y el resto distintas, para que la moda
# sea una y la tasa de distintos supere el umbral de alta cardinalidad.
.abin_hex <- function() {
  i <- seq_len(300L)
  ifelse(i <= 40L, "0a0b0c", sprintf("%02x%02x%02x%02x", i %% 256L,
                                     (i * 7L) %% 256L, i %/% 256L, 1L))
}

.abin_perfilar <- function(con, tabla, literal) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  tipo <- if (grepl("^0x", literal(""))) "varbinary(100)" else "bytea"
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id int, bin ", tipo, ")"))
  filas <- paste0("(", seq_len(300L), ",", literal(.abin_hex()), ")")
  for (b in split(filas, ceiling(seq_along(filas) / 150L))) {
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ",
                               paste(b, collapse = ",")))
  }
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", bloque_muestra = "con_muestra",
    muestra = 1000L, proteger_datos_personales = FALSE
  )))
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  resultado
}

.abin_comprobar <- function(resultado) {
  muestra <- resultado$perfil_muestra$columnas
  fila <- muestra[muestra$columna == "bin", , drop = FALSE]
  expect_identical(as.character(fila$tipo_declarado), "blob")
  expect_identical(as.character(fila$tipo_inferido), "blob")
  expect_true(is.na(fila$unicode_evaluado))
  expect_equal(as.numeric(fila$longitud_minima), 3)
  expect_equal(as.numeric(fila$longitud_maxima), 4)
  h <- resultado$perfil_muestra$hallazgos
  expect_false(any(h$columna %in% "bin" & h$tipo_hallazgo %in% "alta_cardinalidad"))
  moda <- resultado$resumen_tabla$columnas$moda[
    resultado$resumen_tabla$columnas$columna == "bin"
  ]
  expect_match(as.character(moda), "^3:[0-9a-f]{32}$")
  as.character(moda)
}

test_that("una columna binaria de la muestra es blob y su moda es la misma en dos motores", {
  skip_on_cran()
  modas <- character()
  for (puerto in c(55432L, 55493L)) {
    con <- .abin_con_pg(puerto)
    r <- .abin_perfilar(con, paste0("abin_pg_", puerto),
                        function(h) paste0("decode('", h, "','hex')"))
    modas <- c(modas, .abin_comprobar(r))
    DBI::dbDisconnect(con)
  }
  con <- .abin_con_mssql()
  r <- .abin_perfilar(con, "abin_mssql", function(h) paste0("0x", h))
  modas <- c(modas, .abin_comprobar(r))
  DBI::dbDisconnect(con)
  expect_length(unique(modas), 1L)
})
