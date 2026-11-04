.abb_con_pg <- function(puerto) {
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

.abb_con_mssql <- function() {
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

.abb_hex <- function() {
  i <- seq_len(300L)
  ifelse(i <= 40L, "0a0b0c", sprintf(
    "%02x%02x%02x%02x", i %% 256L, (i * 7L) %% 256L, i %/% 256L, 1L
  ))
}

.abb_sin_avisos <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(aviso) {
    avisos <<- c(avisos, conditionMessage(aviso))
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

.abb_comprobar <- function(conexion, tabla, binario, crear) {
  try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)), silent = TRUE)
  on.exit({
    try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE ", tabla, " (id int, bin ", binario, ")"
  ))
  filas <- paste0("(", seq_len(300L), ",", vapply(
    .abb_hex(), crear, character(1L)
  ), ")")
  for (lote in split(filas, ceiling(seq_along(filas) / 100L))) {
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(lote, collapse = ",")
    ))
  }
  por_bloques <- .abb_sin_avisos(perfilar_dbi(
    conexion, tabla, universo = "tabla_completa",
    bloque_muestra = "con_muestra", muestra = 1000L,
    bloque_filas = 400L, proteger_datos_personales = FALSE
  ))
  con_muestra <- .abb_sin_avisos(perfilar_dbi(
    conexion, tabla, universo = "tabla_completa",
    bloque_muestra = "con_muestra", muestra = 1000L,
    proteger_datos_personales = FALSE
  ))
  expect_length(por_bloques$avisos, 0L)
  expect_length(con_muestra$avisos, 0L)
  bloque <- por_bloques$valor$resumen_tabla$columnas
  bloque <- bloque[bloque$columna == "bin", , drop = FALSE]
  muestra <- con_muestra$valor$perfil_muestra$columnas
  muestra <- muestra[muestra$columna == "bin", , drop = FALSE]
  expect_equal(as.numeric(bloque$n_validos), 300)
  for (campo in c(
    "longitud_minima", "longitud_maxima", "longitud_media", "n_distintos"
  )) {
    expect_equal(as.numeric(bloque[[campo]]), as.numeric(muestra[[campo]]),
                 info = campo)
  }
  expect_identical(as.character(bloque$moda), as.character(muestra$moda))
  expect_identical(bloque$frecuencia_moda, muestra$frecuencia_moda)
  expect_equal(as.numeric(bloque$longitud_minima), 3)
  expect_equal(as.numeric(bloque$longitud_maxima), 4)
  expect_equal(as.numeric(bloque$longitud_media), 3.866666666666667)
}

test_that("binarias por bloques coinciden con la muestra en PostgreSQL", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .abb_con_pg(puerto)
    .abb_comprobar(
      con, paste0("a2561_bin_pg_", puerto), "bytea",
      function(hex) paste0("decode('", hex, "','hex')")
    )
  }
})

test_that("binarias por bloques coinciden con la muestra en SQL Server", {
  skip_on_cran()
  con <- .abb_con_mssql()
  .abb_comprobar(con, "a2561_bin_mssql", "varbinary(100)",
                 function(hex) paste0("0x", hex))
})
