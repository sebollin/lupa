.aes_con_pg <- function(puerto) {
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

.aes_con_mssql <- function() {
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

.aes_literal <- function(x, motor = "postgres") {
  if (is.na(x)) return("NULL")
  if (is.nan(x)) {
    return(if (motor == "postgres") {
      "CAST('NaN' AS DOUBLE PRECISION)"
    } else {
      "CAST('NaN' AS FLOAT)"
    })
  }
  if (is.infinite(x)) {
    return(if (motor == "postgres") {
      "CAST('Infinity' AS DOUBLE PRECISION)"
    } else {
      "1e309"
    })
  }
  sprintf("%.17g", x)
}

.aes_es_cifra <- function(observado, esperado) {
  is.finite(observado) && observado != 0 && is.finite(esperado) &&
    esperado != 0 && abs(observado / esperado - 1) <= 1e-12
}

.aes_crear_tabla <- function(con, tabla, x, motor = "postgres") {
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
  tipo <- if (motor == "mssql") "float" else "double precision"
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                             " (id int, x ", tipo, ")"))
  valores <- vapply(x, .aes_literal, character(1L), motor = motor)
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", seq_along(x), ",", valores, ")", collapse = ",")
  ))
}

.aes_fila <- function(resultado) {
  resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
  ]
}

.aes_probar_desvio <- function(con, tabla, x, esperado, motor = "postgres") {
  .aes_crear_tabla(con, tabla, x, motor)
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
    metricas = "desvio", incluir_valores = FALSE,
    proteger_datos_personales = FALSE
  )))
  fila <- .aes_fila(resultado)
  sql <- resultado$resumen_tabla$sql
  registro <- sql[sql$columna == "x" & sql$metrica == "desvio", , drop = FALSE]
  expect_true(.aes_es_cifra(as.numeric(fila$desvio), esperado))
  expect_identical(as.character(registro$estado[[1L]]), "calculado")
  expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
  invisible(resultado)
}

test_that("escala el desvio sin basicos ni valores en PG y SQL Server", {
  skip_on_cran()
  set.seed(20261104)
  tiny <- 1e-160 * (1 + runif(30L))
  invisible(runif(80L))
  sub <- 1e-149 * (1 + runif(40L) * 1e-8)
  ulp <- 1e-150 * (1 + (0:9) * 2^-52)
  casos <- list(
    tiny = list(x = tiny, esperado = 3.212636809309274e-161),
    sub = list(x = sub, esperado = 3.058769917596946e-158),
    ulp = list(x = ulp, esperado = 6.683092180725329e-166)
  )
  probar_pg <- function(puerto) {
    con <- .aes_con_pg(puerto)
    tablas <- paste0("aes_pg_sin_", puerto, "_", names(casos))
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    for (nombre in names(casos)) {
      .aes_probar_desvio(
        con, paste0("aes_pg_sin_", puerto, "_", nombre),
        casos[[nombre]]$x, casos[[nombre]]$esperado
      )
    }
  }
  for (puerto in c(55432L, 55493L)) probar_pg(puerto)
  probar_mssql <- function() {
    con <- .aes_con_mssql()
    on.exit({
    for (nombre in names(casos)) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS aes_mssql_sin_",
                                     nombre)), silent = TRUE)
    }
    DBI::dbDisconnect(con)
    }, add = TRUE)
    for (nombre in names(casos)) {
      .aes_probar_desvio(
        con, paste0("aes_mssql_sin_", nombre), casos[[nombre]]$x,
        casos[[nombre]]$esperado, motor = "mssql"
      )
    }
  }
  probar_mssql()
})

test_that("los basicos ocultos deciden la escala de media y desvio", {
  skip_on_cran()
  set.seed(20261104)
  extremos <- c(1e300, 0, 0, 0)
  invisible(runif(290L))
  alea <- runif(200L, 1e307, 1.7e308)
  casos <- list(
    extremos = list(
      x = extremos, media = 2.5e299, desvio = 5e299
    ),
    alea = list(
      x = alea, media = 8.621965374052524e307,
      desvio = 4.508407076339053e307
    )
  )
  probar <- function(con, prefijo, motor = "postgres") {
    for (nombre in names(casos)) {
      tabla <- paste0(prefijo, nombre)
      .aes_crear_tabla(con, tabla, casos[[nombre]]$x, motor)
      resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, universo = "tabla_completa",
        bloque_muestra = "solo_agregados", metricas = c("basicos", "desvio"),
        incluir_valores = FALSE, proteger_datos_personales = FALSE
      )))
      fila <- .aes_fila(resultado)
      expect_true(.aes_es_cifra(as.numeric(fila$media), casos[[nombre]]$media),
                  info = paste(prefijo, nombre, "media"))
      expect_true(.aes_es_cifra(as.numeric(fila$desvio), casos[[nombre]]$desvio),
                  info = paste(prefijo, nombre, "desvio"))
      expect_false(any(as.character(resultado$resumen_tabla$sql$estado) ==
                       "no_disponible"))
      expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
    }
  }
  probar_pg <- function(puerto) {
    con <- .aes_con_pg(puerto)
    tablas <- paste0("aes_pg_basicos_", puerto, "_", names(casos))
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    probar(con, paste0("aes_pg_basicos_", puerto, "_"))
  }
  for (puerto in c(55432L, 55493L)) probar_pg(puerto)
  probar_mssql <- function() {
    con <- .aes_con_mssql()
    tablas <- paste0("aes_mssql_basicos_", names(casos))
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    probar(con, "aes_mssql_basicos_", motor = "mssql")
  }
  probar_mssql()
})

test_that("un solo valor no dispara el repliegue del desvio", {
  skip_on_cran()
  probar <- function(con, tabla, motor = "postgres") {
    .aes_crear_tabla(con, tabla, c(1e300, NA_real_, NA_real_), motor)
    avisos <- character()
    resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
      metricas = "desvio", proteger_datos_personales = FALSE
    )), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    registro <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$columna == "x" &
        resultado$resumen_tabla$sql$metrica == "desvio", , drop = FALSE
    ]
    expect_identical(as.character(registro$estado[[1L]]), "no_aplica")
    expect_match(as.character(registro$motivo[[1L]]), "al menos dos")
    expect_length(avisos, 0L)
    expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
  }
  probar_pg <- function(puerto) {
    con <- .aes_con_pg(puerto)
    tabla <- paste0("aes_pg_uno_", puerto)
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS aes_pg_uno_",
                                     puerto)), silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    probar(con, tabla)
  }
  for (puerto in c(55432L, 55493L)) probar_pg(puerto)
  probar_mssql <- function() {
    con <- .aes_con_mssql()
    on.exit({
      try(DBI::dbExecute(con, "DROP TABLE IF EXISTS aes_mssql_uno"),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    probar(con, "aes_mssql_uno", motor = "mssql")
  }
  probar_mssql()
})

test_that("PostgreSQL excluye NaN e infinitos de las magnitudes", {
  skip_on_cran()
  probar_pg <- function(puerto) {
    con <- .aes_con_pg(puerto)
    tabla <- paste0("aes_nan_pg_", puerto)
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    .aes_crear_tabla(con, tabla, c(Inf, NaN, 1e200, -1e200))
    avisos <- character()
    resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
      metricas = c("validos", "basicos", "desvio"),
      proteger_datos_personales = FALSE
    )), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    fila <- .aes_fila(resultado)
    expect_equal(as.numeric(fila$minimo), -1e200)
    expect_equal(as.numeric(fila$maximo), 1e200)
    expect_equal(as.numeric(fila$media), 0)
    expect_true(.aes_es_cifra(
      as.numeric(fila$desvio), 1.4142135623730951e200
    ))
    expect_length(avisos, 0L)
    expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
    registros <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$columna == "x" &
        resultado$resumen_tabla$sql$metrica %in%
          c("minimo", "maximo", "media", "desvio"), , drop = FALSE
    ]
    expect_true(all(as.character(registros$estado) == "calculado"))
  }
  for (puerto in c(55432L, 55493L)) probar_pg(puerto)
})
