.adv_con_pg <- function(puerto) {
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

.adv_con_mssql <- function() {
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

.adv_sd_referencia <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  y <- x / escala
  m <- mean(y)
  d <- y - m
  sqrt(max(0, (sum(d^2) - sum(d)^2 / length(y)) / (length(y) - 1L))) * escala
}

.adv_media_referencia <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  sum(x / escala) / length(x) * escala
}

.adv_tolerancia <- function(x) {
  if (length(x) == 1L && is.finite(x) && x != 0) 1e-12 else 1e-320
}

test_that("la muestra vacia declara cero filas en PostgreSQL", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .adv_con_pg(puerto)
    tabla <- paste0("adv_zero_", puerto)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla,
      " (id int, fija char(8), texto text, bin bytea)"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " SELECT i, lpad(i::text,8,'0'), repeat('x',3000),",
      " decode(repeat('ab',300),'hex') FROM generate_series(1,5) i"
    ))
    aviso <- NULL
    resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 3L,
      muestra = 3L, max_bytes_materializacion = 4096,
      proteger_datos_personales = FALSE
    )), warning = function(w) {
      aviso <<- w
      invokeRestart("muffleWarning")
    })
    expect_equal(resultado$resumen_tabla$meta$muestreo$filas_obtenidas, 0)
    expect_equal(resultado$resumen_tabla$meta$materializacion$n_filas, 0)
    expect_match(conditionMessage(aviso), "0 fila\\(s\\)")
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("orden_muestra queda declarado cuando gobierna TABLESAMPLE o RANDOM", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .adv_con_pg(puerto)
    tabla <- paste0("adv_order_", puerto)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id int, x int)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES (1,10),(2,20),(3,30),(4,40)"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 3L,
      muestra = 3L, orden_muestra = "id",
      proteger_datos_personales = FALSE
    )))
    orden <- resultado$resumen_tabla$meta$orden_muestra
    expect_identical(orden$solicitado, "id")
    expect_identical(orden$aplicado, FALSE)
    expect_match(orden$motivo, "no se aplico", ignore.case = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("SQL Server calcula bordes extremos con centro escalado", {
  skip_on_cran()
  con <- .adv_con_mssql()
  tabla <- "adv_extremos_mssql"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id int, xmax float, x6 float, casi float, signado float, alea float)"
  ))
  set.seed(17)
  xmax <- .Machine$double.xmax
  casi <- c(xmax, xmax * (1 - 2^-52), xmax)
  signado <- c(xmax, -xmax, 0, 1)
  alea <- runif(200L, 1e307, 1.7e308)
  literal <- function(x) if (is.na(x)) "NULL" else sprintf("%.17g", x)
  filas <- vapply(seq_len(200L), function(i) paste0(
    "(", i, ",", literal(if (i <= 30L) xmax else NA_real_), ",",
    literal(if (i <= 30L) 6e306 else NA_real_), ",",
    literal(if (i <= 3L) casi[[i]] else NA_real_), ",",
    literal(if (i <= 4L) signado[[i]] else NA_real_), ",",
    literal(alea[[i]]), ")"
  ), character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ",")
  ))
  guardado <- DBI::dbGetQuery(con, paste0(
    "SELECT xmax,x6,casi,signado,alea FROM ", tabla
  ))
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  columnas <- resultado$resumen_tabla$columnas
  for (nombre in names(guardado)) {
    esperado <- .adv_sd_referencia(guardado[[nombre]])
    observado <- columnas$desvio[columnas$columna == nombre]
    if (esperado == 0) {
      expect_identical(as.numeric(observado), 0)
    } else {
      expect_equal(observado, esperado, tolerance = abs(esperado) * 1e-12)
    }
  }
})

test_that("SQL Server recorta CHAR y corrobora la moda binaria", {
  skip_on_cran()
  con <- .adv_con_mssql()
  tabla <- "adv_texto_bin_mssql"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id int, ch char(10), bin varbinary(max))"
  ))
  filas <- c(
    paste0("(", seq_len(40L), ",'ab',CONVERT(varbinary(max),REPLICATE(0x01,400)))"),
    paste0("(", 40L + seq_len(30L), ",'cd',CONVERT(varbinary(max),REPLICATE(0x02,400)))"),
    paste0("(", 70L + seq_len(20L), ",'ef',CONVERT(varbinary(max),REPLICATE(0x03,400)))")
  )
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ",")
  ))
  completa <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", proteger_datos_personales = FALSE
  )))
  muestra <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 1000L,
    muestra = 1000L, orden_muestra = "id", proteger_datos_personales = FALSE
  )))
  moda_completa <- completa$resumen_tabla$columnas$moda[
    completa$resumen_tabla$columnas$columna == "bin"
  ]
  moda_muestra <- muestra$resumen_tabla$columnas$moda[
    muestra$resumen_tabla$columnas$columna == "bin"
  ]
  expect_identical(as.character(moda_completa), as.character(moda_muestra))
  expect_match(as.character(moda_completa), "^400:")
  expect_equal(
    completa$resumen_tabla$columnas$moda[
      completa$resumen_tabla$columnas$columna == "ch"
    ], "ab"
  )
  expect_equal(
    muestra$perfil_muestra$columnas$moda[
      muestra$perfil_muestra$columnas$columna == "ch"
    ], "ab"
  )
  expect_false(any(completa$resumen_tabla$cobertura$estado == "no_corroborada"))
})

test_that("PostgreSQL calcula extremos con la referencia escalada", {
  skip_on_cran()
  literal <- function(x) if (is.na(x)) "NULL" else sprintf("%.17g", x)
  for (puerto in c(55432L, 55493L)) {
    con <- .adv_con_pg(puerto)
    tabla <- paste0("adv_extremos_pg_", puerto)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla,
      " (id int, tiny30 double precision, tiny_mix double precision,",
      " sub_e200 double precision, borde151 double precision, casi_xm double precision)"
    ))
    set.seed(20261104 + puerto)
    tiny30 <- 1e-160 * (1 + runif(30L))
    tiny_mix <- c(1e-160 * runif(20L), -1e-161 * runif(20L))
    sub_e200 <- 1e-200 * (1 + runif(50L))
    borde151 <- 1e151 + runif(40L) * 1e140
    casi_xm <- c(.Machine$double.xmax,
                 .Machine$double.xmax * (1 - 2^-52),
                 .Machine$double.xmax)
    filas <- vapply(seq_len(50L), function(i) paste0(
      "(", i, ",", literal(if (i <= length(tiny30)) tiny30[[i]] else NA_real_),
      ",", literal(if (i <= length(tiny_mix)) tiny_mix[[i]] else NA_real_),
      ",", literal(if (i <= length(sub_e200)) sub_e200[[i]] else NA_real_),
      ",", literal(if (i <= length(borde151)) borde151[[i]] else NA_real_),
      ",", literal(if (i <= length(casi_xm)) casi_xm[[i]] else NA_real_), ")"
    ), character(1L))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ",")
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
      proteger_datos_personales = FALSE
    )))
    columnas <- resultado$resumen_tabla$columnas
    esperados <- list(
      tiny30 = tiny30, tiny_mix = tiny_mix, sub_e200 = sub_e200,
      borde151 = borde151, casi_xm = casi_xm
    )
    for (nombre in names(esperados)) {
      esperado <- .adv_sd_referencia(esperados[[nombre]])
      observado <- columnas$desvio[columnas$columna == nombre]
      media <- columnas$media[columnas$columna == nombre]
      esperado_media <- .adv_media_referencia(esperados[[nombre]])
      expect_equal(as.numeric(media), esperado_media,
                   tolerance = .adv_tolerancia(esperado_media),
                   info = nombre)
      expect_equal(as.numeric(observado), esperado,
                   tolerance = .adv_tolerancia(esperado),
                   info = nombre)
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("SQL Server calcula los tres rangos extremos", {
  skip_on_cran()
  con <- .adv_con_mssql()
  tabla <- "adv_extremos_mssql_nuevos"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (id int, tiny30 float, sub_e200 float, borde151 float, casi_xm float)"
  ))
  set.seed(20261105)
  tiny30 <- 1e-160 * (1 + runif(30L))
  sub_e200 <- 1e-200 * (1 + runif(50L))
  borde151 <- 1e151 + runif(40L) * 1e140
  casi_xm <- c(.Machine$double.xmax,
               .Machine$double.xmax * (1 - 2^-52),
               .Machine$double.xmax)
  literal <- function(x) if (is.na(x)) "NULL" else sprintf("%.17g", x)
  filas <- vapply(seq_len(50L), function(i) paste0(
    "(", i, ",", literal(if (i <= length(tiny30)) tiny30[[i]] else NA_real_),
    ",", literal(if (i <= length(sub_e200)) sub_e200[[i]] else NA_real_),
    ",", literal(if (i <= length(borde151)) borde151[[i]] else NA_real_),
    ",", literal(if (i <= length(casi_xm)) casi_xm[[i]] else NA_real_), ")"
  ), character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ",")
  ))
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )))
  columnas <- resultado$resumen_tabla$columnas
  esperados <- list(
    tiny30 = tiny30, sub_e200 = sub_e200, borde151 = borde151,
    casi_xm = casi_xm
  )
  for (nombre in names(esperados)) {
    esperado <- .adv_sd_referencia(esperados[[nombre]])
    observado <- columnas$desvio[columnas$columna == nombre]
    expect_equal(as.numeric(observado), esperado,
                 tolerance = .adv_tolerancia(esperado),
                 info = nombre)
  }
})

test_that("PostgreSQL conserva la moda binaria en tabla y muestra", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .adv_con_pg(puerto)
    tabla <- paste0("adv_bin_pg_", puerto)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (id int, bin bytea)"
    ))
    valores <- vapply(seq_len(90L), function(i) paste0(
      "(", i, ",decode(repeat('", if (i <= 40L) "01" else
        if (i <= 70L) "02" else "03", "',400),'hex'))"
    ), character(1L))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(valores, collapse = ",")
    ))
    completa <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa", proteger_datos_personales = FALSE
    )))
    muestra <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 1000L,
      muestra = 1000L, proteger_datos_personales = FALSE
    )))
    moda_completa <- completa$resumen_tabla$columnas$moda[
      completa$resumen_tabla$columnas$columna == "bin"
    ]
    moda_muestra <- muestra$perfil_muestra$columnas$moda[
      muestra$perfil_muestra$columnas$columna == "bin"
    ]
    expect_identical(as.character(moda_completa), as.character(moda_muestra))
    expect_match(as.character(moda_completa), "^400:")
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    DBI::dbDisconnect(con)
  }
})
