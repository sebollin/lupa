.av3_con_pg <- function(puerto) {
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

.av3_con_mssql <- function() {
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

.av3_limpiar <- function(con, tablas) {
  for (tabla in tablas) {
    existe <- tryCatch(DBI::dbExistsTable(con, tabla), error = function(e) FALSE)
    if (isTRUE(existe)) {
      try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    }
  }
}

.av3_desconectar <- function(con) {
  valido <- tryCatch(DBI::dbIsValid(con), error = function(e) FALSE)
  if (isTRUE(valido)) DBI::dbDisconnect(con)
}

.av3_registro <- function(perfil, columna, metrica) {
  sql <- perfil$resumen_tabla$sql
  sql[sql$columna == columna & sql$metrica == metrica, , drop = FALSE]
}

.av3_sd_exacto <- function(x) {
  x <- as.numeric(x)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  y <- x / escala
  media <- sum(y) / length(y)
  sqrt(sum((y - media)^2) / (length(y) - 1)) * escala
}

test_that("R3 conserva basicos cuando el desvio por bloques no representa", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r3_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x double precision)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES (1.7976931348623157e308),",
      "(-1.7976931348623157e308),(1.7976931348623157e308),",
      "(-1.7976931348623157e308)"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, bloque_filas = 2L, metricas = c("basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    fila <- perfil$resumen_tabla$columnas
    expect_equal(fila$minimo, -.Machine$double.xmax)
    expect_equal(fila$maximo, .Machine$double.xmax)
    expect_equal(fila$media, 0)
    expect_equal(fila$n_ceros, 0)
    expect_equal(fila$n_negativos, 2)
    expect_true(is.na(fila$desvio))
    expect_identical(
      as.character(.av3_registro(perfil, "x", "desvio")$estado),
      "no_disponible"
    )
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R4 no acusa una columna numerica al agotarse el presupuesto", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r4_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x double precision)"))
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1),(2),(3)"))
    for (tope in c(2L, 3L)) {
      perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, bloque_filas = 5000L, max_consultas = tope,
        metricas = c("basicos", "desvio"), proteger_datos_personales = FALSE
      )))
      registros <- perfil$resumen_tabla$sql
      registros <- registros[registros$columna == "x" &
        registros$metrica %in% c("media", "desvio"), , drop = FALSE]
      expect_true(all(as.character(registros$estado) == "no_disponible"))
      expect_false(any(grepl("no es numerica", registros$motivo,
                             fixed = TRUE)))
    }
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R5 conserva la moda decimal sin una consulta extra", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r5_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (d numeric(20,2))"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " SELECT 1.23 FROM generate_series(1,20000)"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, bloque_filas = 5000L, max_consultas = 4L,
      metricas = c("moda", "basicos", "desvio", "mediana"),
      proteger_datos_personales = FALSE
    )))
    expect_identical(as.character(perfil$resumen_tabla$columnas$moda), "1.23")
    expect_identical(
      as.character(.av3_registro(perfil, "d", "moda")$estado), "calculado"
    )
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R6 calcula ABS de enteros en un tipo que no desborda", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r6_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (s smallint, i integer, b bigint)"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " VALUES (-32768,-2147483648,-9223372036854775808),(1,1,1),(2,2,2)"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = c("basicos", "desvio"),
      incluir_valores = FALSE, proteger_datos_personales = FALSE
    )))
    columnas <- perfil$resumen_tabla$columnas
    for (nombre in c("s", "i", "b")) {
      valores <- switch(nombre,
        s = c(-32768, 1, 2),
        i = c(-2147483648, 1, 2),
        b = c(-9223372036854775808, 1, 2)
      )
      observado <- columnas[columnas$columna == nombre, , drop = FALSE]
      expect_equal(observado$media, mean(valores), tolerance = 1e-12)
      expect_equal(observado$desvio, .av3_sd_exacto(valores), tolerance = 1e-12)
      expect_equal(observado$n_negativos, 1)
    }
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R8 conserva mediana de real en PostgreSQL", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r8_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (r real)"))
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (0.3),(1)"))
    for (pedidas in list("mediana", c("desvio", "mediana"))) {
      perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, metricas = pedidas, proteger_datos_personales = FALSE
      )))
      fila <- perfil$resumen_tabla$columnas
      fila <- fila[fila$columna == "r", , drop = FALSE]
      expect_equal(fila$mediana, 0.65, tolerance = 1e-14)
      expect_identical(
        as.character(.av3_registro(perfil, "r", "mediana")$estado),
        "calculado"
      )
    }
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R11 desempata la moda binaria como la muestra", {
  skip_on_cran()
  con <- .av3_con_mssql()
  tabla <- "av3_r11_mssql"
  on.exit({
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }, add = TRUE)
  .av3_limpiar(con, tabla)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (bin varbinary(max))"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla,
    " VALUES (CONVERT(varbinary(max),0x01)),",
    "(CONVERT(varbinary(max),0x01)),",
    "(CONVERT(varbinary(max),0x0102)),",
    "(CONVERT(varbinary(max),0x0102))"
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, metricas = "moda", proteger_datos_personales = FALSE
  )))
  esperado <- "1:55a54008ad1ba589aa210d2629c1df41"
  expect_identical(as.character(perfil$resumen_tabla$columnas$moda), esperado)
  expect_identical(as.character(perfil$perfil_muestra$columnas$moda), esperado)
  expect_false(any(as.character(perfil$resumen_tabla$sql$estado) == "divergencia"))
})

test_that("R12 calcula el desvio pequeno por bloques", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r12_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x double precision)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " SELECT 1e-150 + i*1e-165 FROM generate_series(1,10) i"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, bloque_filas = 3L, metricas = "desvio",
      proteger_datos_personales = FALSE
    )))
    expect_equal(
      perfil$resumen_tabla$columnas$desvio,
      3.0398228676209173e-165,
      tolerance = 1e-12
    )
    expect_identical(
      as.character(.av3_registro(perfil, "x", "desvio")$estado), "calculado"
    )
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("R13 no publica NA como calculado en bloques", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_r13_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (uno integer, vacia integer, single integer)"
    ))
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1,NULL,7)"))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, bloque_filas = 1L, metricas = c("basicos", "desvio"),
      proteger_datos_personales = FALSE
    )))
    for (nombre in c("uno", "single")) {
      registro <- .av3_registro(perfil, nombre, "desvio")
      expect_identical(as.character(registro$estado), "no_aplica")
      expect_true(is.na(perfil$resumen_tabla$columnas$desvio[
        perfil$resumen_tabla$columnas$columna == nombre
      ]))
    }
    for (metrica in c("minimo", "media", "desvio")) {
      registro <- .av3_registro(perfil, "vacia", metrica)
      expect_identical(as.character(registro$estado), "sin_valores")
    }
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("Caso 2 no paga sonda de magnitud redundante", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .av3_con_pg(puerto)
    tabla <- paste0("av3_c2_pg", puerto)
    on.exit({
      .av3_limpiar(con, tabla)
      .av3_desconectar(con)
    }, add = TRUE)
    .av3_limpiar(con, tabla)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x integer)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " SELECT i FROM generate_series(1,10) i"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = "desvio", incluir_valores = FALSE,
      proteger_datos_personales = FALSE
    )))
    plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(
      con, tabla, metricas = "desvio", incluir_valores = FALSE
    )))
    expect_equal(perfil$resumen_tabla$columnas$desvio, 3.02765035409749,
                 tolerance = 1e-12)
    expect_false(any(as.character(perfil$resumen_tabla$sql$metrica) ==
                     "sonda_magnitud"))
    # La estimacion de filas del catalogo del aviso de costo se manda siempre y
    # ahora se cuenta; el tope de 12 es el de las demas consultas.
    aviso <- lupa:::.consultas_catalogo_aviso_dbi(
      con, tabla, list(universo = "tabla_completa")
    )
    expect_lte(perfil$resumen_tabla$meta$consultas$emitidas - aviso, 12)
    expect_equal(perfil$resumen_tabla$meta$consultas$emitidas,
                 attr(plan, "total"))
    .av3_limpiar(con, tabla)
    .av3_desconectar(con)
  }
})

test_that("con bloque_filas, SQL Server conserva la mediana de las columnas numericas", {
  skip_on_cran()
  con <- .av3_con_mssql()
  tabla <- "av3_mediana_bloques"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id int, x float, m money)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("(", 1:10, ",", (1:10) * 1.5, ",", (1:10) * 2.25, ")", collapse = ",")
  ))
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
    bloque_filas = 3L, proteger_datos_personales = FALSE
  )))
  f <- r$resumen_tabla$columnas
  # El prototipo de cero filas dice la clase; el tipo de SQL Server llega como
  # codigo ODBC. Antes la mediana salia «La columna no es numerica».
  expect_equal(as.numeric(f$mediana[f$columna == "id"]), 5.5)
  expect_equal(as.numeric(f$mediana[f$columna == "x"]), 8.25)
  expect_equal(as.numeric(f$mediana[f$columna == "m"]), 12.375)
  s <- r$resumen_tabla$sql
  expect_false(any(grepl("no es numerica", s$motivo[s$metrica == "mediana"],
                         fixed = TRUE)))
})
