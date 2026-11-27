# La tabla de cada lectura de medir_dbi() en PostgreSQL (actual y 9.3) y SQL
# Server: la que resuelve el catalogo del motor, con la base actual, el esquema
# y el nombre que guarda, como en SQLite y DuckDB. Sin ella la columna `tabla`
# de las lecturas era `NA`: dos llamadas sobre la misma tabla con otro rotulo,
# con un INSERT entre ellas, se unian con `rbind()` como si sus cifras
# cerraran, y el control de lecturas era `desconocido`. MySQL y MariaDB, en
# `test-local-medir-dbi-identidad-y-tinyint-mysql.R`. Se saltean sin el motor.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.idsrv_conectar_pg <- function(puerto, base = Sys.getenv("LUPA_PG_DBNAME", "lupa")) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = base, bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.idsrv_conectar_mssql <- function() {
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

.idsrv_fecha <- as.Date("2026-01-01")

.idsrv_medir <- function(con, tablas) {
  nuc <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nuc$NoNulo, paste0("N", e)), e, "x")
  })
  suppressWarnings(medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
                             id_medicion = "j", fecha = .idsrv_fecha))
}

.idsrv_tabla <- function(m, entidad) {
  l <- attr(m, "lecturas", exact = TRUE)
  unique(l$tabla[l$entidad == entidad])
}

.idsrv_citar <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

# Tres rotulos de la misma tabla en una llamada: el nombre, el nombre con el
# esquema por omision y, donde el motor no distingue mayusculas, el nombre en
# mayusculas. Son una tabla. Despues, dos llamadas con otro rotulo y un INSERT
# entre ellas: unidas, no cierran.
.idsrv_probar <- function(con, motor, base, esquema, mayusculas) {
  tabla <- "lupa_mdi_ident"
  borrar <- function() {
    try(DBI::dbExecute(con, paste0("DROP TABLE ", .idsrv_citar(con, tabla))),
        silent = TRUE)
  }
  borrar()
  on.exit(borrar(), add = TRUE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", .idsrv_citar(con, tabla), " (x INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", .idsrv_citar(con, tabla),
                             " VALUES (1), (2), (NULL)"))
  con_esquema <- DBI::Id(schema = esquema, table = tabla)
  rotulos <- list(Nombre = tabla, ConEsquema = con_esquema)
  if (mayusculas) rotulos$Mayusculas <- toupper(tabla)
  m <- .idsrv_medir(con, rotulos)
  lecturas <- attr(m, "lecturas", exact = TRUE)
  expect_false(anyNA(lecturas$tabla), info = motor)
  expect_identical(length(unique(lecturas$tabla)), 1L, info = motor)
  # La identidad termina en la base, el esquema y el nombre citados; delante va
  # la huella del servidor.
  expect_true(endsWith(.idsrv_tabla(m, "Nombre"), paste0(
    ".", .idsrv_esperada(con, base, esquema, tabla)
  )), info = motor)
  expect_identical(m$estado_cuentas, rep("cierran", length(rotulos)), info = motor)
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9)))
  expect_false(any(s$control_lecturas == "desconocido"), info = motor)

  a <- .idsrv_medir(con, list(Nombre = tabla))
  DBI::dbExecute(con, paste0("INSERT INTO ", .idsrv_citar(con, tabla), " VALUES (4)"))
  b <- .idsrv_medir(con, list(ConEsquema = con_esquema))
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), info = motor)
  expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L),
                   info = motor)
}

.idsrv_esperada <- function(con, base, esquema, tabla) {
  as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(catalog = base, schema = esquema, table = tabla)
  ))
}

test_that("PostgreSQL: la tabla de cada lectura es la que resuelve el motor", {
  for (puerto in c(Sys.getenv("LUPA_PG_PORT", "55432"),
                   Sys.getenv("LUPA_PG93_PORT", "55493"))) {
    con <- .idsrv_conectar_pg(puerto)
    actual <- DBI::dbGetQuery(con, "SELECT current_database() AS b, current_schema() AS e")
    .idsrv_probar(con, paste("postgresql", puerto), as.character(actual$b),
                  as.character(actual$e), mayusculas = FALSE)
    DBI::dbDisconnect(con)
  }
})

test_that("PostgreSQL: una homonima en otro esquema es otra tabla", {
  con <- .idsrv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  esquema <- "lupa_mdi_otro"
  tabla <- "lupa_mdi_homonima"
  limpiar <- function() {
    try(DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", esquema, " CASCADE")),
        silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  }
  limpiar()
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", esquema))
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1), (2), (3)"))
  DBI::dbExecute(con, paste0("CREATE TABLE ", esquema, ".", tabla, " (x INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", esquema, ".", tabla,
                             " VALUES (1), (2), (3), (4), (5)"))
  # `Id(catalog = "s", table = "t")` se cita `"s"."t"` y el motor lee la del
  # esquema `s`: es la misma que `Id(schema = "s")`, no la visible.
  m <- .idsrv_medir(con, list(
    Propia = tabla, Otra = DBI::Id(schema = esquema, table = tabla),
    Catalogo = DBI::Id(catalog = esquema, table = tabla)
  ))
  expect_false(anyNA(attr(m, "lecturas", exact = TRUE)$tabla))
  expect_false(identical(.idsrv_tabla(m, "Propia"), .idsrv_tabla(m, "Otra")))
  expect_identical(.idsrv_tabla(m, "Catalogo"), .idsrv_tabla(m, "Otra"))
  expect_identical(m$n_filas_leidas, c(3, 5, 5))
  expect_identical(m$estado_cuentas, c("cierran", "cierran", "cierran"))
})

test_that("SQL Server: la tabla de cada lectura es la que resuelve el motor", {
  con <- .idsrv_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  actual <- DBI::dbGetQuery(con, "SELECT DB_NAME() AS b, SCHEMA_NAME() AS e")
  # Si la base no distingue mayusculas, el nombre en mayusculas es la misma
  # tabla, y el catalogo la nombra como la guarda.
  sin_caja <- DBI::dbGetQuery(
    con, "SELECT CASE WHEN 'a' = 'A' THEN 1 ELSE 0 END AS s"
  )$s
  .idsrv_probar(con, "sqlserver", as.character(actual$b), as.character(actual$e),
                mayusculas = identical(as.integer(sin_caja), 1L))
})

# La identidad lleva la base: la misma `public.t` en dos bases del mismo
# servidor son dos tablas, y unidas no se comparan sus filas. Sin la base eran
# una sola, y la union publicaba que la tabla cambio entre lecturas.
.idsrv_dos_bases <- function(puerto) {
  con <- .idsrv_conectar_pg(puerto)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  otra_base <- "lupa_mdi_otra"
  tabla <- "lupa_mdi_dos_bases"
  limpiar <- function() {
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbExecute(con, paste0("DROP DATABASE IF EXISTS ", otra_base)), silent = TRUE)
  }
  limpiar()
  creada <- tryCatch({
    DBI::dbExecute(con, paste0("CREATE DATABASE ", otra_base))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creada, "La credencial no puede crear una base.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  otra <- .idsrv_conectar_pg(puerto, otra_base)
  on.exit(DBI::dbDisconnect(otra), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (1), (2), (NULL)"))
  DBI::dbExecute(otra, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(otra, paste0("INSERT INTO ", tabla, " VALUES (1), (2), (3), (4), (5)"))
  a <- .idsrv_medir(con, list(Actual = tabla))
  b <- .idsrv_medir(otra, list(Otra = tabla))
  expect_false(identical(.idsrv_tabla(a, "Actual"), .idsrv_tabla(b, "Otra")),
               info = puerto)
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$n_filas_leidas, c(3, 5), info = puerto)
  expect_identical(u$estado_cuentas, c("cierran", "cierran"), info = puerto)
}

test_that("PostgreSQL: la misma tabla en dos bases del servidor son dos tablas", {
  for (puerto in c(Sys.getenv("LUPA_PG_PORT", "55432"),
                   Sys.getenv("LUPA_PG93_PORT", "55493"))) {
    .idsrv_dos_bases(puerto)
  }
})

# La identidad lleva el servidor: la misma `base.esquema.tabla` en PostgreSQL 16
# y en 9.3 son dos tablas, y unidas no se comparan (antes salian `no_cierran`,
# como si la tabla hubiera cambiado entre lecturas). La misma tabla por otra
# conexion al mismo servidor sigue siendo una.
test_that("PostgreSQL: la misma tabla en dos servidores son dos tablas", {
  uno <- .idsrv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(uno), add = TRUE)
  otro <- .idsrv_conectar_pg(Sys.getenv("LUPA_PG93_PORT", "55493"))
  on.exit(DBI::dbDisconnect(otro), add = TRUE)
  tabla <- "lupa_mdi_dos_servidores"
  limpiar <- function() {
    for (con in list(uno, otro)) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    }
  }
  limpiar()
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbExecute(uno, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(uno, paste0("INSERT INTO ", tabla, " VALUES (1), (2), (3)"))
  DBI::dbExecute(otro, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(otro, paste0("INSERT INTO ", tabla, " VALUES (1), (2), (3), (4), (5)"))
  a <- .idsrv_medir(uno, list(Uno = tabla))
  b <- .idsrv_medir(otro, list(Otro = tabla))
  expect_false(identical(.idsrv_tabla(a, "Uno"), .idsrv_tabla(b, "Otro")))
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  tres <- .idsrv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(tres), add = TRUE, after = FALSE)
  c3 <- .idsrv_medir(tres, list(Tres = tabla))
  expect_identical(.idsrv_tabla(c3, "Tres"), .idsrv_tabla(a, "Uno"))
})

# SQL Server: `DBI::Id(catalog = <otra base>, schema = "dbo")` es la tabla de
# esa base. Sus columnas y sus tipos salen del catalogo de esa base, no de la
# homonima de la actual, y sin `rowversion` el contador de modificaciones es el
# de esa base: antes la medicion abortaba ("Could not find table or object ID").
test_that("SQL Server: un Id de otra base se lee y se describe con el catalogo de esa base", {
  con <- .idsrv_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  otra <- "lupa_mdi_otra"
  tabla <- "lupa_mdi_otra_base"
  limpiar <- function() {
    try(DBI::dbExecute(con, paste0("IF OBJECT_ID(N'dbo.", tabla, "') IS NOT NULL DROP TABLE dbo.",
                                   tabla)), silent = TRUE)
    try(DBI::dbExecute(con, paste0("IF DB_ID(N'", otra, "') IS NOT NULL DROP DATABASE ", otra)),
        silent = TRUE)
  }
  limpiar()
  creada <- tryCatch({
    DBI::dbExecute(con, paste0("CREATE DATABASE ", otra))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creada, "La credencial no puede crear una base.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0("CREATE TABLE dbo.", tabla, " (x INT, b INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO dbo.", tabla, " VALUES (1, 1), (NULL, 2), (3, 3)"))
  DBI::dbExecute(con, paste0("CREATE TABLE ", otra, ".dbo.", tabla, " (x INT, b VARCHAR(10))"))
  DBI::dbExecute(con, paste0("INSERT INTO ", otra, ".dbo.", tabla,
                             " VALUES (1, 'x'), (2, 'x'), (NULL, 'y'), (4, 'x'), (5, NULL)"))
  id <- DBI::Id(catalog = otra, schema = "dbo", table = tabla)
  m <- .idsrv_medir(con, list(Otra = id))
  expect_identical(m$n_filas_leidas, 5)
  expect_true(endsWith(.idsrv_tabla(m, "Otra"), paste0(".", .idsrv_citar(con, id))))
  sesion <- .verificar_sesion_medicion_dbi(con, "sqlserver")
  esquema <- .esquema_medicion_dbi(con, id, "sqlserver", sesion = sesion)
  expect_identical(unname(esquema$tipos[["b"]]), "VARCHAR")
  actual <- .idsrv_medir(con, list(Actual = tabla))
  u <- suppressWarnings(rbind(actual, m))
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
})

# La huella del servidor sale de lo que la conexion declara, sin SQL: una
# credencial a la que se le revoco `EXECUTE` sobre las funciones que dicen la
# direccion o el inicio del servidor mide igual. Antes la consulta de catalogo
# las llamaba, fallaba, y la tabla no se media con el aviso de que no estaba.
# Y si la consulta de catalogo falla por otra razon, el motivo dice el error del
# motor, no que la tabla no esta. Los permisos se restauran al salir.
.idsrv_rol_comun <- function(puerto) {
  duenio <- .idsrv_conectar_pg(puerto)
  on.exit(DBI::dbDisconnect(duenio), add = TRUE)
  rol <- "lupa_mdi_lector"
  tabla <- "lupa_mdi_permisos"
  clave <- paste(sample(c(letters, LETTERS, 0:9), 24, replace = TRUE), collapse = "")
  funciones <- c("pg_postmaster_start_time()", "inet_server_addr()", "inet_server_port()",
                 "pg_catalog.pg_table_is_visible(oid)")
  limpiar <- function() {
    for (f in funciones) {
      try(DBI::dbExecute(duenio, paste0("GRANT EXECUTE ON FUNCTION ", f, " TO PUBLIC")),
          silent = TRUE)
    }
    try(DBI::dbExecute(duenio, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbExecute(duenio, paste0("REVOKE USAGE ON SCHEMA public FROM ", rol)), silent = TRUE)
    try(DBI::dbExecute(duenio, paste0("DROP ROLE IF EXISTS ", rol)), silent = TRUE)
  }
  limpiar()
  creado <- tryCatch({
    DBI::dbExecute(duenio, paste0("CREATE ROLE ", rol, " LOGIN PASSWORD '", clave, "'"))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creado, "La credencial no puede crear un rol.")
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbExecute(duenio, paste0("GRANT USAGE ON SCHEMA public TO ", rol))
  DBI::dbExecute(duenio, paste0("CREATE TABLE ", tabla, " (x INT)"))
  DBI::dbExecute(duenio, paste0("INSERT INTO ", tabla, " VALUES (1), (NULL), (3)"))
  DBI::dbExecute(duenio, paste0("GRANT SELECT ON ", tabla, " TO ", rol))
  for (f in funciones[1:3]) {
    DBI::dbExecute(duenio, paste0("REVOKE EXECUTE ON FUNCTION ", f, " FROM PUBLIC"))
  }
  lector <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(puerto), user = rol, password = clave,
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(lector), "El rol nuevo no puede conectarse.")
  on.exit(DBI::dbDisconnect(lector), add = TRUE, after = FALSE)
  m <- .idsrv_medir(lector, list(Lector = tabla))
  expect_identical(m$n_filas_leidas, 3, info = puerto)
  expect_identical(m$numerador, 2, info = puerto)
  expect_false(anyNA(attr(m, "lecturas", exact = TRUE)$tabla), info = puerto)
  # Ahora falla la consulta de catalogo: el motivo nombra la funcion revocada.
  DBI::dbExecute(duenio, paste0("REVOKE EXECUTE ON FUNCTION ", funciones[[4L]], " FROM PUBLIC"))
  sin <- .idsrv_medir(lector, list(Lector = tabla))
  expect_identical(nrow(sin), 0L, info = puerto)
  motivo <- attr(sin, "cobertura_metricas", exact = TRUE)$motivo
  expect_true(any(grepl("pg_table_is_visible", motivo, fixed = TRUE, useBytes = TRUE)),
              info = puerto)
}

test_that("PostgreSQL: sin permiso sobre las funciones del servidor la tabla se mide igual", {
  for (puerto in c(Sys.getenv("LUPA_PG_PORT", "55432"),
                   Sys.getenv("LUPA_PG93_PORT", "55493"))) {
    .idsrv_rol_comun(puerto)
  }
})
