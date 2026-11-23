# DuckDB compara los nombres de tabla, de esquema y de catalogo sin distinguir
# mayusculas solo en ASCII: dos nombres que difieren en la caja de una letra no
# ASCII son dos objetos. La tabla de cada lectura se busca con esa misma
# comparacion: con la de Unicode, las dos tablas eran una, y un nombre escrito
# en otra caja se leia de una y se registraba como la otra.

.plg_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.plg_con <- function() {
  skip_if_not_installed("duckdb")
  DBI::dbConnect(duckdb::duckdb())
}

.plg_citar <- function(con, ...) {
  as.character(DBI::dbQuoteIdentifier(con, DBI::Id(...)))
}

.plg_llenar <- function(con, tabla_sql, filas) {
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla_sql, " (x INTEGER)"))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla_sql, " VALUES ",
    paste0("(", seq_len(filas), ")", collapse = ", ")
  ))
}

.plg_medir <- function(con, tablas) {
  nucleo <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nucleo$NoNulo, paste0("N", e)), e, "x")
  })
  medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
            id_medicion = "j", fecha = .plg_fecha)
}

.plg_tabla <- function(m, entidad) {
  l <- attr(m, "lecturas", exact = TRUE)
  unique(l$tabla[l$entidad == entidad])
}

# `Ano` con la enie minuscula y `ANO` con la mayuscula, y lo mismo escrito con
# la primera letra en otra caja.
.plg_nombres <- function() {
  enie <- intToUtf8(241L)
  ENIE <- intToUtf8(209L)
  list(
    minuscula = paste0("A", enie, "o"), mayuscula = paste0("A", ENIE, "O"),
    pedida_minuscula = paste0("a", enie, "O"),
    pedida_mayuscula = paste0("a", ENIE, "o")
  )
}

test_that("en DuckDB dos tablas que difieren en la caja de una letra no ASCII son dos tablas", {
  skip_on_cran()
  con <- .plg_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  n <- .plg_nombres()
  .plg_llenar(con, .plg_citar(con, table = n$minuscula), 4L)
  .plg_llenar(con, .plg_citar(con, table = n$mayuscula), 6L)
  m <- .plg_medir(con, list(
    Minuscula = n$minuscula, Mayuscula = n$mayuscula,
    PideMinuscula = n$pedida_minuscula, PideMayuscula = n$pedida_mayuscula
  ))
  # Lo que leyo cada una y la tabla que registro son la misma.
  expect_identical(m$n_filas_leidas, c(4, 6, 4, 6))
  expect_identical(m$estado_cuentas, rep("cierran", 4L))
  expect_identical(.plg_tabla(m, "PideMinuscula"), .plg_tabla(m, "Minuscula"))
  expect_identical(.plg_tabla(m, "PideMayuscula"), .plg_tabla(m, "Mayuscula"))
  expect_false(identical(.plg_tabla(m, "Minuscula"), .plg_tabla(m, "Mayuscula")))
})

test_that("en DuckDB dos esquemas que difieren en la caja de una letra no ASCII son dos esquemas", {
  skip_on_cran()
  con <- .plg_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  n <- .plg_nombres()
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", .plg_citar(con, schema = n$minuscula)))
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", .plg_citar(con, schema = n$mayuscula)))
  .plg_llenar(con, .plg_citar(con, schema = n$minuscula, table = "t"), 2L)
  .plg_llenar(con, .plg_citar(con, schema = n$mayuscula, table = "t"), 3L)
  m <- .plg_medir(con, list(
    Minuscula = DBI::Id(schema = n$minuscula, table = "t"),
    Mayuscula = DBI::Id(schema = n$mayuscula, table = "t"),
    PideMinuscula = DBI::Id(schema = n$pedida_minuscula, table = "T"),
    PideMayuscula = DBI::Id(schema = n$pedida_mayuscula, table = "T"),
    # Y con el catalogo escrito.
    BaseMinuscula = DBI::Id(catalog = "memory", schema = n$pedida_minuscula, table = "t"),
    BaseMayuscula = DBI::Id(catalog = "memory", schema = n$pedida_mayuscula, table = "t")
  ))
  expect_identical(m$n_filas_leidas, c(2, 3, 2, 3, 2, 3))
  expect_identical(m$estado_cuentas, rep("cierran", 6L))
  expect_identical(.plg_tabla(m, "PideMinuscula"), .plg_tabla(m, "Minuscula"))
  expect_identical(.plg_tabla(m, "PideMayuscula"), .plg_tabla(m, "Mayuscula"))
  expect_identical(.plg_tabla(m, "BaseMinuscula"), .plg_tabla(m, "Minuscula"))
  expect_identical(.plg_tabla(m, "BaseMayuscula"), .plg_tabla(m, "Mayuscula"))
  expect_false(identical(.plg_tabla(m, "Minuscula"), .plg_tabla(m, "Mayuscula")))
})

test_that("en DuckDB un esquema en otra caja no ASCII no tapa a la base adjunta que si se llama asi", {
  skip_on_cran()
  enie <- intToUtf8(241L)
  ENIE <- intToUtf8(209L)
  # Un esquema con la enie mayuscula en la base de la sesion y una base adjunta
  # con la minuscula: el nombre con la minuscula no es de ese esquema, es de la
  # base. Con un nombre anterior y otro posterior al de la base de la sesion,
  # para que no dependa del orden en que el catalogo devuelve sus filas.
  for (letras in list(c("A", "a"), c("Z", "z"))) {
    esquema <- paste0(letras[[1L]], ENIE, letras[[1L]])
    base <- paste0(letras[[1L]], enie, letras[[2L]])
    pedida <- paste0(letras[[2L]], enie, letras[[1L]])
    con <- .plg_con()
    DBI::dbExecute(con, paste0("CREATE SCHEMA ", .plg_citar(con, schema = esquema)))
    .plg_llenar(con, .plg_citar(con, schema = esquema, table = "t"), 3L)
    DBI::dbExecute(con, paste0("ATTACH ':memory:' AS ", .plg_citar(con, catalog = base)))
    .plg_llenar(con, .plg_citar(con, catalog = base, schema = "main", table = "t"), 5L)
    m <- .plg_medir(con, list(
      Pedida = DBI::Id(schema = pedida, table = "t"),
      Esquema = DBI::Id(catalog = "memory", schema = esquema, table = "t"),
      Base = DBI::Id(catalog = base, schema = "main", table = "t")
    ))
    DBI::dbDisconnect(con, shutdown = TRUE)
    expect_identical(m$n_filas_leidas, c(5, 3, 5), label = letras[[1L]])
    expect_identical(m$estado_cuentas, rep("cierran", 3L), label = letras[[1L]])
    expect_identical(.plg_tabla(m, "Pedida"), .plg_tabla(m, "Base"),
                     label = letras[[1L]])
    expect_false(identical(.plg_tabla(m, "Pedida"), .plg_tabla(m, "Esquema")),
                 label = letras[[1L]])
  }
})

test_that("en DuckDB dos bases adjuntas que difieren en la caja de una letra no ASCII son dos bases", {
  skip_on_cran()
  con <- .plg_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  n <- .plg_nombres()
  for (base in c(n$minuscula, n$mayuscula)) {
    DBI::dbExecute(con, paste0("ATTACH ':memory:' AS ", .plg_citar(con, catalog = base)))
  }
  .plg_llenar(con, .plg_citar(con, catalog = n$minuscula, schema = "main", table = "t"), 2L)
  .plg_llenar(con, .plg_citar(con, catalog = n$mayuscula, schema = "main", table = "t"), 3L)
  m <- .plg_medir(con, list(
    Minuscula = DBI::Id(catalog = n$minuscula, schema = "main", table = "t"),
    Mayuscula = DBI::Id(catalog = n$mayuscula, schema = "main", table = "t"),
    PideMinuscula = DBI::Id(catalog = n$pedida_minuscula, schema = "MAIN", table = "t"),
    PideMayuscula = DBI::Id(catalog = n$pedida_mayuscula, schema = "MAIN", table = "t")
  ))
  expect_identical(m$n_filas_leidas, c(2, 3, 2, 3))
  expect_identical(m$estado_cuentas, rep("cierran", 4L))
  expect_identical(.plg_tabla(m, "PideMinuscula"), .plg_tabla(m, "Minuscula"))
  expect_identical(.plg_tabla(m, "PideMayuscula"), .plg_tabla(m, "Mayuscula"))
  expect_false(identical(.plg_tabla(m, "Minuscula"), .plg_tabla(m, "Mayuscula")))
})

test_that("en DuckDB el catalogo MEMORY es memory", {
  skip_on_cran()
  con <- .plg_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .plg_llenar(con, "ventas", 4L)
  a <- .plg_medir(con, list(Minuscula = "ventas"))
  DBI::dbExecute(con, "INSERT INTO ventas VALUES (5)")
  b <- .plg_medir(con, list(
    Mayuscula = DBI::Id(catalog = "MEMORY", schema = "main", table = "ventas")
  ))
  u <- suppressWarnings(rbind(a, b))
  expect_false(anyNA(attr(u, "lecturas", exact = TRUE)$tabla))
  expect_identical(.plg_tabla(u, "Mayuscula"), .plg_tabla(u, "Minuscula"))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("el control: en DuckDB un nombre ASCII en otra caja es la misma tabla", {
  skip_on_cran()
  con <- .plg_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .plg_llenar(con, "ventas", 4L)
  m <- .plg_medir(con, list(Minuscula = "ventas", Mayuscula = "VENTAS"))
  expect_identical(m$n_filas_leidas, c(4, 4))
  expect_identical(.plg_tabla(m, "Mayuscula"), .plg_tabla(m, "Minuscula"))
})
