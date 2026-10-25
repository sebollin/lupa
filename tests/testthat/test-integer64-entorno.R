# La guarda de `integer64`. Sin el metodo `mtfrm.integer64` -R anterior a 4.2.0, o
# bit64 anterior a 4.6.0- `match()` de base compara los bits del `double`
# subyacente, en el que todos los negativos de -2^52 a -1 son `NaN`, y los iguala
# entre si. Medido el 2026-10-25 en R 4.1.0 con bit64 4.0.5: `perfilar()` conto 4
# distintos en una columna de 7, y por bloques la mediana de
# -3000000001..-3000000005 salio -3000000001, sin ningun aviso.
#
# El entorno sin el metodo se simula reemplazando la unica pregunta que hace la
# guarda, `.integer64_con_match_confiable()`.

.datos_integer64_entorno <- function() {
  data.frame(a = 1:7, x = bit64::as.integer64(c(-1, -1, -2, -3, 5, 7, 100)))
}

test_that("con bit64 4.6.0 el match() de base distingue los negativos, que es lo que la guarda pregunta", {
  skip_if_not_installed("bit64", "4.6.0")
  expect_true(.integer64_con_match_confiable())
  x <- bit64::as.integer64(c(-1, -1, -2, -3, 5))
  expect_identical(base::match(x, unique(x)), c(1L, 1L, 2L, 3L, 4L))
})

test_that("sin el metodo, perfilar() y analizar() se niegan con una condicion propia", {
  skip_if_not_installed("bit64", "4.6.0")
  local_mocked_bindings(.integer64_con_match_confiable = function() FALSE)
  d <- .datos_integer64_entorno()
  expect_error(suppressMessages(perfilar(d)), class = "lupa_error_integer64")
  expect_error(suppressMessages(analizar(d)), class = "lupa_error_integer64")
  e <- tryCatch(suppressMessages(perfilar(d)), error = identity)
  expect_false(inherits(e, "lupa_error_dbi"))
  # El mensaje nombra la columna, las versiones y el remedio.
  expect_match(conditionMessage(e), "(`x`)", fixed = TRUE)
  expect_match(conditionMessage(e), "R 4.2.0 y bit64 4.6.0", fixed = TRUE)
  expect_match(conditionMessage(e), "as.numeric()", fixed = TRUE)
  # Control: la misma tabla en `double` corre; la guarda solo mira la clase.
  d$x <- c(-1, -1, -2, -3, 5, 7, 100)
  p <- suppressMessages(perfilar(d))
  expect_identical(p$columnas$n_distintos[p$columnas$columna == "x"], 6L)
})

test_that("sin el metodo, la lectura por bloques se niega con la clase de la via DBI", {
  skip_if_not_installed("bit64", "4.6.0")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.integer64_con_match_confiable = function() FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # RSQLite decide la clase por los valores: `x` llega `integer64` porque no
  # entra en 32 bits, y el prototipo de cero filas la ve `integer`.
  DBI::dbExecute(con, "CREATE TABLE t (n INTEGER, x INTEGER)")
  DBI::dbExecute(con, paste(
    "INSERT INTO t VALUES (1, -3000000001), (2, -3000000002), (3, -3000000003)"
  ))
  e <- expect_error(
    suppressWarnings(perfilar_dbi(con, "t", bloque_filas = 2L,
                                  bloque_muestra = "solo_agregados")),
    class = "lupa_error_integer64"
  )
  expect_s3_class(e, "lupa_error_dbi")
  # Sin bloques los agregados los calcula el motor y se devuelven; la muestra,
  # que perfilaria R, no, y el aviso dice por que.
  avisos <- character()
  r <- withCallingHandlers(perfilar_dbi(con, "t"), warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_null(r$perfil_muestra)
  expect_true(any(grepl("integer64", avisos, fixed = TRUE)))
  columnas <- r$resumen_tabla$columnas
  expect_identical(as.integer(columnas$n_distintos[columnas$columna == "x"]), 3L)
  # Control: la tabla sin `integer64`, por bloques, corre.
  DBI::dbExecute(con, "CREATE TABLE s (n INTEGER, y REAL)")
  DBI::dbExecute(con, "INSERT INTO s VALUES (1, -1.5), (2, 2.5), (3, 4)")
  expect_s3_class(
    suppressWarnings(perfilar_dbi(con, "s", bloque_filas = 2L,
                                  bloque_muestra = "solo_agregados")),
    "perfil_dbi"
  )
})

# El metodo se registra al cargar bit64, y un `integer64` puede llegar sin bit64
# cargado -de `readRDS()`-: la primera version de la guarda se negaba ahi con
# bit64 4.8 instalado. Hace falta otro proceso, porque en este bit64 ya esta
# cargado. El hijo carga `lupa` del mismo lugar que este: con `load_all()` desde
# el arbol, si no instalado; un `library()` a secas podria medir otra version.
test_that("un integer64 que llega sin bit64 cargado no dispara la guarda: se carga antes de mirar", {
  skip_on_cran()
  skip_if_not_installed("bit64", "4.6.0")
  ruta <- find.package("lupa")
  desde_fuente <- file.exists(file.path(ruta, "R", "utils.R"))
  if (desde_fuente) skip_if_not_installed("pkgload")
  carga <- if (desde_fuente) {
    sprintf('suppressMessages(pkgload::load_all("%s", quiet = TRUE))', ruta)
  } else {
    sprintf('suppressMessages(library(lupa, lib.loc = "%s"))', dirname(ruta))
  }
  guion <- tempfile(fileext = ".R")
  on.exit(unlink(guion), add = TRUE)
  writeLines(c(
    carga,
    "i64 <- function(v) vapply(v, function(k) {",
    "  b <- if (k >= 0) c(as.raw(k), as.raw(rep(0, 7))) else",
    "    c(as.raw(256 + k), as.raw(rep(255, 7)))",
    "  readBin(b, 'double', size = 8, endian = 'little')",
    "}, 0)",
    "d <- data.frame(a = 1:6)",
    "d$codigo <- i64(c(-1, -1, -2, -3, 5, 7))",
    "class(d$codigo) <- 'integer64'",
    "cat('cargado_antes', isNamespaceLoaded('bit64'), '\\n')",
    "p <- perfilar(d, analizar_dependencias = FALSE, proteger_datos_personales = FALSE)",
    "cat('distintos', p$columnas$n_distintos[p$columnas$columna == 'codigo'], '\\n')"
  ), guion)
  salida <- system2(file.path(R.home("bin"), "Rscript"), shQuote(guion),
                    stdout = TRUE, stderr = TRUE)
  # La premisa: el hijo arranca sin bit64 cargado. Si no, no mide nada.
  expect_true(any(grepl("cargado_antes FALSE", salida, fixed = TRUE)), info = paste(salida, collapse = "\n"))
  # Cinco distintos a mano: -1, -2, -3, 5 y 7.
  expect_true(any(grepl("distintos 5", salida, fixed = TRUE)), info = paste(salida, collapse = "\n"))
})

# Recorre todas las exportadas con la tabla como primer argumento: ninguna puede
# correr sin pasar por la guarda. Asi se encontraron las cinco que solo entraban
# por `.tabla_base()`. Una funcion que falla por otro motivo -le falta un
# argumento, no recibe tablas- no cuenta: no llego a usar los valores.
test_that("ninguna funcion exportada usa una columna integer64 sin pasar por la guarda", {
  skip_on_cran()
  skip_if_not_installed("bit64", "4.6.0")
  local_mocked_bindings(.integer64_con_match_confiable = function() FALSE)
  d <- .datos_integer64_entorno()
  exportadas <- sort(getNamespaceExports("lupa"))
  resultado <- vapply(exportadas, function(nombre) {
    f <- get(nombre, envir = asNamespace("lupa"))
    if (!is.function(f)) return("no es funcion")
    tryCatch(
      withCallingHandlers(
        { utils::capture.output(suppressMessages(f(d))); "corre" },
        warning = function(w) invokeRestart("muffleWarning")
      ),
      lupa_error_integer64 = function(e) "guarda",
      error = function(e) "otro error"
    )
  }, character(1L))
  expect_identical(names(resultado)[resultado == "corre"], character())
  # Y el recorrido llega: las de entrada principal estan entre las que se niegan.
  expect_true(all(c("perfilar", "analizar", "clasificar_variables",
                    "detectar_asociaciones") %in% names(resultado)[resultado == "guarda"]))
})
