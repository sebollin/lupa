# Funciones de base que comparan u ordenan un `integer64` por debajo, fuera del
# `match()` y el `%in%` del paquete.
#
# Medido en R 4.2.0 y 4.6.2, con bit64 4.0.5, 4.6.0.1 y 4.8.2 (cargado, no
# adjunto, como dentro del paquete):
#
# - `order()` ordena por los bits del `double` subyacente y manda los negativos
#   al final; `factor(x)` saca sus niveles en ese orden, y `table(x)` sus
#   nombres. Mal con cualquier version.
# - `setdiff()`, `intersect()`, `union()` e `is.element()` le quitan la clase con
#   `as.vector()` y comparan esos bits, en los que todos los negativos de -2^52
#   a -1 son `NaN`: mal en R 4.2.0 con cualquier bit64, bien en R 4.6.2.
# - `merge()` por una clave `integer64` y `factor()` con niveles `integer64`
#   llaman al `match()` de base con el objeto intacto: bien con bit64 4.6.0 o
#   posterior, mal con una anterior.
#
# Hoy ninguna llamada del paquete les pasa un `integer64`. Esta prueba cuida que
# un sitio nuevo no entre callado: traza esas funciones mientras las publicas
# perfilan una tabla con columnas `integer64`, y falla si el paquete le pasa una
# a alguna, directamente o a traves de otras de base. Para `merge()`, `factor()`
# con niveles y cualquier otro camino al `match()` de base se traza `mtfrm()`:
# el `match()` interno lo llama desde C sobre todo objeto con clase, y `match()`
# mismo no se puede trazar porque el compilador lo inlinea. Mira el tipo de lo
# que llega, no el texto del codigo: las mas de cien llamadas a `setdiff()` e
# `intersect()` sobre nombres de columnas no cuentan.
#
# `rank()` no esta: sobre `integer64` da lo mismo que `bit64::rank.integer64()`,
# tambien por encima de 2^53 y con empates. `sort()` tampoco: despacha al metodo
# de bit64. Las dos, ademas, ya pasan por `.orden_por_bytes()` (otra prueba
# recorre el espacio de nombres).

.trazadas_integer64 <- list(
  mtfrm = quote(inherits(x, "integer64")),
  setdiff = quote(inherits(x, "integer64") || inherits(y, "integer64")),
  intersect = quote(inherits(x, "integer64") || inherits(y, "integer64")),
  union = quote(inherits(x, "integer64") || inherits(y, "integer64")),
  is.element = quote(inherits(el, "integer64") || inherits(table, "integer64")),
  order = quote(any(vapply(list(...), inherits, logical(1L), "integer64"))),
  factor = quote(inherits(x, "integer64") ||
                   (!missing(levels) && inherits(levels, "integer64")))
)

# Corre `codigo` con las funciones de arriba trazadas y devuelve, por cada
# llamada que recibio un `integer64` del paquete, "funcion <- sitio". El sitio es
# la primera funcion de la pila que no es de R base: si es del paquete, la
# llamada cuenta; si es de otro paquete (DBI, duckdb), no es de lupa. Las
# funciones de este archivo tienen el espacio de nombres del paquete por
# encima, asi que cuentan como del paquete: es lo que usa el control.
.llamadas_integer64_a_base <- function(codigo) {
  ns <- asNamespace("lupa")
  de_r <- lapply(c("base", "stats", "utils", "methods"), asNamespace)
  registro <- new.env(parent = emptyenv())
  registro$filas <- character()
  anotar <- function(funcion, marco) {
    marcos <- sys.frames()
    i <- which(vapply(marcos, identical, logical(1L), marco))[1L]
    while (!is.na(i) && i > 1L) {
      i <- i - 1L
      f <- sys.function(i)
      if (is.primitive(f)) next
      arriba <- topenv(environment(f))
      if (any(vapply(de_r, identical, logical(1L), arriba))) next
      if (identical(arriba, ns)) {
        quien <- paste(deparse(sys.call(i)[[1L]]), collapse = "")
        registro$filas <- c(registro$filas, paste(funcion, "<-", quien))
      }
      return(invisible())
    }
  }
  for (nombre in names(.trazadas_integer64)) {
    suppressMessages(trace(
      nombre, where = asNamespace("base"), print = FALSE,
      tracer = bquote(if (.(.trazadas_integer64[[nombre]])) {
        .(anotar)(.(nombre), environment())
      })
    ))
  }
  on.exit(for (nombre in names(.trazadas_integer64)) {
    suppressMessages(untrace(nombre, where = asNamespace("base")))
  }, add = TRUE)
  codigo
  registro$filas
}

# Negativos repetidos, ceros, `NA`, valores por encima de 2^53 -desde texto: un
# literal `double` ya redondeo-, una casi clave negativa, extremos, y `zona`
# que depende de `cod`, para que corran las dependencias y los hallazgos.
.datos_integer64_base <- function(n = 160L, semilla = 11L) {
  set.seed(semilla)
  d <- data.frame(id = seq_len(n))
  d$neg <- bit64::as.integer64(as.character(
    sample(c(-5:-1, 0L, 1:3, NA), n, TRUE)
  ))
  k <- sample(1:12, n, TRUE)
  d$cod <- bit64::as.integer64(as.character(-3000000000 - k))
  d$big <- bit64::as.integer64(paste0(
    sample(c("", "-"), n, TRUE), "9007199254740", sample(990:995, n, TRUE)
  ))
  d$ide <- bit64::as.integer64(as.character(-(seq_len(n)) * 7L))
  d$ide[2L] <- d$ide[1L]
  mezcla <- sample(c(-1000L, -1L, 0L, 0L, 1L, 2L, 7L, NA), n, TRUE)
  mezcla[c(5L, 9L)] <- c(-90000L, 85000L)
  d$mix <- bit64::as.integer64(as.character(mezcla))
  d$x <- round(stats::rnorm(n, 50, 10), 2)
  d$s <- sample(c("a", "b", "c"), n, TRUE)
  d$zona <- c("norte", "sur", "este")[k %% 3L + 1L]
  d
}

test_that("la traza ve un integer64 que el paquete pasa a cada una, tambien compilado y a traves de base", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  # Funciones con el espacio de nombres del paquete por encima, como las de
  # `R/`: cada una pasa un `integer64` por una de las vias.
  a <- bit64::as.integer64(c("-1", "-2", "5"))
  sitios <- list(
    sitio_match = function() base::match(a, a),
    sitio_merge = function() merge(data.frame(k = a), data.frame(k = a[1L])),
    sitio_factor_niveles = function() factor(c("5", "-1"), levels = a),
    sitio_factor = function() factor(a),
    sitio_table = function() table(a),
    sitio_setdiff = function() setdiff(a, a[1L]),
    sitio_intersect = function() intersect(a, a[1L]),
    sitio_union = function() union(a, a),
    sitio_is_element = function() is.element(a, a),
    sitio_order = function() order(a),
    sitio_order_texto_primero = function() order(c("x", "y", "z"), a)
  )
  sitios <- lapply(sitios, function(f) {
    environment(f) <- list2env(list(a = a), parent = asNamespace("lupa"))
    f
  })
  esperado <- c(
    sitio_match = "mtfrm", sitio_merge = "mtfrm",
    sitio_factor_niveles = "mtfrm", sitio_factor = "factor",
    sitio_table = "factor", sitio_setdiff = "setdiff",
    sitio_intersect = "intersect", sitio_union = "union",
    sitio_is_element = "is.element", sitio_order = "order",
    sitio_order_texto_primero = "order"
  )
  for (compilar in c(FALSE, TRUE)) {
    for (nombre in names(sitios)) {
      f <- sitios[[nombre]]
      # El paquete instalado esta compilado a bytes, y el compilador inlinea
      # algunas de base (`match()`): la traza tiene que verlas igual.
      if (compilar) f <- compiler::cmpfun(f)
      vistas <- .llamadas_integer64_a_base(f())
      expect_true(paste(esperado[[nombre]], "<- f") %in% vistas,
                  info = paste(nombre, if (compilar) "compilada"))
    }
  }
  # Y no cuenta lo que no es del paquete: la misma llamada desde una funcion de
  # otro espacio de nombres, ni lo que no es `integer64`.
  ajena <- function() setdiff(a, a[1L])
  environment(ajena) <- list2env(list(a = a), parent = asNamespace("DBI"))
  expect_identical(.llamadas_integer64_a_base(ajena()), character())
  texto <- sitios$sitio_setdiff
  environment(texto) <- list2env(list(a = as.character(a)),
                                 parent = asNamespace("lupa"))
  expect_identical(.llamadas_integer64_a_base(texto()), character())
  # Al salir, las funciones de base quedan como estaban.
  expect_false(inherits(get("setdiff", envir = asNamespace("base")),
                        "functionWithTrace"))
  expect_false(inherits(get("mtfrm", envir = asNamespace("base")),
                        "functionWithTrace"))
})

test_that("las funciones publicas no pasan un integer64 a funciones de base que lo comparan u ordenan", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  d <- .datos_integer64_base()
  anterior <- d
  anterior$neg[1:40] <- bit64::as.integer64("-9")
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  resultados <- list()
  vistas <- .llamadas_integer64_a_base(suppressWarnings(suppressMessages({
    p <- perfilar(d, fecha = fecha)
    resultados$perfil <- p
    resultados$protegido <- perfilar(d, fecha = fecha,
                                     proteger_datos_personales = TRUE,
                                     max_filas_hallazgo = Inf)
    resultados$hallazgos <- hallazgos(p)
    resultados$analisis <- analizar(d, fecha = fecha, id_medicion = "m1")
    resultados$comparacion <- comparar_perfiles(
      perfilar(anterior, fecha = fecha - 86400), p
    )
    resultados$plan <- planificar_limpieza(p, datos = d)
    resultados$aplicado <- aplicar(resultados$plan, d)
    resultados$por_grupo <- perfilar_por(d, por = "neg")
    con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "t", d)
    resultados$dbi <- perfilar_dbi(con, "t")
    resultados$dbi_bloques <- perfilar_dbi(con, "t", bloque_filas = 50L)
  })))
  # Primera mitad: el recorrido llego a las columnas `integer64`, en memoria y
  # por el motor, y midio lo que el `match()` decide. Sin esto, cero llamadas
  # pasaria tambien con un recorrido que no corrio.
  tipos <- resultados$perfil$columnas$tipo_inferido
  expect_true(all(tipos[resultados$perfil$columnas$columna %in%
                          c("neg", "cod", "big", "ide", "mix")] == "integer64"))
  expect_true("integer64" %in%
                resultados$dbi$perfil_muestra$columnas$tipo_inferido)
  dependencias <- resultados$perfil$dependencias
  expect_true("cod zona" %in% paste(dependencias$determinante,
                                    dependencias$dependiente))
  expect_identical(unique(vistas), character())
})
