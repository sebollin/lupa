# Una columna `integer64` con bit64 anterior a 4.6.0 da el mismo perfil que con
# bit64 nuevo, en vez de una negativa.
#
# Sin el metodo `mtfrm.integer64` -que bit64 registra desde la 4.6.0-, `match()`
# de base en R 4.2.0 aplica `as.vector()`: quita la clase y deja los bits del
# `double`, en el que todos los negativos de -2^52 a -1 son `NaN`, y base los
# iguala entre si. Medido en R 4.2.0 con bit64 4.0.5 y la guarda apagada: por
# bloques `n_distintos` salia 46 donde habia 50, y cambiaban las dependencias
# funcionales y los hallazgos. El paquete se negaba con `lupa_error_integer64`;
# ahora su propio `match()` y su `%in%` emparejan un `integer64` por su texto,
# que es lo que hace base con bit64 nuevo.
#
# Aca no hay un bit64 viejo instalado: su `match()` se reproduce cambiando, solo
# mientras dura cada prueba, el metodo que bit64 deja en la tabla S3 de base.
# Quitarlo no alcanza para reproducir el `match()` equivocado: en R 4.5.2 y 4.6.2
# `mtfrm.default()` ya usa `as.character()` y sin el metodo `match()` sale bien.
# Hay que reemplazarlo por el `as.vector()` de R 4.2.0.

# Corre `codigo` con el `mtfrm.integer64` de la tabla S3 de base reemplazado por
# `as.vector()` (`modo = "bits"`, el `match()` de R 4.2.0 con bit64 viejo) o
# quitado (`modo = "ausente"`, lo que ve la guarda con bit64 viejo), y lo deja
# como estaba al salir, falle o no.
.con_mtfrm_integer64 <- function(modo, codigo) {
  tabla <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
  habia <- exists("mtfrm.integer64", envir = tabla, inherits = FALSE)
  original <- if (habia) get("mtfrm.integer64", envir = tabla, inherits = FALSE)
  on.exit({
    if (habia) {
      assign("mtfrm.integer64", original, envir = tabla)
    } else if (exists("mtfrm.integer64", envir = tabla, inherits = FALSE)) {
      rm("mtfrm.integer64", envir = tabla)
    }
  }, add = TRUE)
  if (identical(modo, "bits")) {
    assign("mtfrm.integer64", function(x) as.vector(x), envir = tabla)
  } else if (habia) {
    rm("mtfrm.integer64", envir = tabla)
  }
  codigo
}

# Negativos repetidos, ceros, `NA` y valores por encima de 2^53 -desde texto: un
# literal `double` ya redondeo-, en columnas que se repiten entre si para que
# haya dependencias funcionales que medir.
.datos_bit64_anterior <- function() {
  set.seed(7)
  n <- 120L
  d <- data.frame(id = seq_len(n))
  d$neg <- bit64::as.integer64(as.character(
    sample(c(-5:-1, 0L, 1:3, NA), n, TRUE)
  ))
  k <- sample(1:12, n, TRUE)
  d$cod <- bit64::as.integer64(as.character(-3000000000 - k))
  d$big <- bit64::as.integer64(paste0(
    sample(c("", "-"), n, TRUE), "9007199254740", sample(990:995, n, TRUE)
  ))
  d$s <- sample(c("a", "b", "c"), n, TRUE)
  # `zona` depende de `cod`: con los negativos igualados, la particion de `cod`
  # es una sola y la dependencia se pierde.
  d$zona <- c("norte", "sur", "este")[k %% 3L + 1L]
  d
}

# Lo que cambia entre dos corridas iguales: relojes, ids de corrida y cierres.
.sin_relojes <- function(x) {
  relojes <- c("duracion_ms", "cpu_ms", "duracion_estimada_ms",
               "duracion_referencia_ms", "ms_por_fila", "cota_lectura_ms",
               "fecha_hora", "rss_maximo", "corrida_id", "lectura_proceso",
               "evento_id", "tiempos")
  if (is.function(x)) return(NULL)
  if (!is.list(x) || is.null(names(x))) return(x)
  quitar <- names(x) %in% relojes
  if (any(quitar)) x <- x[!quitar]
  x[] <- lapply(x, .sin_relojes)
  x
}

test_that("el match() y el %in% del paquete emparejan un integer64 por su texto aunque el de base iguale los negativos", {
  skip_if_not_installed("bit64")
  # Cambia un metodo de la tabla S3 de base mientras dura: no en CRAN.
  skip_on_cran()
  x <- bit64::as.integer64(c("-1", "-1", "-2", "-3", "5"))
  grandes <- bit64::as.integer64(c("9007199254740992", "9007199254740993"))
  .con_mtfrm_integer64("bits", {
    # La premisa: el `match()` de base iguala los negativos. Sin esto la prueba
    # no reproduce nada.
    expect_identical(base::match(x, unique(x)), c(1L, 1L, 1L, 1L, 4L))
    expect_identical(match(x, unique(x)), c(1L, 1L, 2L, 3L, 4L))
    expect_identical(x %in% bit64::as.integer64(c("-2", "5")),
                     c(FALSE, FALSE, TRUE, FALSE, TRUE))
    expect_identical(tabulate(match(x, unique(x))), c(2L, 1L, 1L, 1L))
    # Contra un `double` se comparan textos, como base con bit64 nuevo: no se
    # trunca el `double` a entero.
    expect_identical(match(x, c(-1, 5, -2.5)), c(1L, 1L, NA, NA, 2L))
    # Por encima de 2^53 el texto no pierde cifras.
    expect_identical(match(grandes[2L], grandes), 2L)
  })
  # Fuera de la simulacion, lo mismo: el `match()` del paquete no depende de
  # lo que haga el de base.
  expect_identical(match(x, unique(x)), c(1L, 1L, 2L, 3L, 4L))
  # Y lo que no es `integer64` pasa sin tocar.
  expect_identical(match(c("b", "z"), c("a", "b")), c(2L, NA))
  expect_identical(c(1.5, NA) %in% c(NA, 2), c(FALSE, TRUE))
})

test_that("sin mtfrm.integer64 la guarda no se niega y el perfil es el mismo", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  d <- .datos_bit64_anterior()
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  referencia <- suppressMessages(perfilar(d, fecha = fecha))
  sin_metodo <- .con_mtfrm_integer64("ausente", {
    expect_true(is.null(utils::getS3method("mtfrm", "integer64", optional = TRUE)))
    suppressMessages(perfilar(d, fecha = fecha))
  })
  expect_identical(sin_metodo, referencia)
})

test_that("con el match() de base que iguala los negativos, el perfil y lo que encadena no cambian", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  d <- .datos_bit64_anterior()
  anterior <- d
  anterior$neg[1:30] <- bit64::as.integer64("-9")
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  correr <- function() {
    p <- suppressMessages(perfilar(d, fecha = fecha))
    list(
      perfil = p,
      analisis = suppressMessages(analizar(d, fecha = fecha, id_medicion = "m1")),
      hallazgos = hallazgos(p),
      comparacion = comparar_perfiles(
        suppressMessages(perfilar(anterior, fecha = fecha - 86400)), p
      ),
      plan = planificar_limpieza(p, datos = d)
    )
  }
  referencia <- correr()
  simulado <- .con_mtfrm_integer64("bits", {
    # La premisa, sobre estos datos: el `match()` de base no empareja como el
    # texto de los valores.
    texto <- bit64::as.character.integer64(d$neg)
    expect_false(identical(base::match(d$neg, unique(d$neg)),
                           base::match(texto, unique(texto))))
    correr()
  })
  # Primera mitad: hay dependencias que el `match()` decide. Sin ellas la
  # comparacion pasaria con el defecto vivo.
  expect_true(nrow(referencia$perfil$dependencias) > 0L)
  for (parte in names(referencia)) {
    expect_identical(simulado[[parte]], referencia[[parte]], info = parte)
  }
})

test_that("por la via DBI, con y sin bloques, el resultado no cambia y la muestra se perfila", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", .datos_bit64_anterior())
  correr <- function() {
    avisos <- character()
    r <- withCallingHandlers(
      list(
        sin_bloques = perfilar_dbi(con, "t"),
        con_bloques = perfilar_dbi(con, "t", bloque_filas = 50L)
      ),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(resultado = .sin_relojes(unclass(r)), avisos = avisos)
  }
  referencia <- correr()
  # Control: dos corridas iguales dan lo mismo una vez quitados los relojes.
  expect_identical(correr(), referencia)
  simulado <- .con_mtfrm_integer64("bits", correr())
  # Primera mitad: las columnas llegan `integer64` -RSQLite las da asi porque
  # no entran en 32 bits- y la muestra se perfila.
  expect_true("integer64" %in% referencia$resultado$sin_bloques$perfil_muestra$columnas$tipo_inferido)
  expect_false(is.null(simulado$resultado$sin_bloques$perfil_muestra))
  expect_identical(simulado$avisos, referencia$avisos)
  expect_identical(simulado$resultado$sin_bloques, referencia$resultado$sin_bloques)
  expect_identical(simulado$resultado$con_bloques, referencia$resultado$con_bloques)
})
