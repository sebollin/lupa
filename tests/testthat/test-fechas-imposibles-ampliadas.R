testthat::skip_on_cran()

# Formas de fecha que la regla de fechas imposibles no miraba -ano de dos
# cifras, mes y ano, fecha-hora y formatos que ningun otro valor usa- y el
# identificador numerico con ausentes, que no es una numeracion 1..n.

hallazgos_ampliados <- function(d, tipo, ...) {
  h <- as.data.frame(hallazgos(perfilar(d, ...)))
  h[h$tipo_hallazgo == tipo, , drop = FALSE]
}

filas_ampliadas <- function(h) h$trazabilidad[[1L]]$indices_fila

fechas_ampliadas <- function(n, formato = "%Y-%m-%d") {
  format(as.Date("2022-01-01") + (seq_len(n) * 11L) %% 300L, formato)
}

test_that("fecha_invalida ve el ano de dos cifras con una fecha imposible", {
  d <- data.frame(id = 1:300, f = fechas_ampliadas(300, "%d/%m/%y"),
                  stringsAsFactors = FALSE)
  d$f[c(1, 2, 3)] <- c("31/02/22", "32/01/22", "99/99/99")
  h <- hallazgos_ampliados(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 3)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(filas_ampliadas(h)), c(1L, 2L, 3L))
})

test_that("fecha_invalida ve el mes imposible de una fecha parcial", {
  d <- data.frame(id = 1:300, f = fechas_ampliadas(300, "%Y-%m"),
                  stringsAsFactors = FALSE)
  d$f[c(1, 2, 3)] <- c("2022-13", "2022-00", "2022-12")
  h <- hallazgos_ampliados(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 2)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(filas_ampliadas(h)), c(1L, 2L))
})

test_that("fecha_invalida ve la fecha imposible de una fecha-hora", {
  d <- data.frame(id = 1:300, f = fechas_ampliadas(300, "%Y-%m-%d %H:%M"),
                  stringsAsFactors = FALSE)
  d$f[c(1, 2, 3)] <- c("2022-02-30 10:00", "2022-13-01 10:00",
                       "2022-01-01 10:00")
  h <- hallazgos_ampliados(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 2)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(filas_ampliadas(h)), c(1L, 2L))
})

test_that("fecha_invalida ve una fecha imposible en un formato no dominante", {
  d <- data.frame(id = 1:300, f = fechas_ampliadas(300),
                  stringsAsFactors = FALSE)
  d$f[c(3, 9, 27, 81)] <- c("30/02/2022", "2022-02-30", "2022-13-01",
                            "0000-00-00")
  h <- hallazgos_ampliados(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 4)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(filas_ampliadas(h)), c(3L, 9L, 27L, 81L))
  expect_match(h$evidencia, "30/02/2022", fixed = TRUE)
})

test_that("las formas validas de fecha no se acusan", {
  d <- data.frame(
    id = 1:300,
    dos_cifras = fechas_ampliadas(300, "%d/%m/%y"),
    parcial = fechas_ampliadas(300, "%Y-%m"),
    stringsAsFactors = FALSE
  )
  d$dos_cifras[c(1, 2, 3, 4)] <- c("03/04/22", "13/01/22", "01/02/03",
                                   "31/12/99")
  d$parcial[c(1, 2, 3)] <- c("2022-01", "2022-12", "2022-06")
  expect_equal(nrow(hallazgos_ampliados(d, "fecha_invalida")), 0L)
})

test_that("la fecha-hora con zona horaria y el fin del dia no se acusan", {
  base <- format(
    as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + seq_len(300) * 3600,
    "%Y-%m-%dT%H:%M:%S"
  )
  d <- data.frame(id = 1:300, f = paste0(base, "-03:00"),
                  stringsAsFactors = FALSE)
  d$f[c(1, 2, 3)] <- c("2024-01-31T23:30:00Z",
                       "2024-01-31T23:30:00+05:30",
                       "2024-01-31T24:00:00-03:00")
  expect_equal(nrow(hallazgos_ampliados(d, "fecha_invalida")), 0L)

  e <- data.frame(id = 1:300, f = fechas_ampliadas(300, "%Y-%m-%d %H:%M"),
                  stringsAsFactors = FALSE)
  e$f[1] <- "2024-01-31 24:00"
  expect_equal(nrow(hallazgos_ampliados(e, "fecha_invalida")), 0L)
  e$f[2] <- "2024-02-30 24:00"
  h <- hallazgos_ampliados(e, "fecha_invalida")
  expect_equal(filas_ampliadas(h), 2L)
})

test_that("identificador_con_ausentes ve el identificador numerico", {
  n <- 500
  d <- data.frame(id = 1:n, num = 1001:1500, stringsAsFactors = FALSE)
  d$num[c(4, 40, 77)] <- NA
  h <- hallazgos_ampliados(d, "identificador_con_ausentes")
  expect_equal(nrow(h), 1L)
  expect_equal(h$columna, "num")
  expect_equal(h$n_afectados, 3)
  expect_equal(h$n_evaluados, 500)
  expect_equal(sort(filas_ampliadas(h)), c(4L, 40L, 77L))
  expect_equal(as.character(h$severidad), "ok")
  expect_match(h$evidencia, "1..n", fixed = TRUE)
})

test_that("identificador_con_ausentes no acusa un conteo ni una numeracion", {
  n <- 500
  d <- data.frame(
    id = 1:n,
    conteo = replace(seq_len(n), c(4L, 40L, 77L), NA_integer_),
    numeracion = replace(seq_len(n), c(5L, 41L, 78L), NA_integer_),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L)
  # Aunque se llame como una clave, una numeracion 1..n sigue siendo un conteo.
  names(d)[names(d) == "numeracion"] <- "id_secuencia"
  expect_equal(nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L)
})

test_that("identificador_con_ausentes no acusa una magnitud unica", {
  n <- 500
  set.seed(20261120)
  d <- data.frame(id = 1:n, monto = round(stats::runif(n, 1000, 9000), 2))
  d$monto[c(4, 40, 77)] <- NA
  expect_equal(nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L)
})

test_that("identificador_con_ausentes ve un identificador numerico disperso", {
  n <- 500
  set.seed(20261121)
  d <- data.frame(id = 1:n, id_factura = sample(100000:999999, n),
                  stringsAsFactors = FALSE)
  d$id_factura[c(4, 40, 77)] <- NA
  h <- hallazgos_ampliados(d, "identificador_con_ausentes")
  expect_equal(nrow(h), 1L)
  expect_equal(h$columna, "id_factura")
  expect_equal(sort(filas_ampliadas(h)), c(4L, 40L, 77L))
})

test_that("perfilar_por aplica la regla numerica por grupo", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 300),
    num = c(1001:1300, seq_len(300)),
    stringsAsFactors = FALSE
  )
  d$num[c(4, 40, 77)] <- NA
  pp <- perfilar_por(d, por = "g")
  x <- pp[pp$tipo_hallazgo == "identificador_con_ausentes", , drop = FALSE]
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$grupo), "a")
})

test_that("perfilar_dbi publica las dos formas nuevas en SQLite y DuckDB", {
  skip_if_not_installed("DBI")
  n <- 300
  d <- data.frame(
    id = 1:n,
    num = 1001:1300,
    f = fechas_ampliadas(n, "%d/%m/%y"),
    stringsAsFactors = FALSE
  )
  d$num[c(4, 40, 77)] <- NA
  d$f[c(5, 9)] <- c("31/02/22", "30/02/22")

  revisar <- function(con) {
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "t", d)
    h <- as.data.frame(hallazgos(perfilar_dbi(con, "t")))
    expect_true("fecha_invalida" %in% h$tipo_hallazgo)
    expect_true("identificador_con_ausentes" %in% h$tipo_hallazgo)
    f <- h[h$tipo_hallazgo == "fecha_invalida", , drop = FALSE]
    expect_equal(sort(f$trazabilidad[[1L]]$indices_fila), c(5L, 9L))
  }

  revisiones <- 0L
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    revisar(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
    revisiones <- revisiones + 1L
  }
  if (requireNamespace("duckdb", quietly = TRUE) &&
      exists(".duckdb_prueba", mode = "function")) {
    con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
    on.exit(try(duckdb::duckdb_shutdown(con), silent = TRUE), add = TRUE)
    revisar(con)
    revisiones <- revisiones + 1L
  }
  expect_gte(revisiones, 1L)
})

test_that("analizar incluye las dos ampliaciones en el perfil", {
  n <- 300
  d <- data.frame(
    id = 1:n,
    num = 1001:1300,
    f = fechas_ampliadas(n, "%d/%m/%y"),
    stringsAsFactors = FALSE
  )
  d$num[c(4, 40, 77)] <- NA
  d$f[c(5, 9)] <- c("31/02/22", "30/02/22")
  r <- analizar(
    d, fecha = as.POSIXct("2024-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  h <- as.data.frame(r$perfil$hallazgos)
  expect_true("fecha_invalida" %in% h$tipo_hallazgo)
  expect_true("identificador_con_ausentes" %in% h$tipo_hallazgo)
  expect_equal(sort(h$trazabilidad[[match("fecha_invalida", h$tipo_hallazgo)]]$
                     indices_fila), c(5L, 9L))
})

test_that("el segundo intercalar 23:59:60 no se acusa en fecha-hora", {
  base <- format(
    as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + seq_len(300) * 3600,
    "%Y-%m-%d %H:%M:%S"
  )
  d <- data.frame(id = 1:300, f = base, stringsAsFactors = FALSE)
  d$f[c(1, 2)] <- c("2016-12-31 23:59:60", "2022-06-30 23:59:60")
  expect_equal(nrow(hallazgos_ampliados(d, "fecha_invalida")), 0L)
})

test_that("el segundo intercalar 23:59:60 no se acusa en hora", {
  base <- format(
    as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + seq_len(300) * 60,
    "%H:%M:%S"
  )
  d <- data.frame(id = 1:300, hora = base, stringsAsFactors = FALSE)
  d$hora[c(1, 2, 3)] <- c("23:59:60", "12:00:60", "23:59:61")
  h <- hallazgos_ampliados(d, "hora_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(sort(filas_ampliadas(h)), c(2L, 3L))
})

test_that("identificador_con_ausentes no acusa nombres con magnitud ni subcadenas", {
  set.seed(20261120)
  N <- 2000
  nombres <- c(
    "num_ventas", "numero_de_hijos", "nro_orden", "documento_monto",
    "importe_codigo", "id_peso", "valid", "paid", "idx", "pid", "ide"
  )
  for (nm in nombres) {
    v <- sample(5000:90000, N)
    v[sample(N, 10)] <- NA
    d <- data.frame(x = v)
    names(d) <- nm
    expect_equal(
      nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L,
      info = nm
    )
  }
})

test_that("identificador_con_ausentes no acusa id_ratio flotante", {
  set.seed(20261123)
  d <- data.frame(id_ratio = c(runif(1990, 1, 5000), rep(NA, 10)))
  expect_equal(nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L)
})

test_that("identificador_con_ausentes no acusa un identificador con valores no enteros", {
  set.seed(20261122)
  d <- data.frame(id_cliente = c(runif(1990, 1, 5000), rep(NA, 10)))
  expect_equal(nrow(hallazgos_ampliados(d, "identificador_con_ausentes")), 0L)
})

test_that("identificador_con_ausentes sigue acusando claves numericas completas", {
  set.seed(20261121)
  N <- 2000
  for (nm in c("id_cliente", "cliente_id", "codigo_postal")) {
    v <- sample(5000:90000, N)
    v[sample(N, 10)] <- NA
    d <- data.frame(x = v)
    names(d) <- nm
    h <- hallazgos_ampliados(d, "identificador_con_ausentes")
    expect_equal(nrow(h), 1L, info = nm)
    expect_equal(h$columna, nm, info = nm)
  }
})
