# Dos lecturas de fechas-hora que dependian de una forma y no del instante:
# `detectar_formatos_fecha()` sobre un `POSIXlt` contaba cero filas con la fecha
# civil cambiada en UTC -`format()` de un `POSIXlt` ignora `tz`-, y una fecha
# RFC 3339 con `t` o `z` minuscula, que la norma admite, no era una fecha.

.fecha_rfc <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("un POSIXlt cuenta la fecha civil distinta en UTC como su POSIXct", {
  zona <- "America/Montevideo"
  lt <- as.POSIXlt(c("2020-01-31 23:30:00", "2020-06-30 23:30:00",
                     "2020-06-30 12:00:00"), tz = zona)
  ct <- as.POSIXct(lt)
  # A mano: las 23:30 de Montevideo (UTC-3) son las 02:30 del dia siguiente en
  # UTC; el mediodia no cambia de dia.
  esperado <- sum(format(ct, "%Y-%m-%d", tz = zona) !=
                    format(ct, "%Y-%m-%d", tz = "UTC"))
  expect_identical(esperado, 2L)
  r_lt <- detectar_formatos_fecha(lt)
  r_ct <- detectar_formatos_fecha(ct)
  expect_identical(attr(r_lt, "n_filas_fecha_civil_distinta_utc"), 2L)
  expect_identical(attr(r_lt, "zona_horaria_origen"), zona)
  expect_identical(attributes(r_lt), attributes(r_ct))
  expect_identical(as.data.frame(r_lt), as.data.frame(r_ct))
})

test_that("una fecha RFC 3339 con t o z minuscula se lee como con mayuscula", {
  mayuscula <- c("2020-01-31T23:30:00Z", "2020-06-30T23:30:00Z",
                 "2020-03-31T23:30:00.5Z", "2020-03-31T20:30:00-03:00")
  formas <- list(
    z = sub("Z$", "z", mayuscula),
    t = sub("T", "t", mayuscula, fixed = TRUE),
    las_dos = sub("Z$", "z", sub("T", "t", mayuscula, fixed = TRUE))
  )
  campos <- c("tipo_inferido", "minimo_fecha", "maximo_fecha", "media_fecha",
              "mediana_fecha", "estado_resumen_cuantitativo",
              "n_fechas_resumidas")
  p <- perfilar(data.frame(v = mayuscula, stringsAsFactors = FALSE),
                fecha = .fecha_rfc)
  referencia <- p$columnas[, campos]
  expect_identical(referencia$tipo_inferido, "fecha-hora")
  for (nombre in names(formas)) {
    x <- formas[[nombre]]
    expect_identical(as.data.frame(detectar_formatos_fecha(x)),
                     as.data.frame(detectar_formatos_fecha(mayuscula)),
                     info = nombre)
    q <- perfilar(data.frame(v = x, stringsAsFactors = FALSE),
                  fecha = .fecha_rfc)
    expect_identical(q$columnas[, campos], referencia, info = nombre)
  }
})

test_that("el plan convierte la columna con z minuscula a los mismos instantes", {
  mayuscula <- c("2020-01-31T23:30:00Z", "2020-06-30T23:30:00Z",
                 "2020-03-31T23:30:00Z")
  datos <- data.frame(M = mayuscula, z = sub("Z$", "z", mayuscula),
                      stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos, fecha = .fecha_rfc), datos)
  campos <- c("estrategia", "estado", "destructiva", "aplicar")
  de_m <- as.data.frame(plan)[plan$columna == "M", campos]
  de_z <- as.data.frame(plan)[plan$columna == "z", campos]
  rownames(de_m) <- rownames(de_z) <- NULL
  expect_identical(de_z, de_m)
  expect_true(any(de_z$aplicar))
  limpio <- aplicar(plan, datos)$datos
  expect_true(inherits(limpio$z, "POSIXct"))
  expect_identical(as.numeric(limpio$z), as.numeric(limpio$M))
  # A mano: el primero es el 31 de enero de 2020 a las 23:30 en UTC.
  expect_identical(as.numeric(limpio$z[[1L]]),
                   as.numeric(ISOdatetime(2020, 1, 31, 23, 30, 0, tz = "UTC")))
})

test_that("una t o una z que no cierran una fecha-hora no cambian nada", {
  # Solo se sube la letra cuando el texto entero ya tiene la forma de una
  # fecha-hora: lo demas sigue siendo lo que era.
  x <- c("2020-01-31tarde", "2020-01-31 z", "t2020-01-31", "zona 3")
  expect_identical(nrow(detectar_formatos_fecha(x)), 0L)
  expect_identical(inferir_tipo(x)$tipo,
                   inferir_tipo(toupper(x))$tipo)
})
