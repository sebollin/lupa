# Ronda 14: fechas, validadores y perfil por grupos.

test_that("el formato compacto respeta su rango de anos tambien en el resumen", {
  x <- c(sprintf("202401%02d", 10:19), "99991231")
  columnas <- suppressWarnings(perfilar(data.frame(f = x)))$columnas
  # Antes: maximo_fecha 9999-12-31 y 11 fechas resumidas.
  expect_equal(as.character(columnas$maximo_fecha), "2024-01-19")
  expect_equal(columnas$n_fechas_resumidas, 10)
  expect_equal(columnas$n_valores_excluidos_resumen, 1)
})

test_that("una fecha ambigua entre dia/mes y mes/dia no se resuelve por mayoria", {
  rango <- function(x) {
    c <- suppressWarnings(perfilar(data.frame(f = x)))$columnas
    c(as.character(c$minimo_fecha), as.character(c$maximo_fecha))
  }
  base <- c("13/06/2020", "06/30/2020", "01/12/2020")
  # Antes: agregar una fila que no esta en ningun extremo movia el rango cinco
  # meses, porque `01/12` pasaba de 1-dic a 12-ene.
  expect_identical(rango(base), rango(c(base, "06/29/2020")))
  expect_identical(rango(base), c("2020-06-13", "2020-06-30"))
  # Y el plan no convierte mientras haya valores que los dos formatos leen
  # distinto.
  df <- data.frame(f = base)
  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(df)), datos = df))
  accion <- plan[plan$estrategia == "convertir_fecha_confirmada", , drop = FALSE]
  expect_identical(as.character(accion$estado), "bloqueada")
  # Control: sin ambiguos, la conversion sigue lista.
  df2 <- data.frame(f = c("13/06/2020", "06/30/2020", "14/02/2021", "12/31/2020"))
  plan2 <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(df2)), datos = df2))
  expect_identical(
    as.character(plan2$estado[plan2$estrategia == "convertir_fecha_confirmada"]),
    "lista"
  )
})

test_that("un segundo 60 y un huso imposible no son fechas, y no hay aviso crudo", {
  avisos <- character()
  withCallingHandlers(
    detectar_formatos_fecha(c("2024-01-31T23:30:00+15:00", "2024-01-30T10:00:00-03:00")),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("outside", avisos, fixed = TRUE)))
  formatos <- detectar_formatos_fecha(c("2024-01-31 23:59:60", "2024-01-30 10:00:00"))
  expect_true(all(formatos$n <= 1L))
})

test_that("los validadores responden por el dato, no por su escritura en R", {
  # Un doble se valida por sus digitos, con cualquier `scipen`.
  expect_true(validar_ci_uy(12000000))
  expect_true(validar_luhn(59000000))
  viejo <- options(scipen = -9)
  on.exit(options(viejo), add = TRUE)
  expect_true(validar_ci_uy(12345672))
  options(viejo)
  # Letras no ASCII que `toupper()` llevaba a ASCII.
  expect_false(any(validar_iso3166(c("\u017fe", "\u0131t"))))
  expect_false(validar_iso4217("u\u017fd"))
  expect_false(validar_mod97("\u017f123405"))
  expect_true(validar_iso3166("uy"))
  # La misma URL marcada latin1 y UTF-8.
  u <- "https://espa\u00f1a.es"
  expect_identical(validar_url(c(u, iconv(u, "UTF-8", "latin1"))), c(TRUE, TRUE))
  # NaN es ausente.
  expect_true(is.na(validar_luhn(NaN)))
})

test_that("perfilar_por etiqueta bien cada clase y declara lo que no perfila", {
  set.seed(2)
  grupos_de <- function(g) {
    d <- data.frame(x = c(rep(NA, 5), stats::runif(75)))
    d$g <- rep(g, each = 40)[1:80]
    h <- suppressWarnings(perfilar_por(d, "g", min_filas = 10))
    cb <- attr(h, "cobertura_grupos")
    unique(c(as.character(h$grupo), as.character(cb$grupo)))
  }
  expect_identical(
    grupos_de(as.Date(c("2020-01-01", "2021-06-15"))),
    c("2020-01-01", "2021-06-15")
  )
  expect_length(grupos_de(complex(real = c(1e17, 1e17 + 32), imaginary = 1)), 2L)
  # Un factor con NA como nivel no aborta.
  f <- addNA(factor(rep(c("a", NA), each = 40)))
  expect_no_error(suppressWarnings(perfilar_por(
    data.frame(g = f, x = stats::runif(80)), "g", min_filas = 10
  )))
  # Nombres repetidos y min_filas fraccionario se rechazan con un error claro.
  repetidos <- data.frame(g = rep("a", 40), v = 1, v = 2, check.names = FALSE)
  expect_error(perfilar_por(repetidos, "g"), "repetidos")
  expect_error(perfilar_por(data.frame(g = rep("a", 40), x = 1), "g",
                            min_filas = 10.7), "entero")
  # La reconciliacion cierra aunque un grupo salga limpio.
  limpio <- data.frame(g = rep(c("a", "b"), each = 40),
                       x = c(stats::runif(40), c(rep(NA, 20), stats::runif(20))))
  h <- suppressWarnings(perfilar_por(limpio, "g", min_filas = 10))
  cb <- attr(h, "cobertura_grupos")
  filas <- c(stats::setNames(h$n_filas_grupo, h$grupo),
             stats::setNames(cb$n_filas_grupo, cb$grupo))
  expect_equal(sum(filas[!duplicated(names(filas))]), nrow(limpio))
})

test_that("perfilar_por etiqueta un integer64 grande y una fecha-hora por su valor", {
  skip_if_not_installed("bit64")
  g <- bit64::as.integer64(rep(c("9007199254740993", "9007199254740992"), each = 40))
  d <- data.frame(g = g, x = c(rep(NA, 5), stats::runif(75)))
  h <- suppressWarnings(perfilar_por(d, "g", min_filas = 10))
  etiquetas <- unique(c(as.character(h$grupo),
                        as.character(attr(h, "cobertura_grupos")$grupo)))
  # Antes: 4.4501477170144038e-308, los bits crudos del entero.
  expect_setequal(etiquetas, c("9007199254740993", "9007199254740992"))
  expect_equal(sum(as.character(d$g) == "9007199254740993"), 40L)
})

test_that("una fecha-hora como grupo no se etiqueta con sus segundos", {
  t0 <- as.POSIXct(c("2020-01-01 10:00:00", "2020-01-01 10:00:01"), tz = "UTC")
  d <- data.frame(g = rep(t0, each = 40), x = c(rep(NA, 5), stats::runif(75)))
  h <- suppressWarnings(perfilar_por(d, "g", min_filas = 10))
  etiquetas <- unique(c(as.character(h$grupo),
                        as.character(attr(h, "cobertura_grupos")$grupo)))
  expect_length(etiquetas, 2L)
  expect_true(all(startsWith(etiquetas, "2020-01-01 10:00:0")))
})

test_that("el veredicto de comparar_equivalencia coincide con su diferencia publicada", {
  a <- 0x1.871999999999ap+9
  b <- 0x1.3e2e1181231ecp+9
  tolerancia <- 0x1.7dd8c2c666665p-3
  anterior <- data.frame(columna = "x", media = a, stringsAsFactors = FALSE)
  actual <- data.frame(columna = "x", media = b, stringsAsFactors = FALSE)
  r <- comparar_equivalencia(anterior, actual, tolerancia = tolerancia)
  # Antes: la diferencia publicada superaba la tolerancia por un ulp y el
  # veredicto decia `equivalente`.
  expect_identical(
    as.character(r$veredicto) == "equivalente",
    r$diferencia_normalizada <= tolerancia
  )
  # Control y direccion opuesta: con la tolerancia igual a la diferencia que
  # publica, la fila es equivalente.
  r2 <- comparar_equivalencia(anterior, actual,
                              tolerancia = r$diferencia_normalizada)
  expect_identical(as.character(r2$veredicto), "equivalente")
})

test_that("un nivel `(ausente)` declarado y sin filas se declara junto a los NA", {
  g <- factor(c(rep("a", 40), rep(NA, 40)), levels = c("a", "(ausente)"))
  h <- suppressWarnings(perfilar_por(data.frame(g = g, x = rep(1:4, 20)), "g",
                                     min_filas = 10))
  cb <- attr(h, "cobertura_grupos")
  expect_true(any(cb$grupo == "(ausente)" & cb$n_filas_grupo == 0L))
  # Control: el grupo de los NA se perfila con sus 40 filas.
  expect_true(any(h$grupo == "(ausente)" & h$n_filas_grupo == 40L))
})
