# Ronda 28-N: lo que la refutacion de los diagnosticos numericos y
# estructurales encontro. Una prueba por hallazgo, con la cifra rehecha a mano y
# tablas chicas; la unica columna grande (H6, 200.000 filas) se mide sobre la
# guarda, sin perfilar.

primer_digito_a_mano <- function(x) {
  as.integer(substr(formatC(x, format = "e", digits = 6), 1L, 1L))
}

chi2_benford_a_mano <- function(x) {
  observados <- tabulate(primer_digito_a_mano(x), 9L)
  esperados <- length(x) * log10(1 + 1 / seq_len(9L))
  sum((observados - esperados)^2 / esperados)
}

relaciones_aritmeticas_r28n <- function(perfil) {
  h <- as.data.frame(perfil$hallazgos)
  h[h$tipo_hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
}

test_that("H6: la guarda de Benford no depende de cuantas filas se cargaron", {
  # La columna de la refutacion: magnitud entera 1..9999 con un 20 % de montos
  # fabricados en 5000..5999. Con 200.000 filas ocupa casi todos los enteros de
  # su rango, y la guarda la daba por secuencia correlativa.
  set.seed(2812)
  for (n in c(2000L, 200000L)) {
    x <- floor(10^stats::runif(n, 0, 4))
    k <- sample(n, n / 5)
    x[k] <- sample(5000:5999, length(k), TRUE)
    distintos <- sort(unique(x))
    # A mano: con 200.000 filas la densidad pasa 0,5, pero cada valor se repite.
    if (n == 200000L) {
      expect_gt(length(distintos) / (max(distintos) - min(distintos) + 1), 0.5)
    }
    expect_lt(length(distintos) / n, 0.9)
    expect_false(.parece_correlativo_benford(x))
    # `posible_identificador = TRUE` es lo que el perfil le pasa cuando publica
    # el texto de codigo de catalogo, como hacia con 200.000 filas.
    r <- .resultado_benford_columna(x, "monto", "entero", TRUE)
    expect_true(r$aplica)
    expect_true(r$desviacion)
    if (n == 2000L) {
      expect_equal(r$estadistico, chi2_benford_a_mano(x), tolerance = 1e-9)
    }
  }
  # El correlativo de verdad sigue sin aplicar a los dos tamanos, con y sin
  # huecos.
  for (n in c(2000L, 200000L)) {
    id <- sample(n)
    expect_true(.parece_correlativo_benford(id))
    expect_false(.resultado_benford_columna(id, "id", "entero", TRUE)$aplica)
  }
  con_huecos <- sort(sample(9000L, 5000L))
  expect_true(.parece_correlativo_benford(con_huecos))
  # Un valor que concentra filas no es que la numeracion se repita: se descuenta
  # el excedente de la moda (el caso de test-N76).
  expect_true(.parece_correlativo_benford(c(1:2000, rep(500, 500))))
})

test_that("H6: el perfil entero no calla la desviacion al crecer la tabla", {
  set.seed(28061)
  for (n in c(500L, 3000L)) {
    x <- floor(10^stats::runif(n, 0, 3.05))
    k <- sample(n, round(0.2 * n))
    x[k] <- sample(500:599, length(k), TRUE)
    p <- suppressWarnings(perfilar(
      data.frame(monto = x, id = sample(n)), analizar_dependencias = FALSE
    ))
    h <- as.data.frame(p$hallazgos)
    r <- p$meta$benford$resultados
    expect_true(r$monto$aplica)
    expect_equal(r$monto$estadistico, chi2_benford_a_mano(x), tolerance = 1e-9)
    expect_true(any(h$tipo_hallazgo == "desviacion_benford" &
                      h$columna == "monto"))
    expect_false(r$id$aplica)
  }
})

test_that("H2: una columna que la tolerancia absorbe no completa una suma", {
  set.seed(285)
  n <- 400L
  bruto <- round(stats::runif(n, 1e9, 2e9), 2)
  com <- round(stats::runif(n, 1, 9), 2)
  hijos <- sample(0:5, n, TRUE) * 1
  hijos[c(3, 50, 77, 120, 300)] <- c(25, 30, 22, 40, 28)
  d <- data.frame(bruto = bruto, neto = bruto - com, comision = com,
                  hijos = hijos)
  # A mano: con la tolerancia por omision `neto ~= bruto` ya se cumple solo, y
  # `neto + hijos` cae fuera justo en las cinco filas donde hijos supera 20.
  banda <- 1e-8 * pmax(1, abs(d$bruto), abs(d$neto))
  expect_true(all(abs(d$bruto - d$neto) <= banda))
  fuera <- which(abs(d$bruto - (d$neto + d$hijos)) >
                   1e-8 * pmax(1, abs(d$bruto), abs(d$neto + d$hijos)))
  expect_equal(fuera, c(3L, 50L, 77L, 120L, 300L))
  # Y la relacion real es exacta salvo el redondeo de un doble.
  expect_true(all(abs(d$bruto - (d$neto + d$comision)) <=
                    4 * .Machine$double.eps * abs(d$bruto)))

  p <- suppressWarnings(perfilar(d, analizar_dependencias = FALSE))
  rel <- relaciones_aritmeticas_r28n(p)
  expect_equal(rel$columna, "neto,comision,bruto")
  expect_equal(as.character(rel$severidad), "ok")
  expect_match(rel$evidencia, "se cumple exacta", fixed = TRUE)
  expect_gte(p$meta$aritmetica_columnas$identidades_descartadas_escala, 1)

  # Sin la columna real, la ajena no inventa la suma; y la misma tabla en otra
  # escala sigue sin relaciones.
  d2 <- data.frame(bruto = bruto, neto = bruto - com,
                   cuotas = sample(1:12, n, TRUE) * 1)
  expect_equal(nrow(relaciones_aritmeticas_r28n(suppressWarnings(
    perfilar(d2, analizar_dependencias = FALSE)
  ))), 0L)
  d4 <- data.frame(bruto = bruto / 1e6, neto = (bruto - com) / 1e6,
                   cuotas = d2$cuotas)
  expect_equal(nrow(relaciones_aritmeticas_r28n(suppressWarnings(
    perfilar(d4, analizar_dependencias = FALSE)
  ))), 0L)
})

test_that("H7: un desborde de enteros no apaga ni aborta la relacion", {
  set.seed(2814)
  a <- sample(1:1000000, 300, TRUE)
  b <- sample(1:1000000, 300, TRUE)
  a[10] <- 2000000000L
  b[10] <- 200000000L
  total <- as.numeric(a) + as.numeric(b)
  # A mano: en doble la identidad vale en todas las filas, y en entero la fila
  # 10 desborda.
  expect_true(all(total == as.double(a) + as.double(b)))
  expect_gt(as.double(a[10]) + b[10], .Machine$integer.max)
  avisos <- character()
  p <- withCallingHandlers(
    perfilar(data.frame(a = a, b = b, total = total),
             analizar_dependencias = FALSE),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  rel <- relaciones_aritmeticas_r28n(p)
  expect_equal(rel$columna, "a,b,total")
  expect_equal(rel$n_evaluados, 300)
  expect_equal(rel$n_afectados, 0)
  expect_false(any(grepl("integer|enteros", avisos)))

  # El rango de un entero de -2e9 a 2e9 tambien desbordaba, y ahi perfilar()
  # abortaba.
  ancho <- c(-2000000000L, 2000000000L, sample(-1000000:1000000, 58))
  extra <- sample(1:1000, 60, TRUE)
  p2 <- suppressWarnings(perfilar(
    data.frame(a = ancho, b = extra, total = as.double(ancho) + extra),
    analizar_dependencias = FALSE
  ))
  expect_equal(relaciones_aritmeticas_r28n(p2)$columna, "a,b,total")
})

test_that("H7: por DBI, el INTEGER que devuelve RSQLite no calla la relacion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(2821)
  a <- sample(1:1500000000, 120, TRUE)
  b <- sample(1:1500000000, 120, TRUE)
  total <- as.numeric(a) + as.numeric(b)
  expect_gt(sum(total > .Machine$integer.max), 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = a, b = b, total = total))
  expect_true(is.integer(DBI::dbGetQuery(con, "select a from t limit 1")$a))
  pd <- suppressWarnings(perfilar_dbi(con, "t"))
  rel <- relaciones_aritmeticas_r28n(pd$perfil_muestra)
  expect_equal(rel$columna, "a,b,total")
})

test_that("H9: el centinela declarado sale de Benford como sale del resumen", {
  set.seed(2815)
  m <- round(exp(stats::rnorm(2000, 8, 2.5)), 2)
  m[sample(2000, 160)] <- 999
  limpio <- m[m != 999]
  p <- suppressWarnings(perfilar(
    data.frame(monto = m), sentinelas_numericos = 999,
    analizar_dependencias = FALSE
  ))
  r <- p$meta$benford$resultados$monto
  expect_equal(p$columnas$media, mean(limpio))
  expect_equal(r$n_finitos, length(limpio))
  expect_equal(r$estadistico, chi2_benford_a_mano(limpio), tolerance = 1e-9)
  expect_false(any(p$hallazgos$tipo_hallazgo == "desviacion_benford"))
  # Control: sin declarar, el resumen tambien los cuenta y Benford con el; ahi
  # la desviacion que producen los 999 es real para los dos.
  p0 <- suppressWarnings(perfilar(data.frame(monto = m),
                                  analizar_dependencias = FALSE))
  expect_equal(p0$meta$benford$resultados$monto$n_finitos, 2000L)
  expect_equal(p0$meta$benford$resultados$monto$estadistico,
               chi2_benford_a_mano(m), tolerance = 1e-9)
})

test_that("H9: el centinela declarado no acusa filas en la aritmetica", {
  set.seed(2816)
  n <- 400L
  a <- round(stats::runif(n, 10, 900), 2)
  b <- round(stats::runif(n, 10, 900), 2)
  total <- a + b
  marcadas <- c(5L, 17L, 33L, 80L, 150L, 222L, 301L, 350L)
  b[marcadas] <- -9
  p <- suppressWarnings(perfilar(
    data.frame(a = a, b = b, total = total), sentinelas_numericos = -9,
    analizar_dependencias = FALSE
  ))
  rel <- relaciones_aritmeticas_r28n(p)
  # A mano: sin las ocho filas declaradas, la identidad vale en las 392.
  sanas <- setdiff(seq_len(n), marcadas)
  expect_true(all(abs(total[sanas] - (a[sanas] + b[sanas])) <=
                    1e-8 * pmax(1, abs(total[sanas]))))
  expect_equal(rel$columna, "a,b,total")
  expect_equal(as.character(rel$severidad), "ok")
  expect_equal(rel$n_evaluados, length(sanas))
  expect_equal(rel$n_afectados, 0)
})

test_that("H3: el corte muestreado se refina al limite exacto", {
  set.seed(28063)
  n <- 600L
  ingreso <- round(stats::runif(n, 1000, 200000), 2)
  y <- stats::runif(n)
  y[ingreso < 50000] <- NA
  # A mano: mas de 200 cambios de valor, y la regla exacta separa el ultimo
  # ausente del primer presente.
  expect_gt(length(unique(ingreso)) - 1L, 200L)
  primer_presente <- min(ingreso[!is.na(y)])
  expect_lt(max(ingreso[is.na(y)]), primer_presente)
  p <- suppressWarnings(perfilar(data.frame(ingreso = ingreso, y = y),
                                 analizar_dependencias = FALSE))
  h <- as.data.frame(p$hallazgos)
  sugerencia <- h$sugerencia[h$tipo_hallazgo == "posible_ausencia_estructural"]
  expect_length(sugerencia, 1L)
  formula <- sub("^.*list\\(y = (~ [^`]*)\\)\\)`.*$", "\\1", sugerencia)
  regla <- eval(parse(text = formula)[[1L]])
  aplica <- eval(regla[[2L]], data.frame(ingreso = ingreso))
  expect_identical(aplica, !is.na(y))
  expect_equal(eval(regla[[2L]][[3L]]), primer_presente)
})

test_that("H11: el corte de un instante lleva su zona y no depende de la sesion", {
  t0 <- as.POSIXct("2024-02-28 12:00:00", tz = "UTC") + (0:119) * 1800
  set.seed(2818)
  y <- stats::runif(120)
  corte_real <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")
  y[t0 < corte_real] <- NA
  p <- suppressWarnings(perfilar(data.frame(alta = t0, y = y),
                                 analizar_dependencias = FALSE))
  h <- as.data.frame(p$hallazgos)
  sugerencia <- h$sugerencia[h$tipo_hallazgo == "posible_ausencia_estructural"]
  expect_length(sugerencia, 1L)
  expect_match(sugerencia, 'tz = "UTC"', fixed = TRUE)
  formula <- sub("^.*list\\(y = (~ [^`]*)\\)\\)`.*$", "\\1", sugerencia)
  # Se pega en una sesion de otra zona: tiene que ubicar las 120 filas igual.
  zona <- Sys.getenv("TZ", unset = NA)
  on.exit(
    if (is.na(zona)) Sys.unsetenv("TZ") else Sys.setenv(TZ = zona),
    add = TRUE
  )
  Sys.setenv(TZ = "America/Montevideo")
  regla <- eval(parse(text = formula)[[1L]])
  aplica <- eval(regla[[2L]], data.frame(alta = t0))
  expect_identical(aplica, t0 >= corte_real)
  expect_identical(aplica, !is.na(y))
})

test_that("H8: con OutDec = ',' la sugerencia sigue siendo codigo R", {
  set.seed(28064)
  n <- 300L
  ingreso <- round(stats::runif(n, 1000, 90000), 2)
  y <- stats::runif(n)
  y[ingreso < 30000] <- NA
  primer_presente <- min(ingreso[!is.na(y)])
  opciones <- options(OutDec = ",")
  on.exit(options(opciones), add = TRUE)
  p <- suppressWarnings(perfilar(data.frame(ingreso = ingreso, y = y),
                                 analizar_dependencias = FALSE))
  h <- as.data.frame(p$hallazgos)
  sugerencia <- h$sugerencia[h$tipo_hallazgo == "posible_ausencia_estructural"]
  expect_length(sugerencia, 1L)
  formula <- sub("^.*list\\(y = (~ [^`]*)\\)\\)`.*$", "\\1", sugerencia)
  regla <- eval(parse(text = formula)[[1L]])
  expect_equal(eval(regla[[2L]][[3L]]), primer_presente)
  expect_identical(eval(regla[[2L]], data.frame(ingreso = ingreso)), !is.na(y))
})

test_that("H1: los ordenes de magnitud son log10(max/min), como dice la ayuda", {
  set.seed(3)
  x <- c(0.534, 534, round(exp(stats::runif(498, log(0.534), log(534))), 6))
  x <- pmin(pmax(x, 0.534), 534)
  # A mano: el cociente es 1000 exacto en doble y su logaritmo es 3.
  expect_identical(max(x) / min(x), 1000)
  expect_identical(log10(max(x) / min(x)), 3)
  p <- suppressWarnings(perfilar(data.frame(m = x),
                                 analizar_dependencias = FALSE))
  r <- p$meta$benford$resultados$m
  expect_identical(r$ordenes_magnitud, 3)
  expect_true(r$aplica)
  # Y la comparacion publicada se lee verdadera aun a 4e-16 del umbral.
  casi <- 3 - 2 * .Machine$double.eps
  texto <- .decimal_frente_a_umbral(casi, 3)
  expect_lt(as.numeric(texto), 3)
})

test_that("H4: n_afectados cuenta las ausencias que la regla explica", {
  set.seed(28066)
  cat_a <- c(rep("A", 900), rep("B", 100))
  y <- stats::runif(1000)
  y[cat_a == "B"] <- NA
  y[sample(which(cat_a == "A"), 9)] <- NA
  p <- suppressWarnings(perfilar(data.frame(cat = cat_a, y = y),
                                 analizar_dependencias = FALSE))
  h <- as.data.frame(p$hallazgos)
  senal <- h[h$tipo_hallazgo == "posible_ausencia_estructural", ]
  # A mano: 109 ausencias, 100 de ellas donde la regla dice que no corresponde.
  expect_equal(senal$n_evaluados, sum(is.na(y)))
  expect_equal(senal$n_afectados, sum(is.na(y) & cat_a == "B"))
  expect_equal(senal$n_afectados, 100)

  # Con el determinante faltante en 200 filas vacias, esas no se explican.
  cat_b <- c(rep("A", 500), rep(NA, 200), rep("B", 100))
  y2 <- stats::runif(800)
  y2[is.na(cat_b) | cat_b %in% "B"] <- NA
  p2 <- suppressWarnings(perfilar(data.frame(cat = cat_b, y = y2),
                                  analizar_dependencias = FALSE))
  h2 <- as.data.frame(p2$hallazgos)
  senal2 <- h2[h2$tipo_hallazgo == "posible_ausencia_estructural", ]
  expect_equal(senal2$n_evaluados, 300)
  expect_equal(senal2$n_afectados, sum(is.na(y2) & cat_b %in% "B"))
  expect_equal(senal2$n_afectados, 100)
})

test_that("H5: una unidad no confirma una escala que solo dicen los valores", {
  peso_kg <- c(70, 82, 65, 91, 77, 58, 103, 66)
  attr(peso_kg, "units") <- "kg"
  peso_dec <- c(70.5, 82.1, 65.3, 91, 77.25, 58, 103.4, 66)
  attr(peso_dec, "units") <- "kg"
  hijos <- c(0, 1, 2, 3, 4, 2, 1, 0)
  attr(hijos, "measure") <- "scale"
  d <- data.frame(peso_kg = peso_kg, peso_dec = peso_dec, hijos = hijos)
  r <- clasificar_variables(d, proteger_datos_personales = FALSE)
  fila <- function(columna) r[r$columna == columna, , drop = FALSE]
  # A mano: todos enteros, asi que pueden ser una cuenta o una medida redondeada.
  expect_true(all(peso_kg == floor(peso_kg)))
  expect_equal(fila("peso_kg")$escala_propuesta, "discreta")
  expect_false(fila("peso_kg")$confirmada)
  expect_lt(fila("peso_kg")$confianza, 1)
  # Con decimales la unidad si decide: continua y confirmada.
  expect_equal(fila("peso_dec")$escala_propuesta, "continua")
  expect_true(fila("peso_dec")$confirmada)
  # `measure = "scale"` es una declaracion y se lee como la lee SPSS.
  expect_equal(fila("hijos")$escala_propuesta, "continua")
  expect_true(fila("hijos")$confirmada)
})

# Segunda vuelta 28-N -----------------------------------------------------------

veredicto_numeracion_r28n <- function(perfil, columna) {
  h <- as.data.frame(perfil$hallazgos)
  cb <- as.data.frame(perfil$cobertura_diagnosticos)
  fila <- perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
  c(
    identificador = any(h$columna %in% columna &
                          h$tipo_hallazgo == "posible_identificador"),
    densa = isTRUE(fila$secuencia_entera_densa),
    tukey_callado = any(cb$columna %in% columna & cb$diagnostico == "outliers" &
                          grepl("numeracion", cb$motivo, fixed = TRUE))
  )
}

unicidad_sin_moda_a_mano <- function(x) {
  conteos <- table(x)
  length(conteos) / (length(x) - max(conteos) + 1)
}

test_that("V1: la regla de numeracion es una y no depende de n", {
  # La densidad sola decidia tambien `posible_identificador` y las guardas de
  # Tukey, centinelas y patrones: una edad de 18 a 90 salia "codigo de
  # catalogo", y una magnitud acotada pasaba a numeracion al crecer la tabla.
  veredictos <- list()
  for (n in c(400L, 3000L)) {
    set.seed(28071)
    d <- data.frame(
      monto = floor(10^stats::runif(n, 0, 3.05)),
      edad = sample(18:90, n, TRUE),
      id = sample(n)
    )
    # A mano: la edad cubre su tramo de enteros y se repite; el id no se repite.
    expect_gt(length(unique(d$edad)) / 73, 0.9)
    expect_lt(unicidad_sin_moda_a_mano(d$edad), 0.9)
    expect_lt(unicidad_sin_moda_a_mano(d$monto), 0.9)
    expect_equal(unicidad_sin_moda_a_mano(d$id), 1)
    p <- suppressWarnings(perfilar(
      d, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
    veredictos[[as.character(n)]] <- lapply(
      stats::setNames(names(d), names(d)),
      function(columna) veredicto_numeracion_r28n(p, columna)
    )
  }
  esperado_magnitud <- c(identificador = FALSE, densa = FALSE,
                         tukey_callado = FALSE)
  for (n in names(veredictos)) {
    expect_equal(veredictos[[n]]$edad, esperado_magnitud, info = n)
    expect_equal(veredictos[[n]]$monto, esperado_magnitud, info = n)
    expect_true(veredictos[[n]]$id[["identificador"]], info = n)
    expect_true(veredictos[[n]]$id[["densa"]], info = n)
  }
  # La magnitud de r28n-12 con 200.000 filas, sobre la medida de la secuencia:
  # densa por cobertura, no por numeracion.
  set.seed(2812)
  x <- floor(10^stats::runif(200000L, 0, 4))
  secuencia <- .resumen_secuencia_entera(x, list(tipo = "entero"), NULL)
  expect_gt(secuencia$densidad, 0.8)
  expect_false(secuencia$densa)
  expect_true(.resumen_secuencia_entera(
    sample(200000L), list(tipo = "entero"), NULL
  )$densa)
})

test_that("V1: por DBI la misma columna da el mismo veredicto que en memoria", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(28073)
  d <- data.frame(edad = sample(18:90, 1500L, TRUE), id = sample(1500L))
  memoria <- suppressWarnings(perfilar(
    d, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  por_dbi <- suppressWarnings(perfilar_dbi(
    con, "t", muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  ))$perfil_muestra
  for (columna in names(d)) {
    expect_equal(veredicto_numeracion_r28n(por_dbi, columna),
                 veredicto_numeracion_r28n(memoria, columna), info = columna)
  }
  expect_false(veredicto_numeracion_r28n(por_dbi, "edad")[["identificador"]])
  expect_true(veredicto_numeracion_r28n(por_dbi, "id")[["identificador"]])
})

test_that("V2: lo que la aplicabilidad deja fuera sale de Benford y la aritmetica", {
  set.seed(28072)
  n <- 1200L
  tipo <- rep(c("A", "B"), times = c(1000L, 200L))
  monto <- round(exp(stats::rnorm(n, 8, 2.5)), 2)
  monto[tipo == "B"] <- 500
  p <- suppressWarnings(perfilar(
    data.frame(tipo = tipo, monto = monto),
    aplicabilidad = list(monto = ~ tipo == "A"), analizar_dependencias = FALSE
  ))
  r <- p$meta$benford$resultados$monto
  # A mano, sobre el universo que el resumen usa.
  expect_equal(p$columnas$media[p$columnas$columna == "monto"],
               mean(monto[tipo == "A"]))
  expect_equal(r$n_finitos, 1000L)
  expect_equal(r$estadistico, chi2_benford_a_mano(monto[tipo == "A"]),
               tolerance = 1e-9)
  expect_false(r$desviacion)

  a <- round(stats::runif(n, 10, 900), 2)
  b <- round(stats::runif(n, 10, 900), 2)
  total <- a + b
  b[tipo == "B"] <- round(stats::runif(200L, 1000, 5000), 2)
  dentro <- tipo == "A"
  expect_true(all(abs(total[dentro] - (a[dentro] + b[dentro])) <=
                    1e-8 * pmax(1, abs(total[dentro]))))
  p2 <- suppressWarnings(perfilar(
    data.frame(tipo = tipo, a = a, b = b, total = total),
    aplicabilidad = list(b = ~ tipo == "A"), analizar_dependencias = FALSE
  ))
  rel <- relaciones_aritmeticas_r28n(p2)
  expect_equal(rel$columna, "a,b,total")
  expect_equal(rel$n_evaluados, sum(dentro))
  expect_equal(rel$n_afectados, 0)
})

test_that("V2: la traza de outliers usa el universo declarado", {
  # Expuesto por V1: sin la guarda de densidad que callaba Tukey sobre enteros,
  # la traza de `outliers` -cuartiles de la columna entera- no coincidia con el
  # conteo -cuartiles del universo-. En decimales ya pasaba antes.
  set.seed(1)
  base <- round(stats::rnorm(950, 40, 8)) + 0.25
  datos <- data.frame(
    universo = c(rep(TRUE, 950), rep(FALSE, 50)),
    h = c(base, rep(9999, 30), rep(9998, 20))
  )
  cuartiles <- stats::quantile(base, c(0.25, 0.75), names = FALSE)
  iqr <- cuartiles[[2L]] - cuartiles[[1L]]
  a_mano <- sum(base < cuartiles[[1L]] - 1.5 * iqr |
                  base > cuartiles[[2L]] + 1.5 * iqr)
  expect_no_warning(p <- perfilar(
    datos, analizar_dependencias = FALSE, aplicabilidad = list(h = ~ universo)
  ))
  h <- as.data.frame(p$hallazgos)
  fila <- h[h$columna %in% "h" & h$tipo_hallazgo == "outliers", ]
  expect_equal(fila$n_afectados, a_mano)
  expect_equal(fila$trazabilidad[[1L]]$total, a_mano)
  expect_true(all(fila$trazabilidad[[1L]]$indices <= 950L))
})

test_that("V3: el numero que vuelve a su valor no depende de LC_NUMERIC", {
  anterior <- Sys.getlocale("LC_NUMERIC")
  on.exit(try(suppressWarnings(Sys.setlocale("LC_NUMERIC", anterior)),
              silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_NUMERIC", "de_DE.UTF-8"),
    error = function(e) NA_character_
  ))
  if (is.na(puesto) || !identical(puesto, "de_DE.UTF-8")) {
    skip("no se pudo fijar LC_NUMERIC=de_DE.UTF-8")
  }
  x <- c(1.5, 0.1 + 0.2, 1 / 3)
  expect_no_warning(etiquetas <- .etiqueta_numero_reversible(x))
  expect_identical(etiquetas[1:2], c("1.5", "0.30000000000000004"))
  expect_identical(as.numeric(etiquetas), x)
  expect_identical(.caracter_por_omision(1.5), "1.5")
  expect_no_warning(cifras <- .cifras_reversibles(x))
  expect_identical(cifras, c(NA, 17L, 16L))
})
