# Senales redundantes, duplicados aproximados, distribucion de valores,
# asociaciones y estimaciones externas. Cada prueba rehace la cifra a
# mano; las que necesitan `stringdist` lo declaran.

# El valor citado de una columna en una evidencia `col=valor; ...`, sin la nota
# de escapes que puede seguir a la ultima fila citada.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r27d_citado <- function(evidencia, columna) {
  trozo <- regmatches(
    evidencia, regexpr(paste0(columna, "=[^;|]*"), evidencia)
  )
  trozo <- sub(" [[]valores escapados.*$", "", trozo)
  sub("[[:space:]]+$", "", sub(paste0("^", columna, "="), "", trozo))
}

test_that("una fila con el mismo infinito en todas las columnas concuerda", {
  d <- data.frame(a = c(1, Inf, 3, -Inf, 5), b = c(1, Inf, 4, Inf, 5))
  # A mano: `Inf == Inf` es TRUE y `-Inf == Inf` es FALSE: filas 3 y 4.
  a_mano <- which(d$a != d$b)
  expect_identical(a_mano, c(3L, 4L))

  senal <- senal_redundante(c("a", "b"))
  r <- detectar_discordancias(d, senal, proteger_datos_personales = FALSE)
  expect_identical(r$n_discordantes, 2)
  expect_equal(r$proporcion, 2 / 5)
  expect_match(r$evidencia, "fila 3", fixed = TRUE)
  expect_match(r$evidencia, "fila 4", fixed = TRUE)
  expect_no_match(r$evidencia, "fila 2", fixed = TRUE)

  # Sin citar filas no abortaba, pero contaba la fila 2 como discordante.
  r0 <- detectar_discordancias(
    d, senal, max_ejemplos = 0, proteger_datos_personales = FALSE
  )
  expect_identical(r0$n_discordantes, 2)

  solo <- detectar_discordancias(
    data.frame(a = c(1, Inf), b = c(1, Inf)), senal
  )
  expect_identical(solo$n_discordantes, 0)
})

test_that("una senal compara instantes, numeros y duraciones por valor", {
  # Zona fija para no depender de la base de husos de la maquina.
  t0 <- as.POSIXct("2024-03-01 12:00:00", tz = "UTC")
  t1 <- t0
  attr(t1, "tzone") <- "Etc/GMT+3"
  d <- data.frame(id = 1:3)
  d$utc <- t0 + c(0, 60, 120)
  d$local <- t1 + c(0, 60, 120)
  # A mano: el mismo instante en las tres filas.
  expect_identical(sum(as.numeric(d$utc) != as.numeric(d$local)), 0L)
  r <- detectar_discordancias(
    d, senal_redundante(c("utc", "local")), proteger_datos_personales = FALSE
  )
  expect_identical(r$n_discordantes, 0)

  # Un numero con todas sus cifras: 1e5 es el texto "100000"; 0.1 + 0.2 no es
  # el texto "0.3".
  n <- data.frame(x = c(1e5, 0.1 + 0.2), y = c("100000", "0.3"))
  rn <- detectar_discordancias(
    n, senal_redundante(c("x", "y")), proteger_datos_personales = FALSE
  )
  expect_identical(rn$n_discordantes, 1)
  expect_match(rn$evidencia, "fila 2", fixed = TRUE)

  # Duraciones: una hora y sesenta minutos son la misma; dos horas y 121
  # minutos, no.
  h <- data.frame(id = 1:2)
  h$horas <- as.difftime(c(1, 2), units = "hours")
  h$minutos <- as.difftime(c(60, 121), units = "mins")
  rh <- detectar_discordancias(
    h, senal_redundante(c("horas", "minutos")),
    proteger_datos_personales = FALSE
  )
  expect_identical(rh$n_discordantes, 1)
  expect_match(rh$evidencia, "fila 2", fixed = TRUE)
})

test_that("la evidencia no cita iguales dos valores que el veredicto separo", {
  t0 <- as.POSIXct("2024-03-01 12:00:00", tz = "UTC")
  d <- data.frame(id = 1:2)
  d$a <- c(t0, t0)
  d$b <- c(t0 + 0.3, t0)
  r <- detectar_discordancias(
    d, senal_redundante(c("a", "b")), proteger_datos_personales = FALSE
  )
  expect_identical(r$n_discordantes, 1)
  expect_false(identical(
    .r27d_citado(r$evidencia, "a"), .r27d_citado(r$evidencia, "b")
  ))

  # La tilde compuesta y la descompuesta: dos textos por la regla documentada,
  # y citados de forma que se vea por que.
  compuesta <- intToUtf8(c(99L, 97L, 102L, 233L))
  descompuesta <- intToUtf8(c(99L, 97L, 102L, 101L, 769L))
  expect_false(identical(charToRaw(compuesta), charToRaw(descompuesta)))
  t <- data.frame(x = c(compuesta, "y"), y = c(descompuesta, "y"))
  rt <- detectar_discordancias(
    t, senal_redundante(c("x", "y")), proteger_datos_personales = FALSE
  )
  expect_identical(rt$n_discordantes, 1)
  citado_x <- .r27d_citado(rt$evidencia, "x")
  citado_y <- .r27d_citado(rt$evidencia, "y")
  expect_false(identical(citado_x, citado_y))
  expect_identical(citado_x, "caf<U+00E9>")
  expect_identical(citado_y, "cafe<U+0301>")
  expect_match(rt$evidencia, "valores escapados", fixed = TRUE)

  # Control: una fila que se cita bien no se reescribe.
  c3 <- data.frame(a = c(1, 2), b = c(1, 3))
  rc <- detectar_discordancias(
    c3, senal_redundante(c("a", "b")), proteger_datos_personales = FALSE
  )
  expect_match(rc$evidencia, "fila 2: a=2; b=3", fixed = TRUE)
})

test_that("la barra invertida se escapa una vez y la distancia sale de la receta", {
  skip_if_not_installed("stringdist")
  barra <- rawToChar(as.raw(0x5c))
  v <- c(paste0("x", barra, "y"), paste0("x", barra, "z"), "x/y")
  d <- data.frame(v = v, stringsAsFactors = FALSE)
  r <- detectar_duplicados_aproximados(
    d, "v", normalizar = FALSE, umbral = 1, max_resultados = Inf,
    proteger_datos_personales = FALSE
  )
  par <- r$pares[r$pares$fila_1 == 1L & r$pares$fila_2 == 2L, ]
  # A mano, con la barra escrita doble como dice la receta: "x\\y" contra
  # "x\\z", cuatro caracteres y tres coincidencias en orden. Jaro =
  # (3/4 + 3/4 + 3/3) / 3 = 5/6; Winkler con prefijo comun 3 y p = 0.1:
  # distancia = (1 - 5/6) * (1 - 3 * 0.1).
  expect_equal(par$distancia, (1 - 5 / 6) * (1 - 0.3))
  # Y la receta del manual, con los dos escapes.
  escapar <- function(v) {
    gsub("|", "\\|", gsub("\\", "\\\\", v, fixed = TRUE), fixed = TRUE)
  }
  expect_equal(
    par$distancia,
    stringdist::stringdist(escapar(v[1]), escapar(v[2]), method = "jw", p = 0.1)
  )
  # Con el umbral por omision el par no entra: con la barra cuadruplicada
  # entraba a 0.067.
  omision <- detectar_duplicados_aproximados(
    d, "v", normalizar = FALSE, proteger_datos_personales = FALSE
  )
  expect_identical(nrow(omision$pares), 0L)

  # Varias columnas: la misma receta, pegada con " | ".
  dos <- data.frame(
    c1 = c(paste0("a", barra, "b"), paste0("a", barra, "c")),
    c2 = c("x | y", "x | y"), stringsAsFactors = FALSE
  )
  r2 <- detectar_duplicados_aproximados(
    dos, c("c1", "c2"), normalizar = FALSE, umbral = 1,
    proteger_datos_personales = FALSE
  )
  fila <- function(i) paste(escapar(dos$c1[i]), escapar(dos$c2[i]), sep = " | ")
  expect_equal(
    r2$pares$distancia[[1L]],
    stringdist::stringdist(fila(1), fila(2), method = "jw", p = 0.1)
  )
})

test_that("dos dobles que coinciden en 15 cifras no salen como el mismo valor", {
  skip_if_not_installed("stringdist")
  d <- data.frame(m = c(0.1 + 0.2, 0.3, 1e15, 1e15 + 1, 7))
  # A mano: ningun par de valores guardados iguales.
  expect_false(d$m[[1]] == d$m[[2]])
  expect_false(d$m[[3]] == d$m[[4]])
  r <- detectar_duplicados_aproximados(
    d, columnas = "m", normalizar = FALSE, umbral = 1, max_resultados = Inf,
    proteger_datos_personales = FALSE
  )
  expect_false(any(as.character(r$pares$tipo_par) == "exacto"))
  expect_false(any(r$pares$distancia == 0))
  expect_equal(r$alcance$n_pares_exactos, 0)
  par <- r$pares[r$pares$fila_1 == 1L & r$pares$fila_2 == 2L, ]
  expect_identical(par$evidencia_1, "m=0.30000000000000004")
  expect_identical(par$evidencia_2, "m=0.3")

  # Control: dos dobles iguales siguen siendo exactos, y un doble corriente se
  # sigue escribiendo como siempre.
  iguales <- detectar_duplicados_aproximados(
    data.frame(m = c(0.3, 0.3, 7)), columnas = "m", normalizar = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_identical(as.character(iguales$pares$tipo_par), "exacto")
  expect_identical(iguales$pares$evidencia_1, "m=0.3")
})

test_that("NA contra cadena vacia no se atribuye a la normalizacion", {
  skip_if_not_installed("stringdist")
  d <- data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz"),
    barrio = c(NA, "", "Centro"), stringsAsFactors = FALSE
  )
  r <- detectar_duplicados_aproximados(
    d, c("nombre", "barrio"), normalizar = FALSE,
    proteger_datos_personales = FALSE
  )
  # A mano: sin ningun paso de normalizacion activo, nada pudo igualar por ella.
  expect_identical(nrow(r$pares), 1L)
  expect_identical(as.character(r$pares$tipo_par), "exacto")
  expect_false(r$pares$igualo_normalizar)
  expect_equal(r$alcance$n_pares_exactos_normalizados, 0)

  # Control: lo que si iguala la normalizacion sigue atribuido a ella.
  n <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana Perez", "ana  perez"), stringsAsFactors = FALSE),
    "nombre", proteger_datos_personales = FALSE
  )
  expect_identical(as.character(n$pares$tipo_par), "exacto_normalizado")
})

test_that("una fila hecha solo de blancos no entra a la comparacion", {
  skip_if_not_installed("stringdist")
  d <- data.frame(v = c("   ", " ", "\t", "Ana", "Bea"), stringsAsFactors = FALSE)
  # A mano: dos filas con texto, y "ana"/"bea" no estan a 0.10.
  a_mano <- sum(nzchar(trimws(d$v)))
  expect_identical(a_mano, 2L)
  for (normalizar in list(NULL, FALSE)) {
    r <- detectar_duplicados_aproximados(
      d, "v", normalizar = normalizar, proteger_datos_personales = FALSE
    )
    expect_equal(r$alcance$n_filas_validas, a_mano)
    expect_identical(nrow(r$pares), 0L)
  }
  lsh <- detectar_duplicados_aproximados(
    d, "v", estrategia = "lsh", proteger_datos_personales = FALSE
  )
  expect_identical(nrow(lsh$pares), 0L)
})

test_that("sin stringdist, los conteos de comparar filas son NA y no cero", {
  testthat::local_mocked_bindings(
    .stringdist_disponible = function() FALSE, .package = "lupa"
  )
  d <- data.frame(
    v = c("Ana Perez", "Ana Perez", "Luis Diaz", "Bea", "Carlos", "Diana"),
    stringsAsFactors = FALSE
  )
  r <- detectar_duplicados_aproximados(d, "v", proteger_datos_personales = FALSE)
  expect_false(r$disponible)
  campos <- c(
    "n_filas_validas", "n_pares_hallados", "n_pares_exactos",
    "n_pares_exactos_normalizados", "n_pares_aproximados"
  )
  expect_true(all(is.na(unlist(r$alcance[campos]))))
  # Lo que si se sabe: ningun par comparado, los 15 posibles sin comparar.
  expect_equal(r$alcance$n_pares_comparados, 0)
  expect_equal(r$alcance$n_pares_sin_comparar, 6 * 5 / 2)
})

test_that("con las filas recorridas y menos de dos validas, cero pares es lo medido", {
  skip_if_not_installed("stringdist")
  r <- detectar_duplicados_aproximados(
    data.frame(v = c("Ana", NA, ""), stringsAsFactors = FALSE), "v",
    proteger_datos_personales = FALSE
  )
  expect_equal(r$alcance$n_filas_validas, 1)
  expect_equal(r$alcance$n_pares_hallados, 0)
  expect_equal(r$alcance$n_pares_exactos, 0)
})

test_that("distribucion_valores cuenta dobles por valor, como perfilar()", {
  d <- data.frame(x = c(0.1 + 0.2, 0.3, 0.3, 1e15, 1e15 + 1))
  r <- distribucion_valores(d, proteger_datos_personales = FALSE)
  f <- r$frecuencias
  # A mano: cuatro valores distintos; 0.3 aparece dos veces en cinco.
  expect_identical(length(unique(d$x)), 4L)
  expect_equal(r$alcance$n_distintos_muestra, 4)
  expect_identical(nrow(f), 4L)
  expect_identical(f$frecuencia, c(2L, 1L, 1L, 1L))
  expect_equal(f$proporcion[[1L]], 2 / 5)
  expect_identical(f$valor[[1L]], "0.3")
  # Cada fila publicada vuelve a su valor: ninguna escritura repetida.
  expect_identical(anyDuplicated(f$valor), 0L)
  expect_true(all(as.numeric(f$valor) %in% d$x))
  # Y lo mismo que cuenta perfilar() sobre la misma tabla.
  p <- perfilar(d, proteger_datos_personales = FALSE)
  expect_equal(p$columnas$n_distintos, 4)
  expect_equal(p$columnas$frecuencia_moda, 2)
})

test_that("en distribucion_valores un empate no depende del orden de las filas", {
  t1 <- data.frame(v = c("b", "b", "a", "a", "c"), stringsAsFactors = FALSE)
  t2 <- t1[c(3, 4, 1, 2, 5), , drop = FALSE]
  # A mano: a y b empatan con 2; por valor, "a" va primero.
  expect_identical(as.integer(table(t1$v)[c("a", "b")]), c(2L, 2L))
  uno <- distribucion_valores(t1, max_valores = 1, proteger_datos_personales = FALSE)
  dos <- distribucion_valores(t2, max_valores = 1, proteger_datos_personales = FALSE)
  expect_identical(uno$frecuencias$valor, "a")
  expect_identical(dos$frecuencias$valor, "a")
  # El mismo desempate que la moda de perfilar().
  expect_identical(
    as.character(perfilar(t2, proteger_datos_personales = FALSE)$columnas$moda),
    "a"
  )
  # Los numeros se desempatan por valor, no por su texto ("10" < "9").
  n1 <- data.frame(x = c(10, 10, 9, 9, 2))
  n2 <- n1[c(3, 4, 1, 2, 5), , drop = FALSE]
  expect_identical(
    distribucion_valores(n1, max_valores = 1)$frecuencias$valor, "9"
  )
  expect_identical(
    distribucion_valores(n2, max_valores = 1)$frecuencias$valor, "9"
  )
})

test_that("eta cuadrado no se publica con un solo grupo en el par", {
  d <- data.frame(
    y = c(3:8, NA, NA, NA),
    z = c("u", "v", "u", "v", "u", "v", NA, NA, NA),
    h = c(rep("p", 6), rep("q", 3)), stringsAsFactors = FALSE
  )
  # A mano: en las filas con `y`, `h` toma un solo valor.
  expect_identical(unique(d$h[!is.na(d$y)]), "p")
  r <- detectar_asociaciones(d, umbral = 0)
  publicados <- paste(r$columna_1, r$columna_2)
  expect_false(any(publicados %in% c("y h", "h y")))
  omitidos <- attr(r, "pares_omitidos_medicion")
  expect_true(any(
    paste(omitidos$columna_1, omitidos$columna_2) %in% c("y h", "h y")
  ))
  # Control: con dos grupos se mide, y es la razon entre sumas de cuadrados.
  x <- d$y[1:6]
  g <- d$z[1:6]
  entre <- sum(tapply(x, g, function(v) length(v) * (mean(v) - mean(x))^2))
  total <- sum((x - mean(x))^2)
  yz <- r[r$columna_1 == "y" & r$columna_2 == "z", ]
  expect_equal(yz$asociacion, entre / total)
})

test_that("dos tabulados de la misma entidad no comparten id_medida", {
  f <- as.POSIXct("2024-06-30", tz = "UTC")
  m1 <- medicion_desde_estimaciones(
    data.frame(region = c("Norte", "Sur"), stat = c(0.4, 0.3), cv = c(0.1, 0.3)),
    "ech2024", "survey", atributo = "region", fecha = f
  )
  m2 <- medicion_desde_estimaciones(
    data.frame(sexo = c("Mujer", "Varon"), stat = c(0.5, 0.2), cv = c(0.05, 0.25)),
    "ech2024", "survey", atributo = "sexo", fecha = f
  )
  # A mano: cuatro medidas en cada tabulado y ninguna es la misma.
  expect_length(intersect(m1$id_medida, m2$id_medida), 0L)
  unidas <- rbind(m1, m2)
  expect_identical(length(unique(unidas$id_medida)), 8L)
  # El id nombra lo que mide.
  expect_true(all(grepl("Norte|Sur", m1$id_medida)))
  regla <- regla_evaluacion(
    "cv", function(x, maximo) x <= maximo,
    metricas = "CoeficienteVariacion@ech2024", umbrales = list(maximo = 0.2)
  )
  ev <- evaluar(unidas, perfil_evaluacion("p", regla))
  # A mano: cv <= 0.2 en Norte (0.1) y en Mujer (0.05): dos de cuatro.
  expect_equal(ev$reglas$n_medidas, 4)
  expect_equal(ev$reglas$resultado, 0.5)
  # Control: la misma medida unida consigo misma se sigue rechazando.
  expect_error(evaluar(rbind(m1, m1), perfil_evaluacion("p", regla)), "repite")
})

test_that("medicion_desde_estimaciones publica una medicion que acumula el historico", {
  f <- as.POSIXct("2024-06-30", tz = "UTC")
  m1 <- medicion_desde_estimaciones(
    data.frame(region = c("Norte", "Sur"), stat = c(0.4, 0.3)),
    "ech2024", "survey", atributo = "region", fecha = f
  )
  m2 <- medicion_desde_estimaciones(
    data.frame(region = c("Norte", "Sur"), stat = c(0.41, 0.33)),
    "ech2024", "survey", atributo = "region", fecha = f + 86400
  )
  expect_s3_class(m1, "medicion")
  expect_s3_class(m1, "medicion_calidad")
  h <- historico_calidad(m1, m2)
  expect_s3_class(h, "historico_calidad")
})

test_that("una celda infinita es un numero fuera de dominio, no un texto", {
  expect_warning(
    m <- medicion_desde_estimaciones(
      data.frame(stat = c(1, Inf), se = c(0.1, 0.2)), "e", "s"
    ),
    "1 fuera del dominio", fixed = TRUE
  )
  descartadas <- attr(m, "celdas_descartadas")
  expect_identical(descartadas$valor, "Inf")
  expect_identical(descartadas$motivo, "fuera_de_dominio")
  # Control: un texto que no es un numero sigue siendo `no_numerica`.
  expect_warning(
    t <- medicion_desde_estimaciones(
      data.frame(stat = c("1", "abc"), stringsAsFactors = FALSE), "e", "s"
    ),
    "fuera del dominio", fixed = TRUE
  )
  expect_identical(attr(t, "celdas_descartadas")$motivo, "no_numerica")
})
