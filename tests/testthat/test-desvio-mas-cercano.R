# El desvio publicado es el doble mas cercano del desvio exacto (con n - 1), y
# la media lo es tambien con valores de mas de 2^900 junto a otros diminutos.
#
# El acumulador sacaba el desvio de Welford en doble: el centrado, los
# cuadrados y la fusion de bloques redondeaban cada uno, y el desvio quedaba a
# uno, dos o tres ulps del mas cercano, en memoria y por `bloque_filas`, sobre
# columnas sin nada raro. La media, con el exponente de sus partes subido por un
# valor de mas de 2^900, perdia los valores 2^1922 veces menores que el mayor.
#
# Las referencias se calcularon fuera de R, con fracciones exactas (`sd()` no es
# referencia), y se escriben en hexadecimal para comparar hasta el ultimo bit.
# Los vectores se arman con operaciones de base correctamente redondeadas, sin
# generador aleatorio ni funciones de `libm`, para que sean los mismos en toda
# plataforma.
#
# Tampoco con literales decimales de exponente grande: R los lee con
# `R_strtod()`, que acumula en `long double`, y en macOS sobre arm64 -donde ese
# tipo es un doble- `1e200` llega a tres ulps del mas cercano. El desvio de
# `d * 1e200` salia ahi otro, el exacto de esos otros valores. Las dos
# constantes van en hexadecimal, que se lee exacto en toda plataforma.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.dmc_1e200 <- 0x1.4e718d7d7625ap+664
.dmc_1e_menos_301 <- 0x1.124e63593f5e1p-1000

.dmc_columna <- function(perfil, campo) {
  as.numeric(perfil$columnas[[campo]][perfil$columnas$columna == "v"])
}

.dmc_memoria <- function(x, campo = "desvio") {
  .dmc_columna(suppressWarnings(perfilar(data.frame(v = x))), campo)
}

test_that("el desvio de una columna comun en memoria es el doble mas cercano", {
  # Las cuatro quedaban a un ulp.
  expect_identical(.dmc_memoria(((1:25) %% 7 - 3) * 0.3), 0x1.292e3c4e97e9cp-1)
  expect_identical(.dmc_memoria(((1:25) %% 11 - 5) * 0.3), 0x1.eb851eb851eb8p-1)
  expect_identical(
    .dmc_memoria(((1:25) %% 11 - 5) * 0.3 + 1000), 0x1.eb851eb851efep-1
  )
  expect_identical(
    .dmc_memoria(((1:40) %% 13 - 6) * 0.1 + 1000), 0x1.879acd87c399fp-2
  )
  expect_identical(.dmc_memoria(c(-2^51, 1, 2, 2^51, 3)), 0x1.6a09e667f3bcdp+50)
  # Controles: donde el desvio en doble ya era el mas cercano, sigue igual.
  expect_identical(.dmc_memoria(((1:25) %% 7 - 3) * 0.1), 0x1.8c3da5be1fe27p-3)
  expect_identical(.dmc_memoria(c(1, 2, 3, 4)), 0x1.4a7e9cb8a3491p+0)
})

test_that("el desvio de los bordes es el doble mas cercano", {
  d <- ((1:9) %% 4 - 1.5) / 3
  g <- (1:30 %% 7 - 3) * 0.1
  casos <- list(
    list(v = c(0.1, 0.3), desvio = 0x1.21a1851ff630ap-3),
    list(v = rep(0.1, 25), desvio = 0),
    list(v = c(-2^51, 1, 2, 2^51), desvio = 0x1.a20bd700c2c3ep+50),
    list(v = 2^53 - c(2, 4, 8, 10, 12, 30), desvio = 0x1.40a3ad2e3e392p+3),
    list(v = (1:50 %% 9) * 1.01 + 0.07, desvio = 0x1.47863d86a0311p+1),
    list(v = 2^52 + c(1, 5, 9, 13, 2, 7), desvio = 0x1.1f6823d085e6bp+2),
    # El cuadrado de 1e200 desborda.
    list(v = d * .dmc_1e200, desvio = 0x1.f811717a5a6aap+662),
    list(v = g * 2^500, desvio = 0x1.9ad5813c45737p+497),
    list(v = d * .dmc_1e_menos_301, desvio = 0x1.9d6e17385709bp-1002),
    # Subnormales: 2 y 23 veces 2^-1074.
    list(v = c(3, 5, 7) * 2^-1074, desvio = 2 * 2^-1074),
    list(v = (1:11 %% 5) * 2^-1070, desvio = 23 * 2^-1074)
  )
  for (caso in casos) {
    expect_identical(.dmc_memoria(caso$v), caso$desvio)
  }
})

test_that("el desvio por bloque_filas es el doble mas cercano", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  d <- ((1:9) %% 4 - 1.5) / 3
  casos <- list(
    # Por bloques de siete quedaban a uno y a dos ulps.
    list(v = ((1:25) %% 11 - 5) * 0.3, desvio = 0x1.eb851eb851eb8p-1),
    list(v = ((1:200) %% 7 - 3) * 0.1 + 1000, desvio = 0x1.980c5694878e0p-3),
    list(v = ((1:25) %% 7 - 3) * 0.1, desvio = 0x1.8c3da5be1fe27p-3),
    list(v = d * .dmc_1e200, desvio = 0x1.f811717a5a6aap+662),
    list(v = c(3, 5, 7) * 2^-1074, desvio = 2 * 2^-1074)
  )
  for (caso in casos) {
    DBI::dbWriteTable(con, "dmc", data.frame(v = caso$v), overwrite = TRUE)
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "dmc", bloque_muestra = "solo_agregados", bloque_filas = 7L,
      proteger_datos_personales = FALSE
    )))
    sql <- perfil$resumen_tabla$sql
    expect_identical(
      sql$estado[sql$columna == "v" & sql$metrica == "desvio"], "calculado"
    )
    expect_identical(
      as.numeric(perfil$resumen_tabla$columnas$desvio), caso$desvio
    )
  }
})

test_that("la manera rapida y la exacta publican lo mismo", {
  ejecutar <- function(x, modo, tamano = 10000L) {
    lupa:::.ejecutar_vector_bloques(
      x, "cuantitativos", tamano = tamano,
      configuracion = list(escala = 1, desvio = modo)
    )
  }
  vectores <- list(
    ((1:3000) %% 97 - 48) * 0.01 + 7,
    c(rep(0, 40), ((1:500) %% 13) * 0.25 + 1000),
    ((1:2000) %% 37) * 1.5 - 20
  )
  for (x in vectores) {
    for (tamano in c(7L, 10000L)) {
      rapida <- ejecutar(x, "rapido", tamano)
      exacta <- ejecutar(x, "exacto", tamano)
      expect_true(isTRUE(rapida$acumulador$desvio_decidido))
      expect_identical(
        rapida$resultado$resultado$desvio, exacta$resultado$resultado$desvio
      )
    }
  }
  # Una cota que no deja decidir no se publica como decidida, y la columna en
  # memoria se recorre otra vez por la manera exacta.
  rapida <- ejecutar(vectores[[1L]], "rapido")
  estado <- rapida$acumulador$estado_familia
  estado$cota_cuadrados <- 1e10
  expect_false(lupa:::.desvio_mas_cercano_bloques(estado)$decidido)
  # Un bloque fuera del rango de la manera rapida (1e200) va por la exacta.
  d <- ((1:9) %% 4 - 1.5) / 3
  rapida <- ejecutar(d * .dmc_1e200, "rapido")
  expect_false(isTRUE(rapida$acumulador$estado_familia$desvio_valido))
  expect_identical(
    lupa:::.resumen_cuantitativo_bloques(d * .dmc_1e200)$desvio,
    0x1.f811717a5a6aap+662
  )
  # Una columna constante decide 0 por la rapida, sin recorrerse otra vez.
  rapida <- ejecutar(rep(0.3, 30), "rapido", 7L)
  expect_true(isTRUE(rapida$acumulador$desvio_decidido))
  expect_identical(rapida$resultado$resultado$desvio, 0)
})

test_that("un desvio que cae en un punto medio va a par", {
  # Con n = 2 y `sum(x - c) = 0`, `W = 2 Q` y el desvio es `sqrt(Q)`. Con
  # `Q = (1 + 3 2^-53)^2` el exacto es el punto medio entre 1 + 2^-52 (impar)
  # y 1 + 2^-51 (par). Un poco de mas o de menos decide el lado.
  estado <- function(partes, cota = 0) {
    list(
      n = 2, desvio_modo = "exacto", desvio_valido = TRUE,
      centro_desvio = NA_real_, partes_suma = numeric(), exponente_partes = 0,
      partes_cuadrados = partes, exponente_cuadrados = 0,
      cota_cuadrados = cota
    )
  }
  medio <- c(1, 3 * 2^-52, 9 * 2^-106)
  decidir <- lupa:::.desvio_mas_cercano_bloques
  expect_identical(decidir(estado(medio)), list(valor = 1 + 2^-51,
                                                decidido = TRUE))
  expect_identical(decidir(estado(c(medio, -2^-300)))$valor, 1 + 2^-52)
  expect_identical(decidir(estado(c(medio, 2^-300)))$valor, 1 + 2^-51)
  # Con una cota que contiene el punto medio no se decide.
  expect_false(decidir(estado(medio, cota = 2^-300))$decidido)
})

test_that("fusionar dos estados da el desvio del recorrido entero", {
  x <- ((1:300) %% 11 - 5) * 0.3 + 1000
  partir <- function(y) {
    indices <- lupa:::.particionar_bloques(length(y), k = 2L)
    lapply(indices, function(bloque) {
      bloque$valores <- y[bloque$valores]
      bloque
    })
  }
  for (modo in c("rapido", "exacto")) {
    # La derecha empieza mas abajo: su centro es otro.
    y <- c(x[1:150], x[151:300] - 3.5)
    bloques <- partir(y)
    izquierda <- lupa:::.iniciar_acumulador(
      "x", "numeric", familia = "cuantitativos",
      configuracion = list(desvio = modo)
    )
    derecha <- lupa:::.iniciar_acumulador(
      "x", "numeric", familia = "cuantitativos",
      configuracion = list(desvio = modo)
    )
    lupa:::.absorber_acumulador(izquierda, bloques[[1L]])
    lupa:::.absorber_acumulador(derecha, bloques[[2L]])
    fusionada <- lupa:::.finalizar_acumulador(lupa:::.fusionar_acumuladores(izquierda, derecha))
    expect_identical(fusionada$resultado$desvio, 0x1.fd4af469faa46p+0,
                     info = modo)
  }
})

test_that("la media con valores 2^1922 veces menores es la exacta", {
  # Con 2^1000 en la columna el exponente de las partes es 100, y en esa unidad
  # los valores menores que 2^-922 caian en los subnormales: la media salia 0,
  # a 17 ulps, o el empate a par que el valor perdido rompia.
  d <- (1:25 %% 7) * 0.1 - 0.3
  casos <- list(
    list(v = c(2^1000, -2^1000, d * 2^-1000), media = -0x1.e573ac901e54ep-1008),
    list(v = c(2^1000, -2^1000, d * 2^-921), media = -0x1.e573ac901e54ep-929),
    list(v = c(2^1000, -2^1000, c(3, 5, 7) * 2^-1074), media = 3 * 2^-1074),
    # Sin el ultimo, la media es el punto medio entre 1 + 2^-52 y 1 + 2^-51
    # (por 2^1000), y el empate va a par, arriba; el -2^-1000 la lleva abajo.
    list(v = c(2^1001, 2^1001, 3 * 2^949, -2^-1000),
         media = 0x1.0000000000001p+1000),
    # Control: con +2^-1000 queda arriba, como el par.
    list(v = c(2^1001, 2^1001, 3 * 2^949, 2^-1000),
         media = 0x1.0000000000002p+1000)
  )
  for (caso in casos) {
    expect_identical(.dmc_memoria(caso$v, "media"), caso$media)
  }
  # Por bloques, con los diminutos antes que el mayor: el exponente sube a
  # mitad de camino y lo ya sumado pasa a la unidad nueva sin perder nada.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "dmc", data.frame(v = c(d * 2^-1000, 2^1000, -2^1000)))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "dmc", bloque_muestra = "solo_agregados", bloque_filas = 7L,
    proteger_datos_personales = FALSE
  )))
  expect_identical(
    as.numeric(perfil$resumen_tabla$columnas$media), -0x1.e573ac901e54ep-1008
  )
})
