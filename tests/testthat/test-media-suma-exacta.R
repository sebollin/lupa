# La media publicada es el doble mas cercano de la media exacta.
#
# El acumulador por bloques la sacaba de Welford, centrada en la media del
# primer bloque: restarle un centro con fraccion a valores cerca de 2^51 pierde
# esa fraccion, y `c(-2^51, 1, 2, 2^51)` publicaba 0,6875 por 0,75. Lo mismo en
# `perfil_muestra` -la corroboracion lo contaba como divergencia contra el
# `AVG` del motor-, en `bloque_filas` y con `integer64`. Y sin cancelacion
# tambien: columnas comunes quedaban a uno u ocho ulps.
#
# Las referencias se calcularon fuera de R, con fracciones exactas (`mean()`
# no es referencia: acumula en `long double` donde lo hay), y se escriben en
# hexadecimal para comparar hasta el ultimo bit. Los vectores se arman con
# operaciones de base correctamente redondeadas, sin generador aleatorio ni
# funciones de `libm`, para que sean los mismos en toda plataforma.

.mse_media <- function(perfil) {
  as.numeric(perfil$columnas$media[perfil$columnas$columna == "v"])
}

test_that("la media de valores que se cancelan es la exacta", {
  casos <- list(
    list(v = c(-2^51, 1, 2, 2^51), media = 0.75),
    list(v = c(-2^51, 1, 2, 2^51, 3), media = 0x1.3333333333333p+0),
    list(v = c(2^51, 1, 2, -2^51, 3), media = 0x1.3333333333333p+0),
    list(v = c(-2^52, 1, 2, 2^52, 3), media = 0x1.3333333333333p+0),
    list(v = c(-2^53, 1, 3, 2^53), media = 1),
    # Cuatro bloques: la media cambiaba de signo.
    list(v = c(rep(2^51, 15000), rep(-2^51, 15000), 1:3),
         media = 0x1.a3637230afb37p-13)
  )
  for (caso in casos) {
    perfil <- suppressWarnings(perfilar(data.frame(v = caso$v)))
    expect_identical(.mse_media(perfil), caso$media)
  }
})

test_that("la media de una columna comun es el doble mas cercano", {
  # `mean()` da los dos a un ulp de la exacta, igual que main.
  d <- (1:25 %% 7) * 0.1 - 0.3
  g <- ((1:60) %% 13 - 6.5) / 9
  expect_identical(
    .mse_media(suppressWarnings(perfilar(data.frame(v = d)))),
    -0x1.0624dd2f1a9e8p-7
  )
  expect_identical(
    .mse_media(suppressWarnings(perfilar(data.frame(v = g)))),
    -0x1.3e93e93e93e94p-4
  )
  # El control: donde la media en doble ya era la exacta, sigue igual.
  expect_identical(
    .mse_media(suppressWarnings(perfilar(data.frame(v = c(1, 2, 3, 4))))), 2.5
  )
  expect_identical(
    .mse_media(suppressWarnings(perfilar(data.frame(v = c(0.1, 0.2))))),
    0x1.3333333333334p-3
  )
})

test_that("la media de valores diminutos o junto a uno enorme es la exacta", {
  # Por debajo de 1e-150 la escala va por la dispersion y los momentos se
  # desplazan al minimo: la media salia de `media * escala + desplazamiento`,
  # que cancela (las dos primeras, a 4 y 8 ulps). Por encima de 1e150 los valores
  # se dividian por la escala del mayor, y los de 2^-100 junto a 2^1000 caian
  # bajo el menor doble: la media salia 0.
  d <- (1:25 %% 7) * 0.1 - 0.3
  g <- ((1:60) %% 13 - 6.5) / 9
  chicos <- ((1:9) %% 4 - 1.5) / 3 * 2^-100
  casos <- list(
    list(v = d * 2^-1000, media = -0x1.0624dd2f1a9e8p-1007),
    list(v = g * 2^-1000, media = -0x1.3e93e93e93e94p-1004),
    list(v = c(2^1000, -2^1000, chicos), media = -0x1.f07c1f07c1f07p-107),
    list(v = c(2^1000, chicos, -2^1000), media = -0x1.f07c1f07c1f07p-107),
    # Controles: medias subnormales, con el empate a par en la grilla de
    # 2^-1074 (1,5 va a 2; 1/3 va a 0).
    list(v = g * 2^-1060, media = -0x0.00000000004fap-1022),
    list(v = c(3, 0) * 2^-1074, media = 0x0.0000000000002p-1022),
    list(v = c(1, 0, 0) * 2^-1074, media = 0)
  )
  for (caso in casos) {
    perfil <- suppressWarnings(perfilar(data.frame(v = caso$v)))
    expect_identical(.mse_media(perfil), caso$media)
  }
})

test_that("la media de un integer64 que se cancela es la exacta", {
  skip_if_not_installed("bit64")
  v <- bit64::as.integer64(c(
    "-2251799813685248", "1", "2", "2251799813685248"
  ))
  perfil <- suppressWarnings(perfilar(data.frame(v = v)))
  expect_identical(.mse_media(perfil), 0.75)
})

test_that("la media de la muestra coincide con la del motor y no diverge", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE mse_t (v BIGINT, w DOUBLE)")
  DBI::dbExecute(con, paste(
    "INSERT INTO mse_t VALUES (-2251799813685248, -2251799813685248),",
    "(1, 1), (2, 2), (2251799813685248, 2251799813685248), (3, 3)"
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "mse_t", proteger_datos_personales = FALSE
  )))
  muestra <- perfil$perfil_muestra$columnas
  resumen <- perfil$resumen_tabla$columnas
  for (columna in c("v", "w")) {
    expect_identical(
      as.numeric(muestra$media[muestra$columna == columna]),
      0x1.3333333333333p+0
    )
    expect_identical(
      as.numeric(resumen$media[resumen$columna == columna]),
      0x1.3333333333333p+0
    )
  }
  corroboracion <- perfil$resumen_tabla$meta$corroboracion_bloques
  expect_identical(as.integer(corroboracion$divergencias), 0L)
})

test_that("la media por bloque_filas es la exacta", {
  testthat::skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE mse_b (v INTEGER, i INTEGER)")
  enteros <- c(-2251799813685248, 1, 2, 2251799813685248, 3)
  # Cuarenta enteros chicos: por bloques de siete la media salia a ocho ulps.
  chicos <- c(-44L, -29L, 15L, -12L, -9L, 47L, -9L, 1L, -27L, -23L, -33L,
              -7L, -11L, -10L, 35L, -35L, -22L, 37L, 13L, -13L, -14L, 10L,
              25L, -14L, 46L, 31L, -30L, 33L, -13L, 38L, 0L, -28L, -48L, 44L,
              -20L, 0L, 10L, -19L, 47L, 8L)
  DBI::dbExecute(con, paste0(
    "INSERT INTO mse_b VALUES ",
    paste0("(", c(sprintf("%.0f", enteros), rep("NULL", 35)), ", ",
           chicos, ")", collapse = ", ")
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "mse_b", bloque_muestra = "solo_agregados", bloque_filas = 7L,
    proteger_datos_personales = FALSE
  )))
  resumen <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  expect_identical(
    sql$estado[sql$columna == "v" & sql$metrica == "media"], "calculado"
  )
  expect_identical(
    as.numeric(resumen$media[resumen$columna == "v"]), 0x1.3333333333333p+0
  )
  expect_identical(
    as.numeric(resumen$media[resumen$columna == "i"]), -0.75
  )
  # Valores diminutos por bloques: la escala se adapta en cada bloque.
  DBI::dbExecute(con, "CREATE TABLE mse_c (v REAL)")
  d <- (1:25 %% 7) * 0.1 - 0.3
  DBI::dbWriteTable(con, "mse_c", data.frame(v = d * 2^-1000), append = TRUE)
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "mse_c", bloque_muestra = "solo_agregados", bloque_filas = 7L,
    proteger_datos_personales = FALSE
  )))
  expect_identical(
    as.numeric(perfil$resumen_tabla$columnas$media), -0x1.0624dd2f1a9e8p-1007
  )
})
