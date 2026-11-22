# `orden_texto` elige como se ordena el texto en las reglas que el paquete
# interpreta con un operador de orden: el rango de
# `ValoresPosiblesPorComprension` y `<`, `<=`, `>`, `>=` en la formula de
# `ReglaIntegridadIntraEntidad`.
#
# Con el orden del locale, la misma regla sobre los mismos datos daba otro
# numero en otra sesion -3 de 8 bajo `es_UY.UTF-8` y 2 de 8 bajo `C`-, y
# `medir_dbi()` la declaraba `no_medible` porque ningun motor reproduce el
# locale de R. Con `"bytes"` el orden es el del UTF-8 -el del punto de codigo-:
# el mismo en toda sesion, y el que un motor sabe reproducir.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.ot_enie <- intToUtf8(241L)

# Un locale cuya intercalacion no es la de `C`, como en
# test-desempate-independiente-del-locale.R: no toda maquina tiene los mismos.
.ot_locale_no_c <- function() {
  anterior <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", anterior)), add = TRUE)
  for (candidato in c("es_UY.UTF-8", "es_ES.UTF-8", "en_US.UTF-8", "C.UTF-8")) {
    puesto <- suppressWarnings(Sys.setlocale("LC_COLLATE", candidato))
    if (nzchar(puesto) && !identical(sort(c("a", "A")), c("A", "a"))) {
      return(candidato)
    }
  }
  NULL
}

.ot_con_colacion <- function(locale, expr) {
  anterior <- Sys.getlocale("LC_COLLATE")
  cambiado <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", anterior)), add = TRUE)
  if (!nzchar(cambiado)) return(NULL)
  force(expr)
}

.ot_datos <- function() {
  data.frame(
    t = c("a", "B", "z", "Z", .ot_enie, "m", "A", "b"),
    stringsAsFactors = FALSE
  )
}

.ot_fecha <- as.POSIXct("2026-04-01", tz = "UTC")

.ot_modelo <- function() {
  nuc <- metricas_nucleo()
  modelo(
    instanciar(especializar(
      nuc$ValoresPosiblesPorComprension, "Rango", minimo = "a", maximo = "b"
    ), "d", "t"),
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "Mayor", regla = ~ t > "z"
    ), "d", "t"),
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "Menor", regla = ~ t < "a"
    ), "d", "t")
  )
}

.ot_cuenta <- function(medicion) {
  x <- as.data.frame(medicion)
  vapply(c("Rango@d.t", "Mayor@d.t", "Menor@d.t"), function(nombre) {
    sum(x$resultado[x$metrica_instanciada == nombre])
  }, numeric(1L))
}

test_that("con orden_texto = 'bytes' la regla de texto da lo mismo en cualquier locale", {
  locale <- .ot_locale_no_c()
  skip_if(is.null(locale), "no hay un locale con otra intercalacion que C")
  medir_en <- function(loc) {
    .ot_con_colacion(loc, .ot_cuenta(medir(
      .ot_modelo(), list(d = .ot_datos()), id_medicion = "x",
      fecha = .ot_fecha, orden_texto = "bytes"
    )))
  }
  en_locale <- medir_en(locale)
  en_c <- medir_en("C")
  # El orden del punto de codigo: en [a, b] entran "a" y "b"; "Z", "B" y "A"
  # van antes que "a"; la enie va despues de "z".
  esperado <- c("Rango@d.t" = 2, "Mayor@d.t" = 1, "Menor@d.t" = 3)
  expect_identical(en_locale, esperado)
  expect_identical(en_c, esperado)
})

test_that("con 'bytes' la enie va despues de z y Z antes que a, celda por celda", {
  m <- medir(
    .ot_modelo(), list(d = .ot_datos()), id_medicion = "x", fecha = .ot_fecha,
    orden_texto = "bytes"
  )
  x <- as.data.frame(m)
  mayor <- x[x$metrica_instanciada == "Mayor@d.t", , drop = FALSE]
  menor <- x[x$metrica_instanciada == "Menor@d.t", , drop = FALSE]
  expect_identical(as.numeric(mayor$resultado[mayor$fila == 5L]), 1)
  expect_identical(as.numeric(mayor$resultado[mayor$fila == 3L]), 0)
  expect_identical(as.numeric(menor$resultado[menor$fila == 4L]), 1)
  expect_identical(as.numeric(menor$resultado[menor$fila == 1L]), 0)
})

test_that("'idioma' es la medicion de siempre y la que se usa por omision", {
  locale <- .ot_locale_no_c()
  skip_if(is.null(locale), "no hay un locale con otra intercalacion que C")
  .ot_con_colacion(locale, {
    por_omision <- medir(.ot_modelo(), list(d = .ot_datos()),
                         id_medicion = "x", fecha = .ot_fecha)
    idioma <- medir(.ot_modelo(), list(d = .ot_datos()), id_medicion = "x",
                    fecha = .ot_fecha, orden_texto = "idioma")
    bytes <- medir(.ot_modelo(), list(d = .ot_datos()), id_medicion = "x",
                   fecha = .ot_fecha, orden_texto = "bytes")
    expect_identical(idioma, por_omision)
    # En un locale que no es el de `C` el orden del idioma es otro: si diera lo
    # mismo, la prueba no distinguiria nada.
    expect_false(identical(.ot_cuenta(idioma), .ot_cuenta(bytes)))
  })
  expect_error(
    medir(.ot_modelo(), list(d = .ot_datos()), orden_texto = "locale"),
    "orden_texto"
  )
})

test_that("'bytes' solo cambia donde R compara texto", {
  nuc <- metricas_nucleo()
  d <- data.frame(
    n = c(10, 4, 6),
    alta = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
    baja = as.Date(c("2026-01-15", "2026-01-15", "2026-03-01"))
  )
  m <- modelo(
    # Un numero contra un texto: R compara el texto del numero, "10" < "5".
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "NumTxt", regla = ~ n < "5"
    ), "d", "n"),
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "Fechas", regla = ~ alta <= baja
    ), "d", c("alta", "baja")),
    instanciar(especializar(
      nuc$ValoresPosiblesPorComprension, "Num", minimo = 5, maximo = 9
    ), "d", "n")
  )
  idioma <- as.data.frame(medir(m, list(d = d), id_medicion = "x", fecha = .ot_fecha))
  bytes <- as.data.frame(medir(m, list(d = d), id_medicion = "x", fecha = .ot_fecha,
                               orden_texto = "bytes"))
  expect_identical(bytes$resultado[bytes$metrica_instanciada == "NumTxt@d.n"],
                   c(1, 1, 0))
  # Las columnas, no los atributos: el sello de `NumTxt` cambia con el orden,
  # que es lo que mide (ver test-sello-medicion.R).
  expect_identical(
    lapply(bytes[bytes$metrica_instanciada != "NumTxt@d.n", ], identity),
    lapply(idioma[idioma$metrica_instanciada != "NumTxt@d.n", ], identity)
  )
})

test_that("el resumen en memoria ordena igual que la medicion completa", {
  completa <- medir(.ot_modelo(), list(d = .ot_datos()), id_medicion = "x",
                    fecha = .ot_fecha, orden_texto = "bytes")
  resumen <- as.data.frame(medir(
    .ot_modelo(), list(d = .ot_datos()), id_medicion = "x", fecha = .ot_fecha,
    orden_texto = "bytes", detalle = "resumen"
  ))
  expect_identical(
    stats::setNames(resumen$numerador, resumen$metrica_instanciada)[
      names(.ot_cuenta(completa))
    ],
    .ot_cuenta(completa)
  )
})

.ot_comparar_motor <- function(con, modelo_d, datos, ...) {
  DBI::dbWriteTable(con, "d", datos, overwrite = TRUE)
  memoria <- as.data.frame(medir(
    modelo_d, list(d = datos), id_medicion = "x", fecha = .ot_fecha,
    orden_texto = "bytes", detalle = "resumen"
  ))
  motor <- as.data.frame(medir_dbi(
    modelo_d, con, id_medicion = "x", fecha = .ot_fecha,
    orden_texto = "bytes", ...
  ))
  columnas <- c("metrica_instanciada", "numerador", "denominador", "resultado")
  memoria <- memoria[order(memoria$metrica_instanciada), columnas]
  motor <- motor[order(motor$metrica_instanciada), columnas]
  rownames(memoria) <- rownames(motor) <- NULL
  list(memoria = memoria, motor = motor)
}

test_that("SQLite y DuckDB miden por bytes lo mismo que la memoria", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  ascii <- data.frame(t = c("a", "B", "z", "Z", "m", "A", "b", "c"),
                      stringsAsFactors = FALSE)
  # SQLite: ASCII en el rango -con texto no ASCII su guarda declara la columna,
  # ver la prueba que sigue- y no ASCII en la formula.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  r <- .ot_comparar_motor(con, .ot_modelo(), ascii)
  expect_identical(nrow(r$motor), 3L)
  expect_identical(r$motor, r$memoria)
  formulas <- .ot_modelo()
  formulas$metricas <- formulas$metricas[2:3]
  r <- .ot_comparar_motor(con, formulas, .ot_datos())
  expect_identical(nrow(r$motor), 2L)
  expect_identical(r$motor, r$memoria)

  dk <- DBI::dbConnect(.duckdb_prueba(), ":memory:")
  on.exit(DBI::dbDisconnect(dk, shutdown = TRUE), add = TRUE)
  for (datos in list(ascii, .ot_datos())) {
    r <- .ot_comparar_motor(dk, .ot_modelo(), datos)
    expect_identical(nrow(r$motor), 3L)
    expect_identical(r$motor, r$memoria)
  }
})

test_that("sin 'bytes' la regla de texto sigue no_medible en el motor, y con 'bytes' no", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  ascii <- data.frame(t = c("a", "B", "z"), stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", ascii)
  dk <- DBI::dbConnect(.duckdb_prueba(), ":memory:")
  on.exit(DBI::dbDisconnect(dk, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(dk, "d", ascii)
  for (conexion in list(con, dk)) {
    expect_true(all(plan_medicion_dbi(.ot_modelo(), conexion)$via == "no_medible"))
    expect_true(all(
      plan_medicion_dbi(.ot_modelo(), conexion, orden_texto = "bytes")$via ==
        "sql_consolidado"
    ))
  }
})

test_that("en SQLite el rango con texto no ASCII se mide por bloques, por bytes", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rango <- .ot_modelo()
  rango$metricas <- rango$metricas[1L]
  sin_bloques <- suppressWarnings(.ot_comparar_motor(con, rango, .ot_datos()))
  expect_identical(nrow(sin_bloques$motor), 0L)
  r <- .ot_comparar_motor(con, rango, .ot_datos(), estrategia_no_sql = "bloques")
  expect_identical(r$motor, r$memoria)
  expect_identical(r$motor$numerador, 2)
})

test_that("una base SQLite en UTF-16 no traduce el orden por bytes", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA encoding = 'UTF-16le'")
  DBI::dbWriteTable(con, "d", data.frame(t = c("a", "B", "z"), stringsAsFactors = FALSE))
  plan <- plan_medicion_dbi(.ot_modelo(), con, orden_texto = "bytes")
  expect_true(all(plan$via == "no_medible"))
  expect_true(all(grepl("UTF-16", plan$motivo, fixed = TRUE)))
})

test_that("con texto que no es UTF-8 valido, memoria y motor dan los mismos numeros", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  latin <- iconv(paste0("B", intToUtf8(225L), "sico"), from = "UTF-8", to = "latin1")
  Encoding(latin) <- "latin1"
  malo <- rawToChar(as.raw(c(0x61, 0xC3, 0x28, 0x62)))
  Encoding(malo) <- "bytes"
  # Un primer byte invalido: como texto escapado iria antes de "a"; por sus
  # bytes va despues de "z".
  primero_malo <- rawToChar(as.raw(c(0xFF, 0x61)))
  datos <- data.frame(t = c(latin, "Basico", malo, "a", primero_malo, "m"),
                      stringsAsFactors = FALSE)
  nuc <- metricas_nucleo()
  rango <- modelo(instanciar(especializar(
    nuc$ValoresPosiblesPorComprension, "R", minimo = "a", maximo = "z"
  ), "d", "t"))
  formula <- modelo(
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "Mayor", regla = ~ t > "a"
    ), "d", "t"),
    instanciar(especializar(
      nuc$ReglaIntegridadIntraEntidad, "Menor", regla = ~ t < "z"
    ), "d", "t")
  )
  dk <- DBI::dbConnect(.duckdb_prueba(), ":memory:")
  on.exit(DBI::dbDisconnect(dk, shutdown = TRUE), add = TRUE)
  r <- suppressWarnings(.ot_comparar_motor(dk, rango, datos))
  expect_identical(r$motor$denominador, 4)
  expect_identical(r$motor, r$memoria)
  expect_identical(
    plan_medicion_dbi(rango, dk, orden_texto = "bytes")$via, "sql_consolidado"
  )
  r <- suppressWarnings(.ot_comparar_motor(dk, formula, datos))
  expect_identical(r$motor, r$memoria)
  # `\xffa` > "a" por sus bytes, y no < "z".
  expect_identical(r$motor$numerador, c(3, 5))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  r <- suppressWarnings(.ot_comparar_motor(con, formula, datos))
  expect_identical(r$motor, r$memoria)
})
