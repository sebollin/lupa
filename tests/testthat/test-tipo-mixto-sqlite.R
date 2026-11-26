# SQLite guarda numeros y texto en la misma columna, y su controlador tipa la
# columna por la primera fila que llega: con la tabla en otro orden, o con una
# muestra al azar, el mismo `'ND'` se leia como `0` o como texto, y la cifra
# publicada cambiaba de una corrida a otra sin decirlo. Lo que se lee de una
# columna asi tiene que ser lo mismo en cualquier orden, y si no se puede leer sin
# perder valores, se declara.
testthat::skip_on_cran()
testthat::skip_if_not_installed("RSQLite")

.tms_orden_a <- c("-9", "1", "2", "'ND'", "'nd'", "NULL")
.tms_orden_b <- c("'ND'", "'nd'", "-9", "1", "2", "NULL")
.tms_orden_c <- c("NULL", "'ND'", "-9", "1", "2", "'nd'")

.tms_con <- function(valores, tipo = "") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, paste0("CREATE TABLE t (x ", tipo, ")"))
  for (valor in valores) {
    DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", valor, ")"))
  }
  con
}

.tms_perfilar <- function(valores, ...) {
  con <- .tms_con(valores)
  on.exit(DBI::dbDisconnect(con))
  avisos <- character()
  r <- withCallingHandlers(
    perfilar_dbi(
      con, "t", cadenas_ausencia = "ND",
      sentinelas_numericos = -9, ...
    ),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(r = r, avisos = avisos)
}

test_that("la muestra del motor lee igual una columna mixta en cualquier orden", {
  for (corrida in 1:4) {
    for (orden in list(.tms_orden_a, .tms_orden_b, .tms_orden_c)) {
      l <- .tms_perfilar(orden, universo = "muestra_motor", muestra_motor = 6)
      col <- l$r$resumen_tabla$columnas
      # 'ND', 'nd' y el -9 declarado: tres faltantes disfrazados, y ninguna moda
      # inventada (`0` no esta en la tabla).
      expect_equal(col$n_faltantes_disfrazados, 3)
      expect_true(is.na(col$moda))
      expect_equal(col$n_distintos, 5)
      cob <- l$r$resumen_tabla$cobertura
      declara <- cob$estado == "degradado" & grepl("mezclados", cob$motivo, fixed = TRUE)
      expect_true(any(declara))
      # Ni un aviso de R suelto: la mezcla se declara en la cobertura.
      expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
    }
  }
})

test_that("la muestra por omision y la finita leen igual la columna mixta, y lo declaran", {
  for (via in list(list(), list(muestra = 6))) {
    cifras <- lapply(list(.tms_orden_a, .tms_orden_b, .tms_orden_c), function(orden) {
      l <- do.call(.tms_perfilar, c(list(orden), via))
      expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
      cob <- l$r$resumen_tabla$cobertura
      expect_true(any(cob$estado == "degradado" &
                        grepl("mezclados", cob$motivo, fixed = TRUE)))
      # La muestra ya no contradice al resumen SQL sobre los mismos valores.
      expect_false(any(grepl("n_distintos|frecuencia_moda", cob$elemento) &
                         cob$estado == "divergencia"))
      l$r$resumen_tabla$columnas[, c("n_validos", "n_distintos")]
    })
    expect_identical(cifras[[1L]], cifras[[2L]])
    expect_identical(cifras[[1L]], cifras[[3L]])
  }
})

test_that("la fuente por bloques lee la columna mixta de una sola forma y lo declara", {
  salidas <- lapply(list(.tms_orden_a, .tms_orden_b, .tms_orden_c), function(orden) {
    l <- .tms_perfilar(orden, bloque_filas = 2)
    cob <- l$r$resumen_tabla$cobertura
    # La fuente por bloques sigue disponible: cada bloque llega igual.
    expect_false(any(cob$bloque == "fuente_bloques" & cob$estado == "no_disponible"))
    expect_true(any(cob$estado == "degradado" &
                      grepl("mezclados", cob$motivo, fixed = TRUE)))
    expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
    col <- l$r$resumen_tabla$columnas
    expect_equal(col$n_distintos, 5)
    pm <- l$r$perfil_muestra$columnas
    # 'ND', 'nd' y el -9 declarado, como en la muestra del motor.
    expect_equal(pm$n_faltantes_disfrazados, 3)
    list(col[, c("n_validos", "n_distintos", "moda")],
         pm[, c("n_distintos", "moda", "n_faltantes_disfrazados")])
  })
  expect_identical(salidas[[1L]], salidas[[2L]])
  expect_identical(salidas[[1L]], salidas[[3L]])
})

test_that("medir_dbi() por bloques mide la regla sobre la columna mixta leida de una sola forma", {
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(nuc$Formato, "F", expresion_regular = "^[0-9]+$"), "t", "x"
  ))
  resultados <- vapply(list(.tms_orden_a, .tms_orden_b, .tms_orden_c), function(orden) {
    con <- .tms_con(orden)
    avisos <- character()
    r <- withCallingHandlers(
      medir_dbi(mod, con, bloque_filas = 2, estrategia_no_sql = "bloques"),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    DBI::dbDisconnect(con)
    expect_identical(nrow(r), 1L)
    expect_false(any(grepl("mixed type", avisos, fixed = TRUE)))
    # Cinco valores: `1` y `2` son digitos; `-9`, `ND` y `nd`, no. Leida por la
    # primera fila, el texto llegaba como `0` y cumplia.
    expect_equal(r$numerador, 2)
    expect_equal(r$denominador, 5)
    r$resultado
  }, numeric(1L))
  expect_identical(resultados[[1L]], resultados[[2L]])
  expect_identical(resultados[[1L]], resultados[[3L]])
})

test_that("una columna sin tipo que no mezcla sigue leyendose con su tipo", {
  con <- .tms_con(c("5", "7", "9", "11", "13", "NULL"))
  on.exit(DBI::dbDisconnect(con))
  r <- perfilar_dbi(con, "t", universo = "muestra_motor",
                    muestra_motor = 6)
  col <- r$resumen_tabla$columnas
  expect_equal(col$minimo, 5)
  expect_equal(col$maximo, 13)
  cob <- r$resumen_tabla$cobertura
  expect_false(any(grepl("mezclados", cob$motivo, fixed = TRUE)))
})

test_that("una columna de afinidad numerica que guarda texto sigue declarando su divergencia", {
  con <- .tms_con(c("10", "20", "'abc'", "40", "'abc'", "60"), tipo = "INTEGER")
  on.exit(DBI::dbDisconnect(con))
  p <- suppressWarnings(perfilar_dbi(con, "t"))
  cob <- p$resumen_tabla$cobertura
  # La causa medida con `typeof()` de siempre, y ninguna relectura como texto.
  expect_true(any(cob$estado == "divergencia"))
  expect_false(any(grepl("mezclados", cob$motivo, fixed = TRUE)))
})

test_that("una columna sin tipo con solo numeros se lee como doble, no como texto", {
  # Un entero de 64 bits y un real: no hay texto que perder, y se lee como
  # doble. El entero pasa de 2^53: se declara como un entero ancho y sus
  # magnitudes no se publican.
  con <- .tms_con(c("9007199254740993", "2.5", "1"))
  on.exit(DBI::dbDisconnect(con))
  r <- suppressWarnings(perfilar_dbi(con, "t"))
  cob <- r$resumen_tabla$cobertura
  expect_false(any(grepl("texto mezclados", cob$motivo, fixed = TRUE)))
  fila <- cob[cob$bloque == "perfil_muestra" & cob$elemento == "x", , drop = FALSE]
  expect_identical(fila$estado, "degradado")
  expect_true(grepl("enteros y reales", fila$motivo, fixed = TRUE))
  expect_true(grepl("entero ancho", fila$motivo, fixed = TRUE))
  pm <- r$perfil_muestra$columnas
  expect_identical(pm$tipo_declarado, "doble")
  expect_true(is.na(pm$maximo))
  expect_true(is.na(pm$minimo))
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(nuc$Formato, "F", expresion_regular = "^[0-9.]+$"), "t", "x"
  ))
  m <- suppressWarnings(medir_dbi(mod, con, bloque_filas = 2, estrategia_no_sql = "bloques"))
  expect_identical(nrow(m), 1L)
  expect_equal(m$resultado, 1)
})

test_that("un BLOB mezclado no se relee como texto: se declara y no pierde bytes", {
  # `CAST(blob AS TEXT)` corta en el primer byte nulo: estos dos BLOB distintos
  # colapsarian en "abc".
  orden <- c("x'61626300646566'", "x'00'", "x'616263'", "'zzz'")
  con <- .tms_con(orden)
  on.exit(DBI::dbDisconnect(con))
  r <- suppressWarnings(perfilar_dbi(con, "t"))
  expect_equal(r$resumen_tabla$columnas$n_distintos, 4)
  cob <- r$resumen_tabla$cobertura
  expect_true(any(cob$estado == "degradado" & grepl("BLOB", cob$motivo, fixed = TRUE)))
  expect_false(any(cob$estado == "divergencia"))
})

test_that("la relectura como texto escribe los numeros como R: 1 y 1.0 son el mismo valor", {
  for (orden in list(c("1", "1.0", "'2'"), c("'2'", "1", "1.0"))) {
    l <- .tms_perfilar(orden)
    col <- l$r$resumen_tabla$columnas
    expect_equal(col$n_distintos, 2)
    expect_equal(col$moda, "1")
    expect_equal(col$frecuencia_moda, 2)
  }
  # La cobertura dice lo que pasa: se lee como texto y se perfila como texto.
  cob <- .tms_perfilar(c("1.0", "'1'", "2", "'2.0'", "3"))$r$resumen_tabla$cobertura
  expect_true(any(grepl("se perfila como texto", cob$motivo, fixed = TRUE)))
  expect_false(any(grepl("no se calculan", cob$motivo, fixed = TRUE)))
})
