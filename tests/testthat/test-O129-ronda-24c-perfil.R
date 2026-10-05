# Ronda 24-C: lo que afirma el perfil, contado otra vez.

.trazas_dentro_de_la_tabla_O129 <- function(h, n) {
  all(vapply(h$trazabilidad, function(tr) {
    is.null(tr$indices_fila) || all(tr$indices_fila >= 1L & tr$indices_fila <= n)
  }, logical(1L)))
}

test_that("H10: una columna data.frame anidada es compuesta, no celdas como filas", {
  d <- data.frame(id = 1:12)
  d$sub <- data.frame(
    a = c(rep(NA, 8), 1:4), b = c(rep(NA, 9), letters[1:3]),
    c = c(rep(NA, 10), 1, 2)
  )
  # A mano: 27 celdas ausentes en 12 filas. Antes se publicaban como 27
  # faltantes, `prop_faltantes = 2.25` y una traza hasta la fila 34.
  expect_identical(sum(is.na(d$sub)), 27L)
  p <- perfilar(d)
  fila <- p$columnas[p$columnas$columna == "sub", ]
  expect_equal(fila$n, 12)
  expect_true(is.na(fila$n_faltantes))
  expect_true(is.na(fila$prop_faltantes))
  expect_identical(fila$tipo_declarado, "data.frame")
  h <- hallazgos(p)
  expect_true(.trazas_dentro_de_la_tabla_O129(h, nrow(d)))
  propias <- h[h$columna == "sub", ]
  expect_false("faltantes" %in% propias$tipo_hallazgo)
  i <- which(propias$tipo_hallazgo == "tipo_compuesto_no_analizado")
  expect_length(i, 1L)
  expect_equal(propias$n_afectados[[i]], 12)
  expect_equal(propias$trazabilidad[[i]]$indices_fila, 1:12)
  expect_match(propias$descripcion[[i]], "tabla anidada")
})

test_that("H10: un tibble empaquetado tambien es compuesto", {
  skip_if_not_installed("tibble")
  tb <- tibble::tibble(
    id = 1:12,
    sub = tibble::tibble(a = c(rep(NA, 8), 1:4), b = c(rep(NA, 9), letters[1:3]))
  )
  p <- perfilar(tb)
  fila <- p$columnas[p$columnas$columna == "sub", ]
  expect_equal(fila$n, 12)
  expect_true(is.na(fila$prop_faltantes))
  h <- hallazgos(p)
  expect_true(.trazas_dentro_de_la_tabla_O129(h, nrow(tb)))
  expect_false("faltantes" %in% h$tipo_hallazgo[h$columna == "sub"])
})

.traza_O129 <- function(h, tipo) {
  i <- which(h$tipo_hallazgo == tipo)
  if (length(i) != 1L) return(NULL)
  h$trazabilidad[[i]]$indices_fila
}

test_that("H4: los centinelas declarados no corren las trazas de los cuantitativos", {
  x <- c(-1, -1, -1, 10:35)
  x[20] <- 0
  x[25] <- -5
  x <- c(x, 1000)
  # A mano, sobre la tabla y no sobre el vector sin centinelas.
  expect_identical(which(x == 0), 20L)
  expect_identical(which(x < 0 & x != -1), 25L)
  expect_identical(which(x == 1000), 30L)
  h <- hallazgos(perfilar(
    data.frame(x = x), sentinelas_numericos = -1,
    columnas_sin_ceros = "x", columnas_no_negativas = "x"
  ))
  # Antes: 17, 22 y 27.
  expect_equal(.traza_O129(h, "ceros_no_permitidos"), 20L)
  expect_equal(.traza_O129(h, "negativos_no_permitidos"), 25L)
  expect_equal(.traza_O129(h, "outliers"), 30L)

  y <- c(-1, -1, 1:20, Inf, 30:35, NaN)
  expect_identical(which(is.nan(y) | is.infinite(y)), c(23L, 30L))
  h2 <- hallazgos(perfilar(data.frame(y = y), sentinelas_numericos = -1))
  # Antes: 21 y 28.
  expect_equal(.traza_O129(h2, "valores_no_finitos"), c(23L, 30L))

  # `perfilar_por()` traduce a la tabla original sobre la traza ya alineada.
  d <- data.frame(g = rep(c("A", "B"), each = 30), x = c(x, x))
  por <- perfilar_por(
    d, por = "g", min_filas = 5L, sentinelas_numericos = -1,
    columnas_sin_ceros = "x"
  )
  i <- which(por$grupo == "B" & por$tipo_hallazgo == "ceros_no_permitidos")
  expect_equal(por$trazabilidad[[i]]$indices_fila, 50L)
})

test_that("H4: el centinela en integer64 tampoco corre la traza de outliers", {
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64(c(-1, -1, -1, 10:35, 1000))
  h <- hallazgos(perfilar(data.frame(x = x), sentinelas_numericos = -1))
  # A mano: el 1000 esta en la fila 30. Antes: 27.
  expect_equal(.traza_O129(h, "outliers"), 30L)
})

test_that("H9: la traza del centinela convierte como el resumen (factor y coma decimal)", {
  v <- c(rep(c(21, 34, 47, 52, 38), 8), rep(8888, 5), 0, -3)
  # A mano: el centinela esta en las filas 41 a 45.
  expect_identical(which(v == 8888), 41:45)
  formas <- list(
    factor = factor(v),
    coma = sub("^(-?[0-9]+)$", "\\1,0", as.character(v))
  )
  for (forma in names(formas)) {
    avisos <- character()
    h <- withCallingHandlers(
      hallazgos(perfilar(data.frame(x = formas[[forma]], stringsAsFactors = FALSE))),
      lupa_trazabilidad_incoherente = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    i <- which(h$tipo_hallazgo == "posible_centinela_numerico")
    expect_length(i, 1L)
    expect_equal(h$n_afectados[[i]], 5, info = forma)
    expect_identical(h$trazabilidad[[i]]$estado, "disponible", info = forma)
    expect_equal(h$trazabilidad[[i]]$indices_fila, 41:45, info = forma)
    expect_length(avisos, 0L)
  }
})

test_that("H9: una traza vacia no_disponible no publica total cero", {
  traza <- lupa:::.trazabilidad_indices(integer(), "completo")
  expect_identical(traza$estado, "no_disponible")
  expect_true(is.na(traza$total))
})

test_that("H1: el NaN no se descuenta dos veces del denominador del resumen", {
  evaluados <- function(x, tipo) {
    h <- hallazgos(perfilar(
      data.frame(x = x), columnas_sin_ceros = "x", columnas_no_negativas = "x"
    ))
    h$n_evaluados[h$tipo_hallazgo == tipo]
  }
  con_nan <- c(0, 1, 2, NaN, NaN, NaN, 5, -3)
  con_na <- c(0, 1, 2, NA, NA, NA, 5, -3)
  # A mano: cinco valores entran al resumen (la media publicada es su promedio).
  expect_identical(sum(is.finite(con_nan)), 5L)
  # Antes: 2 con NaN y 5 con NA, sobre el mismo dato.
  expect_equal(evaluados(con_nan, "ceros_no_permitidos"), 5)
  expect_equal(evaluados(con_nan, "negativos_no_permitidos"), 5)
  expect_equal(evaluados(con_na, "ceros_no_permitidos"), 5)
  # Con la mitad de NaN la resta llegaba a cero y volvia la columna entera (4).
  expect_equal(evaluados(c(0, -1, NaN, NaN), "ceros_no_permitidos"), 2)
  # `outliers`, con NaN e Inf: 23 finitos. Antes: 21.
  x <- c(1:22, 100, NaN, NaN, Inf)
  expect_identical(sum(is.finite(x)), 23L)
  expect_equal(evaluados(x, "outliers"), 23)
})

test_that("H3: con truncado, la traza de mayusculas trae primero las variantes", {
  v <- c(rep("Montevideo", 30), "MONTEVIDEO", rep(c("Salto", "Rivera"), 10))
  expect_identical(which(v == "MONTEVIDEO"), 31L)
  h <- hallazgos(perfilar(data.frame(v = v), max_filas_hallazgo = 10L))
  traza <- h$trazabilidad[[which(h$tipo_hallazgo == "mayusculas_inconsistentes")]]
  expect_identical(traza$estado, "truncada")
  expect_equal(traza$total, 31)
  # Antes: 1..10, diez "Montevideo" y ningun testigo de la colision.
  expect_equal(traza$indices_fila, c(31L, 1:9))
  expect_setequal(unique(v[traza$indices_fila]), c("MONTEVIDEO", "Montevideo"))

  # Dos grupos: antes la traza era 3..12, todas "Alfa".
  v2 <- c("ZONA", "Zona", rep("Alfa", 12), "ALFA", rep(c("x1", "x2"), 5))
  h2 <- hallazgos(perfilar(data.frame(v = v2), max_filas_hallazgo = 10L))
  i <- which(h2$tipo_hallazgo == "mayusculas_inconsistentes")
  expect_equal(h2$n_afectados[[i]], 4)
  # A mano: los cuatro valores del hallazgo estan en la traza truncada.
  expect_setequal(unique(v2[h2$trazabilidad[[i]]$indices_fila]),
                  c("ZONA", "Zona", "Alfa", "ALFA"))
})

test_that("H3: con truncado, la traza de normalizacion_unicode trae la variante", {
  skip_if_not_installed("stringi")
  compuesta <- "Jos\u00e9"
  descompuesta <- "Jose\u0301"
  v <- c(rep(compuesta, 30), descompuesta, rep(c("Ana", "Luis"), 10))
  h <- hallazgos(perfilar(data.frame(v = v), max_filas_hallazgo = 10L))
  i <- which(h$tipo_hallazgo == "normalizacion_unicode")
  expect_length(i, 1L)
  expect_true(31L %in% h$trazabilidad[[i]]$indices_fila)
})

test_that("H2: un patron con frecuencia igual a umbral_patron_raro es raro", {
  cod <- c(sprintf("%04d", 1:94), sprintf("AB%02d", 1:5), "x")
  # A mano: las cinco filas AB01..AB05 tienen frecuencia exactamente 0.05, la
  # maxima declarada por omision; con la "x" son seis filas raras.
  expect_true(5 / 100 == 0.05)
  h <- hallazgos(perfilar(data.frame(cod = cod)))
  i <- which(h$tipo_hallazgo == "patron_raro")
  # Antes: 1 afectada y 5 "excluidas por superar" el umbral.
  expect_equal(h$n_afectados[[i]], 6)
  expect_equal(h$trazabilidad[[i]]$indices_fila, 95:100)
  expect_match(h$evidencia[[i]], "excluidos_por_umbral=0 filas", fixed = TRUE)
  # La misma frontera en las dos implementaciones de los patrones.
  memoria <- descubrir_patrones(cod)
  bloques <- lupa:::.descubrir_patrones_bloques(cod, tamano = 7L)
  expect_identical(attr(memoria, "n_patrones_raros"), 2L)
  expect_identical(attr(bloques, "n_patrones_raros"), 2L)
  # Por encima del umbral sigue excluido.
  h2 <- hallazgos(perfilar(data.frame(cod = cod), umbral_patron_raro = 0.04))
  expect_equal(h2$n_afectados[[which(h2$tipo_hallazgo == "patron_raro")]], 1)
})

test_that("H5: perfilar_por() traduce tambien indices_unidades a la tabla original", {
  # `casi_duplicados_vocabulario` compara con `stringdist`, un Suggests: sin el,
  # el hallazgo no existe y no hay traza que traducir.
  skip_if_not_installed("stringdist")
  n <- 80
  g <- rep(c("A", "B"), n / 2)
  v <- rep(c("Salto", "Rivera", "Rocha", "Florida"), length.out = n)
  v[g == "B"][1:36] <- "Montevideo"
  v[g == "B"][37:38] <- "Montevido"
  v[g == "B"][39] <- "MONTEVIDEO"
  d <- data.frame(g = g, v = v, stringsAsFactors = FALSE)
  por <- perfilar_por(d, por = "g", min_filas = 5L)
  i <- which(por$grupo == "B" & por$tipo_hallazgo == "casi_duplicados_vocabulario")
  expect_length(i, 1L)
  unidades <- por$trazabilidad[[i]]$indices_unidades
  # A mano: la primera fila de cada forma del grupo, en la tabla entera.
  esperadas <- match(c("MONTEVIDEO", "Montevido", "Montevideo"), d$v)
  expect_identical(esperadas, c(78L, 74L, 2L))
  # Antes: 39, 37, 1 -posiciones dentro de la rebanada, filas del grupo A-.
  expect_setequal(unidades, esperadas)
  expect_true(all(d$g[unidades] == "B"))
  # Y coincide con la tabla entera.
  h <- hallazgos(perfilar(d))
  j <- which(h$tipo_hallazgo == "casi_duplicados_vocabulario")
  expect_setequal(unidades, h$trazabilidad[[j]]$indices_unidades)
})

test_that("H6: patron_raro sobre una muestra sin hallazgo deja fila de cobertura", {
  cod <- sprintf("%04d", 1:200)
  raras <- c(3, 50, 77, 101, 150, 199)
  cod[raras] <- paste0("X", seq_along(raras))
  # A mano: ninguna fila rara cae en la rejilla de `muestra = 67`.
  rejilla <- unique(as.integer(round(seq.int(1, 200, length.out = 67))))
  expect_length(intersect(raras, rejilla), 0L)
  p <- perfilar(data.frame(cod = cod), muestra = 67)
  expect_false("patron_raro" %in% hallazgos(p)$tipo_hallazgo)
  cobertura <- p$cobertura_diagnosticos
  # Antes: cobertura vacia, un perfil "limpio" sobre una enumeracion parcial.
  fila <- cobertura[cobertura$diagnostico == "patron_raro" &
                      cobertura$columna == "cod", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "67 de 200", fixed = TRUE)
  # Sin muestreo no hay fila: se emite el hallazgo con las seis filas.
  completo <- perfilar(data.frame(cod = cod), muestra = Inf)
  h <- hallazgos(completo)
  expect_equal(h$n_afectados[h$tipo_hallazgo == "patron_raro"], 6)
  expect_false("patron_raro" %in% completo$cobertura_diagnosticos$diagnostico)
})

test_that("H7: n_evaluados de patron_raro es el universo de su proporcion_dominante", {
  cod <- c(sprintf("%04d", 1:90), NA, NA, "", " ", "AB1", "AB2", "AB3",
           "x", "y", "zz")
  h <- hallazgos(perfilar(data.frame(cod = cod, stringsAsFactors = FALSE)))
  i <- which(h$tipo_hallazgo == "patron_raro")
  expect_length(i, 1L)
  publicada <- as.numeric(sub(
    ".*proporcion_dominante=([0-9.]+).*", "\\1", h$evidencia[[i]]
  ))
  # A mano: 98 filas con valor, 90 con el patron dominante, 8 raras. Todo lo
  # no dominante es raro, asi que 1 - afectados/evaluados ES la proporcion del
  # dominante. Antes: 1 - 8/100 = 0.920 contra 0.918 publicada.
  expect_identical(sum(!is.na(cod)), 98L)
  expect_equal(publicada, round(90 / 98, 3))
  expect_equal(h$n_afectados[[i]], 8)
  expect_equal(
    round(1 - h$n_afectados[[i]] / h$n_evaluados[[i]], 3), publicada
  )
  # El mismo denominador publicado en las dos implementaciones de patrones.
  expect_identical(attr(descubrir_patrones(cod), "n_evaluados"), 98L)
  expect_identical(
    attr(lupa:::.descubrir_patrones_bloques(cod, tamano = 7L), "n_evaluados"),
    98L
  )
})

test_that("H8: muestra_motor retiene la traza como no_disponible y conserva el conteo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  d <- data.frame(id = 1:200, s = rep(c("aa", "bb", "cc", "dd"), length.out = 200))
  d$s[c(5, 150, 151, 152)] <- "AA"
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  pd <- suppressMessages(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 199
  ))
  pm <- pd$perfil_muestra
  avisos <- character()
  h <- withCallingHandlers(hallazgos(pm), lupa_trazabilidad_incoherente = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(avisos, 0L)
  expect_gt(nrow(h), 0L)
  for (i in seq_len(nrow(h))) {
    traza <- h$trazabilidad[[i]]
    # Antes: NULL, fuera de los cuatro estados.
    expect_true(is.list(traza))
    expect_length(traza$indices_fila, 0L)
    if (h$unidad_conteo[[i]] %in% c("columna", "formato")) {
      expect_identical(traza$estado, "no_aplica")
    } else {
      expect_identical(traza$estado, "no_disponible")
      expect_identical(traza$alcance, "orden_muestra_no_estable")
    }
  }
  i <- which(h$tipo_hallazgo == "patron_raro")
  # El total conocido se conserva: cuatro filas "AA" en la muestra.
  expect_equal(h$trazabilidad[[i]]$total, h$n_afectados[[i]])
  # A mano: un patron raro (`A+`) en la muestra; antes se publicaba 0.
  patrones <- pm$patrones$s
  expect_identical(attr(patrones, "n_patrones_raros"), 1L)
  expect_identical(attr(patrones, "n_patrones_raros_trazabilidad"), 1L)
  expect_length(attr(patrones, "patrones_raros_trazabilidad"), 0L)
})

test_that("H11: la moda empatada por bloque_filas sigue el desempate del paquete", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  casos <- list(
    dobles = c(rep(c(9.579, 10.608, 11.517, 3.25), each = 3), 20 + (1:20) / 7),
    texto = c(rep(c("zeta", "alfa", "Mu"), each = 3), paste0("v", 1:20))
  )
  # A mano: el menor numero empatado, y el primero por bytes ("M" < "a").
  esperadas <- c(dobles = "3.25", texto = "Mu")
  for (caso in names(casos)) {
    d <- data.frame(v = casos[[caso]], stringsAsFactors = FALSE)
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(con, "t", d)
    memoria <- as.character(suppressWarnings(perfilar(d))$columnas$moda)
    bloques <- as.character(suppressWarnings(suppressMessages(
      perfilar_dbi(con, "t", bloque_filas = 5L)
    ))$resumen_tabla$columnas$moda)
    DBI::dbDisconnect(con)
    expect_identical(memoria, esperadas[[caso]], info = caso)
    # Antes: "10.608" y "alfa".
    expect_identical(bloques, esperadas[[caso]], info = caso)
  }
})
