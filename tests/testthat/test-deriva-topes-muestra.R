test_that("los topes que cambian la muestra explican la deriva de patrones", {
  testthat::skip_on_cran()
  n <- 62L
  datos <- data.frame(
    ciudad = rep("Montevideo", n),
    a = rep(1, n), b = rep(2, n), c = rep(3, n), d = rep(4, n),
    e = rep(5, n), f = rep(6, n), stringsAsFactors = FALSE
  )
  datos$ciudad[1:4] <- sprintf("%04d", 1:4)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  ))
  movido <- suppressWarnings(perfilar(
    datos, fecha = fecha + 60, max_celdas_muestra = 200,
    analizar_dependencias = FALSE
  ))

  expect_identical(as.integer(movido$meta$muestra_efectiva), 28L)
  hallazgo <- movido$hallazgos[
    movido$hallazgos$tipo_hallazgo == "patron_raro" &
      movido$hallazgos$columna == "ciudad", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  deriva <- suppressWarnings(comparar_perfiles(base, movido))
  expect_true(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(
    deriva$aspecto == "hallazgo" & deriva$valor_actual == "patron_raro"
  ))
})

test_that("una tabla que pasa de `muestra` filas a una mas sigue comparando patrones", {
  testthat::skip_on_cran()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- data.frame(codigo = sprintf("AB-%03d", seq_len(200L)),
                     stringsAsFactors = FALSE)
  # La fila nueva tiene otra forma y tiene que entrar en la muestra de 200: se
  # busca entre valores de la misma forma uno que la huella elija.
  actual <- NULL
  for (i in seq_len(500L)) {
    nuevo <- paste(letters[c(i %% 26L + 1L, (i %/% 26L) %% 26L + 1L, 3L)],
                   collapse = "")
    prueba <- rbind(base, data.frame(codigo = nuevo, stringsAsFactors = FALSE))
    if (201L %in% lupa:::.indices_huella_tabla(prueba, 200L)$indices) {
      actual <- prueba
      break
    }
  }
  expect_false(is.null(actual))
  anterior <- suppressWarnings(perfilar(
    base, muestra = 200L, fecha = fecha, analizar_dependencias = FALSE
  ))
  posterior <- suppressWarnings(perfilar(
    actual, muestra = 200L, fecha = fecha + 60, analizar_dependencias = FALSE
  ))
  # La premisa: un lado se lee entero y el otro es una muestra.
  expect_gte(as.numeric(anterior$meta$muestra_efectiva), 200)
  expect_lt(as.numeric(posterior$meta$muestra_efectiva), 201)
  deriva <- suppressWarnings(comparar_perfiles(anterior, posterior))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(deriva$aspecto == "patron" & deriva$cambio == "aparecido"))
})

test_that("el mismo tope de bytes que recorta distinto en cada entrega no corta los patrones", {
  testthat::skip_on_cran()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  n <- 400L
  corta <- data.frame(codigo = sprintf("AB-%03d", seq_len(n)),
                      nota = sprintf("n%03d", seq_len(n)),
                      stringsAsFactors = FALSE)
  larga <- corta
  larga$nota <- paste0(larga$nota, strrep("x", 40L))
  tope <- as.numeric(utils::object.size(corta)) / 2
  anterior <- suppressWarnings(perfilar(
    corta, fecha = fecha, max_bytes_muestra = tope,
    analizar_dependencias = FALSE
  ))
  posterior <- suppressWarnings(perfilar(
    larga, fecha = fecha + 60, max_bytes_muestra = tope,
    analizar_dependencias = FALSE
  ))
  # La premisa: el tope recorta las dos, a tamanos distintos.
  efectiva_a <- as.numeric(anterior$meta$muestra_efectiva)
  efectiva_b <- as.numeric(posterior$meta$muestra_efectiva)
  expect_lt(efectiva_a, n)
  expect_lt(efectiva_b, n)
  expect_false(identical(efectiva_a, efectiva_b))
  deriva <- suppressWarnings(comparar_perfiles(anterior, posterior))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
})

.dtm_tabla <- function(n = 3000L) {
  data.frame(id = seq_len(n), codigo = sprintf("AB-%04d", seq_len(n)),
             bloque = rep(seq_len(n / 10L), each = 10L),
             stringsAsFactors = FALSE)
}

.dtm_perfil <- function(datos, minuto = 0, ...) {
  suppressWarnings(perfilar(
    datos, muestra = 1000L, analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC") + minuto * 60, ...
  ))
}

test_that("un tope declarado distinto que no recorta no corta los patrones", {
  testthat::skip_on_cran()
  base <- .dtm_tabla()
  # Una fila de otra forma que la muestra de 1.000 ve: se busca entre filas.
  actual <- NULL
  for (fila in seq(7L, 3000L, by = 37L)) {
    prueba <- base
    prueba$codigo[fila] <- "zz99"
    if (fila %in% lupa:::.indices_huella_tabla(prueba, 1000L)$indices) {
      actual <- prueba
      break
    }
  }
  expect_false(is.null(actual))
  anterior <- .dtm_perfil(base, max_celdas_muestra = 1e6)
  posterior <- .dtm_perfil(actual, 1, max_celdas_muestra = 2e6)
  # La premisa: los dos muestrean 1.000 filas y ningun tope recorta.
  expect_equal(as.numeric(anterior$meta$muestra_efectiva), 1000)
  expect_equal(as.numeric(posterior$meta$muestra_efectiva), 1000)
  deriva <- suppressWarnings(comparar_perfiles(anterior, posterior))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(deriva$aspecto == "patron" & deriva$cambio == "aparecido"))
  # El control: el mismo cambio de tope cuando si recorta sigue declarando.
  recortado <- .dtm_perfil(actual, 1, max_celdas_muestra = 1500)
  expect_lt(as.numeric(recortado$meta$muestra_efectiva), 1000)
  deriva_recorte <- suppressWarnings(comparar_perfiles(anterior, recortado))
  expect_true(any(deriva_recorte$aspecto == "configuracion_patrones"))
})

test_that("dos muestras elegidas con otra regla no comparan sus patrones", {
  testthat::skip_on_cran()
  base <- .dtm_tabla()
  anterior <- .dtm_perfil(base)
  sin_regla <- function(deriva) !any(deriva$aspecto == "configuracion_patrones")
  # Una columna agregada cambia la huella de cada fila.
  con_columna <- base
  con_columna$extra <- seq_len(nrow(base)) %% 7L
  deriva <- suppressWarnings(comparar_perfiles(anterior, .dtm_perfil(con_columna, 1)))
  fila <- deriva[deriva$aspecto == "configuracion_patrones", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$evidencia, "huella_v2 sobre bloque, codigo, id;", fixed = TRUE)
  expect_false(any(deriva$aspecto == "patron"))
  # Otra `clave`, tambien.
  por_id <- .dtm_perfil(base, clave = "id")
  por_bloque <- .dtm_perfil(base, 1, clave = "bloque")
  expect_false(sin_regla(suppressWarnings(comparar_perfiles(por_id, por_bloque))))
  # Un perfil sin `muestra_huella` -como los de la 0.1.2, que muestreaban por
  # posicion- contra uno nuevo.
  viejo <- anterior
  viejo$meta$muestra_huella <- NULL
  deriva_viejo <- suppressWarnings(comparar_perfiles(viejo, .dtm_perfil(base, 1)))
  fila_viejo <- deriva_viejo[deriva_viejo$aspecto == "configuracion_patrones", ,
                             drop = FALSE]
  expect_equal(nrow(fila_viejo), 1L)
  expect_match(fila_viejo$evidencia, "por posici", fixed = TRUE)
  # El control: la misma regla compara, y si un lado se lee entero la regla del
  # otro no cuenta.
  expect_true(sin_regla(suppressWarnings(comparar_perfiles(anterior, .dtm_perfil(base, 1)))))
  entera <- suppressWarnings(perfilar(
    base[seq_len(1000L), ], muestra = 1000L, analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  ))
  expect_true(sin_regla(suppressWarnings(comparar_perfiles(entera, .dtm_perfil(con_columna, 1)))))
})
