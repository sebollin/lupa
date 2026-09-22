# O17. Dos cifras publicadas que no decian de donde salian.
#
#   * `comparar_equivalencia()` publicaba `diferencia_relativa`, que por debajo
#     de magnitud 1 no es relativa: el divisor es 1 y la comparacion pasa a ser
#     absoluta. Una media de 0,001 que pasaba a 0,4 -cuatrocientas veces- salia
#     `equivalente` con tolerancia 0,5, y el veredicto viajaba sin su criterio.
#     La tolerancia mixta se conserva, porque dividir por casi cero convierte el
#     ruido en un cambio del cien por ciento; lo que cambia es que ahora se
#     declara: la columna se llama por lo que es y el motivo dice cuando la
#     escala fue la unidad.
#
#   * La evidencia de Benford publicaba `1:NA/NA; ... 9:NA/NA` en TODA corrida:
#     pasaba las nueve proporciones juntas a un formateador que devolvia `NA`
#     para cualquier vector. El chi-cuadrado y su p-valor iban al lado de un
#     desglose que no tenia ningun numero.

test_that("la equivalencia declara cuando la escala fue la unidad", {
  chico <- comparar_equivalencia(
    perfilar(data.frame(m = c(0.0005, 0.0015))),
    perfilar(data.frame(m = c(0.2, 0.6))),
    tolerancia = 0.5
  )
  fila <- as.data.frame(chico)[chico$campo == "media", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true("diferencia_normalizada" %in% names(fila))
  expect_false("diferencia_relativa" %in% names(fila))
  # El criterio no cambia: sigue siendo equivalente. Lo que cambia es que lo dice.
  expect_identical(as.character(fila$veredicto), "equivalente")
  expect_identical(fila$motivo, "dentro_de_tolerancia_escala_unidad")
  expect_equal(fila$diferencia_normalizada, 0.399, tolerance = 1e-9)

  # Control: por encima de magnitud 1 la escala es el valor y el motivo no lleva
  # el sufijo, porque ahi la cifra SI es relativa.
  grande <- comparar_equivalencia(
    perfilar(data.frame(m = c(100, 300))),
    perfilar(data.frame(m = c(100, 302))),
    tolerancia = 0.5
  )
  fila_grande <- as.data.frame(grande)[grande$campo == "media", , drop = FALSE]
  expect_identical(fila_grande$motivo, "dentro_de_tolerancia")
  expect_equal(fila_grande$diferencia_normalizada, 1 / 201, tolerance = 1e-9)

  # Y un cambio grande sigue saliendo materialmente distinto, con su motivo.
  lejos <- comparar_equivalencia(
    perfilar(data.frame(m = c(100, 300))),
    perfilar(data.frame(m = c(100, 900))),
    tolerancia = 0.1
  )
  fila_lejos <- as.data.frame(lejos)[lejos$campo == "media", , drop = FALSE]
  expect_identical(as.character(fila_lejos$veredicto), "materialmente_distinto")
  expect_identical(fila_lejos$motivo, "fuera_de_tolerancia")
})

test_that("la evidencia de Benford publica el desglose por digito", {
  perfil <- perfilar(data.frame(tarifa = rep(c(1, 10, 100, 1000), 30)))
  hallazgo <- as.data.frame(hallazgos(perfil))
  fila <- hallazgo[grepl("benford", hallazgo$tipo_hallazgo), , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$evidencia, "observado/esperado por digito", fixed = TRUE)
  # Ni un solo NA: los nueve digitos traen su observado y su esperado.
  expect_false(grepl("NA/NA", fila$evidencia, fixed = TRUE))
  expect_match(fila$evidencia, "1:1.000/0.301", fixed = TRUE)
  expect_match(fila$evidencia, "9:0.000/0.046", fixed = TRUE)
})

test_that("los formateadores publicados no callan sobre un vector", {
  # La raiz: devolvian `NA` para cualquier longitud distinta de uno, asi que
  # cada llamada con un vector era un defecto silencioso.
  expect_identical(
    lupa:::.formatear_decimal_publicado(c(0.5, 0.25, NA)),
    c("0.500", "0.250", NA_character_)
  )
  expect_identical(
    lupa:::.formatear_numero_publicado(c(1234.5, NA)),
    c("1234.5", NA_character_)
  )
  # Los escalares y los vacios no cambian.
  expect_identical(lupa:::.formatear_decimal_publicado(0.5), "0.500")
  expect_identical(lupa:::.formatear_decimal_publicado(NA_real_), NA_character_)
  expect_identical(lupa:::.formatear_decimal_publicado(numeric()), character())
})
