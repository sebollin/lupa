test_that("F-1 y F-6 protegen las dos formas de la media temporal", {
  datos <- data.frame(fecha_nacimiento = as.Date(c(
    "1990-05-12", "1985-11-03", "1990-05-12", "2001-07-25"
  )))
  protegido <- perfilar(datos, analizar_dependencias = FALSE)$columnas
  claro <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )$columnas

  expect_true(isTRUE(protegido$dato_personal_protegido))
  expect_equal(protegido$media_fecha, "[valor protegido]")
  expect_equal(protegido$desvio, claro$desvio)
  expect_equal(
    protegido$detalle_proteccion_personal,
    "[estadisticos de orden y momentos protegidos]"
  )
  expect_equal(protegido$minimo_fecha, "[valor protegido]")
  expect_equal(protegido$maximo_fecha, "[valor protegido]")
  expect_equal(protegido$mediana_fecha, "[valor protegido]")
  expect_equal(protegido$moda, "[valor protegido]")
})

test_that("F-2 omite del comparador los campos de un lado protegido", {
  datos <- data.frame(cedula = c(
    12345678, 87654321, 12345678, 13579246, 24681357
  ))
  protegido <- perfilar(datos, analizar_dependencias = FALSE)$columnas
  claro <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )$columnas
  comparacion <- comparar_equivalencia(protegido, claro, tolerancia = 1e-9)
  campos <- attr(comparacion, "campos_protegidos")

  expect_false(any(
    as.character(comparacion$veredicto) == "materialmente_distinto"
  ))
  expect_identical(names(campos), c("columna", "campo", "lado"))
  expect_true(all(campos$columna == "cedula"))
  expect_true(all(campos$lado == "anterior"))
  expect_true(all(c("moda", "minimo", "maximo", "media", "mediana") %in%
                  campos$campo))
  expect_identical(levels(comparacion$veredicto), c(
    "identico", "equivalente", "materialmente_distinto"
  ))
})

test_that("F-3 no compara desvio cuando solo un lado es temporal", {
  fechas <- as.Date(c(
    "2026-01-01", "2026-01-02", "2026-01-01", "2026-01-02",
    "2026-01-01", "2026-01-02"
  ))
  temporal <- perfilar(data.frame(x = fechas), analizar_dependencias = FALSE)$columnas
  numerico <- perfilar(
    data.frame(x = c(0, temporal$desvio * sqrt(2))),
    analizar_dependencias = FALSE
  )$columnas
  comparacion <- comparar_equivalencia(temporal, numerico, tolerancia = 1e-9)
  detalle <- attr(comparacion, "detalle_campos_no_comparables")
  desvio <- detalle[detalle$columna == "x" & detalle$campo == "desvio", , drop = FALSE]

  expect_false(any(comparacion$campo == "desvio"))
  expect_true("desvio" %in% attr(comparacion, "campos_no_comparables"))
  expect_equal(nrow(desvio), 1L)
  expect_identical(desvio$motivo, "tipo_cambiado:temporal_vs_no_temporal")

  segundo_temporal <- perfilar(
    data.frame(x = fechas + 1), analizar_dependencias = FALSE
  )$columnas
  ambos_temporales <- comparar_equivalencia(
    temporal, segundo_temporal, tolerancia = 1e-9
  )
  expect_true(any(ambos_temporales$campo == "desvio"))
})

test_that("F-4 el detalle declara una moda de texto protegida", {
  datos <- data.frame(documento = c(
    "V-12345678", "V-87654321", "V-12345678", "V-13579246", "V-24681357"
  ))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)$columnas

  expect_equal(perfil$moda, "[valor protegido]")
  expect_false(is.na(perfil$detalle_proteccion_personal))
  expect_match(perfil$detalle_proteccion_personal, "orden", fixed = TRUE)
})

test_that("F-5 el mapa y absorber rechazan entradas no atomicas", {
  valores <- list(c(1, 2), c(1, 2), "x", c(1, 2))
  matriz <- matrix(c(1, 1, 2, 2), nrow = 2L)

  for (entrada in list(valores, matriz)) {
    mapa <- lupa:::.mapa_distintos_bloques(entrada, tamano = 2L)
    expect_identical(mapa$estado, "no_disponible")
    expect_identical(mapa$motivo, "entrada_no_soportada:no_atomica")
    expect_true(is.na(mapa$exacto))
    expect_null(mapa$resultado)
  }

  acumulador <- lupa:::iniciar(
    "x", "list", familia = "distintos", max_entradas = 10L
  )
  acumulador <- lupa:::absorber(acumulador, list(
    valores = valores, ordinal_inicio = 1, ordinal_fin = 4
  ))
  resultado <- lupa:::finalizar(acumulador)
  expect_identical(resultado$estado, "no_disponible")
  expect_identical(resultado$motivo, "entrada_no_soportada:no_atomica")
  expect_true(is.na(resultado$exacto))
})

test_that("F-6 conserva el desvio numerico y protege la media", {
  datos <- data.frame(cedula = c(
    12345678, 87654321, 12345678, 13579246, 24681357
  ))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)$columnas

  expect_true(is.na(perfil$media))
  expect_true(is.finite(perfil$desvio))
  expect_equal(
    perfil$detalle_proteccion_personal,
    "[estadisticos de orden y momentos protegidos]"
  )
})
