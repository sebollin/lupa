test_that("las fechas y separadores arbitrarios no son formas de documento", {
  datos <- data.frame(
    fecha_iso = rep("2024-01-15", 5L),
    fecha_puntos = rep("15.03.2024", 5L),
    fecha_guiones = rep("15-03-2024", 5L),
    fecha_compacta = rep("20240115", 5L),
    importe_espaciado = rep("1 000 000", 5L),
    decimal = rep("3.14159265", 5L),
    separadores = rep("1-2-3-4-5-6-7", 5L),
    fecha_barras = rep("15/03/2024", 5L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false(any(perfil$datos_personales$columna %in% names(datos)))
})

test_that("las formas estructuradas de CI y RUT siguen reconocidas", {
  datos <- data.frame(
    ci = rep("1.234.567-2", 5L),
    rut_espacios = rep("21 100 342 0017", 5L),
    rut_plano = rep("211406340011", 5L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_setequal(perfil$datos_personales$columna, names(datos))
  expect_equal(
    perfil$datos_personales$proporcion_compatible,
    rep(1, 3L)
  )
})

test_that("la longitud cruda filtra el trabajo de digitos", {
  libre <- rep(paste0(strrep("texto ", 5L), "administrativo"), 1001L)
  perfil <- perfilar(data.frame(observacion = libre), analizar_dependencias = FALSE)
  expect_false("observacion" %in% perfil$datos_personales$columna)
})
