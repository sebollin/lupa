test_that("edades y códigos comunes no son faltantes por defecto", {
  edad <- perfilar(data.frame(
    edad = c(66, 77, 88, 45, 30, 22, 51, 63, 70, 41)
  ))
  oficina <- perfilar(data.frame(
    codigo_oficina = c("66", "77", "88", "12", "34")
  ))

  expect_equal(edad$columnas$n_faltantes_disfrazados, 0L)
  expect_equal(oficina$columnas$n_faltantes_disfrazados, 0L)
  expect_false("posible_identificador" %in% edad$hallazgos$tipo_hallazgo)
  expect_false(any(
    oficina$hallazgos$tipo_hallazgo == "faltantes_disfrazados"
  ))
})

test_that("el sentinela 999 es predeterminado y los contextuales son opt-in", {
  predeterminado <- perfilar(data.frame(x = c(999, 1:9)))
  no_predeterminado <- perfilar(data.frame(x = c(9999, 1:9)))
  contextual <- perfilar(
    data.frame(x = c(66, 77, 88, 9999, 12)),
    sentinelas_numericos = sentinelas_naniar
  )

  expect_equal(predeterminado$columnas$n_faltantes_disfrazados, 1L)
  expect_equal(no_predeterminado$columnas$n_faltantes_disfrazados, 0L)
  expect_equal(contextual$columnas$n_faltantes_disfrazados, 4L)
  expect_false(any(
    contextual$hallazgos$columna == "x" &
      contextual$hallazgos$severidad == "error"
  ))
  numerico <- predeterminado$hallazgos[
    predeterminado$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_true(all(numerico$severidad == "sospechoso"))
})

test_that("los faltantes textuales inequívocos conservan severidad error", {
  resultado <- perfilar(data.frame(x = c("S/D", "dato", "otro")))
  hallazgo <- resultado$hallazgos[
    resultado$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_true(hallazgo$severidad == "error")
  expect_error(
    perfilar(data.frame(x = 1), sentinelas_numericos = "999"),
    "vector numérico"
  )
})

test_that("las cadenas de ausencia están congeladas dentro del paquete", {
  cadenas <- lupa:::.cadenas_na()

  expect_true(all(c("n / a", "s/d", "sin dato") %in% cadenas))
  expect_false("valor corriente" %in% cadenas)
})

test_that("un posible identificador exige forma y alta unicidad", {
  datos <- data.frame(
    edad = c(21, 22, 25, 31, 40, 45, 52, 63, 70, 81),
    id = sprintf("TR%03d", 1:10),
    stringsAsFactors = FALSE
  )
  resultado <- perfilar(datos)
  candidatos <- resultado$hallazgos[
    resultado$hallazgos$tipo_hallazgo == "posible_identificador", ,
    drop = FALSE
  ]

  expect_equal(candidatos$columna, "id")
  expect_true(candidatos$severidad == "ok")
})

test_that("las fechas totalmente ambiguas explican por qué no tienen rango", {
  resultado <- perfilar(data.frame(
    f = c("01/02/2020", "03/04/2020", "05/06/2020")
  ))
  columna <- resultado$columnas[resultado$columnas$columna == "f", ]
  ambiguo <- resultado$hallazgos[
    resultado$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo", ,
    drop = FALSE
  ]

  expect_equal(columna$tipo_inferido, "fecha")
  expect_equal(columna$proporcion_tipo_inferido, 1)
  expect_true(is.na(columna$minimo_fecha))
  expect_equal(nrow(ambiguo), 1L)
  expect_match(ambiguo$descripcion, "rango temporal no se calcula")
})

test_that("se detectan espacios al borde e inconsistencias de mayúsculas", {
  resultado <- perfilar(data.frame(
    localidad = c("Canelones", "Canelones ", "montevideo", "Montevideo")
  ))
  tipos <- resultado$hallazgos$tipo_hallazgo

  expect_true("espacios_sobrantes" %in% tipos)
  expect_true("mayusculas_inconsistentes" %in% tipos)
  expect_equal(resultado$columnas$n_espacios_borde, 1L)
  expect_equal(resultado$columnas$n_variantes_mayusculas, 2L)
})

test_that("se detectan nombres de columna no sintácticos o duplicados", {
  datos <- structure(
    list(1:2, 3:4, 5:6),
    names = c("$id", "f+e)cha ", "$id"),
    class = "data.frame",
    row.names = .set_row_names(2L)
  )
  resultado <- perfilar(datos)
  hallazgo <- resultado$hallazgos[
    resultado$hallazgos$tipo_hallazgo == "nombres_columnas_problematicos", ,
    drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_true(hallazgo$severidad == "sospechoso")
  expect_match(hallazgo$evidencia, "f+e)cha ", fixed = TRUE)
})
