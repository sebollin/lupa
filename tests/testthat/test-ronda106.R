test_that("marco_cepal conserva niveles, principios y alcance", {
  marco <- marco_cepal()
  tabla <- as.data.frame(marco)
  niveles <- c(
    "Nivel A. Gestión del sistema estadístico",
    "Nivel B. Gestión del entorno institucional",
    "Nivel C. Gestión del proceso estadístico",
    "Nivel D. Gestión de los productos estadísticos"
  )
  principios <- c(
    "Coordinación del sistema estadístico nacional",
    "Gestión de las relaciones con los usuarios de datos, los proveedores de datos y otros grupos de interés",
    "Gestión de normas y estándares estadísticos",
    "Asegurar la independencia profesional",
    "Asegurar la imparcialidad y la objetividad",
    "Asegurar la transparencia",
    "Asegurar la confidencialidad estadística y la seguridad de los datos",
    "Asegurar el compromiso con la calidad",
    "Asegurar la suficiencia de los recursos",
    "Asegurar la solidez metodológica",
    "Asegurar una buena relación costo-eficiencia",
    "Asegurar procedimientos estadísticos apropiados",
    "Manejo de la carga del encuestado",
    "Asegurar la relevancia",
    "Asegurar la precisión y la confiabilidad",
    "Asegurar la oportunidad y la puntualidad",
    "Asegurar la accesibilidad y la claridad",
    "Asegurar la coherencia y la comparabilidad",
    "Gestión de los metadatos"
  )

  expect_s3_class(marco, "marco_calidad")
  expect_equal(nrow(tabla), 19L)
  expect_equal(unique(tabla$dimension), niveles)
  expect_equal(tabla$factor, principios)
  expect_equal(tabla$principio, 1:19)
  expect_equal(as.integer(table(tabla$dimension)), c(3L, 6L, 4L, 6L))
  expect_false(any(tabla$perfil_mide))
  expect_equal(
    tabla$disponibilidad,
    c(rep("fuera_de_alcance", 13L), rep("disponible", 6L))
  )
  expect_true(all(nzchar(tabla$descripcion)))
  expect_true(all(nzchar(tabla$como_resolverlo)))
  expect_true(all(grepl("No es una limitación transitoria", tabla$como_resolverlo,
                        fixed = TRUE)[1:13]))
  expect_true(all(grepl("no se establece sobre una tabla", tabla$como_resolverlo,
                        fixed = TRUE)[1:13]))
  expect_true(grepl("Naciones Unidas", marco$origen, fixed = TRUE))
  expect_true(grepl("CEA/CEPAL", marco$origen, fixed = TRUE))
  expect_true(grepl("https://unstats.un.org/unsd/methodology/dataquality/",
                    marco$origen, fixed = TRUE))
  expect_true(grepl("https://repositorio.cepal.org/handle/11362/47464",
                    marco$origen, fixed = TRUE))
  expect_false("catalogo_cepal" %in% getNamespaceExports("lupa"))

  impreso <- capture.output(print(marco), type = "message")
  expect_true(any(grepl("Marco de aseguramiento", impreso, fixed = TRUE)))
})

test_that("cobertura_analisis respeta el alcance de marco_cepal", {
  perfil <- perfilar(data.frame(x = 1:3), analizar_dependencias = FALSE)
  cobertura <- cobertura_analisis(perfil, modelo = marco_cepal())

  expect_equal(nrow(cobertura), 19L)
  expect_equal(
    as.character(cobertura$estado),
    c(rep("fuera_de_alcance", 13L), rep("no_declarada", 6L))
  )
  expect_false(any(as.character(cobertura$estado) == "medida"))
  expect_true(all(grepl("no se establece sobre una tabla",
                        cobertura$como_resolverlo[1:13], fixed = TRUE)))
})
