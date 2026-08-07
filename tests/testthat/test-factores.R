test_that("las columnas factor entran por los caminos publicos", {
  datos <- data.frame(
    zona = factor(c("Norte", "NORTE", "sur", "SUR")),
    codigo = factor(c("A", "B", "A", "B"))
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(perfil$columnas$tipo_declarado == "factor"))
  expect_s3_class(analizar(datos, analizar_dependencias = FALSE), "analisis")

  plan <- planificar_limpieza(perfil)
  guiado <- suppressMessages(guiar_limpieza(
    plan, datos, selector = function(decision) "convertir_minusculas"
  ))
  salida <- aplicar(guiado, datos)$datos
  expect_type(salida$zona, "character")
  expect_identical(salida$zona, c("norte", "norte", "sur", "sur"))
})

test_that("los metodos de metricas reciben texto aunque la columna sea factor", {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(
      nucleo$DesactualizacionPorFormato,
      validador = function(x) nchar(x) == 3L
    ),
    "entrega", "codigo"
  )
  resultado <- medir(
    modelo(instancia),
    data.frame(codigo = factor(c("ABC", "XX", NA))),
    id_medicion = "factor"
  )
  expect_identical(resultado$resultado, c(0, 1))
})

test_that("los marcos normalizan factores adicionales sin ocultarlos en el perfil", {
  marco <- marco_calidad("Marco factor", data.frame(
    dimension = factor("Estructura"),
    factor = factor("Presencia"),
    responsable = factor("equipo")
  ))
  tabla <- as.data.frame(marco)
  expect_type(tabla$responsable, "character")
  expect_identical(tabla$responsable, "equipo")
})

test_that("los duplicados exhaustivos no pierden filas con columnas factor", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(valor = factor(sprintf("v%04d", seq_len(1000L))))
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = "valor", estrategia = "teselas",
    max_pares = Inf, max_resultados = 1L
  )
  expect_equal(resultado$alcance$n_pares_comparados, 499500)
  expect_equal(resultado$alcance$n_pares_posibles, 499500)
})
