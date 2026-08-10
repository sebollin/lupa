test_that("los diagnosticos textuales por vocabulario conservan el perfil", {
  tablas <- list(
    baja = data.frame(
      ciudad = rep(c("Montevideo", "Salto", NA, ""), 30L),
      nombre = rep(c(" Ana", "Ana", "JosÃ©"), 40L),
      stringsAsFactors = FALSE
    ),
    alta = data.frame(
      codigo = sprintf("COD-%04d", seq_len(240L)),
      monto = rep(c("1.234,50", "2.000,00", "3.50"), 80L),
      stringsAsFactors = FALSE
    ),
    mojibake = data.frame(
      nombre = rep(c("JosÃ©", "DirecciÃ³n�", "Ana", NA_character_), 25L),
      stringsAsFactors = FALSE
    ),
    bordes = data.frame(
      texto = rep(c(" dato", "dato ", "dato", "otro"), 25L),
      stringsAsFactors = FALSE
    ),
    una = data.frame(texto = "solo", stringsAsFactors = FALSE),
    vacia = data.frame(texto = character(), stringsAsFactors = FALSE)
  )

  comparar <- function(datos) {
    optimizado <- perfilar(
      datos, analizar_dependencias = FALSE,
      duplicados_aproximados = FALSE
    )
    testthat::local_mocked_bindings(
      .vocabulario_texto = function(textos, umbral, valores = NULL) {
        list(
          valores = textos,
          indices = seq_along(textos),
          usar = FALSE,
          n_distintos = length(unique(textos[!is.na(textos)]))
        )
      },
      .package = "lupa"
    )
    directo <- perfilar(
      datos, analizar_dependencias = FALSE,
      duplicados_aproximados = FALSE
    )
    directo$meta$fecha_hora <- optimizado$meta$fecha_hora
    expect_identical(directo, optimizado)
  }
  lapply(tablas, comparar)
})

test_that("la reparacion de codificacion se calcula una sola vez por valor", {
  textos <- rep(c("JosÃ©", "Ana", "DirecciÃ³n�", NA_character_), 100L)
  esperado <- lupa:::.analizar_codificacion(textos)
  obtenido <- lupa:::.analizar_codificacion_vocabulario(textos)
  expect_identical(obtenido, esperado)
})
