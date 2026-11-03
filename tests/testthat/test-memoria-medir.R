test_that("medir une salidas compactas por columnas", {
  nucleo <- metricas_nucleo()
  m1 <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloX"),
    "t", "x", nombre_instancia = "M1"
  )
  m2 <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloY"),
    "t", "y", nombre_instancia = "M2"
  )
  salidas <- list(
    data.frame(
      resultado = c(TRUE, FALSE), entidad = "t", atributo = "x",
      fila = 1:2, objeto = paste0("t$x[", 1:2, "]"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      resultado = FALSE, entidad = "t", atributo = "y", fila = 3L,
      objeto = "t$y[3]", stringsAsFactors = FALSE
    )
  )
  fecha <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  resultado <- lupa:::.unir_salidas_medicion(
    salidas, list(m1, m2), "corrida", fecha
  )
  esperado <- data.frame(
    id_medicion = rep("corrida", 3L), fecha = rep(fecha, 3L),
    metrica = rep("NoNulo", 3L),
    metrica_especifica = c(rep("NoNuloX", 2L), "NoNuloY"),
    metrica_instanciada = c(rep("M1", 2L), "M2"),
    dimension = rep("Completitud", 3L), factor = rep("Densidad", 3L),
    orientacion = rep("conformidad", 3L),
    granularidad = rep("instanciaAtributo", 3L),
    tipo_resultado = rep("booleano", 3L), entidad = rep("t", 3L),
    atributo = c("x", "x", "y"), fila = c(1L, 2L, 3L),
    objeto_medible = c("t$x[1]", "t$x[2]", "t$y[3]"),
    resultado = c(1, 0, 0), agregacion = rep(NA_character_, 3L),
    stringsAsFactors = FALSE
  )
  expect_identical(resultado, esperado)
})
