# La evidencia de `tipo_declarado_distinto` afirmaba "la conversion no pierde
# ningun valor" solo porque todos los valores eran compatibles con el tipo
# inferido, al lado de un plan que declaraba la misma conversion destructiva o
# la bloqueaba: `007` pierde el cero, `2020-01-31` y `31/01/2020` quedan en la
# misma fecha. La evidencia dice ahora lo mismo que el plan.

.fecha_conversion <- as.POSIXct("2026-01-01", tz = "UTC")

.evidencia_y_plan <- function(x) {
  datos <- data.frame(v = x, stringsAsFactors = FALSE)
  p <- perfilar(datos, fecha = .fecha_conversion)
  h <- as.data.frame(p$hallazgos)
  h <- h[h$tipo_hallazgo == "tipo_declarado_distinto", , drop = FALSE]
  plan <- as.data.frame(planificar_limpieza(p, datos))
  conversion <- plan[grepl("^convertir|^desambiguar", plan$estrategia), ,
                     drop = FALSE]
  list(
    evidencia = h$evidencia, sugerencia = h$sugerencia,
    sin_perdida_plan = nrow(conversion) > 0L &&
      all(as.character(conversion$estado) == "lista" & !conversion$destructiva)
  )
}

test_that("la evidencia solo afirma que no se pierde nada cuando el plan tampoco pierde", {
  casos <- list(
    codigo = c("007", "008", "007", "008", "009", "007"),
    fechas_que_chocan = c("2020-01-31", "31/01/2020", "31.01.2020",
                          "2020-02-29", "29/02/2020"),
    convencion_mixta = c("1234,5", "2234,5", "1234.5", "4234.5", "5234,25"),
    dia_mes = c("01/02/2020", "03/04/2020", "05/06/2020"),
    limpio = c("7", "8", "9", "10"),
    fechas_limpias = c("2020-01-31", "2020-02-29"),
    mezcla_sin_choque = c("2020-01-31", "15/02/2020", "2020-03-10",
                          "20/04/2020"),
    # Dieciocho decimales: el `double` no los guarda y el plan la declara
    # destructiva por precision.
    decimales_largos = c("0,123456789012345678", "0,987654321098765432"),
    decimales_cortos = c("1,23", "2,45")
  )
  for (nombre in names(casos)) {
    r <- .evidencia_y_plan(casos[[nombre]])
    expect_identical(length(r$evidencia), 1L, info = nombre)
    expect_identical(
      grepl("no pierde ning", r$evidencia, fixed = TRUE), r$sin_perdida_plan,
      info = nombre
    )
  }
})

test_that("la evidencia nombra lo que la conversion perderia", {
  r <- .evidencia_y_plan(c("007", "008", "007", "008", "009", "007"))
  # A mano: los seis valores empiezan con cero.
  expect_match(r$evidencia, "6 valores tienen cero inicial", fixed = TRUE)
  expect_match(r$sugerencia, "Antes de convertir", fixed = TRUE)
  # `2020-01-31`, `31/01/2020` y `31.01.2020` son la misma fecha, y
  # `2020-02-29` y `29/02/2020` otra: cinco valores que chocan, los mismos que
  # el plan cuenta como no inyectivos.
  r <- .evidencia_y_plan(c("2020-01-31", "31/01/2020", "31.01.2020",
                           "2020-02-29", "29/02/2020"))
  expect_match(r$evidencia, "5 valores escritos de formas distintas", fixed = TRUE)
  r <- .evidencia_y_plan(c("01/02/2020", "03/04/2020", "05/06/2020"))
  expect_match(r$evidencia, "lectura de fecha", fixed = TRUE)
  # A mano: `as.numeric("0.123456789012345678")` a 18 cifras no es el texto:
  # 0.123456789012345677 donde R lee el decimal mas cercano, y
  # 0.123456789012345691 en macOS sobre arm64, donde la lectura acumula en un
  # doble. El otro tambien cambia la ultima.
  expect_false(identical(sprintf("%.18f", as.numeric("0.123456789012345678")),
                         "0.123456789012345678"))
  r <- .evidencia_y_plan(c("0,123456789012345678", "0,987654321098765432"))
  expect_match(r$evidencia, "2 valores tienen m\u00e1s cifras", fixed = TRUE)
})
