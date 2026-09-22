# O09. La conversion regional dice lo que hace y cuenta lo que cambia.
#
# Tres defectos de una misma accion, recomendada y aplicada sola:
#
#   * El plan prometia 11 cambios y el registro publicaba 20. El plan heredaba
#     `n_numeros_texto`, que solo cuenta los valores con separadores; la
#     conversion cambia de tipo la columna entera, tambien el "7". Las otras
#     dos conversiones de tipo ya contaban valores presentes con unidad
#     "valor". Y el ejecutor contaba solo los no vacios: un "" que la
#     conversion vuelve `NA` cambiaba sin figurar.
#
#   * Una columna de "10%" pasaba a 0.1 con una justificacion que aseguraba
#     que se convertia "sin elegir entre interpretaciones". El factor 1/100 es
#     la convencion del paquete para proporciones, pero no se decia.
#
#   * Toda columna no convertible recibia el mismo motivo -falta evidencia para
#     distinguir el separador decimal del de miles-, tambien una que mezcla `5`
#     con `5 %` y no tiene ningun separador.

.o09_plan <- function(valores) {
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos = datos)
  list(datos = datos, plan = plan,
       fila = plan$estrategia == "convertir_numero_regional")
}

test_that("el plan y el registro cuentan lo mismo en la conversion regional", {
  caso <- .o09_plan(c(
    "1.234,56", "7", "7,5", "10", "3,14159", "0,75", "2.500,50", "12,5", "0",
    "-5,25", "42", "300.000,99", "1", "2", "3", "4,5", "6", "8", "9", "11"
  ))
  expect_equal(sum(caso$fila), 1L)
  plan <- caso$plan
  expect_true(plan$aplicar[caso$fila])
  expect_equal(plan$n_afectadas[caso$fila], 20)
  expect_identical(plan$unidad_conteo[caso$fila], "valor")
  plan$aplicar <- caso$fila

  registro <- as.data.frame(aplicar(plan, caso$datos)$registro)
  expect_identical(as.character(registro$estado), "ejecutada")
  expect_equal(registro$n_cambiadas, 20L)
})

test_that("un vacio que la conversion vuelve NA cuenta como cambio", {
  datos <- data.frame(v = c("1.234,5", "2,5", "", "3,25", NA, "4,75"),
                      stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos = datos)
  fila <- plan$estrategia == "convertir_numero_regional"
  expect_equal(sum(fila), 1L)
  plan$aplicar <- fila
  resultado <- aplicar(plan, datos)
  # El vacio efectivamente cambio: si esto deja de ser cierto, la prueba no mide.
  expect_true(is.na(resultado$datos$v[[3L]]))
  registro <- as.data.frame(resultado$registro)
  expect_identical(as.character(registro$estado), "ejecutada")
  expect_equal(registro$n_cambiadas, 5L)
  expect_equal(plan$n_afectadas[fila], registro$n_cambiadas)
})

test_that("la justificacion declara la division por 100 y la unidad descartada", {
  porcentaje <- .o09_plan(paste0(c(5, 10, 20, 30, 15, 25, 40, 50, 12, 60), "%"))
  expect_true(porcentaje$plan$aplicar[porcentaje$fila])
  expect_match(porcentaje$plan$justificacion[porcentaje$fila],
               "se dividen por 100", fixed = TRUE)
  # Y lo que declara es lo que hace.
  salida <- aplicar(porcentaje$plan, porcentaje$datos)$datos$v
  expect_equal(salida[[1L]], 0.05)

  kilos <- .o09_plan(paste(12:21, "kg"))
  expect_true(kilos$plan$aplicar[kilos$fila])
  expect_match(kilos$plan$justificacion[kilos$fila], "\"kg\" deja de formar",
               fixed = TRUE)

  # Sin unidad ni moneda no se agrega ninguna nota.
  llano <- .o09_plan(c("1.234,5", "2,5", "3,25", "4", "5,5", "7,1", "8,2", "9,3"))
  expect_true(llano$plan$aplicar[llano$fila])
  expect_false(grepl("deja de formar|dividen por 100",
                     llano$plan$justificacion[llano$fila]))
})

test_that("una columna no convertible publica su motivo real", {
  mezcla <- .o09_plan(c("5", "10 %", "20", "30 %", "15", "25 %", "40", "50 %",
                        "12", "60 %"))
  expect_false(mezcla$plan$aplicar[mezcla$fila])
  motivo <- mezcla$plan$justificacion[mezcla$fila]
  expect_match(motivo, "mezcla unidades", fixed = TRUE)
  expect_false(grepl("separador", motivo, fixed = TRUE))

  # Donde el motivo SI es el separador, se sigue diciendo.
  ambigua <- .o09_plan(c("1.234", "2.345", "3.456", "4.567", "5.678", "6.789"))
  expect_false(ambigua$plan$aplicar[ambigua$fila])
  expect_match(ambigua$plan$justificacion[ambigua$fila], "separador decimal",
               fixed = TRUE)

  # Y lo que el perfil no trae no se afirma.
  expect_match(lupa:::.motivo_numero_no_seguro(data.frame(x = 1)),
               "el perfil no lo acredita", fixed = TRUE)
})
