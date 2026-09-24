# O38: los metodos `print` mostraban un subconjunto de campos sin decirlo,
# mientras el mismo paquete declara cada recorte en sus informes ("Se muestran
# 2 de 6 filas"). El del perfil mostraba 5 campos de 114; el del plan, 12 de
# 20, y los ocho que oculta incluyen `evidencia` y `justificacion`, que son lo
# que se lee para decidir si aplicar una accion.

test_that("el print del perfil declara cuantos campos muestra", {
  datos <- data.frame(id = seq_len(20L), v = as.numeric(seq_len(20L)))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  salida <- capture.output(print(perfil), type = "message")
  aviso <- grep("Se muestran", salida, value = TRUE, fixed = TRUE)

  expect_true(length(aviso) >= 1L)
  expect_match(aviso[[1L]], "campos", fixed = TRUE)
  expect_match(aviso[[1L]], "columnas()", fixed = TRUE)
  # El numero que declara es el del objeto, no uno fijo.
  expect_match(aviso[[1L]], as.character(ncol(perfil$columnas)), fixed = TRUE)
})

test_that("el print del plan declara cuantos campos muestra", {
  datos <- data.frame(v = c(" a ", "b", "c"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)

  salida <- capture.output(print(plan), type = "message")
  aviso <- grep("Se muestran", salida, value = TRUE, fixed = TRUE)

  expect_true(length(aviso) >= 1L)
  expect_match(aviso[[1L]], as.character(ncol(as.data.frame(plan))),
               fixed = TRUE)
  expect_match(aviso[[1L]], "as.data.frame(plan)", fixed = TRUE)
})

test_that("si no se recorta nada no se anuncia un recorte", {
  # Control: el aviso sale del objeto, no de una constante. Una tabla cuyas
  # columnas se muestran todas no tiene nada que declarar.
  vista <- data.frame(a = 1, b = 2)
  expect_silent(lupa:::.avisar_campos_no_mostrados(vista, vista, "x"))
})
