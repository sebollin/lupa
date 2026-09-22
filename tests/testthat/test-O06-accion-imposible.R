# O06. El plan no recomienda una accion que no se puede ejecutar.
#
# La deteccion cuenta filas duplicadas con `duplicated.data.frame()`, que
# tolera columnas-lista, y el ejecutor agrupa por los codigos de `factor()`,
# que no las admite y aborta. Sobre una tabla con una columna-lista, el plan
# recomendaba y activaba `marcar_filas_duplicadas`, y el registro la dejaba
# `fallida` en cada corrida: una recomendacion que no podia cumplirse nunca.

test_that("con una columna-lista, las acciones de duplicados quedan bloqueadas", {
  datos <- data.frame(x = c(1, 1, 2))
  datos$l <- list(1, 1, 2)
  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE), datos)
  filas <- plan$hallazgo == "filas_duplicadas"
  expect_true(any(filas))
  expect_false(any(as.logical(plan$recomendada[filas])))
  expect_false(any(as.logical(plan$aplicar[filas])))
  expect_true(all(as.character(plan$estado[filas]) == "bloqueada"))
  # Y el motivo tiene que decir por que, no quedar en silencio.
  marcar <- plan$estrategia == "marcar_filas_duplicadas"
  expect_true(grepl("lista", as.character(plan$justificacion[marcar]), fixed = TRUE))

  # Lo que importa: aplicar el plan ya no deja acciones fallidas.
  registro <- aplicar(plan, datos)$registro
  expect_false(any(as.character(registro$estado) == "fallida"))
})

test_that("sin columnas-lista, marcar duplicados se sigue recomendando", {
  datos <- data.frame(x = c(1, 1, 2), y = c("a", "a", "b"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE), datos)
  marcar <- plan$estrategia == "marcar_filas_duplicadas"
  expect_true(any(marcar))
  expect_true(all(as.logical(plan$recomendada[marcar])))
  expect_true(all(as.logical(plan$aplicar[marcar])))
})
