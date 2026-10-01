# El piso numerico de la proteccion no tapa numeros que son estructura.
#
# Tapaba cualquier numero cuya representacion fuera un valor protegido. Medido en
# una base real de millones de filas: los `indices_fila` de una traza que
# coincidian con un documento protegido quedaban en NA y lupa se acusaba a si
# misma -"total de traza no coincide con sus indices"-. Y en `perfilar_dbi()`, una
# columna `id = 1..n` hacia que el conteo `n` saliera NA en todas las columnas. La
# coincidencia exige seis caracteres, asi que el caso real necesita mas de 100.000
# filas; aca se ejercita el piso sobre las estructuras reales de un perfil chico,
# con los valores protegidos elegidos para coincidir.

test_that("los indices de la traza y los conteos sobreviven a un valor que coincide", {
  datos <- data.frame(
    x = c("a", "SIN DATO", "b", "SIN DATO", "c", "d", "SIN DATO", "e"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, proteger_datos_personales = FALSE))
  hallazgos <- perfil$hallazgos
  con_traza <- which(vapply(hallazgos$trazabilidad, function(t) {
    length(t$indices_fila) > 0L
  }, logical(1L)))
  expect_gt(length(con_traza), 0L)
  indices <- hallazgos$trazabilidad[[con_traza[[1L]]]]$indices_fila
  # Los valores "protegidos" son justo los indices y los conteos de la tabla.
  valores <- unique(c(as.character(indices), as.character(nrow(datos))))
  protegido <- lupa:::.proteger_numeros_parametros(hallazgos, valores)
  expect_identical(protegido$trazabilidad[[con_traza[[1L]]]]$indices_fila, indices)
  protegidas <- lupa:::.proteger_numeros_parametros(perfil$columnas, valores)
  expect_identical(protegidas$n, perfil$columnas$n)
})

test_that("el control: un campo de valor que coincide se sigue tapando", {
  datos <- data.frame(monto = c(123456, 234567, 345678, NA))
  perfil <- suppressWarnings(perfilar(datos, proteger_datos_personales = FALSE))
  columnas <- perfil$columnas
  expect_equal(columnas$minimo[[1L]], 123456)
  protegidas <- lupa:::.proteger_numeros_parametros(
    columnas, c("123456", "345678")
  )
  # El minimo y el maximo son valores de la tabla: se tapan.
  expect_true(is.na(protegidas$minimo[[1L]]))
  expect_true(is.na(protegidas$maximo[[1L]]))
  # El conteo, al lado, no.
  expect_identical(protegidas$n, columnas$n)
})

test_that("un campo que no se sabe que es falla cerrado", {
  # La lista es de ESTRUCTURA: lo que no esta en ella se sigue tapando, asi que
  # un campo nuevo no queda publicado por omision.
  x <- list(campo_nuevo = 123456, n = 123456, indices_fila = 123456L,
            minimo = 123456)
  protegido <- lupa:::.proteger_numeros_parametros(x, "123456")
  expect_true(is.na(protegido$campo_nuevo))
  expect_true(is.na(protegido$minimo))
  expect_identical(protegido$n, 123456)
  expect_identical(protegido$indices_fila, 123456L)
  # Y la lista conserva su forma: ningun elemento desaparece.
  expect_identical(names(protegido), names(x))
})

test_that("un contenedor con nombre de estructura no se saltea entero", {
  # El primer arreglo salteaba por el NOMBRE tambien a los contenedores, y el
  # perfil tiene uno que se llama `columnas`: la tabla entera quedaba sin tapar y
  # el `minimo` de una columna copia volvia a publicar documentos. Lo atrapo
  # `test-proteccion-por-columna.R`; esta es la forma minima.
  x <- list(
    columnas = data.frame(minimo = 123456, n = 123456),
    muestra = list(valor = 123456, filas = 123456)
  )
  protegido <- lupa:::.proteger_numeros_parametros(x, "123456")
  expect_true(is.na(protegido$columnas$minimo))
  expect_true(is.na(protegido$muestra$valor))
  expect_identical(protegido$columnas$n, 123456)
  expect_identical(protegido$muestra$filas, 123456)
})
