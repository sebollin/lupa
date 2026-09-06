test_that("una numeracion con un valor que sobresale deja de ser numeracion", {
  # La densidad cuenta la cobertura del rango e ignora la multiplicidad: mil
  # valores distintos sobre mil posiciones dan densidad 1 aunque uno aparezca
  # ciento una veces. Antes, el grupo callaba lo que la tabla completa acusaba.
  id <- c(1:1000, rep(999L, 100L), 2001:3000)
  completa <- perfilar(
    data.frame(id = id),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  grupo <- perfilar(
    data.frame(id = id[1:1100]),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  # El grupo cubre su rango entero y aun asi no es una numeracion limpia.
  expect_equal(grupo$columnas$densidad_secuencia_entera[[1L]], 1)
  expect_false(grupo$columnas$secuencia_entera_densa[[1L]])
  expect_true(grupo$columnas$moda_sobresale_secuencia_entera[[1L]])

  # Y las dos puertas dicen lo mismo, que es el defecto que se arreglo.
  expect_true("faltantes_disfrazados" %in% completa$hallazgos$tipo)
  expect_true("faltantes_disfrazados" %in% grupo$hallazgos$tipo)
})

test_that("una numeracion limpia sigue siendo numeracion", {
  perfil <- perfilar(
    data.frame(id = 1:1000),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  expect_true(perfil$columnas$secuencia_entera_densa[[1L]])
  expect_false(perfil$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_false("faltantes_disfrazados" %in% perfil$hallazgos$tipo)
})

test_that("una clave foranea reparte y conserva su condicion de numeracion", {
  # Control: las reglas que miran el TAMANO de la moda rompen esto, porque una
  # clave foranea legitima tiene modas grandes. La que mira la FORMA no.
  id <- rep(1:100, times = c(8L, rep(5L, 99L)))
  perfil <- perfilar(
    data.frame(id = id),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  expect_false(perfil$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_true(perfil$columnas$secuencia_entera_densa[[1L]])
})

test_that("perder la condicion de numeracion no inventa un hallazgo", {
  # Apagar el escudo de "esto es una numeracion" solo deja que se mire la lista
  # de centinelas; no acusa por si solo. Con la moda en un valor que no es
  # centinela no hay nada que informar.
  comun <- perfilar(
    data.frame(id = rep(1:100, times = c(40L, rep(5L, 99L)))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(comun$columnas$secuencia_entera_densa[[1L]])
  expect_false("faltantes_disfrazados" %in% comun$hallazgos$tipo)

  centinela <- perfilar(
    data.frame(id = c(rep(999L, 40L), rep(setdiff(1:100, 999), each = 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_true("faltantes_disfrazados" %in% centinela$hallazgos$tipo)
})
