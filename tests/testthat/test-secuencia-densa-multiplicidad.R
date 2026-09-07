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
  # de centinelas; no acusa por si solo. Con el valor repetido en un `7`, que no
  # es centinela, no hay nada que informar.
  comun <- perfilar(
    data.frame(id = c(1:60, rep(7L, 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(comun$columnas$secuencia_entera_densa[[1L]])
  expect_false("faltantes_disfrazados" %in% comun$hallazgos$tipo)

  centinela <- perfilar(
    data.frame(id = c(1:60, rep(-9L, 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_true("faltantes_disfrazados" %in% centinela$hallazgos$tipo)
})

test_that("un senuelo frecuente no tapa al centinela", {
  # Lo encontro una refutacion externa. Comparar la moda contra el SEGUNDO valor
  # supone que el centinela ES la moda, y es derrotable: con `1:60`, cinco `-9` y
  # un `7` repetido seis veces las frecuencias quedan {6, 5, 1, ...}, el cociente
  # moda/segundo da 1,2 y los cinco centinelas se callaban. Por eso la frecuencia
  # maxima se compara contra la TIPICA, no contra la segunda.
  for (repeticiones in c(6L, 20L)) {
    id <- c(1:60, rep(-9L, 5L), rep(7L, repeticiones))
    perfil <- perfilar(
      data.frame(id = id),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    )
    expect_true(
      perfil$columnas$moda_sobresale_secuencia_entera[[1L]],
      info = paste("senuelo x", repeticiones)
    )
    expect_true(
      "faltantes_disfrazados" %in% perfil$hallazgos$tipo,
      info = paste("senuelo x", repeticiones)
    )
  }
})

test_that("una columna donde todos los valores repiten no es una numeracion", {
  # El alcance de la regla. En una numeracion los valores NO se repiten: su
  # frecuencia tipica es 1. Una clave foranea repite todo -medido sobre el banco
  # real, `brewery_id` tiene mediana 3 y solo el 22,8 % de sus valores aparece
  # una vez- y por eso esta regla no la toca: conserva su escudo.
  foranea <- perfilar(
    data.frame(id = rep(1:100, times = c(40L, rep(5L, 99L)))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(foranea$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_true(foranea$columnas$secuencia_entera_densa[[1L]])

  pareja <- perfilar(
    data.frame(id = rep(1:60, each = 4L)),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(pareja$columnas$moda_sobresale_secuencia_entera[[1L]])
})
