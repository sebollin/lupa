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

test_that("una clave foranea con distribucion pareja conserva su escudo", {
  # El alcance de la regla, medido sobre formas reales. `brewery_id` del banco
  # tiene 62 contra un segundo de 38 -cociente 1,63- y no sobresale; una clave
  # foranea perfectamente pareja tampoco. Lo que sobresale es un valor que
  # DUPLICA al resto, no uno grande.
  pareja <- perfilar(
    data.frame(id = rep(1:60, each = 4L)),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(pareja$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_true(pareja$columnas$secuencia_entera_densa[[1L]])

  suave <- perfilar(
    data.frame(id = rep(1:100, times = c(8L, rep(5L, 99L)))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(suave$columnas$moda_sobresale_secuencia_entera[[1L]])
})

test_that("perder el escudo con una moda dominante legitima no acusa nada", {
  # Una categoria que domina ocho veces al resto SI sobresale, y esta bien que
  # el paquete la vuelva a mirar. Pero mirar no es acusar: el valor dominante no
  # esta en la lista de centinelas, asi que no se informa nada. Ese es el control
  # que abarata cualquier falso positivo de esta senal.
  legitima <- perfilar(
    data.frame(id = rep(1:100, times = c(40L, rep(5L, 99L)))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_true(legitima$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_equal(as.numeric(legitima$columnas$n_faltantes_disfrazados[[1L]]), 0)

  centinela <- perfilar(
    data.frame(id = c(rep(-9L, 40L), rep(1:99, each = 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_equal(as.numeric(centinela$columnas$n_faltantes_disfrazados[[1L]]), 40)
})

test_that("un centinela masivo dentro de una clave foranea no queda callado", {
  # Lo encontro una refutacion externa, y era un agujero del arreglo anterior:
  # `rep(1:40, each = 4)` con cien `-9` da frecuencias {100, 4, 4, ...}. La
  # regla exigia forma de numeracion -frecuencia tipica 1- y una clave foranea
  # nunca la tiene, asi que el escudo se tragaba cien ausencias codificadas.
  # Contra el SEGUNDO valor mas frecuente si se ve: 100 contra 4.
  perfil <- perfilar(
    data.frame(id = c(rep(1:40, each = 4L), rep(-9L, 100L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  expect_true(perfil$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_false(perfil$columnas$secuencia_entera_densa[[1L]])
  expect_equal(as.numeric(perfil$columnas$n_faltantes_disfrazados[[1L]]), 100)
})

test_that("bajo el factor el paquete no conjetura, y la declaracion lo atraviesa", {
  # La banda de tolerancia, que es deliberada: cuatro repeticiones sobre una
  # numeracion dan un cociente de 4 y no llegan al factor 5. El paquete no
  # acusa lo que no puede sostener; lo que el usuario DECLARA si atraviesa la
  # guarda, que es la regla que gobierna todo el paquete.
  valores <- c(1:60, rep(-9L, 4L))
  callado <- perfilar(
    data.frame(id = valores),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(callado$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_equal(as.numeric(callado$columnas$n_faltantes_disfrazados[[1L]]), 0)

  declarado <- perfilar(
    data.frame(id = valores), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, sentinelas_numericos = -9
  )
  expect_equal(as.numeric(declarado$columnas$n_faltantes_disfrazados[[1L]]), 4)
})

test_that("un grupo bajo el umbral de densidad no acusa lo que la tabla calla", {
  # Tercera forma de la misma discrepancia entre puertas. La columna es densa en
  # la tabla completa, pero un grupo cae bajo el umbral y emitia `faltantes`
  # -"0 ausentes reales y 4 disfrazados"-. Ese tipo no estaba en la reubicacion,
  # que solo cubria `faltantes_disfrazados`, asi que las dos puertas discrepaban
  # sobre las mismas cuatro filas.
  a <- c(1:60, rep(-9L, 4L))
  b <- c(1:34, rep(-9L, 4L))
  datos <- data.frame(
    x = c(a, b),
    g = c(rep("A", length(a)), rep("B", length(b))),
    stringsAsFactors = FALSE
  )
  por_grupo <- perfilar_por(
    datos, "g", analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  completa <- perfilar(
    data.frame(x = datos$x), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )

  expect_equal(as.numeric(completa$columnas$n_faltantes_disfrazados[[1L]]), 0)
  expect_equal(
    sum(grepl("faltantes", as.character(por_grupo$tipo_hallazgo))), 0L
  )
  # Y la cobertura lo declara en vez de callarlo en silencio.
  cobertura <- attr(por_grupo, "cobertura_diagnosticos", exact = TRUE)
  expect_true("centinelas_numericos" %in% cobertura$diagnostico)
})

test_that("una ausencia real del grupo NO se reubica", {
  # Control: la reubicacion exige que el grupo no tenga ninguna ausencia real.
  # Un faltante de verdad no es de la particion.
  a <- c(1:60, rep(-9L, 4L))
  b <- c(1:30, rep(NA_integer_, 4L), rep(-9L, 4L))
  datos <- data.frame(
    x = c(a, b),
    g = c(rep("A", length(a)), rep("B", length(b))),
    stringsAsFactors = FALSE
  )
  por_grupo <- perfilar_por(
    datos, "g", analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )

  expect_gt(sum(grepl("faltantes", as.character(por_grupo$tipo_hallazgo))), 0L)
})
