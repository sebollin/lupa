test_that("una numeracion conserva su condicion aunque la moda sobresalga", {
  # `densa` responde por la cobertura del rango. La moda es una señal aparte de
  # la guarda de centinelas: no debe contaminar los tres escudos de forma.
  id <- c(1:1000, rep(999L, 100L), 2001:3000)
  completa <- perfilar(
    data.frame(id = id),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  grupo <- perfilar(
    data.frame(id = id[1:1100]),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  # El grupo cubre su rango entero; la multiplicidad no cambia la respuesta de
  # `densa`, aunque sí queda publicada como señal independiente.
  expect_equal(grupo$columnas$densidad_secuencia_entera[[1L]], 1)
  expect_true(grupo$columnas$secuencia_entera_densa[[1L]])
  expect_true(grupo$columnas$moda_sobresale_secuencia_entera[[1L]])

  # Y las dos puertas dicen lo mismo, que es el defecto que se arreglo.
  expect_true("faltantes_disfrazados" %in% completa$hallazgos$tipo)
  expect_true("faltantes_disfrazados" %in% grupo$hallazgos$tipo)
})

test_that("la guarda combina rango y frecuencia del candidato", {
  x1 <- c(1:1000, rep(500L, 40L))
  x2 <- c(1:1500, rep(1501:1505, each = 200L), rep(-9L, 200L))
  x3 <- c(1:1000, rep(999L, 100L), 2001:3000)
  x4 <- 1:1000
  valores <- lapply(list(x1, x2, x3, x4), function(x) {
    perfil <- perfilar(
      data.frame(x = x), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    as.numeric(perfil$columnas$n_faltantes_disfrazados[[1L]])
  })

  # `x3` da 101 y no 100, y la diferencia importa: la columna tiene 101 filas
  # con el valor 999 -cien copias agregadas mas la que trae `1:1000`-, y cual de
  # ellas "pertenece" a la secuencia no esta en el dato. Restar la ocurrencia
  # legitima informaria menos filas afectadas de las que hay, y ademas haria
  # mentir al plan de limpieza: prometeria convertir 100 celdas y convertiria
  # 101. Medido.
  expect_equal(unlist(valores, use.names = FALSE), c(0, 200, 101, 0))
})

test_that("los escudos de forma siguen a la numeracion", {
  perfil <- perfilar(
    data.frame(x = as.character(c(1:1000, rep(500L, 40L)))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)

  expect_true("posible_identificador" %in% tipos)
  expect_false("patron_raro" %in% tipos)
  expect_false("tipo_declarado_distinto" %in% tipos)
  expect_false("alta_cardinalidad" %in% tipos)
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

test_that("una moda legitima no inventa un hallazgo", {
  # Una moda legítima no es un centinela. Aunque la señal abra la mirada de la
  # guarda, el valor dominante no está en la lista y no hay nada que informar.
  comun <- perfilar(
    data.frame(id = c(1:60, rep(7L, 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_true(comun$columnas$secuencia_entera_densa[[1L]])
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
  expect_true(perfil$columnas$secuencia_entera_densa[[1L]])
  expect_equal(as.numeric(perfil$columnas$n_faltantes_disfrazados[[1L]]), 100)
})

test_that("un candidato fuera de rango abre la guarda aunque no haya acantilado", {
  # El rango es una señal independiente de la frecuencia: cuatro repeticiones
  # de un candidato fuera del rango ya abren la guarda. La declaración, además,
  # sigue atravesándola como siempre.
  valores <- c(1:60, rep(-9L, 4L))
  callado <- perfilar(
    data.frame(id = valores),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_false(callado$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_equal(as.numeric(callado$columnas$n_faltantes_disfrazados[[1L]]), 4)

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

  expect_equal(as.numeric(completa$columnas$n_faltantes_disfrazados[[1L]]), 8)
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

test_that("varios centinelas empatados no se cubren entre si", {
  # La tercera version de esta regla la rompio una refutacion externa. Comparar
  # la frecuencia maxima contra la del SEGUNDO valor queda vacua cuando hay
  # varios centinelas con la misma frecuencia: `{499, 499, 499, 2, ...}` da
  # `max == segundo`, y mil cuatrocientos noventa y siete valores se callaban.
  # El acantilado no mira quien es el segundo sino donde se corta la
  # distribucion: de 499 a 2 hay un salto de 249.
  id <- c(rep(1:4000, each = 2L), rep(-9L, 499L), rep(-99L, 499L),
          rep(-999L, 499L))
  perfil <- perfilar(
    data.frame(id = id),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  expect_true(perfil$columnas$moda_sobresale_secuencia_entera[[1L]])
  expect_gt(as.numeric(perfil$columnas$n_faltantes_disfrazados[[1L]]), 1400)
})

test_that("el acantilado se busca arriba, no en la cola", {
  # Control del tope. Una columna real con muchas frecuencias distintas tiene
  # caidas en su cola -de 2 a 1 hay un salto de 2, y `beers$abv` llegaba a 3 a
  # mitad de la suya- que no dicen nada de la columna. El grupo que sobresale
  # esta siempre arriba.
  ruta <- testthat::test_path("..", "..", "..", "notas-desarrollo",
                              "bateria-raha", "datos", "beers_dirty.csv")
  skip_if_not(file.exists(ruta), "el banco real no esta disponible")
  datos <- utils::read.csv(ruta, stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  sobresalen <- perfil$columnas$columna[
    !is.na(perfil$columnas$moda_sobresale_secuencia_entera) &
      perfil$columnas$moda_sobresale_secuencia_entera
  ]
  expect_length(sobresalen, 0L)
})

test_that("la guarda se decide por candidato y no por columna", {
  # Dos candidatos de la lista por omision en la MISMA columna, con evidencia
  # opuesta: `-9` cae fuera del rango de la numeracion -es una ausencia
  # codificada- y `999` aparece UNA vez, en su lugar, dentro de `1:1500`.
  #
  # Decidir la guarda por columna las trataba igual: el `-9` la abria y una vez
  # abierta se marcaba tambien el `999`, que no es sospechoso de nada. Un solo
  # interruptor para dos candidatos con evidencia contraria.
  x <- c(1:1500, rep(1501:1505, each = 200L), rep(-9, 200))
  perfil <- perfilar(
    data.frame(v = x), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )

  # 200, no 201: se acusan los `-9` y no el `999` de la secuencia.
  expect_equal(
    as.numeric(perfil$columnas$n_faltantes_disfrazados[[1L]]), 200
  )

  # Y el control por el otro lado: sin el `-9` que abre la guarda, el mismo
  # `999` legitimo no se acusa solo.
  sin_centinela <- c(1:1500, rep(1501:1505, each = 200L))
  perfil_limpio <- perfilar(
    data.frame(v = sin_centinela), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_equal(
    as.numeric(perfil_limpio$columnas$n_faltantes_disfrazados[[1L]]), 0
  )
})

test_that("una declaracion atraviesa tambien el filtro por candidato", {
  # El filtro por candidato ES la guarda, y una declaracion explicita la
  # atraviesa entera. `999` dentro de `501:1000` no cae fuera del rango ni
  # sobresale en frecuencia: por su cuenta el paquete no lo marca, y hace bien.
  # Pero si el usuario DICE que 999 es un centinela, se marca.
  denso <- data.frame(id = 501:1000)

  solo <- perfilar(denso, analizar_dependencias = FALSE,
                   proteger_datos_personales = FALSE)
  expect_equal(as.numeric(solo$columnas$n_faltantes_disfrazados[[1L]]), 0)

  declarado <- perfilar(denso, analizar_dependencias = FALSE,
                        proteger_datos_personales = FALSE,
                        sentinelas_numericos = c(999))
  expect_true(as.numeric(declarado$columnas$n_faltantes_disfrazados[[1L]]) > 0)
})
