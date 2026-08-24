# Los limites conocidos de las dos reglas, escritos como comprobaciones para que
# esten declarados y no se descubran de nuevo. Ninguno se apago en silencio: los
# dos arreglos que los habrian cerrado se midieron, callaban cosas reales, y se
# retiraron dejando escrito por que en `R/hallazgos.R` y `R/columnas.R`.

test_that("una clave dispersa recibe Benford, que es el limite conocido", {
  # No es lo deseable, es lo medido y declarado. Se prefiere hablar de mas sobre
  # un identificador disperso antes que callar el dato malo de una magnitud, que
  # es lo que costaba el arreglo. Si algun dia esto cambia, el cambio tiene que
  # venir con una senal que no dependa de cuantas filas se cargaron.
  set.seed(3)
  clave <- sort(sample.int(600000L, 10000L))
  perfil <- perfilar(
    data.frame(MEsId = clave), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  # La columna es unica y su densidad es baja: por las dos puertas queda fuera.
  expect_equal(perfil$columnas$tasa_distintos, 1)
  expect_lt(perfil$columnas$densidad_secuencia_entera, 0.5)
  expect_false(.parece_identificador_numerico(perfil$columnas))
})

test_that("la densidad no puede separar una clave dispersa de un monto", {
  # Es el hecho que hace que el limite anterior no se arregle bajando un umbral:
  # la clave es MAS dispersa que el monto, asi que ningun corte sobre ese eje los
  # distingue. Si esta comprobacion falla, la premisa cambio y conviene volver a
  # mirar el problema entero.
  set.seed(3)
  densidad <- function(v) {
    perfilar(
      data.frame(x = v), analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE
    )$columnas$densidad_secuencia_entera
  }
  clave <- sort(sample.int(2300000L, 10000L))
  monto <- round(stats::rlnorm(10000L, 9, 1.2))
  expect_lt(densidad(clave), densidad(monto))
})

test_that("el par 9999 y 9998 se reconoce, que es lo que costaba la guarda retirada", {
  # `9999` = "no sabe" junto a `9998` = "no contesta" es la codificacion mas
  # comun en microdatos de encuesta. Descartar al candidato con vecinos la
  # rompia: se callaban los dos centinelas y salian cincuenta y cinco codigos de
  # ausencia informados como valores extremos. Esta comprobacion existe para que
  # ninguna guarda futura vuelva a costar eso sin que se note.
  set.seed(31)
  horas <- sample(0:80, 300L, replace = TRUE)
  perfil <- perfilar(
    data.frame(h = c(horas, rep(9999L, 30L), rep(9998L, 25L))),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$centinela_valor, 9999)
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_true("posible_centinela_numerico" %in% tipos)
})

test_that("un tramo de codigos altos recibe el aviso de centinela, y es el lado barato", {
  # El falso positivo que la guarda retirada arreglaba: `222` se marca y `221` y
  # `223` no, con la misma frecuencia y la misma lejania. Queda asi a proposito,
  # porque el hallazgo es un aviso que dice que lo decida quien conoce la
  # columna, y callarlo costaba centinelas verdaderos.
  set.seed(6)
  codigos <- c(
    sample.int(50L, 900L, replace = TRUE),
    rep(221L, 8L), rep(222L, 8L), rep(223L, 8L)
  )
  perfil <- perfilar(
    data.frame(cod = codigos), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$centinela_valor, 222)
  # Y el aviso no afirma que sea un defecto: su severidad no es `error`.
  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "posible_centinela_numerico", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_false(as.character(hallazgo$severidad[[1L]]) == "error")
})
