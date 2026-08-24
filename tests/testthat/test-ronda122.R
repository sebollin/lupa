# Una clave primaria dispersa recibia `desviacion_benford`: se le afirmaba un
# problema de calidad a una columna que no tiene distribucion que analizar.

test_that("una clave dispersa se reconoce como identificador y no recibe Benford", {
  # Diez mil valores unicos repartidos en un rango de seiscientos mil. La
  # densidad da 0,017 -muy por debajo del medio que pide la numeracion densa- y
  # los huecos son irregulares, asi que tambien abria salto de escala. Por las
  # dos puertas quedaba afuera, y Benford se disparaba sobre la clave.
  set.seed(3)
  clave <- sort(sample.int(600000L, 10000L))
  perfil <- perfilar(
    data.frame(MEsId = clave), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_false("desviacion_benford" %in% tipos)
  expect_true("posible_identificador" %in% tipos)
})

test_that("un monto unico por casualidad sigue siendo una magnitud", {
  # El riesgo de mirar la unicidad: hasta unos cien valores, un monto sale
  # unico solo, sin que eso signifique nada. Lo que separa no es la unicidad
  # sino si el azar la explica: aqui espera 0,08 coincidencias, asi que no
  # haber tenido ninguna no dice nada.
  set.seed(3)
  monto <- round(stats::rnorm(100L, 45000, 12000))
  monto <- monto[monto > 0]
  perfil <- perfilar(
    data.frame(monto = monto), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_equal(perfil$columnas$tasa_distintos, 1)
  expect_false(.parece_identificador_numerico(perfil$columnas))
})

test_that("la densidad no podia separar estos casos, y por eso hace falta otra senal", {
  # Es el hecho que obliga al criterio nuevo: la clave es MAS dispersa que el
  # monto, asi que ningun umbral de densidad los distingue. Si esta prueba
  # falla, la premisa del criterio dejo de valer.
  set.seed(3)
  clave <- sort(sample.int(2300000L, 10000L))
  monto <- round(stats::rlnorm(10000L, 9, 1.2))
  densidad <- function(v) {
    p <- perfilar(
      data.frame(x = v), analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE
    )
    p$columnas$densidad_secuencia_entera
  }
  expect_lt(densidad(clave), densidad(monto))
})

test_that("un codigo de catalogo sigue siendo una numeracion", {
  # La otra direccion: el criterio nuevo no puede sacarle el reconocimiento a
  # lo que ya funcionaba. Un codigo repetido tiene tasa 0,03 y no entra por la
  # puerta de la unicidad, pero su densidad es 1 y entra por la de siempre.
  set.seed(3)
  codigo <- sample.int(284L, 2000L, replace = TRUE)
  perfil <- perfilar(
    data.frame(cod = codigo), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  # Se afirma el mecanismo y no un umbral ajustado a este fixture: la columna
  # no es unica -asi que la puerta nueva no la deja pasar- y su densidad es
  # alta -asi que entra por la de siempre-.
  expect_lt(perfil$columnas$tasa_distintos, 1)
  expect_gte(perfil$columnas$densidad_secuencia_entera, 0.5)
  expect_true(.parece_identificador_numerico(perfil$columnas))
})
