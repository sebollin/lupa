# Un `double` subnormal -0 < |x| < .Machine$double.xmin- no es una medicion:
# sale de reinterpretar un patron de bits o de un desbordamiento por defecto.
#
# El caso que lo motivo, medido con DBI y sin lupa de por medio: el controlador
# de duckdb escribe una columna `integer64` como DOUBLE reinterpretando los
# bits, y la base devuelve 1,06e-314 donde el dato era 2147483648. La basura
# queda EN la base, asi que el resumen SQL y la muestra coinciden y la
# corroboracion cruzada no tiene divergencia que declarar: el absurdo se
# publicaba como un dato calculado, con estado sano.

subnormales <- function(n) rep(c(1.06e-314, 2.47e-314), length.out = n)

test_that("una columna con valores subnormales se declara", {
  perfil <- perfilar(data.frame(x = c(subnormales(4), 1, 2)))
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valores_subnormales", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  # `sospechoso` y no `error`: el hallazgo publica un hecho medido -hay valores
  # subnormales- y no un veredicto sobre su origen. Ver el test de abajo.
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_equal(hallazgo$n_afectados, 4)
  expect_equal(hallazgo$n_evaluados, 6)
  # Y dice cuales son las filas: contar sin poder nombrar es lo que la guarda
  # de trazabilidad del paquete persigue.
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 1:4)
})

test_that("el conteo es exacto y no depende de que el valor sea extremo", {
  # Un subnormal entre dos valores normales no aparece en minimo ni en maximo:
  # derivar la senal del resumen se lo habria perdido.
  perfil <- perfilar(data.frame(x = c(-5, 1.06e-314, 5)))
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valores_subnormales", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_afectados, 1)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 2L)
  expect_false(abs(perfil$columnas$minimo) < .Machine$double.xmin)
  expect_false(abs(perfil$columnas$maximo) < .Machine$double.xmin)
})

# Las clases que son `double` POR DEBAJO. La primera bateria de control eran
# todas columnas numericas peladas -homogeneas en justo la propiedad cuya
# ausencia era el fallo- y por eso no vio nada: `Date` y `POSIXct` son dobles y
# `abs()` no esta definido para ellos, asi que la senal abortaba el perfil
# entero de cualquier tabla con una fecha. La guarda mira `oldClass()` y no una
# lista de clases, porque una lista deja afuera la proxima.
test_that("las clases que son doble por debajo no rompen ni disparan", {
  skip_if_not_installed("bit64")
  casos <- list(
    fecha = as.Date(c("2020-01-01", "2021-06-15")),
    fecha_hora = as.POSIXct(
      c("2020-01-01 10:00", "2021-06-15 12:00"), tz = "UTC"
    ),
    diferencia = as.difftime(c(1, 2), units = "days"),
    entero64 = bit64::as.integer64(c(2147483648, 5000000000)),
    factor = factor(c("a", "b")),
    logico = c(TRUE, FALSE),
    complejo = c(1 + 2i, 3 + 4i)
  )
  for (nombre in names(casos)) {
    perfil <- expect_no_error(perfilar(data.frame(x = casos[[nombre]])))
    expect_false(
      "valores_subnormales" %in% perfil$hallazgos$tipo_hallazgo,
      info = nombre
    )
  }
  # Y las mismas clases conviviendo con una columna que SI dispara: el perfil
  # tiene que describir las dos cosas en la misma corrida.
  mezcla <- data.frame(
    f = as.Date(c("2020-01-01", "2021-06-15")),
    h = as.POSIXct(c("2020-01-01 10:00", "2021-06-15 12:00"), tz = "UTC"),
    x = c(1.06e-314, 2.47e-314)
  )
  hallazgos <- perfilar(mezcla)$hallazgos
  propio <- hallazgos[hallazgos$tipo_hallazgo == "valores_subnormales", ]
  expect_equal(nrow(propio), 1L)
  expect_equal(as.character(propio$columna), "x")
})

# La mitad de control, y es la que decide que la senal sirva: no puede haber
# falsos positivos. Se midio sobre datos reales antes de escribirla -140
# columnas numericas de 17 archivos, cero subnormales, y el valor no nulo mas
# chico de todo el banco es 0,001, unas 4,5e304 veces el umbral-.
test_that("nada normal se declara subnormal", {
  casos <- list(
    normal_muy_chico = c(1e-300, 2e-300, 3e-300),
    normal = c(0.001, 1, 1000),
    ceros = c(0, 0, 0),
    enteros = c(1L, 2L, 3L),
    ausentes = as.numeric(c(NA, NA)),
    negativos = c(-1e-300, -1, -1000),
    infinitos = c(Inf, -Inf, 1)
  )
  for (nombre in names(casos)) {
    perfil <- perfilar(data.frame(x = casos[[nombre]]))
    expect_false(
      "valores_subnormales" %in% perfil$hallazgos$tipo_hallazgo,
      info = nombre
    )
  }
  # Un texto tampoco, aunque su forma se parezca.
  perfil <- perfilar(
    data.frame(x = c("1.06e-314", "2e-320"), stringsAsFactors = FALSE)
  )
  expect_false("valores_subnormales" %in% perfil$hallazgos$tipo_hallazgo)
})

test_that("la senal no filtra valores de una columna protegida", {
  datos <- data.frame(
    documento = c(sprintf("%08d", 1:8), "12345678", "12345678"),
    monto = c(subnormales(4), rep(100, 6)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, columnas_personales = "monto")
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valores_subnormales", ,
    drop = FALSE
  ]
  # Primero: el mecanismo se activo. Sin esto no se prueba nada.
  expect_equal(nrow(hallazgo), 1L)
  texto <- paste(
    hallazgo$descripcion, hallazgo$evidencia, hallazgo$sugerencia
  )
  expect_false(grepl("e-314", texto, fixed = TRUE))
  expect_false(grepl("1.06", texto, fixed = TRUE))
})

test_that("la senal cuenta como el resto bajo muestreo y sobrevive al viaje", {
  datos <- data.frame(x = c(subnormales(50), rep(1, 900), rep(-2, 50)))
  for (muestra in list(Inf, 100)) {
    perfil <- perfilar(datos, muestra = muestra, columnas_no_negativas = "x")
    hallazgos <- perfil$hallazgos
    propio <- hallazgos[hallazgos$tipo_hallazgo == "valores_subnormales", ]
    vecino <- hallazgos[hallazgos$tipo_hallazgo == "negativos_no_permitidos", ]
    expect_equal(nrow(propio), 1L)
    expect_equal(nrow(vecino), 1L)
    # Los dos informan sobre la misma poblacion: la tabla entera, con el
    # muestreo declarado aparte en meta.
    expect_equal(propio$n_evaluados, vecino$n_evaluados, info = muestra)
    expect_equal(propio$n_afectados, 50)
  }

  analisis <- analizar(data.frame(x = c(subnormales(4), 1, 2)))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(analisis, archivo)
  leido <- leer_analisis(archivo)
  antes <- analisis$perfil$hallazgos
  despues <- leido$perfil$hallazgos
  expect_equal(
    antes$n_afectados[antes$tipo_hallazgo == "valores_subnormales"],
    despues$n_afectados[despues$tipo_hallazgo == "valores_subnormales"]
  )
})

# La primera version de este diagnostico afirmaba, en el `.Rd` y en NEWS, que
# "no hay magnitud real a esa escala" y lo marcaba como `error`. Es falso: una
# probabilidad de cola calculada cae legitimamente en el rango subnormal, y el
# refutador lo encontro atacando este mismo arreglo. El hallazgo se conserva
# -sigue detectando el caso que lo motivo- pero dice lo que midio y no lo que
# supone.
test_that("el diagnostico no afirma el origen de los valores subnormales", {
  # `2^-1050` da 8,3e-317: aritmetica IEEE pura, subnormal, y el mismo valor en
  # cualquier version de R. La primera version de este test usaba `pnorm(-38)`,
  # que en R moderno da 2,9e-316 y **en R 4.1.0 -el minimo declarado- desborda
  # a cero exacto**: el contenedor del minimo lo encontro con cinco fallos. Un
  # test no puede apoyarse en la precision de una funcion que cambia entre
  # versiones.
  expect_true(2^-1050 > 0)
  expect_true(2^-1050 < .Machine$double.xmin)

  probabilidades <- data.frame(
    pval = c(2^-(1040:1060), runif(50, 1e-5, 1))
  )
  perfil <- perfilar(probabilidades)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valores_subnormales", ,
    drop = FALSE
  ]
  # Se emite -el hecho es cierto- pero como sospecha, no como error.
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_match(hallazgo$descripcion, "No es concluyente")
  expect_match(hallazgo$sugerencia, "no aplica")

  # Y el caso que motivo el diagnostico se sigue detectando.
  bits <- perfilar(data.frame(x = c(1.06e-314, 2.47e-314, 1.5e-314)))
  detectado <- bits$hallazgos[
    bits$hallazgos$tipo_hallazgo == "valores_subnormales", ,
    drop = FALSE
  ]
  expect_equal(nrow(detectado), 1L)
  expect_equal(detectado$n_afectados, 3)
})
