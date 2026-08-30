.tabla_ronda158 <- function(repetidas = FALSE) {
  base <- data.frame(
    codigo = c("a01", "a02", "a03", "a04", "a05", "a06"),
    con_na = c(NA_real_, 10, 20, NA_real_, 40, 50),
    factor = factor(
      c("rojo", "azul", NA, "verde", "rojo", "azul"),
      levels = c("azul", "rojo", "verde"), exclude = NULL
    ),
    fecha = as.Date("2020-01-01") + 0:5,
    entero64 = bit64::as.integer64(101:106),
    lista = I(lapply(seq_len(6L), function(i) c(i, i + 1L))),
    stringsAsFactors = FALSE
  )
  if (repetidas) rbind(base, base[1:3, , drop = FALSE]) else base
}

.conteos_base_ronda158 <- function(datos) {
  adelante <- base::duplicated.data.frame(datos)
  atras <- base::duplicated.data.frame(datos, fromLast = TRUE)
  c(
    filas_duplicadas = sum(adelante),
    filas_en_grupos_duplicados = sum(adelante | atras)
  )
}

test_that("el contador repliega las columnas de lista a base", {
  skip_if_not_installed("bit64")
  datos <- .tabla_ronda158(repetidas = TRUE)
  esperado <- .conteos_base_ronda158(datos)

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(unname(unlist(resultado)), unname(esperado))
  expect_false(anyNA(unlist(resultado)))
})

test_that("el contador conserva una fila por elemento con matrices", {
  datos <- data.frame(
    codigo = c("a", "a", "b"),
    matriz = I(rbind(c(1, 2), c(1, 2), c(3, 4)))
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(
    unname(unlist(resultado)),
    unname(c(filas_duplicadas = 1L, filas_en_grupos_duplicados = 2L))
  )
})

test_that("las tablas chicas no pagan la via rapida", {
  datos <- data.frame(a = seq_len(10L), b = rev(seq_len(10L)))
  llamada <- FALSE
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      NULL
    },
    .package = "lupa"
  )

  lupa:::.conteos_filas_duplicadas(datos)

  expect_false(llamada)
})

test_that("las tablas suficientes usan la via rapida y dan lo mismo que base", {
  datos <- data.frame(a = rep(c("a", "b"), 15L), b = seq_len(30L))
  llamada <- FALSE
  original <- lupa:::.filas_duplicadas_frank
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      original(datos)
    },
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_true(llamada)
  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("un fallo inesperado de la via rapida repliega a base", {
  # El resultado exacto lo fija `duplicated()`. Si el atajo falla, la respuesta
  # sigue siendo la de base y no un ausente.
  datos <- data.frame(a = rep(c("a", "b"), 15L), b = rep(seq_len(15L), 2L))
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) stop("fallo de prueba"),
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("la via rapida y la de base coinciden en los tipos y bordes", {
  # `frank()` ordena y `duplicated.data.frame` pega cadenas: los casos donde
  # ordenar e imprimir difieren son los que hay que fijar.
  casos <- list(
    texto = data.frame(a = rep(c("a", "b", NA), 10L), stringsAsFactors = FALSE),
    doble_infinitos = data.frame(a = rep(c(1.5, Inf, -Inf, NA), 8L)),
    factor = data.frame(a = factor(rep(c("a", "b", NA), 10L))),
    logico = data.frame(a = rep(c(TRUE, FALSE, NA), 10L)),
    fecha = data.frame(a = as.Date("2020-01-01") + rep(c(0L, 1L, NA), 10L)),
    momento = data.frame(
      a = as.POSIXct("2020-01-01", tz = "UTC") + rep(c(0L, 1L, NA), 10L)
    ),
    cero_y_menos_cero = data.frame(a = rep(c(0, -0, 1), 10L)),
    dos_columnas = data.frame(
      a = rep(c("x", "y"), 15L), b = rep(c(1L, 1L, 2L), 10L),
      stringsAsFactors = FALSE
    )
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    expect_identical(
      unname(unlist(lupa:::.conteos_filas_duplicadas(datos))),
      unname(.conteos_base_ronda158(datos)),
      info = nombre
    )
  }
})

test_that("una tabla con NaN se mide por la via de base", {
  # `frank()` no distingue `NaN` de `NA`; `duplicated.data.frame` si. Donde hay
  # un `NaN` manda la semantica de base, que es la que fija el resultado.
  datos <- data.frame(a = rep(c(NA_real_, NaN, 1), 10L))
  expect_true(lupa:::.tiene_nan_en_dobles(datos))
  llamada <- FALSE
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      NULL
    },
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_false(llamada)
  expect_identical(
    unname(unlist(resultado)),
    unname(.conteos_base_ronda158(datos))
  )
})

test_that("perfilar deja identico el data.frame y data.table de entrada", {
  skip_if_not_installed("bit64")
  for (repetidas in c(FALSE, TRUE)) {
    datos_df <- .tabla_ronda158(repetidas)
    datos_dt <- data.table::as.data.table(datos_df)
    for (datos in list(datos_df, datos_dt)) {
      antes <- data.table::copy(datos)
      resultado <- perfilar(
        datos,
        analizar_dependencias = FALSE,
        casi_duplicados_vocabulario = FALSE
      )

      expect_s3_class(resultado, "perfil")
      expect_identical(datos, antes)
    }
  }
})

test_that("los conteos rapidos coinciden con base sin filas repetidas", {
  skip_if_not_installed("bit64")
  datos <- .tabla_ronda158(repetidas = FALSE)
  esperado <- .conteos_base_ronda158(datos)
  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_identical(unname(unlist(resultado)), unname(esperado))
})

test_that("el acelerador respeta la semantica de base con cualquier redondeo", {
  # `setNumericRounding()` es un ajuste GLOBAL de la sesion que cambia cuantos
  # bits se comparan de un doble al ordenar. `frank()` ordena, asi que con 1 o 2
  # agrupa valores que `duplicated()` distingue. Cualquier otro paquete de la
  # sesion puede haberlo cambiado.
  skip_if_not_installed("data.table")
  anterior <- data.table::getNumericRounding()
  on.exit(data.table::setNumericRounding(anterior), add = TRUE)

  eps <- .Machine$double.eps
  casos <- list(
    dobles_contiguos = data.frame(
      x = rep(c(1, 1 + eps, 2, 2 + eps), 10L),
      y = rep(c("a", "a", "b", "b"), 10L),
      stringsAsFactors = FALSE
    ),
    una_columna_doble = data.frame(x = rep(c(1, 1 + eps, 1 + 2 * eps), 15L)),
    momentos_submicro = data.frame(
      t = rep(as.POSIXct("2020-01-01", tz = "UTC") + c(0, 1e-6, 2e-6), 15L)
    ),
    magnitud_grande = data.frame(
      x = rep(c(2^40, 2^40 + 1e-3, 2^40 + 2e-3), 15L)
    )
  )

  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    for (redondeo in 0:2) {
      data.table::setNumericRounding(redondeo)
      expect_identical(
        unname(unlist(lupa:::.conteos_filas_duplicadas(datos))),
        unname(.conteos_base_ronda158(datos)),
        info = paste(nombre, "con redondeo", redondeo)
      )
    }
  }
})

test_that("una tabla sin dobles no paga la guarda de redondeo", {
  # El ajuste solo puede cambiar la comparacion de dobles. Si no hay ninguno, la
  # via rapida tiene que seguir corriendo aunque la sesion este en 2.
  skip_if_not_installed("data.table")
  anterior <- data.table::getNumericRounding()
  on.exit(data.table::setNumericRounding(anterior), add = TRUE)
  datos <- data.frame(
    a = rep(c("x", "y", "z"), 15L), b = rep(1:3, 15L), stringsAsFactors = FALSE
  )

  data.table::setNumericRounding(2L)

  expect_true(lupa:::.redondeo_numerico_es_exacto(datos))
  llamada <- FALSE
  original <- lupa:::.filas_duplicadas_frank
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      original(datos)
    },
    .package = "lupa"
  )

  lupa:::.conteos_filas_duplicadas(datos)

  expect_true(llamada)
})

test_that("con dobles y redondeo inexacto se mide por la via de base", {
  skip_if_not_installed("data.table")
  anterior <- data.table::getNumericRounding()
  on.exit(data.table::setNumericRounding(anterior), add = TRUE)
  datos <- data.frame(x = rep(c(1.5, 2.5, 3.5), 15L))

  data.table::setNumericRounding(1L)

  expect_false(lupa:::.redondeo_numerico_es_exacto(datos))
  llamada <- FALSE
  local_mocked_bindings(
    .filas_duplicadas_frank = function(datos) {
      llamada <<- TRUE
      NULL
    },
    .package = "lupa"
  )

  resultado <- lupa:::.conteos_filas_duplicadas(datos)

  expect_false(llamada)
  expect_identical(
    unname(unlist(resultado)), unname(.conteos_base_ronda158(datos))
  )
})

test_that("un metodo de duplicated que ignora fromLast no cambia el conteo", {
  # `bit64` devuelve para `integer64` el mismo vector con `fromLast = TRUE` que
  # sin el. La via de base heredaba ese defecto y la rapida no, asi que la
  # respuesta cambiaba segun el umbral de filas. Ahora las dos dan vuelta las
  # filas en vez de confiar en el argumento.
  skip_if_not_installed("bit64")
  valores <- rep(bit64::as.integer64(c(100, 200, 300)), 10L)
  datos <- data.frame(x = valores)

  # 30 filas, 3 valores distintos: todas participan en grupos repetidos.
  for (n in c(10L, 30L)) {
    parcial <- datos[seq_len(n), , drop = FALSE]
    resultado <- lupa:::.conteos_filas_duplicadas(parcial)
    expect_identical(
      as.integer(resultado$filas_en_grupos_duplicados), n,
      info = paste(n, "filas")
    )
    expect_identical(
      as.integer(resultado$filas_duplicadas), n - 3L,
      info = paste(n, "filas")
    )
  }

  # Y las dos vias tienen que coincidir, que es lo que el umbral rompia.
  base <- lupa:::.filas_duplicadas_base(datos)
  rapida <- lupa:::.filas_duplicadas_frank(datos)
  expect_identical(base$adelante, rapida$adelante)
  expect_identical(base$atras, rapida$atras)
})

test_that("la traza de filas duplicadas incluye todos los participantes integer64", {
  skip_if_not_installed("bit64")
  esperado <- 2:5
  datos_doble <- data.frame(id = c(1, 2, 2, 3, 3))
  datos_integer64 <- data.frame(
    id = bit64::as.integer64(c(1, 2, 2, 3, 3))
  )

  for (datos in list(datos_doble, datos_integer64)) {
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE,
      max_filas_hallazgo = Inf
    )
    hallazgo <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "filas_duplicadas", , drop = FALSE
    ]
    expect_equal(hallazgo$n_afectados, 4)
    expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, esperado)
    expect_equal(hallazgo$trazabilidad[[1L]]$total, 4)
  }
})
