# Lo que quedaba abierto de las cinco auditorias externas.

test_that("el desvio de una columna temporal esta en segundos", {
  # Los demas momentos viajan formateados en `minimo_fecha` y companía; el
  # desvio no es un momento sino una duracion, queda como numero, y ese numero
  # esta en segundos porque fecha y fecha-hora se unifican en esa unidad. Sin
  # esta prueba la unidad puede cambiar en silencio, y 86.400 veces es mucho.
  fechas <- as.Date("2020-01-01") + 0:4
  perfil <- perfilar(
    data.frame(fecha = fechas), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas
  expect_identical(as.character(fila$tipo_inferido), "fecha")
  expect_true(is.na(fila$minimo))
  expect_equal(as.character(fila$minimo_fecha), "2020-01-01")
  # sd de 0:4 dias = 1,5811 dias = 136.610,4 segundos
  expect_equal(fila$desvio, stats::sd(as.numeric(fechas) * 86400))
  expect_equal(fila$desvio / 86400, stats::sd(as.numeric(fechas)))
})

test_that("las dos puertas describen el tipo que cada una tiene delante", {
  # No es una discrepancia de calculo: SQLite no preserva `DATE` ni `BOOLEAN`,
  # asi que por la puerta DBI esas columnas son numeros. La prueba fija la
  # divergencia para que quede declarada y no se descubra en produccion.
  skip_if_not_installed("RSQLite")
  datos <- data.frame(
    logico = c(TRUE, FALSE, TRUE, FALSE, NA),
    fecha = as.Date("2020-01-01") + 0:4,
    stringsAsFactors = FALSE
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  en_memoria <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  por_motor <- perfilar_dbi(conexion, "t", proteger_datos_personales = FALSE)
  columnas_motor <- por_motor$perfil_muestra$columnas

  expect_identical(as.character(en_memoria$columnas$tipo_declarado),
                   c("logico", "fecha"))
  expect_identical(as.character(columnas_motor$tipo_declarado),
                   c("entero", "doble"))
  # La moda de la fecha: formateada por una puerta, entero crudo por la otra.
  expect_equal(as.character(en_memoria$columnas$moda[[2L]]), "2020-01-01")
  expect_equal(as.character(columnas_motor$moda[[2L]]), "18262")
  # Y el desvio, en segundos contra dias: el factor es exactamente 86.400.
  expect_equal(
    en_memoria$columnas$desvio[[2L]] / columnas_motor$desvio[[2L]], 86400
  )
})

test_that("las dos etiquetas estadisticas se entienden sin saber estadistica", {
  # Un hallazgo que hay que traducir antes de poder descartarlo cuesta mas que
  # uno que se entiende: quien no sabe que es Benford no lo puede juzgar.
  set.seed(8)
  montos <- c(
    sample(c(9:99, 900:999, 90000:99999, 9000000:9999999), 500L, TRUE),
    sample(c(10:19, 1000:1099), 30L, TRUE)
  )
  perfil <- perfilar(
    data.frame(monto = as.numeric(montos)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  benford <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "desviacion_benford", ,
    drop = FALSE
  ]
  expect_equal(nrow(benford), 1L)
  descripcion <- as.character(benford$descripcion[[1L]])
  # Dice que reparto se esperaba antes de nombrar la ley.
  expect_match(descripcion, "primer digito")
  expect_match(descripcion, "30 %")
  expect_match(descripcion, "no evidencia de fraude")

  atipicos <- perfilar(
    data.frame(x = c(seq_len(60L), 900000L)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )$hallazgos
  atipicos <- atipicos[
    as.character(atipicos$tipo_hallazgo) == "outliers", , drop = FALSE
  ]
  expect_equal(nrow(atipicos), 1L)
  texto <- as.character(atipicos$descripcion[[1L]])
  # Dice que pasa antes de nombrar a Tukey, no despues.
  expect_match(texto, "muy alejados del grueso")
  expect_lt(regexpr("alejados", texto)[[1L]], regexpr("Tukey", texto)[[1L]])
})

test_that("las cuatro funciones que solo llamaba la suite ya no viajan", {
  # Viajaban en cada instalacion sin que nada las usara, y sus pruebas daban la
  # impresion de que estaban vivas.
  muertas <- c(
    ".data_frame_vacio", ".pares_acumulador_duplicados",
    ".evidencia_fila_aproximada", ".comparar_representacion_conversion"
  )
  for (nombre in muertas) {
    expect_false(
      exists(nombre, envir = asNamespace("lupa"), inherits = FALSE),
      info = nombre
    )
  }
  # La vectorizada, que si se usa, sigue estando.
  expect_true(exists(".evidencia_filas_aproximada",
                     envir = asNamespace("lupa"), inherits = FALSE))
})

test_that("una numeracion se describe igual sea integer o double", {
  # La guarda miraba el tipo inferido, y por la puerta DBI casi todo llega como
  # `doble`: el mismo codigo 1..284, con los mismos 179 valores fuera de los
  # limites de Tukey, se callaba guardado como `integer` y se senalaba guardado
  # como `double`. El detector de secuencias ya comprobaba que los valores
  # fueran enteros; exigir ademas el tipo de almacenamiento solo dejaba gente
  # afuera.
  set.seed(9)
  codigos <- c(sample(seq_len(60L), 1900L, TRUE), sample(61:284, 224L, TRUE))
  perfilar_codigo <- function(x) {
    perfilar(
      data.frame(cod = x), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
    )
  }
  como_entero <- perfilar_codigo(as.integer(codigos))
  como_doble <- perfilar_codigo(as.numeric(codigos))

  # Las dos miden los mismos valores extremos...
  expect_gt(como_entero$columnas$n_outliers, 0L)
  expect_equal(como_doble$columnas$n_outliers, como_entero$columnas$n_outliers)
  # ...y las dos miden la misma densidad, que antes era NA para `double`.
  expect_equal(
    como_doble$columnas$densidad_secuencia_entera,
    como_entero$columnas$densidad_secuencia_entera
  )

  senalado <- function(perfil) {
    "outliers" %in% as.character(perfil$hallazgos$tipo_hallazgo[
      as.character(perfil$hallazgos$severidad) != "ok"
    ])
  }
  declarado <- function(perfil) {
    "outliers" %in% as.character(perfil$cobertura_diagnosticos$diagnostico)
  }
  expect_false(senalado(como_entero))
  expect_false(senalado(como_doble))
  expect_true(declarado(como_entero))
  expect_true(declarado(como_doble))
})
