datos_sucios <- data.frame(
  id = c(1, 2, 3, 1),
  fecha = c("2020-01-31", "31/01/2020", "2020-03-01", "2020-01-31"),
  categoria = c("A", "S/D", "", "A"),
  constante = rep("UY", 4),
  monto = c(10, -99, 0, 10),
  id_copia = c(1, 2, 3, 1),
  stringsAsFactors = FALSE
)

test_that("perfilar crea la estructura S3 completa", {
  resultado <- perfilar(
    datos_sucios,
    columnas_sin_ceros = "monto",
    columnas_no_negativas = "monto"
  )

  expect_s3_class(resultado, "perfil")
  expect_named(resultado, c(
    "general", "columnas", "patrones", "formatos_fecha", "dependencias",
    "hallazgos", "meta"
  ))
  expect_equal(resultado$general$filas, 4L)
  expect_equal(resultado$general$columnas, 6L)
  expect_equal(resultado$general$celdas, 24L)
  expect_type(resultado$general$celdas, "double")
  expect_equal(resultado$general$filas_duplicadas, 1L)
  expect_equal(nrow(resultado$columnas), 6L)
  expect_equal(names(resultado$patrones), names(datos_sucios))
  expect_equal(names(resultado$formatos_fecha), names(datos_sucios))
})

test_that("las proporciones están en la escala unitaria", {
  resultado <- perfilar(datos_sucios)
  proporciones <- resultado$columnas[c(
    "prop_faltantes", "prop_faltantes_disfrazados", "prop_faltantes_totales",
    "tasa_distintos", "proporcion_tipo_inferido"
  )]
  valores <- unlist(proporciones, use.names = FALSE)
  valores <- valores[is.finite(valores)]
  expect_true(all(valores >= 0 & valores <= 1))
})

test_that("el muestreo no limita las métricas calculadas sobre todas las filas", {
  datos <- data.frame(x = c(rep("dato", 150), rep(" dato", 50)))
  resultado <- perfilar(datos, muestra = 20)

  expect_equal(resultado$meta$filas_analizadas, 20)
  expect_true(resultado$meta$muestreo)
  expect_equal(resultado$columnas$n_espacios_borde, 50L)
  expect_equal(resultado$columnas$n, 200L)

  completo <- perfilar(datos, muestra = Inf)
  expect_equal(completo$meta$filas_analizadas, 200)
  expect_false(completo$meta$muestreo)
})

test_that("se detectan faltantes disfrazados y blancos", {
  resultado <- perfilar(datos_sucios)
  categoria <- resultado$columnas[resultado$columnas$columna == "categoria", ]

  expect_equal(categoria$n_faltantes, 0L)
  expect_equal(categoria$n_faltantes_disfrazados, 2L)
  expect_equal(categoria$prop_faltantes_totales, 0.5)
  expect_equal(categoria$n_blancos, 1L)
  expect_true(any(
    resultado$hallazgos$columna == "categoria" &
      resultado$hallazgos$tipo_hallazgo == "faltantes_disfrazados"
  ))
})

test_that("los hallazgos mínimos son objetos filtrables", {
  resultado <- perfilar(
    datos_sucios,
    columnas_sin_ceros = "monto",
    columnas_no_negativas = "monto"
  )
  tipos <- resultado$hallazgos$tipo_hallazgo

  expect_true(all(c(
    "constante", "faltantes", "faltantes_disfrazados",
    "formatos_fecha_mixtos", "tipo_declarado_distinto",
    "ceros_no_permitidos", "negativos_no_permitidos",
    "filas_duplicadas", "columnas_duplicadas"
  ) %in% tipos))
  expect_true(is.ordered(resultado$hallazgos$severidad))
  expect_equal(
    levels(resultado$hallazgos$severidad),
    c("ok", "sospechoso", "error")
  )
  mixto <- resultado$hallazgos[tipos == "formatos_fecha_mixtos", ]
  expect_true(all(mixto$severidad == "error"))
})

test_that("se detectan identificadores, patrones raros y outliers", {
  datos <- data.frame(
    id = sprintf("ID%03d", 1:100),
    codigo = c(rep("AB123", 99), "MAL!"),
    valor = c(rep(10, 99), 10000),
    stringsAsFactors = FALSE
  )
  resultado <- perfilar(datos)
  tipos <- resultado$hallazgos$tipo_hallazgo

  expect_true("posible_identificador" %in% tipos)
  expect_true("patron_raro" %in% tipos)
  expect_true("outliers" %in% tipos)
  raro <- resultado$hallazgos[tipos == "patron_raro", "evidencia"]
  expect_match(raro, "MAL!", fixed = TRUE)
})

test_that("las métricas numéricas, textuales y de fecha están disponibles", {
  datos <- data.frame(
    texto = c("a", "abcd", NA),
    numero = c(1, 2, 100),
    fecha = as.Date(c("2020-01-01", "2020-01-03", "2020-01-02"))
  )
  resultado <- perfilar(datos)$columnas
  texto <- resultado[resultado$columna == "texto", ]
  numero <- resultado[resultado$columna == "numero", ]
  fecha <- resultado[resultado$columna == "fecha", ]

  expect_equal(texto$longitud_minima, 1)
  expect_equal(texto$longitud_maxima, 4)
  expect_equal(numero$minimo, 1)
  expect_equal(numero$maximo, 100)
  expect_equal(numero$mediana, 2)
  expect_equal(fecha$minimo_fecha, "2020-01-01")
  expect_equal(fecha$maximo_fecha, "2020-01-03")
})

test_that("las fechas de texto y Date comparten unidades temporales", {
  texto <- perfilar(
    data.frame(f = c("2023-11-30", "2023-12-01", "2023-06-15")),
    analizar_dependencias = FALSE
  )$columnas
  fecha <- perfilar(
    data.frame(f = as.Date(c("2023-11-30", "2023-12-01", "2023-06-15"))),
    analizar_dependencias = FALSE
  )$columnas

  expect_equal(texto$minimo_fecha, "2023-06-15")
  expect_equal(texto$maximo_fecha, "2023-12-01")
  expect_equal(texto[c("minimo_fecha", "maximo_fecha", "media_fecha")],
               fecha[c("minimo_fecha", "maximo_fecha", "media_fecha")])
  anios <- as.integer(substr(
    perfilar(datos_administrativos, analizar_dependencias = FALSE)$columnas$minimo_fecha,
    1L, 4L
  ))
  expect_true(all(anios[!is.na(anios)] >= 1900L & anios[!is.na(anios)] <= 2100L))
})

test_that("los métodos de presentación devuelven el resumen", {
  resultado <- perfilar(datos_sucios)
  expect_equal(summary(resultado), resultado$columnas)
  expect_equal(as.data.frame(resultado), resultado$columnas)
  salida <- suppressMessages(capture.output(print(resultado)))
  expect_match(paste(salida, collapse = "\n"), "columna")
  archivo <- reportar(resultado)
  expect_true(file.exists(archivo))
  unlink(archivo)
  expect_error(reportar(datos_sucios), "Cada objeto")
})

test_that("la conversión opcional a tibble conserva una fila por columna", {
  skip_if_not_installed("tibble")
  resultado <- perfilar(datos_sucios)
  convertido <- tibble::as_tibble(resultado)
  expect_s3_class(convertido, "tbl_df")
  expect_equal(nrow(convertido), ncol(datos_sucios))
  expect_false("as_tibble.perfil" %in% getNamespaceExports("lupa"))
  expect_true(is.function(getS3method(
    "as_tibble", "perfil", envir = asNamespace("tibble")
  )))
})

test_that("se aceptan tibble y data.table sin cambiar su contenido", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")
  tib <- tibble::as_tibble(datos_sucios)
  dt <- data.table::as.data.table(datos_sucios)

  expect_s3_class(perfilar(tib), "perfil")
  expect_s3_class(perfilar(dt), "perfil")
  expect_equal(perfilar(dt)$general$filas, nrow(datos_sucios))
})

test_that("se validan datos y umbrales", {
  expect_error(perfilar(1:3), "data.frame")
  expect_error(perfilar(datos_sucios, umbral_alta_cardinalidad = 2), "entre 0 y 1")
  expect_error(
    perfilar(datos_sucios, umbral_casi_clave_dependencia = 2), "umbrales"
  )
  expect_error(
    perfilar(
      datos_sucios,
      umbral_faltantes_sospechoso = 0.8,
      umbral_faltantes_error = 0.2
    ),
    "no puede ser menor"
  )
})

test_that("los umbrales de faltantes se comparan en sentido estricto", {
  exacto <- perfilar(
    data.frame(x = c(NA, 1:9)), analizar_dependencias = FALSE
  )
  expect_false("faltantes" %in% exacto$hallazgos$tipo_hallazgo)

  superior <- perfilar(
    data.frame(x = c(NA, NA, 1:8)), analizar_dependencias = FALSE
  )
  expect_true("faltantes" %in% superior$hallazgos$tipo_hallazgo)
})

test_that("un data frame sin columnas conserva una salida manipulable", {
  resultado <- perfilar(data.frame())
  expect_s3_class(resultado, "perfil")
  expect_equal(nrow(summary(resultado)), 0L)
  expect_true(all(c("prop_faltantes_totales", "n_outliers") %in%
    names(summary(resultado))))
  salida <- suppressMessages(capture.output(print(resultado)))
  expect_match(paste(salida, collapse = "\n"), "columna")
})
