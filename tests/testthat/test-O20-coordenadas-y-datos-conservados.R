# O20. Dos afirmaciones que hablaban de un objeto que ya no era.
#
#   * `perfilar_por()` perfila cada grupo sobre su rebanada, y la trazabilidad
#     publicaba los indices de ESA rebanada con `localizador = "indice_fila"` y
#     `alcance = "completo"`. Contra la tabla que el usuario tiene -la unica que
#     tiene- senalaban filas inocentes: en un grupo de 102 filas, el hallazgo de
#     las filas 199 a 202 salia como 99 a 102. La documentacion habla de usar
#     los indices sobre los datos originales.
#
#   * `guardar_analisis(incluir_datos = FALSE)` deja un objeto sin datos, y su
#     `meta$persistencia$datos_incluidos` lo declara, pero `datos_conservados`
#     seguia en TRUE -el valor de cuando se creo el analisis- y el informe HTML
#     del objeto releido publicaba "datos conservados: TRUE".

test_that("los indices por grupo apuntan a la tabla que el usuario tiene", {
  n <- 300L
  datos <- data.frame(
    region = rep(c("norte", "sur"), each = n / 2L),
    suc = paste0("s", seq_len(n)),
    monto = as.numeric(seq_len(n)),
    stringsAsFactors = FALSE
  )
  # Dos filas duplicadas exactas dentro del grupo "sur", al final de la tabla.
  datos$suc[[299L]] <- datos$suc[[297L]]
  datos$monto[[299L]] <- datos$monto[[297L]]
  datos$suc[[300L]] <- datos$suc[[298L]]
  datos$monto[[300L]] <- datos$monto[[298L]]

  por_grupo <- perfilar_por(datos, "region", min_filas = 10L)
  fila <- por_grupo[
    por_grupo$grupo == "sur" & por_grupo$tipo_hallazgo == "filas_duplicadas", ,
    drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  traza <- fila$trazabilidad[[1L]]
  expect_identical(traza$estado, "disponible")
  indices <- traza$indices_fila
  expect_true(length(indices) > 0L)
  # Estan en el marco de la tabla original: son filas del final, no del comienzo.
  expect_true(all(indices > n / 2L))
  # Y las filas que senalan son de verdad las duplicadas.
  senaladas <- datos[indices, c("suc", "monto")]
  expect_true(any(duplicated(senaladas)))
})

test_that("un analisis guardado sin datos no dice que los conserva", {
  datos <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  analisis <- analizar(datos, nombre = "prueba", conservar_datos = TRUE)
  expect_true(analisis$meta$datos_conservados)

  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(analisis, archivo, incluir_datos = FALSE)
  releido <- leer_analisis(archivo)

  expect_null(releido$datos)
  expect_false(releido$meta$persistencia$datos_incluidos)
  expect_false(releido$meta$datos_conservados)

  html <- tempfile(fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  reportar(releido, archivo = html)
  texto <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  expect_match(texto, "datos conservados: FALSE", fixed = TRUE)
  expect_false(grepl("datos conservados: TRUE", texto, fixed = TRUE))
})

test_that("guardado CON datos sigue diciendo que los conserva", {
  # El control: el arreglo no puede apagar la declaracion cuando es cierta.
  datos <- data.frame(a = c(1, 2, 3), stringsAsFactors = FALSE)
  analisis <- analizar(datos, nombre = "prueba", conservar_datos = TRUE)
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(analisis, archivo, incluir_datos = TRUE)
  releido <- leer_analisis(archivo)
  expect_false(is.null(releido$datos))
  expect_true(releido$meta$persistencia$datos_incluidos)
  expect_true(releido$meta$datos_conservados)
})
