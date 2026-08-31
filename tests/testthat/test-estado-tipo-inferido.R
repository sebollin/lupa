.datos_fecha_ambigua_estado <- function() {
  c("01/02/2020", "03/04/2020", "05/06/2020", "07/08/2020")
}

.perfil_muestra_estado <- function(datos) {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)
  perfilar_dbi(
    conexion, "t", muestra = nrow(datos), bloque_muestra = "con_muestra",
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )$perfil_muestra
}

test_that("una columna 100 por ciento ambigua declara candidato en memoria y DBI", {
  datos <- data.frame(fecha_ambigua = .datos_fecha_ambigua_estado(),
                      stringsAsFactors = FALSE)
  memoria <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila_memoria <- memoria$columnas[1L, , drop = FALSE]
  expect_identical(fila_memoria$tipo_inferido[[1L]], "fecha")
  expect_identical(fila_memoria$estado_tipo_inferido[[1L]], "candidato")
  expect_equal(fila_memoria$proporcion_tipo_inferido[[1L]], 1)

  muestra <- .perfil_muestra_estado(datos)
  fila_muestra <- muestra$columnas[1L, , drop = FALSE]
  expect_identical(fila_muestra$tipo_inferido[[1L]], "fecha")
  expect_identical(fila_muestra$estado_tipo_inferido[[1L]], "candidato")
  expect_equal(fila_muestra$proporcion_tipo_inferido[[1L]], 1)
})

test_that("una columna ISO 100 por ciento es confirmada", {
  datos <- data.frame(fecha = c("2020-01-01", "2020-01-02"),
                      stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_identical(perfil$columnas$estado_tipo_inferido[[1L]], "confirmado")

  muestra <- .perfil_muestra_estado(datos)
  expect_identical(muestra$columnas$estado_tipo_inferido[[1L]], "confirmado")
})

test_that("una columna de texto sin formatos conserva estado NA", {
  datos <- data.frame(texto = c("alpha", "beta", "gamma"),
                      stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  expect_identical(perfil$columnas$tipo_inferido[[1L]], "texto")
  expect_true(is.na(perfil$columnas$estado_tipo_inferido[[1L]]))

  muestra <- .perfil_muestra_estado(datos)
  expect_true(is.na(muestra$columnas$estado_tipo_inferido[[1L]]))
})

test_that("un formato confirmado no salva otro formato candidato", {
  datos <- data.frame(fecha = c(
    "2020-01-02", "2020-03-04", "01/02/2020", "03/04/2020"
  ), stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  expect_identical(fila$tipo_inferido[[1L]], "fecha")
  expect_identical(fila$estado_tipo_inferido[[1L]], "candidato")
})

test_that("print muestra candidato y calla el estado confirmado", {
  datos_ambigua <- data.frame(
    fecha_ambigua = .datos_fecha_ambigua_estado(), stringsAsFactors = FALSE
  )
  ambigua <- perfilar(
    datos_ambigua, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  salida_ambigua <- paste(capture.output(print(ambigua)), collapse = " ")
  expect_match(salida_ambigua, "texto .*fecha \\(candidato\\)")

  confirmada <- perfilar(
    data.frame(fecha = c("2020-01-01", "2020-01-02")),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  salida_confirmada <- paste(capture.output(print(confirmada)), collapse = " ")
  expect_false(grepl("\\(candidato\\)", salida_confirmada))
})

test_that("los canales de fechas mantienen su comportamiento", {
  datos <- data.frame(
    fecha = .datos_fecha_ambigua_estado(), stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  expect_true(is.na(fila$minimo_fecha[[1L]]))
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_identical(as.character(hallazgo$severidad[[1L]]), "sospechoso")

  plan <- planificar_limpieza(perfil, datos)
  accion <- plan[plan$estrategia == "desambiguar_fecha_en_origen", , drop = FALSE]
  expect_equal(nrow(accion), 1L)
  expect_identical(as.character(accion$estado[[1L]]), "bloqueada")
  expect_false(accion$recomendada[[1L]])
})
