# Tercera evaluacion real: lo que dice perfilar_dbi() de si mismo.

.estimacion_moda_O110 <- function(metodo_lote = "hash", metodo = NULL) {
  lotes <- data.frame(
    lote = 1L, columnas = "codigo", tamano_estimado_bytes = 1e9,
    supera_memoria = TRUE, stringsAsFactors = FALSE
  )
  if (!is.null(metodo_lote)) lotes$metodo <- metodo_lote
  list(
    estado = "estimado", disponible = TRUE, es_estimacion = TRUE,
    familia = "moda", supera_memoria = TRUE, lotes_sobre_memoria = 1L,
    lotes = lotes, work_mem = "4MB", memoria_efectiva = "8MB",
    fuente = "pg_stats", motivo = "estimacion", metodo = metodo
  )
}

test_that("el aviso de derrame de la moda nombra el metodo de sus lotes", {
  mensajes <- utils::capture.output(
    lupa:::.avisar_derrame_estimado_postgresql_dbi(
      .estimacion_moda_O110(), habilitado = TRUE, umbral_bytes = 1
    ),
    type = "message"
  )
  # Antes: "Metodo: ." -el metodo es de cada lote, no de la estimacion-.
  expect_true(any(grepl("Metodo: hash.", mensajes, fixed = TRUE)))
  expect_false(any(grepl("Metodo: .", mensajes, fixed = TRUE)))
  # Control: sin metodo en los lotes, el de la estimacion.
  mensajes <- utils::capture.output(
    lupa:::.avisar_derrame_estimado_postgresql_dbi(
      .estimacion_moda_O110(metodo_lote = NULL, metodo = "sort"),
      habilitado = TRUE, umbral_bytes = 1
    ),
    type = "message"
  )
  expect_true(any(grepl("Metodo: sort.", mensajes, fixed = TRUE)))
})

test_that("el aviso de costo de la moda no deja un espacio antes del punto", {
  proyeccion <- list(
    disponible = TRUE, duracion_estimada_ms = 1e7, n_columnas = 2L,
    fuente = "una fuente", fuentes_cardinalidad = "medicion de la corrida",
    columnas_sin_cardinalidad = character()
  )
  mensajes <- utils::capture.output(
    lupa:::.avisar_costo_moda_dbi(proyeccion, habilitado = TRUE,
                                  umbral_segundos = 1),
    type = "message"
  )
  expect_true(any(grepl("medicion de la corrida.", mensajes, fixed = TRUE)))
  expect_false(any(grepl("corrida .", mensajes, fixed = TRUE)))
})

test_that("el motor dice que tapo y no afirma una media que no estaba", {
  resumen <- list(
    meta = list(motor = list()),
    columnas = data.frame(
      columna = c("doc", "edad"), minimo = c(1, 18), maximo = c(9, 90),
      media = c(NA, 50), mediana = c(5, 50), moda = c("x", "y"),
      stringsAsFactors = FALSE
    )
  )
  protegido <- lupa:::.proteger_resumen_dbi(resumen, "doc", "prueba")$columnas
  # Antes: "[estadisticos de orden y la media protegidos]" sin media que tapar.
  expect_identical(
    protegido$detalle_proteccion_personal[protegido$columna == "doc"],
    "[estadisticos de orden protegidos]"
  )
  expect_true(is.na(protegido$detalle_proteccion_personal[protegido$columna == "edad"]))
})

test_that("con orden y media tapados, motor y memoria dicen lo mismo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(3)
  cedula <- function(n) {
    base <- sample(1000000:6999999, n)
    vapply(base, function(b) {
      d <- as.integer(strsplit(sprintf("%07d", b), "")[[1]])
      v <- (10 - sum(d * c(2, 9, 8, 7, 6, 3, 4)) %% 10) %% 10
      as.numeric(paste0(b, v))
    }, numeric(1))
  }
  datos <- data.frame(id = 1:300, documento = cedula(300),
                      edad = sample(18:90, 300, TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", datos)
  memoria <- as.data.frame(columnas(suppressWarnings(perfilar(datos))))
  motor <- as.data.frame(columnas(suppressWarnings(perfilar_dbi(con, "personas"))))
  detalle <- function(x) {
    as.character(x$detalle_proteccion_personal[x$columna == "documento"])
  }
  # Control: aca las dos tapan orden y media, y el texto ya coincidia.
  expect_identical(detalle(motor), detalle(memoria))
  expect_true(all(is.na(motor[motor$columna == "documento", c("minimo", "maximo", "media")])))
})
