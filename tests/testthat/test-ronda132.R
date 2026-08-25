# `muestra = Inf` pide la tabla entera para el perfil de muestra. Existe porque
# sin eso habia que averiguar antes cuantas filas tiene la tabla y pasarlas a
# mano: se podia perfilar todo, pero no se podia *decir* "todo".
#
# Importa mas de lo que parece. El resumen de tabla no se muestrea nunca -con
# `modo = "exacto"` se agrega en el motor sobre todas las filas-, pero los
# diagnosticos que necesitan los valores en R salen de esta muestra, y sin
# `orden_muestra` son las PRIMERAS filas que devuelva el motor, no una muestra
# aleatoria. Un defecto que viva al final de la tabla no se ve.

test_that("muestra = Inf trae la tabla entera y lo declara", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(9)
  n <- 5000L
  datos <- data.frame(
    id = seq_len(n),
    nombre = paste0("EMPRESA ", sample(500L, n, TRUE)),
    stringsAsFactors = FALSE
  )
  # El defecto vive al final: las primeras filas nunca lo alcanzan.
  datos$nombre[[n - 3L]] <- "EMPRESA  7"
  DBI::dbWriteTable(con, "t", datos)

  acotado <- perfilar_dbi(con, "t", muestra = 100L)
  entero <- perfilar_dbi(con, "t", muestra = Inf)

  m_acotado <- acotado$perfil_muestra$meta$origen_dbi$muestreo
  m_entero <- entero$perfil_muestra$meta$origen_dbi$muestreo

  expect_equal(m_acotado$filas_obtenidas, 100)
  expect_false(m_acotado$tabla_completa)
  expect_true(grepl("LIMIT", m_acotado$sql_muestra, fixed = TRUE))

  expect_equal(m_entero$filas_obtenidas, n)
  expect_true(m_entero$tabla_completa)
  # Sin `LIMIT`: es la diferencia observable, no solo una etiqueta.
  expect_false(grepl("LIMIT", m_entero$sql_muestra, fixed = TRUE))

  # Y el resumen de tabla es exacto en los dos: no es lo que esta muestra decide.
  expect_equal(
    acotado$resumen_tabla$meta$filas, entero$resumen_tabla$meta$filas
  )
})

test_that("el defecto del final aparece con Inf y no con el valor acotado", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(9)
  n <- 5000L
  datos <- data.frame(
    id = seq_len(n),
    nombre = paste0("EMPRESA ", sample(500L, n, TRUE)),
    stringsAsFactors = FALSE
  )
  datos$nombre[[n - 3L]] <- "EMPRESA  7"
  DBI::dbWriteTable(con, "t", datos)

  visto <- function(m) {
    p <- perfilar_dbi(con, "t", muestra = m)
    any(grepl("EMPRESA  7", unlist(p$perfil_muestra$patrones), fixed = TRUE)) ||
      p$perfil_muestra$meta$origen_dbi$muestreo$filas_obtenidas >= n - 3L
  }
  expect_false(visto(100L))
  expect_true(visto(Inf))
})

test_that("muestra invalida se rechaza nombrando la salida", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:5))
  # El mensaje tiene que nombrar `Inf`, porque es la opcion que antes no existia
  # y nadie va a adivinar que ahora si.
  expect_error(perfilar_dbi(con, "t", muestra = 0L), "Inf")
  expect_error(perfilar_dbi(con, "t", muestra = -3L), "Inf")
  expect_error(perfilar_dbi(con, "t", muestra = 2.5), "Inf")
  expect_error(perfilar_dbi(con, "t", muestra = NA_real_), "Inf")
})
