# O25: la proteccion debe mantenerse entre las puertas que publican un
# resumen temporal, un analisis persistido y un perfil DBI.

test_that("analizar_tiempo respeta la proteccion del perfil y sin perfil no", {
  datos <- data.frame(
    id = 1:30,
    nacimiento = as.Date("1960-01-01") + 1:30,
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, columnas_personales = "nacimiento",
    analizar_dependencias = FALSE
  )
  temporal <- analizar_tiempo(datos, perfil = perfil, columnas = "nacimiento")
  fila <- temporal$resumen[temporal$resumen$columna == "nacimiento", , drop = FALSE]

  expect_true(isTRUE(perfil$columnas$dato_personal_protegido[
    perfil$columnas$columna == "nacimiento"
  ]))
  expect_true(is.na(fila$fecha_minima))
  expect_true(is.na(fila$fecha_maxima))
  expect_equal(fila$proteccion_temporal, "[rangos y huecos protegidos]")

  # Control: sin perfil no existe una declaracion de proteccion que leer.
  sin_perfil <- analizar_tiempo(datos, columnas = "nacimiento")
  fila_sin_perfil <- sin_perfil$resumen[
    sin_perfil$resumen$columna == "nacimiento", , drop = FALSE
  ]
  expect_equal(fila_sin_perfil$fecha_minima, min(datos$nacimiento))
  expect_equal(fila_sin_perfil$fecha_maxima, max(datos$nacimiento))
})

test_that("el guardado declara la proteccion que ya trae el analisis", {
  datos <- data.frame(
    nombre = paste0("Nombre", 1:20, " Apellido", 1:20),
    documento = sprintf("%08d", 1000000 + 1:20),
    stringsAsFactors = FALSE
  )
  protegido <- analizar(
    datos, nombre = "clientes", conservar_datos = TRUE,
    analizar_dependencias = FALSE
  )
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  expect_error(
    guardar_analisis(protegido, archivo, incluir_datos = TRUE),
    "crear el analisis"
  )
  guardar_analisis(
    protegido, archivo, incluir_datos = TRUE,
    proteger_datos_personales = FALSE, sobrescribir = TRUE
  )
  leido <- leer_analisis(archivo)
  expect_true(leido$meta$persistencia$evidencia_protegida)
  expect_true(all(leido$datos$nombre == "[valor protegido]"))
  expect_true(all(leido$datos$documento == "[valor protegido]"))

  # Control: si el analisis no declaro proteccion, el archivo queda abierto.
  sin_proteccion <- analizar(
    datos, nombre = "clientes", conservar_datos = TRUE,
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  archivo_abierto <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo_abierto), add = TRUE)
  guardar_analisis(
    sin_proteccion, archivo_abierto, incluir_datos = TRUE,
    proteger_datos_personales = FALSE
  )
  abierto <- leer_analisis(archivo_abierto)
  expect_false(abierto$meta$persistencia$evidencia_protegida)
  expect_true(abierto$datos$nombre[[1L]] == datos$nombre[[1L]])
})

test_that("DBI declara la duda de una muestra parcial y no la repite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  n <- 3000L
  datos <- data.frame(
    id = seq_len(n),
    apodo = c(paste0("fideo", seq_len(500L), "z"), rep("40056788", 2500L)),
    ciudad = rep("Montevideo", n),
    sueldo = seq_len(n),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "clientes", datos)

  parcial <- suppressWarnings(perfilar_dbi(
    con, "clientes", max_celdas_muestra = 800L,
    analizar_dependencias = FALSE
  ))
  fila_parcial <- parcial$resumen_tabla$columnas[
    parcial$resumen_tabla$columnas$columna == "apodo", , drop = FALSE
  ]
  expect_equal(fila_parcial$moda, "[valor protegido]")
  cobertura <- parcial$resumen_tabla$cobertura
  duda <- cobertura[
    cobertura$bloque == "resumen_tabla" &
      cobertura$elemento == "apodo::moda", , drop = FALSE
  ]
  expect_equal(nrow(duda), 1L)
  expect_match(duda$motivo, "200 de 3000", fixed = TRUE)
  expect_false(grepl("40056788", paste(duda, collapse = " "), fixed = TRUE))
  expect_false(any(
    cobertura$bloque == "corroboracion" &
      cobertura$elemento == "apodo::moda"
  ))

  # Control: con todas las filas clasificadas no se agrega la duda y la cifra
  # conserva el comportamiento previo.
  completa <- suppressWarnings(perfilar_dbi(
    con, "clientes", max_celdas_muestra = Inf,
    analizar_dependencias = FALSE
  ))
  fila_completa <- completa$resumen_tabla$columnas[
    completa$resumen_tabla$columnas$columna == "apodo", , drop = FALSE
  ]
  expect_equal(fila_completa$moda, "40056788")
  expect_false(any(
    completa$resumen_tabla$cobertura$elemento == "apodo::moda"
  ))
})
