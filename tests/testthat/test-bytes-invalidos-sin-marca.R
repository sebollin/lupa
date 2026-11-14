# Un CSV en latin-1 o Windows-1252 leido con `read.csv()` sin declarar la
# codificacion deja, en una sesion UTF-8, celdas SIN MARCA con bytes que no son
# UTF-8 valido: el espacio duro como separador de miles, un grado, un byte
# suelto. Esas celdas se excluyen del analisis textual y se cuentan en
# `n_codificacion_invalida`; el resto de la columna se describe igual. Varios
# lectores leian la columna cruda en vez del texto analizable, y en una sesion
# UTF-8 `trimws()` o `as.numeric()` abortaban el perfil entero. Bajo `C` se
# leia, asi que lo que dicen estas pruebas es lo que esa sesion ya decia.

testthat::skip_on_cran()

.bytes_sin_marca <- function(...) rawToChar(as.raw(c(...)))

.fecha_bytes <- as.POSIXct("2026-01-15", tz = "UTC")

test_that("un byte suelto en una columna de numeros no aborta el perfil y queda contado", {
  b <- .bytes_sin_marca
  casos <- list(
    # El byte al final de un decimal con coma.
    byte_suelto = list(
      v = c("12,5", "13,1", paste0("14,2", b(0xFF)), "15,0", "16,3", "2,2",
            "3,3", "4,4"),
      fila = 3L, tipo = "doble", minimo = 2.2, maximo = 16.3
    ),
    # El espacio duro de latin-1 como separador de miles.
    espacio_duro = list(
      v = c(paste0("1", b(0xA0), "234,50"), "2.100,00", "350,25", "4.000,10",
            "12,00", "7,50"),
      fila = 1L, tipo = "doble", minimo = 7.5, maximo = 4000.1
    ),
    # El grado de latin-1 detras de un numero.
    grado = list(
      v = c("12,5", paste0("13", b(0xB0)), "14", "15", "16", "17", "18"),
      fila = 2L, tipo = "doble", minimo = 12.5, maximo = 18
    )
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    expect_false(validUTF8(caso$v[[caso$fila]]), info = nombre)
    datos <- data.frame(v = caso$v, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, fecha = .fecha_bytes)
    fila <- as.data.frame(perfil$columnas)
    # El perfil sale con lo que dice el resto de la columna.
    expect_identical(as.character(fila$tipo_inferido), caso$tipo, info = nombre)
    expect_equal(fila$minimo, caso$minimo, info = nombre)
    expect_equal(fila$maximo, caso$maximo, info = nombre)
    # Y la celda ilegible, donde el paquete la cuenta.
    expect_identical(as.integer(fila$n_codificacion_invalida), 1L, info = nombre)
    h <- as.data.frame(hallazgos(perfil))
    expect_true("codificacion_invalida" %in% h$tipo_hallazgo, info = nombre)
    # La evidencia de la conversion dice lo que dice el plan: la celda no se
    # lee y la conversion queda bloqueada. Afirmaba que no se perdia nada.
    evidencia <- h$evidencia[h$tipo_hallazgo == "tipo_declarado_distinto"]
    expect_length(evidencia, 1L)
    expect_match(evidencia, "1 valor no es texto UTF-8 v\u00e1lido", fixed = TRUE,
                 info = nombre)
    expect_false(grepl("no pierde ning", evidencia, fixed = TRUE), info = nombre)
    plan <- as.data.frame(planificar_limpieza(perfil, datos))
    conversion <- plan[grepl("^convertir", plan$estrategia), , drop = FALSE]
    expect_identical(as.character(conversion$estado), "bloqueada", info = nombre)
  }
})

test_that("la comparacion de orden entre columnas lee la columna como el perfil", {
  # Leia los numeros de la columna cruda: en una sesion UTF-8 abortaba adentro
  # de un `tryCatch()` y la columna salia de la comparacion sin que el alcance
  # lo dijera; bajo `C` contaba la celda como un valor que no se pudo convertir
  # y descartaba el par. El perfil ya la excluyo del analisis: el alcance es el
  # de la misma tabla con esa celda ausente. Vale en cualquier sesion.
  inicio <- c(10L, 14L, 22L, 31L, 17L, 45L, 12L, 28L, 33L, 19L, 40L, 25L)
  fin <- inicio + c(3L, 8L, 1L, 12L, 5L, 2L, 9L, 4L, 7L, 6L, 11L, 10L)
  ausente <- data.frame(inicio = as.character(inicio), fin = as.character(fin),
                        stringsAsFactors = FALSE)
  ausente$fin[[7L]] <- NA
  ilegible <- ausente
  ilegible$fin[[7L]] <- paste0(fin[[7L]], .bytes_sin_marca(0xFF))
  orden <- function(datos) perfilar(datos, fecha = .fecha_bytes)$meta$orden_columnas
  esperado <- orden(ausente)
  expect_identical(esperado$pares_evaluados_orden, 1)
  expect_identical(orden(ilegible), esperado)
})

test_that("la conversion de tipo no da por ausente lo que no es UTF-8 valido", {
  # `convertir_tipo` contaba presentes sobre el texto legible, que pone `NA` en
  # la celda ilegible: la accion salia `lista` y no destructiva, y `aplicar()`
  # escribia `NA` donde estaba el valor. La regional ya la contaba presente y
  # quedaba bloqueada. Vale en cualquier sesion.
  ilegible <- paste0("14", .bytes_sin_marca(0xFF))
  datos <- data.frame(v = c("12", "13", ilegible, "15", "16"),
                      stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = .fecha_bytes)
  plan <- planificar_limpieza(perfil, datos)
  tabla <- as.data.frame(plan)
  fila <- tabla$estrategia == "convertir_tipo"
  expect_identical(sum(fila), 1L)
  expect_identical(as.character(tabla$estado[fila]), "bloqueada")
  expect_false(isTRUE(tabla$aplicar[fila]))
  expect_match(as.character(tabla$justificacion[fila]),
               "1 de 5 valores presentes no se lee", fixed = TRUE)
  # El plan por omision no toca la celda.
  resultado <- aplicar(plan, datos)
  expect_identical(charToRaw(resultado$datos$v[[3L]]), charToRaw(ilegible))
})

test_that("una cadena de ausencia con un byte que no es UTF-8 se acepta como declarada", {
  # En una sesion que no es UTF-8, "sin marca" es la codificacion nativa y el
  # caso no existe.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  marca <- paste0("sin dato", .bytes_sin_marca(0xA0))
  valores <- c("-999", "12", "15", marca, "-999", "20", "22", marca, "30")
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = .fecha_bytes, cadenas_ausencia = marca)
  expect_identical(charToRaw(perfil$meta$cadenas_ausencia), charToRaw(marca))
  fila <- as.data.frame(perfil$columnas)
  expect_identical(as.integer(fila$n_codificacion_invalida), 2L)
  # La declaracion vacia se sigue rechazando.
  expect_error(perfilar(datos, cadenas_ausencia = "  "), "cadenas_ausencia")
})

test_that("perfilar_dbi() lee la muestra y la moda aunque traigan bytes que no son UTF-8", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  b <- .bytes_sin_marca
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # La muestra se perfila: abortaba en el mismo lector que `perfilar()` y el
  # perfil salia sin ella, con un aviso.
  numeros <- data.frame(
    id = 1:8,
    v = c("12,5", "13,1", paste0("14,2", b(0xFF)), "15,0", "16,3", "2,2",
          "3,3", "4,4"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "numeros", numeros)
  avisos <- character()
  perfil <- withCallingHandlers(
    perfilar_dbi(con, "numeros"),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("no se pudo perfilar", avisos, fixed = TRUE)))
  expect_false(is.null(perfil$perfil_muestra))
  muestra <- as.data.frame(perfil$perfil_muestra$columnas)
  expect_identical(as.integer(muestra$n_codificacion_invalida[muestra$columna == "v"]), 1L)
  expect_true("codificacion_invalida" %in% hallazgos(perfil)$tipo_hallazgo)
  # La moda describe lo almacenado y puede ser ilegible: la corroboracion la
  # leia con `as.numeric()`, que aborta sobre ella.
  ilegibles <- data.frame(
    id = 1:6,
    v = paste0(b(0xFF), c("Canelones", "Canelones", "Salto", "Salto", "Salto",
                          "Rivera")),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "ilegibles", ilegibles)
  perfil <- suppressWarnings(perfilar_dbi(con, "ilegibles"))
  columnas_dbi <- as.data.frame(columnas(perfil))
  moda <- unlist(columnas_dbi$moda[columnas_dbi$columna == "v"])
  expect_identical(charToRaw(moda), charToRaw(paste0(b(0xFF), "Salto")))
})
