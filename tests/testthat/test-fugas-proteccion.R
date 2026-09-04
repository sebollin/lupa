capturar_consola_proteccion <- function(codigo) {
  archivo <- tempfile()
  conexion <- file(archivo, open = "wt")
  sink(conexion)
  sink(conexion, type = "message")
  resultado <- tryCatch(
    force(codigo),
    finally = {
      sink(type = "message")
      sink()
      close(conexion)
    }
  )
  invisible(resultado)
  paste(readLines(archivo, warn = FALSE), collapse = " ")
}

test_that("guiar_limpieza enmascara filas enteras con columnas protegidas", {
  set.seed(16)
  nd <- 30L
  datos <- data.frame(
    documento = sprintf("707771%02d", seq_len(nd)),
    correo = sprintf("persona%03d@correo771.test", seq_len(nd)),
    telefono = sprintf("0997700%02d", seq_len(nd)),
    stringsAsFactors = FALSE
  )
  datos[3L, ] <- datos[2L, ]
  perfil <- perfilar(
    datos, proteger_datos_personales = TRUE,
    analizar_dependencias = FALSE
  )

  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_true("filas_duplicadas" %in% tipos)
  expect_true(all(perfil$columnas$dato_personal_protegido))
  expect_true(all(perfil$columnas$moda == "[valor protegido]"))

  plan <- planificar_limpieza(perfil, datos)
  expect_setequal(
    attr(plan, "columnas_datos_personales_protegidas", exact = TRUE),
    names(datos)
  )
  consola <- paste(
    capturar_consola_proteccion(print(perfil)),
    capturar_consola_proteccion(print(perfil$columnas)),
    capturar_consola_proteccion(print(plan)),
    capturar_consola_proteccion(
      guiar_limpieza(plan, datos, selector = function(x) 0L)
    )
  )
  expect_match(consola, "Ejemplos reales:", fixed = TRUE)
  expect_match(
    consola,
    "fila 2 [grupo 1]: documento=\"[valor protegido]\", correo=\"[valor protegido]\", telefono=\"[valor protegido]\"",
    fixed = TRUE
  )
  expect_match(
    consola,
    "fila 3 [grupo 1]: documento=\"[valor protegido]\", correo=\"[valor protegido]\", telefono=\"[valor protegido]\"",
    fixed = TRUE
  )
  crudos <- unique(unlist(lapply(datos, as.character), use.names = FALSE))
  crudos <- crudos[!is.na(crudos) & nzchar(crudos)]
  expect_false(any(vapply(
    crudos, function(valor) grepl(valor, consola, fixed = TRUE), logical(1L)
  )))
})

test_that("la proteccion conserva la senal geometrica pero no publica bbox", {
  skip_if_not_installed("sf")
  set.seed(77)
  wkt <- sprintf(
    "POINT (%.5f %.5f)",
    runif(50L, -56.2, -56.0), runif(50L, -34.9, -34.7)
  )
  perfil <- perfilar(
    data.frame(domicilio = wkt, stringsAsFactors = FALSE),
    proteger_datos_personales = TRUE, analizar_dependencias = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "domicilio", , drop = FALSE]

  expect_true(all(perfil$columnas$dato_personal_protegido))
  expect_identical(fila$tipo_geometria, "POINT")
  expect_identical(fila$dimension_geometria, "XY")
  expect_true(!is.na(fila$bbox_alcance))

  bbox <- c("bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax")
  expect_true(all(is.na(fila[bbox])))
  expect_match(fila$bbox_alcance, "proteg", ignore.case = TRUE)
  geometria <- sf::st_sfc(
    lapply(seq_len(3L), function(i) {
      sf::st_point(c(-56 + i / 1000, -34 + i / 1000))
    }),
    crs = 4326
  )
  datos_sf <- data.frame(geometria)
  names(datos_sf) <- "domicilio"
  perfil_sf <- perfilar(
    datos_sf, proteger_datos_personales = TRUE,
    analizar_dependencias = FALSE
  )
  fila_sf <- perfil_sf$columnas[
    perfil_sf$columnas$columna == "domicilio", , drop = FALSE
  ]
  expect_identical(fila_sf$crs_declarado, "4326")
  expect_identical(fila_sf$tipo_geometria, "POINT")
  expect_identical(fila_sf$dimension_geometria, "XY")
  expect_identical(fila_sf$n_geometrias_vacias, 0L)
  expect_identical(fila_sf$n_geometrias_invalidas, 0L)
  expect_identical(fila_sf$n_bbox_evaluados, 3L)
  expect_true(all(is.na(fila_sf[bbox])))
  consola <- paste(
    capturar_consola_proteccion(print(perfil)),
    capturar_consola_proteccion(print(perfil$columnas)),
    capturar_consola_proteccion(print(perfil_sf$columnas))
  )
  coordenadas <- unique(unlist(regmatches(
    wkt, gregexpr("[-+]?[0-9]+\\.[0-9]+", wkt, perl = TRUE)
  )))
  expect_false(any(vapply(
    coordenadas,
    function(valor) grepl(valor, consola, fixed = TRUE),
    logical(1L)
  )))
})
