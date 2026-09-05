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

# `perfilar_por()` publica los valores de la columna de agrupacion como etiqueta
# de grupo, y `perfilar()` sobre esa misma columna los enmascara. Encontrado el
# 2026-09-04 pidiendo construir la fuga N+1, no revisando la capa de proteccion.
#
# No se enmascara -la etiqueta es el eje del resultado- pero se declara. Lo que
# este archivo fija es que la declaracion exista y que NO aparezca donde no
# corresponde: sin esa segunda mitad, un atributo que se llenara siempre pasaria
# igual y no probaria nada.
test_that("perfilar_por declara que las etiquetas de grupo son datos personales", {
  documentos <- c("5.836.595-5", "4.112.987-2", "7.965.431-K",
                  "1.234.567-8", "9.876.543-2")
  datos <- data.frame(
    documento = rep(documentos, 8L),
    atributo = rep(c("edad", "sueldo", "altura", "peso"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )

  # Primera mitad obligatoria: el mecanismo se activa. Si `perfilar()` no
  # clasificara la columna como personal, todo lo de abajo pasaria sin medir.
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(perfil$datos_personales$columna == "documento" &
                    perfil$datos_personales$proteger))

  agrupado <- suppressWarnings(
    perfilar_por(datos, "documento", min_filas = 2L)
  )
  declarado <- attr(agrupado, "etiquetas_personales")
  expect_equal(nrow(declarado), 1L)
  expect_equal(declarado$columna, "documento")
  expect_equal(declarado$n_grupos, length(documentos))
  expect_match(declarado$motivo, "seudonimizada", fixed = TRUE)

  # Y avisa al ejecutar, no solo en un atributo que nadie mira.
  expect_message(
    perfilar_por(datos, "documento", min_filas = 2L),
    "documento",
    fixed = TRUE
  )

  # Control 1: una columna de agrupacion que NO lleva datos personales no
  # declara nada. Sin esto, un atributo siempre lleno pasaria el test.
  neutros <- data.frame(
    region = rep(c("norte", "sur", "este", "oeste", "centro"), 8L),
    atributo = rep(c("a", "b", "c", "d"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )
  sin_personales <- perfilar_por(neutros, "region", min_filas = 2L)
  expect_equal(nrow(attr(sin_personales, "etiquetas_personales")), 0L)

  # Control 2: con la proteccion desactivada el usuario ya declaro que quiere
  # los valores, asi que no se avisa de algo que pidio.
  sin_proteccion <- perfilar_por(datos, "documento", min_filas = 2L,
                                 proteger_datos_personales = FALSE)
  expect_equal(nrow(attr(sin_proteccion, "etiquetas_personales")), 0L)
})

# Nombrar la columna de agrupacion en un argumento que se reenvia a `perfilar()`
# hacia fallar la corrida entera, porque esa columna se recorta de cada rebanada
# y `perfilar()` la denuncia como inexistente. Se destapo al agregar el canal de
# declaracion: `columnas_personales = <la columna de agrupacion>` es la unica
# forma de decir que las etiquetas llevan datos personales cuando el lexico no
# reconoce el nombre, y era justamente la que reventaba.
test_that("perfilar_por acepta argumentos que nombran la columna de agrupacion", {
  datos <- data.frame(
    codigo_interno = rep(sprintf("X%04d", 1:5), 8L),
    atributo = rep(c("a", "b", "c", "d"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )

  # Primera mitad: sin declarar, el lexico NO reconoce este nombre. Si lo
  # reconociera, lo de abajo pasaria sin probar el canal de declaracion.
  sin_declarar <- perfilar_por(datos, "codigo_interno", min_filas = 2L)
  expect_equal(nrow(attr(sin_declarar, "etiquetas_personales")), 0L)

  declarado <- suppressWarnings(perfilar_por(
    datos, "codigo_interno", min_filas = 2L,
    columnas_personales = "codigo_interno"
  ))
  etiquetas <- attr(declarado, "etiquetas_personales")
  expect_equal(nrow(etiquetas), 1L)
  expect_equal(etiquetas$tipo, "declarada_por_el_usuario")
  # Y la corrida sigue produciendo los grupos: el argumento se recorta, no anula.
  expect_equal(length(unique(declarado$grupo)), 5L)

  # Los otros tres argumentos que validaban existencia y por eso reventaban.
  for (extra in list(
    list(columnas_opcionales = "codigo_interno"),
    list(aplicabilidad = list(codigo_interno = function(x) rep(TRUE, length(x))))
  )) {
    salida <- suppressWarnings(do.call(
      perfilar_por,
      c(list(datos, "codigo_interno", min_filas = 2L), extra)
    ))
    expect_equal(length(unique(salida$grupo)), 5L,
                 info = names(extra)[1L])
  }

  # Control: un argumento que nombra una columna que SI esta en la rebanada
  # sigue llegando a `perfilar()` y no se recorta por error.
  con_opcional <- suppressWarnings(perfilar_por(
    datos, "codigo_interno", min_filas = 2L, columnas_opcionales = "valor"
  ))
  expect_equal(length(unique(con_opcional$grupo)), 5L)
})

# Los ausentes forman un grupo con la etiqueta `(ausente)`. Si la columna trae
# ese texto como valor real, los dos caen en el mismo grupo: no se pierde
# ninguna fila -la suma se conserva- pero se publica un grupo que junta dos
# cosas distintas. No se cambia la etiqueta; se declara la colision.
test_that("perfilar_por declara cuando el grupo (ausente) junta dos cosas", {
  datos <- data.frame(
    g = c(rep(NA_character_, 20L), rep("(ausente)", 20L), rep("real", 20L)),
    v = as.character(1:60),
    stringsAsFactors = FALSE
  )
  salida <- perfilar_por(datos, "g", min_filas = 5L)
  cobertura <- attr(salida, "cobertura_grupos")

  fila <- cobertura[cobertura$grupo == "(ausente)" &
                      grepl("valor real", cobertura$motivo, fixed = TRUE), ,
                    drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_filas_grupo, 40L)
  expect_match(fila$motivo, "20 fila(s) con la columna de agrupacion ausente",
               fixed = TRUE)

  # No se pierde ni se duplica ninguna fila: la invariante que importa.
  por_grupo <- unique(salida[, c("grupo", "n_filas_grupo")])
  expect_equal(sum(por_grupo$n_filas_grupo), nrow(datos))

  # Control: sin el literal en los datos no se declara nada. Sin esta mitad, una
  # fila de cobertura que se escribiera siempre pasaria el test.
  sin_colision <- data.frame(
    g = c(rep(NA_character_, 20L), rep("real", 20L)),
    v = as.character(1:40),
    stringsAsFactors = FALSE
  )
  cobertura_limpia <- attr(perfilar_por(sin_colision, "g", min_filas = 5L),
                           "cobertura_grupos")
  expect_equal(sum(grepl("valor real", cobertura_limpia$motivo, fixed = TRUE)), 0L)
})
