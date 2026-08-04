.datos_personales_cinco <- function() {
  data.frame(
    documento_texto = c(
      "39174820", "48213756", "50382614", "57493021", "61847205"
    ),
    ci = c(39174820, 48213756, 50382614, 57493021, 61847205),
    nombre = c(
      "Ada Neri", "Bruno Costa", "Carla Diaz", "Diego Luna", "Eva Rios"
    ),
    correo = paste0(
      c("ada", "bruno", "carla", "diego", "eva"), "@example.invalid"
    ),
    fecha_nacimiento = as.Date(c(
      "1958-09-13", "1964-05-21", "1974-03-18", "1982-11-07", "1991-01-22"
    )),
    stringsAsFactors = FALSE
  )
}

.valores_crudos_prueba <- function(datos) {
  valores <- unique(unlist(lapply(datos, as.character), use.names = FALSE))
  valores[!is.na(valores)]
}

.coincidencias_crudas <- function(x, crudos, ruta = "objeto") {
  resultado <- character()
  if (inherits(x, "data.frame") || is.list(x)) {
    for (nombre in names(x)) {
      resultado <- c(
        resultado,
        .coincidencias_crudas(x[[nombre]], crudos, paste0(ruta, "$", nombre))
      )
    }
  } else if (is.atomic(x)) {
    valores <- as.character(x)
    indices <- which(!is.na(valores) & valores %in% crudos)
    if (length(indices)) {
      resultado <- paste0(ruta, "[", indices, "] = ", valores[indices])
    }
  }
  resultado
}

test_that("analizar protege valores crudos de los cuatro tipos personales", {
  datos <- .datos_personales_cinco()
  crudos <- .valores_crudos_prueba(datos)
  analisis <- analizar(
    datos, muestra = Inf, analizar_dependencias = FALSE,
    conservar_datos = TRUE
  )

  expect_setequal(
    unique(analisis$perfil$datos_personales$tipo),
    c("documento_identidad", "nombre", "correo", "fecha_nacimiento")
  )
  expect_length(.coincidencias_crudas(analisis, crudos), 0L)

  columnas <- analisis$perfil$columnas
  documentos <- columnas$tipo_dato_personal == "documento_identidad"
  nacimiento <- columnas$tipo_dato_personal == "fecha_nacimiento"
  expect_true(all(is.na(columnas$minimo[documentos])))
  expect_true(all(is.na(columnas$maximo[documentos])))
  expect_true(all(is.na(columnas$mediana[documentos])))
  expect_true(all(
    columnas$detalle_proteccion_personal[documentos] ==
      "[estadisticos de orden protegidos]"
  ))
  expect_true(all(
    columnas$minimo_fecha[nacimiento] == "[valor protegido]"
  ))
  expect_true(all(
    columnas$maximo_fecha[nacimiento] == "[valor protegido]"
  ))
  expect_true(all(
    columnas$mediana_fecha[nacimiento] == "[valor protegido]"
  ))

  cuantiles <- analisis$distribuciones$cuantiles
  expect_equal(nrow(cuantiles[cuantiles$columna == "ci", ]), 5L)
  expect_true(all(is.na(cuantiles$valor[cuantiles$columna == "ci"])))
  expect_true(all(
    cuantiles$estado[cuantiles$columna == "ci"] == "valor_protegido"
  ))
  expect_true(all(is.na(analisis$temporal$resumen$fecha_minima)))
  expect_true(all(is.na(analisis$temporal$resumen$fecha_maxima)))
  expect_equal(
    analisis$temporal$resumen$proteccion_temporal,
    "[rangos y huecos protegidos]"
  )
  expect_setequal(
    analisis$meta$columnas_datos_protegidas, names(datos)
  )
})

test_that("el HTML protege incluso un analisis originalmente abierto", {
  datos <- .datos_personales_cinco()
  crudos <- .valores_crudos_prueba(datos)
  abierto <- analizar(
    datos, muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  archivo <- tempfile(fileext = ".html")
  reportar(abierto, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")

  expect_false(any(vapply(
    crudos, function(valor) grepl(valor, html, fixed = TRUE), logical(1L)
  )))
  expect_match(html, "estadisticos de orden", fixed = TRUE)
  expect_match(html, "valor_protegido", fixed = TRUE)
  expect_match(html, "rangos y huecos protegidos", fixed = TRUE)
})

test_that("desactivar la proteccion conserva perfil cuantiles datos y HTML", {
  datos <- .datos_personales_cinco()
  analisis <- analizar(
    datos, muestra = Inf, analizar_dependencias = FALSE,
    conservar_datos = TRUE, proteger_datos_personales = FALSE
  )
  ci <- analisis$perfil$columnas$columna == "ci"
  nacimiento <- analisis$perfil$columnas$columna == "fecha_nacimiento"

  expect_equal(analisis$perfil$columnas$minimo[ci], min(datos$ci))
  expect_equal(analisis$perfil$columnas$maximo[ci], max(datos$ci))
  expect_equal(analisis$perfil$columnas$mediana[ci], stats::median(datos$ci))
  expect_equal(
    analisis$perfil$columnas$minimo_fecha[nacimiento],
    as.character(min(datos$fecha_nacimiento))
  )
  expect_equal(
    analisis$perfil$columnas$maximo_fecha[nacimiento],
    as.character(max(datos$fecha_nacimiento))
  )
  expect_equal(
    analisis$perfil$columnas$mediana_fecha[nacimiento], "1974-03-18"
  )
  expect_equal(
    analisis$distribuciones$cuantiles$valor,
    as.numeric(stats::quantile(datos$ci, names = FALSE))
  )
  expect_true(all(analisis$distribuciones$cuantiles$estado == "calculado"))
  expect_identical(analisis$datos, datos)

  archivo <- tempfile(fileext = ".html")
  reportar(
    analisis, archivo = archivo, proteger_datos_personales = FALSE
  )
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_true(grepl("39174820", html, fixed = TRUE))
  expect_true(grepl("1958-09-13", html, fixed = TRUE))
})

test_that("el rango de nacimiento se diagnostica sin publicar sus extremos", {
  datos <- data.frame(fecha_nacimiento = as.Date(c(
    "1899-12-31", "1970-01-01", "1980-01-01", "1990-01-01", "2030-01-01"
  )))
  perfil <- perfilar(
    datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "fecha_nacimiento_fuera_rango", ,
    drop = FALSE
  ]

  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "error")
  expect_match(hallazgo$descripcion, "anterior a 1900", fixed = TRUE)
  expect_match(hallazgo$descripcion, "posterior a la fecha", fixed = TRUE)
  expect_equal(hallazgo$evidencia, "[evidencia protegida]")
  expect_equal(perfil$columnas$minimo_fecha, "[valor protegido]")
  expect_equal(perfil$columnas$maximo_fecha, "[valor protegido]")

  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "anterior a 1900", fixed = TRUE)
  expect_match(html, "posterior a la fecha", fixed = TRUE)
  expect_false(grepl("1899-12-31|2030-01-01", html))
})

test_that("extremos exactos integer64 personales tambien se protegen", {
  skip_if_not_installed("bit64")
  datos <- data.frame(documento = bit64::as.integer64(c(
    "9007199254740993", "9007199254740995", "9007199254740997"
  )))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(perfil$columnas$minimo_exacto, "[valor protegido]")
  expect_equal(perfil$columnas$maximo_exacto, "[valor protegido]")
  expect_equal(
    perfil$columnas$detalle_proteccion_personal,
    "[estadisticos de orden protegidos]"
  )
})

test_that("planes deriva e historico no reintroducen los valores protegidos", {
  datos <- .datos_personales_cinco()
  crudos <- .valores_crudos_prueba(datos)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  actualizados <- datos
  actualizados$ci <- actualizados$ci + 10
  perfil_actual <- perfilar(actualizados, analizar_dependencias = FALSE)
  deriva <- comparar_perfiles(perfil, perfil_actual)

  nucleo <- metricas_nucleo()
  medida <- medir(
    modelo(nucleo$NoNulo()("datos", "ci")), datos,
    id_medicion = "personal"
  )
  historico <- historico_calidad(medida)

  expect_length(.coincidencias_crudas(plan, crudos), 0L)
  expect_length(.coincidencias_crudas(deriva, crudos), 0L)
  expect_length(.coincidencias_crudas(historico, crudos), 0L)

  archivo <- tempfile(fileext = ".html")
  reportar(plan, deriva, historico, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_false(any(vapply(
    crudos, function(valor) grepl(valor, html, fixed = TRUE), logical(1L)
  )))
})

test_that("la proteccion de datos conservados cubre factores y listas", {
  datos <- data.frame(grupo = factor(c("A", "B")), numero = 1:2)
  datos$detalle <- I(list(list(valor = 1), list(valor = 2)))
  resultado <- lupa:::.proteger_datos_conservados(
    datos, c("grupo", "numero", "detalle")
  )

  expect_equal(resultado$grupo, rep("[valor protegido]", 2L))
  expect_true(all(is.na(resultado$numero)))
  expect_true(all(vapply(resultado$detalle, is.null, logical(1L))))
  expect_null(lupa:::.proteger_datos_conservados(NULL, "x"))
})

test_that("la proteccion admite objetos creados con esquemas anteriores", {
  datos <- .datos_personales_cinco()
  analisis <- analizar(
    datos, muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  analisis$distribuciones$cuantiles$estado <- NULL
  analisis$perfil$columnas$detalle_proteccion_personal <- NULL

  protegido <- lupa:::.proteger_analisis(analisis)

  expect_true("estado" %in% names(protegido$distribuciones$cuantiles))
  expect_true(all(
    protegido$distribuciones$cuantiles$estado == "valor_protegido"
  ))
  expect_true("detalle_proteccion_personal" %in% names(protegido$perfil$columnas))
  expect_true(is.na(lupa:::.fecha_resumida_personal("")))

  clasificacion <- protegido$perfil$datos_personales[1L, , drop = FALSE]
  clasificacion$tipo <- "fecha_nacimiento"
  expect_length(lupa:::.hallazgos_rango_nacimiento(
    protegido$perfil$columnas[0L, , drop = FALSE], clasificacion,
    as.POSIXct("2026-01-01", tz = "UTC")
  ), 0L)
})

test_that("la proteccion conserva el nombre completo de una columna con coma", {
  valores <- c("  Wilson Cabrera Techera ", "ESTELA NUNEZ BERRUTTI",
               "heber pintos olivera", "Rosalia  Fagundez")
  datos <- data.frame(x = rep(valores, 4L), stringsAsFactors = FALSE)
  names(datos) <- "nombre, apellido"
  analisis <- analizar(datos, proteger_datos_personales = TRUE)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  invisible(reportar(analisis, archivo = archivo))
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_false(any(vapply(
    trimws(valores), function(valor) grepl(valor, html, fixed = TRUE),
    logical(1L)
  )))
  expect_match(html, "evidencia protegida", fixed = TRUE)
})
