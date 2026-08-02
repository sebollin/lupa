test_that("la ambigüedad día-mes se conserva con tres separadores", {
  for (separador in c("/", "-", ".")) {
    valores <- paste0(c("03", "05"), separador, c("04", "06"),
                      separador, "2023")
    formatos <- detectar_formatos_fecha(valores)
    expect_equal(nrow(formatos), 2L, info = separador)
    expect_true(all(formatos$estado == "candidato"), info = separador)
    perfil <- perfilar(
      data.frame(f = valores), analizar_dependencias = FALSE
    )
    expect_true(is.na(perfil$columnas$minimo_fecha), info = separador)
    expect_true("formato_fecha_ambiguo" %in% perfil$hallazgos$tipo_hallazgo,
                info = separador)
  }
  mes_primero <- detectar_formatos_fecha(c("12-31-2023", "11-30-2023"))
  expect_equal(mes_primero$formato, "%m-%d-%Y")
  expect_equal(mes_primero$estado, "confirmado")
})

test_that("microsegundos y offsets ISO 8601 se reconocen sin perder UTC", {
  valores <- c(
    "2019-07-24 01:39:40.452122",
    "2023-11-30T08:30:00Z",
    "2023-11-30T08:30:00-03:00"
  )
  tipos <- vapply(valores, function(x) inferir_tipo(x)$tipo, character(1L))
  expect_true(all(tipos == "fecha-hora"))
  formatos <- lapply(valores, detectar_formatos_fecha)
  expect_match(formatos[[1L]]$formato, "%OS", fixed = TRUE)
  expect_match(formatos[[2L]]$formato, "%z", fixed = TRUE)
  expect_match(formatos[[3L]]$formato, "%z", fixed = TRUE)
  perfil <- perfilar(
    data.frame(f = valores[2:3]), analizar_dependencias = FALSE
  )
  expect_equal(perfil$columnas$minimo_fecha, "2023-11-30 08:30:00")
  expect_equal(perfil$columnas$maximo_fecha, "2023-11-30 11:30:00")
})

test_that("integer64 fuera de double se resume sin inventar extremos", {
  skip_if_not_installed("bit64")
  originales <- c("9007199254740993", "9007199254740995", "9007199254740997")
  x <- bit64::as.integer64(originales)
  perfil <- perfilar(data.frame(x = x), analizar_dependencias = FALSE)
  fila <- perfil$columnas

  expect_equal(fila$n_distintos, 3L)
  expect_true(is.na(fila$minimo) && is.na(fila$maximo))
  expect_true(fila$minimo_exacto %in% originales)
  expect_true(fila$maximo_exacto %in% originales)
  expect_equal(fila$estado_estadisticos, "omitidos_precision")
  expect_true(
    "integer64_fuera_precision_double" %in% perfil$hallazgos$tipo_hallazgo
  )
})

test_that("integer64 seguro conserva estadísticas ordinarias y extremos exactos", {
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64(c("-2", "0", "5", NA))
  perfil <- perfilar(data.frame(x = x), analizar_dependencias = FALSE)
  fila <- perfil$columnas
  expect_equal(c(fila$minimo, fila$maximo), c(-2, 5))
  expect_equal(c(fila$minimo_exacto, fila$maximo_exacto), c("-2", "5"))
  expect_equal(c(fila$n_ceros, fila$n_negativos), c(1L, 1L))
  expect_equal(fila$estado_estadisticos, "calculados")
})

test_that("NaN e infinitos quedan distinguidos y visibles", {
  perfil <- perfilar(
    data.frame(x = c(1, 2, 3, Inf, -Inf, NaN, NA)),
    analizar_dependencias = FALSE
  )
  fila <- perfil$columnas
  expect_equal(fila$n_nan, 1L)
  expect_equal(fila$n_infinito_positivo, 1L)
  expect_equal(fila$n_infinito_negativo, 1L)
  expect_equal(c(fila$minimo, fila$maximo), c(1, 3))
  expect_true("valores_no_finitos" %in% perfil$hallazgos$tipo_hallazgo)
})

test_that("Unicode equivalente se compara en NFC sin modificar el original", {
  skip_if_not_installed("stringi")
  valores <- c("Jos\u00e9", "Jose\u0301")
  perfil <- perfilar(data.frame(nombre_corto = valores),
                     analizar_dependencias = FALSE)
  expect_equal(perfil$columnas$n_distintos, 2L)
  expect_equal(perfil$columnas$n_variantes_unicode, 2L)
  expect_true("normalizacion_unicode" %in% perfil$hallazgos$tipo_hallazgo)
  expect_identical(valores, c("Jos\u00e9", "Jose\u0301"))
})

test_that("listas y geometrías no afirman cero distintos", {
  lista <- perfilar(
    data.frame(x = I(list(1, 2, 3))), analizar_dependencias = FALSE
  )
  expect_equal(lista$columnas$n_distintos, 3L)

  skip_if_not_installed("sf")
  geometria <- sf::st_sfc(
    sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
    crs = 4326
  )
  datos <- sf::st_sf(id = 1:3, geometry = geometria)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "geometry", ]
  expect_equal(fila$n_distintos, 3L)
  expect_match(fila$tipo_declarado, "^sfc_")
})

test_that("Escala y vigencia materializan cinco métricas del catálogo", {
  datos <- data.frame(
    valor = c(10, 20, 0),
    actualizado = as.Date(c("2026-01-01", "2026-01-20", "2026-02-10"))
  )
  contrato <- vigencia(
    "actualizado", fecha_acceso = as.Date("2026-02-15"),
    fecha_ultimo_cambio = as.Date("2026-02-01"),
    fecha_limite = as.Date("2026-02-01"),
    inicio_intervalo = as.Date("2026-01-10"),
    fin_intervalo = as.Date("2026-02-05"), frecuencia_cambio = 10
  )
  nucleo <- metricas_nucleo()
  instancias <- list(
    instanciar(especializar(nucleo$Escala, escala = escala(0.5)), "t", "valor"),
    instanciar(especializar(nucleo$DesactualizacionPorFecha,
                            vigencia = contrato), "t", "valor"),
    instanciar(especializar(nucleo$DesactualizacionPorCambios,
                            vigencia = contrato), "t", "valor"),
    instanciar(especializar(nucleo$OportunidadEntPorFecha,
                            vigencia = contrato), "t"),
    instanciar(especializar(nucleo$OportunidadEntPorIntervalo,
                            vigencia = contrato), "t")
  )
  medidas <- medir(modelo(instancias), datos)
  expect_equal(
    medidas$resultado[medidas$metrica == "Escala"], c(0.95, 0.975, 0)
  )
  expect_equal(
    medidas$resultado[medidas$metrica == "DesactualizacionPorFecha"],
    c(31, 12, 0)
  )
  expect_equal(
    medidas$resultado[medidas$metrica == "DesactualizacionPorCambios"],
    c(4, 2, 0)
  )
  expect_equal(
    medidas$resultado[medidas$metrica == "OportunidadEntPorFecha"],
    c(1, 1, 0)
  )
  expect_equal(
    medidas$resultado[medidas$metrica == "OportunidadEntPorIntervalo"],
    c(0, 1, 0)
  )
  expect_equal(as.character(catalogo_agesic()$estado[c(9, 42, 43, 48, 49)]),
               rep("implementada", 5L))
})

test_that("los contratos temporales y de escala rechazan afirmaciones incompletas", {
  expect_error(vigencia(""), "nombre no vacío")
  expect_error(vigencia("f", fecha_acceso = NA), "fechas válidas")
  expect_error(
    vigencia("f", inicio_intervalo = as.Date("2026-01-01")),
    "intervalo exige"
  )
  expect_error(vigencia("f", frecuencia_cambio = "mensual"), "duración positiva")
  expect_error(vigencia("f", frecuencia_cambio = 0), "duración positiva")
  contrato <- vigencia(
    "f", frecuencia_cambio = as.difftime(2, units = "days"),
    fecha_acceso = as.Date("2026-02-01")
  )
  expect_equal(contrato$frecuencia_cambio_segundos, 2 * 86400)

  expect_error(escala(-1), "no negativo")
  expect_error(escala(2, "relativo"), "\u005b0, 1\u005d")
  expect_s3_class(escala(function(x) rep(0.1, length(x))), "escala_medicion")

  nucleo <- metricas_nucleo()
  expect_error(especializar(nucleo$Escala, escala = 0.1), "escala\\(\\)")
  expect_error(
    especializar(nucleo$DesactualizacionPorFecha, vigencia = list()),
    "vigencia\\(\\)"
  )
})

test_that("las métricas con contrato se abstienen ante datos o insumos inválidos", {
  nucleo <- metricas_nucleo()
  escala_inst <- function(contrato, atributo = "x") {
    instanciar(especializar(nucleo$Escala, escala = contrato), "t", atributo)
  }
  expect_error(
    medir(modelo(escala_inst(escala(0.1))), data.frame(x = letters[1:2])),
    "numérico ordinario"
  )
  expect_error(
    medir(modelo(escala_inst(escala(0.1))), data.frame(x = c(1, Inf))),
    "no finitos"
  )
  expect_error(
    medir(
      modelo(escala_inst(escala(function(x) c(-1, 0)))),
      data.frame(x = c(1, 2))
    ),
    "no devolvió"
  )
  expect_error(
    medir(
      modelo(escala_inst(escala(function(x) rep(2, length(x)), "relativo"))),
      data.frame(x = c(1, 2))
    ),
    "\u005b0, 1\u005d"
  )
  relativa <- medir(
    modelo(escala_inst(escala(c(0.1, 0.2, 0.3), "relativo"))),
    data.frame(x = c(1, NA, 3))
  )
  expect_equal(relativa$resultado, c(0.9, 0.7))

  sin_frecuencia <- vigencia("f", fecha_acceso = as.Date("2026-02-01"))
  inst_cambios <- instanciar(
    especializar(nucleo$DesactualizacionPorCambios,
                  vigencia = sin_frecuencia), "t", "x"
  )
  expect_error(
    medir(modelo(inst_cambios), data.frame(f = as.Date("2026-01-01"))),
    "frecuencia_cambio"
  )
  inst_fecha <- instanciar(
    especializar(nucleo$OportunidadEntPorFecha,
                  vigencia = sin_frecuencia), "t"
  )
  expect_error(
    medir(modelo(inst_fecha), data.frame(f = as.Date("2026-01-01"))),
    "fecha_limite"
  )
  inst_intervalo <- instanciar(
    especializar(nucleo$OportunidadEntPorIntervalo,
                  vigencia = sin_frecuencia), "t"
  )
  expect_error(
    medir(modelo(inst_intervalo), data.frame(f = as.Date("2026-01-01"))),
    "intervalo"
  )
  contrato_numero <- vigencia("f", fecha_acceso = as.Date("2026-02-01"),
                              frecuencia_cambio = 1)
  instancia_tipo <- instanciar(
    especializar(nucleo$DesactualizacionPorFecha,
                  vigencia = contrato_numero), "t", "x"
  )
  expect_error(
    medir(modelo(instancia_tipo), data.frame(f = 1:2)),
    "Date o POSIXt"
  )

  instancia_atraso <- instanciar(
    especializar(nucleo$DesactualizacionPorFecha,
                  vigencia = contrato_numero), "t", "x"
  )
  atraso <- medir(
    modelo(instancia_atraso),
    data.frame(f = as.Date(c("2026-01-30", "2026-02-01")))
  )
  expect_equal(atraso$resultado, c(1, 0))

  sin_referencia <- vigencia("f", fecha_acceso = as.Date("2026-02-01"))
  instancia_sin_referencia <- instanciar(
    especializar(nucleo$DesactualizacionPorFecha,
                  vigencia = sin_referencia), "t", "x"
  )
  expect_error(
    medir(
      modelo(instancia_sin_referencia),
      data.frame(f = as.Date("2026-01-01"))
    ),
    "fecha_ultimo_cambio.*frecuencia_cambio"
  )
})

test_that("ErrorEstandar sigue la desviación estándar declarada por el marco", {
  x <- c(1, 2, 3, 4, 100)
  instancia <- instanciar(
    especializar(metricas_nucleo()$ErrorEstandar), "t", "x"
  )
  medida <- medir(modelo(instancia), data.frame(x = x))
  expect_equal(medida$resultado, stats::sd(x))
  expect_equal(medida$tipo_resultado, "numero_real")
  expect_error(agregar(medida, "entidad", "promedio"), "\u005b0, 1\u005d")
  catalogo <- catalogo_agesic()
  fila <- catalogo[catalogo$metrica_agesic == "ErrorEstandar", ]
  expect_match(fila$observacion, "desviación estándar")
})

test_that("cada tipo de resultado rechaza valores incompatibles", {
  validar <- lupa:::.resultados_validos_tipo
  expect_false(validar("1", "real"))
  expect_false(validar(1.1, "real"))
  expect_false(validar(0.5, "booleano"))
  expect_false(validar(1.5, "entero"))
  expect_false(validar(-1, "duracion"))
  expect_true(validar(c(0, 2.5, 3, 4),
                      c("booleano", "numero_real", "entero", "duracion")))
})

test_that("POSIXct conserva tipo y rango plausibles", {
  valores <- as.POSIXct(
    c("2023-06-15 10:00:00", "2023-11-30 08:30:00"), tz = "UTC"
  )
  perfil <- perfilar(data.frame(f = valores), analizar_dependencias = FALSE)
  expect_equal(perfil$columnas$tipo_declarado, "fecha-hora")
  expect_true(all(substr(
    c(perfil$columnas$minimo_fecha, perfil$columnas$maximo_fecha), 1, 4
  ) == "2023"))

  ordenado <- perfilar(
    data.frame(nivel = ordered(c("bajo", "alto"), levels = c("bajo", "alto"))),
    analizar_dependencias = FALSE
  )
  expect_equal(ordenado$columnas$tipo_declarado, "factor-ordenado")
})

test_that("los resúmenes se abstienen cuando el tipo no admite una afirmación", {
  expect_true(is.na(lupa:::.fecha_resumida(Inf, "fecha")))
  sin_formato <- lupa:::.parsear_fechas(
    "no es fecha", data.frame(formato = character(), estado = character())
  )
  expect_true(is.na(sin_formato))
  formato_desconocido <- lupa:::.parsear_fechas(
    "2023-01-01",
    data.frame(formato = "%Q", estado = "confirmado")
  )
  expect_true(is.na(formato_desconocido))
  sin_coincidencia <- lupa:::.parsear_fechas(
    "no es fecha",
    data.frame(formato = "%Y-%m-%d", estado = "confirmado")
  )
  expect_true(is.na(sin_coincidencia))
})

test_that("la cobertura distingue medido, no declarado, no aplica y alcance", {
  perfil <- perfilar(data.frame(x = c(1, NA)), analizar_dependencias = FALSE)
  cobertura <- cobertura_analisis(perfil)
  expect_setequal(
    levels(cobertura$estado),
    c("medida", "no_declarada", "no_aplica", "fuera_de_alcance")
  )
  expect_equal(
    as.character(cobertura$estado[cobertura$factor == "Densidad"]), "medida"
  )
  expect_true(all(
    as.character(cobertura$estado[cobertura$dimension == "Frescura"]) ==
      "no_aplica"
  ))
  expect_true(all(nzchar(cobertura$como_resolverlo)))
  expect_error(cobertura_analisis(data.frame(x = 1)), "perfilar")
  expect_error(cobertura_analisis(perfil, data.frame()), "medir")

  instancia <- instanciar(
    especializar(metricas_nucleo()$NoNulo), "t", "x"
  )
  medicion <- medir(modelo(instancia), data.frame(x = c(1, NA)))
  con_medicion <- cobertura_analisis(perfil, medicion)
  fila <- con_medicion$dimension == medicion$dimension[[1L]] &
    con_medicion$factor == medicion$factor[[1L]]
  expect_equal(as.character(con_medicion$estado[fila]), "medida")
})

test_that("los valores personales se clasifican sin juicio y se protegen", {
  datos <- data.frame(
    cedula = c("1.234.567-2", "4.567.890-1"),
    nombre = c("María Fernández", "Juan Pérez"),
    correo = c("maria@example.uy", "juan@example.uy")
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_setequal(perfil$datos_personales$tipo, c("cedula", "nombre", "correo"))
  personales <- perfil$hallazgos$tipo_hallazgo == "dato_personal_posible"
  expect_true(all(perfil$hallazgos$severidad[personales] == "ok"))
  expect_true(all(perfil$columnas$moda == "[valor protegido]"))

  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_false(grepl("1.234.567-2", html, fixed = TRUE))
  expect_false(grepl("María Fernández", html, fixed = TRUE))

  declarado <- perfilar(
    datos, analizar_dependencias = FALSE,
    datos_personales_permitidos = FALSE
  )
  personales <- declarado$hallazgos$tipo_hallazgo == "dato_personal_posible"
  expect_true(all(declarado$hallazgos$severidad[personales] == "error"))

  sin_proteccion <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  archivo_abierto <- tempfile(fileext = ".html")
  reportar(
    sin_proteccion, archivo = archivo_abierto,
    proteger_datos_personales = FALSE
  )
  html_abierto <- paste(readLines(archivo_abierto, warn = FALSE), collapse = "\n")
  expect_true(grepl("1.234.567-2", html_abierto, fixed = TRUE))
  expect_true(grepl("María Fernández", html_abierto, fixed = TRUE))
})

test_that("la clasificación personal explicita evidencia débil y protege dependencias", {
  por_nombre <- perfilar(
    data.frame(correo = c("sin arroba", "valor inválido")),
    analizar_dependencias = FALSE
  )
  expect_equal(por_nombre$datos_personales$tipo, "correo")
  expect_equal(por_nombre$datos_personales$fundamento, "nombre de columna")
  expect_equal(por_nombre$datos_personales$proporcion_compatible, 0)

  vacio <- perfilar(
    data.frame(correo = c(NA_character_, NA_character_)),
    analizar_dependencias = FALSE
  )
  expect_true(is.na(vacio$datos_personales$proporcion_compatible))

  por_forma <- perfilar(
    data.frame(codigo = c("12345672", "45678901")),
    analizar_dependencias = FALSE
  )
  expect_equal(por_forma$datos_personales$tipo, "cedula")
  expect_equal(por_forma$datos_personales$fundamento,
               "forma de documento dominante")

  cedulas <- paste0(seq_len(10), ".234.567-2")
  categorias <- rep(LETTERS[seq_len(10)], each = 20)
  categorias[[1L]] <- "Z"
  con_dependencia <- perfilar(data.frame(
    cedula = rep(cedulas, each = 20), categoria = categorias
  ))
  sensibles <- con_dependencia$dependencias$determinante == "cedula" |
    con_dependencia$dependencias$dependiente == "cedula"
  expect_true(any(sensibles))
  expect_true(any(
    con_dependencia$dependencias$evidencia[sensibles] == "[evidencia protegida]"
  ))
})

test_that("el reporte incluye siempre la cobertura del perfil", {
  perfil <- perfilar(data.frame(x = 1:3), analizar_dependencias = FALSE)
  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_match(html, "Cobertura del análisis", fixed = TRUE)
  expect_match(html, "no_declarada", fixed = TRUE)
})
