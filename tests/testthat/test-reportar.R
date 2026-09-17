.objetos_reporte_prueba <- function() {
  fecha_1 <- as.POSIXct("2026-01-31 12:00:00", tz = "UTC")
  fecha_2 <- as.POSIXct("2026-02-28 12:00:00", tz = "UTC")
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "PresenciaCodigo"),
    "personas", "codigo"
  )
  modelo_prueba <- modelo(instancia)
  medidas_1 <- medir(
    modelo_prueba, data.frame(codigo = c("A", NA, "C")),
    id_medicion = "enero", fecha = fecha_1
  )
  medidas_2 <- medir(
    modelo_prueba, data.frame(codigo = c("A", "B", "C")),
    id_medicion = "febrero", fecha = fecha_2
  )
  regla <- regla_evaluacion("Dato presente", function(x) x > 0)
  perfil_evaluacion_prueba <- perfil_evaluacion("Básico", regla)
  evaluacion_1 <- evaluar(medidas_1, perfil_evaluacion_prueba)
  evaluacion_2 <- evaluar(medidas_2, perfil_evaluacion_prueba)
  historico <- historico_calidad(evaluacion_2, evaluacion_1)
  anterior <- perfilar(
    data.frame(codigo = c("AA1", "AA2", "AA3")),
    nombre = "Padrón enero", fecha = fecha_1, expandir = TRUE
  )
  actual <- perfilar(
    data.frame(codigo = c("AA1", "2", "AA3", NA)),
    nombre = "Padrón febrero", fecha = fecha_2, expandir = TRUE
  )
  list(
    perfil = actual,
    medicion = medidas_2,
    evaluacion = evaluacion_2,
    historico = historico,
    deriva_perfil = comparar_perfiles(anterior, actual),
    deriva_calidad = detectar_deriva_calidad(historico),
    plan = planificar_limpieza(actual)
  )
}

.leer_html_prueba <- function(archivo) {
  paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("reportar crea un HTML autocontenido y seguro desde un perfil", {
  peligro <- paste0(
    "<script>alert(1)</script> & \"dato\" 'otro' https://externo.invalid ",
    "src=archivo @import"
  )
  datos <- data.frame(
    setNames(list(c(peligro, "normal", "S/D")), " columna <riesgo> "),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, nombre = peligro,
    fecha = as.POSIXct("2026-03-01 09:30:00", tz = "UTC"),
    expandir = TRUE
  )
  archivo <- tempfile("reporte & prueba ", fileext = ".html")
  resultado <- withVisible(reportar(
    perfil, archivo = archivo, titulo = peligro,
    fecha = as.POSIXct("2026-03-02 10:00:00", tz = "UTC")
  ))
  html <- .leer_html_prueba(archivo)

  expect_false(resultado$visible)
  expect_equal(resultado$value, normalizePath(archivo, winslash = "/"))
  expect_true(file.exists(archivo))
  expect_gt(file.info(archivo)$size, 0)
  expect_match(html, "<!doctype html>", fixed = TRUE)
  expect_match(html, "<html lang=\"es\">", fixed = TRUE)
  expect_match(html, "<head>", fixed = TRUE)
  expect_match(html, "</head>", fixed = TRUE)
  expect_match(html, "<body>", fixed = TRUE)
  expect_match(html, "</body>", fixed = TRUE)
  expect_match(html, "</html>", fixed = TRUE)
  expect_match(html, "<meta charset=\"UTF-8\">", fixed = TRUE)
  expect_match(html, "Dependencias funcionales", fixed = TRUE)
  expect_match(html, "<style>", fixed = TRUE)
  expect_match(html, "@media print", fixed = TRUE)
  expect_match(html, "overflow-x:auto", fixed = TRUE)
  expect_false(grepl("<script>alert\\(1\\)</script>", html))
  expect_match(
    html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE
  )
  expect_match(html, "&amp;", fixed = TRUE)
  expect_match(html, "&quot;dato&quot;", fixed = TRUE)
  expect_match(html, "&#39;otro&#39;", fixed = TRUE)
  expect_false(grepl("https?://|src\\s*=|@import", html, ignore.case = TRUE))
})

test_that("reportar conserva rangos plausibles para fechas importadas como texto", {
  perfil <- perfilar(
    data.frame(f = c("2023-11-30", "2023-12-01", "2023-06-15")),
    analizar_dependencias = FALSE
  )
  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo)
  html <- .leer_html_prueba(archivo)

  expect_match(html, "2023-06-15", fixed = TRUE)
  expect_false(grepl("4620236", html, fixed = TRUE))
})

test_that("reportar combina todas las secciones soportadas", {
  objetos <- .objetos_reporte_prueba()
  archivo <- tempfile(fileext = ".html")
  reportar(
    objetos, archivo = archivo,
    fecha = as.POSIXct("2026-03-02", tz = "UTC")
  )
  html <- .leer_html_prueba(archivo)

  expect_match(html, "Perfil de datos", fixed = TRUE)
  expect_match(html, "Medidas de calidad", fixed = TRUE)
  expect_match(html, "Evaluación de calidad", fixed = TRUE)
  expect_match(html, "Evaluaciones de reglas", fixed = TRUE)
  expect_match(html, "Perfiles de madurez", fixed = TRUE)
  expect_match(html, "Histórico de calidad", fixed = TRUE)
  expect_match(html, "Evolución de perfiles de madurez", fixed = TRUE)
  expect_match(html, "Deriva del perfil de datos", fixed = TRUE)
  expect_match(html, "Deriva del modelo de calidad", fixed = TRUE)
  expect_match(html, "Plan de limpieza", fixed = TRUE)
  expect_match(html, "justificacion", fixed = TRUE)
  expect_match(html, "delta", fixed = TRUE)
})

test_that("reportar informa de manera visible cada truncamiento", {
  datos <- data.frame(
    a = c("a", "A", "1", "-", "aa"),
    b = c("b", "B", "2", "+", "bb"),
    c = c("c", "C", "3", "*", "cc"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, expandir = TRUE, max_patrones = 10)
  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo, max_filas = 2, max_patrones = 2)
  html <- .leer_html_prueba(archivo)

  expect_match(html, "Se muestran 2 de 3 columnas con patrones", fixed = TRUE)
  expect_match(html, "Se muestran 2 de 5 patrones distintos", fixed = TRUE)
  expect_match(html, "Se muestran 2 de", fixed = TRUE)
  expect_equal(attr(perfil$patrones$a, "n_patrones_distintos"), 5L)
})

test_that("reportar valida rutas, objetos, límites y sobrescritura", {
  perfil <- perfilar(data.frame(x = 1:2))
  archivo <- tempfile(fileext = ".html")
  reportar(perfil, archivo = archivo)

  expect_error(reportar(perfil, archivo = archivo), "ya existe")
  expect_silent(reportar(perfil, archivo = archivo, sobrescribir = TRUE))
  expect_error(reportar(data.frame(x = 1)), "Cada objeto")
  expect_error(reportar(list()), "al menos un objeto")
  expect_error(reportar(perfil, titulo = ""), "titulo")
  expect_error(reportar(perfil, max_filas = 0), "max_filas")
  expect_error(reportar(perfil, max_patrones = NA), "max_patrones")
  expect_error(reportar(perfil, fecha = NA), "fecha")
  expect_error(reportar(perfil, sobrescribir = NA), "sobrescribir")
  expect_error(
    reportar(perfil, archivo = file.path(tempfile(), "reporte.html")),
    "No existe"
  )
  expect_error(reportar(perfil, archivo = ""), "archivo")
  completo <- reportar(perfil, max_filas = Inf, max_patrones = Inf)
  expect_true(file.exists(completo))
  unlink(completo)
})

test_that("los renderizadores cubren valores complejos y perfiles heredados", {
  expect_equal(lupa:::.resumir_valor_reporte(NULL), "")
  expect_equal(lupa:::.resumir_valor_reporte(identity), "<función>")
  expect_match(
    lupa:::.resumir_valor_reporte(data.frame(a = 1)),
    "tabla: 1 filas", fixed = TRUE
  )
  expect_equal(
    lupa:::.resumir_valor_reporte(as.Date("2026-01-02")), "2026-01-02"
  )
  expect_equal(lupa:::.resumir_valor_reporte(list("a")), "a")
  expect_match(
    lupa:::.resumir_valor_reporte(as.list(seq_len(10))), "…", fixed = TRUE
  )
  expect_equal(
    nchar(lupa:::.resumir_valor_reporte(strrep("x", 300))), 240L
  )
  expect_match(lupa:::.html_texto("a\r\nb"), "a<br>b", fixed = TRUE)
  expect_match(lupa:::.html_tabla(matrix(1:4, 2), 1), "<table>", fixed = TRUE)

  perfil <- perfilar(
    data.frame(texto = sprintf("valor-%03d", 1:200)),
    muestra = 20, max_patrones = 5
  )
  attr(perfil$patrones$texto, "n_patrones_distintos") <- NULL
  perfil$hallazgos$severidad <- NULL
  html <- lupa:::.seccion_perfil(perfil, Inf, Inf)
  expect_match(html, "Patrones estimados sobre 20 de 200", fixed = TRUE)

  objetos <- .objetos_reporte_prueba()
  solo_medidas <- historico_calidad(objetos$medicion)
  expect_equal(nrow(lupa:::.evolucion_historico(solo_medidas)), 0L)
})

test_that("un fallo de escritura se informa sin dejar un resultado parcial", {
  testthat::local_mocked_bindings(
    .copiar_archivo_reporte = function(...) FALSE,
    .package = "lupa"
  )
  archivo <- tempfile(fileext = ".html")
  expect_error(
    lupa:::.escribir_reporte("<html></html>", archivo, FALSE),
    "No se pudo escribir"
  )
  expect_false(file.exists(archivo))
})

# El informe es lo que se comparte con quien no corre R: si el objeto declara
# que algo quedo fuera de la medicion y el informe lo calla, quien lee el
# informe cree que se midio. El muestreo y `cobertura_diagnosticos` ya se
# publicaban; estas dos declaraciones de `meta` no.
test_that("el informe publica lo que el perfil declara sobre su alcance", {
  # a. La entrada convertida. El .Rd dice que la declaracion existe "para que
  #    el perfil no aparente haber recibido lo que no recibio", y el informe
  #    volvia a aparentarlo.
  perfil <- perfilar(matrix(1:60, nrow = 10))
  expect_true(nzchar(perfil$meta$entrada_convertida))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  invisible(reportar(perfil, archivo = archivo))
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Entrada convertida", fixed = TRUE)

  # b. Las columnas que el analisis de orden dejo sin comparar.
  set.seed(7)
  datos <- as.data.frame(matrix(rnorm(25 * 30), ncol = 25))
  names(datos) <- paste0("v", seq_len(25))
  perfil_ancho <- perfilar(datos)
  omitidas <- perfil_ancho$meta$orden_columnas$columnas_omitidas
  expect_true(isTRUE(perfil_ancho$meta$orden_columnas$truncado))
  expect_true(length(omitidas) > 0L)

  archivo2 <- tempfile(fileext = ".html")
  on.exit(unlink(archivo2), add = TRUE)
  invisible(reportar(perfil_ancho, archivo = archivo2))
  html2 <- paste(readLines(archivo2, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html2, "sin comparar")
  for (columna in omitidas) {
    expect_match(html2, columna, fixed = TRUE)
  }
})

# El control, y es el que impide que la seccion se vuelva ruido: un perfil que
# no tiene nada que declarar no gana una seccion que afirme algo.
test_that("el informe no inventa una seccion de alcance", {
  perfil <- perfilar(
    data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  )
  # Para un `data.frame` el campo existe y vale NA: no hay conversion que
  # declarar. La guarda mira el valor, no la presencia del campo.
  expect_true(is.na(perfil$meta$entrada_convertida))
  expect_false(isTRUE(perfil$meta$orden_columnas$truncado))

  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  invisible(reportar(perfil, archivo = archivo))
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Alcance de la corrida", html, fixed = TRUE))
})
