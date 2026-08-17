.medidas_r111 <- function() {
  datos <- data.frame(
    Importe = c("10", "20", NA, "40", "20"),
    Resolucion = c("AA01", "AA02", "AA03", "MAL", "AA02"),
    stringsAsFactors = FALSE
  )
  nucleo <- metricas_nucleo()
  instancias <- list(
    instanciar(especializar(nucleo$NoNulo), "padron", "Importe"),
    instanciar(
      especializar(nucleo$Formato, expresion_regular = "^AA[0-9]{2}$"),
      "padron", "Resolucion"
    ),
    instanciar(especializar(nucleo$EntidadDuplicada), "padron")
  )
  medir(
    modelo(instancias), datos, id_medicion = "ronda-111",
    fecha = as.POSIXct("2026-08-16", tz = "UTC")
  )
}

test_that("el tablero declara filas, agregaciones, orientacion y alcance", {
  tablero <- tablero_calidad(.medidas_r111())
  alcance <- attr(tablero, "alcance", exact = TRUE)

  expect_s3_class(tablero, "tablero_calidad")
  expect_equal(nrow(tablero), 3L)
  expect_equal(
    names(tablero),
    c(
      "componente", "dimension", "factor", "metrica", "objeto", "valor",
      "orientacion", "agregacion", "umbral", "universo"
    )
  )
  expect_equal(tablero$agregacion, rep("ratio", 3L))
  expect_equal(
    tablero$orientacion, c("conformidad", "conformidad", "defecto")
  )
  expect_equal(tablero$objeto, c("Importe", "Resolucion", "(tabla)"))
  expect_equal(alcance$factores_marco, 17L)
  expect_equal(alcance$factores_medidos, 3L)
  expect_equal(
    sum(alcance[c(
      "factores_medidos", "sin_metrica_declarada", "no_aplican",
      "fuera_de_alcance"
    )]),
    alcance$factores_marco
  )
  salida <- capture.output(impreso <- print(tablero))
  expect_match(paste(salida, collapse = "\n"), "factores_marco")
  expect_identical(impreso, tablero)
})

test_that("el tablero permite cambiar una agregacion real con umbral", {
  metrica_real <- metrica(
    "RealR111", "Proporcion real de prueba.", "instanciaAtributo", "real",
    dimension = "Completitud", factor = "Densidad",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad]][[instancia$atributos]]
      data.frame(
        resultado = x, entidad = instancia$entidad,
        atributo = instancia$atributos, fila = seq_along(x),
        objeto = paste0("tabla$x[", seq_along(x), "]"),
        stringsAsFactors = FALSE
      )
    },
    orientacion = "conformidad"
  )
  medidas <- medir(
    modelo(instanciar(especializar(metrica_real), "tabla", "x")),
    data.frame(x = c(0.2, 0.8, 0.9)), id_medicion = "real-r111"
  )
  promedio <- tablero_calidad(medidas)
  umbral <- tablero_calidad(
    medidas, agregaciones = "ratio_umbral",
    umbrales = stats::setNames(0.8, unique(medidas$metrica_instanciada))
  )

  expect_equal(promedio$agregacion, "promedio")
  expect_equal(promedio$valor, mean(c(0.2, 0.8, 0.9)))
  expect_equal(umbral$agregacion, "ratio_umbral")
  expect_equal(umbral$umbral, 0.8)
  expect_equal(umbral$valor, 2 / 3)
  expect_error(
    tablero_calidad(medidas, agregaciones = "ratio_umbral"),
    "requiere un umbral"
  )
})

test_that("indice_calidad sin pesos devuelve el tablero", {
  medidas <- .medidas_r111()
  expect_s3_class(indice_calidad(medidas), "tablero_calidad")
  expect_identical(indice_calidad(tablero_calidad(medidas)),
                   tablero_calidad(medidas))
})

test_that("el indice conserva transformaciones, pesos, cobertura y universos", {
  indice <- indice_calidad(
    .medidas_r111(),
    pesos = c(Completitud = 0.5, Exactitud = 0.3, Unicidad = 0.2)
  )

  expect_s3_class(indice, "indice_calidad")
  expect_equal(indice$valor, 0.5 * 0.8 + 0.3 * 0.8 + 0.2 * 0.6)
  expect_identical(
    indice$pesos,
    c(Completitud = 0.5, Exactitud = 0.3, Unicidad = 0.2)
  )
  expect_equal(indice$cobertura$factores_marco, 17L)
  expect_equal(indice$cobertura$factores_en_indice, 3L)
  expect_match(indice$cobertura$factores, "Completitud / Densidad")
  expect_equal(nrow(indice$invertidas), 1L)
  expect_equal(indice$invertidas$metrica, "EntidadDuplicada")
  expect_equal(indice$invertidas$transformacion, "1 - valor")
  expect_equal(indice$invertidas$valor_indice, 0.6)
  expect_match(indice$advertencia_universos, "universos distintos")
  expect_match(indice$advertencia_universos, "celdas")
  expect_match(indice$advertencia_universos, "filas")
  mensajes <- testthat::capture_messages(
    salida <- capture.output(impreso <- print(indice))
  )
  expect_match(paste(mensajes, collapse = "\n"), "Cobertura del .ndice",
               ignore.case = TRUE)
  expect_identical(impreso, indice)
})

test_that("los pesos del indice fallan con mensajes nominales", {
  medidas <- .medidas_r111()
  expect_error(
    indice_calidad(
      medidas, pesos = c(Completitud = 0.4, Exactitud = 0.3, Unicidad = 0.2)
    ),
    "sumar uno"
  )
  expect_error(
    indice_calidad(
      medidas,
      pesos = c(
        Completitud = 0.4, Exactitud = 0.3, Unicidad = 0.2,
        Fantasma = 0.1
      )
    ),
    "Sobran pesos para: Fantasma"
  )
  expect_error(
    indice_calidad(
      medidas, pesos = c(Completitud = 0.5, Exactitud = 0.5)
    ),
    "Faltan pesos para: Unicidad"
  )
})

test_that("una dimension con varios objetos exige combinacion interna", {
  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(list(
      instanciar(
        especializar(nucleo$NoNulo), "tabla", "a",
        nombre_instancia = "no-nulo-a"
      ),
      instanciar(
        especializar(nucleo$NoNulo), "tabla", "b",
        nombre_instancia = "no-nulo-b"
      )
    )),
    data.frame(a = c(1, NA), b = c(1, 2)), id_medicion = "interno-r111"
  )
  tablero <- tablero_calidad(medidas)
  expect_error(
    indice_calidad(tablero, pesos = c(Completitud = 1)),
    "requieren `pesos_internos`: Completitud"
  )
  internos <- stats::setNames(c(0.25, 0.75), tablero$componente)
  indice <- indice_calidad(
    tablero, pesos = c(Completitud = 1), pesos_internos = internos
  )
  expect_equal(indice$valor, 0.25 * 0.5 + 0.75 * 1)
  expect_match(
    indice$dimensiones$combinacion_interna,
    "pesos_internos declarados"
  )
})

test_that("no_aplica se excluye y un tablero entero no produce indice", {
  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(especializar(nucleo$ErrorEstandar), "tabla", "x")),
    data.frame(x = c(1, 2, 4)), id_medicion = "no-aplica-r111"
  )
  adicional <- medidas
  adicional$id_medicion[] <- "ronda-111"
  combinadas <- rbind(.medidas_r111(), adicional)
  class(combinadas) <- c("medicion", "data.frame")
  mixto <- indice_calidad(
    combinadas,
    pesos = c(Completitud = 0.5, Exactitud = 0.3, Unicidad = 0.2)
  )
  expect_equal(nrow(mixto$excluidas), 1L)
  expect_equal(mixto$excluidas$metrica, "ErrorEstandar")
  expect_match(mixto$excluidas$motivo_exclusion, "no_aplica")

  resultado <- indice_calidad(medidas, pesos = c(Exactitud = 1))

  expect_true(is.na(resultado$valor))
  expect_equal(nrow(resultado$excluidas), 1L)
  expect_equal(resultado$excluidas$orientacion, "no_aplica")
  expect_match(resultado$motivo, "No hay .ndice", ignore.case = TRUE)
  salida <- capture.output(print(resultado))
  expect_match(paste(salida, collapse = "\n"), "no_aplica")
})

test_that("analizar mide y agrega sin retener detalle salvo pedido", {
  datos <- data.frame(
    codigo = c("AA01", "AA02", NA, "AA02"),
    stringsAsFactors = FALSE
  )
  resultado <- analizar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  con_detalle <- analizar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE,
    conservar_detalle_medicion = TRUE
  )

  expect_s3_class(resultado$tablero, "tablero_calidad")
  expect_s3_class(resultado$medicion, "medicion")
  expect_null(resultado$detalle_medicion)
  expect_gt(nrow(con_detalle$detalle_medicion), nrow(con_detalle$medicion))
  expect_true(all(!resultado$decision_medicion$confirmada))
  expect_true(all(resultado$decision_medicion$medida[
    resultado$decision_medicion$estado == "lista"
  ]))

  apagado <- analizar(
    datos, medir_propuesta = FALSE, analizar_dependencias = FALSE
  )
  expect_null(apagado$medicion)
  expect_equal(nrow(apagado$tablero), 0L)
})

test_that("reportar incluye tablero y autoria no confirmada", {
  analisis <- analizar(
    data.frame(codigo = c("AA01", NA, "AA01")),
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  archivo <- tempfile(fileext = ".html")
  reportar(analisis, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_match(html, "Tablero de calidad", fixed = TRUE)
  expect_match(
    html, "La propuesta es de lupa y nadie la confirm\u00f3.", fixed = TRUE
  )
  expect_match(html, "Decisi\u00f3n de medici\u00f3n", fixed = TRUE)
  expect_match(html, "agregacion", fixed = TRUE)
})

test_that("analizar 50000 filas conserva un resultado pequeno", {
  n <- 50000L
  datos <- data.frame(
    id = sprintf("ID%06d", seq_len(n)),
    codigo = rep(sprintf("AA%03d", 1:20), length.out = n),
    categoria = rep(LETTERS[1:5], length.out = n),
    importe = as.numeric(seq_len(n)),
    observacion = rep(c("dato valido", "dato valido", NA), length.out = n),
    stringsAsFactors = FALSE
  )
  resultado <- analizar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )

  expect_null(resultado$detalle_medicion)
  expect_lte(nrow(resultado$medicion), nrow(resultado$decision_medicion))
  expect_lt(as.numeric(object.size(resultado)), 10 * 1024^2)
})
