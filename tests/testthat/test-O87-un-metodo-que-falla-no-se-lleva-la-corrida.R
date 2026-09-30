# Un metodo que falla no se lleva la medicion de las demas metricas.
#
# `?medir` promete que una metrica que no puede medirse deja su motivo en
# `cobertura_metricas`. Cinco metodos del catalogo -`ErrorEstandar`, `Escala`, las
# dos `OportunidadAtributoPor*` y cualquiera con un atributo que la tabla no trae-
# llamaban `stop()`, y el error mataba la corrida ENTERA: ni filas, ni coberturas,
# ni la medicion de las otras metricas del modelo. Medido: `NoNulo` y
# `ErrorEstandar` sobre una columna de un solo valor devolvian un error y nada mas.
#
# El arreglo envuelve la LLAMADA al metodo, no los cinco metodos: la propiedad es
# "un metodo que falla no tumba a los demas", y alcanza tambien a los metodos que
# el usuario escribe. La metrica queda `no_medible` -no `contrato_incompleto`, que
# dice otra cosa- y `medir()` avisa, porque el error era la unica senal y
# callarlo habria dejado una medicion con menos filas en silencio.

.o87_modelo <- function(otra) {
  nucleo <- metricas_nucleo()
  modelo(
    instanciar(especializar(nucleo$NoNulo, nombre_especifico = "Completa"),
               "t", "edad"),
    otra
  )
}

.o87_medir <- function(modelo, datos) {
  avisos <- character()
  medicion <- withCallingHandlers(
    medir(modelo, list(t = datos), id_medicion = "o87"),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(medicion = medicion, avisos = avisos)
}

test_that("la otra metrica del modelo se mide igual", {
  nucleo <- metricas_nucleo()
  ee <- instanciar(especializar(nucleo$ErrorEstandar), "t", "edad")
  casos <- list(
    todo_na = list(datos = data.frame(edad = c(NA_real_, NA, NA)),
                   mensaje = "al menos dos"),
    un_valor = list(datos = data.frame(edad = c(5, NA, NA)),
                    mensaje = "al menos dos"),
    texto = list(datos = data.frame(edad = c("a", "b", "c"),
                                    stringsAsFactors = FALSE),
                 mensaje = "num.*rico")
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    r <- .o87_medir(.o87_modelo(ee), caso$datos)
    # Las tres medidas de `NoNulo` sobreviven.
    expect_equal(sum(r$medicion$metrica == "NoNulo"), 3L, info = nombre)
    expect_false(any(r$medicion$metrica == "ErrorEstandar"), info = nombre)
    cobertura <- attr(r$medicion, "cobertura_metricas", exact = TRUE)
    expect_equal(nrow(cobertura), 1L, info = nombre)
    expect_identical(as.character(cobertura$estado), "no_medible")
    # El motivo nombra la instancia y conserva lo que dijo el metodo.
    expect_true(grepl("ErrorEstandar@t.edad", cobertura$motivo, fixed = TRUE),
                info = nombre)
    expect_true(grepl(caso$mensaje, cobertura$motivo), info = nombre)
    expect_true(nzchar(cobertura$como_resolverlo))
    expect_true(any(grepl("no se midieron porque su m", r$avisos, fixed = TRUE)),
                info = nombre)
  }
})

test_that("vale para todo metodo, tambien el que escribe el usuario", {
  nucleo <- metricas_nucleo()
  fechas <- data.frame(
    edad = c(1, 2, 3), f = as.Date("2026-01-01") + 0:2, texto = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  propia <- metrica(
    "Propia", "Una metrica que siempre falla.", "atributo", "real"
  )
  casos <- list(
    escala_no_finita = list(
      instancia = instanciar(especializar(nucleo$Escala, escala = escala(0.1)),
                             "t", "edad"),
      datos = data.frame(edad = c(1, Inf, 3)), mensaje = "no finitos"
    ),
    oportunidad_sin_fecha = list(
      instancia = instanciar(
        especializar(nucleo$OportunidadAtributoPorFecha,
                     fecha_limite = as.Date("2026-01-02")),
        "t", "edad"
      ),
      datos = fechas, mensaje = "Date o POSIXt"
    ),
    atributo_mal_escrito = list(
      instancia = instanciar(especializar(nucleo$NoNulo), "t", "eddad"),
      datos = fechas, mensaje = "eddad"
    ),
    metodo_del_usuario = list(
      instancia = instanciar(
        especializar(propia), "t", "edad",
        metodo = function(tablas, instancia) stop("el metodo propio fallo")
      ),
      datos = fechas, mensaje = "el metodo propio fallo"
    )
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    r <- .o87_medir(.o87_modelo(caso$instancia), caso$datos)
    expect_equal(sum(r$medicion$metrica == "NoNulo" &
                       r$medicion$metrica_especifica == "Completa"),
                 nrow(caso$datos), info = nombre)
    cobertura <- attr(r$medicion, "cobertura_metricas", exact = TRUE)
    expect_identical(as.character(cobertura$estado), "no_medible", info = nombre)
    expect_true(grepl(caso$mensaje, cobertura$motivo, fixed = TRUE), info = nombre)
  }
})

test_that("el control: una metrica sana no deja cobertura ni avisa", {
  ee <- instanciar(especializar(metricas_nucleo()$ErrorEstandar), "t", "edad")
  r <- .o87_medir(.o87_modelo(ee), data.frame(edad = c(5, 6, 7)))
  expect_null(attr(r$medicion, "cobertura_metricas", exact = TRUE))
  expect_false(any(grepl("no se midieron", r$avisos, fixed = TRUE)))
  expect_equal(sum(r$medicion$metrica == "ErrorEstandar"), 1L)
})

test_that("el aviso no afirma que se midieron otras si no hay otras", {
  ee <- instanciar(especializar(metricas_nucleo()$ErrorEstandar), "t", "edad")
  sola <- .o87_medir(modelo(ee), data.frame(edad = c(5, NA)))
  expect_equal(nrow(sola$medicion), 0L)
  expect_false(any(grepl("las dem", sola$avisos, fixed = TRUE)))
  acompanada <- .o87_medir(.o87_modelo(ee), data.frame(edad = c(5, NA)))
  expect_true(any(grepl("las dem", acompanada$avisos, fixed = TRUE)))
})

test_that("el estado viaja: impresion, evaluacion e historico", {
  ee <- instanciar(especializar(metricas_nucleo()$ErrorEstandar), "t", "edad")
  medicion <- .o87_medir(.o87_modelo(ee), data.frame(edad = c(5, NA)))$medicion
  lineas_cli <- NULL
  otras <- capture.output(lineas_cli <- cli::cli_fmt(print(medicion)))
  expect_true(grepl("no_medible", paste(c(otras, lineas_cli), collapse = " "),
                    fixed = TRUE))
  # Una metrica que no se midio no es un cero ni un exito al evaluar: sin otra
  # medida, `evaluar()` se niega y repite el motivo del metodo.
  sola <- .o87_medir(modelo(ee), data.frame(edad = c(5, NA)))$medicion
  expect_error(
    evaluar(sola, perfil_evaluacion("P", regla_evaluacion("R", function(x) x < 1))),
    "al menos dos"
  )
  historico <- as.data.frame(acumular_historico(historico_calidad(), medicion))
  fila <- historico[historico$nivel == "metrica_no_evaluada", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$agregacion, "no_medible")
})
