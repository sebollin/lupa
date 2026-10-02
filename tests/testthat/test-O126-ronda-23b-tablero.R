# Ronda 23-B: la agregacion, el tablero y el indice de calidad.

.fecha_O126 <- as.POSIXct("2026-02-01", tz = "UTC")

test_that("la supresion del tablero se empareja por la metrica instanciada", {
  nucleo <- metricas_nucleo()
  datos <- data.frame(codigo = c("A", "B", "A"), stringsAsFactors = FALSE)
  for (nombre in list(NULL, "DuplicadoPadron")) {
    modelo_confirmado <- modelo(list(instanciar(
      especializar(nucleo$EntidadDuplicada, nombre), "datos"
    )))
    analisis <- suppressWarnings(analizar(
      datos, modelo_confirmado = modelo_confirmado,
      perfil_evaluacion = perfil_evaluacion("Operativo", regla_evaluacion(
        "Medida publicable", function(x) x > 0.9, desenlace = "suprimir"
      )),
      analizar_dependencias = FALSE, fecha = .fecha_O126, id_medicion = "m1"
    ))
    tablero <- tablero_calidad(analisis)
    # Antes, con nombre propio: 0.6666667 en el tablero, el indice y el HTML.
    expect_identical(as.character(tablero$valor), "[valor suprimido]",
                     info = if (is.null(nombre)) "sin nombre" else nombre)
    expect_true(is.na(indice_calidad(analisis, pesos = c(Unicidad = 1))$valor))
  }
  # Y sin nombre propio, solo la instancia suprimida: no la otra de la misma
  # metrica generica, que cumple.
  instancias <- list(
    instanciar(especializar(nucleo$NoNulo), "t", "edad"),
    instanciar(especializar(nucleo$NoNulo), "t", "codigo")
  )
  medicion <- medir(modelo(instancias),
                    list(t = data.frame(edad = c(1, NA, NA, 4),
                                        codigo = c("a", "b", NA, "d"))),
                    id_medicion = "m", fecha = .fecha_O126)
  evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla_evaluacion(
    "R", function(x) x > 0.9, metricas = "NoNulo@t.edad", desenlace = "suprimir"
  )))
  tablero <- lupa:::.proteger_tablero_desenlaces(
    tablero_calidad(medicion), lupa:::.desenlaces_de_objeto(evaluacion)
  )
  codigo <- tablero[tablero$metrica_instanciada == "NoNulo@t.codigo", ]
  expect_false(identical(as.character(codigo$valor), "[valor suprimido]"))
})

.medicion_alcance_O126 <- function() {
  nucleo <- metricas_nucleo()
  especie <- especializar(nucleo$GradoOportunidadAtributoPorFecha,
                          fecha_solicitud = as.Date("2026-01-01"),
                          fecha_fin_utilidad = as.Date("2026-01-11"))
  datos <- data.frame(
    x = as.Date(c("2025-12-31", "2026-01-01", "2026-01-06", "2026-01-11",
                  "2026-01-20", NA)),
    y = as.Date(c("2025-12-31", "2026-01-01", "2026-01-06", "2026-01-11",
                  "2026-01-20", "2026-01-06"))
  )
  medir(modelo(instanciar(especie, "t", "x"), instanciar(especie, "t", "y")),
        datos, id_medicion = "M", fecha = .fecha_O126)
}

test_that("el alcance agregado suma las partes completas y se atribuye por fila", {
  medicion <- .medicion_alcance_O126()
  dos_pasos <- agregar(agregar(medicion, "atributo", "promedio"),
                       "entidad", "promedio")
  alcance <- attr(dos_pasos, "alcance_medidas")
  # Antes: "5 de 6", lo de x solo; el numero uso 5 + 6 = 11 de 12.
  expect_identical(c(alcance$medidas, alcance$en_el_universo), c(11, 12))
  por_fila <- attr(agregar(medicion, "instanciaEntidad", "promedio"), "alcance_medidas")
  # Antes: cinco filas iguales con "5 de 6" y nada sobre la sexta, la unica parcial.
  expect_identical(nrow(por_fila), 1L)
  expect_equal(c(por_fila$medidas, por_fila$en_el_universo), c(1, 2))
  expect_match(por_fila$motivo, "^Fila 6")
  # Y rbind() no pierde el alcance de la parte que no va primero.
  completa <- medir(modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "u", "z")),
                    list(u = data.frame(z = 1:3)), id_medicion = "M",
                    fecha = .fecha_O126)
  expect_false(is.null(attr(rbind(completa, medicion), "alcance_medidas")))
})

test_that("ratio_umbral alcanza el umbral en el borde, en el tablero y en agregar", {
  especie <- especializar(metricas_nucleo()$GradoOportunidadAtributoPorFecha,
                          fecha_solicitud = as.Date("2026-01-01"),
                          fecha_fin_utilidad = as.Date("2026-01-11"))
  instancia <- instanciar(especie, "t", "entrega")
  medicion <- medir(modelo(instancia),
                    data.frame(entrega = as.Date("2026-01-01") + 0:10),
                    id_medicion = "M", fecha = .fecha_O126)
  nombre <- unique(medicion$metrica_instanciada)
  for (umbral in c(0.1, 0.2)) {
    a_mano <- mean((10 - 0:10) >= round(10 * umbral))
    tablero <- tablero_calidad(medicion,
                               agregaciones = setNames("ratio_umbral", nombre),
                               umbrales = setNames(umbral, nombre))
    # Antes: 0,0999...9 no alcanzaba 0,1, y el tablero daba 9/11.
    expect_equal(tablero$valor, a_mano, info = umbral)
    expect_equal(agregar(medicion, "atributo", "ratio_umbral", umbral = umbral)$resultado,
                 a_mano, info = umbral)
  }
})

test_that("el universo de una celda no depende del camino", {
  datos <- data.frame(codigo = c("a", "b", NA, "d"), stringsAsFactors = FALSE)
  modelo_confirmado <- modelo(list(instanciar(
    especializar(metricas_nucleo()$NoNulo), "datos", "codigo"
  )))
  analisis <- suppressWarnings(analizar(datos, modelo_confirmado = modelo_confirmado,
                                        analizar_dependencias = FALSE,
                                        fecha = .fecha_O126, id_medicion = "m"))
  # Antes: "celdas" en el tablero del analisis y "columnas" desde su medicion.
  expect_identical(tablero_calidad(analisis$medicion)$universo,
                   tablero_calidad(analisis)$universo)
})

test_that("el tablero no hace pasar una duracion por una proporcion", {
  especie <- especializar(
    metricas_nucleo()$DesactualizacionPorFecha,
    vigencia = vigencia("f", fecha_acceso = as.Date("2026-02-01"),
                        frecuencia_cambio = 1)
  )
  instancia <- instanciar(especie, "t", "x")
  datos <- data.frame(f = as.Date(c("2026-02-01", "2025-11-01")), x = 1:2)
  medicion <- medir(modelo(instancia), datos, id_medicion = "M", fecha = .fecha_O126)
  # Antes: el mensaje dependia de los valores -"[0, 1]" con 91 dias-.
  expect_error(agregar(medicion, "atributo", "promedio"), "duracion")
  agregada <- suppressWarnings(analizar(
    datos, modelo_confirmado = modelo(instancia), analizar_dependencias = FALSE,
    fecha = .fecha_O126, id_medicion = "M"
  ))$medicion
  fila <- agregada[agregada$metrica == "DesactualizacionPorFecha", ]
  # Antes: tipo "real", y agregar() la aceptaba despues como proporcion.
  expect_identical(as.character(fila$tipo_resultado), "duracion")
  expect_error(agregar(fila, "entidad", "promedio"), "duracion")
})

test_that("agregar rechaza medidas repetidas y nombra la causa de un valor fuera de rango", {
  medicion <- medir(modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x")),
                    data.frame(x = c(1, NA, 3)), id_medicion = "M", fecha = .fecha_O126)
  # Antes: el doble de medidas, como una sola celda.
  expect_error(agregar(rbind(medicion, medicion), "atributo", "ratio"), "repiten")
  expect_error(tablero_calidad(rbind(medicion, medicion)), "repiten")
  con_na <- medicion
  con_na$resultado[[1L]] <- NA
  # Antes: "deben estar en [0, 1]", sin la metrica ni la causa.
  expect_error(agregar(con_na, "atributo", "ratio"), "NoNulo@t.x.*ausentes")
  # Un umbral que la agregacion no usa se rechaza.
  expect_error(agregar(medicion, "atributo", "ratio", umbral = 0.9), "ratio_umbral")
  expect_error(
    tablero_calidad(medicion, umbrales = c("NoNulo@t.x" = 0.9)), "Sobra el umbral"
  )
})

test_that("dos partes agregadas por separado no repiten su identificador", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  medicion <- medir(
    modelo(instanciar(especie, "t1", "x"), instanciar(especie, "t2", "x")),
    list(t1 = data.frame(x = c(1, NA)), t2 = data.frame(x = c(1, 2))),
    id_medicion = "M", fecha = .fecha_O126
  )
  partes <- lapply(c("t1", "t2"), function(tabla) {
    agregar(medicion[medicion$entidad == tabla, ], "atributo", "ratio")
  })
  union <- rbind(partes[[1L]], partes[[2L]])
  # Antes: las dos partes salian como `M-agg-ratio-000001`, y `evaluar()` y el
  # tablero rechazaban la union como si fuera una medida repetida.
  expect_false(anyDuplicated(union$id_medida) > 0L)
  regla <- regla_evaluacion("Completo", function(x) x == 1,
                            metricas = unique(union$metrica_instanciada))
  evaluacion <- evaluar(union, perfil_evaluacion("Control", regla))
  expect_setequal(evaluacion$medidas$id_medida, union$id_medida)
  expect_equal(nrow(tablero_calidad(union)), 2L)
  # El mismo agregado repetido sigue repitiendo su identificador.
  expect_error(tablero_calidad(rbind(partes[[1L]], partes[[1L]])), "repiten")
  expect_identical(
    agregar(medicion[medicion$entidad == "t1", ], "atributo", "ratio")$id_medida,
    partes[[1L]]$id_medida
  )
})

test_that("una corrida armada por partes no repite sus identificadores", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  parte <- function(tabla, valores) {
    datos <- list(data.frame(x = valores))
    names(datos) <- tabla
    medir(modelo(instanciar(especie, tabla, "x")), datos,
          id_medicion = "M", fecha = .fecha_O126)
  }
  a <- parte("t1", c(1, NA))
  union <- rbind(a, parte("t2", c(1, 2)))
  # Antes: las dos partes numeraban desde `M-000001`, y `evaluar()` y
  # `historico_calidad()` rechazaban la corrida como unida consigo misma.
  expect_false(anyDuplicated(union$id_medida) > 0L)
  regla <- regla_evaluacion("Completo", function(x) x == 1,
                            metricas = unique(union$metrica_instanciada))
  evaluacion <- evaluar(union, perfil_evaluacion("Control", regla))
  expect_setequal(evaluacion$medidas$id_medida, union$id_medida)
  expect_equal(nrow(as.data.frame(historico_calidad(union))) > 0L, TRUE)
  # La misma parte dos veces sigue siendo una medida repetida.
  expect_error(evaluar(rbind(a, a), perfil_evaluacion("Control", regla)), "repite")
  expect_error(historico_calidad(rbind(a, a)), "repite")
})

test_that("una metrica sin dimension se nombra o se excluye", {
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    data.frame(resultado = !is.na(x), entidad = instancia$entidad,
               atributo = instancia$atributos, fila = seq_along(x),
               objeto = paste0(instancia$entidad, "$", instancia$atributos, "[",
                               seq_along(x), "]"),
               stringsAsFactors = FALSE)
  }
  sin_dimension <- metrica("OrigenDeclarado", "Indica si se declaro el origen.",
                           "instanciaAtributo", "booleano",
                           orientacion = "conformidad", metodo = metodo)
  medicion <- tryCatch(
    medir(modelo(sin_dimension()(entidad = "entrega", atributos = "origen")),
          list(entrega = data.frame(origen = c("a", NA))),
          id_medicion = "M", fecha = .fecha_O126),
    error = function(e) NULL
  )
  skip_if(is.null(medicion), "metrica() exige ahora una dimension")
  # Antes: "Las dimensiones y los factores no pueden ser ausentes ni vacios."
  expect_error(tablero_calidad(medicion), "OrigenDeclarado")
})
