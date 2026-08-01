instancia_nueva <- function(metrica, atributos = character(), ...) {
  instanciar(especializar(metrica, ...), "tabla", atributos)
}

test_that("ValoresPosiblesPorComprension admite predicado y rango", {
  nucleo <- metricas_nucleo()
  por_rango <- instancia_nueva(
    nucleo$ValoresPosiblesPorComprension, "edad",
    minimo = 18, maximo = 65, inclusivo = c(TRUE, FALSE)
  )
  por_predicado <- instancia_nueva(
    nucleo$ValoresPosiblesPorComprension, "codigo",
    predicado = function(x) grepl("^[A-Z]{2}$", x)
  )
  medicion <- medir(
    modelo(por_rango, por_predicado),
    data.frame(
      edad = c(18, 64, 65, NA),
      codigo = c("UY", "x", NA, "AR")
    )
  )

  rango <- medicion[medicion$atributo == "edad", , drop = FALSE]
  predicado <- medicion[medicion$atributo == "codigo", , drop = FALSE]
  expect_equal(rango$resultado, c(1, 1, 0))
  expect_equal(predicado$resultado, c(1, 0, 1))
  expect_equal(agregar(rango, "atributo", "ratio")$resultado, 2 / 3)

  inclusivo <- instancia_nueva(
    nucleo$ValoresPosiblesPorComprension, "edad",
    minimo = 18, maximo = 65, inclusivo = TRUE
  )
  expect_equal(
    medir(modelo(inclusivo), data.frame(edad = c(18, 65)))$resultado,
    c(1, 1)
  )
})

test_that("las duplicaciones marcan todas las apariciones del grupo", {
  nucleo <- metricas_nucleo()
  atributo <- instancia_nueva(nucleo$AtributoDuplicado, "id")
  conjunto <- instancia_nueva(
    nucleo$ConjuntoAtributosDuplicado, c("nombre", "zona")
  )
  entidad <- instancia_nueva(nucleo$EntidadDuplicada)
  datos <- data.frame(
    id = c(1, 1, 2, 3, NA, NA),
    nombre = c("Ana", "Ana", "Beto", "Caro", NA, NA),
    zona = c("N", "N", "S", "S", "X", "X"),
    stringsAsFactors = FALSE
  )
  medicion <- medir(modelo(atributo, conjunto, entidad), datos)

  por_metrica <- split(medicion$resultado, medicion$metrica)
  expect_equal(por_metrica$AtributoDuplicado, c(1, 1, 0, 0))
  expect_equal(
    por_metrica$ConjuntoAtributosDuplicado, c(1, 1, 0, 0, 1, 1)
  )
  expect_equal(por_metrica$EntidadDuplicada, c(1, 1, 0, 0, 1, 1))

  medidas_atributo <- medicion[
    medicion$metrica == "AtributoDuplicado", , drop = FALSE
  ]
  medidas_conjunto <- medicion[
    medicion$metrica == "ConjuntoAtributosDuplicado", , drop = FALSE
  ]
  medidas_entidad <- medicion[
    medicion$metrica == "EntidadDuplicada", , drop = FALSE
  ]
  expect_equal(
    agregar(medidas_atributo, "atributo", "ratio")$resultado, 0.5
  )
  expect_equal(
    agregar(medidas_conjunto, "entidad", "ratio")$resultado, 4 / 6
  )
  expect_equal(
    agregar(medidas_entidad, "entidad", "ratio")$resultado, 4 / 6
  )
})

test_that("DesactualizacionPorFormato identifica formatos obsoletos", {
  nucleo <- metricas_nucleo()
  telefono <- instancia_nueva(
    nucleo$DesactualizacionPorFormato, "telefono",
    expresion_regular = "^[0-9]{8}$"
  )
  validador <- instancia_nueva(
    nucleo$DesactualizacionPorFormato, "codigo",
    validador = function(x) nchar(x) == 3L
  )
  medicion <- medir(
    modelo(telefono, validador),
    data.frame(
      telefono = c("29001234", "2001234", NA),
      codigo = c("ABC", "XX", NA)
    )
  )
  expect_equal(
    medicion$resultado[medicion$atributo == "telefono"], c(0, 1)
  )
  expect_equal(
    medicion$resultado[medicion$atributo == "codigo"], c(0, 1)
  )
})

test_that("Oportunidad aplica la formula continua y acota el resultado", {
  nucleo <- metricas_nucleo()
  por_fecha <- instancia_nueva(
    nucleo$OportunidadAtributoPorFecha, "entrega",
    fecha_solicitud = as.Date("2026-01-01"),
    fecha_fin_utilidad = as.Date("2026-01-11")
  )
  por_intervalo <- instancia_nueva(
    nucleo$OportunidadAtributoPorIntervalo, "entrega_2",
    inicio_vigencia = as.POSIXct("2026-01-01", tz = "UTC"),
    fin_vigencia = as.POSIXct("2026-01-11", tz = "UTC")
  )
  datos <- data.frame(
    entrega = as.Date(c(
      "2025-12-31", "2026-01-01", "2026-01-06", "2026-01-11",
      "2026-01-20", NA
    )),
    entrega_2 = as.POSIXct(c(
      "2025-12-31", "2026-01-01", "2026-01-06", "2026-01-11",
      "2026-01-20", NA
    ), tz = "UTC")
  )
  medicion <- medir(modelo(por_fecha, por_intervalo), datos)
  esperado <- c(1, 1, 0.5, 0, 0)
  expect_equal(
    medicion$resultado[medicion$atributo == "entrega"], esperado
  )
  expect_equal(
    medicion$resultado[medicion$atributo == "entrega_2"], esperado
  )

  medidas <- medicion[medicion$atributo == "entrega", , drop = FALSE]
  expect_equal(agregar(medidas, "atributo", "promedio")$resultado, 0.5)
  expect_equal(
    agregar(medidas, "atributo", "ratio_umbral", umbral = 0.5)$resultado,
    3 / 5
  )
})

test_that("Oportunidad admite referencias por fila", {
  nucleo <- metricas_nucleo()
  especifica <- especializar(
    nucleo$OportunidadAtributoPorFecha,
    fecha_solicitud = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
    fecha_fin_utilidad = as.Date(c("2026-01-11", "2026-02-21", "2026-03-31"))
  )
  instancia <- instanciar(especifica, "tabla", "entrega")
  medicion <- medir(
    modelo(instancia),
    data.frame(entrega = as.Date(c("2026-01-06", NA, "2026-03-16")))
  )
  expect_equal(medicion$resultado, c(0.5, 0.5))
})

test_that("DensidadPonderada penaliza mas el ausente critico", {
  nucleo <- metricas_nucleo()
  densidad <- instancia_nueva(
    nucleo$DensidadPonderada, c("critico", "auxiliar"),
    coeficientes = c(auxiliar = 0.2, critico = 0.8)
  )
  medicion <- medir(
    modelo(densidad),
    data.frame(
      critico = c(1, NA, 1, NA),
      auxiliar = c(1, 1, NA, NA)
    )
  )
  expect_equal(medicion$resultado, c(1, 0.2, 0.8, 0))
  expect_equal(
    agregar(medicion, "entidad", "promedio")$resultado, 0.5
  )
  expect_equal(
    agregar(
      medicion, "entidad", "ratio_umbral", umbral = 0.8
    )$resultado,
    0.5
  )

  sin_nombres <- instancia_nueva(
    nucleo$DensidadPonderada, c("critico", "auxiliar"),
    coeficientes = c(0.8, 0.2)
  )
  expect_equal(
    medir(
      modelo(sin_nombres),
      data.frame(critico = 1, auxiliar = NA_real_)
    )$resultado,
    0.8
  )
})

test_that("el catalogo de AGESIC contiene y clasifica 49 entradas", {
  catalogo <- catalogo_agesic()
  expect_s3_class(catalogo, "data.frame")
  expect_equal(nrow(catalogo), 49L)
  expect_named(catalogo, c(
    "numero", "dimension", "factor", "metrica_agesic", "clase_catalogo",
    "estado", "metrica_lupa", "implementacion", "observacion"
  ))
  expect_equal(
    as.integer(table(catalogo$estado)), c(20L, 6L, 13L, 10L)
  )
  expect_equal(
    levels(catalogo$estado),
    c(
      "implementada", "via_agregacion", "requiere_referencial",
      "fuera_de_alcance"
    )
  )
  ratio <- catalogo[catalogo$metrica_agesic == "RatioAtributoDuplicado", ]
  expect_equal(as.character(ratio$estado), "via_agregacion")
  expect_equal(ratio$implementacion, 'agregar(m, "atributo", "ratio")')
  ratio_densidad <- catalogo[
    catalogo$metrica_agesic == "RatioDensidadPonderada",
  ]
  expect_match(ratio_densidad$implementacion, "ratio_umbral", fixed = TRUE)
  oportunidad <- catalogo[grepl("^OportunidadAtributo", catalogo$metrica_agesic), ]
  expect_true(all(as.character(oportunidad$estado) == "implementada"))
  expect_true(all(grepl("real continuo", oportunidad$observacion)))
  expect_equal(length(unique(catalogo$numero)), 49L)
})

test_that("las configuraciones nuevas rechazan contratos invalidos", {
  nucleo <- metricas_nucleo()
  comprension <- nucleo$ValoresPosiblesPorComprension
  expect_error(especializar(comprension), "exige un `predicado` o un rango")
  expect_error(
    especializar(comprension, predicado = function(x) TRUE, minimo = 1),
    "no ambos"
  )
  expect_error(especializar(comprension, predicado = 1), "predicado")
  expect_error(especializar(comprension, minimo = 1), "minimo.*maximo")
  expect_error(especializar(comprension, minimo = 2, maximo = 1), "mayor")
  expect_error(
    especializar(comprension, minimo = 1, maximo = 2, inclusivo = NA),
    "inclusivo"
  )
  expect_error(
    especializar(comprension, minimo = 1, maximo = 2, otro = TRUE),
    "no acepta"
  )
  predicado_malo <- instancia_nueva(
    comprension, "x", predicado = function(x) rep(TRUE, length(x) + 1L)
  )
  expect_error(
    medir(modelo(predicado_malo), data.frame(x = 1:2)), "longitud 2"
  )

  desactualizacion <- nucleo$DesactualizacionPorFormato
  expect_error(especializar(desactualizacion), "exactamente")
  expect_error(
    especializar(
      desactualizacion, expresion_regular = "x", validador = is.na
    ),
    "exactamente"
  )
  expect_error(
    especializar(desactualizacion, expresion_regular = 1), "cadena"
  )
  expect_error(especializar(desactualizacion, validador = 1), "validador")
  validador_malo <- instancia_nueva(
    desactualizacion, "x", validador = function(x) rep(NA, length(x))
  )
  expect_error(
    medir(modelo(validador_malo), data.frame(x = "a")), "sin NA"
  )
})

test_that("los vinculos y fechas nuevas se validan", {
  nucleo <- metricas_nucleo()
  atributo <- instancia_nueva(nucleo$AtributoDuplicado, c("a", "b"))
  expect_error(
    medir(modelo(atributo), data.frame(a = 1, b = 1)), "requiere 1"
  )
  conjunto <- instancia_nueva(nucleo$ConjuntoAtributosDuplicado, "a")
  expect_error(
    medir(modelo(conjunto), data.frame(a = 1)), "al menos 2"
  )
  repetido <- instancia_nueva(
    nucleo$ConjuntoAtributosDuplicado, c("a", "a")
  )
  expect_error(
    medir(modelo(repetido), data.frame(a = 1)), "distinto"
  )
  faltante <- instancia_nueva(
    nucleo$ConjuntoAtributosDuplicado, c("a", "b")
  )
  expect_error(
    medir(modelo(faltante), data.frame(a = 1)), "No se encontraron"
  )
  entidad_con_atributo <- instancia_nueva(nucleo$EntidadDuplicada, "a")
  expect_error(
    medir(modelo(entidad_con_atributo), data.frame(a = 1)), "EntidadDuplicada"
  )

  oportunidad <- nucleo$OportunidadAtributoPorFecha
  expect_error(
    especializar(
      oportunidad, fecha_solicitud = as.Date("2026-01-01"),
      fecha_fin_utilidad = as.Date("2026-01-01")
    ),
    "intervalo"
  )
  expect_error(
    especializar(
      oportunidad, fecha_solicitud = as.Date("2026-01-02"),
      fecha_fin_utilidad = as.Date("2026-01-01")
    ),
    "invertido"
  )
  expect_error(
    especializar(
      oportunidad, fecha_solicitud = "2026-01-01",
      fecha_fin_utilidad = as.Date("2026-01-02")
    ),
    "fechas"
  )
  expect_error(
    especializar(
      oportunidad,
      fecha_solicitud = as.Date(c("2026-01-01", "2026-01-02")),
      fecha_fin_utilidad = as.Date(c(
        "2026-01-03", "2026-01-04", "2026-01-05"
      ))
    ),
    "longitudes compatibles"
  )
  numerica <- instancia_nueva(
    oportunidad, "entrega",
    fecha_solicitud = as.Date("2026-01-01"),
    fecha_fin_utilidad = as.Date("2026-01-02")
  )
  expect_error(
    medir(modelo(numerica), data.frame(entrega = 1)), "Date o POSIXt"
  )
  fechas_cortas <- instancia_nueva(
    oportunidad, "entrega",
    fecha_solicitud = as.Date(c("2026-01-01", "2026-01-02")),
    fecha_fin_utilidad = as.Date(c("2026-01-03", "2026-01-04"))
  )
  expect_error(
    medir(
      modelo(fechas_cortas),
      data.frame(entrega = as.Date(rep("2026-01-02", 3L)))
    ),
    "longitud 1"
  )
})

test_that("los coeficientes de densidad se validan", {
  nucleo <- metricas_nucleo()
  densidad <- nucleo$DensidadPonderada
  expect_error(especializar(densidad, coeficientes = c(0.4, 0.4)), "sumen 1")
  expect_error(especializar(densidad, coeficientes = c(1, NA)), "sumen 1")
  expect_error(
    especializar(densidad, coeficientes = c(a = 0.5, a = 0.5)),
    "nombres"
  )
  sin_atributos <- instancia_nueva(densidad, coeficientes = 1)
  expect_error(
    medir(modelo(sin_atributos), data.frame(a = 1)), "al menos 1"
  )
  nombres_malos <- instancia_nueva(
    densidad, c("a", "b"), coeficientes = c(a = 0.5, c = 0.5)
  )
  expect_error(
    medir(modelo(nombres_malos), data.frame(a = 1, b = 2)), "coincidir"
  )
  longitud_mala <- instancia_nueva(
    densidad, c("a", "b"), coeficientes = 1
  )
  expect_error(
    medir(modelo(longitud_mala), data.frame(a = 1, b = 2)), "un coeficiente"
  )
})
