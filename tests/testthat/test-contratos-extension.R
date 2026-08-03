metodo_origen_documentado <- function(tablas, instancia) {
  expect_type(tablas, "list")
  expect_true(all(vapply(tablas, inherits, logical(1L), "data.frame")))
  expect_true(all(c(
    "entidad", "atributos", "configuracion", "referencial", "declaracion"
  ) %in% names(instancia)))
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  data.frame(
    resultado = !is.na(x) & nzchar(x),
    entidad = instancia$entidad,
    atributo = instancia$atributos,
    fila = seq_along(x),
    objeto = paste0(
      instancia$entidad, "$", instancia$atributos, "[", seq_along(x), "]"
    ),
    columna_extra = "se descarta",
    stringsAsFactors = FALSE
  )
}

test_that("el contrato documentado de metodo recorre las dos fabricas", {
  origen_declarado <- metrica(
    nombre = "OrigenDeclarado",
    semantica = "Indica si cada registro declara su sistema de origen.",
    granularidad = "instanciaAtributo",
    tipo_resultado = "booleano",
    dimension = "Trazabilidad",
    factor = "Origen documentado",
    metodo = metodo_origen_documentado
  )
  origen <- origen_declarado()(
    entidad = "entrega", atributos = "origen"
  )
  datos <- data.frame(origen = c("sistema_a", "", "sistema_b"))
  resultado <- medir(modelo(origen), datos, id_medicion = "contrato")

  expect_s3_class(origen_declarado, "metrica_generica")
  expect_s3_class(origen, "metrica_instanciada")
  expect_equal(resultado$resultado, c(1, 0, 1))
  expect_equal(resultado$fila, 1:3)
  expect_false("columna_extra" %in% names(resultado))
  expect_error(
    origen_declarado(entidad = "entrega", atributos = "origen"),
    "especializarse primero"
  )
})

test_that("las propiedades se consultan y el validador respeta la declaracion", {
  validar <- function(configuracion) {
    if (!"umbral" %in% names(configuracion)) {
      stop("Falta umbral.", call. = FALSE)
    }
    list(
      umbral = as.numeric(configuracion$umbral),
      inclusivo = if ("inclusivo" %in% names(configuracion)) {
        isTRUE(configuracion$inclusivo)
      } else TRUE
    )
  }
  generica <- metrica(
    "SuperaUmbral", "Compara con un umbral.",
    "instanciaAtributo", "booleano",
    propiedades = c("umbral", "inclusivo"),
    metodo = metodo_origen_documentado,
    validar_propiedades = validar
  )
  especifica <- especializar(generica, umbral = 5)
  instancia <- especifica("entrega", "valor")

  expect_equal(
    propiedades_metrica(generica),
    data.frame(
      propiedad = c("umbral", "inclusivo"), configurada = c(FALSE, FALSE)
    )
  )
  expect_true(all(propiedades_metrica(especifica)$configurada))
  expect_true(all(propiedades_metrica(instancia)$configurada))
  expect_identical(instancia$configuracion, list(umbral = 5, inclusivo = TRUE))
  expect_error(especializar(generica), "Falta umbral")
  expect_error(propiedades_metrica(list()), "métrica genérica")

  sin_validador <- metrica(
    "Acotada", "Usa dos límites.", "atributo", "real",
    propiedades = c("minimo", "maximo"),
    metodo = metodo_origen_documentado
  )
  expect_error(especializar(sin_validador, minimo = 0), "Faltan propiedades")
  expect_error(
    especializar(sin_validador, minimo = 0, maximo = 1, extra = 2),
    "no declaradas"
  )
})

test_that("un validador no puede introducir propiedades ajenas o anonimas", {
  fabrica <- function(validador) metrica(
    "Configurable", "Prueba un contrato.", "atributo", "real",
    propiedades = "umbral", metodo = metodo_origen_documentado,
    validar_propiedades = validador
  )
  expect_error(
    especializar(fabrica(function(x) list(otra = 1)), umbral = 1),
    "no declaradas"
  )
  expect_error(
    especializar(fabrica(function(x) list(1)), umbral = 1),
    "nombres únicos"
  )
  duplicadas <- function(x) {
    resultado <- list(1, 2)
    names(resultado) <- c("umbral", "umbral")
    resultado
  }
  expect_error(
    especializar(fabrica(duplicadas), umbral = 1), "nombres únicos"
  )
  vacia <- especializar(fabrica(function(x) list()), umbral = 1)
  expect_false(propiedades_metrica(vacia)$configurada)
})

test_that("instanciar conserva el referencial y permite reemplazar el metodo", {
  base <- function(tablas, instancia) data.frame(
    resultado = FALSE, entidad = instancia$entidad,
    atributo = instancia$atributos, fila = 1L, objeto = "entrega$x[1]"
  )
  reemplazo <- function(tablas, instancia) data.frame(
    resultado = TRUE, entidad = instancia$entidad,
    atributo = instancia$atributos, fila = 1L, objeto = "entrega$x[1]"
  )
  generica <- metrica(
    "Comprobacion", "Comprueba un valor.",
    "instanciaAtributo", "booleano", metodo = base
  )
  padron <- referencial(
    data.frame(codigo = "A"), clave = "codigo", nombre = "Códigos"
  )
  predeterminada <- generica()("entrega", "x")
  reemplazada <- instanciar(
    generica(), "entrega", "x", nombre_instancia = "reemplazada",
    metodo = reemplazo, referencial = padron
  )
  datos <- data.frame(x = 1)

  expect_identical(reemplazada$referencial, padron)
  expect_equal(medir(modelo(predeterminada), datos)$resultado, 0)
  expect_equal(medir(modelo(reemplazada), datos)$resultado, 1)
})

test_that("marco_calidad documenta los defaults y conserva metadatos", {
  abreviado <- marco_calidad("Marco abreviado", list(
    Estructura = c("Presencia", "Duplicación")
  ))
  tabla <- as.data.frame(abreviado)
  expect_false(any(tabla$perfil_mide))
  expect_true(all(tabla$aplicabilidad == "siempre"))
  expect_true(all(tabla$disponibilidad == "disponible"))
  expect_true(all(nzchar(tabla$como_resolverlo)))

  detallado <- marco_calidad("Marco detallado", data.frame(
    dimension = "Estructura", factor = "Presencia",
    perfil_mide = TRUE, aplicabilidad = "siempre",
    disponibilidad = "disponible",
    como_resolverlo = "Examinar valores ausentes.",
    responsable = "equipo de datos"
  ))
  perfil <- perfilar(data.frame(x = c(1, NA)), analizar_dependencias = FALSE)
  cobertura <- cobertura_analisis(perfil, modelo = detallado)

  expect_identical(as.data.frame(detallado)$responsable, "equipo de datos")
  expect_identical(unique(cobertura$marco), "Marco detallado")
  expect_identical(as.character(cobertura$estado), "medida")
})

test_that("referencial hace explícitos clave valor completitud y alcance", {
  parcial <- referencial(
    data.frame(codigo = c("A", "B"), etiqueta = c("Uno", "Dos")),
    clave = "codigo", valor = "etiqueta"
  )
  expect_false(parcial$completo)
  expect_true(is.na(parcial$alcance))
  expect_equal(parcial$clave, "codigo")
  expect_equal(parcial$valor, "etiqueta")
  expect_error(
    referencial(data.frame(codigo = "A"), "codigo", completo = TRUE),
    "alcance"
  )
  expect_error(
    referencial(data.frame(codigo = c("A", "A")), "codigo"),
    "unívocamente"
  )
})

test_that("regla_evaluacion consume resultados en orden y valida su salida", {
  origen_declarado <- metrica(
    "OrigenDeclarado", "Comprueba procedencia.",
    "instanciaAtributo", "booleano", metodo = metodo_origen_documentado
  )
  instancia <- origen_declarado()("entrega", "origen")
  medidas <- medir(
    modelo(instancia), data.frame(origen = c("A", "", "B")),
    id_medicion = "reglas"
  )
  llamadas <- 0L
  condicion <- function(x) {
    llamadas <<- llamadas + 1L
    expect_identical(x, medidas$resultado)
    x == 1
  }
  regla <- regla_evaluacion(
    "Origen presente", condicion,
    metricas = unique(medidas$metrica_instanciada)
  )
  evaluacion <- evaluar(medidas, perfil_evaluacion("Control", regla))

  expect_equal(llamadas, 1L)
  expect_equal(evaluacion$reglas$resultado, 2 / 3)
  expect_null(regla_evaluacion("Todas", function(x) x > 0)$metricas)
  expect_error(
    evaluar(
      medidas,
      perfil_evaluacion(
        "Mala", regla_evaluacion("Longitud", function(x) TRUE)
      )
    ),
    "uno por medida"
  )
  expect_error(
    evaluar(
      medidas,
      perfil_evaluacion(
        "Mala", regla_evaluacion("Ausente", function(x) rep(NA, length(x)))
      )
    ),
    "sin NA"
  )
})

test_that("marco_iso25012 contiene las quince características y su mapeo", {
  iso <- marco_iso25012()
  tabla <- as.data.frame(iso)
  factores <- c(
    "Exactitud", "Completitud", "Consistencia", "Credibilidad", "Actualidad",
    "Accesibilidad", "Conformidad", "Confidencialidad", "Eficiencia",
    "Precisión", "Trazabilidad", "Comprensibilidad",
    "Disponibilidad", "Portabilidad", "Recuperabilidad"
  )
  conteos <- table(tabla$dimension)

  expect_s3_class(iso, "marco_calidad")
  expect_equal(nrow(tabla), 15L)
  expect_setequal(tabla$factor, factores)
  expect_equal(
    as.integer(conteos[c(
      "Inherente", "Inherente y dependiente del sistema",
      "Dependiente del sistema"
    )]),
    c(5L, 7L, 3L)
  )
  expect_false(any(tabla$perfil_mide))
  expect_true(all(nzchar(tabla$descripcion)))
  expect_true(all(nzchar(tabla$como_resolverlo)))
  expect_identical(iso$origen, "ISO/IEC 25012:2008")

  perfil <- perfilar(data.frame(x = 1:3), analizar_dependencias = FALSE)
  cobertura <- cobertura_analisis(perfil, modelo = iso)
  expect_equal(unique(cobertura$marco), "Marco ISO/IEC 25012:2008")
  expect_false(any(grepl("AGESIC", cobertura$marco, fixed = TRUE)))
})
