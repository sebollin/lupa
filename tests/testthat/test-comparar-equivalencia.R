.frame_campo_equivalencia <- function(campo, valor) {
  resultado <- data.frame(columna = "c", stringsAsFactors = FALSE)
  resultado[[campo]] <- valor
  resultado
}

.comparar_caso_equivalencia <- function(caso) {
  comparar_equivalencia(
    .frame_campo_equivalencia(caso$campo, caso$anterior),
    .frame_campo_equivalencia(caso$campo, caso$actual),
    tolerancia = caso$tolerancia
  )
}

test_that("comparar_equivalencia cumple los 14 casos de la especificacion", {
  casos <- list(
    list(campo = "media", anterior = Inf, actual = -Inf, tolerancia = 1e-14,
         veredicto = "materialmente_distinto", motivo = "infinito",
         eje = "flotante"),
    list(campo = "media", anterior = Inf, actual = 1e300, tolerancia = 1e-14,
         veredicto = "materialmente_distinto", motivo = "infinito",
         eje = "flotante"),
    list(campo = "desvio", anterior = -Inf, actual = -1e308,
         tolerancia = 1e-14, veredicto = "materialmente_distinto",
         motivo = "infinito", eje = "flotante"),
    list(campo = "media", anterior = Inf, actual = Inf, tolerancia = 1e-14,
         veredicto = "identico", motivo = "igualdad_exacta", eje = "flotante"),
    list(campo = "media", anterior = NA_real_, actual = NaN,
         tolerancia = 1e-14, veredicto = "equivalente",
         motivo = "faltante_misma_clase", eje = "flotante"),
    list(campo = "media", anterior = NaN, actual = NaN, tolerancia = 1e-14,
         veredicto = "identico", motivo = "igualdad_exacta", eje = "flotante"),
    list(campo = "media", anterior = NA_real_, actual = NA_real_,
         tolerancia = 1e-14, veredicto = "identico", motivo = "igualdad_exacta",
         eje = "flotante"),
    list(campo = "media", anterior = NA_real_, actual = 0,
         tolerancia = 1e-14, veredicto = "materialmente_distinto",
         motivo = "faltante_un_lado", eje = "flotante"),
    list(campo = "media", anterior = 3.8e-10, actual = 0,
         tolerancia = 1e-14, veredicto = "materialmente_distinto",
         motivo = "fuera_de_tolerancia", eje = "flotante"),
    list(campo = "media", anterior = 3.8e-10, actual = 0,
         tolerancia = 1e-8, veredicto = "equivalente",
         motivo = "dentro_de_tolerancia", eje = "flotante"),
    list(campo = "minimo", anterior = 1, actual = 2, tolerancia = 2,
         veredicto = "materialmente_distinto", motivo = "eje_exacto",
         eje = "exacto"),
    list(campo = "media", anterior = 2.5, actual = 2.5, tolerancia = 1e-14,
         veredicto = "identico", motivo = "igualdad_exacta", eje = "flotante"),
    list(campo = "moda", anterior = "a", actual = "b", tolerancia = 1e-14,
         veredicto = "materialmente_distinto", motivo = "eje_valor",
         eje = "valor"),
    list(campo = "minimo_fecha", anterior = "2020-01-02",
         actual = "2020-01-03", tolerancia = 1e-14,
         veredicto = "materialmente_distinto", motivo = "eje_fecha",
         eje = "fecha")
  )

  for (i in seq_along(casos)) {
    resultado <- .comparar_caso_equivalencia(casos[[i]])
    expect_equal(as.character(resultado$veredicto[[1L]]), casos[[i]]$veredicto,
                 info = paste("caso", i))
    expect_identical(resultado$motivo[[1L]], casos[[i]]$motivo,
                     info = paste("caso", i))
    expect_identical(resultado$tipo_eje[[1L]], casos[[i]]$eje,
                     info = paste("caso", i))
    expect_equal(resultado$tolerancia[[1L]], casos[[i]]$tolerancia,
                 info = paste("caso", i))
  }
})

test_that("comparar_equivalencia cubre clases y valores limite", {
  nan_a <- readBin(
    as.raw(c(0x01, 0, 0, 0, 0, 0, 0xf8, 0x7f)),
    "double", n = 1L, size = 8L, endian = "little"
  )
  nan_b <- readBin(
    as.raw(c(0x02, 0, 0, 0, 0, 0, 0xf8, 0x7f)),
    "double", n = 1L, size = 8L, endian = "little"
  )
  casos <- list(
    list(campo = "media", anterior = -0, actual = 0, tolerancia = 0,
         veredicto = "identico", motivo = "igualdad_exacta"),
    list(campo = "n", anterior = 1L, actual = 1.0, tolerancia = 0,
         veredicto = "identico", motivo = "igualdad_exacta"),
    list(campo = "n", anterior = NA_integer_, actual = NA_real_, tolerancia = 0,
         veredicto = "equivalente", motivo = "faltante_misma_clase"),
    list(campo = "media", anterior = nan_a, actual = nan_b, tolerancia = 0,
         veredicto = "identico", motivo = "igualdad_exacta"),
    list(campo = "media", anterior = NaN, actual = Inf, tolerancia = 0,
         veredicto = "materialmente_distinto", motivo = "faltante_un_lado"),
    list(campo = "moda", anterior = NA_character_, actual = NA_character_,
         tolerancia = 0, veredicto = "identico", motivo = "igualdad_exacta"),
    list(campo = "moda", anterior = NA_character_, actual = "a", tolerancia = 0,
         veredicto = "materialmente_distinto", motivo = "faltante_un_lado"),
    list(campo = "minimo_fecha", anterior = NA_character_,
         actual = NA_character_, tolerancia = 0, veredicto = "identico",
         motivo = "igualdad_exacta"),
    list(campo = "minimo_fecha", anterior = NA_character_,
         actual = "2020-01-01", tolerancia = 0,
         veredicto = "materialmente_distinto", motivo = "faltante_un_lado")
  )
  for (i in seq_along(casos)) {
    resultado <- .comparar_caso_equivalencia(casos[[i]])
    expect_equal(as.character(resultado$veredicto[[1L]]), casos[[i]]$veredicto,
                 info = paste("limite", i))
    expect_identical(resultado$motivo[[1L]], casos[[i]]$motivo,
                     info = paste("limite", i))
  }
})

test_that("la equivalencia usa la interseccion y declara campos no comparables", {
  anterior <- data.frame(
    # F-3: este fixture prueba la interseccion, no un cambio de esquema
    # temporal; una fecha contra un numero ya no compara sus magnitudes.
    columna = "c", media = 1, mediana = 2, tipo_inferido = "numero",
    campo_nuevo = 10, stringsAsFactors = FALSE
  )
  actual <- data.frame(
    columna = "c", media = 1, tipo_inferido = "texto",
    campo_nuevo = 20, stringsAsFactors = FALSE
  )
  resultado <- comparar_equivalencia(anterior, actual, tolerancia = 0)

  expect_equal(resultado$campo, "media")
  expect_setequal(
    attr(resultado, "campos_no_comparables"),
    c("tipo_inferido", "campo_nuevo")
  )
  expect_identical(levels(resultado$veredicto), c(
    "identico", "equivalente", "materialmente_distinto"
  ))
  expect_true(is.ordered(resultado$veredicto))
  expect_identical(attr(resultado, "resumen"), c(
    identico = 1L, equivalente = 0L, materialmente_distinto = 0L
  ))
})

test_that("comparar_equivalencia acepta perfil, perfil_dbi y frame columnas", {
  perfil <- perfilar(
    data.frame(x = c(1, 2, 3)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  resultado_memoria <- comparar_equivalencia(perfil, perfil, tolerancia = 0)
  expect_s3_class(resultado_memoria, "equivalencia_perfiles")
  expect_gt(nrow(resultado_memoria), 0L)

  perfil_dbi <- list(resumen_tabla = list(columnas = perfil$columnas))
  class(perfil_dbi) <- "perfil_dbi"
  resultado_dbi <- comparar_equivalencia(
    perfil_dbi, perfil_dbi, tolerancia = 0
  )
  expect_equal(nrow(resultado_dbi), nrow(resultado_memoria))
  expect_equal(
    resultado_dbi[, c("columna", "campo", "veredicto", "motivo", "tipo_eje")],
    resultado_memoria[, c("columna", "campo", "veredicto", "motivo", "tipo_eje")]
  )
})

test_that("el registro incluye el conteo valido publicado por DBI", {
  anterior <- data.frame(columna = "c", n_validos = 3, stringsAsFactors = FALSE)
  actual <- data.frame(columna = "c", n_validos = 4, stringsAsFactors = FALSE)
  resultado <- comparar_equivalencia(anterior, actual, tolerancia = 100)

  expect_identical(resultado$campo[[1L]], "n_validos")
  expect_identical(as.character(resultado$veredicto[[1L]]),
                   "materialmente_distinto")
  expect_identical(resultado$motivo[[1L]], "eje_exacto")
})

test_that("la tolerancia es obligatoria y valida", {
  frame <- .frame_campo_equivalencia("media", 1)
  expect_error(comparar_equivalencia(frame, frame), "tolerancia")
  for (valor in list(NA_real_, NaN, Inf, -1, c(0, 1), "0")) {
    expect_error(
      comparar_equivalencia(frame, frame, tolerancia = valor), "tolerancia"
    )
  }
})
