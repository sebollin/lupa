test_that("las ramas de normalizacion declaran perfiles y casos limite", {
  expect_error(
    normalizacion(puntuacion = c(TRUE, FALSE)),
    "logicos escalares"
  )
  expect_error(normalizacion(proteger = NA_character_), "no vacio")
  expect_error(normalizacion(proteger = intToUtf8(769L)), "grafema")
  expect_output(print(normalizacion()), "Perfil de normalizacion")
  expect_error(lupa:::.normalizacion_preset("estrecho"), "debe ser amplio")
  expect_error(lupa:::.resolver_normalizacion(c(TRUE, FALSE)), "vector de")
  expect_error(
    lupa:::.resolver_normalizacion(list(a = normalizacion(), a = normalizacion())),
    "nombres de columnas unicos"
  )
  expect_error(
    lupa:::.resolver_normalizacion(list(a = 1L)),
    "perfil valido"
  )
  expect_error(lupa:::.resolver_normalizacion(1), "TRUE, FALSE")

  resuelta <- lupa:::.resolver_normalizacion(
    list(nombre = normalizacion(acentos = FALSE))
  )
  expect_true(lupa:::.normalizacion_tiene_pasos_resuelta(resuelta))
  expect_true(lupa:::.normalizacion_tiene_pasos_resuelta(resuelta, "nombre"))
  expect_identical(
    lupa:::.normalizacion_para_columna(normalizacion(), "nombre"),
    normalizacion()
  )
  expect_true(is.list(lupa:::.normalizacion_resumen(normalizacion())))

  etapas <- lupa:::.normalizacion_etapas(
    "‘Ａﬁna, Á’", normalizacion(
      ancho = TRUE, ligaduras = TRUE, puntuacion = TRUE,
      comillas = TRUE, acentos = TRUE
    )
  )
  expect_true(all(c("ancho", "ligaduras", "comillas", "puntuacion",
                    "descomposicion_canonica", "acentos") %in% names(etapas)))
  expect_identical(lupa:::.normalizacion_etapas(NA_character_, normalizacion()),
                   stats::setNames(NA_character_, "entrada"))

  expect_identical(lupa:::.normalizacion_ordenar(97L), 97L)
  expect_identical(
    lupa:::.normalizacion_protecciones(normalizacion(proteger = "ñ"))[[1L]]$base,
    110L
  )
  expect_true(length(lupa:::.normalizacion_quitar_acentos(
    utf8ToInt("á"), normalizacion()
  )) == 1L)
  expect_identical(
    lupa:::.normalizacion_ancho(utf8ToInt("Ａ　A")),
    utf8ToInt("A A")
  )
  expect_identical(
    lupa:::.normalizacion_ligaduras(utf8ToInt("ﬁx")),
    utf8ToInt("fix")
  )
  expect_identical(
    lupa:::.normalizacion_puntuacion(utf8ToInt("A, B!")),
    utf8ToInt("A B")
  )
  expect_identical(lupa:::.normalizacion_espacios_codigos(integer()), integer())
  expect_identical(lupa:::.normalizacion_regex_codigos(integer()), "(?!)")
  expect_identical(
    lupa:::.normalizacion_reemplazar_tabla(c("sin caracteres especiales", NA_character_)),
    c("sin caracteres especiales", NA_character_)
  )
  expect_identical(
    lupa:::.normalizacion_reemplazar_tabla("\u00e1"), "a\u0301"
  )
  expect_equal(
    lupa:::.normalizacion_fusiones(character(), normalizacion()),
    list(pasos = list(), n_distintos_normalizados = 0L)
  )
  expect_true(length(lupa:::.normalizacion_fusiones_tabla(
    list(c("A", "a")), lupa:::.resolver_normalizacion(normalizacion())
  )) == 1L)
})

test_that("las ramas de conversion y referenciales declaran su alcance", {
  skip_if_not_installed("stringdist")
  ahora <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  expect_match(
    lupa:::.texto_representacion_conversion(ahora),
    "UTC"
  )
  config <- lupa:::.validar_config_referencial(list())
  expect_error(lupa:::.validar_config_referencial(list(extra = TRUE)), "no aceptan")
  expect_error(lupa:::.validar_config_referencial(list(normalizar = 1)), "perfil valido")
  expect_error(lupa:::.validar_config_referencial(list(proximidad = NA)), "logico")
  expect_error(lupa:::.validar_config_referencial(list(metodo = "x")), "medida")
  expect_error(lupa:::.validar_config_referencial(list(p = 1)), "0.25")
  expect_error(lupa:::.validar_config_referencial(list(umbral = -1)), "no negativo")
  expect_error(lupa:::.validar_config_referencial(list(max_pares = 0)), "entero positivo")
  expect_error(
    referencial(data.frame(codigo = "A"), clave = "codigo", normalizar = 1),
    "no describe un perfil"
  )

  sin_fallos <- lupa:::.referencial_proximidad(
    integer(), character(), character(), data.frame(), config
  )
  expect_true(sin_fallos$alcance$disponible)
  expect_match(sin_fallos$alcance$motivo, "No hubo fallos")
  apagada <- lupa:::.referencial_proximidad(
    1L, "x", "x", data.frame(x = "x"), modifyList(config, list(proximidad = FALSE))
  )
  expect_match(apagada$alcance$motivo, "desactivada")
  limite <- lupa:::.referencial_proximidad(
    1L, "x", c("a", "b"), data.frame(x = c("a", "b")),
    modifyList(config, list(max_pares = 1L))
  )
  expect_true(limite$alcance$truncado)
  expect_match(limite$alcance$motivo, "limite de pares")
  una <- lupa:::.referencial_proximidad(
    1L, "x", "a", data.frame(x = "a"), config
  )
  expect_equal(una$alcance$n_pares_comparados, 1)
  expect_identical(
    lupa:::.referencial_filas_texto(data.frame(), "x", lupa:::.resolver_normalizacion(TRUE)),
    character()
  )
  expect_identical(lupa:::.referencial_filas_original_texto(data.frame(), "x"), character())
  expect_identical(
    lupa:::.referencial_normalizacion(
      list(configuracion = list(normalizar = NULL)),
      list(normalizar = NULL)
    )$general,
    normalizacion()
  )
  testthat::local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  expect_match(
    lupa:::.referencial_proximidad(1L, "x", "x", data.frame(x = "x"), config)$alcance$motivo,
    "stringdist"
  )
})

test_that("las ramas de claves y utilidades conservan declaraciones vacias", {
  perfil <- lupa:::.resolver_normalizacion(TRUE)
  expect_equal(
    lupa:::.resumen_clave_normalizada(data.frame(), integer(), character(), perfil),
    list(unicidad = NA, distintos = NA_integer_)
  )
  expect_equal(
    lupa:::.resumen_clave_normalizada(
      data.frame(x = NA_character_), 1L, "x", perfil
    ),
    list(unicidad = FALSE, distintos = 0L)
  )
  expect_equal(
    nrow(detectar_claves(data.frame(a = c(1, 1), b = c("x", "x")))),
    0L
  )
  perfil_dos <- perfilar(data.frame(x = 1:2), analizar_dependencias = FALSE)
  expect_error(detectar_claves(data.frame(y = 1:2), perfil = perfil_dos), "corresponder")
  expect_equal(nrow(detectar_relaciones(data.frame(), data.frame())), 0L)

  expect_identical(lupa:::.tipo_declarado(structure(raw(1), class = "clase_rara")), "clase_rara")
  expect_true(is.na(lupa:::.texto_valor(integer())))
  expect_identical(lupa:::.normalizar_columnas_texto(1:2), 1:2)
  expect_identical(lupa:::.normalizar_columnas_texto(data.frame(x = 1:2)), data.frame(x = 1:2))
})

test_that("los decodificadores declaran entradas imposibles y no reparables", {
  expect_null(lupa:::.ftfy_desde_utf8(NULL))
  expect_null(lupa:::.ftfy_desde_utf8(as.raw(0L)))
  expect_identical(lupa:::.ftfy_desde_utf8_variantes(raw()), integer())
  expect_null(lupa:::.ftfy_desde_utf8_variantes(as.raw(0xff)))
  expect_identical(lupa:::.ftfy_decode_inconsistent_utf8("texto sano"), "texto sano")
  expect_null(lupa:::.ftfy_fallback_latin1_windows1252("texto sano"))
  expect_equal(lupa:::.ftfy_reparar_uno(character()), list(
    texto = NA_character_, pasos = character(), estado = "sin_texto"
  ))
  expect_equal(lupa:::.ftfy_reparar_uno("texto sano")$estado, "no_parece_roto")
  expect_equal(lupa:::.reparar_mojibake_uno("texto sano"), NA_character_)
  expect_equal(lupa:::.ftfy_estado_agregado(character()), "no_parece_roto")
})
