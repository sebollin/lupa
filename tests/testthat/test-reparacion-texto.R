test_that("el motor reproduce la batería de ftfy y conserva texto legítimo", {
  skip_if_not_installed("jsonlite")
  mojibake <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "casos-mojibake.json"),
    simplifyVector = FALSE
  )
  legitimos <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "casos-legitimos.json"),
    simplifyVector = FALSE
  )
  reparados <- vapply(mojibake, function(caso) {
    .ftfy_reparar_uno(caso$roto)$texto
  }, character(1L))
  esperados <- vapply(mojibake, function(caso) caso$ftfy_fix_encoding, character(1L))
  expect_equal(sum(reparados == esperados), 72L)
  usa_perdida <- vapply(mojibake, function(caso) {
    any(grepl("replace_lossy_sequences", .ftfy_reparar_uno(caso$roto)$pasos,
              fixed = TRUE))
  }, logical(1L))
  expect_equal(sum(usa_perdida), 6L)
  estados_perdida <- vapply(mojibake[usa_perdida], function(caso) {
    .ftfy_reparar_uno(caso$roto)$estado
  }, character(1L))
  expect_true(all(estados_perdida == "reparado_parcialmente"))
  intactos <- vapply(legitimos, function(caso) {
    identical(.ftfy_reparar_uno(caso$texto)$texto, caso$ftfy)
  }, logical(1L))
  expect_true(all(intactos))
})

test_that("los transcodificadores de texto cubren casos parciales y C1", {
  expect_equal(.ftfy_reparar_uno("voilÃ le travail")$texto, "voilà le travail")
  expect_equal(.ftfy_reparar_uno("MontevideoÂ 11000")$texto, "Montevideo 11000")
  expect_equal(.ftfy_reparar_uno("El barrio de Ã‘uÃ±oa")$texto,
               "El barrio de Ñuñoa")
  expect_equal(.ftfy_reparar_uno("Costo\u009339.000")$texto,
               "Costo“39.000")
  expect_identical(
    as.integer(.ftfy_replace_lossy_sequences(as.raw(c(0xc3, 0x1a)))),
    c(0xefL, 0xbfL, 0xbdL)
  )
})

test_that("las tablas congeladas incluyen ftfy y la extension koi8-r", {
  skip_if_not_installed("jsonlite")
  esperadas <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "tablas-codificacion.json"),
    simplifyVector = FALSE
  )
  expect_identical(names(.ftfy_tablas_bytes), names(esperadas))
  for (nombre in names(esperadas)) {
    expect_identical(.ftfy_tablas_bytes[[nombre]], as.integer(esperadas[[nombre]]))
  }
  expect_length(.ftfy_tablas_bytes, 11L)
  expect_true(all(vapply(.ftfy_tablas_bytes, length, integer(1L)) == 128L))
})

test_that("las categorias de badness son las de ftfy 6.3.1", {
  skip_if_not_installed("jsonlite")
  esperadas <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-categorias-6.3.1.json"),
    simplifyVector = FALSE
  )
  expect_identical(.ftfy_categorias, esperadas)
})

test_that("la expresion completa de badness es la de ftfy 6.3.1", {
  skip_if_not_installed("jsonlite")
  esperado <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-badness-6.3.1.json")
  )
  normalizar <- function(p) gsub("\\s+|#[^\\n]*", "", p, perl = TRUE)
  base <- substring(.ftfy_badness_re_ftfy, nchar("(*UTF)(*UCP)") + 1L)
  expect_identical(normalizar(base),
                   normalizar(esperado$ftfy_pattern))
  # Las alternativas que se pierden con reglas incompletas son un caso real:
  # el mojibake de cp437 contiene caracteres de caja.
  expect_true(.ftfy_es_mojibake(
    "Instituto Nacional de Estad├¡stica ΓÇö Montevideo, Uruguay"
  ))
  expect_false(.ftfy_es_mojibake("São Paulo"))
})

test_that("las extensiones deliberadas de badness cubren los cuatro arreglos", {
  expect_true(.ftfy_es_mojibake("Ã¥klagarmyndighets"))
  expect_true(.ftfy_es_mojibake("п∙я│п╩п╦ п▓я▀ п╫п╣"))
  expect_true(.ftfy_es_mojibake("â…“"))
  expect_false(.ftfy_es_mojibake("Charlotte Brontë…\u201d"))
  expect_identical(.ftfy_reparar_uno("Ã¥klagarmyndighets")$texto,
                   "åklagarmyndighets")
  expect_identical(.ftfy_reparar_uno("п∙я│п╩п╦ п▓я▀ п╫п╣")$texto,
                   "Если Вы не")
  expect_identical(.ftfy_reparar_uno("â…“")$texto, "⅓")
  expect_identical(.ftfy_reparar_uno("â…›")$texto, "⅛")
  expect_identical(.ftfy_reparar_uno("Charlotte Brontë…\u201d")$texto,
                   "Charlotte Brontë…\u201d")
})

test_that("la extension cp437 exige contexto latino y no toca cajas", {
  expect_identical(.ftfy_reparar_uno("c├│digo postal")$texto,
                   "código postal")
  expect_identical(.ftfy_reparar_uno("├¡ndice")$texto, "índice")
  expect_identical(.ftfy_reparar_uno("┬┐que diferencia hay?")$texto,
                   "¿que diferencia hay?")
  for (texto in c(
    "Total ═╗ 100", "Depto ═╣ Poblacion", "Subtotal ═╝",
    "╔══════════════╗"
  )) {
    expect_identical(.ftfy_reparar_uno(texto)$texto, texto)
  }

  cajas <- unique(c(
    strsplit(.ftfy_categorias$box, "", fixed = TRUE)[[1L]],
    intToUtf8(utf8ToInt("═"):utf8ToInt("╬"), multiple = TRUE)
  ))
  cajas <- setdiff(cajas, "-")
  pares <- unname(as.vector(outer(cajas, cajas, paste0)))
  salidas <- vapply(pares, function(x) .ftfy_reparar_uno(x)$texto,
                    character(1L), USE.NAMES = FALSE)
  expect_length(pares, 2025L)
  expect_identical(salidas, pares)
})

test_that("decode_inconsistent_utf8 usa el detector de ftfy por subcadena", {
  skip_if_not_installed("jsonlite")
  fixture <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-utf8-detector-6.3.1.json"),
    simplifyVector = FALSE
  )
  normalizar <- function(x) gsub("\\s+|#[^\\n]*", "", x, perl = TRUE)
  expect_identical(normalizar(.ftfy_utf8_detector_re),
                   normalizar(fixture$ftfy_pattern))
  casos <- fixture$cases
  salidas <- vapply(casos, function(caso) .ftfy_reparar_uno(caso$input)$texto,
                    character(1L))
  expect_identical(salidas, vapply(casos, `[[`, character(1L), "expected"))
})

test_that("replace_lossy_sequences coincide con el fixture de ftfy", {
  skip_if_not_installed("jsonlite")
  fixture <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-lossy-6.3.1.json"),
    simplifyVector = FALSE
  )
  for (caso in fixture$cases) {
    salida <- .ftfy_replace_lossy_sequences(as.raw(as.integer(caso$bytes)))
    expect_identical(as.integer(salida), as.integer(caso$expected))
  }
})

test_that("la reparación no introduce controles invisibles", {
  skip_if_not_installed("jsonlite")
  mojibake <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "casos-mojibake.json"),
    simplifyVector = FALSE
  )
  legitimos <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "casos-legitimos.json"),
    simplifyVector = FALSE
  )
  entradas <- c(
    vapply(mojibake, `[[`, character(1L), "roto"),
    vapply(legitimos, `[[`, character(1L), "texto"),
    "DirecciÃ³n\ufffd"
  )
  salidas <- vapply(entradas, function(x) .ftfy_reparar_uno(x)$texto,
                    character(1L))
  control <- function(x) {
    cp <- utf8ToInt(x)
    cp[cp <= 8L | cp %in% c(11L, 12L) | (cp >= 14L & cp <= 31L)]
  }
  expect_true(all(vapply(seq_along(entradas), function(i) {
    antes <- control(entradas[[i]])
    despues <- control(salidas[[i]])
    if (!length(despues)) return(TRUE)
    all(tabulate(despues, nbins = 31L) <= tabulate(antes, nbins = 31L))
  }, logical(1L))))
  reparado <- .ftfy_reparar_uno("DirecciÃ³n\ufffd")
  expect_identical(reparado$texto, "Dirección\ufffd")
  expect_identical(reparado$estado, "reparado_parcialmente")
})

test_that("la reparación deduplica valores y conserva estados explícitos", {
  textos <- c("PaysandÃº", "PaysandÃº", "texto normal", "\ufffd")
  resultado <- .analizar_codificacion(textos)
  expect_equal(resultado$n_reparables, 2L)
  expect_equal(resultado$n_irreparables, 1L)
  expect_equal(resultado$estados[[1L]], "reparado")
  expect_equal(resultado$estados[[4L]], "no_se_pudo")
  expect_equal(.ftfy_reparar_uno(NA_character_)$estado, "sin_texto")
  expect_equal(.ftfy_reparar_uno("")$estado, "sin_texto")
  expect_equal(.ftfy_reparar_uno(factor("PaysandÃº"))$estado, "reparado")
  expect_equal(.ftfy_reparar_uno(42)$estado, "no_parece_roto")
})

test_that("la pérdida previa conserva reparación parcial sin habilitar aplicación", {
  datos <- data.frame(nombre = "DirecciÃ³n\ufffd",
                      stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  indice <- which(perfil$hallazgos$tipo_hallazgo == "codificacion_rota")
  expect_equal(as.character(perfil$hallazgos$estado_reparacion[[indice]]),
               "reparado_parcialmente")
  plan <- planificar_limpieza(perfil)
  accion <- plan[plan$hallazgo == "codificacion_rota", , drop = FALSE]
  expect_equal(as.character(accion$estado_reparacion[[1L]]),
               "reparado_parcialmente")
  expect_false(isTRUE(accion$aplicar[[1L]]))
  salida <- aplicar(plan, datos)
  expect_identical(salida$datos$nombre[[1L]], datos$nombre[[1L]])
})

test_that("el estado de reparación acompaña hallazgo, plan y registro", {
  datos <- data.frame(nombre = c("PaysandÃº", "PaysandÃº"),
                      stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  indice <- which(perfil$hallazgos$tipo_hallazgo == "codificacion_rota")
  expect_true(length(indice) == 1L)
  expect_equal(as.character(perfil$hallazgos$estado_reparacion[[indice]]), "reparado")
  plan <- planificar_limpieza(perfil)
  accion <- plan[plan$hallazgo == "codificacion_rota", , drop = FALSE]
  expect_true(nrow(accion) == 1L)
  expect_equal(as.character(accion$estado_reparacion[[1L]]), "reparado")
  salida <- aplicar(plan, datos)
  registro <- salida$registro[salida$registro$hallazgo == "codificacion_rota", , drop = FALSE]
  expect_equal(as.character(registro$estado_reparacion[[1L]]), "reparado")
  expect_equal(salida$datos$nombre[[1L]], "Paysandú")
})

test_that("una accion no lista identifica su fila en el error", {
  datos <- data.frame(x = "texto \ufffd", stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos))
  plan$aplicar[[1L]] <- TRUE
  expect_error(
    aplicar(plan, datos),
    "fila 1.*columna 'x'.*estrategia.*estado 'informativa'"
  )
})
