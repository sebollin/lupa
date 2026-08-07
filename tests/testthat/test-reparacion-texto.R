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

test_that("las tablas congeladas tienen el contrato de ftfy", {
  skip_if_not_installed("jsonlite")
  esperadas <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-tablas-6.3.1.json"),
    simplifyVector = FALSE
  )
  expect_identical(names(.ftfy_tablas_bytes), names(esperadas))
  for (nombre in names(esperadas)) {
    expect_identical(.ftfy_tablas_bytes[[nombre]], as.integer(esperadas[[nombre]]))
  }
  expect_length(.ftfy_tablas_bytes, 10L)
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
