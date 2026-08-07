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
  expect_gte(sum(reparados == esperados), 66L)
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
  expect_equal(.ftfy_replace_lossy_sequences("Niño")$estado, "sin_perdida")
  expect_equal(.ftfy_replace_lossy_sequences("Ni\ufffd")$estado, "no_se_pudo")
})

test_that("las tablas congeladas tienen el contrato de ftfy", {
  expect_length(.ftfy_tablas_bytes, 10L)
  expect_true(all(vapply(.ftfy_tablas_bytes, length, integer(1L)) == 128L))
  expect_identical(.ftfy_tablas_bytes[["latin-1"]], 128:255)
  # Valores distintivos de las tablas de ftfy; no se calculan al cargar lupa.
  expect_identical(.ftfy_tablas_bytes[["sloppy-windows-1252"]][1:4],
                   c(8364L, 129L, 8218L, 402L))
  expect_identical(.ftfy_tablas_bytes[["macroman"]][1:4],
                   c(196L, 197L, 199L, 201L))
  expect_identical(.ftfy_tablas_bytes[["cp437"]][1:4],
                   c(199L, 252L, 233L, 226L))
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
