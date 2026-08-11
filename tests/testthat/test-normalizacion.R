test_that("el perfil de normalización compara sin tocar los datos", {
  perfil <- normalizacion()
  expect_s3_class(perfil, "normalizacion_lupa")
  expect_identical(
    lupa:::.normalizacion_aplicar(c("JOSÉ PÉREZ", "jose perez"), perfil),
    c("jose perez", "jose perez")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(c("Pena", "Peña"), perfil),
    c("pena", "peña")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(c("‘Ana’ Silva", "Ana Silva", "D’Angelo"), perfil),
    c("ana silva", "ana silva", "d'angelo")
  )
})

test_that("la descomposición canónica iguala NFC y NFD", {
  skip_if_not_installed("stringi")
  nfd <- intToUtf8(c(74L, 111L, 115L, 101L, 769L))
  salida <- lupa:::.normalizacion_aplicar(
    c("José", nfd, "g̃"),
    normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                  comillas = FALSE)
  )
  expect_identical(salida[[1L]], salida[[2L]])
  expect_identical(salida[[3L]], "g̃")
  marcas_desordenadas <- intToUtf8(c(769L, 803L, 97L))
  marcas_ordenadas <- intToUtf8(c(803L, 769L, 97L))
  expect_identical(
    lupa:::.normalizacion_aplicar(
      c(marcas_desordenadas, marcas_ordenadas),
      normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                    comillas = FALSE)
    )[[1L]],
    lupa:::.normalizacion_aplicar(
      marcas_ordenadas,
      normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                    comillas = FALSE)
    )[[1L]]
  )
  expect_identical(salida, stringi::stri_trans_nfd(c("José", nfd, "g̃")))
})

test_that("la tabla latina de descomposición coincide con stringi", {
  skip_if_not_installed("stringi")
  codigos <- c(0x00A0:0x024F, 0x1E00:0x1EFF)
  caracteres <- intToUtf8(codigos, multiple = TRUE)
  perfil <- normalizacion(
    minusculas = FALSE, espacios = FALSE, acentos = FALSE,
    comillas = FALSE
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(caracteres, perfil),
    stringi::stri_trans_nfd(caracteres)
  )
})

test_that("los pasos optativos son explícitos", {
  entrada <- c("oﬁcina", "ＡＢＣ", "Ana, Silva")
  expect_identical(
    lupa:::.normalizacion_aplicar(entrada, normalizacion()),
    c("oﬁcina", "ａｂｃ", "ana, silva")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(
      entrada, normalizacion(ligaduras = TRUE, ancho = TRUE, puntuacion = TRUE)
    ),
    c("oficina", "abc", "ana silva")
  )
  expect_true(is.list(lupa:::.resolver_normalizacion("amplio")))
})

test_that("la herencia pasa por el perfil y las fusiones quedan observables", {
  datos <- data.frame(nombre = c("JOSÉ", "jose"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     normalizar = normalizacion(acentos = FALSE))
  expect_s3_class(perfil$meta$normalizacion, "normalizacion_resuelta_lupa")
  duplicados <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", normalizar = NULL, perfil = perfil,
    max_resultados = Inf
  )
  expect_true(nrow(duplicados$pares) > 0L)
  expect_false(any(duplicados$pares$tipo_par == "exacto"))
  expect_true(is.list(duplicados$normalizacion$fusiones))
  expect_true(is.list(perfil$meta$normalizacion_fusiones$nombre))
  claves <- detectar_claves(datos, normalizar = NULL, perfil = perfil)
  expect_true(all(c("unicidad_exacta", "unicidad_normalizada") %in% names(claves)))
})

test_that("los perfiles por columna se resuelven sin mezclar criterios", {
  datos <- data.frame(nombre = c("JOSÉ", "jose"), codigo = c("A-1", "A1"),
                      stringsAsFactors = FALSE)
  perfil <- list(
    nombre = normalizacion(acentos = TRUE),
    codigo = normalizacion(acentos = FALSE, puntuacion = TRUE)
  )
  salida <- detectar_duplicados_aproximados(
    datos, columnas = names(datos), normalizar = perfil, max_resultados = Inf
  )
  expect_equal(salida$pares$tipo_par, "exacto")
})

test_that("las metricas de vocabulario aceptan perfiles de normalizacion", {
  skip_if_not_installed("stringdist")
  metrica <- especializar(
    metricas_nucleo()$EntidadContradictoria,
    normalizar = normalizacion(acentos = FALSE)
  )
  medicion <- medir(
    modelo(instanciar(metrica, "personas", "nombre")),
    data.frame(nombre = c("JOSÉ", "jose"), stringsAsFactors = FALSE)
  )
  alcance <- attr(medicion, "alcance_metricas")[[1L]]
  expect_identical(alcance$normalizacion$general$acentos, FALSE)
})
