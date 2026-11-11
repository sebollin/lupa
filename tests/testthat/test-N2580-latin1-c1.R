testthat::skip_on_cran()

.n2580_latin1 <- function(codigo, sufijo = "") {
  texto <- paste0(" borde ", intToUtf8(codigo), sufijo, " ")
  salida <- iconv(texto, from = "UTF-8", to = "latin1")
  Encoding(salida) <- "latin1"
  salida
}

.n2580_esperado <- function(codigo) {
  indefinidos <- c(0x81L, 0x8dL, 0x8fL, 0x90L, 0x9dL)
  if (codigo %in% indefinidos) return(intToUtf8(codigo))
  crudo <- rawToChar(as.raw(codigo))
  Encoding(crudo) <- "latin1"
  enc2utf8(crudo)
}

.n2580_codigos <- function() c(seq.int(0x80L, 0xffL))

test_that("recortar_espacios conserva todos los bytes latin1 altos", {
  codigos <- .n2580_codigos()
  datos <- data.frame(
    texto = vapply(codigos, .n2580_latin1, character(1L)),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, fecha = "2026-01-15"), datos
  )
  plan$aplicar <- as.character(plan$estrategia) == "recortar_espacios"

  resultado <- aplicar(plan, datos)
  fila <- resultado$registro[
    resultado$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]
  expect_identical(nrow(fila), 1L)
  expect_identical(as.character(fila$estado), "ejecutada")
  expect_identical(as.numeric(fila$n_cambiadas), as.numeric(length(codigos)))
  expect_identical(
    as.numeric(fila$n_codificacion_normalizada), as.numeric(length(codigos))
  )
  esperados <- paste0(
    "borde ", vapply(codigos, .n2580_esperado, character(1L))
  )
  expect_identical(enc2utf8(resultado$datos$texto), esperados)
  expect_true(all(Encoding(resultado$datos$texto) == "UTF-8"))
})

test_that("las acciones textuales reciben latin1 en una forma segura", {
  codigos <- .n2580_codigos()
  textos <- vapply(
    codigos,
    function(codigo) .n2580_latin1(codigo, sufijo = "x"),
    character(1L)
  )
  acciones <- list(
    recortar_espacios = function(x) .recortar_texto(x)$valor,
    eliminar_controles_invisibles = function(x) {
      .quitar_controles_invisibles(x)$valor
    },
    normalizar_espacios_invisibles = function(x) {
      .normalizar_espacios_invisibles(x)$valor
    },
    decodificar_entidades_html = function(x) {
      .decodificar_entidades_html(x)$valor
    },
    reemplazar_separadores = function(x) {
      .reemplazar_separadores(x)$valor
    },
    reparar_codificacion = function(x) {
      .reparar_codificacion(x, list(max_iteraciones = 2L))$valor
    },
    convertir_minusculas = function(x) {
      .transformar_capitalizacion(x, "convertir_minusculas", list())$valor
    }
  )
  for (nombre in names(acciones)) {
    expect_no_error(suppressWarnings(acciones[[nombre]](textos)))
  }
})
