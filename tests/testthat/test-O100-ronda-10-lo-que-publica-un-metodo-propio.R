# Ronda 10: lo que publica un metodo propio.

.o100_propia <- function(metodo) {
  instanciar(
    especializar(metrica("Propia", "Prueba.", "instanciaAtributo", "booleano")),
    "t", "valor", metodo = metodo
  )
}

test_that("una fila que no existe deja la metrica no medible", {
  con_fila <- function(fila) {
    function(tablas, instancia) {
      data.frame(resultado = TRUE, entidad = "t", atributo = "valor",
                 fila = fila, objeto = "x")
    }
  }
  datos <- data.frame(valor = c(1, 2, 3, 4))
  # Antes: `fila = 99` sobre cuatro filas se publicaba como medida, y `3e9`
  # salia NA despues de la validacion.
  .expect_no_medible(medir(modelo(.o100_propia(con_fila(99))), datos),
                     "no existe")
  .expect_no_medible(medir(modelo(.o100_propia(con_fila(3e9))), datos),
                     "no existe")
  # Control: la ultima fila existe y se mide.
  expect_equal(medir(modelo(.o100_propia(con_fila(4))), datos)$fila, 4L)
})

test_that("la etiqueta de un metodo propio no publica un valor personal", {
  documentos <- sprintf("%08d", 77177100 + 1:6)
  datos <- data.frame(documento = documentos, valor = c(1, 1, 2, NA, 3, 2),
                      stringsAsFactors = FALSE)
  cita <- function(tablas, instancia) {
    doc <- tablas$t$documento
    data.frame(resultado = TRUE, entidad = "t", atributo = "valor", fila = 1L,
               objeto = paste("fila con documento", doc[1], "revisada"))
  }
  medicion <- medir(modelo(.o100_propia(cita)), list(t = datos),
                    columnas_personales = "documento")
  # Antes: el documento salia en `objeto_medible`.
  expect_false(any(grepl(documentos[[1L]], medicion$objeto_medible, fixed = TRUE)))
  expect_identical(medicion$objeto_medible, "t$valor[1]")
  # Control: sin proteccion pedida, la etiqueta queda como la escribio el metodo;
  # y sin columnas personales en la tabla, tambien.
  abierta <- medir(modelo(.o100_propia(cita)), list(t = datos),
                   proteger_datos_personales = FALSE)
  expect_match(abierta$objeto_medible, "revisada", fixed = TRUE)
  # (Una columna que se LLAMA `documento` ya se clasifica personal por el nombre,
  # asi que el control usa otro.)
  cita_codigo <- function(tablas, instancia) {
    data.frame(resultado = TRUE, entidad = "t", atributo = "valor", fila = 1L,
               objeto = paste("fila con codigo", tablas$t$codigo[1], "revisada"))
  }
  sin_personales <- data.frame(codigo = c("a", "b", "c"), valor = 1:3)
  propia_sin <- medir(modelo(.o100_propia(cita_codigo)), list(t = sin_personales))
  expect_match(propia_sin$objeto_medible, "revisada", fixed = TRUE)
})
