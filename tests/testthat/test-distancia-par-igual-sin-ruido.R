# Dos textos iguales estan a distancia 0 por construccion, pero `stringdist`
# puede devolver ruido de coma flotante en lugar del cero: 3.3e-17 en macOS
# arm64, y lo mismo en Linux con R-devel compilado con multiplicacion fusionada.
# Ese ruido se publicaba en la evidencia ("distancia 0.0000000000000000333").
# Aca se imita el ruido sumandolo a lo que devuelve `stringdist`.

.dpi_con_ruido <- function(codigo) {
  original_d <- stringdist::stringdist
  original_m <- stringdist::stringdistmatrix
  testthat::with_mocked_bindings(
    codigo,
    stringdist = function(...) original_d(...) + 3.3e-17,
    stringdistmatrix = function(...) original_m(...) + 3.3e-17,
    .package = "stringdist"
  )
}

test_that("un par igual tras normalizar publica distancia 0 aunque stringdist devuelva ruido", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  datos <- data.frame(n = c("Juan Perez", "Juan Perez ", "Maria"))
  pares <- .dpi_con_ruido(detectar_duplicados_aproximados(datos))
  expect_identical(pares$pares$tipo_par[[1L]], "exacto_normalizado")
  expect_identical(pares$pares$distancia[[1L]], 0)
  expect_true(grepl("distancia 0 con jw (umbral 0.1)",
                    pares$hallazgos$evidencia[[1L]], fixed = TRUE))
})

test_that("un par con el mismo texto publica distancia 0 aunque stringdist devuelva ruido", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  datos <- data.frame(n = c("Ana Gomez", "Ana Gomez", "Pedro Ruiz", "Pedro Ruis"))
  pares <- .dpi_con_ruido(detectar_duplicados_aproximados(datos))
  exacto <- pares$pares[pares$pares$tipo_par == "exacto", , drop = FALSE]
  expect_identical(nrow(exacto), 1L)
  expect_identical(exacto$distancia, 0)
  # Control: el par aproximado conserva su distancia, con el ruido incluido.
  aproximado <- pares$pares[pares$pares$tipo_par == "aproximado", , drop = FALSE]
  expect_identical(nrow(aproximado), 1L)
  expect_gt(aproximado$distancia, 0)
})
