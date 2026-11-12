# La cita del paquete lleva el DOI que CRAN le asigna solo cuando el paquete
# viene de CRAN, como la cita que R arma sola para un paquete sin archivo
# CITATION. Con archivo propio R no lo agrega: lo tiene que decir el archivo.
# Una version de desarrollo no esta en CRAN, y citarla con ese DOI mandaria a
# una pagina donde esa version no existe.

test_that("la cita lleva el DOI de CRAN solo en una instalacion desde CRAN", {
  archivo <- system.file("CITATION", package = "lupa")
  skip_if(!nzchar(archivo), "el paquete no trae su archivo de cita")
  base <- list(Package = "lupa", Version = "9.9.9", Encoding = "UTF-8")
  de_cran <- utils::readCitationFile(
    archivo, meta = c(base, list(Repository = "CRAN"))
  )
  de_otro <- utils::readCitationFile(archivo, meta = base)
  de_universo <- utils::readCitationFile(
    archivo, meta = c(base, list(Repository = "https://ejemplo.r-universe.dev"))
  )

  doi <- "10.32614/CRAN.package.lupa"
  expect_identical(de_cran[[1L]]$doi, doi)
  expect_true(grepl(doi, de_cran[[1L]]$textVersion, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("9.9.9", de_cran[[1L]]$textVersion, fixed = TRUE,
                    useBytes = TRUE))

  expect_null(de_otro[[1L]]$doi)
  expect_false(grepl("doi", de_otro[[1L]]$textVersion, fixed = TRUE,
                     useBytes = TRUE))
  expect_null(de_universo[[1L]]$doi)

  # La segunda entrada es el marco conceptual, y no cambia con el origen.
  expect_length(de_cran, 2L)
  expect_identical(
    unclass(de_cran[[2L]])[[1L]]$title, unclass(de_otro[[2L]])[[1L]]$title
  )
})
