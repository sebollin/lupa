# El catalogo promete, para cada entrada `via_agregacion`, "la llamada que los
# materializa". La llamada no es una expresion autonoma: trae nombres libres.
# La documentacion declara cuales son -`m`, la medicion base, y `u`, el umbral
# que elige quien agrega- y aqui se mide que no aparezca ningun otro y que la
# receta, con esos dos ligados, devuelva el ratio que dice devolver.

entradas_agregadas_o47 <- function() {
  catalogo <- catalogo_agesic()
  catalogo[as.character(catalogo$estado) == "via_agregacion", , drop = FALSE]
}

nombres_libres_o47 <- function(llamada) {
  setdiff(all.vars(parse(text = llamada)[[1L]]), character())
}

test_that("la receta publicada solo usa los nombres que el catalogo declara", {
  entradas <- entradas_agregadas_o47()
  expect_gt(nrow(entradas), 0L)

  for (i in seq_len(nrow(entradas))) {
    llamada <- entradas$implementacion[[i]]
    expect_false(is.na(llamada))
    libres <- nombres_libres_o47(llamada)
    expect_true(
      all(libres %in% c("m", "u")),
      info = paste("entrada", entradas$numero[[i]], ":", llamada)
    )
    expect_true("m" %in% libres)
  }
})

test_that("cada receta agrega hacia una granularidad que el marco permite", {
  entradas <- entradas_agregadas_o47()
  disponibles <- c(metricas_nucleo(), metricas_referencial())

  for (i in seq_len(nrow(entradas))) {
    nombre <- as.character(entradas$metrica_lupa[[i]])
    generica <- disponibles[[nombre]]
    expect_false(is.null(generica), info = nombre)
    origen <- attr(generica, "declaracion")$granularidad
    destino <- .validar_granularidad(
      as.character(parse(text = entradas$implementacion[[i]])[[1L]][[3L]]),
      aceptar_relacional = TRUE
    )
    expect_true(
      any(.transiciones_granularidad$origen == origen &
            .transiciones_granularidad$destino == destino),
      info = paste(nombre, origen, "->", destino)
    )
  }
})

test_that("la receta con umbral materializa el ratio que nombra", {
  datos <- data.frame(
    nombre = c("Ana", "Luis", NA, "Mar", "Sol", NA),
    edad = c(20, NA, 35, 40, NA, 18),
    ciudad = c("MVD", "MVD", "SLG", NA, "MVD", "SLG"),
    stringsAsFactors = FALSE
  )
  especifica <- especializar(
    metricas_nucleo()$DensidadPonderada,
    coeficientes = c(nombre = 0.5, edad = 0.3, ciudad = 0.2)
  )
  instancia <- instanciar(especifica, "personas", names(datos))
  m <- medir(modelo(instancia), datos)
  u <- 0.8

  entradas <- entradas_agregadas_o47()
  llamada <- entradas$implementacion[
    as.character(entradas$metrica_lupa) == "DensidadPonderada"
  ]
  expect_equal(length(llamada), 1L)

  agregada <- eval(parse(text = llamada)[[1L]])
  esperado <- mean(as.data.frame(m)$resultado >= u)

  expect_equal(nrow(agregada), 1L)
  expect_equal(agregada$resultado, esperado)
  expect_identical(as.character(agregada$agregacion), "ratio_umbral")
  expect_identical(as.character(agregada$granularidad), "entidad")
})

test_that("la receta sin umbral materializa el ratio que nombra", {
  datos <- data.frame(edad = c(20, NA, 35, 40, NA, 18))
  instancia <- instanciar(
    especializar(metricas_nucleo()$NoNulo), "personas", "edad"
  )
  m <- medir(modelo(instancia), datos)

  entradas <- entradas_agregadas_o47()
  llamada <- entradas$implementacion[
    as.character(entradas$metrica_lupa) == "NoNulo"
  ]
  expect_equal(length(llamada), 1L)

  agregada <- eval(parse(text = llamada)[[1L]])
  esperado <- mean(!is.na(datos$edad))

  expect_equal(nrow(agregada), 1L)
  expect_equal(agregada$resultado, esperado)
  expect_identical(as.character(agregada$agregacion), "ratio")
  expect_identical(as.character(agregada$granularidad), "atributo")
})
