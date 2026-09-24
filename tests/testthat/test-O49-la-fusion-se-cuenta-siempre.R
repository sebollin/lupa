# `n_no_reversibles` promete contar las celdas cuyo valor se perdio, y la
# documentacion fija el criterio: "se mide sobre el RESULTADO, no por el nombre
# de la accion". No se media: los ejecutores que no devolvian la cuenta la
# dejaban en cero. Medido sobre `ana | ANA | beto`, `convertir_mayusculas` funde
# dos celdas y publicaba 0, mientras `eliminar_controles_invisibles` publicaba 1
# con la misma fusion. Ahora la cuenta se toma en la puerta comun, asi que
# tambien alcanza a la proxima estrategia que se agregue sin devolverla.

aplicar_una_o49 <- function(valores, estrategia, permitir = FALSE) {
  datos <- data.frame(col = valores, stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- which(as.character(plan$estrategia) == estrategia)
  if (!length(fila)) return(NULL)
  plan$aplicar <- FALSE
  plan$aplicar[fila[[1L]]] <- TRUE
  resultado <- aplicar(plan, datos, permitir_eliminacion = permitir)
  registro <- as.data.frame(resultado$registro)
  list(
    registro = registro[as.character(registro$estrategia) == estrategia, ,
                        drop = FALSE],
    antes = as.character(datos$col),
    despues = as.character(resultado$datos$col)
  )
}

# La cuenta independiente: celdas que, despues, son indistinguibles de otra que
# antes era distinta. No se pregunta por la estrategia, se mira el resultado.
celdas_fundidas_o49 <- function(antes, despues) {
  sum(duplicated(despues)) - sum(duplicated(antes))
}

test_that("las normalizaciones de escritura cuentan la fusion que producen", {
  casos <- list(
    list(estrategia = "convertir_mayusculas",
         valores = c("ana", "ANA", "Ana", "beto")),
    list(estrategia = "convertir_minusculas",
         valores = c("ana", "ANA", "Ana", "beto")),
    list(estrategia = "convertir_titulo",
         valores = c("juana perez", "Juana Perez", "x")),
    list(estrategia = "decodificar_entidades_html",
         valores = c("a&amp;b", "a&b", "zz")),
    list(estrategia = "eliminar_controles_invisibles",
         valores = c(paste0("A", intToUtf8(0x200b), "B"), "AB", "zz")),
    list(estrategia = "normalizar_espacios_invisibles",
         valores = c(paste0("a", intToUtf8(0x00a0), "b"), "a b", "zz")),
    list(estrategia = "recortar_espacios",
         valores = c(" x ", "x", "y", "z"))
  )

  for (caso in casos) {
    medido <- aplicar_una_o49(caso$valores, caso$estrategia)
    expect_false(is.null(medido), info = caso$estrategia)
    fundidas <- celdas_fundidas_o49(medido$antes, medido$despues)
    expect_gt(fundidas, 0L)
    expect_equal(
      medido$registro$n_no_reversibles[[1L]], as.numeric(fundidas),
      info = paste(caso$estrategia, ":", paste(medido$despues, collapse = "|"))
    )
  }
})

test_that("sin fusion no se cuenta nada", {
  # El control tiene que poder fallar: si la cuenta contara todo cambio, estas
  # dos acciones -que cambian celdas sin perder nada- publicarian un numero.
  recorte <- aplicar_una_o49(c(" a ", " b ", "c"), "recortar_espacios")
  expect_equal(celdas_fundidas_o49(recorte$antes, recorte$despues), 0L)
  expect_gt(recorte$registro$n_cambiadas[[1L]], 0)
  expect_equal(recorte$registro$n_no_reversibles[[1L]], 0)

  marca <- aplicar_una_o49(c("a", NA, "b"), "marcar_filas_ausentes")
  expect_gt(marca$registro$n_cambiadas[[1L]], 0)
  expect_equal(marca$registro$n_no_reversibles[[1L]], 0)
})

test_that("la accion que ya cuenta su perdida conserva la suya", {
  # `convertir_ausencias_textuales` no funde nada -cada `S/D` pasa a `NA`- y su
  # ejecutor cuenta esa perdida. La medicion en la puerta comun no la pisa.
  conversion <- aplicar_una_o49(c("S/D", "a", "b"), "convertir_ausencias_textuales")
  expect_equal(conversion$registro$n_cambiadas[[1L]], 1)
  expect_equal(conversion$registro$n_no_reversibles[[1L]], 1)
})

test_that("una fila eliminada no se cuenta como celda perdida", {
  # La perdida por filas ya viaja en `n_filas_eliminadas`; contarla tambien aca
  # seria contar dos veces la misma cosa.
  eliminacion <- aplicar_una_o49(c("a", NA, "b"), "eliminar_filas_ausentes",
                                 permitir = TRUE)
  expect_equal(eliminacion$registro$n_filas_eliminadas[[1L]], 1)
  expect_equal(eliminacion$registro$n_no_reversibles[[1L]], 0)
})
