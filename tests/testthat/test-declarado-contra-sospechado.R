# La regla que gobierna los resumenes: el paquete EXCLUYE lo que el usuario
# declara e INCLUYE lo que el mismo sospecha.
#
# Se llego aca dando dos vueltas, y las dos quedan fijadas para que nadie las
# vuelva a dar. Primero se creyo que los estadisticos eran literales -que
# describian la columna como esta guardada- porque en texto `moda` vale "N/A"
# cuando hay seiscientos "N/A". Despues se midio que `aplicabilidad` y un `NA`
# escrito a mano SI cambian la media sobre los mismos datos, y entonces la
# regla literal no existia: `sentinelas_numericos`, que es una declaracion del
# usuario, era la unica de las tres que se ignoraba.
#
# Lo que este archivo cubre y `test-centinela-declarado.R` no:
#   1. las TRES formas de declarar dan el mismo resumen;
#   2. `moda` fuera de `[minimo, maximo]` NO es una contradiccion nueva: ya
#      pasaba con `Inf`, y esa es la razon de que se conserve;
#   3. la lista por omision NO escala la severidad, que es el caso que protege
#      a quien tiene un `999` legitimo;
#   4. la sugerencia del hallazgo dice la consecuencia, no solo la accion.

test_that("las tres formas de declarar ausencia dan el mismo resumen", {
  set.seed(1)
  base <- round(stats::rnorm(950, 40, 8))
  crudo <- c(base, rep(9999L, 30), rep(9998L, 20))

  declarado <- perfilar(data.frame(h = crudo),
                        sentinelas_numericos = c(9999, 9998),
                        analizar_dependencias = FALSE)$columnas
  todas <- perfilar(
    data.frame(h = crudo, universo = crudo < 9000),
    aplicabilidad = list(h = ~ universo == TRUE),
    analizar_dependencias = FALSE
  )$columnas
  # Esa tabla trae tambien la columna auxiliar `universo`: hay que quedarse con
  # la fila de `h` o se comparan vectores de largo distinto.
  con_universo <- todas[todas$columna == "h", , drop = FALSE]
  a_mano <- crudo
  a_mano[a_mano >= 9000] <- NA
  con_na <- perfilar(data.frame(h = a_mano),
                     analizar_dependencias = FALSE)$columnas

  for (campo in c("media", "mediana", "minimo", "maximo", "desvio")) {
    expect_equal(declarado[[campo]], con_universo[[campo]], info = campo)
    expect_equal(declarado[[campo]], con_na[[campo]], info = campo)
  }
  # Y las tres coinciden con la verdad calculada a mano.
  expect_equal(declarado$media, mean(base), tolerance = 1e-8)

  # La exclusion no es muda: queda declarada en el dato.
  expect_equal(declarado$n_valores_excluidos_resumen, 50L)
  expect_equal(declarado$estado_resumen_cuantitativo, "calculados_sobre_valores")
})

test_that("una moda fuera del rango no es nueva: `Inf` ya se comportaba asi", {
  # `Inf` se excluye de los estadisticos desde siempre y `n_infinito_positivo`
  # lo declara; `moda` en cambio lo sigue informando. Un centinela declarado
  # tiene que comportarse igual, porque es el mismo caso: un valor presente que
  # no vale como dato. Si algun dia se decide que `moda` debe excluirlos, este
  # test tiene que cambiar A LA VEZ para los dos, o quedaran discrepando.
  valores <- round(seq(10, 90, length.out = 400))

  con_infinito <- perfilar(
    data.frame(h = c(rep(Inf, 600), valores)),
    analizar_dependencias = FALSE
  )$columnas
  # `moda` es una columna de texto: guarda la representacion, no el numero.
  expect_equal(con_infinito$maximo, 90)
  expect_equal(con_infinito$moda, "Inf")
  expect_equal(con_infinito$n_infinito_positivo, 600L)
  expect_gt(as.numeric(con_infinito$moda), con_infinito$maximo)

  con_centinela <- perfilar(
    data.frame(h = c(rep(9999, 600), valores)),
    sentinelas_numericos = 9999, analizar_dependencias = FALSE
  )$columnas
  expect_equal(con_centinela$maximo, 90)
  expect_equal(con_centinela$moda, "9999")
  expect_equal(con_centinela$n_valores_excluidos_resumen, 600L)
  expect_gt(as.numeric(con_centinela$moda), con_centinela$maximo)
})

test_that("la lista por omision no escala la severidad, y la declarada si", {
  base <- seq_len(400)

  # `-999` viene en la lista por omision: es una conjetura del paquete, y una
  # conjetura no alcanza para un `error`. Este es el caso que protege a quien
  # tiene un codigo legitimo con esa forma.
  por_omision <- perfilar(data.frame(x = c(rep(-999, 600), base)),
                          analizar_dependencias = FALSE)$hallazgos
  expect_equal(
    as.character(por_omision$severidad[por_omision$tipo_hallazgo == "faltantes"]),
    "sospechoso"
  )

  # El mismo valor, declarado por quien tiene los datos, si escala.
  declarado <- perfilar(data.frame(x = c(rep(-999, 600), base)),
                        sentinelas_numericos = -999,
                        analizar_dependencias = FALSE)$hallazgos
  expect_equal(
    as.character(declarado$severidad[declarado$tipo_hallazgo == "faltantes"]),
    "error"
  )
})

test_that("la sugerencia del centinela dice la consecuencia, no solo la accion", {
  set.seed(1)
  perfil <- perfilar(
    data.frame(h = c(round(stats::rnorm(950, 40, 8)), rep(9999L, 30))),
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "posible_centinela_numerico", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  sugerencia <- hallazgo$sugerencia[[1L]]

  expect_match(sugerencia, "sentinelas_numericos", fixed = TRUE)
  # Lo que faltaba: que dice que pasa con los numeros al declararlo.
  expect_match(sugerencia, "sale de", fixed = TRUE)
  expect_match(sugerencia, "n_valores_excluidos_resumen", fixed = TRUE)
  expect_match(sugerencia, "moda", fixed = TRUE)
})

test_that("pedir `sentinelas_naniar` es elegir heuristica, no declarar", {
  # `sentinelas_naniar` es una lista que PUBLICA el paquete, mas ancha que la de
  # por omision, y su documentacion avisa por que no viene puesta: `66`, `77` y
  # `88` tambien son edades. Elegirla no es afirmar nada sobre una columna
  # concreta, asi que no puede sacar valores del promedio ni escalar a `error`.
  #
  # Se detecto porque `test-hallazgos-administrativos.R` fallo al arreglar
  # §2.215: la primera version tomaba por declaracion todo vector distinto del
  # de por omision, incluida la lista del propio paquete.
  edades <- c(66, 77, 88, 45, 30, 22, 51, 63, 70, 41)

  con_naniar <- perfilar(data.frame(edad = edades),
                         sentinelas_numericos = sentinelas_naniar,
                         analizar_dependencias = FALSE)
  columna <- con_naniar$columnas

  # Los cuenta como disfrazados -para eso se pidio la lista-...
  expect_equal(columna$n_faltantes_disfrazados, 3L)
  # ...y NO los saca de los estadisticos.
  expect_equal(columna$media, mean(edades), tolerance = 1e-8)
  expect_equal(columna$maximo, 88)
  expect_equal(columna$n_valores_excluidos_resumen, 0L)
  expect_false(any(con_naniar$hallazgos$severidad == "error"))

  # El mismo `66`, declarado a mano, si sale.
  a_mano <- perfilar(data.frame(edad = edades),
                     sentinelas_numericos = 66,
                     analizar_dependencias = FALSE)$columnas
  expect_equal(a_mano$n_valores_excluidos_resumen, 1L)
  expect_equal(a_mano$media, mean(edades[edades != 66]), tolerance = 1e-8)
})
