# Una fila de `cobertura_diagnosticos` existe para declarar CUANTO se midio.
# Si pierde su cifra, no declara nada y ademas parece completa: el mensaje salia
# "El resumen cuantitativo se calculo sobre  valores", con el hueco donde iba el
# numero, porque se pedia `fila$n_validos` y esa columna no existe -el conteo
# valido se deriva de `n_aplicables - n_faltantes`-.
#
# No rompia nada, no avisaba y no cambiaba ningun calculo. Por eso sobrevivio.

test_that("el motivo de cobertura trae su cifra y no un hueco", {
  datos <- data.frame(val = c(10, 20, 30, 40, 9999))
  perfil <- perfilar(
    datos, sentinelas_numericos = 9999, analizar_dependencias = FALSE
  )
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "resumen_cuantitativo", ]
  expect_equal(nrow(fila), 1L)

  motivo <- fila$motivo[[1L]]
  # cuatro valores resumidos: 5 aplicables, 0 faltantes, 1 excluido
  expect_match(motivo, "se calculo sobre 4 valores", fixed = TRUE)
  expect_match(motivo, "dejo afuera 1 valores", fixed = TRUE)
})

test_that("ningun motivo de cobertura deja un hueco donde va una cifra", {
  # La comprobacion general, que es la que atrapa al PROXIMO: un doble espacio en
  # estos mensajes es siempre una cifra que se evaporo al concatenar un vector
  # vacio. Se recorren varias formas de tabla para que la regla se ejerza.
  tablas <- list(
    numerica = data.frame(val = c(10, 20, 30, 40, 9999)),
    texto = data.frame(t = c("uno", "dos", "dos", NA, "tres"),
                       stringsAsFactors = FALSE),
    mixta = data.frame(
      v = c(1, 2, NA, 4, -99), t = c("a", "b", "b", "c", NA),
      stringsAsFactors = FALSE
    ),
    fechas = data.frame(
      f = as.Date(c("2026-01-01", "2026-01-02", NA, "2026-01-04", "2026-01-05"))
    )
  )
  for (nombre in names(tablas)) {
    perfil <- suppressWarnings(perfilar(
      tablas[[nombre]], sentinelas_numericos = c(9999, -99),
      analizar_dependencias = FALSE
    ))
    cobertura <- perfil$cobertura_diagnosticos
    if (is.null(cobertura) || !nrow(cobertura)) next
    motivos <- cobertura$motivo
    expect_false(
      any(grepl("  ", motivos, fixed = TRUE)), info = nombre
    )
    # y ninguno termina con la preposicion suelta, que es la otra forma del hueco
    expect_false(any(grepl("sobre $| de $| afuera $", motivos)), info = nombre)
  }
})
