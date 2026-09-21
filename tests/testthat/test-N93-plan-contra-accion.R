# La capa de ACCION: lo que el paquete propone hacer con lo medido.
#
# Dos defectos, los dos sobre una columna declarada `bytes`:
#
#   guiar_limpieza()   ABORTABA la corrida entera con el error crudo de R "no
#                      se permite traduccion de cadenas con bytes de
#                      codificacion", armando los ejemplos que le muestra al
#                      usuario. Y a diferencia de `aplicar()` -que anota la
#                      falla en su registro- no declaraba nada: la corrida se
#                      perdia sin rastro.
#   aplicar()          informaba `n_cambiadas = 2` donde el plan anuncio 1: la
#                      celda declarada, que la accion NO toco, se contaba como
#                      cambiada porque `!=` compara texto y el texto distingue
#                      la marca, aunque los bytes sean identicos.
#
# Decir que se cambio una celda que no cambio es informar como hecho lo que no
# se hizo.

.n93_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

.N93_SEC <- c(0x61, 0xc3, 0xb1, 0x6f)

.n93_plan_de <- function(valores) {
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  list(datos = datos, plan = suppressWarnings(planificar_limpieza(perfil)))
}

test_that("guiar_limpieza() no aborta sobre una columna declarada `bytes`", {
  base <- c("Montevideo", "MONTEVIDEO", "montevideo", "Salto", "SALTO")
  for (primero in list(.n93_bytes(.N93_SEC), rawToChar(as.raw(.N93_SEC)))) {
    p <- .n93_plan_de(c(primero, base))
    skip_if(!nrow(p$plan), "el plan salio vacio")
    expect_no_error(utils::capture.output(suppressWarnings(
      guiar_limpieza(p$plan, p$datos, selector = function(x) 0L)
    )))
  }
})

test_that("lo que el plan anuncia es lo que `aplicar()` hace", {
  # Un caracter invisible y un espacio duro, para que las acciones existan, y
  # una celda declarada `bytes` que ninguna de las dos debe tocar.
  invisible_ <- rawToChar(as.raw(c(0x61, 0xe2, 0x80, 0x8b, 0x62)))
  duro <- rawToChar(as.raw(c(0x63, 0xc2, 0xa0, 0x64)))
  p <- .n93_plan_de(c(.n93_bytes(.N93_SEC), invisible_, duro, "limpio"))
  skip_if(!nrow(p$plan), "el plan salio vacio")

  for (i in seq_len(nrow(p$plan))) {
    estrategia <- p$plan$estrategia[[i]]
    activo <- p$plan
    activo$aplicar <- seq_len(nrow(p$plan)) == i
    resultado <- suppressWarnings(aplicar(activo, p$datos))
    registro <- resultado$registro
    fila <- registro[registro$estrategia == estrategia, , drop = FALSE]
    if (!nrow(fila)) next
    expect_identical(fila$estado[[1L]], "ejecutada", info = estrategia)
    expect_equal(
      as.numeric(fila$n_cambiadas[[1L]]),
      as.numeric(p$plan$n_afectadas[[i]]),
      info = paste(estrategia, ": anuncio contra hecho")
    )
  }
})

test_that("una celda cuyos bytes no cambiaron no se cuenta como cambiada", {
  contar <- getFromNamespace(".celdas_cambiadas", "lupa")
  crudo <- .n93_bytes(.N93_SEC)
  texto <- rawToChar(as.raw(.N93_SEC))
  # Mismos bytes, distinta marca: `!=` de R dice que difieren y los bytes no.
  expect_false(contar(crudo, texto)[[1L]])
  # Y un cambio de verdad si se cuenta.
  expect_true(contar("a", "b")[[1L]])
  # NA en el original no cuenta; que pase a NA, si.
  expect_false(contar(NA_character_, "b")[[1L]])
  expect_true(contar("a", NA_character_)[[1L]])
})
