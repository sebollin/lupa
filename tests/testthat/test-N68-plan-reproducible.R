# El plan de limpieza es texto que el usuario edita y vuelve a entrar al
# paquete. Dos ediciones legitimas lo rompian: un `orden` repetido y una
# decision de grupo sin accion activa.

test_that("un orden repetido no vuelve el resultado dependiente del orden de filas", {
  datos <- data.frame(x = c("\u200b A ", "B"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  skip_if(nrow(plan) < 2L, "el plan generado no trae dos acciones")

  # Empatadas a proposito: el usuario dice "me da igual el orden entre estas".
  plan$orden[] <- 500L
  invertido <- plan[rev(seq_len(nrow(plan))), , drop = FALSE]
  class(invertido) <- class(plan)

  original <- aplicar(plan, datos)
  reordenado <- aplicar(invertido, datos)

  # Desempatando por el indice de fila, estas dos salidas diferian: una
  # conservaba el espacio inicial y la otra no. El dato del usuario cambiaba
  # por como habia ordenado las filas de su plan.
  expect_identical(original$datos$x, reordenado$datos$x)
  expect_identical(
    as.character(original$plan_aplicado$estrategia),
    as.character(reordenado$plan_aplicado$estrategia)
  )
})

test_that("un grupo elegido sin accion activa se rechaza", {
  datos <- data.frame(x = c(NA_character_, "a"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  skip_if(!any(!is.na(plan$grupo)), "el plan generado no trae grupos")

  # `decision_grupo` distingue una eleccion de una omision. Marcado como
  # elegido y con todo desactivado no es ninguna de las dos, y el paquete ya
  # rechaza las otras contradicciones de esta familia en vez de adivinar.
  contradictorio <- plan
  contradictorio$aplicar <- FALSE
  contradictorio$decision_grupo <- "elegida"

  expect_error(
    aplicar(contradictorio, datos),
    "marcado como elegido y no tiene ninguna"
  )

  # Y el plan coherente -todo desactivado, decision desactivada- sigue corriendo.
  coherente <- plan
  coherente$aplicar <- FALSE
  coherente$decision_grupo <- "desactivada"
  expect_silent(resultado <- aplicar(coherente, datos))
  expect_identical(nrow(resultado$plan_aplicado), 0L)
})

test_that("Encoding 'bytes' se publica como lo publica R", {
  imprimir <- getFromNamespace(".print_data_frame_bytes", "lupa")

  # `Encoding() == "bytes"` no es una codificacion mas: es la declaracion de
  # que eso no se interprete como texto. El paquete lo marcaba UTF-8 y
  # publicaba el caracter, interpretando justo lo que se pidio no interpretar.
  crudo <- rawToChar(charToRaw("a\u00f1o"))
  Encoding(crudo) <- "bytes"
  expect_identical(Encoding(crudo), "bytes")

  marco <- data.frame(v = crudo, stringsAsFactors = FALSE)
  expect_identical(
    capture.output(imprimir(marco)),
    capture.output(print.data.frame(marco))
  )
})
