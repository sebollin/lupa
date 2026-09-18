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

test_that("el consentimiento de eliminacion no se evade editando el plan", {
  datos <- data.frame(x = c(NA, "a", NA, "b"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  fila <- which(plan$estrategia == "eliminar_filas_ausentes")
  skip_if(!length(fila), "el plan generado no trae la accion eliminatoria")

  activo <- plan
  activo$aplicar <- FALSE
  activo$aplicar[fila] <- TRUE

  # La puerta, tal como debe estar.
  expect_error(aplicar(activo, datos), "permitir_eliminacion")

  # Y la puerta editando la celda que el plan expone: `destructiva` es un
  # hallazgo del paquete, no una decision. Combinarla con la estrategia en la
  # condicion hacia que poner FALSE ahi borrara las filas sin consentimiento.
  disfrazado <- activo
  disfrazado$destructiva[fila] <- FALSE
  expect_error(aplicar(disfrazado, datos), "permitir_eliminacion")

  # Con el permiso explicito sigue corriendo, y elimina.
  resultado <- aplicar(activo, datos, permitir_eliminacion = TRUE)
  expect_lt(nrow(resultado$datos), nrow(datos))
})

test_that("una edicion del plan no deja escapar un error interno de R", {
  datos <- data.frame(x = c(" A ", "B"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  plan$aplicar <- FALSE
  plan$aplicar[1L] <- TRUE

  # Se comprobaba que los identificadores fueran unicos y no que fueran texto.
  # Vaciando la columna, el usuario recibia "los argumentos implican un numero
  # diferente de filas: 0, 1", un error interno de armar el registro.
  for (roto in list(I(list(character(0))), seq_len(nrow(plan)),
                    c(NA_character_, tail(plan$id_accion, -1L)))) {
    alterado <- plan
    alterado$id_accion <- roto
    expect_error(aplicar(alterado, datos), "debe ser un vector de texto")
  }
  expect_silent(aplicar(plan, datos))
})

test_that("la evidencia del plan no depende del locale que lo genero", {
  texto_ejemplo <- getFromNamespace(".texto_ejemplo", "lupa")
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  originales <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  on.exit(
    for (categoria in categorias) {
      suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
    },
    add = TRUE
  )
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) skip("no hay ningun locale UTF-8 en esta maquina")

  casos <- c(
    rawToChar(charToRaw("\u00c1rbol")), rawToChar(charToRaw("ba\u00f1o")),
    "con \"comilla\"", "con\\barra"
  )
  bytes_en <- function(locale) {
    for (categoria in categorias) suppressWarnings(Sys.setlocale(categoria, locale))
    lapply(vapply(casos, texto_ejemplo, character(1L)), charToRaw)
  }

  # La evidencia viaja EN el plan, que el usuario edita y vuelve a entregar:
  # `encodeString()` la hacia depender del locale y dos personas obtenian
  # planes distintos de los mismos datos.
  expect_identical(bytes_en(locale_utf8), bytes_en("C"))
})

test_that("una accion repetida sin efecto se registra como fallida", {
  datos <- data.frame(a = c("x", "y"), b = c("x", "y"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  fila <- which(plan$estrategia == "marcar_columnas_duplicadas")
  skip_if(!length(fila), "el plan generado no trae la marca de duplicadas")

  repetida <- rbind(plan[fila, , drop = FALSE], plan[fila, , drop = FALSE])
  repetida$id_accion <- c("manual-1", "manual-2")
  repetida$grupo <- NA_character_
  repetida$aplicar <- TRUE
  class(repetida) <- class(plan)

  resultado <- aplicar(repetida, datos)
  # La segunda no cambia nada -la marca ya esta- y devolvia n = 1 fijo.
  expect_identical(as.character(resultado$registro$estado), c("ejecutada", "fallida"))
  expect_identical(resultado$registro$n_cambiadas, c(1, 0))
  expect_match(resultado$registro$error[[2L]], "sin efecto")
})
