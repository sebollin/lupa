# O44: `guiar_limpieza()` mostraba cinco ejemplos de cuarenta sin decirlo,
# mientras el mismo paquete declara cada recorte en sus informes y en sus
# impresiones. Una lista recortada que no dice que lo es se lee como la lista
# entera, y esta se lee justo antes de decidir si aplicar una accion.

test_that("la guia declara cuantos ejemplos muestra de cuantos hay", {
  datos <- data.frame(
    v = c(paste0("dato", seq_len(30L)), rep(NA_character_, 40L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)

  salida <- capture.output(
    invisible(guiar_limpieza(plan, datos = datos,
                             selector = function(...) 0L)),
    type = "message"
  )
  ejemplos <- grep("Ejemplos reales", salida, value = TRUE, fixed = TRUE)

  expect_true(length(ejemplos) >= 1L)
  expect_true(any(grepl("(5 de 40)", ejemplos, fixed = TRUE)))
})

test_that("si los ejemplos entran enteros no se anuncia un recorte", {
  # Control: el aviso sale de la cuenta, no de una constante.
  datos <- data.frame(
    v = c("dato1", "dato2", "dato3", NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)

  salida <- capture.output(
    invisible(guiar_limpieza(plan, datos = datos,
                             selector = function(...) 0L)),
    type = "message"
  )
  ejemplos <- grep("Ejemplos reales", salida, value = TRUE, fixed = TRUE)

  if (length(ejemplos)) {
    expect_false(any(grepl(" de ", ejemplos, fixed = TRUE)))
  } else {
    succeed("esta tabla no produjo grupos con ejemplos")
  }
})

test_that("el recorte lleva su total aunque nadie lo imprima", {
  # La cuenta viaja con los valores: quien consuma el auxiliar directamente
  # tambien puede saber cuantos habia.
  recortados <- lupa:::.recortar_ejemplos(as.character(seq_len(12L)), 5L)

  expect_length(recortados, 5L)
  expect_equal(attr(recortados, "total_ejemplos", exact = TRUE), 12L)
})
