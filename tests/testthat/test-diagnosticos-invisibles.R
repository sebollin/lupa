test_that("los controles invisibles se cuentan y se muestran de forma visible", {
  datos <- data.frame(
    control = c("A\u200bB", paste0("x", intToUtf8(1L), "y"), "limpio", NA),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "control", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "controles_invisibles", , drop = FALSE
  ]

  expect_equal(columna$n_controles_invisibles, 2L)
  expect_equal(hallazgo$n_evaluados, 4)
  expect_equal(hallazgo$n_afectados, 2)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_match(hallazgo$evidencia, "<U\\+200B>", fixed = FALSE)
  expect_match(hallazgo$evidencia, "<U\\+0001>", fixed = FALSE)
})

test_that("las entidades HTML válidas se distinguen de ampersands legítimos", {
  datos <- data.frame(
    texto = c(
      "Jos&eacute;", "&#243;", "&#x00f1;", "Ventas & Marketing",
      "A&B", "Tom & Jerry S.A.", "P&L; resultado", "normal"
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "entidades_html", , drop = FALSE
  ]

  expect_equal(columna$n_entidades_html, 3L)
  expect_equal(hallazgo$n_afectados, 3)
  expect_match(hallazgo$evidencia, "&eacute;", fixed = TRUE)
  expect_false(any(grepl("Ventas & Marketing|A&B|P&L;", hallazgo$evidencia)))

  falsos_positivos <- perfilar(data.frame(
    texto = rep(c("Ventas & Marketing", "A&B", "Tom & Jerry S.A.",
                  "P&L; resultado"), 25L),
    stringsAsFactors = FALSE
  ), analizar_dependencias = FALSE)
  expect_false(any(
    falsos_positivos$hallazgos$tipo_hallazgo == "entidades_html"
  ))
})

test_that("los saltos de línea se cuentan y se escapan en la evidencia", {
  datos <- data.frame(
    observacion = c("una\nlinea", "dos\rlineas", "tres\r\nlineas", "limpia"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "observacion", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "saltos_linea", , drop = FALSE
  ]

  expect_equal(columna$n_saltos_linea, 3L)
  expect_equal(hallazgo$n_afectados, 3)
  expect_match(hallazgo$evidencia, "\\\\n", fixed = FALSE)
  expect_match(hallazgo$evidencia, "\\\\r", fixed = FALSE)
})

test_that("sólo los controles se recomiendan y las acciones registran cambios", {
  datos <- data.frame(
    control = paste0("A", intToUtf8(0x200B), "B"),
    html = "Jos&eacute;",
    observacion = "una\nlinea",
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  control <- plan[plan$estrategia == "eliminar_controles_invisibles", , drop = FALSE]
  html <- plan[plan$estrategia == "decodificar_entidades_html", , drop = FALSE]
  saltos <- plan[plan$estrategia == "reemplazar_saltos_linea", , drop = FALSE]
  expect_true(control$recomendada)
  expect_true(control$aplicar)
  expect_false(html$recomendada)
  expect_false(html$aplicar)
  expect_false(saltos$recomendada)
  expect_false(saltos$aplicar)

  plan$aplicar[plan$estrategia %in% c(
    "decodificar_entidades_html", "reemplazar_saltos_linea"
  )] <- TRUE
  resultado <- aplicar(plan, datos)
  expect_equal(resultado$datos$control, "AB")
  expect_equal(resultado$datos$html, "José")
  expect_equal(resultado$datos$observacion, "una linea")
  expect_equal(
    resultado$registro$n_cambiadas[
      match(c("eliminar_controles_invisibles", "decodificar_entidades_html",
              "reemplazar_saltos_linea"), resultado$registro$estrategia)
    ],
    c(1, 1, 1)
  )
})
