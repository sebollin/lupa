test_that("los controles invisibles se cuentan y se muestran de forma visible", {
  datos <- data.frame(
    control = c(
      "A\u200bB", paste0("x", intToUtf8(1L), "y"), "C\ufeffD", "limpio", NA
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "control", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "controles_invisibles", , drop = FALSE
  ]

  expect_equal(columna$n_controles_invisibles, 3L)
  expect_equal(hallazgo$n_evaluados, 5)
  expect_equal(hallazgo$n_afectados, 3)
  expect_equal(hallazgo$unidad_conteo, "fila")
  expect_match(hallazgo$evidencia, "<U\\+200B>", fixed = FALSE)
  expect_match(hallazgo$evidencia, "<U\\+0001>", fixed = FALSE)
  expect_match(hallazgo$evidencia, "<U\\+FEFF>", fixed = FALSE)
})

test_that("los bytes UTF-8 inválidos se aíslan antes de la pasada invisible", {
  invalido <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  perfil <- perfilar(
    data.frame(texto = c(invalido, "limpio"), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
  expect_equal(fila$n_codificacion_invalida, 1L)
  expect_equal(fila$n_controles_invisibles, 0L)
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
    observacion = c(
      "una\nlinea", "dos\rlineas", "tres\r\nlineas", "cuatro\tcolumnas",
      "cinco\fformfeed", "seis\vvertical", "limpia"
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "observacion", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "separadores_en_campo", , drop = FALSE
  ]

  expect_equal(columna$n_separadores_en_campo, 6L)
  expect_equal(hallazgo$n_afectados, 6)
  expect_match(hallazgo$evidencia, "\\\\n", fixed = FALSE)
  expect_match(hallazgo$evidencia, "\\\\r", fixed = FALSE)
  expect_match(hallazgo$evidencia, "\\\\t", fixed = FALSE)
  expect_match(hallazgo$evidencia, "\\\\f", fixed = FALSE)
  expect_match(hallazgo$evidencia, "\\\\v", fixed = FALSE)
})

test_that("los separadores no se confunden con controles invisibles", {
  datos <- data.frame(
    texto = c("Juan\tRodríguez", "a\fb", "c\vd", "x\001y", "limpio"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
  expect_equal(columna$n_controles_invisibles, 1L)
  expect_equal(columna$n_separadores_en_campo, 3L)
  expect_equal(
    sum(perfil$hallazgos$tipo_hallazgo == "controles_invisibles"), 1L
  )
  expect_equal(
    sum(perfil$hallazgos$tipo_hallazgo == "separadores_en_campo"), 1L
  )
})

test_that("el tabulador conserva el dato por omisión y se reemplaza explícitamente", {
  datos <- data.frame(
    nombre = "Juan\tRodríguez",
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  expect_false(any(plan$estrategia == "eliminar_controles_invisibles"))
  expect_true(any(plan$estrategia == "reemplazar_separadores"))
  expect_false(plan$aplicar[plan$estrategia == "reemplazar_separadores"])

  por_defecto <- aplicar(plan, datos)
  expect_identical(por_defecto$datos$nombre, "Juan\tRodríguez")

  plan$aplicar[plan$estrategia == "reemplazar_separadores"] <- TRUE
  explicito <- aplicar(plan, datos)
  expect_identical(explicito$datos$nombre, "Juan Rodríguez")
  expect_equal(explicito$registro$n_cambiadas[
    explicito$registro$estrategia == "reemplazar_separadores"
  ], 1L)
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
  saltos <- plan[plan$estrategia == "reemplazar_separadores", , drop = FALSE]
  expect_true(control$recomendada)
  expect_true(control$aplicar)
  expect_false(html$recomendada)
  expect_false(html$aplicar)
  expect_false(saltos$recomendada)
  expect_false(saltos$aplicar)

  plan$aplicar[plan$estrategia %in% c(
    "decodificar_entidades_html", "reemplazar_separadores"
  )] <- TRUE
  resultado <- aplicar(plan, datos)
  expect_equal(resultado$datos$control, "AB")
  expect_equal(resultado$datos$html, "José")
  expect_equal(resultado$datos$observacion, "una linea")
  expect_equal(
    resultado$registro$n_cambiadas[
      match(c("eliminar_controles_invisibles", "decodificar_entidades_html",
              "reemplazar_separadores"), resultado$registro$estrategia)
    ],
    c(1, 1, 1)
  )
  # Quitar el control no colapso este valor con ningun otro: `AB` sigue siendo
  # distinguible de todo lo demas, asi que no se perdio el valor de la celda.
  # El conteo declaraba 1 por regla de accion -toda celda modificada-, y esa
  # regla no distingue este caso del que SI pierde: con claves marcadas por un
  # espacio de ancho cero, la misma accion fusiona cinco claves en tres.
  expect_equal(
    resultado$registro$n_no_reversibles[
      resultado$registro$estrategia == "eliminar_controles_invisibles"
    ],
    0
  )
})

test_that("los invisibles Unicode se clasifican sin borrar ZWJ ni ZWNJ", {
  datos <- data.frame(
    texto = c(
      paste0("A", intToUtf8(0x00A0), "B"),
      paste0("C", intToUtf8(0x200B), "D"),
      paste0("E", intToUtf8(0x200C), "F"),
      paste0("G", intToUtf8(0x200D), "H"),
      paste0("I", intToUtf8(0x2060), "J")
    ), stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
  expect_equal(columna$n_controles_invisibles, 5L)
  expect_equal(columna$n_espacios_invisibles, 1L)
  expect_equal(columna$n_invisibles_eliminables, 2L)
  expect_equal(columna$n_invisibles_significativos, 2L)
  plan <- planificar_limpieza(perfil, datos)
  expect_true(any(plan$estrategia == "eliminar_controles_invisibles"))
  expect_true(any(plan$estrategia == "normalizar_espacios_invisibles"))
  # Las dos hermanas de esta rama se trataban al reves: `eliminar_controles_`
  # se recomendaba y se activaba sola y `normalizar_espacios_` era destructiva y
  # exigia confirmacion. Se igualaron el 2026-09-29, por decision de Sebastian y
  # con el argumento medido: normalizar un espacio Unicode deja un espacio -es
  # cambio de forma, no de valor-, y el paquete recomienda y activa
  # `recortar_espacios`, que quita espacios enteros.
  for (hermana in c(
    "eliminar_controles_invisibles", "normalizar_espacios_invisibles"
  )) {
    cual <- plan$estrategia == hermana
    expect_true(plan$recomendada[cual], info = hermana)
    expect_false(plan$destructiva[cual], info = hermana)
    expect_true(plan$aplicar[cual], info = hermana)
    # `reversible` sigue en FALSE: quitar o convertir un caracter no se deshace.
    # Lo que ya no se afirma es que destruya el VALOR.
    expect_false(plan$reversible[cual], info = hermana)
  }
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  expect_identical(resultado$datos$texto, c("A B", "CD", "E\u200cF", "G\u200dH", "IJ"))
  registro_espacio <- resultado$registro[
    resultado$registro$estrategia == "normalizar_espacios_invisibles", , drop = FALSE
  ]
  # Mismo criterio: `A B` no colapso con ninguna otra fila.
  expect_equal(registro_espacio$n_no_reversibles, 0)
  expect_false(registro_espacio$destructiva)

  # Y la guarda que sostiene la decision, en vez de un comentario: la
  # confirmacion tendria sentido si alguna de las dos acciones pudiera tocar un
  # invisible SIGNIFICATIVO -un ZWJ o un ZWNJ, que cambian la forma de una
  # palabra-, y ninguna puede, porque sus conjuntos de codigos son disjuntos del
  # de los significativos. Si alguien agrega uno ahi, esto falla.
  expect_equal(
    intersect(
      lupa:::.codigos_espacios_invisibles,
      lupa:::.codigos_invisibles_significativos
    ),
    integer(0)
  )
  expect_equal(
    intersect(
      lupa:::.codigos_invisibles_eliminables,
      lupa:::.codigos_invisibles_significativos
    ),
    integer(0)
  )
  # Y el control de que la guarda mide algo: los significativos existen y esta
  # columna los tiene, intactos despues de aplicar las dos acciones.
  expect_length(lupa:::.codigos_invisibles_significativos, 2L)
  expect_true(any(grepl(intToUtf8(0x200D), resultado$datos$texto, fixed = TRUE)))
})

test_that("la comparación normalizada trata espacios y basura sin tocar ZWJ", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c(
      "Ana\u00a0Perez", "Ana Perez", "Familia\u200dUnida", "FamiliaUnida"
    ), stringsAsFactors = FALSE
  )
  normalizado <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas", umbral = 0.1,
    max_resultados = Inf
  )
  expect_true(any(normalizado$pares$tipo_par == "exacto_normalizado"))
  sin_normalizar <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", estrategia = "teselas", umbral = 0.1,
    max_resultados = Inf, normalizar = FALSE
  )
  expect_gt(nrow(sin_normalizar$pares), 0L)
  expect_false(any(sin_normalizar$pares$tipo_par == "exacto"))
})
