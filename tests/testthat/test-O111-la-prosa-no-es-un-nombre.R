# Tercera evaluacion real: la prosa del paquete no se tapa por azar.
#
# La regla de variantes tapa una celda entera si su forma sin separadores
# contiene un valor protegido. Con los nombres de millones de personas como
# agujas, alguno aparecia cruzando palabras de la prosa -"...COLUMNACONTIENE..."-
# y en una base real quedaron tapadas 38 de 64 descripciones, tambien de
# columnas que no eran personales.

test_that("en la prosa la variante exige limites; en los valores, no", {
  casos <- c(
    correo = "contactar a maria.nunez123@correo.uy",
    separado = "beneficiaria: maria.nunez",
    documento = "caja 771.771-01",
    pegado = "correo marianunezsrl@correo.uy",
    sin_tildes = "mailto juan.perez@correo.uy",
    prosa = "La columna contiene valores con un formato distinto"
  )
  valores <- c("Maria Nunez", "77177101", "ACONTI", "UMNACO",
               "Juan P\u00e9rez")
  prosa <- names(casos) == "prosa"
  tapado <- lupa:::.reemplazar_variantes_separadas(
    casos, valores, exigir_limites = prosa
  ) == "[valor protegido]"
  names(tapado) <- names(casos)
  # Lo que la regla existe para atrapar sigue tapado.
  expect_true(tapado[["correo"]])
  expect_true(tapado[["separado"]])
  expect_true(tapado[["documento"]])
  # Fuera de la prosa no hace falta limite: el nombre pegado a otras letras es
  # un valor de una persona. Una refutacion lo encontro publicado cuando la
  # regla de limites valia en todas partes.
  expect_true(tapado[["pegado"]])
  # Y la tilde es cosmetica: antes "juan.perez" se publicaba frente a "Juan
  # Perez" con tilde.
  expect_true(tapado[["sin_tildes"]])
  # En la prosa del paquete, "ACONTI" cruzando "columna contiene" no tapa.
  expect_false(tapado[["prosa"]])
  # Control: la misma prosa como valor si se tapa -la regla fuerte-.
  expect_true(
    lupa:::.reemplazar_variantes_separadas(casos[["prosa"]], valores) ==
      "[valor protegido]"
  )
})

test_that("con muchos nombres protegidos, la prosa de otras columnas se lee", {
  set.seed(7)
  n <- 20000
  silabas <- c("ma", "ri", "a", "na", "lo", "pe", "ro", "sa", "ca", "te", "ni",
               "co", "la", "va", "le", "do", "mo", "be", "tu", "ga", "so", "fe",
               "di", "ne", "ta", "li", "ve", "sin", "con", "tie")
  d <- data.frame(
    primer_apellido = vapply(seq_len(n), function(i) {
      paste(sample(silabas, sample(3:4, 1), TRUE), collapse = "")
    }, ""),
    categoria = sample(c("alta", "media", "baja", " baja", NA), n, TRUE),
    constante = "x",
    stringsAsFactors = FALSE
  )
  h <- hallazgos(suppressWarnings(perfilar(d)))
  otras <- h$columna %in% c("categoria", "constante")
  expect_true(any(otras))
  expect_false(any(h$descripcion[otras] == "[valor protegido]"))
  expect_false(any(h$sugerencia[otras] == "[valor protegido]"))
  # Control: la columna de apellidos sigue protegida.
  p <- suppressWarnings(perfilar(d))
  expect_true(p$datos_personales$proteger[p$datos_personales$columna == "primer_apellido"])
})
