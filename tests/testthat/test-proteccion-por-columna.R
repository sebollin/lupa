test_that("un centinela compartido se publica en la columna que no es personal", {
  # `S/D` es un centinela de `cedula` y a la vez el "sin dato" de `sexo`. Con
  # enmascarado global por valor, `sexo` publicaba `[valor protegido]` sin tener
  # un solo dato personal, y el lector no podia distinguir eso de una columna
  # que si los tiene.
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    monto = c(100, 200, 100, 300, 100),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)

  expect_true("S/D" %in% perfil$patrones$sexo$ejemplos)
  expect_identical(
    as.character(perfil$columnas$moda[perfil$columnas$columna == "sexo"]), "F"
  )

  # Y la columna protegida sigue tapada, que es lo que no cambia.
  expect_true(all(perfil$patrones$cedula$ejemplos == "[valor protegido]"))
  expect_identical(
    as.character(perfil$columnas$moda[perfil$columnas$columna == "cedula"]),
    "[valor protegido]"
  )
})

test_that("la evidencia de un hallazgo sobre una columna no personal se publica", {
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  sobre_sexo <- perfil$hallazgos[
    !is.na(perfil$hallazgos$columna) & perfil$hallazgos$columna == "sexo",
  ]

  expect_gt(nrow(sobre_sexo), 0L)
  expect_false(any(grepl(
    "[valor protegido]", as.character(sobre_sexo$evidencia), fixed = TRUE
  )))
})

test_that("un hallazgo sin columna atribuible sigue enmascarado", {
  # Una fila duplicada muestra filas ENTERAS de la tabla, que incluyen la
  # columna protegida. No hay a quien atribuirle esa fila, asi que el barrido
  # general la sigue gobernando: es el borde que la politica por columna deja
  # explicitamente afuera.
  datos <- data.frame(
    cedula = c("48123456", "48123456", "51987654"),
    monto = c(100, 100, 200),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  sin_columna <- perfil$hallazgos[is.na(perfil$hallazgos$columna), ]

  skip_if(nrow(sin_columna) == 0L, "esta tabla no produjo hallazgos de fila")
  expect_false(any(grepl(
    "48123456", as.character(sin_columna$evidencia), fixed = TRUE
  )))
})

test_that("ningun valor identificante se publica en otra columna", {
  # El limite de la politica por columna: si un valor de una columna protegida
  # aparece tambien en una que no lo esta, por columna se publicaria. Aca se
  # comprueba que eso no ocurre con los valores que identifican, porque el paso
  # por columna no es lo unico que protege: los componentes sin columna
  # atribuible siguen bajo el barrido general.
  datos <- data.frame(
    correo = c("juan.perez@x.uy", "ana@y.uy", "b@z.uy", "c@w.uy"),
    nota = c("sin novedad", "sin novedad", "revisar", "revisar"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  protegidas <- perfil$datos_personales$columna[perfil$datos_personales$proteger]
  skip_if(length(protegidas) == 0L, "el correo no se clasifico como protegido")

  textos <- unlist(lapply(perfil, function(x) {
    if (is.list(x)) unlist(lapply(x, as.character)) else as.character(x)
  }))
  expect_false(any(grepl("juan.perez@x.uy", textos, fixed = TRUE)))
})
