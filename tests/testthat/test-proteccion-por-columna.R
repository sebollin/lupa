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

test_that("un valor identificante no se publica aunque viva en otra columna", {
  # El piso de la politica por columna. Sin el, una columna que el clasificador
  # no marco publicaba los valores de la protegida con solo repetirlos: medido,
  # tres de cuatro documentos de ocho digitos salian por una copia llamada
  # `codigo_operacion`, por un texto libre que los contenia y por una copia
  # guardada como numero.
  documentos <- c("48123456", "51987654", "39112233", "12345678", "48123456")
  textos_de <- function(x, acumulado = character()) {
    if (is.list(x)) {
      for (parte in x) acumulado <- textos_de(parte, acumulado)
      return(acumulado)
    }
    if (is.character(x) || is.factor(x)) {
      acumulado <- c(acumulado, as.character(x))
    }
    acumulado
  }
  copias <- list(
    codigo_operacion = documentos,
    nota = paste0("ref ", documentos),
    clave_externa = as.numeric(documentos)
  )
  for (nombre in names(copias)) {
    datos <- data.frame(documento = documentos, stringsAsFactors = FALSE)
    datos[[nombre]] <- copias[[nombre]]
    datos$monto <- c(100, 200, 100, 300, 100)
    perfil <- perfilar(datos)

    publicado <- textos_de(perfil)
    publicado <- publicado[!is.na(publicado)]
    for (documento in unique(documentos)) {
      expect_false(
        any(grepl(documento, publicado, fixed = TRUE)),
        info = paste(nombre, documento)
      )
    }
  }
})

test_that("el piso no tapa el vocabulario corto que se comparte", {
  # Control: el piso protege lo que identifica y NO deshace la politica por
  # columna. `"S/D"` tiene tres caracteres, no identifica a nadie y sigue
  # publicandose donde describe a una columna que no es personal.
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)

  expect_true("S/D" %in% perfil$patrones$sexo$ejemplos)
  expect_true(all(perfil$patrones$cedula$ejemplos == "[valor protegido]"))
})

test_that("el piso alcanza a los campos numericos, no solo al texto", {
  # Lo encontro una refutacion externa y el piso no lo cubria: `copia` se
  # clasifica `documento_identidad` con poder discriminante debil, asi que no se
  # protege, y publicaba en `minimo`, `maximo`, `media` y `mediana` los
  # documentos de `documento`, que en el mismo objeto sale enmascarada. Sobre
  # doce filas, `minimo` y `maximo` son dos documentos exactos.
  datos <- data.frame(
    documento = sprintf("707771%02d", 1:12), stringsAsFactors = FALSE
  )
  datos$copia <- datos$documento
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  i <- match("copia", perfil$columnas$columna)
  expect_true(is.na(perfil$columnas$minimo[[i]]))
  expect_true(is.na(perfil$columnas$maximo[[i]]))
  expect_true(is.na(perfil$columnas$media[[i]]))
  expect_true(is.na(perfil$columnas$mediana[[i]]))
})

test_that("el piso numerico no toca un numero que no es un valor protegido", {
  # Control: el piso tapa un numero cuya representacion ES un valor
  # identificante de una columna protegida, y ningun otro. Una columna de
  # importes conserva sus estadisticos enteros.
  datos <- data.frame(
    documento = sprintf("707771%02d", 1:12),
    monto = c(1500, 2300, 1500, 4800, 2300, 990, 1500, 7200, 3100, 990, 4800, 2300),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  i <- match("monto", perfil$columnas$columna)
  expect_equal(perfil$columnas$minimo[[i]], 990)
  expect_equal(perfil$columnas$maximo[[i]], 7200)
  expect_false(is.na(perfil$columnas$media[[i]]))
})
