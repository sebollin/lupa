# Un nombre de columna es estructura, no un dato: la proteccion no puede
# enmascararlo, y tampoco puede dejar de enmascarar un valor por proteger nombres.
#
# Lo encontro una evaluacion sobre una base real de 4.244.471 filas: la proteccion
# de datos personales enmascaraba tambien los NOMBRES de columna cuando un valor
# protegido aparecia dentro de un nombre. `fecha_nacimiento` salia publicada como
# `fecha_[valor protegido]`, y las dos guardas que comparan los nombres del perfil
# con los de los datos abortaban `planificar_limpieza()` y `analizar()`. Bastaba UN
# valor entre millones.
#
# Y el arreglo tiene dos lados que no se pueden separar. El caso minimo que lo
# reproduce tiene un VALOR -`"documento"`- igual a un NOMBRE de columna
# -`documento`-, asi que "proteger los nombres en todas partes" dejaba de
# enmascarar la moda de esa columna y filtraba el valor. La proteccion va por
# CAMPO, y el campo se reconoce por su contenido.

.o86_datos <- function() {
  data.frame(
    tipo_documento = rep(c("DNI", "PAS"), 6),
    documento = rep("documento", 12),
    distrito = rep(c("A", "B"), each = 6),
    zona = rep(c("Norte", "Sur"), each = 6),
    stringsAsFactors = FALSE
  )
}

test_that("los nombres de columna se publican intactos", {
  datos <- .o86_datos()
  perfil <- suppressWarnings(perfilar(datos))
  # Antes: `tipo_[valor protegido] | [valor protegido] | distrito | zona`.
  expect_identical(as.character(perfil$columnas$columna), names(datos))
  # Y los campos de nombres de las otras tablas: la proteccion no los cambia. Se
  # compara contra el perfil SIN proteger en vez de contra `names(datos)`, porque
  # un hallazgo de nivel tabla -filas duplicadas, por ejemplo- publica `NA` en
  # `columna`, y ese `NA` no es un nombre ni tiene por que serlo. La primera
  # version de esta prueba lo trataba como un nombre enmascarado.
  sin_proteger <- suppressWarnings(
    perfilar(datos, proteger_datos_personales = FALSE)
  )
  expect_identical(
    as.character(perfil$hallazgos$columna),
    as.character(sin_proteger$hallazgos$columna)
  )
  expect_identical(
    as.character(perfil$datos_personales$columna),
    as.character(sin_proteger$datos_personales$columna)
  )
})

test_that("el perfil vuelve a servir para planificar y analizar", {
  datos <- .o86_datos()
  perfil <- suppressWarnings(perfilar(datos))
  # Con dependencias: es la condicion para que la guarda de `planificar_limpieza`
  # compare nombres.
  expect_gt(nrow(perfil$dependencias), 0L)
  expect_no_error(suppressWarnings(planificar_limpieza(perfil, datos)))
  expect_no_error(suppressWarnings(analizar(datos)))
})

test_that("el valor que coincide con un nombre SIGUE enmascarado", {
  # La mitad que hace que el arreglo no sea "proteger los nombres en todas
  # partes": el valor de la columna `documento` es `"documento"`, y es un dato
  # personal. Su moda tiene que seguir tapada.
  datos <- .o86_datos()
  perfil <- suppressWarnings(perfilar(datos))
  fila <- perfil$columnas[perfil$columnas$columna == "documento", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_false(identical(as.character(fila$moda), "documento"))
  expect_match(as.character(fila$moda), "valor protegido", fixed = TRUE)
})

test_that("un nombre que contiene el valor se publica entero", {
  # El caso de produccion: el valor protegido cae DENTRO de un nombre mas largo.
  datos <- data.frame(
    fecha_nacimiento = as.Date("2000-01-01") + 0:11,
    nacimiento = rep("nacimiento", 12),
    grupo = rep(c("a", "b"), 6),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, columnas_personales = "nacimiento"
  ))
  expect_true("fecha_nacimiento" %in% as.character(perfil$columnas$columna))
  expect_false(any(grepl(
    "fecha_[valor protegido]", as.character(perfil$columnas$columna),
    fixed = TRUE
  )))
})

test_that("la proteccion de los valores en texto libre no se afloja", {
  # El control del lado que el arreglo no debe tocar: un valor personal adentro
  # de un campo que NO es de nombres se sigue tapando.
  x <- data.frame(
    columna = c("documento", "zona"),
    evidencia = c("se vio el valor documento en la fila 3", "nada"),
    stringsAsFactors = FALSE
  )
  protegido <- lupa:::.proteger_textos_salida(
    x, "documento", intocables = c("documento", "zona")
  )
  # El campo de nombres queda intacto...
  expect_identical(protegido$columna, x$columna)
  # ...y el de texto libre se enmascara igual que antes.
  expect_false(grepl("valor documento", protegido$evidencia[[1L]], fixed = TRUE))
  expect_match(protegido$evidencia[[1L]], "valor protegido", fixed = TRUE)
})

test_that("un campo de nombres se reconoce por su contenido, no por su nombre", {
  nombres <- c("a", "b", "c, d", "otra")
  es <- lupa:::.es_columna_de_nombres
  # Todos los valores son nombres de la entrada.
  expect_true(es(c("a", "b", "a"), nombres))
  # Referencias a varias columnas, con los separadores del paquete.
  expect_true(es(c("a, b", "a+b", "a | otra"), nombres))
  # Un nombre que lleva una coma se reconoce ENTERO antes de partirlo.
  expect_true(es("c, d", nombres))
  # Si un solo valor no es un nombre, el campo es de valores y se enmascara.
  expect_false(es(c("a", "documento"), nombres))
  # Sin nombres de referencia no se saltea nada: el valor por omision es cerrado.
  expect_false(es(c("a", "b"), character()))
  # Un campo vacio no es de nombres.
  expect_false(es(c(NA, ""), nombres))
})
