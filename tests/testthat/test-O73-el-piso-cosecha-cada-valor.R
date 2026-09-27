# El piso de datos personales se quedaba sin agujas y el informe publicaba tres
# de cuatro cedulas CON su proteccion puesta.
#
# `reportar()` promete aplicar "su propia proteccion predeterminada" sobre un
# perfil que quien llama decidio conservar abierto -que es un camino documentado-.
# La cosecha de agujas tomaba la celda `ejemplos` completa, y esa celda trae hasta
# TRES valores unidos con " | ": la aguja era la cadena unida, que no existe en
# ningun otro lado, asi que el reemplazo no encontraba nada. Con documentos
# numericos el mismo camino funciona -las agujas llegan por `minimo` y `maximo`-, y
# por eso las pruebas que reportaban un perfil abierto pasaban sin ver la fuga.
#
# Las mitades de control son la mitad de la prueba: un piso que tapa de mas
# silencia el informe, que es el otro modo de fallar.

cedulas_o73 <- c("4.123.456-7", "1.987.654-3", "3.456.789-0", "5.678.901-2")

html_de <- function(objeto) {
  # `tempfile()` de base y no el ayudante de withr, que no esta en `Suggests`:
  # usar un paquete sin declarar pasa la suite -testthat lo trae- y rompe el check
  # con "import not declared". Ya paso dos veces en este proyecto, y la segunda fue
  # por nombrarlo en un COMENTARIO con la forma de dos puntos, que es la que el
  # check busca en el texto.
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(objeto, archivo = archivo)
  paste(readLines(archivo, warn = FALSE), collapse = "\n")
}

cuantas_cedulas <- function(html) {
  sum(vapply(cedulas_o73, function(v) {
    grepl(v, html, fixed = TRUE, useBytes = TRUE)
  }, logical(1L)))
}

datos_o73 <- function() {
  data.frame(
    cedula = cedulas_o73,
    obs = c("contacto 4.123.456-7", "nota 1.987.654-3",
            "ref 3.456.789-0", "sin datos"),
    stringsAsFactors = FALSE
  )
}

test_that("el informe no publica el documento que viaja en un texto libre", {
  datos <- datos_o73()
  # El perfil abierto es un camino documentado: `reportar()` dice que aplica
  # ADEMAS su propia proteccion, y aca esa proteccion esta en su omision.
  abierto <- perfilar(datos, proteger_datos_personales = FALSE)
  expect_identical(cuantas_cedulas(html_de(abierto)), 0L)
  # Y por el camino por omision tampoco, que ya funcionaba: es el control de que
  # la prueba no esta midiendo otra cosa.
  expect_identical(cuantas_cedulas(html_de(perfilar(datos))), 0L)
})

test_that("la cosecha de agujas separa los valores unidos de un ejemplo", {
  # La causa, medida en el sitio: una celda con tres valores unidos tiene que
  # entregar tres agujas ademas de la cadena entera.
  unidos <- paste(c("4.123.456-7", "1.987.654-3"), collapse = " | ")
  partes <- unlist(
    strsplit(unidos, " | ", fixed = TRUE, useBytes = TRUE), use.names = FALSE
  )
  expect_identical(partes, c("4.123.456-7", "1.987.654-3"))
  # Y el separador esta escrito en un solo lugar, para que el que une y el que
  # separa no puedan discrepar.
  expect_identical(lupa:::.SEPARADOR_EJEMPLOS, " | ")
})

test_that("una variante que solo cambia la caja queda enmascarada", {
  datos <- data.frame(
    nombre = c("Maria Nunez de Castro", "Jose Angel Pereira",
               "Ana Lucia Ferreyra", "Maria Nunez de Castro"),
    notas = c("hablo con maria nunez de castro", "x", "y", "z"),
    stringsAsFactors = FALSE
  )
  html <- html_de(perfilar(datos))

  expect_false(grepl("hablo con maria nunez de castro", html, fixed = TRUE))
  expect_false(grepl("Maria Nunez de Castro", html, fixed = TRUE))
})

test_that("un fragmento del documento queda enmascarado", {
  # La cedula sin su verificador: "4.123.456" normaliza a "4123456", que NO
  # contiene a "41234567", asi que ninguna de las dos puntas de la regla de
  # separadores lo ve. Las corridas de digitos se comparan en las dos direcciones.
  datos <- data.frame(
    cedula = cedulas_o73,
    obs = c("caja 4.123.456", "caja 1.987.654", "caja 3.456.789", "sin datos"),
    stringsAsFactors = FALSE
  )
  html <- html_de(perfilar(datos))

  expect_false(grepl("caja 4.123.456", html, fixed = TRUE))
  expect_false(grepl("caja 1.987.654", html, fixed = TRUE))
})

test_that("el piso no tapa un numero ajeno de la misma longitud", {
  # Mitad de control. Un expediente que no es un documento personal se sigue
  # publicando: si esta prueba se pone roja, el arreglo paso a silenciar el
  # informe en vez de proteger un dato.
  datos <- data.frame(
    cedula = cedulas_o73,
    expediente = c("EXP-770011", "EXP-770012", "EXP-770013", "EXP-770014"),
    stringsAsFactors = FALSE
  )
  html <- html_de(perfilar(datos))

  expect_true(grepl("770011", html, fixed = TRUE))
  expect_identical(cuantas_cedulas(html), 0L)
})

test_that("el piso no tapa una palabra que comparte un tramo con un apellido", {
  # El otro control, y el que fija el limite ELEGIDO: la regla de corridas se
  # aplica a digitos y no a texto justamente para no tapar una palabra corriente
  # por compartir seis caracteres con un apellido.
  datos <- data.frame(
    nombre = c("Maria Nunez de Castro", "Jose Angel Pereira",
               "Ana Lucia Ferreyra", "Maria Nunez de Castro"),
    lugar = c("castro urdiales", "montevideo", "castro urdiales", "salto"),
    stringsAsFactors = FALSE
  )
  html <- html_de(perfilar(datos))

  expect_true(grepl("castro urdiales", html, fixed = TRUE))
  expect_false(grepl("Maria Nunez de Castro", html, fixed = TRUE))
})

test_that("una tabla sin columnas personales no se enmascara en ninguna celda", {
  datos <- data.frame(
    monto = c(100.5, 200.25, 300, 4567.75),
    ciudad = c("salto", "rivera", "salto", "artigas"),
    stringsAsFactors = FALSE
  )
  html <- html_de(perfilar(datos))

  expect_false(grepl("[valor protegido]", html, fixed = TRUE))
  expect_true(grepl("salto", html, fixed = TRUE))
})

test_that("el plegado de caja no aborta sobre bytes que no son UTF-8 validos", {
  # `toupper()` ABORTA -"invalid multibyte string"- sobre una cadena asi, y este
  # paquete trabaja justamente con codificaciones rotas: el plegado usa
  # `perl = TRUE, useBytes = TRUE` sobre el ASCII por eso. La prueba existe porque
  # la primera version de este arreglo podia usar `toupper()`.
  invalido <- rawToChar(as.raw(c(0x61, 0xE9, 0x62)))
  expect_no_error(
    lupa:::.reemplazar_variantes_separadas(
      c(invalido, "4.123.456-7"), "41234567"
    )
  )
  salida <- lupa:::.reemplazar_variantes_separadas(
    c(invalido, "4.123.456-7"), "41234567"
  )
  expect_identical(salida[[2L]], "[valor protegido]")
  expect_identical(salida[[1L]], invalido)
})
