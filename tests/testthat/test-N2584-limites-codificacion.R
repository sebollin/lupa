testthat::skip_on_cran()

# Los fixtures se arman con intToUtf8(): el archivo queda en ASCII.
u <- function(...) intToUtf8(c(...))
columna_texto <- function(perfil) {
  perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
}
perfilar_texto <- function(datos) {
  perfilar(datos, fecha = "2026-01-15", muestra = Inf)
}
limpiar_por_omision <- function(datos) {
  perfil <- perfilar_texto(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicar(plan, datos)$datos$texto
}
sin_espacios <- function(x) gsub(paste0("[ ", u(0xA0L), "]"), "", enc2utf8(x))

test_that("un espacio duro delante de una palabra rota no cambia lo que se repara", {
  # "Sanliurfa" con la ese con cedilla y la i sin punto, leida como Windows-1252.
  rota <- paste0(u(0xC5L, 0x17EL), "anl", u(0xC4L, 0xB1L), "urfa")
  bien <- paste0(u(0x15EL), "anl", u(0x131L), "urfa")
  delante <- list(
    nada = "", espacio = " ", duro = u(0xA0L), blando = u(0xADL),
    dos_duros = u(0xA0L, 0xA0L), tras_ascii = paste0("Av.", u(0xA0L))
  )
  for (nombre in names(delante)) {
    texto <- paste0(delante[[nombre]], rota)
    testthat::expect_identical(
      .ftfy_reparar_uno(texto)$texto, paste0(delante[[nombre]], bien),
      info = nombre
    )
  }
  # Un espacio duro agregado despues de la mala conversion, detras de una letra
  # ya rota: la celda se repara entera, sin volver a leer como bytes la letra
  # reparada.
  peru <- paste0("PER", u(0xC3L, 0x9AL), u(0xA0L), "y", u(0xA0L),
                 u(0xC3L, 0x81L), "rea")
  testthat::expect_identical(
    .ftfy_reparar_uno(peru)$texto,
    paste0("PER", u(0xDAL), u(0xA0L), "y", u(0xA0L), u(0xC1L), "rea")
  )
  # Con el plan por omision la celda queda como con un espacio comun delante, y
  # al volver a perfilar ya no hay mojibake adentro.
  for (prefijo in c(" ", u(0xA0L))) {
    datos <- data.frame(texto = paste0(prefijo, rota), stringsAsFactors = FALSE)
    salida <- limpiar_por_omision(datos)
    testthat::expect_identical(salida, bien)
    final <- columna_texto(perfilar_texto(
      data.frame(texto = salida, stringsAsFactors = FALSE)
    ))
    testthat::expect_equal(final$n_codificacion_rota[[1L]], 0L)
  }
})

test_that("el portugues en mayusculas antes de un espacio duro o un signo no es mojibake", {
  irma <- paste0("IRM", u(0xC3L))
  textos <- c(
    paste0(irma, u(0xA0L), "DE JESUS"), paste0(u(0xABL), irma, u(0xBBL)),
    paste0(irma, u(0xA0L)), paste0("MA", u(0xC7L, 0xC3L), u(0xB4L)),
    paste0("Rua da ", irma, u(0xA0L), "DE")
  )
  for (texto in textos) {
    datos <- data.frame(texto = c(texto, "otro"), stringsAsFactors = FALSE)
    fila <- columna_texto(perfilar_texto(datos))
    testthat::expect_equal(fila$n_codificacion_rota[[1L]], 0L, info = texto)
    # El plan puede normalizar el espacio duro; las letras quedan como estaban.
    testthat::expect_identical(
      sin_espacios(limpiar_por_omision(datos)[[1L]]), sin_espacios(texto),
      info = texto
    )
  }
  # Despues de reparar una letra vecina, la pasada siguiente tampoco lo toca.
  maca <- paste0("MA", u(0xC3L, 0x87L, 0xC3L, 0x83L), u(0xA0L), "12")
  testthat::expect_identical(
    .ftfy_reparar_uno(maca)$texto,
    paste0("MA", u(0xC7L, 0xC3L), u(0xA0L), "12")
  )
})

test_that("las minusculas rotas siguen reconociendose donde son mojibake", {
  casos <- list(
    c(paste0("XIX", u(0xC3L, 0xA8L), "me"), paste0("XIX", u(0xE8L), "me")),
    c(paste0("D", u(0xC3L, 0xA9L), "fense"), paste0("D", u(0xE9L), "fense")),
    c(paste0("B", u(0xC3L, 0xA0L)), paste0("B", u(0xE0L))),
    c(paste0("voil", u(0xC3L, 0xA0L)), paste0("voil", u(0xE0L))),
    c(paste0("STRA", u(0xC3L, 0x178L), "E"), paste0("STRA", u(0xDFL), "E")),
    c(paste0("AV", u(0xC3L, 0x201DL)), paste0("AV", u(0xD4L))),
    c(paste0("Peque", u(0xC3L, 0xB1L), "a ", u(0xC2L, 0xBAL)),
      paste0("Peque", u(0xF1L), "a ", u(0xBAL)))
  )
  for (caso in casos) {
    datos <- data.frame(texto = caso[[1L]], stringsAsFactors = FALSE)
    fila <- columna_texto(perfilar_texto(datos))
    testthat::expect_equal(fila$n_codificacion_rota[[1L]], 1L, info = caso[[2L]])
    testthat::expect_identical(limpiar_por_omision(datos), caso[[2L]])
  }
})

test_that("un ordinal delante de una comilla no es mojibake", {
  textos <- c(
    paste0("3", u(0xBAL, 0x201DL)), paste0("1", u(0xBAL, 0xBBL)),
    paste0("N.", u(0xBAL, 0xBBL)), paste0("2", u(0xAAL, 0x201DL)),
    paste0("el 3", u(0xBAL, 0x201DL), " piso")
  )
  datos <- data.frame(texto = textos, stringsAsFactors = FALSE)
  fila <- columna_texto(perfilar_texto(datos))
  testthat::expect_equal(fila$n_codificacion_rota[[1L]], 0L)
  testthat::expect_identical(limpiar_por_omision(datos), textos)
  # En una columna latin1, el espacio duro de la misma celda se vuelve a
  # normalizar: la celda ya no queda protegida como si estuviera rota.
  jose <- paste0("JOS", u(0xC9L), u(0xA0L), "P", u(0xC9L), "REZ, 3",
                 u(0xBAL, 0xBBL))
  datos <- data.frame(texto = c(iconv(jose, "UTF-8", "latin1"), "otro"),
                      stringsAsFactors = FALSE)
  fila <- columna_texto(perfilar_texto(datos))
  testthat::expect_equal(fila$n_codificacion_rota[[1L]], 0L)
  testthat::expect_identical(
    enc2utf8(limpiar_por_omision(datos)[[1L]]),
    gsub(u(0xA0L), " ", jose, fixed = TRUE)
  )
})

test_that("la u con breve rota detras de una minuscula se reconoce y se repara", {
  bien <- c(paste0("a", u(0x16DL), "to"), paste0("anka", u(0x16DL)),
            paste0("kvaza", u(0x16DL), " ne"), paste0("Kra", u(0x12DL)))
  # Los bytes UTF-8 leidos como latin-1.
  rotas <- vapply(bien, function(x) {
    intToUtf8(as.integer(charToRaw(enc2utf8(x))))
  }, character(1L), USE.NAMES = FALSE)
  datos <- data.frame(texto = rotas, stringsAsFactors = FALSE)
  fila <- columna_texto(perfilar_texto(datos))
  testthat::expect_equal(fila$n_codificacion_rota[[1L]], 4L)
  testthat::expect_identical(limpiar_por_omision(datos), bien)
  # Detras de una mayuscula, el mismo par es una palabra en mayusculas cortada
  # por un guion blando legitimo.
  legitimos <- c(paste0("P", u(0xC5L, 0xADL), "SKE"),
                 paste0("SK", u(0xC5L, 0xADL), "NE"),
                 paste0("P", u(0xC5L, 0xADL), "ker"),
                 paste0(u(0xC5L, 0xADL), "ker"))
  datos <- data.frame(texto = legitimos, stringsAsFactors = FALSE)
  fila <- columna_texto(perfilar_texto(datos))
  testthat::expect_equal(fila$n_codificacion_rota[[1L]], 0L)
})

test_that("el espacio duro detras de una A con anillo sigue siendo texto", {
  # La A con anillo + espacio duro + palabra es tambien noruego o sueco bien
  # escrito, y
  # ninguna regla lo distingue de la S con caron rota: no se lo toca.
  textos <- c(paste0(u(0xC5L, 0xA0L), "i Lofoten"),
              paste0("P", u(0xC5L, 0xA0L), "TUR"),
              paste0("KYL", u(0xC4L, 0xA0L), "TALO"))
  datos <- data.frame(texto = textos, stringsAsFactors = FALSE)
  fila <- columna_texto(perfilar_texto(datos))
  testthat::expect_equal(fila$n_codificacion_rota[[1L]], 0L)
  testthat::expect_identical(
    sin_espacios(limpiar_por_omision(datos)), sin_espacios(textos)
  )
})
