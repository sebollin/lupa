# Ronda 21: la regla de digitos -lo que la ronda 20 aflojo y lo que tapaba de
# mas-, los campos numericos, y la consulta de `perfilar_dbi()` fuera de la
# muestra.

.hojas_O122 <- function(x) {
  hojas <- character()
  recorrer <- function(y) {
    for (a in setdiff(names(attributes(y)), c("names", "class", "row.names"))) {
      recorrer(attr(y, a, exact = TRUE))
    }
    if (is.factor(y)) hojas <<- c(hojas, levels(y))
    else if (is.character(y)) hojas <<- c(hojas, y)
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(x)
  hojas[!is.na(hojas)]
}

.numeros_O122 <- function(x) {
  numeros <- numeric()
  recorrer <- function(y) {
    if (is.numeric(y) && !is.list(y)) numeros <<- c(numeros, as.numeric(y))
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(x)
  numeros[!is.na(numeros)]
}

.tapa_O122 <- function(texto, protegidos) {
  identical(
    lupa:::.reemplazar_valores_protegidos(texto, protegidos, exigir_limites = FALSE),
    "[valor protegido]"
  )
}

test_that("un numero protegido se reconoce con cualquier agrupacion", {
  # Antes, todos estos se publicaban enteros.
  expect_true(.tapa_O122("llamar al 2901 1234", "29011234"))
  expect_true(.tapa_O122("cel 099 12 34 56", "099123456"))
  expect_true(.tapa_O122("(2) 487 1234", "24871234"))
  expect_true(.tapa_O122("4111 1111 1111 1111", "4111111111111111"))
  expect_true(.tapa_O122("CI 4 . 123 . 456-7", "4.123.456-7"))
  expect_true(.tapa_O122("CI 3:456:789-0", "34567890"))
  expect_true(.tapa_O122(paste0("CI 1", intToUtf8(0xb7), "987", intToUtf8(0xb7), "654-3"),
                         "19876543"))
  # Y de punta a punta, una columna por escritura.
  protegidos <- c("29011234", "4111111111111111", "123456789", "0049123456789012")
  escritos <- c("2901 1234", "4111 1111 1111 1111", "123 45 6789", "0049 1234 5678 9012")
  n <- 40L
  datos <- data.frame(id = seq_len(n))
  datos$identificador <- rep(protegidos, length.out = n)
  for (i in seq_along(escritos)) {
    datos[[paste0("c", i)]] <- rep(c(escritos[[i]], "otro valor", "tercero"),
                                   length.out = n)
  }
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "identificador"))
  hojas <- .hojas_O122(perfil)
  for (escrito in escritos) {
    expect_false(any(grepl(escrito, hojas, fixed = TRUE)), info = escrito)
  }
})

test_that("el documento entero se reconoce redondo, con ceros o en otra escritura", {
  # La cedula 5.000.000-0 tiene verificador valido: el filtro de redondos era
  # para el parcial y descartaba tambien el documento entero.
  expect_true(.tapa_O122("CI 50000000", "5.000.000-0"))
  # Un cero a la izquierda no cambia el numero.
  expect_true(.tapa_O122("041234567", "4.123.456-7"))
  expect_true(.tapa_O122("ref 01234567", "1.234.567"))
  # Digitos arabigo-indicos, persas y devanagari.
  digitos <- c(4, 1, 2, 3, 4, 5, 6, 7)
  for (cero in c(0x0660, 0x06F0, 0x0966)) {
    expect_true(.tapa_O122(intToUtf8(cero + digitos), "41234567"), info = cero)
  }
})

test_that("el documento sin su primer digito se reconoce tras un comodin, tambien con cero", {
  # Antes se descartaba todo parcial que empezaba con cero.
  expect_true(.tapa_O122("CI *.012.345-8", "5.012.345-8"))
  expect_true(.tapa_O122("CI *0123458", "5.012.345-8"))
  expect_true(.tapa_O122("CI X.012.345-8", "5.012.345-8"))
})

test_that("la parte decimal, una hora o una lista no se tapan como documento", {
  # Antes: la parte decimal coincidia con un documento sin su primer digito, y el
  # entero de un monto con decimales, con uno sin su verificador.
  expect_false(.tapa_O122("-30.1078721", "51078721"))
  expect_false(.tapa_O122("p=0.6626367", "56626367"))
  expect_false(.tapa_O122("5763464.51", "57634645"))
  expect_false(.tapa_O122("12:34:56", "123456"))
  expect_false(.tapa_O122("67, 20, 855", "67208551"))
  expect_false(.tapa_O122("367277.602", "36727760"))
  # Y lo que la regla de la ronda 20 ya no tapaba, sigue sin taparse.
  expect_false(.tapa_O122("N/A (49710); - (49576)", "49710495"))
  expect_false(.tapa_O122("2003-05-17", "20030517"))
  expect_false(.tapa_O122("1.000.000", "10000003"))
})

test_that("en la sugerencia se tapa el numero o la cita, no la frase", {
  set.seed(22)
  n <- 150L
  documentos <- c(48889997, 41234567, 19876543)
  escribir <- function(x) {
    s <- sprintf("%08d", x)
    paste0(substr(s, 1, 1), ".", substr(s, 2, 4), ".", substr(s, 5, 7), "-",
           substr(s, 8, 8))
  }
  # El corte numerico, sin comillas: era la cedula protegida.
  datos <- data.frame(id = seq_len(n))
  datos$documento <- escribir(rep(documentos, length.out = n))
  datos$nro_tramite <- sample(c(48889990:48889999, 30000000:30000100), n, TRUE)
  datos$nro_tramite[1:30] <- 48889997
  datos$observado <- ifelse(datos$nro_tramite >= 48889997, NA, round(runif(n), 2))
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "documento"))
  hojas <- .hojas_O122(perfil)
  expect_false(any(grepl("48889997", hojas, fixed = TRUE)))
  # Y el nivel citado detras de una comilla suelta en el nombre de la columna.
  for (nombre in c("det_txt", "det\"txt")) {
    datos <- data.frame(id = seq_len(n))
    datos$documento <- escribir(rep(documentos, length.out = n))
    datos[[nombre]] <- sample(c("48889997", "ALFA", "BETA"), n, TRUE)
    datos$observado <- ifelse(datos[[nombre]] == "BETA", NA, round(runif(n), 2))
    perfil <- suppressWarnings(perfilar(datos, columnas_personales = "documento"))
    hallazgos <- perfil$hallazgos
    sugerencia <- hallazgos$sugerencia[
      hallazgos$tipo_hallazgo == "posible_ausencia_estructural"
    ]
    expect_length(sugerencia, 1L)
    expect_false(any(grepl("48889997", .hojas_O122(perfil), fixed = TRUE)),
                 info = nombre)
    # Antes, con `det_txt`, la sugerencia entera era "[valor protegido]".
    expect_match(sugerencia, "aplicabilidad", fixed = TRUE, info = nombre)
    expect_match(sugerencia, "[valor protegido]", fixed = TRUE, info = nombre)
  }
})

test_that("un campo numerico que es un documento escrito se tapa, y su rango tambien", {
  datos <- data.frame(
    documento = rep(c("5.432.198-6", "4.123.456-7", "1.987.654-3"), 10),
    cajas = c(8, 12, 30, rep(15, 26), 54321986),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "documento"))
  fila <- perfil$columnas[perfil$columnas$columna == "cajas", ]
  # Antes: `maximo` = 54321986, el documento sin sus separadores.
  expect_false(54321986 %in% .numeros_O122(perfil))
  # Y el maximo tapado no se reconstruye con el minimo y las posiciones.
  reconstruido <- fila$minimo + fila$n_posiciones_secuencia_entera - 1
  expect_true(is.na(reconstruido) || reconstruido != 54321986)
})

test_that("una columna de p-valores no se clasifica como documento", {
  set.seed(24)
  datos <- data.frame(p_txt = sprintf("%.7f", runif(300, 0.1, 0.9)),
                      stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos))
  clasificacion <- perfil$datos_personales
  tipo <- as.character(clasificacion$tipo[clasificacion$columna == "p_txt"])
  # Antes: documento_identidad por "forma de documento dominante".
  expect_false(identical(tipo, "documento_identidad"))
  # Y la cedula escrita con sus dos puntos de miles se sigue reconociendo.
  cedulas <- data.frame(ci = sprintf("%d.%03d.%03d-%d", sample(1:6, 300, TRUE),
                                     sample(0:999, 300, TRUE),
                                     sample(0:999, 300, TRUE),
                                     sample(0:9, 300, TRUE)),
                        stringsAsFactors = FALSE)
  clasificacion <- suppressWarnings(perfilar(cedulas))$datos_personales
  expect_identical(as.character(clasificacion$tipo[clasificacion$columna == "ci"]),
                   "documento_identidad")
})

test_that("perfilar_dbi tapa un valor que esta solo fuera de la muestra, en cualquier forma", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(17)
  n <- 3000L
  datos <- data.frame(id = seq_len(n))
  datos$titular <- sprintf("Persona %05d", sample(1:99999, n))
  datos$titular[[n]] <- "Juan Perez Rodriguez"
  # La cedula guardada como ENTERO, con el valor protegido a mitad de rango.
  datos$cedula <- sample(10000000:20000000, n)
  datos$cedula[[n]] <- 15000017L
  datos$referente <- sample(
    c("Marta Lopez Diaz", "Pedro Gomez Sanz", "JUAN PEREZ RODRIGUEZ"), n, TRUE,
    prob = c(30, 20, 50)
  )
  datos$ref_txt <- sample(c("15000017", "otro", "mas"), n, TRUE, prob = c(50, 25, 25))
  datos$cajas <- sample(1:1000, n, TRUE)
  datos$cajas[1:3] <- 15000017L
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(
    conexion, "t", muestra = 300,
    columnas_personales = c("titular", "cedula")
  )))
  hojas <- tolower(.hojas_O122(resultado))
  # Antes: la caja distinta, la cedula entera y el maximo numerico se publicaban.
  expect_false(any(grepl("perez rodriguez", hojas, fixed = TRUE)))
  expect_false(any(grepl("15000017", hojas, fixed = TRUE)))
  expect_false(15000017 %in% .numeros_O122(resultado))
})

test_that("perfilar_dbi cierra y lo declara cuando no puede traer los valores", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(18)
  n <- 2000L
  datos <- data.frame(id = seq_len(n))
  datos$titular <- sprintf("Persona %05d", sample(1:99999, n))
  datos$titular[[n]] <- "Juan Perez Rodriguez"
  datos$referente <- sample(c("Marta Lopez Diaz", "Juan Perez Rodriguez"), n, TRUE)
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)
  testthat::local_mocked_bindings(.MAXIMO_VALORES_PROTEGIDOS_DBI = 10L)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(
    conexion, "t", muestra = 200, columnas_personales = "titular"
  )))
  hojas <- tolower(.hojas_O122(resultado))
  expect_false(any(grepl("perez rodriguez", hojas, fixed = TRUE)))
  expect_match(resultado$resumen_tabla$meta$proteccion_personal$fuera_de_muestra,
               "No verificada", fixed = TRUE)
})
