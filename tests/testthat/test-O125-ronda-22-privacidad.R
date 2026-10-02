# Ronda 22: lo que la regla de digitos de la ronda 21 dejaba pasar o tapaba de
# mas, y dos puertas de `perfilar_dbi()`.

.tapa_O125 <- function(texto, protegidos) {
  identical(
    lupa:::.reemplazar_valores_protegidos(texto, protegidos, exigir_limites = FALSE),
    "[valor protegido]"
  )
}

test_that("guiones, espacios e invisibles de Unicode no cortan el numero", {
  for (signo in c(0x2013, 0x2014, 0x2212, 0x2010, 0x2011, 0xFF0D, 0x00AD, 0x200B,
                  0x200E, 0x2007, 0x3000, 0x2024, 0x2044, 0x30FB)) {
    texto <- paste0("tel 2901", intToUtf8(signo), "1234")
    # Antes: el telefono se publicaba entero.
    expect_true(.tapa_O125(texto, "29011234"), info = sprintf("U+%04X", signo))
  }
  expect_true(.tapa_O125("tel 2901\t1234", "29011234"))
})

test_that("un numero con puntos no es un importe si no tiene forma de miles", {
  # Antes: el ultimo signo decidia, y los cinco se publicaban.
  expect_true(.tapa_O125("cel 099.12.34.56", "099123456"))
  expect_true(.tapa_O125("tel 2901.12.34", "29011234"))
  expect_true(.tapa_O125("tel 01.23.45.67.89", "0123456789"))
  expect_true(.tapa_O125("CI 4.123.456.7", "41234567"))
  expect_true(.tapa_O125("doc 3-456-789.0", "34567890"))
})

test_that("una fecha solo se borra con un ano plausible, y la fecha con hora se reconoce", {
  # Antes: "2901-12-12" se borraba como fecha y el telefono se publicaba.
  expect_true(.tapa_O125("tel 2901-12-12", "29011212"))
  expect_true(.tapa_O125("tel 2412/10/15", "24121015"))
  # La fecha con hora protegida, escrita de otra forma.
  for (escrito in c("2024-06-27T21:24:25", "2024-06-27T21:24:25Z",
                    "27/06/2024 21:24:25")) {
    expect_true(.tapa_O125(paste("alta", escrito), "2024-06-27 21:24:25"),
                info = escrito)
  }
})

test_that("una coordenada, una hora con fraccion o una IP no se toman por documento", {
  # Antes: con estos documentos protegidos, las cuatro se tapaban.
  expect_false(.tapa_O125("-33.340517", "33340517"))
  expect_false(.tapa_O125("POINT (-55.946952 -30.019086)", "94695230"))
  expect_false(.tapa_O125("2024-05-17 12:26:58.5412576", "54125763"))
  expect_false(.tapa_O125("206.38.82.140", "38821405"))
  # Y un telefono escrito con un punto se sigue reconociendo.
  expect_true(.tapa_O125("2336.5544", "23365544"))
})

test_that("una cita de la prosa con digitos no ASCII se prueba", {
  digitos <- c(4, 1, 2, 3, 4, 5, 6, 7)
  ancho <- intToUtf8(0xFF10 + digitos)
  texto <- paste0("Volver a perfilar: `aplicabilidad = list(n = ~ p %in% c(\"",
                  ancho, "\", \"Beta\"))`.")
  tapado <- lupa:::.reemplazar_valores_protegidos(texto, "4.123.456-7",
                                                   exigir_limites = TRUE)
  # Antes: la cita salia entera, porque no tenia seis digitos ASCII.
  expect_false(grepl(ancho, tapado, fixed = TRUE))
  expect_match(tapado, "aplicabilidad", fixed = TRUE)
})

test_that("un nombre de columna con tildes no exceptua el valor protegido", {
  valor <- paste0("Jos", intToUtf8(0xE9), " P", intToUtf8(0xE9), "rez")
  # Antes: la columna `jose_perez` dejaba publicado "jose_perez".
  expect_identical(
    lupa:::.reemplazar_valores_protegidos("jose_perez", valor,
                                          intocables = "jose_perez"),
    "[valor protegido]"
  )
})

test_that("el desvio no reconstruye el extremo que el piso tapo", {
  datos <- data.frame(
    documento = rep(c("4.888.999-7", "4.123.456-7", "1.987.654-3"), 100),
    importe = c(rep(c(10, 20, 30), 99), 10, 20, 48889997),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "documento"))
  fila <- perfil$columnas[perfil$columnas$columna == "importe", ]
  expect_true(is.na(fila$maximo))
  # Antes: desvio y n daban 48.890.008, a once del documento.
  expect_true(is.na(fila$desvio))
})

test_that("perfilar_dbi lee como texto un documento en una columna numerica de SQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE t (id INTEGER, cedula NUMERIC, ref TEXT)")
  n <- 2000L
  cedulas <- as.character(sample(10000000:20000000, n))
  cedulas[[n]] <- "5.765.432-1"
  referencias <- sample(c("5.765.432-1", "otro", "mas"), n, TRUE, prob = c(50, 25, 25))
  DBI::dbWriteTable(conexion, "t", data.frame(id = seq_len(n), cedula = cedulas,
                                              ref = referencias),
                    append = TRUE)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(
    conexion, "t", muestra = 200, columnas_personales = "cedula"
  )))
  hojas <- character()
  recorrer <- function(y) {
    if (is.character(y)) hojas <<- c(hojas, y)
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(resultado)
  # Antes: la moda de `ref` y el motivo del maximo publicaban la cedula.
  expect_false(any(grepl("5.765.432-1", hojas, fixed = TRUE)))
})
