# Ronda 23: lo que la ronda 22 aflojo en la proteccion, y lo que todavia
# tapaba de mas.

.tapa_O127 <- function(texto, protegido, escrito = texto) {
  salida <- lupa:::.reemplazar_valores_protegidos(texto, protegido)
  !grepl(escrito, salida, fixed = TRUE)
}

.intacto_O127 <- function(texto, protegido) {
  identical(lupa:::.reemplazar_valores_protegidos(texto, protegido), texto)
}

test_that("un telefono guardado con forma de fecha no sale del piso", {
  # Antes: el piso aceptaba cualquier ano y "2901-12-12" no era un valor
  # protegido.
  expect_true(.tapa_O127("llamar al 2901-12-12", "2901-12-12", "2901-12-12"))
  expect_true(.tapa_O127("tel 2101-12-12", "21011212", "2101-12-12"))
  datos <- data.frame(
    telefono = rep(c("2901-12-12", "2600-11-30", "4732-10-15"), 20),
    nota = rep(c("llamar al 2901-12-12", "sin novedad", "reclamo"), 20),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "telefono"))
  hojas <- unlist(list(perfil$columnas$moda, perfil$patrones$nota$ejemplos))
  expect_false(any(grepl("2901-12-12", hojas, fixed = TRUE)))
})

test_that("un nombre de columna dentro de un valor protegido no lo exceptua", {
  correo <- "juan_perez@empresa.com.uy"
  salida <- lupa:::.reemplazar_valores_protegidos(
    paste("contacto", correo), correo, intocables = "juan_perez"
  )
  # Antes: el nombre se apartaba del texto y no del valor, y el correo salia.
  expect_false(grepl("juan_perez@", salida, fixed = TRUE))
})

test_that("un documento con forma de IP o de decimal se reconoce entero", {
  # Antes: la IP se borraba antes de buscar, y el decimal "inequivoco" no se
  # pegaba al resto.
  expect_true(.tapa_O127("tel 29.10.12.34", "29101234"))
  expect_true(.tapa_O127("CI 1.234.123.4", "12341234"))
  expect_true(.tapa_O127("(555) 123.4567", "5551234567"))
  expect_true(.tapa_O127("cel 099.123456", "099123456"))
  expect_true(.tapa_O127("(02) 901.1234", "029011234"))
})

test_that("la fecha con hora se reconoce en mas formatos, y la hora no se traga un documento", {
  protegido <- "2024-06-27 21:24:25"
  for (escrito in c("06/27/2024 21:24:25", "27/06/2024, 21:24:25",
                    "20240627T212425", "2024-06-27 21.24.25")) {
    expect_true(.tapa_O127(paste("alta", escrito), protegido, escrito), info = escrito)
  }
  expect_true(.tapa_O127("10:30:00,41234567", "41234567", "41234567"))
})

test_that("los espacios e invisibles de Unicode que faltaban unen el numero", {
  for (signo in c(0x2028, 0x2029, 0x0085, 0xFE0F, 0x3164, 0x2800, 0x3001, 0xFF0B)) {
    texto <- paste0("tel 2901", intToUtf8(signo), "1234")
    expect_true(.tapa_O127(texto, "29011234"), info = sprintf("U+%04X", signo))
  }
})

test_that("una IP al final de una frase, un ISBN-10, una razon o una lista no se tapan", {
  # Antes, con estos documentos protegidos, los cinco se tapaban.
  expect_true(.intacto_O127("servidor 71.104.145.246.", "10414524"))
  expect_true(.intacto_O127("ISBN 0-306-40615-2", "30640615"))
  expect_true(.intacto_O127("razon 494:2714", "49427143"))
  expect_true(.intacto_O127("lista 12|345|6789", "12345678"))
  expect_true(.intacto_O127("ticket #5726550", "57265501") ||
                .intacto_O127("ticket #5726550", "72655019"))
  # El numeral ya no es un comodin: no se busca el documento sin su primer
  # digito detras de el.
  expect_true(.intacto_O127("ticket #2345678", "12345678"))
  expect_false(.intacto_O127("CI *2345678", "12345678"))
})

test_that("el motivo de perfilar_dbi tapa la cita con sus comillas de CSV", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE t (id INTEGER, cedula NUMERIC, monto REAL)")
  set.seed(3)
  n <- 1000L
  datos <- data.frame(id = seq_len(n),
                      cedula = as.character(sample(10000000:49999999, n)),
                      monto = round(runif(n, 1, 100), 2), stringsAsFactors = FALSE)
  # Una cedula de texto con sus comillas de importacion, que es el maximo.
  datos$cedula[[n]] <- "\"5.765.432-1\""
  DBI::dbWriteTable(conexion, "t", datos, append = TRUE)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(
    conexion, "t", muestra = 5000, columnas_personales = "cedula"
  )))
  motivos <- resultado$resumen_tabla$sql$motivo
  # Antes: ("[valor protegido]"5.765.432-1"[valor protegido]").
  expect_false(any(grepl("765.432", motivos, fixed = TRUE)))
})
