# Ronda 25: lo que la refutacion de la privacidad encontro despues de la ronda
# 24 -un aborto, fugas y tapado de mas-.

.tapa_O133 <- function(texto, protegido, escrito = texto, ...) {
  salida <- lupa:::.reemplazar_valores_protegidos(texto, protegido, ...)
  !grepl(escrito, salida, fixed = TRUE)
}

.intacto_O133 <- function(texto, protegido, ...) {
  identical(lupa:::.reemplazar_valores_protegidos(texto, protegido, ...), texto)
}

.cedulas_O133 <- function(n, semilla) {
  set.seed(semilla)
  sprintf("%d.%03d.%03d-%d", sample(1:6, n, TRUE), sample(0:999, n, TRUE),
          sample(0:999, n, TRUE), sample(0:9, n, TRUE))
}

# El nombre "Anio Ingreso" con la enie, armado sin escribir la letra en el archivo.
.anio_O133 <- paste0("A", rawToChar(as.raw(c(0xc3, 0xb1))), "o Ingreso")
Encoding(.anio_O133) <- "UTF-8"

test_that("los nombres no ASCII entre comillas se apartan y se reponen sin abortar", {
  anio <- .anio_O133
  x <- paste0("SELECT \"id\", \"", anio, "\", \"cedula\" FROM t")
  apartado <- lupa:::.apartar_nombres_delimitados(x, c("id", anio, "cedula"))
  # Antes: `match()` entre piezas marcadas `bytes` y nombres en UTF-8 abortaba
  # con dos piezas o mas, y con una sola no encontraba el nombre.
  expect_false(grepl(anio, apartado$x, fixed = TRUE, useBytes = TRUE))
  repuesto <- lupa:::.reponer_nombres_delimitados(apartado$x, apartado)
  expect_identical(charToRaw(repuesto), charToRaw(x))
  sola <- paste0("cita \"", anio, "\"")
  expect_false(identical(lupa:::.apartar_nombres_delimitados(sola, anio)$x, sola))
  expect_identical(
    lupa:::.reemplazar_valores_protegidos(
      paste(x, "WHERE x = '4.123.456-7'"), "4.123.456-7",
      intocables = c("id", anio, "cedula")
    ),
    paste(x, "WHERE x = '[valor protegido]'")
  )
})

test_that("perfilar y perfilar_dbi no abortan con un nombre no ASCII y una cedula", {
  n <- 60L
  cedula <- .cedulas_O133(n, 251)
  d <- data.frame(a = rep(c("A", "B", "C"), length.out = n), cedula = cedula,
                  obs = rep(c(paste("reclamo CI", cedula[[1L]]), "ok"),
                            length.out = n), stringsAsFactors = FALSE)
  names(d)[[1L]] <- .anio_O133
  # Antes: "no se permite traduccion de cadenas con bytes", por omision.
  perfil <- suppressWarnings(perfilar(d))
  hojas <- c(perfil$columnas$moda, perfil$hallazgos$evidencia)
  expect_false(any(grepl(cedula[[1L]], hojas, fixed = TRUE)))
  # El nombre citado en la evidencia de los nombres no sintacticos sale entero.
  expect_true(any(grepl(.anio_O133, perfil$hallazgos$evidencia, fixed = TRUE,
                        useBytes = TRUE)))
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", d)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(conexion, "t")))
  expect_false(any(grepl(cedula[[1L]], resultado$resumen_tabla$columnas$moda,
                         fixed = TRUE)))
})

test_that("clasificar_variables y detectar_discordancias aplican el piso de perfilar", {
  n <- 40L
  cedula <- .cedulas_O133(n, 252)
  citada <- cedula[[7L]]
  d <- data.frame(cedula = cedula, stringsAsFactors = FALSE)
  d$obs <- rep(c(paste("reclamo CI", citada), "ok"), length.out = n)
  d$obs2 <- ifelse(seq_len(n) %% 4L == 1L, toupper(d$obs), d$obs)
  clasificacion <- clasificar_variables(d)
  niveles <- unlist(clasificacion$niveles_observados, use.names = FALSE)
  # Antes: `reclamo CI <cedula>` y `RECLAMO CI <cedula>` en los niveles de
  # `obs` y `obs2`, mientras perfilar() los tapaba sobre la misma tabla.
  expect_false(any(grepl(citada, niveles, fixed = TRUE)))
  expect_true("reclamo CI [valor protegido]" %in% niveles)
  expect_true("ok" %in% niveles)
  discordancias <- detectar_discordancias(
    d, senal_redundante(c("obs", "obs2")), max_ejemplos = 50L
  )
  expect_false(any(grepl(citada, discordancias$evidencia, fixed = TRUE)))
  expect_true(grepl("obs=reclamo CI [valor protegido]",
                    discordancias$evidencia, fixed = TRUE))
  # Sin proteccion, el texto sale tal cual: el piso no toca lo que no protege.
  abierta <- clasificar_variables(d, proteger_datos_personales = FALSE)
  expect_true(any(grepl(citada, unlist(abierta$niveles_observados),
                        fixed = TRUE)))
})

test_that("la fecha con hora protegida se tapa con barra, arroba, guion doble o corchete", {
  protegido <- "2024-06-07 09:04:05"
  # Antes: estas formas se publicaban, y sus hermanas " - ", ";" y "|" no.
  for (texto in c("alta 2024-06-07 / 09:04:05", "alta 2024-06-07 @ 09:04:05",
                  "alta 2024-06-07 -- 09:04:05", "alta 2024-06-07 [09:04:05]",
                  "alta 2024-06-07T090405", "alta 07/06/2024 / 09:04:05")) {
    salida <- lupa:::.reemplazar_valores_protegidos(texto, protegido)
    expect_false(grepl("09:?04:?05", salida), info = texto)
  }
  # Otra fecha con hora escrita igual, no.
  expect_true(.intacto_O133("alta 2024-06-08 / 09:04:05", protegido))
  expect_true(.intacto_O133("alta 2024-06-07T090406", protegido))
})

test_that("los asteriscos de Unicode son comodin: el de ocho rayos y el de centro abierto", {
  protegido <- "4.123.456-7"
  # U+2733 (el del emoji), U+2732, U+2749 y U+1F7B6: antes, solo seis de los
  # cuarenta y siete que Unicode llama asterisco.
  for (cp in c(0x2733, 0x2732, 0x2749, 0x1F7B6, 0x229B)) {
    texto <- paste0("CI ", intToUtf8(cp), ".123.456-7")
    expect_false(grepl("123.456-7", lupa:::.reemplazar_valores_protegidos(
      texto, protegido), fixed = TRUE), info = sprintf("U+%04X", cp))
    # Otro documento tras el mismo asterisco, no.
    otro <- paste0("CI ", intToUtf8(cp), ".999.888-1")
    expect_true(.intacto_O133(otro, protegido), info = sprintf("U+%04X", cp))
  }
})

test_that("el correo protegido escrito con (at), arroba o dot se tapa, y la frase no", {
  protegido <- "juan_perez12@gmail.com"
  # Antes: diez de doce formas se publicaban.
  for (texto in c("escribir a juan_perez12(at)gmail.com",
                  "escribir a juan_perez12 [at] gmail.com",
                  "escribir a juan_perez12_AT_gmail.com",
                  "escribir a juan_perez12 arroba gmail.com",
                  "escribir a juan_perez12(a)gmail.com",
                  "escribir a juan_perez12 at gmail dot com",
                  "escribir a juan_perez12(at)gmail(punto)com")) {
    expect_false(.intacto_O133(texto, protegido), info = texto)
  }
  # Otro correo escrito igual, y las frases con esas palabras, no.
  expect_true(.intacto_O133("escribir a maria_lopez7(at)gmail.com", protegido))
  expect_true(.intacto_O133("maria at lopez", "Maria Lopez"))
  expect_true(.intacto_O133("maria punto lopez", "Maria Lopez"))
  expect_true(.intacto_O133("nos vemos at the office", protegido))
})

test_that("el agudo modificador U+02CA en lugar de la tilde no esconde el nombre", {
  agudo <- intToUtf8(0x2CA)
  texto <- paste0("titular Jose", agudo, "fina Gonza", agudo, "lez")
  # Antes: salia en la moda, los ejemplos y el HTML; con U+00B4 se tapaba.
  expect_false(.intacto_O133(texto, "Josefina Gonzalez"))
  expect_false(.intacto_O133(texto, "Josefina Gonzalez", exigir_limites = TRUE))
  # Otro nombre con el mismo signo, no.
  expect_true(.intacto_O133(paste0("titular Mari", agudo, "a Lopez"),
                            "Josefina Gonzalez"))
})

test_that("una sola regla de columna de fechas: ano en dos cifras y AAAAMMDD con una celda mala", {
  set.seed(253)
  n <- 400L
  nacimiento <- as.Date("1940-01-01") + sample(0:24000, n, TRUE)
  ingreso <- format(as.Date("1990-01-01") + sample(0:12000, n, TRUE), "%d/%m/%Y")
  dos_cifras <- ifelse(seq_len(n) %% 5L < 3L, format(nacimiento, "%d/%m/%y"),
                       format(nacimiento, "%d/%m/%Y"))
  larga <- which(nchar(dos_cifras) == 10L)[[1L]]
  ingreso[1:60] <- format(nacimiento[[larga]], "%d/%m/%Y")
  compacta <- format(nacimiento, "%Y%m%d")
  compacta[[17L]] <- "99999999"
  alta <- format(as.Date("1990-01-01") + sample(0:12000, n, TRUE), "%Y%m%d")
  alta[1:60] <- compacta[[1L]]
  d <- data.frame(id = seq_len(n), nac = dos_cifras, ingreso = ingreso,
                  nac2 = compacta, alta = alta, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(d, columnas_personales = c("nac", "nac2")))
  moda <- function(columna) perfil$columnas$moda[perfil$columnas$columna == columna]
  # Antes: el 60 % en dd/mm/aa, o una sola celda mala en la AAAAMMDD, hacian que
  # la columna "no fuera de fechas" y sus fechas tapaban la moda de las otras.
  expect_identical(moda("ingreso"), format(nacimiento[[larga]], "%d/%m/%Y"))
  expect_identical(moda("alta"), compacta[[1L]])
  expect_true(lupa:::.columna_de_fechas(dos_cifras))
  expect_true(lupa:::.columna_de_fechas(compacta))
  # El relleno de la columna de fechas sigue en el piso, y el telefono con
  # forma de fecha de una columna que no es de fechas tambien.
  expect_true("99999999" %in% lupa:::.valores_publicables_protegidos(d, "nac2"))
  telefono <- sprintf("2%07d", seq_len(40L) * 1013L)
  telefono[1:2] <- "2012-11-05"
  expect_identical(lupa:::.fechas_de_columna_no_fecha(telefono), "2012-11-05")
})

test_that("el centinela o el relleno de una columna protegida no vacia el perfil de otra", {
  n <- 200L
  cedula <- sprintf("%d.%03d.%03d-%d", 1:n %% 5L + 1L, 100L + 1:n %% 900L,
                    1:n, 1:n %% 10L)
  cedula[1:12] <- "99999999"
  telefono <- sprintf("29%02d%04d", 1:n %% 100L, 1:n)
  telefono[20:31] <- "------"
  monto <- 100 + seq_len(n) * 7
  monto[1:8] <- 99999999
  obs <- ifelse(seq_len(n) %% 3L == 0L, "ok", "sin observaciones ------ revisar")
  d <- data.frame(cedula = cedula, telefono = telefono, monto = monto,
                  obs = obs, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(d, columnas_personales = c("cedula", "telefono")))
  columnas <- perfil$columnas
  fila <- function(columna, campo) columnas[[campo]][columnas$columna == columna]
  # Antes: moda `[valor protegido]` y minimo, mediana y maximo en NA, y la moda
  # de `obs` con el relleno tapado.
  expect_identical(fila("monto", "moda"), "99999999")
  expect_false(is.na(fila("monto", "mediana")))
  expect_identical(fila("obs", "moda"), "sin observaciones ------ revisar")
  # Las protegidas siguen tapadas por columna.
  expect_identical(fila("cedula", "moda"), "[valor protegido]")
  # Donde el documento se repite, el digito repetido sigue en el piso.
  movimientos <- c(rep(cedula[50:59], 18), rep("22222222", 20))
  expect_identical(lupa:::.rellenos_de_columna(movimientos), character())
  expect_identical(lupa:::.rellenos_de_columna(cedula), "99999999")
})

test_that("la IP con mascara y el importe largo con miles se prueban enteros", {
  # Antes: el tramo de octetos "4123234" de la IP con mascara, y el de tres
  # grupos "3919024" del importe, casaban con el documento sin verificador.
  expect_true(.intacto_O133("ruta 10.4.123.234/24", "4.123.234-5"))
  expect_true(.intacto_O133("monto 3.919.024.166", "3.919.024-1"))
  expect_true(.intacto_O133("$ 5.902.468.069", "5.902.468-1"))
  # Enteros, siguen tapados: la IP que es el documento, la cedula colombiana.
  expect_false(.intacto_O133("CI 4.123.234.5/24", "4.123.234-5"))
  expect_false(.intacto_O133("CC 1.023.456.789", "1023456789"))
  # El celular con su prefijo y espacios no es un importe: su tramo se busca.
  expect_false(.intacto_O133("tel 598 099 123 456", "099123456"))
})
