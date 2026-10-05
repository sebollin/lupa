# Ronda 24: lo que la refutacion de la privacidad encontro despues de la ronda
# 23 -fugas y tapado de mas-.

.tapa_O130 <- function(texto, protegido, escrito = texto, ...) {
  salida <- lupa:::.reemplazar_valores_protegidos(texto, protegido, ...)
  !grepl(escrito, salida, fixed = TRUE)
}

.intacto_O130 <- function(texto, protegido, ...) {
  identical(lupa:::.reemplazar_valores_protegidos(texto, protegido, ...), texto)
}

test_that("perfilar_dbi sin muestreo lee como texto la cedula guardada como texto en SQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE t (id INTEGER, cedula INTEGER, obs TEXT)")
  set.seed(24)
  n <- 120L
  cedula <- as.character(sample(10000000:59999999, n))
  # Una cedula escrita con formato: SQLite la guarda como texto y RSQLite, que
  # tipa la columna por sus primeras filas, la leia como 5.
  cedula[[60L]] <- "5.932.446-1"
  obs <- rep(c("sin novedad", "pago al dia"), length.out = n)
  obs[1:70] <- "reclamo CI 5.932.446-1"
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = seq_len(n), cedula = cedula, obs = obs, stringsAsFactors = FALSE
  ), append = TRUE)
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(
    conexion, "t", columnas_personales = "cedula"
  )))
  hojas <- c(
    resultado$resumen_tabla$columnas$moda,
    resultado$perfil_muestra$columnas$moda,
    resultado$perfil_muestra$patrones$obs$ejemplos
  )
  # Antes: la moda de `obs` salia exacta en el resumen y en la muestra, sin
  # muestreo -la llamada por omision-; con muestreo se tapaba.
  expect_false(any(grepl("932.446", hojas, fixed = TRUE)))
  expect_true(any(grepl("reclamo CI [valor protegido]", hojas, fixed = TRUE)))
})

test_that("un nombre de columna dentro de un valor protegido con otra caja no lo exceptua", {
  nombres <- c("juan_perez", "maria_lopez")
  for (correo in c("JUAN_PEREZ@EMPRESA.COM.UY", "Juan_Perez@empresa.com.uy")) {
    salida <- lupa:::.reemplazar_valores_protegidos(
      "escribir a juan_perez@empresa.com.uy", correo, intocables = nombres
    )
    # Antes: el nombre se apartaba del texto y no del valor en mayusculas, y el
    # correo salia.
    expect_false(grepl("juan_perez@", salida, fixed = TRUE), info = correo)
  }
})

test_that("el nombre apartado del valor no tapa el dominio ni los correos de otros", {
  nombres <- c("juan_perez", "maria_lopez")
  # Antes: la aguja recortada `@empresa.com.uy` tapaba los tres.
  for (texto in c("www.empresa.com.uy", "dominio empresa.com.uy",
                  "maria_lopez@empresa.com.uy")) {
    expect_true(
      .intacto_O130(texto, "juan_perez@empresa.com.uy", intocables = nombres),
      info = texto
    )
  }
})

test_that("cada marca apartada se repone en su lugar aunque se tape otra", {
  salida <- lupa:::.reemplazar_valores_protegidos(
    "escribir a juan_perez@empresa.com.uy, con copia a maria_lopez",
    "juan_perez@empresa.com.uy", intocables = c("juan_perez", "maria_lopez")
  )
  # Antes: "... con copia a juan_perez", la marca de otra columna.
  expect_identical(salida, "escribir a [valor protegido], con copia a maria_lopez")
  # Y en el mecanismo: si se pierde la primera marca, la segunda sigue siendo
  # la suya.
  apartado <- lupa:::.apartar_marcas_paquete(
    "a juan_perez b maria_lopez", c("juan_perez", "maria_lopez")
  )
  sin_primera <- sub("^a [^ ]+ b", "a  b", apartado$x)
  expect_identical(
    lupa:::.reponer_marcas_paquete(sin_primera, apartado), "a  b maria_lopez"
  )
})

test_that("un telefono con forma de fecha de una columna que no es de fechas sigue en el piso", {
  set.seed(24)
  telefono <- sprintf("%d-%02d-%02d", sample(2101:2999, 60, TRUE),
                      sample(0:99, 60, TRUE), sample(0:99, 60, TRUE))
  telefono[1:8] <- "2012-11-05"
  datos <- data.frame(
    telefono = telefono,
    obs = c(rep("llamar al 2012-11-05", 20), rep("sin novedad", 40)),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "telefono"))
  hojas <- unlist(list(perfil$columnas$moda, perfil$patrones$obs$ejemplos))
  # Antes: el piso sacaba "2012-11-05" por su forma de fecha y salia en los
  # ejemplos de `obs`.
  expect_false(any(grepl("2012-11-05", hojas, fixed = TRUE)))
  # Y la decision es por columna: en una columna de fechas la fecha sola sigue
  # fuera del piso.
  expect_identical(lupa:::.fechas_que_identifican(datos, "telefono"), "2012-11-05")
  fechas <- data.frame(nacimiento = c("2012-11-05", "1990-01-02", "1985-07-30"))
  expect_identical(lupa:::.fechas_que_identifican(fechas, "nacimiento"), character())
})

test_that("la raya y el apostrofo de un CSV de Windows leido como latin1 unen el numero", {
  for (byte in c(0x96, 0x97, 0x92)) {
    # Leido con `fileEncoding = "latin1"`: el control C1 en UTF-8.
    texto <- paste0("tel 2901", intToUtf8(byte), "1234")
    expect_false(.intacto_O130(texto, "29011234"), info = sprintf("U+%04X", byte))
    # Leido con `encoding = "latin1"`: el byte, marcado.
    marcado <- rawToChar(as.raw(c(0x74, 0x65, 0x6c, 0x20, 0x32, 0x39, 0x30, 0x31,
                                  byte, 0x31, 0x32, 0x33, 0x34)))
    Encoding(marcado) <- "latin1"
    expect_false(.intacto_O130(marcado, "29011234"), info = sprintf("0x%02X", byte))
  }
})

test_that("la fecha con hora protegida se reconoce con otros separadores entre fecha y hora", {
  protegido <- "2024-06-27 21:24:25"
  for (escrito in c("2024-06-27  21:24:25", "27/06/2024;21:24:25",
                    "2024-06-27 - 21:24:25", "backup_2024-06-27_21-24-25.sql",
                    "2024-06-27 21h24m25s", "2024-06-27 (21:24:25)",
                    "2024-06-27 | 21:24:25")) {
    expect_false(.intacto_O130(paste("alta", escrito), protegido), info = escrito)
  }
})

test_that("la fraccion de segundo tras punto no se traga un documento", {
  for (texto in c("alta 10:30:00.41234567", "alta 10:30:00.4123456-7",
                  "registro 10:30:00.41234567")) {
    expect_false(.intacto_O130(texto, "4.123.456-7"), info = texto)
  }
  # La fraccion de siete cifras sigue siendo una fraccion: no es el documento
  # 5.412.576-1 sin su verificador.
  expect_true(.intacto_O130("2024-05-17 12:26:58.5412576", "54125761"))
})

test_that("un documento de diez cifras en cuatro grupos no se borra como ISBN-10", {
  expect_false(.intacto_O130("NIT 900-123-456-7", "9001234567"))
  # Un ISBN-10 valido, con o sin la etiqueta, y uno con la etiqueta, no se tapan.
  expect_true(.intacto_O130("0-306-40615-2", "30640615"))
  expect_true(.intacto_O130("ISBN 4 046 33747 5", "46337475"))
  expect_true(.intacto_O130("ISBN 8.261.57570.2", "82615757"))
})

test_that("los invisibles y los parecidos que faltaban unen el numero", {
  for (signo in c(0x17B4, 0x180B, 0x2065, 0x0001, 0x009F, 0x20DD, 0x0387, 0x00B4,
                  0x02D7, 0xFE51, 0x2571)) {
    texto <- paste0("tel 2901", intToUtf8(signo), "1234")
    expect_false(.intacto_O130(texto, "29011234"), info = sprintf("U+%04X", signo))
  }
})

test_that("los asteriscos de Unicode son comodin, y el asterisco pegado a una letra", {
  for (signo in c(0xFF0A, 0x2217, 0xFE61, 0x2731, 0x204E, 0x066D)) {
    texto <- paste0("CI ", intToUtf8(signo), ".123.456-7")
    expect_false(.intacto_O130(texto, "4.123.456-7"), info = sprintf("U+%04X", signo))
  }
  expect_false(.intacto_O130("CI*123.456-7", "4.123.456-7"))
})

test_that("la equis seguida de un espacio no es comodin", {
  # Antes, las dos se tapaban como el documento sin su primer digito.
  expect_true(.intacto_O130("pack x 1781643", "21781643"))
  expect_true(.intacto_O130("92 x 6931234", "16931234"))
  # El documento recortado con la equis pegada se sigue tapando.
  expect_false(.intacto_O130("CI X2345678", "12345678"))
  expect_false(.intacto_O130("CI X.123.456-7", "4.123.456-7"))
})

test_that("una direccion IP se tapa solo cuando coincide entera", {
  # Antes: el tramo de octetos "21823590" era la cedula 2.182.359-0.
  expect_true(.intacto_O130("origen 218.235.90.247", "21823590"))
  expect_false(.intacto_O130("tel 29.10.12.34", "29101234"))
})

test_that("una fecha con espacios y el ano al final no es un documento", {
  expect_true(.intacto_O130("alta 16 01 2002", "16012002"))
  # Con el ano adelante puede ser un telefono escrito de a pares, y se busca.
  expect_false(.intacto_O130("llamar al 2012 11 05", "20121105"))
})

test_that("el valor que contiene un nombre no se busca dentro de otro nombre", {
  nombres <- c("juan_perez", "juan_perez_mail_id")
  # El valor contiene `juan_perez` y esta dentro de `juan_perez_mail_id`: la
  # columna se sigue nombrando entera, en la SQL y suelta.
  expect_true(.intacto_O130("COUNT(`juan_perez_mail_id`) de juan_perez",
                            "juan_perez_mail", intocables = nombres))
  expect_true(.intacto_O130("ver juan_perez_mail_id", "juan_perez_mail",
                            intocables = nombres))
  expect_false(.intacto_O130("enviar a JUAN_PEREZ_MAIL hoy", "juan_perez_mail",
                             intocables = nombres))
})
