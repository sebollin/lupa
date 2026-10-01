# Ronda 19: lo que la proteccion de datos personales todavia dejaba salir -un
# valor escapado, otro grupo, el motor, otras escrituras- y lo que tapaba de mas
# -palabras y marcas del paquete, conteos, fechas-.

.hojas_O118 <- function(x) {
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

.titulares_O118 <- c("Ana Maria Ruiz", "Carlos Sosa Diaz", "Elena Vidal Mora")

test_that("un valor protegido con un caracter que se escapa no sale escapado", {
  for (separador in c("\u00a0", "\n", "\t", "\u200b")) {
    v <- paste0("Juan", separador, "P\u00e9rez")
    n <- 30L
    datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
    datos$titular <- rep(c(v, .titulares_O118), length.out = n)
    datos$referente <- rep(c(v, toupper(v), paste0(" ", v), "Ana Maria Ruiz",
                             "Carlos Sosa Diaz"), length.out = n)
    perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
    hojas <- .hojas_O118(perfil)
    # Antes: `Juan<U+00A0>P\u00e9rez` en cuatro evidencias de `referente`.
    expect_false(any(grepl("Juan<U+", hojas, fixed = TRUE)), label = separador)
    expect_false(any(grepl("Juan\\nP", hojas, fixed = TRUE)), label = separador)
    expect_false(any(grepl("Juan\\tP", hojas, fixed = TRUE)), label = separador)
    expect_false(any(grepl(v, hojas, fixed = TRUE)), label = separador)
  }
})

test_that("perfilar_por no publica el titular protegido de otro grupo", {
  n <- 80L
  datos <- data.frame(id = seq_len(n), depto = rep(c("Norte", "Sur"), each = n / 2),
                      stringsAsFactors = FALSE)
  datos$titular <- c(rep(c("Juan Perez Rodriguez", "Ana Maria Ruiz"), length.out = 40),
                     rep(c("Carlos Sosa Diaz", "Elena Vidal Mora"), length.out = 40))
  datos$referente <- c(
    rep(c("Carlos Sosa Diaz", "Elena Vidal Mora"), length.out = 40),
    rep(c("Juan Perez Rodriguez", "JUAN PEREZ RODRIGUEZ",
          " Juan Perez Rodriguez", "Elena Vidal Mora"), length.out = 40)
  )
  agrupado <- suppressMessages(perfilar_por(
    datos, por = "depto", min_filas = 10L, columnas_personales = "titular"
  ))
  hojas <- .hojas_O118(agrupado)
  # Antes: en la evidencia del grupo Sur, exacto.
  expect_false(any(grepl("Juan Perez Rodriguez", hojas, ignore.case = TRUE)))
  # Las etiquetas, que se publican por decision, siguen ahi.
  expect_true(all(c("Norte", "Sur") %in% agrupado$grupo))
})

test_that("perfilar_dbi tapa en el resumen el valor de una persona que es moda de otra columna", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  n <- 60L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$titular <- c(rep("Ana Maria Ruiz", 20),
                     rep(c("Juan Perez Rodriguez", "Carlos Sosa Diaz",
                           "Elena Vidal Mora", "Lucia Fernandez Gil"), length.out = 40))
  datos$referente <- c(rep("Juan Perez Rodriguez", 25),
                       rep(c("Pedro Gomez Sanz", "Marta Lopez Diaz"), length.out = 35))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  resultado <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "t", columnas_personales = "titular")
  ))
  hojas <- .hojas_O118(resultado)
  # Antes: la moda SQL de `referente` y el motivo de la corroboracion.
  expect_false(any(grepl("Juan Perez Rodriguez", hojas, fixed = TRUE)))
  # Control: lo que no es de ninguna persona protegida se publica.
  expect_true(any(grepl("Pedro Gomez Sanz", hojas, fixed = TRUE)))
})

test_that("las variantes por compatibilidad, caja y codificacion no salen", {
  cp1252 <- rawToChar(as.raw(c(0x4d, 0x61, 0x9a, 0x6b, 0x6f, 0x76, 0xe1, 0x20,
                               0x4a, 0x61, 0x6e, 0x61)))
  cortado <- paste0("pago a MAR\u00cdA FERN\u00c1NDEZ seg", rawToChar(as.raw(0xc3)))
  pares <- list(
    c("Steffi Griffith", "Ste\ufb00i Gri\ufb03th"),
    c("\u30bf\u30ca\u30ab \u30bf\u30ed\u30a6", "\uff80\uff85\uff76 \uff80\uff9b\uff73"),
    c("\u0257anjuma \u0199abiru", "\u018aANJUMA \u0198ABIRU"),
    c("\u10d2\u10d8\u10dd\u10e0\u10d2\u10d8 \u10d1\u10d4\u10e0\u10d8\u10eb\u10d4",
      "\u1c92\u1c98\u1c9d\u1ca0\u1c92\u1c98 \u1c91\u1c94\u1ca0\u1c98\u1cab\u1c94"),
    c("Maria Lopez", "\U0001D40C\U0001D41A\U0001D42B\U0001D422\U0001D41A \U0001D40B\U0001D428\U0001D429\U0001D41E\U0001D433"),
    c(cp1252, "Ma\u0161kov\u00e1 Jana"),
    c("Mar\u00eda Fern\u00e1ndez", cortado)
  )
  for (par in pares) {
    n <- 24L
    datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
    datos$titular <- rep(c(par[[1L]], .titulares_O118), length.out = n)
    datos$referente <- rep(c(par[[2L]], par[[2L]], "sin novedad", "otra cosa"),
                           length.out = n)
    # El control va en otra columna: en `referente` el patron puede juntar la
    # variante con "sin novedad" en la misma celda de ejemplos, y esa celda se
    # tapa entera, que es la regla.
    datos$estado <- rep("activo", n)
    perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
    hojas <- .hojas_O118(perfil)
    expect_false(any(grepl(par[[2L]], hojas, fixed = TRUE, useBytes = TRUE)),
                 label = par[[2L]])
    expect_true(any(grepl("activo", hojas, fixed = TRUE)))
  }
})

test_that("un nivel que no es UTF-8 no se cita en bytes legibles", {
  latin1 <- function(s) {
    x <- iconv(s, "UTF-8", "latin1")
    Encoding(x) <- "unknown"
    x
  }
  n <- 80L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$titular <- rep(c(latin1("Juan P\u00e9rez"), .titulares_O118), length.out = n)
  datos$proveedor <- rep(c(latin1("juanp\u00e9rezsrl"), "particular"), length.out = n)
  datos$rut <- ifelse(datos$proveedor == "particular", NA,
                      sprintf("21%07d0018", seq_len(n)))
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  sugerencias <- as.character(perfil$hallazgos$sugerencia)
  # Antes: `proveedor == "<lupa-byte:6A75616E70E972657A73726C>"`.
  expect_false(any(grepl("6A75616E70E9", sugerencias, fixed = TRUE)))
})

test_that("nombres y apellidos que son palabras del paquete no tapan su texto", {
  n <- 300L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$primer_nombre <- rep(c("M\u00e1ximo", "Constante", "Ana", "Luis"), length.out = n)
  datos$primer_apellido <- rep(c("Blanco", "Patr\u00f3n", "Ruiz", "Sosa"), length.out = n)
  datos$observacion <- rep(c("", " ", "pago", "pago", "pago", "s/d"), length.out = n)
  datos$codigo <- c(sprintf("AB-%04d", seq_len(n - 1L)), "ab-0001")
  datos$sin_dato <- rep(NA_character_, n)
  datos$contacto <- rep(c("maximo blanco", "pago", "pago", "pago"), length.out = n)
  perfil <- suppressWarnings(perfilar(
    datos, columnas_personales = c("primer_nombre", "primer_apellido")
  ))
  h <- perfil$hallazgos
  evidencias <- as.character(h$evidencia[h$columna %in% c("observacion", "codigo")])
  # Antes: la marca `<blanco>` tapaba la evidencia entera, y `grupo_maximo`
  # tambien.
  expect_false(any(evidencias == "[valor protegido]"))
  expect_true(any(grepl("<blanco>", evidencias, fixed = TRUE)))
  descripciones <- as.character(h$descripcion[h$columna %in% c("codigo", "sin_dato")])
  expect_false(any(descripciones == "[valor protegido]"))
  # El otro lado: el nombre y el apellido juntos en un texto de otra columna se
  # siguen tapando.
  hojas <- .hojas_O118(perfil)
  expect_false(any(grepl("maximo blanco", hojas, fixed = TRUE)))
})

test_that("la regla de digitos no tapa conteos con muchos documentos protegidos", {
  documentos <- c("41234567", "92345671", "51234560")
  # Seis cifras que estan DENTRO de un documento pero no son el documento sin su
  # verificador: antes la celda se tapaba.
  texto <- "<blanco> (123456) filas"
  expect_identical(
    lupa:::.reemplazar_variantes_separadas(texto, documentos), texto
  )
  # El documento sin su verificador se sigue tapando, con sus separadores.
  expect_identical(
    lupa:::.reemplazar_variantes_separadas("caja 4.123.456", documentos),
    "[valor protegido]"
  )
})

test_that("una fecha de nacimiento protegida no tapa las fechas de otras columnas", {
  # Las fechas de alta caen en once dias, y entre los nacimientos protegidos hay
  # personas nacidas justo esos dias: el minimo, la mediana y el maximo de
  # `fecha_alta` coinciden con el cumpleanos de alguien. Antes salian tapados.
  alta <- as.Date("2000-01-01") + 0:10
  datos <- data.frame(
    id = seq_len(110),
    fecha_nacimiento = rep(c(alta[c(1L, 6L, 11L)],
                             as.Date("1950-01-01") + seq(0, 7000, length.out = 8)),
                           length.out = 110),
    fecha_alta = rep(alta, 10)
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "fecha_nacimiento"))
  columnas <- perfil$columnas
  fila_alta <- columnas[columnas$columna == "fecha_alta", ]
  campos <- intersect(c("minimo_fecha", "mediana_fecha", "maximo_fecha"),
                      names(columnas))
  expect_length(campos, 3L)
  for (campo in campos) {
    expect_false(identical(as.character(fila_alta[[campo]]), "[valor protegido]"),
                 label = campo)
  }
  # La columna de fechas de nacimiento se sigue protegiendo.
  nacimiento <- columnas[columnas$columna == "fecha_nacimiento", ]
  expect_identical(as.character(nacimiento$minimo_fecha), "[valor protegido]")
})
