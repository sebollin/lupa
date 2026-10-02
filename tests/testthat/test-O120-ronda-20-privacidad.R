# Ronda 20: lo que la ronda 19 aflojo o dejo abierto en la proteccion de datos
# personales -el desescapado, las marcas, la regla de digitos, la muestra del
# motor- y lo que todavia tapaba de mas.

.hojas_O120 <- function(x) {
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

test_that("un valor protegido con una barra literal no se publica", {
  barra <- "\\"
  usuarios <- paste0("CORP", barra, c("nrodriguez", "tsilvera", "asosavidal", "ecastrogil"))
  n <- 80L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$usuario <- rep(usuarios, length.out = n)
  datos$aprobado_por <- rep(c(usuarios[[1L]], toupper(usuarios[[2L]]), "particular"),
                            length.out = n)
  datos$rut <- ifelse(datos$aprobado_por == "particular", NA,
                      sprintf("21%07d0018", seq_len(n)))
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "usuario"))
  hojas <- tolower(.hojas_O120(perfil))
  # Antes: en la cita de la sugerencia, y en otra caja en moda y ejemplos.
  expect_false(any(grepl("rodriguez", hojas, fixed = TRUE)))
  expect_false(any(grepl("silvera", hojas, fixed = TRUE)))
})

test_that("un valor protegido que trae una marca del paquete no se publica", {
  domicilios <- c("Av. Italia 2345<br>Apto 101", "Bvar. Artigas 1180<br>Apto 3",
                  "Colonia 1234 Apto 5")
  n <- 30L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$domicilio <- rep(domicilios, length.out = n)
  datos$referencia <- rep(c(domicilios, toupper(domicilios)), length.out = n)
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "domicilio"))
  hojas <- tolower(.hojas_O120(perfil))
  # Antes: los dos con la marca salian exactos en los ejemplos.
  expect_false(any(grepl("italia 2345", hojas, fixed = TRUE)))
  expect_false(any(grepl("artigas 1180", hojas, fixed = TRUE)))
})

test_that("la regla de digitos reconoce el documento por sus tramos", {
  documentos <- c("61112223", "37778881", "41234567", "45556662", "12223334")
  tapa <- function(texto) {
    identical(lupa:::.reemplazar_variantes_separadas(texto, documentos),
              "[valor protegido]")
  }
  # Antes, todos estos se publicaban.
  expect_true(tapa("ref 6.111.222-9"))
  expect_true(tapa("ref 3.777.888/2024"))
  expect_true(tapa("ref 4,123,456"))
  expect_true(tapa("ref 4\u00a0123\u00a0456"))
  expect_true(tapa("ref *.555.666-2"))
  expect_true(tapa("ref 1.222.333 2.999.000"))
  # Controles: dos conteos vecinos no se pegan, y seis cifras de adentro de un
  # documento no son el documento.
  expect_false(tapa("N/A (49710); - (49576)"))
  expect_false(tapa("<blanco> (123456) filas"))
  expect_false(tapa("se evaluaron 5.000 de 988.203 valores distintos"))
})

test_that("la regla de digitos no toma una fecha ni un parcial corto por documento", {
  # Una fecha de calendario partida en tramos coincidia con una cedula, y el
  # parcial de seis cifras de una cedula de siete coincidia con casi cualquier
  # conteo cuando hay cientos de miles protegidas. Medido con un millon de
  # cedulas: dieciseis celdas tapadas de mas.
  documentos <- c("20030517", "5400523")
  igual <- function(texto) {
    identical(lupa:::.reemplazar_variantes_separadas(texto, documentos), texto)
  }
  expect_true(igual("alta 2003-05-17"))
  expect_true(igual("alta 17/05/2003"))
  expect_true(igual("0 ausentes reales y 400523 disfrazados"))
  # Controles: los documentos enteros se siguen tapando.
  expect_false(igual("ref 2.003.051-7"))
  expect_false(igual("ref 540.052-3"))
})

test_that("los conteos del paquete no se toman por documentos", {
  documentos <- c("15121970", "10000003", "50001184")
  regla <- function(texto, prosa = FALSE) {
    lupa:::.reemplazar_variantes_separadas(texto, documentos, exigir_limites = prosa)
  }
  # En la prosa, fuera de lo citado, un numero es un conteo del paquete.
  motivo <- "Eso son 1.512.197 de 12.497.500 pares posibles."
  expect_identical(regla(motivo, prosa = TRUE), motivo)
  # Citado, es un valor y se tapa la cita.
  expect_false(identical(regla("`x` == \"1.512.197\"", prosa = TRUE),
                         "`x` == \"1.512.197\""))
  # Un numero redondo y la parte decimal de otro no son documentos.
  expect_identical(regla("muestra de 100.000 de 1.000.000 valores"),
                   "muestra de 100.000 de 1.000.000 valores")
  expect_identical(regla("p=0.0001184"), "p=0.0001184")
  # Control: en un campo de valores, el documento sin verificador se tapa.
  expect_identical(regla("ref 1.512.197"), "[valor protegido]")
})

test_that("perfilar_dbi tapa el valor de una persona que esta fuera de la muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(14)
  n <- 3000L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$titular <- sprintf("Persona %s %05d", sample(c("Alvarez", "Benitez", "Cabrera"),
                                                     n, TRUE), sample(1:99999, n))
  datos$titular[[n]] <- "Juan Perez Rodriguez"
  datos$referente <- sample(c("Juan Perez Rodriguez", "Marta Lopez Diaz",
                              "Pedro Gomez Sanz"), n, TRUE, prob = c(50, 25, 25))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", muestra = 300, columnas_personales = "titular"
  )))
  # Antes: la moda SQL de `referente` y el perfil de la muestra.
  expect_false(any(grepl("Juan Perez Rodriguez", .hojas_O120(resultado), fixed = TRUE)))
})

test_that("un latin1 con un par que parece UTF-8 se reconoce igual", {
  latin1 <- function(s) {
    x <- iconv(s, "UTF-8", "latin1")
    Encoding(x) <- "unknown"
    x
  }
  protegidos <- c("JOS\u00c9\u00a0LUIS MU\u00d1OZ", "REN\u00c9\u00a0IBA\u00d1EZ PAZ")
  n <- 60L
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos$titular <- rep(c(protegidos, "Ana Maria Ruiz"), length.out = n)
  datos$referente <- rep(c(latin1(protegidos), "otro"), length.out = n)
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  hojas <- .hojas_O120(perfil)
  # Antes: la moda de `referente` publicaba los bytes latin1 del nombre.
  expect_false(any(hojas %in% latin1(protegidos)))
})

test_that("una fecha de nacimiento compacta no tapa las fechas de otras columnas", {
  alta <- as.Date("2000-01-01") + 0:10
  nacimiento <- rep(c(alta[c(1L, 6L, 11L)],
                      as.Date("1950-01-01") + seq(0, 7000, length.out = 8)),
                    length.out = 110)
  datos <- data.frame(
    id = seq_len(110),
    fecha_nacimiento = as.integer(format(nacimiento, "%Y%m%d")),
    fecha_alta = rep(alta, 10)
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "fecha_nacimiento"))
  fila <- perfil$columnas[perfil$columnas$columna == "fecha_alta", ]
  for (campo in intersect(c("minimo_fecha", "mediana_fecha", "maximo_fecha"),
                          names(fila))) {
    expect_false(identical(as.character(fila[[campo]]), "[valor protegido]"),
                 label = campo)
  }
  expect_false(identical(lupa:::.valores_identificantes("1963-09-30T00:00:00"),
                         "1963-09-30T00:00:00"))
})

test_that("un nombre protegido que es palabra del paquete no tapa la unidad", {
  datos <- data.frame(
    primer_nombre = rep(c("Segundo", "Ana", "Luis", "Marta"), 5),
    alta = as.Date("2020-01-01") + 0:19, stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "primer_nombre"))
  unidad <- perfil$columnas$unidad[perfil$columnas$columna == "alta"]
  skip_if(is.null(unidad) || is.na(unidad))
  # Antes: `[valor protegido]` en lugar de `segundos`.
  expect_false(identical(as.character(unidad), "[valor protegido]"))
})
