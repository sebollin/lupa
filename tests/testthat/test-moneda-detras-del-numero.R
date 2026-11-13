# Un simbolo de moneda es moneda delante o detras del numero, pegado o separado
# por un espacio -tambien el duro o el fino-. La forma europea escribe el
# simbolo detras (`1.234,56` y el euro), y el perfil la contaba solo delante: la
# columna quedaba como texto, sin `numero_como_texto` y sin conversion. Un
# codigo de tres letras sigue la regla de siempre: separado y ISO 4217.
testthat::skip_on_cran()

.fecha_moneda_detras <- as.POSIXct("2026-01-15", tz = "UTC")
.euro_moneda_detras <- "\u20ac"
.conversiones_moneda_detras <- c("convertir_tipo", "convertir_numero_regional")

# El perfil de la columna, su conversion en el plan y lo que deja `aplicar()`
# con el plan tal como sale.
.lectura_moneda_detras <- function(x) {
  datos <- data.frame(x = x, stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, fecha = .fecha_moneda_detras, analizar_dependencias = FALSE
  )
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[
    as.character(plan$columna) %in% "x" &
      as.character(plan$estrategia) %in% .conversiones_moneda_detras, ,
    drop = FALSE
  ]
  hallazgos <- perfil$hallazgos[perfil$hallazgos$columna %in% "x", , drop = FALSE]
  list(
    fila = perfil$columnas, conversion = conversion,
    hallazgos = as.character(hallazgos$tipo_hallazgo),
    aplicado = aplicar(plan, datos)$datos$x
  )
}

# Lo que se espera de una columna de importes en una sola moneda: se cuentan
# todos, se publica la moneda, y el plan los convierte por omision.
.importes_moneda_detras <- function(lectura, moneda, valores, info) {
  testthat::expect_identical(
    as.integer(lectura$fila$n_numeros_texto), length(valores), info = info
  )
  testthat::expect_identical(
    as.character(lectura$fila$numero_texto_moneda), moneda, info = info
  )
  testthat::expect_true("numero_como_texto" %in% lectura$hallazgos, info = info)
  testthat::expect_false("monedas_mixtas" %in% lectura$hallazgos, info = info)
  testthat::expect_identical(
    as.character(lectura$conversion$estrategia), "convertir_numero_regional",
    info = info
  )
  testthat::expect_identical(
    as.character(lectura$conversion$estado), "lista", info = info
  )
  testthat::expect_true(isTRUE(lectura$conversion$aplicar), info = info)
  testthat::expect_identical(lectura$aplicado, valores, info = info)
}

test_that("el simbolo detras del numero, pegado o separado, es la moneda de la columna", {
  euro <- .euro_moneda_detras
  importes <- c("1.234,56", "2.000,00")
  formas <- list(
    dolar_detras_pegado = list(x = paste0(importes, "$"), moneda = "$"),
    dolar_detras = list(x = paste0(importes, " $"), moneda = "$"),
    euro_detras_pegado = list(x = paste0(importes, euro), moneda = euro),
    euro_detras = list(x = paste0(importes, " ", euro), moneda = euro),
    # Control: delante se reconocia y se sigue reconociendo igual.
    dolar_delante = list(x = paste0("$ ", importes), moneda = "$"),
    euro_delante_pegado = list(x = paste0(euro, importes), moneda = euro)
  )
  for (nombre in names(formas)) {
    lectura <- .lectura_moneda_detras(formas[[nombre]]$x)
    # A mano: con la coma decimal, mil doscientos treinta y cuatro con 56, y
    # dos mil.
    .importes_moneda_detras(
      lectura, formas[[nombre]]$moneda, c(1234.56, 2000), nombre
    )
  }
})

test_that("U$S se escribe pegado como $, delante o detras", {
  for (x in list(c("U$S35", "U$S40"), c("35 U$S", "40 U$S"),
                 c("35U$S", "40U$S"), c("U$S 35", "U$S 40"))) {
    lectura <- .lectura_moneda_detras(x)
    .importes_moneda_detras(lectura, "U$S", c(35, 40), x[[1L]])
  }
})

test_that("el espacio duro y el fino separan el simbolo igual que el espacio", {
  euro <- .euro_moneda_detras
  importes <- c("1.234,56", "2.000,00")
  for (espacio in c("\u00a0", "\u202f")) {
    nombre <- sprintf("U+%04X", utf8ToInt(espacio))
    detras <- .lectura_moneda_detras(paste0(importes, espacio, euro))
    .importes_moneda_detras(detras, euro, c(1234.56, 2000), paste(nombre, "detras"))
    delante <- .lectura_moneda_detras(paste0(euro, espacio, importes))
    .importes_moneda_detras(delante, euro, c(1234.56, 2000), paste(nombre, "delante"))
  }
})

test_that("el mismo simbolo delante y detras en una columna es una sola moneda", {
  euro <- .euro_moneda_detras
  x <- c(paste0(euro, " 10"), paste0("20 ", euro), paste0(euro, "30"),
         paste0("40", euro))
  lectura <- .lectura_moneda_detras(x)
  # Contaba solo los dos con el simbolo delante y no proponia convertir.
  .importes_moneda_detras(lectura, euro, c(10, 20, 30, 40), "posiciones")
})

test_that("monedas distintas detras del numero dan monedas_mixtas y no se convierten", {
  euro <- .euro_moneda_detras
  casos <- list(
    detras = c(paste0("10 ", euro), "20 $", paste0("30 ", euro), "40 $"),
    posiciones = c(paste0(euro, " 10"), "20 $", paste0(euro, " 30"), "40 $"),
    simbolo_y_codigo = c("10 $", "20 USD", "30 $", "40 USD")
  )
  for (nombre in names(casos)) {
    x <- casos[[nombre]]
    lectura <- .lectura_moneda_detras(x)
    expect_identical(as.integer(lectura$fila$n_numeros_texto), 4L, info = nombre)
    expect_identical(as.character(lectura$fila$numero_texto_moneda), "",
                     info = nombre)
    expect_true("monedas_mixtas" %in% lectura$hallazgos, info = nombre)
    expect_false(any(lectura$conversion$aplicar), info = nombre)
    expect_identical(lectura$aplicado, x, info = nombre)
  }
})

test_that("un valor con moneda delante y detras no es un numero", {
  euro <- .euro_moneda_detras
  casos <- list(
    simbolo = c("$ 35 $", "$ 40 $"),
    euro = paste0(euro, c("35", "40"), euro),
    # Se contaban con la moneda de delante, y la conversion por omision les
    # quitaba la de detras sin decirlo: `USD 35 EUR` quedaba en 35.
    codigos = c("USD 35 EUR", "USD 40 EUR"),
    simbolo_y_codigo = c("$ 35 USD", "$ 40 USD")
  )
  for (nombre in names(casos)) {
    x <- casos[[nombre]]
    lectura <- .lectura_moneda_detras(x)
    expect_identical(as.integer(lectura$fila$n_numeros_texto), 0L, info = nombre)
    expect_false("numero_como_texto" %in% lectura$hallazgos, info = nombre)
    expect_identical(nrow(lectura$conversion), 0L, info = nombre)
    expect_identical(lectura$aplicado, x, info = nombre)
  }
  # El conversor es el mismo lector: marcado a mano tampoco convierte.
  expect_error(
    .convertir_numero_regional(
      c("USD 35 EUR", "USD 40 EUR"), list(convencion = "sin_separadores")
    ),
    "formato"
  )
})

test_that("un codigo de tres letras pegado al numero sigue sin ser una moneda", {
  # Control: lo decidido para los codigos no cambia con el simbolo detras.
  for (x in list(c("35USD", "40USD"), c("35.50USD", "40.25USD"))) {
    lectura <- .lectura_moneda_detras(x)
    expect_identical(as.integer(lectura$fila$n_numeros_texto), 0L, info = x[[1L]])
    expect_identical(nrow(lectura$conversion), 0L, info = x[[1L]])
    expect_identical(lectura$aplicado, x, info = x[[1L]])
  }
  separado <- .lectura_moneda_detras(c("35 USD", "40 USD"))
  .importes_moneda_detras(separado, "USD", c(35, 40), "35 USD")
})

test_that("la conversion regional quita el simbolo detras y lee el numero", {
  euro <- .euro_moneda_detras
  x <- c(paste0("1.234,56 ", euro), paste0("-2.000,50", euro), NA)
  convertido <- .convertir_numero_regional(
    x, list(convencion = "decimal_coma", moneda = euro)
  )
  expect_identical(convertido$valor, c(1234.56, -2000.5, NA))
  expect_identical(convertido$n, 2L)
})

test_that("un salto de linea dentro del valor no vuelve moneda al texto entero", {
  # `$ 35\nkg` responde a la forma -moneda delante, unidad detras- y la moneda
  # publicada era el valor entero: dos valores, dos monedas falsas.
  lectura <- .lectura_moneda_detras(c("$ 35\nkg", "$ 40\nkg"))
  expect_identical(as.character(lectura$fila$numero_texto_moneda), "$")
  expect_false("monedas_mixtas" %in% lectura$hallazgos)
})

test_that("el simbolo detras se reconoce igual bajo el locale C", {
  euro <- .euro_moneda_detras
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", anterior), silent = TRUE), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"), error = function(e) NA_character_
  ))
  skip_if(is.na(puesto) || !identical(puesto, "C"), "sin locale C")
  x <- c(paste0("1.234,56\u00a0", euro), paste0("2.000,00 ", euro))
  lectura <- .lectura_moneda_detras(x)
  .importes_moneda_detras(lectura, euro, c(1234.56, 2000), "C")
})
