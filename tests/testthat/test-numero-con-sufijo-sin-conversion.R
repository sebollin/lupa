# Un numero con un sufijo que no es una unidad (`12 KG`) o con tres letras que
# no son un codigo ISO 4217 (`ABC 13`) no se convierte: convertirlo perderia lo
# que lo acompana. Pero sigue escrito como un numero, y la columna que lo trae
# junto a otros numeros sigue guardando numeros como texto. Contarlo como texto
# cualquiera bajaba la proporcion de la columna y apagaba `numero_como_texto`
# para todos los demas valores, tambien para `1,5` y `2,5`.

.fecha_sufijo_sin_conversion <- as.POSIXct("2026-01-15", tz = "UTC")

.lectura_sufijo_sin_conversion <- function(x, ...) {
  datos <- data.frame(id = seq_along(x), x = x, stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, fecha = .fecha_sufijo_sin_conversion, analizar_dependencias = FALSE,
    ...
  )
  plan <- planificar_limpieza(perfil, datos)
  h <- perfil$hallazgos[perfil$hallazgos$columna %in% "x", , drop = FALSE]
  conversion <- plan[
    as.character(plan$columna) %in% "x" &
      as.character(plan$estrategia) %in% "convertir_numero_regional", ,
    drop = FALSE
  ]
  list(
    perfil = perfil,
    fila = perfil$columnas[perfil$columnas$columna %in% "x", , drop = FALSE],
    hallazgos = h, tipos = as.character(h$tipo_hallazgo),
    conversion = conversion,
    aplicado = aplicar(plan, datos)$datos$x
  )
}

.hallazgo_sufijo <- function(lectura, tipo) {
  lectura$hallazgos[lectura$tipos == tipo, , drop = FALSE]
}

test_that("un 12 KG no apaga numero_como_texto para los numeros de la columna", {
  skip_on_cran()
  x <- c("1,5", "2,5", "3", "12 KG")
  lectura <- .lectura_sufijo_sin_conversion(x)
  h <- .hallazgo_sufijo(lectura, "numero_como_texto")
  expect_identical(nrow(h), 1L)
  # A mano: `1,5` y `2,5` son los numeros con separador; `3` no lo trae y
  # `12 KG` no se convierte.
  expect_identical(as.numeric(h$n_afectados), 2)
  expect_identical(h$trazabilidad[[1L]]$indices_fila, 1:2)
  expect_true(grepl("1 valor lleva un sufijo", h$evidencia, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl('"12 KG"', h$evidencia, fixed = TRUE, useBytes = TRUE))
  # La coma decimal de `1,5` decide la convencion; `12 KG` no la contradice.
  expect_identical(as.character(lectura$fila$numero_texto_convencion), "decimal_coma")
  expect_false(as.logical(lectura$fila$numero_texto_ambiguo))
  # La conversion no se ejecuta mientras este `12 KG`: no la marca el plan y
  # los datos quedan como estaban.
  expect_false(as.logical(lectura$fila$numero_texto_seguro))
  expect_identical(nrow(lectura$conversion), 1L)
  expect_false(any(lectura$conversion$aplicar))
  expect_identical(lectura$aplicado, x)
})

test_that("un codigo que no es ISO 4217 no apaga numero_como_texto junto a un importe", {
  skip_on_cran()
  una_moneda <- .lectura_sufijo_sin_conversion(c("USD 12", "USD 14", "USD 15", "ABC 13"))
  h <- .hallazgo_sufijo(una_moneda, "numero_como_texto")
  expect_identical(nrow(h), 1L)
  expect_identical(as.numeric(h$n_afectados), 3)
  expect_true(grepl('"ABC 13"', h$evidencia, fixed = TRUE, useBytes = TRUE))
  # `ABC` sigue sin ser una moneda: la columna tiene una sola.
  expect_false("monedas_mixtas" %in% una_moneda$tipos)
  expect_identical(as.character(una_moneda$fila$numero_texto_moneda), "USD")
  expect_identical(una_moneda$aplicado, c("USD 12", "USD 14", "USD 15", "ABC 13"))

  dos_monedas <- .lectura_sufijo_sin_conversion(c("USD 12", "EUR 14", "USD 15", "ABC 13"))
  expect_identical(
    as.numeric(.hallazgo_sufijo(dos_monedas, "numero_como_texto")$n_afectados), 3
  )
  mixtas <- .hallazgo_sufijo(dos_monedas, "monedas_mixtas")
  expect_identical(as.numeric(mixtas$n_afectados), 3)
  expect_true(grepl("USD (2); EUR (1)", mixtas$evidencia, fixed = TRUE, useBytes = TRUE))
})

test_that("un 12 KG no apaga numero_como_texto junto a numeros con unidad", {
  skip_on_cran()
  metros <- .lectura_sufijo_sin_conversion(c("2 m", "3 m", "4 m", "12 KG"))
  h <- .hallazgo_sufijo(metros, "numero_como_texto")
  expect_identical(as.numeric(h$n_afectados), 3)
  expect_identical(as.character(metros$fila$numero_texto_unidad), "m")
  expect_identical(metros$aplicado, c("2 m", "3 m", "4 m", "12 KG"))

  mezcla <- .lectura_sufijo_sin_conversion(c("2 m", "3 kg", "4 m", "12 KG"))
  expect_identical(
    as.numeric(.hallazgo_sufijo(mezcla, "numero_como_texto")$n_afectados), 3
  )
  # `KG` no es una unidad -termina en mayuscula-: las observadas son dos.
  unidades <- .hallazgo_sufijo(mezcla, "unidades_mixtas")
  expect_identical(as.numeric(unidades$n_afectados), 3)
  expect_true(grepl("m (2); kg (1)", unidades$evidencia, fixed = TRUE, useBytes = TRUE))
})

test_that("el borde del 80 % cuenta los numeros con sufijo y no el texto", {
  skip_on_cran()
  # A mano: siete numeros y un `12 KG` entre diez valores son el 80 %.
  justo <- .lectura_sufijo_sin_conversion(c(rep("1,5", 7L), "12 KG", "abc", "def"))
  expect_true("numero_como_texto" %in% justo$tipos)
  # Uno menos -seis y el `12 KG`, el 70 %- no alcanza.
  debajo <- .lectura_sufijo_sin_conversion(c(rep("1,5", 6L), "12 KG", "abc", "def", "ghi"))
  expect_false("numero_como_texto" %in% debajo$tipos)
})

test_that("una columna declarada codigo no recibe numero_como_texto y lo declara", {
  skip_on_cran()
  lectura <- .lectura_sufijo_sin_conversion(
    c("1,5", "2,5", "3", "12 KG"), columnas_codigo = "x"
  )
  expect_false("numero_como_texto" %in% lectura$tipos)
  cobertura <- lectura$perfil$cobertura_diagnosticos
  expect_identical(
    sum(cobertura$diagnostico == "numero_como_texto" & cobertura$columna == "x"),
    1L
  )
})

test_that("un codigo ISO 4217 delante y una unidad tras un salto de linea son un numero", {
  skip_on_cran()
  # El salto es el espacio entre el numero y la unidad, como en `$ 35\nkg`: se
  # lee igual que `USD 12 kg`. La extraccion del codigo no cruzaba el salto y
  # la validacion recibia el texto entero, que no es un codigo.
  salto <- .lectura_sufijo_sin_conversion(c("USD 12\nkg", "USD 13\nkg", "USD 14\nkg"))
  espacio <- .lectura_sufijo_sin_conversion(c("USD 12 kg", "USD 13 kg", "USD 14 kg"))
  expect_identical(as.integer(salto$fila$n_numeros_texto), 3L)
  expect_identical(as.character(salto$fila$numero_texto_moneda), "USD")
  expect_identical(as.character(salto$fila$numero_texto_unidad), "kg")
  expect_false("monedas_mixtas" %in% salto$tipos)
  expect_identical(
    as.numeric(.hallazgo_sufijo(salto, "numero_como_texto")$n_afectados), 3
  )
  campos <- c("n_numeros_texto", "proporcion_numeros_texto", "numero_texto_seguro",
              "numero_texto_unidad", "numero_texto_moneda", "numero_texto_convencion")
  expect_identical(as.list(salto$fila[campos]), as.list(espacio$fila[campos]))
  expect_identical(salto$aplicado, espacio$aplicado)
  expect_identical(salto$aplicado, c(12, 13, 14))
})

test_that("un 12 KG sigue sin convertirse", {
  skip_on_cran()
  x <- c("1,5", "2,5", "3", "12 KG")
  lectura <- .lectura_sufijo_sin_conversion(x)
  expect_identical(lectura$aplicado, x)
  # Marcada a mano, la conversion tampoco le quita el `KG`: el conversor no la
  # ejecuta.
  expect_error(
    .convertir_numero_regional(x, list(convencion = "decimal_coma")),
    "formato"
  )
  # Una columna de solo valores con sufijo no tiene numeros escritos como texto.
  solos <- .lectura_sufijo_sin_conversion(c("12 KG", "13 KG", "14 KG"))
  expect_identical(as.integer(solos$fila$n_numeros_texto), 0L)
  expect_false("numero_como_texto" %in% solos$tipos)
  expect_identical(nrow(solos$conversion), 0L)
  expect_identical(solos$aplicado, c("12 KG", "13 KG", "14 KG"))
})
