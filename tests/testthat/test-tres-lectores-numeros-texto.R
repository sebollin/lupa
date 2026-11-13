# Una columna de numeros escritos como texto la leen tres partes del paquete: la
# inferencia del tipo, el resumen y la conversion del plan de limpieza. Las tres
# cuentan y leen lo mismo, y la frase del hallazgo dice lo que el plan hace. Lo
# que acompana a un numero se reconoce como unidad o como moneda, o el valor no
# es un numero: el que cuenta y el que convierte son el mismo lector.
testthat::skip_on_cran()

.fecha_tres_lectores <- as.POSIXct("2026-01-15", tz = "UTC")
.conversiones_tres_lectores <- c("convertir_tipo", "convertir_numero_regional")

# El perfil de la columna, su plan y lo que deja `aplicar()` con el plan tal
# como sale, sin tocar ninguna marca.
.tres_lectores <- function(x) {
  datos <- data.frame(x = x, stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, fecha = .fecha_tres_lectores, analizar_dependencias = FALSE
  )
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[
    as.character(plan$columna) %in% "x" &
      as.character(plan$estrategia) %in% .conversiones_tres_lectores, ,
    drop = FALSE
  ]
  hallazgos <- perfil$hallazgos[perfil$hallazgos$columna %in% "x", , drop = FALSE]
  list(
    datos = datos, fila = perfil$columnas, plan = plan, conversion = conversion,
    hallazgos = as.character(hallazgos$tipo_hallazgo),
    frase_tipo = as.character(
      hallazgos$evidencia[hallazgos$tipo_hallazgo == "tipo_declarado_distinto"]
    ),
    aplicado = aplicar(plan, datos)$datos$x
  )
}

# Lo que deja la conversion de la columna cuando se la marca a mano.
.marcar_conversion <- function(lectura) {
  plan <- lectura$plan
  plan$aplicar <- as.character(plan$columna) %in% "x" &
    as.character(plan$estrategia) %in% .conversiones_tres_lectores
  aplicar(plan, lectura$datos)$datos$x
}

.dice_no_pierde <- function(lectura) {
  length(lectura$frase_tipo) == 1L &&
    grepl("no pierde", lectura$frase_tipo, fixed = TRUE, useBytes = TRUE)
}

test_that("un separador inicial es evidencia de su convencion, y la mezcla se bloquea", {
  x <- c(rep(".5", 3L), rep("1.234,56", 4L), "7", "12,5")
  lectura <- .tres_lectores(x)
  fila <- lectura$fila
  expect_identical(as.character(fila$tipo_inferido), "doble")
  expect_identical(as.numeric(fila$proporcion_tipo_inferido), 1)
  expect_identical(as.character(fila$numero_texto_convencion), "mixta")
  expect_true(as.logical(fila$numero_texto_ambiguo))
  # A mano: los nueve valores se leen, cada uno con su propia evidencia.
  expect_identical(as.integer(fila$n_valores_excluidos_resumen), 0L)
  expect_equal(as.numeric(fila$maximo), 1234.56)
  expect_false(.dice_no_pierde(lectura))
  expect_identical(as.character(lectura$conversion$estado), "bloqueada")
  expect_identical(lectura$aplicado, x)
})

test_that("una columna y su gemela con el cero escrito se leen y se convierten igual", {
  sin_cero <- .tres_lectores(c(",5", ",25", "1.500", "2,75"))
  con_cero <- .tres_lectores(c("0,5", "0,25", "1.500", "2,75"))
  for (campo in c("tipo_inferido", "proporcion_tipo_inferido",
                  "numero_texto_convencion", "numero_texto_ambiguo",
                  "numero_texto_seguro", "n_valores_excluidos_resumen",
                  "maximo", "media")) {
    expect_identical(sin_cero$fila[[campo]], con_cero$fila[[campo]], info = campo)
  }
  expect_identical(as.character(sin_cero$fila$numero_texto_convencion),
                   "decimal_coma")
  expect_identical(as.character(sin_cero$conversion$estado),
                   as.character(con_cero$conversion$estado))
  # A mano: con la coma decimal, `1.500` es mil quinientos en las dos.
  expect_identical(sin_cero$aplicado, c(0.5, 0.25, 1500, 2.75))
  expect_identical(con_cero$aplicado, c(0.5, 0.25, 1500, 2.75))
})

test_that("la notacion cientifica se cuenta, se resume y se convierte al mismo numero", {
  x <- c("1e3", "2,5", "3.5e2", "4")
  lectura <- .tres_lectores(x)
  fila <- lectura$fila
  expect_identical(as.character(fila$tipo_inferido), "doble")
  expect_identical(as.numeric(fila$proporcion_tipo_inferido), 1)
  expect_identical(as.character(fila$numero_texto_convencion), "decimal_coma")
  expect_identical(as.integer(fila$n_valores_excluidos_resumen), 0L)
  # A mano: 1000, 2,5, 350 y 4.
  expect_identical(as.numeric(fila$media), mean(c(1000, 2.5, 350, 4)))
  expect_true(.dice_no_pierde(lectura))
  expect_identical(as.character(lectura$conversion$estado), "lista")
  expect_identical(lectura$aplicado, c(1000, 2.5, 350, 4))
})

test_that("cualquier forma del exponente es un numero y no un identificador", {
  x <- c("1.2E-05", "3.4E-07", "5.6E-03", "2.2E-04", "1e5", "2E+05")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "doble")
  expect_identical(as.integer(lectura$fila$n_valores_excluidos_resumen), 0L)
  expect_identical(as.numeric(lectura$fila$maximo), 2e5)
  expect_identical(lectura$aplicado, c(1.2e-5, 3.4e-7, 5.6e-3, 2.2e-4, 1e5, 2e5))
})

test_that("los miles de una columna con notacion cientifica no se convierten como decimales", {
  x <- c("12.500", "3.200", "45.000", "850,75", "1,2E+06", "3,4E+05",
         "7,8E+06", "2.150")
  lectura <- .tres_lectores(x)
  # A mano: con la coma decimal de `850,75`, `12.500` es doce mil quinientos.
  expect_identical(as.numeric(lectura$fila$maximo), 7800000)
  expect_identical(as.numeric(lectura$fila$minimo), 850.75)
  convertido <- if (is.numeric(lectura$aplicado)) {
    lectura$aplicado
  } else if (identical(as.character(lectura$conversion$estado), "lista")) {
    .marcar_conversion(lectura)
  } else NULL
  # Si el plan convierte, convierte al numero que el resumen leyo.
  if (!is.null(convertido)) {
    expect_identical(
      convertido, c(12500, 3200, 45000, 850.75, 1.2e6, 3.4e5, 7.8e6, 2150)
    )
  } else {
    expect_false(.dice_no_pierde(lectura))
  }
})

test_that("la frase no dice que no se pierde nada si la conversion regional queda bloqueada", {
  # Cuatro de cinco valores responden al formato regional: el plan convierte
  # por esa via, que no lee la notacion cientifica.
  x <- c("3425.06", "1662.67", "4000.60", "5195.35", "1,5e-0")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "doble")
  expect_identical(as.numeric(lectura$fila$proporcion_tipo_inferido), 1)
  expect_identical(as.character(lectura$conversion$estrategia),
                   "convertir_numero_regional")
  expect_identical(as.character(lectura$conversion$estado), "bloqueada")
  expect_false(.dice_no_pierde(lectura))
  # La frase nombra el motivo con las palabras del plan.
  motivo <- paste0("formato num", intToUtf8(233L), "rico regional")
  expect_true(grepl(motivo, lectura$frase_tipo, fixed = TRUE, useBytes = TRUE))
  expect_identical(lectura$aplicado, x)
})

test_that("sobre un banco fijo la frase, la cuenta y la conversion coinciden", {
  banco <- list(
    plano = c("1", "2", "3", "4"),
    inicial = c(".5", ".25", "1.5", "2"),
    cientifica = c("1e3", "2,5", "3.5e2", "4"),
    regional = c("1.234,56", "2.345,67", "3.456,78", "987,10"),
    mixta = c("1,5", "2.5", "3", "4,25"),
    regional_y_cientifica = c("3425.06", "1662.67", "4000.60", "5195.35", "1,5e-0"),
    decimal_entre_enteros = c("10", "12", "9", "15", "9,5"),
    fuera_de_entero = c("3000000000", "5"),
    con_vacios = c("", " ", "1.500", "2,5", "3,5"),
    con_unidad = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "12 kg"),
    con_codigo = c("1,5", "2,5", "3", "4", "5", "6", "7", "8", "9", "12 KG")
  )
  for (nombre in names(banco)) {
    x <- banco[[nombre]]
    lectura <- .tres_lectores(x)
    fila <- lectura$fila
    leidos <- .leer_numeros_texto(x, as.character(fila$numero_texto_convencion))
    # La inferencia cuenta como doble lo que el lector lee.
    candidatos <- inferir_tipo(x)$candidatos
    expect_identical(
      as.integer(candidatos$compatibles[candidatos$tipo == "doble"]),
      as.integer(sum(is.finite(leidos))), info = nombre
    )
    # La frase solo promete que no se pierde nada si hay una conversion lista.
    if (.dice_no_pierde(lectura)) {
      expect_true(any(as.character(lectura$conversion$estado) == "lista"),
                  info = nombre)
    }
    # Lo que el plan convierte por omision es lo que el lector leyo, y no deja
    # sin valor una celda que lo tenia.
    if (is.numeric(lectura$aplicado)) {
      presentes <- !is.na(x) & nzchar(trimws(x))
      expect_identical(as.numeric(lectura$aplicado[presentes]),
                       leidos[presentes], info = nombre)
      expect_false(anyNA(lectura$aplicado[presentes]), info = nombre)
    }
  }
})

test_that("un sufijo en mayusculas no es una unidad aunque el valor traiga punto decimal", {
  x <- c("1.5TB", "500GB", "250MB", "2TB")
  lectura <- .tres_lectores(x)
  # Contaba `1.5TB` -por su punto- sin unidad que publicar, y la conversion
  # que proponia marcada dejaba 1.5, 500, 250 y 2.
  expect_identical(as.integer(lectura$fila$n_numeros_texto), 0L)
  expect_identical(as.character(lectura$fila$numero_texto_unidad), "")
  expect_false("numero_como_texto" %in% lectura$hallazgos)
  expect_identical(nrow(lectura$conversion), 0L)
  expect_identical(lectura$aplicado, x)
})

test_that("un sufijo en mayusculas no es una unidad aunque el valor traiga coma decimal", {
  x <- c("850K", "1,5M", "2M", "300K")
  lectura <- .tres_lectores(x)
  expect_identical(as.integer(lectura$fila$n_numeros_texto), 0L)
  expect_false("numero_como_texto" %in% lectura$hallazgos)
  expect_identical(nrow(lectura$conversion), 0L)
  expect_identical(lectura$aplicado, x)
})

test_that("un valor con sufijo en mayusculas no se convierte junto a los numeros de la columna", {
  x <- c("1,5", "2,5", "3", "4", "5", "6", "7", "8", "9", "12 KG")
  lectura <- .tres_lectores(x)
  # A mano: nueve de diez responden al formato; `12 KG` no es un numero.
  expect_identical(as.numeric(lectura$fila$proporcion_numeros_texto), 0.9)
  expect_false(as.logical(lectura$fila$numero_texto_seguro))
  expect_false(any(lectura$conversion$aplicar))
  expect_identical(lectura$aplicado, x)
  # El conversor es el mismo lector: marcada a mano tampoco le quita el `KG`.
  expect_error(
    .convertir_numero_regional(x, list(convencion = "decimal_coma")),
    "formato"
  )
})

test_that("tres mayusculas son una moneda solo si son un codigo ISO 4217", {
  leyes <- c("LEY 18331", "LEY 19355", "LEY 17930", "LEY 16000")
  lectura <- .tres_lectores(leyes)
  expect_identical(as.integer(lectura$fila$n_numeros_texto), 0L)
  expect_identical(as.character(lectura$fila$numero_texto_moneda), "")
  expect_false("numero_como_texto" %in% lectura$hallazgos)
  expect_identical(nrow(lectura$conversion), 0L)
  expect_identical(lectura$aplicado, leyes)
  # Detras del numero, lo mismo.
  detras <- .tres_lectores(c("35 LEY", "40 LEY", "12 LEY", "7 LEY"))
  expect_identical(as.integer(detras$fila$n_numeros_texto), 0L)
  expect_identical(nrow(detras$conversion), 0L)

  # Control: un codigo ISO 4217 sigue siendo una moneda, delante y detras.
  importes <- .tres_lectores(c("USD 35", "USD 40", "USD 12", "USD 7"))
  expect_identical(as.integer(importes$fila$n_numeros_texto), 4L)
  expect_identical(as.character(importes$fila$numero_texto_moneda), "USD")
  expect_identical(importes$aplicado, c(35, 40, 12, 7))
  importes_detras <- .tres_lectores(c("35 UYU", "40 UYU", "12 UYU", "7 UYU"))
  expect_identical(as.character(importes_detras$fila$numero_texto_moneda), "UYU")
  expect_identical(importes_detras$aplicado, c(35, 40, 12, 7))

  # Una sola moneda falsa entre importes: ese valor no es un numero, y la
  # columna deja de ser una mezcla de dos monedas.
  mezcla <- .tres_lectores(c("USD 10", "LEY 20", "USD 30", "USD 40"))
  expect_identical(as.integer(mezcla$fila$n_numeros_texto), 3L)
  expect_identical(as.numeric(mezcla$fila$proporcion_numeros_texto), 0.75)
  expect_false("monedas_mixtas" %in% mezcla$hallazgos)
  expect_identical(nrow(mezcla$conversion), 0L)
})

test_that("un sufijo en minusculas sigue siendo una unidad", {
  kilos <- .tres_lectores(c("12 kg", "9 kg", "11 kg", "7 kg"))
  expect_identical(as.integer(kilos$fila$n_numeros_texto), 4L)
  expect_identical(as.character(kilos$fila$numero_texto_unidad), "kg")
  expect_identical(kilos$aplicado, c(12, 9, 11, 7))

  capacidades <- .tres_lectores(c("1.5tb", "500gb", "250mb", "2tb"))
  expect_identical(as.integer(capacidades$fila$n_numeros_texto), 4L)
  expect_identical(as.character(capacidades$fila$numero_texto_unidad), "")
  expect_true("unidades_mixtas" %in% capacidades$hallazgos)
  expect_identical(as.character(capacidades$conversion$estado), "lista")
  expect_false(capacidades$conversion$aplicar)

  bytes <- .tres_lectores(as.character(as.raw(0:39)))
  # A mano: de `00` a `27`, doce llevan letra (`0a`...`0f`, `1a`...`1f`).
  expect_identical(as.integer(bytes$fila$n_numeros_texto), 12L)
  expect_true("unidades_mixtas" %in% bytes$hallazgos)
  expect_false(bytes$conversion$aplicar)
})

test_that("una columna inferida identificador ofrece la conversion sin marcar", {
  x <- c("101a", "102a", "103a", "104a")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "identificador")
  expect_identical(as.character(lectura$fila$numero_texto_unidad), "a")
  expect_identical(as.character(lectura$conversion$estado), "lista")
  expect_false(lectura$conversion$aplicar)
  expect_false(lectura$conversion$recomendada)
  expect_identical(lectura$aplicado, x)
  # Quien sabe que es una magnitud la marca, y se convierte.
  expect_identical(.marcar_conversion(lectura), c(101, 102, 103, 104))
})

test_that("el resumen no lee un numero que trae unidad", {
  x <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "12 kg")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "entero")
  # A mano: `12 kg` queda fuera; el maximo es 9 y la media de 1 a 9 es 5.
  expect_identical(as.integer(lectura$fila$n_valores_excluidos_resumen), 1L)
  expect_identical(as.numeric(lectura$fila$maximo), 9)
  expect_identical(as.numeric(lectura$fila$media), 5)
  expect_identical(.leer_numeros_texto(c("12 kg", "USD 35", "5%", "7")),
                   c(NA, NA, NA, 7))
})

test_that("un decimal entre enteros se publica como doble y no anuncia una perdida", {
  x <- c("10", "12", "9", "15", "9,5")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "doble")
  expect_identical(as.numeric(lectura$fila$proporcion_tipo_inferido), 1)
  expect_true(.dice_no_pierde(lectura))
  expect_identical(lectura$aplicado, c(10, 12, 9, 15, 9.5))
})

test_that("un entero que no entra en integer no promete una conversion sin perdida", {
  x <- c("3000000000", "5")
  lectura <- .tres_lectores(x)
  expect_identical(as.character(lectura$fila$tipo_inferido), "entero")
  # El plan no cambia: la conversion a entero no se ejecuta.
  expect_identical(as.character(lectura$conversion$estado), "bloqueada")
  expect_identical(lectura$aplicado, x)
  expect_false(.dice_no_pierde(lectura))
  expect_true(grepl("no entra en un entero", lectura$frase_tipo, fixed = TRUE,
                    useBytes = TRUE))

  # Control: en el borde del rango la conversion sigue lista y sin perdida.
  borde <- .tres_lectores(c("2147483647", "5"))
  expect_true(.dice_no_pierde(borde))
  expect_identical(borde$aplicado, c(2147483647L, 5L))
})

test_that("una celda vacia o de espacios no cuenta como un valor que la conversion pierde", {
  x <- c("", " ", "1.500", "2,5", "3,5")
  lectura <- .tres_lectores(x)
  expect_identical(as.integer(lectura$fila$n_valores_excluidos_resumen), 2L)
  expect_identical(as.character(lectura$conversion$estado), "lista")
  expect_identical(as.numeric(lectura$conversion$n_afectadas), 5)
  expect_identical(lectura$aplicado, c(NA, NA, 1500, 2.5, 3.5))

  enteros <- .tres_lectores(c(" ", "5", "6"))
  expect_true(.dice_no_pierde(enteros))
  expect_identical(as.character(enteros$conversion$estado), "lista")
  expect_identical(enteros$aplicado, c(NA, 5L, 6L))
})

test_that("un exponente que desborda el doble no es un numero", {
  lectura <- .tres_lectores(c("1e400", "2.5", "3.5"))
  expect_identical(as.character(lectura$fila$tipo_inferido), "texto")
  expect_false(any(lectura$conversion$aplicar))
  expect_identical(lectura$aplicado, c("1e400", "2.5", "3.5"))
})

test_that("la justificacion del porcentaje no promete un rango que no comprueba", {
  lectura <- .tres_lectores(c("150%", "50%"))
  expect_identical(lectura$aplicado, c(1.5, 0.5))
  expect_false(grepl("quedan como proporci", lectura$conversion$justificacion,
                     fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("fuera de [0, 1]", lectura$conversion$justificacion,
                    fixed = TRUE, useBytes = TRUE))
})
