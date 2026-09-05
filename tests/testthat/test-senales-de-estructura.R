# Dos puertas del paquete respondian distinto a la misma pregunta sobre la misma
# tabla, y una senal se publicaba sobre cero observaciones.

# `detectar_claves()` tiene la regla escrita y documentada: un `double` con
# parte fraccionaria es un importe o una coordenada, no un identificador.
# `sugerir_clave()` no la aplicaba y ofrecia un importe como clave.
test_that("sugerir_clave y detectar_claves coinciden sobre los dobles", {
  datos <- data.frame(
    deuda = c(120.5, 87, 3100.25, 42, 999),
    nombre = c("ana", "bo", "caro", "diaz", "eva"),
    edad = c(34, 21, 55, 80, 12),
    id_registro = c(101, 102, 103, 104, 105),
    stringsAsFactors = FALSE
  )
  sugeridas <- sugerir_clave(datos)
  detectadas <- as.character(detectar_claves(datos)$columnas)

  # El hecho medido no se niega: `deuda` identifica cada fila. Lo que cambia es
  # que la reserva se dice y la columna baja en el orden.
  expect_true(sugeridas$identifica[sugeridas$columna == "deuda"])
  expect_match(
    sugeridas$motivo[sugeridas$columna == "deuda"], "parte fraccionaria"
  )
  expect_equal(sugeridas$columna[[nrow(sugeridas)]], "deuda")

  # Y las que sugiere primero son exactamente las que la otra puerta ofrece.
  primeras <- sugeridas$columna[seq_along(detectadas)]
  expect_setequal(primeras, detectadas)

  # La forma publica no cambio.
  expect_identical(
    names(sugeridas),
    c("columna", "identifica", "sin_faltantes", "tasa_distintos",
      "parecido_nombre", "motivo")
  )
})

# La mitad de control: un entero guardado como doble -lo que produce leer un
# CSV- SIGUE siendo candidato. Sin esto, excluir todos los dobles pasaria el
# test de arriba y romperia el caso que la regla existe para conservar.
test_that("un identificador entero guardado como doble sigue siendo candidato", {
  datos <- data.frame(
    id = c(1001, 1002, 1003, 1004, 1005),
    monto = c(10.5, 20.25, 30, 40.75, 50),
    stringsAsFactors = FALSE
  )
  sugeridas <- sugerir_clave(datos)
  expect_equal(sugeridas$columna[[1L]], "id")
  expect_false(grepl("fraccionaria", sugeridas$motivo[sugeridas$columna == "id"]))
  expect_true("id" %in% as.character(detectar_claves(datos)$columnas))
})

# Una columna sin un solo valor observado recibia escala "continua" con
# confianza 0.65 -la misma cifra que sobre mil observaciones- y una evidencia
# que describia el almacenamiento como si describiera los datos.
test_that("sin valores observados no se propone una escala", {
  datos <- data.frame(
    solo_na_num = as.numeric(rep(NA, 5)),
    solo_na_txt = as.character(rep(NA, 5)),
    stringsAsFactors = FALSE
  )
  clasificacion <- clasificar_variables(datos)
  expect_true(all(clasificacion$escala_propuesta == "desconocida"))
  expect_true(all(is.na(clasificacion$confianza)))
  expect_false(any(clasificacion$confirmada))
  expect_true(all(grepl("ningun valor observado", clasificacion$evidencia)))
})

# El control, y decide el alcance de la guarda: donde la CLASE determina la
# escala se mantiene, porque no depende de los datos; donde la determinan los
# valores y no hay ninguno, se dice desconocida.
test_that("la clase que determina la escala sigue declarandola sin datos", {
  datos <- data.frame(
    solo_na_log = as.logical(rep(NA, 5)),
    solo_na_fecha = as.Date(rep(NA, 5)),
    stringsAsFactors = FALSE
  )
  clasificacion <- clasificar_variables(datos)
  expect_equal(
    clasificacion$escala_propuesta[clasificacion$columna == "solo_na_log"],
    "binaria"
  )
  expect_equal(
    clasificacion$escala_propuesta[clasificacion$columna == "solo_na_fecha"],
    "temporal"
  )

  # Y con datos, la senal sigue proponiendo lo que proponia.
  con_datos <- data.frame(
    continua = c(1.5, 2.5, 3.5), discreta = c(1L, 2L, 3L),
    texto = c("a", "b", "c"), stringsAsFactors = FALSE
  )
  esperado <- c(continua = "continua", discreta = "discreta", texto = "nominal")
  observado <- clasificar_variables(con_datos)
  expect_equal(
    observado$escala_propuesta[match(names(esperado), observado$columna)],
    unname(esperado)
  )
  expect_false(any(is.na(observado$confianza)))
})
