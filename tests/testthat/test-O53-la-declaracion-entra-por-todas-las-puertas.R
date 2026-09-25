# `columnas_personales` es el mecanismo que el paquete ofrece para declarar
# personal una columna que su lexico no reconoce -un legajo interno, una edad-.
# `medir()` ya la aceptaba; `distribucion_valores()`, `clasificar_variables()` y
# `detectar_discordancias()` no, y sin perfil la clasificacion corre solo por
# lexico y por forma: `distribucion_valores(datos)` publicaba los veinte valores
# mas frecuentes y el maximo exacto de una columna que `perfilar(datos,
# columnas_personales = ...)` enmascara. La declaracion entra ahora por las tres
# puertas.

tabla_o53 <- function() {
  set.seed(53)
  data.frame(
    legajo = c(rep("ZZUNICO1", 10), rep("ZZUNICO2", 10)),
    zona = c(rep("norte", 10), rep("sur", 10)),
    edad = c(555, sample(20:60, 19, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

publica_o53 <- function(objeto, literal) {
  partes <- if (is.list(objeto) && !is.data.frame(objeto)) objeto else list(objeto)
  textos <- unlist(lapply(partes, function(parte) {
    capture.output(print(parte))
  }))
  any(grepl(literal, textos, fixed = TRUE))
}

test_that("perfilar declara personal lo que se le declara", {
  # El control de que la declaracion significa algo: sin ella, el lexico no
  # reconoce ni `legajo` ni `edad`.
  datos <- tabla_o53()
  sin <- perfilar(datos, analizar_dependencias = FALSE)
  con <- perfilar(datos, columnas_personales = c("legajo", "edad"),
                  analizar_dependencias = FALSE)

  expect_equal(nrow(sin$datos_personales), 0L)
  expect_setequal(
    con$datos_personales$columna[con$datos_personales$proteger],
    c("legajo", "edad")
  )
})

test_that("distribucion_valores acepta la declaracion", {
  datos <- tabla_o53()

  sin_declarar <- distribucion_valores(datos)
  declarando <- distribucion_valores(
    datos, columnas_personales = c("legajo", "edad")
  )

  expect_true(publica_o53(sin_declarar, "ZZUNICO1"))
  expect_true(publica_o53(sin_declarar, "555"))
  expect_false(publica_o53(declarando, "ZZUNICO1"))
  expect_false(publica_o53(declarando, "555"))
  # Y el cuantil protegido lo declara en vez de callar.
  cuantiles <- declarando$cuantiles
  protegidos <- cuantiles[cuantiles$columna == "edad", , drop = FALSE]
  expect_true(all(as.character(protegidos$estado) == "valor_protegido"))
})

test_that("clasificar_variables acepta la declaracion", {
  datos <- tabla_o53()

  expect_true(publica_o53(clasificar_variables(datos), "ZZUNICO1"))
  expect_false(publica_o53(
    clasificar_variables(datos, columnas_personales = "legajo"), "ZZUNICO1"
  ))
})

test_that("detectar_discordancias acepta la declaracion", {
  datos <- tabla_o53()
  senal <- senal_redundante(c("legajo", "zona"), nombre = "s1")

  expect_true(publica_o53(detectar_discordancias(datos, senal), "ZZUNICO1"))
  expect_false(publica_o53(
    detectar_discordancias(datos, senal, columnas_personales = "legajo"),
    "ZZUNICO1"
  ))
})

test_that("pasar el perfil equivale a declarar en la puerta", {
  # Las dos vias tienen que proteger lo mismo: si difirieran, una de las dos
  # estaria describiendo otra clasificacion.
  datos <- tabla_o53()
  perfil <- perfilar(datos, columnas_personales = c("legajo", "edad"),
                     analizar_dependencias = FALSE)

  por_perfil <- distribucion_valores(datos, perfil = perfil)
  por_declaracion <- distribucion_valores(
    datos, columnas_personales = c("legajo", "edad")
  )

  expect_equal(por_perfil$frecuencias$valor, por_declaracion$frecuencias$valor)
  expect_equal(
    as.character(por_perfil$cuantiles$estado),
    as.character(por_declaracion$cuantiles$estado)
  )
})
