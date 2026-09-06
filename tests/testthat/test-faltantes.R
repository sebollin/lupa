
# Las formas de DOS LETRAS SIN SEPARADOR no van en la lista por omision, por la
# misma razon por la que naniar publica 66, 77 y 88 y este paquete no los aplica
# solos: "pueden ser edades, codigos o anos legitimos".
#
# Medido sobre el banco de datos reales: `sd` y `nc` estaban en la lista, y la
# unica columna del banco que los contiene es una de codigos de estado de
# EE.UU., donde `perfilar()` publicaba `faltantes_disfrazados` con severidad
# **error** y evidencia "NC (59); SD (7)".
test_that("una forma de dos letras se juzga por el vocabulario, no sola", {
  # Un sistema de codigos: `SD` es Dakota del Sur. Antes se publicaba
  # `faltantes_disfrazados` con severidad error y evidencia "NC (59); SD (7)"
  # sobre los datos reales del banco de pruebas.
  estados <- data.frame(
    state = c("OR", "IN", "CA", "NC", "SD", "ND", "NY", "TX", "FL", "WA",
              "CO", "LA", "NC", "SD"),
    stringsAsFactors = FALSE
  )
  expect_gte(length(unique(estados$state)), 10L)
  perfil <- perfilar(estados)
  expect_equal(perfil$columnas$n_faltantes_disfrazados, 0L)
  expect_false("faltantes_disfrazados" %in% perfil$hallazgos$tipo_hallazgo)
  # Los valores siguen a la vista: no se ocultan, se dejan de acusar.
  expect_equal(perfil$columnas$n_distintos, length(unique(estados$state)))

  # Una columna de dos valores: ese mismo `SD` es "sin dato", y ahi si cuenta.
  for (par in list(c("SI", "SD"), c("SI", "NC"), c("A", "ND"))) {
    columna <- data.frame(
      valor = c(rep(par[[1L]], 95), rep(par[[2L]], 5)),
      stringsAsFactors = FALSE
    )
    resultado <- perfilar(
      columna, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_equal(
      resultado$columnas$n_faltantes_disfrazados, 5L,
      info = paste(par, collapse = "/")
    )
  }
})

# La familia "no disponible" faltaba entera: la lista tenia `s/d`, `s/i` y `n/c`
# con sus formas y `n/d` -de las abreviaturas mas comunes del espanol- no estaba
# en ninguna.
test_that("las abreviaturas de ausencia con separador se detectan", {
  detectadas <- function(valor) {
    datos <- data.frame(
      x = c("10", "20", valor, "30", "40", "10", "20", valor, "30", "40"),
      stringsAsFactors = FALSE
    )
    perfilar(datos)$columnas$n_faltantes_disfrazados
  }
  for (valor in c("s/d", "S/D", "s/i", "n/c", "n/d", "N/D", "n.d",
                  "no disponible", "sin dato", "sin datos", "n/a", "NA")) {
    expect_equal(detectadas(valor), 2L, info = valor)
  }
})

# `sentinelas_numericos` declara que NUMEROS son ausencia y no tenia equivalente
# textual: la lista de cadenas era fija. La asimetria se noto al hacer que las
# formas de dos letras dependan del vocabulario -quien tuviera un caso donde esa
# regla se equivoca no tenia donde decirlo-. Es la regla del paquete: excluir lo
# que el usuario declara.
test_that("las cadenas de ausencia se pueden declarar", {
  estados <- data.frame(
    state = c("OR", "IN", "CA", "NC", "SD", "ND", "NY", "TX", "FL", "WA",
              "CO", "LA", "NC", "SD"),
    stringsAsFactors = FALSE
  )
  # Sin declarar, la guarda del vocabulario protege al codigo de dos letras.
  expect_equal(perfilar(estados)$columnas$n_faltantes_disfrazados, 0L)
  # Declarado, lo del usuario manda y atraviesa la guarda.
  declarado <- perfilar(estados, cadenas_ausencia = c("nc", "sd"))
  expect_equal(declarado$columnas$n_faltantes_disfrazados, 4L)

  # Y una cadena propia que no esta en ninguna lista incorporada.
  datos <- data.frame(
    x = c(rep("dato", 90), rep("XXX", 10)), stringsAsFactors = FALSE
  )
  expect_equal(perfilar(datos)$columnas$n_faltantes_disfrazados, 0L)
  expect_equal(
    perfilar(datos, cadenas_ausencia = "XXX")$columnas$n_faltantes_disfrazados,
    10L
  )
})

test_that("cadenas_ausencia se valida como su hermana numerica", {
  datos <- data.frame(x = c("a", "b", "c"), stringsAsFactors = FALSE)
  for (invalido in list(1:3, c("a", NA), c("a", ""), c("a", "  "))) {
    expect_error(
      perfilar(datos, cadenas_ausencia = invalido),
      "vector de texto"
    )
  }
  # NULL es el valor por omision y no cambia nada.
  expect_equal(
    perfilar(datos, cadenas_ausencia = NULL)$columnas$n_faltantes_disfrazados,
    perfilar(datos)$columnas$n_faltantes_disfrazados
  )
})
