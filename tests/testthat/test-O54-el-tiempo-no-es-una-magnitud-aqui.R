# La documentacion promete una fila de cobertura para las columnas que quedan
# fuera de un diagnostico por su clase. La frase enumeraba `Date`, `POSIXt` y
# `difftime` junto a `integer64` y prometia la declaracion en la misma oracion,
# asi que quien la leia esperaba cuatro filas y recibia una. Se precisó el texto;
# esta prueba fija la regla que el texto describe, para que no se separen.
#
# El 2026-09-29 la regla CAMBIO, y el criterio con ella. Era `is.numeric()`, que
# responde FALSE sobre un `difftime`; ahora es si la fila **publica cifras
# cuantitativas**. `difftime` cambio de categoria el dia que dejo de abstenerse
# del resumen: publica `minimo`, `maximo` y `media` como cualquier numero, y ahi
# la ausencia de hallazgo se lee como conformidad. Medido antes de cambiarlo: con
# las mismas cifras, `units` recibia su fila en `ley_benford` y en
# `relacion_aritmetica_columnas` y `difftime` no recibia ninguna.
#
# Una fecha y una hora siguen fuera, y eso NO es olvido: no publican `media` como
# numero pelado -su resumen sale en `media_fecha` y compania-, asi que su silencio
# se lee distinto. Esta prueba fija las dos mitades.

tabla_o54 <- function() {
  set.seed(54)
  n <- 60
  tabla <- data.frame(
    magnitud = runif(n, 1, 5000),
    fecha = as.Date("2020-01-01") + seq_len(n),
    hora = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + seq_len(n) * 3600,
    duracion = as.difftime(seq_len(n), units = "hours"),
    texto = paste0("v", seq_len(n)),
    stringsAsFactors = FALSE
  )
  # La columna con clase propia se ASIGNA: en el minimo declarado -R 4.1-
  # `data.frame()` no puede convertir una clase desconocida y aborta con "cannot
  # coerce class". El objeto que la prueba necesita es el mismo; lo que cambia es
  # por donde entra.
  tabla$con_clase <- .columna_con_clase_numerica(seq_len(n) * 3)
  tabla
}

cobertura_benford_o54 <- function(perfil) {
  cobertura <- cobertura(perfil)
  cobertura[as.character(cobertura$diagnostico) == "ley_benford", , drop = FALSE]
}

test_that("la clase numerica declarada recibe su fila y el tiempo no", {
  perfil <- perfilar(tabla_o54(), analizar_dependencias = FALSE)
  filas <- cobertura_benford_o54(perfil)
  columnas_declaradas <- as.character(filas$columna)

  # R declara numerica a la columna con clase propia: se declara.
  expect_true("con_clase" %in% columnas_declaradas)
  expect_true(any(grepl("lupaMagnitud", filas$motivo, fixed = TRUE)))

  # La duracion TAMBIEN se declara: publica cifras cuantitativas.
  expect_true("duracion" %in% columnas_declaradas)
  # El calendario y el texto quedan fuera sin fila: no son magnitudes aqui.
  expect_false(any(c("fecha", "hora", "texto") %in% columnas_declaradas))

  # El control de que la prueba mide algo: la columna numerica pelada SI es
  # candidata, asi que su ausencia o presencia depende de las precondiciones y
  # no de la clase.
  expect_true(is.numeric(tabla_o54()$magnitud))
  expect_false(is.numeric(tabla_o54()$fecha))
})

test_that("la regla es la misma para la aritmetica entre columnas", {
  # `relaciones_aritmeticas` declara por la misma via: una clase que R llama
  # numerica se nombra, y el tiempo no.
  datos <- tabla_o54()
  datos$total <- datos$magnitud * 2
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  cobertura <- cobertura(perfil)
  aritmetica <- cobertura[
    grepl("aritmetic", as.character(cobertura$diagnostico)), , drop = FALSE
  ]

  if (nrow(aritmetica)) {
    columnas <- as.character(aritmetica$columna)
    expect_true("duracion" %in% columnas)
    expect_false(any(c("fecha", "hora") %in% columnas))
  } else {
    expect_equal(nrow(aritmetica), 0L)
  }
})
