# O27: dos cifras que se publicaban sin poder sostenerse. El denominador de los
# diagnosticos que salen del resumen cuantitativo contaba filas que el resumen
# nunca vio, y una accion recomendada que pierde el valor de la celda no lo
# decia en el unico texto que se lee antes de aplicarla.

test_that("outliers no cuenta como evaluadas las filas ausentes", {
  set.seed(270)
  valores <- stats::rnorm(1000)
  valores[3] <- 999
  valores[501:550] <- NA
  datos <- data.frame(id = seq_len(1000), num = valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "num", , drop = FALSE]
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers" &
      perfil$hallazgos$columna == "num", ,
    drop = FALSE
  ]

  expect_equal(fila$n_faltantes[[1L]], 50L)
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_evaluados[[1L]], 950)
  expect_equal(as.character(hallazgo$unidad_conteo[[1L]]), "fila")
})

test_that("sin ausentes el denominador sigue siendo la columna entera", {
  # Control: lo que cambia es lo que no llego al resumen, no el diagnostico.
  set.seed(271)
  valores <- stats::rnorm(1000)
  valores[3] <- 999
  datos <- data.frame(id = seq_len(1000), num = valores)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers" &
      perfil$hallazgos$columna == "num", ,
    drop = FALSE
  ]

  expect_equal(hallazgo$n_evaluados[[1L]], 1000)
})

test_that("los ceros y negativos no permitidos descuentan los ausentes", {
  # El test hermano de `test-alcance-sin-muestreo.R` mide el mismo denominador
  # con cien valores que no convierten. Esta es la otra mitad, la que nunca se
  # habia probado: cien ausentes y ningun valor no convertible.
  utiles <- c(rep("0", 50L), rep("-5", 50L), rep("100", 800L))
  datos <- data.frame(x = c(utiles, rep(NA_character_, 100L)),
                      stringsAsFactors = FALSE)

  perfil <- perfilar(datos, columnas_sin_ceros = "x",
                     columnas_no_negativas = "x", analizar_dependencias = FALSE)

  expect_equal(perfil$columnas$n, 1000L)
  expect_equal(perfil$columnas$n_faltantes, 100L)
  expect_equal(perfil$columnas$n_valores_excluidos_resumen, 0L)
  sobre_resumen <- c("outliers", "ceros_no_permitidos",
                     "negativos_no_permitidos")
  filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% sobre_resumen, , drop = FALSE
  ]
  expect_true(nrow(filas) >= 1L)
  expect_true(all(filas$n_evaluados == 900))

  # Control: un diagnostico que cuenta FILAS conserva la columna entera.
  por_filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "filas_duplicadas", , drop = FALSE
  ]
  if (nrow(por_filas)) expect_true(all(por_filas$n_evaluados == 1000))
})

test_that("la accion que quita un caracter declara lo que el registro cuenta", {
  # La justificacion nombra el conteo, y el conteo es condicional: cuenta las
  # celdas que al quitar el caracter quedan iguales a otra que era distinta.
  # La primera version de esta prueba exigia que contara siempre, que era la
  # regla por accion; `test-O35` mide las dos mitades.
  datos <- data.frame(
    x = rep(c("dato", paste0("otro", intToUtf8(1)), "tres", "cuatro"), 25),
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "eliminar_controles_invisibles", ,
               drop = FALSE]

  expect_equal(nrow(fila), 1L)
  expect_true(fila$aplicar[[1L]])
  expect_match(fila$justificacion[[1L]], "n_no_reversibles", fixed = TRUE)
  expect_match(fila$justificacion[[1L]], "quedan iguales", fixed = TRUE)

  registro <- suppressMessages(aplicar(plan, datos))$registro
  ejecutada <- registro[
    registro$estrategia == "eliminar_controles_invisibles", ,
    drop = FALSE
  ]
  # Aca `otro<control>` no colapsa con ninguna otra fila: el valor no se
  # perdio y el conteo lo dice.
  expect_equal(ejecutada$n_cambiadas[[1L]], 25)
  expect_equal(ejecutada$n_no_reversibles[[1L]], 0)
})

test_that("recortar espacios no anuncia una perdida que no tiene", {
  # Control: la declaracion nueva es de la accion que pierde el valor, no de
  # todas. El registro de `recortar_espacios` cuenta cero irreversibles.
  datos <- data.frame(x = rep(c("dato ", " otro", "tres", "cuatro"), 25),
                      stringsAsFactors = FALSE)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  fila <- as.data.frame(plan)
  fila <- fila[fila$estrategia == "recortar_espacios", , drop = FALSE]

  expect_false(grepl("n_no_reversibles", fila$justificacion[[1L]], fixed = TRUE))
  registro <- suppressMessages(aplicar(plan, datos))$registro
  ejecutada <- registro[registro$estrategia == "recortar_espacios", ,
                        drop = FALSE]
  expect_equal(ejecutada$n_no_reversibles[[1L]], 0)
})

test_that("los presentes no finitos cuentan como excluidos del resumen", {
  # Los dos README prometen que un resumen que deja afuera `NaN` o `Inf` lo
  # dice: los cuenta en `n_valores_excluidos_resumen` y el estado deja de decir
  # "calculados". Con 28 finitos, un `Inf` y un `-Inf` decia `calculados` y
  # `0` excluidos, con el minimo y la media ya calculados sin ellos.
  set.seed(272)
  datos <- data.frame(x = c(stats::rnorm(28), Inf, -Inf), id = seq_len(30L))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados_sobre_valores")
  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 2L)
  expect_equal(fila$n_infinito_positivo[[1L]], 1L)
  expect_equal(fila$n_infinito_negativo[[1L]], 1L)
  expect_true(is.finite(fila$minimo[[1L]]))

  # Y queda declarado donde el paquete declara lo que no midio, con el desglose.
  cobertura <- perfil$cobertura_diagnosticos
  cobertura <- cobertura[cobertura$diagnostico == "resumen_cuantitativo", ,
                         drop = FALSE]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo[[1L]], "no finitos", fixed = TRUE)
  expect_false(grepl("no pudo convertir", cobertura$motivo[[1L]], fixed = TRUE))
})

test_that("una columna sin un solo valor utilizable no dice calculados", {
  # Los tres casos comparten que no se calculo nada, y NO comparten por que: la
  # de treinta `Inf` tiene treinta valores presentes -`n_faltantes = 0`,
  # `n_distintos = 1`- y ninguno sirve; las de `NaN` y `NA` no tienen ninguno,
  # porque el paquete cuenta el `NaN` como ausente. Un solo nombre para las dos
  # afirmaciones era falso en la primera. El estado los separa y el campo que
  # decide es `n_faltantes`, que es la nocion de presencia del paquete.
  casos <- list(
    todo_inf = list(valores = rep(Inf, 30L), estado = "sin_valores_utilizables",
                    presentes = 30L),
    todo_nan = list(valores = rep(NaN, 30L), estado = "sin_valores",
                    presentes = 0L),
    todo_na = list(valores = rep(NA_real_, 30L), estado = "sin_valores",
                   presentes = 0L)
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    datos <- data.frame(x = caso$valores, id = seq_len(30L))
    perfil <- perfilar(datos, analizar_dependencias = FALSE)
    fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

    expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
                 caso$estado, info = nombre)
    # El estado tiene que coincidir con el conteo de presentes de la MISMA fila,
    # **en el universo que el resumen mide**: si no, la fila se contradiria a si
    # misma. La primera version de esta afirmacion usaba `n - n_faltantes`, que
    # omite la aplicabilidad declarada y solo coincide cuando no hay ninguna; el
    # bloque de abajo cubre ese caso para que la omision no vuelva.
    expect_equal(
      fila$n_aplicables[[1L]] - fila$n_faltantes[[1L]], caso$presentes,
      info = nombre
    )
    expect_true(is.na(fila$minimo[[1L]]), info = nombre)
    expect_true(is.na(fila$media[[1L]]), info = nombre)
  }

  # Y la otra puerta de la misma afirmacion: los centinelas se llevan todos los
  # valores de una columna que SI los tenia.
  con_centinelas <- perfilar(
    data.frame(x = rep(-999, 6L), id = seq_len(6L)),
    sentinelas_numericos = -999, analizar_dependencias = FALSE
  )
  fila_centinelas <- con_centinelas$columnas[
    con_centinelas$columnas$columna == "x", , drop = FALSE
  ]
  expect_equal(
    as.character(fila_centinelas$estado_resumen_cuantitativo[[1L]]),
    "sin_valores_utilizables"
  )
  expect_equal(fila_centinelas$n_valores_excluidos_resumen[[1L]], 6L)

  # Con aplicabilidad declarada, el universo que el resumen mide es otro, y el
  # estado se lee contra ESE universo. Son los casos que faltaban.
  universo_vacio <- perfilar(
    data.frame(v = c(1, 2, 3, 4, 5)),
    aplicabilidad = list(v = ~ FALSE), analizar_dependencias = FALSE
  )
  fila_vacio <- universo_vacio$columnas[
    universo_vacio$columnas$columna == "v", , drop = FALSE
  ]
  # Cinco valores en la columna y CERO en el universo: `sin_valores` es cierto de
  # lo que el resumen mide, y los cinco quedan declarados aparte.
  expect_equal(fila_vacio$n[[1L]], 5)
  expect_equal(fila_vacio$n_aplicables[[1L]], 0)
  expect_equal(fila_vacio$n_presentes_fuera_de_aplicabilidad[[1L]], 5)
  expect_equal(
    as.character(fila_vacio$estado_resumen_cuantitativo[[1L]]), "sin_valores"
  )
  expect_equal(
    fila_vacio$n_aplicables[[1L]] - fila_vacio$n_faltantes[[1L]], 0
  )

  # Y el caso que SI es `sin_valores_utilizables` con aplicabilidad: el universo
  # tiene valores presentes y ninguno es finito.
  con_infinitos <- perfilar(
    data.frame(v = c(Inf, Inf, 3, 4, 5), g = c(1, 1, 2, 2, 2)),
    aplicabilidad = list(v = ~ g == 1), analizar_dependencias = FALSE
  )
  fila_infinitos <- con_infinitos$columnas[
    con_infinitos$columnas$columna == "v", , drop = FALSE
  ]
  expect_equal(fila_infinitos$n_aplicables[[1L]], 2)
  expect_equal(
    fila_infinitos$n_aplicables[[1L]] - fila_infinitos$n_faltantes[[1L]], 2
  )
  expect_equal(
    as.character(fila_infinitos$estado_resumen_cuantitativo[[1L]]),
    "sin_valores_utilizables"
  )

  # Y una columna vacia, que es el caso donde `sin_valores` si es cierto.
  vacia <- perfilar(
    data.frame(x = numeric(0), id = integer(0)), analizar_dependencias = FALSE
  )
  expect_equal(
    as.character(
      vacia$columnas$estado_resumen_cuantitativo[
        vacia$columnas$columna == "x"
      ][[1L]]
    ),
    "sin_valores"
  )

  # Control: con valores utilizables el estado sigue siendo `calculados`.
  set.seed(273)
  datos <- data.frame(x = stats::rnorm(30L), id = seq_len(30L))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]
  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados")
  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 0L)
})

test_that("un centinela declarado sigue contandose como antes", {
  # Control de la cuenta nueva: lo que ya se excluia no cambia de numero.
  set.seed(274)
  datos <- data.frame(x = c(stats::rnorm(90), rep(-999, 10L)),
                      id = seq_len(100L))

  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     sentinelas_numericos = -999)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]

  expect_equal(fila$n_valores_excluidos_resumen[[1L]], 10L)
  expect_equal(as.character(fila$estado_resumen_cuantitativo[[1L]]),
               "calculados_sobre_valores")
})

test_that("la cifra del motivo se cuenta, no se deriva restando", {
  # Esta cifra se rompio TRES veces por ser una resta:
  #
  #   1. `aplicables - faltantes - excluidos`: el `NaN` esta en dos de los tres
  #      sumandos y se restaba dos veces (28 finitos publicaban 27).
  #   2. descontar `n_nan` una vez: supone que todo `NaN` esta en `n_faltantes`, y
  #      un `"NaN"` de TEXTO no lo esta -`is.na("NaN")` es FALSE-, asi que la
  #      cifra salia sobrestimada (4 finitos publicaban 5).
  #   3. ahora se CUENTA donde se calcula el resumen.
  #
  # El control de cada caso es la media, que sale por otro camino: si el resumen
  # uso N valores, la media es la de esos N.
  casos <- list(
    texto_nan = list(
      datos = data.frame(v = c("10", "20", "NaN", "30", "40"),
                         stringsAsFactors = FALSE),
      usados = 4, media = 25
    ),
    nan_nativo = list(
      datos = data.frame(v = c(10, 20, NaN, 30, 40)), usados = 4, media = 25
    ),
    nan_e_infinito = list(
      datos = data.frame(v = c(rep(1, 28), NaN, Inf)), usados = 28, media = 1
    ),
    dos_nan = list(
      datos = data.frame(v = c(rep(1, 28), NaN, NaN)), usados = 28, media = 1
    ),
    infinito_solo = list(
      datos = data.frame(v = c(10, 20, 30, 40, Inf)), usados = 4, media = 25
    )
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    perfil <- perfilar(caso$datos, analizar_dependencias = FALSE)
    fila <- perfil$columnas[perfil$columnas$columna == "v", , drop = FALSE]
    cobertura <- perfil$cobertura_diagnosticos
    motivo <- cobertura$motivo[
      as.character(cobertura$diagnostico) == "resumen_cuantitativo"
    ]
    expect_length(motivo, 1L)
    expect_match(
      motivo[[1L]],
      paste0("se calculo sobre ", caso$usados, " valores"),
      fixed = TRUE, info = nombre
    )
    # El otro camino: la media es la de esos valores y de ningun otro numero.
    expect_equal(fila$media[[1L]], caso$media, info = nombre)
  }
})

test_that("el desglose no afirma como cuenta la columna lo que dejo afuera", {
  # Decia "`NaN`, que la columna cuenta como ausente", y eso es falso cuando el
  # `NaN` llega como texto: la fila publica `n_faltantes = 0` y `n_distintos = 5`.
  # Quien cuenta la presencia es `n_faltantes`, que esta en la misma fila.
  de_texto <- perfilar(
    data.frame(v = c("10", "20", "NaN", "30", "40"), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )
  fila <- de_texto$columnas[de_texto$columnas$columna == "v", , drop = FALSE]
  expect_equal(fila$n_faltantes[[1L]], 0)
  expect_equal(fila$n_nan[[1L]], 1L)
  motivo <- de_texto$cobertura_diagnosticos$motivo[
    as.character(de_texto$cobertura_diagnosticos$diagnostico) ==
      "resumen_cuantitativo"
  ]
  expect_false(grepl("cuenta como ausente", motivo[[1L]], fixed = TRUE))
  # Lo que si dice, porque es cierto de los dos casos.
  expect_match(motivo[[1L]], "1 `NaN`", fixed = TRUE)
  expect_match(motivo[[1L]], "valor no finito", fixed = TRUE)

  # Y el infinito si declara presencia, porque un `Inf` siempre esta presente.
  con_infinito <- perfilar(
    data.frame(v = c(10, 20, 30, 40, Inf)), analizar_dependencias = FALSE
  )
  motivo_inf <- con_infinito$cobertura_diagnosticos$motivo[
    as.character(con_infinito$cobertura_diagnosticos$diagnostico) ==
      "resumen_cuantitativo"
  ]
  expect_match(motivo_inf[[1L]], "presente y no utilizable", fixed = TRUE)
})
