datos_plan <- function() {
  datos <- data.frame(
    " fecha mala " = c("2020-01-31", "2020-12-31", "2020-01-31"),
    categoria = c(" A", "S/D", " A"),
    numero = c("1", "2", "3"),
    extremo = c(1, 100, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rbind(datos, datos[1, , drop = FALSE])
}

test_that("el plan es un objeto de datos con políticas explícitas", {
  datos <- datos_plan()
  plan <- planificar_limpieza(perfilar(datos), datos)

  expect_s3_class(plan, "plan_limpieza")
  expect_true(all(c(
    "id_accion", "columna", "hallazgo", "estrategia", "recomendada",
    "justificacion", "n_afectadas", "reversible", "estado", "aplicar",
    "parametros", "orden", "grupo", "decision_grupo",
    "recomendacion_grupo", "destructiva", "evidencia", "severidad_origen"
  ) %in% names(plan)))
  expect_equal(anyDuplicated(plan$id_accion), 0L)
  expect_true(is.list(plan$parametros))
  expect_true(all(nzchar(plan$justificacion)))

  activas <- plan$estrategia[plan$aplicar]
  expect_true(all(c(
    "convertir_ausencias_textuales", "recortar_espacios",
    "convertir_tipo",
    "marcar_filas_duplicadas", "normalizar_nombres"
  ) %in% activas))
  expect_true(all(!plan$aplicar[plan$estrategia == "marcar_outliers"]))
  expect_true(all(plan$recomendada[plan$estrategia == "marcar_outliers"]))
  expect_true(any(as.character(plan$estado) == "informativa"))
  expect_true(all(!plan$aplicar[as.character(plan$estado) == "informativa"]))
  expect_s3_class(plan[1, , drop = FALSE], "plan_limpieza")
  expect_false(inherits(plan[, c("estrategia", "aplicar")], "plan_limpieza"))

  salida <- suppressMessages(capture.output(print(plan)))
  expect_match(paste(salida, collapse = "\n"), "estrategia")
})

test_that("aplicar trabaja sobre una copia y deja una bitácora verificable", {
  datos <- datos_plan()
  original <- datos
  plan <- planificar_limpieza(perfilar(datos), datos)
  resultado <- aplicar(plan, datos)

  expect_s3_class(resultado, "resultado_limpieza")
  expect_identical(datos, original)
  expect_s3_class(resultado$datos[["X.fecha.mala."]], "Date")
  expect_equal(resultado$datos$categoria, c("A", NA, "A", "A"))
  expect_type(resultado$datos$numero, "integer")
  expect_equal(resultado$datos$.fila_duplicada, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(resultado$datos$.grupo_duplicado, c(1L, NA, NA, 1L))
  expect_equal(nrow(resultado$registro), sum(plan$aplicar))
  expect_identical(
    attr(resultado$datos, "registro_limpieza"), resultado$registro
  )
  expect_true(all(resultado$registro$n_cambiadas >= 0))
  expect_true(inherits(resultado$registro$fecha_hora, "POSIXct"))
  expect_true(is.list(resultado$registro$parametros))
  expect_true(all(c(
    "n_no_reversibles", "justificacion", "estado", "error"
  ) %in% names(resultado$registro)))
  expect_true(all(resultado$registro$estado == "ejecutada"))
  expect_true(all(is.na(resultado$registro$error)))

  salida <- suppressMessages(capture.output(print(resultado)))
  expect_length(salida, 0L)
})

test_that("los sentinelas numéricos requieren una decisión del usuario", {
  datos <- data.frame(codigo = c(999, 66, 77, 88, 12, 34))
  plan <- planificar_limpieza(perfilar(datos), datos)
  indice <- which(plan$estrategia == "convertir_sentinelas_numericos")

  expect_length(indice, 1L)
  expect_false(plan$recomendada[[indice]])
  expect_false(plan$aplicar[[indice]])
  expect_equal(plan$n_afectadas[[indice]], 1)

  plan$aplicar[indice] <- TRUE
  resultado <- aplicar(plan, datos)
  expect_true(is.na(resultado$datos$codigo[[1L]]))
  expect_equal(resultado$datos$codigo[-1L], c(66, 77, 88, 12, 34))
  expect_equal(resultado$registro$n_cambiadas, 1)
})

test_that("una fecha ambigua queda bloqueada y no ofrece interpretación", {
  datos <- data.frame(
    fecha = c("01/02/2020", "03/04/2020", "05/06/2020"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos), datos)
  accion <- plan[plan$estrategia == "desambiguar_fecha_en_origen", ]

  expect_equal(as.character(accion$estado), "bloqueada")
  expect_false(accion$recomendada)
  expect_false(accion$aplicar)
  plan$aplicar[plan$estrategia == "desambiguar_fecha_en_origen"] <- TRUE
  expect_error(aplicar(plan, datos), "estado 'lista'")
})

test_that("los extremos sólo se marcan cuando se activa la propuesta", {
  datos <- data.frame(valor = c(rep(1, 20), 100))
  plan <- planificar_limpieza(perfilar(datos))
  indice <- which(plan$estrategia == "marcar_outliers")

  expect_false(plan$aplicar[[indice]])
  sin_marca <- aplicar(plan, datos)$datos
  expect_false(any(grepl("outlier", names(sin_marca))))

  plan$aplicar[indice] <- TRUE
  con_marca <- aplicar(plan, datos)$datos
  expect_equal(sum(con_marca$.outlier_valor), 1L)
  expect_true(tail(con_marca$.outlier_valor, 1L))
})

test_that("las conversiones exactas cubren dobles, lógicos y fechas", {
  datos <- data.frame(
    doble = c("1.5", "2", "3.25"),
    logico = c("TRUE", "FALSE", "TRUE"),
    fecha = c("2020-01-31", "2020-02-29", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos), datos)
  resultado <- aplicar(plan, datos)$datos

  expect_type(resultado$doble, "double")
  expect_equal(resultado$doble, c(1.5, 2, 3.25))
  expect_type(resultado$logico, "logical")
  expect_equal(resultado$logico, c(TRUE, FALSE, TRUE))
  expect_s3_class(resultado$fecha, "Date")
})

test_that("la regla de conversión separa formato de identidad", {
  casos <- list(
    entero_limpio = list(valor = "12345", recomendar = TRUE),
    decimal_limpio = list(valor = "123.45", recomendar = TRUE),
    decimal_con_ceros_de_cola = list(valor = "6.50", recomendar = TRUE),
    negativos = list(valor = "-4711", recomendar = TRUE),
    fecha_iso = list(valor = "2020-03-15", recomendar = TRUE),
    fecha_dia_mes = list(valor = "15/03/2020", recomendar = TRUE),
    logico = list(valor = "TRUE", recomendar = TRUE),
    documento_con_ceros = list(
      valor = c(sprintf("%08d", 10000001:10000089),
                sprintf("0%07d", 1:11)), recomendar = FALSE
    ),
    colision_numerica = list(valor = c("0123", "123"), recomendar = FALSE),
    telefono_fuera_de_rango = list(valor = "+593993765352", recomendar = FALSE),
    miles = list(valor = "1,234,567", recomendar = FALSE),
    codigo_alfanumerico = list(valor = "AB000123", recomendar = FALSE)
  )
  for (nombre in names(casos)) {
    datos <- data.frame(x = casos[[nombre]]$valor, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, analizar_dependencias = FALSE)
    plan <- planificar_limpieza(perfil, datos)
    acciones <- plan[
      plan$estrategia %in% c("convertir_tipo", "convertir_numero_regional"),
      , drop = FALSE
    ]
    recomendar <- nrow(acciones) > 0L && any(acciones$recomendada)
    expect_identical(recomendar, casos[[nombre]]$recomendar, info = nombre)
    if (!casos[[nombre]]$recomendar && nrow(acciones)) {
      expect_false(any(acciones$aplicar), info = nombre)
    }
  }
  fecha <- data.frame(x = "15/03/2020", stringsAsFactors = FALSE)
  plan_fecha <- planificar_limpieza(
    perfilar(fecha, analizar_dependencias = FALSE), fecha
  )
  expect_true(plan_fecha$recomendada[
    plan_fecha$estrategia == "convertir_tipo"
  ])
  resultado_fecha <- aplicar(plan_fecha, fecha)
  expect_s3_class(resultado_fecha$datos$x, "Date")
  expect_equal(as.character(resultado_fecha$datos$x), "2020-03-15")
  expect_equal(resultado_fecha$registro$n_no_reversibles, 0)
})

test_that("convertir_tipo sólo se recomienda si conserva la representación", {
  documentos <- data.frame(
    documento = c(
      sprintf("%08d", 10000001:10000089),
      sprintf("0%07d", 1:11)
    ),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(documentos, analizar_dependencias = FALSE), documentos
  )
  accion <- plan[plan$estrategia == "convertir_tipo", , drop = FALSE]

  expect_equal(nrow(accion), 1L)
  expect_false(accion$recomendada[[1L]])
  expect_false(accion$aplicar[[1L]])
  expect_false(accion$reversible[[1L]])
  expect_true(accion$destructiva[[1L]])
  expect_equal(accion$parametros[[1L]]$n_no_reversibles, 11L)
  expect_match(accion$justificacion[[1L]], "11 valores")

  accion_activa <- which(plan$estrategia == "convertir_tipo")
  plan$aplicar[accion_activa] <- TRUE
  plan$estado[accion_activa] <- "lista"
  resultado <- aplicar(plan, documentos)
  registro <- resultado$registro[
    resultado$registro$estrategia == "convertir_tipo", , drop = FALSE
  ]
  expect_equal(registro$n_no_reversibles, 11)
  expect_true(registro$destructiva)
  expect_equal(as.character(resultado$datos$documento[[90L]]), "1")
})

test_that("sin datos completos la reversibilidad queda bloqueada", {
  datos <- data.frame(codigo = c("1", "2", "3"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE)
  )
  accion <- plan[plan$estrategia == "convertir_tipo", , drop = FALSE]
  expect_equal(nrow(accion), 1L)
  expect_false(accion$recomendada[[1L]])
  expect_false(accion$aplicar[[1L]])
  expect_equal(as.character(accion$estado[[1L]]), "bloqueada")
  expect_match(accion$justificacion[[1L]], "pase `datos`")
})

test_that("una conversión imposible se registra sin abortar el plan", {
  datos <- data.frame(
    telefono = c("+593993700001", "+593993700002"),
    zona = c(" A", "B"), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  indice <- which(plan$estrategia == "convertir_tipo")
  expect_equal(length(indice), 1L)
  expect_false(plan$recomendada[[indice]])
  expect_false(plan$aplicar[[indice]])
  expect_true(plan$destructiva[[indice]])

  plan$aplicar[indice] <- TRUE
  plan$estado[indice] <- "lista"
  resultado <- aplicar(plan, datos)
  fallo <- resultado$registro[
    resultado$registro$estrategia == "convertir_tipo", , drop = FALSE
  ]
  expect_equal(fallo$estado[[1L]], "fallida")
  expect_match(fallo$error[[1L]], "representarse como enteros")
  expect_identical(resultado$datos$telefono, datos$telefono)
  expect_identical(resultado$datos$zona, c("A", "B"))
  expect_true(any(resultado$registro$estrategia == "recortar_espacios"))
})

test_that("las conversiones recomendadas no pierden identidad", {
  datos <- data.frame(
    entero = c("1", "2", "3"),
    doble = c("1.5", "2", "3.25"),
    fecha = c("2020-01-01", "2020-02-02", "2020-03-03"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  conversiones <- plan[grepl("^convertir_", plan$estrategia), , drop = FALSE]
  expect_true(all(!conversiones$recomendada | conversiones$reversible))
  resultado <- aplicar(plan, datos)
  activas <- resultado$registro$estrategia[
    resultado$registro$estado == "ejecutada" &
      resultado$registro$estrategia %in% conversiones$estrategia
  ]
  expect_true(length(activas) > 0L)
  expect_equal(sum(resultado$registro$n_no_reversibles), 0)
})

test_that("recortar espacios devuelve texto para una columna factor", {
  datos <- data.frame(x = factor(c(" A", "B ", " A")))
  plan <- planificar_limpieza(perfilar(datos))
  resultado <- aplicar(plan, datos)$datos

  expect_type(resultado$x, "character")
  expect_identical(resultado$x, c("A", "B", "A"))
})

test_that("la aplicación rechaza deriva de esquema y planes inválidos", {
  datos <- data.frame(" x" = c(" 1", "2"), check.names = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)

  expect_error(aplicar(plan[, -1L], datos), "contrato")
  expect_error(aplicar(plan, 1:2), "data.frame")

  duplicado <- plan
  duplicado$id_accion[] <- "misma"
  expect_error(aplicar(duplicado, datos), "únicos")

  con_na <- plan
  con_na$aplicar[[1L]] <- NA
  expect_error(aplicar(con_na, datos), "sin NA")

  sin_parametros <- plan
  sin_parametros$parametros <- "x"
  expect_error(aplicar(sin_parametros, datos), "columna de listas")

  renombrados <- datos
  names(renombrados) <- "y"
  expect_error(aplicar(plan, renombrados), "columna llamada|no coinciden")
})

test_that("las precondiciones impiden conversiones parciales y marcas pisadas", {
  datos <- data.frame(x = c("1", "2"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)
  datos$x[[2L]] <- "no convertible"
  fallo_conversion <- aplicar(plan, datos)
  expect_equal(fallo_conversion$registro$estado[
    fallo_conversion$registro$estrategia == "convertir_tipo"
  ], "fallida")
  expect_match(fallo_conversion$registro$error[
    fallo_conversion$registro$estrategia == "convertir_tipo"
  ], "no pueden convertirse")

  extremos <- data.frame(valor = c(rep(1, 20), 100))
  plan_extremos <- planificar_limpieza(perfilar(extremos))
  plan_extremos$aplicar[plan_extremos$estrategia == "marcar_outliers"] <- TRUE
  extremos$.outlier_valor <- FALSE
  fallo_marca <- aplicar(plan_extremos, extremos)
  expect_true(any(grepl("marca ya existe", fallo_marca$registro$error)))

  duplicados <- data.frame(x = c(1, 1), .fila_duplicada = FALSE)
  plan_duplicados <- planificar_limpieza(perfilar(duplicados))
  fallo_duplicados <- aplicar(plan_duplicados, duplicados)
  expect_true(any(grepl("marca ya existe", fallo_duplicados$registro$error)))
})

test_that("un plan vacío devuelve una copia y un registro vacío", {
  datos <- data.frame()
  plan <- planificar_limpieza(perfilar(datos))
  resultado <- aplicar(plan, datos)

  expect_equal(nrow(plan), 0L)
  expect_equal(nrow(resultado$registro), 0L)
  expect_equal(nrow(resultado$plan_aplicado), 0L)
  expect_identical(resultado$datos, structure(
    datos, registro_limpieza = resultado$registro
  ))
  expect_error(planificar_limpieza(datos), "clase perfil")
})

test_that("data.table se copia antes de aplicar el plan", {
  skip_if_not_installed("data.table")
  datos <- data.table::data.table(x = c(" A", "B"))
  original <- data.table::copy(datos)
  resultado <- aplicar(planificar_limpieza(perfilar(datos)), datos)

  expect_identical(datos, original)
  expect_s3_class(resultado$datos, "data.table")
  expect_equal(resultado$datos$x, c("A", "B"))
})

test_that("formatos candidatos e inferencias parciales no se convierten", {
  fechas <- data.frame(
    fecha = c("2020-01-01", "01/02/2020", "03/04/2020"),
    stringsAsFactors = FALSE
  )
  plan_fechas <- planificar_limpieza(perfilar(fechas))
  mixta <- plan_fechas[plan_fechas$estrategia == "convertir_fecha_confirmada", ]
  expect_equal(as.character(mixta$estado), "bloqueada")
  expect_false(mixta$recomendada)

  parcial <- data.frame(x = c("1", "2", "3", "4", "x"))
  plan_parcial <- planificar_limpieza(perfilar(parcial))
  conversion <- plan_parcial[plan_parcial$estrategia == "convertir_tipo", ]
  expect_equal(as.character(conversion$estado), "bloqueada")
  expect_false(conversion$recomendada)
  expect_false(conversion$aplicar)
})

test_that("los nombres duplicados se resuelven antes de ligar acciones", {
  datos <- data.frame(a = c(" A", "B"), b = c(" A", "B"), check.names = FALSE)
  names(datos) <- c(" x", " x")
  plan <- planificar_limpieza(perfilar(datos))

  expect_true("normalizar_nombres" %in% plan$estrategia)
  expect_false("recortar_espacios" %in% plan$estrategia)
  resultado <- aplicar(plan, datos)$datos
  expect_equal(names(resultado), c("X.x", "X.x.1"))
})

test_that("las transformaciones internas validan su contrato", {
  expect_error(
    lupa:::.reemplazar_ausencias_textuales(1:2, list(valores = "x")),
    "columna de texto"
  )
  texto_numerico <- lupa:::.reemplazar_sentinelas_numericos(
    c("999", "1"), list(valores = 999)
  )
  expect_equal(texto_numerico$n, 1L)
  expect_error(
    lupa:::.reemplazar_sentinelas_numericos(
      as.Date("2020-01-01"), list(valores = 999)
    ),
    "texto o números"
  )
  expect_error(lupa:::.recortar_texto(1:2), "columna de texto")
  expect_error(lupa:::.convertir_logico(c("sí", "quizás")), "lógico")
  expect_error(
    lupa:::.convertir_fecha("2020-01-01", list(formatos = character())),
    "formatos confirmados"
  )
  expect_error(
    lupa:::.convertir_fecha(
      c("2020-01-01", "mal"),
      list(formatos = "%Y-%m-%d", tipo = "fecha")
    ),
    "no responden"
  )
  expect_error(
    lupa:::.convertir_fecha(
      "2020-01-01", list(formatos = "%Q", tipo = "fecha")
    ),
    "formato no reconocido"
  )
  fecha_hora <- lupa:::.convertir_fecha(
    "2020-01-01 10:30:00",
    list(formatos = "%Y-%m-%d %H:%M:%S", tipo = "fecha-hora")
  )
  expect_s3_class(fecha_hora, "POSIXct")
  expect_error(
    lupa:::.convertir_tipo("1.5", list(tipo = "entero")),
    "representarse como enteros"
  )
  expect_error(
    lupa:::.convertir_tipo("999999999999", list(tipo = "entero")),
    "representarse como enteros"
  )
  expect_error(
    lupa:::.convertir_tipo("1", list(tipo = "texto")),
    "No hay una conversión"
  )
  expect_false(any(lupa:::.marca_outliers(c(NA_real_, NA_real_))))
})

test_that("se rechazan una estrategia desconocida y la deriva de nombres", {
  datos <- data.frame(" x" = c(" A", "B"), check.names = FALSE)
  plan <- planificar_limpieza(perfilar(datos))

  desconocido <- plan[plan$estrategia == "recortar_espacios", , drop = FALSE]
  desconocido$estrategia <- "sin_implementacion"
  fallo_desconocido <- aplicar(desconocido, datos)
  expect_match(fallo_desconocido$registro$error, "no implementada")

  nombres <- plan[plan$estrategia == "normalizar_nombres", , drop = FALSE]
  datos_renombrados <- datos
  names(datos_renombrados) <- "y"
  expect_error(aplicar(nombres, datos_renombrados), "no coinciden")
})
