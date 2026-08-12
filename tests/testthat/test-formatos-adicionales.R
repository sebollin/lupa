test_that("las fechas admiten campos sin relleno y señalan años cortos", {
  sin_relleno <- detectar_formatos_fecha(c("3/1/2023", "30/11/2023"))
  corto <- detectar_formatos_fecha(c("30/11/23 08:28", "31/12/24 09:00"))

  expect_equal(sin_relleno$formato, "%d/%m/%Y")
  expect_true(all(corto$anio_dos_digitos))
  expect_true(all(corto$estado == "candidato"))
  perfil <- perfilar(
    data.frame(fecha = c("30/11/23", "31/12/24")),
    analizar_dependencias = FALSE
  )
  expect_true("anio_de_dos_digitos" %in% perfil$hallazgos$tipo_hallazgo)
  expect_true(is.na(perfil$columnas$minimo_fecha))
  expect_error(detectar_formatos_fecha(data.frame(x = "2020-01-01")),
               "vector")
  expect_equal(detectar_formatos_fecha(as.Date(c("2020-01-01", NA)))$n, 1L)
  expect_equal(
    detectar_formatos_fecha(as.POSIXct("2020-01-01 10:00:00", tz = "UTC"))$formato,
    "%Y-%m-%d %H:%M:%S"
  )
  expect_equal(nrow(detectar_formatos_fecha(c("texto", "otro"))), 0L)
  expect_equal(detectar_formatos_fecha(c("12/31/2020", "11/30/2020"))$formato,
               "%m/%d/%Y")
})

test_that("coincidencias aisladas no convierten códigos en fechas mixtas", {
  datos <- data.frame(
    lote = c(rep("ABC-123", 20), "30/03/25", "2024-01-01"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_equal(perfil$columnas$tipo_inferido, "texto")
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "anio_de_dos_digitos"))
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos"))
})

test_that("reconoce meses escritos en espanol e ingles sin locale", {
  valores <- c(
    "15 de marzo de 2024", "15-Mar-24", "15 mar 2024",
    "1 de enero de 2020", "3-Ago-2023", "MARZO 2024", "Set 2024",
    "15 de setiembre de 2024", "12 de Diciembre de 1999",
    "31/01/2020", "2020-01-01"
  )
  resultado <- detectar_formatos_fecha(valores)
  expect_equal(attr(resultado, "compatibles"), 11L)
  expect_true(all(c(
    "%d de %B de %Y", "%d-%b-%y", "%d %b %Y", "%d-%b-%Y",
    "%B %Y", "%b %Y", "%d/%m/%Y", "%Y-%m-%d"
  ) %in% resultado$formato))
  expect_true(any(resultado$anio_dos_digitos & resultado$formato == "%d-%b-%y"))
  expect_equal(inferir_tipo(valores)$tipo, "fecha")
})

test_that("los meses escritos no dependen de LC_TIME", {
  valores <- c("15 de marzo de 2024", "3-Ago-2023", "15-Mar-24")
  base <- detectar_formatos_fecha(valores)
  locales <- c("C", "es_UY.UTF-8", "en_US.UTF-8")
  resultados <- lapply(locales, function(locale) {
    anterior <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", anterior), add = TRUE)
    try(Sys.setlocale("LC_TIME", locale), silent = TRUE)
    detectar_formatos_fecha(valores)
  })
  for (resultado in resultados) {
    expect_equal(resultado[, c("formato", "n", "estado", "anio_dos_digitos")],
                 base[, c("formato", "n", "estado", "anio_dos_digitos")])
    expect_equal(attr(resultado, "compatibles"), attr(base, "compatibles"))
  }
})

test_that("nombres de mes dentro de texto libre no son fechas", {
  valores <- c(
    "La reunión será en marzo de 2024",
    "Marzo es un mes del año",
    "texto libre con agosto y diciembre"
  )
  resultado <- detectar_formatos_fecha(valores)
  expect_equal(nrow(resultado), 0L)
  expect_equal(attr(resultado, "compatibles"), 0L)
})

test_that("se detectan fechas repartidas entre tres columnas", {
  datos <- data.frame(
    fecha_anio = c(2020, 2021, 2022),
    fecha_mes = c(1, 2, 3), fecha_dia = c(3, 28, 15)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "fecha_partida_columnas", ]

  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "fecha_dia.*fecha_mes.*fecha_anio")
})

test_that("los números regionales seguros se convierten y los ambiguos no", {
  seguro <- data.frame(
    importe = c("1.234,56", "2.000,00"),
    porcentaje = c("45%", "20%"),
    peso = c("5 kg", "10 kg"), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(seguro, analizar_dependencias = FALSE), seguro
  )
  acciones <- plan[plan$estrategia == "convertir_numero_regional", ]
  expect_true(all(acciones$recomendada & acciones$aplicar))
  limpio <- aplicar(plan, seguro)$datos
  expect_equal(limpio$importe, c(1234.56, 2000))
  expect_equal(limpio$porcentaje, c(0.45, 0.2))
  expect_equal(limpio$peso, c(5, 10))

  ambiguo <- data.frame(valor = c("1.234", "2.345"))
  perfil <- perfilar(ambiguo, analizar_dependencias = FALSE)
  expect_equal(perfil$columnas$tipo_inferido, "texto")
  plan_ambiguo <- planificar_limpieza(perfil, ambiguo)
  accion <- plan_ambiguo[plan_ambiguo$estrategia == "convertir_numero_regional", ]
  expect_false(accion$recomendada)
  expect_false(accion$aplicar)
  accion$aplicar <- TRUE
  fallo <- aplicar(accion, ambiguo)
  expect_match(fallo$registro$error[[1L]], "punto_sin_coma")
  accion$parametros[[1L]]$punto_sin_coma <- "miles"
  expect_equal(aplicar(accion, ambiguo)$datos$valor, c(1234, 2345))
  accion$parametros[[1L]]$punto_sin_coma <- "decimal"
  expect_equal(aplicar(accion, ambiguo)$datos$valor, c(1.234, 2.345))
  expect_error(
    lupa:::.convertir_numero_regional(1:2, list(punto_sin_coma = "miles")),
    "columna de texto"
  )
  expect_error(
    lupa:::.convertir_numero_regional(c("1,2", "mal"),
                                      list(punto_sin_coma = "miles")),
    "no responden"
  )
})

test_that("los números reconocen convención decimal y moneda de otros países", {
  datos <- data.frame(
    usd = c("USD 1,234.56", "USD 2,000.00"),
    clp = c("CLP 150.000", "CLP 250.000"),
    eur = c("EUR 1.234,56", "EUR 2.000,00"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  filas <- perfil$columnas

  expect_equal(filas$n_numeros_texto, c(2L, 2L, 2L))
  expect_equal(
    filas$numero_texto_convencion,
    c("decimal_punto", "ambigua", "decimal_coma")
  )
  expect_equal(filas$numero_texto_moneda, c("USD", "CLP", "EUR"))
  expect_true(filas$numero_texto_seguro[filas$columna == "usd"])
  plan <- planificar_limpieza(perfil, datos)
  usd <- plan[
    plan$columna == "usd" & plan$estrategia == "convertir_numero_regional", ]
  eur <- plan[
    plan$columna == "eur" & plan$estrategia == "convertir_numero_regional", ]
  expect_true(usd$recomendada[[1L]])
  expect_true(eur$recomendada[[1L]])
  limpio <- aplicar(rbind(usd, eur), datos)$datos
  expect_equal(limpio$usd, c(1234.56, 2000))
  expect_equal(limpio$eur, c(1234.56, 2000))

  tres_decimales <- perfilar(
    data.frame(coma = c("1234,567", "2345,678"),
               punto = c("1234.567", "2345.678")),
    analizar_dependencias = FALSE
  )
  expect_equal(
    tres_decimales$columnas$numero_texto_convencion,
    c("decimal_coma", "decimal_punto")
  )
  identificador <- perfilar(
    data.frame(codigo = c("ABC1234", "ABC5678")),
    analizar_dependencias = FALSE
  )
  expect_equal(identificador$columnas$n_numeros_texto, 0L)
  expect_false(any(identificador$hallazgos$tipo_hallazgo == "numero_como_texto"))

  mixtos <- perfilar(
    data.frame(x = c("1.234,56", "1,234.56")),
    analizar_dependencias = FALSE
  )
  expect_equal(mixtos$columnas$numero_texto_convencion, "mixta")
  expect_false(mixtos$columnas$numero_texto_seguro)
})

test_that("la conversión regional exige resolver también la coma ambigua", {
  ambiguo <- data.frame(valor = c("1,234", "2,345"))
  plan <- planificar_limpieza(
    perfilar(ambiguo, analizar_dependencias = FALSE), ambiguo
  )
  accion <- plan[plan$estrategia == "convertir_numero_regional", ]
  accion$aplicar <- TRUE

  fallo <- aplicar(accion, ambiguo)
  expect_match(fallo$registro$error[[1L]], "coma_sin_punto")
  accion$parametros[[1L]]$coma_sin_punto <- "miles"
  expect_equal(aplicar(accion, ambiguo)$datos$valor, c(1234, 2345))
  accion$parametros[[1L]]$coma_sin_punto <- "decimal"
  expect_equal(aplicar(accion, ambiguo)$datos$valor, c(1.234, 2.345))

  expect_equal(
    lupa:::.convertir_numero_regional(
      "1.234", list(punto_sin_coma = "miles")
    )$valor,
    1234
  )
  expect_equal(
    lupa:::.convertir_numero_regional(
      "1.234,56", list(convencion = "es-UY")
    )$valor,
    1234.56
  )
  expect_error(
    lupa:::.convertir_numero_regional("1", list(convencion = "mixta")),
    "no está confirmada"
  )
  expect_error(
    lupa:::.convertir_numero_regional(
      "1,2", list(convencion = "sin_separadores")
    ),
    "No fue posible"
  )
})

test_that("el mojibake reparable cambia y el texto sano permanece intacto", {
  datos <- data.frame(
    lugar = c("PaysandÃº", "GONZÃLEZ", "Paysandú", "Ñandú"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true("codificacion_rota" %in% perfil$hallazgos$tipo_hallazgo)
  plan <- planificar_limpieza(perfil)
  accion <- plan[plan$estrategia == "reparar_codificacion", ]
  expect_true(accion$recomendada && accion$aplicar)
  limpio <- aplicar(plan, datos)$datos$lugar
  expect_equal(limpio, c("Paysandú", "GONZÁLEZ", "Paysandú", "Ñandú"))

  perdido <- perfilar(data.frame(x = "texto �"), analizar_dependencias = FALSE)
  plan_perdido <- planificar_limpieza(perdido)
  expect_true(any(as.character(plan_perdido$estado) == "informativa"))
  expect_false(any(plan_perdido$aplicar))
  expect_error(lupa:::.reparar_codificacion(1:2, list()), "columna de texto")
  factor_roto <- factor(c("PaysandÃº", "Paysandú"))
  reparado <- lupa:::.reparar_codificacion(factor_roto, list())
  expect_type(reparado$valor, "character")
  expect_identical(reparado$valor, c("Paysandú", "Paysandú"))
})
