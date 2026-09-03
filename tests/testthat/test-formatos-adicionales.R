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
  anterior <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", anterior), add = TRUE)
  expect_equal(Sys.setlocale("LC_TIME", "C"), "C")
  base <- detectar_formatos_fecha(valores)

  # El `try()` que habia aca se tragaba el fallo de `Sys.setlocale`, y entonces
  # en una maquina sin esos locales -una imagen minima de contenedor no los
  # genera- el locale no cambiaba y el test comparaba el resultado contra si
  # mismo: pasaba sin haber probado su propia afirmacion. Ademas dejaba el aviso
  # "OS reports request to set locale cannot be honored" en el resumen de la
  # suite, que es la unica pista que quedaba de que no habia medido nada.
  #
  # Ahora se comprueba que el locale quedo puesto, y si ninguno de los dos se
  # puede poner, el test se saltea diciendo por que en vez de dar por bueno un
  # PASS vacio.
  fijar_locale <- function(locale) {
    puesto <- suppressWarnings(
      tryCatch(Sys.setlocale("LC_TIME", locale), error = function(e) "")
    )
    identical(puesto, locale)
  }
  locales <- c("es_UY.UTF-8", "en_US.UTF-8")
  efectivos <- Filter(fijar_locale, locales)
  skip_if(
    !length(efectivos),
    paste(
      "Ninguno de los locales", paste(locales, collapse = " ni "),
      "esta generado en esta maquina: sin cambiar LC_TIME el test no puede",
      "probar que el resultado no depende de LC_TIME."
    )
  )

  for (locale in efectivos) {
    expect_true(fijar_locale(locale))
    resultado <- detectar_formatos_fecha(valores)
    expect_equal(resultado[, c("formato", "n", "estado", "anio_dos_digitos")],
                 base[, c("formato", "n", "estado", "anio_dos_digitos")],
                 info = locale)
    expect_equal(attr(resultado, "compatibles"), attr(base, "compatibles"),
                 info = locale)
  }
})

test_that("las expresiones de meses sólo recorren candidatos", {
  longitudes <- integer()
  original <- lupa:::.detectar_meses_regexec
  local_mocked_bindings(
    .detectar_meses_regexec = function(expresion, texto) {
      longitudes <<- c(longitudes, length(texto))
      original(expresion, texto)
    },
    .package = "lupa"
  )
  valores <- c(rep("texto libre con marzo adentro", 9999L),
               "15 de marzo de 2024")
  detectar_formatos_fecha(valores)
  expect_gt(length(longitudes), 0L)
  expect_true(all(longitudes == 1L))
})

test_that("el parseo de fechas reutiliza los meses ya detectados", {
  llamadas <- 0L
  original <- lupa:::.detectar_meses_texto
  local_mocked_bindings(
    .detectar_meses_texto = function(valores) {
      llamadas <<- llamadas + 1L
      original(valores)
    },
    .package = "lupa"
  )
  perfilar(
    data.frame(fecha = c(
      "15 de marzo de 2024", NA_character_, "", "16 de marzo de 2024"
    )),
    analizar_dependencias = FALSE
  )
  expect_equal(llamadas, 1L)
})

test_that("los períodos mensuales declaran su granularidad y no inventan días", {
  valores <- c("enero 2023", "febrero 2023", "diciembre 2024")
  formatos <- detectar_formatos_fecha(valores)
  expect_equal(formatos$granularidad, "mes")
  perfil <- perfilar(data.frame(periodo = valores), analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "periodo", , drop = FALSE]
  expect_equal(fila$estado_resumen_cuantitativo, "granularidad_incompleta")
  expect_true(is.na(fila$minimo_fecha) && is.na(fila$media_fecha))
  expect_equal(fila$n_fechas_resumidas, 0L)
  expect_equal(fila$n_fechas_excluidas_granularidad, 3L)
  expect_equal(perfil$formatos_fecha$periodo$granularidad, "mes")
})

test_that("los períodos minoritarios quedan fuera con alcance explícito", {
  valores <- c(sprintf("2023-01-%02d", seq_len(9L)), "enero 2022")
  perfil <- perfilar(
    data.frame(fecha = valores), analizar_dependencias = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "fecha", , drop = FALSE]
  expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_dias")
  expect_equal(fila$n_fechas_resumidas, 9L)
  expect_equal(fila$n_fechas_excluidas_granularidad, 1L)
  expect_equal(fila$minimo_fecha, "2023-01-01")
  expect_equal(fila$maximo_fecha, "2023-01-09")
  expect_match(
    paste(names(fila), collapse = " "), "n_fechas_excluidas_granularidad"
  )
})

test_that("los períodos numéricos de mes se detectan sin hora ni parseo", {
  especificaciones <- lupa:::.especificaciones_fecha()
  meses <- especificaciones[especificaciones$granularidad == "mes", ,
                             drop = FALSE]

  expect_setequal(
    meses$formato,
    c("%Y-%m", "%m/%Y", "%Y/%m")
  )
  expect_true(all(startsWith(meses$expresion, "^")))
  expect_true(all(endsWith(meses$expresion, "$")))
  expect_false(any(grepl("%H", meses$formato, fixed = TRUE)))
  expect_false("%Y%m" %in% especificaciones$formato)

  valores <- c("2023-05", "05/2023", "2023/05")
  formatos <- lapply(valores, detectar_formatos_fecha)
  for (i in seq_along(valores)) {
    expect_equal(formatos[[i]]$formato, meses$formato[i], info = valores[[i]])
    expect_equal(formatos[[i]]$granularidad, "mes", info = valores[[i]])
    expect_equal(formatos[[i]]$estado, "confirmado", info = valores[[i]])
    expect_equal(attr(formatos[[i]], "compatibles"), 1L, info = valores[[i]])
    expect_true(
      is.na(lupa:::.parsear_fechas(valores[[i]], formatos[[i]])[[1L]]),
      info = valores[[i]]
    )
  }

  expect_equal(inferir_tipo("202305")$tipo, "entero")
  expect_equal(
    detectar_formatos_fecha("2023-05 14:30")$formato,
    character()
  )
})

test_that("los períodos numéricos declaran el alcance del resumen", {
  total <- 1000L
  casos <- c(minoria = 100L, mitad = 500L, columna_entera = total)
  for (n_mes in casos) {
    valores <- c(
      rep("2024-01-15", total - n_mes),
      rep("2023-05", n_mes)
    )
    perfil <- perfilar(
      data.frame(fecha = valores), muestra = Inf,
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    )
    fila <- perfil$columnas[perfil$columnas$columna == "fecha", , drop = FALSE]
    if (n_mes < total) {
      expect_equal(fila$tipo_inferido, "fecha", info = names(n_mes))
      expect_equal(fila$n_fechas_resumidas, total - n_mes,
                   info = names(n_mes))
      expect_equal(fila$n_fechas_excluidas_granularidad, n_mes,
                   info = names(n_mes))
      expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_dias",
                   info = names(n_mes))
      expect_equal(fila$minimo_fecha, "2024-01-15", info = names(n_mes))
    } else {
      expect_equal(fila$tipo_inferido, "fecha", info = names(n_mes))
      expect_equal(fila$n_fechas_resumidas, 0L, info = names(n_mes))
      expect_equal(fila$n_fechas_excluidas_granularidad, total,
                   info = names(n_mes))
      expect_equal(fila$estado_resumen_cuantitativo,
                   "granularidad_incompleta", info = names(n_mes))
      expect_true(is.na(fila$minimo_fecha), info = names(n_mes))
    }
  }
})

test_that("los años de meses escritos quedan acotados al rango de fechas", {
  expect_equal(attr(detectar_formatos_fecha(c("Set 1000", "Mar 1000")),
                    "compatibles"), 0L)
  expect_equal(attr(detectar_formatos_fecha(c("Set 1799", "Mar 2101")),
                    "compatibles"), 0L)
  expect_equal(attr(detectar_formatos_fecha(c("Set 1800", "Mar 2100")),
                    "compatibles"), 2L)
})

test_that("separadores y comas reales forman formatos mixtos", {
  ingles <- detectar_formatos_fecha(c("Mar 3, 2024", "Mar 4 2024"))
  expect_true(attr(ingles, "formatos_mixtos"))
  expect_setequal(ingles$formato, c("%b %d, %Y", "%b %d %Y"))
  separados <- detectar_formatos_fecha(c("15-Mar-2024", "16 Mar 2024"))
  expect_true(attr(separados, "formatos_mixtos"))
  expect_setequal(separados$formato, c("%d-%b-%Y", "%d %b %Y"))
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
