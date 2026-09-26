# Tres cosas de la capa de frescura, medidas sobre el contrato de `vigencia()`.
#
# 1. La documentacion promete que "cada metrica valida los campos que necesita y
#    se abstiene si faltan". Las cuatro metricas ligadas a `vigencia()` llamaban
#    `stop()`: `medir()` abortaba entero y no devolvia nada -ni filas, ni ceros,
#    ni cobertura-, asi que una metrica sin su campo se llevaba puesta la
#    medicion de todas las demas.
# 2. `Date` y `POSIXct` no comparan en la misma escala: un `Date` se ancla a la
#    medianoche UTC. Medido, la misma corrida con los mismos datos daba `1 0 0`
#    con `TZ=UTC` y `0 0 0` con `TZ=America/Montevideo`, sin que nada lo dijera.
# 3. La semantica publicada de `OportunidadEntPorFecha` decia "antes de su fecha
#    limite" y el calculo es `<=`: la fila exactamente en el limite salia
#    oportuna. La gemela por atributo ya declaraba "inclusive", asi que la
#    conducta era la correcta y la descripcion la que mentia.

datos_o61 <- function(columna = as.Date(c("2026-01-10", "2026-01-31",
                                          "2026-02-01"))) {
  data.frame(actualizado = columna, v = seq_along(columna))
}

medir_frescura_o61 <- function(metrica, contrato, datos = datos_o61()) {
  instancia <- instanciar(especializar(metrica, vigencia = contrato), "t")
  otra <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "v")
  medir(modelo(list(instancia, otra)), list(t = datos), id_medicion = "o61")
}

test_that("un contrato incompleto se abstiene y no se lleva la corrida", {
  nucleo <- metricas_nucleo()
  casos <- list(
    list(metrica = nucleo$OportunidadEntPorFecha, campo = "fecha_limite"),
    list(metrica = nucleo$OportunidadEntPorIntervalo, campo = "intervalo"),
    list(metrica = nucleo$DesactualizacionPorFecha, campo = "fecha_ultimo_cambio"),
    list(metrica = nucleo$DesactualizacionPorCambios, campo = "frecuencia_cambio")
  )
  for (caso in casos) {
    medicion <- medir_frescura_o61(caso$metrica, vigencia("actualizado"))
    # Las medidas de la OTRA metrica del modelo sobreviven.
    expect_equal(nrow(medicion), 3L, info = caso$campo)
    expect_true(all(as.character(medicion$metrica) == "NoNulo"),
                info = caso$campo)
    cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
    expect_s3_class(cobertura, "data.frame")
    expect_equal(nrow(cobertura), 1L, info = caso$campo)
    expect_identical(as.character(cobertura$estado), "contrato_incompleto")
    expect_true(grepl("no se midi", cobertura$motivo[[1L]]))
    # La abstencion tiene que NOMBRAR el campo que falta: sin eso queda muda y no
    # se distingue de una metrica que no se pidio. Es lo que garantizaba el
    # `stop()` que esta ronda retiro.
    expect_true(grepl(caso$campo, cobertura$motivo[[1L]], fixed = TRUE),
                info = caso$campo)
    expect_true(nzchar(cobertura$como_resolverlo[[1L]]))
  }
})

test_that("la abstencion no se interpreta como exito al evaluar", {
  medicion <- medir_frescura_o61(
    metricas_nucleo()$OportunidadEntPorFecha, vigencia("actualizado")
  )
  evaluacion <- evaluar(medicion, perfil_evaluacion(
    "P", regla_evaluacion("R", function(x) x > 0.5)
  ))
  reglas <- as.data.frame(evaluacion$reglas)

  expect_true(is.na(reglas$resultado[[1L]]))
  expect_true(is.na(reglas$n_medidas[[1L]]))
})

test_that("con el campo declarado mide y no deja cobertura", {
  # El control: si la abstencion se disparara siempre, no distinguiria nada.
  medicion <- medir_frescura_o61(
    metricas_nucleo()$OportunidadEntPorFecha,
    vigencia("actualizado", fecha_limite = as.Date("2026-01-31"))
  )
  expect_equal(nrow(medicion), 6L)
  expect_null(attr(medicion, "cobertura_metricas", exact = TRUE))
})

test_that("mezclar una fecha de calendario con un instante se declara", {
  nucleo <- metricas_nucleo()
  con_instante <- datos_o61(as.POSIXct(
    c("2026-01-31 00:00:00", "2026-01-31 12:00:00", "2026-02-01 00:00:00")
  ))
  expect_warning(
    medir_frescura_o61(
      nucleo$OportunidadEntPorFecha,
      vigencia("actualizado", fecha_limite = as.Date("2026-01-31")),
      datos = con_instante
    ),
    "fecha de calendario con un instante"
  )

  # Y los dos casos sin mezcla no avisan: la guarda distingue.
  expect_silent(medir_frescura_o61(
    nucleo$OportunidadEntPorFecha,
    vigencia("actualizado", fecha_limite = as.Date("2026-01-31"))
  ))
  expect_silent(medir_frescura_o61(
    nucleo$OportunidadEntPorFecha,
    vigencia("actualizado",
             fecha_limite = as.POSIXct("2026-01-31 23:00:00", tz = "UTC")),
    datos = datos_o61(as.POSIXct(
      c("2026-01-10 12:00:00", "2026-01-31 12:00:00", "2026-02-01 12:00:00"),
      tz = "UTC"
    ))
  ))
})

test_that("la semantica publicada y el borde dicen lo mismo", {
  nucleo <- metricas_nucleo()
  semantica <- attr(nucleo$OportunidadEntPorFecha, "declaracion")$semantica
  expect_true(grepl("inclusive", semantica, fixed = TRUE))
  expect_false(grepl("antes de", semantica, fixed = TRUE))

  medicion <- medir_frescura_o61(
    nucleo$OportunidadEntPorFecha,
    vigencia("actualizado", fecha_limite = as.Date("2026-01-31"))
  )
  frescura <- as.data.frame(medicion)
  frescura <- frescura[as.character(frescura$metrica) == "OportunidadEntPorFecha", ]
  # fila 1: antes del limite; fila 2: exactamente en el limite; fila 3: despues.
  expect_equal(frescura$resultado, c(1, 1, 0))
})

test_that("la impresion de la medicion declara la metrica que no midio", {
  # La impresion es lo primero que se mira despues de medir, y era la unica capa
  # que quedaba muda: el tablero ya imprimia esta tabla, el historico la registra
  # como fila y `evaluar()` deja NA.
  # La impresion sale por DOS canales: el cuadro por la salida estandar y los
  # titulos y avisos por cli. Medido, `capture.output()` trae 19 lineas sin los
  # titulos y `cli_fmt()` trae 4 lineas sin el cuadro: quedarse con uno solo
  # habria hecho pasar una prueba que no mira donde esta el dato.
  texto_impreso <- function(objeto) {
    lineas_cli <- NULL
    otras <- capture.output(lineas_cli <- cli::cli_fmt(print(objeto)))
    paste(c(otras, lineas_cli), collapse = " ")
  }
  medicion <- medir_frescura_o61(
    metricas_nucleo()$OportunidadEntPorFecha, vigencia("actualizado")
  )
  texto <- texto_impreso(medicion)
  expect_true(grepl("Cobertura de m", texto, fixed = TRUE))
  expect_true(grepl("contrato_incompleto", texto, fixed = TRUE))
  expect_true(grepl("fecha_limite", texto, fixed = TRUE))

  # Y el caso donde NINGUNA metrica midio, que antes de esta ronda era imposible
  # porque `medir()` abortaba: lo unico que se imprimia era el encabezado de un
  # cuadro vacio, sin una palabra sobre el motivo.
  sola <- instanciar(
    especializar(metricas_nucleo()$OportunidadEntPorFecha,
                 vigencia = vigencia("actualizado")), "t"
  )
  vacia <- medir(modelo(sola), list(t = datos_o61()), id_medicion = "o61-vacia")
  expect_equal(nrow(vacia), 0L)
  expect_true(grepl("contrato_incompleto", texto_impreso(vacia), fixed = TRUE))

  # El control: con el campo declarado no hay nada que declarar, y la seccion NO
  # se abre. Sin esta mitad, una seccion que se imprimiera siempre pasaria las
  # tres comprobaciones de arriba.
  completa <- medir_frescura_o61(
    metricas_nucleo()$OportunidadEntPorFecha,
    vigencia("actualizado", fecha_limite = as.Date("2026-01-31"))
  )
  expect_false(grepl("Cobertura de m", texto_impreso(completa), fixed = TRUE))
})

# El defecto no estaba en un sitio sino en una propiedad: **comparar temporales de
# clases distintas**. Lo escribi primero para los cuatro sitios que tenia delante
# -las metricas de frescura por entidad-, y habia once mas. Siete los encontro
# este recorrido y no yo, midiendo cada uno contra su control:
#
#   las cuatro de oportunidad POR ATRIBUTO: la fila que cae exactamente en el
#     limite daba `1 1 0` con `TZ=UTC` y `1 0 0` con `TZ=Asia/Tokyo`, sin aviso;
#   ValoresPosiblesPorExtension y ValoresPosiblesPorComprension: un dominio o un
#     rango de instantes contra una columna `Date` no coincide con NINGUN valor y
#     publicaban `0 0 0` donde el control da `1 1 0`;
#   Formato con `diccionario`: idem, `0 0 0` contra `1 1 0`;
#   NoNulo con `valores_nulos`: el peor de todos, porque es la metrica de
#     COMPLETITUD. Con `1900-01-01` como ausencia disfrazada en una columna
#     `Date`, un centinela `POSIXct` publicaba `1 1 1` -no falta nada- y el mismo
#     centinela como fecha de calendario publica `1 0 1`;
#   las tres referenciales: el apareo con el padron pasa por texto, asi que una
#     clave de instantes publicaba `0 0 0` y una cobertura de `0` contra el `1`
#     del control.
#
# Por eso esta prueba no recorre una lista de sitios: recorre los CATALOGOS,
# prueba cada combinacion de propiedades que los validadores aceptan, y fija el
# resultado de CADA PAR (metrica, propiedades, columna). Una metrica nueva que
# compare fechas aparece como un par que no esta en la tabla y la prueba falla.
#
# Los pares con `avisa = FALSE` son el limite declarado de la guarda: una columna
# NUMERICA contra una declaracion temporal no es la confusion entre calendario e
# instante, y ahi la guarda calla a proposito. Sin esa mitad, una guarda que
# avisara siempre pasaria igual.
test_that("toda metrica temporal de los catalogos declara la mezcla de clases", {
  instante <- as.POSIXct("2026-01-31 00:00:00", tz = "UTC")
  datos <- data.frame(
    f = as.Date(c("2026-01-30", "2026-01-31", "2026-02-01")),
    g = c("a", "b", "c"),
    x = c(1, 2, 3)
  )
  contrato_instantes <- vigencia(
    "f", fecha_acceso = instante, fecha_ultimo_cambio = instante,
    fecha_limite = instante, inicio_intervalo = instante - 86400,
    fin_intervalo = instante + 86400, frecuencia_cambio = 1
  )
  avisa_de_mezcla <- function(instancia) {
    avisos <- character(0)
    medicion <- withCallingHandlers(
      try(medir(modelo(instancia), list(t = datos)), silent = TRUE),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (inherits(medicion, "try-error")) return(NULL)
    any(grepl("fecha de calendario con un instante", avisos))
  }

  medidos <- character(0)
  nucleo <- metricas_nucleo()
  for (nombre in names(nucleo)) {
    declaracion <- attr(nucleo[[nombre]], "declaracion", exact = TRUE)
    propiedades <- declaracion$propiedades
    if (!length(propiedades)) next
    # Todas, de a una y de a dos. Con todas juntas, una metrica que exige un
    # predicado no se instancia nunca; con la misma fecha en las dos puntas de un
    # intervalo, su validador lo rechaza por duracion cero. Las dos cosas dejaban
    # metricas sin ejercitar, y una metrica sin ejercitar se cuenta como si no
    # existiera.
    combinaciones <- list(propiedades)
    if (length(propiedades) > 1L) {
      combinaciones <- c(
        combinaciones,
        lapply(seq_along(propiedades), function(i) propiedades[i]),
        unlist(lapply(seq_along(propiedades), function(i) {
          lapply(setdiff(seq_along(propiedades), i), function(j) {
            propiedades[c(i, j)]
          })
        }), recursive = FALSE)
      )
    }
    for (combo in combinaciones) {
      valores <- lapply(seq_along(combo), function(i) {
        if (identical(combo[[i]], "vigencia")) {
          contrato_instantes
        } else {
          instante + (i - 1L) * 86400
        }
      })
      names(valores) <- combo
      especifica <- try(
        do.call(especializar, c(list(nucleo[[nombre]]), valores)), silent = TRUE
      )
      if (inherits(especifica, "try-error")) next
      for (atributo in c("f", "x")) {
        argumentos <- if (identical(declaracion$granularidad, "instanciaEntidad")) {
          list(especifica, "t")
        } else {
          list(especifica, "t", atributo)
        }
        instancia <- try(do.call(instanciar, argumentos), silent = TRUE)
        if (inherits(instancia, "try-error")) next
        avisa <- avisa_de_mezcla(instancia)
        if (is.null(avisa)) next
        medidos <- c(medidos, paste(
          nombre, paste(combo, collapse = "+"), atributo, avisa, sep = "|"
        ))
      }
    }
  }

  # El padron no es una propiedad de la metrica: entra por
  # `instanciar(..., referencial = )`. Por eso el recorrido de arriba no alcanza a
  # estas tres, y son justamente las que quedaron sin guarda la primera vez.
  clave_instantes <- as.POSIXct(c("2026-01-30", "2026-01-31"), tz = "UTC")
  padrones <- list(
    referencial(data.frame(clave = clave_instantes), "clave"),
    referencial(data.frame(clave = clave_instantes, valor = c("a", "b")),
                "clave", valor = "valor"),
    referencial(data.frame(clave = clave_instantes), "clave",
                completo = TRUE, alcance = "padron de prueba")
  )
  referenciales <- metricas_referencial()
  for (nombre in names(referenciales)) {
    for (indice in seq_along(padrones)) {
      for (atributos in list("f", c("f", "g"))) {
        instancia <- try(
          instanciar(especializar(referenciales[[nombre]]), "t", atributos,
                     referencial = padrones[[indice]]),
          silent = TRUE
        )
        if (inherits(instancia, "try-error")) next
        avisa <- avisa_de_mezcla(instancia)
        if (is.null(avisa)) next
        medidos <- c(medidos, paste(
          nombre, paste0("padron", indice), paste(atributos, collapse = "+"),
          avisa, sep = "|"
        ))
      }
    }
  }

  esperados <- c(
    "DesactualizacionPorCambios|vigencia|f|TRUE",
    "DesactualizacionPorCambios|vigencia|x|TRUE",
    "DesactualizacionPorFecha|vigencia|f|TRUE",
    "DesactualizacionPorFecha|vigencia|x|TRUE",
    "Formato|diccionario|f|TRUE",
    "Formato|diccionario|x|FALSE",
    "GradoOportunidadAtributoPorFecha|fecha_solicitud+fecha_fin_utilidad|f|TRUE",
    "GradoOportunidadAtributoPorIntervalo|inicio_vigencia+fin_vigencia|f|TRUE",
    "NoNulo|valores_nulos|f|TRUE",
    "NoNulo|valores_nulos|x|FALSE",
    "OportunidadAtributoPorFecha|fecha_limite|f|TRUE",
    "OportunidadAtributoPorIntervalo|inicio_vigencia+fin_vigencia|f|TRUE",
    "OportunidadEntPorFecha|vigencia|f|TRUE",
    "OportunidadEntPorFecha|vigencia|x|TRUE",
    "OportunidadEntPorIntervalo|vigencia|f|TRUE",
    "OportunidadEntPorIntervalo|vigencia|x|TRUE",
    "ValoresPosiblesPorComprension|minimo+maximo|f|TRUE",
    "ValoresPosiblesPorComprension|minimo+maximo|x|FALSE",
    "ValoresPosiblesPorExtension|valores|f|TRUE",
    "ValoresPosiblesPorExtension|valores|x|FALSE",
    # `CorrectitudSemFuerte` mide con los tres padrones -solo necesita la clave-,
    # y las tres corridas quedan fijadas: mas cobertura por el mismo precio.
    "CorrectitudSemFuerte|padron1|f|TRUE",
    "CorrectitudSemFuerte|padron2|f|TRUE",
    "CorrectitudSemFuerte|padron3|f|TRUE",
    "CorrectitudSemDebil|padron2|f+g|TRUE",
    "RatioCobertura|padron3|f|TRUE"
  )
  expect_setequal(unique(medidos), esperados)
  # Y que se midio algo: una tabla vacia satisface cualquier comparacion de
  # conjuntos con ella misma.
  expect_gte(length(unique(medidos)), length(esperados))
})

test_that("la mezcla cambia el numero y por eso se declara", {
  # La guarda no arregla el numero: lo declara. Aca se mide que el numero CAMBIA,
  # que es lo que la hace necesaria. El instante se parsea dentro de cada huso: si
  # se parsea una sola vez arriba, se mide tres veces el huso inicial.
  datos <- data.frame(f = as.Date(c("2026-01-30", "2026-01-31", "2026-02-01")))
  huso_previo <- Sys.getenv("TZ", unset = NA)
  on.exit({
    if (is.na(huso_previo)) Sys.unsetenv("TZ") else Sys.setenv(TZ = huso_previo)
  }, add = TRUE)

  medir_con_huso <- function(huso) {
    Sys.setenv(TZ = huso)
    limite <- as.POSIXct("2026-01-31 00:00:00")
    instancia <- instanciar(
      especializar(metricas_nucleo()$OportunidadAtributoPorFecha,
                   fecha_limite = limite), "t", "f"
    )
    suppressWarnings(as.numeric(medir(modelo(instancia), list(t = datos))$resultado))
  }
  expect_equal(medir_con_huso("UTC"), c(1, 1, 0))
  expect_equal(medir_con_huso("Asia/Tokyo"), c(1, 0, 0))
})
