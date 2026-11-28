# Pesada para CRAN: construye tablas, perfila y consulta dos motores en memoria.
testthat::skip_on_cran()

.hallazgos_de <- function(datos, tipo) {
  h <- as.data.frame(hallazgos(perfilar(datos, nombre = "x")))
  h[h$tipo_hallazgo == tipo, , drop = FALSE]
}

.datos_texto_truncado <- function(n = 500L, cortados = 40L) {
  set.seed(11)
  largos <- sprintf(
    "Descripcion numero %d del tramite", sample(1:1e6, n)
  )
  texto <- sprintf("Caso %d", sample(1:1e6, n))
  texto[seq_len(cortados)] <- substr(largos[seq_len(cortados)], 1L, 20L)
  data.frame(descripcion = texto, stringsAsFactors = FALSE)
}

.datos_apilamiento <- function(n = 500L) {
  set.seed(12)
  edad <- round(runif(n, 20, 80) / 5) * 5
  edad[1:50] <- sample(20:80, 50)
  data.frame(edad = edad)
}

.datos_serie <- function(n = 500L) {
  set.seed(13)
  acum <- cumsum(sample(1:10, n, TRUE))
  acum[200:210] <- acum[200:210] - 200
  data.frame(acumulado = acum)
}

.datos_escala <- function(n = 400L) {
  set.seed(14)
  altura <- round(rnorm(n, 170, 8))
  altura[1:25] <- round(altura[1:25] / 100, 2)
  data.frame(altura = altura)
}

test_that("el texto cortado en un tope de campo se informa con su traza", {
  h <- .hallazgos_de(.datos_texto_truncado(), "texto_truncado")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados[[1L]], 40)
  expect_equal(h$n_evaluados[[1L]], 500)
  expect_match(h$evidencia[[1L]], "20")
  expect_match(h$evidencia[[1L]], "anterior y posterior")
  traza <- h$trazabilidad[[1L]]
  expect_equal(traza$estado, "disponible")
  expect_equal(length(traza$indices_fila), 40L)
})

test_that("un codigo de largo fijo o una categorica no son texto truncado", {
  set.seed(15)
  codigos <- data.frame(
    codigo = sprintf("%08d", sample(1:99999999, 500L)),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(.hallazgos_de(codigos, "texto_truncado")), 0L)

  set.seed(16)
  motivos <- sprintf("Motivo de consulta %02d", 1:25)
  categorica <- data.frame(
    motivo = sample(motivos, 800L, TRUE),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(.hallazgos_de(categorica, "texto_truncado")), 0L)
})

test_that("un tope tipico avisa con pocas filas cortadas", {
  set.seed(17)
  texto <- sprintf("Texto libre %d con palabras", sample(1:1e6, 200L))
  largo <- paste(rep("palabra", 60), collapse = " ")
  for (i in 1:3) {
    texto[[i]] <- substr(sprintf("%d %s", i, largo), 1L, 255L)
  }
  h <- .hallazgos_de(
    data.frame(descripcion = texto, stringsAsFactors = FALSE),
    "texto_truncado"
  )
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados[[1L]], 3)
})

test_that("un texto de ancho fijo o de vocabulario discreto no es truncado", {
  set.seed(31)
  n <- 500L
  textos <- list(
    fecha_registro = format(
      as.POSIXct("2024-01-01", tz = "UTC") + sample(0:3e7, n), "%Y-%m-%d %H:%M:%S"
    ),
    matricula = paste(
      replicate(n, paste(sample(LETTERS, 3, TRUE), collapse = "")),
      sample(1000:9999, n, TRUE)
    ),
    telefono = sprintf(
      "09%d %03d %03d", sample(1:9, n, TRUE),
      sample(0:999, n, TRUE), sample(0:999, n, TRUE)
    ),
    iban = sprintf(
      "UY%02d %04d %04d %04d", sample(10:99, n, TRUE),
      sample(0:9999, n, TRUE), sample(0:9999, n, TRUE),
      sample(0:9999, n, TRUE)
    ),
    expediente = paste0("Expediente ", sample(10000:99999, n, TRUE), "/2024"),
    direccion = paste("Av Italia", sample(1:9999, n, TRUE))
  )
  for (columna in names(textos)) {
    datos <- stats::setNames(
      data.frame(textos[[columna]], stringsAsFactors = FALSE), columna
    )
    expect_equal(
      nrow(.hallazgos_de(datos, "texto_truncado")), 0L, info = columna
    )
  }

  # Vocabulario discreto de nombres: el maximo natural cae en un tope tipico,
  # pero termina en una palabra completa de la columna.
  nombres <- c(
    "Maria", "Juan", "Ana", "Luis", "Carlos", "Lucia", "Pedro", "Sofia",
    "Diego", "Valentina", "Martin", "Camila", "Gonzalo", "Florencia",
    "Santiago", "Agustina", "Nicolas", "Mariana", "Alejandro", "Victoria"
  )
  apellidos <- c(
    "Rodriguez", "Fernandez", "Gonzalez", "Lopez", "Martinez", "Perez",
    "Garcia", "Sosa", "Silva", "Alvarez", "Techera", "Cabrera",
    "Etcheverry", "Bentancor", "Larrosa", "Pintos"
  )
  set.seed(2)
  completos <- paste(
    sample(nombres, 800L, TRUE), sample(apellidos, 800L, TRUE),
    sample(apellidos, 800L, TRUE)
  )
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(nombre_completo = completos, stringsAsFactors = FALSE),
      "texto_truncado"
    )),
    0L
  )
})

test_that("un tope que corta a la mayoria de la columna sigue siendo truncado", {
  palabras <- c(
    "el", "de", "la", "que", "y", "en", "un", "se", "no", "por", "con",
    "una", "su", "para", "es", "al", "lo", "como", "mas", "pero",
    "servicio", "tramite", "solicitud", "usuario", "atencion", "respuesta",
    "documento"
  )
  cortas <- c(
    "el", "de", "la", "que", "y", "en", "un", "se", "no", "por", "con",
    "una", "su", "para", "es", "al", "lo"
  )
  set.seed(41)
  largos <- vapply(seq_len(900), function(i) {
    substr(
      paste(sample(palabras, sample(40:80, 1L), TRUE), collapse = " "),
      1L, 100L
    )
  }, "")
  breves <- vapply(seq_len(100), function(i) {
    paste(sample(cortas, sample(2:5, 1L), TRUE), collapse = " ")
  }, "")
  for (tope in c(100L, 50L)) {
    texto <- c(substr(largos, 1L, tope), breves)
    h <- .hallazgos_de(
      data.frame(comentario = texto, stringsAsFactors = FALSE),
      "texto_truncado"
    )
    expect_equal(nrow(h), 1L, info = paste("tope", tope))
    expect_equal(h$n_afectados[[1L]], 900L, info = paste("tope", tope))
  }
})

test_that("una palabra cortada de una o dos letras no calla el tope", {
  cortas <- c(
    "el", "de", "la", "que", "y", "en", "un", "se", "no", "por", "con",
    "una", "su", "para", "es", "al", "lo", "como", "mas", "pero", "otro"
  )
  largas <- c(
    "documento", "respuesta", "solicitud", "atencion", "servicio",
    "tramite", "usuario", "observacion", "descripcion", "comentario"
  )
  set.seed(42)
  breves <- vapply(seq_len(1000), function(i) {
    paste(sample(cortas, sample(3:12, 1L), TRUE), collapse = " ")
  }, "")
  prefijos <- vapply(seq_len(3), function(i) {
    substr(paste(sample(largas, 6L, TRUE), collapse = " "), 1L, 47L)
  }, "")
  cortados <- paste0(prefijos, " de")
  expect_true(all(nchar(cortados) == 50L))
  expect_equal(length(unique(cortados)), 3L)
  h <- .hallazgos_de(
    data.frame(comentario = c(breves, cortados), stringsAsFactors = FALSE),
    "texto_truncado"
  )
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados[[1L]], 3L)
})

test_that("el apilamiento usa el indice de Whipple con la misma regla por base", {
  set.seed(18)
  n <- 500L
  cinco <- sample(20:80, n, TRUE)
  cinco[sample(n, 300L)] <- 5L * sample(4:16, 300L, TRUE)
  h <- .hallazgos_de(data.frame(edad = cinco), "apilamiento")
  expect_equal(nrow(h), 1L)
  expect_match(h$evidencia[[1L]], "M\u00faltiplos de 5")
  expect_match(h$evidencia[[1L]], "Whipple")
  expect_match(h$evidencia[[1L]], "Naciones Unidas")

  set.seed(19)
  diez <- sample(20:80, n, TRUE)
  diez[sample(n, 225L)] <- 10L * sample(2:8, 225L, TRUE)
  h <- .hallazgos_de(data.frame(edad = diez), "apilamiento")
  expect_equal(nrow(h), 1L)
  expect_match(h$evidencia[[1L]], "M\u00faltiplos de 10")
})

test_that("el apilamiento reconoce magnitudes por palabra completa", {
  set.seed(20)
  n <- 500L
  edad <- sample(20:80, n, TRUE)
  edad[sample(n, 400L)] <- 5L * sample(4:16, 400L, TRUE)
  expect_equal(
    nrow(.hallazgos_de(data.frame(edad_al_ingreso = edad), "apilamiento")), 1L
  )
  expect_equal(
    nrow(.hallazgos_de(data.frame(talla_cm = edad), "apilamiento")), 1L
  )
  # Un plazo por norma no es una magnitud medida a mano: sus multiplos vienen
  # del contrato, no del redondeo de quien mide.
  expect_equal(
    nrow(.hallazgos_de(data.frame(plazo_pago_dias = edad), "apilamiento")), 0L
  )
})

test_that("el apilamiento separa nombres compuestos y magnitudes medidas a mano", {
  set.seed(32)
  n <- 500L
  apilada <- function() {
    ifelse(runif(n) < 0.8, 5 * sample(4:16, n, TRUE), sample(18:80, n, TRUE))
  }
  for (columna in c(
    "EdadIngreso", "edadpaciente", "edad2", "pa_sistolica", "cintura_cm"
  )) {
    datos <- stats::setNames(data.frame(apilada()), columna)
    expect_equal(
      nrow(.hallazgos_de(datos, "apilamiento")), 1L, info = columna
    )
  }

  # Las horas de trabajo y los anios de servicio se redondean por convencion,
  # no por la forma de medirlos.
  set.seed(33)
  horas <- data.frame(
    horas_semanales = sample(
      c(40, 40, 40, 40, 35, 45, 30, 20, 48, 44, 36, 25, 38, 42, 8, 60, 15, 10),
      1000L, TRUE
    )
  )
  expect_equal(nrow(.hallazgos_de(horas, "apilamiento")), 0L)
  servicio <- data.frame(
    anios_servicio = c(
      sample(c(5, 10, 15, 20, 25, 30, 35), 500L, TRUE),
      sample(1:38, 500L, TRUE)
    )
  )
  expect_equal(nrow(.hallazgos_de(servicio, "apilamiento")), 0L)
  espera <- data.frame(
    tiempo_espera = sample(c(5, 10, 15, 20, 30, 45, 60, 7, 12), 1000L, TRUE)
  )
  expect_equal(nrow(.hallazgos_de(espera, "apilamiento")), 0L)

  # Muestras chicas: el indice exige un minimo de filas y que su limite
  # inferior supere el corte. Estas semillas acusaban en la version anterior.
  for (s in c(4L, 9L, 19L)) {
    set.seed(s)
    datos <- data.frame(edad = sample(c(20:34, 40:44), 80L, TRUE))
    expect_equal(
      nrow(.hallazgos_de(datos, "apilamiento")), 0L, info = paste("n=80", s)
    )
  }
  set.seed(150)
  datos <- data.frame(edad = sample(18:99, 150L, TRUE))
  expect_equal(nrow(.hallazgos_de(datos, "apilamiento")), 0L)
})

test_that("un redondeo mas grueso o una columna que no es magnitud no se apila", {
  set.seed(21)
  n <- 500L
  valor <- 100L * sample(1:1000, n, TRUE)
  valor[sample(n, 425L)] <- 500L * sample(1:100, 425L, TRUE)
  expect_equal(nrow(.hallazgos_de(data.frame(valor = valor), "apilamiento")), 0L)
  expect_equal(
    nrow(.hallazgos_de(data.frame(inventario_dias = valor), "apilamiento")), 0L
  )

  set.seed(22)
  precio <- sample(seq(10, 200, by = 5), n, TRUE)
  expect_equal(nrow(.hallazgos_de(data.frame(precio = precio), "apilamiento")), 0L)
})

test_that("la resolucion, los plazos y las siglas clinicas se miden por su grilla", {
  set.seed(43)
  n <- 1000L
  apilada <- function() {
    ifelse(runif(n) < 0.8, 5L * sample(4:16, n, TRUE), sample(18:80, n, TRUE))
  }

  # Un aparato de 2 mmHg o una talla de ropa de dos en dos: los pares son la
  # grilla de la medicion, no una preferencia por el digito terminal.
  pares <- data.frame(presion_sistolica = 2L * sample(45:90, n, TRUE))
  expect_equal(nrow(.hallazgos_de(pares, "apilamiento")), 0L)
  ropa <- data.frame(talla = sample(seq(36, 58, by = 2), n, TRUE))
  ropa$talla[sample(n, 100L)] <- sample(c(40, 50), 100L, TRUE)
  expect_equal(nrow(.hallazgos_de(ropa, "apilamiento")), 0L)
  expect_equal(
    nrow(.hallazgos_de(data.frame(edad = apilada()), "apilamiento")), 1L
  )

  for (columna in c(
    "PAS", "TAS", "pad", "sbp", "dbp", "bp", "imc", "bmi",
    "circunferencia_abdominal"
  )) {
    datos <- stats::setNames(data.frame(apilada()), columna)
    expect_equal(
      nrow(.hallazgos_de(datos, "apilamiento")), 1L, info = columna
    )
  }
  # Los valores de laboratorio no se miden a mano y no habilitan la regla.
  for (columna in c("glucemia", "colesterol")) {
    datos <- stats::setNames(data.frame(apilada()), columna)
    expect_equal(
      nrow(.hallazgos_de(datos, "apilamiento")), 0L, info = columna
    )
  }

  # Los plazos por norma traen sus multiplos del contrato, no del redondeo.
  plazos <- c(30, 60, 90, 45, 15, 120, 7, 14, 21, 28, 35, 18)
  set.seed(44)
  for (columna in c("dias_plazo", "dias_vencimiento", "plazo_pago_dias")) {
    datos <- stats::setNames(
      data.frame(
        sample(plazos, n, TRUE, prob = c(10, 8, 6, 3, 2, 1, 4, 3, 2, 2, 1, .5))
      ),
      columna
    )
    expect_equal(
      nrow(.hallazgos_de(datos, "apilamiento")), 0L, info = columna
    )
  }
})

test_that("el apilamiento no confunde un diseno redondeado con un sesgo", {
  set.seed(23)
  redondeada <- data.frame(edad = sample(seq(20, 80, by = 5), 500, TRUE))
  expect_equal(nrow(.hallazgos_de(redondeada, "apilamiento")), 0L)

  set.seed(24)
  precios <- data.frame(
    precio = sample(seq(10, 200, by = 10), 500, TRUE) - 0.01
  )
  expect_equal(nrow(.hallazgos_de(precios, "apilamiento")), 0L)
})

test_that("una serie acumulada que retrocede se informa y un reinicio no", {
  h <- .hallazgos_de(.datos_serie(), "serie_acumulada_retrocede")
  expect_equal(nrow(h), 1L)
  expect_gte(h$n_afectados[[1L]], 1)
  expect_equal(h$trazabilidad[[1L]]$estado, "disponible")

  # Un acumulado mensual que arranca en enero baja a menos de la mitad del pico
  # anterior y vuelve a crecer: es un reinicio, no un retroceso.
  set.seed(25)
  for (s in 1:20) {
    set.seed(s)
    anual <- unlist(lapply(1:5, function(a) {
      cumsum(round(100 * rlnorm(12, 0, 0.3)))
    }))
    expect_equal(
      nrow(.hallazgos_de(
        data.frame(acumulado_anual = anual), "serie_acumulada_retrocede"
      )), 0L
    )
  }

  # Contadores que vuelven a cero: cada caida es un reinicio.
  set.seed(26)
  contador <- data.frame(
    contador = rep(cumsum(sample(1:10, 50, TRUE)), 10)
  )
  expect_equal(
    nrow(.hallazgos_de(contador, "serie_acumulada_retrocede")), 0L
  )
  # Acumulados por grupo que arrancan en una base alta: el corte entre grupos
  # es el reinicio de otra serie.
  set.seed(27)
  saldo <- unlist(lapply(1:30, function(g) {
    500 + cumsum(sample(1:10, 40, TRUE))
  }))
  expect_equal(
    nrow(.hallazgos_de(data.frame(saldo = saldo), "serie_acumulada_retrocede")),
    0L
  )
})

test_that("una perdida permanente o una caida de una fila no son un reinicio", {
  set.seed(35)
  n <- 500L
  acumulado <- cumsum(sample(5:50, n, TRUE))
  for (perdida in c(0.9, 0.7, 0.5, 0.3)) {
    x <- acumulado
    x[251:n] <- round(x[251:n] - perdida * x[250])
    h <- .hallazgos_de(
      data.frame(acumulado = x), "serie_acumulada_retrocede"
    )
    expect_equal(nrow(h), 1L, info = paste("perdida", perdida))
    expect_equal(h$n_afectados[[1L]], 1L)
  }
  for (fraccion in c(0.8, 0.6, 0.4, 0.1)) {
    x <- acumulado
    x[250] <- round(x[250] * fraccion)
    expect_equal(
      nrow(.hallazgos_de(
        data.frame(acumulado = x), "serie_acumulada_retrocede"
      )),
      1L,
      info = paste("caida de una fila", fraccion)
    )
  }
  # Una base alta no vuelve reinicio a una caida chica.
  x <- acumulado + 5e6
  x[250] <- x[250] - 100
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(acumulado = x), "serie_acumulada_retrocede"
    )),
    1L
  )
})

test_that("un saldo inicial alto no vuelve reinicio a un retroceso", {
  set.seed(45)
  n <- 1000L
  saldo <- 5e6 + cumsum(sample(5:50, n, TRUE))
  un_valor <- saldo
  un_valor[500] <- un_valor[500] - 1e4
  h <- .hallazgos_de(
    data.frame(acumulado = un_valor), "serie_acumulada_retrocede"
  )
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados[[1L]], 1L)

  permanente <- saldo
  permanente[501:n] <- permanente[501:n] - 12000
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(acumulado = permanente), "serie_acumulada_retrocede"
    )),
    1L
  )

  # Un reinicio de verdad vuelve al primer valor y sigue desde ahi.
  reinicio <- saldo
  reinicio[501:n] <- reinicio[501:n] - (reinicio[500] - reinicio[1])
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(acumulado = reinicio), "serie_acumulada_retrocede"
    )),
    0L
  )
})

test_that("los cortes regulares de cuentas apiladas no son retrocesos", {
  # Cortes regulares que parten la columna en tramos crecientes: cuentas
  # apiladas con saldo inicial distinto, no retrocesos.
  bases <- rep(c(20, 5000, 1000, 3000, 800, 4000), length.out = 30L)
  set.seed(46)
  apilado <- unlist(lapply(seq_along(bases), function(g) {
    bases[[g]] + cumsum(sample(1:10, 40L, TRUE))
  }))
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(saldo_acumulado = apilado), "serie_acumulada_retrocede"
    )),
    0L
  )

  # El corte no siempre cae cada exactamente 40 filas: basta con que los
  # tramos crecientes tengan una longitud parecida.
  largos <- c(38L, 41L, 39L, 42L, 40L, 37L, 43L, 39L)
  bases <- c(1000L, 100L, 200L, 50L, 150L, 80L, 200L, 20L)
  irregular <- unlist(Map(function(base, largo) {
    base + cumsum(rep(5L, largo))
  }, bases, largos), use.names = FALSE)
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(acumulado = irregular), "serie_acumulada_retrocede"
    )),
    0L
  )

  # Si el nombre dice saldo no hay una llave para separar las cuentas: no se
  # habilita la regla, aunque tambien contenga `acumulado`.
  real <- cumsum(seq_len(1000L))
  real[500L] <- real[499L] - 1000L
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(saldo_acumulado = real), "serie_acumulada_retrocede"
    )),
    0L
  )
})

test_that("un contador de dias ordenado no es una serie acumulada", {
  # Una columna de dias desde el inicio, ordenada, con una fila fuera de orden
  # no es una serie que acumula: es un plazo.
  set.seed(77)
  dias <- .sort_por_bytes(sample(0:365, 1000L, TRUE))
  dias[500] <- 100
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(dias_desde_inicio = dias), "serie_acumulada_retrocede"
    )),
    0L
  )
})

test_that("un identificador, una numeracion o una poblacion no son una serie", {
  id <- 1:1000
  id[c(100L, 500L)] <- id[c(500L, 100L)]
  expect_equal(
    nrow(.hallazgos_de(data.frame(id = id), "serie_acumulada_retrocede")), 0L
  )
  set.seed(28)
  poblacion <- cumsum(runif(200, -1, 2))
  expect_equal(
    nrow(.hallazgos_de(
      data.frame(poblacion = poblacion), "serie_acumulada_retrocede"
    )), 0L
  )
})

test_that("una serie con ausentes se evalua sobre los presentes", {
  set.seed(29)
  acum <- cumsum(sample(1:10, 500, TRUE))
  acum[200:210] <- acum[200:210] - 200
  acum[c(50L, 150L, 250L)] <- NA_real_
  h <- .hallazgos_de(
    data.frame(acumulado = acum), "serie_acumulada_retrocede"
  )
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_evaluados[[1L]], 497)
})

test_that("la escala mixta es informativa y el recorte no se recomienda ni aplica", {
  h <- .hallazgos_de(.datos_escala(), "escala_mixta")
  expect_equal(nrow(h), 1L)
  expect_equal(as.character(h$severidad[[1L]]), "ok")
  expect_equal(h$n_afectados[[1L]], 25)
  expect_match(h$evidencia[[1L]], "100")
  expect_match(h$descripcion[[1L]], "dos poblaciones distintas")

  datos <- .datos_escala()
  plan <- planificar_limpieza(perfilar(datos, nombre = "x"))
  acciones <- plan[plan$columna == "altura", , drop = FALSE]
  expect_true("marcar_outliers" %in% acciones$estrategia)
  winsor <- acciones[which(acciones$estrategia == "winsorizar_outliers"), ]
  expect_equal(nrow(winsor), 1L)
  expect_false(winsor$recomendada[[1L]])
  expect_false(winsor$aplicar[[1L]])
  expect_match(winsor$justificacion[[1L]], "mezclar escalas")
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_false("escala_mixta" %in% sin_accion$hallazgo)
})

test_that("una lognormal o una bimodal legitima no se acusan de escala mixta", {
  set.seed(30)
  for (sigma in c(0.5, 1, 1.5, 2, 2.5, 3)) {
    datos <- data.frame(x = rlnorm(400, 0, sigma))
    expect_equal(nrow(.hallazgos_de(datos, "escala_mixta")), 0L)
  }
  bimodal <- data.frame(x = c(rnorm(200, 25, 4), rnorm(200, 75, 8)))
  expect_equal(nrow(.hallazgos_de(bimodal, "escala_mixta")), 0L)
})

test_that("perfilar_por publica los hallazgos dentro de cada grupo", {
  texto <- .datos_texto_truncado(n = 300L, cortados = 30L)
  edad <- .datos_apilamiento(n = 300L)
  datos <- data.frame(
    grupo = rep(c("A", "B"), length.out = 300L),
    descripcion = texto$descripcion,
    edad = edad$edad
  )
  por <- perfilar_por(datos, "grupo", min_filas = 150L)
  expect_true(any(por$tipo_hallazgo == "texto_truncado"))
  expect_true(any(por$tipo_hallazgo == "apilamiento"))
  expect_true(all(c("A", "B") %in% por$grupo[
    por$tipo_hallazgo == "texto_truncado"
  ]))
})

test_that("los hallazgos llegan a SQLite y DuckDB en memoria", {
  datos <- cbind(
    .datos_texto_truncado(), .datos_apilamiento(),
    .datos_serie(), .datos_escala(500L)
  )
  motores <- list()
  if (requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE)) {
    motores$sqlite <- function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  if (requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("duckdb", quietly = TRUE)) {
    motores$duckdb <- function() DBI::dbConnect(.duckdb_prueba(), ":memory:")
  }
  skip_if(length(motores) == 0L, "Sin motores DBI en memoria disponibles.")
  for (nombre_motor in names(motores)) {
    con <- motores[[nombre_motor]]()
    tryCatch({
      DBI::dbWriteTable(con, "exactitud", datos)
      perfil <- perfilar_dbi(con, "exactitud")
      tipos <- as.character(perfil$perfil_muestra$hallazgos$tipo_hallazgo)
      expect_true(
        all(c(
          "texto_truncado", "apilamiento", "serie_acumulada_retrocede",
          "escala_mixta"
        ) %in% tipos),
        info = nombre_motor
      )
    }, finally = DBI::dbDisconnect(con))
  }
})

test_that("una muestra parcial declara que no evaluo la serie acumulada", {
  skip_if_not_installed("RSQLite")
  datos <- data.frame(
    id = seq_len(2000L),
    acumulado = cumsum(sample(1:10, 2000L, TRUE))
  )
  datos$acumulado[1501:2000] <- datos$acumulado[1501:2000] - 500
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "serie", datos)
  perfil <- suppressWarnings(perfilar_dbi(con, "serie", muestra = 100L))
  tipos <- as.character(perfil$perfil_muestra$hallazgos$tipo_hallazgo)
  expect_false("serie_acumulada_retrocede" %in% tipos)
  cobertura <- perfil$perfil_muestra$cobertura_diagnosticos
  fila <- cobertura[
    cobertura$diagnostico == "serie_acumulada_retrocede", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "orden")
})

test_that("analizar() y el plan de limpieza no destruyen datos por estos tipos", {
  datos <- .datos_escala()
  analisis <- analizar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_true("escala_mixta" %in% analisis$perfil$hallazgos$tipo_hallazgo)

  plan <- planificar_limpieza(perfilar(datos, nombre = "x"))
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_true(is.data.frame(sin_accion))
  expect_false("escala_mixta" %in% plan$hallazgo)

  limpio <- data.frame(altura = rnorm(400, 170, 8))
  comparacion <- comparar_perfiles(
    perfilar(datos, nombre = "a", fecha = as.POSIXct("2026-01-01", tz = "UTC")),
    perfilar(limpio, nombre = "b", fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  )
  expect_s3_class(comparacion, "deriva_perfil")
})
