# Los dos hallazgos informativos de ausencia -`posible_ausencia_estructural` y
# `regla_silencia_ausencia`- cuentan filas, y ahora las nombran: su traza trae
# exactamente las filas de `n_afectados`. Antes decian "200 ausencias" con la
# traza vacia con la que nace un hallazgo, y la guarda no los miraba por ser
# informativos.
#
# Pesada para CRAN (perfila una veintena de tablas chicas): corre con
# NOT_CRAN=true, en la suite completa y en la integracion continua.
testthat::skip_on_cran()

perfilar_ausencia <- function(datos, ...) {
  perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ...
  )
}

hallazgo_de_ausencia <- function(perfil, tipo, columna) {
  h <- perfil$hallazgos
  hallazgo <- h[as.character(h$tipo_hallazgo) == tipo &
                  as.character(h$columna) == columna, , drop = FALSE]
  expect_equal(nrow(hallazgo), 1L, info = paste(tipo, columna))
  hallazgo
}

campos_de_base <- c(
  "estado", "localizador", "indices_fila", "claves", "total", "mostrados",
  "truncado", "limite", "alcance"
)

# La forma de los hallazgos de fila: las filas afectadas, ni una mas ni una
# menos, y `total` igual a lo que el hallazgo cuenta.
esperar_filas_de_ausencia <- function(perfil, tipo, columna, esperadas) {
  hallazgo <- hallazgo_de_ausencia(perfil, tipo, columna)
  etiqueta <- paste(tipo, columna)
  expect_identical(as.character(hallazgo$severidad), "ok", info = etiqueta)
  expect_identical(as.character(hallazgo$unidad_conteo), "fila", info = etiqueta)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_identical(traza$estado, "disponible", info = etiqueta)
  expect_identical(traza$alcance, "completo", info = etiqueta)
  expect_identical(traza$indices_fila, as.integer(esperadas), info = etiqueta)
  expect_equal(traza$total, length(esperadas), info = etiqueta)
  expect_equal(hallazgo$n_afectados, length(esperadas), info = etiqueta)
  # Sin `columnas`: el hallazgo es de una columna, y ese campo lo leen el plan
  # y la deriva para tratarlo como de varias.
  expect_setequal(names(traza), campos_de_base)
  invisible(traza)
}

detalle_de_la_guarda_de_ausencia <- function(expr) {
  detalle <- character()
  withCallingHandlers(expr, lupa_trazabilidad_incoherente = function(w) {
    detalle <<- c(detalle, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  detalle
}

# `valor` existe solo cuando `tipo` es "A". Filas intercaladas, para que los
# indices no sean un bloque, y tres ausencias que la regla no explica.
tabla_por_niveles <- function() {
  tipo <- rep(c("A", "B"), each = 200L)
  valor <- ifelse(tipo == "A", "dato", NA)
  valor[c(5L, 9L, 13L)] <- NA
  orden <- as.vector(rbind(1:200, 201:400))
  datos <- data.frame(
    tipo = tipo[orden], valor = valor[orden], stringsAsFactors = FALSE
  )
  datos
}

# `fecha_jubilacion` existe a partir de los 65.
tabla_por_umbral <- function() {
  edad <- rep(18:90, length.out = 600L)
  data.frame(
    edad = edad,
    fecha_jubilacion = ifelse(edad >= 65L, "2020-01-01", NA),
    stringsAsFactors = FALSE
  )
}

test_that("la ausencia determinada por niveles nombra las filas que la regla explica", {
  datos <- tabla_por_niveles()
  expect_no_warning(
    perfil <- perfilar_ausencia(datos), class = "lupa_trazabilidad_incoherente"
  )
  explicadas <- which(is.na(datos$valor) & datos$tipo == "B")
  expect_length(explicadas, 200L)
  traza <- esperar_filas_de_ausencia(
    perfil, "posible_ausencia_estructural", "valor", explicadas
  )
  # Las tres ausencias de donde la columna si corresponde no entran: el
  # hallazgo no las cuenta.
  sin_explicar <- which(is.na(datos$valor) & datos$tipo == "A")
  expect_length(sin_explicar, 3L)
  expect_false(any(sin_explicar %in% traza$indices_fila))
})

test_that("la ausencia determinada por un umbral nombra el lado donde no corresponde", {
  datos <- tabla_por_umbral()
  perfil <- perfilar_ausencia(datos)
  esperar_filas_de_ausencia(
    perfil, "posible_ausencia_estructural", "fecha_jubilacion",
    which(is.na(datos$fecha_jubilacion) & datos$edad < 65L)
  )

  # El otro sentido: el tutor existe por debajo de los 18.
  edad <- rep(5:60, length.out = 600L)
  menores <- data.frame(
    edad = edad, tutor = ifelse(edad < 18L, "si", NA), stringsAsFactors = FALSE
  )
  esperar_filas_de_ausencia(
    perfilar_ausencia(menores), "posible_ausencia_estructural", "tutor",
    which(is.na(menores$tutor) & menores$edad >= 18L)
  )
})

test_that("con ruido y con el determinante ausente la traza sigue siendo lo que se cuenta", {
  datos <- tabla_por_umbral()
  # Cuatro ausencias del lado donde la columna corresponde, y diez filas sin
  # edad: ni unas ni otras son ausencias que la regla explique.
  del_lado_que_corresponde <- which(datos$edad >= 65L)[c(1L, 20L, 40L, 60L)]
  datos$fecha_jubilacion[del_lado_que_corresponde] <- NA
  sin_edad <- seq(7L, 600L, by = 61L)
  datos$edad[sin_edad] <- NA
  expect_no_warning(
    perfil <- perfilar_ausencia(datos), class = "lupa_trazabilidad_incoherente"
  )
  explicadas <- which(
    is.na(datos$fecha_jubilacion) & !is.na(datos$edad) & datos$edad < 65L
  )
  traza <- esperar_filas_de_ausencia(
    perfil, "posible_ausencia_estructural", "fecha_jubilacion", explicadas
  )
  expect_false(any(del_lado_que_corresponde %in% traza$indices_fila))
  expect_false(any(sin_edad %in% traza$indices_fila))
})

test_that("las columnas que se reparten las filas nombran cada una sus ausencias", {
  cual <- rep(c("a", "b", "c"), length.out = 300L)
  datos <- data.frame(
    tel_fijo = ifelse(cual == "a", "2900", NA),
    tel_movil = ifelse(cual == "b", "099", NA),
    correo = ifelse(cual == "c", "x@y.uy", NA),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar_ausencia(datos)
  for (columna in names(datos)) {
    esperar_filas_de_ausencia(
      perfil, "posible_ausencia_estructural", columna,
      which(is.na(datos[[columna]]))
    )
  }
})

test_that("una columna opcional nombra sus celdas vacias, no las disfrazadas", {
  # 150 `NA` y 10 `s/d`. En una columna opcional el universo son las celdas
  # con valor, y `s/d` es un valor: el aviso cuenta 150, no 160.
  datos <- data.frame(
    x = c(rep(NA, 150), rep("s/d", 10), rep("v", 40)),
    tipo = rep(c("A", "B"), each = 100), stringsAsFactors = FALSE
  )
  perfil <- perfilar_ausencia(datos, columnas_opcionales = "x")
  traza <- esperar_filas_de_ausencia(
    perfil, "regla_silencia_ausencia", "x", 1:150
  )
  expect_false(any(151:160 %in% traza$indices_fila))
})

test_that("una regla de aplicabilidad nombra las ausentes de dentro del universo", {
  datos <- data.frame(
    x = c(rep(NA, 150), rep("s/d", 10), rep("v", 40)),
    tipo = rep(c("A", "B"), each = 100), stringsAsFactors = FALSE
  )
  regla <- list(x = ~ tipo == "A")
  # El universo son las 100 primeras filas, todas vacias. Las 50 vacias de
  # fuera no cuentan.
  esperar_filas_de_ausencia(
    perfilar_ausencia(datos, aplicabilidad = regla),
    "regla_silencia_ausencia", "x", 1:100
  )

  # Aca un disfrazado SI es una ausencia: dentro del universo, 60 `NA` y 25
  # `s/d`.
  disfrazados <- data.frame(
    x = c(rep(NA, 60), rep("s/d", 25), rep("v", 15), rep("w", 100)),
    tipo = rep(c("A", "B"), each = 100), stringsAsFactors = FALSE
  )
  esperar_filas_de_ausencia(
    perfilar_ausencia(disfrazados, aplicabilidad = regla),
    "regla_silencia_ausencia", "x", 1:85
  )

  # Donde la regla no se pudo decidir la fila no esta en el universo.
  indeterminada <- disfrazados
  indeterminada$tipo[c(3L, 70L, 150L)] <- NA
  esperar_filas_de_ausencia(
    perfilar_ausencia(indeterminada, aplicabilidad = regla),
    "regla_silencia_ausencia", "x", setdiff(1:85, c(3L, 70L))
  )
})

test_that("con clave y con tope la traza de ausencia se publica como cualquier otra", {
  datos <- tabla_por_niveles()
  datos$id <- sprintf("K%04d", seq_len(nrow(datos)))
  perfil <- perfilar_ausencia(datos, clave = "id")
  hallazgo <- hallazgo_de_ausencia(
    perfil, "posible_ausencia_estructural", "valor"
  )
  traza <- hallazgo$trazabilidad[[1L]]
  expect_identical(traza$localizador, "clave_declarada")
  expect_identical(traza$claves$id, datos$id[traza$indices_fila])

  # 1.500 ausencias explicadas con el tope por omision de 1.000.
  grande <- data.frame(
    tipo = rep(c("A", "B"), each = 1500L),
    valor = c(rep("dato", 1500L), rep(NA, 1500L)), stringsAsFactors = FALSE
  )
  hallazgo <- hallazgo_de_ausencia(
    perfilar_ausencia(grande), "posible_ausencia_estructural", "valor"
  )
  traza <- hallazgo$trazabilidad[[1L]]
  expect_identical(traza$estado, "truncada")
  expect_identical(traza$indices_fila, 1501:2500)
  expect_equal(traza$total, 1500)
  expect_equal(hallazgo$n_afectados, 1500)
})

test_that("con el determinante protegido se nombran las mismas filas y se callan sus niveles", {
  datos <- data.frame(
    sector = rep(c("salud", "obra"), each = 200L),
    matricula = c(rep("M-1", 200L), rep(NA, 200L)),
    stringsAsFactors = FALSE
  )
  perfilar_con <- function(proteger) {
    perfilar_ausencia(
      datos, proteger_datos_personales = proteger,
      columnas_personales = c(sector = "otro")
    )
  }
  abierto <- hallazgo_de_ausencia(
    perfilar_con(FALSE), "posible_ausencia_estructural", "matricula"
  )
  protegido <- hallazgo_de_ausencia(
    perfilar_con(TRUE), "posible_ausencia_estructural", "matricula"
  )
  # Como en los demas hallazgos de fila sobre una columna protegida: los
  # indices se publican, los valores no.
  expect_identical(abierto$trazabilidad[[1L]]$indices_fila, 201:400)
  expect_identical(
    protegido$trazabilidad[[1L]]$indices_fila,
    abierto$trazabilidad[[1L]]$indices_fila
  )
  expect_equal(protegido$trazabilidad[[1L]]$total, 200)
  texto_abierto <- paste(abierto$evidencia, abierto$sugerencia)
  texto_protegido <- paste(protegido$evidencia, protegido$sugerencia)
  expect_true(grepl("salud", texto_abierto, fixed = TRUE, useBytes = TRUE))
  expect_false(grepl("salud", texto_protegido, fixed = TRUE, useBytes = TRUE))
  expect_false(any(grepl(
    "salud",
    as.character(unlist(protegido$trazabilidad[[1L]], use.names = FALSE)),
    fixed = TRUE, useBytes = TRUE
  )))
})

# Un perfil como lo guardaba la version anterior: los dos informativos de
# ausencia con la traza vacia con la que nace un hallazgo.
ausencia_sin_filas <- function(perfil) {
  de_ausencia <- which(as.character(perfil$hallazgos$tipo_hallazgo) %in% c(
    "posible_ausencia_estructural", "regla_silencia_ausencia"
  ))
  for (i in de_ausencia) {
    perfil$hallazgos$trazabilidad[[i]] <- .trazabilidad_vacia()
  }
  perfil
}

test_that("el plan y la deriva no cambian porque el hallazgo traiga filas", {
  datos <- tabla_por_niveles()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  nuevo <- perfilar_ausencia(datos, fecha = fecha, nombre = "t")
  viejo <- ausencia_sin_filas(nuevo)
  expect_identical(
    planificar_limpieza(nuevo, datos = datos),
    planificar_limpieza(viejo, datos = datos)
  )
  expect_equal(nrow(comparar_perfiles(viejo, nuevo)), 0L)
  expect_equal(nrow(comparar_perfiles(nuevo, viejo)), 0L)
  # Y el perfil de la version anterior se publica sin avisos.
  expect_length(detalle_de_la_guarda_de_ausencia(hallazgos(viejo)), 0L)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_length(detalle_de_la_guarda_de_ausencia(suppressMessages(
    reportar(viejo, archivo = archivo, sobrescribir = TRUE, fecha = fecha)
  )), 0L)
})

test_that("la guarda revisa un informativo que cuenta filas", {
  datos <- tabla_por_niveles()
  perfil <- perfilar_ausencia(datos)
  guarda <- function(hallazgos, con_datos) {
    detalle_de_la_guarda_de_ausencia(if (con_datos) {
      .advertir_incoherencias_trazabilidad(hallazgos, datos, names(datos))
    } else {
      .advertir_incoherencias_trazabilidad(hallazgos)
    })
  }
  # Intacto, calla en los dos momentos.
  expect_length(guarda(perfil$hallazgos, con_datos = TRUE), 0L)
  expect_length(guarda(perfil$hallazgos, con_datos = FALSE), 0L)

  # Cuenta 200 filas y no nombra ninguna: al construir se acusa. Al publicar
  # no, porque asi lo guardaba la version anterior.
  sin_filas <- ausencia_sin_filas(perfil)$hallazgos
  expect_match(
    guarda(sin_filas, con_datos = TRUE),
    "posible_ausencia_estructural en valor: traza no disponible", fixed = TRUE
  )
  expect_length(guarda(sin_filas, con_datos = FALSE), 0L)

  # Una fila de menos en la traza: cuenta 200 y nombra 199. Eso no es la forma
  # de ninguna version, y se acusa siempre.
  i <- which(perfil$hallazgos$tipo_hallazgo == "posible_ausencia_estructural")
  corta <- perfil$hallazgos
  traza <- corta$trazabilidad[[i]]
  traza$indices_fila <- traza$indices_fila[-1L]
  traza$mostrados <- 199L
  traza$total <- 199
  corta$trazabilidad[[i]] <- traza
  expect_match(guarda(corta, con_datos = TRUE), "cuenta 200 y traza 199",
               fixed = TRUE)
  expect_match(guarda(corta, con_datos = FALSE), "cuenta 200 y traza 199",
               fixed = TRUE)

  # Lo mismo para el aviso de una regla que silencia la ausencia.
  opcional <- data.frame(
    x = c(rep(NA, 150), rep("v", 50)), stringsAsFactors = FALSE
  )
  avisado <- perfilar_ausencia(opcional, columnas_opcionales = "x")
  guarda_opcional <- function(hallazgos, con_datos) {
    detalle_de_la_guarda_de_ausencia(if (con_datos) {
      .advertir_incoherencias_trazabilidad(hallazgos, opcional, names(opcional))
    } else {
      .advertir_incoherencias_trazabilidad(hallazgos)
    })
  }
  expect_length(guarda_opcional(avisado$hallazgos, con_datos = TRUE), 0L)
  sin_filas <- ausencia_sin_filas(avisado)$hallazgos
  expect_match(
    guarda_opcional(sin_filas, con_datos = TRUE),
    "regla_silencia_ausencia en x: traza no disponible", fixed = TRUE
  )
  expect_length(guarda_opcional(sin_filas, con_datos = FALSE), 0L)
})

test_that("la guarda no acusa a los informativos que no cuentan filas", {
  # `posible_identificador` cuenta columnas; `casi_duplicados_vocabulario` sin
  # grupos y una regularidad aritmetica que se cumple siempre cuentan cero.
  # Ninguno tiene filas que deber, antes y ahora.
  datos <- data.frame(
    id = 1:60, a = 1:60, b = 61:120, stringsAsFactors = FALSE
  )
  datos$total <- datos$a + datos$b
  datos$ciudad <- rep(c("Salto", "Rocha", "Minas"), 20L)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  informativos <- perfil$hallazgos[
    as.character(perfil$hallazgos$severidad) == "ok", , drop = FALSE
  ]
  expect_true("posible_identificador" %in% informativos$tipo_hallazgo)
  expect_false(any(
    informativos$unidad_conteo %in% "fila" & informativos$n_afectados > 0
  ))
  expect_length(detalle_de_la_guarda_de_ausencia(
    .advertir_incoherencias_trazabilidad(perfil$hallazgos, datos, names(datos))
  ), 0L)
  expect_length(detalle_de_la_guarda_de_ausencia(hallazgos(perfil)), 0L)

  # Los dos conjuntos del paquete: sus informativos son de columna o cuentan
  # cero, y la guarda calla al construir y al publicar.
  for (nombre in c("datos_administrativos", "datos_operativos")) {
    tabla <- get(nombre)
    detalle <- detalle_de_la_guarda_de_ausencia(
      perfil <- perfilar(tabla, analizar_dependencias = FALSE)
    )
    expect_length(detalle, 0L)
    expect_length(detalle_de_la_guarda_de_ausencia(hallazgos(perfil)), 0L)
  }
})

test_that("por grupo y por DBI la traza de ausencia sale como la de los demas hallazgos", {
  # Por grupo: indices de la tabla original.
  datos <- data.frame(
    grupo = rep(c("norte", "sur"), 200L),
    opcional = "v", stringsAsFactors = FALSE
  )
  vacias <- which(datos$grupo == "sur")[1:150]
  datos$opcional[vacias] <- NA
  por_grupo <- perfilar_por(
    datos, por = "grupo", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, columnas_opcionales = "opcional"
  )
  fila <- which(por_grupo$tipo_hallazgo == "regla_silencia_ausencia")
  expect_length(fila, 1L)
  expect_identical(as.character(por_grupo$grupo[[fila]]), "sur")
  expect_identical(por_grupo$trazabilidad[[fila]]$indices_fila, vacias)
  expect_equal(por_grupo$n_afectados[[fila]], 150)

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  tabla <- data.frame(
    id = 1:600, valor = c(rep("v", 300), rep(NA, 300)),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", tabla)
  completa <- suppressMessages(perfilar_dbi(
    con, "t", columnas_opcionales = "valor"
  ))
  expect_length(
    detalle_de_la_guarda_de_ausencia(h <- hallazgos(completa)), 0L
  )
  i <- which(h$tipo_hallazgo == "regla_silencia_ausencia")
  expect_length(i, 1L)
  expect_identical(h$trazabilidad[[i]]$indices_fila, 301:600)
  expect_equal(h$n_afectados[[i]], 300)

  # Con la muestra elegida por el motor la traza se retiene y conserva el
  # total, y eso esta declarado: la guarda calla.
  muestra <- suppressMessages(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 300,
    columnas_opcionales = "valor"
  ))
  expect_length(
    detalle_de_la_guarda_de_ausencia(h <- hallazgos(muestra)), 0L
  )
  # La mitad de la tabla esta vacia: que una muestra de 300 baje del umbral
  # de 0,4 pide 30 vacias menos que las esperadas, casi cinco desvios.
  i <- which(h$tipo_hallazgo == "regla_silencia_ausencia")
  expect_length(i, 1L)
  expect_identical(h$trazabilidad[[i]]$estado, "no_disponible")
  expect_identical(h$trazabilidad[[i]]$alcance, "orden_muestra_no_estable")
  expect_equal(h$trazabilidad[[i]]$total, h$n_afectados[[i]])
})
