# Ronda 25-H: el historico y la deriva entre sesiones. Cada prueba rehace a mano
# la cifra que el paquete publica y falla contra el arbol anterior al arreglo.

.r25h_perfil <- function(nombre = "P", condicion = function(x) x > 0) {
  perfil_evaluacion(nombre, regla_evaluacion("R", condicion))
}

.r25h_evaluacion <- function(entidad, valores, id, fecha, perfil = .r25h_perfil(),
                             marco = NULL, atributo = "edad") {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), entidad, atributo)
  datos <- stats::setNames(data.frame(valores), atributo)
  modelo_corrida <- if (is.null(marco)) {
    modelo(instancia)
  } else {
    suppressWarnings(modelo(instancia, marco = marco))
  }
  medicion <- medir(
    modelo_corrida, datos, id_medicion = id,
    fecha = as.POSIXct(fecha, tz = "UTC")
  )
  evaluar(medicion, perfil)
}

.r25h_csv <- function(historico) {
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  utils::write.csv(historico, archivo, row.names = FALSE)
  utils::read.csv(archivo, stringsAsFactors = FALSE)
}

.r25h_pares <- function(deriva) {
  deriva <- as.data.frame(deriva)
  deriva <- deriva[deriva$aspecto == "resultado", , drop = FALSE]
  paste(deriva$id_medicion_anterior, deriva$id_medicion_actual)
}

test_that("H1: la deriva de un historico releido de un CSV separa las tablas", {
  # Perfil por corrida: tablaA ene 1, tablaB feb 0, tablaA mar 1. La serie de
  # tablaA es ene -> mar, delta 1 - 1 = 0; tablaB tiene una sola corrida.
  historico <- historico_calidad(
    .r25h_evaluacion("tablaA", c(1, 2, 3, 4), "A-ene", "2026-01-31"),
    .r25h_evaluacion("tablaB", c(NA, NA, NA, NA), "B-feb", "2026-02-28"),
    .r25h_evaluacion("tablaA", c(1, 2, 3, 4), "A-mar", "2026-03-31")
  )
  leido <- .r25h_csv(historico)
  objeto <- as.data.frame(detectar_deriva_calidad(historico))
  csv <- as.data.frame(detectar_deriva_calidad(leido))
  # Antes: el CSV publicaba A-ene -> B-feb, delta -1, severidad error.
  expect_identical(.r25h_pares(csv), "A-ene A-mar")
  expect_identical(.r25h_pares(csv), .r25h_pares(objeto))
  expect_equal(csv$delta, 1 - 1)
  expect_identical(csv$identidad_tabla, "tablaA")
})

test_that("H1: un cambio de marco sigue siendo no comparable despues del CSV", {
  # ene con marco: 1,2,3,4 -> 1. feb sin marco: 1,NA,NA,NA -> 0,25. El objeto no
  # publica la resta 0,25 - 1 = -0,75: declara el cambio de marco.
  historico <- historico_calidad(
    .r25h_evaluacion("t", c(1, 2, 3, 4), "ene", "2026-01-31", marco = marco_agesic()),
    .r25h_evaluacion("t", c(1, NA, NA, NA), "feb", "2026-02-28")
  )
  csv <- as.data.frame(detectar_deriva_calidad(.r25h_csv(historico)))
  expect_false(any(csv$aspecto == "resultado"))
  expect_true(all(is.na(csv$delta)))
  expect_identical(unique(csv$cambio), "no_comparable")
})

test_that("H1: sin configuracion en las filas la deriva no da veredicto y lo dice", {
  historico <- historico_calidad(
    .r25h_evaluacion("tablaA", c(1, 2, 3, 4), "A-ene", "2026-01-31"),
    .r25h_evaluacion("tablaB", c(NA, NA, NA, NA), "B-feb", "2026-02-28")
  )
  # Un CSV exportado sin las columnas de configuracion, como los anteriores.
  sin_configuracion <- as.data.frame(historico)
  sin_configuracion <- sin_configuracion[setdiff(names(sin_configuracion), c(
    "identidad_tabla", "configuracion_modelo", "configuracion_marco",
    "configuracion_tipos_resultado", "configuracion_aplicabilidad",
    "configuracion_perfil"
  ))]
  deriva <- detectar_deriva_calidad(.r25h_csv(sin_configuracion))
  # Antes: delta 0 - 1 = -1 y severidad error, restando tablaB de tablaA.
  expect_equal(nrow(deriva), 1L)
  expect_true(is.na(deriva$delta))
  expect_true(is.na(deriva$significativo))
  expect_true(is.na(deriva$severidad))
  expect_identical(deriva$cambio, "no_comparable")
  diagnosticos <- attr(deriva, "cobertura_diagnosticos")
  expect_true(any(grepl("configuraci", diagnosticos$motivo)))
})

test_that("H3: acumular sobre un historico releido de un CSV es idempotente", {
  medicion <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "edad")),
    data.frame(edad = c(1, NA, 3)), id_medicion = "ene",
    fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )
  evaluacion <- evaluar(medicion, .r25h_perfil())
  # Las tres formas del informe: solo evaluacion, solo medicion y las dos.
  for (caso in list(list(evaluacion), list(medicion), list(medicion, evaluacion))) {
    historico <- do.call(historico_calidad, caso)
    otra_vez <- acumular_historico(.r25h_csv(historico), caso[[length(caso)]])
    # Antes: "Difieren: id_medida (NA contra NA)".
    expect_equal(nrow(otra_vez), nrow(historico))
    expect_equal(
      as.data.frame(otra_vez), as.data.frame(historico),
      ignore_attr = TRUE
    )
  }
})

test_that("H2: una medida suprimida no publica su valor en la fila de otro perfil", {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "edad")
  medicion <- medir(
    modelo(instancia), data.frame(edad = c(20, NA, 30)), id_medicion = "ene",
    fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )
  # Medidas 1, 0, 1. "Pub" (x > 0,5) suprime la segunda; "Presente" (x > 0)
  # la evalua 0, que en una metrica booleana es el valor suprimido mismo.
  suprime <- evaluar(medicion, perfil_evaluacion(
    "Publicable", regla_evaluacion("Pub", function(x) x > 0.5, desenlace = "suprimir")
  ))
  otro <- evaluar(medicion, .r25h_perfil("Basico"))
  segunda <- "ene-NoNulo@t.edad-000002"
  caminos <- list(
    historico_calidad(medicion, suprime, otro, detalle = "completo"),
    historico_calidad(medicion, otro, suprime, detalle = "completo"),
    acumular_historico(historico_calidad(otro, detalle = "completo"), suprime)
  )
  for (historico in caminos) {
    filas <- historico[historico$id_medida %in% segunda, , drop = FALSE]
    # Antes: la fila Basico/Presente publicaba resultado 0, sin marca.
    expect_true(all(is.na(filas$resultado)))
    expect_true(all(grepl("[valor suprimido]", filas$objeto_medible, fixed = TRUE)))
    expect_true("Basico" %in% filas$perfil)
  }
  # Y el informe del historico solo, sin la evaluacion que suprime al lado.
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(caminos[[1L]], archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  filas_html <- regmatches(html, gregexpr("<tr>(?:(?!</tr>).)*?Basico(?:(?!</tr>).)*?000002.*?</tr>", html, perl = TRUE))[[1L]]
  expect_length(filas_html, 1L)
  expect_match(filas_html, "[valor suprimido]", fixed = TRUE)
})

.r25h_coleccion <- function(con) {
  datos <- list(
    a1 = data.frame(x = c(1, 2, 3, 4)), a2 = data.frame(x = c(1, 2, 3, 4)),
    a3 = data.frame(x = c(NA, NA, NA, NA))
  )
  for (n in names(datos)) DBI::dbWriteTable(con, n, datos[[n]])
  especie <- especializar(metricas_nucleo()$NoNulo)
  modelo_a <- modelo(lapply(names(datos), function(t) instanciar(especie, t, "x")))
  coleccion_a <- coleccion(con, names(datos), nombre = "A")
  function(id, fecha, tablas) {
    medicion <- suppressWarnings(medir(
      modelo_a, datos, id_medicion = id, fecha = as.POSIXct(fecha, tz = "UTC")
    ))
    entidades <- agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")
    agregar(
      entidades[entidades$entidad %in% tablas, ], "coleccion", "promedio_ponderado",
      pesos = stats::setNames(rep(1 / length(tablas), length(tablas)), tablas),
      coleccion = coleccion_a
    )
  }
}

test_that("H5: una parte que entra o sale de la frontera no es mejora ni deterioro", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  corrida <- .r25h_coleccion(con)
  perfil <- .r25h_perfil(condicion = function(x) x > 0.8)
  # ene: (1 + 1 + 0) / 3 = 0,667 -> 0. feb, sin a3: (1 + 1) / 2 = 1 -> 1. mar,
  # con a3 otra vez -> 0. Los datos de cada tabla son los mismos en las tres.
  ene <- evaluar(corrida("ene", "2026-01-31", c("a1", "a2", "a3")), perfil)
  feb <- evaluar(corrida("feb", "2026-02-28", c("a1", "a2")), perfil)
  mar <- evaluar(corrida("mar", "2026-03-31", c("a1", "a2", "a3")), perfil)
  expect_equal(c(ene$perfiles$resultado, feb$perfiles$resultado), c(0, 1))
  deriva <- as.data.frame(detectar_deriva_calidad(historico_calidad(ene, feb, mar)))
  # Antes: ene -> feb delta +1 "mejora" y feb -> mar delta -1 "error".
  expect_false(any(deriva$aspecto == "resultado"))
  expect_true(all(is.na(deriva$delta)))
  expect_identical(deriva$aspecto, rep("cobertura_frontera", 2L))
  expect_identical(deriva$cambio, rep("no_comparable", 2L))
  expect_match(deriva$evidencia[[1L]], "a3", fixed = TRUE)
  # Control: la misma frontera incompleta en las dos corridas SI compara.
  abr <- evaluar(corrida("abr", "2026-04-30", c("a1", "a2")), perfil)
  igual <- as.data.frame(detectar_deriva_calidad(historico_calidad(feb, abr)))
  expect_identical(igual$aspecto, "resultado")
  expect_equal(igual$delta, 1 - 1)
})

test_that("H8: rbind de dos corridas conserva la parte no medida de cada una", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  corrida <- .r25h_coleccion(con)
  ene <- corrida("ene", "2026-01-31", c("a1", "a2"))
  feb <- corrida("feb", "2026-02-28", c("a1", "a2"))
  mar <- corrida("mar", "2026-03-31", c("a1", "a2", "a3"))
  partes <- function(historico) {
    filas <- historico[historico$nivel == "parte_no_medida", , drop = FALSE]
    sort(paste(filas$id_medicion, filas$entidad))
  }
  # A ene y a feb les falta a3; a mar, nada: dos partes, una por corrida.
  # Antes: `historico_calidad(rbind(ene, feb))` no guardaba ninguna.
  expect_identical(partes(historico_calidad(rbind(ene, feb))), c("ene a3", "feb a3"))
  expect_identical(
    partes(historico_calidad(rbind(ene, feb))), partes(historico_calidad(ene, feb))
  )
  # Coberturas distintas: cada parte va a su corrida y no a la otra.
  expect_identical(partes(historico_calidad(rbind(ene, mar))), "ene a3")
  perfil <- .r25h_perfil()
  expect_identical(
    partes(historico_calidad(evaluar(rbind(ene, mar), perfil))), "ene a3"
  )
})

test_that("H4: comparar_evaluaciones no publica el delta que la deriva no sostiene", {
  completa <- c(1, 2, 3, 4)
  cuarto <- c(1, NA, NA, NA)
  # Marco en ene y no en feb: la resta 0,25 - 1 = -0,75 no se publica.
  marco <- comparar_evaluaciones(
    .r25h_evaluacion("t", completa, "ene", "2026-01-31", marco = marco_agesic()),
    .r25h_evaluacion("t", cuarto, "feb", "2026-02-28")
  )
  expect_true(is.na(marco$delta))
  expect_match(marco$comparacion, "marco", fixed = TRUE)
  # Dos tablas distintas con el mismo perfil: tampoco.
  tablas <- comparar_evaluaciones(
    .r25h_evaluacion("tablaA", completa, "A", "2026-01-31"),
    .r25h_evaluacion("tablaB", cuarto, "B", "2026-02-28")
  )
  expect_true(is.na(tablas$delta))
  expect_match(tablas$comparacion, "tablaA contra tablaB", fixed = TRUE)
  # Control: la misma tabla y el mismo modelo si comparan, y con el delta de
  # la deriva: 0,25 - 1 = -0,75.
  ene <- .r25h_evaluacion("t", completa, "ene", "2026-01-31")
  feb <- .r25h_evaluacion("t", cuarto, "feb", "2026-02-28")
  directo <- comparar_evaluaciones(ene, feb)
  expect_equal(directo$delta, 0.25 - 1)
  expect_equal(
    directo$delta, detectar_deriva_calidad(historico_calidad(ene, feb))$delta
  )
  # Con la misma fecha la deriva ordena por `id_medicion` -"a" antes que "z"-
  # y aca manda el orden de los argumentos: z (1) -> a (0,25) = -0,75.
  z <- .r25h_evaluacion("t", completa, "z", "2026-01-31")
  a <- .r25h_evaluacion("t", cuarto, "a", "2026-01-31")
  expect_equal(comparar_evaluaciones(z, a)$delta, 0.25 - 1)
})

test_that("H6: una fecha de texto con desplazamiento se lee con el, y entera", {
  trece <- as.numeric(as.POSIXct("2026-01-31 13:00:00", tz = "UTC"))
  # 10:00 en -03:00 son las 13:00 UTC, se escriba como se escriba.
  for (texto in c("2026-01-31 10:00:00-03:00", "2026-01-31T10:00:00-0300",
                  "2026-01-31 10:00:00 -03", "2026-01-31T13:00:00Z",
                  "2026-01-31 13:00:00 UTC", "2026-01-31 14:00:00+01:00")) {
    expect_equal(as.numeric(.fecha_utc(texto)), trece, info = texto)
  }
  expect_equal(
    as.numeric(.fecha_utc("2026-01-31")),
    as.numeric(as.POSIXct("2026-01-31", tz = "UTC"))
  )
  # Lo que no se lee entero no se lee a medias.
  expect_error(.fecha_utc("2026-01-31 10:00:00 basura"), "No se pudo leer")
  expect_error(.fecha_utc("2026-01-31 25:00:00"), "No se pudo leer")
  # De punta a punta, el fin del horario de verano europeo: A a las 02:30+0200
  # (00:30 UTC, perfil 1) y B a las 02:10+0100 (01:10 UTC, perfil 0,25). A va
  # antes que B: 0,25 - 1 = -0,75. Leida sin el desplazamiento, B iba antes.
  historico <- historico_calidad(
    .r25h_evaluacion("t", c(1, 2, 3, 4), "A", "2026-10-25 00:30:00"),
    .r25h_evaluacion("t", c(1, NA, NA, NA), "B", "2026-10-25 01:10:00")
  )
  leido <- .r25h_csv(historico)
  leido$fecha <- ifelse(
    leido$id_medicion == "A", "2026-10-25 02:30:00+0200", "2026-10-25 02:10:00+0100"
  )
  deriva <- detectar_deriva_calidad(leido)
  expect_identical(.r25h_pares(deriva), "A B")
  expect_equal(deriva$delta, 0.25 - 1)
})

test_that("H7: un cambio de modelo que mantiene la comparacion no es no_comparable", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  datos <- data.frame(edad = c(1, 2, 3, 4), nombre = c("a", NA, NA, NA))
  corrida <- function(atributo, id, fecha) {
    evaluar(medir(
      modelo(instanciar(especie, "t", atributo)), datos, id_medicion = id,
      fecha = as.POSIXct(fecha, tz = "UTC")
    ), .r25h_perfil())
  }
  # uno: NoNulo de edad -> 1; tres: NoNulo de nombre -> 0,25. Mismo tipo y sin
  # marco: la deriva mantiene la comparacion, 0,25 - 1 = -0,75.
  historico <- historico_calidad(
    corrida("edad", "uno", "2026-01-31"), corrida("nombre", "tres", "2026-03-31")
  )
  deriva <- as.data.frame(detectar_deriva_calidad(historico))
  resultado <- deriva[deriva$aspecto == "resultado", ]
  modelo_cambiado <- deriva[deriva$aspecto == "configuracion_modelo", ]
  expect_equal(resultado$delta, 0.25 - 1)
  # Antes: la fila del modelo decia `no_comparable` sobre el par que comparaba.
  expect_true(is.na(modelo_cambiado$cambio))
  expect_false(any(deriva$cambio %in% "no_comparable"))
  # Y el informe publica el mismo delta, con la nota del modelo al lado.
  evolucion <- lupa:::.evolucion_historico(historico)
  tres <- evolucion[evolucion$id_medicion == "tres", ]
  expect_equal(tres$delta, 0.25 - 1)
  expect_match(tres$comparacion, "Cambio el modelo", fixed = TRUE)
  # Control: con cambio de marco sigue sin delta y `no_comparable`.
  con_marco <- historico_calidad(
    .r25h_evaluacion("t", c(1, 2, 3, 4), "ene", "2026-01-31", marco = marco_agesic()),
    .r25h_evaluacion("t", c(1, NA, NA, NA), "feb", "2026-02-28")
  )
  deriva_marco <- as.data.frame(detectar_deriva_calidad(con_marco))
  expect_identical(deriva_marco$cambio, "no_comparable")
  expect_true(is.na(lupa:::.evolucion_historico(con_marco)$delta[[2L]]))
})
