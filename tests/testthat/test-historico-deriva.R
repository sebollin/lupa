.crear_corrida_historica <- function(id, fecha, valores) {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
    "tabla", "dato"
  )
  medidas <- medir(
    modelo(instancia), data.frame(dato = valores),
    id_medicion = id, fecha = as.POSIXct(fecha, tz = "UTC")
  )
  perfil <- perfil_evaluacion(
    "Operativo",
    regla_evaluacion("Dato presente", function(x) x == 1)
  )
  list(medidas = medidas, evaluacion = evaluar(medidas, perfil))
}

test_that("el historico es una tabla plana y versionada", {
  enero <- .crear_corrida_historica(
    "enero", "2026-01-31 23:30:00", c(1, NA)
  )
  febrero <- .crear_corrida_historica(
    "febrero", "2026-02-28 23:30:00", c(1, 2)
  )

  resumen <- historico_calidad(list(enero$evaluacion, febrero$evaluacion))
  expect_s3_class(resumen, "historico_calidad")
  expect_s3_class(resumen, "data.frame")
  expect_equal(nrow(resumen), 4L)
  expect_equal(
    unique(resumen$nivel), c("evaluacion_regla", "evaluacion_perfil")
  )
  expect_true(all(resumen$version_esquema == 1L))
  expect_equal(attr(resumen, "version_esquema"), 1L)
  expect_false(any(vapply(resumen, is.list, logical(1L))))
  expect_equal(attr(resumen$fecha, "tzone"), "UTC")

  completo <- historico_calidad(enero$evaluacion, detalle = "completo")
  conteos <- table(completo$nivel)
  expect_equal(names(conteos), c(
    "evaluacion_medida", "evaluacion_perfil", "evaluacion_regla"
  ))
  expect_equal(as.integer(conteos), c(2L, 1L, 1L))
  medidas <- historico_calidad(enero$medidas)
  expect_equal(nrow(medidas), 2L)
  expect_true(all(medidas$nivel == "medida"))
  expect_equal(medidas$metrica, rep("NoNulo", 2L))

  combinado <- historico_calidad(enero$medidas, enero$evaluacion)
  expect_equal(nrow(combinado), 4L)
  expect_equal(
    sort(unique(combinado$nivel)),
    c("evaluacion_perfil", "evaluacion_regla", "medida")
  )
})

test_that("acumular es incremental, idempotente y detecta conflictos", {
  enero <- .crear_corrida_historica("enero", "2026-01-31", c(1, NA))
  febrero <- .crear_corrida_historica("febrero", "2026-02-28", c(1, 2))
  historico <- historico_calidad(enero$evaluacion)
  ampliado <- acumular_historico(historico, febrero$evaluacion)
  expect_equal(nrow(ampliado), 4L)
  expect_equal(nrow(acumular_historico(ampliado, febrero$evaluacion)), 4L)
  expect_equal(
    nrow(acumular_historico(historico, list(febrero$evaluacion))), 4L
  )
  expect_equal(nrow(acumular_historico(ampliado)), 4L)
  expect_equal(nrow(historico_calidad(ampliado)), 4L)

  conflicto <- febrero$evaluacion
  conflicto$perfiles$resultado <- 0.25
  expect_error(
    acumular_historico(ampliado, conflicto), "contenido diferente"
  )
  expect_error(historico_calidad(1), "Cada objeto")
  expect_error(
    lupa:::.normalizar_medicion_historico(data.frame()), "medir"
  )
  expect_error(
    lupa:::.normalizar_evaluacion_historico(data.frame(), "resumen"),
    "evaluar"
  )

  evaluacion_invalida <- febrero$evaluacion
  evaluacion_invalida$reglas$resultado <- NA_real_
  expect_error(historico_calidad(evaluacion_invalida), "no cumple")

  duplicado <- historico_calidad(febrero$evaluacion)
  duplicado$id_registro[[2L]] <- duplicado$id_registro[[1L]]
  expect_error(
    lupa:::.combinar_historico(historico_calidad(), duplicado),
    "duplicados"
  )

  fecha_inconsistente <- ampliado
  fecha_inconsistente$id_medicion[[2L]] <- fecha_inconsistente$id_medicion[[1L]]
  fecha_inconsistente$fecha[[2L]] <- fecha_inconsistente$fecha[[1L]] + 3600
  expect_error(
    acumular_historico(fecha_inconsistente), "id_medicion"
  )

  nivel_invalido <- ampliado
  nivel_invalido$nivel[[1L]] <- "otro"
  expect_error(acumular_historico(nivel_invalido), "niveles")
})

test_that("el historico se guarda y recupera sin estructuras anidadas", {
  corrida <- .crear_corrida_historica("enero", "2026-01-31", c(1, NA))
  historico <- historico_calidad(corrida$evaluacion)
  archivo <- tempfile(fileext = ".rds")
  ruta <- guardar_historico(historico, archivo)
  recuperado <- leer_historico(archivo)

  expect_true(file.exists(ruta))
  expect_s3_class(recuperado, "historico_calidad")
  expect_equal(recuperado, historico)
  expect_error(guardar_historico(historico, archivo), "ya existe")
  expect_silent(guardar_historico(historico, archivo, sobrescribir = TRUE))
  expect_error(
    guardar_historico(historico, archivo, sobrescribir = NA), "sobrescribir"
  )
  expect_error(guardar_historico(historico, ""), "archivo")
  expect_error(
    guardar_historico(historico, file.path(tempfile(), "x.rds")),
    "No existe"
  )
  expect_error(leer_historico(tempfile()), "RDS existente")

  futuro <- historico
  futuro$version_esquema <- 2L
  attr(futuro, "version_esquema") <- 2L
  archivo_futuro <- tempfile(fileext = ".rds")
  saveRDS(futuro, archivo_futuro)
  expect_error(leer_historico(archivo_futuro), "no compatible")

  invalido <- historico
  invalido$resultado[[1L]] <- 2
  expect_error(guardar_historico(invalido, tempfile()), "resultados")
  expect_error(guardar_historico(data.frame(), tempfile()), "esquema")
})

test_that("la deriva de calidad ordena N corridas y aplica el umbral", {
  r1 <- .crear_corrida_historica(
    "r1", "2026-01-01", c(rep(1, 80), rep(NA, 20))
  )
  r2 <- .crear_corrida_historica(
    "r2", "2026-02-01", c(rep(1, 77), rep(NA, 23))
  )
  r3 <- .crear_corrida_historica(
    "r3", "2026-03-01", c(rep(1, 70), rep(NA, 30))
  )
  r4 <- .crear_corrida_historica(
    "r4", "2026-04-01", c(rep(1, 55), rep(NA, 45))
  )
  historico <- historico_calidad(
    r3$evaluacion, r1$evaluacion, r4$evaluacion, r2$evaluacion
  )
  deriva <- detectar_deriva_calidad(historico, umbral = 0.05)

  expect_s3_class(deriva, "deriva_calidad")
  expect_equal(deriva$id_medicion_anterior, c("r1", "r2", "r3"))
  expect_equal(deriva$id_medicion_actual, c("r2", "r3", "r4"))
  expect_equal(deriva$delta, c(-0.03, -0.07, -0.15), tolerance = 1e-12)
  expect_equal(deriva$significativo, c(FALSE, TRUE, TRUE))
  expect_equal(deriva$direccion, c("estable", "deterioro", "deterioro"))
  expect_equal(as.character(deriva$severidad), c("ok", "sospechoso", "error"))
  expect_true(is.ordered(deriva$severidad))

  por_regla <- detectar_deriva_calidad(historico, nivel = "regla")
  expect_equal(por_regla$regla, rep("Dato presente", 3L))
  expect_equal(por_regla$delta, deriva$delta)

  una <- historico_calidad(r1$evaluacion)
  expect_equal(nrow(detectar_deriva_calidad(una)), 0L)
  expect_error(detectar_deriva_calidad(historico, umbral = 0), "umbral")
  expect_error(
    detectar_deriva_calidad(historico_calidad(r1$medidas)),
    "no contiene evaluaciones"
  )
})

test_that("comparar_perfiles cubre esquema, tipos, métricas y patrones", {
  n <- 10L
  anterior <- perfilar(
    data.frame(
      tipo = seq_len(n),
      faltantes = seq_len(n),
      card = rep(c("A", "B"), n / 2L),
      rango = seq_len(n),
      codigo = rep("AA1", n),
      constante = rep("X", n),
      vieja = seq_len(n),
      stringsAsFactors = FALSE
    ),
    fecha = as.POSIXct("2026-01-31 23:00:00", tz = "America/Montevideo")
  )
  actual <- perfilar(
    data.frame(
      tipo = letters[seq_len(n)],
      faltantes = c(seq_len(7L), NA, NA, NA),
      card = LETTERS[seq_len(n)],
      rango = c(seq_len(9L), 100L),
      codigo = c(rep("AA1", 9L), "B-3"),
      constante = rep(c("X", "Y"), n / 2L),
      nueva = seq_len(n),
      stringsAsFactors = FALSE
    ),
    fecha = as.POSIXct("2026-02-28 23:00:00", tz = "America/Montevideo")
  )
  deriva <- comparar_perfiles(anterior, actual)

  expect_s3_class(deriva, "deriva_perfil")
  expect_true(is.ordered(deriva$severidad))
  expect_true(any(
    deriva$aspecto == "columna" & deriva$cambio == "desaparecida" &
      deriva$columna == "vieja" & deriva$severidad == "error"
  ))
  expect_true(any(
    deriva$aspecto == "columna" & deriva$cambio == "aparecida" &
      deriva$columna == "nueva" & deriva$severidad == "error"
  ))
  expect_true(any(
    deriva$columna == "tipo" & deriva$aspecto == "tipo_declarado" &
      deriva$severidad == "error"
  ))
  expect_true(any(
    deriva$columna == "faltantes" & deriva$aspecto == "faltantes" &
      deriva$delta == 0.3 & deriva$severidad == "error"
  ))
  expect_true(any(
    deriva$columna == "card" & deriva$aspecto == "cardinalidad" &
      deriva$significativo
  ))
  expect_true(any(
    deriva$columna == "rango" & deriva$aspecto == "rango" &
      deriva$severidad == "sospechoso"
  ))
  expect_true(any(
    deriva$columna == "codigo" & deriva$aspecto == "patron" &
      deriva$cambio == "aparecido"
  ))
  expect_true(any(
    deriva$columna == "constante" & deriva$aspecto == "hallazgo" &
      deriva$cambio == "resuelto" & deriva$severidad == "ok"
  ))
  expect_equal(attr(deriva$fecha_anterior, "tzone"), "UTC")
  expect_false(any(
    deriva$columna == "vieja" & deriva$aspecto == "hallazgo" &
      deriva$cambio == "resuelto", na.rm = TRUE
  ))
})

test_that("los patrones desaparecidos y configuraciones distintas se informan", {
  anterior <- perfilar(
    data.frame(codigo = rep("AA1", 10L)),
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  actual <- perfilar(
    data.frame(codigo = rep("B-3", 10L)),
    fecha = as.POSIXct("2026-02-01", tz = "UTC")
  )
  deriva <- comparar_perfiles(anterior, actual)
  expect_true(any(
    deriva$aspecto == "patron" & deriva$cambio == "desaparecido" &
      deriva$severidad == "error"
  ))

  otra_config <- perfilar(
    data.frame(codigo = rep("B-3", 10L)), expandir = TRUE,
    fecha = as.POSIXct("2026-03-01", tz = "UTC")
  )
  no_comparable <- comparar_perfiles(actual, otra_config)
  expect_true(any(
    no_comparable$aspecto == "configuracion_patrones" &
      no_comparable$cambio == "no_comparable" &
      no_comparable$severidad == "error"
  ))
  expect_false(any(no_comparable$aspecto == "patron"))
})

test_that("los cambios pequeños permanecen como observaciones ok", {
  anterior <- perfilar(
    data.frame(categoria = rep(c("A", "B"), 50L)),
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  actual <- anterior
  actual$meta$fecha_hora <- as.POSIXct("2026-02-01", tz = "UTC")
  actual$columnas$tasa_distintos <- anterior$columnas$tasa_distintos + 0.02
  actual$columnas$n_distintos <- anterior$columnas$n_distintos + 2L
  deriva <- comparar_perfiles(anterior, actual)

  cardinalidad <- deriva[deriva$aspecto == "cardinalidad", , drop = FALSE]
  expect_equal(nrow(cardinalidad), 1L)
  expect_false(cardinalidad$significativo)
  expect_equal(as.character(cardinalidad$severidad), "ok")
  expect_equal(cardinalidad$delta, 0.02)

  sin_cambios <- comparar_perfiles(anterior, anterior)
  expect_s3_class(sin_cambios, "deriva_perfil")
  expect_equal(nrow(sin_cambios), 0L)
})

test_that("la deriva tolera rangos de fecha no parseables", {
  perfil <- perfilar(
    datos_administrativos,
    fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  expect_equal(nrow(comparar_perfiles(perfil, perfil)), 0L)

  corrupto <- perfil
  indice <- which(!is.na(corrupto$columnas$minimo_fecha))[[1L]]
  corrupto$columnas$minimo_fecha[[indice]] <- "4620236-06-30"
  corrupto$columnas$maximo_fecha[[indice]] <- "4660214-04-27"
  expect_no_error(comparar_perfiles(corrupto, corrupto))
})

test_that("las entradas de deriva se validan", {
  perfil <- perfilar(
    data.frame(x = 1:3), fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  expect_error(comparar_perfiles(data.frame(), perfil), "anterior")
  expect_error(comparar_perfiles(perfil, data.frame()), "actual")
  expect_error(comparar_perfiles(perfil, perfil, umbral_cambio = -1), "umbrales")
  expect_error(
    comparar_perfiles(perfil, perfil, umbral_cambio = 0.2, umbral_error = 0.1),
    "umbral_error"
  )
  expect_error(perfilar(data.frame(x = 1), fecha = NA), "fecha")

  fecha_date <- perfilar(data.frame(x = 1), fecha = as.Date("2026-01-01"))
  expect_s3_class(fecha_date$meta$fecha_hora, "POSIXct")
  expect_equal(attr(fecha_date$meta$fecha_hora, "tzone"), "UTC")
})

test_that("las ramas de comparabilidad estructural quedan explícitas", {
  expect_true(lupa:::.distinto_deriva(1:2, 1))
  expect_false(lupa:::.distinto_deriva(NA_real_, NA_real_))
  expect_true(lupa:::.distinto_deriva(NA_real_, 1))

  base <- perfilar(
    data.frame(x = c(1, 2, 3)),
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  sin_config <- base
  sin_config$meta$muestra <- NULL
  deriva_config <- comparar_perfiles(sin_config, base)
  expect_true(any(deriva_config$aspecto == "configuracion_patrones"))

  fechas_a <- perfilar(
    data.frame(f = as.Date(c("2026-01-01", "2026-01-10"))),
    fecha = as.POSIXct("2026-01-31", tz = "UTC")
  )
  fechas_b <- perfilar(
    data.frame(f = as.Date(c("2026-01-01", "2026-01-20"))),
    fecha = as.POSIXct("2026-02-28", tz = "UTC")
  )
  expect_true(any(comparar_perfiles(fechas_a, fechas_b)$aspecto == "rango"))

  constante_a <- perfilar(
    data.frame(x = c(1, 1)), fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  constante_b <- perfilar(
    data.frame(x = c(2, 2)), fecha = as.POSIXct("2026-02-01", tz = "UTC")
  )
  rango <- comparar_perfiles(constante_a, constante_b)
  expect_true(any(is.finite(rango$cambio_relativo[rango$aspecto == "rango"])))

  base_texto <- perfilar(
    data.frame(x = c("AA1", "AA2", "AA3")),
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  patron_invalido <- base_texto
  patron_invalido$patrones[[1L]] <- data.frame(otra = 1)
  deriva_patron <- comparar_perfiles(patron_invalido, base_texto)
  expect_true(any(deriva_patron$aspecto == "patron"))

  sin_hallazgos_a <- base
  sin_hallazgos_b <- base
  sin_hallazgos_a$hallazgos <- sin_hallazgos_a$hallazgos[0, , drop = FALSE]
  sin_hallazgos_b$hallazgos <- sin_hallazgos_b$hallazgos[0, , drop = FALSE]
  expect_equal(nrow(comparar_perfiles(sin_hallazgos_a, sin_hallazgos_b)), 0L)

  con_retirada <- perfilar(
    data.frame(retirada = rep("x", 3L), comun = 1:3),
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  sin_retirada <- perfilar(
    data.frame(comun = 1:3),
    fecha = as.POSIXct("2026-02-01", tz = "UTC")
  )
  cambios <- comparar_perfiles(con_retirada, sin_retirada)
  expect_false(any(
    cambios$columna == "retirada" & cambios$cambio == "resuelto",
    na.rm = TRUE
  ))

  severidad_a <- constante_a
  severidad_b <- constante_a
  severidad_b$meta$fecha_hora <- as.POSIXct("2026-02-01", tz = "UTC")
  severidad_b$hallazgos$severidad <- factor(
    "error", levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  cambio_severidad <- comparar_perfiles(severidad_a, severidad_b)
  expect_true(any(
    cambio_severidad$aspecto == "severidad_hallazgo" &
      cambio_severidad$cambio == "agravado"
  ))
})
