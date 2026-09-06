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

test_that("las claves historicas codifican cada elemento del vector", {
  entradas <- c("Basico", "Presente", "otro", NA_character_)
  claves <- lupa:::.escapar_clave(entradas)
  expect_length(claves, length(entradas))
  expect_equal(length(unique(claves[seq_len(3L)])), 3L)
  expect_equal(claves[[4L]], "~")
  ids <- lupa:::.clave_historico(entradas, entradas)
  expect_length(ids, length(entradas))
  expect_equal(length(unique(ids[seq_len(3L)])), 3L)
})

test_that("el texto UTF-8 ilegible queda marcado como ausente en la clave", {
  invalido <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  valores <- lupa:::.texto_analizable(invalido)$valores
  expect_true(is.na(valores))
  expect_identical(lupa:::.escapar_clave(valores), "~")
})

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

test_that("el cambio de centinelas se declara sin apagar la deriva", {
  datos <- data.frame(x = c(rep(1, 90), rep(2, 10), rep(9999, 10)))
  anterior <- perfilar(
    datos, sentinelas_numericos = numeric(), analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  actual <- perfilar(
    datos, sentinelas_numericos = 9999, analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-02-01", tz = "UTC")
  )

  deriva <- comparar_perfiles(anterior, actual)
  politica <- deriva[deriva$aspecto == "configuracion_sentinelas_numericos", ,
                     drop = FALSE]
  expect_equal(nrow(politica), 1L)
  expect_equal(politica$cambio, "modificado")
  expect_equal(as.character(politica$severidad), "error")
  expect_match(politica$descripcion, "se mantienen las comparaciones")
  expect_true(any(deriva$aspecto == "faltantes"))
  expect_true(any(deriva$aspecto == "rango"))
  expect_true(any(deriva$aspecto == "hallazgo"))

  posterior <- perfilar(
    data.frame(x = c(rep(1, 80), rep(3, 20), rep(9999, 10))),
    sentinelas_numericos = 9999, analizar_dependencias = FALSE,
    fecha = as.POSIXct("2026-03-01", tz = "UTC")
  )
  deriva_estable <- comparar_perfiles(actual, posterior)
  expect_false(any(
    deriva_estable$aspecto == "configuracion_sentinelas_numericos"
  ))
  expect_true(any(deriva_estable$aspecto == "rango"))
})

test_that("la deriva de calidad declara cambios del modelo y conserva la senal", {
  nucleo <- metricas_nucleo()
  no_nulo <- instanciar(especializar(nucleo$NoNulo), "tabla", "dato")
  regla_corte <- function(umbral) regla_evaluacion(
    "corte", function(x, minimo) x >= minimo,
    umbrales = list(minimo = umbral)
  )
  datos_corte <- data.frame(dato = c("OK", "OK", "OK", NA_character_))
  anterior <- evaluar(
    agregar(
      medir(modelo(no_nulo), datos_corte, id_medicion = "corte-a",
            fecha = as.POSIXct("2026-01-01", tz = "UTC")),
      "atributo", "ratio"
    ),
    perfil_evaluacion("P", regla_corte(0.5))
  )
  actual <- evaluar(
    agregar(
      medir(modelo(no_nulo), datos_corte, id_medicion = "corte-b",
            fecha = as.POSIXct("2026-02-01", tz = "UTC")),
      "atributo", "ratio"
    ),
    perfil_evaluacion("P", regla_corte(0.9))
  )
  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  resultado <- deriva[deriva$aspecto == "resultado", , drop = FALSE]
  expect_equal(resultado$delta, -1)
  expect_equal(as.character(resultado$severidad), "error")
  expect_true(any(deriva$aspecto == "configuracion_perfil" &
                    deriva$severidad == "error"))
  expect_match(
    deriva$descripcion[deriva$aspecto == "configuracion_perfil"],
    "se mantienen las comparaciones"
  )

  formato <- instanciar(
    especializar(nucleo$Formato, expresion_regular = "^OK$"),
    "tabla", "dato"
  )
  anterior <- evaluar(
    medir(modelo(no_nulo), data.frame(dato = c("OK", "BAD")),
          id_medicion = "metrica-a", fecha = as.POSIXct("2026-01-01", tz = "UTC")),
    perfil_evaluacion("P", regla_evaluacion("pasa", function(x) x == 1))
  )
  actual <- evaluar(
    medir(modelo(formato), data.frame(dato = c("OK", "BAD")),
          id_medicion = "metrica-b", fecha = as.POSIXct("2026-02-01", tz = "UTC")),
    perfil_evaluacion("P", regla_evaluacion("pasa", function(x) x == 1))
  )
  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  expect_equal(deriva$delta[deriva$aspecto == "resultado"], -0.5)
  expect_true(any(deriva$aspecto == "configuracion_modelo" &
                    deriva$severidad == "error"))

  datos_aplicabilidad <- data.frame(
    tiene = c("Si", "Si", "No", "No"), dato = c("OK", "OK", NA, NA)
  )
  anterior <- evaluar(
    medir(modelo(no_nulo), datos_aplicabilidad, id_medicion = "app-a",
          fecha = as.POSIXct("2026-01-01", tz = "UTC")),
    perfil_evaluacion("P", regla_evaluacion("pasa", function(x) x == 1))
  )
  actual <- evaluar(
    medir(modelo(no_nulo), datos_aplicabilidad,
          aplicabilidad = list(dato = ~ tiene == "Si"), id_medicion = "app-b",
          fecha = as.POSIXct("2026-02-01", tz = "UTC")),
    perfil_evaluacion("P", regla_evaluacion("pasa", function(x) x == 1))
  )
  deriva <- detectar_deriva_calidad(historico_calidad(anterior, actual))
  resultado <- deriva[deriva$aspecto == "resultado", , drop = FALSE]
  expect_equal(resultado$delta, 0.5)
  expect_equal(resultado$direccion, "mejora")
  expect_true(any(deriva$aspecto == "configuracion_aplicabilidad" &
                    deriva$severidad == "error"))
})

test_that("el historico separa corridas de tablas distintas", {
  nucleo <- metricas_nucleo()
  regla <- perfil_evaluacion("P", regla_evaluacion("pasa", function(x) x == 1))
  corrida <- function(entidad, id, valor, fecha) {
    metrica <- instanciar(especializar(nucleo$NoNulo), entidad, "dato")
    evaluar(
      medir(modelo(metrica), data.frame(dato = valor), id_medicion = id,
            fecha = as.POSIXct(fecha, tz = "UTC")), regla
    )
  }
  historico <- historico_calidad(
    corrida("clientes", "clientes-1", 1, "2026-01-01"),
    corrida("ventas", "ventas-1", NA, "2026-02-01")
  )
  expect_equal(
    attr(historico, "configuracion_evaluacion")$identidad_tabla,
    c("clientes", "ventas")
  )
  expect_equal(nrow(detectar_deriva_calidad(historico)), 0L)
  expect_equal(nrow(detectar_deriva_calidad(historico, nivel = "regla")), 0L)

  mismo <- historico_calidad(
    corrida("clientes", "clientes-1", 1, "2026-01-01"),
    corrida("clientes", "clientes-2", NA, "2026-02-01")
  )
  deriva <- detectar_deriva_calidad(mismo)
  expect_equal(nrow(deriva), 1L)
  expect_equal(deriva$identidad_tabla, "clientes")
  expect_equal(deriva$direccion, "deterioro")
  expect_false(any(grepl("configuracion", deriva$aspecto)))
})

# Un perfil guardado por otra version de lupa puede no traer un campo que esta
# version compara. Antes de arreglarlo, cuatro campos hacian abortar
# comparar_perfiles() con el mensaje interno de R, y dos publicaban una fila de
# cambio atribuida a los datos cuando lo que faltaba era el campo.
test_that("un perfil sin un campo declara la no comparabilidad y no aborta", {
  d1 <- data.frame(
    x = c(1, 2, 3, 4, 5), t = c("a", "b", "a", "c", "b"),
    stringsAsFactors = FALSE
  )
  d2 <- data.frame(
    x = c(1, 2, 3, 4, 9), t = c("a", "b", "z", "c", "b"),
    stringsAsFactors = FALSE
  )
  actual <- perfilar(d2)

  esperado <- c(
    tasa_distintos = "cardinalidad", n_distintos = "cardinalidad",
    prop_faltantes_totales = "faltantes", tipo_inferido = "tipo_inferido",
    minimo = "rango", maximo = "rango", minimo_fecha = "rango",
    maximo_fecha = "rango"
  )
  for (campo in names(esperado)) {
    viejo <- perfilar(d1)
    viejo$columnas <- viejo$columnas[
      , setdiff(names(viejo$columnas), campo), drop = FALSE
    ]
    comparacion <- expect_no_error(comparar_perfiles(viejo, actual))

    declaradas <- comparacion[comparacion$cambio == "no_comparable", ]
    expect_true(
      esperado[[campo]] %in% declaradas$aspecto,
      info = paste("sin", campo, "no se declaro", esperado[[campo]])
    )
    # Y no puede atribuir a los datos lo que es una diferencia de forma.
    expect_equal(
      sum(comparacion$cambio == "modificado" &
            comparacion$aspecto %in% declaradas$aspecto),
      0L,
      info = paste("sin", campo, "publico un cambio en un aspecto no comparable")
    )
  }

  # La fila declarada dice de que lado falta, que es lo que permite entender
  # que el problema es la version del perfil y no los datos.
  viejo <- perfilar(d1)
  viejo$columnas <- viejo$columnas[
    , setdiff(names(viejo$columnas), "tasa_distintos"), drop = FALSE
  ]
  fila <- comparar_perfiles(viejo, actual)
  fila <- fila[fila$aspecto == "cardinalidad" & fila$cambio == "no_comparable", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$valor_anterior, "tasa_distintos")
  expect_match(fila$evidencia, "perfil anterior")
})

# La mitad de control: dos perfiles de esta version no declaran nada y siguen
# detectando los cambios reales. Sin esto, una version que declarara todo
# siempre pasaria el test de arriba.
test_that("dos perfiles de esta version no declaran campos ausentes", {
  d1 <- data.frame(
    x = c(1, 2, 3, 4, 5), t = c("a", "b", "a", "c", "b"),
    stringsAsFactors = FALSE
  )
  d2 <- data.frame(
    x = c(1, 2, 3, 4, 9), t = c("a", "b", "z", "c", "b"),
    stringsAsFactors = FALSE
  )
  comparacion <- comparar_perfiles(perfilar(d1), perfilar(d2))

  aspectos <- c(
    "tipo_declarado", "tipo_inferido", "faltantes", "cardinalidad", "rango"
  )
  expect_equal(
    sum(comparacion$cambio == "no_comparable" &
          comparacion$aspecto %in% aspectos),
    0L
  )
  expect_true("rango" %in% comparacion$aspecto)
  expect_true("cardinalidad" %in% comparacion$aspecto)
})

# La deriva promete distinguir un cambio en los datos de un cambio en la vara.
# La normalizacion decide QUE VALORES SON EL MISMO y era la unica de las cuatro
# politicas que no se declaraba: con los mismos datos y `normalizar = FALSE`
# desaparecia `casi_duplicados_vocabulario` y la comparacion informaba "Un
# hallazgo del perfil anterior ya no esta presente" con severidad `ok`. En
# monitoreo, apagar la normalizacion se leia como que los datos mejoraron.
test_that("un cambio de normalizacion se declara y no pasa por mejora", {
  jose <- rawToChar(as.raw(c(0x4A, 0x6F, 0x73, 0xC3, 0xA9)))
  datos <- data.frame(
    ciudad = c(rep(jose, 10), rep("jose", 10), rep("JOSE ", 20)),
    stringsAsFactors = FALSE
  )
  con <- perfilar(datos)
  sin <- perfilar(datos, normalizar = FALSE)
  # Primero: el mecanismo se activo. Sin un hallazgo que desaparezca, la
  # comparacion no tendria nada que atribuir mal.
  expect_true("casi_duplicados_vocabulario" %in% con$hallazgos$tipo_hallazgo)
  expect_false("casi_duplicados_vocabulario" %in% sin$hallazgos$tipo_hallazgo)

  comparacion <- comparar_perfiles(con, sin)
  fila <- comparacion[comparacion$aspecto == "configuracion_normalizacion", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$severidad), "error")
  expect_match(fila$evidencia, "no es una mejora")

  # Y sigue reportando la desaparicion del hallazgo: la fila nueva no la tapa,
  # la explica.
  expect_true(any(comparacion$cambio == "resuelto"))
})

test_that("la misma normalizacion no inventa una fila de configuracion", {
  jose <- rawToChar(as.raw(c(0x4A, 0x6F, 0x73, 0xC3, 0xA9)))
  datos <- data.frame(
    ciudad = c(rep(jose, 10), rep("jose", 10), rep("JOSE ", 20)),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(comparar_perfiles(perfilar(datos), perfilar(datos))), 0L)
  expect_equal(
    nrow(comparar_perfiles(
      perfilar(datos, normalizar = FALSE),
      perfilar(datos, normalizar = FALSE)
    )),
    0L
  )
})

# Una politica ENMASCARADA no es una politica distinta. La proteccion de datos
# personales reemplaza `meta$sentinelas_numericos` por NA cuando no puede
# decidir que centinela pertenece a que columna, y comparar un perfil protegido
# contra uno sin proteger daba "Cambio la politica de centinelas numericos" con
# severidad `error` sobre dos corridas que usaban la MISMA politica.
test_that("una politica oculta se declara no comparable, no cambiada", {
  set.seed(31)
  datos <- data.frame(
    ciudad = sample(c("Montevideo", "Salto", "Rivera"), 60, TRUE),
    x = c(rnorm(55, 100, 10), -999, -999, 0, 0, NA),
    stringsAsFactors = FALSE
  )
  sin_declarar <- perfilar(datos)
  con_personal <- perfilar(datos, columnas_personales = "ciudad")
  # El mecanismo se activo: una publica la politica y la otra la enmascara.
  expect_false(anyNA(sin_declarar$meta$sentinelas_numericos))
  expect_true(all(is.na(con_personal$meta$sentinelas_numericos)))

  fila <- comparar_perfiles(sin_declarar, con_personal)
  fila <- fila[fila$aspecto == "configuracion_sentinelas_numericos", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$cambio), "no_comparable")
  expect_equal(as.character(fila$severidad), "sospechoso")
  expect_match(fila$descripcion, "no se puede")

  # El control, y es el que impide que la guarda tape un cambio real: una
  # politica que SI cambio se sigue declarando como cambiada, con error.
  otra <- comparar_perfiles(
    sin_declarar, perfilar(datos, sentinelas_numericos = c(-999))
  )
  otra <- otra[otra$aspecto == "configuracion_sentinelas_numericos", ]
  expect_equal(nrow(otra), 1L)
  expect_equal(as.character(otra$cambio), "modificado")
  expect_equal(as.character(otra$severidad), "error")
})

# Las politicas son CONJUNTOS: el orden en que se escriben es un artefacto de la
# declaracion. Sin ordenar, la misma politica escrita distinto producia una fila
# "modificado / error" -la severidad mas alta, justo la que una serie de calidad
# lee como transicion de politica-, y en el caso de `proteger` la fila mostraba
# valor_anterior y valor_actual IDENTICOS: se contradecia sola.
#
# Es la misma regla que el paquete fija para los pesos de `agregar()`: la misma
# declaracion escrita en otro orden da el mismo numero.
test_that("una politica escrita en otro orden no es un cambio de vara", {
  set.seed(3)
  datos <- data.frame(
    ciudad = sample(c("Montevideo", "Salto"), 200, TRUE),
    monto = c(rnorm(195, 100, 10), -999, -99, 0, 0, 999),
    stringsAsFactors = FALSE
  )
  filas_de <- function(anterior, actual, patron) {
    comparacion <- comparar_perfiles(anterior, actual)
    sum(grepl(patron, comparacion$aspecto))
  }

  expect_equal(
    filas_de(
      perfilar(datos, sentinelas_numericos = c(999, -9, -99, -999, -9999)),
      perfilar(datos, sentinelas_numericos = c(-9, -99, -999, -9999, 999)),
      "configuracion_sentinelas"
    ),
    0L
  )
  expect_equal(
    filas_de(
      perfilar(datos, normalizar = normalizacion(proteger = c("n", "u"))),
      perfilar(datos, normalizar = normalizacion(proteger = c("u", "n"))),
      "configuracion_normalizacion"
    ),
    0L
  )
})

# Los controles, y son los que impiden que ordenar tape un cambio real: las
# cuatro clases de cambio de vara se siguen viendo, y la guarda de la politica
# oculta tambien.
test_that("ordenar la politica no tapa ningun cambio real", {
  set.seed(3)
  datos <- data.frame(
    ciudad = sample(c("Montevideo", "Salto"), 200, TRUE),
    monto = c(rnorm(195, 100, 10), -999, -99, 0, 0, 999),
    stringsAsFactors = FALSE
  )
  filas_de <- function(anterior, actual, patron) {
    comparacion <- comparar_perfiles(anterior, actual)
    sum(grepl(patron, comparacion$aspecto))
  }
  expect_equal(
    filas_de(perfilar(datos), perfilar(datos, sentinelas_numericos = c(-999)),
             "configuracion_sentinelas"),
    1L
  )
  expect_equal(
    filas_de(perfilar(datos), perfilar(datos, normalizar = FALSE),
             "configuracion_normalizacion"),
    1L
  )
  expect_equal(
    filas_de(
      perfilar(datos, normalizar = normalizacion(proteger = c("n"))),
      perfilar(datos, normalizar = normalizacion(proteger = c("n", "u"))),
      "configuracion_normalizacion"
    ),
    1L
  )
  expect_equal(
    filas_de(perfilar(datos), perfilar(datos, columnas_personales = "ciudad"),
             "configuracion_sentinelas"),
    1L
  )
  expect_equal(nrow(comparar_perfiles(perfilar(datos), perfilar(datos))), 0L)
})

# Un unico argumento que sea una lista se despliega como lista de objetos, y un
# `data.frame` TAMBIEN es una lista: `historico_calidad(data.frame())` se
# desplegaba a cero objetos y devolvia un historico vacio en silencio, mientras
# `NULL` y `character(0)` -igual de vacios- daban error. Cuatro entradas
# equivalentes, dos conductas opuestas.
test_that("las entradas vacias se tratan todas igual", {
  datos <- data.frame(x = c(1, 2, NA, 4, 5))
  medicion <- medir(
    modelo(instanciar(especializar(metricas_nucleo()[[1L]]), "t", "x")), datos
  )

  # Un historico vacio es un concepto valido: sin argumentos, o una lista de
  # cero objetos.
  expect_equal(nrow(historico_calidad()), 0L)
  expect_equal(nrow(historico_calidad(list())), 0L)

  # Lo que no es un contenedor de mediciones se rechaza, y el mensaje lo dice.
  for (invalido in list(data.frame(), NULL, character(0))) {
    expect_error(historico_calidad(invalido), "Cada objeto debe ser")
  }

  # `acumular_historico()` tiene el mismo desplegado y por eso la misma regla:
  # arreglar solo una de las dos habria dejado la conducta vieja en la otra.
  historico <- historico_calidad(medicion)
  expect_equal(nrow(acumular_historico(historico, list())), nrow(historico))
  expect_error(
    acumular_historico(historico, data.frame()), "Cada objeto debe ser"
  )

  # Y lo legitimo no cambia.
  expect_equal(nrow(historico_calidad(medicion)), 5L)
  expect_equal(nrow(historico_calidad(list(medicion, medicion))), 5L)
  expect_equal(nrow(acumular_historico(historico, medicion)), 5L)
})

# La comparacion entre corridas solo puede declarar un cambio de vara si la vara
# quedo registrada en `meta`. Seis de las que deciden si un hallazgo se emite y
# con que severidad no viajaban: cambiar `umbral_faltantes_error` sobre la misma
# tabla producia UNA fila -`severidad_hallazgo / atenuado`- y ninguna
# `configuracion_*`. Quien leia la deriva veia que un hallazgo se atenuo y no
# podia saber que fue porque se movio el umbral.
test_that("un cambio de umbral se declara como cambio de vara", {
  set.seed(3)
  datos <- data.frame(x = c(rnorm(50), rep(NA, 50)))
  comparacion <- comparar_perfiles(
    perfilar(datos, umbral_faltantes_error = 0.4),
    perfilar(datos, umbral_faltantes_error = 0.9)
  )
  fila <- comparacion[comparacion$aspecto == "configuracion_umbrales", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$severidad), "error")
  expect_match(fila$valor_anterior, "umbral_faltantes_error")
  expect_match(fila$evidencia, "y no de los datos")

  # La consecuencia sigue publicandose: la fila nueva no la tapa, la explica.
  expect_true(any(comparacion$cambio == "atenuado"))

  # Y las seis varas viajan en `meta`.
  meta <- perfilar(datos)$meta
  for (vara in c("umbral_faltantes_sospechoso", "umbral_faltantes_error",
                 "umbral_alta_cardinalidad", "umbral_patron_dominante",
                 "columnas_sin_ceros", "columnas_no_negativas")) {
    expect_true(vara %in% names(meta), info = vara)
  }
})

# Los controles: la fila no puede aparecer cuando lo que cambian son los DATOS,
# que es la promesa central de esta capa.
test_that("un cambio de datos no se declara como cambio de umbral", {
  set.seed(3)
  base <- data.frame(x = c(rnorm(50), rep(NA, 50)))
  otros <- data.frame(x = c(rnorm(70), rep(NA, 30)))
  filas_umbral <- function(a, b) {
    sum(comparar_perfiles(a, b)$aspecto == "configuracion_umbrales")
  }
  expect_equal(filas_umbral(perfilar(base), perfilar(base)), 0L)
  expect_equal(filas_umbral(perfilar(base), perfilar(otros)), 0L)
  # Y una vara declarada por columna tambien se ve.
  expect_equal(
    filas_umbral(perfilar(base), perfilar(base, columnas_sin_ceros = "x")), 1L
  )
})
