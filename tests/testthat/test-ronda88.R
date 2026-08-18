test_that("la falta de stringi se declara como informacion", {
  local_mocked_bindings(
    .stringi_disponible = function() FALSE,
    .package = "lupa"
  )
  perfil <- perfilar(
    data.frame(nombre = c("Jos\u00e9", "Jose\u0301")),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[1L, , drop = FALSE]
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "normalizacion_unicode", ,
    drop = FALSE
  ]
  expect_false(fila$unicode_evaluado)
  expect_true(is.na(fila$n_variantes_unicode))
  expect_equal(nrow(cobertura), 1L)
  expect_equal(cobertura$dependencia, "stringi")
  expect_match(cobertura$motivo, "no se pudo evaluar", ignore.case = TRUE)
  expect_false("normalizacion_unicode" %in% perfil$hallazgos$tipo_hallazgo)

  ascii <- perfilar(
    data.frame(nombre = c("Jose", "Maria")),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila_ascii <- ascii$columnas[1L, , drop = FALSE]
  expect_true(fila_ascii$unicode_evaluado)
  expect_equal(fila_ascii$n_variantes_unicode, 0L)
  expect_false(any(ascii$hallazgos$tipo_hallazgo == "normalizacion_unicode"))
})

test_that("las dependencias opcionales ausentes no inundan los sospechosos", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  set.seed(8801)
  datos <- as.data.frame(setNames(
    replicate(40L, sample(c("alpha", "beta", "gamma"), 100L, replace = TRUE),
             simplify = FALSE),
    paste0("v", seq_len(40L))
  ), stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  opcionales <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "proximidad_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(opcionales), 40L)
  expect_true(all(opcionales$dependencia == "stringdist"))
  expect_equal(nrow(perfil$hallazgos), 0L)
  salida <- capture.output(print(perfil), type = "message")
  expect_true(any(grepl("0 hallazgos sospechosos", salida, fixed = TRUE)))
  expect_true(any(grepl("40 diagnosticos no evaluados", salida, fixed = TRUE)))
})

test_that("integer64 sin bit64 es una declaracion informativa", {
  local_mocked_bindings(
    .bit64_disponible = function() FALSE,
    .package = "lupa"
  )
  x <- structure(c(1, 2), class = "integer64")
  perfil <- perfilar(
    data.frame(codigo = x), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "integer64_sin_soporte", ,
    drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_equal(cobertura$dependencia, "bit64")
  expect_false("integer64_sin_soporte" %in% perfil$hallazgos$tipo_hallazgo)
})

test_that("una zona sin declarar no depende de la maquina", {
  x <- as.POSIXct(c("2026-01-01 23:59:00", "2026-01-01 12:00:00"))
  anterior <- Sys.getenv("TZ", unset = NA_character_)
  on.exit(if (is.na(anterior)) Sys.unsetenv("TZ") else Sys.setenv(TZ = anterior),
          add = TRUE)
  resultados <- lapply(c("UTC", "America/Montevideo", "Asia/Tokyo"), function(tz) {
    Sys.setenv(TZ = tz)
    perfil <- perfilar(
      data.frame(fecha = x), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    fila <- perfil$columnas[1L, , drop = FALSE]
    cobertura <- perfil$cobertura_diagnosticos[
      perfil$cobertura_diagnosticos$diagnostico == "zona_horaria_fecha_hora", ,
      drop = FALSE
    ]
    list(
      zona = fila$zona_horaria_origen,
      n = fila$n_filas_fecha_civil_distinta_utc,
      motivo = cobertura$motivo,
      resolver = cobertura$como_resolverlo,
      cobertura = cobertura
    )
  })
  expect_true(all(vapply(resultados, function(x) identical(x$zona, "sin_declarar"), logical(1L))))
  expect_true(all(vapply(resultados, function(x) is.na(x$n), logical(1L))))
  expect_equal(vapply(resultados, `[[`, character(1L), "motivo"),
               rep(resultados[[1L]]$motivo, 3L))
  expect_equal(vapply(resultados, `[[`, character(1L), "resolver"),
               rep(resultados[[1L]]$resolver, 3L))
  expect_true(all(vapply(resultados, function(x) nrow(x$cobertura) == 1L,
                         logical(1L))))

  solo_na <- perfilar(
    data.frame(fecha = as.POSIXct(NA)), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, proteger_datos_personales = FALSE
  )
  expect_true(is.na(solo_na$columnas$n_filas_fecha_civil_distinta_utc[[1L]]))
})

test_that("el alcance de formatos y patrones conserva la columna completa", {
  valores <- rep("2026-01-01", 200000L)
  directo <- detectar_formatos_fecha(valores, muestra = 100000L)
  perfil <- perfilar(
    data.frame(fecha = valores), muestra = 100000L,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  formatos <- perfil$formatos_fecha[[1L]]
  expect_equal(attr(formatos, "total"), attr(directo, "total"))
  expect_equal(attr(formatos, "analizados"), attr(directo, "analizados"))
  expect_equal(attr(formatos, "muestreado"), attr(directo, "muestreado"))
  patrones <- perfil$patrones[[1L]]
  expect_equal(attr(patrones, "filas_analizadas"), 100000L)
  expect_equal(attr(patrones, "filas_analizadas"), attr(patrones, "analizados"))
})

test_that("la clase token_unico evita llamar errata a un codigo", {
  expect_equal(
    lupa:::.clase_diferencia_vocabulario("Montevideo", "Montevido"),
    "token_unico"
  )
  expect_equal(
    lupa:::.clase_diferencia_vocabulario("CAMINO CARRASCO", "CAMINO AGRARIOS"),
    "token_completo"
  )
  expect_equal(
    lupa:::.clase_diferencia_vocabulario("A001", "A002"),
    "token_unico"
  )
  expect_equal(
    lupa:::.clase_diferencia_vocabulario("A 001", "A001"),
    "mixta"
  )
})

test_that("la normalizacion no afirma identidad en la descripcion", {
  perfil <- perfilar(
    data.frame(nombre = c(rep("LA PE\u00d1A", 10L), rep("LA PENA", 5L))),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$descripcion, "forma normalizada coincide")
  expect_match(hallazgo$descripcion, "no confirma que sean la misma entidad")
})
