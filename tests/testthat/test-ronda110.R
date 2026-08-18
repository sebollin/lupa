.tipos_senalados_r110 <- function(perfil, columna) {
  perfil$hallazgos$tipo_hallazgo[
    perfil$hallazgos$columna == columna &
      as.character(perfil$hallazgos$severidad) != "ok"
  ]
}

test_that("las secuencias enteras densas apagan solo la lectura del contenido", {
  huecos_id <- unique(c(seq(10L, 2690L, by = 10L), 271:284))
  datos <- data.frame(
    index = as.character(seq_len(2410L)),
    id = as.character(setdiff(seq_len(2692L), huecos_id)),
    brewery_id = as.character(rep(0:557, length.out = 2410L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )

  expect_true(all(perfil$columnas$secuencia_entera_densa))
  expect_equal(
    perfil$columnas$densidad_secuencia_entera,
    c(1, 2410 / 2692, 1), tolerance = 1e-12
  )
  expect_equal(perfil$columnas$n_huecos_secuencia_entera, c(0, 282, 0))
  for (columna in names(datos)) {
    tipos <- .tipos_senalados_r110(perfil, columna)
    expect_false("faltantes_disfrazados" %in% tipos)
    expect_false("patron_raro" %in% tipos)
  }
  identificadores <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "posible_identificador", , drop = FALSE
  ]
  expect_setequal(identificadores$columna, c("index", "id"))
  expect_true(all(as.character(identificadores$severidad) == "ok"))
  expect_true(all(grepl("secuencia_entera_densa=TRUE", identificadores$evidencia,
                        fixed = TRUE)))

  con_ausente_y_duplicado <- data.frame(
    id = c(as.character(seq_len(100L)), "50", rep(NA_character_, 20L))
  )
  conservado <- perfilar(
    con_ausente_y_duplicado, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_true(conservado$columnas$secuencia_entera_densa)
  expect_equal(conservado$columnas$n_faltantes, 20L)
  expect_true("faltantes" %in% conservado$hallazgos$tipo_hallazgo)
  expect_true("filas_duplicadas" %in% conservado$hallazgos$tipo_hallazgo)
})

test_that("un identificador no secuencial conserva el patron corto sospechoso", {
  documentos <- c(
    sprintf("%08d", seq(10000001L, 10001000L, by = 2L)), "9"
  )
  perfil <- perfilar(
    data.frame(documento = documentos), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "documento", ]
  tipos <- .tipos_senalados_r110(perfil, "documento")

  expect_false(fila$secuencia_entera_densa)
  expect_lt(fila$densidad_secuencia_entera, 0.001)
  expect_true("patron_raro" %in% tipos)

  fuera_de_rango <- perfilar(
    data.frame(id = as.character(c(seq_len(100L), 10000L))),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_false(fuera_de_rango$columnas$secuencia_entera_densa)
  expect_true("outliers" %in% .tipos_senalados_r110(fuera_de_rango, "id"))
})

test_that("la condicion densa es causal para las dos supresiones", {
  local_mocked_bindings(
    .resumen_secuencia_entera = function(...) {
      list(
        densa = FALSE, densidad = 1, n_posiciones = 1000,
        n_huecos = 0, umbral_densidad = 0.8, min_distintos = 20L
      )
    },
    .package = "lupa"
  )
  perfil <- perfilar(
    data.frame(index = as.character(seq_len(1000L))),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  tipos <- .tipos_senalados_r110(perfil, "index")
  expect_true("faltantes_disfrazados" %in% tipos)
  expect_true("patron_raro" %in% tipos)
})

.controles_vocabulario_corto_r110 <- function() {
  list(
    m_f = c(rep("M", 510L), rep("F", 490L)),
    si_no = c(rep("SI", 600L), rep("NO", 400L)),
    abcd = rep(c("A", "B", "C", "D"), each = 250L),
    meses = rep(c("ene", "feb", "mar", "abr", "may", "jun"), each = 160L),
    estados = c(
      rep("MO", 400L), rep("CA", 200L), rep("MA", 150L),
      rep("SA", 150L), rep("PA", 100L)
    ),
    paises = c(
      rep("UY", 400L), rep("AR", 300L), rep("BR", 200L), rep("CL", 100L)
    ),
    niveles = c(rep("alto", 400L), rep("medio", 350L), rep("bajo", 250L))
  )
}

test_that("los siete vocabularios cortos legitimos quedan callados", {
  perfiles <- lapply(.controles_vocabulario_corto_r110(), function(x) {
    perfilar(
      data.frame(valor = x), analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
  })
  senales <- vapply(perfiles, function(perfil) {
    any(
      perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario" &
        as.character(perfil$hallazgos$severidad) != "ok"
    )
  }, logical(1L))
  expect_false(any(senales), info = paste(names(senales)[senales], collapse = ", "))
})

test_that("un catalogo breve sesgado sin forma mayoritaria queda callado", {
  estados <- c(
    rep("CA", 400L), rep("MI", 300L), rep("WI", 200L), rep("WV", 4L),
    rep("AR", 3L), rep("OR", 93L)
  )
  perfil <- perfilar(
    data.frame(estado = estados), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_false(any(
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario" &
      as.character(perfil$hallazgos$severidad) != "ok"
  ))
})

test_that("las erratas breves raras frente a una forma dominante se detectan", {
  skip_if_not_installed("stringdist")
  casos <- list(
    emergencia = c(
      rep("yes", 830L), rep("no", 143L), rep("yxs", 11L),
      rep("yex", 9L), rep("xes", 5L), rep("xo", 2L)
    ),
    respuesta = c(rep("SI", 600L), rep("NO", 390L), rep("SL", 10L))
  )
  perfiles <- lapply(casos, function(x) perfilar(
    data.frame(valor = x), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  ))
  hallazgos <- lapply(perfiles, function(perfil) perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario" &
      as.character(perfil$hallazgos$severidad) == "sospechoso", , drop = FALSE
  ])

  expect_true(all(vapply(hallazgos, nrow, integer(1L)) > 0L))
  expect_match(hallazgos$emergencia$evidencia, "yes (830)", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "yxs (11)", fixed = TRUE)
  expect_match(hallazgos$respuesta$evidencia, "SI (600)", fixed = TRUE)
  expect_match(hallazgos$respuesta$evidencia, "SL (10)", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "distancia_edicion<=1", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "participacion_variante<=0.050", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "asimetria>=10.0", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "participacion_dominante>=0.500", fixed = TRUE)
  expect_match(hallazgos$emergencia$evidencia, "largo<=6", fixed = TRUE)
})

test_that("los umbrales del vocabulario corto son publicos y validados", {
  perfil <- perfilar(
    data.frame(x = c(rep("yes", 100L), "yxs")),
    analizar_dependencias = FALSE
  )
  expect_equal(perfil$meta$umbral_variante_rara_vocabulario, 0.05)
  expect_equal(perfil$meta$min_asimetria_vocabulario_corto, 10)
  expect_equal(
    perfil$meta$min_participacion_dominante_vocabulario_corto, 0.5
  )
  expect_error(
    perfilar(data.frame(x = "a"), umbral_variante_rara_vocabulario = 2),
    "umbral_variante_rara_vocabulario", fixed = TRUE
  )
  expect_error(
    perfilar(data.frame(x = "a"), min_asimetria_vocabulario_corto = 0.5),
    "min_asimetria_vocabulario_corto", fixed = TRUE
  )
  expect_error(
    perfilar(
      data.frame(x = "a"),
      min_participacion_dominante_vocabulario_corto = 2
    ),
    "min_participacion_dominante_vocabulario_corto", fixed = TRUE
  )
})

test_that("las nueve detecciones reales de precision se conservan", {
  skip_if_not_installed("stringdist")
  perfiles <- list(
    localidad = perfilar(
      data.frame(x = c(rep("Montevideo", 30L), rep("Montevido", 3L))),
      analizar_dependencias = FALSE
    ),
    acento = perfilar(
      data.frame(x = c(rep("San Jose", 30L), rep("San Jos\u00e9", 3L))),
      analizar_dependencias = FALSE
    ),
    orden = {
      inicio <- seq_len(100L)
      fin <- inicio + 10L
      fin[seq_len(3L)] <- inicio[seq_len(3L)] - 1L
      perfilar(data.frame(inicio = inicio, fin = fin),
               analizar_dependencias = FALSE)
    },
    monedas = perfilar(
      data.frame(x = c("100 UYU", "25 USD", "300 UYU", "40 USD")),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ),
    unidades = perfilar(
      data.frame(x = c("12 kg", "13500 g", "9 kg", "800 g", "11 kg")),
      analizar_dependencias = FALSE
    ),
    multivaluados = perfilar(
      data.frame(x = c(
        "099111222", "099111222; 24001234", "24005678",
        "099333444, 24009999"
      )), analizar_dependencias = FALSE
    ),
    fechas = perfilar(
      data.frame(x = rep(c("2020-01-31", "31/01/2020"), each = 10L)),
      analizar_dependencias = FALSE
    ),
    faltantes = perfilar(
      data.frame(x = c(rep("dato", 30L), "NA")),
      analizar_dependencias = FALSE
    ),
    espacios = perfilar(
      data.frame(x = c(rep("dato", 30L), " dato")),
      analizar_dependencias = FALSE
    )
  )
  esperados <- c(
    localidad = "casi_duplicados_vocabulario",
    acento = "casi_duplicados_vocabulario",
    orden = "relacion_orden_columnas",
    monedas = "monedas_mixtas",
    unidades = "unidades_mixtas",
    multivaluados = "celdas_multivaluadas",
    fechas = "formatos_fecha_mixtos",
    faltantes = "faltantes_disfrazados",
    espacios = "espacios_sobrantes"
  )
  for (nombre in names(esperados)) {
    expect_true(
      esperados[[nombre]] %in% perfiles[[nombre]]$hallazgos$tipo_hallazgo,
      info = nombre
    )
  }
})
