# La inferencia necesita evidencia de la columna y las declaraciones siguen
# siendo afirmaciones del usuario.

testthat::skip_on_cran()

.tipos_dominio_O184b <- c(
  "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
)

.hallazgos_dominio_O184b <- function(perfil) {
  perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% .tipos_dominio_O184b,
    , drop = FALSE
  ]
}

testthat::test_that("la columna sostiene la regla inferida", {
  set.seed(1841)
  n <- 60L
  fecha <- as.Date("2026-11-19")
  fecha_ref <- c(
    rep(as.Date("1970-01-01"), 8L),
    as.Date("1970-01-01") + seq_len(n - 8L)
  )
  datos <- data.frame(
    fecha_prevista = fecha + seq_len(n),
    longitud_cable = sample(200:5000, n, TRUE),
    pct_change = sample(120:400, n, TRUE),
    edad_paciente_dias = sample(0:30000, n, TRUE),
    lat = sample(500000:600000, n, TRUE),
    lng = sample(6000000:6200000, n, TRUE),
    fecha_ref = fecha_ref,
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  ))
  hallazgos <- .hallazgos_dominio_O184b(perfil)
  # Los nombres que anuncian otra unidad o valores esperados afuera no infieren
  # nada. Una pareja `lat`/`lng` en UTM si es una latitud por nombre: queda como
  # aviso `ok` de la columna, sin acusar filas, y dice la escala.
  testthat::expect_true(all(hallazgos$columna %in% c("lat", "lng")))
  testthat::expect_true(all(hallazgos$severidad == "ok"))
})

testthat::test_that("los positivos verdaderos y los nombres inequivocos sobreviven", {
  set.seed(1842)
  n <- 60L
  fecha <- as.Date("2026-11-19")
  edad_anios <- c(sample(0:90, n - 1L, TRUE), 210)
  edad_anos <- c(sample(0:90, n - 1L, TRUE), 210)
  datos <- data.frame(
    edad = c(sample(0:90, n - 1L, TRUE), 210),
    porcentaje = c(sample(0:100, n - 1L, TRUE), 120),
    latitud = c(runif(n - 1L, -40, -30), 95),
    fecha_nacimiento = c(fecha - sample(5000:20000, n - 1L), fecha + 1L),
    edad_anios = edad_anios,
    stringsAsFactors = FALSE
  )
  datos[[paste0("edad_a", intToUtf8(0xF1), "os")]] <- edad_anos
  perfil <- suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  ))
  hallazgos <- .hallazgos_dominio_O184b(perfil)
  fuera <- hallazgos[hallazgos$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  futura <- hallazgos[hallazgos$tipo_hallazgo == "fecha_futura", , drop = FALSE]
  testthat::expect_equal(fuera$n_afectados[fuera$columna == "edad"], 1)
  testthat::expect_equal(fuera$n_afectados[fuera$columna == "porcentaje"], 1)
  testthat::expect_equal(fuera$n_afectados[fuera$columna == "latitud"], 1)
  testthat::expect_equal(fuera$n_afectados[fuera$columna == "edad_anios"], 1)
  testthat::expect_equal(
    fuera$n_afectados[fuera$columna == paste0("edad_a", intToUtf8(0xF1), "os")],
    1
  )
  testthat::expect_equal(
    futura$n_afectados[futura$columna == "fecha_nacimiento"], 1
  )
})

testthat::test_that("un fin abierto queda como centinela y no como futuro masivo", {
  set.seed(1843)
  fecha <- as.Date("2026-11-19")
  datos <- data.frame(
    fecha_hasta = c(rep("9999-12-31", 40L),
                    as.character(fecha + sample(5:2000, 20L))),
    stringsAsFactors = FALSE
  )
  hallazgos <- .hallazgos_dominio_O184b(suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )))
  testthat::expect_equal(
    hallazgos$n_afectados[hallazgos$tipo_hallazgo == "fecha_centinela"],
    40
  )
  testthat::expect_false(any(hallazgos$tipo_hallazgo == "fecha_futura"))
})

testthat::test_that("Date de referencia no acusa horas del mismo dia", {
  fecha <- as.Date("2026-11-19")
  datos <- data.frame(
    fecha_alta = as.POSIXct("2026-11-19 00:30:00", tz = "UTC") + 0:59
  )
  hallazgos <- .hallazgos_dominio_O184b(suppressWarnings(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )))
  testthat::expect_false(any(hallazgos$tipo_hallazgo == "fecha_futura"))
})

testthat::test_that("las reglas declaradas no pasan por la guarda inferida", {
  datos <- data.frame(edad = rep(200, 60L), grupo = rep("a", 60L))
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.Date("2026-11-19"),
    dominios = list(edad = list(maximo = 100)),
    analizar_dependencias = FALSE
  ))
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valor_fuera_dominio" &
      perfil$hallazgos$columna == "edad", , drop = FALSE
  ]
  testthat::expect_equal(hallazgo$n_afectados, 60)
  testthat::expect_identical(as.character(hallazgo$severidad), "error")
  testthat::expect_error(
    perfilar(
      datos, dominios = list(grupo = list(minimo = 0)),
      analizar_dependencias = FALSE
    ),
    "requiere una columna numerica"
  )
})

testthat::test_that("un perfil viejo sin dominios queda no comparable", {
  datos <- data.frame(edad = c(rep(40, 59L), 210))
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.Date("2026-11-19"), analizar_dependencias = FALSE
  ))
  viejo <- perfil
  viejo$meta$dominios <- NULL
  deriva <- comparar_perfiles(viejo, perfil)
  configuracion <- deriva[deriva$aspecto == "configuracion_dominios", , drop = FALSE]
  testthat::expect_equal(nrow(configuracion), 1L)
  testthat::expect_identical(as.character(configuracion$cambio), "no_comparable")
  testthat::expect_identical(as.character(configuracion$severidad), "sospechoso")
  testthat::expect_false(any(configuracion$severidad == "error"))
})
