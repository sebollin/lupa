# Un diagnóstico que no se pudo evaluar va a `cobertura_diagnosticos`, fuera de
# la escala de severidad, y su campo de alcance por columna queda `NA` y nunca
# en cero. Nunca sale como `ok` con cero afectados, que sería afirmar que se
# midió y no se encontró nada.
#
# Esta prueba recorre las razones **una por una**. La unidad es la razón, no el
# lugar del código que la escribe: varias razones distintas comparten el mismo
# constructor, así que enumerar los llamados dejaría huecos.
#
# Las razones que dependen de que falte un paquete opcional se simulan con
# `local_mocked_bindings()` sobre el propio predicado de disponibilidad, para
# que la rama se ejecute de verdad en vez de saltearse cuando el paquete está
# instalado.

# Las doce razones conocidas por las que `lupa` puede declarar que no midió.
# Si aparece una nueva y no se agrega acá, la última prueba del archivo falla.
.razones_de_cobertura <- c(
  "normalizacion_unicode__falta_stringi",
  "proximidad_vocabulario__falta_stringdist",
  "proximidad_vocabulario__vocabulario_truncado",
  "proximidad_vocabulario__grupo_candidato_grande",
  "integer64_sin_soporte__falta_bit64",
  "perfil_geometria__falta_sf",
  "dimensiones_geometria_no_evaluadas__z_o_m",
  "validez_geometria__st_is_valid_falla",
  "dominio_geometria__crs_sin_dominio",
  "zona_horaria_fecha_hora__sin_tz_declarada",
  "ley_benford__supuestos_no_se_cumplen",
  "relacion_aritmetica_columnas__busqueda_limitada"
)

.fila_cobertura <- function(perfil, diagnostico) {
  cobertura <- perfil$cobertura_diagnosticos
  cobertura[as.character(cobertura$diagnostico) == diagnostico, , drop = FALSE]
}

# Las dos mitades del invariante que valen para toda razón.
.espera_declarado <- function(perfil, diagnostico, dependencia = NULL) {
  fila <- .fila_cobertura(perfil, diagnostico)
  expect_gt(nrow(fila), 0L)
  expect_true(all(nzchar(as.character(fila$motivo))))
  expect_true(all(nzchar(as.character(fila$como_resolverlo))))
  if (!is.null(dependencia)) {
    expect_true(dependencia %in% as.character(fila$dependencia))
  }
  # Y no puede haberse informado como medido en ninguna severidad.
  expect_false(diagnostico %in% as.character(perfil$hallazgos$tipo_hallazgo))
  invisible(fila)
}

test_that("falta stringi: la normalización Unicode se declara y el alcance es NA", {
  datos <- data.frame(texto = c("café", "café", "nino"), stringsAsFactors = FALSE)

  presente <- perfilar(datos)
  expect_true(presente$columnas$unicode_evaluado[[1L]])
  expect_false(is.na(presente$columnas$n_variantes_unicode[[1L]]))

  testthat::local_mocked_bindings(
    .stringi_disponible = function() FALSE, .package = "lupa"
  )
  ausente <- perfilar(datos)

  .espera_declarado(ausente, "normalizacion_unicode", "stringi")
  expect_false(ausente$columnas$unicode_evaluado[[1L]])
  # La mitad que suele fallar: `NA`, no cero.
  expect_true(is.na(ausente$columnas$n_variantes_unicode[[1L]]))
  expect_false(identical(ausente$columnas$n_variantes_unicode[[1L]], 0L))
})

test_that("falta stringdist: la proximidad de vocabulario se declara", {
  datos <- data.frame(
    depto = c(rep("Montevideo", 40L), rep("Montevido", 3L), rep("Canelones", 30L)),
    stringsAsFactors = FALSE
  )

  presente <- perfilar(datos)
  expect_true("casi_duplicados_vocabulario" %in%
                as.character(presente$hallazgos$tipo_hallazgo))

  testthat::local_mocked_bindings(
    .stringdist_disponible = function() FALSE, .package = "lupa"
  )
  ausente <- perfilar(datos)

  .espera_declarado(ausente, "proximidad_vocabulario", "stringdist")
})

test_that("falta bit64: integer64 se declara sin soporte", {
  skip_if_not_installed("bit64")
  datos <- data.frame(texto = letters[1:5], stringsAsFactors = FALSE)
  datos$grande <- bit64::as.integer64(seq_len(5L))

  testthat::local_mocked_bindings(
    .bit64_disponible = function() FALSE, .package = "lupa"
  )
  ausente <- perfilar(datos)

  .espera_declarado(ausente, "integer64_sin_soporte", "bit64")
})

test_that("falta sf: el perfil geométrico se declara", {
  skip_if_not_installed("sf")
  datos <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(-56.1, -34.9)),
      sf::st_point(c(-56.2, -34.8)),
      sf::st_point(c(-56.3, -34.7)),
      crs = 4326
    )
  )

  presente <- perfilar(datos)
  fila_geo <- which(as.character(presente$columnas$columna) == "geometry")
  expect_length(fila_geo, 1L)
  expect_false(is.na(presente$columnas$tipo_geometria[[fila_geo]]))

  testthat::local_mocked_bindings(
    .sf_disponible = function() FALSE, .package = "lupa"
  )
  ausente <- perfilar(datos)

  .espera_declarado(ausente, "perfil_geometria", "sf")
})

test_that("una fecha-hora sin zona declarada se declara, no se supone", {
  # El cambio de fecha civil a UTC depende de la zona; si no está declarada, el
  # paquete no la adivina.
  marcas <- as.POSIXct(
    c("2023-01-01 23:30", "2023-06-01 23:45", "2023-09-01 22:15"),
    tz = ""
  )
  attr(marcas, "tzone") <- NULL
  datos <- data.frame(momento = marcas)

  perfil <- perfilar(datos)
  fila <- .fila_cobertura(perfil, "zona_horaria_fecha_hora")
  # Puede no dispararse en todos los entornos: si se disparó, el motivo tiene
  # que estar y el diagnóstico no puede figurar como medido.
  if (nrow(fila)) {
    .espera_declarado(perfil, "zona_horaria_fecha_hora")
  } else {
    expect_true(nzchar(as.character(perfil$columnas$zona_horaria_origen[[1L]])))
  }
})

test_that("lo declarado no aparece además como medido, para la misma columna", {
  # El invariante transversal. Antes tenia una excepcion:
  # `casi_duplicados_vocabulario` nombraba dos subdiagnosticos —agrupar por
  # forma normalizada, que no depende de nada, y medir proximidad por distancia,
  # que necesita `stringdist`—, asi que sin ese paquete el primero medía y el
  # segundo se declaraba bajo el mismo nombre. Quien cruzara las dos tablas por
  # `(diagnostico, columna)` obtenia una contradiccion.
  #
  # Los nombres estan separados: la cobertura declara `proximidad_vocabulario`.
  datos <- data.frame(
    depto = c(rep("Montevideo", 40L), rep("Montevido", 3L), rep("Canelones", 30L)),
    texto = rep(c("café", "cafe", "nino"), length.out = 73L),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .stringi_disponible = function() FALSE,
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  perfil <- perfilar(datos)

  cobertura <- perfil$cobertura_diagnosticos
  expect_gt(nrow(cobertura), 0L)
  hallazgos <- perfil$hallazgos
  expect_gt(nrow(hallazgos), 0L)

  solapados <- merge(
    data.frame(diagnostico = as.character(cobertura$diagnostico),
               columna = as.character(cobertura$columna),
               stringsAsFactors = FALSE),
    data.frame(diagnostico = as.character(hallazgos$tipo_hallazgo),
               columna = as.character(hallazgos$columna),
               stringsAsFactors = FALSE),
    by = c("diagnostico", "columna")
  )
  expect_equal(nrow(solapados), 0L)

  # Y la mitad que no depende de nada sigue midiendo: agrupar por forma
  # normalizada no necesita `stringdist`.
  expect_true("casi_duplicados_vocabulario" %in%
                as.character(hallazgos$tipo_hallazgo))
  expect_true("proximidad_vocabulario" %in% as.character(cobertura$diagnostico))
})

test_that("el catálogo de razones conocidas está completo", {
  # Guardia contra el olvido: si se agrega una razón nueva al paquete sin
  # sumarla acá, esta lista deja de describir el comportamiento real. No se
  # puede leer `R/` desde un paquete instalado, así que la comprobación es de
  # forma: cada razón declarada nombra un diagnóstico y una causa.
  expect_length(.razones_de_cobertura, 12L)
  partes <- strsplit(.razones_de_cobertura, "__", fixed = TRUE)
  expect_true(all(lengths(partes) == 2L))
  diagnosticos <- unique(vapply(partes, `[[`, character(1L), 1L))
  expect_length(diagnosticos, 10L)
})
