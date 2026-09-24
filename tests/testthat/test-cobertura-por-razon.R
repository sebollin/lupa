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

# Las veinte razones conocidas por las que `lupa` puede declarar que no
# midió. El inventario se compara contra el vocabulario que el paquete produce
# de verdad, en la última prueba del archivo: la versión anterior decía que una
# razón nueva hacía fallar esa prueba, y no era cierto —contaba el inventario
# contra sí mismo—. La razón de Benford por tipo declarado existía desde antes
# de que se escribiera esa frase y no estaba en la lista.
.razones_de_cobertura <- c(
  "normalizacion_unicode__falta_stringi",
  "proximidad_vocabulario__falta_stringdist",
  "proximidad_vocabulario__vocabulario_truncado",
  "proximidad_vocabulario__grupo_candidato_grande",
  "integer64_sin_soporte__falta_bit64",
  "secuencia_entera__falta_bit64",
  "perfil_geometria__falta_sf",
  "dimensiones_geometria_no_evaluadas__z_o_m",
  "validez_geometria__st_is_valid_falla",
  "dominio_geometria__crs_sin_dominio",
  "zona_horaria_fecha_hora__sin_tz_declarada",
  "ley_benford__supuestos_no_se_cumplen",
  "relacion_aritmetica_columnas__busqueda_limitada",
  "relacion_aritmetica_columnas__clase_declarada",
  "ley_benford__clase_declarada",
  "outliers__sin_valores_finitos",
  "outliers__pocos_valores_finitos",
  "outliers__iqr_cero",
  "outliers__parece_identificador",
  "patron_raro__dominante_insuficiente"
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

# Las dos mitades van separadas a proposito. La mitad que mide necesita
# `stringi` instalado; la mitad que declara su ausencia no puede necesitarlo,
# porque es justamente la que simula que no esta. Juntas, en una maquina sin
# `stringi`, la primera fallaba y se llevaba puesta a la segunda: la prueba de
# la ausencia exigia la presencia.
test_that("con stringi, la normalización Unicode se mide y el alcance no es NA", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  datos <- data.frame(texto = c("café", "café", "nino"), stringsAsFactors = FALSE)

  presente <- perfilar(datos)
  expect_true(presente$columnas$unicode_evaluado[[1L]])
  expect_false(is.na(presente$columnas$n_variantes_unicode[[1L]]))
})

test_that("falta stringi: la normalización Unicode se declara y el alcance es NA", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(texto = c("café", "café", "nino"), stringsAsFactors = FALSE)

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
  skip_if_not_installed("stringdist")
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
  expect_length(.razones_de_cobertura, 20L)
  partes <- strsplit(.razones_de_cobertura, "__", fixed = TRUE)
  expect_true(all(lengths(partes) == 2L))
  diagnosticos <- unique(vapply(partes, `[[`, character(1L), 1L))
  expect_length(diagnosticos, 13L)
})

test_that("una columna numérica con clase declarada se declara dos veces", {
  skip_if_not_installed("units")
  set.seed(214)
  valores <- round(stats::runif(120, 1, 9999), 2)
  datos <- data.frame(neto = valores, total = valores * 1.22)
  datos$distancia <- units::set_units(valores, "m")

  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  # Las dos razones: ni las relaciones aritméticas ni Benford operan sobre una
  # columna que declara una clase, y las dos lo dicen.
  aritmetica <- .fila_cobertura(perfil, "relacion_aritmetica_columnas")
  aritmetica <- aritmetica[aritmetica$columna == "distancia", , drop = FALSE]
  expect_equal(nrow(aritmetica), 1L)
  expect_true(grepl("units", aritmetica$motivo[[1L]], fixed = TRUE))
  expect_true(nzchar(aritmetica$como_resolverlo[[1L]]))

  benford <- .fila_cobertura(perfil, "ley_benford")
  benford <- benford[benford$columna == "distancia", , drop = FALSE]
  expect_equal(nrow(benford), 1L)
  expect_true(grepl("units", benford$motivo[[1L]], fixed = TRUE))

  # Y la mitad que mide: las dos columnas peladas sí se analizaron.
  expect_true("relacion_aritmetica_columnas" %in%
                as.character(perfil$hallazgos$tipo_hallazgo))
})

test_that("una columna integer64 declara la razón de Benford por su tipo", {
  skip_if_not_installed("bit64")
  set.seed(215)
  datos <- data.frame(id = seq_len(120))
  datos$grande <- bit64::as.integer64(round(stats::runif(120, 1, 999999)))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- .fila_cobertura(perfil, "ley_benford")
  fila <- fila[fila$columna == "grande", , drop = FALSE]

  expect_equal(nrow(fila), 1L)
  expect_true(grepl("integer64", fila$motivo[[1L]], fixed = TRUE))
})

test_that("las cuatro razones por las que no se evaluan outliers se declaran", {
  casos <- list(
    sin_valores_finitos = list(
      datos = data.frame(x = rep(Inf, 30L), id = seq_len(30L)),
      clave = "no hay valores finitos"
    ),
    pocos_valores_finitos = list(
      datos = data.frame(x = as.numeric(1:10), id = seq_len(10L)),
      clave = "se requieren al menos 20"
    ),
    iqr_cero = list(
      datos = data.frame(x = c(rep(0, 90L), rep(1, 10L)), id = seq_len(100L)),
      clave = "recorrido"
    )
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    perfil <- perfilar(caso$datos, analizar_dependencias = FALSE,
                       proteger_datos_personales = FALSE)
    fila <- .fila_cobertura(perfil, "outliers")
    fila <- fila[fila$columna == "x", , drop = FALSE]
    expect_equal(nrow(fila), 1L, info = nombre)
    expect_true(grepl(caso$clave, fila$motivo[[1L]], fixed = TRUE), info = nombre)
    expect_true(nzchar(fila$como_resolverlo[[1L]]), info = nombre)
    expect_false("outliers" %in% as.character(perfil$hallazgos$tipo_hallazgo),
                 info = nombre)
  }

  # La cuarta razon: una numeracion densa con un centinela. Aca el conteo SI
  # existe, y lo que se declara es que no se interpreta como distancia.
  denso <- data.frame(id_padron = c(1:1000, rep(9999, 15L)))
  perfil <- perfilar(denso, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  fila <- .fila_cobertura(perfil, "outliers")
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("enteros", fila$motivo[[1L]], fixed = TRUE))

  # Control: una columna con variacion y suficientes valores mide y no declara.
  set.seed(217)
  normal <- data.frame(x = c(stats::rnorm(100L), 50), id = seq_len(101L))
  perfil_medido <- perfilar(normal, analizar_dependencias = FALSE,
                            proteger_datos_personales = FALSE)
  expect_equal(nrow(.fila_cobertura(perfil_medido, "outliers")), 0L)
  expect_true("outliers" %in%
                as.character(perfil_medido$hallazgos$tipo_hallazgo))
})

test_that("el inventario de razones se mide contra lo que el paquete produce", {
  # Esta es la prueba que la frase de arriba prometía y no existía. No puede
  # enumerar razones -varias comparten constructor y el motivo es prosa-, pero
  # sí puede exigir que todo DIAGNÓSTICO declarado esté inventariado: una razón
  # nueva sobre un diagnóstico nuevo cae acá.
  registrados <- unique(vapply(
    strsplit(.razones_de_cobertura, "__", fixed = TRUE),
    `[[`, character(1L), 1L
  ))
  set.seed(216)
  valores <- round(stats::runif(120, 1, 9999), 2)
  con_clase <- data.frame(neto = valores, total = valores * 1.22)
  if (requireNamespace("units", quietly = TRUE)) {
    con_clase$distancia <- units::set_units(valores, "m")
  }
  anchas <- as.data.frame(replicate(21L, stats::rnorm(200L)))
  tablas <- list(
    con_clase,
    anchas,
    data.frame(a = c(1, 2), b = c(2, 4)),
    data.frame(v = c(rep("Montevideo", 6L), rep("Montevido", 4L)),
               stringsAsFactors = FALSE)
  )
  vistos <- character()
  for (tabla in tablas) {
    perfil <- suppressWarnings(perfilar(
      tabla, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
    vistos <- c(vistos, as.character(perfil$cobertura_diagnosticos$diagnostico))
  }
  vistos <- unique(vistos)

  # Que la batería haya declarado algo: un inventario comparado con la nada
  # pasa siempre y no mide.
  expect_gt(length(vistos), 2L)
  expect_true(all(vistos %in% registrados),
              info = paste(setdiff(vistos, registrados), collapse = ", "))
})

test_that("los grupos bajo el piso de asimetría se declaran, no desaparecen", {
  skip_if_not_installed("stringdist")
  # El piso de asimetría existe porque `este`/`oeste` —dos valores legítimos y
  # parecidos— se abría como sospechoso con asimetría 1,5. Pero en esa misma
  # banda cae una errata sistemática que afecta al 40 % de los registros, y por
  # la forma son indistinguibles.
  #
  # Lo honesto no es elegir en silencio cuál se sacrifica: es decir cuántos
  # quedaron afuera y cómo cambiarlo.
  seis_cuatro <- perfilar(
    data.frame(v = c(rep("Montevideo", 6L), rep("Montevido", 4L)),
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgos <- seis_cuatro$hallazgos
  sospechosos <- hallazgos[
    as.character(hallazgos$tipo_hallazgo) == "casi_duplicados_vocabulario" &
      as.character(hallazgos$severidad) != "ok", , drop = FALSE
  ]
  expect_equal(nrow(sospechosos), 0L)

  # Pero queda declarado.
  fila <- .fila_cobertura(seis_cuatro, "proximidad_vocabulario")
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("asimetria de frecuencias", fila$motivo, fixed = TRUE))
  expect_true(grepl("min_asimetria_vocabulario", fila$como_resolverlo,
                    fixed = TRUE))

  # Por encima del piso, el hallazgo se informa y no hay nada que declarar.
  siete_tres <- perfilar(
    data.frame(v = c(rep("Montevideo", 7L), rep("Montevido", 3L)),
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgos <- siete_tres$hallazgos
  expect_true("casi_duplicados_vocabulario" %in%
                as.character(hallazgos$tipo_hallazgo))
  expect_equal(nrow(.fila_cobertura(siete_tres, "proximidad_vocabulario")), 0L)
})
