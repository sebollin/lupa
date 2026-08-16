relaciones_aritmeticas_r99 <- function(datos, ...) {
  perfil <- perfilar(datos, analizar_dependencias = FALSE, ...)
  perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas", ,
    drop = FALSE
  ]
}

tabla_suma_r99 <- function(n, rotas = 1L) {
  x <- seq_len(n) * 7
  y <- (seq_len(n) + 3L)^2
  z <- x + y
  if (rotas > 0L) z[seq_len(rotas)] <- z[seq_len(rotas)] + 1000
  data.frame(x, y, z)
}

test_that("el limite absoluto rescata una sola violacion en tablas chicas", {
  tamanos <- c(50L, 150L, 199L, 200L, 1000L)
  medicion <- lapply(tamanos, function(n) {
    relaciones <- relaciones_aritmeticas_r99(tabla_suma_r99(n))
    suma <- relaciones[
      lengths(strsplit(relaciones$columna, ",", fixed = TRUE)) == 3L, ,
      drop = FALSE
    ]
    data.frame(
      n = n,
      cumplimiento = (n - 1) / n,
      informa = nrow(suma) == 1L
    )
  })
  medicion <- do.call(rbind, medicion)

  expect_equal(medicion$n, tamanos)
  expect_equal(medicion$cumplimiento, (tamanos - 1) / tamanos)
  expect_true(all(medicion$informa))

  evidencia_50 <- relaciones_aritmeticas_r99(tabla_suma_r99(50L))$evidencia
  expect_match(evidencia_50, "0.980 de cumplimiento", fixed = TRUE)
  expect_match(evidencia_50, "cumplimiento >= 0.995", fixed = TRUE)
  expect_match(evidencia_50, "discrepancias <= 1", fixed = TRUE)
  expect_match(evidencia_50, "al menos 3 filas comparables", fixed = TRUE)
})

test_that("diez violaciones de cincuenta no forman una relacion", {
  relaciones <- relaciones_aritmeticas_r99(tabla_suma_r99(50L, 10L))
  expect_equal(nrow(relaciones), 0L)
})

test_that("los dos controles negativos siguen en cero", {
  set.seed(9901)
  ruido <- as.data.frame(replicate(10L, rnorm(200L)))
  expect_equal(nrow(relaciones_aritmeticas_r99(ruido)), 0L)

  set.seed(9902)
  latente <- rnorm(200L)
  correlacionadas <- as.data.frame(replicate(
    10L, latente + rnorm(200L, sd = 0.03)
  ))
  correlaciones <- stats::cor(correlacionadas)
  expect_gt(min(correlaciones[upper.tri(correlaciones)]), 0.99)
  expect_equal(nrow(relaciones_aritmeticas_r99(correlacionadas)), 0L)
})

test_that("los criterios aritmeticos son configurables y visibles", {
  datos <- tabla_suma_r99(50L)
  sin_rescate <- relaciones_aritmeticas_r99(
    datos, max_violaciones_aritmetica = 0L
  )
  expect_equal(nrow(sin_rescate), 0L)

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  alcance <- perfil$meta$aritmetica_columnas
  expect_equal(alcance$umbral_cumplimiento, 0.995)
  expect_equal(alcance$max_violaciones, 1L)
  expect_equal(alcance$minimo_filas_comparables, 3L)

  expect_error(
    perfilar(datos, max_violaciones_aritmetica = -1),
    "max_violaciones_aritmetica"
  )
  expect_error(
    perfilar(datos, min_filas_aritmetica = 2), "min_filas_aritmetica"
  )
})

test_that("metrica acepta el vocabulario relacional y guarda el canonico", {
  catalogo <- granularidades()
  con_alias <- !is.na(catalogo$relacional)
  normalizadas <- vapply(
    catalogo$relacional[con_alias],
    function(alias) lupa:::.declaracion_metrica(metrica(
      "Cobertura", "Cobertura relacional.", alias, "real"
    ))$granularidad,
    character(1L)
  )
  expect_equal(unname(normalizadas), catalogo$granularidad[con_alias])
  expect_equal(
    unname(normalizadas[catalogo$relacional[con_alias] == "columna"]),
    "atributo"
  )

  error <- expect_error(
    metrica("M", "Semántica.", "renglon", "real")
  )
  mensaje <- conditionMessage(error)
  expect_match(mensaje, "Ontología:", fixed = TRUE)
  expect_match(mensaje, "Relacional:", fixed = TRUE)
  expect_true(all(vapply(
    catalogo$granularidad, grepl, logical(1L), x = mensaje,
    fixed = TRUE
  )))
  aliases <- stats::na.omit(catalogo$relacional)
  expect_true(all(vapply(
    aliases, grepl, logical(1L), x = mensaje, fixed = TRUE
  )))
})

test_that("el error de enganche enumera las metricas instanciadas", {
  nucleo <- metricas_nucleo()
  depto <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "DeptoDeclarado"),
    "padron", "depto"
  )
  localidad <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "LocalidadDeclarada"),
    "padron", "localidad"
  )
  medicion <- medir(
    modelo(depto, localidad),
    data.frame(depto = "Montevideo", localidad = "Montevideo")
  )
  perfil <- perfil_evaluacion(
    "Padrón",
    regla_evaluacion(
      "mayoria declarada", function(x) x == 1, "DeptoDeclarado"
    )
  )

  error <- expect_error(evaluar(medicion, perfil))
  mensaje <- conditionMessage(error)
  expect_match(mensaje, "Solicitadas: DeptoDeclarado", fixed = TRUE)
  expect_match(mensaje, "DeptoDeclarado@padron.depto", fixed = TRUE)
  expect_match(
    mensaje, "LocalidadDeclarada@padron.localidad", fixed = TRUE
  )
})
