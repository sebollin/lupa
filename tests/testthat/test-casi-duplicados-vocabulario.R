test_that("las variantes que la normalizacion funde quedan nombradas", {
  datos <- data.frame(
    localidad = c(
      rep("San José", 20), rep("San Jose", 5),
      rep("Montevideo", 20), rep("MONTEVIDEO", 3), rep("Montevido", 2),
      rep("Canelones", 10), rep("Canelónes", 2), rep("Canelone", 1),
      "sin_variantes"
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, duplicados_aproximados = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "San José")
  expect_match(hallazgo$evidencia, "San Jose")
  expect_match(hallazgo$evidencia, "\\(20\\)")
  expect_match(hallazgo$evidencia, "\\(5\\)")
  expect_match(hallazgo$evidencia, "origen=normalizacion")
  expect_false(grepl("sin_variantes", hallazgo$evidencia, fixed = TRUE))
  expect_equal(hallazgo$unidad_conteo, "valor_distinto")
  grupos <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos$localidad, lupa:::.resolver_normalizacion(TRUE), "localidad"
  )$grupos
  asimetrias <- vapply(grupos, `[[`, numeric(1L), "asimetria")
  expect_true(all(diff(asimetrias) <= 0))
})

test_that("los grupos usan el vocabulario y no la cantidad de filas", {
  perfil <- lupa:::.resolver_normalizacion(TRUE)
  corto <- c("San José", "San Jose", "Canelones", "Canelónes", "aislado")
  largo <- c(
    rep("San José", 100), rep("San Jose", 4),
    rep("Canelones", 80), rep("Canelónes", 2), rep("aislado", 60)
  )
  grupos_corto <- lupa:::.grupos_casi_duplicados_vocabulario(
    corto, perfil, "x"
  )$grupos
  grupos_largo <- lupa:::.grupos_casi_duplicados_vocabulario(
    largo, perfil, "x"
  )$grupos
  formas <- function(grupos) {
    salida <- lapply(grupos, function(x) sort(x$variantes))
    salida[order(vapply(salida, function(x) paste(x, collapse = "|"),
                            character(1L)))]
  }
  expect_equal(formas(grupos_corto), formas(grupos_largo))
  expect_equal(
    sort(unname(unlist(lapply(grupos_largo, `[[`, "frecuencias")))),
    sort(c(100L, 4L, 80L, 2L))
  )
})

test_that("un vocabulario sin variantes no recibe grupos", {
  set.seed(7801)
  valores <- replicate(300, paste(sample(letters, 18, replace = TRUE),
                                  collapse = ""))
  valores <- make.unique(valores)
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    valores, lupa:::.resolver_normalizacion(TRUE), "x"
  )
  expect_length(resultado$grupos, 0L)
})

test_that("sin stringdist se conservan las fusiones exactas y se declara la ausencia", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    c("San José", "San Jose", "aislado"),
    lupa:::.resolver_normalizacion(TRUE), "x"
  )
  expect_length(resultado$grupos, 1L)
  expect_equal(sort(resultado$grupos[[1L]]$variantes), sort(c("San José", "San Jose")))
  expect_false(resultado$alcance$distancia_disponible)
  expect_match(resultado$alcance$motivo_distancia, "stringdist")
})

test_that("el alcance declara un recorte del vocabulario o de sus pares", {
  valores <- paste0("v", seq_len(40))
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    valores, lupa:::.resolver_normalizacion(TRUE), "x",
    max_valores = 10L, max_pares = 10L
  )
  expect_true(resultado$alcance$truncado)
  expect_equal(resultado$alcance$n_valores_evaluados, 10L)
  expect_true(resultado$alcance$n_pares_sin_comparar > 0)
})

test_that("la distancia no encadena variantes en un solo grupo", {
  datos <- c(
    rep("Marano", 20), rep("Marabo", 5), rep("Marebo", 5),
    rep("Marebe", 5), rep("Karebe", 5)
  )
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos, lupa:::.resolver_normalizacion(FALSE), "x"
  )
  grupos <- resultado$grupos
  contiene <- vapply(grupos, function(grupo) {
    all(c("Marano", "Marebo") %in% grupo$variantes)
  }, logical(1L))
  expect_gt(length(grupos), 0L)
  expect_false(any(contiene))
  if (length(grupos)) {
    expect_true(all(vapply(grupos, function(grupo) {
      is.finite(grupo$distancia_minima) &&
        is.finite(grupo$distancia_maxima)
    }, logical(1L))))
  }
})

test_that("el piso informa un grupo real en vocabularios pequenos", {
  construir <- function(n) {
    c("Montevideo", "MONTEVIDEO", "Montevideo ",
      paste0("aislado", seq_len(n - 3L)))
  }
  for (n in 3:12) {
    resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
      construir(n), lupa:::.resolver_normalizacion(TRUE), "x"
    )
    expect_true(resultado$alcance$limite_aplicado == (n >= 20L))
    expect_true(any(vapply(resultado$grupos, function(grupo) {
      all(c("Montevideo", "MONTEVIDEO", "Montevideo ") %in%
        grupo$variantes)
    }, logical(1L))))
  }
})

test_that("un componente grande no se presenta como toda una columna chica", {
  datos <- rep(paste0("Zona ", LETTERS[seq_len(15)]),
               times = seq(100, 72, by = -2))
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos, lupa:::.resolver_normalizacion(FALSE), "zona"
  )
  expect_false(resultado$alcance$aplicable)
  expect_true(resultado$alcance$limite_aplicado)
  expect_equal(resultado$alcance$min_tamano_grupo_limite, 10L)
  expect_equal(resultado$alcance$tamano_grupo_maximo, 15L)
  expect_length(resultado$grupos, 0L)
  perfil <- perfilar(
    data.frame(zona = datos), analizar_dependencias = FALSE,
    normalizar = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "grupo_maximo: 15")
  expect_match(hallazgo$descripcion, "no aplica")

  # El mismo grupo grande no se bloquea por su tamano cuando comparte la
  # columna con un vocabulario mucho mayor y su proporcion es pequena.
  relleno <- paste0("aislado", seq_len(100))
  resultado_amplio <- lupa:::.grupos_casi_duplicados_vocabulario(
    c(datos, relleno), lupa:::.resolver_normalizacion(FALSE), "zona"
  )
  expect_true(resultado_amplio$alcance$aplicable)
  expect_true(any(vapply(resultado_amplio$grupos, function(grupo) {
    length(grupo$variantes) == 15L
  }, logical(1L))))
})

test_that("las secuencias numericas separan familias de entidades", {
  zonas <- function(n) paste0("Zona ", sprintf("%02d", seq_len(n)))
  for (n in c(9L, 15L)) {
    resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
      zonas(n), lupa:::.resolver_normalizacion(FALSE), "zona"
    )
    expect_length(resultado$grupos, 0L)
  }
  resultado_mixto <- lupa:::.grupos_casi_duplicados_vocabulario(
    c(zonas(15L), paste0("Localidad ", sprintf("%02d", seq_len(25L)))),
    lupa:::.resolver_normalizacion(FALSE), "ubicacion"
  )
  expect_length(resultado_mixto$grupos, 0L)
  perfil_zonas <- perfilar(
    data.frame(zona = rep(zonas(15L), times = seq(100, 72, by = -2)),
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, normalizar = FALSE
  )
  hallazgo_zonas <- perfil_zonas$hallazgos[
    perfil_zonas$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo_zonas), 1L)
  expect_match(hallazgo_zonas$evidencia,
               "descartados por secuencia numerica")
  expect_false(grepl("No se entrega el grupo mayor",
                     hallazgo_zonas$evidencia, fixed = TRUE))
  expect_match(hallazgo_zonas$descripcion,
               "diferencias numericas se consideran entidades distintas")

  ruta <- lupa:::.grupos_casi_duplicados_vocabulario(
    c(rep("Ruta 5", 100), rep("Ruta 05", 10)),
    lupa:::.resolver_normalizacion(FALSE), "ruta"
  )
  expect_true(any(vapply(ruta$grupos, function(grupo) {
    all(c("Ruta 5", "Ruta 05") %in% grupo$variantes)
  }, logical(1L))))

  pares_permitidos <- list(
    c("Calle 18 esquina 5", "Calle 18 esquina 05"),
    c("1.000", "1000")
  )
  for (par in pares_permitidos) {
    resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
      c(rep(par[[1L]], 20), rep(par[[2L]], 5)),
      lupa:::.resolver_normalizacion(FALSE), "x"
    )
    expect_gt(length(resultado$grupos), 0L)
  }

  pares_rechazados <- list(
    c("Escuela N 0102", "Escuela N 0103"),
    c("Ruta 5", "Ruta V")
  )
  for (par in pares_rechazados) {
    resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
      c(rep(par[[1L]], 20), rep(par[[2L]], 5)),
      lupa:::.resolver_normalizacion(FALSE), "x"
    )
    expect_length(resultado$grupos, 0L)
  }
})

test_that("la regla numerica distingue las diez parejas de referencia", {
  parejas <- data.frame(
    izquierda = c(
      "Zona 01", "Escuela N 0001", "Calle 18", "Montevideo",
      "Montevideo", "Canelones", "San José", "Maldonado",
      "Ruta 5", "Piso 1"
    ),
    derecha = c(
      "Zona 02", "Escuela N 0002", "Calle 8", "Montevideoz",
      "Montevido", "Canelónes", "San Jose", "Maldonado.",
      "Ruta 05", "Piso 01"
    ),
    esperado = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                 TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(parejas))) {
    resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
      c(rep(parejas$izquierda[[i]], 20L),
        rep(parejas$derecha[[i]], 5L)),
      lupa:::.resolver_normalizacion(FALSE), "x"
    )
    expect_equal(length(resultado$grupos) > 0L, parejas$esperado[[i]],
                 info = paste(parejas$izquierda[[i]], "/",
                              parejas$derecha[[i]]))
  }
})

test_that("un grupo que abarca casi todo el vocabulario no se entrega", {
  letras <- vapply(seq_len(2000), function(i) {
    paste0(
      letters[(i - 1L) %/% 676L + 1L],
      letters[((i - 1L) %/% 26L) %% 26L + 1L],
      letters[(i - 1L) %% 26L + 1L]
    )
  }, character(1L))
  valores <- paste0(strrep("x", 80), letras)
  datos <- c(rep(valores[[1L]], 10L), valores[-1L])
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos, lupa:::.resolver_normalizacion(FALSE), "x"
  )
  expect_false(resultado$alcance$aplicable)
  expect_gt(resultado$alcance$proporcion_grupo_maximo, 0.5)
  expect_length(resultado$grupos, 0L)
  perfil <- perfilar(
    data.frame(escuela = datos), analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$descripcion, "no aplica")
  expect_match(hallazgo$evidencia, "grupo_maximo")
})

test_that("el detector de vocabulario se puede apagar", {
  datos <- data.frame(x = c("San José", "San Jose", "aislado"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_false(any(
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario"
  ))
  expect_false(perfil$meta$casi_duplicados_vocabulario)
})

test_that("declara cuando la estrella no tiene asimetria", {
  datos <- data.frame(x = c("Marano", "Marabo", "Maravo"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, normalizar = TRUE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "sin_asimetria")
  expect_match(hallazgo$sugerencia, "detectar_duplicados_aproximados")
  expect_match(hallazgo$evidencia, "pares cercanos")
  expect_true(is.na(hallazgo$n_afectados))
})

test_that("la ceguera de la estrella no oculta fusiones exactas", {
  datos <- data.frame(x = c("San José", "San Jose", "Marano", "Marabo"))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, normalizar = TRUE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(hallazgo$evidencia, "San José")
  expect_match(hallazgo$evidencia, "San Jose")
  expect_match(hallazgo$evidencia, "motivo_grupos=sin_asimetria")
})

test_that("la distancia en nombres de vias queda declarada como sospecha", {
  datos <- c(
    rep("CAMINO CARRASCO", 30L), rep("CAMINO AGRARIOS", 2L),
    rep("CALLE A", 20L), rep("CALLE B", 3L),
    rep("UYCASJC_SN_030", 20L), rep("UYCAEPR_SN_030", 3L),
    rep("UYCAANO_SN_030", 2L),
    rep("Montevideo", 30L), rep("Montevido", 2L),
    rep("San José", 20L), rep("San Jose", 5L)
  )
  resultado <- lupa:::.grupos_casi_duplicados_vocabulario(
    datos, lupa:::.resolver_normalizacion(TRUE), "nombre"
  )
  grupos <- resultado$grupos
  expect_true(any(vapply(grupos, function(g) {
    all(c("CAMINO CARRASCO", "CAMINO AGRARIOS") %in% g$variantes)
  }, logical(1L))))
  expect_true(any(vapply(grupos, function(g) {
    all(c("Montevideo", "Montevido") %in% g$variantes)
  }, logical(1L))))
  expect_true(any(vapply(grupos, function(g) {
    all(c("CALLE A", "CALLE B") %in% g$variantes)
  }, logical(1L))))
  expect_true(any(vapply(grupos, function(g) {
    all(c("San José", "San Jose") %in% g$variantes) &&
      grepl("normalizacion", g$origen, fixed = TRUE)
  }, logical(1L))))
  perfil <- perfilar(
    data.frame(nombre = datos), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_match(hallazgo$evidencia, "origen=distancia")
  expect_match(hallazgo$descripcion, "distancia es heurística")
  expect_match(hallazgo$descripcion, "no confirma identidad")
  expect_match(hallazgo$sugerencia, "distancia no confirma identidad")
})
