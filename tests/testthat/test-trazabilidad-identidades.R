tipos_perfilado <- c(
  "alta_cardinalidad", "anio_de_dos_digitos", "casi_clave",
  "casi_duplicados_vocabulario", "celdas_multivaluadas",
  "ceros_no_permitidos", "codificacion_invalida", "codificacion_rota",
  "columnas_duplicadas", "controles_invisibles",
  "coordenada_fuera_dominio", "crs_no_declarado", "desviacion_benford",
  "entidades_html",
  "fecha_partida_columnas", "filas_duplicadas", "formato_fecha_ambiguo",
  "formatos_fecha_mixtos", "geometria_invalida", "geometria_vacia",
  "mayusculas_inconsistentes", "monedas_mixtas", "negativos_no_permitidos",
  "nombres_columnas_problematicos", "normalizacion_unicode",
  "numero_como_texto", "outliers", "patron_raro", "posible_identificador",
  "relacion_orden_columnas", "separadores_en_campo",
  "tipo_compuesto_no_analizado", "tipo_declarado_distinto",
  "tipos_geometria_mixtos", "unidades_mixtas", "valores_no_finitos",
  "zona_horaria_fecha_hora"
)

tipos_cubiertos <- character()

registrar_tipo <- function(tipo) {
  if (tipo %in% tipos_perfilado) {
    tipos_cubiertos <<- unique(c(tipos_cubiertos, tipo))
  }
  invisible(NULL)
}

seleccionar_hallazgo <- function(perfil, tipo, columna = NULL) {
  seleccion <- as.character(perfil$hallazgos$tipo_hallazgo) == tipo
  if (is.null(columna)) {
    seleccion <- seleccion & is.na(perfil$hallazgos$columna)
  } else {
    seleccion <- seleccion & as.character(perfil$hallazgos$columna) == columna
  }
  hallazgo <- perfil$hallazgos[seleccion, , drop = FALSE]
  expect_equal(nrow(hallazgo), 1L, info = paste(tipo, columna))
  registrar_tipo(tipo)
  hallazgo
}

esperar_filas <- function(perfil, tipo, columna, esperadas,
                          unidad = "fila") {
  hallazgo <- seleccionar_hallazgo(perfil, tipo, columna)
  expect_equal(as.character(hallazgo$unidad_conteo[[1L]]), unidad)
  expect_true(as.character(hallazgo$severidad[[1L]]) != "ok")
  expect_true(is.na(hallazgo$n_afectados[[1L]]) ||
                hallazgo$n_afectados[[1L]] > 0)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(as.character(traza$estado), "disponible")
  expect_equal(as.character(traza$alcance), "completo")
  expect_setequal(traza$indices_fila, esperadas)
  expect_equal(traza$total, length(esperadas))
  if (unidad == "fila" || unidad == "geometria") {
    expect_equal(hallazgo$n_afectados[[1L]], length(esperadas))
  }
  invisible(hallazgo)
}

esperar_unidad_no_fila <- function(perfil, tipo, columna = NULL,
                                   unidad, no_aplica = FALSE,
                                   requiere_afectados = TRUE) {
  hallazgo <- seleccionar_hallazgo(perfil, tipo, columna)
  expect_equal(as.character(hallazgo$unidad_conteo[[1L]]), unidad)
  if (requiere_afectados) {
    expect_true(is.na(hallazgo$n_afectados[[1L]]) ||
                  hallazgo$n_afectados[[1L]] > 0)
  }
  traza <- hallazgo$trazabilidad[[1L]]
  if (no_aplica) {
    expect_equal(as.character(traza$estado), "no_aplica")
    expect_equal(as.character(traza$alcance), "no_aplica")
  }
  invisible(hallazgo)
}

esperar_valores_y_filas <- function(perfil, tipo, columna, n_unidades,
                                    esperadas) {
  hallazgo <- seleccionar_hallazgo(perfil, tipo, columna)
  expect_equal(as.character(hallazgo$unidad_conteo[[1L]]), "valor_distinto")
  expect_equal(hallazgo$n_afectados[[1L]], n_unidades)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(as.character(traza$estado), "disponible")
  expect_equal(as.character(traza$alcance), "completo")
  expect_setequal(traza$indices_fila, esperadas)
  expect_equal(traza$total, length(esperadas))
  invisible(hallazgo)
}

test_that("los indices de corrupciones sembradas sobreviven al perfilado", {
  set.seed(20260818)
  vocabulario_disponible <- requireNamespace("stringdist", quietly = TRUE)
  n <- 1000L
  edades <- as.character(sample(20:70, n, replace = TRUE))
  indices_atipicos <- sort(sample(seq_len(n), 10L))
  edades[indices_atipicos] <- as.character(sample(900:999, 10L))
  ciudad <- rep("Montevideo", n)
  indices_erratas <- sort(sample(seq_len(n), 25L))
  ciudad[indices_erratas] <- sample(
    c("Montevido", "Montevideoo", "Motevideo", "Montevideo ",
      "MONTEVIDEO"), 25L, replace = TRUE
  )
  indices_mojibake <- sort(sample(setdiff(seq_len(n), indices_atipicos), 15L))
  nombre <- rep("Jose Perez", n)
  nombre[indices_mojibake] <- sample(
    c("Jos\u00c3\u00a9 P\u00c3\u00a9rez", "Jos\ufffd Perez", "Mu\u00c3\u00b1oz"),
    15L, replace = TRUE
  )
  datos <- data.frame(
    edad = edades, ciudad = ciudad, nombre = nombre,
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = vocabulario_disponible
  )

  esperar_filas(perfil, "outliers", "edad", indices_atipicos)
  esperar_filas(perfil, "patron_raro", "nombre", indices_mojibake)
  esperar_filas(perfil, "codificacion_rota", "nombre", indices_mojibake)
  esperar_valores_y_filas(
    perfil, "mayusculas_inconsistentes", "ciudad", 2L,
    which(ciudad %in% c("Montevideo", "MONTEVIDEO"))
  )
  if (vocabulario_disponible) {
    esperar_valores_y_filas(
      perfil, "casi_duplicados_vocabulario", "ciudad", 5L, seq_len(n)
    )
  }
  esperar_unidad_no_fila(
    perfil, "tipo_declarado_distinto", "edad", "columna", no_aplica = TRUE
  )
})

test_that("los diagnosticos textuales nombran sus celdas exactas", {
  n <- 100L
  indice_espacios <- c(3L, 17L)
  indice_controles <- c(5L, 19L)
  indice_html <- c(7L, 23L)
  indice_separadores <- c(11L, 29L)
  indice_mayusculas <- c(13L, 31L)
  indice_invalida <- c(37L, 41L)
  indice_rota <- c(43L, 47L)
  espacios <- rep("valor", n)
  espacios[indice_espacios] <- " valor "
  controles <- rep("hola mundo", n)
  controles[indice_controles] <- "hola\u200bmundo"
  html <- rep("Cafe", n)
  html[indice_html] <- "Ca&amp;fe"
  separadores <- rep("linea uno", n)
  separadores[indice_separadores] <- "linea uno\nlinea dos"
  mayusculas <- rep("montevideo", n)
  mayusculas[indice_mayusculas] <- "MONTEVIDEO"
  invalida <- rep("Hola", n)
  mal <- rawToChar(as.raw(c(0x48, 0x6f, 0x6c, 0x61, 0xff, 0xfe)))
  Encoding(mal) <- "UTF-8"
  invalida[indice_invalida] <- mal
  rota <- rep("Jose Perez", n)
  rota[indice_rota] <- "Jos\u00c3\u00a9 P\u00c3\u00a9rez"
  moneda <- rep("$ 100", n)
  moneda[seq(2L, n, by = 2L)] <- "USD 100"
  unidad <- rep("5 kg", n)
  unidad[seq(2L, n, by = 2L)] <- "5000 g"
  unicode <- rep("caf\u00e9", n)
  unicode[seq(2L, n, by = 2L)] <- "cafe\u0301"
  datos <- data.frame(
    espacios = espacios, controles = controles, html = html,
    separadores = separadores, mayusculas = mayusculas,
    invalida = invalida, rota = rota, moneda = moneda,
    unidad = unidad, unicode = unicode,
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE,
    sentinelas_numericos = numeric()
  )

  esperar_filas(perfil, "espacios_sobrantes", "espacios", indice_espacios)
  esperar_filas(perfil, "controles_invisibles", "controles", indice_controles)
  esperar_filas(perfil, "entidades_html", "html", indice_html)
  esperar_filas(perfil, "separadores_en_campo", "separadores", indice_separadores)
  esperar_valores_y_filas(
    perfil, "mayusculas_inconsistentes", "mayusculas", 2L, seq_len(n)
  )
  esperar_filas(perfil, "codificacion_invalida", "invalida", indice_invalida)
  esperar_filas(perfil, "codificacion_rota", "rota", indice_rota)
  esperar_filas(perfil, "numero_como_texto", "moneda", seq_len(n))
  esperar_filas(perfil, "monedas_mixtas", "moneda", seq_len(n))
  esperar_filas(perfil, "numero_como_texto", "unidad", seq_len(n))
  esperar_filas(perfil, "unidades_mixtas", "unidad", seq_len(n))
  if (requireNamespace("stringi", quietly = TRUE)) {
    esperar_valores_y_filas(
      perfil, "normalizacion_unicode", "unicode", 2L, seq_len(n)
    )
  }
})

test_that("los hallazgos numericos y temporales comparten los indices medidos", {
  n <- 100L
  atipicos <- rep("10", n)
  atipicos[c(8L, 71L)] <- c("1000", "-1000")
  no_finitos <- rep("1", n)
  no_finitos[c(12L, 35L)] <- c("Inf", "-Inf")
  ceros <- as.character(seq_len(n))
  ceros[c(14L, 28L)] <- "0"
  negativos <- as.character(seq_len(n))
  negativos[c(16L, 32L)] <- c("-1", "-2")
  perfil <- perfilar(
    data.frame(
      atipicos = atipicos, no_finitos = no_finitos,
      ceros = ceros, negativos = negativos,
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    columnas_sin_ceros = "ceros", columnas_no_negativas = "negativos",
    sentinelas_numericos = numeric(), casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(perfil, "outliers", "atipicos", c(8L, 71L))
  esperar_filas(perfil, "valores_no_finitos", "no_finitos", c(12L, 35L))
  esperar_filas(perfil, "ceros_no_permitidos", "ceros", c(14L, 28L))
  esperar_filas(perfil, "negativos_no_permitidos", "negativos", c(16L, 32L))

  clave <- c(sprintf("K%03d", seq_len(398L)), "K001", "K002")
  perfil_clave <- perfilar(
    data.frame(casi_unico = clave), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(perfil_clave, "casi_clave", "casi_unico", c(1:2, 399:400))

  momentos <- as.POSIXct(
    "2020-03-01 22:30:00", tz = "America/Montevideo"
  ) + (seq_len(n) - 1L) * 3600
  perfil_zona <- perfilar(
    data.frame(momento = momentos), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  indices_zona <- which(
    format(momentos, "%Y-%m-%d", tz = attr(momentos, "tzone")) !=
      format(momentos, "%Y-%m-%d", tz = "UTC")
  )
  esperar_filas(perfil_zona, "zona_horaria_fecha_hora", "momento", indices_zona)

  inicio <- as.Date("2020-01-01") + seq_len(n)
  fin <- inicio + 1L
  indices_orden <- c(9L, 37L)
  fin[indices_orden] <- inicio[indices_orden] - 1L
  perfil_orden <- perfilar(
    data.frame(inicio = inicio, fin = fin), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(
    perfil_orden, "relacion_orden_columnas", "inicio,fin", indices_orden
  )
})

test_that("las unidades que no son fila se declaran y no se comparan como filas", {
  n <- 600L
  d <- data.frame(
    constante = rep("unico", n),
    alta_card = paste0(
      "v", seq_len(n), "-", sample(letters, n, replace = TRUE)
    ),
    texto_alta = as.character(sample(
      c(rep(9000:9999, 3L), 1000:1999), n, replace = TRUE
    )),
    anio_corto = ifelse(seq_len(n) %% 20L == 0L, "12/03/99", "12/03/2019"),
    fecha_ambigua = ifelse(seq_len(n) %% 15L == 0L,
                           "01/02/2020", "2020-02-01"),
    anio = rep(2020, n), mes = rep(1, n), dia = rep(1, n),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  d$igual <- d$constante
  perfil <- perfilar(
    d, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )

  esperar_unidad_no_fila(
    perfil, "alta_cardinalidad", "texto_alta", "columna", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "tipo_declarado_distinto", "anio_corto", "columna", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "formato_fecha_ambiguo", "anio_corto", "formato", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "anio_de_dos_digitos", "anio_corto", "formato", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "formatos_fecha_mixtos", "anio_corto", "formato", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "fecha_partida_columnas", NULL, "columna", no_aplica = TRUE
  )
  esperar_unidad_no_fila(
    perfil, "columnas_duplicadas", "constante", "columna", no_aplica = TRUE
  )

  nombres_malos <- data.frame(a = 1:4, b = 1:4, check.names = FALSE)
  names(nombres_malos) <- c("nombre malo", "nombre malo")
  perfil_nombres <- perfilar(
    nombres_malos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_unidad_no_fila(
    perfil_nombres, "nombres_columnas_problematicos", NULL,
    "columna", no_aplica = TRUE
  )

  perfil_id <- perfilar(
    data.frame(id_denso = seq_len(300L)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_unidad_no_fila(
    perfil_id, "posible_identificador", "id_denso", "columna",
    no_aplica = TRUE
  )
})

test_that("las ausencias, duplicados y separaciones conservan sus filas", {
  faltantes <- perfilar(
    data.frame(x = c(999, 1:9)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(faltantes, "faltantes_disfrazados", "x", 1L)

  duplicadas <- data.frame(id = c(1L, 2L, 1L, 2L), valor = c("a", "b", "a", "b"))
  perfil_duplicadas <- perfilar(
    duplicadas, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(perfil_duplicadas, "filas_duplicadas", NULL, seq_len(4L))

  n <- 300L
  saltos <- rep("linea uno", n)
  indices_saltos <- seq(25L, n, by = 25L)
  saltos[indices_saltos] <- "linea uno\nlinea dos"
  perfil_saltos <- perfilar(
    data.frame(saltos = saltos), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(perfil_saltos, "separadores_en_campo", "saltos", indices_saltos)

  n <- 400L
  multi <- sprintf("AB%03d", seq_len(n))
  indices_multi <- seq(20L, n, by = 20L)
  multi[indices_multi] <- paste0(multi[indices_multi], ";AB999")
  perfil_multi <- perfilar(
    data.frame(codigo = multi), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(perfil_multi, "celdas_multivaluadas", "codigo", indices_multi)
})

test_that("Benford conserva su unidad propia y las matrices enumeran filas", {
  montos <- c(
    sample(c(9:99, 900:999, 90000:99999, 9000000:9999999), 500L, TRUE),
    sample(c(10:19, 1000:1099), 30L, TRUE)
  )
  perfil_benford <- perfilar(
    data.frame(monto = as.numeric(montos)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_unidad_no_fila(
    perfil_benford, "desviacion_benford", "monto", "valor_positivo",
    requiere_afectados = FALSE
  )

  d <- data.frame(x = seq_len(50L))
  d$matriz <- matrix(rnorm(100L), nrow = 50L, ncol = 2L)
  perfil_matriz <- perfilar(
    d, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(
    perfil_matriz, "tipo_compuesto_no_analizado", "matriz", seq_len(50L)
  )
})

test_that("las geometrias trazan exactamente sus filas afectadas", {
  skip_if_not_installed("sf")
  set.seed(20260818)
  n <- 60L
  geo <- sf::st_sfc(lapply(seq_len(n), function(i) {
    sf::st_point(c(i %% 30, i %% 20))
  }), crs = 4326)
  indices_vacias <- c(5L, 9L)
  geo[indices_vacias] <- sf::st_sfc(sf::st_polygon())[[1L]]
  indice_invalida <- 13L
  geo[indice_invalida] <- sf::st_polygon(
    list(rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)))
  )
  indice_mixta <- 17L
  geo[indice_mixta] <- sf::st_linestring(rbind(c(0, 0), c(1, 1)))
  perfil <- perfilar(
    sf::st_sf(id = seq_len(n), geometry = geo),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(
    perfil, "geometria_invalida", "geometry", indice_invalida,
    unidad = "geometria"
  )
  esperar_filas(perfil, "geometria_vacia", "geometry", indices_vacias,
               unidad = "geometria")
  esperar_filas(perfil, "tipos_geometria_mixtos", "geometry", seq_len(n),
               unidad = "geometria")
  perfil_sin_crs <- perfilar(
    sf::st_sf(id = 1:20, geometry = sf::st_sfc(lapply(1:20, function(i) {
      sf::st_point(c(i, i))
    }))), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  esperar_unidad_no_fila(
    perfil_sin_crs, "crs_no_declarado", "geometry", "columna",
    no_aplica = TRUE
  )
  puntos <- lapply(seq_len(20L), function(i) {
    sf::st_point(c(if (i %% 5L == 0L) 400 else i, i))
  })
  perfil_dominio <- perfilar(
    sf::st_sf(id = 1:20, geometry = sf::st_sfc(puntos, crs = 4326)),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  esperar_filas(
    perfil_dominio, "coordenada_fuera_dominio", "geometry",
    seq(5L, 20L, by = 5L), unidad = "geometria"
  )
})

test_that("la lista de identidades cubre los treinta y siete tipos", {
  expect_length(tipos_perfilado, 37L)
  opcionales_ausentes <- c(
    if (requireNamespace("stringdist", quietly = TRUE)) {
      character()
    } else "casi_duplicados_vocabulario",
    if (requireNamespace("stringi", quietly = TRUE)) {
      character()
    } else "normalizacion_unicode",
    if (requireNamespace("sf", quietly = TRUE)) {
      character()
    } else c(
      "coordenada_fuera_dominio", "crs_no_declarado",
      "geometria_invalida", "geometria_vacia", "tipos_geometria_mixtos"
    )
  )
  expect_setequal(
    tipos_cubiertos, setdiff(tipos_perfilado, opcionales_ausentes)
  )
})
