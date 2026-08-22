.tablas_limpias_r107 <- function(n = 40L) {
  envolver <- function(valor, nombre = "valor") {
    salida <- data.frame(
      clave = sprintf("R%06d", seq_along(valor)),
      stringsAsFactors = FALSE
    )
    salida[[nombre]] <- valor
    salida
  }
  repetir <- function(x) rep(x, length.out = n)
  fechas <- as.Date("2024-01-01") + seq_len(n) - 1L
  fecha_hora <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") +
    seq_len(n) * 3600

  set.seed(10701)
  lognormal <- stats::rlnorm(80L, meanlog = 0, sdlog = 1)
  set.seed(10702)
  continua <- stats::runif(n, -10, 10)

  list(
    claves = data.frame(clave = sprintf("R%06d", seq_len(n))),
    nombres_con_coma = envolver(repetir(c(
      "P\u00e9rez, Ana", "Silva, Bruno", "N\u00fa\u00f1ez, Clara", "Sosa, Diego"
    )), "nombre"),
    direcciones = envolver(repetir(c(
      "18 de Julio 1234", "Colonia 456", "Yi 789", "Rivera 2345"
    )), "direccion"),
    decimales_con_coma = envolver(
      sub("\\.", ",", sprintf("%.2f", seq(10.25, length.out = n, by = 0.5))),
      "importe"
    ),
    fechas_como_texto = envolver(format(fechas, "%Y-%m-%d"), "fecha_texto"),
    texto_libre = envolver(
      paste("Observacion valida de la fila", letters[(seq_len(n) - 1L) %% 26L + 1L],
            LETTERS[(seq_len(n) * 7L - 1L) %% 26L + 1L], seq_len(n)),
      "comentario"
    ),
    moneda_unica = envolver(sprintf("$ %d", 100L + seq_len(n)), "precio"),
    unidad_unica = envolver(sprintf("%d kg", 10L + seq_len(n)), "peso"),
    identificadores_correlativos = envolver(
      sprintf("EXP-%04d-A", seq_len(n)), "expediente"
    ),
    codigos_postales = envolver(
      sprintf("%05d", as.integer(seq(100, 4000, length.out = n))),
      "codigo_postal"
    ),
    telefonos = envolver(
      sprintf("+598 9%07d", 1000000L + seq_len(n)), "telefono"
    ),
    correos = envolver(
      sprintf("persona%03d@example.org", seq_len(n)), "correo"
    ),
    urls = envolver(
      sprintf("https://example.org/recurso/%03d", seq_len(n)),
      "url"
    ),
    booleanos_texto = envolver(repetir(c("si", "no")), "vigente"),
    categorias_con_tildes = envolver(
      repetir(c("tr\u00e1mite", "revisi\u00f3n", "aprobaci\u00f3n", "archivo")),
      "estado"
    ),
    continuas = envolver(continua, "medicion"),
    enteros_acotados = envolver(repetir(1:5), "nivel"),
    fecha_hora = envolver(fecha_hora, "instante"),
    fechas = envolver(fechas, "fecha"),
    factores = envolver(factor(repetir(c("norte", "sur", "este", "oeste"))),
                         "region"),
    logicos = envolver(repetir(c(TRUE, FALSE)), "activo"),
    faltantes_declarados = envolver(
      replace(seq_len(n), c(8L, 24L), NA_integer_), "conteo"
    ),
    mixta_realista = data.frame(
      clave = sprintf("R%06d", seq_len(n)),
      departamento = repetir(c("Artigas", "Canelones", "Maldonado", "Rocha")),
      casos = repetir(c(12L, 7L, 19L, 4L, 15L)),
      tasa = continua,
      vigente = repetir(c(TRUE, FALSE)),
      stringsAsFactors = FALSE
    ),
    nombres_persona = envolver(
      repetir(c("Ana P\u00e9rez", "Bruno Silva", "Clara N\u00fa\u00f1ez", "Diego Sosa")),
      "persona"
    ),
    ciudades = envolver(
      repetir(c("Montevideo", "Salto", "Paysand\u00fa", "Melo")), "ciudad"
    ),
    factor_ordenado = envolver(
      ordered(repetir(c("bajo", "medio", "alto")),
              levels = c("bajo", "medio", "alto")),
      "prioridad"
    ),
    porcentajes_texto = envolver(
      sprintf("%d %%", 10L + seq_len(n)), "porcentaje"
    ),
    versiones = envolver(
      sprintf("v%d.%d.%d", 1L + (seq_len(n) %% 3L),
              seq_len(n) %% 10L, seq_len(n) %% 7L),
      "version"
    ),
    redes = envolver(
      sprintf("192.0.2.%d", seq_len(n)), "direccion_ip"
    ),
    documentos = envolver(
      sprintf("DOC-%08d", 10000000L + seq_len(n)), "documento"
    ),
    lognormal = envolver(lognormal, "duracion")
  )
}

test_that("31 tablas limpias tienen solo afirmaciones verdaderas", {
  # Estos tres numeros son los que publica la tabla de evidencia del README:
  # 31 tablas de control, 0 hallazgos de severidad error, 8 senales para
  # revisar. Se fijan aca a proposito.
  #
  # Antes decia 43 tablas y 25 senales, de cuando el conjunto era mas grande y
  # el paquete hacia mas ruido. El generador se redujo, el ruido bajo a 8, y el
  # README siguio publicando los viejos porque ninguna prueba los ataba. Un
  # numero publicado que ninguna prueba vigila se vuelve mentira sin que nadie
  # se entere; que este fallando aca es la unica forma de que eso no pase otra
  # vez.
  tablas <- .tablas_limpias_r107()
  expect_equal(length(tablas), 31L)

  perfiles <- lapply(tablas, perfilar, analizar_dependencias = FALSE)
  nombres <- rep(names(perfiles), vapply(perfiles, function(x) {
    nrow(x$hallazgos)
  }, integer(1L)))
  hallazgos <- do.call(rbind, lapply(perfiles, `[[`, "hallazgos"))
  hallazgos$tabla <- nombres

  expect_false(any(as.character(hallazgos$severidad) == "error"))
  observados <- hallazgos[as.character(hallazgos$severidad) != "ok", ]
  # Las "senales para revisar" del README son exactamente estas.
  expect_equal(nrow(observados), 8L)
  afirmaciones <- sort(paste(
    observados$tabla, observados$columna, observados$tipo_hallazgo, sep = "::"
  ))
  esperadas <- sort(c(
    "booleanos_texto::vigente::tipo_declarado_distinto",
    "decimales_con_coma::importe::numero_como_texto",
    "decimales_con_coma::importe::tipo_declarado_distinto",
    "fechas_como_texto::fecha_texto::tipo_declarado_distinto",
    "lognormal::duracion::outliers",
    "moneda_unica::precio::numero_como_texto",
    "porcentajes_texto::porcentaje::numero_como_texto",
    "unidad_unica::peso::numero_como_texto"
  ))
  expect_equal(afirmaciones, esperadas)
})

test_that("un resultado negativo de vocabulario queda en ok", {
  skip_if_not_installed("stringdist")
  perfil <- perfilar(
    data.frame(clave = sprintf("R%06d", seq_len(300L))),
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "ok")
  expect_equal(hallazgo$n_afectados, 0)
  expect_match(hallazgo$evidencia, "grupos: 0", fixed = TRUE)
  expect_match(hallazgo$evidencia, "8193 pares cercanos", fixed = TRUE)
})

test_that("variantes reales de vocabulario siguen siendo sospechosas", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(localidad = c(
    rep("Montevideo", 30L), rep("Montevido", 3L), rep("MONTEVIDEO", 5L)
  ))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(as.character(hallazgo$severidad), "sospechoso")
  expect_match(hallazgo$evidencia, "Montevideo", fixed = TRUE)
  expect_match(hallazgo$evidencia, "Montevido", fixed = TRUE)
  expect_match(hallazgo$evidencia, "MONTEVIDEO", fixed = TRUE)
  expect_match(hallazgo$evidencia, "grupos: 1", fixed = TRUE)
})

test_that("el solapamiento separa escala de relacion fila a fila", {
  set.seed(1)
  espurio <- data.frame(
    mayor = stats::runif(300L, 100, 9000),
    menor = stats::runif(300L, 10, 900)
  )
  perfil_espurio <- perfilar(espurio, analizar_dependencias = FALSE)
  expect_false(any(
    perfil_espurio$hallazgos$tipo_hallazgo == "relacion_orden_columnas"
  ))
  expect_equal(perfil_espurio$meta$orden_columnas$pares_descartados_magnitud, 1)
  expect_equal(
    perfil_espurio$meta$orden_columnas$pares_rescatados_brecha_estable, 0
  )

  inicio <- seq(100, 900, length.out = 300L)
  fin <- inicio + 10
  fin[seq_len(6L)] <- inicio[seq_len(6L)] - 1
  perfil_real <- perfilar(
    data.frame(inicio = inicio, fin = fin), analizar_dependencias = FALSE
  )
  hallazgo <- perfil_real$hallazgos[
    perfil_real$hallazgos$tipo_hallazgo == "relacion_orden_columnas", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$n_afectados, 6)
  expect_match(hallazgo$evidencia, "Solapamiento intercuartil: 0.975")
  expect_match(hallazgo$evidencia, "umbral: 0.100")
  expect_match(hallazgo$evidencia, "IQR de la brecha:")
  expect_match(hallazgo$evidencia, "criterio alternativo: 0.000")
})

test_that("la unicidad completa no se afirma como categoria", {
  correos <- sprintf("persona%03d@example.org", seq_len(40L))
  perfil_unico <- perfilar(
    data.frame(correo = correos), analizar_dependencias = FALSE
  )
  expect_false(any(
    perfil_unico$hallazgos$tipo_hallazgo == "alta_cardinalidad"
  ))

  con_repeticion <- c(correos[seq_len(30L)], rep(correos[[1L]], 10L))
  perfil_categorico <- perfilar(
    data.frame(categoria = con_repeticion), analizar_dependencias = FALSE
  )
  expect_true(any(
    perfil_categorico$hallazgos$tipo_hallazgo == "alta_cardinalidad"
  ))
})

test_that("una tabla ajena conserva el perfil base identico", {
  skip_if_not_installed("stringdist")
  set.seed(10703)
  etiquetas <- replicate(24L, paste(sample(letters, 20L, replace = TRUE),
                                    collapse = ""))
  datos <- data.frame(
    etiqueta = etiquetas,
    grupo = rep(c("a", "b", "c"), length.out = 24L),
    stringsAsFactors = FALSE
  )
  fecha <- as.POSIXct("2026-08-16 12:00:00", tz = "UTC")
  perfil_base <- perfilar(datos, fecha = fecha, analizar_dependencias = FALSE)

  local_mocked_bindings(
    .hallazgos_casi_duplicados_vocabulario = function(...) {
      salida <- list()
      attr(salida, "cobertura_diagnosticos") <-
        lupa:::.cobertura_diagnosticos_vacia()
      salida
    },
    .package = "lupa"
  )
  perfil_sin_cambio <- perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE
  )
  expect_identical(perfil_base, perfil_sin_cambio)
})
