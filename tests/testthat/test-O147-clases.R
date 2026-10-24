# Las clases de columna por todo el catalogo. Cada prueba rehace su cifra a
# mano y falla contra el codigo anterior al arreglo.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.fecha_O147 <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("H1: una columna que no se escribe como texto entra como compuesta y no aborta", {
  # La propiedad de una `vctrs_rcrd` -`as.character()` aborta- con una clase
  # propia, sin vctrs: `[` sin metodo pierde la clase, como en muchas clases S3.
  registerS3method(
    "as.character", "o147_sin_escritura",
    function(x, ...) stop("sin escritura"), envir = baseenv()
  )
  d <- data.frame(id = 1:12, v = c(1:11, 50))
  d$z <- structure(as.numeric(1:12), class = "o147_sin_escritura")
  expect_error(as.character(d$z), "sin escritura")
  p <- perfilar(d, fecha = .fecha_O147)
  fila <- p$columnas[p$columnas$columna == "z", ]
  expect_identical(fila$estado_resumen_cuantitativo, "tipo_compuesto_no_analizado")
  expect_identical(fila$tipo_declarado, "o147_sin_escritura")
  h <- p$hallazgos[p$hallazgos$columna %in% "z", ]
  expect_identical(h$tipo_hallazgo, "tipo_compuesto_no_analizado")
  expect_equal(h$n_afectados, 12)
  expect_match(h$descripcion, "no se pueden escribir como texto", fixed = TRUE)
  # Las otras columnas, con las mismas cifras que sin ella.
  p0 <- perfilar(d[, c("id", "v")], fecha = .fecha_O147)
  campos <- c("n", "n_faltantes", "n_distintos", "media", "maximo", "n_outliers")
  expect_equal(
    p$columnas[p$columnas$columna %in% c("id", "v"), campos],
    p0$columnas[, campos], ignore_attr = TRUE
  )
  expect_equal(p0$columnas$maximo[2], max(d$v))
  # Los otros dos sitios que abortaban: el clasificador personal y la
  # identidad de las relaciones; y la funcion de vector.
  expect_no_error(clasificar_variables(d))
  r <- detectar_relaciones(d, d)
  expect_true(all(r$cardinalidad[r$columna_tabla1 == "z"] == "sin_comparar"))
  expect_identical(nrow(detectar_formatos_fecha(d$z)), 0L)
})

test_that("H2: un integer(0) o un NULL en una lista no es una ausencia estructural", {
  g <- rep(c("x", "y", "z"), 10)
  d <- data.frame(g = g)
  d$l <- lapply(1:30, function(i) if (i %% 3 == 0) integer(0) else i)
  # A mano: el perfil cuenta ausencias con is.na(), y no hay ninguna.
  expect_identical(sum(is.na(d$l)), 0L)
  p <- perfilar(d, fecha = .fecha_O147)
  expect_equal(p$columnas$n_faltantes[p$columnas$columna == "l"], 0)
  sugerida <- p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
    p$hallazgos$columna %in% "l"
  expect_false(any(sugerida))
  d$l <- lapply(1:30, function(i) if (i %% 3 == 0) NULL else i)
  p <- perfilar(d, fecha = .fecha_O147)
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
  # Control: con NA en las mismas filas la sugerencia sale, y seguirla cierra.
  d$l <- lapply(1:30, function(i) if (i %% 3 == 0) NA else i)
  p <- perfilar(d, fecha = .fecha_O147)
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural", ]
  expect_equal(h$n_afectados, 10)
  p2 <- perfilar(
    d, fecha = .fecha_O147, aplicabilidad = list(l = ~ g %in% c("x", "y"))
  )
  fila <- p2$columnas[p2$columnas$columna == "l", ]
  expect_equal(fila$n_presentes_fuera_de_aplicabilidad, 0)
  expect_false(any(p2$hallazgos$tipo_hallazgo == "valor_fuera_de_aplicabilidad"))
})

test_that("H3: analizar() mide una columna de nombre vacio o NA con ese nombre", {
  for (nombre in list("", NA_character_)) {
    d <- data.frame(a = 1:3, b = c(1, NA, 3))
    names(d)[2] <- nombre
    a <- analizar(d, fecha = .fecha_O147)
    m <- a$medicion
    fila <- m[m$metrica == "NoNulo", ]
    expect_identical(nrow(fila), 1L)
    expect_identical(fila$atributo, nombre)
    # A mano: dos de tres celdas presentes.
    expect_equal(fila$resultado, 2 / 3)
  }
  # Las otras puertas que abortaban con una columna llamada `NA`, como en
  # `perfilar()`: la puntuacion del nombre de clave y la seleccion por nombre.
  d <- data.frame(g = rep(c("a", "b"), each = 40), v = c(1:79, NA))
  names(d)[2] <- NA
  s <- sugerir_clave(d)
  expect_equal(s$parecido_nombre[is.na(s$columna)], 0)
  grupos <- perfilar_por(d, por = "g", min_filas = 10)
  expect_setequal(unique(grupos$grupo), c("a", "b"))
  # Un nombre que la tabla no tiene no se rechaza al instanciar: se declara.
  instancia <- instanciar(
    especializar(metricas_nucleo()$NoNulo), "d", atributos = "zz"
  )
  medicion <- suppressWarnings(medir(modelo(instancia), data.frame(a = 1:3)))
  expect_identical(attr(medicion, "cobertura_metricas")$estado, "no_medible")
})

test_that("H4: con nombres repetidos la traza apunta a la fila de su columna", {
  v1 <- c(1:21, 1:9)
  v1[4] <- 500
  v2 <- c(1:21, 1:9) + 0.5
  v2[10] <- 900
  d <- data.frame(x = v1, y = v2)
  names(d) <- c("x", "x")
  # A mano: el extremo de la primera esta en la fila 4 y el de la segunda en
  # la 10.
  expect_identical(which(v1 == 500), 4L)
  expect_identical(which(v2 == 900), 10L)
  p <- expect_no_warning(perfilar(d, fecha = .fecha_O147))
  h <- p$hallazgos
  extremos <- which(h$tipo_hallazgo == "outliers")
  expect_identical(
    lapply(h$trazabilidad[extremos], function(t) as.integer(t$indices_fila)),
    list(4L, 10L)
  )
  expect_identical(
    vapply(h$trazabilidad[extremos], `[[`, character(1L), "estado"),
    c("disponible", "disponible")
  )
  # El par x,x se compara entre las dos columnas, no la primera consigo misma:
  # la fila 4 es la unica donde la primera supera a la segunda.
  orden <- which(h$tipo_hallazgo == "relacion_orden_columnas")
  expect_identical(as.integer(h$trazabilidad[[orden]]$indices_fila), 4L)
  expect_identical(which(v1 > v2), 4L)
  expect_null(h$trazabilidad[[extremos[[1L]]]]$posicion_columna)
})

test_that("H4: con un nombre repetido analizar() no liga metricas a ese nombre", {
  # Las dos columnas `x` tienen faltantes: dos `NoNulo` sobre `x` eran el mismo
  # instrumento y `analizar()` abortaba entero.
  d <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, 3, 4), c = c(NA, 1, 2, 3))
  names(d) <- c("x", "x", "y")
  a <- analizar(d, fecha = .fecha_O147)
  propuesta <- a$propuesta_modelo
  en_x <- propuesta$atributos == "x"
  expect_identical(sum(en_x), 2L)
  expect_identical(unique(propuesta$estado[en_x]), "nombre_repetido")
  expect_false(any(propuesta$incluir[en_x]))
  # La que no se repite se mide: tres de cuatro celdas presentes.
  m <- a$medicion
  expect_identical(m$atributo, "y")
  expect_equal(m$resultado, 3 / 4)
})

test_that("H5: una columna sin valor comparable por fila se declara en relaciones y senales", {
  # La matriz se comparaba por sus seis elementos y publicaba 1:1 con
  # cobertura 0,5 contra tres filas.
  t1 <- data.frame(id = 1:3)
  t1$m <- matrix(1:6, 3)
  r <- detectar_relaciones(t1, data.frame(b = 1:3))
  fila <- r[r$columna_tabla1 == "m", ]
  expect_identical(fila$cardinalidad, "sin_comparar")
  expect_identical(fila$motivo_poda, "columna_no_comparable")
  expect_true(is.na(fila$cobertura_tabla1_en_tabla2))
  # Senales: una lista de vectores, una de NULL y una matriz abortaban o
  # culpaban a una transformacion que nadie declaro.
  for (columna in list(
    list(1:2, 3L, 4:5), rep(list(NULL), 3), matrix(1:6, 3)
  )) {
    d <- data.frame(id = 1:3)
    d$z <- columna
    s <- detectar_discordancias(d, senal_redundante(c("id", "z")))
    expect_identical(s$n_evaluadas, 0)
    expect_true(is.na(s$n_discordantes))
    expect_match(s$evidencia, "no se evaluo", fixed = TRUE)
  }
  # Control: una lista de escalares se compara (3 contra 4 en la fila 3).
  d <- data.frame(id = 1:3)
  d$z <- list(1L, 2L, 4L)
  s <- detectar_discordancias(d, senal_redundante(c("id", "z")))
  expect_identical(s$n_discordantes, 1)
})

test_that("H5: una magnitud con unidad no hace abortar relaciones ni senales", {
  skip_if_not_installed("units")
  u <- units::set_units(1:3, "m")
  r <- detectar_relaciones(data.frame(a = u), data.frame(b = 1:3))
  expect_identical(r$cardinalidad, "sin_comparar")
  expect_identical(r$motivo_poda, "columna_no_comparable")
  expect_match(attr(r, "podas")$detalle, "unidad", fixed = TRUE)
  s <- detectar_discordancias(
    data.frame(a = u, b = u), senal_redundante(c("a", "b"))
  )
  expect_identical(s$n_evaluadas, 0)
  # Con una transformacion a numero, se compara: 0 discordantes en 3.
  s <- detectar_discordancias(
    data.frame(a = u, b = u),
    senal_redundante(
      c("a", "b"),
      transformacion = list(a = as.numeric, b = as.numeric)
    )
  )
  expect_identical(c(s$n_evaluadas, s$n_discordantes), c(3, 0))
})

test_that("H6: el nivel NA de un factor es un faltante en las cinco lecturas", {
  n <- 40L
  f <- factor(rep(c("a", "b", NA), length.out = n), exclude = NULL)
  d <- data.frame(
    id = seq_len(n),
    flag = c(rep(c(TRUE, FALSE, TRUE), length.out = n - 1L), NA)
  )
  d$f <- f
  # A mano: 13 celdas en el nivel NA, 27 con valor; la fila 40 tiene `f`
  # presente y `flag` ausente, asi que hay 26 pares presentes.
  ausentes <- is.na(as.character(f))
  expect_identical(sum(ausentes), 13L)
  expect_identical(sum(!ausentes & !is.na(d$flag)), 26L)
  p <- perfilar(d, fecha = .fecha_O147)
  fila <- p$columnas[p$columnas$columna == "f", ]
  expect_equal(fila$n_faltantes, 13)
  expect_equal(fila$n_distintos, 2)
  expect_true(any(p$hallazgos$tipo_hallazgo == "faltantes" &
                    p$hallazgos$columna %in% "f"))
  dep <- detectar_dependencias(d)
  expect_equal(dep$n_evaluados[dep$determinante == "f"], 26)
  dv <- distribucion_valores(d)
  expect_equal(dv$alcance$n_analizados[dv$alcance$columna == "f"], 27)
  expect_equal(inferir_tipo(f)$n_analizados, 27)
  propuesta <- proponer_modelo(p, d)
  no_nulo <- propuesta$metrica == "NoNulo" &
    vapply(propuesta$atributos_ligados, identical, logical(1L), "f")
  expect_true(any(no_nulo))
  # La cadena: marcar las ausentes marca las 13, y el factor no cambia.
  plan <- planificar_limpieza(p, d)
  plan$aplicar <- as.character(plan$estrategia) == "marcar_filas_ausentes" &
    plan$columna %in% "f"
  r <- aplicar(plan, d)
  expect_identical(sum(r$datos$.ausente_f), 13L)
  expect_identical(levels(r$datos$f), levels(f))
})

test_that("H7: un byte no es un texto escrito y el plan no ofrece lo que no ejecuta", {
  d <- data.frame(id = 1:40)
  d$b <- as.raw(0:39)
  # A mano: 12 bytes tienen una letra hexadecimal (0a-0f, 1a-1f); son bytes.
  expect_identical(sum(grepl("[a-f]", as.character(d$b))), 12L)
  p <- perfilar(d, fecha = .fecha_O147)
  expect_false(any(p$hallazgos$columna %in% "b"))
  expect_equal(p$columnas$n_distintos[p$columnas$columna == "b"], 40)
  expect_false(any(planificar_limpieza(p, d)$columna %in% "b"))
  # La regla del plan: una conversion que se probo sobre los datos y no se
  # pudo ejecutar no sale `lista`.
  texto <- data.frame(id = 1:40, b = as.character(d$b), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(texto, fecha = .fecha_O147), d)
  w <- as.character(plan$estrategia) == "convertir_numero_regional"
  expect_identical(as.character(plan$estado[w]), "bloqueada")
  expect_false(plan$aplicar[w])
})

test_that("H8: la misma columna como POSIXlt y como POSIXct da la misma fila", {
  ct <- as.POSIXct("2024-01-01", tz = "UTC") + 86400 * c(rep(0, 10), 1:20)
  # A mano: el primer instante se repite 10 veces.
  expect_identical(max(table(format(ct))), 10L)
  d1 <- data.frame(id = 1:30)
  d1$t <- ct
  d2 <- d1
  d2$t <- as.POSIXlt(ct)
  f1 <- perfilar(d1, fecha = .fecha_O147)$columnas
  f2 <- perfilar(d2, fecha = .fecha_O147)$columnas
  expect_equal(f2$frecuencia_moda[2], 10)
  distintos <- names(f1)[!mapply(identical, as.list(f1[2, ]), as.list(f2[2, ]))]
  expect_identical(distintos, character())
})

test_that("H9: la divergencia de una columna SQLite de tipo mixto lleva su causa", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE m (id INTEGER, v INTEGER)")
  DBI::dbExecute(con, paste(
    "INSERT INTO m VALUES (1, 10), (2, 20), (3, 'abc'), (4, 40), (5, 50),",
    "(6, 60), (7, 'abc'), (8, 80)"
  ))
  # A mano: dos valores guardados como texto en una columna INTEGER.
  tipos <- DBI::dbGetQuery(con, "SELECT SUM(typeof(v) = 'text') AS n FROM m")
  expect_equal(tipos$n, 2)
  pd <- suppressWarnings(perfilar_dbi(con, "m"))
  cobertura <- pd$resumen_tabla$cobertura
  divergencias <- cobertura[cobertura$estado == "divergencia", ]
  expect_true(nrow(divergencias) > 0L)
  expect_true(all(grepl("Causa medida", divergencias$motivo, fixed = TRUE)))
  # Control: la misma tabla sin texto no diverge.
  DBI::dbExecute(con, "UPDATE m SET v = id * 10")
  pd <- suppressWarnings(perfilar_dbi(con, "m"))
  cobertura <- pd$resumen_tabla$cobertura
  expect_false(any(cobertura$estado == "divergencia", na.rm = TRUE))
})
