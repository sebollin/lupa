# O180: ronda 39-P, los tres altos de la refutacion de la proteccion tras la
# ronda 38, los tres de antes (igual en f6a5ce5 y en 38a1efa). H-08: la sigla
# del documento con puntos -el encabezado `C.I.` de una planilla uruguaya- se
# normalizaba `c_i_` y no se reconocia, y las cedulas sin digito verificador se
# publicaban. H-17: la fecha de la nomenclatura GIK de GeneXus -`PerFchNac`,
# `PerFchDef`- no se reconocia y publicaba la moda y los extremos de la fecha de
# nacimiento. H-01: la inicial `a`, `o` o `u` de un login `a_perez` se leia
# como la preposicion y la columna de quien actua publicaba los logins de todos.
# Cada bloque rehace la cifra a mano y falla contra 38a1efa, salvo los de la
# cara 2, que vigilan que el arreglo no tape de mas.

# Los textos y los numeros de todas las hojas de un objeto, y su impresion.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r39p_textos <- function(x, imprimir = FALSE) {
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (inherits(y, "Date")) {
      textos <<- c(textos, format(y))
    } else if (is.numeric(y)) {
      y <- as.numeric(y)
      textos <<- c(textos, format(y[!is.na(y)], scientific = FALSE, trim = TRUE,
                                  digits = 15))
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    a <- attributes(y)
    a <- a[setdiff(names(a), c("names", "row.names", "class", "dim",
                               "dimnames", "levels"))]
    if (length(a)) recorrer(a)
    invisible()
  }
  recorrer(unclass(x))
  if (imprimir) {
    # La impresion sale casi toda como mensajes de cli: se recogen las
    # condiciones, no solo el flujo de salida.
    mensajes <- character()
    salida <- withCallingHandlers(
      utils::capture.output(print(x)),
      message = function(m) {
        mensajes <<- c(mensajes, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    textos <- c(textos, salida, mensajes)
  }
  textos[!is.na(textos)]
}

# Cuantos de `valores` aparecen en `x`, enteros: no dentro de un numero mas
# largo (una duracion en milisegundos puede contener siete cifras de una
# cedula).
.r39p_apariciones <- function(x, valores, imprimir = FALSE) {
  textos <- .r39p_textos(x, imprimir = imprimir)
  valores <- unique(as.character(valores))
  sum(vapply(valores, function(v) {
    patron <- paste0("(^|[^0-9])", gsub(".", "\\.", v, fixed = TRUE),
                     "($|[^0-9])")
    any(grepl(patron, textos, perl = TRUE, useBytes = TRUE))
  }, logical(1L)))
}

.r39p_clasificacion <- function(dp, columna) {
  k <- which(dp$columna == columna)
  if (length(k) != 1L) return("no")
  paste0(dp$tipo[k], ":", dp$proteger[k])
}

# Las celdas de `p$columnas` de `columna` que difieren de las de la corrida
# sin proteccion.
.r39p_celdas_tapadas <- function(p1, p0, columna) {
  c1 <- p1$columnas[p1$columnas$columna == columna, , drop = FALSE]
  c0 <- p0$columnas[p0$columnas$columna == columna, , drop = FALSE]
  comunes <- intersect(names(c1), names(c0))
  comunes[vapply(comunes, function(k) {
    !identical(format(c1[[k]]), format(c0[[k]]))
  }, logical(1L))]
}

# Cedulas de siete cifras sin digito verificador, distintas, desde `desde`.
.r39p_cedulas <- function(n, desde = 0L) {
  as.character(1500000L + ((seq_len(n) + desde) * 7919L) %% 5000000L)
}

.r39p_con_puntos <- function(x) {
  sub("^([0-9])([0-9]{3})([0-9]{3})$", "\\1.\\2.\\3", x, perl = TRUE)
}

test_that("H-08: la sigla del documento con puntos o guion bajo -C.I.- se reconoce y las cedulas sin verificador no se publican", {
  n <- 120L
  encabezados <- c("C.I.", "C.I", "c.i.", "C. I.", "Nro. C.I.",
                   "C.I. Titular", "C.I. N\u00ba", "N\u00b0 C.I.",
                   "N\u00ba de C.I.", "c_i", "nro_c_i", "D.N.I.")
  for (escritura in c("texto", "puntos", "entero")) {
    d <- data.frame(id = seq_len(n))
    valores <- list()
    for (j in seq_along(encabezados)) {
      ced <- .r39p_cedulas(n, desde = j * 1000L)
      v <- switch(escritura, texto = ced, puntos = .r39p_con_puntos(ced),
                  entero = as.integer(ced))
      d[[encabezados[[j]]]] <- v
      valores[[j]] <- as.character(v)
    }
    p <- suppressMessages(perfilar(d))
    for (j in seq_along(encabezados)) {
      h <- encabezados[[j]]
      expect_identical(.r39p_clasificacion(p$datos_personales, h),
                       "documento_identidad:TRUE",
                       info = paste(escritura, h))
      expect_identical(.r39p_apariciones(p, valores[[j]], imprimir = TRUE), 0L,
                       info = paste(escritura, h))
    }
  }
})

test_that("H-08, la cara 2: una sigla que no es la del documento, o C.I. sin cifras de documento, se publica entera", {
  n <- 120L
  d <- data.frame(
    id = seq_len(n),
    "C.I.I.U." = sprintf("%04d", 1110L + (seq_len(n) * 37L) %% 8000L),
    c_i_i_u = sprintf("%04d", 1200L + (seq_len(n) * 53L) %% 8000L),
    "Cod. C.I.I.U." = sprintf("%05d", 11000L + (seq_len(n) * 61L) %% 80000L),
    "C.I.E." = rep_len(c("J45.0", "E11.9", "I10", "K21.0", "M54.5"), n),
    "C.I." = rep_len(c("S", "N", "S"), n),
    "S.A." = rep_len(c("SI", "NO"), n),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  p1 <- suppressMessages(perfilar(d))
  p0 <- suppressMessages(perfilar(d, proteger_datos_personales = FALSE))
  for (columna in names(d)[-1L]) {
    expect_false(isTRUE(any(p1$datos_personales$proteger[
      p1$datos_personales$columna == columna])), info = columna)
    expect_identical(.r39p_celdas_tapadas(p1, p0, columna), character(),
                     info = columna)
  }
})

test_that("H-17: la fecha de nacimiento y de defuncion de la nomenclatura GIK de GeneXus se reconoce y no se publica", {
  n <- 150L
  esperado <- c(
    PerFchNac = "fecha_nacimiento", CliFchNac = "fecha_nacimiento",
    FchNac = "fecha_nacimiento", AfiFchNac = "fecha_nacimiento",
    FNac = "fecha_nacimiento", PerFNac = "fecha_nacimiento",
    PerFchDef = "fecha_fallecimiento", PerFchFall = "fecha_fallecimiento"
  )
  d <- data.frame(PerId = seq_len(n))
  # Cada columna en sus propios ocho anios: ningun valor se repite en otra.
  for (j in seq_along(esperado)) {
    desde <- as.Date(sprintf("%d-01-01", 1930L + 8L * j))
    d[[names(esperado)[[j]]]] <- desde + (seq_len(n) * 19L) %% 2900L
  }
  p <- suppressMessages(perfilar(d))
  for (columna in names(esperado)) {
    expect_identical(.r39p_clasificacion(p$datos_personales, columna),
                     paste0(esperado[[columna]], ":TRUE"), info = columna)
    expect_identical(.r39p_apariciones(p, format(d[[columna]]),
                                       imprimir = TRUE), 0L, info = columna)
  }
})

test_that("H-17, la cara 2: las fechas y los codigos GIK que no son personales se publican enteros", {
  n <- 150L
  fechas <- function(k) as.Date("2001-01-01") + (seq_len(n) * k) %% 8000L
  d <- data.frame(
    PerId = seq_len(n), PerFchAlta = fechas(17L), CliFchIng = fechas(23L),
    TrmFch = fechas(29L), FacFchVto = fechas(31L), EqpFchFalla = fechas(37L),
    OrdFchDefinitiva = fechas(41L), PerFchBaja = fechas(43L),
    PerCod = sprintf("P%05d", seq_len(n) * 7L),
    TrmNro = sprintf("2024-%05d", seq_len(n)),
    stringsAsFactors = FALSE
  )
  p1 <- suppressMessages(perfilar(d))
  p0 <- suppressMessages(perfilar(d, proteger_datos_personales = FALSE))
  for (columna in names(d)[-1L]) {
    expect_false(isTRUE(any(p1$datos_personales$proteger[
      p1$datos_personales$columna == columna])), info = columna)
    expect_identical(.r39p_celdas_tapadas(p1, p0, columna), character(),
                     info = columna)
  }
})

# Cuarenta apellidos, diez por columna: ningun login se repite en otra columna.
.r39p_apellidos <- c(
  "perez", "gonzalez", "machado", "silva", "sosa", "nunez", "acosta",
  "cabrera", "rocha", "lopez", "fernandez", "rodriguez", "pereira", "diaz",
  "suarez", "ramirez", "olivera", "castro", "romero", "ferreira", "mendez",
  "alvarez", "vazquez", "benitez", "correa", "torres", "gomez", "medina",
  "barreto", "cardozo", "santos", "hernandez", "morales", "rivero",
  "silveira", "duarte", "techera", "moreira", "varela", "borges"
)
# Tres de diez con la inicial `a`, `o` o `u`: el 70 %, debajo del 80 %.
.r39p_iniciales <- c("a", "o", "u", "j", "m", "l", "r", "p", "c", "g")

test_that("H-01: el login con la inicial a, o, u pegada por guion bajo, guion o punto se protege en las cuatro columnas de quien actua", {
  n <- 200L
  formas <- list(
    guion_bajo = function(i, a) paste0(i, "_", a),
    guion = function(i, a) paste0(i, "-", a),
    sufijo_bajo = function(i, a) paste0(a, "_", i),
    sufijo_guion = function(i, a) paste0(a, "-", i),
    sufijo_punto = function(i, a) paste0(a, ".", i),
    mayusculas = function(i, a) toupper(paste0(i, "_", a)),
    mayusculas_sufijo_punto = function(i, a) toupper(paste0(a, ".", i)),
    con_numero = function(i, a) paste0(i, "_", a, seq_along(a) %% 9L + 1L)
  )
  columnas <- c("usuario_alta", "funcionario_baja", "responsable_alta",
                "solicitante_baja")
  for (forma in names(formas)) {
    d <- data.frame(id = seq_len(n))
    logins <- list()
    for (j in seq_along(columnas)) {
      apellidos <- .r39p_apellidos[(j - 1L) * 10L + seq_len(10L)]
      logins[[j]] <- formas[[forma]](.r39p_iniciales, apellidos)
      d[[columnas[[j]]]] <- rep_len(logins[[j]], n)
    }
    p <- suppressMessages(perfilar(d))
    for (j in seq_along(columnas)) {
      expect_identical(.r39p_clasificacion(p$datos_personales, columnas[[j]]),
                       "nombre:TRUE", info = paste(forma, columnas[[j]]))
      expect_identical(.r39p_apariciones(p, logins[[j]], imprimir = TRUE), 0L,
                       info = paste(forma, columnas[[j]]))
    }
  }
})

test_that("H-01, la cara 2: los estados y los motivos con guion bajo siguen siendo estados, y una categoria con letra suelta no es un nombre", {
  n <- 200L
  d <- data.frame(
    id = seq_len(n),
    usuario_baja = rep_len(c("sin_baja", "baja_definitiva", "cancelada",
                             "por_renuncia", "a_pedido"), n),
    funcionario_baja = rep_len(c("CESE", "RENUNCIA", "JUBILACION",
                                 "FALLECIMIENTO", "TRASLADO"), n),
    responsable_grupo = rep_len(c("Grupo_A", "Grupo_B", "Turno-A", "Plan_A",
                                  "Nivel-B"), n),
    tipo_cliente = rep_len(c("tipo_a", "tipo_b", "clase-a", "categoria_u"), n),
    stringsAsFactors = FALSE
  )
  p1 <- suppressMessages(perfilar(d))
  p0 <- suppressMessages(perfilar(d, proteger_datos_personales = FALSE))
  for (columna in names(d)[-1L]) {
    expect_false(isTRUE(any(p1$datos_personales$proteger[
      p1$datos_personales$columna == columna])), info = columna)
    expect_identical(.r39p_celdas_tapadas(p1, p0, columna), character(),
                     info = columna)
  }
})

# La tabla de las pruebas por la via DBI: los tres altos en una tabla.
.r39p_tabla_dbi <- function(n = 600L) {
  logins <- paste0(.r39p_iniciales, "_", .r39p_apellidos[seq_len(10L)])
  data.frame(
    PerId = seq_len(n),
    PerFchNac = format(as.Date("1940-01-01") + (seq_len(n) * 13L) %% 7000L),
    "C.I." = .r39p_con_puntos(.r39p_cedulas(n, desde = 50000L)),
    usuario_alta = rep_len(logins, n),
    PerImpCuota = round(500 + (seq_len(n) * 7.31) %% 2500, 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.r39p_comprobar_dbi <- function(p, d, motor) {
  dp <- p$perfil_muestra$datos_personales
  protegidas <- as.character(dp$columna[dp$proteger %in% TRUE])
  for (columna in c("PerFchNac", "C.I.", "usuario_alta")) {
    expect_true(columna %in% protegidas, info = paste(motor, columna))
    expect_identical(.r39p_apariciones(p, d[[columna]]), 0L,
                     info = paste(motor, columna))
  }
}

test_that("H-08, H-17 y H-01 por la via DBI: perfilar_dbi() protege las tres columnas y no publica sus valores", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  d <- .r39p_tabla_dbi()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "afiliados", d)
  p <- suppressWarnings(suppressMessages(perfilar_dbi(con, "afiliados")))
  .r39p_comprobar_dbi(p, d, "sqlite")
})

test_that("H-08, H-17 y H-01 en PostgreSQL real: perfilar_dbi() protege las tres columnas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) skip("PostgreSQL real no disponible en el puerto 55432.")
  tabla <- "r39p_dev_o180_afiliados"
  on.exit({
    try(DBI::dbRemoveTable(con, tabla), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  try(DBI::dbRemoveTable(con, tabla), silent = TRUE)
  d <- .r39p_tabla_dbi()
  d$PerFchNac <- as.Date(d$PerFchNac)
  DBI::dbWriteTable(con, tabla, d)
  p <- suppressWarnings(suppressMessages(perfilar_dbi(con, tabla)))
  d$PerFchNac <- format(d$PerFchNac)
  .r39p_comprobar_dbi(p, d, "postgres")
})
