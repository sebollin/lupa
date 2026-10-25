# O177: la proteccion de datos personales. Cuatro de los cinco casos son
# regresiones de reglas que buscaban no tapar de mas -P-H1, la forma de un
# estado en la regla de quien actua; P-H3, la frase; P-H2, la forma nueva del
# importe; P-H5, el documento sin cifras decidido sobre la muestra- y el quinto,
# P-H4, es anterior: el encabezado con tilde de un CSV de Windows. Mas B1
# (nombres pegados y plurales de la abreviatura). Cada bloque rehace la cifra a
# mano y falla contra el codigo anterior al arreglo, salvo los de la cara 2, que
# vigilan que el arreglo no tape de mas.

# Los textos y los numeros de todas las hojas de un objeto, y su impresion.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r38p_textos <- function(x, imprimir = FALSE) {
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.numeric(y)) {
      y <- as.numeric(y)
      textos <<- c(textos, format(y[!is.na(y)], scientific = FALSE, trim = TRUE,
                                  digits = 15))
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(unclass(x))
  if (imprimir) {
    textos <- c(textos, utils::capture.output(print(x)))
  }
  textos[!is.na(textos)]
}

# Cuantos de `valores` aparecen, como texto entero o dentro de otro, en `x`.
.r38p_apariciones <- function(x, valores, imprimir = FALSE) {
  textos <- .r38p_textos(x, imprimir = imprimir)
  sum(vapply(valores, function(v) {
    any(grepl(v, textos, fixed = TRUE, useBytes = TRUE))
  }, logical(1L)))
}

.r38p_protegidas <- function(p) {
  dp <- p$datos_personales
  as.character(dp$columna[dp$proteger %in% TRUE])
}

test_that("P-H1 y P-H3: la regla de quien actua protege logins, apellidos con forma de estado y nombres autocapitalizados", {
  casos <- list(
    A = list("usuario_alta", c("jperez", "mgonzalez", "lmachado", "asilva")),
    B = list("usuario_alta", c("jperez", "mgonzalez", "lmachado", "asilva",
                               "rsosa", "mdelgado", "pfuentes", "cnunez",
                               "gacosta", "fcabrera")),
    C = list("usuario_alta", c("PEREZ, J.", "MACHADO, L.", "DELGADO, M.",
                               "GOMEZ, A.", "SILVA, R.")),
    D = list("responsable_alta", c("Juan perez", "Maria gonzalez", "Ana silva",
                                   "Luis rodriguez", "Carlos sosa")),
    E = list("solicitante_alta", c("Juan carlos Perez", "Maria jose Rodriguez",
                                   "Ana laura Silva", "Jose luis Gomez",
                                   "Pedro Sosa", "Lucia Fernandez",
                                   "Martin Lopez", "Sofia Diaz")),
    F = list("funcionario_baja", c("j.machado", "m.delgado", "p.sosa")),
    H = list("usuario_alta", c("Nelida Machado", "Juan Perez", "Ana Gomez")),
    I = list("usuario_baja", c("lmachado2", "jperez1", "agomez", "rsilva")),
    J = list("usuario_alta", c("mdelgado", "jrodriguez", "agonzalez")),
    # Un usuario del sistema junto a tres logins no es un estado: con el, la
    # columna quedaria en 3 de 4.
    K = list("usuario_alta", c("SISTEMA", "jperez", "mdelgado", "lmachado")),
    L = list("funcionario_baja", c("Machado", "Perez", "Delgado", "Gomez"))
  )
  # La cifra a mano: con cuatro funcionarios, uno que pasara por estado deja la
  # columna en el 75 %, debajo del 80 % que pide la regla.
  expect_lt(3 / 4, .PROPORCION_FORMA_PERSONAL)
  n <- 60L
  for (caso in names(casos)) {
    columna <- casos[[caso]][[1L]]
    valores <- casos[[caso]][[2L]]
    d <- data.frame(nro_tramite = sprintf("T-%05d", seq_len(n)),
                    x = rep(valores, length.out = n), stringsAsFactors = FALSE)
    names(d)[[2L]] <- columna
    p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
    expect_identical(.r38p_protegidas(p), columna, info = caso)
    expect_identical(.r38p_apariciones(p, valores), 0L, info = caso)
  }
})

test_that("P-H1, el recorrido: un login con cualquier apellido con forma de estado se protege en las ocho columnas de quien actua", {
  apellidos <- c("machado", "delgado", "maldonado", "salgado", "garrido",
                 "mercado", "prado", "hurtado", "fuentes", "ventura", "segura",
                 "sarmiento", "collado", "alvarado", "coronado", "quesada",
                 "estrada", "tejada", "granado", "cortes", "flores", "torres")
  columnas <- c("usuario_alta", "usuario_baja", "funcionario_alta",
                "funcionario_baja", "responsable_alta", "responsable_baja",
                "solicitante_alta", "solicitante_baja")
  otros <- c("jperez", "mgonzalez", "asilva")
  n <- 40L
  ejercidos <- 0L
  for (inicio in seq(1L, length(apellidos), by = length(columnas))) {
    tanda <- apellidos[inicio:min(length(apellidos), inicio + 7L)]
    d <- as.data.frame(stats::setNames(lapply(seq_along(tanda), function(i) {
      rep(c(paste0(substr(letters[i], 1L, 1L), tanda[[i]]), otros),
          length.out = n)
    }), columnas[seq_along(tanda)]), stringsAsFactors = FALSE)
    p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
    expect_setequal(.r38p_protegidas(p), names(d))
    expect_identical(.r38p_apariciones(p, paste0(letters[seq_along(tanda)],
                                                 tanda)), 0L)
    ejercidos <- ejercidos + length(tanda)
  }
  expect_identical(ejercidos, length(apellidos))
  # Ninguna palabra de la lista de estados es un nombre de pila, ni ninguna
  # lectura en singular de estos apellidos esta en la lista.
  expect_identical(intersect(.PALABRAS_DE_ESTADO, .NOMBRES_DE_PILA),
                   character())
  expect_identical(intersect(.singular_de_estado(apellidos),
                             .PALABRAS_DE_ESTADO), character())
})

test_that("P-H1, la cara 2: los estados y los motivos de la lista se publican, en mayusculas y en plural", {
  plural <- function(p) ifelse(grepl("[aeiou]$", p), paste0(p, "s"),
                               paste0(p, "es"))
  titulo <- function(p) paste0(toupper(substr(p, 1L, 1L)), substring(p, 2L))
  palabras <- .PALABRAS_DE_ESTADO
  n <- length(palabras) * 2L
  d <- data.frame(
    usuario_baja = rep(titulo(palabras), 2L),
    funcionario_baja = rep(toupper(plural(palabras)), 2L),
    responsable_alta = rep(c("Fin de contrato", "Renuncia voluntaria",
                             "Mutuo acuerdo", "Baja definitiva",
                             "Pase a otra oficina", "Cancelada"),
                           length.out = n),
    stringsAsFactors = FALSE
  )
  con <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_identical(.r38p_protegidas(con), character())
  for (campo in c("moda", "minimo", "maximo")) {
    expect_identical(con$columnas[[campo]], sin$columnas[[campo]], info = campo)
  }
  expect_identical(nrow(d), n)
})

test_that("P-H3: en una columna calificada, el nombre autocapitalizado es un nombre y la frase que empieza con Dolores o Victoria no", {
  n <- 60L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    responsable_grupo = r(c("Juan perez", "Maria gonzalez", "Ana silva",
                            "Luis rodriguez", "Carlos sosa")),
    contacto_alta = r(c("Juan carlos Perez", "Maria jose Rodriguez",
                        "Ana laura Silva", "Jose luis Gomez")),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  expect_setequal(.r38p_protegidas(p), names(d))
  expect_identical(.r38p_apariciones(p, unique(unlist(d))), 0L)
  frases <- data.frame(
    tipo_sintoma_paciente = r(c("Dolores musculares agudos",
                                "Dolores articulares cronicos",
                                "Dolores de cabeza")),
    resultado_socio = r(c("Victoria local", "Victoria visitante")),
    stringsAsFactors = FALSE
  )
  pf <- suppressWarnings(perfilar(frases, proteger_datos_personales = TRUE))
  sf <- suppressWarnings(perfilar(frases, proteger_datos_personales = FALSE))
  expect_identical(.r38p_protegidas(pf), character())
  expect_identical(pf$columnas$moda, sf$columnas$moda)
  expect_true(all(.NOMBRES_QUE_SON_PALABRAS %in% .NOMBRES_DE_PILA))
})

test_that("P-H2: el celular escrito con puntos de a tres y su cero se protege en las columnas que deciden por sus valores", {
  set.seed(3817L)
  base <- sprintf("09%d.%03d.%03d", sample(1:9, 40L, TRUE),
                  sample(0:999, 40L, TRUE), sample(0:999, 40L, TRUE))
  # La cifra a mano: `"099"` y dos grupos de tres son las nueve cifras de un
  # celular, y empiezan con cero.
  expect_identical(nchar(gsub("[^0-9]", "", base[[1L]])), 9L)
  expect_true(startsWith(base[[1L]], "0"))
  columnas <- c("telefono_tipo", "celular_estado", "responsable_canal",
                "usuario_canal", "contacto_alta", "cliente_medio",
                "tel_contacto_tipo")
  n <- 120L
  d <- as.data.frame(stats::setNames(lapply(seq_along(columnas), function(i) {
    sample(base, n, TRUE)
  }), columnas), stringsAsFactors = FALSE)
  p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  expect_setequal(.r38p_protegidas(p), columnas)
  expect_identical(.r38p_apariciones(p, base), 0L)
})

test_that("P-H2, el recorrido: cuarenta escrituras de un telefono, con la palabra y sin ella, por perfilar()", {
  formas <- c(
    "099123456", "099 123 456", "099-123-456", "099.123.456", "099.12.34.56",
    "(099) 123 456", "+598 99 123 456", "+59899123456", "00598 99 123 456",
    "2901 1234", "2901-1234", "2901.1234", "29011234", "(02) 901 1234",
    "4732 1230", "4732.1230", "4732-1230", "2 901 1234",
    "099 123 456 / 2901 1234", "2901 1234 int 101", "099123456.0", "99123456",
    "99 123 456", "99.123.456", "099,123,456", "098 765 430", "098.765.430",
    "098.765.000", "2400 0010", "2400-0010", "+598 2 901 1234",
    "+54 11 4123 4567", "+55 51 91234 5678", "099 123.456", "099.123456",
    "4.732.1234", "091 234-567", "2901 2019", "2901-2019", "092.019.912"
  )
  # Lo que se espera es la respuesta de antes de esas reglas, salvo lo decidido
  # para el celular de a tres sin su cero, con la palabra, y las dos
  # limitaciones declaradas -el fijo con punto cuyo ultimo grupo termina en
  # cero, como un importe con cuatro decimales; el que tiene un grupo de cuatro
  # que empieza con `00`, como un codigo rellenado-.
  no_sin <- c("29011234", "99123456", "99 123 456", "99.123.456",
              "099,123,456", "2901-2019", "4732.1230", "2400-0010")
  no_con <- c("099,123,456", "2901-2019", "4732.1230", "2400-0010")
  armar <- function(prefijo) {
    as.data.frame(stats::setNames(lapply(formas, function(v) rep(v, 6L)),
                                  paste0(prefijo, seq_along(formas))),
                  stringsAsFactors = FALSE)
  }
  con <- suppressWarnings(perfilar(armar("celular_estado_"), proteger_datos_personales = TRUE))
  sin <- suppressWarnings(perfilar(armar("cliente_medio_"), proteger_datos_personales = TRUE))
  con_palabra <- paste0("celular_estado_", seq_along(formas)) %in%
    .r38p_protegidas(con)
  sin_palabra <- paste0("cliente_medio_", seq_along(formas)) %in%
    .r38p_protegidas(sin)
  expect_identical(stats::setNames(con_palabra, formas),
                   stats::setNames(!formas %in% no_con, formas))
  expect_identical(stats::setNames(sin_palabra, formas),
                   stats::setNames(!formas %in% no_sin, formas))
  expect_identical(length(con_palabra), 40L)
  # Y los importes que esas reglas destaparon se siguen publicando.
  m <- 24L
  r <- function(v) rep(v, length.out = m)
  importes <- data.frame(
    saldo_socio = r(c("1.456.651,89", "1.456.651,89", "2.345.678,10")),
    importe_cliente = r(c("1,456,651.89", "1,456,651.89", "2,345,678.10")),
    monto_titular = r(c("12 345.50", "12 345.50", "23 456,75")),
    ajuste_usuario_monto = r(c("-12345.10", "-12345.10", "-2345.50")),
    importe_cliente_dec4 = r(c("1234.5000", "1234.5000", "2345.7500")),
    cliente_id_suc = r(c("10-0012345", "10-0012345", "10-0012346")),
    stringsAsFactors = FALSE
  )
  pi <- suppressWarnings(perfilar(importes, proteger_datos_personales = TRUE))
  si <- suppressWarnings(perfilar(importes, proteger_datos_personales = FALSE))
  expect_identical(.r38p_protegidas(pi), character())
  expect_identical(pi$columnas$moda, si$columnas$moda)
})

test_that("P-H4: los encabezados con tilde de un CSV de Windows leido sin declarar la codificacion se reconocen", {
  b <- function(...) rawToChar(as.raw(c(...)))
  # `Direccion`, `Telefono`, `Telefono movil`, `Cedula` y `Fecha de defuncion`
  # con su tilde en Windows-1252: un byte por letra, que no es UTF-8 valido.
  encabezados <- c(
    direccion = b(0x44, 0x69, 0x72, 0x65, 0x63, 0x63, 0x69, 0xf3, 0x6e),
    telefono = b(0x54, 0x65, 0x6c, 0xe9, 0x66, 0x6f, 0x6e, 0x6f),
    movil = b(0x54, 0x65, 0x6c, 0xe9, 0x66, 0x6f, 0x6e, 0x6f, 0x20, 0x6d, 0xf3,
              0x76, 0x69, 0x6c),
    cedula = b(0x43, 0xe9, 0x64, 0x75, 0x6c, 0x61),
    defuncion = b(0x46, 0x65, 0x63, 0x68, 0x61, 0x20, 0x64, 0x65, 0x20, 0x64,
                  0x65, 0x66, 0x75, 0x6e, 0x63, 0x69, 0xf3, 0x6e)
  )
  expect_false(any(validUTF8(encabezados)))
  set.seed(3815L)
  n <- 60L
  calles <- paste(sample(c("Bv. Artigas", "Av. Italia", "Colonia", "Rivera"),
                         n, TRUE), sample(1000:3999, n, TRUE))
  fijos <- sprintf("2%07d", sample(1e6:9999999, n))
  celulares <- sprintf("09%d %03d %03d", sample(1:9, n, TRUE),
                       sample(0:999, n, TRUE), sample(0:999, n, TRUE))
  cedulas <- as.character(sample(1000000:5999999, n))
  fechas <- format(as.Date("2018-01-01") + sample(0:2000, n, TRUE))
  d <- data.frame(id = seq_len(n), calles, fijos, celulares, cedulas, fechas,
                  stringsAsFactors = FALSE)
  names(d) <- c("id", unname(encabezados))
  p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  expect_identical(length(.columnas_personales_rapidas(d)), 5L)
  expect_identical(length(.r38p_protegidas(p)), 5L)
  expect_identical(.r38p_apariciones(p, c(calles, celulares, cedulas)), 0L)
  # Por el archivo, como lo lee `read.csv()` sin declarar la codificacion.
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  cuerpo <- paste(d$id, calles, fijos, celulares, cedulas, fechas, sep = ";")
  writeBin(c(charToRaw("id;"),
             unlist(lapply(seq_along(encabezados), function(j) {
               c(charToRaw(encabezados[[j]]),
                 charToRaw(if (j < length(encabezados)) ";" else "\n"))
             })),
             charToRaw(paste0(paste(cuerpo, collapse = "\n"), "\n"))),
           archivo)
  leido <- utils::read.csv(archivo, sep = ";", check.names = FALSE,
                           colClasses = "character")
  expect_identical(ncol(leido), 6L)
  pl <- suppressWarnings(perfilar(leido, proteger_datos_personales = TRUE))
  expect_identical(length(.r38p_protegidas(pl)), 5L)
  expect_identical(.r38p_apariciones(pl, c(calles, celulares, cedulas)), 0L)
})

test_that("P-H5: por la via DBI con muestra, el documento por el nombre que la muestra no alcanzo se protege y el resumen no publica sus extremos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(3823L)
  n <- 2000L
  ced <- sample(1000000:5999999, n)
  ralo <- function(v) {
    x <- rep(NA, n)
    x[(n - length(v) + 1L):n] <- v
    x
  }
  conyuges <- ced[(n - 3L):n] + 1L
  tutores <- c("2.976.443-9", "5.840.566-4", "3.785.489-8", "4.898.652-3")
  d <- data.frame(
    id = seq_len(n), nro_documento = ced,
    cedula_conyuge = ralo(as.numeric(conyuges)),
    ci_tutor = ralo(tutores), stringsAsFactors = FALSE
  )
  # La cifra a mano: las columnas ralas tienen cuatro valores en las ultimas
  # filas, y una muestra de 200 al azar entre 2000 no ve ninguno con
  # probabilidad (1 - 4/2000)^200, mas de dos de cada tres veces; la ordenada
  # por `id` y las primeras filas de los bloques, nunca.
  expect_gt((1 - 4 / n)^200, 0.6)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "padron", d)
  valores <- c(as.character(conyuges), tutores)
  ralas <- c("cedula_conyuge", "ci_tutor")
  for (via in c("muestra", "bloques", "orden")) {
    p <- suppressWarnings(switch(via,
      muestra = perfilar_dbi(con, "padron", muestra = 200, proteger_datos_personales = TRUE),
      bloques = perfilar_dbi(con, "padron", bloque_filas = 500, muestra = 1000, proteger_datos_personales = TRUE),
      orden = perfilar_dbi(con, "padron", muestra = 200, orden_muestra = "id", proteger_datos_personales = TRUE)
    ))
    expect_true(all(ralas %in% .r38p_protegidas(p$perfil_muestra)), info = via)
    columnas <- p$resumen_tabla$columnas
    expect_true(all(is.na(unlist(columnas[columnas$columna == "cedula_conyuge",
                                          c("minimo", "maximo", "mediana")]))),
                info = via)
    expect_identical(.r38p_apariciones(p, valores), 0L, info = via)
  }
})

test_that("P-H5, la cara 2: un documento por el nombre sin cifras se publica si hay valores bastantes, y un solo valor repetido no desmiente", {
  n <- 40L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    cedula_valida = r(c("S", "S", "N")),
    cod_documento = r(c(1L, 1L, 2L, 9L)),
    stringsAsFactors = FALSE
  )
  con <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_identical(.r38p_protegidas(con), character())
  expect_identical(con$columnas$moda, sin$columnas$moda)
  # Lo que no desmiente: pocos valores, ceros, un solo valor repetido.
  pocos <- data.frame(cedula_valida = c("S", "N", "S"),
                      cedula_conyuge = c(0, 0, 0), stringsAsFactors = FALSE)
  pp <- suppressWarnings(perfilar(pocos, proteger_datos_personales = TRUE))
  expect_setequal(.r38p_protegidas(pp), names(pocos))
  ceros <- data.frame(cedula_conyuge = r(0), ci_tutor = r("S/D"),
                      stringsAsFactors = FALSE)
  pc <- suppressWarnings(perfilar(ceros, proteger_datos_personales = TRUE))
  expect_setequal(.r38p_protegidas(pc), names(ceros))
  # La cifra a mano: tres valores no llegan al umbral; cuarenta, si.
  expect_lt(nrow(pocos), .MIN_VALORES_DESMIENTEN_DOCUMENTO)
  expect_gte(n, .MIN_VALORES_DESMIENTEN_DOCUMENTO)
})

test_that("B1: los nombres pegados de los sistemas heredados y el plural de la abreviatura se reconocen", {
  set.seed(3819L)
  n <- 60L
  cel <- sprintf("09%d%06d", sample(1:9, n, TRUE), sample(0:999999, n, TRUE))
  nom <- sample(c("Martin Rodriguez", "Ana Suarez", "Luis Gomez", "Sofia Diaz"),
                n, TRUE)
  ced <- as.character(sample(1000000:5999999, n))
  dir <- paste("Colonia", sample(1000:3999, n, TRUE))
  d <- data.frame(nrotel = cel, TELCEL = cel, tels = cel, cels = cel,
                  fonos = cel, nomape = nom, NOMBRECOMPLETO = nom, noms = nom,
                  nrodoc = ced, NRODOC_TXT = ced, dirpart = dir,
                  DIRPARTICULAR = dir, dirs = dir, stringsAsFactors = FALSE)
  esperado <- c(nrotel = "telefono", TELCEL = "telefono", tels = "telefono",
                cels = "telefono", fonos = "telefono", nomape = "nombre",
                NOMBRECOMPLETO = "nombre", noms = "nombre",
                nrodoc = "documento_identidad",
                NRODOC_TXT = "documento_identidad", dirpart = "domicilio",
                DIRPARTICULAR = "domicilio", dirs = "domicilio")
  p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  dp <- p$datos_personales
  protegidas <- dp[dp$proteger %in% TRUE, , drop = FALSE]
  expect_setequal(protegidas$columna, names(esperado))
  expect_identical(stats::setNames(protegidas$tipo, protegidas$columna)[
    names(esperado)], esperado)
  expect_identical(.r38p_apariciones(p, c(cel, nom, ced, dir)), 0L)
})
