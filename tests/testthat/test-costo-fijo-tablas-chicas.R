# El costo fijo de perfilar() y analizar() sobre tablas chicas: armar tablas sin
# el precio por columna de data.frame(), no probar formatos de fecha que no
# pueden casar y no clasificar dos veces la misma tabla. Ninguna de esas cosas
# puede cambiar una salida: las equivalencias se comprueban con identical().

# Compara una construccion con la de base, incluido el error si lo hay.
.igual_a_base <- function(rapida, base) {
  a <- tryCatch(rapida(), error = function(e) paste("error:", conditionMessage(e)))
  b <- tryCatch(base(), error = function(e) paste("error:", conditionMessage(e)))
  identical(a, b)
}

test_that(".data_frame_simple() arma lo mismo que data.frame()", {
  acento <- "Mart\u00edn"
  latin1 <- iconv(acento, "UTF-8", "latin1")
  hora <- as.POSIXct("2024-01-31 10:20:30", tz = "America/Montevideo")
  casos <- list(
    list(a = 1, b = "x", c = NA),
    list(a = 1:3, b = "x", c = NA),
    list(a = 1:3, b = c("x", "y", "z"), c = c(TRUE, FALSE, NA)),
    list(a = integer(), b = character()),
    list(a = c(1.5, 2), `b c` = TRUE, `if` = 1L, `1x` = "a"),
    list(a = 1, a = 2, a.1 = 3),
    list(`a b` = 1, a.b = 2),
    list(a = acento, b = latin1, c = NA_character_),
    list(f = factor(c("b", "a")), o = factor(c("b", "a"), ordered = TRUE)),
    list(f = factor("a"), n = 1:3),
    list(d = as.Date("2024-01-31"), n = 1:2),
    list(t = hora, n = 1:2),
    list(t = as.POSIXct(c("2024-01-31", NA), tz = "UTC"), z = complex(real = 1:2, imaginary = 0)),
    list(r = as.raw(1:2), n = 1:2),
    # De aca en adelante se resuelve con data.frame().
    list(a = c(x = 1, y = 2), b = 1:2),
    list(a = list(1, 2), b = 1:2),
    list(a = NULL, b = 1:2),
    list(a = 1:2, b = 1:3),
    list(a = 1:2, b = integer()),
    list(a = 1:4, b = 1:2),
    list(a = structure(1:2, etiqueta = "x"), b = 1:2),
    list(a = structure(1:2, class = "propia"), b = 1:2),
    list(a = I("x"), b = 1:2),
    list(m = matrix(1:4, 2), b = 1:2),
    list(a = as.difftime(1:2, units = "days"), b = 1:2),
    list(a = as.difftime(1, units = "days"), b = 1:2),
    list(a = as.POSIXlt("2024-01-31", tz = "UTC"), b = 1)
  )
  for (i in seq_along(casos)) {
    caso <- casos[[i]]
    for (factores in c(FALSE, TRUE)) for (chequear in c(TRUE, FALSE)) {
      expect_true(.igual_a_base(
        function() do.call(.data_frame_simple, c(caso, list(
          stringsAsFactors = factores, check.names = chequear))),
        function() do.call(data.frame, c(caso, list(
          stringsAsFactors = factores, check.names = chequear)))
      ), info = paste("caso", i, factores, chequear))
    }
  }
  # Sin nombre la columna se nombra con la expresion, como en data.frame().
  x <- 1:2
  expect_identical(.data_frame_simple(x, y = 3), data.frame(x, y = 3))
  expect_identical(.data_frame_simple(), data.frame())
})

test_that("las filas del perfil se arman sin pasar por data.frame()", {
  # Pesada para CRAN (mas de 1 s medido): corre con NOT_CRAN=true.
  skip_on_cran()
  # La fila de cada columna perfilada es la construccion mas cara del perfil de
  # una tabla chica. Si una columna nueva de la fila trae atributos o nombres,
  # la fila vuelve en silencio al data.frame() de siempre: la salida no cambia,
  # el costo si.
  caidas <- character()
  original <- .filas_data_frame_simple
  local_mocked_bindings(
    .filas_data_frame_simple = function(columnas) {
      n <- original(columnas)
      if (is.na(n)) {
        caidas <<- c(caidas, deparse(sys.call(-2L)[[1L]])[[1L]])
      }
      n
    },
    .package = "lupa"
  )
  datos <- data.frame(
    n = c(1.5, NA, 3, -99), i = c(1L, 2L, NA, 4L), l = c(TRUE, NA, FALSE, TRUE),
    s = c("a", "b", NA, "S/D"), f = factor(c("x", "y", "x", NA)),
    d = as.Date("2024-01-01") + 0:3,
    t = as.POSIXct("2024-01-01 10:00", tz = "UTC") + 0:3,
    fecha_texto = c("2024-01-31", "31/01/2024", "2024/02/15", NA),
    stringsAsFactors = FALSE
  )
  perfilar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_false(".perfilar_columna" %in% caidas)
  expect_false(".fila_formato" %in% caidas)
})

test_that(".apilar_filas() apila lo mismo que rbind() sin nombres de fila", {
  base_apilada <- function(filas) {
    x <- do.call(rbind, filas)
    rownames(x) <- NULL
    x
  }
  fila <- function(...) data.frame(..., stringsAsFactors = FALSE)
  casos <- list(
    list(fila(a = 1, b = "x"), fila(a = 2, b = "y")),
    list(fila(a = 1L, b = NA), fila(a = 2.5, b = TRUE), fila(a = NA, b = 3L)),
    list(fila(a = TRUE), fila(a = 2L), fila(a = 0.5)),
    list(fila(a = 1:3, b = c("x", "y", "z")), fila(a = 4L, b = "w")),
    list(fila(a = "Mart\u00edn"), fila(a = NA_character_)),
    list(fila(a = 1)),
    list(uno = fila(a = 1), dos = fila(a = 2)),
    list(fila(a = 1, a = 2, check.names = FALSE), fila(a = 3, a = 4, check.names = FALSE)),
    # De aca en adelante se resuelve con rbind().
    list(fila(a = TRUE), fila(a = "x")),
    list(fila(a = 1L), fila(a = "x")),
    list(fila(a = "x"), fila(a = 1.5)),
    list(fila(a = factor("x")), fila(a = factor("y"))),
    list(fila(a = as.Date("2024-01-01")), fila(a = as.Date("2024-01-02"))),
    list(fila(a = 1, b = 2), fila(b = 3, a = 4)),
    list(fila(a = 1)[0, , drop = FALSE], fila(a = "x")),
    list(fila(a = 1), fila(a = 2), data.frame()),
    list(structure(fila(a = 1), marca = "x"), fila(a = 2)),
    list(fila(a = complex(real = 1, imaginary = 1)), fila(a = 2))
  )
  for (i in seq_along(casos)) {
    expect_true(.igual_a_base(
      function() .apilar_filas(casos[[i]]),
      function() base_apilada(casos[[i]])
    ), info = paste("caso", i))
  }
})

test_that("descartar un formato de fecha por sus caracteres es una condicion necesaria", {
  especificaciones <- .especificaciones_fecha()
  # Todo lo que la deteccion puede recibir: las escrituras de la tabla con
  # todos los separadores y sufijos, y valores que casan con una parte.
  bases <- c(
    "2024-01", "01/2024", "2024/01", "2024-01-31", "31/01/2024", "01/31/2024",
    "31-01-2024", "01-31-2024", "2024/01/31", "31.01.2024", "01.31.2024",
    "20240131", "31/01/24", "01/31/24", "31-01-24", "01-31-24", "31.01.24",
    "01.31.24", "24-01-31", "1/2/2024", "2024-1-2"
  )
  sufijos <- c(
    "", " 10:20", " 10:20:30", " 10:20:30.5", "T10:20", "T10:20:30",
    "T10:20:30.25", "T10:20:30Z", "T10:20:30-03:00", "T10:20:30.5+01:00"
  )
  valores <- c(outer(bases, sufijos, paste0))
  for (k in c(1L, 2L, 5L, length(valores))) {
    set.seed(k)
    muestra <- sample(valores, k)
    posibles <- .formatos_fecha_posibles(muestra, especificaciones)
    for (i in which(!posibles)) {
      expect_false(any(.es_fecha_valida(
        muestra, especificaciones$formato[[i]],
        especificaciones$expresion[[i]], especificaciones$granularidad[[i]]
      )), info = paste(especificaciones$formato[[i]], paste(muestra, collapse = " ")))
    }
  }
  # Lo que el extractor no sabe leer no exige nada.
  expect_identical(
    .literales_exigidos_fecha(c("^a.b$", "^[0-9]+T?$", "^x(y)$", "^\\d{2}$")),
    c("", "", "", "")
  )
  expect_identical(
    .literales_exigidos_fecha("^[0-9]{4}-[0-9]{2}T[0-9]{2}:[0-9]{2}\\.[0-9]+(?:Z|[+-][0-9]{2}:[0-9]{2})$"),
    "-.:T"
  )
  # Con otra tabla que la instalada, los caracteres se calculan al vuelo.
  otra <- especificaciones[c(4L, 5L), , drop = FALSE]
  expect_identical(
    .formatos_fecha_posibles(c("2024-01-31", "2024-02-01"), otra),
    c(TRUE, FALSE)
  )
})

test_that("la deteccion de fechas no prueba formatos que no pueden casar", {
  # Sin esto, una columna que no es de fechas pagaba 147 llamadas a
  # `.es_fecha_valida()` sobre un vector vacio, y una de fechas con guion
  # probaba tambien los formatos con barra, punto, `T` y hora.
  llamadas <- character()
  original <- .es_fecha_valida
  local_mocked_bindings(
    .es_fecha_valida = function(x, formato, ...) {
      llamadas <<- c(llamadas, formato)
      original(x, formato, ...)
    },
    .package = "lupa"
  )
  sin_fechas <- detectar_formatos_fecha(c("web", "telefono", "S/D"))
  expect_identical(llamadas, character())
  expect_identical(nrow(sin_fechas), 0L)

  llamadas <- character()
  con_guion <- detectar_formatos_fecha(c("2024-01-31", "2024-02-15", "16-03-2024"))
  expect_true(all(grepl("^[-%a-zA-Z]*$", llamadas)), info = paste(llamadas, collapse = " "))
  expect_lte(length(llamadas), 10L)
  expect_setequal(con_guion$formato, c("%Y-%m-%d", "%d-%m-%Y"))
})

test_that("analizar() clasifica una sola vez los datos personales de la tabla que mide", {
  # Pesada para CRAN (mas de 1 s medido): corre con NOT_CRAN=true.
  skip_on_cran()
  # Cada `medir()` de la medicion por metrica clasificaba de nuevo las columnas
  # personales de la misma tabla para la marca `datos_personales_sin_proteger`:
  # cinco veces sobre esta tabla, el 16 % de `analizar()`. Queda una
  # clasificacion de `perfilar()` y una de la medicion.
  nombres <- character()
  original <- .clasificar_dato_personal
  local_mocked_bindings(
    .clasificar_dato_personal = function(x, nombre, ...) {
      nombres <<- c(nombres, nombre)
      original(x, nombre, ...)
    },
    .package = "lupa"
  )
  analisis <- analizar(
    datos_operativos, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    id_medicion = "costo-fijo"
  )
  expect_lte(sum(nombres == "contacto"), 2L)
  # La marca que se publica sigue saliendo.
  expect_identical(
    attr(analisis$plan_limpieza, "datos_personales_sin_proteger"), "contacto"
  )
})

test_that("la memoria de la clasificacion personal solo vive durante la medicion", {
  # Pesada para CRAN (mas de 1 s medido): corre con NOT_CRAN=true.
  skip_on_cran()
  llamadas <- 0L
  avisar <- FALSE
  original <- .clasificar_dato_personal
  local_mocked_bindings(
    .clasificar_dato_personal = function(...) {
      llamadas <<- llamadas + 1L
      if (avisar) warning("aviso de prueba")
      original(...)
    },
    .package = "lupa"
  )
  datos <- data.frame(correo = c("a@example.invalid", "b@example.invalid"),
                      n = 1:2, stringsAsFactors = FALSE)
  # Apagada: cada llamada clasifica.
  primera <- .columnas_personales_rapidas(datos)
  .columnas_personales_rapidas(datos)
  expect_identical(llamadas, 4L)
  # Encendida: la tabla identica se clasifica una vez, y otra tabla aparte. Se
  # apaga al salir de `con_memoria()` aunque falle una expectativa, para no
  # dejarla encendida a las pruebas que siguen.
  con_memoria <- function(codigo) {
    expect_true(.encender_memo_columnas_personales())
    on.exit(.apagar_memo_columnas_personales())
    expect_false(.encender_memo_columnas_personales())
    codigo
  }
  llamadas <- 0L
  con_memoria({
    expect_identical(.columnas_personales_rapidas(datos), primera)
    expect_identical(.columnas_personales_rapidas(datos), primera)
    expect_identical(llamadas, 2L)
    .columnas_personales_rapidas(datos[1L, , drop = FALSE])
    expect_identical(llamadas, 4L)
    expect_identical(
      .columnas_personales_rapidas(datos, detalle = TRUE),
      .clasificar_columnas_personales_tabla(datos, TRUE, character(), NULL)
    )
    # Lo que avisa no se guarda: cada llamada vuelve a avisar.
    avisar <- TRUE
    otra <- data.frame(x = 1:3)
    expect_warning(.columnas_personales_rapidas(otra), "aviso de prueba")
    expect_warning(.columnas_personales_rapidas(otra), "aviso de prueba")
  })
  expect_false(.memo_columnas_personales$activa)
  avisar <- FALSE
  # Y `analizar()` la deja apagada y vacia.
  analizar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
           id_medicion = "costo-fijo")
  expect_false(.memo_columnas_personales$activa)
  expect_identical(.memo_columnas_personales$entradas, list())
})

test_that(".fila_de_tabla() da la misma fila que `[`", {
  latin1 <- iconv("Mart\u00edn", "UTF-8", "latin1")
  base <- data.frame(
    a = c(1.5, NA, 3), b = c("x", NA, "Mart\u00edn"), c = c(TRUE, FALSE, NA),
    d = 1:3, stringsAsFactors = FALSE
  )
  con <- function(f) {
    x <- base
    f(x)
  }
  casos <- list(
    base,
    base[, "a", drop = FALSE],
    base[, 0L, drop = FALSE],
    con(function(x) { x$e <- c(latin1, "y", NA); x }),
    con(function(x) { x$z <- complex(real = 1:3, imaginary = 0); x }),
    # De aca en adelante se resuelve con `[`.
    con(function(x) { x$f <- factor(x$b); x }),
    con(function(x) { x$g <- as.Date("2024-01-31") + 0:2; x }),
    con(function(x) { x$h <- as.POSIXct("2024-01-31", tz = "UTC") + 0:2; x }),
    con(function(x) { x$l <- I(list(1, "a", NULL)); x }),
    con(function(x) { x$m <- matrix(1:6, 3); x }),
    con(function(x) { x$n <- c(u = 1, v = 2, w = 3); x }),
    con(function(x) { rownames(x) <- c("r1", "r2", "r3"); x }),
    con(function(x) { class(x) <- c("tabla_propia", "data.frame"); x }),
    con(function(x) { attr(x, "marca") <- "m"; x }),
    con(function(x) { names(x)[[2L]] <- "a"; x })
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    casos[[length(casos) + 1L]] <- con(function(x) {
      x$i <- bit64::as.integer64(c("-1", "9007199254740993", NA))
      x
    })
  }
  for (k in seq_along(casos)) {
    for (i in list(1L, 2L, 3L, 2, 4L, NA_integer_, c(1L, 3L))) {
      expect_true(.igual_a_base(
        function() .fila_de_tabla(casos[[k]], i),
        function() casos[[k]][i, , drop = FALSE]
      ), info = paste("caso", k, "fila", paste(i, collapse = ",")))
    }
  }
  expect_true(.fila_por_via_corta(base, 2L))
  expect_false(.fila_por_via_corta(casos[[6L]], 2L))
  # La tabla de columnas del perfil es la que paga esto, unas treinta veces
  # por `planificar_limpieza()` sobre la tabla de ejemplo. Si una columna nueva
  # de esa tabla trae atributos -un factor, una fecha, una lista-, la fila vuelve
  # en silencio a `[`: la salida no cambia, el costo si.
  perfil <- perfilar(datos_operativos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_true(.fila_por_via_corta(perfil$columnas, 3L))
})

test_that("un valor sin coma no se parte en nombres unidos por coma", {
  # `.es_columna_de_nombres()` contesta sin llamar a `.separar_nombres_columnas()`
  # cuando el valor no tiene una coma: solo le pregunta si da DOS piezas o mas,
  # y sin coma da a lo sumo una. Si un dia la separacion aprende otro separador,
  # esta prueba avisa que la respuesta corta quedo mal.
  latin1 <- iconv("Mart\u00edn", "UTF-8", "latin1")
  roto <- rawToChar(as.raw(c(0x61, 0xe9, 0x62)))
  nombres <- c("a", "b", "a b", "a+b", "c | d", "x->y", " a", "ab", "aa",
               latin1, roto, "<lupa-byte:61E962>", "e, f", "(tabla)")
  valores <- c(
    nombres[!grepl(",", nombres, fixed = TRUE, useBytes = TRUE)], "a + b", "a  b", "abab", "aab", "aaa", "a b a", "a|b", "a->b",
    " a ", "a\tb", "a;b", paste0(latin1, " b"), paste0(roto, "+a"),
    paste0(roto, roto), "e", "f", "e f", "(tabla: t1)", "texto libre sin nada"
  )
  for (v in valores) {
    expect_false(grepl(",", v, fixed = TRUE, useBytes = TRUE))
    expect_lte(length(.separar_nombres_columnas(v, nombres, sep = ",")), 1L)
  }
  # Control: con una coma si se parte, que es la pregunta que se sigue haciendo.
  expect_identical(.separar_nombres_columnas("a,b", nombres, sep = ","), c("a", "b"))
  expect_true(.es_columna_de_nombres(c("a, b", "e, f", "a + b", "c | d"), nombres))
  expect_false(.es_columna_de_nombres(c("a", "texto libre sin nada"), nombres))
})

test_that("los atajos del pliegue para comparar dan lo mismo que el camino largo", {
  # Pesada para CRAN (mas de 2 s medido): corre con NOT_CRAN=true.
  skip_on_cran()
  # `.plegar_para_comparar()` contesta la entrada toda ASCII con la caja ASCII, y
  # la chica con el mapa reducido a los caracteres que aparecen. Lo comparado es
  # el valor, los bytes y la marca de cada texto: `identical()` sobre textos
  # traduce a UTF-8 antes de comparar, y no ve una marca distinta.
  u <- function(...) intToUtf8(c(...))
  huella <- function(x) {
    list(x, Encoding(x), lapply(x, function(z) if (is.na(z)) NA else charToRaw(z)))
  }
  latin1 <- iconv(u(0x4A, 0x6F, 0x73, 0xE9, 0x20, 0xC1, 0x52, 0x45, 0x41), "UTF-8", "latin1")
  roto <- rawToChar(as.raw(c(0x4A, 0x6F, 0x73, 0xE9, 0x20, 0x50)))
  roto_utf8 <- roto
  Encoding(roto_utf8) <- "UTF-8"
  roto_bytes <- roto
  Encoding(roto_bytes) <- "bytes"
  entradas <- list(
    character(), NA_character_, c(NA, NA),
    c("abc", "Juan PEREZ", "", NA, "771.771-01", "A-Z [x] \\ ^"),
    # El prefijo reservado y los escapes publicados son ASCII; desescapados no.
    c("<lupa-byte:E9>", "<LUPA-BYTE:c3a9>", "<U+00A0>x", "a\\nb", "\\xe9"),
    c("Jos<U+00E9>", "a<lupa-byte:E9>b", "x\\u00e9y", "<lupa-byte:ZZ>", "<lupa-byte:E>"),
    c(u(0x50, 0x61, 0x74, 0x72, 0xF3, 0x6E), u(0xC1, 0x52, 0x45, 0x41), "abc"),
    c(latin1, "abc"), latin1,
    c(roto, roto_utf8, roto_bytes, rawToChar(as.raw(c(0x8A, 0x9A, 0x8C, 0x9C, 0x41))),
      rawToChar(as.raw(c(0xC3, 0xA9, 0x41, 0xE9))), rawToChar(as.raw(c(0xC9, 0xA0, 0x42))),
      rawToChar(as.raw(c(0x50, 0xC3, 0xA9, 0x52)))),
    c(roto, "ABC"),
    # Mayusculas fuera de ASCII, pliegues de varios caracteres y letras sin pliegue.
    c(u(0x0130, 0x0049), u(0x1E9E), u(0x0391, 0x03A3, 0x03C2), u(0x0414, 0x0416),
      u(0xFF21, 0xFF22), u(0x0531), u(0x01C4, 0x01C5), u(0x00DF, 0x0132)),
    c(u(0x4E2D, 0x6587), u(0x0E01), u(0x1F600), u(0x2603), u(0x00A0, 0x3000)),
    c(u(0x4A, 0x6F, 0x73, 0x02CA, 0x65), u(0x31, 0x02B9, 0x32), u(0x02BC, 0x6E)),
    c(u(0x53, 0x0301, 0x61, 0x6E, 0x63, 0x68, 0x65, 0x7A), u(0x65, 0x0300)),
    c(u(0xFB01), u(0x0149, 0x41), u(0x01C6), u(0x2474)),
    c(a = "ABC", b = u(0xC9)), 123.5, factor(c("B", u(0xC9))),
    # Mas grande que el tope del mapa reducido: va por el mapa entero.
    rep(c(u(0x50, 0x61, 0x74, 0x72, 0xF3, 0x6E, 0x20), "abc"), 6000)
  )
  # Cada caracter de los mapas, solo y todos juntos: el mapa reducido tiene que
  # dar lo mismo que el entero para cualquiera de ellos.
  mapas <- unique(c(
    strtoi(names(.PLIEGUE_COMPARAR_UNO), base = 16L),
    strtoi(names(.PLIEGUE_COMPARAR_VARIOS), base = 16L),
    strtoi(names(.MAPA_TRANSLITERACION_ASCII), base = 16L),
    strtoi(names(.MAPA_MINUSCULAS_ACENTUADAS), base = 16L),
    strtoi(names(.MAPA_MINUSCULAS_OTROS_ALFABETOS), base = 16L)
  ))
  sueltos <- intToUtf8(mapas, multiple = TRUE)
  entradas <- c(entradas, list(sueltos, paste0("x", sueltos, "Y"),
                               paste0(sueltos, collapse = "")))
  comparar <- function(lugar) {
    for (k in seq_along(entradas)) {
      for (desescapar in c(FALSE, TRUE)) for (invalidos in c("por_byte", "entero")) {
        expect_identical(
          huella(.plegar_para_comparar(entradas[[k]], desescapar, invalidos)),
          huella(.plegar_para_comparar(entradas[[k]], desescapar, invalidos,
                                       atajos = FALSE)),
          info = paste(lugar, "entrada", k, desescapar, invalidos)
        )
      }
    }
  }
  comparar("locale de la sesion")
  # Bajo `C` `chartr()` no tiene locale multibyte: lee el texto marcado como UTF-8
  # y lo demas como bytes. Se restaura al salir aunque falle una expectativa.
  con_ctype <- function(locale, codigo) {
    viejo <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", viejo), add = TRUE)
    if (!identical(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)), locale)) {
      return(invisible(FALSE))
    }
    force(codigo)
    invisible(TRUE)
  }
  con_ctype("C", comparar("LC_CTYPE=C"))

  # Las premisas de los atajos, sobre los mapas tal como estan: ningun origen
  # ASCII, ningun guion -que `chartr()` lee como rango- ni origen repetido, y
  # todo ASCII plegado por el camino largo es su caja ASCII.
  codigos <- strtoi(names(.PLIEGUE_COMPARAR_UNO), base = 16L)
  expect_false(any(codigos < 128L))
  expect_false(anyDuplicated(codigos) > 0L)
  expect_false(0x2DL %in% c(codigos, unname(.PLIEGUE_COMPARAR_UNO)))
  expect_false(any(strtoi(names(.PLIEGUE_COMPARAR_VARIOS), base = 16L) < 128L))
  ascii <- intToUtf8(1:127, multiple = TRUE)
  expect_identical(.plegar_para_comparar(ascii, atajos = FALSE), .minusculas_ascii(ascii))
  expect_identical(.plegar_para_comparar(ascii), .minusculas_ascii(ascii))
})
