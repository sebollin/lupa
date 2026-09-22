# N97. Una accion recomendada no puede destruir el valor del usuario.
#
# Dos defectos medidos por la puerta publica, los dos sobre datos perfectamente
# validos, y los dos con la misma raiz: `utf8ToInt()` devuelve `NA` -no vacio,
# no error- sobre lo que no decodifica, y ese `NA` no se queda quieto.
#
#   * `eliminar_controles_invisibles` REEMPLAZABA el valor por la cadena
#     literal "NA". `paste0(intToUtf8(NA), collapse = "")` la produce. Sobre
#     una columna que mezcla `latin1` con UTF-8, tres de siete valores se
#     volvian "NA" al aplicar una accion marcada `recomendada`.
#
#   * `recortar_espacios` convertia texto `latin1` VALIDO en bytes invalidos.
#     `trimws()` sobre un vector que contiene una sola cadena marcada `bytes`
#     devuelve marcadas `bytes` tambien a las `latin1` que viajaban al lado, y
#     ahi su contenido deja de ser recuperable. El destino de una fila lo
#     decidia lo que hubiera en el resto de la tanda.
#
# Los fixtures se construyen con `rawToChar(as.raw(...))`: la fuente del
# paquete es ASCII y hay que poder elegir la marca.

.n97_marcar <- function(bytes, codificacion) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- codificacion
  s
}

# "cano" con la enie, en latin1 y en UTF-8 declarado `bytes`.
.n97_latin1 <- function(cola = integer()) {
  .n97_marcar(c(0x63, 0x61, 0xf1, 0x6f, cola), "latin1")
}
.n97_bytes <- function(cola = integer()) {
  .n97_marcar(c(0x63, 0x61, 0xc3, 0xb1, 0x6f, cola), "bytes")
}

test_that("eliminar_controles_invisibles no reemplaza el valor por la cadena NA", {
  invisible_cero <- intToUtf8(0x200B)
  columna <- c(
    rep(.n97_latin1(), 3L),
    rep(paste0("otro", invisible_cero), 3L),
    "limpio"
  )
  datos <- data.frame(v = columna, stringsAsFactors = FALSE)

  plan <- planificar_limpieza(perfilar(datos), datos)
  elegida <- as.character(plan$estrategia) == "eliminar_controles_invisibles"
  # Si el paquete deja de recomendar esta accion, la prueba no mide: que falle.
  expect_true(any(elegida))
  plan$aplicar <- elegida

  resultado <- aplicar(plan, datos)$datos$v

  # Ningun valor puede haberse vuelto la cadena "NA": el usuario no la escribio.
  expect_equal(sum(!is.na(resultado) & resultado == "NA"), 0L)
  # Y el valor latin1 tiene que seguir siendo el mismo texto.
  esperado <- enc2utf8(.n97_latin1())
  expect_identical(enc2utf8(resultado[[1L]]), esperado)
  # La accion tiene que seguir haciendo su trabajo sobre lo que si puede leer.
  expect_false(grepl(invisible_cero, resultado[[4L]], fixed = TRUE))
})

test_that("recortar_espacios no convierte texto latin1 valido en bytes invalidos", {
  columna <- c(
    rep(.n97_latin1(0x20), 3L),
    .n97_bytes(0x20),
    rep("cano ", 2L),
    "otro"
  )
  datos <- data.frame(v = columna, id = seq_along(columna), stringsAsFactors = FALSE)

  plan <- planificar_limpieza(perfilar(datos), datos)
  elegida <- as.character(plan$estrategia) == "recortar_espacios"
  expect_true(any(elegida))
  plan$aplicar <- elegida

  resultado <- aplicar(plan, datos)$datos$v

  # El contenido de las filas latin1 sigue siendo recuperable como texto.
  esperado <- enc2utf8(.n97_latin1())
  for (i in seq_len(3L)) {
    expect_true(validUTF8(enc2utf8(resultado[[i]])), info = paste("fila", i))
    expect_identical(enc2utf8(resultado[[i]]), esperado, info = paste("fila", i))
  }
  # La fila declarada `bytes` conserva su declaracion: nadie la interpreto.
  expect_identical(Encoding(resultado[[4L]]), "bytes")
  # Y el recorte ocurrio de verdad.
  expect_false(grepl(" $", enc2utf8(resultado[[1L]])))

  # El perfil de los datos limpiados no puede acusar una codificacion rota que
  # la limpieza acaba de fabricar.
  hallazgo <- hallazgos(perfilar(data.frame(v = resultado, stringsAsFactors = FALSE)))
  expect_false(any(as.character(hallazgo$tipo) == "codificacion_invalida"))
})

test_that("el decodificador comun no devuelve NA ni inventa un valor", {
  expect_null(lupa:::.codigos_decodificables(NA_character_))
  # latin1 se convierte sin perder nada.
  expect_identical(
    lupa:::.codigos_decodificables(.n97_latin1()),
    utf8ToInt(enc2utf8(.n97_latin1()))
  )
  # `bytes` que ES UTF-8 valido se puede tratar sin perdida.
  expect_identical(
    lupa:::.codigos_decodificables(.n97_bytes()),
    utf8ToInt(enc2utf8(.n97_latin1()))
  )
  # Lo que no se puede leer devuelve NULL, que es lo unico honesto: quien llama
  # decide, y lo que no puede es inventar un valor.
  expect_null(lupa:::.codigos_decodificables(.n97_marcar(c(0x41, 0xff, 0x42), "bytes")))
  expect_null(lupa:::.codigos_decodificables(.n97_marcar(c(0x63, 0x61, 0x66, 0xe9), "unknown")))
  # Y nunca devuelve un vector con NA adentro.
  for (caso in list("hola", .n97_latin1(), .n97_bytes(), intToUtf8(0x200B))) {
    codigos <- lupa:::.codigos_decodificables(caso)
    if (!is.null(codigos)) expect_false(anyNA(codigos))
  }
})

test_that("una marca bytes en la columna no decide el destino de sus vecinas", {
  # La misma fila latin1, con y sin el vecino declarado `bytes`.
  con_vecino <- lupa:::.recortar_texto(
    c(.n97_latin1(0x20), .n97_bytes(0x20), "otro ")
  )$valor
  sin_vecino <- lupa:::.recortar_texto(
    c(.n97_latin1(0x20), "otro ")
  )$valor
  expect_identical(enc2utf8(con_vecino[[1L]]), enc2utf8(sin_vecino[[1L]]))
})

test_that("una accion no toca las celdas que no necesita tocar", {
  # El arreglo de mas arriba dejo de destruir el valor, pero pasaba TODA celda
  # decodificable por `paste0(intToUtf8(...))`, asi que una celda `latin1` sin
  # ningun control perdia su declaracion y sus bytes (`f1` -> `c3 b1`) al
  # aplicar una accion que no tenia nada que hacer en ella. Y el registro no lo
  # contaba, porque cuenta cambios de TEXTO y como texto era el mismo valor:
  # un `n` que no es el numero de cosas que pasaron.
  invisible_cero <- intToUtf8(0x200B)
  limpia <- .n97_latin1()
  columna <- c(limpia, paste0("otro", invisible_cero), limpia, "limpio")
  datos <- data.frame(v = columna, id = seq_along(columna), stringsAsFactors = FALSE)

  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE), datos)
  elegida <- as.character(plan$estrategia) == "eliminar_controles_invisibles"
  expect_true(any(elegida))
  plan$aplicar <- elegida

  resultado <- aplicar(plan, datos)
  salida <- resultado$datos$v

  # Las celdas sin control vuelven IDENTICAS, byte por byte y con su marca.
  for (i in c(1L, 3L, 4L)) {
    expect_identical(charToRaw(salida[[i]]), charToRaw(columna[[i]]),
                     info = paste("celda", i))
    expect_identical(Encoding(salida[[i]]), Encoding(columna[[i]]),
                     info = paste("celda", i))
  }
  # Y la que si lo tenia se limpio.
  expect_false(grepl(invisible_cero, salida[[2L]], fixed = TRUE))

  # El numero informado es el numero de celdas que cambiaron.
  registro <- as.data.frame(resultado$registro)
  expect_equal(registro$n_cambiadas[[1L]], 1L)
})

test_that("normalizar espacios invisibles tampoco reescribe lo que no cambia", {
  espacio_duro <- intToUtf8(0x00A0)
  limpia <- .n97_latin1()
  columna <- c(limpia, paste0("a", espacio_duro, "b"), limpia, "limpio")
  antes <- lupa:::.normalizar_espacios_invisibles(columna)$valor
  for (i in c(1L, 3L, 4L)) {
    expect_identical(charToRaw(antes[[i]]), charToRaw(columna[[i]]),
                     info = paste("celda", i))
  }
  expect_false(grepl(espacio_duro, antes[[2L]], fixed = TRUE))
})

test_that("ningun ejecutor de texto deja que un valor bytes decida el destino de sus vecinos", {
  # Una operacion vectorizada sobre un vector con UN valor marcado `bytes`
  # cambia de modo para todo el vector: `gsub(perl = TRUE)` devolvia ilegible
  # la celda `latin1` de al lado y `tolower()`/`toupper()` abortaban. Se habia
  # arreglado en el recorte con una copia propia y el defecto seguia en los
  # otros. La primera medicion de estos ejecutores dio "ok" porque el dato no
  # tenia nada que cada uno cambiara: por eso esta prueba exige, ademas, que
  # la celda efectivamente cambie.
  enie <- c(0x63, 0x61, 0xf1, 0x6f)
  roto <- .n97_marcar(c(0x72, 0x6f, 0x74, 0x6f, 0xff, 0x20), "bytes")
  casos <- list(
    recortar = list(c(enie, 0x20), function(x) lupa:::.recortar_texto(x)$valor),
    separadores = list(c(enie, 0x09, 0x64),
                       function(x) lupa:::.reemplazar_separadores(x)$valor),
    minusculas = list(c(0x43, 0x41, 0xd1, 0x4f), function(x)
      lupa:::.transformar_capitalizacion(x, "convertir_minusculas", list())$valor),
    mayusculas = list(enie, function(x)
      lupa:::.transformar_capitalizacion(x, "convertir_mayusculas", list())$valor),
    titulo = list(enie, function(x)
      lupa:::.transformar_capitalizacion(x, "convertir_titulo", list())$valor)
  )
  for (nombre in names(casos)) {
    celda <- .n97_marcar(casos[[nombre]][[1L]], "latin1")
    transformar <- casos[[nombre]][[2L]]
    con_vecino <- transformar(c(celda, roto, "ok"))
    sin_vecino <- transformar(c(celda, "ok"))
    # El ejecutor tiene que haber hecho algo: si no, esto no mide nada.
    expect_false(identical(enc2utf8(sin_vecino[[1L]]), enc2utf8(celda)), info = nombre)
    # Y el vecino no puede cambiar el resultado.
    expect_identical(enc2utf8(con_vecino[[1L]]), enc2utf8(sin_vecino[[1L]]), info = nombre)
    expect_true(validUTF8(enc2utf8(con_vecino[[1L]])), info = nombre)
    # Lo que no se puede leer conserva su declaracion.
    expect_identical(Encoding(con_vecino[[2L]]), "bytes", info = nombre)
  }
})
