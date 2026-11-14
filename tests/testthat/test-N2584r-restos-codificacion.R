testthat::skip_on_cran()

# Una celda declarada `latin1` con los bytes dados.
.n2584r_latin1 <- function(bytes) {
  x <- rawToChar(as.raw(bytes))
  Encoding(x) <- "latin1"
  x
}

# La misma celda escrita en UTF-8, como la leen las acciones de texto: los
# cinco bytes que Windows-1252 no define son los puntos C1 de igual numero.
.n2584r_gemela <- function(bytes) {
  x <- intToUtf8(bytes)
  Encoding(x) <- "UTF-8"
  x
}

.n2584r_perfil <- function(datos) {
  perfilar(datos, fecha = "2026-01-15")
}

.n2584r_sin_windows_1252 <- c(0x81L, 0x8DL, 0x8FL, 0x90L, 0x9DL)

test_that("la misma celda en latin1 y en UTF-8 es un valor aunque traiga un byte que Windows-1252 no define", {
  for (byte in .n2584r_sin_windows_1252) {
    bytes <- c(0xC3L, byte, 0x52L, 0x45L, 0x41L)
    latin1 <- .n2584r_latin1(bytes)
    gemela <- .n2584r_gemela(bytes)
    datos <- data.frame(
      t = c(latin1, gemela, "ok", "otro valor"), stringsAsFactors = FALSE
    )
    perfil <- .n2584r_perfil(datos)
    expect_identical(
      perfil$columnas$n_distintos[perfil$columnas$columna == "t"], 3L,
      info = sprintf("byte %02X", byte)
    )
    expect_identical(as.integer(perfil$general$filas_duplicadas), 1L,
                     info = sprintf("byte %02X", byte))
  }
})

test_that("la celda latin1 no es el texto de cuatro letras con que R escribe su byte", {
  latin1 <- .n2584r_latin1(c(0xC3L, 0x81L, 0x52L, 0x45L, 0x41L))
  literal <- paste0(intToUtf8(0xC3L), "<81>REA")
  Encoding(literal) <- "UTF-8"
  datos <- data.frame(t = c(latin1, literal, "ok"), n = c(1L, 1L, 2L),
                      stringsAsFactors = FALSE)
  perfil <- .n2584r_perfil(datos)
  expect_identical(perfil$columnas$n_distintos[perfil$columnas$columna == "t"], 3L)
  expect_identical(as.integer(perfil$general$filas_duplicadas), 0L)
})

test_that("las filas que solo difieren en la marca se marcan como duplicadas al aplicar", {
  bytes <- c(0x78L, 0x8DL, 0x79L)
  datos <- data.frame(
    t = c(.n2584r_latin1(bytes), .n2584r_gemela(bytes), "ok"),
    n = c(1L, 1L, 2L), stringsAsFactors = FALSE
  )
  perfil <- .n2584r_perfil(datos)
  plan <- planificar_limpieza(perfil, datos)
  marcar <- plan$estrategia == "marcar_filas_duplicadas"
  expect_identical(sum(marcar), 1L)
  expect_identical(as.numeric(plan$n_afectadas[marcar]), 2)
  plan$aplicar <- marcar
  resultado <- aplicar(plan, datos)
  fila <- resultado$registro[resultado$registro$estrategia == "marcar_filas_duplicadas", ]
  expect_identical(as.character(fila$estado), "ejecutada")
  expect_identical(as.numeric(fila$n_cambiadas), 2)
})

test_that("una columna latin1 y su gemela UTF-8 son columnas duplicadas tambien con esos bytes", {
  for (byte in .n2584r_sin_windows_1252) {
    bytes <- c(0xC3L, byte, 0x52L, 0x45L, 0x41L)
    datos <- data.frame(
      a = c(.n2584r_latin1(bytes), "ok", "otro valor", "tres"),
      b = c(.n2584r_gemela(bytes), "ok", "otro valor", "tres"),
      stringsAsFactors = FALSE
    )
    perfil <- .n2584r_perfil(datos)
    expect_identical(nrow(perfil$general$columnas_duplicadas), 1L,
                     info = sprintf("byte %02X", byte))
  }
})

test_that("normalizar dos celdas que ya eran el mismo texto no cuenta irreversibles", {
  bytes <- c(0x61L, 0xA0L, 0xC3L, 0x81L, 0x52L, 0x45L, 0x41L)
  datos <- data.frame(
    t = c(.n2584r_latin1(bytes), .n2584r_gemela(bytes), "ok", "otro valor"),
    stringsAsFactors = FALSE
  )
  resultado <- aplicar(planificar_limpieza(.n2584r_perfil(datos), datos), datos)
  texto <- resultado$registro[!is.na(resultado$registro$columna) &
                                resultado$registro$columna == "t", ]
  expect_true(nrow(texto) > 0L)
  expect_identical(as.numeric(texto$n_no_reversibles), rep(0, nrow(texto)))
})

test_that("los invisibles de transporte fuera del catalogo se cuentan y se quitan", {
  codigos <- c(0x2061L, 0x2062L, 0x2063L, 0x2064L, 0x206AL, 0x206FL,
               0x115FL, 0x1160L, 0x3164L, 0xFFA0L, 0xE0001L)
  for (codigo in codigos) {
    celda <- intToUtf8(c(0x61L, codigo, 0x62L))
    datos <- data.frame(t = c(celda, "ok", "otro valor"), stringsAsFactors = FALSE)
    perfil <- .n2584r_perfil(datos)
    fila <- perfil$columnas[perfil$columnas$columna == "t", ]
    info <- sprintf("U+%04X", codigo)
    expect_identical(fila$n_controles_invisibles, 1L, info = info)
    expect_identical(fila$n_invisibles_eliminables, 1L, info = info)
    plan <- planificar_limpieza(perfil, datos)
    quitar <- plan$estrategia == "eliminar_controles_invisibles"
    expect_true(any(quitar & plan$aplicar), info = info)
    resultado <- aplicar(plan, datos)
    expect_identical(as.character(resultado$datos$t[[1L]]), "ab", info = info)
  }
})

test_that("las etiquetas de una bandera se informan y se conservan", {
  # La bandera de Escocia: U+1F3F4 y las etiquetas g, b, s, c, t con la de cierre.
  bandera <- intToUtf8(c(0x1F3F4L, 0xE0067L, 0xE0062L, 0xE0073L, 0xE0063L,
                         0xE0074L, 0xE007FL))
  datos <- data.frame(t = c(paste0("hola ", bandera), "ok", "otro valor"),
                      stringsAsFactors = FALSE)
  perfil <- .n2584r_perfil(datos)
  fila <- perfil$columnas[perfil$columnas$columna == "t", ]
  expect_identical(fila$n_controles_invisibles, 1L)
  expect_identical(fila$n_invisibles_eliminables, 0L)
  expect_identical(fila$n_espacios_invisibles, 0L)
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "controles_invisibles", ]
  expect_identical(nrow(hallazgo), 1L)
  expect_identical(as.character(hallazgo$severidad), "sospechoso")
  expect_false(grepl("ZWJ/ZWNJ). Unen", hallazgo$descripcion, fixed = TRUE))

  # Junto a un espacio de ancho cero, la accion quita ese y deja la bandera.
  con_zwsp <- data.frame(
    t = c(paste0("hola ", bandera), intToUtf8(c(0x61L, 0x200BL, 0x62L)), "ok"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(.n2584r_perfil(con_zwsp), con_zwsp)
  resultado <- aplicar(plan, con_zwsp)
  expect_identical(utf8ToInt(resultado$datos$t[[1L]]),
                   utf8ToInt(paste0("hola ", bandera)))
  expect_identical(as.character(resultado$datos$t[[2L]]), "ab")
})

test_that("un control que ninguna accion quita se informa sin recomendar quitarlo", {
  # Un euro, U+001F y la A con anillo: sin el control, la celda se leeria como
  # una letra mal convertida, y la accion lo conserva.
  celda <- intToUtf8(c(0x20ACL, 0x1FL, 0xC5L, 0x53L, 0x45L))
  datos <- data.frame(t = c(celda, "ok", "otro valor", "tres", "cuatro"),
                      stringsAsFactors = FALSE)
  perfil <- .n2584r_perfil(datos)
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "controles_invisibles", ]
  expect_identical(nrow(hallazgo), 1L)
  expect_identical(as.character(hallazgo$severidad), "error")
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(plan$estrategia %in% c(
    "eliminar_controles_invisibles", "normalizar_espacios_invisibles"
  )))
  expect_false(startsWith(hallazgo$sugerencia, "Eliminar"))
})

test_that("la decodificacion de entidades corre antes que la caja aunque el orden editado diga otra cosa", {
  datos <- data.frame(
    t = c("x &amp; y", "jos&eacute; perez", "ana", "ANA", "Ana", "ana", "beto"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(.n2584r_perfil(datos), datos)
  plan$aplicar <- plan$estrategia %in% c("decodificar_entidades_html", "convertir_titulo")
  esperado <- aplicar(plan, datos)
  titulo <- which(plan$estrategia == "convertir_titulo")
  decodificar <- which(plan$estrategia == "decodificar_entidades_html")
  editado <- plan
  editado$orden[titulo] <- plan$orden[decodificar]
  editado$orden[decodificar] <- plan$orden[titulo]
  resultado <- aplicar(editado, datos)
  expect_identical(as.character(resultado$datos$t), as.character(esperado$datos$t))
  expect_identical(as.character(resultado$datos$t[[2L]]),
                   paste0("Jos", intToUtf8(0xE9L), " Perez"))
  expect_identical(as.character(resultado$registro$estrategia[1:2]),
                   c("decodificar_entidades_html", "convertir_titulo"))
})
