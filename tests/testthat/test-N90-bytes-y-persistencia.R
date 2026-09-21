# Vuelta n90. Cuatro arreglos, y los valores se construyen con `as.raw()` y se
# comparan por bytes o por `Encoding()`: es la unica forma que no engania cuando
# lo que se mide ES la codificacion.

.n90_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

.N90_SEC <- c(0x61, 0xc3, 0xb1, 0x6f)   # "ano" con enie
.N90_ROTO <- c(0x41, 0xff, 0x42)        # bytes invalidos

.n90_tiene_enie <- function(x) {
  if (is.na(x)) return(FALSE)
  aguja <- c(0xc3L, 0xb1L)
  cuerpo <- as.integer(charToRaw(x))
  n <- length(aguja)
  length(cuerpo) >= n && any(vapply(
    seq_len(length(cuerpo) - n + 1L),
    function(k) identical(cuerpo[k:(k + n - 1L)], aguja), logical(1L)
  ))
}

.n90_analisis <- function(valores) {
  suppressWarnings(analizar(
    data.frame(v = valores, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC")
  ))
}

test_that("la clave distingue lo declarado `bytes` de los mismos bytes sin declarar", {
  # Sobre una cadena TODA invalida los dos escapes coincidian -cada byte
  # escapado- y los mismos bytes declarados y sin declarar daban la misma clave.
  # El declarado usa ahora hexadecimal, que es como R lo imprime: `A\xffB`.
  declarado <- .n90_bytes(.N90_ROTO)
  crudo <- rawToChar(as.raw(.N90_ROTO))
  expect_false(declarado == crudo)
  expect_false(identical(.clave_bytes(declarado), .clave_bytes(crudo)))
  valores <- c(declarado, crudo, "z")
  expect_identical(
    length(unique(.clave_bytes(valores))), length(unique(valores))
  )
  # Y la clave del declarado dice lo mismo que la consola.
  expect_identical(.clave_bytes(declarado), format(declarado, justify = "none"))
})

test_that("el hallazgo `constante` y su traza cuentan lo mismo", {
  # Era consecuencia de lo anterior: la clave fundia las tres filas, el hallazgo
  # decia 3 y la traza -que usa el `==` de R, que NO funde- respaldaba 1.
  declarado <- .n90_bytes(.N90_ROTO)
  crudo <- rawToChar(as.raw(.N90_ROTO))
  perfil <- suppressWarnings(perfilar(
    data.frame(v = c(declarado, crudo, crudo), stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  # `perfilar()` tiene su propia guarda de trazabilidad: si cuenta y traza no
  # coinciden, avisa. Que no avise es la comprobacion.
  expect_no_warning(suppressMessages(perfilar(
    data.frame(v = c(declarado, crudo, crudo), stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )))
  expect_identical(perfil$columnas$n_distintos[[1L]], 2L)
})

test_that("las distribuciones y los niveles no interpretan lo declarado", {
  declarado <- .n90_bytes(.N90_SEC)
  datos <- data.frame(
    v = c(declarado, declarado, "z"), stringsAsFactors = FALSE
  )
  distribucion <- suppressWarnings(distribucion_valores(datos))
  for (valor in distribucion$frecuencias$valor) {
    expect_false(.n90_tiene_enie(valor), info = paste("valor publicado:", valor))
  }
  analisis <- .n90_analisis(c(declarado, declarado, "z"))
  for (nivel in unlist(analisis$variables$niveles_observados)) {
    expect_false(.n90_tiene_enie(nivel), info = paste("nivel publicado:", nivel))
  }
  # Y el caso corriente sigue publicando el caracter.
  # El acentuado se escribe por BYTES, no con un escape: los escapes de este
  # archivo ya se convirtieron una vez en el camino y dejaron la comprobacion
  # midiendo una palabra sin tilde.
  con_tilde <- rawToChar(as.raw(.N90_SEC))
  Encoding(con_tilde) <- "UTF-8"
  normal <- suppressWarnings(distribucion_valores(data.frame(
    v = c(con_tilde, con_tilde, "z"), stringsAsFactors = FALSE
  )))
  expect_true(any(vapply(normal$frecuencias$valor, .n90_tiene_enie, logical(1L))))
})

test_that("el reporte no incrusta los bytes crudos de un valor declarado", {
  # Dentro de un documento UTF-8, los bytes `c3 b1` SE RENDERIZAN como el
  # caracter: el navegador publica lo que la declaracion prohibio interpretar.
  declarado <- .n90_bytes(.N90_SEC)
  analisis <- .n90_analisis(c(declarado, declarado, "z"))
  for (extension in c(".md", ".html")) {
    archivo <- tempfile(fileext = extension)
    on.exit(unlink(archivo), add = TRUE)
    suppressWarnings(reportar(analisis, archivo = archivo))
    cuerpo <- as.integer(readBin(archivo, "raw", file.info(archivo)$size))
    aguja <- c(0xc3L, 0xb1L)
    n <- length(aguja)
    presente <- any(vapply(
      seq_len(length(cuerpo) - n + 1L),
      function(k) identical(cuerpo[k:(k + n - 1L)], aguja), logical(1L)
    ))
    expect_false(presente, info = paste("bytes crudos en el reporte", extension))
  }
})

test_that("un analisis guardado y leido tiene la misma forma", {
  # El Rd afirma que `meta$persistencia` es "el unico campo que un objeto leido
  # tiene y el original no". No era cierto: `copia$x <- NULL` BORRA el elemento,
  # y tres componentes desaparecian de sus nombres.
  analisis <- .n90_analisis(c("a", "b", "a"))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(analisis, archivo, sobrescribir = TRUE)
  leido <- leer_analisis(archivo)

  expect_identical(names(leido), names(analisis))
  expect_identical(
    setdiff(names(leido$meta), names(analisis$meta)), "persistencia"
  )
  distintos <- Filter(
    function(campo) !identical(analisis[[campo]], leido[[campo]]),
    names(analisis)
  )
  expect_identical(distintos, "meta")
})
