# Genera `R/normalizacion-unicode.R`, las dos tablas con que `normalizacion()`
# hace la descomposicion canonica y ordena sus marcas: la que promete que NFC y
# NFD son equivalentes en el subconjunto latino que cubre el paquete.
#
# - `.normalizacion_decomposiciones`: la descomposicion canonica completa (NFD)
#   de cada letra latina precompuesta cuyo NFD es una base anterior a U+0300
#   -latin o alfabeto fonetico, como la ezh de `01EF`- seguida de marcas de
#   U+0300 a U+0332, mas los dos simbolos que Unicode declara
#   canonicamente equivalentes a una letra latina: el signo Kelvin (U+212A, es
#   la K) y el signo Angstrom (U+212B, es la A con anillo).
# - `.normalizacion_clases_combinantes`: la clase combinante canonica de cada
#   marca de U+0300 a U+0332, que decide el orden canonico de las marcas.
#
# La tabla de clases estaba escrita a mano y 25 de sus 51 valores no eran los de
# Unicode: `a` con agudo y macron debajo en NFC y la misma en NFD no se
# igualaban con `acentos = FALSE`, y dos marcas de clase 220 en distinto orden
# -canonicamente distintas- si. Ahora la da el oraculo.
#
# Se corre desde la raiz del paquete, o con la raiz como argumento:
#   Rscript data-raw/normalizacion_unicode.R [raiz]
# Necesita `stringi`, que no es dependencia del paquete en tiempo de ejecucion:
# el calculo se hace aca, una vez, y el paquete lleva el resultado como numeros
# para no depender de ninguna biblioteca ni de la version de Unicode de la
# plataforma. La prueba `test-O144-ronda-29t-texto.R` compara las tablas con
# `stringi` cuando esta instalado.

argumentos <- commandArgs(trailingOnly = TRUE)
raiz <- if (length(argumentos)) argumentos[[1L]] else "."

marcas <- 0x0300:0x0332

# Clase combinante canonica: ICU la expone como propiedad `ccc` en sus
# expresiones regulares.
caracteres_marca <- intToUtf8(marcas, multiple = TRUE)
clases <- rep(NA_integer_, length(marcas))
for (valor in 0:254) {
  casa <- stringi::stri_detect_regex(
    caracteres_marca, sprintf("^\\p{ccc=%d}$", valor)
  )
  clases[casa] <- valor
}
if (anyNA(clases) || any(clases == 0L)) {
  stop("Una marca de U+0300 a U+0332 quedo sin clase combinante.")
}

# Descomposiciones: el latin precompuesto y los simbolos de letras.
candidatos <- c(0x00C0:0x024F, 0x1E00:0x1EFF, 0x2100:0x214F)
nfd <- stringi::stri_trans_nfd(intToUtf8(candidatos, multiple = TRUE))
puntos <- lapply(nfd, utf8ToInt)
cubre <- vapply(seq_along(candidatos), function(i) {
  p <- puntos[[i]]
  !identical(p, candidatos[[i]]) && p[[1L]] < 0x0300 &&
    all(p[-1L] %in% marcas)
}, logical(1L))
claves <- candidatos[cubre]
valores <- puntos[cubre]
if (!all(c(0x212A, 0x212B) %in% claves)) {
  stop("Faltan los simbolos Kelvin y Angstrom entre las descomposiciones.")
}

lineas_decomposiciones <- paste0(
  "    \"", claves, "\" = c(",
  vapply(valores, paste, character(1L), collapse = ", "), ")",
  c(rep(",", length(claves) - 1L), "")
)
entradas_clases <- paste0("\"", marcas, "\" = ", clases, "L")
grupos <- split(entradas_clases, ceiling(seq_along(entradas_clases) / 4L))
lineas_clases <- paste0(
  "  ", vapply(grupos, paste, character(1L), collapse = ", "),
  c(rep(",", length(grupos) - 1L), "")
)

writeLines(c(
  "# Generado por `data-raw/normalizacion_unicode.R`; no editar a mano.",
  "#",
  paste0("# Descomposicion canonica (NFD) del latin precompuesto y clase ",
         "combinante canonica"),
  paste0("# de las marcas U+0300 a U+0332, segun Unicode ",
         stringi::stri_info()$Unicode.version,
         ". Puntos de codigo en decimal."),
  ".normalizacion_decomposiciones <-",
  "  list(",
  lineas_decomposiciones,
  "  )",
  "",
  ".normalizacion_clases_combinantes <- c(",
  lineas_clases,
  ")"
), file.path(raiz, "R", "normalizacion-unicode.R"))
cat("descomposiciones:", length(claves), " clases:", length(marcas), "\n")
