# Genera `R/mapa-pliegue-comparar.R`, el mapa con que la proteccion de datos
# personales iguala las variantes cosmeticas de un valor antes de compararlo.
# Para cada letra o digito de los rangos de abajo se calcula su forma plegada:
# plegado de caja completo de Unicode, descomposicion de compatibilidad (NFKD),
# sin marcas combinantes, y otra vez plegado y descomposicion por si la primera
# pasada dejo algo en mayuscula. Asi quedan iguales la mayuscula y la minuscula
# de cualquier alfabeto con caja, la letra con y sin diacriticos, la ligadura y
# sus letras, el ancho completo y el medio ancho, y las letras matematicas.
#
# Se corre desde la raiz del paquete. Necesita `stringi`, que no es dependencia
# del paquete: el calculo se hace aca, una vez, y el paquete lleva el resultado
# como puntos de codigo para no depender de ninguna biblioteca ni de la version
# de Unicode de la plataforma.
#
# Salen dos tablas. `.PLIEGUE_COMPARAR_UNO` lleva los casos de un caracter a un
# caracter dentro del plano basico, que se aplican con `chartr()`.
# `.PLIEGUE_COMPARAR_VARIOS` lleva el resto -un caracter que se vuelve varios,
# como la ligadura `ffi`, o uno fuera del plano basico, como las letras
# matematicas-, que se aplica caracter por caracter: `chartr()` trabaja con
# `wchar_t`, que en Windows es de 16 bits.

rangos <- c(
  0x00A0:0x2FFF, # latin, griego, cirilico, armenio, georgiano, compatibilidad...
  0x3000:0x30FF, # puntuacion CJK, hiragana y katakana
  0x3130:0x318F, # jamo de compatibilidad
  0xA640:0xA69F, # cirilico extendido B
  0xA720:0xA7FF, # latin extendido D
  0xAB30:0xABBF, # latin extendido E y cherokee en minuscula
  0xF900:0xFAFF, # ideogramas de compatibilidad
  0xFB00:0xFDFF, # ligaduras latinas, armenias, hebreas y arabes
  0xFE70:0xFEFF, # formas de presentacion arabes
  0xFF00:0xFFEF, # ancho completo y medio ancho
  0x1D400:0x1D7FF, # letras y digitos matematicos
  0x1E900:0x1E95F  # adlam
)
caracteres <- intToUtf8(rangos, multiple = TRUE)
asignados <- stringi::stri_detect_regex(caracteres, "^[\\p{L}\\p{N}]$")
plegar <- function(x) {
  for (vuelta in 1:2) {
    x <- stringi::stri_trans_nfkd(stringi::stri_trans_casefold(x))
    x <- stringi::stri_replace_all_regex(x, "\\p{M}", "")
  }
  x
}
plegados <- plegar(caracteres)
# La i sin punto no tiene descomposicion ni otra caja: se iguala a la i, que es
# como se escribe el mismo nombre sin teclado turco.
plegados[rangos == 0x0131] <- "i"
cambia <- asignados & plegados != caracteres & nzchar(plegados) &
  stringi::stri_detect_regex(plegados, "^[\\p{L}\\p{N}]+$")
puntos <- lapply(plegados, utf8ToInt)
uno <- cambia & lengths(puntos) == 1L & rangos <= 0xFFFF &
  vapply(puntos, function(p) length(p) == 1L && p[[1L]] <= 0xFFFF, logical(1L))
varios <- cambia & !uno

formatear <- function(nombres, valores, por_linea) {
  entradas <- paste0("`", nombres, "` = ", valores)
  lineas <- vapply(
    split(entradas, ceiling(seq_along(entradas) / por_linea)),
    function(grupo) paste0("  ", paste(grupo, collapse = ", ")),
    character(1L)
  )
  paste0(lineas, c(rep(",", length(lineas) - 1L), ""))
}
lineas_uno <- formatear(
  sprintf("%04X", rangos[uno]),
  sprintf("0x%04X", vapply(puntos[uno], `[[`, integer(1L), 1L)),
  4L
)
lineas_varios <- formatear(
  sprintf("%04X", rangos[varios]),
  vapply(puntos[varios], function(p) {
    paste0("\"", paste(sprintf("%04X", p), collapse = " "), "\"")
  }, character(1L)),
  3L
)

writeLines(c(
  "# Generado por `data-raw/mapa_pliegue_comparar.R`; no editar a mano.",
  "#",
  paste0("# Forma plegada de cada letra o digito, segun Unicode ",
         stringi::stri_info()$Unicode.version, ": plegado de caja,"),
  "# descomposicion de compatibilidad y sin marcas combinantes. Puntos de codigo",
  "# hexadecimales, para que el archivo sea ASCII.",
  ".PLIEGUE_COMPARAR_UNO <- c(",
  lineas_uno,
  ")",
  "",
  ".PLIEGUE_COMPARAR_VARIOS <- c(",
  lineas_varios,
  ")"
), "R/mapa-pliegue-comparar.R")
cat("uno:", sum(uno), " varios:", sum(varios), "\n")
