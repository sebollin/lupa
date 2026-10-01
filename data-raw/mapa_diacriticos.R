# Genera `R/mapa-diacriticos.R`: para cada letra de los rangos de abajo cuya
# descomposicion canonica (NFD) es una letra base seguida solo de marcas
# combinantes, la letra base. Lo usa la proteccion de datos personales para
# comparar un valor protegido con sus variantes sin tilde: el mapa de
# transliteracion del paquete cubre Latin-1 y Latin Extended-A, y fuera de ahi
# un nombre vietnamita o griego escrito sin sus marcas no se reconocia.
#
# Se corre desde la raiz del paquete. Necesita `stringi`, que no es dependencia
# del paquete: la descomposicion se calcula aca, una vez, y el paquete lleva el
# resultado como puntos de codigo para que no dependa de ninguna biblioteca ni
# de la version de Unicode de la plataforma.

rangos <- c(
  0x0180:0x024F, # Latin Extended-B: la O y la U con cuerno del vietnamita, el pinyin
  0x0370:0x03FF, # Griego: tonos y dieresis
  0x0400:0x04FF, # Cirilico: la e con dieresis, la i breve y las del ucraniano
  0x1E00:0x1EFF, # Latin Extended Additional: el vietnamita
  0x1F00:0x1FFF  # Griego extendido: el politonico
)

caracteres <- intToUtf8(rangos, multiple = TRUE)
descompuestos <- lapply(stringi::stri_trans_nfd(caracteres), utf8ToInt)
base <- vapply(descompuestos, `[[`, integer(1L), 1L)
solo_marcas <- vapply(descompuestos, function(cp) {
  length(cp) > 1L &&
    all(stringi::stri_detect_regex(intToUtf8(cp[-1L], multiple = TRUE), "^\\p{M}$"))
}, logical(1L))
base_es_letra <- stringi::stri_detect_regex(
  intToUtf8(base, multiple = TRUE), "^\\p{L}$"
)
elegidos <- solo_marcas & base_es_letra & base != rangos

entradas <- sprintf("`%04X` = 0x%04X", rangos[elegidos], base[elegidos])
lineas <- vapply(
  split(entradas, ceiling(seq_along(entradas) / 4L)),
  function(grupo) paste0("  ", paste(grupo, collapse = ", ")),
  character(1L)
)
lineas <- paste0(lineas, c(rep(",", length(lineas) - 1L), ""))

writeLines(c(
  "# Generado por `data-raw/mapa_diacriticos.R`; no editar a mano.",
  "#",
  "# Letra con diacriticos -> su letra base, segun la descomposicion canonica de",
  paste0("# Unicode ", stringi::stri_info()$Unicode.version,
         ". Cubre Latin Extended-B, griego, griego extendido,"),
  "# cirilico y Latin Extended Additional. Lo que queda en Latin-1 y Latin",
  "# Extended-A lo resuelve `.MAPA_TRANSLITERACION_ASCII`. Puntos de codigo",
  "# hexadecimales, para que el archivo sea ASCII.",
  ".MAPA_DIACRITICOS <- c(",
  lineas,
  ")"
), "R/mapa-diacriticos.R")
