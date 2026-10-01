# El mapa de transliteracion no sirve para bajar caja: reemplazar `E` aguda por
# `E` pierde justo la distincion que este paso debe conservar. Estas entradas son
# puntos de codigo, no caracteres literales, para que el mapa sea auditable y el
# fuente siga siendo ASCII fuera de comentarios.
.MAPA_MINUSCULAS_ACENTUADAS <- c(
  `00C0` = 0x00E0, `00C1` = 0x00E1, `00C2` = 0x00E2, `00C3` = 0x00E3,
  `00C4` = 0x00E4, `00C5` = 0x00E5, `00C6` = 0x00E6, `00C7` = 0x00E7,
  `00C8` = 0x00E8, `00C9` = 0x00E9, `00CA` = 0x00EA, `00CB` = 0x00EB,
  `00CC` = 0x00EC, `00CD` = 0x00ED, `00CE` = 0x00EE, `00CF` = 0x00EF,
  `00D0` = 0x00F0, `00D1` = 0x00F1, `00D2` = 0x00F2, `00D3` = 0x00F3,
  `00D4` = 0x00F4, `00D5` = 0x00F5, `00D6` = 0x00F6, `00D8` = 0x00F8,
  `00D9` = 0x00F9, `00DA` = 0x00FA, `00DB` = 0x00FB, `00DC` = 0x00FC,
  `00DD` = 0x00FD, `00DE` = 0x00FE,
  `0100` = 0x0101, `0102` = 0x0103, `0104` = 0x0105, `0106` = 0x0107,
  `0108` = 0x0109, `010A` = 0x010B, `010C` = 0x010D, `010E` = 0x010F,
  `0110` = 0x0111, `0112` = 0x0113, `0114` = 0x0115, `0116` = 0x0117,
  `0118` = 0x0119, `011A` = 0x011B, `011C` = 0x011D, `011E` = 0x011F,
  `0120` = 0x0121, `0122` = 0x0123, `0124` = 0x0125, `0126` = 0x0127,
  `0128` = 0x0129, `012A` = 0x012B, `012C` = 0x012D, `012E` = 0x012F,
  # U+0130, la I mayuscula con punto, es la unica mayuscula latina que
  # faltaba, y es justo la que motiva todo esto: sin entrada propia caia en
  # `tolower()`, que la baja a `i` en un locale UTF-8 y la deja intacta bajo
  # `C`. Se fija a `i` -que es lo que dan glibc y la intencion del usuario al
  # comparar- para que el resultado no dependa de la configuracion regional.
  `0130` = 0x0069,
  `0132` = 0x0133, `0134` = 0x0135, `0136` = 0x0137, `0139` = 0x013A,
  `013B` = 0x013C, `013D` = 0x013E, `013F` = 0x0140, `0141` = 0x0142,
  `0143` = 0x0144, `0145` = 0x0146, `0147` = 0x0148, `014A` = 0x014B,
  `014C` = 0x014D, `014E` = 0x014F, `0150` = 0x0151, `0152` = 0x0153,
  `0154` = 0x0155, `0156` = 0x0157, `0158` = 0x0159, `015A` = 0x015B,
  `015C` = 0x015D, `015E` = 0x015F, `0160` = 0x0161, `0162` = 0x0163,
  `0164` = 0x0165, `0166` = 0x0167, `0168` = 0x0169, `016A` = 0x016B,
  `016C` = 0x016D, `016E` = 0x016F, `0170` = 0x0171, `0172` = 0x0173,
  `0174` = 0x0175, `0176` = 0x0177, `0178` = 0x00FF, `0179` = 0x017A,
  `017B` = 0x017C, `017D` = 0x017E,
  `1E9E` = 0x00DF
)

# Fuera del latin, el pliegue caia en `tolower()`, que depende del locale: bajo
# un locale UTF-8 bajaba el cirilico y el griego, y bajo `C` no, asi que
# `detectar_claves()` sobre {MOSKVA, moskva} en cirilico daba un veredicto
# distinto segun la sesion -medido sobre 584 pares mayuscula/minuscula: 3 fallan
# en UTF-8 y 303 bajo `C`-. Se agregan al mapa, por rangos, los alfabetos con caja
# mas usados -griego, cirilico, armenio y el latin de ancho completo-; lo que
# queda afuera se CONSERVA en cualquier locale, como promete el pliegue.
.MAPA_MINUSCULAS_OTROS_ALFABETOS <- local({
  pares <- function(desde, hasta, salto) {
    mayusculas <- seq.int(desde, hasta)
    stats::setNames(mayusculas + salto, sprintf("%04X", mayusculas))
  }
  alternos <- function(desde, hasta) {
    mayusculas <- seq.int(desde, hasta, by = 2L)
    stats::setNames(mayusculas + 1L, sprintf("%04X", mayusculas))
  }
  c(
    # Griego: las mayusculas basicas y las acentuadas.
    pares(0x0391, 0x03A1, 32L), pares(0x03A3, 0x03AB, 32L),
    c(`0386` = 0x03AC, `0388` = 0x03AD, `0389` = 0x03AE, `038A` = 0x03AF,
      `038C` = 0x03CC, `038E` = 0x03CD, `038F` = 0x03CE),
    # Cirilico basico y sus extensiones de a pares.
    pares(0x0410, 0x042F, 32L), pares(0x0400, 0x040F, 80L),
    alternos(0x0460, 0x0480), alternos(0x048A, 0x04BE),
    c(`04C0` = 0x04CF), alternos(0x04C1, 0x04CD), alternos(0x04D0, 0x052E),
    # Armenio.
    pares(0x0531, 0x0556, 48L),
    # Latin de ancho completo y la schwa.
    pares(0xFF21, 0xFF3A, 32L), c(`018F` = 0x0259)
  )
})

# Deja el vector en un estado que las operaciones Unicode puedan leer sin
# depender del locale.
# Los que ya son UTF-8 valido se MARCAN -conserva los acentos, que es lo que el
# pliegue necesita-; los que no, se reparan con `iconv(sub = "byte")`, que es
# determinista aunque quede feo. Un texto ya marcado se deja como esta.
.textos_para_plegar <- function(textos) {
  if (!length(textos)) return(textos)
  textos <- as.character(textos)
  sin_marca <- !is.na(textos) & Encoding(textos) %in% c("unknown", "bytes")
  if (!any(sin_marca)) return(textos)
  bytes_validos <- sin_marca & validUTF8(textos)
  if (any(bytes_validos)) {
    marcados <- textos[bytes_validos]
    Encoding(marcados) <- "UTF-8"
    textos[bytes_validos] <- marcados
  }
  irreparables <- sin_marca & !bytes_validos
  if (any(irreparables)) {
    textos[irreparables] <- iconv(textos[irreparables], to = "UTF-8", sub = "byte")
  }
  textos
}

.normalizacion_minusculas_vector <- local({
  mapa <- c(.MAPA_MINUSCULAS_ACENTUADAS, .MAPA_MINUSCULAS_OTROS_ALFABETOS)
  origen <- paste0(
    intToUtf8(strtoi(names(mapa), base = 16L), multiple = TRUE),
    collapse = ""
  )
  destino <- paste0(
    intToUtf8(unname(mapa), multiple = TRUE),
    collapse = ""
  )
  function(textos) {
    # `chartr()` interpreta el texto SEGUN EL LOCALE cuando la cadena no declara
    # su codificacion, y aborta con "invalid input multibyte string" si los bytes
    # no son validos ahi. Un CSV en espanol leido con `read.csv()` sale con
    # `Encoding()` en `unknown` -es el caso mas comun que existe- y bajo
    # `LC_CTYPE=C` esos bytes no son interpretables: `perfilar()` moria con un
    # error crudo de R. El comentario de `.clave_bytes()` en `R/utils.R` ya
    # nombraba este mismo defecto para el orden; faltaba aplicarlo aca.
    #
    # No alcanza con `.clave_bytes()`: repara, pero deja los acentos escapados
    # como bytes -`caf<c3><89>` y `caf<c3><a9>`-, y entonces `CAFE` con tilde y
    # `cafe` con tilde dejan de plegarse juntos, que es justo lo que esta
    # funcion existe para hacer. Hay que MARCAR lo que ya es UTF-8 valido, y
    # reparar solo lo que no lo es.
    textos <- .textos_para_plegar(textos)
    # El ASCII se baja con `chartr()`, no con `tolower()`: lo que no esta en el
    # mapa se conserva igual en cualquier locale, y la `I` no se vuelve U+0131
    # bajo un locale turco.
    chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz",
           chartr(origen, destino, textos))
  }
})

# Pliegue para COMPARAR un valor protegido con sus variantes, que no es el de la
# normalizacion: alla se conservan los acentos -`papa` y `papá` son palabras
# distintas-, y aca todo lo cosmetico se pliega porque el error caro es publicar
# el nombre de una persona. Una refutacion midio que la proteccion se escapaba
# fuera del latin occidental: el nombre vietnamita escrito sin sus marcas, el
# griego o el cirilico en mayusculas, el armenio, el ancho completo. La
# comparacion iba por bytes y el pliegue de caja era solo ASCII.
#
# Lo que hace, en este orden:
#
#   1. Todo pasa a UTF-8 valido. Lo marcado `latin1` se convierte; lo que no
#      declara codificacion y es UTF-8 valido se marca, y lo que no lo es se LEE
#      COMO LATIN1. Esto ultimo es la otra mitad del arreglo: un nombre
#      protegido en latin1 sin marca no se reconocia en la misma celda escrita en
#      UTF-8, porque la reparacion por bytes lo dejaba como `Jos<e9>`. Latin1
#      asigna un caracter a cada byte, asi que nunca falla, y es la lectura
#      correcta para lo que viene de Windows o de un CSV viejo.
#   2. El ancho completo pasa a ASCII.
#   3. Las letras con diacriticos fuera de Latin-1 y Latin Extended-A pasan a su
#      base con `.MAPA_DIACRITICOS`, y las de adentro con la transliteracion.
#   4. Se baja la caja con el mapa del paquete, que no depende del locale, y la
#      sigma final se iguala a la sigma.
.plegar_para_comparar <- local({
  origen_diacriticos <- paste0(
    intToUtf8(strtoi(names(.MAPA_DIACRITICOS), base = 16L), multiple = TRUE),
    collapse = ""
  )
  destino_diacriticos <- paste0(
    intToUtf8(unname(.MAPA_DIACRITICOS), multiple = TRUE),
    collapse = ""
  )
  origen_ancho <- paste0(
    intToUtf8(c(0xFF01:0xFF5E, 0x3000), multiple = TRUE),
    collapse = ""
  )
  destino_ancho <- paste0(
    intToUtf8(c(0x0021:0x007E, 0x0020), multiple = TRUE),
    collapse = ""
  )
  sigma_final <- intToUtf8(0x03C2)
  sigma <- intToUtf8(0x03C3)
  function(textos) {
    textos <- as.character(textos)
    if (!length(textos)) return(textos)
    marcas <- Encoding(textos)
    presentes <- !is.na(textos)
    en_latin1 <- presentes & marcas == "latin1"
    if (any(en_latin1)) {
      textos[en_latin1] <- iconv(textos[en_latin1], from = "latin1", to = "UTF-8")
    }
    sin_marca <- presentes & marcas %in% c("unknown", "bytes")
    if (any(sin_marca)) {
      validos <- sin_marca & validUTF8(textos)
      if (any(validos)) {
        marcados <- textos[validos]
        Encoding(marcados) <- "UTF-8"
        textos[validos] <- marcados
      }
      invalidos <- sin_marca & !validos
      if (any(invalidos)) {
        textos[invalidos] <- iconv(
          textos[invalidos], from = "latin1", to = "UTF-8"
        )
      }
    }
    textos <- chartr(origen_ancho, destino_ancho, textos)
    textos <- chartr(origen_diacriticos, destino_diacriticos, textos)
    textos <- .transliterar_ascii(textos)
    textos <- .normalizacion_minusculas_vector(textos)
    gsub(sigma_final, sigma, textos, fixed = TRUE)
  }
})
