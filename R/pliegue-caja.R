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

.normalizacion_minusculas_vector <- local({
  origen <- paste0(
    intToUtf8(strtoi(names(.MAPA_MINUSCULAS_ACENTUADAS), base = 16L),
              multiple = TRUE),
    collapse = ""
  )
  destino <- paste0(
    intToUtf8(unname(.MAPA_MINUSCULAS_ACENTUADAS), multiple = TRUE),
    collapse = ""
  )
  function(textos) {
    # El ASCII I se fija antes de delegar lo que no esta en el mapa: evita que
    # un locale turco convierta una comparacion reproducible en U+0131.
    tolower(chartr("I", "i", chartr(origen, destino, textos)))
  }
})
