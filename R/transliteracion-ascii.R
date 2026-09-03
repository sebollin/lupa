# Este mapa explicito es la garantia de la transliteracion; no se delega en
# `iconv`, porque `ASCII//TRANSLIT` cambia entre implementaciones y plataformas.
# Los nombres son puntos de codigo hexadecimales para que este archivo tambien
# sea ASCII. Incluye las letras de Latin-1 y Latin Extended-A.
.MAPA_TRANSLITERACION_ASCII <- c(
  `00AA` = "a", `00B5` = "u", `00BA` = "o",
  `00C0` = "A", `00C1` = "A", `00C2` = "A", `00C3` = "A",
  `00C4` = "A", `00C5` = "A", `00C6` = "AE", `00C7` = "C",
  `00C8` = "E", `00C9` = "E", `00CA` = "E", `00CB` = "E",
  `00CC` = "I", `00CD` = "I", `00CE` = "I", `00CF` = "I",
  `00D0` = "D", `00D1` = "N", `00D2` = "O", `00D3` = "O",
  `00D4` = "O", `00D5` = "O", `00D6` = "O", `00D8` = "O",
  `00D9` = "U", `00DA` = "U", `00DB` = "U", `00DC` = "U",
  `00DD` = "Y", `00DE` = "TH", `00DF` = "ss",
  `00E0` = "a", `00E1` = "a", `00E2` = "a", `00E3` = "a",
  `00E4` = "a", `00E5` = "a", `00E6` = "ae", `00E7` = "c",
  `00E8` = "e", `00E9` = "e", `00EA` = "e", `00EB` = "e",
  `00EC` = "i", `00ED` = "i", `00EE` = "i", `00EF` = "i",
  `00F0` = "d", `00F1` = "n", `00F2` = "o", `00F3` = "o",
  `00F4` = "o", `00F5` = "o", `00F6` = "o", `00F8` = "o",
  `00F9` = "u", `00FA` = "u", `00FB` = "u", `00FC` = "u",
  `00FD` = "y", `00FE` = "th", `00FF` = "y",
  `0100` = "A", `0101` = "a", `0102` = "A", `0103` = "a",
  `0104` = "A", `0105` = "a", `0106` = "C", `0107` = "c",
  `0108` = "C", `0109` = "c", `010A` = "C", `010B` = "c",
  `010C` = "C", `010D` = "c", `010E` = "D", `010F` = "d",
  `0110` = "D", `0111` = "d", `0112` = "E", `0113` = "e",
  `0114` = "E", `0115` = "e", `0116` = "E", `0117` = "e",
  `0118` = "E", `0119` = "e", `011A` = "E", `011B` = "e",
  `011C` = "G", `011D` = "g", `011E` = "G", `011F` = "g",
  `0120` = "G", `0121` = "g", `0122` = "G", `0123` = "g",
  `0124` = "H", `0125` = "h", `0126` = "H", `0127` = "h",
  `0128` = "I", `0129` = "i", `012A` = "I", `012B` = "i",
  `012C` = "I", `012D` = "i", `012E` = "I", `012F` = "i",
  `0130` = "I", `0131` = "i", `0132` = "IJ", `0133` = "ij",
  `0134` = "J", `0135` = "j", `0136` = "K", `0137` = "k",
  `0138` = "k", `0139` = "L", `013A` = "l", `013B` = "L",
  `013C` = "l", `013D` = "L", `013E` = "l", `013F` = "L",
  `0140` = "l", `0141` = "L", `0142` = "l", `0143` = "N",
  `0144` = "n", `0145` = "N", `0146` = "n", `0147` = "N",
  `0148` = "n", `0149` = "n", `014A` = "N", `014B` = "n",
  `014C` = "O", `014D` = "o", `014E` = "O", `014F` = "o",
  `0150` = "O", `0151` = "o", `0152` = "OE", `0153` = "oe",
  `0154` = "R", `0155` = "r", `0156` = "R", `0157` = "r",
  `0158` = "R", `0159` = "r", `015A` = "S", `015B` = "s",
  `015C` = "S", `015D` = "s", `015E` = "S", `015F` = "s",
  `0160` = "S", `0161` = "s", `0162` = "T", `0163` = "t",
  `0164` = "T", `0165` = "t", `0166` = "T", `0167` = "t",
  `0168` = "U", `0169` = "u", `016A` = "U", `016B` = "u",
  `016C` = "U", `016D` = "u", `016E` = "U", `016F` = "u",
  `0170` = "U", `0171` = "u", `0172` = "U", `0173` = "u",
  `0174` = "W", `0175` = "w", `0176` = "Y", `0177` = "y",
  `0178` = "Y", `0179` = "Z", `017A` = "z", `017B` = "Z",
  `017C` = "z", `017D` = "Z", `017E` = "z", `017F` = "s"
)

# Estos casos requieren sustitucion, porque su salida no es siempre un solo
# caracter ASCII (y quedan fuera del tramo que se pasa a `chartr`).
.CODIGOS_ESPECIALES_TRANSLITERACION_ASCII <- c(
  "00C6", "00D0", "00D8", "00DE", "00DF", "00E6", "00F0", "00F8",
  "00FE", "0110", "0111", "0132", "0133", "0138", "0149", "0152",
  "0153"
)

.transliterar_ascii <- function(x) {
  # Todo punto de codigo que no esta en el mapa se deja intacto; la limpieza
  # posterior de cada consumidor decide que hacer con lo que no sea alfanumerico.
  x <- as.character(x)
  especiales <- .CODIGOS_ESPECIALES_TRANSLITERACION_ASCII
  mapa_uno_a_uno <- .MAPA_TRANSLITERACION_ASCII[
    !names(.MAPA_TRANSLITERACION_ASCII) %in% especiales
  ]
  origen <- paste0(
    vapply(
      strtoi(names(mapa_uno_a_uno), base = 16L),
      intToUtf8,
      character(1L)
    ),
    collapse = ""
  )
  destino <- paste0(unname(mapa_uno_a_uno), collapse = "")
  salida <- chartr(origen, destino, x)
  for (codigo in especiales) {
    salida <- gsub(
      intToUtf8(strtoi(codigo, base = 16L)),
      .MAPA_TRANSLITERACION_ASCII[[codigo]],
      salida,
      fixed = TRUE
    )
  }
  salida
}
