# Motor de reparacion de texto en R puro.
# Las categorias y el orden de las tablas siguen ftfy 6.3.1 (Robyn Speer).
# Derivado del diseno de ftfy, bajo Apache-2.0:
# https://github.com/rspeer/python-ftfy (badness.py, chardata.py, fixes.py).
# Las tablas son literales congelados; no dependen de iconv en tiempo de carga.

.ftfy_tablas_bytes <- list(
  latin.1 = c(c(128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255)),
  sloppy.windows.1252 = c(c(8364, 129, 8218, 402, 8222, 8230, 8224, 8225, 710, 8240, 352, 8249, 338, 141, 381, 143, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 732, 8482, 353, 8250, 339, 157, 382, 376, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255)),
  sloppy.windows.1251 = c(c(1026, 1027, 8218, 1107, 8222, 8230, 8224, 8225, 8364, 8240, 1033, 8249, 1034, 1036, 1035, 1039, 1106, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 152, 8482, 1113, 8250, 1114, 1116, 1115, 1119, 160, 1038, 1118, 1032, 164, 1168, 166, 167, 1025, 169, 1028, 171, 172, 173, 174, 1031, 176, 177, 1030, 1110, 1169, 181, 182, 183, 1105, 8470, 1108, 187, 1112, 1029, 1109, 1111, 1040, 1041, 1042, 1043, 1044, 1045, 1046, 1047, 1048, 1049, 1050, 1051, 1052, 1053, 1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061, 1062, 1063, 1064, 1065, 1066, 1067, 1068, 1069, 1070, 1071, 1072, 1073, 1074, 1075, 1076, 1077, 1078, 1079, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 1103)),
  sloppy.windows.1250 = c(c(8364, 129, 8218, 131, 8222, 8230, 8224, 8225, 136, 8240, 352, 8249, 346, 356, 381, 377, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 152, 8482, 353, 8250, 347, 357, 382, 378, 160, 711, 728, 321, 164, 260, 166, 167, 168, 169, 350, 171, 172, 173, 174, 379, 176, 177, 731, 322, 180, 181, 182, 183, 184, 261, 351, 187, 317, 733, 318, 380, 340, 193, 194, 258, 196, 313, 262, 199, 268, 201, 280, 203, 282, 205, 206, 270, 272, 323, 327, 211, 212, 336, 214, 215, 344, 366, 218, 368, 220, 221, 354, 223, 341, 225, 226, 259, 228, 314, 263, 231, 269, 233, 281, 235, 283, 237, 238, 271, 273, 324, 328, 243, 244, 337, 246, 247, 345, 367, 250, 369, 252, 253, 355, 729)),
  sloppy.windows.1253 = c(c(8364, 129, 8218, 402, 8222, 8230, 8224, 8225, 136, 8240, 138, 8249, 140, 141, 142, 143, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 152, 8482, 154, 8250, 156, 157, 158, 159, 160, 901, 902, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 8213, 176, 177, 178, 179, 900, 181, 182, 183, 904, 905, 906, 187, 908, 189, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927, 928, 929, 210, 931, 932, 933, 934, 935, 936, 937, 938, 939, 940, 941, 942, 943, 944, 945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974, 255)),
  sloppy.windows.1254 = c(c(8364, 129, 8218, 402, 8222, 8230, 8224, 8225, 710, 8240, 352, 8249, 338, 141, 142, 143, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 732, 8482, 353, 8250, 339, 157, 158, 376, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 286, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 304, 350, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 287, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 305, 351, 255)),
  sloppy.windows.1257 = c(c(8364, 129, 8218, 131, 8222, 8230, 8224, 8225, 136, 8240, 138, 8249, 140, 168, 711, 184, 144, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 152, 8482, 154, 8250, 156, 175, 731, 159, 160, 161, 162, 163, 164, 165, 166, 167, 216, 169, 342, 171, 172, 173, 174, 198, 176, 177, 178, 179, 180, 181, 182, 183, 248, 185, 343, 187, 188, 189, 190, 230, 260, 302, 256, 262, 196, 197, 280, 274, 268, 201, 377, 278, 290, 310, 298, 315, 352, 323, 325, 211, 332, 213, 214, 215, 370, 321, 346, 362, 220, 379, 381, 223, 261, 303, 257, 263, 228, 229, 281, 275, 269, 233, 378, 279, 291, 311, 299, 316, 353, 324, 326, 243, 333, 245, 246, 247, 371, 322, 347, 363, 252, 380, 382, 729)),
  iso.8859.2 = c(c(128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 260, 728, 321, 164, 317, 346, 167, 168, 352, 350, 356, 377, 173, 381, 379, 176, 261, 731, 322, 180, 318, 347, 711, 184, 353, 351, 357, 378, 733, 382, 380, 340, 193, 194, 258, 196, 313, 262, 199, 268, 201, 280, 203, 282, 205, 206, 270, 272, 323, 327, 211, 212, 336, 214, 215, 344, 366, 218, 368, 220, 221, 354, 223, 341, 225, 226, 259, 228, 314, 263, 231, 269, 233, 281, 235, 283, 237, 238, 271, 273, 324, 328, 243, 244, 337, 246, 247, 345, 367, 250, 369, 252, 253, 355, 729)),
  macroman = c(c(196, 197, 199, 201, 209, 214, 220, 225, 224, 226, 228, 227, 229, 231, 233, 232, 234, 235, 237, 236, 238, 239, 241, 243, 242, 244, 246, 245, 250, 249, 251, 252, 8224, 176, 162, 163, 167, 8226, 182, 223, 174, 169, 8482, 180, 168, 8800, 198, 216, 8734, 177, 8804, 8805, 165, 181, 8706, 8721, 8719, 960, 8747, 170, 186, 937, 230, 248, 191, 161, 172, 8730, 402, 8776, 916, 171, 187, 8230, 160, 192, 195, 213, 338, 339, 8211, 8212, 8220, 8221, 8216, 8217, 247, 9674, 255, 376, 8260, 8364, 8249, 8250, 64257, 64258, 8225, 183, 8218, 8222, 8240, 194, 202, 193, 203, 200, 205, 206, 207, 204, 211, 212, 57374, 210, 218, 219, 217, 305, 710, 732, 175, 728, 729, 730, 184, 733, 731, 711)),
  cp437 = c(c(199, 252, 233, 226, 228, 224, 229, 231, 234, 235, 232, 239, 238, 236, 196, 197, 201, 230, 198, 244, 246, 242, 251, 249, 255, 214, 220, 162, 163, 165, 8359, 402, 225, 237, 243, 250, 241, 209, 170, 186, 191, 8976, 172, 189, 188, 161, 171, 187, 9617, 9618, 9619, 9474, 9508, 9569, 9570, 9558, 9557, 9571, 9553, 9559, 9565, 9564, 9563, 9488, 9492, 9524, 9516, 9500, 9472, 9532, 9566, 9567, 9562, 9556, 9577, 9574, 9568, 9552, 9580, 9575, 9576, 9572, 9573, 9561, 9560, 9554, 9555, 9579, 9578, 9496, 9484, 9608, 9604, 9612, 9616, 9600, 945, 223, 915, 960, 931, 963, 181, 964, 934, 920, 937, 948, 8734, 966, 949, 8745, 8801, 177, 8805, 8804, 8992, 8993, 247, 8776, 176, 8729, 183, 8730, 8319, 178, 9632, 160))
)
.ftfy_tablas_bytes <- lapply(.ftfy_tablas_bytes, as.integer)
names(.ftfy_tablas_bytes) <- c(
  "latin-1", "sloppy-windows-1252", "sloppy-windows-1251",
  "sloppy-windows-1250", "sloppy-windows-1253", "sloppy-windows-1254",
  "sloppy-windows-1257", "iso-8859-2", "macroman", "cp437"
)

.ftfy_categorias <- list(
  common = "\u00a0\u00ad\u00b7\u00b4\u2013\u2014\u2015\u2026\u2019",
  c1 = "\u0080-\u009f",
  bad = paste0("\u00a6\u00a4\u00a8\u00ac\u00af\u00b8\u0192\u02c6\u02c7", "\u02d8\u02db\u02dc\u2020\u2021\u2030\u2310\u25ca\ufffd", "\u00aa\u00ba"),
  law = "\u00b6\u00a7",
  currency = "\u00a2\u00a3\u00a5\u20a7\u20ac",
  start_punctuation = "\u00a1\u00ab\u00bf\u00a9\u0384\u0385\u2018\u201a\u201c\u201e\u2022\u2039\uf8ff",
  end_punctuation = "\u00ae\u00bb\u02dd\u201d\u203a\u2122",
  numeric = "\u00b2\u00b3\u00b9\u00b1\u00bc\u00bd\u00be\u00d7\u00b5\u00f7\u2044\u2202\u2206\u220f\u2211\u221a\u221e\u2229\u222b\u2248\u2260\u2261\u2264\u2265\u2116",
  kaomoji = "\u00d2-\u00d6\u00d9-\u00dc\u00f2-\u00f6\u00f8-\u00fc\u0150\u00b0",
  upper_accented = "\u00c0-\u00d1\u00d8\u00dc\u00dd\u0102\u0104\u0106\u010c\u010e\u0110\u0118\u011a\u011e\u0130\u0139\u013d\u0141\u0143\u0147\u0152\u0158\u015a\u015e\u0160\u0162\u0164\u016e\u0170\u0178\u0179\u017b\u017d\u0403",
  lower_accented = "\u00df\u00e0-\u00f1\u0103\u0105\u0107\u010d\u010f\u0111\u0119\u011b\u011f\u013a\u013e\u0142\u0144\u0148\u0153\u0155\u015b\u015f\u0161\u0163\u0165\u016f\u0171\u017a\u017c\u017e\u0453\ufb01\ufb02",
  upper_common = "\u00de\u0391-\u03a9\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u03aa\u03ab\u0401-\u042f",
  lower_common = "\u03b1-\u03c9\u03ac\u03ad\u03ae\u03af\u03cc\u03cd\u03ce\u0390\u03b0\u0430-\u044f",
  box = "\u2502\u250c\u2510\u2518\u251c\u2524\u252c\u253c\u2580\u2584\u2588\u258c\u2590\u2591\u2592\u2593\u2554\u2557\u255a\u255d\u2560\u2563\u2566\u2569\u256c"
)
.ftfy_re_cat <- function(...) {
  claves <- list(...)
  paste0("[", paste0(unlist(.ftfy_categorias[unlist(claves)]), collapse = ""), "]")
}
.ftfy_badness_re <- paste(
  .ftfy_re_cat("c1"),
  paste0(.ftfy_re_cat("bad", "lower_accented", "upper_accented",
                       "start_punctuation", "end_punctuation", "currency",
                       "numeric", "law"), .ftfy_re_cat("bad")),
  paste0(.ftfy_re_cat("bad"), .ftfy_re_cat("lower_accented", "upper_accented",
                                              "start_punctuation", "end_punctuation",
                                              "currency", "numeric", "law")),
  paste0(.ftfy_re_cat("lower_accented", "end_punctuation", "currency", "numeric"),
         .ftfy_re_cat("upper_accented")),
  paste0(.ftfy_re_cat("end_punctuation", "currency", "numeric"),
         .ftfy_re_cat("lower_accented")),
  paste0(.ftfy_re_cat("lower_accented", "end_punctuation"),
         .ftfy_re_cat("currency")),
  paste0(.ftfy_re_cat("upper_accented"), .ftfy_re_cat("numeric", "law")),
  paste0(.ftfy_re_cat("currency", "numeric"), .ftfy_re_cat("start_punctuation")),
  paste0("[a-z]", .ftfy_re_cat("upper_accented"),
         .ftfy_re_cat("start_punctuation", "currency")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented"),
         .ftfy_re_cat("start_punctuation", "end_punctuation"), "\\w"),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented", "currency", "numeric", "law"),
         .ftfy_re_cat("end_punctuation"), .ftfy_re_cat("start_punctuation")),
  "[\u00c2\u00c3\u00ce\u00d0][\u20ac\u0153\u0160\u0161\u00a2\u00a3\u0178\u017e\u00a0\u00ad\u00ae\u00a9\u00b0\u00b7\u00bb\u2013\u2014\u00b4\u00a1\u00ab\u00bf\u2018\u201a\u201c\u201e\u2022\u2039\u203a\u2122\u201d]",
  paste0("[a-z]\\s?[\u00c3\u00c2][ ]"),
  paste0("^[\u00c3\u00c2][ ]"),
  paste0(.ftfy_re_cat("upper_accented"), "\u00b0"),
  sep = "|"
)
.MOJIBAKE_CATEGORIES <- .ftfy_categorias
.BADNESS_RE <- .ftfy_badness_re
.ftfy_badness_re <- paste(
  .ftfy_badness_re,
  paste0("[a-zA-Z]", .ftfy_re_cat("lower_common", "upper_common"),
         .ftfy_re_cat("bad")),
  paste0(.ftfy_re_cat("bad"), .ftfy_re_cat("lower_accented", "upper_accented",
                                             "box", "start_punctuation", "end_punctuation",
                                             "currency", "numeric")),
  paste0(.ftfy_re_cat("box"), .ftfy_re_cat("kaomoji")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented", "currency", "numeric",
                      "start_punctuation", "end_punctuation"), .ftfy_re_cat("box")),
  paste0(.ftfy_re_cat("box"), .ftfy_re_cat("end_punctuation")),
  "[\u0152\u0153][^A-Za-z]",
  "\u00d7[\u00b2\u00b3]",
  "[\u00d8\u00d9][\u00a0\u00ad\u00b7\u00b4\u2013\u2014\u2015\u2026\u2019\u00a2\u00a3\u00a5\u20a7\u20ac\u00a6\u00a4\u00a8\u00ac\u00af\u00b8\u0192\u02c6\u02c7\u02d8\u02db\u02dc\u2020\u2021\u2030\u2310\u25ca\ufffd\u00aa\u00ba\u00b2\u00b3\u00b9\u00b1\u00bc\u00bd\u00be\u00a1\u00ab\u00bf\u00a9\u0384\u0385\u2018\u201a\u201c\u201e\u2022\u2039\u0178\u0160\u00ae\u00b0\u00b5\u00bb]",
  "\u00e0[\u00b2\u00b5\u00b9\u00bc\u00bd\u00be]",
  "\u221a[\u00b1\u2202\u2020\u2260\u00ae\u2122\u00b4\u2264\u2265\u00a5\u00b5\u00f8]|\u2248[\u00b0\u00a2]|\u201a\u00c4[\u00ec\u00ee\u00ef\u00f2\u00f4\u00fa\u00f9\u00fb\u2020\u00b0\u00a2\u03c0]|\u201a[\u00e2\u00f3][\u00e0\u00e4\u00b0\u00ea]",
  "\u0432\u0402|[\u0412\u0413\u0420\u0421][\u0080-\u009f\u00a6\u00a4\u00a8\u00ac\u00af\u00b8\u0192\u02c6\u02c7\u02d8\u02db\u02dc\u2020\u2021\u2030\u2310\u25ca\ufffd\u00aa\u00ba\u00a1\u00ab\u00bf\u00a9\u0384\u0385\u2018\u201a\u201c\u201e\u2022\u2039\u00ae\u00bb\u02dd\u201d\u203a\u2122\u00a2\u00a3\u00a5\u20a7\u20ac\u00b0\u00b5][\u0412\u0413\u0420\u0421]",
  "\u0413\u045e\u0412\u0402\u0412.[A-Za-z ]|\u00c3[\u00a0\u00a1]|[a-z.,?!\u00ae\u00bb\u02dd\u201d\u203a\u2122] \u00c2 [ \u00a1\u00ab\u00bf\u00a9\u0384\u0385\u2018\u201a\u201c\u201e\u2022\u2039\u00ae\u00bb\u02dd\u201d\u203a\u2122]",
  "\u03b2\u20ac[\u2122\u00a0\u0386\u00ad\u00ae\u00b0]|[\u0392\u0393\u039e\u039f][\u0080-\u009f\u00a6\u00a4\u00a8\u00ac\u00af\u00b8\u0192\u02c6\u02c7\u02d8\u02db\u02dc\u2020\u2021\u2030\u2310\u25ca\ufffd\u00aa\u00ba\u00a1\u00ab\u00bf\u00a9\u0384\u0385\u2018\u201a\u201c\u201e\u2022\u2039\u00ae\u00bb\u02dd\u201d\u203a\u2122\u00a2\u00a3\u00a5\u20a7\u20ac\u00b0][\u0392\u0393\u039e\u039f]",
  sep = "|"
)
.ftfy_es_mojibake <- function(x) {
  if (!length(x) || is.na(x) || !nzchar(x)) return(FALSE)
  grepl(.ftfy_badness_re, x, perl = TRUE)
}
.ftfy_codificar <- function(texto, tabla) {
  cps <- utf8ToInt(texto)
  if (anyNA(cps)) return(NULL)
  bytes <- integer(length(cps))
  ascii <- cps < 128L
  bytes[ascii] <- cps[ascii]
  if (any(!ascii)) {
    idx <- match(cps[!ascii], tabla)
    if (anyNA(idx)) return(NULL)
    bytes[!ascii] <- 127L + idx
  }
  as.raw(bytes)
}
.ftfy_desde_utf8 <- function(bytes) {
  if (is.null(bytes) || any(bytes == as.raw(0L))) return(NULL)
  texto <- rawToChar(bytes)
  Encoding(texto) <- "UTF-8"
  if (validUTF8(texto)) texto else NULL
}
.ftfy_restaurar_a0 <- function(bytes) {
  if (is.null(bytes) || !length(bytes)) return(NULL)
  hex <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  pares <- substring(hex, seq(1L, nchar(hex), 2L), seq(2L, nchar(hex), 2L))
  cambiado <- FALSE
  i <- 1L
  while (i <= length(pares)) {
    if (i + 1L <= length(pares) && pares[i + 1L] == "20" &&
        pares[i] %in% c("c2", "c3", "c5", "ce", "d0", "d9")) {
      pares[i + 1L] <- "a0"; cambiado <- TRUE; i <- i + 2L; next
    }
    if (i + 2L <= length(pares) && pares[i] %in% c("e2", "e3") &&
        pares[i + 1L] == "20") {
      pares[i + 1L] <- "a0"; cambiado <- TRUE; i <- i + 3L; next
    }
    if (i + 2L <= length(pares) && pares[i] %in% c("e0", "e1", "e2", "e3") &&
        pares[i + 2L] == "20") {
      pares[i + 2L] <- "a0"; cambiado <- TRUE; i <- i + 3L; next
    }
    i <- i + 1L
  }
  if (!cambiado) return(NULL)
  as.raw(strtoi(pares, 16L))
}
.ftfy_fix_c1_controls <- function(texto) {
  cps <- utf8ToInt(texto)
  if (!length(cps) || !any(cps >= 128L & cps <= 159L)) return(texto)
  tabla <- .ftfy_tablas_bytes[["sloppy-windows-1252"]]
  indices <- cps >= 128L & cps <= 159L
  cps[indices] <- tabla[cps[indices] - 127L]
  intToUtf8(cps)
}
.ftfy_normalizar_nbsp <- function(texto) sub("\u00a0", " ", texto, fixed = TRUE)
.ftfy_ajustar_espacio_a0 <- function(origen, texto) {
  # En una exportacion que convirtio 0xA0 en un unico espacio se pierde la
  # separacion siguiente a una vocal. ftfy conserva esa frontera textual.
  if (grepl("\u00c3 ", origen, fixed = TRUE) &&
      !grepl("\u00c3  ", origen, fixed = TRUE)) {
    sub("\u00e0([^[:space:]])", "\u00e0 \\1", texto, perl = TRUE)
  } else texto
}
.ftfy_decode_inconsistent_utf8 <- function(texto) {
  # La misma transcodificacion por bytes funciona cuando solo un segmento del
  # valor fue degradado; los segmentos ASCII se conservan byte a byte.
  .ftfy_un_paso(texto)
}
.ftfy_replace_lossy_sequences <- function(texto) {
  # U+FFFD ya declara perdida de informacion: no se reemplaza ni se adivina.
  list(texto = texto, estado = if (grepl("\uFFFD", texto, fixed = TRUE)) {
    "no_se_pudo"
  } else "sin_perdida")
}
.ftfy_un_paso <- function(texto) {
  for (nombre in names(.ftfy_tablas_bytes)) {
    bytes <- .ftfy_codificar(texto, .ftfy_tablas_bytes[[nombre]])
    if (is.null(bytes)) next
    salida <- .ftfy_desde_utf8(bytes)
    if (!is.null(salida)) {
      if (nombre != "macroman") salida <- .ftfy_normalizar_nbsp(salida)
      return(list(texto = salida, paso = paste0("encode:", nombre, ";decode:utf-8")))
    }
    if (nombre != "macroman") {
      reparados <- .ftfy_restaurar_a0(bytes)
      salida <- .ftfy_desde_utf8(reparados)
      if (!is.null(salida)) {
        salida <- .ftfy_normalizar_nbsp(salida)
        salida <- .ftfy_ajustar_espacio_a0(texto, salida)
        return(list(texto = salida, paso = paste0("encode:", nombre, ";restore_byte_a0;decode:utf-8")))
      }
    }
  }
  NULL
}
.ftfy_reparar_uno <- function(x, max_iteraciones = 20L) {
  if (length(x) != 1L) {
    return(list(texto = NA_character_, pasos = character(), estado = "sin_texto"))
  }
  valor <- as.character(x)
  if (is.na(valor) || !nzchar(valor)) {
    return(list(texto = valor, pasos = character(), estado = "sin_texto"))
  }
  actual <- enc2utf8(valor)
  antes_c1 <- actual
  actual <- .ftfy_fix_c1_controls(actual)
  pasos <- if (!identical(actual, antes_c1)) "fix_c1_controls" else character()
  if (!.ftfy_es_mojibake(actual)) {
    return(list(texto = actual, pasos = pasos, estado = if (length(pasos)) "reparado" else "no_parece_roto"))
  }
  tope <- max(1L, as.integer(max_iteraciones[[1L]]))
  for (i in seq_len(min(tope, 20L))) {
    paso <- .ftfy_un_paso(actual)
    if (is.null(paso) || identical(paso$texto, actual)) break
    actual <- paso$texto
    pasos <- c(pasos, paso$paso)
    if (!.ftfy_es_mojibake(actual)) break
  }
  estado <- if (!length(pasos)) "no_se_pudo" else if (.ftfy_es_mojibake(actual)) "reparado_parcialmente" else "reparado"
  list(texto = actual, pasos = pasos, estado = estado)
}
# Compatibilidad con planes antiguos: el nombre historico sigue aceptado.
.reparar_mojibake_uno <- function(x, max_iteraciones = 20L) {
  r <- .ftfy_reparar_uno(x, max_iteraciones)
  if (identical(r$estado, "reparado")) r$texto else NA_character_
}
.ftfy_estado_agregado <- function(estados) {
  estados <- estados[!is.na(estados)]
  if (!length(estados) || all(estados == "no_parece_roto")) return("no_parece_roto")
  if (any(estados == "reparado_parcialmente")) return("reparado_parcialmente")
  if (any(estados == "no_se_pudo") && any(estados == "reparado")) {
    return("reparado_parcialmente")
  }
  if (any(estados == "reparado")) return("reparado")
  "no_se_pudo"
}
.analizar_codificacion <- function(textos) {
  valores <- as.character(textos)
  reparados <- rep(NA_character_, length(valores))
  estados <- rep(NA_character_, length(valores))
  pasos <- vector("list", length(valores))
  candidatos <- which(!is.na(valores) & (vapply(valores, .ftfy_es_mojibake, logical(1L)) |
    grepl("\uFFFD", valores, fixed = TRUE)))
  if (length(candidatos)) {
    unicos <- unique(valores[candidatos])
    resultados <- lapply(unicos, .ftfy_reparar_uno)
    reemplazo <- grepl("\uFFFD", unicos, fixed = TRUE)
    if (any(reemplazo)) {
      resultados[reemplazo] <- lapply(resultados[reemplazo], function(z) {
        z$estado <- "no_se_pudo"
        z
      })
    }
    mapa <- match(valores[candidatos], unicos)
    reparados[candidatos] <- vapply(resultados[mapa], function(z) z[["texto"]], character(1L))
    estados[candidatos] <- vapply(resultados[mapa], function(z) z[["estado"]], character(1L))
    pasos[candidatos] <- lapply(resultados[mapa], function(z) z[["pasos"]])
  }
  reparables <- !is.na(reparados) & !is.na(valores) & reparados != valores & estados == "reparado"
  parciales <- !is.na(reparados) & !is.na(valores) & reparados != valores & estados == "reparado_parcialmente"
  irreparables <- !is.na(valores) & grepl("\uFFFD", valores, fixed = TRUE)
  afectados <- reparables | parciales | irreparables
  ejemplos <- utils::head(which(afectados), 5L)
  evidencia <- if (!length(ejemplos)) "" else paste(vapply(ejemplos, function(i) {
    origen <- encodeString(valores[[i]], quote = '"')
    if (reparables[[i]] || parciales[[i]]) paste0(origen, " -> ", encodeString(reparados[[i]], quote = '"'), " [", estados[[i]], "]")
    else paste0(origen, " (contiene un caracter de reemplazo irrecuperable)")
  }, character(1L)), collapse = "; ")
  list(n = sum(afectados), n_reparables = sum(reparables),
       n_reparables_parcialmente = sum(parciales), n_irreparables = sum(irreparables),
       n_no_se_pudo = sum(estados == "no_se_pudo", na.rm = TRUE), evidencia = evidencia,
       reparados = reparados, estados = estados, pasos = pasos,
       estado = .ftfy_estado_agregado(estados))
}
