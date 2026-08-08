# Motor de reparacion de texto en R puro.
# Las categorias y las primeras diez tablas siguen [ftfy 6.3.1]
# (https://github.com/rspeer/python-ftfy), de [Robyn Speer]
# (https://github.com/rspeer). Derivado del diseno de ftfy, bajo Apache-2.0:
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
  macroman = c(c(196, 197, 199, 201, 209, 214, 220, 225, 224, 226, 228, 227, 229, 231, 233, 232, 234, 235, 237, 236, 238, 239, 241, 243, 242, 244, 246, 245, 250, 249, 251, 252, 8224, 176, 162, 163, 167, 8226, 182, 223, 174, 169, 8482, 180, 168, 8800, 198, 216, 8734, 177, 8804, 8805, 165, 181, 8706, 8721, 8719, 960, 8747, 170, 186, 937, 230, 248, 191, 161, 172, 8730, 402, 8776, 8710, 171, 187, 8230, 160, 192, 195, 213, 338, 339, 8211, 8212, 8220, 8221, 8216, 8217, 247, 9674, 255, 376, 8260, 8364, 8249, 8250, 64257, 64258, 8225, 183, 8218, 8222, 8240, 194, 202, 193, 203, 200, 205, 206, 207, 204, 211, 212, 63743, 210, 218, 219, 217, 305, 710, 732, 175, 728, 729, 730, 184, 733, 731, 711)),
  cp437 = c(c(199, 252, 233, 226, 228, 224, 229, 231, 234, 235, 232, 239, 238, 236, 196, 197, 201, 230, 198, 244, 246, 242, 251, 249, 255, 214, 220, 162, 163, 165, 8359, 402, 225, 237, 243, 250, 241, 209, 170, 186, 191, 8976, 172, 189, 188, 161, 171, 187, 9617, 9618, 9619, 9474, 9508, 9569, 9570, 9558, 9557, 9571, 9553, 9559, 9565, 9564, 9563, 9488, 9492, 9524, 9516, 9500, 9472, 9532, 9566, 9567, 9562, 9556, 9577, 9574, 9568, 9552, 9580, 9575, 9576, 9572, 9573, 9561, 9560, 9554, 9555, 9579, 9578, 9496, 9484, 9608, 9604, 9612, 9616, 9600, 945, 223, 915, 960, 931, 963, 181, 964, 934, 920, 937, 948, 8734, 966, 949, 8745, 8801, 177, 8805, 8804, 8992, 8993, 247, 8776, 176, 8729, 183, 8730, 8319, 178, 9632, 160))
)
.ftfy_tablas_bytes <- lapply(.ftfy_tablas_bytes, as.integer)
# La tabla koi8-r es una extension de lupa para el issue #231 de ftfy.
.ftfy_tablas_bytes$koi8.r <- as.integer(c(
  9472, 9474, 9484, 9488, 9492, 9496, 9500, 9508, 9516, 9524, 9532,
  9600, 9604, 9608, 9612, 9616, 9617, 9618, 9619, 8992, 9632, 8729,
  8730, 8776, 8804, 8805, 160, 8993, 176, 178, 183, 247, 9552, 9553,
  9554, 1105, 9555, 9556, 9557, 9558, 9559, 9560, 9561, 9562, 9563,
  9564, 9565, 9566, 9567, 9568, 9569, 1025, 9570, 9571, 9572, 9573,
  9574, 9575, 9576, 9577, 9578, 9579, 9580, 169, 1102, 1072, 1073,
  1094, 1076, 1077, 1092, 1075, 1093, 1080, 1081, 1082, 1083, 1084,
  1085, 1086, 1087, 1103, 1088, 1089, 1090, 1091, 1078, 1074, 1100,
  1099, 1079, 1096, 1101, 1097, 1095, 1098, 1070, 1040, 1041, 1062,
  1044, 1045, 1060, 1043, 1061, 1048, 1049, 1050, 1051, 1052, 1053,
  1054, 1055, 1071, 1056, 1057, 1058, 1059, 1046, 1042, 1068, 1067,
  1047, 1064, 1069, 1065, 1063, 1066
))
names(.ftfy_tablas_bytes) <- c(
  "latin-1", "sloppy-windows-1252", "sloppy-windows-1251",
  "sloppy-windows-1250", "sloppy-windows-1253", "sloppy-windows-1254",
  "sloppy-windows-1257", "iso-8859-2", "macroman", "cp437", "koi8-r"
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
  kaomoji = "\u00d2-\u00d6\u00d9-\u00dc\u00f2-\u00f6\u00f8-\u00fc\u0150\u014c\u016a\u0172\u00b0",
  upper_accented = "\u00c0-\u00d1\u00d8\u00dc\u00dd\u0102\u0100\u0104\u0106\u010c\u010e\u0110\u0118\u011a\u0112\u0116\u011e\u0122\u0130\u012a\u0136\u0139\u013d\u0141\u013b\u0143\u0147\u0145\u0152\u0158\u015a\u015e\u0160\u0162\u0164\u016e\u0170\u0178\u0179\u017b\u017d\u0490",
  lower_accented = "\u00df\u00e0-\u00f1\u0103\u0105\u0101\u0107\u010d\u010f\u0111\u0119\u011b\u0113\u0117\u011f\u0123\u012f\u012b\u0137\u013a\u013e\u0142\u013c\u0153\u0155\u015b\u015f\u0161\u0165\u00fc\u017a\u017c\u017e\u0491\ufb01\ufb02",
  upper_common = "\u00de\u0391-\u03a9\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u03aa\u03ab\u0401-\u042f",
  lower_common = "\u03b1-\u03c9\u03ac\u03ad\u03ae\u03af\u03b0\u0430-\u045f",
  box = "\u2502\u250c\u2510\u2518\u251c\u2524\u252c\u253c\u2550-\u256c\u2580\u2584\u2588\u258c\u2590\u2591\u2592\u2593"
)
.ftfy_re_cat <- function(...) {
  claves <- list(...)
  paste0("[", paste0(unlist(.ftfy_categorias[unlist(claves)]), collapse = ""), "]")
}
.ftfy_badness_alternatives <- c(
  .ftfy_re_cat("c1"),
  paste0(.ftfy_re_cat("bad", "lower_accented", "upper_accented", "box",
                       "start_punctuation", "end_punctuation", "currency",
                       "numeric", "law"), .ftfy_re_cat("bad")),
  paste0("[a-zA-Z]", .ftfy_re_cat("lower_common", "upper_common"),
         .ftfy_re_cat("bad")),
  paste0(.ftfy_re_cat("bad"), .ftfy_re_cat("lower_accented", "upper_accented",
                                                   "box", "start_punctuation",
                                                   "end_punctuation", "currency",
                                                   "numeric", "law")),
  paste0(.ftfy_re_cat("lower_accented", "lower_common", "box", "end_punctuation",
                      "currency", "numeric"), .ftfy_re_cat("upper_accented")),
  paste0(.ftfy_re_cat("box", "end_punctuation", "currency", "numeric"),
         .ftfy_re_cat("lower_accented")),
  paste0(.ftfy_re_cat("lower_accented", "box", "end_punctuation"),
         .ftfy_re_cat("currency")),
  paste0("\\s", .ftfy_re_cat("upper_accented"), .ftfy_re_cat("currency")),
  paste0(.ftfy_re_cat("upper_accented", "box"), .ftfy_re_cat("numeric", "law")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented", "box", "currency",
                      "end_punctuation"), .ftfy_re_cat("start_punctuation"),
         .ftfy_re_cat("numeric")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented", "currency", "numeric",
                      "box", "law"), .ftfy_re_cat("end_punctuation"),
         .ftfy_re_cat("start_punctuation")),
  paste0(.ftfy_re_cat("currency", "numeric", "box"), .ftfy_re_cat("start_punctuation")),
  paste0("[a-z]", .ftfy_re_cat("upper_accented"),
         .ftfy_re_cat("start_punctuation", "currency")),
  paste0(.ftfy_re_cat("box"), .ftfy_re_cat("kaomoji")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented", "currency", "numeric",
                      "start_punctuation", "end_punctuation", "law"),
         .ftfy_re_cat("box")),
  paste0(.ftfy_re_cat("box"), .ftfy_re_cat("end_punctuation")),
  paste0(.ftfy_re_cat("lower_accented", "upper_accented"),
         .ftfy_re_cat("start_punctuation", "end_punctuation"), "\\w"),
  "[\u0152\u0153][^A-Za-z]",
  paste0(.ftfy_re_cat("upper_accented"), "\u00b0"),
  paste0("[\u00c2\u00c3\u00ce\u00d0][\u20ac\u0153\u0160\u0161\u00a2\u00a3\u0178\u017e\\xa0\\xad\u00ae\u00a9\u00b0\u00b7\u00bb",
         .ftfy_categorias$start_punctuation, .ftfy_categorias$end_punctuation,
         "\u2013\u2014\u00b4]"),
  "\u00d7[\u00b2\u00b3]"
)
.ftfy_badness_alternatives <- c(
  .ftfy_badness_alternatives,
  paste0("[\u00d8\u00d9][", .ftfy_categorias$common, .ftfy_categorias$currency,
         .ftfy_categorias$bad, .ftfy_categorias$numeric,
         .ftfy_categorias$start_punctuation, "\u0178\u0160\u00ae\u00b0\u00b5\u00bb][\u00d8\u00d9][",
         .ftfy_categorias$common, .ftfy_categorias$currency,
         .ftfy_categorias$bad, .ftfy_categorias$numeric,
         .ftfy_categorias$start_punctuation, "\u0178\u0160\u00ae\u00b0\u00b5\u00bb]"),
  "\u00e0[\u00b2\u00b5\u00b9\u00bc\u00bd\u00be]",
  "\u221a[\u00b1\u2202\u2020\u2260\u00ae\u2122\u00b4\u2264\u2265\u00a5\u00b5\u00f8]",
  "\u2248[\u00b0\u00a2]",
  "\u201a\u00c4[\u00ec\u00ee\u00ef\u00f2\u00f4\u00fa\u00f9\u00fb\u2020\u00b0\u00a2\u03c0]",
  "\u201a[\u00e2\u00f3][\u00e0\u00e4\u00b0\u00ea]",
  "\u0432\u0402",
  paste0("[\u0412\u0413\u0420\u0421][", .ftfy_categorias$c1, .ftfy_categorias$bad,
         .ftfy_categorias$start_punctuation, .ftfy_categorias$end_punctuation,
         .ftfy_categorias$currency, "\u00b0\u00b5][\u0412\u0413\u0420\u0421]"),
  "\u0413\u045e\u0412\u0402\u0412.[A-Za-z ]",
  "\u00c3[\\xa0\u00a1]",
  "[a-z]\\s?[\u00c3\u00c2][ ]",
  "^[\u00c3\u00c2][ ]",
  paste0("[a-z.,?!", .ftfy_categorias$end_punctuation, "]\u00c2[",
         " ", .ftfy_categorias$start_punctuation, .ftfy_categorias$end_punctuation, "]"),
  "\u03b2\u20ac[\u2122\\xa0\u0386\\xad\u00ae\u00b0]",
  paste0("[\u0392\u0393\u039e\u039f][", .ftfy_categorias$c1, .ftfy_categorias$bad,
         .ftfy_categorias$start_punctuation, .ftfy_categorias$end_punctuation,
         .ftfy_categorias$currency, "\u00b0][\u0392\u0393\u039e\u039f]"),
  "\u0101\u20ac"
)
.ftfy_badness_re_ftfy <- paste0(
  "(*UTF)(*UCP)", paste(.ftfy_badness_alternatives, collapse = "|")
)
# Correcciones deliberadas sobre ftfy 6.3.1: la regla de inicio funciona
# tambien al principio de la cadena, la regla de caja reconoce cirilico comun
# para KOI8-R y la secuencia especifica de a-circunflejo evita una regla amplia.
.ftfy_badness_extensiones <- c(
  paste0("(?:^|\\s)", .ftfy_re_cat("upper_accented"),
         .ftfy_re_cat("currency"), "\\w"),
  paste0(.ftfy_re_cat("lower_common", "upper_common", "lower_accented",
                      "upper_accented", "currency", "numeric"),
         .ftfy_re_cat("box")),
  paste0("\u00e2", .ftfy_re_cat("common"),
         .ftfy_re_cat("start_punctuation", "end_punctuation", "currency",
                      "numeric", "common"))
)
.ftfy_badness_re <- paste0(
  "(*UTF)(*UCP)(?:", substring(
    .ftfy_badness_re_ftfy, nchar("(*UTF)(*UCP)") + 1L
  ), "|",
  paste(.ftfy_badness_extensiones, collapse = "|"), ")"
)
.ftfy_utf8_detector_re <-
  "(?<![\u0080-\u00bf\u0104\u00c6\u013d\u0141\u00d8\u0156\u015a\u0160\u015e\u0164\u0178\u0179\u017d\u017b\u0152\u0105\u00e6\u0192\u013e\u0142\u00f8\u0157\u015b\u0161\u015f\u0165\u017a\u017e\u017c\u0153\u02c6\u02c7\u02d8\u02db\u02dc\u02dd\u0384\u0385\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u0401\u0402\u0403\u0404\u0405\u0406\u0407\u0408\u0409\u040a\u040b\u040c\u040e\u040f\u0451\u0452\u0453\u0454\u0455\u0456\u0457\u0458\u0459\u045a\u045b\u045c\u045e\u045f\u0490\u0491\u2020\u2021\u2030\u2039\u203a\u20ac\u2116\u2122])([\u0102\u00c2\u00c4\u0100\u00c5\u00c3\u00c6\u0106\u010c\u00c7\u010e\u0110\u00c9\u011a\u00ca\u00cb\u0116\u00c8\u0112\u0118\u00d0\u011e\u0122\u00cd\u00ce\u00cf\u0130\u00cc\u012a\u0136\u0139\u013b\u0141\u0143\u0147\u0145\u00d1\u00d3\u00d4\u00d6\u0150\u00d2\u014c\u00d8\u00d5\u0158\u015a\u0160\u015e\u0162\u00de\u00da\u00db\u00dc\u0170\u00d9\u016a\u0172\u016e\u00dd\u0179\u017d\u017b\u00df\u00d7\u0392\u0393\u0394\u0395\u0396\u0397\u0398\u0399\u039a\u039b\u039c\u039d\u039e\u039f\u03a0\u03a1\u03a3\u03a4\u03a5\u03a6\u03a7\u03a8\u03a9\u03aa\u03ab\u03ac\u03ad\u03ae\u03af\u0412\u0413\u0414\u0415\u0416\u0417\u0418\u0419\u041a\u041b\u041c\u041d\u041e\u041f\u0420\u0421\u0422\u0423\u0424\u0425\u0426\u0427\u0428\u0429\u042a\u042b\u042c\u042d\u042e\u042f][\u0080-\u00bf\u0104\u00c6\u013d\u0141\u00d8\u0156\u015a\u0160\u015e\u0164\u0178\u0179\u017d\u017b\u0152\u0105\u00e6\u0192\u013e\u0142\u00f8\u0157\u015b\u0161\u015f\u0165\u017a\u017e\u017c\u0153\u02c6\u02c7\u02d8\u02db\u02dc\u02dd\u0384\u0385\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u0401\u0402\u0403\u0404\u0405\u0406\u0407\u0408\u0409\u040a\u040b\u040c\u040e\u040f\u0451\u0452\u0453\u0454\u0455\u0456\u0457\u0458\u0459\u045a\u045b\u045c\u045e\u045f\u0490\u0491\u2013\u2014\u2015\u2018\u2019\u201a\u201c\u201d\u201e\u2020\u2021\u2022\u2026\u2030\u2039\u203a\u20ac\u2116\u2122]|[\u00e1\u0103\u00e2\u00e4\u00e0\u0101\u0105\u00e5\u00e3\u00e6\u0107\u010d\u00e7\u010f\u00e9\u011b\u00ea\u00eb\u0117\u00e8\u0113\u0119\u0119\u0123\u00ed\u00ee\u00ef\u00ec\u012b\u012f\u0137\u013a\u013c\u0155\u017a\u03b0\u03b1\u03b2\u03b3\u03b4\u03b5\u03b6\u03b7\u03b8\u03b9\u03ba\u03bb\u03bc\u03bd\u03be\u03bf\u0430\u0431\u0432\u0433\u0434\u0435\u0436\u0437\u0438\u0439\u043a\u043b\u043c\u043d\u043e\u043f][\u0080-\u00bf\u0104\u00c6\u013d\u0141\u00d8\u0156\u015a\u0160\u015e\u0164\u0178\u0179\u017d\u017b\u0152\u0105\u00e6\u0192\u013e\u0142\u00f8\u0157\u015b\u0161\u015f\u0165\u017a\u017e\u017c\u0153\u02c6\u02c7\u02d8\u02db\u02dc\u02dd\u0384\u0385\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u0401\u0402\u0403\u0404\u0405\u0406\u0407\u0408\u0409\u040a\u040b\u040c\u040e\u040f\u0451\u0452\u0453\u0454\u0455\u0456\u0457\u0458\u0459\u045a\u045b\u045c\u045e\u045f\u0490\u0491\u2013\u2014\u2015\u2018\u2019\u201a\u201c\u201d\u201e\u2020\u2021\u2022\u2026\u2030\u2039\u203a\u20ac\u2116\u2122]{2}|[\u0111\u00f0\u011f\u00f3\u0161\u03c0\u03c3\u0440\u0443][\u0080-\u00bf\u0104\u00c6\u013d\u0141\u00d8\u0156\u015a\u0160\u015e\u0164\u0178\u0179\u017d\u017b\u0152\u0105\u00e6\u0192\u013e\u0142\u00f8\u0157\u015b\u0161\u015f\u0165\u017a\u017e\u017c\u0153\u02c6\u02c7\u02d8\u02db\u02dc\u02dd\u0384\u0385\u0386\u0388\u0389\u038a\u038c\u038e\u038f\u0401\u0402\u0403\u0404\u0405\u0406\u0407\u0408\u0409\u040a\u040b\u040c\u040e\u040f\u0451\u0452\u0453\u0454\u0455\u0456\u0457\u0458\u0459\u045a\u045b\u045c\u045e\u045f\u0490\u0491\u2013\u2014\u2015\u2018\u2019\u201a\u201c\u201d\u201e\u2020\u2021\u2022\u2026\u2030\u2039\u203a\u20ac\u2116\u2122]{3})+"
.MOJIBAKE_CATEGORIES <- .ftfy_categorias
.BADNESS_RE <- .ftfy_badness_re
.ftfy_es_mojibake <- function(x, usar_extensiones = TRUE) {
  if (!length(x) || is.na(x) || !nzchar(x)) return(FALSE)
  # Todas las categor\u00edas de badness de ftfy contienen al menos un car\u00e1cter
  # no ASCII. Evitar la expresi\u00f3n completa en texto ASCII mantiene barato el
  # perfilado de columnas grandes sin cambiar el predicado.
  if (!grepl("[^\\x00-\\x7f]", x, perl = TRUE)) return(FALSE)
  patron <- if (isTRUE(usar_extensiones)) .ftfy_badness_re else .ftfy_badness_re_ftfy
  grepl(patron, x, perl = TRUE)
}
.ftfy_codificar <- function(texto, tabla, permitir_perdida = FALSE) {
  cps <- utf8ToInt(texto)
  if (anyNA(cps)) return(NULL)
  bytes <- integer(length(cps))
  ascii <- cps < 128L
  bytes[ascii] <- cps[ascii]
  if (any(!ascii)) {
    cp_no_ascii <- cps[!ascii]
    perdidos <- cp_no_ascii == 0xfffdL
    idx <- match(cp_no_ascii[!perdidos], tabla)
    if (anyNA(idx)) return(NULL)
    valores <- integer(length(cp_no_ascii))
    valores[perdidos] <- 0x1aL
    valores[!perdidos] <- 127L + idx
    if (!permitir_perdida && any(perdidos)) return(NULL)
    bytes[!ascii] <- valores
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
.ftfy_decode_inconsistent_utf8 <- function(texto, usar_extensiones = TRUE) {
  # UTF8_DETECTOR_RE encuentra subcadenas de mojibake, no corridas completas
  # de no-ASCII. Cada subcadena se repara recursivamente sólo si es más corta
  # que el valor total, como en ftfy.
  coincidencias <- gregexpr(.ftfy_utf8_detector_re, texto, perl = TRUE)[[1L]]
  if (identical(coincidencias, -1L)) return(texto)
  longitudes <- attr(coincidencias, "match.length")
  salida <- texto
  for (i in rev(seq_along(coincidencias))) {
    desde <- coincidencias[[i]]
    hasta <- desde + longitudes[[i]] - 1L
    segmento <- substring(texto, desde, hasta)
    if (nchar(segmento) < nchar(texto) && .ftfy_es_mojibake(segmento)) {
      reparado <- .ftfy_reparar_uno(
        segmento, usar_extensiones_inicial = usar_extensiones
      )
      if (!identical(reparado$texto, segmento)) {
        salida <- paste0(substr(salida, 1L, desde - 1L), reparado$texto,
                       substr(salida, hasta + 1L, nchar(salida)))
      }
    }
  }
  salida
}
.ftfy_replace_lossy_sequences <- function(bytes) {
  # Es la transliteracion en enteros de LOSSY_UTF8_RE de ftfy. El byte 0x1A
  # representa un U+FFFD que un codec estricto ya habia producido.
  valores <- as.integer(bytes)
  n <- length(valores)
  salida <- integer()
  reemplazo <- c(0xefL, 0xbfL, 0xbdL)
  es_continuacion <- function(x) x >= 0x80L && x <= 0xbfL
  es_marca <- function(x) x == 0x1aL || x == 0x3fL
  es_continuacion_o_sustituto <- function(x) x == 0x1aL || es_continuacion(x)
  i <- 1L
  while (i <= n) {
    inicio <- i
    largo <- 0L
    if (valores[[i]] == 0x1aL) {
      largo <- 1L
    } else if (i < n && valores[[i]] %in% 0xc2:0xdf &&
               valores[[i + 1L]] == 0x1aL) {
      largo <- 2L
    } else if (i < n && valores[[i]] %in% 0xc2:0xc3 &&
               valores[[i + 1L]] == 0x3fL) {
      largo <- 2L
    } else if (i + 5L <= n && valores[[i]] == 0xedL &&
               valores[[i + 1L]] %in% 0xa0:0xaf &&
               es_marca(valores[[i + 2L]]) && valores[[i + 3L]] == 0xedL &&
               valores[[i + 4L]] %in% 0xb0:0xbf &&
               es_continuacion_o_sustituto(valores[[i + 5L]])) {
      largo <- 6L
    } else if (i + 5L <= n && valores[[i]] == 0xedL &&
               valores[[i + 1L]] %in% 0xa0:0xaf &&
               es_continuacion_o_sustituto(valores[[i + 2L]]) &&
               valores[[i + 3L]] == 0xedL &&
               valores[[i + 4L]] %in% 0xb0:0xbf &&
               es_marca(valores[[i + 5L]])) {
      largo <- 6L
    } else if (i + 2L <= n && valores[[i]] %in% 0xe0:0xef &&
               es_marca(valores[[i + 1L]]) &&
               es_continuacion_o_sustituto(valores[[i + 2L]])) {
      largo <- 3L
    } else if (i + 2L <= n && valores[[i]] %in% 0xe0:0xef &&
               es_continuacion_o_sustituto(valores[[i + 1L]]) &&
               es_marca(valores[[i + 2L]])) {
      largo <- 3L
    } else if (i + 3L <= n && valores[[i]] %in% 0xf0:0xf4 &&
               es_marca(valores[[i + 1L]]) &&
               es_continuacion_o_sustituto(valores[[i + 2L]]) &&
               es_continuacion_o_sustituto(valores[[i + 3L]])) {
      largo <- 4L
    } else if (i + 3L <= n && valores[[i]] %in% 0xf0:0xf4 &&
               es_continuacion_o_sustituto(valores[[i + 1L]]) &&
               es_marca(valores[[i + 2L]]) &&
               es_continuacion_o_sustituto(valores[[i + 3L]])) {
      largo <- 4L
    } else if (i + 3L <= n && valores[[i]] %in% 0xf0:0xf4 &&
               es_continuacion_o_sustituto(valores[[i + 1L]]) &&
               es_continuacion_o_sustituto(valores[[i + 2L]]) &&
               es_marca(valores[[i + 3L]])) {
      largo <- 4L
    }
    if (largo) {
      salida <- c(salida, reemplazo)
      i <- inicio + largo
    } else {
      salida <- c(salida, valores[[i]])
      i <- i + 1L
    }
  }
  as.raw(salida)
}
.ftfy_un_paso <- function(texto) {
  for (nombre in names(.ftfy_tablas_bytes)) {
    es_sloppy <- startsWith(nombre, "sloppy-")
    bytes <- .ftfy_codificar(
      texto, .ftfy_tablas_bytes[[nombre]], permitir_perdida = es_sloppy
    )
    if (is.null(bytes)) next
    pasos_transcodificacion <- character()
    if (nombre != "macroman") {
      reparados <- .ftfy_restaurar_a0(bytes)
      if (!is.null(reparados)) {
        bytes <- reparados
        pasos_transcodificacion <- c(pasos_transcodificacion, "restore_byte_a0")
      }
    }
    if (es_sloppy) {
      reemplazados <- .ftfy_replace_lossy_sequences(bytes)
      if (!identical(reemplazados, bytes)) {
        bytes <- reemplazados
        pasos_transcodificacion <- c(pasos_transcodificacion,
                                     "replace_lossy_sequences")
      }
    }
    salida <- .ftfy_desde_utf8(bytes)
    if (!is.null(salida)) {
      if (nombre != "macroman") salida <- .ftfy_normalizar_nbsp(salida)
      if ("restore_byte_a0" %in% pasos_transcodificacion) {
        salida <- .ftfy_ajustar_espacio_a0(texto, salida)
      }
      if (!identical(salida, texto)) {
        sufijo <- paste(c(paste0("encode:", nombre), pasos_transcodificacion,
                          "decode:utf-8"), collapse = ";")
        return(list(texto = salida, paso = sufijo))
      }
    }
  }
  NULL
}
.ftfy_fallback_latin1_windows1252 <- function(texto) {
  cps <- utf8ToInt(texto)
  # ftfy only uses this fallback when the complete value is representable in
  # Latin-1 and is not also representable by its sloppy Windows-1252 map. A
  # value containing, for example, U+20AC must not be rewritten merely because
  # it also contains a C1 control.
  if (!length(cps) || !all(cps <= 255L) ||
      !any(cps >= 128L & cps <= 159L)) return(NULL)
  tabla <- .ftfy_tablas_bytes[["sloppy-windows-1252"]]
  permitidos_windows <- c(0:127, tabla)
  if (all(cps %in% permitidos_windows)) return(NULL)
  indices <- cps >= 128L & cps <= 159L
  cps[indices] <- tabla[cps[indices] - 127L]
  salida <- intToUtf8(cps)
  if (identical(salida, texto)) NULL else salida
}
.ftfy_reparar_uno <- function(x, max_iteraciones = 20L,
                              usar_extensiones_inicial = TRUE) {
  if (length(x) != 1L) {
    return(list(texto = NA_character_, pasos = character(), estado = "sin_texto"))
  }
  valor <- as.character(x)
  if (is.na(valor) || !nzchar(valor)) {
    return(list(texto = valor, pasos = character(), estado = "sin_texto"))
  }
  actual <- enc2utf8(valor)
  pasos <- character()
  usar_extensiones <- isTRUE(usar_extensiones_inicial)
  if (!.ftfy_es_mojibake(actual)) {
    return(list(texto = actual, pasos = pasos, estado = if (length(pasos)) "reparado" else "no_parece_roto"))
  }
  tope <- max(1L, as.integer(max_iteraciones[[1L]]))
  for (i in seq_len(min(tope, 20L))) {
    if (!.ftfy_es_mojibake(actual, usar_extensiones)) break
    paso <- .ftfy_un_paso(actual)
    if (is.null(paso)) {
      inconsistente <- .ftfy_decode_inconsistent_utf8(actual, usar_extensiones)
      if (!identical(inconsistente, actual)) {
        actual <- inconsistente
        pasos <- c(pasos, "decode_inconsistent_utf8")
        # Las extensiones de lupa son puertas de entrada. Una vez que se
        # reparó una subcadena, las vueltas siguientes usan la regla base de
        # ftfy y no reinterpretan el residuo producido por fix_c1_controls.
        usar_extensiones <- FALSE
        if (!.ftfy_es_mojibake(actual, usar_extensiones)) break
        next
      }
      fallback <- .ftfy_fallback_latin1_windows1252(actual)
      if (!is.null(fallback)) {
        actual <- fallback
        pasos <- c(pasos, "encode:latin-1;decode:windows-1252")
        if (!.ftfy_es_mojibake(actual, usar_extensiones)) break
        next
      }
      con_c1 <- .ftfy_fix_c1_controls(actual)
      if (!identical(con_c1, actual)) {
        actual <- con_c1
        pasos <- c(pasos, "fix_c1_controls")
        usar_extensiones <- FALSE
        if (!.ftfy_es_mojibake(actual, usar_extensiones)) break
        next
      }
      break
    }
    if (identical(paso$texto, actual)) break
    actual <- paso$texto
    pasos <- c(pasos, paso$paso)
    if (!.ftfy_es_mojibake(actual, usar_extensiones)) break
  }
  estado <- if (!length(pasos)) {
    "no_se_pudo"
  } else if (.ftfy_es_mojibake(actual, usar_extensiones) ||
             grepl("\uFFFD", actual, fixed = TRUE)) {
    # El marcador de reemplazo conserva la p\u00e9rdida de informaci\u00f3n aunque la
    # medida ya no clasifique el resto del texto como mojibake.
    "reparado_parcialmente"
  } else {
    "reparado"
  }
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
    con_perdida <- grepl("\uFFFD", unicos, fixed = TRUE)
    if (any(con_perdida)) {
      resultados[con_perdida] <- lapply(which(con_perdida), function(i) {
        z <- resultados[[i]]
        if (identical(z$estado, "no_parece_roto") &&
            identical(z$texto, unicos[[i]])) {
          z$estado <- "no_se_pudo"
        }
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
