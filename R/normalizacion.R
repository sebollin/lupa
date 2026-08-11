#' Perfiles de normalizacion para comparar valores
#'
#' La normalizacion afecta unicamente la representacion usada para comparar;
#' nunca modifica los datos de entrada. La descomposicion canonica y el orden
#' de sus marcas son siempre activos para que NFC y NFD sean equivalentes en el
#' subconjunto latino cubierto por lupa.
#'
#' @param minusculas,espacios,acentos,comillas,puntuacion,ligaduras,ancho
#'   Activan el paso correspondiente.
#' @param proteger Grafemas cuyas marcas deben conservarse al quitar acentos.
#'   Puede incluir una base seguida de una o más marcas combinantes, como
#'   \code{"g\u0303"} para la letra guaraní.
#' @return Un objeto de clase normalizacion_lupa.
#' @export
normalizacion <- function(minusculas = TRUE, espacios = TRUE, acentos = TRUE,
                          comillas = TRUE, puntuacion = FALSE,
                          ligaduras = FALSE, ancho = FALSE,
                          proteger = c("\u00f1", "\u00fc",
                                       intToUtf8(c(103L, 771L)))) {
  valores <- list(
    minusculas = minusculas, espacios = espacios, acentos = acentos,
    comillas = comillas, puntuacion = puntuacion, ligaduras = ligaduras,
    ancho = ancho
  )
  if (any(!vapply(valores, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1L)))) {
    stop("Los pasos de normalizacion() deben ser logicos escalares.", call. = FALSE)
  }
  if (!is.character(proteger) || anyNA(proteger) || any(!nzchar(proteger))) {
    stop("proteger debe ser un vector de caracteres no vacio ni NA.", call. = FALSE)
  }
  if (any(vapply(proteger, function(x) {
    codigos <- .normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    )
    !length(codigos) || .normalizacion_clase(codigos[[1L]]) != 0L ||
      any(vapply(codigos[-1L], .normalizacion_clase, integer(1L)) == 0L)
  }, logical(1L)))) {
    stop("Cada elemento de proteger debe ser un grafema Unicode valido.",
         call. = FALSE)
  }
  structure(c(valores, list(proteger = unique(proteger))),
            class = c("normalizacion_lupa", "list"))
}

#' @export
print.normalizacion_lupa <- function(x, ...) {
  cat("Perfil de normalizacion de lupa\n")
  pasos <- c("minusculas", "espacios", "acentos", "comillas", "puntuacion",
             "ligaduras", "ancho")
  cat(paste0("  ", pasos, " = ", vapply(x[pasos], as.character, character(1L)),
             collapse = "\n"), "\n", sep = "")
  cat("  proteger = ", paste(x$proteger, collapse = ", "), "\n", sep = "")
  invisible(x)
}
.normalizacion_preset <- function(nombre) {
  if (identical(nombre, "amplio")) {
    return(normalizacion(puntuacion = TRUE, ligaduras = TRUE, ancho = TRUE))
  }
  stop("El preset de normalizar debe ser amplio.", call. = FALSE)
}
.normalizacion_resuelta <- function(general, por_columna = list()) {
  estructura <- list(general = general, por_columna = por_columna)
  class(estructura) <- c("normalizacion_resuelta_lupa", "list")
  estructura
}
.resolver_normalizacion <- function(normalizar = TRUE, perfil = NULL) {
  if (is.null(normalizar)) {
    heredada <- if (!is.null(perfil) && inherits(perfil, "perfil")) {
      perfil$meta$normalizacion
    } else NULL
    normalizar <- if (is.null(heredada)) TRUE else heredada
  }
  if (inherits(normalizar, "normalizacion_resuelta_lupa")) return(normalizar)
  if (inherits(normalizar, "normalizacion_lupa")) {
    return(.normalizacion_resuelta(normalizar))
  }
  if (is.logical(normalizar) && length(normalizar) == 1L && !is.na(normalizar)) {
    return(.normalizacion_resuelta(normalizacion(
      minusculas = normalizar, espacios = normalizar, acentos = normalizar,
      comillas = normalizar
    )))
  }
  if (is.logical(normalizar)) {
    stop("normalizar debe ser un vector de l\u00f3gicos escalares sin NA.",
         call. = FALSE)
  }
  if (is.character(normalizar) && length(normalizar) == 1L && !is.na(normalizar)) {
    return(.normalizacion_resuelta(.normalizacion_preset(normalizar)))
  }
  if (is.list(normalizar)) {
    nombres <- names(normalizar)
    if (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres)) ||
        anyDuplicated(nombres)) {
      stop("La lista de normalizar debe tener nombres de columnas unicos.", call. = FALSE)
    }
    perfiles <- lapply(normalizar, function(x) {
      if (is.character(x) && length(x) == 1L) x <- .normalizacion_preset(x)
      if (is.logical(x) && length(x) == 1L && !is.na(x)) {
        x <- normalizacion(minusculas = x, espacios = x, acentos = x,
                           comillas = x)
      }
      if (!inherits(x, "normalizacion_lupa")) {
        stop("Cada valor de la lista normalizar debe ser un perfil valido.",
             call. = FALSE)
      }
      x
    })
    return(.normalizacion_resuelta(normalizacion(), perfiles))
  }
  stop("normalizar debe ser TRUE, FALSE, amplio, un perfil o una lista nombrada.",
       call. = FALSE)
}
.normalizacion_para_columna <- function(resuelta, columna) {
  if (!inherits(resuelta, "normalizacion_resuelta_lupa")) {
    resuelta <- .resolver_normalizacion(resuelta)
  }
  if (length(resuelta$por_columna) && columna %in% names(resuelta$por_columna)) {
    resuelta$por_columna[[columna]]
  } else resuelta$general
}
.normalizacion_resumen <- function(resuelta) {
  if (!inherits(resuelta, "normalizacion_resuelta_lupa")) {
    resuelta <- .resolver_normalizacion(resuelta)
  }
  list(general = unclass(resuelta$general),
       por_columna = lapply(resuelta$por_columna, unclass))
}

.normalizacion_decomposiciones <-
  list(
    "192" = c(65, 768),
    "193" = c(65, 769),
    "194" = c(65, 770),
    "195" = c(65, 771),
    "196" = c(65, 776),
    "197" = c(65, 778),
    "199" = c(67, 807),
    "200" = c(69, 768),
    "201" = c(69, 769),
    "202" = c(69, 770),
    "203" = c(69, 776),
    "204" = c(73, 768),
    "205" = c(73, 769),
    "206" = c(73, 770),
    "207" = c(73, 776),
    "209" = c(78, 771),
    "210" = c(79, 768),
    "211" = c(79, 769),
    "212" = c(79, 770),
    "213" = c(79, 771),
    "214" = c(79, 776),
    "217" = c(85, 768),
    "218" = c(85, 769),
    "219" = c(85, 770),
    "220" = c(85, 776),
    "221" = c(89, 769),
    "224" = c(97, 768),
    "225" = c(97, 769),
    "226" = c(97, 770),
    "227" = c(97, 771),
    "228" = c(97, 776),
    "229" = c(97, 778),
    "231" = c(99, 807),
    "232" = c(101, 768),
    "233" = c(101, 769),
    "234" = c(101, 770),
    "235" = c(101, 776),
    "236" = c(105, 768),
    "237" = c(105, 769),
    "238" = c(105, 770),
    "239" = c(105, 776),
    "241" = c(110, 771),
    "242" = c(111, 768),
    "243" = c(111, 769),
    "244" = c(111, 770),
    "245" = c(111, 771),
    "246" = c(111, 776),
    "249" = c(117, 768),
    "250" = c(117, 769),
    "251" = c(117, 770),
    "252" = c(117, 776),
    "253" = c(121, 769),
    "255" = c(121, 776),
    "256" = c(65, 772),
    "257" = c(97, 772),
    "258" = c(65, 774),
    "259" = c(97, 774),
    "260" = c(65, 808),
    "261" = c(97, 808),
    "262" = c(67, 769),
    "263" = c(99, 769),
    "264" = c(67, 770),
    "265" = c(99, 770),
    "266" = c(67, 775),
    "267" = c(99, 775),
    "268" = c(67, 780),
    "269" = c(99, 780),
    "270" = c(68, 780),
    "271" = c(100, 780),
    "274" = c(69, 772),
    "275" = c(101, 772),
    "276" = c(69, 774),
    "277" = c(101, 774),
    "278" = c(69, 775),
    "279" = c(101, 775),
    "280" = c(69, 808),
    "281" = c(101, 808),
    "282" = c(69, 780),
    "283" = c(101, 780),
    "284" = c(71, 770),
    "285" = c(103, 770),
    "286" = c(71, 774),
    "287" = c(103, 774),
    "288" = c(71, 775),
    "289" = c(103, 775),
    "290" = c(71, 807),
    "291" = c(103, 807),
    "292" = c(72, 770),
    "293" = c(104, 770),
    "296" = c(73, 771),
    "297" = c(105, 771),
    "298" = c(73, 772),
    "299" = c(105, 772),
    "300" = c(73, 774),
    "301" = c(105, 774),
    "302" = c(73, 808),
    "303" = c(105, 808),
    "304" = c(73, 775),
    "308" = c(74, 770),
    "309" = c(106, 770),
    "310" = c(75, 807),
    "311" = c(107, 807),
    "313" = c(76, 769),
    "314" = c(108, 769),
    "315" = c(76, 807),
    "316" = c(108, 807),
    "317" = c(76, 780),
    "318" = c(108, 780),
    "323" = c(78, 769),
    "324" = c(110, 769),
    "325" = c(78, 807),
    "326" = c(110, 807),
    "327" = c(78, 780),
    "328" = c(110, 780),
    "332" = c(79, 772),
    "333" = c(111, 772),
    "334" = c(79, 774),
    "335" = c(111, 774),
    "336" = c(79, 779),
    "337" = c(111, 779),
    "340" = c(82, 769),
    "341" = c(114, 769),
    "342" = c(82, 807),
    "343" = c(114, 807),
    "344" = c(82, 780),
    "345" = c(114, 780),
    "346" = c(83, 769),
    "347" = c(115, 769),
    "348" = c(83, 770),
    "349" = c(115, 770),
    "350" = c(83, 807),
    "351" = c(115, 807),
    "352" = c(83, 780),
    "353" = c(115, 780),
    "354" = c(84, 807),
    "355" = c(116, 807),
    "356" = c(84, 780),
    "357" = c(116, 780),
    "360" = c(85, 771),
    "361" = c(117, 771),
    "362" = c(85, 772),
    "363" = c(117, 772),
    "364" = c(85, 774),
    "365" = c(117, 774),
    "366" = c(85, 778),
    "367" = c(117, 778),
    "368" = c(85, 779),
    "369" = c(117, 779),
    "370" = c(85, 808),
    "371" = c(117, 808),
    "372" = c(87, 770),
    "373" = c(119, 770),
    "374" = c(89, 770),
    "375" = c(121, 770),
    "376" = c(89, 776),
    "377" = c(90, 769),
    "378" = c(122, 769),
    "379" = c(90, 775),
    "380" = c(122, 775),
    "381" = c(90, 780),
    "382" = c(122, 780),
    "416" = c(79, 795),
    "417" = c(111, 795),
    "431" = c(85, 795),
    "432" = c(117, 795),
    "461" = c(65, 780),
    "462" = c(97, 780),
    "463" = c(73, 780),
    "464" = c(105, 780),
    "465" = c(79, 780),
    "466" = c(111, 780),
    "467" = c(85, 780),
    "468" = c(117, 780),
    "469" = c(85, 776, 772),
    "470" = c(117, 776, 772),
    "471" = c(85, 776, 769),
    "472" = c(117, 776, 769),
    "473" = c(85, 776, 780),
    "474" = c(117, 776, 780),
    "475" = c(85, 776, 768),
    "476" = c(117, 776, 768),
    "478" = c(65, 776, 772),
    "479" = c(97, 776, 772),
    "480" = c(65, 775, 772),
    "481" = c(97, 775, 772),
    "482" = c(198, 772),
    "483" = c(230, 772),
    "486" = c(71, 780),
    "487" = c(103, 780),
    "488" = c(75, 780),
    "489" = c(107, 780),
    "490" = c(79, 808),
    "491" = c(111, 808),
    "492" = c(79, 808, 772),
    "493" = c(111, 808, 772),
    "494" = c(439, 780),
    "495" = c(658, 780),
    "496" = c(106, 780),
    "500" = c(71, 769),
    "501" = c(103, 769),
    "504" = c(78, 768),
    "505" = c(110, 768),
    "506" = c(65, 778, 769),
    "507" = c(97, 778, 769),
    "508" = c(198, 769),
    "509" = c(230, 769),
    "510" = c(216, 769),
    "511" = c(248, 769),
    "512" = c(65, 783),
    "513" = c(97, 783),
    "514" = c(65, 785),
    "515" = c(97, 785),
    "516" = c(69, 783),
    "517" = c(101, 783),
    "518" = c(69, 785),
    "519" = c(101, 785),
    "520" = c(73, 783),
    "521" = c(105, 783),
    "522" = c(73, 785),
    "523" = c(105, 785),
    "524" = c(79, 783),
    "525" = c(111, 783),
    "526" = c(79, 785),
    "527" = c(111, 785),
    "528" = c(82, 783),
    "529" = c(114, 783),
    "530" = c(82, 785),
    "531" = c(114, 785),
    "532" = c(85, 783),
    "533" = c(117, 783),
    "534" = c(85, 785),
    "535" = c(117, 785),
    "536" = c(83, 806),
    "537" = c(115, 806),
    "538" = c(84, 806),
    "539" = c(116, 806),
    "542" = c(72, 780),
    "543" = c(104, 780),
    "550" = c(65, 775),
    "551" = c(97, 775),
    "552" = c(69, 807),
    "553" = c(101, 807),
    "554" = c(79, 776, 772),
    "555" = c(111, 776, 772),
    "556" = c(79, 771, 772),
    "557" = c(111, 771, 772),
    "558" = c(79, 775),
    "559" = c(111, 775),
    "560" = c(79, 775, 772),
    "561" = c(111, 775, 772),
    "562" = c(89, 772),
    "563" = c(121, 772),
    "7680" = c(65, 805),
    "7681" = c(97, 805),
    "7682" = c(66, 775),
    "7683" = c(98, 775),
    "7684" = c(66, 803),
    "7685" = c(98, 803),
    "7686" = c(66, 817),
    "7687" = c(98, 817),
    "7688" = c(67, 807, 769),
    "7689" = c(99, 807, 769),
    "7690" = c(68, 775),
    "7691" = c(100, 775),
    "7692" = c(68, 803),
    "7693" = c(100, 803),
    "7694" = c(68, 817),
    "7695" = c(100, 817),
    "7696" = c(68, 807),
    "7697" = c(100, 807),
    "7698" = c(68, 813),
    "7699" = c(100, 813),
    "7700" = c(69, 772, 768),
    "7701" = c(101, 772, 768),
    "7702" = c(69, 772, 769),
    "7703" = c(101, 772, 769),
    "7704" = c(69, 813),
    "7705" = c(101, 813),
    "7706" = c(69, 816),
    "7707" = c(101, 816),
    "7708" = c(69, 807, 774),
    "7709" = c(101, 807, 774),
    "7710" = c(70, 775),
    "7711" = c(102, 775),
    "7712" = c(71, 772),
    "7713" = c(103, 772),
    "7714" = c(72, 775),
    "7715" = c(104, 775),
    "7716" = c(72, 803),
    "7717" = c(104, 803),
    "7718" = c(72, 776),
    "7719" = c(104, 776),
    "7720" = c(72, 807),
    "7721" = c(104, 807),
    "7722" = c(72, 814),
    "7723" = c(104, 814),
    "7724" = c(73, 816),
    "7725" = c(105, 816),
    "7726" = c(73, 776, 769),
    "7727" = c(105, 776, 769),
    "7728" = c(75, 769),
    "7729" = c(107, 769),
    "7730" = c(75, 803),
    "7731" = c(107, 803),
    "7732" = c(75, 817),
    "7733" = c(107, 817),
    "7734" = c(76, 803),
    "7735" = c(108, 803),
    "7736" = c(76, 803, 772),
    "7737" = c(108, 803, 772),
    "7738" = c(76, 817),
    "7739" = c(108, 817),
    "7740" = c(76, 813),
    "7741" = c(108, 813),
    "7742" = c(77, 769),
    "7743" = c(109, 769),
    "7744" = c(77, 775),
    "7745" = c(109, 775),
    "7746" = c(77, 803),
    "7747" = c(109, 803),
    "7748" = c(78, 775),
    "7749" = c(110, 775),
    "7750" = c(78, 803),
    "7751" = c(110, 803),
    "7752" = c(78, 817),
    "7753" = c(110, 817),
    "7754" = c(78, 813),
    "7755" = c(110, 813),
    "7756" = c(79, 771, 769),
    "7757" = c(111, 771, 769),
    "7758" = c(79, 771, 776),
    "7759" = c(111, 771, 776),
    "7760" = c(79, 772, 768),
    "7761" = c(111, 772, 768),
    "7762" = c(79, 772, 769),
    "7763" = c(111, 772, 769),
    "7764" = c(80, 769),
    "7765" = c(112, 769),
    "7766" = c(80, 775),
    "7767" = c(112, 775),
    "7768" = c(82, 775),
    "7769" = c(114, 775),
    "7770" = c(82, 803),
    "7771" = c(114, 803),
    "7772" = c(82, 803, 772),
    "7773" = c(114, 803, 772),
    "7774" = c(82, 817),
    "7775" = c(114, 817),
    "7776" = c(83, 775),
    "7777" = c(115, 775),
    "7778" = c(83, 803),
    "7779" = c(115, 803),
    "7780" = c(83, 769, 775),
    "7781" = c(115, 769, 775),
    "7782" = c(83, 780, 775),
    "7783" = c(115, 780, 775),
    "7784" = c(83, 803, 775),
    "7785" = c(115, 803, 775),
    "7786" = c(84, 775),
    "7787" = c(116, 775),
    "7788" = c(84, 803),
    "7789" = c(116, 803),
    "7790" = c(84, 817),
    "7791" = c(116, 817),
    "7792" = c(84, 813),
    "7793" = c(116, 813),
    "7794" = c(85, 804),
    "7795" = c(117, 804),
    "7796" = c(85, 816),
    "7797" = c(117, 816),
    "7798" = c(85, 813),
    "7799" = c(117, 813),
    "7800" = c(85, 771, 769),
    "7801" = c(117, 771, 769),
    "7802" = c(85, 772, 776),
    "7803" = c(117, 772, 776),
    "7804" = c(86, 771),
    "7805" = c(118, 771),
    "7806" = c(86, 803),
    "7807" = c(118, 803),
    "7808" = c(87, 768),
    "7809" = c(119, 768),
    "7810" = c(87, 769),
    "7811" = c(119, 769),
    "7812" = c(87, 776),
    "7813" = c(119, 776),
    "7814" = c(87, 775),
    "7815" = c(119, 775),
    "7816" = c(87, 803),
    "7817" = c(119, 803),
    "7818" = c(88, 775),
    "7819" = c(120, 775),
    "7820" = c(88, 776),
    "7821" = c(120, 776),
    "7822" = c(89, 775),
    "7823" = c(121, 775),
    "7824" = c(90, 770),
    "7825" = c(122, 770),
    "7826" = c(90, 803),
    "7827" = c(122, 803),
    "7828" = c(90, 817),
    "7829" = c(122, 817),
    "7830" = c(104, 817),
    "7831" = c(116, 776),
    "7832" = c(119, 778),
    "7833" = c(121, 778),
    "7835" = c(383, 775),
    "7840" = c(65, 803),
    "7841" = c(97, 803),
    "7842" = c(65, 777),
    "7843" = c(97, 777),
    "7844" = c(65, 770, 769),
    "7845" = c(97, 770, 769),
    "7846" = c(65, 770, 768),
    "7847" = c(97, 770, 768),
    "7848" = c(65, 770, 777),
    "7849" = c(97, 770, 777),
    "7850" = c(65, 770, 771),
    "7851" = c(97, 770, 771),
    "7852" = c(65, 803, 770),
    "7853" = c(97, 803, 770),
    "7854" = c(65, 774, 769),
    "7855" = c(97, 774, 769),
    "7856" = c(65, 774, 768),
    "7857" = c(97, 774, 768),
    "7858" = c(65, 774, 777),
    "7859" = c(97, 774, 777),
    "7860" = c(65, 774, 771),
    "7861" = c(97, 774, 771),
    "7862" = c(65, 803, 774),
    "7863" = c(97, 803, 774),
    "7864" = c(69, 803),
    "7865" = c(101, 803),
    "7866" = c(69, 777),
    "7867" = c(101, 777),
    "7868" = c(69, 771),
    "7869" = c(101, 771),
    "7870" = c(69, 770, 769),
    "7871" = c(101, 770, 769),
    "7872" = c(69, 770, 768),
    "7873" = c(101, 770, 768),
    "7874" = c(69, 770, 777),
    "7875" = c(101, 770, 777),
    "7876" = c(69, 770, 771),
    "7877" = c(101, 770, 771),
    "7878" = c(69, 803, 770),
    "7879" = c(101, 803, 770),
    "7880" = c(73, 777),
    "7881" = c(105, 777),
    "7882" = c(73, 803),
    "7883" = c(105, 803),
    "7884" = c(79, 803),
    "7885" = c(111, 803),
    "7886" = c(79, 777),
    "7887" = c(111, 777),
    "7888" = c(79, 770, 769),
    "7889" = c(111, 770, 769),
    "7890" = c(79, 770, 768),
    "7891" = c(111, 770, 768),
    "7892" = c(79, 770, 777),
    "7893" = c(111, 770, 777),
    "7894" = c(79, 770, 771),
    "7895" = c(111, 770, 771),
    "7896" = c(79, 803, 770),
    "7897" = c(111, 803, 770),
    "7898" = c(79, 795, 769),
    "7899" = c(111, 795, 769),
    "7900" = c(79, 795, 768),
    "7901" = c(111, 795, 768),
    "7902" = c(79, 795, 777),
    "7903" = c(111, 795, 777),
    "7904" = c(79, 795, 771),
    "7905" = c(111, 795, 771),
    "7906" = c(79, 795, 803),
    "7907" = c(111, 795, 803),
    "7908" = c(85, 803),
    "7909" = c(117, 803),
    "7910" = c(85, 777),
    "7911" = c(117, 777),
    "7912" = c(85, 795, 769),
    "7913" = c(117, 795, 769),
    "7914" = c(85, 795, 768),
    "7915" = c(117, 795, 768),
    "7916" = c(85, 795, 777),
    "7917" = c(117, 795, 777),
    "7918" = c(85, 795, 771),
    "7919" = c(117, 795, 771),
    "7920" = c(85, 795, 803),
    "7921" = c(117, 795, 803),
    "7922" = c(89, 768),
    "7923" = c(121, 768),
    "7924" = c(89, 803),
    "7925" = c(121, 803),
    "7926" = c(89, 777),
    "7927" = c(121, 777),
    "7928" = c(89, 771),
    "7929" = c(121, 771)
  )

.normalizacion_clases_combinantes <- c(
  "768" = 230L, "769" = 230L, "770" = 230L, "771" = 230L,
  "772" = 230L, "773" = 230L, "774" = 230L, "775" = 230L,
  "776" = 230L, "777" = 230L, "778" = 230L, "779" = 220L,
  "780" = 230L, "781" = 230L, "782" = 230L, "783" = 230L,
  "784" = 230L, "785" = 230L, "786" = 232L, "787" = 202L,
  "788" = 220L, "789" = 220L, "790" = 230L, "791" = 230L,
  "792" = 230L, "793" = 230L, "794" = 230L, "795" = 216L,
  "796" = 220L, "797" = 220L, "798" = 220L, "799" = 202L,
  "800" = 202L, "801" = 230L, "802" = 220L, "803" = 220L,
  "804" = 230L, "805" = 230L, "806" = 130L, "807" = 202L,
  "808" = 202L, "809" = 230L, "810" = 220L, "811" = 230L,
  "812" = 220L, "813" = 230L, "814" = 230L, "815" = 230L,
  "816" = 230L, "817" = 230L, "818" = 230L
)
.normalizacion_descomponer <- function(codigos) {
  expandir <- function(codigo) {
    valor <- .normalizacion_decomposiciones[[as.character(codigo)]]
    if (is.null(valor)) return(as.integer(codigo))
    unlist(lapply(valor, expandir), use.names = FALSE)
  }
  unlist(lapply(codigos, expandir), use.names = FALSE)
}
.normalizacion_clase <- function(codigo) {
  valor <- unname(.normalizacion_clases_combinantes[as.character(codigo)])
  if (!length(valor) || is.na(valor)) 0L else as.integer(valor)
}
.normalizacion_ordenar <- function(codigos) {
  if (length(codigos) < 2L) return(codigos)
  clases <- vapply(codigos, .normalizacion_clase, integer(1L))
  salida <- integer()
  inicio <- 1L
  while (inicio <= length(codigos)) {
    fin <- inicio
    while (fin < length(codigos) && clases[[fin + 1L]] != 0L) fin <- fin + 1L
    if (fin > inicio) {
      bloque <- inicio:fin
      if (clases[[inicio]] == 0L) {
        marcas <- (inicio + 1L):fin
        orden <- c(inicio, marcas[order(clases[marcas], seq_along(marcas),
                                        method = "radix")])
      } else {
        orden <- bloque[order(clases[bloque], seq_along(bloque),
                              method = "radix")]
      }
      salida <- c(salida, codigos[orden])
    } else {
      salida <- c(salida, codigos[inicio:fin])
    }
    inicio <- fin + 1L
  }
  salida
}
.normalizacion_protecciones <- function(perfil) {
  lapply(perfil$proteger, function(x) {
    codigos <- .normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    )
    list(base = codigos[[1L]], marcas = codigos[-1L])
  })
}
.normalizacion_quitar_acentos <- function(codigos, perfil, protecciones = NULL) {
  if (is.null(protecciones)) protecciones <- .normalizacion_protecciones(perfil)
  salida <- integer()
  i <- 1L
  while (i <= length(codigos)) {
    if (.normalizacion_clase(codigos[[i]]) != 0L) {
      i <- i + 1L
      next
    }
    j <- i
    while (j < length(codigos) &&
           .normalizacion_clase(codigos[[j + 1L]]) != 0L) j <- j + 1L
    marcas <- if (j > i) codigos[(i + 1L):j] else integer()
    conservar <- any(vapply(protecciones, function(p) {
      identical(as.integer(p$base), as.integer(codigos[[i]])) &&
        identical(as.integer(p$marcas), as.integer(marcas))
    }, logical(1L)))
    salida <- c(salida, codigos[[i]], if (conservar) marcas else integer())
    i <- j + 1L
  }
  salida
}
.normalizacion_comillas <- function(codigos) {
  aperturas <- c(34L, 0xAB, 0x2018, 0x201B, 0x201C, 0x201E, 0x2039)
  cierres <- c(34L, 0xBB, 0x2019, 0x201D, 0x201F, 0x203A)
  es_letra <- function(x) (x >= 65L && x <= 90L) ||
    (x >= 97L && x <= 122L) || x > 127L
  borrar <- logical(length(codigos))
  for (i in seq_along(codigos)) {
    anterior <- if (i == 1L) 32L else codigos[[i - 1L]]
    siguiente <- if (i == length(codigos)) 32L else codigos[[i + 1L]]
    if (codigos[[i]] %in% aperturas && es_letra(siguiente) &&
        (i == 1L || anterior == 32L)) borrar[[i]] <- TRUE
    if (codigos[[i]] %in% cierres && es_letra(anterior) &&
        (i == length(codigos) || siguiente == 32L)) borrar[[i]] <- TRUE
    if (codigos[[i]] %in% c(39L, 0x2019, 0x201A) && es_letra(anterior) &&
        (i == length(codigos) || siguiente == 32L)) borrar[[i]] <- TRUE
  }
  codigos <- codigos[!borrar]
  codigos[codigos %in% c(0x2019, 0x201A, 0x201B)] <- 39L
  codigos
}
.normalizacion_ancho <- function(codigos) {
  vapply(codigos, function(x) {
    if (x >= 0xFF01 && x <= 0xFF5E) x - 0xFEE0L else
      if (x == 0x3000) 32L else x
  }, integer(1L))
}
.normalizacion_ligaduras <- function(codigos) {
  mapa <- list("64256" = c(102L, 102L), "64257" = c(102L, 105L),
               "64258" = c(102L, 108L), "64259" = c(102L, 102L, 105L),
               "64260" = c(102L, 102L, 108L), "64261" = c(383L),
               "64262" = c(115L, 116L))
  unlist(lapply(codigos, function(x) {
    valor <- mapa[[as.character(x)]]
    if (is.null(valor)) x else valor
  }), use.names = FALSE)
}
.normalizacion_puntuacion <- function(codigos) {
  quitar <- vapply(codigos, function(x) {
    (x >= 33L && x <= 47L) || (x >= 58L && x <= 64L) ||
      (x >= 91L && x <= 96L) || (x >= 123L && x <= 126L) ||
      (x >= 0x2000 && x <= 0x206F) || (x >= 0x3001 && x <= 0x303F)
  }, logical(1L))
  codigos[!quitar]
}
.normalizacion_minusculas <- function(texto) {
  codigos <- utf8ToInt(texto)
  # El ASCII I se fija antes de delegar el resto en R para que un locale
  # turco no convierta una comparacion reproducible en U+0131.
  codigos[codigos == 73L] <- 105L
  tolower(paste0(intToUtf8(codigos, multiple = TRUE), collapse = ""))
}
.normalizacion_espacios_codigos <- function(codigos) {
  if (!length(codigos)) return(codigos)
  codigos[codigos %in% .codigos_espacios_invisibles] <- 32L
  codigos[!(codigos %in% .codigos_control_eliminable_set)]
}
.normalizacion_a_texto <- function(codigos) {
  paste0(intToUtf8(codigos, multiple = TRUE), collapse = "")
}
.normalizacion_regex_codigos <- function(codigos) {
  if (!length(codigos)) return("(?!)")
  paste0("(*UTF)[",
         paste0("\\x{", sprintf("%04X", as.integer(codigos)), "}",
                collapse = ""), "]")
}

# La tabla es pequeña, pero construir la alternancia y sus reemplazos para
# cada llamada era una parte apreciable del costo del perfil de fusiones. Se
# guarda una sola vez y se aplica con gregexpr/regmatches sobre el vector
# completo. Los reemplazos pueden tener distinta longitud (por ejemplo, una
# letra vietnamita se descompone en tres puntos de código), por eso chartr no
# alcanza para esta tabla.
.normalizacion_tabla_vectorizada <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      codigos <- as.integer(names(.normalizacion_decomposiciones))
      caracteres <- intToUtf8(codigos, multiple = TRUE)
      reemplazos <- vapply(
        codigos,
        function(codigo) .normalizacion_a_texto(
          .normalizacion_ordenar(.normalizacion_descomponer(codigo))
        ),
        character(1L)
      )
      cache <<- list(
        patron = paste0("(*UTF)(?:", paste0(
          "\\x{", sprintf("%04X", codigos), "}", collapse = "|"
        ), ")"),
        reemplazos = stats::setNames(reemplazos, caracteres)
      )
    }
    cache
  }
})

.normalizacion_reemplazar_tabla <- function(textos) {
  tabla <- .normalizacion_tabla_vectorizada()
  aciertos <- !is.na(textos) & grepl(tabla$patron, textos, perl = TRUE)
  if (!any(aciertos)) return(textos)
  seleccion <- which(aciertos)
  afectados <- textos[seleccion]
  coincidencias <- gregexpr(tabla$patron, afectados, perl = TRUE)
  encontrados <- regmatches(afectados, coincidencias)
  reemplazos <- lapply(encontrados, function(x) {
    if (!length(x)) character() else unname(tabla$reemplazos[x])
  })
  regmatches(afectados, coincidencias) <- reemplazos
  textos[seleccion] <- afectados
  textos
}

.normalizacion_proteger_vector <- function(textos, perfil) {
  if (!isTRUE(perfil$acentos) || !length(perfil$proteger)) {
    return(list(textos = textos, marcadores = character(),
                colision = rep(FALSE, length(textos))))
  }
  grafemas <- vapply(perfil$proteger, function(x) {
    .normalizacion_a_texto(.normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    ))
  }, character(1L))
  orden <- order(nchar(grafemas, type = "chars"), decreasing = TRUE,
                 method = "radix")
  grafemas <- grafemas[orden]
  marcadores <- intToUtf8(0xF0000L + seq_along(grafemas), multiple = TRUE)
  colision <- rep(FALSE, length(textos))
  for (marcador in marcadores) {
    colision <- colision | (!is.na(textos) &
      grepl(marcador, textos, fixed = TRUE, useBytes = FALSE))
  }
  for (i in seq_along(grafemas)) {
    # No proteger un prefijo de un grafema con marcas adicionales: por
    # ejemplo, `ü` no debe conservarse dentro de `ǘ` (u + diéresis + agudo),
    # que el camino escalar considera un grafema distinto.
    patron <- paste0(
      "(*UTF)",
      paste0("\\x{", sprintf("%04X", utf8ToInt(grafemas[[i]])), "}",
             collapse = ""),
      "(?![\\x{0300}-\\x{0332}])"
    )
    textos <- gsub(patron, marcadores[[i]], textos, perl = TRUE)
  }
  list(textos = textos, marcadores = stats::setNames(marcadores, grafemas),
       colision = colision)
}

.normalizacion_vector_rapida <- function(textos, perfil) {
  original <- as.character(textos)
  nombres <- names(original)
  salida <- original
  names(salida) <- nombres
  no_na <- !is.na(salida)
  if (!any(no_na)) return(salida)

  # Los pliegues optativos y las comillas dependen de contexto o de una tabla
  # de longitud variable. Los valores que los necesitan se delegan abajo al
  # recorrido escalar; el caso común (texto sin esas señales) permanece en C.
  lento <- rep(FALSE, length(salida))
  if (isTRUE(perfil$comillas)) {
    lento <- lento | (!is.na(original) & grepl(
      "(*UTF)[\\\"'\\x{00AB}\\x{00BB}\\x{2018}-\\x{201F}\\x{2039}-\\x{203A}]",
      original, perl = TRUE
    ))
  }
  if (isTRUE(perfil$puntuacion) || isTRUE(perfil$ligaduras) ||
      isTRUE(perfil$ancho)) {
    lento[no_na] <- TRUE
  }

  salida <- .normalizacion_reemplazar_tabla(salida)
  if (isTRUE(perfil$acentos)) {
    protegidos <- .normalizacion_proteger_vector(salida, perfil)
    salida <- protegidos$textos
    lento <- lento | protegidos$colision
  } else {
    protegidos <- list(marcadores = character())
  }

  if (isTRUE(perfil$acentos)) {
    salida <- gsub("(*UTF)[\\x{0300}-\\x{0332}]", "", salida,
                   perl = TRUE)
  }
  if (length(protegidos$marcadores)) {
    for (grafema in names(protegidos$marcadores)) {
      salida <- gsub(unname(protegidos$marcadores[[grafema]]), grafema,
                     salida, fixed = TRUE, useBytes = FALSE)
    }
  }
  if (isTRUE(perfil$minusculas)) {
    salida <- tolower(chartr("I", "i", salida))
  }
  if (isTRUE(perfil$espacios)) {
    salida <- gsub(.normalizacion_regex_codigos(.codigos_espacios_invisibles),
                   " ", salida, perl = TRUE)
    salida <- gsub(.normalizacion_regex_codigos(
      .codigos_control_eliminable_set
    ), "", salida, perl = TRUE)
    salida <- trimws(gsub("[[:space:]]+", " ", salida, perl = TRUE))
  }

  # Dos o más marcas requieren el orden canónico estable; las comillas y los
  # pliegues optativos requieren el contexto de la implementación escalar.
  marcas <- "[\\x{0300}-\\x{0332}]"
  complejas <- !is.na(salida) & grepl(
    paste0("(*UTF)", marcas, ".*", marcas), salida, perl = TRUE
  )
  lento <- lento | complejas
  if (any(lento & no_na)) {
    indices <- which(lento & no_na)
    salida[indices] <- vapply(
      original[indices], .normalizacion_uno, character(1L),
      perfil = perfil, protecciones = NULL, USE.NAMES = FALSE
    )
  }
  salida
}
.normalizacion_etapas <- function(texto, perfil, protecciones = NULL) {
  if (is.na(texto)) return(stats::setNames(NA_character_, "entrada"))
  codigos <- utf8ToInt(as.character(texto))
  etapas <- list(entrada = .normalizacion_a_texto(codigos))
  agregar <- function(nombre, nuevos) {
    etapas[[nombre]] <<- .normalizacion_a_texto(nuevos)
    codigos <<- nuevos
  }
  if (isTRUE(perfil$espacios)) {
    agregar("espacios", .normalizacion_espacios_codigos(codigos))
  }
  if (isTRUE(perfil$ancho)) {
    agregar("ancho", .normalizacion_ancho(codigos))
  }
  if (isTRUE(perfil$ligaduras)) {
    agregar("ligaduras", .normalizacion_ligaduras(codigos))
  }
  if (isTRUE(perfil$comillas)) {
    agregar("comillas", .normalizacion_comillas(codigos))
  }
  if (isTRUE(perfil$puntuacion)) {
    agregar("puntuacion", .normalizacion_puntuacion(codigos))
  }
  codigos <- .normalizacion_ordenar(.normalizacion_descomponer(codigos))
  agregar("descomposicion_canonica", codigos)
  if (isTRUE(perfil$acentos)) {
    if (is.null(protecciones)) protecciones <- .normalizacion_protecciones(perfil)
    agregar("acentos", .normalizacion_quitar_acentos(
      codigos, perfil, protecciones))
  }
  if (isTRUE(perfil$minusculas)) {
    texto_actual <- .normalizacion_minusculas(.normalizacion_a_texto(codigos))
    codigos <- utf8ToInt(texto_actual)
    etapas[["minusculas"]] <- texto_actual
  }
  if (isTRUE(perfil$espacios)) {
    texto_actual <- trimws(gsub("[[:space:]]+", " ",
      .normalizacion_a_texto(codigos), perl = TRUE))
    etapas[["espacios_finales"]] <- texto_actual
  }
  etapas
}
.normalizacion_uno <- function(texto, perfil, protecciones = NULL) {
  etapas <- .normalizacion_etapas(texto, perfil, protecciones)
  unname(etapas[[length(etapas)]])
}
.normalizacion_aplicar <- function(textos, perfil) {
  .normalizacion_vector_rapida(textos, perfil)
}
.normalizacion_fusiones <- function(textos, perfil) {
  if (!length(textos)) {
    return(list(pasos = list(), n_distintos_normalizados = 0L))
  }
  completo <- .normalizacion_aplicar(textos, perfil)
  n_completo <- length(unique(completo))
  pasos <- list()
  nombres <- c("espacios", "ancho", "ligaduras", "comillas",
               "puntuacion", "acentos", "minusculas")
  activos <- nombres[vapply(perfil[nombres], isTRUE, logical(1L))]
  for (nombre in activos) {
    sin_paso <- perfil
    sin_paso[[nombre]] <- FALSE
    sin_normalizar <- .normalizacion_aplicar(textos, sin_paso)
    pasos[[nombre]] <- max(0L,
      length(unique(sin_normalizar)) - n_completo)
  }
  list(pasos = pasos, n_distintos_normalizados = n_completo)
}
.normalizacion_fusiones_vocabulario <- function(textos, perfil) {
  n_distintos <- length(textos)
  fusiones <- .normalizacion_fusiones(textos, perfil)
  list(
    estado = "exacto",
    n_distintos = n_distintos,
    n_usados = n_distintos,
    n_distintos_normalizados = fusiones$n_distintos_normalizados,
    pasos = fusiones$pasos
  )
}
.normalizacion_fusiones_tabla <- function(datos, resuelta) {
  nombres <- names(datos)
  if (is.null(nombres)) nombres <- paste0("V", seq_along(datos))
  salida <- lapply(seq_along(datos), function(i) {
    x <- datos[[i]]
    if (is.list(x) || is.matrix(x)) return(list())
    valores <- suppressWarnings(as.character(.texto_analizable(x)$valores))
    valores <- unique(valores[!is.na(valores)])
    .normalizacion_fusiones_vocabulario(
      valores, .normalizacion_para_columna(resuelta, nombres[[i]])
    )
  })
  names(salida) <- make.unique(nombres)
  salida
}
