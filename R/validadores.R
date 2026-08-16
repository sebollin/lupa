# Copias locales consultadas el 2026-08-03. ISO permite usar libremente sus
# códigos; ISO 3166 se mantiene en OBP y SIX publica la lista vigente de 4217.
.codigos_iso3166_alpha2 <- strsplit(
  paste0(
    "AD AE AF AG AI AL AM AO AQ AR AS AT AU AW AX AZ BA BB BD BE BF BG BH BI ",
    "BJ BL BM BN BO BQ BR BS BT BV BW BY BZ CA CC CD CF CG CH CI CK CL CM CN ",
    "CO CR CU CV CW CX CY CZ DE DJ DK DM DO DZ EC EE EG EH ER ES ET FI FJ FK ",
    "FM FO FR GA GB GD GE GF GG GH GI GL GM GN GP GQ GR GS GT GU GW GY HK HM ",
    "HN HR HT HU ID IE IL IM IN IO IQ IR IS IT JE JM JO JP KE KG KH KI KM KN ",
    "KP KR KW KY KZ LA LB LC LI LK LR LS LT LU LV LY MA MC MD ME MF MG MH MK ",
    "ML MM MN MO MP MQ MR MS MT MU MV MW MX MY MZ NA NC NE NF NG NI NL NO NP ",
    "NR NU NZ OM PA PE PF PG PH PK PL PM PN PR PS PT PW PY QA RE RO RS RU RW SA ",
    "SB SC SD SE SG SH SI SJ SK SL SM SN SO SR SS ST SV SX SY SZ TC TD TF TG TH ",
    "TJ TK TL TM TN TO TR TT TV TW TZ UA UG UM US UY UZ VA VC VE VG VI VN VU ",
    "WF WS YE YT ZA ZM ZW"
  ), " ", fixed = TRUE
)[[1L]]

.codigos_iso3166_alpha3 <- strsplit(
  paste0(
    "ABW AFG AGO AIA ALA ALB AND ARE ARG ARM ASM ATA ATF ATG AUS AUT AZE BDI ",
    "BEL BEN BES BFA BGD BGR BHR BHS BIH BLM BLR BLZ BMU BOL BRA BRB BRN BTN ",
    "BVT BWA CAF CAN CCK CHE CHL CHN CIV CMR COD COG COK COL COM CPV CRI CUB ",
    "CUW CXR CYM CYP CZE DEU DJI DMA DNK DOM DZA ECU EGY ERI ESH ESP EST ETH ",
    "FIN FJI FLK FRA FRO FSM GAB GBR GEO GGY GHA GIB GIN GLP GMB GNB GNQ GRC ",
    "GRD GRL GTM GUF GUM GUY HKG HMD HND HRV HTI HUN IDN IMN IND IOT IRL IRN ",
    "IRQ ISL ISR ITA JAM JEY JOR JPN KAZ KEN KGZ KHM KIR KNA KOR KWT LAO LBN ",
    "LBR LBY LCA LIE LKA LSO LTU LUX LVA MAC MAF MAR MCO MDA MDG MDV MEX MHL ",
    "MKD MLI MLT MMR MNE MNG MNP MOZ MRT MSR MTQ MUS MWI MYS MYT NAM NCL NER ",
    "NFK NGA NIC NIU NLD NOR NPL NRU NZL OMN PAK PAN PCN PER PHL PLW PNG POL ",
    "PRI PRK PRT PRY PSE PYF QAT REU ROU RUS RWA SAU SDN SEN SGP SGS SHN SJM ",
    "SLB SLE SLV SMR SOM SPM SRB SSD STP SUR SVK SVN SWE SWZ SXM SYC SYR TCA ",
    "TCD TGO THA TJK TKL TKM TLS TON TTO TUN TUR TUV TWN TZA UGA UKR UMI URY ",
    "USA UZB VAT VCT VEN VGB VIR VNM VUT WLF WSM YEM ZAF ZMB ZWE"
  ), " ", fixed = TRUE
)[[1L]]

.codigos_iso3166_numerico <- strsplit(
  paste0(
    "004 008 010 012 016 020 024 028 031 032 036 040 044 048 050 051 052 056 ",
    "060 064 068 070 072 074 076 084 086 090 092 096 100 104 108 112 116 120 ",
    "124 132 136 140 144 148 152 156 158 162 166 170 174 175 178 180 184 188 ",
    "191 192 196 203 204 208 212 214 218 222 226 231 232 233 234 238 239 242 ",
    "246 248 250 254 258 260 262 266 268 270 275 276 288 292 296 300 304 308 ",
    "312 316 320 324 328 332 334 336 340 344 348 352 356 360 364 368 372 376 ",
    "380 384 388 392 398 400 404 408 410 414 417 418 422 426 428 430 434 438 ",
    "440 442 446 450 454 458 462 466 470 474 478 480 484 492 496 498 499 500 ",
    "504 508 512 516 520 524 528 531 533 534 535 540 548 554 558 562 566 570 ",
    "574 578 580 581 583 584 585 586 591 598 600 604 608 612 616 620 624 626 ",
    "630 634 638 642 643 646 652 654 659 660 662 663 666 670 674 678 682 686 ",
    "688 690 694 702 703 704 705 706 710 716 724 728 729 732 740 744 748 752 ",
    "756 760 762 764 768 772 776 780 784 788 792 795 796 798 800 804 807 818 ",
    "826 831 832 833 834 840 850 854 858 860 862 876 882 887 894"
  ), " ", fixed = TRUE
)[[1L]]

.codigos_iso4217 <- strsplit(
  paste0(
    "AED AFN ALL AMD AOA ARS AUD AWG AZN BAM BBD BDT BHD BIF BMD BND BOB BOV ",
    "BRL BSD BTN BWP BYN BZD CAD CDF CHE CHF CHW CLF CLP CNY COP COU CRC CUP ",
    "CVE CZK DJF DKK DOP DZD EGP ERN ETB EUR FJD FKP GBP GEL GHS GIP GMD GNF ",
    "GTQ GYD HKD HNL HTG HUF IDR ILS INR IQD IRR ISK JMD JOD JPY KES KGS KHR ",
    "KMF KPW KRW KWD KYD KZT LAK LBP LKR LRD LSL LYD MAD MDL MGA MKD MMK MNT ",
    "MOP MRU MUR MVR MWK MXN MXV MYR MZN NAD NGN NIO NOK NPR NZD OMR PAB PEN ",
    "PGK PHP PKR PLN PYG QAR RON RSD RUB RWF SAR SBD SCR SDG SEK SGD SHP SLE ",
    "SOS SRD SSP STN SVC SYP SZL THB TJS TMT TND TOP TRY TTD TWD TZS UAH UGX ",
    "USD USN UYI UYU UYW UZS VED VES VND VUV WST XAD XAF XAG XAU XBA XBB XBC ",
    "XBD XCD XCG XDR XOF XPD XPF XPT XSU XTS XUA XXX YER ZAR ZMW ZWG"
  ), " ", fixed = TRUE
)[[1L]]

.texto_validador <- function(x, recortar = TRUE) {
  if (!is.atomic(x) || is.list(x)) {
    stop("`x` debe ser un vector atomico.", call. = FALSE)
  }
  texto <- as.character(x)
  valido <- !is.na(texto) & validUTF8(texto)
  if (isTRUE(recortar)) texto[valido] <- trimws(texto[valido])
  list(texto = texto, valido = valido)
}

.resultado_validador_vector <- function(x, evaluar, recortar = TRUE) {
  preparado <- .texto_validador(x, recortar = recortar)
  resultado <- rep(FALSE, length(preparado$texto))
  resultado[is.na(preparado$texto)] <- NA
  indices <- which(preparado$valido)
  if (length(indices)) {
    resultado[indices] <- evaluar(preparado$texto[indices])
  }
  resultado
}

#' Validadores internacionales de sintaxis y dígitos de control
#'
#' Estas funciones son puras y vectorizadas: no consultan la red ni prueban la
#' existencia de un país, moneda, buzón o entidad emisora. Los códigos se
#' contrastan con copias locales de las listas vigentes al preparar esta
#' versión del paquete. Los valores ausentes devuelven `NA`; todo valor presente
#' que no cumple el contrato devuelve `FALSE`.
#'
#' `validar_correo()` comprueba un subconjunto práctico y deliberadamente
#' conservador de la sintaxis `addr-spec`: parte local de puntos y caracteres
#' ASCII habituales, seguida por un dominio DNS con al menos un punto. No
#' admite comentarios, cadenas entre comillas ni literales de dominio válidos
#' en la gramática completa de RFC 5322, y no prueba entrega ni existencia.
#'
#' `validar_url()` comprueba URLs jerárquicas con host y acepta sólo los
#' esquemas `http` y `https`: son los esquemas de red que el paquete puede
#' reconocer sin atribuir semántica a protocolos arbitrarios. `javascript:` y
#' `data:` se rechazan deliberadamente, incluso si se parecen a una cadena con
#' dos puntos. Por omisión el esquema es obligatorio; con
#' `esquema_obligatorio = FALSE` también se acepta un host como `ejemplo.uy`.
#' Se aceptan nombres de dominio Unicode (IDN) y nombres ASCII en punycode,
#' puertos entre 1 y 65535, rutas, consultas y fragmentos con codificación
#' porcentual válida. No se consultan DNS ni se afirma que el recurso exista.
#' Los espacios, separadores Unicode y caracteres de control literales hacen
#' que el valor sea inválido; un espacio porcentualmente codificado sigue la
#' sintaxis de una URL y no es un espacio literal.
#'
#' `validar_luhn()` acepta únicamente dígitos y aplica el algoritmo de Luhn.
#' `validar_mod97()` acepta letras ASCII y dígitos, transforma las letras a
#' `A = 10, ..., Z = 35` y exige resto 1 conforme a ISO 7064 MOD 97-10. No
#' reordena caracteres: protocolos como IBAN deben hacer antes su transformación
#' propia.
#'
#' @param x Vector que se desea validar.
#' @param tipo Forma de código ISO 3166: dos letras (`"alpha2"`), tres
#'   (`"alpha3"`) o tres dígitos (`"numerico"`). Los valores numéricos de uno o
#'   dos dígitos se completan con ceros a la izquierda.
#' @param esquema_obligatorio Si es `TRUE`, exige `http://` o `https://`.
#'   `TRUE` es el valor estricto por omisión; `FALSE` permite una URL con host
#'   sin esquema, como `ejemplo.uy`.
#'
#' @return Vector lógico de la misma longitud que `x`.
#' @name validadores_formato
#' @seealso [pack_validadores()], [validar_ci_uy()], [especializar()]
#'
#' @references
#' International Organization for Standardization. *ISO 3166 Country Codes*.
#' <https://www.iso.org/iso-3166-country-codes.html>
#'
#' SIX Group. *ISO 4217 Currency Codes*.
#' <https://www.six-group.com/en/products-services/financial-information/market-reference-data/data-standards.html>
#'
#' Resnick P (2008). *Internet Message Format*, RFC 5322.
#' <https://www.rfc-editor.org/rfc/rfc5322>
#'
#' Luhn HP (1960). *Computer for Verifying Numbers*, US Patent 2,950,048.
#' <https://patents.google.com/patent/US2950048A/en>
#'
#' International Organization for Standardization. *ISO/IEC 7064:2003*.
#' <https://www.iso.org/standard/31531.html>
#'
#' @examples
#' validar_iso3166(c("UY", "CL", "ZZ"))
#' validar_iso4217(c("UYU", "CLP", "ZZZ"))
#' validar_correo(c("persona@example.org", "sin-arroba"))
#' validar_url(c("https://ejemplo.uy", "ejemplo.uy"))
#' validar_url("ejemplo.uy", esquema_obligatorio = FALSE)
#' validar_luhn(c("79927398713", "79927398714"))
#' validar_mod97(c("9999123456789012141490", "9999123456789012141491"))
NULL

.url_porcentaje_valido <- function(valor) {
  grepl("^(?:[^%]|%[0-9A-Fa-f]{2})*$", valor, perl = TRUE)
}

.url_componentes_validos <- function(valor) {
  .url_porcentaje_valido(valor) &&
    !grepl("[<>\"{}|\\^`]", valor, perl = TRUE)
}

.url_ipv4_valido <- function(host) {
  partes <- strsplit(host, ".", fixed = TRUE)[[1L]]
  length(partes) == 4L && all(grepl("^[0-9]{1,3}$", partes, perl = TRUE)) &&
    all(as.integer(partes) <= 255L)
}

.url_ipv6_valido <- function(host) {
  if (!grepl("^[0-9A-Fa-f:.]+$", host, perl = TRUE) ||
      !grepl(":", host, fixed = TRUE)) return(FALSE)
  posiciones_compresion <- gregexpr("::", host, fixed = TRUE)[[1L]]
  if (sum(posiciones_compresion > 0L) > 1L) return(FALSE)
  comprimido <- grepl("::", host, fixed = TRUE)
  grupos <- strsplit(host, ":", fixed = TRUE)[[1L]]
  grupos <- grupos[nzchar(grupos)]
  if (any(!grepl("^[0-9A-Fa-f]{1,4}$", grupos, perl = TRUE))) {
    # La parte final puede ser una dirección IPv4 embebida.
    ipv4 <- utils::tail(grupos, 1L)
    if (!.url_ipv4_valido(ipv4)) return(FALSE)
    grupos <- utils::head(grupos, -1L)
    if (length(grupos) && any(!grepl("^[0-9A-Fa-f]{1,4}$", grupos,
                                      perl = TRUE))) return(FALSE)
    cantidad <- length(grupos) + 2L
  } else {
    cantidad <- length(grupos)
  }
  if (comprimido) cantidad < 8L else cantidad == 8L
}

.url_host_valido <- function(host) {
  if (!nzchar(host)) return(FALSE)
  if (startsWith(host, "[") || endsWith(host, "]")) {
    return(startsWith(host, "[") && endsWith(host, "]") &&
      .url_ipv6_valido(substr(host, 2L, nchar(host) - 1L)))
  }
  if (grepl("[\\[\\]:]", host, perl = TRUE)) return(FALSE)
  if (grepl("^[0-9.]+$", host, perl = TRUE) && grepl("\\.", host, perl = TRUE)) {
    return(.url_ipv4_valido(host))
  }
  host_sin_punto_final <- sub("\\.$", "", host)
  if (!nzchar(host_sin_punto_final) || nchar(host, type = "bytes") > 253L) {
    return(FALSE)
  }
  etiquetas <- strsplit(host_sin_punto_final, ".", fixed = TRUE)[[1L]]
  all(nchar(etiquetas, type = "bytes") <= 63L) &&
    all(grepl(
      "^[\\p{L}\\p{N}](?:[\\p{L}\\p{N}\\p{M}-]*[\\p{L}\\p{N}\\p{M}])?$",
      etiquetas, perl = TRUE
    ))
}

.url_uno <- function(valor, esquema_obligatorio) {
  if (!nzchar(valor) ||
      grepl("[[:space:]]|\\p{Z}|\\p{Cc}", valor, perl = TRUE) ||
      !.url_porcentaje_valido(valor)) return(FALSE)

  tiene_esquema <- grepl("^[A-Za-z][A-Za-z0-9+.-]*:", valor, perl = TRUE)
  if (tiene_esquema) {
    esquema <- tolower(sub(":.*$", "", valor, perl = TRUE))
    if (!esquema %in% c("http", "https")) return(FALSE)
    if (!grepl("^[A-Za-z][A-Za-z0-9+.-]*://", valor, perl = TRUE)) {
      return(FALSE)
    }
    cuerpo <- sub("^[A-Za-z][A-Za-z0-9+.-]*://", "", valor, perl = TRUE)
  } else {
    if (isTRUE(esquema_obligatorio) || grepl("^//", valor, perl = TRUE)) {
      return(FALSE)
    }
    cuerpo <- valor
  }
  if (!nzchar(cuerpo) || grepl("^/", cuerpo, perl = TRUE)) return(FALSE)

  posicion <- regexpr("[/?#]", cuerpo, perl = TRUE)[[1L]]
  if (posicion < 0L) {
    autoridad <- cuerpo
    resto <- ""
  } else {
    autoridad <- substr(cuerpo, 1L, posicion - 1L)
    resto <- substr(cuerpo, posicion, nchar(cuerpo))
  }
  if (!nzchar(autoridad) || !.url_componentes_validos(resto)) return(FALSE)

  usuario <- sub("@[^@]*$", "", autoridad, perl = TRUE)
  host_puerto <- if (grepl("@", autoridad, fixed = TRUE)) {
    sub("^.*@", "", autoridad, perl = TRUE)
  } else autoridad
  if (grepl("@", host_puerto, fixed = TRUE)) return(FALSE)
  if (nzchar(usuario) && !.url_componentes_validos(usuario)) return(FALSE)

  puerto <- ""
  puerto_declarado <- FALSE
  if (startsWith(host_puerto, "[")) {
    cierre <- regexpr("]", host_puerto, fixed = TRUE)[[1L]]
    if (cierre < 0L) return(FALSE)
    host <- substr(host_puerto, 1L, cierre)
    sufijo <- substr(host_puerto, cierre + 1L, nchar(host_puerto))
    if (nzchar(sufijo)) {
      if (!startsWith(sufijo, ":")) return(FALSE)
      puerto_declarado <- TRUE
      puerto <- substr(sufijo, 2L, nchar(sufijo))
    }
  } else {
    posiciones_colon <- gregexpr(":", host_puerto, fixed = TRUE)[[1L]]
    cantidad_colon <- sum(posiciones_colon > 0L)
    if (cantidad_colon == 1L) {
      host <- sub(":.*$", "", host_puerto, perl = TRUE)
      puerto_declarado <- TRUE
      puerto <- sub("^.*:", "", host_puerto, perl = TRUE)
    } else if (cantidad_colon > 1L) {
      return(FALSE)
    } else {
      host <- host_puerto
    }
  }
  if (puerto_declarado &&
      (!grepl("^[0-9]{1,5}$", puerto, perl = TRUE) ||
       as.numeric(puerto) < 1 || as.numeric(puerto) > 65535)) return(FALSE)
  .url_host_valido(host)
}

#' @rdname validadores_formato
#' @export
validar_url <- function(x, esquema_obligatorio = TRUE) {
  if (!is.logical(esquema_obligatorio) ||
      length(esquema_obligatorio) != 1L || is.na(esquema_obligatorio)) {
    stop("`esquema_obligatorio` debe ser TRUE o FALSE.", call. = FALSE)
  }
  .resultado_validador_vector(
    x,
    function(valor) vapply(
      valor, .url_uno, logical(1L), esquema_obligatorio = esquema_obligatorio
    ),
    recortar = FALSE
  )
}

#' @rdname validadores_formato
#' @export
validar_iso3166 <- function(x, tipo = c("alpha2", "alpha3", "numerico")) {
  tipo <- match.arg(tipo)
  if (identical(tipo, "numerico")) {
    return(.resultado_validador_vector(x, function(valor) {
      candidato <- rep("", length(valor))
      digitos <- grepl("^[0-9]{1,3}$", valor, perl = TRUE)
      candidato[digitos] <- sprintf("%03d", as.integer(valor[digitos]))
      candidato %in% .codigos_iso3166_numerico
    }))
  }
  codigos <- if (identical(tipo, "alpha2")) .codigos_iso3166_alpha2 else {
    .codigos_iso3166_alpha3
  }
  .resultado_validador_vector(x, function(valor) toupper(valor) %in% codigos)
}

#' @rdname validadores_formato
#' @export
validar_iso4217 <- function(x) {
  .resultado_validador_vector(
    x, function(valor) toupper(valor) %in% .codigos_iso4217
  )
}

#' @rdname validadores_formato
#' @export
validar_correo <- function(x) {
  .resultado_validador_vector(x, function(valor) {
    longitud <- nchar(valor, type = "bytes")
    estructura <- grepl(
      paste0(
        "^[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+",
        "(?:\\.[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+)*@",
        "[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?",
        "(?:\\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)+$"
      ),
      valor, perl = TRUE
    )
    partes <- strsplit(valor, "@", fixed = TRUE)
    local <- vapply(partes, function(z) if (length(z)) z[[1L]] else "", "")
    estructura & longitud <= 254L & nchar(local, type = "bytes") <= 64L
  })
}

.luhn_vector <- function(valor) {
  resultado <- rep(FALSE, length(valor))
  validos <- grepl("^[0-9]+$", valor, perl = TRUE)
  if (!any(validos)) return(resultado)
  indices <- which(validos)
  longitudes <- nchar(valor[indices], type = "chars")
  for (longitud in unique(longitudes)) {
    posiciones <- indices[longitudes == longitud]
    texto <- valor[posiciones]
    digitos <- do.call(cbind, lapply(seq_len(longitud), function(j) {
      as.integer(substr(texto, j, j))
    }))
    # La duplicación se cuenta desde la derecha y no incluye el dígito de
    # control (la última posición).
    duplicar <- (longitud - seq_len(longitud)) %% 2L == 1L
    digitos[, duplicar] <- digitos[, duplicar, drop = FALSE] * 2L
    digitos[digitos > 9L] <- digitos[digitos > 9L] - 9L
    resultado[posiciones] <- rowSums(digitos) %% 10L == 0L
  }
  resultado
}

#' @rdname validadores_formato
#' @export
validar_luhn <- function(x) {
  .resultado_validador_vector(x, .luhn_vector)
}

.resto_mod97 <- function(valor) {
  caracteres <- strsplit(toupper(valor), "", fixed = TRUE)[[1L]]
  if (!length(caracteres) || any(!grepl("^[A-Z0-9]$", caracteres))) {
    return(NA_integer_)
  }
  expandido <- paste0(vapply(caracteres, function(caracter) {
    if (grepl("^[0-9]$", caracter)) caracter else {
      as.character(match(caracter, LETTERS) + 9L)
    }
  }, character(1L)), collapse = "")
  resto <- 0L
  for (digito in utf8ToInt(expandido) - utf8ToInt("0")) {
    resto <- (resto * 10L + digito) %% 97L
  }
  resto
}

#' @rdname validadores_formato
#' @export
validar_mod97 <- function(x) {
  .resultado_validador_vector(x, function(valor) {
    vapply(valor, function(z) identical(.resto_mod97(z), 1L), logical(1L))
  })
}

#' Validadores estructurales de Uruguay
#'
#' `validar_ci_uy()` comprueba la cédula de identidad mediante su dígito
#' verificador módulo 10. Acepta siete u ocho dígitos y separadores de puntos,
#' espacios o guion; una cédula de siete dígitos se completa con cero inicial.
#'
#' `validar_rut_uy()` comprueba la estructura de doce dígitos del RUT y su
#' dígito verificador módulo 11. El algoritmo operativo no está publicado por
#' DGI como especificación normativa abierta; esta implementación se contrastó
#' con la implementación pública de `python-stdnum` y con ejemplos públicos.
#' Por eso valida estructura y dígito, no vigencia ni existencia registral.
#'
#' @inheritParams validar_iso3166
#'
#' @return Vector lógico de la misma longitud que `x`.
#' @name validadores_uy
#' @seealso [pack_validadores()], [validar_iso3166()]
#'
#' @references
#' Poder Ejecutivo de Uruguay (1978). Decreto 501/978, artículo 2.
#' <https://www.impo.com.uy/bases/decretos/501-1978/2>
#'
#' Unidad de Acceso a la Información Pública (2024). Resolución 145/024.
#' <https://www.gub.uy/unidad-acceso-informacion-publica/institucional/normativa/resolucion-n-145024-sobre-reserva-informacion>
#'
#' de Jong A. *python-stdnum: Uruguay RUT*.
#' <https://github.com/arthurdejong/python-stdnum/blob/master/stdnum/uy/rut.py>
#'
#' @examples
#' validar_ci_uy(c("1.234.567-2", "1.234.567-3"))
#' validar_rut_uy(c("21 100 342 0017", "21 030 367 0014"))
NULL

.solo_digitos <- function(valor) gsub("[.[:space:]-]", "", valor, perl = TRUE)

.ci_uy_vector <- function(valor) {
  resultado <- rep(FALSE, length(valor))
  digitos <- .solo_digitos(valor)
  validos <- grepl("^[0-9]{7,8}$", digitos, perl = TRUE)
  if (!any(validos)) return(resultado)
  indices <- which(validos)
  texto <- digitos[indices]
  texto[nchar(texto) == 7L] <- paste0("0", texto[nchar(texto) == 7L])
  numeros <- do.call(cbind, lapply(seq_len(8L), function(j) {
    as.integer(substr(texto, j, j))
  }))
  sumas <- as.vector(numeros[, seq_len(7L), drop = FALSE] %*%
    c(2L, 9L, 8L, 7L, 6L, 3L, 4L))
  esperado <- (10L - sumas %% 10L) %% 10L
  resultado[indices] <- numeros[, 8L] == esperado
  resultado
}

#' @rdname validadores_uy
#' @export
validar_ci_uy <- function(x) {
  .resultado_validador_vector(x, .ci_uy_vector)
}

.rut_uy_uno <- function(valor) {
  digitos <- .solo_digitos(valor)
  if (!grepl("^[0-9]{12}$", digitos, perl = TRUE)) return(FALSE)
  numeros <- utf8ToInt(digitos) - utf8ToInt("0")
  prefijo <- as.integer(substr(digitos, 1L, 2L))
  if (prefijo < 1L || prefijo > 22L ||
      identical(substr(digitos, 3L, 8L), "000000") ||
      !identical(substr(digitos, 9L, 11L), "001")) return(FALSE)
  verificador <- (-sum(numeros[1:11] * c(4L, 3L, 2L, 9L, 8L, 7L,
                                         6L, 5L, 4L, 3L, 2L))) %% 11L
  verificador <= 9L && numeros[[12L]] == verificador
}

#' @rdname validadores_uy
#' @export
validar_rut_uy <- function(x) {
  .resultado_validador_vector(
    x, function(valor) vapply(valor, .rut_uy_uno, logical(1L))
  )
}

#' Crear y consultar packs de validadores
#'
#' Un pack es una lista con nombres de funciones vectorizadas. No se registra
#' en estado global: puede definirse en otro paquete o en un script y conectarse
#' directamente con la propiedad `validador` de `Formato` o con
#' `validadores_personales` en [perfilar()]. Esta forma permite agregar países o
#' dominios sin modificar el núcleo de `lupa`; el pack uruguayo predeterminado
#' de `perfilar()` usa exactamente esta misma puerta.
#'
#' @param nombre Nombre del pack.
#' @param validadores Lista con nombres de funciones que aceptan un vector y
#'   devuelven un vector lógico de igual longitud.
#' @param pais Código ISO 3166 alpha-2 opcional del país al que pertenece el
#'   pack. `NULL` representa un pack internacional o no territorial.
#' @param descripcion Descripción breve opcional.
#'
#' @return `pack_validadores()` devuelve un objeto S3 `pack_validadores`.
#'   `validadores_internacionales()` y `validadores_uruguay()` devuelven packs
#'   preparados para usar.
#' @export
#' @seealso [especializar()], [validar_iso3166()], [validar_ci_uy()]
#'
#' @examples
#' internacionales <- validadores_internacionales()
#' internacionales$correo(c("persona@example.org", "incorrecto"))
#'
#' # Un pack de otro país se construye sin registrar ni cambiar el núcleo.
#' digito_rut_cl <- function(x) {
#'   uno <- function(valor) {
#'     z <- toupper(gsub("[.-]", "", valor))
#'     if (!grepl("^[0-9]{7,8}[0-9K]$", z)) return(FALSE)
#'     cuerpo <- as.integer(strsplit(substr(z, 1, nchar(z) - 1), "")[[1]])
#'     suma <- sum(rev(cuerpo) * rep(2:7, length.out = length(cuerpo)))
#'     esperado <- 11 - suma %% 11
#'     esperado <- if (esperado == 11) "0" else if (esperado == 10) "K" else
#'       as.character(esperado)
#'     identical(substr(z, nchar(z), nchar(z)), esperado)
#'   }
#'   ifelse(is.na(x), NA, vapply(as.character(x), uno, logical(1)))
#' }
#' chile <- pack_validadores(
#'   "Chile", list(rut = digito_rut_cl), pais = "CL",
#'   descripcion = "Validadores mantenidos por el proyecto consumidor."
#' )
#' chile$rut(c("12.345.678-5", "12.345.678-4"))
#' @export
pack_validadores <- function(nombre, validadores, pais = NULL,
                             descripcion = NULL) {
  if (!is.character(nombre) || length(nombre) != 1L || is.na(nombre) ||
      !nzchar(trimws(nombre))) {
    stop("`nombre` debe ser una cadena no vacia.", call. = FALSE)
  }
  if (!is.list(validadores) || !length(validadores) ||
      is.null(names(validadores)) || anyNA(names(validadores)) ||
      any(!nzchar(names(validadores))) || anyDuplicated(names(validadores)) ||
      !all(vapply(validadores, is.function, logical(1L)))) {
    stop("`validadores` debe ser una lista con nombres unicos de funciones.",
         call. = FALSE)
  }
  if (!is.null(pais)) {
    if (!is.character(pais) || length(pais) != 1L || is.na(pais) ||
        !validar_iso3166(pais, "alpha2")) {
      stop("`pais` debe ser un codigo ISO 3166 alpha-2 vigente o NULL.",
           call. = FALSE)
    }
    pais <- toupper(pais)
  }
  if (!is.null(descripcion) && (!is.character(descripcion) ||
      length(descripcion) != 1L || is.na(descripcion) || !nzchar(descripcion))) {
    stop("`descripcion` debe ser una cadena no vacia o NULL.", call. = FALSE)
  }
  estructura <- validadores
  attr(estructura, "nombre") <- trimws(nombre)
  attr(estructura, "pais") <- pais
  attr(estructura, "descripcion") <- descripcion
  class(estructura) <- c("pack_validadores", "list")
  estructura
}

#' @rdname pack_validadores
#' @export
validadores_internacionales <- function() {
  pack_validadores(
    "Internacionales",
    list(
      iso3166_alpha2 = function(x) validar_iso3166(x, "alpha2"),
      iso3166_alpha3 = function(x) validar_iso3166(x, "alpha3"),
      iso4217 = validar_iso4217,
      correo = validar_correo,
      luhn = validar_luhn,
      mod97 = validar_mod97
    ),
    descripcion = "Codigos y digitos de control de uso internacional."
  )
}

#' @rdname pack_validadores
#' @export
validadores_uruguay <- function() {
  pack_validadores(
    "Uruguay", list(cedula = validar_ci_uy, rut = validar_rut_uy), pais = "UY",
    descripcion = "Validadores estructurales de documentos uruguayos."
  )
}

#' @export
print.pack_validadores <- function(x, ...) {
  cat("<pack_validadores>", attr(x, "nombre", exact = TRUE), "\n")
  pais <- attr(x, "pais", exact = TRUE)
  if (!is.null(pais)) cat("Pais:", pais, "\n")
  cat("Validadores:", paste(names(x), collapse = ", "), "\n")
  invisible(x)
}
