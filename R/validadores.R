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

.texto_validador <- function(x) {
  if (!is.atomic(x) || is.list(x)) {
    stop("`x` debe ser un vector atomico.", call. = FALSE)
  }
  texto <- as.character(x)
  valido <- !is.na(texto) & validUTF8(texto)
  texto[valido] <- trimws(texto[valido])
  list(texto = texto, valido = valido)
}

.resultado_validador_vector <- function(x, evaluar) {
  preparado <- .texto_validador(x)
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
#' validar_luhn(c("79927398713", "79927398714"))
#' validar_mod97(c("9999123456789012141490", "9999123456789012141491"))
NULL

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

.luhn_uno <- function(valor) {
  if (!grepl("^[0-9]+$", valor, perl = TRUE)) return(FALSE)
  digitos <- utf8ToInt(valor) - utf8ToInt("0")
  posiciones <- rev(seq_along(digitos))
  duplicar <- posiciones %% 2L == 0L
  digitos[duplicar] <- digitos[duplicar] * 2L
  digitos[digitos > 9L] <- digitos[digitos > 9L] - 9L
  sum(digitos) %% 10L == 0L
}

#' @rdname validadores_formato
#' @export
validar_luhn <- function(x) {
  .resultado_validador_vector(
    x, function(valor) vapply(valor, .luhn_uno, logical(1L))
  )
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
#' <https://arthurdejong.org/python-stdnum/doc/2.2/stdnum.uy.rut>
#'
#' @examples
#' validar_ci_uy(c("1.234.567-2", "1.234.567-3"))
#' validar_rut_uy(c("21 100 342 0017", "21 030 367 0014"))
NULL

.solo_digitos <- function(valor) gsub("[.[:space:]-]", "", valor, perl = TRUE)

.ci_uy_uno <- function(valor) {
  digitos <- .solo_digitos(valor)
  if (!grepl("^[0-9]{7,8}$", digitos, perl = TRUE)) return(FALSE)
  digitos <- paste0(strrep("0", 8L - nchar(digitos)), digitos)
  numeros <- utf8ToInt(digitos) - utf8ToInt("0")
  verificador <- (10L - sum(numeros[1:7] * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
  numeros[[8L]] == verificador
}

#' @rdname validadores_uy
#' @export
validar_ci_uy <- function(x) {
  .resultado_validador_vector(
    x, function(valor) vapply(valor, .ci_uy_uno, logical(1L))
  )
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
#' directamente con la propiedad `validador` de `Formato`. Esta forma permite
#' agregar países o dominios sin modificar el núcleo de `lupa`.
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
