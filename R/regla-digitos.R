# La regla de digitos de la proteccion de datos personales: cuando un numero de
# un texto publicado es un documento protegido -o el documento sin su
# verificador, o sin su primer digito-. Vive aparte de
# `.reemplazar_variantes_separadas()` para poder probarla sola.
#
# Ronda 21. La regla de la ronda 20 aceptaba solo la forma de miles -grupos del
# medio de tres cifras- y un unico separador de una lista corta. Un telefono
# "2901 1234", "099 12 34 56", una tarjeta "4111 1111 1111 1111", un IBAN, o la
# cedula escrita "4 . 123 . 456-7" no dejaban ningun tramo que fuera el valor, y
# se publicaban enteros; antes de esa ronda se tapaban. Y del otro lado tapaba
# de mas: la parte decimal de una coordenada o de un p-valor de siete cifras
# coincidia con un documento sin su primer digito. Medido en una refutacion.
#
# Ahora hay dos niveles. Un numero COMPACTO une grupos de digitos con un solo
# signo -"4.123.456-7", "6723640.38"-; un numero SUELTO une compactos con espacios
# o parentesis -"(2) 487 1234", "2901 1234"-. Del suelto se prueba como documento
# entero cualquier concatenacion de grupos contiguos, con cualquier agrupacion.
# El documento PARCIAL se busca con mas cuidado, porque es el que coincide por
# azar: sin su verificador, en las concatenaciones de grupos que no son de un
# decimal; sin su primer digito, solo si un comodin lo reemplaza -"*.012.345-8",
# "X.123.456-7"-, que es como se escribe el documento recortado a proposito.

# El ano de una fecha plausible, de 1800 a 2100: lo comparten el piso de
# `.valores_identificantes()` y la regla de digitos, que antes lo escribian cada
# uno a su manera. Hasta 2100 y no 2199: un telefono fijo "2101-12-12" tiene
# forma de fecha valida, y una fecha del siglo XXII en un dato es rara.
.ANIO_PLAUSIBLE <- "(?:1[89][0-9]{2}|20[0-9]{2}|2100)"

.REGLA_DIGITOS <- local({
  anio <- .ANIO_PLAUSIBLE
  octeto <- "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])"
  espacio <- "[ \u00a0\u2009\u202f]"
  # La coma une solo sin espacio despues: "4,123,456" es un numero y
  # "67, 20, 855" es una lista.
  # Con espacios alrededor solo unen el guion y el punto -"2600 - 1122",
  # "4 . 123 . 456-7"- y el parentesis de un prefijo -"(2) 487 1234"-; los demas
  # signos, pegados. Con cualquiera, "2983 4272 / 2988 2968" eran un solo numero
  # y su tramo del medio coincidia con un telefono. Medido en la ronda 22.
  #
  # Los dos puntos y la barra vertical NO unen: una razon "494:2714" y una lista
  # "12|345|6789" coincidian con una cedula en uno de cada siete y uno de cada
  # cuatro casos. Medido en la ronda 23. Un documento escrito con ellos no se
  # reconoce, y la ayuda lo dice.
  separador <- paste0(
    "(?:", espacio, "{1,2}|", espacio, "?[-.]{1,2}", espacio, "?|",
    "[-./'_+\u00b7()\\[\\]]{1,2}|[)\\]]", espacio, "|", espacio, "[(\\[]|",
    ",(?!", espacio, "))"
  )
  signo_compacto <- "[-./,'_+\u00b7]"
  # Entre la fecha y la hora -ver `fecha_hora`-. La barra, la arroba, el guion
  # doble y el corchete se sumaron en la ronda 25: la fecha con hora protegida
  # se publicaba escrita "2024-06-07 / 09:04:05", " @ ", " -- " o con la hora
  # entre corchetes, mientras sus hermanas " - ", ";" y "|" se tapaban.
  entre_fecha_hora <- "(?:[Tt_]| {1,3}| *(?:--|[-;,|/@]) *| *[(\\[] *)"
  numero <- paste0("[0-9]+(?:", separador, "[0-9]+)*")
  list(
    numero = numero,
    compacto = paste0("[0-9]+(?:", signo_compacto, "[0-9]+)*"),
    signo_compacto = signo_compacto,
    # El comodin es el que tapa cifras: el asterisco y la equis. El numeral, el
    # signo de pregunta y la vineta precedian numeros de ticket y de lista, y
    # uno de cada cuatro se tapaba como documento recortado. Medido en la ronda
    # 23.
    # El asterisco vale tambien pegado a una letra -"CI*123.456-7"-; la equis no,
    # porque es parte de la palabra. Y tras la equis el numero sigue pegado o con
    # un signo de los que unen, no con un espacio: "92 x 6931234" y
    # "pack x 1781643" son una multiplicacion y una cantidad, y tres de cada diez
    # se tapaban. Medido en la ronda 24.
    comodin = paste0(
      "(?:(?<!\\p{N})\\*[*xX]*(?:", separador, ")?|",
      "(?<![\\p{L}\\p{N}])[xX][*xX]*(?:", signo_compacto, "{1,2})?)(", numero, ")"
    ),
    # Una fecha de calendario no es un documento: partida en tramos,
    # `2003-05-17` da `20030517`, que con un millon de cedulas es la de
    # alguien. Con un ANO plausible, 1800 a 2199: con cualquiera, el telefono
    # "2901-12-12" se borraba como fecha y se publicaba. Medido en la ronda 22.
    # Y con espacios, solo con el ano al final -"16 01 2002"-: una de cada
    # veintidos se tapaba. Medido en la ronda 24. Con el ano adelante no, porque
    # "2012 11 05" es tambien un telefono fijo escrito de a pares.
    fecha = paste0(
      "(?<![0-9])(?:", anio, "[-/.](?:0?[1-9]|1[0-2])[-/.](?:0?[1-9]|[12][0-9]|3[01])|",
      "(?:0?[1-9]|[12][0-9]|3[01])[-/.](?:0?[1-9]|1[0-2])[-/.]", anio, "|",
      "(?:(?:0?[1-9]|[12][0-9]|3[01]) (?:0?[1-9]|1[0-2])|",
      "(?:0?[1-9]|1[0-2]) (?:0?[1-9]|[12][0-9]|3[01])) ", anio, ")(?![0-9])"
    ),
    # Y la hora, por lo mismo -`12:34:56`-, con su fraccion de segundo: sin ella,
    # la fraccion de `12:26:58.5412576` quedaba como un numero suelto de siete
    # cifras, y una de cada ocho coincidia con una cedula.
    # La fraccion con tope -nueve cifras tras un punto, tres tras una coma-: sin
    # el, la hora se tragaba un documento pegado, "10:30:00,41234567".
    # Tras el punto, de una a siete cifras o nueve -milisegundos, microsegundos,
    # los diez millonesimos de .NET, nanosegundos-, nunca ocho; y la fraccion no
    # va seguida de un guion y una sola cifra, que es un verificador:
    # "10:30:00.41234567" y "10:30:00.4123456-7" se tragaban un documento.
    # Medido en la ronda 24.
    hora = paste0(
      "(?<![0-9])(?:[01]?[0-9]|2[0-3]):[0-5][0-9]",
      "(?::[0-5][0-9](?:[.](?:[0-9]{1,7}|[0-9]{9})|,[0-9]{1,3})?)?",
      "(?![0-9]|-[0-9](?![0-9]))"
    ),
    # Un ISBN-13 tampoco: 978 o 979 y trece cifras con guiones o espacios.
    isbn = "(?<![0-9])97[89](?:[- ][0-9]+){3}[- ][0-9Xx](?![0-9])",
    # Y el ISBN-10, con sus cuatro grupos y diez cifras, unidos por el mismo
    # signo: guiones, espacios o puntos -con espacios o puntos se tapaba uno de
    # cada treinta-. Solo con su digito de control valido o precedido de "ISBN":
    # con cualquier diez cifras en cuatro grupos se borraba tambien un NIT
    # "900-123-456-7", y se publicaba. Medido en la ronda 24.
    isbn10 = paste0(
      "(?<![0-9])((?i:isbn)(?:-?1[03])?[ :#]{0,3})?",
      "([0-9]{1,5}([- .])[0-9]{1,7}\\3[0-9]{1,7}\\3[0-9Xx])(?![0-9])"
    ),
    # Una direccion IPv4 -cuatro octetos de 0 a 255, sin ceros a la izquierda-
    # no se borra: se reconoce como documento ENTERO y no como parcial. Borrarla
    # dejaba pasar el telefono "29.10.12.34" y la cedula "1.234.123.4", que
    # tienen esa forma. Medido en la ronda 23.
    ip = paste0("^(?:", octeto, "\\.){3}", octeto, "$"),
    # Y la IP con su mascara CIDR, "10.4.123.234/24": la barra une, el numero
    # ya no tenia forma de IP y se probaban sus tramos de octetos -una de cada
    # seis se tapaba-. La mascara se separa con una barra vertical, que no une,
    # y la IP queda sola: se prueba entera. Medido en la ronda 25.
    miles_largo = "^[0-9]{1,3}([.,'])[0-9]{3}(?:\\1[0-9]{3}){2,}$",
    cidr = paste0("(?<![0-9.])((?:", octeto, "\\.){3}", octeto,
                  ")/(3[0-2]|[12]?[0-9])(?![0-9]|[./][0-9])"),
    # La fecha CON HORA si puede ser un valor protegido, y se arma en el orden
    # en que se guarda -ano, mes, dia, horas- antes de borrar la fecha.
    # Entre la fecha y la hora, una T, uno a tres espacios, un guion bajo, un
    # guion o dos, un punto y coma, una coma, una barra, una barra vertical, una
    # arroba, un parentesis o un corchete;
    # entre horas, minutos y segundos, dos puntos, punto, guion o las letras h y
    # m. Y la forma compacta ISO, "20240627T212425". Con un solo espacio, la
    # fecha con hora se publicaba escrita con dos, con " - ", con ";", en un
    # nombre de archivo o como "21h24m25s". Medido en la ronda 24.
    fecha_hora = list(
      paste0("(?<![0-9])(", anio, ")[-/.]([0-9]{1,2})[-/.]([0-9]{1,2})", entre_fecha_hora,
             "([0-9]{1,2})[-:.hH]([0-9]{2})(?:[-:.mM]([0-9]{2}))?"),
      paste0("(?<![0-9])([0-9]{1,2})[-/.]([0-9]{1,2})[-/.](", anio, ")", entre_fecha_hora,
             "([0-9]{1,2})[-:.hH]([0-9]{2})(?:[-:.mM]([0-9]{2}))?"),
      paste0("(?<![0-9])(", anio, ")([0-9]{2})([0-9]{2})[Tt]",
             "([0-9]{2})([0-9]{2})([0-9]{2})?(?![0-9])"),
      # La fecha con separadores y la hora compacta, "2024-06-07T090405": la
      # mezcla de las dos formas ISO, que se publicaba. Medido en la ronda 25.
      paste0("(?<![0-9])(", anio, ")[-/.]([0-9]{1,2})[-/.]([0-9]{1,2})[Tt]",
             "([0-9]{2})([0-9]{2})([0-9]{2})?(?![0-9])")
    )
  )
})

# Concatenaciones de grupos contiguos con un largo entre `minimo` y `maximo`;
# con `desde`, solo las que empiezan en ese grupo.
.concatenaciones_grupos <- function(grupos, minimo, maximo, desde = NULL) {
  k <- length(grupos)
  if (!k) return(character())
  largos <- nchar(grupos, type = "bytes")
  salida <- character()
  for (i in if (is.null(desde)) seq_len(k) else desde) {
    acumulado <- 0L
    for (j in i:k) {
      acumulado <- acumulado + largos[[j]]
      if (acumulado > maximo) break
      if (acumulado >= minimo) {
        salida <- c(salida, paste0(grupos[i:j], collapse = ""))
      }
    }
  }
  salida
}

# Los grupos de un numero suelto, y cuales son de un compacto DECIMAL: uno cuyo
# ultimo signo es un punto o una coma y cuyo ultimo grupo no tiene tres cifras
# -"6723640.38", "-30.1078721"-. Un decimal no es un documento sin verificador.
#
# Y en un importe con miles y decimales -"4.351.256,92"- la parte decimal no se
# pega a la entera ni para el documento entero: "35125692" no esta escrito en
# ningun lado, y con un millon de cedulas coincidia con alguna. Con un solo signo
# -"2336.5544"- no se sabe si es un decimal o un telefono con punto, y se prueba.
.grupos_numero <- function(numero) {
  regla <- .REGLA_DIGITOS
  compactos <- regmatches(
    numero, gregexpr(regla$compacto, numero, perl = TRUE)
  )[[1L]]
  grupos <- character()
  decimal <- logical()
  fraccion <- logical()
  simples <- logical()
  todos_decimales <- TRUE
  for (compacto in compactos) {
    partes <- strsplit(compacto, regla$signo_compacto, perl = TRUE)[[1L]]
    signos <- regmatches(
      compacto, gregexpr(regla$signo_compacto, compacto, perl = TRUE)
    )[[1L]]
    k <- length(partes)
    largos <- nchar(partes, type = "bytes")
    ultimo <- if (length(signos)) signos[[length(signos)]] else ""
    # Es un decimal si el ultimo signo es un punto o una coma, el ultimo grupo no
    # tiene tres cifras, y lo de antes es un entero: un solo grupo, o un numero
    # de miles con OTRO signo -"4.123.456,7"-. Con solo mirar el ultimo signo,
    # "099.12.34.56", "01.23.45.67.89" o "CI 4.123.456.7" eran importes y se
    # publicaban. Medido en la ronda 22: el mismo signo no separa miles y
    # decimales a la vez.
    es_decimal <- ultimo %in% c(".", ",") && largos[[k]] != 3L && (
      k == 2L || (
        all(signos[-length(signos)] == signos[[1L]]) && signos[[1L]] != ultimo &&
          signos[[1L]] %in% c(".", ",", "'", "_") &&
          largos[[1L]] <= 3L && all(largos[2:(k - 1L)] == 3L)
      )
    )
    # Y la parte decimal no se pega al resto para el documento entero cuando es
    # inequivoca: la de un importe con miles, o la de un numero de hasta tres
    # cifras enteras y cuatro o mas decimales -una coordenada "-33.340517", un
    # p-valor-, que se tapaba si con la entera formaba una cedula.
    # Un entero con un cero adelante -"099.123456"- no es la parte entera de
    # un decimal: es un telefono. Medido en la ronda 23.
    inequivoca <- es_decimal && (k > 2L || (
      largos[[1L]] <= 3L && largos[[k]] >= 4L &&
        !(largos[[1L]] >= 2L && startsWith(partes[[1L]], "0"))
    ))
    grupos <- c(grupos, partes)
    decimal <- c(decimal, rep(es_decimal, k))
    fraccion <- c(fraccion, seq_len(k) == k & inequivoca)
    simples <- c(simples, rep(k == 2L && inequivoca, k))
    todos_decimales <- todos_decimales && es_decimal
  }
  # Y la de un decimal suelto solo es inequivoca si TODO el numero son
  # decimales -una coordenada, un par lat lon-: "(555) 123.4567" y
  # "(02) 901.1234" son telefonos, con un prefijo que no es decimal.
  if (!todos_decimales) fraccion[simples] <- FALSE
  list(grupos = grupos, decimal = decimal, fraccion = fraccion)
}

# Concatenaciones para el documento entero: por tramos sin parte decimal.
.concatenaciones_completas <- function(partes, minimo, maximo) {
  if (!length(partes$grupos)) return(character())
  tramos <- rle(!partes$fraccion)
  fin <- cumsum(tramos$lengths)
  inicio <- fin - tramos$lengths + 1L
  salida <- character()
  for (t in which(tramos$values)) {
    salida <- c(salida, .concatenaciones_grupos(
      partes$grupos[inicio[[t]]:fin[[t]]], minimo = minimo, maximo = maximo
    ))
  }
  salida
}

.concatenaciones_parciales <- function(partes, maximo, desde_inicio = FALSE) {
  if (!length(partes$grupos)) return(character())
  tramos <- rle(!partes$decimal)
  fin <- cumsum(tramos$lengths)
  inicio <- fin - tramos$lengths + 1L
  salida <- character()
  for (t in which(tramos$values)) {
    if (desde_inicio && inicio[[t]] != 1L) next
    salida <- c(salida, .concatenaciones_grupos(
      partes$grupos[inicio[[t]]:fin[[t]]],
      minimo = .MIN_LARGO_VALOR_IDENTIFICANTE + 1L, maximo = maximo,
      desde = if (desde_inicio) 1L
    ))
  }
  salida
}

.sin_ceros_iniciales <- function(x) sub("^0+", "", x)

# Los conjuntos contra los que se compara, armados una vez por llamada. Un cero a
# la izquierda no cambia el numero: "041234567" es el documento 41234567, como lo
# deja una exportacion de ancho fijo. Medido en una refutacion.
.documentos_digitos <- function(digitos) {
  digitos <- unique(digitos[!is.na(digitos) & nzchar(digitos)])
  largo <- nchar(digitos, type = "bytes")
  parcial <- largo - 1L >= .MIN_LARGO_VALOR_IDENTIFICANTE + 1L
  completos <- .sin_ceros_iniciales(digitos)
  prefijos <- .sin_ceros_iniciales(substr(digitos, 1L, largo - 1L)[parcial])
  list(
    completos = unique(completos[
      nchar(completos, type = "bytes") >= .MIN_LARGO_VALOR_IDENTIFICANTE
    ]),
    prefijos = unique(prefijos[
      nchar(prefijos, type = "bytes") >= .MIN_LARGO_VALOR_IDENTIFICANTE + 1L
    ]),
    sufijos = unique(substr(digitos, 2L, largo)[parcial]),
    # Con margen para los ceros iniciales, que no cuentan.
    maximo = if (length(largo)) max(largo) + 4L else 0L
  )
}

# Los guiones, espacios e invisibles de Unicode cortaban el numero: el telefono
# "2901 1234" con un guion largo, un espacio de cifra, un guion suave o un
# espacio de ancho cero entre los grupos se publicaba. Medido en la ronda 22. Se
# llevan a los de ASCII antes de buscar.
.normalizar_signos_digitos <- function(texto) {
  tryCatch({
    # Los asteriscos de Unicode son el comodin, como el de ASCII. Medido en la
    # ronda 24: el documento sin su primer digito tras el asterisco de ancho
    # completo o el operador asterisco se publicaba. Y en la ronda 25, con los
    # otros treinta y ocho que Unicode llama asterisco: el de ocho rayos del
    # emoji, el de centro abierto, los de rayos y globos, el encerrado en un
    # circulo o un cuadrado, el eslavo y los combinantes. No los dos signos
    # cuneiformes, que son letras, ni la etiqueta, que no se ve. Va PRIMERO: los
    # combinantes son marcas, y abajo se borran.
    texto <- gsub(
      paste0("[\uff0a\ufe61\u2217\u204e\u066d\u0359\u20f0\u2051\u229b",
             "\u2722-\u2725\u2731-\u2733\u273a-\u273d\u2743\u2749",
             "\u274a\u274b\u29c6\u2a6e\ua673\U0001F7AF-\U0001F7BF]"),
      "*", texto, perl = TRUE
    )
    # Todo signo ASCII de ancho completo, como su forma ASCII: el pliegue lleva
    # las letras y las cifras anchas, y aca habia una lista de signos. La fecha
    # con hora protegida se publicaba escrita con la arroba, el punto y coma o
    # la barra vertical anchos (U+FF20, U+FF1B, U+FF5C) mientras sus hermanas
    # -la coma, la barra y la T anchas- se tapaban. Medido en la ronda 26. La
    # misma lectura que la del correo en palabras: `.ancho_y_espacios_ascii()`.
    texto <- .ancho_y_espacios_ascii(texto)
    # Un control C1 en un texto es casi siempre un CSV de Windows -CP1252- leido
    # como `latin1`: la raya 0x96 queda como U+0096 y cortaba el numero. Se lee
    # como lo que es en CP1252: la raya y la raya larga, las comillas simples, la
    # coma baja y la vineta. Medido en la ronda 24.
    texto <- chartr("\u0082\u0091\u0092\u0095\u0096\u0097",
                    "\u201a\u2018\u2019\u2022\u2013\u2014", texto)
    texto <- gsub("\\p{Cf}", "", texto, perl = TRUE)
    # Y los selectores de variante, que tampoco se ven; las marcas que no ocupan
    # lugar -combinantes y envolventes- y los demas ignorables de Unicode, tambien
    # los no asignados. Medido en la ronda 24: U+17B4, U+180B y U+20DD entre los
    # grupos cortaban el numero.
    texto <- gsub(
      paste0("[\uFE00-\uFE0F\U000E0100-\U000E01EF\\p{Mn}\\p{Me}\u034f",
             "\u17b4\u17b5\u180b-\u180f\u2065\ufff0-\ufff8\U000E0000-\U000E0FFF]"),
      "", texto, perl = TRUE
    )
    # Y los parecidos del guion: el menos modificador, el guion vineta, el menos
    # grueso, la linea de caja y la marca de prolongacion katakana. Medido en la
    # ronda 24.
    texto <- gsub("[\\p{Pd}\u2212\u02d7\u2043\u2796\u2500\u30fc]", "-", texto,
                  perl = TRUE)
    # Todo separador de Unicode -tambien el de linea y el de parrafo-, el salto
    # de linea de C1 y los rellenos que se ven como un espacio: el hangul, el
    # braille en blanco. Medido en la ronda 23. Y los demas controles, que son
    # invisibles: tambien unen, como un espacio. Medido en la ronda 24.
    texto <- gsub(
      "[\\p{Z}\\p{Cc}\u3164\u115f\u1160\uffa0\u2800]", " ",
      texto, perl = TRUE
    )
    # Y los que se parecen a un punto, una barra, una coma o un punto medio: el
    # punto de guia, la barra de fraccion y la de division, la coma y el
    # separador de miles arabes, el punto medio katakana y el operador punto.
    # La ronda 24 agrego el acento agudo que el teclado en espanol pone por
    # apostrofo, las comillas simples, el ano teleia -el mismo caracter que el
    # punto medio-, la vineta, la coma ideografica de medio ancho y la chica, la
    # cedilla, el punto arabe y el armenio, y las diagonales. Sin guiones: `chartr()` los
    # lee como rangos.
    texto <- chartr(
      paste0("\uff0e\uff0c\uff0f\uff1a\u3002\u2024\u2044\u2215\u060c",
             "\u066c\u30fb\uff65\u2219\u22c5\u2019\u02bc\u2027\ufe52",
             "\uff61\ufe50\u3001\u201a\u29f8\uff0b\u2e31",
             "\u00b4\u2018\u201b\uff07\u0387\u2022\u2e33\u02d9\u16eb",
             "\uff64\ufe51\u00b8\u06d4\u0589\u066b\u2571\u27cb"),
      paste0(".,/:..//,,\u00b7\u00b7\u00b7\u00b7''...,,,/+\u00b7",
             "''''\u00b7\u00b7\u00b7\u00b7\u00b7",
             ",,,...//"),
      texto
    )
    texto
  }, error = function(e) NA_character_)
}

# La fecha con hora como documento entero, en el orden en que se guarda: el
# valor protegido "2024-06-27 21:24:25" se publicaba escrito
# "2024-06-27T21:24:25" o "27/06/2024 21:24:25", porque la fecha y la hora se
# borran antes de buscar. Medido en la ronda 22. Una fecha SOLA no es un dato
# -`.valores_identificantes()` no la lleva-; con hora, si.
.fechas_hora_digitos <- function(texto) {
  salida <- character()
  for (indice in seq_along(.REGLA_DIGITOS$fecha_hora)) {
    patron <- .REGLA_DIGITOS$fecha_hora[[indice]]
    piezas <- tryCatch(
      regmatches(texto, gregexpr(patron, texto, perl = TRUE))[[1L]],
      error = function(e) character()
    )
    for (pieza in piezas) {
      partes <- regmatches(pieza, regexec(patron, pieza, perl = TRUE))[[1L]][-1L]
      numeros <- suppressWarnings(as.integer(partes))
      # Dia y mes delante del ano se prueban en los dos ordenes: "27/06/2024" y
      # "06/27/2024" son la misma fecha escrita de dos maneras.
      ordenes <- if (indice == 2L) {
        list(numeros[c(3L, 2L, 1L, 4L, 5L, 6L)], numeros[c(3L, 1L, 2L, 4L, 5L, 6L)])
      } else list(numeros)
      for (orden in ordenes) {
        if (anyNA(orden[1:5])) next
        sin_segundos <- sprintf("%04d%02d%02d%02d%02d", orden[[1L]], orden[[2L]],
                                orden[[3L]], orden[[4L]], orden[[5L]])
        salida <- c(salida, sin_segundos, paste0(sin_segundos, sprintf(
          "%02d", if (is.na(orden[[6L]])) 0L else orden[[6L]]
        )))
      }
    }
  }
  salida
}

# Si un tramo que casa con la regla del ISBN-10 es un ISBN: diez cifras, y el
# digito de control valido o la etiqueta "ISBN" delante. El control es la suma
# de cada cifra por su peso, de diez a uno, multiplo de once; la equis vale diez
# y solo al final.
.es_isbn10 <- function(pieza) {
  partes <- regmatches(pieza, regexec(.REGLA_DIGITOS$isbn10, pieza, perl = TRUE))[[1L]]
  if (length(partes) < 3L) return(FALSE)
  cifras <- gsub("[^0-9Xx]", "", partes[[3L]])
  if (nchar(cifras) != 10L || grepl("[Xx].", cifras)) return(FALSE)
  if (nzchar(partes[[2L]])) return(TRUE)
  valores <- utf8ToInt(cifras) - 48L
  valores[valores > 9L] <- 10L
  sum(valores * 10:1) %% 11L == 0L
}

# Los numeros de `texto` -ya plegado- que se comparan, en sus tres papeles.
.candidatos_digitos <- function(texto, maximo) {
  vacio <- list(completos = character(), prefijos = character(),
                sufijos = character())
  if (is.na(texto) || !grepl("[0-9]", texto, useBytes = TRUE)) return(vacio)
  regla <- .REGLA_DIGITOS
  texto <- .normalizar_signos_digitos(texto)
  if (is.na(texto)) return(vacio)
  fechas_hora <- .fechas_hora_digitos(texto)
  texto <- tryCatch({
    texto <- gsub(regla$fecha, " ", texto, perl = TRUE)
    texto <- gsub(regla$hora, " ", texto, perl = TRUE)
    # La mascara queda entre dos barras verticales, que no unen: con una sola,
    # el rango de redes "10.4.1.0/24-10.4.9.0/24" pegaba la mascara "24" a la IP
    # siguiente por el guion, y uno de cada cuatro se tapaba por los tramos de
    # ese numero. Medido en la ronda 26.
    texto <- gsub(regla$cidr, "\\1|\\2|", texto, perl = TRUE)
    isbn <- regmatches(texto, gregexpr(regla$isbn, texto, perl = TRUE))[[1L]]
    isbn <- isbn[nchar(gsub("[^0-9Xx]", "", isbn)) == 13L]
    for (pieza in isbn) texto <- sub(pieza, " ", texto, fixed = TRUE)
    isbn10 <- regmatches(texto, gregexpr(regla$isbn10, texto, perl = TRUE))[[1L]]
    isbn10 <- isbn10[vapply(isbn10, .es_isbn10, logical(1L))]
    for (pieza in isbn10) texto <- sub(pieza, " ", texto, fixed = TRUE)
    # Dos decimales pegados por una coma -"40.446984,38.209835", un par de
    # coordenadas- son dos numeros, no uno.
    gsub("([0-9][.][0-9]+),(?=[0-9]+[.][0-9])", "\\1, ", texto, perl = TRUE)
  }, error = function(e) NA_character_)
  if (is.na(texto)) return(vacio)
  minimo <- .MIN_LARGO_VALOR_IDENTIFICANTE
  # Un numero redondo -`1.000.000`- es un conteo: que coincida con un documento
  # SIN su verificador no lo vuelve un dato. El documento entero, aunque sea
  # redondo -la cedula 5.000.000-0 tiene verificador valido-, si.
  redondo <- function(v) v[!grepl("^[1-9]0+$", v, perl = TRUE)]
  completos <- fechas_hora
  prefijos <- character()
  sufijos <- character()
  numeros <- regmatches(texto, gregexpr(regla$numero, texto, perl = TRUE))[[1L]]
  for (numero in numeros) {
    partes <- .grupos_numero(numero)
    # Con forma de IP, solo como documento entero, y entera: probar cada tramo
    # de octetos tapaba una de cada cincuenta direcciones -"218.235.90.247" por
    # el tramo "21823590"-. Medido en la ronda 24.
    # Lo mismo un importe con miles de cuatro grupos o mas -"3.919.024.166",
    # "$ 12.345.678.901"-: el tramo de sus tres primeros grupos tiene la forma
    # del documento sin su verificador, y uno de cada seis se tapaba. Medido en
    # la ronda 25. Entero si: la cedula colombiana "1.023.456.789" se escribe
    # asi. Sin el espacio como separador de miles: "598 099 123 456" es un
    # celular con su prefijo, y su tramo es el celular.
    #
    # Y del importe de cuatro grupos, tambien la COLA sin el primero, entera: el
    # celular con su prefijo de pais agrupado de a tres con puntos, comas o
    # apostrofos -"598.099.123.456"- se probaba solo entero y se publicaba,
    # mientras con espacios o guiones se tapaba. Medido en la ronda 26. La cola
    # tiene nueve cifras y solo coincide con un documento cuando empieza con un
    # cero, como el celular; el costo en importes esta en
    # `data-raw/medir_tapado_de_mas.R`.
    if (grepl(regla$ip, numero, perl = TRUE) ||
        grepl(regla$miles_largo, numero, perl = TRUE)) {
      enteros <- paste0(partes$grupos, collapse = "")
      if (grepl(regla$miles_largo, numero, perl = TRUE)) {
        enteros <- c(enteros, paste0(partes$grupos[-1L], collapse = ""))
      }
      enteros <- .sin_ceros_iniciales(enteros)
      largos <- nchar(enteros, type = "bytes")
      completos <- c(completos, enteros[largos >= minimo & largos <= maximo])
      next
    }
    completos <- c(completos, .sin_ceros_iniciales(
      .concatenaciones_completas(partes, minimo, maximo)
    ))
    prefijos <- c(prefijos, redondo(.sin_ceros_iniciales(
      .concatenaciones_parciales(partes, maximo)
    )))
  }
  con_comodin <- regmatches(
    texto, gregexpr(regla$comodin, texto, perl = TRUE)
  )[[1L]]
  for (tramo in con_comodin) {
    numero <- regmatches(tramo, regexpr(regla$numero, tramo, perl = TRUE))
    sufijos <- c(sufijos, redondo(.concatenaciones_parciales(
      .grupos_numero(numero), maximo, desde_inicio = TRUE
    )))
  }
  list(completos = unique(completos), prefijos = unique(prefijos),
       sufijos = unique(sufijos))
}

# Para cada texto, si alguno de sus numeros es un documento. En lote: `%in%`
# arma la tabla de documentos -pueden ser millones- una vez por llamada, y por
# celda costaba una tabla por celda.
.golpes_digitos <- function(textos, documentos) {
  if (!length(textos)) return(logical())
  candidatos <- lapply(textos, .candidatos_digitos, maximo = documentos$maximo)
  en <- function(campo, conjunto) {
    valores <- lapply(candidatos, `[[`, campo)
    dueno <- rep(seq_along(valores), lengths(valores))
    golpe <- logical(length(valores))
    golpe[unique(dueno[unlist(valores, use.names = FALSE) %in% conjunto])] <- TRUE
    golpe
  }
  en("completos", documentos$completos) | en("prefijos", documentos$prefijos) |
    en("sufijos", documentos$sufijos)
}

# Los tramos de un texto encerrados entre dos apariciones de `delimitador` -una
# comilla doble, una invertida-, en bytes. Las comillas escapadas con barra no
# cuentan. Si el delimitador aparece una cantidad IMPAR de veces, uno esta suelto
# -una comilla en el NOMBRE de una columna, como deja `read.csv(check.names =
# FALSE)`- y corre todo el apareo: el valor citado despues quedaba afuera de toda
# cita. Medido en una refutacion. Entonces se prueban tambien los apareos que
# saltean cada delimitador, y se devuelven todos los tramos: quien los usa
# decide cuales tapar.
.tramos_entre_delimitadores <- function(texto, delimitador = "\"") {
  vacio <- matrix(integer(), ncol = 2L, dimnames = list(NULL, c("inicio", "fin")))
  if (is.na(texto)) return(vacio)
  bytes <- charToRaw(texto)
  objetivo <- charToRaw(delimitador)
  barra <- as.raw(0x5c)
  posiciones <- integer()
  i <- 1L
  n <- length(bytes)
  escapa <- identical(delimitador, "\"")
  while (i <= n) {
    if (escapa && bytes[[i]] == barra) {
      i <- i + 2L
      next
    }
    if (bytes[[i]] == objetivo) posiciones <- c(posiciones, i)
    i <- i + 1L
  }
  aparear <- function(p) {
    if (length(p) < 2L) return(vacio)
    pares <- seq_len(length(p) %/% 2L)
    cbind(inicio = p[2L * pares - 1L], fin = p[2L * pares])
  }
  tramos <- aparear(posiciones)
  if (length(posiciones) %% 2L == 1L) {
    for (s in seq_along(posiciones)) {
      tramos <- rbind(tramos, aparear(posiciones[-s]))
    }
  }
  unique(tramos)
}

.texto_de_tramo <- function(texto, inicio, fin) {
  bytes <- charToRaw(texto)[inicio:fin]
  salida <- rawToChar(bytes)
  Encoding(salida) <- Encoding(texto)
  salida
}

# Reemplaza tramos de bytes de `texto`, cada uno por su reemplazo. Los que se
# solapan -los apareos alternativos de una comilla suelta- se unen, y la union se
# reemplaza por `union`.
.reemplazar_tramos_bytes <- function(texto, tramos, reemplazos,
                                     union = "\"[valor protegido]\"") {
  if (!NROW(tramos)) return(texto)
  # Un tramo que CONTIENE a otro que tambien se tapa sobra: el valor esta en el
  # chico. Pasa con los apareos alternativos de una comilla suelta, donde una
  # cita larga abarca el nombre de la columna y el nivel citado; tapar la larga
  # borraba el codigo de la sugerencia.
  contiene_otro <- vapply(seq_len(NROW(tramos)), function(k) {
    any(tramos[-k, "inicio"] >= tramos[k, "inicio"] &
          tramos[-k, "fin"] <= tramos[k, "fin"] &
          (tramos[-k, "inicio"] != tramos[k, "inicio"] |
             tramos[-k, "fin"] != tramos[k, "fin"]))
  }, logical(1L))
  tramos <- tramos[!contiene_otro, , drop = FALSE]
  reemplazos <- rep_len(reemplazos, length(contiene_otro))[!contiene_otro]
  orden <- order(tramos[, "inicio"], -tramos[, "fin"])
  tramos <- tramos[orden, , drop = FALSE]
  reemplazos <- rep_len(reemplazos, NROW(tramos))[orden]
  grupos <- list(list(inicio = tramos[1L, "inicio"], fin = tramos[1L, "fin"],
                      reemplazo = reemplazos[[1L]]))
  for (k in seq_len(NROW(tramos))[-1L]) {
    ultimo <- length(grupos)
    if (tramos[k, "inicio"] <= grupos[[ultimo]]$fin) {
      if (tramos[k, "fin"] > grupos[[ultimo]]$fin ||
          !identical(reemplazos[[k]], grupos[[ultimo]]$reemplazo)) {
        grupos[[ultimo]]$reemplazo <- union
      }
      grupos[[ultimo]]$fin <- max(grupos[[ultimo]]$fin, tramos[k, "fin"])
    } else {
      grupos[[ultimo + 1L]] <- list(inicio = tramos[k, "inicio"],
                                    fin = tramos[k, "fin"],
                                    reemplazo = reemplazos[[k]])
    }
  }
  bytes <- charToRaw(texto)
  marca <- Encoding(texto)
  for (g in rev(grupos)) {
    antes <- if (g$inicio > 1L) bytes[seq_len(g$inicio - 1L)]
    despues <- if (g$fin < length(bytes)) bytes[(g$fin + 1L):length(bytes)]
    bytes <- c(antes, charToRaw(g$reemplazo), despues)
  }
  salida <- rawToChar(bytes)
  Encoding(salida) <- marca
  salida
}
