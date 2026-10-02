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

.REGLA_DIGITOS <- local({
  anio <- "(?:1[89]|2[01])[0-9]{2}"
  octeto <- "(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])"
  espacio <- "[ \u00a0\u2009\u202f]"
  # La coma une solo sin espacio despues: "4,123,456" es un numero y
  # "67, 20, 855" es una lista.
  # Con espacios alrededor solo unen el guion y el punto -"2600 - 1122",
  # "4 . 123 . 456-7"- y el parentesis de un prefijo -"(2) 487 1234"-; los demas
  # signos, pegados. Con cualquiera, "2983 4272 / 2988 2968" eran un solo numero
  # y su tramo del medio coincidia con un telefono. Medido en la ronda 22.
  separador <- paste0(
    "(?:", espacio, "{1,2}|", espacio, "?[-.]{1,2}", espacio, "?|",
    "[-./'_:|+\u00b7()\\[\\]]{1,2}|[)\\]]", espacio, "|", espacio, "[(\\[]|",
    ",(?!", espacio, "))"
  )
  signo_compacto <- "[-./,'_:|+\u00b7]"
  numero <- paste0("[0-9]+(?:", separador, "[0-9]+)*")
  list(
    numero = numero,
    compacto = paste0("[0-9]+(?:", signo_compacto, "[0-9]+)*"),
    signo_compacto = signo_compacto,
    comodin = paste0(
      "(?<![\\p{L}\\p{N}])[*xX#?\u2022]+(?:", separador, ")?(", numero, ")"
    ),
    # Una fecha de calendario no es un documento: partida en tramos,
    # `2003-05-17` da `20030517`, que con un millon de cedulas es la de
    # alguien. Con un ANO plausible, 1800 a 2199: con cualquiera, el telefono
    # "2901-12-12" se borraba como fecha y se publicaba. Medido en la ronda 22.
    fecha = paste0(
      "(?<![0-9])(?:", anio, "[-/.](?:0?[1-9]|1[0-2])[-/.](?:0?[1-9]|[12][0-9]|3[01])|",
      "(?:0?[1-9]|[12][0-9]|3[01])[-/.](?:0?[1-9]|1[0-2])[-/.]", anio, ")(?![0-9])"
    ),
    # Y la hora, por lo mismo -`12:34:56`-, con su fraccion de segundo: sin ella,
    # la fraccion de `12:26:58.5412576` quedaba como un numero suelto de siete
    # cifras, y una de cada ocho coincidia con una cedula.
    hora = paste0(
      "(?<![0-9])(?:[01]?[0-9]|2[0-3]):[0-5][0-9](?::[0-5][0-9](?:[.,][0-9]+)?)?",
      "(?![0-9])"
    ),
    # Un ISBN-13 tampoco: 978 o 979 y trece cifras con guiones o espacios.
    isbn = "(?<![0-9])97[89](?:[- ][0-9]+){3}[- ][0-9Xx](?![0-9])",
    # Una direccion IPv4 tampoco: cuatro octetos de 0 a 255, sin ceros a la
    # izquierda -un celular "099.12.34.56" no lo es-.
    ip = paste0(
      "(?<![0-9.])(?:", octeto, "\\.){3}", octeto, "(?![0-9.])"
    ),
    # La fecha CON HORA si puede ser un valor protegido, y se arma en el orden
    # en que se guarda -ano, mes, dia, horas- antes de borrar la fecha.
    fecha_hora = list(
      paste0("(?<![0-9])(", anio, ")[-/.]([0-9]{1,2})[-/.]([0-9]{1,2})[Tt ]",
             "([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?"),
      paste0("(?<![0-9])([0-9]{1,2})[-/.]([0-9]{1,2})[-/.](", anio, ")[Tt ]",
             "([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?")
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
    inequivoca <- es_decimal &&
      (k > 2L || (largos[[1L]] <= 3L && largos[[k]] >= 4L))
    grupos <- c(grupos, partes)
    decimal <- c(decimal, rep(es_decimal, k))
    fraccion <- c(fraccion, seq_len(k) == k & inequivoca)
  }
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
    texto <- gsub("\\p{Cf}", "", texto, perl = TRUE)
    texto <- gsub("[\\p{Pd}\u2212]", "-", texto, perl = TRUE)
    texto <- gsub("[\\p{Zs}\t\n\r\f\v]", " ", texto, perl = TRUE)
    # Y los que se parecen a un punto, una barra, una coma o un punto medio: el
    # punto de guia, la barra de fraccion y la de division, la coma y el
    # separador de miles arabes, el punto medio katakana y el operador punto.
    chartr(
      paste0("\uff0e\uff0c\uff0f\uff1a\u3002\u2024\u2044\u2215\u060c",
             "\u066c\u30fb\uff65\u2219\u22c5"),
      ".,/:..//,,\u00b7\u00b7\u00b7\u00b7",
      texto
    )
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
      if (indice == 2L) numeros <- numeros[c(3L, 2L, 1L, 4L, 5L, 6L)]
      if (anyNA(numeros[1:5])) next
      sin_segundos <- sprintf("%04d%02d%02d%02d%02d", numeros[[1L]], numeros[[2L]],
                              numeros[[3L]], numeros[[4L]], numeros[[5L]])
      salida <- c(salida, sin_segundos, paste0(sin_segundos, sprintf(
        "%02d", if (is.na(numeros[[6L]])) 0L else numeros[[6L]]
      )))
    }
  }
  salida
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
    texto <- gsub(regla$ip, " ", texto, perl = TRUE)
    isbn <- regmatches(texto, gregexpr(regla$isbn, texto, perl = TRUE))[[1L]]
    isbn <- isbn[nchar(gsub("[^0-9Xx]", "", isbn)) == 13L]
    for (pieza in isbn) texto <- sub(pieza, " ", texto, fixed = TRUE)
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
