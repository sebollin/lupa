# Hallazgos de exactitud que el perfil no publicaba: texto cortado por un tope de
# campo, valores apilados en multiplos redondos, una serie acumulada que
# retrocede y una columna que mezcla dos escalas. Son patrones medidos sobre la
# columna, no errores demostrados: por eso la severidad es `sospechoso`.
#
# Cada detector devuelve solo lo que midio -indices, conteos y las cifras que
# sostienen la afirmacion-. El texto de los hallazgos se arma en un unico lugar
# para que la evidencia y la traza no digan cosas distintas.


# Largos de campo que aparecen como tope en motores y formularios. No son una
# lista cerrada: fuera de ella, el pico tiene que ser anomalo contra sus vecinos.
.topes_de_campo <- c(
  20L, 30L, 32L, 40L, 50L, 64L, 100L, 128L, 200L, 250L, 255L, 256L,
  500L, 1000L, 4000L
)

# Texto truncado por un tope: el sintoma es que ningun valor pasa del largo del
# pico, que el pico acumula valores cortados a mitad de palabra, que el pico es
# una PARTE de la columna -alrededor hay valores mas cortos- y que la columna no
# tiene un formato dominante. Un codigo de ancho fijo, una fecha o una plantilla
# con un numero variable viven en un solo patron y no son un campo truncado. En
# los vocabularios discretos -nombres armados de un repertorio de palabras- el
# maximo natural tambien cae en un tope tipico: ahi la ultima palabra del pico
# tiene que ser una palabra cortada, que no aparezca entera en otro valor.
.UMBRAL_PATRON_TEXTO_TRUNCADO <- 0.5

.detectar_texto_truncado <- function(valores, patrones = NULL,
                                     distinguir_mayusculas = TRUE,
                                     expandir = FALSE) {
  if (is.null(valores) || !is.character(valores)) return(NULL)
  posiciones <- which(!is.na(valores) & nzchar(valores))
  n <- length(posiciones)
  if (n < 30L) return(NULL)
  texto <- valores[posiciones]
  largos <- tryCatch(
    nchar(texto, type = "chars"),
    error = function(e) nchar(texto, type = "bytes")
  )
  if (length(largos) != n || anyNA(largos)) return(NULL)
  # Un tope de campo no deja valores mas largos que el: el pico esta en el
  # largo maximo. Un texto con valores mas largos alrededor no esta cortado.
  tope <- max(largos)
  if (tope < 8L) return(NULL)
  url <- .detectar_url_truncada(texto, largos, tope, posiciones)
  if (!is.null(url)) return(url)
  n_tope <- sum(largos == tope)
  if (n_tope < 3L) return(NULL)
  proporcion <- n_tope / n
  # Alrededor del pico tiene que haber valores mas cortos; si la columna vive
  # en un solo largo no hay un tope que corte nada. Un ancho fijo -una fecha,
  # una matricula, un telefono- cae aca aunque la mayoria de la columna este en
  # el maximo.
  if (sum(largos < tope) < 3L) return(NULL)
  # Texto libre: muchos valores distintos, no un punado de categorias que
  # casualmente comparten largo.
  if (length(unique(texto)) < max(10L, ceiling(0.25 * n))) return(NULL)
  # Los valores del pico parecen cortados a mitad de palabra: texto con
  # espacios que termina en letra o digito.
  cortados <- texto[largos == tope]
  if (sum(grepl(" ", cortados, fixed = TRUE, useBytes = TRUE)) / n_tope < 0.25) {
    return(NULL)
  }
  # Un corte puede caer justo en un espacio; el ultimo caracter visible sigue
  # siendo una letra o un digito, y el espacio final tambien es parte del corte.
  visibles <- sub("[[:space:]]+$", "", cortados, perl = TRUE)
  ultimo <- substr(
    visibles, nchar(visibles, type = "chars"), nchar(visibles, type = "chars")
  )
  if (mean(grepl("[A-Za-z0-9]", ultimo, perl = TRUE, useBytes = TRUE)) < 0.6) {
    return(NULL)
  }
  if (length(unique(cortados)) < 3L) return(NULL)
  # Un formato dominante en los valores del pico es una fecha, una matricula o
  # una plantilla, no texto libre cortado por un tope de campo.
  if (!is.null(patrones) && nrow(patrones)) {
    patrones_pico <- unique(.generalizar_a_patron(
      .textos_para_plegar(cortados), distinguir_mayusculas, expandir
    ))
    proporcion_patron <- patrones$proporcion[match(
      patrones_pico, as.character(patrones$patron)
    )]
    if (any(proporcion_patron >= .UMBRAL_PATRON_TEXTO_TRUNCADO, na.rm = TRUE)) {
      return(NULL)
    }
  }
  # En los topes tipicos con pocos valores, la ultima palabra del pico no puede
  # ser una palabra entera que ya este en otro valor MAS CORTO de la columna:
  # `Rodrigu` es un apellido cortado, pero un nombre que cae justo en el tope
  # termina en una palabra completa. Se mira solo lo mas corto que el pico: la
  # misma palabra a medias repetida en otro valor truncado no es una palabra de
  # la columna. Con muchos valores en el pico el corte ya se ve en el volumen.
  pocos_en_el_pico <- n_tope <= max(10L, ceiling(0.05 * n))
  if (tope %in% .topes_de_campo && pocos_en_el_pico) {
    cortados_distintos <- unique(.textos_para_plegar(cortados))
    # El vocabulario discreto tiene pocas palabras por valor; un comentario
    # libre cortado en el tope tiene muchas y el corte puede caer justo en un
    # espacio, con una ultima palabra entera que no descarta nada.
    palabras_pico <- lengths(
      strsplit(cortados_distintos, "[[:space:]]+", perl = TRUE)
    )
    if (max(palabras_pico) <= 8L) {
      distintas <- unique(.textos_para_plegar(texto[largos < tope]))
      # Un espacio final en el tope es el corte mismo: la palabra anterior
      # quedo entera, pero el valor no. Se guarda esa marca aparte.
      sin_espacio <- sub("[[:space:]]+$", "", cortados_distintos, perl = TRUE)
      en_el_espacio <- nchar(cortados_distintos, type = "chars") >
        nchar(sin_espacio, type = "chars")
      ultimas <- sub("^.*[^[:alnum:]]", "", sin_espacio, perl = TRUE)
      con_palabra <- nzchar(ultimas)
      if (any(en_el_espacio | con_palabra)) {
        unicas <- unique(ultimas[con_palabra])
        aparece <- stats::setNames(rep(FALSE, length(unicas)), unicas)
        # Una palabra de una o dos letras -"de", "la", "y"- no se busca como
        # palabra entera del vocabulario: aparece en casi cualquier texto y no
        # alcanza para descartar el corte. No se mira, queda como cortada.
        largas <- unicas[nchar(unicas, type = "chars") > 2L]
        if (length(largas) && length(largas) <= 20L) {
          for (palabra in largas) {
            patron <- paste0(
              "(^|[^[:alnum:]])", palabra, "([^[:alnum:]]|$)"
            )
            aparece[[palabra]] <-
              sum(grepl(patron, distintas, perl = TRUE)) > 0L
          }
        } else if (length(largas)) {
          # Con muchas palabras cortadas distintas, una tabla de tokens evita
          # un recorrido de la columna por palabra. Un token que aparece en
          # algun valor mas corto ya esta entero en la columna.
          tokens <- unlist(
            strsplit(distintas, "[^[:alnum:]]+", perl = TRUE), use.names = FALSE
          )
          cuenta <- table(tokens)
          presentes <- as.integer(cuenta[largas]) > 0L
          presentes[is.na(presentes)] <- FALSE
          aparece[largas] <- presentes
        }
        no_aparece <- en_el_espacio
        no_aparece[con_palabra] <- !aparece[ultimas[con_palabra]]
        evaluadas <- en_el_espacio | con_palabra
        # Alcanza con que una palabra muestre el corte: en un tope con pocos
        # valores, los cortes que caen en un espacio dejan una palabra entera
        # y no pueden borrar la evidencia de los que caen a mitad.
        if (!any(no_aparece[evaluadas])) return(NULL)
      }
    }
  }
  n_anterior <- sum(largos == tope - 1L)
  n_siguiente <- sum(largos == tope + 1L)
  if (!tope %in% .topes_de_campo) {
    # Fuera de los topes conocidos, el pico tiene que ser anomalo contra los
    # largos inmediatos y ocupar una parte visible de la columna.
    vecinos <- max(n_anterior, n_siguiente, 1L)
    if (n_tope < 3 * vecinos || proporcion < max(3 / n, 0.01)) return(NULL)
  }
  list(
    indices = posiciones[largos == tope],
    n_evaluados = n,
    n_afectados = n_tope,
    largo = tope,
    proporcion = proporcion,
    n_anterior = n_anterior,
    n_siguiente = n_siguiente,
    variante = "palabras"
  )
}

# Topes que una URL alcanza: los de campo desde 250 y los dos largos que fijan
# los navegadores y los servidores web. Por debajo, 100 o 200 caracteres son
# largos que una URL comun alcanza sin que nada la corte.
.topes_de_url <- c(.topes_de_campo[.topes_de_campo >= 250L], 1024L, 2048L)

# URL cortada por un tope: un valor sin espacios no tiene palabras que cortar, asi
# que la evidencia es otra. Todos los valores del tope empiezan con un esquema
# (`https://`), hay al menos tres DISTINTOS -una URL canonica que se repite
# arma un pico de filas sin ningun corte- y esos valores distintos triplican al
# mayor de los cinco largos anteriores medido tambien en valores distintos.
# Ninguno pasa del tope (es el maximo de la columna). Una ruta de archivo o un
# JSON no tienen esquema y quedan afuera.
#
# Un largo no distingue un corte de un largo fijo que un sistema genera siempre
# (URL firmadas, enlaces con un token de ancho fijo), asi que el hallazgo es un
# aviso de columna `ok`, no una sospecha: dice lo observado y las dos lecturas.
# Quedan sin cubrir, a proposito, un log donde las URL se repiten (menos del 25 %
# de valores distintos), una columna donde todo esta en el tope y ninguna URL es
# mas corta, y los esquemas sin `//` (`data:`, `mailto:`).
.detectar_url_truncada <- function(texto, largos, tope, posiciones) {
  if (!tope %in% .topes_de_url) return(NULL)
  en_tope <- largos == tope
  cortados <- texto[en_tope]
  # `://` en vector y de forma fija descarta de un golpe una columna que no es
  # de URL, sin depender de que valor cae primero.
  if (mean(grepl("://", cortados, fixed = TRUE, useBytes = TRUE)) < 0.9) {
    return(NULL)
  }
  es_url <- grepl("^[A-Za-z][A-Za-z0-9+.-]*://", cortados, perl = TRUE,
                  useBytes = TRUE) &
    !grepl(" ", cortados, fixed = TRUE, useBytes = TRUE)
  if (mean(es_url) < 0.9) return(NULL)
  # Solo las URL del tope se cuentan y se nombran: un valor que no lo es y cae
  # justo en ese largo no es parte de lo observado.
  cortados <- cortados[es_url]
  distintos_tope <- length(unique(cortados))
  if (distintos_tope < 3L) return(NULL)
  previos <- largos >= tope - 5L & largos < tope
  distintos_previos <- tabulate(
    as.integer(factor(largos[previos], levels = (tope - 5L):(tope - 1L)))[
      !duplicated(texto[previos])
    ],
    nbins = 5L
  )
  if (distintos_tope < 3 * max(distintos_previos, 1L)) return(NULL)
  if (sum(largos < tope) < 3L) return(NULL)
  if (length(unique(texto)) < max(10L, ceiling(0.25 * length(texto)))) {
    return(NULL)
  }
  list(
    indices = posiciones[en_tope][es_url],
    n_evaluados = length(texto),
    n_afectados = sum(es_url),
    n_distintos = distintos_tope,
    largo = tope,
    proporcion = sum(es_url) / length(texto),
    n_anterior = sum(largos == tope - 1L),
    n_siguiente = 0L,
    variante = "url"
  )
}

# Indice de Whipple: la proporcion observada de multiplos contra la que dejaria
# una distribucion sin apilamiento (20 % para 5, 10 % para 10), llevada a base
# 100. El mismo corte para las dos bases evita que una columna con 60 % de
# multiplos de 5 calle mientras otra con 45 % de multiplos de 10 se acuse. El
# corte queda dentro de la categoria "tosco" y deja afuera la variacion natural
# de una columna sin apilamiento.
.UMBRAL_WHIPPLE <- 150

# Categorias clasicas del indice de Whipple. Se publican con la cifra para que
# el lector sepa donde cae la columna.
.categoria_whipple <- function(indice) {
  if (indice < 105) {
    "menos de 105, muy preciso"
  } else if (indice < 110) {
    "105 a 109,9, preciso"
  } else if (indice < 125) {
    "110 a 124,9, aproximado"
  } else if (indice <= 175) {
    "125 a 175, tosco"
  } else {
    "m\u00e1s de 175, muy impreciso"
  }
}

# El apilamiento solo se mide en magnitudes que se declaran o se miden a mano:
# edades, pesos, tallas, estaturas, cinturas, perimetros, las dos presiones y
# sus siglas clinicas. Los nombres se separan por `_`, `.`, digitos y mayusculas
# internas -`EdadIngreso`, `edad2`, `pa_sistolica`-; `edad` y `age` habilitan
# tambien por prefijo, porque `edadpaciente` y `ageyears` no tienen separador.
# Quedan afuera las horas de trabajo, los anios de servicio, los tiempos de
# espera y los plazos por norma: sus valores redondos vienen de la convencion,
# no del redondeo de quien mide. Tambien quedan afuera los valores de
# laboratorio, que no se miden a mano.
.magnitudes_apilables <- c(
  "edad", "edades", "age", "ages", "peso", "pesos", "weight",
  "talla", "tallas", "altura", "alturas", "estatura", "estaturas", "height",
  "cintura", "cinturas", "perimetro", "perimetros", "circunferencia",
  "circunferencias", "presion", "presiones", "sistolica", "sistolicas",
  "diastolica", "diastolicas", "pas", "pad", "tas", "tad", "sbp", "dbp",
  "bp", "imc", "bmi"
)
# Palabras que empiezan como `age` y no son la edad.
.no_son_edad <- c(
  "agencia", "agencias", "agente", "agentes", "agenda", "agendas",
  "agendado", "agendada", "agendamiento"
)

.tokens_de_nombre <- function(nombre) {
  # El nombre puede traer un byte invalido: el perfil igual lo publica y el
  # diagnostico no puede abortar por eso. Se repara como en el resto del
  # paquete antes de partirlo.
  nombre <- .textos_para_plegar(nombre)
  # Un cambio de minuscula o digito a mayuscula, y de letra a digito, separa
  # palabras: `EdadIngreso`, `edad2`.
  separado <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", nombre, perl = TRUE)
  separado <- gsub("([A-Za-z])([0-9])", "\\1 \\2", separado, perl = TRUE)
  separado <- gsub("([0-9])([A-Za-z])", "\\1 \\2", separado, perl = TRUE)
  tokens <- unlist(
    strsplit(separado, "[^[:alnum:]]+", perl = TRUE), use.names = FALSE
  )
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(character())
  .normalizacion_minusculas_vector(tokens)
}

.es_magnitud_apilable <- function(nombre) {
  if (length(nombre) != 1L || is.na(nombre) || !nzchar(nombre)) return(FALSE)
  tokens <- .tokens_de_nombre(nombre)
  if (!length(tokens)) return(FALSE)
  if (any(tokens %in% .magnitudes_apilables)) return(TRUE)
  prefijo_edad <- substr(tokens, 1L, 4L) == "edad"
  prefijo_age <- substr(tokens, 1L, 3L) == "age" &
    !tokens %in% .no_son_edad
  any(prefijo_edad | prefijo_age)
}

# Limite inferior de Wilson unilateral al 99 % para la proporcion de multiplos.
# El indice publicado usa la proporcion observada; el diagnostico exige ademas
# que el limite inferior supere el corte, para que una muestra chica no llegue
# al umbral por azar. Con n = 80 el ruido de una columna con rango partido lo
# alcanzaba.
.Z_WHIPPLE <- 2.326348
.MIN_FILAS_APILAMIENTO <- 100L

# Maximo comun divisor de dos enteros no negativos.
.mcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b > 0) {
    resto <- a %% b
    a <- b
    b <- resto
  }
  a
}

# Paso de la grilla de los valores: todos los datos son multiplos de este
# numero (2 mmHg de un aparato, 2 en una talla de ropa, 10 g de una balanza).
# Fuera de esa grilla no hay valores que el instrumento o la escala no puedan
# producir, y el indice tiene que compararse contra las opciones que existen.
.resolucion_de_grilla <- function(vi) {
  if (!length(vi)) return(1)
  if (any(!is.finite(vi)) || any(abs(vi) > 2^53)) return(1)
  valores <- .sort_por_bytes(unique(round(vi)))
  if (length(valores) < 2L) return(1)
  diferencias <- diff(valores)
  diferencias <- diferencias[diferencias > 0]
  if (!length(diferencias)) return(1)
  resolucion <- 0
  for (diferencia in diferencias) {
    resolucion <- .mcd(resolucion, diferencia)
    if (resolucion <= 1) return(1)
  }
  if (resolucion < 1) 1 else resolucion
}

# Proporcion de la grilla que cae en los multiplos de `base`: con paso 2, uno de
# cada cinco multiplos de 2 tambien es multiplo de 10.
.esperado_en_grilla <- function(base, resolucion) {
  .mcd(base, resolucion) / base
}

.whipple_inferior <- function(cuenta, n, esperado) {
  if (!is.finite(n) || n <= 0) return(0)
  p <- cuenta / n
  z <- .Z_WHIPPLE
  denominador <- 1 + z^2 / n
  centro <- p + z^2 / (2 * n)
  margen <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  limite <- (centro - margen) / denominador
  if (!is.finite(limite)) return(0)
  100 * limite / esperado
}

.detectar_apilamiento <- function(valores, nombre, concentracion = NULL) {
  if (is.null(valores) || !is.numeric(valores)) return(NULL)
  nombre <- as.character(nombre)[[1L]]
  if (!.es_magnitud_apilable(nombre)) return(NULL)
  # Un unico valor que domina la columna es `valor_concentrado`, no
  # apilamiento: la preferencia por el digito terminal se mide entre varios
  # multiplos, no en el pico de una sola categoria.
  if (is.list(concentracion) && isTRUE(concentracion$dispara)) return(NULL)
  posiciones <- which(is.finite(valores))
  if (length(posiciones) < .MIN_FILAS_APILAMIENTO) return(NULL)
  v <- valores[posiciones]
  enteros <- abs(v - round(v)) <= 1e-8 * pmax(1, abs(v))
  if (mean(enteros) < 0.95) return(NULL)
  vi <- round(v[enteros])
  pos_enteros <- posiciones[enteros]
  # El cero es multiplo de todas las bases y no informa sobre redondeo.
  distintos <- vi != 0
  vi <- vi[distintos]
  pos_enteros <- pos_enteros[distintos]
  if (length(vi) < .MIN_FILAS_APILAMIENTO ||
      length(unique(vi)) < 10L) {
    return(NULL)
  }
  n <- length(vi)
  # Si practicamente todos los valores son multiplos de 5, la columna entera
  # esta redondeada por diseno (una encuesta que pide responder de a 5).
  if (sum(vi %% 5L == 0L) / n >= 0.95) return(NULL)
  # Un redondeo mas grueso -500, 1000- explica los multiplos de 5 y de 10 sin
  # que haya preferencia por el digito terminal: no es apilamiento.
  if (sum(vi %% 50L == 0L) / n >= 0.5) return(NULL)
  # El indice se mide sobre la grilla de resolucion de los datos: si todos son
  # pares, los multiplos de 10 son una de cada cinco opciones, no una de cada
  # diez. Asi una presion tomada por un aparato de 2 mmHg no se confunde con
  # una preferencia por el digito terminal.
  resolucion <- .resolucion_de_grilla(vi)
  candidatos <- list(
    list(
      base = 5L, cuenta = sum(vi %% 5L == 0L),
      esperado = .esperado_en_grilla(5L, resolucion)
    ),
    list(
      base = 10L, cuenta = sum(vi %% 10L == 0L),
      esperado = .esperado_en_grilla(10L, resolucion)
    )
  )
  validos <- list()
  for (candidato in candidatos) {
    proporcion <- candidato$cuenta / n
    indice <- 100 * proporcion / candidato$esperado
    if (proporcion > 0.95) next
    if (indice < .UMBRAL_WHIPPLE ||
        .whipple_inferior(candidato$cuenta, n, candidato$esperado) <
          .UMBRAL_WHIPPLE ||
        candidato$cuenta - candidato$esperado * n < 10) {
      next
    }
    validos[[length(validos) + 1L]] <- list(
      base = candidato$base, proporcion = proporcion, indice = indice,
      cuenta = candidato$cuenta, esperado = candidato$esperado
    )
  }
  if (!length(validos)) return(NULL)
  elegido <- validos[[1L]]
  if (length(validos) > 1L) {
    cinco <- validos[[1L]]
    diez <- validos[[2L]]
    # Los multiplos de 10 son un subconjunto de los de 5. Se publica la base 10
    # cuando casi todos los redondos terminan en 0; si ademas hay valores que
    # terminan en 5, la base real es 5.
    elegido <- if (diez$proporcion >= 0.75 * cinco$proporcion) diez else cinco
  }
  redondos <- vi %% elegido$base == 0L
  list(
    indices = pos_enteros[redondos],
    n_evaluados = n,
    n_afectados = elegido$cuenta,
    base = elegido$base,
    indice = elegido$indice,
    categoria = .categoria_whipple(elegido$indice),
    proporcion = elegido$proporcion,
    esperado = elegido$esperado
  )
}

# Una serie acumulada que baja. El reinicio de un contador no es un retroceso:
# vuelve cerca del comienzo de la serie, medido sobre lo que la serie acumulo
# hasta el pico, y deja el nivel bajo en las filas siguientes. Una caida que
# deja el valor muy por encima de ese nivel es un retroceso, aunque sea grande:
# un saldo inicial alto no vuelve reinicio a cualquier caida. Los cortes de un
# acumulado por grupo o por anio caen en tramos regulares y se reconocen aparte.
# El nombre solo habilita la regla cuando declara acumulacion sin ambiguedad; si
# no dice nada, la forma tiene que ser la de un acumulado que arranca cerca de
# cero y el nombre no puede ser el de un plazo o una duracion.
.tokens_de_tiempo <- c(
  "dia", "dias", "mes", "meses", "plazo", "vencimiento", "duracion",
  "duraciones", "hora", "horas", "minuto", "minutos", "segundo", "segundos",
  "seg", "semana", "semanas", "anio", "anios", "year", "years", "month",
  "months", "day", "days"
)

# Los cortes que caen en tramos crecientes de longitud parecida -cuentas
# apiladas, un acumulado por grupo o por anio- son reinicios de bloque, no
# retrocesos de una sola serie. Se exige mas de un corte y al menos tres tramos;
# si los largos difieren demasiado no se borra la evidencia de una caida real.
.reinicios_en_tramos <- function(v, caidas) {
  reinicios <- rep(FALSE, length(caidas))
  if (length(caidas) < 2L) return(reinicios)
  largos <- diff(c(0L, caidas, length(v)))
  if (length(largos) < 3L || min(largos) < 5L) return(reinicios)
  if (max(largos) / min(largos) > 1.5) return(reinicios)
  reinicios[] <- TRUE
  reinicios
}

.detectar_serie_acumulada <- function(valores, nombre, fila = NULL,
                                      resultado = NULL) {
  if (is.null(valores) || !is.numeric(valores)) return(NULL)
  nombre <- as.character(nombre)[[1L]]
  if (length(valores) < 20L) return(NULL)
  tokens_nombre <- .tokens_de_nombre(nombre)
  # Un saldo puede mezclar cuentas, periodos o grupos: si no hay una llave
  # para separarlos, una caida no permite distinguir un corte de un retroceso.
  # El nombre no habilita esta regla solo por llevar tambien `acumulado`.
  if (any(tokens_nombre %in% c("saldo", "saldos"))) return(NULL)
  # Un identificador o una numeracion no es una serie acumulada, aunque tenga
  # una fila fuera de orden. La decision es la que ya publica el perfil: una
  # secuencia densa y casi sin repetir, o un texto con forma de identificador.
  # No alcanza con `casi_clave`: una serie acumulada tambien es casi unica.
  if (!is.null(resultado)) {
    if (isTRUE(.decision_identificador_numerico(resultado$fila)$posible)) {
      return(NULL)
    }
  } else if (!is.null(fila) &&
             isTRUE(.decision_identificador_numerico(fila)$posible)) {
    return(NULL)
  }
  # Una serie con ausentes se evalua sobre los presentes, en el orden en que
  # aparecen: el salto entre dos filas separadas no se inventa.
  posiciones <- which(is.finite(valores))
  v <- as.numeric(valores[posiciones])
  if (length(v) < 20L) return(NULL)
  diferencias <- diff(v)
  if (!length(diferencias)) return(NULL)
  proporcion <- mean(diferencias >= 0)
  caidas <- which(diferencias < 0)
  if (proporcion < 0.9 || !length(caidas) ||
      length(caidas) > max(1, 0.1 * length(diferencias))) {
    return(NULL)
  }
  # Un reinicio vuelve al orden del comienzo: lo que queda por encima del
  # primer valor es una parte chica de lo acumulado hasta el pico. Con un saldo
  # inicial de 5e6, una caida de 10.000 deja casi todo el saldo y es un
  # retroceso. Ademas el nivel bajo tiene que durar: si la fila siguiente
  # recupera el valor anterior, fue una caida de una fila, no un reinicio.
  inicio <- v[[1L]]
  pico <- cummax(v)
  avance <- pico[caidas] - inicio
  nuevo <- v[caidas + 1L]
  cerca_del_inicio <- avance > 0 & (nuevo - inicio) <= 0.05 * avance
  recupera <- rep(FALSE, length(caidas))
  con_siguiente <- caidas + 2L <= length(v)
  recupera[con_siguiente] <-
    v[caidas[con_siguiente] + 2L] >= v[caidas[con_siguiente]]
  reinicios <- (cerca_del_inicio & !recupera) |
    .reinicios_en_tramos(v, caidas)
  caidas_reales <- caidas[!reinicios]
  if (!length(caidas_reales)) return(NULL)
  sugiere_acumulado <- isTRUE(grepl(
    .regex_sin_caja(
      "acumul\\w*|acum|cumulativ\\w*|cumulative|running[ _-]?total|ytd"
    ),
    nombre, perl = TRUE, useBytes = TRUE
  ))
  maximo <- max(v)
  if (!sugiere_acumulado) {
    # Un plazo o una duracion ordenados no son una serie acumulada, aunque
    # tengan una fila fuera de orden.
    if (any(tokens_nombre %in% .tokens_de_tiempo)) return(NULL)
    # Sin nombre que lo declare, la forma tiene que ser inequivoca: casi
    # monotona, positiva, con el comienzo cerca del cero y con un paso tipico
    # positivo. Una columna ordenada con muchos empates -una posicion, un
    # ranking, valores redondeados- no acumula: repite el mismo nivel.
    pasos <- diferencias[diferencias >= 0]
    if (!(proporcion >= 0.98 && maximo > 0 && min(v) <= 0.05 * maximo &&
          length(pasos) && stats::median(pasos) > 0)) {
      return(NULL)
    }
  }
  list(
    indices = posiciones[caidas_reales + 1L],
    n_evaluados = length(v),
    n_afectados = length(caidas_reales),
    n_caidas = length(caidas),
    n_reinicios = sum(reinicios),
    proporcion = proporcion
  )
}

# Dos escalas en una columna. Se busca en los valores que Tukey ya separo del
# grueso: si al llevarlos por 10, 100 o 1000 caen donde esta el grueso, con una
# dispersion parecida, la minoria no es una cola: es la misma magnitud escrita
# en otra unidad. La minoria tiene que ser chica (<= 25 %) para no llamar
# "escala mezclada" a una bimodal legitima con dos grupos comparables.
.detectar_escala_mixta <- function(valores) {
  if (is.null(valores) || !is.numeric(valores)) return(NULL)
  posiciones <- which(is.finite(valores) & valores > 0)
  if (length(posiciones) < 40L) return(NULL)
  v <- as.numeric(valores[posiciones])
  cuartiles <- stats::quantile(v, c(0.25, 0.75), names = FALSE, type = 7)
  iqr <- cuartiles[[2L]] - cuartiles[[1L]]
  if (!is.finite(iqr) || iqr <= 0) return(NULL)
  lados <- list(
    list(
      nombre = "alto",
      grupo = v > cuartiles[[2L]] + 1.5 * iqr,
      resto = v <= cuartiles[[2L]] + 1.5 * iqr
    ),
    list(
      nombre = "bajo",
      grupo = v < cuartiles[[1L]] - 1.5 * iqr,
      resto = v >= cuartiles[[1L]] - 1.5 * iqr
    )
  )
  elegido <- NULL
  for (lado in lados) {
    n_grupo <- sum(lado$grupo)
    if (n_grupo < 5L || n_grupo > 0.25 * length(v)) next
    grupo <- v[lado$grupo]
    resto <- v[lado$resto]
    if (length(resto) < 20L || length(unique(grupo)) < 3L) next
    mediana_resto <- stats::median(resto)
    if (!is.finite(mediana_resto) || mediana_resto <= 0) next
    q_resto <- stats::quantile(resto, c(0.25, 0.75), names = FALSE, type = 7)
    iqr_resto <- q_resto[[2L]] - q_resto[[1L]]
    if (!is.finite(iqr_resto) || iqr_resto <= 0) next
    for (potencia in 1:3) {
      factor <- 10^potencia
      if (identical(lado$nombre, "alto")) {
        razon <- stats::median(grupo) / mediana_resto
        escalado <- grupo / factor
      } else {
        razon <- mediana_resto / stats::median(grupo)
        escalado <- grupo * factor
      }
      if (!is.finite(razon) || razon <= 0) next
      desvio <- abs(log10(razon) - potencia)
      # La razon es una potencia de diez con la tolerancia que impone una
      # minoria chica: 1,5 veces el factor, no un orden de magnitud cualquiera.
      if (desvio > log10(1.5)) next
      mediana_escalada <- stats::median(escalado)
      if (!is.finite(mediana_escalada) ||
          mediana_escalada < q_resto[[1L]] ||
          mediana_escalada > q_resto[[2L]]) {
        next
      }
      iqr_escalado <- stats::IQR(escalado)
      if (!is.finite(iqr_escalado) || iqr_escalado <= 0) next
      rel_iqr <- iqr_escalado / iqr_resto
      if (!is.finite(rel_iqr) || rel_iqr < 1 / 3 || rel_iqr > 3) next
      dentro <- mean(escalado >= q_resto[[1L]] & escalado <= q_resto[[2L]])
      if (dentro < 0.4) next
      # Una cola de una distribucion asimetrica escalada puede parecerse a su
      # grueso por la mediana. Lo que la distingue de una escala mezclada es que
      # el grupo escalado REPITE la distribucion del grueso: llega hasta su
      # cuartil inferior y cubre su rango intercuartilico. Una cola recortada
      # empieza mas arriba.
      if (min(escalado) > q_resto[[1L]]) next
      q_escalado <- stats::quantile(
        escalado, c(0.25, 0.75), names = FALSE, type = 7
      )
      if (q_escalado[[1L]] > 1.5 * q_resto[[1L]]) next
      if (q_escalado[[2L]] < q_resto[[2L]] / 1.5) next
      puntaje <- desvio + abs(log10(rel_iqr))
      if (is.null(elegido) || puntaje < elegido$puntaje) {
        elegido <- list(
          grupo = lado$grupo, lado = lado$nombre, potencia = potencia,
          factor = factor, razon = razon, mediana_grupo = stats::median(grupo),
          mediana_resto = mediana_resto, dentro = dentro, puntaje = puntaje
        )
      }
    }
  }
  if (is.null(elegido)) return(NULL)
  list(
    indices = posiciones[elegido$grupo],
    n_evaluados = length(v),
    n_afectados = sum(elegido$grupo),
    factor = elegido$factor,
    razon = elegido$razon,
    lado = elegido$lado,
    mediana_grupo = elegido$mediana_grupo,
    mediana_resto = elegido$mediana_resto,
    dentro = elegido$dentro
  )
}

# Arma los cuatro hallazgos de una columna. El universo aplicable ya viene
# aplicado en `resultado$aplicable`; las trazas se calculan aca para que el
# conteo y las filas nombradas salgan del mismo vector.
.hallazgos_exactitud_columna <- function(nombre, x, fila, resultado, datos,
                                         distinguir_mayusculas = TRUE,
                                         expandir = FALSE) {
  salida <- list()
  agregar <- function(h) {
    if (is.null(h) || !length(h$indices)) return(invisible())
    traza <- .trazabilidad_indices(
      h$indices, "completo", limite = 1000L, datos = datos
    )
    h$hallazgo$trazabilidad <- I(list(traza))
    salida[[length(salida) + 1L]] <<- h$hallazgo
    invisible()
  }
  aplicable <- resultado$aplicable

  if (is.character(x) || is.factor(x)) {
    texto <- .texto_en_el_universo(x, aplicable)
    truncado <- .detectar_texto_truncado(
      texto, patrones = resultado$patrones,
      distinguir_mayusculas = distinguir_mayusculas, expandir = expandir
    )
    if (!is.null(truncado)) {
      es_url <- identical(truncado$variante, "url")
      agregar(list(
        indices = truncado$indices,
        hallazgo = .nuevo_hallazgo(
          nombre, "texto_truncado", if (es_url) "ok" else "sospechoso",
          if (es_url) {
            paste0(
              "Aviso de columna: ", truncado$n_distintos, " URL distintas ",
              "tienen exactamente ", truncado$largo, " caracteres y ninguna es ",
              "m\u00e1s larga. Puede ser el tope de un campo que las cort\u00f3 o ",
              "un largo fijo del sistema que las genera."
            )
          } else {
            paste0(
              "La columna acumula valores en un largo que parece un tope de ",
              "campo y los corta a mitad de palabra."
            )
          },
          paste0(
            "Largo m\u00e1ximo: ", truncado$largo, " caracteres; ",
            truncado$n_afectados, " de ", truncado$n_evaluados,
            " valores (",
            .formatear_decimal_publicado(100 * truncado$proporcion),
            " %) est\u00e1n en ese largo; los largos inmediatamente anterior y ",
            "posterior tienen ", truncado$n_anterior, " y ",
            truncado$n_siguiente, " valores. ",
            if (es_url) {
              paste0(
                "Los valores del tope empiezan con un esquema y no tienen ",
                "espacios; ", truncado$n_distintos, " son distintos y triplican ",
                "a los de cada uno de los cinco largos anteriores."
              )
            } else {
              "Los valores del tope tienen palabras y terminan en letra o d\u00edgito."
            }
          ),
          if (es_url) {
            paste0(
              "Comprobar en la fuente si el campo tiene un ancho declarado ",
              "igual a ese largo, o si el sistema que genera las URL produce ",
              "siempre ese largo. El plan de limpieza no modifica estos valores."
            )
          } else {
            paste0(
              "Revisar el ancho declarado de la columna en la fuente y recuperar ",
              "los valores completos; no volver a cargarlos a mano."
            )
          },
          truncado$n_evaluados, truncado$n_afectados, "fila"
        )
      ))
    }
  }

  # Los detectores numericos necesitan la misma conversion que el resumen, no
  # `as.numeric()` sobre la columna cruda: un factor daria codigos de nivel y un
  # numero escrito con coma daria NA. Solo se paga esa conversion en las columnas
  # que el perfil ya reconocio como numericas: sobre una columna de texto libre
  # no hay apilamiento ni escala que medir, y parsearla entera para buscarlos
  # seria un costo sin alcance.
  tipo_inferido <- tryCatch(
    as.character(fila$tipo_inferido[[1L]]), error = function(e) NULL
  )
  evaluar_numericos <- length(tipo_inferido) == 1L &&
    !is.na(tipo_inferido) &&
    tipo_inferido %in% c("entero", "doble", "integer64")
  # Un numero escrito como texto se vuelve a leer para estos diagnosticos, y
  # esa lectura es la cara (el perfil ya leyo la columna una vez). Se paga solo
  # si algo la justifica: un nombre de magnitud que se apila, o valores alejados
  # que puedan ser otra escala. Una columna numerica nativa no se relee.
  if (evaluar_numericos && !is.numeric(x)) {
    n_alejados <- suppressWarnings(as.numeric(fila$n_outliers[[1L]]))
    evaluar_numericos <- .es_magnitud_apilable(nombre) ||
      (length(n_alejados) == 1L && !is.na(n_alejados) && n_alejados > 0)
  }
  numericos <- NULL
  if (evaluar_numericos) {
    cuantitativos <- .cuantitativos_en_el_universo(
      .valores_cuantitativos_hallazgo(x, fila, resultado), aplicable
    )
    numericos <- .numerico_trazable(cuantitativos)
  }

  if (!is.null(numericos)) {
    apilado <- .detectar_apilamiento(
      numericos, nombre, resultado$valor_concentrado
    )
    if (!is.null(apilado)) {
      agregar(list(
        indices = apilado$indices,
        hallazgo = .nuevo_hallazgo(
          nombre, "apilamiento", "sospechoso",
          paste0(
            "Los valores se acumulan en m\u00faltiplos de ", apilado$base,
            " m\u00e1s de lo que explicar\u00eda una distribuci\u00f3n sin redondeo."
          ),
          paste0(
            "M\u00faltiplos de ", apilado$base, ": ", apilado$n_afectados,
            " de ", apilado$n_evaluados, " valores enteros no nulos (",
            .formatear_decimal_publicado(100 * apilado$proporcion),
            " %), frente al ",
            .formatear_decimal_publicado(100 * apilado$esperado),
            " % esperado. \u00cdndice de Whipple: ",
            .formatear_decimal_publicado(apilado$indice),
            ". Categor\u00eda de Naciones Unidas: ", apilado$categoria, "."
          ),
          paste0(
            "Revisar si el redondeo viene de la declaraci\u00f3n de la variable ",
            "o de la forma de recolectarla; si es un artefacto, recuperar el ",
            "valor medido antes de analizarlo. El \u00edndice de Whipple compara ",
            "los m\u00faltiplos observados con los esperados y se clasifica con ",
            "las categor\u00edas de Naciones Unidas, de menos de 105 (muy preciso) ",
            "a m\u00e1s de 175 (muy impreciso)."
          ),
          apilado$n_evaluados, apilado$n_afectados, "fila"
        )
      ))
    }
  }

  if (!is.null(numericos)) {
    serie <- .detectar_serie_acumulada(numericos, nombre, fila, resultado)
    if (!is.null(serie)) {
      agregar(list(
        indices = serie$indices,
        hallazgo = .nuevo_hallazgo(
          nombre, "serie_acumulada_retrocede", "sospechoso",
          paste0(
            "La serie acumulada retrocede en filas que no son el reinicio de ",
            "un contador."
          ),
          paste0(
            serie$n_afectados, " de ", serie$n_evaluados,
            " filas bajan respecto de la anterior; ",
            .formatear_decimal_publicado(100 * serie$proporcion),
            " % de los pasos no son decrecientes. Caidas de reinicio ",
            "descontadas: ", serie$n_reinicios, "."
          ),
          paste0(
            "Revisar el orden de las filas y el origen de esas caidas antes de ",
            "usar los totales; una serie acumulada solo debe bajar al reiniciarse."
          ),
          serie$n_evaluados, serie$n_afectados, "fila"
        )
      ))
    }
  }

  if (!is.null(numericos)) {
    escala <- .detectar_escala_mixta(numericos)
    if (!is.null(escala)) {
      agregar(list(
        indices = escala$indices,
        hallazgo = .nuevo_hallazgo(
          nombre, "escala_mixta", "ok",
          paste0(
            "La columna mezcla dos escalas: una minor\u00eda de valores es el ",
            "grueso multiplicado o dividido por una potencia de diez. Puede ",
            "ser una mezcla de unidades o dos poblaciones distintas."
          ),
          paste0(
            "Grupo ", escala$lado, ": ", escala$n_afectados, " de ",
            escala$n_evaluados, " valores. Raz\u00f3n de medianas: ",
            .formatear_decimal_publicado(escala$razon),
            " (factor ", .formatear_decimal_publicado(escala$factor), "). ",
            "Mediana del grupo: ",
            .formatear_decimal_publicado(escala$mediana_grupo),
            "; mediana del grueso: ",
            .formatear_decimal_publicado(escala$mediana_resto), "."
          ),
          paste0(
            "Declarar la unidad y decidir si son dos poblaciones distintas o ",
            "una unidad mal cargada; separar o convertir antes de sumar o ",
            "comparar. El plan de limpieza no recorta estos valores."
          ),
          escala$n_evaluados, escala$n_afectados, "fila"
        )
      ))
    }
  }

  salida
}
