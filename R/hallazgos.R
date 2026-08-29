# La trazabilidad tiene dos ejes que no se pisan: `estado` dice si se pudo
# localizar y hasta donde, y `localizador` dice CON QUE se localiza. Una
# trazabilidad puede ser al mismo tiempo por clave y truncada; meter las dos
# cosas en un solo campo perderia informacion.
.trazabilidad_vacia <- function(estado = "no_disponible",
                                indices = integer(), total = NA_real_,
                                alcance = "no_evaluado", limite = 1000L,
                                claves = NULL) {
  indices <- as.integer(indices[!is.na(indices)])
  total <- if (length(total) && is.finite(total)) as.numeric(total) else NA_real_
  truncado <- estado %in% c("disponible", "truncada") &&
    is.finite(total) && length(indices) < total
  if (truncado) estado <- "truncada"
  hay_claves <- !is.null(claves) && is.data.frame(claves) && nrow(claves) > 0L
  list(
    estado = estado,
    localizador = if (hay_claves) {
      "clave_declarada"
    } else if (length(indices)) {
      "indice_fila"
    } else {
      "ninguno"
    },
    indices_fila = indices,
    claves = if (hay_claves) claves else NULL,
    total = total,
    mostrados = length(indices),
    truncado = truncado,
    limite = if (is.infinite(limite)) Inf else as.integer(limite),
    alcance = alcance
  )
}

.trazabilidad_indices <- function(indices, alcance, limite = 1000L,
                                  clave = NULL, datos = NULL) {
  indices <- as.integer(indices[!is.na(indices)])
  total <- length(indices)
  mostrados <- utils::head(indices, limite)
  .trazabilidad_vacia(
    # Las dos ramas de este `if` devolvian "disponible", que es un resto de una
    # correccion: con cero indices el objeto decia `estado = "disponible"` y a la
    # vez `localizador = "ninguno"` y `mostrados = 0`, o sea prometia una
    # localizacion que no existe. Un consumidor que mire `estado` para decidir si
    # puede ubicar filas decidia mal.
    #
    # La rama vacia va a `no_disponible`, que es el valor por omision de
    # `.trazabilidad_vacia()` y el que `.limitar_trazabilidad()` ya sabe saltear.
    estado = if (total) "disponible" else "no_disponible",
    indices = mostrados,
    total = total, alcance = alcance, limite = limite,
    claves = .claves_de_filas(datos, clave, mostrados)
  )
}

.limitar_trazabilidad <- function(trazabilidad, limite = 1000L,
                                  clave = NULL, datos = NULL) {
  if (!is.list(trazabilidad) ||
      identical(trazabilidad$estado, "no_disponible")) {
    return(trazabilidad)
  }
  indices <- as.integer(trazabilidad$indices_fila)
  indices <- indices[!is.na(indices)]
  trazabilidad$limite <- if (is.infinite(limite)) Inf else as.integer(limite)
  trazabilidad$indices_fila <- utils::head(indices, limite)
  trazabilidad$mostrados <- length(trazabilidad$indices_fila)
  trazabilidad$truncado <- is.finite(trazabilidad$total) &&
    trazabilidad$mostrados < trazabilidad$total
  if (length(trazabilidad$n_filas_formas_variantes) == 1L &&
      is.finite(trazabilidad$n_filas_formas_variantes)) {
    trazabilidad$mostrados_formas_variantes <- min(
      trazabilidad$mostrados, trazabilidad$n_filas_formas_variantes
    )
    trazabilidad$mostrados_formas_dominantes <- max(
      0L, trazabilidad$mostrados - trazabilidad$mostrados_formas_variantes
    )
  }
  trazabilidad$estado <- if (trazabilidad$truncado) {
    "truncada"
  } else {
    "disponible"
  }
  if (!is.null(clave)) {
    trazabilidad$claves <- .claves_de_filas(
      datos, clave, trazabilidad$indices_fila
    )
  }
  hay_claves <- !is.null(trazabilidad$claves) &&
    is.data.frame(trazabilidad$claves) && nrow(trazabilidad$claves) > 0L
  trazabilidad$localizador <- if (hay_claves) {
    "clave_declarada"
  } else if (length(trazabilidad$indices_fila)) {
    "indice_fila"
  } else {
    "ninguno"
  }
  trazabilidad
}

# Las claves se devuelven como data frame: una fila por indice mostrado y una
# columna por componente de la clave. Concatenarlas perderia los tipos y haria
# ambigua una clave compuesta cuyos valores contengan el separador.
.claves_de_filas <- function(datos, clave, indices) {
  if (is.null(clave) || !length(clave) || is.null(datos)) return(NULL)
  if (!length(indices)) return(NULL)
  faltantes <- setdiff(clave, names(datos))
  if (length(faltantes)) return(NULL)
  salida <- .seleccionar_columnas(datos, clave, filas = indices)
  rownames(salida) <- NULL
  salida
}

.nuevo_hallazgo <- function(columna, tipo, severidad, descripcion,
                            evidencia, sugerencia, n_evaluados = NA_real_,
                            n_afectados = NA_real_, unidad_conteo = NA_character_,
                            estado_reparacion = NA_character_) {
  data.frame(
    columna = columna,
    tipo_hallazgo = tipo,
    severidad = severidad,
    descripcion = descripcion,
    evidencia = evidencia,
    sugerencia = sugerencia,
    n_evaluados = as.numeric(n_evaluados),
    n_afectados = as.numeric(n_afectados),
    unidad_conteo = as.character(unidad_conteo),
    estado_reparacion = as.character(estado_reparacion),
    trazabilidad = I(list(.trazabilidad_vacia())),
    stringsAsFactors = FALSE
  )
}

# La tasa de valores distintos sola no alcanza: con dos valores en tres filas da
# 0,67 y supera cualquier umbral razonable, aunque una columna de dos valores no
# puede tener cardinalidad alta. Por debajo de este numero de valores distintos
# la columna es una categoria, se mire como se mire, y eso es una conclusion
# medida y no una falta de medicion.
.min_distintos_alta_cardinalidad <- 10L

# Un identificador o un codigo no son magnitudes, y hay pruebas que solo
# describen magnitudes: Benford supone un proceso multiplicativo, y los limites
# de Tukey suponen una distribucion. Que un codigo quede lejos de la mediana es
# cierto y no significa nada.
#
# Dos senales, y hacen falta las dos. La primera es la DENSIDAD: una numeracion
# ocupa un tramo compacto de los enteros -`MotId` cubre el 69 % de los que hay
# entre su minimo y su maximo- y una magnitud se reparte por varios ordenes
# -0,005 % para montos entre 9 y 9.999.999-. La unicidad no sirve para esto,
# porque un monto tambien es casi unico.
#
# La segunda es la AUSENCIA DE UN SALTO DE ESCALA. La densidad sola no alcanza:
# un valor fuera de escala de hasta el doble del maximo no la baja lo
# suficiente, asi que quedaban tapados justo los casos que hay que ver. Un `120`
# entre edades de 18 a 70 deja la densidad en 0,52 y el hallazgo se perdia; un
# `2000` detras de 1..1000 la deja en 0,5005 y tambien. Lo que si los delata es
# el hueco que abren: 50 y 1.000 donde el tipico es 1.
#
# Medido sobre un banco de quince columnas con la respuesta conocida -seis
# numeraciones reales y nueve magnitudes con un dato malo dentro-, cruzar las dos
# senales acierta catorce y **no calla ningun dato malo real**. La densidad sola
# acertaba once y callaba dos. Su unico error es hablar de mas sobre un
# identificador disperso, que es el lado barato de equivocarse.
#
# **No se apaga: se declara.** Lo que dejo de decirse queda en
# `cobertura_diagnosticos` con el motivo medido.
.MIN_DENSIDAD_NUMERACION <- 0.5

# Con menos de cinco valores distintos una densidad alta no dice nada. Estaba en
# veinte y era un salto arbitrario: un codigo de diecinueve valores recibia el
# falso positivo y el mismo codigo con un valor mas no lo recibia.
.MIN_DISTINTOS_NUMERACION <- 5L

# **Se probo reconocer la clave dispersa por unicidad y se retiro.** El problema
# es real: sobre una clave repartida en un rango ancho se emite
# `desviacion_benford`, que afirma un problema de calidad sobre una columna que
# no tiene distribucion que analizar. Y la densidad no puede resolverlo, porque
# una clave de 1 a 2.300.000 tiene densidad 0,0043 -mas dispersa que un monto,
# que da 0,0096-: no hay umbral sobre ese eje que los separe.
#
# El criterio que se probo era "unicidad que el azar no explica": con `n` valores
# en `P` posiciones el azar produce del orden de `n^2/(2P)` coincidencias, y no
# tener ninguna cuando se esperaban muchas significa que algo las prohibe. Falla
# por dos motivos medidos:
#
# 1. **Depende de cuantas filas se cargaron, no de la columna.** El estadistico
#    crece con el cuadrado de `n`, asi que la misma clave de cedulas cambia de
#    veredicto al pasar las ~5.200 filas. Medido: un padron de 2.000 filas recibe
#    `desviacion_benford` y el mismo padron con 30.000 se reconoce bien. Eso es
#    una propiedad de la consulta, no del dato.
# 2. **Calla magnitudes reales.** Una lectura acumulada de medidor, un timestamp
#    en segundos, una coordenada UTM redondeada y un monto que solo se llena en
#    algunos expedientes son unicos por su mecanismo, no por ser claves. Sobre la
#    lectura de medidor con un valor absurdo adentro, el criterio se tragaba el
#    valor absurdo -y cuanto mas sutil el error, mas seguro se lo tragaba-.
#
# La regla del paquete es que una guarda solo entra si no calla nada real. Esta
# callaba, asi que no entra: se prefiere hablar de mas sobre un identificador
# disperso, que es el lado barato de equivocarse, y queda dicho arriba.

.parece_identificador_numerico <- function(fila) {
  # No alcanza con `entero`: por la puerta DBI casi todo llega como `doble`, y
  # varios lectores de CSV tambien. La densidad solo se mide cuando todos los
  # valores son enteros, asi que el tipo de almacenamiento no aporta nada.
  numerico <- as.character(fila$tipo_inferido) %in% c("entero", "doble") ||
    as.character(fila$tipo_declarado) %in% c("entero", "doble")
  if (!isTRUE(numerico)) return(FALSE)
  # Un centinela adentro hunde la densidad y abre el salto: un identificador de
  # 1 a 4000 con quince `9999` pasaba de densidad 0,625 a 0,250 y dejaba de
  # reconocerse. Si sacando el centinela la columna es compacta, la numeracion
  # esta ahi y el centinela es lo que hay que informar aparte.
  densidad_limpia <- suppressWarnings(as.numeric(fila$densidad_sin_centinela))
  hay_centinela <- isTRUE(is.finite(densidad_limpia))
  if (hay_centinela) {
    distintos_limpios <- suppressWarnings(as.numeric(fila$n_distintos)) - 1
    return(
      isTRUE(densidad_limpia >= .MIN_DENSIDAD_NUMERACION) &&
        isTRUE(is.finite(distintos_limpios) &&
                 distintos_limpios >= .MIN_DISTINTOS_NUMERACION)
    )
  }
  if (isTRUE(fila$salto_de_escala_secuencia_entera)) return(FALSE)
  densidad <- suppressWarnings(as.numeric(fila$densidad_secuencia_entera))
  distintos <- suppressWarnings(as.numeric(fila$n_distintos))
  isTRUE(is.finite(densidad) && densidad >= .MIN_DENSIDAD_NUMERACION) &&
    isTRUE(is.finite(distintos) && distintos >= .MIN_DISTINTOS_NUMERACION)
}

.cobertura_diagnosticos_vacia <- function() {
  data.frame(
    diagnostico = character(), columna = character(), motivo = character(),
    como_resolverlo = character(), dependencia = character(),
    stringsAsFactors = FALSE
  )
}

.patrones_raros_recortados <- function(patrones) {
  raros_trazabilidad <- attr(
    patrones, "patrones_raros_trazabilidad", exact = TRUE
  )
  n_raros_trazabilidad <- attr(
    patrones, "n_patrones_raros_trazabilidad", exact = TRUE
  )
  if (is.character(raros_trazabilidad) &&
      length(n_raros_trazabilidad) == 1L &&
      is.finite(n_raros_trazabilidad)) {
    return(length(raros_trazabilidad) < n_raros_trazabilidad)
  }
  resumen <- attr(patrones, "resumen_patrones", exact = TRUE)
  if (is.null(resumen) || !is.data.frame(resumen)) return(FALSE)
  n_raros <- attr(patrones, "n_patrones_raros", exact = TRUE)
  if (length(n_raros) == 1L && is.finite(n_raros)) {
    return(n_raros > max(0L, nrow(resumen) - 1L))
  }
  # Without the count, exhaustiveness cannot be established safely.
  TRUE
}

.alcance_patron_raro <- function(patrones) {
  parcial <- isTRUE(.patrones_raros_recortados(patrones))
  muestreado <- isTRUE(attr(patrones, "muestreado", exact = TRUE))
  if (parcial && muestreado) {
    "muestra_patrones+patrones_parciales"
  } else if (parcial) {
    "patrones_parciales"
  } else if (muestreado) {
    "muestra_patrones"
  } else {
    "completo"
  }
}

.desvios_patron_raro_detectados <- function(patrones,
                                            secuencia_entera_densa,
                                            umbral_patron_raro) {
  resumen <- attr(patrones, "resumen_patrones", exact = TRUE)
  if (is.null(resumen) || !is.data.frame(resumen) || !nrow(resumen)) {
    return(NULL)
  }
  raros <- attr(patrones, "patrones_raros_trazabilidad", exact = TRUE)
  if (!is.character(raros)) {
    raros <- if (nrow(resumen) > 1L) resumen[-1L, "patron"] else character()
    raros <- as.character(raros[
      !is.na(resumen[-1L, "proporcion"]) &
        resumen[-1L, "proporcion"] < umbral_patron_raro
    ])
  }
  raros <- unique(raros[!is.na(raros)])
  if (isTRUE(secuencia_entera_densa) && length(raros)) {
    solo_largo <- .patrones_solo_largo_corrida_numerica(
      resumen$patron[[1L]], raros
    )
    raros <- raros[!solo_largo]
  }
  raros
}

.desvios_patron_raro_presentacion <- function(patrones) {
  resumen <- attr(patrones, "resumen_patrones", exact = TRUE)
  desvios <- attr(patrones, "desvios_patron_raro", exact = TRUE)
  if (is.null(resumen) || !is.data.frame(resumen)) {
    return(data.frame(
      patron = character(), n = integer(), proporcion = numeric(),
      ejemplos = character(), stringsAsFactors = FALSE
    ))
  }
  nombres <- if (is.data.frame(desvios) && "patron" %in% names(desvios)) {
    as.character(desvios$patron)
  } else if (is.character(desvios)) {
    desvios
  } else character()
  resumen[resumen$patron %in% nombres, , drop = FALSE]
}

.nuevo_diagnostico_no_evaluado <- function(diagnostico, columna, motivo,
                                           como_resolverlo,
                                           dependencia = NA_character_) {
  data.frame(
    diagnostico = as.character(diagnostico),
    columna = as.character(columna),
    motivo = as.character(motivo),
    como_resolverlo = as.character(como_resolverlo),
    dependencia = as.character(dependencia),
    stringsAsFactors = FALSE
  )
}

# Un valor de texto cuyos bytes no son UTF-8 validos y cuya codificacion nadie
# declaro no se puede analizar: no hay forma de adivinar si `0xE9` era una `e`
# con tilde, una `i` con dieresis o basura. Descartarlo es correcto. Hacerlo en
# silencio no lo es, y era lo que pasaba: esos valores se volvian NA antes de
# llegar a cualquier diagnostico, y el perfil informaba `n_distintos` sobre los
# que quedaron y `n_faltantes` sin contarlos. Medido sobre una columna Latin-1
# de cinco valores distintos, el perfil decia `n_distintos = 2`,
# `n_faltantes = 0` y cero filas de cobertura.
#
# Lo que R sabe convertir ya se convierte en `.texto_analizable()`. Esto declara
# lo que queda, que es texto de codificacion desconocida.
.cobertura_texto_indescifrable <- function(datos, nombres) {
  filas <- list()
  for (i in seq_along(nombres)) {
    columna <- datos[[i]]
    if (!is.character(columna) && !is.factor(columna)) next
    analizable <- .texto_analizable(columna)
    descartados <- length(analizable$posiciones)
    if (!descartados) next
    presentes <- sum(!is.na(as.character(columna)))
    filas[[length(filas) + 1L]] <- .nuevo_diagnostico_no_evaluado(
      "texto_no_descifrable", nombres[[i]],
      paste0(
        "No se pudieron analizar ", descartados, " de ", presentes,
        " valores presentes: sus bytes no son UTF-8 v\u00e1lidos y la columna no ",
        "declara su codificaci\u00f3n. Quedaron fuera de todo diagn\u00f3stico ",
        "de texto, y por eso no cuentan ni como distintos ni como faltantes."
      ),
      paste(
        "Leer el archivo declarando su codificaci\u00f3n \u2014por ejemplo",
        "`read.csv(..., fileEncoding = \"ISO-8859-1\")`\u2014 o marcarla con",
        "`Encoding(x) <- \"latin1\"`. El texto con codificaci\u00f3n declarada se",
        "convierte y se analiza; lo que no se puede identificar se descarta, y",
        "esta fila es la que lo dice."
      )
    )
  }
  if (!length(filas)) return(NULL)
  do.call(rbind, filas)
}

# Las relaciones de orden son una extension del diagnostico, no una metrica:
# encuentran pares de columnas que parecen compartir una relacion aritmetica
# estable para que el usuario pueda declararla despues en el marco de calidad.
# Un `bigint` de Postgres llega como `integer64`, y la trazabilidad lo rechazaba:
# el hallazgo se publicaba -`outliers`, `valores_no_finitos`- y la guarda tenia
# que avisar que no habia con que nombrar las filas. El conteo salia y la
# localizacion no, sobre un tipo que en una base administrativa es de los mas
# comunes.
#
# Se acepta convirtiendo a doble, y con una condicion: por encima de 2^53 la
# conversion deja de ser exacta y dos valores distintos pueden volverse el
# mismo. Ahi la traza no se entrega a medias -se declara con su motivo-, porque
# una fila mal senalada es peor que una fila sin senalar.
#
# La comprobacion no compara contra 2^53: hace la ida y vuelta a `integer64` y
# mira si el valor volvio igual, que es la unica forma de saberlo sin suponer
# nada del formato. Hubo una constante con el limite y quedo sin usar por esta
# misma decision.

.numerico_trazable <- function(cuantitativos) {
  if (is.null(cuantitativos)) return(NULL)
  clase <- cuantitativos$clase
  if (clase %in% c("numero", "fecha", "fecha-hora")) {
    return(suppressWarnings(as.numeric(cuantitativos$valores)))
  }
  if (!identical(clase, "integer64")) return(NULL)
  originales <- cuantitativos$valores
  valores <- suppressWarnings(as.numeric(originales))
  # Comparar el doble ya convertido contra el limite no alcanza, y el caso que
  # lo rompe es exactamente uno: 2^53+1 redondea a 2^53, asi que `abs > 2^53` da
  # falso y dos enteros distintos quedan iguales. La unica comprobacion que no
  # depende de donde caiga el redondeo es la vuelta completa: convertir a doble
  # y de vuelta a entero, y exigir que nada haya cambiado.
  regreso <- tryCatch(bit64::as.integer64(valores), error = function(e) NULL)
  if (is.null(regreso)) return(NULL)
  presentes <- !is.na(originales)
  if (any(presentes & (is.na(regreso) | regreso != originales))) return(NULL)
  valores
}

.tipo_orden_columna <- function(x, fila) {
  if (inherits(x, "Date")) return("fecha")
  if (inherits(x, "POSIXt")) return("fecha-hora")
  if (inherits(x, "integer64")) return(NA_character_)
  if (is.numeric(x)) return("numero")
  tipo <- as.character(fila$tipo_inferido[[1L]])
  if (tipo %in% c("fecha", "fecha-hora", "entero", "doble")) {
    if (tipo %in% c("entero", "doble")) "numero" else tipo
  } else NA_character_
}

.valores_orden_columna <- function(x, fila, formatos) {
  cuantitativos <- tryCatch(
    .valores_cuantitativos(
      x, list(tipo = as.character(fila$tipo_inferido[[1L]])), formatos
    ),
    error = function(e) NULL
  )
  .numerico_trazable(cuantitativos)
}

# Solapamiento relativo de los rangos intercuartilicos. Las columnas ya llegan
# convertidas a una escala numerica comun (incluidas Date y POSIXct). Un rango
# de anchura cero se trata explicitamente: dos constantes solo son comparables
# si tienen el mismo valor, y una constante se solapa si cae dentro del rango
# intercuartil de la otra columna.
.solapamiento_iqr_orden <- function(a, b) {
  a <- a[is.finite(a)]
  b <- b[is.finite(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  qa <- stats::quantile(a, c(0.25, 0.75), na.rm = TRUE,
                        names = FALSE, type = 7)
  qb <- stats::quantile(b, c(0.25, 0.75), na.rm = TRUE,
                        names = FALSE, type = 7)
  if (any(!is.finite(c(qa, qb)))) return(NA_real_)
  anchura_a <- qa[[2L]] - qa[[1L]]
  anchura_b <- qb[[2L]] - qb[[1L]]
  if (anchura_a == 0 && anchura_b == 0) {
    return(as.numeric(qa[[1L]] == qb[[1L]]))
  }
  if (anchura_a == 0) {
    return(as.numeric(qa[[1L]] >= qb[[1L]] && qa[[1L]] <= qb[[2L]]))
  }
  if (anchura_b == 0) {
    return(as.numeric(qb[[1L]] >= qa[[1L]] && qb[[1L]] <= qa[[2L]]))
  }
  ancho_comun <- max(0, min(qa[[2L]], qb[[2L]]) -
    max(qa[[1L]], qb[[1L]]))
  as.numeric(ancho_comun / max(anchura_a, anchura_b))
}

# Una brecha con IQR cero aporta evidencia fila a fila aun cuando las bandas
# centrales no se solapan: al menos la mitad central conserva exactamente el
# mismo desplazamiento. Se usa como criterio estricto, sin tolerancia oculta.
.iqr_brecha_orden <- function(a, b) {
  brecha <- b - a
  brecha <- brecha[is.finite(brecha)]
  if (!length(brecha)) return(NA_real_)
  cuartiles <- stats::quantile(
    brecha, c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7
  )
  if (any(!is.finite(cuartiles))) return(NA_real_)
  as.numeric(cuartiles[[2L]] - cuartiles[[1L]])
}

.razon_permutacion_orden <- function(a, b, proporcion) {
  a <- a[is.finite(a)]
  b <- b[is.finite(b)]
  if (!length(a) || !length(b) || !is.finite(proporcion)) return(NA_real_)
  ordenada <- sort(b)
  n <- length(ordenada)
  esperada <- mean((n - findInterval(a, ordenada, left.open = TRUE)) / n)
  if (!is.finite(esperada) || esperada == 0) return(NA_real_)
  as.numeric(proporcion / esperada)
}

.alcance_orden_columnas <- function(nombres, seleccion, max_columnas,
                                    tipos, n_filas, umbral,
                                    umbral_solapamiento = 0.1) {
  grupos <- split(seleccion, tipos[seleccion])
  pares <- if (length(grupos)) {
    sum(vapply(grupos, function(x) choose(length(x), 2L), numeric(1L)))
  } else 0
  grupos_analizados <- split(nombres[seleccion], tipos[seleccion])
  pares_analizados <- if (length(grupos_analizados)) {
    sum(vapply(grupos_analizados, function(x) choose(length(x), 2L), numeric(1L)))
  } else 0
  list(
    filas_evaluadas = as.numeric(n_filas),
    columnas_comparables = nombres[which(!is.na(tipos))],
    columnas_analizadas = nombres[seleccion],
    columnas_omitidas = nombres[setdiff(which(!is.na(tipos)), seleccion)],
    pares_comparables = as.numeric(pares),
    pares_analizados = as.numeric(pares_analizados),
    truncado = length(which(!is.na(tipos))) > max_columnas,
    max_columnas = as.integer(max_columnas),
    umbral_cumplimiento = as.numeric(umbral),
    umbral_solapamiento_iqr = as.numeric(umbral_solapamiento),
    umbral_iqr_brecha = 0,
    pares_descartados_magnitud = 0,
    pares_rescatados_brecha_estable = 0,
    pares_evaluados_orden = 0,
    minimo_filas = 3L
  )
}

# `resultados` estaba en la firma y el cuerpo no lo tocaba: el llamador
# construia y pasaba un objeto que esta funcion habia dejado de consumir. Es la
# senal mas clara de un refactor a medio terminar, y hace creer que hay una
# dependencia que no existe.
.detectar_orden_columnas <- function(datos, columnas,
                                     formatos_fecha, umbral = 0.95,
                                     max_columnas = 20L,
                                     umbral_solapamiento = 0.1) {
  n_columnas <- ncol(datos)
  if (!n_columnas || !nrow(datos)) {
    return(list(
      hallazgos = list(),
      alcance = .alcance_orden_columnas(
        character(), integer(), max_columnas, character(), nrow(datos), umbral,
        umbral_solapamiento
      )
    ))
  }
  tipos <- vapply(seq_len(n_columnas), function(i) {
    .tipo_orden_columna(datos[[i]], columnas[i, , drop = FALSE])
  }, character(1L))
  comparables <- which(!is.na(tipos))
  seleccion <- utils::head(comparables, max_columnas)
  alcance <- .alcance_orden_columnas(
    names(datos), seleccion, max_columnas, tipos, nrow(datos), umbral,
    umbral_solapamiento
  )
  if (length(seleccion) < 2L) return(list(hallazgos = list(), alcance = alcance))

  grupos <- split(seleccion, tipos[seleccion])
  pares <- lapply(grupos, function(indices) {
    if (length(indices) < 2L) return(NULL)
    utils::combn(indices, 2L, simplify = FALSE)
  })
  pares <- unlist(pares, recursive = FALSE)
  if (!length(pares)) return(list(hallazgos = list(), alcance = alcance))

  # La conversion semantica de una columna se hace una sola vez. Sin esta
  # cache, una tabla ancha volveria a parsear cada columna por cada par.
  valores <- lapply(seleccion, function(indice) {
    .valores_orden_columna(
      datos[[indice]], columnas[indice, , drop = FALSE],
      formatos_fecha[[indice]]
    )
  })
  names(valores) <- as.character(seleccion)

  hallazgos <- list()
  for (par in pares) {
    izquierda <- valores[[as.character(par[[1L]])]]
    derecha <- valores[[as.character(par[[2L]])]]
    if (is.null(izquierda) || is.null(derecha) ||
        length(izquierda) != length(derecha)) next
    comparables_fila <- is.finite(izquierda) & is.finite(derecha)
    n_evaluados <- sum(comparables_fila)
    if (n_evaluados < 3L) next
    izquierda <- izquierda[comparables_fila]
    derecha <- derecha[comparables_fila]
    filas <- which(comparables_fila)
    solapamiento <- .solapamiento_iqr_orden(izquierda, derecha)
    iqr_brecha <- .iqr_brecha_orden(izquierda, derecha)
    brecha_estable <- is.finite(iqr_brecha) && iqr_brecha == 0
    magnitudes_separadas <- !is.finite(solapamiento) ||
      solapamiento < umbral_solapamiento
    if (magnitudes_separadas && !brecha_estable) {
      alcance$pares_descartados_magnitud <-
        alcance$pares_descartados_magnitud + 1
      next
    }
    if (magnitudes_separadas && brecha_estable) {
      alcance$pares_rescatados_brecha_estable <-
        alcance$pares_rescatados_brecha_estable + 1
    }
    # Aca hubo una guarda que descartaba el par cuando ambas columnas parecian
    # numeraciones y la brecha entre ellas no era constante. Se saco: **perdia
    # relaciones reales**. Sobre un banco de cinco pares con la respuesta
    # conocida, se llevaba puesto `anio_inicio <= anio_fin` con duracion
    # variable de uno a diez anios -tres violaciones reales sin informar-, que es
    # de las reglas de integridad mas comunes que hay en una base administrativa.
    #
    # La brecha constante, que era la senal para rescatar el par, solo describe
    # `fin = inicio + k`. Una vigencia real dura lo que dura.
    #
    # Y del otro lado no hay nada que ganar: se midio `IQR(brecha) / rango` en
    # los pares reales y en los espurios y no separa -0,167 en el par real,
    # 0,016 en el espurio-. Un par de contadores independientes supera el umbral
    # de cumplimiento por azar segun la muestra: con una semilla da 0,96 y se
    # informa, con otra da 0,91 y no. Contra eso no hay guarda estructural, y
    # cambiar un falso positivo ocasional por perder `inicio <= fin` es un mal
    # negocio.
    alcance$pares_evaluados_orden <- alcance$pares_evaluados_orden + 1
    cumple_izquierda <- izquierda <= derecha
    cumple_derecha <- derecha <= izquierda
    proporcion_izquierda <- mean(cumple_izquierda)
    proporcion_derecha <- mean(cumple_derecha)
    if (proporcion_izquierda == proporcion_derecha) next
    direccion_izquierda <- proporcion_izquierda > proporcion_derecha
    proporcion <- if (direccion_izquierda) {
      proporcion_izquierda
    } else proporcion_derecha
    razon_permutacion <- if (direccion_izquierda) {
      .razon_permutacion_orden(izquierda, derecha, proporcion)
    } else {
      .razon_permutacion_orden(derecha, izquierda, proporcion)
    }
    # Con pocas filas se permite una sola inversion para no perder el caso
    # minimo (2 de 3); desde 20 comparables se exige el umbral medido de 95 %.
    umbral_efectivo <- if (n_evaluados < 20L) {
      (n_evaluados - 1) / n_evaluados
    } else umbral
    if (proporcion < umbral_efectivo || proporcion >= 1) next
    if (direccion_izquierda) {
      primero <- par[[1L]]
      segundo <- par[[2L]]
      incumple <- !cumple_izquierda
    } else {
      primero <- par[[2L]]
      segundo <- par[[1L]]
      incumple <- !cumple_derecha
    }
    indices_incumplen <- filas[incumple]
    ejemplos <- utils::head(indices_incumplen, 5L)
    evidencia_ejemplos <- if (length(ejemplos)) paste(vapply(
      ejemplos,
      function(fila) paste0(
        "fila ", fila, ": ", names(datos)[[primero]], "=",
        .texto_valor(datos[[primero]][fila]), "; ", names(datos)[[segundo]],
        "=", .texto_valor(datos[[segundo]][fila])
      ), character(1L)
    ), collapse = " | ") else "sin ejemplos"
    nombre_primero <- names(datos)[[primero]]
    nombre_segundo <- names(datos)[[segundo]]
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      paste(nombre_primero, nombre_segundo, sep = ","),
      "relacion_orden_columnas", "sospechoso",
      paste0(
        "La relaci\u00f3n ", nombre_primero, " <= ", nombre_segundo,
        " se rompe en una minor\u00eda de las filas comparables."
      ),
      paste0(
        sprintf("%.3f de cumplimiento; %d de %d filas fuera de orden. ",
                proporcion, length(indices_incumplen), n_evaluados),
        sprintf(
          "Solapamiento intercuartil: %.3f; umbral: %.3f. ",
          solapamiento, umbral_solapamiento
        ),
        sprintf(
          "IQR de la brecha: %.3f; criterio alternativo: 0.000. ",
          iqr_brecha
        ),
        "Razon de permutacion descriptiva (observado/esperado al permutar filas): ",
        if (is.finite(razon_permutacion)) {
          sprintf("%.3f", razon_permutacion)
        } else "NA",
        "; no filtra la deteccion. ",
        evidencia_ejemplos
      ),
      paste0(
        "Formalizar la relaci\u00f3n con ReglaIntegridadIntraEntidad(",
        nombre_primero, ",", nombre_segundo,
        ") y revisar las filas se\u00f1aladas antes de corregirlas."
      ),
      n_evaluados, length(indices_incumplen), "fila"
    )
  }
  list(hallazgos = hallazgos, alcance = alcance)
}

.unir_componentes_vocabulario <- function(n, pares) {
  if (n < 2L || !nrow(pares)) return(seq_len(n))
  padre <- seq_len(n)
  rango <- integer(n)
  raiz <- function(x) {
    while (padre[[x]] != x) {
      padre[[x]] <<- padre[[padre[[x]]]]
      x <- padre[[x]]
    }
    x
  }
  unir <- function(a, b) {
    ra <- raiz(a)
    rb <- raiz(b)
    if (ra == rb) return(invisible(NULL))
    if (rango[[ra]] < rango[[rb]]) {
      padre[[ra]] <<- rb
    } else if (rango[[ra]] > rango[[rb]]) {
      padre[[rb]] <<- ra
    } else {
      padre[[rb]] <<- ra
      rango[[ra]] <<- rango[[ra]] + 1L
    }
    invisible(NULL)
  }
  for (i in seq_len(nrow(pares))) unir(pares[i, 1L], pares[i, 2L])
  vapply(seq_len(n), raiz, integer(1L))
}

.normalizar_token_numerico_vocabulario <- function(token) {
  if (grepl("^[0-9]{1,3}(?:[.,][0-9]{3})+$", token, perl = TRUE)) {
    token <- gsub("[.,]", "", token)
  } else if (!grepl("^[0-9]+$", token, perl = TRUE)) {
    return(token)
  }
  token <- sub("^0+", "", token)
  if (!nzchar(token)) "0" else token
}

.firmas_numericas_vocabulario <- function(textos) {
  vapply(textos, function(texto) {
    if (is.na(texto) || !nzchar(texto)) return("")
    posiciones <- gregexpr("[0-9]+(?:[.,][0-9]+)*", texto, perl = TRUE)[[1L]]
    if (posiciones[[1L]] < 0L) return("")
    tokens <- regmatches(texto, list(posiciones))[[1L]]
    tokens <- vapply(tokens, .normalizar_token_numerico_vocabulario,
                     character(1L))
    paste(tokens, collapse = "\u001f")
  }, character(1L), USE.NAMES = FALSE)
}

# Clasifica la forma de la diferencia como evidencia, nunca como filtro. Se
# aplica solo a las aristas que ya sobrevivieron al detector, no al cuadrado
# completo del vocabulario.
# Un token que aparece en buena parte de la columna es parte del FORMATO, no una
# errata. Medido sobre PED/Flight, el detector agrupaba
#
#   [12:00 a.m. (5) / 12:00 p.m. (42)]     <- doce horas de diferencia
#   [7:10 a.m. (100) / 7:10 p.m. (18)]
#
# porque `a.m.` y `p.m.` difieren en una letra. Pero los dos aparecen en casi
# todos los valores de esa columna: la diferencia es una distincion sistematica
# del formato y no una variante de escritura. En cambio
#
#   [1:48 p.m. (27) / 1:48 p.m.  Delayed (1)]
#
# difiere en un token que aparece UNA vez, y ese si es un problema real: es el
# estado del vuelo pegado dentro de una columna de hora.
#
# El discriminante es esa asimetria: si TODOS los tokens que distinguen a dos
# formas son comunes en la columna, la diferencia es del formato.
.token_frecuencias_vocabulario <- function(formas) {
  if (!length(formas)) return(integer())
  tokens <- strsplit(trimws(as.character(formas)), "[[:space:]]+", perl = TRUE)
  tabla <- table(unlist(lapply(tokens, unique), use.names = FALSE))
  stats::setNames(as.integer(tabla), names(tabla))
}

# Por debajo de este vocabulario la proporcion no dice nada: con cinco formas,
# un token que aparece en una ya esta en el 20 %.
.MIN_FORMAS_TOKEN_SISTEMATICO <- 20L

.diferencia_sistematica_vocabulario <- function(a, b, frecuencias, n_formas,
                                                minimo = 0.2) {
  if (!length(frecuencias) || !n_formas) return(FALSE)
  tokens_a <- strsplit(trimws(as.character(a)), "[[:space:]]+", perl = TRUE)[[1L]]
  tokens_b <- strsplit(trimws(as.character(b)), "[[:space:]]+", perl = TRUE)[[1L]]
  # Tres condiciones antes de descartar nada, y las tres salieron de romper la
  # suite con una version que no las tenia (44 pruebas caidas):
  #
  # 1. **Varios tokens.** Si el valor es un solo token, "el token que difiere" es
  #    el valor entero, y entonces la regla borra justamente el caso central del
  #    detector: `Marano` contra `Marebo`. La idea de "marca de formato" solo
  #    existe cuando hay un token que acompana y otro que distingue.
  # 2. **Misma cantidad de tokens.** Cuando cambia -como al pegar "Delayed"- no
  #    se descarta nada, que es justo lo que hay que conservar.
  # 3. **Vocabulario suficiente.** "Aparece en toda la columna" no significa nada
  #    sobre cinco formas: ahi cualquier token pasa el umbral por aritmetica.
  if (length(tokens_a) < 2L) return(FALSE)
  if (length(tokens_a) != length(tokens_b)) return(FALSE)
  if (n_formas < .MIN_FORMAS_TOKEN_SISTEMATICO) return(FALSE)
  distintos <- which(tokens_a != tokens_b)
  if (!length(distintos)) return(FALSE)
  proporcion <- function(token) {
    valor <- frecuencias[[token]]
    if (is.null(valor) || is.na(valor)) 0 else valor / n_formas
  }
  all(vapply(distintos, function(i) {
    proporcion(tokens_a[[i]]) >= minimo && proporcion(tokens_b[[i]]) >= minimo
  }, logical(1L)))
}

# Dos formas de un mismo grupo pueden diferir por dos motivos que piden dos
# arreglos distintos: **la misma palabra escrita de otra manera** -mayusculas,
# acentos, puntuacion- se corrige normalizando la columna entera, y una
# **errata** se corrige en las filas donde aparece.
#
# La distancia de edicion no los separa. Medido sobre 1.718 pares crudo/canonico
# que un humano normalizo a mano en un registro real: las 416 variantes de
# escritura estan a distancia media 2,29 y las 422 erratas a 1,56, asi que
# ordenar por distancia clasifica al reves. Lo que si los separa es si colapsan
# al quitar caja, acentos y puntuacion.
.clave_sin_escritura <- function(x) {
  y <- tryCatch(enc2utf8(as.character(x)), error = function(e) as.character(x))
  y <- tryCatch(
    iconv(y, from = "UTF-8", to = "ASCII//TRANSLIT"),
    error = function(e) y
  )
  y[is.na(y)] <- ""
  gsub("[^[:alnum:]]", "", tolower(y))
}

# De un grupo de formas, cuantas son la dominante escrita de otra manera.
.variantes_de_escritura_vocabulario <- function(formas) {
  if (length(formas) < 2L) return(0L)
  claves <- .clave_sin_escritura(formas)
  sum(claves[-1L] == claves[[1L]] & nzchar(claves[[1L]]))
}

.clase_diferencia_vocabulario <- function(a, b) {
  a <- trimws(as.character(a)); b <- trimws(as.character(b))
  tokens_a <- strsplit(a, "[[:space:]]+", perl = TRUE)[[1L]]
  tokens_b <- strsplit(b, "[[:space:]]+", perl = TRUE)[[1L]]
  if (length(tokens_a) != length(tokens_b)) return("mixta")
  if (length(tokens_a) == 1L) return("token_unico")
  distintos <- which(tokens_a != tokens_b)
  if (!length(distintos)) return("sin_diferencia")
  clases <- vapply(distintos, function(i) {
    izquierda <- tokens_a[[i]]; derecha <- tokens_b[[i]]
    izq <- strsplit(izquierda, "", fixed = TRUE)[[1L]]
    der <- strsplit(derecha, "", fixed = TRUE)[[1L]]
    limite <- min(length(izq), length(der))
    prefijo <- 0L
    while (prefijo < limite && identical(izq[[prefijo + 1L]], der[[prefijo + 1L]])) {
      prefijo <- prefijo + 1L
    }
    sufijo <- 0L
    while (sufijo < (limite - prefijo) &&
           identical(izq[[length(izq) - sufijo]], der[[length(der) - sufijo]])) {
      sufijo <- sufijo + 1L
    }
    if (prefijo >= 2L || sufijo >= 2L ||
        grepl(izquierda, derecha, fixed = TRUE) ||
        grepl(derecha, izquierda, fixed = TRUE)) {
      "dentro_de_palabra"
    } else {
      "token_completo"
    }
  }, character(1L))
  if (all(clases == "dentro_de_palabra")) "dentro_de_palabra" else
    if (all(clases == "token_completo")) "token_completo" else "mixta"
}

.patrones_solo_largo_corrida_numerica <- function(dominante, desvios) {
  forma <- function(x) gsub("9\\+|9+", "<N>", as.character(x), perl = TRUE)
  !is.na(desvios) & forma(desvios) == forma(dominante) &
    as.character(desvios) != as.character(dominante)
}

# Un presupuesto que no se puede leer no ayuda a decidir: 64638000000 sin
# separadores no dice nada. `decimal.mark` va explicito porque por omision es
# el punto y compartirlo con el separador de miles hace que R avise.
.miles_trabajo <- function(x) {
  format(
    round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE
  )
}

.alcance_vocabulario_largo <- function(
    textos, presentes, columna, max_valores, max_pares, metodo, p, umbral,
    max_proporcion_grupo, umbral_variante_rara, min_asimetria_variante,
    min_asimetria_general, min_participacion_dominante,
    max_asimetria_equifrecuente, max_trabajo, n_excluidos_faltantes,
    max_largo_valor) {
  valores <- unique(textos[presentes])
  largos_valores <- nchar(valores, type = "chars", allowNA = TRUE)
  largos_valores <- largos_valores[is.finite(largos_valores)]
  largos_filas <- nchar(textos[presentes], type = "chars", allowNA = TRUE)
  largos_filas <- largos_filas[is.finite(largos_filas)]
  suma_largos <- sum(largos_valores)
  trabajo <- if (length(largos_valores)) {
    (suma_largos^2 - sum(largos_valores^2)) / 2
  } else 0
  n_valores <- length(valores)
  list(
    grupos = list(), grupos_equifrecuentes = list(),
    alcance = list(
      n_valores_distintos = n_valores,
      n_excluidos_faltantes_disfrazados = n_excluidos_faltantes,
      n_valores_evaluados = 0L,
      n_unidades_normalizadas = NA_integer_,
      n_unidades_comparadas = 0L,
      n_unidades_sin_comparar = NA_integer_,
      n_pares_posibles = if (n_valores) choose(n_valores, 2L) else 0,
      n_pares_comparados = 0,
      n_pares_sin_comparar = if (n_valores) choose(n_valores, 2L) else 0,
      truncado = TRUE,
      trabajo_estimado = trabajo,
      trabajo_comparado = 0,
      trabajo_sin_comparar = trabajo,
      unidad_trabajo = "comparaciones de caracter",
      max_trabajo = max_trabajo,
      motivo_presupuesto = "max_largo_valor",
      distancia_disponible = .stringdist_disponible(),
      motivo_distancia = "",
      metodo = metodo, p = p, umbral = umbral,
      max_valores = max_valores, max_pares = max_pares,
      max_proporcion_grupo = max_proporcion_grupo,
      min_valores_limite = 20L, min_tamano_grupo_limite = 10L,
      limite_aplicado = FALSE,
      tamano_grupo_maximo = 0L, proporcion_grupo_maximo = 0,
      tamano_grupo_maximo_numerico = 0L,
      proporcion_grupo_maximo_numerico = 0,
      n_candidatos_distancia = 0L,
      n_candidatos_edicion_corta = 0L,
      n_descartados_frecuencia_edicion_corta = 0L,
      max_largo_edicion_corta = 6L, max_distancia_edicion_corta = 1L,
      umbral_variante_rara = umbral_variante_rara,
      min_asimetria_variante = min_asimetria_variante,
      min_asimetria_general = min_asimetria_general,
      n_grupos_bajo_piso_asimetria = 0L,
      n_grupos_sin_variante_rara = 0L,
      n_pares_equifrecuentes = 0,
      max_asimetria_equifrecuente = max_asimetria_equifrecuente,
      min_participacion_dominante = min_participacion_dominante,
      n_pares_descartados_numeros = 0,
      n_pares_descartados_formato = 0,
      motivo_grupos = "", aplicable = TRUE,
      largo_excedido = TRUE,
      n_valores_largos = sum(largos_filas > max_largo_valor),
      largo_maximo = if (length(largos_filas)) max(largos_filas) else 0,
      max_largo_valor = max_largo_valor,
      columna = columna
    )
  )
}

.grupos_casi_duplicados_vocabulario <- function(x, perfil, columna,
                                                max_valores = 5000L,
                                                max_pares = 2000000L,
                                                metodo = "jw", p = 0.1,
                                                umbral = 0.10, nucleos = 2L,
                                                max_proporcion_grupo = 0.5,
                                                umbral_variante_rara = 0.05,
                                                min_asimetria_variante = 10,
                                                min_asimetria_general = 2,
                                                min_participacion_dominante = 0.5,
                                                excluir = NULL,
                                                detectar_variantes_equifrecuentes = FALSE,
                                                max_asimetria_equifrecuente = 2,
                                                max_trabajo = 2e10,
                                                max_largo_valor =
                                                  .MAX_LARGO_VALOR_CASI_DUPLICADOS) {
  if (!is.numeric(max_trabajo) || length(max_trabajo) != 1L ||
      is.na(max_trabajo) || max_trabajo <= 0 ||
      (!is.infinite(max_trabajo) && max_trabajo != floor(max_trabajo))) {
    stop(
      "`max_trabajo` debe ser un entero positivo o Inf.",
      call. = FALSE
    )
  }
  max_trabajo <- if (is.infinite(max_trabajo)) Inf else as.numeric(max_trabajo)
  max_largo_valor <- .validar_largo_valor_duplicados(
    max_largo_valor, "max_largo_valor_vocabulario"
  )
  if (!(is.character(x) || is.factor(x)) || is.matrix(x) || is.list(x)) {
    return(NULL)
  }
  textos <- suppressWarnings(as.character(.texto_analizable(x)$valores))
  if (is.null(excluir)) excluir <- rep(FALSE, length(textos))
  if (!is.logical(excluir) || length(excluir) != length(textos)) {
    stop("`excluir` debe ser una mascara logica del largo de `x`.", call. = FALSE)
  }
  excluir[is.na(excluir)] <- FALSE
  presentes_originales <- !is.na(textos) & nzchar(textos)
  n_excluidos_faltantes <- sum(presentes_originales & excluir)
  presentes <- presentes_originales & !excluir
  if (!any(presentes)) return(NULL)
  if (is.finite(max_largo_valor)) {
    largos <- nchar(textos[presentes], type = "chars", allowNA = TRUE)
    largos <- largos[is.finite(largos)]
    if (any(largos > max_largo_valor)) {
      return(.alcance_vocabulario_largo(
        textos, presentes, columna, max_valores, max_pares, metodo, p, umbral,
        max_proporcion_grupo, umbral_variante_rara, min_asimetria_variante,
        min_asimetria_general, min_participacion_dominante,
        max_asimetria_equifrecuente, max_trabajo, n_excluidos_faltantes,
        max_largo_valor
      ))
    }
  }
  # El vocabulario se ordena antes de recortar, y no se toma en el orden en que
  # las formas aparecen. Ese detalle decide el resultado: sobre la columna
  # `nombre` de "Ejes de vias de circulacion" de Montevideo -45.400 filas, 8.318
  # formas distintas- las MISMAS filas dan, segun como vengan ordenadas,
  #
  #   orden de llegada     26 grupos       desordenado (3 semillas)  70, 71, 85
  #   orden alfabetico    148 grupos
  #
  # y el paquete promete que el veredicto no depende de la forma fisica de la
  # tabla. Tomar las primeras en aparecer lo hacia depender.
  #
  # Ordenar tiene ademas una razon de fondo y no solo de reproducibilidad: los
  # casi-duplicados quedan **adyacentes** -`CAMINO CARRASCO` junto a `CAMINO
  # AGRARIOS`-, asi que el corte cae entre familias en vez de partirlas. Una
  # muestra al azar rompe pares: si de un grupo de dos sobrevive uno, el grupo
  # desaparece. Por eso el azar rinde 70-85 y el orden rinde 148.
  #
  # `method = "radix"` no es un detalle: el orden por omision depende de la
  # configuracion regional de la maquina, y eso cambiaria el resultado de una
  # computadora a otra. El radix ordena por bytes, igual en todas.
  presentes_texto <- textos[presentes]
  formas <- .ordenar_por_bytes(unique(presentes_texto))
  n_total <- length(formas)
  n_evaluados <- min(n_total, max_valores)
  crudos <- formas[seq_len(n_evaluados)]
  posicion <- match(presentes_texto, crudos)
  frecuencias <- tabulate(posicion[!is.na(posicion)], nbins = n_evaluados)
  perfil_columna <- .normalizacion_para_columna(perfil, columna)
  normalizados <- .normalizacion_aplicar(crudos, perfil_columna)
  clases <- match(normalizados, unique(normalizados))
  n_unidades <- max(clases, 0L)
  valores_norm <- unique(normalizados)
  longitudes <- if (n_unidades) {
    as.numeric(nchar(valores_norm, type = "chars", allowNA = TRUE))
  } else numeric()
  longitudes[is.na(longitudes)] <- 0
  # La unidad es la comparacion de un caracter contra otro, que es el bucle
  # interno de la distancia: comparar dos valores de largos L1 y L2 cuesta del
  # orden de L1 x L2. Contar pares por largo medio -en vez de por el producto-
  # subestima las cadenas largas: medido, el mismo presupuesto compraba 5,3
  # millones de unidades por segundo con valores de 900 caracteres y 44
  # millones con valores de 40. Ocho veces de diferencia es no tener modelo.
  #
  # La suma de L_i x L_j sobre todos los pares de un prefijo sale exacta y en
  # tiempo lineal de ((suma)^2 - suma de cuadrados) / 2, sin materializar la
  # matriz. Y es creciente en k, que es lo que necesita el corte.
  trabajo_prefijo <- if (n_unidades) {
    suma <- cumsum(longitudes)
    (suma^2 - cumsum(longitudes^2)) / 2
  } else numeric()
  # El mismo `split()` se calculaba aqui y otra vez, identico, en el bucle de
  # mas abajo.
  grupos_de_clase <- split(seq_along(clases), clases)
  representantes <- vapply(grupos_de_clase, `[[`, integer(1L), 1L)
  disponible <- .stringdist_disponible()
  firmas_numericas <- if (disponible) {
    .firmas_numericas_vocabulario(valores_norm)
  } else {
    rep("", length(valores_norm))
  }
  # Y el `rbind()` copiaba la matriz entera en cada vuelta. Se arma la lista y
  # se une una sola vez: medido en el tope realista -2.500 clases de dos- da
  # 0,0116 s contra 0,0056 s, con los mismos pares y en el mismo orden.
  con_pares <- Filter(function(g) length(g) > 1L, grupos_de_clase)
  pares <- if (length(con_pares)) {
    # Para unir una clase exacta basta una estrella: no materializamos el
    # cuadrado de pares que ya son iguales por normalizacion.
    do.call(rbind, lapply(con_pares, function(grupo) {
      cbind(grupo[[1L]], grupo[-1L])
    }))
  } else {
    matrix(integer(), ncol = 2L)
  }
  max_unidades_pares <- if (is.infinite(max_pares)) n_unidades else {
    floor((1 + sqrt(1 + 8 * max_pares)) / 2)
  }
  max_unidades_trabajo <- if (is.infinite(max_trabajo)) n_unidades else {
    dentro <- which(trabajo_prefijo <= max_trabajo)
    if (length(dentro)) max(dentro) else 1L
  }
  max_unidades <- min(
    n_unidades, max(1L, max_unidades_pares),
    max(1L, max_unidades_trabajo)
  )
  trabajo_posible <- if (n_unidades) trabajo_prefijo[[n_unidades]] else 0
  trabajo_comparado <- if (max_unidades) {
    trabajo_prefijo[[max_unidades]]
  } else 0
  trabajo_sin_comparar <- max(0, trabajo_posible - trabajo_comparado)
  motivo_presupuesto <- paste(c(
    if (max_unidades_trabajo < n_unidades) "max_trabajo" else character(),
    if (max_unidades_pares < n_unidades) "max_pares" else character()
  ), collapse = "+")
  distancia_pares <- data.frame(
    fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
    origen = character(),
    stringsAsFactors = FALSE
  )
  distancia_pares_sin_filtro <- distancia_pares
  frecuencia_unidad <- as.numeric(tapply(frecuencias, clases, sum))
  n_candidatos_distancia <- 0L
  n_candidatos_edicion_corta <- 0L
  n_descartados_frecuencia_edicion_corta <- 0L
  n_pares_descartados_numeros <- 0L
  n_pares_descartados_formato <- 0L
  hay_firmas_numericas_distintas <- length(unique(firmas_numericas)) > 1L
  frecuencias_token <- .token_frecuencias_vocabulario(crudos)
  n_formas_token <- length(crudos)
  max_asimetria_equifrecuente <- as.numeric(max_asimetria_equifrecuente)
  padre_equifrecuente <- seq_len(n_unidades)
  rango_equifrecuente <- integer(n_unidades)
  activos_equifrecuentes <- logical(n_unidades)
  raiz_equifrecuente <- function(x) {
    while (padre_equifrecuente[[x]] != x) {
      padre_equifrecuente[[x]] <<- padre_equifrecuente[[
        padre_equifrecuente[[x]]
      ]]
      x <- padre_equifrecuente[[x]]
    }
    x
  }
  unir_equifrecuentes <- function(a, b) {
    raiz_a <- raiz_equifrecuente(a)
    raiz_b <- raiz_equifrecuente(b)
    if (raiz_a == raiz_b) return(invisible(NULL))
    if (rango_equifrecuente[[raiz_a]] < rango_equifrecuente[[raiz_b]]) {
      padre_equifrecuente[[raiz_a]] <<- raiz_b
    } else if (rango_equifrecuente[[raiz_a]] > rango_equifrecuente[[raiz_b]]) {
      padre_equifrecuente[[raiz_b]] <<- raiz_a
    } else {
      padre_equifrecuente[[raiz_b]] <<- raiz_a
      rango_equifrecuente[[raiz_a]] <<- rango_equifrecuente[[raiz_a]] + 1L
    }
    invisible(NULL)
  }
  registrar_equifrecuentes <- function(fila_1, fila_2, distancia) {
    if (!length(fila_1)) return(invisible(NULL))
    frecuencias_1 <- frecuencia_unidad[fila_1]
    frecuencias_2 <- frecuencia_unidad[fila_2]
    menor <- pmin(frecuencias_1, frecuencias_2)
    mayor <- pmax(frecuencias_1, frecuencias_2)
    conservar <- is.finite(menor) & menor > 0 &
      mayor / menor <= max_asimetria_equifrecuente
    if (!any(conservar)) return(invisible(NULL))
    fila_1 <- fila_1[conservar]
    fila_2 <- fila_2[conservar]
    distancia <- distancia[conservar]
    n_pares_equifrecuentes <<- n_pares_equifrecuentes + length(fila_1)
    for (i in seq_along(fila_1)) {
      activos_equifrecuentes[[fila_1[[i]]]] <<- TRUE
      activos_equifrecuentes[[fila_2[[i]]]] <<- TRUE
      unir_equifrecuentes(fila_1[[i]], fila_2[[i]])
    }
    if (isTRUE(detectar_variantes_equifrecuentes)) {
      pares_equifrecuentes_1 <<- c(pares_equifrecuentes_1, fila_1)
      pares_equifrecuentes_2 <<- c(pares_equifrecuentes_2, fila_2)
      distancias_equifrecuentes <<- c(distancias_equifrecuentes, distancia)
    }
    invisible(NULL)
  }
  n_pares_equifrecuentes <- 0L
  pares_equifrecuentes_1 <- integer()
  pares_equifrecuentes_2 <- integer()
  distancias_equifrecuentes <- numeric()
  if (disponible && max_unidades > 1L) {
    if (hay_firmas_numericas_distintas) {
      mejor_frecuencia_sin_filtro <- numeric(n_unidades)
      mejor_hub_sin_filtro <- integer(n_unidades)
      mejor_distancia_sin_filtro <- numeric(n_unidades)
      empate_mejor_hub_sin_filtro <- logical(n_unidades)
      mejor_origen_sin_filtro <- rep("distancia", n_unidades)
    }
    mejor_frecuencia <- numeric(n_unidades)
    mejor_hub <- integer(n_unidades)
    mejor_distancia <- numeric(n_unidades)
    empate_mejor_hub <- logical(n_unidades)
    mejor_origen <- rep("distancia", n_unidades)
    actualizar_estrellas <- function(fila_1, fila_2, distancia,
                                     sin_filtro = FALSE,
                                     origen = "distancia") {
      frecuencia <- if (sin_filtro) mejor_frecuencia_sin_filtro else {
        mejor_frecuencia
      }
      hub <- if (sin_filtro) mejor_hub_sin_filtro else mejor_hub
      distancias <- if (sin_filtro) mejor_distancia_sin_filtro else {
        mejor_distancia
      }
      empates <- if (sin_filtro) empate_mejor_hub_sin_filtro else {
        empate_mejor_hub
      }
      origenes <- if (sin_filtro) mejor_origen_sin_filtro else mejor_origen
      for (i in seq_along(fila_1)) {
        primero <- as.integer(fila_1[[i]])
        segundo <- as.integer(fila_2[[i]])
        if (frecuencia_unidad[[primero]] == frecuencia_unidad[[segundo]]) {
          next
        }
        if (frecuencia_unidad[[primero]] < frecuencia_unidad[[segundo]]) {
          bajo <- primero
          alto <- segundo
        } else {
          bajo <- segundo
          alto <- primero
        }
        frecuencia_alta <- frecuencia_unidad[[alto]]
        if (frecuencia_alta > frecuencia[[bajo]]) {
          frecuencia[[bajo]] <- frecuencia_alta
          hub[[bajo]] <- alto
          distancias[[bajo]] <- distancia[[i]]
          origenes[[bajo]] <- origen
          empates[[bajo]] <- FALSE
        } else if (frecuencia_alta == frecuencia[[bajo]]) {
          empates[[bajo]] <- TRUE
        }
      }
      if (sin_filtro) {
        mejor_frecuencia_sin_filtro <<- frecuencia
        mejor_hub_sin_filtro <<- hub
        mejor_distancia_sin_filtro <<- distancias
        empate_mejor_hub_sin_filtro <<- empates
        mejor_origen_sin_filtro <<- origenes
      } else {
        mejor_frecuencia <<- frecuencia
        mejor_hub <<- hub
        mejor_distancia <<- distancias
        empate_mejor_hub <<- empates
        mejor_origen <<- origenes
      }
      invisible(NULL)
    }
    registrar_candidatos <- function(fila_1, fila_2, distancia,
                                     edicion_corta = FALSE) {
      origen <- if (edicion_corta) {
        "distancia_edicion_corta"
      } else {
        "distancia"
      }
      if (edicion_corta) {
        distancias_jw <- .distancias_pares_duplicados(
          valores_norm[fila_1], valores_norm[fila_2], metodo, nucleos, p
        )
        no_cubiertos <- is.finite(distancias_jw) & distancias_jw > umbral
        frecuencia_1 <- frecuencia_unidad[fila_1]
        frecuencia_2 <- frecuencia_unidad[fila_2]
        menor <- pmin(frecuencia_1, frecuencia_2)
        mayor <- pmax(frecuencia_1, frecuencia_2)
        cumple_frecuencia <- menor / sum(presentes) <=
          umbral_variante_rara & mayor / menor >= min_asimetria_variante &
          mayor / sum(presentes) >= min_participacion_dominante
        n_candidatos_edicion_corta <<- n_candidatos_edicion_corta +
          sum(no_cubiertos)
        n_descartados_frecuencia_edicion_corta <<-
          n_descartados_frecuencia_edicion_corta +
          sum(no_cubiertos & !cumple_frecuencia)
        conservar <- no_cubiertos & cumple_frecuencia
        if (!any(conservar)) return(invisible(NULL))
        fila_1 <- fila_1[conservar]
        fila_2 <- fila_2[conservar]
        distancia <- distancia[conservar]
      }
      if (hay_firmas_numericas_distintas) {
        actualizar_estrellas(
          fila_1, fila_2, distancia, sin_filtro = TRUE, origen = origen
        )
      }
      compatibles <- firmas_numericas[fila_1] == firmas_numericas[fila_2]
      n_pares_descartados_numeros <<- n_pares_descartados_numeros +
        sum(!compatibles)
      if (!any(compatibles)) return(invisible(NULL))
      fila_1 <- fila_1[compatibles]
      fila_2 <- fila_2[compatibles]
      distancia <- distancia[compatibles]
      del_formato <- vapply(seq_along(fila_1), function(k) {
        .diferencia_sistematica_vocabulario(
          crudos[[fila_1[[k]]]], crudos[[fila_2[[k]]]],
          frecuencias_token, n_formas_token
        )
      }, logical(1L))
      n_pares_descartados_formato <<- n_pares_descartados_formato +
        sum(del_formato)
      if (all(del_formato)) return(invisible(NULL))
      fila_1 <- fila_1[!del_formato]
      fila_2 <- fila_2[!del_formato]
      distancia <- distancia[!del_formato]
      registrar_equifrecuentes(fila_1, fila_2, distancia)
      n_candidatos_distancia <<- n_candidatos_distancia + length(fila_1)
      actualizar_estrellas(fila_1, fila_2, distancia, origen = origen)
      invisible(NULL)
    }
    .comparar_bloques_duplicados(
      valores_norm[seq_len(max_unidades)], seq_len(max_unidades),
      metodo, umbral, bloque = min(1000L, max_unidades), max_resultados = 1L,
      on_pairs = registrar_candidatos, nucleos = nucleos, p = p
    )
    # Con el umbral Jaro--Winkler vigente, una sustitucion entra siempre desde
    # siete caracteres. La ruta de edicion cubre la zona ciega hasta seis.
    max_largo_edicion_corta <- 6L
    indices_cortos <- which(
      seq_len(n_unidades) <= max_unidades &
        nchar(valores_norm, type = "chars", allowNA = TRUE) <=
          max_largo_edicion_corta
    )
    if (length(indices_cortos) > 1L) {
      .comparar_bloques_duplicados(
        valores_norm[indices_cortos], indices_cortos,
        "lv", 1, bloque = min(1000L, length(indices_cortos)),
        max_resultados = 1L,
        on_pairs = function(fila_1, fila_2, distancia) {
          registrar_candidatos(
            fila_1, fila_2, distancia, edicion_corta = TRUE
          )
        }, nucleos = nucleos, p = p
      )
    }
    aristas_estrellas <- function(mejor_hub, mejor_distancia,
                                  empate_mejor_hub,
                                  origen = rep("distancia", n_unidades)) {
      # Solo sobreviven estrellas centradas en un maximo local unico. Una hoja
      # nunca se conecta a otra hoja ni a un nodo que ya depende de un tercero:
      # asi se evita el cierre transitivo de cadenas.
      hojas <- which(mejor_hub > 0L & !empate_mejor_hub)
      aristas <- hojas[vapply(hojas, function(hoja) {
        hub <- mejor_hub[[hoja]]
        mejor_hub[[hub]] == 0L && !empate_mejor_hub[[hub]]
      }, logical(1L))]
      if (!length(aristas)) return(data.frame(
        fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
        origen = character(),
        stringsAsFactors = FALSE
      ))
      data.frame(
        fila_1 = representantes[mejor_hub[aristas]],
        fila_2 = representantes[aristas],
        distancia = mejor_distancia[aristas],
        origen = origen[aristas],
        stringsAsFactors = FALSE
      )
    }
    distancia_pares <- aristas_estrellas(
      mejor_hub, mejor_distancia, empate_mejor_hub, mejor_origen
    )
    if (hay_firmas_numericas_distintas) {
      distancia_pares_sin_filtro <- aristas_estrellas(
        mejor_hub_sin_filtro, mejor_distancia_sin_filtro,
        empate_mejor_hub_sin_filtro, mejor_origen_sin_filtro
      )
    } else {
      distancia_pares_sin_filtro <- distancia_pares
    }
  }
  todas_las_aristas <- rbind(
    pares,
    if (nrow(distancia_pares)) as.matrix(
      distancia_pares[, c("fila_1", "fila_2"), drop = FALSE]
    )
    else matrix(integer(), ncol = 2L)
  )
  componentes <- .unir_componentes_vocabulario(n_evaluados, todas_las_aristas)
  grupos <- split(seq_len(n_evaluados), componentes)
  grupos <- grupos[lengths(grupos) > 1L]
  tamano_grupo_maximo_numerico <- if (length(grupos)) {
    max(lengths(grupos))
  } else 0L
  proporcion_grupo_maximo_numerico <- if (n_evaluados) {
    tamano_grupo_maximo_numerico / n_evaluados
  } else 0
  aristas_sin_filtro <- rbind(
    pares,
    if (nrow(distancia_pares_sin_filtro)) as.matrix(
      distancia_pares_sin_filtro[, c("fila_1", "fila_2"), drop = FALSE]
    ) else matrix(integer(), ncol = 2L)
  )
  componentes_sin_filtro <- .unir_componentes_vocabulario(
    n_evaluados, aristas_sin_filtro
  )
  grupos_sin_filtro <- split(seq_len(n_evaluados), componentes_sin_filtro)
  grupos_sin_filtro <- grupos_sin_filtro[lengths(grupos_sin_filtro) > 1L]
  tamano_grupo_maximo <- if (length(grupos_sin_filtro)) {
    max(lengths(grupos_sin_filtro))
  } else tamano_grupo_maximo_numerico
  proporcion_grupo_maximo <- if (n_evaluados) {
    tamano_grupo_maximo / n_evaluados
  } else 0
  # Un vocabulario pequeno puede contener un grupo facil de inspeccionar, pero
  # un componente que ya tiene muchas variantes es una senal de que se esta
  # agrupando la columna entera. El limite se activa por cualquiera de esas dos
  # senales y solo suprime si ademas la proporcion es grande.
  min_valores_limite <- 20L
  min_tamano_grupo_limite <- 10L
  limite_aplicado <- n_evaluados >= min_valores_limite ||
    tamano_grupo_maximo >= min_tamano_grupo_limite
  # Las fusiones exactas siguen siendo evidencia util aunque `stringdist` no
  # este instalado; el limite evita solamente entregar grupos enormes cuando
  # el diagnostico de distancia esta disponible y puede haber encadenamiento.
  aplicable <- !limite_aplicado || !disponible ||
    proporcion_grupo_maximo <= max_proporcion_grupo
  if (!aplicable) grupos <- list()
  if (!length(grupos)) {
    grupos_salida <- list()
  } else {
    grupos_salida <- lapply(grupos, function(indices) {
      frecuencias_grupo <- frecuencias[indices]
      distancias_grupo <- distancia_pares[
        distancia_pares$fila_1 %in% indices &
          distancia_pares$fila_2 %in% indices, "distancia"
      ]
      indices_unidades <- unique(clases[indices])
      exacta <- nrow(pares) > 0L && any(
        pares[, 1L] %in% indices & pares[, 2L] %in% indices
      )
      por_distancia <- length(distancias_grupo) > 0L
      aristas_grupo <- distancia_pares[
        distancia_pares$fila_1 %in% indices &
          distancia_pares$fila_2 %in% indices, , drop = FALSE
      ]
      usa_edicion_corta <- por_distancia &&
        any(aristas_grupo$origen == "distancia_edicion_corta")
      frecuencias_minoritarias <- frecuencias_grupo[
        frecuencias_grupo < max(frecuencias_grupo)
      ]
      distancia_total <- if (por_distancia &&
          length(indices_unidades) > 1L) {
        matriz <- as.matrix(.matriz_distancias_duplicados(
          valores_norm[indices_unidades], valores_norm[indices_unidades],
          metodo, nucleos, p
        ))
        valores <- matriz[upper.tri(matriz)]
        valores[is.finite(valores)]
      } else numeric()
      list(
        variantes = crudos[indices], frecuencias = frecuencias_grupo,
        asimetria = max(frecuencias_grupo) / min(frecuencias_grupo),
        asimetria_minima = if (length(frecuencias_minoritarias)) {
          max(frecuencias_grupo) / max(frecuencias_minoritarias)
        } else NA_real_,
        participacion_variante_minoritaria_maxima = if (
          length(frecuencias_minoritarias)
        ) {
          max(frecuencias_minoritarias) / sum(presentes)
        } else NA_real_,
        usa_edicion_corta = usa_edicion_corta,
        distancia_minima = if (length(distancia_total)) {
          min(distancia_total)
        } else NA_real_,
        distancia_maxima = if (length(distancia_total)) {
          max(distancia_total)
        } else NA_real_,
        clase_diferencia = if (!por_distancia) {
          "normalizacion_exacta"
        } else {
          clases_aristas <- vapply(seq_len(nrow(aristas_grupo)), function(j) {
            .clase_diferencia_vocabulario(
              crudos[[aristas_grupo$fila_1[[j]]]],
              crudos[[aristas_grupo$fila_2[[j]]]]
            )
          }, character(1L))
          if (!length(clases_aristas)) "indeterminada" else
            if (all(clases_aristas == "dentro_de_palabra")) {
              "dentro_de_palabra"
            } else if (all(clases_aristas == "token_completo")) {
              "token_completo"
            } else if (all(clases_aristas == "token_unico")) {
              "token_unico"
            } else "mixta"
        },
        origen = paste(c(
          if (exacta) "normalizacion" else character(),
          if (por_distancia && !usa_edicion_corta) "distancia" else character(),
          if (usa_edicion_corta) "distancia_edicion_corta" else character()
        ), collapse = "+")
      )
    })
    # Piso de asimetria, y SOLO para los grupos que se formaron por distancia.
    #
    # Sin el, cualquier par dentro del umbral se abre con cualquier desbalance, y
    # una asimetria de 1,5 es evidencia muy floja de una errata: significa que
    # las dos formas son casi igual de comunes. Medido sobre tablas limpias y
    # sobre erratas sembradas, la separacion es limpia:
    #
    #   falsos positivos (este/oeste)            asimetria 1,0 a 1,5
    #   erratas reales (Montevideo/Montevido)    asimetria 9,0 a 66,7
    #
    # Pero el piso NO puede aplicarse a los grupos que se formaron porque su
    # forma normalizada coincide. `Montevideo`, `MONTEVIDEO` y `Montevideo ` son
    # tres grafias del mismo valor, y con una aparicion cada una su asimetria es
    # 1,0: es una deteccion real y aplicarle el piso la mataria. Ahi la
    # diferencia no es una conjetura sobre una errata, es una equivalencia
    # comprobada.
    #
    # Y el costo de este piso, que hay que declarar en vez de esconder: una
    # errata SISTEMATICA que afecta a la mitad de los registros tiene asimetria
    # cercana a 1 y deja de informarse. `Montevideo` cinco veces contra
    # `Montevido` cinco veces es un caso real -dos operadores, una plantilla
    # rota- y es mas grave que el falso positivo que el piso evita. Por eso los
    # grupos descartados por el piso no desaparecen: se cuentan y el alcance los
    # declara.
    conserva <- vapply(grupos_salida, function(grupo) {
      solo_por_distancia <- !grepl("normalizacion", grupo$origen, fixed = TRUE)
      if (!solo_por_distancia) return(TRUE)
      asimetria <- grupo$asimetria
      !is.finite(asimetria) || asimetria >= min_asimetria_general
    }, logical(1L))
    n_grupos_bajo_piso <- sum(!conserva)
    grupos_salida <- grupos_salida[conserva]
  }
  if (length(grupos_salida)) {
    orden <- order(-vapply(grupos_salida, `[[`, numeric(1L), "asimetria"),
                   seq_along(grupos_salida))
    grupos_salida <- grupos_salida[orden]
  }
  if (!exists("n_grupos_bajo_piso", inherits = FALSE)) n_grupos_bajo_piso <- 0L
  componentes_equifrecuentes <- if (n_unidades) {
    vapply(seq_len(n_unidades), raiz_equifrecuente, integer(1L))
  } else integer()
  grupos_equifrecuentes_candidatos <- if (length(activos_equifrecuentes)) {
    candidatos <- split(
      which(activos_equifrecuentes),
      componentes_equifrecuentes[activos_equifrecuentes]
    )
    Filter(function(indices) {
      frecuencias_grupo <- frecuencias[indices]
      length(indices) > 1L &&
        !any(indices %in% as.integer(pares)) &&
        max(frecuencias_grupo) / min(frecuencias_grupo) <=
          max_asimetria_equifrecuente
    }, candidatos)
  } else list()
  grupos_equifrecuentes <- list()
  if (isTRUE(detectar_variantes_equifrecuentes) &&
      length(grupos_equifrecuentes_candidatos) && aplicable) {
    grupos_equifrecuentes <- lapply(
      grupos_equifrecuentes_candidatos,
      function(indices) {
        aristas <- which(
          pares_equifrecuentes_1 %in% indices &
            pares_equifrecuentes_2 %in% indices
        )
        frecuencias_grupo <- frecuencias[indices]
        distancias_grupo <- distancias_equifrecuentes[aristas]
        clases_aristas <- vapply(aristas, function(j) {
          .clase_diferencia_vocabulario(
            crudos[[pares_equifrecuentes_1[[j]]]],
            crudos[[pares_equifrecuentes_2[[j]]]]
          )
        }, character(1L))
        clase <- if (!length(clases_aristas)) "indeterminada" else
          if (all(clases_aristas == "dentro_de_palabra")) {
            "dentro_de_palabra"
          } else if (all(clases_aristas == "token_completo")) {
            "token_completo"
          } else if (all(clases_aristas == "token_unico")) {
            "token_unico"
          } else "mixta"
        list(
          variantes = crudos[indices], frecuencias = frecuencias_grupo,
          asimetria = max(frecuencias_grupo) / min(frecuencias_grupo),
          distancia_minima = if (length(distancias_grupo)) {
            min(distancias_grupo)
          } else NA_real_,
          distancia_maxima = if (length(distancias_grupo)) {
            max(distancias_grupo)
          } else NA_real_,
          clase_diferencia = clase,
          origen = "distancia_equifrecuente"
        )
      }
    )
  }
  list(
    grupos = grupos_salida,
    grupos_equifrecuentes = grupos_equifrecuentes,
    alcance = list(
      n_valores_distintos = n_total,
      n_excluidos_faltantes_disfrazados = n_excluidos_faltantes,
      n_valores_evaluados = n_evaluados,
      n_unidades_normalizadas = n_unidades,
      n_unidades_comparadas = if (disponible) max_unidades else 0L,
       n_unidades_sin_comparar = max(
         0L, n_unidades - if (disponible) max_unidades else 0L
       ),
      n_pares_posibles = if (n_unidades) choose(n_unidades, 2L) else 0,
      n_pares_comparados = if (disponible && max_unidades > 1L) {
        choose(max_unidades, 2L)
      } else 0,
      n_pares_sin_comparar = (if (n_unidades) choose(n_unidades, 2L) else 0) -
        (if (disponible && max_unidades > 1L) choose(max_unidades, 2L) else 0),
      truncado = n_evaluados < n_total || max_unidades < n_unidades,
       trabajo_estimado = trabajo_posible,
       trabajo_comparado = if (disponible) trabajo_comparado else 0,
       trabajo_sin_comparar = if (disponible) trabajo_sin_comparar else
         trabajo_posible,
      unidad_trabajo = "comparaciones de caracter",
      max_trabajo = max_trabajo,
      max_largo_valor = max_largo_valor,
      largo_excedido = FALSE,
      n_valores_largos = 0L,
      largo_maximo = .largo_maximo_valor_duplicados(presentes_texto),
      motivo_presupuesto = motivo_presupuesto,
      distancia_disponible = disponible,
      motivo_distancia = if (disponible) "" else
        "No se calculo la distancia: falta el paquete opcional 'stringdist'.",
      metodo = metodo, p = p, umbral = umbral,
      max_valores = max_valores, max_pares = max_pares,
      max_proporcion_grupo = max_proporcion_grupo,
      min_valores_limite = min_valores_limite,
      min_tamano_grupo_limite = min_tamano_grupo_limite,
      limite_aplicado = limite_aplicado,
      tamano_grupo_maximo = tamano_grupo_maximo,
      proporcion_grupo_maximo = proporcion_grupo_maximo,
      tamano_grupo_maximo_numerico = tamano_grupo_maximo_numerico,
      proporcion_grupo_maximo_numerico = proporcion_grupo_maximo_numerico,
      n_candidatos_distancia = n_candidatos_distancia,
      n_candidatos_edicion_corta = n_candidatos_edicion_corta,
      n_descartados_frecuencia_edicion_corta =
        n_descartados_frecuencia_edicion_corta,
      max_largo_edicion_corta = 6L,
      max_distancia_edicion_corta = 1L,
      umbral_variante_rara = umbral_variante_rara,
      min_asimetria_variante = min_asimetria_variante,
      min_asimetria_general = min_asimetria_general,
      n_grupos_bajo_piso_asimetria = as.integer(n_grupos_bajo_piso),
      n_grupos_sin_variante_rara = as.integer(
        length(grupos_equifrecuentes_candidatos)
      ),
      n_pares_equifrecuentes = as.numeric(n_pares_equifrecuentes),
      max_asimetria_equifrecuente = max_asimetria_equifrecuente,
      min_participacion_dominante = min_participacion_dominante,
      n_pares_descartados_numeros = n_pares_descartados_numeros,
      n_pares_descartados_formato = n_pares_descartados_formato,
      motivo_grupos = if (disponible && n_candidatos_distancia > 0L &&
          !nrow(distancia_pares)) "sin_asimetria" else "",
      aplicable = aplicable
    )
  )
}

.hallazgos_casi_duplicados_vocabulario <- function(
    datos, columnas, perfil, resultados = NULL, max_proporcion_grupo = 0.5,
    umbral_variante_rara = 0.05, min_asimetria_variante = 10,
    min_asimetria_general = 2, min_participacion_dominante = 0.5,
    detectar_variantes_equifrecuentes = FALSE,
    max_asimetria_equifrecuente = 2, max_trabajo = 2e10,
    max_largo_valor = .MAX_LARGO_VALOR_CASI_DUPLICADOS) {
  max_grupos_mostrados <- 20L
  max_variantes_mostradas <- 20L
  hallazgos <- list()
  cobertura <- list()
  for (i in seq_along(datos)) {
    excluir <- if (!is.null(resultados) && length(resultados) >= i &&
        !is.null(resultados[[i]]$faltantes_disfrazados$mascara)) {
      resultados[[i]]$faltantes_disfrazados$mascara
    } else NULL
    grupos <- .grupos_casi_duplicados_vocabulario(
      datos[[i]], perfil, columnas[[i]],
      max_proporcion_grupo = max_proporcion_grupo,
      umbral_variante_rara = umbral_variante_rara,
      min_asimetria_variante = min_asimetria_variante,
      min_asimetria_general = min_asimetria_general,
      min_participacion_dominante = min_participacion_dominante,
      detectar_variantes_equifrecuentes = detectar_variantes_equifrecuentes,
      max_asimetria_equifrecuente = max_asimetria_equifrecuente,
      max_trabajo = max_trabajo,
      max_largo_valor = max_largo_valor,
      excluir = excluir
    )
    if (!is.null(grupos) && !isTRUE(grupos$alcance$distancia_disponible)) {
      cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        "No se pudo evaluar la proximidad del vocabulario: falta el paquete opcional 'stringdist'.",
        "Instalar el paquete 'stringdist' para comparar valores cercanos del vocabulario.",
        "stringdist"
      )
    }
    if (is.null(grupos)) next
    alcance <- grupos$alcance
    if (isTRUE(alcance$largo_excedido)) {
      cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        paste0(
          "No se evaluo la proximidad del vocabulario: ",
          alcance$n_valores_largos,
          " valores superan `max_largo_valor_vocabulario = ",
          alcance$max_largo_valor, "` caracteres; el largo maximo observado es ",
          alcance$largo_maximo,
          ". La columna completa quedo fuera de la comparacion para no usar",
          " una distancia de edicion que no discrimina bien textos largos."
        ),
        paste0(
          "Subir `max_largo_valor_vocabulario` si la columna contiene valores",
          " que deben compararse, o usar `Inf` explicitamente para recuperar",
          " el comportamiento anterior sin tope."
        )
      )
      next
    }
    if (isTRUE(alcance$truncado)) {
      cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        # "Unidades" nombraba dos cosas distintas en la misma frase -formas
        # normalizadas y unidades de trabajo-, y quedaban una al lado de la
        # otra. Las formas se llaman formas.
        paste0(
          "El vocabulario excede el alcance de comparacion: se evaluaron ",
          .miles_trabajo(alcance$n_valores_evaluados), " de ",
          .miles_trabajo(alcance$n_valores_distintos),
          " valores distintos, que son ",
          .miles_trabajo(alcance$n_unidades_normalizadas),
          " formas normalizadas, y se comparo entre si a las ",
          .miles_trabajo(alcance$n_unidades_comparadas),
          " primeras en orden alfabetico. Eso son ",
          .miles_trabajo(alcance$n_pares_comparados), " de ",
          .miles_trabajo(alcance$n_pares_posibles), " pares posibles y ",
          .miles_trabajo(alcance$trabajo_comparado), " de ",
          .miles_trabajo(alcance$trabajo_estimado),
          " ", alcance$unidad_trabajo, ". Quedaron ",
          .miles_trabajo(alcance$n_unidades_sin_comparar), " formas y ",
          .miles_trabajo(alcance$n_pares_sin_comparar),
          " pares sin comparar por ", alcance$motivo_presupuesto, "."
        ),
        # Decir cuantas formas quedaron afuera sin decir cuales son deja
        # suponer que se comparo una muestra. No lo es: el corte toma un tramo
        # del alfabeto, asi que lo que queda afuera son las formas que empiezan
        # mas adelante, no un subconjunto cualquiera.
        paste0(
          "Aumentar `max_trabajo_vocabulario` o `max_pares` si se necesita ",
          "cubrir mas valores; tambien se puede dividir el vocabulario en ",
          "subconjuntos con significado comun. Las formas comparadas son las ",
          "primeras en orden alfabetico, no una muestra: lo que queda afuera es ",
          "el tramo final del alfabeto. El resultado no depende del orden de ",
          "las filas."
        )
      )
    }
    # Los grupos que el piso de asimetria descarto no desaparecen: se declaran.
    # Un error sistematico que afecta al 40 % de los registros tiene asimetria
    # 1,5 y cae bajo el piso; el piso existe porque `este`/`oeste` tambien cae
    # ahi, y por la forma son indistinguibles. Lo honesto no es elegir cual
    # sacrificar en silencio, es decir cuantos quedaron afuera.
    if (isTRUE(alcance$n_grupos_bajo_piso_asimetria > 0L)) {
      cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        paste0(
          alcance$n_grupos_bajo_piso_asimetria,
          if (alcance$n_grupos_bajo_piso_asimetria == 1L) {
            " grupo de valores cercanos no se informo"
          } else " grupos de valores cercanos no se informaron",
          " porque su asimetria de frecuencias quedo por debajo de ",
          alcance$min_asimetria_general,
          ": las formas son casi igual de comunes y no se puede distinguir una",
          " errata sistematica de dos valores legitimamente parecidos."
        ),
        paste0(
          "Bajar `min_asimetria_vocabulario` si en esta columna interesan las",
          " variantes de frecuencia pareja, sabiendo que aumenta el ruido."
        )
      )
    }
    if (isTRUE(alcance$n_grupos_sin_variante_rara > 0L) &&
        isTRUE(alcance$aplicable)) {
      nueva_cobertura <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        paste0(
          alcance$n_grupos_sin_variante_rara,
          if (alcance$n_grupos_sin_variante_rara == 1L) {
            " grupo de formas cercanas no se formo"
          } else " grupos de formas cercanas no se formaron",
          " porque sus frecuencias fueron parecidas: el criterio de variante",
          " rara no pudo distinguir una forma correcta de otra.",
          " Se identificaron ", alcance$n_pares_equifrecuentes,
          " pares cercanos con asimetria <= ",
          formatC(alcance$max_asimetria_equifrecuente,
                  format = "f", digits = 1), "."
        ),
        paste0(
          "Activar un detector de formas equifrecuentes con conocimiento del ",
          "dominio, o revisar los pares manualmente; no se puede elegir una ",
          "forma correcta solo por su frecuencia."
        )
      )
      indice_cobertura <- which(vapply(cobertura, function(x) {
        identical(x$diagnostico[[1L]], "proximidad_vocabulario") &&
          identical(x$columna[[1L]], columnas[[i]])
      }, logical(1L)))
      if (length(indice_cobertura)) {
        indice_cobertura <- indice_cobertura[[1L]]
        cobertura[[indice_cobertura]]$motivo <- paste(
          cobertura[[indice_cobertura]]$motivo,
          nueva_cobertura$motivo, sep = " "
        )
        cobertura[[indice_cobertura]]$como_resolverlo <- paste(
          cobertura[[indice_cobertura]]$como_resolverlo,
          nueva_cobertura$como_resolverlo, sep = " "
        )
      } else {
        cobertura[[length(cobertura) + 1L]] <- nueva_cobertura
      }
    }
    if (!isTRUE(alcance$aplicable)) {
      cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
        "proximidad_vocabulario", columnas[[i]],
        paste0(
          "El grupo candidato mayor abarca ",
          formatC(alcance$proporcion_grupo_maximo,
                  format = "f", digits = 3),
          " del vocabulario y el diagnostico no aplica."
        ),
        paste0(
          "Usar una regla de dominio especifica para la columna o ajustar ",
          "el criterio de agrupacion con conocimiento del vocabulario."
        )
      )
    }
    hay_grupos <- length(grupos$grupos) > 0L
    hay_grupos_equifrecuentes <- length(grupos$grupos_equifrecuentes) > 0L
    resultado_negativo <- !hay_grupos &&
      isTRUE(alcance$distancia_disponible) &&
      !isTRUE(alcance$truncado) && isTRUE(alcance$aplicable) &&
       (identical(alcance$motivo_grupos, "sin_asimetria") ||
        isTRUE(alcance$n_pares_descartados_numeros > 0L))
    if (!hay_grupos && !resultado_negativo && !hay_grupos_equifrecuentes) next
    if (hay_grupos || resultado_negativo) {
    grupos_a_mostrar <- utils::head(grupos$grupos, max_grupos_mostrados)
    evidencia_grupos <- vapply(grupos_a_mostrar, function(grupo) {
      indices <- seq_len(min(length(grupo$variantes), max_variantes_mostradas))
      variantes <- paste(vapply(indices, function(j) {
        paste0(.escapar_texto_visible(grupo$variantes[[j]]), " (",
               grupo$frecuencias[[j]], ")")
      }, character(1L)), collapse = " / ")
      if (length(grupo$variantes) > max_variantes_mostradas) {
        variantes <- paste0(
          variantes, " / ... ", length(grupo$variantes) -
            max_variantes_mostradas, " variantes no mostradas"
        )
      }
      distancia <- if (is.finite(grupo$distancia_minima)) {
        paste0("; distancia_minima=", formatC(
          grupo$distancia_minima, format = "f", digits = 4
        ), "; distancia_maxima=", formatC(
          grupo$distancia_maxima, format = "f", digits = 4
        ))
      } else ""
      criterio_corto <- if (isTRUE(grupo$usa_edicion_corta)) {
        paste0(
          "; asimetria_minima=",
          formatC(grupo$asimetria_minima, format = "f", digits = 1),
          "; participacion_variante_minoritaria_maxima=",
          formatC(
            grupo$participacion_variante_minoritaria_maxima,
            format = "f", digits = 3
          )
        )
      } else ""
      # Cuantas de las formas del grupo son la dominante escrita de otra
      # manera. Sin esto, el grupo mezcla variantes de escritura con erratas y
      # el usuario no sabe si el arreglo es normalizar la columna o corregir
      # filas.
      n_escritura <- .variantes_de_escritura_vocabulario(grupo$variantes)
      escritura <- if (n_escritura > 0L) {
        # Se dice QUE SE COMPARO, no que sean la misma palabra. La comparacion
        # ignora acentos, y en espanol el acento distingue palabras: `papa` y
        # `papa` con tilde colapsa con `papa`, igual que `ano` con tilde y `ano`,
        # o `mas` con tilde y `mas`. El mismo mecanismo que junta `Jose` con
        # `Jose` con tilde -que si es la misma- junta esas otras, y no hay forma
        # de separarlas sin un diccionario del idioma.
        #
        # Afirmar "es la misma palabra escrita distinto" seria decir lo que no
        # se sabe. Informar en que difieren es un hecho verificable, y le deja
        # la lectura a quien conoce la columna.
        paste0(
          "; ", n_escritura, " de ", length(grupo$variantes) - 1L,
          " variantes difieren de la forma dominante solo en mayusculas, ",
          "acentos o puntuacion (en espa\u00f1ol el acento puede cambiar la ",
          "palabra: conviene mirarlas antes de unificar)"
        )
      } else ""
      paste0("[", variantes, "]; asimetria=",
             formatC(grupo$asimetria, format = "f", digits = 1),
             "; origen=", grupo$origen,
             "; clase_diferencia=", grupo$clase_diferencia, distancia,
             escritura, criterio_corto)
    }, character(1L))
    grupos_texto <- if (length(evidencia_grupos)) {
      paste(evidencia_grupos, collapse = "; ")
    } else if (!isTRUE(alcance$aplicable) &&
               isTRUE(alcance$tamano_grupo_maximo_numerico > 0L)) {
      "No se entrega el grupo mayor: excede el limite de proporcion"
    } else if (isTRUE(alcance$n_pares_descartados_numeros > 0L)) {
      paste0(
        "No se formaron grupos por distancia: ",
        alcance$n_pares_descartados_numeros,
        " pares cercanos se descartaron por secuencias numericas incompatibles"
      )
    } else if (identical(alcance$motivo_grupos, "sin_asimetria")) {
      paste0(
        "No se formaron grupos por distancia: ",
        alcance$n_candidatos_distancia,
        " pares cercanos no tuvieron un centro de frecuencia unico"
      )
    } else if (!isTRUE(alcance$distancia_disponible)) {
      alcance$motivo_distancia
    } else {
      "No se enumeraron grupos dentro del alcance comparado"
    }
    hay_distancia <- length(grupos$grupos) > 0L && any(vapply(
      grupos$grupos,
      function(grupo) grepl("distancia", grupo$origen, fixed = TRUE),
      logical(1L)
    ))
    descripcion_grupos <- if (hay_distancia) {
      "Se detectaron valores cercanos; la distancia es heur\u00edstica y no confirma identidad."
    } else {
      "Hay grupos cuya forma normalizada coincide; eso no confirma que sean la misma entidad."
    }
    severidad <- if (hay_grupos) "sospechoso" else "ok"
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      columnas[[i]], "casi_duplicados_vocabulario", severidad,
      if (length(evidencia_grupos)) {
        descripcion_grupos
      } else if (isTRUE(alcance$n_pares_descartados_numeros > 0L)) {
        "No se formaron grupos porque las diferencias numericas se consideran entidades distintas; revisar con una regla especifica si la columna usa otra codificacion."
      } else if (identical(alcance$motivo_grupos, "sin_asimetria")) {
        "No se formaron grupos por distancia porque no hubo asimetria de frecuencia; usar detectar_duplicados_aproximados() para comparar filas."
      } else {
        descripcion_grupos
      },
      paste0(
        grupos_texto,
        "; alcance: ", alcance$n_valores_evaluados, " de ",
        alcance$n_valores_distintos, " valores; ",
        alcance$n_pares_comparados, " pares comparados de ",
        alcance$n_pares_posibles, "; truncado=", alcance$truncado,
        "; unidades normalizadas: ", alcance$n_unidades_comparadas, " de ",
        alcance$n_unidades_normalizadas, "; grupos: ", length(grupos$grupos),
        ", mostrados: ", length(grupos_a_mostrar), "; grupo_maximo: ",
        alcance$tamano_grupo_maximo, " (",
        formatC(alcance$proporcion_grupo_maximo, format = "f", digits = 3),
        "); limite_aplicado=", alcance$limite_aplicado,
        "; valores_excluidos_faltantes_disfrazados=",
        alcance$n_excluidos_faltantes_disfrazados,
        "; motivo_grupos=", alcance$motivo_grupos, ". ",
        "pares descartados por secuencia numerica=",
        alcance$n_pares_descartados_numeros, "; grupo_maximo compatible=",
        alcance$tamano_grupo_maximo_numerico, " (",
        formatC(alcance$proporcion_grupo_maximo_numerico,
                format = "f", digits = 3), "). ",
        alcance$motivo_distancia,
        " criterio_edicion_corta: distancia_edicion<=",
        alcance$max_distancia_edicion_corta,
        "; largo<=", alcance$max_largo_edicion_corta,
        "; participacion_variante<=",
        formatC(alcance$umbral_variante_rara, format = "f", digits = 3),
        "; asimetria>=",
        formatC(alcance$min_asimetria_variante, format = "f", digits = 1),
        "; participacion_dominante>=",
        formatC(
          alcance$min_participacion_dominante, format = "f", digits = 3
        ),
        "; candidatos=", alcance$n_candidatos_edicion_corta,
        "; descartados_por_frecuencia=",
        alcance$n_descartados_frecuencia_edicion_corta, "."
      ),
      if (isTRUE(alcance$aplicable)) {
        if (identical(alcance$motivo_grupos, "sin_asimetria")) {
          "Usar detectar_duplicados_aproximados() para comparar filas cuando no hay una frecuencia central."
        } else {
          paste0(
            "Revisar las variantes y declarar una normalizaci\u00f3n o regla de remediaci\u00f3n editable",
            if (hay_distancia) "; la distancia no confirma identidad." else "."
          )
        }
      } else {
        "Revisar la columna con un alcance o criterio de vocabulario m\u00e1s espec\u00edfico."
      },
      alcance$n_valores_evaluados,
      if (hay_grupos) {
        sum(vapply(grupos$grupos, function(g) length(g$variantes), integer(1L)))
      } else 0,
      "valor_distinto"
    )
    if (hay_grupos) {
      textos <- tryCatch(
        suppressWarnings(as.character(.texto_analizable(datos[[i]])$valores)),
        error = function(e) NULL
      )
      particion_grupos <- lapply(grupos$grupos, function(grupo) {
        dominante <- which.max(grupo$frecuencias)
        list(
          variantes = grupo$variantes[-dominante],
          dominantes = grupo$variantes[dominante]
        )
      })
      variantes <- unique(c(
        unlist(lapply(particion_grupos, `[[`, "variantes"),
               use.names = FALSE),
        unlist(lapply(particion_grupos, `[[`, "dominantes"),
               use.names = FALSE)
      ))
      valores_variantes <- unique(unlist(lapply(
        particion_grupos, `[[`, "variantes"
      ), use.names = FALSE))
      valores_dominantes <- unique(unlist(lapply(
        particion_grupos, `[[`, "dominantes"
      ), use.names = FALSE))
      # Un barrido de la columna por cada variante costaba k veces el largo de
      # la columna. Medido sobre 500.000 filas y 200 variantes: 0,965 s contra
      # 0,018 s con un solo barrido, o sea 54 veces. El paquete ya usa esta
      # forma en el detector de unidades, unas lineas mas abajo.
      #
      # El conjunto de indices es el mismo; cambia el orden DENTRO de cada
      # grupo, que pasa de agrupado-por-valor a orden de fila. Los dos grupos
      # siguen separados y concatenados en el mismo orden, que es de lo que
      # depende `.limitar_trazabilidad()` para partir la traza. Cuando la traza
      # se trunca cambian las filas de ejemplo que se muestran, y mostrar las
      # primeras filas donde aparece es mas util para ir a verificar que
      # mostrarlas agrupadas por valor.
      indices_por_valor <- function(valores) {
        if (is.null(textos) || !length(valores)) return(integer())
        which(!is.na(textos) & textos %in% valores)
      }
      indices_variantes <- indices_por_valor(valores_variantes)
      indices_dominantes <- indices_por_valor(valores_dominantes)
      indices <- c(indices_variantes, indices_dominantes)
      indices_unidades <- if (!is.null(textos)) {
        match(variantes, textos)
      } else integer()
      indices_unidades <- as.integer(indices_unidades[!is.na(indices_unidades)])
      traza <- .trazabilidad_indices(indices, "completo", limite = Inf)
      traza$indices_unidades <- indices_unidades
      traza$n_filas_formas_variantes <- length(indices_variantes)
      traza$n_filas_formas_dominantes <- length(indices_dominantes)
      hallazgo <- hallazgos[[length(hallazgos)]]
      hallazgo$trazabilidad <- I(list(traza))
      hallazgos[[length(hallazgos)]] <- hallazgo
    }
    }
    if (hay_grupos_equifrecuentes) {
      grupos_a_mostrar <- utils::head(
        grupos$grupos_equifrecuentes, max_grupos_mostrados
      )
      evidencia_grupos <- vapply(grupos_a_mostrar, function(grupo) {
        indices <- seq_len(min(length(grupo$variantes), max_variantes_mostradas))
        variantes <- paste(vapply(indices, function(j) {
          paste0(.escapar_texto_visible(grupo$variantes[[j]]), " (",
                 grupo$frecuencias[[j]], ")")
        }, character(1L)), collapse = " / ")
        distancia <- if (is.finite(grupo$distancia_minima)) {
          paste0("; distancia_minima=", formatC(
            grupo$distancia_minima, format = "f", digits = 4
          ), "; distancia_maxima=", formatC(
            grupo$distancia_maxima, format = "f", digits = 4
          ))
        } else ""
        paste0(
          "[", variantes, "]; asimetria=",
          formatC(grupo$asimetria, format = "f", digits = 1),
          "; origen=", grupo$origen,
          "; clase_diferencia=", grupo$clase_diferencia, distancia
        )
      }, character(1L))
      valores <- unique(unlist(lapply(
        grupos$grupos_equifrecuentes, `[[`, "variantes"
      ), use.names = FALSE))
      textos <- tryCatch(
        suppressWarnings(as.character(.texto_analizable(datos[[i]])$valores)),
        error = function(e) NULL
      )
      indices <- if (is.null(textos)) integer() else unlist(lapply(
        valores, function(valor) which(!is.na(textos) & textos == valor)
      ), use.names = FALSE)
      indices_unidades <- if (is.null(textos)) integer() else as.integer(
        # `match` es O(n) sobre la columna entera y se pagaba dos veces. La
        # rama hermana, unas lineas mas arriba, ya lo hacia con uno solo.
        { posiciones <- match(valores, textos)
          posiciones[!is.na(posiciones)] }
      )
      traza <- .trazabilidad_indices(indices, "completo", limite = Inf)
      traza$indices_unidades <- indices_unidades
      traza$n_filas_formas_variantes <- length(indices)
      traza$n_filas_formas_dominantes <- 0L
      nuevo <- .nuevo_hallazgo(
        columnas[[i]], "variantes_equifrecuentes_vocabulario", "sospechoso",
        "Dos formas cercanas se reparten la columna; ninguna evidencia dice cual es la correcta.",
        paste(
          paste(evidencia_grupos, collapse = "; "),
          "; alcance: ", alcance$n_valores_evaluados, " de ",
          alcance$n_valores_distintos, " valores; ",
          length(grupos$grupos_equifrecuentes),
          " grupos; max_asimetria_equifrecuente=",
          formatC(alcance$max_asimetria_equifrecuente,
                  format = "f", digits = 1), sep = ""
        ),
        "Revisar el dominio y declarar una forma canonica solo con evidencia externa; la frecuencia no decide entre las formas.",
        alcance$n_valores_evaluados,
        sum(vapply(
          grupos$grupos_equifrecuentes, function(g) length(g$variantes),
          integer(1L)
        )),
        "valor_distinto"
      )
      nuevo$trazabilidad <- I(list(traza))
      hallazgos[[length(hallazgos) + 1L]] <- nuevo
    }
  }
  salida <- hallazgos
  attr(salida, "cobertura_diagnosticos") <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_diagnosticos_vacia()
  }
  salida
}

.conteo_hallazgo_columna <- function(tipo, fila, resultado, n_validos) {
  n <- if (length(fila$n) && is.finite(fila$n[[1L]])) {
    as.numeric(fila$n[[1L]])
  } else NA_real_
  afectados <- switch(
    tipo,
    # Las filas donde esta el valor centinela: se contaron al detectarlo, asi
    # que el hallazgo puede decir a cuantas alcanza en vez de dejarlo en NA.
    posible_centinela_numerico = {
      repeticiones <- suppressWarnings(as.numeric(fila$centinela_repeticiones))
      if (length(repeticiones) && isTRUE(is.finite(repeticiones))) {
        repeticiones
      } else NA_real_
    },
    tipo_compuesto_no_analizado = if (!is.null(resultado$estructura_no_analizada)) {
      as.numeric(resultado$estructura_no_analizada$filas)
    } else NA_real_,
    constante = {
      # El hallazgo solo se emite cuando n_distintos == 1, asi que el unico
      # valor ocupa todas las filas validas: la frecuencia es conocible por
      # definicion aunque no se haya podido contar sobre la columna.
      frecuencia <- as.numeric(fila$frecuencia_moda)
      if (length(frecuencia) && isTRUE(is.finite(frecuencia))) {
        frecuencia
      } else {
        as.numeric(n_validos)
      }
    },
    casi_clave = if (!is.null(resultado$casi_clave)) {
      as.numeric(resultado$casi_clave$n_filas_en_colision)
    } else NA_real_,
    faltantes = as.numeric(fila$n_faltantes + fila$n_faltantes_disfrazados),
    valor_fuera_de_aplicabilidad =
      as.numeric(fila$n_presentes_fuera_de_aplicabilidad),
    faltantes_disfrazados = as.numeric(fila$n_faltantes_disfrazados),
    espacios_sobrantes = as.numeric(fila$n_espacios_borde),
    controles_invisibles = as.numeric(fila$n_controles_invisibles),
    entidades_html = as.numeric(fila$n_entidades_html),
    separadores_en_campo = as.numeric(fila$n_separadores_en_campo),
    mayusculas_inconsistentes = as.numeric(fila$n_variantes_mayusculas),
    normalizacion_unicode = as.numeric(fila$n_variantes_unicode),
    codificacion_invalida = as.numeric(fila$n_codificacion_invalida),
    codificacion_rota = as.numeric(fila$n_codificacion_rota),
    valores_no_finitos = as.numeric(
      fila$n_nan + fila$n_infinito_positivo + fila$n_infinito_negativo
    ),
    ceros_no_permitidos = as.numeric(fila$n_ceros),
    negativos_no_permitidos = as.numeric(fila$n_negativos),
    outliers = as.numeric(fila$n_outliers),
    celdas_multivaluadas = if (length(resultado$multivaluados)) {
      sum(vapply(resultado$multivaluados, `[[`, numeric(1L), "n_celdas"))
    } else 0,
    numero_como_texto = as.numeric(fila$n_numeros_texto),
    unidades_mixtas = if (!is.null(resultado$numeros_texto$unidades)) {
      sum(resultado$numeros_texto$unidades)
    } else NA_real_,
    monedas_mixtas = if (!is.null(resultado$numeros_texto$monedas)) {
      sum(resultado$numeros_texto$monedas)
    } else NA_real_,
    zona_horaria_fecha_hora = as.numeric(fila$n_filas_fecha_civil_distinta_utc),
    geometria_invalida = as.numeric(fila$n_geometrias_invalidas),
    geometria_vacia = as.numeric(fila$n_geometrias_vacias),
    coordenada_fuera_dominio = as.numeric(fila$n_fuera_de_dominio),
    tipos_geometria_mixtos = n,
    patron_raro = {
      completo <- !isTRUE(.patrones_raros_recortados(resultado$patrones))
      desvios <- attr(resultado$patrones, "desvios_patron_raro",
                      exact = TRUE)
      if (completo && is.data.frame(desvios) && nrow(desvios) &&
          "n" %in% names(desvios)) {
        sum(desvios$n, na.rm = TRUE)
      } else NA_real_
    },
    NA_real_
  )
  if (tipo %in% c("mayusculas_inconsistentes", "normalizacion_unicode")) {
    n_distintos <- fila$n_distintos[[1L]]
    n <- if (length(n_distintos) && isTRUE(is.finite(n_distintos))) {
      as.numeric(n_distintos)
    } else {
      NA_real_
    }
  }
  unidad <- if (tipo %in% c(
    "formato_fecha_ambiguo", "anio_de_dos_digitos", "formatos_fecha_mixtos"
  )) {
    "formato"
  } else if (tipo %in% c(
    "mayusculas_inconsistentes", "normalizacion_unicode"
  )) {
    "valor_distinto"
  } else if (tipo %in% c(
    "posible_identificador", "alta_cardinalidad", "tipo_declarado_distinto",
    "integer64_fuera_precision_double", "integer64_sin_soporte",
    "crs_no_declarado"
  )) {
    "columna"
  } else if (tipo %in% c(
    "geometria_invalida", "geometria_vacia", "coordenada_fuera_dominio",
    "tipos_geometria_mixtos"
  )) {
    "geometria"
  } else "fila"
  if (unidad == "formato") {
    n <- if (!is.null(resultado$formatos)) nrow(resultado$formatos) else NA_real_
    afectados <- if (tipo == "formato_fecha_ambiguo") {
      sum(resultado$formatos$estado == "candidato")
    } else if (tipo == "anio_de_dos_digitos") {
      sum(resultado$formatos$anio_dos_digitos)
    } else {
      nrow(resultado$formatos)
    }
  } else if (unidad == "columna") {
    n <- 1
    afectados <- 1
  } else if (tipo == "patron_raro") {
    analizados <- attr(resultado$patrones, "analizados", exact = TRUE)
    if (!is.null(analizados) && is.finite(analizados)) {
      n <- as.numeric(analizados)
    }
  } else if (tipo == "geometria_invalida") {
    n <- as.numeric(resultado$geometria$n_validez_evaluados)
  } else if (tipo == "coordenada_fuera_dominio") {
    n <- as.numeric(resultado$geometria$n_dominio_evaluados)
  }
  # Cuando se declaro un universo aplicable, el denominador de un diagnostico
  # que cuenta filas es ese universo y no la tabla entera: informar "1 de 1000"
  # sobre una columna cuyo universo declarado es 300 contradice la propia
  # declaracion. Solo alcanza a la unidad `fila`; los diagnosticos que cuentan
  # valores distintos ya operan sobre los presentes, que son aplicables por
  # construccion.
  if (identical(unidad, "fila") && !is.null(fila$n_aplicables) &&
      isTRUE(is.finite(fila$n_aplicables[[1L]])) &&
      isTRUE(is.finite(n)) && isTRUE(n == fila$n[[1L]]) &&
      fila$n_aplicables[[1L]] < fila$n[[1L]]) {
    n <- as.numeric(fila$n_aplicables[[1L]])
  }
  list(n_evaluados = n, n_afectados = afectados, unidad_conteo = unidad)
}

# Un patron es una secuencia de clases con su multiplicidad: `a+9+@a.a+` son
# letras, digitos, arroba, letra, punto y letras, con `+` donde hay una corrida.
# Esto separa las dos clases de desvio: el que cambia la ESTRUCTURA -otra
# secuencia de clases- del que solo cambia el LARGO de una corrida, como
# `persona9@` frente a `persona300@`.
#
# La distincion no cambia la severidad: esta medido y decidido que el caso
# legitimo y el sospechoso son indistinguibles por la forma -un correo con
# numero corto es normal, una cedula de un digito no lo es-. Se declara para que
# quien lea el hallazgo lo resuelva de un vistazo en vez de comparar patrones a
# ojo.
.tokens_patron_raro <- function(patron) {
  if (!length(patron) || is.na(patron) || !nzchar(patron)) return(NULL)
  caracteres <- strsplit(patron, "", fixed = TRUE)[[1L]]
  clases <- character(0)
  multiples <- logical(0)
  i <- 1L
  while (i <= length(caracteres)) {
    mas <- i < length(caracteres) && identical(caracteres[[i + 1L]], "+")
    clases <- c(clases, caracteres[[i]])
    multiples <- c(multiples, mas)
    i <- i + if (mas) 2L else 1L
  }
  list(clases = clases, multiples = multiples)
}

.desvio_solo_por_largo <- function(dominante, desvio) {
  a <- .tokens_patron_raro(dominante)
  b <- .tokens_patron_raro(desvio)
  if (is.null(a) || is.null(b)) return(FALSE)
  identical(a$clases, b$clases) && !identical(a$multiples, b$multiples)
}

.clase_desvio_patron_raro <- function(dominante, desvios) {
  if (!length(desvios)) return(NA_character_)
  solo_largo <- vapply(
    desvios, .desvio_solo_por_largo, logical(1L), dominante = dominante
  )
  if (all(solo_largo)) {
    "largo_de_corrida"
  } else if (any(solo_largo)) {
    "mixto"
  } else {
    "estructural"
  }
}

# El resumen cuantitativo y los indices deben operar sobre la misma vista. En
# particular, un texto declarado como tal puede ser numerico por inferencia.
.valores_cuantitativos_hallazgo <- function(x, fila, resultado) {
  tipo <- tryCatch(as.character(fila$tipo_inferido[[1L]]),
                   error = function(e) NULL)
  if (is.null(tipo) || !length(tipo) || is.na(tipo)) return(NULL)
  valores <- tryCatch(.texto_analizable(x)$valores,
                      error = function(e) NULL)
  if (is.null(valores)) return(NULL)
  tryCatch(
    .valores_cuantitativos(
      valores, list(tipo = tipo), resultado$formatos
    ),
    error = function(e) NULL
  )
}

.indices_patron_raro <- function(x, resultado, expandir = FALSE,
                                 distinguir_mayusculas = TRUE) {
  desvios <- attr(resultado$patrones, "desvios_patron_raro", exact = TRUE)
  raros <- if (is.data.frame(desvios) && "patron" %in% names(desvios)) {
    as.character(desvios$patron)
  } else if (is.character(desvios)) {
    desvios
  } else character()
  if (!length(raros)) return(NULL)
  total <- length(x)
  analizados <- attr(resultado$patrones, "analizados")
  muestreado <- isTRUE(attr(resultado$patrones, "muestreado"))
  base <- if (muestreado && is.finite(analizados)) {
    unique(as.integer(round(seq.int(1, total, length.out = analizados))))
  } else {
    seq_len(total)
  }
  valores <- tryCatch(.texto_analizable(x[base])$valores,
                      error = function(e) NULL)
  if (is.null(valores)) return(NULL)
  patrones <- as.character(valores)
  validos <- !is.na(patrones)
  # La misma generalizacion que usa `descubrir_patrones()`, y por la misma
  # funcion: estas dos comparan sus resultados unas lineas mas abajo, asi que
  # tenerlas escritas por separado era una divergencia esperando pasar.
  patrones[validos] <- .generalizar_a_patron(
    patrones[validos], distinguir_mayusculas, expandir
  )
  # The detector already chose `raros`; this branch only maps that decision to
  # row positions and never applies a second rarity criterion.
  base[which(!is.na(patrones) & patrones %in% unique(raros))]
}

.indices_hallazgo_columna <- function(tipo, x, fila, resultado,
                                      expandir = FALSE,
                                      distinguir_mayusculas = TRUE) {
  ## Las filas donde esta el valor centinela. Se conoce cual es -el perfil lo
  ## midio- asi que decir cuantas son y no cuales seria contarlas sin
  ## nombrarlas, que es la incoherencia que la guarda de trazabilidad persigue.
  ## En una columna protegida el valor viaja como NA y no hay indices que dar,
  ## que es correcto: nombrar las filas seria senalar los valores.
  if (identical(tipo, "posible_centinela_numerico")) {
    valor <- suppressWarnings(as.numeric(fila$centinela_valor))
    if (!length(valor) || !isTRUE(is.finite(valor))) return(NULL)
    numeros <- suppressWarnings(as.numeric(x))
    return(which(!is.na(numeros) & numeros == valor))
  }
  ## Vale para cualquier tipo de columna: las filas con valor fuera del universo
  ## aplicable son las que la propia mascara declarada senala.
  if (identical(tipo, "valor_fuera_de_aplicabilidad")) {
    aplicable <- resultado$aplicable
    if (is.null(aplicable)) return(NULL)
    return(which(!is.na(x) & !aplicable))
  }
  # La condicion es que el analisis de geometria haya dejado sus indices, no que
  # la columna sea un objeto `sfc`. Estaba al reves, y eso descartaba una traza
  # que el paquete ya tenia calculada.
  #
  # Importa por como llegan las geometrias de una base: por DBI vienen como texto
  # WKT o como blob WKB, no como `sfc`. Con `inherits(x, "sfc")` el `switch` no
  # corria, `idx` quedaba NULL y la traza salia `no_disponible` -avisando de una
  # incoherencia que no existia, porque los indices estaban ahi-. Contra una tabla
  # PostGIS real eso produjo
  # `coordenada_fuera_dominio en geom: traza no disponible`.
  #
  # Los indices son posiciones de fila: valen igual este la geometria como `sfc`,
  # como texto o como binario. Mirar la clase de la columna era mirar lo que no
  # decidia.
  geometria <- resultado$geometria
  if (is.list(geometria)) {
    idx <- switch(
      tipo,
      geometria_invalida = geometria$indices_invalidas,
      geometria_vacia = geometria$indices_vacias,
      coordenada_fuera_dominio = geometria$indices_fuera_de_dominio,
      tipos_geometria_mixtos = seq_along(x),
      NULL
    )
    if (!is.null(idx)) return(as.integer(idx))
  }
  if (is.matrix(x)) {
    if (tipo == "tipo_compuesto_no_analizado") return(seq_len(NROW(x)))
    return(NULL)
  }
  if (is.list(x)) {
    ## Una columna de lista no admite analisis de texto, pero sus ausencias si
    ## son conocibles: `is.na()` las identifica elemento a elemento, y es el
    ## mismo criterio con el que se contaron. Contarlas sin nombrarlas era la
    ## unica incoherencia que la guarda seguia encontrando sobre estas columnas.
    if (identical(tipo, "faltantes")) return(which(is.na(x)))
    ## Y por el mismo criterio, en una columna constante las filas afectadas son
    ## todas las no ausentes: el unico valor distinto las ocupa por definicion.
    ## Contarlas sin nombrarlas era la incoherencia que quedaba.
    if (identical(tipo, "constante")) return(which(!is.na(x)))
    return(NULL)
  }
  n <- length(x)
  if (!n) return(integer())
  texto <- tryCatch(.texto_analizable(x)$valores, error = function(e) NULL)
  cuantitativos <- NULL
  cuantitativos_evaluados <- FALSE
  obtener_cuantitativos <- function() {
    if (!cuantitativos_evaluados) {
      cuantitativos <<- .valores_cuantitativos_hallazgo(
        x, fila, resultado
      )
      cuantitativos_evaluados <<- TRUE
    }
    cuantitativos
  }
  invisibles <- NULL
  obtener_invisibles <- function() {
    if (is.null(invisibles) && !is.null(texto) && is.character(texto)) {
      vocabulario <- .vocabulario_texto(texto, .umbral_vocabulario_barato)
      invisibles <<- .predicados_invisibles(vocabulario$valores)
      if (isTRUE(vocabulario$usar)) {
        invisibles <<- lapply(invisibles, function(mascara) {
          resultado <- mascara[vocabulario$indices]
          resultado[is.na(resultado)] <- FALSE
          resultado
        })
      }
    }
    invisibles
  }
  idx <- switch(
    tipo,
    tipo_compuesto_no_analizado = seq_len(n),
    constante = tryCatch(
      which(!is.na(x) & as.character(x) == as.character(fila$moda[[1L]])),
      error = function(e) NULL
    ),
    casi_clave = if (!is.null(resultado$casi_clave)) {
      resultado$casi_clave$indices
    } else NULL,
    faltantes = {
      mascara <- tryCatch(resultado$faltantes_disfrazados$mascara,
                          error = function(e) NULL)
      if (is.null(mascara) || length(mascara) != n) NULL else
        which(is.na(x) | mascara)
    },
    faltantes_disfrazados = {
      mascara <- tryCatch(resultado$faltantes_disfrazados$mascara,
                          error = function(e) NULL)
      if (is.null(mascara) || length(mascara) != n) NULL else which(mascara)
    },
    espacios_sobrantes = if (is.null(texto)) NULL else
      which(!is.na(texto) & texto != trimws(texto)),
    controles_invisibles = {
      invisibles <- obtener_invisibles()
      if (is.null(invisibles)) NULL else which(invisibles$control)
    },
    entidades_html = if (is.null(texto)) NULL else
      which(.entidades_html_en_texto(texto)),
    separadores_en_campo = {
      invisibles <- obtener_invisibles()
      if (is.null(invisibles)) NULL else which(invisibles$separador)
    },
    mayusculas_inconsistentes = if (is.null(texto)) NULL else {
      presentes <- !is.na(texto)
      canon <- tolower(texto)
      grupos <- split(seq_len(n)[presentes], canon[presentes])
      unlist(lapply(grupos, function(g) {
        if (length(unique(texto[g])) > 1L) g else integer()
      }), use.names = FALSE)
    },
    normalizacion_unicode = if (is.null(texto) ||
      !requireNamespace("stringi", quietly = TRUE)) NULL else {
        presentes <- !is.na(texto)
        normal <- stringi::stri_trans_nfc(texto[presentes])
        grupos <- split(which(presentes), normal)
        unlist(lapply(grupos, function(g) {
          if (length(unique(texto[g])) > 1L) g else integer()
        }), use.names = FALSE)
      },
    codificacion_invalida = tryCatch(
      .texto_analizable(x)$posiciones, error = function(e) NULL
    ),
    codificacion_rota = if (is.null(texto)) NULL else {
      cod <- tryCatch(.analizar_codificacion_vocabulario(texto), error = function(e) NULL)
      if (is.null(cod) || is.null(cod$afectados)) NULL else
        which(cod$afectados)
    },
    numero_como_texto = if (is.null(texto)) NULL else {
      partes <- tryCatch(.componentes_numero_texto_optimizado(texto),
                         error = function(e) NULL)
      if (is.null(partes)) NULL else which(
        !is.na(texto) & nzchar(texto) & partes$compatible & partes$especial
      )
    },
    unidades_mixtas = if (is.null(texto)) NULL else {
      partes <- tryCatch(.componentes_numero_texto_optimizado(texto),
                         error = function(e) NULL)
      if (is.null(partes)) NULL else which(
        !is.na(texto) & nzchar(texto) & partes$compatible &
          nzchar(partes$unidad)
      )
    },
    monedas_mixtas = if (is.null(texto)) NULL else {
      partes <- tryCatch(.componentes_numero_texto_optimizado(texto),
                         error = function(e) NULL)
      if (is.null(partes)) NULL else which(
        !is.na(texto) & nzchar(texto) & partes$compatible &
          nzchar(partes$moneda)
      )
    },
    celdas_multivaluadas = if (is.null(resultado$multivaluados)) NULL else {
      unlist(lapply(resultado$multivaluados, `[[`, "indices"),
             use.names = FALSE)
    },
    zona_horaria_fecha_hora = if (inherits(x, "POSIXt")) {
      presentes <- !is.na(x)
      zona <- .zona_horaria_origen(x)
      if (identical(zona, "sin_declarar")) return(NULL)
      origen <- format(x, "%Y-%m-%d", tz = zona)
      utc <- format(x, "%Y-%m-%d", tz = "UTC")
      which(presentes & origen != utc)
    } else NULL,
    valores_no_finitos = {
      valores <- .numerico_trazable(obtener_cuantitativos())
      if (is.null(valores)) {
        NULL
      } else {
        which(is.nan(valores) | is.infinite(valores))
      }
    },
    ceros_no_permitidos = {
      cuantitativos <- obtener_cuantitativos()
      if (is.null(cuantitativos) || !identical(cuantitativos$clase, "numero")) {
        NULL
      } else {
        valores <- cuantitativos$valores
        which(is.finite(valores) & valores == 0)
      }
    },
    negativos_no_permitidos = {
      cuantitativos <- obtener_cuantitativos()
      if (is.null(cuantitativos) || !identical(cuantitativos$clase, "numero")) {
        NULL
      } else {
        valores <- cuantitativos$valores
        which(is.finite(valores) & valores < 0)
      }
    },
    outliers = {
      valores_traza <- .numerico_trazable(obtener_cuantitativos())
      if (is.null(valores_traza)) {
        NULL
      } else {
        tryCatch({
          valores <- valores_traza
          finitos <- is.finite(valores)
          valores_finitos <- valores[finitos]
          if (length(valores_finitos) < 4L) integer() else {
            q <- stats::quantile(valores_finitos, c(0.25, 0.75), names = FALSE)
            iqr <- q[[2L]] - q[[1L]]
            which(finitos & (
              valores < q[[1L]] - 1.5 * iqr |
                valores > q[[2L]] + 1.5 * iqr
            ))
          }
        }, error = function(e) NULL)
      }
    },
    patron_raro = .indices_patron_raro(
      x, resultado, expandir, distinguir_mayusculas
    ),
    NULL
  )
  if (is.null(idx)) return(NULL)
  as.integer(idx)
}

.trazabilidad_hallazgo <- function(tipo, hallazgo, datos, nombres,
                                   resultados, expandir, aproximados,
                                   indice_hallazgo, tipos_hallazgos, limite,
                                   distinguir_mayusculas = TRUE,
                                  clave = NULL) {
  unidad <- as.character(hallazgo$unidad_conteo[[1L]])
  if (unidad %in% c("columna", "formato")) {
    return(.trazabilidad_vacia("no_aplica", alcance = "no_aplica", limite = limite))
  }
  trazabilidad_precalculada <- hallazgo$trazabilidad[[1L]]
  if (tipo %in% c(
      "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
    ) &&
      is.list(trazabilidad_precalculada) &&
      !identical(trazabilidad_precalculada$estado, "no_disponible")) {
    return(.limitar_trazabilidad(
      trazabilidad_precalculada, limite = limite, clave = clave, datos = datos
    ))
  }
  if (tipo %in% c(
      "duplicados_aproximados", "duplicados_exactos_columnas",
      "duplicados_exactos_normalizados"
    )) {
    if (is.null(aproximados) || !nrow(aproximados$pares)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    anteriores <- seq_len(indice_hallazgo - 1L)
    ocurrencia <- 1L + sum(
      tipos_hallazgos[anteriores] == tipo
    )
    tipo_par <- switch(
      tipo,
      duplicados_exactos_columnas = "exacto",
      duplicados_exactos_normalizados = "exacto_normalizado",
      "aproximado"
    )
    indices_pares <- which(aproximados$pares$tipo_par == tipo_par)
    if (ocurrencia > length(indices_pares)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    pares <- aproximados$pares[indices_pares[[ocurrencia]], , drop = FALSE]
    parcial <- aproximados$alcance$n_pares_comparados[[1L]] <
      aproximados$alcance$n_pares_posibles[[1L]]
    return(.trazabilidad_indices(
      c(pares$fila_1[[1L]], pares$fila_2[[1L]]),
      if (parcial) "comparacion_parcial" else "comparacion_completa", limite,
      clave = clave, datos = datos
    ))
  }
  if (tipo == "filas_duplicadas") {
    indices <- which(
      base::duplicated.data.frame(datos) |
        base::duplicated.data.frame(datos, fromLast = TRUE)
    )
    return(.trazabilidad_indices(
      indices, "completo", limite, clave = clave, datos = datos
    ))
  }
  # Un hallazgo sobre la clave sin las filas que repiten obliga a buscarlas a
  # mano, que es justo lo que la trazabilidad existe para evitar.
  if (tipo == "clave_no_unica") {
    if (is.null(clave) || !length(clave) || !all(clave %in% names(datos))) {
      return(.trazabilidad_vacia(limite = limite))
    }
    valores <- .seleccionar_columnas(datos, clave)
    indices <- which(
      base::duplicated.data.frame(valores) |
        base::duplicated.data.frame(valores, fromLast = TRUE)
    )
    return(.trazabilidad_indices(
      indices, "completo", limite, clave = clave, datos = datos
    ))
  }
  if (tipo == "relacion_orden_columnas") {
    nombres_par <- strsplit(
      as.character(hallazgo$columna[[1L]]), ",", fixed = TRUE
    )[[1L]]
    if (length(nombres_par) != 2L) {
      return(.trazabilidad_vacia(limite = limite))
    }
    indices_columnas <- match(trimws(nombres_par), nombres)
    if (anyNA(indices_columnas)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    izquierda <- tryCatch(
      .valores_orden_columna(
        datos[[indices_columnas[[1L]]]],
        resultados[[indices_columnas[[1L]]]]$fila,
        resultados[[indices_columnas[[1L]]]]$formatos
      ),
      error = function(e) NULL
    )
    derecha <- tryCatch(
      .valores_orden_columna(
        datos[[indices_columnas[[2L]]]],
        resultados[[indices_columnas[[2L]]]]$fila,
        resultados[[indices_columnas[[2L]]]]$formatos
      ),
      error = function(e) NULL
    )
    if (is.null(izquierda) || is.null(derecha) ||
        length(izquierda) != length(derecha)) {
      return(.trazabilidad_vacia(limite = limite))
    }
    comparables <- is.finite(izquierda) & is.finite(derecha)
    indices <- which(comparables & izquierda > derecha)
    return(.trazabilidad_indices(
      indices, "completo", limite, clave = clave, datos = datos
    ))
  }
  if (tipo == "relacion_aritmetica_columnas") {
    trazabilidad <- hallazgo$trazabilidad[[1L]]
    if (is.list(trazabilidad) &&
        !identical(trazabilidad$estado, "no_disponible")) {
      return(.limitar_trazabilidad(
        trazabilidad, limite = limite, clave = clave, datos = datos
      ))
    }
    return(.trazabilidad_vacia(limite = limite))
  }
  if (tipo == "bloqueo_por_con_perdida") {
    return(.trazabilidad_vacia(
      estado = "no_disponible",
      total = hallazgo$n_afectados[[1L]],
      alcance = "bloqueo_fuera_de_alcance", limite = limite
    ))
  }
  nombre <- as.character(hallazgo$columna[[1L]])
  indice <- match(nombre, nombres)
  if (is.na(indice)) return(.trazabilidad_vacia(limite = limite))
  indices <- .indices_hallazgo_columna(
    tipo, datos[[indice]], resultados[[indice]]$fila,
    resultados[[indice]], expandir, distinguir_mayusculas
  )
  # Si se declaro un universo aplicable, la traza no puede nombrar filas que
  # quedaron fuera: el conteo ya las excluye y nombrarlas igual produce la
  # incoherencia que la guarda detecta con razon. El hallazgo del valor fuera de
  # aplicabilidad es la excepcion, porque justamente vive ahi.
  aplicable <- resultados[[indice]]$aplicable
  if (!is.null(indices) && !is.null(aplicable) && !all(aplicable) &&
      !identical(tipo, "valor_fuera_de_aplicabilidad")) {
    indices <- indices[aplicable[indices]]
  }
  if (tipo == "patron_raro") {
    alcance <- .alcance_patron_raro(resultados[[indice]]$patrones)
    traza <- if (is.null(indices)) {
      .trazabilidad_vacia(alcance = alcance, limite = limite)
    } else {
      .trazabilidad_indices(
        indices, alcance, limite, clave = clave, datos = datos
      )
    }
    patrones <- resultados[[indice]]$patrones
    traza$n_patrones_raros <- attr(
      patrones, "n_patrones_raros", exact = TRUE
    )
    traza$n_patrones_raros_trazabilidad <- length(attr(
      patrones, "patrones_raros_trazabilidad", exact = TRUE
    ))
    traza$limite_patrones_raros_trazabilidad <- attr(
      patrones, "limite_patrones_raros_trazabilidad", exact = TRUE
    )
    return(traza)
  }
  if (is.null(indices)) {
    return(.trazabilidad_vacia(limite = limite))
  }
  .trazabilidad_indices(
    indices, "completo", limite, clave = clave, datos = datos
  )
}

.agregar_trazabilidad_hallazgos <- function(
    hallazgos, datos, nombres, resultados, expandir = FALSE,
    aproximados = NULL, limite = 1000L, distinguir_mayusculas = TRUE,
    clave = NULL) {
  if (!nrow(hallazgos)) {
    hallazgos$trazabilidad <- I(list())
    return(hallazgos)
  }
  tipos_hallazgos <- as.character(hallazgos$tipo_hallazgo)
  trazabilidad <- lapply(seq_len(nrow(hallazgos)), function(i) {
    .trazabilidad_hallazgo(
      as.character(hallazgos$tipo_hallazgo[[i]]), hallazgos[i, , drop = FALSE],
      datos, nombres, resultados, expandir, aproximados, i,
      tipos_hallazgos, limite, distinguir_mayusculas, clave = clave
    )
  })
  hallazgos$trazabilidad <- I(trazabilidad)
  for (i in seq_len(nrow(hallazgos))) {
    traza <- hallazgos$trazabilidad[[i]]
    tipo <- as.character(hallazgos$tipo_hallazgo[[i]])
    indice_resultado <- match(
      as.character(hallazgos$columna[[i]]), nombres
    )
    if (tipo == "patron_raro" && is.list(traza) &&
        !identical(traza$estado, "no_disponible") &&
        isTRUE(is.finite(traza$total)) &&
        !is.na(indice_resultado) &&
        !isTRUE(.patrones_raros_recortados(
          resultados[[indice_resultado]]$patrones
        )) && is.na(hallazgos$n_afectados[[i]])) {
      hallazgos$n_afectados[[i]] <- traza$total
    }
    if (tipo %in% c(
        "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
      ) &&
        length(traza$mostrados_formas_dominantes) == 1L &&
        length(traza$mostrados_formas_variantes) == 1L) {
      hallazgos$evidencia[[i]] <- paste0(
        hallazgos$evidencia[[i]], "; traza: ", traza$mostrados,
        " filas mostradas (", traza$mostrados_formas_variantes,
        " formas variantes, ", traza$mostrados_formas_dominantes,
        " formas dominantes); total=", traza$total
      )
    }
  }
  .advertir_incoherencias_trazabilidad(hallazgos, datos, nombres)
  hallazgos
}

.indices_unidades_valor_distinto <- function(tipo, x, trazabilidad) {
  if (tipo %in% c(
      "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
    )) {
    representantes <- trazabilidad$indices_unidades
    if (is.null(representantes)) return(NULL)
    textos <- tryCatch(
      suppressWarnings(as.character(.texto_analizable(x)$valores)),
      error = function(e) NULL
    )
    if (is.null(textos)) return(NULL)
    unidades <- textos[as.integer(representantes)]
    return(list(
      indices = which(!is.na(textos) & textos %in% unidades),
      n_unidades = length(representantes)
    ))
  }
  textos <- tryCatch(
    suppressWarnings(as.character(.texto_analizable(x)$valores)),
    error = function(e) NULL
  )
  if (is.null(textos)) return(NULL)
  presentes <- !is.na(textos)
  unicos <- unique(textos[presentes])
  if (tipo == "mayusculas_inconsistentes") {
    canonicos <- tolower(unicos)
  } else if (tipo == "normalizacion_unicode" &&
             requireNamespace("stringi", quietly = TRUE)) {
    canonicos <- stringi::stri_trans_nfc(unicos)
  } else {
    return(NULL)
  }
  colision <- duplicated(canonicos) | duplicated(canonicos, fromLast = TRUE)
  list(
    indices = which(presentes & textos %in% unicos[colision]),
    n_unidades = sum(colision)
  )
}

.advertir_incoherencias_trazabilidad <- function(hallazgos, datos = NULL,
                                                 nombres = NULL) {
  if (!nrow(hallazgos)) return(invisible(hallazgos))
  problemas <- character()
  for (i in seq_len(nrow(hallazgos))) {
    severidad <- as.character(hallazgos$severidad[[i]])
    afectados <- hallazgos$n_afectados[[i]]
    unidad <- as.character(hallazgos$unidad_conteo[[i]])
    traza <- hallazgos$trazabilidad[[i]]
    if (identical(severidad, "ok") || !is.list(traza)) next
    etiqueta <- paste0(
      as.character(hallazgos$tipo_hallazgo[[i]]), " en ",
      as.character(hallazgos$columna[[i]])
    )
    if (isTRUE(is.finite(afectados)) && afectados == 0) {
      problemas <- c(problemas, paste0(etiqueta, ": n_afectados=0"))
      next
    }
    if (!isTRUE(is.finite(afectados)) || is.na(unidad)) next
    no_aplica <- identical(traza$estado, "no_aplica") ||
      identical(traza$alcance, "no_aplica")
    if (unidad %in% c("columna", "formato") && !no_aplica) {
      problemas <- c(problemas, paste0(etiqueta, ": falta no_aplica"))
      next
    }
    if (no_aplica || unidad %in% c("par", "valor_positivo")) next
    if (identical(traza$estado, "no_disponible")) {
      problemas <- c(problemas, paste0(etiqueta, ": traza no disponible"))
      next
    }
    total <- traza$total
    indices <- unique(as.integer(traza$indices_fila))
    if (!isTRUE(is.finite(total)) ||
        (total != length(indices) && !isTRUE(traza$truncado))) {
      problemas <- c(problemas, paste0(
        etiqueta, ": total de traza no coincide con sus indices"
      ))
      next
    }
    if (unidad %in% c("fila", "geometria")) {
      if (total != afectados) {
        problemas <- c(problemas, paste0(
          etiqueta, ": cuenta ", afectados, " y traza ", total
        ))
      }
      next
    }
    if (unidad == "valor_distinto" && !is.null(datos) &&
        !is.null(nombres)) {
      columna <- as.character(hallazgos$columna[[i]])
      indice_columna <- match(columna, nombres)
      esperados <- if (!is.na(indice_columna)) {
        .indices_unidades_valor_distinto(
          as.character(hallazgos$tipo_hallazgo[[i]]),
          datos[[indice_columna]], traza
        )
      } else NULL
      if (!is.null(esperados)) {
        esperados_filas <- unique(as.integer(esperados$indices))
        if (esperados$n_unidades != afectados ||
            total != length(esperados_filas) ||
            any(!indices %in% esperados_filas) ||
            (!isTRUE(traza$truncado) &&
              any(!esperados_filas %in% indices))) {
          problemas <- c(problemas, paste0(
            etiqueta, ": unidades y filas trazadas no coinciden"
          ))
        }
      }
    }
  }
  # Las seis condiciones de arriba comparan dos salidas del propio `lupa` -lo que
  # el hallazgo afirma contra las filas que se le adjuntaron-, y ninguna mira los
  # datos del usuario. No hay nada que corregir en la tabla, y decir "la
  # trazabilidad contiene incoherencias" mandaba a buscar en los datos algo que
  # esta en el codigo.
  #
  # Es una guarda de invariante contra regresiones, y su destinatario nunca fue
  # quien usa el paquete: `tests/testthat/test-guardas-trazabilidad.R` la dispara
  # **mutando `n_afectados` a mano** despues de perfilar, y comprueba con
  # `expect_no_warning()` que el perfil intacto no la emite.
  if (length(problemas)) {
    condicion <- structure(
      list(
        message = paste0(
          "Un hallazgo quedo con su trazabilidad inconsistente. ",
          "**Es un problema de `lupa`, no de sus datos**: no hay nada que ",
          "corregir en la tabla.
",
          "Lo que significa para lo que esta leyendo: las filas que respaldan ",
          "ese hallazgo pueden no corresponderse con lo que el hallazgo cuenta. ",
          "El hallazgo se conserva para no ocultar la evidencia.
",
          "Si modifico a mano el objeto del perfil, esa es la causa mas ",
          "probable; si no, conviene reportarlo con este detalle: ",
          paste(problemas, collapse = "; "), "."
        ),
        call = NULL
      ),
      class = c("lupa_trazabilidad_incoherente", "warning", "condition")
    )
    warning(condicion)
  }
  invisible(hallazgos)
}

.tipos_equivalentes <- function(declarado, inferido) {
  if (identical(declarado, inferido)) {
    return(TRUE)
  }
  declarado %in% c("factor", "factor-ordenado", "texto") &&
    inferido %in% c("texto", "identificador", "desconocido")
}

.hallazgos_columnas <- function(resultados, columnas,
                                umbral_alta_cardinalidad,
                                umbral_faltantes_sospechoso,
                                umbral_faltantes_error,
                                umbral_patron_raro,
                                umbral_patron_dominante,
                                columnas_sin_ceros,
                                columnas_no_negativas) {
  hallazgos <- list()
  cobertura <- list()
  k <- 0L
  agregar <- function(x) {
    conteo <- .conteo_hallazgo_columna(
      as.character(x$tipo_hallazgo[[1L]]), fila, resultado, n_validos
    )
    x$n_evaluados <- conteo$n_evaluados
    x$n_afectados <- conteo$n_afectados
    x$unidad_conteo <- conteo$unidad_conteo
    k <<- k + 1L
    hallazgos[[k]] <<- x
  }
  agregar_cobertura <- function(diagnostico, columna, motivo,
                               como_resolverlo, dependencia = NA_character_) {
    cobertura[[length(cobertura) + 1L]] <<- .nuevo_diagnostico_no_evaluado(
      diagnostico, columna, motivo, como_resolverlo, dependencia
    )
  }

  for (i in seq_along(resultados)) {
    resultado <- resultados[[i]]
    fila <- resultado$fila
    nombre <- columnas[[i]]
    n_invalidos <- if ("n_codificacion_invalida" %in% names(fila) &&
        is.finite(fila$n_codificacion_invalida)) {
      fila$n_codificacion_invalida
    } else {
      0L
    }
    n_validos <- fila$n - fila$n_faltantes - n_invalidos

    geometria <- resultado$geometria
    # La columna se reconocio como geometrica y no se pudo convertir: la
    # geometria aplica y lo que falta es la medicion. Antes esto no producia
    # ninguna fila y `cobertura_diagnosticos` quedaba vacia sobre una columna
    # espacial, que es la forma silenciosa del mismo defecto.
    if (!isTRUE(geometria$aplica) &&
        !is.null(geometria$motivo_representacion) &&
        !is.na(geometria$motivo_representacion)) {
      agregar_cobertura(
        "perfil_geometria", nombre, geometria$motivo_representacion,
        paste(
          "Convertir la columna a una geometria de 'sf' antes de perfilar, o",
          "declarar que la columna no es geometrica."
        )
      )
    }
    if (isTRUE(geometria$aplica)) {
      if (isTRUE(geometria$geometrias_recortadas)) {
        agregar_cobertura(
          "perfil_geometria", nombre, geometria$motivo_recorte,
          paste(
            "Subir el presupuesto con options(lupa.max_geometrias=) o",
            "options(lupa.max_vertices=) si la columna entera debe evaluarse."
          )
        )
      }
      if (isTRUE(geometria$n_validez_no_evaluados > 0L)) {
        agregar_cobertura(
          "geometria_invalida", nombre,
          paste0(
            "La validez topologica no se pudo evaluar en ",
            geometria$n_validez_no_evaluados,
            " geometrias; el conteo informado cubre las ",
            geometria$n_validez_evaluados, " restantes."
          ),
          "Revisar esas geometrias con st_is_valid() para saber por que fallan."
        )
      }
      if (isTRUE(geometria$n_dominio_no_evaluados > 0L)) {
        agregar_cobertura(
          "coordenada_fuera_dominio", nombre,
          paste0(
            "El dominio no se pudo evaluar en ",
            geometria$n_dominio_no_evaluados,
            " geometrias; el conteo informado cubre las ",
            geometria$n_dominio_evaluados, " restantes."
          ),
          "Revisar el CRS declarado y las coordenadas de esas geometrias."
        )
      }
      if (isFALSE(geometria$sf_evaluado)) {
        agregar_cobertura(
          "perfil_geometria", nombre,
          "No se evaluo la geometria porque falta el paquete opcional 'sf'.",
          "Instalar el paquete 'sf' para medir CRS, tipo, validez, vacios, dominio y bbox.",
          "sf"
        )
      } else {
        if (length(geometria$dimensiones_omitidas)) {
          dimensiones <- paste(geometria$dimensiones_omitidas, collapse = " y ")
          agregar_cobertura(
            "dimensiones_geometria_no_evaluadas", nombre,
            if ("M" %in% geometria$dimensiones_omitidas) {
              retiradas <- if ("Z" %in% geometria$dimensiones_omitidas) {
                "Z y M"
              } else {
                "M"
              }
              paste0(
                "El perfil geometrico no evaluo ", dimensiones,
                " como dimensiones de medida; para la validez topologica ",
                "retiro ", retiradas,
                " con st_zm() y evaluo la geometria XY."
              )
            } else {
              paste0(
                "El perfil geometrico usa solamente X e Y; no evaluo la dimension ",
                dimensiones, "."
              )
            },
            paste0(
              "Evaluar ", dimensiones,
              " con reglas que declaren su unidad y dominio esperados."
            )
          )
        }
        if (is.na(fila$crs_declarado[[1L]])) {
          agregar(.nuevo_hallazgo(
            nombre, "crs_no_declarado", "error",
            "La columna geometrica no declara un CRS; sin el no se puede validar el dominio ni comparar posiciones.",
            "CRS ausente; n_fuera_de_dominio queda en NA.",
            "Declarar el CRS de origen sin transformar ni asumir EPSG:4326 por omision."
          ))
        }
        if (isFALSE(geometria$validez_evaluada)) {
          agregar_cobertura(
            "validez_geometria", nombre,
            paste0("No se pudo evaluar st_is_valid(): ", geometria$motivo_validez),
            "Revisar que las geometrias tengan una representacion compatible con sf.",
            "sf"
          )
        } else if (isTRUE(fila$n_geometrias_invalidas > 0L)) {
          validez_geografica_planar <- identical(
            geometria$validez_criterio, "planar"
          ) && isTRUE(geometria$crs_geografico)
          agregar(.nuevo_hallazgo(
            nombre, "geometria_invalida",
            if (validez_geografica_planar) "sospechoso" else "error",
            if (validez_geografica_planar) {
              paste0(
                "La columna contiene geometrias no validas bajo el criterio ",
                "planar aplicado; en un CRS geografico esto requiere revision."
              )
            } else {
              "La columna contiene geometrias topologicamente invalidas bajo el criterio planar."
            },
            if (validez_geografica_planar) {
              paste0(
                fila$n_geometrias_invalidas,
                " geometrias no validas en el plano; este resultado no afirma ",
                "invalidez esferica."
              )
            } else {
              paste(
                fila$n_geometrias_invalidas,
                "geometrias invalidas segun st_is_valid() con criterio planar."
              )
            },
            if (validez_geografica_planar) {
              paste0(
                "Revisar la geometria con un criterio esferico adecuado antes ",
                "de repararla."
              )
            } else {
              "Revisar la causa de invalidez antes de reparar o usar las geometrias."
            }
          ))
        }
        if (isTRUE(fila$n_geometrias_vacias > 0L)) {
          agregar(.nuevo_hallazgo(
            nombre, "geometria_vacia", "sospechoso",
            "La columna contiene geometrias vacias.",
            paste(fila$n_geometrias_vacias, "geometrias vacias segun st_is_empty()."),
            "Confirmar si la ausencia de coordenadas es valida o debe representarse como NA."
          ))
        }
        if (!is.na(fila$crs_declarado[[1L]]) &&
            isFALSE(geometria$dominio_evaluado)) {
          agregar_cobertura(
            "dominio_geometria", nombre,
            paste0("No se pudo evaluar el dominio del CRS declarado: ",
                   geometria$motivo_dominio),
            "Revisar que la definicion del CRS permita transformar y validar sus coordenadas.",
            "sf"
          )
        } else if (isTRUE(fila$n_fuera_de_dominio > 0L)) {
          agregar(.nuevo_hallazgo(
            nombre, "coordenada_fuera_dominio", "error",
            paste0(
              "La columna contiene geometrias cuyos valores, interpretados en ",
              "las unidades del CRS declarado, quedan fuera de su area de uso. ",
              "Este control detecta unidades incompatibles, como grados donde ",
              "se esperan metros; no detecta una zona UTM equivocada cuando ",
              "los valores caen dentro del area de esa zona."
            ),
            paste0(
              fila$n_fuera_de_dominio,
              " geometrias fuera del dominio global o del area de uso de ",
              fila$crs_declarado, "."
            ),
            "Corregir el CRS declarado o las coordenadas desde la fuente; no reproyectar para ocultar el error."
          ))
        }
        if (isTRUE(geometria$tipos_geometria_mixtos)) {
          agregar(.nuevo_hallazgo(
            nombre, "tipos_geometria_mixtos", "sospechoso",
            "La misma columna mezcla tipos de geometria.",
            paste(geometria$tipos_geometria, collapse = " y "),
            "Confirmar si la mezcla es parte del modelo o separar los tipos en columnas o capas explicitas."
          ))
        }
      }
    }

    if (identical(fila$estado_resumen_cuantitativo, "tipo_compuesto_no_analizado")) {
      estructura <- resultado$estructura_no_analizada
      if (isTRUE(estructura$filas > 0L)) {
        agregar(.nuevo_hallazgo(
          nombre, "tipo_compuesto_no_analizado", "sospechoso",
          paste0(
            "La columna contiene una matriz por fila; no se aplan\u00f3 porque mezclar ",
            "sus componentes inventar\u00eda una sem\u00e1ntica de celda."
          ),
          paste0(
            estructura$filas, " filas y ", estructura$componentes,
            " componentes por fila; m\u00e9tricas por valor en NA."
          ),
          "Separar los componentes en columnas con significado expl\u00edcito antes de perfilarlos."
        ))
      } else {
        agregar_cobertura(
          "tipo_compuesto_no_analizado", nombre,
          "La columna matricial no tiene filas sobre las que evaluar sus componentes.",
          "Perfilar una entrega con filas para evaluar la estructura compuesta."
        )
      }
    }

    # Cruce con los faltantes disfrazados. `n_distintos` y `n_validos` cuentan
    # los valores tal como estan guardados, y no descuentan los que dicen
    # ausencia sin ser `NA`. Una columna entera de `"N/A"` daba `n_distintos`
    # igual a 1 y el hallazgo afirmaba, con estas palabras, que la columna
    # "contiene un unico valor **no ausente**". Es falso: no contiene ninguno.
    # La medida ya estaba calculada en la misma fila.
    todo_disfrazado <- isTRUE(
      is.finite(fila$n_faltantes_disfrazados) &&
        fila$n_faltantes_disfrazados > 0L &&
        fila$n_faltantes_disfrazados >= n_validos
    )
    if (todo_disfrazado) {
      agregar(.nuevo_hallazgo(
        nombre, "constante", "sospechoso",
        paste(
          "La columna no tiene ningun valor: los", fila$n_faltantes_disfrazados,
          "que hay dicen ausencia sin estar codificados como NA, asi que es una",
          "columna vacia escrita de otra forma, no una columna constante."
        ),
        paste0("Valor unico: ", fila$moda, "; frecuencia: ", fila$frecuencia_moda),
        paste(
          "Convertir esos valores a NA antes de decidir si la columna aporta",
          "algo; `faltantes_disfrazados` los enumera."
        )
      ))
    } else if (!is.na(fila$n_distintos) && fila$n_distintos == 1L &&
        isTRUE(n_validos > 1L)) {
      # La condicion real es que la moda no se haya podido representar, no la
      # etiqueta del tipo: una columna `sfc` declara "sfc_POINT" y quedaba fuera.
      valor_sin_representar <- !isTRUE(is.finite(fila$frecuencia_moda[[1L]]))
      agregar(.nuevo_hallazgo(
        nombre, "constante", "sospechoso",
        "La columna contiene un \u00fanico valor no ausente.",
        if (valor_sin_representar) {
          paste0(
            "El valor no se pudo representar como texto; frecuencia: ",
            n_validos
          )
        } else {
          paste0("Valor: ", fila$moda, "; frecuencia: ", fila$frecuencia_moda)
        },
        "Confirmar si la columna aporta informaci\u00f3n o si corresponde retirarla."
      ))
      if (valor_sin_representar) {
        agregar_cobertura(
          "constante", nombre,
          "Se midi\u00f3 un \u00fanico valor distinto y la frecuencia se deduce de las filas validas, pero el valor no se pudo representar como texto porque la columna no es at\u00f3mica.",
          "Separar los componentes de la columna o declarar una representaci\u00f3n textual antes de informar el valor."
        )
      }
    }

    casi_clave <- resultado$casi_clave
    if (!is.null(casi_clave) && isTRUE(casi_clave$es_casi_clave)) {
      agregar(.nuevo_hallazgo(
        nombre, "casi_clave", "sospechoso",
        paste0(
          "La columna es casi una clave, pero concentra sus colisiones en ",
          "muy pocos valores."
        ),
        paste0(
          casi_clave$n_distintos, " valores distintos de ", casi_clave$n_filas,
          " (", sprintf("%.3f", casi_clave$tasa_distintos), "); ",
          casi_clave$n_valores_colisionados, " valores colisionados; ",
          casi_clave$n_filas_en_colision, " filas en colision; ",
          casi_clave$n_duplicados_excedentes, " duplicados excedentes; ",
          "concentracion_colisiones=",
          sprintf("%.3f", casi_clave$concentracion_colisiones), ". ",
          "Colisiones: ", .evidencia_colisiones_casi_clave(casi_clave), ". ",
          "criterio_casi_clave: n_filas>=",
          casi_clave$min_filas,
          ", tasa_distintos>=",
          sprintf("%.3f", casi_clave$umbral_unicidad),
          " y concentracion_colisiones>=",
          sprintf("%.3f", casi_clave$umbral_concentracion), ". ",
          "criterio_rol_casi_clave: rol=",
          casi_clave$rol,
          "; roles_temporales_excluidos=fecha,fecha-hora. ",
          "criterio_tipo_casi_clave: tipo=",
          casi_clave$tipo_almacenamiento,
          "; valores_fraccionarios_finitos=",
          casi_clave$n_valores_fraccionarios_finitos,
          "; doble_admitido_solo_sin_fraccionarios_finitos=TRUE."
        ),
        paste0(
          "Revisar los valores y filas en colision; corregirlos o confirmar ",
          "que la columna no es una clave antes de usarla como identificador."
        )
      ))
    }

    # Dos lugares del paquete contestaban la misma pregunta -si la columna es una
    # numeracion- con reglas distintas, y no coincidian. Aca se exigia
    # `secuencia_entera_densa`, que pide densidad 0,8 y veinte distintos; la
    # guarda que decide callar Benford y los limites de Tukey usa 0,5, cinco
    # distintos y la ausencia de salto de escala.
    #
    # El resultado medido: un `MotId` de 1 a 4557 sobre 3.159 filas tiene
    # densidad 0,694. Las guardas lo trataban como numeracion y callaban dos
    # diagnosticos, y este hallazgo no disparaba, asi que **el perfil se
    # quedaba callado sobre dos pruebas sin decir nunca que habia detectado una
    # numeracion**. Ahora las dos preguntas se contestan con el mismo criterio.
    if (isTRUE(n_validos > 1L) &&
        (fila$tipo_inferido == "identificador" ||
         isTRUE(fila$secuencia_entera_densa) ||
         .parece_identificador_numerico(fila)) &&
        (isTRUE(is.finite(fila$tasa_distintos) && fila$tasa_distintos >= 0.9) ||
         .parece_identificador_numerico(fila))) {
      # Una numeracion puede ser casi unica -un identificador de fila- o
      # repetirse mucho -un codigo de catalogo-, y son dos cosas distintas que
      # merecen dos textos distintos. Exigir unicidad alta para las dos dejaba
      # al codigo sin diagnostico: sobre `EstCod`, de 1 a 284 con tasa 0,097,
      # las guardas callaban los limites de Tukey y Benford por tratarse de una
      # numeracion **y el perfil no decia en ninguna parte que lo fuera**. Es el
      # mismo hueco que tenia `MotId` por el lado de la densidad, visto desde el
      # otro extremo.
      es_unico <- isTRUE(is.finite(fila$tasa_distintos) &&
                           fila$tasa_distintos >= 0.9)
      agregar(.nuevo_hallazgo(
        nombre, "posible_identificador", "ok",
        if (es_unico) {
          "La forma y la alta unicidad son compatibles con un identificador."
        } else {
          paste(
            "Los valores ocupan un tramo compacto de los enteros y se repiten:",
            "la forma es la de un codigo de catalogo, no la de una magnitud."
          )
        },
        paste0(
          fila$n_distintos, " valores distintos de ", n_validos,
          " (", sprintf("%.3f", fila$tasa_distintos), ")",
          if (isTRUE(fila$secuencia_entera_densa)) paste0(
            "; secuencia_entera_densa=TRUE; densidad=",
            sprintf("%.3f", fila$densidad_secuencia_entera),
            "; umbral=",
            sprintf("%.3f", fila$umbral_densidad_secuencia_entera)
          ) else if (is.finite(fila$densidad_secuencia_entera)) paste0(
            "; densidad del tramo de enteros=",
            sprintf("%.3f", fila$densidad_secuencia_entera)
          ) else ""
        ),
        if (es_unico) {
          "Validar con el diccionario de datos si corresponde declarar una clave."
        } else {
          paste(
            "Validar con el diccionario de datos si es un codigo; si lo es,",
            "declarar su catalogo permite medir los valores fuera de dominio."
          )
        }
      ))
    } else if (
      fila$tipo_declarado %in% c("texto", "factor", "factor-ordenado") &&
        is.finite(fila$tasa_distintos) &&
        fila$tasa_distintos > umbral_alta_cardinalidad &&
        fila$tasa_distintos < 1 &&
        fila$n_distintos >= .min_distintos_alta_cardinalidad
    ) {
      # Hubo aca una guarda que callaba este hallazgo cuando los valores
      # promediaban 40 caracteres o mas, por suponerlos texto libre. Se saco:
      # **el largo no distingue una categoria de la prosa**, y fallaba en los
      # dos sentidos. Medido: un catalogo de sesenta categorias de producto con
      # etiquetas de 49 caracteres dejaba de medirse -y su cardinalidad alta es
      # justo la senal de normalizacion rota que hay que ver-, mientras que
      # comentarios libres de diez caracteres se seguian marcando.
      #
      # Y no hacia falta: el hallazgo **no afirma que sea un defecto**. Dice que
      # la cardinalidad es alta y ofrece las tres lecturas -texto libre,
      # identificador o categoria mal normalizada- para que decida quien conoce
      # la columna. Callarlo era responder por el usuario una pregunta que el
      # texto ya le estaba haciendo bien.
      agregar(.nuevo_hallazgo(
        nombre, "alta_cardinalidad", "sospechoso",
        "La columna categ\u00f3rica tiene alta cardinalidad.",
        sprintf("Tasa de valores distintos: %.3f", fila$tasa_distintos),
        "Revisar si es texto libre, un identificador o una categor\u00eda mal normalizada."
      ))
    }

    # El espejo del faltante falso: un valor presente donde la regla declarada
    # dice que la columna no corresponde. Sin aplicabilidad declarada este
    # conteo es cero y el hallazgo no existe.
    if (!is.null(fila$n_presentes_fuera_de_aplicabilidad) &&
        isTRUE(fila$n_presentes_fuera_de_aplicabilidad > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "valor_fuera_de_aplicabilidad", "sospechoso",
        "La columna tiene valores en filas donde la regla declarada dice que no corresponde.",
        paste0(
          fila$n_presentes_fuera_de_aplicabilidad,
          " valores presentes fuera del universo aplicable de ",
          fila$n_no_aplica, " filas."
        ),
        "Revisar la regla de aplicabilidad o el proceso que carga la columna."
      ))
    }

    if (is.finite(fila$prop_faltantes_totales) &&
        fila$prop_faltantes_totales > umbral_faltantes_sospechoso) {
      severidad <- if (fila$prop_faltantes_totales > umbral_faltantes_error) {
        "error"
      } else {
        "sospechoso"
      }
      if (severidad == "error" && fila$n_faltantes == 0L &&
          resultado$faltantes_disfrazados$n_textuales == 0L) {
        severidad <- "sospechoso"
      }
      agregar(.nuevo_hallazgo(
        nombre, "faltantes", severidad,
        "La proporci\u00f3n total de faltantes supera el umbral configurado.",
        sprintf(
          "%d ausentes reales y %d disfrazados (%.3f del total)",
          fila$n_faltantes, fila$n_faltantes_disfrazados,
          fila$prop_faltantes_totales
        ),
        "Revisar la obligatoriedad del campo y el proceso que origina los faltantes."
      ))
    }

    if (isTRUE(fila$n_faltantes_disfrazados > 0L)) {
      solo_numericos <- resultado$faltantes_disfrazados$n_textuales == 0L
      agregar(.nuevo_hallazgo(
        nombre, "faltantes_disfrazados",
        if (solo_numericos) "sospechoso" else "error",
        "Hay valores que representan ausencia sin estar codificados como NA.",
        resultado$faltantes_disfrazados$evidencia,
        if (solo_numericos) {
          "Confirmar que los sentinelas num\u00e9ricos representan ausencia antes de normalizarlos."
        } else {
          "Normalizar estas representaciones a NA conservando su significado si fuera necesario."
        }
      ))
    }

    candidatos_fecha <- resultado$formatos[
      resultado$formatos$estado == "candidato" &
        (!resultado$formatos$anio_dos_digitos |
           resultado$formatos$n_inequivocos == 0L), , drop = FALSE
    ]
    if (nrow(candidatos_fecha) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      agregar(.nuevo_hallazgo(
        nombre, "formato_fecha_ambiguo", "sospechoso",
        paste0(
          "La columna es compatible con fecha, pero no permite distinguir ",
          "d\u00eda/mes de mes/d\u00eda; el rango temporal no se calcula."
        ),
        paste(candidatos_fecha$formato, collapse = " o "),
        "Desambiguar el formato con el origen de los datos antes de convertir la columna."
      ))
    }

    formatos_anio_corto <- resultado$formatos[
      resultado$formatos$anio_dos_digitos, , drop = FALSE
    ]
    if (nrow(formatos_anio_corto) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      agregar(.nuevo_hallazgo(
        nombre, "anio_de_dos_digitos", "sospechoso",
        paste0(
          "La fecha expresa el a\u00f1o con dos d\u00edgitos y no permite decidir ",
          "el siglo sin conocimiento del dominio; el rango temporal no se calcula."
        ),
        paste(unique(formatos_anio_corto$formato), collapse = " o "),
        "Confirmar el siglo con el origen antes de convertir a una fecha completa."
      ))
    }

    if (isTRUE(attr(resultado$formatos, "formatos_mixtos")) &&
        fila$tipo_inferido %in% c("fecha", "fecha-hora")) {
      evidencia <- paste0(
        resultado$formatos$formato, " (", resultado$formatos$n, ")",
        collapse = "; "
      )
      agregar(.nuevo_hallazgo(
        nombre, "formatos_fecha_mixtos", "error",
        "Conviven dos o m\u00e1s formatos de fecha o fecha-hora.",
        evidencia,
        "Estandarizar la columna a un \u00fanico formato antes de convertirla a fecha."
      ))
    }

    if (identical(fila$zona_horaria_origen[[1L]], "sin_declarar")) {
      agregar_cobertura(
        "zona_horaria_fecha_hora", nombre,
        "No se evaluo el cambio de fecha civil a UTC porque la zona horaria de origen no esta declarada.",
        "Declarar attr(x, 'tzone') con la zona horaria de origen antes de interpretar el calendario civil."
      )
    } else if (isTRUE(fila$fecha_civil_distinta_utc)) {
      agregar(.nuevo_hallazgo(
        nombre, "zona_horaria_fecha_hora", "sospechoso",
        "La conversi\u00f3n de fecha-hora a UTC cambia la fecha civil del dato.",
        paste0(
          "Zona de origen: ", fila$zona_horaria_origen,
          "; ", fila$n_filas_fecha_civil_distinta_utc,
          " valores cambian de fecha civil al expresarse en UTC."
        ),
        "Conservar la zona de origen al interpretar el calendario civil y revisar los valores se\u00f1alados."
      ))
    }

    identificador_secuencial <- isTRUE(fila$secuencia_entera_densa) &&
      is.finite(fila$tasa_distintos) && fila$tasa_distintos >= 0.9
    if (!.tipos_equivalentes(fila$tipo_declarado, fila$tipo_inferido) &&
        fila$tipo_inferido != "desconocido" && !identificador_secuencial) {
      # La proporcion compatible se calculaba, se imprimia en la evidencia y no
      # decidia nada: el hallazgo sugeria "convertir la columna" igual con 0,98
      # que con 0,85, y esas dos conversiones no cuestan lo mismo -la segunda
      # deja quince de cada cien filas en `NA`-. La capa de remediacion ya
      # distingue los dos casos y exige compatibilidad total antes de proponer
      # la conversion; el hallazgo no lo decia.
      #
      # Tambien se declara si la proporcion salio de una muestra, que es otro
      # campo que estaba calculado y no viajaba a ninguna parte.
      compatible <- suppressWarnings(as.numeric(fila$proporcion_tipo_inferido))
      analizadas <- suppressWarnings(as.numeric(fila$n_filas_analizadas_tipo))
      incompatibles <- if (is.finite(compatible) && is.finite(analizadas)) {
        round(analizadas * (1 - compatible))
      } else NA_real_
      sobre_muestra <- isTRUE(fila$muestreado_tipo_inferido)
      agregar(.nuevo_hallazgo(
        nombre, "tipo_declarado_distinto", "sospechoso",
        "El tipo declarado no coincide con el tipo impl\u00edcito dominante.",
        paste0(
          sprintf(
            "Declarado: %s; inferido: %s (%.3f compatible)",
            fila$tipo_declarado, fila$tipo_inferido, compatible
          ),
          if (is.finite(incompatibles) && incompatibles > 0) paste0(
            "; convertir dejaria ", .miles_trabajo(incompatibles), " de ",
            .miles_trabajo(analizadas), " valores en NA"
          ) else if (isTRUE(compatible == 1)) {
            "; la conversion no pierde ningun valor"
          } else "",
          if (sobre_muestra) paste0(
            "; medido sobre una muestra de ", .miles_trabajo(analizadas),
            " filas, no sobre la columna entera"
          ) else ""
        ),
        if (isTRUE(compatible == 1)) {
          "Confirmar el tipo esperado y convertir la columna de forma expl\u00edcita."
        } else {
          paste(
            "Confirmar el tipo esperado. Antes de convertir, revisar los valores",
            "que no encajan: la conversion los vuelve NA sin avisar."
          )
        }
      ))
    }

    patrones <- attr(resultado$patrones, "resumen_patrones")
    raros <- .desvios_patron_raro_presentacion(resultado$patrones)
    proporcion_dominante <- if (!is.null(patrones) && nrow(patrones)) {
      as.numeric(patrones$proporcion[[1L]])
    } else {
      NA_real_
    }
    if (is.finite(proporcion_dominante) && nrow(patrones) > 1L &&
        proporcion_dominante < umbral_patron_dominante) {
      agregar_cobertura(
        "patron_raro", nombre,
        paste0(
          "No se evaluo patron_raro porque el patron dominante ocupa ",
          sprintf("%.3f", proporcion_dominante),
          " de los valores analizados; es menor que ",
          "umbral_patron_dominante=",
          sprintf("%.3f", umbral_patron_dominante), "."
        ),
        paste0(
          "Revisar la variabilidad de la columna o ajustar ",
          "umbral_patron_dominante (valor actual: ",
          sprintf("%.3f", umbral_patron_dominante),
          ") antes de volver a perfilar."
        )
      )
    } else if (!is.null(patrones) && nrow(patrones) > 1L &&
               nrow(raros) > 0L &&
               proporcion_dominante >= umbral_patron_dominante) {
      n_excluidos <- attr(
        resultado$patrones,
        "n_filas_patrones_no_dominantes_excluidos",
        exact = TRUE
      )
      if (length(n_excluidos) != 1L || !is.finite(n_excluidos)) {
        n_excluidos <- NA_integer_
      }
      evidencia <- paste0(
        raros$patron, " [", raros$ejemplos, "]",
        collapse = "; "
      )
      clase_desvio <- .clase_desvio_patron_raro(
        patrones$patron[[1L]], raros$patron
      )
      agregar(.nuevo_hallazgo(
        nombre, "patron_raro", "sospechoso",
        paste0(
          "Hay valores infrecuentes que no siguen el patr\u00f3n dominante; ",
          "los patrones de frecuencia intermedia no se consideran desv\u00edos."
        ),
        paste0(
          "Dominante: ", patrones$patron[[1L]],
          " (proporcion_dominante=", sprintf("%.3f", proporcion_dominante),
          "). Desv\u00edos: ",
          paste(utils::head(strsplit(evidencia, "; ", fixed = TRUE)[[1L]], 6L),
                collapse = "; "),
          if (!is.na(clase_desvio)) {
            paste0("; clase_desvio=", clase_desvio)
          } else "",
          "; desvio_unicamente_largo_corrida_numerica=",
          identical(clase_desvio, "largo_de_corrida"),
          "; patrones_no_dominantes_excluidos_por_umbral=",
          if (is.na(n_excluidos)) "NA" else as.character(n_excluidos),
          " filas (umbral_patron_raro=", sprintf("%.3f", umbral_patron_raro),
          ")"
        ),
        "Revisar los valores concretos y validar el formato esperado."
      ))
      if (isTRUE(.patrones_raros_recortados(resultado$patrones))) {
        n_raros <- attr(resultado$patrones, "n_patrones_raros",
                        exact = TRUE)
        limite_raros <- attr(
          resultado$patrones, "limite_patrones_raros_trazabilidad",
          exact = TRUE
        )
        agregar_cobertura(
          "patron_raro", nombre,
          paste0(
            "Se detectaron ", n_raros,
            " patrones raros, pero la trazabilidad conserva solo ",
            limite_raros, " nombres; la enumeracion de filas no cubre todos",
            " los patrones raros. El resumen y la evidencia siguen mostrando",
            " como maximo seis."
          ),
          paste0(
            "Revisar la distribucion completa de patrones o aumentar el limite ",
            "de trazabilidad antes de usar la enumeracion de filas como exhaustiva."
          )
        )
      }
    }

    if (isTRUE(fila$n_espacios_borde > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "espacios_sobrantes", "sospechoso",
        "Hay texto con espacios sobrantes al inicio o al final.",
        paste0(
          fila$n_espacios_borde, " valores; ejemplos: ",
          resultado$diagnostico_texto$evidencia_espacios
        ),
        "Aplicar trimws() despu\u00e9s de confirmar que los espacios no son significativos."
      ))
    }
    if (isTRUE(fila$n_controles_invisibles > 0L)) {
      # El hallazgo decidia con el total y tiene los tres subtotales al lado,
      # calculados y sin consultar. Son tres cosas distintas con tres acciones
      # distintas, y se informaban como una sola con severidad `error` fija:
      #
      #   - invisibles de transporte -BOM, ZWSP-: basura, hay que sacarla;
      #   - espacios Unicode -NBSP-: pueden ser deliberados, hay que mirarlos;
      #   - ZWJ/ZWNJ significativos: **parte del texto**, no hay nada que hacer.
      #
      # Una columna cuyo unico invisible es un ZWJ significativo -un emoji
      # compuesto, una ligadura- no tiene ningun defecto, y se marcaba `error`.
      # `n_invisibles_significativos` no lo consultaba ningun archivo del
      # paquete. La capa de remediacion si separa los tres casos.
      eliminables <- suppressWarnings(as.numeric(fila$n_invisibles_eliminables))
      espacios <- suppressWarnings(as.numeric(fila$n_espacios_invisibles))
      significativos <- suppressWarnings(
        as.numeric(fila$n_invisibles_significativos)
      )
      hay <- function(x) isTRUE(is.finite(x) && x > 0)
      solo_significativos <- hay(significativos) && !hay(eliminables) &&
        !hay(espacios)
      # Cuando solo hay uniones de ancho cero no hay nada que ELIMINAR, pero eso
      # no las vuelve correctas: un ZWJ entre `Juan` y `Perez` no tiene funcion
      # tipografica -el ZWJ une emojis y liga escrituras arabes o indicas, no
      # letras latinas- y rompe las comparaciones igual, porque `Juan<ZWJ>Perez`
      # no es `Juan Perez`. El paquete no puede saber si la union corresponde a
      # esa columna, asi que baja de `error` a `sospechoso` y lo dice: no
      # afirma que sea basura ni que este bien.
      agregar(.nuevo_hallazgo(
        nombre, "controles_invisibles",
        if (solo_significativos) "sospechoso" else "error",
        if (solo_significativos) {
          paste(
            "Los unicos caracteres invisibles son uniones de ancho cero",
            "(ZWJ/ZWNJ). Unen emojis y ligan escrituras como el arabe, y ahi",
            "forman parte del texto; entre letras latinas no cumplen ninguna",
            "funcion y rompen las comparaciones sin verse."
          )
        } else {
          paste0(
            "Hay controles, espacios Unicode o marcas invisibles dentro de los ",
            "valores; los ZWJ/ZWNJ se informan porque pueden ser sem\u00e1nticos."
          )
        },
        resultado$diagnostico_texto$evidencia_controles_invisibles,
        if (solo_significativos) {
          paste(
            "Mirar entre que caracteres estan: si unen emojis o ligan una",
            "escritura que lo requiere, no hay nada que hacer; si separan",
            "palabras latinas, hay que eliminarlas."
          )
        } else if (hay(eliminables) && !hay(espacios) && !hay(significativos)) {
          paste0(
            "Eliminar los ", .miles_trabajo(eliminables), " invisibles de ",
            "transporte: no aportan nada y rompen las comparaciones."
          )
        } else {
          "Eliminar s\u00f3lo los invisibles de transporte; revisar los espacios Unicode y conservar ZWJ/ZWNJ cuando tengan significado."
        }
      ))
    }
    if (isTRUE(fila$n_entidades_html > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "entidades_html", "sospechoso",
        "Hay entidades HTML sin decodificar dentro de los valores.",
        resultado$diagnostico_texto$evidencia_entidades_html,
        "Decodificar las entidades HTML s\u00f3lo despu\u00e9s de confirmar que la columna proviene de una fuente escapada."
      ))
    }
    if (isTRUE(fila$n_separadores_en_campo > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "separadores_en_campo", "sospechoso",
        "Hay saltos de l\u00ednea dentro de los valores de texto.",
        resultado$diagnostico_texto$evidencia_separadores_en_campo,
        "Reemplazar los saltos por espacios s\u00f3lo despu\u00e9s de confirmar que no son parte del contenido leg\u00edtimo."
      ))
    }
    if (isTRUE(fila$n_variantes_mayusculas > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "mayusculas_inconsistentes", "sospechoso",
        "Conviven valores que s\u00f3lo se diferencian por may\u00fasculas y min\u00fasculas.",
        resultado$diagnostico_texto$evidencia_mayusculas,
        "Definir y aplicar una convenci\u00f3n de capitalizaci\u00f3n para la columna."
      ))
    }
    if (isFALSE(fila$unicode_evaluado[[1L]])) {
      agregar_cobertura(
        "normalizacion_unicode", nombre,
        "No se pudo evaluar la normalizacion Unicode: falta el paquete opcional 'stringi'.",
        "Instalar el paquete 'stringi' para evaluar equivalencias Unicode NFC/NFD.",
        "stringi"
      )
    } else if (!is.na(fila$n_variantes_unicode) && fila$n_variantes_unicode > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "normalizacion_unicode", "sospechoso",
        paste0(
          "Conviven textos visualmente equivalentes con representaciones ",
          "Unicode NFC y NFD distintas."
        ),
        resultado$diagnostico_texto$evidencia_unicode,
        paste0(
          "Confirmar el dominio y normalizar a NFC sin alterar el contenido ",
          "visible del texto."
        )
      ))
    }
    if (isTRUE(fila$n_codificacion_invalida > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "codificacion_invalida", "error",
        paste0(
          "Hay texto con secuencias de bytes que no forman UTF-8 v\u00e1lido; ",
          "esos valores se excluyeron de los an\u00e1lisis textuales."
        ),
        resultado$diagnostico_texto$evidencia_codificacion_invalida,
        paste0(
          "Volver a leer la fuente declarando su codificaci\u00f3n original; no ",
          "reinterpretar ni reemplazar los bytes sin ese dato."
        )
      ))
    }
    if (isTRUE(fila$n_codificacion_rota > 0L)) {
      reparables_total <- fila$n_codificacion_reparable[[1L]] +
        if ("n_codificacion_reparable_parcialmente" %in% names(fila)) {
          fila$n_codificacion_reparable_parcialmente[[1L]]
        } else 0
      agregar(.nuevo_hallazgo(
        nombre, "codificacion_rota", "error",
        "Hay texto con se\u00f1ales de una conversi\u00f3n de codificaci\u00f3n incorrecta.",
        resultado$diagnostico_texto$evidencia_codificacion,
        if (isTRUE(reparables_total > 0L)) {
          paste0(
            "Reparar los valores cuyo viaje por las codificaciones conocidas ",
            "cierra en texto UTF-8; revisar manualmente los caracteres perdidos."
          )
        } else {
          "Recuperar el dato desde la fuente: el car\u00e1cter original ya no est\u00e1 disponible."
        },
        estado_reparacion = fila$estado_codificacion_reparacion[[1L]]
      ))
    }
    if (isTRUE(fila$n_numeros_texto > 0L) &&
        is.finite(fila$proporcion_numeros_texto) &&
        fila$proporcion_numeros_texto >= 0.8) {
      agregar(.nuevo_hallazgo(
        nombre, "numero_como_texto", "sospechoso",
        if (isTRUE(fila$numero_texto_ambiguo)) {
          paste0(
            "La columna parece num\u00e9rica, pero un separador seguido por tres ",
            "d\u00edgitos puede representar decimales o miles sin evidencia adicional."
          )
        } else {
          paste0(
            "La columna contiene n\u00fameros escritos con una convenci\u00f3n decimal, ",
            "unidad o moneda identificable."
          )
        },
        resultado$numeros_texto$evidencia,
        if (isTRUE(fila$numero_texto_seguro)) {
          "Convertir de forma expl\u00edcita conservando en la bit\u00e1cora la convenci\u00f3n y la unidad."
        } else {
          "Confirmar la convenci\u00f3n decimal y la unidad antes de convertir."
        }
      ))
    }

    unidades <- resultado$numeros_texto$unidades
    if (length(unidades) > 1L) {
      evidencia_unidades <- paste(
        paste0(names(unidades), " (", as.integer(unidades), ")"),
        collapse = "; "
      )
      agregar(.nuevo_hallazgo(
        nombre, "unidades_mixtas", "sospechoso",
        paste0(
          "La columna numerica escrita como texto mezcla dos o mas sufijos ",
          "de unidad; el perfil no los convierte ni supone equivalencias."
        ),
        paste0(
          "Unidades observadas: ", evidencia_unidades,
          "; celdas con unidad: ", sum(unidades)
        ),
        "Separar o normalizar las unidades segun una regla declarada antes de sumar o comparar los valores."
      ))
    }

    monedas <- resultado$numeros_texto$monedas
    if (length(monedas) > 1L) {
      evidencia_monedas <- paste(
        paste0(names(monedas), " (", as.integer(monedas), ")"),
        collapse = "; "
      )
      agregar(.nuevo_hallazgo(
        nombre, "monedas_mixtas", "sospechoso",
        paste0(
          "La columna numerica escrita como texto mezcla dos o mas monedas; ",
          "el perfil no convierte ni supone tasas de cambio."
        ),
        paste0(
          "Monedas observadas: ", evidencia_monedas,
          "; celdas con moneda: ", sum(monedas)
        ),
        "Separar las monedas o declarar una regla de negocio antes de sumar o comparar los valores."
      ))
    }

    if (length(resultado$multivaluados)) {
      for (multivaluado in resultado$multivaluados) {
        distribucion <- multivaluado$valores_por_celda
        evidencia_celdas <- paste(
          paste0(names(distribucion), " valores: ",
                 as.integer(distribucion), " celdas"),
          collapse = "; "
        )
        agregar(.nuevo_hallazgo(
          nombre, "celdas_multivaluadas", "sospechoso",
          paste0(
            "Una minoria homogenea de celdas contiene varios valores; las ",
            "partes comparten tipo y patron con el resto de la columna."
          ),
          paste0(
            "Delimitador: ", multivaluado$delimitador,
            "; celdas: ", multivaluado$n_celdas,
            "; ", evidencia_celdas,
            "; valores separados: ", multivaluado$n_valores
          ),
          "Separar los valores en filas o en una estructura relacionada antes de contar, unir o validar el dominio."
        ))
      }
    }

    if (identical(fila$estado_resumen_cuantitativo, "omitidos_precision")) {
      agregar(.nuevo_hallazgo(
        nombre, "integer64_fuera_precision_double", "sospechoso",
        paste0(
          "La columna integer64 excede el rango de enteros exactamente ",
          "representables por double; se omiten estad\u00edsticos aproximados."
        ),
        paste0(
          "M\u00ednimo exacto: ", fila$minimo_exacto,
          "; m\u00e1ximo exacto: ", fila$maximo_exacto
        ),
        "Conservar la clase integer64 y usar los extremos exactos informados."
      ))
    } else if (identical(fila$estado_resumen_cuantitativo, "requiere_bit64")) {
      agregar_cobertura(
        "integer64_sin_soporte", nombre,
        "La clase integer64 no se resumi\u00f3 porque falta su soporte opcional.",
        "Instalar el paquete 'bit64' para calcular extremos exactos.",
        "bit64"
      )
    }

    n_inf <- fila$n_infinito_positivo + fila$n_infinito_negativo
    if (isTRUE(n_inf > 0L) || isTRUE(fila$n_nan > 0L)) {
      agregar(.nuevo_hallazgo(
        nombre, "valores_no_finitos", "error",
        paste0(
          "La columna contiene NaN o infinitos; los estad\u00edsticos de rango ",
          "describen s\u00f3lo los valores finitos."
        ),
        paste0(
          "NaN: ", fila$n_nan, "; +Inf: ", fila$n_infinito_positivo,
          "; -Inf: ", fila$n_infinito_negativo
        ),
        "Revisar operaciones indefinidas o divisiones por cero en el origen."
      ))
    }

    if (nombre %in% columnas_sin_ceros &&
        !is.na(fila$n_ceros) && fila$n_ceros > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "ceros_no_permitidos", "sospechoso",
        "La columna contiene ceros aunque se configur\u00f3 como no admisible.",
        paste(fila$n_ceros, "ceros"),
        "Verificar si los ceros son v\u00e1lidos o representan otro estado."
      ))
    }
    if (nombre %in% columnas_no_negativas &&
        !is.na(fila$n_negativos) && fila$n_negativos > 0L) {
      agregar(.nuevo_hallazgo(
        nombre, "negativos_no_permitidos", "sospechoso",
        "La columna contiene valores negativos aunque se configur\u00f3 como no negativa.",
        paste(fila$n_negativos, "valores negativos"),
        "Corregir los valores o ajustar la restricci\u00f3n del dominio."
      ))
    }
    # Un valor que cumple las tres senales a la vez -extremo, repetido y con
    # forma de todo nueves- es un centinela, aunque no este en la lista
    # declarada. Ninguna de las tres alcanza sola: `9999` es una edad imposible
    # y un codigo postal valido, un monto real no se repite, y un ano 1999 no
    # tiene esa forma. Por eso la lista por omision no lo trae y hace bien.
    #
    # Se informa aparte de `faltantes_disfrazados`, que sigue contando solo lo
    # declarado. Aca no se afirma que sea ausencia: se dice que tiene la forma
    # de serlo y se ofrece la accion.
    if (!is.na(fila$centinela_valor) &&
        !is.na(fila$centinela_repeticiones)) {
      agregar(.nuevo_hallazgo(
        nombre, "posible_centinela_numerico", "sospechoso",
        # La descripcion nombra un valor de la columna, asi que en una columna
        # protegida no puede decirlo. La evidencia ya salia protegida y la
        # descripcion no: la fuga entraba por ahi.
        if (isTRUE(fila$dato_personal_protegido)) {
          paste0(
            "Un valor aparece ", fila$centinela_repeticiones, " veces, queda ",
            "fuera de los limites de la columna y tiene la forma de los codigos ",
            "que se usan para decir \"sin dato\". No se nombra porque la columna ",
            "esta protegida, y no se cuenta como ausencia porque no esta ",
            "declarado."
          )
        } else {
          paste0(
            "El valor ", .texto_valor(fila$centinela_valor), " aparece ",
            fila$centinela_repeticiones, " veces, queda fuera de los limites de ",
            "la columna y tiene la forma de los codigos que se usan para decir ",
            "\"sin dato\". No se cuenta como ausencia porque no esta declarado."
          )
        },
        if (isTRUE(fila$dato_personal_protegido)) {
          paste0(
            "Repeticiones: ", fila$centinela_repeticiones,
            "; valores fuera de los limites: ", fila$n_outliers
          )
        } else {
          paste0(
            "Valor: ", .texto_valor(fila$centinela_valor), "; repeticiones: ",
            fila$centinela_repeticiones, "; valores fuera de los limites: ",
            fila$n_outliers
          )
        },
        paste0(
          "Si es un codigo de ausencia, agregarlo a `sentinelas_numericos` ",
          "para que se cuente como faltante; si es un dato real, no hay nada ",
          "que hacer."
        )
      ))
    }

    if (!is.na(fila$n_outliers) && fila$n_outliers > 0L) {
      if (.parece_identificador_numerico(fila)) {
        cobertura[[length(cobertura) + 1L]] <- .nuevo_diagnostico_no_evaluado(
          "outliers", nombre,
          paste0(
            "No se evaluaron los limites de Tukey: la columna es de enteros y ",
            "cubre ",
            # La decision se toma con la densidad sin el centinela cuando hay
            # uno, y el texto imprimia la otra: sobre `1..1000` con quince
            # `9999` decidia con 100 % y publicaba "cubre 10 %", que leido como
            # evidencia dice lo contrario de lo que la regla concluyo. El motivo
            # tiene que nombrar el numero que decidio, y decir de donde sale.
            sprintf(
              "%.0f%%",
              100 * suppressWarnings(as.numeric(
                if (isTRUE(is.finite(as.numeric(fila$densidad_sin_centinela)))) {
                  fila$densidad_sin_centinela
                } else {
                  fila$densidad_secuencia_entera
                }
              ))
            ),
            if (isTRUE(is.finite(
              suppressWarnings(as.numeric(fila$densidad_sin_centinela))
            ))) {
              " de los enteros entre su minimo y su maximo una vez descontado el centinela"
            } else {
              " de los enteros entre su minimo y su maximo"
            },
            ", asi que es una ",
            "numeracion -un identificador o un codigo- y no una magnitud. ",
            "Los limites de Tukey describen una distribucion; que un codigo ",
            "quede lejos de la mediana no dice nada de su calidad. Se habrian ",
            "senalado ", fila$n_outliers, " valores."
          ),
          paste(
            "Si la columna es una magnitud y no una numeracion, revisar por que",
            "sus valores ocupan un tramo tan compacto de los enteros. Un valor",
            "fuera de escala rompe esa compacidad y vuelve a senalarse."
          )
        )
      } else {
        agregar(.nuevo_hallazgo(
          nombre, "outliers", "sospechoso",
          paste(
            "Hay valores muy alejados del grueso de la columna: quedan fuera de",
            "la horquilla que va del primer al tercer cuartil ampliada una vez y",
            "media a cada lado (el criterio de Tukey, 1,5 x IQR)."
          ),
          paste(fila$n_outliers, "valores"),
          paste(
            "Mirar esos valores: un centinela como 9999 o una fecha de 1900",
            "aparecen asi, y tambien un dato real poco frecuente. Estar lejos",
            "no los vuelve errores."
          )
        ))
      }
    }
  }
  salida <- hallazgos
  attr(salida, "cobertura_diagnosticos") <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_diagnosticos_vacia()
  }
  salida
}

.nombres_columnas_problematicos <- function(nombres) {
  if (!length(nombres)) {
    return(data.frame(
      original = character(), propuesto = character(), stringsAsFactors = FALSE
    ))
  }
  originales <- as.character(nombres)
  originales[is.na(originales)] <- ""
  propuestos <- make.names(originales, unique = TRUE)
  problema <- originales != propuestos | originales != trimws(originales) |
    duplicated(originales) | duplicated(originales, fromLast = TRUE)
  data.frame(
    original = originales[problema],
    propuesto = propuestos[problema],
    stringsAsFactors = FALSE
  )
}

.normalizar_nombre_fecha <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  tolower(gsub("[^[:alnum:]]+", "_", y, perl = TRUE))
}

.detectar_fecha_partida <- function(datos, nombres, max_candidatos = 10000L) {
  vacio <- function(posibles = 0, evaluados = 0L) {
    list(
      hallados = character(),
      alcance = list(
        n_candidatos_posibles = as.numeric(posibles),
        n_candidatos_evaluados = as.numeric(evaluados),
        n_candidatos_sin_comparar = as.numeric(max(
          0, posibles - evaluados
        )),
        max_candidatos = as.integer(max_candidatos),
        truncado = posibles > evaluados
      )
    )
  }
  if (ncol(datos) < 3L || !nrow(datos)) return(vacio())
  normalizados <- .normalizar_nombre_fecha(nombres)
  roles <- list(
    anio = "(?:anio|ano|year)", mes = "(?:mes|month)", dia = "(?:dia|day)"
  )
  indices <- lapply(roles, function(patron) {
    which(grepl(paste0("(^|_)", patron, "($|_)"), normalizados, perl = TRUE))
  })
  if (any(lengths(indices) == 0L)) return(vacio())
  inter_anio_mes <- length(intersect(indices$anio, indices$mes))
  inter_anio_dia <- length(intersect(indices$anio, indices$dia))
  inter_mes_dia <- length(intersect(indices$mes, indices$dia))
  inter_todos <- length(Reduce(intersect, indices))
  posibles <- prod(lengths(indices)) -
    inter_anio_mes * length(indices$dia) -
    inter_anio_dia * length(indices$mes) -
    inter_mes_dia * length(indices$anio) + 2 * inter_todos
  hallados <- character()
  evaluados <- 0L
  agotado <- FALSE
  for (anio in indices$anio) {
    for (mes in indices$mes) {
      for (dia in indices$dia) {
        posicion <- c(anio, mes, dia)
        if (length(unique(posicion)) != 3L) next
        if (evaluados >= max_candidatos) {
          agotado <- TRUE
          break
        }
        evaluados <- evaluados + 1L
        valores <- lapply(posicion, function(j) {
          suppressWarnings(as.integer(trimws(as.character(datos[[j]]))))
        })
        names(valores) <- c("anio", "mes", "dia")
        presentes <- Reduce(`&`, lapply(valores, Negate(is.na)))
        if (sum(presentes) < 3L) next
        en_rango <- valores$anio >= 1800L & valores$anio <= 2100L &
          valores$mes >= 1L & valores$mes <= 12L &
          valores$dia >= 1L & valores$dia <= 31L
        fechas <- suppressWarnings(as.Date(sprintf(
          "%04d-%02d-%02d", valores$anio, valores$mes, valores$dia
        )))
        proporcion <- mean(en_rango[presentes] & !is.na(fechas[presentes]))
        if (is.finite(proporcion) && proporcion >= 0.8) {
          hallados <- c(hallados, paste0(
            nombres[[posicion[[3L]]]], " + ", nombres[[posicion[[2L]]]],
            " + ", nombres[[posicion[[1L]]]], " (",
            sprintf("%.3f", proporcion), " v\u00e1lidas)"
          ))
        }
      }
      if (agotado) break
    }
    if (agotado) break
  }
  list(
    hallados = unique(hallados),
    alcance = list(
      n_candidatos_posibles = as.numeric(posibles),
      n_candidatos_evaluados = as.numeric(evaluados),
      n_candidatos_sin_comparar = as.numeric(max(0, posibles - evaluados)),
      max_candidatos = as.integer(max_candidatos),
      truncado = posibles > evaluados
    )
  )
}

.columnas_duplicadas <- function(datos, nombres) {
  .pares_de_columnas_identicas(datos, seq_len(ncol(datos)), nombres)
}

.construir_hallazgos <- function(datos, resultados, columnas, duplicadas,
                                 umbral_alta_cardinalidad,
                                 umbral_faltantes_sospechoso,
                                 umbral_faltantes_error,
                                 umbral_patron_raro,
                                 umbral_patron_dominante,
                                 columnas_sin_ceros,
                                 columnas_no_negativas,
                                 n_filas_duplicadas,
                                 n_filas_en_grupos_duplicados,
                                 relaciones_orden = list(),
                                 relaciones_aritmeticas = list(),
                                 normalizacion = NULL,
                                 detectar_casi_duplicados = TRUE,
                                 max_proporcion_grupo = 0.5,
                                 umbral_variante_rara = 0.05,
                                 min_asimetria_variante = 10,
                                 min_asimetria_general = 2,
                                 min_participacion_dominante = 0.5,
                                 detectar_variantes_equifrecuentes = FALSE,
                                 max_asimetria_equifrecuente = 2,
                                 max_trabajo = 2e10,
                                 max_largo_valor =
                                   .MAX_LARGO_VALOR_CASI_DUPLICADOS,
                                 clave_declarada = NULL,
                                 trazador_tiempos = NULL) {
  hallazgos_columnas <- .hallazgos_columnas(
    resultados, columnas, umbral_alta_cardinalidad,
    umbral_faltantes_sospechoso, umbral_faltantes_error,
    umbral_patron_raro, umbral_patron_dominante,
    columnas_sin_ceros, columnas_no_negativas
  )
  cobertura <- attr(hallazgos_columnas, "cobertura_diagnosticos", exact = TRUE)
  if (is.null(cobertura)) cobertura <- .cobertura_diagnosticos_vacia()
  hallazgos <- hallazgos_columnas
  hallazgos_vocabulario <- if (isTRUE(detectar_casi_duplicados)) {
    .medir_etapa_dbi(
      trazador_tiempos, "casi_duplicados_vocabulario",
      .hallazgos_casi_duplicados_vocabulario(
        datos, columnas, normalizacion, resultados = resultados,
        max_proporcion_grupo = max_proporcion_grupo,
        umbral_variante_rara = umbral_variante_rara,
        min_asimetria_variante = min_asimetria_variante,
        min_asimetria_general = min_asimetria_general,
        min_participacion_dominante = min_participacion_dominante,
        detectar_variantes_equifrecuentes = detectar_variantes_equifrecuentes,
        max_asimetria_equifrecuente = max_asimetria_equifrecuente,
        max_trabajo = max_trabajo,
        max_largo_valor = max_largo_valor
      )
    )
  } else {
    .registrar_etapa_dbi(
      trazador_tiempos, "casi_duplicados_vocabulario",
      estado = "no_solicitado"
    )
    list()
  }
  cobertura_vocabulario <- attr(
    hallazgos_vocabulario, "cobertura_diagnosticos", exact = TRUE
  )
  if (!is.null(cobertura_vocabulario) && nrow(cobertura_vocabulario)) {
    cobertura <- rbind(cobertura, cobertura_vocabulario)
  }
  if (length(hallazgos_vocabulario)) {
    hallazgos <- c(hallazgos, hallazgos_vocabulario)
  }
  # Una clave declarada que se repite es la comprobacion mas directa que hay de
  # unicidad, y se informaba con `cli_warn` -un aviso que se lo lleva la
  # consola-. No entraba en la tabla de hallazgos, asi que no tenia severidad, no
  # viajaba al informe ni al plan de limpieza, y no decia que filas repiten.
  # Que el usuario declare la clave es justamente el caso en que el paquete
  # **sabe** cual es: ahi no hay nada que inferir, solo que contar.
  if (!is.null(clave_declarada) && length(clave_declarada) &&
        all(clave_declarada %in% names(datos)) && nrow(datos)) {
    valores_clave <- .seleccionar_columnas(datos, clave_declarada)
    repetidas <- base::duplicated.data.frame(valores_clave) |
      base::duplicated.data.frame(valores_clave, fromLast = TRUE)
    n_repetidas <- sum(repetidas)
    if (n_repetidas > 0L) {
      hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
        NA_character_, "clave_no_unica", "error",
        paste0(
          "La clave declarada no identifica una fila: ",
          if (length(clave_declarada) == 1L) {
            paste0("la columna `", clave_declarada, "`")
          } else {
            paste0("las columnas ", paste0("`", clave_declarada, "`", collapse = ", "))
          },
          " repite valores."
        ),
        paste0(
          n_repetidas, " filas en ",
          nrow(unique(valores_clave[repetidas, , drop = FALSE])),
          " valores repetidos"
        ),
        paste(
          "Revisar si sobra una columna en la clave, si hay duplicados de carga",
          "o si la clave real es otra. La trazabilidad sigue localizando por",
          "ella, pero un valor puede senalar mas de una fila."
        ),
        nrow(datos), n_repetidas, "fila"
      )
    }
  }
  if (n_filas_duplicadas > 0L) {
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "filas_duplicadas", "error",
      "La tabla contiene filas duplicadas exactas.",
      paste0(
        n_filas_en_grupos_duplicados, " filas en grupos duplicados (",
        n_filas_duplicadas, " excedentes)"
      ),
      "Definir una clave y revisar la causa antes de eliminar duplicados.",
      nrow(datos), n_filas_en_grupos_duplicados, "fila"
    )
  }
  # Sin filas, dos columnas son triviamente iguales y la comparacion no tiene
  # contenido sobre el que apoyarse. Eso se declara en vez de afirmarse.
  if (nrow(datos) == 0L && ncol(datos) >= 2L) {
    cobertura <- rbind(cobertura, .nuevo_diagnostico_no_evaluado(
      "columnas_duplicadas", NA_character_,
      paste(
        "No se compararon las columnas entre si porque la tabla no tiene",
        "filas: dos columnas vacias coinciden sin que eso sea evidencia."
      ),
      "Perfilar una tabla con al menos una fila para comparar contenidos."
    ))
  } else if (nrow(duplicadas)) {
    for (i in seq_len(nrow(duplicadas))) {
      hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
        duplicadas$columna_1[[i]], "columnas_duplicadas", "sospechoso",
        "Dos columnas tienen el mismo contenido.",
        paste0(
          duplicadas$columna_1[[i]], " = ", duplicadas$columna_2[[i]],
          "; comparadas sobre ", nrow(datos),
          if (nrow(datos) == 1L) " fila" else " filas"
        ),
        "Confirmar si ambas columnas son necesarias o si existe redundancia.",
        ncol(datos), 2, "columna"
      )
    }
  }
  nombres_problematicos <- .nombres_columnas_problematicos(columnas)
  if (nrow(nombres_problematicos)) {
    evidencia <- paste0(
      encodeString(nombres_problematicos$original, quote = '"'),
      " -> ",
      encodeString(nombres_problematicos$propuesto, quote = '"'),
      collapse = "; "
    )
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "nombres_columnas_problematicos", "sospechoso",
      "La tabla contiene nombres de columna no sint\u00e1cticos o duplicados.",
      evidencia,
      "Renombrar las columnas con nombres sint\u00e1cticos, \u00fanicos y sin espacios al borde.",
      ncol(datos), nrow(nombres_problematicos), "columna"
    )
  }
  fechas_partidas <- .detectar_fecha_partida(datos, columnas)
  if (length(fechas_partidas$hallados)) {
    columnas_partidas <- unique(trimws(unlist(strsplit(
      sub("\\s*\\(.*$", "", fechas_partidas$hallados), "\\+"
    ))))
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      NA_character_, "fecha_partida_columnas", "sospechoso",
      "La tabla parece representar una fecha mediante columnas separadas de a\u00f1o, mes y d\u00eda.",
      paste(fechas_partidas$hallados, collapse = "; "),
      "Confirmar la sem\u00e1ntica y construir una fecha expl\u00edcita sin descartar las columnas de origen.",
      ncol(datos), length(columnas_partidas), "columna"
    )
  }
  if (isTRUE(fechas_partidas$alcance$truncado)) {
    cobertura <- rbind(cobertura, .nuevo_diagnostico_no_evaluado(
      "fecha_partida_columnas", NA_character_,
      paste0(
        "La b\u00fasqueda de fechas partidas compar\u00f3 ",
        fechas_partidas$alcance$n_candidatos_evaluados, " de ",
        fechas_partidas$alcance$n_candidatos_posibles,
        " combinaciones de columnas; quedaron ",
        fechas_partidas$alcance$n_candidatos_sin_comparar,
        " sin comparar por el presupuesto de ",
        fechas_partidas$alcance$max_candidatos, "."
      ),
      "Reducir las columnas candidatas, o solicitar una regla de fecha explicita por nombre y rol."
    ))
  }
  if (length(relaciones_orden)) {
    hallazgos <- c(hallazgos, relaciones_orden)
  }
  if (length(relaciones_aritmeticas)) {
    hallazgos <- c(hallazgos, relaciones_aritmeticas)
  }

  if (length(hallazgos)) {
    resultado <- do.call(rbind, hallazgos)
  } else {
    resultado <- data.frame(
      columna = character(), tipo_hallazgo = character(),
      severidad = character(), descripcion = character(),
      evidencia = character(), sugerencia = character(),
      n_evaluados = numeric(), n_afectados = numeric(),
      unidad_conteo = character(),
      estado_reparacion = character(),
      trazabilidad = I(list()),
      stringsAsFactors = FALSE
    )
  }
  resultado$severidad <- factor(
    resultado$severidad,
    levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  rownames(resultado) <- NULL
  attr(resultado, "cobertura_diagnosticos") <- cobertura
  resultado
}
