.moda_columna <- function(x) {
  validos <- !is.na(x)
  # Dos ausencias distintas, y la diferencia importa: sin valores validos la
  # frecuencia es cero porque se conto y no hay ninguno; sobre una columna de
  # listas no se pudo contar, y eso es NA. Confundirlas hacia que un hallazgo
  # informara cero afectados sobre datos validos.
  if (!any(validos)) {
    return(list(valor = NA_character_, frecuencia = 0L))
  }
  if (is.list(x)) {
    return(list(valor = NA_character_, frecuencia = NA_integer_))
  }
  por_bloques <- .moda_mapa_distintos(.mapa_distintos_bloques(x))
  if (!is.null(por_bloques)) return(por_bloques)
  valores <- x[validos]
  unicos <- unique(valores)
  # `unique()` devuelve en orden de aparicion y `which.max()` toma el primer
  # maximo, asi que ante un empate la moda salia por el orden de las filas: la
  # columna 1,2,3,0,-4 daba moda 1, y la misma columna invertida daba -4. El
  # veredicto no puede depender de la forma fisica de la tabla.
  #
  # El desempate es por valor ascendente, que es el mismo criterio que ya usaba
  # la via DBI -`ORDER BY frecuencia DESC, columna ASC`- y estaba documentado
  # solo de aquel lado. Ahora las dos puertas dan la misma moda sobre los mismos
  # datos.
  #
  # `method = "radix"` en texto: el orden por omision depende de la
  # configuracion regional, y con eso el desempate cambiaria de una maquina a
  # otra. Es el mismo cuidado que en el recorte del vocabulario.
  # `order()` no sabe ordenar todos los tipos: sobre una columna de blobs
  # (`raw`) tira "unimplemented type 'raw' in 'orderVector1'". Se ordena por una
  # clave equivalente donde hace falta, y si aun asi no se puede, se deja el
  # orden de aparicion: es peor que ordenar, pero mucho mejor que romper el
  # perfil entero por una columna exotica.
  if (is.character(unicos)) {
    unicos <- .ordenar_por_bytes(unicos)
  } else {
    clave <- if (is.raw(unicos)) as.integer(unicos) else unicos
    orden <- tryCatch(order(clave), error = function(e) NULL)
    if (!is.null(orden) && length(orden) == length(unicos)) {
      unicos <- unicos[orden]
    }
  }
  indices <- match(valores, unicos)
  frecuencias <- tabulate(indices, nbins = length(unicos))
  posicion <- which.max(frecuencias)
  list(
    valor = .texto_valor(unicos[posicion]),
    frecuencia = as.integer(frecuencias[[posicion]])
  )
}

.resumen_longitud <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    return(c(minimo = NA_real_, maximo = NA_real_, media = NA_real_))
  }
  .resumen_longitudes_bloques(x)
}

.valores_cuantitativos <- function(x, inferencia, formatos,
                                   meses_texto = NULL, vocabulario = NULL,
                                   valores_preparados = NULL) {
  if (inherits(x, "POSIXt")) {
    return(list(valores = as.numeric(x), clase = "fecha-hora",
                n_fechas_resumidas = sum(is.finite(x)),
                n_fechas_excluidas_granularidad = 0L))
  }
  if (inherits(x, "Date")) {
    return(list(valores = as.numeric(x) * 86400, clase = "fecha",
                n_fechas_resumidas = sum(!is.na(x)),
                n_fechas_excluidas_granularidad = 0L))
  }
  if (inherits(x, "integer64")) {
    return(list(valores = x, clase = "integer64",
                n_fechas_resumidas = NA_integer_,
                n_fechas_excluidas_granularidad = NA_integer_))
  }
  if (is.numeric(x)) {
    return(list(valores = as.numeric(x), clase = "numero",
                n_fechas_resumidas = NA_integer_,
                n_fechas_excluidas_granularidad = NA_integer_))
  }
  if (is.character(x) || is.factor(x)) {
    if (inferencia$tipo %in% c("entero", "doble")) {
      valores <- suppressWarnings(as.numeric(sub(",", ".", as.character(x), fixed = TRUE)))
      return(list(valores = valores, clase = "numero",
                  n_fechas_resumidas = NA_integer_,
                  n_fechas_excluidas_granularidad = NA_integer_))
    }
    if (inferencia$tipo %in% c("fecha", "fecha-hora")) {
      granularidades <- if (is.data.frame(formatos) &&
          "granularidad" %in% names(formatos)) {
        formatos$granularidad[formatos$estado == "confirmado"]
      } else character()
      tiene_mes <- any(granularidades == "mes")
      tiene_dia <- any(granularidades == "dia")
      if (tiene_mes && !tiene_dia) {
        return(list(
          valores = numeric(), clase = "fecha_granularidad_incompleta",
          n_fechas_resumidas = 0L, n_fechas_excluidas_granularidad = sum(
            formatos$n[formatos$estado == "confirmado" &
              formatos$granularidad == "mes"], na.rm = TRUE
          )
        ))
      }
      fechas <- .parsear_fechas(
        x, formatos, meses_texto = meses_texto, vocabulario = vocabulario,
        valores_preparados = valores_preparados
      )
      excluidas <- if (tiene_mes) {
        sum(formatos$n[formatos$estado == "confirmado" &
          formatos$granularidad == "mes"], na.rm = TRUE)
      } else 0L
      return(list(
        valores = as.numeric(fechas), clase = inferencia$tipo,
        estado = if (tiene_mes) "calculados_sobre_dias" else "calculados",
        n_fechas_resumidas = sum(is.finite(fechas)),
        n_fechas_excluidas_granularidad = excluidas
      ))
    }
  }
  list(valores = numeric(), clase = "ninguna",
       n_fechas_resumidas = NA_integer_,
       n_fechas_excluidas_granularidad = NA_integer_)
}

# Cuantas veces el hueco mas grande tiene que superar al tipico para que el
# valor que lo abre deje de ser cola y pase a ser anomalia.
#
# El numero no se eligio: se barrio. Sobre un banco de quince columnas con la
# respuesta conocida, contando por separado los dos errores -callar un dato malo
# cuesta mas que hablar de mas-, la meseta sin errores va de 10 a 50:
#
#     6, 8    ->  habla de mas 2 veces
#     10 a 50 ->  sin errores
#     75 o mas -> calla un dato malo
#
# Estaba en 10, pegado al borde inferior. 20 queda dentro de la meseta con
# margen de los dos lados, y del lado que importa -callar- el margen es mayor.
# Los otros dos umbrales se barrieron igual: la densidad no da errores entre 0,2
# y 0,6 y esta en 0,5; el minimo de distintos no da errores entre 3 y 15 y esta
# en 5. Los tres estan en el centro de su meseta, no en un borde.
.FACTOR_SALTO_ESCALA <- 20

# La otra via: el hueco mas grande como proporcion del rango. Las numeraciones
# reales del banco no pasan de 0,067 y los casos a informar arrancan en 0,409.
.PROPORCION_SALTO_ESCALA <- 0.25

.resumen_secuencia_entera <- function(x, inferencia, formatos,
                                      umbral_densidad = 0.8,
                                      min_distintos = 20L) {
  vacio <- list(
    densa = FALSE, densidad = NA_real_, n_posiciones = NA_real_,
    n_huecos = NA_real_, hueco_maximo = NA_real_, salto_de_escala = FALSE,
    umbral_densidad = umbral_densidad,
    min_distintos = as.integer(min_distintos)
  )
  # Se miraba el tipo inferido y solo se aceptaba `entero`, pero unas lineas mas
  # abajo se comprueba que todos los valores sean enteros, que es la condicion
  # real. La del tipo dejaba afuera a las columnas guardadas como `double` con
  # valores enteros -que es como llegan casi todas por la puerta DBI, y como las
  # devuelven varios lectores de CSV-, asi que el mismo codigo se describia
  # distinto segun como estuviera almacenado. Medido: un codigo 1..284 con 179
  # valores fuera de los limites de Tukey se callaba como `integer` y se
  # senalaba como `double`.
  if (!as.character(inferencia$tipo) %in% c("entero", "doble")) return(vacio)
  cuantitativos <- .valores_cuantitativos(x, inferencia, formatos)
  if (!identical(cuantitativos$clase, "numero")) return(vacio)
  presentes <- !is.na(x)
  valores <- cuantitativos$valores
  if (!any(presentes) || length(valores) != length(x) ||
      any(!is.finite(valores[presentes])) ||
      any(valores[presentes] != floor(valores[presentes]))) {
    return(vacio)
  }
  distintos <- sort(unique(valores[presentes]))
  if (!length(distintos)) return(vacio)
  n_posiciones <- max(distintos) - min(distintos) + 1
  if (!is.finite(n_posiciones) || n_posiciones < 1) return(vacio)
  densidad <- length(distintos) / n_posiciones
  # Un valor separado del resto por un hueco desproporcionado no es la cola de
  # la numeracion: es una anomalia, y es justo lo que hay que mirar. Un `120`
  # entre edades de 18 a 70 deja un hueco de 50 donde el tipico es 1; un `2000`
  # detras de 1..1000 deja uno de 1.000. Sin esta medida, la densidad sola
  # tapaba los dos casos, porque un valor fuera de escala de hasta el doble del
  # maximo no baja la densidad lo suficiente.
  huecos <- if (length(distintos) > 1L) diff(distintos) else numeric()
  hueco_tipico <- if (length(huecos)) stats::median(huecos) else NA_real_
  if (!is.finite(hueco_tipico) || hueco_tipico <= 0) hueco_tipico <- 1
  hueco_maximo <- if (length(huecos)) max(huecos) else 0
  list(
    densa = length(distintos) >= min_distintos &&
      densidad >= umbral_densidad,
    densidad = as.numeric(densidad),
    n_posiciones = as.numeric(n_posiciones),
    n_huecos = as.numeric(n_posiciones - length(distintos)),
    hueco_maximo = as.numeric(hueco_maximo),
    hueco_tipico = as.numeric(hueco_tipico),
    # Hay salto de escala por dos vias, y hacen falta las dos. La primera
    # compara el hueco con el tipico, y se le escapa una columna de pocos
    # valores: puntajes del 1 al 13 con un 22 repetido veinte veces abren un
    # hueco de 9 sobre un tipico de 1, que no llega al factor y deja la columna
    # clasificada como numeracion, callando veinte valores extremos.
    #
    # La segunda mira el hueco contra el RANGO, y ahi la separacion es limpia:
    # las numeraciones reales llegan a 0,067 -el mayor es un codigo de 1 a 15- y
    # los casos que hay que informar empiezan en 0,409. Entre medio no hay
    # ninguno, asi que cualquier umbral entre 0,10 y 0,40 da cero errores sobre
    # el banco; 0,25 queda en el centro de esa meseta.
    salto_de_escala = length(distintos) >= 3L && hueco_maximo > 1 &&
      (hueco_maximo >= .FACTOR_SALTO_ESCALA * hueco_tipico ||
         hueco_maximo / n_posiciones >= .PROPORCION_SALTO_ESCALA),
    umbral_densidad = umbral_densidad,
    min_distintos = as.integer(min_distintos)
  )
}

# Un valor centinela -el `9999` que quiere decir "no sabemos"- no se puede
# reconocer por su forma sola: `9999` es una edad imposible y un codigo postal
# perfectamente valido. Por eso la lista de `sentinelas_numericos` no lo trae
# por omision, y hace bien: marcarlo siempre produciria falsos en cualquier
# columna donde ese numero es un dato.
#
# Lo que si lo distingue es que cumpla las **tres** cosas a la vez: que sea un
# valor extremo, que se repita, y que tenga forma de centinela. Un codigo postal
# 9999 repetido treinta veces no es extremo dentro de su columna; un monto real
# de 9999 no se repite; un ano 1999 no tiene forma de centinela. Medido sobre
# ocho columnas con la respuesta conocida -tres con centinela y cinco donde el
# mismo numero es un dato real-, las tres senales juntas aciertan las ocho.
#
# Esto no cambia `faltantes_disfrazados`, que sigue contando solo lo que la
# lista declarada dice. Es una senal aparte, que se informa para que el usuario
# decida si agrega ese valor a su lista.
.MIN_REPETICIONES_CENTINELA <- 5L

# **Se probo descartar al candidato que tiene vecinos inmediatos y se retiro.**
# La idea era que un centinela esta solo mientras que un codigo de catalogo vive
# en un tramo, y sobre el caso que la motivo funcionaba: `222` entre `221` y
# `223` daba razon de vecinos 1,00 y los centinelas sueltos 0,00.
#
# Lo que la tumbo es la codificacion mas comun que existe en microdatos de
# encuesta: `9999` = "no sabe" junto a `9998` = "no contesta". Medido sobre una
# columna de horas trabajadas con treinta `9999` y veinticinco `9998`, el `9998`
# contaba como vecino -razon 0,833-, descartaba al `9999`, y como el `9998` no
# tiene forma de digito repetido tampoco entraba por su cuenta: los dos
# centinelas quedaban invisibles Y la columna dejaba de verse como numeracion,
# asi que salian **cincuenta y cinco codigos de ausencia informados como valores
# extremos de una magnitud**.
#
# No hay senal de forma que separe `221/222/223` de `9998/9999`: en los dos
# casos son valores contiguos, extremos y repetidos. La diferencia es semantica.
# Y la regla del paquete es que una guarda solo entra si no calla nada real, asi
# que se prefiere el falso positivo del `222` -un aviso que dice que lo decida
# quien conoce la columna- antes que callar un centinela verdadero.

.centinela_por_tres_senales <- function(valores, iqr,
                                       sentinelas_numericos = NULL,
                                       q1 = NULL, q3 = NULL) {
  vacio <- list(valor = NA_real_, n = NA_integer_,
                densidad_sin_centinela = NA_real_)
  if (!length(valores) || !is.finite(iqr) || iqr <= 0) return(vacio)
  if (length(valores) < 20L) return(vacio)
  if (is.null(q1) || is.null(q3)) {
    q <- stats::quantile(
      valores, probs = c(0.25, 0.75), names = FALSE, type = 7
    )
    q1 <- q[[1L]]
    q3 <- q[[2L]]
  }
  extremos <- valores[valores < q1 - 1.5 * iqr |
                        valores > q3 + 1.5 * iqr]
  if (!length(extremos)) return(vacio)
  frecuencias <- table(extremos)
  repetidos <- suppressWarnings(
    as.numeric(names(frecuencias)[frecuencias >= .MIN_REPETICIONES_CENTINELA])
  )
  if (!length(repetidos)) return(vacio)
  # Un digito repetido tres veces o mas, con o sin signo: `999`, `-999`, `9999`,
  # `8888`, `7777`. Empezo siendo solo nueves y se amplio midiendo: sobre doce
  # columnas con la respuesta conocida, solo-nueves acertaba diez y se perdia
  # dos centinelas reales -`8888` y `7777` en columnas de edad-, y el patron
  # ancho acierta las doce sin inventar ninguno.
  #
  # Los controles que se le pasaron: un codigo `111` en una columna de 100 a
  # 200, un `0` repetido veinticinco veces entre cantidades, y un `222` entre
  # mediciones de 200 a 260. Ninguno es extremo dentro de su columna.
  #
  # **Y ahi estaba la trampa**, que se descubrio despues: los tres controles
  # eligen valores que no son extremos, o sea que ninguno ejercita la unica
  # situacion en que esta senal puede hacer dano. Se probaba el caso comodo y se
  # leia el resultado como si cubriera el dificil. El `222` de un tramo de
  # codigos altos dentro de una columna concentrada abajo SI es extremo, y ahi
  # esta forma decidia sola. Lo frena la guarda de vecinos, mas abajo.
  con_forma <- repetidos[grepl("^-?([0-9])\\1{2,}$", as.character(repetidos))]
  # Los valores que la lista declarada YA cuenta como ausencia no son asunto de
  # este diagnostico: `faltantes_disfrazados` los informa, y con severidad
  # `error`. Sin este descarte los dos hallazgos se contradecian sobre la misma
  # columna -uno decia que el `999` era una ausencia y el otro que "no se cuenta
  # como ausencia porque no esta declarado", cuando si lo estaba-.
  declarados <- suppressWarnings(as.numeric(sentinelas_numericos))
  declarados <- declarados[is.finite(declarados)]
  if (length(declarados)) con_forma <- setdiff(con_forma, declarados)
  if (!length(con_forma)) return(vacio)
  elegido <- con_forma[[which.max(abs(con_forma))]]
  # Una numeracion con un centinela adentro deja de parecer numeracion: el
  # `9999` abre un hueco enorme y hunde la densidad, asi que sobre un
  # identificador de 1 a 4000 con quince `9999` se volvian a correr Benford y
  # los limites de Tukey. Medido: la densidad cae de 0,625 a 0,250 por culpa del
  # centinela.
  #
  # La columna sin el centinela es lo que hay que mirar para decidir si es una
  # numeracion. El centinela se informa igual, por su cuenta.
  restantes <- valores[valores != elegido]
  distintos <- unique(restantes)
  densidad_limpia <- if (length(distintos) > 1L) {
    rango <- max(distintos) - min(distintos) + 1
    if (is.finite(rango) && rango > 0) length(distintos) / rango else NA_real_
  } else NA_real_
  list(
    valor = as.numeric(elegido),
    n = as.integer(sum(valores == elegido)),
    densidad_sin_centinela = as.numeric(densidad_limpia)
  )
}

.resumen_vacio_cuantitativo <- function(estado = "no_aplica") {
  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_,
    mediana = NA_real_, desvio = NA_real_, minimo_exacto = NA_character_,
    maximo_exacto = NA_character_, minimo_fecha = NA_character_,
    maximo_fecha = NA_character_, media_fecha = NA_character_,
    mediana_fecha = NA_character_, n_ceros = NA_integer_,
    n_negativos = NA_integer_, n_outliers = NA_integer_,
    centinela_valor = NA_real_, centinela_repeticiones = NA_integer_,
    densidad_sin_centinela = NA_real_, n_nan = 0L,
    n_infinito_positivo = 0L, n_infinito_negativo = 0L,
    n_fechas_resumidas = NA_integer_,
    n_fechas_excluidas_granularidad = NA_integer_,
    estado_resumen_cuantitativo = estado
  )
}

.resumen_integer64 <- function(x) {
  vacio <- .resumen_vacio_cuantitativo("sin_valores")
  validos <- !is.na(x)
  if (!any(validos)) return(vacio)
  if (!.bit64_disponible()) {
    vacio$estado_resumen_cuantitativo <- "requiere_bit64"
    return(vacio)
  }
  valores <- x[validos]
  minimo_exacto <- as.character(min(valores))
  maximo_exacto <- as.character(max(valores))
  limite <- bit64::as.integer64("9007199254740991")
  seguros <- valores >= -limite & valores <= limite
  if (!all(seguros)) {
    vacio$minimo_exacto <- minimo_exacto
    vacio$maximo_exacto <- maximo_exacto
    vacio$n_ceros <- as.integer(sum(valores == bit64::as.integer64(0)))
    vacio$n_negativos <- as.integer(sum(valores < bit64::as.integer64(0)))
    vacio$estado_resumen_cuantitativo <- "omitidos_precision"
    return(vacio)
  }
  resultado <- .resumen_cuantitativo(
    as.numeric(valores),
    list(tipo = "doble"),
    data.frame(stringsAsFactors = FALSE)
  )
  resultado$minimo_exacto <- minimo_exacto
  resultado$maximo_exacto <- maximo_exacto
  resultado
}

.fecha_resumida <- function(valor, clase) {
  if (!is.finite(valor)) {
    return(NA_character_)
  }
  if (identical(clase, "fecha")) {
    return(format(
      as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d", tz = "UTC"
    ))
  }
  format(
    as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"
  )
}

.resumen_cuantitativo <- function(x, inferencia, formatos,
                                  meses_texto = NULL,
                                  sentinelas_numericos = NULL,
                                  vocabulario = NULL,
                                  valores_preparados = NULL) {
  cuantitativos <- .valores_cuantitativos(
    x, inferencia, formatos, meses_texto = meses_texto,
    vocabulario = vocabulario, valores_preparados = valores_preparados
  )
  if (identical(cuantitativos$clase, "fecha_granularidad_incompleta")) {
    salida <- .resumen_vacio_cuantitativo("granularidad_incompleta")
    salida$n_fechas_resumidas <- cuantitativos$n_fechas_resumidas
    salida$n_fechas_excluidas_granularidad <-
      cuantitativos$n_fechas_excluidas_granularidad
    return(salida)
  }
  if (identical(cuantitativos$clase, "integer64")) {
    return(.resumen_integer64(cuantitativos$valores))
  }
  valores <- cuantitativos$valores
  n_nan <- if (is.numeric(valores)) sum(is.nan(valores)) else 0L
  n_infinito_positivo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
  } else 0L
  n_infinito_negativo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
  } else 0L
  valores <- valores[is.finite(valores)]
  vacio <- .resumen_vacio_cuantitativo(if (
    identical(cuantitativos$clase, "ninguna")) "no_aplica" else "sin_valores"
  )
  vacio$n_nan <- as.integer(n_nan)
  vacio$n_infinito_positivo <- as.integer(n_infinito_positivo)
  vacio$n_infinito_negativo <- as.integer(n_infinito_negativo)
  if (!length(valores)) {
    return(vacio)
  }

  basicos <- .resumen_cuantitativo_bloques(
    valores, contar_signos = identical(cuantitativos$clase, "numero")
  )
  minimo <- basicos$minimo
  maximo <- basicos$maximo
  media <- basicos$media
  cuartiles <- stats::quantile(
    valores, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7
  )
  # La mediana sigue saliendo de `median()` y no del cuantil 0,5. No son lo
  # mismo hasta el ultimo bit: `median()` promedia los dos centrales con
  # `(a + b) / 2` y `quantile(type = 7)` interpola con `a + 0,5 * (b - a)`, que
  # redondean distinto cuando los centrales son de magnitudes muy dispares.
  # Medido con `c(-1000, 0.000111, 0.25, 1000)`: 0,12505549999999999 contra
  # 0,12505550000000001, y la diferencia llegaba a la mediana informada.
  # Ahorrar un recorrido no vale cambiar un numero que se publica.
  mediana <- stats::median(valores)
  desvio <- basicos$desvio
  # `valores` ya fue filtrado con `is.finite()`: `na.rm = TRUE` conserva la
  # semántica del IQR anterior y también deja la llamada segura si este bloque
  # vuelve a recibir valores ausentes en el futuro.
  iqr <- cuartiles[[2L]] - cuartiles[[1L]]
  centinela <- .centinela_por_tres_senales(
    valores, iqr, sentinelas_numericos,
    q1 = cuartiles[[1L]], q3 = cuartiles[[2L]]
  )
  if (is.finite(iqr)) {
    n_outliers <- sum(valores < cuartiles[[1L]] - 1.5 * iqr |
      valores > cuartiles[[2L]] + 1.5 * iqr)
  } else {
    n_outliers <- 0L
  }

  if (identical(cuantitativos$clase, "numero")) {
    return(list(
      minimo = minimo, maximo = maximo, media = media, mediana = mediana,
      desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
      minimo_fecha = NA_character_, maximo_fecha = NA_character_,
      media_fecha = NA_character_, mediana_fecha = NA_character_,
      n_ceros = basicos$n_ceros, n_negativos = basicos$n_negativos,
      n_outliers = n_outliers, centinela_valor = centinela$valor,
      centinela_repeticiones = centinela$n,
      densidad_sin_centinela = centinela$densidad_sin_centinela,
      n_nan = as.integer(n_nan),
      n_infinito_positivo = as.integer(n_infinito_positivo),
      n_infinito_negativo = as.integer(n_infinito_negativo),
      n_fechas_resumidas = NA_integer_,
      n_fechas_excluidas_granularidad = NA_integer_,
      estado_resumen_cuantitativo = "calculados"
    ))
  }

  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_, mediana = NA_real_,
    desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
    minimo_fecha = .fecha_resumida(minimo, cuantitativos$clase),
    maximo_fecha = .fecha_resumida(maximo, cuantitativos$clase),
    media_fecha = .fecha_resumida(media, cuantitativos$clase),
    mediana_fecha = .fecha_resumida(mediana, cuantitativos$clase),
    n_ceros = 0L, n_negativos = 0L, n_outliers = n_outliers,
    centinela_valor = centinela$valor, centinela_repeticiones = centinela$n,
    densidad_sin_centinela = centinela$densidad_sin_centinela,
    n_nan = as.integer(n_nan),
    n_infinito_positivo = as.integer(n_infinito_positivo),
    n_infinito_negativo = as.integer(n_infinito_negativo),
    n_fechas_resumidas = if (startsWith(cuantitativos$clase, "fecha") &&
        !is.null(cuantitativos$n_fechas_resumidas)) {
      cuantitativos$n_fechas_resumidas
    } else NA_integer_,
    n_fechas_excluidas_granularidad = if (
      startsWith(cuantitativos$clase, "fecha") &&
        !is.null(cuantitativos$n_fechas_excluidas_granularidad)
    ) cuantitativos$n_fechas_excluidas_granularidad else NA_integer_,
    estado_resumen_cuantitativo = if (!is.null(cuantitativos$estado)) {
      cuantitativos$estado
    } else "calculados"
  )
}

.umbral_vocabulario_barato <- 0.5
.umbral_vocabulario_codificacion <- 0.8

# La deduplicacion se activa segun la cardinalidad. En 240.000 filas, cuatro
# valores fueron claramente favorables y 132.610 valores ya hicieron mas caro
# el recorrido para predicados baratos; la reparacion de codificacion siguio
# siendo favorable hasta una relacion de 0,8.
.vocabulario_texto <- function(textos, umbral, valores = NULL) {
  n <- length(textos)
  presentes <- !is.na(textos)
  if (is.null(valores)) valores <- unique(textos[presentes])
  distintos <- length(valores)
  usar <- n > 1L && distintos > 0L && distintos / n <= umbral
  if (usar) {
    indices <- match(textos, valores)
  } else {
    valores <- textos
    indices <- seq_len(n)
  }
  list(
    valores = valores,
    indices = indices,
    usar = usar,
    n_distintos = distintos
  )
}

.componentes_numero_texto_optimizado <- function(textos, valores = NULL) {
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_barato, valores = valores
  )
  partes <- .componentes_numero_texto(vocabulario$valores)
  if (isTRUE(vocabulario$usar)) {
    partes <- lapply(partes, function(valores) valores[vocabulario$indices])
  }
  partes
}

.analizar_codificacion_vocabulario <- function(textos, valores = NULL) {
  if (!is.null(valores) && length(textos) > 1L &&
      length(valores) / length(textos) > .umbral_vocabulario_codificacion) {
    return(.analizar_codificacion(textos))
  }
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_codificacion, valores = valores
  )
  if (!isTRUE(vocabulario$usar)) {
    return(.analizar_codificacion(textos))
  }
  unico <- .analizar_codificacion(vocabulario$valores)
  reparados <- rep(NA_character_, length(textos))
  estados <- rep(NA_character_, length(textos))
  pasos <- vector("list", length(textos))
  presentes <- !is.na(vocabulario$indices)
  reparados[presentes] <- unico$reparados[vocabulario$indices[presentes]]
  estados[presentes] <- unico$estados[vocabulario$indices[presentes]]
  pasos[presentes] <- unico$pasos[vocabulario$indices[presentes]]
  reparables <- !is.na(reparados) & !is.na(textos) &
    reparados != textos & estados == "reparado"
  parciales <- !is.na(reparados) & !is.na(textos) &
    reparados != textos & estados == "reparado_parcialmente"
  irreparables <- !is.na(textos) & grepl("\uFFFD", textos, fixed = TRUE)
  afectados <- reparables | parciales | irreparables
  ejemplos <- utils::head(which(afectados), 5L)
  evidencia <- if (!length(ejemplos)) "" else paste(vapply(ejemplos, function(i) {
    origen <- encodeString(textos[[i]], quote = '"')
    if (reparables[[i]] || parciales[[i]]) {
      paste0(origen, " -> ", encodeString(reparados[[i]], quote = '"'),
             " [", estados[[i]], "]")
    } else {
      paste0(origen, " (contiene un caracter de reemplazo irrecuperable)")
    }
  }, character(1L)), collapse = "; ")
  list(
    n = sum(afectados), n_reparables = sum(reparables),
    n_reparables_parcialmente = sum(parciales),
    n_irreparables = sum(irreparables),
    n_no_se_pudo = sum(estados == "no_se_pudo", na.rm = TRUE),
    evidencia = evidencia, reparados = reparados, estados = estados,
    pasos = pasos, afectados = afectados,
    estado = .ftfy_estado_agregado(estados)
  )
}

.componentes_numero_texto <- function(x) {
  texto <- trimws(as.character(x))
  prefijo <- "(?:[[:upper:]]{3}[[:space:]]+|U\\$S[[:space:]]+|\\p{Sc}[[:space:]]*)?"
  sufijo <- "[[:space:]]*(?:%|[[:alpha:]]+)?$"
  numero_coma <- paste0(
    "(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+)(?:,[0-9]+)?"
  )
  numero_punto <- paste0(
    "(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?"
  )
  patron_coma <- paste0("^", prefijo, "[+-]?", numero_coma, sufijo)
  patron_punto <- paste0("^", prefijo, "[+-]?", numero_punto, sufijo)
  compatible_coma <- !is.na(texto) & grepl(patron_coma, texto, perl = TRUE)
  compatible_punto <- !is.na(texto) & grepl(patron_punto, texto, perl = TRUE)
  compatible <- compatible_coma | compatible_punto
  patron_prefijo <- paste0(
    "^[[:space:]]*(?:[[:upper:]]{3}[[:space:]]+|U\\$S[[:space:]]+|\\p{Sc}[[:space:]]*)"
  )
  moneda <- rep("", length(texto))
  patron_codigo_prefijo <- "^[[:space:]]*([[:upper:]]{3})[[:space:]]+"
  codigo_prefijo <- compatible & grepl(
    patron_codigo_prefijo, texto, perl = TRUE
  )
  moneda[codigo_prefijo] <- sub(
    paste0(patron_codigo_prefijo, ".*$"), "\\1", texto[codigo_prefijo],
    perl = TRUE
  )
  patron_dolar_us_prefijo <- "^[[:space:]]*(U\\$S)[[:space:]]+"
  dolar_us_prefijo <- compatible & grepl(
    patron_dolar_us_prefijo, texto, perl = TRUE
  )
  moneda[dolar_us_prefijo] <- "U$S"
  patron_simbolo_prefijo <- "^[[:space:]]*(\\p{Sc})[[:space:]]*"
  simbolo_prefijo <- compatible & grepl(
    patron_simbolo_prefijo, texto, perl = TRUE
  )
  moneda[simbolo_prefijo] <- sub(
    paste0(patron_simbolo_prefijo, ".*$"), "\\1",
    texto[simbolo_prefijo], perl = TRUE
  )
  patron_codigo_sufijo <- "[[:space:]]+([[:upper:]]{3})$"
  codigo_sufijo <- compatible & grepl(patron_codigo_sufijo, texto, perl = TRUE)
  sin_prefijo <- codigo_sufijo & !nzchar(moneda)
  moneda[sin_prefijo] <- sub(
    paste0("^.*", patron_codigo_sufijo), "\\1", texto[sin_prefijo],
    perl = TRUE
  )
  tiene_moneda <- nzchar(moneda)
  cuerpo <- texto
  cuerpo <- sub(patron_prefijo, "", cuerpo, perl = TRUE)
  # Un sufijo de unidad se reconoce de forma deliberadamente acotada: `%` o
  # una abreviatura alfabetica en minusculas. Las letras mayusculas pegadas al
  # numero (`12A`, `13B`) se reservan para codigos, no para inventar unidades.
  tiene_unidad <- grepl("(?:%|[[:lower:]]{1,8})$", cuerpo, perl = TRUE)
  unidad <- ifelse(
    compatible & tiene_unidad,
    sub("^.*?[[:space:]]*(%|[[:alpha:]]+)$", "\\1", cuerpo, perl = TRUE),
    ""
  )
  sin_unidad <- sub("[[:space:]]*(?:%|[[:alpha:]]+)$", "", cuerpo, perl = TRUE)
  tiene_coma <- grepl(",", sin_unidad, fixed = TRUE)
  tiene_punto <- grepl(".", sin_unidad, fixed = TRUE)
  punto_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:\\.[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  coma_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:,[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  evidencia_coma <- compatible_coma & (
    grepl("\\.[0-9]{3}(?:\\.[0-9]{3})*,[0-9]+$", sin_unidad, perl = TRUE) |
      grepl(",[0-9]{1,2}$|,[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?[0-9]{4,},[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  evidencia_punto <- compatible_punto & (
    grepl(",[0-9]{3}(?:,[0-9]{3})*\\.[0-9]+$", sin_unidad, perl = TRUE) |
      grepl("\\.[0-9]{1,2}$|\\.[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?[0-9]{4,}\\.[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  especial <- tiene_coma | tiene_punto | tiene_moneda | nzchar(unidad)
  list(
    texto = texto, compatible = compatible, especial = especial,
    cuerpo = sin_unidad, unidad = unidad, tiene_coma = tiene_coma,
    tiene_punto = tiene_punto, punto_tres = punto_tres, coma_tres = coma_tres,
    compatible_coma = compatible_coma, compatible_punto = compatible_punto,
    evidencia_coma = evidencia_coma, evidencia_punto = evidencia_punto,
    moneda = moneda
  )
}

.frecuencias_unidades_numero <- function(partes, presentes) {
  unidades <- partes$unidad[presentes & partes$compatible & nzchar(partes$unidad)]
  if (!length(unidades)) return(stats::setNames(integer(), character()))
  niveles <- unique(unidades)
  salida <- tabulate(match(unidades, niveles), nbins = length(niveles))
  stats::setNames(as.integer(salida), niveles)
}

.frecuencias_monedas_numero <- function(partes, presentes) {
  monedas <- partes$moneda[presentes & partes$compatible & nzchar(partes$moneda)]
  if (!length(monedas)) return(stats::setNames(integer(), character()))
  niveles <- unique(monedas)
  salida <- tabulate(match(monedas, niveles), nbins = length(niveles))
  stats::setNames(as.integer(salida), niveles)
}

.tipo_parte_multivaluada <- function(valor) {
  if (grepl("^[+]?[0-9]+$", valor, perl = TRUE)) return("numerico")
  if (grepl("^[[:alnum:]]+$", valor, perl = TRUE) &&
      grepl("[[:alpha:]]", valor, perl = TRUE) &&
      grepl("[0-9]", valor, perl = TRUE)) return("alfanumerico")
  if (grepl("^[+]?[0-9]{4,8}[-./][0-9]+$", valor, perl = TRUE)) {
    return("identificador")
  }
  "texto"
}

.patron_partes_multivaluada <- function(valores, expandir = FALSE) {
  # La tabla publica es la misma evidencia que usa el perfil; el vector por
  # parte se conserva solo para comparar homogeneidad dentro de una celda.
  descubrir_patrones(
    valores, distinguir_mayusculas = TRUE, expandir = expandir,
    max_patrones = 100L, muestra = Inf, umbral_raro = 0
  )
  .generalizar_a_patron(
    valores, distinguir_mayusculas = TRUE, expandir = expandir
  )
}

.detectar_multivaluados <- function(x) {
  vacio <- list()
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(trimws(textos))
  n_presentes <- sum(presentes)
  if (n_presentes < 2L) return(vacio)

  candidatos <- list()
  for (delimitador in c(",", ";", "|")) {
    indices <- which(presentes & grepl(delimitador, textos, fixed = TRUE))
    if (!length(indices)) next
    partes_por_celda <- lapply(indices, function(indice) {
      partes <- trimws(strsplit(textos[[indice]], delimitador, fixed = TRUE)[[1L]])
      if (length(partes) < 2L || any(!nzchar(partes))) character() else partes
    })
    validas <- lengths(partes_por_celda) >= 2L
    if (!any(validas)) next
    indices <- indices[validas]
    partes_por_celda <- partes_por_celda[validas]
    if (delimitador == ",") {
      # No convertir un decimal o un separador de miles en una lista de
      # valores: si la celda completa ya es un numero regional, se excluye.
      numericas <- vapply(textos[indices], function(valor) {
        .componentes_numero_texto(valor)$compatible
      }, logical(1L))
      indices <- indices[!numericas]
      partes_por_celda <- partes_por_celda[!numericas]
    }
    if (!length(indices)) next
    partes <- unlist(partes_por_celda, use.names = FALSE)
    tipos <- vapply(partes, .tipo_parte_multivaluada, character(1L))
    tipo_dominante <- unique(tipos)
    if (length(tipo_dominante) != 1L ||
        !tipo_dominante %in% c("numerico", "alfanumerico", "identificador")) {
      next
    }
    patron_expandido <- .patron_partes_multivaluada(partes, expandir = TRUE)
    patron_comprimido <- .patron_partes_multivaluada(partes, expandir = FALSE)
    homogeneas <- if (tipo_dominante == "numerico") {
      length(unique(patron_comprimido)) == 1L &&
        (max(nchar(partes)) - min(nchar(partes)) <= 2L)
    } else {
      length(unique(patron_expandido)) == 1L &&
        length(unique(nchar(partes))) == 1L
    }
    if (!homogeneas) next

    indices_resto <- which(presentes & !grepl(
      "[,;|]", textos, perl = TRUE
    ))
    if (length(indices_resto)) {
      resto <- trimws(textos[indices_resto])
      tipos_resto <- vapply(resto, .tipo_parte_multivaluada, character(1L))
      resto <- resto[tipos_resto == tipo_dominante]
      if (!length(resto)) next
      patrones_resto <- .patron_partes_multivaluada(
        resto, expandir = tipo_dominante != "numerico"
      )
      patrones_referencia <- if (tipo_dominante == "numerico") {
        unique(patron_comprimido)
      } else unique(patron_expandido)
      if (!all(patrones_resto %in% patrones_referencia)) next
    }
    if (length(indices) > max(1L, floor(n_presentes / 2L))) next
    candidatos[[length(candidatos) + 1L]] <- list(
      delimitador = delimitador,
      indices = indices,
      n_celdas = length(indices),
      valores_por_celda = table(lengths(partes_por_celda)),
      n_valores = length(partes)
    )
  }
  if (!length(candidatos)) return(vacio)
  indices <- sort(unique(unlist(lapply(candidatos, `[[`, "indices"))))
  distribuciones <- lapply(candidatos, `[[`, "valores_por_celda")
  niveles <- sort(unique(as.integer(unlist(lapply(
    distribuciones, names
  )))))
  frecuencias_valores <- stats::setNames(vapply(niveles, function(n) {
    sum(vapply(distribuciones, function(distribucion) {
      valor <- distribucion[[as.character(n)]]
      if (is.null(valor)) 0L else as.integer(valor)
    }, integer(1L)))
  }, integer(1L)), as.character(niveles))
  list(list(
    delimitador = paste(vapply(candidatos, `[[`, character(1L), "delimitador"),
                        collapse = " / "),
    indices = indices,
    n_celdas = length(indices),
    valores_por_celda = frecuencias_valores,
    n_valores = sum(vapply(candidatos, `[[`, numeric(1L), "n_valores"))
  ))
}

# Las dos formas de analizar numeros escritos como texto -la directa y la que
# pasa por el vocabulario- comparten toda la interpretacion: convencion decimal,
# unidad y moneda. Lo unico que cambia entre ellas es COMO se obtienen `partes`,
# `presentes` y `especiales`; una vez que estan, la lectura es la misma.
#
# Estaba escrita dos veces, cuarenta y ocho lineas cada una. No es solo
# repeticion: las dos tienen que dar el mismo veredicto sobre los mismos
# valores, o la misma columna se describiria distinto segun por que camino se
# la miro.
.interpretar_numeros_texto <- function(partes, presentes, especiales,
                                      n_presentes) {
  hay_evidencia_coma <- any(partes$evidencia_coma[presentes])
  hay_evidencia_punto <- any(partes$evidencia_punto[presentes])
  convencion <- if (hay_evidencia_coma && hay_evidencia_punto) {
    "mixta"
  } else if (hay_evidencia_coma) {
    "decimal_coma"
  } else if (hay_evidencia_punto) {
    "decimal_punto"
  } else if (any((partes$punto_tres | partes$coma_tres) & especiales)) {
    "ambigua"
  } else {
    "sin_separadores"
  }
  compatibles_convencion <- switch(
    convencion,
    decimal_coma = partes$compatible_coma,
    decimal_punto = partes$compatible_punto,
    sin_separadores = partes$compatible,
    partes$compatible
  )
  ambiguos <- convencion %in% c("ambigua", "mixta") |
    any(presentes & !compatibles_convencion)
  unidades <- unique(partes$unidad[presentes & partes$compatible])
  unidades_no_vacias <- unidades[nzchar(unidades)]
  unidad_consistente <- length(unidades_no_vacias) <= 1L &&
    !(length(unidades_no_vacias) && any(!nzchar(unidades)))
  monedas <- unique(partes$moneda[presentes & partes$compatible])
  monedas_no_vacias <- monedas[nzchar(monedas)]
  moneda_consistente <- length(monedas_no_vacias) <= 1L &&
    !(length(monedas_no_vacias) && any(!nzchar(monedas)))
  compatibles <- sum(presentes & partes$compatible)
  list(
    n = sum(especiales),
    proporcion = if (n_presentes) compatibles / n_presentes else NA_real_,
    ambiguo = isTRUE(ambiguos),
    seguro = compatibles == n_presentes && !isTRUE(ambiguos) &&
      unidad_consistente && moneda_consistente,
    evidencia = paste(
      encodeString(utils::head(unique(partes$texto[especiales]), 6L), quote = '"'),
      collapse = "; "
    ),
    unidad = if (length(unidades_no_vacias) == 1L) unidades_no_vacias else "",
    moneda = if (length(monedas_no_vacias) == 1L) monedas_no_vacias else "",
    convencion = convencion,
    unidades = .frecuencias_unidades_numero(partes, presentes),
    monedas = .frecuencias_monedas_numero(partes, presentes),
    n_presentes = n_presentes
  )
}

.analizar_numeros_texto_directo <- function(x, umbral_compatibilidad = 0.8) {
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    unidades = stats::setNames(integer(), character()),
    monedas = stats::setNames(integer(), character()), n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(textos)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (!n_presentes || !any(grepl("[0-9]", textos[presentes], perl = TRUE))) {
    return(vacio)
  }
  inicio_numerico <- grepl(
    paste0(
      "^[[:space:]]*(?:(?:[[:upper:]]{3}|U\\$S)[[:space:]]+|",
      "\\p{Sc}[[:space:]]*)?[+-]?[0-9]"
    ), textos[presentes], perl = TRUE
  )
  if (mean(inicio_numerico) < umbral_compatibilidad) return(vacio)
  partes <- .componentes_numero_texto(textos)
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) return(vacio)
  .interpretar_numeros_texto(partes, presentes, especiales, n_presentes)
}

.analizar_numeros_texto <- function(x, umbral_compatibilidad = 0.8,
                                    vocabulario = NULL, n_distintos = NULL,
                                    directo = FALSE) {
  if (isTRUE(directo)) {
    return(.analizar_numeros_texto_directo(x, umbral_compatibilidad))
  }
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    unidades = stats::setNames(integer(), character()),
    monedas = stats::setNames(integer(), character()), n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(textos)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (is.null(vocabulario) && !is.null(n_distintos) &&
      length(textos) > 1L && is.finite(n_distintos) &&
      n_distintos / length(textos) > .umbral_vocabulario_barato) {
    vocabulario_numeros <- list(
      valores = textos, indices = seq_len(length(textos)), usar = FALSE
    )
  } else {
    vocabulario_numeros <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = vocabulario
    )
  }
  if (isTRUE(vocabulario_numeros$usar)) {
    valores_vocabulario <- vocabulario_numeros$valores
    indices_vocabulario <- vocabulario_numeros$indices
    tiene_digitos_vocabulario <- !is.na(valores_vocabulario) &
      grepl("[0-9]", valores_vocabulario, perl = TRUE)
    tiene_digitos <- tiene_digitos_vocabulario[indices_vocabulario]
  } else {
    valores_vocabulario <- textos
    indices_vocabulario <- seq_len(length(textos))
    tiene_digitos <- !is.na(textos) &
      grepl("[0-9]", textos, perl = TRUE)
  }
  if (!n_presentes || !any(tiene_digitos[presentes])) {
    return(vacio)
  }
  patron_inicio <- paste0(
    "^[[:space:]]*(?:(?:[[:upper:]]{3}|U\\$S)[[:space:]]+|",
    "\\p{Sc}[[:space:]]*)?[+-]?[0-9]"
  )
  if (isTRUE(vocabulario_numeros$usar)) {
    inicio_vocabulario <- grepl(
      patron_inicio, valores_vocabulario, perl = TRUE
    )
    inicio_numerico <- inicio_vocabulario[indices_vocabulario]
    partes <- .componentes_numero_texto_optimizado(
      textos, valores = valores_vocabulario
    )
  } else {
    inicio_numerico <- rep(FALSE, length(textos))
    inicio_numerico[presentes] <- grepl(
      patron_inicio,
      textos[presentes], perl = TRUE
    )
    partes <- .componentes_numero_texto(textos)
  }
  if (mean(inicio_numerico[presentes]) < umbral_compatibilidad) {
    return(vacio)
  }
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) {
    return(vacio)
  }
  .interpretar_numeros_texto(partes, presentes, especiales, n_presentes)
}

.codigos_espacios_invisibles <- c(
  0x00A0, 0x1680, 0x2000:0x200A, 0x2028, 0x2029, 0x202F,
  0x205F, 0x3000
)

.codigos_invisibles_eliminables <- c(
  0x00AD, 0x061C, 0x180E, 0x200B, 0x200E, 0x200F, 0x202A:0x202E,
  0x2060, 0x2066:0x2069, 0xFEFF
)

.codigos_invisibles_significativos <- c(0x200C, 0x200D)
.codigos_c0_no_separadores <- c(0:8, 14L:31L)
.codigos_c1 <- 127L:159L
.codigos_salto_linea_set <- 9L:13L
.codigos_control_eliminable_set <- c(
  .codigos_c0_no_separadores, .codigos_c1,
  .codigos_invisibles_eliminables
)
.codigos_control_invisible_set <- c(
  .codigos_c0_no_separadores, .codigos_c1,
  .codigos_espacios_invisibles, .codigos_invisibles_eliminables,
  .codigos_invisibles_significativos
)

.codigos_control_eliminable <- function(codigos) {
  # Los separadores C0 (tabulacion, LF, VT, FF y CR) tienen un diagnostico y
  # una estrategia propios. Los restantes C0/C1 son controles de transporte.
  codigos %in% .codigos_control_eliminable_set
}

.codigos_control_invisible <- function(codigos) {
  # La deteccion informa todos los invisibles, incluidos los espacios Unicode
  # y ZWJ/ZWNJ; la remediacion separa los grupos por neutralidad semantica.
  codigos %in% .codigos_control_invisible_set
}

.predicados_invisibles <- function(textos) {
  n <- length(textos)
  salida <- list(
    control = rep(FALSE, n), eliminable = rep(FALSE, n),
    espacio = rep(FALSE, n), significativo = rep(FALSE, n),
    separador = rep(FALSE, n)
  )
  if (!n) return(salida)
  for (i in seq_len(n)) {
    texto <- textos[[i]]
    if (is.na(texto)) next
    # .texto_analizable() sanea los bytes UTF-8 inválidos antes de llegar
    # aquí; por eso esta única pasada no necesita un tryCatch por predicado.
    codigos <- utf8ToInt(texto)
    salida$control[[i]] <- any(codigos %in% .codigos_control_invisible_set)
    salida$eliminable[[i]] <- any(codigos %in% .codigos_control_eliminable_set)
    salida$espacio[[i]] <- any(codigos %in% .codigos_espacios_invisibles)
    salida$significativo[[i]] <- any(codigos %in% .codigos_invisibles_significativos)
    salida$separador[[i]] <- any(codigos %in% .codigos_salto_linea_set)
  }
  salida
}

# El paquete cubre las entidades HTML habituales en datos en espanol y todas
# las referencias numericas Unicode. No se interpreta cualquier texto entre
# '&' y ';': el nombre debe pertenecer a este mapa.
.entidades_html_comunes <- c(
  quot = "\"", amp = "&", apos = "'", lt = "<", gt = ">", nbsp = "\u00a0",
  iexcl = "\u00a1", cent = "\u00a2", pound = "\u00a3", curren = "\u00a4",
  yen = "\u00a5", sect = "\u00a7", copy = "\u00a9", reg = "\u00ae",
  deg = "\u00b0", plusmn = "\u00b1", para = "\u00b6", middot = "\u00b7",
  laquo = "\u00ab", raquo = "\u00bb", iquest = "\u00bf", euro = "\u20ac",
  Aacute = "\u00c1", Acirc = "\u00c2", Atilde = "\u00c3", Auml = "\u00c4",
  Agrave = "\u00c0", Aring = "\u00c5", Ccedil = "\u00c7",
  Eacute = "\u00c9", Ecirc = "\u00ca", Euml = "\u00cb", Egrave = "\u00c8",
  Iacute = "\u00cd", Icirc = "\u00ce", Iuml = "\u00cf", Igrave = "\u00cc",
  Ntilde = "\u00d1", Oacute = "\u00d3", Ocirc = "\u00d4", Otilde = "\u00d5",
  Ouml = "\u00d6", Ograve = "\u00d2", Uacute = "\u00da", Ucirc = "\u00db",
  Uuml = "\u00dc", Ugrave = "\u00d9", Yacute = "\u00dd",
  aacute = "\u00e1", acirc = "\u00e2", atilde = "\u00e3", auml = "\u00e4",
  agrave = "\u00e0", aring = "\u00e5", ccedil = "\u00e7",
  eacute = "\u00e9", ecirc = "\u00ea", euml = "\u00eb", egrave = "\u00e8",
  iacute = "\u00ed", icirc = "\u00ee", iuml = "\u00ef", igrave = "\u00ec",
  ntilde = "\u00f1", oacute = "\u00f3", ocirc = "\u00f4", otilde = "\u00f5",
  ouml = "\u00f6", ograve = "\u00f2", uacute = "\u00fa", ucirc = "\u00fb",
  uuml = "\u00fc", ugrave = "\u00f9", yacute = "\u00fd", yuml = "\u00ff"
)

# La misma gramatica la usan dos funciones que tienen que coincidir: esta la usa
# para **detectar** entidades y `.decodificar_entidades_html()` para
# **repararlas**. Escritas por separado, si una cambiaba se detectaba lo que no
# se reparaba, o al reves, y ninguna de las dos avisaba. Es el mismo
# acoplamiento que tenian las tres copias de la generalizacion de patrones.
.PATRON_ENTIDAD_HTML <-
  "&(?:#(?:[xX][0-9A-Fa-f]{1,6}|[0-9]{1,7})|[A-Za-z][A-Za-z0-9]+);"

.entidades_html_en_texto <- function(textos) {
  patron <- .PATRON_ENTIDAD_HTML
  vapply(textos, function(texto) {
    if (is.na(texto)) return(FALSE)
    posiciones <- gregexpr(patron, texto, perl = TRUE)[[1L]]
    if (identical(posiciones[[1L]], -1L)) return(FALSE)
    encontrados <- regmatches(texto, list(posiciones))[[1L]]
    any(vapply(encontrados, .entidad_html_valida, logical(1L)))
  }, logical(1L), USE.NAMES = FALSE)
}

.entidad_html_valida <- function(entidad) {
  cuerpo <- substring(entidad, 2L, nchar(entidad) - 1L)
  if (startsWith(cuerpo, "#x") || startsWith(cuerpo, "#X")) {
    punto <- suppressWarnings(strtoi(substring(cuerpo, 3L), base = 16L))
    return(is.finite(punto) && punto > 0L && punto <= 0x10FFFF &&
      !(punto >= 0xD800 && punto <= 0xDFFF))
  }
  if (startsWith(cuerpo, "#")) {
    punto <- suppressWarnings(as.numeric(substring(cuerpo, 2L)))
    return(is.finite(punto) && punto > 0 && punto <= 0x10FFFF &&
      !(punto >= 0xD800 && punto <= 0xDFFF))
  }
  cuerpo %in% names(.entidades_html_comunes)
}

.escapar_texto_visible <- function(texto) {
  if (is.na(texto)) return(NA_character_)
  codigos <- tryCatch(utf8ToInt(texto), error = function(e) integer())
  if (!length(codigos)) return("")
  partes <- vapply(codigos, function(codigo) {
    if (codigo == 9L) return("\\t")
    if (codigo == 10L) return("\\n")
    if (codigo == 11L) return("\\v")
    if (codigo == 12L) return("\\f")
    if (codigo == 13L) return("\\r")
    if (.codigos_control_invisible(codigo)) {
      return(sprintf("<U+%04X>", codigo))
    }
    intToUtf8(codigo)
  }, character(1L), USE.NAMES = FALSE)
  paste0(partes, collapse = "")
}

.evidencia_texto_visible <- function(textos, mascara) {
  ejemplos <- utils::head(unique(textos[which(mascara)]), 6L)
  if (!length(ejemplos)) return("")
  paste(vapply(ejemplos, .escapar_texto_visible, character(1L)), collapse = "; ")
}

.stringi_disponible <- function() {
  requireNamespace("stringi", quietly = TRUE)
}

.bit64_disponible <- function() {
  requireNamespace("bit64", quietly = TRUE)
}

.diagnosticar_texto <- function(x, vocabulario = NULL) {
  vacio <- list(
    n_espacios_borde = 0L,
    evidencia_espacios = "",
    n_variantes_mayusculas = 0L,
    evidencia_mayusculas = "",
    n_variantes_unicode = NA_integer_,
    evidencia_unicode = "",
    # Una columna vacía o compuesta sólo por NA no contiene texto no ASCII que
    # evaluar: la ausencia de `stringi` no limita ese caso.
    unicode_evaluado = TRUE,
    n_codificacion_rota = 0L,
    n_codificacion_reparable = 0L,
    n_codificacion_reparable_parcialmente = 0L,
    n_codificacion_irreparable = 0L,
    n_codificacion_no_se_pudo = 0L,
    estado_codificacion_reparacion = "no_parece_roto",
    evidencia_codificacion = "",
    n_codificacion_invalida = 0L,
    evidencia_codificacion_invalida = "",
    n_controles_invisibles = 0L,
    evidencia_controles_invisibles = "",
    n_invisibles_eliminables = 0L,
    evidencia_invisibles_eliminables = "",
    n_espacios_invisibles = 0L,
    evidencia_espacios_invisibles = "",
    n_invisibles_significativos = 0L,
    evidencia_invisibles_significativos = "",
    n_entidades_html = 0L,
    evidencia_entidades_html = "",
    n_separadores_en_campo = 0L,
    evidencia_separadores_en_campo = ""
  )
  if (!is.character(x) && !is.factor(x)) {
    return(vacio)
  }
  preparacion <- .texto_analizable(x)
  textos <- preparacion$valores
  vacio$n_codificacion_invalida <- length(preparacion$posiciones)
  if (length(preparacion$posiciones)) {
    mostradas <- utils::head(preparacion$posiciones, 8L)
    vacio$evidencia_codificacion_invalida <- paste0(
      length(preparacion$posiciones), " valores; filas: ",
      paste(mostradas, collapse = ", "),
      if (length(preparacion$posiciones) > length(mostradas)) ", ..." else ""
    )
  }
  validos <- !is.na(textos)
  if (!any(validos)) {
    return(vacio)
  }

  unicos <- if (is.null(vocabulario)) {
    unique(textos[validos])
  } else {
    vocabulario
  }
  if (length(textos) > 1L &&
      length(unicos) / length(textos) <= .umbral_vocabulario_barato) {
    vocabulario_barato <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = unicos
    )
    espacios_unicos <- vocabulario_barato$valores !=
      trimws(vocabulario_barato$valores)
    espacios <- validos &
      espacios_unicos[vocabulario_barato$indices]
  } else {
    espacios <- validos & textos != trimws(textos)
  }
  ejemplos_espacios <- utils::head(unique(textos[espacios]), 6L)
  minusculas <- tolower(unicos)
  colision <- duplicated(minusculas) | duplicated(minusculas, fromLast = TRUE)
  variantes <- unicos[colision]
  solo_ascii <- !any(grepl("[^\\x01-\\x7F]", unicos, perl = TRUE))
  if (solo_ascii) {
    evidencia_unicode <- ""
    n_variantes_unicode <- 0L
    unicode_evaluado <- TRUE
  } else if (.stringi_disponible()) {
    normalizados_unicode <- stringi::stri_trans_nfc(unicos)
    colision_unicode <- duplicated(normalizados_unicode) |
      duplicated(normalizados_unicode, fromLast = TRUE)
    variantes_unicode <- unicos[colision_unicode]
    evidencia_unicode <- paste(
      stringi::stri_escape_unicode(utils::head(variantes_unicode, 6L)),
      collapse = "; "
    )
    n_variantes_unicode <- length(variantes_unicode)
    unicode_evaluado <- TRUE
  } else {
    evidencia_unicode <- "Se necesita el paquete opcional 'stringi'."
    n_variantes_unicode <- NA_integer_
    unicode_evaluado <- FALSE
  }
  vocabulario_predicados <- .vocabulario_texto(
    textos, .umbral_vocabulario_barato, valores = unicos
  )
  mapear_predicado <- function(fn) {
    evaluados <- fn(vocabulario_predicados$valores)
    if (isTRUE(vocabulario_predicados$usar)) {
      evaluados[vocabulario_predicados$indices]
    } else {
      evaluados
    }
  }
  invisibles <- .predicados_invisibles(vocabulario_predicados$valores)
  mapear_resultado <- function(evaluados) {
    if (isTRUE(vocabulario_predicados$usar)) {
      evaluados[vocabulario_predicados$indices]
    } else {
      evaluados
    }
  }
  controles <- mapear_resultado(invisibles$control)
  eliminables <- mapear_resultado(invisibles$eliminable)
  espacios_invisibles <- mapear_resultado(invisibles$espacio)
  significativos <- mapear_resultado(invisibles$significativo)
  saltos <- mapear_resultado(invisibles$separador)
  entidades <- mapear_predicado(.entidades_html_en_texto)
  codificacion <- .analizar_codificacion_vocabulario(textos, valores = unicos)

  list(
    n_espacios_borde = sum(espacios),
    evidencia_espacios = paste(
      encodeString(ejemplos_espacios, quote = '"'), collapse = "; "
    ),
    n_variantes_mayusculas = length(variantes),
    evidencia_mayusculas = paste(
      encodeString(utils::head(variantes, 6L), quote = '"'), collapse = "; "
    ),
    n_variantes_unicode = n_variantes_unicode,
    evidencia_unicode = evidencia_unicode,
    unicode_evaluado = unicode_evaluado,
    n_codificacion_rota = codificacion$n,
    n_codificacion_reparable = codificacion$n_reparables,
    n_codificacion_reparable_parcialmente = codificacion$n_reparables_parcialmente,
    n_codificacion_irreparable = codificacion$n_irreparables,
    n_codificacion_no_se_pudo = codificacion$n_no_se_pudo,
    estado_codificacion_reparacion = codificacion$estado,
    evidencia_codificacion = codificacion$evidencia,
    n_codificacion_invalida = length(preparacion$posiciones),
    evidencia_codificacion_invalida = vacio$evidencia_codificacion_invalida,
    n_controles_invisibles = sum(controles, na.rm = TRUE),
    evidencia_controles_invisibles = .evidencia_texto_visible(textos, controles),
    n_invisibles_eliminables = sum(eliminables, na.rm = TRUE),
    evidencia_invisibles_eliminables = .evidencia_texto_visible(textos, eliminables),
    n_espacios_invisibles = sum(espacios_invisibles, na.rm = TRUE),
    evidencia_espacios_invisibles = .evidencia_texto_visible(textos, espacios_invisibles),
    n_invisibles_significativos = sum(significativos, na.rm = TRUE),
    evidencia_invisibles_significativos = .evidencia_texto_visible(textos, significativos),
    n_entidades_html = sum(entidades, na.rm = TRUE),
    evidencia_entidades_html = .evidencia_texto_visible(textos, entidades),
    n_separadores_en_campo = sum(saltos, na.rm = TRUE),
    evidencia_separadores_en_campo = .evidencia_texto_visible(textos, saltos)
  )
}

.n_distintos_columna <- function(x) {
  validos <- tryCatch(!is.na(x), error = function(e) NULL)
  if (is.null(validos) || length(validos) != length(x)) return(NA_integer_)
  if (!any(validos)) return(0L)
  # El acumulador de distintos conserva valores atomicos. Las columnas de
  # listas (incluidas `sfc` y `POSIXlt`) siguen necesitando la igualdad de la
  # pasada unica: intentar guardarlas como representantes del data.frame del
  # mapa puede expandirlas o hacerlas no construibles. La migracion solo toma
  # la via por bloques para las clases que esa representacion puede conservar.
  sobre <- if (is.atomic(x) && is.null(dim(x))) {
    .mapa_distintos_bloques(x)
  } else {
    NULL
  }
  if (!is.null(sobre) && identical(sobre$estado, "calculado")) {
    return(as.integer(nrow(sobre$resultado)))
  }
  tryCatch(as.integer(length(unique(x[validos]))),
           error = function(e) NA_integer_)
}

# Recorta un resultado de faltantes disfrazados al universo aplicable. La
# mascara elemento a elemento es lo que permite hacerlo sin volver a detectar.
.restringir_disfrazados <- function(disfrazados, aplicable) {
  if (is.null(aplicable) || all(aplicable)) return(disfrazados)
  mascara <- disfrazados$mascara & aplicable
  n_aplicables <- sum(aplicable)
  disfrazados$mascara <- mascara
  disfrazados$n <- sum(mascara)
  disfrazados$proporcion <- if (n_aplicables) sum(mascara) / n_aplicables else NA_real_
  if (!is.null(disfrazados$mascara_textual)) {
    disfrazados$mascara_textual <- disfrazados$mascara_textual & aplicable
    disfrazados$n_textuales <- sum(disfrazados$mascara_textual)
  }
  if (!is.null(disfrazados$mascara_numerica)) {
    disfrazados$mascara_numerica <- disfrazados$mascara_numerica & aplicable
    disfrazados$n_numericos <- sum(disfrazados$mascara_numerica)
  }
  disfrazados
}

.perfilar_columna <- function(x, nombre, muestra, max_patrones,
                              distinguir_mayusculas, expandir,
                              umbral_patron_raro,
                              sentinelas_numericos,
                              aplicable = NULL) {
  if (is.matrix(x)) {
    return(.perfilar_columna_matriz(
      x, nombre, muestra, max_patrones, distinguir_mayusculas, expandir,
      umbral_patron_raro, sentinelas_numericos
    ))
  }
  # El universo aplicable se resuelve primero porque gobierna todos los conteos
  # de ausencia de abajo. Sin declaracion, toda la columna aplica y el resto se
  # reduce al comportamiento de siempre.
  aplicable <- if (is.null(aplicable)) {
    rep(TRUE, length(x))
  } else {
    # `as.logical()` borraria el conteo de indeterminados que viaja como
    # atributo, y la fila volveria a confundir "no corresponde" con "no se sabe".
    indeterminados <- attr(aplicable, "n_indeterminados", exact = TRUE)
    convertida <- as.logical(aplicable)
    attr(convertida, "n_indeterminados") <- indeterminados
    convertida
  }
  geometria <- .perfilar_geometria(x)
  preparacion_texto <- .texto_analizable(x)
  x_analisis <- preparacion_texto$valores
  # La columna tal cual, para lo que compara por igualdad en vez de leer texto.
  # Sobre una columna no analizable como texto, `valores` son ausentes
  # declarados: inferir el tipo ahi diria "desconocido" sobre una columna cuyo
  # tipo se conoce perfectamente.
  x_identidad <- preparacion_texto$valores_identidad
  if (is.null(x_identidad)) x_identidad <- x_analisis
  # Las filas fuera del universo salen del analisis, no solo de los conteos.
  # Dejarlas dentro hacia que `n_distintos` contara valores que no aplican
  # mientras `n_validos` ya solo contaba los aplicables, y `tasa_distintos`
  # podia pasar de 1, que es imposible. Se marcan como ausentes en vez de
  # recortarse para que los indices de fila sigan alineados con la tabla.
  if (!all(aplicable)) {
    x_analisis[!aplicable] <- NA
    x_identidad[!aplicable] <- NA
  }
  # El perfilado conserva el resultado intermedio sólo durante esta llamada;
  # la función exportada `inferir_tipo()` nunca devuelve ese caché.
  inferencia <- .inferir_tipo_interno(
    x_identidad, muestra = muestra, conservar_cache = TRUE
  )
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) {
    formatos <- detectar_formatos_fecha(x_analisis, muestra = muestra)
  }
  meses_texto <- attr(formatos, "meses_texto", exact = TRUE)
  zona_horaria_origen <- .zona_horaria_origen(x)
  n_filas_fecha_civil_distinta_utc <- .fechas_civiles_distintas_utc(x)
  attr(formatos, "zona_horaria_origen") <- zona_horaria_origen
  attr(formatos, "n_filas_fecha_civil_distinta_utc") <-
    n_filas_fecha_civil_distinta_utc
  # The month parser is an internal hand-off between type inference and the
  # column summary. It must not remain attached to the public profile table.
  attr(formatos, "meses_texto") <- NULL
  patrones <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    descubrir_patrones(
      x_analisis,
      distinguir_mayusculas = distinguir_mayusculas,
      expandir = expandir,
      max_patrones = max_patrones,
      muestra = muestra,
      umbral_raro = umbral_patron_raro
    )
  } else {
    estructura <- data.frame(
      patron = character(), n = integer(), proporcion = numeric(),
      ejemplos = character(), stringsAsFactors = FALSE
    )
    class(estructura) <- c("patrones", "data.frame")
    estructura
  }
  secuencia_entera <- .resumen_secuencia_entera(
    x_analisis, inferencia, formatos
  )
  attr(patrones, "desvios_patron_raro") <-
    .desvios_patron_raro_detectados(
      patrones, secuencia_entera$densa, umbral_patron_raro
    )
  faltantes_disfrazados <- .detectar_faltantes_disfrazados(
    x_analisis, sentinelas_numericos = sentinelas_numericos,
    detectar_sentinelas_numericos = !isTRUE(secuencia_entera$densa)
  )
  faltantes_disfrazados <- .restringir_disfrazados(faltantes_disfrazados, aplicable)
  rol_propuesto <- .propuesta_escala(x, inferencia$tipo)$rol
  casi_clave <- .resumen_casi_clave(
    x_analisis, rol = rol_propuesto, tipo_implicito = inferencia$tipo,
    disfrazados = faltantes_disfrazados$mascara
  )
  n <- length(x)
  # El universo aplicable. Sin declaracion, toda la columna aplica y todo lo de
  # abajo se reduce al comportamiento de siempre. Con declaracion, las filas
  # donde la columna no corresponde salen del denominador: un ausente por
  # diseno no es un ausente por error, y contarlos juntos era informar como
  # defecto lo que es la forma del dato.
  n_aplicables <- sum(aplicable)
  n_indeterminados <- attr(aplicable, "n_indeterminados", exact = TRUE)
  if (is.null(n_indeterminados)) n_indeterminados <- 0L
  n_no_aplica <- n - n_aplicables - n_indeterminados
  n_faltantes <- sum(is.na(x) & aplicable)
  # Un valor presente donde la regla dice que no corresponde es un hallazgo por
  # derecho propio, no algo para descartar en silencio.
  n_presentes_fuera <- sum(!is.na(x) & !aplicable)
  n_codificacion_invalida <- length(preparacion_texto$posiciones)
  n_validos <- n_aplicables - n_faltantes - n_codificacion_invalida
  n_distintos <- .n_distintos_columna(x_identidad)
  valor_concentrado <- .estadisticos_valor_concentrado(x_identidad)
  vocabulario_texto <- if (
    (is.character(x_analisis) || is.factor(x_analisis)) &&
      n > 1L && is.finite(n_distintos) &&
      n_distintos / n <= .umbral_vocabulario_codificacion
  ) {
    unique(x_analisis[!is.na(x_analisis)])
  } else {
    NULL
  }
  vocabulario_fecha <- if (
    (is.character(x_analisis) || is.factor(x_analisis)) &&
      inferencia$tipo %in% c("fecha", "fecha-hora")
  ) {
    textos_fecha <- trimws(x_analisis)
    presentes_fecha <- !is.na(textos_fecha) & nzchar(textos_fecha)
    valores_fecha <- if (is.null(vocabulario_texto)) NULL else {
      unicos <- trimws(vocabulario_texto)
      unique(unicos[!is.na(unicos) & nzchar(unicos)])
    }
    .vocabulario_texto(
      textos_fecha[presentes_fecha], .umbral_vocabulario_barato,
      valores = valores_fecha
    )
  } else NULL
  moda <- .moda_columna(x_identidad)
  longitudes <- .resumen_longitud(x_analisis)
  cuantitativo <- .resumen_cuantitativo(
    x_analisis, inferencia, formatos, meses_texto = meses_texto,
    sentinelas_numericos = sentinelas_numericos,
    vocabulario = vocabulario_fecha,
    valores_preparados = x_analisis
  )
  diagnostico_texto <- .diagnosticar_texto(x, vocabulario = vocabulario_texto)
  vocabulario_numeros <- if (
    is.null(vocabulario_texto) &&
      (is.character(x_analisis) || is.factor(x_analisis))
  ) x_analisis else vocabulario_texto
  numeros_texto <- .analizar_numeros_texto(
    x_analisis, vocabulario = vocabulario_numeros, n_distintos = n_distintos,
    directo = is.null(vocabulario_texto) &&
      (is.character(x_analisis) || is.factor(x_analisis))
  )
  multivaluados <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    .detectar_multivaluados(x_analisis)
  } else list()
  n_blancos <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    sum(!is.na(x_analisis) & !nzchar(trimws(as.character(x_analisis))))
  } else {
    0L
  }

  fila <- data.frame(
    columna = nombre,
    tipo_declarado = .tipo_declarado(x),
    tipo_inferido = inferencia$tipo,
    proporcion_tipo_inferido = inferencia$proporcion,
    n_filas_analizadas_tipo = inferencia$n_analizados,
    muestreado_tipo_inferido = inferencia$muestreado,
    n = n,
    # El denominador de toda proporcion de ausencia es el universo aplicable,
    # no el total de filas. Sin declaracion son el mismo numero.
    n_aplicables = n_aplicables,
    n_no_aplica = n_no_aplica,
    n_aplicabilidad_indeterminada = n_indeterminados,
    n_presentes_fuera_de_aplicabilidad = n_presentes_fuera,
    n_faltantes = n_faltantes,
    prop_faltantes = if (n_aplicables) n_faltantes / n_aplicables else NA_real_,
    n_faltantes_disfrazados = faltantes_disfrazados$n,
    n_faltantes_disfrazados_textuales = faltantes_disfrazados$n_textuales,
    n_faltantes_disfrazados_numericos = faltantes_disfrazados$n_numericos,
    prop_faltantes_disfrazados = if (n_aplicables) {
      faltantes_disfrazados$n / n_aplicables
    } else NA_real_,
    n_faltantes_totales = n_faltantes + faltantes_disfrazados$n,
    prop_faltantes_totales = if (n_aplicables) {
      (n_faltantes + faltantes_disfrazados$n) / n_aplicables
    } else {
      NA_real_
    },
    n_distintos = n_distintos,
    tasa_distintos = if (n_validos && !is.na(n_distintos)) {
      n_distintos / n_validos
    } else NA_real_,
    secuencia_entera_densa = secuencia_entera$densa,
    densidad_secuencia_entera = secuencia_entera$densidad,
    n_posiciones_secuencia_entera = secuencia_entera$n_posiciones,
    n_huecos_secuencia_entera = secuencia_entera$n_huecos,
    hueco_maximo_secuencia_entera = secuencia_entera$hueco_maximo,
    salto_de_escala_secuencia_entera = secuencia_entera$salto_de_escala,
    umbral_densidad_secuencia_entera = secuencia_entera$umbral_densidad,
    min_distintos_secuencia_entera = secuencia_entera$min_distintos,
    moda = moda$valor,
    frecuencia_moda = moda$frecuencia,
    longitud_minima = unname(longitudes[["minimo"]]),
    longitud_maxima = unname(longitudes[["maximo"]]),
    longitud_media = unname(longitudes[["media"]]),
    minimo = cuantitativo$minimo,
    maximo = cuantitativo$maximo,
    media = cuantitativo$media,
    mediana = cuantitativo$mediana,
    desvio = cuantitativo$desvio,
    minimo_exacto = cuantitativo$minimo_exacto,
    maximo_exacto = cuantitativo$maximo_exacto,
    minimo_fecha = cuantitativo$minimo_fecha,
    maximo_fecha = cuantitativo$maximo_fecha,
    media_fecha = cuantitativo$media_fecha,
    mediana_fecha = cuantitativo$mediana_fecha,
    n_fechas_resumidas = cuantitativo$n_fechas_resumidas,
    n_fechas_excluidas_granularidad =
      cuantitativo$n_fechas_excluidas_granularidad,
    n_ceros = cuantitativo$n_ceros,
    n_negativos = cuantitativo$n_negativos,
    n_outliers = cuantitativo$n_outliers,
    centinela_valor = cuantitativo$centinela_valor,
    centinela_repeticiones = cuantitativo$centinela_repeticiones,
    densidad_sin_centinela = cuantitativo$densidad_sin_centinela,
    n_nan = cuantitativo$n_nan,
    n_infinito_positivo = cuantitativo$n_infinito_positivo,
    n_infinito_negativo = cuantitativo$n_infinito_negativo,
    estado_resumen_cuantitativo = cuantitativo$estado_resumen_cuantitativo,
    zona_horaria_origen = zona_horaria_origen,
    n_filas_fecha_civil_distinta_utc = n_filas_fecha_civil_distinta_utc,
    fecha_civil_distinta_utc = if (is.na(n_filas_fecha_civil_distinta_utc)) {
      NA
    } else n_filas_fecha_civil_distinta_utc > 0L,
    # Como llego la geometria y si se pudo interpretar. Sin esto, una columna
    # WKT o WKB queda indistinguible de un texto cualquiera aguas abajo, y la
    # cobertura conceptual llega a afirmar que la geometria no aplica sobre
    # datos que si son geometricos.
    representacion_geometria = if (is.null(geometria$representacion_geometria)) {
      NA_character_
    } else {
      geometria$representacion_geometria
    },
    motivo_representacion = if (is.null(geometria$motivo_representacion)) {
      NA_character_
    } else {
      geometria$motivo_representacion
    },
    crs_declarado = geometria$crs_declarado,
    tipo_geometria = geometria$tipo_geometria,
    dimension_geometria = geometria$dimension_geometria,
    dimensiones_no_evaluadas = geometria$dimensiones_no_evaluadas,
    n_geometrias_vacias = geometria$n_geometrias_vacias,
    n_geometrias_invalidas = geometria$n_geometrias_invalidas,
    n_validez_evaluados = geometria$n_validez_evaluados,
    validez_criterio = geometria$validez_criterio,
    validez_preprocesamiento = geometria$validez_preprocesamiento,
    n_fuera_de_dominio = geometria$n_fuera_de_dominio,
    n_dominio_evaluados = geometria$n_dominio_evaluados,
    n_bbox_evaluados = geometria$n_bbox_evaluados,
    bbox_alcance = geometria$bbox_alcance,
    bbox_xmin = geometria$bbox_xmin,
    bbox_xmax = geometria$bbox_xmax,
    bbox_ymin = geometria$bbox_ymin,
    bbox_ymax = geometria$bbox_ymax,
    detalle_proteccion_personal = NA_character_,
    n_blancos = n_blancos,
    n_espacios_borde = diagnostico_texto$n_espacios_borde,
    n_variantes_mayusculas = diagnostico_texto$n_variantes_mayusculas,
    n_variantes_unicode = diagnostico_texto$n_variantes_unicode,
    unicode_evaluado = if (isTRUE(preparacion_texto$analizable) &&
                           (is.character(x_analisis) || is.factor(x_analisis))) {
      diagnostico_texto$unicode_evaluado
    } else {
      NA
    },
    n_codificacion_rota = diagnostico_texto$n_codificacion_rota,
    n_codificacion_reparable = diagnostico_texto$n_codificacion_reparable,
    n_codificacion_reparable_parcialmente = diagnostico_texto$n_codificacion_reparable_parcialmente,
    n_codificacion_irreparable = diagnostico_texto$n_codificacion_irreparable,
    n_codificacion_no_se_pudo = diagnostico_texto$n_codificacion_no_se_pudo,
    estado_codificacion_reparacion = diagnostico_texto$estado_codificacion_reparacion,
    n_codificacion_invalida = diagnostico_texto$n_codificacion_invalida,
    n_controles_invisibles = diagnostico_texto$n_controles_invisibles,
    n_invisibles_eliminables = diagnostico_texto$n_invisibles_eliminables,
    n_espacios_invisibles = diagnostico_texto$n_espacios_invisibles,
    n_invisibles_significativos = diagnostico_texto$n_invisibles_significativos,
    n_entidades_html = diagnostico_texto$n_entidades_html,
    n_separadores_en_campo = diagnostico_texto$n_separadores_en_campo,
    n_numeros_texto = numeros_texto$n,
    proporcion_numeros_texto = numeros_texto$proporcion,
    numero_texto_ambiguo = numeros_texto$ambiguo,
    numero_texto_seguro = numeros_texto$seguro,
    numero_texto_unidad = numeros_texto$unidad,
    numero_texto_moneda = numeros_texto$moneda,
    numero_texto_convencion = numeros_texto$convencion,
    stringsAsFactors = FALSE
  )

  list(
    fila = fila,
    inferencia = inferencia,
    formatos = formatos,
    patrones = patrones,
    secuencia_entera = secuencia_entera,
    casi_clave = casi_clave,
    faltantes_disfrazados = faltantes_disfrazados,
    diagnostico_texto = diagnostico_texto,
    numeros_texto = numeros_texto,
    multivaluados = multivaluados,
    valor_concentrado = valor_concentrado,
    geometria = geometria,
    # La mascara viaja con el resultado para que la trazabilidad pueda nombrar
    # las filas del hallazgo de valor fuera de aplicabilidad.
    aplicable = aplicable
  )
}

.perfilar_columna_matriz <- function(x, nombre, muestra, max_patrones,
                                     distinguir_mayusculas, expandir,
                                     umbral_patron_raro,
                                     sentinelas_numericos) {
  resultado <- .perfilar_columna(
    rep(NA_character_, NROW(x)), nombre, muestra, max_patrones,
    distinguir_mayusculas, expandir, umbral_patron_raro,
    sentinelas_numericos
  )
  fila <- resultado$fila
  fila$tipo_declarado <- "matriz"
  fila$tipo_inferido <- "desconocido"
  fila$proporcion_tipo_inferido <- NA_real_
  fila$n_filas_analizadas_tipo <- NA_integer_
  fila$muestreado_tipo_inferido <- NA
  fila$n <- NROW(x)
  enteros_na <- c(
    "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
    "n_presentes_fuera_de_aplicabilidad",
    "n_faltantes", "n_faltantes_disfrazados",
    "n_faltantes_disfrazados_textuales",
    "n_faltantes_disfrazados_numericos", "n_faltantes_totales",
    "n_distintos", "frecuencia_moda", "n_ceros", "n_negativos",
    "n_outliers", "n_nan", "n_infinito_positivo", "n_infinito_negativo",
    "n_blancos", "n_espacios_borde", "n_variantes_mayusculas",
    "representacion_geometria", "motivo_representacion",
    "n_variantes_unicode", "n_codificacion_rota",
    "n_codificacion_reparable", "n_codificacion_reparable_parcialmente",
    "n_codificacion_irreparable", "n_codificacion_no_se_pudo",
    "n_codificacion_invalida", "n_controles_invisibles",
    "n_invisibles_eliminables", "n_espacios_invisibles",
    "n_invisibles_significativos", "n_entidades_html",
    "n_separadores_en_campo", "n_numeros_texto"
  )
  fila[enteros_na] <- NA_integer_
  reales_na <- c(
    "prop_faltantes", "prop_faltantes_disfrazados",
    "prop_faltantes_totales", "tasa_distintos", "longitud_minima",
    "longitud_maxima", "longitud_media", "minimo", "maximo", "media",
    "mediana", "desvio", "proporcion_numeros_texto"
  )
  fila[reales_na] <- NA_real_
  fila$moda <- NA_character_
  fila$minimo_exacto <- NA_character_
  fila$maximo_exacto <- NA_character_
  fila$minimo_fecha <- NA_character_
  fila$maximo_fecha <- NA_character_
  fila$media_fecha <- NA_character_
  fila$mediana_fecha <- NA_character_
  fila$estado_resumen_cuantitativo <- "tipo_compuesto_no_analizado"
  fila$zona_horaria_origen <- NA_character_
  fila$n_filas_fecha_civil_distinta_utc <- NA_integer_
  fila$fecha_civil_distinta_utc <- NA
  fila$unicode_evaluado <- NA
  fila$numero_texto_ambiguo <- FALSE
  fila$numero_texto_seguro <- FALSE
  fila$numero_texto_unidad <- ""
  fila$numero_texto_moneda <- ""
  fila$numero_texto_convencion <- ""
  resultado$fila <- fila
  resultado$inferencia$tipo <- "desconocido"
  resultado$inferencia$proporcion <- NA_real_
  resultado$inferencia$compatibles <- 0L
  resultado$inferencia$n_analizados <- 0L
  resultado$estructura_no_analizada <- list(
    tipo = "matriz", filas = NROW(x), componentes = NCOL(x),
    dimensiones = dim(x)
  )
  resultado
}
