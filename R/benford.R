.umbrales_benford <- function() {
  list(
    minimo_ordenes_magnitud = 3,
    minima_proporcion_positivos = 1,
    minimo_observaciones_utilizables = 100L,
    nivel_significacion = 0.01
  )
}

.columna_candidata_benford <- function(x) {
  # `.es_numerico_pelado(x)` en vez de la lista de cuatro clases: la lista
  # dejaba entrar a `units` y a `lubridate::Period`, y `.primer_digito_
  # significativo()` ABORTA con las dos -"both operands of the expression
  # should be units objects" y "Incompatible classes: <numeric> ^ <Period>"-,
  # porque `10 ^ (log10(x) - exponente)` ejecuta la aritmetica de la clase. El
  # criterio y su porque estan escritos una sola vez, en `utils.R`.
  .es_numerico_pelado(x) &&
    !.es_columna_compuesta(x) &&
    sum(is.finite(x)) >= 50L
}

.primer_digito_significativo <- function(x) {
  exponente <- floor(log10(x))
  digito <- floor(10 ^ (log10(x) - exponente) + 1e-12)
  as.integer(pmin(9, pmax(1, digito)))
}

.texto_distribucion_benford <- function(distribucion) {
  paste0(
    distribucion$digito, ":",
    .formatear_decimal_publicado(distribucion$proporcion_observada), "/",
    .formatear_decimal_publicado(distribucion$proporcion_esperada),
    collapse = "; "
  )
}

.precondiciones_benford <- function(variacion, es_identificador,
                                    n_positivos, proporcion_positivos,
                                    ordenes, umbrales) {
  fallas <- character()
  if (!variacion) {
    fallas <- c(fallas, "sin_variacion")
  }
  if (es_identificador) {
    fallas <- c(fallas, "parece_identificador")
  }
  if (n_positivos < umbrales$minimo_observaciones_utilizables) {
    fallas <- c(fallas, "observaciones_utilizables_insuficientes")
  }
  if (!is.finite(proporcion_positivos) ||
      proporcion_positivos < umbrales$minima_proporcion_positivos) {
    fallas <- c(fallas, "proporcion_positivos_insuficiente")
  }
  if (!is.finite(ordenes) ||
      ordenes < umbrales$minimo_ordenes_magnitud) {
    fallas <- c(fallas, "ordenes_magnitud_insuficientes")
  }
  fallas
}

# Un identificador no es una magnitud, y Benford describe magnitudes que salen
# de procesos multiplicativos. Que `MotId` se desvie de la distribucion es
# cierto y no significa nada.
#
# Esta guarda exigia una corrida consecutiva SIN HUECOS, y un identificador real
# tiene huecos: los que se dieron de baja. Un `MotId` que va de 1 a 4557 sobre
# 3.159 filas no la pasaba, y Benford se corria igual.
#
# Lo que lo describe es la DENSIDAD: una numeracion ocupa un tramo compacto de
# los enteros -0,69 en ese caso- y una magnitud se reparte por varios ordenes de
# magnitud -0,00005 para montos entre 9 y 9.999.999-. La unicidad no sirve: un
# monto tambien es casi unico.
#
# La densidad sola dependia del numero de filas: una magnitud entera acotada
# llena su rango si se cargan filas suficientes. La regla que lo cierra -densa Y
# casi sin repetir- es la de todo el paquete y esta escrita una sola vez, en
# `.numeracion_sin_repetir_conteos()` (`hallazgos.R`), con su medicion. Esta es
# la entrada para un vector crudo.
.numeracion_sin_repetir <- function(valores) {
  n <- length(valores)
  if (!n) return(FALSE)
  conteos <- tabulate(match(valores, unique(valores)))
  .numeracion_sin_repetir_conteos(length(conteos), n, max(conteos))
}

.parece_correlativo_integer64 <- function(x) {
  if (!inherits(x, "integer64") || !.bit64_disponible()) return(FALSE)
  presentes <- x[!is.na(x)]
  if (length(presentes) < 2L) return(FALSE)
  distintos <- .sort_por_bytes(unique(presentes))
  if (length(distintos) < .MIN_DISTINTOS_NUMERACION) return(FALSE)
  if (!.numeracion_sin_repetir(presentes)) return(FALSE)
  uno <- bit64::as.integer64(1)
  rango <- max(distintos) - min(distintos) + uno
  limite_doble <- bit64::as.integer64("9007199254740991")
  if (rango <= 0 || rango > limite_doble) return(FALSE)
  if (length(distintos) / as.numeric(rango) < .MIN_DENSIDAD_NUMERACION) {
    return(FALSE)
  }
  huecos <- diff(distintos)
  huecos_doble <- suppressWarnings(as.numeric(huecos))
  if (any(!is.finite(huecos_doble))) return(FALSE)
  tipico <- stats::median(huecos_doble)
  if (!is.finite(tipico) || tipico <= 0) tipico <- 1
  !(length(distintos) >= 3L &&
      max(huecos_doble) >= .FACTOR_SALTO_ESCALA * tipico &&
      max(huecos_doble) > 1)
}

.parece_correlativo_benford <- function(x) {
  if (inherits(x, "integer64")) return(.parece_correlativo_integer64(x))
  if (!is.numeric(x)) return(FALSE)
  valores <- as.numeric(x[is.finite(x)])
  if (length(valores) < 2L) return(FALSE)
  if (any(valores != floor(valores))) return(FALSE)
  distintos <- .sort_por_bytes(unique(valores))
  if (length(distintos) < .MIN_DISTINTOS_NUMERACION) return(FALSE)
  if (!.numeracion_sin_repetir(valores)) return(FALSE)
  rango <- distintos[[length(distintos)]] - distintos[[1L]] + 1
  if (!is.finite(rango) || rango <= 0) return(FALSE)
  if (length(distintos) / rango < .MIN_DENSIDAD_NUMERACION) return(FALSE)
  # La misma segunda senal que usa la guarda de los limites de Tukey: un valor
  # separado del resto por un hueco desproporcionado no es cola de la
  # numeracion, y una columna que lo tiene vuelve a evaluarse.
  huecos <- diff(distintos)
  tipico <- stats::median(huecos)
  if (!is.finite(tipico) || tipico <= 0) tipico <- 1
  !(length(distintos) >= 3L && max(huecos) >= .FACTOR_SALTO_ESCALA * tipico &&
      max(huecos) > 1)
}

.resultado_benford_columna <- function(x, nombre, tipo_inferido,
                                       posible_identificador,
                                       umbrales = .umbrales_benford(),
                                       clave_declarada = FALSE,
                                       codigo_declarado = FALSE,
                                       sospechosos = NULL) {
  finitos <- if (is.numeric(x)) {
    as.numeric(x[is.finite(x)])
  } else {
    numeric()
  }
  positivos <- finitos[finitos > 0]
  n_finitos <- length(finitos)
  n_positivos <- length(positivos)
  proporcion_positivos <- if (n_finitos) n_positivos / n_finitos else NA_real_
  variacion <- length(unique(finitos)) > 1L
  ordenes <- .ordenes_magnitud_benford(positivos, variacion)
  # Las tres condiciones que excluyen a Benford por identificador se publicaban
  # como una DISYUNCION -"parece un identificador (tipo_inferido,
  # posible_identificador o secuencia correlativa)"- y el usuario no podia saber
  # cual. Peor: medido sobre `c(1:2000, rep(500, 500))`, la cobertura invocaba
  # ese motivo mientras el mismo perfil NO publicaba `posible_identificador` y en
  # su lugar publicaba `valor_concentrado`. De las tres alternativas nombradas,
  # dos eran falsas en esa salida. Lo que corresponde es nombrar el HECHO: el
  # criterio del hallazgo y el de esta guarda no son el mismo -la guarda mira la
  # columna cruda y el hallazgo la fila resumida, que ademas rechaza la moda
  # acantilada- y por eso pueden diferir. Que difieran es defendible; publicar
  # una razon que no ocurrio, no.
  #
  # `posible_identificador` tiene dos textos, y solo uno describe una
  # numeracion: el de la alta unicidad. El otro -"los valores ocupan un tramo
  # compacto de los enteros y se repiten"- es exactamente la forma de una
  # magnitud entera que lleno su rango, y con 200.000 filas lo recibe una
  # columna que cumple Benford (ver `.numeracion_sin_repetir()`). Contarlo aca
  # devolvia el silencio que depende de `n` por la otra puerta. Una clave
  # declarada no pasa por esto: es un hecho, se repita o no.
  repite <- !.numeracion_sin_repetir(finitos)
  motivos_identificador <- c(
    if (identical(as.character(tipo_inferido), "identificador")) {
      "el tipo inferido es identificador"
    },
    if (isTRUE(posible_identificador) &&
        (isTRUE(clave_declarada) || !repite)) {
        "el perfil p\u00fablico `posible_identificador`"
    },
    if (.parece_correlativo_benford(x)) {
      "los valores forman una secuencia correlativa"
    }
  )
  es_identificador <- length(motivos_identificador) > 0L

  # Una columna declarada en `columnas_codigo` es un codigo por declaracion, y
  # sobre un codigo Benford no se aplica: la unica precondicion que se publica
  # es esa, porque las demas no llegan a importar.
  fallas <- if (isTRUE(codigo_declarado)) {
    "codigo_declarado"
  } else {
    .precondiciones_benford(
      variacion, es_identificador, n_positivos, proporcion_positivos,
      ordenes, umbrales
    )
  }

  base <- list(
    columna = nombre,
    aplica = !length(fallas),
    precondiciones_fallidas = fallas,
    n_finitos = as.integer(n_finitos),
    n_positivos = as.integer(n_positivos),
    proporcion_positivos = proporcion_positivos,
    ordenes_magnitud = ordenes,
    parece_identificador = es_identificador,
    motivos_identificador = motivos_identificador,
    clave_declarada = isTRUE(clave_declarada),
    codigo_declarado = isTRUE(codigo_declarado)
  )
  if (length(fallas)) return(base)

  resultado <- c(base, .prueba_benford(positivos, umbrales))
  sin_sospechosos <- .benford_sin_sospechosos(
    x, sospechosos, es_identificador, umbrales
  )
  if (!is.null(sin_sospechosos)) {
    resultado$sin_faltantes_disfrazados <- sin_sospechosos
  }
  resultado
}

# La ayuda promete `log10(max/min)`, y la resta de logaritmos no es esa cuenta
# en el borde: con 534 y 0,534 el cociente da 1000 exacto en doble y
# `log10(1000)` es 3, que cumple; la resta daba 2,9999999999999996 y el motivo
# publicaba "3.000000000000000 < 3". De 50.000 pares decimales con cociente
# 1000, la resta dejaba 5.728 por debajo de 3. El cociente solo puede desbordar
# con extremos fuera de toda escala -un subnormal y un 1e300-, y ahi la resta es
# la misma magnitud sin desbordar.
.ordenes_magnitud_benford <- function(positivos, variacion) {
  if (!length(positivos) || !variacion) return(NA_real_)
  cociente <- max(positivos) / min(positivos)
  if (is.finite(cociente)) {
    log10(cociente)
  } else {
    log10(max(positivos)) - log10(min(positivos))
  }
}

.prueba_benford <- function(positivos, umbrales) {
  n_positivos <- length(positivos)
  digitos <- .primer_digito_significativo(positivos)
  observados <- tabulate(digitos, nbins = 9L)
  proporcion_esperada <- log10(1 + 1 / seq_len(9L))
  esperados <- n_positivos * proporcion_esperada
  estadistico <- sum((observados - esperados) ^ 2 / esperados)
  p_valor <- stats::pchisq(estadistico, df = 8L, lower.tail = FALSE)
  distribucion <- data.frame(
    digito = seq_len(9L),
    n_observado = as.integer(observados),
    proporcion_observada = as.numeric(observados / n_positivos),
    proporcion_esperada = as.numeric(proporcion_esperada),
    n_esperado = as.numeric(esperados),
    stringsAsFactors = FALSE
  )
  list(
    metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
    estadistico = as.numeric(estadistico),
    grados_libertad = 8L,
    p_valor = as.numeric(p_valor),
    distribucion = distribucion,
    desviacion = isTRUE(p_valor < umbrales$nivel_significacion)
  )
}

# **La desviacion que fabrican los posibles centinelas.** Medido: 4.000 montos
# con 200 `999` sin declarar publicaban `desviacion_benford` -X2 = 232, p =
# 1e-45- en el mismo perfil que contaba esos 200 como `faltantes_disfrazados`;
# sin ellos la columna cumple (X2 = 10,6, p = 0,22). Los valores de la lista
# por omision no son una declaracion y no salen de ningun diagnostico -el
# resumen tambien los cuenta, ver `.universo_del_resumen()`-, asi que la prueba
# sobre los datos tal como vienen sigue siendo la que decide. Lo que faltaba es
# lo que el paquete hace con un centinela que sospecha y no le declararon
# (`posible_centinela_numerico`): decir que esta ahi y que pasa si se declara.
# La misma prueba, sin esos valores, va a la evidencia y a
# `sin_faltantes_disfrazados`, con sus precondiciones, porque sacar un valor
# extremo puede dejar a la columna sin los ordenes de magnitud o las
# observaciones que exige.
.benford_sin_sospechosos <- function(x, sospechosos, es_identificador,
                                     umbrales) {
  if (is.null(sospechosos) || !is.logical(sospechosos) ||
      length(sospechosos) != length(x) || !is.numeric(x)) {
    return(NULL)
  }
  valores <- as.numeric(x)
  utilizables <- is.finite(valores)
  marcados <- !is.na(sospechosos) & sospechosos & utilizables & valores > 0
  if (!any(marcados)) return(NULL)
  finitos <- valores[utilizables & !marcados]
  positivos <- finitos[finitos > 0]
  variacion <- length(unique(finitos)) > 1L
  ordenes <- .ordenes_magnitud_benford(positivos, variacion)
  # La precondicion de identificador es la de la columna entera: sacar un
  # centinela no convierte un monto en una numeracion ni al reves.
  fallas <- .precondiciones_benford(
    variacion, es_identificador, length(positivos),
    if (length(finitos)) length(positivos) / length(finitos) else NA_real_,
    ordenes, umbrales
  )
  base <- list(
    n_excluidos = sum(marcados),
    valores_excluidos = .sort_por_bytes(unique(valores[marcados])),
    n_positivos = length(positivos),
    proporcion_positivos = if (length(finitos)) {
      length(positivos) / length(finitos)
    } else NA_real_,
    ordenes_magnitud = ordenes,
    aplica = !length(fallas),
    precondiciones_fallidas = fallas
  )
  if (length(fallas)) return(base)
  prueba <- .prueba_benford(positivos, umbrales)
  c(base, prueba[c("estadistico", "p_valor", "desviacion")])
}

# Una comparacion que se publica tiene que leerse verdadera: 2,99957 con tres
# decimales es "3.000", y el motivo decia "ordenes de magnitud 3.000 < 3". Se
# agregan decimales hasta que el valor y el umbral se distingan.
.decimal_frente_a_umbral <- function(valor, umbral, digits = 3L) {
  if (!isTRUE(is.finite(valor)) || !isTRUE(is.finite(umbral)) ||
      valor == umbral) {
    return(.formatear_decimal_publicado(valor, digits))
  }
  # Se comparan los TEXTOS y no `round()`: los dos no redondean igual, y con
  # 2,9999999999999996 `round(valor, 15)` ya difiere de 3 mientras `formatC()`
  # con 15 decimales sigue escribiendo "3.000000000000000". Y el tope es 17: con
  # 15 decimales dos dobles distintos pueden escribirse igual.
  for (d in seq.int(digits, 17L)) {
    texto <- .formatear_decimal_publicado(valor, d)
    if (!identical(texto, .formatear_decimal_publicado(umbral, d))) {
      return(texto)
    }
  }
  .formatear_decimal_publicado(valor, 17L)
}

.texto_p_benford <- function(p_valor) {
  opciones <- options(scipen = 0)
  on.exit(options(opciones), add = TRUE)
  format.pval(p_valor, digits = 4L)
}

# Lo que el hallazgo dice de la prueba sin los faltantes disfrazados que nadie
# declaro: ver `.benford_sin_sospechosos()`. Vacio si no hay ninguno.
.textos_benford_sin_sospechosos <- function(sin, umbrales) {
  if (is.null(sin)) {
    return(list(descripcion = "", evidencia = "", como_resolverlo = ""))
  }
  valores <- paste(
    .formatear_numero_publicado(utils::head(sin$valores_excluidos, 5L)),
    collapse = ", "
  )
  if (length(sin$valores_excluidos) > 5L) valores <- paste0(valores, ", ...")
  inicio <- paste0(
    " Sin los ", .formatear_numero_publicado(sin$n_excluidos),
    if (sin$n_excluidos == 1L) " valor que el perfil cuenta " else
      " valores que el perfil cuenta ",
    "como faltantes disfrazados sin que est\u00e9n declarados en ",
    "`sentinelas_numericos` (", valores, ")"
  )
  if (!isTRUE(sin$aplica)) {
    motivo <- .motivo_no_aplica_benford(c(sin, list(
      clave_declarada = FALSE, motivos_identificador = character()
    )), umbrales, prefijo = "")
    return(list(
      descripcion = "",
      evidencia = paste0(inicio, ", la prueba no se puede hacer: ", motivo),
      como_resolverlo = .sugerencia_benford_sin_sospechosos(sin, valores)
    ))
  }
  evidencia <- paste0(
    inicio, ", la prueba sobre ", .formatear_numero_publicado(sin$n_positivos),
    if (sin$n_positivos == 1L) " valor positivo da X2=" else
      " valores positivos da X2=",
    .formatear_decimal_publicado(sin$estadistico),
    ", p=", .texto_p_benford(sin$p_valor),
    if (isTRUE(sin$desviacion)) ": la desviaci\u00f3n se mantiene sin ellos." else
      ": sin ellos la columna no se desv\u00eda."
  )
  list(
    descripcion = if (isTRUE(sin$desviacion)) "" else paste0(
      " Sin los valores que el perfil cuenta como faltantes disfrazados la ",
      "columna sigue ese reparto: la desviaci\u00f3n la producen ellos."
    ),
    evidencia = evidencia,
    como_resolverlo = .sugerencia_benford_sin_sospechosos(sin, valores)
  )
}

.sugerencia_benford_sin_sospechosos <- function(sin, valores) {
  if (length(sin$valores_excluidos) == 1L) {
    paste0(
      "Si ", valores, " es un c\u00f3digo de ausencia, declararlo en ",
      "`sentinelas_numericos`: sale de esta prueba como sale del resumen. "
    )
  } else {
    paste0(
      "Si ", valores, " son c\u00f3digos de ausencia, declararlos en ",
      "`sentinelas_numericos`: salen de esta prueba como salen del resumen. "
    )
  }
}

.motivo_no_aplica_benford <- function(resultado, umbrales,
                                      prefijo = paste0(
                                        "No aplica la ley de Benford. ",
                                        "Precondiciones fallidas: "
                                      )) {
  # «Parece un identificador» y «se declaro como clave» son dos afirmaciones
  # distintas: la primera es una inferencia del paquete y la segunda un hecho que
  # trajo el usuario. Publicar la primera cuando corresponde la segunda le
  # atribuye al paquete una deduccion que no hizo, y ademas invita a discutirle
  # el criterio cuando no hubo criterio.
  etiquetas <- c(
    sin_variacion = "sin variaci\u00f3n",
    codigo_declarado = paste(
      "la columna se declar\u00f3 en `columnas_codigo`, as\u00ed que es un c\u00f3digo y no",
      "una magnitud"
    ),
    parece_identificador = if (isTRUE(resultado$clave_declarada)) {
      # Se probo agregar un aviso cuando la columna declarada como clave se
      # reparte por muchos ordenes de magnitud, para que una declaracion
      # equivocada -un monto declarado como clave- no se lleve el analisis en
      # silencio. **Se retiro midiendo**: una clave dispersa real de 1 a 600.000
      # da 4,3 ordenes y un monto da 3,3, asi que el aviso salta en las dos y
      # deja de significar algo. Es la misma razon por la que la densidad no
      # servia para reconocer una clave.
      #
      # Lo que si cubre el caso comun: si la columna declarada repite valores,
      # `clave_no_unica` lo informa con severidad `error`.
      "la clave fue declarada, as\u00ed que la columna identifica filas y no es una magnitud"
    } else if (length(resultado$motivos_identificador)) {
      paste0(
        "parece un identificador: ",
        paste(resultado$motivos_identificador, collapse = " y ")
      )
    } else {
      "parece un identificador"
    },
    observaciones_utilizables_insuficientes = paste0(
      if (resultado$n_positivos == 1L) "observaci\u00f3n positiva utilizable " else
        "observaciones positivas utilizables ",
      .formatear_numero_publicado(resultado$n_positivos),
      " < ",
      .formatear_numero_publicado(umbrales$minimo_observaciones_utilizables)
    ),
    proporcion_positivos_insuficiente = paste0(
      "proporci\u00f3n de positivos ",
      .decimal_frente_a_umbral(
        resultado$proporcion_positivos, umbrales$minima_proporcion_positivos
      ),
      " < ",
      .formatear_decimal_publicado(umbrales$minima_proporcion_positivos)
    ),
    ordenes_magnitud_insuficientes = paste0(
      "\u00f3rdenes de magnitud log10(max/min) ",
      if (is.finite(resultado$ordenes_magnitud)) {
        .decimal_frente_a_umbral(
          resultado$ordenes_magnitud, umbrales$minimo_ordenes_magnitud
        )
      } else {
        "no calculables"
      },
      " < ", .formatear_numero_publicado(umbrales$minimo_ordenes_magnitud)
    )
  )
  paste0(
    prefijo,
    paste(unname(etiquetas[resultado$precondiciones_fallidas]), collapse = "; "),
    "."
  )
}

# `disfrazados`: la mascara de faltantes disfrazados de cada columna, la misma
# que cuenta `n_faltantes_disfrazados` en la fila del perfil.
.diagnosticar_benford <- function(datos, columnas, hallazgos,
                                  clave_declarada = NULL,
                                  columnas_codigo = NULL,
                                  disfrazados = NULL) {
  candidatas <- which(vapply(datos, .columna_candidata_benford, logical(1L)))

  # `integer64` esta excluido por tipo desde el primer commit, junto con `Date`,
  # `POSIXt` y `difftime`. Esos tres son tipos de tiempo y Benford no les aplica
  # por naturaleza; `integer64` en cambio ES un numero, y la exclusion lo dejaba
  # en silencio: la MISMA columna `1:1000` guardada en `entero` producia una fila
  # de cobertura -"ley_benford no aplica, es un identificador"- y guardada en
  # `integer64` no producia nada. Un diagnostico que no corre se declara; esa es
  # la regla del paquete y aca no se cumplia.
  #
  # No se amplia el alcance de Benford a `integer64` -eso cambiaria conducta
  # estadistica y es otra decision-: se declara la no evaluacion, que es lo que
  # faltaba.
  #
  # Y no solo para `integer64`: `inherits(x, "integer64")` es una lista de una
  # sola clase, y con ella `units`, `Period` o `haven_labelled` quedarian en el
  # mismo silencio del que este comentario habla. La pregunta es
  # si la columna es numerica y trae clase declarada, no cual clase es.
  excluidas_por_tipo <- which(vapply(datos, function(x) {
    .es_numerica_con_clase(x) && !.es_columna_compuesta(x) &&
      sum(is.finite(x)) >= 50L
  }, logical(1L)))
  cobertura_tipo <- lapply(excluidas_por_tipo, function(i) {
    .nuevo_diagnostico_no_evaluado(
      "ley_benford", names(datos)[[i]],
      paste0(
        "La columna se guarda como `", .clase_declarada_columna(datos[[i]]),
        "` y el diagn\u00f3stico no se eval\u00faa sobre ese tipo."
      ),
      paste0(
        "No leer la ausencia de hallazgo como conformidad. Para evaluar Benford ",
        "sobre esta columna, convertirla a `double` si sus valores entran en la ",
        "precisi\u00f3n de ese tipo."
      )
    )
  })

  if (!length(candidatas)) {
    return(list(
      hallazgos = list(),
      cobertura = if (length(cobertura_tipo)) {
        do.call(rbind, cobertura_tipo)
      } else .cobertura_diagnosticos_vacia(),
      meta = NULL
    ))
  }

  umbrales <- .umbrales_benford()
  identificadores <- .nombres_para_operar(hallazgos$columna[
    hallazgos$tipo_hallazgo == "posible_identificador"
  ])
  # Una clave que el usuario DECLARO no necesita que se infiera nada: dijo que
  # identifica una fila, y una numeracion no tiene distribucion que analizar.
  #
  # Esto cierra el caso que motivo -y tumbo- la regla de la clave dispersa: ahi
  # se intentaba adivinar cual columna era clave por la forma de sus valores, y
  # el criterio terminaba dependiendo de cuantas filas se habian cargado y
  # callando magnitudes reales. Declarada, la respuesta ya esta, no depende del
  # tamano de la tabla, y no calla nada: si la columna es de verdad una clave,
  # Benford sobre ella no significaba nada.
  #
  # Lo mismo vale para la clave leida del catalogo de la base, que llega por
  # aca cuando quien perfila la pasa.
  identificadores <- unique(c(
    identificadores, .nombres_para_operar(as.character(clave_declarada))
  ))
  resultados <- lapply(candidatas, function(i) {
    nombre <- names(datos)[[i]]
    clave_operativa <- .nombres_para_operar(nombre)
    .resultado_benford_columna(
      datos[[i]], nombre, columnas$tipo_inferido[[i]],
      clave_operativa %in% identificadores, umbrales,
      clave_declarada = clave_operativa %in%
        .nombres_para_operar(as.character(clave_declarada)),
      codigo_declarado = clave_operativa %in%
        .nombres_para_operar(as.character(columnas_codigo)),
      sospechosos = if (length(disfrazados) >= i) disfrazados[[i]]
    )
  })
  names(resultados) <- names(datos)[candidatas]

  cobertura <- lapply(resultados, function(resultado) {
    if (isTRUE(resultado$aplica)) return(NULL)
    .nuevo_diagnostico_no_evaluado(
      "ley_benford", resultado$columna,
      .motivo_no_aplica_benford(resultado, umbrales),
      if (isTRUE(resultado$codigo_declarado)) paste(
        "Si la columna es una magnitud y no un c\u00f3digo, quitarla de",
        "`columnas_codigo` para evaluar Benford."
      ) else paste0(
        "No interpretar una distribuci\u00f3n de primeros d\u00edgitos. Benford requiere ",
        "montos positivos no asignados, al menos ",
        .formatear_numero_publicado(umbrales$minimo_observaciones_utilizables),
        if (umbrales$minimo_observaciones_utilizables == 1L) " observaci\u00f3n y al menos " else
          " observaciones y al menos ",
        .formatear_numero_publicado(umbrales$minimo_ordenes_magnitud),
        if (umbrales$minimo_ordenes_magnitud == 1L) " orden de magnitud." else
          " \u00f3rdenes de magnitud."
      )
    )
  })
  cobertura <- cobertura[!vapply(cobertura, is.null, logical(1L))]
  cobertura <- c(cobertura, cobertura_tipo)
  cobertura <- if (length(cobertura)) {
    do.call(rbind, unname(cobertura))
  } else {
    .cobertura_diagnosticos_vacia()
  }

  nuevos <- lapply(resultados, function(resultado) {
    if (!isTRUE(resultado$aplica) || !isTRUE(resultado$desviacion)) return(NULL)
    sospechosos <- .textos_benford_sin_sospechosos(
      resultado$sin_faltantes_disfrazados, umbrales
    )
    .nuevo_hallazgo(
      resultado$columna, "desviacion_benford", "sospechoso",
      paste0(
        "En muchas magnitudes que crecen por acumulaci\u00f3n -importes, poblaciones, ",
        "\u00e1reas- el primer d\u00edgito no aparece con la misma frecuencia: el 1 encabeza ",
        "cerca del 30 % de los valores y el 9 menos del 5 %. En esta columna los ",
        "primeros d\u00edgitos no siguen ese reparto. Es una se\u00f1al descriptiva para ",
        "revisar, no evidencia de fraude ni de manipulaci\u00f3n: un tope ",
        "administrativo o un monto fijo la producen igual.",
        sospechosos$descripcion
      ),
      paste0(
        resultado$metodo, ": X2=",
        .formatear_decimal_publicado(resultado$estadistico),
        ", p=", .texto_p_benford(resultado$p_valor),
        "; observado/esperado por d\u00edgito: ",
        .texto_distribucion_benford(resultado$distribucion), ".",
        sospechosos$evidencia
      ),
      paste0(
        sospechosos$como_resolverlo,
        "Revisar el proceso y explicaciones inocentes como topes administrativos, ",
        "redondeos, precios psicol\u00f3gicos o subsidios de monto fijo."
      ),
      resultado$n_positivos, NA_real_, "valor_positivo"
    )
  })
  nuevos <- nuevos[!vapply(nuevos, is.null, logical(1L))]
  # Los ordenes de magnitud sin los sospechosos solo los usa el motivo de la
  # evidencia: son log10(max/min), una relacion entre dos datos que la
  # proteccion de datos personales borra de `ordenes_magnitud` y que en una
  # lista anidada se le escaparia.
  resultados <- lapply(resultados, function(resultado) {
    if (!is.null(resultado$sin_faltantes_disfrazados)) {
      resultado$sin_faltantes_disfrazados$ordenes_magnitud <- NULL
    }
    resultado
  })

  list(
    hallazgos = nuevos,
    cobertura = cobertura,
    meta = list(
      metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
      umbrales = umbrales,
      resultados = resultados
    )
  )
}

# Lo que el usuario DECLARO fuera del universo sale del resumen -`media`,
# `minimo`, `n_outliers`- y tiene que salir tambien de los diagnosticos que leen
# la columna como magnitud: Benford y las relaciones aritmeticas. Los dos tomaban
# la columna cruda, y el mismo perfil afirmaba dos cosas contrarias sobre las
# mismas celdas. Son dos declaraciones:
#
# - **`sentinelas_numericos`.** Medido: 160 `999` declarados en un monto
#   lognormal; el resumen publicaba la media sin ellos y Benford contaba 2.000
#   valores y publicaba `desviacion_benford` (X2 = 245,4), cuando sin los `999`
#   la columna cumple (n = 1.840, X2 = 3,98, p = 0,86). Y con `-9` declarado en
#   ocho filas de `b`, `a + b ~= total` salia `sospechoso` acusando justo esas
#   ocho filas, que el resumen contaba como faltantes.
# - **`aplicabilidad`.** Las filas donde la regla dice que la columna no
#   corresponde -o donde no se puede decidir- salen del resumen, y Benford y la
#   aritmetica las seguian contando: un valor que el usuario declaro fuera del
#   universo sostenia una desviacion o acusaba una fila.
#
# No se reescribe ninguna regla: se usan las MISMAS piezas que el resumen -que
# valores son declaracion (`.sentinelas_numericos_declarados()`), que celdas los
# contienen (`.mascara_sentinelas_resumen()`) y la mascara de aplicabilidad que
# `.resolver_aplicabilidad()` arma una vez para todo el perfil-, y la celda pasa
# a `NA`, que es como el resumen la trata. La lista por omision no es una
# declaracion y no saca nada, igual que en el resumen.
.universo_del_resumen <- function(datos, sentinelas_numericos,
                                  mascaras_aplicabilidad = NULL) {
  declarados <- .sentinelas_numericos_declarados(sentinelas_numericos)
  if (!length(datos)) return(datos)
  for (j in seq_along(datos)) {
    x <- datos[[j]]
    if (!.es_numerico_pelado(x) || .es_columna_compuesta(x)) next
    fuera <- rep(FALSE, length(x))
    aplicable <- if (length(mascaras_aplicabilidad) >= j) {
      mascaras_aplicabilidad[[j]]
    } else NULL
    if (!is.null(aplicable) && length(aplicable) == length(x)) {
      fuera <- fuera | !as.logical(aplicable)
    }
    if (length(declarados)) {
      fuera <- fuera | .mascara_sentinelas_resumen(as.numeric(x), declarados)
    }
    if (any(fuera)) {
      x[fuera] <- NA
      datos[[j]] <- x
    }
  }
  datos
}
