# La via DBI mide dos cosas distintas: agregados exactos sobre la tabla
# completa y un perfil completo sobre una muestra traida a memoria. Son
# independientes, y ninguna puede descartar a la otra.
#
# Regla que gobierna todo este archivo: ante un fallo parcial se devuelve lo
# medido con su alcance declarado; nunca el todo descartado, y nunca un cero
# por ausencia de medicion. Un motor que rechaza una consulta degrada esa
# metrica, no el resumen entero.

.dbi_disponible <- function() {
  requireNamespace("DBI", quietly = TRUE)
}

.requerir_dbi <- function() {
  if (!.dbi_disponible()) {
    stop(
      "Para perfilar una muestra de una base se necesita instalar el paquete opcional 'DBI'.",
      call. = FALSE
    )
  }
}

# ---- Condiciones propias -------------------------------------------------
#
# Sin clase propia no hay `tryCatch()` posible: quien llama solo puede mirar el
# texto del mensaje, que cambia con el idioma del motor. Cada porton de la via
# DBI senala con su clase, y todas heredan de `lupa_error_dbi`.

.condicion_dbi <- function(clase, mensaje, datos = list()) {
  condicion <- list(message = mensaje, call = NULL, datos = datos)
  class(condicion) <- c(clase, "lupa_error_dbi", "error", "condition")
  condicion
}

.detener_dbi <- function(clase, mensaje, datos = list()) {
  stop(.condicion_dbi(clase, mensaje, datos))
}

.avisar_dbi <- function(clase, mensaje, datos = list()) {
  aviso <- list(message = mensaje, call = NULL, datos = datos)
  class(aviso) <- c(clase, "lupa_aviso_dbi", "warning", "condition")
  warning(aviso)
}

# ---- Presupuesto de consultas -------------------------------------------
#
# Los agregados de una tabla ancha se emiten por lotes y `muestra` cambia
# exactamente una cosa: acota lo que se trae a R, no el trabajo del motor.
# `max_consultas` tambien lo acota, y lo que no entra en el presupuesto se
# declara no disponible en vez de quedar en un cero silencioso.

# Resolver la forma del desvio cuesta siempre dos sondas, se acierte en la
# primera o no. Podria cortarse al primer acierto y ahorrar una, pero entonces
# el costo dependeria del motor y el plan dejaria de decir exactamente cuantas
# consultas va a emitir. La exactitud del plan vale mas que una consulta.
.PEAJE_FORMA_DESVIO <- 2L

# Veinte columnas dejan el bloque de cinco agregados en unas cien expresiones;
# es un limite conservador para motores con topes de expresiones y sigue dando
# tres consultas sobre una tabla de sesenta columnas.
.TAMANO_LOTE_PLANOS_DBI <- 20L
# Una cardinalidad exacta puede derramar mucho mas que un bloque de agregados
# planos, y por eso tiene su propio lote. Cuanto conviene agrupar se midio sobre
# un PostgreSQL real, con una tabla de 4,5 M filas y `work_mem` de 32 MB:
#
#   lote  exec total  exec/columna  Temp Written  Shared Read
#      1      27,6 s        27,6 s         7.200       90.384
#      2      30,4 s        15,2 s        13.764       90.320
#      4      60,2 s        15,0 s        18.291       90.256
#      8       194 s        24,3 s        43.694       90.192
#
# El `Shared Read` es CONSTANTE entre 1 y 8: el motor hace una sola pasada por la
# tabla y la amortiza entre todos los agregados del mismo SELECT. Lo que crece es
# el derrame, porque los hashes compiten por `work_mem`. Por eso el optimo esta
# en el medio y no en ningun extremo: para ocho conteos, el lote 1 cuesta ~221 s
# y el lote 4 cuesta ~120 s.
#
# Va 2 y no 4 porque dan el mismo tiempo por columna -15,2 contra 15,0- pero el 2
# derrama un 25 % menos, y el optimo depende de `work_mem`, que cambia por
# servidor: el 2 se degrada mejor donde hay poca memoria y rinde casi igual donde
# hay mucha. No lo cambies por una corazonada; si lo cambias, medi de nuevo.
.TAMANO_LOTE_DISTINTOS_DBI <- 2L
# Alias interno y de compatibilidad para codigo que aun menciona el nombre
# anterior. Las dos familias no usan este valor como tamano comun.
.TAMANO_LOTE_DBI <- .TAMANO_LOTE_PLANOS_DBI

# Registro cerrado de los metodos que publican las filas SQL. Las claves tienen
# que coincidir textualmente con `metodo`: no se infiere la clase por la
# metrica, porque una misma metrica puede tener metodos con memoria distinta.
.REGISTRO_MEMORIA_TRABAJO_DBI <- c(
  "COUNT(DISTINCT)" = "creciente",
  "APPROX_COUNT_DISTINCT" = "acotado",
  "approx_count_distinct" = "acotado",
  "approx_count_distinct_generico" = "acotado",
  "ventana_agregado" = "creciente",
  "consulta_actual_sin_guardian" = "creciente",
  "subconsulta_escalar" = "creciente",
  # La CTE de ventanas ordena las filas no nulas. Con un tope real el
  # clasificador de muestras la reduce a R2; cuando la muestra satura el
  # universo, el sort vuelve a crecer con las filas y cae a R3. Las dos
  # etiquetas de fuente son aliases deliberados para que una futura variante
  # pueda publicar el origen sin quedar sin clasificar.
  "cte_ventana" = "creciente",
  "cte_ventana_tablesample_system" = "creciente",
  "cte_ventana_newid" = "creciente",
  "dos_consultas" = "creciente",
  "PERCENTILE_CONT" = "creciente",
  "PERCENTILE_CONT_OVER" = "creciente",
  "approx_percentile" = "acotado",
  "approx_quantile" = "acotado",
  "percentile_approx" = "acotado",
  "quantile" = "acotado",
  # El spool saturado recorre la tabla completa y su trabajo crece con las filas.
  "spool_sesion_cliente" = "creciente",
  # I1 libera cada bloque antes de pedir el siguiente. El estado de los
  # acumuladores queda acotado por `E_distintos` y `P_estado`, no por `n`.
  "dbfetch_bloques" = "acotado",
  "tabla_completa" = "acotado",
  "conteo_universo" = "acotado",
  "pg_stats.n_distinct" = "acotado"
)

# Las capacidades nuevas se sondean una vez por corrida. Todas sus formas
# candidatas se prueban aunque una ya haya acertado: el costo no puede depender
# de que el primer candidato sea aceptado por el motor.

# Las sondas van sobre una constante, no sobre la tabla: lo unico que se prueba
# es si el motor conoce la funcion.
.sondar_forma_desvio <- function(conexion, presupuesto, alias) {
  if (!is.null(presupuesto$forma_desvio)) return(invisible(NULL))
  nativas <- c("STDDEV_SAMP", "STDEV")
  elegida <- NULL
  for (i in seq_along(nativas)) {
    sql <- paste0(
      "SELECT ", nativas[[i]], "(1.0) AS ", alias("desvio"),
      if (.es_oracle_dbi(conexion)) " FROM DUAL" else ""
    )
    intento <- .escalar_dbi(
      conexion, sql, "desvio", presupuesto, etapa = "sonda_desvio"
    )
    if (is.null(elegida) && isTRUE(intento$ok)) elegida <- i
  }
  # Sin ninguna nativa queda el calculo de dos pasadas, que es la ultima forma.
  presupuesto$forma_desvio <- if (is.null(elegida)) 3L else elegida
  invisible(NULL)
}

.presupuesto_dbi <- function(max_consultas = Inf, instrumentar = TRUE) {
  estado <- new.env(parent = emptyenv())
  estado$max <- max_consultas
  estado$usadas <- 0
  estado$reserva <- 0
  estado$agotado <- FALSE
  # La instrumentacion se puede apagar porque medir tambien es trabajo. Los
  # identificadores siguen avanzando cuando esta apagada: identifican el
  # intento de consulta, no afirman que su duracion se haya medido.
  estado$instrumentar <- isTRUE(instrumentar)
  estado$siguiente_consulta <- 0L
  # La forma de calcular el desvio se resuelve una vez por corrida y se recuerda:
  # probarla por columna gastaria hasta tres consultas cada vez y romperia la
  # promesa del plan, que dice exactamente cuantas va a emitir.
  estado$forma_desvio <- NULL
  # Este valor vive en el presupuesto de la corrida y muere con el: sirve para
  # recordar un lote aceptado sin guardar estado global asociado a una conexion.
  estado$tamano_lote_funciono <- NULL
  estado$tamano_lote_planos_funciono <- NULL
  estado$tamano_lote_distintos_funciono <- NULL
  # La referencia temporal de distintos se llena con el primer lote distinto.
  # No se usan estadisticas del catalogo ni agregados planos para proyectar ese
  # costo: son unidades diferentes.
  estado$referencias_distintos <- list()
  estado$proyeccion_distintos <- NULL
  # La moda y la mediana tienen sus propios relojes de referencia: agrupar se
  # proyecta por distinto y ordenar por fila. Las referencias mueren con esta
  # corrida, igual que la de `COUNT(DISTINCT)`.
  estado$referencias_moda <- list()
  estado$proyeccion_moda <- NULL
  estado$referencias_mediana <- list()
  estado$proyeccion_mediana <- NULL
  estado$medianas_pendientes <- NULL
  # La consulta que deja disponible el conteo puede servir como cota de la
  # lectura de cada mediana. No es una tasa de mediana y nunca reemplaza la
  # referencia local o bancaria; solo evita que una referencia bancaria corta
  # deje silencioso un ordenamiento que ya sabemos que debe recorrer las filas.
  estado$duracion_lectura_mediana <- NA_real_
  estado$aviso_moda_emitido <- FALSE
  estado$aviso_mediana_emitido <- FALSE
  estado$aviso_derrame_moda_emitido <- FALSE
  estado$aviso_derrame_mediana_emitido <- FALSE
  # Los avisos se controlan por separado porque hablan de magnitudes y de
  # familias de trabajo incompatibles: segundos de servidor para distintos,
  # moda y mediana, y bytes para memoria. No hay un porton comun que permita
  # silenciar uno sin ocultar el otro.
  estado$avisar_costo_distintos <- TRUE
  estado$umbral_segundos_aviso_distintos <- .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI
  estado$avisar_derrame_estimado <- TRUE
  estado$umbral_bytes_aviso_derrame_estimado <-
    .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI
  # La memoria del hash se estima con estadisticas del catalogo, pero nunca se
  # guarda como si fuera una medicion del derrame. Vive en el presupuesto para
  # que el plan y la corrida conserven el mismo diagnostico sin consultar dos
  # veces el catalogo.
  estado$estimacion_derrame <- list(
    estado = "no_solicitado", disponible = FALSE, es_estimacion = TRUE,
    fuente = NA_character_, motivo = paste(
      "No se solicito una estimacion de derrame porque no se pidio",
      "COUNT(DISTINCT) exacto."
    ), work_mem = NA_character_, work_mem_bytes = NA_real_,
    hash_mem_multiplier = NA_real_, hash_mem_multiplier_disponible = FALSE,
    memoria_efectiva_bytes = NA_real_, memoria_efectiva = NA_character_,
    columnas = data.frame(), lotes = data.frame(),
    lotes_sobre_memoria = integer()
  )
  estado$estimacion_derrame_moda <- .estimacion_derrame_familia_vacia_dbi(
    "moda", "No se solicito una estimacion de derrame para la moda."
  )
  estado$estimacion_derrame_mediana <- .estimacion_derrame_familia_vacia_dbi(
    "mediana", "No se solicito una estimacion de derrame para la mediana."
  )
  # La lectura de pg_stat_statements es opcional: si el servidor no la expone,
  # el informe conserva la incertidumbre y no deduce un derrame del reloj.
  estado$derrame <- list(
    estado = "no_solicitado", disponible = FALSE,
    fuente = NA_character_, motivo = paste(
      "No se solicito una medicion de derrame porque no se pidio",
      "COUNT(DISTINCT) exacto."
    ), consultas = NULL
  )
  # Cuantas consultas espera emitir la corrida y la barra que lo muestra. Van
  # aca porque el presupuesto es el unico objeto que ve pasar TODAS las
  # consultas: colgarlo de otro lado obligaria a enhebrarlo por cada camino.
  estado$previstas <- NA_real_
  estado$barra <- NULL
  estado
}

.registrar_referencia_distintos_dbi <- function(presupuesto, consulta) {
  if (is.null(presupuesto) || !isTRUE(consulta$ok) ||
      is.null(consulta$consulta_id) || length(consulta$consulta_id) != 1L ||
      is.na(consulta$consulta_id) || is.null(consulta$duracion_ms) ||
      length(consulta$duracion_ms) != 1L || is.na(consulta$duracion_ms) ||
      !is.finite(consulta$duracion_ms) || consulta$duracion_ms <= 0) {
    return(invisible(NULL))
  }
  id <- as.character(consulta$consulta_id)
  referencias <- presupuesto$referencias_distintos
  referencias[[id]] <- list(
    consulta_id = as.integer(consulta$consulta_id),
    duracion_ms = as.numeric(consulta$duracion_ms)
  )
  presupuesto$referencias_distintos <- referencias
  invisible(NULL)
}

.proyeccion_distintos_vacia_dbi <- function(n_lotes, motivo = paste(
    "No hay una duracion medida del primer lote de distintos en esta corrida;",
    "no se publica una proyeccion temporal."
  )) {
  list(
    disponible = FALSE, duracion_estimada_ms = NA_real_,
    duracion_referencia_ms = NA_real_, n_lotes = as.integer(n_lotes),
    n_referencias = 0L, fuente = NA_character_, motivo = motivo
  )
}

.proyectar_costo_distintos_dbi <- function(presupuesto, n_lotes) {
  vacia <- .proyeccion_distintos_vacia_dbi(n_lotes)
  if (!is.numeric(n_lotes) || length(n_lotes) != 1L || is.na(n_lotes) ||
      !is.finite(n_lotes) || n_lotes < 1) {
    return(vacia)
  }
  if (n_lotes == 1) {
    return(.proyeccion_distintos_vacia_dbi(
      n_lotes,
      paste(
        "Hay un solo lote de distintos: el costo ya se pago y no hay nada",
        "que proyectar ni que evitar."
      )
    ))
  }
  if (is.null(presupuesto) || !length(presupuesto$referencias_distintos)) {
    return(vacia)
  }
  duraciones <- vapply(
    presupuesto$referencias_distintos,
    function(x) as.numeric(x$duracion_ms), numeric(1L)
  )
  duraciones <- duraciones[is.finite(duraciones) & duraciones > 0]
  if (!length(duraciones)) return(vacia)
  referencia <- stats::median(duraciones)
  estimada <- referencia * as.numeric(n_lotes)
  if (!is.finite(estimada)) return(vacia)
  list(
    disponible = TRUE,
    duracion_estimada_ms = estimada,
    duracion_referencia_ms = referencia,
    n_lotes = as.integer(n_lotes),
    n_referencias = as.integer(length(duraciones)),
    fuente = paste(
      "mediana de", length(duraciones),
      "consulta(s) del primer lote de distintos medidas en esta corrida"
    ),
    motivo = paste(
      "La proyeccion usa la mediana del primer lote de `COUNT(DISTINCT)`",
      "medido en esta corrida y la multiplica por la cantidad de lotes; es",
      "una estimacion, no una medicion del agregado exacto total."
    )
  )
}

.registrar_referencia_moda_dbi <- function(
    presupuesto, consulta, n_distintos, columna = NA_character_) {
  if (is.null(presupuesto) || !isTRUE(consulta$ok) ||
      is.null(consulta$consulta_id) || length(consulta$consulta_id) != 1L ||
      is.na(consulta$consulta_id) || is.null(consulta$duracion_ms) ||
      length(consulta$duracion_ms) != 1L || is.na(consulta$duracion_ms) ||
      !is.finite(consulta$duracion_ms) || consulta$duracion_ms <= 0) {
    return(invisible(NULL))
  }
  distintos <- .numero_dbi(n_distintos)
  if (length(distintos) != 1L || is.na(distintos) || !is.finite(distintos) ||
      distintos <= 0) {
    return(invisible(NULL))
  }
  id <- as.character(consulta$consulta_id)
  referencias <- presupuesto$referencias_moda
  referencias[[id]] <- list(
    consulta_id = as.integer(consulta$consulta_id),
    columna = as.character(columna),
    duracion_ms = as.numeric(consulta$duracion_ms),
    n_distintos = distintos,
    ms_por_distinto = as.numeric(consulta$duracion_ms) / distintos
  )
  presupuesto$referencias_moda <- referencias
  invisible(NULL)
}

.proyeccion_moda_vacia_dbi <- function(
    motivo = paste(
      "No hay una duracion medida de una moda y una cardinalidad utilizable",
      "en esta corrida; no se publica una proyeccion temporal."
    ), columnas_sin_cardinalidad = character()) {
  list(
    disponible = FALSE, duracion_estimada_ms = NA_real_,
    duracion_referencia_ms = NA_real_, n_distintos_proyectados = NA_real_,
    n_columnas = 0L, n_referencias = 0L, fuente = NA_character_,
    columnas_sin_cardinalidad = as.character(columnas_sin_cardinalidad),
    fuentes_cardinalidad = character(),
    motivo = motivo
  )
}

.proyectar_costo_moda_dbi <- function(presupuesto, cardinalidades) {
  if (is.null(cardinalidades)) {
    return(.proyeccion_moda_vacia_dbi(
      "No se recibio la cardinalidad de las columnas pendientes; no se puede proyectar la moda."
    ))
  }
  if (is.numeric(cardinalidades)) {
    cardinalidades <- lapply(cardinalidades, function(valor) list(
      disponible = is.finite(valor) && valor >= 0,
      n_distintos = as.numeric(valor), fuente = "medicion"
    ))
  }
  if (!is.list(cardinalidades) || !length(cardinalidades)) {
    return(.proyeccion_moda_vacia_dbi(
      "No hay modas pendientes con cardinalidad; no se publica una proyeccion."
    ))
  }
  nombres <- names(cardinalidades)
  if (is.null(nombres)) nombres <- rep(NA_character_, length(cardinalidades))
  cardinalidad <- vapply(cardinalidades, function(x) {
    if (!is.list(x)) return(NA_real_)
    if (!is.null(x$disponible) && !isTRUE(x$disponible)) return(NA_real_)
    valor <- .numero_dbi(x$n_distintos)
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor < 0) {
      NA_real_
    } else valor
  }, numeric(1L))
  disponibles <- is.finite(cardinalidad) & cardinalidad >= 0
  sin_cardinalidad <- nombres[!disponibles]
  if (!any(disponibles)) {
    return(.proyeccion_moda_vacia_dbi(
      paste(
        "No se puede proyectar el costo de la moda porque falta la cardinalidad",
        "de todas las columnas pendientes.",
        if (length(sin_cardinalidad)) paste(
          "Columnas:", paste(sin_cardinalidad, collapse = ", ")
        ) else ""
      ),
      columnas_sin_cardinalidad = sin_cardinalidad
    ))
  }
  referencias <- if (is.null(presupuesto)) list() else presupuesto$referencias_moda
  if (is.null(referencias) || !length(referencias)) {
    return(.proyeccion_moda_vacia_dbi(
      paste(
        "No hay una primera moda medida en esta corrida para convertir la",
        "cardinalidad en tiempo; no se puede proyectar la moda.",
        if (length(sin_cardinalidad)) paste(
          "Tambien falta cardinalidad para:", paste(sin_cardinalidad, collapse = ", ")
        ) else ""
      ),
      columnas_sin_cardinalidad = sin_cardinalidad
    ))
  }
  tasas <- vapply(referencias, function(x) {
    valor <- .numero_dbi(x$ms_por_distinto)
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor <= 0) {
      duracion <- .numero_dbi(x$duracion_ms)
      distintos <- .numero_dbi(x$n_distintos)
      if (length(duracion) == 1L && is.finite(duracion) &&
          length(distintos) == 1L && is.finite(distintos) && distintos > 0) {
        valor <- duracion / distintos
      }
    }
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor <= 0) {
      NA_real_
    } else valor
  }, numeric(1L))
  tasas <- tasas[is.finite(tasas) & tasas > 0]
  if (!length(tasas)) {
    return(.proyeccion_moda_vacia_dbi(
      "Las modas ya medidas no tienen una duracion utilizable para convertir la cardinalidad.",
      columnas_sin_cardinalidad = sin_cardinalidad
    ))
  }
  referencia <- stats::median(tasas)
  unidades <- sum(cardinalidad[disponibles])
  estimada <- referencia * unidades
  if (!is.finite(estimada)) {
    return(.proyeccion_moda_vacia_dbi(
      "La proyeccion de la moda no es finita; no se publica un numero.",
      columnas_sin_cardinalidad = sin_cardinalidad
    ))
  }
  fuentes_disponibles <- vapply(cardinalidades[disponibles], function(x) {
    if (is.list(x) && !is.null(x$fuente) && length(x$fuente) &&
        !is.na(x$fuente[[1L]])) {
      as.character(x$fuente[[1L]])
    } else {
      "fuente no declarada"
    }
  }, character(1L))
  motivo <- paste(
    "La proyeccion usa la mediana de las tasas ms por distinto de las modas",
    "medidas en esta corrida y la multiplica por la cardinalidad disponible",
    "de las columnas pendientes. Fuentes de cardinalidad:",
    paste(unique(fuentes_disponibles), collapse = ", "), ".",
    if (length(sin_cardinalidad)) paste(
      "No incluye las columnas sin cardinalidad:", paste(sin_cardinalidad, collapse = ", "),
      "por lo que es parcial."
    ) else ""
  )
  list(
    disponible = TRUE, duracion_estimada_ms = estimada,
    duracion_referencia_ms = referencia,
    n_distintos_proyectados = unidades,
    n_columnas = as.integer(sum(disponibles)),
    n_referencias = as.integer(length(tasas)),
    fuente = paste(
      "mediana de", length(tasas),
      "moda(s) medida(s) en esta corrida (ms por distinto)"
    ),
    columnas_sin_cardinalidad = as.character(sin_cardinalidad),
    fuentes_cardinalidad = unique(fuentes_disponibles),
    motivo = motivo
  )
}

.avisar_costo_moda_dbi <- function(
    proyeccion, habilitado = TRUE,
    umbral_segundos = .UMBRAL_SEGUNDOS_AVISO_MODA_DBI) {
  habilitado <- .validar_interruptor_aviso_dbi(habilitado, "habilitado")
  umbral_segundos <- .validar_umbral_aviso_dbi(
    umbral_segundos, "umbral_segundos"
  )
  if (is.null(proyeccion) || !isTRUE(proyeccion$disponible) ||
      !isTRUE(habilitado) || !is.finite(umbral_segundos) ||
      is.na(proyeccion$duracion_estimada_ms) ||
      proyeccion$duracion_estimada_ms < umbral_segundos * 1000) {
    return(invisible(FALSE))
  }
  detalle <- if (length(proyeccion$columnas_sin_cardinalidad)) paste(
    " La proyeccion es parcial porque no hay cardinalidad para:",
    paste(proyeccion$columnas_sin_cardinalidad, collapse = ", "), "."
  ) else ""
  detalle_fuente <- if (length(proyeccion$fuentes_cardinalidad)) paste(
    " Fuentes de cardinalidad:",
    paste(proyeccion$fuentes_cardinalidad, collapse = ", "), "."
  ) else ""
  cli::cli_alert_warning(paste0(
    "Costo estimado de la moda: ~",
    .segundos_dbi(proyeccion$duracion_estimada_ms), " s para ",
    proyeccion$n_columnas, " columna(s). Fuente: ", proyeccion$fuente,
    ". Es una estimacion, no una medicion.", detalle_fuente, detalle
  ))
  invisible(TRUE)
}

.cardinalidad_aviso_moda_dbi <- function(
    columna, agregados, fuentes = NULL, n_total = NA_real_) {
  fuente <- if (is.null(fuentes)) NULL else fuentes[[columna]]
  valor_fuente <- if (is.null(fuente)) NA_real_ else {
    .numero_dbi(fuente$n_distintos)
  }
  total <- .numero_dbi(n_total)
  if (length(valor_fuente) != 1L || is.na(valor_fuente) ||
      !is.finite(valor_fuente) || valor_fuente < 0) {
    proporcion <- if (is.null(fuente)) NA_real_ else
      .numero_dbi(fuente$proporcion_distintos)
    if (!is.null(fuente) && isTRUE(fuente$exacta) &&
        is.finite(proporcion) && proporcion == 1 &&
        length(total) == 1L && is.finite(total) && !is.na(total)) {
      valor_fuente <- total
    }
  }
  if (length(valor_fuente) == 1L && is.finite(valor_fuente) &&
      !is.na(valor_fuente) && valor_fuente >= 0) {
    return(list(
      disponible = TRUE, n_distintos = valor_fuente,
      fuente = if (is.null(fuente) || is.null(fuente$nombre)) {
        "fuente estructural"
      } else fuente$nombre,
      motivo = if (is.null(fuente) || is.null(fuente$motivo)) {
        NA_character_
      } else fuente$motivo
    ))
  }
  conteo <- if (!is.null(agregados) && !is.null(agregados$conteos)) {
    agregados$conteos[[columna]]
  } else NULL
  valor <- if (!is.null(conteo) && !is.null(conteo$distintos)) {
    .numero_dbi(conteo$distintos$valor)
  } else NA_real_
  if (length(valor) == 1L && is.finite(valor) && !is.na(valor) && valor >= 0) {
    return(list(
      disponible = TRUE, n_distintos = valor, fuente = "medicion de la corrida",
      motivo = "La cardinalidad sale del agregado disponible antes de la moda."
    ))
  }
  valor <- if (is.null(fuente)) NA_real_ else .numero_dbi(fuente$n_distintos)
  if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor < 0) {
    proporcion <- if (is.null(fuente)) NA_real_ else
      .numero_dbi(fuente$proporcion_distintos)
    if (!is.null(fuente) && isTRUE(fuente$exacta) &&
        is.finite(proporcion) && proporcion == 1 &&
        length(total) == 1L && is.finite(total) && !is.na(total)) {
      valor <- total
    }
  }
  if (length(valor) == 1L && is.finite(valor) && !is.na(valor) && valor >= 0) {
    return(list(
      disponible = TRUE, n_distintos = valor,
      fuente = if (is.null(fuente$nombre)) "fuente estructural" else fuente$nombre,
      motivo = if (is.null(fuente$motivo)) NA_character_ else fuente$motivo
    ))
  }
  motivo <- if (is.null(fuente) || is.null(fuente$motivo)) {
    "No hay cardinalidad medida, estructural ni de catalogo para esta columna."
  } else {
    as.character(fuente$motivo)
  }
  list(
    disponible = FALSE, n_distintos = NA_real_,
    fuente = if (is.null(fuente) || is.null(fuente$nombre)) {
      "desconocida"
    } else fuente$nombre,
    motivo = paste(
      "No se puede proyectar el costo de la moda porque falta la cardinalidad;",
      motivo
    )
  )
}

.registrar_referencia_mediana_dbi <- function(
    presupuesto, consulta, n_filas, n_medianas = 1L) {
  if (is.null(presupuesto) || !isTRUE(consulta$ok) ||
      is.null(consulta$consulta_id) || length(consulta$consulta_id) != 1L ||
      is.na(consulta$consulta_id) || is.null(consulta$duracion_ms) ||
      length(consulta$duracion_ms) != 1L || is.na(consulta$duracion_ms) ||
      !is.finite(consulta$duracion_ms) || consulta$duracion_ms <= 0) {
    return(invisible(NULL))
  }
  filas <- .numero_dbi(n_filas)
  medianas <- .numero_dbi(n_medianas)
  if (length(filas) != 1L || is.na(filas) || !is.finite(filas) || filas <= 0 ||
      length(medianas) != 1L || is.na(medianas) || !is.finite(medianas) ||
      medianas <= 0) {
    return(invisible(NULL))
  }
  id <- as.character(consulta$consulta_id)
  referencias <- presupuesto$referencias_mediana
  referencias[[id]] <- list(
    consulta_id = as.integer(consulta$consulta_id),
    n_filas = filas, n_medianas = as.integer(medianas),
    duracion_ms = as.numeric(consulta$duracion_ms),
    ms_por_fila = as.numeric(consulta$duracion_ms) / (filas * medianas)
  )
  presupuesto$referencias_mediana <- referencias
  invisible(NULL)
}

.proyeccion_mediana_vacia_dbi <- function(
    motivo = paste(
      "No hay una duracion medida de mediana en esta corrida y no se",
      "publica una proyeccion temporal."
    )) {
  list(
    disponible = FALSE, duracion_estimada_ms = NA_real_,
    duracion_referencia_ms = NA_real_, ms_por_fila = NA_real_,
    n_filas = NA_real_, n_medianas = 0L, n_referencias = 0L,
    fuente = NA_character_, referencia_declarada = FALSE,
    cota_lectura_ms = NA_real_, motivo = motivo
  )
}

.proyectar_costo_mediana_dbi <- function(
    presupuesto, n_filas, n_medianas, usar_referencia_banco = FALSE) {
  filas <- .numero_dbi(n_filas)
  medianas <- .numero_dbi(n_medianas)
  vacia <- .proyeccion_mediana_vacia_dbi()
  if (length(filas) != 1L || is.na(filas) || !is.finite(filas) || filas <= 0 ||
      length(medianas) != 1L || is.na(medianas) || !is.finite(medianas) ||
      medianas < 1) {
    vacia$motivo <- "No hay un numero utilizable de filas o medianas pendientes; no se puede proyectar la mediana."
    return(vacia)
  }
  referencias <- if (is.null(presupuesto)) list() else presupuesto$referencias_mediana
  tasas <- if (length(referencias)) vapply(referencias, function(x) {
    valor <- .numero_dbi(x$ms_por_fila)
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor <= 0) {
      duracion <- .numero_dbi(x$duracion_ms)
      filas_referencia <- .numero_dbi(x$n_filas)
      medianas_referencia <- .numero_dbi(x$n_medianas)
      if (length(duracion) == 1L && is.finite(duracion) &&
          length(filas_referencia) == 1L && is.finite(filas_referencia) &&
          filas_referencia > 0 && length(medianas_referencia) == 1L &&
          is.finite(medianas_referencia) && medianas_referencia > 0) {
        valor <- duracion / (filas_referencia * medianas_referencia)
      }
    }
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) || valor <= 0) {
      NA_real_
    } else valor
  }, numeric(1L)) else numeric()
  tasas <- tasas[is.finite(tasas) & tasas > 0]
  declarada <- FALSE
  if (length(tasas)) {
    referencia <- stats::median(tasas)
    fuente <- paste(
      "mediana de", length(tasas),
      "consulta(s) de mediana medida(s) en esta corrida (ms por fila)"
    )
    motivo <- paste(
      "La proyeccion usa la mediana de las tasas ms por fila de las",
      "medianas medidas en esta corrida y la multiplica por las filas y",
      "medianas pendientes."
    )
  } else if (isTRUE(usar_referencia_banco)) {
    referencia <- .REFERENCIA_BANCO_MEDIANA_MS_POR_MILLON_FILAS_DBI / 1e6
    fuente <- paste(
      "referencia de banco de otra corrida:",
      .REFERENCIA_BANCO_MEDIANA_MS_POR_MILLON_FILAS_DBI,
      "ms por millon de filas"
    )
    motivo <- paste(
      "No hubo una primera medicion local de mediana en esta corrida; se",
      "usa la referencia de banco declarada de otra corrida, no como una",
      "medicion local."
    )
    declarada <- TRUE
  } else {
    vacia$motivo <- paste(
      "Todavia no hay una primera medicion local de mediana para convertir",
      "las filas en tiempo; la referencia de banco solo se usa cuando no hay",
      "una mediana total local que pueda proyectarse."
    )
    return(vacia)
  }
  estimada <- referencia * filas * medianas
  cota_lectura <- NA_real_
  if (isTRUE(declarada) && !is.null(presupuesto)) {
    lectura <- .numero_dbi(presupuesto$duracion_lectura_mediana)
    if (length(lectura) == 1L && is.finite(lectura) && lectura > 0) {
      cota_lectura <- lectura * medianas
      if (is.finite(cota_lectura) && cota_lectura > estimada) {
        estimada <- cota_lectura
        fuente <- paste(
          fuente,
          paste0(
            "cota de lectura observada en la consulta inicial (",
            .segundos_dbi(lectura), " s)"
          ),
          sep = "; "
        )
        motivo <- paste(
          motivo,
          "La proyeccion no baja de la cota observada de lectura de la consulta que obtuvo las filas; esa cota no es una medicion de mediana.",
          sep = " "
        )
      }
    }
  }
  if (!is.finite(estimada)) {
    vacia$motivo <- "La proyeccion de la mediana no es finita; no se publica un numero."
    return(vacia)
  }
  list(
    disponible = TRUE, duracion_estimada_ms = estimada,
    duracion_referencia_ms = referencia * filas * medianas,
    ms_por_fila = referencia, n_filas = filas,
    n_medianas = as.integer(medianas), n_referencias = as.integer(length(tasas)),
    fuente = fuente, referencia_declarada = declarada,
    cota_lectura_ms = cota_lectura, motivo = motivo
  )
}

.avisar_costo_mediana_dbi <- function(
    proyeccion, habilitado = TRUE,
    umbral_segundos = .UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI) {
  habilitado <- .validar_interruptor_aviso_dbi(habilitado, "habilitado")
  umbral_segundos <- .validar_umbral_aviso_dbi(
    umbral_segundos, "umbral_segundos"
  )
  if (is.null(proyeccion) || !isTRUE(proyeccion$disponible) ||
      !isTRUE(habilitado) || !is.finite(umbral_segundos) ||
      is.na(proyeccion$duracion_estimada_ms) ||
      proyeccion$duracion_estimada_ms < umbral_segundos * 1000) {
    return(invisible(FALSE))
  }
  cli::cli_alert_warning(paste0(
    "Costo estimado de la mediana: ~",
    .segundos_dbi(proyeccion$duracion_estimada_ms), " s para ",
    proyeccion$n_medianas, " mediana(s) sobre ",
    .entero_sql_dbi(proyeccion$n_filas), " filas. Fuente: ",
    proyeccion$fuente, ". ", proyeccion$motivo
  ))
  invisible(TRUE)
}

.segundos_dbi <- function(milisegundos) {
  if (is.null(milisegundos) || length(milisegundos) != 1L ||
      is.na(milisegundos) || !is.finite(milisegundos)) return("sin dato")
  formatC(milisegundos / 1000, format = "f", digits = 1,
          decimal.mark = ",")
}

.UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI <- 30
.UMBRAL_SEGUNDOS_AVISO_MODA_DBI <- 30
.UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI <- 30
.REFERENCIA_BANCO_MEDIANA_MS_POR_MILLON_FILAS_DBI <- 68
.TASA_NEWID_MS_POR_MILLON_DBI <- 650
.MAX_FILAS_NEWID_DBI <- 100000
.MIN_FRACCION_NEWID_DBI <- 0.20
.MAX_PROYECCION_NEWID_MS_DBI <- 1000
.UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI <- 0
# Nombre interno histórico, expresado en milisegundos como lo estaba antes de
# que el umbral público pasara a declarar su unidad en segundos.
.UMBRAL_AVISO_DISTINTOS_DBI <- .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI * 1000

.evaluar_guardia_newid_dbi <- function(n_total, tamano_muestra,
                                       medianas_pendientes = 1L) {
  total <- .numero_dbi(n_total)
  muestra <- .numero_dbi(tamano_muestra)
  pendientes <- .numero_dbi(medianas_pendientes)
  fraccion <- if (length(total) == 1L && is.finite(total) && total > 0 &&
                  length(muestra) == 1L && is.finite(muestra)) {
    muestra / total
  } else {
    NA_real_
  }
  proyeccion <- if (length(total) == 1L && is.finite(total) && total >= 0 &&
                    length(pendientes) == 1L && is.finite(pendientes) &&
                    pendientes >= 1) {
    .TASA_NEWID_MS_POR_MILLON_DBI / 1e6 * total * pendientes
  } else {
    NA_real_
  }
  condiciones <- c(
    total_valido = length(total) == 1L && is.finite(total) && total >= 0,
    limite_filas = length(total) == 1L && is.finite(total) &&
      total <= .MAX_FILAS_NEWID_DBI,
    fraccion_valida = length(fraccion) == 1L && is.finite(fraccion),
    fraccion_minima = length(fraccion) == 1L && is.finite(fraccion) &&
      fraccion >= .MIN_FRACCION_NEWID_DBI,
    proyeccion_valida = length(proyeccion) == 1L && is.finite(proyeccion),
    proyeccion_maxima = length(proyeccion) == 1L && is.finite(proyeccion) &&
      proyeccion <= .MAX_PROYECCION_NEWID_MS_DBI
  )
  aceptado <- all(condiciones)
  list(
    aceptado = aceptado,
    motivo = if (aceptado) {
      "sesgo_muestreo:random_limit_newid_por_fila"
    } else {
      "capacidad_no_aceptada:newid_costo_excede_presupuesto"
    },
    n_total = total,
    tamano_muestra = muestra,
    fraccion = fraccion,
    medianas_pendientes = as.integer(pendientes),
    proyeccion_newid_ms = proyeccion,
    tasa_ms_por_millon = .TASA_NEWID_MS_POR_MILLON_DBI,
    umbral_n_total = .MAX_FILAS_NEWID_DBI,
    umbral_fraccion = .MIN_FRACCION_NEWID_DBI,
    umbral_proyeccion_ms = .MAX_PROYECCION_NEWID_MS_DBI,
    condiciones = condiciones,
    razon = if (aceptado) {
      "NEWID fue aceptado dentro de los tres limites de la politica de costo."
    } else {
      paste(
        "NEWID no se ejecuta: la proyeccion conservadora o el tamano y la",
        "fraccion de la muestra quedan fuera de la politica."
      )
    }
  )
}

.validar_interruptor_aviso_dbi <- function(valor, nombre) {
  if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      paste0("`", nombre, "` debe ser TRUE o FALSE.")
    )
  }
  valor
}

.validar_umbral_aviso_dbi <- function(valor, nombre) {
  if (!is.numeric(valor) || length(valor) != 1L || is.na(valor) || valor < 0 ||
      (!is.infinite(valor) && !is.finite(valor))) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      paste0("`", nombre, "` debe ser un numero no negativo o Inf.")
    )
  }
  as.numeric(valor)
}

.avisar_costo_distintos_dbi <- function(
    proyeccion, habilitado = TRUE,
    umbral_segundos = .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI) {
  habilitado <- .validar_interruptor_aviso_dbi(habilitado, "habilitado")
  umbral_segundos <- .validar_umbral_aviso_dbi(
    umbral_segundos, "umbral_segundos"
  )
  if (is.null(proyeccion) || !isTRUE(proyeccion$disponible) ||
      !isTRUE(habilitado) || !is.finite(umbral_segundos) ||
      is.na(proyeccion$duracion_estimada_ms) ||
      proyeccion$duracion_estimada_ms < umbral_segundos * 1000) {
    return(invisible(NULL))
  }
  cli::cli_alert_warning(paste0(
    "Costo estimado de `COUNT(DISTINCT)`: ~",
    .segundos_dbi(proyeccion$duracion_estimada_ms), " s para ",
    proyeccion$n_lotes, " lote(s). Fuente: ", proyeccion$fuente,
    ". Es una estimacion, no una medicion; el derrame real se informa",
    " despues si la instrumentacion del servidor lo permite."
  ))
  invisible(NULL)
}

# PostgreSQL cuenta la memoria de las operaciones hash en unidades de 1024.
# `avg_width` esta expresado en bytes, asi que la cifra que sigue es una
# aproximacion deliberadamente simple: el ancho medio de la clave mas un
# margen fijo para la entrada y el enlace de la tabla hash. No pretende
# reproducir el planificador ni afirmar que hubo derrame.
.TAMANO_BASE_ENTRADA_HASH_POSTGRESQL_DBI <- 64

# La huella de un `SortTuple` no desaparece cuando el valor es corto. Estos
# pisos gobiernan solo la decision de memoria; el tamano del tape queda como
# una magnitud informativa aparte.
.TAMANO_BASE_SORT_POSTGRESQL_DBI <- 24
.PISO_SORT_FIJOS_POSTGRESQL_DBI <- 32
.PISO_SORT_NUMERIC_POSTGRESQL_DBI <- 42

.memoria_dbi <- function(bytes) {
  if (is.null(bytes) || length(bytes) != 1L || is.na(bytes) ||
      !is.finite(bytes) || bytes < 0) return("sin dato")
  unidades <- c("B", "KB", "MB", "GB", "TB")
  indice <- if (bytes == 0) 1L else min(
    floor(log(bytes, base = 1024)) + 1L, length(unidades)
  )
  valor <- bytes / 1024^(indice - 1L)
  paste0(formatC(valor, format = "f", digits = 1L, decimal.mark = ","),
         " ", unidades[[indice]])
}

.parsear_memoria_postgresql_dbi <- function(valor) {
  if (is.null(valor) || length(valor) != 1L || is.na(valor)) {
    return(NA_real_)
  }
  texto <- tolower(trimws(as.character(valor)))
  partes <- regexec(
    "^([0-9]+(?:\\.[0-9]+)?)\\s*(b|kb|kib|mb|mib|gb|gib|tb|tib)?$",
    texto, perl = TRUE
  )[[1L]]
  if (identical(partes[[1L]], -1L)) return(NA_real_)
  capturas <- regmatches(texto, list(partes))[[1L]]
  numero <- suppressWarnings(as.numeric(capturas[[2L]]))
  unidad <- if (length(capturas) < 3L || is.na(capturas[[3L]])) {
    ""
  } else capturas[[3L]]
  multiplicador <- c(
    b = 1, kb = 1024, kib = 1024, mb = 1024^2, mib = 1024^2,
    gb = 1024^3, gib = 1024^3, tb = 1024^4, tib = 1024^4
  )
  if (!length(unidad) || is.na(unidad) || !nzchar(unidad)) unidad <- "b"
  resultado <- numero * unname(multiplicador[[unidad]])
  if (!is.finite(resultado) || resultado < 0) NA_real_ else resultado
}

.estimacion_derrame_vacia_postgresql_dbi <- function(
    estado = "no_disponible", motivo = "No se pudo estimar el derrame.",
    memoria = NULL) {
  if (is.null(memoria)) memoria <- list()
  list(
    estado = estado, disponible = FALSE, es_estimacion = TRUE,
    fuente = NA_character_, motivo = motivo,
    work_mem = if (is.null(memoria$work_mem)) NA_character_ else memoria$work_mem,
    work_mem_bytes = if (is.null(memoria$work_mem_bytes)) NA_real_ else {
      memoria$work_mem_bytes
    },
    hash_mem_multiplier = if (is.null(memoria$hash_mem_multiplier)) NA_real_ else {
      memoria$hash_mem_multiplier
    },
    hash_mem_multiplier_disponible = isTRUE(memoria$hash_mem_multiplier_disponible),
    memoria_efectiva_bytes = if (is.null(memoria$memoria_efectiva_bytes)) {
      NA_real_
    } else memoria$memoria_efectiva_bytes,
    memoria_efectiva = if (is.null(memoria$memoria_efectiva)) {
      NA_character_
    } else memoria$memoria_efectiva,
    columnas = data.frame(
      columna = character(), n_distintos_estimados = numeric(),
      avg_width = numeric(), tamano_estimado_bytes = numeric(),
      n_relaciones = integer(), stringsAsFactors = FALSE
    ),
    columnas_no_estimadas = data.frame(
      columna = character(), motivo = character(), stringsAsFactors = FALSE
    ),
    lotes = data.frame(
      lote = integer(), columnas = character(),
      n_distintos_estimados = numeric(), tamano_estimado_bytes = numeric(),
      supera_memoria = logical(), stringsAsFactors = FALSE
    ),
    lotes_sobre_memoria = integer(),
    filas_catalogo = .denominador_catalogo_vacio_dbi(motivo)
  )
}

.denominador_catalogo_vacio_dbi <- function(
    motivo = "No hay una estimacion utilizable de `pg_class.reltuples`.") {
  list(
    disponible = FALSE, estado = "no_disponible", filas = NA_real_,
    fuente = NA_character_, n_relaciones = 0L, motivo = motivo
  )
}

# La consulta de estadisticas ya recorre la jerarquia y devuelve solo las hojas;
# aca se aplica el mismo denominador que usa `catalogo`, sin inventar otra
# consulta ni volver a sumar una relacion padre particionada.
.denominador_catalogo_dbi <- function(datos) {
  vacio <- .denominador_catalogo_vacio_dbi()
  if (!is.data.frame(datos) || !all(c(
    "relacion_oid", "reltuples", "hoja"
  ) %in% names(datos))) {
    vacio$motivo <- paste(
      "La respuesta del catalogo no trae la jerarquia y `reltuples` necesarios;",
      "se conserva `sin dato filas`."
    )
    return(vacio)
  }
  relaciones <- unique(datos[c("relacion_oid", "reltuples", "hoja")])
  hojas <- .logico_catalogo_dbi(relaciones$hoja)
  relaciones <- relaciones[!is.na(relaciones$relacion_oid) & hojas %in% TRUE,
                            , drop = FALSE]
  if (!nrow(relaciones)) {
    vacio$motivo <- paste(
      "El catalogo no devolvio relaciones hoja visibles; no hay una",
      "estimacion utilizable de `pg_class.reltuples`."
    )
    return(vacio)
  }
  reltuples <- suppressWarnings(as.numeric(relaciones$reltuples))
  if (any(!is.finite(reltuples) | reltuples <= 0)) {
    vacio$motivo <- paste(
      "`pg_class.reltuples` tiene al menos un valor cero, negativo o no",
      "utilizable; puede no haberse ejecutado ANALYZE. Se conserva `sin dato",
      "filas` y no se supone cero."
    )
    return(vacio)
  }
  filas <- sum(reltuples)
  if (!is.finite(filas) || filas <= 0) {
    vacio$motivo <- paste(
      "La suma de `pg_class.reltuples` no es utilizable; se conserva `sin",
      "dato filas` y no se supone cero."
    )
    return(vacio)
  }
  list(
    disponible = TRUE, estado = "estimado_catalogo", filas = filas,
    fuente = "pg_class.reltuples", n_relaciones = as.integer(nrow(relaciones)),
    motivo = paste(
      "Las filas se estiman como la suma de `pg_class.reltuples` de las",
      "relaciones hoja de la jerarquia ya leida. Es una estimacion de catalogo,",
      "no una medicion del universo de la tabla."
    )
  )
}

.leer_memoria_postgresql_dbi <- function(conexion, presupuesto) {
  if (!is.null(presupuesto) && !is.null(presupuesto$memoria_derrame)) {
    return(presupuesto$memoria_derrame)
  }
  work <- .escalar_dbi(
    conexion, "SHOW work_mem", "work_mem", presupuesto,
    etapa = "estimacion_derrame"
  )
  if (!isTRUE(work$ok)) {
    return(list(
      ok = FALSE, motivo = paste(
        "No se pudo estimar el derrame: no se pudo leer `SHOW work_mem`.",
        work$motivo
      )
    ))
  }
  work_texto <- as.character(work$valor[[1L]])
  work_bytes <- .parsear_memoria_postgresql_dbi(work_texto)
  if (!is.finite(work_bytes)) {
    return(list(
      ok = FALSE, motivo = paste0(
        "No se pudo estimar el derrame: `SHOW work_mem` devolvio un valor",
        " no interpretable (", work_texto, ")."
      )
    ))
  }

  # `hash_mem_multiplier` fue agregado en PostgreSQL 13. Se intenta leerlo y
  # se usa 1 cuando el servidor antiguo responde que el parametro no existe;
  # no se convierte ese error esperado en un fallo de la corrida.
  hash <- .escalar_dbi(
    conexion, "SHOW hash_mem_multiplier", "hash_mem_multiplier", presupuesto,
    etapa = "estimacion_derrame"
  )
  hash_disponible <- FALSE
  multiplicador <- 1
  motivo_hash <- character()
  if (isTRUE(hash$ok)) {
    multiplicador <- suppressWarnings(as.numeric(hash$valor[[1L]]))
    hash_disponible <- is.finite(multiplicador) && multiplicador > 0
    if (!hash_disponible) {
      multiplicador <- 1
      motivo_hash <- "`SHOW hash_mem_multiplier` devolvio un valor no utilizable."
    }
  } else {
    motivo_hash <- paste(
      "El servidor no expone `hash_mem_multiplier`; se usa 1, como en",
      "PostgreSQL anterior a 13."
    )
  }
  efectiva <- work_bytes * multiplicador
  list(
    ok = is.finite(efectiva) && efectiva >= 0,
    motivo = motivo_hash, work_mem = work_texto,
    work_mem_bytes = work_bytes, hash_mem_multiplier = multiplicador,
    hash_mem_multiplier_disponible = hash_disponible,
    memoria_efectiva_bytes = efectiva, memoria_efectiva = .memoria_dbi(efectiva)
  )
}

.estadisticas_hash_postgresql_dbi <- function(conexion, tabla, columnas,
                                              presupuesto) {
  if (!is.null(presupuesto) && !is.null(presupuesto$estadisticas_derrame)) {
    return(presupuesto$estadisticas_derrame)
  }
  piezas <- .piezas_tabla_cardinalidad_dbi(tabla)
  if (is.null(piezas$tabla) || length(piezas$tabla) != 1L ||
      is.na(piezas$tabla) || !nzchar(piezas$tabla)) {
    return(list(
      ok = FALSE, motivo = "No se pudo identificar la relacion para consultar `pg_stats`."
    ))
  }
  citar <- function(valor) as.character(DBI::dbQuoteString(conexion, valor))
  tabla_literal <- citar(piezas$tabla)
  esquema <- piezas$esquema
  filtro_esquema <- if (!is.null(esquema) && length(esquema) == 1L &&
                        !is.na(esquema) && nzchar(esquema)) {
    paste0("n.nspname = ", citar(esquema))
  } else {
    # `pg_table_is_visible()` existe en las versiones antiguas y evita tomar
    # una tabla homonima que no seria la que resolvio el search_path.
    "pg_catalog.pg_table_is_visible(c.oid)"
  }
  columnas_literal <- paste(vapply(columnas, citar, character(1L)), collapse = ", ")
  sql <- paste(
    "WITH RECURSIVE relaciones AS (",
    "SELECT c.oid, n.nspname AS schemaname, c.relname AS tablename,",
    "c.reltuples, TRUE AS es_raiz,",
    "NOT EXISTS (SELECT 1 FROM pg_catalog.pg_inherits h",
    "WHERE h.inhparent = c.oid) AS hoja",
    "FROM pg_catalog.pg_class c",
    "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace",
    "WHERE c.relname =", tabla_literal, "AND", filtro_esquema,
    "UNION ALL",
    "SELECT hija.oid, ns.nspname AS schemaname, hija.relname AS tablename,",
    "hija.reltuples, FALSE AS es_raiz,",
    "NOT EXISTS (SELECT 1 FROM pg_catalog.pg_inherits h2",
    "WHERE h2.inhparent = hija.oid) AS hoja",
    "FROM pg_catalog.pg_inherits herencia",
    "JOIN relaciones padre ON padre.oid = herencia.inhparent",
    "JOIN pg_catalog.pg_class hija ON hija.oid = herencia.inhrelid",
    "JOIN pg_catalog.pg_namespace ns ON ns.oid = hija.relnamespace",
    ")",
    "SELECT r.oid::text AS relacion_oid, r.schemaname, r.tablename,",
    "r.reltuples, r.es_raiz, r.hoja, s.attname, s.n_distinct, s.avg_width,",
    "s.null_frac, format_type(a.atttypid, a.atttypmod) AS tipo",
    "FROM relaciones r",
    "LEFT JOIN pg_catalog.pg_stats s ON",
    "s.schemaname = r.schemaname AND s.tablename = r.tablename",
    "AND s.attname IN (", columnas_literal, ")",
    "LEFT JOIN pg_catalog.pg_attribute a ON a.attrelid = r.oid",
    "AND a.attname = s.attname AND a.attnum > 0 AND NOT a.attisdropped",
    "WHERE r.hoja",
    "ORDER BY r.oid, s.attname"
  )
  consulta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "estimacion_derrame"
  )
  if (!isTRUE(consulta$ok)) {
    salida <- list(
      ok = FALSE, sql = sql, motivo = paste(
        "No se pudo estimar el derrame: no se pudo leer `pg_stats` y",
        "`pg_class`.", consulta$motivo
      )
    )
    if (!is.null(presupuesto)) presupuesto$estadisticas_derrame <- salida
    return(salida)
  }
  esperadas <- c(
    "relacion_oid", "schemaname", "tablename", "reltuples", "es_raiz",
    "hoja", "attname", "n_distinct", "avg_width"
  )
  if (!all(esperadas %in% names(consulta$datos))) {
    salida <- list(
      ok = FALSE, sql = sql, motivo = paste(
        "No se pudo estimar el derrame: la respuesta de `pg_stats` no",
        "contiene las columnas esperadas."
      )
    )
    if (!is.null(presupuesto)) presupuesto$estadisticas_derrame <- salida
    return(salida)
  }
  salida <- list(
    ok = TRUE, sql = sql, datos = consulta$datos,
    filas_catalogo = .denominador_catalogo_dbi(consulta$datos)
  )
  if (!is.null(presupuesto)) presupuesto$estadisticas_derrame <- salida
  salida
}

.estimar_derrame_postgresql_dbi <- function(conexion, tabla, columnas,
                                            presupuesto,
                                            exacto = TRUE,
                                            universo = "tabla_completa",
                                            tamano_lote = .TAMANO_LOTE_DISTINTOS_DBI,
                                            columnas_stats = NULL) {
  if (!isTRUE(exacto)) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      "no_solicitado", paste(
        "No se solicito una estimacion de derrame porque la estrategia de",
        "distintos no emite `COUNT(DISTINCT)` exacto."
      )
    ))
  }
  if (identical(universo, "muestra_motor")) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      motivo = paste(
        "No se pudo estimar el derrame de una muestra con las estadisticas",
        "de la tabla completa; no se inventa una equivalencia."
      )
    ))
  }
  if (!length(columnas)) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      "no_solicitado", paste(
        "No se solicito una estimacion de derrame porque no hay columnas",
        "que vayan a pasar por `COUNT(DISTINCT)`."
      )
    ))
  }
  if (!grepl("postgres|pqconnection", .senas_conexion_dbi(conexion),
             ignore.case = TRUE, perl = TRUE)) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      motivo = paste(
        "No se pudo estimar el derrame: la conexion no fue reconocida como",
        "PostgreSQL y no hay un `pg_stats` portable."
      )
    ))
  }
  memoria <- .leer_memoria_postgresql_dbi(conexion, presupuesto)
  if (!isTRUE(memoria$ok)) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      motivo = memoria$motivo, memoria = memoria
    ))
  }
  if (!is.null(presupuesto)) presupuesto$memoria_derrame <- memoria
  columnas_stats <- if (is.null(columnas_stats)) columnas else columnas_stats
  estadisticas <- .estadisticas_hash_postgresql_dbi(
    conexion, tabla, columnas_stats, presupuesto
  )
  if (!isTRUE(estadisticas$ok)) {
    return(.estimacion_derrame_vacia_postgresql_dbi(
      motivo = estadisticas$motivo, memoria = memoria
    ))
  }
  datos <- estadisticas$datos
  filas_catalogo <- estadisticas$filas_catalogo
  relaciones <- unique(datos[c(
    "relacion_oid", "schemaname", "tablename", "reltuples", "es_raiz", "hoja"
  )])
  relaciones <- relaciones[!is.na(relaciones$relacion_oid) &
                             relaciones$hoja %in% TRUE, , drop = FALSE]
  nombres_stats <- as.character(datos$attname)
  columnas_estimadas <- list()
  columnas_no_estimadas <- list()
  for (columna in columnas) {
    filas <- datos[!is.na(datos$attname) & nombres_stats == columna, , drop = FALSE]
    ids_relaciones <- as.character(relaciones$relacion_oid)
    ids_stats <- if (nrow(filas)) unique(as.character(filas$relacion_oid)) else {
      character()
    }
    faltantes <- setdiff(ids_relaciones, ids_stats)
    motivo <- NULL
    if (!nrow(relaciones)) {
      motivo <- paste(
        "`pg_stats` no devolvio ninguna relacion visible; la tabla puede no",
        "haber sido `ANALYZE` o la credencial puede no tener privilegios sobre",
        "sus columnas."
      )
    } else if (!length(filas)) {
      motivo <- paste(
        "`pg_stats` no devolvio una fila para esta columna; puede no haber",
        "`ANALYZE` o la credencial puede no tener privilegio sobre la columna."
      )
    } else if (length(faltantes)) {
      motivo <- paste(
        "No hay una estadistica visible para todas las relaciones que se",
        "leen; faltan particiones o columnas sin `ANALYZE`/permiso."
      )
    }
    if (!is.null(motivo)) {
      columnas_no_estimadas[[length(columnas_no_estimadas) + 1L]] <- data.frame(
        columna = columna, motivo = motivo, stringsAsFactors = FALSE
      )
      next
    }
    filas <- filas[!duplicated(as.character(filas$relacion_oid)), , drop = FALSE]
    n_distintos <- numeric(nrow(filas))
    tamanos <- numeric(nrow(filas))
    anchos <- numeric(nrow(filas))
    invalidos <- character()
    for (i in seq_len(nrow(filas))) {
      nd <- suppressWarnings(as.numeric(filas$n_distinct[[i]]))
      ancho <- suppressWarnings(as.numeric(filas$avg_width[[i]]))
      reltuples <- suppressWarnings(as.numeric(filas$reltuples[[i]]))
      if (!is.finite(nd) || !is.finite(ancho) || ancho < 0) {
        invalidos <- c(invalidos, "`n_distinct` o `avg_width` no utilizable")
        next
      }
      if (nd < 0) {
        # Un `n_distinct` negativo es una proporcion del universo. `-1` es
        # unico por fila; `reltuples = -1` significa desconocido, no cero.
        if (!is.finite(reltuples) || reltuples < 0) {
          invalidos <- c(
            invalidos,
            "`n_distinct` negativo requiere `pg_class.reltuples`; `reltuples = -1` es desconocido"
          )
          next
        }
        nd <- ceiling(abs(nd) * reltuples)
      } else {
        nd <- ceiling(nd)
      }
      n_distintos[[i]] <- nd
      anchos[[i]] <- ancho
      tamanos[[i]] <- nd * (ancho + .TAMANO_BASE_ENTRADA_HASH_POSTGRESQL_DBI)
    }
    if (length(invalidos) || any(!is.finite(n_distintos)) ||
        any(!is.finite(tamanos))) {
      columnas_no_estimadas[[length(columnas_no_estimadas) + 1L]] <- data.frame(
        columna = columna,
        motivo = paste(unique(invalidos), collapse = "; "),
        stringsAsFactors = FALSE
      )
      next
    }
    total_distintos <- sum(n_distintos)
    columnas_estimadas[[length(columnas_estimadas) + 1L]] <- data.frame(
      columna = columna,
      n_distintos_estimados = total_distintos,
      avg_width = if (total_distintos > 0) {
        sum(n_distintos * anchos) / total_distintos
      } else mean(anchos),
      tamano_estimado_bytes = sum(tamanos),
      n_relaciones = as.integer(nrow(filas)),
      stringsAsFactors = FALSE
    )
  }
  columnas_df <- if (length(columnas_estimadas)) {
    do.call(rbind, columnas_estimadas)
  } else {
    .estimacion_derrame_vacia_postgresql_dbi(memoria = memoria)$columnas
  }
  no_estimadas_df <- if (length(columnas_no_estimadas)) {
    do.call(rbind, columnas_no_estimadas)
  } else {
    .estimacion_derrame_vacia_postgresql_dbi(memoria = memoria)$columnas_no_estimadas
  }
  lotes_indices <- split(
    seq_along(columnas), ceiling(seq_along(columnas) / tamano_lote)
  )
  lotes <- lapply(seq_along(lotes_indices), function(numero) {
    indices <- lotes_indices[[numero]]
    nombres <- columnas[indices]
    conocidos <- nombres %in% columnas_df$columna
    tamano <- if (all(conocidos)) {
      sum(columnas_df$tamano_estimado_bytes[match(nombres, columnas_df$columna)])
    } else NA_real_
    cantidad <- if (all(conocidos)) {
      sum(columnas_df$n_distintos_estimados[match(nombres, columnas_df$columna)])
    } else NA_real_
    data.frame(
      lote = as.integer(numero), columnas = paste(nombres, collapse = ", "),
      n_distintos_estimados = cantidad, tamano_estimado_bytes = tamano,
      supera_memoria = if (is.finite(tamano)) {
        tamano > memoria$memoria_efectiva_bytes
      } else NA,
      stringsAsFactors = FALSE
    )
  })
  lotes_df <- do.call(rbind, lotes)
  sobre <- which(!is.na(lotes_df$supera_memoria) & lotes_df$supera_memoria)
  n_estimadas <- nrow(columnas_df)
  if (!n_estimadas) {
    estado <- "no_disponible"
    motivo <- paste(
      "No se pudo estimar el derrame: `pg_stats` no devolvio estadisticas",
      "utilizables para las columnas de `COUNT(DISTINCT)`."
    )
  } else if (n_estimadas < length(columnas)) {
    estado <- "parcial"
    motivo <- paste(
      "La estimacion de derrame es parcial: no se pudo estimar al menos una",
      "columna. No se afirma que el hash quede dentro o fuera del limite para",
      "los lotes incompletos."
    )
  } else {
    estado <- "estimado"
    motivo <- paste(
      "El tama\u00f1o del hash se estima con `pg_stats.n_distinct`,",
      "`pg_stats.avg_width` y `pg_class.reltuples`. `n_distinct` viene de una",
      "muestra y puede quedar corto: esto es una estimacion, no una medicion.",
      "Que el tama\u00f1o estimado quede dentro del limite no demuestra que no haya",
      "derrame; si se mide despues, manda `pg_stat_statements`."
    )
  }
  if (length(no_estimadas_df)) {
    motivo <- paste(
      motivo, paste(no_estimadas_df$columna, no_estimadas_df$motivo,
                    sep = ": ", collapse = " ")
    )
  }
  fuente <- paste(
    "pg_stats.n_distinct + pg_stats.avg_width + pg_class.reltuples + SHOW work_mem"
  )
  if (isTRUE(memoria$hash_mem_multiplier_disponible)) {
    fuente <- paste0(fuente, " + SHOW hash_mem_multiplier")
  }
  list(
    estado = estado, disponible = n_estimadas > 0L, es_estimacion = TRUE,
    fuente = fuente, motivo = motivo,
    work_mem = memoria$work_mem, work_mem_bytes = memoria$work_mem_bytes,
    hash_mem_multiplier = memoria$hash_mem_multiplier,
    hash_mem_multiplier_disponible = memoria$hash_mem_multiplier_disponible,
    memoria_efectiva_bytes = memoria$memoria_efectiva_bytes,
    memoria_efectiva = memoria$memoria_efectiva,
    columnas = columnas_df, columnas_no_estimadas = no_estimadas_df,
    lotes = lotes_df, lotes_sobre_memoria = as.integer(sobre),
    n_columnas_solicitadas = as.integer(length(columnas)),
    n_columnas_estimadas = as.integer(n_estimadas),
    filas_catalogo = filas_catalogo,
    supera_memoria = if (length(sobre)) TRUE else if (all(
      !is.na(lotes_df$supera_memoria)
    )) FALSE else NA
  )
}

.limite_decision_derrame_dbi <- function(estimacion, familia = NULL) {
  if (is.null(estimacion)) return("no resoluble: falta la estimacion")
  if (identical(estimacion$metodo, "por_columna")) {
    return("no resoluble: las columnas usan metodos de memoria distintos")
  }
  valor <- if (identical(estimacion$metodo, "sort") ||
               identical(familia, "mediana")) {
    estimacion$work_mem
  } else estimacion$memoria_efectiva
  if (is.null(valor) || length(valor) != 1L || is.na(valor) ||
      !nzchar(as.character(valor))) {
    return("no resoluble: el limite del motor no estuvo disponible")
  }
  as.character(valor)
}

.denominador_derrame_dbi <- function(estimacion) {
  fuente <- estimacion$fuente_denominador
  if (is.null(fuente) || length(fuente) != 1L || is.na(fuente) ||
      !nzchar(fuente)) {
    fuente <- "estimacion de catalogo"
  }
  as.character(fuente)
}

.avisar_derrame_estimado_postgresql_dbi <- function(
    estimacion, habilitado = TRUE,
    umbral_bytes = .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI,
    familia = "COUNT(DISTINCT)") {
  habilitado <- .validar_interruptor_aviso_dbi(habilitado, "habilitado")
  umbral_bytes <- .validar_umbral_aviso_dbi(umbral_bytes, "umbral_bytes")
  if (is.null(estimacion)) return(invisible(FALSE))
  if (identical(familia, "COUNT(DISTINCT)") &&
      identical(estimacion$familia, "moda")) familia <- "la moda"
  if (identical(familia, "COUNT(DISTINCT)") &&
      identical(estimacion$familia, "mediana")) familia <- "la mediana"
  if (is.null(estimacion) || !isTRUE(estimacion$es_estimacion) ||
      !isTRUE(habilitado) || !is.finite(umbral_bytes) ||
      !length(estimacion$lotes_sobre_memoria)) return(invisible(FALSE))
  lotes <- estimacion$lotes
  indices <- estimacion$lotes_sobre_memoria
  indices <- indices[is.finite(indices) & indices >= 1L &
    indices <= nrow(lotes)]
  indices <- indices[vapply(
    indices, function(i) is.finite(lotes$tamano_estimado_bytes[[i]]) &&
      lotes$tamano_estimado_bytes[[i]] >= umbral_bytes, logical(1L)
  )]
  if (!length(indices)) return(invisible(FALSE))
  detalle <- paste(vapply(indices, function(i) paste0(
    "lote de ", lotes$columnas[[i]], ": ~",
    .memoria_dbi(lotes$tamano_estimado_bytes[[i]])
  ), character(1L)), collapse = "; ")
  limite <- .limite_decision_derrame_dbi(estimacion, estimacion$familia)
  work_mem <- estimacion$work_mem
  if (is.null(work_mem) || length(work_mem) != 1L || is.na(work_mem) ||
      !nzchar(as.character(work_mem))) work_mem <- "no disponible"
  etiqueta <- if (identical(familia, "COUNT(DISTINCT)")) "" else {
    paste0(" para ", familia)
  }
  cli::cli_alert_warning(paste0(
    "Derrame potencial estimado", etiqueta,
    " (es una estimacion, no una medicion): ",
    detalle, " supera el `work_mem` vigente de ", work_mem,
    ". Metodo: ", estimacion$metodo, ". Limite de decision: ", limite,
    ". Denominador: con ",
    .denominador_derrame_dbi(estimacion), ". ",
    "Subir `work_mem` en esta sesion por encima de ese tama\u00f1o puede evitar el",
    " derrame; lupa no modifica la configuracion. El derrame real, si ocurre,",
    " se informa despues mediante `pg_stat_statements`."
  ))
  invisible(TRUE)
}

# ---- Estimaciones de moda y mediana -------------------------------------

.estimacion_derrame_familia_vacia_dbi <- function(
    familia, motivo = paste("No se pudo estimar el derrame de", familia, "."),
    estado = "no_disponible", memoria = NULL, forma = NA_character_) {
  if (is.null(memoria)) memoria <- list()
  work_bytes <- if (is.null(memoria$work_mem_bytes)) NA_real_ else
    memoria$work_mem_bytes
  list(
    estado = estado, disponible = FALSE, es_estimacion = TRUE,
    familia = familia, metodo = NA_character_, forma = forma,
    fuente = NA_character_, motivo = motivo,
    work_mem = if (is.null(memoria$work_mem)) NA_character_ else memoria$work_mem,
    work_mem_bytes = work_bytes,
    hash_mem_multiplier = if (is.null(memoria$hash_mem_multiplier)) NA_real_ else
      memoria$hash_mem_multiplier,
    hash_mem_multiplier_disponible = isTRUE(memoria$hash_mem_multiplier_disponible),
    memoria_efectiva_bytes = if (is.null(memoria$memoria_efectiva_bytes)) {
      work_bytes
    } else memoria$memoria_efectiva_bytes,
    memoria_efectiva = if (is.null(memoria$memoria_efectiva)) {
      .memoria_dbi(work_bytes)
    } else memoria$memoria_efectiva,
    columnas = data.frame(
      columna = character(), metodo = character(), forma = character(),
      tipo_familia = character(), n_distintos_estimados = numeric(),
      n_validos_catalogo = numeric(), n_validos_medido = numeric(),
      avg_width = numeric(), estado_hash_bytes = numeric(),
      estado_sort_bytes = numeric(), estado_memoria_bytes = numeric(),
      estado_io_total_bytes = numeric(), tamano_estimado_bytes = numeric(),
      supera_memoria = logical(), stringsAsFactors = FALSE
    ),
    columnas_no_estimadas = data.frame(
      columna = character(), motivo = character(), stringsAsFactors = FALSE
    ),
    lotes = data.frame(
      lote = integer(), columnas = character(), metodo = character(),
      forma = character(), estado_hash_bytes = numeric(),
      estado_sort_bytes = numeric(), estado_memoria_bytes = numeric(),
      estado_io_total_bytes = numeric(), tamano_estimado_bytes = numeric(),
      supera_memoria = logical(), stringsAsFactors = FALSE
    ),
    lotes_sobre_memoria = integer(), filas_catalogo =
      .denominador_catalogo_vacio_dbi(motivo),
    n_columnas_solicitadas = 0L, n_columnas_estimadas = 0L,
    supera_memoria = NA
  )
}

.familia_tipo_derrame_dbi <- function(tipo, prototipo = NULL) {
  texto <- if (length(tipo) && !is.na(tipo[[1L]])) {
    as.character(tipo[[1L]])
  } else ""
  if (!nzchar(texto) && !is.null(prototipo) && length(prototipo)) {
    if (inherits(prototipo, c("numeric", "integer64"))) texto <- "numeric"
  }
  if (grepl("numeric|decimal|number", tolower(texto), perl = TRUE)) {
    "numeric"
  } else {
    "fijos"
  }
}

.plan_moda_derrame_dbi <- function(conexion, sql, presupuesto) {
  explicacion <- .consultar_dbi(
    conexion, paste0("EXPLAIN (FORMAT JSON, COSTS OFF) ", sql), presupuesto,
    etapa = "explain_moda"
  )
  if (!isTRUE(explicacion$ok) || is.null(explicacion$datos) ||
      !nrow(explicacion$datos)) {
    return(list(
      ok = FALSE, metodo = NA_character_, motivo = paste(
        "No se pudo derivar el metodo de la moda desde `EXPLAIN (FORMAT JSON,",
        "COSTS OFF)`.", if (is.null(explicacion$motivo)) "" else explicacion$motivo
      )
    ))
  }
  texto <- paste(unlist(explicacion$datos, use.names = FALSE), collapse = " ")
  # Primero se recorre el arbol JSON, desde el nodo raiz (normalmente
  # `Limit`) hasta el primer `Aggregate`. El regex de abajo queda como
  # compatibilidad para drivers que entregan el JSON como una columna opaca.
  arbol <- tryCatch(
    jsonlite::fromJSON(texto, simplifyVector = FALSE),
    error = function(e) NULL
  )
  buscar <- function(nodo) {
    if (!is.list(nodo)) return(NULL)
    tipo_nodo <- nodo[["Node Type"]]
    if (length(tipo_nodo) && identical(as.character(tipo_nodo[[1L]]), "Aggregate")) {
      estrategia <- nodo[["Strategy"]]
      if (length(estrategia)) return(as.character(estrategia[[1L]]))
    }
    planes <- nodo[["Plans"]]
    if (is.list(planes)) {
      for (hijo in planes) {
        encontrado <- buscar(hijo)
        if (!is.null(encontrado)) return(encontrado)
      }
    }
    for (nombre in setdiff(names(nodo), c("Plans", "Node Type", "Strategy"))) {
      encontrado <- buscar(nodo[[nombre]])
      if (!is.null(encontrado)) return(encontrado)
    }
    NULL
  }
  estrategia_arbol <- if (is.list(arbol)) buscar(arbol) else NULL
  if (identical(estrategia_arbol, "Hashed")) {
    return(list(ok = TRUE, metodo = "hash", motivo = NA_character_))
  }
  if (identical(estrategia_arbol, "Sorted")) {
    return(list(ok = TRUE, metodo = "sort", motivo = NA_character_))
  }
  estrategia <- regmatches(
    texto,
    regexpr('"Strategy"[[:space:]]*:[[:space:]]*"[A-Za-z]+"', texto,
            perl = TRUE)
  )
  if (length(estrategia) && nzchar(estrategia)) {
    if (grepl('"Hashed"', estrategia, fixed = TRUE)) {
      return(list(ok = TRUE, metodo = "hash", motivo = NA_character_))
    }
    if (grepl('"Sorted"', estrategia, fixed = TRUE)) {
      return(list(ok = TRUE, metodo = "sort", motivo = NA_character_))
    }
  }
  if (grepl('"Strategy"[[:space:]]*:[[:space:]]*"Hashed"', texto,
            perl = TRUE)) {
    return(list(ok = TRUE, metodo = "hash", motivo = NA_character_))
  }
  if (grepl('"Strategy"[[:space:]]*:[[:space:]]*"Sorted"', texto,
            perl = TRUE)) {
    return(list(ok = TRUE, metodo = "sort", motivo = NA_character_))
  }
  list(
    ok = FALSE, metodo = NA_character_, motivo = paste(
      "`EXPLAIN` no devolvio una estrategia `Hashed` o `Sorted` para el",
      "nodo Aggregate de la moda."
    )
  )
}

.sql_moda_columna_dbi <- function(conexion, tabla_sql, columna_sql, dialecto,
                                  moda_guardian = NULL) {
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  sin_limite <- paste0(
    "SELECT ", columna_sql, " AS ", alias("valor"), ", COUNT(*) AS ",
    alias("frecuencia"), " FROM ", tabla_sql, " WHERE ", columna_sql,
    " IS NOT NULL GROUP BY ", columna_sql, " ORDER BY ", alias("frecuencia"),
    " DESC, ", columna_sql, " ASC"
  )
  acotada <- dialecto$limitar(sin_limite, 1L, 0)
  list(
    sql = if (is.null(moda_guardian)) {
      if (is.null(acotada)) sin_limite else acotada
    } else moda_guardian$construir(columna_sql, tabla_sql),
    filas = if (is.null(acotada)) 1L else -1L
  )
}

.estadisticas_familia_derrame_dbi <- function(datos, columna) {
  if (!is.data.frame(datos) || !all(c(
    "relacion_oid", "reltuples", "hoja", "attname", "n_distinct",
    "avg_width", "null_frac"
  ) %in% names(datos))) {
    return(list(ok = FALSE, motivo = paste(
      "La respuesta del catalogo no contiene `null_frac`, `avg_width` y",
      "`reltuples` para estimar la salida."
    )))
  }
  relaciones <- unique(datos[c("relacion_oid", "reltuples", "hoja")])
  relaciones <- relaciones[
    !is.na(relaciones$relacion_oid) &
      .logico_catalogo_dbi(relaciones$hoja) %in% TRUE, , drop = FALSE
  ]
  filas <- datos[!is.na(datos$attname) & as.character(datos$attname) == columna,
                 , drop = FALSE]
  if (!nrow(relaciones) || !nrow(filas)) {
    return(list(ok = FALSE, motivo = paste(
      "No hay estadisticas visibles para todas las relaciones hoja de la",
      "columna; puede faltar `ANALYZE` o permiso de lectura."
    )))
  }
  filas <- filas[!duplicated(as.character(filas$relacion_oid)), , drop = FALSE]
  faltantes <- setdiff(
    as.character(relaciones$relacion_oid), as.character(filas$relacion_oid)
  )
  if (length(faltantes)) {
    return(list(ok = FALSE, motivo = paste(
      "No hay una estadistica para todas las relaciones hoja de la columna."
    )))
  }
  n_validos <- numeric(nrow(filas))
  n_distintos <- numeric(nrow(filas))
  anchos <- numeric(nrow(filas))
  tipos <- character(nrow(filas))
  for (i in seq_len(nrow(filas))) {
    reltuples <- suppressWarnings(as.numeric(filas$reltuples[[i]]))
    null_frac <- suppressWarnings(as.numeric(filas$null_frac[[i]]))
    nd <- suppressWarnings(as.numeric(filas$n_distinct[[i]]))
    ancho <- suppressWarnings(as.numeric(filas$avg_width[[i]]))
    if (!is.finite(reltuples) || reltuples < 0 || !is.finite(null_frac) ||
        null_frac < 0 || null_frac > 1 || !is.finite(nd) ||
        !is.finite(ancho) || ancho < 0) {
      return(list(ok = FALSE, motivo = paste(
        "`reltuples`, `null_frac`, `n_distinct` o `avg_width` no es utilizable."
      )))
    }
    n_validos[[i]] <- reltuples * (1 - null_frac)
    n_distintos[[i]] <- if (nd < 0) ceiling(abs(nd) * reltuples) else ceiling(nd)
    anchos[[i]] <- ancho
    if ("tipo" %in% names(filas)) tipos[[i]] <- as.character(filas$tipo[[i]])
  }
  validos <- sum(n_validos)
  distintos <- sum(n_distintos)
  ancho <- if (validos > 0) sum(n_validos * anchos) / validos else mean(anchos)
  if (!is.finite(validos) || !is.finite(distintos) || !is.finite(ancho)) {
    return(list(ok = FALSE, motivo = "La suma de las estadisticas no es utilizable."))
  }
  tipo <- tipos[!is.na(tipos) & nzchar(tipos)]
  list(
    ok = TRUE, n_validos = validos, n_distintos = distintos,
    avg_width = ancho, n_relaciones = nrow(filas),
    tipo = if (length(tipo)) tipo[[1L]] else NA_character_
  )
}

.estimar_derrame_familia_postgresql_dbi <- function(
    conexion, tabla, columnas, presupuesto, familia,
    universo = "tabla_completa", tamano_lote = .TAMANO_LOTE_PLANOS_DBI,
    forma = NA_character_, dialecto = NULL, moda_guardian = NULL,
    tipos = NULL, prototipo = NULL, tabla_sql = NULL) {
  vacia <- function(estado = "no_disponible", motivo = paste(
      "No se pudo estimar el derrame de", familia, ".")) {
    .estimacion_derrame_familia_vacia_dbi(
      familia, motivo, estado = estado, forma = forma
    )
  }
  if (identical(universo, "muestra_motor")) {
    return(vacia(motivo = paste(
      "No se pudo estimar el derrame de", familia,
      "sobre `muestra_motor` con estadisticas de la tabla completa; no se",
      "inventa una equivalencia."
    )))
  }
  if (!length(columnas)) {
    return(vacia("no_solicitado", paste(
      "No se solicito una estimacion de derrame porque no hay columnas que",
      "vayan a emitir", familia, "."
    )))
  }
  if (!grepl("postgres|pqconnection", .senas_conexion_dbi(conexion),
             ignore.case = TRUE, perl = TRUE)) {
    return(vacia(motivo = paste(
      "No se pudo estimar el derrame de", familia, ": la conexion no fue",
      "reconocida como PostgreSQL y no hay un `pg_stats` portable."
    )))
  }
  memoria <- .leer_memoria_postgresql_dbi(conexion, presupuesto)
  if (!isTRUE(memoria$ok)) return(.estimacion_derrame_familia_vacia_dbi(
    familia, memoria$motivo, memoria = memoria, forma = forma
  ))
  if (!is.null(presupuesto)) presupuesto$memoria_derrame <- memoria
  estadisticas <- .estadisticas_hash_postgresql_dbi(
    conexion, tabla, columnas, presupuesto
  )
  if (!isTRUE(estadisticas$ok)) return(.estimacion_derrame_familia_vacia_dbi(
    familia, estadisticas$motivo, memoria = memoria, forma = forma
  ))
  datos <- estadisticas$datos
  filas_catalogo <- estadisticas$filas_catalogo
  columnas_estimadas <- list()
  columnas_no_estimadas <- list()
  for (i in seq_along(columnas)) {
    columna <- columnas[[i]]
    estadistica <- .estadisticas_familia_derrame_dbi(datos, columna)
    if (!isTRUE(estadistica$ok)) {
      columnas_no_estimadas[[length(columnas_no_estimadas) + 1L]] <- data.frame(
        columna = columna, motivo = estadistica$motivo,
        stringsAsFactors = FALSE
      )
      next
    }
    posicion <- if (!is.null(prototipo) && !is.null(names(prototipo))) {
      match(columna, names(prototipo))
    } else NA_integer_
    tipo_declarado <- if (!is.null(tipos) && !is.null(names(tipos)) &&
                          !is.na(posicion) && columna %in% names(tipos)) {
      tipos[[columna]]
    } else if (!is.null(tipos) && !is.na(posicion) && posicion <= length(tipos)) {
      tipos[[posicion]]
    } else if (!is.null(tipos) && i <= length(tipos)) {
      tipos[[i]]
    } else NA_character_
    prototipo_columna <- if (!is.null(prototipo) && !is.na(posicion) &&
                             posicion <= length(prototipo)) {
      prototipo[[posicion]]
    } else if (!is.null(prototipo) && i <= length(prototipo)) {
      prototipo[[i]]
    } else NULL
    tipo <- .familia_tipo_derrame_dbi(
      if (is.na(estadistica$tipo)) tipo_declarado else estadistica$tipo,
      prototipo_columna
    )
    nvalid <- estadistica$n_validos
    nd <- estadistica$n_distintos
    ancho <- estadistica$avg_width
    metodo <- if (identical(familia, "moda")) {
      if (is.null(dialecto)) dialecto <- .dialectos_dbi()$limit
      consulta <- .sql_moda_columna_dbi(
        conexion, if (is.null(tabla_sql)) .texto_tabla_dbi(tabla) else tabla_sql,
        as.character(DBI::dbQuoteIdentifier(conexion, columna)), dialecto,
        moda_guardian
      )
      plan <- .plan_moda_derrame_dbi(conexion, consulta$sql, presupuesto)
      if (!isTRUE(plan$ok)) {
        columnas_no_estimadas[[length(columnas_no_estimadas) + 1L]] <- data.frame(
          columna = columna, motivo = plan$motivo, stringsAsFactors = FALSE
        )
        next
      }
      plan$metodo
    } else {
      "sort"
    }
    hash <- if (identical(metodo, "hash")) {
      nd * (ancho + .TAMANO_BASE_ENTRADA_HASH_POSTGRESQL_DBI)
    } else NA_real_
    tape <- nvalid * (ancho + 8)
    piso <- if (identical(tipo, "numeric")) {
      .PISO_SORT_NUMERIC_POSTGRESQL_DBI
    } else .PISO_SORT_FIJOS_POSTGRESQL_DBI
    memoria_sort <- max(
      nvalid * (ancho + .TAMANO_BASE_SORT_POSTGRESQL_DBI), nvalid * piso
    )
    decision <- if (identical(metodo, "hash")) hash else memoria_sort
    limite <- if (identical(metodo, "hash")) {
      memoria$memoria_efectiva_bytes
    } else memoria$work_mem_bytes
    columnas_estimadas[[length(columnas_estimadas) + 1L]] <- data.frame(
      columna = columna, metodo = metodo, forma = forma,
      tipo_familia = tipo, n_distintos_estimados = nd,
      n_validos_catalogo = nvalid, n_validos_medido = NA_real_,
      avg_width = ancho,
      estado_hash_bytes = hash,
      estado_sort_bytes = if (identical(metodo, "sort")) tape else NA_real_,
      estado_memoria_bytes = if (identical(metodo, "sort")) memoria_sort else NA_real_,
      estado_io_total_bytes = NA_real_, tamano_estimado_bytes = decision,
      supera_memoria = if (is.finite(limite)) decision > limite else NA,
      stringsAsFactors = FALSE
    )
  }
  columnas_df <- if (length(columnas_estimadas)) do.call(rbind, columnas_estimadas)
    else .estimacion_derrame_familia_vacia_dbi(familia)$columnas
  no_estimadas_df <- if (length(columnas_no_estimadas)) {
    do.call(rbind, columnas_no_estimadas)
  } else .estimacion_derrame_familia_vacia_dbi(familia)$columnas_no_estimadas
  if (identical(familia, "moda") || !identical(forma, "consolidada")) {
    grupos <- lapply(seq_len(nrow(columnas_df)), function(i) i)
  } else {
    grupos <- split(seq_len(nrow(columnas_df)), ceiling(
      seq_len(nrow(columnas_df)) / max(1L, as.integer(tamano_lote))
    ))
  }
  lotes <- if (length(grupos)) do.call(rbind, lapply(seq_along(grupos), function(i) {
    filas <- columnas_df[grupos[[i]], , drop = FALSE]
    metodo <- unique(filas$metodo)
    metodo <- if (length(metodo) == 1L) metodo else "por_columna"
    decision <- if (all(is.na(filas$estado_memoria_bytes))) {
      max(filas$estado_hash_bytes, na.rm = TRUE)
    } else max(filas$estado_memoria_bytes, na.rm = TRUE)
    limite <- if (identical(metodo, "hash")) memoria$memoria_efectiva_bytes
      else memoria$work_mem_bytes
    data.frame(
      lote = as.integer(i), columnas = paste(filas$columna, collapse = ", "),
      metodo = metodo, forma = forma,
      estado_hash_bytes = if (all(is.na(filas$estado_hash_bytes))) NA_real_
        else max(filas$estado_hash_bytes, na.rm = TRUE),
      estado_sort_bytes = if (all(is.na(filas$estado_sort_bytes))) NA_real_
        else max(filas$estado_sort_bytes, na.rm = TRUE),
      estado_memoria_bytes = if (all(is.na(filas$estado_memoria_bytes))) NA_real_
        else max(filas$estado_memoria_bytes, na.rm = TRUE),
      estado_io_total_bytes = if (identical(familia, "mediana") &&
                                  identical(forma, "consolidada") &&
                                  !all(is.na(filas$estado_sort_bytes))) {
        sum(filas$estado_sort_bytes, na.rm = TRUE)
      } else NA_real_,
      tamano_estimado_bytes = decision,
      supera_memoria = if (is.finite(limite)) decision > limite else NA,
      stringsAsFactors = FALSE
    )
  })) else .estimacion_derrame_familia_vacia_dbi(familia)$lotes
  if (identical(familia, "mediana") && identical(forma, "consolidada") &&
      nrow(lotes)) {
    for (i in seq_along(grupos)) {
      columnas_df$estado_io_total_bytes[grupos[[i]]] <-
        lotes$estado_io_total_bytes[[i]]
    }
  }
  sobre <- which(!is.na(lotes$supera_memoria) & lotes$supera_memoria)
  n_estimadas <- nrow(columnas_df)
  estado <- if (!n_estimadas) "no_disponible" else if (
    n_estimadas < length(columnas)
  ) "parcial" else "estimado"
  motivo <- if (identical(familia, "moda")) paste(
    "El metodo de la moda sale del nodo Aggregate de la consulta exacta",
    "mediante `EXPLAIN (FORMAT JSON, COSTS OFF)` sin ANALYZE. El estado hash",
    "usa `n_distinct * (avg_width + 64)`; el estado sort usa la huella de",
    "decision y el tape declarados por familia. Las estadisticas pueden estar",
    "viejas: un margen no demuestra que no haya derrame."
  ) else paste(
    "La mediana se estima como un sort de los valores no nulos. La huella de",
    "decision usa `n_validos * (avg_width + 24)` con piso por familia y el",
    "tape `n_validos * (avg_width + 8)` es informativo; el tape nunca decide.",
    "El limite de la mediana es `work_mem`, sin multiplicador."
  )
  if (length(no_estimadas_df)) motivo <- paste(
    motivo, paste(no_estimadas_df$columna, no_estimadas_df$motivo,
                  sep = ": ", collapse = " ")
  )
  fuente <- if (identical(familia, "moda")) paste(
    "EXPLAIN (FORMAT JSON, COSTS OFF) de la consulta exacta + pg_stats.n_distinct",
    "+ pg_stats.avg_width + pg_class.reltuples + SHOW work_mem"
  ) else paste(
    "pg_stats.null_frac + pg_stats.avg_width + pg_class.reltuples + SHOW work_mem"
  )
  if (identical(familia, "moda") && isTRUE(memoria$hash_mem_multiplier_disponible)) {
    fuente <- paste0(fuente, " + SHOW hash_mem_multiplier")
  }
  metodo <- unique(columnas_df$metodo)
  metodo <- if (length(metodo) == 1L) metodo[[1L]] else if (length(metodo)) {
    "por_columna"
  } else NA_character_
  limite_general <- if (identical(familia, "mediana") ||
                        identical(metodo, "sort")) {
    memoria$work_mem_bytes
  } else if (identical(metodo, "hash")) {
    memoria$memoria_efectiva_bytes
  } else NA_real_
  memoria_general <- if (is.finite(limite_general)) {
    .memoria_dbi(limite_general)
  } else NA_character_
  list(
    estado = estado, disponible = n_estimadas > 0L, es_estimacion = TRUE,
    familia = familia, metodo = metodo, forma = forma, fuente = fuente,
    motivo = motivo, work_mem = memoria$work_mem,
    work_mem_bytes = memoria$work_mem_bytes,
    hash_mem_multiplier = memoria$hash_mem_multiplier,
    hash_mem_multiplier_disponible = memoria$hash_mem_multiplier_disponible,
    memoria_efectiva_bytes = limite_general,
    memoria_efectiva = memoria_general,
    columnas = columnas_df, columnas_no_estimadas = no_estimadas_df,
    lotes = lotes, lotes_sobre_memoria = as.integer(sobre),
    n_columnas_solicitadas = as.integer(length(columnas)),
    n_columnas_estimadas = as.integer(n_estimadas), filas_catalogo = filas_catalogo,
    supera_memoria = if (length(sobre)) TRUE else if (nrow(lotes) &&
      all(!is.na(lotes$supera_memoria))) FALSE else NA
  )
}

.actualizar_n_validos_estimacion_dbi <- function(estimacion, agregados,
                                                  metricas, salida) {
  if (is.null(estimacion) || !is.data.frame(estimacion$columnas) ||
      !nrow(estimacion$columnas)) return(estimacion)
  pidio <- "validos" %in% metricas
  valores <- vapply(estimacion$columnas$columna, function(columna) {
    resultado <- if (isTRUE(pidio) && !is.null(agregados$conteos[[columna]])) {
      agregados$conteos[[columna]]$validos
    } else NULL
    if (is.list(resultado) && isTRUE(resultado$ok)) {
      .numero_dbi(resultado$valor)
    } else NA_real_
  }, numeric(1L))
  estimacion$columnas$n_validos_medido <- valores
  tiene_medido <- isTRUE(pidio) && any(is.finite(valores))
  estimacion$fuente_denominador <- if (tiene_medido) {
    "n_validos medido"
  } else {
    "estimacion de catalogo"
  }
  if (tiene_medido) {
    for (i in seq_len(nrow(estimacion$columnas))) {
      medido <- valores[[i]]
      if (!is.finite(medido)) next
      tipo <- estimacion$columnas$tipo_familia[[i]]
      ancho <- estimacion$columnas$avg_width[[i]]
      piso <- if (identical(tipo, "numeric")) .PISO_SORT_NUMERIC_POSTGRESQL_DBI
        else .PISO_SORT_FIJOS_POSTGRESQL_DBI
      memoria_sort <- max(
        medido * (ancho + .TAMANO_BASE_SORT_POSTGRESQL_DBI), medido * piso
      )
      if (identical(estimacion$columnas$metodo[[i]], "hash")) {
        decision <- estimacion$columnas$estado_hash_bytes[[i]]
        limite <- estimacion$hash_mem_multiplier * estimacion$work_mem_bytes
      } else {
        decision <- memoria_sort
        limite <- estimacion$work_mem_bytes
        estimacion$columnas$estado_memoria_bytes[[i]] <- memoria_sort
        estimacion$columnas$estado_sort_bytes[[i]] <- medido * (ancho + 8)
      }
      estimacion$columnas$tamano_estimado_bytes[[i]] <- decision
      estimacion$columnas$supera_memoria[[i]] <- is.finite(limite) && decision > limite
    }
    if (identical(salida, "meta")) {
      estimacion$fuente <- paste(
        estimacion$fuente,
        "n_validos medido por los agregados planos de esta corrida cuando la",
        "familia `validos` fue solicitada; avg_width y el resto vienen del catalogo."
      )
      estimacion$motivo <- paste(
        estimacion$motivo, "Para esta salida se uso `n_validos` medido cuando",
        "estuvo disponible; las columnas sin medicion conservan el catalogo."
      )
    }
  } else if (identical(salida, "meta")) {
    estimacion$motivo <- paste(
      estimacion$motivo, "No se midio `n_validos` para esta salida porque",
      "la familia `validos` no fue solicitada o su consulta no fue utilizable;",
      "se conserva el `n_validos_catalogo` del plan."
    )
  }
  if (is.data.frame(estimacion$lotes) && nrow(estimacion$lotes)) {
    for (i in seq_len(nrow(estimacion$lotes))) {
      columnas <- trimws(strsplit(estimacion$lotes$columnas[[i]], ",", fixed = TRUE)[[1L]])
      filas <- estimacion$columnas[estimacion$columnas$columna %in% columnas, , drop = FALSE]
      decision <- if (all(is.na(filas$estado_memoria_bytes))) {
        max(filas$estado_hash_bytes, na.rm = TRUE)
      } else max(filas$estado_memoria_bytes, na.rm = TRUE)
      estimacion$lotes$tamano_estimado_bytes[[i]] <- decision
      estimacion$lotes$estado_memoria_bytes[[i]] <- if (all(
        is.na(filas$estado_memoria_bytes)
      )) NA_real_ else max(filas$estado_memoria_bytes, na.rm = TRUE)
      estimacion$lotes$estado_sort_bytes[[i]] <- if (all(
        is.na(filas$estado_sort_bytes)
      )) NA_real_ else max(filas$estado_sort_bytes, na.rm = TRUE)
      estimacion$lotes$estado_io_total_bytes[[i]] <- if (
        identical(estimacion$familia, "mediana") &&
        identical(estimacion$forma, "consolidada") &&
        !all(is.na(filas$estado_sort_bytes))
      ) sum(filas$estado_sort_bytes, na.rm = TRUE) else NA_real_
      limite <- if (identical(estimacion$metodo, "hash")) {
        estimacion$work_mem_bytes * estimacion$hash_mem_multiplier
      } else estimacion$work_mem_bytes
      estimacion$lotes$supera_memoria[[i]] <- is.finite(limite) && decision > limite
    }
    estimacion$lotes_sobre_memoria <- which(estimacion$lotes$supera_memoria %in% TRUE)
    estimacion$supera_memoria <- if (length(estimacion$lotes_sobre_memoria)) TRUE
      else if (all(!is.na(estimacion$lotes$supera_memoria))) FALSE else NA
  }
  estimacion
}

.filtrar_estimacion_derrame_dbi <- function(estimacion, columnas) {
  if (is.null(estimacion) || !is.data.frame(estimacion$columnas) ||
      !nrow(estimacion$columnas)) return(estimacion)
  columnas <- intersect(as.character(columnas), estimacion$columnas$columna)
  estimacion$columnas <- estimacion$columnas[
    match(columnas, estimacion$columnas$columna), , drop = FALSE
  ]
  if (!is.data.frame(estimacion$lotes) || !nrow(estimacion$lotes)) {
    estimacion$lotes_sobre_memoria <- integer()
    estimacion$supera_memoria <- NA
    return(estimacion)
  }
  lotes <- lapply(seq_len(nrow(estimacion$lotes)), function(i) {
    nombres <- trimws(strsplit(
      estimacion$lotes$columnas[[i]], ",", fixed = TRUE
    )[[1L]])
    nombres <- intersect(nombres, columnas)
    if (!length(nombres)) return(NULL)
    filas <- estimacion$columnas[
      estimacion$columnas$columna %in% nombres, , drop = FALSE
    ]
    metodo <- unique(filas$metodo)
    metodo <- if (length(metodo) == 1L) metodo[[1L]] else "por_columna"
    es_sort <- !all(is.na(filas$estado_memoria_bytes))
    decision <- if (es_sort) max(filas$estado_memoria_bytes, na.rm = TRUE) else
      max(filas$estado_hash_bytes, na.rm = TRUE)
    limite <- if (identical(metodo, "hash")) {
      estimacion$memoria_efectiva_bytes
    } else estimacion$work_mem_bytes
    data.frame(
      lote = estimacion$lotes$lote[[i]],
      columnas = paste(nombres, collapse = ", "), metodo = metodo,
      forma = estimacion$lotes$forma[[i]],
      estado_hash_bytes = if (all(is.na(filas$estado_hash_bytes))) NA_real_
        else max(filas$estado_hash_bytes, na.rm = TRUE),
      estado_sort_bytes = if (all(is.na(filas$estado_sort_bytes))) NA_real_
        else max(filas$estado_sort_bytes, na.rm = TRUE),
      estado_memoria_bytes = if (all(is.na(filas$estado_memoria_bytes))) NA_real_
        else max(filas$estado_memoria_bytes, na.rm = TRUE),
      estado_io_total_bytes = if (
        identical(estimacion$familia, "mediana") &&
        identical(estimacion$forma, "consolidada") &&
        !all(is.na(filas$estado_sort_bytes))
      ) sum(filas$estado_sort_bytes, na.rm = TRUE) else NA_real_,
      tamano_estimado_bytes = decision,
      supera_memoria = if (is.finite(limite)) decision > limite else NA,
      stringsAsFactors = FALSE
    )
  })
  lotes <- Filter(Negate(is.null), lotes)
  estimacion$lotes <- if (length(lotes)) do.call(rbind, lotes) else {
    estimacion$lotes[0L, , drop = FALSE]
  }
  estimacion$lotes_sobre_memoria <- which(
    estimacion$lotes$supera_memoria %in% TRUE
  )
  estimacion$supera_memoria <- if (length(estimacion$lotes_sobre_memoria)) TRUE
    else if (nrow(estimacion$lotes) &&
             all(!is.na(estimacion$lotes$supera_memoria))) FALSE else NA
  estimacion$n_columnas_solicitadas <- as.integer(length(columnas))
  estimacion$n_columnas_estimadas <- as.integer(nrow(estimacion$columnas))
  estimacion
}

# ---- Reloj e instrumentacion --------------------------------------------

# `Sys.time()` puede devolver el mismo instante para dos lecturas. En ese caso
# una duracion cero seria una precision inventada, no una consulta instantanea:
# se publica `NA` y la metadata declara que el reloj no pudo resolver el
# intervalo.
.ahora_dbi <- function() {
  tryCatch(Sys.time(), error = function(e) NULL)
}

.duracion_ms_dbi <- function(inicio, fin) {
  if (is.null(inicio) || is.null(fin)) return(NA_real_)
  segundos <- tryCatch(
    as.numeric(difftime(fin, inicio, units = "secs")),
    error = function(e) NA_real_
  )
  if (!length(segundos) || is.na(segundos) || !is.finite(segundos) ||
      segundos <= 0) {
    return(NA_real_)
  }
  segundos * 1000
}

# `proc.time()` separa el tiempo que el proceso trabajo del que paso esperando
# al motor, a la red o a un bloqueo. Se guardan solo `user.self` y `sys.self`:
# el tiempo de los procesos hijos no pertenece al cliente que esta perfilando.
# A diferencia del reloj transcurrido, cero es una medicion valida aca: una
# consulta puede terminar sin consumir una centesima de CPU. Lo que no se pudo
# leer queda en `NA`, nunca se reemplaza por cero.
.ahora_cpu_dbi <- function() {
  tryCatch({
    tiempo <- proc.time()
    campos <- c("user.self", "sys.self")
    if (!all(campos %in% names(tiempo))) return(NULL)
    valor <- sum(as.numeric(tiempo[campos]))
    if (!length(valor) || is.na(valor) || !is.finite(valor)) NULL else valor
  }, error = function(e) NULL)
}

.duracion_cpu_ms_dbi <- function(inicio, fin) {
  if (is.null(inicio) || is.null(fin) || length(inicio) != 1L ||
      length(fin) != 1L) {
    return(NA_real_)
  }
  diferencia <- suppressWarnings(as.numeric(fin) - as.numeric(inicio))
  if (!length(diferencia) || is.na(diferencia) || !is.finite(diferencia) ||
      diferencia < 0) {
    return(NA_real_)
  }
  diferencia * 1000
}

.medicion_consulta_vacia_dbi <- function(etapa = NA_character_) {
  list(
    consulta_id = NA_integer_, etapa = as.character(etapa),
    duracion_ms = NA_real_, n_filas_resultado = NA_real_,
    bytes_resultado_r = NA_real_, cpu_ms = NA_real_, instrumentar = FALSE
  )
}

.iniciar_consulta_dbi <- function(presupuesto, etapa) {
  if (is.null(presupuesto)) return(.medicion_consulta_vacia_dbi(etapa))
  presupuesto$siguiente_consulta <- presupuesto$siguiente_consulta + 1L
  list(
    consulta_id = as.integer(presupuesto$siguiente_consulta),
    etapa = as.character(etapa),
    instrumentar = isTRUE(presupuesto$instrumentar),
    inicio = if (isTRUE(presupuesto$instrumentar)) .ahora_dbi() else NULL,
    inicio_cpu = if (isTRUE(presupuesto$instrumentar)) .ahora_cpu_dbi() else NULL
  )
}

.terminar_consulta_dbi <- function(medicion, datos = NULL) {
  resultado <- .medicion_consulta_vacia_dbi(medicion$etapa)
  resultado$consulta_id <- medicion$consulta_id
  if (isTRUE(medicion$instrumentar)) {
    resultado$duracion_ms <- .duracion_ms_dbi(medicion$inicio, .ahora_dbi())
    resultado$cpu_ms <- .duracion_cpu_ms_dbi(
      medicion$inicio_cpu, .ahora_cpu_dbi()
    )
    if (is.null(datos)) {
      resultado$n_filas_resultado <- NA_real_
      resultado$bytes_resultado_r <- NA_real_
    } else {
      resultado$n_filas_resultado <- tryCatch(
        as.numeric(nrow(datos)), error = function(e) NA_real_
      )
      resultado$bytes_resultado_r <- tryCatch(
        as.numeric(utils::object.size(datos)), error = function(e) NA_real_
      )
    }
  }
  resultado
}

.adjuntar_medicion_dbi <- function(resultado, medicion) {
  if (is.null(medicion)) return(resultado)
  for (nombre in c(
    "consulta_id", "etapa", "duracion_ms", "n_filas_resultado",
    "bytes_resultado_r", "cpu_ms"
  )) {
    if (!is.null(medicion[[nombre]])) resultado[[nombre]] <- medicion[[nombre]]
  }
  resultado
}

.trazador_tiempos_dbi <- function(activo = TRUE) {
  estado <- new.env(parent = emptyenv())
  estado$activo <- isTRUE(activo)
  estado$etapas <- list()
  # Las etapas se anidan -`perfilado_muestra` envuelve al perfilado por columna,
  # a las dependencias y a los casi-duplicados-, asi que sus duraciones NO se
  # pueden sumar. Eso estaba dicho en la viñeta y en el `Rd`, y no en el objeto:
  # quien lo mira en la consola ve siete filas sin nada que lo advierta. El
  # trazador lleva la profundidad para que el objeto lo diga solo, en vez de
  # depender de una tabla de jerarquia escrita a mano que se desactualiza en
  # cuanto alguien agregue una etapa en otro archivo.
  estado$profundidad <- 0L
  estado
}

.registrar_etapa_dbi <- function(trazador, etapa, inicio = NULL, fin = NULL,
                                 estado = NULL, nivel = NULL,
                                 cpu_inicio = NULL, cpu_fin = NULL) {
  if (is.null(trazador)) return(invisible(NULL))
  medido <- isTRUE(trazador$activo)
  if (is.null(estado)) estado <- if (medido) "medido" else "no_medido"
  if (is.null(nivel)) nivel <- trazador$profundidad + 1L
  trazador$etapas[[length(trazador$etapas) + 1L]] <- list(
    etapa = as.character(etapa),
    duracion_ms = if (medido) .duracion_ms_dbi(inicio, fin) else NA_real_,
    cpu_ms = if (medido) .duracion_cpu_ms_dbi(cpu_inicio, cpu_fin) else NA_real_,
    estado = as.character(estado),
    nivel = as.integer(nivel)
  )
  invisible(NULL)
}

.medir_etapa_dbi <- function(trazador, etapa, expresion, activa = TRUE) {
  if (is.null(trazador)) return(force(expresion))
  if (!isTRUE(activa)) {
    .registrar_etapa_dbi(trazador, etapa, estado = "no_solicitado")
    return(force(expresion))
  }
  inicio <- if (isTRUE(trazador$activo)) .ahora_dbi() else NULL
  cpu_inicio <- if (isTRUE(trazador$activo)) .ahora_cpu_dbi() else NULL
  nivel <- trazador$profundidad + 1L
  trazador$profundidad <- nivel
  # El descuento va por `on.exit` para que un error dentro de la etapa no deje
  # la profundidad corrida y todo lo que venga despues mal clasificado.
  on.exit(trazador$profundidad <- nivel - 1L, add = TRUE)
  valor <- tryCatch(
    list(ok = TRUE, valor = force(expresion)),
    error = function(e) list(ok = FALSE, error = e)
  )
  .registrar_etapa_dbi(
    trazador, etapa, inicio, .ahora_dbi(), nivel = nivel,
    cpu_inicio = cpu_inicio, cpu_fin = .ahora_cpu_dbi()
  )
  if (!isTRUE(valor$ok)) stop(valor$error)
  valor$valor
}

.resumen_tiempos_dbi <- function(trazador) {
  vacio <- data.frame(
    etapa = character(), duracion_ms = numeric(), cpu_ms = numeric(),
    estado = character(),
    nivel = integer(), n_ejecuciones = integer(), stringsAsFactors = FALSE
  )
  if (is.null(trazador) || !length(trazador$etapas)) return(vacio)
  datos <- do.call(rbind, lapply(trazador$etapas, function(x) {
    data.frame(
      etapa = x$etapa, duracion_ms = x$duracion_ms, estado = x$estado,
      cpu_ms = if (is.null(x$cpu_ms)) NA_real_ else x$cpu_ms,
      nivel = if (is.null(x$nivel)) 1L else x$nivel,
      n_ejecuciones = 1L, stringsAsFactors = FALSE
    )
  }))
  grupos <- split(seq_len(nrow(datos)), datos$etapa)
  salida <- do.call(rbind, lapply(names(grupos), function(etapa) {
    filas <- datos[grupos[[etapa]], , drop = FALSE]
    duraciones <- filas$duracion_ms
    cpu <- filas$cpu_ms
    data.frame(
      etapa = etapa,
      duracion_ms = if (all(!is.na(duraciones))) sum(duraciones) else NA_real_,
      cpu_ms = if (all(!is.na(cpu))) sum(cpu) else NA_real_,
      estado = if (any(filas$estado == "no_solicitado")) {
        "no_solicitado"
      } else if (any(filas$estado == "no_medido")) {
        "no_medido"
      } else {
        "medido"
      },
      # Sólo las de nivel 1 se pueden sumar entre sí; las demás están
      # contenidas en alguna de ellas.
      nivel = min(filas$nivel),
      n_ejecuciones = nrow(filas), stringsAsFactors = FALSE
    )
  }))
  rownames(salida) <- NULL
  salida
}

# Perfilar la tabla entera -lo que viene por omision- puede tardar minutos sobre
# una tabla grande, y una corrida callada no se distingue de una colgada. La
# barra avanza contra las consultas que el plan dice que se van a emitir, que es
# un total conocido y no una estimacion.
#
# No se abre sola en pruebas ni en guiones: `interactive()` decide, y
# `options(lupa.progreso = )` manda sobre eso en los dos sentidos. Una barra que
# aparece en la salida de un guion es ruido que despues hay que filtrar.
.abrir_progreso_dbi <- function(presupuesto, previstas, envir) {
  if (is.null(presupuesto)) return(invisible(NULL))
  # Debajo de una docena de consultas la corrida termina antes de que la barra
  # sirva para algo.
  if (!.progreso_activo(previstas, 12)) return(invisible(NULL))
  presupuesto$previstas <- previstas
  presupuesto$barra <- cli::cli_progress_bar(
    "Perfilando", total = previstas, .envir = envir, clear = TRUE
  )
  invisible(presupuesto$barra)
}

.saldo_dbi <- function(presupuesto) {
  presupuesto$max - presupuesto$usadas - presupuesto$reserva
}

.gastar_dbi <- function(presupuesto) {
  if (is.null(presupuesto)) return(TRUE)
  if (.saldo_dbi(presupuesto) < 1) {
    presupuesto$agotado <- TRUE
    return(FALSE)
  }
  presupuesto$usadas <- presupuesto$usadas + 1
  if (!is.null(presupuesto$barra)) {
    # `set` y no `inc`: la cuenta que manda es la del presupuesto, que ya
    # incluye las consultas-porton emitidas antes de abrir la barra.
    try(
      cli::cli_progress_update(
        id = presupuesto$barra,
        set = min(presupuesto$usadas, presupuesto$previstas)
      ),
      silent = TRUE
    )
  }
  TRUE
}

# Las llamadas de metadatos -`dbExistsTable()`, `dbListFields()`- tambien
# viajan a la base. Se cuentan aunque no se puedan rechazar: el numero que se
# informa tiene que ser el numero real de viajes.
.contar_dbi <- function(presupuesto, n = 1) {
  if (is.null(presupuesto)) return(invisible(NULL))
  presupuesto$usadas <- presupuesto$usadas + n
  invisible(NULL)
}

.motivo_presupuesto_dbi <- function(presupuesto) {
  paste0(
    "Se agoto el presupuesto declarado en `max_consultas` (",
    format(presupuesto$max, scientific = FALSE), " consultas)."
  )
}

# ---- Adaptador de dialecto ----------------------------------------------
#
# Acotar filas no es SQL estandar. `LIMIT` no existe en SQL Server ni en
# Oracle anterior a 12c, y `TOP (n)` no existe en ningun otro lado. En vez de
# suponer un dialecto se declara una capacidad -acotar filas- con varias
# implementaciones, se sondea cual acepta el motor con una consulta de cero
# filas, y si ninguna anda queda la via portable: `dbSendQuery()` mas
# `dbFetch(n)`, que no necesita clausula ninguna.
#
# `limitar()` devuelve NULL cuando el dialecto no puede expresar ese recorte;
# quien llama decide entonces si acota en el cliente o si degrada la metrica.

.entero_sql_dbi <- function(n) {
  if (inherits(n, "integer64") && .bit64_disponible_dbi()) {
    return(as.character(n[[1L]]))
  }
  format(round(as.numeric(n)), scientific = FALSE, trim = TRUE)
}

.dialectos_dbi <- function() {
  list(
    limit = list(
      nombre = "limit",
      descripcion = "LIMIT n [OFFSET k]",
      motores = "PostgreSQL, SQLite, MySQL/MariaDB, DuckDB y compatibles",
      patron = paste0(
        "sqlite|postgres|redshift|mysql|mariadb|duckdb|clickhouse|monetdb|",
        "vertica|greenplum|presto|trino|spark|hive|snowflake|bigquery|athena"
      ),
      alias_tabla = function(nombre) paste0(" AS ", nombre),
      mediana_escalar = list(resto = "%", division = "/"),
      limitar = function(sql, n, salto = 0) {
        paste0(
          sql, " LIMIT ", .entero_sql_dbi(n),
          if (salto > 0) paste0(" OFFSET ", .entero_sql_dbi(salto)) else ""
        )
      },
      muestreo = c("tablesample_reservoir", "tablesample_system",
                   "tablesample_bernoulli", "tablesample_percent", "random_limit")
    ),
    top = list(
      nombre = "top",
      descripcion = "TOP (n), y OFFSET k ROWS FETCH NEXT n ROWS ONLY con orden",
      motores = "SQL Server 2012 o posterior, Sybase",
      patron = "sql server|microsoft sql|sqlserver|mssql|tsql|sybase",
      alias_tabla = function(nombre) paste0(" AS ", nombre),
      mediana_escalar = NULL,
      limitar = function(sql, n, salto = 0) {
        if (salto > 0) {
          return(paste0(
            sql, " OFFSET ", .entero_sql_dbi(salto), " ROWS FETCH NEXT ",
            .entero_sql_dbi(n), " ROWS ONLY"
          ))
        }
        if (!grepl("^SELECT ", sql)) return(NULL)
        sub("^SELECT ", paste0("SELECT TOP (", .entero_sql_dbi(n), ") "), sql)
      },
      muestreo = c("tablesample_reservoir", "tablesample_system",
                   "tablesample_bernoulli", "tablesample_percent", "random_limit")
    ),
    fetch_first = list(
      nombre = "fetch_first",
      descripcion = "OFFSET k ROWS FETCH FIRST n ROWS ONLY",
      motores = "Oracle 12c o posterior, DB2, Derby, H2",
      patron = "oracle|db2|informix|derby|hsqldb|\\bh2\\b",
      alias_tabla = function(nombre) paste0(" ", nombre),
      mediana_escalar = NULL,
      limitar = function(sql, n, salto = 0) {
        paste0(
          sql,
          if (salto > 0) paste0(" OFFSET ", .entero_sql_dbi(salto), " ROWS") else "",
          " FETCH FIRST ", .entero_sql_dbi(n), " ROWS ONLY"
        )
      },
      muestreo = c("tablesample_reservoir", "tablesample_system",
                   "tablesample_bernoulli", "tablesample_percent", "oracle_sample",
                   "random_limit")
    ),
    rownum = list(
      nombre = "rownum",
      descripcion = "ROWNUM <= n; no expresa salto",
      motores = "Oracle anterior a 12c",
      patron = "oracle",
      alias_tabla = function(nombre) paste0(" ", nombre),
      mediana_escalar = NULL,
      limitar = function(sql, n, salto = 0) {
        if (salto > 0) return(NULL)
        paste0(
          "SELECT * FROM (", sql, ") lupa_recorte WHERE ROWNUM <= ",
          .entero_sql_dbi(n)
        )
      },
      muestreo = c("tablesample_reservoir", "tablesample_system",
                   "tablesample_bernoulli", "tablesample_percent", "oracle_sample",
                   "random_limit")
    ),
    portable = list(
      nombre = "portable",
      descripcion = "sin clausula de limite: se acota con dbSendQuery() y dbFetch(n)",
      motores = "cualquier motor con DBI",
      patron = NA_character_,
      alias_tabla = function(nombre) paste0(" AS ", nombre),
      mediana_escalar = NULL,
      limitar = function(sql, n, salto = 0) NULL,
      muestreo = c("tablesample_reservoir", "tablesample_system",
                   "tablesample_bernoulli", "tablesample_percent", "random_limit")
    )
  )
}

.candidatos_muestreo_dbi <- function(conexion, dialecto) {
  nombres <- if (!is.null(dialecto$muestreo)) dialecto$muestreo else character()
  todas <- list(
    # Primero la forma de cantidad fija, y la razon salio de medir contra
    # DuckDB: `TABLESAMPLE (p PERCENT)` es a nivel de bloque y devuelve 0 filas
    # o 2.048 sobre una tabla de 5.000, asi que dos consultas del mismo perfil
    # ven muestras de tamano distinto -o una ve cero- y las metricas dejan de
    # ser comparables entre si. Una forma que devuelve exactamente `n` filas no
    # arregla que cada consulta saque su propia muestra, pero al menos respeta
    # el tamano solicitado.
    tablesample_reservoir = list(
      nombre = "tablesample_reservoir",
      descripcion = "TABLESAMPLE RESERVOIR (n ROWS)",
      patron = "duckdb",
      tipo = "tablesample_filas",
      constructor = function(tabla, filas) paste0(
        tabla, " TABLESAMPLE RESERVOIR (", .entero_sql_dbi(filas), " ROWS)"
      )
    ),
    # La fuente por bloques se intenta antes que las formas fila a fila. Se
    # prefiere `SYSTEM` cuando el adaptador la declara porque su costo favorece
    # tablas grandes; su variacion de paginas se publica en la metadata.
    #
    #   TABLESAMPLE SYSTEM (20)      678  904  452  1384
    #   TABLESAMPLE BERNOULLI (20)  1011 1017  981  1050
    #
    # `SYSTEM` elige bloques enteros, asi que sobre una tabla chica el tamano de
    # la muestra salta de un tercio al doble de lo pedido, y puede dar cero.
    # `BERNOULLI` decide fila por fila y se conserva como alternativa declarada;
    # cuesta mas en el motor -recorre la tabla- pero evita el sesgo de paginas.
    tablesample_bernoulli = list(
      nombre = "tablesample_bernoulli",
      descripcion = "TABLESAMPLE BERNOULLI (p)",
      patron = "postgres|redshift|duckdb",
      tipo = "tablesample",
      constructor = function(tabla, porcentaje) paste0(
        tabla, " TABLESAMPLE BERNOULLI (", porcentaje, ")"
      )
    ),
    tablesample_system = list(
      nombre = "tablesample_system",
      descripcion = "TABLESAMPLE SYSTEM (p)",
      patron = "postgres|redshift|sql server|microsoft sql|sqlserver|mssql",
      tipo = "tablesample",
      constructor = function(tabla, porcentaje) paste0(
        tabla, " TABLESAMPLE SYSTEM (", porcentaje, ")"
      )
    ),
    tablesample_percent = list(
      nombre = "tablesample_percent",
      descripcion = "TABLESAMPLE (p PERCENT)",
      patron = "sql server|microsoft sql|sqlserver|mssql|sybase",
      tipo = "tablesample",
      constructor = function(tabla, porcentaje) paste0(
        tabla, " TABLESAMPLE (", porcentaje, " PERCENT)"
      )
    ),
    oracle_sample = list(
      nombre = "oracle_sample",
      descripcion = "SAMPLE (p)",
      patron = "oracle",
      tipo = "tablesample",
      constructor = function(tabla, porcentaje) paste0(
        tabla, " SAMPLE (", porcentaje, ")"
      )
    ),
    random_limit = list(
      nombre = "random_limit",
      descripcion = "ORDER BY funcion pseudoaleatoria con limite del dialecto",
      patron = NA_character_,
      tipo = "aleatorio",
      funciones = list(
        list(nombre = "random", patron = "sqlite|postgres|redshift|duckdb|snowflake|bigquery", sql = "RANDOM()"),
        list(nombre = "rand", patron = "mysql|mariadb|clickhouse", sql = "RAND()"),
        list(nombre = "newid", patron = "sql server|microsoft sql|sqlserver|mssql", sql = "NEWID()"),
        list(nombre = "dbms_random", patron = "oracle", sql = "DBMS_RANDOM.VALUE"),
        list(nombre = "random_generic", patron = NA_character_, sql = "RANDOM()")
      )
    )
  )
  senas <- .senas_conexion_dbi(conexion)
  orden_funciones <- function(funciones) {
    reconocidas <- vapply(funciones, function(x) {
      !is.na(x$patron) && grepl(x$patron, senas, perl = TRUE)
    }, logical(1L))
    c(which(reconocidas), which(!reconocidas))
  }
  if ("random_limit" %in% nombres) {
    funciones <- todas$random_limit$funciones
    todas$random_limit$funciones <- funciones[orden_funciones(funciones)]
  }
  todas[nombres]
}

.sondas_muestreo_previstas_dbi <- function(candidatos) {
  if (!length(candidatos)) return(0L)
  as.integer(sum(vapply(candidatos, function(candidato) {
    if (identical(candidato$tipo, "tablesample_filas") ||
        identical(candidato$tipo, "tablesample")) {
      1L
    } else {
      length(candidato$funciones)
    }
  }, integer(1L))))
}

# `alias` estaba en la firma y la funcion lo REASIGNA antes de usarlo, con
# `dbQuoteIdentifier(conexion, "lupa_sonda")`. O sea que lo que pasara el
# llamador se descartaba siempre; y de hecho pasaba `NULL`. Un argumento
# sombreado es peor que uno sin usar: hace creer que el valor influye.
.forma_muestreo_dbi <- function(candidato, tabla_sql, campos_sql, porcentaje,
                                muestra, dialecto) {
  # Con `muestra = Inf` -la tabla entera, que es el valor por omision- ninguna
  # forma de muestreo en el motor tiene sentido, y hay que decirlo una sola vez:
  #
  # - la de cantidad fija escribiria `RESERVOIR (Inf ROWS)` y el motor no parsea;
  # - la de porcentaje recibe una fraccion saturada en 1, o sea `100 PERCENT`,
  #   que es la tabla entera;
  # - la de orden pseudoaleatorio no tiene donde cortar, y ordenar todo para
  #   llevarse todo tampoco es muestrear.
  #
  # Estaba guardado rama por rama y de tres se guardaron dos: la de DuckDB se
  # paso por alto y contra un motor real daba error de sintaxis. Guardar caso por
  # caso es exactamente como se olvida uno, y ademas deja la siguiente rama que
  # se agregue sin proteger. Por eso vive aca y no adentro.
  if (!is.finite(muestra)) return(NULL)
  if (identical(candidato$tipo, "tablesample_filas")) {
    return(list(
      sql = paste0(
        "SELECT ", paste(campos_sql, collapse = ", "), " FROM ",
        candidato$constructor(tabla_sql, muestra)
      ),
      filas = -1L,
      metodo = candidato$nombre,
      descripcion = candidato$descripcion,
      funcion = NA_character_
    ))
  }
  if (identical(candidato$tipo, "tablesample")) {
    base <- paste0(
      "SELECT ", paste(campos_sql, collapse = ", "), " FROM ",
      candidato$constructor(tabla_sql, porcentaje)
    )
    acotada <- dialecto$limitar(base, muestra, 0)
    return(list(
      sql = if (is.null(acotada)) base else acotada,
      filas = -1L,
      metodo = candidato$nombre,
      descripcion = candidato$descripcion,
      funcion = NA_character_
    ))
  }
  funcion <- candidato$funciones[[1L]]
  base <- paste0(
    "SELECT ", paste(campos_sql, collapse = ", "), " FROM ", tabla_sql,
    " ORDER BY ", funcion$sql
  )
  acotada <- dialecto$limitar(base, muestra, 0)
  if (is.null(acotada)) return(NULL)
  list(
    sql = acotada, filas = -1L, metodo = candidato$nombre,
    descripcion = candidato$descripcion,
    funcion = funcion$nombre
  )
}

.estado_forma_muestreo_dbi <- function(candidato, tabla_sql, campos_sql,
                                       muestra, dialecto) {
  if (is.null(candidato)) {
    return(list(
      forma_construible = FALSE,
      motivo = "capacidad_no_aceptada:sin_forma_muestreo"
    ))
  }
  forma <- tryCatch(
    # `porcentaje = "1"` sólo permite construir la sentencia. No se ejecuta:
    # el plan usa esta comprobación para detectar, entre otros casos, que
    # `muestra = Inf` no tiene una forma de subconjunto que escribir.
    .forma_muestreo_dbi(
      candidato, tabla_sql, campos_sql, porcentaje = "1", muestra, dialecto
    ),
    error = function(e) e
  )
  if (inherits(forma, "condition")) {
    return(list(
      forma_construible = FALSE,
      motivo = "capacidad_no_aceptada:sonda_muestreo"
    ))
  }
  if (is.null(forma)) {
    return(list(
      forma_construible = FALSE,
      motivo = "capacidad_no_aceptada:sonda_muestreo"
    ))
  }
  list(forma_construible = TRUE, motivo = NA_character_)
}

.sondar_muestreo_dbi <- function(conexion, tabla_sql, dialecto, presupuesto) {
  candidatos <- .candidatos_muestreo_dbi(conexion, dialecto)
  if (!length(candidatos)) {
    return(list(
      disponible = FALSE, candidato = NULL, sondas = character(),
      motivo = "capacidad_no_aceptada:sin_forma_muestreo"
    ))
  }
  alias <- as.character(DBI::dbQuoteIdentifier(conexion, "lupa_sonda"))
  sondas <- character()
  aceptada <- NULL
  for (candidato in candidatos) {
    if (identical(candidato$tipo, "tablesample_filas")) {
      sql <- paste0(
        "SELECT 1 AS ", alias, " FROM ", candidato$constructor(tabla_sql, 1L)
      )
    } else if (identical(candidato$tipo, "tablesample")) {
      # Sin `WHERE 1 = 0`, y la razon vale la pena: DuckDB acepta
      # `TABLESAMPLE SYSTEM (10) WHERE 1 = 0` y rechaza la misma clausula sin el
      # filtro, porque con un filtro trivialmente falso no llega a validar el
      # metodo de muestreo. La sonda pasaba y la consulta real fallaba: una
      # sonda que no ejercita la forma que despues se emite no prueba nada. El
      # recorte lo pone el propio muestreo mas el limite del dialecto, asi que
      # sigue siendo barata.
      tabla_sondeada <- candidato$constructor(tabla_sql, "1")
      sql <- paste0("SELECT 1 AS ", alias, " FROM ", tabla_sondeada)
      acotada <- dialecto$limitar(sql, 1L, 0)
      if (!is.null(acotada)) sql <- acotada
    } else {
      funciones <- candidato$funciones
      for (funcion in funciones) {
        sql <- paste0(
          "SELECT 1 AS ", alias, " FROM ", tabla_sql,
          " WHERE 1 = 0 ORDER BY ", funcion$sql
        )
        acotada <- dialecto$limitar(sql, 1L, 0)
        if (!is.null(acotada)) sql <- acotada
        sondas <- c(sondas, sql)
        prueba <- .consultar_dbi(
          conexion, sql, presupuesto, etapa = "sonda_muestreo"
        )
        if (is.null(aceptada) && isTRUE(prueba$ok) && !is.null(acotada)) {
          aceptada <- candidato
          aceptada$funciones <- list(funcion)
        }
      }
      next
    }
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_muestreo"
    )
    if (is.null(aceptada) && isTRUE(prueba$ok)) aceptada <- candidato
  }
  if (is.null(aceptada)) {
    return(list(
      disponible = FALSE, candidato = NULL, sondas = sondas,
      motivo = "capacidad_no_aceptada:sonda_muestreo"
    ))
  }
  list(
    disponible = TRUE, candidato = aceptada, sondas = sondas,
    motivo = NA_character_
  )
}

.fraccion_muestreo_dbi <- function(muestra, n_total) {
  total <- .numero_dbi(n_total)
  if (!is.finite(total) || total <= 0) return(1)
  min(1, max(.Machine$double.eps, muestra / total))
}

.fuente_muestreada_dbi <- function(tabla_sql, campos_sql, muestra, n_total,
                                   dialecto, resolucion) {
  fraccion <- .fraccion_muestreo_dbi(muestra, n_total)
  porcentaje <- formatC(fraccion * 100, format = "fg", digits = 8)
  forma <- .forma_muestreo_dbi(
    resolucion$candidato, tabla_sql, campos_sql, porcentaje, muestra,
    dialecto
  )
  if (is.null(forma)) return(NULL)
  forma$fraccion <- fraccion
  # Lo pedido y lo que la tabla puede dar no son lo mismo. Pedir mil filas de
  # una tabla de diez devuelve diez, y extrapolar dividiendo por mil hundia
  # todos los conteos: `n_validos` caia a cero sobre una columna llena y
  # disparaba `sin_valores`. El tamano efectivo es el que se informa y el que
  # divide.
  total <- .numero_dbi(n_total)
  efectivas <- if (is.finite(total)) min(as.numeric(muestra), total) else {
    as.numeric(muestra)
  }
  forma$filas_solicitadas <- efectivas
  forma$filas_pedidas <- as.numeric(muestra)
  forma
}

.motivo_exito_muestreo_dbi <- function(forma) {
  if (is.null(forma) || is.null(forma$metodo)) return(NA_character_)
  if (identical(forma$metodo, "tablesample_system")) {
    return("sesgo_muestreo:tablesample_system_por_bloques")
  }
  if (identical(forma$metodo, "random_limit") &&
      identical(forma$funcion, "newid")) {
    return("sesgo_muestreo:random_limit_newid_por_fila")
  }
  if (identical(forma$metodo, "random_limit")) {
    return("sesgo_muestreo:random_limit_por_fila")
  }
  paste0("muestreo_aceptado:", forma$metodo)
}

# La mediana exacta puede conservar el limite y el salto sin llevar el orden
# completo a R, pero el conteo que los calcula tiene que vivir en la misma
# sentencia. `%` es el operador que comparten SQLite y PostgreSQL; `/` conserva
# division entera cuando ambos operandos son los conteos enteros de esos
# motores. La sonda de abajo impide extender esa suposicion a otro motor.
.candidatos_mediana_escalar_dbi <- function(conexion, dialecto) {
  forma <- dialecto$mediana_escalar
  if (is.null(forma)) return(list())
  valor <- as.character(DBI::dbQuoteIdentifier(conexion, "valor"))
  construir <- function(expr, tabla, alias, materializar = FALSE) {
    if (isTRUE(materializar)) {
      fuente <- "lupa_mediana_datos"
      cuenta <- "COUNT(*)"
      cuerpo <- paste0(
        "WITH ", fuente, " AS (SELECT ", expr, " AS ", valor,
        " FROM ", tabla, " WHERE ", expr, " IS NOT NULL) ",
        "SELECT AVG(", valor, " * 1.0) AS ", alias,
        " FROM (SELECT ", valor, " FROM ", fuente,
        " ORDER BY ", valor,
        " LIMIT 2 - (SELECT ", cuenta, " ", forma$resto, " 2 FROM ",
        fuente, ")",
        " OFFSET (SELECT (", cuenta, " - 1) ", forma$division,
        " 2 FROM ", fuente, "))", dialecto$alias_tabla("lupa_mediana")
      )
      return(cuerpo)
    }
    cuenta <- paste0("COUNT(", expr, ")")
    paste0(
      "SELECT AVG(", valor, " * 1.0) AS ", alias,
      " FROM (SELECT ", expr, " AS ", valor, " FROM ", tabla,
      " WHERE ", expr, " IS NOT NULL ORDER BY ", expr,
      " LIMIT 2 - (SELECT ", cuenta, " ", forma$resto, " 2 FROM ",
      tabla, ")",
      " OFFSET (SELECT (", cuenta, " - 1) ", forma$division,
      " 2 FROM ", tabla, "))", dialecto$alias_tabla("lupa_mediana")
    )
  }
  list(list(
    nombre = "subconsulta_escalar",
    construir = construir,
    sonda = function(alias, materializar = FALSE) {
      tabla <- paste0(
        "(SELECT 1 AS lupa_valor UNION ALL SELECT 2 AS lupa_valor",
        " UNION ALL SELECT 3 AS lupa_valor UNION ALL SELECT 4 AS lupa_valor)",
        " lupa_mediana_sonda"
      )
      construir('"lupa_valor"', tabla, alias, materializar = materializar)
    },
    error_esperado = "no_aplica"
  ))
}

.sondar_mediana_escalar_dbi <- function(conexion, dialecto, presupuesto,
                                        materializar = FALSE) {
  candidatos <- .candidatos_mediana_escalar_dbi(conexion, dialecto)
  sondas <- character()
  elegida <- NULL
  for (candidato in candidatos) {
    alias <- as.character(DBI::dbQuoteIdentifier(conexion, "mediana"))
    sql <- candidato$sonda(alias, materializar = materializar)
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_mediana_escalar"
    )
    if (!isTRUE(prueba$ok)) next
    celda <- .valor_campo_dbi(prueba$datos, "mediana")
    if (!isTRUE(celda$ok)) next
    valor <- .escalar_finito_dbi(celda$valor)
    if (isTRUE(is.finite(valor)) &&
        isTRUE(all.equal(valor, 2.5, tolerance = 1e-8))) {
      elegida <- candidato
      break
    }
  }
  list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    motivo = if (is.null(elegida)) {
      if (!length(candidatos)) {
        paste0(
          "El dialecto `", dialecto$nombre,
          "` no declara una forma de mediana con subconsulta escalar; se",
          " conserva la via de dos consultas."
        )
      } else {
        paste(
          "El motor no acepto la mediana con subconsulta escalar o no",
          "conservo la division entera esperada; se conserva la via de dos",
          "consultas."
        )
      }
    } else {
      "El motor acepto la mediana con subconsulta escalar y division entera."
    }
  )
}

.publicar_mediana_escalar_dbi <- function(resolucion) {
  if (is.null(resolucion)) return(NULL)
  candidato <- resolucion$candidato
  list(
    disponible = isTRUE(resolucion$disponible),
    metodo = if (is.null(candidato)) NA_character_ else candidato$nombre,
    sondas = resolucion$sondas,
    motivo = resolucion$motivo
  )
}

# La forma de ventanas mantiene la relacion muestreada en una sola referencia:
# el conteo y las posiciones se calculan sobre la misma CTE que luego agrega las
# dos posiciones centrales. Se reserva para los dialectos que no aceptan la
# forma escalar ni una consolidada; en particular es la salida de
# `muestra_motor` en SQL Server con compatibilidad antigua.
.candidatos_mediana_cte_ventana_dbi <- function(conexion) {
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  construir <- function(expr, tabla, alias_salida) {
    valor <- alias("valor")
    paste0(
      "WITH lupa_mediana_datos AS (",
      "SELECT ", expr, " AS ", valor, " FROM ", tabla,
      " WHERE ", expr, " IS NOT NULL), ",
      "ordenada AS (SELECT ", valor, ", COUNT(*) OVER () AS ",
      alias("n_validos"), ", ROW_NUMBER() OVER (ORDER BY ", valor,
      ") AS ", alias("posicion"), " FROM lupa_mediana_datos) ",
      "SELECT AVG(", valor, " * 1.0) AS ", alias_salida,
      " FROM ordenada WHERE ", alias("posicion"),
      " IN ((", alias("n_validos"), " + 1) / 2, (", alias("n_validos"),
      " + 2) / 2)"
    )
  }
  list(list(
    nombre = "cte_ventana",
    construir = construir,
    sonda = function(alias_salida) {
      construir(
        "v", paste0(
          "(VALUES (1.0), (2.0), (3.0), (4.0)) AS lupa_sonda(v)"
        ), alias_salida
      )
    },
    error_esperado = "no_aplica"
  ))
}

.nombre_mediana_cte_muestra_dbi <- function(muestreo) {
  if (is.null(muestreo)) return("cte_ventana")
  metodo <- if (!is.null(muestreo$metodo)) muestreo$metodo else
    if (!is.null(muestreo$candidato)) muestreo$candidato$nombre else NA_character_
  funcion <- if (!is.null(muestreo$funcion)) muestreo$funcion else {
    funciones <- if (is.null(muestreo$candidato)) NULL else
      muestreo$candidato$funciones
    if (length(funciones)) funciones[[1L]]$nombre else NA_character_
  }
  if (identical(metodo, "tablesample_system")) {
    return("cte_ventana_tablesample_system")
  }
  if (identical(metodo, "random_limit") && identical(funcion, "newid")) {
    return("cte_ventana_newid")
  }
  "cte_ventana"
}

.sondar_mediana_cte_ventana_dbi <- function(conexion, presupuesto) {
  if (!is.null(presupuesto) && !is.null(presupuesto$mediana_cte_ventana)) {
    return(presupuesto$mediana_cte_ventana)
  }
  candidatos <- .candidatos_mediana_cte_ventana_dbi(conexion)
  alias <- as.character(DBI::dbQuoteIdentifier(conexion, "mediana"))
  sondas <- character()
  elegida <- NULL
  control_negativo_ok <- FALSE
  for (candidato in candidatos) {
    sql <- candidato$sonda(alias)
    sondas <- c(sondas, sql)
    positiva <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_mediana_cte_ventana"
    )
    celda <- if (isTRUE(positiva$ok)) {
      .valor_campo_dbi(positiva$datos, "mediana")
    } else {
      list(ok = FALSE, valor = NA_real_)
    }
    valor <- if (isTRUE(celda$ok)) .escalar_finito_dbi(celda$valor) else NA_real_
    valor_ok <- isTRUE(is.finite(valor)) &&
      isTRUE(all.equal(valor, 2.5, tolerance = 1e-8))
    sql_negativa <- sub(
      "AVG\\(", "FUNCION_INVALIDA_LUPA(", sql, fixed = FALSE
    )
    sondas <- c(sondas, sql_negativa)
    negativa <- .consultar_dbi(
      conexion, sql_negativa, presupuesto,
      etapa = "sonda_mediana_cte_ventana_control_negativo"
    )
    control_negativo_ok <- !isTRUE(negativa$ok)
    if (is.null(elegida) && valor_ok && control_negativo_ok) {
      elegida <- candidato
    }
  }
  resultado <- list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    control_negativo = control_negativo_ok,
    motivo = if (is.null(elegida)) {
      "capacidad_no_aceptada:sonda_mediana_cte_ventana"
    } else {
      "La sonda de la CTE de ventanas devolvio 2,5 y su control negativo fallo."
    }
  )
  if (!is.null(presupuesto)) presupuesto$mediana_cte_ventana <- resultado
  resultado
}

# La frecuencia de la moda y su denominador pueden salir de la misma
# agregacion: la ventana se aplica sobre los grupos que ya produjo GROUP BY.
# La sonda usa una tabla chica para no asumir que todos los motores aceptan
# una ventana sobre un agregado.
.candidatos_moda_guardian_dbi <- function(conexion, dialecto) {
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  construir <- function(columna, tabla) {
    sin_limite <- paste0(
      "SELECT ", columna, " AS ", alias("valor"), ", COUNT(*) AS ",
      alias("frecuencia"), ", SUM(COUNT(*)) OVER () AS ",
      alias("n_validos_guard"), " FROM ", tabla, " WHERE ", columna,
      " IS NOT NULL GROUP BY ", columna, " ORDER BY ", alias("frecuencia"),
      " DESC, ", columna, " ASC"
    )
    acotada <- dialecto$limitar(sin_limite, 1L, 0)
    if (is.null(acotada)) sin_limite else acotada
  }
  list(list(
    nombre = "ventana_agregado",
    construir = construir,
    sonda = function() {
      tabla <- paste0(
        "(SELECT 1 AS lupa_valor UNION ALL SELECT 2 UNION ALL SELECT 2)",
        dialecto$alias_tabla("lupa_moda_sonda")
      )
      construir(alias("lupa_valor"), tabla)
    }
  ))
}

.sondar_moda_guardian_dbi <- function(conexion, dialecto, presupuesto) {
  if (!is.null(presupuesto) && !is.null(presupuesto$moda_guardian)) {
    return(presupuesto$moda_guardian)
  }
  candidatos <- .candidatos_moda_guardian_dbi(conexion, dialecto)
  sondas <- character()
  elegida <- NULL
  for (candidato in candidatos) {
    sql <- candidato$sonda()
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(
      conexion, sql, presupuesto, filas = 1L,
      etapa = "sonda_moda_guardian"
    )
    if (!isTRUE(prueba$ok)) next
    valor <- .valor_campo_dbi(prueba$datos, "valor")
    frecuencia <- .valor_campo_dbi(prueba$datos, "frecuencia")
    guardian <- .valor_campo_dbi(prueba$datos, "n_validos_guard")
    if (!isTRUE(valor$ok) || !isTRUE(frecuencia$ok) ||
        !isTRUE(guardian$ok)) next
    if (isTRUE(.numero_dbi(valor$valor) == 2) &&
        isTRUE(.numero_dbi(frecuencia$valor) == 2) &&
        isTRUE(.numero_dbi(guardian$valor) == 3)) {
      elegida <- candidato
      break
    }
  }
  resultado <- list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    motivo = if (is.null(elegida)) {
      if (!length(candidatos)) {
        "El adaptador no declara una forma de moda con guardian; se conserva la consulta actual sin guardian."
      } else {
        paste(
          "El motor rechazo la forma de moda con guardian o no devolvio el",
          "resultado esperado; se conserva la consulta actual sin guardian."
        )
      }
    } else {
      paste(
        "El motor acepto la forma de moda con guardian; la cota se comprueba",
        "dentro de la misma sentencia."
      )
    }
  )
  if (!is.null(presupuesto)) presupuesto$moda_guardian <- resultado
  resultado
}

.publicar_moda_guardian_dbi <- function(resolucion) {
  if (is.null(resolucion)) return(NULL)
  candidato <- resolucion$candidato
  list(
    disponible = isTRUE(resolucion$disponible),
    metodo = if (is.null(candidato)) NA_character_ else candidato$nombre,
    sondas = resolucion$sondas,
    motivo = resolucion$motivo
  )
}

# Las sondas ordenan por la COLUMNA de la subconsulta y no por una constante.
# Ordenar por `1.0` parecia lo mas neutral y hacia que dos caminos declarados no
# se ejercitaran NUNCA, cada uno por su motivo, y sin que se notara: la sonda
# fallaba, el paquete degradaba a la via por columna y publicaba valores
# correctos. Medido el 2026-08-30 contra los motores reales:
#
#   - SQL Server rechaza una constante en el `ORDER BY` de una funcion de
#     ventana -"Windowed functions... do not support constants as ORDER BY
#     clause expressions"-, asi que `PERCENTILE_CONT_OVER` no se activaba jamas.
#   - MariaDB 11.8 implementa `PERCENTILE_CONT` SOLO como funcion de ventana,
#     y estaba clasificada con el candidato que no lleva `OVER`: su sonda
#     fallaba por la sintaxis, no por la constante. Pasa al candidato con
#     ventana, donde acepta la consolidada y varias expresiones a la vez.
#
# Una sonda tiene que parecerse a la consulta que habilita. La que ordenaba por
# una constante probaba una forma que el paquete no emite nunca.
#
# Algunos motores pueden obtener varios percentiles en la misma agregacion.
# Esta capacidad se sondea por separado porque una sonda que solo prueba una
# mediana no prueba que el motor acepte varias expresiones en un SELECT.
.candidatos_mediana_consolidada_dbi <- function(conexion) {
  senas <- .senas_conexion_dbi(conexion)
  candidatos <- list(
    list(
      nombre = "PERCENTILE_CONT",
      patron = "postgres|redshift|oracle|snowflake|duckdb",
      exacta = TRUE,
      error_esperado = "desconocido",
      construir = function(expr, tabla, alias) paste0(
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
        ") AS ", alias, " FROM ", tabla
      ),
      construir_multiple = function(expresiones, tabla) paste0(
        "SELECT ", paste(expresiones, collapse = ", "), " FROM ", tabla
      ),
      expresion = function(expr, alias) paste0(
        "PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
        ") AS ", alias
      ),
      sonda = function(alias) paste0(
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY lupa_valor) AS ",
        alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
      )
    ),
    list(
      nombre = "PERCENTILE_CONT_OVER",
      patron = "sql server|microsoft sql|sqlserver|mssql|mariadb",
      exacta = TRUE,
      error_esperado = "desconocido",
      construir = function(expr, tabla, alias) paste0(
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
        ") OVER () AS ", alias, " FROM ", tabla
      ),
      construir_multiple = function(expresiones, tabla) paste0(
        "SELECT DISTINCT ", paste(expresiones, collapse = ", "),
        " FROM ", tabla
      ),
      expresion = function(expr, alias) paste0(
        "PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
        ") OVER () AS ", alias
      ),
      sonda = function(alias) paste0(
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY lupa_valor) OVER () AS ",
        alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
      )
    )
  )
  reconocidos <- vapply(candidatos, function(x) {
    !is.na(x$patron) && grepl(x$patron, senas, perl = TRUE)
  }, logical(1L))
  # La consolidacion solo tiene implementaciones declaradas para estos motores.
  # En un motor sin senas no se adivina una sintaxis ni se paga una sonda que no
  # puede justificar el camino que se emitira: se conserva la mediana actual por
  # columna. Los motores reconocidos se prueban, como las demas capacidades.
  candidatos[which(reconocidos)]
}

.sondar_mediana_consolidada_dbi <- function(conexion, presupuesto) {
  if (!is.null(presupuesto$mediana_consolidada)) {
    return(presupuesto$mediana_consolidada)
  }
  alias <- as.character(DBI::dbQuoteIdentifier(conexion, "lupa_sonda"))
  candidatos <- .candidatos_mediana_consolidada_dbi(conexion)
  sondas <- character()
  motivos_motor <- character()
  elegida <- NULL
  for (candidato in candidatos) {
    sql <- candidato$sonda(alias)
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_mediana_consolidada"
    )
    if (is.null(elegida) && isTRUE(prueba$ok)) elegida <- candidato
    if (!isTRUE(prueba$ok)) {
      motivo <- if (is.null(prueba$motivo) || !length(prueba$motivo) ||
                    all(is.na(prueba$motivo))) {
        ""
      } else {
        .resumir_valor_reporte(prueba$motivo[[1L]], max_caracteres = 240L)
      }
      if (nzchar(motivo)) {
        motivos_motor <- c(
          motivos_motor,
          paste0("`", candidato$nombre, "`: ", motivo)
        )
      }
    }
  }
  motivo_sin_candidato <- if (!length(candidatos)) {
    "No hay una forma consolidada declarada para este motor; se conserva la mediana por columna."
  } else {
    "El motor no acepto una consulta consolidada de `PERCENTILE_CONT`; se conserva la mediana por columna."
  }
  if (length(motivos_motor)) {
    motivo_sin_candidato <- paste0(
      motivo_sin_candidato,
      " Motivo del motor: ",
      paste(unique(motivos_motor), collapse = "; ")
    )
  }
  resultado <- list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    motivo = if (is.null(elegida)) {
      motivo_sin_candidato
    } else {
      paste0("El motor acepto `", elegida$nombre,
             "` para consolidar medianas.")
    }
  )
  presupuesto$mediana_consolidada <- resultado
  resultado
}

.candidatos_aproximacion_dbi <- function(conexion, tipo) {
  senas <- .senas_conexion_dbi(conexion)
  if (identical(tipo, "distintos")) {
    candidatos <- list(
      list(
        nombre = "APPROX_COUNT_DISTINCT",
        patron = "sql server|microsoft sql|sqlserver|mssql",
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT APPROX_COUNT_DISTINCT(", expr, ") AS ", alias,
          " FROM ", tabla
        ),
        expresion = function(expr, alias) paste0(
          "APPROX_COUNT_DISTINCT(", expr, ") AS ", alias
        ),
        sonda = function(alias) paste0(
          "SELECT APPROX_COUNT_DISTINCT(1) AS ", alias
        )
      ),
      list(
        nombre = "approx_count_distinct",
        patron = "spark|databricks|hive",
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT approx_count_distinct(", expr, ") AS ", alias,
          " FROM ", tabla
        ),
        expresion = function(expr, alias) paste0(
          "approx_count_distinct(", expr, ") AS ", alias
        ),
        sonda = function(alias) paste0(
          "SELECT approx_count_distinct(1) AS ", alias
        )
      ),
      list(
        nombre = "approx_count_distinct_generico",
        patron = NA_character_,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT APPROX_COUNT_DISTINCT(", expr, ") AS ", alias,
          " FROM ", tabla
        ),
        expresion = function(expr, alias) paste0(
          "APPROX_COUNT_DISTINCT(", expr, ") AS ", alias
        ),
        sonda = function(alias) paste0(
          "SELECT APPROX_COUNT_DISTINCT(1) AS ", alias
        )
      )
    )
  } else {
    candidatos <- list(
      list(
        nombre = "PERCENTILE_CONT",
        patron = "postgres|redshift|duckdb|sql server|microsoft sql|sqlserver|mssql",
        exacta = TRUE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
          ") AS ", alias, " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY lupa_valor) AS ",
          alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
        )
      ),
      list(
        nombre = "PERCENTILE_CONT_OVER",
        patron = "sql server|microsoft sql|sqlserver|mssql|mariadb",
        exacta = TRUE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
          ") OVER () AS ", alias, " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY lupa_valor) OVER () AS ",
          alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
        )
      ),
      list(
        nombre = "approx_percentile",
        patron = "snowflake|presto|trino",
        exacta = FALSE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT APPROX_PERCENTILE(", expr, ", 0.5) AS ", alias,
          " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT APPROX_PERCENTILE(1.0, 0.5) AS ", alias
        )
      ),
      list(
        nombre = "approx_quantile",
        patron = "duckdb",
        exacta = FALSE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT approx_quantile(", expr, ", 0.5) AS ", alias,
          " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT approx_quantile(1.0, 0.5) AS ", alias
        )
      ),
      list(
        nombre = "percentile_approx",
        patron = "spark|databricks|hive",
        exacta = FALSE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT percentile_approx(", expr, ", 0.5) AS ", alias,
          " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT percentile_approx(1.0, 0.5) AS ", alias
        )
      ),
      list(
        nombre = "quantile",
        patron = "clickhouse",
        exacta = FALSE,
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT quantile(0.5)(", expr, ") AS ", alias,
          " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT quantile(0.5)(1.0) AS ", alias
        )
      )
    )
  }
  reconocidos <- vapply(candidatos, function(x) {
    !is.na(x$patron) && grepl(x$patron, senas, perl = TRUE)
  }, logical(1L))
  candidatos[c(which(reconocidos), which(!reconocidos))]
}

.sondar_aproximacion_dbi <- function(conexion, tipo, presupuesto) {
  candidatos <- .candidatos_aproximacion_dbi(conexion, tipo)
  alias <- as.character(DBI::dbQuoteIdentifier(conexion, "lupa_sonda"))
  sondas <- character()
  elegida <- NULL
  for (candidato in candidatos) {
    sql <- candidato$sonda(alias)
    if (.es_oracle_dbi(conexion) &&
        !grepl("\\bFROM\\b", sql, ignore.case = TRUE, perl = TRUE)) {
      sql <- paste0(sql, " FROM DUAL")
    }
    sondas <- c(sondas, sql)
    resultado <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_aproximacion"
    )
    if (is.null(elegida) && isTRUE(resultado$ok)) elegida <- candidato
  }
  list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    motivo = if (is.null(elegida)) {
      paste0("El motor no acepto una funcion aproximada para `", tipo, "`.")
    } else {
      paste0("El motor acepto `", elegida$nombre, "` para `", tipo, "`.")
    }
  )
}

.publicar_muestreo_dbi <- function(resolucion, forma = NULL, n_total = NA,
                                   muestreo_meta = NULL) {
  candidato <- resolucion$candidato
  motivo_exito <- .motivo_exito_muestreo_dbi(forma)
  numero <- function(objeto, nombre) {
    if (is.null(objeto) || is.null(objeto[[nombre]]) ||
        !length(objeto[[nombre]])) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(objeto[[nombre]][[1L]]))
  }
  list(
    disponible = isTRUE(resolucion$disponible),
    metodo = if (is.null(forma)) {
      if (is.null(candidato)) NA_character_ else candidato$nombre
    } else forma$metodo,
    # `metodo` conserva el nombre historico de la capacidad. Estos campos
    # explicitos hacen auditable la distincion entre TABLESAMPLE y el fallback
    # fila a fila; en particular, un NEWID aceptado no queda como un random
    # anonimo ni pierde el motivo de su aceptacion.
    metodo_muestreo = if (is.null(forma)) {
      if (is.null(candidato)) NA_character_ else candidato$nombre
    } else forma$metodo,
    funcion_muestreo = if (is.null(forma)) {
      funciones <- if (is.null(candidato)) NULL else candidato$funciones
      if (length(funciones)) funciones[[1L]]$nombre else NA_character_
    } else forma$funcion,
    sesgo_muestreo = if (identical(motivo_exito,
                                   "sesgo_muestreo:tablesample_system_por_bloques")) {
      "por_bloques"
    } else if (grepl("random_limit", as.character(if (is.null(forma)) {
      NA_character_
    } else forma$metodo), fixed = TRUE)) {
      "por_fila"
    } else NA_character_,
    descripcion = if (is.null(forma)) {
      if (is.null(candidato)) NA_character_ else candidato$descripcion
    } else forma$descripcion,
    fraccion = if (is.null(forma)) NA_real_ else forma$fraccion,
    # `tamano_muestra` es parte del contrato anterior: conserva el tamano
    # efectivo solicitado a la consulta, que puede estar acotado por el
    # universo. Los nombres explicitos de abajo evitan confundirlo con lo que
    # realmente devolvio la lectura.
    tamano_muestra = if (is.null(forma)) {
      numero(muestreo_meta, "tamano_muestra")
    } else {
      numero(forma, "filas_solicitadas")
    },
    filas_solicitadas = if (!is.null(forma) &&
                            !is.null(forma$filas_pedidas)) {
      numero(forma, "filas_pedidas")
    } else {
      numero(muestreo_meta, "filas_solicitadas")
    },
    filas_pedidas = if (!is.null(forma) &&
                        !is.null(forma$filas_pedidas)) {
      numero(forma, "filas_pedidas")
    } else {
      numero(muestreo_meta, "filas_pedidas")
    },
    filas_obtenidas = numero(muestreo_meta, "filas_obtenidas"),
    universo = n_total,
    sondas = resolucion$sondas,
    motivo = if (is.null(forma) || is.na(motivo_exito)) {
      resolucion$motivo
    } else motivo_exito,
    motivo_exito = if (is.null(forma) || is.na(motivo_exito)) {
      NA_character_
    } else motivo_exito,
    motivo_sonda = resolucion$motivo,
    sql = if (is.null(forma)) NA_character_ else forma$sql
  )
}

.publicar_muestreo_plan_dbi <- function(muestreo, muestra) {
  if (is.null(muestreo)) {
    return(list(
      estado = "no_solicitado", disponible = NA, forma_construible = NA,
      metodo = NA_character_, filas_solicitadas = muestra,
      sondas_previstas = 0L,
      motivo = "El plan no solicita una muestra del motor."
    ))
  }
  candidato <- muestreo$candidato
  sondas_previstas <- if (is.null(muestreo$sondas_previstas)) {
    length(muestreo$sondas)
  } else {
    muestreo$sondas_previstas
  }
  list(
    estado = if (!isTRUE(muestreo$disponible)) "no_disponible" else if (
      length(muestreo$sondas)
    ) "disponible" else "no_sondeado",
    disponible = isTRUE(muestreo$disponible),
    forma_construible = isTRUE(muestreo$forma_construible),
    metodo = if (is.null(candidato)) NA_character_ else candidato$nombre,
    filas_solicitadas = muestra,
    sondas_previstas = as.integer(sondas_previstas),
    motivo = muestreo$motivo
  )
}

.publicar_aproximacion_dbi <- function(resolucion) {
  candidato <- resolucion$candidato
  list(
    disponible = isTRUE(resolucion$disponible),
    metodo = if (is.null(candidato)) NA_character_ else candidato$nombre,
    error_esperado = if (is.null(candidato)) NA_character_ else {
      candidato$error_esperado
    },
    sondas = resolucion$sondas,
    motivo = resolucion$motivo
  )
}

.senas_conexion_dbi <- function(conexion) {
  informacion <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) list())
  campos <- c("dbms.name", "driver.name", "driver", "sourcename", "servername")
  textos <- c(
    class(conexion),
    attr(class(conexion), "package", exact = TRUE),
    unlist(informacion[intersect(campos, names(informacion))], use.names = FALSE)
  )
  textos <- as.character(textos)
  tolower(paste(textos[nzchar(textos) & !is.na(textos)], collapse = " "))
}

.es_oracle_dbi <- function(conexion) {
  grepl("oracle", .senas_conexion_dbi(conexion), fixed = TRUE)
}

# El orden de los candidatos sale de las senas del motor, pero la palabra final
# la tiene la sonda: adivinar por el nombre de la clase es una heuristica, y
# una heuristica no puede decidir si una consulta obligatoria se puede armar.
.orden_dialectos_dbi <- function(conexion) {
  dialectos <- .dialectos_dbi()
  senas <- .senas_conexion_dbi(conexion)
  reconocidos <- vapply(dialectos, function(dialecto) {
    !is.na(dialecto$patron) && grepl(dialecto$patron, senas, perl = TRUE)
  }, logical(1L))
  c(names(dialectos)[reconocidos], names(dialectos)[!reconocidos])
}

.resolver_dialecto_dbi <- function(conexion, sql_forma, dialecto, presupuesto) {
  dialectos <- .dialectos_dbi()
  if (!identical(dialecto, "auto")) {
    return(list(
      dialecto = dialectos[[dialecto]], sondas = character(),
      motivo = "Declarado por el usuario en `dialecto`; no se sondeo el motor.",
      declarado = TRUE
    ))
  }
  sondas <- character()
  for (nombre in .orden_dialectos_dbi(conexion)) {
    candidato <- dialectos[[nombre]]
    if (identical(nombre, "portable")) break
    sql <- candidato$limitar(sql_forma, 1L, 0)
    if (is.null(sql)) next
    sondas <- c(sondas, sql)
    resultado <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_dialecto"
    )
    if (resultado$ok) {
      return(list(
        dialecto = candidato, sondas = sondas, declarado = FALSE,
        motivo = paste0(
          "El motor acepto la sonda de cero filas del dialecto `", nombre, "`."
        )
      ))
    }
    if (isTRUE(presupuesto$agotado)) break
  }
  list(
    dialecto = dialectos$portable, sondas = sondas, declarado = FALSE,
    motivo = paste(
      "Ningun dialecto de limite conocido paso la sonda; las filas se acotan",
      "en el cliente con dbSendQuery() y dbFetch(n)."
    )
  )
}

# ---- Consultas -----------------------------------------------------------

.texto_tabla_dbi <- function(tabla) {
  texto <- tryCatch(as.character(tabla), error = function(e) character())
  if (!length(texto)) texto <- tryCatch(format(tabla), error = function(e) "")
  paste(texto, collapse = ".")
}

.info_conexion_dbi <- function(conexion) {
  info <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) {
    list(no_disponible = conditionMessage(e))
  })
  list(
    clase_conexion = class(conexion),
    informacion_dbi = info
  )
}

.es_conexion_dbi <- function(conexion) {
  isTRUE(tryCatch(
    inherits(conexion, "DBIConnection"), error = function(e) FALSE
  ))
}

.conexion_valida_dbi <- function(conexion) {
  tryCatch(
    isTRUE(DBI::dbIsValid(conexion)),
    error = function(e) {
      isTRUE(tryCatch({
        DBI::dbGetInfo(conexion)
        TRUE
      }, error = function(e) FALSE))
    }
  )
}

# `filas >= 0` usa la via portable: el motor prepara el resultado y R lee solo
# las filas pedidas. Es la unica forma de acotar sin clausula de dialecto.
.consultar_dbi <- function(conexion, sql, presupuesto = NULL, filas = -1L,
                           etapa = "consulta") {
  if (!.gastar_dbi(presupuesto)) {
    return(.adjuntar_medicion_dbi(list(
      ok = FALSE, datos = NULL, motivo = .motivo_presupuesto_dbi(presupuesto)
    ), .medicion_consulta_vacia_dbi(etapa)))
  }
  medicion <- .iniciar_consulta_dbi(presupuesto, etapa)
  if (filas < 0) {
    salida <- tryCatch(
      list(ok = TRUE, datos = DBI::dbGetQuery(conexion, sql), motivo = NA_character_),
      error = function(e) {
        list(ok = FALSE, datos = NULL, motivo = conditionMessage(e))
      }
    )
    return(.adjuntar_medicion_dbi(
      salida, .terminar_consulta_dbi(medicion, salida$datos)
    ))
  }
  resultado <- NULL
  on.exit(
    if (!is.null(resultado)) try(DBI::dbClearResult(resultado), silent = TRUE),
    add = TRUE
  )
  salida <- tryCatch({
    resultado <- DBI::dbSendQuery(conexion, sql)
    list(ok = TRUE, datos = DBI::dbFetch(resultado, n = filas), motivo = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, datos = NULL, motivo = conditionMessage(e))
  })
  .adjuntar_medicion_dbi(
    salida, .terminar_consulta_dbi(medicion, salida$datos)
  )
}

.normalizar_sql_derrame_dbi <- function(sql) {
  if (is.null(sql) || length(sql) != 1L || is.na(sql)) return(NA_character_)
  texto <- sub(";+[[:space:]]*$", "", as.character(sql))
  caracteres <- strsplit(texto, "", fixed = TRUE)[[1L]]
  salida <- character()
  i <- 1L
  literal <- 0L
  limite <- length(caracteres)
  es_borde <- function(valor) !grepl("[A-Za-z0-9_$]", valor, perl = TRUE)
  while (i <= limite) {
    actual <- caracteres[[i]]
    if (identical(actual, "\"")) {
      inicio <- i
      i <- i + 1L
      while (i <= limite) {
        if (identical(caracteres[[i]], "\"") &&
            i < limite && identical(caracteres[[i + 1L]], "\"")) {
          i <- i + 2L
        } else if (identical(caracteres[[i]], "\"")) {
          i <- i + 1L
          break
        } else {
          i <- i + 1L
        }
      }
      salida <- c(salida, paste(caracteres[inicio:(i - 1L)], collapse = ""))
      next
    }
    if (identical(actual, "'")) {
      literal <- literal + 1L
      i <- i + 1L
      while (i <= limite) {
        if (identical(caracteres[[i]], "'") && i < limite &&
            identical(caracteres[[i + 1L]], "'")) {
          i <- i + 2L
        } else if (identical(caracteres[[i]], "'")) {
          i <- i + 1L
          break
        } else {
          i <- i + 1L
        }
      }
      salida <- c(salida, paste0("$", literal))
      next
    }
    resto <- paste(caracteres[i:limite], collapse = "")
    anterior <- if (length(salida)) substr(utils::tail(salida, 1L), 1L, 1L) else ""
    patron_numero <- "^(?:[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?)"
    captura <- regmatches(resto, regexpr(patron_numero, resto, perl = TRUE))
    puede_numero <- length(captura) == 1L && nzchar(captura) &&
      (i == 1L || es_borde(anterior))
    if (puede_numero) {
      siguiente <- i + nchar(captura, type = "chars")
      caracter_siguiente <- if (siguiente <= limite) caracteres[[siguiente]] else ""
      if (!nzchar(caracter_siguiente) || es_borde(caracter_siguiente)) {
        literal <- literal + 1L
        salida <- c(salida, paste0("$", literal))
        i <- siguiente
        next
      }
    }
    salida <- c(salida, actual)
    i <- i + 1L
  }
  gsub("[[:space:]]+", " ", trimws(paste(salida, collapse = "")))
}

.estadisticas_derrame_postgresql_dbi <- function(conexion) {
  sql <- paste(
    "SELECT queryid, query, calls, temp_blks_read, temp_blks_written",
    "FROM pg_stat_statements",
    "WHERE query ILIKE '%COUNT(DISTINCT%'",
    "OR query ILIKE '%AS frecuencia%'",
    "OR query ILIKE '%frecuencia%'",
    "OR query ILIKE '%lupa_mediana%'",
    "OR query ILIKE '%PERCENTILE_CONT%'"
  )
  datos <- tryCatch(
    DBI::dbGetQuery(conexion, sql),
    error = function(e) NULL
  )
  if (is.null(datos) || !all(c(
    "queryid", "query", "calls", "temp_blks_read", "temp_blks_written"
  ) %in% names(datos))) {
    return(NULL)
  }
  datos$query_normalizada <- vapply(
    datos$query, .normalizar_sql_derrame_dbi, character(1L)
  )
  datos
}

.iniciar_instrumentacion_derrame_dbi <- function(conexion, presupuesto,
                                                  exacto = TRUE) {
  estado <- list(
    estado = "no_solicitado", disponible = FALSE,
    fuente = NA_character_, motivo = NA_character_, antes = NULL,
    despues = NULL, consultas = NULL
  )
  if (!isTRUE(exacto)) {
    estado$motivo <- paste(
      "La estrategia de distintos no emite `COUNT(DISTINCT)` exacto."
    )
  } else if (is.null(presupuesto) || !isTRUE(presupuesto$instrumentar)) {
    estado$estado <- "no_medido"
    estado$motivo <- paste(
      "La instrumentacion esta apagada; DBI no mide bloques temporales."
    )
  } else if (!grepl("postgres|pqconnection", .senas_conexion_dbi(conexion),
                    ignore.case = TRUE, perl = TRUE)) {
    estado$estado <- "no_disponible"
    estado$motivo <- paste(
      "El controlador no fue reconocido como PostgreSQL; no se puede",
      "consultar una estadistica de bloques temporales portable."
    )
  } else {
    antes <- .estadisticas_derrame_postgresql_dbi(conexion)
    if (is.null(antes)) {
      estado$estado <- "no_disponible"
      estado$motivo <- paste(
        "El servidor no expone `pg_stat_statements` o la credencial no puede",
        "leer sus bloques temporales."
      )
    } else {
      estado$estado <- "observando"
      estado$disponible <- TRUE
      estado$fuente <- "pg_stat_statements"
      estado$antes <- antes
    }
  }
  if (!is.null(presupuesto)) presupuesto$derrame <- estado
  estado
}

.finalizar_instrumentacion_derrame_dbi <- function(conexion, presupuesto) {
  if (is.null(presupuesto)) return(invisible(NULL))
  estado <- presupuesto$derrame
  if (!identical(estado$estado, "observando")) return(invisible(NULL))
  despues <- .estadisticas_derrame_postgresql_dbi(conexion)
  if (is.null(despues)) {
    estado$estado <- "no_disponible"
    estado$disponible <- FALSE
    estado$motivo <- paste(
      "La lectura final de `pg_stat_statements` fallo; no se publica el",
      "derrame porque no se puede separar esta corrida."
    )
    presupuesto$derrame <- estado
    return(invisible(NULL))
  }
  estado$despues <- despues
  antes <- estado$antes
  consultas <- list()
  tiene_queryid <- all(c("queryid") %in% names(antes)) &&
    all(c("queryid") %in% names(despues))
  clave_queryid <- function(x) {
    valor <- tryCatch(as.character(x), error = function(e) NA_character_)
    if (length(valor) != 1L || is.na(valor) || !nzchar(valor)) NA_character_ else valor
  }
  duplicados_texto <- function(datos) {
    claves <- datos$query_normalizada
    any(!is.na(claves) & nzchar(claves) & duplicated(claves))
  }
  duplicados_queryid <- function(datos) {
    claves <- vapply(datos$queryid, clave_queryid, character(1L))
    any(!is.na(claves) & duplicated(claves))
  }
  hay_ambiguedad_sin_queryid <- !tiene_queryid &&
    (duplicados_texto(antes) || duplicados_texto(despues))
  hay_ambiguedad_queryid <- tiene_queryid &&
    (duplicados_queryid(antes) || duplicados_queryid(despues))
  for (i in seq_len(nrow(despues))) {
    clave <- despues$query_normalizada[[i]]
    if (is.na(clave) || !nzchar(clave)) next
    queryid <- if (tiene_queryid) clave_queryid(despues$queryid[[i]]) else NA_character_
    indices_previos <- if (tiene_queryid && !is.na(queryid) &&
                            !hay_ambiguedad_queryid) {
      previos <- vapply(antes$queryid, clave_queryid, character(1L))
      which(!is.na(previos) & previos == queryid)
    } else if (!tiene_queryid && !hay_ambiguedad_sin_queryid) {
      which(!is.na(antes$query_normalizada) & antes$query_normalizada == clave)
    } else integer()
    previo <- if (length(indices_previos) == 1L) {
      antes[indices_previos, , drop = FALSE]
    } else antes[FALSE, , drop = FALSE]
    llamadas_antes <- if (nrow(previo)) .numero_dbi(previo$calls[[1L]]) else 0
    llamadas_despues <- .numero_dbi(despues$calls[[i]])
    leidos_antes <- if (nrow(previo)) .numero_dbi(previo$temp_blks_read[[1L]]) else 0
    escritos_antes <- if (nrow(previo)) .numero_dbi(previo$temp_blks_written[[1L]]) else 0
    delta_llamadas <- llamadas_despues - llamadas_antes
    delta_leidos <- .numero_dbi(despues$temp_blks_read[[i]]) - leidos_antes
    delta_escritos <- .numero_dbi(despues$temp_blks_written[[i]]) - escritos_antes
    # El contador puede incluir otra sesión o llamada concurrente. Se publica
    # el agregado de la ventana y ese límite de atribución queda explícito.
    if (!isTRUE(delta_llamadas >= 1) || !is.finite(delta_leidos) ||
        !is.finite(delta_escritos) || delta_leidos < 0 || delta_escritos < 0) {
      next
    }
    consultas[[length(consultas) + 1L]] <- list(
      query_normalizada = clave, derrame = delta_leidos > 0 || delta_escritos > 0,
      bloques_temporales_leidos = delta_leidos,
      bloques_temporales_escritos = delta_escritos,
      llamadas_en_ventana = delta_llamadas
    )
  }
  if (!length(consultas)) {
    estado$estado <- "no_disponible"
    estado$disponible <- FALSE
    estado$motivo <- if (hay_ambiguedad_sin_queryid || hay_ambiguedad_queryid) {
      paste(
        "`pg_stat_statements` tiene varias entradas para el mismo texto SQL",
        "normalizado y no se pudo atribuir cada fila por `queryid`; no se",
        "publica el derrame."
      )
    } else {
      paste(
        "`pg_stat_statements` no devolvio una pareja atribuible por `queryid`",
        "para esta corrida; no se publica el derrame."
      )
    }
  } else {
    estado$estado <- "medido"
    estado$consultas <- consultas
    llamadas <- sum(vapply(consultas, function(x) x$llamadas_en_ventana,
                           numeric(1L)))
    estado$motivo <- paste(
      "Los bloques temporales se publican como agregado sobre el texto SQL",
      "normalizado; `llamadas_en_ventana` =", llamadas,
      "y puede incluir otra sesion o llamada concurrente."
    )
  }
  presupuesto$derrame <- estado
  invisible(NULL)
}

.publicar_derrame_dbi <- function(presupuesto) {
  estado <- if (is.null(presupuesto)) NULL else presupuesto$derrame
  if (is.null(estado)) {
    return(list(
      disponible = FALSE, estado = "no_disponible", fuente = NA_character_,
      motivo = "No se pudo iniciar la instrumentacion de derrame.",
      consultas_observadas = 0L, consultas_con_derrame = 0L,
      bloques_temporales_leidos = NA_real_,
      bloques_temporales_escritos = NA_real_, llamadas_en_ventana = NA_real_
    ))
  }
  consultas <- estado$consultas
  if (is.null(consultas)) consultas <- list()
  derrames <- vapply(consultas, function(x) isTRUE(x$derrame), logical(1L))
  leidos <- if (length(consultas)) sum(vapply(
    consultas, function(x) x$bloques_temporales_leidos, numeric(1L)
  )) else NA_real_
  escritos <- if (length(consultas)) sum(vapply(
    consultas, function(x) x$bloques_temporales_escritos, numeric(1L)
  )) else NA_real_
  llamadas <- if (length(consultas)) sum(vapply(
    consultas, function(x) x$llamadas_en_ventana, numeric(1L)
  )) else NA_real_
  list(
    disponible = identical(estado$estado, "medido"),
    estado = estado$estado,
    fuente = estado$fuente,
    motivo = estado$motivo,
    consultas_observadas = as.integer(length(consultas)),
    consultas_con_derrame = as.integer(sum(derrames)),
    bloques_temporales_leidos = leidos,
    bloques_temporales_escritos = escritos,
    llamadas_en_ventana = llamadas
  )
}

# Oracle, DB2, Firebird y Snowflake pliegan a mayusculas los identificadores.
# El alias va comillado para que el motor lo respete, y la comparacion se hace
# sin distinguir caja para que un motor que lo pliegue igual no invente un
# motivo falso.
.campo_resultado_dbi <- function(datos, campo) {
  nombres <- names(datos)
  if (!length(nombres)) return(NA_integer_)
  posicion <- match(campo, nombres)
  if (is.na(posicion)) posicion <- match(tolower(campo), tolower(nombres))
  posicion
}

.valor_campo_dbi <- function(datos, campo) {
  if (is.null(datos) || !nrow(datos)) {
    return(list(
      ok = FALSE, valor = NULL,
      motivo = "La consulta no devolvio ninguna fila."
    ))
  }
  posicion <- .campo_resultado_dbi(datos, campo)
  if (is.na(posicion)) {
    return(list(ok = FALSE, valor = NULL, motivo = paste0(
      "La consulta no devolvio el campo `", campo, "`; devolvio ",
      paste0("`", names(datos), "`", collapse = ", "), "."
    )))
  }
  valores <- datos[[posicion]]
  if (!length(valores)) {
    return(list(ok = FALSE, valor = NULL, motivo = paste0(
      "El campo `", campo, "` volvio vacio."
    )))
  }
  list(ok = TRUE, valor = valores[[1L]], motivo = NA_character_)
}

# El mismo motor que pliega los alias pliega los nombres de columna. Lo que el
# usuario escribio en `orden_muestra` se resuelve contra la grafia que devuelve
# el motor, sin distinguir caja, y se sigue usando la del motor.
.resolver_columnas_dbi <- function(pedidas, campos) {
  posicion <- match(pedidas, campos)
  faltan <- is.na(posicion)
  if (any(faltan)) {
    posicion[faltan] <- match(tolower(pedidas[faltan]), tolower(campos))
  }
  posicion
}

.escalar_dbi <- function(conexion, sql, campo, presupuesto = NULL,
                         etapa = "consulta") {
  resultado <- .consultar_dbi(conexion, sql, presupuesto, etapa = etapa)
  if (!resultado$ok) {
    return(.adjuntar_medicion_dbi(
      list(ok = FALSE, valor = NULL, motivo = resultado$motivo), resultado
    ))
  }
  .adjuntar_medicion_dbi(.valor_campo_dbi(resultado$datos, campo), resultado)
}

# Habia dos funciones con cuerpos identicos para la misma pregunta -esta y
# `.bit64_disponible()` en columnas.R- y una tercera forma distinta en
# `.hay_paquete()`. Tres puntos de verdad para "esta bit64?": cambiar el
# criterio en uno y olvidar el otro es cuestion de tiempo. Queda uno solo.
.bit64_disponible_dbi <- function() .bit64_disponible()

.numero_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  if (inherits(valor, "integer64")) {
    return(suppressWarnings(as.numeric(valor[[1L]])))
  }
  suppressWarnings(as.numeric(valor[[1L]]))
}

# El limite donde un `double` deja de representar enteros exactamente. Debajo de
# el, `numeric` y `integer64` guardan el mismo numero y `integer64` no compra
# nada; encima, el `double` ya perdio digitos y `integer64` es la unica forma de
# conservarlos.
.MAX_ENTERO_EXACTO_DBI <- 2^53

.entero_exacto_grande_dbi <- function(texto) {
  if (is.na(texto) || !grepl("^[+-]?[0-9]+$", texto)) return(FALSE)
  # La comparacion se hace sobre el texto para no perder el digito justo al
  # convertirlo: `as.numeric("9007199254740993")` ya devuelve ...992.
  digitos <- sub("^[+-]", "", texto)
  digitos <- sub("^0+(?=[0-9])", "", digitos, perl = TRUE)
  nchar(digitos) > 16L ||
    (nchar(digitos) == 16L && digitos > "9007199254740992")
}

# Un conteo sale `numeric`, salvo que sea tan grande que un `double` ya no lo
# represente y el motor lo haya entregado de una forma que si lo conserva
# -texto, o `integer64`-.
#
# La version anterior devolvia `integer64` para cualquier conteo cuando `bit64`
# estaba instalado, incluido un 20. Eso no agregaba precision -el double ya era
# exacto- y agregaba tres problemas medidos:
#
#  1. La clase del mismo campo dependia de si el usuario tenia `bit64`, que es
#     un `Suggests`.
#  2. `perfilar()` devolvia `integer` para `n_distintos` y `perfilar_dbi()`
#     devolvia `integer64`: dos puertas del mismo paquete en desacuerdo sobre el
#     mismo campo.
#  3. Lo peor: un perfil guardado con `bit64` presente y leido donde no esta
#     muestra `9.881313e-323` donde midio `20`, sin error y sin aviso, y suma
#     como si fuera un numero. Informar como medido algo que no lo es, en el
#     paquete cuyo argumento es justamente ese.
#
# El caso donde `integer64` si compra exactitud queda intacto, y para un conteo
# significa una tabla de mas de nueve mil billones de filas.
.conteo_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  primero <- valor[[1L]]
  if (inherits(valor, "integer64")) {
    if (is.na(primero)) return(NA_real_)
    texto <- format(primero, scientific = FALSE, trim = TRUE)
    if (.entero_exacto_grande_dbi(texto)) return(primero)
    return(suppressWarnings(as.numeric(primero)))
  }
  texto <- if (is.character(valor)) as.character(primero) else NA_character_
  if (!is.na(texto) && .entero_exacto_grande_dbi(texto) &&
      .bit64_disponible_dbi()) {
    return(bit64::as.integer64(texto))
  }
  numero <- suppressWarnings(as.numeric(primero))
  if (!length(numero) || is.na(numero)) return(NA_real_)
  numero
}

.conteo_estimado_dbi <- function(valor, universo, tamano_muestra) {
  observado <- .numero_dbi(valor)
  universo_numero <- .numero_dbi(universo)
  muestra_numero <- .numero_dbi(tamano_muestra)
  if (!is.finite(observado) || !is.finite(universo_numero) ||
      !is.finite(muestra_numero) || muestra_numero <= 0) {
    if (is.finite(universo_numero) && universo_numero == 0) {
      return(.conteo_dbi(0))
    }
    return(NA_real_)
  }
  # Cuando la muestra cubre el universo no hay nada que extrapolar: el conteo
  # observado ya es el del universo, y multiplicarlo por una razon mayor que
  # uno inventaria filas que no existen.
  if (muestra_numero >= universo_numero) return(.conteo_dbi(observado))
  .conteo_dbi(round(observado / muestra_numero * universo_numero))
}

# Si el conteo que se guarda representa el numero del motor sin perder digitos.
# Tiene que contestar sobre lo que `.conteo_dbi()` guarda y no sobre otra cosa:
# antes decia FALSE para un conteo entregado como texto por encima de 2^53, que
# es justamente el caso donde si se guarda exacto. El paquete se declaraba menos
# preciso de lo que era, que es el error simetrico del que importa, pero error
# igual.
.conteo_exacto_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(FALSE)
  primero <- valor[[1L]]
  if (inherits(valor, "integer64")) {
    if (is.na(primero)) return(FALSE)
    return(.bit64_disponible_dbi())
  }
  if (is.character(valor)) {
    texto <- as.character(primero)
    if (.entero_exacto_grande_dbi(texto)) return(.bit64_disponible_dbi())
  }
  numero <- .numero_dbi(valor)
  is.finite(numero) && abs(numero) <= .MAX_ENTERO_EXACTO_DBI
}

.metadatos_sql_dbi <- function(alcance = "tabla_completa", universo = NA,
                               tamano_muestra = NA, fraccion = NA_real_,
                               metodo = NA_character_,
                               error_esperado = NA_character_, lote = NA_integer_,
                               columnas_compartidas = NA_integer_,
                               id_consulta = NA_integer_,
                               estrategia_solicitada = NA_character_,
                               estrategia_resuelta = NA_character_,
                               estado_estrategia = NA_character_) {
  list(
    alcance = alcance,
    universo = universo,
    tamano_muestra = tamano_muestra,
    fraccion = fraccion,
    metodo = metodo,
    error_esperado = error_esperado,
    lote = lote,
    columnas_compartidas = columnas_compartidas,
    id_consulta = id_consulta,
    estrategia_solicitada = estrategia_solicitada,
    estrategia_resuelta = estrategia_resuelta,
    estado_estrategia = estado_estrategia
  )
}

.agregar_metadatos_estrategia_distintos_dbi <- function(metadatos,
                                                        estrategia) {
  if (is.null(estrategia)) return(metadatos)
  metadatos$estrategia_solicitada <- estrategia$estrategia_solicitada
  metadatos$estrategia_resuelta <- estrategia$estrategia_resuelta
  metadatos$estado_estrategia <- estrategia$estado
  metadatos
}

.mezclar_metadatos_dbi <- function(base, extra = NULL) {
  if (is.null(extra)) return(base)
  for (nombre in names(extra)) {
    if (!is.null(extra[[nombre]])) base[[nombre]] <- extra[[nombre]]
  }
  base
}

# ---- Registro de auditoria ----------------------------------------------

.memoria_trabajo_sql_dbi <- function(estado, metadatos) {
  if (length(estado) != 1L || is.na(estado)) return(NA_character_)
  estado <- as.character(estado)
  if (estado %in% c(
    "no_solicitado", "omitida", "omitido_por_costo",
    "omitido_por_privacidad", "no_disponible", "no_aplica",
    "sin_valores", "no_medido"
  )) {
    return(NA_character_)
  }

  alcance <- if (is.null(metadatos$alcance)) {
    NA_character_
  } else {
    as.character(metadatos$alcance)
  }
  fraccion <- if (is.null(metadatos$fraccion)) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(metadatos$fraccion))
  }
  if (length(alcance) == 1L && identical(alcance, "muestra") &&
      length(fraccion) == 1L && isTRUE(is.finite(fraccion)) &&
      fraccion < 1) {
    return("acotado")
  }

  # Una muestra saturada tiene alcance `muestra`, pero fraccion = 1: su cap no
  # acota el universo y por eso continua en R3 junto a las tablas completas.
  alcance_tabla <- length(alcance) == 1L && (
    alcance %in% c("tabla_completa", "tabla_muestreada") ||
      (identical(alcance, "muestra") &&
         length(fraccion) == 1L && isTRUE(fraccion == 1))
  )
  if (!alcance_tabla) return(NA_character_)

  metodo <- if (is.null(metadatos$metodo)) {
    NA_character_
  } else {
    as.character(metadatos$metodo)
  }
  if (length(metodo) != 1L || is.na(metodo)) {
    return(NA_character_)
  }
  if (!metodo %in% names(.REGISTRO_MEMORIA_TRABAJO_DBI)) {
    # En la muestra saturada `random_limit` ya no limita filas: el agregado
    # plano recorre la tabla completa y conserva su estado acotado de una
    # pasada. No se agrega al registro general de metodos de muestra; fuera de
    # este caso, un metodo no registrado sigue publicando `NA`.
    if (identical(alcance, "muestra") && isTRUE(fraccion == 1) &&
        identical(metodo, "random_limit")) {
      return("acotado")
    }
    return(NA_character_)
  }
  unname(.REGISTRO_MEMORIA_TRABAJO_DBI[[metodo]])
}

.registro_sql_dbi <- function(columna, metricas, estado, motivo, sql,
                              metadatos = NULL, medicion = NULL,
                              etapa = NULL) {
  if (is.null(metadatos)) metadatos <- .metadatos_sql_dbi()
  if (is.null(medicion)) {
    medicion <- .medicion_consulta_vacia_dbi()
  }
  etapa_publicada <- if (!is.null(medicion$etapa) &&
                         length(medicion$etapa) == 1L &&
                         !is.na(medicion$etapa)) {
    medicion$etapa
  } else if (!is.null(etapa) && length(etapa) == 1L && !is.na(etapa)) {
    etapa
  } else if (identical(estado, "no_solicitado")) {
    "no_solicitado"
  } else {
    "resumen_sql"
  }
  medicion_valor <- function(nombre, defecto) {
    valor <- medicion[[nombre]]
    if (is.null(valor) || !length(valor)) defecto else valor
  }
  data.frame(
    columna = rep_len(as.character(columna), length(metricas)),
    metrica = as.character(metricas),
    estado = rep_len(as.character(estado), length(metricas)),
    motivo = rep_len(as.character(motivo), length(metricas)),
    sql = rep_len(as.character(sql), length(metricas)),
    alcance = rep_len(as.character(metadatos$alcance), length(metricas)),
    universo = rep_len(metadatos$universo, length(metricas)),
    tamano_muestra = rep_len(metadatos$tamano_muestra, length(metricas)),
    fraccion = rep_len(as.numeric(metadatos$fraccion), length(metricas)),
    metodo = rep_len(as.character(metadatos$metodo), length(metricas)),
    error_esperado = rep_len(as.character(metadatos$error_esperado), length(metricas)),
    lote = rep_len(as.integer(metadatos$lote), length(metricas)),
    columnas_compartidas = rep_len(
      as.integer(metadatos$columnas_compartidas), length(metricas)
    ),
    id_consulta = rep_len(
      as.integer(metadatos$id_consulta), length(metricas)
    ),
    estrategia_solicitada = rep_len(
      as.character(metadatos$estrategia_solicitada), length(metricas)
    ),
    estrategia_resuelta = rep_len(
      as.character(metadatos$estrategia_resuelta), length(metricas)
    ),
    estado_estrategia = rep_len(
      as.character(metadatos$estado_estrategia), length(metricas)
    ),
    duracion_ms = rep_len(
      as.numeric(medicion_valor("duracion_ms", NA_real_)), length(metricas)
    ),
    n_filas_resultado = rep_len(
      as.numeric(medicion_valor("n_filas_resultado", NA_real_)), length(metricas)
    ),
    bytes_resultado_r = rep_len(
      as.numeric(medicion_valor("bytes_resultado_r", NA_real_)), length(metricas)
    ),
    cpu_ms = rep_len(
      as.numeric(medicion_valor("cpu_ms", NA_real_)), length(metricas)
    ),
    consulta_id = rep_len(
      as.integer(medicion_valor("consulta_id", NA_integer_)), length(metricas)
    ),
    etapa = rep_len(as.character(etapa_publicada), length(metricas)),
    derrame = rep_len(NA, length(metricas)),
    bloques_temporales_leidos = rep_len(NA_real_, length(metricas)),
    bloques_temporales_escritos = rep_len(NA_real_, length(metricas)),
    fuente_derrame = rep_len(NA_character_, length(metricas)),
    llamadas_en_ventana = rep_len(NA_real_, length(metricas)),
    memoria_trabajo = rep_len(
      .memoria_trabajo_sql_dbi(estado, metadatos), length(metricas)
    ),
    stringsAsFactors = FALSE
  )
}

.adjuntar_derrame_sql_dbi <- function(sql, derrame) {
  if (!is.data.frame(sql)) return(sql)
  if (!"derrame" %in% names(sql)) sql$derrame <- NA
  if (!"bloques_temporales_leidos" %in% names(sql)) {
    sql$bloques_temporales_leidos <- NA_real_
  }
  if (!"bloques_temporales_escritos" %in% names(sql)) {
    sql$bloques_temporales_escritos <- NA_real_
  }
  if (!"fuente_derrame" %in% names(sql)) {
    sql$fuente_derrame <- NA_character_
  }
  if (!"llamadas_en_ventana" %in% names(sql)) {
    sql$llamadas_en_ventana <- NA_real_
  }
  if (is.null(derrame) || !identical(derrame$estado, "medido") ||
      !length(derrame$consultas)) {
    return(sql)
  }
  for (consulta in derrame$consultas) {
    indices <- which(
      vapply(sql$sql, .normalizar_sql_derrame_dbi, character(1L)) ==
        consulta$query_normalizada
    )
    if (!length(indices)) next
    sql$derrame[indices] <- isTRUE(consulta$derrame)
    sql$bloques_temporales_leidos[indices] <-
      consulta$bloques_temporales_leidos
    sql$bloques_temporales_escritos[indices] <-
      consulta$bloques_temporales_escritos
    sql$fuente_derrame[indices] <- derrame$fuente
    sql$llamadas_en_ventana[indices] <- consulta$llamadas_en_ventana
  }
  sql
}

.registrar_resultado_dbi <- function(registros, columna, metricas, resultado,
                                     motivo_exito = NA_character_,
                                     metadatos = NULL) {
  metadatos <- .mezclar_metadatos_dbi(metadatos, resultado$metadatos)
  if (is.null(metadatos)) metadatos <- .metadatos_sql_dbi()
  estado <- if (!isTRUE(resultado$ok)) {
    "no_disponible"
  } else if (!is.null(resultado$estado)) {
    resultado$estado
  } else {
    "calculado"
  }
  motivo <- if (isTRUE(resultado$ok)) motivo_exito else resultado$motivo
  sql <- if (is.null(resultado$sql)) NA_character_ else resultado$sql
  c(registros, list(.registro_sql_dbi(
    columna, metricas, estado, motivo, sql, metadatos, medicion = resultado
  )))
}

.marcar_nivel_sql_dbi <- function(sql) {
  if (!is.data.frame(sql) || !"consulta_id" %in% names(sql)) return(sql)
  nivel <- rep.int(1L, nrow(sql))
  medidos <- which(!is.na(sql$consulta_id))
  if (length(medidos)) {
    grupos <- split(medidos, as.character(sql$consulta_id[medidos]))
    for (indices in grupos) {
      if (length(indices) > 1L) nivel[indices[-1L]] <- 2L
    }
  }
  sql$nivel <- as.integer(nivel)
  if ("memoria_trabajo" %in% names(sql)) {
    sql <- sql[c(setdiff(names(sql), "memoria_trabajo"), "memoria_trabajo")]
  }
  sql
}

.cobertura_dbi_vacia <- function() {
  data.frame(
    bloque = character(), elemento = character(), estado = character(),
    motivo = character(), como_resolverlo = character(), sql = character(),
    stringsAsFactors = FALSE
  )
}

.registro_cobertura_dbi <- function(bloque, elemento, estado, motivo,
                                    como_resolverlo, sql = NA_character_) {
  data.frame(
    bloque = as.character(bloque), elemento = as.character(elemento),
    estado = as.character(estado), motivo = as.character(motivo),
    como_resolverlo = as.character(como_resolverlo), sql = as.character(sql),
    stringsAsFactors = FALSE
  )
}

# Compara solo mediciones exactas de `n_validos` y `n_distintos`. Si salen de
# consultas distintas y se contradicen, la explicacion defendible es que la
# tabla cambio entre ambas sentencias: cada valor puede ser correcto dentro de
# su propio grupo de consistencia. No se usa esta funcion para aplicar una cota.
.cobertura_cambio_entre_consultas_dbi <- function(columnas, sql) {
  vacia <- .cobertura_dbi_vacia()
  if (!is.data.frame(columnas) || !is.data.frame(sql) ||
      !all(c("columna", "n_validos", "n_distintos") %in% names(columnas)) ||
      !all(c("columna", "metrica", "estado", "consulta_id", "sql") %in%
             names(sql))) {
    return(vacia)
  }
  nombres <- as.character(columnas$columna)
  exacto <- function(columna, metrica) {
    indices <- which(
      as.character(sql$columna) == columna &
        as.character(sql$metrica) == metrica &
        as.character(sql$estado) == "calculado" &
        !is.na(sql$consulta_id)
    )
    if (!length(indices)) return(NULL)
    indice <- indices[[length(indices)]]
    list(
      id = sql$consulta_id[[indice]],
      sentencia = sql$sql[[indice]]
    )
  }
  numero <- function(x) {
    valor <- .numero_dbi(x)
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor)) {
      return(NA_real_)
    }
    valor
  }
  texto_valor <- function(x) {
    format(x, scientific = FALSE, trim = TRUE)
  }
  texto_sentencia <- function(registro) {
    sentencia <- as.character(registro$sentencia)
    if (!length(sentencia) || is.na(sentencia)) sentencia <- "no conservada"
    paste0("sentencia ", as.character(registro$id), ": ", sentencia)
  }
  registros <- lapply(seq_len(nrow(columnas)), function(i) {
    columna <- nombres[[i]]
    validos <- exacto(columna, "n_validos")
    distintos <- exacto(columna, "n_distintos")
    if (is.null(validos) || is.null(distintos)) return(NULL)
    id_validos <- as.character(validos$id)
    id_distintos <- as.character(distintos$id)
    if (!length(id_validos) || !length(id_distintos) ||
        is.na(id_validos) || is.na(id_distintos) ||
        identical(id_validos, id_distintos)) {
      return(NULL)
    }
    n_validos <- numero(columnas$n_validos[[i]])
    n_distintos <- numero(columnas$n_distintos[[i]])
    if (is.na(n_validos) || is.na(n_distintos) || n_distintos <= n_validos) {
      return(NULL)
    }
    motivo <- paste0(
      "No hubo lectura instantanea de la tabla: `n_validos` = ",
      texto_valor(n_validos), " salio de ", texto_sentencia(validos),
      " y `n_distintos` = ", texto_valor(n_distintos), " salio de ",
      texto_sentencia(distintos), ". Ambos valores son exactos dentro de su",
      " sentencia, pero pertenecen a grupos de consistencia distintos y no se",
      " pueden comparar como una sola fotografia. La diferencia es evidencia",
      " de que la tabla cambio durante la corrida; no se atribuye al motor ni",
      " al paquete."
    )
    .registro_cobertura_dbi(
      "consistencia", columna, "alcance_distinto", motivo,
      paste(
        "Repetir el perfil bajo una instantanea o una transaccion con el nivel",
        "de aislamiento que garantice una lectura consistente."
      ),
      NA_character_
    )
  })
  registros <- Filter(Negate(is.null), registros)
  if (!length(registros)) vacia else do.call(rbind, registros)
}

# ---- Metricas ------------------------------------------------------------

.METRICAS_DBI <- c(
  "validos", "distintos", "moda", "basicos", "mediana", "desvio"
)

.CAMPOS_METRICA_DBI <- list(
  validos = c("n_validos", "n_faltantes", "prop_faltantes"),
  distintos = c("n_distintos", "tasa_distintos"),
  moda = c("moda", "frecuencia_moda"),
  basicos = c("minimo", "maximo", "media", "n_ceros", "n_negativos"),
  mediana = "mediana",
  desvio = "desvio"
)

.METRICAS_OBSERVADAS_MUESTRA_DBI <- c(
  "n_validos", "n_faltantes", "prop_faltantes", "n_distintos",
  "tasa_distintos", "n_ceros", "n_negativos", "frecuencia_moda"
)

.METRICAS_NUMERICAS_DBI <- c("basicos", "mediana", "desvio")

# La estrategia de `distintos` se elige de forma explicita. El orden es parte
# del contrato: el primer valor es el valor por omision de la API.
.ESTRATEGIAS_DISTINTOS_DBI <- c(
  "exacta", "aproximada_motor", "catalogo", "omitida"
)

# La moda puede pagar un agrupamiento que crece con la cardinalidad. La mediana
# no sigue ese regimen: su costo queda gobernado por las filas, asi que no entra
# en la politica proporcional. Se conserva el conjunto historico para que las
# decisiones y el plan sigan describiendo ambas metricas, pero solo la primera
# usa el umbral.
.METRICAS_COSTOSAS_DBI <- c("moda", "mediana")
.METRICA_CARDINALIDAD_COSTO_DBI <- "moda"
.UMBRAL_CARDINALIDAD_COSTO_DBI <- 0.5

# Estas metricas no tienen una cota simple sobre una muestra sin supuestos
# adicionales. Las demas pueden tener un error muestral estimable bajo un plan
# probabilistico, pero esta corrida no lo calcula.
.METRICAS_ERROR_NO_ESTIMABLE_DBI <- c(
  "n_distintos", "tasa_distintos", "moda", "frecuencia_moda", "mediana"
)

.error_esperado_muestreo_dbi <- function(metricas, muestreo, fraccion) {
  fraccion_numero <- suppressWarnings(as.numeric(fraccion))
  sin_muestreo <- is.null(muestreo) || !isTRUE(muestreo$disponible) ||
    (length(fraccion_numero) == 1L && is.finite(fraccion_numero) &&
       fraccion_numero >= 1)
  if (sin_muestreo) return("no_aplica")
  if (any(metricas %in% .METRICAS_ERROR_NO_ESTIMABLE_DBI)) {
    return("no_estimable")
  }
  "no_estimado"
}

.motivo_error_esperado_muestreo_dbi <- function(metricas, estado) {
  if (identical(estado, "no_aplica")) return(NA_character_)
  if (identical(estado, "no_estimable")) {
    if (any(metricas %in% c("moda", "frecuencia_moda"))) {
      return(paste(
        "La moda de una muestra no tiene una cota simple de error sin",
        "supuestos adicionales sobre la distribucion."
      ))
    }
    if (any(metricas %in% "mediana")) {
      return(paste(
        "La mediana de una muestra no tiene una cota simple de error sin",
        "supuestos adicionales sobre la distribucion."
      ))
    }
    return(paste(
      "La cardinalidad observada en una muestra no estima la del universo",
      "sin un estimador declarado."
    ))
  }
  paste(
    "El error muestral podria estimarse bajo un plan probabilistico, pero no",
    "se calculo en esta corrida."
  )
}

.motivo_conteo_observado_muestra_dbi <- function(metricas) {
  if (any(metricas %in% c("n_validos", "n_faltantes", "prop_faltantes"))) {
    return(paste(
      "conteo exacto en la muestra; no estima la tabla completa.",
      "El denominador es `n_total_consulta` de esa misma sentencia."
    ))
  }
  if (any(metricas %in% c("n_ceros", "n_negativos", "frecuencia_moda"))) {
    return(paste(
      "conteo exacto en la muestra; no estima la tabla completa.",
      "La escala es la de las filas observadas por esa consulta."
    ))
  }
  NA_character_
}

.validar_politica_costo_dbi <- function(politica_costo,
                                       umbral_cardinalidad) {
  if (length(politica_costo) > 1L) {
    if (identical(politica_costo, c("todas", "por_cardinalidad"))) {
      politica_costo <- politica_costo[[1L]]
    } else {
      .detener_dbi(
        "lupa_error_argumento_dbi",
        "`politica_costo` debe ser un unico valor: `todas` o `por_cardinalidad`."
      )
    }
  }
  opciones <- c("todas", "por_cardinalidad")
  if (!is.character(politica_costo) || length(politica_costo) != 1L ||
      is.na(politica_costo) || !politica_costo %in% opciones) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      paste0(
        "`politica_costo` debe ser un unico valor: `todas` o `por_cardinalidad`."
      )
    )
  }
  politica <- politica_costo
  if (!is.numeric(umbral_cardinalidad) ||
      length(umbral_cardinalidad) != 1L ||
      is.na(umbral_cardinalidad) || !is.finite(umbral_cardinalidad) ||
      umbral_cardinalidad < 0 || umbral_cardinalidad > 1) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`umbral_cardinalidad` debe ser un numero entre 0 y 1."
    )
  }
  list(
    nombre = politica,
    umbral = as.numeric(umbral_cardinalidad)
  )
}

.metricas_para_politica_costo_dbi <- function(metricas, politica,
                                              incluir_valores) {
  if (!identical(politica$nombre, "por_cardinalidad") ||
      !isTRUE(incluir_valores) ||
      !(.METRICA_CARDINALIDAD_COSTO_DBI %in% metricas)) {
    return(metricas)
  }
  unique(c(metricas, "validos", "distintos"))
}

.validar_estrategia_distintos_dbi <- function(estrategia_distintos) {
  match.arg(estrategia_distintos, .ESTRATEGIAS_DISTINTOS_DBI)
}

.estrategia_distintos_dbi <- function(metricas_solicitadas, politica,
                                      incluir_valores,
                                      estrategia_solicitada) {
  publica <- "distintos" %in% metricas_solicitadas
  para_costo <- identical(politica$nombre, "por_cardinalidad") &&
    isTRUE(incluir_valores) &&
    .METRICA_CARDINALIDAD_COSTO_DBI %in% metricas_solicitadas
  list(
    publica = publica,
    para_costo = para_costo,
    requiere_medicion = publica || para_costo,
    estrategia_solicitada = estrategia_solicitada,
    estrategia_resuelta = NA_character_,
    estado = if (publica || para_costo) "no_disponible" else "no_solicitado",
    disponible = FALSE,
    motivo = NA_character_,
    error_esperado = NA_character_,
    candidato = NULL,
    sondas = character(), estimaciones = NULL, fuentes = NULL,
    sql_catalogo = NA_character_
  )
}

.interpretar_n_distintos_catalogo_dbi <- function(raw, filas_catalogo,
                                                   columna) {
  motivo <- NA_character_
  estimado <- NA_real_
  if (!is.finite(raw)) {
    motivo <- paste0(
      "`pg_stats.n_distinct` no tiene una estimacion utilizable para `",
      columna, "`; puede no haberse ejecutado ANALYZE. No se supone cero."
    )
  } else if (raw >= 0) {
    estimado <- raw
    motivo <- paste0(
      "Estimacion de catalogo `pg_stats.n_distinct = ", raw,
      "`; el valor no fue medido recorriendo la tabla."
    )
  } else if (is.finite(filas_catalogo) && filas_catalogo >= 0) {
    estimado <- round(abs(raw) * filas_catalogo)
    motivo <- paste0(
      "Estimacion de catalogo: `pg_stats.n_distinct = ", raw,
      "` es una fraccion de las filas y `pg_class.reltuples = ",
      filas_catalogo, "; se estima n_distintos = ", estimado,
      ". No fue medido recorriendo la tabla."
    )
  } else {
    motivo <- paste0(
      "`pg_stats.n_distinct = ", raw,
      "` es una fraccion, pero `pg_class.reltuples` no tiene un valor utilizable;",
      " puede no haberse ejecutado ANALYZE. No se supone cero."
    )
  }
  disponible <- is.finite(estimado) && estimado >= 0
  proporcion <- if (disponible && is.finite(filas_catalogo) &&
                    filas_catalogo > 0) {
    min(1, max(0, estimado / filas_catalogo))
  } else {
    NA_real_
  }
  list(
    disponible = disponible,
    n_distintos = if (disponible) estimado else NA_real_,
    n_filas = filas_catalogo, n_distintos_catalogo = raw,
    proporcion_distintos = proporcion, motivo = motivo
  )
}

.logico_catalogo_dbi <- function(valor) {
  if (is.logical(valor)) return(valor)
  texto <- tolower(trimws(as.character(valor)))
  ifelse(
    texto %in% c("true", "t", "1"), TRUE,
    ifelse(texto %in% c("false", "f", "0"), FALSE, NA)
  )
}

.seleccionar_fila_catalogo_dbi <- function(datos, columna,
                                            nombre_columna,
                                            nombre_inherited,
                                            nombre_sin_hijas) {
  sin_decision <- function(motivo = NULL) {
    list(fila = NA_integer_, inherited = NA, motivo = motivo)
  }
  if (is.na(nombre_columna) || is.na(nombre_inherited) ||
      is.na(nombre_sin_hijas)) {
    return(sin_decision(paste(
      "La respuesta de `pg_stats` no informa `inherited` y si la relacion",
      "tiene hijas; no se puede decidir que fila describe lo que se lee."
    )))
  }
  filas <- which(
    tolower(as.character(datos[[nombre_columna]])) == tolower(columna)
  )
  if (!length(filas)) return(sin_decision())
  inherited <- .logico_catalogo_dbi(datos[[nombre_inherited]][filas])
  sin_hijas <- .logico_catalogo_dbi(datos[[nombre_sin_hijas]][filas])
  if (anyNA(inherited) || anyNA(sin_hijas) ||
      length(unique(sin_hijas)) != 1L) {
    return(sin_decision(paste0(
      "Hay filas de `pg_stats` para `", columna,
      "` cuyo `inherited` o relacion de hijas no se puede interpretar;",
      " no hay estimacion utilizable."
    )))
  }
  heredadas <- filas[which(inherited)]
  propias <- filas[which(!inherited)]
  if (length(heredadas) > 1L) {
    return(sin_decision(paste0(
      "Hay ", length(heredadas), " filas de `pg_stats` con `inherited = TRUE`",
      " para `", columna, "`; no se puede decidir cual describe lo que se lee."
    )))
  }
  if (length(heredadas) == 1L) {
    return(list(fila = heredadas[[1L]], inherited = TRUE, motivo = NULL))
  }
  if (length(propias) == 1L && isTRUE(sin_hijas[[1L]])) {
    return(list(fila = propias[[1L]], inherited = FALSE, motivo = NULL))
  }
  if (length(propias) == 1L) {
    return(sin_decision(paste0(
      "La relacion tiene hijas pero `pg_stats` no devolvio una fila con",
      " `inherited = TRUE` para `", columna,
      "`; la fila propia describe otro universo y no hay estimacion utilizable."
    )))
  }
  sin_decision(paste0(
    "Hay ", length(propias), " filas de `pg_stats` con `inherited = FALSE`",
    " para `", columna,
    "` y no se puede decidir cual describe lo que se lee; no hay estimacion",
    " utilizable."
  ))
}

.estimar_distintos_catalogo_dbi <- function(conexion, tabla, columnas,
                                            presupuesto) {
  senas <- .senas_conexion_dbi(conexion)
  if (!grepl("postgres|pqconnection", senas, ignore.case = TRUE)) {
    vacias <- .fuentes_cardinalidad_vacias_dbi(columnas)
    return(list(
      disponible = FALSE, estimaciones = vacias, fuentes = vacias,
      sql = NA_character_, motivo = paste(
        "`catalogo` requiere PostgreSQL y su `pg_stats`: no hay una estadistica",
        "portable de cardinalidad para este motor."
      )
    ))
  }
  piezas <- .piezas_tabla_cardinalidad_dbi(tabla)
  vacias <- .fuentes_cardinalidad_vacias_dbi(columnas)
  if (is.na(piezas$tabla) || !nzchar(piezas$tabla) || !length(columnas)) {
    return(list(
      disponible = FALSE, estimaciones = vacias, fuentes = vacias,
      sql = NA_character_,
      motivo = "No se pudo resolver la relacion o sus columnas para leer `pg_stats`."
    ))
  }
  literal <- function(x) as.character(DBI::dbQuoteString(conexion, x))
  # `pg_stats` tiene un guardian de RLS del motor: con RLS activo para quien
  # consulta, la vista devuelve cero filas. Esto se verifico ejecutando
  # PostgreSQL 16 con un rol con politica. No agregar una guarda redundante ni
  # tratarlo como un defecto pendiente: lupa ya cae en "no disponible".
  condicion_esquema <- if (is.na(piezas$esquema) || !nzchar(piezas$esquema)) {
    "pg_catalog.pg_table_is_visible(c.oid)"
  } else {
    paste0("n.nspname = ", literal(piezas$esquema))
  }
  condicion_columnas <- paste(
    vapply(columnas, literal, character(1L), USE.NAMES = FALSE),
    collapse = ", "
  )
  # Una tabla particionada (`relkind = 'p'`) no tiene filas propias y
  # PostgreSQL puede guardar alli el total de sus particiones. Se excluye del
  # sumatorio para no contar ese total dos veces; las relaciones regulares si
  # aportan sus `reltuples` propios.
  sql <- paste(
    "WITH RECURSIVE relaciones AS (",
    "SELECT c.oid, n.nspname AS schemaname, c.relname AS tablename,",
    "c.reltuples, c.relkind, TRUE AS es_raiz,",
    "NOT EXISTS (SELECT 1 FROM pg_catalog.pg_inherits h",
    "WHERE h.inhparent = c.oid) AS sin_hijas",
    "FROM pg_catalog.pg_class AS c",
    "JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
    "WHERE c.relname = ", literal(piezas$tabla), " AND ",
    condicion_esquema,
    " UNION ",
    "SELECT hija.oid, ns.nspname AS schemaname, hija.relname AS tablename,",
    "hija.reltuples, hija.relkind, FALSE AS es_raiz,",
    "NOT EXISTS (SELECT 1 FROM pg_catalog.pg_inherits h2",
    "WHERE h2.inhparent = hija.oid) AS sin_hijas",
    "FROM pg_catalog.pg_inherits AS herencia",
    "JOIN relaciones AS padre ON padre.oid = herencia.inhparent",
    "JOIN pg_catalog.pg_class AS hija ON hija.oid = herencia.inhrelid",
    "JOIN pg_catalog.pg_namespace AS ns ON ns.oid = hija.relnamespace",
    "), filas_jerarquia AS (",
    "SELECT CASE WHEN SUM(CASE WHEN r.relkind <> 'p' AND r.reltuples < 0",
    "THEN 1 ELSE 0 END) = 0 THEN SUM(CASE WHEN r.relkind <> 'p'",
    "THEN r.reltuples ELSE 0 END) ELSE NULL END AS reltuples",
    "FROM relaciones AS r",
    ")",
    "SELECT s.attname AS lupa_columna,",
    "s.n_distinct AS lupa_n_distinct,",
    "j.reltuples AS lupa_n_filas,",
    "s.inherited AS lupa_inherited,",
    "raiz.sin_hijas AS lupa_sin_hijas ",
    "FROM pg_catalog.pg_stats AS s ",
    "JOIN pg_catalog.pg_class AS c ON c.relname = s.tablename ",
    "JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace ",
    "AND n.nspname = s.schemaname ",
    "JOIN relaciones AS raiz ON raiz.oid = c.oid AND raiz.es_raiz ",
    "CROSS JOIN filas_jerarquia AS j ",
    "WHERE s.tablename = ", literal(piezas$tabla),
    " AND ", condicion_esquema,
    " AND s.attname IN (", condicion_columnas, ")"
  )
  consulta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "cardinalidad_catalogo"
  )
  if (!isTRUE(consulta$ok) || is.null(consulta$datos)) {
    motivo <- paste(
      "No se pudo leer `pg_stats.n_distinct`:",
      if (isTRUE(consulta$ok)) "la consulta no devolvio una tabla." else
        consulta$motivo
    )
    return(list(
      disponible = FALSE, estimaciones = vacias, fuentes = vacias,
      sql = sql, motivo = motivo
    ))
  }
  datos <- consulta$datos
  nombre_columna <- .campo_resultado_dbi(datos, "lupa_columna")
  nombre_distintos <- .campo_resultado_dbi(datos, "lupa_n_distinct")
  nombre_filas <- .campo_resultado_dbi(datos, "lupa_n_filas")
  nombre_inherited <- .campo_resultado_dbi(datos, "lupa_inherited")
  nombre_sin_hijas <- .campo_resultado_dbi(datos, "lupa_sin_hijas")
  estimaciones <- vector("list", length(columnas))
  fuentes <- vector("list", length(columnas))
  names(estimaciones) <- columnas
  names(fuentes) <- columnas
  for (columna in columnas) {
    seleccion <- .seleccionar_fila_catalogo_dbi(
      datos, columna, nombre_columna, nombre_inherited, nombre_sin_hijas
    )
    fila <- seleccion$fila
    raw <- if (!is.na(fila) && !is.na(nombre_distintos)) {
      .numero_dbi(datos[[nombre_distintos]][fila])
    } else {
      NA_real_
    }
    filas_catalogo <- if (!is.na(fila) && !is.na(nombre_filas)) {
      .numero_dbi(datos[[nombre_filas]][fila])
    } else {
      NA_real_
    }
    interpretacion <- if (is.na(fila)) {
      .interpretar_n_distintos_catalogo_dbi(NA_real_, NA_real_, columna)
    } else {
      .interpretar_n_distintos_catalogo_dbi(raw, filas_catalogo, columna)
    }
    if (!is.null(seleccion$motivo)) interpretacion$motivo <- seleccion$motivo
    interpretacion$inherited <- seleccion$inherited
    estimaciones[[columna]] <- list(
      disponible = interpretacion$disponible,
      n_distintos = interpretacion$n_distintos,
      n_filas = interpretacion$n_filas,
      n_distintos_catalogo = interpretacion$n_distintos_catalogo,
      proporcion_distintos = interpretacion$proporcion_distintos,
      inherited = interpretacion$inherited,
      motivo = interpretacion$motivo, sql = sql,
      fuente = "pg_stats.n_distinct"
    )
    fuentes[[columna]] <- if (isTRUE(interpretacion$disponible)) {
      list(
        nombre = "estimacion_catalogo", exacta = FALSE,
        n_distintos = interpretacion$n_distintos,
        n_filas = interpretacion$n_filas,
        proporcion_distintos = interpretacion$proporcion_distintos,
        inherited = interpretacion$inherited,
        motivo = interpretacion$motivo,
        fuente = "pg_stats.n_distinct"
      )
    } else {
      .fuente_cardinalidad_desconocida_dbi()
    }
  }
  disponibles <- vapply(
    estimaciones, function(x) isTRUE(x$disponible), logical(1L)
  )
  list(
    disponible = any(disponibles), estimaciones = estimaciones,
    fuentes = fuentes, sql = sql,
    motivo = if (any(disponibles)) paste(
      "Se leyo `pg_stats.n_distinct` como estimacion de catalogo;",
      "no es una medicion del universo de la tabla y puede faltar sin ANALYZE."
    ) else paste(
      "No hay estimaciones utilizables de `pg_stats.n_distinct`; puede no",
      "haberse ejecutado ANALYZE. No se supone cero."
    )
  )
}

.resolver_estrategia_distintos_dbi <- function(conexion, estrategia,
                                               presupuesto, hay_metrica,
                                               tabla = NULL, columnas = NULL,
                                               universo = "tabla_completa") {
  if (!isTRUE(hay_metrica)) {
    estrategia$estado <- "no_solicitado"
    estrategia$motivo <- paste(
      "La metrica `distintos` no se pidio en esta corrida."
    )
    return(estrategia)
  }
  switch(
    estrategia$estrategia_solicitada,
    exacta = {
      estrategia$estrategia_resuelta <- "COUNT(DISTINCT)"
      estrategia$estado <- "calculado"
      estrategia$disponible <- TRUE
      estrategia$motivo <- paste(
        "Se calculara la cardinalidad exacta sobre las filas de la corrida."
      )
      estrategia$error_esperado <- "no_aplica"
    },
    aproximada_motor = {
      resolucion <- .sondar_aproximacion_dbi(
        conexion, "distintos", presupuesto
      )
      estrategia$disponible <- isTRUE(resolucion$disponible)
      estrategia$candidato <- resolucion$candidato
      estrategia$sondas <- resolucion$sondas
      estrategia$motivo <- resolucion$motivo
      if (isTRUE(resolucion$disponible)) {
        estrategia$estrategia_resuelta <- resolucion$candidato$nombre
        estrategia$estado <- "estimado_motor"
        estrategia$error_esperado <- resolucion$candidato$error_esperado
      } else {
        estrategia$estado <- "no_disponible"
      }
    },
    catalogo = {
      if (identical(universo, "muestra_motor")) {
        # Es el mismo principio que aplica `.estimar_derrame_postgresql_dbi()`:
        # se niega a estimar el derrame de una muestra con estadisticas de la
        # tabla completa y no se inventa una equivalencia entre universos.
        estrategia$disponible <- FALSE
        estrategia$estado <- "no_disponible"
        estrategia$motivo <- paste0(
          "La estrategia `catalogo` no esta disponible con `universo = ",
          universo,
          "`: el catalogo describe la relacion entera y la corrida mide un",
          " subconjunto; usarlo publicaria la cardinalidad de un universo como",
          " si fuera de otro."
        )
      } else {
        catalogo <- .estimar_distintos_catalogo_dbi(
          conexion, tabla, columnas, presupuesto
        )
        estrategia$estimaciones <- catalogo$estimaciones
        estrategia$fuentes <- catalogo$fuentes
        estrategia$sql_catalogo <- catalogo$sql
        estrategia$disponible <- isTRUE(catalogo$disponible)
        estrategia$motivo <- catalogo$motivo
        if (isTRUE(catalogo$disponible)) {
          estrategia$estrategia_resuelta <- "pg_stats.n_distinct"
          estrategia$estado <- "estimado_catalogo"
          estrategia$error_esperado <- "desconocido"
        } else {
          estrategia$estado <- "no_disponible"
        }
      }
    },
    omitida = {
      estrategia$estado <- "omitida"
      estrategia$motivo <- paste(
        "La estrategia `omitida` no emite una consulta para `distintos`."
      )
    }
  )
  estrategia
}

.publicar_estrategia_distintos_dbi <- function(estrategia) {
  list(
    estrategia_solicitada = estrategia$estrategia_solicitada,
    estrategia_resuelta = estrategia$estrategia_resuelta,
    estado = estrategia$estado,
    motivo = estrategia$motivo,
    error_esperado = estrategia$error_esperado,
    disponible = isTRUE(estrategia$disponible),
    publica = isTRUE(estrategia$publica),
    para_costo = isTRUE(estrategia$para_costo),
    requiere_medicion = isTRUE(estrategia$requiere_medicion),
    sondas = estrategia$sondas,
    fuente = if (identical(estrategia$estado, "estimado_catalogo")) {
      "pg_stats.n_distinct"
    } else {
      NA_character_
    },
    sql_catalogo = estrategia$sql_catalogo
  )
}

.publicar_estrategia_mediana_dbi <- function(preparacion) {
  solicitada <- preparacion$estrategia_mediana
  if (!"mediana" %in% preparacion$metricas_ejecucion) {
    if (identical(preparacion$universo, "muestra_motor") &&
        !is.null(preparacion$muestreo) &&
        !isTRUE(preparacion$muestreo$disponible)) {
      motivo <- preparacion$muestreo$motivo
      if (length(motivo) != 1L || is.na(motivo) ||
          !grepl("^[a-z_]+:[a-z_]+", motivo)) {
        motivo <- "capacidad_no_aceptada:sonda_muestreo"
      }
      return(list(
        estrategia_solicitada = solicitada,
        estrategia_resuelta = "no_disponible", estado = "no_disponible",
        motivo = motivo
      ))
    }
    return(list(
      estrategia_solicitada = solicitada,
      estrategia_resuelta = NA_character_, estado = "no_solicitado",
      motivo = "La metrica `mediana` no se pidio en esta corrida."
    ))
  }
  guardia_newid <- if (is.null(preparacion$presupuesto)) NULL else
    preparacion$presupuesto$guardia_newid
  if (identical(preparacion$universo, "muestra_motor") &&
      is.list(guardia_newid) && !isTRUE(guardia_newid$aceptado)) {
    return(list(
      estrategia_solicitada = solicitada,
      estrategia_resuelta = "no_disponible", estado = "no_disponible",
      motivo = guardia_newid$motivo,
      metadata_costo = guardia_newid
    ))
  }
  if (identical(preparacion$universo, "muestra_motor")) {
    cte <- preparacion$mediana_cte_ventana_resolucion
    if (is.list(cte) && isTRUE(cte$disponible)) {
      metodo <- if (!is.null(preparacion$mediana_cte_ventana)) {
        preparacion$mediana_cte_ventana$nombre
      } else {
        cte$candidato$nombre
      }
      return(list(
        estrategia_solicitada = solicitada,
        estrategia_resuelta = metodo,
        estado = "calculado",
        motivo = "Se uso la CTE de ventanas sobre una sola relacion muestreada.",
        sondas = cte$sondas
      ))
    }
    # La mediana muestreada no puede degradarse a `dos_consultas`, aunque el
    # dialecto conserve esa vía para `tabla_completa`.
    if (is.list(cte)) {
      return(list(
        estrategia_solicitada = solicitada,
        estrategia_resuelta = "no_disponible", estado = "no_disponible",
        motivo = "capacidad_no_aceptada:sonda_mediana_cte_ventana",
        sondas = cte$sondas
      ))
    }
  }
  consolidada <- preparacion$mediana_consolidada_resolucion
  if (is.list(consolidada) && isTRUE(consolidada$disponible)) {
    return(list(
      estrategia_solicitada = solicitada,
      estrategia_resuelta = consolidada$candidato$nombre,
      estado = "calculado",
      motivo = "Se uso una forma exacta consolidada aceptada por el motor."
    ))
  }
  escalar <- preparacion$mediana_escalar_resolucion
  if (is.list(escalar) && isTRUE(escalar$disponible)) {
    return(list(
      estrategia_solicitada = solicitada,
      estrategia_resuelta = escalar$candidato$nombre,
      estado = "calculado",
      motivo = "Se uso una forma exacta escalar aceptada por el motor."
    ))
  }
  aproximada <- preparacion$aproximaciones_resolucion$mediana
  if (is.list(aproximada) && isTRUE(aproximada$disponible)) {
    return(list(
      estrategia_solicitada = solicitada,
      estrategia_resuelta = aproximada$candidato$nombre,
      estado = "estimado",
      motivo = "No hubo una forma exacta aceptada; se uso la aproximacion nativa al final."
    ))
  }
  list(
    estrategia_solicitada = solicitada,
    estrategia_resuelta = "exacta_por_columna", estado = "calculado",
    motivo = "Se uso la forma exacta por columna como ultimo camino."
  )
}

.fuente_cardinalidad_desconocida_dbi <- function() {
  list(
    nombre = "desconocida", exacta = FALSE,
    proporcion_distintos = NA_real_, motivo = paste(
      "No hay una garantia estructural ni una estimacion de catalogo utilizable;",
      "la cardinalidad solo se puede conocer midiendo `distintos`."
    )
  )
}

.fuentes_cardinalidad_vacias_dbi <- function(columnas) {
  salida <- rep(list(.fuente_cardinalidad_desconocida_dbi()),
                length(columnas))
  names(salida) <- columnas
  salida
}

.validar_metricas_dbi <- function(metricas) {
  if (is.null(metricas)) return(.METRICAS_DBI)
  if (!is.character(metricas) || !length(metricas) || anyNA(metricas)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`metricas` debe ser NULL o un vector de texto sin ausentes."
    )
  }
  desconocidas <- setdiff(metricas, .METRICAS_DBI)
  if (length(desconocidas)) {
    .detener_dbi("lupa_error_argumento_dbi", paste0(
      "Metricas desconocidas: ", paste(desconocidas, collapse = ", "),
      ". Las disponibles son: ", paste(.METRICAS_DBI, collapse = ", "), "."
    ))
  }
  unique(metricas)
}

.validar_max_consultas_dbi <- function(max_consultas) {
  if (!is.numeric(max_consultas) || length(max_consultas) != 1L ||
      is.na(max_consultas) || max_consultas < 1) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`max_consultas` debe ser un numero mayor o igual a 1, o Inf."
    )
  }
  as.numeric(max_consultas)
}

.validar_tamano_lote_dbi <- function(tamano_lote, nombre = "tamano_lote") {
  if (!is.numeric(tamano_lote) || length(tamano_lote) != 1L ||
      is.na(tamano_lote) || !is.finite(tamano_lote) || tamano_lote < 1 ||
      tamano_lote != floor(tamano_lote)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      paste0("`", nombre, "` debe ser un entero positivo finito.")
    )
  }
  as.integer(tamano_lote)
}

.resolver_tamanos_lote_dbi <- function(tamano_lote,
                                       tamano_lote_planos,
                                       tamano_lote_distintos) {
  # El argumento antiguo sigue siendo util para reproducir corridas previas.
  # Solo en ese caso se copia a las dos familias; la API nueva siempre deja
  # que los distintos tengan un lote propio y conservador.
  if (!is.null(tamano_lote)) {
    tamano_lote_planos <- tamano_lote
    tamano_lote_distintos <- tamano_lote
  }
  list(
    planos = .validar_tamano_lote_dbi(
      tamano_lote_planos, "tamano_lote_planos"
    ),
    distintos = .validar_tamano_lote_dbi(
      tamano_lote_distintos, "tamano_lote_distintos"
    )
  )
}

# `Inf` significa "toda la tabla, sea del tamano que sea". Es la unica forma de
# pedirlo sin averiguar antes cuantas filas hay: el armado de la muestra ya no
# pone `LIMIT` cuando lo pedido no es menor que el total, asi que `Inf` cae solo
# en ese camino. Existe porque un analisis de calidad no se corre todos los dias
# y suele convenir esperar antes que mirar menos filas.
.validar_muestra_dbi <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      muestra < 1 ||
      (is.finite(muestra) && muestra != floor(muestra))) {
    stop(
      "`muestra` debe ser un entero positivo, o `Inf` para la tabla entera.",
      call. = FALSE
    )
  }
  as.numeric(muestra)
}

# Los topes de la muestra tienen que decidirse antes de la lectura que se
# entrega a `perfilar()`. Las celdas se pueden resolver con el esquema y el
# conteo que ya acompana a la corrida; los bytes quedan abiertos hasta la
# sonda chica que hace el bloque DBI. El plan usa la misma funcion con un
# `n_total` desconocido y publica entonces una cota, no una falsa exactitud.
.tope_muestra_dbi <- function(muestra, n_total, n_columnas,
                             max_celdas_muestra, max_bytes_muestra) {
  max_celdas_muestra <- .validar_limite_duplicados(
    max_celdas_muestra, "max_celdas_muestra"
  )
  max_bytes_muestra <- .validar_limite_duplicados(
    max_bytes_muestra, "max_bytes_muestra"
  )
  total <- .numero_dbi(n_total)
  total_conocido <- length(total) == 1L && !is.na(total) &&
    is.finite(total) && total >= 0
  solicitado <- if (total_conocido) min(total, muestra) else muestra
  por_celdas <- if (n_columnas > 0 && is.finite(max_celdas_muestra)) {
    floor(as.numeric(max_celdas_muestra) / n_columnas)
  } else {
    Inf
  }
  if (total_conocido && total > 0 && por_celdas < 1) {
    stop(
      "`max_celdas_muestra` debe permitir al menos una fila para todas las columnas.",
      call. = FALSE
    )
  }
  filas <- min(solicitado, por_celdas)
  if (total_conocido && total > 0 && filas < 1) {
    stop(
      "`max_celdas_muestra` debe permitir al menos una fila para todas las columnas.",
      call. = FALSE
    )
  }
  list(
    filas_solicitadas = solicitado,
    filas_por_celdas = por_celdas,
    filas_por_bytes = Inf,
    filas_efectivas = filas,
    celdas_solicitadas = solicitado * n_columnas,
    celdas_efectivas = filas * n_columnas,
    bytes_sonda = NA_real_, bytes_muestra = NA_real_,
    max_celdas_muestra = max_celdas_muestra,
    max_bytes_muestra = max_bytes_muestra,
    recortada = filas < solicitado,
    motivos = character()
  )
}

.tope_muestra_plan_dbi <- function(muestra, n_columnas,
                                  max_celdas_muestra, max_bytes_muestra) {
  max_celdas_muestra <- .validar_limite_duplicados(
    max_celdas_muestra, "max_celdas_muestra"
  )
  max_bytes_muestra <- .validar_limite_duplicados(
    max_bytes_muestra, "max_bytes_muestra"
  )
  por_celdas <- if (n_columnas > 0 && is.finite(max_celdas_muestra)) {
    floor(as.numeric(max_celdas_muestra) / n_columnas)
  } else {
    Inf
  }
  filas_maximas <- min(as.numeric(muestra), por_celdas)
  list(
    filas_solicitadas = as.numeric(muestra),
    filas_por_celdas = por_celdas,
    filas_maximas = filas_maximas,
    celdas_solicitadas = as.numeric(muestra) * n_columnas,
    celdas_maximas = filas_maximas * n_columnas,
    max_celdas_muestra = max_celdas_muestra,
    max_bytes_muestra = max_bytes_muestra,
    recortada_por_celdas = filas_maximas < as.numeric(muestra),
    requiere_sonda_bytes = is.finite(max_bytes_muestra),
    motivo = paste(
      "El tope de celdas se resuelve antes de leer con el ancho del esquema;",
      if (is.finite(max_bytes_muestra)) {
        "el tope de bytes requiere una sonda de hasta 100 filas antes de fijar el limite final."
      } else {
        "el tope de bytes esta desactivado con `Inf`."
      }
    )
  )
}

# ---- Tipos ---------------------------------------------------------------
#
# Que una columna admita agregados cuantitativos lo decide el motor, no el
# prototipo que expone el driver. Un DECIMAL, un NUMBER o un BIGINT devuelto
# como texto o como `integer64` cae en `no_aplica` si solo se mira la clase de
# R, aunque SQL si pueda calcularlo. Por eso se consulta tambien el tipo
# declarado en `dbColumnInfo()`, que nunca degrada la decision: solo la
# habilita.

.PATRON_TIPO_NUMERICO_DBI <- paste0(
  "^(decimal|numeric|number|int|integer|int2|int4|int8|bigint|smallint|",
  "tinyint|mediumint|float|float4|float8|double|real|money|smallmoney|",
  "binary_float|binary_double|integer64|int64)$"
)

.tipo_declarado_numerico_dbi <- function(tipo) {
  if (is.null(tipo) || !length(tipo) || is.na(tipo[[1L]])) return(FALSE)
  limpio <- tolower(trimws(sub("\\(.*", "", as.character(tipo[[1L]]))))
  limpio <- gsub("[[:space:]]+", "", limpio)
  grepl(.PATRON_TIPO_NUMERICO_DBI, limpio, perl = TRUE)
}

# Un tipo temporal declarado por el motor manda sobre lo que diga el prototipo.
# El `dbFetch(n = 0)` de algunos controladores -RMariaDB entre ellos- devuelve
# un `numeric(0)` para una columna `DATE`: la clase se pierde con las filas. Sin
# esta comprobacion, `is.numeric()` decia que si, y la columna se media como
# numero: `MIN` daba los dias desde 1970 y `AVG(f * 1.0)` daba YYYYMMDD en
# MariaDB. Dos unidades distintas, las dos publicadas como `calculado`.
.PATRON_TIPO_TEMPORAL_DBI <- paste0(
  "^(date|datetime|datetime2|smalldatetime|timestamp|timestamptz|",
  "timestampwithtimezone|timestampwithouttimezone|time|timetz|year|",
  "interval|datetimeoffset)$"
)

.tipo_declarado_temporal_dbi <- function(tipo) {
  if (is.null(tipo) || !length(tipo) || is.na(tipo[[1L]])) return(FALSE)
  limpio <- tolower(trimws(sub("\\(.*", "", as.character(tipo[[1L]]))))
  limpio <- gsub("[[:space:]_-]+", "", limpio)
  grepl(.PATRON_TIPO_TEMPORAL_DBI, limpio, perl = TRUE)
}

# `dbExistsTable()` no distingue una tabla que no existe de una que existe y no
# se puede ver, y el mensaje lo decia asi. Pero cuando la consulta de
# comprobacion falla, **el motor suele decir cual de las dos es**: PostgreSQL
# responde "permiso denegado a la relacion", SQL Server "permission was denied".
# Repetir la disyuncion con esa evidencia en la mano es informar como incierto
# algo que ya esta resuelto: el reverso del invariante, y en una corrida real
# fueron veintitres tablas descritas como "no existe o no hay permiso" cuando el
# motor habia dicho que era permiso.
.PATRON_PERMISO_DBI <- paste0(
  "permiso denegado|permission denied|permission was denied|",
  "insufficient privilege|no tiene privilegios|not authorized|",
  "acceso denegado|access denied|ORA-00942|ORA-01031"
)

.mensaje_tabla_inaccesible_dbi <- function(motivo) {
  detalle <- if (!is.na(motivo)) motivo else ""
  if (!is.na(motivo) &&
      grepl(.PATRON_PERMISO_DBI, motivo, ignore.case = TRUE, perl = TRUE)) {
    return(paste(
      "La tabla existe pero la credencial no tiene permiso para verla: el",
      "motor lo dijo explicitamente.", detalle
    ))
  }
  paste(
    "La tabla solicitada no existe en la conexion DBI, o la credencial no",
    "tiene permiso para verla. El motor no distinguio entre las dos, y",
    "`dbExistsTable()` tampoco.", detalle
  )
}

.es_numerico_dbi <- function(prototipo, tipo_declarado = NA_character_) {
  if (inherits(prototipo, c("Date", "POSIXt", "difftime"))) return(FALSE)
  if (.tipo_declarado_temporal_dbi(tipo_declarado)) return(FALSE)
  if (is.numeric(prototipo)) return(TRUE)
  if (inherits(prototipo, "integer64")) return(TRUE)
  if (is.logical(prototipo) || is.raw(prototipo) || is.list(prototipo)) {
    return(FALSE)
  }
  .tipo_declarado_numerico_dbi(tipo_declarado)
}

# ---- Resumen por columna -------------------------------------------------

.fila_resumen_dbi <- function(columna, n_total) {
  list(
    columna = columna,
    n = .conteo_dbi(n_total),
    n_validos = NA_real_,
    n_faltantes = NA_real_,
    prop_faltantes = NA_real_,
    n_distintos = NA_real_,
    tasa_distintos = NA_real_,
    moda = NA_character_,
    frecuencia_moda = NA_real_,
    minimo = NA_real_,
    maximo = NA_real_,
    media = NA_real_,
    mediana = NA_real_,
    desvio = NA_real_,
    n_ceros = NA_real_,
    n_negativos = NA_real_
  )
}

# `numeric(0)` es lo que devuelve `as.numeric(NULL)`, y es lo que reventaba el
# ensamblado de la fila y los `if`. Ningun valor entra a la fila sin pasar por
# aca.
.escalar_finito_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  numero <- suppressWarnings(as.numeric(valor[[1L]]))
  if (!length(numero) || is.na(numero)) return(NA_real_)
  numero
}

# El motor devolvio algo y R no lo pudo leer como numero: no es lo mismo que el
# motor no haya devuelto nada. Pasa de verdad -SQLite responde `MIN` de una
# columna `DATE` como el texto "2020-01-01", que `as.numeric()` convierte en
# `NA`- y el resultado se publicaba como `calculado` con valor `NA`, que es la
# contradiccion exacta que el paquete persigue: `NA` significa "no se midio" y
# `calculado` significa "se midio".
#
# La comprobacion es agnostica del motor y del tipo: si vino un valor no nulo y
# la conversion lo perdio, la metrica no se publica.
.valor_perdido_en_conversion_dbi <- function(crudo, convertido) {
  if (is.null(crudo) || !length(crudo)) return(FALSE)
  original <- crudo[[1L]]
  if (is.null(original) || !length(original) || is.na(original)) return(FALSE)
  !isTRUE(is.finite(convertido))
}

# Y el mismo cuidado con los enteros grandes: un `integer64` por encima de 2^53
# pierde exactitud al pasar a doble, y el maximo publicado seria un numero que
# no esta en la columna. Se comprueba con la vuelta completa, que no depende de
# donde caiga el redondeo.
.entero_perdido_en_conversion_dbi <- function(crudo) {
  if (is.null(crudo) || !length(crudo)) return(FALSE)
  original <- crudo[[1L]]
  if (!inherits(original, "integer64") || is.na(original)) return(FALSE)
  if (!.bit64_disponible_dbi()) return(FALSE)
  regreso <- tryCatch(
    bit64::as.integer64(as.numeric(original)), error = function(e) NULL
  )
  is.null(regreso) || is.na(regreso) || regreso != original
}

.metricas_omitidas_dbi <- function(registros, columna, metricas, estado,
                                    motivo, metadatos = NULL) {
  campos <- unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE)
  if (!length(campos)) return(registros)
  c(registros, list(
    .registro_sql_dbi(
      columna, campos, estado, motivo, NA_character_, metadatos = metadatos
    )
  ))
}

.motivo_decision_costo_dbi <- function(decision, metrica) {
  if (is.null(decision) || is.null(decision$detalle)) return(NA_character_)
  especifica <- decision$detalle[[metrica]]
  if (is.list(especifica) && length(especifica$motivo)) {
    return(as.character(especifica$motivo))
  }
  # Alias de compatibilidad para decisiones creadas por extensiones que aun no
  # publican el detalle separado.
  decision$detalle$motivo
}

.decision_costo_dbi <- function(columna, conteos, politica,
                                n_validos = NA_real_, n_distintos = NA_real_,
                                alcance = "la tabla",
                                fuente_cardinalidad_costo = NULL) {
  conservar <- list(moda = TRUE, mediana = TRUE)
  fuente <- fuente_cardinalidad_costo
  if (is.null(fuente)) fuente <- .fuente_cardinalidad_desconocida_dbi()
  motivo_mediana <- paste(
    "La mediana se conserva: no se aplica un umbral de proporcion porque su",
    "costo medido permanece plano frente a la cardinalidad y lo gobierna el",
    "numero de filas. `umbral_cardinalidad` gobierna solo la moda."
  )
  detalle <- list(
    n_validos = n_validos, n_distintos = n_distintos,
    proporcion_distintos = NA_real_, motivo = NA_character_,
    fuente_cardinalidad_costo = fuente$nombre,
    umbral_cardinalidad = politica$umbral,
    moda = list(
      estado = "conservada", motivo = NA_character_,
      criterio = "proporcion_distintos", umbral = politica$umbral
    ),
    mediana = list(
      estado = "conservada", motivo = motivo_mediana,
      criterio = "filas", umbral = NA_real_
    )
  )
  if (!identical(politica$nombre, "por_cardinalidad")) {
    detalle$moda$motivo <- paste0(
      "La moda se conserva porque `politica_costo = \"", politica$nombre,
      "\"` no aplica el criterio de cardinalidad."
    )
    detalle$motivo <- detalle$moda$motivo
    return(c(conservar, detalle = list(detalle)))
  }
  validos <- .numero_dbi(n_validos)
  distintos <- .numero_dbi(n_distintos)
  proporcion <- if (is.finite(validos) && validos > 0 &&
                    is.finite(distintos) && distintos >= 0) {
    distintos / validos
  } else {
    NA_real_
  }
  distintos_fuente <- .numero_dbi(fuente$n_distintos)
  if (is.finite(distintos_fuente) && distintos_fuente >= 0) {
    distintos <- distintos_fuente
    if (is.finite(fuente$proporcion_distintos)) {
      proporcion <- as.numeric(fuente$proporcion_distintos)
    } else if (is.finite(validos) && validos > 0) {
      proporcion <- distintos / validos
    }
    detalle$fuente_cardinalidad_costo <- fuente$nombre
  } else if (!is.null(fuente$proporcion_distintos) &&
      is.finite(fuente$proporcion_distintos)) {
    proporcion <- as.numeric(fuente$proporcion_distintos)
    if (!is.finite(validos) && proporcion == 1) validos <- NA_real_
    if (!is.finite(distintos) && is.finite(validos) && proporcion == 1) {
      distintos <- validos
    }
    detalle$fuente_cardinalidad_costo <- fuente$nombre
  } else if (is.finite(proporcion)) {
    detalle$fuente_cardinalidad_costo <- "medicion_exacta"
  }
  detalle$n_validos <- if (is.finite(validos)) validos else n_validos
  detalle$n_distintos <- if (is.finite(distintos)) distintos else n_distintos
  detalle$proporcion_distintos <- proporcion
  if (is.finite(proporcion) && proporcion >= politica$umbral) {
    conservar$moda <- FALSE
    detalle$moda$estado <- "omitida_por_costo"
    detalle$moda$motivo <- paste0(
      "Se omitio la moda de la columna `", columna, "`: tiene ", distintos,
      " valores distintos de ",
      validos, " validos (", formatC(proporcion, format = "f", digits = 3),
      ") sobre ", alcance, "; la politica optativa considera que agrupar u",
      " ordenar toda la columna no justifica el costo. Para pedirla igual, use",
      " `politica_costo = \"todas\"`; para mover el criterio, cambie",
      " `umbral_cardinalidad`."
    )
  } else if (is.finite(proporcion)) {
    detalle$moda$motivo <- paste0(
      "La moda se conserva: la proporcion de distintos (",
      formatC(proporcion, format = "f", digits = 3), ") queda por debajo de ",
      "`umbral_cardinalidad` (",
      formatC(politica$umbral, format = "f", digits = 3), ")."
    )
  } else {
    detalle$moda$motivo <- paste(
      "La moda se conserva porque no se pudo determinar una proporcion de",
      "distintos utilizable para aplicar `umbral_cardinalidad`."
    )
  }
  detalle$motivo <- detalle$moda$motivo
  c(conservar, detalle = list(detalle))
}

.decisiones_costo_dbi <- function(conexion, columnas, agregados, politica,
                                  n_total, universo = "tabla_completa",
                                  fuentes_cardinalidad_costo = NULL) {
  salida <- vector("list", length(columnas))
  names(salida) <- columnas
  alcance <- if (identical(universo, "muestra_motor")) "la muestra medida" else
    "la tabla completa"
  for (columna in columnas) {
    conteo <- agregados$conteos[[columna]]
    validos <- if (is.null(conteo) || is.null(conteo$validos)) {
      NA_real_
    } else .numero_dbi(conteo$validos$valor)
    distintos <- if (is.null(conteo) || is.null(conteo$distintos)) {
      NA_real_
    } else .numero_dbi(conteo$distintos$valor)
    fuente <- if (is.null(fuentes_cardinalidad_costo)) {
      NULL
    } else {
      fuentes_cardinalidad_costo[[columna]]
    }
    salida[[columna]] <- .decision_costo_dbi(
      columna, conteo, politica, validos, distintos, alcance,
      fuente_cardinalidad_costo = fuente
    )
  }
  salida
}

.piezas_tabla_cardinalidad_dbi <- function(tabla) {
  if (isTRUE(.es_id_dbi(tabla))) {
    piezas <- .partes_id_dbi(tabla)
    return(list(
      esquema = if ("esquema" %in% names(piezas)) piezas[["esquema"]] else NA_character_,
      tabla = unname(piezas[["tabla"]])
    ))
  }
  if (is.character(tabla) && length(tabla) == 1L && !is.na(tabla)) {
    cortado <- .partir_identificador(tabla)
    partes <- vapply(
      cortado$partes, .quitar_comillas_identificador, character(1L),
      USE.NAMES = FALSE
    )
    if (length(partes) == 2L) {
      return(list(esquema = partes[[1L]], tabla = partes[[2L]]))
    }
    return(list(esquema = NA_character_, tabla = partes[[1L]]))
  }
  list(esquema = NA_character_, tabla = NA_character_)
}

.resolver_fuentes_cardinalidad_dbi <- function(conexion, tabla, columnas,
                                               estrategia, presupuesto) {
  fuentes <- .fuentes_cardinalidad_vacias_dbi(columnas)
  if (identical(estrategia$estrategia_solicitada, "catalogo") &&
      is.list(estrategia$fuentes)) {
    for (columna in intersect(names(fuentes), names(estrategia$fuentes))) {
      fuentes[[columna]] <- estrategia$fuentes[[columna]]
    }
  }
  # La clave se lee siempre para publicarla en `meta$clave`. Su resultado solo
  # gobierna la politica de costo cuando esa politica lo necesita.
  piezas <- .piezas_tabla_cardinalidad_dbi(tabla)
  catalogo <- .clave_primaria_dbi(
    conexion, piezas$tabla, piezas$esquema, presupuesto = presupuesto
  )
  # Red universal, por encima del filtro de cada motor: una clave cuyas columnas
  # no estan entre las que se acaban de medir no puede ser de la tabla medida.
  #
  # Existe porque el filtro por esquema resuelve el caso de CADA motor conocido,
  # y esta clase de fallo aparece justamente en los que no se conocen: el
  # catalogo contesta por una homonima y la respuesta llega con la forma de una
  # respuesta buena. Esta comprobacion no necesita saber la precedencia de nadie
  # -compara contra las columnas que el propio perfilado leyo-, asi que cubre
  # tambien al motor que todavia no se probo. Se descubrio sobre SQL Server el
  # 2026-08-30: publicaba clave en `x` para una tabla de columnas `y, dato`.
  # `length(columnas)` no es defensa de rutina: si el esquema de la tabla no se
  # pudo leer, la lista llega vacia y TODA columna de la clave pareceria ajena.
  # Descartar una clave buena porque no se sabe contra que compararla seria
  # silenciar por ignorancia, que es el error opuesto y de igual importancia.
  if (length(catalogo$columnas) && length(columnas)) {
    ajenas <- catalogo$columnas[
      is.na(.resolver_columnas_dbi(catalogo$columnas, columnas))
    ]
    if (length(ajenas)) catalogo <- .clave_de_otra_relacion(catalogo, ajenas)
  }
  # Una clave compuesta no vuelve unica cada columna por separado. Solo una
  # clave primaria simple, aplicada y validada, permite afirmar que su columna
  # tiene tantos distintos como valores validos, sin contarla.
  if (isTRUE(estrategia$para_costo) &&
      length(catalogo$columnas) == 1L &&
      identical(catalogo$garantia, "garantizada")) {
    posicion <- .resolver_columnas_dbi(catalogo$columnas, columnas)
    if (length(posicion) == 1L && !is.na(posicion)) {
      columna <- columnas[[posicion]]
      fuentes[[columna]] <- list(
        nombre = "clave_primaria_garantizada", exacta = TRUE,
        proporcion_distintos = 1, motivo = paste(
          "La clave primaria simple esta declarada y garantizada por el",
          "catalogo; sus valores validos son unicos."
        )
      )
    }
  }
  list(fuentes = fuentes, catalogo = catalogo)
}

.columnas_distintos_ejecucion_dbi <- function(
    metricas, estrategia_distintos, campos,
    fuentes_cardinalidad_costo = NULL, politica_costo = NULL) {
  if (!"distintos" %in% metricas) return(character())
  if (identical(estrategia_distintos$estado, "estimado_catalogo")) {
    return(character())
  }
  if (isTRUE(estrategia_distintos$publica)) return(campos)
  if (is.null(fuentes_cardinalidad_costo)) return(NULL)
  if (!is.null(politica_costo) &&
      identical(politica_costo$nombre, "por_cardinalidad")) {
    return(campos[vapply(
      fuentes_cardinalidad_costo,
      function(x) !isTRUE(x$exacta), logical(1L)
    )])
  }
  character()
}

# Los dos conteos recorren la misma tabla, asi que se piden juntos: una
# consulta por columna en vez de dos. `COUNT(DISTINCT ...)` no es universal, y
# consolidar acopla fallos, asi que si la consulta combinada se rechaza los dos
# conteos se reintentan por separado. Un rechazo no puede arrastrar a la otra
# metrica.
.conteos_columna_dbi <- function(conexion, tabla_sql, columna_sql, alias,
                                 pide_validos, pide_distintos,
                                 presupuesto = NULL,
                                 aproximacion_distintos = NULL,
                                 incluir_total = FALSE) {
  total_alias <- alias("n_total_consulta")
  sql_validos <- paste0(
    "SELECT ", if (isTRUE(incluir_total)) {
      paste0("COUNT(*) AS ", total_alias, ", ")
    } else "",
    "COUNT(", columna_sql, ") AS ", alias("n_validos"),
    " FROM ", tabla_sql
  )
  expresion_distintos <- if (is.null(aproximacion_distintos)) {
    c(
      paste0("COUNT(", columna_sql, ") AS ", alias("n_validos_guard")),
      paste0(
        "COUNT(DISTINCT ", columna_sql, ") AS ", alias("n_distintos")
      )
    )
  } else if (is.function(aproximacion_distintos$expresion)) {
    c(
      paste0("COUNT(", columna_sql, ") AS ", alias("n_validos_guard")),
      aproximacion_distintos$expresion(
        columna_sql, alias("n_distintos")
      )
    )
  } else {
    NULL
  }
  sql_distintos <- if (!is.null(aproximacion_distintos)) {
    if (is.function(aproximacion_distintos$expresion)) {
      paste0(
        "SELECT ", paste(expresion_distintos, collapse = ", "),
        " FROM ", tabla_sql
      )
    } else {
      aproximacion_distintos$construir(
        columna_sql, tabla_sql, alias("n_distintos")
      )
    }
  } else {
    paste0(
      "SELECT ", paste(expresion_distintos, collapse = ", "),
      " FROM ", tabla_sql
    )
  }
  puede_consolidar <- is.character(expresion_distintos) &&
    length(expresion_distintos) > 0L &&
    all(!is.na(expresion_distintos) & nzchar(trimws(expresion_distintos)))
  if (pide_validos && pide_distintos) {
    if (puede_consolidar) {
      sql <- paste0(
        "SELECT ", if (isTRUE(incluir_total)) {
          paste0("COUNT(*) AS ", total_alias, ", ")
        } else "",
        "COUNT(", columna_sql, ") AS ", alias("n_validos"),
        ", ", paste(expresion_distintos, collapse = ", "),
        " FROM ", tabla_sql
      )
      consulta <- .consultar_dbi(
        conexion, sql, presupuesto, etapa = "conteos"
      )
      if (isTRUE(consulta$ok)) {
        validos <- .valor_campo_dbi(consulta$datos, "n_validos")
        distintos <- .valor_campo_dbi(consulta$datos, "n_distintos")
        if (isTRUE(validos$ok) && isTRUE(distintos$ok)) {
          validos$sql <- sql
          distintos$sql <- sql
          validos <- .adjuntar_medicion_dbi(validos, consulta)
          distintos <- .adjuntar_medicion_dbi(distintos, consulta)
          validos <- .adjuntar_denominador_consulta_dbi(
            validos, consulta, if (isTRUE(incluir_total)) total_alias else NULL
          )
          distintos <- .adjuntar_guardian_distintos_dbi(
            distintos, consulta, alias("n_validos_guard")
          )
          if (!is.null(aproximacion_distintos)) {
            distintos$metadatos <- .mezclar_metadatos_dbi(
              distintos$metadatos, list(
              metodo = aproximacion_distintos$nombre,
              error_esperado = aproximacion_distintos$error_esperado
              )
            )
            distintos$estado <- "estimado"
          }
          salida <- list(
            validos = validos, distintos = distintos, consolidada = TRUE
          )
          if (isTRUE(incluir_total)) {
            salida$conteo <- .valor_campo_dbi(
              consulta$datos, .nombre_alias_dbi(total_alias)
            )
            salida$conteo$sql <- sql
            salida$conteo$metadatos <- list(
              id_consulta = as.integer(consulta$consulta_id)
            )
            salida$conteo <- .adjuntar_medicion_dbi(
              salida$conteo, consulta
            )
          }
          return(salida)
        }
      }
    }
  }
  resultado <- list(consolidada = FALSE)
  if (pide_validos) {
    consulta_validos <- .consultar_dbi(
      conexion, sql_validos, presupuesto, etapa = "conteos"
    )
    validos <- .resultado_lote_dbi(
      consulta_validos, sql_validos, alias("n_validos"), list()
    )
    validos <- .adjuntar_denominador_consulta_dbi(
      validos, consulta_validos,
      if (isTRUE(incluir_total)) total_alias else NULL
    )
    resultado$validos <- validos
    if (isTRUE(incluir_total) && isTRUE(validos$ok)) {
      resultado$conteo <- .resultado_lote_dbi(
        consulta_validos, sql_validos, total_alias, list(
          id_consulta = as.integer(consulta_validos$consulta_id)
        )
      )
    }
  }
  if (pide_distintos) {
    consulta_distintos <- .consultar_dbi(
      conexion, sql_distintos, presupuesto, etapa = "conteos"
    )
    distintos <- .resultado_lote_dbi(
      consulta_distintos, sql_distintos, alias("n_distintos"), list()
    )
    distintos <- .adjuntar_guardian_distintos_dbi(
      distintos, consulta_distintos,
      if (isTRUE(puede_consolidar)) alias("n_validos_guard") else NULL
    )
    if (!is.null(aproximacion_distintos) && isTRUE(distintos$ok)) {
      distintos$metadatos <- .mezclar_metadatos_dbi(
        distintos$metadatos, list(
          metodo = aproximacion_distintos$nombre,
          error_esperado = aproximacion_distintos$error_esperado
        )
      )
      distintos$estado <- "estimado"
    }
    resultado$distintos <- distintos
  }
  resultado
}

.lotes_columnas_dbi <- function(columnas, tamano_lote) {
  if (!length(columnas)) return(list())
  split(columnas, ceiling(seq_along(columnas) / tamano_lote))
}

.metadatos_lote_dbi <- function(numero, columnas) {
  list(
    lote = as.integer(numero),
    columnas_compartidas = as.integer(length(columnas)),
    id_consulta = NA_integer_
  )
}

.alias_agregado_dbi <- function(alias, lote, posicion, metrica) {
  alias(paste0("lupa_l", lote, "_c", posicion, "_", metrica))
}

.nombre_alias_dbi <- function(alias) {
  texto <- as.character(alias)
  texto <- sub("^`(.*)`$", "\\1", texto)
  texto <- sub('^"(.*)"$', "\\1", texto)
  sub("^\\[(.*)\\]$", "\\1", texto)
}

.resultado_lote_dbi <- function(consulta, sql, alias, metadatos) {
  if (!consulta$ok) {
    return(.adjuntar_medicion_dbi(list(
      ok = FALSE, valor = NULL, motivo = consulta$motivo, sql = sql,
      metadatos = metadatos
    ), consulta))
  }
  resultado <- .valor_campo_dbi(consulta$datos, .nombre_alias_dbi(alias))
  resultado$sql <- sql
  resultado$metadatos <- metadatos
  if (isTRUE(resultado$ok) && !is.null(consulta$consulta_id) &&
      length(consulta$consulta_id) == 1L && !is.na(consulta$consulta_id)) {
    resultado$metadatos$id_consulta <- as.integer(consulta$consulta_id)
  }
  .adjuntar_medicion_dbi(resultado, consulta)
}

.valor_denominador_consulta_dbi <- function(consulta, alias) {
  vacio <- list(
    n_total_consulta = NA_real_,
    consulta_id_denominador = NA_integer_
  )
  if (is.null(alias) || !isTRUE(consulta$ok)) return(vacio)
  celda <- .valor_campo_dbi(consulta$datos, .nombre_alias_dbi(alias))
  if (!isTRUE(celda$ok)) return(vacio)
  valor <- .conteo_dbi(celda$valor)
  if (is.na(valor)) return(vacio)
  list(
    n_total_consulta = valor,
    consulta_id_denominador = as.integer(consulta$consulta_id)
  )
}

.adjuntar_denominador_consulta_dbi <- function(resultado, consulta, alias) {
  resultado$metadatos <- .mezclar_metadatos_dbi(
    resultado$metadatos,
    .valor_denominador_consulta_dbi(consulta, alias)
  )
  resultado
}

.adjuntar_guardian_dbi <- function(resultado, consulta, alias) {
  guardian <- list(
    n_validos_guard = NA_real_,
    consulta_id_guard = NA_integer_,
    cota_comprobable = FALSE
  )
  if (!is.null(alias) && isTRUE(consulta$ok)) {
    celda <- .valor_campo_dbi(consulta$datos, .nombre_alias_dbi(alias))
    if (isTRUE(celda$ok)) {
      valor <- .conteo_dbi(celda$valor)
      if (!is.na(valor)) {
        guardian$n_validos_guard <- valor
        guardian$consulta_id_guard <- as.integer(consulta$consulta_id)
        guardian$cota_comprobable <- !is.na(consulta$consulta_id)
      }
    }
  }
  resultado$metadatos <- .mezclar_metadatos_dbi(
    resultado$metadatos, guardian
  )
  resultado
}

.adjuntar_guardian_distintos_dbi <- function(resultado, consulta, alias) {
  .adjuntar_guardian_dbi(resultado, consulta, alias)
}

.adjuntar_guardian_moda_dbi <- function(resultado, consulta, alias) {
  .adjuntar_guardian_dbi(resultado, consulta, alias)
}

.conteo_desde_consulta_dbi <- function(consulta, sql, alias) {
  if (!isTRUE(consulta$ok)) return(NULL)
  resultado <- .valor_campo_dbi(
    consulta$datos, .nombre_alias_dbi(alias)
  )
  if (!isTRUE(resultado$ok) || is.na(.conteo_dbi(resultado$valor))) {
    return(NULL)
  }
  resultado$sql <- sql
  resultado$metadatos <- list(
    id_consulta = as.integer(consulta$consulta_id)
  )
  .adjuntar_medicion_dbi(resultado, consulta)
}

.resultado_lote_campos_dbi <- function(consulta, sql, alias, metricas,
                                       metadatos) {
  if (!consulta$ok) {
    return(.adjuntar_medicion_dbi(list(
      ok = FALSE, datos = NULL, motivo = consulta$motivo, sql = sql,
      metadatos = metadatos
    ), consulta))
  }
  celdas <- lapply(metricas, function(metrica) {
    .valor_campo_dbi(
      consulta$datos, .nombre_alias_dbi(alias[[metrica]])
    )
  })
  fallas <- vapply(celdas, function(celda) !isTRUE(celda$ok), logical(1L))
  if (any(fallas)) {
    return(.adjuntar_medicion_dbi(list(
      ok = FALSE, datos = NULL,
      motivo = paste(vapply(celdas[fallas], `[[`, character(1L), "motivo"),
                     collapse = " "),
      sql = sql, metadatos = metadatos
    ), consulta))
  }
  valores <- lapply(celdas, `[[`, "valor")
  names(valores) <- metricas
  datos <- as.data.frame(valores, stringsAsFactors = FALSE)
  .adjuntar_medicion_dbi(
    list(ok = TRUE, datos = datos, motivo = NA_character_, sql = sql,
         metadatos = utils::modifyList(
           metadatos,
           if (!is.null(consulta$consulta_id) &&
               length(consulta$consulta_id) == 1L &&
               !is.na(consulta$consulta_id)) {
             list(id_consulta = as.integer(consulta$consulta_id))
           } else list()
         )),
    consulta
  )
}

.basicos_columna_dbi <- function(conexion, tabla_sql, columna_sql, alias,
                                 incluir_valores, presupuesto = NULL,
                                 metadatos = NULL) {
  partes <- c(
    if (incluir_valores) paste0("MIN(", columna_sql, ") AS ", alias("minimo")),
    if (incluir_valores) paste0("MAX(", columna_sql, ") AS ", alias("maximo")),
    paste0("AVG(", columna_sql, " * 1.0) AS ", alias("media")),
    paste0(
      "SUM(CASE WHEN ", columna_sql, " = 0 THEN 1 ELSE 0 END) AS ",
      alias("n_ceros")
    ),
    paste0(
      "SUM(CASE WHEN ", columna_sql, " < 0 THEN 1 ELSE 0 END) AS ",
      alias("n_negativos")
    )
  )
  sql <- paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql)
  resultado <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "basicos"
  )
  resultado$sql <- sql
  resultado$metadatos <- metadatos
  resultado
}

.formas_desvio_dbi <- function(tabla_sql, columna_sql, alias) {
  media_sql <- paste0(
    "(SELECT AVG(", columna_sql, " * 1.0) FROM ", tabla_sql, ")"
  )
  list(
    paste0(
      "STDDEV_SAMP(", columna_sql, " * 1.0) AS ", alias("desvio")
    ),
    paste0(
      "STDEV(", columna_sql, " * 1.0) AS ", alias("desvio")
    ),
    paste0(
      "SQRT(SUM((", columna_sql, " - ", media_sql, ") * (",
      columna_sql, " - ", media_sql, ")) / (COUNT(", columna_sql,
      ") - 1.0)) AS ", alias("desvio")
    )
  )
}

.desvio_columna_dbi <- function(conexion, tabla_sql, columna_sql, alias,
                                forma, presupuesto = NULL, metadatos = NULL) {
  formas <- .formas_desvio_dbi(tabla_sql, columna_sql, alias)
  sql <- paste0("SELECT ", formas[[forma]], " FROM ", tabla_sql)
  resultado <- .escalar_dbi(
    conexion, sql, "desvio", presupuesto, etapa = "desvio"
  )
  resultado$sql <- sql
  resultado$metadatos <- metadatos
  resultado
}

.guardian_dbi <- function(resultado) {
  metadatos <- resultado$metadatos
  guardado <- if (is.null(metadatos)) NA_real_ else {
    .numero_dbi(metadatos$n_validos_guard)
  }
  id_resultado <- suppressWarnings(as.integer(resultado$consulta_id))
  id_guardian <- if (is.null(metadatos)) NA_integer_ else {
    suppressWarnings(as.integer(metadatos$consulta_id_guard))
  }
  comprobable <- isTRUE(metadatos$cota_comprobable) &&
    length(guardado) == 1L && !is.na(guardado) && is.finite(guardado) &&
    length(id_resultado) == 1L && !is.na(id_resultado) &&
    length(id_guardian) == 1L && !is.na(id_guardian) &&
    identical(id_resultado, id_guardian)
  list(valor = guardado, comprobable = comprobable)
}

.guardian_distintos_dbi <- function(resultado) .guardian_dbi(resultado)

.guardian_moda_dbi <- function(resultado) .guardian_dbi(resultado)

.motivo_cota_moda_comprobable_dbi <- function() {
  paste(
    "La cota simple `frecuencia_moda <= n_validos` se comprobo dentro de la misma",
    "sentencia mediante `n_validos_guard`."
  )
}

.motivo_cota_distintos_no_comprobable_dbi <- function() {
  paste(
    "No se pudo comprobar la cota `n_distintos <= n_validos`: los valores",
    "no tienen un guardian en la misma sentencia y pueden provenir de",
    "consultas distintas. No se atribuye una inconsistencia al motor."
  )
}

.motivo_cota_moda_no_comprobable_dbi <- function() {
  paste(
    "No se pudo comprobar una cota simple de frecuencia de la moda: la frecuencia",
    "y el numero de valores validos no tienen un guardian en la misma",
    "sentencia y pueden provenir de consultas distintas. No se atribuye una",
    "inconsistencia al motor."
  )
}

.medianas_lote_consolidadas_dbi <- function(
    conexion, tabla_sql, lote, nombres_sql, alias, numero, candidato,
    presupuesto, estado = "calculado", n_filas = NA_real_,
    n_medianas_pendientes = length(lote),
    usar_referencia_banco = FALSE) {
  aliases <- vapply(seq_along(lote), function(i) {
    .alias_agregado_dbi(alias, "mediana", numero, i)
  }, character(1L), USE.NAMES = FALSE)
  expresiones <- vapply(seq_along(lote), function(i) {
    candidato$expresion(nombres_sql[[lote[[i]]]], aliases[[i]])
  }, character(1L), USE.NAMES = FALSE)
  sql <- candidato$construir_multiple(expresiones, tabla_sql)
  if (!is.null(presupuesto)) {
    proyeccion <- .proyectar_costo_mediana_dbi(
      presupuesto, n_filas, n_medianas_pendientes,
      usar_referencia_banco = usar_referencia_banco
    )
    if (isTRUE(proyeccion$disponible)) {
      presupuesto$proyeccion_mediana <- proyeccion
      if (!isTRUE(presupuesto$aviso_mediana_emitido)) {
        avisado <- .avisar_costo_mediana_dbi(
          proyeccion,
          habilitado = if (is.null(presupuesto$avisar_costo_mediana)) TRUE else
            presupuesto$avisar_costo_mediana,
          umbral_segundos = if (is.null(presupuesto$umbral_segundos_aviso_mediana)) {
            .UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI
          } else presupuesto$umbral_segundos_aviso_mediana
        )
        if (isTRUE(avisado)) presupuesto$aviso_mediana_emitido <- TRUE
      }
    }
  }
  consulta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "medianas_consolidadas"
  )
  .registrar_referencia_mediana_dbi(
    presupuesto, consulta, n_filas = n_filas, n_medianas = length(lote)
  )
  salida <- vector("list", length(lote))
  names(salida) <- lote
  if (!isTRUE(consulta$ok)) {
    return(list(resultados = salida, sql = sql, ok = FALSE))
  }
  for (i in seq_along(lote)) {
    celda <- .valor_campo_dbi(
      consulta$datos, .nombre_alias_dbi(aliases[[i]])
    )
    if (!isTRUE(celda$ok)) next
    valor <- .escalar_finito_dbi(celda$valor)
    if (.valor_perdido_en_conversion_dbi(celda$valor, valor)) next
    metadatos_resultado <- .metadatos_lote_dbi(numero, lote)
    metadatos_resultado <- c(
      metadatos_resultado,
      list(
        metodo = candidato$nombre,
        error_esperado = if (identical(estado, "estimado")) {
          candidato$error_esperado
        } else {
          "no_aplica"
        }
      )
    )
    salida[[lote[[i]]]] <- .adjuntar_medicion_dbi(
      list(
        ok = TRUE, valor = valor, motivo = NA_character_, sql = sql,
        estado = estado,
        metadatos = metadatos_resultado
      ), consulta
    )
  }
  list(resultados = salida, sql = sql, ok = TRUE)
}

.medianas_consolidadas_dbi <- function(
    conexion, tabla_sql, columnas, nombres_sql, alias, candidato,
    presupuesto, tamano_lote, estado = "calculado") {
  salida <- vector("list", length(columnas))
  names(salida) <- columnas
  if (is.null(candidato) || !length(columnas)) return(salida)
  lotes <- .lotes_columnas_dbi(columnas, tamano_lote)
  for (numero in seq_along(lotes)) {
    lote <- lotes[[numero]]
    pendientes <- sum(vapply(lotes[numero:length(lotes)], length, integer(1L)))
    resultado <- .medianas_lote_consolidadas_dbi(
      conexion, tabla_sql, lote, nombres_sql, alias, numero, candidato,
      presupuesto, estado = estado,
      n_filas = if (is.null(presupuesto)) NA_real_ else {
        if (is.null(presupuesto$filas_mediana)) NA_real_ else
          presupuesto$filas_mediana
      },
      n_medianas_pendientes = pendientes,
      usar_referencia_banco = length(lotes) == 1L
    )
    # Si la forma consolidada falla sobre la consulta real, no se transforma
    # ese fallo en una `NA` calculada: el llamador deja el resultado ausente y
    # usa el camino exacto por columna, que ya declara sus propios errores.
    for (columna in lote) {
      if (!is.null(resultado$resultados[[columna]])) {
        salida[[columna]] <- resultado$resultados[[columna]]
      }
    }
    pendientes_estado <- if (is.null(presupuesto)) NA_real_ else
      .numero_dbi(presupuesto$medianas_pendientes)
    if (!is.null(presupuesto) && isTRUE(resultado$ok) &&
        length(pendientes_estado) == 1L && is.finite(pendientes_estado)) {
      presupuesto$medianas_pendientes <- max(
        0, pendientes_estado - length(lote)
      )
    }
    if (!is.null(presupuesto) && isTRUE(resultado$ok) &&
        pendientes > length(lote)) {
      proyeccion <- .proyectar_costo_mediana_dbi(
        presupuesto, presupuesto$filas_mediana, pendientes - length(lote)
      )
      presupuesto$proyeccion_mediana <- proyeccion
    }
  }
  salida
}

.resultado_agregado_no_emitido_dbi <- function(etapa, motivo,
                                               sql = NA_character_) {
  list(
    ok = FALSE, valor = NULL, datos = NULL, motivo = motivo, sql = sql,
    metadatos = NULL, consulta_id = NA_integer_, etapa = etapa,
    duracion_ms = NA_real_, n_filas_resultado = NA_real_,
    bytes_resultado_r = NA_real_, cpu_ms = NA_real_
  )
}

.resultados_lote_agregados_dbi <- function(consulta, sql,
                                           alias_por_columna, metricas,
                                           metricas_basicos, metadatos) {
  nombres <- names(alias_por_columna)
  salida <- vector("list", length(nombres))
  names(salida) <- nombres
  for (i in seq_along(nombres)) {
    aliases <- alias_por_columna[[i]]
    resultado <- list(consolidada = TRUE)
    if (!is.null(aliases$total)) {
      resultado$conteo <- .resultado_lote_dbi(
        consulta, sql, aliases$total, metadatos
      )
    }
    if ("validos" %in% metricas) {
      resultado$validos <- .resultado_lote_dbi(
        consulta, sql, aliases$validos, metadatos
      )
      resultado$validos <- .adjuntar_denominador_consulta_dbi(
        resultado$validos, consulta, aliases$total
      )
    }
    if ("basicos" %in% metricas && !is.null(aliases$basicos)) {
      resultado$basicos <- .resultado_lote_campos_dbi(
        consulta, sql, aliases$basicos, metricas_basicos, metadatos
      )
      resultado$basicos <- .adjuntar_denominador_consulta_dbi(
        resultado$basicos, consulta, aliases$total
      )
    }
    if ("desvio" %in% metricas && !is.null(aliases$desvio)) {
      resultado$desvio <- .resultado_lote_dbi(
        consulta, sql, aliases$desvio, metadatos
      )
      resultado$desvio <- .adjuntar_denominador_consulta_dbi(
        resultado$desvio, consulta, aliases$total
      )
    }
    salida[[i]] <- resultado
  }
  salida
}

.tope_sondas_agregados_dbi <- function(presupuesto, n) {
  saldo <- if (is.null(presupuesto)) Inf else .saldo_dbi(presupuesto)
  # La otra mitad queda disponible para metricas que aun no se sondearon. Es
  # la misma reserva que usa la lectura de muestra: recuperar una parte no
  # puede dejar sin presupuesto el resto del perfil.
  min(2 * n, .TOPE_SONDAS_DESCARTE_DBI, max(0, floor(saldo / 2)))
}

.recordar_lote_agregados_dbi <- function(presupuesto, n, familia = "planos") {
  if (is.null(presupuesto) || n < 1) return(invisible(NULL))
  ranura <- if (identical(familia, "distintos")) {
    "tamano_lote_distintos_funciono"
  } else {
    "tamano_lote_planos_funciono"
  }
  previo <- presupuesto[[ranura]]
  if (is.null(previo) || n > previo) presupuesto[[ranura]] <- n
  # Alias de compatibilidad: conserva el mayor lote plano, que era el unico
  # lote que existia antes de separar las familias.
  previo_total <- presupuesto$tamano_lote_funciono
  if (is.null(previo_total) || n > previo_total) {
    presupuesto$tamano_lote_funciono <- n
  }
  invisible(NULL)
}

.agregados_lote_con_biseccion_dbi <- function(
    conexion, tabla_sql, lote, nombres_sql, es_numerico, metricas,
    incluir_valores, presupuesto, tamano_lote, numero, alias, forma,
    etapa = "agregados", incluir_total = FALSE,
    tabla_total_sql = tabla_sql) {
  metricas_basicos <- if ("basicos" %in% metricas) {
    if (incluir_valores) {
      c("minimo", "maximo", "media", "n_ceros", "n_negativos")
    } else {
      c("media", "n_ceros", "n_negativos")
    }
  } else {
    character()
  }
  alias_por_columna <- vector("list", length(lote))
  names(alias_por_columna) <- lote
  for (i in seq_along(lote)) {
    alias_por_columna[[i]] <- list(
      total = if (isTRUE(incluir_total)) alias("n_total_consulta"),
      validos = if ("validos" %in% metricas) {
        .alias_agregado_dbi(alias, numero, i, "n_validos")
      },
      basicos = if ("basicos" %in% metricas && es_numerico[[i]]) {
        salida <- lapply(
          metricas_basicos,
          function(metrica) .alias_agregado_dbi(alias, numero, i, metrica)
        )
        names(salida) <- metricas_basicos
        salida
      },
      desvio = if ("desvio" %in% metricas && es_numerico[[i]]) {
        .alias_agregado_dbi(alias, numero, i, "desvio")
      }
    )
  }
  expresiones_columna <- function(i) {
    campo <- lote[[i]]
    aliases <- alias_por_columna[[i]]
    c(
      if ("validos" %in% metricas) paste0(
        "COUNT(", nombres_sql[[campo]], ") AS ", aliases$validos
      ),
      if ("basicos" %in% metricas && es_numerico[[i]]) c(
        if (incluir_valores) paste0(
          "MIN(", nombres_sql[[campo]], ") AS ", aliases$basicos$minimo
        ),
        if (incluir_valores) paste0(
          "MAX(", nombres_sql[[campo]], ") AS ", aliases$basicos$maximo
        ),
        paste0(
          "AVG(", nombres_sql[[campo]], " * 1.0) AS ",
          aliases$basicos$media
        ),
        paste0(
          "SUM(CASE WHEN ", nombres_sql[[campo]],
          " = 0 THEN 1 ELSE 0 END) AS ", aliases$basicos$n_ceros
        ),
        paste0(
          "SUM(CASE WHEN ", nombres_sql[[campo]],
          " < 0 THEN 1 ELSE 0 END) AS ", aliases$basicos$n_negativos
        )
      ),
      if ("desvio" %in% metricas && es_numerico[[i]]) {
        formas <- .formas_desvio_dbi(
          tabla_sql, nombres_sql[[campo]],
          function(nombre) aliases$desvio
        )
        formas[[forma]]
      }
    )
  }
  total_alias <- alias("n_total_consulta")
  expresion_total <- paste0("COUNT(*) AS ", total_alias)
  universo_alias <- alias("lupa_n_total")
  expresion_universo <- paste0(
    "(SELECT COUNT(*) FROM ", tabla_total_sql, ") AS ", universo_alias
  )
  construir <- function(indices, con_total = FALSE, con_universo = FALSE) {
    expresiones <- c(
      if (isTRUE(con_total)) expresion_total,
      if (isTRUE(con_universo)) expresion_universo,
      unlist(lapply(indices, expresiones_columna), use.names = FALSE)
    )
    paste0(
      "SELECT ", paste(expresiones, collapse = ", "), " FROM ", tabla_sql
    )
  }

  cache <- list()
  clave <- function(indices) paste(indices, collapse = ",")
  sondear <- function(indices, con_total = incluir_total,
                      con_universo = FALSE) {
    llave <- paste(
      clave(indices), isTRUE(con_total), isTRUE(con_universo), sep = "|"
    )
    if (!is.null(cache[[llave]])) return(isTRUE(cache[[llave]]$ok))
    sql <- construir(
      indices, con_total = con_total, con_universo = con_universo
    )
    consulta <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = etapa
    )
    cache[[llave]] <<- list(ok = consulta$ok, consulta = consulta, sql = sql)
    isTRUE(consulta$ok)
  }

  completo <- seq_along(lote)
  universo_separado <- !identical(tabla_total_sql, tabla_sql)
  inicial <- sondear(
    completo, con_total = incluir_total,
    con_universo = incluir_total && universo_separado
  )
  conteo <- NULL
  if (isTRUE(incluir_total)) {
    entrada_total <- cache[[paste(
      clave(completo), isTRUE(incluir_total),
      isTRUE(incluir_total && universo_separado), sep = "|"
    )]]
    if (isTRUE(inicial)) {
      conteo <- .conteo_desde_consulta_dbi(
        entrada_total$consulta, entrada_total$sql,
        if (isTRUE(universo_separado)) universo_alias else total_alias
      )
    }
    if (is.null(conteo)) {
      sql_total <- paste0(
        "SELECT COUNT(*) AS ", universo_alias, " FROM ", tabla_total_sql
      )
      conteo <- .escalar_dbi(
        conexion, sql_total, .nombre_alias_dbi(universo_alias), presupuesto,
        etapa = "conteo_filas"
      )
      conteo$sql <- sql_total
      if (!isTRUE(conteo$ok)) conteo$metadatos <- NULL
    }
  }
  if (inicial) {
    grupos <- list(completo)
    culpables <- integer()
    pendientes <- integer()
    agotado <- FALSE
    sondas <- 0L
  } else {
    aislamiento <- .aislar_ilegibles_dbi(
      sondear, function() {
        is.null(presupuesto) || .saldo_dbi(presupuesto) >= 1
      }, length(lote), .tope_sondas_agregados_dbi(presupuesto, length(lote)),
      conservar_legibles = TRUE
    )
    grupos <- aislamiento$legibles
    culpables <- aislamiento$culpables
    pendientes <- unlist(aislamiento$pendientes, use.names = FALSE)
    agotado <- isTRUE(aislamiento$agotado)
    sondas <- aislamiento$sondas
  }
  if (length(grupos)) {
    .recordar_lote_agregados_dbi(
      presupuesto, max(vapply(grupos, length, integer(1L))), "planos"
    )
  }
  clasificados <- sort(unique(c(unlist(grupos, use.names = FALSE), culpables)))
  desconocidos <- setdiff(seq_along(lote), clasificados)
  if (length(desconocidos)) {
    pendientes <- sort(unique(c(pendientes, desconocidos)))
  }

  resultado <- vector("list", length(lote))
  names(resultado) <- lote
  for (grupo in grupos) {
    llave <- paste(
      clave(grupo), isTRUE(incluir_total),
      isTRUE(universo_separado && identical(grupo, completo)), sep = "|"
    )
    entrada <- cache[[llave]]
    if (is.null(conteo) && isTRUE(incluir_total) && !isTRUE(universo_separado)) {
      conteo <- .conteo_desde_consulta_dbi(
        entrada$consulta, entrada$sql, total_alias
      )
    }
    metadatos <- .metadatos_lote_dbi(numero, lote[grupo])
    valores <- .resultados_lote_agregados_dbi(
      entrada$consulta, entrada$sql, alias_por_columna[grupo], metricas,
      metricas_basicos, metadatos
    )
    for (i in seq_along(grupo)) {
      resultado[[lote[[grupo[[i]]]]]] <- valores[[i]]
    }
  }

  # Una columna que falla en la sonda combinada no necesariamente falla para
  # todas las metricas: se reintenta por columna con las mismas puertas que
  # habia antes. La biseccion localiza donde mirar, pero no convierte el fallo
  # de un agregado en la perdida de los demas.
  for (i in culpables) {
    campo <- lote[[i]]
    metadatos <- .metadatos_lote_dbi(numero, campo)
    individual <- list(consolidada = FALSE)
    if ("validos" %in% metricas) {
      conteos_columna <- .conteos_columna_dbi(
        conexion, tabla_sql, nombres_sql[[campo]], alias,
        TRUE, FALSE, presupuesto, incluir_total = TRUE
      )
      individual$validos <- conteos_columna$validos
    }
    if ("basicos" %in% metricas && es_numerico[[i]]) {
      individual$basicos <- .basicos_columna_dbi(
        conexion, tabla_sql, nombres_sql[[campo]], alias, incluir_valores,
        presupuesto, metadatos
      )
    }
    if ("desvio" %in% metricas && es_numerico[[i]]) {
      individual$desvio <- .desvio_columna_dbi(
        conexion, tabla_sql, nombres_sql[[campo]], alias, forma,
        presupuesto, metadatos
      )
    }
    for (metrica in intersect(names(individual), c("validos", "basicos", "desvio"))) {
      individual[[metrica]]$metadatos <- .mezclar_metadatos_dbi(
        metadatos, individual[[metrica]]$metadatos
      )
    }
    resultado[[campo]] <- individual
  }

  if (length(pendientes)) {
    motivo <- paste(
      "El lote de agregados fue rechazado y no se pudieron aislar todas sus",
      "columnas dentro del presupuesto de sondas; las columnas pendientes no",
      "se suponen culpables ni legibles."
    )
    for (i in pendientes) {
      campo <- lote[[i]]
      metadatos <- .metadatos_lote_dbi(numero, campo)
      sin_consulta <- .resultado_agregado_no_emitido_dbi(etapa, motivo)
      individual <- list(consolidada = FALSE)
      if ("validos" %in% metricas) {
        individual$validos <- .resultado_lote_dbi(
          sin_consulta, NA_character_, alias_por_columna[[i]]$validos,
          metadatos
        )
      }
      if ("basicos" %in% metricas && !is.null(alias_por_columna[[i]]$basicos)) {
        individual$basicos <- .resultado_lote_campos_dbi(
          sin_consulta, NA_character_, alias_por_columna[[i]]$basicos,
          metricas_basicos, metadatos
        )
      }
      if ("desvio" %in% metricas && !is.null(alias_por_columna[[i]]$desvio)) {
        individual$desvio <- .resultado_lote_dbi(
          sin_consulta, NA_character_, alias_por_columna[[i]]$desvio,
          metadatos
        )
      }
      resultado[[campo]] <- individual
    }
  }
  list(
    resultados = resultado, sondas = sondas, agotado = agotado,
    inicial = inicial, conteo = conteo
  )
}

.conteos_distintos_lote_dbi <- function(
    conexion, tabla_sql, lote, nombres_sql, alias, numero, presupuesto,
    aproximacion_distintos = NULL, incluir_total = FALSE,
    tabla_total_sql = tabla_sql) {
  alias_por_columna <- vapply(seq_along(lote), function(i) {
    .alias_agregado_dbi(alias, numero, i, "n_distintos")
  }, character(1L), USE.NAMES = FALSE)
  guardian_por_columna <- vapply(seq_along(lote), function(i) {
    .alias_agregado_dbi(alias, numero, i, "n_validos_guard")
  }, character(1L), USE.NAMES = FALSE)
  if (!is.null(aproximacion_distintos) &&
      !is.function(aproximacion_distintos$expresion)) {
    salida <- vector("list", length(lote))
    names(salida) <- lote
    conteo <- NULL
    if (isTRUE(incluir_total)) {
      total_alias <- alias("lupa_n_total")
      sql_total <- paste0(
        "SELECT COUNT(*) AS ", total_alias, " FROM ", tabla_total_sql
      )
      conteo <- .escalar_dbi(
        conexion, sql_total, .nombre_alias_dbi(total_alias), presupuesto,
        etapa = "conteo_filas"
      )
      conteo$sql <- sql_total
    }
    for (i in seq_along(lote)) {
      sql <- aproximacion_distintos$construir(
        nombres_sql[[lote[[i]]]], tabla_sql, alias_por_columna[[i]]
      )
      resultado <- .escalar_dbi(
        conexion, sql, .nombre_alias_dbi(alias_por_columna[[i]]),
        presupuesto, etapa = "conteos"
      )
      resultado$sql <- sql
      resultado$metadatos <- .metadatos_lote_dbi(numero, lote[[i]])
      resultado$metadatos <- .mezclar_metadatos_dbi(
        resultado$metadatos,
        list(
          n_validos_guard = NA_real_,
          consulta_id_guard = NA_integer_,
          cota_comprobable = FALSE
        )
      )
      if (isTRUE(resultado$ok)) {
        resultado$metadatos <- .mezclar_metadatos_dbi(
          resultado$metadatos,
          list(
            metodo = aproximacion_distintos$nombre,
            error_esperado = aproximacion_distintos$error_esperado
          )
        )
        resultado$estado <- "estimado"
      }
      salida[[lote[[i]]]] <- list(
        consolidada = FALSE, distintos = resultado
      )
    }
    return(list(
      resultados = salida, sondas = 0L, agotado = isTRUE(presupuesto$agotado),
      inicial = FALSE, conteo = conteo
    ))
  }
  expresion <- function(i) {
    if (is.null(aproximacion_distintos)) {
      return(c(
        paste0(
          "COUNT(", nombres_sql[[lote[[i]]]], ") AS ",
          guardian_por_columna[[i]]
        ),
        paste0(
          "COUNT(DISTINCT ", nombres_sql[[lote[[i]]]], ") AS ",
          alias_por_columna[[i]]
        )
      ))
    }
    if (is.null(aproximacion_distintos$expresion)) return(NULL)
    c(
      paste0(
        "COUNT(", nombres_sql[[lote[[i]]]], ") AS ",
        guardian_por_columna[[i]]
      ),
      aproximacion_distintos$expresion(
        nombres_sql[[lote[[i]]]], alias_por_columna[[i]]
      )
    )
  }
  total_alias <- alias("lupa_n_total")
  expresion_total <- if (identical(tabla_total_sql, tabla_sql)) {
    paste0("COUNT(*) AS ", total_alias)
  } else {
    paste0("(SELECT COUNT(*) FROM ", tabla_total_sql, ") AS ", total_alias)
  }
  construir <- function(indices, con_total = FALSE) {
    expresiones <- c(
      if (isTRUE(con_total)) expresion_total,
      unlist(lapply(indices, expresion), use.names = FALSE)
    )
    paste0(
      "SELECT ", paste(expresiones, collapse = ", "), " FROM ", tabla_sql
    )
  }
  cache <- list()
  clave <- function(indices) paste(indices, collapse = ",")
  sondear <- function(indices, con_total = FALSE) {
    llave <- clave(indices)
    if (!is.null(cache[[llave]])) return(isTRUE(cache[[llave]]$ok))
    sql <- construir(indices, con_total = con_total)
    consulta <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "conteos"
    )
    cache[[llave]] <<- list(ok = consulta$ok, consulta = consulta, sql = sql)
    isTRUE(consulta$ok)
  }
  completo <- seq_along(lote)
  inicial <- sondear(completo, con_total = incluir_total)
  conteo <- NULL
  if (isTRUE(incluir_total)) {
    entrada_total <- cache[[clave(completo)]]
    if (isTRUE(inicial)) {
      valor_total <- .valor_campo_dbi(
        entrada_total$consulta$datos, .nombre_alias_dbi(total_alias)
      )
      if (isTRUE(valor_total$ok) &&
          !is.na(.conteo_dbi(valor_total$valor))) {
        conteo <- valor_total
        conteo$sql <- entrada_total$sql
        conteo$metadatos <- list(
          id_consulta = as.integer(entrada_total$consulta$consulta_id)
        )
        conteo <- .adjuntar_medicion_dbi(conteo, entrada_total$consulta)
      }
    }
    if (is.null(conteo)) {
      sql_total <- paste0(
        "SELECT COUNT(*) AS ", total_alias, " FROM ", tabla_total_sql
      )
      conteo <- .escalar_dbi(
        conexion, sql_total, .nombre_alias_dbi(total_alias), presupuesto,
        etapa = "conteo_filas"
      )
      conteo$sql <- sql_total
      if (!isTRUE(conteo$ok)) conteo$metadatos <- NULL
    }
  }
  if (inicial) {
    grupos <- list(completo)
    culpables <- integer()
    pendientes <- integer()
    agotado <- FALSE
    sondas <- 0L
  } else {
    aislamiento <- .aislar_ilegibles_dbi(
      sondear, function() {
        is.null(presupuesto) || .saldo_dbi(presupuesto) >= 1
      }, length(lote), .tope_sondas_agregados_dbi(presupuesto, length(lote)),
      conservar_legibles = TRUE
    )
    grupos <- aislamiento$legibles
    culpables <- aislamiento$culpables
    pendientes <- unlist(aislamiento$pendientes, use.names = FALSE)
    agotado <- isTRUE(aislamiento$agotado)
    sondas <- aislamiento$sondas
  }
  if (length(grupos)) {
    .recordar_lote_agregados_dbi(
      presupuesto, max(vapply(grupos, length, integer(1L))), "distintos"
    )
  }
  clasificados <- sort(unique(c(unlist(grupos, use.names = FALSE), culpables)))
  desconocidos <- setdiff(seq_along(lote), clasificados)
  if (length(desconocidos)) {
    pendientes <- sort(unique(c(pendientes, desconocidos)))
  }
  salida <- vector("list", length(lote))
  names(salida) <- lote
  estimar <- function(resultado) {
    if (is.null(aproximacion_distintos) || !isTRUE(resultado$ok)) {
      return(resultado)
    }
    resultado$metadatos <- .mezclar_metadatos_dbi(
      resultado$metadatos,
      list(
        metodo = aproximacion_distintos$nombre,
        error_esperado = aproximacion_distintos$error_esperado
      )
    )
    resultado$estado <- "estimado"
    resultado
  }
  for (grupo in grupos) {
    llave <- clave(grupo)
    entrada <- cache[[llave]]
    metadatos <- .metadatos_lote_dbi(numero, lote[grupo])
    for (i in seq_along(grupo)) {
      resultado <- .resultado_lote_dbi(
        entrada$consulta, entrada$sql, alias_por_columna[[grupo[[i]]]],
        metadatos
      )
      resultado <- .adjuntar_guardian_distintos_dbi(
        resultado, entrada$consulta,
        guardian_por_columna[[grupo[[i]]]]
      )
      salida[[lote[[grupo[[i]]]]]] <- list(
        consolidada = TRUE, distintos = estimar(resultado)
      )
    }
  }
  for (i in culpables) {
    campo <- lote[[i]]
    metadatos <- .metadatos_lote_dbi(numero, campo)
    resultado <- .conteos_columna_dbi(
      conexion, tabla_sql, nombres_sql[[campo]], alias,
      FALSE, TRUE, presupuesto, aproximacion_distintos
    )$distintos
    resultado$metadatos <- .mezclar_metadatos_dbi(
      metadatos, resultado$metadatos
    )
    salida[[campo]] <- list(
      consolidada = FALSE, distintos = estimar(resultado)
    )
  }
  if (length(pendientes)) {
    motivo <- paste(
      "El lote de conteos distintos fue rechazado y no se pudieron aislar",
      "todas sus columnas dentro del presupuesto de sondas; las columnas",
      "pendientes no se suponen culpables ni legibles."
    )
    for (i in pendientes) {
      campo <- lote[[i]]
      sin_consulta <- .resultado_agregado_no_emitido_dbi("conteos", motivo)
      sin_consulta <- .resultado_lote_dbi(
        sin_consulta, NA_character_, alias_por_columna[[i]],
        .metadatos_lote_dbi(numero, campo)
      )
      salida[[campo]] <- list(
        consolidada = FALSE, distintos = estimar(sin_consulta)
      )
    }
  }
  list(resultados = salida, sondas = sondas, agotado = agotado,
       conteo = conteo)
}

.agregados_consolidados_dbi <- function(conexion, tabla_sql, columnas,
                                        columnas_sql, es_numerico, metricas,
                                        incluir_valores, presupuesto,
                                        tamano_lote_planos,
                                        tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                                        aproximacion_distintos = NULL,
                                        tabla_total_sql = tabla_sql,
                                        conteo_inicial = NULL,
                                        columnas_distintos = NULL) {
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  por_columna <- function() {
    salida <- vector("list", length(columnas))
    names(salida) <- columnas
    salida
  }
  agregados <- list(
    conteos = por_columna(), basicos = por_columna(), desvio = por_columna()
  )
  configuracion_aviso <- function(nombre, defecto) {
    valor <- if (is.null(presupuesto)) NULL else presupuesto[[nombre]]
    if (is.null(valor)) defecto else valor
  }
  nombres_sql <- stats::setNames(columnas_sql, columnas)
  conteo <- conteo_inicial
  n_total <- if (is.null(conteo)) NA_real_ else .conteo_dbi(conteo$valor)
  tomar_conteo <- function(resultado) {
    if (is.null(conteo) && !is.null(resultado$conteo)) {
      conteo <<- resultado$conteo
      n_total <<- .conteo_dbi(conteo$valor)
    }
    invisible(NULL)
  }

  metricas_planas <- intersect(c("validos", "basicos", "desvio"), metricas)
  incluir_total_muestra <- !identical(tabla_total_sql, tabla_sql)
  numericas <- columnas[es_numerico]
  columnas_planas <- if ("validos" %in% metricas_planas) {
    columnas
  } else {
    numericas
  }
  if (length(metricas_planas) && length(columnas_planas)) {
    if ("desvio" %in% metricas_planas) .sondar_forma_desvio(
      conexion, presupuesto, alias
    )
    forma <- presupuesto$forma_desvio
    lotes <- .lotes_columnas_dbi(columnas_planas, tamano_lote_planos)
    etapa <- if ("basicos" %in% metricas_planas) {
      "basicos"
    } else if ("desvio" %in% metricas_planas) {
      "desvio"
    } else {
      "conteos"
    }
    for (numero in seq_along(lotes)) {
      lote <- lotes[[numero]]
      indices <- match(lote, columnas)
      es_numerico_lote <- es_numerico[indices]
      resultado_lote <- .agregados_lote_con_biseccion_dbi(
        conexion, tabla_sql, lote, nombres_sql, es_numerico_lote,
        metricas_planas, incluir_valores, presupuesto, tamano_lote_planos, numero,
        alias, forma, etapa,
        incluir_total = "validos" %in% metricas_planas || incluir_total_muestra,
        tabla_total_sql = tabla_total_sql
      )
      tomar_conteo(resultado_lote)
      resultado_lote <- resultado_lote$resultados
      for (campo in lote) {
        resultado <- resultado_lote[[campo]]
        # Un resultado con filas pero sin uno de sus alias no es un rechazo de
        # columna. Mantener el reintento anterior por metrica evita cambiar la
        # disponibilidad si un controlador pliega o renombra los alias.
        if ("validos" %in% metricas_planas &&
            !is.null(resultado$validos) && !isTRUE(resultado$validos$ok) &&
            isTRUE(resultado$consolidada)) {
          resultado$validos <- .conteos_columna_dbi(
            conexion, tabla_sql, nombres_sql[[campo]], alias,
            TRUE, FALSE, presupuesto, incluir_total = TRUE
          )$validos
        }
        if ("basicos" %in% metricas_planas &&
            !is.null(resultado$basicos) && !isTRUE(resultado$basicos$ok) &&
            isTRUE(resultado$consolidada)) {
          resultado$basicos <- .basicos_columna_dbi(
            conexion, tabla_sql, nombres_sql[[campo]], alias, incluir_valores,
            presupuesto, .metadatos_lote_dbi(numero, campo)
          )
        }
        if ("desvio" %in% metricas_planas &&
            !is.null(resultado$desvio) && !isTRUE(resultado$desvio$ok) &&
            isTRUE(resultado$consolidada)) {
          resultado$desvio <- .desvio_columna_dbi(
            conexion, tabla_sql, nombres_sql[[campo]], alias, forma,
            presupuesto, .metadatos_lote_dbi(numero, campo)
          )
        }
        if ("validos" %in% metricas_planas) {
          base_conteos <- agregados$conteos[[campo]]
          if (is.null(base_conteos)) base_conteos <- list(consolidada = TRUE)
          agregados$conteos[[campo]] <- utils::modifyList(
            base_conteos, list(validos = resultado$validos)
          )
        }
        if ("basicos" %in% metricas_planas && !is.null(resultado$basicos)) {
          agregados$basicos[[campo]] <- resultado$basicos
        }
        if ("desvio" %in% metricas_planas && !is.null(resultado$desvio)) {
          agregados$desvio[[campo]] <- resultado$desvio
        }
      }
    }
  }
  # Si no hubo una familia plana que pudiera llevar el total fusionado, se
  # obtiene ahora, antes de pagar cualquier cardinalidad exacta.
  if (is.null(conteo)) {
    total_alias <- alias("lupa_n_total")
    sql_total <- paste0(
      "SELECT COUNT(*) AS ", total_alias, " FROM ", tabla_total_sql
    )
    conteo <- .escalar_dbi(
      conexion, sql_total, .nombre_alias_dbi(total_alias), presupuesto,
      etapa = "conteo_filas"
    )
    conteo$sql <- sql_total
    n_total <- .conteo_dbi(conteo$valor)
  }
  if (!is.null(presupuesto) && !is.null(conteo) && isTRUE(conteo$ok)) {
    duracion_lectura <- .numero_dbi(conteo$duracion_ms)
    if (length(duracion_lectura) == 1L && is.finite(duracion_lectura) &&
        duracion_lectura > 0) {
      presupuesto$duracion_lectura_mediana <- duracion_lectura
    }
  }
  # Primero quedan disponibles los agregados planos y el total exacto que se
  # fusiona con su primera consulta. Despues se paga COUNT(DISTINCT),
  # que conserva su propia familia y su lote conservador.
  if ("distintos" %in% metricas &&
      (is.null(columnas_distintos) || length(columnas_distintos))) {
    if (is.null(columnas_distintos)) columnas_distintos <- columnas
    n_lotes_distintos <- length(.lotes_columnas_dbi(
      columnas_distintos, tamano_lote_distintos
    ))
    if (!is.null(aproximacion_distintos)) {
      presupuesto$proyeccion_distintos <- .proyeccion_distintos_vacia_dbi(
        n_lotes_distintos,
        paste(
          "La estrategia de distintos es aproximada; no se proyecta el costo",
          "de `COUNT(DISTINCT)` exacto."
        )
      )
    }
    .avisar_derrame_estimado_postgresql_dbi(
      if (is.null(presupuesto)) NULL else presupuesto$estimacion_derrame,
      habilitado = configuracion_aviso("avisar_derrame_estimado", TRUE),
      umbral_bytes = configuracion_aviso(
        "umbral_bytes_aviso_derrame_estimado",
        .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI
      )
    )
    lotes <- .lotes_columnas_dbi(
      columnas_distintos, tamano_lote_distintos
    )
    for (numero in seq_along(lotes)) {
      lote <- lotes[[numero]]
      resultado_lote <- .conteos_distintos_lote_dbi(
        conexion, tabla_sql, lote, nombres_sql, alias, numero, presupuesto,
        aproximacion_distintos,
        incluir_total = FALSE, tabla_total_sql = tabla_total_sql
      )
      tomar_conteo(resultado_lote)
      if (is.null(aproximacion_distintos) && numero == 1L &&
          is.null(presupuesto$proyeccion_distintos)) {
        resultados_referencia <- resultado_lote$resultados
        if (is.list(resultados_referencia)) {
          for (resultado_columna in resultados_referencia) {
            if (is.list(resultado_columna) &&
                !is.null(resultado_columna$distintos)) {
              .registrar_referencia_distintos_dbi(
                presupuesto, resultado_columna$distintos
              )
            }
          }
        }
        proyeccion <- .proyectar_costo_distintos_dbi(
          presupuesto, n_lotes_distintos
        )
        presupuesto$proyeccion_distintos <- proyeccion
        # El aviso temporal llega despues de pagar el primer lote, que es la
        # referencia honesta, y antes de iniciar el segundo. Con un solo lote
        # no hay trabajo futuro que evitar y la proyeccion se declara ausente.
        if (n_lotes_distintos > 1L) {
          .avisar_costo_distintos_dbi(
            proyeccion,
            habilitado = configuracion_aviso("avisar_costo_distintos", TRUE),
            umbral_segundos = configuracion_aviso(
              "umbral_segundos_aviso_distintos",
              .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI
            )
          )
        }
      }
      resultados <- resultado_lote$resultados
      for (campo in lote) {
        base_conteos <- agregados$conteos[[campo]]
        if (is.null(base_conteos)) base_conteos <- list()
        agregados$conteos[[campo]] <- utils::modifyList(
          base_conteos, resultados[[campo]]
        )
      }
    }
  }
  agregados$n_total <- n_total
  agregados$conteo <- conteo
  agregados$sql_conteo <- if (is.null(conteo$sql)) NA_character_ else conteo$sql
  agregados$estimacion_derrame <- if (is.null(presupuesto)) {
    .estimacion_derrame_vacia_postgresql_dbi(
      "no_solicitado", "No se solicito una estimacion de derrame."
    )
  } else presupuesto$estimacion_derrame
  agregados
}

.moda_columna_dbi <- function(conexion, tabla_sql, columna_sql, dialecto,
                              presupuesto, moda_guardian = NULL) {
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  consulta <- .sql_moda_columna_dbi(
    conexion, tabla_sql, columna_sql, dialecto, moda_guardian
  )
  sql_moda <- consulta$sql
  resultado <- .consultar_dbi(
    conexion, sql_moda, presupuesto, filas = consulta$filas,
    etapa = "moda"
  )
  resultado$sql <- sql_moda
  resultado$metadatos <- list(
    metodo = if (is.null(moda_guardian)) {
      "consulta_actual_sin_guardian"
    } else {
      moda_guardian$nombre
    },
    n_validos_guard = NA_real_,
    consulta_id_guard = NA_integer_,
    cota_comprobable = FALSE
  )
  if (!is.null(moda_guardian)) {
    resultado <- .adjuntar_guardian_moda_dbi(
      resultado, resultado, alias("n_validos_guard")
    )
  }
  resultado
}

.resumen_columna_dbi <- function(conexion, tabla_sql, columna, prototipo,
                                 n_total, dialecto = NULL,
                                 metricas = .METRICAS_DBI, presupuesto = NULL,
                                 incluir_valores = TRUE,
                                 tipo_declarado = NA_character_,
                                 motivo_ilegible = NA_character_,
                                 universo = "tabla_completa", muestreo = NULL,
                                 aproximacion_distintos = NULL,
                                 aproximacion_mediana = NULL,
                                 tamano_muestra = NA_real_,
                                 fraccion_muestra = NA_real_,
                                 agregados = NULL,
                                 mediana_consolidada = NULL,
                                 mediana_escalar = NULL,
                                 mediana_cte_ventana = NULL,
                                 mediana_cte_ventana_motivo = NA_character_,
                                 moda_guardian = NULL,
                                 moda_precalculada = NULL,
                                 decisiones_costo = NULL,
                                 publica_distintos = TRUE,
                                 estrategia_distintos = NULL) {
  if (is.null(dialecto)) dialecto <- .dialectos_dbi()$limit
  if (is.null(estrategia_distintos)) {
    estrategia_distintos <- list(
      estrategia_solicitada = "exacta",
      estrategia_resuelta = "COUNT(DISTINCT)",
      estado = "calculado", disponible = TRUE,
      motivo = NA_character_, error_esperado = "no_aplica",
      publica = isTRUE(publica_distintos), para_costo = FALSE,
      requiere_medicion = isTRUE(publica_distintos), sondas = character()
    )
  }
  fila <- .fila_resumen_dbi(columna, n_total)
  literales <- character()
  es_muestreado <- identical(universo, "muestra_motor")
  metricas_de_error <- unlist(
    .CAMPOS_METRICA_DBI[metricas], use.names = FALSE
  )
  error_esperado <- if (es_muestreado) {
    .error_esperado_muestreo_dbi(
      metricas_de_error, muestreo, fraccion_muestra
    )
  } else {
    "no_aplica"
  }
  metadatos <- if (es_muestreado) {
    .metadatos_sql_dbi(
      alcance = "muestra", universo = n_total,
      tamano_muestra = tamano_muestra, fraccion = fraccion_muestra,
      metodo = if (is.null(muestreo) || is.null(muestreo$metodo)) {
        NA_character_
      } else {
        muestreo$metodo
      },
      error_esperado = error_esperado
    )
  } else {
    .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = n_total, tamano_muestra = NA,
      fraccion = 1, metodo = "tabla_completa",
      error_esperado = "no_aplica"
    )
  }
  metadatos <- .agregar_metadatos_estrategia_distintos_dbi(
    metadatos, estrategia_distintos
  )
  if (es_muestreado && !is.null(muestreo) &&
      !isTRUE(muestreo$disponible)) {
    # La incapacidad de construir o gobernar la fuente afecta a todas las
    # metricas de este alcance. El total `n` se registra fuera de esta funcion;
    # aqui no se permite que la ruta de respaldo lea `tabla_sql` completa.
    motivo_muestreo <- if (length(muestreo$motivo) == 1L &&
                           !is.na(muestreo$motivo)) {
      as.character(muestreo$motivo)
    } else {
      "capacidad_no_aceptada:sonda_muestreo"
    }
    registros <- .metricas_omitidas_dbi(
      list(), columna, metricas, "no_disponible", motivo_muestreo,
      metadatos = metadatos
    )
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }
  registrar <- function(registros, metrica, resultado, motivo_exito = NA_character_) {
    if (es_muestreado && isTRUE(resultado$ok) && is.null(resultado$estado)) {
      resultado$estado <- if (any(metrica %in% .METRICAS_OBSERVADAS_MUESTRA_DBI)) {
        "observado_muestra"
      } else "estimado"
    }
    if (es_muestreado && isTRUE(resultado$ok) &&
        any(metrica %in% "mediana")) {
      # Una mediana consolidada puede llegar con estado `calculado` porque la
      # misma forma tambien sirve sobre la tabla completa. Bajo muestreo sigue
      # siendo un estadistico de las filas observadas, no una medicion del
      # universo.
      resultado$estado <- "estimado"
    }
    metadatos_registro <- metadatos
    if (es_muestreado) {
      error_esperado_registro <- .error_esperado_muestreo_dbi(
        metrica, muestreo, fraccion_muestra
      )
      metadatos_registro$error_esperado <- error_esperado_registro
      motivo_error_esperado <- .motivo_error_esperado_muestreo_dbi(
        metrica, error_esperado_registro
      )
      motivo_conteo <- .motivo_conteo_observado_muestra_dbi(metrica)
      if (is.na(motivo_exito) && !is.na(motivo_conteo)) {
        motivo_exito <- motivo_conteo
      }
      if (isTRUE(resultado$ok) && all(is.na(motivo_exito)) &&
          !is.na(motivo_error_esperado)) {
        motivo_exito <- motivo_error_esperado
      }
      if (isTRUE(resultado$ok) && any(metrica %in% "mediana") &&
          !is.null(muestreo) &&
          length(muestreo$motivo) == 1L && !is.na(muestreo$motivo) &&
          grepl("^sesgo_muestreo:", as.character(muestreo$motivo))) {
        motivo_muestreo <- as.character(muestreo$motivo)
        motivo_exito <- if (length(motivo_exito) != 1L ||
                            is.na(motivo_exito) ||
                            !nzchar(as.character(motivo_exito))) {
          motivo_muestreo
        } else {
          paste(motivo_muestreo, as.character(motivo_exito), sep = "; ")
        }
      }
    }
    .registrar_resultado_dbi(
      registros, columna, metrica, resultado, motivo_exito,
      metadatos = metadatos_registro
    )
  }
  omitir <- function(registros, metrica, estado, motivo) {
    .metricas_omitidas_dbi(
      registros, columna, metrica, estado, motivo, metadatos = metadatos
    )
  }
  if (!is.na(motivo_ilegible)) {
    registros <- .metricas_omitidas_dbi(
      list(), columna, .METRICAS_DBI, "no_disponible", motivo_ilegible,
      metadatos = metadatos
    )
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }
  if (es_muestreado && (is.null(muestreo) || !isTRUE(muestreo$disponible))) {
    motivo <- if (is.null(muestreo)) {
      "No se resolvio una capacidad de muestreo del motor."
    } else {
      muestreo$motivo
    }
    registros <- omitir(list(), .METRICAS_DBI, "no_disponible", motivo)
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }
  columna_sql <- as.character(DBI::dbQuoteIdentifier(conexion, columna))
  alias <- function(nombre) {
    as.character(DBI::dbQuoteIdentifier(conexion, nombre))
  }
  registros <- list()
  motivo_no_pedida <- paste(
    "La metrica no se pidio en esta corrida; ver `metricas`."
  )
  motivo_privacidad <- paste(
    "El valor no se informa por `incluir_valores = FALSE`;",
    "la consulta no se emitio."
  )
  filas_mediana <- if (es_muestreado) .numero_dbi(tamano_muestra) else
    .numero_dbi(n_total)
  preparar_aviso_mediana <- function() {
    if (is.null(presupuesto)) return(invisible(NULL))
    pendientes <- .numero_dbi(presupuesto$medianas_pendientes)
    if (length(pendientes) != 1L || is.na(pendientes) ||
        !is.finite(pendientes) || pendientes < 1) pendientes <- 1
    proyeccion <- .proyectar_costo_mediana_dbi(
      presupuesto, filas_mediana, pendientes,
      usar_referencia_banco = pendientes == 1L
    )
    if (isTRUE(proyeccion$disponible)) {
      presupuesto$proyeccion_mediana <- proyeccion
      if (!isTRUE(presupuesto$aviso_mediana_emitido)) {
        avisado <- .avisar_costo_mediana_dbi(
          proyeccion,
          habilitado = if (is.null(presupuesto$avisar_costo_mediana)) TRUE else
            presupuesto$avisar_costo_mediana,
          umbral_segundos = if (is.null(presupuesto$umbral_segundos_aviso_mediana)) {
            .UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI
          } else presupuesto$umbral_segundos_aviso_mediana
        )
        if (isTRUE(avisado)) presupuesto$aviso_mediana_emitido <- TRUE
      }
    }
    invisible(NULL)
  }
  finalizar_mediana <- function(resultado) {
    .registrar_referencia_mediana_dbi(
      presupuesto, resultado, filas_mediana, n_medianas = 1L
    )
    if (!is.null(presupuesto)) {
      pendientes <- .numero_dbi(presupuesto$medianas_pendientes)
      if (isTRUE(resultado$ok) && length(pendientes) == 1L &&
          is.finite(pendientes)) {
        presupuesto$medianas_pendientes <- max(0, pendientes - 1L)
      }
      if (isTRUE(resultado$ok) && isTRUE(presupuesto$medianas_pendientes > 0)) {
        presupuesto$proyeccion_mediana <- .proyectar_costo_mediana_dbi(
          presupuesto, filas_mediana, presupuesto$medianas_pendientes
        )
      }
    }
    invisible(NULL)
  }

  conteos <- if (!is.null(agregados) &&
                 !is.null(agregados$conteos[[columna]])) {
    agregados$conteos[[columna]]
  } else {
    pide_distintos <- "distintos" %in% metricas &&
      isTRUE(estrategia_distintos$disponible) &&
      !identical(estrategia_distintos$estado, "estimado_catalogo")
    .conteos_columna_dbi(
      conexion, tabla_sql, columna_sql, alias,
      "validos" %in% metricas, pide_distintos, presupuesto,
      aproximacion_distintos = aproximacion_distintos
    )
  }

  fuentes_denominador <- Filter(Negate(is.null), list(
    conteos$validos, conteos$basicos, conteos$desvio,
    conteos$distintos
  ))
  if (!is.null(agregados)) {
    fuentes_denominador <- c(
      fuentes_denominador,
      Filter(Negate(is.null), list(
        agregados$basicos[[columna]], agregados$desvio[[columna]]
      ))
    )
  }
  n_total_consulta <- if (length(fuentes_denominador)) {
    candidatos <- vapply(fuentes_denominador, function(resultado) {
      if (is.null(resultado$metadatos)) return(NA_real_)
      .numero_dbi(resultado$metadatos$n_total_consulta)
    }, numeric(1L))
    candidatos <- candidatos[is.finite(candidatos)]
    if (length(candidatos)) candidatos[[1L]] else NA_real_
  } else {
    NA_real_
  }
  # Un agregado sobre una muestra vacia devuelve ceros y una fila valida para
  # SQL, pero eso no significa que se hayan observado cero valores: no se
  # observo ninguna fila. `n_validos` y `n_distintos` no alcanzan para hacer la
  # distincion, porque una muestra no vacia puede tener todos sus valores nulos.
  # Cuando el denominador local esta disponible y vale cero, ninguna metrica de
  # alcance muestral tiene base para publicarse. `n` queda intacto porque sale
  # del conteo de la tabla completa.
  muestra_vacia <- es_muestreado && isTRUE(n_total_consulta == 0)
  if (muestra_vacia) {
    metodo_vacio <- if (is.null(muestreo$metodo) ||
                        is.na(muestreo$metodo) || !nzchar(muestreo$metodo)) {
      "muestreo_sin_metodo"
    } else as.character(muestreo$metodo)
    motivo <- paste0("muestra_vacia:", metodo_vacio, "_sin_filas")
    registros <- .metricas_omitidas_dbi(
      list(), columna, metricas, "no_disponible", motivo,
      metadatos = metadatos
    )
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }
  tamano_pedido_muestra <- if (is.null(muestreo)) NA_real_ else {
    .numero_dbi(muestreo$tamano_muestra)
  }
  muestra_insuficiente <- es_muestreado &&
    !is.null(muestreo) &&
    identical(muestreo$metodo, "tablesample_system") &&
    length(tamano_pedido_muestra) == 1L &&
    is.finite(tamano_pedido_muestra) && tamano_pedido_muestra > 0 &&
    is.finite(n_total_consulta) &&
    n_total_consulta < tamano_pedido_muestra / 2
  if (muestra_insuficiente) {
    registros <- .metricas_omitidas_dbi(
      list(), columna, metricas, "no_disponible",
      "muestra_inestable:tablesample_system_tamano_insuficiente",
      metadatos = metadatos
    )
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }

  sin_valores_validos_muestra <- FALSE

  if ("validos" %in% metricas) {
    validos <- conteos$validos
    registros <- registrar(registros, .CAMPOS_METRICA_DBI$validos, validos)
    if (validos$ok) {
      validos_observados <- .conteo_dbi(validos$valor)
      fila$n_validos <- validos_observados
      sin_valores_validos_muestra <- es_muestreado &&
        !is.na(validos_observados) && .numero_dbi(validos_observados) == 0
      if (!is.na(fila$n_validos) && !is.na(n_total_consulta)) {
        fila$n_faltantes <- n_total_consulta - validos_observados
        if (es_muestreado) {
          muestra_numero <- n_total_consulta
          fila$prop_faltantes <- if (is.finite(muestra_numero) &&
                                     muestra_numero > 0) {
            (muestra_numero - .numero_dbi(validos_observados)) / muestra_numero
          } else if (.numero_dbi(n_total) == 0) {
            NA_real_
          } else {
            NA_real_
          }
        } else {
          fila$prop_faltantes <- if (.numero_dbi(n_total_consulta) > 0) {
            .numero_dbi(fila$n_faltantes) / .numero_dbi(n_total_consulta)
          } else {
            NA_real_
          }
        }
      }
    }
  } else {
    registros <- omitir(registros, "validos", "no_solicitado", motivo_no_pedida)
  }

  if ("distintos" %in% metricas) {
    if (!isTRUE(estrategia_distintos$disponible)) {
      estado <- if (identical(estrategia_distintos$estado, "omitida")) {
        "omitida"
      } else {
        "no_disponible"
      }
      registros <- .metricas_omitidas_dbi(
        registros, columna, "distintos", estado,
        estrategia_distintos$motivo, metadatos = metadatos
      )
    } else {
      distintos <- if (identical(
        estrategia_distintos$estado, "estimado_catalogo"
      )) {
        estimacion <- estrategia_distintos$estimaciones[[columna]]
        if (is.null(estimacion) || !isTRUE(estimacion$disponible)) {
          list(
            ok = FALSE, valor = NULL,
            motivo = if (is.null(estimacion)) paste0(
              "No se obtuvo una estimacion de `pg_stats.n_distinct` para `",
              columna, "`."
            ) else estimacion$motivo,
            sql = NA_character_, estado = NULL,
            metadatos = list(fuente = "pg_stats.n_distinct")
          )
        } else {
          list(
            ok = TRUE, valor = estimacion$n_distintos,
            motivo = estimacion$motivo, sql = NA_character_,
            estado = "estimado_catalogo",
            metadatos = list(
              fuente = "pg_stats.n_distinct",
              n_distintos_catalogo = estimacion$n_distintos_catalogo,
              n_filas_catalogo = estimacion$n_filas,
              inherited_catalogo = estimacion$inherited
            )
          )
        }
      } else {
        conteos$distintos
      }
      if (is.null(distintos)) {
        distintos <- list(
          ok = FALSE, valor = NULL,
          motivo = "La estrategia resolvio una medicion pero no se obtuvo su resultado.",
          sql = NA_character_
        )
      }
      motivo_cota <- NA_character_
      if (isTRUE(distintos$ok) && isTRUE(sin_valores_validos_muestra)) {
        distintos$ok <- FALSE
        distintos$estado <- NULL
        distintos$motivo <- "muestra_inestable:sin_valores_validos"
      }
      if (isTRUE(distintos$ok)) {
        distintos$metadatos <- .mezclar_metadatos_dbi(
          distintos$metadatos, list(
            metodo = estrategia_distintos$estrategia_resuelta,
            error_esperado = estrategia_distintos$error_esperado
          )
        )
        if (identical(estrategia_distintos$estado, "estimado_motor")) {
          distintos$estado <- "estimado_motor"
        } else if (identical(
          estrategia_distintos$estado, "estimado_catalogo"
        )) {
          distintos$estado <- "estimado_catalogo"
        }
        candidato <- .conteo_dbi(distintos$valor)
        guardian <- .guardian_distintos_dbi(distintos)
        motivo_cota <- if (!guardian$comprobable &&
                           is.null(distintos$metadatos$fuente)) {
          .motivo_cota_distintos_no_comprobable_dbi()
        } else {
          NA_character_
        }
        if (guardian$comprobable && !is.na(candidato) &&
            .numero_dbi(candidato) > guardian$valor) {
          distintos$ok <- FALSE
          distintos$estado <- NULL
          distintos$motivo <- paste0(
            "Se comprobo en la consulta que el motor informo ", candidato,
            " valores distintos sobre ", guardian$valor,
            " validos, que es imposible; la metrica no se publica."
          )
          motivo_cota <- NA_character_
        } else if (isTRUE(publica_distintos)) {
          fila$n_distintos <- candidato
          denominador <- if (guardian$comprobable) guardian$valor else {
            .numero_dbi(fila$n_validos)
          }
          if (!is.na(fila$n_distintos) && is.finite(denominador) &&
              denominador > 0) {
            fila$tasa_distintos <- .numero_dbi(fila$n_distintos) / denominador
          }
        }
      }
      if (isTRUE(publica_distintos)) {
        registros <- registrar(
          registros, .CAMPOS_METRICA_DBI$distintos, distintos,
          motivo_exito = motivo_cota
        )
      }
    }
  } else {
    registros <- omitir(registros, "distintos", "no_solicitado", motivo_no_pedida)
  }

  if (!("moda" %in% metricas)) {
    registros <- omitir(registros, "moda", "no_solicitado", motivo_no_pedida)
  } else if (!incluir_valores) {
    registros <- omitir(registros, "moda", "omitido_por_privacidad", motivo_privacidad)
  } else if (!is.null(decisiones_costo) &&
             identical(decisiones_costo$moda, FALSE)) {
    registros <- omitir(
      registros, "moda", "omitido_por_costo",
      .motivo_decision_costo_dbi(decisiones_costo, "moda")
    )
  } else {
    moda <- if (is.null(moda_precalculada)) {
      .moda_columna_dbi(
        conexion, tabla_sql, columna_sql, dialecto, presupuesto
      )
    } else {
      moda_precalculada
    }
    if (moda$ok && nrow(moda$datos)) {
      # La moda no pasaba por la verificacion de nombre: un motor que plegara
      # el alias devolvia `NULL[[1L]]`, o sea NA, con estado `calculado`. Un
      # registro que declara exito sobre una metrica que no se obtuvo es peor
      # que un `no_disponible`.
      celda <- .valor_campo_dbi(moda$datos, "valor")
      frecuencia <- .valor_campo_dbi(moda$datos, "frecuencia")
      if (!celda$ok) {
        moda$ok <- FALSE
        moda$motivo <- celda$motivo
      } else if (!frecuencia$ok) {
        moda$ok <- FALSE
        moda$motivo <- frecuencia$motivo
      } else {
        valor_moda <- tryCatch(.texto_valor(celda$valor), error = function(e) e)
        if (inherits(valor_moda, "error")) {
          moda$ok <- FALSE
          moda$motivo <- conditionMessage(valor_moda)
        } else {
          candidato <- .conteo_dbi(frecuencia$valor)
          guardian <- .guardian_moda_dbi(moda)
          motivo_cota <- if (guardian$comprobable) {
            .motivo_cota_moda_comprobable_dbi()
          } else {
            .motivo_cota_moda_no_comprobable_dbi()
          }
          cota_invalida <- guardian$comprobable && !is.na(candidato) &&
            .numero_dbi(candidato) > guardian$valor
          if (cota_invalida) {
            moda$ok <- FALSE
            moda$motivo <- paste0(
              "Se comprobo en la consulta que la frecuencia de la moda (",
              candidato, ") supera los ", guardian$valor,
              " valores validos; la metrica no se publica."
            )
            motivo_cota <- NA_character_
          } else {
            fila$moda <- valor_moda
            fila$frecuencia_moda <- candidato
            if (!is.na(candidato)) moda$motivo <- motivo_cota
          }
        }
      }
    } else if (moda$ok) {
      if (es_muestreado) {
        moda$ok <- FALSE
        moda$motivo <- "muestra_inestable:sin_valores_validos"
      } else {
        moda$motivo <- "La columna no contiene valores no nulos."
        moda$estado <- "sin_valores"
      }
      # `frecuencia_moda` quedaba en NA, y NA dice "no se midio". Aca si se
      # midio: el motor conto los valores no nulos y no hay ninguno. Es la
      # distincion que el paquete sostiene en todas partes -cero es medido y
      # ninguno-, y `perfilar()` ya devolvia cero para esta misma columna. Las
      # dos puertas daban numeros distintos sobre el mismo dato.
      #
      # Solo cuando la lectura fue sobre la tabla entera. Si se midio sobre una
      # muestra, que la muestra no traiga valores no prueba que la columna este
      # vacia, y ahi NA con su motivo es lo correcto.
      if (!es_muestreado) fila$frecuencia_moda <- 0
    }
    if (es_muestreado) {
      registros <- registrar(
        registros, "moda", moda, motivo_exito = moda$motivo
      )
      registros <- registrar(
        registros, "frecuencia_moda", moda
      )
    } else {
      registros <- registrar(
        registros, .CAMPOS_METRICA_DBI$moda, moda, motivo_exito = moda$motivo
      )
    }
  }

  metricas_numericas <- unlist(
    .CAMPOS_METRICA_DBI[.METRICAS_NUMERICAS_DBI], use.names = FALSE
  )
  pedidas_numericas <- intersect(metricas, .METRICAS_NUMERICAS_DBI)
  no_pedidas_numericas <- setdiff(.METRICAS_NUMERICAS_DBI, metricas)
  if (length(no_pedidas_numericas)) {
    registros <- omitir(
      registros, no_pedidas_numericas, "no_solicitado", motivo_no_pedida
    )
  }
  if (!length(pedidas_numericas)) {
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }
  campos_pedidos <- unlist(
    .CAMPOS_METRICA_DBI[pedidas_numericas], use.names = FALSE
  )

  if (!.es_numerico_dbi(prototipo, tipo_declarado)) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, campos_pedidos, "no_aplica",
      paste0(
        "DBI expuso la columna como `", paste(class(prototipo), collapse = "/"),
        "`", if (length(tipo_declarado) == 1L && !is.na(tipo_declarado)) {
          paste0(" y el motor la declara `", tipo_declarado, "`")
        } else "",
        "; no se aplicaron agregados cuantitativos."
      ),
      NA_character_, metadatos = metadatos
    )))
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }

  # `n_validos` solo condiciona a las metricas que lo necesitan de verdad. Que
  # no se haya podido contar no es motivo para no calcular un minimo.
  sin_conteo <- is.na(fila$n_validos)
  if (!sin_conteo && .numero_dbi(fila$n_validos) == 0) {
    if (es_muestreado) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, campos_pedidos, "no_disponible",
        "muestra_inestable:sin_valores_validos", NA_character_,
        metadatos = metadatos
      )))
      return(list(
        fila = fila, sql = do.call(rbind, registros), literales = literales
      ))
    }
    registros <- c(registros, list(.registro_sql_dbi(
      columna, campos_pedidos, "sin_valores",
      "La columna no contiene valores no nulos.", NA_character_,
      metadatos = metadatos
    )))
    return(list(
      fila = fila, sql = do.call(rbind, registros), literales = literales
    ))
  }

  if ("basicos" %in% pedidas_numericas) {
    # `AVG(columna)` sin castear trunca en los motores con semantica entera.
    # Multiplicar por 1.0 promueve el tipo sin depender de un nombre de tipo
    # que cambia con el motor.
    basicos <- if (!is.null(agregados) &&
                   !is.null(agregados$basicos[[columna]])) {
      agregados$basicos[[columna]]
    } else {
      .basicos_columna_dbi(
        conexion, tabla_sql, columna_sql, alias, incluir_valores, presupuesto
      )
    }
    calculados <- c("media", "n_ceros", "n_negativos")
    if (incluir_valores) calculados <- c("minimo", "maximo", calculados)
    if (basicos$ok && nrow(basicos$datos)) {
      # Se leen todos los campos antes de tocar la fila: media a medias, con la
      # auditoria diciendo `no_disponible`, seria un tercer estado que no
      # existe en el vocabulario del paquete.
      leidos <- list()
      for (metrica in calculados) {
        celda <- .valor_campo_dbi(basicos$datos, metrica)
        if (!celda$ok) {
          basicos$ok <- FALSE
          basicos$motivo <- celda$motivo
          leidos <- list()
          break
        }
        leidos[[metrica]] <- if (metrica %in% c("n_ceros", "n_negativos")) {
          .conteo_dbi(celda$valor)
        } else {
          convertido <- .escalar_finito_dbi(celda$valor)
          if (.valor_perdido_en_conversion_dbi(celda$valor, convertido)) {
            basicos$ok <- FALSE
            basicos$motivo <- paste0(
              "El motor devolvio un valor para `", metrica, "` que no se pudo ",
              "leer como numero (",
              utils::head(as.character(celda$valor[[1L]]), 1L),
              "): probablemente la columna no es de la magnitud que estas ",
              "metricas suponen. No se publica como calculada."
            )
            leidos <- list()
            break
          }
          if (.entero_perdido_en_conversion_dbi(celda$valor)) {
            basicos$ok <- FALSE
            basicos$motivo <- paste0(
              "El valor de `", metrica, "` es un entero por encima de 2^53 y ",
              "pasarlo a doble lo cambia: el numero publicado no estaria en la ",
              "columna. No se publica como calculada."
            )
            leidos <- list()
            break
          }
          convertido
        }
      }
      # Un maximo menor que el minimo es imposible, y aparece de verdad: un
      # `BIGINT UNSIGNED` cerca del tope vuelve del driver como `integer64`
      # negativo, asi que el motor informa `min = 1` y `max = -1` sin mentir
      # -miente la representacion-. Se declara no disponible en vez de
      # publicarse, igual que se hace cuando hay mas distintos que validos.
      rango <- c(leidos[["minimo"]], leidos[["maximo"]])
      if (length(rango) == 2L && all(is.finite(rango)) &&
          rango[[2L]] < rango[[1L]]) {
        basicos$ok <- FALSE
        basicos$motivo <- paste0(
          "El motor informo un maximo (", rango[[2L]], ") menor que el minimo (",
          rango[[1L]], "), que es imposible: probablemente el tipo no entra en ",
          "la representacion del controlador. Las metricas no se publican."
        )
        leidos <- list()
      }
      for (metrica in names(leidos)) fila[[metrica]] <- leidos[[metrica]]
    } else if (basicos$ok) {
      basicos$ok <- FALSE
      basicos$motivo <- "La consulta de agregados no devolvio ninguna fila."
    }
    if (is.null(basicos$sql)) basicos$sql <- NA_character_
    if (es_muestreado && isTRUE(basicos$ok)) {
      for (metrica in calculados) {
        resultado_metrica <- basicos
        registros <- registrar(registros, metrica, resultado_metrica)
      }
    } else {
      registros <- registrar(registros, calculados, basicos)
    }
    if (!incluir_valores) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, c("minimo", "maximo"), "omitido_por_privacidad",
        motivo_privacidad, NA_character_, metadatos = metadatos
      )))
    }
  }

  if ("mediana" %in% pedidas_numericas) {
    if (!incluir_valores) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, "mediana", "omitido_por_privacidad", motivo_privacidad,
        NA_character_, metadatos = metadatos
      )))
    } else if (!is.null(decisiones_costo) &&
               identical(decisiones_costo$mediana, FALSE)) {
      registros <- omitir(
        registros, "mediana", "omitido_por_costo",
        .motivo_decision_costo_dbi(decisiones_costo, "mediana")
      )
    } else if (es_muestreado && !is.null(presupuesto) &&
               is.list(presupuesto$guardia_newid) &&
               !isTRUE(presupuesto$guardia_newid$aceptado)) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, "mediana", "no_disponible",
        presupuesto$guardia_newid$motivo, NA_character_,
        metadatos = .mezclar_metadatos_dbi(
          metadatos, list(metodo = NA_character_)
        )
      )))
    } else if (!is.null(mediana_consolidada)) {
      mediana <- mediana_consolidada
      if (isTRUE(mediana$ok)) fila$mediana <- .escalar_finito_dbi(mediana$valor)
      registros <- registrar(registros, "mediana", mediana)
    } else if (!is.null(aproximacion_mediana)) {
      sql_mediana <- aproximacion_mediana$construir(
        columna_sql, tabla_sql, alias("mediana")
      )
      preparar_aviso_mediana()
      mediana <- .escalar_dbi(
        conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
      )
      finalizar_mediana(mediana)
      mediana$sql <- sql_mediana
      if (isTRUE(mediana$ok)) {
        mediana$estado <- "estimado"
        mediana$metadatos <- list(
          metodo = aproximacion_mediana$nombre,
          error_esperado = aproximacion_mediana$error_esperado
        )
        fila$mediana <- .escalar_finito_dbi(mediana$valor)
      }
      registros <- registrar(registros, "mediana", mediana)
    } else if (!is.null(mediana_cte_ventana)) {
      sql_mediana <- mediana_cte_ventana$construir(
        columna_sql, tabla_sql, alias("mediana")
      )
      preparar_aviso_mediana()
      mediana <- .escalar_dbi(
        conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
      )
      finalizar_mediana(mediana)
      mediana$sql <- sql_mediana
      mediana$metadatos <- list(metodo = mediana_cte_ventana$nombre)
      if (isTRUE(mediana$ok)) {
        valor <- .escalar_finito_dbi(mediana$valor)
        if (is.na(valor)) {
          mediana$ok <- FALSE
          mediana$motivo <- "muestra_inestable:sin_valores_validos"
        } else {
          fila$mediana <- valor
        }
      }
      registros <- registrar(registros, "mediana", mediana)
    } else if (!is.null(mediana_escalar)) {
      sql_mediana <- mediana_escalar$construir(
        columna_sql, tabla_sql, alias("mediana"),
        materializar = es_muestreado
      )
      preparar_aviso_mediana()
      mediana <- .escalar_dbi(
        conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
      )
      finalizar_mediana(mediana)
      mediana$sql <- sql_mediana
      mediana$metadatos <- list(metodo = mediana_escalar$nombre)
      if (isTRUE(mediana$ok)) {
        valor <- .escalar_finito_dbi(mediana$valor)
        if (is.na(valor)) {
          mediana$estado <- "sin_valores"
          mediana$motivo <- "La columna no contiene valores no nulos."
        } else {
          fila$mediana <- valor
        }
      }
      registros <- registrar(registros, "mediana", mediana)
    } else if (es_muestreado) {
      motivo <- if (length(mediana_cte_ventana_motivo) == 1L &&
                    !is.na(mediana_cte_ventana_motivo)) {
        mediana_cte_ventana_motivo
      } else {
        "capacidad_no_aceptada:sonda_mediana_cte_ventana"
      }
      registros <- c(registros, list(.registro_sql_dbi(
        columna, "mediana", "no_disponible", motivo, NA_character_,
        metadatos = metadatos
      )))
    } else if (sin_conteo || (es_muestreado && !exists("validos_observados"))) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, "mediana", "no_disponible",
        paste(
          "La mediana exacta necesita la cantidad de valores no nulos,",
          "que no se pudo conocer."
        ),
        NA_character_, metadatos = metadatos
      )))
    } else {
      n_validos_numero <- if (es_muestreado) {
        .numero_dbi(validos_observados)
      } else {
        .numero_dbi(fila$n_validos)
      }
      limite <- if (n_validos_numero %% 2 == 0) 2 else 1
      desplazamiento <- floor((n_validos_numero - 1) / 2)
      interna <- paste0(
        "SELECT ", columna_sql, " AS ", alias("valor"), " FROM ", tabla_sql,
        " WHERE ", columna_sql, " IS NOT NULL ORDER BY ", columna_sql
      )
      acotada <- dialecto$limitar(interna, limite, desplazamiento)
      if (is.null(acotada)) {
        registros <- c(registros, list(.registro_sql_dbi(
          columna, "mediana", "no_disponible",
          paste0(
            "El dialecto `", dialecto$nombre, "` no expresa un salto de filas; ",
            "la mediana exacta exigiria traer ",
            .entero_sql_dbi(desplazamiento + limite), " filas a memoria."
          ),
          NA_character_, metadatos = metadatos
        )))
      } else {
        sql_mediana <- paste0(
          "SELECT AVG(", alias("valor"), " * 1.0) AS ", alias("mediana"),
          " FROM (", acotada, ")", dialecto$alias_tabla("lupa_mediana")
        )
        preparar_aviso_mediana()
        mediana <- .escalar_dbi(
          conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
        )
        finalizar_mediana(mediana)
        if (mediana$ok) fila$mediana <- .escalar_finito_dbi(mediana$valor)
        mediana$sql <- sql_mediana
        mediana$metadatos <- list(metodo = "dos_consultas")
        registros <- registrar(registros, "mediana", mediana)
      }
    }
  }

  if ("desvio" %in% pedidas_numericas) {
    if (!sin_conteo && .numero_dbi(fila$n_validos) < 2) {
      registros <- c(registros, list(.registro_sql_dbi(
        columna, "desvio", "no_aplica",
        "El desvio muestral requiere al menos dos valores no nulos.",
        NA_character_, metadatos = metadatos
      )))
    } else {
      .sondar_forma_desvio(conexion, presupuesto, alias)
      desvio <- if (!is.null(agregados) &&
                    !is.null(agregados$desvio[[columna]])) {
        agregados$desvio[[columna]]
      } else {
        .desvio_columna_dbi(
          conexion, tabla_sql, columna_sql, alias, presupuesto$forma_desvio,
          presupuesto
        )
      }
      if (desvio$ok) fila$desvio <- .escalar_finito_dbi(desvio$valor)
      registros <- registrar(registros, "desvio", desvio)
    }
  }

  sobrantes <- setdiff(
    metricas_numericas,
    unlist(lapply(registros, function(x) x$metrica), use.names = FALSE)
  )
  sobrantes <- intersect(sobrantes, campos_pedidos)
  if (length(sobrantes)) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, sobrantes, "no_disponible",
      "La metrica no se pudo calcular en esta corrida.", NA_character_,
      metadatos = metadatos
    )))
  }

  list(fila = fila, sql = do.call(rbind, registros), literales = literales)
}

.columnas_dbi_vacias <- function() {
  data.frame(
    columna = character(), n = numeric(), n_validos = numeric(),
    n_faltantes = numeric(), prop_faltantes = numeric(),
    n_distintos = numeric(), tasa_distintos = numeric(),
    moda = character(), frecuencia_moda = numeric(), minimo = numeric(),
    maximo = numeric(), media = numeric(), mediana = numeric(),
    desvio = numeric(), n_ceros = numeric(), n_negativos = numeric(),
    stringsAsFactors = FALSE
  )
}

.incorporar_distintos_estructurales_dbi <- function(agregados, fuentes,
                                                     n_total, universo) {
  if (is.null(fuentes) || !length(fuentes)) return(agregados)
  for (columna in names(fuentes)) {
    fuente <- fuentes[[columna]]
    if (is.null(fuente) || !isTRUE(fuente$exacta) ||
        !is.finite(fuente$proporcion_distintos) ||
        fuente$proporcion_distintos != 1 || is.na(n_total) ||
        identical(universo, "muestra_motor")) {
      next
    }
    resultado <- list(
      ok = TRUE, valor = n_total, motivo = NA_character_, sql = NA_character_,
      estado = "garantizado",
      metadatos = list(fuente = fuente$nombre)
    )
    base <- agregados$conteos[[columna]]
    if (!is.null(base) && !is.null(base$distintos)) next
    if (is.null(base)) base <- list(consolidada = TRUE)
    agregados$conteos[[columna]] <- utils::modifyList(
      base, list(distintos = resultado)
    )
  }
  agregados
}

.resumen_tabla_dbi <- function(conexion, tabla, tabla_sql, campos, prototipo,
                               n_total, sql_conteo, info_conexion,
                               dialecto = NULL, metricas = .METRICAS_DBI,
                               metricas_ejecucion = metricas,
                               presupuesto = NULL, incluir_valores = TRUE,
                               tipos_declarados = NULL,
                               motivos_ilegibles = NULL,
                                universo = "tabla_completa",
                                estrategia_mediana = "exacta",
                                # `tabla_sql` no se usa en el cuerpo: esta para
                                # ser el valor por omision de la linea de abajo.
                                # Es una omision razonable -medir sobre la misma
                                # tabla- aunque el unico llamador siempre la
                                # pise con la tabla muestreada.
                                tabla_metricas_sql = tabla_sql,
                                muestreo = NULL, aproximaciones = list(),
                                tamano_muestra = NA_real_,
                                fraccion_muestra = NA_real_,
                                campos_consolidados = NULL,
                                campos_sql_consolidados = NULL,
                                es_numerico_consolidados = NULL,
                                tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                                tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                                conteo = NULL,
                                tabla_total_sql = tabla_sql,
                                moda_guardian = NULL,
                                mediana_consolidada = NULL,
                                mediana_escalar = NULL,
                                mediana_cte_ventana = NULL,
                                mediana_cte_ventana_motivo = NA_character_,
                                fuentes_cardinalidad_costo = NULL,
                                estrategia_distintos = list(
                                  publica = TRUE, disponible = TRUE,
                                  estrategia_solicitada = "exacta",
                                  estrategia_resuelta = "COUNT(DISTINCT)",
                                  estado = "calculado",
                                  motivo = NA_character_,
                                  error_esperado = "no_aplica"
                                ),
                                politica_costo = list(
                                  nombre = "todas",
                                  umbral = .UMBRAL_CARDINALIDAD_COSTO_DBI
                                )) {
  if (is.null(dialecto)) dialecto <- .dialectos_dbi()$limit
  if (is.null(campos_consolidados)) campos_consolidados <- campos
  if (is.null(campos_sql_consolidados)) {
    campos_sql_consolidados <- vapply(campos_consolidados, function(campo) {
      as.character(DBI::dbQuoteIdentifier(conexion, campo))
    }, character(1L), USE.NAMES = FALSE)
  }
  if (is.null(es_numerico_consolidados)) {
    es_numerico_consolidados <- vapply(seq_along(campos_consolidados), function(i) {
      .es_numerico_dbi(
        prototipo[[i]],
        if (i <= length(tipos_declarados)) tipos_declarados[[i]] else NA_character_
      )
    }, logical(1L))
  }
  exacto_derrame <- (
    "distintos" %in% metricas_ejecucion &&
      identical(estrategia_distintos$estrategia_resuelta, "COUNT(DISTINCT)")
  ) || ("moda" %in% metricas_ejecucion && isTRUE(incluir_valores)) || (
    "mediana" %in% metricas_ejecucion && isTRUE(incluir_valores) &&
      (!is.null(mediana_consolidada) || !is.null(mediana_escalar) ||
       (!identical(universo, "muestra_motor") && is.null(aproximaciones$mediana)))
  )
  if (!is.null(presupuesto)) .iniciar_instrumentacion_derrame_dbi(
    conexion, presupuesto, exacto = exacto_derrame
  )
  agregados <- .agregados_consolidados_dbi(
    conexion, tabla_metricas_sql, campos_consolidados,
    campos_sql_consolidados, es_numerico_consolidados, metricas_ejecucion,
    incluir_valores, presupuesto, tamano_lote_planos, tamano_lote_distintos,
    aproximacion_distintos = aproximaciones$distintos,
    tabla_total_sql = tabla_total_sql, conteo_inicial = conteo,
    columnas_distintos = .columnas_distintos_ejecucion_dbi(
      metricas_ejecucion, estrategia_distintos, campos_consolidados,
      fuentes_cardinalidad_costo, politica_costo
    )
  )
  conteo <- agregados$conteo
  n_total <- agregados$n_total
  if (is.null(conteo) || !isTRUE(conteo$ok) || is.na(n_total)) {
    motivo <- if (is.null(conteo)) {
      "No se pudo obtener el conteo exacto de la tabla."
    } else if (!isTRUE(conteo$ok)) {
      paste0("No se pudo contar la tabla: ", conteo$motivo)
    } else {
      "La consulta de conteo no devolvio un numero utilizable."
    }
    # El presupuesto es una degradacion esperable, no un error fatal: las
    # metricas que ya entraron siguen siendo utiles aunque el denominador no
    # haya entrado. Otros fallos de conteo si impiden interpretar el resumen y
    # conservan la condicion de error que ya tenia la via DBI.
    motivo_conteo <- if (is.null(conteo)) "" else {
      if (is.null(conteo$motivo)) "" else as.character(conteo$motivo)
    }
    agotado_por_presupuesto <- length(motivo_conteo) == 1L &&
      grepl("presupuesto declarado", motivo_conteo, fixed = TRUE)
    if (!agotado_por_presupuesto) {
      .detener_dbi(
        "lupa_error_conteo_dbi", motivo,
        datos = list(sql = if (is.null(conteo)) NA_character_ else conteo$sql)
      )
    }
    n_total <- NA_real_
  }
  agregados <- .incorporar_distintos_estructurales_dbi(
    agregados, fuentes_cardinalidad_costo, n_total, universo
  )
  sql_conteo_registro <- if (is.null(conteo$sql)) {
    sql_conteo
  } else {
    conteo$sql
  }
  if (identical(universo, "muestra_motor")) {
    if (is.finite(.numero_dbi(tamano_muestra))) {
      # Si la solicitud supera el universo, la consulta puede devolver menos
      # filas que las pedidas. El tamano efectivo es el que se midio y el que
      # debe aparecer en la metadata; usar el pedido volveria a extrapolar una
      # muestra que en realidad cubrio toda la tabla.
      tamano_muestra <- min(.numero_dbi(tamano_muestra), .numero_dbi(n_total))
      fraccion_muestra <- .fraccion_muestreo_dbi(tamano_muestra, n_total)
    }
    if (!is.null(muestreo)) {
      muestreo$universo <- n_total
      muestreo$tamano_muestra <- tamano_muestra
      muestreo$fraccion <- fraccion_muestra
    }
  }
  decisiones_costo <- .decisiones_costo_dbi(
    conexion, campos_consolidados, agregados, politica_costo, n_total, universo,
    fuentes_cardinalidad_costo = fuentes_cardinalidad_costo
  )
  if (!is.null(presupuesto)) {
    presupuesto$estimacion_derrame_moda <- .actualizar_n_validos_estimacion_dbi(
      presupuesto$estimacion_derrame_moda, agregados, metricas_ejecucion, "meta"
    )
    presupuesto$estimacion_derrame_mediana <- .actualizar_n_validos_estimacion_dbi(
      presupuesto$estimacion_derrame_mediana, agregados, metricas_ejecucion, "meta"
    )
  }
  # La moda y la mediana son dos familias distintas: preparar todas las modas
  # antes de entrar en la mediana deja disponibles primero las metricas con
  # mayor cobertura, incluso cuando no hay una consolidacion de medianas.
  columnas_modas <- intersect(campos_consolidados, campos)
  columnas_modas <- columnas_modas[
    vapply(columnas_modas, function(campo) {
      decision <- decisiones_costo[[campo]]
      motivo <- if (is.null(motivos_ilegibles) ||
                    !campo %in% names(motivos_ilegibles)) {
        NA_character_
      } else {
        motivos_ilegibles[[campo]]
      }
      "moda" %in% metricas_ejecucion && isTRUE(incluir_valores) &&
        (is.null(decision) || isTRUE(decision$moda)) &&
        is.na(motivo)
    }, logical(1L))
  ]
  modas <- vector("list", length(columnas_modas))
  names(modas) <- columnas_modas
  if (length(columnas_modas) && !is.null(presupuesto) &&
      !isTRUE(presupuesto$aviso_derrame_moda_emitido)) {
    estimacion_aviso <- .filtrar_estimacion_derrame_dbi(
      presupuesto$estimacion_derrame_moda, columnas_modas
    )
    avisado <- .avisar_derrame_estimado_postgresql_dbi(
      estimacion_aviso,
      habilitado = if (is.null(presupuesto$avisar_derrame_estimado)) TRUE else
        presupuesto$avisar_derrame_estimado,
      umbral_bytes = if (is.null(presupuesto$umbral_bytes_aviso_derrame_estimado)) {
        .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI
      } else presupuesto$umbral_bytes_aviso_derrame_estimado
    )
    if (isTRUE(avisado)) presupuesto$aviso_derrame_moda_emitido <- TRUE
  }
  for (posicion in seq_along(columnas_modas)) {
    campo <- columnas_modas[[posicion]]
    pendientes <- columnas_modas[posicion:length(columnas_modas)]
    cardinalidades <- stats::setNames(lapply(pendientes, function(nombre) {
      .cardinalidad_aviso_moda_dbi(
        nombre, agregados, fuentes_cardinalidad_costo, n_total
      )
    }), pendientes)
    if (!is.null(presupuesto)) {
      proyeccion <- .proyectar_costo_moda_dbi(presupuesto, cardinalidades)
      presupuesto$proyeccion_moda <- proyeccion
      if (!isTRUE(presupuesto$aviso_moda_emitido)) {
        avisado <- .avisar_costo_moda_dbi(
          proyeccion,
          habilitado = if (is.null(presupuesto$avisar_costo_moda)) TRUE else
            presupuesto$avisar_costo_moda,
          umbral_segundos = if (is.null(presupuesto$umbral_segundos_aviso_moda)) {
            .UMBRAL_SEGUNDOS_AVISO_MODA_DBI
          } else presupuesto$umbral_segundos_aviso_moda
        )
        if (isTRUE(avisado)) presupuesto$aviso_moda_emitido <- TRUE
      }
    }
    modas[[campo]] <- .moda_columna_dbi(
      conexion, tabla_metricas_sql,
      as.character(DBI::dbQuoteIdentifier(conexion, campo)),
      dialecto, presupuesto, moda_guardian = moda_guardian
    )
    if (!is.null(presupuesto)) {
      cardinalidad <- .cardinalidad_aviso_moda_dbi(
        campo, agregados, fuentes_cardinalidad_costo, n_total
      )
      .registrar_referencia_moda_dbi(
        presupuesto, modas[[campo]], cardinalidad$n_distintos, campo
      )
    }
  }
  columnas_medianas <- intersect(campos_consolidados, campos)
  columnas_medianas <- columnas_medianas[
    match(columnas_medianas, campos_consolidados) <= length(es_numerico_consolidados) &
      es_numerico_consolidados[match(columnas_medianas, campos_consolidados)]
  ]
  columnas_medianas <- columnas_medianas[
    vapply(columnas_medianas, function(campo) {
      decision <- decisiones_costo[[campo]]
      "mediana" %in% metricas_ejecucion && isTRUE(incluir_valores) &&
      (is.null(decision) || isTRUE(decision$mediana))
    }, logical(1L))
  ]
  if (length(columnas_medianas) && !is.null(presupuesto) &&
      !isTRUE(presupuesto$aviso_derrame_mediana_emitido)) {
    estimacion_aviso <- .filtrar_estimacion_derrame_dbi(
      presupuesto$estimacion_derrame_mediana, columnas_medianas
    )
    avisado <- .avisar_derrame_estimado_postgresql_dbi(
      estimacion_aviso,
      habilitado = if (is.null(presupuesto$avisar_derrame_estimado)) TRUE else
        presupuesto$avisar_derrame_estimado,
      umbral_bytes = if (is.null(presupuesto$umbral_bytes_aviso_derrame_estimado)) {
        .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI
      } else presupuesto$umbral_bytes_aviso_derrame_estimado
    )
    if (isTRUE(avisado)) presupuesto$aviso_derrame_mediana_emitido <- TRUE
  }
  if (length(columnas_medianas) && identical(universo, "muestra_motor") &&
      !is.null(muestreo) && identical(muestreo$metodo, "random_limit") &&
      identical(muestreo$funcion_muestreo, "newid") &&
      !is.null(presupuesto) && is.null(presupuesto$guardia_newid)) {
    guardia_newid <- .evaluar_guardia_newid_dbi(
      n_total, tamano_muestra, length(columnas_medianas)
    )
    presupuesto$guardia_newid <- guardia_newid
    if (!isTRUE(guardia_newid$aceptado) &&
        (is.null(presupuesto$avisar_costo_mediana) ||
         isTRUE(presupuesto$avisar_costo_mediana))) {
      cli::cli_alert_warning(paste0(
        "Mediana muestreada no disponible: ", guardia_newid$motivo,
        ". Proyeccion NEWID: ",
        if (is.finite(guardia_newid$proyeccion_newid_ms)) {
          formatC(guardia_newid$proyeccion_newid_ms, format = "f", digits = 1)
        } else "sin dato",
        " ms; n_total = ", .entero_sql_dbi(guardia_newid$n_total),
        ", fraccion = ", formatC(guardia_newid$fraccion,
                                  format = "f", digits = 3), "."
      ))
    }
  }
  medianas <- vector("list", length(columnas_medianas))
  names(medianas) <- columnas_medianas
  if (length(columnas_medianas) && !is.null(presupuesto)) {
    presupuesto$filas_mediana <- if (identical(universo, "muestra_motor")) {
      .numero_dbi(tamano_muestra)
    } else {
      .numero_dbi(n_total)
    }
    presupuesto$medianas_pendientes <- length(columnas_medianas)
  }
  if (length(columnas_medianas) && !is.null(mediana_consolidada)) {
    medianas <- .medianas_consolidadas_dbi(
      conexion, tabla_metricas_sql, columnas_medianas,
      stats::setNames(campos_sql_consolidados, campos_consolidados),
      function(nombre) as.character(DBI::dbQuoteIdentifier(conexion, nombre)),
      mediana_consolidada, presupuesto, tamano_lote_planos
    )
  }
  tipo_de <- function(i) {
    if (is.null(tipos_declarados) || i > length(tipos_declarados)) {
      return(NA_character_)
    }
    tipos_declarados[[i]]
  }
  motivo_de <- function(campo) {
    if (is.null(motivos_ilegibles) || !campo %in% names(motivos_ilegibles)) {
      return(NA_character_)
    }
    motivos_ilegibles[[campo]]
  }
  resultados <- lapply(seq_along(campos), function(i) {
    campo <- campos[[i]]
    .resumen_columna_dbi(
      conexion, tabla_metricas_sql, campo,
      if (i <= length(prototipo)) prototipo[[i]] else NA,
      n_total, dialecto = dialecto, metricas = metricas,
      presupuesto = presupuesto, incluir_valores = incluir_valores,
      tipo_declarado = tipo_de(i), motivo_ilegible = motivo_de(campo),
      universo = universo, muestreo = muestreo,
      aproximacion_distintos = aproximaciones$distintos,
      aproximacion_mediana = aproximaciones$mediana,
      tamano_muestra = tamano_muestra, fraccion_muestra = fraccion_muestra,
      agregados = agregados,
      moda_guardian = moda_guardian,
      mediana_consolidada = medianas[[campo]],
      mediana_escalar = mediana_escalar,
      mediana_cte_ventana = mediana_cte_ventana,
      mediana_cte_ventana_motivo = mediana_cte_ventana_motivo,
      moda_precalculada = modas[[campo]],
      decisiones_costo = decisiones_costo[[campo]],
      publica_distintos = isTRUE(estrategia_distintos$publica),
      estrategia_distintos = estrategia_distintos
    )
  })
  .finalizar_instrumentacion_derrame_dbi(conexion, presupuesto)
  columnas <- if (length(resultados)) {
    filas <- lapply(resultados, `[[`, "fila")
    as.data.frame(do.call(rbind, lapply(filas, as.data.frame)),
                  stringsAsFactors = FALSE)
  } else {
    .columnas_dbi_vacias()
  }
  numericas <- intersect(
    c("prop_faltantes", "tasa_distintos", "minimo", "maximo", "media",
      "mediana", "desvio"), names(columnas)
  )
  columnas[numericas] <- lapply(columnas[numericas], as.numeric)
  sql <- if (length(resultados)) {
    rbind(
      .registro_sql_dbi(
        campos, rep("n", length(campos)), "calculado", NA_character_,
        sql_conteo_registro,
        metadatos = .mezclar_metadatos_dbi(.metadatos_sql_dbi(
          # `n` is the declared exception: it counts the complete universe even
          # when every other SQL metric describes a motor sample.
          alcance = "tabla_completa",
          universo = n_total, tamano_muestra = if (identical(universo, "muestra_motor")) {
            tamano_muestra
          } else NA_real_,
          fraccion = if (identical(universo, "muestra_motor")) fraccion_muestra else 1,
          metodo = "conteo_universo",
          error_esperado = "no_aplica"
        ), conteo$metadatos),
        medicion = conteo,
        etapa = "conteo_filas"
      ),
      do.call(rbind, lapply(resultados, `[[`, "sql"))
    )
  } else {
    .registro_sql_dbi(character(), character(), character(), character(), character())
  }
  sql <- .marcar_nivel_sql_dbi(sql)
  rownames(columnas) <- NULL
  rownames(sql) <- NULL
  filas_obtenidas_muestra <- NA_real_
  if (identical(universo, "muestra_motor")) {
    resultados_denominador <- list()
    for (familia_nombre in c("conteos", "basicos", "desvio")) {
      familia <- agregados[[familia_nombre]]
      if (is.list(familia)) {
        resultados_denominador <- c(
          resultados_denominador, unname(familia)
        )
      }
    }
    denominadores <- unlist(lapply(resultados_denominador, function(resultado) {
      medidos <- if (!is.null(resultado$metadatos)) {
        list(resultado)
      } else {
        Filter(Negate(is.null), resultado[c(
          "validos", "basicos", "desvio"
        )])
      }
      vapply(medidos, function(medido) {
        if (is.null(medido$metadatos)) return(NA_real_)
        .numero_dbi(medido$metadatos$n_total_consulta)
      }, numeric(1L))
    }), use.names = FALSE)
    denominadores <- denominadores[is.finite(denominadores)]
    if (length(denominadores)) filas_obtenidas_muestra <- denominadores[[1L]]
  }
  meta <- list(
       universo = universo,
       estrategia_mediana = estrategia_mediana,
       filas_obtenidas_muestra = filas_obtenidas_muestra,
       alcance = if (identical(universo, "muestra_motor")) {
         "tabla_muestreada"
       } else {
         "tabla_completa"
       },
      tabla = .texto_tabla_dbi(tabla),
       filas = n_total,
      motor = info_conexion,
      sql_conteo_filas = sql_conteo,
      criterio_moda = paste(
        "mayor frecuencia; empates resueltos por el valor ascendente",
        "segun el orden del motor"
      ),
      # El motor compara con su cotejamiento y R con `==`. Sobre una columna
      # `utf8mb4_general_ci`, `"A"` y `"a"` son un valor para el motor y dos
      # para R, asi que `resumen_tabla` y `perfil_muestra` pueden informar
      # cardinalidades distintas de las mismas filas. Las dos son ciertas en su
      # propia comparacion; lo que faltaba era decir cual usa cada bloque.
      criterio_comparacion = paste(
        "el resumen SQL agrupa y ordena con el cotejamiento del motor, que",
        "puede ignorar caja o acentos; el perfil de muestra compara en R, que",
        "distingue byte a byte. Una diferencia entre los dos bloques en",
        "`n_distintos` o en la moda es esperable sobre columnas con",
        "cotejamiento insensible, y no es un error de ninguno de los dos"
      ),
      # En `muestra_motor` la relacion muestreada tiene una sola identidad: la
      # del spool externo que se abre antes de las pasadas. Este campo histórico
      # ya no se publica; el contrato legible vive en `meta$materializacion` y
      # en `meta$muestreo`, donde `muestra_id` no se confunde con un id de
      # consulta SQL.
      solo_lectura = TRUE,
      objetos_temporales = FALSE,
      snapshot = FALSE,
      nota_snapshot = paste(
        "No hubo lectura instantanea: cada agregado se midio en su propio",
        "momento y la tabla pudo cambiar entre consultas. La cobertura declara",
        "las comparaciones incoherentes entre grupos de consistencia distintos."
      ),
      politica_costo = politica_costo,
      decisiones_costo = decisiones_costo,
      # Este objeto usa el catalogo solo para anticipar el costo de memoria.
      # Nunca reemplaza a `meta$derrame`, que solo puede salir de una medicion
      # posterior por `pg_stat_statements`.
      estimacion_derrame = agregados$estimacion_derrame,
      estimacion_derrame_moda = if (is.null(presupuesto)) {
        .estimacion_derrame_familia_vacia_dbi(
          "moda", "No se solicito una estimacion de derrame."
        )
      } else presupuesto$estimacion_derrame_moda,
      estimacion_derrame_mediana = if (is.null(presupuesto)) {
        .estimacion_derrame_familia_vacia_dbi(
          "mediana", "No se solicito una estimacion de derrame."
        )
      } else presupuesto$estimacion_derrame_mediana
    )
  if (!identical(universo, "muestra_motor")) {
    meta$filas_obtenidas_muestra <- NULL
  }
  list(
    columnas = columnas,
    sql = sql,
    cobertura = .cobertura_dbi_vacia(),
    literales = unlist(lapply(resultados, `[[`, "literales"), use.names = TRUE),
    meta = meta
  )
}

.verificar_orden_dbi <- function(conexion, tabla_sql, orden_sql, dialecto,
                                 presupuesto = NULL) {
  alias <- as.character(DBI::dbQuoteIdentifier(conexion, "n_grupos_repetidos"))
  sql <- paste0(
    "SELECT COUNT(*) AS ", alias, " FROM (SELECT ",
    paste(orden_sql, collapse = ", "), " FROM ", tabla_sql,
    " GROUP BY ", paste(orden_sql, collapse = ", "),
    " HAVING COUNT(*) > 1)", dialecto$alias_tabla("lupa_orden_repetido")
  )
  resultado <- .escalar_dbi(
    conexion, sql, "n_grupos_repetidos", presupuesto,
    etapa = "verificacion_orden"
  )
  repetidos <- .escalar_finito_dbi(resultado$valor)
  list(
    unico = resultado$ok && identical(repetidos, 0),
    sql = sql,
    motivo = if (!resultado$ok) {
      paste0("No se pudo verificar la unicidad del orden: ", resultado$motivo)
    } else if (is.na(repetidos)) {
      "La verificacion de unicidad no devolvio un conteo utilizable."
    } else if (repetidos > 0) {
      "Las columnas de orden no identifican cada fila de forma unica."
    } else {
      "Las columnas de orden identifican cada fila de forma unica."
    }
  )
}

# ---- Esquema: enumerar y sondear ----------------------------------------
#
# `SELECT *` es un porton innecesario: los nombres ya los dio
# `dbListFields()`. Y cuando el motor rechaza una columna -un LOB que no
# materializa, un permiso a nivel de columna- enumerar tampoco alcanza por si
# solo: hay que sondear columna por columna y descartar la que falle, para que
# las sanas se perfilen enteras.

.campos_dbi <- function(conexion, tabla, tabla_sql, presupuesto) {
  .contar_dbi(presupuesto)
  campos <- tryCatch(
    DBI::dbListFields(conexion, tabla),
    error = function(e) e
  )
  if (!inherits(campos, "condition") && length(campos)) {
    return(list(campos = campos, motivo = NA_character_, origen = "dbListFields"))
  }
  motivo <- if (inherits(campos, "condition")) {
    conditionMessage(campos)
  } else {
    "dbListFields() no devolvio ningun campo."
  }
  # El metodo por omision de DBI emite `SELECT * ... LIMIT 0`, asi que un motor
  # sin `LIMIT` muere aca, en la primera consulta, con el texto crudo del
  # driver. Se degrada a leer el esquema con una consulta de cero filas.
  respaldo <- .consultar_dbi(
    conexion, paste0("SELECT * FROM ", tabla_sql, " WHERE 1 = 0"),
    presupuesto, etapa = "esquema"
  )
  if (respaldo$ok && length(names(respaldo$datos))) {
    return(list(
      campos = names(respaldo$datos), origen = "select_cero_filas",
      motivo = paste0(
        "dbListFields() no sirvio (", motivo,
        "); los nombres se leyeron con una consulta de cero filas."
      )
    ))
  }
  .detener_dbi("lupa_error_campos_dbi", paste0(
    "No se pudieron enumerar las columnas de la tabla. dbListFields(): ",
    motivo, ". Consulta de cero filas: ",
    if (respaldo$ok) "no devolvio campos" else respaldo$motivo, "."
  ))
}

.sql_esquema_dbi <- function(tabla_sql, campos_sql) {
  paste0(
    "SELECT ", paste(campos_sql, collapse = ", "), " FROM ", tabla_sql,
    " WHERE 1 = 0"
  )
}

.leer_esquema_dbi <- function(conexion, sql, presupuesto, etapa = "esquema") {
  if (!.gastar_dbi(presupuesto)) {
    return(c(list(
      ok = FALSE, datos = NULL, tipos = NULL,
      motivo = .motivo_presupuesto_dbi(presupuesto)
    ), .medicion_consulta_vacia_dbi(etapa)))
  }
  medicion <- .iniciar_consulta_dbi(presupuesto, etapa)
  resultado <- NULL
  on.exit(
    if (!is.null(resultado)) try(DBI::dbClearResult(resultado), silent = TRUE),
    add = TRUE
  )
  salida <- tryCatch({
    resultado <- DBI::dbSendQuery(conexion, sql)
    prototipo <- DBI::dbFetch(resultado, n = 0L)
    informacion <- tryCatch(DBI::dbColumnInfo(resultado), error = function(e) NULL)
    tipos <- if (is.data.frame(informacion) && "type" %in% names(informacion)) {
      as.character(informacion$type)
    } else {
      NULL
    }
    list(ok = TRUE, datos = prototipo, tipos = tipos, motivo = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, datos = NULL, tipos = NULL, motivo = conditionMessage(e))
  })
  .adjuntar_medicion_dbi(
    salida, .terminar_consulta_dbi(medicion, salida$datos)
  )
}

.esquema_dbi <- function(conexion, tabla_sql, campos, presupuesto) {
  campos_sql <- vapply(campos, function(campo) {
    as.character(DBI::dbQuoteIdentifier(conexion, campo))
  }, character(1L), USE.NAMES = FALSE)
  sql <- .sql_esquema_dbi(tabla_sql, campos_sql)
  esquema <- .leer_esquema_dbi(conexion, sql, presupuesto)
  if (esquema$ok) {
    return(list(
      campos = campos, campos_sql = campos_sql, prototipo = esquema$datos,
      tipos = esquema$tipos, sql = sql, ilegibles = character(),
      motivos = list(), sondeo = FALSE
    ))
  }
  # Enumerar no alcanza cuando una de las columnas es la que el motor rechaza.
  # Sondear cuesta una consulta por columna, y solo se paga cuando la lectura
  # conjunta ya fallo.
  motivo_conjunto <- esquema$motivo
  legibles <- character()
  legibles_sql <- character()
  prototipos <- list()
  tipos <- character()
  motivos <- list()
  for (i in seq_along(campos)) {
    sonda <- .sql_esquema_dbi(tabla_sql, campos_sql[[i]])
    parcial <- .leer_esquema_dbi(
      conexion, sonda, presupuesto, etapa = "esquema"
    )
    if (parcial$ok && length(names(parcial$datos))) {
      legibles <- c(legibles, campos[[i]])
      legibles_sql <- c(legibles_sql, campos_sql[[i]])
      prototipos[[length(prototipos) + 1L]] <- parcial$datos[[1L]]
      tipos <- c(tipos, if (length(parcial$tipos)) parcial$tipos[[1L]] else NA_character_)
    } else {
      motivos[[campos[[i]]]] <- paste0(
        "El motor rechazo la lectura de la columna: ",
        if (parcial$ok) "no devolvio campos" else parcial$motivo, "."
      )
    }
  }
  if (!length(legibles)) {
    .detener_dbi("lupa_error_esquema_dbi", paste0(
      "No se pudo leer el esquema de la tabla ni columna por columna. ",
      "Lectura conjunta: ", motivo_conjunto, "."
    ))
  }
  names(prototipos) <- legibles
  prototipo <- prototipos
  list(
    campos = legibles, campos_sql = legibles_sql, prototipo = prototipo,
    tipos = tipos, sql = .sql_esquema_dbi(tabla_sql, legibles_sql),
    ilegibles = names(motivos), motivos = motivos, sondeo = TRUE,
    motivo_conjunto = motivo_conjunto
  )
}

# ---- Plan previo ---------------------------------------------------------

.plan_consultas_dbi <- function(campos, es_numerico, metricas, incluir_valores,
                                  con_orden, dialecto, emitidas = 0,
                                  universo = "tabla_completa",
                                  muestreo_disponible = TRUE,
                                  tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                                  tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                                  incluir_muestra = TRUE,
                                  mediana_consolidada = FALSE,
                                  columnas_distintos = NULL,
                                  columnas_moda = NULL,
                                  columnas_moda_max = NULL,
                                  columnas_mediana = NULL,
                                  columnas_mediana_max = NULL,
                                  consultas_muestra = 1L) {
  n_columnas <- length(campos)
  n_numericas <- sum(es_numerico)
  con_valores <- isTRUE(incluir_valores)
  acota_con_salto <- !is.null(dialecto$limitar("SELECT 1", 1L, 1))
  n_lotes <- function(n, tamano) if (n > 0) ceiling(n / tamano) else 0
  tamanos_lotes <- function(n, tamano) {
    if (n < 1) return(numeric())
    cantidad <- n %/% tamano
    resto <- n %% tamano
    c(
      rep(tamano, cantidad),
      if (resto > 0) resto else numeric()
    )
  }
  extra_biseccion <- function(n, tamano) {
    sum(2 * tamanos_lotes(n, tamano) - 1)
  }
  columnas_planas <- if ("validos" %in% metricas) n_columnas else n_numericas
  hay_planos <- any(c("validos", "basicos", "desvio") %in% metricas) &&
    columnas_planas > 0
  mide_metricas <- !identical(universo, "muestra_motor") ||
    isTRUE(muestreo_disponible)
  n_metricas <- function(valor) if (mide_metricas) valor else 0
  n_distintos <- if (is.null(columnas_distintos)) n_columnas else {
    length(columnas_distintos)
  }
  n_moda <- if (is.null(columnas_moda)) n_columnas else length(columnas_moda)
  n_moda_max <- if (is.null(columnas_moda_max)) n_moda else {
    length(columnas_moda_max)
  }
  n_mediana <- if (is.null(columnas_mediana)) n_numericas else {
    length(columnas_mediana)
  }
  n_mediana_max <- if (is.null(columnas_mediana_max)) n_mediana else {
    length(columnas_mediana_max)
  }
  alcance_agregado <- if (identical(universo, "muestra_motor")) {
    "lee una muestra del motor"
  } else {
    "escanea la tabla completa"
  }
  clases <- list(c(
    "portones (esquema y sondas)", emitidas, "una vez"
  ))
  if (hay_planos) {
    n_planos <- n_lotes(columnas_planas, tamano_lote_planos)
    if ("desvio" %in% metricas && n_numericas > 0) {
      n_planos <- n_planos + .PEAJE_FORMA_DESVIO
    }
    clases[[length(clases) + 1L]] <- c(
      "agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio)",
      n_metricas(n_planos), alcance_agregado
    )
  }
  if (!hay_planos) {
    clases[[length(clases) + 1L]] <- c(
      "total exacto (COUNT)", 1, "escanea la tabla completa"
    )
  }
  if ("distintos" %in% metricas && n_distintos > 0) {
    clases[[length(clases) + 1L]] <- c(
      "distintos por lotes (clase separada)",
      n_metricas(n_lotes(n_distintos, tamano_lote_distintos)), alcance_agregado
    )
  }
  clases <- c(
    clases,
    list(
      c(
        "moda (GROUP BY + orden + limite)",
        if ("moda" %in% metricas && con_valores) n_metricas(n_moda) else 0,
        if (identical(universo, "muestra_motor")) "lee una muestra del motor" else
          "ordena o agrupa la tabla completa",
        if ("moda" %in% metricas && con_valores) n_metricas(n_moda_max) else 0
      ),
      c(
        "mediana (orden total + limite/salto)",
        if ("mediana" %in% metricas && con_valores && acota_con_salto) {
          if (isTRUE(mediana_consolidada)) {
            n_metricas(n_lotes(n_mediana, tamano_lote_planos))
          } else {
            n_metricas(n_mediana)
          }
        } else 0,
        if (identical(universo, "muestra_motor")) "lee una muestra del motor" else
          "ordena la tabla completa",
        if ("mediana" %in% metricas && con_valores && acota_con_salto) {
          if (isTRUE(mediana_consolidada)) {
            n_metricas(n_lotes(n_mediana_max, tamano_lote_planos))
          } else {
            n_metricas(n_mediana_max)
          }
        } else 0
      ),
      c(
        "verificacion de unicidad del orden", if (con_orden) 1 else 0,
        "ordena o agrupa la tabla completa"
      )
    )
  )
  if (isTRUE(incluir_muestra)) {
    clases[[length(clases) + 1L]] <- c(
      "muestra", consultas_muestra, "lee las filas pedidas"
    )
  }
  plan <- data.frame(
    clase_consulta = vapply(clases, function(x) x[[1L]], character(1L)),
    n_consultas = vapply(clases, function(x) as.numeric(x[[2L]]), numeric(1L)),
    alcance = vapply(clases, function(x) x[[3L]], character(1L)),
    n_consultas_max = vapply(clases, function(x) {
      if (length(x) >= 4L) as.numeric(x[[4L]]) else as.numeric(x[[2L]])
    }, numeric(1L)),
    stringsAsFactors = FALSE
  )
  plan <- plan[plan$n_consultas > 0 | plan$n_consultas_max > 0, , drop = FALSE]
  rownames(plan) <- NULL
  # El total NO es un techo cuando el motor rechaza lotes. La biseccion vuelve a
  # sondear el grupo y puede recorrer el arbol hasta 2n - 1 nodos adicionales
  # para un lote de n columnas; las respuestas de los grupos aceptados se
  # reutilizan como resultados. Se publica el rango para declarar ese peor caso
  # sin suponer que todas las columnas individuales fallan.
  attr(plan, "extra_si_se_rechazan_lotes") <-
    n_metricas(
      (if ("distintos" %in% metricas) {
        extra_biseccion(n_distintos, tamano_lote_distintos)
      } else 0) +
      (if (hay_planos) {
        extra_biseccion(columnas_planas, tamano_lote_planos)
      } else 0)
    )
  plan
}

# Cuantas consultas se emiten no dice cuanto cuestan. Catorce consultas sobre
# dos millones de filas son mucho mas trabajo que doscientas sobre mil, y el
# plan que solo cuenta consultas deja al usuario sin la pregunta que de verdad
# tiene: si esto tarda segundos, minutos u horas.
#
# La magnitud se estima en dos numeros que son cuentas de verdad y no indices
# inventados: cuantas filas hay que leer, y cuantas veces hay que ordenar la
# tabla entera. El peso de cada clase sale de su `alcance`, que es vocabulario
# cerrado -lo escribe `.plan_consultas_dbi()` y nadie mas-.
.lecturas_clase_dbi <- function(alcance, n_consultas, filas, muestra) {
  switch(
    alcance,
    # Los portones son un punado de consultas triviales mas un COUNT(*), que si
    # recorre la tabla: se cuenta esa pasada y no el resto.
    "una vez" = filas,
    "escanea la tabla completa" = n_consultas * filas,
    "escanea la tabla completa dos veces" = 2 * n_consultas * filas,
    "ordena la tabla completa" = n_consultas * filas,
    "ordena o agrupa la tabla completa" = n_consultas * filas,
    # `muestra` llega SIEMPRE finita: el unico que llama es `.trabajo_plan_dbi()`,
    # que traduce ahi el `Inf` -toda la tabla- a `filas` antes de bajar hasta
    # aca. Hubo un tiempo en que esta linea intentaba traducirlo tambien, con un
    # `if (is.finite(muestra)) muestra else filas`; la rama `else` nunca corria
    # porque la normalizacion de arriba ya habia cerrado el valor, y el
    # comentario que la acompanaba afirmaba una traduccion que no ocurria. Dos
    # sitios que dicen hacer lo mismo y uno que no se ejecuta es peor que uno
    # solo: la traduccion vive arriba y aca se cuenta lo que llego.
    "lee una muestra del motor" = n_consultas * muestra,
    "lee las filas pedidas" = muestra,
    NA_real_
  )
}

.UMBRAL_TRABAJO_MEDIO_DBI <- 1e7
.UMBRAL_TRABAJO_ALTO_DBI <- 1e9

# El otro medio del reloj. Lo de arriba cuenta el trabajo del MOTOR; el
# detector de vocabulario y las comparaciones de proximidad se hacen en R,
# sobre la muestra, y no aparecen en ninguna lectura de fila. Una tabla de
# 3.912 filas con una columna de geometria en texto salia "baja" -64.592
# lecturas- y tardaba 35 segundos: cada numero que informaba era cierto y el
# juicio era falso, porque medir la mitad y llamarlo el total es informar como
# completo algo parcial.
#
# La unidad es el par de formas comparadas, que es una cuenta y no un indice:
# la muestra trae m filas, las formas distintas son a lo sumo m, y el detector
# nunca compara mas de `max_pares` por columna. Cuanto cuesta cada par depende
# del largo de los valores, que el plan no puede saber sin leerlos; el supuesto
# queda declarado igual que el de `log2(filas)`.
.UMBRAL_PARES_MEDIO_DBI <- 2e6
.UMBRAL_PARES_ALTO_DBI <- 2e8

# Los alcances que traen filas a R. Vocabulario cerrado: lo escribe
# `.plan_consultas_dbi()` y nadie mas.
# Los dos alcances que involucran una muestra, que NO son la misma cosa:
# `"lee una muestra del motor"` es trabajo del motor -cuando
# `universo = "muestra_motor"`, el motor muestrea para sus propios agregados- y
# `"lee las filas pedidas"` es
# el bloque del cliente, que es lo unico que trae filas a R.
.ALCANCES_CON_MUESTRA_DBI <- c("lee una muestra del motor", "lee las filas pedidas")

# El trabajo del CLIENTE cuelga solo del segundo. Contarlo sobre el conjunto de
# los dos hacia que `universo = "muestra_motor"` con `bloque_muestra = "solo_agregados"`
# declarara cuatro millones de pares de formas a comparar en R sin traer una sola
# fila, y el plan impreso se contradecia a dos lineas: "el plan incluye solo
# agregados SQL" y despues "mas 4.000.000 pares de formas a comparar en R".
.ALCANCE_BLOQUE_CLIENTE_DBI <- "lee las filas pedidas"

# El tope de pares se lee de la firma del detector en vez de copiarse: si el
# valor cambia alla, el plan no se queda estimando contra un numero viejo.
.max_pares_vocabulario_dbi <- function() {
  as.numeric(eval(formals(.grupos_casi_duplicados_vocabulario)$max_pares))
}

.ORDEN_MAGNITUD_DBI <- c("baja", "media", "alta")

.mayor_magnitud_dbi <- function(a, b) {
  pos_a <- match(a, .ORDEN_MAGNITUD_DBI)
  pos_b <- match(b, .ORDEN_MAGNITUD_DBI)
  if (is.na(pos_a) || is.na(pos_b)) return("desconocida")
  .ORDEN_MAGNITUD_DBI[[max(pos_a, pos_b)]]
}

.trabajo_plan_dbi <- function(plan, filas, muestra, columnas_texto = 0,
                              max_pares = NULL) {
  vacio <- list(
    filas_leidas = NA_real_, ordenaciones = NA_real_,
    equivalente = NA_real_, magnitud = "desconocida",
    magnitud_motor = "desconocida", magnitud_texto = "desconocida",
    columnas_texto = NA_real_, pares_texto = NA_real_
  )
  # El conteo se convierte primero y se valida despues, en vez de exigir
  # `is.numeric()`: sobre una tabla grande `n_total` llega como `integer64`, y
  # `is.numeric()` da FALSE para esa clase. Rechazarla habria dejado sin
  # estimacion justo el caso donde mas importa.
  #
  # Cada guarda esta por un caso que se probo y fallaba: `Inf` hacia NaN al
  # multiplicar por cero ordenaciones y reventaba el `if`; un conteo negativo
  # daba magnitud "baja" con lecturas negativas; y un conteo no numerico
  # emitia un aviso de coercion antes de rendirse.
  numero <- function(x) {
    if (is.null(x) || length(x) != 1L) return(NA_real_)
    valor <- suppressWarnings(as.numeric(x))
    if (length(valor) != 1L || is.na(valor) || !is.finite(valor) ||
        valor < 0) {
      return(NA_real_)
    }
    valor
  }
  filas <- numero(filas)
  if (!nrow(plan)) return(vacio)
  muestra_entera <- is.numeric(muestra) && length(muestra) == 1L &&
    !is.na(muestra) && is.infinite(muestra) && muestra > 0
  if (is.na(filas)) {
    # Aunque el tamano de la tabla sea desconocido, una muestra finita pedida
    # por el usuario acota el trabajo que ocurrira en R. La mitad del motor
    # sigue desconocida; no se convierte esa incertidumbre en cero.
    n_texto <- numero(columnas_texto)
    if (is.na(n_texto)) n_texto <- 0
    muestra_numero <- numero(muestra)
    if (is.na(muestra_numero) && muestra_entera) muestra_numero <- NA_real_
    max_pares_numero <- if (is.null(max_pares)) {
      .max_pares_vocabulario_dbi()
    } else numero(max_pares)
    if (is.na(max_pares_numero)) max_pares_numero <- Inf
    hay_bloque_cliente <- any(plan$alcance %in% .ALCANCE_BLOQUE_CLIENTE_DBI)
    pares <- if (!hay_bloque_cliente) {
      0
    } else if (is.finite(muestra_numero)) {
      n_texto * min(muestra_numero * (muestra_numero - 1) / 2,
                    max_pares_numero)
    } else {
      NA_real_
    }
    if (!is.na(pares) && is.finite(pares)) {
      magnitud_texto <- if (pares < .UMBRAL_PARES_MEDIO_DBI) {
        "baja"
      } else if (pares < .UMBRAL_PARES_ALTO_DBI) {
        "media"
      } else {
        "alta"
      }
    } else {
      magnitud_texto <- "desconocida"
    }
    vacio$columnas_texto <- n_texto
    vacio$pares_texto <- pares
    vacio$magnitud_texto <- magnitud_texto
    return(vacio)
  }
  # `muestra = Inf` no es un valor invalido: es "la tabla entera", y es el valor
  # por omision. `numero()` rechaza lo no finito -- bien para `filas` y para los
  # conteos, que con `Inf` hacian NaN --, asi que aca hay que traducirlo antes de
  # perderlo. Sin esta linea el plan contaba el bloque de muestra como CERO filas
  # leidas y CERO pares de formas: `muestra = Inf` declaraba menos trabajo que
  # `muestra = filas` pidiendo exactamente las mismas filas, y caia en magnitud
  # "baja", que es la que no imprime palancas. Medido sobre 200.000 x 4:
  # 400.000 lecturas y 0 pares contra 600.000 y 4.000.000.
  muestra <- numero(muestra)
  if (is.na(muestra)) muestra <- if (muestra_entera) filas else 0
  # Y se acota una sola vez, aca, por el mismo motivo. Pedir mas filas de las
  # que hay no trae mas filas: la lectura real es `min(n_total, muestra)`. Sin
  # este tope, `muestra = 1e6` sobre una tabla de 100 filas declaraba 1.001.200
  # lecturas contra las 1.300 de `muestra = 100`, trayendo las dos las mismas
  # 100 filas. El lado del cliente ya se acotaba -por eso `pares_texto` daba
  # bien-, o sea que las dos mitades de la cuenta usaban tamanos distintos.
  muestra <- min(filas, muestra)
  lecturas <- vapply(seq_len(nrow(plan)), function(i) {
    suppressWarnings(as.numeric(.lecturas_clase_dbi(
      plan$alcance[[i]], plan$n_consultas[[i]], filas, muestra
    )))
  }, numeric(1L))
  if (anyNA(lecturas) || any(!is.finite(lecturas))) return(vacio)
  ordena <- grepl("^ordena", plan$alcance)
  ordenaciones <- sum(plan$n_consultas[ordena])
  # Ordenar no cuesta lo mismo que recorrer: una ordenacion completa se cuenta
  # como log2(filas) pasadas. Es el supuesto de libro y queda declarado, para
  # que quien no lo comparta pueda rehacer la cuenta con los dos numeros de
  # arriba, que no dependen de el.
  factor_orden <- max(1, log2(max(2, filas)))
  equivalente <- sum(lecturas) + ordenaciones * filas * (factor_orden - 1)
  if (!is.finite(equivalente)) return(vacio)
  magnitud_motor <- if (equivalente < .UMBRAL_TRABAJO_MEDIO_DBI) {
    "baja"
  } else if (equivalente < .UMBRAL_TRABAJO_ALTO_DBI) {
    "media"
  } else {
    "alta"
  }
  # El trabajo por valor solo existe si algo se trae a R, y por eso se pregunta
  # por el alcance de las consultas y no por una etiqueta de preset. `metricas`
  # selecciona SQL; `bloque_muestra` decide aparte si el plan trae filas. Cuando ese bloque
  # se omite no hay pares de formas que contar, aunque haya columnas de texto.
  n_texto <- numero(columnas_texto)
  if (is.na(n_texto)) n_texto <- 0
  if (is.null(max_pares)) max_pares <- .max_pares_vocabulario_dbi()
  tope_pares <- numero(max_pares)
  if (is.na(tope_pares)) tope_pares <- Inf
  filas_muestra <- if (any(plan$alcance %in% .ALCANCE_BLOQUE_CLIENTE_DBI)) {
    min(filas, muestra)
  } else {
    0
  }
  pares_texto <- n_texto * min(filas_muestra * (filas_muestra - 1) / 2, tope_pares)
  if (!is.finite(pares_texto)) pares_texto <- 0
  magnitud_texto <- if (pares_texto < .UMBRAL_PARES_MEDIO_DBI) {
    "baja"
  } else if (pares_texto < .UMBRAL_PARES_ALTO_DBI) {
    "media"
  } else {
    "alta"
  }
  list(
    filas_leidas = sum(lecturas), ordenaciones = ordenaciones,
    equivalente = equivalente,
    magnitud = .mayor_magnitud_dbi(magnitud_motor, magnitud_texto),
    magnitud_motor = magnitud_motor, magnitud_texto = magnitud_texto,
    columnas_texto = n_texto, pares_texto = pares_texto
  )
}

.SUPUESTO_TRABAJO_DBI <- paste(
  "El trabajo es una estimaci\u00f3n, no una medici\u00f3n, y son dos",
  "mitades. La del motor cuenta cu\u00e1ntas filas habr\u00eda que leer si",
  "ning\u00fan \u00edndice ayudara, y cuenta cada ordenaci\u00f3n completa",
  "como log2(filas) pasadas; un \u00edndice sobre la columna ordenada, o una",
  "tabla que entra en la memoria del motor, la bajan mucho. La del cliente",
  "cuenta los pares de formas que el detector de vocabulario podr\u00eda",
  "comparar en R sobre la muestra: como mucho `max_pares` por columna de",
  "texto. El conteo de pares es exacto; lo que el plan no puede saber sin",
  "leer los valores es cu\u00e1nto cuesta cada uno, que depende de su largo,",
  "as\u00ed que con valores muy largos el tiempo real es varias veces el que",
  "sugiere la referencia. Referencias: unos cinco millones de lecturas de fila",
  "por segundo sobre PostgreSQL 16 local (2.000.000 de filas por 40 columnas en",
  "metricas = c('validos', 'basicos', 'desvio'): 14 consultas, 5,3 segundos). Ese cociente est\u00e1 en las",
  "unidades que cuenta este plan y proviene de otras corridas, no es el tiempo",
  "de esta tabla.",
  "No representa filas que el motor haya le\u00eddo: la",
  "cuenta supone que ning\u00fan \u00edndice ayuda y cobra el desv\u00edo como",
  "dos pasadas, aunque un motor con desv\u00edo nativo lo resuelva en una. Sirve",
  "para convertir `filas_leidas` en segundos, que es para lo que est\u00e1, y no",
  "como medida de lo que el motor lee. Y de 660.000 a 1.150.000 pares por",
  "segundo sobre valores de cuarenta caracteres -la banda cubre dos m\u00e1quinas",
  "distintas-, que bajan a entre 70.000 y 80.000 sobre valores de doscientos.",
  "Esa tasa",
  "cuenta los pares que se comparan de verdad: con valores largos el detector",
  "recorta por `max_trabajo`, y dividir por los pares que el plan contar\u00eda",
  "inflaba la cifra cuatro veces."
)

.SUPUESTO_COSTO_DISTINTOS_PLAN_DBI <- paste(
  "El plan no proyecta el costo temporal de `COUNT(DISTINCT)` antes de",
  "correr: la unica referencia honesta es el primer lote de distintos medido",
  "en esta corrida, y el plan no emite consultas de datos."
)

.MOTIVO_MEMORIA_PROCESAMIENTO_DBI <- paste(
  "no escala de forma predecible con las filas ni con las celdas; se midio."
)

.REFERENCIAS_MEMORIA_PROCESAMIENTO_DBI <- list(
  traer = "~0,13 GB por millon de filas",
  procesar = "~1,0-1,5 MB por cada mil filas",
  variacion = "1,62x entre tablas de la misma magnitud"
)

.celdas_plan_dbi <- function(filas, columnas) {
  valores <- vapply(list(filas, columnas), function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }, numeric(1L))
  if (anyNA(valores) || any(!is.finite(valores)) || any(valores < 0)) {
    return(NA_real_)
  }
  celdas <- valores[[1L]] * valores[[2L]]
  if (!is.finite(celdas)) NA_real_ else celdas
}

.memoria_procesamiento_plan_dbi <- function(filas, celdas, pares_texto) {
  list(
    estado = "no_estimada",
    estimada = FALSE,
    motivo = .MOTIVO_MEMORIA_PROCESAMIENTO_DBI,
    magnitud = list(
      filas = filas,
      celdas = celdas,
      pares_texto = pares_texto
    ),
    referencias = .REFERENCIAS_MEMORIA_PROCESAMIENTO_DBI,
    distincion = paste(
      "Ver todas las filas y tener todas las filas en memoria no son lo mismo."
    ),
    reparto_observado = paste(
      "En corridas de referencia, 4,5 M de filas entraron en 0,6 GB y tardaron",
      "25 s; procesar 4,5 M requirio aproximadamente 7 GB y 12,8 M",
      "aproximadamente 19 GB. El problema observado esta en el procesamiento",
      "en R, no en la red ni en el motor."
    )
  )
}

.filas_plan_dbi <- function(preparacion) {
  vacio <- .denominador_catalogo_vacio_dbi(paste(
    "No hay una estimacion utilizable de filas en el catalogo ya leido;",
    "se conserva `sin dato filas`."
  ))
  estimacion <- preparacion$estimacion_derrame
  filas_catalogo <- if (is.null(estimacion)) NULL else {
    estimacion$filas_catalogo
  }
  if (is.list(filas_catalogo) && isTRUE(filas_catalogo$disponible)) {
    return(filas_catalogo)
  }
  if (is.list(filas_catalogo) && !is.null(filas_catalogo$motivo) &&
      length(filas_catalogo$motivo) == 1L &&
      !is.na(filas_catalogo$motivo) && nzchar(filas_catalogo$motivo)) {
    vacio$motivo <- if (grepl("sin dato filas", filas_catalogo$motivo,
                              fixed = TRUE)) {
      filas_catalogo$motivo
    } else {
      paste(filas_catalogo$motivo, "Se conserva `sin dato filas`.")
    }
  }
  # `estrategia = "catalogo"` ya trae el mismo denominador en cada estimacion
  # de columna. Se reutiliza ese valor y se exige que no haya dos universos en
  # la misma respuesta, igual que en `.seleccionar_fila_catalogo_dbi()`.
  estrategia <- preparacion$estrategia_distintos
  estimaciones <- if (is.null(estrategia)) NULL else estrategia$estimaciones
  if (identical(estrategia$estado, "estimado_catalogo") &&
      is.list(estimaciones) && length(estimaciones)) {
    filas <- vapply(estimaciones, function(x) {
      if (!is.list(x)) return(NA_real_)
      .numero_dbi(x$n_filas)
    }, numeric(1L))
    filas <- filas[is.finite(filas) & filas > 0]
    if (length(filas) && length(unique(filas)) == 1L) {
      return(list(
        disponible = TRUE, estado = "estimado_catalogo", filas = filas[[1L]],
        fuente = "pg_class.reltuples", n_relaciones = NA_integer_,
        motivo = paste(
          "Las filas se toman de `pg_class.reltuples` a traves de la",
          "jerarquia que ya leyo la estrategia `catalogo`. Es una estimacion",
          "de catalogo, no una medicion del universo de la tabla."
        )
      ))
    }
  }
  vacio
}

.proyeccion_plan_vacia_dbi <- function(motivo, unidad) {
  list(
    disponible = FALSE, estado = "no_disponible", unidad = unidad,
    magnitud = NA_real_, filas = NA_real_, n_columnas = 0L,
    n_medianas = 0L, n_distintos_proyectados = NA_real_,
    fuente = NA_character_, motivo = motivo
  )
}

.proyecciones_plan_catalogo_dbi <- function(
    preparacion, filas_plan, incluir_valores = TRUE) {
  motivo_filas <- filas_plan$motivo
  vacia_moda <- .proyeccion_plan_vacia_dbi(
    motivo_filas, "n_distintos_estimados"
  )
  vacia_mediana <- .proyeccion_plan_vacia_dbi(motivo_filas, "filas_estimadas")
  if (!isTRUE(filas_plan$disponible)) {
    return(list(moda = vacia_moda, mediana = vacia_mediana))
  }
  fuente <- "estimacion de catalogo: pg_class.reltuples"
  if ("moda" %in% preparacion$metricas_ejecucion &&
      isTRUE(incluir_valores)) {
    datos <- preparacion$estimacion_derrame$columnas
    if ((!is.data.frame(datos) || !nrow(datos)) &&
        is.list(preparacion$fuentes_cardinalidad_costo)) {
      fuentes <- preparacion$fuentes_cardinalidad_costo
      datos <- data.frame(
        columna = names(fuentes),
        n_distintos_estimados = vapply(
          fuentes,
          function(fuente) {
            if (!is.list(fuente)) {
              return(NA_real_)
            }
            .numero_dbi(fuente$n_distintos)
          },
          numeric(1L)
        ),
        stringsAsFactors = FALSE
      )
    }
    if (is.data.frame(datos) && all(c(
      "columna", "n_distintos_estimados"
    ) %in% names(datos))) {
      columnas <- preparacion$campos
      datos <- datos[match(columnas, datos$columna), , drop = FALSE]
      conocidos <- is.finite(datos$n_distintos_estimados) &
        datos$n_distintos_estimados >= 0
      if (any(conocidos)) {
        n_distintos <- sum(datos$n_distintos_estimados[conocidos])
        if (is.finite(n_distintos)) {
          vacia_moda <- list(
            disponible = TRUE, estado = "estimado_catalogo",
            unidad = "n_distintos_estimados", magnitud = n_distintos,
            filas = filas_plan$filas,
            n_columnas = as.integer(sum(conocidos)), n_medianas = 0L,
            n_distintos_proyectados = n_distintos,
            fuente = paste(
              "estimacion de catalogo:",
              "pg_stats.n_distinct + pg_class.reltuples"
            ),
            motivo = paste(
              "La magnitud de moda usa la suma de `n_distintos` estimados",
              "por el catalogo; no es una duracion ni una medicion. Las",
              "columnas sin estadistica quedan fuera de esta proyeccion."
            )
          )
        }
      }
    }
  } else {
    vacia_moda$estado <- "no_solicitado"
    vacia_moda$motivo <- "La moda no se solicito en este plan."
  }
  if ("mediana" %in% preparacion$metricas_ejecucion &&
      isTRUE(incluir_valores)) {
    n_medianas <- sum(preparacion$es_numerico)
    magnitud <- filas_plan$filas * n_medianas
    if (n_medianas > 0 && is.finite(magnitud)) {
      vacia_mediana <- list(
        disponible = TRUE, estado = "estimado_catalogo",
        unidad = "filas_estimadas", magnitud = magnitud,
        filas = filas_plan$filas, n_columnas = 0L,
        n_medianas = as.integer(n_medianas),
        n_distintos_proyectados = NA_real_, fuente = fuente,
        motivo = paste(
          "La magnitud de mediana usa las filas estimadas por",
          "`pg_class.reltuples`; no es una duracion ni una medicion."
        )
      )
    }
  } else {
    vacia_mediana$estado <- "no_solicitado"
    vacia_mediana$motivo <- "La mediana no se solicito en este plan."
  }
  list(moda = vacia_moda, mediana = vacia_mediana)
}

.texto_filas_plan_dbi <- function(filas, fuente = NULL) {
  numero <- .numero_dbi(filas)
  if (length(numero) != 1L || is.na(numero) || !is.finite(numero)) {
    return("sin dato filas")
  }
  texto <- .miles_dbi(numero)
  if (!is.null(fuente) && length(fuente) && !is.na(fuente) &&
      grepl("pg_class\\.reltuples", fuente, fixed = FALSE)) {
    paste0("~", texto, " filas (estimacion de catalogo)")
  } else {
    paste0(texto, " filas")
  }
}


#' Planificar el costo de `perfilar_dbi()` antes de pagarlo
#'
#' Emite sólo consultas de preparación —leer el esquema y sondear capacidades—
#' y devuelve cuántas consultas emitiría el perfilado completo, de qué clase y
#' con qué alcance sobre la tabla. No escanea datos para decidir el costo.
#' Cuando `politica_costo = "por_cardinalidad"`, una clave estructural exacta
#' puede cerrar la decisión; si no hay una fuente de catálogo utilizable, el
#' plan publica el rango entre omitir y ejecutar la moda; la mediana no entra en
#' ese criterio proporcional. Nunca lanza `COUNT(DISTINCT ...)` para despejar
#' esa incertidumbre.
#' Las fuentes estructurales se resuelven cuando la política necesita la
#' cardinalidad, aunque `estrategia_distintos` no permita medirla. La
#' disponibilidad de la estrategia gobierna la medición, no el conocimiento que
#' ya da el catálogo.
#' Para el `COUNT(DISTINCT)` exacto, la preparación puede consultar además las
#' estadísticas de PostgreSQL (`pg_stats`, `pg_class.reltuples`, `SHOW work_mem`
#' y, desde PostgreSQL 13, `SHOW hash_mem_multiplier`) para estimar el tamaño
#' del hash y avisar un posible derrame. Esa consulta de metadatos no publica
#' cardinalidad medida ni reemplaza la medición posterior.
#' Para la moda y la mediana se publican, además, los atributos
#' `estimacion_derrame_moda` y `estimacion_derrame_mediana`. La moda deriva su
#' `metodo` (`"hash"` o `"sort"`) del primer `Aggregate` de un
#' `EXPLAIN (FORMAT JSON, COSTS OFF)` de la consulta exacta, sin `ANALYZE` ni
#' lectura de datos. La mediana siempre modela un sort y distingue su huella de
#' decisión de la magnitud del tape. Ambas usan `n_validos` de catálogo en el
#' plan, pisos de 32 bytes para tipos fijos y 42 para `numeric`, y dejan el
#' objeto como `no_disponible` cuando el catálogo o el motor no permiten una
#' estimación.
#' Cuando esa lectura de catálogo trae `pg_class.reltuples` positivo, el plan lo
#' reutiliza como una estimación declarada del número de filas. La magnitud y las
#' proyecciones de trabajo de moda y mediana quedan entonces disponibles y dicen
#' explícitamente `estimado_catalogo`; no son duraciones ni mediciones. Un valor
#' cero o negativo —típico de una relación sin `ANALYZE`— conserva el estado
#' `sin dato filas`. Los demás motores conservan ese estado si no tienen una
#' lectura de catálogo ya disponible.
#'
#' `estrategia_distintos` declara la procedencia de `n_distintos` antes de la
#' corrida y conserva por separado lo pedido, lo resuelto y el estado. No hay
#' `auto`: `"exacta"` es el valor por omisión, `"aproximada_motor"` queda
#' `no_disponible` si el motor no ofrece una función aceptada, `"catalogo"`
#' lee `pg_stats.n_distinct` en PostgreSQL y publica una estimación con estado
#' `estimado_catalogo` cuando `universo = "tabla_completa"`, y `"omitida"` no
#' emite el agregado. Con `universo = "muestra_motor"`, `catalogo` queda
#' `no_disponible`: sus estadísticas describen la relación entera y no el
#' subconjunto de la corrida.
#' Un `n_distinct`
#' positivo es un conteo y uno negativo una fracción de las filas. Si la
#' relación tiene descendientes se usa la fila `inherited = TRUE`, que describe
#' la consulta sin `ONLY`; si no tiene hijas se usa la única fila propia. La
#' fracción se convierte con la suma de `pg_class.reltuples` de la jerarquía.
#' Si falta una estimación utilizable —por ejemplo, antes de `ANALYZE`— o hay
#' filas ambiguas, se conserva `no_disponible`, nunca cero.
#' `fuente_cardinalidad_costo` sigue siendo independiente y sólo describe el
#' número usado por la política de costo cuando esa política se pide.
#'
#' El plan previo no proyecta segundos para `COUNT(DISTINCT)`: no lee los datos
#' y, por tanto, no tiene una referencia medida. La única referencia honesta es
#' el primer lote de distintos de la corrida, pero obtenerla cuesta una
#' consulta que este planificador no emite. Durante `perfilar_dbi()`, cuando hay
#' más de un lote y la instrumentación está activa, esa primera medición se
#' multiplica por la cantidad de lotes y se publica en
#' `resumen_tabla$meta$costo_distintos`. El aviso temporal llega después del
#' primer lote y antes del segundo; con un solo lote se declara que no hay nada
#' que proyectar.
#'
#' La memoria del procesamiento no se estima: no escala de forma predecible con
#' las filas ni con las celdas, y eso se midió. El atributo
#' `memoria_procesamiento` conserva esa declaración, la magnitud conocida del
#' trabajo (`filas`, `celdas` y `pares_texto`) y referencias medidas de otras
#' corridas. Esas referencias no son una predicción para la tabla del plan:
#' traer costó aproximadamente 0,13 GB por millón de filas y procesar en R
#' aproximadamente 1,0-1,5 MB por cada mil filas, pero esta segunda cifra varió
#' 1,62x entre tablas de la misma magnitud.
#'
#' Ver todas las filas y tener todas las filas en memoria no son lo mismo. En
#' corridas de referencia, 4,5 millones de filas entraron en 0,6 GB y tardaron
#' 25 segundos, mientras que procesar 4,5 millones ocupó aproximadamente 7 GB
#' y procesar 12,8 millones aproximadamente 19 GB. El problema observado está
#' en el procesamiento en R, no en la red ni en el motor.
#'
#' Para `universo = "muestra_motor"`, el plan declara una única selección que
#' se pagará para cerrar el spool externo de sesión cliente. `attr(plan,
#' "materializacion")` publica `pagado = FALSE`, backend, versión, presupuesto,
#' result set, fetches esperados, filas y bytes; `attr(plan, "pasadas")` declara
#' que valor, índice y LSH leerán el mismo spool. La referencia medida para
#' justificar este costo es PostgreSQL 16, 2 millones de filas: 10.000, 100.000
#' y 500.000 filas dieron 0,448/1,684/5,598 s con spool frente a
#' 0,814/2,178/3,265 s reordenando cada pasada; el cruce está entre 100.000 y
#' 500.000, y la elección prioriza identidad.
#'
#' @inheritParams perfilar_dbi
#' @param bloque_filas En la via I1, entero positivo que activa la fuente por
#'   bloques de `tabla_completa`. Cada bloque se obtiene con un unico result
#'   set (`dbSendQuery()` + `dbFetch(n = bloque_filas)`) y se absorbe antes de
#'   liberar la entrada. `NULL` conserva el plan historico. El plan publica la
#'   proyeccion, el orden, los bloques previstos y el costo declarado sin leer
#'   las filas.
#' @param instrumentar En el plan, si es `TRUE`, cronometra las consultas de
#'   preparación. No habilita consultas de datos ni agrega mediciones al objeto
#'   devuelto: sus costos siguen siendo predicciones. Por omisión es `FALSE`.
#'
#' @return Data frame de clase `plan_perfilado_dbi` con `clase_consulta`,
#'   `n_consultas`, `n_consultas_max` y `alcance`, y los atributos `total`,
#'   `total_minimo`, `total_maximo`, `total_lotes_rechazados`, `columnas`,
#'   `columnas_numericas`, `dialecto`, `consultas_emitidas`, `metricas`,
#'   `metricas_ejecucion`, `politica_costo`, `estrategia_distintos`,
#'   `fuente_cardinalidad_costo`, `moda_guardian`, `mediana_consolidada`, `filas`,
#'   `filas_fuente`, `estimacion_filas`, `proyecciones`, `mediana_escalar`,
#'   `tamano_lote_planos`, `tamano_lote_distintos`, `estimacion_derrame`,
#'   `estimacion_derrame_moda`, `estimacion_derrame_mediana`,
#'   `celdas`, `memoria_procesamiento`, `max_celdas_muestra`,
#'   `max_bytes_muestra`, `tope_muestra` y `muestreo`, y,
#'   cuando se pide `distintos`, `supuesto_costo_distintos`.
#'   `memoria_procesamiento` siempre tiene `estado = "no_estimada"`: no es una
#'   estimación de consumo, sino la declaración de su ausencia, el motivo, la
#'   magnitud del trabajo y referencias medidas de otras corridas. El atributo
#'   `estimacion_derrame` es independiente: sólo describe la estimación del hash
#'   en el motor para `COUNT(DISTINCT)` y no la memoria del procesamiento en R.
#'   `estimacion_derrame_moda` y `estimacion_derrame_mediana` describen,
#'   respectivamente, el nodo real de la moda y el sort de la mediana. Sus
#'   lotes deciden por el máximo de columna; en una mediana consolidada,
#'   `estado_io_total_bytes` es sólo la suma informativa de los tapes.
#'   `muestreo` declara si la forma muestreada se pudo construir sin emitir una
#'   consulta de datos. En `universo = "muestra_motor"`, cuando su `estado` es
#'   `"no_disponible"`, el plan excluye las métricas SQL de esa muestra y
#'   `supuesto` conserva el motivo.
#'   Cuando se pide `bloque_muestra = "solo_agregados"`, también conserva ese
#'   valor en el atributo `bloque_muestra` y no incluye la fila de la lectura de
#'   muestra.
#'
#'   Los atributos `max_celdas_muestra`, `max_bytes_muestra` y `tope_muestra`
#'   declaran la cota que se aplicará al bloque `perfil_muestra`. El plan no lee
#'   datos: resuelve la cota de celdas con el ancho del esquema y declara que la
#'   cota de bytes requiere una sonda de hasta cien filas durante la corrida.
#'   Si la muestra pedida supera la cota de celdas, `tope_muestra` y `print()`
#'   lo dicen antes de correr. La fila de consulta de muestra conserva su
#'   conteo separado de los agregados; los topes no cambian ninguna consulta
#'   SQL de resumen.
#'
#'   El costo no se declara como un número sino como un rango: `total` es el
#'   extremo inferior, que supone que la política omite la moda cuya cardinalidad
#'   no se conoce, y `total_maximo` el superior, que supone que la ejecuta. La
#'   mediana queda fuera de ese supuesto proporcional y se cuenta según las
#'   columnas numéricas solicitadas. Ambos incluyen la preparación y el perfilado; el rechazo
#'   de lotes puede agregar las sondas de bisección declaradas por
#'   `total_lotes_rechazados`. El costo real cae entre los extremos cuando
#'   `universo` es `"tabla_completa"` o `attr(plan, "muestreo")$estado` es
#'   `"disponible"`; en `"no_sondeado"` la forma sólo se construyó localmente
#'   y el intervalo queda condicionado a que la sonda de la corrida la acepte.
#'   Si la forma muestreada no se puede construir, el plan declara ese caso,
#'   excluye sus métricas del rango y `attr(plan, "supuesto")` dice por qué.
#'
#'   Cuántas consultas se emiten no dice cuánto cuestan: catorce consultas
#'   sobre dos millones de filas son mucho más trabajo que doscientas sobre
#'   mil. Por eso el plan estima además la magnitud, y la estima en sus dos
#'   mitades, porque el reloj de una corrida no lo decide siempre el motor.
#'
#'   La del motor va en `filas_leidas` (cuántas filas habría que leer) y
#'   `ordenaciones_completas` (cuántas veces habría que ordenar la tabla
#'   entera), y se resume en `magnitud_motor`. La del cliente va en
#'   `columnas_texto` y `pares_texto` —cuántos pares de formas podría comparar
#'   en R el detector de vocabulario sobre la muestra— y se resume en
#'   `magnitud_texto`. `magnitud` es la mayor de las dos: `"baja"`, `"media"`,
#'   `"alta"`, o `"desconocida"` si no se conoce el número de filas.
#'   `supuesto_costo` dice de dónde sale cada cuenta.
#'
#'   El plan previo no publica duraciones, CPU, ni filas o bytes medidos, ni agrega
#'   una proyección temporal de `COUNT(DISTINCT)`. Puede publicar filas estimadas
#'   por `pg_class.reltuples` y proyecciones de trabajo de moda/mediana, siempre
#'   rotuladas como estimación de catálogo y no como medición. Aunque el plan de una corrida
#'   conserve el atributo `supuesto_costo_distintos`, la medición y la proyección
#'   sólo aparecen en `resumen_tabla$meta$costo_distintos`, después de ejecutar
#'   el primer lote. El atributo sólo declara por qué esa proyección no existe
#'   antes de correr; no es una duración ni una estimación temporal.
#'
#'   Si se pide `politica_costo = "por_cardinalidad"`, el plan busca primero una
#'   garantía estructural o una fuente de catálogo. Si la fuente queda
#'   desconocida, no emite un agregado para aclararla: `n_consultas` omite la
#'   moda y `n_consultas_max` deja abierto el camino que la ejecuta. La mediana
#'   se conserva porque su costo medido es plano frente a la cardinalidad y está
#'   gobernado por las filas. La corrida mide `distintos` sólo si la política lo
#'   necesita. La política por omisión es `"todas"`: el paquete no elige por el
#'   usuario.
#'   Una fuente estructural se resuelve aunque la estrategia de distintos este
#'   omitida o no disponible; esta ultima solo gobierna si se puede medir.
#'   El catalogo de la clave primaria se consulta siempre para conservar esa
#'   respuesta en `resumen_tabla$meta$clave`; es una lectura de metadatos y no
#'   un recorrido de la tabla.
#'
#'   Contar sólo el motor daba juicios falsos con números ciertos: una tabla de
#'   3.912 filas con una columna de geometría en texto pedía 64.592 lecturas
#'   —magnitud `"baja"`— y tardaba 35 segundos, porque el trabajo estaba en la
#'   comparación de formas, que no es una lectura de fila. El método de
#'   impresión muestra las dos mitades, avisa cuando la magnitud es alta y
#'   nombra las palancas para acotarla, que no son las mismas de un lado que
#'   del otro.
#' @export
#' @seealso [perfilar_dbi()]
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
#'   plan_perfilado_dbi(con, "ejemplo")
#'   DBI::dbDisconnect(con)
#' }
plan_perfilado_dbi <- function(conexion, tabla,
                               universo = c("tabla_completa", "muestra_motor"),
                               muestra_motor = NULL, muestra = Inf,
                               orden_muestra = NULL,
                               metricas = .METRICAS_DBI,
                               estrategia_distintos = "exacta",
                               estrategia_mediana = c("exacta", "aproximada_motor"),
                               politica_costo = c("todas", "por_cardinalidad"),
                               bloque_muestra = c("con_muestra", "solo_agregados"),
                               max_consultas = Inf,
                               dialecto = "auto", incluir_valores = TRUE,
                               tamano_lote = NULL,
                               tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                               tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                               instrumentar = FALSE,
                               umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
                               max_celdas_muestra = .MAX_CELDAS_MUESTRA,
                               max_bytes_muestra = .MAX_BYTES_MUESTRA,
                               bloque_filas = NULL,
                               max_bytes_procesamiento = .MAX_BYTES_MUESTRA,
                               max_bytes_materializacion = .MAX_BYTES_MUESTRA) {
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  max_celdas_muestra <- .validar_limite_duplicados(
    max_celdas_muestra, "max_celdas_muestra"
  )
  max_bytes_muestra <- .validar_limite_duplicados(
    max_bytes_muestra, "max_bytes_muestra"
  )
  max_bytes_procesamiento <- .validar_presupuesto_bytes_dbi(
    max_bytes_procesamiento, "max_bytes_procesamiento"
  )
  max_bytes_materializacion <- .validar_presupuesto_bytes_dbi(
    max_bytes_materializacion, "max_bytes_materializacion"
  )
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, universo = universo,
    muestra_motor = muestra_motor, muestra = muestra,
    orden_muestra = orden_muestra,
    estrategia_mediana = estrategia_mediana, metricas = metricas,
    max_consultas = max_consultas, dialecto = dialecto,
    tamano_lote = tamano_lote,
    tamano_lote_planos = tamano_lote_planos,
    tamano_lote_distintos = tamano_lote_distintos,
    bloque_muestra = bloque_muestra, instrumentar = instrumentar,
    contar = FALSE, contar_muestreo = FALSE,
    sondar_muestreo = FALSE,
    incluir_valores = incluir_valores,
    estrategia_distintos = estrategia_distintos,
    politica_costo = politica_costo,
    umbral_cardinalidad = umbral_cardinalidad,
    max_bytes_procesamiento = max_bytes_procesamiento,
    max_bytes_materializacion = max_bytes_materializacion
  )
  filas_plan <- .filas_plan_dbi(preparacion)
  es_numerico <- vapply(seq_along(preparacion$campos), function(i) {
    .es_numerico_dbi(
      preparacion$prototipo[[i]],
      if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  tope_muestra <- .tope_muestra_plan_dbi(
    preparacion$muestra, length(preparacion$campos),
    max_celdas_muestra, max_bytes_muestra
  )
  consultas_antes_agregados <- preparacion$presupuesto$usadas
  # La corrida real cuenta el universo antes de construir un porcentaje para
  # TABLESAMPLE. El plan no ejecuta ese COUNT(*), pero si debe publicarlo en la
  # cantidad prevista para que su total siga coincidiendo con la corrida.
  muestreo_plan <- identical(preparacion$universo, "muestra_motor") &&
    !is.null(preparacion$muestreo) &&
    !is.null(preparacion$muestreo$candidato)
  emitidas_plan <- consultas_antes_agregados +
    if (isTRUE(muestreo_plan)) {
      sondas_muestreo <- if (is.null(preparacion$muestreo$sondas_previstas)) {
        length(preparacion$muestreo$sondas)
      } else {
        preparacion$muestreo$sondas_previstas
      }
      conteo_muestreo <- identical(
        preparacion$muestreo$candidato$tipo, "tablesample"
      ) || (
        identical(preparacion$muestreo$candidato$tipo, "aleatorio") &&
          any(vapply(
            preparacion$muestreo$candidato$funciones,
            function(funcion) identical(funcion$nombre, "newid"),
            logical(1L)
          ))
      )
      as.integer(sondas_muestreo) + as.integer(conteo_muestreo)
    } else {
      0L
    }
  # El plan no consulta la tabla para despejar la cardinalidad. Un catalogo
  # estructural ya leido puede fijar la decision; lo demas abre un rango entre
  # omitir y ejecutar la moda. La mediana queda fuera de ese criterio y la
  # corrida medira lo que la politica explicita necesite, una sola vez.
  decisiones_costo <- .decisiones_costo_dbi(
    conexion, preparacion$campos,
    list(conteos = stats::setNames(
      vector("list", length(preparacion$campos)), preparacion$campos
    )),
    preparacion$politica_costo, preparacion$n_total, preparacion$universo,
    fuentes_cardinalidad_costo = preparacion$fuentes_cardinalidad_costo
  )
  columnas_moda_plan <- NULL
  columnas_moda_max <- NULL
  columnas_mediana_plan <- NULL
  columnas_mediana_max <- NULL
  if (identical(preparacion$politica_costo$nombre, "por_cardinalidad") &&
      isTRUE(incluir_valores) &&
      .METRICA_CARDINALIDAD_COSTO_DBI %in% preparacion$metricas) {
    desconocidas <- vapply(
      preparacion$fuentes_cardinalidad_costo,
      function(x) identical(x$nombre, "desconocida"), logical(1L)
    )
    if ("moda" %in% preparacion$metricas) {
      columnas_moda_plan <- preparacion$campos[
        vapply(decisiones_costo, function(x) isTRUE(x$moda), logical(1L)) &
          !desconocidas
      ]
      columnas_moda_max <- unique(c(
        columnas_moda_plan, preparacion$campos[desconocidas]
      ))
    } else {
      columnas_moda_plan <- character()
      columnas_moda_max <- character()
    }
    if ("mediana" %in% preparacion$metricas) {
      columnas_mediana_plan <- preparacion$campos[
        vapply(decisiones_costo, function(x) isTRUE(x$mediana), logical(1L)) &
          !desconocidas
      ]
      columnas_mediana_plan <- intersect(
        columnas_mediana_plan, preparacion$campos[preparacion$es_numerico]
      )
      columnas_mediana_max <- unique(c(
        columnas_mediana_plan,
        preparacion$campos[desconocidas & preparacion$es_numerico]
      ))
    } else {
      columnas_mediana_plan <- character()
      columnas_mediana_max <- character()
    }
  }
  plan <- .plan_consultas_dbi(
    preparacion$campos, es_numerico, preparacion$metricas_ejecucion, incluir_valores,
    length(preparacion$orden_sql) > 0 &&
      identical(preparacion$bloque_muestra, "con_muestra"), preparacion$dialecto,
    # Solo se cuentan aqui las consultas de preparacion. No hay un COUNT(*)
    # propio: el total se conocera en la corrida y viajara con el primer
    # agregado plano que pueda llevarlo.
    emitidas = emitidas_plan,
    universo = preparacion$universo,
    muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
      preparacion$muestreo$disponible,
    tamano_lote_planos = preparacion$tamano_lote_planos,
    tamano_lote_distintos = preparacion$tamano_lote_distintos,
    incluir_muestra = identical(preparacion$bloque_muestra, "con_muestra"),
    mediana_consolidada = !is.null(preparacion$mediana_consolidada),
    columnas_distintos = preparacion$columnas_distintos_ejecucion,
    columnas_moda = columnas_moda_plan,
    columnas_moda_max = columnas_moda_max,
    columnas_mediana = columnas_mediana_plan,
    columnas_mediana_max = columnas_mediana_max,
    consultas_muestra = if (identical(
      preparacion$bloque_muestra, "con_muestra"
    ) && isTRUE(tope_muestra$requiere_sonda_bytes)) 2L else 1L
  )
  attr(plan, "total") <- sum(plan$n_consultas)
  extra <- attr(plan, "extra_si_se_rechazan_lotes", exact = TRUE)
  if (is.null(extra)) extra <- 0
  attr(plan, "total_minimo") <- sum(plan$n_consultas)
  attr(plan, "total_maximo") <- sum(plan$n_consultas_max) + extra
  attr(plan, "total_lotes_rechazados") <- attr(plan, "total_maximo")
  attr(plan, "extra_si_se_rechazan_lotes") <- NULL
  muestreo_plan_publico <- .publicar_muestreo_plan_dbi(
    preparacion$muestreo, if (identical(
      preparacion$universo, "muestra_motor"
    )) preparacion$muestra_motor else preparacion$muestra
  )
  motivo_muestreo <- if (identical(
    muestreo_plan_publico$estado, "no_disponible"
  )) {
    paste(
      "La forma muestreada no se puede construir; el plan no incluye sus",
      "metricas SQL. Motivo:", muestreo_plan_publico$motivo
    )
  } else ""
  # El total es un RANGO, no una prediccion exacta ni un techo. Ademas de las
  # sondas por rechazo de lotes, la cardinalidad desconocida deja abiertas las
  # moda que la politica puede omitir despues de medir.
  attr(plan, "supuesto") <- paste(
    "El plan no escanea datos para decidir el costo. Cuando la fuente de",
    "cardinalidad es desconocida, `total` supone que la politica omite la",
    "moda y `total_maximo` que la ejecuta; la mediana se conserva porque su",
    "costo medido es plano frente a la cardinalidad; la corrida mide `distintos`",
    "si la politica lo necesita y sigue esa decision.",
    if ("distintos" %in% preparacion$metricas_ejecucion) {
      .SUPUESTO_COSTO_DISTINTOS_PLAN_DBI
    } else "",
    "Las fuentes estructurales exactas cierran ese intervalo. En cualquiera",
    "de los dos extremos, si el motor rechaza un lote se vuelve a sondear el",
    "arbol de biseccion, hasta 2n - 1 consultas adicionales por lote; las",
    "respuestas aceptadas se reutilizan.", motivo_muestreo
  )
  attr(plan, "columnas") <- length(preparacion$campos)
  attr(plan, "columnas_numericas") <- sum(es_numerico)
  attr(plan, "columnas_ilegibles") <- preparacion$esquema$ilegibles
  attr(plan, "dialecto") <- preparacion$dialecto$nombre
  attr(plan, "consultas_emitidas") <- preparacion$presupuesto$usadas
  attr(plan, "metricas") <- preparacion$metricas
  attr(plan, "metricas_ejecucion") <- preparacion$metricas_ejecucion
  attr(plan, "muestreo") <- muestreo_plan_publico
  if ("distintos" %in% preparacion$metricas_ejecucion) {
    attr(plan, "supuesto_costo_distintos") <-
      .SUPUESTO_COSTO_DISTINTOS_PLAN_DBI
  }
  attr(plan, "politica_costo") <- preparacion$politica_costo
  attr(plan, "estrategia_distintos") <- .publicar_estrategia_distintos_dbi(
    preparacion$estrategia_distintos
  )
  attr(plan, "universo") <- preparacion$universo
  attr(plan, "estrategia_mediana") <- .publicar_estrategia_mediana_dbi(
    preparacion
  )
  attr(plan, "fuente_cardinalidad_costo") <-
    preparacion$fuentes_cardinalidad_costo
  attr(plan, "estimacion_derrame") <- preparacion$estimacion_derrame
  attr(plan, "estimacion_derrame_moda") <- preparacion$estimacion_derrame_moda
  attr(plan, "estimacion_derrame_mediana") <- preparacion$estimacion_derrame_mediana
  attr(plan, "moda_guardian") <- .publicar_moda_guardian_dbi(
    preparacion$moda_guardian_resolucion
  )
  attr(plan, "mediana_consolidada") <- preparacion$mediana_consolidada_resolucion
  attr(plan, "mediana_escalar") <- .publicar_mediana_escalar_dbi(
    preparacion$mediana_escalar_resolucion
  )
  attr(plan, "filas") <- filas_plan$filas
  attr(plan, "filas_fuente") <- filas_plan$fuente
  attr(plan, "estimacion_filas") <- filas_plan
  attr(plan, "proyecciones") <- .proyecciones_plan_catalogo_dbi(
    preparacion, filas_plan, incluir_valores = incluir_valores
  )
  attr(plan, "muestra") <- if (identical(
    preparacion$bloque_muestra, "con_muestra"
  )) preparacion$muestra else NA_real_
  attr(plan, "tamano_lote") <- preparacion$tamano_lote_planos
  attr(plan, "tamano_lote_planos") <- preparacion$tamano_lote_planos
  attr(plan, "tamano_lote_distintos") <- preparacion$tamano_lote_distintos
  attr(plan, "max_celdas_muestra") <- max_celdas_muestra
  attr(plan, "max_bytes_muestra") <- max_bytes_muestra
  attr(plan, "tope_muestra") <- tope_muestra
  if (identical(preparacion$bloque_muestra, "solo_agregados")) {
    attr(plan, "bloque_muestra") <- preparacion$bloque_muestra
  }
  # Que columnas van a pasar por el detector de vocabulario. Se mira el
  # prototipo -lo que devuelve el driver- y no el tipo declarado: es lo que de
  # verdad va a llegar a R.
  es_texto <- if (is.null(preparacion$prototipo)) {
    logical()
  } else {
    vapply(
      preparacion$prototipo,
      function(x) is.character(x) || is.factor(x),
      logical(1L)
    )
  }
  trabajo <- .trabajo_plan_dbi(
    plan, filas_plan$filas, tope_muestra$filas_maximas, sum(es_texto)
  )
  attr(plan, "filas_leidas") <- trabajo$filas_leidas
  attr(plan, "ordenaciones_completas") <- trabajo$ordenaciones
  attr(plan, "columnas_texto") <- trabajo$columnas_texto
  attr(plan, "pares_texto") <- trabajo$pares_texto
  attr(plan, "magnitud_motor") <- trabajo$magnitud_motor
  attr(plan, "magnitud_texto") <- trabajo$magnitud_texto
  attr(plan, "magnitud") <- trabajo$magnitud
  attr(plan, "supuesto_costo") <- .SUPUESTO_TRABAJO_DBI
  attr(plan, "celdas") <- .celdas_plan_dbi(
    filas_plan$filas, length(preparacion$campos)
  )
  attr(plan, "memoria_procesamiento") <- .memoria_procesamiento_plan_dbi(
    filas = attr(plan, "filas", exact = TRUE),
    celdas = attr(plan, "celdas", exact = TRUE),
    pares_texto = attr(plan, "pares_texto", exact = TRUE)
  )
  if (!is.null(bloque_filas)) {
    fuente <- .fuente_bloques_dbi(
      conexion, tabla, tabla_sql = preparacion$tabla_sql,
      campos = preparacion$campos, campos_sql = preparacion$campos_sql,
      prototipo = preparacion$prototipo, tipos = preparacion$tipos,
      orden_sql = preparacion$orden_sql,
      clave = preparacion$catalogo_cardinalidad,
      dialecto = preparacion$dialecto, presupuesto = preparacion$presupuesto,
      bloque_filas = bloque_filas
    )
    plan <- .plan_bloques_fuente_dbi(
      fuente, bloque_filas, preparacion$campos,
      preparacion$metricas_ejecucion, preparacion$bloque_muestra,
      preparacion$muestra
    )
  }
  if (identical(preparacion$universo, "muestra_motor")) {
    spool_plan <- .plan_materializacion_spool_dbi(
      conexion, preparacion, max_bytes_materializacion
    )
    attr(plan, "materializacion") <- spool_plan
    attr(plan, "pasadas") <- spool_plan$pasadas
    attr(plan, "max_bytes_procesamiento") <- max_bytes_procesamiento
    attr(plan, "max_bytes_materializacion") <- max_bytes_materializacion
    attr(plan, "costo_materializacion") <- spool_plan$costo
    costo_plan <- attr(plan, "costo", exact = TRUE)
    if (is.null(costo_plan) || !is.list(costo_plan)) costo_plan <- list()
    costo_plan$materializacion <- spool_plan$costo
    costo_plan$seleccion_unica <- spool_plan$seleccion_unica
    costo_plan$spool <- "una seleccion; lecturas posteriores en cliente"
    attr(plan, "costo") <- costo_plan
  }
  class(plan) <- unique(c("plan_perfilado_dbi", class(plan)))
  plan
}

.miles_dbi <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("sin dato")
  # `decimal.mark` va explicito: por omision es el punto, y compartirlo con el
  # separador de miles hace que R avise en cada llamada.
  format(
    round(x), big.mark = ".", decimal.mark = ",",
    scientific = FALSE, trim = TRUE
  )
}

#' @export
print.plan_perfilado_dbi <- function(x, ...) {
  # Subconjuntar un plan -`plan[, c("clase_consulta", "n_consultas")]`- conserva
  # la clase y pierde los atributos, y este metodo imprimia entonces "sin dato
  # consultas sobre sin dato filas". Un encabezado que no puede decir nada no es
  # un plan: se imprime la tabla y se dice por que falta el resto.
  total <- attr(x, "total", exact = TRUE)
  if (is.null(total)) {
    cli::cli_alert_warning(paste(
      "Este objeto conserva la clase del plan pero no sus atributos;",
      "seguramente sea un subconjunto. Se imprime solo la tabla."
    ))
    print.data.frame(x, ...)
    return(invisible(x))
  }
  cli::cli_h1("Plan de perfilado")
  filas <- attr(x, "filas", exact = TRUE)
  filas_fuente <- attr(x, "filas_fuente", exact = TRUE)
  texto_filas <- .texto_filas_plan_dbi(filas, filas_fuente)
  techo <- attr(x, "total_lotes_rechazados", exact = TRUE)
  # El rango se imprime como rango. Decir "techo" era exactamente lo que el
  # atributo `supuesto` desmiente dos lineas mas abajo.
  cuenta <- if (!is.null(techo) && !is.na(techo) && techo > total) {
    paste0("entre ", .miles_dbi(total), " y ", .miles_dbi(techo), " consultas")
  } else {
    paste0(.miles_dbi(total), " consultas")
  }
  cli::cli_alert_info(paste0(
    cuenta, " sobre ",
    texto_filas, " y ",
    .miles_dbi(attr(x, "columnas", exact = TRUE)), " columnas (dialecto ",
    attr(x, "dialecto", exact = TRUE), ")"
  ))
  spool_plan <- attr(x, "materializacion", exact = TRUE)
  if (is.list(spool_plan)) {
    cli::cli_text(
      "Muestra: una seleccion materializada en spool externo de sesion cliente;",
      " resultsets = ", spool_plan$costo$resultsets,
      ", presupuesto = ", .miles_dbi(spool_plan$presupuesto), " bytes."
    )
    cli::cli_text(
      "Spool pagado: ", isTRUE(spool_plan$pagado),
      "; los bloques y pasadas leen el mismo `muestra_id` cuando se materializa."
    )
  }
  fuente_plan <- attr(x, "fuente", exact = TRUE)
  if (is.list(fuente_plan) && !is.null(fuente_plan$metodo_orden)) {
    estado_capacidad <- if (isTRUE(fuente_plan$disponible)) {
      "disponible"
    } else {
      paste0("no disponible (", fuente_plan$motivo %||% "sin motivo", ")")
    }
    cli::cli_text(
      "Fuente: un result set incremental (dbSendQuery + dbFetch); capacidad: ",
      estado_capacidad, "."
    )
    cli::cli_text(
      "Orden: ", fuente_plan$metodo_orden,
      "; estable = ", isTRUE(fuente_plan$estable),
      "; orden_id = ", fuente_plan$orden_id, "."
    )
    bloques_plan <- attr(x, "bloques", exact = TRUE)
    if (is.list(bloques_plan)) {
      cli::cli_text(
        "Bloques previstos: objetivo ", .miles_dbi(bloques_plan$objetivo),
        " filas; minimo ", .miles_dbi(bloques_plan$minimo),
        "; maximo ", .miles_dbi(bloques_plan$maximo), "."
      )
    }
    pasadas_plan <- attr(x, "pasadas", exact = TRUE)
    if (is.list(pasadas_plan)) {
      cli::cli_text(
        "Pasadas: valor = ", pasadas_plan$valor,
        "; indice = ", pasadas_plan$indice,
        "; materializacion = ", pasadas_plan$materializacion, "."
      )
    }
  }
  proyecciones <- attr(x, "proyecciones", exact = TRUE)
  if (is.list(proyecciones)) {
    if (isTRUE(proyecciones$moda$disponible)) {
      cli::cli_text(
        "Proyecci\u00f3n de moda: ~",
        .miles_dbi(proyecciones$moda$magnitud),
        " distintos acumulados en ", proyecciones$moda$n_columnas,
        " columna(s); estimaci\u00f3n de cat\u00e1logo, no duraci\u00f3n. Fuente: ",
        proyecciones$moda$fuente, "."
      )
    }
    if (isTRUE(proyecciones$mediana$disponible)) {
      cli::cli_text(
        "Proyecci\u00f3n de mediana: ~",
        .miles_dbi(proyecciones$mediana$magnitud),
        " filas-mediana; estimaci\u00f3n de cat\u00e1logo, no duraci\u00f3n. Fuente: ",
        proyecciones$mediana$fuente, "."
      )
    }
  }
  for (familia in c("moda", "mediana")) {
    estimacion <- attr(x, paste0("estimacion_derrame_", familia), exact = TRUE)
    if (is.null(estimacion) || identical(estimacion$estado, "no_solicitado")) {
      next
    }
    limite <- if (identical(estimacion$metodo, "sort") ||
                  identical(familia, "mediana")) {
      estimacion$work_mem
    } else estimacion$memoria_efectiva
    cli::cli_text(
      "Derrame estimado de ", familia, ": ", estimacion$estado,
      "; metodo/forma = ", estimacion$metodo, "/", estimacion$forma,
      "; limite de decision = ", limite, "; motivo: ", estimacion$motivo
    )
  }
  if (identical(attr(x, "bloque_muestra", exact = TRUE), "solo_agregados")) {
    cli::cli_text(
      "Perfil de muestra: no solicitado; el plan incluye solo agregados SQL."
    )
  } else {
    tope <- attr(x, "tope_muestra", exact = TRUE)
    if (!is.null(tope)) {
      if (isTRUE(tope$recortada_por_celdas)) {
        cli::cli_text(
          "Perfil de muestra: el tope de celdas lo limita a como m\u00e1ximo ",
          .miles_dbi(tope$filas_maximas), " filas antes de leer."
        )
      } else {
        cli::cli_text(
          "Perfil de muestra: se solicitan ",
          .miles_dbi(tope$filas_solicitadas),
          " filas; el tope de celdas no reduce ese pedido conocido."
        )
      }
      if (isTRUE(tope$requiere_sonda_bytes)) {
        cli::cli_text(
          "El tope de bytes requiere una sonda previa de hasta 100 filas;",
          " el l\u00edmite final se fija despu\u00e9s de medirla."
        )
      }
    }
  }
  memoria <- attr(x, "memoria_procesamiento", exact = TRUE)
  if (!is.null(memoria)) {
    cli::cli_h2("Memoria del procesamiento")
    cli::cli_text(
      "Memoria del procesamiento: no estimada. Motivo: ", memoria$motivo
    )
    magnitud_memoria <- memoria$magnitud
    cli::cli_text(
      "Magnitud del trabajo (no consumo de memoria): ",
      .miles_dbi(magnitud_memoria$filas), " filas; ",
      .miles_dbi(magnitud_memoria$celdas), " celdas; ",
      .miles_dbi(magnitud_memoria$pares_texto), " pares de texto."
    )
    cli::cli_text(
      "Datos de referencia medidos, no predicci\u00f3n: traer la tabla cost\u00f3 ",
      memoria$referencias$traer, "; procesar en R cost\u00f3 ",
      memoria$referencias$procesar, "."
    )
    cli::cli_text(
      "La referencia de procesamiento vari\u00f3 por ",
      memoria$referencias$variacion,
      "; esa variaci\u00f3n es justamente el motivo por el que no se estima."
    )
    cli::cli_text(memoria$distincion)
    cli::cli_text(memoria$reparto_observado)
  }
  magnitud <- attr(x, "magnitud", exact = TRUE)
  if (is.null(magnitud)) magnitud <- "desconocida"
  if (identical(magnitud, "desconocida")) {
    cli::cli_alert_warning(paste(
      "No se pudo estimar el trabajo: falta el n\u00famero de filas.",
      "El conteo de consultas sigue siendo v\u00e1lido."
    ))
    # Los supuestos NO se imprimen aca: el bloque final ya los imprime para
    # toda magnitud distinta de "baja", y "desconocida" lo es. Este sitio los
    # duplicaba: el plan mostraba dos veces los mismos dos parrafos, y un texto
    # repetido se lee como un error del que lo escribio, no como enfasis.
  } else {
    trabajo <- paste0(
      .miles_dbi(attr(x, "filas_leidas", exact = TRUE)), " lecturas de fila y ",
      .miles_dbi(attr(x, "ordenaciones_completas", exact = TRUE)),
      " ordenaciones completas"
    )
    # El trabajo en R no se puede dejar afuera del titular. Cuando la mitad que
    # decide el reloj es esta, un "bajo" a secas manda al usuario a esperar
    # treinta segundos creyendo que iban a ser dos.
    n_texto <- attr(x, "columnas_texto", exact = TRUE)
    if (!is.null(n_texto) && !is.na(n_texto) && n_texto > 0) {
      trabajo <- paste0(
        trabajo, " en el motor, m\u00e1s ",
        .miles_dbi(attr(x, "pares_texto", exact = TRUE)),
        " pares de formas a comparar en R sobre ", .miles_dbi(n_texto),
        if (n_texto == 1) " columna de texto" else " columnas de texto"
      )
    }
    # Una magnitud alta o media no es un error: es una corrida que conviene
    # decidir a ojos abiertos. Por eso el aviso nombra las palancas concretas en
    # vez de limitarse a decir que es grande.
    #
    # Las nombra tambien en "media", y eso salio de una corrida contra motores
    # reales: una tabla de 4,5 millones de filas en PostgreSQL tardo 6,2 minutos
    # con las opciones por omision y su plan la clasificaba **media**. El aviso
    # avisaba, pero quien no conociera `universo = 'muestra_motor'` -que baja esa misma
    # tabla a 39 segundos- no tenia como enterarse. Un plan que dice "va a
    # costar" sin decir "y asi se acota" deja al usuario a mitad de camino
    # justo donde la decision importa.
    if (identical(magnitud, "alta") || identical(magnitud, "media")) {
      if (identical(magnitud, "alta")) {
        cli::cli_alert_danger(paste0("Trabajo estimado alto: ", trabajo))
      } else {
        cli::cli_alert_warning(paste0("Trabajo estimado medio: ", trabajo))
      }
      cli::cli_text("Para acotarlo, sin cambiar nada m\u00e1s:")
      palancas <- c(
        "universo = 'muestra_motor' mide sobre una muestra que trae el motor",
        "metricas = c(...) saca clases de consulta enteras del plan",
        "muestra = n baja las filas que se traen a R",
        "max_consultas = n pone un techo duro y declara lo que quede afuera"
      )
      # Si lo que pesa es el trabajo por valor, las palancas del motor no
      # alcanzan: bajar consultas no toca lo que se hace despues en R.
      if (identical(attr(x, "magnitud_texto", exact = TRUE), "alta")) {
        palancas <- c(
          palancas,
          paste(
            "max_trabajo_vocabulario = n acota la comparaci\u00f3n de formas,",
            "que es lo que pesa en R"
          )
        )
      }
      cli::cli_ul(palancas)
    } else {
      cli::cli_alert_success(paste0("Trabajo estimado bajo: ", trabajo))
    }
  }
  supuesto_distintos <- attr(x, "supuesto_costo_distintos", exact = TRUE)
  if (!is.null(supuesto_distintos)) {
    cli::cli_text(supuesto_distintos)
  }
  # `muestra = Inf` -lo que viene por omision- trae la tabla entera a R. Es lo
  # correcto para un analisis de calidad: los diagnosticos que miran los valores
  # -patrones, formatos, dependencias y casi-duplicados- solo ven lo que se les trae, y sin
  # `orden_muestra` una muestra acotada son las PRIMERAS filas del motor, no una
  # al azar. Pero conviene decirlo antes y no despues, porque sobre una tabla
  # grande es lo que manda el reloj.
  muestra_plan <- attr(x, "muestra", exact = TRUE)
  if (!identical(attr(x, "bloque_muestra", exact = TRUE), "solo_agregados") &&
      !is.null(muestra_plan) && !is.finite(muestra_plan) &&
      (is.null(fuente_plan) || isTRUE(fuente_plan$disponible))) {
    filas_plan <- attr(x, "filas", exact = TRUE)
    cli::cli_text(
      "El perfil de muestra trae la tabla entera",
      if (!is.null(filas_plan) && !is.na(filas_plan)) {
        paste0(" -", .texto_filas_plan_dbi(filas_plan, filas_fuente), "-")
      } else "",
      ": es el valor por omisi\u00f3n y sobre una tabla grande puede demorar. ",
      "`muestra = n` acota cuantas filas se traen, a cambio de mirar menos."
    )
  }
  # Los supuestos son largos y solo pesan cuando el numero incomoda. Sobre una
  # tabla chica el usuario ya tiene su respuesta en la linea de arriba, y dos
  # parrafos de letra fina la tapan. No se ocultan: siguen en los atributos, y
  # la palabra "techo" viaja con el conteo en todos los casos.
  if (!identical(magnitud, "baja")) {
    supuesto <- attr(x, "supuesto_costo", exact = TRUE)
    if (!is.null(supuesto)) cli::cli_text(supuesto)
    techo <- attr(x, "supuesto", exact = TRUE)
    if (!is.null(techo)) cli::cli_text(techo)
  } else {
    cli::cli_text(
      "Los supuestos de la cuenta est\u00e1n en los atributos ",
      "`supuesto_costo` y `supuesto`."
    )
  }
  print.data.frame(as.data.frame(x), row.names = FALSE)
  invisible(x)
}

# ---- Proteccion de datos personales -------------------------------------
#
# El resumen de tabla completa es el bloque de mayor alcance -cubre toda la
# base, no una muestra- y era el unico que nunca pasaba por la proteccion. Se
# protegen los valores de la fila, los estadisticos de orden, los momentos y
# el SQL guardado.

.proteger_informacion_conexion_dbi <- function(info) {
  sensibles <- c("dbname", "host", "username", "servername", "sourcename")
  presentes <- intersect(sensibles, names(info$informacion_dbi))
  for (campo in presentes) {
    info$informacion_dbi[[campo]] <- "[dato de conexion protegido]"
  }
  if (length(presentes)) {
    info$informacion_dbi$proteccion_aplicada <- TRUE
  }
  info
}

.proteger_resumen_dbi <- function(resumen, sensibles, base_clasificacion) {
  resumen$meta$proteccion_personal <- list(
    aplicada = length(sensibles) > 0,
    base = base_clasificacion,
    columnas = sensibles
  )
  resumen$meta$motor <- .proteger_informacion_conexion_dbi(resumen$meta$motor)
  if (!length(sensibles)) return(resumen)
  reemplazo <- "[valor protegido]"
  columnas <- resumen$columnas
  indices <- columnas$columna %in% sensibles
  if (!any(indices)) return(resumen)
  columnas$moda[indices & !is.na(columnas$moda)] <- reemplazo
  # Los estadisticos de orden son valores reales de una celda. Los momentos
  # tambien identifican cuando la columna es un documento: la media de las
  # cedulas de una tabla chica reconstruye demasiado.
  campos <- intersect(
    c("minimo", "maximo", "mediana", "media"), names(columnas)
  )
  oculto <- rep(FALSE, nrow(columnas))
  for (campo in campos) {
    tapar <- indices & !is.na(columnas[[campo]])
    oculto <- oculto | tapar
    columnas[[campo]][tapar] <- NA_real_
  }
  if (any(oculto)) {
    if (!"detalle_proteccion_personal" %in% names(columnas)) {
      columnas$detalle_proteccion_personal <- NA_character_
    }
    columnas$detalle_proteccion_personal[oculto] <-
      "[estadisticos de orden y momentos protegidos]"
  }
  resumen$columnas <- columnas
  literales <- resumen$literales
  if (length(literales)) {
    filas_sensibles <- resumen$sql$columna %in% sensibles
    for (i in which(filas_sensibles)) {
      texto <- resumen$sql$sql[[i]]
      if (is.na(texto)) next
      for (literal in literales) {
        texto <- gsub(literal, "[literal protegido]", texto, fixed = TRUE)
      }
      resumen$sql$sql[[i]] <- texto
    }
  }
  resumen
}

# ---- Bloque de la muestra ------------------------------------------------

# Un tipo que el controlador no puede traer en una lectura corriente: `TEXT`,
# `NTEXT`, `IMAGE`, los `MAX` de SQL Server, `CLOB`, `BLOB`, `BYTEA`. El caso
# que lo destapo es real: en una tabla de 158 columnas, 90 son de estos tipos, y
# el driver ODBC responde `07009` al leerlas.
# Tipos que muchos controladores no saben traer en una lectura corriente: hay
# que pedirlos aparte, o convertirlos, o no pedirlos. El nombre viene del motor
# ya sin espacios (ver mas abajo), asi que "nvarchar (max)" llega pegado.
#
# Este patron es un ATAJO, no la comprobacion. Reconocer el tipo ahorra el
# descarte, pero no reconocerlo no puede costar la muestra: quien decide es
# `.aislar_ilegibles_dbi()`, que le pregunta al motor.
.PATRON_TIPO_LARGO_DBI <- paste0(
  "^(text|ntext|image|clob|nclob|blob|bytea|longtext|mediumtext|tinytext|",
  "longblob|mediumblob|tinyblob|xml|xmltype|json|jsonb|geometry|geography|",
  "long|longraw|bfile|n?(var)?(char|binary)\\(max\\)|",
  "(sql_?)?w?longvar(char|binary))$"
)

# Un controlador no tiene por que informar el nombre del tipo. `odbc` resuelve
# `dbColumnInfo()` con `nanodbc::result::column_datatype()`, que devuelve el
# codigo numerico de ODBC: contra el driver `{SQL Server}` una tabla de noventa
# columnas `varchar(max)` llega como noventa veces "-1". Un patron de nombres
# reconoce cero de noventa, y esa es exactamente la tabla que motivo el
# reintento.
#   -1  SQL_LONGVARCHAR    -4  SQL_LONGVARBINARY    -10  SQL_WLONGVARCHAR
.CODIGOS_TIPO_LARGO_DBI <- c("-1", "-4", "-10")

.columnas_de_tipo_largo_dbi <- function(tipos) {
  if (is.null(tipos) || !length(tipos)) return(integer())
  limpio <- tolower(trimws(as.character(tipos)))
  limpio <- gsub("[[:space:]]+", "", limpio)
  which(!is.na(limpio) &
          (limpio %in% .CODIGOS_TIPO_LARGO_DBI |
             grepl(.PATRON_TIPO_LARGO_DBI, limpio, perl = TRUE)))
}

# Tope de sondas del descarte. Aislar por biseccion cuesta a lo sumo 2n-1
# sondas sobre n columnas -es el tamano del arbol cuando todas las hojas son
# culpables-, asi que 2n es "lo que alcanza en el peor caso" y no un numero
# elegido a dedo. El tope absoluto acota el costo cuando la tabla es enorme; si
# se agota, el aislamiento queda parcial y se declara.
.TOPE_SONDAS_DESCARTE_DBI <- 512L

# Averigua CUALES columnas impiden leer, en vez de adivinarlo por el nombre del
# tipo. Divide el conjunto en dos y baja solo por las mitades que siguen
# fallando: los subconjuntos que se leen bien se podan enteros.
#
# `sondear(indices)` devuelve TRUE si el motor entrega una fila con esas
# columnas. `hay_saldo()` se consulta ANTES de cada sonda: sin eso una sonda
# rechazada por presupuesto se leeria como "esta columna no se puede leer", que
# es una causa que nadie midio.
#
# Devuelve las columnas que fallan por si solas. Con
# `conservar_legibles = TRUE` tambien devuelve los grupos aceptados y las hojas
# que quedaron pendientes, para que un llamador que ya midio una sonda pueda
# reutilizarla sin volver a consultar. El camino de la muestra usa el valor por
# omision y conserva exactamente su salida y su presupuesto.
.aislar_ilegibles_dbi <- function(sondear, hay_saldo, n, tope,
                                  conservar_legibles = FALSE) {
  culpables <- integer()
  gastadas <- 0L
  agotado <- FALSE
  pendientes <- list(seq_len(n))
  legibles <- list()
  while (length(pendientes)) {
    if (gastadas >= tope || !isTRUE(hay_saldo())) {
      agotado <- TRUE
      break
    }
    grupo <- pendientes[[1L]]
    pendientes <- pendientes[-1L]
    gastadas <- gastadas + 1L
    if (isTRUE(sondear(grupo))) {
      if (isTRUE(conservar_legibles)) legibles[[length(legibles) + 1L]] <- grupo
      next
    }
    if (length(grupo) == 1L) {
      culpables <- c(culpables, grupo)
      next
    }
    corte <- length(grupo) %/% 2L
    pendientes <- c(
      list(grupo[seq_len(corte)]),
      list(grupo[(corte + 1L):length(grupo)]),
      pendientes
    )
  }
  salida <- list(
    culpables = sort(culpables), sondas = gastadas, agotado = agotado
  )
  if (isTRUE(conservar_legibles)) {
    salida$legibles <- legibles
    salida$pendientes <- pendientes
  }
  salida
}

# Dos omisiones distintas no pueden contarse igual. El descarte COMPROBO que
# esas columnas no se leen: cada una fallo sola y el resto se leyo junto. El
# atajo por tipo solo las SUPUSO, porque el reintento se dispara ante cualquier
# fallo habiendo columnas de tipo largo declaradas, y un corte de red que se
# recupera en el segundo intento produciria el mismo camino. El texto lo dice
# distinto segun cual de los dos fue, y el motivo textual del motor viaja
# entero en los dos para que quien lea decida.
.motivo_omision_muestra_dbi <- function(tipo_omision, columnas, sondas, motivo_motor) {
  n <- length(columnas)
  lista <- paste(columnas, collapse = ", ")
  motor <- if (length(motivo_motor) != 1L || is.na(motivo_motor) ||
               !nzchar(as.character(motivo_motor))) {
    " El motor no informo un motivo."
  } else {
    paste0(" El motor dijo: ", as.character(motivo_motor), ".")
  }
  if (identical(tipo_omision, "descarte")) {
    paste0(
      "La lectura completa de la muestra fallo. Se aislo por descarte, con ",
      sondas, if (sondas == 1L) " sonda" else " sondas", " al motor, ",
      if (n == 1L) {
        "la columna que no se puede leer: "
      } else {
        paste0("las ", n, " columnas que no se pueden leer: ")
      },
      lista,
      ". Cada una fallo por si sola y el resto se leyo junto.", motor
    )
  } else {
    paste0(
      "La lectura completa de la muestra fallo y se reintento sin ",
      if (n == 1L) {
        "la columna de tipo largo declarada: "
      } else {
        paste0("las ", n, " columnas de tipo largo declaradas: ")
      },
      lista,
      ". No se comprobo que sean la causa;", motor
    )
  }
}

# `constante` afirma que la columna tiene un unico valor no ausente. Sobre una
# muestra esa afirmacion no se puede sostener: basta una fila no leida para
# desmentirla, y una tabla de 200 filas con tres valores donde 50 filas traen
# uno solo produce exactamente ese hallazgo. Es el caso que separa una
# proporcion de una universal: `prop_faltantes` estimada sobre la muestra sigue
# siendo una estimacion honesta de la tabla, pero "un unico valor" es una
# cuantificacion universal y la muestra no la alcanza.
#
# El hallazgo no se apaga: se traslada a `cobertura_diagnosticos`, con el
# tamano de la muestra y el de la tabla, para que quede escrito que se dejo de
# decir y por que.
.constante_no_medible_en_muestra_dbi <- function(perfil, muestreo) {
  if (is.null(perfil) || isTRUE(muestreo$tabla_completa)) return(perfil)
  hallazgos <- perfil$hallazgos
  if (is.null(hallazgos) || !nrow(hallazgos)) return(perfil)
  sobra <- as.character(hallazgos$tipo_hallazgo) == "constante"
  if (!any(sobra)) return(perfil)
  filas <- .entero_sql_dbi(muestreo$filas_obtenidas)
  total <- .entero_sql_dbi(muestreo$filas_totales_fuente)
  nuevas <- do.call(rbind, lapply(which(sobra), function(i) {
    .nuevo_diagnostico_no_evaluado(
      "constante", as.character(hallazgos$columna[[i]]),
      paste0(
        "No se evaluo si la columna es constante: el perfil describe ", filas,
        " filas de las ", total, " de la tabla. En la muestra se observo un ",
        "unico valor, y eso no prueba que sea el unico de la tabla."
      ),
      paste(
        "Perfilar la tabla completa, o contar los valores distintos con",
        "`resumen_tabla_dbi()`, que los mide sobre todas las filas."
      )
    )
  }))
  perfil$hallazgos <- hallazgos[!sobra, , drop = FALSE]
  rownames(perfil$hallazgos) <- NULL
  cobertura <- perfil$cobertura_diagnosticos
  perfil$cobertura_diagnosticos <- if (is.null(cobertura) || !nrow(cobertura)) {
    nuevas
  } else {
    rbind(cobertura, nuevas)
  }
  rownames(perfil$cobertura_diagnosticos) <- NULL
  perfil
}

.bloque_muestra_dbi <- function(conexion, tabla, tabla_sql, campos, campos_sql,
                                muestra, muestra_motor, orden_muestra, orden_sql, dialecto,
                                n_total, presupuesto, info_conexion,
                                argumentos, muestreo = NULL,
                                tipos_declarados = NULL,
                                trazador = NULL,
                                max_celdas_muestra = .MAX_CELDAS_MUESTRA,
                                max_bytes_muestra = .MAX_BYTES_MUESTRA) {
  cobertura <- .cobertura_dbi_vacia()
  # En Oracle la cadena vacia **es** NULL: no hay forma de distinguirlas, ni
  # desde SQL ni desde el controlador. Eso cambia una medida que el paquete
  # publica: los mismos tres valores `("", NA, "x")` dan dos faltantes por
  # Oracle y uno por SQLite. La completitud de la misma columna sale distinta
  # segun el motor, y no porque el dato cambie.
  #
  # No es un defecto que se pueda arreglar -es la semantica del motor- pero
  # callarlo si lo seria: quien compare completitud entre entregas de motores
  # distintos estaria leyendo una diferencia que no esta en los datos.
  if (identical(.via_clave_primaria(conexion), "all_constraints")) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "faltantes", NA_character_, "advertido",
      paste(
        "En este motor la cadena vacia y el nulo son el mismo valor, asi que",
        "los faltantes informados incluyen las cadenas vacias y no se pueden",
        "separar. Medido: las mismas tres filas dan dos faltantes aqui y uno",
        "en un motor que las distingue."
      ),
      paste(
        "Al comparar completitud entre motores, tener presente que la",
        "diferencia puede venir de esta semantica y no del dato. Si importa",
        "distinguirlas, hay que marcarlas con un valor propio antes de cargar."
      )
    ))
  }
  verificacion <- if (length(orden_sql)) {
    .verificar_orden_dbi(conexion, tabla_sql, orden_sql, dialecto, presupuesto)
  } else {
    list(
      unico = FALSE, sql = NA_character_,
      motivo = "No se declaro `orden_muestra`; SQL no garantiza el orden de las filas."
    )
  }
  total_numero <- .numero_dbi(n_total)
  total_conocido <- length(total_numero) == 1L &&
    !is.na(total_numero) && is.finite(total_numero)
  alcance <- .tope_muestra_dbi(
    muestra, n_total, length(campos_sql),
    max_celdas_muestra, max_bytes_muestra
  )
  n_obtener <- alcance$filas_efectivas
  usa_muestreo <- !is.null(muestreo) && isTRUE(muestreo$disponible)
  # La receta de la lectura estaba escrita una sola vez y el reintento la
  # rehacia a mano, asi que perdia por el camino el muestreo del motor: volvia a
  # una lectura de primeras filas mientras `metodo` seguia declarando
  # `TABLESAMPLE`. Ahora la arma la misma funcion para cualquier subconjunto de
  # columnas, y lo que se declara sale de lo que se emitio.
  armar_muestra_dbi <- function(indices, filas_solicitadas = n_obtener) {
    sub_sql <- campos_sql[indices]
    origen <- if (usa_muestreo) {
      .fuente_muestreada_dbi(
        tabla_sql, sub_sql, muestra_motor, n_total, dialecto,
        list(candidato = muestreo$candidato)
      )
    } else {
      NULL
    }
    base <- if (!is.null(origen)) {
      origen$sql
    } else {
      paste0(
        "SELECT ", paste(sub_sql, collapse = ", "), " FROM ", tabla_sql,
        if (length(orden_sql)) {
          paste0(" ORDER BY ", paste(orden_sql, collapse = ", "))
        } else ""
      )
    }
    recorte <- if (!is.null(origen) &&
                   filas_solicitadas < origen$filas_solicitadas) {
      dialecto$limitar(base, filas_solicitadas, 0)
    } else if (!is.null(origen)) {
      NULL
    } else if (!total_conocido || filas_solicitadas < total_numero) {
      dialecto$limitar(base, filas_solicitadas, 0)
    } else {
      NULL
    }
    list(
      fuente = origen,
      sql = if (is.null(recorte)) base else recorte,
      filas = if (!is.null(origen)) {
        origen$filas
      } else if (is.null(recorte) && (!total_conocido || filas_solicitadas < total_numero)) {
        filas_solicitadas
      } else {
        -1L
      },
      acotado_en = if (!is.null(origen)) {
        "motor_muestreo"
      } else if (!is.null(recorte)) {
        "motor"
      } else if (!total_conocido || filas_solicitadas < total_numero) {
        "cliente"
      } else {
        "sin recorte"
      },
      metodo = if (!is.null(origen)) {
        origen$metodo
      } else if (length(orden_sql)) {
        "primeras_filas_segun_orden"
      } else {
        "primeras_filas_sin_orden_garantizado"
      }
    )
  }
  # Una sonda pregunta si el motor entrega UNA fila con este subconjunto. Va sin
  # `ORDER BY` y acotada a una fila porque lo unico que interesa es si la
  # lectura sale, no que devuelva.
  sondear_muestra_dbi <- function(indices) {
    if (!length(indices)) return(TRUE)
    base <- paste0(
      "SELECT ", paste(campos_sql[indices], collapse = ", "),
      " FROM ", tabla_sql
    )
    recorte <- dialecto$limitar(base, 1, 0)
    respuesta <- .consultar_dbi(
      conexion, if (is.null(recorte)) base else recorte, presupuesto,
      filas = if (is.null(recorte)) 1L else -1L,
      etapa = "sonda_lectura_muestra"
    )
    isTRUE(respuesta$ok)
  }
  hay_saldo_dbi <- function() {
    is.null(presupuesto) || .saldo_dbi(presupuesto) >= 1
  }
  lectura_inicio <- if (isTRUE(presupuesto$instrumentar)) .ahora_dbi() else NULL
  lectura_cpu_inicio <- if (isTRUE(presupuesto$instrumentar)) {
    .ahora_cpu_dbi()
  } else NULL
  if (is.finite(max_bytes_muestra)) {
    filas_sonda <- min(n_obtener, 100)
    sonda <- armar_muestra_dbi(seq_along(campos_sql), filas_sonda)
    consulta_sonda <- .consultar_dbi(
      conexion, sonda$sql, presupuesto, filas = sonda$filas,
      etapa = "sonda_bytes_muestra"
    )
    if (isTRUE(consulta_sonda$ok)) {
      alcance$bytes_sonda <- as.numeric(utils::object.size(consulta_sonda$datos))
      filas_sonda_obtenidas <- nrow(consulta_sonda$datos)
      if (filas_sonda_obtenidas > 0) {
        bytes_vacios <- as.numeric(utils::object.size(
          consulta_sonda$datos[0, , drop = FALSE]
        ))
        if (bytes_vacios > max_bytes_muestra) {
          .detener_dbi(
            "lupa_error_argumento_dbi",
            paste0(
              "`max_bytes_muestra` es menor que el tamano minimo de la",
              " muestra vacia (", bytes_vacios, " bytes)."
            )
          )
        }
        bytes_por_fila <- max(
          (alcance$bytes_sonda - bytes_vacios) / filas_sonda_obtenidas,
          alcance$bytes_sonda / filas_sonda_obtenidas, 1
        )
        alcance$filas_por_bytes <- floor(
          max(0, as.numeric(max_bytes_muestra) - bytes_vacios) /
            bytes_por_fila
        )
        if (alcance$filas_por_bytes < 1) {
          .detener_dbi(
            "lupa_error_argumento_dbi",
            "`max_bytes_muestra` no permite materializar una fila de la muestra."
          )
        }
        alcance$filas_efectivas <- min(
          alcance$filas_efectivas, alcance$filas_por_bytes
        )
        alcance$celdas_efectivas <-
          alcance$filas_efectivas * length(campos_sql)
      }
    }
  }
  n_obtener <- alcance$filas_efectivas
  # La consulta final siempre se arma con el límite resuelto. La sonda es sólo
  # una medida previa para decidir ese límite; nunca se recorta en R lo que ya
  # vino del motor.
  armado <- armar_muestra_dbi(seq_along(campos_sql), alcance$filas_efectivas)
  fuente <- armado$fuente
  sql_muestra <- armado$sql
  filas <- armado$filas
  acotado_en <- armado$acotado_en
  muestreo_meta <- list(
    filas_solicitadas = as.numeric(muestra),
    filas_solicitadas_sin_topes = alcance$filas_solicitadas,
    filas_maximas_por_celdas = alcance$filas_por_celdas,
    filas_maximas_por_bytes = alcance$filas_por_bytes,
    max_celdas_muestra = max_celdas_muestra,
    max_bytes_muestra = max_bytes_muestra,
    celdas_solicitadas = alcance$celdas_solicitadas,
    celdas_efectivas = alcance$celdas_efectivas,
    bytes_sonda = alcance$bytes_sonda,
    bytes_muestra = alcance$bytes_muestra,
    filas_obtenidas = NA_real_,
    filas_totales_fuente = n_total,
    tabla_completa = FALSE,
    metodo = if (!is.null(fuente)) {
      fuente$metodo
    } else if (length(orden_sql)) {
      "primeras_filas_segun_orden"
    } else {
      "primeras_filas_sin_orden_garantizado"
    },
    acotado_en = acotado_en,
    dialecto = dialecto$nombre,
    columnas_leidas = campos,
    orden_muestra = orden_muestra,
    orden_unico_verificado = isTRUE(verificacion$unico),
    reproducible = isTRUE(verificacion$unico),
    motivo_reproducibilidad = verificacion$motivo,
    sql_verificacion_orden = verificacion$sql,
    sql_muestra = sql_muestra
  )
  if (!is.null(fuente)) {
    muestreo_meta$fraccion <- fuente$fraccion
    muestreo_meta$tamano_muestra <- fuente$filas_solicitadas
    muestreo_meta$universo <- n_total
    muestreo_meta$capacidad <- muestreo$candidato$nombre
  }
  consulta <- .consultar_dbi(
    conexion, sql_muestra, presupuesto, filas = filas,
    etapa = "lectura_muestra"
  )
  campos_omitidos <- character()
  if (!consulta$ok) {
    # Una sola columna que el controlador no sabe traer se llevaba puesta la
    # muestra entera: el usuario perdia el perfil por fila de las otras ciento
    # cincuenta. Es el reflejo de todo-o-nada otra vez, y la salida es la misma
    # que en los demas lugares: reintentar sin lo que no se puede leer y
    # declarar que quedo afuera.
    #
    # El reintento no castea. Castear exige una sintaxis por motor y una
    # decision sobre cuanto truncar, y las dos cosas son adivinar; dejar la
    # columna afuera y decirlo no supone nada.
    #
    # Dos caminos, y el orden importa. El primero es el atajo por tipo: sale
    # gratis cuando el motor informa nombres que se reconocen. El segundo es el
    # descarte, que le PREGUNTA al motor cuales columnas no puede traer en vez
    # de deducirlo del nombre del tipo. La version anterior tenia solo el
    # atajo, y contra el driver `{SQL Server}` -que informa el tipo como codigo
    # ODBC, "-1" y no "varchar(max)"- reconocia cero de noventa columnas: el
    # reintento no se disparaba nunca y la muestra se perdia igual, en
    # silencio. Cualquier patron de tipos va a ir siempre atras del zoo de
    # controladores; la correccion no puede colgar de el.
    motivo_original <- consulta$motivo
    recuperado <- NULL
    tipo_omision <- NA_character_
    sondas_descarte <- 0L
    descarte_agotado <- FALSE
    largas <- .columnas_de_tipo_largo_dbi(tipos_declarados)
    if (length(largas) && length(largas) < length(campos_sql)) {
      quedan <- setdiff(seq_along(campos_sql), largas)
      candidato <- armar_muestra_dbi(quedan)
      prueba <- .consultar_dbi(
        conexion, candidato$sql, presupuesto, filas = candidato$filas,
        etapa = "lectura_muestra"
      )
      if (isTRUE(prueba$ok)) {
        recuperado <- list(
          consulta = prueba, armado = candidato,
          quedan = quedan, omitidas = largas
        )
        tipo_omision <- "tipo_declarado"
      }
    }
    culpables <- integer()
    tope_descarte <- 0
    if (is.null(recuperado) && length(campos_sql) > 1L) {
      # El descarte gasta consultas del mismo presupuesto que el resto del
      # perfil. Recuperar la muestra a costa de quedarse sin saldo para las
      # demas mediciones seria cambiar un agujero por otro, asi que el descarte
      # no puede llevarse mas de la mitad de lo que queda.
      saldo_actual <- if (is.null(presupuesto)) Inf else .saldo_dbi(presupuesto)
      tope_descarte <- min(
        2L * length(campos_sql), .TOPE_SONDAS_DESCARTE_DBI,
        max(0, floor(saldo_actual / 2))
      )
      aislamiento <- .aislar_ilegibles_dbi(
        sondear_muestra_dbi, hay_saldo_dbi, length(campos_sql), tope_descarte
      )
      sondas_descarte <- aislamiento$sondas
      descarte_agotado <- isTRUE(aislamiento$agotado)
      culpables <- aislamiento$culpables
      if (length(culpables) && length(culpables) < length(campos_sql)) {
        quedan <- setdiff(seq_along(campos_sql), culpables)
        candidato <- armar_muestra_dbi(quedan)
        prueba <- .consultar_dbi(
          conexion, candidato$sql, presupuesto, filas = candidato$filas,
          etapa = "lectura_muestra"
        )
        if (isTRUE(prueba$ok)) {
          recuperado <- list(
            consulta = prueba, armado = candidato,
            quedan = quedan, omitidas = culpables
          )
        tipo_omision <- "descarte"
        }
      }
    }
    if (!is.null(recuperado)) {
      campos_omitidos <- campos[recuperado$omitidas]
      motivo_omision <- .motivo_omision_muestra_dbi(
        tipo_omision, campos_omitidos, sondas_descarte, motivo_original
      )
      cobertura <- rbind(cobertura, .registro_cobertura_dbi(
        "perfil_muestra", .texto_tabla_dbi(tabla), "alcance_distinto",
        motivo_omision,
        paste(
          "El resumen por columna las cubre igual; lo que falta es su perfil",
          "por fila. Para incluirlas, convertirlas a texto acotado en una",
          "vista y perfilar esa vista."
        ),
        recuperado$armado$sql
      ))
      consulta <- recuperado$consulta
      sql_muestra <- recuperado$armado$sql
      filas <- recuperado$armado$filas
      campos <- campos[recuperado$quedan]
      campos_sql <- campos_sql[recuperado$quedan]
      # `muestreo_meta` se arma antes de leer, asi que sin esto quedaba
      # congelado con la lectura que fallo: declaraba haber leido la columna
      # que justamente no se pudo leer, y publicaba el SQL original en vez
      # del que de verdad se emitio. Es el invariante al reves -informar como
      # medido lo que no se midio- y en el peor lugar, porque `meta` es donde
      # se mira para saber que se hizo. `metodo` y `acotado_en` entran por lo
      # mismo: el reintento puede cambiar la forma de muestrear.
      muestreo_meta$columnas_leidas <- campos
      muestreo_meta$sql_muestra <- sql_muestra
      muestreo_meta$acotado_en <- recuperado$armado$acotado_en
      muestreo_meta$metodo <- recuperado$armado$metodo
      if (!is.null(recuperado$armado$fuente)) {
        muestreo_meta$fraccion <- recuperado$armado$fuente$fraccion
        muestreo_meta$tamano_muestra <- recuperado$armado$fuente$filas_solicitadas
      }
      muestreo_meta$columnas_omitidas <- campos_omitidos
      muestreo_meta$motivo_columnas_omitidas <- motivo_omision
      # Como se decidio la omision, para que se pueda filtrar: comprobada por
      # descarte, o supuesta por el tipo que declaro el motor.
      muestreo_meta$omision_comprobada <- identical(tipo_omision, "descarte")
      muestreo_meta$sondas_descarte <- as.numeric(sondas_descarte)
    } else if (sondas_descarte > 0L) {
      # El descarte corrio y no sirvio. Decirlo es parte del resultado: sin
      # esto el motivo final seria el del motor a secas y nadie sabria que ya
      # se busco la columna culpable. Y cada final es distinto -no aislar
      # ninguna no es lo mismo que que fallen todas-, asi que el texto no puede
      # ser uno solo.
      detalle <- if (!length(culpables)) {
        paste(
          "ninguna fallo por si sola: el fallo no es de una columna en",
          "particular."
        )
      } else if (length(culpables) == length(campos_sql)) {
        "todas fallan por si solas: no queda ningun subconjunto legible."
      } else {
        "la lectura sin las que fallan tampoco salio."
      }
      if (descarte_agotado) {
        detalle <- paste0(
          "se agoto el tope de sondas (", tope_descarte,
          ") y el aislamiento quedo parcial; ", detalle
        )
      }
      consulta$motivo <- paste0(
        motivo_original, " Se sondearon ", sondas_descarte,
        " subconjuntos de columnas para aislar cual no se puede leer, y ",
        detalle
      )
    }
  }
  if (!consulta$ok) {
    .registrar_etapa_dbi(
      trazador, "lectura_muestra", lectura_inicio, .ahora_dbi(),
      cpu_inicio = lectura_cpu_inicio, cpu_fin = .ahora_cpu_dbi()
    )
    .registrar_etapa_dbi(
      trazador, "perfilado_muestra", estado = "no_solicitado"
    )
    # Antes se descartaba aca el resumen entero cuando fallaba la consulta de
    # muestra. Ahora la muestra se declara no disponible y el resumen sale igual,
    # con su alcance.
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "no_disponible",
      paste0("No se pudo leer la muestra: ", consulta$motivo),
      paste(
        "El resumen de tabla completa se devolvio igual. Para reintentar solo",
        "la muestra, declarar `dialecto` o `orden_muestra`, o reducir",
        "`muestra`."
      ),
      sql_muestra
    ))
    return(list(perfil = NULL, cobertura = cobertura, muestreo = muestreo_meta))
  }
  datos_muestra <- consulta$datos
  n_obtenidas <- nrow(datos_muestra)
  alcance$filas_efectivas <- as.numeric(n_obtenidas)
  alcance$celdas_efectivas <- alcance$filas_efectivas * length(campos_sql)
  alcance$bytes_muestra <- if (is.finite(max_bytes_muestra)) {
    as.numeric(utils::object.size(datos_muestra))
  } else {
    NA_real_
  }
  alcance$recortada <- alcance$filas_efectivas < alcance$filas_solicitadas
  alcance$motivos <- .motivos_muestra_perfilado(alcance)
  .registrar_etapa_dbi(
    trazador, "lectura_muestra", lectura_inicio, .ahora_dbi(),
    cpu_inicio = lectura_cpu_inicio, cpu_fin = .ahora_cpu_dbi()
  )
  muestreo_meta$filas_obtenidas <- as.numeric(n_obtenidas)
  muestreo_meta$celdas_efectivas <- alcance$celdas_efectivas
  muestreo_meta$bytes_sonda <- alcance$bytes_sonda
  muestreo_meta$bytes_muestra <- alcance$bytes_muestra
  muestreo_meta$tabla_completa <- n_obtenidas == .numero_dbi(n_total)
  if (!identical(as.numeric(n_obtenidas), as.numeric(n_obtener))) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "alcance_distinto",
      paste0(
        "La consulta de muestra devolvio ", n_obtenidas, " filas; se esperaban ",
        .entero_sql_dbi(n_obtener), ". La tabla pudo cambiar durante la lectura."
      ),
      paste(
        "El perfil de la muestra describe las filas efectivamente leidas;",
        "`filas_obtenidas` declara cuantas son."
      ),
      sql_muestra
    ))
    muestreo_meta$coincide_con_lo_pedido <- FALSE
  } else {
    muestreo_meta$coincide_con_lo_pedido <- TRUE
  }
  if (is.null(argumentos$nombre)) {
    argumentos$nombre <- paste0("muestra DBI de ", .texto_tabla_dbi(tabla))
  }
  argumentos$muestra <- Inf
  # Los topes ya se resolvieron sobre la relación remota. Pasar `Inf` acá
  # evita una segunda decisión local que podría volver a recortar en R lo que
  # el bloque acaba de traer y, además, deja la cobertura con una sola fuente
  # de verdad: el alcance calculado antes de la lectura.
  argumentos$max_celdas_muestra <- Inf
  argumentos$max_bytes_muestra <- Inf
  if (!is.null(trazador)) {
    attr(datos_muestra, "lupa_trazador_tiempos_dbi") <- trazador
  }
  perfil <- tryCatch(
    .medir_etapa_dbi(
      trazador, "perfilado_muestra",
      do.call(perfilar, c(list(datos = datos_muestra), argumentos))
    ),
    error = function(e) e
  )
  if (inherits(perfil, "condition")) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "no_disponible",
      paste0(
        "La muestra se leyo pero no se pudo perfilar: ",
        conditionMessage(perfil)
      ),
      paste(
        "El resumen de tabla completa se devolvio igual. Revisar los",
        "argumentos enviados a perfilar()."
      ),
      sql_muestra
    ))
    return(list(perfil = NULL, cobertura = cobertura, muestreo = muestreo_meta))
  }
  perfil$meta$filas_analizadas <- alcance$filas_efectivas
  perfil$meta$muestreo <- n_obtenidas < .numero_dbi(n_total)
  perfil$meta$muestra <- as.numeric(muestra)
  perfil$meta$muestra_efectiva <- alcance$filas_efectivas
  perfil$meta$celdas_solicitadas <- alcance$celdas_solicitadas
  perfil$meta$celdas_efectivas <- alcance$celdas_efectivas
  perfil$meta$max_celdas_muestra <- max_celdas_muestra
  perfil$meta$max_bytes_muestra <- max_bytes_muestra
  perfil$meta$bytes_sonda <- alcance$bytes_sonda
  perfil$meta$bytes_muestra <- alcance$bytes_muestra
  cobertura_topes <- .cobertura_muestra_perfilado(alcance)
  perfil$meta$tope_que_mando <- .tope_que_mando(alcance)
  perfil$meta$tope_que_mando_texto <- if (nrow(cobertura_topes)) {
    cobertura_topes$motivo[[1L]]
  } else {
    NA_character_
  }
  if (nrow(cobertura_topes)) {
    perfil$cobertura_diagnosticos <- rbind(
      perfil$cobertura_diagnosticos, cobertura_topes
    )
    rownames(perfil$cobertura_diagnosticos) <- NULL
    # El resumen es el canal principal de la via DBI. Reusar exactamente la
    # fila del perfil evita que dos redacciones del mismo tope se separen con
    # el tiempo y que una de las dos deje de declarar lo que la otra vio.
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", .texto_tabla_dbi(tabla), "degradado",
      cobertura_topes$motivo[[1L]], cobertura_topes$como_resolverlo[[1L]],
      sql_muestra
    ))
  }
  perfil <- .constante_no_medible_en_muestra_dbi(perfil, muestreo_meta)
  perfil$meta$origen_dbi <- list(
    tipo = "DBI",
    conexion = info_conexion,
    tabla = .texto_tabla_dbi(tabla),
    muestreo = muestreo_meta,
    solo_lectura = TRUE,
    objetos_temporales = FALSE
  )
  list(perfil = perfil, cobertura = cobertura, muestreo = muestreo_meta)
}

# ---- Portones ------------------------------------------------------------

.preparar_dbi <- function(conexion, tabla, universo, muestra_motor, muestra,
                          orden_muestra, estrategia_mediana, metricas,
                          max_consultas, dialecto, tamano_lote = NULL,
                          tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                          tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                          bloque_muestra, instrumentar = TRUE,
                          contar = TRUE, incluir_valores = TRUE,
                          estrategia_distintos = "exacta",
                          politica_costo = "todas",
                          umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
                          contar_muestreo = TRUE,
                          sondar_muestreo = TRUE,
                          max_bytes_procesamiento = .MAX_BYTES_MUESTRA,
                          max_bytes_materializacion = .MAX_BYTES_MUESTRA) {
  .requerir_dbi()
  max_bytes_procesamiento <- .validar_presupuesto_bytes_dbi(
    max_bytes_procesamiento, "max_bytes_procesamiento"
  )
  max_bytes_materializacion <- .validar_presupuesto_bytes_dbi(
    max_bytes_materializacion, "max_bytes_materializacion"
  )
  universo <- match.arg(universo, c("tabla_completa", "muestra_motor"))
  estrategia_mediana <- match.arg(
    estrategia_mediana, c("exacta", "aproximada_motor")
  )
  dialecto <- match.arg(
    dialecto, c("auto", "limit", "top", "fetch_first", "rownum", "portable")
  )
  muestra <- .validar_muestra_dbi(muestra)
  if (identical(universo, "muestra_motor")) {
    if (!is.numeric(muestra_motor) || length(muestra_motor) != 1L ||
        is.na(muestra_motor) || !is.finite(muestra_motor) ||
        muestra_motor < 1 || muestra_motor != floor(muestra_motor)) {
      .detener_dbi(
        "lupa_error_argumento_dbi",
        "`muestra_motor` debe ser un entero positivo finito cuando `universo = \"muestra_motor\"`."
      )
    } else {
      muestra_motor <- as.numeric(muestra_motor)
    }
    if (identical(estrategia_mediana, "aproximada_motor")) {
      .detener_dbi(
        "lupa_error_argumento_dbi",
        "`universo = \"muestra_motor\"` no se puede combinar con `estrategia_mediana = \"aproximada_motor\"`: la muestra ya es una aproximacion del universo."
      )
    }
  } else if (!is.null(muestra_motor)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`muestra_motor` solo se puede usar con `universo = \"muestra_motor\"`."
    )
  }
  bloque_muestra <- match.arg(
    bloque_muestra, c("con_muestra", "solo_agregados")
  )
  if (identical(bloque_muestra, "solo_agregados")) {
    orden_muestra <- NULL
  }
  metricas_solicitadas <- .validar_metricas_dbi(metricas)
  politica_costo <- .validar_politica_costo_dbi(
    politica_costo, umbral_cardinalidad
  )
  estrategia_distintos <- .validar_estrategia_distintos_dbi(
    estrategia_distintos
  )
  metricas <- .metricas_para_politica_costo_dbi(
    metricas_solicitadas, politica_costo, incluir_valores
  )
  max_consultas <- .validar_max_consultas_dbi(max_consultas)
  tamanos_lote <- .resolver_tamanos_lote_dbi(
    tamano_lote, tamano_lote_planos, tamano_lote_distintos
  )
  if (!is.logical(instrumentar) || length(instrumentar) != 1L ||
      is.na(instrumentar)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`instrumentar` debe ser TRUE o FALSE."
    )
  }
  if (!.es_conexion_dbi(conexion)) {
    .detener_dbi(
      "lupa_error_conexion_dbi",
      "`conexion` debe ser una conexion DBI: no hereda de `DBIConnection`."
    )
  }
  if (!.conexion_valida_dbi(conexion)) {
    .detener_dbi(
      "lupa_error_conexion_dbi",
      "`conexion` debe ser una conexion DBI abierta y valida."
    )
  }
  presupuesto <- .presupuesto_dbi(max_consultas, instrumentar = instrumentar)
  .contar_dbi(presupuesto)
  # Un nombre de dos partes con punto es lo que cualquiera escribe, y
  # `dbExistsTable()` no lo resuelve: lo toma como un nombre literal. Antes esto
  # hacia que `coleccion("esquema.tabla")` funcionara y `perfilar_dbi()` con el
  # mismo texto fallara diciendo que la tabla no existe. Se conserva el literal
  # cuando existe y, si no, se usa el mismo parseo que `coleccion()`.
  existe <- tryCatch(
    DBI::dbExistsTable(conexion, tabla),
    error = function(e) e
  )
  if (inherits(existe, "condition")) {
    .detener_dbi("lupa_error_tabla_dbi", paste0(
      "No se pudo comprobar `tabla`: ", conditionMessage(existe)
    ))
  }
  if (is.character(tabla) && length(tabla) == 1L && !is.na(tabla) &&
      grepl(".", tabla, fixed = TRUE)) {
    if (!isTRUE(existe)) {
      cortado <- tryCatch(.partir_identificador(tabla), error = function(e) NULL)
      partes <- if (!is.null(cortado) && !isTRUE(cortado$abierto)) {
        vapply(cortado$partes, .quitar_comillas_identificador, character(1L),
               USE.NAMES = FALSE)
      } else character()
      if (length(partes) == 2L && all(nzchar(partes))) {
        tabla <- DBI::Id(schema = partes[[1L]], table = partes[[2L]])
      }
    }
  }
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  motivo_existe <- NA_character_
  if (!isTRUE(existe)) {
    # Algunos controladores no implementan nombres calificados en
    # `dbExistsTable()`. La consulta no trae datos y confirma la tabla sin
    # depender de una API de metadatos incompleta.
    comprobacion <- .consultar_dbi(
      conexion, paste0("SELECT 1 FROM ", tabla_sql, " WHERE 1 = 0"),
      presupuesto, etapa = "porton_tabla"
    )
    existe <- isTRUE(comprobacion$ok)
    if (!isTRUE(existe)) motivo_existe <- comprobacion$motivo
  }
  if (!isTRUE(existe)) {
    .detener_dbi("lupa_error_tabla_dbi", paste(
      .mensaje_tabla_inaccesible_dbi(motivo_existe)
    ))
  }
  lista_campos <- .campos_dbi(conexion, tabla, tabla_sql, presupuesto)
  campos_declarados <- lista_campos$campos
  if (!is.null(orden_muestra) &&
      (!is.character(orden_muestra) || !length(orden_muestra) ||
       anyNA(orden_muestra) || any(!nzchar(orden_muestra)))) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`orden_muestra` debe ser NULL o nombres de columnas."
    )
  }
  if (length(orden_muestra)) {
    posicion <- .resolver_columnas_dbi(orden_muestra, campos_declarados)
    if (anyNA(posicion)) {
      .detener_dbi("lupa_error_argumento_dbi", paste0(
        "Columnas de `orden_muestra` inexistentes: ",
        paste(orden_muestra[is.na(posicion)], collapse = ", "), "."
      ))
    }
    orden_muestra <- campos_declarados[posicion]
  }

  esquema <- .esquema_dbi(conexion, tabla_sql, campos_declarados, presupuesto)
  campos <- esquema$campos
  prototipo <- esquema$prototipo
  if (length(names(prototipo)) == length(campos) &&
      !identical(names(prototipo), campos)) {
    campos <- names(prototipo)
  }
  orden_sql <- if (length(orden_muestra)) {
    posicion <- .resolver_columnas_dbi(orden_muestra, campos)
    if (anyNA(posicion)) {
      .detener_dbi("lupa_error_esquema_dbi", paste0(
        "El motor rechazo las columnas de `orden_muestra`: ",
        paste(orden_muestra[is.na(posicion)], collapse = ", "), "."
      ))
    }
    orden_muestra <- campos[posicion]
    vapply(orden_muestra, function(campo) {
      as.character(DBI::dbQuoteIdentifier(conexion, campo))
    }, character(1L), USE.NAMES = FALSE)
  } else {
    character()
  }
  # La forma de la consulta obligatoria se prueba antes de gastar el bloque de
  # agregados: proyeccion, ORDER BY y clausula de limite, con cero filas.
  sql_forma <- paste0(
    "SELECT ", paste(esquema$campos_sql, collapse = ", "), " FROM ", tabla_sql,
    " WHERE 1 = 0",
    if (length(orden_sql)) paste0(" ORDER BY ", paste(orden_sql, collapse = ", ")) else ""
  )
  resolucion <- .resolver_dialecto_dbi(
    conexion, sql_forma, dialecto, presupuesto
  )
  es_numerico <- vapply(seq_along(campos), function(i) {
    .es_numerico_dbi(
      prototipo[[i]],
      if (i <= length(esquema$tipos)) esquema$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  muestreo <- NULL
  if (identical(universo, "muestra_motor")) {
    if (isTRUE(sondar_muestreo)) {
      muestreo <- .sondar_muestreo_dbi(
        conexion, tabla_sql, resolucion$dialecto, presupuesto
      )
      muestreo$sondas_previstas <- length(muestreo$sondas)
    } else {
      # Elegir una candidata por las senas del motor no comprueba que funcione:
      # el plan no puede pagar una sonda que ya lee la tabla solo para saberlo.
      # Publica las sondas que la corrida intentara y deja la verificacion para
      # la corrida; si fallan, el resultado se degrada como cualquier capacidad.
      candidatos <- .candidatos_muestreo_dbi(conexion, resolucion$dialecto)
      muestreo <- list(
        disponible = length(candidatos) > 0L,
        candidato = if (length(candidatos)) candidatos[[1L]] else NULL,
        sondas = character(),
        sondas_previstas = .sondas_muestreo_previstas_dbi(candidatos),
        motivo = "La capacidad de muestreo queda sin sondear hasta la corrida."
      )
    }
    estado_forma <- .estado_forma_muestreo_dbi(
      muestreo$candidato, tabla_sql, esquema$campos_sql, muestra_motor,
      resolucion$dialecto
    )
    muestreo$forma_construible <- estado_forma$forma_construible
    if (!isTRUE(estado_forma$forma_construible)) {
      # La corrida y el plan comparten esta decisión estructural: no hay una
      # consulta de datos que pueda producir métricas sobre una muestra que no
      # se puede escribir. La lista de métricas solicitadas se conserva para
      # publicar cada ausencia, pero no se intenta emitirlas.
      if (!is.null(muestreo$candidato)) {
        muestreo$disponible <- FALSE
        if (is.null(muestreo$sondas) || !length(muestreo$sondas) ||
            grepl("no pudo construir", muestreo$motivo, fixed = TRUE) ||
            grepl("sin sondear", muestreo$motivo, fixed = TRUE)) {
          muestreo$motivo <- estado_forma$motivo
        }
      }
      # No tiene sentido sondear moda, mediana ni cardinalidad para una
      # muestra que ya sabemos que no se puede escribir. `metricas_solicitadas`
      # conserva el pedido para que el resumen publique sus ausencias.
      metricas <- character()
    }
  }
  estrategia_distintos <- .estrategia_distintos_dbi(
    metricas_solicitadas, politica_costo, incluir_valores,
    estrategia_distintos
  )
  estrategia_distintos <- .resolver_estrategia_distintos_dbi(
    conexion, estrategia_distintos,
    presupuesto, "distintos" %in% metricas,
    tabla = tabla, columnas = campos, universo = universo
  )
  fuentes_cardinalidad_costo <- .fuentes_cardinalidad_vacias_dbi(campos)
  catalogo_cardinalidad <- NULL
  # La disponibilidad gobierna la medicion de cardinalidad, no la lectura de
  # una garantia estructural que no recorre la tabla. La consulta de catalogo
  # tampoco recorre la tabla y se hace en todas las corridas para publicar la
  # misma informacion en `meta$clave`.
  fuentes <- .resolver_fuentes_cardinalidad_dbi(
    conexion, tabla, campos, estrategia_distintos, presupuesto
  )
  fuentes_cardinalidad_costo <- fuentes$fuentes
  catalogo_cardinalidad <- fuentes$catalogo
  fuentes_no_exactas <- vapply(
    fuentes_cardinalidad_costo,
    function(x) !isTRUE(x$exacta), logical(1L)
  )
  muestreo_ejecutable <- !identical(universo, "muestra_motor") ||
    isTRUE(muestreo$disponible)
  estrategia_distintos$requiere_medicion <- if (identical(
    estrategia_distintos$estado, "estimado_catalogo"
  )) {
    FALSE
  } else {
    (isTRUE(estrategia_distintos$publica) &&
       isTRUE(estrategia_distintos$disponible) &&
       isTRUE(muestreo_ejecutable) &&
       (identical(universo, "muestra_motor") || any(fuentes_no_exactas))) ||
      (isTRUE(estrategia_distintos$para_costo) &&
       isTRUE(estrategia_distintos$disponible) &&
       isTRUE(muestreo_ejecutable) && any(fuentes_no_exactas))
  }
  # Una estrategia no disponible no abre una segunda oportunidad para ejecutar
  # el conteo exacto. Se quita de la ejecucion, pero queda en `metricas` para
  # que el resumen publique el motivo de la ausencia.
  if (!isTRUE(estrategia_distintos$disponible)) {
    metricas <- setdiff(metricas, "distintos")
  }
  aproximaciones <- list()
  aproximaciones_resolucion <- list()
  moda_guardian_resolucion <- NULL
  moda_guardian <- NULL
  mediana_consolidada_resolucion <- NULL
  mediana_consolidada <- NULL
  mediana_escalar_resolucion <- NULL
  mediana_escalar <- NULL
  mediana_cte_ventana_resolucion <- NULL
  mediana_cte_ventana <- NULL
  mediana_cte_ventana_motivo <- NA_character_
  if ("moda" %in% metricas && isTRUE(incluir_valores)) {
    moda_guardian_resolucion <- .sondar_moda_guardian_dbi(
      conexion, resolucion$dialecto, presupuesto
    )
    if (isTRUE(moda_guardian_resolucion$disponible)) {
      moda_guardian <- moda_guardian_resolucion$candidato
    }
  }
  if ("mediana" %in% metricas && isTRUE(incluir_valores) &&
      any(es_numerico)) {
    mediana_consolidada_resolucion <- .sondar_mediana_consolidada_dbi(
      conexion, presupuesto
    )
    if (isTRUE(mediana_consolidada_resolucion$disponible)) {
      mediana_consolidada <- mediana_consolidada_resolucion$candidato
    }
  }
  if (identical(estrategia_distintos$estrategia_solicitada,
                "aproximada_motor") &&
      isTRUE(estrategia_distintos$disponible)) {
    resolucion_distintos <- list(
      disponible = TRUE, candidato = estrategia_distintos$candidato,
      sondas = estrategia_distintos$sondas,
      motivo = estrategia_distintos$motivo
    )
    aproximaciones_resolucion$distintos <- resolucion_distintos
    aproximaciones$distintos <- estrategia_distintos$candidato
  }
  if ("mediana" %in% metricas && isTRUE(incluir_valores) &&
      any(es_numerico) && is.null(mediana_consolidada) &&
      is.null(mediana_escalar)) {
    mediana_escalar_resolucion <- .sondar_mediana_escalar_dbi(
      conexion, resolucion$dialecto, presupuesto,
      materializar = identical(universo, "muestra_motor")
    )
    if (isTRUE(mediana_escalar_resolucion$disponible)) {
      mediana_escalar <- mediana_escalar_resolucion$candidato
    }
  }
  if (identical(universo, "muestra_motor") && "mediana" %in% metricas &&
      isTRUE(incluir_valores) && any(es_numerico) &&
      is.null(mediana_consolidada) && is.null(mediana_escalar)) {
    mediana_cte_ventana_resolucion <- .sondar_mediana_cte_ventana_dbi(
      conexion, presupuesto
    )
    mediana_cte_ventana_motivo <- mediana_cte_ventana_resolucion$motivo
    if (isTRUE(mediana_cte_ventana_resolucion$disponible)) {
      mediana_cte_ventana <- mediana_cte_ventana_resolucion$candidato
      mediana_cte_ventana$nombre <- .nombre_mediana_cte_muestra_dbi(
        muestreo
      )
    }
  }
  if ("mediana" %in% metricas && isTRUE(incluir_valores) &&
      any(es_numerico) && identical(estrategia_mediana, "aproximada_motor") &&
      is.null(mediana_consolidada) && is.null(mediana_escalar)) {
    resolucion_mediana <- .sondar_aproximacion_dbi(
      conexion, "mediana", presupuesto
    )
    aproximaciones_resolucion$mediana <- resolucion_mediana
    if (isTRUE(resolucion_mediana$disponible)) {
      aproximaciones$mediana <- resolucion_mediana$candidato
    }
  }
  sql_conteo <- paste0(
    "SELECT COUNT(*) AS ",
    as.character(DBI::dbQuoteIdentifier(conexion, "lupa_n_total")),
    " FROM ", tabla_sql
  )
  # El plan necesita el total antes de estimar el trabajo y conserva por eso el
  # conteo como porton propio. En una corrida, en cambio, el total viaja con la
  # primera consulta de agregados. Las formas de TABLESAMPLE necesitan conocer
  # el total antes de poder escribir su porcentaje; ese es el unico camino de
  # una corrida que paga el porton por adelantado para no cambiar su muestra.
  contar_adelante <- isTRUE(contar) || (
    isTRUE(contar_muestreo) && identical(universo, "muestra_motor") &&
      !is.null(muestreo) &&
      !is.null(muestreo$candidato) &&
      (identical(muestreo$candidato$tipo, "tablesample") ||
       (identical(muestreo$candidato$tipo, "aleatorio") &&
        length(muestreo$candidato$funciones) == 1L &&
        identical(muestreo$candidato$funciones[[1L]]$nombre, "newid")))
  )
  conteo <- NULL
  n_total <- NA_real_
  if (contar_adelante) {
    conteo <- .escalar_dbi(
      conexion, sql_conteo, "lupa_n_total", presupuesto,
      etapa = "conteo_filas"
    )
    if (!conteo$ok) {
      .detener_dbi("lupa_error_conteo_dbi", paste0(
        "No se pudo contar la tabla: ", conteo$motivo
      ), datos = list(sql = sql_conteo))
    }
    n_total <- .conteo_dbi(conteo$valor)
    if (is.na(n_total)) {
      .detener_dbi("lupa_error_conteo_dbi", paste0(
        "La consulta de conteo no devolvio un numero utilizable. SQL: ",
        sql_conteo
      ), datos = list(sql = sql_conteo))
    }
  }
  hay_agregados_fusionables <- length(campos) > 0L && (
    any(metricas %in% c("validos", "distintos")) ||
      any(es_numerico) && any(metricas %in% c("basicos", "desvio"))
  )
  columnas_distintos_ejecucion <- .columnas_distintos_ejecucion_dbi(
    metricas, estrategia_distintos, campos,
    fuentes_cardinalidad_costo, politica_costo
  )
  columnas_moda_estimacion <- if (
    "moda" %in% metricas && isTRUE(incluir_valores)
  ) campos else character()
  columnas_mediana_estimacion <- if (
    "mediana" %in% metricas && isTRUE(incluir_valores)
  ) campos[es_numerico] else character()
  columnas_catalogo_derrame <- unique(c(
    columnas_distintos_ejecucion,
    columnas_moda_estimacion,
    columnas_mediana_estimacion
  ))
  # Se consulta el catalogo una sola vez por corrida. En la corrida real el
  # aviso se emite mas tarde, cuando los agregados planos ya terminaron, pero
  # los datos de la estimacion quedan listos antes del primer distinto. En el
  # plan esto sigue siendo solo lectura de metadatos, nunca una medicion.
  presupuesto$estimacion_derrame <- .estimar_derrame_postgresql_dbi(
    conexion, tabla, columnas_distintos_ejecucion, presupuesto,
    exacto = identical(estrategia_distintos$estrategia_resuelta, "COUNT(DISTINCT)"),
    universo = universo, tamano_lote = tamanos_lote$distintos,
    columnas_stats = columnas_catalogo_derrame
  )
  forma_mediana_estimacion <- if (!is.null(mediana_consolidada)) {
    "consolidada"
  } else if (!is.null(mediana_escalar)) {
    "subconsulta_escalar"
  } else {
    "dos_consultas"
  }
  presupuesto$estimacion_derrame_moda <-
    .estimar_derrame_familia_postgresql_dbi(
      conexion, tabla, columnas_moda_estimacion, presupuesto, "moda",
      universo = universo, tamano_lote = tamanos_lote$planos,
      forma = NA_character_, dialecto = resolucion$dialecto,
      moda_guardian = moda_guardian, tipos = esquema$tipos,
      prototipo = prototipo, tabla_sql = tabla_sql
    )
  presupuesto$estimacion_derrame_mediana <-
    .estimar_derrame_familia_postgresql_dbi(
      conexion, tabla, columnas_mediana_estimacion, presupuesto, "mediana",
      universo = universo, tamano_lote = tamanos_lote$planos,
      forma = forma_mediana_estimacion, tipos = esquema$tipos,
      prototipo = prototipo, tabla_sql = tabla_sql
    )
  metricas_ejecucion <- metricas
  list(
    universo = universo, muestra_motor = muestra_motor,
    estrategia_mediana = estrategia_mediana,
    metricas = metricas_solicitadas, metricas_ejecucion = metricas_ejecucion,
    muestra = muestra,
    bloque_muestra = bloque_muestra,
    max_consultas = max_consultas, presupuesto = presupuesto,
    instrumentar = isTRUE(instrumentar),
    tamano_lote = tamanos_lote$planos,
    tamano_lote_planos = tamanos_lote$planos,
    tamano_lote_distintos = tamanos_lote$distintos,
    tabla_sql = tabla_sql, campos = campos, campos_sql = esquema$campos_sql,
    prototipo = prototipo, tipos = esquema$tipos, esquema = esquema,
    es_numerico = es_numerico, muestreo = muestreo,
    aproximaciones = aproximaciones,
    aproximaciones_resolucion = aproximaciones_resolucion,
    moda_guardian = moda_guardian,
    moda_guardian_resolucion = moda_guardian_resolucion,
    mediana_consolidada = mediana_consolidada,
    mediana_consolidada_resolucion = mediana_consolidada_resolucion,
    mediana_escalar = mediana_escalar,
    mediana_escalar_resolucion = mediana_escalar_resolucion,
    mediana_cte_ventana = mediana_cte_ventana,
    mediana_cte_ventana_resolucion = mediana_cte_ventana_resolucion,
    mediana_cte_ventana_motivo = mediana_cte_ventana_motivo,
    politica_costo = politica_costo,
    estrategia_distintos = estrategia_distintos,
    fuentes_cardinalidad_costo = fuentes_cardinalidad_costo,
    catalogo_cardinalidad = catalogo_cardinalidad,
    columnas_distintos_ejecucion = columnas_distintos_ejecucion,
    estimacion_derrame = presupuesto$estimacion_derrame,
    estimacion_derrame_moda = presupuesto$estimacion_derrame_moda,
    estimacion_derrame_mediana = presupuesto$estimacion_derrame_mediana,
    n_total = n_total, conteo = conteo, sql_conteo = sql_conteo,
    conteo_fusionable = hay_agregados_fusionables && !(
      identical(universo, "muestra_motor") && !is.null(muestreo) &&
        !is.null(muestreo$candidato) &&
        identical(muestreo$candidato$tipo, "tablesample")
    ),
    orden_sql = orden_sql,
    orden_muestra = orden_muestra, dialecto = resolucion$dialecto,
    max_bytes_procesamiento = max_bytes_procesamiento,
    max_bytes_materializacion = max_bytes_materializacion,
    resolucion = resolucion, campos_declarados = campos_declarados,
    lista_campos = lista_campos
  )
}

#' Perfilar una muestra leída mediante DBI
#'
#' Calcula en SQL un resumen sobre la tabla completa o sobre una relación
#' muestreada por el motor, según `universo`. Con `universo = "muestra_motor"`
#' ejecuta una sola selección y la materializa en un spool externo de la sesión
#' cliente; el resumen y [perfilar()] leen esa misma materialización.
#' `bloque_muestra = "solo_agregados"` sigue omitiendo el objeto
#' `perfil_muestra`, pero no vuelve a seleccionar filas.
#'
#' Antes, la promesa era literalmente: "la función no escribe en la conexión ni
#' crea objetos temporales". Ahora es: "no escribe en la conexión ni crea
#' objetos temporales del motor; `muestra_motor` puede crear un spool externo
#' temporal de sesión, con bytes, checksum, presupuesto y estado". `DBI` es una
#' dependencia opcional. Cada agregado no disponible queda en `NA` y su
#' consulta, estado y motivo se conservan en `resumen_tabla$sql`.
#' Las expresiones se ejecutan como capacidades a comprobar, no como un
#' dialecto SQL universal.
#'
#' @section Dos tablas se llaman cobertura:
#' El resultado trae dos, y cubren cosas distintas. `resumen_tabla$cobertura`
#' habla de **métricas SQL**: qué pidió esta función al motor y qué pasó, con
#' `bloque`, `elemento`, `estado` —`no_disponible`, `no_solicitado`,
#' `degradado`, `presupuesto_agotado`, `alcance_distinto`— y la consulta en
#' `sql`. `alcance_distinto` declara que dos valores exactos incoherentes
#' salieron de grupos de consistencia distintos: es evidencia de que la tabla
#' cambio durante la corrida, no una acusacion contra el motor.
#' `perfil_muestra$cobertura_diagnosticos` habla de **diagnósticos**: qué
#' comprobación no se corrió sobre la muestra y por qué, con `diagnostico`,
#' `columna`, `motivo` y `como_resolverlo`, el mismo esquema que devuelve
#' [perfilar()]. Un motor que rechaza una columna aparece en la primera; una
#' prueba estadística que no corresponde a esa columna, en la segunda. Comparten
#' la palabra y no el vocabulario, así que conviene mirar cuál se está leyendo.
#' Si se omite el bloque con `bloque_muestra = "solo_agregados"`, la cobertura
#' usa el estado `no_solicitado`: no es un fallo ni se cuenta como una métrica
#' no disponible. En `muestra_motor`, la selección materializada se hace una
#' sola vez antes de las pasadas; el spool se relee y se verifica mediante su
#' trailer. Un trailer ausente publica
#' `spool_incompleto:trailer_ausente`; un trailer o checksum que no coincide
#' publica `spool_checksum_invalido`.
#'
#' @section Fallo parcial:
#' Ningún bloque descarta al otro. Si se pide la muestra pero el motor rechaza su
#' consulta, o si la muestra no se puede perfilar, el resultado sale igual con
#' `perfil_muestra = NULL` y una fila en `resumen_tabla$cobertura` que declara
#' el estado `no_disponible`, el motivo y cómo resolverlo. Si se pide sólo
#' agregados, `perfil_muestra = NULL` se acompaña de una fila `no_solicitado`:
#' no se intentó leer la muestra. Si el motor rechaza una columna, esa columna
#' queda con sus métricas en `no_disponible` y las demás se perfilan enteras.
#' Los `stop()` de esta vía llevan clase de condición propia —todas heredan de
#' `lupa_error_dbi`— para que se puedan rescatar con `tryCatch()`:
#' `lupa_error_conexion_dbi`, `lupa_error_tabla_dbi`, `lupa_error_campos_dbi`,
#' `lupa_error_conteo_dbi`, `lupa_error_esquema_dbi` y
#' `lupa_error_argumento_dbi`.
#'
#' @section Muestra y aproximaciones:
#' `universo = "muestra_motor"` sondea las formas declaradas por el adaptador y
#' usa `TABLESAMPLE` cuando el motor lo acepta, o una función pseudoaleatoria con
#' el límite del dialecto. Si ninguna forma es compatible, las métricas SQL
#' quedan en `no_disponible`: no se sustituyen por resultados de la tabla completa.
#' Cada registro publica `alcance`, `universo`, `tamano_muestra`, `fraccion`,
#' `metodo` y `error_esperado`. En `resumen_tabla$meta$muestreo`,
#' `tamano_muestra` conserva el nombre historico y declara el tamano efectivo
#' solicitado a la consulta; `filas_solicitadas` y `filas_pedidas` declaran el
#' pedido original y `filas_obtenidas` las filas que devolvio el spool. La
#' materializacion publica `muestra_id`, `snapshot_id`, `orden_id`, `n_filas`,
#' `bytes`, `checksum`, backend, version y presupuesto en
#' `meta$materializacion`; las pasadas publican el mismo `muestra_id`.
#' El contrato de `perfil_muestra` es campo por campo: `meta$filas_analizadas`
#' es la cantidad efectivamente entregada a los diagnósticos; `hallazgos` solo
#' contiene familias disponibles; y `cobertura_diagnosticos` tiene una fila por
#' familia no evaluada. `meta$origen_dbi$muestreo` conserva filas solicitadas,
#' entregadas, reproducibilidad, `muestra_id`, `snapshot_id` y checksum. El
#' método de impresión remite a esa cobertura cuando el campo no está
#' disponible; no reemplaza la ausencia con `NULL` silencioso.
#' Bajo `bloque_filas`, `perfil_muestra` sigue siendo la muestra diagnóstica
#' acotada por `muestra`, `max_celdas_muestra` y `max_bytes_muestra`; no es la
#' cobertura completa. Por eso `perfil_muestra$general$filas` es el tamaño de
#' esa muestra y la cobertura se lee en `meta$bloques$filas_vistas`.
#' En `muestra_motor`, todas las metricas SQL salvo `n` describen la relacion
#' muestreada; `n` es el total de la tabla completa y esta marcado con
#' `alcance = "tabla_completa"`, `metodo = "conteo_universo"` y
#' `error_esperado = "no_aplica"`. Los conteos `n_validos`, `n_faltantes`,
#' `prop_faltantes`, `n_distintos`, `tasa_distintos`, `n_ceros`,
#' `n_negativos` y `frecuencia_moda` son observaciones de esa muestra, no
#' extrapolaciones al universo. Sus motivos declaran la escala local y el
#' denominador `n_total_consulta` cuando corresponde.
#' `TABLESAMPLE SYSTEM` es la fuente preferida cuando el adaptador la declara y
#' la sonda la acepta; publica `metodo_muestreo = "tablesample_system"`,
#' `sesgo_muestreo = "por_bloques"` y el motivo estable
#' `sesgo_muestreo:tablesample_system_por_bloques`. Si se usa el fallback
#' `random_limit` con `NEWID()`, la metadata publica
#' `metodo_muestreo = "random_limit"`, `funcion_muestreo = "newid"`,
#' `sesgo_muestreo = "por_fila"` y
#' `sesgo_muestreo:random_limit_newid_por_fila`. Ese fallback se acepta solo
#' si la politica de costo conserva `n_total`, la proyeccion, la tasa y sus
#' tres umbrales; si no, la mediana queda `no_disponible` con
#' `capacidad_no_aceptada:newid_costo_excede_presupuesto` aunque se silencien
#' los avisos.
#' Si la consulta de la muestra devuelve cero filas, no hay base para medir las
#' metricas de alcance `muestra`: se publican con valor `NA`, estado
#' `no_disponible` y el motivo estable
#' `muestra_vacia:random_limit_sin_filas` (o el metodo efectivo equivalente).
#' Esto no permite concluir que
#' la columna este vacia, por lo que no se publica cero ni se dispara la
#' cascada `sin_valores`. Una muestra no vacia sin valores validos usa
#' `muestra_inestable:sin_valores_validos`; una capacidad no aceptada usa
#' `capacidad_no_aceptada:...`. `n` conserva el conteo de la tabla completa.
#'
#' En una muestra, `error_esperado` vale `no_estimado` para metricas cuyo error
#' podria calcularse bajo un plan probabilistico pero no se calculo,
#' `no_estimable` para la moda, la mediana y la cardinalidad observada, que no
#' tienen una cota simple sin supuestos o un estimador declarado, y `no_aplica`
#' cuando no hubo muestreo efectivo. El `motivo` de cada registro explica la
#' distincion. `metodo`, `tamano_muestra` y `fraccion` conservan las condiciones
#' de la corrida; no se publica una cota numerica sin una formula justificada.
#' Los distintos de una muestra se publican como cardinalidad de la muestra,
#' no como cardinalidad del universo. `estrategia_distintos` es explicita y
#' vale `"exacta"` por omision: calcula `COUNT(DISTINCT)` sobre las filas de la
#' corrida. `"aproximada_motor"` sondea una funcion nativa y, si no hay una
#' capacidad aceptada, deja la metrica `no_disponible`; nunca ejecuta el conteo
#' exacto como repliegue. `"catalogo"` lee `pg_stats.n_distinct` y publica
#' `estimado_catalogo` sólo cuando `universo = "tabla_completa"`. Con
#' `universo = "muestra_motor"` queda `no_disponible`, porque el catálogo
#' describe la relación entera y la corrida mide un subconjunto; no se inventa
#' una equivalencia entre universos.
#' `"omitida"` no emite la
#' consulta. Cada resultado y el atributo `meta$estrategia_distintos` separan
#' `estrategia_solicitada`, `estrategia_resuelta` y `estado`.
#'
#' Las comparaciones que tienen una cota dura usan solo valores del mismo grupo
#' de consistencia. En esta version, el grupo queda probado por el
#' `consulta_id` que ya se registra en `resumen_tabla$sql`: dos metricas con el
#' mismo identificador salieron de la misma sentencia. La consulta exacta de
#' distintos trae `COUNT(columna) AS n_validos_guard` junto a
#' `COUNT(DISTINCT columna)`. Si una capacidad aproximada sólo construye una
#' consulta completa y no puede traer ese guardian, la cota no se comprueba y
#' el motivo lo declara; no se atribuye un valor imposible al motor.
#' La consulta de la moda intenta traer `SUM(COUNT(*)) OVER () AS
#' n_validos_guard` junto a su frecuencia. La forma se sondea antes de usarla;
#' si el motor la rechaza, se conserva la consulta anterior y
#' `meta$moda_guardian` publica el repliegue. Cuando la sonda pasa, la cota
#' `frecuencia_moda <= n_validos` se comprueba dentro de la misma sentencia y
#' el motivo de la métrica lo declara.
#' `meta$snapshot` queda en `FALSE`, siguiendo la declaracion de colecciones: no
#' hubo lectura instantanea del motor. `snapshot_id` identifica positivamente la
#' materializacion cliente, pero solo se publica evidencia positiva de snapshot
#' transaccional cuando el adaptador la demuestra. La cobertura agrega una entrada concreta solo si
#' `n_validos` y `n_distintos` son exactos, incoherentes y provienen de grupos
#' distintos; su motivo conserva ambas sentencias.
#' La mediana muestral que no tiene una forma consolidada o escalar usa una
#' sola CTE con `COUNT(*) OVER ()`, `ROW_NUMBER() OVER` y el promedio de las
#' posiciones centrales. La sonda barata repite esa construccion sobre
#' `VALUES (1.0), (2.0), (3.0), (4.0)`, exige `2.5` y un control negativo que
#' falle; no se sondea la tabla de produccion. Si esa capacidad falla, la
#' mediana se publica como `no_disponible` con
#' `capacidad_no_aceptada:sonda_mediana_cte_ventana`, nunca como
#' `dos_consultas` bajo `muestra_motor`.
#'
#' Las cotas de error no documentadas de una aproximacion nativa quedan como
#' `"desconocido"`. Una aproximacion solo se consolida cuando entrega una
#' expresion que se puede incrustar en el `SELECT`; si solo construye una
#' consulta completa, se emite por separado. Una consulta no emitida o sin un
#' valor utilizable queda `no_disponible`.
#'
#' @section Dialecto:
#' Acotar filas no es SQL estándar. En vez de suponer `LIMIT`, la función
#' sondea el motor con una consulta de cero filas y elige la primera capacidad
#' que acepte: `LIMIT n [OFFSET k]`, `TOP (n)` y `OFFSET … FETCH NEXT`,
#' `FETCH FIRST … ROWS ONLY`, o `ROWNUM`. Si ninguna sirve queda la vía
#' portable, que acota en el cliente con `dbSendQuery()` y `dbFetch(n)`; en ese
#' caso la mediana exacta se declara no disponible en vez de traer media tabla
#' a memoria. El alias de subconsulta se escribe con `AS` o sin él según el
#' motor, y los alias de columna van comillados y se comparan sin distinguir
#' caja, porque hay motores que los pliegan a mayúsculas.
#' Para los motores del dialecto `limit`, la mediana exacta usa una sola
#' sentencia: el `COUNT` queda como subconsulta escalar de la consulta que
#' ordena y recorta. La forma se sondea antes de usarla; en SQLite y
#' PostgreSQL se usan `%` y `/` con division entera. Bajo
#' `muestra_motor`, los dialectos sin una forma escalar ni consolidada usan la
#' CTE de ventanas descrita arriba y no degradan a `dos_consultas`. En
#' `tabla_completa` se conserva el camino previo, incluido `dos_consultas` si
#' es el que resuelve ese dialecto; `PERCENTILE_CONT` no cambia.
#' La mediana consolidada tambien se sondea antes de usarla; en SQL Server
#' requiere nivel de compatibilidad >= 110. Se sondea; estos son los motivos
#' conocidos de que no se active: la funcion no esta disponible o el motor
#' rechaza la sonda. En ese caso el mensaje del motor queda en
#' `meta$mediana_consolidada$motivo`, y se conserva la mediana por columna.
#'
#' @section Costo:
#' Los agregados de una tabla ancha se emiten por lotes; `muestra` acota lo que
#' se trae a R, no el trabajo del motor. `bloque_muestra` decide si se trae esa
#' muestra; `universo`, `metricas`, `tamano_lote_planos`, `tamano_lote_distintos` y
#' `max_consultas` acotan el trabajo SQL. [plan_perfilado_dbi()] dice cuántas
#' consultas se van a emitir antes de emitirlas. El orden de degradación es
#' agregados planos, total del universo cuando hace falta, distintos, moda y
#' mediana. Los
#' agregados planos sobre la misma tabla y filtro —`COUNT(col)`,
#' mínimos, máximos, medias, ceros, negativos y desvío— comparten una consulta
#' por lote y cada consulta que trae `n_validos` lleva además
#' `COUNT(*) AS n_total_consulta` en la misma sentencia. La completitud usa ese
#' denominador local, no el total de otro lote.
#' La fusión conserva la medición, no una identidad bit a bit entre
#' agrupamientos: la media y el desvío pueden diferir en el último bit según
#' cómo se agrupen las sumas, porque la suma en punto flotante no es asociativa.
#'
#' El total del universo se conserva por separado cuando el perfil se calcula
#' sobre una muestra. Si el lote completo es rechazado, sus mitades se sondean
#' por bisección: los grupos
#' aceptados se reutilizan como mediciones y las columnas culpables se reintentan
#' por métrica, con su denominador local. Las fuentes `TABLESAMPLE` que necesitan
#' el total del universo para escribir un porcentaje lo cuentan antes.
#' `COUNT(DISTINCT ...)` queda en una clase separada y usa su propio tamaño de
#' lote, conservador por omisión porque una cardinalidad puede derramar mucho
#' más que veinte agregados planos; la consulta exacta trae su
#' `n_validos_guard` compañero.
#' La proyección temporal no usa esos agregados planos: si hay más de un lote y
#' `instrumentar = TRUE`, se mide el primer lote de distintos y, después de
#' ejecutarlo, se multiplica su mediana por la cantidad total de lotes. El aviso
#' llega antes del segundo lote, en la unidad que se va a evitar; con un solo
#' lote no hay nada que proyectar. Si la duración no se pudo medir, el resultado
#' declara la proyección como no disponible.
#' La moda tiene otro canal: después de cada moda medida se obtiene una tasa en
#' ms por distinto y se usa para proyectar las modas pendientes. La cardinalidad
#' se toma del agregado de la corrida, de una clave garantizada o de la
#' estimación de catálogo que esté disponible; si falta, `meta$costo_moda` lo
#' declara y no inventa un número. El aviso llega antes de la siguiente moda.
#' La mediana se proyecta en ms por fila. La primera mediana medida en esta
#' corrida sirve para proyectar las restantes y el aviso precede a ese trabajo.
#' Si no existe una primera medición local para una mediana total, usa la
#' referencia declarada de otra corrida de 68 ms por millón de filas.
#' Si la consulta inicial que obtuvo las filas fue medida y resulta una cota
#' mayor, se publica también esa cota de lectura —no como medición de mediana—
#' para no subestimar una tabla grande recién cargada.
#' Las dos proyecciones quedan separadas en `meta$costo_moda` y
#' `meta$costo_mediana`; apagar el aviso no apaga su medición ni su metadata.
#' Antes de la primera consulta exacta se estima, cuando PostgreSQL expone
#' `pg_stats`, el tamaño de los hashes con `n_distinct`, `avg_width` y
#' `pg_class.reltuples`; `SHOW work_mem` y, desde PostgreSQL 13,
#' `SHOW hash_mem_multiplier` dan el límite efectivo. `meta$estimacion_derrame`
#' y `attr(meta$plan, "estimacion_derrame")` conservan el diagnóstico, siempre
#' rotulado como estimación y nunca como derrame medido. Si supera el límite se
#' avisa antes de pagar `COUNT(DISTINCT)` y se recomienda subir `work_mem` en la
#' sesión; el paquete no lo modifica. Una estadística ausente, un permiso
#' insuficiente o una versión sin el parámetro dejan la parte correspondiente
#' como no disponible. `n_distinct` es una estimación de muestra y puede quedar
#' corta; si luego `pg_stat_statements` mide un derrame, esa medición manda.
#' La misma corrida publica `meta$estimacion_derrame_moda` y
#' `meta$estimacion_derrame_mediana`. La moda deriva `metodo` del plan exacto
#' mediante `EXPLAIN (FORMAT JSON, COSTS OFF)` sin `ANALYZE`; la mediana usa
#' siempre un sort contra `work_mem`, con pisos de 32 bytes para tipos fijos y
#' 42 para `numeric`. El plan usa `n_validos` de catálogo. En la meta y en el
#' aviso, cuando se pidió la familia `validos`, ese denominador se reemplaza por
#' el medido en los agregados planos y se conserva también el valor de catálogo;
#' si no se midió, el motivo declara que se retuvo el catálogo. Por arbitraje
#' H5, este alcance K2 gobierna solo el denominador: si la estimación publicada
#' marca `supera_memoria = TRUE`, el aviso se emite también con catálogo y
#' declara su fuente. La consolidada
#' decide por el máximo de columna y publica la suma de tapes sólo como
#' `estado_io_total_bytes` informativo.
#'
#' Los avisos de esta vía son deliberadamente distintos de los de [perfilar()]:
#' `perfilar_dbi()` los emite también en guiones no interactivos porque el costo
#' relevante ocurre en el servidor y puede consumir decenas de segundos antes
#' de que el llamador pueda hacer algo. En cambio, el aviso de tabla ancha de
#' [perfilar()] estima trabajo local sobre una tabla ya en R y queda limitado a
#' sesiones interactivas para no convertir la salida de un guion en ruido.
#' Cada aviso DBI tiene su propio interruptor y umbral porque sus unidades no
#' son comparables —segundos frente a bytes— y silenciar uno no debe ocultar el
#' otro. Apagar un aviso no apaga ninguna medición: `meta$costo_distintos`,
#' `meta$costo_moda`, `meta$costo_mediana`, `meta$derrame` y
#' `meta$estimacion_derrame` se publican igual.
#' Lo que no entra en el presupuesto queda en `no_disponible` con su motivo,
#' nunca en cero. `meta$tamano_lote_funciono` conserva el mayor lote aceptado
#' durante esa corrida; no se guarda estado global asociado a la conexión.
#'
#' @section Instrumentación:
#' `resumen_tabla$sql` conserva una fila por métrica y agrega la duración de la
#' consulta que la respalda, las filas devueltas y los bytes que ese resultado
#' ocupa en R. `consulta_id` identifica el intento dentro de la corrida y
#' `duracion_ms` es la duración de la consulta, repetida en cada fila que esa
#' consulta produjo; no es una duración por métrica. La columna `nivel` marca
#' qué filas se pueden sumar: la primera fila de cada `consulta_id` queda en
#' `nivel = 1` y sus repeticiones en `nivel = 2`. Por eso una suma segura usa
#' sólo `duracion_ms[nivel == 1]` (con `na.rm = TRUE` si corresponde), no la
#' columna completa.
#' `id_consulta` identifica la consulta de datos que produjo la medición: dos
#' métricas con el mismo identificador vieron exactamente las mismas filas y se
#' pueden comparar directamente. `NA` declara que esa garantía no se puede
#' hacer; en particular, las métricas por columna —moda, frecuencia de la moda
#' y mediana— no comparten filas con otras métricas. `etapa` permite agruparlo
#' (`conteos`, `moda`, `basicos`, `mediana`,
#' `desvio`, `lectura_muestra` y las sondas). Las métricas no solicitadas o que
#' no emitieron consulta conservan esos campos y los dejan en `NA`; en
#' particular, `NA` no significa cero.
#'
#' `resumen_tabla$sql$memoria_trabajo` agrega el eje del estado de trabajo del
#' motor: se deriva en orden como `NA` para una fila sin medición, `acotado`
#' para una muestra con `fraccion < 1` y, en los demás alcances efectivos,
#' según el método resuelto del registro; una muestra saturada (`fraccion = 1`)
#' cae a este último caso porque su tope es vacuo y se clasifica como tabla
#' completa. Sus únicos valores son
#' `"creciente"`, `"acotado"` y `NA`.
#'
#' En PostgreSQL, con `instrumentar = TRUE`, se toma una foto de
#' `pg_stat_statements` antes y después de la ventana que cubre los agregados
#' exactos instrumentables de distintos, moda y mediana. Las firmas se filtran
#' por texto y se normalizan en el paquete: cada literal numérico o de cadena
#' se reemplaza por su parámetro posicional y los identificadores entre comillas
#' dobles se conservan. Sólo se publica un derrame cuando una consulta coincide
#' y su contador aumentó en al menos una llamada dentro de la ventana. En ese caso,
#' `resumen_tabla$sql` agrega `derrame`,
#' `bloques_temporales_leidos`, `bloques_temporales_escritos` y
#' `fuente_derrame`, además de `llamadas_en_ventana`; `resumen_tabla$meta$derrame`
#' conserva el resumen y la fuente. Cuando el contador aumentó más de una vez,
#' el motivo declara que los bloques son un agregado de ese texto exacto y
#' pueden incluir otra sesión o llamada concurrente. Si la extensión no está
#' disponible o la instrumentación está apagada, el estado queda
#' `no_disponible` o `no_medido`: el paquete no deduce un derrame del tiempo ni
#' modifica `work_mem`.
#'
#' `resumen_tabla$meta$estimacion_derrame` es un diagnóstico distinto: conserva
#' la estimación de memoria y siempre la rotula como no medida. Puede quedar
#' parcial o no disponible por permisos, falta de `ANALYZE`, particiones sin
#' estadísticas o un motor que no sea PostgreSQL. Si ambos diagnósticos existen,
#' `meta$derrame` es la evidencia posterior y prevalece sobre la estimación;
#' una estimación que no superó el límite no contradice un derrame medido.
#' `meta$estimacion_derrame_moda` y `meta$estimacion_derrame_mediana` siguen la
#' misma regla, con el método del plan de la moda y el sort de la mediana;
#' `n_validos_medido` sólo aparece con valor cuando la familia `validos` fue
#' solicitada y sus agregados planos lo pudieron medir.
#'
#' `resumen_tabla$tiempos` reúne las etapas grandes del cliente en las mismas
#' unidades (`duracion_ms`): `lectura_muestra`, `perfilado_muestra`,
#' `perfilado_columnas`, y los análisis opcionales cuando se solicitan. Una
#' etapa apagada queda con estado `no_solicitado`; si la instrumentación se
#' apaga queda con estado `no_medido` y duración `NA`. La columna `nivel` dice cuáles se
#' pueden sumar: las de `nivel = 1` son disjuntas entre sí, y las de nivel mayor
#' están contenidas en alguna de ellas. `perfilado_muestra` es inclusivo
#' -contiene el perfilado por columna, las dependencias y los casi-duplicados-,
#' así que sumar la columna entera da más que la corrida.
#'
#' @section Datos personales:
#' `proteger_datos_personales` viaja en `...` hacia [perfilar()] y vale `TRUE`
#' por omisión. La protección alcanza a los dos bloques: en `resumen_tabla` se
#' reemplaza la moda de las columnas clasificadas como personales, se ocultan
#' sus estadísticos de orden y sus momentos, se sanean los datos de conexión y
#' el SQL guardado no contiene ningún valor derivado de los datos. La
#' clasificación se toma del perfil de la muestra; si la muestra no se pudo
#' leer, la protección se aplica a todas las columnas y `meta` lo declara.
#' `incluir_valores = FALSE` va más lejos: no emite las consultas de moda ni de
#' mediana y no informa mínimo ni máximo, útil cuando la tabla es un padrón y
#' la moda de un identificador único es un documento real.
#'
#' @param conexion Conexión abierta compatible con DBI.
#' @param tabla Nombre de tabla o un objeto aceptado por
#'   [DBI::dbQuoteIdentifier()].
#' @param universo Universo sobre el que se calculan los agregados SQL:
#'   `"tabla_completa"` (por omisión) o `"muestra_motor"`. En el segundo caso,
#'   todas las métricas SQL usan la relación muestreada por el motor y no se
#'   reemplazan silenciosamente por resultados de la tabla completa.
#' @param muestra_motor Cantidad positiva y finita de filas que el motor debe
#'   tomar cuando `universo = "muestra_motor"`. Es obligatorio en ese universo y
#'   debe quedar `NULL` para `"tabla_completa"`; la función rechaza temprano
#'   valores no enteros, no positivos o `Inf`.
#' @param muestra Cantidad positiva de filas solicitadas para el perfil de
#'   muestra que se trae a R, o `Inf` para traer la tabla entera. Este límite es
#'   independiente de `universo`: en `muestra_motor`, `muestra_motor` decide las
#'   filas del resumen SQL y `muestra` decide las filas del bloque
#'   `perfil_muestra`. En `tabla_completa`, `muestra` no cambia los agregados.
#'
#'   Sin `orden_muestra`, las filas del bloque en R no son una muestra aleatoria
#'   garantizada sino las primeras que devuelva el motor. El límite también
#'   alcanza la muestra común con que se buscan dependencias. Use un entero
#'   finito para acotar ese trabajo cuando el tiempo no sea la restricción.
#' @param max_celdas_muestra Máximo de celdas que puede contener el bloque
#'   `perfil_muestra`. Por defecto es `1000000`; se calcula antes de leer como
#'   filas por columnas del esquema. Si reduce la muestra,
#'   `perfil_muestra$cobertura_diagnosticos` usa la misma declaración que
#'   `perfilar()`, con las celdas solicitadas, el umbral y el tope que mandó.
#'   `Inf` desactiva este tope. No modifica los agregados SQL.
#' @param max_bytes_muestra Máximo de bytes de la muestra materializada que
#'   alimenta `perfil_muestra`. Por defecto es `512 MiB`. Como el tamaño no se
#'   conoce desde el esquema, primero se lee una sonda de hasta cien filas y con
#'   ella se fija el límite final en SQL o en `dbFetch(n)`, antes de leer el
#'   resto. Si reduce la muestra, `cobertura_diagnosticos` informa los bytes
#'   observados, el umbral y cuál tope mandó. `Inf` desactiva este tope. No
#'   modifica los agregados SQL.
#' @param max_bytes_procesamiento Límite de bytes del estado retenido por el
#'   procesamiento por bloques y por la lectura del spool en R. Se comprueba
#'   antes de publicar un bloque o una pasada; `Inf` desactiva este límite.
#' @param max_bytes_materializacion Presupuesto de bytes del spool externo de
#'   `muestra_motor`. Se mide por chunk antes de cada escritura, dejando espacio
#'   para el trailer; si no alcanza, se publica
#'   `spool_presupuesto_excedido` y
#'   `muestra_inestable:presupuesto_materializacion` sin mezclar una muestra
#'   parcial con resultados completos. `Inf` desactiva este límite.
#' @param bloque_filas En la via I1, entero positivo que activa el recorrido de
#'   `tabla_completa` por bloques. Se abre un unico result set con
#'   `dbSendQuery()` y cada bloque se lee con `dbFetch(n = bloque_filas)` antes
#'   de liberar la entrada; `NULL` conserva la via existente. El resultado
#'   publica `meta$alcance$orden`, `meta$bloques`, `meta$bytes` y los eventos del
#'   vigilante. Los drivers sin retencion incremental demostrada (incluido
#'   DuckDB en esta matriz) quedan `no_disponible` con su motivo, sin caer en
#'   `dbGetQuery()`. Si se pide `moda` o `mediana`, I1 inicia tambien el mapa
#'   central de distintos aunque `n_distintos` no se haya pedido: la mediana
#'   depende de ese mapa y la fila `n_distintos` conserva `no_solicitado`.
#'   Bajo `bloque_filas`, `perfil_muestra` sigue siendo la muestra diagnóstica
#'   acotada por `muestra`, `max_celdas_muestra` y `max_bytes_muestra`; su
#'   `general$filas` no es la cobertura, que se publica en
#'   `meta$bloques$filas_vistas`.
#' @param orden_muestra Columnas para `ORDER BY`. La salida solo declara orden
#'   reproducible cuando la combinación es única en toda la tabla. Sin este
#'   argumento, DBI no garantiza el orden ni la pertenencia de una muestra
#'   limitada, y `meta` lo declara expresamente. No se usa cuando
#'   `bloque_muestra = "solo_agregados"`. En la via I1, la identidad de la
#'   fuente por bloques gobierna el recorrido y este pedido queda declarado en
#'   `meta$orden_muestra` con el motivo estable de que no gobierna esa via.
#' @section Progreso:
#' Traer la tabla entera puede tardar minutos sobre una tabla grande, y una
#' corrida callada no se distingue de una colgada. En una sesión interactiva se
#' muestra una barra que avanza contra las consultas que el plan dice que se van
#' a emitir —un total conocido, no una estimación—, y no aparece cuando la
#' corrida es de menos de una docena de consultas, porque termina antes de que
#' sirva.
#'
#' `options(lupa.progreso = TRUE)` la fuerza y `FALSE` la apaga; fuera de una
#' sesión interactiva está apagada, para que la salida de un guion no traiga
#' ruido que despues haya que filtrar. No cambia ningún valor de lo que se mide:
#' hay una prueba que compara el perfil con la barra y sin ella.
#'
#' @param estrategia_mediana Preferencia para resolver `mediana`: `"exacta"`
#'   (por omisión) o `"aproximada_motor"`. La sonda prueba siempre primero una
#'   forma nativa exacta consolidada, luego una forma exacta por columna y deja
#'   las funciones nativas aproximadas para el final. Por eso esta opción
#'   describe la estrategia habilitada, no garantiza el método ejecutado:
#'   `meta$estrategia_mediana` y `resumen_tabla$sql$metodo` publican el método
#'   que efectivamente corrió. Una mediana resuelta por una forma exacta queda
#'   `estado = "calculado"` y `error_esperado = "no_aplica"`, aunque se haya
#'   pedido `"aproximada_motor"`; sólo una aproximación ejecutada queda
#'   `estado = "estimado"`. `universo = "muestra_motor"` combinado con
#'   `"aproximada_motor"` se rechaza temprano.
#' @param metricas Selección explícita de grupos de métricas: `"validos"`,
#'   `"distintos"`, `"moda"`, `"basicos"`, `"mediana"` y `"desvio"`. El
#'   valor por omisión solicita las seis. Para traducir presets de versiones
#'   anteriores: `seguro` equivale a
#'   `c("validos", "basicos", "desvio")` y `conteos` equivale a
#'   `"validos"`; `exacto` equivale a los valores por omisión de esta firma.
#' @param estrategia_distintos Procedencia explícita para `n_distintos`:
#'   `"exacta"` (por omisión) emite `COUNT(DISTINCT)`; `"aproximada_motor"`
#'   usa una función nativa aceptada por el motor y deja la métrica en
#'   `no_disponible` si no existe; `"catalogo"` lee
#'   `pg_stats.n_distinct` en PostgreSQL y publica el resultado como
#'   `estimado_catalogo`, nunca como medición, cuando
#'   `universo = "tabla_completa"`. En `muestra_motor` queda `no_disponible`,
#'   porque el catálogo describe la relación entera y la corrida mide un
#'   subconjunto; y
#'   `"omitida"` no emite ninguna consulta. No hay repliegue automático entre
#'   estrategias. El resultado publica `estrategia_solicitada`,
#'   `estrategia_resuelta` y `estado` en `meta$estrategia_distintos`, y las dos
#'   primeras también en `resumen_tabla$sql`. En `pg_stats`, un valor positivo
#'   es el conteo estimado y uno negativo es una fracción de las filas. Cuando la
#'   relación tiene descendientes se elige `inherited = TRUE`, porque esa fila
#'   describe lo que lee una consulta sin `ONLY`; una relación sin hijas usa su
#'   única fila propia. Las fracciones se convierten con la suma de
#'   `pg_class.reltuples` de la jerarquía. Si no hay una fila utilizable —por
#'   ejemplo, antes de `ANALYZE`— o hay ambigüedad, la métrica queda
#'   `no_disponible`, no en cero.
#' @param max_consultas Presupuesto declarado de consultas. Al agotarse, las
#'   métricas restantes quedan en `no_disponible` con ese motivo.
#' @param tamano_lote Cantidad máxima de columnas por consulta consolidada.
#'   Se conserva por compatibilidad y, si se informa, fija el tamaño de las dos
#'   familias. Para control separado, usar `tamano_lote_planos` y
#'   `tamano_lote_distintos`.
#' @param tamano_lote_planos Cantidad máxima de columnas por consulta de
#'   agregados planos. El valor por omisión es 20.
#' @param tamano_lote_distintos Cantidad máxima de columnas por consulta de
#'   cardinalidades exactas. El valor por omisión es 2, medido sobre el servidor
#'   de referencia: el `Shared Read` fue constante entre lotes y el costo por
#'   columna fue casi igual para uno y dos, mientras el lote de dos derramó
#'   menos que los lotes mayores. Una sola cardinalidad todavía puede forzar un
#'   agregado pesado y derramar mucho más que un lote plano.
#' @param dialecto Capacidad de acotar filas: `"auto"` la sondea, y
#'   `"limit"`, `"top"`, `"fetch_first"`, `"rownum"` o `"portable"` la
#'   declaran sin sondeo.
#' @param incluir_valores Si el resumen informa valores de celda: moda, mínimo,
#'   máximo y mediana. Con `FALSE` esas consultas no se emiten.
#' @param politica_costo Política optativa para las métricas caras. El valor por
#'   omisión, `"todas"`, conserva moda y mediana para todas las columnas
#'   solicitadas. `"por_cardinalidad"` resuelve primero las fuentes
#'   estructurales y mide valores válidos y distintos sólo cuando hace falta y
#'   la estrategia lo permite. Luego omite, por columna, sólo la moda cuando la
#'   proporción de distintos alcanza `umbral_cardinalidad`; la mediana se
#'   conserva porque las mediciones disponibles muestran que su costo depende de
#'   las filas y no de la cardinalidad. Los únicos valores aceptados son
#'   `"todas"` y `"por_cardinalidad"`; no hay alias históricos.
#' @param umbral_cardinalidad Proporción entre valores distintos y válidos que
#'   activa la omisión de la moda con `politica_costo = "por_cardinalidad"`. El
#'   valor por omisión es `0.5` sólo cuando esa política se pide explícitamente;
#'   se puede mover en cada llamada. Este argumento no gobierna la mediana:
#'   `meta$decisiones_costo` explica la decisión de cada métrica por separado.
#'   Para pedir todas las métricas use `politica_costo = "todas"`.
#' @param avisar_costo_distintos Si es `TRUE`, avisa, después de medir el primer
#'   lote y antes del segundo, cuando la proyección del costo de
#'   `COUNT(DISTINCT)` alcanza `umbral_segundos_aviso_distintos`. Por omisión es
#'   `TRUE`. Este aviso no depende de `interactive()`: también llega en guiones
#'   porque el costo se paga en el servidor y puede durar decenas de segundos.
#' @param umbral_segundos_aviso_distintos Segundos estimados a partir de los
#'   cuales se emite el aviso del costo de `COUNT(DISTINCT)`, después del primer
#'   lote. Por omisión es `30`, el umbral histórico; `Inf` lo desactiva
#'   explícitamente. Con un solo lote no se publica una proyección porque el
#'   costo ya se pagó. El valor no cambia la proyección ni la medición que se
#'   publica.
#' @param avisar_costo_moda Si es `TRUE`, avisa antes de ejecutar las modas
#'   pendientes cuando su proyección alcanza `umbral_segundos_aviso_moda`. Por
#'   omisión es `TRUE`. La tasa se mide con modas anteriores de esta corrida;
#'   si falta cardinalidad, se declara en la proyección y no se supone.
#' @param umbral_segundos_aviso_moda Segundos estimados a partir de los cuales
#'   se emite el aviso de la moda. Por omisión es `30`; `Inf` lo desactiva
#'   explícitamente. El valor no cambia la medición ni la proyección publicada.
#' @param avisar_costo_mediana Si es `TRUE`, avisa antes de ejecutar las
#'   medianas pendientes cuando su proyección alcanza
#'   `umbral_segundos_aviso_mediana`. Por omisión es `TRUE`. La proyección sigue
#'   las filas, no la cardinalidad.
#' @param umbral_segundos_aviso_mediana Segundos estimados a partir de los cuales
#'   se emite el aviso de la mediana. Por omisión es `30`; `Inf` lo desactiva
#'   explícitamente. Cuando no existe una primera medición local, una sola
#'   mediana total usa la referencia de banco declarada de 68 ms por millón de
#'   filas de otra corrida. Si la consulta inicial que obtuvo las filas fue
#'   medida y da una cota mayor, se publica como cota de lectura, no como
#'   medición de mediana.
#' @param avisar_derrame_estimado Si es `TRUE`, avisa cuando un lote de
#'   `COUNT(DISTINCT)` supera la memoria efectiva y su tamaño estimado alcanza
#'   `umbral_bytes_aviso_derrame_estimado`. Por omisión es `TRUE`. Este aviso
#'   sólo puede aparecer cuando PostgreSQL permite estimar el hash.
#' @param umbral_bytes_aviso_derrame_estimado Tamaño estimado del hash, en
#'   bytes, a partir del cual se avisa un derrame potencial entre los lotes que
#'   ya superan la memoria efectiva. Por omisión es `0`, que conserva el aviso
#'   para cualquier lote que la supere; `Inf` lo desactiva explícitamente. El
#'   valor no cambia la estimación ni la medición posterior del derrame.
#' @param bloque_muestra Qué bloques se solicitan: `"con_muestra"` (por
#'   omisión) calcula también `perfil_muestra`, o `"solo_agregados"` omite su
#'   lectura y devuelve sólo los agregados SQL. La segunda opción no cambia el
#'   alcance de esos agregados: eso lo decide `universo`.
#' @param instrumentar Si se cronometra cada consulta y las etapas grandes de R
#'   y, en PostgreSQL, se intenta atribuir el uso de bloques temporales de los
#'   `COUNT(DISTINCT)` exactos mediante `pg_stat_statements`.
#'   Por omisión es `TRUE`; agrega `duracion_ms`, `cpu_ms`,
#'   `n_filas_resultado`, `bytes_resultado_r`, `consulta_id`, `etapa` y `nivel`
#'   a `resumen_tabla$sql`, y el resumen `resumen_tabla$tiempos`. Con `FALSE` se
#'   conserva el mismo plan, la misma cantidad y el mismo orden de consultas,
#'   pero los campos medibles quedan en `NA`. `id_consulta` **no** depende de
#'   esta opcion: no es una medicion sino un hecho estructural sobre que
#'   consulta produjo cada metrica, y se publica igual con `FALSE`. Las
#'   duraciones usan `Sys.time()` y el CPU del cliente usa la suma de
#'   `proc.time()[c("user.self", "sys.self")]`. `cpu_ms` es cero cuando el
#'   proceso no consumió CPU; `NA` significa que no se pudo medir. Los
#'   intervalos que el reloj no puede resolver no se publican como cero.
#' @param ... Argumentos enviados a [perfilar()] para analizar la muestra.
#'
#' @return Objeto de clase `perfil_dbi` con dos componentes: `resumen_tabla`, de
#'   alcance completo o muestreado según `universo`, y `perfil_muestra`, un objeto
#'   `perfil` cuyo `meta$origen_dbi` declara tabla, conexión, SQL y alcance.
#'   `perfil_muestra` es `NULL` si la muestra no se pudo obtener o si se pidió
#'   `bloque_muestra = "solo_agregados"`; `resumen_tabla$cobertura` distingue
#'   esos casos con `no_disponible` y `no_solicitado`, respectivamente.
#'   `resumen_tabla$meta$clave` conserva siempre la respuesta del catálogo de la
#'   clave primaria: `columnas`, `fuente`, `motivo`, `garantia` y `estado`.
#'   `garantia` puede ser `garantizada`, `declarada_no_garantizada`,
#'   `desconocida` o `no_declarada`; `estado` conserva, cuando el motor los
#'   expone, `visible`, `restriccion_diferible`,
#'   `universo_incluye_descendientes` e `indice_no_unico`. Una consulta fallida
#'   queda diferenciada de una clave no declarada.
#' @export
#' @seealso [plan_perfilado_dbi()], [perfilar()]
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
#'   resultado <- perfilar_dbi(con, "ejemplo", muestra = 5, orden_muestra = "id")
#'   DBI::dbDisconnect(con)
#' }
perfilar_dbi <- function(conexion, tabla,
                         universo = c("tabla_completa", "muestra_motor"),
                         muestra_motor = NULL, muestra = Inf,
                         orden_muestra = NULL,
                         metricas = .METRICAS_DBI,
                         estrategia_distintos = "exacta",
                         estrategia_mediana = c("exacta", "aproximada_motor"),
                         politica_costo = c("todas", "por_cardinalidad"),
                         bloque_muestra = c("con_muestra", "solo_agregados"),
                         max_consultas = Inf,
                         dialecto = "auto", incluir_valores = TRUE,
                         tamano_lote = NULL,
                         tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                         tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                         instrumentar = TRUE,
                         umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
                         avisar_costo_distintos = TRUE,
                         umbral_segundos_aviso_distintos =
                           .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI,
                         avisar_costo_moda = TRUE,
                         umbral_segundos_aviso_moda =
                           .UMBRAL_SEGUNDOS_AVISO_MODA_DBI,
                         avisar_costo_mediana = TRUE,
                         umbral_segundos_aviso_mediana =
                           .UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI,
                         avisar_derrame_estimado = TRUE,
                         umbral_bytes_aviso_derrame_estimado =
                           .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI,
                         max_celdas_muestra = .MAX_CELDAS_MUESTRA,
                         max_bytes_muestra = .MAX_BYTES_MUESTRA,
                         bloque_filas = NULL,
                         max_bytes_procesamiento = .MAX_BYTES_MUESTRA,
                         max_bytes_materializacion = .MAX_BYTES_MUESTRA,
                         ...) {
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  max_celdas_muestra <- .validar_limite_duplicados(
    max_celdas_muestra, "max_celdas_muestra"
  )
  max_bytes_muestra <- .validar_limite_duplicados(
    max_bytes_muestra, "max_bytes_muestra"
  )
  max_bytes_procesamiento <- .validar_presupuesto_bytes_dbi(
    max_bytes_procesamiento, "max_bytes_procesamiento"
  )
  max_bytes_materializacion <- .validar_presupuesto_bytes_dbi(
    max_bytes_materializacion, "max_bytes_materializacion"
  )
  avisar_costo_distintos <- .validar_interruptor_aviso_dbi(
    avisar_costo_distintos, "avisar_costo_distintos"
  )
  umbral_segundos_aviso_distintos <- .validar_umbral_aviso_dbi(
    umbral_segundos_aviso_distintos, "umbral_segundos_aviso_distintos"
  )
  avisar_costo_moda <- .validar_interruptor_aviso_dbi(
    avisar_costo_moda, "avisar_costo_moda"
  )
  umbral_segundos_aviso_moda <- .validar_umbral_aviso_dbi(
    umbral_segundos_aviso_moda, "umbral_segundos_aviso_moda"
  )
  avisar_costo_mediana <- .validar_interruptor_aviso_dbi(
    avisar_costo_mediana, "avisar_costo_mediana"
  )
  umbral_segundos_aviso_mediana <- .validar_umbral_aviso_dbi(
    umbral_segundos_aviso_mediana, "umbral_segundos_aviso_mediana"
  )
  avisar_derrame_estimado <- .validar_interruptor_aviso_dbi(
    avisar_derrame_estimado, "avisar_derrame_estimado"
  )
  umbral_bytes_aviso_derrame_estimado <- .validar_umbral_aviso_dbi(
    umbral_bytes_aviso_derrame_estimado,
    "umbral_bytes_aviso_derrame_estimado"
  )
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, universo = universo,
    muestra_motor = muestra_motor, muestra = muestra,
    orden_muestra = orden_muestra,
    estrategia_mediana = estrategia_mediana, metricas = metricas,
    max_consultas = max_consultas, dialecto = dialecto,
    tamano_lote = tamano_lote,
    tamano_lote_planos = tamano_lote_planos,
    tamano_lote_distintos = tamano_lote_distintos,
    bloque_muestra = bloque_muestra, instrumentar = instrumentar, contar = FALSE,
    contar_muestreo = TRUE,
    incluir_valores = incluir_valores,
    estrategia_distintos = estrategia_distintos,
    politica_costo = politica_costo,
    umbral_cardinalidad = umbral_cardinalidad,
    max_bytes_procesamiento = max_bytes_procesamiento,
    max_bytes_materializacion = max_bytes_materializacion
  )
  if (identical(preparacion$universo, "muestra_motor")) {
    return(.perfil_muestra_spool_dbi(
      conexion = conexion, tabla = tabla, preparacion = preparacion,
      incluir_valores = incluir_valores, bloque_filas = bloque_filas,
      max_bytes_procesamiento = max_bytes_procesamiento,
      max_bytes_materializacion = max_bytes_materializacion,
      argumentos = list(...)
    ))
  }
  preparacion$presupuesto$avisar_derrame_estimado <- avisar_derrame_estimado
  preparacion$presupuesto$umbral_bytes_aviso_derrame_estimado <-
    umbral_bytes_aviso_derrame_estimado
  if (!is.null(bloque_filas)) {
    return(.perfilar_dbi_bloques(
      conexion = conexion, tabla = tabla, preparacion = preparacion,
      metricas = preparacion$metricas_ejecucion,
      incluir_valores = incluir_valores, bloque_filas = bloque_filas,
      max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra,
      argumentos = list(...),
      max_bytes_procesamiento = max_bytes_procesamiento,
      metricas_publicas = preparacion$metricas
    ))
  }
  presupuesto <- preparacion$presupuesto
  presupuesto$avisar_costo_distintos <- avisar_costo_distintos
  presupuesto$umbral_segundos_aviso_distintos <-
    umbral_segundos_aviso_distintos
  presupuesto$avisar_costo_moda <- avisar_costo_moda
  presupuesto$umbral_segundos_aviso_moda <- umbral_segundos_aviso_moda
  presupuesto$avisar_costo_mediana <- avisar_costo_mediana
  presupuesto$umbral_segundos_aviso_mediana <- umbral_segundos_aviso_mediana
  presupuesto$avisar_derrame_estimado <- avisar_derrame_estimado
  presupuesto$umbral_bytes_aviso_derrame_estimado <-
    umbral_bytes_aviso_derrame_estimado
  info_conexion <- .info_conexion_dbi(conexion)
  es_numerico <- vapply(seq_along(preparacion$campos), function(i) {
    .es_numerico_dbi(
      preparacion$prototipo[[i]],
      if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  tope_muestra <- .tope_muestra_plan_dbi(
    preparacion$muestra, length(preparacion$campos),
    max_celdas_muestra, max_bytes_muestra
  )
  plan <- .plan_consultas_dbi(
    preparacion$campos, es_numerico, preparacion$metricas_ejecucion, incluir_valores,
    length(preparacion$orden_sql) > 0 &&
      identical(preparacion$bloque_muestra, "con_muestra"), preparacion$dialecto,
    emitidas = presupuesto$usadas, universo = preparacion$universo,
    muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
      preparacion$muestreo$disponible,
    tamano_lote_planos = preparacion$tamano_lote_planos,
    tamano_lote_distintos = preparacion$tamano_lote_distintos,
    columnas_distintos = preparacion$columnas_distintos_ejecucion,
    incluir_muestra = identical(preparacion$bloque_muestra, "con_muestra"),
    mediana_consolidada = !is.null(preparacion$mediana_consolidada),
    consultas_muestra = if (identical(
      preparacion$bloque_muestra, "con_muestra"
    ) && isTRUE(tope_muestra$requiere_sonda_bytes)) 2L else 1L
  )
  # Las consultas obligatorias que faltan -verificacion de orden y, cuando se
  # pidio, muestra- se reservan para que el presupuesto no se las coma.
  presupuesto$reserva <- if (identical(preparacion$bloque_muestra, "con_muestra")) {
    (if (length(preparacion$orden_sql)) 2 else 1) +
      if (isTRUE(tope_muestra$requiere_sonda_bytes)) 1 else 0
  } else {
    0
  }
  # El plan ya dice cuantas consultas se van a emitir, asi que la barra avanza
  # contra un total conocido y no contra una estimacion. Se abre en el marco de
  # esta funcion para que viva lo que dure el perfilado y se cierre sola.
    .abrir_progreso_dbi(
    presupuesto, sum(plan$n_consultas_max), environment()
  )

  fuente_muestreada <- NULL
  muestreo_meta <- preparacion$muestreo
  muestreo_publico <- if (is.null(preparacion$muestreo)) {
    NULL
  } else {
    .publicar_muestreo_dbi(preparacion$muestreo, n_total = preparacion$n_total)
  }
  # `NEWID()` es un respaldo de la fuente muestreada, no una puerta para
  # ejecutar primero un ordenamiento completo y decidir despues si costaba
  # demasiado. `n_total` se conto arriba para los caminos aleatorios; la
  # politica queda en el presupuesto para que estrategia, fila y metadata
  # compartan exactamente la misma decision.
  if (identical(preparacion$universo, "muestra_motor") &&
      !is.null(preparacion$muestreo) &&
      identical(preparacion$muestreo$candidato$nombre, "random_limit") &&
      length(preparacion$muestreo$candidato$funciones) == 1L &&
      identical(preparacion$muestreo$candidato$funciones[[1L]]$nombre, "newid") &&
      "mediana" %in% preparacion$metricas_ejecucion &&
      isTRUE(incluir_valores) && any(preparacion$es_numerico)) {
    pendientes_newid <- sum(preparacion$es_numerico)
    guardia_newid <- .evaluar_guardia_newid_dbi(
      preparacion$n_total, preparacion$muestra_motor, pendientes_newid
    )
    presupuesto$guardia_newid <- guardia_newid
    if (!isTRUE(guardia_newid$aceptado)) {
      if (isTRUE(avisar_costo_mediana)) {
        cli::cli_alert_warning(paste0(
          "Mediana muestreada no disponible: ", guardia_newid$motivo,
          ". Proyeccion NEWID: ",
          if (is.finite(guardia_newid$proyeccion_newid_ms)) {
            formatC(guardia_newid$proyeccion_newid_ms,
                    format = "f", digits = 1)
          } else "sin dato",
          " ms; n_total = ", .entero_sql_dbi(guardia_newid$n_total),
          ", fraccion = ", formatC(guardia_newid$fraccion,
                                    format = "f", digits = 3), "."
        ))
      }
      preparacion$muestreo$disponible <- FALSE
      preparacion$muestreo$motivo <- guardia_newid$motivo
      muestreo_publico <- .publicar_muestreo_dbi(
        preparacion$muestreo, n_total = preparacion$n_total
      )
    }
  }
  tabla_metricas_sql <- preparacion$tabla_sql
  if (identical(preparacion$universo, "muestra_motor") &&
      !is.null(preparacion$muestreo) &&
      isTRUE(preparacion$muestreo$disponible)) {
    fuente_muestreada <- .fuente_muestreada_dbi(
      preparacion$tabla_sql, preparacion$campos_sql, preparacion$muestra_motor,
      preparacion$n_total, preparacion$dialecto, preparacion$muestreo
    )
    if (is.null(fuente_muestreada)) {
      preparacion$muestreo$disponible <- FALSE
      preparacion$muestreo$motivo <- paste(
        "La forma muestreada resuelta no pudo construir una consulta de",
        "subconjunto compatible con el dialecto elegido."
      )
      muestreo_publico <- .publicar_muestreo_dbi(
        preparacion$muestreo, n_total = preparacion$n_total
      )
    } else {
      tabla_metricas_sql <- paste0(
        "(", fuente_muestreada$sql, ")",
        preparacion$dialecto$alias_tabla("lupa_muestra")
      )
      muestreo_meta <- c(
        preparacion$muestreo,
        list(
          metodo = fuente_muestreada$metodo,
          descripcion = fuente_muestreada$descripcion,
          fraccion = fuente_muestreada$fraccion,
          tamano_muestra = fuente_muestreada$filas_solicitadas,
          sql = fuente_muestreada$sql
        )
      )
      muestreo_publico <- .publicar_muestreo_dbi(
        preparacion$muestreo, fuente_muestreada, preparacion$n_total
      )
    }
  }
  if (identical(preparacion$universo, "muestra_motor") &&
      !is.null(preparacion$muestreo) &&
      !isTRUE(preparacion$muestreo$disponible)) {
    # Mantener una relacion vacia permite que el resumen conserve su forma y
    # el conteo del universo, pero evita que un rechazo de capacidad vuelva a
    # caer accidentalmente sobre `tabla_sql` completa.
    tabla_metricas_sql <- paste0(
      "(SELECT ", paste(preparacion$campos_sql, collapse = ", "),
      " FROM ", preparacion$tabla_sql, " WHERE 1 = 0)",
      preparacion$dialecto$alias_tabla("lupa_muestra")
    )
  }

  campos_todos <- unique(c(preparacion$campos, preparacion$esquema$ilegibles))
  trazador <- .trazador_tiempos_dbi(preparacion$instrumentar)
  consultas_antes_resumen <- presupuesto$usadas
  resumen <- .resumen_tabla_dbi(
    conexion, tabla, preparacion$tabla_sql, campos_todos,
    preparacion$prototipo, preparacion$n_total, preparacion$sql_conteo,
    info_conexion, dialecto = preparacion$dialecto,
    metricas = preparacion$metricas, presupuesto = presupuesto,
    metricas_ejecucion = preparacion$metricas_ejecucion,
    incluir_valores = incluir_valores, tipos_declarados = preparacion$tipos,
    motivos_ilegibles = preparacion$esquema$motivos,
     universo = preparacion$universo,
     estrategia_mediana = preparacion$estrategia_mediana,
     tabla_metricas_sql = tabla_metricas_sql,
     muestreo = muestreo_publico, aproximaciones = preparacion$aproximaciones,
     tamano_muestra = if (is.null(fuente_muestreada)) NA_real_ else
       fuente_muestreada$filas_solicitadas,
     fraccion_muestra = if (is.null(fuente_muestreada)) NA_real_ else
       fuente_muestreada$fraccion,
     campos_consolidados = preparacion$campos,
     campos_sql_consolidados = preparacion$esquema$campos_sql,
     es_numerico_consolidados = preparacion$es_numerico,
    tamano_lote_planos = preparacion$tamano_lote_planos,
    tamano_lote_distintos = preparacion$tamano_lote_distintos,
    conteo = preparacion$conteo,
    moda_guardian = preparacion$moda_guardian,
    mediana_consolidada = preparacion$mediana_consolidada,
    mediana_escalar = preparacion$mediana_escalar,
    mediana_cte_ventana = preparacion$mediana_cte_ventana,
    mediana_cte_ventana_motivo = preparacion$mediana_cte_ventana_motivo,
    fuentes_cardinalidad_costo = preparacion$fuentes_cardinalidad_costo,
    estrategia_distintos = preparacion$estrategia_distintos,
    politica_costo = preparacion$politica_costo
  )
  derrame <- .publicar_derrame_dbi(presupuesto)
  resumen$sql <- .adjuntar_derrame_sql_dbi(
    resumen$sql, presupuesto$derrame
  )
  resumen$meta$derrame <- derrame
  resumen$meta$estimacion_derrame_moda <- presupuesto$estimacion_derrame_moda
  resumen$meta$estimacion_derrame_mediana <- presupuesto$estimacion_derrame_mediana
  resumen$meta$costo_distintos <- presupuesto$proyeccion_distintos
  resumen$meta$costo_moda <- presupuesto$proyeccion_moda
  resumen$meta$costo_mediana <- presupuesto$proyeccion_mediana
  if (identical(preparacion$politica_costo$nombre, "por_cardinalidad")) {
    decisiones <- resumen$meta$decisiones_costo
    columnas_moda <- names(decisiones)[vapply(
      decisiones, function(x) isTRUE(x$moda), logical(1L)
    )]
    columnas_mediana <- names(decisiones)[vapply(
      decisiones, function(x) isTRUE(x$mediana), logical(1L)
    )]
    columnas_mediana <- intersect(
      columnas_mediana, preparacion$campos[preparacion$es_numerico]
    )
    plan <- .plan_consultas_dbi(
      preparacion$campos, es_numerico, preparacion$metricas_ejecucion,
      incluir_valores,
      length(preparacion$orden_sql) > 0 &&
        identical(preparacion$bloque_muestra, "con_muestra"),
      preparacion$dialecto, emitidas = consultas_antes_resumen,
      universo = preparacion$universo,
      muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
        preparacion$muestreo$disponible,
      tamano_lote_planos = preparacion$tamano_lote_planos,
      tamano_lote_distintos = preparacion$tamano_lote_distintos,
      columnas_distintos = preparacion$columnas_distintos_ejecucion,
      incluir_muestra = identical(preparacion$bloque_muestra, "con_muestra"),
      mediana_consolidada = !is.null(preparacion$mediana_consolidada),
      columnas_moda = columnas_moda, columnas_moda_max = columnas_moda,
      columnas_mediana = columnas_mediana,
      columnas_mediana_max = columnas_mediana,
      consultas_muestra = if (identical(
        preparacion$bloque_muestra, "con_muestra"
      ) && isTRUE(tope_muestra$requiere_sonda_bytes)) 2L else 1L
    )
  }
  attr(plan, "moda_guardian") <- .publicar_moda_guardian_dbi(
    preparacion$moda_guardian_resolucion
  )
  attr(plan, "estimacion_derrame") <- preparacion$estimacion_derrame
  attr(plan, "estimacion_derrame_moda") <- preparacion$estimacion_derrame_moda
  attr(plan, "estimacion_derrame_mediana") <- preparacion$estimacion_derrame_mediana
  # En la corrida el conteo sale de la primera consulta de agregados. Desde
  # aca es el total que gobierna el denominador, la muestra y toda la metadata;
  # no se conserva el valor desconocido de la preparacion.
  preparacion$n_total <- resumen$meta$filas
  if (identical(preparacion$universo, "muestra_motor")) {
    if (!is.null(fuente_muestreada)) {
      fuente_muestreada$filas_solicitadas <- min(
        as.numeric(fuente_muestreada$filas_solicitadas),
        .numero_dbi(preparacion$n_total)
      )
      fuente_muestreada$fraccion <- .fraccion_muestreo_dbi(
        fuente_muestreada$filas_solicitadas, preparacion$n_total
      )
      muestreo_meta <- c(
        preparacion$muestreo,
        list(
          metodo = fuente_muestreada$metodo,
          descripcion = fuente_muestreada$descripcion,
          fraccion = fuente_muestreada$fraccion,
          tamano_muestra = fuente_muestreada$filas_solicitadas,
          sql = fuente_muestreada$sql
        )
      )
      muestreo_publico <- .publicar_muestreo_dbi(
        preparacion$muestreo, fuente_muestreada, preparacion$n_total
      )
    } else {
      muestreo_publico <- .publicar_muestreo_dbi(
        if (is.null(preparacion$muestreo)) {
          list(disponible = FALSE, candidato = NULL, sondas = character(),
               motivo = "No se resolvio una capacidad de muestreo del motor.")
        } else preparacion$muestreo,
        n_total = preparacion$n_total
      )
    }
  }
  resumen$meta$sql_esquema <- preparacion$esquema$sql
  resumen$meta$universo <- preparacion$universo
  resumen$meta$estrategia_mediana <- .publicar_estrategia_mediana_dbi(
    preparacion
  )
  resumen$meta$metricas <- preparacion$metricas
  resumen$meta$metricas_ejecucion <- preparacion$metricas_ejecucion
  resumen$meta$politica_costo <- preparacion$politica_costo
  resumen$meta$estrategia_distintos <- .publicar_estrategia_distintos_dbi(
    preparacion$estrategia_distintos
  )
  resumen$meta$fuente_cardinalidad_costo <-
    preparacion$fuentes_cardinalidad_costo
  resumen$meta$moda_guardian <- .publicar_moda_guardian_dbi(
    preparacion$moda_guardian_resolucion
  )
  resumen$meta$mediana_consolidada <-
    preparacion$mediana_consolidada_resolucion
  resumen$meta$mediana_escalar <-
    .publicar_mediana_escalar_dbi(preparacion$mediana_escalar_resolucion)
  # La salida del catalogo sigue la misma ruta `meta$clave` que la salida en
  # memoria. Se conserva completa para no perder fuente, motivo ni estados.
  resumen$meta$clave <- preparacion$catalogo_cardinalidad
  resumen$meta$incluir_valores <- incluir_valores
  resumen$meta$max_celdas_muestra <- max_celdas_muestra
  resumen$meta$max_bytes_muestra <- max_bytes_muestra
  resumen$meta$tamano_lote <- preparacion$tamano_lote
  resumen$meta$tamano_lote_funciono <- presupuesto$tamano_lote_funciono
  resumen$meta$tamano_lote_planos <- preparacion$tamano_lote_planos
  resumen$meta$tamano_lote_distintos <- preparacion$tamano_lote_distintos
  resumen$meta$tamano_lote_planos_funciono <-
    presupuesto$tamano_lote_planos_funciono
  resumen$meta$tamano_lote_distintos_funciono <-
    presupuesto$tamano_lote_distintos_funciono
  resumen$meta$instrumentacion <- list(
    activa = isTRUE(preparacion$instrumentar),
    unidad_duracion = "ms",
    reloj = "Sys.time()",
    cpu = "proc.time()[['user.self']] + proc.time()[['sys.self']]",
    unidad_cpu = "ms",
    precision = paste(
      "La precision efectiva es la de la plataforma; los intervalos que el",
      "reloj no puede resolver quedan en NA y nunca se publican como cero.",
      "El CPU queda en NA si proc.time() no pudo leerse."
    ),
    apagable = TRUE
  )
  resumen$meta$plan <- plan
  resumen$meta$dialecto <- list(
    nombre = preparacion$dialecto$nombre,
    descripcion = preparacion$dialecto$descripcion,
    motivo = preparacion$resolucion$motivo,
    declarado = preparacion$resolucion$declarado,
    sondas = preparacion$resolucion$sondas
  )
  if (identical(preparacion$estrategia_mediana, "aproximada_motor")) {
    resumen$meta$aproximaciones <- lapply(
      preparacion$aproximaciones_resolucion, .publicar_aproximacion_dbi
    )
  }
  cobertura <- .cobertura_dbi_vacia()
  cobertura <- rbind(
    cobertura,
    .cobertura_cambio_entre_consultas_dbi(
      resumen$columnas, resumen$sql
    )
  )
  if (identical(preparacion$universo, "muestra_motor")) {
    if (!is.null(muestreo_publico) &&
        length(resumen$meta$filas_obtenidas_muestra) == 1L &&
        is.finite(resumen$meta$filas_obtenidas_muestra) &&
        (is.null(muestreo_publico$filas_obtenidas) ||
         is.na(muestreo_publico$filas_obtenidas))) {
      muestreo_publico$filas_obtenidas <-
        resumen$meta$filas_obtenidas_muestra
    }
    resumen$meta$muestreo <- muestreo_publico
    if (!is.null(presupuesto$guardia_newid) &&
        !is.null(resumen$meta$muestreo)) {
      resumen$meta$muestreo$politica_newid <- presupuesto$guardia_newid
    }
    if (is.null(fuente_muestreada)) {
      cobertura <- rbind(cobertura, .registro_cobertura_dbi(
        "resumen_tabla", .texto_tabla_dbi(tabla), "no_disponible",
        if (is.null(preparacion$muestreo)) {
          "No se pudo resolver una capacidad de muestreo del motor."
        } else {
          preparacion$muestreo$motivo
        },
        paste(
          "El universo `muestra_motor` no reemplaza la estimacion por un calculo",
          "sobre la tabla completa. Usar `universo = \"tabla_completa\"` o un",
          "adaptador compatible."
        ),
        NA_character_
      ))
    }
  }
  # Un conteo por encima de 2^53 no sobrevive al doble. Se declara en vez de
  # afirmar una exactitud que no hay.
  resumen$meta$conteo_exacto <- .conteo_exacto_dbi(preparacion$n_total)

  if (length(preparacion$esquema$ilegibles)) {
    for (campo in preparacion$esquema$ilegibles) {
      cobertura <- rbind(cobertura, .registro_cobertura_dbi(
        "resumen_tabla", campo, "no_disponible",
        preparacion$esquema$motivos[[campo]],
        paste(
          "Las demas columnas se midieron enteras. Revisar el tipo de la",
          "columna y los permisos a nivel de columna."
        ),
        preparacion$esquema$motivo_conjunto
      ))
    }
  }
  if (!identical(preparacion$lista_campos$origen, "dbListFields")) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), "degradado",
      preparacion$lista_campos$motivo,
      "No hace falta hacer nada: los nombres se obtuvieron por otra via.",
      NA_character_
    ))
  }

  presupuesto$reserva <- 0
  bloque <- if (identical(preparacion$bloque_muestra, "con_muestra")) {
    .bloque_muestra_dbi(
      conexion, tabla, preparacion$tabla_sql, preparacion$campos,
      preparacion$campos_sql, preparacion$muestra,
      if (identical(preparacion$universo, "muestra_motor")) {
        preparacion$muestra_motor
      } else {
        preparacion$muestra
      },
      preparacion$orden_muestra,
      preparacion$orden_sql, preparacion$dialecto, preparacion$n_total,
      presupuesto, info_conexion, list(...), muestreo = muestreo_meta,
      tipos_declarados = preparacion$tipos, trazador = trazador,
      max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra
    )
  } else {
    .registrar_etapa_dbi(
      trazador, "lectura_muestra", estado = "no_solicitado"
    )
    .registrar_etapa_dbi(
      trazador, "perfilado_muestra", estado = "no_solicitado"
    )
    list(
      perfil = NULL,
      cobertura = .registro_cobertura_dbi(
        "perfil_muestra", .texto_tabla_dbi(tabla), "no_solicitado",
        paste(
          "No se solicito el perfil de muestra: este resultado contiene",
          "unicamente los agregados SQL de la configuracion elegida."
        ),
        paste(
          "Para obtener diagnosticos sobre valores y hallazgos por fila, volver",
          "a perfilar con `bloque_muestra = \"con_muestra\"`."
        )
      ),
      muestreo = NULL
    )
  }
  if (!is.null(muestreo_publico) && !is.null(bloque$muestreo)) {
    muestreo_publico <- .publicar_muestreo_dbi(
      preparacion$muestreo, fuente_muestreada, preparacion$n_total,
      muestreo_meta = bloque$muestreo
    )
  }
  if (identical(preparacion$universo, "muestra_motor")) {
    if (!is.null(muestreo_publico) &&
        length(resumen$meta$filas_obtenidas_muestra) == 1L &&
        is.finite(resumen$meta$filas_obtenidas_muestra) &&
        (is.null(muestreo_publico$filas_obtenidas) ||
         is.na(muestreo_publico$filas_obtenidas))) {
      muestreo_publico$filas_obtenidas <-
        resumen$meta$filas_obtenidas_muestra
    }
    resumen$meta$muestreo <- muestreo_publico
    if (!is.null(presupuesto$guardia_newid) &&
        !is.null(resumen$meta$muestreo)) {
      resumen$meta$muestreo$politica_newid <- presupuesto$guardia_newid
    }
  }
  cobertura <- rbind(cobertura, bloque$cobertura)
  resumen$tiempos <- .resumen_tiempos_dbi(trazador)
  if (isTRUE(presupuesto$agotado)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), "presupuesto_agotado",
      .motivo_presupuesto_dbi(presupuesto),
      paste0(
        "Subir `max_consultas` o reducir el trabajo con `universo`, `metricas` o la estrategia elegida. ",
        "El plan previo esta en `meta$plan` y en plan_perfilado_dbi()."
      ),
      NA_character_
    ))
  }
  rownames(cobertura) <- NULL
  resumen$cobertura <- cobertura
  resumen$meta$consultas <- list(
    emitidas = presupuesto$usadas,
    presupuesto = presupuesto$max,
    agotado = isTRUE(presupuesto$agotado)
  )

  argumentos <- list(...)
  proteger <- is.null(argumentos$proteger_datos_personales) ||
    isTRUE(argumentos$proteger_datos_personales)
  if (proteger) {
    if (is.null(bloque$perfil)) {
      # Sin muestra no hay clasificacion posible. Ante la duda se protege: para
      # una funcion de privacidad el valor por omision seguro es cerrado.
      resumen <- .proteger_resumen_dbi(
        resumen, resumen$columnas$columna, "sin_clasificacion_disponible"
      )
    } else {
      resumen <- .proteger_resumen_dbi(
        resumen,
        .columnas_personales_protegidas(bloque$perfil$datos_personales),
        "perfil_muestra"
      )
    }
  } else {
    resumen$meta$proteccion_personal <- list(
      aplicada = FALSE, base = "desactivada por el usuario",
      columnas = character()
    )
  }
  resumen$literales <- NULL

  if (is.null(bloque$perfil) && any(bloque$cobertura$estado == "no_disponible")) {
    motivos <- bloque$cobertura$motivo[
      bloque$cobertura$estado == "no_disponible"
    ]
    .avisar_dbi("lupa_muestra_dbi_no_disponible", paste0(
      "El resumen SQL se calculo y se devuelve, pero la muestra no: ",
      paste(motivos, collapse = " "),
      " Ver `resumen_tabla$cobertura`."
    ))
  }

  if (isTRUE(derrame$disponible) && derrame$consultas_con_derrame > 0) {
    .avisar_dbi("lupa_aviso_derrame_dbi", paste0(
      "Se observo derrame real en ", derrame$consultas_con_derrame,
      " consulta(s) exacta(s) de distintos, moda o mediana: ",
      derrame$bloques_temporales_leidos, " bloques temporales leidos y ",
      derrame$bloques_temporales_escritos,
      " escritos. Fuente: `", derrame$fuente,
      "`. El detalle queda en `resumen_tabla$sql`."
    ))
  }

  estructura <- list(resumen_tabla = resumen, perfil_muestra = bloque$perfil)
  class(estructura) <- "perfil_dbi"
  estructura
}

.texto_clave_dbi <- function(clave) {
  if (!is.list(clave) || is.null(clave$garantia)) return(NULL)
  columnas <- if (length(clave$columnas)) {
    paste0("`", paste(as.character(clave$columnas), collapse = "`, `"), "`")
  } else {
    "ninguna"
  }
  fuente <- if (length(clave$fuente) && !is.na(clave$fuente[[1L]])) {
    paste0(" (fuente `", as.character(clave$fuente[[1L]]), "`)")
  } else {
    ""
  }
  estado <- clave$estado
  visible <- if (is.list(estado) && length(estado$visible)) {
    estado$visible[[1L]]
  } else {
    NA
  }
  motivo <- if (length(clave$motivo) && !all(is.na(clave$motivo))) {
    paste(as.character(clave$motivo), collapse = " ")
  } else {
    ""
  }
  if (is.na(visible)) {
    texto <- "no se pudo preguntar al catalogo"
    if (nzchar(motivo)) texto <- paste0(texto, ": ", motivo)
  } else if (identical(visible, FALSE)) {
    texto <- "el catalogo no deja ver la clave"
  } else if (identical(clave$garantia, "no_declarada")) {
    texto <- "no declara clave"
  } else {
    texto <- paste0("garantia `", as.character(clave$garantia), "` en ", columnas)
  }
  detalles <- character()
  if (is.list(estado) && isTRUE(estado$restriccion_diferible)) {
    detalles <- c(detalles, "restriccion diferible")
  }
  if (is.list(estado) && isTRUE(estado$universo_incluye_descendientes)) {
    detalles <- c(detalles, "universo con descendientes")
  }
  if (is.list(estado) && isTRUE(estado$indice_no_unico)) {
    detalles <- c(detalles, "indice no unico")
  }
  if (length(detalles)) {
    texto <- paste0(texto, " (", paste(detalles, collapse = "; "), ")")
  }
  paste0("Clave primaria: ", texto, fuente, ".")
}

# Columnas cuya consulta de valores fue parte de la corrida. La estimacion
# debe describir el mismo subconjunto que el aviso emitido antes de consultar.
.columnas_estimacion_derrame_publicadas_dbi <- function(x, familia, estimacion) {
  if (is.null(estimacion) || !is.data.frame(estimacion$columnas) ||
      !nrow(estimacion$columnas)) return(character())
  sql <- x$resumen_tabla$sql
  nombre <- familia
  if (!is.data.frame(sql) || !all(c("columna", "metrica", "estado") %in% names(sql))) {
    return(character())
  }
  estados_omitidos <- c("no_solicitado", "omitido_por_costo",
                        "omitido_por_privacidad")
  ejecutadas <- unique(as.character(sql$columna[
    sql$metrica == nombre & !(sql$estado %in% estados_omitidos)
  ]))
  intersect(as.character(estimacion$columnas$columna), ejecutadas)
}

#' @export
print.perfil_dbi <- function(x, ...) {
  meta <- x$resumen_tabla$meta
  alcance <- if (identical(meta$alcance, "tabla_muestreada") ||
                 identical(meta$alcance_texto, "tabla_muestreada") ||
                 (is.list(meta$alcance) &&
                  identical(meta$alcance$universo_id, "muestra_motor"))) {
    "tabla muestreada"
  } else {
    "tabla completa"
  }
  cli::cli_text("Perfil DBI de {.strong {meta$tabla}}")
  cli::cli_text(
    "Resumen de {alcance}: {nrow(x$resumen_tabla$columnas)} columnas sobre {meta$filas} filas"
  )
  texto_clave <- .texto_clave_dbi(meta$clave)
  if (!is.null(texto_clave)) cli::cli_text(texto_clave)
  estados <- table(x$resumen_tabla$sql$estado)
  if (length(estados)) {
    detalle <- paste0(names(estados), " ", as.integer(estados), collapse = ", ")
    cli::cli_text("M\u00e9tricas: {detalle}")
  }
  if (!is.null(meta$consultas)) {
    cli::cli_text(
      "Consultas emitidas: {meta$consultas$emitidas} (dialecto {meta$dialecto$nombre})"
    )
  }
  estimacion <- meta$estimacion_derrame
  if (!is.null(estimacion) && !identical(estimacion$estado, "no_solicitado")) {
    if (identical(estimacion$estado, "estimado") ||
        identical(estimacion$estado, "parcial")) {
      detalle_estimacion <- if (identical(estimacion$estado, "parcial")) {
        paste("parcial;", estimacion$motivo)
      } else if (isTRUE(estimacion$supera_memoria)) {
        paste(
          "el tama\u00f1o estimado supera el limite efectivo para hash de",
          estimacion$memoria_efectiva
        )
      } else {
        paste(
          "el tama\u00f1o estimado no supera el limite efectivo para hash de",
          estimacion$memoria_efectiva,
          "; esto no demuestra que no haya derrame"
        )
      }
      cli::cli_text(paste0(
        "Derrame estimado (no medido): ", detalle_estimacion,
        ". Fuente: `", estimacion$fuente, "`."
      ))
    } else {
      cli::cli_text(paste0(
        "Derrame estimado: no se pudo estimar (", estimacion$motivo, ")."
      ))
    }
  }
  for (familia in c("moda", "mediana")) {
    nombre <- paste0("estimacion_derrame_", familia)
    estimacion_familia <- meta[[nombre]]
    if (is.null(estimacion_familia) ||
        identical(estimacion_familia$estado, "no_solicitado")) next
    columnas_estimadas <- .columnas_estimacion_derrame_publicadas_dbi(
      x, familia, estimacion_familia
    )
    if (!length(columnas_estimadas)) next
    estimacion_familia <- .filtrar_estimacion_derrame_dbi(
      estimacion_familia, columnas_estimadas
    )
    limite <- .limite_decision_derrame_dbi(estimacion_familia, familia)
    detalle <- if (isTRUE(estimacion_familia$supera_memoria)) {
      paste("supera el limite de decision", limite)
    } else if (identical(estimacion_familia$estado, "estimado")) {
      paste("no supera el limite de decision", limite)
    } else estimacion_familia$motivo
    cli::cli_text(paste0(
      "Derrame estimado de ", familia, " (no medido): ", detalle,
      ". Metodo/forma: ", estimacion_familia$metodo, "/",
      estimacion_familia$forma, ". Fuente: `", estimacion_familia$fuente, "`."
    ))
  }
  derrame <- meta$derrame
  if (!is.null(derrame) && !identical(derrame$estado, "no_solicitado")) {
    if (isTRUE(derrame$disponible)) {
      cli::cli_text(
        "Derrame medido: {derrame$consultas_con_derrame} consulta{?/s}, ",
        "{derrame$bloques_temporales_leidos} bloques temporales leidos, ",
        "{derrame$bloques_temporales_escritos} escritos."
      )
    } else {
      cli::cli_text(
        "Derrame: no disponible ({derrame$motivo})."
      )
    }
  }
  estado_muestra <- .estado_bloque_muestra_dbi(x)
  if (identical(estado_muestra, "no_solicitado")) {
    cli::cli_text(
      "Perfil de muestra: no solicitado; el motivo est\u00e1 en `resumen_tabla$cobertura`."
    )
  } else if (is.null(x$perfil_muestra)) {
    cli::cli_text(
      "Perfil de muestra: no disponible; el motivo est\u00e1 en `resumen_tabla$cobertura`."
    )
  } else {
    muestreo <- x$perfil_muestra$meta$origen_dbi$muestreo
    cli::cli_text(
      "Perfil de muestra: {muestreo$filas_obtenidas} filas, reproducible: {muestreo$reproducible}"
    )
  }
  n_cobertura <- if (is.null(x$resumen_tabla$cobertura)) {
    0L
  } else {
    nrow(x$resumen_tabla$cobertura)
  }
  if (n_cobertura) {
    cli::cli_text(
      "Cobertura: {n_cobertura} anotaci{?\u00f3n/ones} en `resumen_tabla$cobertura`"
    )
  }
  proteccion <- meta$proteccion_personal
  if (!is.null(proteccion) && isTRUE(proteccion$aplicada)) {
    n_protegidas <- length(proteccion$columnas)
    cli::cli_text(
      "Protecci\u00f3n de datos personales aplicada a {n_protegidas} columna{?/s}."
    )
  }
  aproximaciones <- meta$aproximaciones
  if (length(aproximaciones)) {
    aplicada <- vapply(
      aproximaciones, function(a) isTRUE(a$disponible), logical(1L)
    )
    if (any(aplicada)) {
      detalle <- paste(
        paste0(
          names(aproximaciones)[aplicada], " por ",
          vapply(aproximaciones[aplicada], function(a) a$metodo, character(1L))
        ),
        collapse = " y "
      )
      cli::cli_text(
        "Aproximaciones aplicadas: {detalle}. El error esperado y las sondas ",
        "est\u00e1n en `resumen_tabla$meta$aproximaciones`."
      )
    }
    if (any(!aplicada)) {
      sin_aproximar <- paste(
        names(aproximaciones)[!aplicada], collapse = " y "
      )
      cierre <- if (sum(!aplicada) > 1L) {
        "las m\u00e9tricas se midieron exactas."
      } else {
        "la m\u00e9trica se midi\u00f3 exacta."
      }
      cli::cli_text(
        "Sin aproximaci\u00f3n para {sin_aproximar}: el motor no la ",
        "acept\u00f3 y {cierre}"
      )
    }
  }
  cli::cli_text(
    "No se imprime ning\u00fan valor de celda: est\u00e1n en `resumen_tabla$columnas`."
  )
  invisible(x)
}
