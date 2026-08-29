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
  # Las referencias de tiempo se llenan con consultas planas aceptadas. No se
  # usan estadisticas del catalogo para proyectar el costo de distintos.
  estado$referencias_planas <- list()
  estado$proyeccion_distintos <- NULL
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

.registrar_referencia_plana_dbi <- function(presupuesto, consulta) {
  if (is.null(presupuesto) || !isTRUE(consulta$ok) ||
      is.null(consulta$consulta_id) || length(consulta$consulta_id) != 1L ||
      is.na(consulta$consulta_id) || is.null(consulta$duracion_ms) ||
      length(consulta$duracion_ms) != 1L || is.na(consulta$duracion_ms) ||
      !is.finite(consulta$duracion_ms) || consulta$duracion_ms <= 0) {
    return(invisible(NULL))
  }
  id <- as.character(consulta$consulta_id)
  referencias <- presupuesto$referencias_planas
  referencias[[id]] <- list(
    consulta_id = as.integer(consulta$consulta_id),
    duracion_ms = as.numeric(consulta$duracion_ms)
  )
  presupuesto$referencias_planas <- referencias
  invisible(NULL)
}

.proyectar_costo_distintos_dbi <- function(presupuesto, n_lotes) {
  vacia <- list(
    disponible = FALSE, duracion_estimada_ms = NA_real_,
    duracion_referencia_ms = NA_real_, n_lotes = as.integer(n_lotes),
    n_referencias = 0L, fuente = NA_character_, motivo = paste(
      "No hay una duracion medida de un agregado plano en esta corrida;",
      "no se publica una proyeccion temporal."
    )
  )
  if (is.null(presupuesto) || !length(presupuesto$referencias_planas) ||
      !is.numeric(n_lotes) || length(n_lotes) != 1L || is.na(n_lotes) ||
      !is.finite(n_lotes) || n_lotes < 1) {
    return(vacia)
  }
  duraciones <- vapply(
    presupuesto$referencias_planas,
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
      "consulta(s) de agregados planos medidas en esta corrida"
    ),
    motivo = paste(
      "La proyeccion supone una pasada de costo comparable por lote de",
      "distintos; es una estimacion y no una medicion del agregado exacto."
    )
  )
}

.segundos_dbi <- function(milisegundos) {
  if (is.null(milisegundos) || length(milisegundos) != 1L ||
      is.na(milisegundos) || !is.finite(milisegundos)) return("sin dato")
  formatC(milisegundos / 1000, format = "f", digits = 1,
          decimal.mark = ",")
}

.UMBRAL_AVISO_DISTINTOS_DBI <- 30 * 1000

.avisar_costo_distintos_dbi <- function(proyeccion) {
  if (is.null(proyeccion) || !isTRUE(proyeccion$disponible) ||
      is.na(proyeccion$duracion_estimada_ms) ||
      proyeccion$duracion_estimada_ms < .UMBRAL_AVISO_DISTINTOS_DBI) {
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
      muestreo = c("tablesample_reservoir", "tablesample_bernoulli",
                   "tablesample_system", "tablesample_percent", "random_limit")
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
      muestreo = c("tablesample_reservoir", "tablesample_bernoulli",
                   "tablesample_system", "tablesample_percent", "random_limit")
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
      muestreo = c("tablesample_reservoir", "tablesample_bernoulli",
                   "tablesample_system", "tablesample_percent", "oracle_sample",
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
      muestreo = c("tablesample_reservoir", "tablesample_bernoulli",
                   "tablesample_system", "tablesample_percent", "oracle_sample",
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
      muestreo = c("tablesample_reservoir", "tablesample_bernoulli",
                   "tablesample_system", "tablesample_percent", "random_limit")
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
    # arregla que cada consulta saque su propia muestra, pero al menos las saca
    # todas del mismo tamano.
    tablesample_reservoir = list(
      nombre = "tablesample_reservoir",
      descripcion = "TABLESAMPLE RESERVOIR (n ROWS)",
      patron = "duckdb",
      tipo = "tablesample_filas",
      constructor = function(tabla, filas) paste0(
        tabla, " TABLESAMPLE RESERVOIR (", .entero_sql_dbi(filas), " ROWS)"
      )
    ),
    # Y antes de las de bloque, la de fila. Medido contra PostgreSQL 16 sobre una
    # tabla de 5.000 filas, pidiendo el 20 %:
    #
    #   TABLESAMPLE SYSTEM (20)      678  904  452  1384
    #   TABLESAMPLE BERNOULLI (20)  1011 1017  981  1050
    #
    # `SYSTEM` elige bloques enteros, asi que sobre una tabla chica el tamano de
    # la muestra salta de un tercio al doble de lo pedido, y puede dar cero.
    # `BERNOULLI` decide fila por fila y se queda donde se le pidio. Cuesta mas
    # en el motor -recorre la tabla- pero un tamano que no se puede anticipar
    # hace que dos metricas del mismo perfil no sean comparables.
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

.sondar_muestreo_dbi <- function(conexion, tabla_sql, dialecto, presupuesto) {
  candidatos <- .candidatos_muestreo_dbi(conexion, dialecto)
  if (!length(candidatos)) {
    return(list(
      disponible = FALSE, candidato = NULL, sondas = character(),
      motivo = "El adaptador no declara formas candidatas de muestreo."
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
      motivo = paste(
        "El motor rechazo todas las formas candidatas de muestreo; no se",
        "calcularon metricas parciales sobre la tabla completa."
      )
    ))
  }
  list(
    disponible = TRUE, candidato = aceptada, sondas = sondas,
    motivo = paste0(
      "El motor acepto la sonda de muestreo `", aceptada$nombre, "`."
    )
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

# Algunos motores pueden obtener varios percentiles en la misma agregacion.
# Esta capacidad se sondea por separado porque una sonda que solo prueba una
# mediana no prueba que el motor acepte varias expresiones en un SELECT.
.candidatos_mediana_consolidada_dbi <- function(conexion) {
  senas <- .senas_conexion_dbi(conexion)
  candidatos <- list(
    list(
      nombre = "PERCENTILE_CONT",
      patron = "postgres|redshift|oracle|snowflake|mariadb",
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
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY 1.0) AS ",
        alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
      )
    ),
    list(
      nombre = "PERCENTILE_CONT_OVER",
      patron = "sql server|microsoft sql|sqlserver|mssql",
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
        "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY 1.0) OVER () AS ",
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
  elegida <- NULL
  for (candidato in candidatos) {
    sql <- candidato$sonda(alias)
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "sonda_mediana_consolidada"
    )
    if (is.null(elegida) && isTRUE(prueba$ok)) elegida <- candidato
  }
  resultado <- list(
    disponible = !is.null(elegida), candidato = elegida, sondas = sondas,
    motivo = if (is.null(elegida)) {
      if (!length(candidatos)) {
        "No hay una forma consolidada declarada para este motor; se conserva la mediana por columna."
      } else {
        "El motor no acepto una consulta consolidada de `PERCENTILE_CONT`; se conserva la mediana por columna."
      }
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
        patron = "postgres|redshift|sql server|microsoft sql|sqlserver|mssql",
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
          ") AS ", alias, " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY 1.0) AS ",
          alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
        )
      ),
      list(
        nombre = "PERCENTILE_CONT_OVER",
        patron = "sql server|microsoft sql|sqlserver|mssql",
        error_esperado = "desconocido",
        construir = function(expr, tabla, alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ", expr,
          ") OVER () AS ", alias, " FROM ", tabla
        ),
        sonda = function(alias) paste0(
          "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY 1.0) OVER () AS ",
          alias, " FROM (SELECT 1.0 AS lupa_valor) lupa_sonda"
        )
      ),
      list(
        nombre = "approx_percentile",
        patron = "snowflake|presto|trino",
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
    filas_obtenidas = numero(muestreo_meta, "filas_obtenidas"),
    universo = n_total,
    sondas = resolucion$sondas,
    motivo = resolucion$motivo,
    sql = if (is.null(forma)) NA_character_ else forma$sql
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
  gsub("[[:space:]]+", " ", trimws(texto))
}

.estadisticas_derrame_postgresql_dbi <- function(conexion) {
  sql <- paste(
    "SELECT query, calls, temp_blks_read, temp_blks_written",
    "FROM pg_stat_statements",
    "WHERE query ILIKE '%COUNT(DISTINCT%'"
  )
  datos <- tryCatch(
    DBI::dbGetQuery(conexion, sql),
    error = function(e) NULL
  )
  if (is.null(datos) || !all(c(
    "query", "calls", "temp_blks_read", "temp_blks_written"
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
  for (i in seq_len(nrow(despues))) {
    clave <- despues$query_normalizada[[i]]
    if (is.na(clave) || !nzchar(clave)) next
    indices_previos <- which(
      !is.na(antes$query_normalizada) & antes$query_normalizada == clave
    )
    previo <- antes[indices_previos, , drop = FALSE]
    llamadas_antes <- if (nrow(previo)) .numero_dbi(previo$calls[[1L]]) else 0
    llamadas_despues <- .numero_dbi(despues$calls[[i]])
    leidos_antes <- if (nrow(previo)) {
      .numero_dbi(previo$temp_blks_read[[1L]])
    } else 0
    escritos_antes <- if (nrow(previo)) {
      .numero_dbi(previo$temp_blks_written[[1L]])
    } else 0
    delta_llamadas <- llamadas_despues - llamadas_antes
    delta_leidos <- .numero_dbi(despues$temp_blks_read[[i]]) - leidos_antes
    delta_escritos <- .numero_dbi(despues$temp_blks_written[[i]]) - escritos_antes
    # Una llamada exacta permite atribuir los bloques a esta corrida. Si hubo
    # otra llamada concurrente o se reiniciaron las estadisticas, se declara
    # desconocido en vez de adjudicarle sus bloques a esta consulta.
    if (!isTRUE(delta_llamadas == 1) || !is.finite(delta_leidos) ||
        !is.finite(delta_escritos) || delta_leidos < 0 || delta_escritos < 0) {
      next
    }
    consultas[[length(consultas) + 1L]] <- list(
      query_normalizada = clave, derrame = delta_leidos > 0 || delta_escritos > 0,
      bloques_temporales_leidos = delta_leidos,
      bloques_temporales_escritos = delta_escritos
    )
  }
  if (!length(consultas)) {
    estado$estado <- "no_disponible"
    estado$disponible <- FALSE
    estado$motivo <- paste(
      "`pg_stat_statements` no permitio atribuir una llamada exacta a esta",
      "corrida; no se publica el derrame."
    )
  } else {
    estado$estado <- "medido"
    estado$consultas <- consultas
    estado$motivo <- paste(
      "Los bloques temporales se atribuyeron a la consulta exacta de esta",
      "corrida mediante `pg_stat_statements`."
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
      bloques_temporales_escritos = NA_real_
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
  list(
    disponible = identical(estado$estado, "medido"),
    estado = estado$estado,
    fuente = estado$fuente,
    motivo = estado$motivo,
    consultas_observadas = as.integer(length(consultas)),
    consultas_con_derrame = as.integer(sum(derrames)),
    bloques_temporales_leidos = leidos,
    bloques_temporales_escritos = escritos
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
                               id_muestra = NA_integer_,
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
    id_muestra = id_muestra,
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
    id_muestra = rep_len(
      as.integer(metadatos$id_muestra), length(metricas)
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

# ---- Metricas y modo -----------------------------------------------------

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

.METRICAS_NUMERICAS_DBI <- c("basicos", "mediana", "desvio")

# La estrategia de `distintos` se elige de forma explicita. El orden es parte
# del contrato: el primer valor es el valor por omision de la API.
.ESTRATEGIAS_DISTINTOS_DBI <- c(
  "exacta", "aproximada_motor", "catalogo", "omitida"
)

# Moda y mediana son las metricas que pueden pagar un agrupamiento u ordenacion
# por columna. No se omiten nunca por sorpresa: la politica por cardinalidad es
# opt-in y su umbral queda en la llamada, en la metadata y en los motivos.
.METRICAS_COSTOSAS_DBI <- c("moda", "mediana")
.UMBRAL_CARDINALIDAD_COSTO_DBI <- 0.95

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

.validar_politica_costo_dbi <- function(politica_costo,
                                       umbral_cardinalidad) {
  politica <- match.arg(
    politica_costo,
    c("todas", "ninguna", "por_cardinalidad", "cardinalidad")
  )
  if (identical(politica, "ninguna")) politica <- "todas"
  if (identical(politica, "cardinalidad")) politica <- "por_cardinalidad"
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
      !any(.METRICAS_COSTOSAS_DBI %in% metricas)) {
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
    any(.METRICAS_COSTOSAS_DBI %in% metricas_solicitadas)
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
    sondas = character()
  )
}

.resolver_estrategia_distintos_dbi <- function(conexion, estrategia,
                                               presupuesto, hay_metrica) {
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
      estrategia$estado <- "no_disponible"
      estrategia$motivo <- paste(
        "La procedencia `catalogo` esta declarada, pero la estadistica de",
        "cardinalidad del catalogo aun no esta implementada; `pg_stats` no",
        "se usa en esta version."
      )
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
    sondas = estrategia$sondas
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

.metricas_de_modo_dbi <- function(modo) {
  switch(
    modo,
    exacto = .METRICAS_DBI,
    seguro = c("validos", "basicos", "desvio"),
    conteos = "validos",
    muestreado = .METRICAS_DBI,
    aproximado = .METRICAS_DBI
  )
}

.validar_metricas_dbi <- function(metricas, modo) {
  if (is.null(metricas)) return(.metricas_de_modo_dbi(modo))
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

.decision_costo_dbi <- function(columna, conteos, politica,
                                n_validos = NA_real_, n_distintos = NA_real_,
                                alcance = "la tabla",
                                fuente_cardinalidad_costo = NULL) {
  conservar <- list(moda = TRUE, mediana = TRUE)
  fuente <- fuente_cardinalidad_costo
  if (is.null(fuente)) fuente <- .fuente_cardinalidad_desconocida_dbi()
  detalle <- list(
    n_validos = n_validos, n_distintos = n_distintos,
    proporcion_distintos = NA_real_, motivo = NA_character_,
    fuente_cardinalidad_costo = fuente$nombre
  )
  if (!identical(politica$nombre, "por_cardinalidad")) {
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
  if (!is.null(fuente$proporcion_distintos) &&
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
    conservar$mediana <- FALSE
    detalle$motivo <- paste0(
      "Se omitieron las metricas caras solicitadas (moda y/o mediana) de la ",
      "columna `", columna, "`: tiene ", distintos,
      " valores distintos de ",
      validos, " validos (", formatC(proporcion, format = "f", digits = 3),
      ") sobre ", alcance, "; la politica optativa considera que agrupar u",
      " ordenar toda la columna no justifica el costo. Para pedirla igual,",
      " use `politica_costo = \"todas\"`; para mover el criterio, cambie",
      " `umbral_cardinalidad`."
    )
  }
  c(conservar, detalle = list(detalle))
}

.decisiones_costo_dbi <- function(conexion, columnas, agregados, politica,
                                  n_total, modo,
                                  fuentes_cardinalidad_costo = NULL) {
  salida <- vector("list", length(columnas))
  names(salida) <- columnas
  alcance <- if (identical(modo, "muestreado")) "la muestra medida" else
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
  if (!isTRUE(estrategia$para_costo)) {
    return(list(fuentes = fuentes, catalogo = NULL))
  }
  piezas <- .piezas_tabla_cardinalidad_dbi(tabla)
  catalogo <- .clave_primaria_dbi(
    conexion, piezas$tabla, piezas$esquema, presupuesto = presupuesto
  )
  # Una clave compuesta no vuelve unica cada columna por separado. Solo una
  # clave primaria simple, aplicada y validada, permite afirmar que su columna
  # tiene tantos distintos como valores validos, sin contarla.
  if (length(catalogo$columnas) == 1L &&
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
              id_muestra = as.integer(consulta$consulta_id)
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
          id_muestra = as.integer(consulta_validos$consulta_id)
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
    id_muestra = NA_integer_
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
    resultado$metadatos$id_muestra <- as.integer(consulta$consulta_id)
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
    id_muestra = as.integer(consulta$consulta_id)
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
             list(id_muestra = as.integer(consulta$consulta_id))
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
    presupuesto, estado = NULL) {
  aliases <- vapply(seq_along(lote), function(i) {
    .alias_agregado_dbi(alias, "mediana", numero, i)
  }, character(1L), USE.NAMES = FALSE)
  expresiones <- vapply(seq_along(lote), function(i) {
    candidato$expresion(nombres_sql[[lote[[i]]]], aliases[[i]])
  }, character(1L), USE.NAMES = FALSE)
  sql <- candidato$construir_multiple(expresiones, tabla_sql)
  consulta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "medianas_consolidadas"
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
    if (identical(estado, "estimado")) {
      metadatos_resultado <- c(
        metadatos_resultado,
        list(metodo = candidato$nombre,
             error_esperado = candidato$error_esperado)
      )
    }
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
    presupuesto, tamano_lote, estado = NULL) {
  salida <- vector("list", length(columnas))
  names(salida) <- columnas
  if (is.null(candidato) || !length(columnas)) return(salida)
  lotes <- .lotes_columnas_dbi(columnas, tamano_lote)
  for (numero in seq_along(lotes)) {
    lote <- lotes[[numero]]
    resultado <- .medianas_lote_consolidadas_dbi(
      conexion, tabla_sql, lote, nombres_sql, alias, numero, candidato,
      presupuesto, estado = estado
    )
    # Si la forma consolidada falla sobre la consulta real, no se transforma
    # ese fallo en una `NA` calculada: el llamador deja el resultado ausente y
    # usa el camino exacto por columna, que ya declara sus propios errores.
    for (columna in lote) {
      if (!is.null(resultado$resultados[[columna]])) {
        salida[[columna]] <- resultado$resultados[[columna]]
      }
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
    }
    if ("desvio" %in% metricas && !is.null(aliases$desvio)) {
      resultado$desvio <- .resultado_lote_dbi(
        consulta, sql, aliases$desvio, metadatos
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
    if (!isTRUE(con_universo)) {
      .registrar_referencia_plana_dbi(presupuesto, consulta)
    }
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
          id_muestra = as.integer(entrada_total$consulta$consulta_id)
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
        incluir_total = "validos" %in% metricas_planas,
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
  # Primero quedan disponibles los agregados planos y el total exacto que se
  # fusiona con su primera consulta. Despues se paga COUNT(DISTINCT),
  # que conserva su propia familia y su lote conservador.
  if ("distintos" %in% metricas) {
    if (is.null(columnas_distintos)) columnas_distintos <- columnas
    n_lotes_distintos <- length(.lotes_columnas_dbi(
      columnas_distintos, tamano_lote_distintos
    ))
    if (is.null(aproximacion_distintos)) {
      proyeccion <- .proyectar_costo_distintos_dbi(
        presupuesto, n_lotes_distintos
      )
      presupuesto$proyeccion_distintos <- proyeccion
      .avisar_costo_distintos_dbi(proyeccion)
    } else {
      presupuesto$proyeccion_distintos <- list(
        disponible = FALSE, duracion_estimada_ms = NA_real_,
        duracion_referencia_ms = NA_real_, n_lotes = as.integer(n_lotes_distintos),
        n_referencias = 0L, fuente = NA_character_, motivo = paste(
          "La estrategia de distintos es aproximada; no se proyecta el costo",
          "de `COUNT(DISTINCT)` exacto."
        )
      )
    }
    instrumentacion_derrame <- .iniciar_instrumentacion_derrame_dbi(
      conexion, presupuesto,
      exacto = is.null(aproximacion_distintos)
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
      resultados <- resultado_lote$resultados
      for (campo in lote) {
        base_conteos <- agregados$conteos[[campo]]
        if (is.null(base_conteos)) base_conteos <- list()
        agregados$conteos[[campo]] <- utils::modifyList(
          base_conteos, resultados[[campo]]
        )
      }
    }
    if (identical(instrumentacion_derrame$estado, "observando")) {
      .finalizar_instrumentacion_derrame_dbi(conexion, presupuesto)
    }
  }
  agregados$n_total <- n_total
  agregados$conteo <- conteo
  agregados$sql_conteo <- if (is.null(conteo$sql)) NA_character_ else conteo$sql
  agregados
}

.moda_columna_dbi <- function(conexion, tabla_sql, columna_sql, dialecto,
                              presupuesto, moda_guardian = NULL) {
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
  sql_moda <- if (is.null(moda_guardian)) {
    if (is.null(acotada)) sin_limite else acotada
  } else {
    moda_guardian$construir(columna_sql, tabla_sql)
  }
  resultado <- .consultar_dbi(
    conexion, sql_moda, presupuesto,
    filas = if (is.null(acotada)) 1L else -1L,
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
                                 modo = "exacto", muestreo = NULL,
                                 aproximacion_distintos = NULL,
                                 aproximacion_mediana = NULL,
                                 tamano_muestra = NA_real_,
                                 fraccion_muestra = NA_real_,
                                 agregados = NULL,
                                 mediana_consolidada = NULL,
                                 mediana_escalar = NULL,
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
  es_muestreado <- identical(modo, "muestreado")
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
  registrar <- function(registros, metrica, resultado, motivo_exito = NA_character_) {
    if (es_muestreado && isTRUE(resultado$ok) && is.null(resultado$estado)) {
      resultado$estado <- if (any(metrica %in% c("n_distintos", "tasa_distintos"))) {
        "observado_muestra"
      } else {
        "estimado"
      }
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
      if (isTRUE(resultado$ok) && all(is.na(motivo_exito)) &&
          !is.na(motivo_error_esperado)) {
        motivo_exito <- motivo_error_esperado
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
    "La metrica no se pidio en esta corrida; ver `metricas` y `modo`."
  )
  motivo_privacidad <- paste(
    "El valor no se informa por `incluir_valores = FALSE`;",
    "la consulta no se emitio."
  )

  conteos <- if (!is.null(agregados) &&
                 !is.null(agregados$conteos[[columna]])) {
    agregados$conteos[[columna]]
  } else {
    pide_distintos <- "distintos" %in% metricas &&
      isTRUE(estrategia_distintos$disponible)
    .conteos_columna_dbi(
      conexion, tabla_sql, columna_sql, alias,
      "validos" %in% metricas, pide_distintos, presupuesto,
      aproximacion_distintos = aproximacion_distintos
    )
  }

  if ("validos" %in% metricas) {
    validos <- conteos$validos
    registros <- registrar(registros, .CAMPOS_METRICA_DBI$validos, validos)
    if (validos$ok) {
      validos_observados <- .conteo_dbi(validos$valor)
      n_total_consulta <- if (is.null(validos$metadatos)) NA_real_ else {
        .numero_dbi(validos$metadatos$n_total_consulta)
      }
      fila$n_validos <- if (es_muestreado) {
        .conteo_estimado_dbi(
          validos_observados, n_total, n_total_consulta
        )
      } else {
        validos_observados
      }
      if (!is.na(fila$n_validos) && !is.na(n_total_consulta)) {
        fila$n_faltantes <- if (es_muestreado) {
          n_total - fila$n_validos
        } else {
          n_total_consulta - validos_observados
        }
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
      distintos <- conteos$distintos
      if (is.null(distintos)) {
        distintos <- list(
          ok = FALSE, valor = NULL,
          motivo = "La estrategia resolvio una medicion pero no se obtuvo su resultado.",
          sql = NA_character_
        )
      }
      motivo_cota <- NA_character_
      if (isTRUE(distintos$ok)) {
        distintos$metadatos <- .mezclar_metadatos_dbi(
          distintos$metadatos, list(
            metodo = estrategia_distintos$estrategia_resuelta,
            error_esperado = estrategia_distintos$error_esperado
          )
        )
        if (identical(estrategia_distintos$estado, "estimado_motor")) {
          distintos$estado <- "estimado_motor"
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
          if (!is.na(fila$n_distintos) && guardian$comprobable &&
              guardian$valor > 0) {
            denominador <- guardian$valor
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
      registros, "moda", "omitido_por_costo", decisiones_costo$detalle$motivo
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
            fila$frecuencia_moda <- if (es_muestreado) {
              .conteo_estimado_dbi(candidato, n_total, tamano_muestra)
            } else {
              candidato
            }
            if (!is.na(candidato)) moda$motivo <- motivo_cota
          }
        }
      }
    } else if (moda$ok) {
      moda$motivo <- "La columna no contiene valores no nulos."
      moda$estado <- "sin_valores"
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
    registros <- registrar(
      registros, .CAMPOS_METRICA_DBI$moda, moda, motivo_exito = moda$motivo
    )
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
      if (es_muestreado) {
        for (metrica in c("n_ceros", "n_negativos")) {
          if (!is.null(leidos[[metrica]])) {
            fila[[metrica]] <- .conteo_estimado_dbi(
              leidos[[metrica]], n_total, tamano_muestra
            )
          }
        }
      }
    } else if (basicos$ok) {
      basicos$ok <- FALSE
      basicos$motivo <- "La consulta de agregados no devolvio ninguna fila."
    }
    if (is.null(basicos$sql)) basicos$sql <- NA_character_
    registros <- registrar(registros, calculados, basicos)
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
        decisiones_costo$detalle$motivo
      )
    } else if (!is.null(mediana_consolidada)) {
      mediana <- mediana_consolidada
      if (isTRUE(mediana$ok)) fila$mediana <- .escalar_finito_dbi(mediana$valor)
      registros <- registrar(registros, "mediana", mediana)
    } else if (!is.null(aproximacion_mediana)) {
      sql_mediana <- aproximacion_mediana$construir(
        columna_sql, tabla_sql, alias("mediana")
      )
      mediana <- .escalar_dbi(
        conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
      )
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
    } else if (!is.null(mediana_escalar)) {
      sql_mediana <- mediana_escalar$construir(
        columna_sql, tabla_sql, alias("mediana"),
        materializar = es_muestreado
      )
      mediana <- .escalar_dbi(
        conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
      )
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
        mediana <- .escalar_dbi(
          conexion, sql_mediana, "mediana", presupuesto, etapa = "mediana"
        )
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
                                                     n_total, modo) {
  if (is.null(fuentes) || !length(fuentes)) return(agregados)
  for (columna in names(fuentes)) {
    fuente <- fuentes[[columna]]
    if (is.null(fuente) || !isTRUE(fuente$exacta) ||
        !is.finite(fuente$proporcion_distintos) ||
        fuente$proporcion_distintos != 1 || is.na(n_total) ||
        modo %in% c("muestreado", "aproximado")) {
      next
    }
    resultado <- list(
      ok = TRUE, valor = n_total, motivo = NA_character_, sql = NA_character_,
      estado = if (identical(modo, "muestreado")) "estimado" else "garantizado",
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
                                modo = "exacto",
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
  agregados <- .agregados_consolidados_dbi(
    conexion, tabla_metricas_sql, campos_consolidados,
    campos_sql_consolidados, es_numerico_consolidados, metricas_ejecucion,
    incluir_valores, presupuesto, tamano_lote_planos, tamano_lote_distintos,
    aproximacion_distintos = aproximaciones$distintos,
    tabla_total_sql = tabla_total_sql, conteo_inicial = conteo,
    columnas_distintos = if (is.null(fuentes_cardinalidad_costo)) NULL else {
      # Una fuente exacta solo evita medir cuando `distintos` es interno para
      # decidir el costo. Si quien llama pidio la metrica, hay que publicarla;
      # en una muestra o con una aproximacion tambien hay que medir el universo
      # que corresponde a esa estrategia.
      if (isTRUE(estrategia_distintos$publica)) {
        campos_consolidados
      } else if (identical(politica_costo$nombre, "por_cardinalidad")) {
        campos_consolidados[vapply(
          fuentes_cardinalidad_costo,
          function(x) !isTRUE(x$exacta), logical(1L)
        )]
      } else {
        character()
      }
    }
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
    agregados, fuentes_cardinalidad_costo, n_total, modo
  )
  sql_conteo_registro <- if (is.null(conteo$sql)) {
    sql_conteo
  } else {
    conteo$sql
  }
  if (identical(modo, "muestreado")) {
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
    conexion, campos_consolidados, agregados, politica_costo, n_total, modo,
    fuentes_cardinalidad_costo = fuentes_cardinalidad_costo
  )
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
      "moda" %in% metricas && isTRUE(incluir_valores) &&
        (is.null(decision) || isTRUE(decision$moda)) &&
        is.na(motivo)
    }, logical(1L))
  ]
  modas <- vector("list", length(columnas_modas))
  names(modas) <- columnas_modas
  for (campo in columnas_modas) {
    modas[[campo]] <- .moda_columna_dbi(
      conexion, tabla_metricas_sql,
      as.character(DBI::dbQuoteIdentifier(conexion, campo)),
      dialecto, presupuesto, moda_guardian = moda_guardian
    )
  }
  columnas_medianas <- intersect(campos_consolidados, campos)
  columnas_medianas <- columnas_medianas[
    match(columnas_medianas, campos_consolidados) <= length(es_numerico_consolidados) &
      es_numerico_consolidados[match(columnas_medianas, campos_consolidados)]
  ]
  columnas_medianas <- columnas_medianas[
    vapply(columnas_medianas, function(campo) {
      decision <- decisiones_costo[[campo]]
      "mediana" %in% metricas && isTRUE(incluir_valores) &&
        (is.null(decision) || isTRUE(decision$mediana))
    }, logical(1L))
  ]
  medianas <- vector("list", length(columnas_medianas))
  names(medianas) <- columnas_medianas
  if (length(columnas_medianas) && !is.null(mediana_consolidada)) {
    medianas <- .medianas_consolidadas_dbi(
      conexion, tabla_metricas_sql, columnas_medianas,
      stats::setNames(campos_sql_consolidados, campos_consolidados),
      function(nombre) as.character(DBI::dbQuoteIdentifier(conexion, nombre)),
      mediana_consolidada, presupuesto, tamano_lote_planos,
      estado = if (identical(modo, "aproximado")) "estimado" else NULL
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
      modo = modo, muestreo = muestreo,
      aproximacion_distintos = aproximaciones$distintos,
      aproximacion_mediana = aproximaciones$mediana,
      tamano_muestra = tamano_muestra, fraccion_muestra = fraccion_muestra,
      agregados = agregados,
      moda_guardian = moda_guardian,
      mediana_consolidada = medianas[[campo]],
      mediana_escalar = mediana_escalar,
      moda_precalculada = modas[[campo]],
      decisiones_costo = decisiones_costo[[campo]],
      publica_distintos = isTRUE(estrategia_distintos$publica),
      estrategia_distintos = estrategia_distintos
    )
  })
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
          alcance = if (identical(modo, "muestreado")) "tabla_muestreada" else
            "tabla_completa",
          universo = n_total, tamano_muestra = if (identical(modo, "muestreado")) {
            tamano_muestra
          } else NA_real_,
          fraccion = if (identical(modo, "muestreado")) fraccion_muestra else 1,
          metodo = if (identical(modo, "muestreado")) "conteo_universo" else
            "conteo_universo",
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
  rownames(columnas) <- NULL
  rownames(sql) <- NULL
  list(
    columnas = columnas,
    sql = sql,
    cobertura = .cobertura_dbi_vacia(),
    literales = unlist(lapply(resultados, `[[`, "literales"), use.names = TRUE),
    meta = list(
       alcance = if (identical(modo, "muestreado")) {
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
      # Un consumidor automatico lee el objeto, no la vineta. Que cada metrica
      # muestreada saque su propia muestra estaba documentado en prosa, y por
      # coherencia con el invariante tiene que estar donde se lee: dos metricas
      # de la misma columna en modo muestreado describen conjuntos de filas del
      # mismo tamano y no los mismos. Solo aparece cuando corresponde; en los
      # modos que miden sobre la tabla entera no hay nada que advertir.
      # La primera version de este campo decia que cada metrica saca su propia
      # muestra, y con los agregados consolidados eso dejo de ser cierto: las
      # columnas que comparten una consulta comparten tambien las filas
      # muestreadas. La consolidacion mejora la coherencia dentro del lote -las
      # razones entre columnas del mismo lote son exactas- y la empeora entre
      # lotes. `id_muestra` deja la garantia comprobable en cada fila; un valor
      # ausente declara que no se puede afirmar que dos metricas vieron las
      # mismas filas. Un campo que describe un alcance de muestreo que no es el
      # real es peor que no tenerlo.
      muestras_independientes = if (modo %in% c("muestreado", "aproximado")) {
        paste(
          "el muestreo se resuelve por consulta, no por perfilado. Las columnas",
          "que comparten una consulta consolidada -ver `id_muestra`, `lote` y",
          "`columnas_compartidas` en `sql`- se miden sobre las MISMAS filas, asi",
          "que sus metricas son comparables entre si. Dos consultas distintas",
          "-otro `id_muestra`, u otra clase como moda o mediana- sacan muestras",
          "distintas del mismo tamano, asi que comparar entre ellas es comparar",
          "conjuntos de filas que no coinciden. Es inherente a muestrear en el",
          "motor sin materializar una tabla intermedia, y perfilar es solo",
          "lectura. Para que todo el perfil hable de las mismas filas, el camino",
          "es `perfil_muestra`"
        )
      } else {
        NA_character_
      },
      solo_lectura = TRUE,
      objetos_temporales = FALSE,
      snapshot = FALSE,
      nota_snapshot = paste(
        "No hubo lectura instantanea: cada agregado se midio en su propio",
        "momento y la tabla pudo cambiar entre consultas. La cobertura declara",
        "las comparaciones incoherentes entre grupos de consistencia distintos."
      ),
      politica_costo = politica_costo,
      decisiones_costo = decisiones_costo
    )
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
                                  modo = "exacto", muestreo_disponible = TRUE,
                                  tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                                  tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                                  incluir_muestra = TRUE,
                                  mediana_consolidada = FALSE,
                                  columnas_distintos = NULL,
                                  columnas_moda = NULL,
                                  columnas_moda_max = NULL,
                                  columnas_mediana = NULL,
                                  columnas_mediana_max = NULL) {
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
  mide_metricas <- !identical(modo, "muestreado") || isTRUE(muestreo_disponible)
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
  alcance_agregado <- if (identical(modo, "muestreado")) {
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
        if (identical(modo, "muestreado")) "lee una muestra del motor" else
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
        if (identical(modo, "muestreado")) "lee una muestra del motor" else
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
      "muestra", 1, "lee las filas pedidas"
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
# `"lee una muestra del motor"` es trabajo del motor -en `modo = "muestreado"`
# el motor muestrea para sus propios agregados- y `"lee las filas pedidas"` es
# el bloque del cliente, que es lo unico que trae filas a R.
.ALCANCES_CON_MUESTRA_DBI <- c("lee una muestra del motor", "lee las filas pedidas")

# El trabajo del CLIENTE cuelga solo del segundo. Contarlo sobre el conjunto de
# los dos hacia que `modo = "muestreado"` con `bloque_muestra = "solo_agregados"`
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
  # por el alcance de las consultas y no por el modo. `modo` selecciona metricas
  # SQL; `bloque_muestra` decide aparte si el plan trae filas. Cuando ese bloque
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
  "modo seguro: 14 consultas, 5,3 segundos). Ese cociente est\u00e1 en las",
  "unidades que cuenta este plan, no en filas que el motor haya le\u00eddo: la",
  "cuenta supone que ning\u00fan \u00edndice ayuda y cobra el desv\u00edo como",
  "dos pasadas, aunque un motor con desv\u00edo nativo lo resuelva en una. Sirve",
  "para convertir `filas_leidas` en segundos, que es para lo que est\u00e1, y no",
  "como medida de lo que el motor lee. Y de 660.000 a 1.150.000 pares por",
  "segundo sobre valores de cuarenta caracteres -la banda cubre dos m\u00e1quinas",
  "distintas-, que bajan a unos 80.000 sobre valores de doscientos. Esa tasa",
  "cuenta los pares que se comparan de verdad: con valores largos el detector",
  "recorta por `max_trabajo`, y dividir por los pares que el plan contar\u00eda",
  "inflaba la cifra cuatro veces."
)


#' Planificar el costo de `perfilar_dbi()` antes de pagarlo
#'
#' Emite sólo consultas de preparación —leer el esquema y sondear capacidades—
#' y devuelve cuántas consultas emitiría el perfilado completo, de qué clase y
#' con qué alcance sobre la tabla. No escanea datos para decidir el costo.
#' Cuando `politica_costo = "por_cardinalidad"`, una clave estructural exacta
#' puede cerrar la decisión; si no hay una fuente de catálogo utilizable, el
#' plan publica el rango entre omitir y ejecutar las métricas caras. Nunca
#' lanza `COUNT(DISTINCT ...)` para despejar esa incertidumbre.
#' Las fuentes estructurales se resuelven cuando la política necesita la
#' cardinalidad, aunque `estrategia_distintos` no permita medirla. La
#' disponibilidad de la estrategia gobierna la medición, no el conocimiento que
#' ya da el catálogo.
#'
#' `estrategia_distintos` declara la procedencia de `n_distintos` antes de la
#' corrida y conserva por separado lo pedido, lo resuelto y el estado. No hay
#' `auto`: `"exacta"` es el valor por omisión, `"aproximada_motor"` queda
#' `no_disponible` si el motor no ofrece una función aceptada, `"catalogo"`
#' queda `no_disponible` hasta implementar su estadística y `"omitida"` no
#' emite el agregado. `fuente_cardinalidad_costo` sigue siendo independiente y
#' sólo describe el número usado por la política de costo cuando esa política
#' se pide.
#'
#' El plan previo no puede publicar segundos medidos porque no lee los datos.
#' Durante `perfilar_dbi()`, en cambio, los agregados planos se ejecutan antes
#' que los distintos. Si se midieron en esta misma corrida, el plan que queda
#' en `resumen_tabla$meta$plan` agrega `costo_distintos`: la mediana de esas
#' duraciones multiplicada por la cantidad de lotes de distintos. Es una
#' estimación rotulada, fundada en la tabla y el servidor actuales, no en
#' `reltuples` ni en otra estadística de catálogo. El aviso se emite antes de
#' iniciar el primer `COUNT(DISTINCT)` y sólo si supera el umbral de 30
#' segundos; no pide confirmación y nunca bloquea un guion no interactivo.
#'
#' @inheritParams perfilar_dbi
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
#'   `mediana_escalar`,
#'   `tamano_lote_planos` y `tamano_lote_distintos`. Cuando se pide
#'   `bloque_muestra = "solo_agregados"`, también conserva ese valor en el
#'   atributo `bloque_muestra` y no incluye la fila de la lectura de muestra.
#'
#'   El costo no se declara como un número sino como un rango: `total` es el
#'   extremo inferior, que supone que la política omite las métricas caras cuya
#'   cardinalidad no se conoce, y `total_maximo` el superior, que supone que las
#'   ejecuta. Ambos incluyen la preparación y el perfilado previsto; el rechazo
#'   de lotes puede agregar las sondas de bisección declaradas por
#'   `total_lotes_rechazados`. El costo real cae entre los extremos, y
#'   `attr(plan, "supuesto")` dice por qué se mueve en cada dirección.
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
#'   El plan previo no publica duraciones, CPU, filas ni bytes medidos. El plan
#'   de una corrida de `perfilar_dbi()` puede agregar `costo_distintos` cuando
#'   ya hay duraciones de agregados planos de esa misma corrida; sus campos
#'   dicen explícitamente que la proyección sigue siendo una estimación.
#'
#'   Si se pide `politica_costo = "por_cardinalidad"`, el plan busca primero una
#'   garantía estructural o una fuente de catálogo. Si la fuente queda
#'   desconocida, no emite un agregado para aclararla: `n_consultas` omite moda
#'   y mediana, y `n_consultas_max` deja abierto el camino que las ejecuta. La
#'   corrida mide `distintos` sólo si la política lo necesita. La política por
#'   omisión es `"todas"`: el paquete no elige por el usuario.
#'   Una fuente estructural se resuelve aunque la estrategia de distintos este
#'   omitida o no disponible; esta ultima solo gobierna si se puede medir.
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
plan_perfilado_dbi <- function(conexion, tabla, muestra = Inf,
                               orden_muestra = NULL,
                               modo = c("exacto", "seguro", "conteos",
                                         "muestreado", "aproximado"),
                               metricas = NULL, max_consultas = Inf,
                               dialecto = "auto", incluir_valores = TRUE,
                               tamano_lote = NULL,
                               tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                               tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                               bloque_muestra = c("con_muestra", "solo_agregados"),
                               instrumentar = FALSE,
                               estrategia_distintos = "exacta",
                               politica_costo = c("todas", "ninguna",
                                                   "por_cardinalidad", "cardinalidad"),
                               umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI) {
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, muestra = muestra,
    orden_muestra = orden_muestra, modo = modo, metricas = metricas,
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
    umbral_cardinalidad = umbral_cardinalidad
  )
  es_numerico <- vapply(seq_along(preparacion$campos), function(i) {
    .es_numerico_dbi(
      preparacion$prototipo[[i]],
      if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  consultas_antes_agregados <- preparacion$presupuesto$usadas
  # La corrida real cuenta el universo antes de construir un porcentaje para
  # TABLESAMPLE. El plan no ejecuta ese COUNT(*), pero si debe publicarlo en la
  # cantidad prevista para que su total siga coincidiendo con la corrida.
  muestreo_plan <- identical(preparacion$modo, "muestreado") &&
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
      )
      as.integer(sondas_muestreo) + as.integer(conteo_muestreo)
    } else {
      0L
    }
  # El plan no consulta la tabla para despejar la cardinalidad. Un catalogo
  # estructural ya leido puede fijar la decision; lo demas abre un rango entre
  # omitir y ejecutar las metricas caras. La corrida medira lo que la politica
  # explicita necesite, una sola vez.
  decisiones_costo <- .decisiones_costo_dbi(
    conexion, preparacion$campos,
    list(conteos = stats::setNames(
      vector("list", length(preparacion$campos)), preparacion$campos
    )),
    preparacion$politica_costo, preparacion$n_total, preparacion$modo,
    fuentes_cardinalidad_costo = preparacion$fuentes_cardinalidad_costo
  )
  columnas_moda_plan <- NULL
  columnas_moda_max <- NULL
  columnas_mediana_plan <- NULL
  columnas_mediana_max <- NULL
  if (identical(preparacion$politica_costo$nombre, "por_cardinalidad") &&
      isTRUE(incluir_valores) &&
      any(.METRICAS_COSTOSAS_DBI %in% preparacion$metricas)) {
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
    modo = preparacion$modo,
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
    columnas_mediana_max = columnas_mediana_max
  )
  attr(plan, "total") <- sum(plan$n_consultas)
  extra <- attr(plan, "extra_si_se_rechazan_lotes", exact = TRUE)
  if (is.null(extra)) extra <- 0
  attr(plan, "total_minimo") <- sum(plan$n_consultas)
  attr(plan, "total_maximo") <- sum(plan$n_consultas_max) + extra
  attr(plan, "total_lotes_rechazados") <- attr(plan, "total_maximo")
  attr(plan, "extra_si_se_rechazan_lotes") <- NULL
  # El total es un RANGO, no una prediccion exacta ni un techo. Ademas de las
  # sondas por rechazo de lotes, la cardinalidad desconocida deja abiertas las
  # metricas caras que la politica puede omitir despues de medir.
  attr(plan, "supuesto") <- paste(
    "El plan no escanea datos para decidir el costo. Cuando la fuente de",
    "cardinalidad es desconocida, `total` supone que la politica omite las",
    "metricas caras y `total_maximo` que las ejecuta; la corrida mide",
    "`distintos` si la politica lo necesita y sigue esa decision.",
    "Las fuentes estructurales exactas cierran ese intervalo. En cualquiera",
    "de los dos extremos, si el motor rechaza un lote se vuelve a sondear el",
    "arbol de biseccion, hasta 2n - 1 consultas adicionales por lote; las",
    "respuestas aceptadas se reutilizan."
  )
  attr(plan, "columnas") <- length(preparacion$campos)
  attr(plan, "columnas_numericas") <- sum(es_numerico)
  attr(plan, "columnas_ilegibles") <- preparacion$esquema$ilegibles
  attr(plan, "dialecto") <- preparacion$dialecto$nombre
  attr(plan, "consultas_emitidas") <- preparacion$presupuesto$usadas
  attr(plan, "metricas") <- preparacion$metricas
  attr(plan, "metricas_ejecucion") <- preparacion$metricas_ejecucion
  attr(plan, "politica_costo") <- preparacion$politica_costo
  attr(plan, "estrategia_distintos") <- .publicar_estrategia_distintos_dbi(
    preparacion$estrategia_distintos
  )
  attr(plan, "fuente_cardinalidad_costo") <-
    preparacion$fuentes_cardinalidad_costo
  attr(plan, "moda_guardian") <- .publicar_moda_guardian_dbi(
    preparacion$moda_guardian_resolucion
  )
  attr(plan, "mediana_consolidada") <- preparacion$mediana_consolidada_resolucion
  attr(plan, "mediana_escalar") <- .publicar_mediana_escalar_dbi(
    preparacion$mediana_escalar_resolucion
  )
  attr(plan, "filas") <- preparacion$n_total
  attr(plan, "muestra") <- if (identical(
    preparacion$bloque_muestra, "con_muestra"
  )) preparacion$muestra else NA_real_
  attr(plan, "tamano_lote") <- preparacion$tamano_lote_planos
  attr(plan, "tamano_lote_planos") <- preparacion$tamano_lote_planos
  attr(plan, "tamano_lote_distintos") <- preparacion$tamano_lote_distintos
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
    plan, preparacion$n_total, preparacion$muestra, sum(es_texto)
  )
  attr(plan, "filas_leidas") <- trabajo$filas_leidas
  attr(plan, "ordenaciones_completas") <- trabajo$ordenaciones
  attr(plan, "columnas_texto") <- trabajo$columnas_texto
  attr(plan, "pares_texto") <- trabajo$pares_texto
  attr(plan, "magnitud_motor") <- trabajo$magnitud_motor
  attr(plan, "magnitud_texto") <- trabajo$magnitud_texto
  attr(plan, "magnitud") <- trabajo$magnitud
  attr(plan, "supuesto_costo") <- .SUPUESTO_TRABAJO_DBI
  class(plan) <- c("plan_perfilado_dbi", class(plan))
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
    .miles_dbi(filas), " filas y ",
    .miles_dbi(attr(x, "columnas", exact = TRUE)), " columnas (dialecto ",
    attr(x, "dialecto", exact = TRUE), ")"
  ))
  if (identical(attr(x, "bloque_muestra", exact = TRUE), "solo_agregados")) {
    cli::cli_text(
      "Perfil de muestra: no solicitado; el plan incluye solo agregados SQL."
    )
  }
  magnitud <- attr(x, "magnitud", exact = TRUE)
  if (is.null(magnitud)) magnitud <- "desconocida"
  if (identical(magnitud, "desconocida")) {
    cli::cli_alert_warning(paste(
      "No se pudo estimar el trabajo: falta el n\u00famero de filas.",
      "El conteo de consultas sigue siendo v\u00e1lido."
    ))
    supuesto_costo <- attr(x, "supuesto_costo", exact = TRUE)
    if (!is.null(supuesto_costo)) cli::cli_text(supuesto_costo)
    techo <- attr(x, "supuesto", exact = TRUE)
    if (!is.null(techo)) cli::cli_text(techo)
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
    # avisaba, pero quien no conociera `modo = 'muestreado'` -que baja esa misma
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
        "modo = 'muestreado' mide sobre una muestra que trae el motor",
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
  proyeccion <- attr(x, "costo_distintos", exact = TRUE)
  if (!is.null(proyeccion) && isTRUE(proyeccion$disponible)) {
    cli::cli_text(paste0(
      "Costo de `COUNT(DISTINCT)` proyectado: ~",
      .segundos_dbi(proyeccion$duracion_estimada_ms), " s para ",
      proyeccion$n_lotes, " lote(s). Fuente: ", proyeccion$fuente,
      ". Es una estimacion, no una medicion."
    ))
  }
  # `muestra = Inf` -lo que viene por omision- trae la tabla entera a R. Es lo
  # correcto para un analisis de calidad: los diagnosticos que miran los valores
  # -patrones, formatos, casi-duplicados- solo ven lo que se les trae, y sin
  # `orden_muestra` una muestra acotada son las PRIMERAS filas del motor, no una
  # al azar. Pero conviene decirlo antes y no despues, porque sobre una tabla
  # grande es lo que manda el reloj.
  muestra_plan <- attr(x, "muestra", exact = TRUE)
  if (!identical(attr(x, "bloque_muestra", exact = TRUE), "solo_agregados") &&
      !is.null(muestra_plan) && !is.finite(muestra_plan)) {
    filas_plan <- attr(x, "filas", exact = TRUE)
    cli::cli_text(
      "El perfil de muestra trae la tabla entera",
      if (!is.null(filas_plan) && !is.na(filas_plan)) {
        paste0(" -", .miles_dbi(filas_plan), " filas-")
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
.motivo_omision_muestra_dbi <- function(modo, columnas, sondas, motivo_motor) {
  n <- length(columnas)
  lista <- paste(columnas, collapse = ", ")
  if (identical(modo, "descarte")) {
    paste0(
      "La lectura completa de la muestra fallo. Se aislo por descarte, con ",
      sondas, if (sondas == 1L) " sonda" else " sondas", " al motor, ",
      if (n == 1L) {
        "la columna que no se puede leer: "
      } else {
        paste0("las ", n, " columnas que no se pueden leer: ")
      },
      lista,
      ". Cada una fallo por si sola y el resto se leyo junto. El motor dijo: ",
      motivo_motor
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
      ". No se comprobo que sean la causa; el motor dijo: ", motivo_motor
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
                                muestra, orden_muestra, orden_sql, dialecto,
                                n_total, presupuesto, info_conexion,
                                argumentos, muestreo = NULL,
                                tipos_declarados = NULL,
                                trazador = NULL) {
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
  n_obtener <- if (total_conocido) min(total_numero, muestra) else muestra
  usa_muestreo <- !is.null(muestreo) && isTRUE(muestreo$disponible)
  # La receta de la lectura estaba escrita una sola vez y el reintento la
  # rehacia a mano, asi que perdia por el camino el muestreo del motor: volvia a
  # una lectura de primeras filas mientras `metodo` seguia declarando
  # `TABLESAMPLE`. Ahora la arma la misma funcion para cualquier subconjunto de
  # columnas, y lo que se declara sale de lo que se emitio.
  armar_muestra_dbi <- function(indices) {
    sub_sql <- campos_sql[indices]
    origen <- if (usa_muestreo) {
      .fuente_muestreada_dbi(
        tabla_sql, sub_sql, muestra, n_total, dialecto,
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
    recorte <- if (!is.null(origen)) {
      NULL
    } else if (!total_conocido || muestra < total_numero) {
      dialecto$limitar(base, muestra, 0)
    } else {
      NULL
    }
    list(
      fuente = origen,
      sql = if (is.null(recorte)) base else recorte,
      filas = if (!is.null(origen)) {
        origen$filas
      } else if (is.null(recorte) && (!total_conocido || muestra < total_numero)) {
        muestra
      } else {
        -1L
      },
      acotado_en = if (!is.null(origen)) {
        "motor_muestreo"
      } else if (!is.null(recorte)) {
        "motor"
      } else if (!total_conocido || muestra < total_numero) {
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
  armado <- armar_muestra_dbi(seq_along(campos_sql))
  fuente <- armado$fuente
  sql_muestra <- armado$sql
  filas <- armado$filas
  acotado_en <- armado$acotado_en
  muestreo_meta <- list(
    filas_solicitadas = as.numeric(muestra),
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
  lectura_inicio <- if (isTRUE(presupuesto$instrumentar)) .ahora_dbi() else NULL
  lectura_cpu_inicio <- if (isTRUE(presupuesto$instrumentar)) {
    .ahora_cpu_dbi()
  } else NULL
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
    modo_omision <- NA_character_
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
        modo_omision <- "tipo_declarado"
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
          modo_omision <- "descarte"
        }
      }
    }
    if (!is.null(recuperado)) {
      campos_omitidos <- campos[recuperado$omitidas]
      motivo_omision <- .motivo_omision_muestra_dbi(
        modo_omision, campos_omitidos, sondas_descarte, motivo_original
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
      muestreo_meta$omision_comprobada <- identical(modo_omision, "descarte")
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
  .registrar_etapa_dbi(
    trazador, "lectura_muestra", lectura_inicio, .ahora_dbi(),
    cpu_inicio = lectura_cpu_inicio, cpu_fin = .ahora_cpu_dbi()
  )
  muestreo_meta$filas_obtenidas <- as.numeric(n_obtenidas)
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

.preparar_dbi <- function(conexion, tabla, muestra, orden_muestra, modo,
                          metricas, max_consultas, dialecto, tamano_lote = NULL,
                          tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                          tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                          bloque_muestra, instrumentar = TRUE,
                          contar = TRUE, incluir_valores = TRUE,
                          estrategia_distintos = "exacta",
                          politica_costo = "todas",
                          umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
                          contar_muestreo = TRUE,
                          sondar_muestreo = TRUE) {
  .requerir_dbi()
  modo <- match.arg(
    modo, c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  )
  dialecto <- match.arg(
    dialecto, c("auto", "limit", "top", "fetch_first", "rownum", "portable")
  )
  muestra <- .validar_muestra_dbi(muestra)
  bloque_muestra <- match.arg(
    bloque_muestra, c("con_muestra", "solo_agregados")
  )
  if (identical(bloque_muestra, "solo_agregados")) {
    orden_muestra <- NULL
  }
  metricas_solicitadas <- .validar_metricas_dbi(metricas, modo)
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
  if (identical(modo, "muestreado")) {
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
  }
  estrategia_distintos <- .estrategia_distintos_dbi(
    metricas_solicitadas, politica_costo, incluir_valores,
    estrategia_distintos
  )
  estrategia_distintos <- .resolver_estrategia_distintos_dbi(
    conexion, estrategia_distintos,
    presupuesto, "distintos" %in% metricas
  )
  fuentes_cardinalidad_costo <- .fuentes_cardinalidad_vacias_dbi(campos)
  catalogo_cardinalidad <- NULL
  # La disponibilidad gobierna la medicion de cardinalidad, no la lectura de
  # una garantia estructural que no recorre la tabla. Una estrategia omitida o
  # sin capacidad no puede medir, pero tampoco debe tapar una clave conocida.
  if (isTRUE(estrategia_distintos$para_costo)) {
    fuentes <- .resolver_fuentes_cardinalidad_dbi(
      conexion, tabla, campos, estrategia_distintos, presupuesto
    )
    fuentes_cardinalidad_costo <- fuentes$fuentes
    catalogo_cardinalidad <- fuentes$catalogo
  }
  fuentes_no_exactas <- vapply(
    fuentes_cardinalidad_costo,
    function(x) !isTRUE(x$exacta), logical(1L)
  )
  estrategia_distintos$requiere_medicion <-
    (isTRUE(estrategia_distintos$publica) &&
       isTRUE(estrategia_distintos$disponible) &&
       (identical(modo, "muestreado") || any(fuentes_no_exactas))) ||
    (isTRUE(estrategia_distintos$para_costo) &&
       isTRUE(estrategia_distintos$disponible) && any(fuentes_no_exactas))
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
  if (identical(modo, "aproximado")) {
    if ("mediana" %in% metricas && any(es_numerico)) {
      resolucion_mediana <- if (!is.null(mediana_consolidada_resolucion) &&
                                isTRUE(mediana_consolidada_resolucion$disponible)) {
        mediana_consolidada_resolucion
      } else {
        .sondar_aproximacion_dbi(conexion, "mediana", presupuesto)
      }
      aproximaciones_resolucion$mediana <- resolucion_mediana
      if (!isTRUE(resolucion_mediana$disponible)) {
        aproximaciones$mediana <- NULL
      } else {
        aproximaciones$mediana <- resolucion_mediana$candidato
      }
    }
  }
  if ("mediana" %in% metricas && isTRUE(incluir_valores) &&
      any(es_numerico) && is.null(mediana_consolidada) &&
      is.null(aproximaciones$mediana)) {
    mediana_escalar_resolucion <- .sondar_mediana_escalar_dbi(
      conexion, resolucion$dialecto, presupuesto,
      materializar = identical(modo, "muestreado")
    )
    if (isTRUE(mediana_escalar_resolucion$disponible)) {
      mediana_escalar <- mediana_escalar_resolucion$candidato
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
    isTRUE(contar_muestreo) && identical(modo, "muestreado") &&
      !is.null(muestreo) &&
      !is.null(muestreo$candidato) &&
      identical(muestreo$candidato$tipo, "tablesample")
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
  columnas_distintos_ejecucion <- if ("distintos" %in% metricas) {
    if (isTRUE(estrategia_distintos$publica)) {
      campos
    } else {
      campos[vapply(
        fuentes_cardinalidad_costo,
        function(x) !isTRUE(x$exacta), logical(1L)
      )]
    }
  } else {
    character()
  }
  list(
    modo = modo, metricas = metricas_solicitadas,
    metricas_ejecucion = metricas, muestra = muestra,
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
    politica_costo = politica_costo,
    estrategia_distintos = estrategia_distintos,
    fuentes_cardinalidad_costo = fuentes_cardinalidad_costo,
    catalogo_cardinalidad = catalogo_cardinalidad,
    columnas_distintos_ejecucion = columnas_distintos_ejecucion,
    n_total = n_total, conteo = conteo, sql_conteo = sql_conteo,
    conteo_fusionable = hay_agregados_fusionables && !(
      identical(modo, "muestreado") && !is.null(muestreo) &&
        !is.null(muestreo$candidato) &&
        identical(muestreo$candidato$tipo, "tablesample")
    ),
    orden_sql = orden_sql,
    orden_muestra = orden_muestra, dialecto = resolucion$dialecto,
    resolucion = resolucion, campos_declarados = campos_declarados,
    lista_campos = lista_campos
  )
}

#' Perfilar una muestra leída mediante DBI
#'
#' Calcula en SQL un resumen sobre la tabla completa o sobre una relación
#' muestreada por el motor, según `modo`, y, por omisión, en un bloque separado
#' ejecuta [perfilar()] sobre una muestra traída a memoria. El resumen completo
#' de 105 campos no se presenta como calculado por la base: esos campos
#' pertenecen exclusivamente a `perfil_muestra` y su universo es la muestra.
#' `bloque_muestra = "solo_agregados"` permite omitir esa lectura y pedir sólo
#' los agregados SQL.
#'
#' Esta función no escribe en la conexión ni crea objetos temporales. `DBI` es
#' una dependencia opcional. Cada agregado no disponible queda en `NA` y su
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
#' no disponible.
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
#' `modo = "muestreado"` sondea las formas declaradas por el adaptador y usa
#' `TABLESAMPLE` cuando el motor lo acepta, o una función pseudoaleatoria con el
#' límite del dialecto. Si ninguna forma es compatible, las métricas SQL quedan
#' en `no_disponible`: no se sustituyen por resultados de la tabla completa.
#' Cada registro publica `alcance`, `universo`, `tamano_muestra`, `fraccion`,
#' `metodo` y `error_esperado`. En `resumen_tabla$meta$muestreo`,
#' `tamano_muestra` conserva el nombre historico y declara el tamano efectivo
#' solicitado a la consulta; `filas_solicitadas` declara el pedido original y
#' `filas_obtenidas` las filas que devolvio la lectura del bloque
#' `perfil_muestra`. Esta ultima puede ser `NA` si el bloque no se solicito o
#' fallo antes de leer.
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
#' exacto como repliegue. `"catalogo"` esta declarada pero queda
#' `no_disponible` en esta version porque todavia no implementa una estadistica
#' de cardinalidad; en particular, no usa `pg_stats`. `"omitida"` no emite la
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
#' hubo lectura instantanea. La cobertura agrega una entrada concreta solo si
#' `n_validos` y `n_distintos` son exactos, incoherentes y provienen de grupos
#' distintos; su motivo conserva ambas sentencias.
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
#' PostgreSQL se usan `%` y `/` con division entera. Los dialectos que no
#' declaran esa forma conservan las dos consultas y lo publican en el metodo
#' de `resumen_tabla$sql`; `PERCENTILE_CONT` no cambia.
#'
#' @section Costo:
#' Los agregados de una tabla ancha se emiten por lotes; `muestra` acota lo que
#' se trae a R, no el trabajo del motor. `bloque_muestra` decide si se trae esa
#' muestra; `modo`, `metricas`, `tamano_lote_planos`, `tamano_lote_distintos` y
#' `max_consultas` acotan el trabajo SQL. [plan_perfilado_dbi()] dice cuántas
#' consultas se van a emitir antes de emitirlas. El orden de degradación es
#' agregados planos, total del universo cuando hace falta, distintos, moda y
#' mediana. Los
#' agregados planos sobre la misma tabla y filtro —`COUNT(col)`,
#' mínimos, máximos, medias, ceros, negativos y desvío— comparten una consulta
#' por lote y cada consulta que trae `n_validos` lleva además
#' `COUNT(*) AS n_total_consulta` en la misma sentencia. La completitud usa ese
#' denominador local, no el total de otro lote. El total del universo se conserva
#' por separado cuando el perfil se calcula sobre una muestra. Si el lote
#' completo es rechazado, sus mitades se sondean por bisección: los grupos
#' aceptados se reutilizan como mediciones y las columnas culpables se reintentan
#' por métrica, con su denominador local. Las fuentes `TABLESAMPLE` que necesitan
#' el total del universo para escribir un porcentaje lo cuentan antes.
#' `COUNT(DISTINCT ...)` queda en una clase separada y usa su propio tamaño de
#' lote, conservador por omisión porque una cardinalidad puede derramar mucho
#' más que veinte agregados planos; la consulta exacta trae su
#' `n_validos_guard` compañero.
#' Antes de la primera consulta exacta se anuncia su costo sólo si hay una
#' proyección temporal fundada en agregados planos medidos en esta corrida.
#' La fuente y el valor quedan en `meta$costo_distintos`; no se usa una
#' predicción basada en `reltuples`.
#' Lo que no entra en el presupuesto queda en `no_disponible` con su motivo,
#' nunca en cero. `meta$tamano_lote_funciono` conserva el mayor lote aceptado
#' durante esa corrida; no se guarda estado global asociado a la conexión.
#'
#' @section Instrumentación:
#' `resumen_tabla$sql` conserva una fila por métrica y agrega la duración de la
#' consulta que la respalda, las filas devueltas y los bytes que ese resultado
#' ocupa en R. `consulta_id` identifica el intento dentro de la corrida y
#' `id_muestra` identifica la consulta de datos que produjo la medición: dos
#' métricas con el mismo identificador vieron exactamente las mismas filas y se
#' pueden comparar directamente. `NA` declara que esa garantía no se puede
#' hacer; en particular, las métricas por columna —moda, frecuencia de la moda
#' y mediana— no comparten filas con otras métricas. `etapa` permite agruparlo
#' (`conteos`, `moda`, `basicos`, `mediana`,
#' `desvio`, `lectura_muestra` y las sondas). Las métricas no solicitadas o que
#' no emitieron consulta conservan esos campos y los dejan en `NA`; en
#' particular, `NA` no significa cero.
#'
#' En PostgreSQL, con `instrumentar = TRUE`, se toma una foto de
#' `pg_stat_statements` antes y después de los `COUNT(DISTINCT)` exactos. Sólo
#' se publica un derrame cuando una consulta coincide y su contador aumentó en
#' exactamente una llamada atribuible a esta corrida. En ese caso,
#' `resumen_tabla$sql` agrega `derrame`,
#' `bloques_temporales_leidos`, `bloques_temporales_escritos` y
#' `fuente_derrame`; `resumen_tabla$meta$derrame` conserva el resumen y la
#' fuente. Si la extensión no está disponible, la consulta fue concurrente o
#' la instrumentación está apagada, el estado queda `no_disponible` o
#' `no_medido` con el motivo: el paquete no deduce un derrame del tiempo y no
#' modifica `work_mem`.
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
#' @param muestra Cantidad positiva de filas solicitadas para el perfil de
#'   muestra, o `Inf` para traer la tabla entera. Con `Inf` la consulta sale sin
#'   `LIMIT` y `tabla_completa` queda en `TRUE`.
#'
#'   El resumen de tabla **no** se muestrea: con `modo = "exacto"` se calcula en
#'   el motor sobre todas las filas. Lo que sale de esta muestra son los
#'   diagnosticos que necesitan los valores en R -patrones, formatos,
#'   casi-duplicados-, y sin `orden_muestra` no son una muestra aleatoria sino
#'   las primeras filas que devuelva el motor. Medido sobre una tabla de 200.000
#'   filas con un defecto plantado al final: con el valor por omision aparecen
#'   tres hallazgos y con `Inf` aparecen cinco, a cambio de 10 segundos en vez
#'   de 2. Un analisis de calidad no se corre todos los dias; si el tiempo no es
#'   la restriccion, `Inf` es la opcion honesta.
#' @param orden_muestra Columnas para `ORDER BY`. La salida solo declara orden
#'   reproducible cuando la combinación es única en toda la tabla. Sin este
#'   argumento, DBI no garantiza el orden ni la pertenencia de una muestra
#'   limitada, y `meta` lo declara expresamente. No se usa cuando
#'   `bloque_muestra = "solo_agregados"`.
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
#' @param modo Conjunto de métricas del resumen: `"exacto"` las calcula todas,
#'   `"seguro"` evita las que ordenan o agrupan la tabla completa y
#'   `"conteos"` deja solo el conteo de valores no nulos, `"muestreado"`
#'   calcula estimaciones sobre filas elegidas por el motor y `"aproximado"`
#'   usa funciones nativas aproximadas para las métricas que ese modo define.
#' @param metricas Selección explícita de grupos de métricas, que tiene
#'   prioridad sobre `modo`: `"validos"`, `"distintos"`, `"moda"`,
#'   `"basicos"`, `"mediana"` y `"desvio"`.
#' @param estrategia_distintos Procedencia explícita para `n_distintos`:
#'   `"exacta"` (por omisión) emite `COUNT(DISTINCT)`; `"aproximada_motor"`
#'   usa una función nativa aceptada por el motor y deja la métrica en
#'   `no_disponible` si no existe; `"catalogo"` queda declarada pero
#'   `no_disponible` hasta implementar la estadística del catálogo; y
#'   `"omitida"` no emite ninguna consulta. No hay repliegue automático entre
#'   estrategias. El resultado publica `estrategia_solicitada`,
#'   `estrategia_resuelta` y `estado` en `meta$estrategia_distintos`, y las dos
#'   primeras también en `resumen_tabla$sql`.
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
#'   solicitadas. `"ninguna"` es un alias de `"todas"`; `"por_cardinalidad"`
#'   (también `"cardinalidad"`) resuelve primero las fuentes estructurales y
#'   mide valores válidos y distintos sólo cuando hace falta y la estrategia lo
#'   permite. Luego omite, por columna, moda y mediana cuando la proporción de
#'   distintos alcanza `umbral_cardinalidad`. Una estrategia no disponible no
#'   se convierte en una medición exacta.
#' @param umbral_cardinalidad Proporción entre valores distintos y válidos que
#'   activa `politica_costo = "por_cardinalidad"`. El valor por omisión es `0.95`
#'   sólo cuando esa política se pide explícitamente; se puede mover en cada
#'   llamada. Para pedir todas las métricas use `politica_costo = "todas"`.
#' @param bloque_muestra Qué bloques se solicitan: `"con_muestra"` (por
#'   omisión) calcula también `perfil_muestra`, o `"solo_agregados"` omite su
#'   lectura y devuelve sólo los agregados SQL. La segunda opción no cambia el
#'   alcance de esos agregados: eso lo decide `modo`.
#' @param instrumentar Si se cronometra cada consulta y las etapas grandes de R
#'   y, en PostgreSQL, se intenta atribuir el uso de bloques temporales de los
#'   `COUNT(DISTINCT)` exactos mediante `pg_stat_statements`.
#'   Por omisión es `TRUE`; agrega `duracion_ms`, `cpu_ms`,
#'   `n_filas_resultado`, `bytes_resultado_r`, `consulta_id` y `etapa` a
#'   `resumen_tabla$sql`, y el resumen `resumen_tabla$tiempos`. Con `FALSE` se
#'   conserva el mismo plan, la misma cantidad y el mismo orden de consultas,
#'   pero los campos medibles quedan en `NA`. `id_muestra` **no** depende de
#'   esta opcion: no es una medicion sino un hecho estructural sobre que
#'   consulta produjo cada metrica, y se publica igual con `FALSE`. Las
#'   duraciones usan `Sys.time()` y el CPU del cliente usa la suma de
#'   `proc.time()[c("user.self", "sys.self")]`. `cpu_ms` es cero cuando el
#'   proceso no consumió CPU; `NA` significa que no se pudo medir. Los
#'   intervalos que el reloj no puede resolver no se publican como cero.
#' @param ... Argumentos enviados a [perfilar()] para analizar la muestra.
#'
#' @return Objeto de clase `perfil_dbi` con dos componentes: `resumen_tabla`, de
#'   alcance completo o muestreado según `modo`, y `perfil_muestra`, un objeto
#'   `perfil` cuyo `meta$origen_dbi` declara tabla, conexión, SQL y alcance.
#'   `perfil_muestra` es `NULL` si la muestra no se pudo obtener o si se pidió
#'   `bloque_muestra = "solo_agregados"`; `resumen_tabla$cobertura` distingue
#'   esos casos con `no_disponible` y `no_solicitado`, respectivamente.
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
perfilar_dbi <- function(conexion, tabla, muestra = Inf,
                         orden_muestra = NULL,
                         modo = c("exacto", "seguro", "conteos", "muestreado",
                                   "aproximado"),
                         metricas = NULL, max_consultas = Inf,
                         dialecto = "auto", incluir_valores = TRUE,
                         tamano_lote = NULL,
                         tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
                         tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
                         bloque_muestra = c("con_muestra", "solo_agregados"),
                         instrumentar = TRUE,
                         estrategia_distintos = "exacta",
                         politica_costo = c("todas", "ninguna",
                                             "por_cardinalidad", "cardinalidad"),
                         umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
                         ...) {
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, muestra = muestra,
    orden_muestra = orden_muestra, modo = modo, metricas = metricas,
    max_consultas = max_consultas, dialecto = dialecto,
    tamano_lote = tamano_lote,
    tamano_lote_planos = tamano_lote_planos,
    tamano_lote_distintos = tamano_lote_distintos,
    bloque_muestra = bloque_muestra, instrumentar = instrumentar, contar = FALSE,
    contar_muestreo = TRUE,
    incluir_valores = incluir_valores,
    estrategia_distintos = estrategia_distintos,
    politica_costo = politica_costo,
    umbral_cardinalidad = umbral_cardinalidad
  )
  presupuesto <- preparacion$presupuesto
  info_conexion <- .info_conexion_dbi(conexion)
  es_numerico <- vapply(seq_along(preparacion$campos), function(i) {
    .es_numerico_dbi(
      preparacion$prototipo[[i]],
      if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  plan <- .plan_consultas_dbi(
    preparacion$campos, es_numerico, preparacion$metricas_ejecucion, incluir_valores,
    length(preparacion$orden_sql) > 0 &&
      identical(preparacion$bloque_muestra, "con_muestra"), preparacion$dialecto,
    emitidas = presupuesto$usadas, modo = preparacion$modo,
    muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
      preparacion$muestreo$disponible,
    tamano_lote_planos = preparacion$tamano_lote_planos,
    tamano_lote_distintos = preparacion$tamano_lote_distintos,
    columnas_distintos = preparacion$columnas_distintos_ejecucion,
    incluir_muestra = identical(preparacion$bloque_muestra, "con_muestra"),
    mediana_consolidada = !is.null(preparacion$mediana_consolidada)
  )
  # Las consultas obligatorias que faltan -verificacion de orden y, cuando se
  # pidio, muestra- se reservan para que el presupuesto no se las coma.
  presupuesto$reserva <- if (identical(preparacion$bloque_muestra, "con_muestra")) {
    if (length(preparacion$orden_sql)) 2 else 1
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
  tabla_metricas_sql <- preparacion$tabla_sql
  if (identical(preparacion$modo, "muestreado") &&
      !is.null(preparacion$muestreo) &&
      isTRUE(preparacion$muestreo$disponible)) {
    fuente_muestreada <- .fuente_muestreada_dbi(
      preparacion$tabla_sql, preparacion$campos_sql, preparacion$muestra,
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
     modo = preparacion$modo, tabla_metricas_sql = tabla_metricas_sql,
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
    fuentes_cardinalidad_costo = preparacion$fuentes_cardinalidad_costo,
    estrategia_distintos = preparacion$estrategia_distintos,
    politica_costo = preparacion$politica_costo
  )
  derrame <- .publicar_derrame_dbi(presupuesto)
  resumen$sql <- .adjuntar_derrame_sql_dbi(
    resumen$sql, presupuesto$derrame
  )
  resumen$meta$derrame <- derrame
  resumen$meta$costo_distintos <- presupuesto$proyeccion_distintos
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
      modo = preparacion$modo,
      muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
        preparacion$muestreo$disponible,
      tamano_lote_planos = preparacion$tamano_lote_planos,
      tamano_lote_distintos = preparacion$tamano_lote_distintos,
      columnas_distintos = preparacion$columnas_distintos_ejecucion,
      incluir_muestra = identical(preparacion$bloque_muestra, "con_muestra"),
      mediana_consolidada = !is.null(preparacion$mediana_consolidada),
      columnas_moda = columnas_moda, columnas_moda_max = columnas_moda,
      columnas_mediana = columnas_mediana,
      columnas_mediana_max = columnas_mediana
    )
  }
  if (!is.null(presupuesto$proyeccion_distintos)) {
    attr(plan, "costo_distintos") <- presupuesto$proyeccion_distintos
  }
  attr(plan, "moda_guardian") <- .publicar_moda_guardian_dbi(
    preparacion$moda_guardian_resolucion
  )
  # En la corrida el conteo sale de la primera consulta de agregados. Desde
  # aca es el total que gobierna el denominador, la muestra y toda la metadata;
  # no se conserva el valor desconocido de la preparacion.
  preparacion$n_total <- resumen$meta$filas
  if (identical(preparacion$modo, "muestreado")) {
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
  resumen$meta$modo <- preparacion$modo
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
  resumen$meta$incluir_valores <- incluir_valores
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
  if (identical(preparacion$modo, "aproximado")) {
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
  if (identical(preparacion$modo, "muestreado")) {
    resumen$meta$muestreo <- muestreo_publico
    if (is.null(fuente_muestreada)) {
      cobertura <- rbind(cobertura, .registro_cobertura_dbi(
        "resumen_tabla", .texto_tabla_dbi(tabla), "no_disponible",
        if (is.null(preparacion$muestreo)) {
          "No se pudo resolver una capacidad de muestreo del motor."
        } else {
          preparacion$muestreo$motivo
        },
        paste(
          "El modo `muestreado` no reemplaza la estimacion por un calculo",
          "sobre la tabla completa. Usar otro modo o un adaptador compatible."
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
      preparacion$campos_sql, preparacion$muestra, preparacion$orden_muestra,
      preparacion$orden_sql, preparacion$dialecto, preparacion$n_total,
      presupuesto, info_conexion, list(...), muestreo = muestreo_meta,
      tipos_declarados = preparacion$tipos, trazador = trazador
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
          "unicamente los agregados SQL del modo elegido."
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
  if (identical(preparacion$modo, "muestreado")) {
    resumen$meta$muestreo <- muestreo_publico
  }
  cobertura <- rbind(cobertura, bloque$cobertura)
  resumen$tiempos <- .resumen_tiempos_dbi(trazador)
  if (isTRUE(presupuesto$agotado)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "resumen_tabla", .texto_tabla_dbi(tabla), "presupuesto_agotado",
      .motivo_presupuesto_dbi(presupuesto),
      paste0(
        "Subir `max_consultas` o reducir el trabajo con `modo` o `metricas`. ",
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
      " consulta(s) de `COUNT(DISTINCT)`: ",
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

#' @export
print.perfil_dbi <- function(x, ...) {
  meta <- x$resumen_tabla$meta
  alcance <- if (identical(meta$alcance, "tabla_muestreada")) {
    "tabla muestreada"
  } else {
    "tabla completa"
  }
  cli::cli_text("Perfil DBI de {.strong {meta$tabla}}")
  cli::cli_text(
    "Resumen de {alcance}: {nrow(x$resumen_tabla$columnas)} columnas sobre {meta$filas} filas"
  )
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
