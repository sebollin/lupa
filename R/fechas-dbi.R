# Resumen de fechas y periodos en el motor (`perfilar_dbi(resumen_fechas = TRUE)`
# o `periodos`).
#
# El motor no calcula epocas ni hace aritmetica con las fechas: compara contra
# literales -los de `.literal_sql_medicion()`, los mismos de `medir_dbi()`- y
# devuelve valores de la columna (`MIN`, `MAX`, `MAX(CASE WHEN x < literal THEN
# x END)`), que R lee por el mismo controlador que lee la columna. Con eso la
# regla de `dominios.R` decide igual que en memoria: la reserva de cada
# candidata (`.dominio_centinela_aislado_extremos()`), su minimo de repeticiones
# (`.dominio_minimo_centinela()`), las lecturas (`.dominio_lecturas_fecha()`) y
# la fila publicada (`.fila_resumen_fechas()`) son las mismas funciones.
#
# Lo que el motor no puede contar como R lo lee se declara, o se cuenta en R por
# bloques con `estrategia_no_sql = "bloques"`:
# - SQLite guarda en una columna `DATE` o `TIMESTAMP` texto (lo que escribe un
#   CSV importado, otra herramienta): RSQLite lo lee con reglas propias -acepta
#   `1970-1-1`, devuelve `NA` para `1970-01-01T00:00:00`- y la comparacion contra
#   el literal numerico no lo ve. Medido: tres `1970-01-01` guardados como texto
#   daban 0 en el motor y 3 en R. Se mide con `typeof()` en la misma pasada.
# - Un instante a menos de un segundo de una candidata lejana (9999-12-31,
#   0001-01-01) con resolucion de microsegundos: R lo lee como la candidata (el
#   doble ya no distingue el microsegundo ahi) y la comparacion exacta no.
# - Una columna de texto que la muestra lee como fecha: sus formatos los decide
#   la columna entera, no la muestra.
# - Un `TIMESTAMP` de MariaDB, y en DuckDB una conexion que entrega las fechas
#   con el huso forzado (`tz_out_convert = "force"`): lo que R lee no son los
#   instantes que compara el literal.
# - Un periodo con una columna que no corresponde a todas las filas
#   (`aplicabilidad`, `columnas_opcionales`): la consulta cuenta la tabla
#   entera y la memoria, las filas donde corresponden las dos.
# - En DuckDB, un `infinity` en un periodo con las dos columnas de tipos
#   distintos: R lee el infinito de cada tipo como la fecha mas lejana que cabe
#   en ese tipo, y el motor da por iguales los dos. Con las dos columnas del
#   mismo tipo el motor lo cuenta como R lo lee, y no se lo aparta.
# Lo que se cuenta en R lo cuenta la misma funcion que en memoria, sobre la
# columna entera -el resumen de un texto, sobre su vocabulario; un periodo,
# sobre sus columnas-, hasta un tope de filas; pasado el tope se declara y se
# deja de leer. Asi lo contado no depende del orden de las filas ni del tamano
# del bloque, y es lo que da `perfilar()` o no se publica.
# Que camino toma cada columna y cada periodo se pregunta en un solo lugar
# (`.destino_columna_fechas_dbi()`, `.destino_periodo_dbi()`): ahi preguntan
# la corrida y el plan, que anuncia estas consultas (`.filas_plan_fechas_dbi()`)
# y contra el que la corrida las concilia (`.conciliar_fechas_dbi()`).
# SQLite, DuckDB y MariaDB (con RMariaDB, que entrega `DATE` y `DATETIME` como
# `Date` y `POSIXct`; medido contra `perfilar()` sobre el servidor): en los demas
# motores la equivalencia no esta medida y el
# resumen se declara.

.MOTORES_RESUMEN_FECHAS_DBI <- c("sqlite", "duckdb", "mariadb")
# El tope de valores distintos que se traen para decidir si las fechas forman un
# calendario: el `muestra` de `perfilar()`.
.TOPE_DISTINTOS_CALENDARIO_DBI <- 100000L
# El tope de formas distintas de una fecha escrita como texto que se acumulan
# por bloques, y de filas que se reconstruyen para decidir sus formatos.
.TOPE_VOCABULARIO_FECHAS_DBI <- 100000L
.TOPE_FILAS_TEXTO_FECHAS_DBI <- 5e6
.BLOQUE_FECHAS_DBI <- 50000L

# El predicado de SQLite que cuenta valores guardados como texto o blob en una
# columna. Lo usan el resumen de fechas y la guarda de `medir_dbi()` sobre
# columnas de fecha (`.motivo_fecha_texto_sqlite_dbi()`).
.sql_guardado_texto_sqlite_dbi <- function(x) {
  paste0("(typeof(", x, ") IN ('text', 'blob'))")
}

.motivo_fecha_texto_sqlite_dbi <- function(columna, n = NULL) {
  paste0(
    "La columna de fecha `", columna, "` tiene ",
    if (is.null(n)) "valores " else paste0(format(n, scientific = FALSE, trim = TRUE),
                                           if (n == 1L) " valor " else " valores "),
    .FRAGMENTO_TEXTO_MEDIDO_DBI, ": SQLite no los compara contra la fecha como ",
    "R los lee (RSQLite convierte el texto con reglas propias)."
  )
}

# Un valor devuelto por el motor para una columna de fecha, en segundos UTC. Los
# controladores tipados (DuckDB) devuelven `Date` o `POSIXct`; SQLite devuelve el
# numero crudo -dias en una columna `DATE`, segundos en una `TIMESTAMP`-, la
# misma unidad que RSQLite aplica al leer la columna.
.segundos_fecha_dbi <- function(valor, clase) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  if (inherits(valor, "Date")) return(as.numeric(valor[[1L]]) * 86400)
  if (inherits(valor, "POSIXct")) return(as.numeric(valor[[1L]]))
  numero <- .numero_dbi(valor)
  if (identical(clase, "fecha")) numero * 86400 else numero
}

# El literal de un instante (en segundos) para una columna de la clase dada.
# Una fecha de calendario solo vale medianoche: otro instante no tiene literal
# en una columna `DATE` (ninguna fecha vale eso). Un instante con fraccion de
# segundo -la referencia, si la fecha se declaro con `Sys.time()`- lleva los
# microsegundos truncados en DuckDB (exacto para `>` sobre microsegundos
# enteros) y el numero de 17 cifras en SQLite.
.literal_fecha_dbi <- function(conexion, segundos, clase, motor) {
  if (!is.finite(segundos)) return(NA_character_)
  if (identical(clase, "fecha")) {
    if (segundos %% 86400 != 0) return(NA_character_)
    return(.literal_sql_medicion(
      conexion, as.Date(segundos / 86400, origin = "1970-01-01"), "fecha", motor
    ))
  }
  if (segundos == floor(segundos)) {
    return(.literal_sql_medicion(
      conexion, as.POSIXct(segundos, origin = "1970-01-01", tz = "UTC"),
      "instante", motor
    ))
  }
  if (identical(motor, "sqlite")) return(formatC(segundos, digits = 17, format = "g"))
  entero <- floor(segundos)
  micro <- floor(segundos * 1e6) - entero * 1e6
  paste0(
    "TIMESTAMP '",
    format(as.POSIXct(entero, origin = "1970-01-01", tz = "UTC"),
           "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    ".", formatC(micro, width = 6, flag = "0", format = "d"), "'"
  )
}

# El umbral de futura de `.dominio_no_futuro()` llevado a un literal y un
# operador: con la referencia como `Date` cuenta el dia civil UTC (futura desde
# la medianoche siguiente), si no el instante. En una columna `DATE` las dos
# lecturas dan lo mismo: `x * 86400 > ref` si y solo si `x > piso(ref / 86400)`.
.umbral_futura_dbi <- function(conexion, referencia, fecha_es_date, clase, motor) {
  dia <- floor(referencia / 86400)
  if (identical(clase, "fecha")) {
    return(list(op = ">", literal = .literal_fecha_dbi(conexion, dia * 86400, "fecha", motor)))
  }
  if (isTRUE(fecha_es_date)) {
    return(list(op = ">=", literal = .literal_fecha_dbi(conexion, (dia + 1) * 86400, clase, motor)))
  }
  list(op = ">", literal = .literal_fecha_dbi(conexion, referencia, clase, motor))
}

# Los agregados de una columna de fecha calculados en R sobre lo que R lee
# (la via por bloques): los mismos campos que trae la consulta del motor, asi la
# decision es la misma funcion. `v` son segundos (los `NA` ya fuera).
.agregados_fecha_r <- function(v, candidatos, declaradas, referencia,
                               fecha_es_date) {
  futura <- !.dominio_no_futuro(v, referencia, fecha_es_date)
  es_declarada <- if (any(declaradas)) v %in% candidatos[declaradas] else rep(FALSE, length(v))
  utiles <- v[!futura & !es_declarada]
  extremos <- lapply(seq_along(candidatos), function(j) {
    base <- v[!(v %in% candidatos[-j])]
    debajo <- base[base < candidatos[[j]]]
    encima <- base[base > candidatos[[j]]]
    list(
      nb = length(base), mnb = if (length(base)) min(base) else NA_real_,
      mxb = if (length(base)) max(base) else NA_real_,
      deb = if (length(debajo)) max(debajo) else NA_real_,
      enc = if (length(encima)) min(encima) else NA_real_
    )
  })
  list(
    n = length(v), mn = if (length(v)) min(v) else NA_real_,
    mx = if (length(v)) max(v) else NA_real_,
    fut = sum(futura), ult = if (length(utiles)) max(utiles) else NA_real_,
    cuenta = vapply(candidatos, function(c) sum(v == c), numeric(1L)),
    extremos = extremos
  )
}

# Junta los agregados de dos bloques.
.sumar_agregados_fecha <- function(a, b) {
  if (is.null(a)) return(b)
  minimo <- function(x, y) suppressWarnings(min(c(x, y), na.rm = TRUE))
  maximo <- function(x, y) suppressWarnings(max(c(x, y), na.rm = TRUE))
  finito <- function(x) if (is.finite(x)) x else NA_real_
  list(
    n = a$n + b$n, mn = finito(minimo(a$mn, b$mn)), mx = finito(maximo(a$mx, b$mx)),
    fut = a$fut + b$fut, ult = finito(maximo(a$ult, b$ult)),
    cuenta = a$cuenta + b$cuenta,
    extremos = Map(function(x, y) list(
      nb = x$nb + y$nb, mnb = finito(minimo(x$mnb, y$mnb)),
      mxb = finito(maximo(x$mxb, y$mxb)), deb = finito(maximo(x$deb, y$deb)),
      enc = finito(minimo(x$enc, y$enc))
    ), a$extremos, b$extremos)
  )
}

# La decision sobre una columna a partir de sus agregados: la misma regla que
# `.hallazgos_dominio_columna()` aplica sobre los valores. `extremo(j)` da los
# agregados de la base de la candidata `j` (la pasada 2 del motor, o lo que
# acumularon los bloques). `serie()` da el paso de la grilla, o `NA`.
.decidir_fechas_columna <- function(columna, clase, regla, inferida, agregados,
                                    candidatos, etiquetas, declaradas, extremo,
                                    referencia, fecha_es_date, serie) {
  n <- agregados$n
  cuenta <- agregados$cuenta
  fin_abierto <- inferida && isTRUE(regla$fin_vigencia)
  validas <- vapply(seq_along(candidatos), function(j) {
    if (declaradas[[j]]) return(TRUE)
    if (cuenta[[j]] < .dominio_minimo_centinela(etiquetas[[j]], fin_abierto)) {
      return(FALSE)
    }
    e <- extremo(j)
    .dominio_centinela_aislado_extremos(
      e$nb, cuenta[[j]], e$mnb, e$mxb, e$deb, e$enc, candidatos[[j]]
    )
  }, logical(1L))
  incorporadas <- validas & !declaradas
  es_9999 <- candidatos == .dominio_fecha_numero("9999-12-31", columna)
  abierto <- if (fin_abierto) sum(cuenta[incorporadas & es_9999]) else 0
  total <- sum(cuenta[validas])
  n_declaradas <- sum(cuenta[validas & declaradas])
  resto <- total - n_declaradas - abierto
  lectura_centinelas <- if (inferida && resto > 0) {
    .dominio_lecturas_fecha(n, regla, n_centinelas = sum(cuenta[incorporadas]))$centinelas
  } else NA_character_
  # Una regla inferida sin regla de futuro solo aplica si aparece alguna
  # candidata, y ninguna regla aplica sin valores (`.dominio_decidir_inferida()`).
  aplicada <- n > 0 && (!inferida || isTRUE(regla$no_futuras) ||
                          any(cuenta > 0))
  futuras <- "fila"
  if (inferida && isTRUE(regla$no_futuras) && isTRUE(agregados$fut > 0)) {
    futuras <- .dominio_lecturas_fecha(
      n, regla, n_futuras = agregados$fut, minimo = agregados$mn,
      referencia = referencia, serie = serie
    )$futuras
  }
  futura_candidata <- !.dominio_no_futuro(candidatos, referencia, fecha_es_date)
  n_declaradas_no_futuras <- sum(cuenta[validas & declaradas & !futura_candidata])
  .fila_resumen_fechas(
    columna, clase = clase, n_valores = n, regla = regla,
    aplicada = list(aplicada = aplicada, inferida = inferida,
                    sostenimiento = list(futuras = futuras)),
    centinelas = list(
      n = total, n_declaradas = n_declaradas, n_fin_abierto = abierto,
      lectura = lectura_centinelas,
      incorporadas = candidatos[incorporadas & cuenta > 0]
    ),
    cuentas = list(
      n_futuras = agregados$fut, ultimo = agregados$ult,
      n_evaluadas = n - agregados$fut - n_declaradas_no_futuras,
      n_declaradas_no_futuras = n_declaradas_no_futuras
    )
  )
}

# Las candidatas de una regla: etiquetas, segundos y si son declaradas.
.candidatas_regla_dbi <- function(regla, columna) {
  etiquetas <- if (is.null(regla)) character() else as.character(regla$centinelas %||% character())
  if (!length(etiquetas)) {
    return(list(etiquetas = character(), segundos = numeric(), declaradas = logical()))
  }
  declaradas <- if (identical(regla$fuente, "declarado")) {
    rep(TRUE, length(etiquetas))
  } else rep_len(as.logical(regla$centinelas_declaradas %||% FALSE), length(etiquetas))
  list(etiquetas = etiquetas, segundos = .dominio_fecha_numero(etiquetas, columna),
       declaradas = declaradas)
}

# Lee columnas por bloques con el controlador -lo que R lee, con sus avisos- y le
# pasa cada bloque (un frame) a `absorber()`. Si `absorber()` devuelve `FALSE`
# -ya paso su tope- deja de pedir bloques: lo que falta de la tabla no se trae.
# Gasta una consulta del presupuesto.
.leer_columnas_por_bloques_dbi <- function(conexion, tabla_sql, columnas_sql,
                                           presupuesto, absorber) {
  if (!.gastar_dbi(presupuesto)) {
    return(list(ok = FALSE, motivo = .motivo_presupuesto_dbi(presupuesto)))
  }
  avisos <- 0L
  sql <- paste0("SELECT ", paste(columnas_sql, collapse = ", "), " FROM ", tabla_sql)
  salida <- tryCatch({
    res <- .enviar_consulta_dbi(conexion, sql)
    on.exit(.limpiar_consulta_dbi(res, conexion), add = TRUE)
    while (!.dbi("dbHasCompleted", conexion, DBI::dbHasCompleted(res))) {
      bloque <- withCallingHandlers(
        .traer_filas_dbi(res, n = .BLOQUE_FECHAS_DBI, conexion),
        warning = function(w) {
          avisos <<- avisos + 1L
          invokeRestart("muffleWarning")
        }
      )
      seguir <- absorber(bloque)
      if (identical(seguir, FALSE)) break
    }
    list(ok = TRUE, avisos = avisos, sql = sql)
  }, error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e)))
  salida
}

# Una sola columna: la misma lectura, con el bloque como vector.
.leer_columna_por_bloques_dbi <- function(conexion, tabla_sql, columna_sql,
                                          presupuesto, absorber) {
  .leer_columnas_por_bloques_dbi(
    conexion, tabla_sql, paste0(columna_sql, " AS v"), presupuesto,
    function(bloque) absorber(bloque[[1L]])
  )
}

# Lo que se sabe de una columna del resumen de fechas antes de consultar: si se
# declara sin mas ("declarada": con `universo = "muestra_motor"`, o con una
# regla de `aplicabilidad`, que el motor no traduce), si lo que R lee no es lo
# que compara el literal ("forzada": con `"bloques"` se lee, si no se declara)
# o si la cuenta el motor ("sql"). La corrida y el plan preguntan aca, y asi
# cuentan las mismas consultas.
.destino_columna_fechas_dbi <- function(campo, conexion, preparacion, motor,
                                        con_aplicabilidad) {
  if (identical(preparacion$universo, "muestra_motor")) {
    return(list(destino = "declarada", motivo = paste0(
      "Con `universo = \"muestra_motor\"` el resumen describe la muestra del ",
      "motor: las fechas se leen de `perfil_muestra`."
    )))
  }
  if (.nombres_para_operar(campo) %in% con_aplicabilidad) {
    return(list(destino = "declarada", motivo = paste(
      "La columna tiene `aplicabilidad` declarada, que el resumen de fechas",
      "del motor no traduce: se lee de `perfil_muestra`."
    )))
  }
  # Fuera de SQLite, DuckDB y MariaDB la cuenta por literales no esta medida.
  if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI) {
    return(list(destino = "forzada", motivo = paste0(
      "El resumen de fechas por literales solo est\u00e1 medido contra R en ",
      "SQLite, DuckDB y MariaDB; en este motor se lee de `perfil_muestra`."
    )))
  }
  if (.timestamp_con_huso_dbi(preparacion, campo, motor)) {
    return(list(destino = "forzada", motivo = .MOTIVO_TIMESTAMP_FECHAS_DBI))
  }
  if (.huso_forzado_dbi(conexion, motor)) {
    return(list(destino = "forzada", motivo = .MOTIVO_DUCKDB_HUSO_FORZADO_FECHAS_DBI))
  }
  list(destino = "sql", motivo = NA_character_)
}

# El resumen de fechas de la tabla en el motor. Devuelve las filas (una por
# columna, las de `.fila_resumen_fechas()` mas `estado`, `motivo` y `sql`) y la
# cantidad de consultas. Con `solo_declaraciones` devuelve la clase de cada
# columna y su regla -lo que necesitan los periodos- sin consultar ni leer
# nada: sin `resumen_fechas` las filas no se publican, y contarlas igual
# gastaba consultas que el plan no anuncia y, con `"bloques"`, una lectura de
# cada columna de texto.
.resumen_fechas_dbi <- function(conexion, preparacion, argumentos, fecha_hora,
                                fecha_es_date, estrategia_no_sql,
                                perfil_muestra = NULL,
                                solo_declaraciones = FALSE) {
  campos <- preparacion$campos
  prototipo <- preparacion$prototipo
  tabla_sql <- preparacion$tabla_sql
  presupuesto <- preparacion$presupuesto
  motor <- tryCatch(.motor_medicion_dbi(conexion), error = function(e) NA_character_)
  referencia <- as.numeric(fecha_hora)
  clases <- vapply(campos, .clase_fecha_columna_dbi, character(1L),
                   preparacion = preparacion, USE.NAMES = FALSE)
  # Las columnas de texto que la muestra lee como fecha.
  tipos_muestra <- if (is.list(perfil_muestra) && is.data.frame(perfil_muestra$columnas)) {
    stats::setNames(as.character(perfil_muestra$columnas$tipo_inferido),
                    .nombres_para_operar(as.character(perfil_muestra$columnas$columna)))
  } else NULL
  texto_fecha <- vapply(campos, function(campo) {
    v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
    (is.character(v) || is.factor(v)) && !is.null(tipos_muestra) &&
      isTRUE(tipos_muestra[.nombres_para_operar(campo)] %in% c("fecha", "fecha-hora"))
  }, logical(1L), USE.NAMES = FALSE)
  tipos <- vapply(seq_along(campos), function(i) {
    if (!is.na(clases[[i]])) return(clases[[i]])
    if (texto_fecha[[i]]) return(unname(tipos_muestra[.nombres_para_operar(campos[[i]])]))
    "texto"
  }, character(1L))
  resultados <- lapply(tipos, function(t) list(inferencia = list(tipo = t)))
  dominios <- .normalizar_dominios(argumentos$dominios, campos)
  centinelas_fecha <- .normalizar_centinelas_fecha(argumentos$centinelas_fecha)
  reglas <- lapply(seq_along(campos), function(i) {
    if (is.na(clases[[i]]) && !texto_fecha[[i]]) return(NULL)
    .dominio_regla_columna(campos[[i]], i, campos, resultados, dominios, centinelas_fecha)
  })
  if (isTRUE(solo_declaraciones)) {
    return(list(filas = NULL, reglas = reglas, clases = clases, consultas = 0L))
  }
  con_aplicabilidad <- .nombres_para_operar(names(argumentos$aplicabilidad %||% list()))
  filas <- vector("list", length(campos))
  consultas <- 0L
  sql_pasada <- NA_character_
  declarar <- function(i, motivo) {
    fila <- .fila_resumen_fechas(campos[[i]])
    fila$clase_fecha <- tipos[[i]]
    .completar_fila_fechas_dbi(fila, "no_disponible", motivo)
  }
  forzado <- rep(NA_character_, length(campos))
  evaluables <- integer()
  sin_muestra <- is.null(tipos_muestra)
  for (i in seq_along(campos)) {
    if (is.na(clases[[i]]) && !texto_fecha[[i]]) {
      v <- if (is.data.frame(prototipo)) prototipo[[campos[[i]]]] else NULL
      leida_fecha <- !sin_muestra &&
        isTRUE(tipos_muestra[.nombres_para_operar(campos[[i]])] %in% c("fecha", "fecha-hora"))
      if (leida_fecha) {
        # La muestra la lee como fecha y el controlador no la entrega ni como
        # fecha ni como texto: el resumen no la cuenta y lo dice, no deja `NA`.
        tipos[[i]] <- unname(tipos_muestra[.nombres_para_operar(campos[[i]])])
        filas[[i]] <- declarar(i, paste0(
          "La muestra lee la columna como fecha, pero el controlador la entrega como `",
          class(v)[[1L]] %||% "desconocido", "`: el resumen de fechas no la compara como R."
        ))
      } else if (sin_muestra && (is.character(v) || is.factor(v))) {
        # Sin muestra no se sabe si una columna de texto es una fecha: se
        # declara, no se deja un `NA` mudo.
        filas[[i]] <- declarar(i, paste(
          "Sin muestra no se sabe si esta columna de texto es una fecha:",
          "el resumen de fechas no la eval\u00faa."
        ))
      }
      next
    }
    # Lo que se sabe antes de consultar (`.destino_columna_fechas_dbi()`): se
    # declara, o lo que R lee no es lo que compara el literal, o va al motor.
    destino <- .destino_columna_fechas_dbi(
      campos[[i]], conexion, preparacion, motor, con_aplicabilidad
    )
    if (identical(destino$destino, "declarada")) {
      filas[[i]] <- declarar(i, destino$motivo)
      next
    }
    if (identical(destino$destino, "forzada")) forzado[[i]] <- destino$motivo
    evaluables <- c(evaluables, i)
  }
  nativas <- evaluables[!is.na(clases[evaluables])]
  textos <- evaluables[is.na(clases[evaluables])]
  forzadas <- integer()
  for (i in nativas[!is.na(forzado[nativas])]) {
    if (identical(estrategia_no_sql, "bloques")) {
      forzadas <- c(forzadas, i)
    } else {
      filas[[i]] <- declarar(i, paste(forzado[[i]], "Con `estrategia_no_sql = \"bloques\"` se cuenta en R."))
    }
  }
  nativas <- nativas[is.na(forzado[nativas])]
  # ---- pasada 1: una consulta para todas las columnas nativas ----
  info <- list()
  if (length(nativas)) {
    partes <- character()
    for (i in nativas) {
      x <- as.character(DBI::dbQuoteIdentifier(conexion, campos[[i]]))
      a <- paste0("f", i, "_")
      cand <- .candidatas_regla_dbi(reglas[[i]]$regla, campos[[i]])
      literales <- vapply(cand$segundos, .literal_fecha_dbi, character(1L),
                          conexion = conexion, clase = clases[[i]], motor = motor)
      umbral <- .umbral_futura_dbi(conexion, referencia, fecha_es_date, clases[[i]], motor)
      no_futura <- paste0("NOT (", x, " ", umbral$op, " ", umbral$literal, ")")
      declaradas_lit <- literales[cand$declaradas & !is.na(literales)]
      sin_declaradas <- if (length(declaradas_lit)) {
        paste0(" AND ", x, " NOT IN (", paste(declaradas_lit, collapse = ", "), ")")
      } else ""
      partes <- c(partes,
        paste0("COUNT(", x, ") AS ", a, "n"),
        paste0("MIN(", x, ") AS ", a, "mn"), paste0("MAX(", x, ") AS ", a, "mx"),
        paste0("SUM(CASE WHEN ", x, " ", umbral$op, " ", umbral$literal,
               " THEN 1 ELSE 0 END) AS ", a, "fut"),
        paste0("MAX(CASE WHEN ", x, " IS NOT NULL AND ", no_futura, sin_declaradas,
               " THEN ", x, " END) AS ", a, "ult"))
      for (j in seq_along(literales)) if (!is.na(literales[[j]])) {
        partes <- c(partes, paste0("SUM(CASE WHEN ", x, " = ", literales[[j]],
                                   " THEN 1 ELSE 0 END) AS ", a, "c", j))
      }
      if (identical(motor, "sqlite")) {
        partes <- c(partes, paste0("SUM(CASE WHEN ", .sql_guardado_texto_sqlite_dbi(x),
                                   " THEN 1 ELSE 0 END) AS ", a, "tx"))
      }
      # Con microsegundos, R lee como la candidata un instante a menos de un ULP
      # de ella; el ULP pasa del microsegundo desde |c| >= 2^33 s.
      lejanas <- which(!identical(clases[[i]], "fecha") & !is.na(literales) &
                         abs(cand$segundos) >= 2^33 & motor != "sqlite")
      for (j in lejanas) {
        bajo <- .literal_fecha_dbi(conexion, cand$segundos[[j]] - 1, clases[[i]], motor)
        alto <- if (cand$segundos[[j]] + 1 <= 253402300799) {
          .literal_fecha_dbi(conexion, cand$segundos[[j]] + 1, clases[[i]], motor)
        } else NA_character_
        partes <- c(partes, paste0(
          "SUM(CASE WHEN ", x, " <> ", literales[[j]], " AND ", x, " > ", bajo,
          if (!is.na(alto)) paste0(" AND ", x, " < ", alto) else "",
          " THEN 1 ELSE 0 END) AS ", a, "cerca", j))
      }
      info[[as.character(i)]] <- list(x = x, cand = cand, literales = literales,
                                      umbral = umbral, lejanas = lejanas)
    }
    sql_pasada <- paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql)
    r1 <- .consultar_dbi(conexion, sql_pasada, presupuesto, etapa = "fechas")
    consultas <- consultas + 1L
    if (!isTRUE(r1$ok) || !is.data.frame(r1$datos) || nrow(r1$datos) != 1L) {
      motivo <- paste0("No se pudo leer el resumen de fechas del motor: ",
                       r1$motivo %||% "sin resultado", ".")
      for (i in nativas) filas[[i]] <- declarar(i, motivo)
      nativas <- integer()
    } else {
      d1 <- r1$datos
      lee <- function(alias) if (alias %in% names(d1)) d1[[alias]] else NULL
      for (i in nativas) {
        a <- paste0("f", i, "_"); ci <- info[[as.character(i)]]
        cuenta <- vapply(seq_along(ci$cand$segundos), function(j) {
          v <- .numero_dbi(lee(paste0(a, "c", j))); if (is.na(v)) 0 else v
        }, numeric(1L))
        ci$agregados <- list(
          n = .numero_dbi(lee(paste0(a, "n"))),
          mn = .segundos_fecha_dbi(lee(paste0(a, "mn")), clases[[i]]),
          mx = .segundos_fecha_dbi(lee(paste0(a, "mx")), clases[[i]]),
          fut = { v <- .numero_dbi(lee(paste0(a, "fut"))); if (is.na(v)) 0 else v },
          ult = .segundos_fecha_dbi(lee(paste0(a, "ult")), clases[[i]]),
          cuenta = cuenta
        )
        ci$texto <- if (identical(motor, "sqlite")) .numero_dbi(lee(paste0(a, "tx"))) else 0
        ci$cerca <- sum(vapply(ci$lejanas, function(j) {
          v <- .numero_dbi(lee(paste0(a, "cerca", j))); if (is.na(v)) 0 else v
        }, numeric(1L)))
        info[[as.character(i)]] <- ci
      }
    }
  }
  # ---- lo que el SQL no cuenta como R: por bloques o declarado ----
  por_bloques <- integer()
  for (i in nativas) {
    ci <- info[[as.character(i)]]
    motivo <- NULL
    if (isTRUE(ci$texto > 0)) {
      motivo <- .motivo_fecha_texto_sqlite_dbi(campos[[i]], ci$texto)
    } else if (isTRUE(ci$cerca > 0)) {
      motivo <- paste0(
        "La columna `", campos[[i]], "` tiene ", ci$cerca, " instantes a menos de ",
        "un segundo de una fecha centinela lejana: R los lee como esa fecha y la ",
        "comparaci\u00f3n exacta del motor no."
      )
    }
    if (!is.null(motivo)) {
      if (identical(estrategia_no_sql, "bloques")) {
        por_bloques <- c(por_bloques, i)
        info[[as.character(i)]]$motivo_bloques <- motivo
      } else {
        filas[[i]] <- declarar(i, paste(motivo, "Con `estrategia_no_sql = \"bloques\"` se cuenta en R."))
      }
    }
  }
  for (i in forzadas) {
    info[[as.character(i)]] <- list(
      x = as.character(DBI::dbQuoteIdentifier(conexion, campos[[i]])),
      cand = .candidatas_regla_dbi(reglas[[i]]$regla, campos[[i]]),
      motivo_bloques = forzado[[i]]
    )
  }
  por_bloques <- c(por_bloques, forzadas)
  sql_nativas <- setdiff(nativas, por_bloques)
  sql_nativas <- sql_nativas[vapply(sql_nativas, function(i) is.null(filas[[i]]), logical(1L))]
  # ---- pasada 2: la reserva de las incorporadas que llegaron a su minimo ----
  extremos_sql <- list()
  pedidos <- list()
  for (i in sql_nativas) {
    ci <- info[[as.character(i)]]
    regla <- reglas[[i]]$regla
    inferida <- isTRUE(reglas[[i]]$inferida)
    fin_abierto <- inferida && isTRUE(regla$fin_vigencia)
    for (j in seq_along(ci$cand$segundos)) {
      if (ci$cand$declaradas[[j]] || is.na(ci$literales[[j]])) next
      if (ci$agregados$cuenta[[j]] < .dominio_minimo_centinela(ci$cand$etiquetas[[j]], fin_abierto)) next
      pedidos[[length(pedidos) + 1L]] <- c(i, j)
    }
  }
  if (length(pedidos)) {
    partes <- character()
    for (p in pedidos) {
      i <- p[[1L]]; j <- p[[2L]]; ci <- info[[as.character(i)]]
      otros <- ci$literales[-j]; otros <- otros[!is.na(otros)]
      base <- paste0(ci$x, " IS NOT NULL",
                     if (length(otros)) paste0(" AND ", ci$x, " NOT IN (", paste(otros, collapse = ", "), ")") else "")
      a <- paste0("g", i, "_", j, "_")
      partes <- c(partes,
        paste0("SUM(CASE WHEN ", base, " THEN 1 ELSE 0 END) AS ", a, "nb"),
        paste0("MIN(CASE WHEN ", base, " THEN ", ci$x, " END) AS ", a, "mnb"),
        paste0("MAX(CASE WHEN ", base, " THEN ", ci$x, " END) AS ", a, "mxb"),
        paste0("MAX(CASE WHEN ", base, " AND ", ci$x, " < ", ci$literales[[j]], " THEN ", ci$x, " END) AS ", a, "deb"),
        paste0("MIN(CASE WHEN ", base, " AND ", ci$x, " > ", ci$literales[[j]], " THEN ", ci$x, " END) AS ", a, "enc"))
    }
    r2 <- .consultar_dbi(conexion, paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql),
                         presupuesto, etapa = "fechas")
    consultas <- consultas + 1L
    if (!isTRUE(r2$ok) || !is.data.frame(r2$datos) || nrow(r2$datos) != 1L) {
      motivo <- paste0("No se pudo medir la reserva de las fechas centinela en el motor: ",
                       r2$motivo %||% "sin resultado", ".")
      for (i in unique(vapply(pedidos, `[[`, numeric(1L), 1L))) filas[[i]] <- declarar(i, motivo)
    } else {
      d2 <- r2$datos
      for (p in pedidos) {
        i <- p[[1L]]; j <- p[[2L]]; a <- paste0("g", i, "_", j, "_")
        extremos_sql[[paste(i, j)]] <- list(
          nb = .numero_dbi(d2[[paste0(a, "nb")]]),
          mnb = .segundos_fecha_dbi(d2[[paste0(a, "mnb")]], clases[[i]]),
          mxb = .segundos_fecha_dbi(d2[[paste0(a, "mxb")]], clases[[i]]),
          deb = .segundos_fecha_dbi(d2[[paste0(a, "deb")]], clases[[i]]),
          enc = .segundos_fecha_dbi(d2[[paste0(a, "enc")]], clases[[i]])
        )
      }
    }
  }
  # ---- decision por columna nativa ----
  for (i in sql_nativas) {
    if (!is.null(filas[[i]])) next
    ci <- info[[as.character(i)]]
    estado_lectura <- NULL
    serie <- function() {
      d <- .consultar_dbi(conexion, paste0(
        "SELECT DISTINCT ", ci$x, " AS v FROM ", tabla_sql, " WHERE ", ci$x,
        " IS NOT NULL LIMIT ", .TOPE_DISTINTOS_CALENDARIO_DBI + 1L
      ), presupuesto, etapa = "fechas")
      consultas <<- consultas + 1L
      if (!isTRUE(d$ok) || !is.data.frame(d$datos)) {
        estado_lectura <<- paste0("No se pudo leer la grilla de fechas: ", d$motivo %||% "sin resultado", ".")
        return(NA_real_)
      }
      if (nrow(d$datos) > .TOPE_DISTINTOS_CALENDARIO_DBI) {
        estado_lectura <<- paste0(
          "La columna tiene m\u00e1s de ", .TOPE_DISTINTOS_CALENDARIO_DBI,
          " fechas distintas: no se decide si forman un calendario."
        )
        return(NA_real_)
      }
      v <- d$datos[[1L]]
      segundos <- if (inherits(v, "Date")) as.numeric(v) * 86400 else if (inherits(v, "POSIXct")) {
        as.numeric(v)
      } else if (identical(clases[[i]], "fecha")) as.numeric(v) * 86400 else as.numeric(v)
      .dominio_serie_regular(segundos)
    }
    fila <- .decidir_fechas_columna(
      campos[[i]], clases[[i]], reglas[[i]]$regla, isTRUE(reglas[[i]]$inferida),
      ci$agregados, ci$cand$segundos, ci$cand$etiquetas, ci$cand$declaradas,
      extremo = function(j) extremos_sql[[paste(i, j)]],
      referencia = referencia, fecha_es_date = fecha_es_date, serie = serie
    )
    fila <- .completar_fila_fechas_dbi(fila, "calculado", NA_character_, sql_pasada)
    if (!is.null(estado_lectura)) {
      fila$lectura_fechas_futuras <- NA_character_
      fila$motivo <- estado_lectura
      fila$estado_lectura_futuras <- "no_disponible"
    }
    filas[[i]] <- fila
  }
  # ---- por bloques: lo que R lee, con los mismos agregados ----
  for (i in por_bloques) {
    ci <- info[[as.character(i)]]
    agregados <- NULL
    distintos <- numeric(); demasiados <- FALSE
    lectura <- .leer_columna_por_bloques_dbi(conexion, tabla_sql, ci$x, presupuesto, function(b) {
      v <- if (inherits(b, "Date")) as.numeric(b) * 86400 else as.numeric(b)
      v <- v[!is.na(v) & is.finite(v)]
      agregados <<- .sumar_agregados_fecha(agregados, .agregados_fecha_r(
        v, ci$cand$segundos, ci$cand$declaradas, referencia, fecha_es_date
      ))
      if (!demasiados) {
        distintos <<- unique(c(distintos, v))
        if (length(distintos) > .TOPE_DISTINTOS_CALENDARIO_DBI) demasiados <<- TRUE
      }
    })
    consultas <- consultas + 1L
    if (!isTRUE(lectura$ok)) {
      filas[[i]] <- declarar(i, paste0("No se pudo leer la columna por bloques: ", lectura$motivo, "."))
      next
    }
    if (is.null(agregados)) {
      agregados <- .agregados_fecha_r(numeric(), ci$cand$segundos, ci$cand$declaradas,
                                      referencia, fecha_es_date)
    }
    fila <- .decidir_fechas_columna(
      campos[[i]], clases[[i]], reglas[[i]]$regla, isTRUE(reglas[[i]]$inferida),
      agregados, ci$cand$segundos, ci$cand$etiquetas, ci$cand$declaradas,
      extremo = function(j) agregados$extremos[[j]],
      referencia = referencia, fecha_es_date = fecha_es_date,
      serie = function() if (demasiados) NA_real_ else .dominio_serie_regular(distintos)
    )
    filas[[i]] <- .completar_fila_fechas_dbi(fila, "calculado", paste0(
      ci$motivo_bloques, " Se cont\u00f3 en R leyendo la columna por bloques",
      if (isTRUE(lectura$avisos > 0)) paste0(" (el controlador avis\u00f3 ", lectura$avisos,
                                             " veces al convertir valores)") else "",
      "."
    ), lectura$sql)
  }
  # ---- texto que la muestra lee como fecha ----
  for (i in textos) {
    if (!identical(estrategia_no_sql, "bloques")) {
      # Una columna de fecha que el controlador no sabe leer llega como texto
      # (MySQL con `TIMESTAMP`, `DATETIME` o `DATE`): decir que es texto en el
      # motor describe mal el tipo, que es de fecha.
      tipo_motor <- preparacion$esquema$tipos_motor[campos[[i]]]
      descripcion <- if (length(tipo_motor) == 1L && !is.na(tipo_motor) &&
                         grepl("^(date|smalldatetime|timestamp)",
                               .minusculas_ascii(as.character(tipo_motor)),
                               useBytes = TRUE)) {
        paste0(
          "es de tipo `", tipo_motor, "` en el motor, pero el controlador ",
          "la entrega como texto, y la muestra la lee como fecha"
        )
      } else {
        "es texto en el motor y la muestra la lee como fecha"
      }
      filas[[i]] <- declarar(i, paste0(
        "La columna `", campos[[i]], "` ", descripcion, ": sus formatos los ",
        "decide la columna entera. Con `estrategia_no_sql = \"bloques\"` se ",
        "cuenta en R."
      ))
      next
    }
    filas[[i]] <- .resumen_fecha_texto_por_bloques_dbi(
      conexion, tabla_sql, campos[[i]], presupuesto, argumentos, fecha_hora,
      dominios = argumentos$dominios, centinelas_fecha = centinelas_fecha,
      declarar = function(motivo) declarar(i, motivo)
    )
    consultas <- consultas + 1L
  }
  presentes <- !vapply(filas, is.null, logical(1L))
  list(filas = if (any(presentes)) .apilar_filas(filas[presentes]) else NULL,
       reglas = reglas, clases = clases, consultas = consultas)
}

# Una columna de texto que la muestra lee como fecha, contada sobre la columna
# entera: se acumula su vocabulario (cada forma distinta con su cuenta) en todos
# los bloques y `perfilar()` decide formatos y cuentas sobre ese multiconjunto,
# que es lo que decide sobre la columna (la deteccion de formatos no depende
# del orden de las filas). Asi no hay una segunda regla de formatos.
.resumen_fecha_texto_por_bloques_dbi <- function(conexion, tabla_sql, columna,
                                                 presupuesto, argumentos,
                                                 fecha_hora, dominios,
                                                 centinelas_fecha, declarar) {
  claves_voc <- character(); formas_voc <- character(); cuentas_voc <- numeric()
  filas <- 0; demasiado <- FALSE
  x <- as.character(DBI::dbQuoteIdentifier(conexion, columna))
  lectura <- .leer_columna_por_bloques_dbi(conexion, tabla_sql, x, presupuesto, function(b) {
    b <- as.character(b)
    filas <<- filas + length(b)
    # Pasado el tope no se decide sobre una parte, y tampoco se sigue leyendo:
    # `FALSE` le dice al lector que corte.
    if (filas > .TOPE_FILAS_TEXTO_FECHAS_DBI) {
      demasiado <<- TRUE
      return(FALSE)
    }
    b <- b[!is.na(b)]
    if (!length(b)) return(TRUE)
    claves <- .clave_bytes(b)
    unicos <- !duplicated(claves)
    cuentas <- tabulate(match(claves, claves[unicos]), nbins = sum(unicos))
    previas <- match(claves[unicos], claves_voc)
    ya <- !is.na(previas)
    cuentas_voc[previas[ya]] <<- cuentas_voc[previas[ya]] + cuentas[ya]
    claves_voc <<- c(claves_voc, claves[unicos][!ya])
    formas_voc <<- c(formas_voc, b[unicos][!ya])
    cuentas_voc <<- c(cuentas_voc, cuentas[!ya])
    if (length(claves_voc) > .TOPE_VOCABULARIO_FECHAS_DBI) {
      demasiado <<- TRUE
      return(FALSE)
    }
    TRUE
  })
  if (!isTRUE(lectura$ok)) {
    return(declarar(paste0("No se pudo leer la columna por bloques: ", lectura$motivo, ".")))
  }
  if (demasiado) {
    return(declarar(paste0(
      "La columna de texto tiene m\u00e1s de ", .TOPE_VOCABULARIO_FECHAS_DBI,
      " formas distintas o m\u00e1s de ", format(.TOPE_FILAS_TEXTO_FECHAS_DBI, scientific = FALSE),
      " filas: no se decide sobre una parte."
    )))
  }
  formas <- formas_voc
  vocabulario <- cuentas_voc
  datos <- stats::setNames(
    data.frame(rep(formas, as.integer(vocabulario)), stringsAsFactors = FALSE),
    columna
  )
  # Las filas ausentes cuentan para `perfilar()` como en la tabla.
  ausentes <- filas - nrow(datos)
  if (ausentes > 0) {
    datos <- rbind(datos, stats::setNames(
      data.frame(rep(NA_character_, ausentes), stringsAsFactors = FALSE), columna
    ))
  }
  dominio_columna <- if (isFALSE(dominios)) FALSE else if (is.list(dominios) &&
      columna %in% names(dominios)) dominios[columna] else NULL
  argumentos_perfil <- list(
    datos, nombre = columna, fecha = fecha_hora, muestra = Inf,
    analizar_dependencias = FALSE, resumen_fechas = TRUE,
    centinelas_fecha = if (length(centinelas_fecha)) centinelas_fecha else NULL,
    max_celdas_muestra = Inf, max_bytes_muestra = Inf
  )
  if (!is.null(dominio_columna)) argumentos_perfil$dominios <- dominio_columna
  if (inherits(argumentos$fecha, "Date")) argumentos_perfil$fecha <- argumentos$fecha
  perfil <- tryCatch(
    suppressWarnings(suppressMessages(do.call(perfilar, argumentos_perfil))),
    error = function(e) e
  )
  if (inherits(perfil, "error")) {
    return(declarar(paste0("No se pudo perfilar la columna le\u00edda por bloques: ",
                           conditionMessage(perfil), ".")))
  }
  fechas <- perfil$columnas
  fila <- .fila_resumen_fechas(columna)
  actual <- perfil$general$actualidad$columnas
  if (!is.data.frame(actual) || !nrow(actual)) {
    return(declarar("La columna le\u00edda por bloques no se lee como fecha en la tabla entera."))
  }
  for (campo in .CAMPOS_RESUMEN_FECHAS) fila[[campo]] <- fechas[[campo]][[1L]]
  fila$clase_fecha <- actual$clase[[1L]]
  fila$n_evaluadas_actualidad <- actual$n_evaluadas[[1L]]
  fila$n_centinelas_declaradas_no_futuras <- actual$n_centinelas_declaradas_no_futuras[[1L]]
  fila$admite_futuras <- actual$admite_futuras[[1L]]
  fila$ultimo_dato_seg <- as.numeric(fecha_hora) - actual$antiguedad_dias[[1L]] * 86400
  fila$ultimo_es_incorporada <- grepl(
    "suele ser centinela", actual$motivo[[1L]] %||% "", fixed = TRUE, useBytes = TRUE
  )
  .completar_fila_fechas_dbi(fila, "calculado", paste0(
    "Texto le\u00eddo como fecha: formatos y cuentas decididos sobre la columna ",
    "entera, le\u00edda por bloques."
  ), lectura$sql)
}

# Los campos del motor que acompanan a cada fila del resumen de fechas: su
# estado, su motivo, la consulta y el estado propio de la lectura de futuras.
.completar_fila_fechas_dbi <- function(fila, estado, motivo, sql = NA_character_) {
  fila$estado <- estado
  fila$motivo <- motivo
  fila$sql <- sql
  fila$estado_lectura_futuras <- NA_character_
  fila
}

# El universo aplicable de las dos columnas de un periodo. Una columna deja de
# corresponder a todas las filas de dos maneras -una regla de `aplicabilidad`, o
# `columnas_opcionales`, que aplica donde hay valor-, y la memoria cuenta el
# periodo solo en las filas donde corresponden las dos. Devuelve lo que hay que
# pasarle a `perfilar()` para que cuente igual -las reglas y las opcionales de
# esas dos columnas- y las columnas que hay que leer: las dos y las que nombran
# sus reglas.
.universo_periodo_dbi <- function(p, campos, argumentos) {
  propias <- c(p$inicio, p$fin)
  aplicabilidad <- argumentos$aplicabilidad
  reglas <- if (is.list(aplicabilidad) && length(aplicabilidad) &&
                !is.null(names(aplicabilidad))) {
    aplicabilidad[!is.na(.indice_nombre(names(aplicabilidad), propias))]
  } else list()
  opcionales <- as.character(argumentos$columnas_opcionales %||% character())
  opcionales <- propias[!is.na(.indice_nombre(propias, opcionales))]
  nombradas <- unlist(lapply(reglas, function(f) {
    if (inherits(f, "formula") && length(f) == 2L) {
      all.names(f[[2L]], functions = FALSE)
    } else character()
  }), use.names = FALSE)
  indices <- .indice_nombre(unique(as.character(nombradas)), campos)
  list(
    reglas = reglas, opcionales = opcionales,
    columnas = unique(c(propias, campos[indices[!is.na(indices)]])),
    declarado = length(reglas) > 0L || length(opcionales) > 0L
  )
}

.MOTIVO_UNIVERSO_PERIODO_DBI <- paste(
  "Una columna del periodo no corresponde a todas las filas (`aplicabilidad` o",
  "`columnas_opcionales`), y la consulta del motor no traduce esa declaraci\u00f3n."
)

# La regla de `aplicabilidad` de una de las dos columnas del periodo que no da
# un valor logico, si la hay (ver `.reglas_aplicabilidad_no_logicas()`).
.regla_no_logica_periodo_dbi <- function(p, reglas) {
  if (!is.data.frame(reglas) || !nrow(reglas)) return(NULL)
  k <- which(!is.na(.indice_nombre(reglas$columna, c(p$inicio, p$fin))))
  if (!length(k)) return(NULL)
  reglas[k[[1L]], , drop = FALSE]
}

# Esa regla no se aplica, y la memoria no cuenta un periodo con una regla asi:
# el periodo se declara, con que da la regla y como escribirla.
.fila_periodo_regla_no_logica_dbi <- function(p, regla) {
  .fila_periodo_sin_cifras(p, paste(
    .motivo_aplicabilidad_no_logica_dbi(regla),
    "El periodo no se cuenta sin ella.",
    .remedio_aplicabilidad_no_logica_dbi(regla)
  ))
}

# La causa que cita un motivo, sin su punto final ni sus blancos: quien la cita
# cierra la frase, y un error de `perfilar()` ya trae la suya cerrada. Por
# bytes, para no depender de la codificacion de la sesion.
.sin_punto_final_dbi <- function(texto) {
  texto <- as.character(texto)
  if (length(texto) != 1L || is.na(texto)) return(texto)
  crudo <- charToRaw(texto)
  while (length(crudo) &&
         crudo[[length(crudo)]] %in% as.raw(c(0x2e, 0x20, 0x09, 0x0a, 0x0d))) {
    crudo <- crudo[-length(crudo)]
  }
  salida <- rawToChar(crudo)
  Encoding(salida) <- Encoding(texto)
  salida
}

# El tipo que declara el motor para dos columnas, y si es el mismo. Sin el tipo
# de alguna no se puede afirmar que lo sea.
.tipos_motor_periodo_dbi <- function(preparacion, campo_inicio, campo_fin) {
  tipo <- function(campo) {
    valor <- .atributo_columna_dbi(preparacion$tipos, campo)
    if (is.null(valor) || !length(valor) || is.na(valor[[1L]])) return(NA_character_)
    trimws(as.character(valor[[1L]]))
  }
  # `TIMESTAMP WITH TIME ZONE` guarda lo mismo que `TIMESTAMP`, microsegundos,
  # y R lee el mismo numero para el infinito de los dos.
  guardado <- function(tipo) {
    tipo <- .minusculas_ascii(tipo)
    if (tipo %in% c("timestamp with time zone", "timestamptz")) "timestamp" else tipo
  }
  tipos <- c(tipo(campo_inicio), tipo(campo_fin))
  list(tipos = tipos, iguales = !anyNA(tipos) && identical(
    guardado(tipos[[1L]]), guardado(tipos[[2L]])
  ))
}

# R lee un `infinity` de DuckDB como la fecha mas lejana que cabe en el tipo de
# la columna: un numero finito, distinto en cada tipo. Con las dos columnas del
# mismo tipo el motor y R ordenan igual; entre tipos distintos el motor da por
# iguales dos infinitos que en R son dos fechas distintas.
.motivo_periodo_infinito_dbi <- function(n, tipos) {
  paste0(
    "Las columnas del periodo son de tipos distintos",
    if (!anyNA(tipos)) paste0(" (`", tipos[[1L]], "` y `", tipos[[2L]], "`)") else "",
    " y ", format(n, scientific = FALSE, trim = TRUE),
    if (n == 1) " fila tiene" else " filas tienen",
    " una fecha infinita: R lee el infinito de cada tipo como una fecha lejana ",
    "distinta y el motor los compara como iguales."
  )
}

# Lo que se sabe de un periodo antes de consultar: si no se cuenta
# ("declarado": con `universo = "muestra_motor"`, o con una regla de
# `aplicabilidad` que por el tipo de las columnas no da un valor logico), si el
# motor no lo cuenta como R ("sin_sql": con `"bloques"` se lee, si no se
# declara) o si va a la consulta del motor ("sql"). En ese caso `condicional`
# dice si lo que la consulta encuentre puede mandarlo a leerse igual: en
# SQLite, una fecha guardada como texto; en DuckDB, un infinito entre columnas
# de tipos distintos. La corrida y el plan preguntan aca, y asi cuentan las
# mismas consultas.
.destino_periodo_dbi <- function(p, conexion, preparacion, motor, clases,
                                 argumentos = list(),
                                 aplicabilidad_no_logica = NULL) {
  campos <- preparacion$campos
  i <- .indice_nombre(p$inicio, campos); j <- .indice_nombre(p$fin, campos)
  # El perfil describe la muestra del motor: el periodo de la tabla entera no
  # se publica a su lado, en ningun motor ni con ninguna estrategia.
  if (identical(preparacion$universo, "muestra_motor")) {
    return(list(destino = "declarado", motivo = paste(
      "Con `universo = \"muestra_motor\"` los periodos se leen de",
      "`perfil_muestra`."
    )))
  }
  # La regla no se aplica (ver `perfilar_dbi()`), y la memoria no cuenta un
  # periodo con una regla asi.
  regla <- .regla_no_logica_periodo_dbi(p, aplicabilidad_no_logica)
  if (!is.null(regla)) return(list(destino = "declarado", regla = regla))
  sin_sql <- function(motivo) list(destino = "sin_sql", motivo = motivo)
  if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI) {
    return(sin_sql(paste(
      "Los periodos por literales solo est\u00e1n medidos contra R en SQLite,",
      "DuckDB y MariaDB."
    )))
  }
  # La consulta cuenta la tabla entera. Si una de las dos columnas no
  # corresponde a todas las filas, la memoria cuenta otro universo.
  if (.universo_periodo_dbi(p, campos, argumentos)$declarado) {
    return(sin_sql(.MOTIVO_UNIVERSO_PERIODO_DBI))
  }
  if (is.na(clases[[i]]) || is.na(clases[[j]])) {
    return(sin_sql(paste(
      "Una columna del periodo no tiene tipo de fecha en el motor (el texto que",
      "R lee como fecha, el motor no lo compara como R)."
    )))
  }
  if (identical(motor, "sqlite") && !identical(clases[[i]], clases[[j]])) {
    return(sin_sql(paste(
      "En SQLite una fecha se guarda en d\u00edas y una fecha-hora en segundos:",
      "el motor no compara las dos como R."
    )))
  }
  if (.timestamp_con_huso_dbi(preparacion, campos[[i]], motor) ||
      .timestamp_con_huso_dbi(preparacion, campos[[j]], motor)) {
    return(sin_sql(.MOTIVO_TIMESTAMP_FECHAS_DBI))
  }
  if (.huso_forzado_dbi(conexion, motor)) {
    return(sin_sql(.MOTIVO_DUCKDB_HUSO_FORZADO_FECHAS_DBI))
  }
  tipos <- .tipos_motor_periodo_dbi(preparacion, campos[[i]], campos[[j]])
  infinitos <- identical(motor, "duckdb") && !tipos$iguales
  list(
    destino = "sql", i = i, j = j, tipos = tipos$tipos, infinitos = infinitos,
    condicional = identical(motor, "sqlite") || infinitos
  )
}

# Los periodos en el motor: una consulta por tabla con la misma particion que
# `.particion_periodo()`, contra literales.
.periodos_dbi <- function(conexion, preparacion, periodos, reglas, clases,
                          motor, presupuesto, estrategia_no_sql = "no_medible",
                          argumentos = list(), fecha_hora = NULL,
                          aplicabilidad_no_logica = NULL) {
  if (is.null(periodos)) return(list(tabla = NULL, consultas = 0L))
  campos <- preparacion$campos
  tabla_sql <- preparacion$tabla_sql
  conocidas_base <- .dominio_fecha_numero(.CENTINELAS_FECHA_DOMINIO, "periodos")
  filas <- list(); partes <- character(); pendientes <- list()
  consultas <- 0L
  # Lo que el SQL no puede contar como R lo lee: con `"bloques"` se cuenta en
  # R; si no, se declara, sin cifras.
  sin_sql <- function(p, motivo) {
    if (identical(estrategia_no_sql, "bloques")) {
      consultas <<- consultas + 1L
      return(.periodo_por_bloques_dbi(
        conexion, preparacion, p, presupuesto, argumentos, fecha_hora, motivo
      ))
    }
    .fila_periodo_sin_cifras(p, paste(
      motivo, "Con `estrategia_no_sql = \"bloques\"` se cuenta en R."
    ))
  }
  for (k in seq_along(periodos)) {
    p <- periodos[[k]]
    # Lo que se sabe antes de consultar (`.destino_periodo_dbi()`): no se
    # cuenta, o el motor no lo cuenta como R, o va a la consulta.
    destino <- .destino_periodo_dbi(
      p, conexion, preparacion, motor, clases, argumentos, aplicabilidad_no_logica
    )
    if (identical(destino$destino, "declarado")) {
      filas[[k]] <- if (is.null(destino$regla)) {
        .fila_periodo_sin_cifras(p, destino$motivo)
      } else .fila_periodo_regla_no_logica_dbi(p, destino$regla)
      next
    }
    if (identical(destino$destino, "sin_sql")) {
      filas[[k]] <- sin_sql(p, destino$motivo)
      next
    }
    i <- destino$i; j <- destino$j
    xi <- as.character(DBI::dbQuoteIdentifier(conexion, campos[[i]]))
    xf <- as.character(DBI::dbQuoteIdentifier(conexion, campos[[j]]))
    lit <- function(segundos, clase) {
      l <- vapply(segundos, .literal_fecha_dbi, character(1L), conexion = conexion,
                  clase = clase, motor = motor)
      l[!is.na(l)]
    }
    en <- function(x, l) if (length(l)) paste0("(", x, " IN (", paste(l, collapse = ", "), "))") else "(1 = 0)"
    decl_i <- lit(.centinelas_declaradas_regla(reglas[[i]]$regla, p$inicio), clases[[i]])
    decl_f_seg <- .centinelas_declaradas_regla(reglas[[j]]$regla, p$fin)
    marcas_seg <- if (length(p$fin_abierto)) .dominio_fecha_numero(p$fin_abierto, p$fin) else numeric()
    conocidas_seg <- setdiff(conocidas_base, c(decl_f_seg, marcas_seg))
    # Un `infinity` de DuckDB es para el motor una fecha mas, la mayor de
    # todas, y asi lo lee R: como la fecha mas lejana que cabe en el tipo. No
    # se lo aparta, como no lo aparta `perfilar()` sobre lo que R lee.
    iv <- paste0("(", xi, " IS NOT NULL",
                 if (length(decl_i)) paste0(" AND NOT ", en(xi, decl_i)) else "", ")")
    fn <- paste0("(", xf, " IS NULL)")
    fm <- paste0("(", xf, " IS NOT NULL AND ", en(xf, lit(marcas_seg, clases[[j]])), ")")
    fd <- paste0("(", xf, " IS NOT NULL AND NOT ", fm,
                 " AND ", en(xf, lit(decl_f_seg, clases[[j]])), ")")
    fc <- paste0("(", xf, " IS NOT NULL AND NOT ", fm,
                 " AND NOT ", fd, " AND ", en(xf, lit(conocidas_seg, clases[[j]])), ")")
    fv <- paste0("(", xf, " IS NOT NULL AND NOT ", fm,
                 " AND NOT ", fd, " AND NOT ", fc, ")")
    suma <- function(cond, alias) paste0("SUM(CASE WHEN ", cond, " THEN 1 ELSE 0 END) AS p", k, "_", alias)
    partes <- c(partes,
      paste0("COUNT(*) AS p", k, "_n"),
      suma(paste(iv, "AND", fv, "AND", xi, "<=", xf), "n_cerrados_validos"),
      suma(paste(iv, "AND", fv, "AND", xf, "<", xi), "n_fin_antes_inicio"),
      suma(paste(iv, "AND", fn), "n_fin_abierto_nulo"),
      suma(paste(iv, "AND", fm), "n_fin_abierto_centinela"),
      suma(paste(iv, "AND", fd), "n_fin_centinela_declarado"),
      suma(paste(iv, "AND", fc), "n_fin_centinela_sin_declarar"),
      suma(paste("NOT", iv, "AND", fv), "n_sin_inicio_con_fin"),
      suma(paste("NOT", iv, "AND NOT", fv), "n_ambos_ausentes_o_centinela"))
    for (m in seq_along(conocidas_seg)) {
      l <- lit(conocidas_seg[[m]], clases[[j]])
      if (length(l)) partes <- c(partes, suma(paste(iv, "AND", fc, "AND", xf, "=", l), paste0("k", m)))
    }
    if (identical(motor, "sqlite")) {
      partes <- c(partes, suma(paste(.sql_guardado_texto_sqlite_dbi(xi), "OR",
                                     .sql_guardado_texto_sqlite_dbi(xf)), "texto"))
    }
    # Entre dos tipos distintos un infinito no se compara como R lo lee: se
    # cuentan las filas que lo tienen, en la misma pasada.
    if (isTRUE(destino$infinitos)) {
      partes <- c(partes, suma(paste0(
        "NOT isfinite(", xi, ") OR NOT isfinite(", xf, ")"
      ), "infinito"))
    }
    pendientes[[length(pendientes) + 1L]] <- list(
      k = k, p = p, conocidas = conocidas_seg, tipos = destino$tipos
    )
  }
  if (length(pendientes)) {
    sql <- paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql)
    r <- .consultar_dbi(conexion, sql, presupuesto, etapa = "periodos")
    consultas <- consultas + 1L
    for (pe in pendientes) {
      k <- pe$k
      if (!isTRUE(r$ok) || !is.data.frame(r$datos) || nrow(r$datos) != 1L) {
        filas[[k]] <- .fila_periodo_sin_cifras(pe$p, paste0(
          "No se pudo contar el periodo en el motor: ", r$motivo %||% "sin resultado", "."
        ))
        next
      }
      texto <- .numero_dbi(r$datos[[paste0("p", k, "_texto")]])
      if (isTRUE(texto > 0)) {
        filas[[k]] <- sin_sql(pe$p, paste0(
          "Las columnas del periodo tienen ", format(texto, scientific = FALSE, trim = TRUE),
          if (texto == 1) " fila con una fecha " else " filas con fechas ",
          .FRAGMENTO_TEXTO_MEDIDO_DBI, ": SQLite no las compara como R las lee."
        ))
        next
      }
      infinito <- .numero_dbi(r$datos[[paste0("p", k, "_infinito")]])
      if (isTRUE(infinito > 0)) {
        filas[[k]] <- sin_sql(pe$p, .motivo_periodo_infinito_dbi(infinito, pe$tipos))
        next
      }
      valor <- function(alias) {
        v <- .numero_dbi(r$datos[[paste0("p", k, "_", alias)]]); if (is.na(v)) 0 else v
      }
      nombres <- c("n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
                   "n_fin_abierto_centinela", "n_fin_centinela_declarado",
                   "n_fin_centinela_sin_declarar", "n_sin_inicio_con_fin",
                   "n_ambos_ausentes_o_centinela")
      particion <- stats::setNames(lapply(nombres, valor), nombres)
      observadas <- pe$conocidas[vapply(seq_along(pe$conocidas), function(m) {
        alias <- paste0("p", k, "_k", m)
        alias %in% names(r$datos) && isTRUE(.numero_dbi(r$datos[[alias]]) > 0)
      }, logical(1L))]
      particion$conocidas_observadas <- vapply(
        .sort_por_bytes(observadas), .fecha_resumida, character(1L), clase = "fecha"
      )
      filas[[k]] <- .fila_periodo(pe$p, particion)
    }
  }
  tabla <- .apilar_filas(filas)
  rownames(tabla) <- NULL
  list(tabla = tabla, consultas = consultas)
}

# Agrega al `perfil_dbi` lo pedido: los campos del resumen de fechas en
# `resumen_tabla$columnas`, sus filas en `resumen_tabla$sql`, la actualidad y
# los periodos en `resumen_tabla$meta`. Sin `resumen_fechas` ni `periodos` no
# toca nada.
.agregar_resumen_fechas_dbi <- function(x, conexion, preparacion, argumentos,
                                        fecha_hora, fecha_declarada,
                                        estrategia_no_sql,
                                        incluir_valores = TRUE,
                                        aplicabilidad_no_logica = NULL) {
  resumen_fechas <- isTRUE(argumentos$resumen_fechas)
  periodos <- .normalizar_periodos(argumentos$periodos, preparacion$campos)
  if ((!resumen_fechas && is.null(periodos)) || !is.list(x$resumen_tabla)) return(x)
  fecha_es_date <- inherits(argumentos$fecha, "Date")
  # Lo que gasta cada clase, del presupuesto: es lo que cuenta
  # `meta$consultas$emitidas`.
  usadas <- function() {
    n <- preparacion$presupuesto$usadas
    if (is.numeric(n) && length(n) == 1L) as.numeric(n) else NA_real_
  }
  usadas_antes <- usadas()
  resultado <- .resumen_fechas_dbi(
    conexion, preparacion, argumentos, fecha_hora, fecha_es_date,
    estrategia_no_sql, perfil_muestra = x$perfil_muestra,
    solo_declaraciones = !resumen_fechas
  )
  usadas_resumen <- usadas()
  emitidas <- stats::setNames(
    if (is.na(usadas_antes)) as.numeric(resultado$consultas) else usadas_resumen - usadas_antes,
    .CLASE_PLAN_RESUMEN_FECHAS_DBI
  )
  rt <- x$resumen_tabla
  if (resumen_fechas) {
    filas <- resultado$filas
    # El ultimo dato es un valor de la columna: con `incluir_valores = FALSE`
    # no se publica (tampoco cuantos dias hace, que lo reconstruye).
    if (!isTRUE(incluir_valores) && is.data.frame(filas) && nrow(filas)) {
      filas$ultimo_dato_fecha <- NA_character_
      filas$ultimo_dato_seg <- NA_real_
    }
    rt$columnas <- .agregar_resumen_fechas_columnas(rt$columnas, filas)
    if (is.data.frame(filas) && nrow(filas)) {
      sql_nuevas <- do.call(rbind, lapply(seq_len(nrow(filas)), function(k) {
        data.frame(
          columna = filas$columna[[k]], metrica = .CAMPOS_RESUMEN_FECHAS,
          estado = ifelse(
            .CAMPOS_RESUMEN_FECHAS == "lectura_fechas_futuras" &
              identical(filas$estado_lectura_futuras[[k]] %||% NA_character_, "no_disponible"),
            "no_disponible", ifelse(
              .CAMPOS_RESUMEN_FECHAS == "ultimo_dato_fecha" & !isTRUE(incluir_valores) &
                filas$estado[[k]] == "calculado",
              "omitido_por_privacidad", filas$estado[[k]]
            )
          ),
          motivo = filas$motivo[[k]], sql = filas$sql[[k]],
          alcance = "tabla_completa", etapa = "fechas", stringsAsFactors = FALSE
        )
      }))
      rt$sql <- .apilar_filas_sql_dbi(rt$sql, sql_nuevas)
      filas$motivo_motor <- ifelse(filas$estado == "calculado", NA_character_, filas$motivo)
    }
    rt$meta$actualidad <- .actualidad_desde_resumen(
      filas, fecha_hora, fecha_declarada, fecha_es_date
    )
  }
  if (!is.null(periodos)) {
    motor <- tryCatch(.motor_medicion_dbi(conexion), error = function(e) NA_character_)
    p <- .periodos_dbi(conexion, preparacion, periodos, resultado$reglas,
                       resultado$clases, motor, preparacion$presupuesto,
                       estrategia_no_sql = estrategia_no_sql,
                       argumentos = argumentos, fecha_hora = fecha_hora,
                       aplicabilidad_no_logica = aplicabilidad_no_logica)
    rt$meta$periodos <- p$tabla
    rt$meta$declaracion_periodos <- periodos
    emitidas[[.CLASE_PLAN_PERIODOS_DBI]] <- if (is.na(usadas_resumen)) {
      as.numeric(p$consultas)
    } else usadas() - usadas_resumen
  }
  if (is.list(rt$meta$consultas) && !is.null(preparacion$presupuesto)) {
    rt$meta$consultas$emitidas <- preparacion$presupuesto$usadas
  }
  # El plan de estas consultas, el mismo que anuncia `plan_perfilado_dbi()`, y
  # su conciliacion con lo emitido.
  rt$meta <- .conciliar_fechas_dbi(rt$meta, .filas_plan_fechas_dbi(
    conexion, preparacion, resumen_fechas, periodos, estrategia_no_sql,
    aplicabilidad = argumentos$aplicabilidad,
    columnas_opcionales = argumentos$columnas_opcionales %||% character(),
    aplicabilidad_no_logica = aplicabilidad_no_logica
  ), emitidas, preparacion)
  x$resumen_tabla <- rt
  x
}

# Las filas nuevas de `resumen_tabla$sql`, con las columnas del frame que ya
# existe (lo que no se sabe va como `NA`).
.apilar_filas_sql_dbi <- function(sql, nuevas) {
  if (!is.data.frame(sql) || !nrow(sql)) return(nuevas)
  for (campo in setdiff(names(sql), names(nuevas))) {
    plantilla <- sql[[campo]][NA_integer_]
    nuevas[[campo]] <- rep(plantilla, nrow(nuevas))
  }
  nuevas <- nuevas[names(sql)]
  salida <- rbind(sql, nuevas)
  rownames(salida) <- NULL
  salida
}

.CLASE_PLAN_RESUMEN_FECHAS_DBI <- "resumen de fechas (cuentas contra literales)"
.CLASE_PLAN_PERIODOS_DBI <- "periodos (particion contra literales)"

# Las filas del plan para el resumen de fechas y los periodos, con lo que se
# sabe sin leer filas (`.destino_columna_fechas_dbi()`,
# `.destino_periodo_dbi()`: las mismas preguntas que hace la corrida).
# `n_consultas` es lo que la corrida emite seguro: la pasada de cuentas si
# alguna columna va al motor, la consulta de periodos si alguno va al motor y,
# con `"bloques"`, una lectura por cada columna y por cada periodo que el motor
# no cuenta como R. `n_consultas_max` suma lo que depende de los datos: la
# reserva de una fecha incorporada que llega a su minimo; por cada columna que
# va al motor, su grilla si tiene fechas futuras o, con `"bloques"`, su lectura
# si la pasada la manda a R; con `"bloques"`, la lectura de cada texto que la
# muestra lea como fecha y la de cada periodo que la consulta mande a R.
.filas_plan_fechas_dbi <- function(conexion, preparacion, resumen_fechas,
                                   periodos, estrategia_no_sql,
                                   aplicabilidad = NULL,
                                   columnas_opcionales = character(),
                                   aplicabilidad_no_logica = NULL) {
  fila <- function(clase, seguras, maximas) {
    data.frame(
      clase_consulta = clase, n_consultas = seguras,
      alcance = "agrega la tabla completa", n_consultas_max = maximas,
      stringsAsFactors = FALSE
    )
  }
  ninguna <- data.frame(
    clase_consulta = character(), n_consultas = numeric(),
    alcance = character(), n_consultas_max = numeric(),
    stringsAsFactors = FALSE
  )
  filas <- list(ninguna)
  if (!isTRUE(resumen_fechas) && is.null(periodos)) return(ninguna)
  if (identical(preparacion$universo, "muestra_motor")) return(ninguna)
  motor <- tryCatch(.motor_medicion_dbi(conexion), error = function(e) NA_character_)
  en_bloques <- identical(estrategia_no_sql, "bloques")
  # Sin la cuenta por literales medida en el motor, solo consulta la lectura por
  # bloques, si se pide.
  if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI && !en_bloques) return(ninguna)
  campos <- preparacion$campos
  prototipo <- preparacion$prototipo
  clases <- vapply(campos, .clase_fecha_columna_dbi, character(1L),
                   preparacion = preparacion, USE.NAMES = FALSE)
  es_texto <- vapply(campos, function(campo) {
    v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
    is.character(v) || is.factor(v)
  }, logical(1L), USE.NAMES = FALSE)
  if (isTRUE(resumen_fechas) && (any(!is.na(clases)) || any(es_texto))) {
    con_aplicabilidad <- .nombres_para_operar(names(aplicabilidad %||% list()))
    destinos <- vapply(campos, function(campo) {
      .destino_columna_fechas_dbi(
        campo, conexion, preparacion, motor, con_aplicabilidad
      )$destino
    }, character(1L), USE.NAMES = FALSE)
    al_motor <- sum(!is.na(clases) & destinos == "sql")
    forzadas <- sum(!is.na(clases) & destinos == "forzada")
    textos <- sum(is.na(clases) & es_texto & destinos != "declarada")
    seguras <- (if (al_motor > 0) 1 else 0) + (if (en_bloques) forzadas else 0)
    filas[[length(filas) + 1L]] <- fila(
      .CLASE_PLAN_RESUMEN_FECHAS_DBI, seguras,
      seguras + (if (al_motor > 0) 1 + al_motor else 0) +
        (if (en_bloques) textos else 0)
    )
  }
  if (!is.null(periodos)) {
    destinos <- lapply(periodos, function(p) {
      .destino_periodo_dbi(
        p, conexion, preparacion, motor, clases,
        list(aplicabilidad = aplicabilidad, columnas_opcionales = columnas_opcionales),
        aplicabilidad_no_logica
      )
    })
    destino <- vapply(destinos, function(d) d$destino, character(1L))
    condicionales <- sum(vapply(destinos, function(d) isTRUE(d$condicional), logical(1L)))
    seguras <- (if (any(destino == "sql")) 1 else 0) +
      (if (en_bloques) sum(destino == "sin_sql") else 0)
    filas[[length(filas) + 1L]] <- fila(
      .CLASE_PLAN_PERIODOS_DBI, seguras,
      seguras + (if (en_bloques) condicionales else 0)
    )
  }
  salida <- do.call(rbind, filas)
  rownames(salida) <- NULL
  salida
}

# Suma filas a un plan conservando sus atributos. Con `totales`, suma ademas a
# los totales que el plan ya trae: lo seguro al total y al minimo, lo maximo al
# maximo.
.sumar_filas_plan_dbi <- function(plan, filas, totales = FALSE) {
  if (!is.data.frame(plan) || !is.data.frame(filas) || !nrow(filas)) return(plan)
  atributos <- attributes(plan)
  columnas <- c("clase_consulta", "n_consultas", "alcance", "n_consultas_max")
  nuevo <- rbind(as.data.frame(plan)[columnas], filas[columnas])
  rownames(nuevo) <- NULL
  for (a in setdiff(names(atributos), c("names", "row.names"))) {
    attr(nuevo, a) <- atributos[[a]]
  }
  if (isTRUE(totales)) {
    sumar <- function(nombres, cuanto) {
      for (a in nombres) {
        actual <- attr(nuevo, a, exact = TRUE)
        if (is.numeric(actual) && length(actual) == 1L) attr(nuevo, a) <<- actual + cuanto
      }
    }
    sumar(c("total", "total_minimo"), sum(filas$n_consultas))
    sumar(c("total_maximo", "total_lotes_rechazados"), sum(filas$n_consultas_max))
  }
  nuevo
}

# El presupuesto visto desde las consultas de fechas: de las fallas, solo las
# de sus etapas. La conciliacion toma por via de respaldo a una consulta de mas
# cuando alguna consulta fallo, y una capacidad que el motor rechaza al
# prepararse -la de collation en PostgreSQL 9.3, la de la fuente por bloques en
# SQL Server- no tiene que ver con una lectura de fechas que activaron los
# datos: con ella la lectura quedaba como no anunciada y la cuenta desfasada.
.ETAPAS_FECHAS_DBI <- c("fechas", "periodos")

.presupuesto_fechas_dbi <- function(presupuesto) {
  if (is.null(presupuesto)) return(NULL)
  list(
    max = presupuesto$max, usadas = presupuesto$usadas,
    agotado = presupuesto$agotado,
    consultas_bloqueadas = presupuesto$consultas_bloqueadas,
    guardas_faltantes_bloqueadas = presupuesto$guardas_faltantes_bloqueadas,
    fallas_consultas = Filter(function(falla) {
      as.character(falla$etapa %||% "") %in% .ETAPAS_FECHAS_DBI
    }, presupuesto$fallas_consultas),
    fallas_sondas = list()
  )
}

# Las consultas de fechas se emiten al salir, cuando la corrida ya concilio las
# suyas contra su plan. Aca se suman: las filas de fechas a `meta$plan`, y lo
# anunciado y lo emitido por clase a `meta$consultas$conciliacion`, con la regla
# de las demas clases (`.conciliar_consultas_dbi()`): lo que los datos activan
# dentro del maximo anunciado es condicional y no desfasa. `emitidas` lleva lo
# que cada clase gasto del presupuesto.
.conciliar_fechas_dbi <- function(meta, filas_plan, emitidas, preparacion) {
  if (!is.data.frame(filas_plan)) return(meta)
  ejecutado <- filas_plan
  ejecutado$n_consultas <- vapply(ejecutado$clase_consulta, function(clase) {
    n <- emitidas[clase]
    if (is.na(n)) 0 else as.numeric(n)
  }, numeric(1L), USE.NAMES = FALSE)
  # Lo emitido por una clase que el plan no trae no se pierde: queda sin anuncio.
  sin_fila <- emitidas[!names(emitidas) %in% ejecutado$clase_consulta & emitidas > 0]
  if (length(sin_fila)) {
    ejecutado <- rbind(ejecutado, data.frame(
      clase_consulta = names(sin_fila), n_consultas = as.numeric(sin_fila),
      alcance = "agrega la tabla completa", n_consultas_max = as.numeric(sin_fila),
      stringsAsFactors = FALSE
    ))
  }
  if (!nrow(ejecutado)) return(meta)
  ejecutado$n_consultas_max <- ejecutado$n_consultas
  if (is.data.frame(meta$plan)) {
    meta$plan <- .sumar_filas_plan_dbi(meta$plan, filas_plan, totales = TRUE)
  }
  if (!is.list(meta$consultas) || !is.list(meta$consultas$conciliacion)) return(meta)
  fechas <- .conciliar_consultas_dbi(
    filas_plan, ejecutado, sum(ejecutado$n_consultas), preparacion,
    .presupuesto_fechas_dbi(preparacion$presupuesto)
  )
  seguras <- sum(filas_plan$n_consultas)
  maximas <- sum(filas_plan$n_consultas_max)
  sumar <- function(conciliacion) {
    if (!is.list(conciliacion)) return(conciliacion)
    if (is.numeric(conciliacion$anunciadas) && is.numeric(conciliacion$emitidas)) {
      conciliacion$anunciadas <- conciliacion$anunciadas + fechas$anunciadas
      conciliacion$emitidas <- conciliacion$emitidas + fechas$emitidas
      conciliacion$diferencia <- conciliacion$emitidas - conciliacion$anunciadas
      if (is.data.frame(conciliacion$detalle) && nrow(fechas$detalle)) {
        conciliacion$detalle <- rbind(conciliacion$detalle, fechas$detalle)
        rownames(conciliacion$detalle) <- NULL
      }
      if (!identical(fechas$estado, "conciliado")) conciliacion$estado <- "desfasado"
    }
    for (a in c("total", "total_minimo")) {
      actual <- attr(conciliacion, a, exact = TRUE)
      if (is.numeric(actual) && length(actual) == 1L) attr(conciliacion, a) <- actual + seguras
      if (is.numeric(conciliacion[[a]]) && length(conciliacion[[a]]) == 1L) {
        conciliacion[[a]] <- conciliacion[[a]] + seguras
      }
    }
    for (a in c("total_maximo", "total_lotes_rechazados")) {
      actual <- attr(conciliacion, a, exact = TRUE)
      if (is.numeric(actual) && length(actual) == 1L) attr(conciliacion, a) <- actual + maximas
      if (is.numeric(conciliacion[[a]]) && length(conciliacion[[a]]) == 1L) {
        conciliacion[[a]] <- conciliacion[[a]] + maximas
      }
    }
    conciliacion
  }
  conciliacion <- meta$consultas$conciliacion
  for (parte in c("plan_corrida", "plan_previo")) {
    if (is.list(conciliacion[[parte]])) conciliacion[[parte]] <- sumar(conciliacion[[parte]])
  }
  meta$consultas$conciliacion <- sumar(conciliacion)
  meta
}

# Un periodo que no se conto: estado y motivo, y ninguna cifra (ni siquiera `n`:
# una tabla con filas no tiene cero filas porque no se las haya contado).
.fila_periodo_sin_cifras <- function(p, motivo) {
  vacia <- .particion_periodo(numeric(), numeric(), numeric(), numeric(), numeric(), numeric())
  fila <- .fila_periodo(p, vacia, estado = "no_disponible", motivo = motivo)
  cifras <- grepl("^n$|^n_|^prop_", names(fila))
  fila[cifras] <- lapply(fila[cifras], function(v) NA_real_)
  fila
}

# Un periodo contado en R. Se leen por bloques, hasta el tope, las columnas que
# la cuenta necesita -las dos del periodo y las que nombran sus reglas de
# `aplicabilidad`- y `perfilar()` cuenta el periodo sobre ellas, con las mismas
# declaraciones: `dominios` y `centinelas_fecha`, y la `aplicabilidad` y las
# `columnas_opcionales` de esas dos columnas. Es la funcion de la memoria sobre
# la tabla entera: que lee como fecha, con que formato lee un texto, en que
# filas corresponde cada columna y que fechas estan declaradas se deciden ahi,
# una sola vez, y no dependen del orden de las filas ni del tamanio del bloque.
# Si `perfilar()` no cuenta el periodo -una columna que no lee como fecha, una
# regla que no puede evaluar-, queda declarado con ese motivo: no hay una
# segunda regla que lo cuente de otra manera.
.periodo_por_bloques_dbi <- function(conexion, preparacion, p, presupuesto,
                                     argumentos, fecha_hora, motivo) {
  campos <- preparacion$campos
  propias <- c(p$inicio, p$fin)
  universo <- .universo_periodo_dbi(p, campos, argumentos)
  columnas <- universo$columnas
  bloques <- list(); filas <- 0
  lectura <- .leer_columnas_por_bloques_dbi(
    conexion, preparacion$tabla_sql,
    vapply(columnas, function(c) as.character(DBI::dbQuoteIdentifier(conexion, c)), character(1L)),
    presupuesto, function(b) {
      filas <<- filas + nrow(b)
      # Pasado el tope no se cuenta sobre una parte, y tampoco se sigue
      # leyendo: `FALSE` le dice al lector que corte.
      if (filas > .TOPE_FILAS_TEXTO_FECHAS_DBI) return(FALSE)
      bloques[[length(bloques) + 1L]] <<- b
      TRUE
    }
  )
  if (!isTRUE(lectura$ok)) {
    return(.fila_periodo_sin_cifras(p, paste0(
      motivo, " No se pudo leer por bloques: ", lectura$motivo, "."
    )))
  }
  if (filas > .TOPE_FILAS_TEXTO_FECHAS_DBI) {
    return(.fila_periodo_sin_cifras(p, paste0(
      motivo, " La tabla tiene m\u00e1s de ",
      format(.TOPE_FILAS_TEXTO_FECHAS_DBI, scientific = FALSE),
      " filas: no se cuenta en R."
    )))
  }
  datos <- if (length(bloques)) do.call(rbind, bloques) else NULL
  if (is.null(datos)) {
    # Ninguna fila leida: las columnas con el tipo que entrega el controlador,
    # que es lo que R leeria de la tabla vacia.
    prototipo <- .prototipo_tabla_dbi(preparacion)
    posiciones <- if (is.null(prototipo)) NA_integer_ else {
      .indice_nombre(columnas, names(prototipo))
    }
    if (anyNA(posiciones)) {
      return(.fila_periodo_sin_cifras(p, paste0(
        motivo, " No se pudo leer por bloques: sin resultado."
      )))
    }
    datos <- prototipo[posiciones]
  }
  names(datos) <- columnas
  # Lo mismo que antes de leer, ahora con las filas: hay columnas cuyo tipo el
  # motor no declara, y solo lo leido dice que la regla no da un valor logico.
  regla_no_logica <- .regla_no_logica_periodo_dbi(
    p, .reglas_aplicabilidad_no_logicas(datos, universo$reglas)
  )
  if (!is.null(regla_no_logica)) {
    return(.fila_periodo_regla_no_logica_dbi(p, regla_no_logica))
  }
  declaracion <- list(inicio = p$inicio, fin = p$fin)
  if (length(p$fin_abierto)) declaracion$fin_abierto <- p$fin_abierto
  # Lo que no cambia el periodo y cuesta se apaga: las dependencias y los dos
  # diagnosticos de hallazgos que recorren el vocabulario y las ausencias.
  argumentos_perfil <- list(
    nombre = "periodos",
    fecha = if (inherits(argumentos$fecha, "Date")) argumentos$fecha else fecha_hora,
    muestra = Inf, analizar_dependencias = FALSE, periodos = declaracion,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    max_celdas_muestra = Inf, max_bytes_muestra = Inf
  )
  dominios <- argumentos$dominios
  if (isFALSE(dominios)) {
    argumentos_perfil$dominios <- FALSE
  } else if (is.list(dominios) && length(names(dominios))) {
    elegidos <- dominios[!is.na(.indice_nombre(names(dominios), propias))]
    if (length(elegidos)) argumentos_perfil$dominios <- elegidos
  }
  centinelas <- .normalizar_centinelas_fecha(argumentos$centinelas_fecha)
  if (length(centinelas)) argumentos_perfil$centinelas_fecha <- centinelas
  if (length(universo$reglas)) argumentos_perfil$aplicabilidad <- universo$reglas
  if (length(universo$opcionales)) {
    argumentos_perfil$columnas_opcionales <- universo$opcionales
  }
  perfil <- tryCatch(
    suppressWarnings(suppressMessages(
      do.call(perfilar, c(list(datos = datos), argumentos_perfil))
    )),
    error = function(e) e
  )
  if (inherits(perfil, "error")) {
    return(.fila_periodo_sin_cifras(p, paste0(
      motivo, " No se pudo contar en R: ",
      .sin_punto_final_dbi(
        .motivo_perfilado_muestra_dbi(perfil, datos, argumentos_perfil)
      ), "."
    )))
  }
  fila <- perfil$general$periodos
  fila$periodo <- p$periodo
  fila$motivo <- paste0(
    motivo,
    if (universo$declarado) {
      paste(" Se cont\u00f3 en R leyendo las columnas por bloques, sobre las",
            "filas donde corresponden las dos del periodo")
    } else " Se cont\u00f3 en R leyendo las dos columnas por bloques",
    if (isTRUE(lectura$avisos > 0)) paste0(" (el controlador avis\u00f3 ", lectura$avisos,
                                           " veces al convertir valores)") else "",
    "."
  )
  fila
}



# La clase de fecha de una columna tal como R la va a leer: la del prototipo y,
# si el prototipo no la trae, la que declara el controlador. RMariaDB entrega
# un resultado sin filas con `DATE` y `DATETIME` como numeros, aunque las filas
# llegan como `Date` y `POSIXct`: mirar solo el prototipo dejaba la columna
# afuera del resumen sin decirlo.
.clase_fecha_columna_dbi <- function(campo, preparacion) {
  prototipo <- preparacion$prototipo
  v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
  declarado <- if (!is.null(preparacion$tipos) && campo %in% names(preparacion$tipos)) {
    as.character(preparacion$tipos[[campo]])
  } else NA_character_
  if (inherits(v, "Date") || identical(declarado, "Date")) return("fecha")
  if (inherits(v, "POSIXct") || declarado %in% c("POSIXct", "POSIXt")) return("fecha-hora")
  NA_character_
}


# Un `TIMESTAMP` de MariaDB se guarda en UTC y se lee en el huso de la sesion:
# lo que R lee y lo que compara el literal dependen de ese huso. Se declara (con
# `"bloques"` se cuenta en R, sobre lo que R lee).
.MOTIVO_TIMESTAMP_FECHAS_DBI <- paste(
  "La columna es `TIMESTAMP`, que MariaDB lee en el huso de la sesi\u00f3n: el",
  "literal del motor y lo que R lee pueden no coincidir."
)

.MOTIVO_DUCKDB_HUSO_FORZADO_FECHAS_DBI <- paste(
  "DuckDB convierte la fecha-hora con `tz_out_convert = \"force\"` en un",
  "huso distinto de UTC: el motor y R pueden no coincidir."
)

.timestamp_con_huso_dbi <- function(preparacion, campo, motor) {
  if (!identical(motor, "mariadb")) return(FALSE)
  tipos <- preparacion$esquema$tipos_motor
  if (is.null(tipos) || !campo %in% names(tipos)) return(FALSE)
  identical(.minusculas_ascii(as.character(tipos[[campo]])), "timestamp")
}

.huso_forzado_dbi <- function(conexion, motor) {
  if (!identical(motor, "duckdb") || is.null(conexion)) return(FALSE)
  tryCatch(
    # Los slots de una clase S4 son atributos: se leen sin `methods`, que el
    # paquete no declara.
    identical(attr(conexion, "tz_out_convert", exact = TRUE), "force") &&
      !identical(attr(conexion, "timezone_out", exact = TRUE), "UTC"),
    error = function(e) FALSE
  )
}

