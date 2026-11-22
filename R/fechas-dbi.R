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
# Solo SQLite y DuckDB: en los demas motores la equivalencia no esta medida y el
# resumen se declara.

.MOTORES_RESUMEN_FECHAS_DBI <- c("sqlite", "duckdb")
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

# Lee una columna por bloques con el controlador -lo que R lee, con sus avisos- y
# le pasa cada bloque a `absorber()`. Gasta una consulta del presupuesto.
.leer_columna_por_bloques_dbi <- function(conexion, tabla_sql, columna_sql,
                                          presupuesto, absorber) {
  if (!.gastar_dbi(presupuesto)) {
    return(list(ok = FALSE, motivo = .motivo_presupuesto_dbi(presupuesto)))
  }
  avisos <- 0L
  sql <- paste0("SELECT ", columna_sql, " AS v FROM ", tabla_sql)
  salida <- tryCatch({
    res <- .enviar_consulta_dbi(conexion, sql)
    on.exit(.limpiar_consulta_dbi(res, conexion), add = TRUE)
    while (!DBI::dbHasCompleted(res)) {
      bloque <- withCallingHandlers(
        .traer_filas_dbi(res, n = .BLOQUE_FECHAS_DBI, conexion),
        warning = function(w) {
          avisos <<- avisos + 1L
          invokeRestart("muffleWarning")
        }
      )
      absorber(bloque[[1L]])
    }
    list(ok = TRUE, avisos = avisos, sql = sql)
  }, error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e)))
  salida
}

# El resumen de fechas de la tabla en el motor. Devuelve las filas (una por
# columna, las de `.fila_resumen_fechas()` mas `estado`, `motivo` y `sql`) y la
# cantidad de consultas.
.resumen_fechas_dbi <- function(conexion, preparacion, argumentos, fecha_hora,
                                fecha_es_date, estrategia_no_sql,
                                perfil_muestra = NULL) {
  campos <- preparacion$campos
  prototipo <- preparacion$prototipo
  tabla_sql <- preparacion$tabla_sql
  presupuesto <- preparacion$presupuesto
  motor <- tryCatch(.motor_medicion_dbi(conexion), error = function(e) NA_character_)
  referencia <- as.numeric(fecha_hora)
  clase_de <- function(campo) {
    v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
    if (inherits(v, "Date")) "fecha" else if (inherits(v, "POSIXct")) "fecha-hora" else NA_character_
  }
  clases <- vapply(campos, clase_de, character(1L), USE.NAMES = FALSE)
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
  con_aplicabilidad <- .nombres_para_operar(names(argumentos$aplicabilidad %||% list()))
  filas <- vector("list", length(campos))
  consultas <- 0L
  sql_pasada <- NA_character_
  declarar <- function(i, motivo) {
    fila <- .fila_resumen_fechas(campos[[i]])
    fila$clase_fecha <- tipos[[i]]
    .completar_fila_fechas_dbi(fila, "no_disponible", motivo)
  }
  global <- if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI) {
    paste0("El resumen de fechas en el motor solo est\u00e1 medido contra R en ",
           "SQLite y DuckDB; en este motor se lee de `perfil_muestra`.")
  } else if (identical(preparacion$universo, "muestra_motor")) {
    paste0("Con `universo = \"muestra_motor\"` el resumen describe la muestra del ",
           "motor: las fechas se leen de `perfil_muestra`.")
  } else NULL
  evaluables <- integer()
  sin_muestra <- is.null(tipos_muestra)
  for (i in seq_along(campos)) {
    if (is.na(clases[[i]]) && !texto_fecha[[i]]) {
      # Sin muestra no se sabe si una columna de texto es una fecha: se declara,
      # no se deja un `NA` mudo.
      v <- if (is.data.frame(prototipo)) prototipo[[campos[[i]]]] else NULL
      if (sin_muestra && (is.character(v) || is.factor(v))) {
        filas[[i]] <- declarar(i, paste(
          "Sin muestra no se sabe si esta columna de texto es una fecha:",
          "el resumen de fechas no la eval\u00faa."
        ))
      }
      next
    }
    if (!is.null(global)) {
      filas[[i]] <- declarar(i, global)
    } else if (.nombres_para_operar(campos[[i]]) %in% con_aplicabilidad) {
      filas[[i]] <- declarar(i, paste(
        "La columna tiene `aplicabilidad` declarada, que el resumen de fechas",
        "del motor no traduce: se lee de `perfil_muestra`."
      ))
    } else {
      evaluables <- c(evaluables, i)
    }
  }
  nativas <- evaluables[!is.na(clases[evaluables])]
  textos <- evaluables[is.na(clases[evaluables])]
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
      filas[[i]] <- declarar(i, paste0(
        "La columna `", campos[[i]], "` es texto en el motor y la muestra la lee ",
        "como fecha: sus formatos los decide la columna entera. Con ",
        "`estrategia_no_sql = \"bloques\"` se cuenta en R."
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
    if (demasiado) return(invisible())
    b <- as.character(b)
    filas <<- filas + length(b)
    b <- b[!is.na(b)]
    if (!length(b)) return(invisible())
    claves <- .clave_bytes(b)
    unicos <- !duplicated(claves)
    cuentas <- tabulate(match(claves, claves[unicos]), nbins = sum(unicos))
    previas <- match(claves[unicos], claves_voc)
    ya <- !is.na(previas)
    cuentas_voc[previas[ya]] <<- cuentas_voc[previas[ya]] + cuentas[ya]
    claves_voc <<- c(claves_voc, claves[unicos][!ya])
    formas_voc <<- c(formas_voc, b[unicos][!ya])
    cuentas_voc <<- c(cuentas_voc, cuentas[!ya])
    if (length(claves_voc) > .TOPE_VOCABULARIO_FECHAS_DBI ||
        filas > .TOPE_FILAS_TEXTO_FECHAS_DBI) demasiado <<- TRUE
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

# Los periodos en el motor: una consulta por tabla con la misma particion que
# `.particion_periodo()`, contra literales.
.periodos_dbi <- function(conexion, preparacion, periodos, reglas, clases,
                          motor, presupuesto) {
  if (is.null(periodos)) return(list(tabla = NULL, consultas = 0L))
  campos <- preparacion$campos
  tabla_sql <- preparacion$tabla_sql
  conocidas_base <- .dominio_fecha_numero(.CENTINELAS_FECHA_DOMINIO, "periodos")
  filas <- list(); partes <- character(); pendientes <- list()
  for (k in seq_along(periodos)) {
    p <- periodos[[k]]
    i <- .indice_nombre(p$inicio, campos); j <- .indice_nombre(p$fin, campos)
    motivo <- NULL
    if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI) {
      motivo <- "Los periodos en el motor solo est\u00e1n medidos contra R en SQLite y DuckDB."
    } else if (identical(preparacion$universo, "muestra_motor")) {
      motivo <- "Con `universo = \"muestra_motor\"` los periodos se leen de `perfil_muestra`."
    } else if (is.na(clases[[i]]) || is.na(clases[[j]])) {
      motivo <- "Las dos columnas del periodo tienen que ser fechas en el motor."
    } else if (identical(motor, "sqlite") && !identical(clases[[i]], clases[[j]])) {
      motivo <- paste(
        "En SQLite una fecha se guarda en d\u00edas y una fecha-hora en segundos:",
        "el motor no compara las dos como R."
      )
    }
    if (!is.null(motivo)) {
      fila <- .fila_periodo(p, .particion_periodo(numeric(), numeric(), numeric(), numeric(), numeric(), numeric()),
                            estado = "no_disponible", motivo = motivo)
      fila[, grepl("^n_|^prop_", names(fila))] <- NA
      filas[[k]] <- fila
      next
    }
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
    iv <- paste0("(", xi, " IS NOT NULL", if (length(decl_i)) paste0(" AND NOT ", en(xi, decl_i)) else "", ")")
    fn <- paste0("(", xf, " IS NULL)")
    fm <- paste0("(", xf, " IS NOT NULL AND ", en(xf, lit(marcas_seg, clases[[j]])), ")")
    fd <- paste0("(", xf, " IS NOT NULL AND NOT ", fm, " AND ", en(xf, lit(decl_f_seg, clases[[j]])), ")")
    fc <- paste0("(", xf, " IS NOT NULL AND NOT ", fm, " AND NOT ", fd, " AND ", en(xf, lit(conocidas_seg, clases[[j]])), ")")
    fv <- paste0("(", xf, " IS NOT NULL AND NOT ", fm, " AND NOT ", fd, " AND NOT ", fc, ")")
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
    pendientes[[length(pendientes) + 1L]] <- list(k = k, p = p, conocidas = conocidas_seg)
  }
  consultas <- 0L
  if (length(pendientes)) {
    sql <- paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql)
    r <- .consultar_dbi(conexion, sql, presupuesto, etapa = "periodos")
    consultas <- 1L
    for (pe in pendientes) {
      k <- pe$k
      if (!isTRUE(r$ok) || !is.data.frame(r$datos) || nrow(r$datos) != 1L) {
        fila <- .fila_periodo(pe$p, .particion_periodo(numeric(), numeric(), numeric(), numeric(), numeric(), numeric()),
                              estado = "no_disponible",
                              motivo = paste0("No se pudo contar el periodo en el motor: ", r$motivo %||% "sin resultado", "."))
        fila[, grepl("^n_|^prop_", names(fila))] <- NA
        filas[[k]] <- fila
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
                                        incluir_valores = TRUE) {
  resumen_fechas <- isTRUE(argumentos$resumen_fechas)
  periodos <- .normalizar_periodos(argumentos$periodos, preparacion$campos)
  if ((!resumen_fechas && is.null(periodos)) || !is.list(x$resumen_tabla)) return(x)
  fecha_es_date <- inherits(argumentos$fecha, "Date")
  resultado <- .resumen_fechas_dbi(
    conexion, preparacion, argumentos, fecha_hora, fecha_es_date,
    estrategia_no_sql, perfil_muestra = x$perfil_muestra
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
                       resultado$clases, motor, preparacion$presupuesto)
    rt$meta$periodos <- p$tabla
    rt$meta$declaracion_periodos <- periodos
  }
  if (is.list(rt$meta$consultas) && !is.null(preparacion$presupuesto)) {
    rt$meta$consultas$emitidas <- preparacion$presupuesto$usadas
  }
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

# Las consultas del resumen de fechas y de los periodos en el plan, sin leer
# filas. La pasada de cuentas es una; la reserva, una mas si alguna fecha
# incorporada llega a su minimo; la grilla, una por columna que la necesite;
# los bloques, una lectura por columna que el SQL no cuenta como R. Lo que
# depende de los datos va al extremo superior.
.plan_resumen_fechas_dbi <- function(plan, conexion, preparacion, resumen_fechas,
                                     periodos, estrategia_no_sql) {
  if (!isTRUE(resumen_fechas) && is.null(periodos)) return(plan)
  motor <- tryCatch(.motor_medicion_dbi(conexion), error = function(e) NA_character_)
  if (!motor %in% .MOTORES_RESUMEN_FECHAS_DBI ||
      identical(preparacion$universo, "muestra_motor")) {
    return(plan)
  }
  prototipo <- preparacion$prototipo
  fechas <- sum(vapply(preparacion$campos, function(campo) {
    v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
    inherits(v, c("Date", "POSIXct"))
  }, logical(1L)))
  textos <- sum(vapply(preparacion$campos, function(campo) {
    v <- if (is.data.frame(prototipo)) prototipo[[campo]] else NULL
    is.character(v) || is.factor(v)
  }, logical(1L)))
  filas <- list()
  if (isTRUE(resumen_fechas) && (fechas > 0 || textos > 0)) {
    bloques <- if (identical(estrategia_no_sql, "bloques")) fechas + textos else 0
    filas[[length(filas) + 1L]] <- data.frame(
      clase_consulta = "resumen de fechas (cuentas contra literales)",
      n_consultas = if (fechas > 0) 1 else 0,
      alcance = "agrega la tabla completa",
      n_consultas_max = (if (fechas > 0) 2 + fechas else 0) + bloques,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(periodos)) {
    filas[[length(filas) + 1L]] <- data.frame(
      clase_consulta = "periodos (particion contra literales)", n_consultas = 1,
      alcance = "agrega la tabla completa", n_consultas_max = 1,
      stringsAsFactors = FALSE
    )
  }
  if (!length(filas)) return(plan)
  atributos <- attributes(plan)
  nuevo <- rbind(as.data.frame(plan)[c("clase_consulta", "n_consultas", "alcance", "n_consultas_max")],
                 do.call(rbind, filas))
  rownames(nuevo) <- NULL
  for (a in setdiff(names(atributos), c("names", "row.names"))) {
    attr(nuevo, a) <- atributos[[a]]
  }
  nuevo
}
