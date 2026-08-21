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
# Perfilar 158 columnas emite 778 consultas y `muestra` cambia exactamente
# una: acota lo que se trae a R, no el trabajo del motor. `max_consultas` si
# lo acota, y lo que no entra en el presupuesto se declara no disponible en
# vez de quedar en un cero silencioso.

# Resolver la forma del desvio cuesta siempre dos sondas, se acierte en la
# primera o no. Podria cortarse al primer acierto y ahorrar una, pero entonces
# el costo dependeria del motor y el plan dejaria de decir exactamente cuantas
# consultas va a emitir. La exactitud del plan vale mas que una consulta.
.PEAJE_FORMA_DESVIO <- 2L

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
    sql <- paste0("SELECT ", nativas[[i]], "(1.0) AS ", alias("desvio"))
    intento <- .escalar_dbi(conexion, sql, "desvio", presupuesto)
    if (is.null(elegida) && isTRUE(intento$ok)) elegida <- i
  }
  # Sin ninguna nativa queda el calculo de dos pasadas, que es la ultima forma.
  presupuesto$forma_desvio <- if (is.null(elegida)) 3L else elegida
  invisible(NULL)
}

.presupuesto_dbi <- function(max_consultas = Inf) {
  estado <- new.env(parent = emptyenv())
  estado$max <- max_consultas
  estado$usadas <- 0
  estado$reserva <- 0
  estado$agotado <- FALSE
  # La forma de calcular el desvio se resuelve una vez por corrida y se recuerda:
  # probarla por columna gastaria hasta tres consultas cada vez y rompería la
  # promesa del plan, que dice exactamente cuantas va a emitir.
  estado$forma_desvio <- NULL
  estado
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
      limitar = function(sql, n, salto = 0) {
        paste0(
          sql, " LIMIT ", .entero_sql_dbi(n),
          if (salto > 0) paste0(" OFFSET ", .entero_sql_dbi(salto)) else ""
        )
      },
      muestreo = c("tablesample_system", "tablesample_percent", "random_limit")
    ),
    top = list(
      nombre = "top",
      descripcion = "TOP (n), y OFFSET k ROWS FETCH NEXT n ROWS ONLY con orden",
      motores = "SQL Server 2012 o posterior, Sybase",
      patron = "sql server|microsoft sql|sqlserver|mssql|tsql|sybase",
      alias_tabla = function(nombre) paste0(" AS ", nombre),
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
      muestreo = c("tablesample_system", "tablesample_percent", "random_limit")
    ),
    fetch_first = list(
      nombre = "fetch_first",
      descripcion = "OFFSET k ROWS FETCH FIRST n ROWS ONLY",
      motores = "Oracle 12c o posterior, DB2, Derby, H2",
      patron = "oracle|db2|informix|derby|hsqldb|\\bh2\\b",
      alias_tabla = function(nombre) paste0(" ", nombre),
      limitar = function(sql, n, salto = 0) {
        paste0(
          sql,
          if (salto > 0) paste0(" OFFSET ", .entero_sql_dbi(salto), " ROWS") else "",
          " FETCH FIRST ", .entero_sql_dbi(n), " ROWS ONLY"
        )
      },
      muestreo = c("tablesample_system", "tablesample_percent", "random_limit")
    ),
    rownum = list(
      nombre = "rownum",
      descripcion = "ROWNUM <= n; no expresa salto",
      motores = "Oracle anterior a 12c",
      patron = "oracle",
      alias_tabla = function(nombre) paste0(" ", nombre),
      limitar = function(sql, n, salto = 0) {
        if (salto > 0) return(NULL)
        paste0(
          "SELECT * FROM (", sql, ") lupa_recorte WHERE ROWNUM <= ",
          .entero_sql_dbi(n)
        )
      },
      muestreo = c("tablesample_system", "tablesample_percent")
    ),
    portable = list(
      nombre = "portable",
      descripcion = "sin clausula de limite: se acota con dbSendQuery() y dbFetch(n)",
      motores = "cualquier motor con DBI",
      patron = NA_character_,
      alias_tabla = function(nombre) paste0(" AS ", nombre),
      limitar = function(sql, n, salto = 0) NULL,
      muestreo = c("tablesample_system", "tablesample_percent", "random_limit")
    )
  )
}

.candidatos_muestreo_dbi <- function(conexion, dialecto) {
  nombres <- if (!is.null(dialecto$muestreo)) dialecto$muestreo else character()
  todas <- list(
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

.forma_muestreo_dbi <- function(candidato, tabla_sql, campos_sql, porcentaje,
                                muestra, dialecto, alias) {
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
    if (identical(candidato$tipo, "tablesample")) {
      tabla_sondeada <- candidato$constructor(tabla_sql, "1")
      sql <- paste0("SELECT 1 AS ", alias, " FROM ", tabla_sondeada, " WHERE 1 = 0")
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
        prueba <- .consultar_dbi(conexion, sql, presupuesto)
        if (is.null(aceptada) && isTRUE(prueba$ok) && !is.null(acotada)) {
          aceptada <- candidato
          aceptada$funciones <- list(funcion)
        }
      }
      next
    }
    sondas <- c(sondas, sql)
    prueba <- .consultar_dbi(conexion, sql, presupuesto)
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
    dialecto, NULL
  )
  if (is.null(forma)) return(NULL)
  forma$fraccion <- fraccion
  forma$filas_solicitadas <- as.numeric(muestra)
  forma
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
    sondas <- c(sondas, sql)
    resultado <- .consultar_dbi(conexion, sql, presupuesto)
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

.publicar_muestreo_dbi <- function(resolucion, forma = NULL, n_total = NA) {
  candidato <- resolucion$candidato
  list(
    disponible = isTRUE(resolucion$disponible),
    metodo = if (is.null(forma)) {
      if (is.null(candidato)) NA_character_ else candidato$nombre
    } else forma$metodo,
    descripcion = if (is.null(forma)) {
      if (is.null(candidato)) NA_character_ else candidato$descripcion
    } else forma$descripcion,
    fraccion = if (is.null(forma)) NA_real_ else forma$fraccion,
    tamano_muestra = if (is.null(forma)) NA_real_ else forma$filas_solicitadas,
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
    resultado <- .consultar_dbi(conexion, sql, presupuesto)
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

# `filas >= 0` usa la via portable: el motor prepara el resultado y R lee solo
# las filas pedidas. Es la unica forma de acotar sin clausula de dialecto.
.consultar_dbi <- function(conexion, sql, presupuesto = NULL, filas = -1L) {
  if (!.gastar_dbi(presupuesto)) {
    return(list(
      ok = FALSE, datos = NULL, motivo = .motivo_presupuesto_dbi(presupuesto)
    ))
  }
  if (filas < 0) {
    return(tryCatch(
      list(ok = TRUE, datos = DBI::dbGetQuery(conexion, sql), motivo = NA_character_),
      error = function(e) {
        list(ok = FALSE, datos = NULL, motivo = conditionMessage(e))
      }
    ))
  }
  resultado <- NULL
  on.exit(
    if (!is.null(resultado)) try(DBI::dbClearResult(resultado), silent = TRUE),
    add = TRUE
  )
  tryCatch({
    resultado <- DBI::dbSendQuery(conexion, sql)
    list(ok = TRUE, datos = DBI::dbFetch(resultado, n = filas), motivo = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, datos = NULL, motivo = conditionMessage(e))
  })
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

.escalar_dbi <- function(conexion, sql, campo, presupuesto = NULL) {
  resultado <- .consultar_dbi(conexion, sql, presupuesto)
  if (!resultado$ok) {
    return(list(ok = FALSE, valor = NULL, motivo = resultado$motivo))
  }
  .valor_campo_dbi(resultado$datos, campo)
}

.bit64_disponible_dbi <- function() {
  requireNamespace("bit64", quietly = TRUE)
}

.numero_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  if (inherits(valor, "integer64")) {
    return(suppressWarnings(as.numeric(valor[[1L]])))
  }
  suppressWarnings(as.numeric(valor[[1L]]))
}

.conteo_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(NA_real_)
  if (inherits(valor, "integer64")) return(valor[[1L]])
  texto <- if (is.character(valor)) {
    as.character(valor[[1L]])
  } else {
    NA_character_
  }
  if (.bit64_disponible_dbi() && !is.na(texto) &&
      grepl("^[+-]?[0-9]+$", texto)) {
    return(bit64::as.integer64(texto))
  }
  numero <- suppressWarnings(as.numeric(valor[[1L]]))
  if (!length(numero) || is.na(numero)) return(NA_real_)
  if (.bit64_disponible_dbi() && is.finite(numero) &&
      abs(numero) <= 2^53) {
    return(bit64::as.integer64(format(numero, scientific = FALSE, trim = TRUE)))
  }
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
  .conteo_dbi(round(observado / muestra_numero * universo_numero))
}

.conteo_exacto_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(FALSE)
  if (inherits(valor, "integer64")) return(.bit64_disponible_dbi())
  numero <- .numero_dbi(valor)
  is.finite(numero) && abs(numero) <= 2^53
}

.metadatos_sql_dbi <- function(alcance = "tabla_completa", universo = NA,
                               tamano_muestra = NA, fraccion = NA_real_,
                               metodo = NA_character_,
                               error_esperado = NA_character_) {
  list(
    alcance = alcance,
    universo = universo,
    tamano_muestra = tamano_muestra,
    fraccion = fraccion,
    metodo = metodo,
    error_esperado = error_esperado
  )
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
                              metadatos = NULL) {
  if (is.null(metadatos)) metadatos <- .metadatos_sql_dbi()
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
    stringsAsFactors = FALSE
  )
}

.registrar_resultado_dbi <- function(registros, columna, metricas, resultado,
                                     motivo_exito = NA_character_,
                                     metadatos = NULL) {
  metadatos <- .mezclar_metadatos_dbi(metadatos, resultado$metadatos)
  if (is.null(metadatos)) metadatos <- .metadatos_sql_dbi()
  estado <- if (!is.null(resultado$estado)) {
    resultado$estado
  } else if (resultado$ok) {
    "calculado"
  } else {
    "no_disponible"
  }
  motivo <- if (resultado$ok) motivo_exito else resultado$motivo
  sql <- if (is.null(resultado$sql)) NA_character_ else resultado$sql
  c(registros, list(.registro_sql_dbi(
    columna, metricas, estado, motivo, sql, metadatos
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

.validar_muestra_dbi <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      !is.finite(muestra) || muestra < 1 || muestra != floor(muestra)) {
    stop("`muestra` debe ser un entero positivo finito.", call. = FALSE)
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

.es_numerico_dbi <- function(prototipo, tipo_declarado = NA_character_) {
  if (inherits(prototipo, c("Date", "POSIXt", "difftime"))) return(FALSE)
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

# Los dos conteos recorren la misma tabla, asi que se piden juntos: una
# consulta por columna en vez de dos. `COUNT(DISTINCT ...)` no es universal, y
# consolidar acopla fallos, asi que si la consulta combinada se rechaza los dos
# conteos se reintentan por separado. Un rechazo no puede arrastrar a la otra
# metrica.
.conteos_columna_dbi <- function(conexion, tabla_sql, columna_sql, alias,
                                 pide_validos, pide_distintos,
                                 presupuesto = NULL,
                                 aproximacion_distintos = NULL) {
  sql_validos <- paste0(
    "SELECT COUNT(", columna_sql, ") AS ", alias("n_validos"),
    " FROM ", tabla_sql
  )
  sql_distintos <- if (!is.null(aproximacion_distintos)) {
    aproximacion_distintos$construir(
      columna_sql, tabla_sql, alias("n_distintos")
    )
  } else {
    paste0(
      "SELECT COUNT(DISTINCT ", columna_sql, ") AS ", alias("n_distintos"),
      " FROM ", tabla_sql
    )
  }
  if (pide_validos && pide_distintos) {
    sql <- paste0(
      "SELECT COUNT(", columna_sql, ") AS ", alias("n_validos"),
      ", COUNT(DISTINCT ", columna_sql, ") AS ", alias("n_distintos"),
      " FROM ", tabla_sql
    )
    consulta <- .consultar_dbi(conexion, sql, presupuesto)
    if (consulta$ok) {
      validos <- .valor_campo_dbi(consulta$datos, "n_validos")
      distintos <- .valor_campo_dbi(consulta$datos, "n_distintos")
      if (validos$ok && distintos$ok) {
        validos$sql <- sql
        distintos$sql <- sql
        if (!is.null(aproximacion_distintos)) {
          distintos$metadatos <- list(
            metodo = aproximacion_distintos$nombre,
            error_esperado = aproximacion_distintos$error_esperado
          )
          distintos$estado <- "estimado"
        }
        return(list(
          validos = validos, distintos = distintos, consolidada = TRUE
        ))
      }
    }
  }
  resultado <- list(consolidada = FALSE)
  if (pide_validos) {
    validos <- .escalar_dbi(conexion, sql_validos, "n_validos", presupuesto)
    validos$sql <- sql_validos
    resultado$validos <- validos
  }
  if (pide_distintos) {
    distintos <- .escalar_dbi(
      conexion, sql_distintos, "n_distintos", presupuesto
    )
    distintos$sql <- sql_distintos
    if (!is.null(aproximacion_distintos)) {
      distintos$metadatos <- list(
        metodo = aproximacion_distintos$nombre,
        error_esperado = aproximacion_distintos$error_esperado
      )
      distintos$estado <- "estimado"
    }
    resultado$distintos <- distintos
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
                                 fraccion_muestra = NA_real_) {
  if (is.null(dialecto)) dialecto <- .dialectos_dbi()$limit
  fila <- .fila_resumen_dbi(columna, n_total)
  literales <- character()
  es_muestreado <- identical(modo, "muestreado")
  metadatos <- if (es_muestreado) {
    .metadatos_sql_dbi(
      alcance = "muestra", universo = n_total,
      tamano_muestra = tamano_muestra, fraccion = fraccion_muestra,
      metodo = if (is.null(muestreo) || is.null(muestreo$metodo)) {
        NA_character_
      } else {
        muestreo$metodo
      },
      error_esperado = "desconocido"
    )
  } else {
    .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = n_total, tamano_muestra = NA,
      fraccion = 1,
      metodo = if (identical(modo, "aproximado")) "respaldo_exacto" else
        "tabla_completa",
      error_esperado = NA_character_
    )
  }
  registrar <- function(registros, metrica, resultado, motivo_exito = NA_character_) {
    if (es_muestreado && isTRUE(resultado$ok) && is.null(resultado$estado)) {
      resultado$estado <- if (any(metrica %in% c("n_distintos", "tasa_distintos"))) {
        "observado_muestra"
      } else {
        "estimado"
      }
    }
    .registrar_resultado_dbi(
      registros, columna, metrica, resultado, motivo_exito,
      metadatos = metadatos
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

  conteos <- .conteos_columna_dbi(
    conexion, tabla_sql, columna_sql, alias,
    "validos" %in% metricas, "distintos" %in% metricas, presupuesto,
    aproximacion_distintos = aproximacion_distintos
  )

  if ("validos" %in% metricas) {
    validos <- conteos$validos
    registros <- registrar(registros, .CAMPOS_METRICA_DBI$validos, validos)
    if (validos$ok) {
      validos_observados <- .conteo_dbi(validos$valor)
      fila$n_validos <- if (es_muestreado) {
        .conteo_estimado_dbi(validos_observados, n_total, tamano_muestra)
      } else {
        validos_observados
      }
      if (!is.na(fila$n_validos)) {
        fila$n_faltantes <- n_total - fila$n_validos
        if (es_muestreado) {
          muestra_numero <- .numero_dbi(tamano_muestra)
          fila$prop_faltantes <- if (is.finite(muestra_numero) && muestra_numero > 0) {
            (muestra_numero - .numero_dbi(validos_observados)) / muestra_numero
          } else if (.numero_dbi(n_total) == 0) {
            NA_real_
          } else {
            NA_real_
          }
        } else {
          fila$prop_faltantes <- if (.numero_dbi(n_total) > 0) {
            .numero_dbi(fila$n_faltantes) / .numero_dbi(n_total)
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
    distintos <- conteos$distintos
    if (identical(modo, "aproximado") && is.null(aproximacion_distintos)) {
      distintos$metadatos <- list(metodo = "COUNT(DISTINCT)")
    }
    registros <- registrar(registros, .CAMPOS_METRICA_DBI$distintos, distintos)
    if (distintos$ok) {
      candidato <- .conteo_dbi(distintos$valor)
      # Coherencia interna antes de aceptar el numero. No puede haber mas
      # valores distintos que validos; si el motor lo dice, el resultado es
      # imposible y corresponde declararlo no disponible en vez de publicarlo
      # como calculado. Una tasa mayor que 1 no es un dato: es un sintoma.
      limite_distintos <- if (es_muestreado && exists("validos_observados")) {
        .numero_dbi(validos_observados)
      } else {
        .numero_dbi(fila$n_validos)
      }
      imposible <- !is.na(candidato) && is.finite(limite_distintos) &&
        .numero_dbi(candidato) > limite_distintos
      if (imposible) {
        registros <- registrar(
          registros, .CAMPOS_METRICA_DBI$distintos,
          list(
            ok = FALSE, valor = NULL, sql = distintos$sql,
            motivo = paste0(
              "El motor informo ", candidato, " valores distintos sobre ",
              limite_distintos,
              " validos, que es imposible; la metrica no se publica."
            )
          )
        )
      } else {
        fila$n_distintos <- candidato
        if (!is.na(fila$n_distintos) && !is.na(fila$n_validos) &&
            .numero_dbi(fila$n_validos) > 0) {
          denominador <- if (es_muestreado && exists("validos_observados")) {
            .numero_dbi(validos_observados)
          } else {
            .numero_dbi(fila$n_validos)
          }
          fila$tasa_distintos <- .numero_dbi(fila$n_distintos) / denominador
        }
      }
    }
  } else {
    registros <- omitir(registros, "distintos", "no_solicitado", motivo_no_pedida)
  }

  if (!("moda" %in% metricas)) {
    registros <- omitir(registros, "moda", "no_solicitado", motivo_no_pedida)
  } else if (!incluir_valores) {
    registros <- omitir(registros, "moda", "omitido_por_privacidad", motivo_privacidad)
  } else {
    sin_limite <- paste0(
      "SELECT ", columna_sql, " AS ", alias("valor"), ", COUNT(*) AS ",
      alias("frecuencia"), " FROM ", tabla_sql, " WHERE ", columna_sql,
      " IS NOT NULL GROUP BY ", columna_sql, " ORDER BY ", alias("frecuencia"),
      " DESC, ", columna_sql, " ASC"
    )
    acotada <- dialecto$limitar(sin_limite, 1L, 0)
    sql_moda <- if (is.null(acotada)) sin_limite else acotada
    moda <- .consultar_dbi(
      conexion, sql_moda, presupuesto,
      filas = if (is.null(acotada)) 1L else -1L
    )
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
          # Un valor no puede repetirse mas veces que la cantidad de filas
          # validas. Si el motor lo dice, la metrica es imposible y se declara,
          # no se publica.
          limite_moda <- if (es_muestreado && exists("validos_observados")) {
            .numero_dbi(validos_observados)
          } else {
            .numero_dbi(fila$n_validos)
          }
          if (!is.na(candidato) && is.finite(limite_moda) &&
              .numero_dbi(candidato) > limite_moda) {
            moda$ok <- FALSE
            moda$motivo <- paste0(
              "El motor informo una frecuencia de ", candidato, " sobre ",
              limite_moda, " valores validos, que es imposible."
            )
          } else {
            fila$moda <- valor_moda
            fila$frecuencia_moda <- if (es_muestreado) {
              .conteo_estimado_dbi(candidato, n_total, tamano_muestra)
            } else {
              candidato
            }
          }
        }
      }
    } else if (moda$ok) {
      moda$motivo <- "La columna no contiene valores no nulos."
      moda$estado <- "sin_valores"
    }
    moda$sql <- sql_moda
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
    sql_basicos <- paste0(
      "SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql
    )
    basicos <- .consultar_dbi(conexion, sql_basicos, presupuesto)
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
          .escalar_finito_dbi(celda$valor)
        }
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
    basicos$sql <- sql_basicos
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
    } else if (!is.null(aproximacion_mediana)) {
      sql_mediana <- aproximacion_mediana$construir(
        columna_sql, tabla_sql, alias("mediana")
      )
      mediana <- .escalar_dbi(conexion, sql_mediana, "mediana", presupuesto)
      mediana$sql <- sql_mediana
      mediana$estado <- "estimado"
      mediana$metadatos <- list(
        metodo = aproximacion_mediana$nombre,
        error_esperado = aproximacion_mediana$error_esperado
      )
      if (mediana$ok) fila$mediana <- .escalar_finito_dbi(mediana$valor)
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
        mediana <- .escalar_dbi(conexion, sql_mediana, "mediana", presupuesto)
        if (mediana$ok) fila$mediana <- .escalar_finito_dbi(mediana$valor)
        mediana$sql <- sql_mediana
        if (identical(modo, "aproximado")) {
          mediana$metadatos <- list(metodo = "mediana_exacta")
        }
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
      # Tres formas, de la mas portable a la mas casera. Las dos primeras son
      # funciones nativas del motor: `STDDEV_SAMP` es la del estandar y la
      # aceptan PostgreSQL, MySQL y Oracle; `STDEV` es la de SQL Server. La
      # tercera es el calculo de dos pasadas, que sirve donde no hay ninguna.
      #
      # Esa tercera forma pone la media como subconsulta escalar —para que el
      # SQL guardado no lleve ningun valor derivado de los datos— y ahi esta el
      # detalle que solo aparecio contra un motor real: SQL Server rechaza una
      # subconsulta dentro de un agregado. El arreglo de privacidad habia roto
      # la compatibilidad, y ningun motor simulado lo iba a mostrar.
      media_sql <- paste0(
        "(SELECT AVG(", columna_sql, " * 1.0) FROM ", tabla_sql, ")"
      )
      formas <- list(
        paste0("SELECT STDDEV_SAMP(", columna_sql, " * 1.0) AS ",
               alias("desvio"), " FROM ", tabla_sql),
        paste0("SELECT STDEV(", columna_sql, " * 1.0) AS ",
               alias("desvio"), " FROM ", tabla_sql),
        paste0(
          "SELECT SQRT(SUM((", columna_sql, " - ", media_sql, ") * (",
          columna_sql, " - ", media_sql, ")) / (COUNT(", columna_sql,
          ") - 1.0)) AS ", alias("desvio"), " FROM ", tabla_sql
        )
      )
      .sondar_forma_desvio(conexion, presupuesto, alias)
      sql_desvio <- formas[[presupuesto$forma_desvio]]
      desvio <- .escalar_dbi(conexion, sql_desvio, "desvio", presupuesto)
      desvio$sql <- sql_desvio
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

.resumen_tabla_dbi <- function(conexion, tabla, tabla_sql, campos, prototipo,
                               n_total, sql_conteo, info_conexion,
                               dialecto = NULL, metricas = .METRICAS_DBI,
                               presupuesto = NULL, incluir_valores = TRUE,
                               tipos_declarados = NULL,
                               motivos_ilegibles = NULL,
                               modo = "exacto", tabla_metricas_sql = tabla_sql,
                               muestreo = NULL, aproximaciones = list(),
                               tamano_muestra = NA_real_,
                               fraccion_muestra = NA_real_) {
  if (is.null(dialecto)) dialecto <- .dialectos_dbi()$limit
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
      tamano_muestra = tamano_muestra, fraccion_muestra = fraccion_muestra
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
        campos, rep("n", length(campos)), "calculado", NA_character_, sql_conteo,
        metadatos = .metadatos_sql_dbi(
          alcance = if (identical(modo, "muestreado")) "tabla_muestreada" else
            "tabla_completa",
          universo = n_total, tamano_muestra = if (identical(modo, "muestreado")) {
            tamano_muestra
          } else NA_real_,
          fraccion = if (identical(modo, "muestreado")) fraccion_muestra else 1,
          metodo = if (identical(modo, "muestreado")) "conteo_universo" else
            "conteo_universo"
        )
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
      solo_lectura = TRUE,
       objetos_temporales = FALSE
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
  resultado <- .escalar_dbi(conexion, sql, "n_grupos_repetidos", presupuesto)
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
    conexion, paste0("SELECT * FROM ", tabla_sql, " WHERE 1 = 0"), presupuesto
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

.leer_esquema_dbi <- function(conexion, sql, presupuesto) {
  if (!.gastar_dbi(presupuesto)) {
    return(list(
      ok = FALSE, datos = NULL, tipos = NULL,
      motivo = .motivo_presupuesto_dbi(presupuesto)
    ))
  }
  resultado <- NULL
  on.exit(
    if (!is.null(resultado)) try(DBI::dbClearResult(resultado), silent = TRUE),
    add = TRUE
  )
  tryCatch({
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
    parcial <- .leer_esquema_dbi(conexion, sonda, presupuesto)
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
                                 modo = "exacto", muestreo_disponible = TRUE) {
  n_columnas <- length(campos)
  n_numericas <- sum(es_numerico)
  con_valores <- isTRUE(incluir_valores)
  acota_con_salto <- !is.null(dialecto$limitar("SELECT 1", 1L, 1))
  consolida_conteos <- all(c("validos", "distintos") %in% metricas)
  mide_metricas <- !identical(modo, "muestreado") || isTRUE(muestreo_disponible)
  n_metricas <- function(valor) if (mide_metricas) valor else 0
  clases <- list(
    c("portones (conteo, esquema y sondas)", emitidas, "una vez"),
    c(
      "conteos consolidados (no nulos + distintos)",
       if (consolida_conteos) n_metricas(n_columnas) else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "ordena o agrupa la tabla completa"
    ),
    c(
      "COUNT(col) no nulos",
       if ("validos" %in% metricas && !consolida_conteos) n_metricas(n_columnas) else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "escanea la tabla completa"
    ),
    c(
      "COUNT DISTINCT",
       if ("distintos" %in% metricas && !consolida_conteos) n_metricas(n_columnas) else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "ordena o agrupa la tabla completa"
    ),
    c(
      "moda (GROUP BY + orden + limite)",
       if ("moda" %in% metricas && con_valores) n_metricas(n_columnas) else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "ordena o agrupa la tabla completa"
    ),
    c(
      "MIN/MAX/AVG/SUM CASE", if ("basicos" %in% metricas) n_metricas(n_numericas) else 0,
      if (identical(modo, "muestreado")) "lee una muestra del motor" else
        "escanea la tabla completa"
    ),
    c(
      "mediana (orden total + limite/salto)",
       if ("mediana" %in% metricas && con_valores && acota_con_salto) {
         n_metricas(n_numericas)
       } else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "ordena la tabla completa"
    ),
    c(
      # La primera columna numerica paga hasta dos consultas extra probando las
      # formas nativas del motor; despues la forma queda resuelta y cada columna
      # cuesta una sola. El plan cuenta ese peaje para seguir siendo exacto.
      "desvio (nativo del motor, o SUM de cuadrados)",
       if ("desvio" %in% metricas && n_numericas > 0) {
         n_metricas(n_numericas + .PEAJE_FORMA_DESVIO)
       } else 0,
       if (identical(modo, "muestreado")) "lee una muestra del motor" else
         "escanea la tabla completa dos veces"
    ),
    c(
      "verificacion de unicidad del orden", if (con_orden) 1 else 0,
      "ordena o agrupa la tabla completa"
    ),
    c("muestra", 1, "lee las filas pedidas")
  )
  plan <- data.frame(
    clase_consulta = vapply(clases, function(x) x[[1L]], character(1L)),
    n_consultas = vapply(clases, function(x) as.numeric(x[[2L]]), numeric(1L)),
    alcance = vapply(clases, function(x) x[[3L]], character(1L)),
    stringsAsFactors = FALSE
  )
  plan <- plan[plan$n_consultas > 0, , drop = FALSE]
  rownames(plan) <- NULL
  plan
}

#' Planificar el costo de `perfilar_dbi()` antes de pagarlo
#'
#' Emite sólo las consultas-portón —contar filas, leer el esquema y sondear el
#' dialecto— y devuelve cuántas consultas emitiría el perfilado completo, de
#' qué clase y con qué alcance sobre la tabla. Con 158 columnas el perfilado
#' por omisión emite 778 consultas; saberlo antes es la diferencia entre una
#' herramienta y una sorpresa.
#'
#' @inheritParams perfilar_dbi
#'
#' @return Data frame con `clase_consulta`, `n_consultas` y `alcance`, y los
#'   atributos `total`, `columnas`, `columnas_numericas`, `dialecto`,
#'   `consultas_emitidas` y `metricas`.
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
plan_perfilado_dbi <- function(conexion, tabla, muestra = 1000L,
                               orden_muestra = NULL,
                               modo = c("exacto", "seguro", "conteos",
                                         "muestreado", "aproximado"),
                               metricas = NULL, max_consultas = Inf,
                               dialecto = "auto", incluir_valores = TRUE) {
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, muestra = muestra,
    orden_muestra = orden_muestra, modo = modo, metricas = metricas,
    max_consultas = max_consultas, dialecto = dialecto
  )
  es_numerico <- vapply(seq_along(preparacion$campos), function(i) {
    .es_numerico_dbi(
      preparacion$prototipo[[i]],
      if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else NA_character_
    )
  }, logical(1L))
  plan <- .plan_consultas_dbi(
    preparacion$campos, es_numerico, preparacion$metricas, incluir_valores,
    length(preparacion$orden_sql) > 0, preparacion$dialecto,
    emitidas = preparacion$presupuesto$usadas, modo = preparacion$modo,
    muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
      preparacion$muestreo$disponible
  )
  attr(plan, "total") <- sum(plan$n_consultas)
  attr(plan, "columnas") <- length(preparacion$campos)
  attr(plan, "columnas_numericas") <- sum(es_numerico)
  attr(plan, "columnas_ilegibles") <- preparacion$esquema$ilegibles
  attr(plan, "dialecto") <- preparacion$dialecto$nombre
  attr(plan, "consultas_emitidas") <- preparacion$presupuesto$usadas
  attr(plan, "metricas") <- preparacion$metricas
  attr(plan, "filas") <- preparacion$n_total
  plan
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

.bloque_muestra_dbi <- function(conexion, tabla, tabla_sql, campos, campos_sql,
                                muestra, orden_muestra, orden_sql, dialecto,
                                n_total, presupuesto, info_conexion,
                                argumentos, muestreo = NULL) {
  cobertura <- .cobertura_dbi_vacia()
  verificacion <- if (length(orden_sql)) {
    .verificar_orden_dbi(conexion, tabla_sql, orden_sql, dialecto, presupuesto)
  } else {
    list(
      unico = FALSE, sql = NA_character_,
      motivo = "No se declaro `orden_muestra`; SQL no garantiza el orden de las filas."
    )
  }
  n_obtener <- min(.numero_dbi(n_total), muestra)
  usa_muestreo <- !is.null(muestreo) && isTRUE(muestreo$disponible)
  fuente <- if (usa_muestreo) {
    .fuente_muestreada_dbi(
      tabla_sql, campos_sql, muestra, n_total, dialecto,
      list(candidato = muestreo$candidato)
    )
  } else {
    NULL
  }
  base_muestra <- if (!is.null(fuente)) {
    fuente$sql
  } else {
    paste0(
      "SELECT ", paste(campos_sql, collapse = ", "), " FROM ", tabla_sql,
      if (length(orden_sql)) {
        paste0(" ORDER BY ", paste(orden_sql, collapse = ", "))
      } else ""
    )
  }
  acotada <- if (!is.null(fuente)) {
    NULL
  } else if (muestra < .numero_dbi(n_total)) {
    dialecto$limitar(base_muestra, muestra, 0)
  } else {
    NULL
  }
  sql_muestra <- if (is.null(acotada)) base_muestra else acotada
  filas <- if (!is.null(fuente)) {
    fuente$filas
  } else if (is.null(acotada) && muestra < .numero_dbi(n_total)) {
    muestra
  } else {
    -1L
  }
  acotado_en <- if (!is.null(fuente)) {
    "motor_muestreo"
  } else if (!is.null(acotada)) {
    "motor"
  } else if (muestra < .numero_dbi(n_total)) {
    "cliente"
  } else {
    "sin recorte"
  }
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
  consulta <- .consultar_dbi(conexion, sql_muestra, presupuesto, filas = filas)
  if (!consulta$ok) {
    # Antes se descartaba aca el resumen entero: 158 columnas y 1548 metricas
    # ya calculadas, tiradas por la consulta numero 778. Ahora la muestra se
    # declara no disponible y el resumen sale igual, con su alcance.
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
  perfil <- tryCatch(
    do.call(perfilar, c(list(datos = datos_muestra), argumentos)),
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
                          metricas, max_consultas, dialecto) {
  .requerir_dbi()
  modo <- match.arg(
    modo, c("exacto", "seguro", "conteos", "muestreado", "aproximado")
  )
  dialecto <- match.arg(
    dialecto, c("auto", "limit", "top", "fetch_first", "rownum", "portable")
  )
  muestra <- .validar_muestra_dbi(muestra)
  metricas <- .validar_metricas_dbi(metricas, modo)
  max_consultas <- .validar_max_consultas_dbi(max_consultas)
  if (!.es_conexion_dbi(conexion)) {
    .detener_dbi(
      "lupa_error_conexion_dbi",
      "`conexion` debe ser una conexion DBI: no hereda de `DBIConnection`."
    )
  }
  if (!DBI::dbIsValid(conexion)) {
    .detener_dbi(
      "lupa_error_conexion_dbi",
      "`conexion` debe ser una conexion DBI abierta y valida."
    )
  }
  presupuesto <- .presupuesto_dbi(max_consultas)
  .contar_dbi(presupuesto)
  # Un nombre de dos partes con punto es lo que cualquiera escribe, y
  # `dbExistsTable()` no lo resuelve: lo toma como un nombre literal. Antes esto
  # hacia que `coleccion("esquema.tabla")` funcionara y `perfilar_dbi()` con el
  # mismo texto fallara diciendo que la tabla no existe. Se resuelve con el
  # mismo parseo que usa `coleccion()`, y solo si el nombre literal no existe:
  # una tabla que de verdad se llama con un punto adentro sigue ganando.
  if (is.character(tabla) && length(tabla) == 1L && !is.na(tabla) &&
      grepl(".", tabla, fixed = TRUE)) {
    literal <- tryCatch(
      isTRUE(DBI::dbExistsTable(conexion, tabla)), error = function(e) FALSE
    )
    if (!literal) {
      cortado <- tryCatch(.partir_identificador(tabla), error = function(e) NULL)
      partes <- if (!is.null(cortado) && !isTRUE(cortado$abierto)) {
        vapply(cortado$partes, .quitar_comillas_identificador, character(1L),
               USE.NAMES = FALSE)
      } else character()
      if (length(partes) == 2L && all(nzchar(partes))) {
        calificada <- DBI::Id(schema = partes[[1L]], table = partes[[2L]])
        if (isTRUE(tryCatch(
          DBI::dbExistsTable(conexion, calificada), error = function(e) FALSE
        ))) {
          tabla <- calificada
        }
      }
    }
  }
  existe <- tryCatch(
    DBI::dbExistsTable(conexion, tabla),
    error = function(e) e
  )
  if (inherits(existe, "condition")) {
    .detener_dbi("lupa_error_tabla_dbi", paste0(
      "No se pudo comprobar `tabla`: ", conditionMessage(existe)
    ))
  }
  if (!isTRUE(existe)) {
    # `dbExistsTable()` devuelve FALSE tanto si la tabla no existe como si la
    # credencial no la ve. Confundir las dos cosas manda al usuario a crear una
    # tabla que ya esta ahi.
    .detener_dbi("lupa_error_tabla_dbi", paste(
      "La tabla solicitada no existe en la conexion DBI, o la credencial no",
      "tiene permiso para verla. Las dos situaciones se ven igual desde",
      "dbExistsTable()."
    ))
  }
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
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

  sql_conteo <- paste0(
    "SELECT COUNT(*) AS ",
    as.character(DBI::dbQuoteIdentifier(conexion, "n")), " FROM ", tabla_sql
  )
  conteo <- .escalar_dbi(conexion, sql_conteo, "n", presupuesto)
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
    muestreo <- .sondar_muestreo_dbi(
      conexion, tabla_sql, resolucion$dialecto, presupuesto
    )
  }
  aproximaciones <- list()
  aproximaciones_resolucion <- list()
  if (identical(modo, "aproximado")) {
    if ("distintos" %in% metricas) {
      resolucion_distintos <- .sondar_aproximacion_dbi(
        conexion, "distintos", presupuesto
      )
      aproximaciones_resolucion$distintos <- resolucion_distintos
      if (!isTRUE(resolucion_distintos$disponible)) {
        aproximaciones$distintos <- NULL
      } else {
        aproximaciones$distintos <- resolucion_distintos$candidato
      }
    }
    if ("mediana" %in% metricas && any(es_numerico)) {
      resolucion_mediana <- .sondar_aproximacion_dbi(
        conexion, "mediana", presupuesto
      )
      aproximaciones_resolucion$mediana <- resolucion_mediana
      if (!isTRUE(resolucion_mediana$disponible)) {
        aproximaciones$mediana <- NULL
      } else {
        aproximaciones$mediana <- resolucion_mediana$candidato
      }
    }
  }
  list(
    modo = modo, metricas = metricas, muestra = muestra,
    max_consultas = max_consultas, presupuesto = presupuesto,
    tabla_sql = tabla_sql, campos = campos, campos_sql = esquema$campos_sql,
    prototipo = prototipo, tipos = esquema$tipos, esquema = esquema,
    es_numerico = es_numerico, muestreo = muestreo,
    aproximaciones = aproximaciones,
    aproximaciones_resolucion = aproximaciones_resolucion,
    n_total = n_total, sql_conteo = sql_conteo, orden_sql = orden_sql,
    orden_muestra = orden_muestra, dialecto = resolucion$dialecto,
    resolucion = resolucion, campos_declarados = campos_declarados,
    lista_campos = lista_campos
  )
}

#' Perfilar una muestra leída mediante DBI
#'
#' Calcula en SQL un resumen sobre la tabla completa o sobre una relación
#' muestreada por el motor, según `modo`, y en un bloque separado ejecuta
#' [perfilar()] sobre una muestra traída a memoria. El resumen completo de 105
#' campos no se presenta como calculado por la base: esos campos pertenecen
#' exclusivamente a `perfil_muestra` y su universo es la muestra.
#'
#' Esta función no escribe en la conexión ni crea objetos temporales. `DBI` es
#' una dependencia opcional. Cada agregado no disponible queda en `NA` y su
#' consulta, estado y motivo se conservan en `resumen_tabla$sql`.
#' Las expresiones se ejecutan como capacidades a comprobar, no como un
#' dialecto SQL universal.
#'
#' @section Fallo parcial:
#' Ningún bloque descarta al otro. Si el motor rechaza la consulta de muestra,
#' o si la muestra no se puede perfilar, el resultado sale igual con
#' `perfil_muestra = NULL` y una fila en `resumen_tabla$cobertura` que declara
#' el motivo y cómo resolverlo. Si el motor rechaza una columna, esa columna
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
#' `metodo` y `error_esperado`. Los distintos de una muestra se publican como
#' cardinalidad de la muestra, no como cardinalidad del universo. `modo =
#' "aproximado"` sondea `APPROX_COUNT_DISTINCT`, `approx_count_distinct` y las
#' formas de cuantiles del motor; cuando ninguna responde usa el respaldo exacto
#' y lo registra por métrica. Las cotas de error no documentadas quedan como
#' `"desconocido"`.
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
#'
#' @section Costo:
#' Perfilar 158 columnas con `modo = "exacto"` emite 778 consultas, y `muestra`
#' acota lo que se trae a R, no el trabajo del motor. `modo`, `metricas` y
#' `max_consultas` sí lo acotan, y [plan_perfilado_dbi()] dice cuántas
#' consultas se van a emitir antes de emitirlas. Lo que no entra en el
#' presupuesto queda en `no_disponible` con su motivo, nunca en cero.
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
#' @param muestra Cantidad positiva y finita de filas solicitadas.
#' @param orden_muestra Columnas para `ORDER BY`. La salida solo declara orden
#'   reproducible cuando la combinación es única en toda la tabla. Sin este
#'   argumento, DBI no garantiza el orden ni la pertenencia de una muestra
#'   limitada, y `meta` lo declara expresamente.
#' @param modo Conjunto de métricas del resumen: `"exacto"` las calcula todas,
#'   `"seguro"` evita las que ordenan o agrupan la tabla completa y
#'   `"conteos"` deja solo el conteo de valores no nulos, `"muestreado"`
#'   calcula estimaciones sobre filas elegidas por el motor y `"aproximado"`
#'   usa funciones nativas aproximadas cuando la sonda las acepta.
#' @param metricas Selección explícita de grupos de métricas, que tiene
#'   prioridad sobre `modo`: `"validos"`, `"distintos"`, `"moda"`,
#'   `"basicos"`, `"mediana"` y `"desvio"`.
#' @param max_consultas Presupuesto declarado de consultas. Al agotarse, las
#'   métricas restantes quedan en `no_disponible` con ese motivo.
#' @param dialecto Capacidad de acotar filas: `"auto"` la sondea, y
#'   `"limit"`, `"top"`, `"fetch_first"`, `"rownum"` o `"portable"` la
#'   declaran sin sondeo.
#' @param incluir_valores Si el resumen informa valores de celda: moda, mínimo,
#'   máximo y mediana. Con `FALSE` esas consultas no se emiten.
#' @param ... Argumentos enviados a [perfilar()] para analizar la muestra.
#'
#' @return Objeto de clase `perfil_dbi` con exactamente dos bloques:
#'   `resumen_tabla`, de alcance completo o muestreado según `modo`, y
#'   `perfil_muestra`, un objeto `perfil` cuyo `meta$origen_dbi` declara tabla,
#'   conexión, SQL y alcance, o `NULL` si la muestra no se pudo obtener.
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
perfilar_dbi <- function(conexion, tabla, muestra = 1000L,
                         orden_muestra = NULL,
                         modo = c("exacto", "seguro", "conteos", "muestreado",
                                   "aproximado"),
                         metricas = NULL, max_consultas = Inf,
                         dialecto = "auto", incluir_valores = TRUE, ...) {
  preparacion <- .preparar_dbi(
    conexion = conexion, tabla = tabla, muestra = muestra,
    orden_muestra = orden_muestra, modo = modo, metricas = metricas,
    max_consultas = max_consultas, dialecto = dialecto
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
    preparacion$campos, es_numerico, preparacion$metricas, incluir_valores,
    length(preparacion$orden_sql) > 0, preparacion$dialecto,
    emitidas = presupuesto$usadas, modo = preparacion$modo,
    muestreo_disponible = if (is.null(preparacion$muestreo)) TRUE else
      preparacion$muestreo$disponible
  )
  # Las dos consultas obligatorias que faltan -verificacion de orden y
  # muestra- se reservan para que el presupuesto no se las coma.
  presupuesto$reserva <- if (length(preparacion$orden_sql)) 2 else 1

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
  resumen <- .resumen_tabla_dbi(
    conexion, tabla, preparacion$tabla_sql, campos_todos,
    preparacion$prototipo, preparacion$n_total, preparacion$sql_conteo,
    info_conexion, dialecto = preparacion$dialecto,
    metricas = preparacion$metricas, presupuesto = presupuesto,
    incluir_valores = incluir_valores, tipos_declarados = preparacion$tipos,
    motivos_ilegibles = preparacion$esquema$motivos,
    modo = preparacion$modo, tabla_metricas_sql = tabla_metricas_sql,
    muestreo = muestreo_publico, aproximaciones = preparacion$aproximaciones,
    tamano_muestra = if (is.null(fuente_muestreada)) NA_real_ else
      fuente_muestreada$filas_solicitadas,
    fraccion_muestra = if (is.null(fuente_muestreada)) NA_real_ else
      fuente_muestreada$fraccion
  )
  resumen$meta$sql_esquema <- preparacion$esquema$sql
  resumen$meta$modo <- preparacion$modo
  resumen$meta$metricas <- preparacion$metricas
  resumen$meta$incluir_valores <- incluir_valores
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
  bloque <- .bloque_muestra_dbi(
    conexion, tabla, preparacion$tabla_sql, preparacion$campos,
    preparacion$campos_sql, preparacion$muestra, preparacion$orden_muestra,
    preparacion$orden_sql, preparacion$dialecto, preparacion$n_total,
    presupuesto, info_conexion, list(...), muestreo = muestreo_meta
  )
  cobertura <- rbind(cobertura, bloque$cobertura)
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

  if (is.null(bloque$perfil)) {
    motivos <- bloque$cobertura$motivo[
      bloque$cobertura$estado == "no_disponible"
    ]
    .avisar_dbi("lupa_muestra_dbi_no_disponible", paste0(
      "El resumen SQL se calculo y se devuelve, pero la muestra no: ",
      paste(motivos, collapse = " "),
      " Ver `resumen_tabla$cobertura`."
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
  if (is.null(x$perfil_muestra)) {
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
  cli::cli_text(
    "No se imprime ning\u00fan valor de celda: est\u00e1n en `resumen_tabla$columnas`."
  )
  invisible(x)
}
