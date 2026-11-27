# Bateria de conformidad entre la lectura de R y el dialecto DBI.
#
# No conformar es que el motor cuenta y da distinto que R. Que `medir_dbi()` se
# abstenga con los valores inventados por una guarda propia -texto no ASCII en
# SQLite, una regla que da NULL, un cotejamiento que no es binario, una
# construccion que el traductor no admite- no es no conformar: es quedar fuera
# de alcance con esos valores, y con los datos reales decide la misma guarda,
# como siempre. Por eso la abstencion se registra aparte y nunca le quita la
# regla a `medir_dbi()`: la bateria solo quita lo que el motor cuenta distinto.

.VIAS_SQL_CONFORMIDAD <- c(
  "sql_consolidado", "sql_dedicado", "sql_dedicado_referencial"
)

# Lo que `medir_dbi(conformidad =)` exige para aceptar una bateria: sin estos
# atributos no puede comprobar de que conexion, modelo y configuracion es.
.ATRIBUTOS_CONFORMIDAD_DBI <- c("motor", "identidad", "configuracion", "reglas")
.COLUMNAS_CONFORMIDAD_DBI <- c(
  "motor", "version", "intercalacion", "metrica_instanciada", "metrica",
  "regla", "caso", "esquema", "condicion", "n_motor", "n_r",
  "denominador_motor", "denominador_r", "coincide", "conformidad", "motivo",
  "accion"
)

.validar_argumentos_conformidad_dbi <- function(conexion, modelo,
                                                max_valores_in) {
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  if (!inherits(con, "DBIConnection")) {
    .detener("`conexion` debe ser una conexi\u00f3n DBI activa.", call. = FALSE)
  }
  if (!inherits(modelo, "modelo_calidad")) {
    .detener("`modelo` debe provenir de modelo().", call. = FALSE)
  }
  if (!is.numeric(max_valores_in) || length(max_valores_in) != 1L ||
      is.na(max_valores_in) || !is.finite(max_valores_in) ||
      max_valores_in < 1 || max_valores_in != floor(max_valores_in)) {
    .detener("`max_valores_in` debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(max_valores_in)
}

# ---------------------------------------------------------------------------
# Senas del motor y procedencia de la bateria
# ---------------------------------------------------------------------------

# Los servidores en los que corre la bateria, ademas de SQLite y DuckDB. En
# ellos la relacion de literales se escribe con el tipo de cada columna en la
# sintaxis del motor (`.escritura_columna_conformidad()`) y se nombra como una
# expresion de tabla comun: sin alias, una tabla derivada no vale en
# PostgreSQL 9.3, MySQL, MariaDB ni SQL Server.
.SERVIDORES_CONFORMIDAD_DBI <- c("postgresql", "mysql", "mariadb", "sqlserver")
.MOTORES_VERIFICADOS_CONFORMIDAD_DBI <- c(
  "sqlite", "duckdb", .SERVIDORES_CONFORMIDAD_DBI
)

# Version e intercalacion por omision de un servidor, en una sentencia. La
# intercalacion es la que toma una columna de texto creada sin cotejamiento
# propio: la de la base.
.SQL_SENAS_SERVIDOR_CONFORMIDAD_DBI <- list(
  postgresql = paste(
    "SELECT current_setting('server_version') AS version,",
    "d.datcollate AS intercalacion FROM pg_catalog.pg_database d",
    "WHERE d.datname = current_database()"
  ),
  mysql = "SELECT VERSION() AS version, @@collation_database AS intercalacion",
  sqlserver = paste(
    "SELECT CAST(SERVERPROPERTY('ProductVersion') AS NVARCHAR(128)) AS version,",
    "CAST(DATABASEPROPERTYEX(DB_NAME(), 'Collation') AS NVARCHAR(128))",
    "AS intercalacion"
  )
)

.senas_servidor_conformidad_dbi <- function(conexion, motor) {
  sql <- .SQL_SENAS_SERVIDOR_CONFORMIDAD_DBI[[
    if (identical(motor, "mariadb")) "mysql" else motor
  ]]
  leido <- tryCatch(.traer_consulta_dbi(conexion, sql), error = function(e) {
    .repropagar_transaccion_dbi(e)
    if (.es_interrupcion_dbi(e)) .detener(e)
    e
  })
  valor <- function(campo) {
    if (!is.data.frame(leido) || !nrow(leido) || !campo %in% names(leido)) {
      return(NA_character_)
    }
    x <- as.character(leido[[campo]][[1L]])
    if (!length(x) || is.na(x) || !nzchar(trimws(x))) NA_character_ else trimws(x)
  }
  version <- valor("version")
  intercalacion <- valor("intercalacion")
  motivo <- if (!is.na(intercalacion)) {
    NA_character_
  } else if (inherits(leido, "error")) {
    paste0("no se pudo leer la intercalaci\u00f3n por omisi\u00f3n del motor: ",
           .mensaje_motor(leido))
  } else {
    "el motor no inform\u00f3 su intercalaci\u00f3n por omisi\u00f3n"
  }
  list(version = version, intercalacion = intercalacion,
       intercalacion_motivo = motivo)
}

# Si la intercalacion por omision de MySQL o MariaDB es NO PAD, con el mismo
# criterio que el catalogo de una columna (`.esquema_medicion_dbi()`): MySQL lo
# publica en `information_schema.COLLATIONS`; MariaDB, en el nombre.
.no_pad_por_omision_conformidad_dbi <- function(conexion, motor, senas) {
  if (!motor %in% c("mysql", "mariadb")) return(NA)
  cot <- senas$intercalacion %||% NA_character_
  if (is.na(cot)) return(NA)
  cot_l <- .minusculas_ascii(cot)
  version <- .minusculas_ascii(senas$version %||% "")
  if (grepl("mariadb", version, fixed = TRUE, useBytes = TRUE)) {
    return(grepl("_nopad_", cot_l, fixed = TRUE, useBytes = TRUE) ||
             identical(cot_l, "binary"))
  }
  leido <- tryCatch(.traer_consulta_dbi(conexion, paste0(
    "SELECT PAD_ATTRIBUTE AS pad FROM information_schema.COLLATIONS ",
    "WHERE COLLATION_NAME = ", as.character(DBI::dbQuoteString(conexion, cot))
  )), error = function(e) {
    .repropagar_transaccion_dbi(e)
    if (.es_interrupcion_dbi(e)) .detener(e)
    NULL
  })
  if (!is.data.frame(leido) || !nrow(leido)) return(NA)
  identical(.mayusculas_ascii(trimws(as.character(leido[[1L]][[1L]]))), "NO PAD")
}

.version_conformidad_dbi <- function(conexion, motor) {
  if (motor %in% .SERVIDORES_CONFORMIDAD_DBI) {
    return(.senas_servidor_conformidad_dbi(conexion, motor))
  }
  sql <- switch(
    motor,
    sqlite = "SELECT sqlite_version() AS version",
    duckdb = "SELECT version() AS version",
    NULL
  )
  version <- if (is.null(sql)) {
    NA_character_
  } else {
    tryCatch(as.character(.traer_consulta_dbi(conexion, sql)$version[[1L]]),
             error = function(e) NA_character_)
  }
  intercalacion <- NA_character_
  motivo <- NA_character_
  if (identical(motor, "sqlite")) {
    intercalacion <- "BINARY"
  } else if (identical(motor, "duckdb")) {
    leida <- tryCatch(
      as.character(.traer_consulta_dbi(
        conexion, "SELECT current_setting('default_collation') AS collation"
      )$collation[[1L]]),
      error = function(e) NA_character_
    )
    if (!length(leida) || is.na(leida)) {
      motivo <- "no se pudo leer `default_collation` del motor"
    } else if (!nzchar(leida)) {
      # Sin intercalacion por omision, DuckDB compara el texto por sus bytes:
      # el valor vacio no es "desconocida", es la binaria.
      intercalacion <- "binary"
    } else {
      intercalacion <- .minusculas_ascii(leida)
    }
  } else {
    motivo <- paste0(
      "el motor `", motor, "` no est\u00e1 verificado en la bater\u00eda"
    )
  }
  list(version = version, intercalacion = intercalacion,
       intercalacion_motivo = motivo)
}

.huella_conformidad_dbi <- function(textos) {
  texto <- paste(as.character(textos), collapse = "\n")
  h <- .hash_texto_huella(texto)
  sprintf("%.0f-%.0f", h$h1, h$h2)
}

# El servidor se identifica por su ubicacion (archivo, anfitrion, puerto, base)
# y solo se guarda su huella: ni el usuario ni la ruta viajan en la bateria. La
# misma huella identifica la fuente de un referencial del motor en el sello.
.identidad_conformidad_dbi <- function(conexion, motor, senas) {
  list(
    motor = motor,
    version = as.character(senas$version %||% NA_character_),
    intercalacion = as.character(senas$intercalacion %||% NA_character_),
    servidor = .lugar_conexion_dbi(conexion)
  )
}

# El lugar es la identidad del servidor y de la base, y lo dice el motor en UNA
# sentencia, una vez por corrida y solo cuando alguien la pide (la bateria de
# conformidad, el sello de un referencial del motor). Lo que la conexion
# declara -`dbname`, `host`, `port`- es como se llego, no adonde: `localhost`
# y `127.0.0.1` son el mismo servidor, una ruta relativa y la absoluta son el
# mismo archivo, y dos servidores con una base del mismo nombre no son el mismo
# lugar. Dos bases en memoria no se distinguen: no tienen nada que las nombre.
#
# * SQLite y DuckDB: el archivo que el motor dice tener abierto, con la ruta
#   normalizada, en el mismo campo `dbname` de siempre: la huella de un archivo
#   abierto por su ruta absoluta no cambia.
# * PostgreSQL: el identificador del sistema (`pg_control_system()`, 9.6 en
#   adelante; antes, la direccion y el puerto del servidor) y la base actual.
# * MySQL y MariaDB: `@@hostname`, `@@port` y la base actual.
# * SQL Server: `@@SERVERNAME`, el nombre de la maquina y la base actual.
#
# Si el motor no contesta -otro motor, una credencial sin permiso- queda lo que
# la conexion declara, como antes.
.normalizar_archivo_lugar_dbi <- function(valor) {
  valor <- as.character(valor)
  if (length(valor) != 1L || is.na(valor) || !nzchar(trimws(valor))) {
    return(NA_character_)
  }
  # Sin plegar la caja con el locale de la sesion: solo ASCII.
  if (.minusculas_ascii(trimws(valor)) %in% c("memory", ":memory:", "<na>")) {
    return(NA_character_)
  }
  suppressWarnings(normalizePath(path.expand(valor), winslash = "/",
                                  mustWork = FALSE))
}

.SQL_LUGAR_DBI <- list(
  sqlite = "PRAGMA database_list",
  duckdb = paste(
    "SELECT path AS archivo FROM duckdb_databases()",
    "WHERE database_name = current_database()"
  ),
  postgresql = paste(
    "SELECT CAST(system_identifier AS TEXT) AS sistema,",
    "current_database() AS base FROM pg_control_system()"
  ),
  postgresql_viejo = paste(
    "SELECT host(inet_server_addr()) AS direccion,",
    "CAST(inet_server_port() AS TEXT) AS puerto, current_database() AS base"
  ),
  mysql = paste(
    "SELECT @@hostname AS anfitrion, CAST(@@port AS CHAR) AS puerto,",
    "DATABASE() AS base"
  ),
  sqlserver = paste(
    "SELECT CAST(@@SERVERNAME AS NVARCHAR(128)) AS servidor,",
    "CAST(SERVERPROPERTY('MachineName') AS NVARCHAR(128)) AS maquina,",
    "DB_NAME() AS base"
  )
)

# Lo que el motor dice de si mismo: una lista con `archivo`, o con las partes
# del servidor; `NULL` si no contesto.
.lugar_del_motor_dbi <- function(conexion) {
  motor <- tryCatch(.motor_medicion_dbi(conexion, "auto"),
                    error = function(e) "otro")
  forma <- switch(
    motor,
    sqlite = "sqlite", duckdb = "duckdb", mysql = , mariadb = "mysql",
    sqlserver = "sqlserver",
    postgresql = {
      version <- .version_servidor_postgresql_dbi(conexion)
      if (!is.na(version) && version < 90600L) {
        "postgresql_viejo"
      } else "postgresql"
    },
    NULL
  )
  if (is.null(forma)) return(NULL)
  datos <- tryCatch(
    .traer_consulta_dbi(conexion, .SQL_LUGAR_DBI[[forma]]),
    error = function(e) {
      .repropagar_transaccion_dbi(e)
      if (.es_interrupcion_dbi(e)) .detener(e)
      NULL
    }
  )
  if (!is.data.frame(datos) || !nrow(datos)) return(NULL)
  if (identical(forma, "sqlite")) {
    if (!all(c("name", "file") %in% names(datos))) return(NULL)
    principal <- which(as.character(datos$name) == "main")
    if (!length(principal)) return(NULL)
    archivo <- .normalizar_archivo_lugar_dbi(datos$file[[principal[[1L]]]])
    return(if (is.na(archivo)) NULL else list(archivo = archivo))
  }
  if (identical(forma, "duckdb")) {
    archivo <- .normalizar_archivo_lugar_dbi(datos[[1L]][[1L]])
    return(if (is.na(archivo)) NULL else list(archivo = archivo))
  }
  partes <- vapply(datos[1L, , drop = FALSE], function(x) {
    as.character(x[[1L]])
  }, character(1L))
  names(partes) <- .minusculas_ascii(names(datos))
  if (all(is.na(partes))) return(NULL)
  list(servidor = partes)
}

# Lo que la conexion declara de su lugar, sin SQL: los campos de `dbGetInfo()`
# (que la corrida ya leyo) con la base, el anfitrion -o la carpeta del socket-,
# el puerto y el nombre del servidor, como `nombre=valor`. Nunca el usuario.
# `archivo` reemplaza la base cuando el motor dijo que archivo tiene abierto.
.partes_lugar_declarado_dbi <- function(info, archivo = NULL) {
  nombres <- names(info)
  if (is.null(nombres)) nombres <- character()
  normales <- .minusculas_ascii(gsub("[^A-Za-z0-9]", "", nombres, useBytes = TRUE))
  lugar <- which(normales %in% c(
    "dbname", "databasename", "dbdir", "host", "hostname", "port",
    "server", "servername"
  ))
  vapply(lugar, function(i) {
    valor <- paste(as.character(unlist(info[[i]])), collapse = ",")
    if (!is.null(archivo) &&
        normales[[i]] %in% c("dbname", "databasename", "dbdir")) {
      valor <- archivo
    }
    paste0(normales[[i]], "=", valor)
  }, character(1L))
}

.lugar_conexion_dbi <- function(conexion) {
  corrida <- .corrida_dbi(conexion)
  if (!is.null(corrida) && !is.null(corrida$lugar)) return(corrida$lugar)
  info <- .info_senas_conexion_dbi(conexion)
  del_motor <- .lugar_del_motor_dbi(conexion)
  partes <- if (!is.null(del_motor$servidor)) {
    paste0(names(del_motor$servidor), "=", del_motor$servidor)
  } else {
    .partes_lugar_declarado_dbi(info, del_motor$archivo)
  }
  huella <- .huella_conformidad_dbi(c(class(conexion)[[1L]], .sort_por_bytes(partes)))
  if (!is.null(corrida)) corrida$lugar <- huella
  huella
}

.formulas_conformidad <- function(x) {
  if (inherits(x, "formula")) return(list(x))
  if (is.list(x) && !is.data.frame(x)) {
    return(unlist(lapply(x, .formulas_conformidad), recursive = FALSE))
  }
  list()
}

# Una formula se deparsea sin los valores que toma de su entorno: `~ t == x`
# con otro `x` es otra regla, y la huella tiene que verlo.
.libres_formula_conformidad <- function(f, columnas) {
  variables <- setdiff(all.vars(f), columnas)
  if (!length(variables)) return(character())
  entorno <- environment(f)
  vapply(.sort_por_bytes(variables), function(v) {
    valor <- if (is.null(entorno)) NULL else {
      tryCatch(get0(v, envir = entorno, inherits = TRUE),
               error = function(e) NULL)
    }
    if (is.function(valor)) valor <- "<funcion>"
    paste0(v, "=", tryCatch(.texto_configuracion_calidad(valor),
                            error = function(e) "<sin leer>"))
  }, character(1L))
}

.tablas_conformidad_dbi <- function(con, fisicas) {
  vapply(fisicas, function(f) {
    paste(as.character(DBI::dbQuoteIdentifier(con, f)), collapse = ".")
  }, character(1L))
}

# La definicion de cada regla: su configuracion completa, lo que sus formulas
# toman del entorno y la tabla fisica de cada entidad.
.definiciones_conformidad_dbi <- function(modelo, tablas_texto) {
  textos <- .configuracion_modelo_calidad(modelo)$metricas
  nombres <- names(textos)
  stats::setNames(vapply(nombres, function(nm) {
    instancia <- modelo$metricas[[nm]]
    libres <- unlist(lapply(
      .formulas_conformidad(instancia$configuracion),
      .libres_formula_conformidad, columnas = instancia$atributos
    ), use.names = FALSE)
    .huella_conformidad_dbi(c(
      textos[[nm]], libres,
      paste0(instancia$entidad, "->", tablas_texto[instancia$entidad])
    ))
  }, character(1L)), nombres)
}

.configuracion_conformidad_dbi <- function(motor, max_valores_in, aplicabilidad) {
  libres <- unlist(lapply(
    .formulas_conformidad(aplicabilidad), .libres_formula_conformidad,
    columnas = character()
  ), use.names = FALSE)
  .huella_conformidad_dbi(c(
    motor, as.character(as.integer(max_valores_in)),
    .texto_configuracion_calidad(aplicabilidad), libres
  ))
}

# De que conexion, modelo y configuracion es una bateria. Lo calculan igual
# `conformidad_dbi()` al armarla y `medir_dbi()` al recibirla.
.procedencia_conformidad_dbi <- function(conexion, modelo, tablas,
                                         aplicabilidad, max_valores_in, motor) {
  resuelto <- .resolver_tablas_medir_dbi(modelo, conexion, tablas)
  con <- resuelto$conexion
  senas <- .version_conformidad_dbi(con, motor)
  list(
    con = con,
    fisicas = resuelto$fisicas,
    senas = senas,
    identidad = .identidad_conformidad_dbi(con, motor, senas),
    configuracion = .configuracion_conformidad_dbi(
      motor, max_valores_in, aplicabilidad
    ),
    reglas = .definiciones_conformidad_dbi(
      modelo, .tablas_conformidad_dbi(con, resuelto$fisicas)
    )
  )
}

# ---------------------------------------------------------------------------
# Tipos de las columnas: los de la tabla real o, si no esta, los de la regla
# ---------------------------------------------------------------------------

# El esquema real sale del catalogo (`SELECT * ... WHERE 1 = 0` y las vistas
# del motor): ninguna fila del usuario se lee.
.sesion_conformidad_dbi <- function(con, motor) {
  tryCatch(
    .verificar_sesion_medicion_dbi(con, motor),
    error = function(e) {
      .repropagar_transaccion_dbi(e)
      if (.es_interrupcion_dbi(e)) .detener(e)
      list(ok = FALSE, motor = motor, n_consultas = 0L)
    }
  )
}

.esquemas_reales_conformidad_dbi <- function(con, fisicas, motor,
                                             sesion = .sesion_conformidad_dbi(con, motor)) {
  lapply(fisicas, function(f) {
    existe <- tryCatch(isTRUE(.existe_tabla_dbi(con, f)),
                       error = function(e) FALSE)
    if (!existe) return(NULL)
    tryCatch(.esquema_medicion_dbi(con, f, motor, sesion = sesion),
             error = function(e) NULL)
  })
}

.clase_conformidad <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  .clase_r_columna_dbi(x)
}

.clase_especificacion_conformidad <- function(spec) {
  valores <- switch(
    spec$nodo,
    no_ausente = spec$valores_nulos,
    en_conjunto = spec$conjunto,
    en_rango = list(spec$minimo, spec$maximo),
    fecha_en_rango = list(spec$desde, spec$hasta),
    NULL
  )
  if (!is.list(valores)) valores <- list(valores)
  valores <- Filter(function(v) !is.null(v) && length(v), valores)
  if (!length(valores)) return(NA_character_)
  .clase_conformidad(valores[[1L]])
}

.constantes_formula_conformidad <- function(expr, entorno, columnas) {
  resultado <- stats::setNames(vector("list", length(columnas)), columnas)
  agregar <- function(nombre, valor) {
    if (!nombre %in% columnas || is.null(valor) || !length(valor)) return()
    resultado[[nombre]] <<- c(resultado[[nombre]], as.list(valor))
  }
  constante <- function(e) {
    if (any(all.names(e, functions = FALSE) %in% columnas)) return(NULL)
    tryCatch(.evaluar_constante_formula_segura(e, entorno),
             error = function(err) NULL)
  }
  recorrer <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    nombre <- as.character(e[[1L]])
    args <- as.list(e)[-1L]
    if (nombre %in% c("==", "!=", "<", "<=", ">", ">=", "%in%") &&
        length(args) >= 2L) {
      for (i in seq_len(2L)) {
        if (is.symbol(args[[i]]) && as.character(args[[i]]) %in% columnas) {
          valor <- constante(args[[3L - i]])
          if (!is.null(valor)) agregar(as.character(args[[i]]), valor)
        }
      }
    }
    if (length(args)) invisible(lapply(args, recorrer))
    invisible(NULL)
  }
  recorrer(expr)
  resultado
}

# Las formulas que cuentan para la regla: la propia, su `aplicable` y la
# `aplicabilidad` de sus atributos (la misma seleccion que hace el traductor).
.formulas_regla_conformidad <- function(spec, instancia, aplicabilidad) {
  propias <- c(
    if (identical(spec$nodo, "predicado_fila")) list(spec$formula),
    if (!is.null(spec$aplicable)) list(spec$aplicable)
  )
  externas <- if (length(aplicabilidad)) {
    Filter(function(f) inherits(f, "formula"),
           aplicabilidad[names(aplicabilidad) %in% instancia$atributos])
  } else list()
  c(propias, unname(externas))
}

# Las columnas de cada entidad que la relacion inventada necesita. Un simbolo de
# una formula que no es columna -una constante del entorno- no entra: el
# traductor lo trataria como columna y mediria otra regla.
.columnas_caso_conformidad <- function(spec, instancia, aplicabilidad, reales) {
  if (identical(spec$nodo, "cobertura_referencial")) {
    if (identical(spec$entidad_ref, spec$entidad_dep)) {
      return(stats::setNames(list(unique(c(spec$pk, spec$fk))), spec$entidad_ref))
    }
    return(stats::setNames(list(spec$pk, spec$fk),
                           c(spec$entidad_ref, spec$entidad_dep)))
  }
  base <- switch(
    spec$nodo,
    duplicado = spec$columnas,
    predicado_fila = instancia$atributos,
    spec$columna
  )
  real <- reales[[spec$entidad]]
  extra <- unlist(lapply(
    .formulas_regla_conformidad(spec, instancia, aplicabilidad),
    function(f) {
      if (identical(spec$nodo, "predicado_fila") && identical(f, spec$formula)) {
        return(character())
      }
      v <- all.vars(f)
      if (!is.null(real)) return(intersect(v, real$campos))
      entorno <- environment(f)
      v[!vapply(v, function(s) {
        !is.null(entorno) && exists(s, envir = entorno, inherits = TRUE) &&
          !is.function(get0(s, envir = entorno, inherits = TRUE))
      }, logical(1L))]
    }
  ), use.names = FALSE)
  stats::setNames(list(unique(c(base, instancia$atributos, extra))), spec$entidad)
}

.inferencia_regla_conformidad <- function(spec, instancia, aplicabilidad,
                                          columnas) {
  clases <- stats::setNames(rep(NA_character_, length(columnas)), columnas)
  constantes <- stats::setNames(vector("list", length(columnas)), columnas)
  for (f in .formulas_regla_conformidad(spec, instancia, aplicabilidad)) {
    k <- .constantes_formula_conformidad(f[[2L]], environment(f), columnas)
    for (columna in columnas) {
      if (length(k[[columna]])) {
        constantes[[columna]] <- c(constantes[[columna]], k[[columna]])
      }
    }
  }
  for (columna in columnas) {
    if (length(constantes[[columna]])) {
      clases[[columna]] <- .clase_conformidad(constantes[[columna]][[1L]])
    }
  }
  if (spec$nodo %in% c("no_ausente", "en_conjunto", "en_rango", "fecha_en_rango")) {
    clase <- .clase_especificacion_conformidad(spec)
    if (!is.na(clase)) clases[[spec$columna]] <- clase
  }
  if (identical(spec$nodo, "predicado_fila") && is.symbol(spec$formula[[2L]])) {
    simbolo <- as.character(spec$formula[[2L]])
    if (simbolo %in% columnas) clases[[simbolo]] <- "logico"
  }
  list(clases = clases, constantes = constantes)
}

# Cada variante fija la clase de toda columna: la de la tabla real si existe, la
# que dicen los valores de la regla si no, y para lo que queda sin clase, cada
# candidata del tipo de regla.
.variantes_caso_conformidad <- function(spec, instancia, columnas, reales,
                                        aplicabilidad) {
  todas <- unique(unlist(columnas, use.names = FALSE))
  inferidas <- .inferencia_regla_conformidad(spec, instancia, aplicabilidad, todas)
  clases <- stats::setNames(lapply(names(columnas), function(e) {
    real <- reales[[e]]
    cols <- columnas[[e]]
    stats::setNames(vapply(cols, function(c) {
      if (!is.null(real) && c %in% real$campos) {
        real$clases[[c]]
      } else {
        inferidas$clases[[c]]
      }
    }, character(1L)), cols)
  }), names(columnas))
  desconocidas <- any(is.na(unlist(clases, use.names = FALSE)))
  candidatas <- if (!desconocidas) NA_character_ else switch(
    spec$nodo,
    no_ausente = , predicado_fila = c("texto", "doble", "fecha"),
    duplicado = , cobertura_referencial = c("texto", "doble"),
    "texto"
  )
  principal <- switch(
    spec$nodo,
    cobertura_referencial = list(spec$entidad_dep, spec$fk),
    duplicado = list(spec$entidad, spec$columnas[[1L]]),
    predicado_fila = list(spec$entidad, instancia$atributos[[1L]]),
    list(spec$entidad, spec$columna)
  )
  lapply(candidatas, function(candidata) {
    cl <- lapply(clases, function(x) {
      x[is.na(x)] <- candidata
      x
    })
    list(
      clases = cl,
      constantes = inferidas$constantes,
      etiqueta = cl[[principal[[1L]]]][[principal[[2L]]]]
    )
  })
}

# ---------------------------------------------------------------------------
# Valores inventados
# ---------------------------------------------------------------------------

.vacio_clase_conformidad <- function(clase) {
  switch(
    clase,
    texto = character(),
    entero = integer(),
    doble = numeric(),
    fecha = as.Date(character()),
    instante = as.POSIXct(character(), tz = "UTC"),
    logico = logical(),
    NULL
  )
}

.es_ascii_conformidad <- function(x) {
  vapply(x, function(s) is.na(s) || all(charToRaw(s) < as.raw(0x80L)),
         logical(1L), USE.NAMES = FALSE)
}

.limites_conformidad <- function(clase, no_ascii = FALSE) {
  switch(
    clase,
    texto = c(
      NA_character_, "", " x ", "X", "x", "123", "123 ",
      if (isTRUE(no_ascii)) {
        c(intToUtf8(0x00E1L), intToUtf8(0x00C1L), intToUtf8(0x00F1L))
      }
    ),
    entero = c(NA_integer_, -2147483647L, -1L, 0L, 1L, 2147483647L),
    doble = c(
      NA_real_, -1e12, -0.1, 0, 0.1, 1e12,
      9007199254740991, 9007199254740992
    ),
    fecha = as.Date(c(
      NA_character_, "1970-01-01", "2026-01-01", "2026-12-31",
      "9999-12-31"
    )),
    instante = as.POSIXct(c(
      NA_character_, "1970-01-01 00:00:00", "2026-01-01 00:00:00",
      "2026-12-31 23:59:59"
    ), tz = "UTC"),
    logico = c(NA, FALSE, TRUE),
    NULL
  )
}

# Lleva un valor de la regla a la clase de la columna, o lo descarta si en esa
# clase no se puede escribir como literal (NaN, un entero fuera de rango, un
# texto con bytes invalidos, un instante con fraccion de segundo).
.coercer_conformidad <- function(x, clase) {
  vacio <- .vacio_clase_conformidad(clase)
  if (is.null(x) || !length(x) || inherits(x, "integer64")) return(vacio)
  if (is.factor(x)) x <- as.character(x)
  fecha_o_instante <- inherits(x, c("Date", "POSIXt"))
  switch(
    clase,
    texto = if (is.character(x)) x[!is.na(x) & validUTF8(x)] else vacio,
    entero = if (is.numeric(x) && !fecha_o_instante) {
      y <- as.numeric(x)
      as.integer(y[is.finite(y) & y == floor(y) & abs(y) <= 2147483647])
    } else vacio,
    doble = if (is.numeric(x) && !fecha_o_instante) {
      y <- as.numeric(x)
      y[!is.na(y)]
    } else vacio,
    fecha = if (inherits(x, "Date")) x[!is.na(x)] else vacio,
    instante = if (inherits(x, "POSIXt")) {
      y <- as.POSIXct(x, tz = "UTC")
      y <- y[!is.na(y)]
      y[as.numeric(y) == floor(as.numeric(y))]
    } else vacio,
    logico = if (is.logical(x)) x[!is.na(x)] else vacio,
    vacio
  )
}

# Los bordes de un rango, de un lado y del otro. Se calculan en doble: el
# vecino de 2147483647L no es un entero, y la coercion lo descarta sin aviso.
.vecinos_conformidad <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(list())
  if (inherits(x, c("Date", "POSIXt"))) return(list(x - 1, x + 1))
  if (is.numeric(x) && !inherits(x, "integer64")) {
    y <- as.numeric(x)
    return(list(y - 1, y - 0.1, y + 0.1, y + 1))
  }
  list()
}

# Cada constante de texto entra tambien con otra caja y con espacios: es donde
# un cotejamiento que no es binario cuenta distinto que R.
.variantes_texto_conformidad <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(character())
  unique(c(x, .mayusculas_ascii(x), .minusculas_ascii(x),
           paste0(x, " "), paste0(" ", x)))
}

.valores_columna_conformidad <- function(clase, adicionales, no_ascii) {
  base <- .limites_conformidad(clase, no_ascii)
  if (is.null(base)) return(NULL)
  extra <- lapply(adicionales, .coercer_conformidad, clase = clase)
  extra <- if (length(extra)) do.call(c, extra) else .vacio_clase_conformidad(clase)
  if (identical(clase, "texto")) {
    extra <- .variantes_texto_conformidad(extra)
    if (!isTRUE(no_ascii)) extra <- extra[.es_ascii_conformidad(extra)]
  }
  unique(c(base, extra))
}

.adicionales_regla_conformidad <- function(spec) {
  switch(
    spec$nodo,
    en_conjunto = as.list(spec$conjunto),
    no_ausente = as.list(spec$valores_nulos),
    en_rango = c(
      list(spec$minimo, spec$maximo),
      .vecinos_conformidad(spec$minimo), .vecinos_conformidad(spec$maximo)
    ),
    fecha_en_rango = unlist(lapply(
      Filter(Negate(is.null), list(spec$desde, spec$hasta)),
      function(x) c(list(x), .vecinos_conformidad(x))
    ), recursive = FALSE),
    list()
  )
}

.base_duplicado_conformidad <- function(clase, no_ascii) {
  switch(
    clase,
    texto = c(
      "A", "A", "a", "a ", "", NA_character_, "123", "123 ",
      if (isTRUE(no_ascii)) {
        c(intToUtf8(0x00E1L), intToUtf8(0x00E1L), intToUtf8(0x00C1L),
          intToUtf8(0x00F1L))
      }
    ),
    entero = c(1L, 1L, 2L, 2L, 0L, NA_integer_, 2147483647L, -1L),
    doble = c(1, 1, 2, 2, 0, NA_real_, 9007199254740991, -1),
    fecha = as.Date(c(
      "2026-01-01", "2026-01-01", "2026-01-02", "2026-01-02",
      "1970-01-01", NA_character_, "9999-12-31", "2026-01-03"
    )),
    instante = as.POSIXct(c(
      "2026-01-01 00:00:00", "2026-01-01 00:00:00", "2026-01-01 00:00:01",
      NA_character_
    ), tz = "UTC"),
    logico = c(TRUE, TRUE, FALSE, NA),
    NULL
  )
}

.base_referencial_conformidad <- function(clase, no_ascii) {
  switch(
    clase,
    texto = list(
      ref = c("A", "B", "123", NA_character_,
              if (isTRUE(no_ascii)) intToUtf8(0x00F1L)),
      dep = c("A", "Z", NA_character_, "B", "A ",
              if (isTRUE(no_ascii)) c(intToUtf8(0x00F1L), "n"))
    ),
    entero = list(ref = c(1L, 2L, 2147483647L, NA_integer_),
                  dep = c(1L, 3L, NA_integer_, 2L, 2147483647L)),
    doble = list(ref = c(1, 2, 9007199254740991, NA_real_),
                 dep = c(1, 3, NA_real_, 2, 9007199254740991)),
    fecha = list(
      ref = as.Date(c("2026-01-01", "2026-01-02", NA_character_)),
      dep = as.Date(c("2026-01-01", "2026-01-03", NA_character_, "2026-01-02"))
    ),
    instante = list(
      ref = as.POSIXct(c("2026-01-01 00:00:00", NA_character_), tz = "UTC"),
      dep = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 00:00:01",
                         NA_character_), tz = "UTC")
    ),
    logico = list(ref = c(TRUE, NA), dep = c(TRUE, FALSE, NA)),
    NULL
  )
}

.reciclar_conformidad <- function(valores) {
  n <- max(lengths(valores))
  lapply(valores, function(x) rep(x, length.out = n))
}

# Dos columnas se cruzan todas con todas: asi `a > b` ve filas con `a` mayor,
# igual y menor que `b`, y la aplicabilidad ve cada valor dentro y fuera.
.tabla_caso_conformidad <- function(valores) {
  if (length(valores) == 2L && prod(lengths(valores)) <= 600) {
    n1 <- length(valores[[1L]])
    n2 <- length(valores[[2L]])
    i <- rep(seq_len(n1), times = n2)
    j <- rep(seq_len(n2), each = n1)
    valores <- stats::setNames(
      list(valores[[1L]][i], valores[[2L]][j]), names(valores)
    )
  } else {
    valores <- .reciclar_conformidad(valores)
  }
  as.data.frame(valores, check.names = FALSE, stringsAsFactors = FALSE)
}

.datos_caso_conformidad <- function(spec, columnas, clases, constantes,
                                    no_ascii) {
  if (identical(spec$nodo, "cobertura_referencial")) {
    ref <- .base_referencial_conformidad(
      clases[[spec$entidad_ref]][[spec$pk]], no_ascii
    )$ref
    dep <- .base_referencial_conformidad(
      clases[[spec$entidad_dep]][[spec$fk]], no_ascii
    )$dep
    if (is.null(ref) || is.null(dep)) return(NULL)
    if (identical(spec$entidad_ref, spec$entidad_dep)) {
      valores <- if (identical(spec$pk, spec$fk)) {
        stats::setNames(list(unique(c(ref, dep))), spec$pk)
      } else {
        .reciclar_conformidad(stats::setNames(list(ref, dep), c(spec$pk, spec$fk)))
      }
      tabla <- as.data.frame(valores, check.names = FALSE, stringsAsFactors = FALSE)
      return(stats::setNames(list(tabla), spec$entidad_ref))
    }
    tabla_ref <- as.data.frame(stats::setNames(list(ref), spec$pk),
                               check.names = FALSE, stringsAsFactors = FALSE)
    tabla_dep <- as.data.frame(stats::setNames(list(dep), spec$fk),
                               check.names = FALSE, stringsAsFactors = FALSE)
    return(stats::setNames(list(tabla_ref, tabla_dep),
                           c(spec$entidad_ref, spec$entidad_dep)))
  }
  entidad <- names(columnas)[[1L]]
  cols <- columnas[[entidad]]
  cl <- clases[[entidad]]
  if (identical(spec$nodo, "duplicado")) {
    valores <- stats::setNames(lapply(seq_along(cols), function(i) {
      base <- .base_duplicado_conformidad(cl[[cols[[i]]]], no_ascii)
      if (is.null(base)) return(NULL)
      n <- length(base)
      base[((seq_len(n) + i - 2L) %% n) + 1L]
    }), cols)
    if (any(vapply(valores, is.null, logical(1L)))) return(NULL)
    tabla <- as.data.frame(.reciclar_conformidad(valores), check.names = FALSE,
                           stringsAsFactors = FALSE)
    return(stats::setNames(list(tabla), entidad))
  }
  valores <- stats::setNames(lapply(cols, function(columna) {
    adicionales <- c(
      constantes[[columna]],
      if (identical(columna, spec$columna)) .adicionales_regla_conformidad(spec)
    )
    .valores_columna_conformidad(cl[[columna]], adicionales, no_ascii)
  }), cols)
  if (any(vapply(valores, is.null, logical(1L)))) return(NULL)
  stats::setNames(list(.tabla_caso_conformidad(valores)), entidad)
}

.filas_con_nulos_conformidad <- function(tabla) {
  if (!ncol(tabla)) return(logical(nrow(tabla)))
  Reduce(`|`, lapply(tabla, is.na))
}

# ---------------------------------------------------------------------------
# Relacion de literales con los tipos de la columna
# ---------------------------------------------------------------------------

# En un servidor, la columna supuesta es la que el motor crearia para esa
# clase sin mas indicaciones: su tipo (y el nombre de base que da el catalogo,
# que es lo que lee el traductor), y para el texto la intercalacion de la base.
# No es binaria en ninguno de los servidores por omision, y suponerla binaria
# -como en SQLite- hacia que el traductor contara con un cotejamiento que una
# columna nueva no tiene.
.TIPOS_SUPUESTOS_SERVIDOR_CONFORMIDAD_DBI <- list(
  postgresql = list(
    texto = c("TEXT", "text"), entero = c("INTEGER", "int4"),
    doble = c("DOUBLE PRECISION", "float8"), fecha = c("DATE", "date"),
    instante = c("TIMESTAMP WITHOUT TIME ZONE", "timestamp"),
    logico = c("BOOLEAN", "bool"), entero64 = c("BIGINT", "int8")
  ),
  mysql = list(
    texto = c("VARCHAR(255)", "varchar"), entero = c("INT", "int"),
    doble = c("DOUBLE", "double"), fecha = c("DATE", "date"),
    instante = c("DATETIME", "datetime"), logico = c("TINYINT(1)", "tinyint"),
    entero64 = c("BIGINT", "bigint")
  ),
  sqlserver = list(
    texto = c("NVARCHAR", "nvarchar"), entero = c("INT", "int"),
    doble = c("FLOAT", "float"), fecha = c("DATE", "date"),
    instante = c("DATETIME2", "datetime2"), logico = c("BIT", "bit"),
    entero64 = c("BIGINT", "bigint")
  )
)

.esquema_supuesto_servidor_conformidad <- function(motor, clases, senas) {
  campos <- names(clases)
  tabla <- .TIPOS_SUPUESTOS_SERVIDOR_CONFORMIDAD_DBI[[
    if (identical(motor, "mariadb")) "mysql" else motor
  ]]
  tipo_de <- function(clase, k) {
    t <- tabla[[clase]]
    if (is.null(t)) tabla$texto[[k]] else t[[k]]
  }
  texto <- unname(clases == "texto")
  todos <- function(valor) stats::setNames(rep(valor, length(campos)), campos)
  cot_texto <- if (identical(motor, "postgresql")) {
    "default"
  } else if (is.na(senas$intercalacion %||% NA_character_)) {
    "desconocido"
  } else {
    senas$intercalacion
  }
  cotejamientos <- stats::setNames(
    ifelse(texto, cot_texto, "desconocido"), campos
  )
  cot_l <- .minusculas_ascii(cot_texto)
  por_texto <- function(valor) stats::setNames(ifelse(texto, valor, NA), campos)
  list(
    referencia = NA_character_, nombre = NA_character_,
    sql = NULL, campos = campos, clases = clases,
    tipos = stats::setNames(vapply(clases, tipo_de, character(1L), k = 1L), campos),
    tipos_base = stats::setNames(vapply(clases, tipo_de, character(1L), k = 2L), campos),
    cotejamientos = cotejamientos,
    # PostgreSQL no admite una intercalacion no determinista como la de la
    # base: la columna supuesta compara por bytes al buscar igualdad.
    colacion_determinista = if (identical(motor, "postgresql")) {
      stats::setNames(texto, campos)
    } else todos(NA),
    colacion_binaria = if (motor %in% c("mysql", "mariadb")) {
      por_texto(grepl("(_bin|binary)$", cot_l, perl = TRUE, useBytes = TRUE))
    } else todos(NA),
    colacion_no_pad = if (motor %in% c("mysql", "mariadb")) {
      por_texto(isTRUE(senas$intercalacion_no_pad))
    } else todos(NA),
    colacion_bin2 = if (identical(motor, "sqlserver")) {
      por_texto(grepl("_bin2(_utf8)?$", cot_l, perl = TRUE, useBytes = TRUE))
    } else todos(NA),
    sesion = senas$sesion %||% list(ok = FALSE, motor = motor),
    n_campos = length(campos),
    prototipo = NULL, n_consultas_esquema = 0L,
    supuesto = todos(TRUE)
  )
}

.esquema_supuesto_conformidad <- function(motor, clases, senas) {
  if (motor %in% .SERVIDORES_CONFORMIDAD_DBI) {
    return(.esquema_supuesto_servidor_conformidad(motor, clases, senas))
  }
  campos <- names(clases)
  tipos <- stats::setNames(vapply(clases, function(clase) switch(
    clase,
    texto = "TEXT",
    entero = "INTEGER",
    doble = "DOUBLE",
    fecha = "DATE",
    instante = "TIMESTAMP",
    logico = "BOOLEAN",
    entero64 = "BIGINT",
    "TEXT"
  ), character(1L)), campos)
  cotejamiento <- if (identical(motor, "sqlite")) "binary" else "predeterminado"
  # DuckDB aplica a toda columna sin cotejamiento propio el de la sesion, y asi
  # lo lee `.esquema_medicion_dbi()`: el traductor lo ve igual aca.
  if (identical(motor, "duckdb") && !is.na(senas$intercalacion) &&
      !identical(senas$intercalacion, "binary")) {
    cotejamiento <- senas$intercalacion
  }
  todos <- function(valor) stats::setNames(rep(valor, length(campos)), campos)
  list(
    referencia = NA_character_, nombre = NA_character_,
    sql = NULL, campos = campos, clases = clases, tipos = tipos,
    tipos_base = stats::setNames(.minusculas_ascii(tipos), campos),
    cotejamientos = todos(cotejamiento),
    colacion_determinista = todos(TRUE),
    colacion_binaria = todos(TRUE),
    colacion_no_pad = todos(TRUE),
    colacion_bin2 = todos(TRUE),
    sesion = list(ok = TRUE), n_campos = length(campos),
    prototipo = NULL, n_consultas_esquema = 0L,
    supuesto = todos(TRUE)
  )
}

# El esquema del caso: el real, restringido a las columnas de la relacion; una
# columna que la tabla no tiene toma el tipo supuesto.
.esquema_caso_conformidad <- function(real, motor, clases, senas) {
  supuesto <- .esquema_supuesto_conformidad(motor, clases, senas)
  if (is.null(real)) return(supuesto)
  campos <- names(clases)
  en_real <- campos %in% real$campos
  esq <- real
  for (campo in c("clases", "tipos", "tipos_base", "cotejamientos",
                  "colacion_determinista", "colacion_binaria",
                  "colacion_no_pad", "colacion_bin2")) {
    v <- supuesto[[campo]]
    desde_real <- real[[campo]]
    if (!is.null(desde_real)) {
      v[en_real] <- desde_real[campos[en_real]]
    }
    esq[[campo]] <- v
  }
  esq$campos <- campos
  esq$n_campos <- length(campos)
  esq$sql <- NULL
  esq$supuesto <- stats::setNames(!en_real, campos)
  esq
}

# El tipo al que MySQL y MariaDB convierten un literal para escribirlo como lo
# guardaria la columna: `CAST()` solo admite unos pocos destinos, y no el tipo
# declarado (`VARCHAR(20)`, `INT`). El texto lleva el juego de caracteres de su
# intercalacion, que es el prefijo de su nombre: sin el, `COLLATE` rechaza la
# intercalacion de una columna `latin1` sobre un literal de la conexion.
.cast_mysql_conformidad <- function(base, completo, cotejamiento) {
  precision <- regmatches(completo, regexpr("\\([0-9]+\\)", completo, useBytes = TRUE))
  precision <- if (length(precision)) precision else ""
  juego <- if (!is.na(cotejamiento) &&
               grepl("^[a-z0-9]+_", cotejamiento, useBytes = TRUE)) {
    sub("_.*$", "", cotejamiento, useBytes = TRUE)
  } else NA_character_
  switch(
    base,
    char = , varchar = , tinytext = , text = , mediumtext = , longtext = ,
    enum = , set = if (is.na(juego)) "CHAR" else paste0("CHAR CHARACTER SET ", juego),
    tinyint = , smallint = , mediumint = , int = , integer = , bigint = {
      if (grepl("UNSIGNED", completo, fixed = TRUE, useBytes = TRUE)) {
        "UNSIGNED"
      } else "SIGNED"
    },
    decimal = , numeric = trimws(gsub(
      "(UNSIGNED|ZEROFILL)", "", completo, useBytes = TRUE
    )),
    float = "FLOAT",
    double = , real = "DOUBLE",
    date = "DATE",
    datetime = , timestamp = paste0("DATETIME", precision),
    time = paste0("TIME", precision),
    NA_character_
  )
}

# Con que se escribe cada valor de una columna de la relacion: el tipo del
# `CAST` y la clausula `COLLATE`, o el motivo por el que el motor no puede
# escribirlo con el tipo de la columna. En SQLite y DuckDB, una columna
# supuesta va sin tipo y sin intercalacion, y la real con lo que dice su
# catalogo. En un servidor toda columna lleva su tipo y su intercalacion, la
# real y la supuesta: un literal sin ellas no es lo que guardaria la columna
# (MySQL le da la intercalacion de la conexion, no la de la base).
.escritura_columna_conformidad <- function(conexion, motor, esq, columna) {
  supuesto <- isTRUE(esq$supuesto[[columna]])
  servidor <- motor %in% .SERVIDORES_CONFORMIDAD_DBI
  tipo <- if (supuesto && !servidor) NA_character_ else esq$tipos[[columna]]
  cotejamiento <- if (supuesto && !servidor) {
    NA_character_
  } else esq$cotejamientos[[columna]]
  if (is.null(tipo) || length(tipo) != 1L) tipo <- NA_character_
  if (is.null(cotejamiento) || length(cotejamiento) != 1L) {
    cotejamiento <- NA_character_
  }
  sin_cotejamiento <- is.na(cotejamiento) || .minusculas_ascii(cotejamiento) %in%
    c("binary", "predeterminado", "desconocido")
  if (!servidor) {
    collate <- if (!sin_cotejamiento &&
                   grepl("^[A-Za-z0-9_.]+$", cotejamiento, useBytes = TRUE)) {
      paste0(" COLLATE ", cotejamiento)
    } else ""
    return(list(tipo = if (!is.na(tipo) && nzchar(tipo)) tipo else NA_character_,
                collate = collate))
  }
  base <- .minusculas_ascii(as.character(esq$tipos_base[[columna]] %||% NA_character_))
  if (is.na(base)) base <- ""
  no_admite <- function(que) list(motivo = paste0(
    "la columna `", columna, "` es `", tipo, "` y ", motor, " no ", que
  ))
  if (is.na(tipo) || !nzchar(tipo)) {
    return(list(motivo = paste0(
      "el cat\u00e1logo de ", motor, " no dice el tipo de la columna `", columna, "`"
    )))
  }
  tipo_cast <- switch(
    motor,
    postgresql = tipo,
    mysql = , mariadb = .cast_mysql_conformidad(
      base, .mayusculas_ascii(tipo), .minusculas_ascii(cotejamiento)
    ),
    # Sin largo, `VARCHAR` es `VARCHAR(30)` y recortaria el valor.
    sqlserver = switch(
      base,
      varchar = "VARCHAR(MAX)", nvarchar = "NVARCHAR(MAX)",
      binary = , varbinary = , image = , xml = , sql_variant = , geography = ,
      geometry = , hierarchyid = , timestamp = , rowversion = NA_character_,
      tipo
    )
  )
  if (is.na(tipo_cast)) {
    return(no_admite("tiene una conversi\u00f3n (`CAST`) a ese tipo"))
  }
  collate <- ""
  if (!sin_cotejamiento && !(identical(motor, "postgresql") &&
                             identical(cotejamiento, "default"))) {
    collate <- if (identical(motor, "postgresql")) {
      paste0(" COLLATE ", as.character(DBI::dbQuoteIdentifier(conexion, cotejamiento)))
    } else if (grepl("^[A-Za-z0-9_]+$", cotejamiento, useBytes = TRUE)) {
      paste0(" COLLATE ", cotejamiento)
    } else {
      return(list(motivo = paste0(
        "la intercalaci\u00f3n `", cotejamiento, "` de la columna `", columna,
        "` no se puede escribir en una cl\u00e1usula COLLATE"
      )))
    }
  }
  list(tipo = tipo_cast, collate = collate)
}

.literal_relacion_conformidad <- function(conexion, valor, clase, motor, esq,
                                          columna,
                                          escritura = .escritura_columna_conformidad(
                                            conexion, motor, esq, columna
                                          )) {
  if (!is.null(escritura$motivo)) .detener(structure(
    class = c("lupa_conformidad_sin_tipo", "error", "condition"),
    list(message = escritura$motivo, call = NULL)
  ))
  tipo <- escritura$tipo
  lit <- if (is.na(valor)) {
    "NULL"
  } else if (identical(clase, "logico")) {
    # SQL Server no tiene `TRUE` ni `FALSE`: un `BIT` se escribe 1 o 0.
    if (motor %in% c("sqlite", "sqlserver")) {
      if (valor) "1" else "0"
    } else {
      if (valor) "TRUE" else "FALSE"
    }
  } else {
    .literal_sql_medicion(conexion, valor, clase, motor)
  }
  if (!is.na(tipo) && nzchar(tipo)) {
    lit <- paste0("CAST(", lit, " AS ", tipo, ")")
  }
  paste0(lit, escritura$collate)
}

.literales_columna_conformidad <- function(conexion, motor, x, esq, columna) {
  clase <- esq$clases[[columna]]
  escritura <- .escritura_columna_conformidad(conexion, motor, esq, columna)
  vapply(seq_along(x), function(i) {
    .literal_relacion_conformidad(conexion, x[i], clase, motor, esq, columna,
                                  escritura = escritura)
  }, character(1L))
}

.relacion_literal_conformidad <- function(conexion, motor, tabla, esq,
                                         con_fila = FALSE) {
  literales <- stats::setNames(lapply(names(tabla), function(columna) {
    .literales_columna_conformidad(conexion, motor, tabla[[columna]], esq, columna)
  }), names(tabla))
  alias <- vapply(names(tabla), function(c) .citar_col_dbi(conexion, c),
                  character(1L))
  if (!nrow(tabla)) {
    nulos <- paste0("NULL AS ", alias, collapse = ", ")
    return(paste0("(SELECT ", nulos, " WHERE 1 = 0)"))
  }
  filas <- vapply(seq_len(nrow(tabla)), function(i) {
    paste0(
      "SELECT ",
      paste0(vapply(names(tabla), function(columna) literales[[columna]][[i]],
                    character(1L)), " AS ", alias, collapse = ", "),
      if (con_fila) paste0(", ", i, " AS lupa_fila")
    )
  }, character(1L))
  paste0("(", paste(filas, collapse = " UNION ALL "), ")")
}

# Lo que R leeria de una columna real con ese valor: lo que devuelve el
# controlador al leer la relacion, llevado a la clase de la columna.
.clase_lectura_conformidad <- function(x, clase) {
  switch(
    clase,
    texto = as.character(x),
    entero = {
      y <- suppressWarnings(as.numeric(x))
      z <- rep(NA_integer_, length(y))
      ok <- !is.na(y) & y == floor(y) & abs(y) <= .Machine$integer.max
      z[ok] <- as.integer(y[ok])
      z
    },
    doble = suppressWarnings(as.numeric(x)),
    # RMySQL entrega `DATE` y `DATETIME` como texto `AAAA-MM-DD`; se lee con el
    # lector comun de texto con formato.
    fecha = if (inherits(x, "Date")) x else if (is.character(x)) {
      as.Date(.strptime_utc(x, "%Y-%m-%d"))
    } else {
      structure(suppressWarnings(as.numeric(x)), class = "Date")
    },
    instante = if (inherits(x, "POSIXct")) {
      as.POSIXct(x, tz = "UTC")
    } else if (is.character(x)) {
      as.POSIXct(.strptime_utc(x, "%Y-%m-%d %H:%M:%OS"), tz = "UTC")
    } else {
      as.POSIXct(suppressWarnings(as.numeric(x)), origin = "1970-01-01",
                 tz = "UTC")
    },
    logico = if (is.logical(x)) x else as.logical(suppressWarnings(as.numeric(x))),
    x
  )
}

# Con el tipo real, el motor guarda el valor inventado como lo guardaria en la
# columna: un UBIGINT redondea 9.9 a 10 y un REAL guarda 0.1 en precision
# simple. R tiene que contar sobre eso, no sobre lo que se invento: se vuelve a
# leer la relacion y cada valor se reemplaza por el leido. Lo que el motor no
# guarda (NULL donde habia valor) sale con su fila.
.ajustar_a_lo_guardado_conformidad <- function(conexion, motor, tabla, esq) {
  # En un servidor la columna supuesta tambien lleva tipo e intercalacion: se
  # relee como la real.
  reales <- names(tabla)[!vapply(names(tabla), function(c) {
    isTRUE(esq$supuesto[[c]]) && !motor %in% .SERVIDORES_CONFORMIDAD_DBI
  }, logical(1L))]
  if (!length(reales) || !nrow(tabla)) return(list(tabla = tabla))
  sql <- paste0(
    "SELECT * FROM ",
    .relacion_literal_conformidad(conexion, motor, tabla, esq, con_fila = TRUE),
    " lupa_r ORDER BY lupa_fila"
  )
  leido <- tryCatch(.traer_consulta_dbi(conexion, sql), error = function(e) e)
  if (inherits(leido, "error")) return(list(error = .mensaje_motor(leido)))
  if (!is.data.frame(leido) || nrow(leido) != nrow(tabla) ||
      !all(reales %in% names(leido))) {
    return(list(error = "no se pudo volver a leer la relaci\u00f3n inventada"))
  }
  quitar <- rep(FALSE, nrow(tabla))
  for (columna in reales) {
    nuevo <- .clase_lectura_conformidad(leido[[columna]], esq$clases[[columna]])
    quitar <- quitar | (!is.na(tabla[[columna]]) & is.na(nuevo))
    tabla[[columna]] <- nuevo
  }
  list(tabla = tabla[!quitar, , drop = FALSE])
}

# Si el motor no admite un valor inventado con el tipo real de la columna -un
# DECIMAL(10,2) no guarda 1e12-, ese valor tampoco puede estar en la tabla: se
# descarta su fila y el caso se cuenta con lo demas.
.depurar_datos_conformidad <- function(conexion, motor, datos, esquemas) {
  descartadas <- 0L
  depurados <- stats::setNames(lapply(names(datos), function(e) {
    tabla <- datos[[e]]
    esq <- esquemas[[e]]
    malas <- rep(FALSE, nrow(tabla))
    for (columna in names(tabla)) {
      lits <- .literales_columna_conformidad(
        conexion, motor, tabla[[columna]], esq, columna
      )
      unicos <- unique(lits[!is.na(tabla[[columna]])])
      fallan <- unicos[vapply(unicos, function(l) {
        inherits(tryCatch(
          .traer_consulta_dbi(conexion, paste0("SELECT ", l, " AS lupa_valor")),
          error = function(err) err
        ), "error")
      }, logical(1L))]
      malas <- malas | lits %in% fallan
    }
    descartadas <<- descartadas + sum(malas)
    tabla[!malas, , drop = FALSE]
  }), names(datos))
  list(datos = depurados, descartadas = descartadas)
}

# ---------------------------------------------------------------------------
# Contar un caso como lo cuenta `medir_dbi()` y como lo cuenta R
# ---------------------------------------------------------------------------

.compilar_caso_conformidad <- function(conexion, motor, spec, instancia,
                                       esquemas, aplicabilidad, max_valores_in) {
  universos <- .universos_medicion_dbi(
    aplicabilidad, esquemas, motor, conexion, max_valores_in
  )
  comp <- tryCatch(
    .compilar_especificacion_dbi(
      spec, instancia, esquemas, motor, conexion, universos, max_valores_in
    ),
    error = function(e) list(via = "no_medible", motivo = conditionMessage(e))
  )
  list(comp = comp, universos = universos)
}

.contar_caso_conformidad <- function(conexion, motor, spec, instancia, esquemas,
                                     aplicabilidad, max_valores_in,
                                     relaciones = list()) {
  con_relaciones <- function(sql) .con_relaciones_conformidad(sql, relaciones)
  compilado <- .compilar_caso_conformidad(
    conexion, motor, spec, instancia, esquemas, aplicabilidad, max_valores_in
  )
  comp <- compilado$comp
  if (!comp$via %in% .VIAS_SQL_CONFORMIDAD) {
    return(list(abstencion = list(causa = "traduccion", motivo = comp$motivo)))
  }
  contador <- new.env(parent = emptyenv())
  contador$n <- 0L
  contador$tope <- Inf
  contador$sql <- character()
  if (identical(comp$via, "sql_dedicado_referencial")) {
    q <- tryCatch(
      .consultar_medicion_dbi(conexion, con_relaciones(comp$consulta), contador),
      error = function(e) e
    )
    if (inherits(q, "error")) return(list(error = .mensaje_motor(q)))
    if (!is.null(q$guarda) && isTRUE(as.numeric(q$guarda) > 0)) {
      return(list(abstencion = list(
        causa = "guarda",
        motivo = comp$motivo_guarda %||% .motivo_guarda_dbi(motor)
      )))
    }
    return(list(num = as.numeric(q$num), den = as.numeric(q$den)))
  }
  entidad <- spec$entidad
  esq <- esquemas[[entidad]]
  item <- .preparar_item_medicion_dbi(
    c(list(j = 1L, instancia = instancia, spec = spec, entidad = entidad), comp),
    esq, compilado$universos[[entidad]](instancia), conexion, motor
  )
  # La consulta dedicada se manda con la relacion nombrada; la consolidada
  # termina en `FROM <relacion>`, y ahi la relacion va como tabla derivada con
  # el mismo nombre de alias.
  if (!is.null(item$consulta)) item$consulta <- con_relaciones(item$consulta)
  esq_consolidada <- esq
  if (length(relaciones) && esq$sql %in% names(relaciones)) {
    esq_consolidada$sql <- paste0(relaciones[[esq$sql]], " ", esq$sql)
  }
  r <- .consolidada_medicion_dbi(conexion, esq_consolidada, list(item), contador,
                                 motor)
  res <- .conteos_item_medicion_dbi(r, item, conexion, contador)
  if (!is.null(res$error)) return(list(error = res$error))
  abstencion <- .abstencion_conteos_medicion_dbi(item, res, motor)
  if (!is.null(abstencion)) return(list(abstencion = abstencion))
  list(num = res$num, den = res$den)
}

# Una relacion nombrada vale con alias y sin el, que es como la usa el
# traductor (`FROM t` y `FROM t lupa_r` en la misma sentencia): en un servidor
# cada relacion de literales es una expresion de tabla comun, y la sentencia
# que la usa la lleva delante.
.con_relaciones_conformidad <- function(sql, relaciones) {
  if (!length(relaciones)) return(sql)
  paste0(
    "WITH ", paste0(names(relaciones), " AS ", unlist(relaciones), collapse = ", "),
    " ", sql
  )
}

.esperado_caso_conformidad <- function(spec, instancia, datos, aplicabilidad) {
  # Los avisos de la regla sobre valores inventados no son del usuario: los da
  # `medir()` o `medir_dbi()` con sus datos. Aca solo importa la cuenta.
  r <- tryCatch(suppressWarnings({
    datos_esperados <- datos
    mascara <- NULL
    if (length(aplicabilidad)) {
      aplicables <- .mascaras_aplicabilidad_medicion(datos, aplicabilidad)
      datos_esperados <- .recortar_tablas_aplicables(datos, aplicables, instancia)
      entidad <- instancia$entidad[[1L]]
      mascara <- tryCatch(
        .mascara_aplicable_instancia(datos_esperados[[entidad]], instancia),
        error = function(e) NULL
      )
    }
    .evaluar_especificacion(spec, datos_esperados, instancia,
                            mascara_aplicable = mascara)
  }), error = function(e) e)
  if (inherits(r, "error")) return(list(error = conditionMessage(r)))
  if (identical(spec$nodo, "cobertura_referencial")) {
    return(list(num = as.numeric(r$numerador), den = as.numeric(r$denominador)))
  }
  cumple <- r$cumple
  if (!is.logical(cumple) || anyNA(cumple)) {
    return(list(error = "R no devolvi\u00f3 un vector l\u00f3gico sin NA"))
  }
  list(num = as.numeric(sum(cumple)), den = as.numeric(length(cumple)))
}

.caso_conformidad <- function(conexion, motor, spec, instancia, esquemas_base,
                              datos, aplicabilidad, max_valores_in) {
  # Lo que el traductor rechaza por el esquema no depende de los valores.
  marcadores <- lapply(esquemas_base, function(esq) {
    esq$sql <- "(SELECT 1 AS lupa_marcador)"
    esq
  })
  previo <- .compilar_caso_conformidad(
    conexion, motor, spec, instancia, marcadores, aplicabilidad, max_valores_in
  )$comp
  if (!previo$via %in% .VIAS_SQL_CONFORMIDAD) {
    return(list(conformidad = "abstencion", causa = "traduccion",
                motivo = previo$motivo, datos = datos))
  }
  sin_valores <- function(detalle) {
    list(
      conformidad = "no_evaluada",
      motivo = paste0(
        "el motor no admite los valores inventados con el tipo de la columna: ",
        detalle
      ),
      datos = datos
    )
  }
  for (e in names(datos)) {
    for (columna in names(datos[[e]])) {
      escritura <- .escritura_columna_conformidad(
        conexion, motor, esquemas_base[[e]], columna
      )
      if (!is.null(escritura$motivo)) return(sin_valores(escritura$motivo))
    }
  }
  for (e in names(datos)) {
    ajuste <- .ajustar_a_lo_guardado_conformidad(
      conexion, motor, datos[[e]], esquemas_base[[e]]
    )
    if (!is.null(ajuste$error)) {
      depuracion <- .depurar_datos_conformidad(
        conexion, motor, datos[e], esquemas_base[e]
      )
      if (depuracion$descartadas) {
        ajuste <- .ajustar_a_lo_guardado_conformidad(
          conexion, motor, depuracion$datos[[e]], esquemas_base[[e]]
        )
      }
    }
    if (!is.null(ajuste$error)) return(sin_valores(ajuste$error))
    if (!nrow(ajuste$tabla)) return(sin_valores("no qued\u00f3 ninguna fila"))
    datos[[e]] <- ajuste$tabla
  }
  conteo <- NULL
  for (intento in 1:2) {
    esquemas <- esquemas_base
    relaciones <- list()
    for (i in seq_along(names(datos))) {
      e <- names(datos)[[i]]
      relacion <- .relacion_literal_conformidad(
        conexion, motor, datos[[e]], esquemas[[e]]
      )
      if (motor %in% .SERVIDORES_CONFORMIDAD_DBI) {
        nombre <- paste0("lupa_conf_", i)
        relaciones[[nombre]] <- relacion
        esquemas[[e]]$sql <- nombre
      } else {
        esquemas[[e]]$sql <- relacion
      }
    }
    conteo <- .contar_caso_conformidad(
      conexion, motor, spec, instancia, esquemas, aplicabilidad, max_valores_in,
      relaciones = relaciones
    )
    if (is.null(conteo$error) || intento == 2L) break
    depuracion <- .depurar_datos_conformidad(conexion, motor, datos, esquemas_base)
    if (!depuracion$descartadas) break
    depurados <- depuracion$datos
    if (any(vapply(depurados, nrow, integer(1L)) == 0L)) {
      return(sin_valores(conteo$error))
    }
    datos <- depurados
  }
  if (!is.null(conteo$error)) {
    return(list(conformidad = "abstencion", causa = "consulta",
                motivo = conteo$error, datos = datos))
  }
  if (!is.null(conteo$abstencion)) {
    return(list(conformidad = "abstencion", causa = conteo$abstencion$causa,
                motivo = conteo$abstencion$motivo, datos = datos))
  }
  esperado <- .esperado_caso_conformidad(spec, instancia, datos, aplicabilidad)
  if (!is.null(esperado$error)) {
    return(list(
      conformidad = "no_conforme", causa = "r",
      n_motor = conteo$num, den_motor = conteo$den,
      motivo = paste0(
        "el motor cont\u00f3 ", conteo$num, "/", conteo$den,
        " y R rechaz\u00f3 la regla con estos valores: ", esperado$error
      ),
      coincide = FALSE, datos = datos
    ))
  }
  coincide <- identical(as.numeric(conteo$num), as.numeric(esperado$num)) &&
    identical(as.numeric(conteo$den), as.numeric(esperado$den))
  list(
    conformidad = if (coincide) "conforme" else "no_conforme",
    causa = if (coincide) NA_character_ else "conteo",
    n_motor = conteo$num, den_motor = conteo$den,
    n_r = esperado$num, den_r = esperado$den,
    coincide = coincide,
    motivo = if (coincide) NA_character_ else paste0(
      "el motor cont\u00f3 ", conteo$num, "/", conteo$den,
      " y R cont\u00f3 ", esperado$num, "/", esperado$den
    ),
    datos = datos
  )
}

# ---------------------------------------------------------------------------
# Filas de la bateria
# ---------------------------------------------------------------------------

.accion_conformidad <- function(conformidad, causa, instancia, spec) {
  separable <- tryCatch(.separable_medicion_dbi(instancia, spec),
                        error = function(e) FALSE)
  rechazo <- if (isTRUE(separable)) {
    "medir por bloques en R"
  } else {
    "dejar la regla como no_medible"
  }
  switch(
    conformidad,
    conforme = NA_character_,
    no_conforme = rechazo,
    no_evaluada = "medir_dbi() mide la regla como siempre y avisa",
    no_verificado = "verificar el motor y su cotejamiento antes de medir",
    abstencion = if (isTRUE(causa %in% c("traduccion", "sin_especificacion",
                                         "no_logico"))) {
      rechazo
    } else {
      "ninguna: con los datos reales decide la guarda de medir_dbi()"
    },
    NA_character_
  )
}

# Las metricas referenciales no tienen especificacion: `medir_dbi()` las mide
# con su propia via (`R/referencial-dbi.R`). La igualdad por bytes es la de las
# demas reglas; lo propio de esta via es el pliegue de la normalizacion, que el
# motor escribe con `LOWER()` fuera de su guarda. La bateria lo comprueba en el
# servidor: cada caracter ASCII en tres posiciones contra tres bases, como
# valores inventados en un `SELECT`, y cuenta los pares que el motor y R
# declaran iguales fuera de la guarda, con el perfil de la metrica.
.casos_referencial_conformidad <- function(conexion, instancia, motor, senas) {
  fila <- function(r, caso) {
    r$accion <- .accion_conformidad(r$conformidad, r$causa, instancia, NULL)
    list(.fila_conformidad(
      instancia, "referencial", motor, senas, caso, NA_character_, "siempre", r
    ))
  }
  if (!motor %in% .MOTORES_REFERENCIAL_DBI) {
    return(fila(list(
      conformidad = "abstencion", causa = "traduccion",
      motivo = paste0(
        "las m\u00e9tricas referenciales se miden en el motor en SQLite y ",
        "DuckDB; en `", motor, "` medir_dbi() no las mide"
      )
    ), "regla"))
  }
  referencia <- instancia$referencial
  if (!.referencial_en_motor(referencia)) {
    return(fila(list(
      conformidad = "abstencion", causa = "sin_especificacion",
      motivo = paste0(
        "el referencial est\u00e1 en memoria: medir_dbi() lo compara en R sobre ",
        "los valores distintos (con `bloques`), no en el motor"
      )
    ), "regla"))
  }
  perfil <- .referencial_normalizacion(instancia, referencia)
  columnas <- unique(c(instancia$atributos, referencia$clave, referencia$valor))
  perfiles <- lapply(columnas, function(cn) .normalizacion_para_columna(perfil, cn))
  perfiles <- perfiles[!duplicated(lapply(perfiles, unclass))]
  caracteres <- intToUtf8(1:127, multiple = TRUE)
  textos <- unique(c(paste0(caracteres, "ab"), paste0("a", caracteres, "b"),
                     paste0("ab", caracteres)))
  bases <- c("ab", "AB", "aB")
  valores <- function(x) paste0(
    "(", vapply(x, function(v) as.character(DBI::dbQuoteString(conexion, v)), ""),
    ")", collapse = ", "
  )
  filas <- list()
  for (k in seq_along(perfiles)) {
    p <- perfiles[[k]]
    lado <- function(e) .sql_texto_binario_dbi(
      if (isTRUE(p$minusculas)) paste0("LOWER(", e, ")") else e, motor
    )
    sql <- paste0(
      "WITH lupa_t(x) AS (VALUES ", valores(textos), "), lupa_b(y) AS (VALUES ",
      valores(bases), ") SELECT x, y, CASE WHEN ", lado("x"), " = ", lado("y"),
      " THEN 1 ELSE 0 END AS igual, CASE WHEN ", .sql_guarda_pliegue_dbi("x", motor, p),
      " THEN 1 ELSE 0 END AS guarda FROM lupa_t CROSS JOIN lupa_b"
    )
    r <- tryCatch(.traer_consulta_dbi(conexion, sql), error = function(e) e)
    caso <- paste0("pliegue_ascii", if (length(perfiles) > 1L) paste0("_", k))
    if (inherits(r, "error")) {
      filas <- c(filas, fila(list(
        conformidad = "no_evaluada",
        motivo = paste0("la bater\u00eda no pudo evaluar el caso: ", .mensaje_motor(r))
      ), caso))
      next
    }
    fuera <- as.numeric(r$guarda) == 0
    en_r <- .normalizacion_aplicar(as.character(r$x), p) ==
      .normalizacion_aplicar(as.character(r$y), p)
    en_motor <- as.numeric(r$igual) == 1
    coincide <- all(en_r[fuera] == en_motor[fuera])
    filas <- c(filas, fila(list(
      n_motor = sum(en_motor[fuera]), n_r = sum(en_r[fuera]),
      den_motor = sum(fuera), den_r = sum(fuera), coincide = coincide,
      conformidad = if (coincide) "conforme" else "no_conforme",
      motivo = if (!coincide) paste0(
        "el motor pliega el texto ASCII distinto que la normalizaci\u00f3n de R en ",
        sum(en_r[fuera] != en_motor[fuera]), " de ", sum(fuera), " pares"
      )
    ), caso))
  }
  filas
}

.fila_conformidad <- function(instancia, nodo, motor, senas, caso, esquema,
                              condicion, r) {
  numero <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)
  data.frame(
    motor = motor,
    version = as.character(senas$version %||% NA_character_),
    intercalacion = as.character(senas$intercalacion %||% NA_character_),
    metrica_instanciada = instancia$nombre,
    metrica = as.character(instancia$declaracion$nombre),
    regla = as.character(nodo %||% NA_character_),
    caso = caso,
    esquema = esquema,
    condicion = condicion,
    n_motor = numero(r$n_motor),
    n_r = numero(r$n_r),
    denominador_motor = numero(r$den_motor),
    denominador_r = numero(r$den_r),
    coincide = if (is.null(r$coincide)) NA else as.logical(r$coincide),
    conformidad = r$conformidad,
    motivo = as.character(r$motivo %||% NA_character_),
    accion = as.character(r$accion %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

.conformidad_vacia <- function() {
  data.frame(
    motor = character(), version = character(), intercalacion = character(),
    metrica_instanciada = character(), metrica = character(),
    regla = character(), caso = character(), esquema = character(),
    condicion = character(), n_motor = numeric(), n_r = numeric(),
    denominador_motor = numeric(), denominador_r = numeric(),
    coincide = logical(), conformidad = character(), motivo = character(),
    accion = character(), stringsAsFactors = FALSE
  )
}

.correr_variante_conformidad <- function(conexion, motor, senas, spec,
                                         instancia, columnas, variante, reales,
                                         aplicabilidad, max_valores_in,
                                         no_ascii, caso) {
  esquemas_base <- stats::setNames(lapply(names(columnas), function(e) {
    .esquema_caso_conformidad(reales[[e]], motor, variante$clases[[e]], senas)
  }), names(columnas))
  supuestos <- unlist(lapply(esquemas_base, `[[`, "supuesto"), use.names = FALSE)
  esquema <- if (!any(supuestos)) "tabla" else if (all(supuestos)) {
    "supuesto"
  } else "parcial"
  fila <- function(r, caso_fila, condicion) {
    r$accion <- .accion_conformidad(r$conformidad, r$causa, instancia, spec)
    .fila_conformidad(instancia, spec$nodo, motor, senas, caso_fila, esquema,
                      condicion, r)
  }
  datos <- .datos_caso_conformidad(
    spec, columnas, variante$clases, variante$constantes, no_ascii
  )
  if (is.null(datos)) {
    clases <- unique(unlist(variante$clases, use.names = FALSE))
    return(list(fila(list(
      conformidad = "no_evaluada",
      motivo = paste0("la bater\u00eda no genera valores de clase ",
                      paste(clases, collapse = ", "))
    ), caso, "siempre")))
  }
  r <- .caso_conformidad(conexion, motor, spec, instancia, esquemas_base, datos,
                         aplicabilidad, max_valores_in)
  # Lo que falla solo con NULL se separa: sin NULL la regla se cuenta otra vez,
  # y la fila con NULL queda condicionada a que la tabla real los tenga.
  repetir <- identical(r$conformidad, "no_conforme") ||
    identical(r$causa, "nulos")
  con_nulos <- any(vapply(r$datos, function(t) any(.filas_con_nulos_conformidad(t)),
                          logical(1L)))
  if (!repetir || !con_nulos) return(list(fila(r, caso, "siempre")))
  sin_nulos <- lapply(r$datos, function(t) {
    t[!.filas_con_nulos_conformidad(t), , drop = FALSE]
  })
  if (any(vapply(sin_nulos, nrow, integer(1L)) == 0L)) {
    return(list(fila(r, caso, "siempre")))
  }
  r2 <- .caso_conformidad(conexion, motor, spec, instancia, esquemas_base,
                          sin_nulos, aplicabilidad, max_valores_in)
  list(fila(r, caso, "con_nulos"),
       fila(r2, paste0(caso, "_sin_nulos"), "siempre"))
}

.casos_instancia_conformidad <- function(conexion, instancia, motor, senas,
                                         reales, aplicabilidad, max_valores_in) {
  spec <- .especificacion_metrica(instancia)
  if (.es_metrica_referencial_dbi(instancia)) {
    return(.casos_referencial_conformidad(conexion, instancia, motor, senas))
  }
  if (is.null(spec) || identical(spec$nodo, "abstencion")) {
    r <- list(
      conformidad = "abstencion", causa = "sin_especificacion",
      motivo = paste0(
        "la m\u00e9trica no tiene una especificaci\u00f3n SQL: medir_dbi() ",
        "no la mide en el motor"
      )
    )
    r$accion <- .accion_conformidad("abstencion", r$causa, instancia, spec)
    return(list(.fila_conformidad(
      instancia, spec$nodo %||% NA_character_, motor, senas, "regla",
      NA_character_, "siempre", r
    )))
  }
  columnas <- .columnas_caso_conformidad(spec, instancia, aplicabilidad, reales)
  variantes <- .variantes_caso_conformidad(
    spec, instancia, columnas, reales, aplicabilidad
  )
  filas <- list()
  for (variante in variantes) {
    hay_texto <- any(unlist(variante$clases, use.names = FALSE) == "texto")
    for (no_ascii in if (hay_texto) c(FALSE, TRUE) else FALSE) {
      caso <- paste0("limites_", variante$etiqueta, if (no_ascii) "_no_ascii")
      nuevas <- tryCatch(
        .correr_variante_conformidad(
          conexion, motor, senas, spec, instancia, columnas, variante, reales,
          aplicabilidad, max_valores_in, no_ascii, caso
        ),
        error = function(e) {
          r <- list(
            conformidad = "no_evaluada",
            motivo = paste0("la bater\u00eda no pudo evaluar el caso: ",
                            conditionMessage(e))
          )
          r$accion <- .accion_conformidad("no_evaluada", NA, instancia, spec)
          list(.fila_conformidad(instancia, spec$nodo, motor, senas, caso,
                                 NA_character_, "siempre", r))
        }
      )
      filas <- c(filas, nuevas)
    }
  }
  filas
}

.casos_conformidad_dbi <- function(conexion, modelo, motor, senas, reales,
                                   aplicabilidad, max_valores_in) {
  filas <- list()
  for (instancia in modelo$metricas) {
    nuevas <- tryCatch(
      .casos_instancia_conformidad(
        conexion, instancia, motor, senas, reales, aplicabilidad, max_valores_in
      ),
      error = function(e) {
        nodo <- tryCatch(.especificacion_metrica(instancia)$nodo,
                         error = function(err) NULL)
        r <- list(
          conformidad = "no_evaluada",
          motivo = paste0("la bater\u00eda no pudo evaluar la regla: ",
                          conditionMessage(e)),
          accion = "medir_dbi() mide la regla como siempre y avisa"
        )
        list(.fila_conformidad(instancia, nodo %||% NA_character_, motor,
                               senas, "regla", NA_character_, "siempre", r))
      }
    )
    filas <- c(filas, nuevas)
  }
  if (!length(filas)) return(NULL)
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------------
# Uso en `medir_dbi(conformidad =)`
# ---------------------------------------------------------------------------

.validar_conformidad_medir_dbi <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  completa <- inherits(x, "conformidad_dbi") && inherits(x, "data.frame") &&
    all(.COLUMNAS_CONFORMIDAD_DBI %in% names(x)) &&
    all(vapply(.ATRIBUTOS_CONFORMIDAD_DBI, function(a) {
      !is.null(attr(x, a, exact = TRUE))
    }, logical(1L)))
  if (!completa) {
    .detener(
      "`conformidad` debe ser la tabla que devuelve conformidad_dbi(), con ",
      "su clase y sus atributos.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.avisar_conformidad_dbi <- function(mensaje, reglas) {
  .avisar_dbi(
    "lupa_aviso_conformidad_dbi",
    paste0(mensaje, ": ", paste(reglas, collapse = ", "),
           ". Se miden como siempre."),
    datos = list(reglas = reglas)
  )
}

.aplicar_conformidad_plan_dbi <- function(planes, conformidad, conexion, modelo,
                                          tablas, aplicabilidad,
                                          max_valores_in, motor,
                                          estrategia_no_sql = "no_medible") {
  if (is.null(conformidad)) return(planes)
  .validar_conformidad_medir_dbi(conformidad)
  actual <- .procedencia_conformidad_dbi(
    conexion, modelo, tablas, aplicabilidad, max_valores_in, motor
  )
  identidad <- attr(conformidad, "identidad", exact = TRUE)
  distinto <- character()
  for (campo in c("motor", "version", "intercalacion", "servidor")) {
    if (!identical(as.character(identidad[[campo]]),
                   as.character(actual$identidad[[campo]]))) {
      distinto <- c(distinto, campo)
    }
  }
  # Otro servidor tiene su propia version e intercalacion: lo que difiere es el
  # servidor. En el mismo servidor, una version o una intercalacion distinta
  # dice que cambio desde que se armo la bateria.
  if ("servidor" %in% distinto) {
    distinto <- setdiff(distinto, c("version", "intercalacion"))
  }
  if (!identical(attr(conformidad, "configuracion", exact = TRUE),
                 actual$configuracion)) {
    distinto <- c(distinto, "configuraci\u00f3n")
  }
  if (length(distinto)) {
    .avisar_dbi(
      "lupa_aviso_conformidad_dbi",
      paste0(
        "La bater\u00eda de conformidad no es de esta conexi\u00f3n o de ",
        "esta configuraci\u00f3n (difiere: ", paste(distinto, collapse = ", "),
        "): no se aplica y las reglas se miden como siempre."
      ),
      datos = list(difiere = distinto)
    )
    return(planes)
  }
  reglas <- attr(conformidad, "reglas", exact = TRUE)
  estados <- as.character(conformidad$conformidad)
  condiciones <- as.character(conformidad$condicion)
  nombres_filas <- as.character(conformidad$metrica_instanciada)
  sin_cubrir <- character()
  sin_verificar <- character()
  sin_evaluar <- character()
  motivo_de <- function(filas) {
    m <- unique(as.character(conformidad$motivo[filas]))
    m <- m[!is.na(m) & nzchar(m)]
    paste0(
      "la bater\u00eda de conformidad encontr\u00f3 que el motor cuenta ",
      "distinto que R",
      if (length(m)) paste0(": ", paste(m, collapse = "; ")) else ""
    )
  }
  for (i in seq_along(planes)) {
    plan <- planes[[i]]
    # Lo que `medir_dbi()` no cuenta en el motor no depende de la bateria.
    if (!plan$via %in% .VIAS_SQL_CONFORMIDAD) next
    nombre <- plan$instancia$nombre
    filas <- which(nombres_filas == nombre)
    huella <- if (nombre %in% names(reglas)) reglas[[nombre]] else NA_character_
    if (!length(filas) || !identical(huella, actual$reglas[[nombre]])) {
      sin_cubrir <- c(sin_cubrir, nombre)
      next
    }
    if (any(estados[filas] == "no_verificado")) {
      sin_verificar <- c(sin_verificar, nombre)
    }
    if (any(estados[filas] == "no_evaluada")) {
      sin_evaluar <- c(sin_evaluar, nombre)
    }
    no_conforme <- filas[estados[filas] == "no_conforme"]
    siempre <- no_conforme[condiciones[no_conforme] != "con_nulos"]
    con_nulos <- no_conforme[condiciones[no_conforme] == "con_nulos"]
    if (length(siempre)) {
      # Lo que el motor cuenta distinto se lee por bloques en R si se pidio,
      # por la misma via que lo que no se traduce; si no, queda no_medible.
      plan$via <- if (isTRUE(plan$referencial_motor) &&
                      identical(estrategia_no_sql, "bloques")) {
        # El referencial del motor se compara en R sobre los valores distintos.
        "distintos_r"
      } else if (.admite_bloques_medicion_dbi(
        estrategia_no_sql, plan$instancia, plan$spec, plan$columnas_bloque
      )) "bloques_r" else "no_medible"
      plan$motivo <- motivo_de(siempre)
    } else if (length(con_nulos)) {
      plan[["conformidad_nulos"]] <- paste0(
        motivo_de(con_nulos), " (con NULL en las columnas de la regla)"
      )
    }
    planes[[i]] <- plan
  }
  if (length(sin_cubrir)) {
    .avisar_conformidad_dbi(
      paste0(
        "La bater\u00eda de conformidad no cubre estas reglas (no est\u00e1n ",
        "en ella o cambi\u00f3 su definici\u00f3n)"
      ),
      sin_cubrir
    )
  }
  if (length(sin_verificar)) {
    .avisar_conformidad_dbi(
      "La bater\u00eda no verific\u00f3 el motor de estas reglas",
      sin_verificar
    )
  }
  if (length(sin_evaluar)) {
    .avisar_conformidad_dbi(
      "La bater\u00eda no pudo evaluar alg\u00fan caso de estas reglas",
      sin_evaluar
    )
  }
  planes
}

#' Comprobar la conformidad entre R y un motor DBI
#'
#' Ejecuta, regla por regla, una batería de valores límite inventados en
#' consultas `SELECT` y compara lo que cuenta el motor con lo que cuenta R con la
#' misma regla. No crea tablas ni lee filas del usuario: de cada tabla del modelo
#' lee sólo el esquema del catálogo, para escribir los valores inventados con el
#' tipo y la intercalación de cada columna real. Si la tabla no existe, usa los
#' tipos que se desprenden de los valores declarados en la regla.
#'
#' Corre en SQLite, DuckDB, PostgreSQL (también 9.3), MySQL, MariaDB y SQL
#' Server. En un servidor cada valor inventado se escribe con `CAST` al tipo de
#' la columna —en MySQL y MariaDB, al destino de `CAST()` que corresponde a ese
#' tipo, con el juego de caracteres de su intercalación— y con su cláusula
#' `COLLATE`; una columna que la tabla no tiene es la que el servidor crearía
#' para esa clase, con la intercalación de la base. Si el motor no tiene cómo
#' escribir un literal con el tipo de la columna (`YEAR` en MariaDB,
#' `VARBINARY` en SQL Server), el caso queda `no_evaluada` con ese motivo.
#'
#' La batería responde una sola pregunta: si el motor, cuando cuenta, da lo
#' mismo que R. No conformar es eso y nada más. Que `medir_dbi()` se abstenga
#' por una guarda propia con los valores inventados (texto no ASCII en SQLite,
#' una regla que da `NULL`, un cotejamiento que no es binario) queda registrado
#' como `abstencion`: con los datos reales decide la misma guarda, como siempre.
#'
#' @param conexion Conexión DBI activa que se quiere caracterizar, o una
#'   colección, igual que en [medir_dbi()].
#' @param modelo Modelo creado por [modelo()] con las reglas que se desean
#'   comprobar.
#' @param tablas Tabla física de cada entidad, igual que en [medir_dbi()]. Se
#'   usa sólo para leer el esquema de las columnas.
#' @param aplicabilidad Lista de fórmulas de aplicabilidad, igual que en
#'   [medir_dbi()].
#' @param max_valores_in Tope de valores en listas `IN (...)`.
#' @param dialecto Dialecto SQL: `"auto"`, `"sqlite"`, `"duckdb"`,
#'   `"postgresql"`, `"mysql"`, `"mariadb"` o `"sqlserver"`.
#'
#' @return Un `data.frame` de clase `conformidad_dbi`, con una fila por regla y
#'   caso. La columna `conformidad` dice si el motor cuenta igual que R, no si
#'   la regla se puede medir:
#'   * `conforme`: con estos valores el motor y R dan el mismo numerador y el
#'     mismo denominador. No asegura que `medir_dbi()` mida la regla: con los
#'     datos reales puede abstenerse por su esquema o por una guarda.
#'   * `no_conforme`: el motor cuenta y da distinto que R. Es lo único que hace
#'     que `medir_dbi(conformidad =)` deje de contar la regla en el motor: la
#'     mide por bloques en R con `estrategia_no_sql = "bloques"`, o la declara
#'     `no_medible`. El límite es conservador: una diferencia que sólo aparece
#'     con un valor inventado que no es `NULL` (un entero cerca de 2^53, un texto
#'     con espacios) quita la regla del motor aunque la tabla real no tenga ese
#'     valor.
#'   * `abstencion`: `medir_dbi()` no publicaría cifra con estos valores, por su
#'     traductor o por una guarda; `motivo` dice cuál. No quita la regla.
#'   * `no_evaluada`: la batería no pudo construir o contar el caso; la regla se
#'     mide como siempre, con un aviso.
#'   * `no_verificado`: el motor no es ninguno de los verificados en la batería
#'     (los seis de arriba); la regla se mide como siempre, con un aviso.
#'
#'   `condicion` es `"con_nulos"` en un caso que con los `NULL` inventados no
#'   conformó o se abstuvo: el mismo caso sin `NULL` va en la fila
#'   `*_sin_nulos`, y `medir_dbi()` aplica una no conformidad con `NULL` sólo si
#'   la tabla real tiene `NULL` en las columnas de la regla. Una constante de
#'   texto de la regla entra también con otra caja y con espacios, en un caso
#'   sólo ASCII (`limites_texto`) y en otro con acentos
#'   (`limites_texto_no_ascii`). `esquema` dice de dónde salieron los tipos de
#'   las columnas: `"tabla"` (el catálogo), `"supuesto"` (los valores de la
#'   regla) o `"parcial"`. Los atributos `identidad`, `configuracion` y `reglas`
#'   son huellas de la conexión, de la configuración y de la definición de cada
#'   regla, sin datos personales; `medir_dbi()` los usa para no aplicar una
#'   batería ajena. La huella de la conexión identifica el servidor y la base
#'   por lo que el motor dice de sí mismo, no por cómo se abrió la conexión: el
#'   archivo que SQLite o DuckDB tienen abierto, con la ruta normalizada; en
#'   PostgreSQL el identificador del sistema y la base; en MySQL y MariaDB
#'   `@@hostname`, `@@port` y la base; en SQL Server `@@SERVERNAME`, la máquina
#'   y la base. Se pregunta con una sentencia, una vez por llamada. Dos bases
#'   en memoria no se distinguen. El atributo `intercalacion` es la que el motor usa por
#'   omisión (`NA` con su motivo en `intercalacion_motivo` si no se pudo leer):
#'   `BINARY` en SQLite, `default_collation` en DuckDB, la de la base en
#'   PostgreSQL (`datcollate`) y en SQL Server, y `@@collation_database` en
#'   MySQL y MariaDB. `version` es la que el motor informa. Si la batería es de
#'   otro servidor, `medir_dbi()` dice que difiere el servidor; una versión o una
#'   intercalación distinta en el mismo servidor, que cambió desde que se armó.
#' @export
#' @seealso [medir_dbi()], [plan_medicion_dbi()]
conformidad_dbi <- function(conexion, modelo, tablas = NULL,
                            aplicabilidad = NULL, max_valores_in = 500L,
                            dialecto = "auto") {
  max_valores_in <- .validar_argumentos_conformidad_dbi(
    conexion, modelo, max_valores_in
  )
  .fijar_numerica_c()
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  .con_corrida_dbi(con, "verificar la conformidad", {
  motor <- .motor_medicion_dbi(con, dialecto)
  procedencia <- .procedencia_conformidad_dbi(
    conexion, modelo, tablas, aplicabilidad, max_valores_in, motor
  )
  senas <- procedencia$senas
  if (!motor %in% .MOTORES_VERIFICADOS_CONFORMIDAD_DBI) {
    filas <- lapply(modelo$metricas, function(instancia) {
      spec <- .especificacion_metrica(instancia)
      r <- list(
        conformidad = "no_verificado",
        motivo = paste0("el motor `", motor,
                        "` no est\u00e1 verificado en la bater\u00eda")
      )
      r$accion <- .accion_conformidad("no_verificado", NA, instancia, spec)
      .fila_conformidad(instancia, spec$nodo %||% NA_character_, motor, senas,
                        "servidor", NA_character_, "siempre", r)
    })
    salida <- if (length(filas)) do.call(rbind, filas) else NULL
  } else {
    sesion <- .sesion_conformidad_dbi(procedencia$con, motor)
    if (motor %in% .SERVIDORES_CONFORMIDAD_DBI) {
      # La columna supuesta de un servidor se traduce con la sesion real y con
      # el relleno de la intercalacion de la base.
      senas$sesion <- sesion
      senas$intercalacion_no_pad <- .no_pad_por_omision_conformidad_dbi(
        procedencia$con, motor, senas
      )
    }
    reales <- .esquemas_reales_conformidad_dbi(
      procedencia$con, procedencia$fisicas, motor, sesion
    )
    salida <- .casos_conformidad_dbi(
      procedencia$con, modelo, motor, senas, reales, aplicabilidad,
      max_valores_in
    )
  }
  if (is.null(salida)) salida <- .conformidad_vacia()
  rownames(salida) <- NULL
  class(salida) <- c("conformidad_dbi", "data.frame")
  attr(salida, "motor") <- motor
  attr(salida, "version") <- senas$version
  attr(salida, "intercalacion") <- senas$intercalacion
  attr(salida, "intercalacion_motivo") <- senas$intercalacion_motivo
  attr(salida, "motores_no_verificados") <- setdiff(
    .MOTORES_MEDICION_DBI, .MOTORES_VERIFICADOS_CONFORMIDAD_DBI
  )
  attr(salida, "identidad") <- procedencia$identidad
  attr(salida, "configuracion") <- procedencia$configuracion
  attr(salida, "reglas") <- procedencia$reglas
  salida
  })
}
