# Medicion en motores DBI: `plan_medicion_dbi()` y `medir_dbi()`.
#
# Traduce a SQL las metricas con especificacion (`.especificacion_metrica()`)
# reproduciendo la semantica de R. Lo que un motor no puede garantizar queda
# declarado como `no_medible` con su motivo o, si es separable y se pide
# `estrategia_no_sql = "bloques"`, se evalua en R leyendo por bloques solo las
# columnas necesarias.

# ---------------------------------------------------------------------------
# 1. Politica motor por regla
# ---------------------------------------------------------------------------

.MOTORES_MEDICION_DBI <- c(
  "sqlite", "duckdb", "postgresql", "mysql", "mariadb", "sqlserver"
)

.POLITICA_MOTORES_MEDICION <- local({
  f <- function(regla, sqlite, duckdb, postgresql, mysql, sqlserver) {
    data.frame(
      regla = regla,
      motor = c("sqlite", "duckdb", "postgresql", "mysql", "mariadb", "sqlserver"),
      politica = c(sqlite, duckdb, postgresql, mysql, mysql, sqlserver),
      stringsAsFactors = FALSE
    )
  }
  rbind(
    f("no_ausente sin centinela (toda clase)",
      "sql",
      "sql (NaN -> NULL)",
      "sql (NaN -> NULL; declarado)",
      "sql (declarado)",
      "sql: COUNT_BIG y SUM(CAST(... AS BIGINT)) (declarado)"),
    f("no_ausente con centinela numerico o de fecha",
      "sql; fecha solo con tipos extendidos (si no, R lee un numero y la clase no coincide)",
      "sql",
      "sql; instante solo sobre timestamp o con la sesion en UTC (verificacion SHOW TimeZone) (declarado)",
      "sql; instante solo sobre DATETIME o con time_zone '+00:00' (verificacion de sesion) (declarado)",
      "sql; instante solo sobre DATETIME2; DATETIMEOFFSET no_medible (declarado)"),
    f("no_ausente con centinela de texto ASCII",
      "sql: TRIM de espacio, tab, LF, CR + REPLACE(U+0130, 'i') + LOWER, solo si el valor es ASCII",
      "sql: TRIM con chr() + REPLACE(U+0130, 'i') + LOWER, solo si el valor es ASCII",
      "sql_con_verificacion: lower('I') = 'i' y BTRIM con espacio/tab/LF/CR; char(n) no_medible (declarado)",
      "sql_con_verificacion: colacion de la columna con PAD_ATTRIBUTE = 'NO PAD'; REPLACE de tab antes de TRIM (declarado)",
      "no_medible: los espacios finales se ignoran en = e IN y el plegado depende de la colacion"),
    f("no_ausente con centinela no ASCII",
      "no_medible: LOWER de SQLite no pliega fuera de ASCII",
      "no_medible: LOWER(U+212A) = 'k' en DuckDB y no en el paquete",
      "no_medible", "no_medible", "no_medible"),
    f("en_conjunto / Formato(diccionario) sobre numero, fecha o logico",
      "sql (logico: R lee entero; la clase no coincide con TRUE/FALSE y declina)",
      "sql",
      "sql (declarado)",
      "sql: literal de la misma clase, nunca numero contra texto (declarado)",
      "sql (declarado)"),
    f("en_conjunto / Formato(diccionario) sobre texto",
      "sql_con_guarda: texto no ASCII = 0 (SQLite no valida UTF-8 y R lee el byte invalido como NA); colacion BINARY",
      "sql: colacion por omision binaria",
      "sql_con_verificacion: colacion determinista; char(n)/bpchar no_medible (declarado)",
      "sql_con_guarda: colacion binaria NO PAD, o guarda de espacios finales = 0 (declarado)",
      "sql_con_guarda: colacion _BIN2 y guarda de espacios finales = 0 en columna y dominio (declarado)"),
    f("en_rango sobre numero, fecha o instante",
      "sql (fecha e instante solo con tipos extendidos)",
      "sql",
      "sql; timestamptz con verificacion de huso (declarado)",
      "sql; TIMESTAMP con verificacion de huso (declarado)",
      "sql sobre DATETIME2; DATETIMEOFFSET no_medible (declarado)"),
    f("en_rango sobre texto",
      "no_medible", "no_medible", "no_medible", "no_medible", "no_medible"),
    f("fecha_en_rango (Oportunidad*)",
      "sql en segundos: col * 86400 o col (tipos extendidos); sin ellos R lee numero y las dos vias se abstienen",
      "sql en segundos: epoch()",
      "sql: extract(epoch from ...) sobre timestamp; timestamptz con verificacion de huso (declarado)",
      "sql: UNIX_TIMESTAMP sobre DATETIME con verificacion de huso (declarado)",
      "sql: DATEDIFF_BIG(second, '1970-01-01', col) sobre DATETIME2 (declarado)"),
    f("duplicado sobre numero, fecha o instante",
      "sql (GROUP BY)",
      "sql (GROUP BY; NaN agrupa aparte de NULL, como R)",
      "sql (declarado)", "sql (declarado)", "sql (declarado)"),
    f("duplicado sobre texto",
      "sql_con_guarda: texto no ASCII = 0 (AtributoDuplicado); ConjuntoAtributosDuplicado compara bytes en las dos vias",
      "sql",
      "sql_con_verificacion: colacion determinista; bpchar no_medible (declarado)",
      "sql_con_guarda: NO PAD binaria o espacios finales = 0 (declarado)",
      "sql_con_guarda: _BIN2 y espacios finales = 0 (declarado)"),
    f("predicado de fila con numeros y fechas (==, !=, <, <=, >, >=, is.na, %in%, !, &, |)",
      "sql", "sql",
      "sql (declarado)", "sql (declarado)",
      "no_medible: T-SQL no tiene valores logicos; falta componer la logica de tres valores con CASE"),
    f("predicado de fila con texto (==, !=, %in%, is.na)",
      "sql (colacion BINARY)",
      "sql (colacion binaria)",
      "sql_con_verificacion: colacion determinista (declarado)",
      "sql_con_guarda: NO PAD o espacios finales = 0 (declarado)",
      "no_medible: T-SQL"),
    f("predicado de fila con orden sobre texto (<, <=, >, >=)",
      "no_medible: R ordena con el locale de la sesion",
      "no_medible: R ordena con el locale de la sesion",
      "no_medible", "no_medible", "no_medible"),
    f("predicado de fila con columna logica",
      "no_medible: SQLite guarda el logico como entero",
      "sql",
      "sql (declarado)",
      "no_medible: MySQL/MariaDB guarda BOOLEAN como TINYINT(1) y R lee entero",
      "no_medible: T-SQL BIT y sin tipo logico"),
    f("predicado de fila con %in% que incluye NA sobre doble con NaN",
      "sql (SQLite no guarda NaN)",
      "no_medible: R no iguala NaN con NA",
      "no_medible: PostgreSQL guarda NaN y R no lo iguala con NA",
      "sql (MySQL no guarda NaN; declarado)",
      "no_medible: T-SQL"),
    f("cobertura_referencial (misma clase; o texto contra entero)",
      "sql_con_guarda: texto no ASCII = 0",
      "sql",
      "sql_con_verificacion: colacion determinista (declarado)",
      "sql_con_guarda: NO PAD o espacios finales = 0 (declarado)",
      "sql_con_guarda: _BIN2 y espacios finales = 0 (declarado)"),
    f("cobertura_referencial (doble contra texto, integer64 contra doble, fecha contra instante)",
      "no_medible", "no_medible", "no_medible", "no_medible", "no_medible")
  )
})

.regla_politica_nodo <- function(nodo, clases) {
  es_texto <- any(clases == "texto")
  switch(
    nodo,
    no_ausente = "no_ausente sin centinela (toda clase)",
    en_conjunto = if (es_texto) {
      "en_conjunto / Formato(diccionario) sobre texto"
    } else {
      "en_conjunto / Formato(diccionario) sobre numero, fecha o logico"
    },
    en_rango = if (es_texto) {
      "en_rango sobre texto"
    } else {
      "en_rango sobre numero, fecha o instante"
    },
    fecha_en_rango = "fecha_en_rango (Oportunidad*)",
    duplicado = if (es_texto) {
      "duplicado sobre texto"
    } else {
      "duplicado sobre numero, fecha o instante"
    },
    predicado_fila = if (es_texto) {
      "predicado de fila con texto (==, !=, %in%, is.na)"
    } else {
      "predicado de fila con numeros y fechas (==, !=, <, <=, >, >=, is.na, %in%, !, &, |)"
    },
    cobertura_referencial = "cobertura_referencial (misma clase; o texto contra entero)",
    NA_character_
  )
}

.precondicion_motor_medicion <- function(motor, nodo, clases) {
  if (motor %in% c("sqlite", "duckdb")) return(NULL)
  if (motor %in% c("postgresql", "mysql", "mariadb", "sqlserver")) {
    regla <- .regla_politica_nodo(nodo, clases)
    fila <- .POLITICA_MOTORES_MEDICION[
      .POLITICA_MOTORES_MEDICION$motor == motor &
        .POLITICA_MOTORES_MEDICION$regla == regla, , drop = FALSE
    ]
    detalle <- if (nrow(fila)) paste0(" (", fila$politica[[1L]], ")") else ""
    return(paste0(
      "la traducci\u00f3n a SQL para `", motor,
      "` no est\u00e1 habilitada sin verificaci\u00f3n de equivalencia en ese motor",
      detalle
    ))
  }
  paste0(
    "motor no reconocido (`", motor,
    "`): no se puede garantizar la equivalencia con R"
  )
}

# ---------------------------------------------------------------------------
# 2. Motor, conexion, resolucion de tablas y esquema sin leer filas
# ---------------------------------------------------------------------------

.motor_medicion_dbi <- function(conexion, dialecto = "auto") {
  if (!.es_texto_escalar(dialecto)) {
    .detener(
      "`dialecto` debe ser `\"auto\"` o uno de: ",
      paste(.MOTORES_MEDICION_DBI, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!identical(dialecto, "auto")) {
    d <- .minusculas_ascii(dialecto)
    if (!d %in% .MOTORES_MEDICION_DBI) {
      .detener(
        "`dialecto` no reconocido: `", dialecto, "`. Valores admitidos: `\"auto\"`, ",
        paste(.MOTORES_MEDICION_DBI, collapse = ", "), ".",
        call. = FALSE
      )
    }
    return(d)
  }
  if (inherits(conexion, "SQLiteConnection")) return("sqlite")
  if (inherits(conexion, "duckdb_connection")) return("duckdb")
  senas <- .senas_conexion_dbi(conexion)
  if (grepl("postgres|pqconnection", senas, useBytes = TRUE)) return("postgresql")
  if (grepl("mariadb", senas, useBytes = TRUE)) return("mariadb")
  if (grepl("mysql", senas, useBytes = TRUE)) return("mysql")
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas, useBytes = TRUE)) {
    return("sqlserver")
  }
  "otro"
}

.validar_argumentos_entrada_medir_dbi <- function(modelo, conexion, tablas,
                                                  max_valores_in, max_consultas,
                                                  tamano_lote, bloque_filas) {
  if (!inherits(modelo, "modelo_calidad")) {
    if (inherits(conexion, "modelo_calidad") ||
        inherits(modelo, "DBIConnection") ||
        inherits(modelo, "coleccion_lupa")) {
      .detener(
        "El primer argumento es `modelo` (creado por modelo()) y el segundo es ",
        "`conexion` (conexi\u00f3n DBI o coleccion()), en el mismo orden que ",
        "medir(modelo, datos).",
        call. = FALSE
      )
    }
    .detener("`modelo` debe provenir de modelo().", call. = FALSE)
  }
  if (!inherits(conexion, "coleccion_lupa") && !inherits(conexion, "DBIConnection")) {
    .detener(
      "`conexion` debe ser una conexi\u00f3n DBI activa o un objeto de coleccion().",
      call. = FALSE
    )
  }
  if (!is.numeric(max_valores_in) || length(max_valores_in) != 1L ||
      is.na(max_valores_in) || !is.finite(max_valores_in) ||
      max_valores_in < 1 || max_valores_in != floor(max_valores_in)) {
    .detener("`max_valores_in` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.numeric(max_consultas) || length(max_consultas) != 1L ||
      is.na(max_consultas) || max_consultas < 1) {
    .detener("`max_consultas` debe ser un n\u00famero mayor o igual a 1 o Inf.",
             call. = FALSE)
  }
  if (!is.null(tamano_lote)) {
    if (!is.numeric(tamano_lote) || length(tamano_lote) != 1L ||
        is.na(tamano_lote) || !is.finite(tamano_lote) ||
        tamano_lote < 1 || tamano_lote != floor(tamano_lote)) {
      .detener("`tamano_lote` debe ser NULL o un entero positivo.", call. = FALSE)
    }
  }
  if (!is.null(bloque_filas)) {
    bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  } else {
    bloque_filas <- 10000L
  }
  list(
    max_valores_in = as.integer(max_valores_in),
    max_consultas = as.numeric(max_consultas),
    tamano_lote = if (is.null(tamano_lote)) NULL else as.integer(tamano_lote),
    bloque_filas = as.integer(bloque_filas)
  )
}

.resolver_tablas_medir_dbi <- function(modelo, conexion, tablas) {
  entidades <- unique(unlist(lapply(modelo$metricas, `[[`, "entidad")))
  con <- conexion
  mapa <- stats::setNames(as.list(entidades), entidades)
  if (inherits(conexion, "coleccion_lupa")) {
    con <- conexion$conexion
    col_tablas <- .completar_columnas_coleccion(conexion$tablas)
    for (e in entidades) {
      i <- match(e, as.character(col_tablas$identificador))
      if (is.na(i)) i <- match(e, as.character(col_tablas$tabla))
      if (!is.na(i)) mapa[[e]] <- col_tablas$referencia[[i]]
    }
  }
  validar_entidades <- function(nombres) {
    desconocidas <- setdiff(as.character(nombres), entidades)
    if (length(desconocidas)) {
      .detener(
        "`tablas` nombra la entidad `", desconocidas[[1L]],
        "`, pero no existe en el modelo. Entidades del modelo: ",
        paste(entidades, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(tablas)) {
    if (inherits(tablas, "Id")) {
      if (length(entidades) != 1L) {
        .detener(
          "`tablas` debe ser un vector o lista con nombre por entidad cuando ",
          "el modelo declara varias entidades.",
          call. = FALSE
        )
      }
      mapa[[entidades[[1L]]]] <- .nombre_para_conexion_dbi(tablas)
    } else if (is.character(tablas)) {
      if (length(entidades) == 1L && is.null(names(tablas)) &&
          length(tablas) == 1L) {
        mapa[[entidades[[1L]]]] <- .nombre_para_conexion_dbi(tablas[[1L]])
      } else {
        if (is.null(names(tablas)) || any(!nzchar(names(tablas)))) {
          .detener(
            "`tablas` debe ser un vector o lista con nombre por entidad.",
            call. = FALSE
          )
        }
        validar_entidades(names(tablas))
        for (nm in names(tablas)) {
          mapa[[nm]] <- .nombre_para_conexion_dbi(tablas[[nm]])
        }
      }
    } else if (is.list(tablas)) {
      if (length(entidades) == 1L && is.null(names(tablas)) &&
          length(tablas) == 1L) {
        mapa[[entidades[[1L]]]] <- .nombre_para_conexion_dbi(tablas[[1L]])
      } else {
        if (is.null(names(tablas)) || any(!nzchar(names(tablas)))) {
          .detener(
            "`tablas` debe ser un vector o lista con nombre por entidad.",
            call. = FALSE
          )
        }
        validar_entidades(names(tablas))
        for (nm in names(tablas)) {
          mapa[[nm]] <- .nombre_para_conexion_dbi(tablas[[nm]])
        }
      }
    } else {
      .detener(
        "`tablas` debe ser NULL, un vector de texto, una lista con nombre o un DBI::Id.",
        call. = FALSE
      )
    }
  }
  list(conexion = con, entidades = entidades, fisicas = mapa)
}

.clase_r_columna_dbi <- function(x) {
  if (inherits(x, "integer64")) return("entero64")
  if (inherits(x, "Date")) return("fecha")
  if (inherits(x, "POSIXt")) return("instante")
  if (is.factor(x) || is.character(x)) return("texto")
  if (is.logical(x)) return("logico")
  if (is.integer(x)) return("entero")
  if (is.double(x)) return("doble")
  "otro"
}

.esquema_medicion_dbi <- function(conexion, ref_tabla, motor, contador = NULL) {
  consultar <- function(sql) {
    if (is.null(contador)) {
      .traer_consulta_dbi(conexion, sql)
    } else {
      .consultar_medicion_dbi(
        conexion, sql, contador, registrar_sql = FALSE
      )
    }
  }
  q <- as.character(DBI::dbQuoteIdentifier(conexion, ref_tabla))
  nombre_cat <- if (inherits(ref_tabla, "Id")) {
    partes_id <- ref_tabla@name
    if ("table" %in% names(partes_id)) {
      as.character(partes_id[["table"]])
    } else {
      as.character(unname(partes_id)[[length(partes_id)]])
    }
  } else {
    .nombre_tabla_catalogo_dbi(ref_tabla)
  }
  esquema_cat <- if (inherits(ref_tabla, "Id") &&
                     "schema" %in% names(ref_tabla@name)) {
    as.character(ref_tabla@name[["schema"]])
  } else {
    NA_character_
  }
  proto <- consultar(paste0("SELECT * FROM ", q, " WHERE 1 = 0"))
  n_consultas <- 1L
  campos <- names(proto)
  clases <- vapply(proto, .clase_r_columna_dbi, character(1L))
  tipos <- stats::setNames(rep(NA_character_, length(campos)), campos)
  crear <- NA_character_
  if (identical(motor, "sqlite")) {
    info <- consultar(paste0("PRAGMA table_info(", q, ")"))
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      tipos[info$name] <- .mayusculas_ascii(info$type)
    }
    crear_df <- consultar(paste0(
      "SELECT sql FROM sqlite_master WHERE type IN ('table', 'view') AND name = ",
      DBI::dbQuoteString(conexion, nombre_cat)
    ))
    n_consultas <- n_consultas + 1L
    if (is.data.frame(crear_df) && nrow(crear_df)) {
      crear <- crear_df$sql[[1L]]
    }
    cotejamientos <- .cotejamientos_sql_tabla(crear, campos, "binary")
  } else if (identical(motor, "duckdb")) {
    filtro_esq <- if (!is.na(esquema_cat)) {
      paste0(" AND table_schema = ", DBI::dbQuoteString(conexion, esquema_cat))
    } else ""
    info <- consultar(paste0(
      "SELECT column_name, data_type FROM information_schema.columns ",
      "WHERE table_name = ", DBI::dbQuoteString(conexion, nombre_cat),
      filtro_esq
    ))
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      tipos[info$column_name] <- .mayusculas_ascii(info$data_type)
    }
    crear_df <- tryCatch(
      consultar(paste0(
        "SELECT sql FROM duckdb_tables() WHERE table_name = ",
        DBI::dbQuoteString(conexion, nombre_cat),
        if (!is.na(esquema_cat)) {
          paste0(" AND schema_name = ", DBI::dbQuoteString(conexion, esquema_cat))
        } else ""
      )),
      error = function(e) NULL
    )
    n_consultas <- n_consultas + 1L
    if (is.data.frame(crear_df) && nrow(crear_df)) {
      crear <- crear_df$sql[[1L]]
    }
    cotejamientos <- .cotejamientos_sql_tabla(crear, campos, "predeterminado")
    defecto <- tryCatch(
      consultar("SELECT current_setting('default_collation') AS c")$c,
      error = function(e) character()
    )
    n_consultas <- n_consultas + 1L
    if (length(defecto) && !is.na(defecto[[1L]]) && nzchar(defecto[[1L]])) {
      cotejamientos[is.na(cotejamientos) | cotejamientos == "predeterminado"] <-
        defecto[[1L]]
    }
  } else {
    cotejamientos <- stats::setNames(rep(NA_character_, length(campos)), campos)
  }
  cotejamientos[is.na(cotejamientos)] <- "desconocido"
  list(
    referencia = ref_tabla,
    nombre = nombre_cat,
    sql = q,
    campos = campos,
    clases = clases,
    tipos = tipos,
    cotejamientos = cotejamientos,
    n_campos = length(campos),
    prototipo = .normalizar_columnas_texto(proto),
    n_consultas_esquema = n_consultas
  )
}

.motivo_tabla_no_medible_dbi <- function(ref_tabla) {
  nombre <- .nombre_tabla_catalogo_dbi(ref_tabla)
  if (!length(nombre) || is.na(nombre) || !nzchar(nombre)) {
    nombre <- "desconocida"
  }
  paste0(
    "la tabla `", nombre,
    "` no est\u00e1 disponible en la conexi\u00f3n DBI; no se pudo leer su esquema"
  )
}

# ---------------------------------------------------------------------------
# 3. Traductor SQL de predicados de fila (AST cerrado, sin `eval()`)
# ---------------------------------------------------------------------------

.no_traducible_dbi <- function(...) {
  .detener(structure(
    class = c("lupa_no_traducible_dbi", "error", "condition"),
    list(message = .mensaje_condicion(...), call = NULL)
  ))
}

.cotejamiento_binario_dbi <- function(esq, columna, motor) {
  c <- .minusculas_ascii(esq$cotejamientos[[columna]])
  switch(
    motor,
    sqlite = c %in% c("binary"),
    duckdb = c %in% c("predeterminado", "binary", ""),
    FALSE
  )
}

.citar_col_dbi <- function(conexion, nombre) {
  as.character(DBI::dbQuoteIdentifier(conexion, nombre))
}

.columna_sql_medicion <- function(conexion, esq, columna, motor, alias = NULL,
                                  normalizar_nan = TRUE) {
  ref <- .citar_col_dbi(conexion, columna)
  if (!is.null(alias)) ref <- paste0(alias, ".", ref)
  tipo <- esq$tipos[[columna]]
  doble_declarado <- !is.na(tipo) && grepl(
    "^(DOUBLE|FLOAT|REAL|DECIMAL|NUMERIC|NUMBER)(\\(|$)",
    tipo, useBytes = TRUE
  ) || .tipo_declarado_entero_ancho_dbi(tipo)
  if (identical(esq$clases[[columna]], "doble") && doble_declarado &&
      motor %in% c("sqlite", "duckdb")) {
    ref <- paste0("CAST(", ref, " AS DOUBLE)")
  }
  if (normalizar_nan && identical(esq$clases[[columna]], "doble") &&
      motor %in% c("duckdb", "postgresql") &&
      !is.na(esq$tipos[[columna]]) &&
      grepl("DOUBLE|FLOAT|REAL|NUMERIC|DECIMAL", esq$tipos[[columna]],
            useBytes = TRUE)) {
    nan <- if (identical(motor, "duckdb")) {
      "CAST('NaN' AS DOUBLE)"
    } else {
      "CAST('NaN' AS DOUBLE PRECISION)"
    }
    ref <- paste0("NULLIF(", ref, ", ", nan, ")")
  }
  ref
}

.clases_compatibles_sql <- function(clase_col, clase_lit) {
  identical(clase_col, clase_lit) ||
    (clase_col %in% c("entero", "doble") && clase_lit %in% c("entero", "doble"))
}

.literal_sql_medicion <- function(conexion, valor, clase_col, motor) {
  if (length(valor) != 1L) {
    .no_traducible_dbi("se esperaba un valor escalar, no ", length(valor))
  }
  if (is.na(valor) && !is.nan(valor)) return("NULL")
  clase_lit <- .clase_r_columna_dbi(valor)
  if (!.clases_compatibles_sql(clase_col, clase_lit)) {
    .no_traducible_dbi(
      "compara una columna `", clase_col, "` con un valor `", clase_lit,
      "`: R convierte uno de los dos y el motor no lo hace igual"
    )
  }
  switch(
    clase_lit,
    entero = as.character(valor),
    doble = {
      if (is.nan(valor)) .no_traducible_dbi("un literal NaN")
      if (is.infinite(valor)) {
        if (identical(motor, "duckdb")) {
          if (valor > 0) "CAST('Infinity' AS DOUBLE)" else "CAST('-Infinity' AS DOUBLE)"
        } else if (identical(motor, "sqlite")) {
          if (valor > 0) "9e999" else "-9e999"
        } else {
          .no_traducible_dbi("un literal infinito en ", motor)
        }
      } else {
        literal <- formatC(valor, digits = 17, format = "g")
        if (identical(clase_col, "doble")) {
          paste0("CAST(", literal, " AS DOUBLE)")
        } else {
          literal
        }
      }
    },
    texto = {
      texto <- enc2utf8(as.character(valor))
      if (!validUTF8(texto)) .no_traducible_dbi("un texto con bytes inv\u00e1lidos")
      as.character(DBI::dbQuoteString(conexion, texto))
    },
    logico = switch(
      motor,
      duckdb = , postgresql = if (valor) "TRUE" else "FALSE",
      .no_traducible_dbi(
        "un l\u00f3gico en ", motor, ", que lo guarda como entero"
      )
    ),
    fecha = switch(
      motor,
      duckdb = paste0("DATE '", format(valor, "%Y-%m-%d"), "'"),
      sqlite = as.character(as.integer(unclass(valor))),
      .no_traducible_dbi("una fecha en ", motor)
    ),
    instante = {
      seg <- as.numeric(valor)
      if (seg != floor(seg)) {
        .no_traducible_dbi("un instante con fracci\u00f3n de segundo")
      }
      switch(
        motor,
        duckdb = paste0(
          "TIMESTAMP '", format(valor, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "'"
        ),
        sqlite = formatC(seg, digits = 17, format = "g"),
        .no_traducible_dbi("un instante en ", motor)
      )
    },
    .no_traducible_dbi("un valor de clase ", class(valor)[[1L]])
  )
}

# Traduce una expresion de formula a SQL sin usar nunca `eval()`: primero
# valida el arbol completo con `.validar_ast_formula()`, evalua las subramas
# constantes con `.evaluar_constante_formula_segura()` y traduce los operadores
# con la semantica de NA de R.
.traducir_predicado_dbi <- function(expr, entorno, visibles, esq, motor,
                                    conexion, max_valores_in = 500L,
                                    alias = NULL) {
  # Valida TODO el arbol antes de interpretar cualquier rama constante:
  # `~ t == boom()` o `~ { boom(); t == "x" }` fallan sin ejecutar nada.
  val_ast <- tryCatch(
    .validar_ast_formula(expr, names(visibles), entorno),
    error = function(e) e
  )
  if (inherits(val_ast, "condition")) {
    return(list(sql = NULL, motivo = conditionMessage(val_ast)))
  }
  tiene_columna <- function(e) {
    any(all.names(e, functions = FALSE) %in% names(visibles))
  }
  como_logico <- function(r) {
    if (!identical(r$clase, "logico")) {
      .no_traducible_dbi("la expresi\u00f3n no es l\u00f3gica en R (es `", r$clase, "`)")
    }
    if (is.null(r$sql)) {
      v <- r$valor
      if (length(v) != 1L) {
        .no_traducible_dbi("una constante l\u00f3gica de largo ", length(v))
      }
      r$sql <- if (is.na(v)) {
        "CAST(NULL AS BOOLEAN)"
      } else if (v) {
        "(1 = 1)"
      } else {
        "(1 = 0)"
      }
    }
    r
  }
  trad <- function(e) {
    if (!tiene_columna(e)) {
      v <- tryCatch(
        .evaluar_constante_formula_segura(e, entorno),
        error = function(err) .no_traducible_dbi(conditionMessage(err))
      )
      return(list(valor = v, clase = .clase_r_columna_dbi(v)))
    }
    if (is.symbol(e)) {
      nombre <- as.character(e)
      clase <- visibles[[nombre]]
      if (identical(clase, "logico") && identical(motor, "sqlite")) {
        .no_traducible_dbi("SQLite guarda el l\u00f3gico como entero")
      }
      if (identical(clase, "texto") &&
          !.cotejamiento_binario_dbi(esq, nombre, motor)) {
        .no_traducible_dbi(
          "la columna `", nombre, "` no compara por bytes (cotejamiento `",
          esq$cotejamientos[[nombre]], "`)"
        )
      }
      return(list(
        sql = .columna_sql_medicion(conexion, esq, nombre, motor, alias = alias),
        clase = clase,
        columna = nombre
      ))
    }
    if (!is.call(e)) .no_traducible_dbi("una expresi\u00f3n que no es llamada")
    f <- as.character(e[[1L]])
    args <- as.list(e)[-1L]
    if (identical(f, "(")) return(trad(args[[1L]]))
    if (identical(f, "!")) {
      a <- como_logico(trad(args[[1L]]))
      return(list(sql = paste0("(NOT ", a$sql, ")"), clase = "logico"))
    }
    if (f %in% c("&", "|")) {
      a <- como_logico(trad(args[[1L]]))
      b <- como_logico(trad(args[[2L]]))
      return(list(
        sql = paste0("(", a$sql, if (f == "&") " AND " else " OR ", b$sql, ")"),
        clase = "logico"
      ))
    }
    if (identical(f, "is.na")) {
      a <- trad(args[[1L]])
      if (is.null(a$sql)) .no_traducible_dbi("is.na() de una constante")
      return(list(sql = paste0("(", a$sql, " IS NULL)"), clase = "logico"))
    }
    if (f %in% c("==", "!=", "<", "<=", ">", ">=")) {
      a <- trad(args[[1L]])
      b <- trad(args[[2L]])
      if (is.null(a$sql) && is.null(b$sql)) .no_traducible_dbi("dos constantes")
      if (is.null(a$sql)) {
        tmp <- a; a <- b; b <- tmp
        f <- switch(f, "<" = ">", "<=" = ">=", ">" = "<", ">=" = "<=", f)
      }
      if (!f %in% c("==", "!=") && a$clase %in% c("texto", "logico")) {
        .no_traducible_dbi(
          "`", f, "` sobre ", a$clase,
          ": R ordena el texto con el locale de la sesi\u00f3n"
        )
      }
      lado_b <- if (!is.null(b$sql)) {
        if (!.clases_compatibles_sql(a$clase, b$clase)) {
          .no_traducible_dbi("compara `", a$clase, "` con `", b$clase, "`")
        }
        b$sql
      } else {
        .literal_sql_medicion(conexion, b$valor, a$clase, motor)
      }
      op <- switch(f, "==" = "=", "!=" = "<>", f)
      return(list(sql = paste0("(", a$sql, " ", op, " ", lado_b, ")"), clase = "logico"))
    }
    if (identical(f, "%in%")) {
      a <- trad(args[[1L]])
      if (is.null(a$columna)) {
        .no_traducible_dbi("`%in%` sin una columna a la izquierda")
      }
      if (tiene_columna(args[[2L]])) {
        .no_traducible_dbi("`%in%` contra columnas")
      }
      conjunto <- tryCatch(
        .evaluar_constante_formula_segura(args[[2L]], entorno),
        error = function(err) .no_traducible_dbi(conditionMessage(err))
      )
      if (is.factor(conjunto)) conjunto <- as.character(conjunto)
      con_na <- anyNA(conjunto)
      if (con_na && identical(a$clase, "doble") &&
          motor %in% c("duckdb", "postgresql")) {
        .no_traducible_dbi(
          "`%in%` con NA sobre un doble que puede guardar NaN: R no iguala NaN con NA"
        )
      }
      resto <- unique(conjunto[!is.na(conjunto)])
      if (length(resto) > max_valores_in) {
        .no_traducible_dbi("conjunto de m\u00e1s de ", max_valores_in, " valores")
      }
      lista <- vapply(
        as.list(resto),
        function(v) .literal_sql_medicion(conexion, v, a$clase, motor),
        character(1L)
      )
      dentro <- if (length(lista)) {
        paste0(a$sql, " IN (", paste(lista, collapse = ", "), ")")
      } else {
        "(1 = 0)"
      }
      sql <- if (con_na) {
        paste0("(", a$sql, " IS NULL OR ", dentro, ")")
      } else {
        paste0("(", a$sql, " IS NOT NULL AND ", dentro, ")")
      }
      return(list(sql = sql, clase = "logico"))
    }
    if (f %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
      .no_traducible_dbi("aritm\u00e9tica con columnas fuera del subconjunto SQL")
    }
    .no_traducible_dbi("`", f, "()` fuera del subconjunto traducible")
  }
  r <- tryCatch(
    trad(expr),
    lupa_no_traducible_dbi = function(e) e,
    lupa_formula_no_admitida = function(e) e
  )
  if (inherits(r, "condition")) {
    return(list(sql = NULL, motivo = conditionMessage(r)))
  }
  if (identical(r$clase, "logico")) {
    r <- tryCatch(como_logico(r), lupa_no_traducible_dbi = function(e) e)
    if (inherits(r, "condition")) {
      return(list(sql = NULL, motivo = conditionMessage(r)))
    }
  }
  if (!identical(r$clase, "logico")) {
    return(list(
      sql = NULL,
      no_logico = TRUE,
      motivo = paste0("la f\u00f3rmula no da un l\u00f3gico (", r$clase, ")")
    ))
  }
  list(sql = r$sql, motivo = NULL)
}

.sql_dentro <- function(pred) {
  paste0("(CASE WHEN ", pred, " THEN 1 ELSE 0 END = 1)")
}

.sql_suma <- function(cond) {
  paste0("COALESCE(SUM(CASE WHEN ", cond, " THEN 1 ELSE 0 END), 0)")
}

.sql_y <- function(...) {
  partes <- unlist(list(...))
  partes <- partes[nzchar(partes)]
  if (!length(partes)) "(1 = 1)" else paste0("(", paste(partes, collapse = " AND "), ")")
}

.sql_no_ascii <- function(expr, motor) {
  switch(
    motor,
    sqlite = paste0("(", expr, " GLOB '*[^' || char(1) || '-' || char(127) || ']*')"),
    duckdb = paste0("(strlen(", expr, ") <> length(", expr, "))"),
    .no_traducible_dbi("la guarda de texto no ASCII en ", motor)
  )
}

.sql_plegado_centinela <- function(col, motor, conexion) {
  blancos <- switch(
    motor,
    sqlite = "' ' || char(9) || char(10) || char(13)",
    duckdb = "' ' || chr(9) || chr(10) || chr(13)",
    .no_traducible_dbi("el recorte de bordes en ", motor)
  )
  i_punto <- as.character(DBI::dbQuoteString(conexion, intToUtf8(0x0130L)))
  paste0("REPLACE(TRIM(", col, ", ", blancos, "), ", i_punto, ", 'i')")
}

# ---------------------------------------------------------------------------
# 4. Compilacion de cada nodo desde la especificacion
# ---------------------------------------------------------------------------

.compilar_especificacion_dbi <- function(spec, instancia, esquemas, motor,
                                         conexion, universos, max_valores_in) {
  no <- function(...) list(via = "no_medible", motivo = paste0(...))
  ent <- spec$entidad
  ent_esq <- if (identical(spec$nodo, "cobertura_referencial")) {
    spec$entidad_dep
  } else ent
  esq <- esquemas[[ent_esq]]
  if (is.null(esq)) return(no("la entidad no est\u00e1 en la base"))
  columnas <- switch(
    spec$nodo,
    duplicado = spec$columnas,
    predicado_fila = instancia$atributos,
    cobertura_referencial = spec$fk,
    spec$columna
  )
  faltan <- setdiff(columnas, esq$campos)
  if (length(faltan)) {
    return(list(
      via = "no_medible_metodo",
      motivo = if (identical(spec$nodo, "predicado_fila") ||
                   (identical(spec$nodo, "duplicado") && !isTRUE(spec$excluir_na))) {
        paste0("No se encontraron atributos ligados: ",
               paste(faltan, collapse = ", "), ".")
      } else {
        paste0("No se encontr\u00f3 el atributo ligado: ", ent_esq, "$",
               faltan[[1L]], ".")
      }
    ))
  }
  clases <- esq$clases[columnas]
  no_escalares <- columnas[unname(clases) == "otro"]
  if (length(no_escalares)) {
    return(no(
      "la columna `", no_escalares[[1L]],
      "` no es un tipo escalar que R pueda leer para esta metrica"
    ))
  }
  lecturas_no_doble <- columnas[vapply(columnas, function(c) {
    clase <- esq$clases[[c]]
    !clase %in% c("doble", "entero") &&
      .tipo_declarado_entero_ancho_dbi(esq$tipos[[c]])
  }, logical(1L))]
  if (length(lecturas_no_doble)) {
    columna_no_doble <- lecturas_no_doble[[1L]]
    clase_lectura <- switch(
      esq$clases[[columna_no_doble]],
      entero64 = "entero de 64 bits (`integer64`)",
      texto = "texto",
      esq$clases[[columna_no_doble]]
    )
    tipo_real <- esq$tipos[[columna_no_doble]]
    if (is.null(tipo_real) || !length(tipo_real) || is.na(tipo_real)) {
      tipo_real <- "tipo num\u00e9rico no especificado"
    }
    return(no(
      "R lee la columna `", columna_no_doble, "` como ", clase_lectura,
      ", pero la base la declara como `", as.character(tipo_real),
      "`; la medici\u00f3n no puede reproducir la lectura de R"
    ))
  }
  pre <- .precondicion_motor_medicion(motor, spec$nodo, clases)
  if (!is.null(pre)) return(no(pre))
  U <- universos[[ent_esq]](instancia)
  if (!is.null(U$motivo)) {
    return(no("la regla de `aplicabilidad`: ", U$motivo))
  }
  col <- function(c, nan = TRUE) {
    .columna_sql_medicion(conexion, esq, c, motor, normalizar_nan = nan)
  }
  binario <- function(c) {
    if (identical(esq$clases[[c]], "texto") &&
        !.cotejamiento_binario_dbi(esq, c, motor)) {
      .no_traducible_dbi(
        "la columna `", c, "` no compara por bytes (cotejamiento `",
        esq$cotejamientos[[c]], "`)"
      )
    }
  }
  r <- tryCatch(switch(
    spec$nodo,
    no_ausente = {
      x <- col(spec$columna)
      clase <- esq$clases[[spec$columna]]
      aplicable <- ""
      if (!is.null(spec$aplicable)) {
        t <- .traducir_predicado_dbi(
          spec$aplicable[[2L]], environment(spec$aplicable),
          as.list(esq$clases), esq, motor, conexion, max_valores_in
        )
        if (is.null(t$sql)) .no_traducible_dbi("`aplicable`: ", t$motivo)
        aplicable <- .sql_dentro(t$sql)
      }
      presentes <- if (isTRUE(spec$opcional)) paste0("(", x, " IS NOT NULL)") else ""
      universo <- .sql_y(U$sql, aplicable, presentes)
      centinela <- "(1 = 0)"
      vn <- spec$valores_nulos
      if (length(vn)) {
        es_texto <- identical(clase, "texto") || is.character(vn) || is.factor(vn)
        if (es_texto) {
          if (!identical(clase, "texto")) {
            .no_traducible_dbi(
              "centinela de texto sobre una columna `", clase,
              "`: R compara el texto de cada valor"
            )
          }
          binario(spec$columna)
          objetivos <- unique(.minusculas_para_comparar(.texto_analizable(vn)$valores))
          objetivos <- objetivos[!is.na(objetivos)]
          if (any(grepl("[^\\001-\\177]", objetivos, useBytes = TRUE, perl = TRUE))) {
            .no_traducible_dbi(
              "centinela no ASCII: el plegado de may\u00fasculas del motor no es el del paquete"
            )
          }
          if (length(objetivos) > max_valores_in) {
            .no_traducible_dbi("demasiados centinelas")
          }
          if (length(objetivos)) {
            plegado <- .sql_plegado_centinela(x, motor, conexion)
            centinela <- paste0(
              "(NOT ", .sql_no_ascii(plegado, motor), " AND LOWER(", plegado, ") IN (",
              paste(vapply(objetivos, function(o) {
                as.character(DBI::dbQuoteString(conexion, o))
              }, character(1L)), collapse = ", "), "))"
            )
          }
        } else {
          lits <- unique(vn[!is.na(vn)])
          if (length(lits) > max_valores_in) {
            .no_traducible_dbi("demasiados centinelas")
          }
          if (length(lits)) {
            centinela <- paste0(
              "(", x, " IN (",
              paste(vapply(as.list(lits), function(v) {
                .literal_sql_medicion(conexion, v, clase, motor)
              }, character(1L)), collapse = ", "), "))"
            )
          }
        }
      }
      cumple <- paste0("(", x, " IS NOT NULL AND NOT ", centinela, ")")
      list(
        via = "sql_consolidado",
        universo = universo,
        evaluable = universo,
        cumple = cumple,
        sin_valor = paste0("(", x, " IS NULL)"),
        columnas_completas = spec$columna,
        aplicable_sql = .sql_y(aplicable, presentes)
      )
    },
    en_conjunto = , en_rango = , fecha_en_rango = {
      x <- col(spec$columna)
      clase <- esq$clases[[spec$columna]]
      guarda <- NULL
      if (identical(clase, "texto")) {
        binario(spec$columna)
        if (identical(motor, "sqlite")) guarda <- .sql_no_ascii(x, motor)
      }
      cumple <- if (identical(spec$nodo, "en_conjunto")) {
        conjunto <- spec$conjunto
        if (is.factor(conjunto)) conjunto <- as.character(conjunto)
        if ((clase %in% c("entero", "doble") && is.character(conjunto)) ||
            (identical(clase, "texto") && !is.character(conjunto))) {
          .no_traducible_dbi(
            "dominio de otra clase que la columna: R compara el texto de cada valor"
          )
        }
        lits <- unique(conjunto[!is.na(conjunto)])
        if (length(lits) > max_valores_in) {
          .no_traducible_dbi("dominio de m\u00e1s de ", max_valores_in, " valores")
        }
        if (length(lits)) {
          paste0(
            "(", x, " IN (",
            paste(vapply(as.list(lits), function(v) {
              .literal_sql_medicion(conexion, v, clase, motor)
            }, character(1L)), collapse = ", "), "))"
          )
        } else {
          "(1 = 0)"
        }
      } else if (identical(spec$nodo, "en_rango")) {
        if (identical(clase, "texto")) {
          .no_traducible_dbi(
            "rango sobre texto: R ordena con el locale de la sesi\u00f3n"
          )
        }
        lo <- .literal_sql_medicion(conexion, spec$minimo, clase, motor)
        hi <- .literal_sql_medicion(conexion, spec$maximo, clase, motor)
        paste0(
          "(", x, if (spec$inclusivo[[1L]]) " >= " else " > ", lo,
          " AND ", x, if (spec$inclusivo[[2L]]) " <= " else " < ", hi, ")"
        )
      } else {
        if (!clase %in% c("fecha", "instante")) {
          return(list(
            via = "no_medible_metodo",
            motivo = if (isTRUE(spec$contrato_columna)) {
              "La columna de actualizaci\u00f3n debe ser Date o POSIXt."
            } else {
              "La oportunidad requiere un atributo Date o POSIXt."
            }
          ))
        }
        seg <- switch(
          motor,
          duckdb = if (identical(clase, "fecha")) {
            paste0("epoch(CAST(", x, " AS TIMESTAMP))")
          } else {
            paste0("epoch(", x, ")")
          },
          sqlite = if (identical(clase, "fecha")) paste0("(", x, " * 86400)") else x
        )
        lit <- function(v) formatC(.fecha_numerica(v), digits = 17, format = "g")
        partes <- c(
          if (!is.null(spec$desde)) paste0(seg, " >= ", lit(spec$desde)),
          if (!is.null(spec$hasta)) paste0(seg, " <= ", lit(spec$hasta))
        )
        paste0("(", paste(partes, collapse = " AND "), ")")
      }
      evaluable <- .sql_y(U$sql, paste0("(", x, " IS NOT NULL)"))
      list(
        via = "sql_consolidado",
        universo = U$sql,
        evaluable = evaluable,
        cumple = cumple,
        sin_valor = paste0("(", x, " IS NULL)"),
        guarda = if (!is.null(guarda)) .sql_y(evaluable, guarda),
        columnas_completas = spec$columna
      )
    },
    predicado_fila = {
      vis <- as.list(esq$clases[instancia$atributos])
      t <- .traducir_predicado_dbi(
        spec$formula[[2L]], environment(spec$formula), vis,
        esq, motor, conexion, max_valores_in
      )
      if (isTRUE(t$no_logico)) {
        return(list(
          via = "sql_consolidado",
          universo = U$sql,
          evaluable = U$sql,
          cumple = "(1 = 1)",
          no_logico = TRUE,
          columnas_completas = instancia$atributos
        ))
      }
      if (is.null(t$sql)) .no_traducible_dbi("la f\u00f3rmula: ", t$motivo)
      list(
        via = "sql_consolidado",
        universo = U$sql,
        evaluable = U$sql,
        cumple = t$sql,
        na_regla = .sql_y(U$sql, paste0("(", t$sql, " IS NULL)")),
        columnas_completas = instancia$atributos
      )
    },
    duplicado = {
      for (c in spec$columnas) binario(c)
      cols_g <- vapply(
        spec$columnas,
        function(c) col(c, nan = isTRUE(spec$excluir_na)),
        character(1L)
      )
      filtro <- if (isTRUE(spec$excluir_na)) {
        paste0("(", cols_g[[1L]], " IS NOT NULL)")
      } else ""
      guarda <- if (isTRUE(spec$excluir_na) && identical(motor, "sqlite") &&
                    identical(esq$clases[[spec$columnas]], "texto")) {
        .sql_y(U$sql, filtro, .sql_no_ascii(cols_g[[1L]], motor))
      }
      sub <- paste0(
        "SELECT ", paste(cols_g, collapse = ", "), ", COUNT(*) AS lupa_c FROM ",
        esq$sql, " WHERE ", .sql_y(U$sql, filtro), " GROUP BY ",
        paste(cols_g, collapse = ", ")
      )
      list(
        via = "sql_dedicado",
        consulta = paste0(
          "SELECT COALESCE(SUM(CASE WHEN lupa_c > 1 THEN lupa_c ELSE 0 END), 0)",
          " AS num, COALESCE(SUM(lupa_c), 0) AS den FROM (", sub, ") lupa_g"
        ),
        universo = U$sql,
        evaluable = .sql_y(U$sql, filtro),
        sin_valor = if (isTRUE(spec$excluir_na)) {
          paste0("(", cols_g[[1L]], " IS NULL)")
        },
        guarda = guarda,
        columnas_completas = spec$columnas
      )
    },
    cobertura_referencial = {
      esq_ref <- esquemas[[spec$entidad_ref]]
      if (is.null(esq_ref)) {
        .no_traducible_dbi("la entidad de referencia no est\u00e1 en la base")
      }
      if (!spec$pk %in% esq_ref$campos) {
        return(list(
          via = "no_medible_metodo",
          motivo = paste0(
            "No se encontr\u00f3 el atributo ligado: ",
            spec$entidad_ref, "$", spec$pk, "."
          )
        ))
      }
      pre_ref <- .precondicion_motor_medicion(
        motor, spec$nodo, esq_ref$clases[[spec$pk]]
      )
      if (!is.null(pre_ref)) .no_traducible_dbi(pre_ref)
      cp <- esq_ref$clases[[spec$pk]]
      cf <- esq$clases[[spec$fk]]
      pk <- .columna_sql_medicion(conexion, esq_ref, spec$pk, motor, alias = "lupa_r")
      fk <- .columna_sql_medicion(conexion, esq, spec$fk, motor, alias = "lupa_d")
      texto_cast <- if (identical(motor, "duckdb")) "VARCHAR" else "TEXT"
      igualdad <- if ((.clases_compatibles_sql(cp, cf) &&
                       !(cp %in% "entero64" || cf %in% "entero64")) ||
                      identical(cp, cf)) {
        paste0(pk, " = ", fk)
      } else if (identical(cp, "texto") && identical(cf, "entero")) {
        paste0(pk, " = CAST(", fk, " AS ", texto_cast, ")")
      } else if (identical(cp, "entero") && identical(cf, "texto")) {
        paste0("CAST(", pk, " AS ", texto_cast, ") = ", fk)
      } else {
        .no_traducible_dbi(
          "clave `", cp, "` contra `", cf,
          "`: R compara por el texto can\u00f3nico de cada valor y el motor no escribe igual ese tipo"
        )
      }
      if ("texto" %in% c(cp, cf)) {
        if ((identical(cp, "texto") && !.cotejamiento_binario_dbi(esq_ref, spec$pk, motor)) ||
            (identical(cf, "texto") && !.cotejamiento_binario_dbi(esq, spec$fk, motor))) {
          .no_traducible_dbi("clave de texto sin cotejamiento binario")
        }
      }
      U_ref <- universos[[spec$entidad_ref]](instancia, columnas = spec$pk, alias = "lupa_r")
      if (!is.null(U_ref$motivo)) {
        .no_traducible_dbi("aplicabilidad de la referencia: ", U_ref$motivo)
      }
      U_dep <- universos[[spec$entidad_dep]](instancia, columnas = spec$fk, alias = "lupa_d")
      if (!is.null(U_dep$motivo)) {
        .no_traducible_dbi("aplicabilidad: ", U_dep$motivo)
      }
      evaluable <- .sql_y(U_dep$sql, paste0("(", fk, " IS NOT NULL)"))
      existe <- paste0(
        "EXISTS (SELECT 1 FROM ", esq_ref$sql, " lupa_r WHERE ",
        .sql_y(U_ref$sql, paste0("(", pk, " IS NOT NULL)"), igualdad), ")"
      )
      guarda <- if (identical(motor, "sqlite") && "texto" %in% c(cp, cf)) {
        c(
          if (identical(cf, "texto")) {
            .sql_suma(.sql_y(evaluable, .sql_no_ascii(fk, motor)))
          },
          if (identical(cp, "texto")) {
            paste0(
              "(SELECT ",
              .sql_suma(.sql_y(U_ref$sql, paste0("(", pk, " IS NOT NULL)"),
                               .sql_no_ascii(pk, motor))),
              " FROM ", esq_ref$sql, " lupa_r)"
            )
          }
        )
      }
      consulta <- paste0(
        "SELECT ", .sql_suma(.sql_y(evaluable, existe)), " AS num, ",
        .sql_suma(evaluable), " AS den, ",
        .sql_suma(.sql_y(U_dep$sql)), " AS univ, ",
        .sql_suma(.sql_y(U_dep$sql, paste0("(", fk, " IS NULL)"))), " AS sin_valor, ",
        "COUNT(*) AS n_filas",
        if (length(guarda)) {
          paste0(", (", paste(paste0("COALESCE(", guarda, ", 0)"),
                              collapse = " + "), ") AS guarda")
        } else "",
        " FROM ", esq$sql, " lupa_d"
      )
      list(via = "sql_dedicado_referencial", consulta = consulta)
    }
  ), lupa_no_traducible_dbi = function(e) e)
  if (inherits(r, "condition")) return(no(conditionMessage(r)))
  r
}

# ---------------------------------------------------------------------------
# 5. Planificacion, ejecucion consolidada, bloques y ensamblado
# ---------------------------------------------------------------------------

.no_medible_dbi <- function(instancia, id_medicion, fecha, mensaje,
                            en_base = FALSE,
                            causa = if (en_base) " en la base" else " sobre estos datos") {
  .cobertura_metrica_abstenida(instancia, id_medicion, fecha, list(
    estado = "no_medible",
    motivo = paste0(
      "La m\u00e9trica `", instancia$nombre, "` no se midi\u00f3",
      causa, ": ", mensaje
    ),
    como_resolverlo = if (en_base) {
      paste(
        "Medirla con `estrategia_no_sql = \"bloques\"` si es separable por fila,",
        "o en memoria con `medir()`. Una m\u00e9trica que no se midi\u00f3 no se",
        "interpreta como cero."
      )
    } else {
      paste(
        "Revisar que el atributo ligado exista y tenga el tipo y la cantidad",
        "de valores que la m\u00e9trica requiere, y si el m\u00e9todo es propio,",
        "que cumpla el contrato de `metodo` (?modelo_calidad). Una m\u00e9trica",
        "que no se midi\u00f3 no se interpreta como cero."
      )
    }
  ))
}

.universos_medicion_dbi <- function(aplicabilidad, esquemas, motor, conexion,
                                    max_valores_in) {
  lapply(esquemas, function(esq) {
    function(instancia, columnas = instancia$atributos, alias = NULL) {
      if (is.null(aplicabilidad) || !length(columnas)) return(list(sql = ""))
      reglas <- aplicabilidad[names(aplicabilidad) %in% intersect(columnas, esq$campos)]
      if (!length(reglas)) return(list(sql = ""))
      partes <- character()
      for (f in reglas) {
        if (!.aplicabilidad_es_predicado_fila(f[[2L]])) {
          return(list(motivo = "no es una condici\u00f3n por fila"))
        }
        t <- .traducir_predicado_dbi(
          f[[2L]], environment(f), as.list(esq$clases), esq,
          motor, conexion, max_valores_in, alias = alias
        )
        if (is.null(t$sql)) return(list(motivo = t$motivo))
        partes <- c(partes, .sql_dentro(t$sql))
      }
      list(sql = .sql_y(partes))
    }
  })
}

.verificar_esquema_dbi <- function(spec, instancia, prototipos) {
  if (identical(spec$nodo, "no_ausente") && !is.null(spec$aplicable)) {
    proto <- prototipos[[spec$entidad]]
    val <- if (inherits(proto, "data.frame")) {
      tryCatch(
        .validar_ast_formula(spec$aplicable[[2L]], names(proto),
                             environment(spec$aplicable)),
        error = function(e) e
      )
    } else NULL
    if (inherits(val, "condition")) {
      # No evaluar `aplicable` en R si tiene llamadas fuera del AST cerrado;
      # solo verificar existencia de tabla/columna y dejar que el compilador
      # SQL lo declare no_medible sin ejecutar codigo.
      spec_sin_apl <- spec
      spec_sin_apl$aplicable <- NULL
      inst_sin_apl <- instancia
      inst_sin_apl$configuracion$aplicable <- NULL
      r <- tryCatch(
        .evaluar_especificacion(spec_sin_apl, prototipos, inst_sin_apl),
        error = function(e) e
      )
      if (inherits(r, "error")) return(list(error = conditionMessage(r)))
      return(NULL)
    }
  }
  r <- tryCatch(
    .evaluar_especificacion(spec, prototipos, instancia),
    error = function(e) e
  )
  if (inherits(r, "error")) return(list(error = conditionMessage(r)))
  if (!is.null(r$abstencion)) {
    return(list(abstencion = attr(r$abstencion, "abstencion", exact = TRUE)))
  }
  NULL
}

.limites_escalares_oportunidad <- function(instancia) {
  cfg <- instancia$configuracion
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  switch(
    nombre,
    OportunidadAtributoPorFecha = length(cfg$fecha_limite) == 1L,
    OportunidadAtributoPorIntervalo = length(cfg$inicio_vigencia) == 1L &&
      length(cfg$fin_vigencia) == 1L,
    GradoOportunidadAtributoPorFecha = length(cfg$fecha_solicitud) == 1L &&
      length(cfg$fecha_fin_utilidad) == 1L,
    GradoOportunidadAtributoPorIntervalo = length(cfg$inicio_vigencia) == 1L &&
      length(cfg$fin_vigencia) == 1L,
    OportunidadEntPorFecha = is.null(cfg$vigencia$fecha_limite) ||
      length(cfg$vigencia$fecha_limite) == 1L,
    OportunidadEntPorIntervalo = is.null(cfg$vigencia$inicio_intervalo) ||
      is.null(cfg$vigencia$fin_intervalo) ||
      (length(cfg$vigencia$inicio_intervalo) == 1L &&
         length(cfg$vigencia$fin_intervalo) == 1L),
    TRUE
  )
}

.separable_medicion_dbi <- function(instancia, spec) {
  if (!is.null(spec)) {
    return(spec$nodo %in% c(
      "no_ausente", "en_conjunto", "en_rango", "fecha_en_rango", "predicado_fila"
    ))
  }
  if (!.metodo_del_paquete(instancia$metodo)) return(FALSE)
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  ruta <- .RUTAS_CATALOGO_METRICAS$ruta_sin_especificacion[
    .RUTAS_CATALOGO_METRICAS$metrica == nombre
  ]
  if (length(ruta) != 1L || is.na(ruta) || !startsWith(ruta, "bloques")) {
    return(FALSE)
  }
  .limites_escalares_oportunidad(instancia)
}

.columnas_proyeccion_bloques <- function(instancia, aplicabilidad, esq) {
  cols <- c(
    instancia$atributos,
    instancia$configuracion$vigencia$columna_actualizacion
  )
  if (!is.null(instancia$configuracion$aplicable)) {
    cols <- c(cols, all.vars(instancia$configuracion$aplicable))
  }
  if (!is.null(aplicabilidad) && length(instancia$atributos)) {
    reglas <- aplicabilidad[names(aplicabilidad) %in% instancia$atributos]
    if (length(reglas)) {
      cols <- c(cols, names(reglas), unlist(lapply(reglas, all.vars), use.names = FALSE))
    }
  }
  unique(intersect(as.character(cols), esq$campos))
}

.lotes_consolidada_medicion <- function(items, senas, tamano_lote) {
  if (!length(items)) return(list())
  limite_expr <- .limite_expresiones_dbi(senas)
  # Cada item agrega hasta 9 expresiones SUM(CASE ...).
  max_por_expr <- if (is.finite(limite_expr)) {
    max(1L, as.integer(floor((limite_expr - 1) / 9)))
  } else {
    length(items)
  }
  tope <- if (is.null(tamano_lote)) max_por_expr else min(as.integer(tamano_lote), max_por_expr)
  if (tope >= length(items)) return(list(items))
  indices <- split(seq_along(items), ceiling(seq_along(items) / tope))
  lapply(indices, function(idx) items[idx])
}

.planificar_medicion_dbi <- function(modelo, conexion, tablas, aplicabilidad,
                                     motor, max_valores_in, estrategia_no_sql,
                                     tamano_lote = NULL, contador = NULL) {
  resuelto <- .resolver_tablas_medir_dbi(modelo, conexion, tablas)
  con <- resuelto$conexion
  entidades <- resuelto$entidades
  fisicas <- resuelto$fisicas
  errores_esquema <- stats::setNames(vector("list", length(entidades)), entidades)
  esquemas <- stats::setNames(lapply(entidades, function(e) {
    tryCatch(
      .esquema_medicion_dbi(con, fisicas[[e]], motor, contador = contador),
      error = function(err) {
        errores_esquema[[e]] <<- if (
          inherits(err, "lupa_presupuesto_medicion_dbi")
        ) {
          conditionMessage(err)
        } else {
          .motivo_tabla_no_medible_dbi(fisicas[[e]])
        }
        NULL
      }
    )
  }), entidades)
  prototipos <- lapply(esquemas, function(esq) {
    if (is.null(esq)) NULL else esq$prototipo
  })
  universos <- .universos_medicion_dbi(
    aplicabilidad, Filter(Negate(is.null), esquemas),
    motor, con, max_valores_in
  )
  planes <- lapply(seq_along(modelo$metricas), function(j) {
    instancia <- modelo$metricas[[j]]
    spec <- .especificacion_metrica(instancia)
    ent_dest <- instancia$entidad[[
      if (!is.null(spec) && identical(spec$nodo, "cobertura_referencial")) 2L else 1L
    ]]
    base <- list(j = j, instancia = instancia, spec = spec, entidad = ent_dest)
    if (any(vapply(esquemas[instancia$entidad], is.null, logical(1L)))) {
      motivos <- unlist(errores_esquema[instancia$entidad], use.names = FALSE)
      motivos <- motivos[!is.na(motivos) & nzchar(motivos)]
      return(c(base, list(
        via = "no_medible",
        motivo = if (length(motivos)) motivos[[1L]] else "la tabla no est\u00e1 en la base"
      )))
    }
    cols_bloq <- .columnas_proyeccion_bloques(
      instancia, aplicabilidad, esquemas[[instancia$entidad[[1L]]]]
    )
    a_bloques <- function(motivo) {
      if (identical(estrategia_no_sql, "bloques") &&
          .separable_medicion_dbi(instancia, spec) &&
          length(instancia$entidad) == 1L &&
          length(cols_bloq) > 0L) {
        c(base, list(via = "bloques_r", motivo = motivo, columnas_bloque = cols_bloq))
      } else {
        c(base, list(via = "no_medible", motivo = motivo))
      }
    }
    if (is.null(spec)) {
      nombre <- as.character(instancia$declaracion$nombre)[[1L]]
      ruta <- .RUTAS_CATALOGO_METRICAS$ruta_sin_especificacion[
        .RUTAS_CATALOGO_METRICAS$metrica == nombre
      ]
      motivo <- if (!.metodo_del_paquete(instancia$metodo)) {
        "m\u00e9todo propio de R: no tiene especificaci\u00f3n traducible"
      } else if (length(ruta) == 1L && !is.na(ruta)) {
        paste0("sin especificaci\u00f3n traducible; ruta: ", ruta)
      } else {
        "sin especificaci\u00f3n traducible"
      }
      return(a_bloques(motivo))
    }
    verif <- .verificar_esquema_dbi(spec, instancia, prototipos)
    if (!is.null(verif$error) && identical(spec$nodo, "predicado_fila") &&
        startsWith(verif$error, "El validador de ReglaIntegridadIntraEntidad")) {
      verif$error <- NULL
    }
    if (!is.null(verif$error)) {
      return(c(base, list(via = "no_medible_metodo", motivo = verif$error)))
    }
    if (!is.null(verif$abstencion)) {
      return(c(base, list(via = "abstencion", abstencion = verif$abstencion)))
    }
    comp <- .compilar_especificacion_dbi(
      spec, instancia, esquemas, motor, con, universos, max_valores_in
    )
    if (identical(comp$via, "no_medible")) return(a_bloques(comp$motivo))
    c(base, comp, list(columnas_bloque = cols_bloq))
  })
  n_esquema_estimadas <- sum(vapply(
    Filter(Negate(is.null), esquemas),
    function(e) as.integer(e$n_consultas_esquema),
    integer(1L)
  ))
  senas <- .senas_conexion_dbi(con)
  n_consolidadas <- 0L
  for (e in names(esquemas)) {
    if (is.null(esquemas[[e]])) next
    items_e <- Filter(
      function(x) x$via %in% c("sql_consolidado", "sql_dedicado") &&
        identical(x$entidad, e),
      planes
    )
    if (length(items_e)) {
      n_consolidadas <- n_consolidadas +
        length(.lotes_consolidada_medicion(items_e, senas, tamano_lote))
    }
  }
  list(
    conexion = con,
    planes = planes,
    esquemas = esquemas,
    fisicas = fisicas,
    motor = motor,
    n_esquema = if (is.null(contador)) n_esquema_estimadas else contador$n,
    n_consolidadas = n_consolidadas
  )
}

.consultar_medicion_dbi <- function(conexion, sql, contador,
                                    registrar_sql = TRUE) {
  if (contador$n >= contador$tope) {
    .detener(structure(
      class = c("lupa_presupuesto_medicion_dbi", "error", "condition"),
      list(message = "se agot\u00f3 `max_consultas`", call = NULL)
    ))
  }
  contador$n <- contador$n + 1L
  if (isTRUE(registrar_sql)) {
    contador$sql <- c(contador$sql, sql)
  }
  .traer_consulta_dbi(conexion, sql)
}

.consolidada_medicion_dbi <- function(conexion, esq, items, contador) {
  agregados <- function(it) {
    j <- it$j
    c(
      sprintf("%s AS m%d_rec", .sql_suma(.sql_y(it$universo_aplicabilidad)), j),
      sprintf("%s AS m%d_univ", .sql_suma(.sql_y(it$universo)), j),
      sprintf("%s AS m%d_den", .sql_suma(.sql_y(it$evaluable)), j),
      if (identical(it$via, "sql_consolidado")) {
        sprintf("%s AS m%d_num", .sql_suma(.sql_y(it$evaluable, it$cumple)), j)
      },
      if (!is.null(it$sin_valor)) {
        sprintf("%s AS m%d_sv", .sql_suma(.sql_y(it$universo_dentro, it$sin_valor)), j)
      },
      sprintf(
        "%s AS m%d_apl",
        .sql_suma(.sql_y(it$universo_aplicabilidad, it$aplicable_sql %||% "")), j
      ),
      sprintf(
        "%s AS m%d_comp",
        .sql_suma(.sql_y(it$universo_aplicabilidad, it$completas)), j
      ),
      if (!is.null(it$guarda)) sprintf("%s AS m%d_g", .sql_suma(it$guarda), j),
      if (!is.null(it$na_regla)) sprintf("%s AS m%d_na", .sql_suma(it$na_regla), j)
    )
  }
  correr <- function(grupo) {
    sql <- paste0(
      "SELECT COUNT(*) AS n_filas, ",
      paste(unlist(lapply(grupo, agregados)), collapse = ", "),
      " FROM ", esq$sql
    )
    r <- tryCatch(
      .consultar_medicion_dbi(conexion, sql, contador),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
    if (!inherits(r, "error")) return(list(ok = r))
    if (inherits(r, "lupa_presupuesto_medicion_dbi") || length(grupo) == 1L) {
      msg <- if (inherits(r, "lupa_presupuesto_medicion_dbi")) {
        conditionMessage(r)
      } else {
        .mensaje_motor(r)
      }
      return(list(
        fallo = stats::setNames(
          rep(msg, length(grupo)),
          vapply(grupo, function(x) as.character(x$j), character(1L))
        )
      ))
    }
    mitad <- seq_len(length(grupo) %/% 2L)
    a <- correr(grupo[mitad])
    b <- correr(grupo[-mitad])
    list(
      ok = if (!is.null(a$ok) && !is.null(b$ok)) {
        cbind(a$ok, b$ok[setdiff(names(b$ok), "n_filas")])
      } else if (!is.null(a$ok)) {
        a$ok
      } else {
        b$ok
      },
      fallo = c(a$fallo, b$fallo)
    )
  }
  correr(items)
}

# ---------------------------------------------------------------------------
# 6. Lectura por bloques con proyeccion de columnas (SQLite cursor / DuckDB rowid)
# ---------------------------------------------------------------------------

.absorber_bloque_medicion_dbi <- function(bloque, acum, ent_nombre,
                                          aplicabilidad, id_medicion, fecha,
                                          proteger_datos_personales,
                                          columnas_personales,
                                          validadores_personales) {
  bloque <- .normalizar_columnas_texto(bloque)
  n_bloque <- nrow(bloque)
  tablas_b <- stats::setNames(list(bloque), ent_nombre)
  aplic_b <- .mascaras_aplicabilidad_medicion(tablas_b, aplicabilidad)
  for (k in seq_along(acum)) {
    a <- acum[[k]]
    if (!isTRUE(a$ok)) next
    inst <- a$item$instancia
    gran <- as.character(inst$declaracion$granularidad)[[1L]]
    a$n_filas <- a$n_filas + n_bloque
    tb_inst <- .recortar_tablas_aplicables(tablas_b, aplic_b, inst)
    t_rec <- tb_inst[[ent_nombre]]
    n_rec <- if (inherits(t_rec, "data.frame")) nrow(t_rec) else 0L
    a$n_rec <- a$n_rec + n_rec
    if (n_rec > 0L) {
      dentro_apl <- tryCatch(
        .mascara_aplicable_instancia(t_rec, inst),
        error = function(e) rep(TRUE, n_rec)
      )
      a$n_apl <- a$n_apl + sum(dentro_apl)
      if (length(inst$atributos)) {
        idx_atr <- .indice_nombre(inst$atributos, names(t_rec))
        if (!anyNA(idx_atr)) {
          sub_atr <- .seleccionar_columnas(t_rec, inst$atributos)
          a$n_comp <- a$n_comp + sum(.filas_completas(sub_atr))
          celdas_na <- lapply(idx_atr, function(ii) is.na(t_rec[[ii]])[dentro_apl])
          a$sin_valor <- a$sin_valor + if (identical(gran, "instanciaAtributo")) {
            sum(vapply(celdas_na, sum, numeric(1L)))
          } else {
            sum(Reduce(`|`, celdas_na))
          }
        }
      }
      sola <- modelo(list(inst))
      parcial <- tryCatch(
        withCallingHandlers(
          medir(
            sola, tb_inst, id_medicion = id_medicion, fecha = fecha,
            proteger_datos_personales = proteger_datos_personales,
            columnas_personales = columnas_personales,
            validadores_personales = validadores_personales,
            detalle = "resumen"
          ),
          warning = function(w) {
            if (grepl("porque su m", conditionMessage(w),
                      fixed = TRUE, useBytes = TRUE)) {
              invokeRestart("muffleWarning")
            }
          }
        ),
        error = function(e) e
      )
      if (inherits(parcial, "error")) {
        a$ok <- FALSE
        a$error_metodo <- conditionMessage(parcial)
      } else if (nrow(parcial) > 0L) {
        fila <- parcial[1L, , drop = FALSE]
        a$plantilla <- fila
        a$den <- a$den + as.numeric(fila$denominador[[1L]])
        a$univ <- a$univ + as.numeric(fila$n_universo[[1L]])
        if (identical(fila$agregacion[[1L]], "ratio")) {
          a$num <- a$num + as.numeric(fila$numerador[[1L]])
        } else {
          a$sum_val <- a$sum_val +
            as.numeric(fila$resultado[[1L]]) * as.numeric(fila$denominador[[1L]])
        }
      } else {
        cob_p <- attr(parcial, "cobertura_metricas", exact = TRUE)
        if (inherits(cob_p, "data.frame") && nrow(cob_p)) {
          est <- cob_p$estado[[1L]]
          if (!identical(est, "sin_valores")) {
            a$ok <- FALSE
            a$cobertura_directa <- cob_p[1L, , drop = FALSE]
          } else {
            # Bloque sin valores no nulos: suma su universo para el total final.
            esp <- if (identical(gran, "instanciaAtributo")) {
              sum(dentro_apl) * length(inst$atributos)
            } else {
              sum(dentro_apl)
            }
            a$univ <- a$univ + as.numeric(esp)
          }
        }
      }
    }
    acum[[k]] <- a
  }
  acum
}

.ejecutar_bloques_entidad_medicion <- function(conexion, esq, items, motor,
                                               bloque_filas, aplicabilidad,
                                               id_medicion, fecha,
                                               proteger_datos_personales,
                                               columnas_personales,
                                               validadores_personales,
                                               contador) {
  cols <- unique(unlist(lapply(items, `[[`, "columnas_bloque"), use.names = FALSE))
  cols <- intersect(cols, esq$campos)
  if (!length(cols)) {
    return(lapply(items, function(it) {
      list(ok = FALSE, item = it, error_base = "no hay columnas proyectables para bloques")
    }))
  }
  cols_sql <- vapply(cols, function(c) .citar_col_dbi(conexion, c), character(1L))
  sql_base <- paste0("SELECT ", paste(cols_sql, collapse = ", "), " FROM ", esq$sql)
  ent_nombre <- items[[1L]]$entidad
  acum <- lapply(items, function(it) {
    list(
      ok = TRUE, item = it, n_filas = 0, n_rec = 0, n_apl = 0, n_comp = 0,
      num = 0, sum_val = 0, den = 0, univ = 0, sin_valor = 0,
      plantilla = NULL, error_metodo = NULL, error_base = NULL,
      cobertura_directa = NULL
    )
  })
  cap <- .capacidad_fuente_bloques_dbi(conexion)
  if (isTRUE(cap$disponible)) {
    if (contador$n >= contador$tope) {
      return(lapply(items, function(it) {
        list(ok = FALSE, item = it, error_base = "se agot\u00f3 `max_consultas`")
      }))
    }
    contador$n <- contador$n + 1L
    contador$sql <- c(contador$sql, sql_base)
    rs <- tryCatch(.enviar_consulta_dbi(conexion, sql_base), error = function(e) e)
    if (inherits(rs, "condition")) {
      mot <- .mensaje_motor(rs)
      return(lapply(items, function(it) list(ok = FALSE, item = it, error_base = mot)))
    }
    on.exit(.limpiar_consulta_dbi(rs, conexion), add = TRUE)
    repeat {
      bloque <- tryCatch(
        .traer_filas_dbi(rs, n = as.integer(bloque_filas), conexion),
        error = function(e) e
      )
      if (inherits(bloque, "condition")) {
        mot <- .mensaje_motor(bloque)
        return(lapply(items, function(it) list(ok = FALSE, item = it, error_base = mot)))
      }
      if (!is.data.frame(bloque) || !nrow(bloque)) break
      acum <- .absorber_bloque_medicion_dbi(
        bloque, acum, ent_nombre, aplicabilidad, id_medicion, fecha,
        proteger_datos_personales, columnas_personales, validadores_personales
      )
      rm(bloque)
      if (isTRUE(tryCatch(DBI::dbHasCompleted(rs), error = function(e) FALSE))) break
    }
    return(acum)
  }
  if (identical(motor, "duckdb")) {
    rango <- tryCatch(
      .consultar_medicion_dbi(
        conexion,
        paste0("SELECT MIN(rowid) AS rmin, MAX(rowid) AS rmax FROM ", esq$sql),
        contador
      ),
      error = function(e) e
    )
    if (!inherits(rango, "error") && is.data.frame(rango) && nrow(rango) == 1L) {
      rmin <- as.numeric(rango$rmin[[1L]])
      rmax <- as.numeric(rango$rmax[[1L]])
      if (!is.na(rmin) && !is.na(rmax)) {
        paso <- as.numeric(bloque_filas)
        inicio <- rmin
        while (inicio <= rmax) {
          fin <- inicio + paso
          sql_b <- paste0(
            sql_base, " WHERE rowid >= ", format(inicio, scientific = FALSE),
            " AND rowid < ", format(fin, scientific = FALSE)
          )
          bloque <- tryCatch(
            .consultar_medicion_dbi(conexion, sql_b, contador),
            lupa_presupuesto_medicion_dbi = function(e) e,
            error = function(e) e
          )
          if (inherits(bloque, "error")) {
            mot <- if (inherits(bloque, "lupa_presupuesto_medicion_dbi")) {
              conditionMessage(bloque)
            } else {
              .mensaje_motor(bloque)
            }
            return(lapply(items, function(it) list(ok = FALSE, item = it, error_base = mot)))
          }
          if (is.data.frame(bloque) && nrow(bloque) > 0L) {
            acum <- .absorber_bloque_medicion_dbi(
              bloque, acum, ent_nombre, aplicabilidad, id_medicion, fecha,
              proteger_datos_personales, columnas_personales, validadores_personales
            )
          }
          rm(bloque)
          inicio <- fin
        }
      }
      return(acum)
    }
    # Respaldo para vistas en DuckDB sin rowid: paginado LIMIT/OFFSET.
    offset <- 0
    repeat {
      sql_b <- paste0(
        sql_base, " LIMIT ", as.integer(bloque_filas),
        " OFFSET ", format(offset, scientific = FALSE)
      )
      bloque <- tryCatch(
        .consultar_medicion_dbi(conexion, sql_b, contador),
        lupa_presupuesto_medicion_dbi = function(e) e,
        error = function(e) e
      )
      if (inherits(bloque, "error")) {
        mot <- if (inherits(bloque, "lupa_presupuesto_medicion_dbi")) {
          conditionMessage(bloque)
        } else {
          .mensaje_motor(bloque)
        }
        return(lapply(items, function(it) list(ok = FALSE, item = it, error_base = mot)))
      }
      if (!is.data.frame(bloque) || !nrow(bloque)) break
      acum <- .absorber_bloque_medicion_dbi(
        bloque, acum, ent_nombre, aplicabilidad, id_medicion, fecha,
        proteger_datos_personales, columnas_personales, validadores_personales
      )
      n_leidas <- nrow(bloque)
      rm(bloque)
      if (n_leidas < bloque_filas) break
      offset <- offset + n_leidas
    }
    return(acum)
  }
  lapply(items, function(it) {
    list(ok = FALSE, item = it,
         error_base = paste0("lectura por bloques no disponible: ", cap$motivo))
  })
}

.finalizar_bloque_acumulado <- function(a, id_medicion, fecha) {
  inst <- a$item$instancia
  if (!isTRUE(a$ok)) {
    if (!is.null(a$cobertura_directa)) {
      return(list(cobertura = a$cobertura_directa))
    }
    if (!is.null(a$error_base)) {
      return(list(cobertura = .no_medible_dbi(
        inst, id_medicion, fecha, a$error_base, en_base = TRUE
      )))
    }
    return(list(cobertura = .no_medible_dbi(
      inst, id_medicion, fecha, a$error_metodo %||% "fallo al evaluar por bloques",
      en_base = FALSE
    )))
  }
  if (!a$den) {
    return(list(cobertura = .cobertura_metrica_desde_conteos(
      inst, id_medicion, fecha, list(
        entidad_dependiente_vacia = NULL,
        entidad_presente = TRUE,
        n_recortada = a$n_rec,
        n_original = a$n_filas,
        aplicable_vacio = !is.null(inst$configuracion$aplicable) && a$n_apl == 0,
        sin_valores_atributos = length(inst$atributos) > 0L && a$n_comp == 0
      )
    )))
  }
  fila <- a$plantilla
  agreg <- fila$agregacion[[1L]]
  es_ratio <- identical(agreg, "ratio")
  fila$resultado[[1L]] <- if (es_ratio) a$num / a$den else a$sum_val / a$den
  fila$numerador[[1L]] <- if (es_ratio) as.double(a$num) else NA_real_
  fila$denominador[[1L]] <- as.double(a$den)
  fila$n_universo[[1L]] <- as.double(a$univ)
  no <- a$univ - a$den
  if (no < 0) {
    fila$n_no_evaluados[[1L]] <- NA_real_
    fila$motivo_no_evaluados[[1L]] <- "medidas_repetidas"
  } else {
    fila$n_no_evaluados[[1L]] <- as.double(no)
    fila$motivo_no_evaluados[[1L]] <- if (!no) {
      NA_character_
    } else if (a$sin_valor == no) {
      "sin_valor"
    } else {
      "sin_medida"
    }
  }
  al <- .alcance_medidas_desde_conteos(inst, a$univ, a$den, a$sin_valor)
  list(fila = as.data.frame(unclass(fila), stringsAsFactors = FALSE)[.COLUMNAS_RESUMEN],
       alcance = al)
}

.columnas_personales_esquemas_dbi <- function(esquemas, declaradas, validadores) {
  unique(unlist(lapply(esquemas, function(esq) {
    if (is.null(esq) || !inherits(esq$prototipo, "data.frame") ||
        !ncol(esq$prototipo)) {
      return(character())
    }
    tryCatch(
      .clasificar_columnas_personales_tabla(
        esq$prototipo, detalle = FALSE,
        declaradas = declaradas, validadores = validadores
      ),
      error = function(e) character()
    )
  }), use.names = FALSE))
}

# ---------------------------------------------------------------------------
# 7. Funciones exportadas
# ---------------------------------------------------------------------------

#' Planificar una medición de calidad en una base de datos DBI
#'
#' Clasifica cada métrica del modelo según la vía con que se medirá en el motor
#' (`sql_consolidado`, `sql_dedicado`, `sql_dedicado_referencial`, `bloques_r`,
#' `no_medible`, `no_medible_metodo` o `abstencion`) y estima las consultas
#' necesarias, sin leer filas de ninguna tabla.
#'
#' Sólo consulta el esquema (`SELECT * ... WHERE 1 = 0` y metadatos del motor).
#' No recibe `proteger_datos_personales` y hereda la protección de quien lo
#' llama, igual que [plan_perfilado_dbi()].
#'
#' @param modelo Modelo de calidad creado por [modelo()].
#' @param conexion Conexión DBI activa o colección creada por [coleccion()].
#' @param tablas Mapeo opcional de nombres de entidades del modelo a nombres de
#'   tablas físicas (vector de texto, lista con nombre o [DBI::Id()]). Si es
#'   `NULL`, cada entidad se busca por su nombre (o en `coleccion()`).
#' @param aplicabilidad Lista con nombre de fórmulas de un solo lado por
#'   columna, igual que en [medir()].
#' @param estrategia_no_sql Cómo resolver las métricas que no se compilan a SQL:
#'   `"no_medible"` (por omisión, no lee filas) o `"bloques"` (lee por bloques
#'   sólo las columnas necesarias para las métricas separables por fila).
#' @param bloque_filas Cantidad de filas por bloque cuando
#'   `estrategia_no_sql = "bloques"`. `NULL` elige el tamaño automático.
#' @param tamano_lote Tope opcional de métricas por consulta consolidada. `NULL`
#'   usa el límite del motor.
#' @param max_valores_in Tope de valores en listas `IN (...)` compiladas a SQL.
#' @param max_consultas Presupuesto máximo de consultas emitidas durante la
#'   ejecución, incluidas las consultas de esquema.
#' @param dialecto Dialecto SQL: `"auto"` (por omisión) o uno de `"sqlite"`,
#'   `"duckdb"`, `"postgresql"`, `"mysql"`, `"mariadb"`, `"sqlserver"`.
#'
#' @return Un `data.frame` de clase `plan_medicion_dbi` con una fila por
#'   métrica instanciada (`metrica_instanciada`, `metrica`, `entidad`,
#'   `atributos`, `via`, `motivo`, `columnas_bloque`) y atributos `motor` y
#'   `consultas` (`esquema`, `consolidadas`, `dedicadas`, `bloques`).
#' @export
#' @seealso [medir_dbi()], [medir()], [plan_perfilado_dbi()]
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
#'   mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
#'   plan_medicion_dbi(mod, con)
#'   DBI::dbDisconnect(con)
#' }
plan_medicion_dbi <- function(modelo, conexion, tablas = NULL,
                              aplicabilidad = NULL,
                              estrategia_no_sql = c("no_medible", "bloques"),
                              bloque_filas = NULL, tamano_lote = NULL,
                              max_valores_in = 500L, max_consultas = Inf,
                              dialecto = "auto") {
  .fijar_numerica_c()
  estrategia_no_sql <- .elegir_opcion(estrategia_no_sql)
  args <- .validar_argumentos_entrada_medir_dbi(
    modelo, conexion, tablas, max_valores_in, max_consultas,
    tamano_lote, bloque_filas
  )
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  .fijar_senas_conexion_dbi(con)
  .fijar_proteccion_motor(.protege_mensaje_motor())
  motor <- .motor_medicion_dbi(con, dialecto)
  p <- .planificar_medicion_dbi(
    modelo = modelo, conexion = conexion, tablas = tablas,
    aplicabilidad = aplicabilidad, motor = motor,
    max_valores_in = args$max_valores_in,
    estrategia_no_sql = estrategia_no_sql,
    tamano_lote = args$tamano_lote
  )
  filas <- lapply(p$planes, function(x) {
    data.frame(
      metrica_instanciada = x$instancia$nombre,
      metrica = x$instancia$declaracion$nombre,
      entidad = paste(x$instancia$entidad, collapse = "+"),
      atributos = paste(x$instancia$atributos, collapse = "+"),
      via = x$via,
      motivo = if (is.null(x$motivo)) NA_character_ else as.character(x$motivo),
      columnas_bloque = if (identical(x$via, "bloques_r")) {
        paste(x$columnas_bloque, collapse = ", ")
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  })
  plan <- if (length(filas)) {
    do.call(rbind, filas)
  } else {
    data.frame(
      metrica_instanciada = character(),
      metrica = character(),
      entidad = character(),
      atributos = character(),
      via = character(),
      motivo = character(),
      columnas_bloque = character(),
      stringsAsFactors = FALSE
    )
  }
  rownames(plan) <- NULL
  attr(plan, "motor") <- motor
  attr(plan, "consultas") <- c(
    esquema = as.integer(p$n_esquema),
    consolidadas = as.integer(p$n_consolidadas),
    dedicadas = as.integer(sum(plan$via %in% c("sql_dedicado", "sql_dedicado_referencial"))),
    bloques = as.integer(sum(plan$via == "bloques_r"))
  )
  class(plan) <- c("plan_medicion_dbi", "data.frame")
  plan
}

#' @export
print.plan_medicion_dbi <- function(x, ...) {
  visible <- .marcar_objeto_para_exhibir(x)
  motor <- attr(x, "motor", exact = TRUE)
  consultas <- attr(x, "consultas", exact = TRUE)
  cli::cli_h1("Plan de medici\u00f3n DBI")
  if (!is.null(motor) && !is.null(consultas)) {
    cli::cli_alert_info(.cli_literal(paste0(
      "Motor: ", motor, " | ", nrow(visible),
      if (nrow(visible) == 1L) " m\u00e9trica" else " m\u00e9tricas",
      " (esquema: ", consultas[["esquema"]],
      ", consolidadas: ", consultas[["consolidadas"]],
      ", dedicadas: ", consultas[["dedicadas"]],
      ", bloques: ", consultas[["bloques"]], ")"
    )))
  }
  tabla <- as.data.frame(unclass(visible), stringsAsFactors = FALSE)
  .print_data_frame_bytes(tabla, row.names = FALSE, ...)
  invisible(x)
}

#' Medir un modelo de calidad dentro de una base de datos DBI
#'
#' Evalúa un modelo de calidad directamente en el motor SQL (SQLite y DuckDB
#' verificados; los demás motores devuelven `no_medible` con su motivo según la
#' política de motores) y devuelve un resumen de clase `medicion_resumen` con el
#' mismo contrato de columnas que `medir(..., detalle = "resumen")`.
#'
#' Lo que el motor no puede reproducir con la semántica exacta de R se declara
#' `no_medible` en `attr(resultado, "cobertura_metricas")` —nunca se aproxima
#' ni se interpreta como cero— o, cuando la métrica es separable por fila y se
#' indica `estrategia_no_sql = "bloques"`, se mide en R proyectando sólo sus
#' columnas y leyendo por bloques.
#'
#' En la medición en memoria, `aplicable` y `aplicabilidad` conservan la
#' evaluación publicada de [medir()]; el resumen resuelve cada
#' `aplicabilidad` una sola vez, igual que la medición completa. En la vía DBI,
#' una fórmula traducible sólo puede nombrar columnas de la entidad y
#' literales escritos en la propia fórmula. Un símbolo externo se declara
#' `no_medible` sin consultar ni forzar su entorno.
#'
#' La marca `datos_personales_sin_proteger` (cuando
#' `proteger_datos_personales = FALSE`) se determina por nombre de columna y por
#' `columnas_personales`, sin leer filas de la base.
#'
#' @inheritParams plan_medicion_dbi
#' @param id_medicion Identificador de la corrida. Si es `NULL`, se genera uno.
#' @param fecha Fecha de la corrida (UTC).
#' @param proteger_datos_personales Si es `TRUE`, protege los mensajes del motor
#'   y no agrega la marca `datos_personales_sin_proteger`.
#' @param columnas_personales Columnas declaradas como datos personales, igual
#'   que en [medir()].
#' @param validadores_personales Validadores adicionales de datos personales,
#'   igual que en [medir()].
#' @param instrumentar Si es `TRUE` (por omisión), conserva las consultas SQL y
#'   el tiempo transcurrido en `attr(resultado, "ejecucion_dbi")`.
#'
#' @return Un `data.frame` de clase `medicion_resumen` (sin heredar de
#'   `medicion`) con las 23 columnas del resumen y los atributos de
#'   `medir(detalle = "resumen")` más `ejecucion_dbi`.
#' @export
#' @seealso [plan_medicion_dbi()], [medir()], [perfilar_dbi()],
#'   [tablero_calidad()], [indice_calidad()]
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
#'   mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
#'   res <- medir_dbi(mod, con)
#'   tablero_calidad(res)
#'   DBI::dbDisconnect(con)
#' }
medir_dbi <- function(modelo, conexion, tablas = NULL,
                      id_medicion = NULL, fecha = Sys.time(),
                      aplicabilidad = NULL,
                      estrategia_no_sql = c("no_medible", "bloques"),
                      bloque_filas = NULL, tamano_lote = NULL,
                      max_valores_in = 500L, max_consultas = Inf,
                      dialecto = "auto",
                      proteger_datos_personales = FALSE,
                      columnas_personales = character(),
                      validadores_personales = NULL,
                      instrumentar = TRUE) {
  .fijar_numerica_c()
  estrategia_no_sql <- .elegir_opcion(estrategia_no_sql)
  args <- .validar_argumentos_entrada_medir_dbi(
    modelo, conexion, tablas, max_valores_in, max_consultas,
    tamano_lote, bloque_filas
  )
  fecha_declarada <- !missing(fecha)
  fecha <- .fecha_argumento_utc(fecha)
  .validar_bandera_proteccion(proteger_datos_personales)
  if (!is.logical(instrumentar) || length(instrumentar) != 1L || is.na(instrumentar)) {
    .detener("`instrumentar` debe ser TRUE o FALSE.", call. = FALSE)
  }
  validadores_personales <- .normalizar_validadores_personales(validadores_personales)
  if (is.null(id_medicion)) {
    id_medicion <- .nuevo_id_medicion(fecha)
  }
  if (!.es_texto_escalar(id_medicion)) {
    .detener("`id_medicion` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  .fijar_senas_conexion_dbi(con)
  .fijar_proteccion_motor(proteger_datos_personales)

  inicio <- proc.time()[["elapsed"]]
  motor <- .motor_medicion_dbi(con, dialecto)
  contador <- new.env(parent = emptyenv())
  contador$n <- 0L
  contador$tope <- args$max_consultas
  contador$sql <- character()
  p <- .planificar_medicion_dbi(
    modelo = modelo, conexion = conexion, tablas = tablas,
    aplicabilidad = aplicabilidad, motor = motor,
    max_valores_in = args$max_valores_in,
    estrategia_no_sql = estrategia_no_sql,
    tamano_lote = args$tamano_lote,
    contador = contador
  )
  nombres_disponibles <- unique(unlist(
    lapply(Filter(Negate(is.null), p$esquemas), `[[`, "campos"),
    use.names = FALSE
  ))
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales,
    if (length(nombres_disponibles)) nombres_disponibles else NULL
  )

  universos <- .universos_medicion_dbi(
    aplicabilidad, Filter(Negate(is.null), p$esquemas),
    motor, con, args$max_valores_in
  )
  for (k in seq_along(p$planes)) {
    x <- p$planes[[k]]
    if (!x$via %in% c("sql_consolidado", "sql_dedicado")) next
    esq <- p$esquemas[[x$entidad]]
    U <- universos[[x$entidad]](x$instancia)
    x$universo_aplicabilidad <- U$sql
    x$universo_dentro <- if (identical(x$spec$nodo, "no_ausente")) {
      x$universo
    } else {
      U$sql
    }
    atr <- intersect(x$columnas_completas, esq$campos)
    x$completas <- if (length(atr)) {
      .sql_y(vapply(atr, function(a) {
        paste0("(", .columna_sql_medicion(con, esq, a, motor), " IS NOT NULL)")
      }, character(1L)))
    } else ""
    gran <- as.character(x$instancia$declaracion$granularidad)[[1L]]
    if (identical(gran, "instanciaEntidad") && length(x$instancia$atributos)) {
      x$sin_valor <- paste0(
        "(",
        paste(vapply(x$instancia$atributos, function(a) {
          paste0("(", .columna_sql_medicion(con, esq, a, motor), " IS NULL)")
        }, character(1L)), collapse = " OR "),
        ")"
      )
    } else if (identical(gran, "instanciaEntidad")) {
      x$sin_valor <- NULL
    }
    p$planes[[k]] <- x
  }

  senas <- .senas_conexion_dbi(con)
  resultados <- vector("list", length(p$planes))
  for (e in names(p$esquemas)) {
    if (is.null(p$esquemas[[e]])) next
    items <- Filter(
      function(x) x$via %in% c("sql_consolidado", "sql_dedicado") &&
        identical(x$entidad, e),
      p$planes
    )
    if (!length(items)) next
    lotes <- .lotes_consolidada_medicion(items, senas, args$tamano_lote)
    for (lote in lotes) {
      r <- .consolidada_medicion_dbi(con, p$esquemas[[e]], lote, contador)
      for (it in lote) {
        j <- it$j
        fallo <- if (!is.null(r$fallo)) r$fallo[as.character(j)] else NA_character_
        if (!is.na(fallo)) {
          resultados[[j]] <- list(error = unname(fallo))
          next
        }
        v <- function(s) {
          nm <- sprintf("m%d_%s", j, s)
          if (!is.null(r$ok) && nm %in% names(r$ok)) as.numeric(r$ok[[nm]]) else NA_real_
        }
        res <- list(
          n_filas = as.numeric(r$ok$n_filas),
          rec = v("rec"), univ = v("univ"), den = v("den"), num = v("num"),
          sv = v("sv"), apl = v("apl"), comp = v("comp"), g = v("g"), na = v("na")
        )
        if (identical(it$via, "sql_dedicado")) {
          q <- tryCatch(
            .consultar_medicion_dbi(con, it$consulta, contador),
            lupa_presupuesto_medicion_dbi = function(e) e,
            error = function(e) e
          )
          if (inherits(q, "error")) {
            msg <- if (inherits(q, "lupa_presupuesto_medicion_dbi")) {
              conditionMessage(q)
            } else {
              .mensaje_motor(q)
            }
            resultados[[j]] <- list(error = msg)
            next
          }
          res$num <- as.numeric(q$num)
          res$den <- as.numeric(q$den)
        }
        resultados[[j]] <- res
      }
    }
  }

  filas <- list()
  coberturas <- list()
  alcances <- list()
  vias_finales <- vapply(p$planes, `[[`, character(1L), "via")
  pendientes_bloques <- list()

  for (x in p$planes) {
    instancia <- x$instancia
    j <- x$j
    cob <- function(fila) coberturas[[length(coberturas) + 1L]] <<- fila
    if (identical(x$via, "abstencion")) {
      cob(.cobertura_metrica_abstenida(instancia, id_medicion, fecha, x$abstencion))
      next
    }
    if (identical(x$via, "no_medible_metodo")) {
      cob(.no_medible_dbi(instancia, id_medicion, fecha, x$motivo, en_base = FALSE))
      next
    }
    if (identical(x$via, "no_medible")) {
      cob(.no_medible_dbi(instancia, id_medicion, fecha, x$motivo, en_base = TRUE))
      next
    }
    if (identical(x$via, "bloques_r")) {
      pendientes_bloques[[length(pendientes_bloques) + 1L]] <- x
      next
    }
    if (identical(x$via, "sql_dedicado_referencial")) {
      q <- tryCatch(
        .consultar_medicion_dbi(con, x$consulta, contador),
        lupa_presupuesto_medicion_dbi = function(e) e,
        error = function(e) e
      )
      if (inherits(q, "error")) {
        msg <- if (inherits(q, "lupa_presupuesto_medicion_dbi")) {
          conditionMessage(q)
        } else {
          .mensaje_motor(q)
        }
        cob(.no_medible_dbi(instancia, id_medicion, fecha, msg, en_base = TRUE))
        next
      }
      if (!is.null(q$guarda) && as.numeric(q$guarda) > 0) {
        cob(.no_medible_dbi(
          instancia, id_medicion, fecha,
          paste(
            "SQLite no valida UTF-8 y la clave tiene texto no ASCII: no se puede",
            "garantizar que el motor lea los mismos valores que R"
          ),
          en_base = TRUE
        ))
        next
      }
      univ <- as.numeric(q$univ)
      den <- as.numeric(q$den)
      num <- as.numeric(q$num)
      sv <- as.numeric(q$sin_valor)
      if (!univ) {
        cob(.cobertura_metrica_desde_conteos(
          instancia, id_medicion, fecha, list(
            entidad_dependiente_vacia = instancia$entidad[[2L]],
            entidad_presente = TRUE,
            n_recortada = 0,
            n_original = as.numeric(q$n_filas),
            aplicable_vacio = FALSE,
            sin_valores_atributos = FALSE
          )
        ))
        next
      }
      if (!den) {
        cob(.no_medible_dbi(
          instancia, id_medicion, fecha,
          "Una m\u00e9trica real debe devolver valores finitos en [0, 1].",
          en_base = FALSE,
          causa = ": su m\u00e9todo devolvi\u00f3 una salida que no cumple el contrato"
        ))
        next
      }
      no <- univ - den
      filas[[length(filas) + 1L]] <- .fila_resumen(
        instancia, id_medicion, fecha, x$spec$entidad_dep, x$spec$fk,
        .pegar_texto0(
          x$spec$entidad_dep, "$", x$spec$fk, " -> ",
          x$spec$entidad_ref, "$", x$spec$pk
        ),
        resultado = num / den,
        agregacion = "promedio",
        tipo_resultado = "real",
        numerador = num,
        denominador = den,
        n_universo = univ,
        n_no_evaluados = no,
        motivo_no_evaluados = if (!no) NA_character_ else if (sv == no)
          "sin_valor" else "sin_medida"
      )
      next
    }
    res <- resultados[[j]]
    if (!is.null(res$error)) {
      cob(.no_medible_dbi(instancia, id_medicion, fecha, res$error, en_base = TRUE))
      next
    }
    if (!is.na(res$g) && res$g > 0) {
      motivo_g <- paste(
        "SQLite no valida UTF-8 y la columna tiene texto no ASCII: no se puede",
        "garantizar que el motor lea los mismos valores que R"
      )
      if (identical(estrategia_no_sql, "bloques") &&
          .separable_medicion_dbi(instancia, x$spec) &&
          length(instancia$entidad) == 1L &&
          length(x$columnas_bloque) > 0L) {
        x$via <- "bloques_r"
        x$motivo <- motivo_g
        vias_finales[[j]] <- "bloques_r"
        pendientes_bloques[[length(pendientes_bloques) + 1L]] <- x
        next
      }
      cob(.no_medible_dbi(instancia, id_medicion, fecha, motivo_g, en_base = TRUE))
      next
    }
    if (isTRUE(x$no_logico) || (!is.na(res$na) && res$na > 0)) {
      cob(.no_medible_dbi(
        instancia, id_medicion, fecha,
        paste0(
          "El validador de ReglaIntegridadIntraEntidad debe devolver un vector ",
          "l\u00f3gico sin NA de longitud ", res$rec, "."
        ),
        en_base = FALSE
      ))
      next
    }
    if (!res$den) {
      cob(.cobertura_metrica_desde_conteos(
        instancia, id_medicion, fecha, list(
          entidad_dependiente_vacia = NULL,
          entidad_presente = TRUE,
          n_recortada = res$rec,
          n_original = res$n_filas,
          aplicable_vacio = !is.null(instancia$configuracion$aplicable) && res$apl == 0,
          sin_valores_atributos = length(instancia$atributos) > 0L && res$comp == 0
        )
      ))
      next
    }
    gran <- as.character(instancia$declaracion$granularidad)[[1L]]
    base_univ <- if (identical(x$spec$nodo, "no_ausente")) res$univ else res$rec
    esperadas <- if (identical(gran, "instanciaAtributo")) {
      base_univ * length(instancia$atributos)
    } else {
      base_univ
    }
    no <- esperadas - res$den
    sv <- if (identical(gran, "instanciaEntidad") && !length(instancia$atributos)) {
      NA_real_
    } else {
      res$sv
    }
    al <- .alcance_medidas_desde_conteos(instancia, esperadas, res$den, sv)
    if (!is.null(al)) alcances[[length(alcances) + 1L]] <- al
    atributo <- if (identical(x$spec$objeto, "celda")) {
      x$spec$columna %||% x$spec$columnas
    } else {
      NA_character_
    }
    objeto <- if (identical(x$spec$objeto, "celda")) {
      .pegar_texto0(x$spec$entidad, "$", atributo)
    } else {
      x$spec$entidad
    }
    filas[[length(filas) + 1L]] <- .fila_resumen(
      instancia, id_medicion, fecha, x$spec$entidad, atributo, objeto,
      resultado = res$num / res$den,
      agregacion = "ratio",
      tipo_resultado = "real",
      numerador = res$num,
      denominador = res$den,
      n_universo = esperadas,
      n_no_evaluados = no,
      motivo_no_evaluados = if (!no) NA_character_ else if (!is.na(sv) && sv == no)
        "sin_valor" else "sin_medida"
    )
  }

  # Ejecutar metricas por bloques agrupadas por entidad (una pasada por entidad).
  if (length(pendientes_bloques)) {
    entidades_b <- unique(vapply(pendientes_bloques, `[[`, character(1L), "entidad"))
    for (eb in entidades_b) {
      items_b <- Filter(function(x) identical(x$entidad, eb), pendientes_bloques)
      acums <- .ejecutar_bloques_entidad_medicion(
        conexion = con, esq = p$esquemas[[eb]], items = items_b,
        motor = motor, bloque_filas = args$bloque_filas,
        aplicabilidad = aplicabilidad, id_medicion = id_medicion, fecha = fecha,
        proteger_datos_personales = proteger_datos_personales,
        columnas_personales = columnas_personales,
        validadores_personales = validadores_personales,
        contador = contador
      )
      for (a in acums) {
        fin <- .finalizar_bloque_acumulado(a, id_medicion, fecha)
        if (!is.null(fin$fila)) filas[[length(filas) + 1L]] <- fin$fila
        if (!is.null(fin$cobertura)) coberturas[[length(coberturas) + 1L]] <- fin$cobertura
        if (!is.null(fin$alcance)) alcances[[length(alcances) + 1L]] <- fin$alcance
      }
    }
  }

  orden_modelo <- vapply(modelo$metricas, `[[`, character(1L), "nombre")
  resultado <- if (length(filas)) do.call(rbind, filas) else .resumen_vacio()
  if (nrow(resultado)) {
    resultado <- resultado[
      .orden_por_bytes(match(resultado$metrica_instanciada, orden_modelo)),
      , drop = FALSE
    ]
    rownames(resultado) <- NULL
    resultado$id_medida <- .ids_medida(
      resultado$id_medicion, resultado$metrica_instanciada, "resumen-"
    )
  }
  resultado <- resultado[.COLUMNAS_RESUMEN]
  if (length(coberturas)) {
    cobertura <- do.call(rbind, coberturas)
    cobertura <- cobertura[
      .orden_por_bytes(match(cobertura$metrica_instanciada, orden_modelo)),
      , drop = FALSE
    ]
    rownames(cobertura) <- NULL
    attr(resultado, "cobertura_metricas") <- cobertura
    .avisar_no_medibles(
      cobertura[cobertura$estado != "no_resumible", , drop = FALSE],
      length(modelo$metricas)
    )
  }
  if (length(alcances)) {
    al <- do.call(rbind, alcances)
    al <- al[
      .orden_por_bytes(match(al$metrica_instanciada, orden_modelo)),
      , drop = FALSE
    ]
    rownames(al) <- NULL
    attr(resultado, "alcance_medidas") <- al
  }
  attr(resultado, "fecha_declarada") <- fecha_declarada
  attr(resultado, "configuracion_modelo") <- .configuracion_modelo_calidad(modelo)
  if (inherits(modelo$marco, "marco_calidad")) {
    attr(resultado, "marco_calidad") <- modelo$marco
  }
  attr(resultado, "configuracion_aplicabilidad") <-
    .texto_configuracion_calidad(aplicabilidad)
  attr(resultado, "detalle") <- "resumen"
  if (!isTRUE(proteger_datos_personales) && nrow(resultado)) {
    personales <- .columnas_personales_esquemas_dbi(
      p$esquemas, columnas_personales, validadores_personales
    )
    if (length(personales)) {
      attr(resultado, "datos_personales_sin_proteger") <- personales
    }
  }
  attr(resultado, "ejecucion_dbi") <- list(
    motor = motor,
    consultas = contador$n,
    vias = stats::setNames(vias_finales, orden_modelo),
    sql = if (isTRUE(instrumentar)) contador$sql else NULL,
    segundos = if (isTRUE(instrumentar)) proc.time()[["elapsed"]] - inicio else NULL
  )
  class(resultado) <- c("medicion_resumen", "data.frame")
  resultado
}
