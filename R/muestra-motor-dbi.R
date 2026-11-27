# Muestra reproducible en el motor por huella de la clave y verificacion de
# estabilidad de la tabla durante la lectura DBI.
#
# Cuando `perfilar_dbi()` recorta una muestra sin `orden_muestra`, SQL no
# garantiza el orden de las filas y dos corridas sobre la misma tabla pueden
# leer conjuntos distintos. Si la tabla tiene una clave primaria o unica de
# grado total en `meta$catalogo`, o el usuario declara `clave =`, se ordena por
# una huella determinista de esa clave calculada en el motor (con filtro previo
# sobremuestreado por umbral en tablas grandes para evitar derrame a disco en
# el ordenamiento). La codificacion canonica usa UTF-8 y largo en bytes,
# decimales sin ceros de escala, fechas ISO y UUID en minusculas para lograr la
# misma seleccion entre los motores con MD5 cuando todos los tipos de la clave
# son canonicos; donde no se puede garantizar equivalencia entre motores (SQLite,
# SQL Server anterior a 2019 o tipos dependientes del motor), se publica
# `alcance_reproducibilidad = "motor"`. Sin clave disponible, se declara el
# motivo y se conserva el comportamiento sin orden garantizado.
#
# Para estabilidad, se reusan los contadores sincronos de modificaciones del
# motor cuando existen y estan activos (PostgreSQL `xmin`, SQL Server
# `rowversion` o `sys.dm_db_index_operational_stats`, MySQL `performance_schema`
# con instrumentos/consumidores/objetos verificados, MariaDB
# `INNODB_SYS_TABLESTATS` y `UPDATE_TIME`) capturados en la primera consulta
# consolidada y contrastados en el cierre mas barato posible (solo contador sin
# barrer la tabla en SQL Server DMV y MySQL `performance_schema`; `COUNT(*)` y
# `MAX(xmin)` en PostgreSQL). Donde no hay contador activo, el cierre consulta
# unicamente `COUNT(*)` y, si existe una clave indexada de una columna con
# maximo ya medido, `MAX(clave)` como subconsulta escalar, sin barrer columnas
# no indexadas; nunca publica `FALSE` como garantia de ausencia de cambios: el
# estado es `sin_cambio_en_los_controles` con `controles` y `cambio_detectado = NA`.

.METODO_MUESTRA_HUELLA_MOTOR <- "muestra_huella_motor"
.UMBRAL_FILAS_PREFILTRO_HUELLA_DBI <- 10000
.CLASE_PLAN_ESTABILIDAD_DBI <- "verificacion de estabilidad (conteo y agregado final)"
.CLASE_PLAN_VERIFICACION_CLAVE_DBI <- "verificacion de unicidad de la clave de muestra"
.COLACION_UTF8_MSSQL_DBI <- "Latin1_General_100_BIN2_UTF8"
.COLACION_PUENTE_MSSQL_DBI <- "Latin1_General_BIN"
.TIPOS_CANONICOS_ENTRE_MOTORES_DBI <- c("entero", "decimal", "fecha", "uuid", "texto")

# Identificador canonico de la tabla usando `.identificadores_tabla()` para que
# catalogo, esquema y tabla no colapsen ni se formateen de manera distinta.
.identificador_tabla_dbi <- function(tabla) {
  if (isTRUE(.es_id_dbi(tabla))) {
    piezas <- .partes_id_dbi(tabla)
    cat_val <- if ("catalogo" %in% names(piezas)) piezas[["catalogo"]] else NA_character_
    esq_val <- if ("esquema" %in% names(piezas)) piezas[["esquema"]] else NA_character_
    tab_val <- unname(piezas[["tabla"]])
    return(.identificadores_tabla(esq_val, tab_val, catalogo = cat_val)[[1L]])
  }
  piezas <- .piezas_tabla_cardinalidad_dbi(tabla)
  if (!is.na(piezas$tabla) && nzchar(piezas$tabla)) {
    return(.identificadores_tabla(piezas$esquema, piezas$tabla)[[1L]])
  }
  .texto_tabla_dbi(tabla)
}

# Valida `clave` en la via DBI con las mismas exigencias que `perfilar()` y
# `orden_muestra`, devolviendo los nombres con la grafia del esquema.
.validar_clave_muestra_dbi <- function(clave, campos, esquema = NULL) {
  if (is.null(clave)) return(character())
  if (!is.character(clave) || !length(clave) || anyNA(clave) ||
      !all(nzchar(clave))) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`clave` debe ser un vector de nombres de columna sin NA."
    )
  }
  pos <- .resolver_columnas_dbi(clave, campos)
  if (anyNA(pos)) {
    faltan <- clave[is.na(pos)]
    negadas <- if (is.list(esquema) && length(esquema$sin_permiso)) {
      faltan[.nombres_presentes(faltan, esquema$sin_permiso)]
    } else character()
    if (length(negadas)) {
      .detener_dbi("lupa_error_esquema_dbi", paste0(
        "No se puede usar `clave` para muestrear: la credencial no tiene ",
        "permiso SELECT sobre ", paste(negadas, collapse = ", "), "."
      ))
    }
    .detener_dbi("lupa_error_argumento_dbi", paste0(
      "`clave` nombra columnas que no est\u00e1n en la tabla: ",
      paste(faltan, collapse = ", "), "."
    ))
  }
  resuelta <- campos[pos]
  if (anyDuplicated(.nombres_para_operar(resuelta))) {
    .detener_dbi("lupa_error_argumento_dbi", "`clave` repite una columna.")
  }
  as.character(resuelta)
}

.motor_huella_muestra_dbi <- function(conexion) {
  senas <- .senas_conexion_dbi(conexion)
  if (grepl("postgres|pqconnection", senas, perl = TRUE, useBytes = TRUE)) {
    return("postgresql")
  }
  if (grepl("duckdb", senas, fixed = TRUE, useBytes = TRUE)) {
    return("duckdb")
  }
  if (grepl("mysql|mariadb", senas, perl = TRUE, useBytes = TRUE)) {
    return("mysql")
  }
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas,
            perl = TRUE, useBytes = TRUE)) {
    return("sqlserver")
  }
  if (grepl("sqlite", senas, fixed = TRUE, useBytes = TRUE)) {
    return("sqlite")
  }
  "desconocido"
}

.funcion_hash_motor_dbi <- function(motor_hash) {
  switch(
    motor_hash,
    postgresql = "md5",
    duckdb = "md5",
    mysql = "md5",
    sqlserver = "hashbytes_md5",
    sqlite = "huella_sql_sqlite",
    NA_character_
  )
}

.tipo_admite_huella_motor_dbi <- function(tipo_motor, tipo_declarado,
                                          motor_hash) {
  candidato <- if (length(tipo_motor) == 1L && !is.na(tipo_motor) &&
                   nzchar(as.character(tipo_motor))) {
    as.character(tipo_motor)
  } else if (length(tipo_declarado) == 1L && !is.na(tipo_declarado) &&
             nzchar(as.character(tipo_declarado))) {
    as.character(tipo_declarado)
  } else {
    NA_character_
  }
  if (is.na(candidato)) return(TRUE)
  base <- .minusculas_ascii(trimws(candidato))
  if (identical(motor_hash, "sqlserver") &&
      isTRUE(.tipo_sqlserver_sin_agregados_dbi(candidato))) {
    return(FALSE)
  }
  if (grepl("^(struct|map|union|list)\\b|\\[\\]$", base,
            perl = TRUE, useBytes = TRUE)) {
    return(FALSE)
  }
  if (grepl("^(geometry|geography|xml|jsonb?)$",
            sub("\\s*\\(.*$", "", base, perl = TRUE),
            perl = TRUE, useBytes = TRUE) &&
      !identical(motor_hash, "postgresql")) {
    return(FALSE)
  }
  TRUE
}

# Clasifica el tipo de cada columna de la clave para elegir su serializacion
# canonica y saber con honestidad si la huella coincide entre motores MD5 o
# solo garantiza reproducibilidad dentro de este motor.
.clasificar_tipo_canonico_huella_dbi <- function(tipo_motor, tipo_declarado) {
  candidato <- if (length(tipo_motor) == 1L && !is.na(tipo_motor) &&
                   nzchar(trimws(as.character(tipo_motor)))) {
    as.character(tipo_motor)
  } else if (length(tipo_declarado) == 1L && !is.na(tipo_declarado) &&
             nzchar(trimws(as.character(tipo_declarado)))) {
    as.character(tipo_declarado)
  } else {
    NA_character_
  }
  if (is.na(candidato)) return("otro")
  base <- .minusculas_ascii(trimws(candidato))
  base <- sub("\\s*\\(.*$", "", base, perl = TRUE)
  base <- sub("\\s+unsigned$", "", base, perl = TRUE)
  base <- trimws(base)
  if (grepl("^(uuid|uniqueidentifier)$", base, perl = TRUE, useBytes = TRUE)) {
    return("uuid")
  }
  if (grepl("^date$", base, perl = TRUE, useBytes = TRUE)) {
    return("fecha")
  }
  if (grepl("^(decimal|numeric|dec|fixed|money|smallmoney)$", base,
            perl = TRUE, useBytes = TRUE)) {
    return("decimal")
  }
  if (grepl("^(tinyint|smallint|mediumint|int|integer|bigint|hugeint|uhugeint|int2|int4|int8|serial|smallserial|bigserial|utinyint|usmallint|uinteger|ubigint)$",
            base, perl = TRUE, useBytes = TRUE)) {
    return("entero")
  }
  if (grepl("^(text|varchar|nvarchar|character varying|character|char|nchar|bpchar|tinytext|mediumtext|longtext|citext|string|clob|nclob)$",
            base, perl = TRUE, useBytes = TRUE)) {
    return("texto")
  }
  "otro"
}

.tipos_canonicos_clave_dbi <- function(clave_cols, campos,
                                       tipos_motor = NULL,
                                       tipos_declarados = NULL) {
  if (!length(clave_cols)) return(character())
  pos <- .resolver_columnas_dbi(clave_cols, campos)
  vapply(seq_along(clave_cols), function(k) {
    col_k <- clave_cols[[k]]
    idx_k <- pos[[k]]
    tm <- if (!is.na(idx_k)) {
      .atributo_columna_dbi(tipos_motor, col_k, idx_k) %||% NA_character_
    } else NA_character_
    td <- if (!is.na(idx_k)) {
      .atributo_columna_dbi(tipos_declarados, col_k, idx_k) %||% NA_character_
    } else NA_character_
    .clasificar_tipo_canonico_huella_dbi(tm, td)
  }, character(1L), USE.NAMES = FALSE)
}

# Resuelve que clave se usa para la muestra reproducible en el motor cuando no
# se pidio `orden_muestra`: primero `clave =` declarada, luego PK de grado total
# del catalogo y luego la primera restriccion o indice unico de grado total.
.resolver_clave_muestra_motor_dbi <- function(conexion, catalogo_garantias,
                                              clave_declarada, campos,
                                              enmascaradas = character(),
                                              tipos_motor = NULL,
                                              tipos_declarados = NULL) {
  motor_hash <- .motor_huella_muestra_dbi(conexion)
  funcion_hash <- .funcion_hash_motor_dbi(motor_hash)
  vacio <- function(motivo, origen = NA_character_, columnas = character()) {
    list(
      disponible = FALSE,
      columnas = columnas,
      origen = origen,
      nombre_indice = NA_character_,
      garantizada_catalogo = FALSE,
      requiere_verificacion = FALSE,
      motor_hash = motor_hash,
      funcion_hash = funcion_hash,
      motivo = motivo
    )
  }
  cols_usables <- function(cols) {
    if (!length(cols)) return(list(ok = FALSE, motivo = "Sin columnas de clave."))
    pos <- .resolver_columnas_dbi(cols, campos)
    if (anyNA(pos)) {
      return(list(
        ok = FALSE,
        motivo = "Alguna columna de la clave no est\u00e1 disponible para lectura."
      ))
    }
    resueltas <- campos[pos]
    if (length(enmascaradas) && any(.nombres_presentes(resueltas, enmascaradas))) {
      return(list(
        ok = FALSE,
        motivo = paste(
          "La clave contiene columnas enmascaradas y no puede usarse para",
          "ordenar la muestra en el motor."
        )
      ))
    }
    for (k in seq_along(resueltas)) {
      col_k <- resueltas[[k]]
      idx_k <- pos[[k]]
      tm <- .atributo_columna_dbi(tipos_motor, col_k, idx_k) %||% NA_character_
      td <- .atributo_columna_dbi(tipos_declarados, col_k, idx_k) %||% NA_character_
      if (!isTRUE(.tipo_admite_huella_motor_dbi(tm, td, motor_hash))) {
        return(list(
          ok = FALSE,
          motivo = paste0(
            "La columna de clave `", col_k,
            "` tiene un tipo no comparable para huella en el motor."
          )
        ))
      }
    }
    list(ok = TRUE, columnas = as.character(resueltas), motivo = NA_character_)
  }
  sin_hash_motivo <- function(cols) {
    paste0(
      "El motor no dispone de una funci\u00f3n de hash compatible para muestrear ",
      "por huella de `", paste(cols, collapse = "`, `"),
      "`; SQL no garantiza el orden de las filas."
    )
  }

  # 1. Clave declarada por el usuario (`clave = ...`).
  if (length(clave_declarada) > 0L) {
    chk <- cols_usables(clave_declarada)
    if (!isTRUE(chk$ok)) {
      return(vacio(
        paste0(chk$motivo, " SQL no garantiza el orden de las filas."),
        origen = "clave_declarada", columnas = as.character(clave_declarada)
      ))
    }
    if (is.na(funcion_hash)) {
      return(vacio(
        sin_hash_motivo(chk$columnas),
        origen = "clave_declarada", columnas = chk$columnas
      ))
    }
    garantizada <- isTRUE(.orden_garantizado_unico_dbi(
      catalogo_garantias, chk$columnas, campos
    ))
    return(list(
      disponible = TRUE,
      columnas = chk$columnas,
      origen = "clave_declarada",
      nombre_indice = NA_character_,
      garantizada_catalogo = garantizada,
      requiere_verificacion = !garantizada,
      motor_hash = motor_hash,
      funcion_hash = funcion_hash,
      motivo = if (garantizada) {
        "La unicidad de `clave` est\u00e1 garantizada por el cat\u00e1logo del motor."
      } else {
        "Clave declarada con `clave =`; se verifica su unicidad antes de declarar reproducibilidad."
      }
    ))
  }

  # 2. Clave primaria de grado total leida del catalogo.
  if (is.list(catalogo_garantias)) {
    pk <- catalogo_garantias$clave_primaria
    if (is.list(pk) && identical(pk$grado, "total") && length(pk$columnas) > 0L) {
      chk <- cols_usables(pk$columnas)
      if (isTRUE(chk$ok)) {
        if (is.na(funcion_hash)) {
          return(vacio(
            sin_hash_motivo(chk$columnas),
            origen = "clave_primaria", columnas = chk$columnas
          ))
        }
        return(list(
          disponible = TRUE,
          columnas = chk$columnas,
          origen = "clave_primaria",
          nombre_indice = NA_character_,
          garantizada_catalogo = TRUE,
          requiere_verificacion = FALSE,
          motor_hash = motor_hash,
          funcion_hash = funcion_hash,
          motivo = paste0(
            "Muestra reproducible por huella de la clave primaria (`",
            paste(chk$columnas, collapse = "`, `"), "`) garantizada por el cat\u00e1logo."
          )
        ))
      }
    }

    # 3. Restriccion o indice unico de grado total del catalogo.
    unicas <- catalogo_garantias$claves_unicas
    if (is.data.frame(unicas) && nrow(unicas) > 0L) {
      for (i in seq_len(nrow(unicas))) {
        if (!identical(as.character(unicas$grado[[i]]), "total")) next
        cols_u <- as.character(unicas$columnas[[i]])
        chk <- cols_usables(cols_u)
        if (!isTRUE(chk$ok)) next
        orig <- as.character(unicas$origen[[i]])
        nom_idx <- as.character(unicas$nombre[[i]])
        if (is.na(funcion_hash)) {
          return(vacio(
            sin_hash_motivo(chk$columnas),
            origen = orig, columnas = chk$columnas
          ))
        }
        etiqueta <- if (identical(orig, "restriccion_unica")) {
          "restricci\u00f3n \u00fanica"
        } else {
          "\u00edndice \u00fanico"
        }
        return(list(
          disponible = TRUE,
          columnas = chk$columnas,
          origen = orig,
          nombre_indice = nom_idx,
          garantizada_catalogo = TRUE,
          requiere_verificacion = FALSE,
          motor_hash = motor_hash,
          funcion_hash = funcion_hash,
          motivo = paste0(
            "Muestra reproducible por huella de ", etiqueta, " de grado total (`",
            paste(chk$columnas, collapse = "`, `"), "`) garantizada por el cat\u00e1logo."
          )
        ))
      }
    }
  }

  # 4. Sin clave de grado total ni `clave =`: se dice y se mantiene el camino
  # historico sin orden garantizado.
  vacio(paste(
    "No se declar\u00f3 `orden_muestra` ni hay clave primaria o \u00fanica de grado",
    "total en el cat\u00e1logo ni `clave`; SQL no garantiza el orden de las filas."
  ))
}

# Unica regla compartida entre el plan, la reserva de presupuesto y el bloque de
# muestra para decidir si se emite la consulta de verificacion de unicidad de
# `clave =` (una clave del catalogo de grado total cuesta 0 consultas extra).
.verificar_clave_muestra_necesaria_dbi <- function(clave_info, muestra,
                                                   bloque_muestra, universo,
                                                   orden_muestra) {
  identical(universo, "tabla_completa") &&
    isTRUE(.solicita_muestra_dbi(bloque_muestra)) &&
    !length(orden_muestra) &&
    is.numeric(muestra) && length(muestra) == 1L && is.finite(muestra) &&
    is.list(clave_info) && isTRUE(clave_info$disponible) &&
    isTRUE(clave_info$requiere_verificacion)
}

# Serializacion inyectiva de una clave simple o compuesta: separa NULL de '' y
# antepone el largo en bytes UTF-8 de cada componente (en SQLite, el largo en
# caracteres) para impedir colisiones entre ('a|b', 'c') y ('a', 'b|c') y
# unificar decimales, fechas ISO, enteros grandes, UUID y texto Unicode entre
# los motores con MD5.
.expresion_clave_canonica_dbi <- function(cols_sql, motor_hash,
                                          tipos_canonicos = NULL,
                                          soporta_utf8_mssql = FALSE) {
  n_cols <- length(cols_sql)
  if (is.null(tipos_canonicos) || length(tipos_canonicos) != n_cols) {
    tipos_canonicos <- rep("otro", n_cols)
  }
  col_u8 <- .COLACION_UTF8_MSSQL_DBI
  col_puente <- .COLACION_PUENTE_MSSQL_DBI

  parte <- function(i) {
    c_sql <- cols_sql[[i]]
    tipo_c <- tipos_canonicos[[i]]
    if (identical(motor_hash, "postgresql")) {
      val <- if (identical(tipo_c, "decimal")) {
        paste0(
          "CASE WHEN ", c_sql, " = 0 THEN '0' ",
          "WHEN POSITION('.' IN CAST(", c_sql, " AS text)) > 0 ",
          "THEN RTRIM(RTRIM(CAST(", c_sql, " AS text), '0'), '.') ",
          "ELSE CAST(", c_sql, " AS text) END"
        )
      } else if (identical(tipo_c, "fecha")) {
        paste0("TO_CHAR(", c_sql, ", 'YYYY-MM-DD')")
      } else if (identical(tipo_c, "uuid")) {
        paste0("LOWER(CAST(", c_sql, " AS text))")
      } else {
        paste0("CAST(", c_sql, " AS text)")
      }
      return(paste0(
        "CASE WHEN ", c_sql, " IS NULL THEN 'N' ELSE 'V' || CAST(OCTET_LENGTH(",
        val, ") AS text) || ':' || ", val, " END"
      ))
    }
    if (identical(motor_hash, "duckdb")) {
      val <- if (identical(tipo_c, "decimal")) {
        paste0(
          "CASE WHEN ", c_sql, " = 0 THEN '0' ",
          "WHEN STRPOS(CAST(", c_sql, " AS VARCHAR), '.') > 0 ",
          "THEN RTRIM(RTRIM(CAST(", c_sql, " AS VARCHAR), '0'), '.') ",
          "ELSE CAST(", c_sql, " AS VARCHAR) END"
        )
      } else if (identical(tipo_c, "fecha")) {
        paste0("strftime(", c_sql, ", '%Y-%m-%d')")
      } else if (identical(tipo_c, "uuid")) {
        paste0("LOWER(CAST(", c_sql, " AS VARCHAR))")
      } else {
        paste0("CAST(", c_sql, " AS VARCHAR)")
      }
      return(paste0(
        "CASE WHEN ", c_sql, " IS NULL THEN 'N' ELSE 'V' || CAST(strlen(",
        val, ") AS VARCHAR) || ':' || ", val, " END"
      ))
    }
    if (identical(motor_hash, "mysql")) {
      cast_u8 <- paste0("CAST(", c_sql, " AS CHAR CHARACTER SET utf8mb4)")
      val <- if (identical(tipo_c, "decimal")) {
        paste0(
          "CASE WHEN ", c_sql, " = 0 THEN '0' ",
          "WHEN LOCATE('.', ", cast_u8, ") > 0 ",
          "THEN TRIM(TRAILING '.' FROM TRIM(TRAILING '0' FROM ", cast_u8, ")) ",
          "ELSE ", cast_u8, " END"
        )
      } else if (identical(tipo_c, "fecha")) {
        paste0("DATE_FORMAT(", c_sql, ", '%Y-%m-%d')")
      } else if (identical(tipo_c, "uuid")) {
        paste0("LOWER(", cast_u8, ")")
      } else {
        cast_u8
      }
      return(paste0(
        "CASE WHEN ", c_sql, " IS NULL THEN 'N' ELSE CONCAT('V', OCTET_LENGTH(",
        val, "), ':', ", val, ") END"
      ))
    }
    if (identical(motor_hash, "sqlserver")) {
      if (isTRUE(soporta_utf8_mssql)) {
        val_u8 <- if (identical(tipo_c, "decimal")) {
          s_dec <- paste0("CAST(", c_sql, " AS VARCHAR(64))")
          sin_ceros <- paste0(
            "REPLACE(RTRIM(REPLACE(", s_dec, ", '0', ' ')), ' ', '0')"
          )
          paste0(
            "((CASE WHEN ", c_sql, " = 0 THEN '0' ",
            "WHEN CHARINDEX('.', ", s_dec, ") > 0 THEN ",
            "(CASE WHEN RIGHT(", sin_ceros, ", 1) = '.' ",
            "THEN LEFT(", sin_ceros, ", LEN(", sin_ceros, ") - 1) ",
            "ELSE ", sin_ceros, " END) ",
            "ELSE ", s_dec, " END) COLLATE ", col_u8, ")"
          )
        } else if (identical(tipo_c, "fecha")) {
          paste0("(CONVERT(VARCHAR(10), ", c_sql, ", 23) COLLATE ", col_u8, ")")
        } else if (identical(tipo_c, "uuid")) {
          paste0("(LOWER(CAST(", c_sql, " AS VARCHAR(36))) COLLATE ", col_u8, ")")
        } else if (identical(tipo_c, "entero")) {
          paste0("(CAST(", c_sql, " AS VARCHAR(40)) COLLATE ", col_u8, ")")
        } else {
          # En SQL Server, si una columna ya tiene colacion
          # `Latin1_General_100_BIN2`, aplicar `COLLATE ..._BIN2_UTF8` directo
          # sobre `NVARCHAR(MAX)` se omite como no-op y el `CAST` a `VARCHAR`
          # conserva Windows-1252 (provocando conflicto de colacion o perdida
          # de Unicode). Pasar antes por una colacion intermedia fija
          # (`Latin1_General_BIN`) fuerza el cambio real a UTF-8 para cualquier
          # colacion de origen y fija la colacion explicita en el operando.
          paste0(
            "(CAST((CAST(", c_sql, " AS NVARCHAR(MAX)) COLLATE ", col_puente,
            ") COLLATE ", col_u8, " AS VARCHAR(MAX)) COLLATE ", col_u8, ")"
          )
        }
        return(paste0(
          "CASE WHEN ", c_sql, " IS NULL THEN ('N' COLLATE ", col_u8, ") ",
          "ELSE ('V' COLLATE ", col_u8, ") + ",
          "(CAST(DATALENGTH(", val_u8, ") AS VARCHAR(20)) COLLATE ", col_u8, ") + ",
          "(':' COLLATE ", col_u8, ") + ", val_u8, " END"
        ))
      }
      # SQL Server anterior a 2019 (sin colaciones UTF-8): conserva los bytes
      # Unicode en NVARCHAR; el alcance publicado se restringe a este motor.
      return(paste0(
        "CASE WHEN ", c_sql, " IS NULL THEN (N'N' COLLATE DATABASE_DEFAULT) ",
        "ELSE (N'V' COLLATE DATABASE_DEFAULT) + ",
        "(CAST(LEN((CAST(", c_sql, " AS nvarchar(4000)) COLLATE DATABASE_DEFAULT) + ",
        "(N'x' COLLATE DATABASE_DEFAULT)) - 1 AS nvarchar(20)) COLLATE DATABASE_DEFAULT) + ",
        "(N':' COLLATE DATABASE_DEFAULT) + ",
        "(CAST(", c_sql, " AS nvarchar(4000)) COLLATE DATABASE_DEFAULT) END"
      ))
    }
    paste0(
      "CASE WHEN ", c_sql, " IS NULL THEN 'N' ",
      "WHEN typeof(", c_sql, ") = 'blob' THEN 'B' || CAST(length(hex(",
      c_sql, ")) AS TEXT) || ':' || hex(", c_sql, ") ",
      "ELSE 'V' || CAST(length(CAST(", c_sql,
      " AS TEXT)) AS TEXT) || ':' || CAST(", c_sql, " AS TEXT) END"
    )
  }

  partes <- vapply(seq_len(n_cols), parte, character(1L), USE.NAMES = FALSE)
  if (identical(motor_hash, "mysql")) {
    if (length(partes) == 1L) partes[[1L]] else {
      paste0("CONCAT(", paste(partes, collapse = ", '|', "), ")")
    }
  } else if (identical(motor_hash, "sqlserver")) {
    sep_ms <- if (isTRUE(soporta_utf8_mssql)) {
      paste0(" + ('|' COLLATE ", col_u8, ") + ")
    } else {
      " + (N'|' COLLATE DATABASE_DEFAULT) + "
    }
    paste(partes, collapse = sep_ms)
  } else {
    paste(partes, collapse = " || '|' || ")
  }
}

.expresion_huella_motor_dbi <- function(cols_sql, motor_hash,
                                        version_sqlserver = NA_real_,
                                        tipos_canonicos = NULL) {
  ver <- suppressWarnings(as.numeric(version_sqlserver))
  soporta_utf8_mssql <- identical(motor_hash, "sqlserver") &&
    length(ver) == 1L && is.finite(ver) && ver >= 15
  todos_canonicos <- !is.null(tipos_canonicos) &&
    length(tipos_canonicos) == length(cols_sql) &&
    length(tipos_canonicos) > 0L &&
    all(tipos_canonicos %in% .TIPOS_CANONICOS_ENTRE_MOTORES_DBI)

  canon <- .expresion_clave_canonica_dbi(
    cols_sql, motor_hash,
    tipos_canonicos = tipos_canonicos,
    soporta_utf8_mssql = soporta_utf8_mssql
  )
  alcance_md5 <- if (isTRUE(todos_canonicos)) "entre_motores_md5" else "motor"
  codif_md5 <- if (isTRUE(todos_canonicos)) {
    "utf8_canonica_md5"
  } else {
    "utf8_nativa_motor_md5"
  }

  if (identical(motor_hash, "postgresql")) {
    expr <- paste0("md5(", canon, ") COLLATE \"C\"")
    return(list(
      expr = expr, funcion = "md5", soporta_umbral_hex = TRUE,
      alcance_reproducibilidad = alcance_md5,
      codificacion_huella = codif_md5
    ))
  }
  if (identical(motor_hash, "duckdb")) {
    return(list(
      expr = paste0("md5(", canon, ")"),
      funcion = "md5", soporta_umbral_hex = TRUE,
      alcance_reproducibilidad = alcance_md5,
      codificacion_huella = codif_md5
    ))
  }
  if (identical(motor_hash, "mysql")) {
    return(list(
      expr = paste0("MD5(", canon, ")"),
      funcion = "md5", soporta_umbral_hex = TRUE,
      alcance_reproducibilidad = alcance_md5,
      codificacion_huella = codif_md5
    ))
  }
  if (identical(motor_hash, "sqlserver")) {
    tipo_bin <- if (length(ver) == 1L && is.finite(ver) && ver >= 13) {
      "varbinary(max)"
    } else {
      "varbinary(8000)"
    }
    alcance_ms <- if (isTRUE(soporta_utf8_mssql) && isTRUE(todos_canonicos)) {
      "entre_motores_md5"
    } else {
      "motor"
    }
    codif_ms <- if (!isTRUE(soporta_utf8_mssql)) {
      "utf16_mssql_md5"
    } else {
      codif_md5
    }
    return(list(
      expr = paste0("HASHBYTES('MD5', CONVERT(", tipo_bin, ", ", canon, "))"),
      funcion = "hashbytes_md5", soporta_umbral_hex = TRUE,
      alcance_reproducibilidad = alcance_ms,
      codificacion_huella = codif_ms
    ))
  }
  if (identical(motor_hash, "sqlite")) {
    # SQLite no trae md5() de serie. Esta mezcla entera de dos pasos en modulo
    # primo 2^31 - 1 usa solo funciones escalares estandar, evita desborde a
    # REAL (cada producto es < 2^62) y distribuye uniformemente claves
    # numericas y textuales; el desempate por las columnas de la clave cierra
    # cualquier colision.
    pos_expr <- function(pos, mult) {
      paste0("COALESCE(unicode(substr(", canon, ", ", pos, ", 1)), 0) * ", mult)
    }
    terminos <- c(
      paste0("length(", canon, ") * 1315423"),
      pos_expr(1L, 2654435L), pos_expr(2L, 48271L), pos_expr(3L, 16807L),
      pos_expr(4L, 69621L), pos_expr(5L, 3935559L), pos_expr(6L, 1103515L),
      pos_expr(7L, 214013L), pos_expr(8L, 2531011L),
      pos_expr(-1L, 6364136L), pos_expr(-2L, 1442695L),
      pos_expr(-3L, 2246822L), pos_expr(-4L, 3266489L),
      paste0(
        "COALESCE(unicode(substr(", canon, ", (length(", canon,
        ") / 2) + 1, 1)), 0) * 6682652"
      )
    )
    h1 <- paste0("((", paste(terminos, collapse = " + "), ") % 2147483647)")
    expr <- paste0(
      "(((", h1, " * 48271 + 123456789) % 2147483647 + 2147483647) % 2147483647)"
    )
    return(list(
      expr = expr, funcion = "huella_sql_sqlite", soporta_umbral_hex = FALSE,
      alcance_reproducibilidad = "motor",
      codificacion_huella = "huella_sql_sqlite"
    ))
  }
  NULL
}

# Umbral sobremuestreado a 6 desviaciones estandar para el prefiltro WHERE:
# las k menores huellas estan dentro de `{hash < umbral}` con probabilidad
# > 1 - 1e-9, evitando ordenar la tabla entera cuando k << N.
.umbral_hex_muestra_motor_dbi <- function(k, n_total, sigmas = 6) {
  k_num <- suppressWarnings(as.numeric(k))
  n_num <- suppressWarnings(as.numeric(n_total))
  if (length(k_num) != 1L || length(n_num) != 1L ||
      !is.finite(k_num) || !is.finite(n_num) ||
      k_num < 1 || n_num < .UMBRAL_FILAS_PREFILTRO_HUELLA_DBI ||
      k_num >= n_num) {
    return(NULL)
  }
  p_over <- (k_num + sigmas * sqrt(k_num) + 25) / n_num
  if (!is.finite(p_over) || p_over <= 0 || p_over >= 0.5) return(NULL)
  val <- min(4294967295, ceiling(p_over * 4294967296))
  hi <- floor(val / 65536)
  lo <- val - hi * 65536
  hex8 <- sprintf("%04x%04x", as.integer(hi), as.integer(lo))
  list(
    p_over = p_over,
    hex8 = hex8,
    hex32_mssql = paste0("0x", hex8, strrep("0", 24L))
  )
}

.sql_filtro_umbral_huella_dbi <- function(huella_sql, umbral, motor_hash) {
  if (is.null(huella_sql) || is.null(umbral) ||
      !isTRUE(huella_sql$soporta_umbral_hex)) {
    return(NULL)
  }
  if (identical(motor_hash, "sqlserver")) {
    return(paste0(huella_sql$expr, " < ", umbral$hex32_mssql))
  }
  paste0(huella_sql$expr, " < '", umbral$hex8, "'")
}

# ---- Contadores sincronos de modificaciones del motor --------------------

.motor_contador_estabilidad_dbi <- function(conexion) {
  motor <- tryCatch(.motor_clave_primaria(conexion), error = function(e) NA_character_)
  if (length(motor) == 1L && !is.na(motor) &&
      motor %in% c("postgresql", "sqlserver", "mysql", "mariadb", "sqlite", "duckdb")) {
    return(motor)
  }
  senas <- .senas_conexion_dbi(conexion)
  if (grepl("mariadb", senas, fixed = TRUE, useBytes = TRUE)) return("mariadb")
  if (grepl("mysql", senas, fixed = TRUE, useBytes = TRUE)) return("mysql")
  .motor_huella_muestra_dbi(conexion)
}

.esquema_y_tabla_mysql_estabilidad_dbi <- function(conexion, tabla, tabla_sql) {
  esquema_nombre <- NA_character_
  tabla_nombre <- NA_character_
  if (isTRUE(.es_id_dbi(tabla))) {
    piezas <- .partes_id_dbi(tabla)
    if ("esquema" %in% names(piezas)) esquema_nombre <- unname(piezas[["esquema"]])
    if ("tabla" %in% names(piezas)) tabla_nombre <- unname(piezas[["tabla"]])
  }
  if (is.na(tabla_nombre) || !nzchar(tabla_nombre)) {
    texto <- gsub("`", "", as.character(tabla_sql), fixed = TRUE, useBytes = TRUE)
    partes <- strsplit(texto, ".", fixed = TRUE, useBytes = TRUE)[[1L]]
    tabla_nombre <- partes[[length(partes)]]
    if (length(partes) > 1L) esquema_nombre <- partes[[length(partes) - 1L]]
  }
  citar <- function(x) {
    tryCatch(as.character(DBI::dbQuoteString(conexion, as.character(x))),
             error = function(e) NA_character_)
  }
  tab_lit <- citar(tabla_nombre)
  if (is.na(tab_lit)) return(NULL)
  esq_sql <- if (is.na(esquema_nombre) || !nzchar(esquema_nombre)) {
    "DATABASE()"
  } else {
    citar(esquema_nombre)
  }
  if (is.na(esq_sql)) return(NULL)
  list(esquema_sql = esq_sql, tabla_lit = tab_lit)
}

# Describe el contador sincrono de modificaciones del motor que se puede leer
# dentro de la primera consulta consolidada y de la consulta de cierre sin
# agregar consultas extra. Devuelve NULL si el motor o el tipo de objeto no
# disponen de un contador sincrono por tabla.
.especificacion_contador_estabilidad_dbi <- function(conexion, tabla, tabla_sql,
                                                     tipo_objeto = NA_character_,
                                                     tipos_motor = NULL,
                                                     campos = character(),
                                                     campos_sql = NULL) {
  motor <- .motor_contador_estabilidad_dbi(conexion)
  tipo_obj <- if (length(tipo_objeto) == 1L && !is.na(tipo_objeto)) {
    .minusculas_ascii(trimws(as.character(tipo_objeto)))
  } else ""

  if (identical(motor, "postgresql")) {
    # En PostgreSQL, la columna de sistema `xmin` existe en tablas regulares
    # (`r`), particionadas (`p`) y vistas materializadas (`m`), pero no en
    # vistas (`v`) ni tablas foraneas (`f`). Junto a `COUNT(*)`, `MAX(xmin)`
    # detecta cualquier INSERT, UPDATE (incluso de texto solo y HOT updates),
    # DELETE y DELETE+INSERT a 0 ms, evitando el costo de `SUM(xmin)` en
    # tablas de millones de filas.
    if (!(tipo_obj %in% c("r", "p", "m"))) return(NULL)
    return(list(
      motor = "postgresql",
      mecanismo = "xmin_mvcc",
      etiqueta = "versi\u00f3n de filas del motor (xmin)",
      requiere_tabla_base = TRUE,
      requiere_filas_no_vacias = TRUE,
      solo_contador_cierre = FALSE,
      motivo_inactivo = NULL,
      campos = list(
        list(
          clave = "xmin_max",
          etiqueta_dif = "xmin(max)",
          alias = "lupa_est_xmin_max",
          sql = "CAST(MAX(CAST(xmin AS text)::bigint) AS text)",
          obligatorio = TRUE
        )
      )
    ))
  }

  if (identical(motor, "sqlserver")) {
    # 1. Si la tabla tiene columna `rowversion` / `timestamp`, su MAX junto a
    # COUNT(*) detecta cualquier INSERT, UPDATE o DELETE sin permisos de DMV.
    if (length(campos) && !is.null(tipos_motor)) {
      for (i in seq_along(campos)) {
        col_i <- campos[[i]]
        tm_i <- .atributo_columna_dbi(tipos_motor, col_i, i) %||% NA_character_
        base_tm <- .minusculas_ascii(trimws(as.character(tm_i)))
        if (base_tm %in% c("timestamp", "rowversion")) {
          col_sql_i <- if (!is.null(campos_sql) && col_i %in% names(campos_sql)) {
            unname(campos_sql[[col_i]])
          } else {
            .citar_identificador_dbi(conexion, col_i)
          }
          return(list(
            motor = "sqlserver",
            mecanismo = "rowversion",
            etiqueta = paste0("versi\u00f3n de fila (`", col_i, "`)"),
            requiere_tabla_base = FALSE,
            requiere_filas_no_vacias = TRUE,
            solo_contador_cierre = FALSE,
            motivo_inactivo = NULL,
            campos = list(
              list(
                clave = "rowversion_max",
                etiqueta_dif = paste0("rowversion(", col_i, ")"),
                alias = "lupa_est_rv_max",
                sql = paste0("CAST(MAX(CAST(", col_sql_i, " AS BIGINT)) AS VARCHAR(30))"),
                obligatorio = TRUE
              )
            )
          ))
        }
      }
    }
    # 2. En tablas base de usuario, `sys.dm_db_index_operational_stats` mide
    # escrituras sincronas en tablas con PK y en heaps sin barrer la tabla al
    # cierre (`solo_contador_cierre = TRUE`). La guarda con `OBJECT_ID(..., N'U')`
    # y `HAS_PERMS_BY_NAME` devuelve NULL sin fallar en vistas o sin permiso.
    nombre_simple <- gsub('^[["]|[]"]$', "", as.character(tabla_sql))
    if (startsWith(nombre_simple, "#")) return(NULL)
    if (nzchar(tipo_obj) && !(tipo_obj %in% c("u"))) return(NULL)
    lit <- tryCatch(
      as.character(DBI::dbQuoteString(conexion, as.character(tabla_sql))),
      error = function(e) NA_character_
    )
    if (is.na(lit)) return(NULL)
    obj <- paste0("N", lit)
    # Un nombre de tres partes es una tabla de esa base: el contador y el
    # permiso son los de esa base. Con `DB_ID()` el motor buscaba el id de la
    # otra base en la actual y abortaba la consulta ("Could not find table or
    # object ID").
    partes_ss <- tryCatch(.partir_identificador(as.character(tabla_sql))$partes,
                          error = function(e) character())
    base_ss <- if (length(partes_ss) == 3L) {
      .quitar_comillas_identificador(partes_ss[[1L]])
    } else NA_character_
    base_lit <- if (!is.na(base_ss)) {
      tryCatch(paste0("N", as.character(DBI::dbQuoteString(conexion, base_ss))),
               error = function(e) NA_character_)
    } else NA_character_
    if (!is.na(base_ss) && is.na(base_lit)) return(NULL)
    nombre_base <- if (is.na(base_lit)) "DB_NAME()" else base_lit
    id_base <- if (is.na(base_lit)) "DB_ID()" else paste0("DB_ID(", base_lit, ")")
    sql_ops <- paste0(
      "CASE WHEN OBJECT_ID(", obj, ", N'U') IS NOT NULL AND ",
      "HAS_PERMS_BY_NAME(", nombre_base, ", N'DATABASE', N'VIEW DATABASE STATE') = 1 THEN ",
      "(SELECT CAST(COALESCE(SUM(leaf_insert_count + leaf_update_count + ",
      "leaf_delete_count + leaf_ghost_count), 0) AS VARCHAR(30)) ",
      "FROM sys.dm_db_index_operational_stats(", id_base, ", OBJECT_ID(", obj,
      ", N'U'), NULL, NULL)) ELSE NULL END"
    )
    return(list(
      motor = "sqlserver",
      mecanismo = "dm_db_index_operational_stats",
      # El motor puede negarle esta lectura a una credencial comun: no va
      # dentro de una consulta que mide (ver `.contadores_aparte_medicion_dbi()`).
      negable = TRUE,
      etiqueta = "contador de escrituras del motor (sys.dm_db_index_operational_stats)",
      requiere_tabla_base = FALSE,
      requiere_filas_no_vacias = FALSE,
      solo_contador_cierre = TRUE,
      motivo_inactivo = "no se pudo leer `sys.dm_db_index_operational_stats` (permiso `VIEW DATABASE STATE` o tipo de objeto)",
      campos = list(
        list(
          clave = "dml_ops",
          etiqueta_dif = "contador_dml_motor",
          alias = "lupa_est_dml",
          sql = sql_ops,
          obligatorio = TRUE
        )
      )
    ))
  }

  if (identical(motor, "mysql")) {
    partes <- .esquema_y_tabla_mysql_estabilidad_dbi(conexion, tabla, tabla_sql)
    if (is.null(partes)) return(NULL)
    # Comprobar antes de confiar en `COUNT_WRITE` que el instrumento de E/S de
    # tablas, los consumidores globales/de hilo, las reglas de `setup_actors`
    # (comodin activo sin exclusiones), todos los hilos activos en `threads` y
    # la regla mas especifica de `setup_objects` para la tabla esten activos
    # (`ENABLED = 'YES'` / `INSTRUMENTED = 'YES'`). Si algo esta apagado,
    # devuelve NULL y nunca afirma `sin_cambio_observado`.
    sql_ps <- paste0(
      "CASE WHEN EXISTS (SELECT 1 FROM performance_schema.setup_instruments ",
      "WHERE NAME = 'wait/io/table/sql/handler' AND ENABLED = 'YES') AND ",
      "EXISTS (SELECT 1 FROM performance_schema.setup_consumers ",
      "WHERE NAME = 'global_instrumentation' AND ENABLED = 'YES') AND ",
      "EXISTS (SELECT 1 FROM performance_schema.setup_consumers ",
      "WHERE NAME = 'thread_instrumentation' AND ENABLED = 'YES') AND ",
      "EXISTS (SELECT 1 FROM performance_schema.setup_actors ",
      "WHERE HOST = '%' AND USER = '%' AND ROLE = '%' AND ENABLED = 'YES') AND ",
      "NOT EXISTS (SELECT 1 FROM performance_schema.setup_actors ",
      "WHERE ENABLED <> 'YES') AND ",
      "NOT EXISTS (SELECT 1 FROM performance_schema.threads ",
      "WHERE INSTRUMENTED <> 'YES') AND ",
      "COALESCE((SELECT ENABLED FROM performance_schema.setup_objects ",
      "WHERE OBJECT_TYPE = 'TABLE' AND ", partes$esquema_sql,
      " LIKE OBJECT_SCHEMA AND ", partes$tabla_lit, " LIKE OBJECT_NAME ",
      "ORDER BY (OBJECT_SCHEMA = '%') ASC, (OBJECT_NAME = '%') ASC, ",
      "(ENABLED = 'NO') DESC LIMIT 1), 'NO') = 'YES' THEN ",
      "(SELECT CAST(COUNT_WRITE AS CHAR) FROM ",
      "performance_schema.table_io_waits_summary_by_table ",
      "WHERE OBJECT_SCHEMA = ", partes$esquema_sql,
      " AND OBJECT_NAME = ", partes$tabla_lit, " LIMIT 1) ELSE NULL END"
    )
    return(list(
      motor = "mysql",
      mecanismo = "performance_schema",
      # `performance_schema` exige un permiso que una credencial comun no tiene.
      negable = TRUE,
      etiqueta = "contador de escrituras del motor (performance_schema)",
      requiere_tabla_base = FALSE,
      requiere_filas_no_vacias = FALSE,
      solo_contador_cierre = TRUE,
      motivo_inactivo = paste(
        "la instrumentaci\u00f3n completa de `performance_schema`",
        "(`setup_instruments`, `setup_consumers`, `setup_objects`,",
        "`setup_actors` o `threads`) no estaba activa para garantizar el",
        "conteo de toda escritura sobre esta tabla"
      ),
      campos = list(
        list(
          clave = "dml_write",
          etiqueta_dif = "contador_dml_motor",
          alias = "lupa_est_dml",
          sql = sql_ps,
          obligatorio = TRUE
        )
      )
    ))
  }

  if (identical(motor, "mariadb")) {
    partes <- .esquema_y_tabla_mysql_estabilidad_dbi(conexion, tabla, tabla_sql)
    if (is.null(partes)) return(NULL)
    sql_inno <- paste0(
      "(SELECT CAST(MODIFIED_COUNTER AS CHAR) FROM ",
      "information_schema.INNODB_SYS_TABLESTATS ",
      "WHERE NAME = CONCAT(", partes$esquema_sql, ", '/', ",
      partes$tabla_lit, ") LIMIT 1)"
    )
    sql_upd <- paste0(
      "(SELECT CAST(UPDATE_TIME AS CHAR) FROM information_schema.TABLES ",
      "WHERE TABLE_SCHEMA = ", partes$esquema_sql,
      " AND TABLE_NAME = ", partes$tabla_lit,
      " AND TABLE_TYPE = 'BASE TABLE' LIMIT 1)"
    )
    return(list(
      motor = "mariadb",
      mecanismo = "innodb_modified_counter",
      # `INNODB_SYS_TABLESTATS` exige el permiso `PROCESS`.
      negable = TRUE,
      etiqueta = "contador de modificaciones del motor (INNODB_SYS_TABLESTATS y UPDATE_TIME)",
      requiere_tabla_base = FALSE,
      requiere_filas_no_vacias = FALSE,
      solo_contador_cierre = FALSE,
      motivo_inactivo = NULL,
      campos = list(
        list(
          clave = "inno_mod",
          etiqueta_dif = "innodb_modified_counter",
          alias = "lupa_est_inno_mod",
          sql = sql_inno,
          obligatorio = TRUE
        ),
        list(
          clave = "update_time",
          etiqueta_dif = "update_time",
          alias = "lupa_est_upd_time",
          sql = sql_upd,
          obligatorio = FALSE
        )
      )
    ))
  }

  NULL
}

.alcance_estabilidad_plan_dbi <- function(conexion, preparacion) {
  spec <- .especificacion_contador_estabilidad_dbi(
    conexion, preparacion$tabla_sql, preparacion$tabla_sql,
    tipo_objeto = preparacion$tipo_objeto,
    tipos_motor = preparacion$esquema$tipos_motor,
    campos = preparacion$campos
  )
  if (is.list(spec) && isTRUE(spec$solo_contador_cierre)) {
    return("lee el contador del motor")
  }
  if (is.list(spec)) {
    return("escanea la tabla completa")
  }
  "lee el conteo y maximo de la clave"
}

.expresiones_contador_estabilidad_dbi <- function(conexion, spec,
                                                  tabla_sql,
                                                  tabla_fuente_sql = tabla_sql) {
  if (!is.list(spec) || !length(spec$campos)) return(character())
  alias_fn <- function(nombre) .citar_identificador_dbi(conexion, nombre)
  envuelta <- !identical(tabla_fuente_sql, tabla_sql)
  vapply(spec$campos, function(campo) {
    expr <- campo$sql
    if (isTRUE(spec$requiere_tabla_base) && isTRUE(envuelta)) {
      expr <- paste0("(SELECT ", expr, " FROM ", tabla_sql, ")")
    }
    paste0(expr, " AS ", alias_fn(campo$alias))
  }, character(1L), USE.NAMES = FALSE)
}

.leer_contador_estabilidad_dbi <- function(datos, spec, n_filas = NA_real_) {
  if (!is.list(spec) || !length(spec$campos) || !is.data.frame(datos) ||
      nrow(datos) != 1L) {
    return(list(disponible = FALSE, parcial = FALSE, valores = list()))
  }
  valores <- list()
  obligatorios_ok <- TRUE
  alguno_ok <- FALSE
  for (campo in spec$campos) {
    cel <- .valor_campo_dbi(datos, campo$alias)
    val_txt <- NA_character_
    if (isTRUE(cel$ok) && length(cel$valor) == 1L && !is.na(cel$valor[[1L]])) {
      val_txt <- trimws(as.character(cel$valor[[1L]]))
      if (!nzchar(val_txt)) val_txt <- NA_character_
    }
    valores[[campo$clave]] <- val_txt
    if (!is.na(val_txt)) {
      alguno_ok <- TRUE
    } else if (isTRUE(campo$obligatorio)) {
      obligatorios_ok <- FALSE
    }
  }
  # Si el contador requiere filas vivas (p.ej. `xmin` o `rowversion`) y la
  # tabla esta vacia (`n_filas == 0`), el valor NULL no puede observar un
  # INSERT seguido de DELETE entre consultas: queda no disponible como contador
  # completo y el estado cae a `sin_cambio_en_los_controles`.
  if (isTRUE(spec$requiere_filas_no_vacias) &&
      length(n_filas) == 1L && is.finite(n_filas) && n_filas <= 0) {
    obligatorios_ok <- FALSE
  }
  list(
    disponible = isTRUE(obligatorios_ok) && isTRUE(alguno_ok),
    parcial = isTRUE(alguno_ok) && !isTRUE(obligatorios_ok),
    valores = valores
  )
}

# ---- Verificacion de estabilidad durante la lectura ----------------------

# Regla unica compartida entre plan y corrida: la verificacion de cierre solo
# se agrega en `tabla_completa` cuando hay dos o mas consultas de datos sobre
# la tabla; si todo salio de una sola sentencia, no hace falta una segunda
# pasada.
.requiere_verificacion_estabilidad_dbi <- function(universo,
                                                   n_consultas_datos,
                                                   n_consultas_datos_max =
                                                     n_consultas_datos) {
  if (!identical(universo, "tabla_completa")) {
    return(list(min = 0L, max = 0L))
  }
  list(
    min = if (is.finite(n_consultas_datos) && n_consultas_datos >= 2) 1L else 0L,
    max = if (is.finite(n_consultas_datos_max) && n_consultas_datos_max >= 2) {
      1L
    } else 0L
  )
}

# Normaliza un valor escalar de control para comparar antes vs despues sin
# falsos positivos por formato ni perdida de precision en integer64.
.escalar_estabilidad_dbi <- function(valor) {
  if (is.null(valor) || !length(valor)) return(list(ok = FALSE, texto = NA_character_, num = NA_real_))
  v <- valor[[1L]]
  if (is.na(v)) return(list(ok = TRUE, texto = NA_character_, num = NA_real_))
  if (inherits(v, "integer64")) {
    txt <- as.character(v)
    return(list(ok = TRUE, texto = txt, num = suppressWarnings(as.numeric(txt))))
  }
  if (is.numeric(v)) {
    if (!is.finite(v)) return(list(ok = FALSE, texto = NA_character_, num = NA_real_))
    return(list(
      ok = TRUE,
      texto = format(as.numeric(v), digits = 17L, scientific = FALSE, trim = TRUE),
      num = as.numeric(v)
    ))
  }
  txt <- as.character(v)
  if (!length(txt) || is.na(txt[[1L]])) {
    return(list(ok = TRUE, texto = NA_character_, num = NA_real_))
  }
  num <- suppressWarnings(as.numeric(txt[[1L]]))
  if (length(num) == 1L && is.finite(num) &&
      grepl("^[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", trimws(txt[[1L]]),
            perl = TRUE, useBytes = TRUE)) {
    return(list(
      ok = TRUE,
      texto = format(num, digits = 17L, scientific = FALSE, trim = TRUE),
      num = num
    ))
  }
  list(ok = TRUE, texto = txt[[1L]], num = NA_real_)
}

.valores_estabilidad_iguales_dbi <- function(a, b) {
  if (is.na(a$texto) && is.na(b$texto)) return(TRUE)
  if (is.na(a$texto) || is.na(b$texto)) return(FALSE)
  if (identical(a$texto, b$texto)) return(TRUE)
  if (is.finite(a$num) && is.finite(b$num)) {
    return(identical(a$num, b$num))
  }
  FALSE
}

# Devuelve la unica columna indexada del catalogo (PK de una columna o primer
# indice/restriccion unica de una columna) para que el cierre sin contador
# consulte solo `MAX(clave)` en O(log N) sin barrer columnas no indexadas.
.clave_indexada_estabilidad_dbi <- function(preparacion, campos) {
  cat_g <- preparacion$catalogo_garantias
  if (!is.list(cat_g)) return(character())
  pk <- cat_g$clave_primaria
  if (is.list(pk) && length(pk$columnas) == 1L) {
    pos <- .resolver_columnas_dbi(as.character(pk$columnas), campos)
    if (!anyNA(pos)) return(as.character(campos[pos]))
  }
  unicas <- cat_g$claves_unicas
  if (is.data.frame(unicas) && nrow(unicas) > 0L) {
    for (i in seq_len(nrow(unicas))) {
      cols_u <- as.character(unicas$columnas[[i]])
      if (length(cols_u) != 1L) next
      pos <- .resolver_columnas_dbi(cols_u, campos)
      if (!anyNA(pos)) return(as.character(campos[pos]))
    }
  }
  character()
}

# Captura el estado inicial de estabilidad reusando el contador del motor (si
# estuvo disponible) o, sin contador, unicamente `COUNT(*)` y el maximo de la
# clave indexada de una columna (nunca agregados que barran columnas).
.capturar_antes_estabilidad_dbi <- function(conexion, preparacion, n_total,
                                            columnas_resumen = NULL,
                                            agregados = NULL,
                                            incluir_valores = TRUE,
                                            contador_spec = NULL,
                                            contador_antes = NULL) {
  n_num <- .numero_dbi(n_total)
  n_ok <- length(n_num) == 1L && is.finite(n_num) &&
    isTRUE(.conteo_exacto_dbi(n_total))
  tiene_contador_completo <- is.list(contador_spec) &&
    is.list(contador_antes) && isTRUE(contador_antes$disponible)

  # Con contador sincrono completo no hace falta agregar controles de columnas:
  # el contador ya cubre cualquier cambio en cualquier columna.
  if (isTRUE(tiene_contador_completo)) {
    return(list(
      n_filas = if (isTRUE(n_ok)) n_num else NA_real_,
      conteo_exacto = isTRUE(n_ok),
      controles = list(),
      contador_spec = contador_spec,
      contador_antes = contador_antes
    ))
  }

  elegidas <- list()
  if (isTRUE(incluir_valores)) {
    senas <- .senas_conexion_dbi(conexion)
    campos <- preparacion$campos %||% character()
    campos_sql <- preparacion$campos_sql %||% character()
    if (length(campos_sql) != length(campos)) {
      campos_sql <- vapply(campos, function(c) {
        .citar_identificador_dbi(conexion, c)
      }, character(1L), USE.NAMES = FALSE)
    }
    names(campos_sql) <- campos
    enmascaradas <- preparacion$enmascaradas %||% character()
    tipos_motor <- preparacion$esquema$tipos_motor
    tipos_decl <- preparacion$tipos
    es_numerico <- preparacion$es_numerico
    columnas_nan <- if (is.list(agregados) && is.list(agregados$nan)) {
      names(agregados$nan)[vapply(agregados$nan, function(x) {
        is.list(x) && isTRUE(x$ok) && isTRUE(.numero_dbi(x$valor) > 0)
      }, logical(1L))]
    } else character()

    col_clave <- .clave_indexada_estabilidad_dbi(preparacion, campos)
    if (length(col_clave) == 1L) {
      col <- col_clave[[1L]]
      idx <- .indice_nombre(col, campos)
      tm <- if (!is.na(idx)) .atributo_columna_dbi(tipos_motor, col, idx) %||% NA_character_ else NA_character_
      td <- if (!is.na(idx)) .atributo_columna_dbi(tipos_decl, col, idx) %||% NA_character_ else NA_character_
      es_num <- !is.na(idx) && !is.null(es_numerico) &&
        idx <= length(es_numerico) && isTRUE(es_numerico[[idx]])
      elegible <- !is.na(idx) && isTRUE(es_num) &&
        !(length(enmascaradas) && .nombres_presentes(col, enmascaradas)) &&
        !.es_tipo_binario_muestra_dbi(tm, senas) &&
        !.es_tipo_binario_muestra_dbi(td, senas) &&
        !(grepl("sql server|microsoft sql|sqlserver|mssql", senas,
                perl = TRUE, useBytes = TRUE) &&
          isTRUE(.tipo_sqlserver_sin_agregados_dbi(tm))) &&
        !(is.data.frame(preparacion$lectura_convertida) &&
          nrow(preparacion$lectura_convertida) &&
          col %in% as.character(preparacion$lectura_convertida$columna)) &&
        !(col %in% columnas_nan)

      max_antes <- NULL
      if (isTRUE(elegible)) {
        if (is.list(agregados)) {
          b_col <- agregados$basicos[[col]]
          if (is.list(b_col) && isTRUE(b_col$ok) &&
              is.data.frame(b_col$datos) && nrow(b_col$datos) == 1L) {
            c_max <- .valor_campo_dbi(b_col$datos, "maximo")
            if (isTRUE(c_max$ok)) {
              em_max <- .escalar_estabilidad_dbi(c_max$valor)
              if (isTRUE(em_max$ok)) max_antes <- em_max
            }
          }
        } else if (is.data.frame(columnas_resumen) && nrow(columnas_resumen)) {
          fila_idx <- match(col, as.character(columnas_resumen$columna))
          if (!is.na(fila_idx) && "maximo" %in% names(columnas_resumen) &&
              !is.na(columnas_resumen$maximo[[fila_idx]])) {
            em_max <- .escalar_estabilidad_dbi(columnas_resumen$maximo[[fila_idx]])
            es_dec <- .tipo_declarado_decimal_dbi(tm) || .tipo_declarado_decimal_dbi(td)
            if (isTRUE(em_max$ok) && !isTRUE(es_dec) &&
                is.finite(em_max$num) && abs(em_max$num) <= 9007199254740991) {
              max_antes <- em_max
            }
          }
        }
      }
      if (is.list(max_antes)) {
        elegidas[[1L]] <- list(
          columna = col,
          columna_sql = unname(campos_sql[[col]]),
          usar_validos = FALSE,
          usar_extremos = FALSE,
          usar_maximo = TRUE,
          antes_validos = NULL,
          antes_minimo = NULL,
          antes_maximo = max_antes
        )
      }
    }
  }

  list(
    n_filas = if (isTRUE(n_ok)) n_num else NA_real_,
    conteo_exacto = isTRUE(n_ok),
    controles = elegidas,
    contador_spec = contador_spec,
    contador_antes = contador_antes
  )
}

# Emite la consulta de cierre mas barata posible:
# 1) solo contador del motor (sin FROM tabla) en SQL Server DMV y MySQL PS;
# 2) COUNT(*) + contador en PostgreSQL (xmin), SQL Server rowversion y MariaDB;
# 3) sin contador, solo COUNT(*) y MAX(clave) como subconsultas escalares si
#    hay clave indexada de una columna (0 ms en SQLite sobre 1M filas), o
#    unicamente COUNT(*) si no hay indice.
.consultar_cierre_estabilidad_dbi <- function(conexion, tabla_sql, antes,
                                              presupuesto = NULL) {
  senas <- .senas_conexion_dbi(conexion)
  alias <- function(nombre) .citar_identificador_dbi(conexion, nombre)
  usar_contador_completo <- is.list(antes) && is.list(antes$contador_spec) &&
    is.list(antes$contador_antes) && isTRUE(antes$contador_antes$disponible)
  usar_contador_parcial <- is.list(antes) && is.list(antes$contador_spec) &&
    is.list(antes$contador_antes) && !isTRUE(usar_contador_completo) &&
    isTRUE(antes$contador_antes$parcial)
  usar_contador <- isTRUE(usar_contador_completo) || isTRUE(usar_contador_parcial)
  solo_contador <- isTRUE(usar_contador_completo) &&
    isTRUE(antes$contador_spec$solo_contador_cierre)

  if (isTRUE(solo_contador)) {
    exprs_cnt <- .expresiones_contador_estabilidad_dbi(
      conexion, antes$contador_spec, tabla_sql, tabla_sql
    )
    sql <- paste0("SELECT ", paste(exprs_cnt, collapse = ", "))
    consulta <- .consultar_dbi(
      conexion, sql, presupuesto, etapa = "verificacion_estabilidad"
    )
    if (!isTRUE(consulta$ok) || !is.data.frame(consulta$datos) ||
        nrow(consulta$datos) != 1L) {
      return(list(
        ok = FALSE, solo_contador = TRUE, sql = sql, n_filas = NA_real_,
        controles = list(), contador_despues = NULL,
        motivo = consulta$motivo %||% "La consulta de cierre no devolvi\u00f3 una fila."
      ))
    }
    cnt_d <- .leer_contador_estabilidad_dbi(
      consulta$datos, antes$contador_spec, n_filas = NA_real_
    )
    return(list(
      ok = TRUE,
      solo_contador = TRUE,
      sql = sql,
      n_filas = NA_real_,
      controles = list(),
      contador_despues = cnt_d,
      motivo = if (isTRUE(cnt_d$disponible)) NA_character_ else {
        antes$contador_spec$motivo_inactivo %||%
          "El contador de modificaciones del motor dej\u00f3 de estar disponible al cierre."
      }
    ))
  }

  controles <- if (is.list(antes) && length(antes$controles)) {
    antes$controles
  } else list()
  usar_subq_max <- !isTRUE(usar_contador_completo) &&
    length(controles) == 1L && isTRUE(controles[[1L]]$usar_maximo) &&
    !isTRUE(.es_oracle_dbi(conexion))

  if (isTRUE(usar_subq_max)) {
    # En SQLite, combinar `COUNT(*)` y `MAX(pk)` en el mismo SELECT barre la
    # tabla (~25 ms en 1M filas), mientras que dos subconsultas escalares en
    # una sola sentencia usan los atajos O(log N) del indice (~0 ms en 1M).
    exprs <- c(
      paste0(
        "(SELECT ", .conteo_sql_dbi("COUNT(*)", senas), " FROM ", tabla_sql,
        ") AS ", alias("lupa_est_n")
      ),
      paste0(
        "(SELECT MAX(", controles[[1L]]$columna_sql, ") FROM ", tabla_sql,
        ") AS ", alias("lupa_est_max_1")
      )
    )
    if (isTRUE(usar_contador_parcial)) {
      exprs <- c(
        exprs,
        .expresiones_contador_estabilidad_dbi(
          conexion, antes$contador_spec, tabla_sql, tabla_sql
        )
      )
    }
    sql <- paste0("SELECT ", paste(exprs, collapse = ", "))
  } else {
    exprs <- paste0(
      .conteo_sql_dbi("COUNT(*)", senas), " AS ", alias("lupa_est_n")
    )
    for (i in seq_along(controles)) {
      ctrl <- controles[[i]]
      if (isTRUE(ctrl$usar_validos)) {
        exprs <- c(exprs, paste0(
          .conteo_sql_dbi(paste0("COUNT(", ctrl$columna_sql, ")"), senas),
          " AS ", alias(paste0("lupa_est_v_", i))
        ))
      }
      if (isTRUE(ctrl$usar_extremos)) {
        exprs <- c(
          exprs,
          paste0("MIN(", ctrl$columna_sql, ") AS ", alias(paste0("lupa_est_min_", i))),
          paste0("MAX(", ctrl$columna_sql, ") AS ", alias(paste0("lupa_est_max_", i)))
        )
      } else if (isTRUE(ctrl$usar_maximo)) {
        exprs <- c(
          exprs,
          paste0("MAX(", ctrl$columna_sql, ") AS ", alias(paste0("lupa_est_max_", i)))
        )
      }
    }
    if (isTRUE(usar_contador)) {
      exprs <- c(
        exprs,
        .expresiones_contador_estabilidad_dbi(
          conexion, antes$contador_spec, tabla_sql, tabla_sql
        )
      )
    }
    sql <- paste0("SELECT ", paste(exprs, collapse = ", "), " FROM ", tabla_sql)
  }

  consulta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "verificacion_estabilidad"
  )
  if (!isTRUE(consulta$ok) || !is.data.frame(consulta$datos) ||
      nrow(consulta$datos) != 1L) {
    return(list(
      ok = FALSE, solo_contador = FALSE, sql = sql, n_filas = NA_real_,
      controles = list(), contador_despues = NULL,
      motivo = consulta$motivo %||% "La consulta de cierre no devolvi\u00f3 una fila."
    ))
  }
  celda_n <- .valor_campo_dbi(consulta$datos, "lupa_est_n")
  n_despues <- if (isTRUE(celda_n$ok)) .conteo_dbi(celda_n$valor) else NA_real_
  despues_ctrls <- lapply(seq_along(controles), function(i) {
    ctrl <- controles[[i]]
    v_d <- if (isTRUE(ctrl$usar_validos)) {
      cel <- .valor_campo_dbi(consulta$datos, paste0("lupa_est_v_", i))
      if (isTRUE(cel$ok)) .escalar_estabilidad_dbi(cel$valor) else NULL
    } else NULL
    min_d <- if (isTRUE(ctrl$usar_extremos)) {
      cel <- .valor_campo_dbi(consulta$datos, paste0("lupa_est_min_", i))
      if (isTRUE(cel$ok)) .escalar_estabilidad_dbi(cel$valor) else NULL
    } else NULL
    max_d <- if (isTRUE(ctrl$usar_extremos) || isTRUE(ctrl$usar_maximo)) {
      cel <- .valor_campo_dbi(consulta$datos, paste0("lupa_est_max_", i))
      if (isTRUE(cel$ok)) .escalar_estabilidad_dbi(cel$valor) else NULL
    } else NULL
    list(
      columna = ctrl$columna,
      validos = v_d,
      minimo = min_d,
      maximo = max_d
    )
  })
  contador_despues <- if (isTRUE(usar_contador)) {
    .leer_contador_estabilidad_dbi(
      consulta$datos, antes$contador_spec, n_filas = n_despues
    )
  } else NULL
  list(
    ok = is.finite(n_despues) && isTRUE(.conteo_exacto_dbi(n_despues)),
    solo_contador = FALSE,
    sql = sql,
    n_filas = n_despues,
    controles = despues_ctrls,
    contador_despues = contador_despues,
    motivo = if (is.finite(n_despues)) NA_character_ else {
      "El conteo de cierre super\u00f3 la precisi\u00f3n exacta o devolvi\u00f3 NA."
    }
  )
}

# Formatea la estructura publica de los valores antes/despues de estabilidad.
.resumen_valores_estabilidad_dbi <- function(n_filas, controles_lista,
                                             es_antes = TRUE,
                                             contador_valores = NULL) {
  base <- list(n_filas = as.numeric(n_filas))
  if (is.list(contador_valores) && length(contador_valores)) {
    no_na <- contador_valores[!vapply(contador_valores, is.na, logical(1L))]
    if (length(no_na)) base$contador_motor <- no_na
  }
  if (!length(controles_lista)) return(base)
  agregados <- lapply(controles_lista, function(item) {
    val <- if (es_antes) item$antes_validos else item$validos
    mn <- if (es_antes) item$antes_minimo else item$minimo
    mx <- if (es_antes) item$antes_maximo else item$maximo
    list(
      columna = item$columna,
      n_validos = if (is.list(val)) val$num else NA_real_,
      minimo = if (is.list(mn)) mn$texto else NA_character_,
      maximo = if (is.list(mx)) mx$texto else NA_character_
    )
  })
  names(agregados) <- vapply(controles_lista, `[[`, character(1L), "columna")
  base$agregados <- agregados
  base
}

.lista_controles_estabilidad_dbi <- function(antes, comparo_cierre,
                                             solo_contador,
                                             contador_completo,
                                             contador_parcial,
                                             consulta_unica,
                                             n_intermedios = 0L) {
  if (isTRUE(consulta_unica) && n_intermedios <= 1L) {
    return("consulta SQL \u00fanica")
  }
  out <- character()
  if (isTRUE(contador_completo) && is.list(antes) && is.list(antes$contador_spec)) {
    out <- c(out, antes$contador_spec$etiqueta)
  } else if (isTRUE(contador_parcial) && is.list(antes) && is.list(antes$contador_spec)) {
    out <- c(out, "fecha de modificaci\u00f3n del cat\u00e1logo (UPDATE_TIME)")
  }
  if (isTRUE(comparo_cierre) && !isTRUE(solo_contador)) {
    out <- c(out, "conteo de filas (COUNT(*))")
  } else if (isTRUE(consulta_unica) && n_intermedios >= 2L) {
    out <- c(out, "conteo de filas (COUNT(*)) entre consultas consolidadas")
  }
  ctrls <- if (is.list(antes)) antes$controles %||% list() else list()
  for (ctrl in ctrls) {
    if (isTRUE(ctrl$usar_validos) && isTRUE(ctrl$usar_extremos)) {
      out <- c(out, paste0("v\u00e1lidos, m\u00ednimo y m\u00e1ximo de ", ctrl$columna))
    } else if (isTRUE(ctrl$usar_extremos)) {
      out <- c(out, paste0("m\u00ednimo y m\u00e1ximo de ", ctrl$columna))
    } else if (isTRUE(ctrl$usar_maximo)) {
      out <- c(out, paste0("m\u00e1ximo de ", ctrl$columna))
    } else if (isTRUE(ctrl$usar_validos)) {
      out <- c(out, paste0("v\u00e1lidos de ", ctrl$columna))
    }
  }
  unique(out)
}

.evaluar_estabilidad_dbi <- function(tabla, antes = NULL, despues = NULL,
                                     conteos_intermedios = numeric(),
                                     cobertura_cambio = NULL,
                                     muestra_desajuste = FALSE,
                                     consulta_unica = FALSE,
                                     motivo_no_verificada = NULL) {
  id_tabla <- .identificador_tabla_dbi(tabla)
  diferencias <- character()

  # 1. Discrepancias en conteos de consultas consolidadas intermedias.
  inter <- suppressWarnings(as.numeric(conteos_intermedios))
  inter <- inter[is.finite(inter)]
  if (length(inter) >= 2L && length(unique(inter)) > 1L) {
    diferencias <- c(diferencias, paste0(
      "n_total_consulta vari\u00f3 entre consultas consolidadas (",
      paste(unique(vapply(inter, .entero_sql_dbi, character(1L))), collapse = " -> "),
      ")"
    ))
  }

  # 2. Contradiccion entre consultas exactas (n_distintos > n_validos).
  if (is.data.frame(cobertura_cambio) && nrow(cobertura_cambio) > 0L) {
    cols_c <- unique(as.character(cobertura_cambio$columna))
    diferencias <- c(diferencias, paste0(
      "contradicci\u00f3n entre consultas exactas en ",
      paste0("`", cols_c, "`", collapse = ", ")
    ))
  }

  # 3. Comparacion antes vs despues (contador de modificaciones, conteo y
  # maximo de clave indexada).
  n_a <- if (is.list(antes) && length(antes$n_filas) == 1L) {
    as.numeric(antes$n_filas)
  } else NA_real_
  n_d <- if (is.list(despues) && length(despues$n_filas) == 1L) {
    as.numeric(despues$n_filas)
  } else NA_real_
  solo_cnt_cierre <- is.list(despues) && isTRUE(despues$solo_contador)

  cnt_a <- if (is.list(antes)) antes$contador_antes else NULL
  cnt_d <- if (is.list(despues)) despues$contador_despues else NULL
  cnt_spec <- if (is.list(antes)) antes$contador_spec else NULL
  contador_completo <- is.list(antes) && is.list(despues) && isTRUE(despues$ok) &&
    is.list(cnt_spec) && is.list(cnt_a) && is.list(cnt_d) &&
    isTRUE(cnt_a$disponible) && isTRUE(cnt_d$disponible)
  comparo_cierre <- if (isTRUE(solo_cnt_cierre)) {
    isTRUE(contador_completo)
  } else {
    is.list(antes) && is.list(despues) && isTRUE(despues$ok) &&
      is.finite(n_a) && is.finite(n_d)
  }
  contador_parcial <- isTRUE(comparo_cierre) && !isTRUE(solo_cnt_cierre) &&
    is.list(cnt_spec) && is.list(cnt_a) && is.list(cnt_d) &&
    !isTRUE(contador_completo) &&
    (isTRUE(cnt_a$parcial) || isTRUE(cnt_a$disponible)) &&
    (isTRUE(cnt_d$parcial) || isTRUE(cnt_d$disponible))

  if (isTRUE(comparo_cierre)) {
    if (is.list(cnt_spec) && is.list(cnt_a) && is.list(cnt_d)) {
      for (campo in cnt_spec$campos) {
        va <- cnt_a$valores[[campo$clave]] %||% NA_character_
        vd <- cnt_d$valores[[campo$clave]] %||% NA_character_
        if (!is.na(va) && !is.na(vd) && !identical(va, vd)) {
          diferencias <- c(diferencias, paste0(
            campo$etiqueta_dif, ": ", va, " -> ", vd
          ))
        }
      }
    }
    if (!isTRUE(solo_cnt_cierre) && !identical(n_a, n_d)) {
      diferencias <- c(diferencias, paste0(
        "n_filas: ", .entero_sql_dbi(n_a), " -> ", .entero_sql_dbi(n_d)
      ))
    }
    ctrls_a <- antes$controles %||% list()
    ctrls_d <- despues$controles %||% list()
    for (i in seq_len(min(length(ctrls_a), length(ctrls_d)))) {
      ca <- ctrls_a[[i]]
      cd <- ctrls_d[[i]]
      if (isTRUE(ca$usar_validos) && is.list(ca$antes_validos) &&
          is.list(cd$validos) && isTRUE(cd$validos$ok) &&
          !isTRUE(.valores_estabilidad_iguales_dbi(ca$antes_validos, cd$validos))) {
        diferencias <- c(diferencias, paste0(
          "n_validos(", ca$columna, "): ",
          ca$antes_validos$texto, " -> ", cd$validos$texto
        ))
      }
      if (isTRUE(ca$usar_extremos) && is.list(ca$antes_minimo) &&
          is.list(cd$minimo) && isTRUE(cd$minimo$ok) &&
          !isTRUE(.valores_estabilidad_iguales_dbi(ca$antes_minimo, cd$minimo))) {
        diferencias <- c(diferencias, paste0(
          "minimo(", ca$columna, "): ",
          ca$antes_minimo$texto, " -> ", cd$minimo$texto
        ))
      }
      if ((isTRUE(ca$usar_extremos) || isTRUE(ca$usar_maximo)) &&
          is.list(ca$antes_maximo) &&
          is.list(cd$maximo) && isTRUE(cd$maximo$ok) &&
          !isTRUE(.valores_estabilidad_iguales_dbi(ca$antes_maximo, cd$maximo))) {
        diferencias <- c(diferencias, paste0(
          "maximo(", ca$columna, "): ",
          ca$antes_maximo$texto, " -> ", cd$maximo$texto
        ))
      }
    }
  }

  # 4. La lectura de muestra devolvio un numero de filas distinto al esperado.
  if (isTRUE(muestra_desajuste)) {
    diferencias <- c(
      diferencias,
      "la lectura de muestra devolvi\u00f3 un n\u00famero de filas distinto al esperado"
    )
  }

  antes_pub <- if (is.list(antes)) {
    .resumen_valores_estabilidad_dbi(
      n_a, antes$controles %||% list(), es_antes = TRUE,
      contador_valores = if (is.list(cnt_a)) cnt_a$valores else NULL
    )
  } else NULL
  despues_pub <- if (is.list(despues) && isTRUE(despues$ok)) {
    .resumen_valores_estabilidad_dbi(
      n_d, despues$controles %||% list(), es_antes = FALSE,
      contador_valores = if (is.list(cnt_d)) cnt_d$valores else NULL
    )
  } else NULL
  sql_cierre <- if (is.list(despues) && length(despues$sql) == 1L) {
    as.character(despues$sql)
  } else NA_character_
  controles_lista <- .lista_controles_estabilidad_dbi(
    antes, comparo_cierre, solo_cnt_cierre, contador_completo, contador_parcial,
    consulta_unica, n_intermedios = length(inter)
  )
  mecanismo_cnt <- if (isTRUE(contador_completo) && is.list(cnt_spec)) {
    as.character(cnt_spec$mecanismo)
  } else NA_character_

  if (length(diferencias) > 0L) {
    metodo <- if (isTRUE(contador_completo)) {
      paste0("contador_motor_", cnt_spec$mecanismo)
    } else if (isTRUE(comparo_cierre)) {
      if (length(antes$controles %||% list())) {
        "conteo_y_maximo_clave_antes_despues"
      } else {
        "conteo_antes_despues"
      }
    } else if (length(inter) >= 2L && length(unique(inter)) > 1L) {
      "conteos_consolidados"
    } else {
      "contradiccion_entre_consultas"
    }
    return(list(
      estado = "cambio_detectado",
      cambio_detectado = TRUE,
      metodo = metodo,
      alcance_verificacion = if (isTRUE(contador_completo)) {
        "tabla_completa"
      } else "controles_parciales",
      contador_motor = mecanismo_cnt,
      controles = controles_lista,
      tabla = id_tabla,
      antes = antes_pub,
      despues = despues_pub,
      diferencias = diferencias,
      sql = sql_cierre,
      motivo = paste0(
        "La tabla `", id_tabla, "` cambi\u00f3 durante la lectura: ",
        paste(diferencias, collapse = "; "), "."
      )
    ))
  }

  # Una sola consulta SQL es atomica en si misma. Si hubo biseccion en dos o
  # mas subconsultas sin contador del motor, solo se comparo COUNT(*) y cae a
  # `sin_cambio_en_los_controles`.
  if (isTRUE(consulta_unica) && length(inter) <= 1L) {
    return(list(
      estado = "sin_cambio_observado",
      cambio_detectado = FALSE,
      metodo = "consulta_unica",
      alcance_verificacion = "consulta_unica",
      contador_motor = NA_character_,
      controles = controles_lista,
      tabla = id_tabla,
      antes = antes_pub,
      despues = antes_pub,
      diferencias = character(),
      sql = NA_character_,
      motivo = "Todas las m\u00e9tricas de datos salieron de una sola sentencia SQL sobre la tabla."
    ))
  }

  if (isTRUE(comparo_cierre) && isTRUE(contador_completo)) {
    return(list(
      estado = "sin_cambio_observado",
      cambio_detectado = FALSE,
      metodo = paste0("contador_motor_", cnt_spec$mecanismo),
      alcance_verificacion = "tabla_completa",
      contador_motor = mecanismo_cnt,
      controles = controles_lista,
      tabla = id_tabla,
      antes = antes_pub,
      despues = despues_pub,
      diferencias = character(),
      sql = sql_cierre,
      motivo = paste0(
        "Se contrast\u00f3 ", paste(controles_lista, collapse = ", "),
        " al inicio y al cierre sin cambios observados."
      )
    ))
  }

  if (isTRUE(comparo_cierre) || (isTRUE(consulta_unica) && length(inter) >= 2L)) {
    metodo_parcial <- if (!isTRUE(comparo_cierre)) {
      "conteos_consolidados"
    } else if (length(antes$controles %||% list())) {
      "conteo_y_maximo_clave_antes_despues"
    } else {
      "conteo_antes_despues"
    }
    causa_sin_cnt <- if (is.list(cnt_spec) && !is.null(cnt_spec$motivo_inactivo) &&
                         nzchar(cnt_spec$motivo_inactivo)) {
      cnt_spec$motivo_inactivo
    } else {
      "este motor o tipo de objeto no expuso un contador de modificaciones por tabla"
    }
    return(list(
      estado = "sin_cambio_en_los_controles",
      cambio_detectado = NA,
      metodo = metodo_parcial,
      alcance_verificacion = "controles_parciales",
      contador_motor = NA_character_,
      controles = controles_lista,
      tabla = id_tabla,
      antes = antes_pub,
      despues = if (isTRUE(comparo_cierre)) despues_pub else antes_pub,
      diferencias = character(),
      sql = sql_cierre,
      motivo = paste0(
        "Sin cambios en los controles verificados (",
        paste(controles_lista, collapse = ", "),
        "); ", causa_sin_cnt,
        ", por lo que un cambio fuera de esos controles no se detecta."
      )
    ))
  }

  motivo_final <- if (length(motivo_no_verificada) == 1L &&
                      !is.na(motivo_no_verificada) &&
                      nzchar(motivo_no_verificada)) {
    as.character(motivo_no_verificada)
  } else if (is.list(despues) && length(despues$motivo) == 1L &&
             !is.na(despues$motivo) && nzchar(despues$motivo)) {
    paste0("No se pudo verificar la estabilidad al cierre: ", despues$motivo)
  } else if (is.list(antes) && !isTRUE(antes$conteo_exacto)) {
    "No se pudo verificar la estabilidad: el conteo inicial no estuvo disponible o super\u00f3 la precisi\u00f3n exacta."
  } else {
    "No se realiz\u00f3 una verificaci\u00f3n de estabilidad al cierre."
  }
  list(
    estado = "no_verificada",
    cambio_detectado = NA,
    metodo = "no_disponible",
    alcance_verificacion = "no_verificada",
    contador_motor = NA_character_,
    controles = character(),
    tabla = id_tabla,
    antes = antes_pub,
    despues = despues_pub,
    diferencias = character(),
    sql = sql_cierre,
    motivo = motivo_final
  )
}

# Garantiza en `.sellar_perfil_dbi()` que todo `perfil_dbi` publique
# `resumen_tabla$meta$estabilidad` en forma canonica y refleje cualquier
# contradiccion de consistencia ya anotada en `resumen_tabla$cobertura`.
.normalizar_estabilidad_sello_dbi <- function(estructura) {
  if (!is.list(estructura) || !is.list(estructura$resumen_tabla) ||
      !is.list(estructura$resumen_tabla$meta)) {
    return(estructura)
  }
  meta <- estructura$resumen_tabla$meta
  est <- meta$estabilidad
  tabla_id <- if (length(meta$tabla) == 1L && !is.na(meta$tabla)) {
    as.character(meta$tabla)
  } else NA_character_
  if (!is.list(est) || is.null(est$estado)) {
    est <- list(
      estado = "no_verificada",
      cambio_detectado = NA,
      metodo = "no_disponible",
      alcance_verificacion = "no_verificada",
      contador_motor = NA_character_,
      controles = character(),
      tabla = tabla_id,
      antes = NULL,
      despues = NULL,
      diferencias = character(),
      sql = NA_character_,
      motivo = "No se realiz\u00f3 verificaci\u00f3n de estabilidad en esta v\u00eda."
    )
  }
  if (is.null(est$alcance_verificacion)) {
    est$alcance_verificacion <- if (identical(est$estado, "no_verificada")) {
      "no_verificada"
    } else if (identical(est$estado, "sin_cambio_en_los_controles")) {
      "controles_parciales"
    } else if (identical(est$metodo, "consulta_unica")) {
      "consulta_unica"
    } else "tabla_completa"
  }
  if (is.null(est$contador_motor)) est$contador_motor <- NA_character_
  if (is.null(est$controles)) est$controles <- character()

  cob <- estructura$resumen_tabla$cobertura
  if (is.data.frame(cob) && nrow(cob) > 0L && "bloque" %in% names(cob) &&
      any(as.character(cob$bloque) == "consistencia") &&
      !isTRUE(est$cambio_detectado)) {
    est$estado <- "cambio_detectado"
    est$cambio_detectado <- TRUE
    if (!length(est$diferencias)) {
      est$diferencias <- "contradicci\u00f3n de consistencia registrada en cobertura"
    }
    est$motivo <- paste0(
      "Se registraron contradicciones entre consultas durante la lectura de `",
      est$tabla %||% tabla_id, "`."
    )
  }
  # No exponer literales de columnas protegidas ni cuando `incluir_valores = FALSE`.
  protegidas <- if (is.list(meta$proteccion_personal) &&
                    isTRUE(meta$proteccion_personal$aplicada)) {
    as.character(meta$proteccion_personal$columnas %||% character())
  } else character()
  ocultar_todos <- identical(meta$incluir_valores, FALSE)
  limpiar_lado <- function(lado) {
    if (!is.list(lado) || !is.list(lado$agregados)) return(lado)
    for (nm in names(lado$agregados)) {
      if (ocultar_todos || (length(protegidas) && .nombres_presentes(nm, protegidas))) {
        lado$agregados[[nm]]$minimo <- NA_character_
        lado$agregados[[nm]]$maximo <- NA_character_
      }
    }
    lado
  }
  est$antes <- limpiar_lado(est$antes)
  est$despues <- limpiar_lado(est$despues)
  if ((ocultar_todos || length(protegidas)) && length(est$diferencias)) {
    est$diferencias <- vapply(est$diferencias, function(d) {
      if (grepl("^(minimo|maximo)\\(", d, perl = TRUE, useBytes = TRUE)) {
        col_d <- sub("^(?:minimo|maximo)\\(([^)]+)\\).*$", "\\1", d, perl = TRUE)
        if (ocultar_todos || .nombres_presentes(col_d, protegidas)) {
          return(paste0(
            sub("^([a-z]+\\([^)]+\\)).*$", "\\1", d, perl = TRUE),
            ": cambio observado (valor oculto por privacidad)"
          ))
        }
      }
      d
    }, character(1L), USE.NAMES = FALSE)
    if (identical(est$estado, "cambio_detectado")) {
      est$motivo <- paste0(
        "La tabla `", est$tabla %||% tabla_id, "` cambi\u00f3 durante la lectura: ",
        paste(est$diferencias, collapse = "; "), "."
      )
    }
  }
  estructura$resumen_tabla$meta$estabilidad <- est
  estructura
}

.texto_estabilidad_dbi <- function(estabilidad) {
  if (!is.list(estabilidad) || is.null(estabilidad$estado)) return(NULL)
  estado <- as.character(estabilidad$estado[[1L]])
  if (identical(estado, "cambio_detectado")) {
    return(paste(
      "Estabilidad de la tabla: cambio detectado durante la lectura",
      "(ver `resumen_tabla$meta$estabilidad`)."
    ))
  }
  if (identical(estado, "sin_cambio_observado")) {
    etiqueta <- if (identical(estabilidad$metodo, "consulta_unica")) {
      "sin cambios observados (consulta \u00fanica)"
    } else if (length(estabilidad$contador_motor) == 1L &&
               !is.na(estabilidad$contador_motor) &&
               nzchar(estabilidad$contador_motor)) {
      "sin cambios observados (contador de modificaciones del motor)"
    } else {
      "sin cambios observados"
    }
    return(paste0("Estabilidad de la tabla: ", etiqueta, "."))
  }
  if (identical(estado, "sin_cambio_en_los_controles")) {
    ctrls <- as.character(estabilidad$controles %||% character())
    detalle <- if (length(ctrls)) {
      paste0(paste(ctrls, collapse = ", "), "; ")
    } else ""
    return(paste0(
      "Estabilidad de la tabla: sin cambios en los controles verificados (",
      detalle, "no descarta cambios fuera de esos controles)."
    ))
  }
  "Estabilidad de la tabla: no verificada (ver `resumen_tabla$meta$estabilidad`)."
}
