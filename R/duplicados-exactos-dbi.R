# Conteo de filas duplicadas exactas con regla canonica compartida entre R y
# los motores SQL.
#
# Sin una regla explicita, cada motor agrupa distinto: intercalaciones `_ci` o
# `NOCASE` juntan `'A'` y `'a'`; `PAD SPACE` junta `'x'` y `'x  '` en VARCHAR;
# `CHAR(n)` rellena con espacios en unos motores y no en otros; los decimales
# anchos colapsan al pasarse a `double` en R; y las marcas temporales
# sub-microsegundo pierden precision en `POSIXct`.
#
# Cuando el catalogo garantiza unicidad total de fila (PK o clave/indice unico
# de grado total), las duplicadas son 0 sin consultar la tabla. Cuando el motor
# no puede reproducir la regla para alguna columna, la cifra queda `NA` con el
# motivo explicito: nunca un numero que mide otra cosa.

.CRITERIO_DUPLICADOS_EXACTOS_DBI <- paste0(
  "Se comparan los valores tal como los guarda el motor: texto VARCHAR/TEXT ",
  "byte a byte (may\u00fasculas, acentos y espacios finales distintos, sin PAD SPACE); ",
  "CHAR sin los espacios finales de relleno y con may\u00fasculas y acentos distintos; ",
  "decimales por valor num\u00e9rico exacto (1.0 igual a 1.00, sin redondeo a doble); ",
  "punto flotante por valor almacenado (-0.0 igual a 0.0); fechas y horas con toda ",
  "la precisi\u00f3n del motor (con zona horaria por instante UTC); NULL igual a NULL."
)

.tipo_declarado_sql_columna_dbi <- function(sql_tabla, campo) {
  if (length(sql_tabla) != 1L || is.na(sql_tabla) || !nzchar(sql_tabla) ||
      length(campo) != 1L || is.na(campo) || !nzchar(campo)) {
    return(NA_character_)
  }
  inicio <- regexpr("(", sql_tabla, fixed = TRUE, useBytes = TRUE)
  if (inicio < 1L) return(NA_character_)
  resto <- substring(sql_tabla, inicio + 1L)
  chars <- strsplit(resto, "", fixed = TRUE)[[1L]]
  partes <- character()
  actual <- character()
  nivel <- 0L
  cierre <- ""
  for (ch in chars) {
    if (nzchar(cierre)) {
      actual <- c(actual, ch)
      if (ch == cierre) cierre <- ""
      next
    }
    if (ch %in% c("\"", "'", "`", "[")) {
      cierre <- if (ch == "[") "]" else ch
      actual <- c(actual, ch)
      next
    }
    if (ch == "(") {
      nivel <- nivel + 1L
      actual <- c(actual, ch)
      next
    }
    if (ch == ")") {
      if (nivel == 0L) break
      nivel <- nivel - 1L
      actual <- c(actual, ch)
      next
    }
    if (ch == "," && nivel == 0L) {
      partes <- c(partes, paste(actual, collapse = ""))
      actual <- character()
      next
    }
    actual <- c(actual, ch)
  }
  if (length(actual)) partes <- c(partes, paste(actual, collapse = ""))
  objetivo <- .normalizacion_minusculas_vector(.nombres_para_operar(campo))
  for (parte in partes) {
    limpio <- trimws(parte)
    if (!nzchar(limpio)) next
    if (grepl("^(CONSTRAINT|PRIMARY|UNIQUE|FOREIGN|CHECK)\\b",
              .mayusculas_ascii(limpio), perl = TRUE)) next
    m <- regexpr("^(\"[^\"]+\"|`[^`]+`|\\[[^\\]]+\\]|[^[:space:]]+)\\s+(.+)$",
                 limpio, perl = TRUE)
    if (m < 1L) next
    nombre_raw <- sub("\\s+.*$", "", limpio, perl = TRUE)
    nombre_limpio <- .quitar_comillas_identificador(nombre_raw)
    if (identical(.normalizacion_minusculas_vector(.nombres_para_operar(nombre_limpio)),
                  objetivo)) {
      definicion <- trimws(sub("^(\"[^\"]+\"|`[^`]+`|\\[[^\\]]+\\]|[^[:space:]]+)\\s+",
                               "", limpio, perl = TRUE))
      return(definicion)
    }
  }
  NA_character_
}

.es_tipo_char_fijo_dbi <- function(tipo_motor = NULL, tipo_declarado = NULL,
                                   sql_tabla = NULL, campo = NULL,
                                   tipos_pragma = NULL) {
  candidatos <- character()
  if (!is.null(tipos_pragma) && !is.null(campo) && length(tipos_pragma)) {
    pos <- .indice_nombre(campo, names(tipos_pragma))
    if (!is.na(pos)) {
      tp <- trimws(as.character(tipos_pragma[[pos]]))
      if (!is.na(tp) && nzchar(tp)) candidatos <- c(candidatos, tp)
    }
  }
  if (length(tipo_motor) == 1L && !is.na(tipo_motor) && nzchar(trimws(tipo_motor))) {
    candidatos <- c(candidatos, as.character(tipo_motor))
  }
  if (!is.null(sql_tabla) && !is.null(campo)) {
    def <- .tipo_declarado_sql_columna_dbi(sql_tabla, campo)
    if (!is.na(def)) candidatos <- c(candidatos, def)
  }
  if (!length(candidatos) && length(tipo_declarado) == 1L &&
      !is.na(tipo_declarado) && nzchar(trimws(tipo_declarado))) {
    td <- .minusculas_ascii(trimws(as.character(tipo_declarado)))
    if (!identical(td, "character")) candidatos <- c(candidatos, td)
  }
  for (cand in candidatos) {
    base <- .minusculas_ascii(trimws(cand))
    if (grepl("^(var|nvar|character\\s+varying)", base, perl = TRUE)) next
    if (grepl("^(bpchar|char|nchar|character)(\\s|\\(|$)", base, perl = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

.es_tipo_texto_duplicados_dbi <- function(tipo, prototipo = NULL) {
  if (length(tipo) == 1L && !is.na(tipo) && nzchar(tipo)) {
    base <- .minusculas_ascii(trimws(as.character(tipo)))
    if (grepl("binary|blob|bytea|raw|image", base, perl = TRUE)) return(FALSE)
    if (grepl("date|time|interval", base, perl = TRUE)) return(FALSE)
    if (grepl("char|text|clob|string|citext|name|enum|set", base, perl = TRUE)) {
      return(TRUE)
    }
  }
  inherits(prototipo, c("character", "factor"))
}

.tipo_no_comparable_duplicados_dbi <- function(motor, tipo) {
  if (length(tipo) != 1L || is.na(tipo) || !nzchar(tipo)) return(FALSE)
  base <- .minusculas_ascii(trimws(as.character(tipo)))
  if (identical(motor, "postgresql")) {
    return(grepl("^(xml|json|point|line|lseg|box|path|polygon|circle)$",
                 base, perl = TRUE))
  }
  if (identical(motor, "sqlserver")) {
    return(grepl("^(text|ntext|image|xml|geography|geometry|hierarchyid)$",
                 base, perl = TRUE))
  }
  if (motor %in% c("mysql", "mariadb")) {
    return(grepl("^(geometry|point|linestring|polygon|multipoint|multilinestring|multipolygon|geometrycollection)$",
                 base, perl = TRUE))
  }
  # Los tipos anidados de DuckDB (`STRUCT(...)`, `MAP(...)`, `UNION(...)`, las
  # listas `INTEGER[]` y los arreglos `INTEGER[3]`) no tienen una conversion a
  # bytes: se declaran antes de consultar, en vez de dejar que la consulta falle.
  if (identical(motor, "duckdb")) {
    return(grepl("^(struct|map|union)[[:space:]]*[(]|[]][[:space:]]*$",
                 base, perl = TRUE))
  }
  FALSE
}

.es_tipo_submicro_dbi <- function(tipo) {
  if (length(tipo) != 1L || is.na(tipo) || !nzchar(tipo)) return(FALSE)
  base <- .minusculas_ascii(gsub("[[:space:]]+", "", as.character(tipo)))
  grepl("timestamp_ns|datetime2\\(7\\)|datetimeoffset\\(7\\)|datetime2$|datetimeoffset$",
        base, perl = TRUE)
}

.expresion_duplicados_columna_dbi <- function(motor, campo, campo_sql,
                                              tipo_motor, tipo_declarado,
                                              prototipo = NULL,
                                              sql_tabla = NULL,
                                              tipos_pragma = NULL) {
  tipo <- if (length(tipo_motor) == 1L && !is.na(tipo_motor) && nzchar(tipo_motor)) {
    as.character(tipo_motor)
  } else if (length(tipo_declarado) == 1L && !is.na(tipo_declarado)) {
    as.character(tipo_declarado)
  } else NA_character_

  if (.tipo_no_comparable_duplicados_dbi(motor, tipo)) {
    return(list(
      ok = FALSE, expr = NA_character_, regla = "no_comparable",
      motivo = paste0(
        "La columna `", campo, "` tiene tipo `", tipo,
        "`, que no admite agrupaci\u00f3n exacta en `", motor, "`."
      )
    ))
  }

  es_char <- .es_tipo_char_fijo_dbi(
    tipo_motor = tipo_motor, tipo_declarado = tipo_declarado,
    sql_tabla = sql_tabla, campo = campo, tipos_pragma = tipos_pragma
  )
  es_texto <- es_char || .es_tipo_texto_duplicados_dbi(tipo, prototipo)

  if (es_texto) {
    if (identical(motor, "postgresql")) {
      expr <- if (es_char) {
        paste0("RTRIM(CAST(", campo_sql, " AS TEXT)) COLLATE \"C\"")
      } else {
        paste0("CAST(", campo_sql, " AS TEXT) COLLATE \"C\"")
      }
      return(list(
        ok = TRUE, expr = expr,
        regla = if (es_char) "char_sin_espacios_finales_binario" else "texto_binario",
        motivo = NA_character_
      ))
    }
    if (motor %in% c("mysql", "mariadb")) {
      expr <- if (es_char) {
        paste0("CAST(TRIM(TRAILING ' ' FROM ", campo_sql, ") AS BINARY)")
      } else {
        paste0("CAST(", campo_sql, " AS BINARY)")
      }
      return(list(
        ok = TRUE, expr = expr,
        regla = if (es_char) "char_sin_espacios_finales_binario" else "texto_binario",
        motivo = NA_character_
      ))
    }
    if (identical(motor, "sqlserver")) {
      expr <- if (es_char) {
        paste0("CONVERT(varbinary(max), RTRIM(", campo_sql, "))")
      } else {
        paste0("CONVERT(varbinary(max), ", campo_sql, ")")
      }
      return(list(
        ok = TRUE, expr = expr,
        regla = if (es_char) "char_sin_espacios_finales_binario" else "texto_binario",
        motivo = NA_character_
      ))
    }
    if (identical(motor, "sqlite")) {
      expr <- if (es_char) {
        paste0("RTRIM(", campo_sql, ", ' ') COLLATE BINARY")
      } else {
        paste0(campo_sql, " COLLATE BINARY")
      }
      return(list(
        ok = TRUE, expr = expr,
        regla = if (es_char) "char_sin_espacios_finales_binario" else "texto_binario",
        motivo = NA_character_
      ))
    }
    if (identical(motor, "duckdb")) {
      expr <- if (es_char) {
        paste0("CAST(RTRIM(", campo_sql, ", ' ') AS BLOB)")
      } else {
        paste0("CAST(", campo_sql, " AS BLOB)")
      }
      return(list(
        ok = TRUE, expr = expr,
        regla = if (es_char) "char_sin_espacios_finales_binario" else "texto_binario",
        motivo = NA_character_
      ))
    }
    return(list(
      ok = FALSE, expr = NA_character_, regla = "no_reproducible",
      motivo = paste0(
        "El motor `", motor, "` no tiene una traducci\u00f3n verificada de comparaci\u00f3n ",
        "binaria exacta para la columna textual `", campo, "`."
      )
    ))
  }

  regla <- if (.tipo_declarado_decimal_dbi(tipo)) {
    "decimal_exacto"
  } else if (!is.na(tipo) && grepl("date|time|timestamp", .minusculas_ascii(tipo),
                                   perl = TRUE)) {
    "temporal_nativo"
  } else if (!is.na(tipo) && grepl("float|double|real", .minusculas_ascii(tipo),
                                   perl = TRUE)) {
    "flotante_nativo"
  } else {
    "valor_nativo"
  }
  list(ok = TRUE, expr = campo_sql, regla = regla, motivo = NA_character_)
}

.planificar_duplicados_exactos_dbi <- function(conexion, campos, campos_sql,
                                               tipos_motor, tipos_declarados,
                                               prototipo, catalogo_garantias,
                                               universo, metricas_ejecucion,
                                               ilegibles = character(),
                                               enmascaradas = character(),
                                               sql_tabla = NULL,
                                               tipos_pragma = NULL) {
  motor <- .motor_clave_primaria(conexion)
  ident <- if (is.list(catalogo_garantias)) catalogo_garantias$identificacion else NULL
  garantizado_unico <- identical(universo, "tabla_completa") &&
    is.list(ident) && identical(ident$grado, "total") &&
    isTRUE(ident$tiene_clave)

  if (garantizado_unico) {
    return(list(
      modo = "garantizado",
      n_consultas = 0L,
      fuente = ident$tipo,
      columnas_clave = ident$columnas,
      expresiones = character(),
      regla_columnas = data.frame(
        columna = character(), regla = character(), stringsAsFactors = FALSE
      ),
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      motivo = NA_character_
    ))
  }

  if (!("distintos" %in% metricas_ejecucion)) {
    return(list(
      modo = "no_solicitado",
      n_consultas = 0L,
      fuente = NA_character_,
      expresiones = character(),
      regla_columnas = data.frame(
        columna = character(), regla = character(), stringsAsFactors = FALSE
      ),
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      motivo = "No se solicit\u00f3 `distintos` en `metricas`; no se emiti\u00f3 el agrupamiento de filas."
    ))
  }

  if (length(ilegibles)) {
    return(list(
      modo = "no_disponible",
      n_consultas = 0L,
      fuente = NA_character_,
      expresiones = character(),
      regla_columnas = data.frame(
        columna = character(), regla = character(), stringsAsFactors = FALSE
      ),
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      motivo = paste0(
        "No se cuentan filas duplicadas exactas porque ", length(ilegibles),
        if (length(ilegibles) == 1L) " columna es ilegible" else " columnas son ilegibles",
        " (", paste(ilegibles, collapse = ", "), "): comparar s\u00f3lo el subconjunto medir\u00eda otra cosa."
      )
    ))
  }

  if (length(enmascaradas)) {
    return(list(
      modo = "no_disponible",
      n_consultas = 0L,
      fuente = NA_character_,
      expresiones = character(),
      regla_columnas = data.frame(
        columna = character(), regla = character(), stringsAsFactors = FALSE
      ),
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      motivo = paste0(
        "No se cuentan filas duplicadas exactas porque ", length(enmascaradas),
        if (length(enmascaradas) == 1L) " columna llega enmascarada" else " columnas llegan enmascaradas",
        " (", paste(enmascaradas, collapse = ", "), ")."
      )
    ))
  }

  if (!length(campos) || !length(campos_sql)) {
    return(list(
      modo = "no_disponible",
      n_consultas = 0L,
      fuente = NA_character_,
      expresiones = character(),
      regla_columnas = data.frame(
        columna = character(), regla = character(), stringsAsFactors = FALSE
      ),
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      motivo = "La tabla no tiene columnas comparables para contar filas duplicadas."
    ))
  }

  exprs <- character(length(campos))
  reglas <- character(length(campos))
  for (i in seq_along(campos)) {
    campo <- campos[[i]]
    tm <- .atributo_columna_dbi(tipos_motor, campo, i) %||% NA_character_
    td <- .atributo_columna_dbi(tipos_declarados, campo, i) %||% NA_character_
    proto <- .atributo_columna_dbi(prototipo, campo, i)
    col_sql <- as.character(campos_sql[[i]])
    # Usar siempre el identificador de columna, no una expresion con alias
    col_sql <- sub(
      "\\s+AS\\s+(?:\"(?:\"\"|[^\"])*\"|\\[(?:\\]\\]|[^]])*\\]|`(?:``|[^`])*`|[^[:space:]]+)\\s*$",
      "", col_sql, perl = TRUE
    )
    res <- .expresion_duplicados_columna_dbi(
      motor = motor, campo = campo, campo_sql = col_sql,
      tipo_motor = tm, tipo_declarado = td, prototipo = proto,
      sql_tabla = sql_tabla, tipos_pragma = tipos_pragma
    )
    if (!isTRUE(res$ok)) {
      return(list(
        modo = "no_disponible",
        n_consultas = 0L,
        fuente = NA_character_,
        expresiones = character(),
        regla_columnas = data.frame(
          columna = character(), regla = character(), stringsAsFactors = FALSE
        ),
        criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
        motivo = res$motivo
      ))
    }
    exprs[[i]] <- res$expr
    reglas[[i]] <- res$regla
  }

  list(
    modo = "consulta",
    n_consultas = 1L,
    fuente = "sql_groupby_canonico",
    expresiones = exprs,
    regla_columnas = data.frame(
      columna = as.character(campos),
      regla = reglas,
      stringsAsFactors = FALSE
    ),
    criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
    motivo = NA_character_
  )
}

.calcular_duplicados_exactos_dbi <- function(conexion, tabla_metricas_sql,
                                             plan_dup, dialecto, universo,
                                             presupuesto = NULL) {
  alcance <- if (identical(universo, "muestra_motor")) "muestra" else "tabla_completa"
  if (!is.list(plan_dup) || identical(plan_dup$modo, "no_solicitado")) {
    return(list(
      filas_duplicadas = NA_integer_,
      filas_en_grupos_duplicados = NA_integer_,
      estado = "no_solicitado",
      alcance = alcance,
      fuente = NA_character_,
      criterio = .CRITERIO_DUPLICADOS_EXACTOS_DBI,
      regla_columnas = if (is.list(plan_dup)) plan_dup$regla_columnas else NULL,
      motivo = if (is.list(plan_dup)) plan_dup$motivo else NA_character_,
      sql = NA_character_
    ))
  }
  if (identical(plan_dup$modo, "garantizado")) {
    return(list(
      filas_duplicadas = 0L,
      filas_en_grupos_duplicados = 0L,
      estado = "garantizado",
      alcance = alcance,
      fuente = plan_dup$fuente,
      criterio = plan_dup$criterio,
      regla_columnas = plan_dup$regla_columnas,
      motivo = paste0(
        "Unicidad de fila garantizada por ", plan_dup$fuente, " (`",
        paste(plan_dup$columnas_clave, collapse = "`, `"), "`)."
      ),
      sql = NA_character_
    ))
  }
  if (!identical(plan_dup$modo, "consulta")) {
    return(list(
      filas_duplicadas = NA_integer_,
      filas_en_grupos_duplicados = NA_integer_,
      estado = "no_disponible",
      alcance = alcance,
      fuente = NA_character_,
      criterio = plan_dup$criterio,
      regla_columnas = plan_dup$regla_columnas,
      motivo = plan_dup$motivo,
      sql = NA_character_
    ))
  }

  senas <- .senas_conexion_dbi(conexion)
  alias_n <- .citar_identificador_dbi(conexion, "lupa_dup_n")
  alias_ex <- .citar_identificador_dbi(conexion, "filas_duplicadas")
  alias_gr <- .citar_identificador_dbi(conexion, "filas_en_grupos_duplicados")
  sql <- paste0(
    "SELECT ",
    .conteo_sql_dbi(paste0("COALESCE(SUM(", alias_n, " - 1), 0)"), senas),
    " AS ", alias_ex, ", ",
    .conteo_sql_dbi(paste0("COALESCE(SUM(", alias_n, "), 0)"), senas),
    " AS ", alias_gr,
    " FROM (SELECT ", .conteo_sql_dbi("COUNT(*)", senas), " AS ", alias_n,
    " FROM ", tabla_metricas_sql,
    " GROUP BY ", paste(plan_dup$expresiones, collapse = ", "),
    " HAVING COUNT(*) > 1)", dialecto$alias_tabla("lupa_dup_grupos")
  )
  resp <- .consultar_dbi(conexion, sql, presupuesto, etapa = "duplicados_exactos")
  if (!isTRUE(resp$ok) || !is.data.frame(resp$datos) || !nrow(resp$datos)) {
    return(list(
      filas_duplicadas = NA_integer_,
      filas_en_grupos_duplicados = NA_integer_,
      estado = "no_disponible",
      alcance = alcance,
      fuente = plan_dup$fuente,
      criterio = plan_dup$criterio,
      regla_columnas = plan_dup$regla_columnas,
      motivo = paste0(
        "No se pudieron contar las filas duplicadas exactas en el motor: ",
        resp$motivo %||% "consulta sin resultado."
      ),
      sql = sql
    ))
  }
  c_ex <- .valor_campo_dbi(resp$datos, "filas_duplicadas")
  c_gr <- .valor_campo_dbi(resp$datos, "filas_en_grupos_duplicados")
  v_ex <- if (isTRUE(c_ex$ok)) .conteo_dbi(c_ex$valor) else NA_real_
  v_gr <- if (isTRUE(c_gr$ok)) .conteo_dbi(c_gr$valor) else NA_real_
  if (is.na(v_ex) || is.na(v_gr)) {
    return(list(
      filas_duplicadas = NA_integer_,
      filas_en_grupos_duplicados = NA_integer_,
      estado = "no_disponible",
      alcance = alcance,
      fuente = plan_dup$fuente,
      criterio = plan_dup$criterio,
      regla_columnas = plan_dup$regla_columnas,
      motivo = "La consulta de filas duplicadas no devolvi\u00f3 conteos utilizables.",
      sql = sql
    ))
  }
  list(
    filas_duplicadas = v_ex,
    filas_en_grupos_duplicados = v_gr,
    estado = if (identical(universo, "muestra_motor")) "estimado" else "calculado",
    alcance = alcance,
    fuente = plan_dup$fuente,
    criterio = plan_dup$criterio,
    regla_columnas = plan_dup$regla_columnas,
    motivo = NA_character_,
    sql = sql
  )
}

# Normalizacion decimal exacta como cadena: `"1.0"` y `"1.00"` -> `"1"`,
# `"-0.00"` -> `"0"`, conservando todos los digitos significativos (>15).
.normalizar_decimal_canonico_r <- function(x) {
  texto <- trimws(as.character(x))
  salida <- texto
  ok <- !is.na(texto) & grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)$",
                              texto, perl = TRUE)
  if (!any(ok)) return(salida)
  vals <- texto[ok]
  neg <- startsWith(vals, "-")
  cuerpo <- sub("^[+-]", "", vals, perl = TRUE)
  tiene_punto <- grepl(".", cuerpo, fixed = TRUE, useBytes = TRUE)
  ent <- ifelse(tiene_punto, sub("\\..*$", "", cuerpo, perl = TRUE), cuerpo)
  frac <- ifelse(tiene_punto, sub("^[^.]*\\.", "", cuerpo, perl = TRUE), "")
  ent <- sub("^0+", "", ent, perl = TRUE)
  ent[!nzchar(ent)] <- "0"
  frac <- sub("0+$", "", frac, perl = TRUE)
  res <- ifelse(nzchar(frac), paste0(ent, ".", frac), ent)
  es_cero <- res == "0"
  res <- ifelse(neg & !es_cero, paste0("-", res), res)
  salida[ok] <- res
  salida
}

.reconciliar_duplicados_muestra_dbi <- function(perfil, datos_crudos,
                                                datos_muestra, campos,
                                                tipos_declarados, tipos_motor,
                                                columnas_castadas,
                                                sql_tabla = NULL,
                                                tipos_pragma = NULL,
                                                duplicados_sql = NULL,
                                                tabla_completa = FALSE) {
  if (!is.list(perfil) || !is.data.frame(datos_muestra) || !nrow(datos_muestra)) {
    return(perfil)
  }
  necesita <- FALSE
  trabajo <- datos_muestra
  cols_submicro <- character()

  for (i in seq_along(campos)) {
    campo <- campos[[i]]
    pos <- match(campo, names(trabajo))
    if (is.na(pos)) next
    tm <- .atributo_columna_dbi(tipos_motor, campo, i) %||% NA_character_
    td <- .atributo_columna_dbi(tipos_declarados, campo, i) %||% NA_character_
    tipo <- if (!is.na(tm) && nzchar(tm)) tm else td

    # 1) CHAR fijo: quitar espacios finales de relleno para igualar la regla SQL
    if (.es_tipo_char_fijo_dbi(tipo_motor = tm, tipo_declarado = td,
                               sql_tabla = sql_tabla, campo = campo,
                               tipos_pragma = tipos_pragma) &&
        is.character(trabajo[[pos]])) {
      recortado <- sub(" +$", "", trabajo[[pos]], perl = TRUE)
      if (!identical(recortado, trabajo[[pos]])) {
        trabajo[[pos]] <- recortado
        necesita <- TRUE
      }
    }

    # 2) Decimal ancho traido como texto y luego redondeado a double:
    # recuperar los digitos exactos desde `datos_crudos` antes de `as.numeric()`.
    if (.tipo_declarado_decimal_dbi(tipo) && campo %in% columnas_castadas &&
        is.data.frame(datos_crudos) && campo %in% names(datos_crudos) &&
        is.numeric(trabajo[[pos]])) {
      canon <- .normalizar_decimal_canonico_r(datos_crudos[[campo]])
      # Solo activar si el double realmente perdio distinciones entre textos
      num_conv <- trabajo[[pos]]
      n_dist_canon <- length(unique(canon[!is.na(canon)]))
      n_dist_num <- length(unique(num_conv[!is.na(num_conv)]))
      if (n_dist_canon != n_dist_num) {
        trabajo[[pos]] <- canon
        necesita <- TRUE
      }
    }

    # 3) Timestamp sub-microsegundo convertido a POSIXct (double pierde <~238 ns)
    if (.es_tipo_submicro_dbi(tipo) && inherits(trabajo[[pos]], "POSIXct")) {
      cols_submicro <- c(cols_submicro, campo)
    }
  }

  # Si hubo columnas sub-microsegundo en POSIXct:
  # - si la muestra es la tabla completa y SQL calculo el exacto, usar SQL;
  # - si no, no publicar un conteo potencialmente inflado por redondeo de double.
  if (length(cols_submicro)) {
    if (isTRUE(tabla_completa) && is.list(duplicados_sql) &&
        duplicados_sql$estado %in% c("calculado", "garantizado") &&
        !is.na(duplicados_sql$filas_duplicadas)) {
      nuevo_dup <- as.integer(duplicados_sql$filas_duplicadas)
      nuevo_grp <- as.integer(duplicados_sql$filas_en_grupos_duplicados)
      if (!identical(as.integer(perfil$general$filas_duplicadas), nuevo_dup)) {
        perfil <- .actualizar_duplicados_perfil_muestra(
          perfil, nuevo_dup, nuevo_grp, nrow(datos_muestra)
        )
      }
      return(perfil)
    }
    perfil <- .actualizar_duplicados_perfil_muestra(
      perfil, NA_integer_, NA_integer_, nrow(datos_muestra)
    )
    cob <- .nuevo_diagnostico_no_evaluado(
      "filas_duplicadas", paste(cols_submicro, collapse = ", "),
      paste0(
        "Las marcas temporales de precisi\u00f3n sub-microsegundo (",
        paste(cols_submicro, collapse = ", "),
        ") se leyeron como POSIXct (doble) con p\u00e9rdida de resoluci\u00f3n; ",
        "no se publica un conteo de duplicadas que podr\u00eda confundir instantes distintos."
      ),
      "Consultar `resumen_tabla$meta$duplicados_exactos`, calculado en el motor con precisi\u00f3n nativa."
    )
    perfil$cobertura_diagnosticos <- rbind(
      perfil$cobertura_diagnosticos, cob
    )
    rownames(perfil$cobertura_diagnosticos) <- NULL
    return(perfil)
  }

  if (!isTRUE(necesita)) return(perfil)

  conteos <- .conteos_filas_duplicadas(trabajo)
  nuevo_dup <- conteos$filas_duplicadas
  nuevo_grp <- conteos$filas_en_grupos_duplicados
  if (!identical(perfil$general$filas_duplicadas, nuevo_dup) ||
      !identical(perfil$general$filas_en_grupos_duplicados, nuevo_grp)) {
    perfil <- .actualizar_duplicados_perfil_muestra(
      perfil, nuevo_dup, nuevo_grp, nrow(datos_muestra)
    )
  }
  perfil
}

.actualizar_duplicados_perfil_muestra <- function(perfil, n_dup, n_grp, n_filas) {
  perfil$general$filas_duplicadas <- n_dup
  perfil$general$filas_en_grupos_duplicados <- n_grp
  if (is.data.frame(perfil$hallazgos) && "tipo_hallazgo" %in% names(perfil$hallazgos)) {
    es_dup <- as.character(perfil$hallazgos$tipo_hallazgo) == "filas_duplicadas"
    if (is.na(n_dup) || n_dup <= 0L) {
      if (any(es_dup)) {
        perfil$hallazgos <- perfil$hallazgos[!es_dup, , drop = FALSE]
        rownames(perfil$hallazgos) <- NULL
      }
    } else if (any(es_dup)) {
      idx <- which(es_dup)[[1L]]
      perfil$hallazgos$evidencia[[idx]] <- paste0(
        n_grp,
        if (identical(as.numeric(n_grp), 1)) " fila en grupos duplicados (" else
          " filas en grupos duplicados (",
        n_dup,
        if (identical(as.numeric(n_dup), 1)) " excedente)" else " excedentes)"
      )
      if ("n_afectados" %in% names(perfil$hallazgos)) {
        perfil$hallazgos$n_afectados[[idx]] <- n_grp
      }
    } else {
      nuevo <- .nuevo_hallazgo(
        NA_character_, "filas_duplicadas", "error",
        "La tabla contiene filas duplicadas exactas.",
        paste0(
          n_grp,
          if (identical(as.numeric(n_grp), 1)) " fila en grupos duplicados (" else
            " filas en grupos duplicados (",
          n_dup,
          if (identical(as.numeric(n_dup), 1)) " excedente)" else " excedentes)"
        ),
        "Definir una clave y revisar la causa antes de eliminar duplicados.",
        n_filas, n_grp, "fila"
      )
      faltan <- setdiff(names(perfil$hallazgos), names(nuevo))
      for (f in faltan) nuevo[[f]] <- NA
      nuevo <- .seleccionar_columnas(nuevo, names(perfil$hallazgos))
      perfil$hallazgos <- rbind(perfil$hallazgos, nuevo)
      rownames(perfil$hallazgos) <- NULL
    }
  }
  perfil
}
