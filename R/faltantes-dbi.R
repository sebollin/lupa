.faltantes_dbi_tipo_binario <- function(tipo) {
  length(tipo) == 1L && !is.na(tipo) && grepl(
    "binary|blob|bytea|raw", .minusculas_ascii(as.character(tipo)),
    useBytes = TRUE
  )
}

.faltantes_dbi_tipo_texto <- function(prototipo, tipo) {
  if (.faltantes_dbi_tipo_binario(tipo)) return(FALSE)
  if (inherits(prototipo, c("character", "factor"))) return(TRUE)
  if (length(tipo) != 1L || is.na(tipo)) return(FALSE)
  base <- .minusculas_ascii(as.character(tipo))
  grepl("char|text|clob|string|varchar", base, useBytes = TRUE) &&
    !grepl("binary|blob|date|time", base, useBytes = TRUE)
}

.faltantes_dbi_literal_numerico <- function(
    valor, senas = "", conexion = NULL, exacto = FALSE) {
  if (is.infinite(valor)) {
    positivo <- valor > 0
    if (isTRUE(exacto) && grepl(
      "postgres|pqconnection", senas, useBytes = TRUE
    )) {
      return(paste0(
        "CAST('", if (positivo) "1e309" else "-1e309",
        "' AS NUMERIC)"
      ))
    }
    if (grepl("postgres|pqconnection", senas, useBytes = TRUE)) {
      return(paste0(
        "CAST('", if (positivo) "Infinity" else "-Infinity",
        "' AS DOUBLE PRECISION)"
      ))
    }
    if (grepl("duckdb", senas, useBytes = TRUE)) {
      return(paste0(
        "CAST('", if (positivo) "Infinity" else "-Infinity", "' AS DOUBLE)"
      ))
    }
    if (grepl("sqlite", senas, useBytes = TRUE)) {
      return(if (positivo) "9e999" else "-9e999")
    }
  }
  if (isTRUE(exacto) && is.finite(valor) && valor != trunc(valor)) {
    # La lectura exacta de PostgreSQL compara `numeric` contra `numeric`: el
    # literal tiene que ser el desarrollo decimal exacto del double (`0.1` es
    # `0.1000000000000000055511...`). Contra una columna double el mismo
    # literal vuelve al mismo double.
    cifras <- min(1074L, max(0L, 52L - as.integer(floor(log2(abs(valor))))))
    if (cifras > 1000L) {
      return(format(valor, scientific = TRUE, trim = TRUE, digits = 17))
    }
    exacto <- sprintf(paste0("%.", cifras, "f"), valor)
    return(sub("[.]$", "", sub("0+$", "", exacto)))
  }
  # Un literal fijo con cientos de ceros o cifras enteras se redondea o se
  # interpreta como DECIMAL fuera del rango del doble en algunos motores. La
  # notacion cientifica conserva la cifra que compara el double.
  if (is.finite(valor) && valor != 0 &&
      (abs(valor) < 1e-6 || abs(valor) >= 1e15)) {
    return(format(valor, scientific = TRUE, trim = TRUE, digits = 17))
  }
  format(valor, scientific = FALSE, trim = TRUE, digits = 17)
}

.suma_condicion_faltantes_dbi <- function(condicion, senas) {
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas,
            useBytes = TRUE)) {
    return(paste0("SUM(IIF(", condicion, ", 1, 0))"))
  }
  # `SUM((condicion)::integer)` de PostgreSQL da NULL si la condicion es NULL
  # en todas las filas (texto sin ningun numero, columna toda NULL) y la cuenta
  # quedaba sin publicar. El CASE suma cero en esas filas.
  paste0("SUM(CASE WHEN ", condicion, " THEN 1 ELSE 0 END)")
}

.normalizador_texto_faltantes_dbi <- function(conexion, columna) {
  senas <- .senas_conexion_dbi(conexion)
  if (grepl("duckdb|postgres|pqconnection", senas, useBytes = TRUE)) {
    caracter <- "CHR"
  } else if (grepl(
    "sqlite|sql server|microsoft sql|sqlserver|mssql|mysql|mariadb",
    senas, useBytes = TRUE
  )) {
    caracter <- "CHAR"
  } else {
    return(NULL)
  }
  reemplazada <- paste0(
    "REPLACE(REPLACE(REPLACE(", columna, ", ", caracter,
    "(9), ' '), ", caracter, "(10), ' '), ", caracter, "(13), ' ')"
  )
  recortada <- if (grepl("sql server|microsoft sql|sqlserver|mssql",
                          senas, useBytes = TRUE)) {
    paste0("LTRIM(RTRIM(", reemplazada, "))")
  } else if (grepl("mysql|mariadb", senas, useBytes = TRUE)) {
    paste0("TRIM(BOTH ' ' FROM ", reemplazada, ")")
  } else {
    # El argumento explicito evita que DuckDB quite espacios no separables,
    # que `trimws()` de R conserva.
    paste0("TRIM(", reemplazada, ", ' ')")
  }
  paste0("LOWER(", recortada, ")")
}

.tipo_numerico_texto_faltantes_dbi <- function(senas) {
  # PostgreSQL llama `DOUBLE PRECISION` a este tipo; los demas dialectos
  # cubiertos por la expresion aceptan la forma que ya usa el paquete.
  if (grepl("postgres|pqconnection", senas, useBytes = TRUE)) {
    return("DOUBLE PRECISION")
  }
  .tipo_flotante_escala_desvio_dbi(senas)
}

.PATRON_NUMERICO_TEXTO_FALTANTES_DBI <-
  "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"

.expresion_numerica_texto_faltantes_dbi <- function(conexion, texto) {
  senas <- .senas_conexion_dbi(conexion)
  tipo <- .tipo_numerico_texto_faltantes_dbi(senas)
  reemplazada <- paste0("REPLACE(", texto, ", ',', '.')")
  expresion <- paste0(
    "CASE WHEN ", texto, " LIKE '%e%' AND ", texto,
    " LIKE '%,%' THEN ", texto,
    " WHEN ", texto, " LIKE '%,' THEN ", texto,
    " ELSE ", reemplazada, " END"
  )
  patron <- as.character(DBI::dbQuoteString(
    conexion, .PATRON_NUMERICO_TEXTO_FALTANTES_DBI
  ))
  if (grepl("duckdb", senas, useBytes = TRUE)) {
    return(paste0(
      "CASE WHEN regexp_matches(", expresion, ", ", patron,
      ") THEN TRY_CAST(", expresion, " AS ", tipo, ") END"
    ))
  }
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas,
            useBytes = TRUE)) {
    return(paste0("TRY_CONVERT(", tipo, ", ", expresion, ")"))
  }
  if (grepl("postgres|pqconnection", senas, useBytes = TRUE)) {
    return(paste0(
      "CASE WHEN ", expresion, " ~ ", patron,
      " THEN CAST(", expresion, " AS ", tipo, ") END"
    ))
  }
  if (.es_mysql_dbi(senas)) {
    return(paste0(
      "CASE WHEN ", expresion, " REGEXP ", patron,
      " THEN CAST(", expresion, " AS ", tipo, ") END"
    ))
  }
  if (grepl("sqlite", senas, useBytes = TRUE)) {
    # SQLite no trae regexp. Se valida la mantisa y, si existe, el exponente
    # por separado para evitar que CAST convierta formas invalidas en cero.
    # La expresion llega normalizada y en minusculas: es un alias de la
    # normalizacion textual o la misma expresion que la contiene.
    posicion_exponente <- paste0("INSTR(", expresion, ", 'e')")
    mantisa <- paste0(
      "CASE WHEN ", posicion_exponente, " > 0 THEN SUBSTR(", expresion,
      ", 1, ", posicion_exponente, " - 1) ELSE ", expresion, " END"
    )
    exponente <- paste0(
      "CASE WHEN ", posicion_exponente, " > 0 THEN SUBSTR(", expresion,
      ", ", posicion_exponente, " + 1) ELSE '' END"
    )
    guarda <- paste0(
      "(", expresion, " NOT GLOB '*[^0-9eE+.-]*' AND ",
      expresion, " GLOB '*[0-9]*' AND ",
      mantisa, " NOT GLOB '*.*.*' AND ",
      mantisa, " GLOB '*[0-9]*' AND ",
      "(", posicion_exponente, " = 0 OR (",
      exponente, " NOT GLOB '*[^0-9+-]*' AND ",
      exponente, " NOT GLOB '*.*' AND ",
      exponente, " GLOB '*[0-9]*' AND ",
      exponente, " NOT GLOB '*[0-9][+-]*' AND ",
      exponente, " NOT GLOB '*[+-]*[+-]*')) AND ",
      "(", mantisa, " NOT GLOB '*[0-9.][+-]*' AND ",
      mantisa, " NOT GLOB '*[+-]*[+-]*')",
      ")"
    )
    return(paste0(
      "CASE WHEN ", guarda,
      " THEN CAST(", expresion, " AS ", tipo, ") END"
    ))
  }
  paste0("CAST(", expresion, " AS ", tipo, ")")
}

.condicion_ambigua_numerica_texto_dbi <- function(conexion, texto) {
  senas <- .senas_conexion_dbi(conexion)
  patron_coma <- as.character(DBI::dbQuoteString(
    conexion, "[0-9],[0-9]{3}([^0-9]|$)"
  ))
  patron_miles <- as.character(DBI::dbQuoteString(
    conexion, "[.][0-9]{3}"
  ))
  if (grepl("duckdb", senas, useBytes = TRUE)) {
    return(paste0(
      "(regexp_matches(", texto, ", ", patron_coma,
      ") AND NOT regexp_matches(", texto, ", '[eE]') OR ",
      "regexp_matches(", texto, ", ", patron_miles, "))"
    ))
  }
  if (grepl("postgres|pqconnection", senas, useBytes = TRUE)) {
    return(paste0(
      "(", texto, " ~ ", patron_coma, " AND ",
      texto, " !~ '[eE]' OR ", texto, " ~ ", patron_miles, ")"
    ))
  }
  if (.es_mysql_dbi(senas)) {
    return(paste0(
      "((", texto, " REGEXP ", patron_coma, " AND ",
      texto, " NOT REGEXP '[eE]') OR ", texto, " REGEXP ",
      patron_miles, ")"
    ))
  }
  if (grepl("sqlite", senas, useBytes = TRUE)) {
    return(paste0(
      "((", texto, " GLOB '*[0-9],[0-9][0-9][0-9]*' AND NOT ",
      texto, " GLOB '*[eE]*') OR ",
      texto, " GLOB '*.[0-9][0-9][0-9]*')"
    ))
  }
  if (grepl("sql server|microsoft sql|sqlserver|mssql", senas,
            useBytes = TRUE)) {
    return(paste0(
      "((", texto, " LIKE '%[0-9],[0-9][0-9][0-9]%' AND NOT ",
      texto, " LIKE '%e%' AND NOT ", texto, " LIKE '%E%') OR ",
      texto, " LIKE '%.[0-9][0-9][0-9]%')"
    ))
  }
  "FALSE"
}

.condicion_no_ascii_inexacta_texto_dbi <- function(
    conexion, texto, texto_original = texto) {
  senas <- .senas_conexion_dbi(conexion)
  if (!grepl("sqlite", senas, useBytes = TRUE)) return("FALSE")
  sin_lower <- sub("^LOWER\\((.*)\\)$", "\\1", texto_original)
  # SQLite deja intactas las mayusculas acentuadas del latin-1, mientras que el
  # plegado de `perfilar()` las convierte en sus formas minusculas. Solo se
  # marcan esos puntos: los acentos ya minusculos no alteran la cardinalidad y
  # no deben retirar una cuenta textual reproducible.
  mayusculas_acentuadas <- c(
    192:197, 199L, 200:207, 209L, 210:214, 216L, 217:220
  )
  no_plegada <- paste0(
    "INSTR(", sin_lower, ", CHAR(", mayusculas_acentuadas, ")) > 0",
    collapse = " OR "
  )
  paste0(
    "(", no_plegada, ")"
  )
}

.faltantes_dbi_motor_texto <- function(senas) {
  grepl("sqlite|duckdb|postgres|pqconnection|sql server|microsoft sql|mysql|mariadb",
        senas, useBytes = TRUE)
}

.politicas_faltantes_dbi <- function(
    conexion, campos, prototipo, tipos_declarados, tipos_motor,
    es_numerico, sentinelas_numericos, cadenas_ausencia,
    columnas_opcionales = character(), aplicabilidad = NULL,
    para_exclusion = FALSE) {
  senas <- .senas_conexion_dbi(conexion)
  # `para_exclusion`: la politica que saca los centinelas declarados de las
  # magnitudes del resumen (ver `.condicion_centinelas_declarados_dbi()`). Es la
  # misma regla que la cuenta, con dos diferencias: el universo declarado
  # -opcionales, aplicabilidad- no la declina, porque la exclusion vale dentro
  # de el; y un entero de 64 bits con centinelas declarados se compara exacto,
  # entero contra entero, en vez de quedar estimado.
  # La forma por columna se selecciona dentro del bucle: cada campo con la suya.
  declaracion_sentinelas <- sentinelas_numericos
  declaracion_cadenas <- cadenas_ausencia
  politica_de <- function(campo) {
    s <- .sentinelas_de_columna(declaracion_sentinelas, campo)
    c_ <- .cadenas_de_columna(declaracion_cadenas, campo)
    list(
      sentinelas = .numeros_na(s),
      declarados = .sentinelas_numericos_declarados(s),
      cadenas = c_,
      declarados_texto = .minusculas_para_comparar(c_)
    )
  }
  if (!.es_ausencias_por_columna(declaracion_sentinelas) &&
      !.es_ausencias_por_columna(declaracion_cadenas)) {
    fija <- politica_de(NA_character_)
    politica_de <- function(campo) fija
  }
  aplicar <- if (is.list(aplicabilidad)) names(aplicabilidad) else character()
  nombres <- stats::setNames(seq_along(campos), campos)
  salida <- stats::setNames(vector("list", length(campos)), campos)
  for (campo in campos) {
    politica <- politica_de(campo)
    sentinelas <- politica$sentinelas
    declarados <- politica$declarados
    cadenas_ausencia <- politica$cadenas
    declarados_texto <- politica$declarados_texto
    posicion <- nombres[[campo]]
    prototipo_campo <- .atributo_columna_dbi(prototipo, campo, posicion)
    tipo_declarado <- .atributo_columna_dbi(
      tipos_declarados, campo, posicion
    )
    tipo_motor <- .atributo_columna_dbi(tipos_motor, campo, posicion)
    tipo <- if (length(tipo_declarado) == 1L && !is.na(tipo_declarado)) {
      tipo_declarado
    } else if (length(tipo_motor) == 1L && !is.na(tipo_motor)) {
      tipo_motor
    } else {
      NA_character_
    }
    if (!isTRUE(para_exclusion) &&
        (campo %in% columnas_opcionales || campo %in% aplicar)) {
      salida[[campo]] <- list(
        exacta = FALSE, parcial = FALSE, clase = NA_character_,
        candidatos = numeric(), textuales = character(), numericos = numeric(),
        motivo = "La aplicabilidad de la columna se decide sobre la muestra."
      )
      next
    }
    # Un controlador puede traer una columna binaria como texto (RMySQL declara
    # `VAR_STRING` para un `VARBINARY`): el tipo del motor manda, porque las
    # funciones de texto del motor no aceptan esa columna.
    es_texto <- .faltantes_dbi_tipo_texto(prototipo_campo, tipo) &&
      !.faltantes_dbi_tipo_binario(tipo_motor)
    es_fecha <- inherits(prototipo_campo, c("Date", "POSIXt")) ||
      (length(tipo) == 1L && !is.na(tipo) && grepl(
        "date|time|timestamp", .minusculas_ascii(as.character(tipo)),
        useBytes = TRUE
      ))
    integer64 <- inherits(prototipo_campo, "integer64") ||
      (length(tipo) == 1L && !is.na(tipo) && grepl(
        "bigint|int64", .minusculas_ascii(as.character(tipo)),
        useBytes = TRUE
      ) && !grepl("postgres|pqconnection", senas, useBytes = TRUE))
    if (es_texto) {
      normalizador <- .normalizador_texto_faltantes_dbi(
        conexion, "{columna}"
      )
      todos <- .cadenas_ausencia_columna(character(), cadenas_ausencia)
      ascii <- !grepl("[^\\x00-\\x7F]", todos, perl = TRUE, useBytes = TRUE)
      candidatos <- todos
      soportado <- .faltantes_dbi_motor_texto(senas) && !is.null(normalizador)
      # SQLite solo convierte a minusculas ASCII; una cadena no ASCII no puede
      # afirmarse igual a la comparacion de `perfilar()`.
      no_ascii_compatible <- grepl(
        "duckdb|postgres|pqconnection", senas, useBytes = TRUE
      )
      sqlite_no_ascii <- grepl("sqlite", senas, useBytes = TRUE) &&
        any(!ascii)
      salida[[campo]] <- list(
        exacta = soportado && length(candidatos) > 0L,
        parcial = any(!ascii) && !no_ascii_compatible && !sqlite_no_ascii,
        no_ascii = sqlite_no_ascii,
        clase = "textual", candidatos = c(candidatos, sentinelas),
        textuales = candidatos, numericos = sentinelas,
        declarados_texto = declarados_texto,
        normalizador = normalizador,
        motivo = if (!soportado) {
          paste(
            "La normalizacion textual de `perfilar()` no se puede reproducir",
            "con exactitud en este motor."
          )
        } else if (any(!ascii) && !no_ascii_compatible && !sqlite_no_ascii) {
          paste(
            "Las cadenas no ASCII quedan estimadas en la muestra; SQL cuenta",
            "solo el subconjunto ASCII reproducible."
          )
        } else {
          NA_character_
        }
      )
      next
    }
    es_numero <- isTRUE(es_numerico[[posicion]]) || is.numeric(prototipo_campo)
    # Un centinela desde 2^53 no se puede comparar pasando la columna a doble:
    # `9007199254740993` redondea a `9007199254740992` y contaba como centinela
    # (medido en SQLite: 4 celdas donde `perfilar()` cuenta 1). Ahi la
    # comparacion es entre enteros, sin conversion. SQLite compara un entero con
    # un real sin perder cifras, asi que vale para toda columna numerica suya.
    # El entero de 64 bits se reconoce tambien por su tipo: MySQL y MariaDB lo
    # leen como texto en esta via y su prototipo no lo dice (medido: con `2^53`
    # declarado salia tambien su vecino).
    de_64_bits <- function(x) {
      length(x) == 1L && !is.na(x) && grepl(
        "bigint|int64|integer64", .minusculas_ascii(as.character(x)),
        useBytes = TRUE
      )
    }
    entera <- length(sentinelas) && any(abs(sentinelas) >= 2^53) && (
      grepl("sqlite", senas, useBytes = TRUE) ||
        inherits(prototipo_campo, "integer64") || isTRUE(integer64) ||
        (.es_mysql_dbi(senas) && (de_64_bits(tipo) || de_64_bits(tipo_motor)))
    )
    declina_integer64 <- integer64 &&
      !(isTRUE(para_exclusion) && length(declarados) > 0L) &&
      !(isTRUE(entera) && grepl("sqlite", senas, useBytes = TRUE))
    # Un tipo de 4 bytes con centinelas declarados se compara con el `float32`
    # del centinela: la celda guarda ese numero (`-9.9` es
    # `-9.8999996185302734`), y contra el doble declarado no coincide. Es la
    # regla con que la muestra alinea esas celdas
    # (`.ajustar_sentinelas_real_dbi()`). Vale donde el motor entrega la celda
    # ensanchada a doble: DuckDB, MySQL, MariaDB y SQL Server. PostgreSQL lee su
    # `real` por la forma decimal corta y compara por ahi.
    real <- !isTRUE(entera) && length(declarados) > 0L && (
      grepl("duckdb", senas, fixed = TRUE, useBytes = TRUE) ||
        .es_mysql_dbi(senas) || .es_transact_sql_dbi(senas)
    ) && (.tipo_real_dbi(tipo_motor, senas) || .tipo_real_dbi(tipo, senas))
    if (!es_numero || es_fecha || declina_integer64 || !length(sentinelas)) {
      motivo <- if (declina_integer64) {
        paste(
          "La columna integer64 queda estimada: la conversion a doble no",
          "garantiza la misma comparacion que en R."
        )
      } else if (es_fecha) {
        "Date y POSIXt no cuentan centinelas numericos en R."
      } else if (!es_numero && !es_texto) {
        "El tipo de la columna no estuvo disponible para decidir una politica exacta."
      } else {
        "No hay centinelas numericos aplicables a esta columna."
      }
      salida[[campo]] <- list(
        exacta = FALSE, parcial = FALSE,
        clase = if (es_texto) "textual" else "numerica",
        candidatos = numeric(), textuales = character(), numericos = numeric(),
        motivo = motivo
      )
      next
    }
    salida[[campo]] <- list(
      exacta = TRUE, parcial = FALSE, clase = "numerica",
      candidatos = sentinelas, textuales = character(), numericos = sentinelas,
      declarados = length(declarados) > 0L,
      tipo = tipo,
      comparacion = if (isTRUE(entera)) "entera" else if (isTRUE(real)) {
        "real"
      } else "doble",
      literales = vapply(sentinelas, .faltantes_dbi_literal_numerico,
                         character(1L), senas = senas, USE.NAMES = FALSE),
      motivo = NA_character_, normalizador = NULL
    )
  }
  salida
}

.aliases_faltantes_dbi <- function(policy, alias, numero, posicion,
                                   conexion = NULL, incluir_guardas = TRUE,
                                   incluir_distintos = TRUE) {
  if (!is.list(policy) || !isTRUE(policy$exacta)) return(NULL)
  salida <- list(
    total = .alias_agregado_dbi(alias, numero, posicion, "fd_total"),
    validos = .alias_agregado_dbi(alias, numero, posicion, "fd_validos")
  )
  if (isTRUE(incluir_distintos)) {
    salida$distintos <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_distintos"
    )
  }
  if (identical(policy$clase, "numerica") && isTRUE(incluir_guardas)) {
    salida$minimo <- .alias_agregado_dbi(alias, numero, posicion, "fd_min")
    salida$maximo <- .alias_agregado_dbi(alias, numero, posicion, "fd_max")
    salida$minimo_sin <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_min_sin"
    )
    salida$maximo_sin <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_max_sin"
    )
    salida$frecuencia_moda <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_frecuencia_moda"
    )
    salida$frecuencia_segunda <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_frecuencia_segunda"
    )
    salida$n_frecuencias_moda <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_n_frecuencias_moda"
    )
  }
  if (identical(policy$clase, "textual") && length(policy$numericos)) {
    salida$ambiguos <- .alias_agregado_dbi(
      alias, numero, posicion, "fd_ambiguos"
    )
    if (isTRUE(policy$no_ascii)) {
      salida$no_ascii <- .alias_agregado_dbi(
        alias, numero, posicion, "fd_no_ascii"
      )
    }
  }
  indices_candidatos <- if (identical(policy$clase, "textual")) {
    # El total se cuenta como una union. Solo hacen falta las abreviaturas que
    # pueden abrir la guarda de vocabulario; el resto no participa en una
    # decision posterior.
    which(policy$candidatos %in% .cadenas_na_ambiguas &
      !(policy$candidatos %in% policy$declarados_texto))
  } else if (isTRUE(policy$declarados)) {
    integer()
  } else {
    seq_along(policy$candidatos)
  }
  candidatos <- lapply(seq_along(policy$candidatos), function(i) {
    .alias_agregado_dbi(alias, numero, posicion, paste0("fd_c", i))
  })
  names(candidatos) <- as.character(seq_along(candidatos))
  salida$candidatos <- candidatos
  salida$indices_candidatos <- indices_candidatos
  salida
}

.resultado_faltantes_lote_dbi <- function(consulta, sql, aliases, metadatos) {
  leer <- function(alias) {
    .resultado_lote_dbi(consulta, sql, alias, metadatos)
  }
  indices_candidatos <- aliases$indices_candidatos %||%
    seq_along(aliases$candidatos)
  candidatos <- lapply(
    aliases$candidatos[as.character(indices_candidatos)], leer
  )
  names(candidatos) <- as.character(indices_candidatos)
  salida <- list(
    total = leer(aliases$total), candidatos = candidatos,
    validos = leer(aliases$validos),
    sql = sql, metadatos = metadatos
  )
  if (!is.null(aliases$distintos)) {
    salida$distintos <- leer(aliases$distintos)
  }
  if (!is.null(aliases$ambiguos)) salida$ambiguos <- leer(aliases$ambiguos)
  if (!is.null(aliases$no_ascii)) salida$no_ascii <- leer(aliases$no_ascii)
  if (!is.null(aliases$minimo)) {
    for (nombre in c(
      "minimo", "maximo", "minimo_sin", "maximo_sin",
      "frecuencia_moda", "frecuencia_segunda", "n_frecuencias_moda"
    )) {
      salida[[nombre]] <- leer(aliases[[nombre]])
    }
  }
  requeridos <- c(list(salida$total), salida$candidatos,
                  list(salida$validos))
  if (!is.null(salida$distintos)) {
    requeridos <- c(requeridos, list(salida$distintos))
  }
  if (!is.null(aliases$ambiguos)) requeridos <- c(
    requeridos, list(salida$ambiguos)
  )
  if (!is.null(aliases$no_ascii)) requeridos <- c(
    requeridos, list(salida$no_ascii)
  )
  if (!is.null(aliases$minimo)) {
    requeridos <- c(
      requeridos,
      salida[c(
        "minimo", "maximo", "minimo_sin", "maximo_sin",
        "frecuencia_moda", "frecuencia_segunda", "n_frecuencias_moda"
      )]
    )
  }
  # Una fila puede llegar con los primeros alias y fallar al leer los demas.
  # No alcanza con que el total y los candidatos parezcan validos: todos los
  # campos que pidio la consulta deben estar presentes para publicar la cuenta.
  salida$ok <- all(vapply(
    requeridos, function(x) isTRUE(x$ok), logical(1L)
  ))
  salida
}

.resultado_faltantes_desde_resultado_dbi <- function(resultado, aliases,
                                                    metadatos = list()) {
  if (is.null(aliases)) return(NULL)
  consulta <- list(
    ok = isTRUE(resultado$ok),
    datos = if (isTRUE(resultado$ok)) resultado$datos else NULL,
    motivo = resultado$motivo %||% "La consulta de agregados no se emitio.",
    consulta_id = resultado$consulta_id %||% NA_integer_
  )
  .resultado_faltantes_lote_dbi(
    consulta, resultado$sql %||% NA_character_, aliases, metadatos
  )
}

.expresiones_faltantes_dbi <- function(
    conexion, policy, columna, aliases, tabla_fuente,
    solo_guardas = FALSE, columna_original = columna,
    columna_normalizada = NULL, columna_numerica = NULL,
    comparar_double_original = FALSE, peso = NULL,
    solo_condicion = FALSE) {
  # `solo_condicion`: devuelve la condicion "la celda es un centinela numerico"
  # y nada mas. Es la que cuenta y la que excluye: una regla, dos usos.
  if ((is.null(aliases) && !isTRUE(solo_condicion)) ||
      !isTRUE(policy$exacta)) return(character())
  senas <- .senas_conexion_dbi(conexion)
  es_texto <- identical(policy$clase, "textual")
  normalizador <- if (es_texto) {
    if (is.null(columna_normalizada)) {
      gsub("\\{columna\\}", columna, policy$normalizador)
    } else columna_normalizada
  } else columna
  normalizador_original <- if (es_texto && !is.null(columna_normalizada)) {
    gsub("\\{columna\\}", columna_original, policy$normalizador)
  } else normalizador
  numerico_texto <- if (es_texto) {
    columna_numerica %||%
      .expresion_numerica_texto_faltantes_dbi(conexion, normalizador)
  } else {
    tipo <- policy$tipo %||% NA_character_
    postgres <- grepl("postgres|pqconnection", senas, useBytes = TRUE)
    exacta <- postgres && (
      isTRUE(comparar_double_original) ||
        .es_lectura_double_exacta_dbi(columna)
    )
    doble <- .tipo_declarado_float8_dbi(tipo, conexion)
    if (exacta || isTRUE(doble) || identical(policy$comparacion, "entera")) {
      if (isTRUE(comparar_double_original)) columna_original else columna
    } else {
      paste0("CAST(", columna, " AS ",
             .tipo_numerico_texto_faltantes_dbi(senas), ")")
    }
  }
  literal_de <- function(valor) {
    if (identical(policy$comparacion, "entera")) {
      # Entero contra entero: el literal son las cifras exactas del centinela.
      # Uno con fraccion, o fuera del entero de 64 bits, no coincide con ningun
      # entero; SQLite ademas puede guardar reales en la misma columna y ahi
      # el literal de siempre compara exacto.
      entero <- is.finite(valor) && valor == trunc(valor) && abs(valor) < 2^63
      literal <- if (entero) {
        sprintf("%.0f", valor)
      } else if (grepl("sqlite", senas, useBytes = TRUE)) {
        .faltantes_dbi_literal_numerico(
          valor, senas = senas, conexion = conexion
        )
      } else NA_character_
      return(list(
        valor = valor, literal = literal, exacto = FALSE, doble = FALSE
      ))
    }
    if (identical(policy$comparacion, "real")) {
      # La columna va ensanchada a doble, como la entrega el motor, y el
      # literal es el `float32` del centinela, tambien ensanchado: los dos son
      # dobles exactos y la igualdad no depende de como redondea el motor al
      # convertir. Un centinela fuera del rango de 4 bytes no coincide con
      # ninguna celda.
      guardado <- if (is.finite(valor)) .real_dbi_a_doble(valor) else NA_real_
      literal <- if (length(guardado) == 1L && is.finite(guardado)) {
        .faltantes_dbi_literal_numerico(
          guardado, senas = senas, conexion = conexion
        )
      } else NA_character_
      return(list(
        valor = valor, literal = literal, exacto = FALSE, doble = FALSE
      ))
    }
    postgres <- grepl("postgres|pqconnection", senas, useBytes = TRUE)
    # En la consulta consolidada la lectura exacta llega como una columna de
    # una tabla derivada (`"x"`, sin `float8send()` a la vista), asi que no se
    # puede decidir por la expresion. El desarrollo decimal exacto de un double
    # finito sirve contra las dos lecturas: contra `numeric` compara exacto y
    # contra `double precision` vuelve al mismo double.
    finito <- postgres && is.finite(valor)
    cifras <- if (finito && valor != 0 && valor != trunc(valor)) {
      min(1074L, max(0L, 52L - as.integer(floor(log2(abs(valor))))))
    } else 0L
    infinito_exacto <- postgres && is.infinite(valor) &&
      (isTRUE(comparar_double_original) ||
       .es_lectura_double_exacta_dbi(columna))
    exacto <- (finito && cifras <= 1000L) || infinito_exacto
    if (isTRUE(comparar_double_original)) exacto <- FALSE
    literal <- .faltantes_dbi_literal_numerico(
      valor, senas = senas, conexion = conexion, exacto = exacto
    )
    list(
      valor = valor, literal = literal, exacto = exacto,
      doble = finito && cifras > 1000L
    )
  }
  literales_info <- lapply(policy$numericos, literal_de)
  literales <- vapply(literales_info, `[[`, character(1L), "literal")
  if (length(literales_info) && any(vapply(
    literales_info, `[[`, logical(1L), "doble"
  ))) {
    # La representacion numerica exacta de PostgreSQL puede superar las mil
    # cifras decimales. El doble original es la comparacion de memoria; se
    # acota antes del CAST para que los infinitos numericos de la lectura exacta
    # no intenten convertirse a DOUBLE.
    columna_doble <- paste0(
      "CASE WHEN ", columna, " BETWEEN -1e308 AND 1e308 THEN CAST(",
      columna, " AS ", .tipo_numerico_texto_faltantes_dbi(senas), ") END"
    )
  } else {
    columna_doble <- numerico_texto
  }
  condiciones_textuales <- if (es_texto && length(policy$textuales)) {
    paste0(
      normalizador, " IN (",
      paste(vapply(
        policy$textuales,
        function(valor) as.character(DBI::dbQuoteString(conexion, valor)),
        character(1L), USE.NAMES = FALSE
      ), collapse = ", "), ")"
    )
  } else character()
  # Un literal `NA` es un centinela que no puede coincidir con el tipo de la
  # columna: no entra en la lista.
  condiciones_numericas <- if (length(literales[!is.na(literales)])) {
    paste0(
      if (any(vapply(literales_info, `[[`, logical(1L), "doble"))) {
        columna_doble
      } else numerico_texto,
      " IN (", paste(literales[!is.na(literales)], collapse = ", "), ")"
    )
  } else character()
  if (isTRUE(solo_condicion)) return(condiciones_numericas)
  condiciones <- c(condiciones_textuales, condiciones_numericas)
  suma <- function(condicion) {
    if (is.null(peso)) {
      return(.suma_condicion_faltantes_dbi(condicion, senas))
    }
    if (grepl("sql server|microsoft sql|sqlserver|mssql", senas,
              useBytes = TRUE)) {
      return(paste0("SUM(IIF(", condicion, ", ", peso, ", 0))"))
    }
    paste0(
      "SUM(CASE WHEN ", condicion, " THEN ", peso, " ELSE 0 END)"
    )
  }
  if (isTRUE(solo_guardas)) {
    if (!identical(policy$clase, "numerica")) return(character())
    valor_columna <- if (isTRUE(comparar_double_original)) {
      columna_original
    } else columna
    sin_candidatos <- if (length(literales[!is.na(literales)])) paste0(
      valor_columna, " NOT IN (",
      paste(literales[!is.na(literales)], collapse = ", "), ")"
    ) else "TRUE"
    grupos <- paste0(
      "(SELECT ", valor_columna, " AS lupa_fd_valor, COUNT(*) AS lupa_fd_n",
      " FROM ", tabla_fuente, " WHERE ", columna,
      " IS NOT NULL GROUP BY ", valor_columna, ") lupa_fd_grupos"
    )
    maximo_frecuencia <- paste0(
      "(SELECT MAX(lupa_fd_n) FROM ", grupos, ")"
    )
    segunda_frecuencia <- paste0(
      "(SELECT MAX(lupa_fd_n) FROM ", grupos,
      " WHERE lupa_fd_n < ", maximo_frecuencia, ")"
    )
    n_maximos <- paste0(
      "(SELECT COUNT(*) FROM ", grupos,
      " WHERE lupa_fd_n = ", maximo_frecuencia, ")"
    )
    guardas <- c(
      minimo = paste0("MIN(", valor_columna, ") AS ", aliases$minimo),
      maximo = paste0("MAX(", valor_columna, ") AS ", aliases$maximo),
      minimo_sin = paste0("MIN(CASE WHEN ", sin_candidatos, " THEN ",
                          valor_columna, " ELSE NULL END) AS ",
                          aliases$minimo_sin),
      maximo_sin = paste0("MAX(CASE WHEN ", sin_candidatos, " THEN ",
                          valor_columna, " ELSE NULL END) AS ",
                          aliases$maximo_sin),
      frecuencia_moda = paste0(
        maximo_frecuencia, " AS ", aliases$frecuencia_moda
      ),
      frecuencia_segunda = paste0(
        segunda_frecuencia, " AS ", aliases$frecuencia_segunda
      ),
      n_frecuencias_moda = paste0(
        n_maximos, " AS ", aliases$n_frecuencias_moda
      )
    )
    pedidos <- attr(aliases, "guardas_pedidas", exact = TRUE)
    if (is.null(pedidos)) pedidos <- names(guardas)
    return(unname(guardas[intersect(pedidos, names(guardas))]))
  }
  cuenta <- function(condicion, alias) paste0(
    suma(condicion), " AS ", alias
  )
  total <- if (length(condiciones)) {
    cuenta(paste0("(", paste(condiciones, collapse = ") OR ("), ")"),
           aliases$total)
  } else {
    paste0("0 AS ", aliases$total)
  }
  indices_candidatos <- aliases$indices_candidatos %||%
    seq_along(policy$candidatos)
  condiciones_candidatos <- if (length(indices_candidatos)) {
    vapply(indices_candidatos, function(i) {
      if (es_texto && i <= length(policy$textuales)) {
        paste0(
          normalizador, " = ", as.character(DBI::dbQuoteString(
            conexion, policy$candidatos[[i]]
          ))
        )
      } else {
        info <- literales_info[[i - length(policy$textuales)]]
        if (is.na(info$literal)) "1 = 0" else paste0(
          if (isTRUE(info$doble)) columna_doble else numerico_texto,
          " = ", info$literal
        )
      }
    }, character(1L), USE.NAMES = FALSE)
  } else character()
  cuentas <- if (length(condiciones_candidatos)) {
    vapply(seq_along(condiciones_candidatos), function(i) {
      indice <- indices_candidatos[[i]]
      cuenta(
        condiciones_candidatos[[i]],
        aliases$candidatos[[as.character(indice)]]
      )
    }, character(1L), USE.NAMES = FALSE)
  } else character()
  expr_distintos <- if (es_texto) normalizador else {
    if (isTRUE(comparar_double_original)) columna_original else columna
  }
  expr_validos <- if (is.null(peso)) {
    .contar_no_nulos_sql_dbi(expr_distintos, senas)
  } else {
    suma(paste0(expr_distintos, " IS NOT NULL"))
  }
  expresiones <- c(total, cuentas)
  if (!is.null(aliases$distintos)) {
    expresiones <- c(
      expresiones,
      paste0(.conteo_sql_dbi(
        paste0("COUNT(DISTINCT ", expr_distintos, ")"), senas
      ), " AS ", aliases$distintos)
    )
  }
  expresiones <- c(expresiones, paste0(expr_validos, " AS ", aliases$validos))
  if (es_texto && !is.null(aliases$ambiguos)) {
    ambiguo <- .condicion_ambigua_numerica_texto_dbi(conexion, normalizador)
    expresiones <- c(
      expresiones,
      paste0(suma(ambiguo),
             " AS ", aliases$ambiguos)
    )
    if (!is.null(aliases$no_ascii)) {
      no_ascii <- .condicion_no_ascii_inexacta_texto_dbi(
        conexion, normalizador, normalizador_original
      )
      expresiones <- c(
        expresiones,
        paste0(suma(no_ascii),
               " AS ", aliases$no_ascii)
      )
    }
  }
  if (identical(policy$clase, "numerica") && !isTRUE(solo_guardas) &&
      !is.null(aliases$minimo)) {
    valor_columna <- if (isTRUE(comparar_double_original)) {
      columna_original
    } else columna
    sin_candidatos <- if (length(literales[!is.na(literales)])) paste0(
      valor_columna, " NOT IN (",
      paste(literales[!is.na(literales)], collapse = ", "), ")"
    ) else "1 = 1"
    grupos <- paste0(
      "(SELECT ", valor_columna, " AS lupa_fd_valor, COUNT(*) AS lupa_fd_n",
      " FROM ", tabla_fuente, " WHERE ", valor_columna,
      " IS NOT NULL GROUP BY ", valor_columna, ") lupa_fd_grupos"
    )
    maximo_frecuencia <- paste0(
      "(SELECT MAX(lupa_fd_n) FROM ", grupos, ")"
    )
    segunda_frecuencia <- paste0(
      "(SELECT MAX(lupa_fd_n) FROM ", grupos,
      " WHERE lupa_fd_n < ", maximo_frecuencia, ")"
    )
    n_maximos <- paste0(
      "(SELECT COUNT(*) FROM ", grupos,
      " WHERE lupa_fd_n = ", maximo_frecuencia, ")"
    )
    expresiones <- c(
      expresiones,
      paste0("MIN(", valor_columna, ") AS ", aliases$minimo),
      paste0("MAX(", valor_columna, ") AS ", aliases$maximo),
      paste0("MIN(CASE WHEN ", sin_candidatos, " THEN ", valor_columna,
             " ELSE NULL END) AS ", aliases$minimo_sin),
      paste0("MAX(CASE WHEN ", sin_candidatos, " THEN ", valor_columna,
             " ELSE NULL END) AS ", aliases$maximo_sin),
      paste0(maximo_frecuencia, " AS ", aliases$frecuencia_moda),
      paste0(segunda_frecuencia, " AS ", aliases$frecuencia_segunda),
      paste0(n_maximos, " AS ", aliases$n_frecuencias_moda)
    )
  }
  expresiones
}

.completar_guardas_faltantes_dbi <- function(
    conexion, tabla_fuente, campos, nombres_sql, politicas, agregados,
    resultados, presupuesto = NULL, alias = identity,
    nombres_sql_originales = nombres_sql,
    columnas_double_originales = character()) {
  if (!is.list(politicas) || !length(politicas) ||
      !is.list(agregados$faltantes)) return(agregados)
  for (campo in intersect(names(politicas), campos)) {
    policy <- politicas[[campo]]
    if (!isTRUE(policy$exacta) || !identical(policy$clase, "numerica") ||
        isTRUE(policy$declarados)) next
    base <- agregados$faltantes[[campo]]
    if (!is.list(base) || !isTRUE(base$ok) || !length(base$candidatos)) next
    cuentas <- vapply(base$candidatos, .numero_faltante_dbi, numeric(1L))
    if (anyNA(cuentas) || !any(cuentas > 0)) next
    indice <- match(campo, campos)
    aliases <- .aliases_faltantes_dbi(
      policy, alias, 9999L, indice, conexion, incluir_guardas = TRUE,
      incluir_distintos = FALSE
    )
    aliases$columna_original <- nombres_sql_originales[[campo]] %||%
      nombres_sql[[campo]]
    aliases$comparar_double_original <- campo %in%
      as.character(columnas_double_originales)
    fila <- if (length(resultados) && is.list(resultados[[campo]])) {
      resultados[[campo]]$fila
    } else NULL
    reutilizables <- c("minimo", "maximo", "frecuencia_moda")
    guardados <- list()
    if (is.list(fila)) {
      for (nombre in reutilizables) {
        valor <- fila[[nombre]]
        if (length(valor) == 1L && !is.na(valor)) {
          guardados[[nombre]] <- list(
            ok = TRUE, valor = valor, motivo = NA_character_,
            sql = NA_character_, metadatos = list()
          )
        }
      }
    }
    pedidos <- setdiff(
      c("minimo", "maximo", "minimo_sin", "maximo_sin",
        "frecuencia_moda", "frecuencia_segunda", "n_frecuencias_moda"),
      names(guardados)
    )
    attr(aliases, "guardas_pedidas") <- pedidos
    expresiones <- .expresiones_faltantes_dbi(
      conexion, policy, nombres_sql[[campo]], aliases, tabla_fuente,
      solo_guardas = TRUE,
      columna_original = aliases$columna_original,
      comparar_double_original = isTRUE(aliases$comparar_double_original)
    )
    if (length(expresiones)) {
      sql <- paste0(
        "SELECT ", paste(expresiones, collapse = ", "),
        " FROM ", tabla_fuente
      )
      usadas_antes <- if (is.null(presupuesto)) 0 else presupuesto$usadas
      consulta <- .consultar_dbi(
        conexion, sql, presupuesto, etapa = "conteos"
      )
      if (!is.null(presupuesto) && presupuesto$usadas > usadas_antes) {
        presupuesto$guardas_faltantes_emitidas <-
          (presupuesto$guardas_faltantes_emitidas %||% 0L) + 1L
      } else if (!isTRUE(consulta$ok) && !is.null(presupuesto) &&
                 identical(as.character(consulta$motivo),
                           .motivo_presupuesto_dbi(presupuesto))) {
        presupuesto$guardas_faltantes_bloqueadas <-
          (presupuesto$guardas_faltantes_bloqueadas %||% 0L) + 1L
      }
      for (nombre in pedidos) {
        guardados[[nombre]] <- .resultado_lote_dbi(
          consulta, sql, aliases[[nombre]], list()
        )
      }
      base$metadatos <- .mezclar_metadatos_dbi(
        base$metadatos, list(sql_guardas = sql)
      )
    }
    for (nombre in names(guardados)) base[[nombre]] <- guardados[[nombre]]
    requeridos <- c(
      "minimo", "maximo", "minimo_sin", "maximo_sin",
      "frecuencia_moda", "frecuencia_segunda", "n_frecuencias_moda"
    )
    base$ok <- all(vapply(
      base[requeridos], function(x) is.list(x) && isTRUE(x$ok), logical(1L)
    ))
    if (!base$ok && is.null(base$motivo)) {
      fallas <- base[requeridos][!vapply(
        base[requeridos], function(x) is.list(x) && isTRUE(x$ok), logical(1L)
      )]
      motivos <- vapply(fallas, function(x) {
        if (is.list(x) && length(x$motivo)) as.character(x$motivo) else NA_character_
      }, character(1L))
      motivos <- motivos[!is.na(motivos) & nzchar(motivos)]
      base$motivo <- if (length(motivos)) motivos[[1L]] else
        "No se pudo obtener la guarda de faltantes disfrazados."
    }
    agregados$faltantes[[campo]] <- base
  }
  agregados
}

.motivo_faltantes_dbi <- function(policy, base = NULL) {
  resultados <- if (is.list(base)) {
    c(
      if (length(base$motivo)) list(list(motivo = base$motivo)) else NULL,
      if (is.list(base$total)) list(base$total) else NULL,
      if (length(base$candidatos)) base$candidatos else NULL,
      if (is.list(base$distintos)) list(base$distintos) else NULL,
      if (is.list(base$validos)) list(base$validos) else NULL,
      if (is.list(base$minimo)) list(base$minimo) else NULL,
      if (is.list(base$maximo)) list(base$maximo) else NULL,
      if (is.list(base$minimo_sin)) list(base$minimo_sin) else NULL,
      if (is.list(base$maximo_sin)) list(base$maximo_sin) else NULL,
      if (is.list(base$frecuencia_moda)) list(base$frecuencia_moda) else NULL,
      if (is.list(base$frecuencia_segunda)) {
        list(base$frecuencia_segunda)
      } else NULL,
      if (is.list(base$n_frecuencias_moda)) {
        list(base$n_frecuencias_moda)
      } else NULL
    )
  } else list()
  motivos <- c(
    if (length(resultados)) vapply(resultados, function(x) {
      if (is.list(x)) x$motivo else NA_character_
    }, character(1L)) else NULL,
    if (is.list(policy)) policy$motivo else NULL
  )
  utilizables <- vapply(motivos, function(x) {
    length(x) == 1L && !is.na(x) && nzchar(as.character(x))
  }, logical(1L))
  if (any(utilizables)) {
    return(as.character(motivos[[which(utilizables)[[1L]]]]))
  }
  "No se pudo obtener la cuenta de faltantes disfrazados en SQL."
}

.numero_faltante_dbi <- function(resultado) {
  if (!is.list(resultado) || !isTRUE(resultado$ok)) return(NA_real_)
  .conteo_dbi(resultado$valor)
}

.faltantes_dbi_mapa <- function(agregados, resultados, politicas, universo) {
  salida <- stats::setNames(vector("list", length(politicas)), names(politicas))
  for (campo in names(politicas)) {
    policy <- politicas[[campo]]
    base <- agregados$faltantes[[campo]]
    conteos <- agregados$conteos[[campo]]
    distintos_perfil <- if (is.list(conteos)) conteos$distintos else NULL
    if (is.list(base) && is.list(distintos_perfil) &&
        (!is.list(base$distintos) || !isTRUE(base$distintos$ok))) {
      base$distintos <- distintos_perfil
    }
    vacio <- list(
      exacta = FALSE, parcial = isTRUE(policy$parcial), n = NA_real_,
      universo = universo, clase = policy$clase,
      motivo = .motivo_faltantes_dbi(policy, base)
    )
    inferir_cero <- function() {
      if (!identical(policy$clase, "numerica") ||
          isTRUE(policy$declarados) || !length(policy$numericos)) {
        return(NULL)
      }
      resultado <- resultados[[campo]]
      basicos <- agregados$basicos[[campo]]
      conteos <- agregados$conteos[[campo]]
      fila <- if (is.list(resultado)) resultado$fila else NULL
      datos <- if (is.list(basicos)) basicos$datos else NULL
      minimo <- if (is.data.frame(datos)) {
        .numero_dbi(.valor_campo_dbi(datos, "minimo")$valor)
      } else NA_real_
      maximo <- if (is.data.frame(datos)) {
        .numero_dbi(.valor_campo_dbi(datos, "maximo")$valor)
      } else NA_real_
      if (!is.finite(minimo) && is.list(fila)) {
        minimo <- suppressWarnings(as.numeric(fila$minimo))
      }
      if (!is.finite(maximo) && is.list(fila)) {
        maximo <- suppressWarnings(as.numeric(fila$maximo))
      }
      validos <- if (is.list(conteos)) {
        .numero_faltante_dbi(conteos$validos)
      } else NA_real_
      if (!is.finite(validos) && is.list(fila)) {
        validos <- suppressWarnings(as.numeric(fila$n_validos))
      }
      probado <- length(validos) == 1L && isTRUE(validos == 0) ||
        length(minimo) == 1L && length(maximo) == 1L &&
        is.finite(minimo) && is.finite(maximo) &&
        all(policy$numericos < minimo | policy$numericos > maximo)
      if (!isTRUE(probado)) return(NULL)
      medicion <- NULL
      medicion <- basicos %||%
        if (is.list(conteos)) conteos$validos else NULL
      if (is.null(medicion) && is.list(resultado)) {
        medicion <- resultado$basicos %||% resultado$validos %||%
          resultado$desvio
      }
      if (!is.list(medicion)) {
        medicion <- list(
          ok = TRUE, sql = NA_character_, consulta_id = NA_integer_,
          n_filas_resultado = 1L, metadatos = list()
        )
      }
      medicion$ok <- TRUE
      medicion$valor <- 0
      medicion$motivo <- NA_character_
      list(
        exacta = TRUE, parcial = isTRUE(policy$parcial), n = 0,
        universo = universo, motivo = NA_character_, inferida = TRUE,
        medicion = medicion, sql = medicion$sql %||% NA_character_
      )
    }
    cero <- inferir_cero()
    if (!is.null(cero)) {
      salida[[campo]] <- cero
      next
    }
    if (!isTRUE(policy$exacta) || is.null(base) || !isTRUE(base$ok)) {
      salida[[campo]] <- vacio
      next
    }
    filas <- .conteo_dbi(agregados$n_total)
    if (is.finite(filas) && filas == 0) {
      vacio$motivo <- paste(
        "La tabla no contiene filas sobre las que decidir la guarda de",
        "faltantes disfrazados."
      )
      salida[[campo]] <- vacio
      next
    }
    indices_candidatos <- names(base$candidatos)
    cuentas <- if (length(indices_candidatos)) {
      vapply(base$candidatos, .numero_faltante_dbi, numeric(1L))
    } else numeric()
    if (any(!is.finite(cuentas))) {
      salida[[campo]] <- vacio
      next
    }
    if (identical(policy$clase, "numerica") &&
        !isTRUE(policy$declarados) && !length(indices_candidatos)) {
      vacio$motivo <- paste(
        "La consulta no devolvio las cuentas por candidato necesarias para",
        "decidir la guarda de la secuencia numerica; no se publica un cero",
        "sin la cuenta completa."
      )
      salida[[campo]] <- vacio
      next
    }
    if (identical(policy$clase, "numerica") &&
        !isTRUE(policy$declarados) && !any(cuentas > 0)) {
      # Ningun candidato aparece en la tabla (que tiene filas: el caso de la
      # tabla vacia sale antes). La cuenta es cero, tambien en una columna toda
      # NULL, como en memoria; la guarda no tiene nada que decidir.
      salida[[campo]] <- list(
        exacta = TRUE, parcial = isTRUE(policy$parcial), n = 0,
        universo = universo, motivo = policy$motivo %||% NA_character_
      )
      next
    }
    if (identical(policy$clase, "textual")) {
      if (!is.finite(.numero_faltante_dbi(base$distintos))) {
        vacio$motivo <- paste(
          "La guarda de abreviaturas ambiguas requiere la cardinalidad",
          "distinta; la estrategia de cardinalidad no produjo una medida",
          "utilizable en esta corrida."
        )
        salida[[campo]] <- vacio
        next
      }
      no_ascii <- .numero_faltante_dbi(base$no_ascii)
      if (is.finite(no_ascii) && no_ascii > 0) {
        salida[[campo]] <- list(
          exacta = TRUE, parcial = TRUE, n = NA_real_,
          universo = universo,
          motivo = paste(
            "SQLite no puede reproducir LOWER() para las cadenas no ASCII",
            "presentes en la columna."
          )
        )
        next
      }
      ambiguos <- .numero_faltante_dbi(base$ambiguos)
      if (is.finite(ambiguos) && ambiguos > 0) {
        salida[[campo]] <- list(
          exacta = TRUE, parcial = TRUE, n = NA_real_,
          universo = universo,
          motivo = paste(
            "La columna contiene texto numerico con coma o agrupacion de",
            "miles; no se puede reproducir su convencion en SQL."
          )
        )
        next
      }
      distintos <- .numero_faltante_dbi(base$distintos)
      if (!is.finite(distintos)) {
        salida[[campo]] <- vacio
        next
      }
      ambiguas <- .cadenas_na_ambiguas
      incluir <- rep(TRUE, length(policy$candidatos))
      posiciones_texto <- seq_along(policy$textuales)
      if (distintos >= .min_distintos_alta_cardinalidad &&
          length(posiciones_texto)) {
        incluir[posiciones_texto] <- !(policy$textuales %in% ambiguas &
          !(policy$textuales %in% policy$declarados_texto))
      }
      total <- .numero_faltante_dbi(base$total)
      if (!is.finite(total)) {
        salida[[campo]] <- vacio
        next
      }
      # The SQL total is the exact union. Only excluded ambiguous candidates
      # need their own counts to subtract from that union.
      if (any(!incluir)) {
        excluidos <- which(!incluir)
        disponibles <- match(as.character(excluidos), indices_candidatos)
        if (anyNA(disponibles) || any(!is.finite(cuentas[disponibles]))) {
          vacio$motivo <- paste(
            "La consulta no devolvio las cuentas necesarias para aplicar la",
            "guarda de las abreviaturas ambiguas."
          )
          salida[[campo]] <- vacio
          next
        }
        total <- total - sum(cuentas[disponibles])
      }
      salida[[campo]] <- list(
        exacta = TRUE, parcial = isTRUE(policy$parcial), n = total,
        universo = universo, motivo = policy$motivo %||% NA_character_
      )
      next
    }
    if (isTRUE(policy$declarados)) {
      salida[[campo]] <- list(
        exacta = TRUE, parcial = isTRUE(policy$parcial),
        n = .numero_faltante_dbi(base$total), universo = universo,
        motivo = policy$motivo %||% NA_character_
      )
      next
    }
    stats <- lapply(
      base[c("distintos", "validos", "minimo", "maximo", "minimo_sin",
             "maximo_sin", "frecuencia_moda", "frecuencia_segunda",
             "n_frecuencias_moda")], .numero_faltante_dbi
    )
    finitas_sin_segunda <- stats[setdiff(names(stats), "frecuencia_segunda")]
    if (any(!is.finite(unlist(finitas_sin_segunda)))) {
      salida[[campo]] <- vacio
      next
    }
    candidatos <- policy$candidatos[as.integer(indices_candidatos)]
    candidatos_presentes <- candidatos[cuentas > 0]
    frecuencias_presentes <- cuentas[cuentas > 0]
    rango <- c(stats$minimo, stats$maximo)
    rango_sin <- c(stats$minimo_sin, stats$maximo_sin)
    n_posiciones <- rango[[2L]] - rango[[1L]] + 1
    densidad <- if (is.finite(n_posiciones) && n_posiciones > 0) {
      stats$distintos / n_posiciones
    } else NA_real_
    densa <- is.finite(densidad) &&
      stats$distintos >= .MIN_DISTINTOS_NUMERACION &&
      densidad >= .MIN_DENSIDAD_NUMERACION &&
      .numeracion_sin_repetir_conteos(
        stats$distintos, stats$validos, stats$frecuencia_moda
      )
    if (isTRUE(densa)) {
      efectivos <- .sentinelas_que_abren_guarda_conteos(
        candidatos_presentes, rango_sin, frecuencias_presentes,
        stats$frecuencia_moda,
        stats$n_frecuencias_moda == 1L &&
          is.finite(stats$frecuencia_segunda) &&
          stats$frecuencia_moda / max(1, stats$frecuencia_segunda) >=
            .FACTOR_VALOR_CONCENTRADO
      )
    } else {
      efectivos <- .sentinelas_sin_apariciones_unicas_conteos(
        candidatos_presentes, frecuencias_presentes, rango_sin,
        policy$candidatos
      )
    }
    indices <- match(efectivos, policy$candidatos)
    indices <- indices[!is.na(indices)]
    cuenta_final <- sum(cuentas[indices])
    salida[[campo]] <- list(
      exacta = TRUE, parcial = isTRUE(policy$parcial), n = cuenta_final,
      universo = universo, motivo = policy$motivo %||% NA_character_
    )
  }
  salida
}

.cobertura_faltantes_dbi <- function(mapa, tabla) {
  salida <- .cobertura_dbi_vacia()
  if (!length(mapa)) return(salida)
  filas <- lapply(names(mapa), function(campo) {
    item <- mapa[[campo]]
    calculada <- isTRUE(item$exacta)
    alcance <- if (identical(item$universo, "muestra_motor")) {
      "la relacion muestreada"
    } else "la tabla completa"
    estado <- if (calculada && !isTRUE(item$parcial)) {
      "calculado"
    } else "degradado"
    motivo <- if (calculada && !isTRUE(item$parcial)) {
      paste0(
        "Los faltantes disfrazados se contaron en ", alcance,
        "; universo = `", item$universo, "`."
      )
    } else {
      item$motivo %||% "No se pudo reproducir la politica en SQL."
    }
    .registro_cobertura_dbi(
      "resumen_tabla", paste0(.texto_tabla_dbi(tabla), ".", campo),
      estado, motivo,
      "Revisar la cobertura y perfilar la muestra si la regla no se puede traducir.",
      NA_character_
    )
  })
  do.call(rbind, filas)
}

# La condicion SQL "la celda es un centinela DECLARADO" de una columna numerica,
# o `NULL` si no hay nada que excluir. Es la misma cadena que cuenta los
# faltantes disfrazados (`.expresiones_faltantes_dbi()`), pedida sola: lo que el
# resumen cuenta como centinela y lo que saca de sus magnitudes no pueden
# divergir. Solo lo declarado: la lista por omision y `sentinelas_naniar` se
# senalan y no se excluyen, igual que en `perfilar()`.
.condicion_centinelas_declarados_dbi <- function(
    conexion, policy, columna, columna_original = columna,
    comparar_double_original = FALSE) {
  if (!is.list(policy) || !isTRUE(policy$exacta) ||
      !identical(policy$clase, "numerica") || !isTRUE(policy$declarados)) {
    return(NULL)
  }
  condicion <- .expresiones_faltantes_dbi(
    conexion, policy, columna, aliases = NULL, tabla_fuente = NULL,
    columna_original = columna_original,
    comparar_double_original = comparar_double_original,
    solo_condicion = TRUE
  )
  if (!length(condicion)) NULL else condicion[[1L]]
}

# La columna como entra a las magnitudes: lo declarado ausente se vuelve `NULL`,
# que es como todos los agregados ya tratan a un faltante. Es la misma idea que
# `.excluir_nan_sql_dbi()`.
.columna_medible_dbi <- function(columna_sql, condicion) {
  if (is.null(condicion) || !length(condicion) || is.na(condicion)) {
    return(columna_sql)
  }
  paste0("CASE WHEN ", condicion, " THEN NULL ELSE ", columna_sql, " END")
}

# La politica de ausencia declarada, resuelta para el resumen SQL de una tabla.
#
# El contrato es el de `perfilar()`, campo por campo: un centinela DECLARADO en
# `sentinelas_numericos` sale de las magnitudes (minimo, maximo, media, mediana,
# desvio, ceros y negativos) y sigue contando en validos, distintos y moda;
# `columnas_opcionales` deja los `NULL` fuera del universo; `aplicabilidad`
# restringe todas las cifras a las filas donde la regla se cumple. Lo que el
# motor no puede aplicar queda en `sin_aplicar`, por columna y con el motivo,
# y la cobertura lo declara.
#
# Devuelve una lista:
# * `activa`: si hay alguna de las tres declaraciones.
# * `columnas_medibles`: por campo, la condicion SQL "es un centinela declarado".
# * `agregados_extra`: por campo, los agregados que hacen falta en el lote de
#   planos para leer el resultado (`n_medibles`).
# * `opcionales`: las columnas declaradas opcionales, con su nombre en la tabla.
# * `sin_aplicar`: politica, columna y motivo de lo que el resumen no aplico.
.politica_ausencia_resumen_dbi <- function(
    conexion, campos, prototipo, tipos_declarados, tipos_motor, es_numerico,
    sentinelas_numericos, cadenas_ausencia, columnas_opcionales, aplicabilidad,
    es_numerico_resumen, sql_de, columna_operacion_de,
    aplicabilidad_sql = NULL) {
  senas <- .senas_conexion_dbi(conexion)
  sin_aplicar <- data.frame(
    politica = character(), columna = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  salida <- list(
    activa = FALSE, declara_sentinelas = FALSE,
    columnas_medibles = list(), agregados_extra = list(),
    opcionales = character(), aplicadas = character(),
    con_universo = FALSE, sin_aplicar = sin_aplicar
  )
  declara_sentinelas <- .hay_sentinelas_declarados(sentinelas_numericos)
  opcionales <- campos[.nombres_presentes(campos, columnas_opcionales)]
  con_regla <- is.list(aplicabilidad) && length(aplicabilidad)
  if (!declara_sentinelas && !length(opcionales) && !con_regla) return(salida)
  salida$activa <- TRUE
  salida$declara_sentinelas <- declara_sentinelas
  salida$opcionales <- opcionales
  salida$con_universo <- length(opcionales) > 0L || con_regla
  if (con_regla) {
    # La regla se aplica donde llego traducida (`aplicabilidad_sql`, ver
    # `.aplicabilidad_sql_resumen_dbi()`): ahi la expresion de la columna ya es
    # `CASE WHEN <regla> THEN col END`, todas sus cifras son de las filas donde
    # la regla se cumple, y el mismo lote cuenta en cuantas se cumple y en
    # cuantas no. Donde no se pudo traducir se declara por columna, con el
    # motivo; los bloques y la muestra la evaluan en R.
    con_nombre <- campos[.nombres_presentes(campos, names(aplicabilidad))]
    for (campo in con_nombre) {
      traducida <- aplicabilidad_sql[[campo]]
      regla <- if (is.list(traducida)) traducida$sql else NULL
      if (length(regla) == 1L && !is.na(regla) && !is.na(sql_de(campo))) {
        salida$aplicadas <- c(salida$aplicadas, campo)
        salida$agregados_extra[[campo]] <- c(
          n_aplicables = .suma_condicion_faltantes_dbi(regla, senas),
          n_no_aplica = .suma_condicion_faltantes_dbi(
            paste0("NOT (", regla, ")"), senas
          )
        )
      } else {
        motivo <- if (is.list(traducida) && length(traducida$motivo) == 1L &&
                      !is.na(traducida$motivo)) {
          as.character(traducida$motivo)
        } else {
          paste(
            "la regla se eval\u00faa en R y este resumen no la traduce al SQL",
            "del motor"
          )
        }
        salida$sin_aplicar[nrow(salida$sin_aplicar) + 1L, ] <- list(
          "aplicabilidad", campo, motivo
        )
      }
    }
  }
  if (declara_sentinelas) {
    politicas <- .politicas_faltantes_dbi(
      conexion, campos, prototipo, tipos_declarados, tipos_motor,
      es_numerico, sentinelas_numericos, cadenas_ausencia,
      para_exclusion = TRUE
    )
    for (i in seq_along(campos)) {
      campo <- campos[[i]]
      # Sin magnitudes en el resumen no hay de donde excluir.
      if (!isTRUE(es_numerico_resumen(campo))) next
      declarados <- tryCatch(
        .sentinelas_numericos_declarados(
          .sentinelas_de_columna(sentinelas_numericos, campo)
        ),
        error = function(e) numeric()
      )
      if (!length(declarados)) next
      columna <- sql_de(campo)
      if (is.na(columna)) next
      policy <- politicas[[campo]]
      if (!is.list(policy) || !isTRUE(policy$exacta)) {
        # Una fecha de verdad no cuenta centinelas numericos tampoco en
        # memoria: no hay nada que aplicar. Cualquier otra columna cuya politica
        # no se pudo reproducir se declara.
        prototipo_campo <- .atributo_columna_dbi(prototipo, campo, i)
        if (!inherits(prototipo_campo, c("Date", "POSIXt"))) {
          salida$sin_aplicar[nrow(salida$sin_aplicar) + 1L, ] <- list(
            "sentinelas_numericos", campo,
            if (is.list(policy) && length(policy$motivo) == 1L &&
                !is.na(policy$motivo)) {
              as.character(policy$motivo)
            } else {
              "No se pudo reproducir en SQL la comparaci\u00f3n de la memoria."
            }
          )
        }
        next
      }
      condicion <- .condicion_centinelas_declarados_dbi(
        conexion, policy, columna
      )
      # Sin condicion, ningun centinela declarado puede coincidir con el tipo
      # de la columna: no hay nada que excluir, igual que en memoria.
      if (is.null(condicion)) next
      salida$columnas_medibles[[campo]] <- condicion
      salida$agregados_extra[[campo]] <- c(
        salida$agregados_extra[[campo]],
        n_medibles = .contar_no_nulos_sql_dbi(
          .columna_medible_dbi(columna_operacion_de(campo), condicion), senas
        )
      )
    }
  }
  salida
}

# El resumen de una columna con centinelas declarados: lo que no es magnitud
# -validos, distintos, moda- se resume sobre la columna, y las magnitudes sobre
# la columna medible, con la MISMA maquinaria (`.resumen_columna_dbi()`) y la
# cuenta de medibles en el lugar de la de validos. Asi las guardas que deciden
# por la cuenta y por los extremos -sin valores, un solo valor, el entero ancho,
# los infinitos, la escala de la mediana- valen sobre lo que entra en la cifra,
# sin una regla nueva por metrica. Es la idea de `.magnitudes_finitas_dbi()`.
# `resumir(metricas, agregados, columna_sql)` es la llamada de siempre.
.resumen_columna_con_ausencia_dbi <- function(
    resumir, campo, metricas, agregados, columna_sql, condicion) {
  magnitudes <- intersect(metricas, .METRICAS_NUMERICAS_DBI)
  if (is.null(condicion) || !length(magnitudes)) {
    return(resumir(metricas, agregados, columna_sql))
  }
  otras <- setdiff(metricas, .METRICAS_NUMERICAS_DBI)
  medibles <- agregados$extras[[campo]][["n_medibles"]]
  hay_cuenta <- is.list(medibles) && isTRUE(medibles$ok)
  agregados_m <- agregados
  conteos_m <- agregados_m$conteos[[campo]]
  if (is.null(conteos_m)) conteos_m <- list(consolidada = TRUE)
  conteos_m$validos <- if (hay_cuenta) medibles
  conteos_m$distintos <- NULL
  agregados_m$conteos[[campo]] <- conteos_m
  con_validos <- "validos" %in% metricas && hay_cuenta
  medible <- resumir(
    c(if (con_validos) "validos", magnitudes), agregados_m,
    .columna_medible_dbi(columna_sql, condicion)
  )
  campos_magnitud <- unlist(
    .CAMPOS_METRICA_DBI[.METRICAS_NUMERICAS_DBI], use.names = FALSE
  )
  campos_otros <- unlist(
    .CAMPOS_METRICA_DBI[setdiff(names(.CAMPOS_METRICA_DBI),
                                .METRICAS_NUMERICAS_DBI)],
    use.names = FALSE
  )
  if (length(otras)) {
    salida <- resumir(otras, agregados, columna_sql)
    salida$fila[campos_magnitud] <- medible$fila[campos_magnitud]
    salida$sql <- rbind(
      salida$sql[!as.character(salida$sql$metrica) %in% campos_magnitud, ,
                 drop = FALSE],
      medible$sql[!as.character(medible$sql$metrica) %in% campos_otros, ,
                  drop = FALSE]
    )
    salida$literales <- c(salida$literales, medible$literales)
    salida$cuenta_no_decide <- medible$cuenta_no_decide
    salida$n_finitos <- medible$n_finitos
  } else {
    salida <- medible
  }
  salida$n_medibles <- if (hay_cuenta) medibles$valor else NA
  salida
}

# Las columnas que el resumen publica solo cuando hay una politica de ausencia
# declarada, con el nombre y la definicion de `perfilar()`:
# * `n_valores_excluidos_resumen`: cuantas celdas presentes no entraron en las
#   magnitudes -centinelas declarados, `NaN` e infinitos-. `NA` donde el resumen
#   no tiene magnitudes o no pudo establecerlas.
# * con `columnas_opcionales`: `n_aplicables` y `n_no_aplica`, y la ausencia
#   medida sobre las celdas presentes (`n_faltantes = 0`).
.columnas_ausencia_resumen_dbi <- function(columnas, resultados, agregados,
                                           ausencia, es_numerico_resumen) {
  if (!isTRUE(ausencia$activa) || !nrow(columnas)) return(columnas)
  numero <- function(x) {
    if (is.null(x) || !length(x)) return(NA_real_)
    valor <- suppressWarnings(.numero_dbi(x))
    if (length(valor) != 1L) NA_real_ else as.numeric(valor)
  }
  nombres <- as.character(columnas$columna)
  if (isTRUE(ausencia$declara_sentinelas)) {
    columnas$n_valores_excluidos_resumen <- vapply(
      seq_along(nombres), function(i) {
        campo <- nombres[[i]]
        if (!isTRUE(es_numerico_resumen(campo))) return(NA_real_)
        resultado <- resultados[[i]]
        validos <- numero(columnas$n_validos[[i]])
        if (is.na(validos)) return(NA_real_)
        # Si el motor respondio un infinito o un `NaN` y la consulta no los
        # pudo sacar, la cuenta de lo que entro en las magnitudes no se conoce.
        sql <- resultado$sql
        if (is.data.frame(sql) && any(
          as.character(sql$estado) %in% "no_disponible" & (
            grepl(.FRAGMENTO_INFINITO_DBI, as.character(sql$motivo),
                  fixed = TRUE, useBytes = TRUE) |
              grepl(.FRAGMENTO_NAN_DBI, as.character(sql$motivo),
                    fixed = TRUE, useBytes = TRUE)
          ), na.rm = TRUE
        )) return(NA_real_)
        nan <- agregados$nan[[campo]]
        n_nan <- if (is.list(nan) && isTRUE(nan$ok)) numero(nan$valor) else 0
        if (is.na(n_nan) || campo %in% ausencia$opcionales) n_nan <- 0
        utilizables <- numero(resultado$n_finitos)
        if (is.na(utilizables)) {
          utilizables <- if (!is.null(ausencia$columnas_medibles[[campo]])) {
            numero(resultado$n_medibles)
          } else validos
        }
        if (is.na(utilizables)) return(NA_real_)
        validos + n_nan - utilizables
      }, numeric(1L)
    )
  }
  if (isTRUE(ausencia$con_universo) || length(ausencia$opcionales)) {
    opcional <- .nombres_presentes(nombres, ausencia$opcionales)
    validos <- suppressWarnings(as.numeric(columnas$n_validos))
    total <- suppressWarnings(as.numeric(columnas$n))
    columnas$n_aplicables <- ifelse(opcional, validos, total)
    columnas$n_no_aplica <- ifelse(opcional, total - validos, 0)
    columnas$n_aplicabilidad_indeterminada <- ifelse(is.na(total), NA_real_, 0)
    # En una columna opcional la ausencia nunca es defecto: se mide sobre las
    # celdas presentes.
    conocida <- opcional & !is.na(validos)
    columnas$n_faltantes[conocida] <- 0
    columnas$prop_faltantes[conocida] <- ifelse(validos[conocida] > 0, 0, NA_real_)
    # Con una regla aplicada el universo de la columna son las filas donde se
    # cumple: las cuenta el motor, y la ausencia se mide sobre ellas.
    for (campo in as.character(ausencia$aplicadas)) {
      i <- match(campo, nombres)
      if (is.na(i)) next
      # Cada cuenta llega como el resultado de un agregado del lote.
      cuentas <- agregados$extras[[campo]]
      aplicables <- numero(cuentas[["n_aplicables"]]$valor)
      no_aplica <- numero(cuentas[["n_no_aplica"]]$valor)
      # `SUM()` sobre cero filas es `NULL`: en una tabla vacia son cero.
      if (isTRUE(total[[i]] == 0)) {
        if (is.na(aplicables)) aplicables <- 0
        if (is.na(no_aplica)) no_aplica <- 0
      }
      columnas$n_aplicables[[i]] <- aplicables
      columnas$n_no_aplica[[i]] <- no_aplica
      columnas$n_aplicabilidad_indeterminada[[i]] <-
        total[[i]] - aplicables - no_aplica
      if (is.na(aplicables) || is.na(validos[[i]])) {
        columnas$n_faltantes[[i]] <- NA
        columnas$prop_faltantes[[i]] <- NA_real_
        next
      }
      columnas$n_faltantes[[i]] <- aplicables - validos[[i]]
      columnas$prop_faltantes[[i]] <- if (aplicables > 0) {
        (aplicables - validos[[i]]) / aplicables
      } else NA_real_
    }
  }
  columnas
}

# La expresion de una columna con su regla de `aplicabilidad` adentro: fuera
# de las filas donde la regla se cumple la columna es `NULL`, y todo agregado
# sobre ella -validos, distintos, moda, magnitudes- cuenta solo esas filas.
.columna_aplicable_dbi <- function(columna_sql, regla) {
  paste0("CASE WHEN ", regla, " THEN ", columna_sql, " END")
}

# Las expresiones de las columnas del resumen, con la regla traducida de cada
# una adentro (`reglas`: por campo, `list(sql = )`; ver
# `.aplicabilidad_sql_resumen_dbi()`).
.columnas_con_regla_dbi <- function(campos, columnas_sql, reglas) {
  if (!length(reglas)) return(columnas_sql)
  for (i in seq_along(campos)) {
    if (i > length(columnas_sql) || is.na(columnas_sql[[i]])) next
    traducida <- reglas[[campos[[i]]]]
    regla <- if (is.list(traducida)) traducida$sql else NULL
    if (length(regla) != 1L || is.na(regla)) next
    columnas_sql[[i]] <- .columna_aplicable_dbi(columnas_sql[[i]], regla)
  }
  columnas_sql
}

# Una columna `NOT NULL` no cuenta sus validos: son todas las filas. Con una
# regla aplicada son las filas donde la regla se cumple, que el lote ya conto.
.validos_con_regla_dbi <- function(agregados, ausencia) {
  for (campo in as.character(ausencia$aplicadas)) {
    validos <- agregados$conteos[[campo]]$validos
    if (!is.list(validos) || !is.list(validos$metadatos) ||
        !identical(validos$metadatos$fuente, "not_null_catalogo")) next
    aplicables <- agregados$extras[[campo]][["n_aplicables"]]$valor
    if (is.null(aplicables) || !length(aplicables)) next
    aplicables <- suppressWarnings(as.numeric(.numero_dbi(aplicables)))
    if (length(aplicables) != 1L) next
    if (is.na(aplicables) && isTRUE(validos$valor == 0)) aplicables <- 0
    agregados$conteos[[campo]]$validos$valor <- aplicables
  }
  agregados
}

# El esquema que pide el traductor de reglas (`.traducir_predicado_dbi()`),
# armado con lo que la preparacion de `perfilar_dbi()` ya leyo y sin otra
# consulta: el prototipo, el tipo de cada columna y el `CREATE TABLE` de donde
# sale su cotejamiento. Es lo mismo que `.esquema_medicion_dbi()` lee del
# catalogo en SQLite y DuckDB. `NULL` donde eso no alcanza -otro motor, o un
# tipo que la preparacion no leyo-.
.esquema_reglas_preparacion_dbi <- function(conexion, preparacion) {
  senas <- .senas_conexion_dbi(conexion)
  motor <- if (grepl("sqlite", senas, fixed = TRUE, useBytes = TRUE)) {
    "sqlite"
  } else if (grepl("duckdb", senas, fixed = TRUE, useBytes = TRUE)) {
    "duckdb"
  } else NA_character_
  campos <- preparacion$campos
  proto <- preparacion$prototipo
  if (is.na(motor) || !is.data.frame(proto) || !length(campos)) return(NULL)
  posiciones <- .indice_nombre(campos, names(proto))
  if (anyNA(posiciones)) return(NULL)
  proto <- proto[posiciones]
  names(proto) <- campos
  fuente_tipos <- if (identical(motor, "sqlite")) {
    preparacion$tipos_pragma
  } else preparacion$tipos
  tipos <- vapply(seq_along(campos), function(i) {
    tipo <- .atributo_columna_dbi(fuente_tipos, campos[[i]], i)
    if (length(tipo) != 1L || is.na(tipo)) return(NA_character_)
    .mayusculas_ascii(as.character(tipo))
  }, character(1L))
  if (anyNA(tipos)) return(NULL)
  names(tipos) <- campos
  crear <- if (identical(motor, "sqlite")) {
    attr(preparacion$catalogo_cardinalidad, "sql_tabla", exact = TRUE)
  } else preparacion$esquema$sql_tabla
  cotejamientos <- if (length(crear) == 1L && !is.na(crear)) {
    .cotejamientos_sql_tabla(
      crear, campos,
      if (identical(motor, "sqlite")) "binary" else "predeterminado"
    )
  } else rep(NA_character_, length(campos))
  cotejamientos <- stats::setNames(as.character(cotejamientos), campos)
  cotejamientos[is.na(cotejamientos)] <- "desconocido"
  sin_dato <- stats::setNames(rep(NA, length(campos)), campos)
  list(
    motor = motor, campos = campos,
    clases = vapply(proto, .clase_r_columna_dbi, character(1L)),
    tipos = tipos,
    tipos_base = stats::setNames(rep(NA_character_, length(campos)), campos),
    cotejamientos = cotejamientos, colacion_determinista = sin_dato,
    colacion_binaria = sin_dato, colacion_no_pad = sin_dato,
    colacion_bin2 = sin_dato,
    sesion = list(ok = TRUE, motor = motor, n_consultas = 0L),
    n_campos = length(campos), prototipo = .normalizar_columnas_texto(proto)
  )
}

# Una regla puede nombrar un valor de quien la escribio (`~ edad > tope`).
# `perfilar()` lo lee del entorno de la formula al evaluarla; aca se lee igual
# y se escribe en la regla como el literal que es, antes de traducirla: un
# numero, un texto o un logico sin atributos. Un vector entra solo a la derecha
# de `%in%`. Lo que no es eso queda como estaba y el traductor lo declina.
.resolver_constantes_regla_dbi <- function(expr, entorno, campos) {
  if (!is.environment(entorno)) return(expr)
  ausente <- new.env(parent = emptyenv())
  valor_de <- function(nombre) {
    if (!is.na(.indice_nombre(nombre, campos))) return(ausente)
    valor <- get0(nombre, envir = entorno, inherits = TRUE, ifnotfound = ausente)
    if (identical(valor, ausente) || !is.null(attributes(valor)) ||
        !(is.numeric(valor) || is.character(valor) || is.logical(valor)) ||
        !length(valor)) {
      return(ausente)
    }
    valor
  }
  recorrer <- function(x, conjunto = FALSE) {
    if (is.symbol(x)) {
      nombre <- as.character(x)
      if (!nzchar(nombre)) return(x)
      valor <- valor_de(nombre)
      if (identical(valor, ausente)) return(x)
      if (length(valor) == 1L) return(valor)
      if (isTRUE(conjunto) && length(valor) <= 500L) {
        return(as.call(c(list(as.name("c")), as.list(valor))))
      }
      return(x)
    }
    if (!is.call(x)) return(x)
    cabeza <- x[[1L]]
    es_in <- is.symbol(cabeza) && identical(as.character(cabeza), "%in%")
    for (i in seq_along(x)[-1L]) {
      x[[i]] <- recorrer(x[[i]], conjunto = es_in && i == 3L)
    }
    x
  }
  tryCatch(recorrer(expr), error = function(e) expr)
}

# Las reglas de `aplicabilidad` en el SQL del motor, para el resumen de tabla
# completa: por columna, `list(sql = )` con el predicado o `list(motivo = )`
# con lo que impidio traducirla. Traduce `.traducir_predicado_dbi()`, el mismo
# traductor de `medir_dbi()`, con la semantica de `NA` de R; no manda ninguna
# consulta. `NULL` si no hay reglas.
.aplicabilidad_sql_resumen_dbi <- function(conexion, preparacion,
                                           aplicabilidad) {
  if (!is.list(aplicabilidad) || !length(aplicabilidad) ||
      is.null(names(aplicabilidad))) {
    return(NULL)
  }
  campos <- preparacion$campos
  esq <- tryCatch(
    .esquema_reglas_preparacion_dbi(conexion, preparacion),
    error = function(e) NULL
  )
  sin_punto <- function(texto) {
    sub("[.[:space:]]+$", "", as.character(texto), perl = TRUE)
  }
  salida <- list()
  for (j in seq_along(aplicabilidad)) {
    indice <- .indice_nombre(names(aplicabilidad)[[j]], campos)
    if (is.na(indice)) next
    campo <- campos[[indice]]
    f <- aplicabilidad[[j]]
    if (is.null(esq)) {
      salida[[campo]] <- list(motivo = paste(
        "en este motor la regla no se traduce todav\u00eda dentro del resumen"
      ))
      next
    }
    traducida <- tryCatch({
      expr <- .resolver_constantes_regla_dbi(f[[2L]], environment(f), campos)
      .traducir_predicado_dbi(
        expr, environment(f), as.list(esq$clases), esq, esq$motor, conexion
      )
    }, error = function(e) list(sql = NULL, motivo = conditionMessage(e)))
    salida[[campo]] <- if (length(traducida$sql) == 1L &&
                           !is.na(traducida$sql) && !length(traducida$guardas)) {
      list(sql = as.character(traducida$sql))
    } else {
      list(motivo = sin_punto(
        traducida$motivo %||% traducida$motivo_guarda %||%
          "la regla no se pudo traducir al SQL del motor"
      ))
    }
  }
  salida
}

# Lo que el resumen publica de la politica de ausencia que aplico
# (`resumen_tabla$meta$politica_ausencia`): por politica, las columnas donde se
# aplico y, por columna, lo que no se pudo aplicar y por que. Es lo que leen la
# cobertura, la corroboracion entre bloques y la comparacion entre corridas.
.meta_politica_ausencia_dbi <- function(ausencia) {
  list(
    sentinelas_numericos = names(ausencia$columnas_medibles),
    columnas_opcionales = as.character(ausencia$opcionales),
    aplicabilidad = as.character(ausencia$aplicadas),
    sin_aplicar = ausencia$sin_aplicar
  )
}

# La cobertura de lo que el resumen no pudo aplicar de la politica de ausencia:
# una fila por columna y politica, con el motivo. `elemento` es
# `columna::politica`.
.cobertura_politica_sin_aplicar_dbi <- function(sin_aplicar) {
  if (!is.data.frame(sin_aplicar) || !nrow(sin_aplicar)) {
    return(.cobertura_dbi_vacia())
  }
  do.call(rbind, lapply(seq_len(nrow(sin_aplicar)), function(i) {
    politica <- as.character(sin_aplicar$politica[[i]])
    columna <- as.character(sin_aplicar$columna[[i]])
    motivo <- as.character(sin_aplicar$motivo[[i]])
    if (identical(politica, "aplicabilidad")) {
      .registro_cobertura_dbi(
        "resumen_tabla", paste0(columna, "::", politica), "degradado",
        paste0(
          "El resumen SQL no aplic\u00f3 la regla de `aplicabilidad` de `",
          columna, "`: ", motivo, ". Sus cifras cuentan todas las filas; las",
          " de `perfil_muestra`, cuando lo hay, s\u00f3lo las filas donde la",
          " regla se cumple."
        ),
        paste(
          "Usar las cifras de `perfil_muestra` para esta columna, o escribir",
          "la regla con comparaciones, `%in%`, `is.na()`, `&`, `|` y `!` sobre",
          "columnas y valores literales."
        )
      )
    } else {
      .registro_cobertura_dbi(
        "resumen_tabla", paste0(columna, "::", politica), "degradado",
        paste0(
          "El resumen SQL no aplic\u00f3 `", politica, "` a `", columna, "`: ",
          motivo, " Sus magnitudes cuentan los valores declarados ausentes;",
          " las de `perfil_muestra`, cuando lo hay, no."
        ),
        "Usar las cifras de `perfil_muestra` para esta columna."
      )
    }
  }))
}

# Una regla de `aplicabilidad` que sobre las filas leidas no dio un valor
# logico no se aplico en ningun bloque, y quien la encontro ya la declaro para
# su columna (`.cobertura_aplicabilidad_no_logica_dbi()`). Sale de lo que el
# resumen publica como "sin aplicar", igual que la que se resuelve antes de
# leer con el tipo de las columnas: la columna queda con una sola fila de
# cobertura, que dice lo que paso, y la corroboracion compara sus cifras, que
# cuentan las mismas filas en los dos bloques. `reglas` es lo que devuelve
# `.reglas_aplicabilidad_no_logicas()`.
.politica_sin_reglas_no_logicas_dbi <- function(resumen, reglas) {
  if (!is.data.frame(reglas) || !nrow(reglas) || !is.list(resumen) ||
      !is.list(resumen$meta[["politica_ausencia"]])) {
    return(resumen)
  }
  sin_aplicar <- resumen$meta[["politica_ausencia"]][["sin_aplicar"]]
  if (!is.data.frame(sin_aplicar) || !nrow(sin_aplicar)) return(resumen)
  de_la_regla <- as.character(sin_aplicar$politica) == "aplicabilidad" &
    !is.na(.indice_nombre(as.character(sin_aplicar$columna), reglas$columna))
  if (!any(de_la_regla)) return(resumen)
  sin_aplicar <- sin_aplicar[!de_la_regla, , drop = FALSE]
  rownames(sin_aplicar) <- NULL
  resumen$meta[["politica_ausencia"]][["sin_aplicar"]] <- sin_aplicar
  resumen
}

# Los argumentos de la politica de ausencia que valen sobre unas filas ya
# leidas: sin las reglas de `aplicabilidad` que sobre ellas no dan un valor
# logico, que no se aplican en ningun bloque. No declara nada: lo hace quien
# perfila esas filas.
.argumentos_ausencia_sobre_datos_dbi <- function(datos, argumentos) {
  if (!is.data.frame(datos) || !is.list(argumentos[["aplicabilidad"]])) {
    return(argumentos)
  }
  .aplicabilidad_sobre_datos_dbi(datos, argumentos)$argumentos
}

# La politica de ausencia de una corrida cuyo resumen sale de `perfilar()`
# sobre las filas leidas -la materializacion de `muestra_motor`-: `perfilar()`
# la aplica entera. Devuelve `NULL` si no hay nada declarado; si lo hay, las
# columnas que el resumen agrega y lo que publica en `meta$politica_ausencia`.
.politica_ausencia_spool_dbi <- function(argumentos, campos) {
  sentinelas <- argumentos[["sentinelas_numericos"]]
  opcionales <- argumentos[["columnas_opcionales"]]
  aplicabilidad <- argumentos[["aplicabilidad"]]
  declara_sentinelas <- !is.null(sentinelas) &&
    .hay_sentinelas_declarados(sentinelas)
  universo <- length(opcionales) > 0L ||
    (is.list(aplicabilidad) && length(aplicabilidad) > 0L)
  if (!declara_sentinelas && !universo) return(NULL)
  con_centinelas <- if (declara_sentinelas) {
    campos[vapply(campos, function(campo) {
      length(tryCatch(
        .sentinelas_numericos_declarados(
          .sentinelas_de_columna(sentinelas, campo)
        ),
        error = function(e) numeric()
      )) > 0L
    }, logical(1L))]
  } else character()
  list(
    campos = c(
      if (declara_sentinelas) "n_valores_excluidos_resumen",
      if (universo) c(
        "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada"
      )
    ),
    meta = list(
      sentinelas_numericos = con_centinelas,
      columnas_opcionales = campos[.nombres_presentes(
        campos, as.character(opcionales)
      )],
      aplicabilidad = campos[.nombres_presentes(
        campos, as.character(names(aplicabilidad))
      )],
      sin_aplicar = data.frame(
        politica = character(), columna = character(), motivo = character(),
        stringsAsFactors = FALSE
      )
    )
  )
}

# Que celdas de las filas leidas quedan fuera de las magnitudes de una columna
# por la politica de ausencia declarada, con las mismas reglas que `perfilar()`:
# las que no aplican (`.resolver_aplicabilidad()`) y los centinelas declarados
# (`.mascara_sentinelas_resumen()`). Devuelve una funcion `(columna, valores)`.
.mascaras_ausencia_muestra_dbi <- function(datos, politica) {
  nada <- function(columna, valores) rep(FALSE, length(valores))
  if (!is.list(politica) || !is.data.frame(datos)) return(nada)
  sentinelas <- politica[["sentinelas_numericos"]]
  opcionales <- politica[["columnas_opcionales"]] %||% character()
  aplicabilidad <- politica[["aplicabilidad"]]
  declara <- !is.null(sentinelas) && .hay_sentinelas_declarados(sentinelas)
  universo <- length(opcionales) > 0L ||
    (is.list(aplicabilidad) && length(aplicabilidad) > 0L)
  if (!declara && !universo) return(nada)
  mascaras <- if (universo) {
    tryCatch(
      .resolver_aplicabilidad(
        datos, names(datos), columnas_opcionales = opcionales,
        aplicabilidad = aplicabilidad
      )$mascaras,
      error = function(e) NULL
    )
  }
  function(columna, valores) {
    fuera <- rep(FALSE, length(valores))
    posicion <- match(columna, names(datos))
    if (!is.na(posicion) && length(mascaras) >= posicion &&
        !is.null(mascaras[[posicion]]) &&
        length(mascaras[[posicion]]) == length(valores)) {
      fuera <- !as.logical(mascaras[[posicion]])
    }
    if (declara) {
      declarados <- tryCatch(
        .sentinelas_numericos_declarados(
          .sentinelas_de_columna(sentinelas, columna)
        ),
        error = function(e) numeric()
      )
      if (length(declarados)) {
        fuera <- fuera | .mascara_sentinelas_resumen(valores, declarados)
      }
    }
    fuera
  }
}

# Que celdas son un centinela declarado para las magnitudes, con la regla de
# `perfilar()`: igualdad de dobles (`.mascara_sentinelas_resumen()`) y, en un
# `integer64`, por sus cifras exactas (`.resumen_integer64()`). Fechas, texto y
# logicos no cuentan centinelas numericos.
.mascara_centinelas_magnitud_dbi <- function(valores, declarados) {
  n <- length(valores)
  if (!length(declarados) || inherits(valores, c("Date", "POSIXt"))) {
    return(rep(FALSE, n))
  }
  if (inherits(valores, "integer64")) {
    return(!is.na(valores) &
             .texto_identidad(valores) %in% .texto_identidad(declarados))
  }
  if (!is.numeric(valores)) return(rep(FALSE, n))
  .mascara_sentinelas_resumen(valores, declarados)
}

# El mapa de distintos sin los centinelas declarados, para reconstruir la
# mediana: se les quita la frecuencia en lugar de la fila, porque recortar una
# columna `integer64` sin `bit64` adjunto reinterpreta sus bits.
.mapa_sin_centinelas_bloques_dbi <- function(mapa, declarados,
                                             es_centinela = NULL) {
  if (!is.data.frame(mapa) || !nrow(mapa) || !length(declarados) ||
      !all(c("representante", "frecuencia") %in% names(mapa))) {
    return(mapa)
  }
  # `es_centinela`: la mascara de la corrida, que en un tipo de 4 bytes busca
  # el `float32` del centinela, como al absorber cada bloque.
  fuera <- if (is.function(es_centinela)) {
    es_centinela(mapa$representante)
  } else .mascara_centinelas_magnitud_dbi(mapa$representante, declarados)
  if (length(fuera) != nrow(mapa)) return(mapa)
  if (any(fuera)) mapa$frecuencia[fuera] <- 0
  mapa
}

# El motivo con que queda sin cifras por bloques una columna cuya regla de
# `aplicabilidad` se supo no logica despues de haber leido filas con ella en
# pie (ver `.politica_ausencia_bloques_dbi()`).
.MOTIVO_REGLA_NO_LOGICA_TARDIA_DBI <- paste0(
  "aplicabilidad_no_logica_tras_bloques_leidos: la regla de `aplicabilidad` de",
  " la columna dio un valor l\u00f3gico en los primeros bloques y otro despu\u00e9s"
)

# La politica de ausencia declarada para la lectura por bloques. Los bloques
# llegan a R, asi que se aplica con las reglas de `perfilar()` y no con SQL:
# `.resolver_aplicabilidad()` para el universo de cada columna y
# `.mascara_centinelas_magnitud_dbi()` para los centinelas. Devuelve `NULL` si no
# hay nada declarado. Si una regla no se puede evaluar sobre el bloque, el
# universo no se aplica a ninguna columna y queda declarado con el motivo.
#
# Una regla que sobre las filas de un bloque no da un valor logico no se
# aplica, ni ahi ni en los bloques que siguen, y las demas si: es lo que decide
# la muestra con sus filas (`.aplicabilidad_sobre_datos_dbi()`). Quien recorre
# los bloques la declara una vez (`no_logicas()`). Si eso se supo despues de
# haber leido filas con la regla en pie -una columna cuyo tipo el motor no
# declara llega como logica mientras sus filas son todas `NULL`-, lo acumulado
# de esa columna ya mezcla las dos lecturas: `mascaras()` lo marca con `fallo`
# y la columna queda sin cifras, con ese motivo.
.politica_ausencia_bloques_dbi <- function(argumentos, campos,
                                           alinear = NULL) {
  base <- .politica_ausencia_spool_dbi(argumentos, campos)
  if (is.null(base)) return(NULL)
  sentinelas <- argumentos[["sentinelas_numericos"]]
  opcionales <- argumentos[["columnas_opcionales"]] %||% character()
  aplicabilidad <- argumentos[["aplicabilidad"]]
  estado <- new.env(parent = emptyenv())
  estado$error <- NULL
  estado$aplicabilidad <- aplicabilidad
  estado$no_logicas <- .reglas_aplicabilidad_no_logicas(NULL, NULL)
  estado$tardias <- character()
  estado$filas_leidas <- 0
  declarados <- stats::setNames(lapply(campos, function(campo) {
    if (is.null(sentinelas)) return(numeric())
    tryCatch(
      .sentinelas_numericos_declarados(
        .sentinelas_de_columna(sentinelas, campo)
      ),
      error = function(e) numeric()
    )
  }), campos)
  con_universo <- campos[
    .nombres_presentes(campos, as.character(opcionales)) |
      .nombres_presentes(campos, as.character(names(aplicabilidad)))
  ]
  mascaras <- function(datos) {
    salida <- list()
    if (length(estado$aplicabilidad) && is.null(estado$error)) {
      nuevas <- .reglas_aplicabilidad_no_logicas(datos, estado$aplicabilidad)
      if (nrow(nuevas)) {
        estado$no_logicas <- rbind(estado$no_logicas, nuevas)
        if (estado$filas_leidas > 0) {
          estado$tardias <- c(estado$tardias, nuevas$columna)
        }
        estado$aplicabilidad <- .quitar_reglas_aplicabilidad_dbi(
          estado$aplicabilidad, nuevas
        )
      }
    }
    estado$filas_leidas <- estado$filas_leidas + NROW(datos)
    universo <- if (length(con_universo) && is.null(estado$error)) {
      tryCatch(
        .resolver_aplicabilidad(
          datos, names(datos), columnas_opcionales = opcionales,
          aplicabilidad = estado$aplicabilidad
        )$mascaras,
        error = function(e) {
          estado$error <- conditionMessage(e)
          NULL
        }
      )
    }
    # Un tipo de 4 bytes guarda el `float32` del centinela: se alinea como en
    # la muestra antes de buscarlo (`.ajustar_sentinelas_real_dbi()`).
    alineados <- if (is.function(alinear)) {
      tryCatch(alinear(datos), error = function(e) datos)
    } else datos
    for (campo in campos) {
      posicion <- .indice_nombre(campo, names(datos))
      if (is.na(posicion)) next
      aplicable <- NULL
      if (!is.null(universo) && length(universo) >= posicion &&
          !is.null(universo[[posicion]])) {
        mascara <- universo[[posicion]]
        aplicable <- as.logical(mascara)
        indeterminados <- attr(
          mascara, "indeterminados_por_fila", exact = TRUE
        )
        if (length(indeterminados) == length(aplicable)) {
          aplicable[indeterminados] <- NA
        }
      }
      centinela <- if (length(declarados[[campo]])) {
        .mascara_centinelas_magnitud_dbi(
          alineados[[posicion]], declarados[[campo]]
        )
      }
      if (!is.null(aplicable) || !is.null(centinela)) {
        salida[[campo]] <- list(aplicable = aplicable, centinela = centinela)
      }
      if (!is.na(.indice_nombre(campo, estado$tardias))) {
        salida[[campo]]$fallo <- .MOTIVO_REGLA_NO_LOGICA_TARDIA_DBI
      }
    }
    salida
  }
  list(
    mascaras = mascaras,
    declarados = function(campo) declarados[[campo]] %||% numeric(),
    # Que valores de una columna son un centinela declarado, con la misma
    # alineacion que `mascaras()`: lo usa la mediana, que se reconstruye de los
    # valores distintos y no de las filas.
    centinela = function(campo, valores) {
      propios <- declarados[[campo]] %||% numeric()
      if (!length(propios)) return(rep(FALSE, length(valores)))
      alineados <- valores
      if (is.function(alinear) && is.numeric(valores) &&
          !inherits(valores, "integer64")) {
        suelta <- stats::setNames(
          data.frame(valores, stringsAsFactors = FALSE), campo
        )
        alineados <- tryCatch(alinear(suelta)[[1L]], error = function(e) valores)
        if (length(alineados) != length(valores)) alineados <- valores
      }
      .mascara_centinelas_magnitud_dbi(alineados, propios)
    },
    con_universo = function(campo) {
      campo %in% con_universo && is.null(estado$error) &&
        is.na(.indice_nombre(campo, estado$no_logicas$columna))
    },
    # Las reglas que sobre las filas leidas no dieron un valor logico: no se
    # aplicaron y quien recorre los bloques las declara.
    no_logicas = function() estado$no_logicas,
    campos = base$campos,
    meta = function() {
      meta <- base$meta
      meta$aplicabilidad <- meta$aplicabilidad[is.na(.indice_nombre(
        meta$aplicabilidad, estado$no_logicas$columna
      ))]
      if (!is.null(estado$error)) {
        meta$sin_aplicar <- data.frame(
          politica = ifelse(
            con_universo %in% meta$columnas_opcionales,
            "columnas_opcionales", "aplicabilidad"
          ),
          columna = con_universo,
          motivo = rep(paste0(
            "la regla no se pudo evaluar sobre las filas le\u00eddas por bloques: ",
            estado$error
          ), length(con_universo)),
          stringsAsFactors = FALSE
        )
        # La regla que no dio un valor logico ya esta declarada aparte.
        meta$sin_aplicar <- meta$sin_aplicar[is.na(.indice_nombre(
          as.character(meta$sin_aplicar$columna), estado$no_logicas$columna
        )), , drop = FALSE]
        rownames(meta$sin_aplicar) <- NULL
        meta$columnas_opcionales <- character()
        meta$aplicabilidad <- character()
      }
      meta
    }
  )
}

# Las columnas que el resumen por bloques agrega con una politica declarada,
# las mismas que el resumen SQL (`.columnas_ausencia_resumen_dbi()`).
.columnas_ausencia_bloques_dbi <- function(columnas, ausencia, por_columna) {
  if (!nrow(columnas)) return(columnas)
  nombres <- as.character(columnas$columna)
  for (campo_nuevo in ausencia$campos) {
    columnas[[campo_nuevo]] <- vapply(nombres, function(nombre) {
      valor <- por_columna[[nombre]][[campo_nuevo]]
      valor <- suppressWarnings(as.numeric(valor))
      if (length(valor) != 1L) NA_real_ else valor
    }, numeric(1L), USE.NAMES = FALSE)
  }
  columnas
}

# Los agregados que la politica de ausencia pide junto a las magnitudes
# (`n_medibles`, las cuentas de la regla), para la consulta propia de una
# columna que el lote consolidado no pudo medir: las expresiones con su alias.
.extras_ausencia_consulta_dbi <- function(extras_campo, alias) {
  if (!length(extras_campo)) {
    return(list(aliases = list(), expresiones = character()))
  }
  aliases <- stats::setNames(lapply(names(extras_campo), function(nombre) {
    alias(paste0("lupa_", nombre))
  }), names(extras_campo))
  list(
    aliases = aliases,
    expresiones = paste0(
      unname(extras_campo), " AS ", unlist(aliases, use.names = FALSE)
    )
  )
}

# Esos agregados leidos del resultado de la consulta, con la forma en que los
# deja el lote consolidado. `NULL` si la consulta no salio.
.extras_ausencia_resultado_dbi <- function(consulta, aliases, metadatos) {
  if (!length(aliases) || !is.list(consulta) || !isTRUE(consulta$ok)) {
    return(NULL)
  }
  lapply(aliases, function(alias_extra) {
    .resultado_lote_dbi(
      consulta, consulta$sql %||% NA_character_, alias_extra, metadatos
    )
  })
}
