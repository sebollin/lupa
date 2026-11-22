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
    columnas_opcionales = character(), aplicabilidad = NULL) {
  senas <- .senas_conexion_dbi(conexion)
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
    if (campo %in% columnas_opcionales || campo %in% aplicar) {
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
    if (!es_numero || es_fecha || integer64 || !length(sentinelas)) {
      motivo <- if (integer64) {
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
    comparar_double_original = FALSE, peso = NULL) {
  if (is.null(aliases) || !isTRUE(policy$exacta)) return(character())
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
    if (exacta || isTRUE(doble)) {
      if (isTRUE(comparar_double_original)) columna_original else columna
    } else {
      paste0("CAST(", columna, " AS ",
             .tipo_numerico_texto_faltantes_dbi(senas), ")")
    }
  }
  literal_de <- function(valor) {
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
  condiciones_numericas <- if (length(policy$numericos)) {
    paste0(
      if (any(vapply(literales_info, `[[`, logical(1L), "doble"))) {
        columna_doble
      } else numerico_texto,
      " IN (", paste(literales, collapse = ", "), ")"
    )
  } else character()
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
    sin_candidatos <- if (length(literales)) paste0(
      valor_columna, " NOT IN (", paste(literales, collapse = ", "), ")"
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
        paste0(
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
    sin_candidatos <- paste0(
      valor_columna, " NOT IN (", paste(literales, collapse = ", "), ")"
    )
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
