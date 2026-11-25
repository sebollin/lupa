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
  f <- function(regla, sqlite, duckdb, postgresql, mysql, mariadb, sqlserver) {
    data.frame(
      regla = regla,
      motor = c("sqlite", "duckdb", "postgresql", "mysql", "mariadb", "sqlserver"),
      politica = c(sqlite, duckdb, postgresql, mysql, mariadb, sqlserver),
      stringsAsFactors = FALSE
    )
  }
  rbind(
    f("no_ausente sin centinela (toda clase)",
      "sql",
      "sql (NaN -> NULL)",
      "sql (NaN -> NULL)",
      "sql (binarios no_medible bajo RMySQL)",
      "sql",
      "sql: COUNT_BIG y SUM(CAST(... AS BIGINT))"),
    f("no_ausente con centinela numerico o de fecha",
      "sql; fecha solo con tipos extendidos (si no, R lee un numero y la clase no coincide)",
      "sql",
      "sql; float4, float8 y timestamptz emulan la lectura de R",
      "sql; TIMESTAMP exige sesion en UTC; tipos temporales leidos como texto no_medible",
      "sql; TIMESTAMP exige sesion en UTC",
      "sql sobre DATE, DATETIME, DATETIME2 y DATETIMEOFFSET por epoca"),
    f("no_ausente con centinela de texto ASCII",
      "sql: TRIM de espacio, tab, LF, CR + REPLACE(U+0130, 'i') + LOWER, solo si el valor es ASCII",
      "sql: TRIM con chr() + REPLACE(U+0130, 'i') + LOWER, solo si el valor es ASCII",
      "sql_con_verificacion: colacion determinista, UTF8, lower('I') = 'i' y BTRIM; bpchar no_medible",
      "sql_con_verificacion: colacion binaria NO PAD, lower('I') = 'i' y REGEXP_REPLACE en bordes",
      "sql_con_verificacion: colacion binaria NO PAD (_nopad_bin), lower('I') = 'i' y REGEXP_REPLACE en bordes",
      "sql_con_guarda: _BIN2 y sin espacios finales; char/nchar no_medible"),
    f("no_ausente con centinela no ASCII",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible",
      "sql_con_guarda: pliegue explicito; Unicode fuera del mapa no_medible"),
    f("en_conjunto / Formato(diccionario) sobre numero, fecha o logico",
      "sql (logico: R lee entero; la clase no coincide con TRUE/FALSE y declina)",
      "sql",
      "sql; float4, float8 y timestamptz emulan la lectura de R",
      "sql: literal de la misma clase; FLOAT bajo RMySQL emula su lectura textual",
      "sql: literal de la misma clase; TIMESTAMP exige sesion en UTC; BIT como 1/0",
      "sql sobre numero, BIT, DATE, DATETIME, DATETIME2 y DATETIMEOFFSET por epoca"),
    f("en_conjunto / Formato(diccionario) sobre texto",
      "sql_con_guarda: texto no ASCII = 0 (SQLite no valida UTF-8 y R lee el byte invalido como NA); COLLATE BINARY explicito",
      "sql: COLLATE \"binary\" explicito",
      "sql_con_verificacion: colacion determinista; char(n)/bpchar no_medible",
      "sql_con_guarda: colacion binaria NO PAD, o binaria PAD SPACE con espacios finales = 0 en columna y dominio",
      "sql_con_guarda: colacion binaria NO PAD (_nopad_bin), o binaria PAD SPACE con espacios finales = 0 en columna y dominio",
      "sql_con_guarda: colacion _BIN2 sobre varchar/nvarchar y espacios finales = 0 en columna y dominio; char/nchar no_medible"),
    f("en_rango sobre numero, fecha o instante",
      "sql (fecha e instante solo con tipos extendidos)",
      "sql",
      "sql; float4, float8 y timestamptz emulan la lectura de R",
      "sql; TIMESTAMP exige sesion en UTC; FLOAT bajo RMySQL emula su lectura textual",
      "sql; TIMESTAMP exige sesion en UTC",
      "sql sobre numero, DATE, DATETIME, DATETIME2 y DATETIMEOFFSET por epoca"),
    f("en_rango sobre texto",
      "no_medible", "no_medible", "no_medible", "no_medible", "no_medible", "no_medible"),
    f("en_rango sobre texto con orden_texto = \"bytes\"",
      "sql_con_guarda: COLLATE BINARY en una base UTF-8; texto no ASCII = 0",
      "sql: encode() compara los bytes del UTF-8; el texto que no es UTF-8 valido queda fuera, como en memoria (try(decode())); sin try(), no_medible",
      "no_medible: COLLATE \"C\" escrito, sin comparar con memoria",
      "no_medible: CAST(CONVERT(... USING utf8mb4) AS BINARY) escrito, sin comparar con memoria",
      "no_medible: CAST(CONVERT(... USING utf8mb4) AS BINARY) escrito, sin comparar con memoria",
      "no_medible: _BIN2_UTF8 escrito, sin comparar con memoria"),
    f("fecha_en_rango (Oportunidad*)",
      "sql en segundos: col * 86400 o col (tipos extendidos); sin ellos R lee numero y las dos vias se abstienen",
      "sql en segundos: epoch()",
      "sql: extract(epoch from ...) sobre DATE/TIMESTAMP/TIMESTAMPTZ",
      "sql: DATEDIFF / TIMESTAMPDIFF en microsegundos (la lectura textual de FLOAT se emula)",
      "sql: DATEDIFF / TIMESTAMPDIFF en microsegundos sobre DATE/DATETIME; TIMESTAMP exige sesion en UTC",
      "sql: DATEDIFF_BIG en microsegundos sobre DATE/DATETIME/DATETIME2/DATETIMEOFFSET"),
    f("duplicado sobre numero, fecha o instante",
      "sql (GROUP BY)",
      "sql (GROUP BY; NaN agrupa aparte de NULL, como R)",
      "sql (GROUP BY; NaN agrupa aparte de NULL en conjunto y se excluye en atributo)",
      "sql (GROUP BY; la lectura textual de FLOAT bajo RMySQL se emula)",
      "sql (GROUP BY; TIMESTAMP exige sesion en UTC)",
      "sql (GROUP BY con COUNT_BIG; DATETIMEOFFSET por epoca)"),
    f("duplicado sobre texto",
      "sql_con_guarda: texto no ASCII = 0 (AtributoDuplicado); ConjuntoAtributosDuplicado compara bytes en las dos vias; GROUP BY con COLLATE BINARY",
      "sql (GROUP BY con COLLATE \"binary\")",
      "sql_con_verificacion: colacion determinista; bpchar no_medible",
      "sql_con_guarda: binaria NO PAD, o binaria PAD SPACE con espacios finales = 0",
      "sql_con_guarda: binaria NO PAD (_nopad_bin), o binaria PAD SPACE con espacios finales = 0",
      "sql_con_guarda: _BIN2 sobre varchar/nvarchar y espacios finales = 0; char/nchar no_medible"),
    f("predicado de fila con numeros y fechas (==, !=, <, <=, >, >=, is.na, %in%, !, &, |)",
      "sql", "sql",
      "sql (float4, float8 y timestamptz emulan la lectura de R)",
      "sql (TIMESTAMP exige sesion en UTC; FLOAT bajo RMySQL emula su lectura textual)",
      "sql (TIMESTAMP exige sesion en UTC)",
      "sql: logica de tres valores compuesta con CASE; DATETIMEOFFSET por epoca"),
    f("predicado de fila con texto (==, !=, %in%, is.na)",
      "sql (COLLATE BINARY explicito)",
      "sql (COLLATE \"binary\" explicito)",
      "sql_con_verificacion: colacion determinista; bpchar no_medible",
      "sql_con_guarda: binaria NO PAD, o binaria PAD SPACE con espacios finales = 0 en columna y literales",
      "sql_con_guarda: binaria NO PAD (_nopad_bin), o binaria PAD SPACE con espacios finales = 0 en columna y literales",
      "sql_con_guarda: _BIN2 con CASE de tres valores y espacios finales = 0 en columna y literales; char/nchar no_medible"),
    f("predicado de fila con orden sobre texto (<, <=, >, >=)",
      "no_medible: R ordena con el locale de la sesion",
      "no_medible: R ordena con el locale de la sesion",
      "no_medible", "no_medible", "no_medible", "no_medible"),
    f("predicado de fila con orden sobre texto con orden_texto = \"bytes\"",
      "sql: COLLATE BINARY en una base UTF-8",
      "sql: encode() compara los bytes del UTF-8",
      "no_medible: COLLATE \"C\" escrito, sin comparar con memoria",
      "no_medible: CAST(CONVERT(... USING utf8mb4) AS BINARY) escrito, sin comparar con memoria",
      "no_medible: CAST(CONVERT(... USING utf8mb4) AS BINARY) escrito, sin comparar con memoria",
      "no_medible: _BIN2_UTF8 escrito, sin comparar con memoria"),
    f("predicado de fila con columna logica",
      "no_medible: SQLite guarda el logico como entero",
      "sql",
      "sql",
      "sql sobre TINYINT(1); BIT no se compara bajo RMySQL",
      "sql sobre BIT(1); BOOLEAN/TINYINT(1) conserva la comparacion logica",
      "sql sobre BIT con CASE de tres valores"),
    f("predicado de fila con %in% que incluye NA sobre doble con NaN",
      "sql (SQLite no guarda NaN)",
      "no_medible: R no iguala NaN con NA",
      "no_medible: PostgreSQL guarda NaN y R no lo iguala con NA",
      "sql (MySQL no guarda NaN)",
      "sql (MariaDB no guarda NaN)",
      "sql (SQL Server no guarda NaN)"),
    f("cobertura_referencial (misma clase; o texto contra entero)",
      "sql_con_guarda: texto no ASCII = 0; igualdad con COLLATE BINARY",
      "sql (igualdad con COLLATE \"binary\")",
      "sql_con_verificacion: colacion determinista; CAST(entero AS TEXT); bpchar y float4 no_medible",
      "sql_con_guarda: binaria NO PAD o espacios finales = 0; CAST(entero AS CHAR) COLLATE",
      "sql_con_guarda: binaria NO PAD o espacios finales = 0; CAST(entero AS CHAR) COLLATE",
      "sql_con_guarda: _BIN2 y espacios finales = 0; CAST(entero AS VARCHAR(20)/NVARCHAR(20)) COLLATE"),
    f("cobertura_referencial (doble contra texto, integer64 contra doble, fecha contra instante)",
      "no_medible", "no_medible", "no_medible", "no_medible", "no_medible", "no_medible")
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

.precondicion_motor_medicion <- function(motor, nodo, clases, sesion = NULL) {
  if (motor %in% c("sqlite", "duckdb")) return(NULL)
  if (motor %in% c("postgresql", "mysql", "mariadb", "sqlserver")) {
    if (is.null(sesion) || !isTRUE(sesion$ok)) {
      regla <- .regla_politica_nodo(nodo, clases)
      fila <- .POLITICA_MOTORES_MEDICION[
        .POLITICA_MOTORES_MEDICION$motor == motor &
          .POLITICA_MOTORES_MEDICION$regla == regla, , drop = FALSE
      ]
      detalle <- if (nrow(fila)) paste0(" (", fila$politica[[1L]], ")") else ""
      motivo_base <- if (!is.null(sesion) && !is.null(sesion$motivo)) {
        sesion$motivo
      } else {
        paste0(
          "no se pudo verificar la conexi\u00f3n ni los metadatos del motor `",
          motor, "`"
        )
      }
      return(paste0(motivo_base, detalle))
    }
    return(NULL)
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

# Una tabla de la coleccion por su identificador o, si no, por su nombre: la
# regla de las entidades y de la tabla de un referencial del motor. `NULL` si no
# esta declarada.
.fisica_tabla_coleccion_dbi <- function(nombre, coleccion,
                                        col_tablas = .completar_columnas_coleccion(
                                          coleccion$tablas
                                        )) {
  i <- match(nombre, as.character(col_tablas$identificador))
  if (is.na(i)) i <- match(nombre, as.character(col_tablas$tabla))
  if (is.na(i)) NULL else col_tablas$referencia[[i]]
}

.resolver_tablas_medir_dbi <- function(modelo, conexion, tablas) {
  entidades <- unique(unlist(lapply(modelo$metricas, `[[`, "entidad")))
  con <- conexion
  mapa <- stats::setNames(as.list(entidades), entidades)
  if (inherits(conexion, "coleccion_lupa")) {
    con <- conexion$conexion
    col_tablas <- .completar_columnas_coleccion(conexion$tablas)
    for (e in entidades) {
      fisica <- .fisica_tabla_coleccion_dbi(e, conexion, col_tablas)
      if (!is.null(fisica)) mapa[[e]] <- fisica
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

.verificar_sesion_medicion_dbi <- function(conexion, motor, contador = NULL,
                                           orden_texto = "idioma") {
  if (motor %in% c("sqlite", "duckdb")) {
    sesion <- list(ok = TRUE, motor = motor, n_consultas = 0L)
    # El orden por bytes se traduce en SQLite con `COLLATE BINARY`, que compara
    # con `memcmp()` los bytes que la base guarda: en una base UTF-16 no son los
    # del UTF-8, y el orden sale otro. Se pregunta solo si se pidio ese orden,
    # para no gastar una consulta en la medicion por omision.
    if (identical(motor, "sqlite") && identical(orden_texto, "bytes")) {
      info <- tryCatch(
        if (is.null(contador)) {
          .traer_consulta_dbi(conexion, "PRAGMA encoding")
        } else {
          .consultar_medicion_dbi(conexion, "PRAGMA encoding", contador,
                                  registrar_sql = FALSE)
        },
        lupa_presupuesto_medicion_dbi = function(e) e,
        error = function(e) NULL
      )
      if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
      sesion$n_consultas <- 1L
      sesion$codificacion <- if (is.data.frame(info) && nrow(info)) {
        as.character(info[[1L]][[1L]])
      } else {
        NA_character_
      }
    }
    # DuckDB guarda tal cual el texto con bytes que no son UTF-8 valido que le
    # escribe R, y el rango de la memoria lo deja afuera (`.texto_analizable()`
    # lo lleva a `NA`): el motor contaba 2 de 4 donde la memoria cuenta 1 de 3.
    # Medido. `try(decode(encode(x)))` da `NULL` justo sobre ese texto; se
    # pregunta si esta version de DuckDB lo tiene.
    if (identical(motor, "duckdb") && identical(orden_texto, "bytes")) {
      info <- tryCatch(
        if (is.null(contador)) {
          .traer_consulta_dbi(conexion, "SELECT try(decode(encode('a'))) AS t")
        } else {
          .consultar_medicion_dbi(
            conexion, "SELECT try(decode(encode('a'))) AS t", contador,
            registrar_sql = FALSE
          )
        },
        lupa_presupuesto_medicion_dbi = function(e) e,
        error = function(e) NULL
      )
      if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
      sesion$n_consultas <- 1L
      sesion$valida_utf8 <- is.data.frame(info) && nrow(info) == 1L &&
        identical(as.character(info[[1L]][[1L]]), "a")
    }
    return(sesion)
  }
  if (!motor %in% c("postgresql", "mysql", "mariadb", "sqlserver")) {
    return(list(
      ok = FALSE, motor = motor, n_consultas = 0L,
      motivo = paste0(
        "motor no reconocido (`", motor,
        "`): no se puede garantizar la equivalencia con R"
      )
    ))
  }
  motor_real <- .motor_medicion_dbi(conexion, "auto")
  compatible <- if (motor %in% c("mysql", "mariadb")) {
    motor_real %in% c("mysql", "mariadb")
  } else {
    identical(motor_real, motor)
  }
  if (!compatible) {
    return(list(
      ok = FALSE, motor = motor, n_consultas = 0L,
      motivo = paste0(
        "la conexi\u00f3n no corresponde al motor `", motor,
        "` (detectado `", motor_real,
        "`): no se pueden verificar sus metadatos para traducir con garant\u00eda"
      )
    ))
  }
  consultar <- function(sql) {
    if (is.null(contador)) {
      .traer_consulta_dbi(conexion, sql)
    } else {
      .consultar_medicion_dbi(conexion, sql, contador, registrar_sql = FALSE)
    }
  }
  if (identical(motor, "postgresql")) {
    sql <- paste0(
      "SELECT current_setting('TimeZone') AS tz, ",
      "current_setting('server_version_num') AS vnum, ",
      "current_setting('server_encoding') AS enc, ",
      "current_setting('extra_float_digits') AS efd, ",
      "lower('I') AS li"
    )
    info <- tryCatch(
      consultar(sql),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
    if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
    if (inherits(info, "error") || !is.data.frame(info) || !nrow(info)) {
      return(list(
        ok = FALSE, motor = motor, n_consultas = 1L,
        motivo = paste0(
          "no se pudieron consultar los metadatos de sesi\u00f3n de `",
          motor, "`"
        )
      ))
    }
    tz <- trimws(as.character(info$tz[[1L]]))
    tz_u <- .mayusculas_ascii(tz)
    enc <- .mayusculas_ascii(trimws(as.character(info$enc[[1L]])))
    vnum <- suppressWarnings(as.integer(info$vnum[[1L]]))
    efd <- suppressWarnings(as.integer(info$efd[[1L]]))
    float8_exacto <- !is.na(vnum) && !is.na(efd) &&
      ((vnum >= 120000L && efd >= 1L) || (vnum < 120000L && efd >= 3L))
    return(list(
      ok = TRUE,
      motor = motor,
      n_consultas = 1L,
      tz = tz,
      es_utc = tz_u %in% c(
        "UTC", "ETC/UTC", "GMT", "ETC/GMT", "Z", "+00:00", "00:00",
        "UTC0", "UCT", "UNIVERSAL"
      ),
      version_num = vnum,
      enc = enc,
      es_utf8 = identical(enc, "UTF8"),
      efd = efd,
      float8_exacto = float8_exacto,
      lower_i_es_i = identical(as.character(info$li[[1L]]), "i")
    ))
  }
  if (motor %in% c("mysql", "mariadb")) {
    sql <- paste0(
      "SELECT @@session.time_zone AS stz, ",
      "@@system_time_zone AS systz, ",
      "@@sql_mode AS sql_mode, ",
      "LOWER('I') AS li, ",
      "VERSION() AS ver"
    )
    info <- tryCatch(
      consultar(sql),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
    if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
    if (inherits(info, "error") || !is.data.frame(info) || !nrow(info)) {
      return(list(
        ok = FALSE, motor = motor, n_consultas = 1L,
        motivo = paste0(
          "no se pudieron consultar los metadatos de sesi\u00f3n de `",
          motor, "`"
        )
      ))
    }
    stz <- trimws(as.character(info$stz[[1L]]))
    systz <- trimws(as.character(info$systz[[1L]]))
    stz_u <- .mayusculas_ascii(stz)
    systz_u <- .mayusculas_ascii(systz)
    utc_set <- c("+00:00", "00:00", "UTC", "ETC/UTC", "GMT", "Z")
    es_utc <- (stz_u %in% utc_set) ||
      (identical(stz_u, "SYSTEM") && systz_u %in% utc_set)
    sql_mode <- .mayusculas_ascii(as.character(info$sql_mode[[1L]]))
    return(list(
      ok = TRUE,
      motor = motor,
      n_consultas = 1L,
      tz = if (identical(stz_u, "SYSTEM")) paste0("SYSTEM(", systz, ")") else stz,
      es_utc = es_utc,
      pad_char_full = grepl(
        "PAD_CHAR_TO_FULL_LENGTH", sql_mode, fixed = TRUE, useBytes = TRUE
      ),
      lower_i_es_i = identical(as.character(info$li[[1L]]), "i"),
      es_rmysql = inherits(conexion, "MySQLConnection"),
      es_mariadb = grepl("mariadb", .minusculas_ascii(as.character(info$ver[[1L]])),
                         fixed = TRUE, useBytes = TRUE)
    ))
  }
  # sqlserver
  sql <- paste0(
    "SELECT CAST(SERVERPROPERTY('ProductMajorVersion') AS VARCHAR(20)) AS maj, ",
    "CAST(DATABASEPROPERTYEX(DB_NAME(), 'Collation') AS VARCHAR(128)) AS db_coll, ",
    "LOWER('I') AS li"
  )
  info <- tryCatch(
    consultar(sql),
    lupa_presupuesto_medicion_dbi = function(e) e,
    error = function(e) e
  )
  if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
  if (inherits(info, "error") || !is.data.frame(info) || !nrow(info)) {
    return(list(
      ok = FALSE, motor = motor, n_consultas = 1L,
      motivo = paste0(
        "no se pudieron consultar los metadatos de sesi\u00f3n de `",
        motor, "`"
      )
    ))
  }
  list(
    ok = TRUE,
    motor = motor,
    n_consultas = 1L,
    version_mayor = suppressWarnings(as.integer(info$maj[[1L]])),
    db_coll = as.character(info$db_coll[[1L]]),
    lower_i_es_i = identical(as.character(info$li[[1L]]), "i")
  )
}

# La identidad de una tabla resuelta por el motor, con cada parte citada por la
# conexion: unida con un punto suelto, `main."a.b"` y `"main.a".b` eran la
# misma. `base` es el nombre con que la conexion llama a la base de la tabla
# -el esquema de SQLite, el catalogo de DuckDB- y `esquema`, el esquema dentro
# de esa base. Si la base es un archivo, lo que la identifica es el archivo y
# no ese nombre: la identidad lleva la ruta normalizada, como literal, en lugar
# del nombre. Asi otra base adjunta con el mismo alias -un `DETACH` y otro
# `ATTACH` entre dos llamadas- es otra tabla, y el mismo archivo con dos alias
# es una sola. En memoria no hay archivo y queda el nombre. No es
# `.identificadores_tabla()`, que arma el identificador de una coleccion y lo
# leen el sello y la reanudacion.
.identidad_tabla_dbi <- function(conexion, tabla, esquema = NA_character_,
                                 base = NA_character_, archivo = NA_character_) {
  if (!.es_texto_escalar(tabla)) return(NA_character_)
  tryCatch({
    archivo <- as.character(archivo)
    con_archivo <- length(archivo) == 1L && .es_texto_escalar(archivo)
    partes <- c(
      catalog = if (con_archivo) NA_character_ else base,
      schema = esquema, table = tabla
    )
    nombre <- as.character(DBI::dbQuoteIdentifier(
      conexion, do.call(DBI::Id, as.list(partes[!is.na(partes)]))
    ))
    if (con_archivo) {
      # La misma normalizacion que la reanudacion (`.identidad_fuente_reanudacion()`).
      ruta <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
      nombre <- paste0(as.character(DBI::dbQuoteString(conexion, ruta)), ".", nombre)
    }
    nombre
  }, error = function(e) NA_character_)
}

.esquema_medicion_dbi <- function(conexion, ref_tabla, motor, contador = NULL,
                                  sesion = NULL) {
  consultas <- character()
  consultar <- function(sql) {
    consultas <<- c(consultas, as.character(sql))
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
  catalogo_cat <- if (inherits(ref_tabla, "Id") &&
                      "catalog" %in% names(ref_tabla@name)) {
    as.character(ref_tabla@name[["catalog"]])
  } else {
    NA_character_
  }
  # La tabla que resuelve el motor: el catalogo, el esquema y el nombre con que
  # la guarda su catalogo -no los que escribio el usuario-, buscada en el orden
  # en que el motor busca un nombre sin esquema y con su comparacion de
  # mayusculas. Es la clave de sus lecturas (`.lectura()`): dos rotulos de la
  # misma tabla son una tabla. Cada parte va citada (`.identidad_tabla_dbi()`):
  # `main."a.b"` y `"main.a".b` son dos tablas. Donde no se resuelve, `NA`, y el
  # control de sus cifras es `desconocido`.
  identidad <- NA_character_
  proto <- consultar(paste0("SELECT * FROM ", q, " WHERE 1 = 0"))
  n_consultas <- 1L
  campos <- names(proto)
  clases <- vapply(proto, .clase_r_columna_dbi, character(1L))
  tipos <- stats::setNames(rep(NA_character_, length(campos)), campos)
  tipos_base <- stats::setNames(rep(NA_character_, length(campos)), campos)
  cotejamientos <- stats::setNames(rep(NA_character_, length(campos)), campos)
  colacion_determinista <- stats::setNames(rep(NA, length(campos)), campos)
  colacion_binaria <- stats::setNames(rep(NA, length(campos)), campos)
  colacion_no_pad <- stats::setNames(rep(NA, length(campos)), campos)
  colacion_bin2 <- stats::setNames(rep(NA, length(campos)), campos)
  crear <- NA_character_
  if (identical(motor, "sqlite")) {
    # En SQLite el esquema va delante del PRAGMA y del catalogo:
    # `PRAGMA table_info("main"."t")` es un error de sintaxis, y un referencial
    # `DBI::Id(schema = "main", ...)` quedaba sin medir con la tabla presente.
    cita_esquema <- if (!is.na(esquema_cat)) {
      paste0(as.character(DBI::dbQuoteIdentifier(conexion, esquema_cat)), ".")
    } else ""
    info <- consultar(if (nzchar(cita_esquema)) {
      paste0("PRAGMA ", cita_esquema, "table_info(",
             as.character(DBI::dbQuoteIdentifier(conexion, nombre_cat)), ")")
    } else {
      paste0("PRAGMA table_info(", q, ")")
    })
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      tipos[info$name] <- .mayusculas_ascii(info$type)
    }
    # Sin esquema, SQLite busca en `temp`, en `main` y en las bases adjuntas en
    # su orden (`PRAGMA database_list`), sin distinguir mayusculas: el catalogo
    # dice cual es la tabla y como se llaman ella y su esquema. Las adjuntas se
    # miran solo si no esta en las dos primeras, que es lo comun.
    filtro_cat <- paste0(
      " WHERE type IN ('table', 'view') AND name = ",
      DBI::dbQuoteString(conexion, nombre_cat), " COLLATE NOCASE"
    )
    # El archivo de cada base sale de `pragma_database_list`, en la misma consulta.
    desde <- function(esquema, orden, nombre_esquema) {
      paste0(
        "SELECT ", orden, " AS lupa_orden, ", nombre_esquema, " AS lupa_esquema, ",
        "(SELECT file FROM pragma_database_list WHERE name = ",
        DBI::dbQuoteString(conexion, esquema), " COLLATE NOCASE) AS lupa_archivo, ",
        "name, sql FROM ", as.character(DBI::dbQuoteIdentifier(conexion, esquema)),
        ".sqlite_master", filtro_cat
      )
    }
    crear_df <- consultar(if (nzchar(cita_esquema)) {
      # El esquema con el nombre que le da la base, no el que se escribio.
      desde(esquema_cat, 0L, paste0(
        "COALESCE((SELECT name FROM pragma_database_list WHERE name = ",
        DBI::dbQuoteString(conexion, esquema_cat), " COLLATE NOCASE), ",
        "CASE WHEN ", DBI::dbQuoteString(conexion, esquema_cat),
        " = 'temp' COLLATE NOCASE THEN 'temp' END)"
      ))
    } else {
      paste0(desde("temp", 0L, "'temp'"), " UNION ALL ", desde("main", 1L, "'main'"))
    })
    n_consultas <- n_consultas + 1L
    if (!nzchar(cita_esquema) && (!is.data.frame(crear_df) || !nrow(crear_df))) {
      bases <- consultar("PRAGMA database_list")
      n_consultas <- n_consultas + 1L
      adjuntas <- if (is.data.frame(bases) && nrow(bases)) {
        bases <- bases[.orden_por_bytes(as.integer(bases$seq)), , drop = FALSE]
        as.character(bases$name[!bases$name %in% c("main", "temp")])
      } else character()
      if (length(adjuntas)) {
        crear_df <- consultar(paste(vapply(seq_along(adjuntas), function(i) {
          desde(adjuntas[[i]], i + 1L, DBI::dbQuoteString(conexion, adjuntas[[i]]))
        }, character(1L)), collapse = " UNION ALL "))
        n_consultas <- n_consultas + 1L
      }
    }
    if (is.data.frame(crear_df) && nrow(crear_df)) {
      crear_df <- crear_df[crear_df$lupa_orden == min(crear_df$lupa_orden), ,
                           drop = FALSE]
      # `COLLATE NOCASE` pliega como SQLite: en un esquema casa una sola tabla.
      fila <- crear_df[1L, , drop = FALSE]
      crear <- fila$sql[[1L]]
      # Sin el nombre del esquema que da la base, la tabla no esta resuelta.
      if (.es_texto_escalar(fila$lupa_esquema[[1L]])) {
        identidad <- .identidad_tabla_dbi(
          conexion, base = fila$lupa_esquema[[1L]], tabla = fila$name[[1L]],
          archivo = fila$lupa_archivo[[1L]]
        )
      }
    }
    cotejamientos <- .cotejamientos_sql_tabla(crear, campos, "binary")
  } else if (identical(motor, "duckdb")) {
    # DuckDB busca un nombre sin esquema primero en `temp` y despues en los
    # esquemas de `search_path` (`current_schemas(false)`) de la base de la
    # sesion; con esquema y sin catalogo, `temp` antes que la base de la sesion
    # (`main.t` lee la temporal), y si no hay tal esquema, una base con ese
    # nombre. Tabla, esquema y catalogo se comparan como el motor, que no
    # distingue mayusculas solo en ASCII, y se toman con el nombre que les da el
    # catalogo. Con `lower()`, que pliega todo Unicode, dos tablas que difieren
    # en la caja de una letra no ASCII eran una, y un nombre se leia de una y se
    # registraba como la otra.
    lit <- function(x) as.character(DBI::dbQuoteString(conexion, x))
    igual <- function(columna, valor) {
      paste0(
        "translate(", columna, ", '", .MAYUSCULAS_ASCII, "', '",
        .MINUSCULAS_ASCII, "') = ", lit(.minusculas_ascii(valor))
      )
    }
    # Sin `search_path`, `current_schemas(false)` es una lista vacia y el
    # esquema es el de la sesion.
    ruta <- paste0(
      "(CASE WHEN len(current_schemas(false)) > 0 THEN current_schemas(false) ",
      "ELSE [current_schema()] END)"
    )
    filtro_duck <- if (!is.na(catalogo_cat)) {
      paste0(
        igual("table_catalog", catalogo_cat),
        if (!is.na(esquema_cat)) {
          paste0(" AND ", igual("table_schema", esquema_cat))
        } else " AND table_schema = 'main'"
      )
    } else if (!is.na(esquema_cat)) {
      paste0(
        "((table_catalog IN ('temp', current_database()) AND ",
        igual("table_schema", esquema_cat), ") OR (",
        igual("table_catalog", esquema_cat), " AND table_schema = 'main'))"
      )
    } else {
      paste0(
        "(table_catalog = 'temp' OR (table_catalog = current_database() AND ",
        "list_contains(", ruta, ", table_schema)))"
      )
    }
    orden_duck <- if (!is.na(catalogo_cat)) "0" else if (!is.na(esquema_cat)) {
      paste0(
        "CASE WHEN table_catalog = 'temp' THEN 0 WHEN table_catalog = ",
        "current_database() AND ", igual("table_schema", esquema_cat),
        " THEN 1 ELSE 2 END"
      )
    } else {
      paste0(
        "CASE WHEN table_catalog = 'temp' THEN 0 ELSE ",
        "list_position(", ruta, ", table_schema) END"
      )
    }
    info <- consultar(paste0(
      "SELECT table_catalog, table_schema, table_name, column_name, data_type, ",
      orden_duck, " AS lupa_orden, (SELECT lupa_b.path FROM duckdb_databases() ",
      "lupa_b WHERE lupa_b.database_name = table_catalog) AS lupa_archivo ",
      "FROM information_schema.columns WHERE ", igual("table_name", nombre_cat),
      " AND ", filtro_duck
    ))
    n_consultas <- n_consultas + 1L
    nombre_guardado <- nombre_cat
    esquema_guardado <- esquema_cat
    catalogo_guardado <- catalogo_cat
    if (is.data.frame(info) && nrow(info)) {
      info$lupa_orden <- suppressWarnings(as.numeric(info$lupa_orden))
      info <- info[!is.na(info$lupa_orden), , drop = FALSE]
    }
    if (is.data.frame(info) && nrow(info)) {
      info <- info[info$lupa_orden == min(info$lupa_orden), , drop = FALSE]
      fila <- info[1L, , drop = FALSE]
      info <- info[info$table_catalog == fila$table_catalog[[1L]] &
                     info$table_schema == fila$table_schema[[1L]] &
                     info$table_name == fila$table_name[[1L]], , drop = FALSE]
      tipos[info$column_name] <- .mayusculas_ascii(info$data_type)
      nombre_guardado <- as.character(fila$table_name[[1L]])
      esquema_guardado <- as.character(fila$table_schema[[1L]])
      catalogo_guardado <- as.character(fila$table_catalog[[1L]])
      identidad <- .identidad_tabla_dbi(
        conexion, base = catalogo_guardado, esquema = esquema_guardado,
        tabla = nombre_guardado, archivo = fila$lupa_archivo[[1L]]
      )
    }
    crear_df <- tryCatch(
      consultar(paste0(
        "SELECT sql FROM duckdb_tables() WHERE table_name = ",
        lit(nombre_guardado),
        if (!is.na(esquema_guardado)) {
          paste0(" AND schema_name = ", lit(esquema_guardado))
        } else "",
        if (!is.na(catalogo_guardado)) {
          paste0(" AND database_name = ", lit(catalogo_guardado))
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
  } else if (identical(motor, "postgresql") && isTRUE(sesion$ok)) {
    tiene_det <- !is.null(sesion$version_num) &&
      !is.na(sesion$version_num) && sesion$version_num >= 120000L
    filtro_rel <- if (!is.na(esquema_cat)) {
      paste0(
        "n.nspname = ", DBI::dbQuoteString(conexion, esquema_cat),
        " AND c.relname = ", DBI::dbQuoteString(conexion, nombre_cat)
      )
    } else {
      paste0(
        "c.relname = ", DBI::dbQuoteString(conexion, nombre_cat),
        " AND pg_catalog.pg_table_is_visible(c.oid)"
      )
    }
    sql_pg <- paste0(
      "SELECT a.attname AS column_name, t.typname AS typname, ",
      "pg_catalog.format_type(a.atttypid, a.atttypmod) AS formatted_type, ",
      "co.collname AS collation_name, ",
      if (tiene_det) "co.collisdeterministic" else "TRUE",
      " AS coll_det ",
      "FROM pg_catalog.pg_attribute a ",
      "JOIN pg_catalog.pg_class c ON c.oid = a.attrelid ",
      "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ",
      "JOIN pg_catalog.pg_type t ON t.oid = a.atttypid ",
      "LEFT JOIN pg_catalog.pg_collation co ON co.oid = a.attcollation ",
      "WHERE ", filtro_rel,
      " AND a.attnum > 0 AND NOT a.attisdropped ORDER BY a.attnum"
    )
    info <- consultar(sql_pg)
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      for (i in seq_len(nrow(info))) {
        col_i <- as.character(info$column_name[[i]])
        if (!col_i %in% campos) next
        tp_base <- .minusculas_ascii(as.character(info$typname[[i]]))
        tipos_base[[col_i]] <- tp_base
        tipos[[col_i]] <- .mayusculas_ascii(as.character(info$formatted_type[[i]]))
        cn <- as.character(info$collation_name[[i]])
        if (!is.na(cn) && nzchar(cn)) cotejamientos[[col_i]] <- cn
        det_val <- info$coll_det[[i]]
        es_det <- if (is.logical(det_val)) {
          isTRUE(det_val)
        } else {
          .minusculas_ascii(as.character(det_val)) %in% c("t", "true", "1")
        }
        colacion_determinista[[col_i]] <- isTRUE(es_det) &&
          !is.na(cn) && nzchar(cn) && !identical(tp_base, "citext")
      }
    }
  } else if (motor %in% c("mysql", "mariadb") && isTRUE(sesion$ok)) {
    filtro_esq <- if (!is.na(esquema_cat)) {
      paste0("c.table_schema = ", DBI::dbQuoteString(conexion, esquema_cat))
    } else {
      "c.table_schema = DATABASE()"
    }
    con_pad_attr <- !isTRUE(sesion$es_mariadb)
    sql_my <- if (con_pad_attr) {
      paste0(
        "SELECT c.column_name, c.data_type, c.column_type, ",
        "c.collation_name, co.pad_attribute ",
        "FROM information_schema.columns c ",
        "LEFT JOIN information_schema.collations co ",
        "ON co.collation_name = c.collation_name ",
        "WHERE ", filtro_esq, " AND c.table_name = ",
        DBI::dbQuoteString(conexion, nombre_cat),
        " ORDER BY c.ordinal_position"
      )
    } else {
      paste0(
        "SELECT c.column_name, c.data_type, c.column_type, ",
        "c.collation_name ",
        "FROM information_schema.columns c ",
        "WHERE ", filtro_esq, " AND c.table_name = ",
        DBI::dbQuoteString(conexion, nombre_cat),
        " ORDER BY c.ordinal_position"
      )
    }
    info <- tryCatch(
      consultar(sql_my),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) {
        if (!con_pad_attr) .detener(e)
        consultar(paste0(
          "SELECT c.column_name, c.data_type, c.column_type, ",
          "c.collation_name ",
          "FROM information_schema.columns c ",
          "WHERE ", filtro_esq, " AND c.table_name = ",
          DBI::dbQuoteString(conexion, nombre_cat),
          " ORDER BY c.ordinal_position"
        ))
      }
    )
    if (inherits(info, "lupa_presupuesto_medicion_dbi")) .detener(info)
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      names(info) <- .minusculas_ascii(names(info))
      tiene_pad <- "pad_attribute" %in% names(info)
      for (i in seq_len(nrow(info))) {
        col_i <- as.character(info$column_name[[i]])
        idx <- match(.minusculas_ascii(col_i), .minusculas_ascii(campos))
        if (is.na(idx)) next
        col_real <- campos[[idx]]
        tp_base <- .minusculas_ascii(as.character(info$data_type[[i]]))
        tipos_base[[col_real]] <- tp_base
        tipos[[col_real]] <- .mayusculas_ascii(as.character(info$column_type[[i]]))
        cn <- as.character(info$collation_name[[i]])
        if (!is.na(cn) && nzchar(cn)) {
          cotejamientos[[col_real]] <- cn
          cn_l <- .minusculas_ascii(cn)
          colacion_binaria[[col_real]] <- grepl(
            "(_bin|binary)$", cn_l, perl = TRUE, useBytes = TRUE
          )
          if (tiene_pad && !is.na(info$pad_attribute[[i]])) {
            colacion_no_pad[[col_real]] <- identical(
              .mayusculas_ascii(trimws(as.character(info$pad_attribute[[i]]))),
              "NO PAD"
            )
          } else {
            colacion_no_pad[[col_real]] <- grepl(
              "_nopad_", cn_l, fixed = TRUE, useBytes = TRUE
            ) || identical(cn_l, "binary")
          }
        }
      }
    }
  } else if (identical(motor, "sqlserver") && isTRUE(sesion$ok)) {
    sql_ss <- if (!is.na(esquema_cat)) {
      paste0(
        "SELECT c.name AS column_name, t.name AS data_type, ",
        "c.collation_name, c.precision, c.scale ",
        "FROM sys.columns c ",
        "JOIN sys.objects o ON o.object_id = c.object_id ",
        "JOIN sys.schemas s ON s.schema_id = o.schema_id ",
        "JOIN sys.types t ON t.user_type_id = c.user_type_id ",
        "WHERE s.name = ", DBI::dbQuoteString(conexion, esquema_cat),
        " AND o.name = ", DBI::dbQuoteString(conexion, nombre_cat),
        " ORDER BY c.column_id"
      )
    } else {
      paste0(
        "SELECT c.name AS column_name, t.name AS data_type, ",
        "c.collation_name, c.precision, c.scale ",
        "FROM sys.columns c ",
        "JOIN sys.types t ON t.user_type_id = c.user_type_id ",
        "WHERE c.object_id = OBJECT_ID(",
        DBI::dbQuoteString(conexion, nombre_cat),
        ") ORDER BY c.column_id"
      )
    }
    info <- consultar(sql_ss)
    n_consultas <- n_consultas + 1L
    if (is.data.frame(info) && nrow(info)) {
      names(info) <- .minusculas_ascii(names(info))
      for (i in seq_len(nrow(info))) {
        col_i <- as.character(info$column_name[[i]])
        idx <- match(.minusculas_ascii(col_i), .minusculas_ascii(campos))
        if (is.na(idx)) next
        col_real <- campos[[idx]]
        tp_base <- .minusculas_ascii(as.character(info$data_type[[i]]))
        tipos_base[[col_real]] <- tp_base
        tipos[[col_real]] <- if (tp_base %in% c("decimal", "numeric")) {
          sprintf("%s(%d,%d)", .mayusculas_ascii(tp_base),
                  as.integer(info$precision[[i]]), as.integer(info$scale[[i]]))
        } else {
          .mayusculas_ascii(tp_base)
        }
        cn <- as.character(info$collation_name[[i]])
        if (!is.na(cn) && nzchar(cn)) {
          cotejamientos[[col_real]] <- cn
          colacion_bin2[[col_real]] <- grepl(
            "_bin2(_utf8)?$", .minusculas_ascii(cn), perl = TRUE, useBytes = TRUE
          )
        }
      }
    }
  }
  cotejamientos[is.na(cotejamientos)] <- "desconocido"
  list(
    referencia = ref_tabla,
    nombre = nombre_cat,
    sql = q,
    campos = campos,
    clases = clases,
    tipos = tipos,
    tipos_base = tipos_base,
    cotejamientos = cotejamientos,
    colacion_determinista = colacion_determinista,
    colacion_binaria = colacion_binaria,
    colacion_no_pad = colacion_no_pad,
    colacion_bin2 = colacion_bin2,
    sesion = sesion,
    n_campos = length(campos),
    prototipo = .normalizar_columnas_texto(proto),
    n_consultas_esquema = n_consultas,
    consultas_esquema = consultas,
    identidad = identidad
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
    postgresql = isTRUE(esq$colacion_determinista[[columna]]),
    mysql = , mariadb = isTRUE(esq$colacion_binaria[[columna]]),
    sqlserver = isTRUE(esq$colacion_bin2[[columna]]),
    FALSE
  )
}

.requiere_guarda_espacios_dbi <- function(esq, columna, motor) {
  if (!identical(esq$clases[[columna]], "texto")) return(FALSE)
  if (motor %in% c("mysql", "mariadb")) {
    return(isTRUE(esq$colacion_binaria[[columna]]) &&
             !isTRUE(esq$colacion_no_pad[[columna]]))
  }
  if (identical(motor, "sqlserver")) {
    return(isTRUE(esq$colacion_bin2[[columna]]))
  }
  FALSE
}

.es_tinyint_logico_dbi <- function(esq, columna, motor) {
  if (!motor %in% c("mysql", "mariadb") ||
      !identical(esq$clases[[columna]], "entero")) {
    return(FALSE)
  }
  tipo <- if (!is.null(esq$tipos[[columna]])) esq$tipos[[columna]] else NA_character_
  tipo <- gsub("[[:space:]]+", "", .minusculas_ascii(as.character(tipo)))
  grepl("^tinyint\\(1\\)$", tipo, perl = TRUE, useBytes = TRUE)
}

.logico_entero_compatible_dbi <- function(esq, columna, clase_col,
                                          clase_lit, motor) {
  identical(clase_col, "entero") && identical(clase_lit, "logico") &&
    !is.null(columna) && .es_tinyint_logico_dbi(esq, columna, motor)
}

.columna_como_epoca_dbi <- function(esq, columna, motor) {
  tipo <- if (!is.null(esq$tipos_base)) esq$tipos_base[[columna]] else NA_character_
  (identical(motor, "postgresql") && identical(tipo, "timestamptz") &&
     !isTRUE(esq$sesion$es_utc)) ||
    (identical(motor, "sqlserver") && identical(tipo, "datetimeoffset"))
}

.sql_guarda_espacios_dbi <- function(expr, motor) {
  switch(
    motor,
    mysql = , mariadb = paste0(
      "(CHAR_LENGTH(", expr, ") <> CHAR_LENGTH(TRIM(TRAILING ' ' FROM ", expr, ")))"
    ),
    sqlserver = paste0("(DATALENGTH(", expr, ") <> DATALENGTH(RTRIM(", expr, ")))"),
    .no_traducible_dbi("la guarda de espacios finales en ", motor)
  )
}

.motivo_guarda_dbi <- function(motor, colacion = NULL, sujeto = "la columna") {
  if (identical(motor, "sqlite")) {
    return(paste0(
      "SQLite no valida UTF-8 y ", sujeto, " tiene texto no ASCII: no se puede ",
      "garantizar que el motor lea los mismos valores que R"
    ))
  }
  if (motor %in% c("mysql", "mariadb")) {
    det <- if (!is.null(colacion) && length(colacion) == 1L &&
               !is.na(colacion) && nzchar(colacion)) {
      paste0(" (`", colacion, "`)")
    } else ""
    return(paste0(
      "la columna de texto tiene espacios finales y el cotejamiento del motor",
      det, " es PAD SPACE: no se puede garantizar la igualdad exacta con R"
    ))
  }
  if (identical(motor, "sqlserver")) {
    return(paste(
      "la columna de texto tiene espacios finales y SQL Server los ignora al",
      "comparar o agrupar: no se puede garantizar la igualdad exacta con R"
    ))
  }
  "el motor no garantiza la equivalencia con R sobre estos valores"
}

.verificar_literal_espacios_dbi <- function(valores, esq, columna, motor) {
  if (!.requiere_guarda_espacios_dbi(esq, columna, motor)) return(invisible(NULL))
  vals <- as.character(valores)
  vals <- vals[!is.na(vals)]
  if (any(grepl(" $", vals, useBytes = TRUE))) {
    .no_traducible_dbi(
      "un valor de texto termina en espacio y ", motor,
      " ignora espacios finales al comparar (cotejamiento `",
      esq$cotejamientos[[columna]], "`)"
    )
  }
  invisible(NULL)
}

.validar_columna_base_dbi <- function(esq, columna, motor, requiere_valor = TRUE) {
  tp <- if (!is.null(esq$tipos_base)) esq$tipos_base[[columna]] else NA_character_
  if (is.na(tp) || !nzchar(tp)) return(invisible(NULL))
  if (motor %in% c("mysql", "mariadb")) {
    if (tp %in% c("binary", "varbinary", "tinyblob", "blob",
                  "mediumblob", "longblob", "json", "geometry")) {
      .no_traducible_dbi(
        "la columna `", columna, "` tiene tipo `", tp,
        "` no escalar para traducci\u00f3n SQL en ", motor
      )
    }
    if (identical(tp, "bit") && isTRUE(esq$sesion$es_rmysql) &&
        isTRUE(requiere_valor)) {
      .no_traducible_dbi(
        "la columna `", columna, "` es `BIT` y RMySQL no la lee con fidelidad"
      )
    }
  }
  if (identical(motor, "sqlserver")) {
    if (tp %in% c("text", "ntext", "image", "xml", "geography", "geometry",
                  "hierarchyid", "sql_variant", "binary", "varbinary")) {
      .no_traducible_dbi(
        "la columna `", columna, "` tiene tipo `", tp,
        "` no admitido en consultas de calidad en SQL Server"
      )
    }
  }
  invisible(NULL)
}

.validar_columna_comparacion_dbi <- function(esq, columna, motor,
                                             requiere_no_pad = FALSE) {
  .validar_columna_base_dbi(esq, columna, motor)
  clase <- esq$clases[[columna]]
  tp <- if (!is.null(esq$tipos_base)) esq$tipos_base[[columna]] else NA_character_
  sesion <- esq$sesion
  # Un entero sin signo que el controlador entrega como entero de 32 bits con
  # signo: los valores mayores que 2^31 - 1 llegan a R cambiados (4294967295
  # llega como -1), y el motor compararia el valor verdadero. No se reproduce un
  # desborde: la comparacion queda sin traducir. Contar presentes no depende del
  # valor y sigue midiendose.
  tipo_completo <- if (!is.null(esq$tipos)) esq$tipos[[columna]] else NA_character_
  if (motor %in% c("mysql", "mariadb") && identical(clase, "entero") &&
      length(tipo_completo) == 1L && !is.na(tipo_completo) &&
      grepl("UNSIGNED", tipo_completo, fixed = TRUE, useBytes = TRUE)) {
    .no_traducible_dbi(
      "la columna `", columna, "` es un entero sin signo (`", tipo_completo,
      "`) y el controlador la lee como entero de 32 bits con signo: los ",
      "valores mayores que 2^31 - 1 llegan a R cambiados"
    )
  }
  if (identical(clase, "texto") && !is.na(tp) &&
      .tipo_declarado_temporal_dbi(tp)) {
    .no_traducible_dbi(
      "el controlador lee la columna temporal `", columna, "` (`", tp,
      "`) como texto; el motor no compara texto sobre un tipo temporal como R"
    )
  }
  if (motor %in% c("mysql", "mariadb") && identical(tp, "timestamp") &&
      !isTRUE(sesion$es_utc)) {
    .no_traducible_dbi(
      "la columna `", columna, "` es `TIMESTAMP` y la sesi\u00f3n de ", motor,
      " no est\u00e1 en UTC (time_zone = `", sesion$tz %||% "desconocido", "`)"
    )
  }
  if (identical(clase, "texto")) {
    if (identical(motor, "postgresql") && identical(tp, "bpchar")) {
      .no_traducible_dbi(
        "la columna `", columna, "` es `char(n)` (`bpchar`): PostgreSQL ignora ",
        "sus espacios finales al comparar pero R los lee con relleno"
      )
    }
    if (identical(motor, "sqlserver") && tp %in% c("char", "nchar")) {
      .no_traducible_dbi(
        "la columna `", columna, "` es `", tp, "(n)`: SQL Server ignora sus ",
        "espacios finales al comparar pero R los lee con relleno"
      )
    }
    if (motor %in% c("mysql", "mariadb") && identical(tp, "char") &&
        isTRUE(sesion$pad_char_full)) {
      .no_traducible_dbi(
        "la columna `", columna, "` es `CHAR(n)` con `PAD_CHAR_TO_FULL_LENGTH` ",
        "activo: R lee relleno que el motor ignora"
      )
    }
    # Un cotejamiento que el catalogo no publica -la columna de una vista- no
    # cambia la cuenta donde cada comparacion lleva la intercalacion binaria
    # explicita (`.sql_texto_binario_dbi()`); uno declarado distinto del binario
    # sigue declinando.
    desconocido <- identical(esq$cotejamientos[[columna]], "desconocido") &&
      motor %in% .MOTORES_TEXTO_BINARIO_DBI
    if (!desconocido && !.cotejamiento_binario_dbi(esq, columna, motor)) {
      .no_traducible_dbi(
        "la columna `", columna, "` no compara por bytes (cotejamiento `",
        esq$cotejamientos[[columna]], "`)"
      )
    }
    if (isTRUE(requiere_no_pad) && motor %in% c("mysql", "mariadb") &&
        !isTRUE(esq$colacion_no_pad[[columna]])) {
      .no_traducible_dbi(
        "la columna `", columna, "` usa cotejamiento `",
        esq$cotejamientos[[columna]], "` (PAD SPACE); se requiere NO PAD"
      )
    }
  }
  invisible(NULL)
}

# Como compara cada motor el texto por los bytes de su UTF-8, que es el orden
# por punto de codigo que usa `medir(orden_texto = "bytes")`. Cada traduccion
# envuelve los DOS lados de la comparacion -la columna y el literal, o las dos
# columnas-.
#
#   - SQLite: `COLLATE BINARY` es `memcmp()` sobre lo que la base guarda; en una
#     base UTF-8 son los bytes del UTF-8 (la codificacion se pregunta en la
#     sesion). Y el `COLLATE` explicito gana sobre el que declare la columna.
#   - DuckDB: `encode()` da los bytes del UTF-8 como BLOB, y un BLOB se compara
#     byte a byte, con el prefijo antes. `CAST(... AS BLOB)` no sirve: rechaza
#     todo caracter que no sea ASCII. Y no depende de la intercalacion que
#     declare la columna ni de `default_collation`.
#   - PostgreSQL: `COLLATE "C"` compara los bytes de la codificacion del
#     servidor, que la sesion exige UTF8.
#   - MySQL y MariaDB: el texto pasado a UTF-8 y a binario se compara byte a
#     byte y sin el relleno de espacios de las intercalaciones PAD SPACE.
#   - SQL Server: `_BIN2` sobre `nvarchar` compara unidades de UTF-16, que no
#     siguen el orden del punto de codigo por encima de U+FFFF; por eso se pasa a
#     `varchar` con una intercalacion UTF-8 (SQL Server 2019 en adelante).
#
# `verificado` dice si la traduccion se comparo con la medicion en memoria: las
# que no, quedan escritas y el plan las declara `no_medible` con ese motivo
# hasta que se las compare en su motor.
.ORDEN_BYTES_MOTORES_DBI <- list(
  sqlite = list(plantilla = "(%s COLLATE BINARY)",
                descripcion = "COLLATE BINARY", verificado = TRUE),
  duckdb = list(plantilla = "encode(%s)",
                descripcion = "encode()", verificado = TRUE),
  postgresql = list(plantilla = "(%s COLLATE \"C\")",
                    descripcion = "COLLATE \"C\"", verificado = FALSE),
  mysql = list(plantilla = "CAST(CONVERT(%s USING utf8mb4) AS BINARY)",
               descripcion = "CAST(CONVERT(... USING utf8mb4) AS BINARY)",
               verificado = FALSE),
  mariadb = list(plantilla = "CAST(CONVERT(%s USING utf8mb4) AS BINARY)",
                 descripcion = "CAST(CONVERT(... USING utf8mb4) AS BINARY)",
                 verificado = FALSE),
  sqlserver = list(
    plantilla = paste0(
      "(CAST(%s COLLATE Latin1_General_100_BIN2_UTF8 AS VARCHAR(MAX)) ",
      "COLLATE Latin1_General_100_BIN2_UTF8)"
    ),
    descripcion = "Latin1_General_100_BIN2_UTF8 sobre varchar",
    verificado = FALSE
  )
)

.sql_texto_por_bytes_dbi <- function(expr, motor) {
  sprintf(.ORDEN_BYTES_MOTORES_DBI[[motor]]$plantilla, expr)
}

# `NULL` si el motor compara texto por bytes con una traduccion verificada; si
# no, el motivo, que es lo que el plan publica.
.motivo_orden_bytes_dbi <- function(motor, esq) {
  regla <- .ORDEN_BYTES_MOTORES_DBI[[motor]]
  if (is.null(regla)) {
    return(paste0(
      "orden por bytes sobre texto: no hay traducci\u00f3n para el motor `",
      motor, "`"
    ))
  }
  if (!isTRUE(regla$verificado)) {
    return(paste0(
      "orden por bytes sobre texto: en `", motor, "` se traduce con ",
      regla$descripcion, ", pero esa traducci\u00f3n todav\u00eda no se ",
      "compar\u00f3 con la medici\u00f3n en memoria en este motor"
    ))
  }
  if (identical(motor, "sqlite")) {
    codificacion <- esq$sesion$codificacion
    if (!identical(codificacion, "UTF-8")) {
      leida <- !is.null(codificacion) && !is.na(codificacion)
      return(paste0(
        "orden por bytes sobre texto: ",
        if (leida) {
          paste0("la base SQLite guarda el texto en `", codificacion, "`")
        } else {
          "no se pudo leer en qu\u00e9 codificaci\u00f3n guarda el texto la base SQLite"
        },
        ", y COLLATE BINARY compara esos bytes, que solo en UTF-8 siguen el ",
        "orden del punto de c\u00f3digo"
      ))
    }
  }
  NULL
}

.citar_col_dbi <- function(conexion, nombre) {
  as.character(DBI::dbQuoteIdentifier(conexion, nombre))
}

.tipo_doble_sql_dbi <- function(motor) {
  switch(
    motor,
    postgresql = "DOUBLE PRECISION",
    sqlserver = "FLOAT",
    "DOUBLE"
  )
}

.columna_sql_medicion <- function(conexion, esq, columna, motor, alias = NULL,
                                  normalizar_nan = TRUE) {
  ref <- .citar_col_dbi(conexion, columna)
  if (!is.null(alias)) ref <- paste0(alias, ".", ref)
  tipo <- esq$tipos[[columna]]
  tipo_base <- if (!is.null(esq$tipos_base)) esq$tipos_base[[columna]] else NA_character_
  lectura_controlador <- FALSE
  if (identical(motor, "postgresql") && identical(tipo_base, "float4")) {
    ref <- .lectura_real_postgresql_dbi(ref, version = esq$sesion$version_num)
    lectura_controlador <- TRUE
  } else if (identical(motor, "postgresql") && identical(tipo_base, "float8") &&
             !isTRUE(esq$sesion$float8_exacto)) {
    ref <- paste0("CAST(CAST(", ref, " AS TEXT) AS DOUBLE PRECISION)")
    lectura_controlador <- TRUE
  } else if (identical(motor, "mysql") && isTRUE(esq$sesion$es_rmysql) &&
             identical(tipo_base, "float")) {
    ref <- paste0("CAST(CAST(", ref, " AS CHAR) AS DOUBLE)")
    lectura_controlador <- TRUE
  }
  if (.columna_como_epoca_dbi(esq, columna, motor)) {
    ref <- if (identical(motor, "postgresql")) {
      paste0("EXTRACT(EPOCH FROM ", ref, ")")
    } else {
      paste0(
        "(CAST(DATEDIFF_BIG(microsecond, ",
        "CAST('1970-01-01T00:00:00+00:00' AS DATETIMEOFFSET), ",
        "SWITCHOFFSET(", ref, ", '+00:00')) AS FLOAT) / 1000000.0)"
      )
    }
  }
  doble_declarado <- !is.na(tipo) && grepl(
    "^(DOUBLE|FLOAT|REAL|DECIMAL|NUMERIC|NUMBER|MONEY|SMALLMONEY)(\\(|$| )",
    tipo, useBytes = TRUE
  ) || .tipo_declarado_entero_ancho_dbi(tipo)
  if (identical(esq$clases[[columna]], "doble") && doble_declarado &&
      !isTRUE(lectura_controlador) && !.columna_como_epoca_dbi(esq, columna, motor) &&
      motor %in% c("sqlite", "duckdb", "postgresql", "mysql", "mariadb", "sqlserver")) {
    ref <- paste0("CAST(", ref, " AS ", .tipo_doble_sql_dbi(motor), ")")
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

.literal_sql_medicion <- function(conexion, valor, clase_col, motor,
                                  esq = NULL, columna = NULL) {
  if (length(valor) != 1L) {
    .no_traducible_dbi("se esperaba un valor escalar, no ", length(valor))
  }
  if (is.na(valor) && !is.nan(valor)) return("NULL")
  clase_lit <- .clase_r_columna_dbi(valor)
  compatible_logico <- !is.null(esq) &&
    .logico_entero_compatible_dbi(esq, columna, clase_col, clase_lit, motor)
  if (!.clases_compatibles_sql(clase_col, clase_lit) && !compatible_logico) {
    .no_traducible_dbi(
      "compara una columna `", clase_col, "` con un valor `", clase_lit,
      "`: R convierte uno de los dos y el motor no lo hace igual"
    )
  }
  tp_base <- if (!is.null(esq) && !is.null(columna) && !is.null(esq$tipos_base)) {
    esq$tipos_base[[columna]]
  } else NA_character_
  switch(
    clase_lit,
    entero = as.character(valor),
    doble = {
      if (is.nan(valor)) .no_traducible_dbi("un literal NaN")
      if (is.infinite(valor)) {
        if (identical(motor, "duckdb")) {
          if (valor > 0) "CAST('Infinity' AS DOUBLE)" else "CAST('-Infinity' AS DOUBLE)"
        } else if (identical(motor, "postgresql")) {
          if (valor > 0) {
            "CAST('Infinity' AS DOUBLE PRECISION)"
          } else {
            "CAST('-Infinity' AS DOUBLE PRECISION)"
          }
        } else if (identical(motor, "sqlite")) {
          if (valor > 0) "9e999" else "-9e999"
        } else {
          .no_traducible_dbi("un literal infinito en ", motor)
        }
      } else {
        literal <- formatC(valor, digits = 17, format = "g")
        if (identical(clase_col, "doble")) {
          paste0("CAST(", literal, " AS ", .tipo_doble_sql_dbi(motor), ")")
        } else {
          literal
        }
      }
    },
    texto = {
      texto <- enc2utf8(as.character(valor))
      if (!validUTF8(texto)) .no_traducible_dbi("un texto con bytes inv\u00e1lidos")
      citado <- as.character(DBI::dbQuoteString(conexion, texto))
      if (identical(motor, "sqlserver") &&
          (tp_base %in% c("nvarchar", "nchar") ||
           grepl("[^\\001-\\177]", texto, perl = TRUE, useBytes = TRUE))) {
        paste0("N", citado)
      } else {
        citado
      }
    },
    logico = if (compatible_logico) {
      if (valor) "1" else "0"
    } else {
      switch(
        motor,
        duckdb = , postgresql = if (valor) "TRUE" else "FALSE",
        mariadb = , sqlserver = if (valor) "1" else "0",
        .no_traducible_dbi(
          "un l\u00f3gico en ", motor, ", que lo guarda como entero"
        )
      )
    },
    fecha = switch(
      motor,
      duckdb = , postgresql = , mysql = , mariadb = paste0(
        "DATE '", format(valor, "%Y-%m-%d"), "'"
      ),
      sqlserver = paste0("CAST('", format(valor, "%Y-%m-%d"), "' AS DATE)"),
      sqlite = as.character(as.integer(unclass(valor))),
      .no_traducible_dbi("una fecha en ", motor)
    ),
    instante = {
      seg <- as.numeric(valor)
      if (seg != floor(seg)) {
        .no_traducible_dbi("un instante con fracci\u00f3n de segundo")
      }
      txt_ts <- format(valor, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      if (!is.null(esq) && !is.null(columna) &&
          .columna_como_epoca_dbi(esq, columna, motor)) {
        return(formatC(seg, digits = 17, format = "g"))
      }
      switch(
        motor,
        duckdb = paste0("TIMESTAMP '", txt_ts, "'"),
        postgresql = if (identical(tp_base, "timestamptz")) {
          paste0("TIMESTAMPTZ '", txt_ts, "+00'")
        } else {
          paste0("TIMESTAMP '", txt_ts, "'")
        },
        mysql = , mariadb = paste0("CAST('", txt_ts, "' AS DATETIME)"),
        sqlserver = paste0("CAST('", txt_ts, "' AS DATETIME2)"),
        sqlite = formatC(seg, digits = 17, format = "g"),
        .no_traducible_dbi("un instante en ", motor)
      )
    },
    .no_traducible_dbi("un valor de clase ", class(valor)[[1L]])
  )
}

.sql_es_nulo_predicado <- function(pred, motor) {
  if (identical(motor, "sqlserver")) {
    paste0("(CASE WHEN ", pred, " THEN 1 WHEN NOT (", pred, ") THEN 0 ELSE NULL END IS NULL)")
  } else {
    paste0("(", pred, " IS NULL)")
  }
}

.sql_como_valor_logico <- function(pred, motor) {
  if (identical(motor, "sqlserver")) {
    paste0("(CASE WHEN ", pred, " THEN 1 WHEN NOT (", pred, ") THEN 0 ELSE NULL END)")
  } else {
    pred
  }
}

# Traduce una expresion de formula a SQL sin usar nunca `eval()`: primero
# valida el arbol completo con `.validar_ast_formula()`, evalua las subramas
# constantes con `.evaluar_constante_formula_segura()` y traduce los operadores
# con la semantica de NA de R.
.traducir_predicado_dbi <- function(expr, entorno, visibles, esq, motor,
                                    conexion, max_valores_in = 500L,
                                    alias = NULL, orden_texto = "idioma") {
  # Valida TODO el arbol antes de interpretar cualquier rama constante:
  # `~ t == boom()` o `~ { boom(); t == "x" }` fallan sin ejecutar nada.
  val_ast <- tryCatch(
    .validar_ast_formula(expr, names(visibles), entorno),
    error = function(e) e
  )
  if (inherits(val_ast, "condition")) {
    return(list(sql = NULL, motivo = conditionMessage(val_ast)))
  }
  guardas <- character()
  motivo_guarda <- NULL
  registrar_guarda_col <- function(nombre, sql_col) {
    if (.requiere_guarda_espacios_dbi(esq, nombre, motor)) {
      g <- paste0("(", sql_col, " IS NOT NULL AND ",
                  .sql_guarda_espacios_dbi(sql_col, motor), ")")
      if (!g %in% guardas) guardas <<- c(guardas, g)
      if (is.null(motivo_guarda)) {
        motivo_guarda <<- .motivo_guarda_dbi(motor, esq$cotejamientos[[nombre]])
      }
    }
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
        if (identical(motor, "sqlserver")) "(CAST(NULL AS INT) = 1)" else "CAST(NULL AS BOOLEAN)"
      } else if (v) {
        "(1 = 1)"
      } else {
        "(1 = 0)"
      }
      r$es_predicado <- TRUE
    } else if (!is.null(r$columna) && motor %in% c("sqlserver", "mariadb")) {
      r$sql <- paste0("(", r$sql, " = 1)")
      r$es_predicado <- TRUE
      r$columna <- NULL
    }
    r
  }
  trad <- function(e, para_nulo = FALSE) {
    if (!tiene_columna(e)) {
      v <- tryCatch(
        .evaluar_constante_formula_segura(e, entorno, orden_texto),
        error = function(err) .no_traducible_dbi(conditionMessage(err))
      )
      return(list(valor = v, clase = .clase_r_columna_dbi(v)))
    }
    if (is.symbol(e)) {
      nombre <- as.character(e)
      clase <- visibles[[nombre]]
      .validar_columna_base_dbi(esq, nombre, motor)
      if (!isTRUE(para_nulo)) {
        if (identical(clase, "logico") && identical(motor, "sqlite")) {
          .no_traducible_dbi("SQLite guarda el l\u00f3gico como entero")
        }
        .validar_columna_comparacion_dbi(esq, nombre, motor)
      }
      return(list(
        sql = .columna_sql_medicion(conexion, esq, nombre, motor, alias = alias),
        clase = clase,
        columna = nombre,
        es_predicado = FALSE
      ))
    }
    if (!is.call(e)) .no_traducible_dbi("una expresi\u00f3n que no es llamada")
    f <- as.character(e[[1L]])
    args <- as.list(e)[-1L]
    if (identical(f, "(")) return(trad(args[[1L]], para_nulo = para_nulo))
    if (identical(f, "!")) {
      a <- como_logico(trad(args[[1L]]))
      return(list(sql = paste0("(NOT ", a$sql, ")"), clase = "logico",
                  es_predicado = TRUE))
    }
    if (f %in% c("&", "|")) {
      a <- como_logico(trad(args[[1L]]))
      b <- como_logico(trad(args[[2L]]))
      return(list(
        sql = paste0("(", a$sql, if (f == "&") " AND " else " OR ", b$sql, ")"),
        clase = "logico",
        es_predicado = TRUE
      ))
    }
    if (identical(f, "is.na")) {
      a <- trad(args[[1L]], para_nulo = TRUE)
      if (is.null(a$sql)) .no_traducible_dbi("is.na() de una constante")
      sql_na <- if (isTRUE(a$es_predicado)) {
        .sql_es_nulo_predicado(a$sql, motor)
      } else {
        paste0("(", a$sql, " IS NULL)")
      }
      return(list(sql = sql_na, clase = "logico", es_predicado = TRUE))
    }
    if (f %in% c("==", "!=", "<", "<=", ">", ">=")) {
      a <- trad(args[[1L]])
      b <- trad(args[[2L]])
      if (is.null(a$sql) && is.null(b$sql)) .no_traducible_dbi("dos constantes")
      if (is.null(a$sql)) {
        tmp <- a; a <- b; b <- tmp
        f <- switch(f, "<" = ">", "<=" = ">=", ">" = "<", ">=" = "<=", f)
      }
      # Un orden sobre texto depende de con que intercalacion se compare: la de
      # la sesion de R no la tiene ningun motor, asi que solo se traduce con
      # `orden_texto = "bytes"`, que el motor reproduce por bytes.
      por_bytes <- FALSE
      if (!f %in% c("==", "!=") && a$clase %in% c("texto", "logico")) {
        if (identical(a$clase, "texto") && identical(orden_texto, "bytes")) {
          motivo_bytes <- .motivo_orden_bytes_dbi(motor, esq)
          if (!is.null(motivo_bytes)) .no_traducible_dbi(motivo_bytes)
          por_bytes <- TRUE
        } else {
          .no_traducible_dbi(
            "`", f, "` sobre ", a$clase,
            ": R ordena el texto con el locale de la sesi\u00f3n"
          )
        }
      }
      lado_a <- if (isTRUE(a$es_predicado)) {
        .sql_como_valor_logico(a$sql, motor)
      } else {
        a$sql
      }
      lado_b <- if (!is.null(b$sql)) {
        if (!.clases_compatibles_sql(a$clase, b$clase) &&
            !.logico_entero_compatible_dbi(
              esq, a$columna, a$clase, b$clase, motor
            )) {
          .no_traducible_dbi("compara `", a$clase, "` con `", b$clase, "`")
        }
        if (identical(a$clase, "texto") && identical(b$clase, "texto") &&
            motor %in% c("mysql", "mariadb", "sqlserver") &&
            !is.null(a$columna) && !is.null(b$columna) &&
            !identical(esq$cotejamientos[[a$columna]],
                       esq$cotejamientos[[b$columna]])) {
          .no_traducible_dbi(
            "compara dos columnas de texto con distinto cotejamiento (`",
            esq$cotejamientos[[a$columna]], "` y `",
            esq$cotejamientos[[b$columna]], "`)"
          )
        }
        if (!is.null(a$columna)) registrar_guarda_col(a$columna, a$sql)
        if (!is.null(b$columna)) registrar_guarda_col(b$columna, b$sql)
        if (isTRUE(b$es_predicado)) {
          .sql_como_valor_logico(b$sql, motor)
        } else {
          b$sql
        }
      } else {
        if (!is.null(a$columna)) {
          .verificar_literal_espacios_dbi(b$valor, esq, a$columna, motor)
          registrar_guarda_col(a$columna, a$sql)
        }
        .literal_sql_medicion(conexion, b$valor, a$clase, motor,
                              esq = esq, columna = a$columna)
      }
      if (por_bytes) {
        lado_a <- .sql_texto_por_bytes_dbi(lado_a, motor)
        lado_b <- .sql_texto_por_bytes_dbi(lado_b, motor)
      } else {
        if (identical(a$clase, "texto") && !isTRUE(a$es_predicado)) {
          lado_a <- .sql_texto_binario_dbi(lado_a, motor)
        }
        if (!is.null(b$sql) && identical(b$clase, "texto") &&
            !isTRUE(b$es_predicado)) {
          lado_b <- .sql_texto_binario_dbi(lado_b, motor)
        }
      }
      op <- switch(f, "==" = "=", "!=" = "<>", f)
      return(list(sql = paste0("(", lado_a, " ", op, " ", lado_b, ")"),
                  clase = "logico", es_predicado = TRUE))
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
        .evaluar_constante_formula_segura(args[[2L]], entorno, orden_texto),
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
      .verificar_literal_espacios_dbi(resto, esq, a$columna, motor)
      registrar_guarda_col(a$columna, a$sql)
      lista <- vapply(
        as.list(resto),
        function(v) .literal_sql_medicion(conexion, v, a$clase, motor,
                                          esq = esq, columna = a$columna),
        character(1L)
      )
      dentro <- if (length(lista)) {
        izquierda <- if (identical(a$clase, "texto")) {
          .sql_texto_binario_dbi(a$sql, motor)
        } else {
          a$sql
        }
        paste0(izquierda, " IN (", paste(lista, collapse = ", "), ")")
      } else {
        "(1 = 0)"
      }
      sql <- if (con_na) {
        paste0("(", a$sql, " IS NULL OR ", dentro, ")")
      } else {
        paste0("(", a$sql, " IS NOT NULL AND ", dentro, ")")
      }
      return(list(sql = sql, clase = "logico", es_predicado = TRUE))
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
  list(sql = r$sql, motivo = NULL, guardas = guardas,
       motivo_guarda = motivo_guarda)
}

.sql_dentro <- function(pred) {
  paste0("(CASE WHEN ", pred, " THEN 1 ELSE 0 END = 1)")
}

.sql_suma <- function(cond, motor = "sqlite") {
  if (identical(motor, "sqlserver")) {
    return(paste0(
      "COALESCE(SUM(CAST(CASE WHEN ", cond,
      " THEN 1 ELSE 0 END AS BIGINT)), CAST(0 AS BIGINT))"
    ))
  }
  expr <- paste0("COALESCE(SUM(CASE WHEN ", cond, " THEN 1 ELSE 0 END), 0)")
  if (motor %in% c("mysql", "mariadb")) {
    paste0("CAST(", expr, " AS CHAR)")
  } else {
    expr
  }
}

.sql_conteo_total <- function(motor = "sqlite") {
  if (identical(motor, "sqlserver")) return("COUNT_BIG(*)")
  if (motor %in% c("mysql", "mariadb")) return("CAST(COUNT(*) AS CHAR)")
  "COUNT(*)"
}

.sql_y <- function(...) {
  partes <- unlist(list(...))
  partes <- partes[!is.na(partes) & nzchar(partes)]
  if (!length(partes)) "(1 = 1)" else paste0("(", paste(partes, collapse = " AND "), ")")
}

.sql_o <- function(...) {
  partes <- unlist(list(...))
  partes <- unique(partes[!is.na(partes) & nzchar(partes)])
  if (!length(partes)) NULL else paste0("(", paste(partes, collapse = " OR "), ")")
}

.sql_no_ascii <- function(expr, motor) {
  switch(
    motor,
    sqlite = paste0("(", expr, " GLOB '*[^' || char(1) || '-' || char(127) || ']*')"),
    duckdb = paste0("(strlen(", expr, ") <> length(", expr, "))"),
    postgresql = paste0("(octet_length(", expr, ") <> char_length(", expr, "))"),
    mysql = , mariadb = paste0("(OCTET_LENGTH(", expr, ") <> CHAR_LENGTH(", expr, "))"),
    sqlserver = paste0(
      "(", expr, " COLLATE Latin1_General_100_BIN2 LIKE '%[^ -~]%')"
    ),
    .no_traducible_dbi("la guarda de texto no ASCII en ", motor)
  )
}

# R compara el texto por bytes. En SQLite y DuckDB toda comparacion o
# agrupacion de texto que emite la medicion lleva la intercalacion binaria
# explicita sobre el operando columna: asi no cuenta la de la columna, la que
# una vista hereda sin que el catalogo la publique, ni la de la sesion. Medido:
# una vista `v (x) AS SELECT t AS x` sobre `t COLLATE NOCASE` contaba `a` como
# `A` en SQLite, con el catalogo diciendo BINARY. En DuckDB `binary` es palabra
# reservada y va entre comillas. En los demas motores esa forma no esta
# verificada aca: la expresion queda igual y siguen sus guardas de cotejamiento.
.MOTORES_TEXTO_BINARIO_DBI <- c("sqlite", "duckdb")

.sql_texto_binario_dbi <- function(expr, motor) {
  switch(
    motor,
    sqlite = paste0("(", expr, " COLLATE BINARY)"),
    duckdb = paste0("(", expr, " COLLATE \"binary\")"),
    expr
  )
}

.citar_texto_centinela_dbi <- function(conexion, motor, texto) {
  citado <- as.character(DBI::dbQuoteString(conexion, enc2utf8(texto)))
  if (identical(motor, "sqlserver")) paste0("N", citado) else citado
}

.mapa_plegado_centinela_dbi <- function(objetivos) {
  objetivos <- unique(as.character(objetivos))
  caracteres <- unique(unlist(lapply(objetivos, function(x) {
    intToUtf8(utf8ToInt(x), multiple = TRUE)
  }), use.names = FALSE))
  no_ascii <- caracteres[grepl("[^\\001-\\177]", caracteres,
                               perl = TRUE, useBytes = TRUE)]
  if (!length(no_ascii) && !"i" %in% caracteres) return(data.frame())
  candidatos <- intToUtf8(seq.int(0x00C0L, 0x024FL), multiple = TRUE)
  filas <- lapply(no_ascii, function(objetivo) {
    iguales <- vapply(candidatos, function(candidato) {
      identical(as.character(.minusculas_para_comparar(candidato)), objetivo)
    }, logical(1L))
    candidatos[iguales & candidatos != objetivo]
  })
  altos <- unique(unlist(filas, use.names = FALSE))
  if ("i" %in% caracteres) altos <- c(altos, intToUtf8(0x0130L))
  altos <- unique(as.character(altos))
  if (!length(altos)) return(data.frame())
  bajos <- vapply(altos, function(alto) {
    as.character(.minusculas_para_comparar(alto))
  }, character(1L), USE.NAMES = FALSE)
  data.frame(alto = altos, bajo = bajos, stringsAsFactors = FALSE)
}

.sql_centinela_sin_no_ascii_dbi <- function(expr, motor, conexion, objetivos) {
  mapa <- .mapa_plegado_centinela_dbi(objetivos)
  if (!nrow(mapa)) return(.sql_no_ascii(expr, motor))
  resto <- expr
  for (i in seq_len(nrow(mapa))) {
    for (texto in c(mapa$alto[[i]], mapa$bajo[[i]])) {
      resto <- paste0(
        "REPLACE(", resto, ", ",
        .citar_texto_centinela_dbi(conexion, motor, texto), ", '')"
      )
    }
  }
  .sql_no_ascii(resto, motor)
}

.sql_plegado_centinela <- function(col, motor, conexion, objetivos = character()) {
  recortado <- switch(
    motor,
    sqlite = paste0("TRIM(", col, ", ' ' || char(9) || char(10) || char(13))"),
    duckdb = paste0("TRIM(", col, ", ' ' || chr(9) || chr(10) || chr(13))"),
    postgresql = paste0("BTRIM(", col, ", E' \\t\\n\\r')"),
    mysql = , mariadb = paste0(
      "REGEXP_REPLACE(", col, ", ",
      as.character(DBI::dbQuoteString(conexion, "^[ \t\n\r]+|[ \t\n\r]+$")),
      ", '')"
    ),
    sqlserver = paste0("LTRIM(RTRIM(", col, "))"),
    .no_traducible_dbi("el recorte de bordes en ", motor)
  )
  mapa <- .mapa_plegado_centinela_dbi(objetivos)
  plegado <- recortado
  if (nrow(mapa)) {
    for (i in seq_len(nrow(mapa))) {
      plegado <- paste0(
        "REPLACE(", plegado, ", ",
        .citar_texto_centinela_dbi(conexion, motor, mapa$alto[[i]]), ", ",
        .citar_texto_centinela_dbi(conexion, motor, mapa$bajo[[i]]), ")"
      )
    }
  }
  paste0("LOWER(", plegado, ")")
}

# ---------------------------------------------------------------------------
# 4. Compilacion de cada nodo desde la especificacion
# ---------------------------------------------------------------------------

# La igualdad de una clave contra la del referencial, como la compara R: por el
# texto canonico de cada valor. Misma clase o numero contra numero, por valor;
# texto contra texto, por bytes (`.sql_texto_binario_dbi()`); texto contra
# entero, con el entero escrito como texto. Con `plegar_ref`/`plegar_dep` el
# lado de texto pasa por `LOWER()`: el pliegue de mayusculas de la
# normalizacion, que solo es exacto sobre ASCII y por eso viaja con su guarda
# (`.sql_guarda_pliegue_dbi()`). `NULL` si el par no se puede escribir igual.
.cast_entero_a_texto_dbi <- function(expr_int, esq_txt, col_txt, motor) {
  tp_txt <- if (!is.null(esq_txt$tipos_base)) esq_txt$tipos_base[[col_txt]] else NA_character_
  coll_txt <- esq_txt$cotejamientos[[col_txt]]
  coll_ok <- !is.null(coll_txt) && !is.na(coll_txt) &&
    grepl("^[A-Za-z0-9_]+$", coll_txt, perl = TRUE, useBytes = TRUE)
  sufijo_coll <- if (motor %in% c("mysql", "mariadb", "sqlserver") && coll_ok) {
    paste0(" COLLATE ", coll_txt)
  } else ""
  tipo_txt <- switch(
    motor,
    duckdb = "VARCHAR",
    mysql = , mariadb = "CHAR",
    sqlserver = if (identical(tp_txt, "nvarchar")) "NVARCHAR(20)" else "VARCHAR(20)",
    "TEXT"
  )
  paste0("CAST(", expr_int, " AS ", tipo_txt, ")", sufijo_coll)
}

.igualdad_referencial_dbi <- function(motor, cp, cf, pk, fk, esq_ref, pk_col,
                                      esq, fk_col, plegar_ref = FALSE,
                                      plegar_dep = FALSE) {
  texto_bin <- function(expr) .sql_texto_binario_dbi(expr, motor)
  plegado <- function(expr, plegar) if (isTRUE(plegar)) paste0("LOWER(", expr, ")") else expr
  if ((.clases_compatibles_sql(cp, cf) &&
       !(cp %in% "entero64" || cf %in% "entero64")) ||
      identical(cp, cf)) {
    if (identical(cp, "texto") && identical(cf, "texto")) {
      return(paste0(
        texto_bin(plegado(pk, plegar_ref)), " = ", texto_bin(plegado(fk, plegar_dep))
      ))
    }
    return(paste0(pk, " = ", fk))
  }
  if (identical(cp, "texto") && identical(cf, "entero")) {
    return(paste0(
      texto_bin(plegado(pk, plegar_ref)), " = ",
      texto_bin(.cast_entero_a_texto_dbi(fk, esq_ref, pk_col, motor))
    ))
  }
  if (identical(cp, "entero") && identical(cf, "texto")) {
    return(paste0(
      texto_bin(.cast_entero_a_texto_dbi(pk, esq, fk_col, motor)), " = ",
      texto_bin(plegado(fk, plegar_dep))
    ))
  }
  NULL
}

# Lo que R no lee como escalar, o lee distinto de como la base declara la
# columna: la medicion no puede reproducir la lectura de R. El motivo, o `NULL`.
.motivo_tipos_esquema_dbi <- function(esq_t, cols_t) {
  cl_t <- esq_t$clases[cols_t]
  no_esc <- cols_t[unname(cl_t) == "otro"]
  if (length(no_esc)) {
    return(paste0(
      "la columna `", no_esc[[1L]],
      "` no es un tipo escalar que R pueda leer para esta metrica"
    ))
  }
  lecturas_no_doble <- cols_t[vapply(cols_t, function(c) {
    clase <- esq_t$clases[[c]]
    !clase %in% c("doble", "entero") &&
      .tipo_declarado_entero_ancho_dbi(esq_t$tipos[[c]])
  }, logical(1L))]
  if (length(lecturas_no_doble)) {
    columna_no_doble <- lecturas_no_doble[[1L]]
    clase_lectura <- switch(
      esq_t$clases[[columna_no_doble]],
      entero64 = "entero de 64 bits (`integer64`)",
      texto = "texto",
      esq_t$clases[[columna_no_doble]]
    )
    tipo_real <- esq_t$tipos[[columna_no_doble]]
    if (is.null(tipo_real) || !length(tipo_real) || is.na(tipo_real)) {
      tipo_real <- "tipo num\u00e9rico no especificado"
    }
    return(paste0(
      "R lee la columna `", columna_no_doble, "` como ", clase_lectura,
      ", pero la base la declara como `", as.character(tipo_real),
      "`; la medici\u00f3n no puede reproducir la lectura de R"
    ))
  }
  NULL
}

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
  validar_tipos_esquema <- function(esq_t, cols_t) {
    motivo <- .motivo_tipos_esquema_dbi(esq_t, cols_t)
    if (is.null(motivo)) NULL else no(motivo)
  }
  clases <- esq$clases[columnas]
  fallo_tipo <- validar_tipos_esquema(esq, columnas)
  if (!is.null(fallo_tipo)) return(fallo_tipo)
  pre <- .precondicion_motor_medicion(motor, spec$nodo, clases, sesion = esq$sesion)
  if (!is.null(pre)) return(no(pre))
  U <- universos[[ent_esq]](instancia)
  if (!is.null(U$motivo)) {
    return(no("la regla de `aplicabilidad`: ", U$motivo))
  }
  col <- function(c, nan = TRUE) {
    .columna_sql_medicion(conexion, esq, c, motor, normalizar_nan = nan)
  }
  combinar_guardas <- function(g_local = NULL, mot_local = NULL,
                               g_extra = character(), mot_extra = NULL) {
    todas <- c(U$guardas, g_extra, g_local)
    g_final <- .sql_o(todas)
    mot_final <- mot_local %||% mot_extra %||% U$motivo_guarda
    list(guarda = g_final, motivo = mot_final)
  }
  r <- tryCatch(switch(
    spec$nodo,
    no_ausente = {
      .validar_columna_base_dbi(
        esq, spec$columna, motor, requiere_valor = FALSE
      )
      x <- col(spec$columna)
      clase <- esq$clases[[spec$columna]]
      aplicable <- ""
      g_apl <- character()
      mot_apl <- NULL
      g_texto <- character()
      mot_texto <- NULL
      if (!is.null(spec$aplicable)) {
        t <- .traducir_predicado_dbi(
          spec$aplicable[[2L]], environment(spec$aplicable),
          as.list(esq$clases), esq, motor, conexion, max_valores_in
        )
        if (is.null(t$sql)) .no_traducible_dbi("`aplicable`: ", t$motivo)
        aplicable <- .sql_dentro(t$sql)
        g_apl <- t$guardas
        mot_apl <- t$motivo_guarda
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
          .validar_columna_comparacion_dbi(
            esq, spec$columna, motor, requiere_no_pad = TRUE
          )
          if (identical(motor, "postgresql")) {
            if (!isTRUE(esq$sesion$es_utf8)) {
              .no_traducible_dbi(
                "centinela de texto en PostgreSQL: la codificaci\u00f3n del servidor ",
                "no es UTF8 (`", esq$sesion$enc %||% "desconocida", "`)"
              )
            }
            if (!isTRUE(esq$sesion$lower_i_es_i)) {
              .no_traducible_dbi(
                "centinela de texto en PostgreSQL: `lower('I')` no da `'i'` en este locale"
              )
            }
          }
          if (motor %in% c("mysql", "mariadb") && !isTRUE(esq$sesion$lower_i_es_i)) {
            .no_traducible_dbi(
              "centinela de texto en ", motor, ": `LOWER('I')` no da `'i'`"
            )
          }
          objetivos <- unique(.minusculas_para_comparar(.texto_analizable(vn)$valores))
          objetivos <- objetivos[!is.na(objetivos)]
          if (length(objetivos) > max_valores_in) {
            .no_traducible_dbi("demasiados centinelas")
          }
          if (length(objetivos)) {
            plegado <- .sql_plegado_centinela(
              x, motor, conexion, objetivos = objetivos
            )
            no_admitido <- .sql_centinela_sin_no_ascii_dbi(
              plegado, motor, conexion, objetivos
            )
            g_texto <- no_admitido
            mot_texto <- paste0(
              "la columna `", spec$columna,
              "` contiene texto fuera del subconjunto de plegado Unicode ",
              "que el motor puede reproducir con exactitud"
            )
            centinela <- paste0(
              "(NOT ", no_admitido, " AND ",
              .sql_texto_binario_dbi(plegado, motor), " IN (",
              paste(vapply(objetivos, function(o) {
                .citar_texto_centinela_dbi(conexion, motor, o)
              }, character(1L)), collapse = ", "), "))"
            )
          }
        } else {
          .validar_columna_comparacion_dbi(esq, spec$columna, motor)
          lits <- unique(vn[!is.na(vn)])
          if (length(lits) > max_valores_in) {
            .no_traducible_dbi("demasiados centinelas")
          }
          if (length(lits)) {
            centinela <- paste0(
              "(", x, " IN (",
              paste(vapply(as.list(lits), function(v) {
                .literal_sql_medicion(conexion, v, clase, motor,
                                      esq = esq, columna = spec$columna)
              }, character(1L)), collapse = ", "), "))"
            )
          }
          # SQLite guarda texto en una columna de fecha sin quejarse y RSQLite
          # lo lee como fecha con reglas propias: el literal numerico no lo ve
          # (tres `1970-01-01` guardados como texto daban 1 donde R daba
          # 0,85). Si la columna guarda texto la medida se declara, con la
          # misma guarda que el resumen de fechas de `perfilar_dbi()`.
          if (length(lits) && identical(motor, "sqlite") &&
              clase %in% c("fecha", "instante")) {
            g_texto <- .sql_guardado_texto_sqlite_dbi(x)
            mot_texto <- .motivo_fecha_texto_sqlite_dbi(spec$columna)
          }
        }
      }
      cumple <- paste0("(", x, " IS NOT NULL AND NOT ", centinela, ")")
      cg <- combinar_guardas(
        g_extra = c(g_apl, g_texto),
        mot_extra = mot_apl %||% mot_texto
      )
      list(
        via = "sql_consolidado",
        universo = universo,
        evaluable = universo,
        cumple = cumple,
        sin_valor = paste0("(", x, " IS NULL)"),
        guarda = cg$guarda,
        motivo_guarda = cg$motivo,
        columnas_completas = spec$columna,
        aplicable_sql = .sql_y(aplicable, presentes)
      )
    },
    en_conjunto = , en_rango = , fecha_en_rango = {
      clase <- esq$clases[[spec$columna]]
      if (identical(spec$nodo, "fecha_en_rango") &&
          !clase %in% c("fecha", "instante")) {
        return(list(
          via = "no_medible_metodo",
          motivo = if (isTRUE(spec$contrato_columna)) {
            "La columna de actualizaci\u00f3n debe ser Date o POSIXt."
          } else {
            "La oportunidad requiere un atributo Date o POSIXt."
          }
        ))
      }
      rango_por_bytes <- identical(spec$nodo, "en_rango") &&
        identical(clase, "texto")
      if (rango_por_bytes) {
        if (!identical(.orden_texto_instancia(instancia), "bytes")) {
          .no_traducible_dbi(
            "rango sobre texto: R ordena con el locale de la sesi\u00f3n"
          )
        }
        motivo_bytes <- .motivo_orden_bytes_dbi(motor, esq)
        if (!is.null(motivo_bytes)) .no_traducible_dbi(motivo_bytes)
        if (identical(motor, "duckdb") && !isTRUE(esq$sesion$valida_utf8)) {
          .no_traducible_dbi(
            "rango sobre texto por bytes: esta versi\u00f3n de DuckDB no tiene ",
            "`try()`, que hace falta para dejar afuera, como la medici\u00f3n en ",
            "memoria, el texto con bytes que no son UTF-8 v\u00e1lido"
          )
        }
      }
      .validar_columna_comparacion_dbi(esq, spec$columna, motor)
      x <- col(spec$columna)
      guarda_local <- NULL
      motivo_local <- NULL
      if (identical(clase, "texto")) {
        if (identical(motor, "sqlite")) {
          guarda_local <- .sql_no_ascii(x, motor)
          motivo_local <- .motivo_guarda_dbi(motor)
        } else if (.requiere_guarda_espacios_dbi(esq, spec$columna, motor)) {
          guarda_local <- .sql_guarda_espacios_dbi(x, motor)
          motivo_local <- .motivo_guarda_dbi(motor, esq$cotejamientos[[spec$columna]])
        }
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
        .verificar_literal_espacios_dbi(lits, esq, spec$columna, motor)
        if (length(lits)) {
          x_cmp <- if (identical(clase, "texto")) {
            .sql_texto_binario_dbi(x, motor)
          } else {
            x
          }
          paste0(
            "(", x_cmp, " IN (",
            paste(vapply(as.list(lits), function(v) {
              .literal_sql_medicion(conexion, v, clase, motor,
                                    esq = esq, columna = spec$columna)
            }, character(1L)), collapse = ", "), "))"
          )
        } else {
          "(1 = 0)"
        }
      } else if (identical(spec$nodo, "en_rango")) {
        lo <- .literal_sql_medicion(conexion, spec$minimo, clase, motor,
                                    esq = esq, columna = spec$columna)
        hi <- .literal_sql_medicion(conexion, spec$maximo, clase, motor,
                                    esq = esq, columna = spec$columna)
        xr <- x
        if (rango_por_bytes) {
          xr <- .sql_texto_por_bytes_dbi(x, motor)
          lo <- .sql_texto_por_bytes_dbi(lo, motor)
          hi <- .sql_texto_por_bytes_dbi(hi, motor)
        }
        paste0(
          "(", xr, if (spec$inclusivo[[1L]]) " >= " else " > ", lo,
          " AND ", xr, if (spec$inclusivo[[2L]]) " <= " else " < ", hi, ")"
        )
      } else {
        if (identical(motor, "sqlserver") &&
            (is.null(esq$sesion$version_mayor) ||
             is.na(esq$sesion$version_mayor) ||
             esq$sesion$version_mayor < 13L)) {
          .no_traducible_dbi(
            "SQL Server anterior a 2016 no dispone de `DATEDIFF_BIG`"
          )
        }
        usa_epoca <- .columna_como_epoca_dbi(esq, spec$columna, motor)
        usa_micro <- !usa_epoca && ((motor %in% c("mysql", "mariadb") &&
                        identical(clase, "instante")) ||
          identical(motor, "sqlserver"))
        seg <- if (usa_epoca) x else switch(
          motor,
          duckdb = if (identical(clase, "fecha")) {
            paste0("epoch(CAST(", x, " AS TIMESTAMP))")
          } else {
            paste0("epoch(", x, ")")
          },
          sqlite = if (identical(clase, "fecha")) paste0("(", x, " * 86400)") else x,
          postgresql = paste0("extract(epoch from ", x, ")"),
          mysql = , mariadb = if (identical(clase, "fecha")) {
            paste0("(DATEDIFF(", x, ", '1970-01-01') * 86400)")
          } else {
            paste0("TIMESTAMPDIFF(MICROSECOND, '1970-01-01 00:00:00', ", x, ")")
          },
          sqlserver = paste0(
            "DATEDIFF_BIG(microsecond, '1970-01-01', CAST(", x, " AS DATETIME2))"
          )
        )
        lit <- function(v) {
          if (usa_micro) {
            format(round(.fecha_numerica(v) * 1e6), scientific = FALSE, trim = TRUE)
          } else {
            formatC(.fecha_numerica(v), digits = 17, format = "g")
          }
        }
        partes <- c(
          if (!is.null(spec$desde)) paste0(seg, " >= ", lit(spec$desde)),
          if (!is.null(spec$hasta)) paste0(seg, " <= ", lit(spec$hasta))
        )
        paste0("(", paste(partes, collapse = " AND "), ")")
      }
      evaluable <- .sql_y(U$sql, paste0("(", x, " IS NOT NULL)"))
      # Lo que la memoria evalua del rango: lo presente que es texto UTF-8
      # valido (ver la sesion de DuckDB). En SQLite lo cubre su guarda de texto
      # no ASCII, que ya rige aca.
      if (rango_por_bytes && identical(motor, "duckdb")) {
        evaluable <- .sql_y(
          evaluable, paste0("(try(decode(encode(", x, "))) IS NOT NULL)")
        )
      }
      g_local_ev <- if (!is.null(guarda_local)) .sql_y(evaluable, guarda_local)
      cg <- combinar_guardas(g_local = g_local_ev, mot_local = motivo_local)
      list(
        via = "sql_consolidado",
        universo = U$sql,
        evaluable = evaluable,
        cumple = cumple,
        sin_valor = paste0("(", x, " IS NULL)"),
        guarda = cg$guarda,
        motivo_guarda = cg$motivo,
        columnas_completas = spec$columna
      )
    },
    predicado_fila = {
      vis <- as.list(esq$clases[instancia$atributos])
      t <- .traducir_predicado_dbi(
        spec$formula[[2L]], environment(spec$formula), vis,
        esq, motor, conexion, max_valores_in,
        orden_texto = .orden_texto_instancia(instancia)
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
      g_pred <- if (length(t$guardas)) .sql_y(U$sql, .sql_o(t$guardas))
      cg <- combinar_guardas(g_local = g_pred, mot_local = t$motivo_guarda)
      list(
        via = "sql_consolidado",
        universo = U$sql,
        evaluable = U$sql,
        cumple = t$sql,
        na_regla = .sql_y(U$sql, .sql_es_nulo_predicado(t$sql, motor)),
        guarda = cg$guarda,
        motivo_guarda = cg$motivo,
        columnas_completas = instancia$atributos
      )
    },
    duplicado = {
      for (c in spec$columnas) {
        .validar_columna_comparacion_dbi(esq, c, motor)
      }
      cols_g <- vapply(
        spec$columnas,
        function(c) col(c, nan = isTRUE(spec$excluir_na)),
        character(1L)
      )
      filtro <- if (isTRUE(spec$excluir_na)) {
        paste0("(", cols_g[[1L]], " IS NOT NULL)")
      } else ""
      guardas_dup <- character()
      motivo_dup <- NULL
      if (isTRUE(spec$excluir_na) && identical(motor, "sqlite") &&
          identical(esq$clases[[spec$columnas]], "texto")) {
        guardas_dup <- c(guardas_dup, .sql_no_ascii(cols_g[[1L]], motor))
        motivo_dup <- .motivo_guarda_dbi(motor)
      }
      for (i_c in seq_along(spec$columnas)) {
        cn <- spec$columnas[[i_c]]
        if (.requiere_guarda_espacios_dbi(esq, cn, motor)) {
          guardas_dup <- c(
            guardas_dup,
            paste0("(", cols_g[[i_c]], " IS NOT NULL AND ",
                   .sql_guarda_espacios_dbi(cols_g[[i_c]], motor), ")")
          )
          if (is.null(motivo_dup)) {
            motivo_dup <- .motivo_guarda_dbi(motor, esq$cotejamientos[[cn]])
          }
        }
      }
      g_local_dup <- if (length(guardas_dup)) {
        .sql_y(U$sql, filtro, .sql_o(guardas_dup))
      }
      cg <- combinar_guardas(g_local = g_local_dup, mot_local = motivo_dup)
      cnt_inner <- if (identical(motor, "sqlserver")) "COUNT_BIG(*)" else "COUNT(*)"
      cols_grupo <- vapply(seq_along(cols_g), function(i) {
        if (identical(esq$clases[[spec$columnas[[i]]]], "texto")) {
          .sql_texto_binario_dbi(cols_g[[i]], motor)
        } else {
          cols_g[[i]]
        }
      }, character(1L))
      sub <- paste0(
        "SELECT ", cnt_inner, " AS lupa_c FROM ",
        esq$sql, " WHERE ", .sql_y(U$sql, filtro), " GROUP BY ",
        paste(cols_grupo, collapse = ", ")
      )
      consulta_dup <- if (identical(motor, "sqlserver")) {
        paste0(
          "SELECT COALESCE(SUM(CASE WHEN lupa_c > 1 THEN lupa_c ",
          "ELSE CAST(0 AS BIGINT) END), CAST(0 AS BIGINT)) AS num, ",
          "COALESCE(SUM(lupa_c), CAST(0 AS BIGINT)) AS den FROM (", sub, ") lupa_g"
        )
      } else if (motor %in% c("mysql", "mariadb")) {
        paste0(
          "SELECT CAST(COALESCE(SUM(CASE WHEN lupa_c > 1 THEN lupa_c ELSE 0 END), 0) AS CHAR)",
          " AS num, CAST(COALESCE(SUM(lupa_c), 0) AS CHAR) AS den FROM (", sub, ") lupa_g"
        )
      } else {
        paste0(
          "SELECT COALESCE(SUM(CASE WHEN lupa_c > 1 THEN lupa_c ELSE 0 END), 0)",
          " AS num, COALESCE(SUM(lupa_c), 0) AS den FROM (", sub, ") lupa_g"
        )
      }
      list(
        via = "sql_dedicado",
        consulta = consulta_dup,
        universo = U$sql,
        evaluable = .sql_y(U$sql, filtro),
        sin_valor = if (isTRUE(spec$excluir_na)) {
          paste0("(", cols_g[[1L]], " IS NULL)")
        },
        guarda = cg$guarda,
        motivo_guarda = cg$motivo,
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
      fallo_ref <- validar_tipos_esquema(esq_ref, spec$pk)
      if (!is.null(fallo_ref)) .no_traducible_dbi(fallo_ref$motivo)
      pre_ref <- .precondicion_motor_medicion(
        motor, spec$nodo, esq_ref$clases[[spec$pk]], sesion = esq_ref$sesion
      )
      if (!is.null(pre_ref)) .no_traducible_dbi(pre_ref)
      .validar_columna_comparacion_dbi(esq, spec$fk, motor)
      .validar_columna_comparacion_dbi(esq_ref, spec$pk, motor)
      cp <- esq_ref$clases[[spec$pk]]
      cf <- esq$clases[[spec$fk]]
      # `NaN` es un vacio, como para `is.na()`, `NoNulo` y los rangos: la
      # referencial lo contaba como un valor presente sin pareja -un huerfano-
      # mientras contaba el mismo `NaN` entre los vacios, y sus cuentas no
      # cerraban. Igual en memoria (`.r_cobertura_referencial()`).
      pk <- .columna_sql_medicion(conexion, esq_ref, spec$pk, motor,
                                  alias = "lupa_r", normalizar_nan = TRUE)
      fk <- .columna_sql_medicion(conexion, esq, spec$fk, motor,
                                  alias = "lupa_d", normalizar_nan = TRUE)
      fk_nulo <- .columna_sql_medicion(conexion, esq, spec$fk, motor,
                                       alias = "lupa_d", normalizar_nan = TRUE)
      if (identical(cp, "texto") && identical(cf, "texto") &&
          motor %in% c("mysql", "mariadb", "sqlserver") &&
          !identical(esq_ref$cotejamientos[[spec$pk]],
                     esq$cotejamientos[[spec$fk]])) {
        .no_traducible_dbi(
          "claves de texto con distinto cotejamiento (`",
          esq$cotejamientos[[spec$fk]], "` y `",
          esq_ref$cotejamientos[[spec$pk]], "`)"
        )
      }
      igualdad <- .igualdad_referencial_dbi(
        motor, cp, cf, pk, fk, esq_ref, spec$pk, esq, spec$fk
      )
      if (is.null(igualdad)) {
        .no_traducible_dbi(
          "clave `", cp, "` contra `", cf,
          "`: R compara por el texto can\u00f3nico de cada valor y el motor no escribe igual ese tipo"
        )
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
      guardas_dep <- character()
      guardas_ref <- character()
      motivo_guarda_ref <- NULL
      agregar_guarda_dep <- function(cond, mot) {
        guardas_dep <<- c(guardas_dep, cond)
        if (is.null(motivo_guarda_ref)) motivo_guarda_ref <<- mot
      }
      agregar_guarda_ref <- function(cond, mot) {
        guardas_ref <<- c(guardas_ref, cond)
        if (is.null(motivo_guarda_ref)) motivo_guarda_ref <<- mot
      }
      if (identical(motor, "sqlite") && "texto" %in% c(cp, cf)) {
        mot_sq <- .motivo_guarda_dbi("sqlite", sujeto = "la clave")
        if (identical(cf, "texto")) {
          agregar_guarda_dep(.sql_y(evaluable, .sql_no_ascii(fk, motor)), mot_sq)
        }
        if (identical(cp, "texto")) {
          agregar_guarda_ref(
            .sql_y(U_ref$sql, paste0("(", pk, " IS NOT NULL)"),
                   .sql_no_ascii(pk, motor)),
            mot_sq
          )
        }
      }
      if (.requiere_guarda_espacios_dbi(esq, spec$fk, motor)) {
        agregar_guarda_dep(
          .sql_y(evaluable, .sql_guarda_espacios_dbi(fk, motor)),
          .motivo_guarda_dbi(motor, esq$cotejamientos[[spec$fk]])
        )
      }
      if (.requiere_guarda_espacios_dbi(esq_ref, spec$pk, motor)) {
        agregar_guarda_ref(
          .sql_y(U_ref$sql, paste0("(", pk, " IS NOT NULL)"),
                 .sql_guarda_espacios_dbi(pk, motor)),
          .motivo_guarda_dbi(motor, esq_ref$cotejamientos[[spec$pk]])
        )
      }
      if (length(U_dep$guardas)) {
        agregar_guarda_dep(.sql_o(U_dep$guardas), U_dep$motivo_guarda)
      }
      if (length(U_ref$guardas)) {
        agregar_guarda_ref(.sql_o(U_ref$guardas), U_ref$motivo_guarda)
      }
      cond_gd <- .sql_o(guardas_dep)
      cond_gr <- .sql_o(guardas_ref)
      if (identical(motor, "sqlserver")) {
        # T-SQL no admite una subconsulta `EXISTS` dentro de `SUM(...)`;
        # se proyectan los indicadores por fila en una tabla derivada.
        partes_guarda <- c(
          if (!is.null(cond_gd)) {
            "COALESCE(SUM(CAST(lupa_gd AS BIGINT)), CAST(0 AS BIGINT))"
          },
          if (!is.null(cond_gr)) {
            paste0("(SELECT ", .sql_suma(cond_gr, motor),
                   " FROM ", esq_ref$sql, " lupa_r)")
          }
        )
        expr_guarda_ss <- if (length(partes_guarda)) {
          paste0(", (", paste(partes_guarda, collapse = " + "), ") AS guarda")
        } else ""
        consulta <- paste0(
          "SELECT ",
          "COALESCE(SUM(CAST(lupa_ev_ex AS BIGINT)), CAST(0 AS BIGINT)) AS num, ",
          "COALESCE(SUM(CAST(lupa_ev AS BIGINT)), CAST(0 AS BIGINT)) AS den, ",
          "COALESCE(SUM(CAST(lupa_u AS BIGINT)), CAST(0 AS BIGINT)) AS univ, ",
          "COALESCE(SUM(CAST(lupa_sv AS BIGINT)), CAST(0 AS BIGINT)) AS sin_valor, ",
          "COUNT_BIG(*) AS n_filas, ",
          "(SELECT COUNT_BIG(*) FROM ", esq_ref$sql, ") AS n_filas_ref",
          expr_guarda_ss,
          " FROM (SELECT ",
          "CASE WHEN ", .sql_y(evaluable, existe), " THEN 1 ELSE 0 END AS lupa_ev_ex, ",
          "CASE WHEN ", evaluable, " THEN 1 ELSE 0 END AS lupa_ev, ",
          "CASE WHEN ", .sql_y(U_dep$sql), " THEN 1 ELSE 0 END AS lupa_u, ",
          "CASE WHEN ", .sql_y(U_dep$sql, paste0("(", fk_nulo, " IS NULL)")),
          " THEN 1 ELSE 0 END AS lupa_sv",
          if (!is.null(cond_gd)) {
            paste0(", CASE WHEN ", cond_gd, " THEN 1 ELSE 0 END AS lupa_gd")
          } else "",
          " FROM ", esq$sql, " lupa_d) lupa_s"
        )
      } else {
        guarda_sumas <- c(
          if (!is.null(cond_gd)) .sql_suma(cond_gd, motor),
          if (!is.null(cond_gr)) {
            paste0("(SELECT ", .sql_suma(cond_gr, motor),
                   " FROM ", esq_ref$sql, " lupa_r)")
          }
        )
        expr_guarda_ref <- if (length(guarda_sumas)) {
          if (motor %in% c("mysql", "mariadb")) {
            sumas_num <- paste0("CAST(", guarda_sumas, " AS SIGNED)")
            paste0(", CAST((", paste(sumas_num, collapse = " + "), ") AS CHAR) AS guarda")
          } else {
            paste0(", (", paste(paste0("COALESCE(", guarda_sumas, ", 0)"),
                                collapse = " + "), ") AS guarda")
          }
        } else ""
        consulta <- paste0(
          "SELECT ", .sql_suma(.sql_y(evaluable, existe), motor), " AS num, ",
          .sql_suma(evaluable, motor), " AS den, ",
          .sql_suma(.sql_y(U_dep$sql), motor), " AS univ, ",
          .sql_suma(.sql_y(U_dep$sql, paste0("(", fk_nulo, " IS NULL)")), motor), " AS sin_valor, ",
          .sql_conteo_total(motor), " AS n_filas, ",
          # Las filas de la tabla referida que vio ESTA sentencia: si cambio
          # entre la lectura de otra metrica y esta, la cobertura se midio contra
          # otra tabla, y solo comparando los dos conteos se ve.
          "(SELECT ", .sql_conteo_total(motor), " FROM ", esq_ref$sql,
          ") AS n_filas_ref",
          expr_guarda_ref,
          " FROM ", esq$sql, " lupa_d"
        )
      }
      list(
        via = "sql_dedicado_referencial",
        consulta = consulta,
        motivo_guarda = motivo_guarda_ref
      )
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
      if (is.null(aplicabilidad) || !length(columnas)) {
        return(list(sql = "", guardas = character(), motivo_guarda = NULL))
      }
      reglas <- aplicabilidad[names(aplicabilidad) %in% intersect(columnas, esq$campos)]
      if (!length(reglas)) {
        return(list(sql = "", guardas = character(), motivo_guarda = NULL))
      }
      partes <- character()
      guardas <- character()
      motivo_guarda <- NULL
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
        if (length(t$guardas)) guardas <- unique(c(guardas, t$guardas))
        if (is.null(motivo_guarda) && !is.null(t$motivo_guarda)) {
          motivo_guarda <- t$motivo_guarda
        }
      }
      list(sql = .sql_y(partes), guardas = guardas, motivo_guarda = motivo_guarda)
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
                                     tamano_lote = NULL, contador = NULL,
                                     orden_texto = "idioma") {
  resuelto <- .resolver_tablas_medir_dbi(modelo, conexion, tablas)
  con <- resuelto$conexion
  entidades <- resuelto$entidades
  fisicas <- resuelto$fisicas
  error_sesion <- NULL
  sesion <- tryCatch(
    .verificar_sesion_medicion_dbi(
      con, motor, contador = contador, orden_texto = orden_texto
    ),
    lupa_presupuesto_medicion_dbi = function(err) {
      error_sesion <<- conditionMessage(err)
      list(ok = FALSE, motor = motor, n_consultas = 0L, motivo = error_sesion)
    },
    error = function(err) {
      list(
        ok = FALSE, motor = motor, n_consultas = 0L,
        motivo = paste0(
          "no se pudieron consultar los metadatos de sesi\u00f3n de `", motor, "`"
        )
      )
    }
  )
  errores_esquema <- stats::setNames(vector("list", length(entidades)), entidades)
  esquemas <- stats::setNames(lapply(entidades, function(e) {
    if (!is.null(error_sesion)) {
      errores_esquema[[e]] <<- error_sesion
      return(NULL)
    }
    tryCatch(
      .esquema_medicion_dbi(con, fisicas[[e]], motor,
                            contador = contador, sesion = sesion),
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
  # Las tablas de los referenciales del motor: su esquema se lee una vez por
  # corrida y lo cuentan las consultas del plan.
  referenciales <- .resolutor_referenciales_dbi(
    conexion, con, motor, contador, sesion, error_sesion
  )
  planes <- lapply(seq_along(modelo$metricas), function(j) {
    # La copia de la instancia lleva el orden de texto pedido: la compilacion la
    # lee, y la via por bloques la mide en R con ella.
    instancia <- .con_orden_texto(modelo$metricas[[j]], orden_texto)
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
      if (.admite_bloques_medicion_dbi(estrategia_no_sql, instancia, spec,
                                       cols_bloq)) {
        c(base, list(via = "bloques_r", motivo = motivo, columnas_bloque = cols_bloq))
      } else {
        c(base, list(via = "no_medible", motivo = motivo))
      }
    }
    if (.es_metrica_referencial_dbi(instancia)) {
      return(.planificar_referencial_dbi(
        base, instancia, esquemas, motor, conexion, con, universos,
        estrategia_no_sql, referenciales
      ))
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
  esquemas_referencial <- referenciales$esquemas()
  n_esquema_estimadas <- as.integer(sesion$n_consultas %||% 0L) + sum(vapply(
    c(Filter(Negate(is.null), esquemas), esquemas_referencial),
    function(e) as.integer(e$n_consultas_esquema),
    integer(1L)
  ))
  # Una validacion de la clave por tabla y clave de referencial, compartida por
  # las metricas que la usan.
  n_validaciones_referencial <- length(unique(unlist(lapply(planes, function(x) {
    if (isTRUE(x$referencial_motor) &&
        identical(x$via, "sql_dedicado_referencial")) x$clave_validacion
  }))))
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
    n_consolidadas = n_consolidadas,
    esquemas_referencial = esquemas_referencial,
    n_validaciones_referencial = n_validaciones_referencial
  )
}

.enriquecer_planes_medicion_dbi <- function(p, conexion, aplicabilidad,
                                            max_valores_in) {
  universos <- .universos_medicion_dbi(
    aplicabilidad, Filter(Negate(is.null), p$esquemas),
    p$motor, conexion, max_valores_in
  )
  # Lo que cada item suma antes de la consulta sale de `.preparar_item_medicion_dbi()`,
  # la misma que usa `conformidad_dbi()`. Estaba copiada aqui, y la copia no
  # contaba los vacios de la columna de una vigencia.
  for (k in seq_along(p$planes)) {
    x <- p$planes[[k]]
    if (!x$via %in% c("sql_consolidado", "sql_dedicado")) next
    esq <- p$esquemas[[x$entidad]]
    p$planes[[k]] <- .preparar_item_medicion_dbi(
      x, esq, universos[[x$entidad]](x$instancia), conexion, p$motor
    )
  }
  list(p = p, universos = universos)
}

.compilar_plan_medicion_dbi <- function(conexion, p, tablas, aplicabilidad,
                                        max_valores_in, tamano_lote,
                                        bloque_filas) {
  enriquecido <- .enriquecer_planes_medicion_dbi(
    p, conexion, aplicabilidad, max_valores_in
  )
  p <- enriquecido$p
  motor <- p$motor
  ejecutar <- function() {
    estado <- .CONTEXTO_MOTOR$compilador
    agregar <- function(sql, etapa) {
      if (length(sql) != 1L || is.na(sql)) return(invisible(NULL))
      .compilar_sql_dbi(conexion, sql, etapa, motor)
      invisible(NULL)
    }
    for (esq in c(p$esquemas, p$esquemas_referencial)) {
      if (is.null(esq)) next
      for (sql in esq$consultas_esquema %||% character()) {
        agregar(sql, "esquema")
      }
    }
    validadas <- character()
    for (entidad in names(p$esquemas)) {
      esq <- p$esquemas[[entidad]]
      if (is.null(esq)) next
      items <- Filter(function(x) {
        x$via %in% c("sql_consolidado", "sql_dedicado") &&
          identical(x$entidad, entidad)
      }, p$planes)
      if (length(items)) {
        lotes <- .lotes_consolidada_medicion(
          items, .senas_conexion_dbi(conexion), tamano_lote
        )
        for (lote in lotes) {
          agregar(
            .sql_consolidada_medicion_dbi(esq, lote, motor),
            "consolidada"
          )
        }
        for (item in items) {
          if (identical(item$via, "sql_dedicado")) {
            agregar(.sql_dedicada_medicion_dbi(item, esq, motor), "dedicada")
          }
        }
      }
      referenciales <- Filter(function(x) {
        identical(x$via, "sql_dedicado_referencial") &&
          identical(x$entidad, entidad)
      }, p$planes)
      for (item in referenciales) {
        if (!is.null(item$consulta_validacion) &&
            !item$clave_validacion %in% validadas) {
          validadas <- c(validadas, item$clave_validacion)
          agregar(item$consulta_validacion, "referencial")
        }
        agregar(item$consulta, "referencial")
      }
      bloques <- Filter(function(x) {
        identical(x$via, "bloques_r") && identical(x$entidad, entidad)
      }, p$planes)
      if (length(bloques)) {
        columnas <- unique(unlist(lapply(bloques, `[[`, "columnas_bloque"),
                                use.names = FALSE))
        columnas <- intersect(columnas, esq$campos)
        if (length(columnas)) {
          columnas_sql <- vapply(
            columnas, function(x) .citar_col_dbi(conexion, x), character(1L)
          )
          agregar(
            paste0("SELECT ", paste(columnas_sql, collapse = ", "),
                   " FROM ", esq$sql),
            "bloques"
          )
        }
      }
    }
    invisible(estado)
  }
  .with_compilador_dbi(conexion, motor, ejecutar())$compilacion
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

.sql_consolidada_medicion_dbi <- function(esq, items, motor, extra = character()) {
  agregados <- function(it) {
    j <- it$j
    c(
      sprintf("%s AS m%d_rec", .sql_suma(.sql_y(it$universo_aplicabilidad), motor), j),
      sprintf("%s AS m%d_univ", .sql_suma(.sql_y(it$universo), motor), j),
      sprintf("%s AS m%d_den", .sql_suma(.sql_y(it$evaluable), motor), j),
      if (identical(it$via, "sql_consolidado")) {
        sprintf("%s AS m%d_num", .sql_suma(.sql_y(it$evaluable, it$cumple), motor), j)
      },
      if (!is.null(it$sin_valor)) {
        sprintf("%s AS m%d_sv", .sql_suma(.sql_y(it$universo_dentro, it$sin_valor), motor), j)
      },
      sprintf(
        "%s AS m%d_apl",
        .sql_suma(.sql_y(it$universo_aplicabilidad, it$aplicable_sql %||% ""), motor), j
      ),
      sprintf(
        "%s AS m%d_comp",
        .sql_suma(.sql_y(it$universo_aplicabilidad, it$completas), motor), j
      ),
      if (!is.null(it$guarda)) sprintf("%s AS m%d_g", .sql_suma(it$guarda, motor), j),
      if (!is.null(it$na_regla)) sprintf("%s AS m%d_na", .sql_suma(it$na_regla, motor), j)
    )
  }
  paste0(
    "SELECT ", .sql_conteo_total(motor), " AS n_filas, ",
    paste(c(unlist(lapply(items, agregados)), extra), collapse = ", "),
    " FROM ", esq$sql
  )
}

# La consulta dedicada trae, en la misma sentencia, el universo, los vacios y
# las filas de la tabla. Antes solo traia `num` y `den`, y la fila se armaba con
# el universo de la consulta consolidada: dos lecturas en una fila. Medido: tres
# filas insertadas entre las dos publicaron `n_no_evaluados = -1`.
.sql_dedicada_medicion_dbi <- function(it, esq, motor, extra = character()) {
  if (is.null(it$universo_aplicabilidad)) return(it$consulta)
  conteos <- c(
    paste0(.sql_conteo_total(motor), " AS n_filas"),
    paste0(.sql_suma(.sql_y(it$universo_aplicabilidad), motor), " AS rec"),
    paste0(.sql_suma(.sql_y(it$universo), motor), " AS univ"),
    if (!is.null(it$sin_valor)) {
      paste0(.sql_suma(.sql_y(it$universo_dentro, it$sin_valor), motor), " AS sv")
    },
    extra
  )
  paste0(
    "SELECT lupa_g.num, lupa_g.den, lupa_c.* FROM (", it$consulta,
    ") lupa_g CROSS JOIN (SELECT ", paste(conteos, collapse = ", "),
    " FROM ", esq$sql, ") lupa_c"
  )
}

# Una lectura suelta del contador de una tabla, para lo que no pasa por una
# consulta de agregados. `NA` sin contador o si no se pudo leer.
.leer_contador_tabla_dbi <- function(conexion, esq, spec, contador) {
  if (!is.list(spec) || is.null(esq)) return(NA_character_)
  exprs <- .expresiones_contador_estabilidad_dbi(conexion, spec, esq$sql)
  if (!length(exprs)) return(NA_character_)
  q <- tryCatch(
    .consultar_medicion_dbi(
      conexion,
      paste0("SELECT ", paste(exprs, collapse = ", "), ", ", .sql_conteo_total(),
             " AS n_filas FROM ", esq$sql),
      contador
    ),
    error = function(e) NULL
  )
  if (!is.data.frame(q)) return(NA_character_)
  .instantanea_contador_dbi(q, spec)
}

# El valor del contador de modificaciones del motor que vio una lectura, como
# texto comparable, o `NA` si no hay contador o no se pudo leer.
.instantanea_contador_dbi <- function(datos, spec) {
  if (!is.list(spec) || !is.data.frame(datos) || nrow(datos) != 1L) {
    return(NA_character_)
  }
  n <- suppressWarnings(as.numeric(datos$n_filas[[1L]]))
  leido <- tryCatch(
    .leer_contador_estabilidad_dbi(datos, spec, n_filas = n),
    error = function(e) NULL
  )
  if (!isTRUE(leido$disponible)) return(NA_character_)
  valores <- unlist(leido$valores)
  paste0(
    spec$mecanismo %||% "contador", ":",
    paste(names(valores), valores, sep = "=", collapse = ";"),
    ";n=", format(n, scientific = FALSE)
  )
}

.consolidada_medicion_dbi <- function(conexion, esq, items, contador,
                                      motor = "sqlite",
                                      contador_estabilidad = NULL) {
  extra <- if (is.list(contador_estabilidad)) {
    .expresiones_contador_estabilidad_dbi(conexion, contador_estabilidad, esq$sql)
  } else character()
  correr <- function(grupo) {
    sql <- .sql_consolidada_medicion_dbi(esq, grupo, motor, extra)
    r <- tryCatch(
      .consultar_medicion_dbi(conexion, sql, contador),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
    if (!inherits(r, "error")) {
      # Cada item guarda las filas y el contador de SU lectura: partida la
      # consulta en mitades tras un error, la segunda mitad heredaba el conteo
      # de la primera y un cambio de la tabla entre las dos no se veia.
      instantanea <- .instantanea_contador_dbi(r, contador_estabilidad)
      lectura <- paste0("consulta-", contador$n)
      for (x in grupo) {
        r[[sprintf("m%d_nf", x$j)]] <- as.numeric(r$n_filas[[1L]])
        r[[sprintf("m%d_inst", x$j)]] <- instantanea
        r[[sprintf("m%d_lect", x$j)]] <- lectura
      }
      return(list(ok = r))
    }
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
        cbind(a$ok, b$ok[grepl("^m[0-9]+_", names(b$ok))])
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

# Lo que un item SQL suma a lo que compila el traductor antes de la consulta
# consolidada: el universo de aplicabilidad, la marca de filas completas y, en
# granularidad de entidad, la de fila sin valor. Vive en un solo sitio porque
# `conformidad_dbi()` tiene que contar exactamente lo mismo que `medir_dbi()`.
.preparar_item_medicion_dbi <- function(x, esq, U, con, motor) {
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
  # Las columnas de las que sale el valor, no solo los atributos: una metrica de
  # vigencia lee la fecha de la vigencia sin declararla como atributo, y sin
  # contar sus vacios publicaba `sin_medida` para filas que estaban vacias.
  valor <- intersect(.columnas_valor_instancia(x$instancia), esq$campos)
  if (identical(gran, "instanciaEntidad") && length(valor)) {
    x$sin_valor <- paste0(
      "(",
      paste(vapply(valor, function(a) {
        paste0("(", .columna_sql_medicion(con, esq, a, motor), " IS NULL)")
      }, character(1L)), collapse = " OR "),
      ")"
    )
  } else if (identical(gran, "instanciaEntidad")) {
    x$sin_valor <- NULL
  }
  x
}

# Los conteos de un item a partir de la consulta consolidada y, si es
# dedicado, de su consulta propia. Devuelve `list(error = )` si algo fallo.
.conteos_item_medicion_dbi <- function(r, it, con, contador, esq = NULL,
                                       motor = NULL, contador_estabilidad = NULL) {
  j <- it$j
  fallo <- if (!is.null(r$fallo)) r$fallo[as.character(j)] else NA_character_
  if (!is.na(fallo)) return(list(error = unname(fallo)))
  v <- function(s) {
    nm <- sprintf("m%d_%s", j, s)
    if (!is.null(r$ok) && nm %in% names(r$ok)) as.numeric(r$ok[[nm]]) else NA_real_
  }
  inst <- sprintf("m%d_inst", j)
  lect <- sprintf("m%d_lect", j)
  res <- list(
    n_filas = if (!is.na(v("nf"))) v("nf") else as.numeric(r$ok$n_filas),
    rec = v("rec"), univ = v("univ"), den = v("den"), num = v("num"),
    sv = v("sv"), apl = v("apl"), comp = v("comp"), g = v("g"), na = v("na"),
    instantanea = if (!is.null(r$ok) && inst %in% names(r$ok)) {
      as.character(r$ok[[inst]])
    } else NA_character_,
    lectura = if (!is.null(r$ok) && lect %in% names(r$ok)) {
      as.character(r$ok[[lect]])
    } else NA_character_
  )
  if (identical(it$via, "sql_dedicado")) {
    extra <- if (is.list(contador_estabilidad)) {
      .expresiones_contador_estabilidad_dbi(con, contador_estabilidad, esq$sql)
    } else character()
    consulta <- if (is.null(esq)) {
      it$consulta
    } else {
      .sql_dedicada_medicion_dbi(it, esq, motor, extra)
    }
    q <- tryCatch(
      .consultar_medicion_dbi(con, consulta, contador),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
    if (inherits(q, "error")) {
      msg <- if (inherits(q, "lupa_presupuesto_medicion_dbi")) {
        conditionMessage(q)
      } else {
        .mensaje_motor(q)
      }
      return(list(error = msg))
    }
    res$num <- as.numeric(q$num)
    res$den <- as.numeric(q$den)
    # La fila entera de la misma lectura: universo, vacios y filas.
    if (!is.null(esq) && all(c("n_filas", "rec", "univ") %in% names(q))) {
      res$n_filas <- as.numeric(q$n_filas)
      res$rec <- as.numeric(q$rec)
      res$univ <- as.numeric(q$univ)
      if ("sv" %in% names(q)) res$sv <- as.numeric(q$sv)
      res$instantanea <- .instantanea_contador_dbi(q, contador_estabilidad)
      res$lectura <- paste0("consulta-", contador$n)
    }
  }
  res
}

# Por que `medir_dbi()` no publica la cifra de un item ya contado: la guarda
# encontro valores que el motor no lee como R, o la regla dio NULL en alguna
# fila (R exige un logico sin NA). `NULL` si la cifra se publica.
.abstencion_conteos_medicion_dbi <- function(x, res, motor) {
  if (!is.na(res$g) && res$g > 0) {
    return(list(
      causa = "guarda",
      motivo = x$motivo_guarda %||% .motivo_guarda_dbi(motor)
    ))
  }
  if (isTRUE(x$no_logico) || (!is.na(res$na) && res$na > 0)) {
    return(list(
      causa = if (isTRUE(x$no_logico)) "no_logico" else "nulos",
      motivo = paste0(
        "El validador de ReglaIntegridadIntraEntidad debe devolver un vector ",
        "l\u00f3gico sin NA de longitud ", res$rec, "."
      )
    ))
  }
  NULL
}

# Si una regla que el motor no puede contar como R se lee por bloques en R: la
# misma condicion para lo que no se traduce, para lo que la guarda frena y para
# lo que la bateria de conformidad encontro no conforme.
.admite_bloques_medicion_dbi <- function(estrategia_no_sql, instancia, spec,
                                         columnas_bloque) {
  identical(estrategia_no_sql, "bloques") &&
    .separable_medicion_dbi(instancia, spec) &&
    length(instancia$entidad) == 1L &&
    length(columnas_bloque) > 0L
}

# Filas del universo con NULL en alguna columna de la regla. Sin el conteo, se
# supone que las hay: una no conformidad no se descarta por falta de la cifra.
.nulos_conteos_medicion_dbi <- function(res) {
  n <- res$rec - res$comp
  if (!length(n) || is.na(n)) Inf else n
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
        }
      }
      sola <- modelo(list(inst))
      # El resumen del bloque, sin huella de datos ni aviso de cuentas: lo que
      # vale es la suma de los bloques, que cierra el finalizador.
      parcial <- tryCatch(
        withCallingHandlers(
          .medir_resumen(
            sola, tb_inst, id_medicion = id_medicion, fecha = fecha,
            fecha_declarada = TRUE, aplicabilidad = NULL,
            proteger_datos_personales = proteger_datos_personales,
            columnas_personales = columnas_personales,
            validadores_personales = validadores_personales,
            interno_bloque = TRUE
          ),
          warning = function(w) {
            if (inherits(w, "lupa_aviso_no_medibles")) invokeRestart("muffleWarning")
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
        a$sin_valor <- a$sin_valor + as.numeric(fila$n_sin_valor[[1L]])
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
            # Bloque sin valores no nulos: suma su universo para el total final,
            # y todo ese universo esta vacio.
            esp <- if (identical(.unidad_universo_instancia(inst), "celda")) {
              sum(dentro_apl) * length(inst$atributos)
            } else {
              sum(dentro_apl)
            }
            a$univ <- a$univ + as.numeric(esp)
            a$sin_valor <- a$sin_valor + as.numeric(esp)
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
      if (isTRUE(tryCatch(
        .dbi("dbHasCompleted", conexion, DBI::dbHasCompleted(rs)),
        error = function(e) FALSE
      ))) break
    }
    return(acum)
  }
  if (identical(motor, "duckdb")) {
    rango <- tryCatch(
      .consultar_medicion_dbi(
        conexion,
        paste0("SELECT MIN(rowid) AS rmin, MAX(rowid) AS rmax, COUNT(*) AS n_filas FROM ",
               esq$sql),
        contador
      ),
      error = function(e) e
    )
    if (!inherits(rango, "error") && is.data.frame(rango) && nrow(rango) == 1L) {
      rmin <- as.numeric(rango$rmin[[1L]])
      rmax <- as.numeric(rango$rmax[[1L]])
      # Las filas de la tabla al empezar: los rangos de `rowid` son fijos, y una
      # tabla que cambia entre bloques se recorre a medias. Medido: con tres
      # altas recorrio 12 filas, ni las 10 del principio ni las 13 del final.
      n_inicio <- as.numeric(rango$n_filas[[1L]])
      acum <- lapply(acum, function(a) { a$n_inicio <- n_inicio; a })
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
    # Respaldo para vistas en DuckDB sin rowid: paginado LIMIT/OFFSET. Sin
    # orden ni instantanea, las paginas pueden saltear o repetir filas si la
    # vista cambia: se cuentan las filas antes de paginar para compararlas con
    # las recorridas.
    conteo <- tryCatch(
      .consultar_medicion_dbi(
        conexion, paste0("SELECT COUNT(*) AS n_filas FROM ", esq$sql), contador
      ),
      error = function(e) NULL
    )
    if (is.data.frame(conteo) && nrow(conteo) == 1L) {
      n_inicio <- as.numeric(conteo$n_filas[[1L]])
      acum <- lapply(acum, function(a) { a$n_inicio <- n_inicio; a })
    }
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
  ne <- .no_evaluados_resumen(as.double(a$univ), as.double(a$den), as.double(a$sin_valor))
  fila$n_no_evaluados[[1L]] <- ne$no
  fila$motivo_no_evaluados[[1L]] <- ne$motivo
  fila$n_sin_valor[[1L]] <- as.double(a$sin_valor)
  fila <- as.data.frame(unclass(fila), stringsAsFactors = FALSE)[.COLUMNAS_RESUMEN]
  fila <- .cerrar_fila_dbi(fila, inst, a$n_filas)
  al <- .alcance_medidas_desde_conteos(inst, a$univ, a$den, a$sin_valor)
  list(fila = fila, alcance = al)
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
#' `distintos_r`, `no_medible`, `no_medible_metodo` o `abstencion`) y estima las
#' consultas necesarias, sin leer filas de ninguna tabla. Un referencial del
#' motor suma la consulta de esquema de su tabla y una validación de su clave
#' por tabla y clave, que comparten las métricas que lo usan (en `dedicadas`);
#' `distintos_r` cuenta en `bloques`.
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
#' @param orden_texto Orden del texto en el rango de
#'   `ValoresPosiblesPorComprension` y en las comparaciones `<`, `<=`, `>` y
#'   `>=` de la fórmula de `ReglaIntegridadIntraEntidad`, como en [medir()]. Con
#'   `"idioma"` (por omisión) esas reglas sobre texto quedan `no_medible`:
#'   ningún motor reproduce el orden del locale de la sesión de R. Con
#'   `"bytes"` se traducen a una comparación por los bytes del UTF-8 —el orden
#'   por punto de código: `"Z"` antes que `"a"`, `"ñ"` después de `"z"`— y
#'   miden lo mismo que `medir(orden_texto = "bytes")`: en SQLite con `COLLATE
#'   BINARY`, si la base guarda el texto en UTF-8, y en DuckDB con `encode()`.
#'   Un texto con bytes que no son UTF-8 válido se ordena por sus bytes, como en
#'   memoria; y el rango lo deja afuera, como en memoria: en DuckDB con
#'   `try(decode())` (una versión sin `try()` lo declara `no_medible`) y en
#'   SQLite con su guarda de siempre para el texto no ASCII, porque SQLite no
#'   valida UTF-8. En PostgreSQL, MySQL, MariaDB y
#'   SQL Server la traducción está escrita pero todavía no se comparó con la
#'   medición en memoria: se declara `no_medible` con ese motivo, y con
#'   `estrategia_no_sql = "bloques"` la regla se mide en R, por bytes.
#' @param dialecto Dialecto SQL: `"auto"` (por omisión) o uno de `"sqlite"`,
#'   `"duckdb"`, `"postgresql"`, `"mysql"`, `"mariadb"`, `"sqlserver"`.
#' @param compilar Si es `TRUE`, le pide al motor que compile, sin ejecutarlas
#'   ni traer filas, las consultas SQL del plan, y publica una fila por
#'   sentencia en `attr(plan, "compilacion")`. Ver la sección Compilación.
#'
#' @return Un `data.frame` de clase `plan_medicion_dbi` con una fila por
#'   métrica instanciada (`metrica_instanciada`, `metrica`, `entidad`,
#'   `atributos`, `via`, `motivo`, `columnas_bloque`) y atributos `motor` y
#'   `consultas` (`esquema`, `consolidadas`, `dedicadas`, `bloques`), y, cuando
#'   se pide, `compilacion`.
#' @inheritSection plan_perfilado_dbi Compilación
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
                              dialecto = "auto",
                              orden_texto = c("idioma", "bytes"),
                              compilar = FALSE) {
  .fijar_numerica_c()
  if (!is.logical(compilar) || length(compilar) != 1L || is.na(compilar)) {
    .detener("`compilar` debe ser TRUE o FALSE.", call. = FALSE)
  }
  estrategia_no_sql <- .elegir_opcion(estrategia_no_sql)
  orden_texto <- .elegir_opcion(orden_texto)
  args <- .validar_argumentos_entrada_medir_dbi(
    modelo, conexion, tablas, max_valores_in, max_consultas,
    tamano_lote, bloque_filas
  )
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  .con_corrida_dbi(con, "planificar una medici\u00f3n", {
  .fijar_proteccion_motor(.protege_mensaje_motor())
  motor <- .motor_medicion_dbi(con, dialecto)
  if (isTRUE(compilar)) .exigir_compilacion_segura_dbi(con, motor)
  p <- .planificar_medicion_dbi(
    modelo = modelo, conexion = conexion, tablas = tablas,
    aplicabilidad = aplicabilidad, motor = motor,
    max_valores_in = args$max_valores_in,
    estrategia_no_sql = estrategia_no_sql,
    tamano_lote = args$tamano_lote,
    orden_texto = orden_texto
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
    dedicadas = as.integer(
      sum(plan$via %in% c("sql_dedicado", "sql_dedicado_referencial")) +
        (p$n_validaciones_referencial %||% 0L)
    ),
    bloques = as.integer(sum(plan$via %in% c("bloques_r", "distintos_r")))
  )
  guarda_duckdb <- .guarda_duckdb_contada_dbi(con, .ACCIONES_MEDICION_DBI)
  if (guarda_duckdb > 0L) {
    attr(plan, "consultas") <- c(
      attr(plan, "consultas"), transaccion = guarda_duckdb
    )
  }
  if (isTRUE(compilar)) {
    compilacion <- .compilar_plan_medicion_dbi(
      con, p, tablas, aplicabilidad, args$max_valores_in,
      args$tamano_lote, bloque_filas
    )
    # Una fila por sentencia compilada, sin completar hasta el conteo de
    # `attr(plan, "consultas")`: ese conteo incluye la consulta de la sesion,
    # que se ejecuta al planificar como siempre, y cuenta por metrica las
    # medidas por bloques, que se calculan en R sobre una sola lectura por
    # entidad -la fila `bloques`-. Una fila sin sentencia no es una sentencia.
    attr(plan, "compilacion") <- compilacion
  }
  class(plan) <- c("plan_medicion_dbi", "data.frame")
  plan
  })
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
#' Evalúa un modelo de calidad directamente en el motor SQL (SQLite, DuckDB,
#' PostgreSQL actual y 9.3, MySQL, MariaDB y SQL Server verificados según la
#' política de motores, sus verificaciones de metadatos y sus guardas) y
#' devuelve un resumen de clase `medicion_resumen` con el mismo contrato de
#' columnas que `medir(..., detalle = "resumen")`.
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
#' @param conformidad Tabla devuelta por [conformidad_dbi()] con esta misma
#'   conexión, modelo y configuración. Una regla que se cuenta en el motor deja
#'   de contarse en él sólo si la batería la encontró `no_conforme`: el motor
#'   cuenta y da distinto que R. Con `estrategia_no_sql = "bloques"` se mide
#'   leyendo sus columnas por bloques en R, como lo que no se traduce a SQL; si
#'   no, se declara `no_medible`. Si la diferencia apareció sólo con `NULL`
#'   (`condicion = "con_nulos"`), esto se aplica únicamente si la tabla real tiene
#'   `NULL` en las columnas de la regla. Una `abstencion` no quita la regla: con los datos
#'   reales decide la guarda de siempre. Si la batería es de otra conexión o de
#'   otra configuración, o si una regla no está en ella, cambió su definición,
#'   no se evaluó o su motor no está verificado, se avisa y la regla se mide como
#'   siempre. Una tabla sin la clase y los atributos de [conformidad_dbi()] se
#'   rechaza. `NULL` conserva la medición habitual.
#'
#' @return Un `data.frame` de clase `medicion_resumen` (sin heredar de
#'   `medicion`) con las columnas del resumen —también las de sus cuentas,
#'   ver [medir()]— y los atributos de `medir(detalle = "resumen")` más
#'   `ejecucion_dbi`. Cada fila sale de una sola lectura, y cada lectura de una
#'   tabla queda en `attr(x, "lecturas")`: si dos lecturas de la misma tabla en la
#'   corrida ven distinto número de filas, sus cifras quedan `no_cierran` y
#'   `medir_dbi()` avisa. Ese control ve filas agregadas o quitadas, no un
#'   `UPDATE` que no cambia el conteo; donde el motor ofrece un contador de
#'   modificaciones que lupa lee, se compara también ese contador y el control
#'   se publica como `contador_del_motor`; si no, como
#'   `solo_filas_agregadas_o_quitadas`. En SQL Server con `READ COMMITTED` por
#'   bloqueo una sola sentencia tampoco es una instantánea. La tabla de cada
#'   lectura es la que el motor resuelve para su nombre al leer su esquema, una
#'   vez por llamada —si la base es un archivo, con la ruta del archivo en
#'   lugar del nombre con que se adjuntó—: si
#'   durante la medición cambia lo que resuelve un nombre, por ejemplo se crea
#'   una tabla temporal con ese nombre, ese cambio no se ve. Dos o más
#'   lecturas de la misma tabla nunca son `una_lectura`.
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
                      instrumentar = TRUE,
                      orden_texto = c("idioma", "bytes"),
                      conformidad = NULL) {
  .fijar_numerica_c()
  estrategia_no_sql <- .elegir_opcion(estrategia_no_sql)
  orden_texto <- .elegir_opcion(orden_texto)
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
  # La forma de `columnas_personales` no depende de las tablas: se valida antes
  # de la primera consulta. Salia despues de planificar la medicion; abajo, con
  # los campos ya leidos, se mira que existan.
  .normalizar_columnas_personales(columnas_personales, NULL)
  if (is.null(id_medicion)) {
    id_medicion <- .nuevo_id_medicion(fecha)
  }
  if (!.es_texto_escalar(id_medicion)) {
    .detener("`id_medicion` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  con <- if (inherits(conexion, "coleccion_lupa")) conexion$conexion else conexion
  .con_corrida_dbi(con, "medir", {
  .fijar_proteccion_motor(proteger_datos_personales)

  inicio <- proc.time()[["elapsed"]]
  motor <- .motor_medicion_dbi(con, dialecto)
  contador <- new.env(parent = emptyenv())
  # En DuckDB la corrida ya mando las dos sentencias de su guarda de
  # transaccion, y el plan las anuncia.
  contador$n <- .guarda_duckdb_contada_dbi(con, .ACCIONES_MEDICION_DBI)
  contador$tope <- args$max_consultas
  contador$sql <- character()
  p <- .planificar_medicion_dbi(
    modelo = modelo, conexion = conexion, tablas = tablas,
    aplicabilidad = aplicabilidad, motor = motor,
    max_valores_in = args$max_valores_in,
    estrategia_no_sql = estrategia_no_sql,
    tamano_lote = args$tamano_lote,
    contador = contador,
    orden_texto = orden_texto
  )
  p$planes <- .aplicar_conformidad_plan_dbi(
    p$planes, conformidad, conexion = conexion, modelo = modelo,
    tablas = tablas, aplicabilidad = aplicabilidad,
    max_valores_in = args$max_valores_in, motor = motor,
    estrategia_no_sql = estrategia_no_sql
  )
  nombres_disponibles <- unique(unlist(
    lapply(Filter(Negate(is.null), p$esquemas), `[[`, "campos"),
    use.names = FALSE
  ))
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales,
    if (length(nombres_disponibles)) nombres_disponibles else NULL
  )

  enriquecido <- .enriquecer_planes_medicion_dbi(
    p, con, aplicabilidad, args$max_valores_in
  )
  p <- enriquecido$p
  universos <- enriquecido$universos

  senas <- .senas_conexion_dbi(con)
  resultados <- vector("list", length(p$planes))
  # Cada lectura de una tabla queda registrada con sus filas y, donde el motor
  # tiene un contador de modificaciones, con su valor: si dos lecturas de la
  # misma tabla no coinciden, las cifras de esa tabla no cierran (C5).
  lecturas <- list()
  # La tabla que resolvio el motor para cada entidad: la clave de su lectura.
  tabla_de <- function(entidad) {
    esq <- if (.es_texto_escalar(entidad)) p$esquemas[[entidad]]
    if (is.list(esq) && .es_texto_escalar(esq$identidad)) esq$identidad else NA_character_
  }
  registrar <- function(entidad, n_filas, instantanea = NA_character_,
                        lectura = paste0("consulta-", contador$n),
                        tabla = tabla_de(entidad), control = NULL) {
    if (is.null(entidad) || is.na(entidad) || !length(n_filas)) return(invisible(NULL))
    if (is.null(control)) {
      control <- if (is.na(instantanea)) {
        "solo_filas_agregadas_o_quitadas"
      } else "contador_del_motor"
    }
    lecturas[[length(lecturas) + 1L]] <<- .lectura(
      id_medicion, entidad, suppressWarnings(as.numeric(n_filas)), instantanea,
      control, lectura = lectura, tabla = tabla
    )
    invisible(NULL)
  }
  # Los referenciales en R, como tablas referidas de la via `distintos_r`.
  referidas_memoria <- .referidas_memoria(modelo, unique(c(
    names(p$esquemas), unlist(lapply(modelo$metricas, `[[`, "entidad"))
  )))
  contadores <- lapply(names(p$esquemas), function(e) {
    esq <- p$esquemas[[e]]
    if (is.null(esq)) return(NULL)
    tryCatch(
      .especificacion_contador_estabilidad_dbi(
        con, esq$sql, esq$sql, tipos_motor = esq$tipos, campos = esq$campos
      ),
      error = function(err) NULL
    )
  })
  names(contadores) <- names(p$esquemas)
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
      r <- .consolidada_medicion_dbi(con, p$esquemas[[e]], lote, contador,
                                     motor = motor,
                                     contador_estabilidad = contadores[[e]])
      for (it in lote) {
        resultados[[it$j]] <- .conteos_item_medicion_dbi(
          r, it, con, contador, esq = p$esquemas[[e]], motor = motor,
          contador_estabilidad = contadores[[e]]
        )
        if (is.null(resultados[[it$j]]$error)) {
          registrar(e, resultados[[it$j]]$n_filas, resultados[[it$j]]$instantanea,
                    resultados[[it$j]]$lectura)
        }
      }
    }
  }

  filas <- list()
  coberturas <- list()
  alcances <- list()
  vias_finales <- vapply(p$planes, `[[`, character(1L), "via")
  pendientes_bloques <- list()
  validaciones_referencial <- new.env(parent = emptyenv())
  agregar_salida <- function(s) {
    for (l in s$lecturas) {
      registrar(l$entidad, l$n_filas,
                instantanea = if (is.null(l$instantanea)) NA_character_ else l$instantanea,
                lectura = l$lectura,
                tabla = if (.es_texto_escalar(l$tabla)) l$tabla else tabla_de(l$entidad),
                control = l$control)
    }
    if (!is.null(s$fila) && !is.null(s$n_filas)) {
      s$fila <- .cerrar_fila_dbi(
        s$fila, instancia, s$n_filas, s$n_filas_ref %||% NA_real_
      )
    }
    if (!is.null(s$fila)) filas[[length(filas) + 1L]] <<- s$fila
    if (!is.null(s$cobertura)) coberturas[[length(coberturas) + 1L]] <<- s$cobertura
    if (!is.null(s$alcance)) alcances[[length(alcances) + 1L]] <<- s$alcance
  }

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
    # El referencial del motor (`R/referencial-dbi.R`). Si la guarda encuentra
    # texto que el motor no escribe como R y se pidio `bloques`, se compara en R
    # sobre los valores distintos.
    if (isTRUE(x$referencial_motor) &&
        identical(x$via, "sql_dedicado_referencial")) {
      s <- .ejecutar_referencial_dbi(
        x, con, contador, id_medicion, fecha, validaciones_referencial
      )
      if (isTRUE(s$guarda)) {
        # La consulta que encontro la guarda tambien leyo las dos tablas.
        previas <- s$lecturas
        if (isTRUE(x$admite_distintos)) {
          x$via <- "distintos_r"
          vias_finales[[j]] <- "distintos_r"
          s <- .ejecutar_distintos_referencial_dbi(
            x, con, contador, id_medicion, fecha, p$esquemas, universos, motor
          )
        } else {
          s <- list(cobertura = .no_medible_dbi(
            instancia, id_medicion, fecha, s$motivo, en_base = TRUE
          ))
        }
        s$lecturas <- c(previas, s$lecturas)
      }
      agregar_salida(s)
      next
    }
    if (identical(x$via, "distintos_r")) {
      x$referida_memoria <- referidas_memoria[[instancia$nombre]]
      agregar_salida(.ejecutar_distintos_referencial_dbi(
        x, con, contador, id_medicion, fecha, p$esquemas, universos, motor
      ))
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
          x$motivo_guarda %||% .motivo_guarda_dbi(motor),
          en_base = TRUE
        ))
        next
      }
      univ <- as.numeric(q$univ)
      den <- as.numeric(q$den)
      num <- as.numeric(q$num)
      sv <- as.numeric(q$sin_valor)
      if (!is.null(x[["conformidad_nulos"]]) && !isTRUE(sv == 0)) {
        cob(.no_medible_dbi(
          instancia, id_medicion, fecha, x[["conformidad_nulos"]], en_base = TRUE
        ))
        next
      }
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
      ne <- .no_evaluados_resumen(univ, den, sv)
      registrar(x$spec$entidad_dep, q$n_filas)
      registrar(x$spec$entidad_ref, q$n_filas_ref)
      fila_ref <- .fila_resumen(
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
        n_no_evaluados = ne$no,
        motivo_no_evaluados = ne$motivo,
        n_sin_valor = sv,
        entidad_referida = x$spec$entidad_ref
      )
      filas[[length(filas) + 1L]] <- .cerrar_fila_dbi(
        fila_ref, instancia, q$n_filas, q$n_filas_ref
      )
      next
    }
    res <- resultados[[j]]
    if (!is.null(res$error)) {
      cob(.no_medible_dbi(instancia, id_medicion, fecha, res$error, en_base = TRUE))
      next
    }
    abstencion <- .abstencion_conteos_medicion_dbi(x, res, motor)
    # Lo que el motor no cuenta como R con estos datos -la guarda encontro
    # valores que no lee igual, o la bateria vio una no conformidad que solo
    # aparece con NULL y la tabla los tiene- se lee por bloques si se pidio, y si
    # no, queda no_medible. Una no conformidad sin NULL ya se resolvio al planear.
    motivo_g <- if (identical(abstencion$causa, "guarda")) {
      abstencion$motivo
    } else if (is.null(abstencion) && !is.null(x[["conformidad_nulos"]]) &&
               .nulos_conteos_medicion_dbi(res) > 0) {
      x[["conformidad_nulos"]]
    }
    if (!is.null(motivo_g)) {
      if (.admite_bloques_medicion_dbi(estrategia_no_sql, instancia, x$spec,
                                       x$columnas_bloque)) {
        x$via <- "bloques_r"
        x$motivo <- motivo_g
        vias_finales[[j]] <- "bloques_r"
        pendientes_bloques[[length(pendientes_bloques) + 1L]] <- x
        next
      }
      cob(.no_medible_dbi(instancia, id_medicion, fecha, motivo_g, en_base = TRUE))
      next
    }
    if (!is.null(abstencion)) {
      cob(.no_medible_dbi(
        instancia, id_medicion, fecha, abstencion$motivo, en_base = FALSE
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
    unidad <- .unidad_universo_instancia(instancia)
    base_univ <- if (identical(x$spec$nodo, "no_ausente")) res$univ else res$rec
    esperadas <- if (identical(unidad, "celda")) {
      base_univ * length(instancia$atributos)
    } else {
      base_univ
    }
    sv <- if (!length(.columnas_valor_instancia(instancia))) NA_real_ else res$sv
    ne <- .no_evaluados_resumen(esperadas, res$den, sv)
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
    fila_sql <- .fila_resumen(
      instancia, id_medicion, fecha, x$spec$entidad, atributo, objeto,
      resultado = res$num / res$den,
      agregacion = "ratio",
      tipo_resultado = "real",
      numerador = res$num,
      denominador = res$den,
      n_universo = esperadas,
      n_no_evaluados = ne$no,
      motivo_no_evaluados = ne$motivo,
      n_sin_valor = sv,
      unidad = unidad
    )
    filas[[length(filas) + 1L]] <- .cerrar_fila_dbi(fila_sql, instancia, res$n_filas)
  }

  # Ejecutar metricas por bloques agrupadas por entidad (una pasada por entidad).
  if (length(pendientes_bloques)) {
    entidades_b <- unique(vapply(pendientes_bloques, `[[`, character(1L), "entidad"))
    for (eb in entidades_b) {
      items_b <- Filter(function(x) identical(x$entidad, eb), pendientes_bloques)
      # La lectura por bloques no lleva agregados donde poner el contador del
      # motor: si hay contador, se lee antes y despues, y la lectura vale como
      # instantanea solo si los dos coinciden.
      antes <- .leer_contador_tabla_dbi(con, p$esquemas[[eb]], contadores[[eb]], contador)
      acums <- .ejecutar_bloques_entidad_medicion(
        conexion = con, esq = p$esquemas[[eb]], items = items_b,
        motor = motor, bloque_filas = args$bloque_filas,
        aplicabilidad = aplicabilidad, id_medicion = id_medicion, fecha = fecha,
        proteger_datos_personales = proteger_datos_personales,
        columnas_personales = columnas_personales,
        validadores_personales = validadores_personales,
        contador = contador
      )
      despues <- .leer_contador_tabla_dbi(con, p$esquemas[[eb]], contadores[[eb]], contador)
      instantanea_b <- if (!is.na(antes) && identical(antes, despues)) antes else if (
        !is.na(antes) && !is.na(despues)) paste0("cambio:", antes, "->", despues) else NA_character_
      if (length(acums)) {
        a1 <- acums[[1L]]
        # Un cursor es una sentencia; los rangos de `rowid` y las paginas son
        # varias, y se registran como dos lecturas -el conteo del principio y lo
        # recorrido-: sin contador, solo se comparan sus filas.
        paginada <- !is.null(a1$n_inicio) && !is.na(a1$n_inicio)
        if (paginada) {
          registrar(eb, a1$n_inicio, instantanea_b, paste0("bloques-inicio:", eb))
        }
        registrar(eb, a1$n_filas,
                  if (!is.na(instantanea_b) && startsWith(instantanea_b, "cambio:")) {
                    antes
                  } else instantanea_b,
                  paste0("bloques:", eb))
        if (!is.na(instantanea_b) && startsWith(instantanea_b, "cambio:") && !paginada) {
          registrar(eb, a1$n_filas, instantanea_b, paste0("bloques-cierre:", eb))
        }
      }
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
  lecturas <- .unir_lecturas(lecturas)
  resultado <- .estado_cuentas_lecturas(resultado, lecturas)
  attr(resultado, "lecturas") <- lecturas
  .avisar_cuentas_resumen(resultado)
  if (length(coberturas)) {
    cobertura <- do.call(
      rbind, lapply(coberturas, .ubicar_cobertura_metricas, modelo)
    )
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
  # La misma huella que en memoria: la definicion no depende de donde se mide.
  # Salvo la de un referencial del motor, que es la tabla de ESTA base.
  attr(resultado, "sello_metricas") <- .sello_metricas(
    .fuente_referenciales_dbi(modelo, con), id_medicion, aplicabilidad,
    orden_texto, .lector_esquemas(p$esquemas)
  )
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
  })
}
