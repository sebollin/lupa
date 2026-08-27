# Leer la clave primaria del catalogo en vez de adivinarla.
#
# Sobre un data.frame no hay a quien preguntarle cual es la clave, y por eso
# `sugerir_clave()` ordena candidatas para que decida quien conoce la tabla. En
# una base relacional esa pregunta **ya tiene respuesta escrita**: la clave
# primaria esta declarada en el catalogo del motor. Ahi no hay nada que
# ordenar ni que sugerir, hay que leerla.
#
# Se intenta en orden: primero la forma estandar -`information_schema`, que
# soportan PostgreSQL, MySQL, MariaDB y SQL Server-, despues las formas propias
# de los motores que no la traen. Si ninguna funciona **no se inventa nada**: se
# devuelve el motivo, y quien llama lo declara en `cobertura_diagnosticos`.

# La via se elige por el controlador, no probando una tras otra. Probar hasta
# acertar gastaria un numero de consultas que depende del motor, y
# `plan_perfilado_dbi()` promete **exactamente** cuantas se emiten: es la misma
# razon por la que la sonda del desvio gasta siempre dos aunque acierte en la
# primera. Con el controlador se resuelve en una, y el plan la puede contar.
#
# `information_schema` es el estandar y lo trae la mayoria; SQLite y Oracle no,
# y DuckDB lo trae pero tambien tiene el suyo. Un controlador desconocido cae al
# estandar, que es la apuesta con mas chance de andar.
.motor_clave_primaria <- function(conexion) {
  clase <- paste(class(conexion), collapse = " ")
  if (grepl("Ora|Oracle", clase)) return("oracle")
  if (grepl("SQLite", clase, fixed = TRUE)) return("sqlite")
  if (grepl("DuckDB|duckdb", clase)) return("duckdb")
  if (grepl("MariaDB|RMariaDB", clase)) return("mariadb")
  if (grepl("MySQL|RMySQL", clase)) return("mysql")
  if (grepl("Pq|Postgres", clase, ignore.case = TRUE)) {
    return("postgresql")
  }
  if (grepl("SQLServer|SQL Server|Microsoft", clase, ignore.case = TRUE)) {
    return("sqlserver")
  }
  if (grepl("Odbc|ODBC", clase)) {
    motor <- tryCatch(
      as.character(DBI::dbGetInfo(conexion)$dbms.name),
      error = function(e) ""
    )
    if (length(motor) && grepl("oracle", motor, ignore.case = TRUE)) {
      return("oracle")
    }
    if (length(motor) && grepl("sqlite", motor, ignore.case = TRUE)) {
      return("sqlite")
    }
    if (length(motor) && grepl("duckdb", motor, ignore.case = TRUE)) {
      return("duckdb")
    }
    if (length(motor) && grepl("mariadb", motor, ignore.case = TRUE)) {
      return("mariadb")
    }
    if (length(motor) && grepl("mysql", motor, ignore.case = TRUE)) {
      return("mysql")
    }
    if (length(motor) && grepl("postgres", motor, ignore.case = TRUE)) {
      return("postgresql")
    }
    if (length(motor) && grepl("sql server|microsoft", motor, ignore.case = TRUE)) {
      return("sqlserver")
    }
  }
  "desconocido"
}

.via_clave_primaria <- function(conexion) {
  motor <- .motor_clave_primaria(conexion)
  if (identical(motor, "sqlite")) return("pragma")
  if (identical(motor, "oracle")) return("all_constraints")
  if (identical(motor, "duckdb")) return("duckdb_constraints")
  "information_schema"
}

# Cada entrada arma el SQL con el esquema y la tabla ya separados.
.consultas_clave_primaria <- function() {
  list(
    list(
      nombre = "information_schema",
      motores = "PostgreSQL, MySQL, MariaDB, SQL Server",
      sql = function(esquema, tabla, motor = "desconocido") {
        estado <- if (motor %in% c("postgresql", "mysql")) {
          paste0(
            ", t.enforced AS constraint_enforced",
            if (identical(motor, "postgresql")) {
              ", pc.convalidated AS constraint_validated"
            } else {
              ""
            }
          )
        } else {
          ""
        }
        joins <- if (identical(motor, "postgresql")) {
          paste(
            "JOIN pg_catalog.pg_class pc_rel",
            "ON pc_rel.relname = t.table_name",
            "JOIN pg_catalog.pg_namespace pc_ns",
            "ON pc_ns.oid = pc_rel.relnamespace",
            "AND pc_ns.nspname = t.table_schema",
            "JOIN pg_catalog.pg_constraint pc",
            "ON pc.conrelid = pc_rel.oid",
            "AND pc.conname = t.constraint_name",
            "AND pc.contype = 'p'"
          )
        } else {
          ""
        }
        paste0(
          "SELECT k.column_name, k.ordinal_position", estado, " ",
          "FROM information_schema.table_constraints t ",
          "JOIN information_schema.key_column_usage k ",
          "ON t.constraint_name = k.constraint_name ",
          "AND t.table_schema = k.table_schema ",
          "AND t.table_name = k.table_name ",
          joins, " ",
          "WHERE t.constraint_type = 'PRIMARY KEY' ",
          "AND t.table_name = ", .texto_sql_clave(tabla),
          if (!is.na(esquema)) {
            paste0(" AND t.table_schema = ", .texto_sql_clave(esquema))
          } else {
            ""
          },
          " ORDER BY k.ordinal_position"
        )
      }
    ),
    list(
      nombre = "pragma",
      motores = "SQLite",
      sql = function(esquema, tabla) {
        # `PRAGMA table_info` devuelve una columna `pk` con la posicion dentro
        # de la clave, y 0 para las que no son parte. No acepta esquema.
        #
        # Se piden TODAS las columnas y no solo las de la clave, aunque despues
        # haya que filtrar: sobre una tabla que no existe SQLite devuelve cero
        # filas **sin error**, asi que filtrando en el SQL "no declara clave" y
        # "no pude preguntar" llegan iguales. Pidiendo todo, cero filas
        # significa que la tabla no esta y filas sin ningun `pk > 0` significa
        # que no tiene clave. Son dos respuestas distintas.
        paste0(
          "SELECT name AS column_name, pk AS ordinal_position, ",
          "\"notnull\" AS column_notnull ",
          "FROM pragma_table_info(", .texto_sql_clave(tabla), ") ",
          "ORDER BY pk"
        )
      }
    ),
    list(
      nombre = "duckdb_constraints",
      motores = "DuckDB",
      sql = function(esquema, tabla) {
        paste0(
          "SELECT UNNEST(constraint_column_names) AS column_name, ",
          "1 AS ordinal_position FROM duckdb_constraints() ",
          "WHERE constraint_type = 'PRIMARY KEY' ",
          "AND table_name = ", .texto_sql_clave(tabla),
          if (!is.na(esquema)) {
            paste0(" AND schema_name = ", .texto_sql_clave(esquema))
          } else {
            ""
          }
        )
      }
    ),
    list(
      nombre = "all_constraints",
      motores = "Oracle",
      sql = function(esquema, tabla) {
        paste0(
          "SELECT c.column_name, c.position AS ordinal_position, ",
          "t.status AS constraint_status, ",
          "t.validated AS constraint_validated ",
          "FROM all_constraints t JOIN all_cons_columns c ",
          "ON t.constraint_name = c.constraint_name ",
          "AND t.owner = c.owner ",
          "WHERE t.constraint_type = 'P' ",
          "AND t.table_name = ", .texto_sql_clave(toupper(tabla)),
          if (!is.na(esquema)) {
            paste0(" AND t.owner = ", .texto_sql_clave(toupper(esquema)))
          } else {
            ""
          },
          " ORDER BY c.position"
        )
      }
    )
  )
}

# Un literal de texto para el catalogo. No se usa `DBI::dbQuoteString()` porque
# esta funcion tiene que poder armar el SQL sin conexion para poder probarse, y
# lo unico que viaja son nombres de tabla y de esquema que ya se validaron.
.texto_sql_clave <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

# El estado bruto se conserva aparte de la conclusion. Una entrada visible en
# el catalogo es la fuente; solo un estado que diga que la restriccion esta
# aplicada y validada permite llamarla garantia. Si el catalogo no expone esos
# campos, no se los completa con una suposicion.
.campo_clave <- function(datos, candidatos) {
  nombres <- names(datos)
  posicion <- match(candidatos, nombres)
  if (all(is.na(posicion))) {
    posicion <- match(tolower(candidatos), tolower(nombres))
  }
  posicion <- posicion[!is.na(posicion)]
  if (!length(posicion)) return(NULL)
  datos[[posicion[[1L]]]]
}

.estado_clave <- function(valor, tipo = c("si_no", "oraculo")) {
  tipo <- match.arg(tipo)
  if (is.null(valor) || !length(valor)) return(NA)
  if (is.logical(valor)) return(if (is.na(valor[[1L]])) NA else valor[[1L]])
  texto <- toupper(trimws(as.character(valor[[1L]])))
  if (is.na(texto) || !nzchar(texto)) return(NA)
  afirmativos <- if (tipo == "oraculo") "ENABLED" else c("YES", "TRUE", "1")
  negativos <- if (tipo == "oraculo") "DISABLED" else c("NO", "FALSE", "0")
  if (texto %in% afirmativos) return(TRUE)
  if (texto %in% negativos) return(FALSE)
  if (tipo == "oraculo" && texto == "VALIDATED") return(TRUE)
  if (tipo == "oraculo" && texto %in% c("NOT VALIDATED", "NOT_VALIDATED")) {
    return(FALSE)
  }
  NA
}

.garantia_clave_primaria <- function(datos, via, motor) {
  enforced <- .campo_clave(datos, c("constraint_enforced", "enforced"))
  status <- .campo_clave(datos, c("constraint_status", "status"))
  validated <- .campo_clave(datos, c("constraint_validated", "validated"))
  aplicada <- if (!is.null(status)) {
    .estado_clave(status, "oraculo")
  } else {
    .estado_clave(enforced, "si_no")
  }
  validada <- .estado_clave(validated, "oraculo")
  estado <- list(
    visible = TRUE,
    aplicada = aplicada,
    validada = validada,
    consultado = c(
      enforced = !is.null(enforced), status = !is.null(status),
      validated = !is.null(validated)
    ),
    valores = list(
      enforced = if (is.null(enforced)) NA_character_ else as.character(enforced[[1L]]),
      status = if (is.null(status)) NA_character_ else as.character(status[[1L]]),
      validated = if (is.null(validated)) NA_character_ else as.character(validated[[1L]])
    )
  )

  if (identical(via, "all_constraints")) {
    garantia <- if (isTRUE(aplicada) && isTRUE(validada)) {
      "garantizada"
    } else if (identical(aplicada, FALSE) || identical(validada, FALSE)) {
      "declarada_no_garantizada"
    } else {
      "desconocida"
    }
    return(list(garantia = garantia, estado = estado))
  }

  # PostgreSQL y MySQL publican ENFORCED en TABLE_CONSTRAINTS. PostgreSQL
  # tambien permite consultar la validacion en pg_constraint; MySQL no tiene
  # un segundo estado para PRIMARY KEY y documenta ENFORCED=YES para ella. El
  # resto de motores de esta via no expone un estado comparable y queda
  # desconocido.
  if (identical(via, "information_schema") && identical(motor, "postgresql")) {
    garantia <- if (identical(aplicada, TRUE) && isTRUE(validada)) {
      "garantizada"
    } else if (identical(aplicada, FALSE) || identical(validada, FALSE)) {
      "declarada_no_garantizada"
    } else {
      "desconocida"
    }
    return(list(garantia = garantia, estado = estado))
  }
  if (identical(via, "information_schema") && identical(motor, "mysql")) {
    garantia <- if (identical(aplicada, TRUE)) {
      "garantizada"
    } else if (identical(aplicada, FALSE)) {
      "declarada_no_garantizada"
    } else {
      "desconocida"
    }
    return(list(garantia = garantia, estado = estado))
  }
  list(garantia = "desconocida", estado = estado)
}

# Devuelve `list(columnas, fuente, motivo, garantia, estado)`. `columnas` es
# `character(0)` cuando la tabla no declara clave primaria -que es una
# respuesta, no un fallo- y `motivo` dice por que no se pudo leer cuando no se
# pudo. `garantia` nunca convierte la mera visibilidad en una validacion.
.clave_primaria_dbi <- function(conexion, tabla, esquema = NA_character_,
                                presupuesto = NULL) {
  vacio <- function(fuente, motivo, garantia = "desconocida",
                    visible = NA) {
    list(
      columnas = character(), fuente = fuente, motivo = motivo,
      garantia = garantia,
      estado = list(
        visible = visible, aplicada = NA, validada = NA,
        consultado = c(enforced = FALSE, status = FALSE, validated = FALSE),
        valores = list(enforced = NA_character_, status = NA_character_,
                        validated = NA_character_)
      )
    )
  }
  if (!length(tabla) || is.na(tabla) || !nzchar(tabla)) {
    return(vacio(NA_character_, "No se indico la tabla."))
  }
  nombre_via <- .via_clave_primaria(conexion)
  motor <- .motor_clave_primaria(conexion)
  vias <- .consultas_clave_primaria()
  via <- vias[[which(vapply(vias, function(v) v$nombre == nombre_via, logical(1L)))]]
  sql <- if (identical(via$nombre, "information_schema")) {
    via$sql(esquema, tabla, motor = motor)
  } else {
    via$sql(esquema, tabla)
  }
  respuesta <- .consultar_dbi(conexion, sql, presupuesto)
  if (!isTRUE(respuesta$ok)) {
    return(vacio(via$nombre, paste0(via$nombre, ": ", respuesta$motivo)))
  }
  datos <- respuesta$datos
  if (!inherits(datos, "data.frame") || !nrow(datos)) {
    if (identical(via$nombre, "pragma")) {
      # Cero filas pidiendo todas las columnas: la tabla no esta.
      return(vacio(via$nombre, paste0(
        via$nombre, ": la tabla no existe o no es visible, ",
        "asi que no se pudo preguntar por su clave."
      )))
    }
    # En las otras vias, cero filas es la respuesta del catalogo: no hay clave
    # declarada. Que una tabla inexistente conteste igual es un limite conocido
    # de preguntar en una sola consulta, y por eso quien llama desde
    # `perfilar_dbi()` ya comprobo que la tabla existe.
    return(vacio(
      via$nombre, motivo = NA_character_, garantia = "no_declarada",
      visible = FALSE
    ))
  }
  columna <- if ("column_name" %in% names(datos)) {
    "column_name"
  } else {
    names(datos)[[1L]]
  }
  posicion <- if ("ordinal_position" %in% names(datos)) {
    suppressWarnings(as.numeric(datos$ordinal_position))
  } else {
    rep(1, nrow(datos))
  }
  if (identical(via$nombre, "pragma")) {
    # `pk` vale 0 en las columnas que no son parte de la clave.
    partes <- which(is.finite(posicion) & posicion > 0)
    if (!length(partes)) {
      return(vacio(
        via$nombre, motivo = NA_character_, garantia = "no_declarada",
        visible = FALSE
      ))
    }
    orden <- partes[order(posicion[partes])]
    garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
    return(list(
      columnas = as.character(datos[[columna]])[orden], fuente = via$nombre,
      motivo = NA_character_, garantia = garantia$garantia,
      estado = garantia$estado
    ))
  }
  garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
  list(
    columnas = as.character(datos[[columna]]), fuente = via$nombre,
    motivo = NA_character_, garantia = garantia$garantia,
    estado = garantia$estado
  )
}
