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
.via_clave_primaria <- function(conexion) {
  clase <- paste(class(conexion), collapse = " ")
  if (grepl("SQLite", clase, fixed = TRUE)) return("pragma")
  # `ROracle` llama a su clase `OraConnection`, sin la palabra "Oracle", asi que
  # buscarla por el nombre completo no la encuentra.
  if (grepl("^Ora|Oracle", clase)) return("all_constraints")
  # Por ODBC la clase es la misma para todos los motores -`OdbcConnection`- y el
  # unico que sabe cual es de verdad es el propio controlador.
  if (grepl("Odbc|ODBC", clase)) {
    motor <- tryCatch(
      as.character(DBI::dbGetInfo(conexion)$dbms.name),
      error = function(e) ""
    )
    if (length(motor) && grepl("oracle", motor, ignore.case = TRUE)) {
      return("all_constraints")
    }
    if (length(motor) && grepl("sqlite", motor, ignore.case = TRUE)) {
      return("pragma")
    }
  }
  "information_schema"
}

# Cada entrada arma el SQL con el esquema y la tabla ya separados.
.consultas_clave_primaria <- function() {
  list(
    list(
      nombre = "information_schema",
      motores = "PostgreSQL, MySQL, MariaDB, SQL Server",
      sql = function(esquema, tabla) {
        paste0(
          "SELECT k.column_name, k.ordinal_position ",
          "FROM information_schema.table_constraints t ",
          "JOIN information_schema.key_column_usage k ",
          "ON t.constraint_name = k.constraint_name ",
          "AND t.table_schema = k.table_schema ",
          "AND t.table_name = k.table_name ",
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
          "SELECT name AS column_name, pk AS ordinal_position ",
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
          "SELECT c.column_name, c.position AS ordinal_position ",
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

# Devuelve `list(columnas, fuente, motivo)`. `columnas` es `character(0)` cuando
# la tabla no declara clave primaria -que es una respuesta, no un fallo- y
# `motivo` dice por que no se pudo leer cuando no se pudo.
.clave_primaria_dbi <- function(conexion, tabla, esquema = NA_character_,
                                presupuesto = NULL) {
  vacio <- function(fuente, motivo) {
    list(columnas = character(), fuente = fuente, motivo = motivo)
  }
  if (!length(tabla) || is.na(tabla) || !nzchar(tabla)) {
    return(vacio(NA_character_, "No se indico la tabla."))
  }
  nombre_via <- .via_clave_primaria(conexion)
  vias <- .consultas_clave_primaria()
  via <- vias[[which(vapply(vias, function(v) v$nombre == nombre_via, logical(1L)))]]
  respuesta <- .consultar_dbi(conexion, via$sql(esquema, tabla), presupuesto)
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
    return(list(columnas = character(), fuente = via$nombre, motivo = NA_character_))
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
      return(list(columnas = character(), fuente = via$nombre, motivo = NA_character_))
    }
    orden <- partes[order(posicion[partes])]
    return(list(
      columnas = as.character(datos[[columna]])[orden], fuente = via$nombre,
      motivo = NA_character_
    ))
  }
  list(
    columnas = as.character(datos[[columna]]), fuente = via$nombre,
    motivo = NA_character_
  )
}
