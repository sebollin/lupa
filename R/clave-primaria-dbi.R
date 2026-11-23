# Leer la clave primaria del catalogo en vez de adivinarla.
#
# Sobre un data.frame no hay a quien preguntarle cual es la clave, y por eso
# `sugerir_clave()` ordena candidatas para que decida quien conoce la tabla. En
# una base relacional esa pregunta **ya tiene respuesta escrita**: la clave
# primaria esta declarada en el catalogo del motor. Ahi no hay nada que
# ordenar ni que sugerir, hay que leerla.
#
# Se usa la forma propia de PostgreSQL y, para el resto, primero la forma
# estandar -`information_schema`, que soportan MySQL y SQL Server-; despues las
# formas propias de los motores que no la traen. Si ninguna funciona **no se
# inventa nada**: se devuelve el motivo, y quien llama lo declara en
# `cobertura_diagnosticos`.

# La via se elige por el controlador, no probando una tras otra. Probar hasta
# acertar gastaria un numero de consultas que depende del motor, y
# `plan_perfilado_dbi()` promete **exactamente** cuantas se emiten: es la misma
# razon por la que la sonda del desvio gasta siempre dos aunque acierte en la
# primera. Con el controlador se resuelve en una, y el plan la puede contar.
#
# `information_schema` es el estandar y lo trae la mayoria; MariaDB usa
# `SHOW INDEX` porque su vista estandar oculta la clave a quien solo tiene
# SELECT. PostgreSQL tiene una via propia por la misma razon. SQLite y Oracle
# no lo traen, y DuckDB tambien tiene el suyo. Un controlador desconocido cae
# al estandar, que es la apuesta con mas chance de andar.
.motor_clave_primaria <- function(conexion) {
  clase <- paste(class(conexion), collapse = " ")
  if (grepl("Ora|Oracle", clase)) return("oracle")
  if (grepl("SQLite", clase, fixed = TRUE, useBytes = TRUE)) return("sqlite")
  if (grepl("DuckDB|duckdb", clase)) return("duckdb")
  if (grepl("MariaDB|RMariaDB", clase)) return("mariadb")
  if (grepl("MySQL|RMySQL", clase)) return("mysql")
  if (grepl(.regex_sin_caja("Pq|Postgres"), clase)) {
    return("postgresql")
  }
  if (grepl(.regex_sin_caja("SQLServer|SQL Server|Microsoft"), clase)) {
    return("sqlserver")
  }
  if (grepl("Odbc|ODBC", clase)) {
    motor <- tryCatch(
      as.character(.info_dbi_corrida(conexion)$dbms.name),
      error = function(e) ""
    )
    if (length(motor) && grepl(.regex_sin_caja("oracle"), motor)) {
      return("oracle")
    }
    if (length(motor) && grepl(.regex_sin_caja("sqlite"), motor)) {
      return("sqlite")
    }
    if (length(motor) && grepl(.regex_sin_caja("duckdb"), motor)) {
      return("duckdb")
    }
    if (length(motor) && grepl(.regex_sin_caja("mariadb"), motor)) {
      return("mariadb")
    }
    if (length(motor) && grepl(.regex_sin_caja("mysql"), motor)) {
      return("mysql")
    }
    if (length(motor) && grepl(.regex_sin_caja("postgres"), motor)) {
      return("postgresql")
    }
    if (length(motor) && grepl(.regex_sin_caja("sql server|microsoft"), motor)) {
      return("sqlserver")
    }
  }
  "desconocido"
}

.via_clave_primaria <- function(conexion) {
  motor <- .motor_clave_primaria(conexion)
  if (identical(motor, "mariadb")) return("show_index")
  if (identical(motor, "postgresql")) return("pg_catalog")
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
      motores = "MySQL, SQL Server",
      sql = function(esquema, tabla, motor = "desconocido") {
        posicion <- if (identical(motor, "mysql")) {
          "CAST(k.ordinal_position AS SIGNED)"
        } else {
          "k.ordinal_position"
        }
        estado <- if (identical(motor, "mysql")) {
          ", t.enforced AS constraint_enforced"
        } else {
          ""
        }
        # Sin esquema, filtrar solo por nombre trae la tabla homonima de TODAS
        # las bases y sus claves se fusionan. Reproducido contra MySQL 8.4 el
        # 2026-08-29: con `lupa.choque` (clave `id`) y `otra.choque` (clave
        # `p,q`), la respuesta era `columnas = id, p, q` con garantia
        # `garantizada`. Un nombre sin calificar significa "la del esquema por
        # omision de esta sesion", que es de donde se leen los datos.
        propio <- if (identical(motor, "mysql")) {
          " AND t.table_schema = DATABASE() "
        } else if (identical(motor, "sqlserver")) {
          # SQL Server resuelve un nombre sin calificar en DOS pasos: primero el
          # esquema por omision del usuario, y si no esta ahi, `dbo`.
          #
          # Filtrar por `SCHEMA_NAME()` daba un FALSO NEGATIVO -una tabla que
          # vive solo en `dbo` se lee, pero el catalogo devolvia cero filas-. La
          # correccion de eso, `IN (SCHEMA_NAME(), 'dbo')`, resolvia ese caso e
          # introducia uno PEOR: con homonimas en los dos esquemas y la clave
          # declarada SOLO en `dbo`, el catalogo devuelve UNA fila -la de `dbo`-,
          # `.clave_ambigua()` no se dispara porque no hay dos que comparar, y el
          # paquete publicaba la clave de una tabla que no es la que lee. Medido
          # contra SQL Server 2022 el 2026-08-30: sobre una tabla cuyas columnas
          # son `y, dato` publicaba clave primaria en la columna `x`, que ahi ni
          # existe, y con filas repetidas. Es el invariante roto: informa como
          # medido lo que no midio.
          #
          # La forma honesta no es replicar la precedencia sino PREGUNTARLA.
          # `OBJECT_ID` resuelve el nombre con las mismas reglas que usa la
          # consulta de datos y `OBJECT_SCHEMA_NAME` dice en que esquema cayo,
          # asi que el filtro apunta exactamente a la relacion que se mide. El
          # comentario anterior decia que esto no se escribia porque el equipo no
          # tenia controladores ODBC con que medirlo; ya los tiene, y esta
          # verificado contra el motor real, que era la condicion.
          paste0(
            " AND t.table_schema = OBJECT_SCHEMA_NAME(OBJECT_ID(",
            .texto_sql_clave(tabla), ")) "
          )
        } else {
          # Un motor que no se conoce no recibe una funcion inventada: se pide
          # el esquema para poder DECLARAR la ambiguedad si aparece, que es lo
          # que hace `.clave_ambigua()`.
          ""
        }
        paste0(
          "SELECT k.column_name, ", posicion, " AS ordinal_position", estado, ", ",
          "t.table_schema AS constraint_esquema, ",
          "t.constraint_name AS constraint_nombre ",
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
            propio
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
        # Y en la misma consulta, el `CREATE TABLE` de la tabla: es lo unico que
        # dice el cotejamiento de cada columna (`COLLATE NOCASE`), y con el la
        # comparacion de entregas sabe si dos conteos de distintos de un texto
        # cuentan igual (`.cotejamientos_sql_tabla()`). No es otra consulta.
        paste0(
          "SELECT name AS column_name, pk AS ordinal_position, ",
          "\"notnull\" AS column_notnull, ",
          "(SELECT sql FROM sqlite_master WHERE type IN ('table', 'view') ",
          "AND name = ", .texto_sql_clave(tabla), ") AS lupa_sql_tabla ",
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
          "t.validated AS constraint_validated, ",
          # Oracle tambien admite restricciones diferibles, con el mismo efecto
          # que en PostgreSQL: dentro de una transaccion puede haber duplicados
          # que se rechazaran recien al confirmar. Se mira DEFERRABLE y no
          # DEFERRED, porque el segundo es el estado inicial y `SET CONSTRAINTS`
          # puede diferir despues una que empezo inmediata.
          "t.deferrable AS constraint_diferible ",
          "FROM all_constraints t JOIN all_cons_columns c ",
          "ON t.constraint_name = c.constraint_name ",
          "AND t.owner = c.owner ",
          "WHERE t.constraint_type = 'P' ",
          "AND t.table_name = ",
          .texto_sql_clave(.mayusculas_identificador_sql(tabla)),
          if (!is.na(esquema)) {
            paste0(
              " AND t.owner = ",
              .texto_sql_clave(.mayusculas_identificador_sql(esquema))
            )
          } else {
            ""
          },
          " ORDER BY c.position"
        )
      }
    ),
    list(
      nombre = "pg_catalog",
      motores = "PostgreSQL",
      sql = function(esquema, tabla) {
        # `information_schema.table_constraints` no muestra las restricciones
        # de una tabla cuyo unico privilegio del usuario es SELECT. En los
        # catalogos del sistema la restriccion y sus columnas se relacionan por
        # OID y `conkey`, de modo que tampoco hace falta adivinar el nombre de
        # la restriccion ni recorrer los datos de la tabla.
        paste0(
          "SELECT a.attname AS column_name, k.ordinal_position, ",
          "TRUE AS constraint_enforced, ",
          "c.convalidated AS constraint_validated, ",
          # Una consulta sin ONLY incluye a los descendientes. Con herencia
          # tradicional la clave del padre NO gobierna sus filas; con
          # particionado declarativo SI, porque el motor exige que la clave
          # incluya las columnas de particion. `pg_inherits` registra las dos
          # relaciones, asi que hace falta `relkind` para distinguirlas: 'p' es
          # una tabla particionada. Todo en la misma consulta, sin ida y vuelta.
          "(SELECT count(*) FROM pg_catalog.pg_inherits i ",
          "WHERE i.inhparent = r.oid) AS constraint_descendientes, ",
          "r.relkind AS constraint_relkind, ",
          # La unicidad la impone el indice que respalda la restriccion, no la
          # fila de `pg_constraint`. Leerlo es preferir el mecanismo a la
          # declaracion, que es la tesis del paquete, y sale gratis: el `JOIN`
          # va en la misma consulta. `conindid` apunta a ese indice.
          #
          # Se exige evidencia POSITIVA de los cinco campos, no la ausencia de un
          # FALSE. Medido: un `CREATE UNIQUE INDEX CONCURRENTLY` que falla deja
          # un indice con `indisunique = t` pero `indisvalid = f` e
          # `indisready = f`, y ese indice NO impone unicidad -la tabla acepta un
          # duplicado nuevo y queda con 4 validos y 2 distintos-.
          #
          # PostgreSQL impide que ese indice respalde una clave primaria
          # -"index is not valid" al intentarlo-, asi que por DDL soportado no se
          # llega. Se comprueba igual porque la asimetria manda: un falso
          # negativo pierde una optimizacion, un falso positivo publica un numero
          # exacto equivocado.
          "i.indisprimary AS constraint_indice_primario, ",
          "i.indisunique AS constraint_indice_unico, ",
          "i.indisvalid AS constraint_indice_valido, ",
          "i.indisready AS constraint_indice_listo, ",
          "i.indislive AS constraint_indice_vivo, ",
          # Una restriccion diferible puede estar violada dentro de una
          # transaccion abierta: el catalogo la informa validada igual.
          "c.condeferrable AS constraint_diferible ",
          "FROM pg_catalog.pg_constraint c ",
          "JOIN pg_catalog.pg_class r ON r.oid = c.conrelid ",
          "JOIN pg_catalog.pg_namespace n ON n.oid = r.relnamespace ",
          "LEFT JOIN pg_catalog.pg_index i ON i.indexrelid = c.conindid ",
          # `generate_subscripts` y no `unnest(...) WITH ORDINALITY`: la segunda
          # se incorporo en PostgreSQL 9.4 y contra 9.3 la consulta entera falla
          # con "syntax error at or near WITH ORDINALITY". Medido contra un
          # servidor 9.3.25: con la sintaxis vieja el paquete devolvia
          # `columnas = ()` y garantia `desconocida` sobre tablas que SI tienen
          # clave primaria, o sea que no avisaba, callaba. `generate_subscripts`
          # existe desde antes y conserva el orden de una clave compuesta.
          "JOIN LATERAL generate_subscripts(c.conkey, 1) ",
          "AS k(ordinal_position) ON TRUE ",
          "JOIN pg_catalog.pg_attribute a ON a.attrelid = r.oid ",
          "AND a.attnum = c.conkey[k.ordinal_position] ",
          "WHERE c.contype = 'p' ",
          "AND r.relname = ", .texto_sql_clave(tabla),
          if (!is.na(esquema)) {
            paste0(" AND n.nspname = ", .texto_sql_clave(esquema))
          } else {
            # Sin esquema, filtrar solo por nombre trae TODAS las tablas que se
            # llamen asi, en todos los esquemas, y sus claves se fusionan en una
            # sola respuesta. Reproducido el 2026-08-29 contra PostgreSQL 16: con
            # `public.dup` (clave `id`), `s1.dup` (clave `a,b`) y `s2.dup` (clave
            # `x`), `perfilar_dbi(con, "dup")` publicaba `columnas = id, a, x, b`
            # con garantia `garantizada`. Esa clave no existe en ninguna tabla, y
            # se presentaba como verificada sobre la que se midio.
            #
            # `pg_table_is_visible()` devuelve cierto solo para la relacion que
            # alcanzaria una consulta SIN calificar, resolviendo el `search_path`
            # igual que lo hace el motor: es la misma tabla de la que se leen los
            # datos. Existe desde mucho antes de 9.3, asi que no mueve el piso.
            " AND pg_catalog.pg_table_is_visible(r.oid) "
          },
          " ORDER BY k.ordinal_position"
        )
      }
    ),
    list(
      nombre = "show_index",
      motores = "MariaDB",
      sql = function(esquema, tabla) {
        identificador <- function(x) {
          paste0("`", gsub("`", "``", as.character(x), fixed = TRUE), "`")
        }
        origen <- if (is.na(esquema)) {
          identificador(tabla)
        } else {
          paste0(identificador(esquema), ".", identificador(tabla))
        }
        paste0("SHOW INDEX FROM ", origen)
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

.mayusculas_identificador_sql <- function(x) {
  chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
         as.character(x))
}

.error_permiso_clave <- function(motivo, reconocido = NA_character_) {
  .motivo_de_permiso_dbi(motivo, reconocido)
}

# El estado bruto se conserva aparte de la conclusion. Una entrada visible en
# el catalogo es la fuente; en los motores que publican esos estados, solo una
# restriccion aplicada y validada permite llamarla garantia. Si el catalogo no
# expone esos campos, no se los completa con una suposicion. SQLite agrega dos
# ejes: la unicidad que su PRIMARY KEY da entre valores no nulos y la ausencia
# de nulos que solo se puede garantizar cuando `notnull` lo dice para cada
# columna.
.campo_clave <- function(datos, candidatos) {
  nombres <- names(datos)
  posicion <- .indice_nombre(candidatos, nombres)
  if (all(is.na(posicion))) {
    posicion <- match(
      .normalizacion_minusculas_vector(.nombres_para_operar(candidatos)),
      .normalizacion_minusculas_vector(.nombres_para_operar(nombres))
    )
  }
  posicion <- posicion[!is.na(posicion)]
  if (!length(posicion)) return(NULL)
  datos[[posicion[[1L]]]]
}

.estado_clave <- function(valor, tipo = c("si_no", "oraculo", "diferible")) {
  tipo <- match.arg(tipo)
  if (is.null(valor) || !length(valor)) return(NA)
  if (is.logical(valor)) return(if (is.na(valor[[1L]])) NA else valor[[1L]])
  texto <- .mayusculas_ascii(trimws(as.character(valor[[1L]])))
  if (is.na(texto) || !nzchar(texto)) return(NA)
  # PostgreSQL devuelve `condeferrable` como logico; Oracle publica el texto
  # DEFERRABLE o NOT DEFERRABLE en `ALL_CONSTRAINTS`. El mismo hecho, dos
  # representaciones, un solo interprete.
  afirmativos <- switch(tipo,
    oraculo = "ENABLED",
    diferible = c("DEFERRABLE", "YES", "TRUE", "1"),
    c("YES", "TRUE", "1")
  )
  negativos <- switch(tipo,
    oraculo = "DISABLED",
    diferible = c("NOT DEFERRABLE", "NOT_DEFERRABLE", "NO", "FALSE", "0"),
    c("NO", "FALSE", "0")
  )
  if (texto %in% afirmativos) return(TRUE)
  if (texto %in% negativos) return(FALSE)
  if (tipo == "oraculo" && texto == "VALIDATED") return(TRUE)
  if (tipo == "oraculo" && texto %in% c("NOT VALIDATED", "NOT_VALIDATED")) {
    return(FALSE)
  }
  NA
}

# `%||%` no sirve aca: llego a base recien en R 4.4.0 y el paquete declara
# `R (>= 4.2.0)`. Usarlo compilaria en esta maquina y fallaria en el piso que
# DESCRIPTION promete, que es la clase de defecto que ningun check local ve.
# Devuelve el motivo si las filas no se pueden atribuir a UNA sola relacion, y
# `NULL` si no hay ambiguedad. Se mira lo que el catalogo mismo publica: el
# esquema y el nombre de la restriccion. Cuando la via no los trae -`pragma` de
# SQLite, `SHOW INDEX` de MariaDB- no hay nada que comparar y tampoco hay riesgo:
# las dos preguntan por una tabla, no por un nombre suelto.
.clave_ambigua <- function(datos) {
  campos <- intersect(
    c("constraint_esquema", "constraint_nombre"), names(datos)
  )
  if (!length(campos) || !nrow(datos)) return(NULL)
  # Se arma un cuadro propio en vez de seleccionar columnas del que llego. No es
  # estetica: `data.table` cambia la semantica de `[` segun los imports de quien
  # llama -`cedta`-, asi que `datos[, campos, drop = FALSE]` sobre una tabla de
  # entrada no significa siempre lo mismo. Una prueba estructural cuenta esos
  # sitios y no deja que crezcan.
  partes <- lapply(campos, function(campo) as.character(datos[[campo]]))
  names(partes) <- campos
  distintas <- sum(!duplicated(as.data.frame(partes, stringsAsFactors = FALSE)))
  if (distintas <= 1L) return(NULL)
  esquemas <- if ("constraint_esquema" %in% campos) {
    unique(as.character(datos$constraint_esquema))
  } else {
    character()
  }
  paste0(
    "El cat\u00e1logo devolvi\u00f3 la clave primaria de ", distintas,
    if (distintas == 1L) " relaci\u00f3n distinta para ese nombre" else
      " relaciones distintas para ese nombre",
    if (length(esquemas) > 1L) {
      paste0(" -esquemas: ", paste(esquemas, collapse = ", "), "-")
    } else {
      ""
    },
    ". Fusionarlas dar\u00eda una clave que no existe en ninguna, as\u00ed que no se ",
    "publica ninguna: calificar el nombre con su esquema resuelve cu\u00e1l es."
  )
}

.motivo_garantia <- function(garantia) {
  motivo <- garantia$motivo
  if (is.null(motivo) || !length(motivo)) NA_character_ else as.character(motivo)[1L]
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
  # En PostgreSQL una consulta sin `ONLY` incluye a las tablas que heredan, y la
  # restriccion del padre no gobierna esas filas. Medido: sobre una tabla con un
  # hijo que repite un valor, el catalogo informa `contype = p` y
  # `convalidated = t`, y la consulta que corre el paquete da 6001 valores con
  # 6000 distintos. Publicar "garantizada" ahi seria afirmar sobre un universo
  # que no es el que se midio.
  descendientes <- .campo_clave(datos, "constraint_descendientes")
  relkind <- .campo_clave(datos, "constraint_relkind")
  # `pg_inherits` registra tanto la herencia tradicional como el particionado
  # declarativo, y solo la primera deja filas fuera del alcance de la clave.
  # Medido contra PostgreSQL 16: una tabla `relkind = 'r'` con un hijo que
  # repite un valor da 6001 validos y 6000 distintos; una `relkind = 'p'` con
  # dos particiones da 19999 y 19999. En PostgreSQL anterior a la version 10 no
  # existe 'p', y ahi toda descendencia es herencia, que es lo correcto.
  es_particionada <- !is.null(relkind) &&
    identical(as.character(relkind[[1L]]), "p")
  hay_descendientes <- !is.null(descendientes) &&
    isTRUE(suppressWarnings(as.numeric(descendientes[[1L]]) > 0)) &&
    !es_particionada
  # Una restriccion `DEFERRABLE` puede estar violada mientras una transaccion
  # sigue abierta, y el catalogo la sigue informando validada. Medido: dentro de
  # una transaccion que inserta un duplicado, `convalidated = t` y la consulta da
  # 3 validos con 2 distintos. No se puede saber desde aca si hay una
  # transaccion con violaciones pendientes, y el costo de equivocarse es
  # publicar un exacto falso, asi que la garantia no se afirma.
  diferible <- .campo_clave(datos, "constraint_diferible")
  es_diferible <- !is.null(diferible) &&
    isTRUE(.estado_clave(diferible, "diferible"))
  # Si el indice que respalda la clave no es unico, no hay unicidad aunque la
  # restriccion figure validada. Por DDL normal no se llega a ese estado -al
  # adjuntar una particion el motor crea el indice unico solo-, asi que esto es
  # defensa ante un catalogo alterado a mano o un estado anormal. Cuando la
  # columna no viene -otros motores- no cambia nada.
  campos_indice <- c(
    "constraint_indice_primario", "constraint_indice_unico",
    "constraint_indice_valido", "constraint_indice_listo",
    "constraint_indice_vivo"
  )
  leidos <- lapply(campos_indice, function(nombre) .campo_clave(datos, nombre))
  names(leidos) <- campos_indice
  # Si ninguno vino -otros motores no traen estas columnas- no se exige nada.
  hay_datos_indice <- any(!vapply(leidos, is.null, logical(1L)))
  indice_no_unico <- hay_datos_indice && !all(vapply(
    leidos,
    function(x) !is.null(x) && isTRUE(.estado_clave(x, "si_no")),
    logical(1L)
  ))
  estado <- list(
    visible = TRUE,
    aplicada = aplicada,
    validada = validada,
    universo_incluye_descendientes = hay_descendientes,
    relacion_particionada = es_particionada,
    restriccion_diferible = es_diferible,
    indice_no_unico = indice_no_unico,
    unicidad = NA_character_,
    unicidad_aplica_a = NA_character_,
    ausencia_de_nulos = NA_character_,
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

  if (identical(via, "pragma")) {
    posicion <- .campo_clave(datos, c("ordinal_position", "pk"))
    posicion <- suppressWarnings(as.numeric(posicion))
    partes <- which(is.finite(posicion) & posicion > 0)
    notnull <- .campo_clave(datos, c("column_notnull", "notnull"))
    estado$unicidad <- "garantizada"
    estado$unicidad_aplica_a <- "valores no nulos"
    if (length(partes) && !is.null(notnull) && length(notnull) >= max(partes)) {
      estados_notnull <- vapply(
        notnull[partes], .estado_clave, logical(1L), tipo = "si_no"
      )
      if (length(estados_notnull) && all(!is.na(estados_notnull)) &&
          all(estados_notnull)) {
        estado$ausencia_de_nulos <- "garantizada"
      } else {
        estado$ausencia_de_nulos <- "no_verificada"
      }
    } else {
      estado$ausencia_de_nulos <- "no_verificada"
    }
    garantia <- if (identical(estado$ausencia_de_nulos, "garantizada")) {
      "garantizada"
    } else {
      "desconocida"
    }
    return(list(garantia = garantia, estado = estado))
  }

  if (identical(via, "all_constraints")) {
    # Oracle admite restricciones diferibles igual que PostgreSQL, y con el
    # mismo efecto: dentro de una transaccion puede haber duplicados que se
    # rechazaran recien al confirmar. Se mira DEFERRABLE y no DEFERRED, porque
    # `SET CONSTRAINTS` puede diferir despues una que empezo inmediata.
    garantia <- if (es_diferible) {
      "declarada_no_garantizada"
    } else if (isTRUE(aplicada) && isTRUE(validada)) {
      "garantizada"
    } else if (identical(aplicada, FALSE) || identical(validada, FALSE)) {
      "declarada_no_garantizada"
    } else {
      "desconocida"
    }
    return(list(garantia = garantia, estado = estado))
  }

  # PostgreSQL publica la validacion en pg_constraint. La via directa agrega
  # TRUE para `aplicada`: la fila filtrada por `contype = 'p'` es la declaracion
  # de una PRIMARY KEY. MySQL no tiene un segundo estado para PRIMARY KEY y
  # documenta ENFORCED=YES para ella. El resto de motores de esta via no expone
  # un estado comparable y queda desconocido.
  #
  # `convalidated` es una guarda DEFENSIVA, no la compensacion de una diferencia
  # entre motores. Medido contra PostgreSQL 16: `PRIMARY KEY ... NOT VALID` y
  # `UNIQUE ... NOT VALID` los rechaza el motor -"constraints cannot be marked
  # NOT VALID"-, igual que MySQL. La columna existe en el catalogo para todos los
  # tipos de restriccion, pero una clave primaria no puede llegar a `FALSE` por
  # DDL normal: se comprueba por si aparece un estado transitorio o anormal.
  if (identical(via, "pg_catalog") && identical(motor, "postgresql")) {
    garantia <- if (hay_descendientes || es_diferible || indice_no_unico) {
      # La restriccion existe y es valida, pero no gobierna lo que se va a
      # medir: o el universo es otro -descendientes-, o puede estar violada en
      # este instante -diferible-. Declarada, no garantizada aca.
      "declarada_no_garantizada"
    } else if (identical(aplicada, TRUE) && isTRUE(validada)) {
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
  # Ultimo recurso: la via leyo la clave pero el motor no expone un estado
  # comparable. Decir "desconocida" y callar POR QUE deja a quien perfila sin
  # saber si le falta un privilegio, si el paquete no cubre su motor, o si es un
  # limite del catalogo. Medido el 2026-08-29 sobre los contenedores: MySQL 8.4
  # trae `ENFORCED` en `information_schema.TABLE_CONSTRAINTS` y MariaDB 11.8 NO
  # -sus columnas son CONSTRAINT_CATALOG, CONSTRAINT_SCHEMA, CONSTRAINT_NAME,
  # TABLE_SCHEMA, TABLE_NAME y CONSTRAINT_TYPE-, asi que sobre MariaDB no hay
  # nada que preguntar y la respuesta honesta es "no se puede saber por aca".
  faltantes <- names(which(!estado$consultado))
  list(
    garantia = "desconocida", estado = estado,
    motivo = paste0(
      "La clave est\u00e1 declarada y sus columnas se leyeron del cat\u00e1logo, pero ",
      "el motor no permite confirmar que la restricci\u00f3n est\u00e9 aplicada: la v\u00eda ",
      "`", via, "` sobre `", motor, "` no expone ",
      if (length(faltantes)) paste0("`", paste(faltantes, collapse = "`, `"), "`")
      else "un estado comparable",
      ". No es un privilegio que falte ni un defecto de la tabla: es lo que ",
      "ese cat\u00e1logo publica."
    )
  )
}

# Una respuesta vacia solo permite decir "no declarada" cuando la via puede
# ver el catalogo completo de una tabla que ya es accesible para quien perfila.
# Una consulta fallida se representa antes con `visible = NA`.
#
# Que motores filtran `information_schema` por permisos NO se deduce: se midio,
# el 2026-08-27, contra contenedores reales, creando un rol con solo `SELECT`
# sobre una tabla con clave primaria y contando las filas que devuelve
# `information_schema.table_constraints`.
#
#   MySQL 8        el rol restringido ve 1  -> visible
#   MariaDB 11     el rol restringido ve 0  -> usa `show_index`
#   PostgreSQL 16  el rol restringido ve 0  -> por eso su via es `pg_catalog`
#
# MariaDB y MySQL parecen el mismo motor y aca no lo son. Suponerlo habria
# hecho que `lupa` afirmara "no hay clave declarada" sobre una tabla que si la
# tiene, que es exactamente el defecto que este cambio corrige.
#
# SQL Server YA se midio y dejo de ser ambiguo. Con un rol de solo `SELECT`
# sobre tablas con clave simple, compuesta y sin clave, la vista devuelve 1, 1 y
# 0. Lo sostienen dos mediciones independientes sobre dos versiones distintas del
# motor -un contenedor 2022 y un servidor 2016 con la credencial real de un
# perfilado-, no la documentacion: MariaDB documenta lo mismo que MySQL y
# midiendo devuelve lo contrario.
.catalogo_clave_visible <- function(via, motor) {
  if (via %in% c("pg_catalog", "pragma", "duckdb_constraints")) {
    return(TRUE)
  }
  if (identical(via, "all_constraints")) return(TRUE)
  if (identical(via, "show_index") && identical(motor, "mariadb")) {
    return(TRUE)
  }
  # `information_schema.table_constraints` NO se comporta igual en motores que
  # aceptan la misma consulta. Medido con un rol de solo SELECT sobre una tabla
  # con clave primaria declarada:
  #
  #   MySQL 8        -> 1 fila   (visible)
  #   SQL Server     -> 1 fila   (visible)
  #   MariaDB 11     -> 0 filas  (por eso usa `show_index`)
  #   PostgreSQL     -> 0 filas  (por eso usa `pg_catalog`)
  #
  # SQL Server esta sostenido por dos mediciones independientes: un contenedor
  # 2022 local y un servidor 2016 con la credencial real de un perfilado.
  identical(via, "information_schema") &&
    motor %in% c("mysql", "sqlserver")
}

#' Lee la clave primaria desde el catalogo del motor sin recorrer la tabla.
#'
#' La salida separa una clave no declarada, una consulta fallida y un catalogo
#' que no mostro filas con visibilidad ambigua. En SQLite, `estado` separa la
#' unicidad entre valores no nulos de la ausencia de nulos.
#'
#' @keywords internal
#' @noRd
#
# Devuelve `list(columnas, fuente, motivo, garantia, estado)`. `columnas` es
# `character(0)` cuando el catalogo visible no declara clave primaria o cuando
# no se pudo resolver la visibilidad; `motivo` distingue ambos casos y los
# errores de consulta. `garantia` nunca convierte la mera visibilidad en una
# validacion.
# El estado de una clave que NO se publica. Vive aparte porque lo usan dos
# sitios -la salida vacia de cada via y la red que descarta una clave ajena- y
# dos copias de la misma lista de doce campos se desincronizan solas.
.estado_clave_vacio <- function(visible = NA) {
  list(
    visible = visible, aplicada = NA, validada = NA,
    unicidad = NA_character_, unicidad_aplica_a = NA_character_,
    ausencia_de_nulos = NA_character_,
    consultado = c(enforced = FALSE, status = FALSE, validated = FALSE),
    valores = list(enforced = NA_character_, status = NA_character_,
                   validated = NA_character_)
  )
}

# Descarta ENTERA una clave cuyas columnas no estan en la tabla medida. No se
# recortan las columnas ajenas para publicar el resto: eso daria una clave que
# ningun catalogo declara, que es peor que no publicar ninguna.
.clave_de_otra_relacion <- function(catalogo, ajenas) {
  catalogo$columnas <- character()
  catalogo$garantia <- "desconocida"
  catalogo$motivo <- paste0(
    "El cat\u00e1logo declar\u00f3 una clave primaria sobre ",
    paste(ajenas, collapse = ", "),
    if (length(ajenas) > 1L) ", que no est\u00e1n " else ", que no est\u00e1 ",
    "entre las columnas de la tabla medida: la respuesta pertenece a otra ",
    "relaci\u00f3n con el mismo nombre en otro esquema. No se publica clave; ",
    "calificar el nombre con su esquema resuelve cu\u00e1l es."
  )
  catalogo$estado <- .estado_clave_vacio(visible = TRUE)
  catalogo
}

.clave_primaria_dbi <- function(conexion, tabla, esquema = NA_character_,
                                presupuesto = NULL) {
  vacio <- function(fuente, motivo, garantia = "desconocida",
                    visible = NA) {
    list(
      columnas = character(), fuente = fuente, motivo = motivo,
      garantia = garantia,
      estado = .estado_clave_vacio(visible = visible)
    )
  }
  if (!length(tabla) || is.na(tabla) || !nzchar(tabla)) {
    return(vacio(NA_character_, "No se indic\u00f3 la tabla."))
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
  respuesta <- .consultar_dbi(
    conexion, sql, presupuesto, etapa = "clave_primaria"
  )
  if (!isTRUE(respuesta$ok)) {
    return(vacio(
      via$nombre, paste0(
        via$nombre, ": ", respuesta$motivo,
        if (.error_permiso_clave(respuesta$motivo, respuesta$reconocido)) {
          " El mensaje parece relacionado con permisos, pero eso no alcanza para afirmarlo."
        } else {
          ""
        }
      ),
      # `visible = FALSE` es una afirmacion positiva de invisibilidad, y
      # deducirla del TEXTO del mensaje de error es exactamente la inferencia
      # que este paquete no hace: los textos cambian por motor, por version y
      # por idioma del servidor. Hasta que haya un codigo comprobado por
      # adaptador -SQLSTATE o el codigo del motor-, lo unico defendible es que
      # la visibilidad no se pudo establecer. La pista de texto sigue usandose,
      # pero solo para redactar el motivo, no para llenar el campo.
      visible = NA
    ))
  }
  datos <- respuesta$datos
  resultado <- .interpretar_clave_primaria_dbi(datos, via, motor, vacio)
  # El `CREATE TABLE` que trajo la consulta de SQLite viaja en un atributo: no
  # es parte de la clave, y `perfilar_dbi()` lo lee de ahi.
  if (inherits(datos, "data.frame") && nrow(datos) &&
      "lupa_sql_tabla" %in% names(datos)) {
    attr(resultado, "sql_tabla") <- as.character(datos$lupa_sql_tabla[[1L]])
  }
  resultado
}

.interpretar_clave_primaria_dbi <- function(datos, via, motor, vacio) {
  if (!inherits(datos, "data.frame") || !nrow(datos)) {
    if (identical(via$nombre, "pragma")) {
      # Cero filas pidiendo todas las columnas: la tabla no esta.
      return(vacio(via$nombre, paste0(
        via$nombre, ": la tabla no existe o no es visible, ",
        "as\u00ed que no se pudo preguntar por su clave."
      ), visible = NA))
    }
    if (.catalogo_clave_visible(via$nombre, motor)) {
      # Que una tabla inexistente conteste igual es un limite conocido de
      # preguntar en una sola consulta, y por eso quien llama desde
      # `perfilar_dbi()` ya comprobo que la tabla existe.
      return(vacio(
        via$nombre, motivo = NA_character_, garantia = "no_declarada",
        visible = TRUE
      ))
    }
    # Una vista de catalogo con visibilidad restringida puede devolver cero
    # filas tanto para una tabla sin clave como para una clave que el usuario no
    # puede ver. No se elige una de esas explicaciones.
    return(vacio(
      via$nombre,
      motivo = paste0(
        via$nombre, ": no devolvi\u00f3 filas; la clave puede no estar declarada ",
        "o no ser visible para esta credencial."
      ),
      garantia = "desconocida", visible = NA
    ))
  }
  if (identical(via$nombre, "show_index")) {
    nombre_clave <- .campo_clave(datos, c("key_name"))
    if (is.null(nombre_clave)) {
      return(vacio(
        via$nombre,
        "show_index: la consulta no devolvi\u00f3 el campo `Key_name`.",
        visible = NA
      ))
    }
    primarias <- which(
      !is.na(nombre_clave) &
        .mayusculas_ascii(trimws(as.character(nombre_clave))) == "PRIMARY"
    )
    if (!length(primarias)) {
      return(vacio(
        via$nombre, motivo = NA_character_, garantia = "no_declarada",
        visible = TRUE
      ))
    }
    datos <- datos[primarias, , drop = FALSE]
  }
  # Red de seguridad, por encima de los filtros de cada via: si las filas del
  # catalogo pertenecen a MAS DE UNA relacion, fusionarlas produce una clave que
  # no existe en ninguna. Los filtros de arriba resuelven el caso conocido -el
  # nombre sin calificar-, pero un motor que no se conoce, o un catalogo que
  # sorprenda, llegarian igual hasta aca. Ante lo que no se puede atribuir a una
  # sola tabla, la respuesta es no publicar clave y decir por que.
  ambigua <- .clave_ambigua(datos)
  if (!is.null(ambigua)) {
    return(vacio(
      via$nombre, motivo = ambigua, garantia = "desconocida", visible = TRUE
    ))
  }
  columna <- .campo_clave(datos, c("column_name"))
  if (is.null(columna)) columna <- datos[[1L]]
  posicion <- .campo_clave(datos, c("ordinal_position", "seq_in_index"))
  if (is.null(posicion)) posicion <- rep(1, nrow(datos))
  posicion <- suppressWarnings(as.numeric(posicion))
  if (identical(via$nombre, "pragma")) {
    # `pk` vale 0 en las columnas que no son parte de la clave.
    partes <- which(is.finite(posicion) & posicion > 0)
    if (!length(partes)) {
      return(vacio(
        via$nombre, motivo = NA_character_, garantia = "no_declarada",
        visible = TRUE
      ))
    }
    orden <- partes[.orden_por_bytes(posicion[partes])]
    garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
    return(list(
      columnas = as.character(columna)[orden], fuente = via$nombre,
      motivo = .motivo_garantia(garantia), garantia = garantia$garantia,
      estado = garantia$estado
    ))
  }
  if (identical(via$nombre, "show_index")) {
    orden <- .orden_por_bytes(posicion, na.last = TRUE)
    garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
    return(list(
      columnas = as.character(columna)[orden], fuente = via$nombre,
      motivo = .motivo_garantia(garantia), garantia = garantia$garantia,
      estado = garantia$estado
    ))
  }
  garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
  list(
    columnas = as.character(columna), fuente = via$nombre,
    motivo = .motivo_garantia(garantia), garantia = garantia$garantia,
    estado = garantia$estado
  )
}

# ---- Garantias completas del catalogo ------------------------------------
#
# Extiende la lectura de clave primaria para relevar en una sola consulta por
# controlador (dos en MariaDB, donde SHOW INDEX no admite UNION):
#   1. clave primaria (conservando intacto el contrato de `meta$clave`);
#   2. restricciones e indices unicos (con semantica de NULL y parciales);
#   3. claves foraneas (y si el motor las valida);
#   4. columnas NOT NULL (combinadas con las lecturas ya existentes de esquema).
#
# Nunca prueba vias hasta acertar: la via sale del controlador, de modo que
# `plan_perfilado_dbi()` cuenta exactamente las consultas emitidas.

.via_catalogo_garantias <- function(conexion) {
  motor <- .motor_clave_primaria(conexion)
  if (identical(motor, "postgresql")) return("pg_catalog")
  if (identical(motor, "sqlserver")) return("sys_catalog")
  if (identical(motor, "mysql")) return("information_schema_mysql")
  if (identical(motor, "mariadb")) return("show_index_mariadb")
  if (identical(motor, "sqlite")) return("pragma")
  if (identical(motor, "duckdb")) return("duckdb_catalog")
  if (identical(motor, "oracle")) return("all_constraints")
  "information_schema"
}

.claves_unicas_vacias_dbi <- function() {
  data.frame(
    nombre = character(),
    origen = character(),
    columnas = I(list()),
    es_parcial = logical(),
    filtro = character(),
    semantica_null = character(),
    todas_not_null = logical(),
    grado = character(),
    motivo = character(),
    stringsAsFactors = FALSE
  )
}

.claves_foraneas_vacias_dbi <- function() {
  data.frame(
    nombre = character(),
    columnas = I(list()),
    tabla_referenciada = character(),
    columnas_referenciadas = I(list()),
    validada = logical(),
    grado = character(),
    motivo = character(),
    stringsAsFactors = FALSE
  )
}

.not_null_vacio_dbi <- function() {
  data.frame(
    columna = character(),
    not_null = logical(),
    grado = character(),
    n_blancos = numeric(),
    not_null_con_blancos = logical(),
    motivo = character(),
    stringsAsFactors = FALSE
  )
}

.estado_lectura_catalogo_vacio <- function(motivo = NA_character_) {
  item <- function(leido = FALSE, m = motivo) {
    list(leido = leido, motivo = m)
  }
  list(
    clave_primaria = item(FALSE, motivo),
    claves_unicas = item(FALSE, motivo),
    claves_foraneas = item(FALSE, motivo),
    not_null = item(FALSE, motivo)
  )
}

.extraer_filtro_where_sql <- function(sql_indice) {
  if (!length(sql_indice) || is.na(sql_indice[[1L]])) return(NA_character_)
  texto <- trimws(as.character(sql_indice[[1L]]))
  if (!nzchar(texto)) return(NA_character_)
  pat_where <- .regex_sin_caja("\\bWHERE\\b")
  m <- regexpr(pat_where, texto, perl = TRUE)
  if (m < 1L) return(NA_character_)
  parte <- trimws(substring(texto, m + attr(m, "match.length")))
  parte <- sub(";\\s*$", "", parte, perl = TRUE)
  if (nzchar(parte)) parte else NA_character_
}

.extraer_columnas_parentesis_sql <- function(sql_texto) {
  if (!length(sql_texto) || is.na(sql_texto[[1L]])) return(character())
  texto <- as.character(sql_texto[[1L]])
  inicio <- regexpr("(", texto, fixed = TRUE, useBytes = TRUE)
  if (inicio < 1L) return(character())
  resto <- substring(texto, inicio + 1L)
  chars <- strsplit(resto, "", fixed = TRUE)[[1L]]
  nivel <- 1L
  cierre <- ""
  buf <- character()
  for (ch in chars) {
    if (nzchar(cierre)) {
      buf <- c(buf, ch)
      if (ch == cierre) cierre <- ""
      next
    }
    if (ch %in% c("\"", "'", "`")) {
      cierre <- ch
      buf <- c(buf, ch)
      next
    }
    if (ch == "(") {
      nivel <- nivel + 1L
      buf <- c(buf, ch)
      next
    }
    if (ch == ")") {
      nivel <- nivel - 1L
      if (nivel == 0L) break
      buf <- c(buf, ch)
      next
    }
    buf <- c(buf, ch)
  }
  interior <- paste(buf, collapse = "")
  if (!nzchar(trimws(interior))) return(character())
  partes <- strsplit(interior, ",", fixed = TRUE)[[1L]]
  vapply(partes, function(p) {
    limpio <- trimws(p)
    pat_ord <- paste0(
      "\\s+(",
      .regex_sin_caja("ASC"), "|", .regex_sin_caja("DESC"), "|",
      .regex_sin_caja("NULLS"), "\\s+", .regex_sin_caja("FIRST"), "|",
      .regex_sin_caja("NULLS"), "\\s+", .regex_sin_caja("LAST"),
      ").*$"
    )
    limpio <- sub(pat_ord, "", limpio, perl = TRUE)
    limpio <- trimws(limpio)
    .quitar_comillas_identificador(limpio)
  }, character(1L), USE.NAMES = FALSE)
}

.parsear_fk_duckdb_texto <- function(texto) {
  if (!length(texto) || is.na(texto[[1L]])) return(NULL)
  s <- as.character(texto[[1L]])
  pat_ref <- .regex_sin_caja("REFERENCES")
  m <- regexpr(
    paste0(pat_ref, "\\s+([^\\(]+)\\(([^\\)]+)\\)"),
    s, perl = TRUE
  )
  if (m < 1L) return(NULL)
  trozo <- regmatches(s, m)
  sin_ref <- trimws(sub(paste0("^", pat_ref, "\\s+"), "", trozo, perl = TRUE))
  p_ini <- regexpr("(", sin_ref, fixed = TRUE, useBytes = TRUE)
  if (p_ini < 1L) return(NULL)
  destino <- trimws(substring(sin_ref, 1L, p_ini - 1L))
  cols_raw <- substring(sin_ref, p_ini)
  cols_ref <- .extraer_columnas_parentesis_sql(cols_raw)
  partes <- strsplit(destino, ".", fixed = TRUE)[[1L]]
  partes <- vapply(trimws(partes), .quitar_comillas_identificador,
                   character(1L), USE.NAMES = FALSE)
  if (length(partes) >= 2L) {
    ref_esq <- partes[[length(partes) - 1L]]
    ref_tab <- partes[[length(partes)]]
  } else {
    ref_esq <- NA_character_
    ref_tab <- partes[[1L]]
  }
  list(
    ref_esquema = ref_esq,
    ref_tabla = ref_tab,
    ref_columnas = cols_ref
  )
}

.sql_catalogo_garantias_pg <- function(esquema, tabla) {
  filtro_esq <- if (!is.na(esquema)) {
    paste0(" AND n.nspname = ", .texto_sql_clave(esquema))
  } else {
    " AND pg_catalog.pg_table_is_visible(r.oid)"
  }
  lit_tab <- .texto_sql_clave(tabla)
  paste0(
    "SELECT 'pk'::text AS cat_tipo, ",
    "a.attname::text AS column_name, ",
    "k.ordinal_position::integer AS ordinal_position, ",
    "n.nspname::text AS constraint_esquema, ",
    "c.conname::text AS constraint_nombre, ",
    "TRUE AS constraint_enforced, ",
    "c.convalidated AS constraint_validated, ",
    "(SELECT count(*) FROM pg_catalog.pg_inherits ih WHERE ih.inhparent = r.oid)::integer AS constraint_descendientes, ",
    "r.relkind::text AS constraint_relkind, ",
    "i.indisprimary AS constraint_indice_primario, ",
    "i.indisunique AS constraint_indice_unico, ",
    "i.indisvalid AS constraint_indice_valido, ",
    "i.indisready AS constraint_indice_listo, ",
    "i.indislive AS constraint_indice_vivo, ",
    "c.condeferrable AS constraint_diferible, ",
    "FALSE AS idx_es_restriccion, ",
    "FALSE AS idx_es_parcial, ",
    "NULL::text AS idx_filtro, ",
    "NULL::text AS idx_def, ",
    "FALSE AS idx_es_expresion, ",
    "NULL::text AS ref_esquema, ",
    "NULL::text AS ref_tabla, ",
    "NULL::text AS ref_columna ",
    "FROM pg_catalog.pg_constraint c ",
    "JOIN pg_catalog.pg_class r ON r.oid = c.conrelid ",
    "JOIN pg_catalog.pg_namespace n ON n.oid = r.relnamespace ",
    "LEFT JOIN pg_catalog.pg_index i ON i.indexrelid = c.conindid ",
    "JOIN LATERAL generate_subscripts(c.conkey, 1) AS k(ordinal_position) ON TRUE ",
    "JOIN pg_catalog.pg_attribute a ON a.attrelid = r.oid AND a.attnum = c.conkey[k.ordinal_position] ",
    "WHERE c.contype = 'p' AND r.relname = ", lit_tab, filtro_esq,
    " UNION ALL ",
    "SELECT 'uk'::text AS cat_tipo, ",
    "COALESCE(a.attname::text, '') AS column_name, ",
    "(k.ordinal_position + 1)::integer AS ordinal_position, ",
    "n.nspname::text AS constraint_esquema, ",
    "COALESCE(c.conname::text, ic.relname::text) AS constraint_nombre, ",
    "TRUE AS constraint_enforced, ",
    "COALESCE(c.convalidated, TRUE) AS constraint_validated, ",
    "(SELECT count(*) FROM pg_catalog.pg_inherits ih WHERE ih.inhparent = r.oid)::integer AS constraint_descendientes, ",
    "r.relkind::text AS constraint_relkind, ",
    "i.indisprimary AS constraint_indice_primario, ",
    "i.indisunique AS constraint_indice_unico, ",
    "i.indisvalid AS constraint_indice_valido, ",
    "i.indisready AS constraint_indice_listo, ",
    "i.indislive AS constraint_indice_vivo, ",
    "COALESCE(c.condeferrable, FALSE) AS constraint_diferible, ",
    "(c.oid IS NOT NULL) AS idx_es_restriccion, ",
    "(i.indpred IS NOT NULL) AS idx_es_parcial, ",
    "pg_catalog.pg_get_expr(i.indpred, i.indrelid)::text AS idx_filtro, ",
    "pg_catalog.pg_get_indexdef(i.indexrelid)::text AS idx_def, ",
    "(i.indkey[k.ordinal_position] = 0 OR i.indexprs IS NOT NULL) AS idx_es_expresion, ",
    "NULL::text AS ref_esquema, ",
    "NULL::text AS ref_tabla, ",
    "NULL::text AS ref_columna ",
    "FROM pg_catalog.pg_index i ",
    "JOIN pg_catalog.pg_class r ON r.oid = i.indrelid ",
    "JOIN pg_catalog.pg_namespace n ON n.oid = r.relnamespace ",
    "JOIN pg_catalog.pg_class ic ON ic.oid = i.indexrelid ",
    "LEFT JOIN pg_catalog.pg_constraint c ON c.conindid = i.indexrelid AND c.conrelid = r.oid AND c.contype = 'u' ",
    "JOIN LATERAL generate_subscripts(i.indkey, 1) AS k(ordinal_position) ON TRUE ",
    "LEFT JOIN pg_catalog.pg_attribute a ON a.attrelid = r.oid AND a.attnum = i.indkey[k.ordinal_position] AND i.indkey[k.ordinal_position] > 0 ",
    "WHERE i.indisunique AND NOT i.indisprimary AND r.relname = ", lit_tab, filtro_esq,
    " UNION ALL ",
    "SELECT 'fk'::text AS cat_tipo, ",
    "a.attname::text AS column_name, ",
    "k.ordinal_position::integer AS ordinal_position, ",
    "n.nspname::text AS constraint_esquema, ",
    "c.conname::text AS constraint_nombre, ",
    "TRUE AS constraint_enforced, ",
    "c.convalidated AS constraint_validated, ",
    "(SELECT count(*) FROM pg_catalog.pg_inherits ih WHERE ih.inhparent = r.oid)::integer AS constraint_descendientes, ",
    "r.relkind::text AS constraint_relkind, ",
    "NULL::boolean AS constraint_indice_primario, ",
    "NULL::boolean AS constraint_indice_unico, ",
    "NULL::boolean AS constraint_indice_valido, ",
    "NULL::boolean AS constraint_indice_listo, ",
    "NULL::boolean AS constraint_indice_vivo, ",
    "c.condeferrable AS constraint_diferible, ",
    "FALSE AS idx_es_restriccion, ",
    "FALSE AS idx_es_parcial, ",
    "NULL::text AS idx_filtro, ",
    "NULL::text AS idx_def, ",
    "FALSE AS idx_es_expresion, ",
    "nr.nspname::text AS ref_esquema, ",
    "rr.relname::text AS ref_tabla, ",
    "ar.attname::text AS ref_columna ",
    "FROM pg_catalog.pg_constraint c ",
    "JOIN pg_catalog.pg_class r ON r.oid = c.conrelid ",
    "JOIN pg_catalog.pg_namespace n ON n.oid = r.relnamespace ",
    "JOIN pg_catalog.pg_class rr ON rr.oid = c.confrelid ",
    "JOIN pg_catalog.pg_namespace nr ON nr.oid = rr.relnamespace ",
    "JOIN LATERAL generate_subscripts(c.conkey, 1) AS k(ordinal_position) ON TRUE ",
    "JOIN pg_catalog.pg_attribute a ON a.attrelid = r.oid AND a.attnum = c.conkey[k.ordinal_position] ",
    "JOIN pg_catalog.pg_attribute ar ON ar.attrelid = rr.oid AND ar.attnum = c.confkey[k.ordinal_position] ",
    "WHERE c.contype = 'f' AND r.relname = ", lit_tab, filtro_esq,
    " ORDER BY cat_tipo, constraint_nombre, ordinal_position"
  )
}

.sql_catalogo_garantias_mssql <- function(esquema, tabla) {
  obj_expr <- if (!is.na(esquema)) {
    paste0(
      "OBJECT_ID(N",
      .texto_sql_clave(paste0(
        "[", gsub("]", "]]", esquema, fixed = TRUE), "].[",
        gsub("]", "]]", tabla, fixed = TRUE), "]"
      )), ")"
    )
  } else {
    paste0("OBJECT_ID(N", .texto_sql_clave(tabla), ")")
  }
  paste0(
    "SELECT 'idx' AS cat_tipo, ",
    "c.name AS column_name, ",
    "ic.key_ordinal AS ordinal_position, ",
    "i.name AS constraint_nombre, ",
    "OBJECT_SCHEMA_NAME(i.object_id) AS constraint_esquema, ",
    "CAST(i.is_primary_key AS INT) AS es_pk, ",
    "CAST(i.is_unique_constraint AS INT) AS es_restriccion_unica, ",
    "CAST(i.is_disabled AS INT) AS deshabilitado, ",
    "CAST(i.has_filter AS INT) AS es_parcial, ",
    "CAST(i.filter_definition AS NVARCHAR(4000)) AS filtro, ",
    "CAST(c.is_nullable AS INT) AS es_nulo, ",
    "CAST(NULL AS INT) AS no_confiable, ",
    "CAST(NULL AS NVARCHAR(256)) AS ref_esquema, ",
    "CAST(NULL AS NVARCHAR(256)) AS ref_tabla, ",
    "CAST(NULL AS NVARCHAR(256)) AS ref_columna ",
    "FROM sys.indexes i ",
    "JOIN sys.index_columns ic ON i.object_id = ic.object_id AND i.index_id = ic.index_id ",
    "JOIN sys.columns c ON ic.object_id = c.object_id AND ic.column_id = c.column_id ",
    "WHERE i.object_id = ", obj_expr,
    " AND i.is_unique = 1 AND ic.is_included_column = 0 ",
    "UNION ALL ",
    "SELECT 'fk' AS cat_tipo, ",
    "cp.name AS column_name, ",
    "fkc.constraint_column_id AS ordinal_position, ",
    "fk.name AS constraint_nombre, ",
    "OBJECT_SCHEMA_NAME(fk.parent_object_id) AS constraint_esquema, ",
    "0 AS es_pk, ",
    "0 AS es_restriccion_unica, ",
    "CAST(fk.is_disabled AS INT) AS deshabilitado, ",
    "0 AS es_parcial, ",
    "CAST(NULL AS NVARCHAR(4000)) AS filtro, ",
    "0 AS es_nulo, ",
    "CAST(fk.is_not_trusted AS INT) AS no_confiable, ",
    "OBJECT_SCHEMA_NAME(fk.referenced_object_id) AS ref_esquema, ",
    "OBJECT_NAME(fk.referenced_object_id) AS ref_tabla, ",
    "cr.name AS ref_columna ",
    "FROM sys.foreign_keys fk ",
    "JOIN sys.foreign_key_columns fkc ON fk.object_id = fkc.constraint_object_id ",
    "JOIN sys.columns cp ON fkc.parent_object_id = cp.object_id AND fkc.parent_column_id = cp.column_id ",
    "JOIN sys.columns cr ON fkc.referenced_object_id = cr.object_id AND fkc.referenced_column_id = cr.column_id ",
    "WHERE fk.parent_object_id = ", obj_expr,
    " ORDER BY cat_tipo, constraint_nombre, ordinal_position"
  )
}

.sql_catalogo_garantias_mysql <- function(esquema, tabla) {
  esq_expr <- if (!is.na(esquema)) .texto_sql_clave(esquema) else "DATABASE()"
  lit_tab <- .texto_sql_clave(tabla)
  paste0(
    "SELECT 'pk' AS cat_tipo, ",
    "k.column_name AS column_name, ",
    "CAST(k.ordinal_position AS SIGNED) AS ordinal_position, ",
    "t.table_schema AS constraint_esquema, ",
    "t.constraint_name AS constraint_nombre, ",
    "t.enforced AS constraint_enforced, ",
    "CAST(NULL AS CHAR) AS idx_nullable, ",
    "CAST(0 AS SIGNED) AS idx_es_restriccion, ",
    "CAST(0 AS SIGNED) AS idx_es_expresion, ",
    "CAST(0 AS SIGNED) AS idx_sub_part, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks, ",
    "CAST(NULL AS CHAR) AS ref_esquema, ",
    "CAST(NULL AS CHAR) AS ref_tabla, ",
    "CAST(NULL AS CHAR) AS ref_columna ",
    "FROM information_schema.table_constraints t ",
    "JOIN information_schema.key_column_usage k ",
    "ON t.constraint_name = k.constraint_name AND t.table_schema = k.table_schema AND t.table_name = k.table_name ",
    "JOIN information_schema.tables tb ON tb.table_schema = t.table_schema AND tb.table_name = t.table_name ",
    "WHERE t.constraint_type = 'PRIMARY KEY' AND t.table_schema = ", esq_expr,
    " AND t.table_name = ", lit_tab,
    " UNION ALL ",
    "SELECT 'uk' AS cat_tipo, ",
    "COALESCE(s.column_name, '') AS column_name, ",
    "CAST(s.seq_in_index AS SIGNED) AS ordinal_position, ",
    "s.table_schema AS constraint_esquema, ",
    "s.index_name AS constraint_nombre, ",
    "COALESCE(tc.enforced, 'YES') AS constraint_enforced, ",
    "s.nullable AS idx_nullable, ",
    "CAST(CASE WHEN tc.constraint_name IS NOT NULL THEN 1 ELSE 0 END AS SIGNED) AS idx_es_restriccion, ",
    "CAST(CASE WHEN s.column_name IS NULL THEN 1 ELSE 0 END AS SIGNED) AS idx_es_expresion, ",
    "CAST(COALESCE(s.sub_part, 0) AS SIGNED) AS idx_sub_part, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks, ",
    "CAST(NULL AS CHAR) AS ref_esquema, ",
    "CAST(NULL AS CHAR) AS ref_tabla, ",
    "CAST(NULL AS CHAR) AS ref_columna ",
    "FROM information_schema.statistics s ",
    "JOIN information_schema.tables tb ON tb.table_schema = s.table_schema AND tb.table_name = s.table_name ",
    "LEFT JOIN information_schema.table_constraints tc ",
    "ON tc.table_schema = s.table_schema AND tc.table_name = s.table_name ",
    "AND tc.constraint_name = s.index_name AND tc.constraint_type = 'UNIQUE' ",
    "WHERE s.non_unique = 0 AND s.index_name <> 'PRIMARY' AND s.table_schema = ", esq_expr,
    " AND s.table_name = ", lit_tab,
    " UNION ALL ",
    "SELECT 'fk' AS cat_tipo, ",
    "k.column_name AS column_name, ",
    "CAST(k.ordinal_position AS SIGNED) AS ordinal_position, ",
    "k.table_schema AS constraint_esquema, ",
    "k.constraint_name AS constraint_nombre, ",
    "COALESCE(tc.enforced, 'YES') AS constraint_enforced, ",
    "CAST(NULL AS CHAR) AS idx_nullable, ",
    "CAST(0 AS SIGNED) AS idx_es_restriccion, ",
    "CAST(0 AS SIGNED) AS idx_es_expresion, ",
    "CAST(0 AS SIGNED) AS idx_sub_part, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks, ",
    "k.referenced_table_schema AS ref_esquema, ",
    "k.referenced_table_name AS ref_tabla, ",
    "k.referenced_column_name AS ref_columna ",
    "FROM information_schema.key_column_usage k ",
    "JOIN information_schema.tables tb ON tb.table_schema = k.table_schema AND tb.table_name = k.table_name ",
    "LEFT JOIN information_schema.table_constraints tc ",
    "ON tc.table_schema = k.table_schema AND tc.table_name = k.table_name ",
    "AND tc.constraint_name = k.constraint_name AND tc.constraint_type = 'FOREIGN KEY' ",
    "WHERE k.referenced_table_name IS NOT NULL AND k.table_schema = ", esq_expr,
    " AND k.table_name = ", lit_tab,
    " UNION ALL ",
    "SELECT 'meta' AS cat_tipo, ",
    "CAST(NULL AS CHAR) AS column_name, ",
    "CAST(0 AS SIGNED) AS ordinal_position, ",
    "tb.table_schema AS constraint_esquema, ",
    "CAST(NULL AS CHAR) AS constraint_nombre, ",
    "CAST(NULL AS CHAR) AS constraint_enforced, ",
    "CAST(NULL AS CHAR) AS idx_nullable, ",
    "CAST(0 AS SIGNED) AS idx_es_restriccion, ",
    "CAST(0 AS SIGNED) AS idx_es_expresion, ",
    "CAST(0 AS SIGNED) AS idx_sub_part, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks, ",
    "CAST(NULL AS CHAR) AS ref_esquema, ",
    "CAST(NULL AS CHAR) AS ref_tabla, ",
    "CAST(NULL AS CHAR) AS ref_columna ",
    "FROM information_schema.tables tb ",
    "WHERE tb.table_schema = ", esq_expr,
    " AND tb.table_name = ", lit_tab,
    " ORDER BY cat_tipo, constraint_nombre, ordinal_position"
  )
}

.sql_fks_mariadb <- function(esquema, tabla) {
  esq_expr <- if (!is.na(esquema)) .texto_sql_clave(esquema) else "DATABASE()"
  lit_tab <- .texto_sql_clave(tabla)
  paste0(
    "SELECT 'fk' AS cat_tipo, ",
    "k.constraint_name AS constraint_nombre, ",
    "k.column_name AS column_name, ",
    "CAST(k.ordinal_position AS SIGNED) AS ordinal_position, ",
    "k.referenced_table_schema AS ref_esquema, ",
    "k.referenced_table_name AS ref_tabla, ",
    "k.referenced_column_name AS ref_columna, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks ",
    "FROM information_schema.key_column_usage k ",
    "JOIN information_schema.tables tb ON tb.table_schema = k.table_schema AND tb.table_name = k.table_name ",
    "WHERE k.referenced_table_name IS NOT NULL AND k.table_schema = ", esq_expr,
    " AND k.table_name = ", lit_tab,
    " UNION ALL ",
    "SELECT 'meta' AS cat_tipo, ",
    "CAST(NULL AS CHAR) AS constraint_nombre, ",
    "CAST(NULL AS CHAR) AS column_name, ",
    "CAST(0 AS SIGNED) AS ordinal_position, ",
    "CAST(NULL AS CHAR) AS ref_esquema, ",
    "CAST(NULL AS CHAR) AS ref_tabla, ",
    "CAST(NULL AS CHAR) AS ref_columna, ",
    "tb.engine AS motor_tabla, ",
    "CAST(@@foreign_key_checks AS SIGNED) AS fk_checks ",
    "FROM information_schema.tables tb ",
    "WHERE tb.table_schema = ", esq_expr,
    " AND tb.table_name = ", lit_tab,
    " ORDER BY cat_tipo, constraint_nombre, ordinal_position"
  )
}

.sql_catalogo_garantias_sqlite <- function(esquema, tabla) {
  lit_tab <- .texto_sql_clave(tabla)
  paste0(
    "SELECT 'col' AS cat_tipo, ",
    "name AS column_name, ",
    "pk AS ordinal_position, ",
    "\"notnull\" AS column_notnull, ",
    "type AS column_type, ",
    "(SELECT sql FROM sqlite_master WHERE type IN ('table', 'view') AND name = ", lit_tab, ") AS lupa_sql_tabla, ",
    "NULL AS constraint_nombre, ",
    "NULL AS idx_origen, ",
    "NULL AS idx_parcial, ",
    "NULL AS idx_sql, ",
    "NULL AS ref_tabla, ",
    "NULL AS ref_columna, ",
    "NULL AS fk_enforced ",
    "FROM pragma_table_info(", lit_tab, ") ",
    "UNION ALL ",
    "SELECT 'idx' AS cat_tipo, ",
    "ii.name AS column_name, ",
    "ii.seqno + 1 AS ordinal_position, ",
    "NULL AS column_notnull, ",
    "NULL AS column_type, ",
    "NULL AS lupa_sql_tabla, ",
    "il.name AS constraint_nombre, ",
    "il.origin AS idx_origen, ",
    "il.partial AS idx_parcial, ",
    "(SELECT sql FROM sqlite_master WHERE type = 'index' AND name = il.name) AS idx_sql, ",
    "NULL AS ref_tabla, ",
    "NULL AS ref_columna, ",
    "NULL AS fk_enforced ",
    "FROM pragma_index_list(", lit_tab, ") il ",
    "JOIN pragma_index_info(il.name) ii ",
    "WHERE il.\"unique\" = 1 ",
    "UNION ALL ",
    "SELECT 'fk' AS cat_tipo, ",
    "fk.\"from\" AS column_name, ",
    "fk.seq + 1 AS ordinal_position, ",
    "NULL AS column_notnull, ",
    "NULL AS column_type, ",
    "NULL AS lupa_sql_tabla, ",
    "CAST(fk.id AS TEXT) AS constraint_nombre, ",
    "NULL AS idx_origen, ",
    "NULL AS idx_parcial, ",
    "NULL AS idx_sql, ",
    "fk.\"table\" AS ref_tabla, ",
    "fk.\"to\" AS ref_columna, ",
    "(SELECT foreign_keys FROM pragma_foreign_keys) AS fk_enforced ",
    "FROM pragma_foreign_key_list(", lit_tab, ") fk"
  )
}

.sql_catalogo_garantias_duckdb <- function(esquema, tabla) {
  lit_tab <- .texto_sql_clave(tabla)
  filtro_esq <- if (!is.na(esquema)) {
    paste0(" AND schema_name = ", .texto_sql_clave(esquema))
  } else {
    ""
  }
  paste0(
    "SELECT 'con' AS cat_tipo, ",
    "CAST(constraint_index AS VARCHAR) AS constraint_nombre, ",
    "constraint_type AS con_tipo, ",
    "constraint_text AS con_texto, ",
    "UNNEST(constraint_column_names) AS column_name, ",
    "UNNEST(range(1, len(constraint_column_names) + 1)) AS ordinal_position, ",
    "FALSE AS idx_es_parcial, ",
    "NULL::VARCHAR AS idx_sql ",
    "FROM duckdb_constraints() ",
    "WHERE table_name = ", lit_tab, filtro_esq,
    " UNION ALL ",
    "SELECT 'idx' AS cat_tipo, ",
    "index_name AS constraint_nombre, ",
    "CASE WHEN is_primary THEN 'PRIMARY KEY' ELSE 'UNIQUE INDEX' END AS con_tipo, ",
    "sql AS con_texto, ",
    "'' AS column_name, ",
    "1 AS ordinal_position, ",
    "FALSE AS idx_es_parcial, ",
    "sql AS idx_sql ",
    "FROM duckdb_indexes() ",
    "WHERE table_name = ", lit_tab, filtro_esq, " AND is_unique"
  )
}

.construir_tabla_not_null_dbi <- function(campos, mapa_not_null,
                                          tipo_objeto = NA_character_,
                                          leido = TRUE,
                                          motivo_general = NA_character_) {
  if (!length(campos)) return(.not_null_vacio_dbi())
  es_vista <- length(tipo_objeto) == 1L && !is.na(tipo_objeto) &&
    .mayusculas_ascii(trimws(as.character(tipo_objeto))) %in%
      c("V", "VIEW", "M", "MATERIALIZED VIEW")
  n <- length(campos)
  vals <- rep(NA, n)
  grados <- rep("ninguna", n)
  motivos <- rep(NA_character_, n)
  if (!isTRUE(leido) || is.null(mapa_not_null)) {
    motivos[] <- if (!is.na(motivo_general)) motivo_general else
      "No se pudo leer la restricci\u00f3n NOT NULL desde el cat\u00e1logo."
  } else {
    pos <- .indice_nombre(campos, names(mapa_not_null))
    for (i in seq_len(n)) {
      if (is.na(pos[[i]])) {
        motivos[[i]] <- "La columna no apareci\u00f3 en la lectura de NOT NULL del cat\u00e1logo."
        next
      }
      v <- mapa_not_null[[pos[[i]]]]
      if (is.na(v)) {
        motivos[[i]] <- "El cat\u00e1logo no inform\u00f3 si la columna admite NULL."
      } else if (isTRUE(v)) {
        vals[[i]] <- TRUE
        if (isTRUE(es_vista)) {
          grados[[i]] <- "parcial"
          motivos[[i]] <- "La columna pertenece a una vista; NOT NULL no es una restricci\u00f3n directa de tabla base."
        } else {
          grados[[i]] <- "total"
        }
      } else {
        vals[[i]] <- FALSE
        grados[[i]] <- "ninguna"
      }
    }
  }
  data.frame(
    columna = as.character(campos),
    not_null = vals,
    grado = grados,
    n_blancos = rep(NA_real_, n),
    not_null_con_blancos = rep(NA, n),
    motivo = motivos,
    stringsAsFactors = FALSE
  )
}

.identificacion_catalogo_dbi <- function(clave, claves_unicas) {
  grado_pk <- if (identical(clave$garantia, "garantizada")) {
    "total"
  } else if (is.list(clave$estado) &&
             identical(clave$estado$unicidad, "garantizada") &&
             length(clave$columnas) > 0L) {
    "parcial"
  } else {
    "ninguna"
  }
  if (length(clave$columnas) > 0L && grado_pk %in% c("total", "parcial")) {
    return(list(
      tiene_clave = TRUE,
      tipo = "clave_primaria",
      grado = grado_pk,
      columnas = as.character(clave$columnas),
      nombre = NA_character_,
      motivo = clave$motivo %||% NA_character_
    ))
  }
  if (is.data.frame(claves_unicas) && nrow(claves_unicas) > 0L) {
    validas <- which(claves_unicas$grado %in% c("total", "parcial"))
    if (length(validas)) {
      totales <- validas[claves_unicas$grado[validas] == "total"]
      elegida <- if (length(totales)) totales[[1L]] else validas[[1L]]
      return(list(
        tiene_clave = TRUE,
        tipo = as.character(claves_unicas$origen[[elegida]]),
        grado = as.character(claves_unicas$grado[[elegida]]),
        columnas = as.character(claves_unicas$columnas[[elegida]]),
        nombre = as.character(claves_unicas$nombre[[elegida]]),
        motivo = as.character(claves_unicas$motivo[[elegida]])
      ))
    }
  }
  if (length(clave$columnas) > 0L) {
    return(list(
      tiene_clave = TRUE,
      tipo = "clave_primaria",
      grado = "ninguna",
      columnas = as.character(clave$columnas),
      nombre = NA_character_,
      motivo = clave$motivo %||% NA_character_
    ))
  }
  visible <- if (is.list(clave$estado) && length(clave$estado$visible)) {
    clave$estado$visible[[1L]]
  } else NA
  list(
    tiene_clave = FALSE,
    tipo = if (identical(clave$garantia, "no_declarada") && isTRUE(visible)) {
      "ninguna"
    } else "desconocida",
    grado = "ninguna",
    columnas = character(),
    nombre = NA_character_,
    motivo = clave$motivo %||% NA_character_
  )
}

.filtrar_unicas_por_campos <- function(claves_unicas, campos) {
  if (!is.data.frame(claves_unicas) || !nrow(claves_unicas) || !length(campos)) {
    return(claves_unicas)
  }
  conservar <- vapply(seq_len(nrow(claves_unicas)), function(i) {
    cols <- as.character(claves_unicas$columnas[[i]])
    length(cols) > 0L && !anyNA(.resolver_columnas_dbi(cols, campos))
  }, logical(1L))
  salida <- claves_unicas[conservar, , drop = FALSE]
  rownames(salida) <- NULL
  salida
}

.normalizar_not_null_mapa <- function(not_null_df) {
  if (!is.data.frame(not_null_df) || !nrow(not_null_df)) return(logical())
  stats::setNames(
    not_null_df$grado == "total" & .es_cierto_vec(not_null_df$not_null),
    as.character(not_null_df$columna)
  )
}

.es_cierto_vec <- function(x) {
  !is.na(x) & as.logical(x)
}

.grado_clave_unica <- function(enforced, validated, parcial, filtro,
                               diferible, descendientes, expresion,
                               deshabilitado, semantica_null,
                               todas_not_null) {
  if (isTRUE(expresion)) {
    return(list(
      grado = "ninguna",
      motivo = "\u00cdndice \u00fanico sobre expresi\u00f3n: no garantiza unicidad directa sobre columnas."
    ))
  }
  if (isTRUE(deshabilitado)) {
    return(list(
      grado = "ninguna",
      motivo = "\u00cdndice \u00fanico deshabilitado en el cat\u00e1logo."
    ))
  }
  if (identical(enforced, FALSE) || identical(validated, FALSE)) {
    return(list(
      grado = "ninguna",
      motivo = "La restricci\u00f3n \u00fanica no est\u00e1 aplicada o validada por el motor."
    ))
  }
  if (isTRUE(diferible)) {
    return(list(
      grado = "ninguna",
      motivo = "Restricci\u00f3n \u00fanica diferible (DEFERRABLE): puede violarse dentro de una transacci\u00f3n."
    ))
  }
  if (isTRUE(descendientes)) {
    return(list(
      grado = "ninguna",
      motivo = "La tabla tiene descendientes por herencia; el \u00edndice del padre no cubre esas filas."
    ))
  }
  if (isTRUE(parcial)) {
    return(list(
      grado = "parcial",
      motivo = paste0(
        "\u00cdndice \u00fanico parcial/filtrado",
        if (!is.na(filtro) && nzchar(filtro)) paste0(" (", filtro, ")") else "",
        ": s\u00f3lo garantiza unicidad en ese subconjunto."
      )
    ))
  }
  if (isTRUE(todas_not_null) || identical(semantica_null, "nulls_not_distinct")) {
    return(list(grado = "total", motivo = NA_character_))
  }
  list(
    grado = "parcial",
    motivo = "Unicidad garantizada entre valores no nulos (sem\u00e1ntica NULLS DISTINCT: admite m\u00faltiples NULL)."
  )
}

.interpretar_catalogo_garantias_pg <- function(datos, campos, mapa_not_null,
                                               tipo_objeto, vacio_pk) {
  vias <- .consultas_clave_primaria()
  via_pk <- vias[[which(vapply(vias, function(v) v$nombre == "pg_catalog",
                               logical(1L)))[[1L]]]]
  es_df <- inherits(datos, "data.frame") && nrow(datos) > 0L
  tipo_fila <- if (es_df && "cat_tipo" %in% names(datos)) {
    as.character(datos$cat_tipo)
  } else character()

  datos_pk <- if (es_df) datos[tipo_fila == "pk", , drop = FALSE] else data.frame()
  clave <- .interpretar_clave_primaria_dbi(datos_pk, via_pk, "postgresql", vacio_pk)

  not_null_df <- .construir_tabla_not_null_dbi(
    campos, mapa_not_null, tipo_objeto = tipo_objeto,
    leido = !is.null(mapa_not_null),
    motivo_general = if (is.null(mapa_not_null)) {
      "No se consult\u00f3 `pg_attribute.attnotnull`."
    } else NA_character_
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  unicas <- .claves_unicas_vacias_dbi()
  datos_uk <- if (es_df) datos[tipo_fila == "uk", , drop = FALSE] else data.frame()
  if (nrow(datos_uk)) {
    grupos <- split(seq_len(nrow(datos_uk)), as.character(datos_uk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_uk <- lapply(nombres_g, function(nom) {
      idx <- grupos[[nom]]
      sub <- datos_uk[idx, , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      def_txt <- if ("idx_def" %in% names(sub)) as.character(sub$idx_def[[1L]]) else NA_character_
      # Si el indice tiene clausula INCLUDE (PG 11+), solo cuentan las columnas clave.
      cols_clave <- as.character(sub$column_name)
      pat_inc <- paste0(.regex_sin_caja("\\bINCLUDE"), "\\s*\\(")
      if (!is.na(def_txt) && grepl(pat_inc, def_txt, perl = TRUE)) {
        antes_inc <- sub(paste0(pat_inc, ".*$"), "", def_txt, perl = TRUE)
        cols_def <- .extraer_columnas_parentesis_sql(antes_inc)
        if (length(cols_def) && length(cols_def) <= length(cols_clave)) {
          cols_clave <- cols_clave[seq_len(length(cols_def))]
        }
      }
      expresion <- any(.es_cierto_vec(sub$idx_es_expresion)) || any(!nzchar(cols_clave))
      parcial <- .es_cierto_vec(sub$idx_es_parcial)[[1L]]
      filtro <- if (parcial && "idx_filtro" %in% names(sub)) {
        as.character(sub$idx_filtro[[1L]])
      } else NA_character_
      nnd <- !is.na(def_txt) && grepl("NULLS NOT DISTINCT", def_txt, fixed = TRUE, useBytes = TRUE)
      sem_null <- if (nnd) "nulls_not_distinct" else "nulls_distinct"
      es_restr <- .es_cierto_vec(sub$idx_es_restriccion)[[1L]]
      valido_idx <- all(vapply(
        c("constraint_indice_unico", "constraint_indice_valido",
          "constraint_indice_listo", "constraint_indice_vivo"),
        function(col) col %in% names(sub) && isTRUE(.estado_clave(sub[[col]][[1L]], "si_no")),
        logical(1L)
      ))
      desc <- suppressWarnings(as.numeric(sub$constraint_descendientes[[1L]]))
      relk <- as.character(sub$constraint_relkind[[1L]])
      hay_desc <- is.finite(desc) && desc > 0 && !identical(relk, "p")
      diferible <- isTRUE(.estado_clave(sub$constraint_diferible[[1L]], "diferible"))
      validada <- isTRUE(.estado_clave(sub$constraint_validated[[1L]], "si_no"))
      pos_nn <- .indice_nombre(cols_clave, names(nn_map))
      todas_nn <- !expresion && length(cols_clave) > 0L &&
        !anyNA(pos_nn) && all(nn_map[pos_nn])
      g <- .grado_clave_unica(
        enforced = valido_idx, validated = validada, parcial = parcial,
        filtro = filtro, diferible = diferible, descendientes = hay_desc,
        expresion = expresion, deshabilitado = !valido_idx,
        semantica_null = sem_null, todas_not_null = todas_nn
      )
      data.frame(
        nombre = nom,
        origen = if (es_restr) "restriccion_unica" else "indice_unico",
        columnas = I(list(cols_clave[nzchar(cols_clave)])),
        es_parcial = parcial,
        filtro = filtro,
        semantica_null = sem_null,
        todas_not_null = todas_nn,
        grado = g$grado,
        motivo = g$motivo,
        stringsAsFactors = FALSE
      )
    })
    unicas <- do.call(rbind, filas_uk)
    rownames(unicas) <- NULL
  }

  fks <- .claves_foraneas_vacias_dbi()
  datos_fk <- if (es_df) datos[tipo_fila == "fk", , drop = FALSE] else data.frame()
  if (nrow(datos_fk)) {
    grupos <- split(seq_len(nrow(datos_fk)), as.character(datos_fk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_fk <- lapply(nombres_g, function(nom) {
      sub <- datos_fk[grupos[[nom]], , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      conval <- isTRUE(.estado_clave(sub$constraint_validated[[1L]], "si_no"))
      diferible <- isTRUE(.estado_clave(sub$constraint_diferible[[1L]], "diferible"))
      desc <- suppressWarnings(as.numeric(sub$constraint_descendientes[[1L]]))
      relk <- as.character(sub$constraint_relkind[[1L]])
      hay_desc <- is.finite(desc) && desc > 0 && !identical(relk, "p")
      if (!conval) {
        grado <- "parcial"
        motivo <- "Restricci\u00f3n declarada NOT VALID (convalidated = FALSE): valida escrituras nuevas pero no garantiza filas preexistentes."
      } else if (diferible) {
        grado <- "parcial"
        motivo <- "Clave for\u00e1nea diferible (DEFERRABLE): puede violarse dentro de una transacci\u00f3n abierta."
      } else if (hay_desc) {
        grado <- "parcial"
        motivo <- "La tabla tiene descendientes por herencia; la clave for\u00e1nea del padre no cubre esas filas."
      } else {
        grado <- "total"
        motivo <- NA_character_
      }
      ref_id <- .identificadores_tabla(
        as.character(sub$ref_esquema[[1L]]),
        as.character(sub$ref_tabla[[1L]])
      )
      data.frame(
        nombre = nom,
        columnas = I(list(as.character(sub$column_name))),
        tabla_referenciada = ref_id,
        columnas_referenciadas = I(list(as.character(sub$ref_columna))),
        validada = conval,
        grado = grado,
        motivo = motivo,
        stringsAsFactors = FALSE
      )
    })
    fks <- do.call(rbind, filas_fk)
    rownames(fks) <- NULL
  }

  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = TRUE, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = TRUE, motivo = NA_character_),
      claves_foraneas = list(leido = TRUE, motivo = NA_character_),
      not_null = list(
        leido = !is.null(mapa_not_null),
        motivo = if (is.null(mapa_not_null)) "No se consult\u00f3 `pg_attribute.attnotnull`." else NA_character_
      )
    )
  )
}

.interpretar_catalogo_garantias_mssql <- function(datos, campos, mapa_not_null,
                                                  tipo_objeto, vacio_pk) {
  es_df <- inherits(datos, "data.frame") && nrow(datos) > 0L
  tipo_fila <- if (es_df && "cat_tipo" %in% names(datos)) {
    as.character(datos$cat_tipo)
  } else character()

  not_null_df <- .construir_tabla_not_null_dbi(
    campos, mapa_not_null, tipo_objeto = tipo_objeto,
    leido = !is.null(mapa_not_null),
    motivo_general = if (is.null(mapa_not_null)) {
      "No se consult\u00f3 `sys.columns.is_nullable`."
    } else NA_character_
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  datos_idx <- if (es_df) datos[tipo_fila == "idx", , drop = FALSE] else data.frame()
  es_pk <- if (nrow(datos_idx)) .es_cierto_vec(as.numeric(datos_idx$es_pk) != 0) else logical()
  datos_pk <- if (nrow(datos_idx)) datos_idx[es_pk, , drop = FALSE] else data.frame()
  if (nrow(datos_pk)) {
    ord <- .orden_por_bytes(suppressWarnings(as.numeric(datos_pk$ordinal_position)))
    datos_pk <- datos_pk[ord, , drop = FALSE]
    deshab <- .es_cierto_vec(as.numeric(datos_pk$deshabilitado) != 0)[[1L]]
    estado_pk <- .estado_clave_vacio(visible = TRUE)
    estado_pk$aplicada <- !deshab
    estado_pk$validada <- !deshab
    estado_pk$unicidad <- if (deshab) "no_verificada" else "garantizada"
    estado_pk$ausencia_de_nulos <- if (deshab) "no_verificada" else "garantizada"
    clave <- list(
      columnas = as.character(datos_pk$column_name),
      fuente = "sys_catalog",
      motivo = if (deshab) "La clave primaria est\u00e1 deshabilitada en `sys.indexes`." else NA_character_,
      garantia = if (deshab) "declarada_no_garantizada" else "garantizada",
      estado = estado_pk
    )
  } else {
    clave <- vacio_pk("sys_catalog", NA_character_, garantia = "no_declarada", visible = TRUE)
  }

  unicas <- .claves_unicas_vacias_dbi()
  datos_uk <- if (nrow(datos_idx)) datos_idx[!es_pk, , drop = FALSE] else data.frame()
  if (nrow(datos_uk)) {
    grupos <- split(seq_len(nrow(datos_uk)), as.character(datos_uk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_uk <- lapply(nombres_g, function(nom) {
      sub <- datos_uk[grupos[[nom]], , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      cols <- as.character(sub$column_name)
      deshab <- .es_cierto_vec(as.numeric(sub$deshabilitado) != 0)[[1L]]
      parcial <- .es_cierto_vec(as.numeric(sub$es_parcial) != 0)[[1L]]
      es_restr <- .es_cierto_vec(as.numeric(sub$es_restriccion_unica) != 0)[[1L]]
      filtro <- if (parcial && "filtro" %in% names(sub)) as.character(sub$filtro[[1L]]) else NA_character_
      todas_nn <- all(!.es_cierto_vec(as.numeric(sub$es_nulo) != 0))
      # En SQL Server un UNIQUE trata NULL como igual a NULL (admite a lo sumo un NULL).
      sem_null <- "nulls_not_distinct"
      g <- .grado_clave_unica(
        enforced = !deshab, validated = !deshab, parcial = parcial,
        filtro = filtro, diferible = FALSE, descendientes = FALSE,
        expresion = FALSE, deshabilitado = deshab,
        semantica_null = sem_null, todas_not_null = todas_nn
      )
      data.frame(
        nombre = nom,
        origen = if (es_restr) "restriccion_unica" else "indice_unico",
        columnas = I(list(cols)),
        es_parcial = parcial,
        filtro = filtro,
        semantica_null = sem_null,
        todas_not_null = todas_nn,
        grado = g$grado,
        motivo = g$motivo,
        stringsAsFactors = FALSE
      )
    })
    unicas <- do.call(rbind, filas_uk)
    rownames(unicas) <- NULL
  }

  fks <- .claves_foraneas_vacias_dbi()
  datos_fk <- if (es_df) datos[tipo_fila == "fk", , drop = FALSE] else data.frame()
  if (nrow(datos_fk)) {
    grupos <- split(seq_len(nrow(datos_fk)), as.character(datos_fk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_fk <- lapply(nombres_g, function(nom) {
      sub <- datos_fk[grupos[[nom]], , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      deshab <- .es_cierto_vec(as.numeric(sub$deshabilitado) != 0)[[1L]]
      no_conf <- .es_cierto_vec(as.numeric(sub$no_confiable) != 0)[[1L]]
      if (deshab) {
        grado <- "ninguna"
        validada <- FALSE
        motivo <- "Clave for\u00e1nea deshabilitada (is_disabled = 1): SQL Server no la aplica."
      } else if (no_conf) {
        grado <- "parcial"
        validada <- FALSE
        motivo <- "Clave for\u00e1nea no confiable (is_not_trusted = 1, p. ej. WITH NOCHECK): valida escrituras nuevas pero no garantiza filas preexistentes."
      } else {
        grado <- "total"
        validada <- TRUE
        motivo <- NA_character_
      }
      ref_id <- .identificadores_tabla(
        as.character(sub$ref_esquema[[1L]]),
        as.character(sub$ref_tabla[[1L]])
      )
      data.frame(
        nombre = nom,
        columnas = I(list(as.character(sub$column_name))),
        tabla_referenciada = ref_id,
        columnas_referenciadas = I(list(as.character(sub$ref_columna))),
        validada = validada,
        grado = grado,
        motivo = motivo,
        stringsAsFactors = FALSE
      )
    })
    fks <- do.call(rbind, filas_fk)
    rownames(fks) <- NULL
  }

  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = TRUE, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = TRUE, motivo = NA_character_),
      claves_foraneas = list(leido = TRUE, motivo = NA_character_),
      not_null = list(
        leido = !is.null(mapa_not_null),
        motivo = if (is.null(mapa_not_null)) "No se consult\u00f3 `sys.columns.is_nullable`." else NA_character_
      )
    )
  )
}

.interpretar_catalogo_garantias_mysql <- function(datos, campos, mapa_not_null,
                                                  tipo_objeto, vacio_pk) {
  vias <- .consultas_clave_primaria()
  via_pk <- vias[[which(vapply(vias, function(v) v$nombre == "information_schema",
                               logical(1L)))[[1L]]]]
  es_df <- inherits(datos, "data.frame") && nrow(datos) > 0L
  if (es_df) names(datos) <- .minusculas_ascii(names(datos))
  tipo_fila <- if (es_df && "cat_tipo" %in% names(datos)) {
    as.character(datos$cat_tipo)
  } else character()

  datos_pk <- if (es_df) datos[tipo_fila == "pk", , drop = FALSE] else data.frame()
  clave <- .interpretar_clave_primaria_dbi(datos_pk, via_pk, "mysql", vacio_pk)

  not_null_df <- .construir_tabla_not_null_dbi(
    campos, mapa_not_null, tipo_objeto = tipo_objeto,
    leido = !is.null(mapa_not_null),
    motivo_general = if (is.null(mapa_not_null)) {
      "No se consult\u00f3 `information_schema.columns.IS_NULLABLE`."
    } else NA_character_
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  unicas <- .claves_unicas_vacias_dbi()
  datos_uk <- if (es_df) datos[tipo_fila == "uk", , drop = FALSE] else data.frame()
  if (nrow(datos_uk)) {
    grupos <- split(seq_len(nrow(datos_uk)), as.character(datos_uk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_uk <- lapply(nombres_g, function(nom) {
      sub <- datos_uk[grupos[[nom]], , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      cols <- as.character(sub$column_name)
      expresion <- any(suppressWarnings(as.numeric(sub$idx_es_expresion)) != 0, na.rm = TRUE) ||
        any(!nzchar(cols))
      enforced <- .estado_clave(sub$constraint_enforced[[1L]], "si_no")
      es_restr <- isTRUE(suppressWarnings(as.numeric(sub$idx_es_restriccion[[1L]])) != 0)
      pos_nn <- .indice_nombre(cols, names(nn_map))
      todas_nn <- !expresion && length(cols) > 0L && (
        (!anyNA(pos_nn) && all(nn_map[pos_nn])) ||
          all(.mayusculas_ascii(trimws(as.character(sub$idx_nullable))) != "YES")
      )
      g <- .grado_clave_unica(
        enforced = if (is.na(enforced)) TRUE else enforced,
        validated = TRUE, parcial = FALSE, filtro = NA_character_,
        diferible = FALSE, descendientes = FALSE,
        expresion = expresion, deshabilitado = identical(enforced, FALSE),
        semantica_null = "nulls_distinct", todas_not_null = todas_nn
      )
      data.frame(
        nombre = nom,
        origen = if (es_restr) "restriccion_unica" else "indice_unico",
        columnas = I(list(cols[nzchar(cols)])),
        es_parcial = FALSE,
        filtro = NA_character_,
        semantica_null = "nulls_distinct",
        todas_not_null = todas_nn,
        grado = g$grado,
        motivo = g$motivo,
        stringsAsFactors = FALSE
      )
    })
    unicas <- do.call(rbind, filas_uk)
    rownames(unicas) <- NULL
  }

  datos_meta <- if (es_df) datos[tipo_fila == "meta", , drop = FALSE] else data.frame()
  motivo_fk_meta <- .motivo_motor_fks_mysql(datos_meta)
  fks <- .interpretar_fks_mysql_mariadb(
    if (es_df) datos[tipo_fila == "fk", , drop = FALSE] else data.frame()
  )

  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = TRUE, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = TRUE, motivo = NA_character_),
      claves_foraneas = list(leido = TRUE, motivo = motivo_fk_meta),
      not_null = list(
        leido = !is.null(mapa_not_null),
        motivo = if (is.null(mapa_not_null)) "No se consult\u00f3 `IS_NULLABLE`." else NA_character_
      )
    )
  )
}

.motivo_motor_fks_mysql <- function(datos_meta) {
  if (!inherits(datos_meta, "data.frame") || !nrow(datos_meta)) {
    return(NA_character_)
  }
  names(datos_meta) <- .minusculas_ascii(names(datos_meta))
  motor_t <- if ("motor_tabla" %in% names(datos_meta)) {
    trimws(as.character(datos_meta$motor_tabla[[1L]]))
  } else NA_character_
  fk_chk <- if ("fk_checks" %in% names(datos_meta)) {
    suppressWarnings(as.numeric(datos_meta$fk_checks[[1L]]))
  } else NA_real_
  if (!is.na(motor_t) && nzchar(motor_t) &&
      !identical(.mayusculas_ascii(motor_t), "INNODB")) {
    return(paste0(
      "El motor de almacenamiento (`", motor_t,
      "`) no almacena ni valida claves for\u00e1neas; s\u00f3lo InnoDB las aplica."
    ))
  }
  if (is.finite(fk_chk) && fk_chk == 0) {
    return("`foreign_key_checks` est\u00e1 desactivado (0) en esta sesi\u00f3n.")
  }
  NA_character_
}

.interpretar_fks_mysql_mariadb <- function(datos_fk) {
  if (!inherits(datos_fk, "data.frame") || !nrow(datos_fk)) {
    return(.claves_foraneas_vacias_dbi())
  }
  names(datos_fk) <- .minusculas_ascii(names(datos_fk))
  grupos <- split(seq_len(nrow(datos_fk)), as.character(datos_fk$constraint_nombre))
  nombres_g <- .sort_por_bytes(names(grupos))
  filas <- lapply(nombres_g, function(nom) {
    sub <- datos_fk[grupos[[nom]], , drop = FALSE]
    ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
    sub <- sub[ord, , drop = FALSE]
    motor_t <- if ("motor_tabla" %in% names(sub)) {
      trimws(as.character(sub$motor_tabla[[1L]]))
    } else NA_character_
    fk_chk <- if ("fk_checks" %in% names(sub)) {
      suppressWarnings(as.numeric(sub$fk_checks[[1L]]))
    } else NA_real_
    es_innodb <- !is.na(motor_t) && identical(.mayusculas_ascii(motor_t), "INNODB")
    enforced <- if ("constraint_enforced" %in% names(sub)) {
      .estado_clave(sub$constraint_enforced[[1L]], "si_no")
    } else TRUE
    if (!es_innodb) {
      grado <- "ninguna"
      validada <- FALSE
      motivo <- paste0(
        "El motor de almacenamiento (`", motor_t %||% "desconocido",
        "`) no valida claves for\u00e1neas; s\u00f3lo InnoDB las aplica."
      )
    } else if (identical(enforced, FALSE)) {
      grado <- "ninguna"
      validada <- FALSE
      motivo <- "La clave for\u00e1nea figura como no aplicada (ENFORCED = NO)."
    } else if (is.finite(fk_chk) && fk_chk == 0) {
      grado <- "ninguna"
      validada <- FALSE
      motivo <- "`foreign_key_checks` est\u00e1 desactivado (0) en esta sesi\u00f3n."
    } else {
      grado <- "total"
      validada <- TRUE
      motivo <- NA_character_
    }
    ref_id <- .identificadores_tabla(
      as.character(sub$ref_esquema[[1L]]),
      as.character(sub$ref_tabla[[1L]])
    )
    data.frame(
      nombre = nom,
      columnas = I(list(as.character(sub$column_name))),
      tabla_referenciada = ref_id,
      columnas_referenciadas = I(list(as.character(sub$ref_columna))),
      validada = validada,
      grado = grado,
      motivo = motivo,
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  rownames(salida) <- NULL
  salida
}

.interpretar_catalogo_garantias_mariadb <- function(datos_idx, resp_fk,
                                                    campos, mapa_not_null,
                                                    tipo_objeto, vacio_pk) {
  vias <- .consultas_clave_primaria()
  via_pk <- vias[[which(vapply(vias, function(v) v$nombre == "show_index",
                               logical(1L)))[[1L]]]]
  clave <- .interpretar_clave_primaria_dbi(datos_idx, via_pk, "mariadb", vacio_pk)
  not_null_df <- .construir_tabla_not_null_dbi(
    campos, mapa_not_null, tipo_objeto = tipo_objeto,
    leido = !is.null(mapa_not_null),
    motivo_general = if (is.null(mapa_not_null)) {
      "No se consult\u00f3 `information_schema.columns.IS_NULLABLE`."
    } else NA_character_
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  unicas <- .claves_unicas_vacias_dbi()
  if (inherits(datos_idx, "data.frame") && nrow(datos_idx)) {
    kname <- .campo_clave(datos_idx, "key_name")
    non_uniq <- .campo_clave(datos_idx, "non_unique")
    col_nom <- .campo_clave(datos_idx, "column_name")
    seq_idx <- .campo_clave(datos_idx, "seq_in_index")
    null_col <- .campo_clave(datos_idx, "null")
    if (!is.null(kname) && !is.null(non_uniq) && !is.null(col_nom)) {
      es_uk <- suppressWarnings(as.numeric(non_uniq)) == 0 &
        .mayusculas_ascii(trimws(as.character(kname))) != "PRIMARY"
      if (any(es_uk, na.rm = TRUE)) {
        sub_all <- datos_idx[which(es_uk), , drop = FALSE]
        kn_sub <- as.character(kname[which(es_uk)])
        grupos <- split(seq_len(nrow(sub_all)), kn_sub)
        nombres_g <- .sort_por_bytes(names(grupos))
        filas_uk <- lapply(nombres_g, function(nom) {
          idx_g <- grupos[[nom]]
          pos_g <- if (!is.null(seq_idx)) {
            suppressWarnings(as.numeric(seq_idx[which(es_uk)][idx_g]))
          } else seq_along(idx_g)
          ord <- .orden_por_bytes(pos_g)
          cols <- as.character(col_nom[which(es_uk)][idx_g][ord])
          expresion <- any(is.na(cols) | !nzchar(cols))
          pos_nn <- .indice_nombre(cols, names(nn_map))
          null_vals <- if (!is.null(null_col)) {
            as.character(null_col[which(es_uk)][idx_g][ord])
          } else rep("YES", length(cols))
          todas_nn <- !expresion && length(cols) > 0L && (
            (!anyNA(pos_nn) && all(nn_map[pos_nn])) ||
              all(.mayusculas_ascii(trimws(null_vals)) != "YES")
          )
          g <- .grado_clave_unica(
            enforced = TRUE, validated = TRUE, parcial = FALSE,
            filtro = NA_character_, diferible = FALSE, descendientes = FALSE,
            expresion = expresion, deshabilitado = FALSE,
            semantica_null = "nulls_distinct", todas_not_null = todas_nn
          )
          data.frame(
            nombre = nom,
            origen = "indice_unico",
            columnas = I(list(cols[!is.na(cols) & nzchar(cols)])),
            es_parcial = FALSE,
            filtro = NA_character_,
            semantica_null = "nulls_distinct",
            todas_not_null = todas_nn,
            grado = g$grado,
            motivo = g$motivo,
            stringsAsFactors = FALSE
          )
        })
        unicas <- do.call(rbind, filas_uk)
        rownames(unicas) <- NULL
      }
    }
  }

  fk_ok <- isTRUE(resp_fk$ok) && inherits(resp_fk$datos, "data.frame")
  datos_fk_all <- if (fk_ok) resp_fk$datos else data.frame()
  if (fk_ok && nrow(datos_fk_all)) {
    names(datos_fk_all) <- .minusculas_ascii(names(datos_fk_all))
  }
  tf_fk <- if (fk_ok && nrow(datos_fk_all) && "cat_tipo" %in% names(datos_fk_all)) {
    as.character(datos_fk_all$cat_tipo)
  } else rep("fk", nrow(datos_fk_all))
  datos_fk_solo <- if (fk_ok && nrow(datos_fk_all)) {
    datos_fk_all[tf_fk == "fk", , drop = FALSE]
  } else data.frame()
  datos_fk_meta <- if (fk_ok && nrow(datos_fk_all)) {
    datos_fk_all[tf_fk == "meta", , drop = FALSE]
  } else data.frame()
  fks <- if (fk_ok) .interpretar_fks_mysql_mariadb(datos_fk_solo) else .claves_foraneas_vacias_dbi()
  motivo_fk_meta <- if (fk_ok) {
    .motivo_motor_fks_mysql(datos_fk_meta)
  } else {
    paste0("information_schema: ", resp_fk$motivo)
  }

  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = TRUE, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = TRUE, motivo = NA_character_),
      claves_foraneas = list(
        leido = fk_ok,
        motivo = motivo_fk_meta
      ),
      not_null = list(
        leido = !is.null(mapa_not_null),
        motivo = if (is.null(mapa_not_null)) "No se consult\u00f3 `IS_NULLABLE`." else NA_character_
      )
    )
  )
}

.interpretar_catalogo_garantias_sqlite <- function(datos, campos, vacio_pk) {
  vias <- .consultas_clave_primaria()
  via_pk <- vias[[which(vapply(vias, function(v) v$nombre == "pragma",
                               logical(1L)))[[1L]]]]
  es_df <- inherits(datos, "data.frame") && nrow(datos) > 0L
  tipo_fila <- if (es_df && "cat_tipo" %in% names(datos)) {
    as.character(datos$cat_tipo)
  } else character()

  datos_col <- if (es_df) datos[tipo_fila == "col", , drop = FALSE] else data.frame()
  clave <- .interpretar_clave_primaria_dbi(datos_col, via_pk, "sqlite", vacio_pk)
  if (nrow(datos_col) && "lupa_sql_tabla" %in% names(datos_col)) {
    attr(clave, "sql_tabla") <- as.character(datos_col$lupa_sql_tabla[[1L]])
  }
  tipos_pragma <- if (nrow(datos_col) &&
      all(c("column_name", "column_type") %in% names(datos_col))) {
    stats::setNames(
      as.character(datos_col$column_type),
      as.character(datos_col$column_name)
    )
  } else NULL

  mapa_nn <- if (nrow(datos_col)) {
    stats::setNames(
      suppressWarnings(as.numeric(datos_col$column_notnull)) != 0,
      as.character(datos_col$column_name)
    )
  } else NULL
  campos_uso <- if (length(campos)) campos else if (nrow(datos_col)) as.character(datos_col$column_name) else character()
  not_null_df <- .construir_tabla_not_null_dbi(
    campos_uso, mapa_nn, leido = !is.null(mapa_nn),
    motivo_general = if (is.null(mapa_nn)) "pragma_table_info no devolvi\u00f3 filas." else NA_character_
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  unicas <- .claves_unicas_vacias_dbi()
  datos_idx <- if (es_df) datos[tipo_fila == "idx", , drop = FALSE] else data.frame()
  if (nrow(datos_idx)) {
    # Excluir el indice automatico que respalda la PRIMARY KEY (`origin == 'pk'`)
    no_pk <- is.na(datos_idx$idx_origen) | as.character(datos_idx$idx_origen) != "pk"
    datos_uk <- datos_idx[no_pk, , drop = FALSE]
    if (nrow(datos_uk)) {
      grupos <- split(seq_len(nrow(datos_uk)), as.character(datos_uk$constraint_nombre))
      nombres_g <- .sort_por_bytes(names(grupos))
      filas_uk <- lapply(nombres_g, function(nom) {
        sub <- datos_uk[grupos[[nom]], , drop = FALSE]
        ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
        sub <- sub[ord, , drop = FALSE]
        cols <- as.character(sub$column_name)
        expresion <- any(is.na(cols) | !nzchar(cols))
        parcial <- isTRUE(suppressWarnings(as.numeric(sub$idx_parcial[[1L]])) != 0)
        filtro <- if (parcial) .extraer_filtro_where_sql(sub$idx_sql[[1L]]) else NA_character_
        orig <- as.character(sub$idx_origen[[1L]])
        origen <- if (identical(orig, "u")) "restriccion_unica" else "indice_unico"
        pos_nn <- .indice_nombre(cols, names(nn_map))
        todas_nn <- !expresion && length(cols) > 0L && !anyNA(pos_nn) && all(nn_map[pos_nn])
        g <- .grado_clave_unica(
          enforced = TRUE, validated = TRUE, parcial = parcial,
          filtro = filtro, diferible = FALSE, descendientes = FALSE,
          expresion = expresion, deshabilitado = FALSE,
          semantica_null = "nulls_distinct", todas_not_null = todas_nn
        )
        data.frame(
          nombre = nom,
          origen = origen,
          columnas = I(list(cols[!is.na(cols) & nzchar(cols)])),
          es_parcial = parcial,
          filtro = filtro,
          semantica_null = "nulls_distinct",
          todas_not_null = todas_nn,
          grado = g$grado,
          motivo = g$motivo,
          stringsAsFactors = FALSE
        )
      })
      unicas <- do.call(rbind, filas_uk)
      rownames(unicas) <- NULL
    }
  }

  fks <- .claves_foraneas_vacias_dbi()
  datos_fk <- if (es_df) datos[tipo_fila == "fk", , drop = FALSE] else data.frame()
  if (nrow(datos_fk)) {
    grupos <- split(seq_len(nrow(datos_fk)), as.character(datos_fk$constraint_nombre))
    nombres_g <- .sort_por_bytes(names(grupos))
    filas_fk <- lapply(nombres_g, function(nom) {
      sub <- datos_fk[grupos[[nom]], , drop = FALSE]
      ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
      sub <- sub[ord, , drop = FALSE]
      fk_on <- isTRUE(suppressWarnings(as.numeric(sub$fk_enforced[[1L]])) != 0)
      if (fk_on) {
        grado <- "parcial"
        validada <- TRUE
        motivo <- "PRAGMA foreign_keys = 1 en esta conexi\u00f3n, pero SQLite no garantiza filas cargadas cuando estuvo desactivado."
      } else {
        grado <- "ninguna"
        validada <- FALSE
        motivo <- "PRAGMA foreign_keys = 0 en esta conexi\u00f3n: SQLite no valida claves for\u00e1neas."
      }
      ref_id <- .identificadores_tabla(NA_character_, as.character(sub$ref_tabla[[1L]]))
      data.frame(
        nombre = paste0("fk_", nom),
        columnas = I(list(as.character(sub$column_name))),
        tabla_referenciada = ref_id,
        columnas_referenciadas = I(list(as.character(sub$ref_columna))),
        validada = validada,
        grado = grado,
        motivo = motivo,
        stringsAsFactors = FALSE
      )
    })
    fks <- do.call(rbind, filas_fk)
    rownames(fks) <- NULL
  }

  leido <- nrow(datos_col) > 0L
  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = leido, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = leido, motivo = if (leido) NA_character_ else clave$motivo),
      claves_foraneas = list(leido = leido, motivo = if (leido) NA_character_ else clave$motivo),
      not_null = list(leido = leido, motivo = if (leido) NA_character_ else clave$motivo)
    ),
    tipos_pragma = tipos_pragma
  )
}

.interpretar_catalogo_garantias_duckdb <- function(datos, campos, mapa_not_null,
                                                   vacio_pk) {
  vias <- .consultas_clave_primaria()
  via_pk <- vias[[which(vapply(vias, function(v) v$nombre == "duckdb_constraints",
                               logical(1L)))[[1L]]]]
  es_df <- inherits(datos, "data.frame") && nrow(datos) > 0L
  tipo_fila <- if (es_df && "cat_tipo" %in% names(datos)) {
    as.character(datos$cat_tipo)
  } else character()
  datos_con <- if (es_df) datos[tipo_fila == "con", , drop = FALSE] else data.frame()
  datos_idx <- if (es_df) datos[tipo_fila == "idx", , drop = FALSE] else data.frame()

  datos_pk <- if (nrow(datos_con)) {
    datos_con[as.character(datos_con$con_tipo) == "PRIMARY KEY", , drop = FALSE]
  } else data.frame()
  if (nrow(datos_pk)) {
    ord <- .orden_por_bytes(suppressWarnings(as.numeric(datos_pk$ordinal_position)))
    datos_pk <- datos_pk[ord, , drop = FALSE]
  }
  clave <- .interpretar_clave_primaria_dbi(datos_pk, via_pk, "duckdb", vacio_pk)

  # NOT NULL: combinar DESCRIBE (mapa_not_null) con duckdb_constraints() NOT NULL / PK
  mapa_nn <- mapa_not_null
  if (is.null(mapa_nn) && length(campos)) {
    mapa_nn <- stats::setNames(rep(FALSE, length(campos)), campos)
  }
  if (nrow(datos_con) && !is.null(mapa_nn)) {
    nn_cols <- unique(as.character(datos_con$column_name[
      as.character(datos_con$con_tipo) %in% c("NOT NULL", "PRIMARY KEY")
    ]))
    pos_nn <- .indice_nombre(nn_cols, names(mapa_nn))
    mapa_nn[pos_nn[!is.na(pos_nn)]] <- TRUE
  }
  not_null_df <- .construir_tabla_not_null_dbi(
    campos, mapa_nn, leido = !is.null(mapa_nn)
  )
  nn_map <- .normalizar_not_null_mapa(not_null_df)

  filas_uk <- list()
  if (nrow(datos_con)) {
    datos_u <- datos_con[as.character(datos_con$con_tipo) == "UNIQUE", , drop = FALSE]
    if (nrow(datos_u)) {
      grupos <- split(seq_len(nrow(datos_u)), as.character(datos_u$constraint_nombre))
      for (nom in .sort_por_bytes(names(grupos))) {
        sub <- datos_u[grupos[[nom]], , drop = FALSE]
        ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
        sub <- sub[ord, , drop = FALSE]
        cols <- as.character(sub$column_name)
        pos_nn <- .indice_nombre(cols, names(nn_map))
        todas_nn <- length(cols) > 0L && !anyNA(pos_nn) && all(nn_map[pos_nn])
        g <- .grado_clave_unica(
          enforced = TRUE, validated = TRUE, parcial = FALSE,
          filtro = NA_character_, diferible = FALSE, descendientes = FALSE,
          expresion = FALSE, deshabilitado = FALSE,
          semantica_null = "nulls_distinct", todas_not_null = todas_nn
        )
        filas_uk[[length(filas_uk) + 1L]] <- data.frame(
          nombre = paste0("unique_", nom),
          origen = "restriccion_unica",
          columnas = I(list(cols)),
          es_parcial = FALSE,
          filtro = NA_character_,
          semantica_null = "nulls_distinct",
          todas_not_null = todas_nn,
          grado = g$grado,
          motivo = g$motivo,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (nrow(datos_idx)) {
    no_pk <- as.character(datos_idx$con_tipo) != "PRIMARY KEY"
    sub_idx <- datos_idx[no_pk, , drop = FALSE]
    for (i in seq_len(nrow(sub_idx))) {
      nom <- as.character(sub_idx$constraint_nombre[[i]])
      sql_i <- as.character(sub_idx$idx_sql[[i]])
      cols <- .extraer_columnas_parentesis_sql(sql_i)
      parcial <- !is.na(sql_i) && grepl(.regex_sin_caja("\\bWHERE\\b"), sql_i, perl = TRUE)
      filtro <- if (parcial) .extraer_filtro_where_sql(sql_i) else NA_character_
      expresion <- !length(cols) || (length(campos) > 0L && anyNA(.resolver_columnas_dbi(cols, campos)))
      pos_nn <- .indice_nombre(cols, names(nn_map))
      todas_nn <- !expresion && length(cols) > 0L && !anyNA(pos_nn) && all(nn_map[pos_nn])
      g <- .grado_clave_unica(
        enforced = TRUE, validated = TRUE, parcial = parcial,
        filtro = filtro, diferible = FALSE, descendientes = FALSE,
        expresion = expresion, deshabilitado = FALSE,
        semantica_null = "nulls_distinct", todas_not_null = todas_nn
      )
      filas_uk[[length(filas_uk) + 1L]] <- data.frame(
        nombre = nom,
        origen = "indice_unico",
        columnas = I(list(cols)),
        es_parcial = parcial,
        filtro = filtro,
        semantica_null = "nulls_distinct",
        todas_not_null = todas_nn,
        grado = g$grado,
        motivo = g$motivo,
        stringsAsFactors = FALSE
      )
    }
  }
  unicas <- if (length(filas_uk)) {
    salida_u <- do.call(rbind, filas_uk)
    rownames(salida_u) <- NULL
    salida_u
  } else .claves_unicas_vacias_dbi()

  fks <- .claves_foraneas_vacias_dbi()
  if (nrow(datos_con)) {
    datos_f <- datos_con[as.character(datos_con$con_tipo) == "FOREIGN KEY", , drop = FALSE]
    if (nrow(datos_f)) {
      grupos <- split(seq_len(nrow(datos_f)), as.character(datos_f$constraint_nombre))
      filas_fk <- lapply(.sort_por_bytes(names(grupos)), function(nom) {
        sub <- datos_f[grupos[[nom]], , drop = FALSE]
        ord <- .orden_por_bytes(suppressWarnings(as.numeric(sub$ordinal_position)))
        sub <- sub[ord, , drop = FALSE]
        ref <- .parsear_fk_duckdb_texto(sub$con_texto[[1L]])
        ref_id <- if (is.null(ref)) NA_character_ else {
          .identificadores_tabla(ref$ref_esquema, ref$ref_tabla)
        }
        ref_cols <- if (is.null(ref)) character() else ref$ref_columnas
        data.frame(
          nombre = paste0("fk_", nom),
          columnas = I(list(as.character(sub$column_name))),
          tabla_referenciada = ref_id,
          columnas_referenciadas = I(list(ref_cols)),
          validada = TRUE,
          grado = "total",
          motivo = NA_character_,
          stringsAsFactors = FALSE
        )
      })
      fks <- do.call(rbind, filas_fk)
      rownames(fks) <- NULL
    }
  }

  list(
    clave = clave,
    claves_unicas = unicas,
    claves_foraneas = fks,
    not_null = not_null_df,
    estado_lectura = list(
      clave_primaria = list(leido = TRUE, motivo = clave$motivo %||% NA_character_),
      claves_unicas = list(leido = TRUE, motivo = NA_character_),
      claves_foraneas = list(leido = TRUE, motivo = NA_character_),
      not_null = list(leido = TRUE, motivo = NA_character_)
    )
  )
}

.catalogo_garantias_dbi <- function(conexion, tabla, esquema = NA_character_,
                                    campos = character(),
                                    not_null_columnas = NULL,
                                    tipo_objeto = NA_character_,
                                    presupuesto = NULL) {
  vacio_pk <- function(fuente, motivo, garantia = "desconocida", visible = NA) {
    list(
      columnas = character(), fuente = fuente, motivo = motivo,
      garantia = garantia,
      estado = .estado_clave_vacio(visible = visible)
    )
  }
  via <- .via_catalogo_garantias(conexion)
  motor <- .motor_clave_primaria(conexion)
  fallo_completo <- function(motivo) {
    clave <- vacio_pk(via, motivo, garantia = "desconocida", visible = NA)
    not_null_df <- .construir_tabla_not_null_dbi(
      campos, not_null_columnas, tipo_objeto = tipo_objeto,
      leido = !is.null(not_null_columnas),
      motivo_general = motivo
    )
    est <- .estado_lectura_catalogo_vacio(motivo)
    if (!is.null(not_null_columnas)) {
      est$not_null <- list(leido = TRUE, motivo = NA_character_)
    }
    list(
      clave = clave,
      catalogo = list(
        fuente = via, motor = motor, visible = NA, motivo = motivo,
        identificacion = .identificacion_catalogo_dbi(clave, .claves_unicas_vacias_dbi()),
        clave_primaria = list(
          columnas = character(), grado = "ninguna",
          garantia = clave$garantia, motivo = motivo
        ),
        claves_unicas = .claves_unicas_vacias_dbi(),
        claves_foraneas = .claves_foraneas_vacias_dbi(),
        not_null = not_null_df,
        estado_lectura = est
      )
    )
  }
  if (!length(tabla) || is.na(tabla) || !nzchar(tabla)) {
    return(fallo_completo("No se indic\u00f3 la tabla."))
  }

  if (via %in% c("all_constraints", "information_schema")) {
    clave <- .clave_primaria_dbi(conexion, tabla, esquema, presupuesto = presupuesto)
    motivo_no_cubierto <- paste0(
      "La v\u00eda `", via, "` sobre `", motor,
      "` s\u00f3lo consulta la clave primaria; las dem\u00e1s garant\u00edas del cat\u00e1logo no se leyeron."
    )
    not_null_df <- .construir_tabla_not_null_dbi(
      campos, not_null_columnas, tipo_objeto = tipo_objeto,
      leido = !is.null(not_null_columnas),
      motivo_general = motivo_no_cubierto
    )
    ident <- .identificacion_catalogo_dbi(clave, .claves_unicas_vacias_dbi())
    return(list(
      clave = clave,
      catalogo = list(
        fuente = via, motor = motor,
        visible = if (is.list(clave$estado)) clave$estado$visible else NA,
        motivo = motivo_no_cubierto,
        identificacion = ident,
        clave_primaria = list(
          columnas = clave$columnas,
          grado = if (identical(clave$garantia, "garantizada")) "total" else "ninguna",
          garantia = clave$garantia,
          motivo = clave$motivo %||% NA_character_
        ),
        claves_unicas = .claves_unicas_vacias_dbi(),
        claves_foraneas = .claves_foraneas_vacias_dbi(),
        not_null = not_null_df,
        estado_lectura = list(
          clave_primaria = list(leido = !is.na(clave$estado$visible),
                                motivo = clave$motivo %||% NA_character_),
          claves_unicas = list(leido = FALSE, motivo = motivo_no_cubierto),
          claves_foraneas = list(leido = FALSE, motivo = motivo_no_cubierto),
          not_null = list(
            leido = !is.null(not_null_columnas),
            motivo = if (is.null(not_null_columnas)) motivo_no_cubierto else NA_character_
          )
        )
      )
    ))
  }

  interpretado <- NULL
  if (identical(via, "show_index_mariadb")) {
    vias_pk <- .consultas_clave_primaria()
    via_si <- vias_pk[[which(vapply(vias_pk, function(v) v$nombre == "show_index",
                                    logical(1L)))[[1L]]]]
    resp_idx <- .consultar_dbi(
      conexion, via_si$sql(esquema, tabla), presupuesto, etapa = "clave_primaria"
    )
    resp_fk <- .consultar_dbi(
      conexion, .sql_fks_mariadb(esquema, tabla), presupuesto,
      etapa = "catalogo_garantias"
    )
    if (!isTRUE(resp_idx$ok)) {
      motivo <- paste0(
        "show_index: ", resp_idx$motivo,
        if (.error_permiso_clave(resp_idx$motivo, resp_idx$reconocido)) {
          " El mensaje parece relacionado con permisos, pero eso no alcanza para afirmarlo."
        } else ""
      )
      return(fallo_completo(motivo))
    }
    interpretado <- .interpretar_catalogo_garantias_mariadb(
      resp_idx$datos, resp_fk, campos, not_null_columnas, tipo_objeto, vacio_pk
    )
  } else {
    sql <- switch(via,
      pg_catalog = .sql_catalogo_garantias_pg(esquema, tabla),
      sys_catalog = .sql_catalogo_garantias_mssql(esquema, tabla),
      information_schema_mysql = .sql_catalogo_garantias_mysql(esquema, tabla),
      pragma = .sql_catalogo_garantias_sqlite(esquema, tabla),
      duckdb_catalog = .sql_catalogo_garantias_duckdb(esquema, tabla)
    )
    respuesta <- .consultar_dbi(conexion, sql, presupuesto, etapa = "clave_primaria")
    if (!isTRUE(respuesta$ok)) {
      motivo <- paste0(
        via, ": ", respuesta$motivo,
        if (.error_permiso_clave(respuesta$motivo, respuesta$reconocido)) {
          " El mensaje parece relacionado con permisos, pero eso no alcanza para afirmarlo."
        } else ""
      )
      return(fallo_completo(motivo))
    }
    interpretado <- switch(via,
      pg_catalog = .interpretar_catalogo_garantias_pg(
        respuesta$datos, campos, not_null_columnas, tipo_objeto, vacio_pk
      ),
      sys_catalog = .interpretar_catalogo_garantias_mssql(
        respuesta$datos, campos, not_null_columnas, tipo_objeto, vacio_pk
      ),
      information_schema_mysql = .interpretar_catalogo_garantias_mysql(
        respuesta$datos, campos, not_null_columnas, tipo_objeto, vacio_pk
      ),
      pragma = .interpretar_catalogo_garantias_sqlite(
        respuesta$datos, campos, vacio_pk
      ),
      duckdb_catalog = .interpretar_catalogo_garantias_duckdb(
        respuesta$datos, campos, not_null_columnas, vacio_pk
      )
    )
  }

  clave <- interpretado$clave
  if (length(clave$columnas) && length(campos)) {
    ajenas <- clave$columnas[is.na(.resolver_columnas_dbi(clave$columnas, campos))]
    if (length(ajenas)) clave <- .clave_de_otra_relacion(clave, ajenas)
  }
  unicas <- .filtrar_unicas_por_campos(interpretado$claves_unicas, campos)
  grado_pk <- if (identical(clave$garantia, "garantizada")) {
    "total"
  } else if (is.list(clave$estado) &&
             identical(clave$estado$unicidad, "garantizada") &&
             length(clave$columnas) > 0L) {
    "parcial"
  } else {
    "ninguna"
  }
  ident <- .identificacion_catalogo_dbi(clave, unicas)
  est_lec <- interpretado$estado_lectura
  motivos_lec <- vapply(est_lec, function(x) {
    if (!isTRUE(x$leido) && length(x$motivo) == 1L && !is.na(x$motivo)) {
      as.character(x$motivo)
    } else NA_character_
  }, character(1L))
  motivos_lec <- unique(motivos_lec[!is.na(motivos_lec)])
  motivo_cat <- if (length(motivos_lec)) paste(motivos_lec, collapse = " ") else NA_character_

  list(
    clave = clave,
    tipos_pragma = interpretado$tipos_pragma,
    catalogo = list(
      fuente = via,
      motor = motor,
      visible = if (is.list(clave$estado)) clave$estado$visible else NA,
      motivo = motivo_cat,
      identificacion = ident,
      clave_primaria = list(
        columnas = clave$columnas,
        grado = grado_pk,
        garantia = clave$garantia,
        motivo = clave$motivo %||% NA_character_
      ),
      claves_unicas = unicas,
      claves_foraneas = interpretado$claves_foraneas,
      not_null = interpretado$not_null,
      estado_lectura = est_lec
    )
  )
}

# Columnas simples con unicidad garantizada sobre valores no nulos (sirve para
# omitir `COUNT(DISTINCT col)` porque `COUNT(DISTINCT)` ignora NULL).
.columnas_unicidad_simple_dbi <- function(catalogo, campos) {
  salida <- list()
  if (!is.list(catalogo) || !length(campos)) return(salida)
  pk <- catalogo$clave_primaria
  if (is.list(pk) && length(pk$columnas) == 1L &&
      pk$grado %in% c("total", "parcial")) {
    pos <- .resolver_columnas_dbi(pk$columnas, campos)
    if (length(pos) == 1L && !is.na(pos)) {
      col <- campos[[pos]]
      salida[[col]] <- list(
        nombre = "clave_primaria_garantizada",
        exacta = TRUE,
        proporcion_distintos = 1,
        requiere_validos = !identical(pk$grado, "total"),
        motivo = paste(
          "La clave primaria simple est\u00e1 declarada y garantizada por el",
          "cat\u00e1logo; sus valores v\u00e1lidos son \u00fanicos."
        )
      )
    }
  }
  unicas <- catalogo$claves_unicas
  if (is.data.frame(unicas) && nrow(unicas)) {
    for (i in seq_len(nrow(unicas))) {
      cols <- as.character(unicas$columnas[[i]])
      if (length(cols) != 1L || isTRUE(unicas$es_parcial[[i]])) next
      if (!(unicas$grado[[i]] %in% c("total", "parcial"))) next
      pos <- .resolver_columnas_dbi(cols, campos)
      if (length(pos) != 1L || is.na(pos)) next
      col <- campos[[pos]]
      if (!is.null(salida[[col]])) next
      orig <- as.character(unicas$origen[[i]])
      nombre_fuente <- if (identical(orig, "restriccion_unica")) {
        "restriccion_unica_garantizada"
      } else {
        "indice_unico_garantizado"
      }
      salida[[col]] <- list(
        nombre = nombre_fuente,
        exacta = TRUE,
        proporcion_distintos = 1,
        requiere_validos = !isTRUE(unicas$todas_not_null[[i]]),
        motivo = paste0(
          "Unicidad garantizada por ",
          if (identical(orig, "restriccion_unica")) "restricci\u00f3n \u00fanica" else "\u00edndice \u00fanico",
          " (`", unicas$nombre[[i]], "`); sus valores v\u00e1lidos son \u00fanicos."
        )
      )
    }
  }
  salida
}

.columnas_not_null_garantizadas_dbi <- function(catalogo, campos) {
  if (!is.list(catalogo) || !is.data.frame(catalogo$not_null) ||
      !nrow(catalogo$not_null) || !length(campos)) {
    return(character())
  }
  nn <- catalogo$not_null
  cols <- as.character(nn$columna[
    nn$grado == "total" & .es_cierto_vec(nn$not_null)
  ])
  intersect(campos, cols)
}

# TRUE si `orden_muestra` contiene las columnas de una PK o clave/indice unico
# de grado total: garantiza filas unicas sin ejecutar `GROUP BY ... HAVING`.
.orden_garantizado_unico_dbi <- function(catalogo, orden_muestra, campos) {
  if (!is.list(catalogo) || !length(orden_muestra) || !length(campos)) {
    return(FALSE)
  }
  pos_orden <- .resolver_columnas_dbi(orden_muestra, campos)
  if (anyNA(pos_orden)) return(FALSE)
  orden_norm <- campos[pos_orden]
  pk <- catalogo$clave_primaria
  if (is.list(pk) && identical(pk$grado, "total") && length(pk$columnas)) {
    pos_pk <- .resolver_columnas_dbi(pk$columnas, campos)
    if (!anyNA(pos_pk) && all(campos[pos_pk] %in% orden_norm)) return(TRUE)
  }
  unicas <- catalogo$claves_unicas
  if (is.data.frame(unicas) && nrow(unicas)) {
    for (i in seq_len(nrow(unicas))) {
      if (!identical(unicas$grado[[i]], "total")) next
      cols <- as.character(unicas$columnas[[i]])
      pos_u <- .resolver_columnas_dbi(cols, campos)
      if (length(cols) && !anyNA(pos_u) && all(campos[pos_u] %in% orden_norm)) {
        return(TRUE)
      }
    }
  }
  FALSE
}
