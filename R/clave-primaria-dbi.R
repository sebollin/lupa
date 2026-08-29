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
        estado <- if (identical(motor, "mysql")) {
          ", t.enforced AS constraint_enforced"
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
          "AND t.table_name = ", .texto_sql_clave(toupper(tabla)),
          if (!is.na(esquema)) {
            paste0(" AND t.owner = ", .texto_sql_clave(toupper(esquema)))
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
            ""
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

.error_permiso_clave <- function(motivo) {
  if (is.null(motivo) || !length(motivo) || all(is.na(motivo))) {
    return(FALSE)
  }
  texto <- tolower(paste(as.character(motivo), collapse = " "))
  grepl(
    "permission|privilege|permiso|privileg|access denied|command denied|denegad|not authorized|unauthorized",
    texto
  )
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
  posicion <- match(candidatos, nombres)
  if (all(is.na(posicion))) {
    posicion <- match(tolower(candidatos), tolower(nombres))
  }
  posicion <- posicion[!is.na(posicion)]
  if (!length(posicion)) return(NULL)
  datos[[posicion[[1L]]]]
}

.estado_clave <- function(valor, tipo = c("si_no", "oraculo", "diferible")) {
  tipo <- match.arg(tipo)
  if (is.null(valor) || !length(valor)) return(NA)
  if (is.logical(valor)) return(if (is.na(valor[[1L]])) NA else valor[[1L]])
  texto <- toupper(trimws(as.character(valor[[1L]])))
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
  # declarativo, y sólo la primera deja filas fuera del alcance de la clave.
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
  list(garantia = "desconocida", estado = estado)
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
.clave_primaria_dbi <- function(conexion, tabla, esquema = NA_character_,
                                presupuesto = NULL) {
  vacio <- function(fuente, motivo, garantia = "desconocida",
                    visible = NA) {
    list(
      columnas = character(), fuente = fuente, motivo = motivo,
      garantia = garantia,
      estado = list(
        visible = visible, aplicada = NA, validada = NA,
        unicidad = NA_character_, unicidad_aplica_a = NA_character_,
        ausencia_de_nulos = NA_character_,
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
    return(vacio(
      via$nombre, paste0(
        via$nombre, ": ", respuesta$motivo,
        if (.error_permiso_clave(respuesta$motivo)) {
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
  if (!inherits(datos, "data.frame") || !nrow(datos)) {
    if (identical(via$nombre, "pragma")) {
      # Cero filas pidiendo todas las columnas: la tabla no esta.
      return(vacio(via$nombre, paste0(
        via$nombre, ": la tabla no existe o no es visible, ",
        "asi que no se pudo preguntar por su clave."
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
        via$nombre, ": no devolvio filas; la clave puede no estar declarada ",
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
        "show_index: la consulta no devolvio el campo `Key_name`.",
        visible = NA
      ))
    }
    primarias <- which(
      !is.na(nombre_clave) &
        toupper(trimws(as.character(nombre_clave))) == "PRIMARY"
    )
    if (!length(primarias)) {
      return(vacio(
        via$nombre, motivo = NA_character_, garantia = "no_declarada",
        visible = TRUE
      ))
    }
    datos <- datos[primarias, , drop = FALSE]
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
    orden <- partes[order(posicion[partes])]
    garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
    return(list(
      columnas = as.character(columna)[orden], fuente = via$nombre,
      motivo = NA_character_, garantia = garantia$garantia,
      estado = garantia$estado
    ))
  }
  if (identical(via$nombre, "show_index")) {
    orden <- order(posicion, na.last = TRUE)
    garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
    return(list(
      columnas = as.character(columna)[orden], fuente = via$nombre,
      motivo = NA_character_, garantia = garantia$garantia,
      estado = garantia$estado
    ))
  }
  garantia <- .garantia_clave_primaria(datos, via$nombre, motor)
  list(
    columnas = as.character(columna), fuente = via$nombre,
    motivo = NA_character_, garantia = garantia$garantia,
    estado = garantia$estado
  )
}
