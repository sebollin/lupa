# Fuente incremental DBI para la etapa I1.
#
# Esta via es optativa: `perfilar_dbi()` solo la usa cuando recibe
# `bloque_filas`. El camino historico de agregados SQL y su bloque de muestra
# queda separado para que una corrida existente no cambie de resultado.

.MOTIVO_DBI_FETCH_NO_INCREMENTAL <- "no_disponible:dbfetch_no_incremental"
.ALIAS_LOCALIZADOR_DBI <- "__lupa_row_locator"
.ALIAS_ORDINAL_DBI <- "__lupa_row_number"

.validar_bloque_filas_dbi <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    .detener_dbi(
      "lupa_error_argumento_dbi",
      "`bloque_filas` debe ser un entero positivo finito o NULL."
    )
  }
  as.numeric(x)
}

.motor_fuente_bloques_dbi <- function(conexion) {
  if (exists(".motor_clave_primaria", mode = "function")) {
    return(.motor_clave_primaria(conexion))
  }
  clase <- paste(class(conexion), collapse = " ")
  if (grepl("duckdb", clase, ignore.case = TRUE)) return("duckdb")
  if (grepl("sqlite", clase, ignore.case = TRUE)) return("sqlite")
  if (grepl("postgres|pqc|rpostgres", clase, ignore.case = TRUE)) {
    return("postgresql")
  }
  "desconocido"
}

.capacidad_fuente_bloques_dbi <- function(conexion) {
  motor <- .motor_fuente_bloques_dbi(conexion)
  clase <- paste(class(conexion), collapse = " ")
  info <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) list())
  texto_info <- paste(unlist(info, use.names = FALSE), collapse = " ")
  if (identical(motor, "desconocido") &&
      grepl("duckdb", paste(clase, texto_info), ignore.case = TRUE)) {
    motor <- "duckdb"
  }
  retencion <- c(
    sqlite = 0,
    postgresql = 0.168,
    mariadb = 0.088,
    mysql = 0.041,
    sqlserver = 0.066,
    duckdb = 0.86
  )
  version <- info$server.version %||% info$db.version %||% NA_character_
  version <- as.character(version)[[1L]]
  if (identical(motor, "postgresql") && grepl("^9\\.", version)) {
    retencion[["postgresql"]] <- 0.165
  }
  disponible <- motor %in% names(retencion) && !identical(motor, "duckdb")
  motivo <- if (identical(motor, "duckdb")) {
    .MOTIVO_DBI_FETCH_NO_INCREMENTAL
  } else if (!disponible) {
    .MOTIVO_DBI_FETCH_NO_INCREMENTAL
  } else {
    NA_character_
  }
  list(
    motor = motor,
    driver = paste(class(conexion), collapse = "/"),
    version_driver = version,
    incremental = disponible,
    disponible = disponible,
    motivo = motivo,
    retencion = if (motor %in% names(retencion)) unname(retencion[[motor]]) else NA_real_,
    retencion_driver = if (motor %in% names(retencion)) unname(retencion[[motor]]) else NA_real_,
    retencion_fuente = if (motor %in% names(retencion)) {
      "matriz_retencion_driver_v2"
    } else {
      "no_demostrada"
    },
    retenido_por_fetch = disponible
  )
}

.matriz_capacidades_fuente_bloques_dbi <- function() {
  data.frame(
    driver = c("SQLite / RSQLite", "PostgreSQL 16 / RPostgres",
      "PostgreSQL 9.3 / RPostgres", "MariaDB / RMariaDB",
      "MySQL 8.4 / RMySQL", "DuckDB / duckdb",
      "SQL Server 2022 / FreeTDS"),
    motor = c("sqlite", "postgresql", "postgresql", "mariadb", "mysql",
      "duckdb", "sqlserver"),
    version = c("", "16", "9.3", "11.8", "8.4", "", "2022"),
    retencion_driver = c(0, 0.168, 0.165, 0.088, 0.041, 0.86, 0.066),
    dbfetch_incremental = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    motivo_no_disponible = c(NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, .MOTIVO_DBI_FETCH_NO_INCREMENTAL,
      NA_character_),
    fuente = "matriz_retencion_driver_v2",
    stringsAsFactors = FALSE
  )
}

.nombre_tabla_catalogo_dbi <- function(tabla) {
  texto <- tryCatch(.texto_tabla_dbi(tabla), error = function(e) "")
  if (!length(texto) || is.na(texto)) "" else texto
}

.tipos_textuales_fuente_dbi <- function(campos, prototipo = NULL,
                                        tipos = NULL) {
  vapply(seq_along(campos), function(i) {
    valor <- if (!is.null(prototipo) && i <= length(prototipo)) {
      prototipo[[i]]
    } else NULL
    tipo <- if (!is.null(tipos) && i <= length(tipos)) tipos[[i]] else NA_character_
    is.character(valor) || is.factor(valor) ||
      (!is.null(tipo) && length(tipo) == 1L &&
       grepl("char|text|clob|varchar|citext", tipo, ignore.case = TRUE))
  }, logical(1L))
}

.collation_fuente_bloques_dbi <- function(conexion, tabla, campos, motor,
                                          prototipo = NULL, tipos = NULL,
                                          presupuesto = NULL) {
  textual <- .tipos_textuales_fuente_dbi(campos, prototipo, tipos)
  nombres <- stats::setNames(rep("no_aplica", length(campos)), campos)
  determinismo <- stats::setNames(rep(TRUE, length(campos)), campos)
  demostrada <- stats::setNames(rep(TRUE, length(campos)), campos)
  fuente <- stats::setNames(rep("tipo_no_textual", length(campos)), campos)
  if (!any(textual)) {
    return(list(
      collation = nombres, determinista = determinismo,
      demostrada = demostrada, fuente = fuente
    ))
  }

  if (identical(motor, "sqlite")) {
    nombres[textual] <- "BINARY/default"
    fuente[textual] <- "sqlite_semantica_determinista"
    return(list(
      collation = nombres, determinista = determinismo,
      demostrada = demostrada, fuente = fuente
    ))
  }

  if (identical(motor, "postgresql")) {
    referencia <- .nombre_tabla_catalogo_dbi(tabla)
    sql <- paste0(
      "SELECT a.attname AS column_name, co.collname AS collation_name, ",
      "co.collisdeterministic AS collation_deterministic ",
      "FROM pg_catalog.pg_attribute a ",
      "JOIN pg_catalog.pg_class c ON c.oid = a.attrelid ",
      "JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ",
      "LEFT JOIN pg_catalog.pg_collation co ON co.oid = a.attcollation ",
      "WHERE c.oid = to_regclass(",
      as.character(DBI::dbQuoteString(conexion, referencia)),
      ") AND a.attnum > 0 AND NOT a.attisdropped AND a.attname IN (",
      paste(vapply(campos[textual], function(campo) {
        as.character(DBI::dbQuoteString(conexion, campo))
      }, character(1L)), collapse = ", "), ")"
    )
    respuesta <- tryCatch(
      .consultar_dbi(conexion, sql, presupuesto, etapa = "sonda_collation_dbi"),
      error = function(e) list(ok = FALSE, motivo = conditionMessage(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- tolower(names(datos))
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      determinista <- which(columnas == "collation_deterministic")[[1L]]
      for (i in seq_len(nrow(datos))) {
        campo <- as.character(datos[[nombre]][[i]])
        posicion <- match(tolower(campo), tolower(campos))
        if (is.na(posicion) || !textual[[posicion]]) next
        nombres[[posicion]] <- as.character(datos[[colacion]][[i]])
        valor <- datos[[determinista]][[i]]
        valor <- if (is.logical(valor)) valor else {
          tolower(as.character(valor)) %in% c("t", "true", "1", "yes")
        }
        determinismo[[posicion]] <- isTRUE(valor)
        demostrada[[posicion]] <- TRUE
        fuente[[posicion]] <- "pg_catalog.pg_collation.collisdeterministic"
      }
    } else {
      demostrada[textual] <- FALSE
      determinismo[textual] <- FALSE
      fuente[textual] <- "pg_catalog_collation_no_demostrada"
    }
    return(list(
      collation = nombres, determinista = determinismo,
      demostrada = demostrada, fuente = fuente
    ))
  }

  if (motor %in% c("mariadb", "mysql")) {
    piezas <- tryCatch(.piezas_tabla_cardinalidad_dbi(tabla),
                       error = function(e) list(esquema = NA_character_,
                                                tabla = ""))
    filtro_esquema <- if (!is.na(piezas$esquema) && nzchar(piezas$esquema)) {
      paste0("table_schema = ",
             as.character(DBI::dbQuoteString(conexion, piezas$esquema)))
    } else "table_schema = DATABASE()"
    sql <- paste0(
      "SELECT column_name, collation_name FROM information_schema.columns ",
      "WHERE ", filtro_esquema, " AND table_name = ",
      as.character(DBI::dbQuoteString(conexion, piezas$tabla)),
      " AND column_name IN (",
      paste(vapply(campos[textual], function(campo) {
        as.character(DBI::dbQuoteString(conexion, campo))
      }, character(1L)), collapse = ", "), ")"
    )
    respuesta <- tryCatch(
      .consultar_dbi(conexion, sql, presupuesto, etapa = "sonda_collation_dbi"),
      error = function(e) list(ok = FALSE, motivo = conditionMessage(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- tolower(names(datos))
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      for (i in seq_len(nrow(datos))) {
        posicion <- match(tolower(as.character(datos[[nombre]][[i]])),
                          tolower(campos))
        if (is.na(posicion) || !textual[[posicion]]) next
        colacion_actual <- as.character(datos[[colacion]][[i]])
        nombres[[posicion]] <- colacion_actual
        determinismo[[posicion]] <- grepl("(_bin2?|binary)$",
                                           colacion_actual, ignore.case = TRUE)
        demostrada[[posicion]] <- TRUE
        fuente[[posicion]] <- paste0(
          "information_schema.columns.collation_name:",
          if (isTRUE(determinismo[[posicion]])) "binaria" else "no_binaria"
        )
      }
    } else {
      demostrada[textual] <- FALSE
      determinismo[textual] <- FALSE
      fuente[textual] <- "information_schema_collation_no_demostrada"
    }
    return(list(
      collation = nombres, determinista = determinismo,
      demostrada = demostrada, fuente = fuente
    ))
  }

  if (identical(motor, "sqlserver")) {
    referencia <- .nombre_tabla_catalogo_dbi(tabla)
    sql <- paste0(
      "SELECT c.name AS column_name, c.collation_name, ",
      "CASE WHEN c.collation_name LIKE '%_BIN' OR ",
      "c.collation_name LIKE '%_BIN2' THEN 1 ELSE 0 END AS ",
      "collation_deterministic FROM sys.columns c ",
      "WHERE c.object_id = OBJECT_ID(",
      as.character(DBI::dbQuoteString(conexion, referencia)), ") AND c.name IN (",
      paste(vapply(campos[textual], function(campo) {
        as.character(DBI::dbQuoteString(conexion, campo))
      }, character(1L)), collapse = ", "), ")"
    )
    respuesta <- tryCatch(
      .consultar_dbi(conexion, sql, presupuesto, etapa = "sonda_collation_dbi"),
      error = function(e) list(ok = FALSE, motivo = conditionMessage(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- tolower(names(datos))
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      determinista <- which(columnas == "collation_deterministic")[[1L]]
      for (i in seq_len(nrow(datos))) {
        posicion <- match(tolower(as.character(datos[[nombre]][[i]])),
                          tolower(campos))
        if (is.na(posicion) || !textual[[posicion]]) next
        colacion_actual <- as.character(datos[[colacion]][[i]])
        nombres[[posicion]] <- colacion_actual
        valor <- datos[[determinista]][[i]]
        determinismo[[posicion]] <- as.character(valor) %in% c("1", "TRUE", "T")
        demostrada[[posicion]] <- TRUE
        fuente[[posicion]] <- "sys.columns.collation_name_binaria"
      }
    } else {
      demostrada[textual] <- FALSE
      determinismo[textual] <- FALSE
      fuente[textual] <- "sys.columns_collation_no_demostrada"
    }
    return(list(
      collation = nombres, determinista = determinismo,
      demostrada = demostrada, fuente = fuente
    ))
  }

  # Para un motor cuya semantica de collation no se pudo consultar no se
  # afirma estabilidad para una clave textual. Una clave numerica no depende
  # de collation y conserva la garantia del catalogo.
  demostrada[textual] <- FALSE
  determinismo[textual] <- FALSE
  nombres[textual] <- "no_demostrada"
  fuente[textual] <- "motor_sin_sonda_collation_determinismo"
  list(
    collation = nombres, determinista = determinismo,
    demostrada = demostrada, fuente = fuente
  )
}

.sondar_expresion_fuente_bloques_dbi <- function(conexion, sql,
                                                 presupuesto = NULL) {
  respuesta <- tryCatch(
    .consultar_dbi(conexion, sql, presupuesto, etapa = "sonda_fuente_bloques"),
    error = function(e) list(ok = FALSE, motivo = conditionMessage(e))
  )
  isTRUE(respuesta$ok)
}

.fuente_bloques_dbi <- function(conexion, tabla, tabla_sql = NULL,
                               campos = NULL, campos_sql = NULL,
                               prototipo = NULL, tipos = NULL,
                               orden_muestra = NULL, orden_sql = NULL,
                               clave = NULL, dialecto = NULL,
                               presupuesto = NULL, bloque_filas = 10000L,
                               snapshot_id = NULL) {
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  capacidad <- .capacidad_fuente_bloques_dbi(conexion)
  if (is.null(tabla_sql)) {
    tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  }
  if (is.null(campos)) campos <- DBI::dbListFields(conexion, tabla)
  campos <- as.character(campos)
  if (is.null(campos_sql)) {
    campos_sql <- vapply(campos, function(campo) {
      as.character(DBI::dbQuoteIdentifier(conexion, campo))
    }, character(1L), USE.NAMES = FALSE)
  }
  if (length(campos) != length(campos_sql)) {
    stop("`campos` y `campos_sql` deben tener el mismo largo.", call. = FALSE)
  }
  if (!is.null(orden_muestra) && length(orden_muestra)) {
    orden_muestra <- as.character(orden_muestra)
    posiciones <- if (exists(".resolver_columnas_dbi", mode = "function")) {
      .resolver_columnas_dbi(orden_muestra, campos)
    } else match(tolower(orden_muestra), tolower(campos))
    if (anyNA(posiciones)) {
      stop("`orden_muestra` contiene columnas inexistentes.", call. = FALSE)
    }
    orden_muestra <- campos[posiciones]
    orden_sql <- vapply(orden_muestra, function(campo) {
      as.character(DBI::dbQuoteIdentifier(conexion, campo))
    }, character(1L), USE.NAMES = FALSE)
  } else if (is.null(orden_sql)) {
    orden_sql <- character()
  }

  motor <- capacidad$motor
  if (!isTRUE(capacidad$disponible)) {
    orden <- paste0(
      "dbi-v1|metodo=resultset_no_reproducible|collation=no_demostrada",
      "|deterministic=FALSE|representation=fetch_order"
    )
    fuente_id <- paste0(
      "dbi:", .nombre_tabla_catalogo_dbi(tabla), "|projection=",
      paste(campos, collapse = ",")
    )
    return(structure(list(
      disponible = FALSE, capacidad = capacidad, motivo = capacidad$motivo,
      consulta = NA_character_, orden_sql = character(), orden_id = orden,
      metodo_orden = "resultset_no_reproducible", estable = FALSE,
      snapshot_id = if (is.null(snapshot_id)) NA_character_ else as.character(snapshot_id),
      fuente_id = fuente_id, tabla = tabla, tabla_sql = tabla_sql,
      campos = campos, campos_sql = campos_sql, localizador = NULL,
      clave = clave, bloque_filas = bloque_filas,
      collation = NULL, plan = NULL
    ), class = "fuente_bloques_dbi"))
  }

  if (is.null(clave) && exists(".clave_primaria_dbi", mode = "function")) {
    piezas <- tryCatch(.piezas_tabla_cardinalidad_dbi(tabla),
                       error = function(e) list(esquema = NA_character_,
                                                tabla = .nombre_tabla_catalogo_dbi(tabla)))
    clave <- tryCatch(
      .clave_primaria_dbi(conexion, piezas$tabla, piezas$esquema,
                          presupuesto = presupuesto),
      error = function(e) NULL
    )
  }
  if (is.null(clave)) {
    clave <- list(columnas = character(), garantia = "desconocida")
  }
  pk <- as.character(clave$columnas %||% character())
  if (length(pk)) {
    posiciones_pk <- if (exists(".resolver_columnas_dbi", mode = "function")) {
      .resolver_columnas_dbi(pk, campos)
    } else match(tolower(pk), tolower(campos))
    pk <- if (all(!is.na(posiciones_pk))) campos[posiciones_pk] else character()
  }
  colaciones <- .collation_fuente_bloques_dbi(
    conexion, tabla, campos, motor, prototipo, tipos, presupuesto
  )
  textual_pk <- length(pk) > 0L && any(.tipos_textuales_fuente_dbi(
    pk, if (is.null(prototipo)) NULL else prototipo[match(pk, campos)],
    if (is.null(tipos)) NULL else tipos[match(pk, campos)]
  ))
  pk_pos <- match(pk, campos)
  pk_determinista <- !textual_pk || (
    all(colaciones$demostrada[pk]) && all(colaciones$determinista[pk])
  )
  orden_textual_no_determinista <- textual_pk && !isTRUE(pk_determinista)
  garantia_pk <- identical(clave$garantia, "garantizada") || (
    is.list(clave$estado) && identical(clave$estado$unicidad, "garantizada")
  )
  usa_pk <- length(pk) > 0L &&
    isTRUE(garantia_pk) && !isTRUE(orden_textual_no_determinista)

  localizador <- NULL
  if (!usa_pk && !isTRUE(orden_textual_no_determinista)) {
    candidatos <- switch(
      motor,
      sqlite = list(sql = "rowid", nombre = "rowid", fuente = "sqlite_rowid"),
      postgresql = list(sql = "ctid", nombre = "ctid", fuente = "postgresql_ctid"),
      oracle = list(sql = "ROWID", nombre = "ROWID", fuente = "oracle_rowid"),
      NULL
    )
    if (!is.null(candidatos)) {
      prueba <- paste0(
        "SELECT ", candidatos$sql, " AS ",
        as.character(DBI::dbQuoteIdentifier(conexion, .ALIAS_LOCALIZADOR_DBI)),
        " FROM ", tabla_sql, " WHERE 1 = 0"
      )
      if (.sondar_expresion_fuente_bloques_dbi(conexion, prueba, presupuesto)) {
        localizador <- candidatos
      }
    }
  }

  metodo <- "resultset_no_reproducible"
  estable <- FALSE
  orden_componentes <- character()
  orden_expresiones <- character()
  determinismos <- logical()
  colaciones_orden <- character()
  if (usa_pk) {
    metodo <- "pk"
    orden_expresiones <- vapply(pk, function(campo) {
      as.character(DBI::dbQuoteIdentifier(conexion, campo))
    }, character(1L), USE.NAMES = FALSE)
    orden_componentes <- pk
    determinismos <- colaciones$determinista[pk]
    colaciones_orden <- colaciones$collation[pk]
    estable <- TRUE
  } else if (!is.null(localizador)) {
    metodo <- "row_locator"
    orden_expresiones <- localizador$sql
    orden_componentes <- localizador$nombre
    determinismos <- TRUE
    colaciones_orden <- "no_aplica"
  } else if (!isTRUE(orden_textual_no_determinista)) {
    # ROW_NUMBER() permite conservar ordinales de una lectura que el motor
    # entrega, pero no convierte ese orden en reproducible. La sonda se hace
    # sobre cero filas y no es un fallback a dbGetQuery().
    orden_expresiones <- character()
    row_number <- paste0(
      "ROW_NUMBER() OVER () AS ",
      as.character(DBI::dbQuoteIdentifier(conexion, .ALIAS_ORDINAL_DBI))
    )
    prueba <- paste0(
      "SELECT ", row_number, " FROM ", tabla_sql, " WHERE 1 = 0"
    )
    if (.sondar_expresion_fuente_bloques_dbi(conexion, prueba, presupuesto)) {
      metodo <- "row_number"
      orden_componentes <- "ROW_NUMBER() OVER ()"
      determinismos <- FALSE
      colaciones_orden <- "no_aplica"
    }
  }
  if (!usa_pk && is.null(localizador) && identical(metodo, "resultset_no_reproducible")) {
    orden_componentes <- "fetch_order"
    determinismos <- FALSE
    colaciones_orden <- "no_demostrada"
  }
  orden_id <- paste0(
    "dbi-v1|metodo=", metodo,
    "|expr=", if (length(orden_componentes)) paste(orden_componentes, collapse = ",") else "none",
    "|collation=", paste(colaciones_orden, collapse = ","),
    "|deterministic=", if (length(determinismos) && all(determinismos)) "TRUE" else "FALSE",
    "|representation=", if (identical(metodo, "row_locator")) localizador$fuente else "R_fetch_ordinal"
  )
  if (textual_pk && !isTRUE(pk_determinista)) {
    orden_id <- paste0(
      orden_id, "|pk_collation=", paste(colaciones$collation[pk], collapse = ","),
      "|pk_deterministic=FALSE"
    )
  }
  proyeccion <- campos_sql
  if (identical(metodo, "row_locator")) {
    proyeccion <- c(
      proyeccion,
      paste0(localizador$sql, " AS ",
             as.character(DBI::dbQuoteIdentifier(conexion, .ALIAS_LOCALIZADOR_DBI)))
    )
  }
  if (identical(metodo, "row_number")) {
    proyeccion <- c(
      proyeccion,
      paste0(
        "ROW_NUMBER() OVER () AS ",
        as.character(DBI::dbQuoteIdentifier(conexion, .ALIAS_ORDINAL_DBI))
      )
    )
  }
  consulta <- paste0(
    "SELECT ", paste(proyeccion, collapse = ", "), " FROM ", tabla_sql,
    if (length(orden_expresiones)) paste0(
      " ORDER BY ", paste(orden_expresiones, collapse = ", ")
    ) else ""
  )
  fuente_id <- paste0(
    "dbi:", .nombre_tabla_catalogo_dbi(tabla), "|projection=",
    paste(campos, collapse = ","), "|order=", orden_id
  )
  snapshot <- if (is.null(snapshot_id)) NA_character_ else as.character(snapshot_id)
  if (identical(metodo, "row_locator") && length(snapshot) == 1L &&
      !is.na(snapshot) && nzchar(snapshot)) {
    estable <- TRUE
  }
  estructura <- list(
    disponible = TRUE, capacidad = capacidad, motivo = NA_character_,
    consulta = consulta,
    proyeccion_sql = proyeccion,
    orden_sql = if (length(orden_expresiones)) {
      paste0("ORDER BY ", paste(orden_expresiones, collapse = ", "))
    } else NA_character_,
    orden_expresiones = orden_expresiones,
    orden_id = orden_id, metodo_orden = metodo, estable = estable,
    snapshot_id = snapshot, snapshot = list(
      demostrado = !is.na(snapshot) && nzchar(snapshot),
      id = snapshot,
      motivo = if (is.na(snapshot)) "snapshot_sostenido_no_demostrado" else NA_character_
    ),
    fuente_id = fuente_id, tabla = tabla, tabla_sql = tabla_sql,
    campos = campos, campos_sql = campos_sql,
    localizador = localizador, clave = clave,
    pk = pk, collation = list(
      por_columna = colaciones$collation,
      determinista = colaciones$determinista,
      demostrada = colaciones$demostrada,
      fuente = colaciones$fuente,
      orden = colaciones_orden
    ),
    bloque_filas = bloque_filas,
    plan = list(
      pagado = FALSE, fuente = consulta, orden = orden_id,
      snapshot_id = snapshot, bloques_solicitados = NA_integer_,
      bloques = list(
        filas_pedidas = NA_real_, objetivo = bloque_filas,
        minimo = 1L, maximo = bloque_filas, bytes_esperados = NA_real_
      ),
      pasadas = list(primera = "dbSendQuery + dbFetch", valor = "acumuladores I1",
                     indice = "no_solicitado", lsh = "no_solicitado",
                     materializacion = "fuera_de_alcance_I1"),
      muestra_diagnostica = "perfil_muestra acotado, si se solicita",
      costo = list(consultas_sql = 1L, resultsets = 1L,
                   fetches = NA_integer_, filas = NA_real_, bytes = NA_real_)
    )
  )
  if (textual_pk && !isTRUE(pk_determinista)) {
    estructura$motivo <- "resultset_no_reproducible:collation_no_determinista"
  }
  if (identical(metodo, "resultset_no_reproducible") && textual_pk) {
    estructura$segunda_pasada <- list(
      estado = "no_disponible",
      motivo = "snapshot_sostenido_no_demostrado"
    )
  } else if (identical(metodo, "row_locator")) {
    estructura$segunda_pasada <- list(
      estado = "no_demostrada",
      motivo = "row_locator_requiere_verificacion_ordinal_fila",
      localizador = localizador$nombre
    )
  }
  structure(estructura, class = "fuente_bloques_dbi")
}

.valor_columna_bloque_dbi <- function(datos, nombre, posicion = NULL) {
  if (!inherits(datos, "data.frame")) return(NULL)
  nombres <- names(datos)
  indice <- if (!is.null(posicion)) posicion else match(nombre, nombres)
  if (is.na(indice) || !length(indice)) {
    indice <- match(tolower(nombre), tolower(nombres))
  }
  if (is.na(indice) || !length(indice)) NULL else datos[[indice]]
}

.entero_publico_bloques_dbi <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x > .Machine$integer.max) return(x)
  as.integer(x)
}

.estado_no_disponible_bloques_dbi <- function(motivo) {
  list(
    estado = "no_disponible", exacto = NA, resultado = NULL,
    motivo = motivo, como_resolverlo = paste(
      "Usar un driver con retencion incremental demostrada o mantener la via",
      "SQL existente; no se reemplaza silenciosamente por dbGetQuery()."
    ), cota = NULL, alcance = NULL, bytes_retenidos = 0
  )
}

.familias_fuente_bloques_dbi <- function(metricas, campos, prototipo, tipos,
                                         incluir_valores = TRUE,
                                         max_bytes_procesamiento =
                                           .MAX_BYTES_ESTADO_BLOQUES) {
  salida <- list()
  # `Inf` significa sin presupuesto de estado retenido. El acumulador de
  # bloques conserva un tope finito por seguridad cuando se lo invoca solo;
  # aqui la API DBI ya validó explicitamente que el usuario pidió `Inf`, por
  # lo que no corresponde reintroducir ese tope silenciosamente.
  max_bytes_estado <- if (is.infinite(max_bytes_procesamiento)) 1e15 else
    max_bytes_procesamiento
  for (i in seq_along(campos)) {
    campo <- campos[[i]]
    tipo <- if (!is.null(prototipo) && i <= length(prototipo)) {
      typeof(prototipo[[i]])
    } else "character"
    if ("validos" %in% metricas) {
      salida[[paste(campo, "conteos", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, tipo, familia = "conteos",
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          incluir_ausentes = TRUE, max_bytes = max_bytes_estado
        )
    }
    if (.tipos_textuales_fuente_dbi(
      campo, if (is.null(prototipo)) NULL else prototipo[i],
      if (is.null(tipos)) NULL else tipos[i]
    )) {
      salida[[paste(campo, "longitudes", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, "character", familia = "longitudes",
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          max_bytes = max_bytes_estado
        )
    }
    es_numerico <- .es_numerico_dbi(
      if (!is.null(prototipo) && i <= length(prototipo)) prototipo[[i]] else NULL,
      if (!is.null(tipos) && i <= length(tipos)) tipos[[i]] else NA_character_
    )
    if (es_numerico && any(c("basicos", "desvio") %in% metricas)) {
      salida[[paste(campo, "cuantitativos", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, tipo, familia = "cuantitativos",
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          max_bytes = max_bytes_estado
        )
    }
    if ("distintos" %in% metricas) {
      salida[[paste(campo, "distintos", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, tipo, familia = "distintos",
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          incluir_ausentes = FALSE, max_bytes = max_bytes_estado
        )
    }
  }
  salida
}

.absorber_bloque_fuente_dbi <- function(acumuladores, datos, inicio, fin) {
  if (!length(acumuladores)) return(acumuladores)
  for (nombre in names(acumuladores)) {
    partes <- strsplit(nombre, "\u001f", fixed = TRUE)[[1L]]
    campo <- partes[[1L]]
    valor <- .valor_columna_bloque_dbi(datos, campo)
    if (is.null(valor)) {
      acumuladores[[nombre]] <- .marcar_fallo_acumulador(
        acumuladores[[nombre]], "columna_fuente_no_devuelta"
      )
      next
    }
    bloque <- list(
      valores = valor, ordinal_inicio = inicio, ordinal_fin = fin,
      aplicable = rep(TRUE, length(valor))
    )
    acumuladores[[nombre]] <- .absorber_acumulador(
      acumuladores[[nombre]], bloque
    )
  }
  acumuladores
}

.finalizar_bloques_fuente_dbi <- function(acumuladores) {
  lapply(acumuladores, .finalizar_acumulador)
}

.publicar_familias_bloques_dbi <- function(sobres) {
  if (!length(sobres)) return(list())
  salida <- list()
  for (nombre in names(sobres)) {
    partes <- strsplit(nombre, "\u001f", fixed = TRUE)[[1L]]
    if (length(partes) != 2L) next
    if (is.null(salida[[partes[[1L]]]])) salida[[partes[[1L]]]] <- list()
    salida[[partes[[1L]]]][[partes[[2L]]]] <- sobres[[nombre]]
  }
  salida
}

.resumen_metrica_bloques_dbi <- function(sobre, familia, metrica,
                                         incluir_valores = TRUE) {
  if (is.null(sobre) || identical(sobre$estado, "no_disponible")) {
    return(list(valor = NA, estado = "no_disponible",
                motivo = sobre$motivo %||% "resultado_no_disponible"))
  }
  resultado <- sobre$resultado
  if (identical(familia, "conteos")) {
    valor <- switch(
      metrica,
      n_validos = resultado$n_validos,
      n_faltantes = resultado$n_faltantes,
      prop_faltantes = NA_real_,
      n_ceros = resultado$n_ceros,
      n_negativos = resultado$n_negativos,
      NA_real_
    )
    return(list(valor = valor, estado = "calculado", motivo = NA_character_))
  }
  if (identical(familia, "cuantitativos")) {
    valor <- switch(
      metrica,
      minimo = if (isTRUE(incluir_valores)) resultado$minimo else NA_real_,
      maximo = if (isTRUE(incluir_valores)) resultado$maximo else NA_real_,
      media = resultado$media,
      desvio = resultado$desvio,
      n_ceros = resultado$n_ceros,
      n_negativos = resultado$n_negativos,
      NA_real_
    )
    return(list(valor = valor, estado = "calculado", motivo = NA_character_))
  }
  if (identical(familia, "distintos")) {
    if (!is.data.frame(resultado)) {
      return(list(valor = NA_real_, estado = "no_disponible",
                  motivo = sobre$motivo %||% "mapa_distintos_no_disponible"))
    }
    if (identical(metrica, "n_distintos")) {
      return(list(valor = nrow(resultado), estado = "calculado", motivo = NA_character_))
    }
    return(list(valor = NA_real_, estado = "calculado", motivo = NA_character_))
  }
  list(valor = NA, estado = "no_disponible", motivo = "familia_sin_acumulador")
}

.TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI <- 1000000

.motivo_mediana_mapa_bloques_dbi <- function(mapa) {
  if (!is.data.frame(mapa)) {
    return("familia_sin_acumulador:mapa_distintos_no_iniciado")
  }
  if (!nrow(mapa)) {
    return("familia_sin_acumulador:mapa_distintos_vacio")
  }
  frecuencias <- suppressWarnings(as.numeric(mapa$frecuencia))
  total <- sum(frecuencias)
  if (is.finite(total) && total > .TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI) {
    return(paste0(
      "familia_sin_acumulador:mediana_bloques_supera_tope_reconstruccion:",
      formatC(.TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI,
              format = "f", digits = 0)
    ))
  }
  if (!is.numeric(mapa$representante) || !is.finite(total) || total < 1 ||
      anyNA(frecuencias)) {
    return("familia_sin_acumulador:mapa_distintos_no_reconstruible")
  }
  "familia_sin_acumulador:mediana_requiere_mapa_no_truncado"
}

.mediana_mapa_bloques_dbi <- function(mapa) {
  if (!is.data.frame(mapa) || !nrow(mapa)) return(NA_real_)
  valores <- mapa$representante
  frecuencias <- as.numeric(mapa$frecuencia)
  if (!is.numeric(valores) || anyNA(frecuencias)) return(NA_real_)
  orden <- order(valores)
  valores <- valores[orden]
  frecuencias <- frecuencias[orden]
  total <- sum(frecuencias)
  if (!is.finite(total) || total < 1 ||
      total > .TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI) return(NA_real_)
  expandido <- rep(valores, frecuencias)
  stats::median(expandido, na.rm = TRUE)
}

.fila_y_registros_bloques_dbi <- function(campo, n_total, metricas,
                                          acumuladores, prototipo, tipos,
                                          incluir_valores, fuente,
                                          metricas_publicas = metricas,
                                          decisiones_costo = NULL) {
  fila <- .fila_resumen_dbi(campo, n_total)
  fila$n_nan <- NA_real_
  fila$n_infinito_positivo <- NA_real_
  fila$n_infinito_negativo <- NA_real_
  fila$longitud_minima <- NA_real_
  fila$longitud_maxima <- NA_real_
  fila$longitud_media <- NA_real_
  registros <- list()
  registros[[1L]] <- .registro_sql_dbi(
    campo, "n", "calculado", NA_character_, fuente$consulta,
    metadatos = .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = "tabla_completa",
      metodo = "dbfetch_bloques", error_esperado = "no_aplica",
      id_consulta = 1L, columnas_compartidas = length(fuente$campos)
    ),
    medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
  )
  agregar <- function(nombre, grupo, familias) {
    sobre <- acumuladores[[paste(campo, grupo, sep = "\u001f")]]
    if (inherits(sobre, "acumulador_bloques")) sobre <- sobre$resultado
    metadatos <- .metadatos_sql_dbi(
      alcance = "tabla_completa", universo = "tabla_completa",
      metodo = "dbfetch_bloques", error_esperado = "no_aplica",
      id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
    )
    metrica <- .resumen_metrica_bloques_dbi(
      sobre, grupo, nombre, incluir_valores
    )
    if (identical(nombre, "prop_faltantes")) {
      metrica$valor <- if (is.finite(n_total) && n_total > 0) {
        fila$n_faltantes / n_total
      } else NA_real_
    }
    if (nombre %in% names(fila)) fila[[nombre]] <<- metrica$valor
    registros[[length(registros) + 1L]] <<- .registro_sql_dbi(
      campo, nombre, metrica$estado, metrica$motivo,
      fuente$consulta, metadatos = metadatos,
      medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
    )
  }
  if ("validos" %in% metricas) {
    agregar("n_validos", "conteos", "validos")
    conteos <- acumuladores[[paste(campo, "conteos", sep = "\u001f")]]
    sobre <- if (is.null(conteos)) NULL else if (
      inherits(conteos, "acumulador_bloques")
    ) conteos$resultado else conteos
    if (!is.null(sobre) && identical(sobre$estado, "calculado")) {
      fila$n_faltantes <- n_total - fila$n_validos
      fila$prop_faltantes <- if (is.finite(n_total) && n_total > 0) {
        fila$n_faltantes / n_total
      } else NA_real_
      fila$n_nan <- sobre$resultado$n_nan %||% NA_real_
      fila$n_infinito_positivo <- sobre$resultado$n_infinito_positivo %||% NA_real_
      fila$n_infinito_negativo <- sobre$resultado$n_infinito_negativo %||% NA_real_
    }
    for (nombre in c("n_faltantes", "prop_faltantes")) {
      metrica <- if (nombre == "n_faltantes") {
        list(valor = fila$n_faltantes, estado = "calculado", motivo = NA_character_)
      } else list(valor = fila$prop_faltantes, estado = "calculado", motivo = NA_character_)
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, nombre, metrica$estado, metrica$motivo, fuente$consulta,
        metadatos = .metadatos_sql_dbi(
          alcance = "tabla_completa", universo = "tabla_completa",
          metodo = "dbfetch_bloques", error_esperado = "no_aplica",
          id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
        ), medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
      )
    }
  } else {
    for (nombre in c("n_validos", "n_faltantes", "prop_faltantes")) {
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, nombre, "no_solicitado", "La metrica no fue solicitada.",
        NA_character_, metadatos = .metadatos_sql_dbi()
      )
    }
  }
  if ("distintos" %in% metricas_publicas) {
    sobre <- acumuladores[[paste(campo, "distintos", sep = "\u001f")]]
    if (inherits(sobre, "acumulador_bloques")) sobre <- sobre$resultado
    metrica <- .resumen_metrica_bloques_dbi(sobre, "distintos", "n_distintos")
    fila$n_distintos <- metrica$valor
    fila$tasa_distintos <- if (isTRUE(is.finite(fila$n_validos)) && fila$n_validos > 0) {
      fila$n_distintos / fila$n_validos
    } else NA_real_
    for (nombre in c("n_distintos", "tasa_distintos")) {
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, nombre, metrica$estado, metrica$motivo, fuente$consulta,
        metadatos = .metadatos_sql_dbi(
          alcance = "tabla_completa", universo = "tabla_completa",
          metodo = "dbfetch_bloques", error_esperado = "no_aplica",
          id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
        ), medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
      )
    }
  } else {
    for (nombre in c("n_distintos", "tasa_distintos")) {
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, nombre, "no_solicitado", "La metrica no fue solicitada.",
        NA_character_, metadatos = .metadatos_sql_dbi()
      )
    }
  }
  cuantitativos <- acumuladores[[paste(campo, "cuantitativos", sep = "\u001f")]]
  sobre_cuantitativos <- if (inherits(cuantitativos, "acumulador_bloques")) {
    cuantitativos$resultado
  } else cuantitativos
  if (!is.null(sobre_cuantitativos) &&
      identical(sobre_cuantitativos$estado, "calculado")) {
    fila$n_nan <- sobre_cuantitativos$resultado$n_nan %||% fila$n_nan
    fila$n_infinito_positivo <- sobre_cuantitativos$resultado$n_infinito_positivo %||%
      fila$n_infinito_positivo
    fila$n_infinito_negativo <- sobre_cuantitativos$resultado$n_infinito_negativo %||%
      fila$n_infinito_negativo
  }
  for (grupo in c("basicos", "desvio")) {
    pedidos <- if (identical(grupo, "basicos")) {
      c("minimo", "maximo", "media", "n_ceros", "n_negativos")
    } else "desvio"
    if (!is.null(cuantitativos) && grupo %in% metricas) {
      sobre <- if (inherits(cuantitativos, "acumulador_bloques")) {
        cuantitativos$resultado
      } else cuantitativos
      for (nombre in pedidos) {
        metrica <- .resumen_metrica_bloques_dbi(
          sobre, "cuantitativos", nombre, incluir_valores
        )
        if (!isTRUE(incluir_valores) && nombre %in% c("minimo", "maximo")) {
          metrica$estado <- "omitido_por_privacidad"
          metrica$motivo <- "Los valores se omitieron por privacidad."
        }
        if (nombre %in% names(fila)) fila[[nombre]] <- metrica$valor
        registros[[length(registros) + 1L]] <- .registro_sql_dbi(
          campo, nombre, metrica$estado, metrica$motivo, fuente$consulta,
          metadatos = .metadatos_sql_dbi(
            alcance = "tabla_completa", universo = "tabla_completa",
            metodo = "dbfetch_bloques", error_esperado = "no_aplica",
            id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
          ), medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
        )
      }
    } else {
      for (nombre in pedidos) {
        registros[[length(registros) + 1L]] <- .registro_sql_dbi(
          campo, nombre, if (grupo %in% metricas) "no_aplica" else "no_solicitado",
          if (grupo %in% metricas) "La columna no es numerica." else
            "La metrica no fue solicitada.", NA_character_,
          metadatos = .metadatos_sql_dbi()
        )
      }
    }
  }
  longitudes <- acumuladores[[paste(campo, "longitudes", sep = "\u001f")]]
  if (inherits(longitudes, "acumulador_bloques")) longitudes <- longitudes$resultado
  if (!is.null(longitudes) && identical(longitudes$estado, "calculado")) {
    fila$longitud_minima <- longitudes$resultado["minimo"]
    fila$longitud_maxima <- longitudes$resultado["maximo"]
    fila$longitud_media <- longitudes$resultado["media"]
    for (nombre in c("longitud_minima", "longitud_maxima", "longitud_media")) {
      registros[[length(registros) + 1L]] <- .registro_sql_dbi(
        campo, nombre, "calculado", NA_character_, fuente$consulta,
        metadatos = .metadatos_sql_dbi(
          alcance = "tabla_completa", universo = "tabla_completa",
          metodo = "dbfetch_bloques", error_esperado = "no_aplica",
          id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
        ), medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
      )
    }
  }
  # Moda y mediana quedan como familias de valores no fusionables en I1. Si el
  # mapa exacto ya existe se publica la moda; la mediana se calcula solo sobre
  # un mapa no truncado y se declara como acotada por esa representacion.
  distintos <- acumuladores[[paste(campo, "distintos", sep = "\u001f")]]
  if (inherits(distintos, "acumulador_bloques")) distintos <- distintos$resultado
  mapa <- if (is.null(distintos)) NULL else distintos$resultado
  mapa_truncado <- !is.null(distintos) && identical(distintos$estado, "cota")
  motivo_mapa <- if (mapa_truncado) {
    paste0("familia_sin_acumulador:", distintos$motivo %||%
      "mapa_distintos_truncado")
  } else if (is.null(mapa)) {
    "familia_sin_acumulador:mapa_distintos_no_iniciado"
  } else if (!nrow(mapa)) {
    "familia_sin_acumulador:mapa_distintos_vacio"
  } else NA_character_
  if ("moda" %in% metricas && isTRUE(incluir_valores) && is.data.frame(mapa) && nrow(mapa)) {
    decision_moda <- decisiones_costo[[campo]]
    if (!is.null(decision_moda) && identical(decision_moda$moda, FALSE)) {
      estado_valores <- "omitido_por_costo"
      motivo_valores <- .motivo_decision_costo_dbi(decision_moda, "moda")
    } else {
      indice <- which.max(mapa$frecuencia)
      fila$moda <- tryCatch(as.character(mapa$representante[[indice]]), error = function(e) NA_character_)
      fila$frecuencia_moda <- mapa$frecuencia[[indice]]
      estado_valores <- "calculado"
      motivo_valores <- NA_character_
    }
  } else if ("moda" %in% metricas && !isTRUE(incluir_valores)) {
    estado_valores <- "omitido_por_privacidad"
    motivo_valores <- "Los valores se omitieron por privacidad."
  } else if ("moda" %in% metricas) {
    estado_valores <- "no_disponible"
    motivo_valores <- motivo_mapa
  } else {
    estado_valores <- "no_solicitado"
    motivo_valores <- "La metrica no fue solicitada."
  }
  if ("moda" %in% metricas) {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "moda", estado_valores, motivo_valores, fuente$consulta,
      metadatos = .metadatos_sql_dbi(alcance = "tabla_completa",
        universo = "tabla_completa", metodo = "dbfetch_bloques"),
      medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
    )
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "frecuencia_moda", estado_valores, motivo_valores, fuente$consulta,
      metadatos = .metadatos_sql_dbi(alcance = "tabla_completa",
        universo = "tabla_completa", metodo = "dbfetch_bloques"),
      medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
    )
  } else {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, c("moda", "frecuencia_moda"), "no_solicitado",
      "La metrica no fue solicitada.", NA_character_,
      metadatos = .metadatos_sql_dbi()
    )
  }
  if ("mediana" %in% metricas && isTRUE(incluir_valores)) {
    fila$mediana <- .mediana_mapa_bloques_dbi(mapa)
    estado_mediana <- if (is.na(fila$mediana)) "no_disponible" else "calculado"
    motivo_mediana <- if (is.na(fila$mediana)) {
      if (mapa_truncado) motivo_mapa else .motivo_mediana_mapa_bloques_dbi(mapa)
    } else NA_character_
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", estado_mediana, motivo_mediana, fuente$consulta,
      metadatos = .metadatos_sql_dbi(alcance = "tabla_completa",
        universo = "tabla_completa", metodo = "dbfetch_bloques"),
      medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
    )
  } else if ("mediana" %in% metricas) {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", "omitido_por_privacidad",
      "La mediana se omitio por privacidad.", NA_character_,
      metadatos = .metadatos_sql_dbi()
    )
  } else {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", "no_solicitado", "La metrica no fue solicitada.",
      NA_character_, metadatos = .metadatos_sql_dbi()
    )
  }
  list(fila = fila, sql = do.call(rbind, registros))
}

.plan_bloques_fuente_dbi <- function(fuente, bloque_filas, campos, metricas,
                                     bloque_muestra = "con_muestra",
                                     muestra = NA_real_, orden_muestra = NULL) {
  plan <- data.frame(
    clase_consulta = "fuente por bloques (dbSendQuery + dbFetch)",
    n_consultas = if (isTRUE(fuente$disponible)) 1L else 0L,
    alcance = "tabla_completa",
    n_consultas_max = if (isTRUE(fuente$disponible)) 1L else 0L,
    stringsAsFactors = FALSE
  )
  attr(plan, "total") <- plan$n_consultas
  attr(plan, "total_minimo") <- plan$n_consultas
  attr(plan, "total_maximo") <- plan$n_consultas_max
  attr(plan, "pagado") <- FALSE
  attr(plan, "bloque_filas") <- bloque_filas
  attr(plan, "fuente") <- fuente
  attr(plan, "alcance") <- list(
    fuente_id = fuente$fuente_id, orden = fuente$orden_id,
    snapshot_id = fuente$snapshot_id
  )
  attr(plan, "bloques") <- list(
    filas_pedidas = NA_real_, objetivo = bloque_filas, minimo = 1L,
    maximo = bloque_filas, bytes_esperados = NA_real_,
    solicitados = NA_integer_, recorridos = 0L, filas_vistas = 0,
    fetches = 0L, consultas_sql = if (isTRUE(fuente$disponible)) 1L else 0L,
    sin_solapamiento = TRUE, primer_ordinal = NA_real_, ultimo_ordinal = NA_real_
  )
  attr(plan, "pasadas") <- list(
    primera = "dbFetch incremental", valor = "acumuladores I1",
    indice = "solo con orden estable y snapshot demostrado",
    lsh = "fuera_de_alcance_I1", materializacion = "fuera_de_alcance_I1"
  )
  attr(plan, "muestra_diagnostica") <- list(
    backend = "perfil_muestra existente", filas = NA_real_,
    estado = "acotada_declarada"
  )
  attr(plan, "muestra") <- if (identical(bloque_muestra, "con_muestra")) {
    muestra
  } else NA_real_
  attr(plan, "bloque_muestra") <- bloque_muestra
  attr(plan, "orden_muestra") <- if (is.null(orden_muestra)) {
    character()
  } else as.character(orden_muestra)
  attr(plan, "costo") <- list(
    consultas_sql = if (isTRUE(fuente$disponible)) 1L else 0L,
    resultsets = if (isTRUE(fuente$disponible)) 1L else 0L,
    fetches = NA_integer_, filas = NA_real_, bytes = NA_real_,
    metricas = metricas, columnas = length(campos)
  )
  class(plan) <- c("plan_perfilado_dbi", class(plan))
  plan
}

.recorrer_fuente_bloques_dbi <- function(conexion, fuente, metricas,
                                         prototipo, tipos, bloque_filas,
                                         vigilante = NULL,
                                         incluir_valores = TRUE,
                                         max_bytes_procesamiento =
                                           .MAX_BYTES_ESTADO_BLOQUES) {
  metricas_acumuladores <- unique(c(
    metricas,
    if (isTRUE(incluir_valores) &&
        any(c("moda", "mediana") %in% metricas)) "distintos" else character()
  ))
  acumuladores <- .familias_fuente_bloques_dbi(
    metricas_acumuladores, fuente$campos, prototipo, tipos,
    incluir_valores = incluir_valores,
    max_bytes_procesamiento = max_bytes_procesamiento
  )
  for (nombre in names(acumuladores)) {
    acumuladores[[nombre]]$configuracion$fuente_id <- fuente$fuente_id
    acumuladores[[nombre]]$configuracion$snapshot_id <- fuente$snapshot_id
    acumuladores[[nombre]]$configuracion$orden_id <- fuente$orden_id
  }
  bloques <- list(
    solicitados = 0L, recorridos = 0L, filas_vistas = 0,
    fetches = 0L, consultas_sql = 0L, primer_ordinal = NA_real_,
    ultimo_ordinal = NA_real_,
    sin_solapamiento = TRUE
  )
  bytes_max <- 0
  bytes_entrada <- 0
  locators <- list()
  if (!isTRUE(fuente$disponible)) {
    return(list(acumuladores = acumuladores, sobres = list(),
                bloques = bloques, bytes = list(max_bloque = 0,
                  entrada = 0, texto = 0, retenidos = 0,
                  rss_maximo = NA_real_),
                locators = locators, error = fuente$motivo))
  }
  resultado <- tryCatch(DBI::dbSendQuery(conexion, fuente$consulta), error = function(e) e)
  if (inherits(resultado, "condition")) {
    return(list(acumuladores = acumuladores, sobres = list(),
                bloques = bloques, bytes = list(max_bloque = 0,
                  entrada = 0, texto = 0, retenidos = 0,
                  rss_maximo = NA_real_),
                locators = locators,
                error = paste0("no_disponible:dbSendQuery:", conditionMessage(resultado))))
  }
  bloques$consultas_sql <- 1L
  on.exit(tryCatch(DBI::dbClearResult(resultado), error = function(e) NULL), add = TRUE)
  ordinal <- 0
  repeat {
    bloque <- tryCatch(DBI::dbFetch(resultado, n = as.integer(bloque_filas)),
                       error = function(e) e)
    bloques$fetches <- bloques$fetches + 1L
    if (inherits(bloque, "condition")) {
      return(list(acumuladores = acumuladores, sobres = list(),
                  bloques = bloques,
                  bytes = list(max_bloque = bytes_max, entrada = bytes_entrada,
                                texto = 0,
                                retenidos = sum(vapply(acumuladores, .bytes_retenidos, numeric(1L)),
                                                na.rm = TRUE), rss_maximo = NA_real_),
                  locators = locators,
                  error = paste0("no_disponible:dbFetch:", conditionMessage(bloque))))
    }
    if (!inherits(bloque, "data.frame")) bloque <- as.data.frame(bloque)
    n <- nrow(bloque)
    if (!n) break
    inicio <- ordinal + 1
    fin <- ordinal + n
    ordinal <- fin
    bloques$recorridos <- bloques$recorridos + 1L
    bloques$filas_vistas <- bloques$filas_vistas + n
    if (is.na(bloques$primer_ordinal)) bloques$primer_ordinal <- inicio
    bloques$ultimo_ordinal <- fin
    bytes_bloque <- as.numeric(utils::object.size(bloque))
    bytes_max <- max(bytes_max, bytes_bloque)
    bytes_entrada <- bytes_entrada + bytes_bloque
    if (identical(fuente$metodo_orden, "row_locator")) {
      locators[[length(locators) + 1L]] <- .valor_columna_bloque_dbi(
        bloque, .ALIAS_LOCALIZADOR_DBI
      )
    }
    acumuladores <- .absorber_bloque_fuente_dbi(
      acumuladores, bloque, inicio, fin
    )
    if (!is.null(vigilante)) {
      # La medicion ocurre entre bloques, despues de soltar la referencia de
      # entrada; cada familia conserva su propio tamano retenido.
      rm(bloque)
      gc(verbose = FALSE)
      for (nombre in names(acumuladores)) {
        .registrar_barrera_vigilante(
          vigilante, "bloque", strsplit(nombre, "\u001f", fixed = TRUE)[[1L]][[2L]],
          bloques$recorridos, acumuladores[[nombre]]
        )
      }
    } else {
      rm(bloque)
      gc(verbose = FALSE)
    }
    completado <- tryCatch(DBI::dbHasCompleted(resultado), error = function(e) FALSE)
    if (isTRUE(completado) || n < bloque_filas) break
  }
  sobres <- .finalizar_bloques_fuente_dbi(acumuladores)
  retenidos <- if (length(acumuladores)) {
    sum(vapply(acumuladores, .bytes_retenidos, numeric(1L)), na.rm = TRUE)
  } else 0
  if (!is.null(vigilante) && length(acumuladores)) {
    gc(verbose = FALSE)
    for (nombre in names(acumuladores)) {
      .registrar_barrera_vigilante(
        vigilante, "final", strsplit(nombre, "\u001f", fixed = TRUE)[[1L]][[2L]],
        max(1L, bloques$recorridos), acumuladores[[nombre]],
        sobres[[nombre]]
      )
    }
  }
  list(
    acumuladores = acumuladores, sobres = sobres, bloques = bloques,
    bytes = list(max_bloque = bytes_max, entrada = bytes_entrada,
                 texto = 0,
                 retenidos = retenidos,
                 rss_maximo = if (!is.null(vigilante) && nrow(vigilante$eventos)) {
                   lecturas <- vigilante$eventos$lectura_proceso
                   if (any(is.finite(lecturas))) {
                     max(lecturas[is.finite(lecturas)])
                   } else NA_real_
                 } else NA_real_),
    locators = locators, error = NULL
  )
}

.cobertura_bloques_dbi <- function(metricas, campos, bloque_muestra) {
  cobertura <- .cobertura_dbi_vacia()
  familias_no_fusionables <- c(
    "patrones", "formatos_fecha", "dependencias", "hallazgos", "datos_personales"
  )
  estado <- if (identical(bloque_muestra, "solo_agregados")) {
    "no_solicitado"
  } else "no_disponible"
  motivo <- if (identical(estado, "no_solicitado")) {
    "La muestra diagnostica no fue solicitada en I1."
  } else {
    "muestra_diagnostica_acotada:se_resuelve_en_perfil_muestra"
  }
  for (familia in familias_no_fusionables) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", familia, estado, motivo,
      "Solicitar el bloque de muestra para ejecutar diagnosticos acotados.",
      NA_character_
    ))
  }
  cobertura
}

.verificar_identidad_row_locator_dbi <- function(conexion, fuente, locators,
                                                 bloque_filas = fuente$bloque_filas,
                                                 presupuesto = NULL,
                                                 antes_segunda_pasada = NULL) {
  if (!identical(fuente$metodo_orden, "row_locator")) {
    return(list(estado = "no_aplica", identidad = NA,
                motivo = "metodo_orden_no_es_row_locator",
                consultas_sql = 0L, fetches = 0L))
  }
  if (is.function(antes_segunda_pasada)) antes_segunda_pasada()
  esperados <- unlist(locators, use.names = FALSE)
  rs <- tryCatch(DBI::dbSendQuery(conexion, fuente$consulta), error = function(e) e)
  if (inherits(rs, "condition")) {
    return(list(estado = "no_disponible", identidad = NA,
                motivo = paste0("no_disponible:dbSendQuery:", conditionMessage(rs)),
                consultas_sql = 1L, fetches = 0L))
  }
  consultas_sql <- 1L
  fetches <- 0L
  on.exit(tryCatch(DBI::dbClearResult(rs), error = function(e) NULL), add = TRUE)
  observados <- list()
  repeat {
    bloque <- tryCatch(DBI::dbFetch(rs, n = as.integer(bloque_filas)),
                       error = function(e) e)
    fetches <- fetches + 1L
    if (inherits(bloque, "condition")) {
      return(list(estado = "no_disponible", identidad = NA,
                  motivo = paste0("no_disponible:dbFetch:", conditionMessage(bloque)),
                  consultas_sql = consultas_sql, fetches = fetches))
    }
    if (!nrow(bloque)) break
    observados[[length(observados) + 1L]] <- .valor_columna_bloque_dbi(
      bloque, .ALIAS_LOCALIZADOR_DBI
    )
    if (isTRUE(tryCatch(DBI::dbHasCompleted(rs), error = function(e) FALSE)) ||
        nrow(bloque) < bloque_filas) break
  }
  vistos <- unlist(observados, use.names = FALSE)
  iguales <- length(vistos) == length(esperados) && identical(vistos, esperados)
  list(
    estado = if (iguales) "identidad_verificada" else "resultset_no_reproducible",
    identidad = iguales,
    motivo = if (iguales) NA_character_ else
      "row_locator_ordinal_fila_cambio:segunda_pasada_no_identica",
    filas_primera_pasada = length(esperados), filas_segunda_pasada = length(vistos),
    consultas_sql = consultas_sql, fetches = fetches
  )
}

.perfilar_dbi_bloques <- function(conexion, tabla, preparacion, metricas,
                                  incluir_valores, bloque_filas,
                                  max_celdas_muestra, max_bytes_muestra,
                                  argumentos = list(),
                                  max_bytes_procesamiento =
                                    .MAX_BYTES_ESTADO_BLOQUES,
                                  metricas_publicas = preparacion$metricas) {
  bloque_filas <- .validar_bloque_filas_dbi(bloque_filas)
  metricas_publicas <- unique(as.character(metricas_publicas %||% metricas))
  if (!identical(preparacion$universo, "tabla_completa")) {
    motivo <- "no_disponible:fuente_bloques_solo_tabla_completa"
    fuente <- structure(list(
      disponible = FALSE, capacidad = .capacidad_fuente_bloques_dbi(conexion),
      motivo = motivo, consulta = NA_character_, orden_sql = NA_character_,
      orden_id = NA_character_, metodo_orden = "resultset_no_reproducible",
      estable = FALSE, snapshot_id = NA_character_,
      fuente_id = paste0("dbi:", .texto_tabla_dbi(tabla)), tabla = tabla,
      tabla_sql = preparacion$tabla_sql, campos = preparacion$campos,
      campos_sql = preparacion$campos_sql, localizador = NULL,
      clave = preparacion$catalogo_cardinalidad, bloque_filas = bloque_filas
    ), class = "fuente_bloques_dbi")
    recorrido <- list(
      bloques = list(solicitados = 0L, recorridos = 0L, filas_vistas = 0,
        fetches = 0L, consultas_sql = 0L, primer_ordinal = NA_real_,
        ultimo_ordinal = NA_real_,
        sin_solapamiento = TRUE), bytes = list(max_bloque = 0, entrada = 0,
        texto = 0, retenidos = 0, rss_maximo = NA_real_), error = motivo,
      acumuladores = list()
    )
    plan <- .plan_bloques_fuente_dbi(fuente, bloque_filas,
      preparacion$campos, metricas, preparacion$bloque_muestra,
      preparacion$muestra, preparacion$orden_muestra)
    filas <- lapply(preparacion$campos, function(campo) {
      .fila_resumen_dbi(campo, NA_real_)
    })
    columnas <- if (length(filas)) do.call(rbind, lapply(filas, as.data.frame)) else
      .columnas_dbi_vacias()
    sql <- do.call(rbind, lapply(preparacion$campos, function(campo) {
      .registro_sql_dbi(campo, unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE),
        "no_disponible", motivo, NA_character_, .metadatos_sql_dbi())
    }))
    resumen <- list(
      columnas = columnas, sql = sql,
      cobertura = .registro_cobertura_dbi(
        "fuente_bloques", .texto_tabla_dbi(tabla), "no_disponible", motivo,
        "Usar `bloque_filas` solo con `universo = \"tabla_completa\"` en I1.",
        NA_character_
      ),
      meta = list(
        universo = preparacion$universo, alcance = list(
          universo_id = preparacion$universo, fuente_id = fuente$fuente_id,
          orden = NA_character_, snapshot_id = NA_character_, muestra_id = NULL
        ), fuente_bloques = fuente, bloques = recorrido$bloques,
        bytes = recorrido$bytes, familias = list(), plan = plan,
        eventos = data.frame(),
        vigilante = data.frame(), filas = NA_real_, tabla = .texto_tabla_dbi(tabla)
      ), tiempos = .resumen_tiempos_dbi(NULL)
    )
    salida <- list(resumen_tabla = resumen, perfil_muestra = NULL)
    class(salida) <- "perfil_dbi"
    return(salida)
  }
  fuente <- .fuente_bloques_dbi(
    conexion, tabla, tabla_sql = preparacion$tabla_sql,
    campos = preparacion$campos, campos_sql = preparacion$campos_sql,
    prototipo = preparacion$prototipo, tipos = preparacion$tipos,
    orden_muestra = preparacion$orden_muestra, orden_sql = preparacion$orden_sql,
    clave = preparacion$catalogo_cardinalidad,
    dialecto = preparacion$dialecto,
    presupuesto = preparacion$presupuesto, bloque_filas = bloque_filas
  )
  plan <- .plan_bloques_fuente_dbi(
    fuente, bloque_filas, preparacion$campos, metricas,
    preparacion$bloque_muestra, preparacion$muestra, preparacion$orden_muestra
  )
  vigilante <- .iniciar_vigilante(
    corrida_id = paste0("dbi-bloques-", as.integer(Sys.time())),
    tope_bytes = max_bytes_procesamiento
  )
  recorrido <- .recorrer_fuente_bloques_dbi(
    conexion, fuente, metricas, preparacion$prototipo, preparacion$tipos,
    bloque_filas, vigilante = vigilante,
    incluir_valores = incluir_valores,
    max_bytes_procesamiento = max_bytes_procesamiento
  )
  n_total <- recorrido$bloques$filas_vistas
  recorrido$bloques$solicitados <- if (n_total > 0) {
    as.integer(ceiling(n_total / bloque_filas))
  } else 0L
  segunda_pasada <- if (identical(fuente$metodo_orden, "row_locator") &&
                        is.null(recorrido$error)) {
    .verificar_identidad_row_locator_dbi(
      conexion, fuente, recorrido$locators, bloque_filas = bloque_filas,
      presupuesto = preparacion$presupuesto,
      antes_segunda_pasada = argumentos$antes_segunda_pasada
    )
  } else list(
    estado = "no_aplica", identidad = NA,
    motivo = if (is.null(recorrido$error)) "pk_o_resultset_sin_locator" else
      "primera_pasada_no_disponible", consultas_sql = 0L, fetches = 0L
  )
  agregados_costo <- list(conteos = stats::setNames(lapply(
    preparacion$campos, function(campo) {
      sobre <- recorrido$sobres[[paste(campo, "conteos", sep = "\u001f")]]
      mapa <- recorrido$sobres[[paste(campo, "distintos", sep = "\u001f")]]
      validos <- if (!is.null(sobre) && identical(sobre$estado, "calculado")) {
        list(ok = TRUE, valor = sobre$resultado$n_validos)
      } else NULL
      distintos <- if (!is.null(mapa) && identical(mapa$estado, "calculado")) {
        list(ok = TRUE, valor = nrow(mapa$resultado))
      } else NULL
      list(validos = validos, distintos = distintos)
    }
  ), preparacion$campos))
  decisiones_costo <- .decisiones_costo_dbi(
    conexion, preparacion$campos, agregados_costo,
    preparacion$politica_costo, n_total, "tabla_completa",
    fuentes_cardinalidad_costo = preparacion$fuentes_cardinalidad_costo
  )
  presupuesto <- preparacion$presupuesto
  if (!is.null(presupuesto)) {
    presupuesto$estimacion_derrame_moda <-
      .actualizar_n_validos_estimacion_dbi(
        presupuesto$estimacion_derrame_moda, agregados_costo,
        metricas, "meta"
      )
    presupuesto$estimacion_derrame_mediana <-
      .actualizar_n_validos_estimacion_dbi(
        presupuesto$estimacion_derrame_mediana, agregados_costo,
        metricas, "meta"
      )
    columnas_modas <- preparacion$campos[
      vapply(decisiones_costo, function(x) isTRUE(x$moda), logical(1L))
    ]
    if (!("moda" %in% metricas) || !isTRUE(incluir_valores)) {
      columnas_modas <- character()
    }
    if (length(columnas_modas) && !isTRUE(presupuesto$aviso_derrame_moda_emitido)) {
      avisado <- .avisar_derrame_estimado_postgresql_dbi(
        .filtrar_estimacion_derrame_dbi(
          presupuesto$estimacion_derrame_moda, columnas_modas
        ),
        habilitado = presupuesto$avisar_derrame_estimado,
        umbral_bytes = presupuesto$umbral_bytes_aviso_derrame_estimado
      )
      if (isTRUE(avisado)) presupuesto$aviso_derrame_moda_emitido <- TRUE
    }
    columnas_medianas <- preparacion$campos[
      vapply(decisiones_costo, function(x) isTRUE(x$mediana), logical(1L))
    ]
    columnas_medianas <- intersect(
      columnas_medianas, preparacion$campos[preparacion$es_numerico]
    )
    if (!("mediana" %in% metricas) || !isTRUE(incluir_valores)) {
      columnas_medianas <- character()
    }
    if (length(columnas_medianas) &&
        !isTRUE(presupuesto$aviso_derrame_mediana_emitido)) {
      avisado <- .avisar_derrame_estimado_postgresql_dbi(
        .filtrar_estimacion_derrame_dbi(
          presupuesto$estimacion_derrame_mediana, columnas_medianas
        ),
        habilitado = presupuesto$avisar_derrame_estimado,
        umbral_bytes = presupuesto$umbral_bytes_aviso_derrame_estimado
      )
      if (isTRUE(avisado)) presupuesto$aviso_derrame_mediana_emitido <- TRUE
    }
  }
  consultas_segunda <- as.integer(segunda_pasada$consultas_sql %||% 0L)
  fetches_segunda <- as.integer(segunda_pasada$fetches %||% 0L)
  recorrido$bloques$consultas_sql <- as.integer(
    recorrido$bloques$consultas_sql %||% 0L
  ) + consultas_segunda
  recorrido$bloques$fetches <- as.integer(recorrido$bloques$fetches) + fetches_segunda
  plan$n_consultas <- recorrido$bloques$consultas_sql
  plan$n_consultas_max <- recorrido$bloques$consultas_sql
  attr(plan, "total") <- recorrido$bloques$consultas_sql
  attr(plan, "total_minimo") <- recorrido$bloques$consultas_sql
  attr(plan, "total_maximo") <- recorrido$bloques$consultas_sql
  costo_plan <- attr(plan, "costo", exact = TRUE)
  costo_plan$consultas_sql <- recorrido$bloques$consultas_sql
  costo_plan$resultsets <- recorrido$bloques$consultas_sql
  costo_plan$fetches <- recorrido$bloques$fetches
  attr(plan, "costo") <- costo_plan
  fuente$plan$costo$consultas_sql <- recorrido$bloques$consultas_sql
  fuente$plan$costo$resultsets <- recorrido$bloques$consultas_sql
  fuente$plan$costo$fetches <- recorrido$bloques$fetches
  filas <- vector("list", length(preparacion$campos))
  names(filas) <- preparacion$campos
  sql <- list()
  for (campo in preparacion$campos) {
    resultado <- if (isTRUE(fuente$disponible) && is.null(recorrido$error)) {
      .fila_y_registros_bloques_dbi(
        campo, n_total, metricas, recorrido$acumuladores,
        preparacion$prototipo, preparacion$tipos, incluir_valores, fuente,
        metricas_publicas = metricas_publicas,
        decisiones_costo = decisiones_costo
      )
    } else {
      fila <- .fila_resumen_dbi(campo, NA_real_)
      no <- .estado_no_disponible_bloques_dbi(
        recorrido$error %||% fuente$motivo
      )
      metrica <- unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE)
      list(
        fila = fila,
        sql = .registro_sql_dbi(
          campo, metrica, no$estado, no$motivo, fuente$consulta,
          metadatos = .metadatos_sql_dbi(),
          medicion = list(consulta_id = NA_integer_, etapa = "dbfetch_bloques")
        )
      )
    }
    filas[[campo]] <- resultado$fila
    sql[[length(sql) + 1L]] <- resultado$sql
  }
  columnas <- if (length(filas)) {
    do.call(rbind, lapply(filas, function(fila) {
      as.data.frame(fila, stringsAsFactors = FALSE)
    }))
  } else .columnas_dbi_vacias()
  for (nombre in intersect(
    c("n", "n_validos", "n_faltantes", "n_distintos", "frecuencia_moda",
      "n_ceros", "n_negativos"), names(columnas)
  )) {
    valor <- vapply(
      columnas[[nombre]], .entero_publico_bloques_dbi, numeric(1L)
    )
    if (all(!is.na(valor) & valor <= .Machine$integer.max)) {
      valor <- as.integer(valor)
    }
    columnas[[nombre]] <- valor
  }
  rownames(columnas) <- NULL
  sql <- if (length(sql)) do.call(rbind, sql) else data.frame()
  cobertura <- .cobertura_bloques_dbi(metricas, preparacion$campos,
                                      preparacion$bloque_muestra)
  if (!isTRUE(fuente$disponible) || !is.null(recorrido$error)) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "fuente_bloques", .texto_tabla_dbi(tabla), "no_disponible",
      recorrido$error %||% fuente$motivo,
      "Usar un driver con dbFetch incremental demostrado.", fuente$consulta
    ))
  }
  if (identical(segunda_pasada$estado, "resultset_no_reproducible")) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "fuente_bloques", .texto_tabla_dbi(tabla), "degradado",
      segunda_pasada$motivo,
      "Mantener un snapshot DBI o evitar cambios entre pasadas.",
      fuente$consulta
    ))
  }
  rownames(cobertura) <- NULL
  orden_muestra_solicitado <- as.character(
    preparacion$orden_muestra %||% character()
  )
  orden_muestra_meta <- if (length(orden_muestra_solicitado)) {
    list(
      solicitado = orden_muestra_solicitado, aplicado = FALSE,
      motivo = paste(
        "orden_muestra_no_gobierna_fuente_bloques: la via I1 prioriza la",
        "clave primaria o el localizador para preservar la identidad entre",
        "pasadas."
      )
    )
  } else {
    list(solicitado = character(), aplicado = NA,
         motivo = "orden_muestra_no_solicitado")
  }
  meta <- list(
    universo = "tabla_completa", filas = n_total,
    tabla = .texto_tabla_dbi(tabla), motor = .info_conexion_dbi(conexion),
    alcance = list(
      universo_id = "tabla_completa", fuente_id = fuente$fuente_id,
      orden = fuente$orden_id, snapshot_id = fuente$snapshot_id,
      muestra_id = NULL, metodo_orden = fuente$metodo_orden,
      estable = fuente$estable
    ),
    fuente_bloques = fuente,
    familias = .publicar_familias_bloques_dbi(recorrido$sobres),
    segunda_pasada = segunda_pasada,
    bloques = recorrido$bloques,
    bytes = recorrido$bytes,
    vigilante = .eventos_vigilante(vigilante),
    eventos = .eventos_vigilante(vigilante),
    plan = plan,
    consultas = list(
      emitidas = as.integer(recorrido$bloques$consultas_sql %||% 0L),
      fetches = as.integer(recorrido$bloques$fetches %||% 0L),
      presupuesto = preparacion$max_consultas,
      agotado = FALSE
    ),
    orden_muestra = orden_muestra_meta,
    decisiones_costo = decisiones_costo,
    estimacion_derrame_moda = if (is.null(presupuesto)) NULL else
      presupuesto$estimacion_derrame_moda,
    estimacion_derrame_mediana = if (is.null(presupuesto)) NULL else
      presupuesto$estimacion_derrame_mediana,
    metodo = "dbfetch_bloques", snapshot = fuente$snapshot_id,
    clave = preparacion$catalogo_cardinalidad,
    metricas = metricas_publicas, metricas_ejecucion = metricas,
    incluir_valores = incluir_valores,
    solo_lectura = TRUE, objetos_temporales = FALSE,
    nota_inf = paste(
      "Los acumuladores calculan estadisticos sobre valores finitos; las",
      "banderas de NA, NaN e infinitos se conservan por separado. La posible",
      "divergencia de un resumen SQL que no pueda leer +/-Inf se debe a la",
      "representacion del controlador, no a una conversion silenciosa."
    )
  )
  resumen <- list(columnas = columnas, sql = sql, cobertura = cobertura,
                  meta = meta, tiempos = .resumen_tiempos_dbi(NULL))
  if (identical(preparacion$bloque_muestra, "solo_agregados")) {
    perfil_muestra <- NULL
  } else if (!isTRUE(fuente$disponible) || !is.null(recorrido$error)) {
    perfil_muestra <- NULL
    resumen$cobertura <- rbind(
      resumen$cobertura,
      .registro_cobertura_dbi(
        "perfil_muestra", .texto_tabla_dbi(tabla), "no_disponible",
        "fuente_bloques_no_disponible:se_omite_lectura_adicional",
        "Resolver la capacidad incremental del driver antes de pedir diagnosticos.",
        NA_character_
      )
    )
  } else {
    trazador <- .trazador_tiempos_dbi(preparacion$instrumentar)
    argumentos_muestra <- argumentos
    argumentos_muestra$antes_segunda_pasada <- NULL
    bloque_muestra <- .bloque_muestra_dbi(
      conexion, tabla, preparacion$tabla_sql, preparacion$campos,
      preparacion$campos_sql, preparacion$muestra, preparacion$muestra,
      if (length(fuente$orden_expresiones)) fuente$orden_expresiones else
        preparacion$orden_muestra,
      fuente$orden_expresiones, preparacion$dialecto, n_total,
      preparacion$presupuesto, .info_conexion_dbi(conexion),
      argumentos_muestra, muestreo = NULL, tipos_declarados = preparacion$tipos,
      trazador = trazador, max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra
    )
    perfil_muestra <- bloque_muestra$perfil
    if (!is.null(perfil_muestra) && nrow(resumen$cobertura)) {
      conservar <- !(
        resumen$cobertura$bloque == "perfil_muestra" &
          grepl("muestra_diagnostica_acotada", resumen$cobertura$motivo,
                fixed = TRUE)
      )
      resumen$cobertura <- resumen$cobertura[conservar, , drop = FALSE]
      rownames(resumen$cobertura) <- NULL
    }
    resumen$cobertura <- rbind(resumen$cobertura, bloque_muestra$cobertura)
    resumen$tiempos <- .resumen_tiempos_dbi(trazador)
  }
  proteger <- is.null(argumentos$proteger_datos_personales) ||
    isTRUE(argumentos$proteger_datos_personales)
  if (proteger) {
    columnas_protegidas <- if (is.null(perfil_muestra)) {
      preparacion$campos
    } else {
      .columnas_personales_protegidas(perfil_muestra$datos_personales)
    }
    resumen <- .proteger_resumen_dbi(
      resumen, columnas_protegidas,
      if (is.null(perfil_muestra)) "sin_clasificacion_disponible" else
        "perfil_muestra"
    )
  } else {
    resumen$meta$proteccion_personal <- list(
      aplicada = FALSE, base = "desactivada por el usuario", columnas = character()
    )
  }
  salida <- list(resumen_tabla = resumen, perfil_muestra = perfil_muestra)
  class(salida) <- "perfil_dbi"
  salida
}
