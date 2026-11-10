# Fuente incremental DBI para la lectura por bloques.
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
  if (grepl(.regex_sin_caja("duckdb"), clase)) return("duckdb")
  if (grepl(.regex_sin_caja("sqlite"), clase)) return("sqlite")
  if (grepl(.regex_sin_caja("postgres|pqc|rpostgres"), clase)) {
    return("postgresql")
  }
  "desconocido"
}

.capacidad_fuente_bloques_dbi <- function(conexion) {
  motor <- .motor_fuente_bloques_dbi(conexion)
  clase <- paste(class(conexion), collapse = " ")
  info <- tryCatch(.info_dbi_corrida(conexion), error = function(e) list())
  texto_info <- paste(unlist(info, use.names = FALSE), collapse = " ")
  if (identical(motor, "desconocido") &&
      grepl(.regex_sin_caja("duckdb"), paste(clase, texto_info))) {
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
       grepl(.regex_sin_caja("char|text|clob|varchar|citext"), tipo))
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
      error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- .normalizacion_minusculas_vector(
        .nombres_para_operar(names(datos))
      )
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      determinista <- which(columnas == "collation_deterministic")[[1L]]
      for (i in seq_len(nrow(datos))) {
        campo <- as.character(datos[[nombre]][[i]])
        posicion <- match(
          .normalizacion_minusculas_vector(.nombres_para_operar(campo)),
          .normalizacion_minusculas_vector(.nombres_para_operar(campos))
        )
        if (is.na(posicion) || !textual[[posicion]]) next
        nombres[[posicion]] <- as.character(datos[[colacion]][[i]])
        valor <- datos[[determinista]][[i]]
        valor <- if (is.logical(valor)) valor else {
          .minusculas_ascii(as.character(valor)) %in% c("t", "true", "1", "yes")
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
      error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- .normalizacion_minusculas_vector(
        .nombres_para_operar(names(datos))
      )
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      for (i in seq_len(nrow(datos))) {
        posicion <- match(
          .normalizacion_minusculas_vector(.nombres_para_operar(
            as.character(datos[[nombre]][[i]])
          )),
          .normalizacion_minusculas_vector(.nombres_para_operar(campos))
        )
        if (is.na(posicion) || !textual[[posicion]]) next
        colacion_actual <- as.character(datos[[colacion]][[i]])
        nombres[[posicion]] <- colacion_actual
        determinismo[[posicion]] <- grepl(.regex_sin_caja("(_bin2?|binary)$"),
                                           colacion_actual)
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
      error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e))
    )
    datos <- if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    if (inherits(datos, "data.frame") && nrow(datos)) {
      columnas <- .normalizacion_minusculas_vector(
        .nombres_para_operar(names(datos))
      )
      nombre <- which(columnas == "column_name")[[1L]]
      colacion <- which(columnas == "collation_name")[[1L]]
      determinista <- which(columnas == "collation_deterministic")[[1L]]
      for (i in seq_len(nrow(datos))) {
        posicion <- match(
          .normalizacion_minusculas_vector(.nombres_para_operar(
            as.character(datos[[nombre]][[i]])
          )),
          .normalizacion_minusculas_vector(.nombres_para_operar(campos))
        )
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
    error = function(e) list(ok = FALSE, motivo = .mensaje_motor(e))
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
    tabla_sql <- .citar_identificador_dbi(conexion, tabla)
  }
  if (is.null(campos)) {
    # Las legibles, de la misma funcion que usan las demas puertas.
    info <- .columnas_tabla_dbi(conexion, tabla)
    campos <- info$campos[!.nombres_presentes(info$campos, info$ilegibles)]
  }
  campos <- as.character(campos)
  if (is.null(campos_sql)) {
    campos_sql <- vapply(campos, function(campo) {
      .citar_identificador_dbi(conexion, campo)
    }, character(1L), USE.NAMES = FALSE)
  }
  if (length(campos) != length(campos_sql)) {
    .detener("`campos` y `campos_sql` deben tener el mismo largo.", call. = FALSE)
  }
  if (!is.null(orden_muestra) && length(orden_muestra)) {
    orden_muestra <- as.character(orden_muestra)
    posiciones <- if (exists(".resolver_columnas_dbi", mode = "function")) {
      .resolver_columnas_dbi(orden_muestra, campos)
    } else match(
      .normalizacion_minusculas_vector(.nombres_para_operar(orden_muestra)),
      .normalizacion_minusculas_vector(.nombres_para_operar(campos))
    )
    if (anyNA(posiciones)) {
      .detener("`orden_muestra` contiene columnas inexistentes.", call. = FALSE)
    }
    orden_muestra <- campos[posiciones]
    orden_sql <- vapply(orden_muestra, function(campo) {
      .citar_identificador_dbi(conexion, campo)
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
  posiciones_pk <- integer()
  if (length(pk)) {
    posiciones_pk <- if (exists(".resolver_columnas_dbi", mode = "function")) {
      .resolver_columnas_dbi(pk, campos)
    } else match(
      .normalizacion_minusculas_vector(.nombres_para_operar(pk)),
      .normalizacion_minusculas_vector(.nombres_para_operar(campos))
    )
    pk <- if (all(!is.na(posiciones_pk))) campos[posiciones_pk] else character()
  }
  colaciones <- .collation_fuente_bloques_dbi(
    conexion, tabla, campos, motor, prototipo, tipos, presupuesto
  )
  textual_pk <- length(pk) > 0L && any(.tipos_textuales_fuente_dbi(
    pk, if (is.null(prototipo)) NULL else prototipo[posiciones_pk],
    if (is.null(tipos)) NULL else tipos[posiciones_pk]
  ))
  pk_pos <- posiciones_pk
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
        .citar_identificador_dbi(conexion, .ALIAS_LOCALIZADOR_DBI),
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
      .citar_identificador_dbi(conexion, campo)
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
      .citar_identificador_dbi(conexion, .ALIAS_ORDINAL_DBI)
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
             .citar_identificador_dbi(conexion, .ALIAS_LOCALIZADOR_DBI))
    )
  }
  if (identical(metodo, "row_number")) {
    proyeccion <- c(
      proyeccion,
      paste0(
        "ROW_NUMBER() OVER () AS ",
        .citar_identificador_dbi(conexion, .ALIAS_ORDINAL_DBI)
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
      pasadas = list(primera = "dbSendQuery + dbFetch", valor = "acumuladores por bloques",
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
  indice <- if (!is.null(posicion)) posicion else .indice_nombre(nombre, nombres)
  if (is.na(indice) || !length(indice)) {
    indice <- match(
      .normalizacion_minusculas_vector(.nombres_para_operar(nombre)),
      .normalizacion_minusculas_vector(.nombres_para_operar(nombres))
    )
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
      "Usar un driver con retenci\u00f3n incremental demostrada o mantener la v\u00eda",
      "SQL existente; no se reemplaza silenciosamente por dbGetQuery()."
    ), cota = NULL, alcance = NULL, bytes_retenidos = 0
  )
}

.familias_fuente_bloques_dbi <- function(metricas, campos, prototipo, tipos,
                                         incluir_valores = TRUE,
                                         max_bytes_procesamiento =
                                           .MAX_BYTES_ESTADO_BLOQUES,
                                         columnas_binarias = character()) {
  salida <- list()
  # `Inf` significa sin presupuesto de estado retenido. El acumulador de
  # bloques conserva un tope finito por seguridad cuando se lo invoca solo;
  # aqui la API DBI ya validó explicitamente que el usuario pidió `Inf`, por
  # lo que no corresponde reintroducir ese tope silenciosamente.
  max_bytes_estado <- if (is.infinite(max_bytes_procesamiento)) 1e15 else
    max_bytes_procesamiento
  for (i in seq_along(campos)) {
    campo <- campos[[i]]
    binaria <- campo %in% as.character(columnas_binarias)
    tipo <- if (!is.null(prototipo) && i <= length(prototipo)) {
      typeof(prototipo[[i]])
    } else "character"
    if ("validos" %in% metricas) {
      salida[[.clave_par_identificador(campo, "conteos", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, tipo, familia = "conteos",
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          incluir_ausentes = TRUE, max_bytes = max_bytes_estado
        )
    }
    textual <- .tipos_textuales_fuente_dbi(
      campo, if (is.null(prototipo)) NULL else prototipo[i],
      if (is.null(tipos)) NULL else tipos[i]
    )
    if (textual || binaria) {
      salida[[.clave_par_identificador(campo, "longitudes", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, "character", familia = "longitudes",
          configuracion = if (binaria) {
            list(representacion_binaria = TRUE)
          } else list(),
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
      salida[[.clave_par_identificador(campo, "cuantitativos", sep = "\u001f")]] <-
        .iniciar_acumulador(
          campo, tipo, familia = "cuantitativos",
          configuracion = list(
            escala = 1, escala_adaptativa = TRUE, contar_signos = TRUE
          ),
          fuente_id = NA_character_, snapshot_id = NA_character_,
          universo_id = "tabla_completa", orden_id = NA_character_,
          max_bytes = max_bytes_estado
        )
    }
    if ("distintos" %in% metricas) {
      salida[[.clave_par_identificador(campo, "distintos", sep = "\u001f")]] <-
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
    n <- .numero_dbi(resultado$n_evaluados)
    if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 1) {
      return(list(
        valor = NA_real_, estado = if (identical(n, 0)) "sin_valores" else
          "no_disponible",
        motivo = if (identical(n, 0)) {
          "La columna no contiene valores no nulos."
        } else "No se pudo conocer la cantidad de valores cuantitativos."
      ))
    }
    if (identical(metrica, "desvio") && n < 2) {
      return(list(
        valor = NA_real_, estado = "no_aplica",
        motivo = "El desv\u00edo muestral requiere al menos dos valores no nulos."
      ))
    }
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
    if (identical(metrica, "desvio") &&
        (length(valor) != 1L || !is.finite(valor))) {
      return(list(
        valor = NA_real_, estado = "no_disponible",
        motivo = "estadisticos_cuantitativos_no_finitos:desvio"
      ))
    }
    if (metrica %in% c("minimo", "maximo", "media", "n_ceros",
                      "n_negativos") && isTRUE(incluir_valores) &&
        (length(valor) != 1L || !is.finite(valor))) {
      return(list(
        valor = NA_real_, estado = "no_disponible",
        motivo = paste0(
          "estadisticos_cuantitativos_no_finitos:", metrica
        )
      ))
    }
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
  # La misma condicion de columna que retiene `minimo`, `media` y `desvio`; la
  # mediana se decide por su valor (`.mediana_central_integer64_bloques_dbi()`),
  # y si no se publico es porque su central pasa de 2^53 o porque la cantidad
  # es par.
  como_doble <- .integer64_como_doble_dbi(mapa$representante)
  if (!isTRUE(como_doble$ok)) return(como_doble$motivo)
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
  if (!any(is.finite(como_doble$valores) & frecuencias > 0)) {
    return(paste(
      "La columna no contiene valores finitos: los estad\u00edsticos de magnitud",
      "se calculan s\u00f3lo con valores finitos."
    ))
  }
  "familia_sin_acumulador:mediana_requiere_mapa_no_truncado"
}

.mediana_mapa_bloques_dbi <- function(mapa) {
  if (!is.data.frame(mapa) || !nrow(mapa)) return(NA_real_)
  # `order()` y `median()` de base sobre `integer64` ordenan por los bits -los
  # negativos salian `NaN`- y la mediana volvia `integer64` y truncada. Se
  # reconstruye sobre dobles, o no se reconstruye: ver
  # `.integer64_como_doble_dbi()`.
  como_doble <- .integer64_como_doble_dbi(mapa$representante)
  if (!isTRUE(como_doble$ok)) {
    return(.mediana_central_integer64_bloques_dbi(mapa))
  }
  valores <- como_doble$valores
  frecuencias <- as.numeric(mapa$frecuencia)
  if (!is.numeric(valores) || anyNA(frecuencias)) return(NA_real_)
  # Solo valores finitos, como `minimo`, `maximo`, `media` y `desvio` de la
  # misma fila -que salen de `.absorber_cuantitativos()` sobre `finitos`- y como
  # la mediana de `perfilar()`. `na.rm` saca `NA` y `NaN`, no los infinitos:
  # sobre `c(-Inf x3, 10, 20, 30, 40)` la mediana salia 10 -el minimo- al lado
  # de una media de 25.
  finitos <- is.finite(valores)
  valores <- valores[finitos]
  frecuencias <- frecuencias[finitos]
  if (!length(valores)) return(NA_real_)
  orden <- .orden_por_bytes(valores)
  valores <- valores[orden]
  frecuencias <- frecuencias[orden]
  total <- sum(frecuencias)
  if (!is.finite(total) || total < 1 ||
      total > .TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI) return(NA_real_)
  expandido <- rep(valores, frecuencias)
  stats::median(expandido, na.rm = TRUE)
}

# Las una o dos centrales del mapa de distintos, en la clase del mapa: la
# posicion `(n + 1) %/% 2` y la `(n + 2) %/% 2`, que son las que promedia
# `median()`. `.orden_por_bytes()` ordena un `integer64` por su rango, no por
# sus bits. `NULL` si el mapa no alcanza.
.centrales_mapa_bloques_dbi <- function(mapa) {
  if (!is.data.frame(mapa) || !nrow(mapa)) return(NULL)
  valores <- mapa$representante
  frecuencias <- suppressWarnings(as.numeric(mapa$frecuencia))
  presentes <- !is.na(valores) & !is.na(frecuencias) & frecuencias > 0
  if (is.double(valores)) presentes <- presentes & is.finite(valores)
  valores <- valores[presentes]
  frecuencias <- frecuencias[presentes]
  total <- sum(frecuencias)
  if (!length(valores) || !is.finite(total) || total < 1 ||
      total > .TOPE_RECONSTRUCCION_MEDIANA_BLOQUES_DBI) {
    return(NULL)
  }
  orden <- .orden_por_bytes(valores)
  valores <- valores[orden]
  acumuladas <- cumsum(frecuencias[orden])
  posiciones <- unique(c((total + 1) %/% 2, (total + 2) %/% 2))
  valores[vapply(posiciones, function(k) which(acumuladas >= k)[[1L]],
                 integer(1L))]
}

# La regla de la via SQL (`.causa_mediana_inexacta_dbi()`) sobre el mapa de un
# `integer64` con algun valor desde 2^53, pero aca las centrales estan a la
# vista: la mediana se publica si las dos llegan exactas al doble, y es la de
# `median()` sobre ellas. La misma que `perfilar()` (`.mediana_centrales_exactas()`).
.mediana_central_integer64_bloques_dbi <- function(mapa) {
  if (!inherits(mapa$representante, "integer64")) return(NA_real_)
  .mediana_centrales_exactas(.centrales_mapa_bloques_dbi(mapa))
}

# La fila de una columna en la via por bloques, antes de llenarla: la misma forma
# para la que se mide y para la que el esquema no pudo leer.
.fila_vacia_bloques_dbi <- function(campo, n_total) {
  fila <- .fila_resumen_dbi(campo, n_total)
  fila$n_nan <- NA_real_
  fila$n_infinito_positivo <- NA_real_
  fila$n_infinito_negativo <- NA_real_
  fila$longitud_minima <- NA_real_
  fila$longitud_maxima <- NA_real_
  fila$longitud_media <- NA_real_
  fila
}

.fila_y_registros_bloques_dbi <- function(campo, n_total, metricas,
                                          acumuladores, prototipo, tipos,
                                          incluir_valores, fuente,
                                          metricas_publicas = metricas,
                                          decisiones_costo = NULL,
                                          moda_precalculada = NULL,
                                          tipos_motor = NULL,
                                          prototipo_desconocido = FALSE,
                                          decimal_exacto = NULL) {
  fila <- .fila_vacia_bloques_dbi(campo, n_total)
  # La media y la mediana de un decimal que el motor sumo en su tipo (ver
  # `.media_mediana_decimal_bloques_dbi()`), en lugar de las de los dobles.
  exacta <- function(metrica, nombre) {
    decision <- decimal_exacto[[nombre]]
    if (!is.list(decision) || !identical(metrica$estado, "calculado")) {
      return(c(metrica, list(sql = fuente$consulta)))
    }
    list(
      valor = if (isTRUE(decision$ok)) decision$valor else NA_real_,
      estado = if (isTRUE(decision$ok)) "calculado" else "no_disponible",
      motivo = if (isTRUE(decision$ok)) NA_character_ else decision$motivo,
      sql = decision$sql, exacta = isTRUE(decision$ok)
    )
  }
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
    sobre <- acumuladores[[.clave_par_identificador(campo, grupo, sep = "\u001f")]]
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
    conteos <- acumuladores[[.clave_par_identificador(campo, "conteos", sep = "\u001f")]]
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
        campo, nombre, "no_solicitado", "La m\u00e9trica no fue solicitada.",
        NA_character_, metadatos = .metadatos_sql_dbi()
      )
    }
  }
  if ("distintos" %in% metricas_publicas) {
    sobre <- acumuladores[[.clave_par_identificador(campo, "distintos", sep = "\u001f")]]
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
        campo, nombre, "no_solicitado", "La m\u00e9trica no fue solicitada.",
        NA_character_, metadatos = .metadatos_sql_dbi()
      )
    }
  }
  cuantitativos <- acumuladores[[.clave_par_identificador(campo, "cuantitativos", sep = "\u001f")]]
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
  posicion_campo <- match(campo, fuente$campos)
  # El prototipo sale de un `dbFetch(n = 0)`: tiene largo cero por diseno, y lo
  # que dice es su clase. Exigir largo mayor que cero lo descartaba siempre y
  # dejaba solo el tipo declarado, que en SQL Server es un codigo ODBC: la
  # mediana de un `int` salia «La columna no es numerica». Cuando el
  # presupuesto se agoto antes de leer los tipos, el esquema minimo lo declara
  # (`prototipo_desconocido`): sus `logical()` no son una columna logica.
  prototipo_conocido <- !isTRUE(prototipo_desconocido) &&
    !is.na(posicion_campo) && !is.null(prototipo) &&
    posicion_campo <= length(prototipo) &&
    !is.null(prototipo[[posicion_campo]])
  tipo_conocido <- !is.na(posicion_campo) && !is.null(tipos) &&
    posicion_campo <= length(tipos) && !is.na(tipos[[posicion_campo]])
  columna_numerica <- if (prototipo_conocido || tipo_conocido) {
    .es_numerico_dbi(
      if (prototipo_conocido) prototipo[[posicion_campo]] else NULL,
      if (tipo_conocido) tipos[[posicion_campo]] else NA_character_
    )
  } else NA
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
        metrica <- if (nombre %in% c("media", "desvio")) {
          exacta(metrica, nombre)
        } else c(metrica, list(sql = fuente$consulta))
        if (nombre %in% names(fila)) fila[[nombre]] <- metrica$valor
        registros[[length(registros) + 1L]] <- .registro_sql_dbi(
          campo, nombre, metrica$estado, metrica$motivo, metrica$sql,
          metadatos = .metadatos_sql_dbi(
            alcance = "tabla_completa", universo = "tabla_completa",
            metodo = if (isTRUE(metrica$exacta)) {
              "sumas_exactas_decimal"
            } else "dbfetch_bloques",
            error_esperado = "no_aplica",
            id_consulta = NA_integer_, columnas_compartidas = length(fuente$campos)
          ), medicion = list(consulta_id = 1L, etapa = "dbfetch_bloques")
        )
      }
    } else {
      for (nombre in pedidos) {
        disponible <- grupo %in% metricas &&
          (isTRUE(columna_numerica) || is.na(columna_numerica))
        registros[[length(registros) + 1L]] <- .registro_sql_dbi(
          campo, nombre, if (disponible) "no_disponible" else if (
            grupo %in% metricas
          ) "no_aplica" else "no_solicitado",
          if (disponible) {
            paste(
              "No se pudo iniciar el acumulador cuantitativo; la m\u00e9trica",
              "queda no disponible por el presupuesto de la v\u00eda por bloques."
            )
          } else if (grupo %in% metricas) "La columna no es num\u00e9rica." else
            "La m\u00e9trica no fue solicitada.", NA_character_,
          metadatos = .metadatos_sql_dbi()
        )
      }
    }
  }
  longitudes <- acumuladores[[.clave_par_identificador(campo, "longitudes", sep = "\u001f")]]
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
  # Moda y mediana quedan como familias de valores no fusionables en la lectura por bloques. Si el
  # mapa exacto ya existe se publica la moda; la mediana se calcula solo sobre
  # un mapa no truncado y se declara como acotada por esa representacion.
  distintos <- acumuladores[[.clave_par_identificador(campo, "distintos", sep = "\u001f")]]
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
  if ("moda" %in% metricas && isTRUE(incluir_valores) &&
      !is.null(moda_precalculada)) {
    decision_moda <- decisiones_costo[[campo]]
    if (!is.null(decision_moda) && identical(decision_moda$moda, FALSE)) {
      estado_valores <- "omitido_por_costo"
      motivo_valores <- .motivo_decision_costo_dbi(decision_moda, "moda")
    } else if (!isTRUE(moda_precalculada$ok)) {
      estado_valores <- "no_disponible"
      motivo_valores <- moda_precalculada$motivo
    } else if (is.data.frame(moda_precalculada$datos) &&
               nrow(moda_precalculada$datos)) {
      celda <- .valor_campo_dbi(moda_precalculada$datos, "valor")
      frecuencia <- .valor_campo_dbi(
        moda_precalculada$datos, "frecuencia"
      )
      valor_moda <- if (isTRUE(celda$ok)) tryCatch(
        .texto_moda(celda$valor), error = function(e) e
      ) else NULL
      if (!isTRUE(celda$ok) || !isTRUE(frecuencia$ok) ||
          inherits(valor_moda, "error")) {
        estado_valores <- "no_disponible"
        motivo_valores <- if (inherits(valor_moda, "error")) {
          .mensaje_motor(valor_moda)
        } else if (!isTRUE(celda$ok)) celda$motivo else frecuencia$motivo
      } else {
        fila$moda <- valor_moda
        fila$frecuencia_moda <- .conteo_dbi(frecuencia$valor)
        estado_valores <- "calculado"
        motivo_valores <- NA_character_
      }
    } else {
      fila$frecuencia_moda <- 0
      estado_valores <- "sin_valores"
      motivo_valores <- "La columna no contiene valores no nulos."
    }
  } else if ("moda" %in% metricas && isTRUE(incluir_valores) &&
             is.data.frame(mapa) && nrow(mapa)) {
    decision_moda <- decisiones_costo[[campo]]
    if (!is.null(decision_moda) && identical(decision_moda$moda, FALSE)) {
      estado_valores <- "omitido_por_costo"
      motivo_valores <- .motivo_decision_costo_dbi(decision_moda, "moda")
    } else {
      # El desempate es el del resto del paquete -`.moda_de_mapa()`-, no el
      # orden del mapa.
      # Y la escritura tambien: `as.character()` publicaba "1e-20" y
      # "1.234e-05" donde `perfilar()` y el agregado SQL publican
      # "0.00000000000000000001" y "0.00001234", y `comparar_equivalencia()`
      # declaraba `materialmente_distinto` el mismo valor. `.moda_de_mapa()` ya
      # trae el texto escrito con `.texto_moda()`.
      moda_mapa <- .moda_de_mapa(mapa)
      indice <- if (is.null(moda_mapa)) which.max(mapa$frecuencia) else {
        moda_mapa$posicion
      }
      fila$moda <- if (!is.null(moda_mapa)) moda_mapa$valor else tryCatch(
        .texto_moda(mapa$representante[indice]),
        error = function(e) NA_character_
      )
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
    motivo_valores <- "La m\u00e9trica no fue solicitada."
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
      "La m\u00e9trica no fue solicitada.", NA_character_,
      metadatos = .metadatos_sql_dbi()
    )
  }
  # La mediana de una columna que no es numerica no se mide: es `no_aplica`, como
  # en el agregado SQL y con la misma pregunta (`.es_numerico_dbi()`). Salia
  # `no_disponible` con `mapa_distintos_no_reconstruible`, y la corroboracion
  # declaraba `divergencia` contra la mediana que el perfil de muestra calcula
  # sobre un texto que lee como numero ("3,75").
  if ("mediana" %in% metricas && isFALSE(columna_numerica)) {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", "no_aplica", "La columna no es num\u00e9rica.",
      NA_character_, metadatos = .metadatos_sql_dbi()
    )
  } else if ("mediana" %in% metricas && is.na(columna_numerica)) {
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", "no_disponible",
      "No se pudo conocer el tipo de la columna.", NA_character_,
      metadatos = .metadatos_sql_dbi()
    )
  } else if ("mediana" %in% metricas && isTRUE(incluir_valores)) {
    fila$mediana <- .mediana_mapa_bloques_dbi(mapa)
    estado_mediana <- if (is.na(fila$mediana)) "no_disponible" else "calculado"
    motivo_mediana <- if (is.na(fila$mediana)) {
      if (mapa_truncado) motivo_mapa else .motivo_mediana_mapa_bloques_dbi(mapa)
    } else NA_character_
    mediana <- exacta(list(
      valor = fila$mediana, estado = estado_mediana, motivo = motivo_mediana
    ), "mediana")
    fila$mediana <- mediana$valor
    estado_mediana <- mediana$estado
    motivo_mediana <- mediana$motivo
    registros[[length(registros) + 1L]] <- .registro_sql_dbi(
      campo, "mediana", estado_mediana, motivo_mediana, mediana$sql,
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
      campo, "mediana", "no_solicitado", "La m\u00e9trica no fue solicitada.",
      NA_character_, metadatos = .metadatos_sql_dbi()
    )
  }
  sql <- do.call(rbind, registros)
  # La guarda del decimal desde 2^53 (ver `.tipo_declarado_decimal_dbi()`). Aca
  # todo sale de los dobles que entrego el controlador: sobre un `numeric(20,0)`
  # con 60 valores distintos desde 90071992547409001, `n_distintos` salia 4, una
  # moda que no estaba en la columna y un desvio de 17,52 contra 17,46, todo
  # `calculado`. Las siete de magnitud no se publican, como
  # en la via SQL, y tampoco `n_distintos` y la moda: sin bloques los cuenta el
  # motor sobre el valor exacto, y aca se contarian sobre los dobles. Lo mismo
  # vale para un entero ancho que el controlador entrego como doble -un
  # `integer64` no pasa por aca: lo retiene su propia regla-.
  tipo_campo <- if (!is.null(tipos_motor) && !is.na(posicion_campo) &&
                    posicion_campo <= length(tipos_motor) &&
                    !is.na(tipos_motor[[posicion_campo]])) {
    tipos_motor[[posicion_campo]]
  } else if (!is.null(tipos) && !is.na(posicion_campo) &&
             posicion_campo <= length(tipos)) {
    tipos[[posicion_campo]]
  } else NA_character_
  extremos <- if (!is.null(sobre_cuantitativos) &&
                  identical(sobre_cuantitativos$estado, "calculado")) {
    c(sobre_cuantitativos$resultado$minimo, sobre_cuantitativos$resultado$maximo)
  } else NULL
  entero_ancho <- .tipo_declarado_entero_ancho_dbi(tipo_campo)
  tipo_campo_texto <- if (length(tipo_campo) == 1L && !is.na(tipo_campo)) {
    .minusculas_ascii(as.character(tipo_campo))
  } else ""
  entero_sin_signo <- grepl("unsigned", tipo_campo_texto, fixed = TRUE, useBytes = TRUE)
  unsigned_inconsistente <- entero_sin_signo && length(extremos) &&
    any(is.finite(extremos) & extremos < 0)
  if ((entero_ancho || .tipo_declarado_decimal_dbi(tipo_campo) ||
       unsigned_inconsistente) &&
      is.double(extremos) &&
      (unsigned_inconsistente ||
       any(is.finite(extremos) & abs(extremos) >= .MAX_ENTERO_EXACTO_DBI))) {
    retenidas <- c(
      "minimo", "maximo", "media", "n_ceros", "n_negativos", "mediana",
      "desvio", "n_distintos", "tasa_distintos"
    )
    if (is.null(moda_precalculada) || !isTRUE(moda_precalculada$ok)) {
      retenidas <- c(retenidas, "moda", "frecuencia_moda")
    }
    # La media y el desvio de un decimal que salieron de las sumas exactas del
    # motor no dependen de esos dobles.
    for (nombre in c("media", "desvio")) {
      if (isTRUE(decimal_exacto[[nombre]]$ok)) {
        retenidas <- setdiff(retenidas, nombre)
      }
    }
    # La mediana, por sus centrales, que el mapa tiene a la vista: el redondeo
    # es monotono, y una central que llego como doble por debajo de 2^53 llego
    # exacta. Un `UNSIGNED` que aparece negativo no: ahi el doble es otro
    # numero.
    centrales <- if (unsigned_inconsistente) NULL else {
      .centrales_mapa_bloques_dbi(mapa)
    }
    mediana_por_valor <- "mediana" %in% names(fila) &&
      length(fila$mediana) == 1L && is.finite(fila$mediana) &&
      length(centrales) > 0L && is.double(centrales) &&
      !inherits(centrales, "integer64") &&
      all(abs(centrales) < .MAX_ENTERO_EXACTO_DBI)
    if (mediana_por_valor) retenidas <- setdiff(retenidas, "mediana")
    for (nombre in intersect(retenidas, names(fila))) {
      fila[[nombre]] <- if (is.character(fila[[nombre]])) {
        NA_character_
      } else NA_real_
    }
    afectadas <- as.character(sql$metrica) %in% retenidas &
      as.character(sql$estado) == "calculado"
    sql$estado[afectadas] <- "no_disponible"
    motivo_columna <- if (unsigned_inconsistente) {
      paste(
        "El controlador entreg\u00f3 un `UNSIGNED` fuera del rango del doble con",
        "signo: los valores aparecen negativos. Las m\u00e9tricas de magnitud no",
        "se publican como calculadas."
      )
    } else if (entero_ancho) {
      paste(
        .motivo_entero_doble_dbi(
          prefijo = "La columna ", tipo = tipo_campo, mediana_por_valor = TRUE
        ),
        "Con `bloque_filas` tampoco `n_distintos` ni la moda: se contarian en",
        "R sobre esos dobles."
      )
    } else .motivo_decimal_doble_dbi(bloques = TRUE, mediana_por_valor = TRUE)
    sql$motivo[afectadas] <- motivo_columna
  }
  # Con bloques el desvio sale de los dobles que entrego el controlador: un
  # decimal muy grande frente a su desvio no se publica (ver
  # `.desvio_mal_condicionado_dbi()`).
  # Salvo que el motor lo haya calculado restando en el decimal (ver
  # `.media_mediana_decimal_bloques_dbi()`).
  if (.tipo_decimal_o_moneda_dbi(tipo_campo) && is.double(extremos) &&
      "desvio" %in% names(fila) && !isTRUE(decimal_exacto$desvio$ok) &&
      .desvio_mal_condicionado_dbi(fila$desvio, extremos, tipo = tipo_campo)) {
    fila$desvio <- NA_real_
    tapar <- as.character(sql$metrica) == "desvio" &
      as.character(sql$estado) == "calculado"
    sql$estado[tapar] <- "no_disponible"
    sql$motivo[tapar] <- .MOTIVO_DESVIO_DECIMAL_DOBLE_DBI
  }
  list(fila = fila, sql = sql)
}

.plan_bloques_fuente_dbi <- function(fuente, bloque_filas, campos, metricas,
                                     bloque_muestra = "con_muestra",
                                     muestra = NA_real_, orden_muestra = NULL,
                                     previas = NULL, posteriores = NULL) {
  plan <- data.frame(
    clase_consulta = "fuente por bloques (dbSendQuery + dbFetch)",
    n_consultas = if (isTRUE(fuente$disponible)) 1L else 0L,
    alcance = "tabla_completa",
    n_consultas_max = if (isTRUE(fuente$disponible)) 1L else 0L,
    stringsAsFactors = FALSE
  )
  # Lo que la corrida manda antes y despues del recorrido (ver
  # `.plan_bloques_dbi()`); sin ellas el plan contaba solo el recorrido, y una
  # corrida de veinte consultas se anunciaba como de una.
  if (!is.null(previas) || !is.null(posteriores)) {
    plan$n_consultas <- as.numeric(plan$n_consultas)
    plan$n_consultas_max <- as.numeric(plan$n_consultas_max)
    plan <- rbind(previas, plan, posteriores)
    plan <- plan[plan$n_consultas > 0 | plan$n_consultas_max > 0, , drop = FALSE]
    rownames(plan) <- NULL
  }
  attr(plan, "total") <- sum(plan$n_consultas)
  attr(plan, "total_minimo") <- sum(plan$n_consultas)
  attr(plan, "total_maximo") <- sum(plan$n_consultas_max)
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
    primera = "dbFetch incremental", valor = "acumuladores por bloques",
    indice = "s\u00f3lo con orden estable y snapshot demostrado",
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

# Las clases del plan por bloques, con las mismas preguntas que hace
# `.perfilar_dbi_bloques()` para decidir cada consulta: la preparacion y la
# estimacion del aviso de costo (`emitidas`, `catalogo_aviso`), las pruebas de
# capacidad de la fuente (`pruebas_fuente`, las que `.fuente_bloques_dbi()`
# acaba de pagar), la
# segunda pasada del localizador, la moda exacta de los binarios y los
# decimales anchos, la media, la mediana y el desvio de los decimales en el
# decimal, y la lectura de `perfil_muestra`. El minimo supone que los datos no
# piden ninguna consulta mas; el maximo, que piden todas las que pueden: la
# mediana de un decimal se pide solo si sus dos centrales son dobles distintos,
# el desvio solo si la cota del peor caso no alcanza, y la moda de un entero
# leido como doble solo si sus extremos pasan de 2^53.
.plan_bloques_dbi <- function(conexion, preparacion, fuente, bloque_filas,
                              metricas, incluir_valores, tope_muestra,
                              emitidas, pruebas_fuente = 0, catalogo_aviso = 0,
                              orden_muestra = preparacion$orden_muestra) {
  fila <- function(clase, n, alcance, maximo = n) {
    data.frame(clase_consulta = clase, n_consultas = as.numeric(n),
               alcance = alcance, n_consultas_max = as.numeric(maximo),
               stringsAsFactors = FALSE)
  }
  previas <- rbind(
    fila("portones (esquema y sondas)", emitidas, "una vez"),
    fila("estimacion de filas del catalogo (aviso de costo)", catalogo_aviso,
         "lee el catalogo"),
    fila("pruebas de capacidad de la fuente por bloques", pruebas_fuente,
         "lee el catalogo")
  )
  posteriores <- NULL
  if (isTRUE(fuente$disponible) &&
      identical(preparacion$universo, "tabla_completa")) {
    senas <- .senas_conexion_dbi(conexion)
    tipos_motor <- preparacion$esquema$tipos_motor
    tipo_de <- function(i) {
      tipo <- if (!is.null(tipos_motor) && i <= length(tipos_motor)) {
        tipos_motor[[i]]
      } else NA_character_
      if (length(tipo) != 1L || is.na(tipo) || !nzchar(as.character(tipo))) {
        tipo <- if (i <= length(preparacion$tipos)) preparacion$tipos[[i]] else
          NA_character_
      }
      as.character(tipo)
    }
    campos <- preparacion$campos
    modas_seguras <- 0
    modas_posibles <- 0
    if ("moda" %in% metricas && isTRUE(incluir_valores)) {
      for (i in seq_along(campos)) {
        tipo <- tipo_de(i)
        exacta <- .es_tipo_binario_muestra_dbi(tipo, senas) || (
          .tipo_declarado_decimal_dbi(tipo) &&
            .tipo_declarado_entero_ancho_dbi(tipo)
        )
        if (exacta) {
          modas_seguras <- modas_seguras + 1
        } else if (.entero_doble_ancho_moda_dbi(
          if (i <= length(preparacion$prototipo)) preparacion$prototipo[[i]] else
            NULL, tipo, NULL
        )) {
          modas_posibles <- modas_posibles + 1
        }
      }
      # Con la politica por cardinalidad la moda depende de los distintos que
      # cuente el recorrido: ninguna es segura.
      if (identical(preparacion$politica_costo$nombre, "por_cardinalidad")) {
        modas_posibles <- modas_posibles + modas_seguras
        modas_seguras <- 0
      }
    }
    es_decimal <- vapply(seq_along(campos), function(i) {
      tipo <- tipo_de(i)
      .mediana_decimal_por_centrales_dbi(senas, tipo) &&
        i <= length(preparacion$campos_sql_proyeccion)
    }, logical(1L))
    decimales <- sum(es_decimal)
    # La media y el desvio de los decimales van en lotes de una consulta
    # (`.sumas_exactas_decimales_dbi()`), los mismos que arma la corrida; si
    # el motor rechaza uno, se parte hasta aislar la columna.
    lotes <- if (any(c("basicos", "desvio") %in% metricas) && decimales > 0) {
      .lotes_decimales_dbi(
        senas, vapply(which(es_decimal), tipo_de, character(1L)),
        preparacion$tamano_lote_planos %||% .TAMANO_LOTE_PLANOS_DBI
      )
    } else list()
    media <- length(lotes)
    medianas <- if ("mediana" %in% metricas && isTRUE(incluir_valores)) {
      decimales
    } else 0
    desvio <- .consultas_maximas_lotes_decimales_dbi(lotes) - length(lotes)
    con_muestra <- .solicita_muestra_dbi(preparacion$bloque_muestra)
    posteriores <- rbind(
      fila("segunda pasada del localizador",
           if (identical(fuente$metodo_orden, "row_locator")) 1 else 0,
           "tabla_completa"),
      fila("moda exacta en el motor", modas_seguras,
           "ordena o agrupa la tabla completa", modas_seguras + modas_posibles),
      fila("media, mediana y desvio de los decimales en el decimal", media,
           "escanea la tabla completa", media + medianas + desvio),
      fila("verificacion de unicidad del orden",
           if (con_muestra && length(fuente$orden_expresiones)) 1 else 0,
           "ordena o agrupa la tabla completa"),
      fila("muestra", if (!con_muestra) 0 else if (
        isTRUE(tope_muestra$requiere_sonda_bytes)
      ) 2 else 1, .ALCANCE_BLOQUE_CLIENTE_DBI)
    )
  }
  .plan_bloques_fuente_dbi(
    fuente, bloque_filas, preparacion$campos, metricas,
    preparacion$bloque_muestra, preparacion$muestra, orden_muestra,
    previas = previas, posteriores = posteriores
  )
}

.recorrer_fuente_bloques_dbi <- function(conexion, fuente, metricas,
                                         prototipo, tipos, bloque_filas,
                                         vigilante = NULL,
                                         incluir_valores = TRUE,
                                         max_bytes_procesamiento =
                                           .MAX_BYTES_ESTADO_BLOQUES,
                                         tipos_motor = NULL,
                                         columnas_binarias = character(),
                                         sentinelas = NULL) {
  metricas_acumuladores <- unique(c(
    metricas,
    if (isTRUE(incluir_valores) &&
        any(c("moda", "mediana") %in% metricas)) "distintos" else character()
  ))
  acumuladores <- .familias_fuente_bloques_dbi(
    metricas_acumuladores, fuente$campos, prototipo, tipos,
    incluir_valores = incluir_valores,
    max_bytes_procesamiento = max_bytes_procesamiento,
    columnas_binarias = columnas_binarias
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
  resultado <- tryCatch(
    .enviar_consulta_dbi(conexion, fuente$consulta),
    error = function(e) {
      .repropagar_transaccion_dbi(e)
      e
    }
  )
  if (inherits(resultado, "condition")) {
    return(list(acumuladores = acumuladores, sobres = list(),
                bloques = bloques, bytes = list(max_bloque = 0,
                  entrada = 0, texto = 0, retenidos = 0,
                  rss_maximo = NA_real_),
                locators = locators,
                error = paste0("no_disponible:dbSendQuery:", .mensaje_motor(resultado))))
  }
  bloques$consultas_sql <- 1L
  on.exit(.limpiar_consulta_dbi(resultado, conexion), add = TRUE)
  ordinal <- 0
  repeat {
    bloque <- tryCatch(
      .traer_filas_dbi(resultado, n = as.integer(bloque_filas), conexion),
      error = function(e) {
        .repropagar_transaccion_dbi(e)
        e
      }
    )
    bloques$fetches <- bloques$fetches + 1L
    if (inherits(bloque, "condition")) {
      return(list(acumuladores = acumuladores, sobres = list(),
                  bloques = bloques,
                  bytes = list(max_bloque = bytes_max, entrada = bytes_entrada,
                                texto = 0,
                                retenidos = sum(vapply(acumuladores, .bytes_retenidos, numeric(1L)),
                                                na.rm = TRUE), rss_maximo = NA_real_),
                  locators = locators,
                  error = paste0("no_disponible:dbFetch:", .mensaje_motor(bloque))))
    }
    if (!inherits(bloque, "data.frame")) bloque <- as.data.frame(bloque)
    # La fuente por bloques calcula el resumen de la tabla completa. Un
    # centinela fraccionario declarado para `real` solo se alinea al perfilar la
    # muestra, donde hace falta para que R reconozca la ausencia; reescribirlo
    # aca cambiaba el minimo y la media publicados por el resumen completo.
    bloque <- .marcar_utf8_tabla(bloque)
    # Un driver puede decidir la clase por los valores -RSQLite da `integer64`
    # solo si alguno no entra en 32 bits-, y entonces el prototipo de cero filas
    # que mira `.preparar_dbi()` no la ve: los acumuladores la ven aca.
    .exigir_integer64_confiable(bloque, via_dbi = TRUE)
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
    "La muestra diagn\u00f3stica no fue solicitada en la lectura por bloques."
  } else {
    "muestra_diagnostica_acotada:se_resuelve_en_perfil_muestra"
  }
  for (familia in familias_no_fusionables) {
    cobertura <- rbind(cobertura, .registro_cobertura_dbi(
      "perfil_muestra", familia, estado, motivo,
      "Solicitar el bloque de muestra para ejecutar diagn\u00f3sticos acotados.",
      NA_character_
    ))
  }
  cobertura
}

# Los argumentos que la via DBI toma de `...` para si y no le pasa a
# `perfilar()`: la validacion de la entrada (`.validar_argumentos_para_perfilar()`)
# los acepta. `antes_segunda_pasada` corre entre las dos pasadas del recorrido
# por bloques; lo usa la prueba que cambia la tabla en medio.
.ARGUMENTOS_PROPIOS_DBI <- "antes_segunda_pasada"

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
  rs <- tryCatch(
    .enviar_consulta_dbi(conexion, fuente$consulta),
    error = function(e) {
      .repropagar_transaccion_dbi(e)
      e
    }
  )
  if (inherits(rs, "condition")) {
    return(list(estado = "no_disponible", identidad = NA,
                motivo = paste0("no_disponible:dbSendQuery:", .mensaje_motor(rs)),
                consultas_sql = 1L, fetches = 0L))
  }
  consultas_sql <- 1L
  fetches <- 0L
  on.exit(.limpiar_consulta_dbi(rs, conexion), add = TRUE)
  observados <- list()
  repeat {
    bloque <- tryCatch(
      .traer_filas_dbi(rs, n = as.integer(bloque_filas), conexion),
      error = function(e) {
        .repropagar_transaccion_dbi(e)
        e
      }
    )
    fetches <- fetches + 1L
    if (inherits(bloque, "condition")) {
      return(list(estado = "no_disponible", identidad = NA,
                  motivo = paste0("no_disponible:dbFetch:", .mensaje_motor(bloque)),
                  consultas_sql = consultas_sql, fetches = fetches))
    }
    if (!nrow(bloque)) break
    observados[[length(observados) + 1L]] <- .valor_columna_bloque_dbi(
      .marcar_utf8_tabla(bloque), .ALIAS_LOCALIZADOR_DBI
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
  metricas_publicas <- .identificadores_unicos(
    as.character(metricas_publicas %||% metricas)
  )
  if (!identical(preparacion$universo, "tabla_completa")) {
    motivo <- "no_disponible:fuente_bloques_solo_tabla_completa"
    fuente <- structure(list(
      disponible = FALSE, capacidad = .capacidad_fuente_bloques_dbi(conexion),
      motivo = motivo, consulta = NA_character_, orden_sql = NA_character_,
      orden_id = NA_character_, metodo_orden = "resultset_no_reproducible",
      estable = FALSE, snapshot_id = NA_character_,
      fuente_id = paste0("dbi:", .texto_tabla_dbi(tabla)), tabla = tabla,
      tabla_sql = preparacion$tabla_sql, campos = preparacion$campos,
      campos_sql = preparacion$campos_sql_bloques %||%
        preparacion$campos_sql_proyeccion, localizador = NULL,
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
        "Usar `bloque_filas` s\u00f3lo con `universo = \"tabla_completa\"` en la lectura por bloques.",
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
    salida <- .sellar_perfil_dbi(salida)
    return(salida)
  }
  # Lo que la corrida ya pago antes de llegar aca -la preparacion y la
  # estimacion del aviso de costo- y lo que pagan las pruebas de la fuente: el
  # plan publicado es el de `plan_perfilado_dbi()`, con estas cuentas.
  catalogo_aviso <- .consultas_catalogo_aviso_dbi(conexion, tabla, preparacion)
  usadas_antes_fuente <- if (is.null(preparacion$presupuesto)) 0 else
    preparacion$presupuesto$usadas
  fuente <- .fuente_bloques_dbi(
    conexion, tabla, tabla_sql = preparacion$tabla_sql,
    campos = preparacion$campos,
    campos_sql = preparacion$campos_sql_bloques %||%
      preparacion$campos_sql_proyeccion,
    prototipo = preparacion$prototipo, tipos = preparacion$tipos,
    orden_muestra = preparacion$orden_muestra, orden_sql = preparacion$orden_sql,
    clave = preparacion$catalogo_cardinalidad,
    dialecto = preparacion$dialecto,
    presupuesto = preparacion$presupuesto, bloque_filas = bloque_filas
  )
  plan <- .plan_bloques_dbi(
    conexion, preparacion, fuente, bloque_filas, metricas, incluir_valores,
    .tope_muestra_plan_dbi(
      preparacion$muestra, length(preparacion$campos), max_celdas_muestra,
      max_bytes_muestra
    ),
    emitidas = usadas_antes_fuente - catalogo_aviso,
    pruebas_fuente = if (is.null(preparacion$presupuesto)) 0 else
      preparacion$presupuesto$usadas - usadas_antes_fuente,
    catalogo_aviso = catalogo_aviso
  )
  vigilante <- .iniciar_vigilante(
    corrida_id = paste0("dbi-bloques-", as.integer(Sys.time())),
    tope_bytes = max_bytes_procesamiento
  )
  recorrido <- .recorrer_fuente_bloques_dbi(
    conexion, fuente, metricas, preparacion$prototipo, preparacion$tipos,
    bloque_filas, vigilante = vigilante,
    incluir_valores = incluir_valores,
    max_bytes_procesamiento = max_bytes_procesamiento,
    tipos_motor = preparacion$esquema$tipos_motor,
    columnas_binarias = preparacion$columnas_binarias_bloques,
    sentinelas = argumentos[["sentinelas_numericos"]]
  )
  # El recorrido no pasa por el presupuesto -sin el no hay perfil- pero viaja
  # al motor: se cuenta antes de las consultas que si pueden quedar afuera, para
  # que estas decidan con lo que de verdad queda.
  .contar_dbi(preparacion$presupuesto, recorrido$bloques$consultas_sql %||% 0L)
  if (isTRUE(fuente$disponible) && !is.null(recorrido$error)) {
    extras <- argumentos
    extras$antes_segunda_pasada <- NULL
    extras$columnas_opcionales <- NULL
    extras$aplicabilidad <- NULL
    extras$sentinelas_numericos <- NULL
    fallback <- tryCatch(
      do.call(
        perfilar_dbi,
        c(
          list(
            conexion = conexion, tabla = tabla, universo = "tabla_completa",
            muestra = preparacion$muestra,
            orden_muestra = preparacion$orden_muestra,
            metricas = metricas_publicas,
            estrategia_distintos = preparacion$estrategia_distintos$estrategia_solicitada %||%
              "exacta",
            estrategia_mediana = preparacion$estrategia_mediana,
            politica_costo = preparacion$politica_costo$nombre %||% "todas",
            bloque_muestra = preparacion$bloque_muestra, max_consultas = Inf,
            dialecto = "auto", incluir_valores = incluir_valores,
            tamano_lote = preparacion$tamano_lote,
            tamano_lote_planos = preparacion$tamano_lote_planos,
            tamano_lote_distintos = preparacion$tamano_lote_distintos,
            instrumentar = preparacion$instrumentar,
            umbral_cardinalidad = preparacion$politica_costo$umbral %||% 0.5,
            columnas_opcionales = argumentos$columnas_opcionales %||% character(),
            aplicabilidad = argumentos$aplicabilidad,
            sentinelas_numericos = argumentos$sentinelas_numericos %||%
              c(-9, -99, -999, -9999, 999),
            max_bytes_procesamiento = max_bytes_procesamiento,
            max_bytes_materializacion = preparacion$max_bytes_materializacion
          ),
          extras
        )
      ),
      error = function(e) {
        .repropagar_transaccion_dbi(e)
        NULL
      }
    )
    if (!is.null(fallback) && !is.null(fallback$resumen_tabla)) {
      fallback$resumen_tabla$cobertura <- rbind(
        fallback$resumen_tabla$cobertura,
        .registro_cobertura_dbi(
          "fuente_bloques", .texto_tabla_dbi(tabla), "no_disponible",
          paste0(
            "El recorrido por bloques fall\u00f3 (", recorrido$error,
            "); se devolvi\u00f3 el resumen SQL aislado por columna."
          ),
          paste(
            "Corregir la vista o usar el resumen SQL hasta contar con un",
            "recorrido de bloques que pueda aislar la columna culpable."
          ),
          fuente$consulta
        )
      )
      fallback$resumen_tabla$meta$fuente_bloques_fallback <- list(
        motivo = recorrido$error, bloques = recorrido$bloques
      )
      rownames(fallback$resumen_tabla$cobertura) <- NULL
      return(fallback)
    }
  }
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
  .contar_dbi(preparacion$presupuesto, segunda_pasada$consultas_sql %||% 0L)
  agregados_costo <- list(conteos = stats::setNames(lapply(
    preparacion$campos, function(campo) {
      sobre <- recorrido$sobres[[.clave_par_identificador(campo, "conteos", sep = "\u001f")]]
      mapa <- recorrido$sobres[[.clave_par_identificador(campo, "distintos", sep = "\u001f")]]
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
  # Las columnas binarias llegan a R como listas de `raw`, que no pueden entrar
  # en el mapa de distintos sin cambiar su representacion. La misma expresion
  # de largo y MD5 que usa la via SQL permite resolver la moda exacta sin
  # materializar los bytes ni escribir temporales.
  modas_bloques <- list()
  tipo_motor_de <- function(campo) {
    posicion <- match(campo, preparacion$campos)
    tipos_motor <- preparacion$esquema$tipos_motor
    tipo <- if (!is.null(tipos_motor) && posicion <= length(tipos_motor)) {
      tipos_motor[[posicion]]
    } else NA_character_
    if (length(tipo) != 1L || is.na(tipo) || !nzchar(as.character(tipo))) {
      tipo <- if (posicion <= length(preparacion$tipos)) {
        preparacion$tipos[[posicion]]
      } else NA_character_
    }
    as.character(tipo)
  }
  # Lo que el total del plan anuncia y falta pagar queda reservado (ver
  # `.plan_bloques_dbi()`): la moda de un entero leido como doble, la mediana
  # por centrales y el desvio centrado los piden los datos y entran solo con lo
  # que sobra. Sin la reserva, con un `max_consultas` igual al total del plan,
  # una de esas consultas se llevaba el lugar de la media de un decimal o de la
  # lectura de la muestra, que el total si anuncia.
  reservado <- function(clase) {
    sum(plan$n_consultas[plan$clase_consulta == clase])
  }
  reserva_modas <- reservado("moda exacta en el motor")
  reserva_decimales <- reservado(
    "media, mediana y desvio de los decimales en el decimal"
  )
  reserva_muestra <- reservado("verificacion de unicidad del orden") +
    reservado("muestra")
  fijar_reserva <- function() {
    if (!is.null(presupuesto)) {
      presupuesto$reserva <- reserva_modas + reserva_decimales + reserva_muestra
    }
  }
  fijar_reserva()
  if ("moda" %in% metricas && isTRUE(incluir_valores) &&
      is.null(recorrido$error) && isTRUE(fuente$disponible)) {
    for (campo in preparacion$campos) {
      decision <- decisiones_costo[[campo]]
      if (!is.null(decision) && identical(decision$moda, FALSE)) next
      tipo <- tipo_motor_de(campo)
      binaria <- .es_tipo_binario_muestra_dbi(
        tipo, .senas_conexion_dbi(conexion)
      )
      decimal <- .tipo_declarado_decimal_dbi(tipo)
      decimal_ancho <- decimal && .tipo_declarado_entero_ancho_dbi(tipo)
      posicion <- match(campo, preparacion$campos)
      cuantitativos <- recorrido$sobres[[
        .clave_par_identificador(campo, "cuantitativos", sep = "\u001f")
      ]]
      extremos <- if (!is.null(cuantitativos) &&
                      identical(cuantitativos$estado, "calculado")) {
        c(cuantitativos$resultado$minimo, cuantitativos$resultado$maximo)
      } else NULL
      entero_doble <- .entero_doble_ancho_moda_dbi(
        preparacion$prototipo[[posicion]], tipo, extremos
      )
      # Los decimales ya tienen todos sus valores en el mapa que se construye
      # durante el recorrido. Resolverlos aca con una consulta SQL separada
      # hacia que `bloque_filas` perdiera la moda antes que la via enviada
      # cuando el presupuesto era chico. La representacion de `.texto_moda()`
      # es la misma que la del perfil enviado para los decimales comunes; los
      # anchos quedan sujetos a la guarda de precision de la fila.
      # Un decimal ancho no puede reconstruir su modo desde el doble que
      # entrega el controlador; se pide la misma expresion exacta del motor
      # que usa la via SQL. Los decimales comunes siguen resolviendose con el
      # mapa de la lectura, sin una consulta adicional.
      if (!binaria && !decimal_ancho && !entero_doble) next
      # La moda de un binario o de un decimal ancho esta en el total: usa su
      # lugar reservado.
      if ((binaria || decimal_ancho) && reserva_modas > 0) {
        reserva_modas <- reserva_modas - 1
        fijar_reserva()
      }
      columna_moda_sql <- .expresion_binaria_moda_dbi(
        conexion, preparacion$campos_sql_proyeccion[[posicion]], tipo
      )
      valor_moda_sql <- .expresion_decimal_moda_dbi(
        conexion, columna_moda_sql, tipo, entero_doble = entero_doble
      )
      orden_moda_sql <- .orden_moda_dbi(
        conexion, valor_moda_sql, tipo,
        orden_numerico_sql = if (isTRUE(entero_doble)) {
          columna_moda_sql
        } else NULL
      )
      # Un decimal solo se resuelve en el motor si este lo devuelve como texto
      # exacto. Si el dialecto no tiene esa forma, la expresion es la misma
      # columna, el controlador la entregaria como doble y la guarda del decimal
      # tiene que seguir mandando sobre la moda.
      modas_bloques[[campo]] <- if (isTRUE(is.na(valor_moda_sql))) {
        list(
          ok = FALSE, datos = NULL,
          motivo = paste(
            "No se pudo pedir la moda exacta como texto en este motor;",
            "no se publica la moda del doble redondeado."
          ), sql = NA_character_
        )
      } else {
        .moda_columna_dbi(
          conexion, fuente$tabla_sql, columna_moda_sql,
          preparacion$dialecto, presupuesto = presupuesto,
          valor_sql = valor_moda_sql, orden_sql = orden_moda_sql
        )
      }
    }
  }
  decimales_bloques <- if (is.null(recorrido$error) &&
                           isTRUE(fuente$disponible)) {
    .media_mediana_decimal_bloques_dbi(
      conexion, preparacion, fuente, recorrido$sobres, metricas,
      incluir_valores, tipo_motor_de, presupuesto = presupuesto,
      reserva_media = reserva_decimales,
      n_filas = recorrido$bloques$filas_vistas %||% NA_real_
    )
  } else list(columnas = list(), consultas = 0L)
  # La muestra corre sin reserva, como en la via SQL.
  if (!is.null(presupuesto)) presupuesto$reserva <- 0
  consultas_segunda <- as.integer(segunda_pasada$consultas_sql %||% 0L)
  fetches_segunda <- as.integer(segunda_pasada$fetches %||% 0L)
  recorrido$bloques$consultas_sql <- as.integer(
    recorrido$bloques$consultas_sql %||% 0L
  ) + consultas_segunda + length(modas_bloques) +
    as.integer(decimales_bloques$consultas)
  recorrido$bloques$fetches <- as.integer(recorrido$bloques$fetches) + fetches_segunda
  # Las filas del plan quedan como las anuncio `plan_perfilado_dbi()`; lo que
  # se pago esta en `meta$consultas`. Antes se pisaban con las consultas de la
  # via por bloques sola, sin la preparacion ni la muestra, y el plan de la
  # corrida decia una cuenta que no era ni la anunciada ni la emitida.
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
        decisiones_costo = decisiones_costo,
        moda_precalculada = modas_bloques[[campo]],
        tipos_motor = preparacion$esquema$tipos_motor,
        prototipo_desconocido = isTRUE(
          preparacion$esquema$prototipo_desconocido
        ),
        decimal_exacto = decimales_bloques$columnas[[campo]]
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
  # Las columnas que el esquema no pudo leer -el motor las rechazo, o la sesion
  # no puede escribir su nombre- se declaran como en la via SQL: esta via las
  # dejaba fuera de `columnas`, de `sql` y de la cobertura, sin decirlo.
  recorrida <- isTRUE(fuente$disponible) && is.null(recorrido$error)
  for (campo in preparacion$esquema$ilegibles) {
    filas[[length(filas) + 1L]] <- if (recorrida) {
      .fila_vacia_bloques_dbi(campo, n_total)
    } else .fila_resumen_dbi(campo, NA_real_)
    sql[[length(sql) + 1L]] <- .registro_sql_dbi(
      campo, unlist(.CAMPOS_METRICA_DBI[metricas], use.names = FALSE),
      "no_disponible", preparacion$esquema$motivos[[campo]], NA_character_,
      metadatos = .metadatos_sql_dbi(),
      medicion = list(consulta_id = NA_integer_, etapa = "esquema")
    )
  }
  columnas <- if (length(filas)) {
    # Sin los nombres de las columnas, como en `medir()`: bajo `C`
    # una columna con acento avisaba "unable to translate".
    do.call(rbind, unname(lapply(filas, function(fila) {
      as.data.frame(.fila_magnitudes_dobles_dbi(fila), stringsAsFactors = FALSE)
    })))
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
  if (length(preparacion$esquema$ilegibles)) {
    cobertura <- rbind(cobertura, .cobertura_ilegibles_dbi(preparacion$esquema))
  }
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
        "orden_muestra_no_gobierna_fuente_bloques: la lectura por bloques prioriza la",
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
      emitidas = if (is.null(presupuesto)) {
        as.integer(recorrido$bloques$consultas_sql %||% 0L)
      } else presupuesto$usadas,
      fetches = as.integer(recorrido$bloques$fetches %||% 0L),
      presupuesto = if (is.null(presupuesto)) preparacion$max_consultas else
        presupuesto$max,
      agotado = !is.null(presupuesto) && isTRUE(presupuesto$agotado)
    ),
    orden_muestra = orden_muestra_meta,
    decisiones_costo = decisiones_costo,
    estimacion_derrame_moda = if (is.null(presupuesto)) NULL else
      presupuesto$estimacion_derrame_moda,
    estimacion_derrame_mediana = if (is.null(presupuesto)) NULL else
      presupuesto$estimacion_derrame_mediana,
    metodo = "dbfetch_bloques", snapshot = fuente$snapshot_id,
    # Los mismos dos campos que el agregado SQL, con lo que esta via hace: lee
    # las filas y compara en R. Sobre una columna con cotejamiento insensible a
    # caja, la misma tabla daba `n_distintos` 5 y moda "Montevideo" sin
    # `bloque_filas` y 9 y "Rivera" con el, y solo uno de los dos `resumen_tabla`
    # decia por que.
    criterio_moda = paste(
      "mayor frecuencia; empates resueltos por el valor ascendente: los",
      "n\u00fameros por su valor y el texto por sus bytes, como en `perfilar()`"
    ),
    criterio_comparacion = paste(
      "el recorrido por bloques lee las filas y compara en R, que distingue",
      "byte a byte, como el perfil de muestra; no usa el cotejamiento del",
      "motor. Sobre una columna con cotejamiento insensible a caja o acentos,",
      "`n_distintos` y la moda pueden diferir de los del resumen SQL sin",
      "`bloque_filas`, y no es un error de ninguno de los dos"
    ),
    clave = preparacion$catalogo_cardinalidad,
    metricas = metricas_publicas, metricas_ejecucion = metricas,
    incluir_valores = incluir_valores,
    solo_lectura = TRUE, objetos_temporales = FALSE,
    nota_inf = paste(
      "Los acumuladores calculan estad\u00edsticos sobre valores finitos; las",
      "banderas de NA, NaN e infinitos se conservan por separado. La posible",
      "divergencia de un resumen SQL que no pueda leer +/-Inf se debe a la",
      "representaci\u00f3n del controlador, no a una conversi\u00f3n silenciosa."
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
        "Resolver la capacidad incremental del driver antes de pedir diagn\u00f3sticos.",
        NA_character_
      )
    )
  } else {
    trazador <- .trazador_tiempos_dbi(preparacion$instrumentar)
    argumentos_muestra <- argumentos
    argumentos_muestra$antes_segunda_pasada <- NULL
    bloque_muestra <- .bloque_muestra_dbi(
      conexion, tabla, preparacion$tabla_sql, preparacion$campos,
      preparacion$campos_sql_proyeccion, preparacion$muestra,
      preparacion$muestra,
      if (length(fuente$orden_expresiones)) fuente$orden_expresiones else
        preparacion$orden_muestra,
      fuente$orden_expresiones, preparacion$dialecto, n_total,
      preparacion$presupuesto, .info_conexion_dbi(conexion),
      argumentos_muestra, muestreo = NULL, prototipo = preparacion$prototipo,
      tipos_declarados = preparacion$tipos,
      tipos_motor = preparacion$esquema$tipos_motor,
      trazador = trazador, max_celdas_muestra = max_celdas_muestra,
      max_bytes_muestra = max_bytes_muestra,
      enmascaradas = preparacion$enmascaradas
    )
    perfil_muestra <- bloque_muestra$perfil
    if (!is.null(perfil_muestra) && nrow(resumen$cobertura)) {
      conservar <- !(
        resumen$cobertura$bloque == "perfil_muestra" &
          grepl("muestra_diagnostica_acotada", resumen$cobertura$motivo,
                fixed = TRUE, useBytes = TRUE)
      )
      resumen$cobertura <- resumen$cobertura[conservar, , drop = FALSE]
      rownames(resumen$cobertura) <- NULL
    }
    resumen$cobertura <- rbind(resumen$cobertura, bloque_muestra$cobertura)
    resumen$tiempos <- .resumen_tiempos_dbi(trazador)
  }
  proteger <- .protege_argumentos(argumentos)
  if (proteger) {
    # El mismo piso por valor que la via sin bloques, con la muestra y, si no
    # cubre la tabla, con los valores de la tabla entera: ver
    # `.proteger_salida_muestra_dbi()`. Aca corria solo con los extremos de las
    # columnas protegidas, y una cedula copiada en una columna de codigos salia
    # en `columnas$maximo` y en `meta$familias`.
    protegido <- .proteger_salida_muestra_dbi(
      resumen,
      if (is.null(perfil_muestra)) list(perfil = NULL) else bloque_muestra,
      conexion, preparacion, preparacion$presupuesto
    )
    resumen <- protegido$resumen
    perfil_muestra <- protegido$perfil
  } else {
    resumen$meta$proteccion_personal <- list(
      aplicada = FALSE, base = "desactivada por el usuario", columnas = character()
    )
  }
  # Lo emitido se lee al final: la lectura de `perfil_muestra` y la proteccion
  # tambien consultan.
  if (!is.null(presupuesto)) {
    resumen$meta$consultas$emitidas <- presupuesto$usadas
    resumen$meta$consultas$agotado <- isTRUE(presupuesto$agotado)
  }
  salida <- list(resumen_tabla = resumen, perfil_muestra = perfil_muestra)
  salida <- .sellar_perfil_dbi(salida)
  salida
}

# ---- La media y la mediana de un decimal, en el decimal --------------------
#
# Por `bloque_filas` y por el spool de la muestra, R recibe el doble de cada
# decimal, y la media y la mediana de esos dobles no son las del decimal: la
# mediana de 0,0001 y 0,0002 salia 0,00015000000000000001 y es 0,00015. Medido
# en los siete motores sobre columnas de dos a nueve valores: a un ulp del doble
# mas cercano en 9 de 112 medianas y 14 de 112 medias de SQL Server por bloques,
# y en 2 de 46 y 4 de 46 de PostgreSQL, MySQL y MariaDB; lo mismo por el spool,
# y en DuckDB. El decimal exacto llega como texto -la suma que calcula el motor,
# o el valor que la muestra ya trae como texto- y lo que se publica es el doble
# mas cercano de ese texto dividido por la cantidad, con empate a par.
#
# Sin una biblioteca de enteros grandes: un entero no negativo se guarda en
# cifras de base 1e7, la menos significativa primero, y cada operacion queda
# por debajo de 2^53, donde la aritmetica de los dobles es exacta.
.BASE_ENTERO_GRANDE_DBI <- 1e7

# Cociente y resto por la base de un entero `v` entre 0 y 2^53. `floor(v /
# base)` puede redondear hacia arriba cuando `v` esta justo debajo de un
# multiplo -9e15 - 1-, y el resto saldria negativo: se corrige con el resto,
# que es exacto.
.dividir_base_entero_grande_dbi <- function(v) {
  base <- .BASE_ENTERO_GRANDE_DBI
  cociente <- floor(v / base)
  resto <- v - cociente * base
  if (resto < 0) {
    cociente <- cociente - 1
    resto <- resto + base
  } else if (resto >= base) {
    cociente <- cociente + 1
    resto <- resto - base
  }
  c(cociente, resto)
}

.normalizar_entero_grande_dbi <- function(x) {
  acarreo <- 0
  for (j in seq_along(x)) {
    partes <- .dividir_base_entero_grande_dbi(x[[j]] + acarreo)
    acarreo <- partes[[1L]]
    x[[j]] <- partes[[2L]]
  }
  while (acarreo > 0) {
    partes <- .dividir_base_entero_grande_dbi(acarreo)
    x <- c(x, partes[[2L]])
    acarreo <- partes[[1L]]
  }
  while (length(x) > 1L && x[[length(x)]] == 0) x <- x[-length(x)]
  if (!length(x)) 0 else x
}

# Un doble entero entre 0 y 2^53, en cifras.
.entero_grande_desde_doble_dbi <- function(x) {
  .normalizar_entero_grande_dbi(x)
}

.comparar_entero_grande_dbi <- function(a, b) {
  if (length(a) != length(b)) return(sign(length(a) - length(b)))
  for (j in rev(seq_along(a))) {
    if (a[[j]] != b[[j]]) return(sign(a[[j]] - b[[j]]))
  }
  0
}

.sumar_entero_grande_dbi <- function(a, b) {
  largo <- max(length(a), length(b))
  .normalizar_entero_grande_dbi(
    c(a, rep(0, largo - length(a))) + c(b, rep(0, largo - length(b)))
  )
}

# `a - b` con `a >= b`.
.restar_entero_grande_dbi <- function(a, b) {
  base <- .BASE_ENTERO_GRANDE_DBI
  d <- a - c(b, rep(0, length(a) - length(b)))
  for (j in seq_along(d)) {
    if (d[[j]] < 0) {
      d[[j]] <- d[[j]] + base
      d[[j + 1L]] <- d[[j + 1L]] - 1
    }
  }
  .normalizar_entero_grande_dbi(d)
}

# Por un entero `k` menor que 2^26: cada producto queda debajo de 2^53.
.por_chico_entero_grande_dbi <- function(a, k) {
  .normalizar_entero_grande_dbi(a * k)
}

.por_potencia_dos_entero_grande_dbi <- function(a, e) {
  while (e >= 25) {
    a <- .por_chico_entero_grande_dbi(a, 2^25)
    e <- e - 25
  }
  if (e > 0) a <- .por_chico_entero_grande_dbi(a, 2^e)
  a
}

.por_potencia_diez_entero_grande_dbi <- function(a, s) {
  if (s <= 0 || identical(a, 0)) return(a)
  a <- .por_chico_entero_grande_dbi(a, 10^(s %% 7))
  if (s %/% 7 > 0) a <- c(rep(0, s %/% 7), a)
  a
}

# Por un entero `n` menor que 2^53, en dos mitades de 26 bits.
.por_entero_entero_grande_dbi <- function(a, n) {
  alto <- floor(n / 2^26)
  bajo <- n - alto * 2^26
  producto <- .por_chico_entero_grande_dbi(a, bajo)
  if (alto > 0) {
    producto <- .sumar_entero_grande_dbi(
      producto,
      .por_potencia_dos_entero_grande_dbi(
        .por_chico_entero_grande_dbi(a, alto), 26
      )
    )
  }
  producto
}

# Los decimales escritos como texto -lo que escriben `CAST(... AS TEXT)`,
# `CAST(... AS CHAR)` y `CONVERT(VARCHAR, ...)` de un decimal, sin exponente-,
# en la escala comun -la mayor- y partidos en grupos de siete cifras: una
# matriz de dobles enteros, el grupo mas significativo primero, y el signo. Un
# cero con signo -`-0.00`- es cero. `NULL` si algun texto no es un decimal. Con
# operaciones vectoriales: un millon de textos en menos de un segundo.
.grupos_decimales_texto_dbi <- function(texto) {
  texto <- as.character(texto)
  if (!length(texto) || anyNA(texto) ||
      any(grepl("[^0-9.+-]", texto, perl = TRUE))) {
    return(NULL)
  }
  primero <- substr(texto, 1L, 1L)
  negativo <- primero == "-"
  con_signo <- negativo | primero == "+"
  cuerpo <- texto
  cuerpo[con_signo] <- substr(
    texto[con_signo], 2L, nchar(texto[con_signo], type = "bytes")
  )
  if (any(grepl("[+-]", cuerpo, perl = TRUE))) return(NULL)
  punto <- regexpr(".", cuerpo, fixed = TRUE, useBytes = TRUE)
  fraccion <- (nchar(cuerpo, type = "bytes") - punto) * (punto > 0)
  digitos <- sub(".", "", cuerpo, fixed = TRUE, useBytes = TRUE)
  if (any(!nzchar(digitos)) ||
      any(grepl(".", digitos, fixed = TRUE, useBytes = TRUE))) {
    return(NULL)
  }
  escala <- max(fraccion)
  faltan <- escala - fraccion
  if (any(faltan > 0)) {
    digitos[faltan > 0] <- paste0(
      digitos[faltan > 0], strrep("0", faltan[faltan > 0])
    )
  }
  ancho <- nchar(digitos, type = "bytes")
  n_grupos <- ceiling(max(ancho) / 7)
  relleno <- n_grupos * 7 - ancho
  if (any(relleno > 0)) {
    digitos[relleno > 0] <- paste0(
      strrep("0", relleno[relleno > 0]), digitos[relleno > 0]
    )
  }
  grupos <- matrix(0, nrow = length(texto), ncol = n_grupos)
  for (j in seq_len(n_grupos)) {
    grupos[, j] <- as.numeric(substr(digitos, j * 7L - 6L, j * 7L))
  }
  list(
    negativo = negativo & rowSums(grupos) > 0, grupos = grupos,
    escala = escala
  )
}

# La suma exacta de las filas `filas` de unos grupos: `list(negativo, cifras,
# escala)` con las cifras en base 1e7, la menos significativa primero. Cada
# grupo se suma como doble: exacto mientras haya menos de 9e8 sumandos.
.sumar_grupos_decimales_dbi <- function(grupos, filas = NULL) {
  if (is.null(grupos)) return(NULL)
  matriz <- grupos$grupos
  negativo <- grupos$negativo
  if (!is.null(filas)) {
    matriz <- matriz[filas, , drop = FALSE]
    negativo <- negativo[filas]
  }
  if (nrow(matriz) > 9e8) return(NULL)
  sumar <- function(cuales) {
    .normalizar_entero_grande_dbi(
      rev(colSums(matriz[cuales, , drop = FALSE]))
    )
  }
  positivos <- sumar(!negativo)
  negativos <- sumar(negativo)
  signo <- .comparar_entero_grande_dbi(positivos, negativos)
  list(
    negativo = signo < 0,
    cifras = if (signo >= 0) {
      .restar_entero_grande_dbi(positivos, negativos)
    } else .restar_entero_grande_dbi(negativos, positivos),
    escala = grupos$escala
  )
}

# `a + b` de dos decimales con signo en la misma escala, como los devuelve
# `.sumar_grupos_decimales_dbi()`.
.sumar_con_signo_decimal_dbi <- function(a, b) {
  if (identical(a$negativo, b$negativo)) {
    return(list(
      negativo = a$negativo,
      cifras = .sumar_entero_grande_dbi(a$cifras, b$cifras), escala = a$escala
    ))
  }
  orden <- .comparar_entero_grande_dbi(a$cifras, b$cifras)
  if (orden >= 0) {
    list(negativo = a$negativo && orden > 0,
         cifras = .restar_entero_grande_dbi(a$cifras, b$cifras),
         escala = a$escala)
  } else {
    list(negativo = b$negativo,
         cifras = .restar_entero_grande_dbi(b$cifras, a$cifras),
         escala = a$escala)
  }
}

# La suma exacta de decimales escritos como texto. En la escala comun `s` -la
# mayor-, cada decimal es un entero `k`, y si `k` queda debajo de 2^49 sale
# exacto de su doble -el de `dobles`, o la conversion de R-: `round(d * 10^s)`,
# porque el doble dista del decimal unos pocos ulps y `10^s` es exacto hasta
# `s = 22`. Esos enteros se suman en tres grupos de siete cifras sin tocar los
# textos: es casi todo lo que guarda un motor, y un millon de valores cuesta
# unas decimas. El resto, por sus cifras (`.grupos_decimales_texto_dbi()`).
# `NULL` si algun texto no es un decimal.
.sumar_decimales_texto_dbi <- function(texto, dobles = NULL) {
  texto <- as.character(texto)
  if (!length(texto) || anyNA(texto) || length(texto) > 9e8 ||
      any(grepl("[^0-9.+-]", texto, perl = TRUE))) {
    return(NULL)
  }
  dobles <- if (is.null(dobles)) {
    suppressWarnings(as.numeric(texto))
  } else as.numeric(dobles)
  if (length(dobles) != length(texto)) return(NULL)
  punto <- regexpr(".", texto, fixed = TRUE, useBytes = TRUE)
  escala <- max((nchar(texto, type = "bytes") - punto) * (punto > 0))
  valor <- if (escala <= 22) round(abs(dobles) * 10^escala) else NA_real_
  rapido <- is.finite(dobles) & !is.na(valor) & valor < 2^49
  total <- list(negativo = FALSE, cifras = 0, escala = escala)
  if (any(rapido)) {
    a <- valor[rapido]
    alto <- floor(a / 1e14)
    a <- a - alto * 1e14
    medio <- floor(a / 1e7)
    bajo <- a - medio * 1e7
    corregir <- bajo < 0
    medio[corregir] <- medio[corregir] - 1
    bajo[corregir] <- bajo[corregir] + 1e7
    corregir <- bajo >= 1e7
    medio[corregir] <- medio[corregir] + 1
    bajo[corregir] <- bajo[corregir] - 1e7
    signo <- dobles[rapido] < 0
    for (lado in c(FALSE, TRUE)) {
      cuales <- signo == lado
      if (!any(cuales)) next
      total <- .sumar_con_signo_decimal_dbi(total, list(
        negativo = lado,
        cifras = .normalizar_entero_grande_dbi(c(
          sum(bajo[cuales]), sum(medio[cuales]), sum(alto[cuales])
        )),
        escala = escala
      ))
    }
  }
  if (any(!rapido)) {
    resto <- .sumar_grupos_decimales_dbi(
      .grupos_decimales_texto_dbi(texto[!rapido])
    )
    if (is.null(resto)) return(NULL)
    resto$cifras <- .por_potencia_diez_entero_grande_dbi(
      resto$cifras, escala - resto$escala
    )
    resto$escala <- escala
    total <- .sumar_con_signo_decimal_dbi(total, resto)
  }
  if (all(total$cifras == 0)) total$negativo <- FALSE
  total
}

# El orden exacto de unos grupos: por cada grupo de siete cifras, con el signo
# del valor, de la mas significativa a la menos -en los negativos, la de mayor
# valor absoluto queda primero-.
.orden_grupos_decimales_dbi <- function(grupos) {
  signo <- ifelse(grupos$negativo, -1, 1)
  claves <- lapply(seq_len(ncol(grupos$grupos)), function(j) {
    signo * grupos$grupos[, j]
  })
  do.call(.orden_por_bytes, claves)
}

# El doble mas cercano de `decimal / divisor`, con empate a par: `decimal` es lo
# que devuelve `.sumar_decimales_texto_dbi()` y `divisor`, un entero positivo
# menor que 2^53. Un candidato -el texto convertido por R y dividido- se acepta
# cuando el valor exacto queda entre los puntos medios con sus vecinos, que son
# racionales diadicos y se comparan como enteros: `M * 10^-s / n` contra
# `B * 2^E` es `M * 2^-E` contra `n * B * 10^s`. Si no, se pasa al vecino. `NA`
# fuera de los dobles normales, donde no hace falta: ningun decimal de los
# motores llega ahi.
.doble_mas_cercano_decimal_dbi <- function(decimal, divisor = 1) {
  if (is.null(decimal) || length(divisor) != 1L || !is.finite(divisor) ||
      divisor < 1 || divisor != floor(divisor) || divisor >= 2^53) {
    return(NA_real_)
  }
  cifras <- decimal$cifras
  escala <- decimal$escala
  if (all(cifras == 0)) return(0)
  arriba <- length(cifras)
  texto <- paste0(
    sprintf("%.0f", cifras[[arriba]]),
    paste(sprintf("%07.0f", rev(cifras[-arriba])), collapse = "")
  )
  aproximado <- suppressWarnings(
    as.numeric(paste0(texto, "e-", escala)) / divisor
  )
  if (length(aproximado) != 1L || !is.finite(aproximado) ||
      aproximado < 2^-1000 || aproximado > 2^1000) {
    return(NA_real_)
  }
  # El valor exacto contra `B * 2^E`, con `B` entero grande.
  comparar <- function(b, e) {
    izquierda <- .por_potencia_dos_entero_grande_dbi(cifras, max(0, -e))
    derecha <- .por_potencia_dos_entero_grande_dbi(
      .por_potencia_diez_entero_grande_dbi(
        .por_entero_entero_grande_dbi(b, divisor), escala
      ),
      max(0, e)
    )
    .comparar_entero_grande_dbi(izquierda, derecha)
  }
  uno <- 1
  y <- aproximado
  for (intento in seq_len(8L)) {
    e <- floor(log2(y)) - 52
    m <- y / 2^e
    if (m >= 2^53) {
      e <- e + 1
      m <- y / 2^e
    } else if (m < 2^52) {
      e <- e - 1
      m <- y / 2^e
    }
    entero_m <- .entero_grande_desde_doble_dbi(m)
    potencia <- m == 2^52
    medio_arriba <- comparar(
      .sumar_entero_grande_dbi(.por_chico_entero_grande_dbi(entero_m, 2), uno),
      e - 1
    )
    if (medio_arriba > 0) {
      y <- y + 2^e
      next
    }
    # Debajo de una potencia de dos el vecino esta a medio paso.
    medio_abajo <- if (potencia) {
      comparar(
        .restar_entero_grande_dbi(
          .por_chico_entero_grande_dbi(entero_m, 4), uno
        ),
        e - 2
      )
    } else {
      comparar(
        .restar_entero_grande_dbi(
          .por_chico_entero_grande_dbi(entero_m, 2), uno
        ),
        e - 1
      )
    }
    if (medio_abajo < 0) {
      y <- y - if (potencia) 2^(e - 1) else 2^e
      next
    }
    par <- m %% 2 == 0
    if (medio_arriba == 0 && !par) y <- y + 2^e
    if (medio_abajo == 0 && !par && !potencia) y <- y - 2^e
    return(if (isTRUE(decimal$negativo)) -y else y)
  }
  NA_real_
}

# `a * b`, enteros grandes. Cada producto de cifras queda debajo de 1e14 y cada
# posicion suma a lo sumo 80 de ellos antes de llevar: debajo de 2^53.
.multiplicar_entero_grande_dbi <- function(a, b) {
  if (all(a == 0) || all(b == 0)) return(0)
  if (length(a) < length(b)) {
    c_ <- a
    a <- b
    b <- c_
  }
  salida <- numeric(length(a) + length(b) + 1L)
  for (inicio in seq(1L, length(b), by = 80L)) {
    for (j in inicio:min(length(b), inicio + 79L)) {
      if (b[[j]] == 0) next
      posiciones <- j + seq_along(a) - 1L
      salida[posiciones] <- salida[posiciones] + a * b[[j]]
    }
    acarreo <- 0
    for (k in seq_along(salida)) {
      partes <- .dividir_base_entero_grande_dbi(salida[[k]] + acarreo)
      acarreo <- partes[[1L]]
      salida[[k]] <- partes[[2L]]
    }
  }
  .normalizar_entero_grande_dbi(salida)
}

# Un doble aproximado de un entero grande, como mantisa y exponente decimal,
# para no desbordar con mas de 308 cifras.
.aproximar_entero_grande_dbi <- function(a) {
  arriba <- length(a)
  tomar <- max(1L, arriba - 3L):arriba
  mantisa <- sum(a[tomar] * .BASE_ENTERO_GRANDE_DBI^(tomar - min(tomar)))
  list(mantisa = mantisa, exponente = 7 * (min(tomar) - 1L))
}

# El doble mas cercano de `sqrt(N / D)`, con `N >= 0` y `D > 0` enteros
# grandes, con empate a par: un candidato se acepta cuando `N / D` queda entre
# los cuadrados de los puntos medios con sus vecinos, que se comparan como
# enteros (`B^2 * 2^(2E) * D` contra `N`). `NA` fuera de los dobles normales.
.raiz_mas_cercana_cociente_dbi <- function(numerador, denominador) {
  if (all(numerador == 0)) return(0)
  if (all(denominador == 0)) return(NA_real_)
  an <- .aproximar_entero_grande_dbi(numerador)
  ad <- .aproximar_entero_grande_dbi(denominador)
  # Un candidato a pocos ulps: la raiz del cociente de las mantisas, con la
  # potencia de diez partida en una par.
  diferencia <- an$exponente - ad$exponente
  cociente <- an$mantisa / ad$mantisa
  if (diferencia %% 2 != 0) {
    cociente <- cociente * 10
    diferencia <- diferencia - 1
  }
  y <- sqrt(cociente) * 10^(diferencia / 2)
  if (!is.finite(y) || y < 2^-500 || y > 2^500) return(NA_real_)
  # El signo de `(B * 2^E)^2 - N / D`.
  frente <- function(b, e) {
    cuadrado <- .multiplicar_entero_grande_dbi(
      .multiplicar_entero_grande_dbi(b, b), denominador
    )
    izquierda <- .por_potencia_dos_entero_grande_dbi(cuadrado, max(0, 2 * e))
    derecha <- .por_potencia_dos_entero_grande_dbi(numerador, max(0, -2 * e))
    .comparar_entero_grande_dbi(izquierda, derecha)
  }
  uno <- 1
  for (intento in seq_len(16L)) {
    e <- floor(log2(y)) - 52
    m <- y / 2^e
    if (m >= 2^53) {
      e <- e + 1
      m <- y / 2^e
    } else if (m < 2^52) {
      e <- e - 1
      m <- y / 2^e
    }
    entero_m <- .entero_grande_desde_doble_dbi(m)
    potencia <- m == 2^52
    arriba <- frente(
      .sumar_entero_grande_dbi(.por_chico_entero_grande_dbi(entero_m, 2), uno),
      e - 1
    )
    if (arriba < 0) {
      y <- y + 2^e
      next
    }
    abajo <- if (potencia) {
      frente(
        .restar_entero_grande_dbi(
          .por_chico_entero_grande_dbi(entero_m, 4), uno
        ),
        e - 2
      )
    } else {
      frente(
        .restar_entero_grande_dbi(
          .por_chico_entero_grande_dbi(entero_m, 2), uno
        ),
        e - 1
      )
    }
    if (abajo > 0) {
      y <- y - if (potencia) 2^(e - 1) else 2^e
      next
    }
    par <- m %% 2 == 0
    if (arriba == 0 && !par) y <- y + 2^e
    if (abajo == 0 && !par && !potencia) y <- y - 2^e
    return(y)
  }
  NA_real_
}

# Un decimal exacto -`list(negativo, cifras, escala)`- llevado a otra escala
# mayor o igual.
.a_escala_decimal_dbi <- function(decimal, escala) {
  decimal$cifras <- .por_potencia_diez_entero_grande_dbi(
    decimal$cifras, escala - decimal$escala
  )
  decimal$escala <- escala
  decimal
}

# Un decimal exacto desde su texto (`NULL` si no lo es).
.decimal_desde_texto_dbi <- function(texto) {
  if (length(texto) != 1L || is.na(texto)) return(NULL)
  .sumar_decimales_texto_dbi(as.character(texto))
}

# El desvio muestral exacto, el doble mas cercano de `sqrt(V)`, de la suma de
# las diferencias con un centro `A` y la de sus cuadrados `B`, exactas (como las
# devuelven `.sumar_decimales_texto_dbi()` o la suma del motor):
# `V = (n * B - A^2) / (n * (n - 1))`. Sin cancelacion: es aritmetica entera.
.desvio_exacto_sumas_dbi <- function(n, suma, cuadrados) {
  if (is.null(suma) || is.null(cuadrados) || length(n) != 1L ||
      !is.finite(n) || n < 2 || n >= 2^53 || n != floor(n) ||
      isTRUE(cuadrados$negativo)) {
    return(NA_real_)
  }
  escala <- max(cuadrados$escala, 2 * suma$escala)
  cuadrados <- .a_escala_decimal_dbi(cuadrados, escala)
  suma2 <- .por_potencia_diez_entero_grande_dbi(
    .multiplicar_entero_grande_dbi(suma$cifras, suma$cifras),
    escala - 2 * suma$escala
  )
  entero_n <- .entero_grande_desde_doble_dbi(n)
  primero <- .por_entero_entero_grande_dbi(cuadrados$cifras, n)
  if (.comparar_entero_grande_dbi(primero, suma2) < 0) return(NA_real_)
  numerador <- .restar_entero_grande_dbi(primero, suma2)
  denominador <- .por_potencia_diez_entero_grande_dbi(
    .por_entero_entero_grande_dbi(entero_n, n - 1), escala
  )
  .raiz_mas_cercana_cociente_dbi(numerador, denominador)
}

# Las sumas exactas de unos enteros no negativos y de sus cuadrados, con signo
# para la primera: `cifras` es una matriz de cifras en base 1e7, la menos
# significativa primero, y `negativo` el signo de cada fila. Cada cifra se parte
# en `r` (menos de 1e4) y `h` (menos de 1e3), en las posiciones `10^(7j)` y
# `10^(7j + 4)`: los productos quedan debajo de 1e8 y sus sumas, con menos de
# 9e7 filas, debajo de 2^53.
.sumas_enteras_y_cuadrados_dbi <- function(cifras, negativo) {
  n <- nrow(cifras)
  if (n > 9e7) return(NULL)
  terminos <- list()
  exponentes <- numeric()
  for (j in seq_len(ncol(cifras))) {
    alto <- floor(cifras[, j] / 1e4)
    terminos[[length(terminos) + 1L]] <- cifras[, j] - alto * 1e4
    exponentes <- c(exponentes, 7 * (j - 1L))
    terminos[[length(terminos) + 1L]] <- alto
    exponentes <- c(exponentes, 7 * (j - 1L) + 4)
  }
  sumar_potencias <- function(valores, exponente) {
    .por_potencia_diez_entero_grande_dbi(
      .entero_grande_desde_doble_dbi(valores), exponente
    )
  }
  positivos <- 0
  negativos <- 0
  for (t in seq_along(terminos)) {
    positivos <- .sumar_entero_grande_dbi(
      positivos, sumar_potencias(sum(terminos[[t]][!negativo]), exponentes[[t]])
    )
    negativos <- .sumar_entero_grande_dbi(
      negativos, sumar_potencias(sum(terminos[[t]][negativo]), exponentes[[t]])
    )
  }
  signo <- .comparar_entero_grande_dbi(positivos, negativos)
  cuadrados <- 0
  for (t in seq_along(terminos)) {
    for (u in t:length(terminos)) {
      producto <- sum(terminos[[t]] * terminos[[u]])
      if (producto == 0) next
      termino <- sumar_potencias(producto, exponentes[[t]] + exponentes[[u]])
      if (u > t) termino <- .por_chico_entero_grande_dbi(termino, 2)
      cuadrados <- .sumar_entero_grande_dbi(cuadrados, termino)
    }
  }
  list(
    suma = list(
      negativo = signo < 0,
      cifras = if (signo >= 0) {
        .restar_entero_grande_dbi(positivos, negativos)
      } else .restar_entero_grande_dbi(negativos, positivos)
    ),
    cuadrados = cuadrados
  )
}

# El desvio exacto de un decimal escrito como texto, el doble mas cercano. Las
# diferencias con una de sus centrales (`centro`, una posicion de `texto`) se
# toman exactas -como enteros de menos de 2^49 en la escala comun, o por sus
# cifras con los prestamos llevados- y se suman, ellas y sus cuadrados, sin
# redondeo (`.sumas_enteras_y_cuadrados_dbi()`).
.desvio_decimal_texto_dbi <- function(texto, dobles, centro) {
  n <- length(texto)
  if (n < 2L || length(dobles) != n || length(centro) != 1L ||
      is.na(centro) || centro < 1L || centro > n) {
    return(NA_real_)
  }
  punto <- regexpr(".", texto, fixed = TRUE, useBytes = TRUE)
  escala <- max((nchar(texto, type = "bytes") - punto) * (punto > 0))
  k <- if (escala <= 22) round(dobles * 10^escala) else NA_real_
  base <- .BASE_ENTERO_GRANDE_DBI
  if (!anyNA(k) && all(abs(k) < 2^49)) {
    delta <- k - k[[centro]]
    negativo <- delta < 0
    a <- abs(delta)
    alto <- floor(a / 1e14)
    resto <- a - alto * 1e14
    medio <- floor(resto / base)
    bajo <- resto - medio * base
    corregir <- bajo < 0
    medio[corregir] <- medio[corregir] - 1
    bajo[corregir] <- bajo[corregir] + base
    corregir <- bajo >= base
    medio[corregir] <- medio[corregir] + 1
    bajo[corregir] <- bajo[corregir] - base
    cifras <- cbind(bajo, medio, alto)
  } else {
    grupos <- .grupos_decimales_texto_dbi(texto)
    if (is.null(grupos)) return(NA_real_)
    d <- ifelse(grupos$negativo, -1, 1) * grupos$grupos
    d <- d - matrix(d[centro, ], nrow = n, ncol = ncol(d), byrow = TRUE)
    signo <- numeric(n)
    for (j in seq_len(ncol(d))) {
      pendiente <- signo == 0
      signo[pendiente] <- sign(d[pendiente, j])
    }
    d <- d * signo
    acarreo <- numeric(n)
    for (j in rev(seq_len(ncol(d)))) {
      valor <- d[, j] + acarreo
      acarreo <- floor(valor / base)
      d[, j] <- valor - acarreo * base
    }
    # La resta de dos valores de distinto signo puede tener un grupo mas.
    cifras <- cbind(d[, rev(seq_len(ncol(d))), drop = FALSE], acarreo)
    negativo <- signo < 0
  }
  sumas <- .sumas_enteras_y_cuadrados_dbi(cifras, negativo)
  if (is.null(sumas)) return(NA_real_)
  .desvio_exacto_sumas_dbi(
    n, c(sumas$suma, list(escala = escala)),
    list(negativo = FALSE, cifras = sumas$cuadrados, escala = 2 * escala)
  )
}

# La media, la mediana y el desvio exactos de un decimal escrito como texto -lo
# que la muestra del motor trae para un decimal que el controlador entregaria
# como doble-: el doble mas cercano de la suma exacta dividida por la cantidad,
# y de la suma de sus una o dos centrales, en el orden exacto de los decimales;
# el desvio, centrado en la primera central (`.desvio_decimal_texto_dbi()`).
# El orden exacto solo hace falta cerca de las centrales: los dobles de los
# textos -`dobles`, o su conversion- ordenan bien todo lo que dista de ellas mas
# que su error, que es de unos ulps y aca se toma de 2^-40 relativo; lo que
# queda en esa ventana se ordena por sus cifras. `NA` donde no se puede (un
# texto que no es un decimal, ningun valor).
.media_mediana_decimal_texto_dbi <- function(texto, dobles = NULL) {
  texto <- as.character(texto)
  presentes <- !is.na(texto)
  dobles <- if (is.null(dobles)) {
    suppressWarnings(as.numeric(texto[presentes]))
  } else as.numeric(dobles)[presentes]
  texto <- texto[presentes]
  vacio <- list(media = NA_real_, mediana = NA_real_, desvio = NA_real_)
  if (!length(texto) || length(dobles) != length(texto) ||
      any(!is.finite(dobles))) {
    return(vacio)
  }
  n <- length(texto)
  media <- .doble_mas_cercano_decimal_dbi(
    .sumar_decimales_texto_dbi(texto, dobles), n
  )
  posiciones <- unique(c((n + 1L) %/% 2L, (n + 2L) %/% 2L))
  q <- dobles[.orden_por_bytes(dobles)[posiciones]]
  bajo <- min(q) - abs(min(q)) * 2^-40
  alto <- max(q) + abs(max(q)) * 2^-40
  ventana <- which(dobles >= bajo & dobles <= alto)
  rangos <- posiciones - sum(dobles < bajo)
  grupos <- .grupos_decimales_texto_dbi(texto[ventana])
  if (is.null(grupos) || any(rangos < 1L | rangos > length(ventana))) {
    return(list(media = media, mediana = NA_real_, desvio = NA_real_))
  }
  centrales <- .orden_grupos_decimales_dbi(grupos)[rangos]
  list(
    media = media,
    mediana = .doble_mas_cercano_decimal_dbi(
      .sumar_grupos_decimales_dbi(grupos, centrales), length(centrales)
    ),
    desvio = .desvio_decimal_texto_dbi(
      texto, dobles, ventana[[centrales[[1L]]]]
    )
  )
}

# ---- Sumas exactas de los decimales en el motor ----------------------------
#
# La media y el desvio de un decimal salen de dos sumas que el motor hace sin
# redondeo y devuelve como texto: la de la columna y la de los cuadrados de sus
# diferencias con un centro. Cada expresion se elige para que no pueda
# desbordar ni redondear con las cifras que puede tener -la precision del tipo,
# las cifras enteras de los extremos que ya se leyeron, las filas-: un error
# del motor tumba la consulta entera, y un redondeo callado da un desvio falso.
# Lo medido en cada motor:
#
# * PostgreSQL: `numeric` es exacto con cualquier cantidad de cifras. No hace
#   falta centro: se suman `x` y `x * x`. `NaN` e `Infinity` (PostgreSQL 14 o
#   mas) quedan fuera, como en la media de la tabla completa: `x - x = 0` solo
#   vale para un numero finito, en cualquier version.
# * MySQL y MariaDB: la suma de un `DECIMAL` es exacta aun con 66 cifras, pero el
#   producto de dos tiene la suma de sus escalas solo hasta 30 -el cuadrado de un
#   `DECIMAL(38,30)` redondea a 30 decimales sin avisar- y falla pasadas las 65
#   cifras.
# * SQL Server: la suma de un `decimal(p, s)` es un `decimal(38, s)`, y desborda
#   con error -tres valores de 9e7 en un `decimal(38,30)`-; se suman aparte la
#   parte entera, truncada, en `decimal(38,0)`, y la fraccion, `x % 1` (medido:
#   exacto, con compatibilidad 160 y 100). El producto de dos `decimal(q, s)` es
#   exacto solo si `2q + 1 <= 38`: el de dos `decimal(38,30)` redondea a 21
#   decimales sin avisar. `money` se lee como `decimal(19,4)`: `money % 1`
#   pierde los dos ultimos decimales (medido: 0,69 por 0,6911).
# * DuckDB: la suma de un `DECIMAL` es un `DECIMAL(38, s)` y desborda pasadas
#   las 38 cifras ("Overflow in HUGEINT addition"); se parte como en SQL Server.
#   El producto de dos `DECIMAL` de hasta 18 cifras se hace en 64 bits y
#   desborda aunque el resultado tenga 30 ("Overflow in multiplication of
#   DECIMAL(18)", con dos `DECIMAL(15,0)`); con un factor de 19 cifras es un
#   `DECIMAL(38, s)`, en 128 bits.
#
# Donde la expresion directa no entra, el valor absoluto -o la diferencia con el
# centro- se escribe como texto sin el punto, con ceros a la izquierda, y se
# parte en trozos de nueve cifras: la suma es la de los trozos con su signo, en
# su posicion, y la de los cuadrados, la de los productos de cada par de
# trozos, enteros de 18 cifras que ningun motor redondea
# (`.trozos_decimal_dbi()`).

# Las cifras enteras del mayor valor absoluto de `x`, que llega como doble: con
# un margen de una parte en mil millones, porque el doble de un extremo dista
# del decimal unos ulps. `NA` sin ningun valor finito.
.cifras_enteras_dbi <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mayor <- max(abs(x)) * (1 + 1e-9)
  if (mayor < 1) 0 else floor(log10(mayor)) + 1
}

# La precision que declara el tipo: `decimal(18,2)` es 18, y `money` y
# `smallmoney` de SQL Server, que se leen como `decimal(19,4)` y
# `decimal(10,4)`, 19 y 10. `NA` si no la declara.
.precision_declarada_dbi <- function(tipo) {
  if (is.null(tipo) || length(tipo) != 1L || is.na(tipo)) return(NA_integer_)
  limpio <- gsub("[[:space:]]+", "", .minusculas_ascii(as.character(tipo)))
  if (identical(limpio, "money")) return(19L)
  if (identical(limpio, "smallmoney")) return(10L)
  partes <- regmatches(limpio, regexec(
    "^(?:decimal|numeric|number)\\(([0-9]+)(?:,([0-9]+))?\\)(?:unsigned)?$",
    limpio, perl = TRUE
  ))[[1L]]
  if (length(partes) != 3L) return(NA_integer_)
  as.integer(partes[[2L]])
}

# La escala con la que el motor suma la columna: la declarada, y 4 para `money`
# y `smallmoney` de SQL Server.
.escala_suma_decimal_dbi <- function(senas, tipo) {
  if (.es_transact_sql_dbi(senas) && .moneda_sqlserver_dbi(tipo)) return(4L)
  .escala_declarada_dbi(tipo)
}

# Si el tipo de una columna admite las sumas exactas: un decimal, con escala
# fuera de PostgreSQL, que no la necesita.
.sumas_exactas_posibles_dbi <- function(senas, tipo) {
  .mediana_decimal_por_centrales_dbi(senas, tipo) &&
    (.postgresql_dbi(senas) || !is.na(.escala_suma_decimal_dbi(senas, tipo)))
}

# La precision con la que se suma: la declarada, y la de `money` -19- cuando el
# controlador da solo el codigo ODBC (`3`, ver `.moneda_sqlserver_dbi()`), que
# no dice si es `money` o `smallmoney`: 19 alcanza para los dos.
.precision_suma_decimal_dbi <- function(senas, tipo) {
  p <- .precision_declarada_dbi(tipo)
  if (is.na(p) && .es_transact_sql_dbi(senas) && .moneda_sqlserver_dbi(tipo)) {
    p <- 19L
  }
  p
}

.postgresql_dbi <- function(senas) {
  length(senas) == 1L && !is.na(senas) &&
    grepl("postgres|pqconnection", senas, useBytes = TRUE)
}

.duckdb_senas_dbi <- function(senas) {
  length(senas) == 1L && !is.na(senas) &&
    grepl("duckdb", senas, fixed = TRUE, useBytes = TRUE)
}

# La suma de una expresion, como texto, en cada motor.
.suma_como_texto_sql_dbi <- function(senas, expresion) {
  if (.es_transact_sql_dbi(senas)) {
    return(paste0("CONVERT(VARCHAR(100), SUM(", expresion, "))"))
  }
  if (.es_mysql_dbi(senas)) return(paste0("CAST(SUM(", expresion, ") AS CHAR)"))
  if (.postgresql_dbi(senas)) return(paste0("CAST(SUM(", expresion, ") AS TEXT)"))
  paste0("CAST(SUM(", expresion, ") AS VARCHAR)")
}

# Un termino de las sumas exactas: `sum(factor * valor * 10^exponente)` de los
# textos que devuelven sus `expresiones` es la suma buscada.
.termino_suma_dbi <- function(expresiones, factores = NULL, exponentes = NULL) {
  list(
    expresiones = expresiones,
    factores = if (is.null(factores)) rep(1, length(expresiones)) else factores,
    exponentes = if (is.null(exponentes)) {
      rep(0, length(expresiones))
    } else exponentes
  )
}

# Los trozos de nueve cifras de `|expresion|`, un decimal de escala `s` con a
# lo sumo `ancho` cifras sin contar el punto: escrito sin el punto y con ceros a
# la izquierda hasta `ancho` cifras es el entero `|x| * 10^s`. `suma`, la de los
# trozos con el signo del valor (si `lineal`), y `cuadrados`, la de los
# productos de cada par, por dos si son distintos. Cada producto queda debajo de
# 1e18 y su suma, con menos de 1e19 filas, debajo de 1e37: entra en cualquier
# `DECIMAL(38,0)`. `NULL` en un motor sin estas funciones de texto.
.trozos_decimal_dbi <- function(senas, expresion, ancho, s, filas,
                                lineal = FALSE) {
  if (length(ancho) != 1L || !is.finite(ancho) || ancho < 1 || ancho > 80 ||
      length(s) != 1L || is.na(s) || !is.finite(filas) ||
      ceiling(log10(filas + 1)) + 18 > 37) {
    return(NULL)
  }
  if (.es_transact_sql_dbi(senas)) {
    texto <- paste0(
      "REPLACE(CONVERT(VARCHAR(80), ABS(", expresion, ")), '.', '')"
    )
    relleno <- paste0("RIGHT(REPLICATE('0', ", ancho, ") + ", texto, ", ",
                      ancho, ")")
  } else if (.es_mysql_dbi(senas)) {
    relleno <- paste0("LPAD(REPLACE(CAST(ABS(", expresion,
                      ") AS CHAR), '.', ''), ", ancho, ", '0')")
  } else if (.duckdb_senas_dbi(senas)) {
    relleno <- paste0("LPAD(REPLACE(CAST(ABS(", expresion,
                      ") AS VARCHAR), '.', ''), ", ancho, ", '0')")
  } else {
    return(NULL)
  }
  cantidad <- ceiling(ancho / 9)
  trozos <- character(cantidad)
  for (j in seq_len(cantidad)) {
    fin <- ancho - 9L * (j - 1L)
    inicio <- max(1L, fin - 8L)
    trozos[[j]] <- paste0("CAST(SUBSTRING(", relleno, ", ", inicio, ", ",
                          fin - inicio + 1L, ") AS DECIMAL(18,0))")
  }
  suma <- if (isTRUE(lineal)) {
    .termino_suma_dbi(
      vapply(trozos, function(trozo) {
        .suma_como_texto_sql_dbi(senas, paste0(
          "CASE WHEN ", expresion, " < 0 THEN -", trozo, " ELSE ", trozo,
          " END"
        ))
      }, character(1L), USE.NAMES = FALSE),
      exponentes = 9 * (seq_len(cantidad) - 1L) - s
    )
  } else NULL
  expresiones <- character()
  factores <- numeric()
  exponentes <- numeric()
  for (i in seq_len(cantidad)) {
    for (j in i:cantidad) {
      expresiones <- c(expresiones, .suma_como_texto_sql_dbi(
        senas, paste0(trozos[[i]], " * ", trozos[[j]])
      ))
      factores <- c(factores, if (i == j) 1 else 2)
      exponentes <- c(exponentes, 9 * (i - 1L) + 9 * (j - 1L) - 2 * s)
    }
  }
  list(suma = suma,
       cuadrados = .termino_suma_dbi(expresiones, factores, exponentes))
}

# La suma exacta de la columna (ver arriba): `valor` es la expresion que se suma
# -en PostgreSQL, la que deja fuera `NaN` e `Infinity`-, `filas` una cota de
# las filas y `cifras` las enteras del mayor valor absoluto, o `NA` si no se
# leyeron (entonces, las que deja la precision). `NULL` si no se puede.
.suma_exacta_sql_dbi <- function(senas, valor, tipo, filas, cifras) {
  if (.postgresql_dbi(senas)) {
    return(.termino_suma_dbi(.suma_como_texto_sql_dbi(senas, valor)))
  }
  s <- .escala_suma_decimal_dbi(senas, tipo)
  p <- .precision_suma_decimal_dbi(senas, tipo)
  if (is.na(s) || !is.finite(filas)) return(NULL)
  if (is.na(cifras)) cifras <- if (is.na(p)) NA_real_ else max(0, p - s)
  if (is.na(cifras)) return(NULL)
  d <- ceiling(log10(filas + 1))
  # El ancho de los trozos sale del tipo, no de los datos: un valor que no
  # entra en el tipo no existe.
  ancho <- if (is.na(p)) max(1, cifras) + s else max(p, s + 1)
  moneda <- .es_transact_sql_dbi(senas) && .moneda_sqlserver_dbi(tipo)
  base <- if (moneda) paste0("CAST(", valor, " AS DECIMAL(19,4))") else valor
  if (.es_transact_sql_dbi(senas)) {
    if (moneda && cifras + 4 + d <= 38) {
      return(.termino_suma_dbi(.suma_como_texto_sql_dbi(
        senas, paste0("CAST(", valor, " AS DECIMAL(38,4))")
      )))
    }
    if (!moneda && cifras + d <= 38 && s + d <= 38) {
      return(.termino_suma_dbi(c(
        .suma_como_texto_sql_dbi(senas, paste0(
          "CAST(ROUND(", valor, ", 0, 1) AS DECIMAL(38,0))"
        )),
        .suma_como_texto_sql_dbi(senas, paste0(valor, " % 1"))
      )))
    }
  } else if (.es_mysql_dbi(senas)) {
    if (cifras + s + d <= 65) {
      return(.termino_suma_dbi(.suma_como_texto_sql_dbi(senas, valor)))
    }
  } else if (.duckdb_senas_dbi(senas)) {
    if (cifras + d <= 38 && s + d <= 38) {
      return(.termino_suma_dbi(c(
        .suma_como_texto_sql_dbi(senas, paste0(
          "CAST(TRUNC(", valor, ") AS DECIMAL(38,0))"
        )),
        .suma_como_texto_sql_dbi(senas, paste0(
          valor, " - TRUNC(", valor, ")"
        ))
      )))
    }
  } else {
    return(NULL)
  }
  .trozos_decimal_dbi(senas, base, ancho, s, filas, lineal = TRUE)$suma
}

# La suma exacta de los cuadrados de `valor - centro`: `list(termino, centro)`,
# con `centro` el decimal exacto que se resto -cero si no se resto ninguno- y
# su texto, o `NULL`. Con un centro cerca de la media las diferencias tienen
# menos cifras que los valores y el cuadrado entra en el tipo mas veces; sin
# centro -sin media, sin escala, o con diferencias de mas de 38 cifras-, los
# cuadrados de los valores, que no necesitan nada mas que el tipo.
.cuadrados_exactos_sql_dbi <- function(senas, valor, tipo, filas, extremos,
                                       centro) {
  s <- .escala_suma_decimal_dbi(senas, tipo)
  cero <- function(escala) {
    list(texto = NA_character_,
         decimal = list(negativo = FALSE, cifras = 0, escala = escala),
         escala = escala)
  }
  if (.postgresql_dbi(senas)) {
    return(list(
      termino = .termino_suma_dbi(.suma_como_texto_sql_dbi(
        senas, paste0(valor, " * ", valor)
      )),
      centro = cero(0L)
    ))
  }
  p <- .precision_suma_decimal_dbi(senas, tipo)
  if (is.na(s) || !is.finite(filas)) return(NULL)
  d <- ceiling(log10(filas + 1))
  moneda <- .es_transact_sql_dbi(senas) && .moneda_sqlserver_dbi(tipo)
  base <- if (moneda) paste0("CAST(", valor, " AS DECIMAL(19,4))") else valor
  directo <- function(cuadrado) {
    if (.es_transact_sql_dbi(senas)) {
      return(.termino_suma_dbi(c(
        .suma_como_texto_sql_dbi(senas, paste0(
          "CAST(ROUND(", cuadrado, ", 0, 1) AS DECIMAL(38,0))"
        )),
        .suma_como_texto_sql_dbi(senas, paste0(cuadrado, " % 1"))
      )))
    }
    .termino_suma_dbi(.suma_como_texto_sql_dbi(senas, cuadrado))
  }
  # q: las cifras de la diferencia (o del valor), con las de la escala.
  armar <- function(expresion, enteras, centrada) {
    q <- enteras + s
    if (.es_mysql_dbi(senas)) {
      # MariaDB deriva el tipo del producto de los tipos declarados, no de los
      # valores: el de dos `DECIMAL(39,10)` -un `DECIMAL(38,10)` menos un
      # literal- pasa de 65 cifras y lo hace en doble, sin avisar (medido con
      # MariaDB 11.8: 173789153818238643568577.07 por ...290110267265.16), aun
      # con cada factor llevado a `DECIMAL(23,10)`. Directo solo si el del tipo
      # entra.
      if (2 * s <= 30 && 2 * q + d <= 64 && !is.na(p) && 2 * (p + 1) <= 65) {
        return(directo(paste0(expresion, " * ", expresion)))
      }
      if (!centrada || q + 1 <= 65) {
        return(.trozos_decimal_dbi(
          senas, expresion, if (centrada) q + 1 else ancho_tipo, s, filas
        )$cuadrados)
      }
      return(NULL)
    }
    if (.es_transact_sql_dbi(senas)) {
      if (q <= 18 && 2 * enteras + d <= 37 && 2 * s + d <= 37) {
        factor <- paste0("CAST(", expresion, " AS DECIMAL(", max(q, 1),
                         ",", s, "))")
        return(directo(paste0("(", factor, " * ", factor, ")")))
      }
      if (centrada && q > 38) return(NULL)
      trozo <- if (centrada) {
        paste0("CAST(", expresion, " AS DECIMAL(", q, ",", s, "))")
      } else expresion
      return(.trozos_decimal_dbi(
        senas, trozo, if (centrada) q + 1 else ancho_tipo, s, filas
      )$cuadrados)
    }
    if (.duckdb_senas_dbi(senas)) {
      if (2 * q + d <= 38) {
        # Un factor de 19 cifras lleva el producto a 128 bits.
        factor <- paste0("CAST(", expresion, " AS DECIMAL(", max(q, 19L),
                         ",", s, "))")
        return(directo(paste0(factor, " * ", factor)))
      }
      if (centrada && q > 38) return(NULL)
      trozo <- if (centrada) {
        paste0("CAST(", expresion, " AS DECIMAL(", q, ",", s, "))")
      } else expresion
      return(.trozos_decimal_dbi(
        senas, trozo, if (centrada) q + 1 else ancho_tipo, s, filas
      )$cuadrados)
    }
    NULL
  }
  cifras_valor <- .cifras_enteras_dbi(extremos)
  if (is.na(cifras_valor) && !is.na(p)) cifras_valor <- max(0, p - s)
  ancho_tipo <- if (is.na(p)) {
    if (is.na(cifras_valor)) NA_real_ else max(1, cifras_valor) + s
  } else max(p, s + 1)
  if (!is.null(centro) && !is.na(.cifras_enteras_dbi(extremos))) {
    # La diferencia con el centro, con el error de los extremos, que son dobles:
    # desde 2^53 dos decimales distintos son el mismo doble y la diferencia de
    # los dobles da 0 con el valor a cientos de unidades del centro.
    finitos <- extremos[is.finite(extremos)]
    enteras <- .cifras_enteras_dbi(
      max(abs(finitos - as.numeric(centro$texto))) + max(abs(finitos)) * 2^-50
    )
    diferencia <- paste0("(", base, " - (", centro$texto, "))")
    termino <- armar(diferencia, enteras, centrada = TRUE)
    if (!is.null(termino)) return(list(termino = termino, centro = centro))
  }
  if (is.na(cifras_valor) || is.na(ancho_tipo)) return(NULL)
  termino <- armar(base, cifras_valor, centrada = FALSE)
  if (is.null(termino)) return(NULL)
  list(termino = termino, centro = cero(s))
}

# Cuantas expresiones puede pedir una columna en la consulta de las sumas, del
# tipo solo: la cuenta, la suma y los cuadrados por trozos en el peor caso. Con
# eso se arman los lotes, y el plan los cuenta igual que la corrida.
.piezas_maximas_decimal_dbi <- function(senas, tipo) {
  if (.postgresql_dbi(senas)) return(4)
  p <- .precision_suma_decimal_dbi(senas, tipo)
  if (is.na(p)) p <- if (.es_mysql_dbi(senas)) 65 else 38
  k <- ceiling((p + 2) / 9)
  1 + max(2, k) + k * (k + 1) / 2
}

# Cuantas expresiones acepta el `SELECT` de cada motor (medido: PostgreSQL 16 y
# 9.3, 1.664; SQL Server rechaza 4.097 y con 4.096 falla el controlador; MySQL,
# MariaDB y DuckDB aceptan 20.000).
.limite_expresiones_dbi <- function(senas) {
  if (.postgresql_dbi(senas)) return(1600)
  if (.es_transact_sql_dbi(senas)) return(4000)
  Inf
}

# Los lotes de la consulta de las sumas: a lo sumo `tamano_lote` columnas, como
# los agregados planos, y las expresiones que el motor acepta, contadas por el
# tipo de cada columna. Lo usan la corrida y el plan.
.lotes_decimales_dbi <- function(senas, tipos, tamano_lote) {
  if (!length(tipos)) return(list())
  tamano_lote <- suppressWarnings(as.numeric(tamano_lote))
  if (length(tamano_lote) != 1L || !is.finite(tamano_lote) ||
      tamano_lote < 1) {
    tamano_lote <- .TAMANO_LOTE_PLANOS_DBI
  }
  limite <- .limite_expresiones_dbi(senas)
  piezas <- vapply(tipos, function(tipo) {
    .piezas_maximas_decimal_dbi(senas, tipo)
  }, numeric(1L), USE.NAMES = FALSE)
  lotes <- list()
  actual <- integer()
  ocupadas <- 0
  for (i in seq_along(tipos)) {
    if (length(actual) &&
        (length(actual) >= tamano_lote || ocupadas + piezas[[i]] > limite)) {
      lotes[[length(lotes) + 1L]] <- actual
      actual <- integer()
      ocupadas <- 0
    }
    actual <- c(actual, i)
    ocupadas <- ocupadas + piezas[[i]]
  }
  lotes[[length(lotes) + 1L]] <- actual
  lotes
}

# Las consultas que pueden emitir los lotes si el motor rechaza alguno: cada
# lote de k columnas se parte en mitades hasta aislar la que falla, a lo sumo
# 2k - 1 consultas.
.consultas_maximas_lotes_decimales_dbi <- function(lotes) {
  sum(vapply(lotes, function(lote) 2 * length(lote) - 1, numeric(1L)))
}

.MOTIVO_DECIMAL_BLOQUES_DBI <- paste(
  "La columna es decimal y el controlador entrega cada valor como doble, que",
  "no guarda todas sus cifras: la media, la mediana y el desv\u00edo se piden",
  "al motor en el tipo de la columna."
)

# El centro con el que el motor resta: la media aproximada escrita como decimal
# en la escala de la columna -`money` de SQL Server, cuatro decimales-, un
# literal que el motor resta en el tipo de la columna sin perder cifras
# (medido en SQL Server con compatibilidad 160 y 100, y en MySQL: `decimal(38,
# s)` menos un literal de escala `s` sigue siendo `decimal(38, s)` para `s` de
# 2 a 30). `NULL` sin escala declarada, donde no se sabe con cuantas cifras
# escribirlo, o sin media.
.centro_decimal_dbi <- function(senas, tipo, media) {
  escala <- .escala_suma_decimal_dbi(senas, tipo)
  if (is.na(escala)) return(NULL)
  if (length(media) != 1L || !is.finite(media)) return(NULL)
  texto <- formatC(media, format = "f", digits = escala)
  decimal <- .decimal_desde_texto_dbi(texto)
  if (is.null(decimal)) return(NULL)
  .sumar_centro_motor(texto)
  list(texto = texto, decimal = decimal, escala = escala)
}

# `decimal * factor * 10^exponente`, exacto, con `factor` 1 o 2.
.escalar_decimal_dbi <- function(decimal, factor, exponente) {
  if (factor != 1) {
    decimal$cifras <- .por_chico_entero_grande_dbi(decimal$cifras, factor)
  }
  if (exponente >= 0) {
    decimal$cifras <- .por_potencia_diez_entero_grande_dbi(
      decimal$cifras, exponente
    )
  } else {
    decimal$escala <- decimal$escala - exponente
  }
  decimal
}

# La suma exacta de un termino desde los textos que devolvio el motor. `NULL`
# si alguno no es un decimal.
.combinar_termino_dbi <- function(termino, textos) {
  if (is.null(termino) || length(textos) != length(termino$expresiones) ||
      anyNA(textos)) {
    return(NULL)
  }
  partes <- lapply(seq_along(textos), function(j) {
    parte <- .decimal_desde_texto_dbi(textos[[j]])
    if (is.null(parte)) return(NULL)
    .escalar_decimal_dbi(parte, termino$factores[[j]], termino$exponentes[[j]])
  })
  if (any(vapply(partes, is.null, logical(1L)))) return(NULL)
  escala <- max(vapply(partes, `[[`, numeric(1L), "escala"))
  total <- list(negativo = FALSE, cifras = 0, escala = escala)
  for (parte in partes) {
    total <- .sumar_con_signo_decimal_dbi(
      total, .a_escala_decimal_dbi(parte, escala)
    )
  }
  if (all(total$cifras == 0)) total$negativo <- FALSE
  total
}

# Las expresiones de las sumas exactas de una columna, sin centro, con sus
# alias (`prefijo` y la pieza): la cuenta, la de los finitos en PostgreSQL, la
# suma y, si `desvio`, la de los cuadrados de los valores. Las usa la muestra del
# motor, que las pide en la misma sentencia que los agregados: otra sentencia
# veria otra muestra, y ahi no hay media ni extremos leidos de antemano. `NULL`
# si el tipo no deja sumar exacto.
.piezas_sumas_sin_centro_dbi <- function(senas, columna_sql, tipo, filas,
                                         desvio, prefijo, alias) {
  postgres <- .postgresql_dbi(senas)
  valor <- if (postgres) {
    paste0("(CASE WHEN ", columna_sql, " - ", columna_sql, " = 0 THEN ",
           columna_sql, " END)")
  } else columna_sql
  suma <- .suma_exacta_sql_dbi(senas, valor, tipo, filas, NA_real_)
  if (is.null(suma)) return(NULL)
  cuadrados <- if (isTRUE(desvio)) {
    .cuadrados_exactos_sql_dbi(senas, valor, tipo, filas, NA_real_, NULL)
  } else NULL
  partes <- c(
    paste0(
      .conteo_sql_dbi(.contar_no_nulos_sql_dbi(columna_sql, senas), senas),
      " AS ", alias(paste0(prefijo, "n"))
    ),
    if (postgres) paste0("COUNT(", valor, ") AS ", alias(paste0(prefijo, "f"))),
    paste0(suma$expresiones, " AS ", alias(paste0(
      prefijo, "s", seq_along(suma$expresiones)
    ))),
    if (length(cuadrados$termino$expresiones)) paste0(
      cuadrados$termino$expresiones, " AS ", alias(paste0(
        prefijo, "q", seq_along(cuadrados$termino$expresiones)
      ))
    )
  )
  list(partes = partes, suma = suma, cuadrados = cuadrados,
       postgres = postgres, prefijo = prefijo)
}

# La media y el desvio exactos de lo que devolvio el motor para unas piezas
# (`.piezas_sumas_sin_centro_dbi()`, o las de `.sumas_exactas_decimales_dbi()`
# con sus nombres): `list(cuenta, finitos, suma, media, desvio)`, con `NA` donde
# no se puede.
.leer_sumas_exactas_dbi <- function(datos, nombres, suma, cuadrados,
                                    postgres) {
  salida <- list(cuenta = NA_real_, finitos = NA_real_, suma = NULL,
                 media = NA_real_, desvio = NA_real_)
  if (!is.data.frame(datos) || nrow(datos) != 1L) return(salida)
  textos_de <- function(prefijo, cuantos) {
    vapply(seq_len(cuantos), function(j) {
      v <- datos[[paste0(prefijo, j)]]
      if (length(v) == 1L && !is.na(v)) as.character(v) else NA_character_
    }, character(1L))
  }
  cuenta <- .numero_dbi(datos[[nombres$n]])
  finitos <- if (isTRUE(postgres)) .numero_dbi(datos[[nombres$f]]) else cuenta
  if (length(cuenta) != 1L) cuenta <- NA_real_
  if (length(finitos) != 1L) finitos <- NA_real_
  salida$cuenta <- cuenta
  salida$finitos <- finitos
  if (!is.finite(finitos) || finitos < 1 || is.null(suma)) return(salida)
  total <- .combinar_termino_dbi(
    suma, textos_de(nombres$s, length(suma$expresiones))
  )
  salida$suma <- total
  salida$media <- .doble_mas_cercano_decimal_dbi(total, finitos)
  if (finitos >= 2 && !is.null(cuadrados) && !is.null(total)) {
    termino <- cuadrados$termino
    sumados <- .combinar_termino_dbi(
      termino, textos_de(nombres$q, length(termino$expresiones))
    )
    # La suma de las diferencias con el centro es la de la columna menos `n`
    # veces el centro, exacta.
    centro <- cuadrados$centro
    escala <- max(total$escala, centro$decimal$escala)
    a <- .a_escala_decimal_dbi(total, escala)
    c_ <- .a_escala_decimal_dbi(centro$decimal, escala)
    c_$cifras <- .por_entero_entero_grande_dbi(c_$cifras, finitos)
    c_$negativo <- !isTRUE(c_$negativo)
    diferencias <- .sumar_con_signo_decimal_dbi(a, c_)
    if (!is.null(sumados)) {
      salida$desvio <- .desvio_exacto_sumas_dbi(finitos, diferencias, sumados)
    }
  }
  salida
}

# La media y el desvio exactos de unas columnas decimales. Por cada una, la
# cuenta, la suma (`.suma_exacta_sql_dbi()`) y, si se pide el desvio, la de los
# cuadrados (`.cuadrados_exactos_sql_dbi()`), todo como texto; R divide y
# redondea al doble mas cercano (`.doble_mas_cercano_decimal_dbi()`) y saca el
# desvio exacto de las dos sumas (`.desvio_exacto_sumas_dbi()`), sin
# cancelacion: es aritmetica entera. La usan la tabla completa y la via por
# bloques.
#
# `columnas`, una lista por columna con `columna_sql`, `tipo`, `n` (la cuenta
# que ya se conoce, o `NA`), `filas` (una cota de las filas), `media` (la
# aproximada, o `NA`: centra los cuadrados y contrasta el resultado), `cota`
# (cuanto puede distar la aproximada de la exacta), `extremos` (los leidos, o
# `NA`), y `media_pedida` y `desvio_pedido`. Devuelve, por columna, `media` y
# `desvio` como `list(ok, valor, sql, motivo)`. Si la consulta falla o lo que
# devuelve no es lo que ya se sabia de la columna -la cuenta, o una media mas
# lejos que la cota-, `ok = FALSE` con el motivo.
#
# Va por lotes (`.lotes_decimales_dbi()`); un lote que el motor rechaza se
# parte en mitades hasta aislar la columna que lo tumba, y las demas siguen.
#
# El centro es un literal que sale de los datos: con `incluir_valores = FALSE`
# el SQL que se devuelve para publicar lo lleva tapado, y el que corre, no.
.sumas_exactas_decimales_dbi <- function(conexion, tabla_sql, columnas,
                                         presupuesto = NULL,
                                         etapa = "basicos",
                                         prefijo_motivo =
                                           .MOTIVO_DECIMAL_BLOQUES_DBI,
                                         incluir_valores = TRUE,
                                         tamano_lote = .TAMANO_LOTE_PLANOS_DBI) {
  salida <- list(columnas = list(), consultas = 0L, sql = character())
  if (!length(columnas)) return(salida)
  senas <- .senas_conexion_dbi(conexion)
  postgres <- .postgresql_dbi(senas)
  alias <- function(nombre) .citar_identificador_dbi(conexion, nombre)
  fallo <- function(motivo, sql) {
    list(ok = FALSE, valor = NA_real_, sql = sql,
         motivo = if (nzchar(prefijo_motivo)) {
           paste(prefijo_motivo, motivo)
         } else motivo)
  }
  for (k in seq_along(columnas)) {
    columna <- columnas[[k]]
    columna_sql <- columna$columna_sql
    valor <- if (postgres) {
      paste0("(CASE WHEN ", columna_sql, " - ", columna_sql, " = 0 THEN ",
             columna_sql, " END)")
    } else columna_sql
    filas <- suppressWarnings(as.numeric(columna$filas))
    if (length(filas) != 1L || !is.finite(filas)) filas <- 1e15
    extremos <- suppressWarnings(as.numeric(columna$extremos))
    suma <- .suma_exacta_sql_dbi(
      senas, valor, columna$tipo, filas, .cifras_enteras_dbi(extremos)
    )
    cuadrados <- if (isTRUE(columna$desvio_pedido) && !is.null(suma)) {
      .cuadrados_exactos_sql_dbi(
        senas, valor, columna$tipo, filas, extremos,
        .centro_decimal_dbi(senas, columna$tipo, columna$media)
      )
    } else NULL
    columnas[[k]]$suma <- suma
    columnas[[k]]$cuadrados <- cuadrados
    columnas[[k]]$partes <- c(
      paste0(
        .conteo_sql_dbi(.contar_no_nulos_sql_dbi(columna_sql, senas), senas),
        " AS ", alias(paste0("lupa_n_", k))
      ),
      if (postgres) paste0(
        "COUNT(", valor, ") AS ", alias(paste0("lupa_f_", k))
      ),
      if (length(suma$expresiones)) paste0(
        suma$expresiones, " AS ", alias(paste0(
          "lupa_suma_", k, "_", seq_along(suma$expresiones)
        ))
      ),
      if (length(cuadrados$termino$expresiones)) paste0(
        cuadrados$termino$expresiones, " AS ", alias(paste0(
          "lupa_cuadrados_", k, "_", seq_along(cuadrados$termino$expresiones)
        ))
      )
    )
  }
  decidir <- function(k, datos, sql) {
    campo <- names(columnas)[[k]]
    columna <- columnas[[k]]
    resultado <- list()
    leida <- .leer_sumas_exactas_dbi(
      datos,
      list(n = paste0("lupa_n_", k), f = paste0("lupa_f_", k),
           s = paste0("lupa_suma_", k, "_"), q = paste0("lupa_cuadrados_", k, "_")),
      columna$suma, columna$cuadrados, postgres
    )
    finitos <- leida$finitos
    if (!is.finite(finitos) || finitos < 1 || is.null(columna$suma)) {
      return(invisible())
    }
    media <- leida$media
    n_conocida <- suppressWarnings(as.numeric(columna$n))
    media_conocida <- suppressWarnings(as.numeric(columna$media))
    cota <- suppressWarnings(as.numeric(columna$cota))
    coincide <- !is.null(leida$suma) && is.finite(media) && (
      length(n_conocida) != 1L || !is.finite(n_conocida) ||
        isTRUE(n_conocida == leida$cuenta) || isTRUE(n_conocida == finitos)
    ) && (
      length(media_conocida) != 1L || !is.finite(media_conocida) ||
        length(cota) != 1L || !is.finite(cota) ||
        isTRUE(abs(media - media_conocida) <= cota)
    )
    distinta <- fallo(paste(
      "La suma del motor no es la de lo que ya se ley\u00f3 de la columna:",
      "la tabla cambi\u00f3 entre las dos lecturas."
    ), sql)
    if (isTRUE(columna$media_pedida)) {
      resultado$media <- if (coincide) {
        list(ok = TRUE, valor = media, sql = sql, motivo = NA_character_)
      } else distinta
    }
    if (isTRUE(columna$desvio_pedido) && finitos >= 2 &&
        !is.null(columna$cuadrados)) {
      desvio <- if (coincide) leida$desvio else NA_real_
      resultado$desvio <- if (coincide && is.finite(desvio)) {
        list(ok = TRUE, valor = desvio, sql = sql, motivo = NA_character_)
      } else distinta
    }
    if (length(resultado)) salida$columnas[[campo]] <<- resultado
    invisible()
  }
  fallar <- function(indices, motivo, sql) {
    for (k in indices) {
      columna <- columnas[[k]]
      resultado <- list()
      if (isTRUE(columna$media_pedida)) resultado$media <- fallo(motivo, sql)
      if (isTRUE(columna$desvio_pedido)) resultado$desvio <- fallo(motivo, sql)
      if (length(resultado)) {
        salida$columnas[[names(columnas)[[k]]]] <<- resultado
      }
    }
    invisible()
  }
  ejecutar <- function(indices) {
    consulta <- paste0(
      "SELECT ", paste(unlist(lapply(columnas[indices], `[[`, "partes")),
                       collapse = ", "),
      " FROM ", tabla_sql
    )
    sql <- consulta
    if (!isTRUE(incluir_valores)) {
      for (columna in columnas[indices]) {
        texto <- columna$cuadrados$centro$texto
        if (is.null(texto) || is.na(texto)) next
        sql <- gsub(
          paste0(" - (", texto, ")"),
          paste0(" - (", .LITERAL_MOTOR_OMITIDO, ")"), sql, fixed = TRUE,
          useBytes = TRUE
        )
      }
    }
    if (!is.null(presupuesto) && .saldo_dbi(presupuesto) < 1) {
      leida <- .consultar_dbi(conexion, consulta, presupuesto, etapa = etapa)
      return(fallar(indices, leida$motivo, sql))
    }
    leida <- .consultar_dbi(conexion, consulta, presupuesto, etapa = etapa)
    salida$consultas <<- salida$consultas + 1L
    salida$sql <<- c(salida$sql, sql)
    if (isTRUE(leida$ok) && is.data.frame(leida$datos) &&
        nrow(leida$datos) == 1L) {
      for (k in indices) decidir(k, leida$datos, sql)
      return(invisible())
    }
    if (length(indices) > 1L) {
      mitad <- ceiling(length(indices) / 2)
      ejecutar(indices[seq_len(mitad)])
      ejecutar(indices[-seq_len(mitad)])
      return(invisible())
    }
    motivo <- if (isTRUE(leida$ok)) {
      "La consulta no devolvi\u00f3 una fila."
    } else paste0("La consulta fall\u00f3: ", leida$motivo)
    fallar(indices, motivo, sql)
  }
  lotes <- .lotes_decimales_dbi(
    senas, vapply(columnas, function(columna) {
      tipo <- columna$tipo
      if (length(tipo) != 1L || is.na(tipo)) NA_character_ else
        as.character(tipo)
    }, character(1L), USE.NAMES = FALSE),
    tamano_lote
  )
  for (lote in lotes) ejecutar(lote)
  salida
}

# La media, la mediana y el desvio por `bloque_filas` de cada decimal que el
# controlador entrega como doble, en el decimal (ver
# `.media_mediana_decimal_texto_dbi()`):
#
# * la media y el desvio, de las sumas exactas que calcula el motor, leidas
#   como texto, en una consulta para todas las columnas, sin orden
#   (`.sumas_exactas_decimales_dbi()`). El desvio es el doble mas cercano del
#   exacto donde el tipo deja sumar los cuadrados sin redondeo; si no, el de
#   los dobles, y si ese no se puede publicar
#   (`.desvio_mal_condicionado_dbi()`), `no_disponible`. Antes se pedia al
#   motor solo el mal condicionado, con las diferencias como dobles: quedaba a
#   un ulp (500000000000006.60 y vecinos, en los cinco motores con bloques);
# * la mediana solo cuando hace falta: con una cantidad impar la central es un
#   valor, y su doble es el mas cercano; con una par y las dos centrales en el
#   mismo doble, la mediana tambien cae en el, porque el redondeo es monotono.
#   Con dos centrales distintas se piden al motor como texto, con el mismo
#   salto que la via de dos consultas, y R las suma: una consulta ordenada por
#   columna. `money` de SQL Server siempre: odbc no lo lee al doble mas
#   cercano (medido: un `money` de 5e14 con cuatro decimales, central de nueve
#   valores, a un ulp), asi que ni la central de una cantidad impar es la
#   buena.
#
# Un decimal de escala 0 tambien entra: por debajo de 2^53 sus dobles son los
# valores y la media y el desvio salen iguales, pero desde 2^53 no, y el plan
# cuenta las columnas por su tipo. Lo que el motor devuelve se contrasta con lo
# que leyeron los
# bloques -la cuenta, que la media quede a menos de 2^-50 del mayor valor
# absoluto de la de los dobles, y las centrales en sus dobles-: si no, o si la
# consulta no se pudo hacer, la metrica queda `no_disponible` con el motivo, en
# lugar de publicar la de los dobles.
.media_mediana_decimal_bloques_dbi <- function(conexion, preparacion, fuente,
                                               sobres, metricas,
                                               incluir_valores, tipo_de,
                                               presupuesto = NULL,
                                               reserva_media = 0,
                                               n_filas = NA_real_) {
  salida <- list(columnas = list(), consultas = 0L)
  senas <- .senas_conexion_dbi(conexion)
  dialecto <- preparacion$dialecto
  alias <- function(nombre) .citar_identificador_dbi(conexion, nombre)
  resultado_de <- function(campo, familia) {
    sobre <- sobres[[.clave_par_identificador(campo, familia, sep = "\u001f")]]
    if (inherits(sobre, "acumulador_bloques")) sobre <- sobre$resultado
    if (is.list(sobre) && identical(sobre$estado, "calculado")) {
      sobre$resultado
    } else NULL
  }
  fallo <- function(motivo, sql = NA_character_) {
    list(ok = FALSE, valor = NA_real_, sql = sql, motivo = paste(
      .MOTIVO_DECIMAL_BLOQUES_DBI, motivo
    ))
  }
  exactas <- list()
  for (i in seq_along(preparacion$campos)) {
    campo <- preparacion$campos[[i]]
    tipo <- tipo_de(campo)
    if (!.mediana_decimal_por_centrales_dbi(senas, tipo) ||
        i > length(preparacion$campos_sql_proyeccion)) {
      next
    }
    # Toda columna decimal entra en las sumas, sin mirar los datos: el plan las
    # cuenta por el tipo (`.plan_bloques_dbi()`). Lo que leyeron los bloques
    # -la cuenta, la media, los extremos- elige las expresiones y contrasta el
    # resultado cuando esta.
    cuantitativos <- resultado_de(campo, "cuantitativos")
    n <- if (is.null(cuantitativos)) NA_real_ else {
      .numero_dbi(cuantitativos$n_evaluados)
    }
    if (length(n) != 1L) n <- NA_real_
    extremos <- if (is.null(cuantitativos)) NA_real_ else {
      suppressWarnings(as.numeric(c(
        cuantitativos$minimo, cuantitativos$maximo
      )))
    }
    columna_sql <- preparacion$campos_sql_proyeccion[[i]]
    columna <- list()
    media_dobles <- if (is.null(cuantitativos)) NA_real_ else {
      suppressWarnings(as.numeric(cuantitativos$media))
    }
    if (length(media_dobles) != 1L) media_dobles <- NA_real_
    media_pedida <- "basicos" %in% metricas
    desvio_pedido <- "desvio" %in% metricas
    if (media_pedida || desvio_pedido) {
      finitos <- extremos[is.finite(extremos)]
      exactas[[campo]] <- list(
        columna_sql = columna_sql, tipo = tipo, n = n,
        filas = if (is.finite(n)) n else n_filas, media = media_dobles,
        cota = if (length(finitos)) max(abs(finitos)) * 2^-50 else NA_real_,
        extremos = extremos,
        media_pedida = media_pedida, desvio_pedido = desvio_pedido
      )
    }
    mapa <- if ("mediana" %in% metricas && isTRUE(incluir_valores)) {
      resultado_de(campo, "distintos")
    } else NULL
    centrales <- if (is.data.frame(mapa)) {
      .centrales_mapa_bloques_dbi(mapa)
    } else NULL
    total_mapa <- if (is.data.frame(mapa)) {
      sum(suppressWarnings(as.numeric(mapa$frecuencia)))
    } else NA_real_
    moneda <- .es_transact_sql_dbi(senas) && .moneda_sqlserver_dbi(tipo)
    # Un decimal de escala 0 por debajo de 2^53: sus centrales llegaron
    # exactas, y la mediana de los dobles es la buena.
    entera_exacta <- identical(.escala_declarada_dbi(tipo), 0L) &&
      length(centrales) > 0L && is.double(centrales) &&
      all(abs(centrales) < .MAX_ENTERO_EXACTO_DBI)
    if (!entera_exacta &&
        length(centrales) %in% c(1L, 2L) && is.double(centrales) &&
        !inherits(centrales, "integer64") && all(is.finite(centrales)) &&
        (moneda || (length(centrales) == 2L &&
                      centrales[[1L]] != centrales[[2L]])) &&
        isTRUE(total_mapa == n)) {
      interna <- paste0(
        "SELECT ", dialecto$cast_entero_ancho_texto(
          columna_sql, alias("valor"), conexion, tipo = tipo
        ),
        " FROM ", fuente$tabla_sql, " WHERE ", columna_sql,
        " IS NOT NULL ORDER BY ", columna_sql
      )
      cuantas <- length(centrales)
      sql <- if (is.null(dialecto$cast_entero_ancho_texto)) NULL else {
        dialecto$limitar(interna, cuantas, (n + 1) %/% 2 - 1)
      }
      columna$mediana <- if (is.null(sql)) {
        fallo("El dialecto no expresa el salto de filas.")
      } else {
        salida$consultas <- salida$consultas + 1L
        leida <- .consultar_dbi(conexion, sql, presupuesto, etapa = "mediana")
        textos <- if (isTRUE(leida$ok) && is.data.frame(leida$datos) &&
                      ncol(leida$datos) >= 1L) {
          as.character(leida$datos[[1L]])
        } else NULL
        dobles <- suppressWarnings(as.numeric(textos))
        suma <- if (length(textos) == cuantas) {
          .sumar_decimales_texto_dbi(textos)
        } else NULL
        valor <- .doble_mas_cercano_decimal_dbi(suma, cuantas)
        if (!isTRUE(leida$ok)) {
          fallo(paste0("La consulta fall\u00f3: ", leida$motivo), sql)
        } else if (is.null(suma) || !is.finite(valor) ||
                   any(abs(dobles - centrales) >
                         abs(centrales) * 2^-40)) {
          fallo(paste(
            "Las centrales del motor no son las que leyeron los bloques:",
            "la tabla cambi\u00f3 entre las dos lecturas."
          ), sql)
        } else list(ok = TRUE, valor = valor, sql = sql, motivo = NA_character_)
      }
    }
    if (length(columna)) salida$columnas[[campo]] <- columna
  }
  if (length(exactas)) {
    # La media esta en el total del plan y usa el lugar que se le reservo; las
    # medianas de arriba no podian tomarlo (ver `.perfilar_dbi_bloques()`).
    if (!is.null(presupuesto) && reserva_media > 0) {
      presupuesto$reserva <- max(0, presupuesto$reserva - reserva_media)
    }
    sumas <- .sumas_exactas_decimales_dbi(
      conexion, fuente$tabla_sql, exactas, presupuesto = presupuesto,
      incluir_valores = incluir_valores,
      tamano_lote = preparacion$tamano_lote_planos %||% .TAMANO_LOTE_PLANOS_DBI
    )
    salida$consultas <- salida$consultas + sumas$consultas
    for (campo in names(sumas$columnas)) {
      if (is.null(salida$columnas[[campo]])) salida$columnas[[campo]] <- list()
      for (metrica in names(sumas$columnas[[campo]])) {
        salida$columnas[[campo]][[metrica]] <-
          sumas$columnas[[campo]][[metrica]]
      }
    }
  }
  salida
}
