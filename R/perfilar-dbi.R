.dbi_disponible <- function() {
  requireNamespace("DBI", quietly = TRUE)
}

.requerir_dbi <- function() {
  if (!.dbi_disponible()) {
    stop(
      "Para perfilar una muestra de una base se necesita instalar el paquete opcional 'DBI'.",
      call. = FALSE
    )
  }
}

.validar_muestra_dbi <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      !is.finite(muestra) || muestra < 1 || muestra != floor(muestra)) {
    stop("`muestra` debe ser un entero positivo finito.", call. = FALSE)
  }
  as.numeric(muestra)
}

.texto_tabla_dbi <- function(tabla) {
  texto <- tryCatch(as.character(tabla), error = function(e) character())
  if (!length(texto)) texto <- tryCatch(format(tabla), error = function(e) "")
  paste(texto, collapse = ".")
}

.info_conexion_dbi <- function(conexion) {
  info <- tryCatch(DBI::dbGetInfo(conexion), error = function(e) {
    list(no_disponible = conditionMessage(e))
  })
  list(
    clase_conexion = class(conexion),
    informacion_dbi = info
  )
}

.consultar_dbi <- function(conexion, sql) {
  tryCatch(
    list(ok = TRUE, datos = DBI::dbGetQuery(conexion, sql), motivo = NA_character_),
    error = function(e) {
      list(ok = FALSE, datos = NULL, motivo = conditionMessage(e))
    }
  )
}

.escalar_dbi <- function(conexion, sql, campo) {
  resultado <- .consultar_dbi(conexion, sql)
  if (!resultado$ok) {
    return(list(ok = FALSE, valor = NULL, motivo = resultado$motivo))
  }
  if (!nrow(resultado$datos) || !campo %in% names(resultado$datos)) {
    return(list(
      ok = FALSE, valor = NULL,
      motivo = paste0("La consulta no devolvio el campo `", campo, "`.")
    ))
  }
  list(ok = TRUE, valor = resultado$datos[[campo]][[1L]], motivo = NA_character_)
}

.registro_sql_dbi <- function(columna, metricas, estado, motivo, sql) {
  data.frame(
    columna = rep_len(as.character(columna), length(metricas)),
    metrica = as.character(metricas),
    estado = rep_len(as.character(estado), length(metricas)),
    motivo = rep_len(as.character(motivo), length(metricas)),
    sql = rep_len(as.character(sql), length(metricas)),
    stringsAsFactors = FALSE
  )
}

.registrar_resultado_dbi <- function(registros, columna, metricas, resultado,
                                     motivo_exito = NA_character_) {
  estado <- if (!is.null(resultado$estado)) {
    resultado$estado
  } else if (resultado$ok) {
    "calculado"
  } else {
    "no_disponible"
  }
  motivo <- if (resultado$ok) motivo_exito else resultado$motivo
  c(registros, list(.registro_sql_dbi(
    columna, metricas, estado, motivo, resultado$sql
  )))
}

.resumen_columna_dbi <- function(conexion, tabla_sql, columna, prototipo,
                                 n_total) {
  columna_sql <- as.character(DBI::dbQuoteIdentifier(conexion, columna))
  registros <- list()
  fila <- list(
    columna = columna,
    n = as.numeric(n_total),
    n_validos = NA_real_,
    n_faltantes = NA_real_,
    prop_faltantes = NA_real_,
    n_distintos = NA_real_,
    tasa_distintos = NA_real_,
    moda = NA_character_,
    frecuencia_moda = NA_real_,
    minimo = NA_real_,
    maximo = NA_real_,
    media = NA_real_,
    mediana = NA_real_,
    desvio = NA_real_,
    n_ceros = NA_real_,
    n_negativos = NA_real_
  )

  sql_validos <- paste0(
    "SELECT COUNT(", columna_sql, ") AS n_validos FROM ", tabla_sql
  )
  validos <- .escalar_dbi(conexion, sql_validos, "n_validos")
  validos$sql <- sql_validos
  registros <- .registrar_resultado_dbi(
    registros, columna, c("n_validos", "n_faltantes", "prop_faltantes"), validos
  )
  if (validos$ok) {
    fila$n_validos <- as.numeric(validos$valor)
    fila$n_faltantes <- n_total - fila$n_validos
    fila$prop_faltantes <- if (n_total) fila$n_faltantes / n_total else NA_real_
  }

  sql_distintos <- paste0(
    "SELECT COUNT(DISTINCT ", columna_sql, ") AS n_distintos FROM ", tabla_sql
  )
  distintos <- .escalar_dbi(conexion, sql_distintos, "n_distintos")
  distintos$sql <- sql_distintos
  registros <- .registrar_resultado_dbi(
    registros, columna, c("n_distintos", "tasa_distintos"), distintos
  )
  if (distintos$ok) {
    fila$n_distintos <- as.numeric(distintos$valor)
    if (is.finite(fila$n_validos) && fila$n_validos > 0) {
      fila$tasa_distintos <- fila$n_distintos / fila$n_validos
    }
  }

  sql_moda <- paste0(
    "SELECT ", columna_sql, " AS valor, COUNT(*) AS frecuencia FROM ",
    tabla_sql, " WHERE ", columna_sql, " IS NOT NULL GROUP BY ",
    columna_sql, " ORDER BY frecuencia DESC, ", columna_sql, " ASC LIMIT 1"
  )
  moda <- .consultar_dbi(conexion, sql_moda)
  if (moda$ok && nrow(moda$datos)) {
    valor_moda <- tryCatch(
      .texto_valor(moda$datos$valor[[1L]]),
      error = function(e) e
    )
    if (inherits(valor_moda, "error")) {
      moda$ok <- FALSE
      moda$motivo <- conditionMessage(valor_moda)
    } else {
      fila$moda <- valor_moda
      fila$frecuencia_moda <- as.numeric(moda$datos$frecuencia[[1L]])
    }
  } else if (moda$ok) {
    moda$motivo <- "La columna no contiene valores no nulos."
    moda$estado <- "sin_valores"
  }
  moda$sql <- sql_moda
  registros <- .registrar_resultado_dbi(
    registros, columna, c("moda", "frecuencia_moda"), moda,
    motivo_exito = moda$motivo
  )

  es_numerico <- is.numeric(prototipo) &&
    !inherits(prototipo, c("Date", "POSIXt", "integer64"))
  metricas_numericas <- c(
    "minimo", "maximo", "media", "mediana", "desvio", "n_ceros", "n_negativos"
  )
  if (!es_numerico) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, metricas_numericas, "no_aplica",
      paste0(
        "DBI expuso la columna como `", paste(class(prototipo), collapse = "/"),
        "`; no se aplicaron agregados cuantitativos."
      ),
      NA_character_
    )))
    return(list(fila = fila, sql = do.call(rbind, registros)))
  }

  if (!is.finite(fila$n_validos)) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, metricas_numericas, "no_disponible",
      "No se pudo conocer la cantidad de valores no nulos.", NA_character_
    )))
    return(list(fila = fila, sql = do.call(rbind, registros)))
  }
  if (fila$n_validos == 0) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, metricas_numericas, "sin_valores",
      "La columna no contiene valores no nulos.", NA_character_
    )))
    return(list(fila = fila, sql = do.call(rbind, registros)))
  }

  sql_basicos <- paste0(
    "SELECT MIN(", columna_sql, ") AS minimo, MAX(", columna_sql,
    ") AS maximo, AVG(", columna_sql, ") AS media, ",
    "SUM(CASE WHEN ", columna_sql, " = 0 THEN 1 ELSE 0 END) AS n_ceros, ",
    "SUM(CASE WHEN ", columna_sql, " < 0 THEN 1 ELSE 0 END) AS n_negativos ",
    "FROM ", tabla_sql
  )
  basicos <- .consultar_dbi(conexion, sql_basicos)
  if (basicos$ok && nrow(basicos$datos)) {
    for (metrica in c("minimo", "maximo", "media", "n_ceros", "n_negativos")) {
      fila[[metrica]] <- as.numeric(basicos$datos[[metrica]][[1L]])
    }
  }
  basicos$sql <- sql_basicos
  registros <- .registrar_resultado_dbi(
    registros, columna,
    c("minimo", "maximo", "media", "n_ceros", "n_negativos"), basicos
  )

  limite <- if (fila$n_validos %% 2 == 0) 2 else 1
  desplazamiento <- floor((fila$n_validos - 1) / 2)
  sql_mediana <- paste0(
    "SELECT AVG(valor) AS mediana FROM (SELECT ", columna_sql,
    " AS valor FROM ", tabla_sql, " WHERE ", columna_sql,
    " IS NOT NULL ORDER BY ", columna_sql, " LIMIT ", limite,
    " OFFSET ", format(desplazamiento, scientific = FALSE), ") AS lupa_mediana"
  )
  mediana <- .escalar_dbi(conexion, sql_mediana, "mediana")
  if (mediana$ok) fila$mediana <- as.numeric(mediana$valor)
  mediana$sql <- sql_mediana
  registros <- .registrar_resultado_dbi(registros, columna, "mediana", mediana)

  if (fila$n_validos < 2) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, "desvio", "no_aplica",
      "El desvio muestral requiere al menos dos valores no nulos.",
      NA_character_
    )))
  } else if (!is.finite(fila$media)) {
    registros <- c(registros, list(.registro_sql_dbi(
      columna, "desvio", "no_disponible",
      "No se pudo calcular la media necesaria para el desvio.", NA_character_
    )))
  } else {
    media_sql <- as.character(DBI::dbQuoteLiteral(conexion, fila$media))
    sql_desvio <- paste0(
      "SELECT SQRT(SUM((", columna_sql, " - ", media_sql, ") * (", columna_sql,
      " - ", media_sql, ")) / (COUNT(", columna_sql,
      ") - 1.0)) AS desvio FROM ", tabla_sql
    )
    desvio <- .escalar_dbi(conexion, sql_desvio, "desvio")
    desvio$sql <- sql_desvio
    if (desvio$ok) fila$desvio <- as.numeric(desvio$valor)
    registros <- .registrar_resultado_dbi(registros, columna, "desvio", desvio)
  }

  list(fila = fila, sql = do.call(rbind, registros))
}

.resumen_tabla_dbi <- function(conexion, tabla, tabla_sql, campos, prototipo,
                               n_total, sql_conteo, info_conexion) {
  resultados <- lapply(seq_along(campos), function(i) {
    .resumen_columna_dbi(
      conexion, tabla_sql, campos[[i]], prototipo[[i]], n_total
    )
  })
  columnas <- if (length(resultados)) {
    filas <- lapply(resultados, `[[`, "fila")
    as.data.frame(do.call(rbind, lapply(filas, as.data.frame)),
                  stringsAsFactors = FALSE)
  } else {
    data.frame(
      columna = character(), n = numeric(), n_validos = numeric(),
      n_faltantes = numeric(), prop_faltantes = numeric(),
      n_distintos = numeric(), tasa_distintos = numeric(),
      moda = character(), frecuencia_moda = numeric(), minimo = numeric(),
      maximo = numeric(), media = numeric(), mediana = numeric(),
      desvio = numeric(), n_ceros = numeric(), n_negativos = numeric(),
      stringsAsFactors = FALSE
    )
  }
  numericas <- setdiff(names(columnas), c("columna", "moda"))
  columnas[numericas] <- lapply(columnas[numericas], as.numeric)
  sql <- if (length(resultados)) {
    rbind(
      .registro_sql_dbi(
        campos, rep("n", length(campos)), "calculado", NA_character_, sql_conteo
      ),
      do.call(rbind, lapply(resultados, `[[`, "sql"))
    )
  } else {
    .registro_sql_dbi(character(), character(), character(), character(), character())
  }
  rownames(columnas) <- NULL
  rownames(sql) <- NULL
  list(
    columnas = columnas,
    sql = sql,
    meta = list(
      alcance = "tabla_completa",
      tabla = .texto_tabla_dbi(tabla),
      filas = as.numeric(n_total),
      motor = info_conexion,
      sql_conteo_filas = sql_conteo,
      criterio_moda = paste(
        "mayor frecuencia; empates resueltos por el valor ascendente",
        "segun el orden del motor"
      ),
      solo_lectura = TRUE,
      objetos_temporales = FALSE
    )
  )
}

.verificar_orden_dbi <- function(conexion, tabla_sql, orden_sql) {
  sql <- paste0(
    "SELECT COUNT(*) AS n_grupos_repetidos FROM (SELECT ",
    paste(orden_sql, collapse = ", "), " FROM ", tabla_sql,
    " GROUP BY ", paste(orden_sql, collapse = ", "),
    " HAVING COUNT(*) > 1) AS lupa_orden_repetido"
  )
  resultado <- .escalar_dbi(conexion, sql, "n_grupos_repetidos")
  list(
    unico = resultado$ok && identical(as.numeric(resultado$valor), 0),
    sql = sql,
    motivo = if (!resultado$ok) {
      paste0("No se pudo verificar la unicidad del orden: ", resultado$motivo)
    } else if (as.numeric(resultado$valor) > 0) {
      "Las columnas de orden no identifican cada fila de forma unica."
    } else {
      "Las columnas de orden identifican cada fila de forma unica."
    }
  )
}

#' Perfilar una muestra leida mediante DBI
#'
#' Calcula en SQL un resumen acotado sobre toda una tabla y, en un bloque
#' separado, ejecuta [perfilar()] sobre una muestra traida a memoria. El resumen
#' completo de 93 campos no se presenta como calculado por la base: esos campos
#' pertenecen exclusivamente a `perfil_muestra` y su universo es la muestra.
#'
#' Esta funcion no escribe en la conexion ni crea objetos temporales. `DBI` es
#' una dependencia opcional. Cada agregado no disponible queda en `NA` y su
#' consulta, estado y motivo se conservan en `resumen_tabla$sql`.
#' Las expresiones se ejecutan como capacidades a comprobar, no como un
#' dialecto SQL universal: si el motor rechaza una, se registra como no
#' disponible y las demas metricas siguen siendo independientes.
#'
#' @param conexion Conexion abierta compatible con DBI.
#' @param tabla Nombre de tabla o un objeto aceptado por
#'   [DBI::dbQuoteIdentifier()].
#' @param muestra Cantidad positiva y finita de filas solicitadas.
#' @param orden_muestra Columnas para `ORDER BY`. La salida solo declara orden
#'   reproducible cuando la combinacion es unica en toda la tabla. Sin este
#'   argumento, DBI no garantiza el orden ni la pertenencia de una muestra
#'   limitada, y `meta` lo declara expresamente.
#' @param ... Argumentos enviados a [perfilar()] para analizar la muestra.
#'
#' @return Objeto de clase `perfil_dbi` con exactamente dos bloques:
#'   `resumen_tabla`, de alcance completo, y `perfil_muestra`, un objeto
#'   `perfil` cuyo `meta$origen_dbi` declara tabla, conexion, SQL y alcance.
#' @export
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
#'   resultado <- perfilar_dbi(con, "ejemplo", muestra = 5, orden_muestra = "id")
#'   DBI::dbDisconnect(con)
#' }
perfilar_dbi <- function(conexion, tabla, muestra = 1000L,
                         orden_muestra = NULL, ...) {
  .requerir_dbi()
  muestra <- .validar_muestra_dbi(muestra)
  if (!DBI::dbIsValid(conexion)) {
    stop("`conexion` debe ser una conexion DBI abierta y valida.", call. = FALSE)
  }
  existe <- tryCatch(
    DBI::dbExistsTable(conexion, tabla),
    error = function(e) stop(
      "No se pudo comprobar `tabla`: ", conditionMessage(e), call. = FALSE
    )
  )
  if (!isTRUE(existe)) {
    stop("La tabla solicitada no existe en la conexion DBI.", call. = FALSE)
  }
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  campos <- DBI::dbListFields(conexion, tabla)
  if (!is.null(orden_muestra) &&
      (!is.character(orden_muestra) || !length(orden_muestra) ||
       anyNA(orden_muestra) || any(!nzchar(orden_muestra)))) {
    stop("`orden_muestra` debe ser NULL o nombres de columnas.", call. = FALSE)
  }
  desconocidas <- setdiff(orden_muestra, campos)
  if (length(desconocidas)) {
    stop(
      "Columnas de `orden_muestra` inexistentes: ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }

  sql_conteo <- paste0("SELECT COUNT(*) AS n FROM ", tabla_sql)
  conteo <- .escalar_dbi(conexion, sql_conteo, "n")
  if (!conteo$ok) {
    stop("No se pudo contar la tabla: ", conteo$motivo, call. = FALSE)
  }
  n_total <- as.numeric(conteo$valor)
  sql_esquema <- paste0("SELECT * FROM ", tabla_sql, " WHERE 1 = 0")
  esquema <- .consultar_dbi(conexion, sql_esquema)
  if (!esquema$ok) {
    stop("No se pudo leer el esquema de la tabla: ", esquema$motivo, call. = FALSE)
  }
  prototipo <- esquema$datos
  if (!identical(names(prototipo), campos)) campos <- names(prototipo)
  info_conexion <- .info_conexion_dbi(conexion)
  resumen <- .resumen_tabla_dbi(
    conexion, tabla, tabla_sql, campos, prototipo, n_total,
    sql_conteo, info_conexion
  )
  resumen$meta$sql_esquema <- sql_esquema

  orden_sql <- if (length(orden_muestra)) {
    as.character(DBI::dbQuoteIdentifier(conexion, orden_muestra))
  } else character()
  verificacion <- if (length(orden_sql)) {
    .verificar_orden_dbi(conexion, tabla_sql, orden_sql)
  } else {
    list(
      unico = FALSE, sql = NA_character_,
      motivo = "No se declaro `orden_muestra`; SQL no garantiza el orden de las filas."
    )
  }
  n_obtener <- min(n_total, muestra)
  sql_muestra <- paste0(
    "SELECT * FROM ", tabla_sql,
    if (length(orden_sql)) paste0(" ORDER BY ", paste(orden_sql, collapse = ", ")) else "",
    if (muestra < n_total) {
      paste0(" LIMIT ", format(muestra, scientific = FALSE))
    } else ""
  )
  consulta_muestra <- .consultar_dbi(conexion, sql_muestra)
  if (!consulta_muestra$ok) {
    stop("No se pudo leer la muestra: ", consulta_muestra$motivo, call. = FALSE)
  }
  datos_muestra <- consulta_muestra$datos
  n_obtenidas <- nrow(datos_muestra)
  if (!identical(as.numeric(n_obtenidas), as.numeric(n_obtener))) {
    stop(
      "La consulta de muestra devolvio ", n_obtenidas, " filas; se esperaban ",
      n_obtener, ". La tabla pudo cambiar durante la lectura.", call. = FALSE
    )
  }
  reproducible <- isTRUE(verificacion$unico)
  muestreo_meta <- list(
    filas_solicitadas = as.numeric(muestra),
    filas_obtenidas = as.numeric(n_obtenidas),
    filas_totales_fuente = as.numeric(n_total),
    tabla_completa = n_obtenidas == n_total,
    metodo = if (length(orden_sql)) {
      "primeras_filas_segun_orden"
    } else {
      "primeras_filas_sin_orden_garantizado"
    },
    orden_muestra = orden_muestra,
    orden_unico_verificado = isTRUE(verificacion$unico),
    reproducible = reproducible,
    motivo_reproducibilidad = verificacion$motivo,
    sql_verificacion_orden = verificacion$sql,
    sql_muestra = sql_muestra
  )

  argumentos <- list(...)
  if (is.null(argumentos$nombre)) {
    argumentos$nombre <- paste0("muestra DBI de ", .texto_tabla_dbi(tabla))
  }
  argumentos$muestra <- Inf
  perfil <- do.call(perfilar, c(list(datos = datos_muestra), argumentos))
  perfil$meta$origen_dbi <- list(
    tipo = "DBI",
    conexion = info_conexion,
    tabla = .texto_tabla_dbi(tabla),
    muestreo = muestreo_meta,
    solo_lectura = TRUE,
    objetos_temporales = FALSE
  )

  estructura <- list(resumen_tabla = resumen, perfil_muestra = perfil)
  class(estructura) <- "perfil_dbi"
  estructura
}
