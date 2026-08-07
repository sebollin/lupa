.version_esquema_historico <- 1L

.columnas_historico <- c(
  "version_esquema", "nivel", "id_registro", "id_medida", "id_medicion",
  "fecha", "perfil", "regla", "metrica", "metrica_especifica",
  "metrica_instanciada", "dimension", "factor", "granularidad",
  "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
  "n_elementos", "resultado", "agregacion"
)

.historico_vacio <- function() {
  resultado <- data.frame(
    version_esquema = integer(), nivel = character(),
    id_registro = character(), id_medida = character(),
    id_medicion = character(), fecha = as.POSIXct(character(), tz = "UTC"),
    perfil = character(), regla = character(), metrica = character(),
    metrica_especifica = character(), metrica_instanciada = character(),
    dimension = character(), factor = character(), granularidad = character(),
    tipo_resultado = character(), entidad = character(), atributo = character(),
    fila = integer(), objeto_medible = character(), n_elementos = integer(),
    resultado = numeric(), agregacion = character(),
    stringsAsFactors = FALSE
  )
  class(resultado) <- c("historico_calidad", "data.frame")
  attr(resultado, "version_esquema") <- .version_esquema_historico
  resultado
}

.fecha_utc <- function(x) {
  if (inherits(x, "Date")) {
    resultado <- as.POSIXct(x, tz = "UTC")
    attr(resultado, "tzone") <- "UTC"
    return(resultado)
  }
  x <- as.POSIXct(x)
  resultado <- as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
  attr(resultado, "tzone") <- "UTC"
  resultado
}

.valor_columna <- function(x, nombre, tipo = c("texto", "entero")) {
  tipo <- match.arg(tipo)
  if (!nombre %in% names(x)) {
    return(if (tipo == "entero") rep(NA_integer_, nrow(x)) else {
      rep(NA_character_, nrow(x))
    })
  }
  if (tipo == "entero") {
    as.integer(x[[nombre]])
  } else {
    as.character(.texto_analizable(x[[nombre]])$valores)
  }
}

.parte_historico <- function(x, nivel, id_registro, perfil = NA_character_,
                             regla = NA_character_, n_elementos = 1L) {
  n <- nrow(x)
  if (length(perfil) == 1L) perfil <- rep(perfil, n)
  if (length(regla) == 1L) regla <- rep(regla, n)
  if (length(n_elementos) == 1L) n_elementos <- rep(n_elementos, n)
  data.frame(
    version_esquema = rep(.version_esquema_historico, n),
    nivel = rep(nivel, n), id_registro = id_registro,
    id_medida = .valor_columna(x, "id_medida"),
    id_medicion = .valor_columna(x, "id_medicion"),
    fecha = .fecha_utc(x$fecha), perfil = as.character(perfil),
    regla = as.character(regla), metrica = .valor_columna(x, "metrica"),
    metrica_especifica = .valor_columna(x, "metrica_especifica"),
    metrica_instanciada = .valor_columna(x, "metrica_instanciada"),
    dimension = .valor_columna(x, "dimension"),
    factor = .valor_columna(x, "factor"),
    granularidad = .valor_columna(x, "granularidad"),
    tipo_resultado = .valor_columna(x, "tipo_resultado"),
    entidad = .valor_columna(x, "entidad"),
    atributo = .valor_columna(x, "atributo"),
    fila = .valor_columna(x, "fila", "entero"),
    objeto_medible = .valor_columna(x, "objeto_medible"),
    n_elementos = as.integer(n_elementos), resultado = as.numeric(x$resultado),
    agregacion = .valor_columna(x, "agregacion"),
    stringsAsFactors = FALSE
  )
}

.escapar_clave <- function(x) {
  x <- as.character(x)
  cod <- vapply(
    x,
    function(z) if (is.na(z)) NA_character_ else utils::URLencode(z, TRUE),
    character(1L), USE.NAMES = FALSE
  )
  ifelse(is.na(x), "~", paste0("=", cod))
}

.clave_historico <- function(nivel, id_medicion, perfil = NA_character_,
                             regla = NA_character_, id_medida = NA_character_) {
  paste(
    .escapar_clave(nivel), .escapar_clave(id_medicion),
    .escapar_clave(perfil), .escapar_clave(regla),
    .escapar_clave(id_medida), sep = "|"
  )
}

.normalizar_medicion_historico <- function(x) {
  if (!inherits(x, "medicion")) {
    stop("El objeto de medidas debe provenir de medir().", call. = FALSE)
  }
  .validar_medicion_evaluacion(x)
  clave <- .clave_historico("medida", x$id_medicion, id_medida = x$id_medida)
  .parte_historico(x, "medida", clave)
}

.validar_tabla_evaluacion <- function(x, requeridas, nombre) {
  if (!inherits(x, "data.frame") || !nrow(x) ||
      !all(requeridas %in% names(x)) || anyNA(x$id_medicion) ||
      anyNA(x$fecha) ||
      (!is.numeric(x$resultado) && !is.logical(x$resultado)) ||
      anyNA(x$resultado) ||
      any(!is.finite(x$resultado)) || any(x$resultado < 0 | x$resultado > 1)) {
    stop("La tabla `", nombre, "` de la evaluaci\u00f3n no cumple su contrato.",
         call. = FALSE)
  }
  x
}

.normalizar_evaluacion_historico <- function(x, detalle) {
  if (!inherits(x, "evaluacion_calidad")) {
    stop("El objeto de evaluaci\u00f3n debe provenir de evaluar().", call. = FALSE)
  }
  reglas <- .validar_tabla_evaluacion(
    x$reglas, c("id_medicion", "fecha", "perfil", "regla", "n_medidas",
                "resultado"), "reglas"
  )
  perfiles <- .validar_tabla_evaluacion(
    x$perfiles, c("id_medicion", "fecha", "perfil", "n_reglas", "resultado"),
    "perfiles"
  )
  partes <- list(
    .parte_historico(
      reglas, "evaluacion_regla",
      .clave_historico(
        "evaluacion_regla", reglas$id_medicion, reglas$perfil, reglas$regla
      ),
      perfil = reglas$perfil, regla = reglas$regla,
      n_elementos = reglas$n_medidas
    ),
    .parte_historico(
      perfiles, "evaluacion_perfil",
      .clave_historico(
        "evaluacion_perfil", perfiles$id_medicion, perfiles$perfil
      ),
      perfil = perfiles$perfil, n_elementos = perfiles$n_reglas
    )
  )
  if (detalle == "completo") {
    medidas <- .validar_tabla_evaluacion(
      x$medidas,
      c("id_medida", "id_medicion", "fecha", "perfil", "regla",
        "metrica_instanciada", "resultado"), "medidas"
    )
    partes <- c(list(.parte_historico(
      medidas, "evaluacion_medida",
      .clave_historico(
        "evaluacion_medida", medidas$id_medicion, medidas$perfil,
        medidas$regla, medidas$id_medida
      ),
      perfil = medidas$perfil, regla = medidas$regla
    )), partes)
  }
  do.call(rbind, partes)
}

.validar_historico <- function(x) {
  if (!inherits(x, "data.frame") ||
      !all(.columnas_historico %in% names(x))) {
    stop("`historico` no cumple el esquema tabular esperado.", call. = FALSE)
  }
  version <- unique(x$version_esquema)
  if (!length(version)) version <- attr(x, "version_esquema", exact = TRUE)
  if (length(version) != 1L || is.na(version) ||
      version != .version_esquema_historico) {
    stop(
      "Versi\u00f3n de esquema hist\u00f3rico no compatible: ",
      paste(version, collapse = ", "), ".", call. = FALSE
    )
  }
  if (nrow(x) &&
      (anyNA(x$id_registro) || any(!nzchar(x$id_registro)) ||
       anyNA(x$id_medicion) || any(!nzchar(x$id_medicion)) ||
       anyNA(x$fecha) || anyNA(x$resultado) ||
       any(!is.finite(x$resultado)))) {
    stop("El hist\u00f3rico contiene identificadores, fechas o resultados inv\u00e1lidos.",
         call. = FALSE)
  }
  if (nrow(x)) {
    medidas <- x$nivel == "medida"
    if (any(medidas) && !.resultados_validos_tipo(
      x$resultado[medidas], x$tipo_resultado[medidas]
    )) {
      stop("Los resultados de las medidas hist\u00f3ricas no respetan su tipo.",
           call. = FALSE)
    }
    evaluaciones <- !medidas
    if (any(evaluaciones) && any(
      x$resultado[evaluaciones] < 0 | x$resultado[evaluaciones] > 1
    )) {
      stop("Los resultados de evaluaciones hist\u00f3ricas deben estar en [0, 1].",
           call. = FALSE)
    }
  }
  niveles <- c(
    "medida", "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil"
  )
  if (nrow(x) && (any(!x$nivel %in% niveles) || anyDuplicated(x$id_registro))) {
    stop("El hist\u00f3rico contiene niveles o identificadores de registro duplicados.",
         call. = FALSE)
  }
  if (nrow(x)) {
    fechas <- split(as.numeric(x$fecha), x$id_medicion, drop = TRUE)
    if (any(vapply(fechas, function(z) length(unique(z)) != 1L, logical(1L)))) {
      stop("Cada `id_medicion` debe corresponder a una \u00fanica fecha.",
           call. = FALSE)
    }
  }
  x <- x[, .columnas_historico, drop = FALSE]
  x$fecha <- .fecha_utc(x$fecha)
  class(x) <- c("historico_calidad", "data.frame")
  attr(x, "version_esquema") <- .version_esquema_historico
  x
}

.combinar_historico <- function(anterior, nuevo) {
  anterior <- .validar_historico(anterior)
  nuevo <- .validar_historico(nuevo)
  coincidencias <- match(nuevo$id_registro, anterior$id_registro, nomatch = 0L)
  repetidos <- which(coincidencias > 0L)
  if (length(repetidos)) {
    iguales <- vapply(repetidos, function(i) {
      j <- coincidencias[[i]]
      isTRUE(all.equal(
        anterior[j, .columnas_historico, drop = FALSE],
        nuevo[i, .columnas_historico, drop = FALSE],
        check.attributes = FALSE
      ))
    }, logical(1L))
    if (any(!iguales)) {
      stop(
        "Un registro ya existente tiene contenido diferente: ",
        nuevo$id_registro[repetidos[which(!iguales)[[1L]]]], ".",
        call. = FALSE
      )
    }
  }
  agregar <- nuevo[coincidencias == 0L, , drop = FALSE]
  resultado <- if (nrow(agregar)) rbind(anterior, agregar) else anterior
  rownames(resultado) <- NULL
  .validar_historico(resultado)
}

.normalizar_objeto_historico <- function(x, detalle) {
  if (inherits(x, "historico_calidad")) return(.validar_historico(x))
  if (inherits(x, "medicion")) return(.normalizar_medicion_historico(x))
  if (inherits(x, "evaluacion_calidad")) {
    return(.normalizar_evaluacion_historico(x, detalle))
  }
  stop(
    "Cada objeto debe ser una medicion, evaluacion_calidad o historico_calidad.",
    call. = FALSE
  )
}

#' Construir y ampliar un histórico de calidad
#'
#' Crea un data frame plano y versionado con corridas producidas por [medir()] o
#' [evaluar()]. `acumular_historico()` agrega objetos al mismo esquema y es
#' idempotente cuando recibe otra vez registros idénticos.
#'
#' @param ... Objetos `medicion`, `evaluacion_calidad` o `historico_calidad`.
#'   También puede darse una única lista que los contenga.
#' @param detalle Para evaluaciones, `"resumen"` conserva los niveles de regla y
#'   perfil; `"completo"` conserva además cada evaluación de medida. Una
#'   `medicion` pasada explícitamente siempre se conserva completa.
#' @param historico Objeto creado por `historico_calidad()`.
#'
#' @return Data frame S3 `historico_calidad`. La columna `version_esquema` y el
#'   atributo del mismo nombre permiten migraciones futuras. `nivel` corresponde
#'   a `medida`, `evaluacion_medida`, `evaluacion_regla` o
#'   `evaluacion_perfil`.
#'
#' @details
#' El detalle predeterminado evita repetir una fila por celda y regla cuando el
#' objetivo es monitorear la serie de evaluaciones. El objeto no guarda modelos,
#' closures, datos originales ni perfiles de profiling. Esto mantiene la tabla
#' exportable directamente con `write.csv()` o una herramienta de base de datos.
#'
#' El esquema largo mapea las cuatro tablas de la sección 9.5 del marco mediante
#' `nivel`. Las columnas que no corresponden a un nivel quedan como `NA`.
#'
#' @references [AGESIC (2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
#'   *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, sección 9.5, Presidencia de la
#'   República, Uruguay.
#'
#' @export
#' @seealso [medir()], [evaluar()], [detectar_deriva_calidad()], [reportar()]
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
#' medidas <- medir(
#'   modelo(instancia), data.frame(edad = c(20, NA)),
#'   id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")
#' )
#' evaluacion <- evaluar(
#'   medidas,
#'   perfil_evaluacion("Basico", regla_evaluacion("Presente", function(x) x > 0))
#' )
#' historico_calidad(medidas, evaluacion)
historico_calidad <- function(..., detalle = c("resumen", "completo")) {
  detalle <- match.arg(detalle)
  objetos <- list(...)
  if (length(objetos) == 1L && is.list(objetos[[1L]]) &&
      !inherits(objetos[[1L]], c(
        "medicion", "evaluacion_calidad", "historico_calidad"
      ))) {
    objetos <- objetos[[1L]]
  }
  resultado <- .historico_vacio()
  for (objeto in objetos) {
    resultado <- .combinar_historico(
      resultado, .normalizar_objeto_historico(objeto, detalle)
    )
  }
  resultado
}

#' @rdname historico_calidad
#' @export
#' @seealso [historico_calidad()], [leer_historico()]
acumular_historico <- function(historico, ...,
                               detalle = c("resumen", "completo")) {
  detalle <- match.arg(detalle)
  resultado <- .validar_historico(historico)
  objetos <- list(...)
  if (length(objetos) == 1L && is.list(objetos[[1L]]) &&
      !inherits(objetos[[1L]], c(
        "medicion", "evaluacion_calidad", "historico_calidad"
      ))) {
    objetos <- objetos[[1L]]
  }
  for (objeto in objetos) {
    resultado <- .combinar_historico(
      resultado, .normalizar_objeto_historico(objeto, detalle)
    )
  }
  resultado
}

#' Guardar y recuperar un histórico de calidad
#'
#' Persiste el data frame versionado mediante RDS de base R. La escritura no
#' reemplaza un archivo existente salvo consentimiento explícito.
#'
#' @param historico Objeto creado por [historico_calidad()].
#' @param archivo Ruta del archivo RDS.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#'
#' @return `guardar_historico()` devuelve invisiblemente la ruta normalizada;
#'   `leer_historico()` devuelve un `historico_calidad` validado.
#' @export
#' @seealso [guardar_historico()], [detectar_deriva_calidad()]
#'
#' @examples
#' archivo <- tempfile(fileext = ".rds")
#' guardar_historico(historico_calidad(), archivo)
#' leer_historico(archivo)
guardar_historico <- function(historico, archivo, sobrescribir = FALSE) {
  historico <- .validar_historico(historico)
  if (!.es_texto_escalar(archivo)) {
    stop("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    stop("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- dirname(archivo)
  if (!dir.exists(directorio)) {
    stop("No existe el directorio de destino: ", directorio, ".", call. = FALSE)
  }
  if (file.exists(archivo) && !sobrescribir) {
    stop("El archivo ya existe; use `sobrescribir = TRUE` para reemplazarlo.",
         call. = FALSE)
  }
  temporal <- tempfile(".lupa-historico-", tmpdir = directorio)
  on.exit(unlink(temporal), add = TRUE)
  saveRDS(historico, temporal, version = 3L)
  if (!file.copy(temporal, archivo, overwrite = sobrescribir)) {
    stop("No se pudo guardar el hist\u00f3rico en el destino indicado.", call. = FALSE)
  }
  invisible(normalizePath(archivo, mustWork = TRUE))
}

#' @rdname guardar_historico
#' @export
#' @seealso [historico_calidad()], [comparar_evaluaciones()]
leer_historico <- function(archivo) {
  if (!.es_texto_escalar(archivo) || !file.exists(archivo)) {
    stop("`archivo` debe identificar un RDS existente.", call. = FALSE)
  }
  objeto <- readRDS(archivo)
  .validar_historico(objeto)
}

#' Detectar deriva en una serie de evaluaciones
#'
#' Compara corridas consecutivas, ordenadas por fecha dentro de cada perfil o
#' regla, y marca cambios significativos en la escala `[0, 1]`.
#'
#' @param historico Objeto creado por [historico_calidad()].
#' @param nivel `"perfil"` o `"regla"`.
#' @param umbral Cambio absoluto mínimo considerado significativo. El valor
#'   predeterminado de `0.05` representa cinco puntos porcentuales: evita tratar
#'   como deriva diferencias de redondeo, pero sigue siendo sensible a cambios
#'   operativamente visibles.
#'
#' @return Data frame `deriva_calidad` con una fila por par de corridas
#'   consecutivas. Una mejora significativa conserva severidad `ok`; un
#'   deterioro de al menos un umbral es `sospechoso` y uno de al menos dos
#'   umbrales es `error`.
#' @export
#'
#' @examples
#' # El ejemplo de historico_calidad() muestra cómo construir las corridas.
detectar_deriva_calidad <- function(historico, nivel = c("perfil", "regla"),
                                    umbral = 0.05) {
  historico <- .validar_historico(historico)
  nivel <- match.arg(nivel)
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral <= 0 || umbral > 1) {
    stop("`umbral` debe ser un n\u00famero en (0, 1].", call. = FALSE)
  }
  nombre_nivel <- paste0("evaluacion_", nivel)
  datos <- historico[historico$nivel == nombre_nivel, , drop = FALSE]
  columnas <- c(
    "nivel", "perfil", "regla", "id_medicion_anterior", "fecha_anterior",
    "resultado_anterior", "id_medicion_actual", "fecha_actual",
    "resultado_actual", "delta", "cambio_absoluto", "significativo",
    "direccion", "severidad"
  )
  vacio <- data.frame(
    nivel = character(), perfil = character(), regla = character(),
    id_medicion_anterior = character(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    resultado_anterior = numeric(), id_medicion_actual = character(),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    resultado_actual = numeric(), delta = numeric(), cambio_absoluto = numeric(),
    significativo = logical(), direccion = character(), severidad = character(),
    stringsAsFactors = FALSE
  )
  if (!nrow(datos)) {
    stop("El hist\u00f3rico no contiene evaluaciones en el nivel solicitado.",
         call. = FALSE)
  }
  clave <- if (nivel == "perfil") datos$perfil else {
    paste(datos$perfil, datos$regla, sep = "\034")
  }
  grupos <- split(seq_len(nrow(datos)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    orden <- order(datos$fecha[indices], datos$id_medicion[indices])
    indices <- indices[orden]
    if (length(indices) < 2L) return(vacio)
    a <- indices[-length(indices)]
    b <- indices[-1L]
    delta <- datos$resultado[b] - datos$resultado[a]
    significativo <- abs(delta) >= umbral
    direccion <- ifelse(
      delta >= umbral, "mejora",
      ifelse(delta <= -umbral, "deterioro", "estable")
    )
    severidad <- ifelse(
      delta <= -2 * umbral, "error",
      ifelse(delta <= -umbral, "sospechoso", "ok")
    )
    data.frame(
      nivel = rep(nivel, length(a)), perfil = datos$perfil[a],
      regla = if (nivel == "regla") datos$regla[a] else NA_character_,
      id_medicion_anterior = datos$id_medicion[a], fecha_anterior = datos$fecha[a],
      resultado_anterior = datos$resultado[a],
      id_medicion_actual = datos$id_medicion[b], fecha_actual = datos$fecha[b],
      resultado_actual = datos$resultado[b], delta = delta,
      cambio_absoluto = abs(delta), significativo = significativo,
      direccion = direccion, severidad = severidad,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  resultado <- resultado[, columnas, drop = FALSE]
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("deriva_calidad", "data.frame")
  resultado
}
