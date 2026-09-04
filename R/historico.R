# La misma validacion estaba escrita tres veces —aqui, en `reportar.R` y en
# `analisis.R`— y ya habia divergido en tres cosas a la vez: el orden de las
# comprobaciones, si el mensaje nombra el directorio que falta, y la redaccion
# del aviso de sobrescritura. La de `analisis.R` decia "No existe el directorio
# de destino." sin decir cual, que es justo el dato que necesita quien lo lee.
#
# Devuelve el directorio porque las tres lo usan despues para el archivo
# temporal: escribir primero al lado del destino y copiar es lo que evita dejar
# un archivo a medias si algo falla.
.validar_destino_archivo <- function(archivo, sobrescribir) {
  directorio <- dirname(archivo)
  if (!dir.exists(directorio)) {
    stop("No existe el directorio de destino: ", directorio, ".", call. = FALSE)
  }
  if (file.exists(archivo) && !sobrescribir) {
    stop("El archivo ya existe; use `sobrescribir = TRUE` para reemplazarlo.",
         call. = FALSE)
  }
  directorio
}

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
  attr(resultado, "configuracion_evaluacion") <-
    .configuraciones_historico_vacias()
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

.columnas_configuracion_historico <- c(
  "id_medicion", "fecha", "perfil", "identidad_tabla",
  "configuracion_modelo", "configuracion_aplicabilidad",
  "configuracion_perfil"
)

.configuraciones_historico_vacias <- function() {
  data.frame(
    id_medicion = character(), fecha = as.POSIXct(character(), tz = "UTC"),
    perfil = character(), identidad_tabla = character(),
    configuracion_modelo = character(),
    configuracion_aplicabilidad = character(),
    configuracion_perfil = character(), stringsAsFactors = FALSE
  )
}

.fila_configuracion_historico <- function(ids, fechas, perfiles,
                                          configuracion_modelo = NULL,
                                          configuracion_aplicabilidad = NULL,
                                          configuracion_perfil = NULL) {
  ids <- as.character(ids)
  if (!length(ids) || (
    is.null(configuracion_modelo) &&
      is.null(configuracion_aplicabilidad) &&
      is.null(configuracion_perfil)
  )) return(.configuraciones_historico_vacias())
  if (length(fechas) == 1L) fechas <- rep(fechas, length(ids))
  if (length(perfiles) == 1L) perfiles <- rep(perfiles, length(ids))
  entidades <- if (is.list(configuracion_modelo) &&
                   length(configuracion_modelo$entidades)) {
    paste(sort(as.character(configuracion_modelo$entidades)), collapse = "+")
  } else NA_character_
  data.frame(
    id_medicion = ids, fecha = .fecha_utc(fechas), perfil = as.character(perfiles),
    identidad_tabla = rep(entidades, length.out = length(ids)),
    configuracion_modelo = rep(
      if (is.null(configuracion_modelo)) NA_character_ else
        .texto_configuracion_calidad(configuracion_modelo),
      length.out = length(ids)
    ),
    configuracion_aplicabilidad = rep(
      if (is.null(configuracion_aplicabilidad)) NA_character_ else
        as.character(configuracion_aplicabilidad),
      length.out = length(ids)
    ),
    configuracion_perfil = rep(
      if (is.null(configuracion_perfil)) NA_character_ else
        .texto_configuracion_calidad(configuracion_perfil),
      length.out = length(ids)
    ), stringsAsFactors = FALSE
  )
}

.configuracion_historico_medicion <- function(x) {
  configuracion <- attr(x, "configuracion_modelo", exact = TRUE)
  aplicabilidad <- attr(x, "configuracion_aplicabilidad", exact = TRUE)
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  ids <- unique(c(
    as.character(x$id_medicion),
    if (inherits(cobertura, "data.frame")) as.character(cobertura$id_medicion)
  ))
  fechas <- if (length(ids) && nrow(x)) {
    x$fecha[match(ids, x$id_medicion)]
  } else if (length(ids) && inherits(cobertura, "data.frame")) {
    cobertura$fecha[match(ids, cobertura$id_medicion)]
  } else {
    as.POSIXct(character())
  }
  .fila_configuracion_historico(
    ids, fechas, NA_character_, configuracion, aplicabilidad
  )
}

.configuracion_historico_evaluacion <- function(x) {
  configuracion <- attr(x, "configuracion_modelo", exact = TRUE)
  aplicabilidad <- attr(x, "configuracion_aplicabilidad", exact = TRUE)
  perfil <- attr(x, "configuracion_perfil", exact = TRUE)
  fuente <- x$perfiles
  if (!inherits(fuente, "data.frame") || !nrow(fuente)) fuente <- x$reglas
  if (!inherits(fuente, "data.frame") || !nrow(fuente)) {
    cobertura <- x$cobertura_metricas
    if (!inherits(cobertura, "data.frame")) {
      return(.configuraciones_historico_vacias())
    }
    ids <- unique(as.character(cobertura$id_medicion))
    fechas <- cobertura$fecha[match(ids, cobertura$id_medicion)]
    perfiles <- NA_character_
  } else {
    ids <- unique(as.character(fuente$id_medicion))
    fechas <- fuente$fecha[match(ids, fuente$id_medicion)]
    nombre_perfil <- if (inherits(x$perfiles, "data.frame") &&
                         nrow(x$perfiles)) {
      x$perfiles$perfil[[1L]]
    } else {
      x$reglas$perfil[[1L]]
    }
    perfiles <- rep(nombre_perfil, length(ids))
  }
  .fila_configuracion_historico(
    ids, fechas, perfiles, configuracion, aplicabilidad, perfil
  )
}

.validar_configuraciones_historico <- function(x) {
  if (is.null(x)) return(.configuraciones_historico_vacias())
  if (!inherits(x, "data.frame") ||
      !all(.columnas_configuracion_historico %in% names(x))) {
    stop("La configuraci\u00f3n del hist\u00f3rico no cumple su esquema tabular.",
         call. = FALSE)
  }
  x <- x[.columnas_configuracion_historico]
  if (nrow(x) && (
    anyNA(x$id_medicion) || any(!nzchar(x$id_medicion)) || anyNA(x$fecha) ||
      anyDuplicated(paste(x$id_medicion, x$perfil, sep = "\034"))
  )) {
    stop("La configuraci\u00f3n del hist\u00f3rico contiene registros inv\u00e1lidos o duplicados.",
         call. = FALSE)
  }
  x$fecha <- .fecha_utc(x$fecha)
  x
}

.combinar_configuraciones_historico <- function(anterior, nuevo) {
  anterior <- .validar_configuraciones_historico(anterior)
  nuevo <- .validar_configuraciones_historico(nuevo)
  if (!nrow(anterior)) return(nuevo)
  if (!nrow(nuevo)) return(anterior)
  clave_anterior <- paste(anterior$id_medicion, anterior$perfil, sep = "\034")
  clave_nuevo <- paste(nuevo$id_medicion, nuevo$perfil, sep = "\034")
  compartidas <- intersect(clave_nuevo, clave_anterior)
  for (clave in compartidas) {
    i <- match(clave, clave_nuevo)
    j <- match(clave, clave_anterior)
    if (!isTRUE(all.equal(nuevo[i, , drop = FALSE], anterior[j, , drop = FALSE],
                         check.attributes = FALSE))) {
      stop(
        "Una corrida ya existente tiene una configuraci\u00f3n diferente: ",
        nuevo$id_medicion[[i]], ".", call. = FALSE
      )
    }
  }
  resultado <- rbind(
    anterior,
    nuevo[!clave_nuevo %in% clave_anterior, , drop = FALSE]
  )
  rownames(resultado) <- NULL
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
  .validar_medicion_evaluacion(x, permitir_suprimidas = TRUE)
  clave <- .clave_historico("medida", x$id_medicion, id_medida = x$id_medida)
  resultado <- .parte_historico(x, "medida", clave)
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    resultado <- rbind(
      resultado, .parte_historico_metricas_no_evaluadas(cobertura)
    )
  }
  attr(resultado, "configuracion_evaluacion") <-
    .configuracion_historico_medicion(x)
  resultado
}

.validar_tabla_evaluacion <- function(x, requeridas, nombre,
                                     permitir_na = FALSE) {
  if (!inherits(x, "data.frame") || !nrow(x) ||
      !all(requeridas %in% names(x)) || anyNA(x$id_medicion) ||
      anyNA(x$fecha) ||
      (!is.numeric(x$resultado) && !is.logical(x$resultado)) ||
      (!isTRUE(permitir_na) && anyNA(x$resultado)) ||
      any(!is.na(x$resultado) & !is.finite(x$resultado)) ||
      any(!is.na(x$resultado) &
          (x$resultado < 0 | x$resultado > 1))) {
    stop("La tabla `", nombre, "` de la evaluaci\u00f3n no cumple su contrato.",
         call. = FALSE)
  }
  x
}

.normalizar_evaluacion_historico <- function(x, detalle) {
  if (!inherits(x, "evaluacion_calidad")) {
    stop("El objeto de evaluaci\u00f3n debe provenir de evaluar().", call. = FALSE)
  }
  cobertura <- x$cobertura_metricas
  tiene_cobertura <- inherits(cobertura, "data.frame") && nrow(cobertura)
  reglas <- .validar_tabla_evaluacion(
    x$reglas, c("id_medicion", "fecha", "perfil", "regla", "n_medidas",
                "resultado"), "reglas", permitir_na = tiene_cobertura
  )
  perfiles <- .validar_tabla_evaluacion(
    x$perfiles, c("id_medicion", "fecha", "perfil", "n_reglas", "resultado"),
    "perfiles", permitir_na = tiene_cobertura
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
  if (tiene_cobertura) {
    partes <- c(partes, list(.parte_historico_metricas_no_evaluadas(cobertura)))
  }
  resultado <- do.call(rbind, partes)
  attr(resultado, "configuracion_evaluacion") <-
    .configuracion_historico_evaluacion(x)
  resultado
}

.parte_historico_metricas_no_evaluadas <- function(cobertura) {
  n <- nrow(cobertura)
  data.frame(
    version_esquema = rep(.version_esquema_historico, n),
    nivel = rep("metrica_no_evaluada", n),
    id_registro = .clave_historico(
      "metrica_no_evaluada", cobertura$id_medicion,
      cobertura$metrica_instanciada
    ),
    id_medida = rep(NA_character_, n),
    id_medicion = as.character(cobertura$id_medicion),
    fecha = .fecha_utc(cobertura$fecha),
    perfil = rep(NA_character_, n), regla = rep(NA_character_, n),
    metrica = as.character(cobertura$metrica),
    metrica_especifica = as.character(cobertura$metrica_especifica),
    metrica_instanciada = as.character(cobertura$metrica_instanciada),
    dimension = rep(NA_character_, n), factor = rep(NA_character_, n),
    granularidad = rep(NA_character_, n), tipo_resultado = rep(NA_character_, n),
    entidad = as.character(cobertura$entidad),
    atributo = as.character(cobertura$atributo), fila = rep(NA_integer_, n),
    objeto_medible = paste0("M\u00e9trica no evaluada: ", cobertura$motivo),
    n_elementos = rep(NA_integer_, n), resultado = rep(NA_real_, n),
    agregacion = as.character(cobertura$estado), stringsAsFactors = FALSE
  )
}

.validar_historico <- function(x) {
  if (!inherits(x, "data.frame") ||
      !all(.columnas_historico %in% names(x))) {
    stop("`historico` no cumple el esquema tabular esperado.", call. = FALSE)
  }
  configuracion <- attr(x, "configuracion_evaluacion", exact = TRUE)
  x <- .tabla_base(x)
  configuracion <- .validar_configuraciones_historico(configuracion)
  version <- unique(x$version_esquema)
  if (!length(version)) version <- attr(x, "version_esquema", exact = TRUE)
  if (length(version) != 1L || is.na(version) ||
      version != .version_esquema_historico) {
    stop(
      "Versi\u00f3n de esquema hist\u00f3rico no compatible: ",
      paste(version, collapse = ", "), ".", call. = FALSE
    )
  }
  ids_incompletos <- if (nrow(x)) {
    unique(x$id_medicion[x$nivel == "metrica_no_evaluada"])
  } else character()
  valores_suprimidos <- if (nrow(x) && "objeto_medible" %in% names(x)) {
    !is.na(x$objeto_medible) &
      grepl("[valor suprimido]", x$objeto_medible, fixed = TRUE)
  } else {
    rep(FALSE, nrow(x))
  }
  niveles_evaluacion <- x$nivel %in% c(
    "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil"
  )
  if (nrow(x) &&
      (anyNA(x$id_registro) || any(!nzchar(x$id_registro)) ||
       anyNA(x$id_medicion) || any(!nzchar(x$id_medicion)) ||
       anyNA(x$fecha) ||
       any(is.na(x$resultado) & !(
         x$nivel == "metrica_no_evaluada" |
           niveles_evaluacion & x$id_medicion %in% ids_incompletos |
           x$nivel == "medida" & valores_suprimidos
       )))) {
    stop("El hist\u00f3rico contiene identificadores, fechas o resultados inv\u00e1lidos.",
         call. = FALSE)
  }
  if (nrow(x)) {
    medidas <- x$nivel == "medida"
    medidas_validas <- medidas & !valores_suprimidos
    if (any(medidas) && !.resultados_validos_tipo(
      x$resultado[medidas_validas], x$tipo_resultado[medidas_validas]
    )) {
      stop("Los resultados de las medidas hist\u00f3ricas no respetan su tipo.",
           call. = FALSE)
    }
    evaluaciones <- x$nivel %in% c(
      "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil"
    )
    if (any(evaluaciones) && any(
      !is.na(x$resultado[evaluaciones]) &
        (x$resultado[evaluaciones] < 0 | x$resultado[evaluaciones] > 1)
    )) {
      stop("Los resultados de evaluaciones hist\u00f3ricas deben estar en [0, 1].",
           call. = FALSE)
    }
  }
  niveles <- c(
    "medida", "evaluacion_medida", "evaluacion_regla", "evaluacion_perfil",
    "metrica_no_evaluada"
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
  x <- .seleccionar_columnas(x, .columnas_historico)
  x$fecha <- .fecha_utc(x$fecha)
  class(x) <- c("historico_calidad", "data.frame")
  attr(x, "version_esquema") <- .version_esquema_historico
  attr(x, "configuracion_evaluacion") <- configuracion
  x
}

.combinar_historico <- function(anterior, nuevo) {
  anterior <- .validar_historico(anterior)
  nuevo <- .validar_historico(nuevo)
  configuracion <- .combinar_configuraciones_historico(
    attr(anterior, "configuracion_evaluacion", exact = TRUE),
    attr(nuevo, "configuracion_evaluacion", exact = TRUE)
  )
  coincidencias <- match(nuevo$id_registro, anterior$id_registro, nomatch = 0L)
  repetidos <- which(coincidencias > 0L)
  if (length(repetidos)) {
    iguales <- vapply(repetidos, function(i) {
      j <- coincidencias[[i]]
      isTRUE(all.equal(
        .seleccionar_columnas(anterior, .columnas_historico, filas = j),
        .seleccionar_columnas(nuevo, .columnas_historico, filas = i),
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
  attr(resultado, "configuracion_evaluacion") <- configuracion
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

.proteger_historico_desenlaces <- function(x, desenlaces) {
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(c("nivel", "resultado", "objeto_medible") %in% names(x))) {
    return(x)
  }
  medidas <- x$nivel == "medida"
  if (!any(medidas)) return(x)
  suprimidas <- rep(FALSE, nrow(x))
  suprimidas[medidas] <- .filas_desenlaces(
    x[medidas, , drop = FALSE], desenlaces
  )
  if (any(suprimidas)) {
    x$resultado[suprimidas] <- NA_real_
    x$objeto_medible <- as.character(x$objeto_medible)
    ya_marcadas <- !is.na(x$objeto_medible) & grepl(
      "[valor suprimido]", x$objeto_medible, fixed = TRUE
    )
    nuevas <- suprimidas & !ya_marcadas
    x$objeto_medible[nuevas] <- paste0(
      x$objeto_medible[nuevas], " [valor suprimido]"
    )
  }
  x
}

.desenlaces_historico <- function(objetos) {
  partes <- lapply(objetos, .desenlaces_de_objeto)
  partes <- partes[vapply(partes, function(x) {
    inherits(x, "data.frame") && nrow(x)
  }, logical(1L))]
  if (!length(partes)) return(NULL)
  resultado <- do.call(rbind, partes)
  resultado[
    !duplicated(resultado[c("id_medicion", "id_medida", "regla")]),
    , drop = FALSE
  ]
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
#'   `evaluacion_perfil`; una métrica sin valores se conserva como
#'   `metrica_no_evaluada` con su motivo. El atributo
#'   `configuracion_evaluacion` conserva, en una tabla plana separada, el
#'   modelo, la aplicabilidad, el perfil y la identidad de tabla de cada corrida.
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
  resultado <- .proteger_historico_desenlaces(
    resultado, .desenlaces_historico(objetos)
  )
  .validar_historico(resultado)
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
  resultado <- .proteger_historico_desenlaces(
    resultado, .desenlaces_historico(objetos)
  )
  .validar_historico(resultado)
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
#' unlink(archivo)
guardar_historico <- function(historico, archivo, sobrescribir = FALSE) {
  historico <- .validar_historico(historico)
  if (!.es_texto_escalar(archivo)) {
    stop("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    stop("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- .validar_destino_archivo(archivo, sobrescribir)
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
#'   umbrales es `error`. `identidad_tabla` separa series de tablas distintas y
#'   `aspecto` marca el resultado o un cambio de configuración; este último se
#'   informa como `error` pero no suprime la comparación.
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
    "nivel", "perfil", "regla", "identidad_tabla",
    "id_medicion_anterior", "fecha_anterior", "resultado_anterior",
    "id_medicion_actual", "fecha_actual", "resultado_actual", "delta",
    "cambio_absoluto", "significativo", "direccion", "severidad", "aspecto",
    "descripcion", "evidencia"
  )
  vacio <- data.frame(
    nivel = character(), perfil = character(), regla = character(),
    identidad_tabla = character(),
    id_medicion_anterior = character(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    resultado_anterior = numeric(), id_medicion_actual = character(),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    resultado_actual = numeric(), delta = numeric(), cambio_absoluto = numeric(),
    significativo = logical(), direccion = character(), severidad = character(),
    aspecto = character(), descripcion = character(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  if (!nrow(datos)) {
    stop("El hist\u00f3rico no contiene evaluaciones en el nivel solicitado.",
         call. = FALSE)
  }
  configuraciones <- attr(historico, "configuracion_evaluacion", exact = TRUE)
  clave_configuracion <- function(ids, perfiles) {
    paste(
      as.character(ids),
      ifelse(is.na(perfiles), "~", as.character(perfiles)), sep = "\034"
    )
  }
  claves_datos <- clave_configuracion(datos$id_medicion, datos$perfil)
  claves_configuraciones <- if (nrow(configuraciones)) {
    clave_configuracion(configuraciones$id_medicion, configuraciones$perfil)
  } else character()
  indices_configuracion <- match(claves_datos, claves_configuraciones)
  identidad <- rep(NA_character_, nrow(datos))
  if (length(indices_configuracion)) {
    identidad <- configuraciones$identidad_tabla[indices_configuracion]
  }
  identidad[is.na(identidad) | !nzchar(identidad)] <- "<sin_configuracion>"
  clave <- if (nivel == "perfil") datos$perfil else {
    paste(datos$perfil, datos$regla, sep = "\034")
  }
  clave <- paste(clave, identidad, sep = "\034")
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
    regular <- data.frame(
      nivel = rep(nivel, length(a)), perfil = datos$perfil[a],
      regla = if (nivel == "regla") datos$regla[a] else NA_character_,
      identidad_tabla = identidad[a],
      id_medicion_anterior = datos$id_medicion[a], fecha_anterior = datos$fecha[a],
      resultado_anterior = datos$resultado[a],
      id_medicion_actual = datos$id_medicion[b], fecha_actual = datos$fecha[b],
      resultado_actual = datos$resultado[b], delta = delta,
      cambio_absoluto = abs(delta), significativo = significativo,
      direccion = direccion, severidad = severidad, aspecto = "resultado",
      # La fila existe por cada par consecutivo, cambie o no, asi que el texto
      # no puede afirmar un cambio: con dos corridas identicas decia "Cambio el
      # resultado" al lado de `delta = 0`, y una descripcion que contradice a su
      # propio dato es peor que no tenerla.
      descripcion = if (isTRUE(delta == 0)) {
        "El resultado de la evaluaci\u00f3n se mantuvo."
      } else {
        "Cambi\u00f3 el resultado de la evaluaci\u00f3n."
      },
      evidencia = NA_character_,
      stringsAsFactors = FALSE
    )
    if (!nrow(configuraciones)) return(regular)
    anterior_configuracion <- indices_configuracion[a]
    actual_configuracion <- indices_configuracion[b]
    campos <- c(
      modelo = "configuracion_modelo",
      aplicabilidad = "configuracion_aplicabilidad",
      perfil = "configuracion_perfil"
    )
    cambios_configuracion <- lapply(names(campos), function(nombre) {
      campo <- unname(campos[[nombre]])
      anterior <- rep(NA_character_, length(a))
      actual <- rep(NA_character_, length(b))
      validos_a <- !is.na(anterior_configuracion)
      validos_b <- !is.na(actual_configuracion)
      anterior[validos_a] <- configuraciones[[campo]][anterior_configuracion[validos_a]]
      actual[validos_b] <- configuraciones[[campo]][actual_configuracion[validos_b]]
      distintos <- (is.na(anterior) & !is.na(actual)) |
        (!is.na(anterior) & is.na(actual)) |
        (!is.na(anterior) & !is.na(actual) & anterior != actual)
      if (!any(distintos)) return(NULL)
      i <- which(distintos)
      data.frame(
        nivel = rep(nivel, length(i)), perfil = datos$perfil[a[i]],
        regla = if (nivel == "regla") datos$regla[a[i]] else NA_character_,
        identidad_tabla = identidad[a[i]],
        id_medicion_anterior = datos$id_medicion[a[i]],
        fecha_anterior = datos$fecha[a[i]], resultado_anterior = NA_real_,
        id_medicion_actual = datos$id_medicion[b[i]],
        fecha_actual = datos$fecha[b[i]], resultado_actual = NA_real_,
        delta = NA_real_, cambio_absoluto = NA_real_, significativo = TRUE,
        direccion = "configuracion", severidad = "error",
        aspecto = paste0("configuracion_", nombre),
        descripcion = paste(
          switch(
            nombre,
            modelo = "Cambi\u00f3 el modelo de calidad de la corrida;",
            aplicabilidad = "Cambi\u00f3 la aplicabilidad de la corrida;",
            perfil = "Cambi\u00f3 el perfil de evaluaci\u00f3n de la corrida;"
          ),
          "se mantienen las comparaciones para que la deriva de datos no quede",
          "oculta."
        ),
        evidencia = paste0(
          "Anterior: ", ifelse(is.na(anterior[i]), "no declarada", anterior[i]),
          "; actual: ", ifelse(is.na(actual[i]), "no declarada", actual[i]), "."
        ), stringsAsFactors = FALSE
      )
    })
    cambios_configuracion <- cambios_configuracion[
      !vapply(cambios_configuracion, is.null, logical(1L))
    ]
    if (length(cambios_configuracion)) {
      return(rbind(regular, do.call(rbind, cambios_configuracion)))
    }
    regular
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
