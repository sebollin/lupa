.plan_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    estrategia = character(), recomendada = logical(),
    justificacion = character(), n_afectadas = numeric(),
    reversible = logical(), estado = character(), aplicar = logical(),
    orden = integer(), stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

.nueva_accion <- function(columna, hallazgo, estrategia, recomendada,
                          justificacion, n_afectadas, reversible,
                          estado = "lista", aplicar = recomendada,
                          parametros = list(), orden = 500L) {
  estructura <- data.frame(
    id_accion = "", columna = columna, hallazgo = hallazgo,
    estrategia = estrategia, recomendada = recomendada,
    justificacion = justificacion,
    n_afectadas = as.numeric(n_afectadas), reversible = reversible,
    estado = estado, aplicar = aplicar, orden = as.integer(orden),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list(parametros))
  estructura
}

.fila_perfil <- function(perfil, columna) {
  indices <- which(perfil$columnas$columna == columna)
  if (length(indices) == 1L) perfil$columnas[indices, , drop = FALSE] else NULL
}

.formatos_perfil <- function(perfil, columna) {
  indices <- which(perfil$columnas$columna == columna)
  if (length(indices) == 1L) perfil$formatos_fecha[[indices]] else NULL
}

.accion_columna_ambigua <- function(perfil, columna) {
  sum(perfil$columnas$columna == columna) != 1L
}

.justificar_columna_ambigua <- function(texto, perfil, columna) {
  if (!.accion_columna_ambigua(perfil, columna)) {
    return(texto)
  }
  paste0(
    texto, " La acci\u00f3n queda bloqueada porque el nombre '", columna,
    "' no identifica una \u00fanica columna."
  )
}

.estado_columna <- function(perfil, columna, estado = "lista") {
  if (.accion_columna_ambigua(perfil, columna)) "bloqueada" else estado
}

.es_fecha_ambigua <- function(perfil, columna) {
  any(
    perfil$hallazgos$columna == columna &
      perfil$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo",
    na.rm = TRUE
  )
}

.es_fecha_mixta <- function(perfil, columna) {
  any(
    perfil$hallazgos$columna == columna &
      perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos",
    na.rm = TRUE
  )
}

.agregar_accion <- function(acciones, accion) {
  acciones[[length(acciones) + 1L]] <- accion
  acciones
}

#' Construir y aplicar un plan de limpieza auditable
#'
#' `planificar_limpieza()` transforma los hallazgos de un objeto `perfil` en un
#' objeto
#' de datos editable, sin modificar los datos examinados. Cada fila representa
#' una acción propuesta. Sólo se marcan como recomendadas y se activan de forma
#' predeterminada las transformaciones que no requieren conocimiento del
#' dominio. Las decisiones contextuales quedan desactivadas y los formatos de
#' fecha ambiguos quedan bloqueados.
#'
#' `aplicar()` ejecuta exclusivamente las filas con `aplicar == TRUE`, sobre una
#' copia de `datos`. Verifica que cada columna siga siendo identificable y que
#' las conversiones sean completas antes de sustituirla. Devuelve los datos
#' nuevos junto con un registro de las acciones y sus parámetros; el mismo
#' registro queda en el atributo `registro_limpieza` de los datos resultantes.
#'
#' El plan añade `id_accion`, `estado` y `orden` al contrato mínimo. `estado`
#' distingue acciones `lista`, `bloqueada` e `informativa`; así `aplicar = FALSE`
#' no confunde una decisión pendiente con una operación imposible. `orden` hace
#' explícita la secuencia reproducible. `n_afectadas` es la estimación obtenida
#' del perfil, mientras que el registro de aplicación informa `n_cambiadas`
#' calculado sobre los datos recibidos.
#'
#' La columna `reversible` indica si el resultado puede deshacerse sólo con los
#' datos transformados. Agregar una marca es reversible; recortar texto,
#' convertir tipos, normalizar ausencias o cambiar nombres no lo es, aunque el
#' registro conserve sus parámetros.
#'
#' @param perfil Objeto de clase `perfil` creado por [perfilar()].
#' @param plan Objeto de clase `plan_limpieza` o data frame con el mismo
#'   contrato. Puede filtrarse y editarse antes de aplicarlo.
#' @param datos `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el
#'   plan. El objeto recibido no se modifica.
#'
#' @return `planificar_limpieza()` devuelve un data frame de clase
#'   `plan_limpieza`. `aplicar()` devuelve una lista de clase
#'   `resultado_limpieza` con `datos`, `registro` y `plan_aplicado`.
#' @export
#'
#' @examples
#' datos <- data.frame(categoria = c(" A", "S/D", "B"))
#' perfil <- perfilar(datos)
#' plan <- planificar_limpieza(perfil)
#' plan[, c("estrategia", "recomendada", "aplicar")]
#' resultado <- aplicar(plan, datos)
#' resultado$datos
planificar_limpieza <- function(perfil) {
  if (!inherits(perfil, "perfil")) {
    stop("`perfil` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  acciones <- list()
  hallazgos <- perfil$hallazgos

  for (i in seq_len(nrow(hallazgos))) {
    hallazgo <- hallazgos[i, , drop = FALSE]
    tipo <- hallazgo$tipo_hallazgo[[1L]]
    columna <- hallazgo$columna[[1L]]
    fila <- if (!is.na(columna)) .fila_perfil(perfil, columna) else NULL
    estado_columna <- if (!is.na(columna)) {
      .estado_columna(perfil, columna)
    } else {
      "lista"
    }

    if (identical(tipo, "faltantes_disfrazados") && !is.null(fila)) {
      n_textuales <- fila$n_faltantes_disfrazados_textuales[[1L]]
      n_numericos <- fila$n_faltantes_disfrazados_numericos[[1L]]
      if (n_textuales > 0L) {
        justificacion <- .justificar_columna_ambigua(
          paste0(
            "Las representaciones textuales del cat\u00e1logo son marcadores ",
            "expl\u00edcitos de ausencia y pueden normalizarse sin inferir el dominio."
          ), perfil, columna
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_ausencias_textuales", TRUE,
          justificacion, n_textuales, FALSE,
          estado = estado_columna,
          aplicar = identical(estado_columna, "lista"),
          parametros = list(valores = .cadenas_na()), orden = 100L
        ))
      }
      if (n_numericos > 0L) {
        sentinelas <- perfil$meta$sentinelas_numericos
        if (is.null(sentinelas)) sentinelas <- .numeros_na()
        justificacion <- .justificar_columna_ambigua(
          paste0(
            "Un sentinela num\u00e9rico tambi\u00e9n puede ser un valor leg\u00edtimo; ",
            "requiere confirmar el diccionario del campo."
          ), perfil, columna
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_sentinelas_numericos", FALSE,
          justificacion, n_numericos, FALSE,
          estado = estado_columna, aplicar = FALSE,
          parametros = list(valores = sentinelas), orden = 110L
        ))
      }
    } else if (identical(tipo, "espacios_sobrantes") && !is.null(fila)) {
      justificacion <- .justificar_columna_ambigua(
        paste0(
          "Los espacios al borde no aportan contenido y separan categor\u00edas ",
          "que visualmente son iguales."
        ), perfil, columna
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "recortar_espacios", TRUE, justificacion,
        fila$n_espacios_borde[[1L]], FALSE, estado = estado_columna,
        aplicar = identical(estado_columna, "lista"), orden = 200L
      ))
    } else if (identical(tipo, "formato_fecha_ambiguo")) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "desambiguar_fecha_en_origen", FALSE,
        paste0(
          "Los datos no permiten elegir entre d\u00eda/mes y mes/d\u00eda; convertirlos ",
          "inventar\u00eda una interpretaci\u00f3n."
        ), if (is.null(fila)) NA_real_ else fila$n[[1L]], FALSE,
        estado = "bloqueada", aplicar = FALSE,
        parametros = list(candidatos = hallazgo$evidencia[[1L]]), orden = 300L
      ))
    } else if (identical(tipo, "formatos_fecha_mixtos") && !is.null(fila)) {
      formatos <- .formatos_perfil(perfil, columna)
      confirmados <- if (is.null(formatos)) character() else {
        formatos$formato[formatos$estado == "confirmado"]
      }
      seguro <- length(confirmados) >= 2L &&
        !any(formatos$estado != "confirmado") &&
        isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1) &&
        !.accion_columna_ambigua(perfil, columna)
      estado <- if (seguro) "lista" else "bloqueada"
      justificacion <- if (seguro) {
        paste0(
          "Todos los formatos presentes est\u00e1n confirmados por los datos y ",
          "pueden convertirse sin elegir entre candidatos ambiguos."
        )
      } else {
        paste0(
          "La conversi\u00f3n no es segura porque queda alg\u00fan formato candidato, ",
          "hay valores incompatibles o la columna no se identifica de manera \u00fanica."
        )
      }
      destino <- if (any(grepl("%H", confirmados, fixed = TRUE))) {
        "fecha-hora"
      } else {
        "fecha"
      }
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_fecha_confirmada", seguro,
        justificacion, fila$n[[1L]] - fila$n_faltantes[[1L]], FALSE,
        estado = estado, aplicar = seguro,
        parametros = list(formatos = confirmados, tipo = destino), orden = 300L
      ))
    } else if (identical(tipo, "tipo_declarado_distinto") && !is.null(fila) &&
               !.es_fecha_ambigua(perfil, columna) &&
               !.es_fecha_mixta(perfil, columna)) {
      destino <- fila$tipo_inferido[[1L]]
      soportado <- destino %in% c("entero", "doble", "logico", "fecha", "fecha-hora")
      compatible <- isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1)
      formatos <- .formatos_perfil(perfil, columna)
      confirmados <- if (is.null(formatos)) character() else {
        formatos$formato[formatos$estado == "confirmado"]
      }
      fecha_segura <- !destino %in% c("fecha", "fecha-hora") ||
        (length(confirmados) > 0L && !any(formatos$estado != "confirmado"))
      recomendar <- soportado && compatible && fecha_segura &&
        !.accion_columna_ambigua(perfil, columna)
      estado <- if (soportado && fecha_segura) estado_columna else "informativa"
      justificacion <- if (recomendar) {
        paste0(
          "Todos los valores presentes son compatibles con el tipo inferido; ",
          "la conversi\u00f3n no necesita decidir casos dudosos."
        )
      } else {
        paste0(
          "La conversi\u00f3n requiere compatibilidad total, un tipo con conversi\u00f3n ",
          "definida y, para fechas, un formato confirmado."
        )
      }
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_tipo", recomendar, justificacion,
        fila$n[[1L]] - fila$n_faltantes[[1L]], FALSE,
        estado = estado, aplicar = recomendar,
        parametros = list(tipo = destino, formatos = confirmados), orden = 310L
      ))
    } else if (identical(tipo, "filas_duplicadas")) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "marcar_filas_duplicadas", TRUE,
        paste0(
          "Marcar conserva todas las filas y hace visible cada repetici\u00f3n; ",
          "eliminarla exigir\u00eda una decisi\u00f3n sobre personas o registros."
        ), perfil$general$filas_duplicadas, TRUE, estado = "lista",
        aplicar = TRUE, parametros = list(columna_marca = ".fila_duplicada"),
        orden = 50L
      ))
    } else if (identical(tipo, "outliers") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_outliers", FALSE,
        paste0(
          "Un valor extremo puede ser correcto; la marca conserva el dato ",
          "para que el dominio decida c\u00f3mo tratarlo."
        ), fila$n_outliers[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        parametros = list(
          columna_marca = paste0(".outlier_", make.names(columna)),
          regla = "Tukey 1,5 x IQR"
        ), orden = 510L
      ))
    } else if (identical(tipo, "nombres_columnas_problematicos")) {
      nombres <- perfil$columnas$columna
      problema <- .nombres_columnas_problematicos(nombres)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "normalizar_nombres", TRUE,
        paste0(
          "Los nombres sint\u00e1cticos y \u00fanicos evitan referencias ambiguas sin ",
          "alterar el contenido de las columnas."
        ), nrow(problema), FALSE, estado = "lista", aplicar = TRUE,
        parametros = list(
          nombres_esperados = nombres,
          nombres_propuestos = make.names(nombres, unique = TRUE)
        ), orden = 900L
      ))
    } else if (tipo %in% c(
      "mayusculas_inconsistentes", "alta_cardinalidad", "constante",
      "columnas_duplicadas", "ceros_no_permitidos", "negativos_no_permitidos"
    )) {
      estrategias <- c(
        mayusculas_inconsistentes = "definir_capitalizacion",
        alta_cardinalidad = "revisar_cardinalidad",
        constante = "revisar_columna_constante",
        columnas_duplicadas = "revisar_columnas_duplicadas",
        ceros_no_permitidos = "revisar_ceros",
        negativos_no_permitidos = "revisar_negativos"
      )
      n <- if (is.null(fila)) NA_real_ else fila$n[[1L]]
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, estrategias[[tipo]], FALSE,
        paste0(
          "El perfil se\u00f1ala el problema, pero no contiene conocimiento ",
          "suficiente del dominio para elegir una transformaci\u00f3n."
        ), n, NA, estado = "informativa", aplicar = FALSE, orden = 800L
      ))
    }
  }

  if (!length(acciones)) {
    resultado <- .plan_vacio()
  } else {
    resultado <- do.call(rbind, acciones)
    rownames(resultado) <- NULL
    resultado$id_accion <- sprintf("accion-%04d", seq_len(nrow(resultado)))
  }
  resultado$estado <- factor(
    resultado$estado,
    levels = c("lista", "bloqueada", "informativa")
  )
  class(resultado) <- c("plan_limpieza", "data.frame")
  resultado
}

.columnas_plan <- function() {
  c(
    "id_accion", "columna", "hallazgo", "estrategia", "recomendada",
    "justificacion", "n_afectadas", "reversible", "estado", "aplicar",
    "orden", "parametros"
  )
}

.validar_plan_limpieza <- function(plan) {
  requeridas <- .columnas_plan()
  if (!inherits(plan, "data.frame") || !all(requeridas %in% names(plan))) {
    stop("`plan` no cumple el contrato de un plan de limpieza.", call. = FALSE)
  }
  if (!is.logical(plan$aplicar) || anyNA(plan$aplicar)) {
    stop("`plan$aplicar` debe ser un vector l\u00f3gico sin NA.", call. = FALSE)
  }
  if (anyDuplicated(plan$id_accion)) {
    stop("`plan$id_accion` debe contener identificadores \u00fanicos.", call. = FALSE)
  }
  if (!is.list(plan$parametros)) {
    stop("`plan$parametros` debe ser una columna de listas.", call. = FALSE)
  }
  estados <- as.character(plan$estado)
  if (any(plan$aplicar & estados != "lista")) {
    stop("S\u00f3lo se pueden aplicar acciones con estado 'lista'.", call. = FALSE)
  }
  invisible(plan)
}

#' @export
`[.plan_limpieza` <- function(x, ...) {
  resultado <- NextMethod("[")
  if (inherits(resultado, "data.frame") &&
      all(.columnas_plan() %in% names(resultado))) {
    class(resultado) <- unique(c("plan_limpieza", class(resultado)))
  } else if (inherits(resultado, "data.frame")) {
    class(resultado) <- setdiff(class(resultado), "plan_limpieza")
  }
  resultado
}

.copiar_datos <- function(datos) {
  if (inherits(datos, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::copy(datos))
  }
  datos
}

.indice_columna <- function(datos, columna) {
  indices <- which(names(datos) == columna)
  if (length(indices) != 1L) {
    stop(
      "La acci\u00f3n requiere una \u00fanica columna llamada '", columna,
      "'; se encontraron ", length(indices), ".", call. = FALSE
    )
  }
  indices[[1L]]
}

.reemplazar_ausencias_textuales <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La normalizaci\u00f3n textual requiere una columna de texto.", call. = FALSE)
  }
  normalizados <- tolower(trimws(as.character(x)))
  mascara <- !is.na(x) & normalizados %in% parametros$valores
  x[mascara] <- NA
  list(valor = x, n = sum(mascara))
}

.reemplazar_sentinelas_numericos <- function(x, parametros) {
  if (is.character(x) || is.factor(x)) {
    numeros <- suppressWarnings(as.numeric(trimws(as.character(x))))
  } else if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    numeros <- as.numeric(x)
  } else {
    stop("Los sentinelas num\u00e9ricos requieren texto o n\u00fameros.", call. = FALSE)
  }
  mascara <- !is.na(x) & !is.na(numeros) & numeros %in% parametros$valores
  x[mascara] <- NA
  list(valor = x, n = sum(mascara))
}

.recortar_texto <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("El recorte de espacios requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  nuevo <- trimws(anterior)
  mascara <- !is.na(anterior) & anterior != nuevo
  if (is.factor(x)) {
    niveles <- unique(trimws(levels(x)))
    nuevo <- factor(nuevo, levels = niveles, ordered = is.ordered(x))
  }
  list(valor = nuevo, n = sum(mascara))
}

.convertir_logico <- function(x) {
  texto <- tolower(trimws(as.character(x)))
  verdaderos <- c("true", "t", "si", "s\u00ed", "s", "1")
  falsos <- c("false", "f", "no", "n", "0")
  presentes <- !is.na(x)
  validos <- texto %in% c(verdaderos, falsos)
  if (any(presentes & !validos)) {
    stop("Hay valores presentes que no pueden convertirse a l\u00f3gico.", call. = FALSE)
  }
  salida <- rep(NA, length(x))
  salida[presentes] <- texto[presentes] %in% verdaderos
  salida
}

.convertir_fecha <- function(x, parametros) {
  formatos <- parametros$formatos
  if (!length(formatos)) {
    stop("La conversi\u00f3n de fecha requiere formatos confirmados.", call. = FALSE)
  }
  valores <- trimws(as.character(x))
  presentes <- !is.na(x)
  especificaciones <- .especificaciones_fecha()
  indices <- match(formatos, especificaciones$formato)
  if (anyNA(indices)) {
    stop("La conversi\u00f3n de fecha recibi\u00f3 un formato no reconocido.", call. = FALSE)
  }
  compatibles <- rep(FALSE, length(x))
  for (indice in indices) {
    compatibles <- compatibles | .es_fecha_valida(
      valores, especificaciones$formato[[indice]],
      especificaciones$expresion[[indice]]
    )
  }
  if (any(presentes & !compatibles)) {
    stop("Hay valores presentes que no responden a los formatos confirmados.", call. = FALSE)
  }
  tabla_formatos <- data.frame(
    formato = formatos, estado = rep("confirmado", length(formatos)),
    stringsAsFactors = FALSE
  )
  salida <- .parsear_fechas(x, tabla_formatos)
  if (any(presentes & is.na(salida))) {
    stop("Hay valores presentes que no responden a los formatos confirmados.", call. = FALSE)
  }
  if (identical(parametros$tipo, "fecha")) as.Date(salida) else salida
}

.convertir_tipo <- function(x, parametros) {
  tipo <- parametros$tipo
  presentes <- !is.na(x)
  if (tipo %in% c("fecha", "fecha-hora")) {
    return(.convertir_fecha(x, parametros))
  }
  if (identical(tipo, "logico")) {
    return(.convertir_logico(x))
  }
  texto <- sub(",", ".", trimws(as.character(x)), fixed = TRUE)
  numero <- suppressWarnings(as.numeric(texto))
  if (any(presentes & (!is.finite(numero) | is.na(numero)))) {
    stop("Hay valores presentes que no pueden convertirse a n\u00famero.", call. = FALSE)
  }
  if (identical(tipo, "doble")) {
    return(numero)
  }
  if (identical(tipo, "entero")) {
    limites <- c(-.Machine$integer.max - 1, .Machine$integer.max)
    validos <- !presentes |
      (abs(numero - round(numero)) < sqrt(.Machine$double.eps) &
         numero >= limites[[1L]] & numero <= limites[[2L]])
    if (!all(validos)) {
      stop("Hay valores presentes que no pueden representarse como enteros.", call. = FALSE)
    }
    return(as.integer(numero))
  }
  stop("No hay una conversi\u00f3n definida para el tipo '", tipo, "'.", call. = FALSE)
}

.marca_outliers <- function(x) {
  if (inherits(x, c("Date", "POSIXt")) || is.numeric(x)) {
    valores <- as.numeric(x)
  } else {
    valores <- suppressWarnings(as.numeric(
      sub(",", ".", trimws(as.character(x)), fixed = TRUE)
    ))
  }
  validos <- is.finite(valores)
  mascara <- rep(FALSE, length(x))
  if (!any(validos)) return(mascara)
  iqr <- stats::IQR(valores[validos], type = 7)
  cuartiles <- stats::quantile(
    valores[validos], c(0.25, 0.75), names = FALSE, type = 7
  )
  mascara[validos] <- valores[validos] < cuartiles[[1L]] - 1.5 * iqr |
    valores[validos] > cuartiles[[2L]] + 1.5 * iqr
  mascara
}

.agregar_marca <- function(datos, nombre, valor) {
  if (nombre %in% names(datos)) {
    stop("La columna de marca ya existe: ", nombre, ".", call. = FALSE)
  }
  datos[[nombre]] <- valor
  datos
}

.ejecutar_accion <- function(datos, accion) {
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  columna <- accion$columna[[1L]]

  if (identical(estrategia, "normalizar_nombres")) {
    esperados <- parametros$nombres_esperados
    if (length(names(datos)) < length(esperados) ||
        !identical(names(datos)[seq_along(esperados)], esperados)) {
      stop("Los nombres de los datos no coinciden con los usados por el perfil.", call. = FALSE)
    }
    anteriores <- names(datos)
    names(datos) <- make.names(anteriores, unique = TRUE)
    return(list(datos = datos, n = sum(anteriores != names(datos))))
  }
  if (identical(estrategia, "marcar_filas_duplicadas")) {
    marca <- duplicated(datos)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }

  indice <- .indice_columna(datos, columna)
  x <- datos[[indice]]
  if (identical(estrategia, "convertir_ausencias_textuales")) {
    cambio <- .reemplazar_ausencias_textuales(x, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "convertir_sentinelas_numericos")) {
    cambio <- .reemplazar_sentinelas_numericos(x, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "recortar_espacios")) {
    cambio <- .recortar_texto(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "convertir_fecha_confirmada")) {
    datos[[indice]] <- .convertir_fecha(x, parametros)
    return(list(datos = datos, n = sum(!is.na(x))))
  }
  if (identical(estrategia, "convertir_tipo")) {
    datos[[indice]] <- .convertir_tipo(x, parametros)
    return(list(datos = datos, n = sum(!is.na(x))))
  }
  if (identical(estrategia, "marcar_outliers")) {
    marca <- .marca_outliers(x)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  stop("Estrategia de limpieza no implementada: ", estrategia, ".", call. = FALSE)
}

.registro_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    estrategia = character(), n_cambiadas = numeric(),
    fecha_hora = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

#' @rdname planificar_limpieza
#' @export
aplicar <- function(plan, datos) {
  .validar_plan_limpieza(plan)
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  seleccion <- which(plan$aplicar)
  if (length(seleccion)) {
    seleccion <- seleccion[order(plan$orden[seleccion], seleccion)]
  }
  salida <- .copiar_datos(datos)
  registros <- vector("list", length(seleccion))
  for (j in seq_along(seleccion)) {
    accion <- plan[seleccion[[j]], , drop = FALSE]
    ejecutada <- .ejecutar_accion(salida, accion)
    salida <- ejecutada$datos
    registro <- data.frame(
      id_accion = accion$id_accion[[1L]],
      columna = accion$columna[[1L]],
      hallazgo = accion$hallazgo[[1L]],
      estrategia = accion$estrategia[[1L]],
      n_cambiadas = as.numeric(ejecutada$n),
      fecha_hora = as.POSIXct(Sys.time(), tz = "UTC"),
      stringsAsFactors = FALSE
    )
    registro$parametros <- I(list(accion$parametros[[1L]]))
    registros[[j]] <- registro
  }
  registro <- if (length(registros)) do.call(rbind, registros) else .registro_vacio()
  rownames(registro) <- NULL
  attr(salida, "registro_limpieza") <- registro
  estructura <- list(
    datos = salida,
    registro = registro,
    plan_aplicado = plan[seleccion, , drop = FALSE]
  )
  class(estructura) <- "resultado_limpieza"
  estructura
}

#' @export
print.plan_limpieza <- function(x, ...) {
  cli::cli_h1("Plan de limpieza")
  cli::cli_alert_success(paste(sum(x$aplicar), "acciones activadas"))
  cli::cli_alert_info(paste(sum(!x$aplicar), "acciones desactivadas"))
  vista <- x[c(
    "id_accion", "columna", "estrategia", "estado", "recomendada", "aplicar"
  )]
  print.data.frame(vista, row.names = FALSE)
  invisible(x)
}

#' @export
print.resultado_limpieza <- function(x, ...) {
  cli::cli_h1("Resultado de limpieza")
  cli::cli_alert_success(paste(nrow(x$registro), "acciones ejecutadas"))
  cli::cli_text(paste(sum(x$registro$n_cambiadas), "celdas o marcas afectadas"))
  invisible(x)
}
