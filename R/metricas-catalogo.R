.validar_config_comprension <- function(configuracion) {
  permitidas <- c("predicado", "minimo", "maximo", "inclusivo")
  desconocidas <- setdiff(names(configuracion), permitidas)
  if (length(desconocidas)) {
    stop(
      "ValoresPosiblesPorComprension no acepta: ",
      paste(desconocidas, collapse = ", "), ".", call. = FALSE
    )
  }
  tiene_predicado <- !is.null(configuracion$predicado)
  tiene_rango <- !is.null(configuracion$minimo) ||
    !is.null(configuracion$maximo) || !is.null(configuracion$inclusivo)
  if (tiene_predicado == tiene_rango) {
    stop(
      "ValoresPosiblesPorComprension exige un `predicado` o un rango, no ambos.",
      call. = FALSE
    )
  }
  if (tiene_predicado) {
    if (!is.function(configuracion$predicado)) {
      stop("`predicado` debe ser una funci\u00f3n.", call. = FALSE)
    }
    return(list(predicado = configuracion$predicado))
  }
  if (is.null(configuracion$minimo) || is.null(configuracion$maximo) ||
      length(configuracion$minimo) != 1L ||
      length(configuracion$maximo) != 1L ||
      anyNA(c(configuracion$minimo, configuracion$maximo))) {
    stop("El rango exige `minimo` y `maximo` escalares no ausentes.",
         call. = FALSE)
  }
  orden_valido <- tryCatch(
    isTRUE(configuracion$minimo <= configuracion$maximo),
    error = function(e) FALSE
  )
  if (!orden_valido) {
    stop("`minimo` no puede ser mayor que `maximo`.", call. = FALSE)
  }
  inclusivo <- configuracion$inclusivo
  if (is.null(inclusivo)) inclusivo <- c(TRUE, TRUE)
  if (!is.logical(inclusivo) || anyNA(inclusivo) ||
      !length(inclusivo) %in% c(1L, 2L)) {
    stop("`inclusivo` debe contener uno o dos valores l\u00f3gicos.",
         call. = FALSE)
  }
  if (length(inclusivo) == 1L) inclusivo <- rep(inclusivo, 2L)
  list(
    minimo = configuracion$minimo,
    maximo = configuracion$maximo,
    inclusivo = inclusivo
  )
}

.metodo_valores_comprension <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  valores <- x[filas]
  config <- instancia$configuracion
  if (!is.null(config$predicado)) {
    resultado <- .resultado_validador(
      config$predicado(valores), length(valores),
      "ValoresPosiblesPorComprension"
    )
  } else {
    izquierda <- if (config$inclusivo[[1L]]) {
      valores >= config$minimo
    } else {
      valores > config$minimo
    }
    derecha <- if (config$inclusivo[[2L]]) {
      valores <= config$maximo
    } else {
      valores < config$maximo
    }
    resultado <- izquierda & derecha
    if (!is.logical(resultado) || anyNA(resultado)) {
      stop("El rango no se puede comparar con el atributo ligado.",
           call. = FALSE)
    }
  }
  .salida_metodo(
    resultado, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.duplicados_completos <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

.metodo_atributo_duplicado <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  .salida_metodo(
    .duplicados_completos(x[filas]), entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.validar_atributos_tabla <- function(tabla, instancia, minimo = 1L) {
  if (length(instancia$entidad) != 1L ||
      length(instancia$atributos) < minimo ||
      anyDuplicated(instancia$atributos)) {
    stop(
      "La m\u00e9trica ", instancia$declaracion$nombre,
      " requiere una entidad y al menos ", minimo,
      " atributo(s) distinto(s).", call. = FALSE
    )
  }
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop(
      "No se encontraron atributos ligados: ",
      paste(faltantes, collapse = ", "), ".", call. = FALSE
    )
  }
}

.metodo_conjunto_duplicado <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    stop(
      "ConjuntoAtributosDuplicado requiere una entidad y al menos dos atributos.",
      call. = FALSE
    )
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  .validar_atributos_tabla(tabla, instancia, minimo = 2L)
  valores <- tabla[, instancia$atributos, drop = FALSE]
  filas <- seq_len(nrow(tabla))
  .salida_metodo(
    .duplicados_completos(valores), entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",",
           paste(instancia$atributos, collapse = "+"), "]")
  )
}

.metodo_entidad_duplicada <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    stop("EntidadDuplicada requiere una entidad.", call. = FALSE)
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  filas <- seq_len(nrow(tabla))
  if (!length(instancia$atributos)) {
    resultado <- .duplicados_completos(tabla)
  } else {
    .validar_atributos_tabla(tabla, instancia, minimo = 1L)
    grupos <- split(filas, .codigos_filas(tabla[instancia$atributos]))
    resultado <- rep(FALSE, nrow(tabla))
    otros <- setdiff(names(tabla), instancia$atributos)
    compatibles <- function(b, a) {
      if (!length(otros)) return(TRUE)
      all(vapply(otros, function(nombre) {
        x <- tabla[[nombre]][[a]]
        y <- tabla[[nombre]][[b]]
        is.na(x) || is.na(y) || identical(as.character(x), as.character(y))
      }, logical(1L)))
    }
    for (indices in grupos[lengths(grupos) > 1L]) {
      for (i in seq_along(indices)) {
        restantes <- indices[-i]
        resultado[indices[[i]]] <- any(vapply(
          restantes, compatibles, logical(1L), a = indices[[i]]
        ))
      }
    }
  }
  .salida_metodo(
    resultado, entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",]")
  )
}

.validar_config_desactualizacion <- function(configuracion) {
  permitidas <- c("expresion_regular", "validador")
  desconocidas <- setdiff(names(configuracion), permitidas)
  activas <- intersect(names(configuracion), permitidas)
  activas <- activas[!vapply(configuracion[activas], is.null, logical(1L))]
  if (length(desconocidas) || length(activas) != 1L) {
    stop(
      "DesactualizacionPorFormato exige exactamente `expresion_regular` o `validador`.",
      call. = FALSE
    )
  }
  if (activas == "expresion_regular" &&
      !.es_texto_escalar(configuracion$expresion_regular)) {
    stop("`expresion_regular` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (activas == "validador" && !is.function(configuracion$validador)) {
    stop("`validador` debe ser una funci\u00f3n.", call. = FALSE)
  }
  configuracion
}

.metodo_desactualizacion_formato <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  valores <- x[filas]
  config <- instancia$configuracion
  vigente <- if (!is.null(config$expresion_regular)) {
    grepl(config$expresion_regular, as.character(valores), perl = TRUE)
  } else {
    .resultado_validador(
      config$validador(valores), length(valores),
      "DesactualizacionPorFormato"
    )
  }
  .salida_metodo(
    !vigente, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.es_fecha_modelo <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXt")
}

.fecha_numerica <- function(x) {
  if (inherits(x, "Date")) as.numeric(x) * 86400 else as.numeric(x)
}

.validar_intervalo_fechas <- function(inicio, fin, nombre_inicio, nombre_fin) {
  if (!.es_fecha_modelo(inicio) || !.es_fecha_modelo(fin) ||
      !length(inicio) || !length(fin) || anyNA(inicio) || anyNA(fin)) {
    stop(
      "`", nombre_inicio, "` y `", nombre_fin,
      "` deben contener fechas no ausentes.", call. = FALSE
    )
  }
  if (length(inicio) != 1L && length(fin) != 1L &&
      length(inicio) != length(fin)) {
    stop("Los extremos del intervalo deben tener longitudes compatibles.",
         call. = FALSE)
  }
  n <- max(length(inicio), length(fin))
  diferencia <- rep(.fecha_numerica(fin), length.out = n) -
    rep(.fecha_numerica(inicio), length.out = n)
  if (any(!is.finite(diferencia))) {
    stop("El intervalo contiene una duraci\u00f3n no finita.", call. = FALSE)
  }
  if (any(diferencia == 0)) {
    stop(
      "El intervalo tiene duraci\u00f3n cero y no define oportunidad.",
      call. = FALSE
    )
  }
  if (any(diferencia < 0)) {
    stop(
      "`", nombre_fin, "` debe ser posterior a `", nombre_inicio,
      "`; el intervalo est\u00e1 invertido.", call. = FALSE
    )
  }
  invisible(TRUE)
}

.validar_config_oportunidad_fecha <- function(configuracion) {
  configuracion <- .validar_propiedades_base(
    configuracion, c("fecha_solicitud", "fecha_fin_utilidad")
  )
  .validar_intervalo_fechas(
    configuracion$fecha_solicitud, configuracion$fecha_fin_utilidad,
    "fecha_solicitud", "fecha_fin_utilidad"
  )
  configuracion
}

.validar_config_oportunidad_intervalo <- function(configuracion) {
  configuracion <- .validar_propiedades_base(
    configuracion, c("inicio_vigencia", "fin_vigencia")
  )
  .validar_intervalo_fechas(
    configuracion$inicio_vigencia, configuracion$fin_vigencia,
    "inicio_vigencia", "fin_vigencia"
  )
  configuracion
}

.fecha_para_filas <- function(x, filas, n_total, nombre) {
  if (length(x) == 1L) return(rep(.fecha_numerica(x), length(filas)))
  if (length(x) != n_total) {
    stop(
      "`", nombre, "` debe tener longitud 1 o tantas fechas como filas.",
      call. = FALSE
    )
  }
  .fecha_numerica(x[filas])
}

.metodo_oportunidad <- function(tablas, instancia, inicio_nombre, fin_nombre) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  if (!.es_fecha_modelo(x)) {
    stop("La oportunidad requiere un atributo Date o POSIXt.", call. = FALSE)
  }
  filas <- which(!is.na(x))
  config <- instancia$configuracion
  entrega <- .fecha_numerica(x[filas])
  inicio <- .fecha_para_filas(config[[inicio_nombre]], filas, length(x),
                             inicio_nombre)
  fin <- .fecha_para_filas(config[[fin_nombre]], filas, length(x), fin_nombre)
  duracion <- fin - inicio
  if (any(duracion <= 0)) {
    stop("El intervalo de oportunidad debe tener duraci\u00f3n positiva.",
         call. = FALSE)
  }
  resultado <- pmin(1, pmax(0, 1 - (entrega - inicio) / duracion))
  .salida_metodo(
    resultado, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.metodo_oportunidad_fecha <- function(tablas, instancia) {
  .metodo_oportunidad(
    tablas, instancia, "fecha_solicitud", "fecha_fin_utilidad"
  )
}

.metodo_oportunidad_intervalo <- function(tablas, instancia) {
  .metodo_oportunidad(tablas, instancia, "inicio_vigencia", "fin_vigencia")
}

.validar_config_densidad <- function(configuracion) {
  configuracion <- .validar_propiedades_base(configuracion, "coeficientes")
  coeficientes <- configuracion$coeficientes
  if (!is.numeric(coeficientes) || !length(coeficientes) ||
      anyNA(coeficientes) || any(!is.finite(coeficientes)) ||
      any(coeficientes < 0 | coeficientes > 1) ||
      abs(sum(coeficientes) - 1) > sqrt(.Machine$double.eps)) {
    stop(
      "`coeficientes` debe contener pesos en [0, 1] que sumen 1.",
      call. = FALSE
    )
  }
  if (!is.null(names(coeficientes)) &&
      (any(!nzchar(names(coeficientes))) || anyDuplicated(names(coeficientes)))) {
    stop("Los nombres de `coeficientes` deben ser \u00fanicos y no vac\u00edos.",
         call. = FALSE)
  }
  configuracion
}

.metodo_densidad_ponderada <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    stop("DensidadPonderada requiere una entidad.", call. = FALSE)
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  .validar_atributos_tabla(tabla, instancia, minimo = 1L)
  pesos <- instancia$configuracion$coeficientes
  if (!is.null(names(pesos))) {
    if (!setequal(names(pesos), instancia$atributos)) {
      stop(
        "Los nombres de `coeficientes` deben coincidir con los atributos ligados.",
        call. = FALSE
      )
    }
    pesos <- pesos[instancia$atributos]
  } else if (length(pesos) != length(instancia$atributos)) {
    stop("Debe haber un coeficiente por atributo ligado.", call. = FALSE)
  }
  presencia <- !is.na(tabla[, instancia$atributos, drop = FALSE])
  resultado <- as.numeric(as.matrix(presencia) %*% pesos)
  filas <- seq_len(nrow(tabla))
  .salida_metodo(
    resultado, entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",]")
  )
}

.metricas_adicionales <- function() {
  list(
    Escala = metrica(
      "Escala",
      "Estima la precisi\u00f3n de un valor con el error experto de su escala.",
      "instanciaAtributo", "real", propiedades = "escala",
      dimension = "Exactitud", factor = "Precisi\u00f3n",
      metodo = .metodo_escala, validar_propiedades = .validar_config_escala
    ),
    ValoresPosiblesPorComprension = metrica(
      "ValoresPosiblesPorComprension",
      "Indica si un valor satisface un predicado o pertenece a un rango.",
      "instanciaAtributo", "booleano",
      propiedades = c("predicado", "minimo", "maximo", "inclusivo"),
      dimension = "Consistencia", factor = "Integridad de dominio",
      metodo = .metodo_valores_comprension,
      validar_propiedades = .validar_config_comprension
    ),
    AtributoDuplicado = metrica(
      "AtributoDuplicado",
      "Indica si un valor participa en un grupo duplicado del atributo.",
      "instanciaAtributo", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_atributo_duplicado
    ),
    ConjuntoAtributosDuplicado = metrica(
      "ConjuntoAtributosDuplicado",
      "Indica si una combinaci\u00f3n de atributos se repite en otra fila.",
      "instanciaEntidad", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_conjunto_duplicado
    ),
    EntidadDuplicada = metrica(
      "EntidadDuplicada",
      paste0(
        "Indica si otra fila con la misma clave representa la misma entidad, ",
        "con los dem\u00e1s datos iguales o ausentes."
      ),
      "instanciaEntidad", "booleano",
      dimension = "Unicidad", factor = "No-duplicaci\u00f3n",
      metodo = .metodo_entidad_duplicada
    ),
    DesactualizacionPorFormato = metrica(
      "DesactualizacionPorFormato",
      "Indica si el formato de un valor delata que est\u00e1 desactualizado.",
      "instanciaAtributo", "booleano",
      propiedades = c("expresion_regular", "validador"),
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_formato,
      validar_propiedades = .validar_config_desactualizacion
    ),
    DesactualizacionPorFecha = metrica(
      "DesactualizacionPorFecha",
      "Mide en d\u00edas el atraso respecto del \u00faltimo cambio o frecuencia esperada.",
      "instanciaAtributo", "duracion", propiedades = "vigencia",
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_fecha,
      validar_propiedades = .validar_config_vigencia
    ),
    DesactualizacionPorCambios = metrica(
      "DesactualizacionPorCambios",
      "Estima cambios esperados desde la \u00faltima actualizaci\u00f3n.",
      "instanciaAtributo", "entero", propiedades = "vigencia",
      dimension = "Frescura", factor = "Actualidad",
      metodo = .metodo_desactualizacion_cambios,
      validar_propiedades = .validar_config_vigencia
    ),
    OportunidadAtributoPorFecha = metrica(
      "OportunidadAtributoPorFecha",
      "Mide la oportunidad entre solicitud, entrega y fin de utilidad.",
      "instanciaAtributo", "real",
      propiedades = c("fecha_solicitud", "fecha_fin_utilidad"),
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_fecha,
      validar_propiedades = .validar_config_oportunidad_fecha
    ),
    OportunidadAtributoPorIntervalo = metrica(
      "OportunidadAtributoPorIntervalo",
      "Mide la oportunidad dentro de un intervalo de vigencia.",
      "instanciaAtributo", "real",
      propiedades = c("inicio_vigencia", "fin_vigencia"),
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_intervalo,
      validar_propiedades = .validar_config_oportunidad_intervalo
    ),
    OportunidadEntPorFecha = metrica(
      "OportunidadEntPorFecha",
      "Indica si una entidad fue actualizada antes de su fecha l\u00edmite.",
      "instanciaEntidad", "booleano", propiedades = "vigencia",
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_entidad_fecha,
      validar_propiedades = .validar_config_vigencia
    ),
    OportunidadEntPorIntervalo = metrica(
      "OportunidadEntPorIntervalo",
      "Indica si una entidad fue actualizada dentro de su intervalo vigente.",
      "instanciaEntidad", "booleano", propiedades = "vigencia",
      dimension = "Frescura", factor = "Oportunidad",
      metodo = .metodo_oportunidad_entidad_intervalo,
      validar_propiedades = .validar_config_vigencia
    ),
    DensidadPonderada = metrica(
      "DensidadPonderada",
      "Mide la presencia ponderada de atributos en cada fila.",
      "instanciaEntidad", "real", propiedades = "coeficientes",
      dimension = "Completitud", factor = "Densidad",
      metodo = .metodo_densidad_ponderada,
      validar_propiedades = .validar_config_densidad
    )
  )
}

#' Correspondencia con el catálogo de métricas de AGESIC
#'
#' Devuelve las 49 entradas del catálogo del marco y explicita qué parte está
#' disponible en `lupa`. Los ratios no se duplican como métricas: aparecen con
#' estado `"via_agregacion"` y con la llamada que los materializa.
#'
#' @return Data frame con una fila por entrada y las columnas `numero`,
#'   `dimension`, `factor`, `metrica_agesic`, `clase_catalogo`, `estado`,
#'   `metrica_lupa`, `implementacion` y `observacion`. `estado` es un factor con
#'   los niveles `"implementada"`, `"via_agregacion"`,
#'   `"requiere_referencial"` y `"fuera_de_alcance"`.
#'
#' @details
#' `implementada` significa que el motor existe; las especializaciones que
#' dependen de un diccionario o una regla de dominio deben ser configuradas por
#' quien mide. `requiere_referencial` identifica entradas cuyo contrato exige
#' datos externos que esta versión no obtiene ni interpreta.
#' `Escala` se clasifica como implementada con configuración experta mediante
#' [escala()], no como referencial. `DesactualizacionPorFecha`,
#' `DesactualizacionPorCambios` y las oportunidades de entidad requieren un
#' contrato [vigencia()]. `ErrorEstandar` sigue la semántica literal de la tabla
#' 16.5 y devuelve desviación estándar, aunque su nombre pueda sugerir el error
#' estándar de la media.
#'
#' La tabla deja visibles dos decisiones de arquitectura. El resultado real de
#' `OportunidadAtributo*` sigue la fórmula continua del proceso de evaluación,
#' aunque las tablas 16.29 y 16.30 lo declaran booleano. Además,
#' `RatioDensidadPonderada` usa `ratio_umbral`, porque `DensidadPonderada` es
#' real y `ratio` sólo es válido para medidas booleanas.
#'
#' @references AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, capítulo 16, Presidencia de la
#'   República, Uruguay.
#'
#' @export
#' @seealso [metricas_nucleo()], [metricas_referencial()], [agregar()]
#'
#' @examples
#' catalogo <- catalogo_agesic()
#' subset(catalogo, estado == "via_agregacion")
#' table(catalogo$estado)
catalogo_agesic <- function() {
  metricas <- c(
    "CorrectitudSemDebil",
    "CorrectitudSemFuerte",
    "RatioCorrectitudSemFuerte",
    "RatioCorrectitudSemD\u00e9bil",
    "Formato",
    "Formato(Pais, ISOAlpha3)",
    "Formato(Enfermedad, CIE10) [DA:\u00abSalud\u00bb]",
    "Formato(NumeroDocumento, DNIC)",
    "Escala",
    "ErrorEstandar",
    "\u00cdndiceErroresPosicionalesPorUmbral",
    "ValorMedioIncertidumbrePosicional",
    "ErrorHorizontalRelativo",
    "IndiceFidelidadConReferencia",
    "IndiceFidelidadSinReferencia",
    "IndiceFidelidadReferenciaReducida",
    "ReglaIntegridadInterEntidad",
    "ReglaEspacial [TD:\u00abGeo\u00bb]",
    "ReglaIntegridadInterEntidad(Sexo, Enfermedad) [DA:\u00abSalud\u00bb]",
    "ReglaIntegridadIntraEntidad",
    "RatioIntegridadIntraEntidad",
    "ReglaIntegridadIntraEntidad(Sexo,Enfermedad) [DA:\u00abSalud\u00bb]",
    "ValoresPosiblesPorExtensi\u00f3n",
    "ValoresPosiblesPorComprensi\u00f3n",
    "ValoresPosiblesPE(Sexo, AGESIC)",
    "\u00cdndiceFallosConexi\u00f3nNodosEnlace",
    "RatioCobertura",
    "NoNulo",
    "DensidadPonderada",
    "RatioNoNulos",
    "RatioDensidadPonderada",
    "\u00cdtemExcedente",
    "\u00cdndiceItemsExcedentes",
    "AtributoDuplicado",
    "ConjuntoAtributosDuplicado",
    "EntidadDuplicada",
    "RatioAtributoDuplicado",
    "RatioConjuntoAtributosDuplicado",
    "RatioEntidadesDuplicadas",
    "EntidadContradictoria",
    "RatioEntidadContradictoria",
    "Desactualizaci\u00f3nPorFecha",
    "Desactualizaci\u00f3nPorCambios",
    "Desactualizaci\u00f3nPorFormato",
    "Desactualizaci\u00f3nPorFormato(TelefonoFijo, FormatoPNN1)",
    "OportunidadAtributoPorFecha",
    "OportunidadAtributoPorIntervalo",
    "OportunidadEntPorFecha",
    "OportunidadEntPorIntervalo"
  )
  dimension <- c(
    rep("Exactitud", 16L), rep("Consistencia", 10L),
    rep("Completitud", 7L), rep("Unicidad", 8L), rep("Frescura", 8L)
  )
  factor <- c(
    rep("Correctitud sem\u00e1ntica", 4L),
    rep("Correctitud sint\u00e1ctica", 4L),
    rep("Precisi\u00f3n", 2L), rep("Exactitud posicional absoluta", 2L),
    "Exactitud posicional relativa", rep("Fidelidad", 3L),
    rep("Integridad inter-entidad", 3L),
    rep("Integridad intra-entidad", 3L),
    rep("Integridad de dominio", 3L), "Consistencia topol\u00f3gica",
    "Cobertura", rep("Densidad", 4L), rep("Comisi\u00f3n", 2L),
    rep("No-duplicaci\u00f3n", 6L), rep("No-contradicci\u00f3n", 2L),
    rep("Actualidad", 4L), rep("Oportunidad", 4L)
  )
  clase <- rep("generica", 49L)
  clase[c(6:8, 19L, 22L, 25L, 45L)] <- "especifica"
  clase[c(3:4, 21L, 30:31, 37:39, 41L)] <- "agregada"

  implementadas <- c(
    1:2, 5:10, 17L, 20L, 22:25, 27:29, 34:36, 42:49
  )
  agregadas <- c(3:4, 21L, 30:31, 37:39)
  referencial <- 19L
  fuera <- setdiff(seq_len(49L), c(implementadas, agregadas, referencial))
  estado <- rep(NA_character_, 49L)
  estado[implementadas] <- "implementada"
  estado[agregadas] <- "via_agregacion"
  estado[referencial] <- "requiere_referencial"
  estado[fuera] <- "fuera_de_alcance"

  metrica_lupa <- rep(NA_character_, 49L)
  metrica_lupa[1L] <- "CorrectitudSemDebil"
  metrica_lupa[2L] <- "CorrectitudSemFuerte"
  metrica_lupa[c(5:8)] <- "Formato"
  metrica_lupa[9L] <- "Escala"
  metrica_lupa[10L] <- "ErrorEstandar"
  metrica_lupa[17L] <- "ReglaIntegridadInterEntidad"
  metrica_lupa[c(20L, 22L)] <- "ReglaIntegridadIntraEntidad"
  metrica_lupa[c(23L, 25L)] <- "ValoresPosiblesPorExtension"
  metrica_lupa[24L] <- "ValoresPosiblesPorComprension"
  metrica_lupa[27L] <- "RatioCobertura"
  metrica_lupa[28L] <- "NoNulo"
  metrica_lupa[29L] <- "DensidadPonderada"
  metrica_lupa[34L] <- "AtributoDuplicado"
  metrica_lupa[35L] <- "ConjuntoAtributosDuplicado"
  metrica_lupa[36L] <- "EntidadDuplicada"
  metrica_lupa[42L] <- "DesactualizacionPorFecha"
  metrica_lupa[43L] <- "DesactualizacionPorCambios"
  metrica_lupa[c(44L, 45L)] <- "DesactualizacionPorFormato"
  metrica_lupa[46L] <- "OportunidadAtributoPorFecha"
  metrica_lupa[47L] <- "OportunidadAtributoPorIntervalo"
  metrica_lupa[48L] <- "OportunidadEntPorFecha"
  metrica_lupa[49L] <- "OportunidadEntPorIntervalo"
  metrica_lupa[agregadas] <- c(
    "CorrectitudSemFuerte", "CorrectitudSemDebil",
    "ReglaIntegridadIntraEntidad", "NoNulo", "DensidadPonderada",
    "AtributoDuplicado", "ConjuntoAtributosDuplicado", "EntidadDuplicada"
  )

  implementacion <- rep(NA_character_, 49L)
  implementacion[implementadas] <- paste0(
    "metricas_nucleo()$", metrica_lupa[implementadas]
  )
  implementacion[1:2] <- paste0(
    "metricas_referencial()$", metrica_lupa[1:2]
  )
  implementacion[27L] <- "metricas_referencial()$RatioCobertura"
  implementacion[3:4] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[21L] <- "agregar(m, \"entidad\", \"ratio\")"
  implementacion[30L] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[31L] <- paste0(
    "agregar(m, \"entidad\", \"ratio_umbral\", umbral = u)"
  )
  implementacion[37L] <- "agregar(m, \"atributo\", \"ratio\")"
  implementacion[38:39] <- "agregar(m, \"entidad\", \"ratio\")"

  observacion <- rep(NA_character_, 49L)
  observacion[1L] <- paste0(
    "Contrasta el par identificaci\u00f3n-valor; omite ausentes."
  )
  observacion[2L] <- paste0(
    "Verifica que la identificaci\u00f3n exista; omite ausentes."
  )
  observacion[6:8] <- paste0(
    "Especializaci\u00f3n configurable de Formato; el referencial no se incluye."
  )
  observacion[8L] <- paste0(
    "Se conecta un validador externo, por ejemplo uyutils::validar_ci()."
  )
  observacion[9L] <- paste0(
    "Requiere configuraci\u00f3n experta mediante escala(); no usa referencial."
  )
  observacion[10L] <- paste0(
    "Sigue la tabla 16.5: devuelve desviaci\u00f3n est\u00e1ndar, no error de la media."
  )
  observacion[17L] <- paste0(
    "Implementa cobertura de integridad referencial entre PK y FK."
  )
  observacion[c(22L, 25L)] <- paste0(
    "La regla o el diccionario de dominio debe ser provisto al especializar."
  )
  observacion[31L] <- paste0(
    "Se usa RatioUmbral porque la medida base es real, no booleana."
  )
  observacion[27L] <- paste0(
    "Exige referencial() con completo = TRUE y alcance expl\u00edcito."
  )
  observacion[32:33] <- paste0(
    "El factor Comisi\u00f3n est\u00e1 restringido a datos geogr\u00e1ficos en el marco."
  )
  observacion[36L] <- paste0(
    paste0(
      "Sin clave ligada compara filas completas; con clave admite iguales ",
      "o nulos en los dem\u00e1s campos."
    )
  )
  observacion[45L] <- paste0(
    "Especializaci\u00f3n configurable con el formato vigente de ocho d\u00edgitos."
  )
  observacion[42L] <- paste0(
    "Requiere vigencia(); devuelve atraso en d\u00edas como duraci\u00f3n no negativa."
  )
  observacion[43L] <- paste0(
    "Requiere vigencia(); estima cambios con la frecuencia declarada."
  )
  observacion[46:47] <- paste0(
    "Resultado real continuo; el cat\u00e1logo lo declara booleano."
  )
  observacion[48:49] <- paste0(
    "Resultado booleano por fila; exige fechas o intervalos en vigencia()."
  )
  observacion[19L] <- paste0(
    "Requiere una regla sem\u00e1ntica entre entidades no reducible a PK/FK."
  )

  data.frame(
    numero = seq_along(metricas),
    dimension = dimension,
    factor = factor,
    metrica_agesic = metricas,
    clase_catalogo = base::factor(
      clase, levels = c("generica", "especifica", "agregada")
    ),
    estado = base::factor(
      estado,
      levels = c(
        "implementada", "via_agregacion", "requiere_referencial",
        "fuera_de_alcance"
      )
    ),
    metrica_lupa = metrica_lupa,
    implementacion = implementacion,
    observacion = observacion,
    stringsAsFactors = FALSE
  )
}
