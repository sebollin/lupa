#' Reglas y perfiles de evaluación
#'
#' Una regla aplica una condición a los resultados de una o más métricas
#' instanciadas. Un perfil reúne reglas y su evaluación es la media aritmética
#' simple de las evaluaciones de esas reglas; no es un índice de dimensión ni
#' un índice global de calidad.
#'
#' `perfiles_madurez()` crea por omisión los perfiles `Básico`, `Intermedio` y
#' `Avanzado` de AGESIC, con condiciones estrictas `> 0.5`, `> 0.7` y `> 0.9`.
#' El argumento `umbrales` permite construir otra familia con nombres y cortes
#' crecientes propios sobre las mismas métricas instanciadas.
#'
#' @param nombre Nombre de la regla o del perfil.
#' @param condicion Función que recibe resultados en `[0, 1]` y devuelve un
#'   vector lógico sin ausentes de la misma longitud.
#' @param metricas Nombres de métricas instanciadas a las que se aplica la
#'   regla. `NULL` aplica la condición a todas.
#' @param umbrales Vector numérico con nombres, estrictamente creciente y en
#'   `[0, 1]`. `NULL` conserva los tres perfiles incluidos de fábrica.
#' @param ... Reglas creadas por `regla_evaluacion()` o una única lista que las
#'   contenga.
#'
#' @return `regla_evaluacion()` devuelve una `regla_evaluacion`;
#'   `perfil_evaluacion()` devuelve un `perfil_evaluacion`; y
#'   `perfiles_madurez()` devuelve una lista de perfiles.
#' @name reglas_evaluacion
#'
#' @examples
#' regla <- regla_evaluacion("Completitud suficiente", function(x) x > 0.9)
#' perfil <- perfil_evaluacion("Operativo", regla)
#' madurez <- perfiles_madurez("NoNulo")
#' propios <- perfiles_madurez(
#'   "NoNulo", c(Exploratorio = 0.3, Operativo = 0.65, Consolidado = 0.85)
#' )
#' names(madurez)
#' names(propios)
#' perfil$nombre
NULL

#' @rdname reglas_evaluacion
#' @export
#' @seealso [medir()], [evaluar()], [perfiles_madurez()]
regla_evaluacion <- function(nombre, condicion, metricas = NULL) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (!is.function(condicion)) {
    stop("`condicion` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!is.null(metricas) &&
      (!is.character(metricas) || !length(metricas) || anyNA(metricas) ||
       any(!nzchar(metricas)))) {
    stop("`metricas` debe ser NULL o nombres no vac\u00edos.", call. = FALSE)
  }
  estructura <- list(
    nombre = nombre,
    condicion = condicion,
    metricas = unique(metricas)
  )
  class(estructura) <- "regla_evaluacion"
  estructura
}

#' @rdname reglas_evaluacion
#' @export
#' @seealso [regla_evaluacion()], [comparar_evaluaciones()],
#'   [historico_calidad()]
perfil_evaluacion <- function(nombre, ...) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  reglas <- list(...)
  if (length(reglas) == 1L && is.list(reglas[[1L]]) &&
      !inherits(reglas[[1L]], "regla_evaluacion")) {
    reglas <- reglas[[1L]]
  }
  if (!length(reglas) ||
      !all(vapply(reglas, inherits, logical(1L), "regla_evaluacion"))) {
    stop("Un perfil requiere una o m\u00e1s reglas de evaluaci\u00f3n.", call. = FALSE)
  }
  nombres <- vapply(reglas, `[[`, character(1L), "nombre")
  if (anyDuplicated(nombres)) {
    stop("Los nombres de las reglas del perfil deben ser \u00fanicos.", call. = FALSE)
  }
  names(reglas) <- nombres
  estructura <- list(nombre = nombre, reglas = reglas)
  class(estructura) <- "perfil_evaluacion"
  estructura
}

.regla_umbral <- function(nombre, umbral, metricas) {
  force(umbral)
  regla_evaluacion(
    nombre,
    condicion = function(x) x > umbral,
    metricas = metricas
  )
}

#' @rdname reglas_evaluacion
#' @export
#' @seealso [evaluar()], [detectar_deriva_calidad()]
perfiles_madurez <- function(metricas = NULL, umbrales = NULL) {
  fabrica <- is.null(umbrales)
  if (fabrica) {
    umbrales <- c(Basico = 0.5, Intermedio = 0.7, Avanzado = 0.9)
  }
  if (!is.numeric(umbrales) || !length(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      is.null(names(umbrales)) || anyNA(names(umbrales)) ||
      any(!nzchar(names(umbrales))) || anyDuplicated(names(umbrales))) {
    stop(
      "`umbrales` debe ser un vector num\u00e9rico con nombres \u00fanicos en [0, 1].",
      call. = FALSE
    )
  }
  if (length(umbrales) > 1L && any(diff(umbrales) <= 0)) {
    stop("Los umbrales de madurez deben ser estrictamente crecientes.",
         call. = FALSE)
  }
  nombres <- names(umbrales)
  nombres_perfil <- if (fabrica) {
    c(Basico = "B\u00e1sico", Intermedio = "Intermedio", Avanzado = "Avanzado")
  } else {
    nombres
  }
  perfiles <- lapply(seq_along(umbrales), function(i) {
    nombre <- unname(nombres_perfil[[i]])
    umbral <- unname(umbrales[[i]])
    perfil_evaluacion(
      nombre,
      .regla_umbral(paste0("Resultado > ", umbral), umbral, metricas)
    )
  })
  names(perfiles) <- nombres
  perfiles
}

.validar_medicion_evaluacion <- function(medicion) {
  requeridas <- c(
    "id_medida", "id_medicion", "fecha", "metrica_instanciada",
    "tipo_resultado", "resultado"
  )
  if (!inherits(medicion, "data.frame") || !nrow(medicion) ||
      !all(requeridas %in% names(medicion))) {
    stop("`medicion` debe ser un data frame no vac\u00edo producido por medir().",
         call. = FALSE)
  }
  if (!.resultados_validos_tipo(
    medicion$resultado, medicion$tipo_resultado
  )) {
    stop("Los resultados de la medici\u00f3n no respetan su tipo declarado.",
         call. = FALSE)
  }
  medicion
}

.evaluar_regla_medidas <- function(medicion, perfil, regla) {
  seleccion <- if (is.null(regla$metricas)) {
    rep(TRUE, nrow(medicion))
  } else {
    medicion$metrica_instanciada %in% regla$metricas
  }
  medidas <- medicion[seleccion, , drop = FALSE]
  if (!nrow(medidas)) {
    stop(
      "La regla '", regla$nombre, "' no coincide con ninguna m\u00e9trica instanciada.",
      call. = FALSE
    )
  }
  resultado <- regla$condicion(medidas$resultado)
  if (!is.logical(resultado) || length(resultado) != nrow(medidas) ||
      anyNA(resultado)) {
    stop(
      "La condici\u00f3n de la regla '", regla$nombre,
      "' debe devolver l\u00f3gicos sin NA, uno por medida.", call. = FALSE
    )
  }
  data.frame(
    id_medida = medidas$id_medida,
    id_medicion = medidas$id_medicion,
    fecha = medidas$fecha,
    perfil = perfil$nombre,
    regla = regla$nombre,
    metrica_instanciada = medidas$metrica_instanciada,
    resultado = resultado,
    stringsAsFactors = FALSE
  )
}

.resumir_evaluaciones_regla <- function(evaluaciones) {
  clave <- interaction(
    evaluaciones$id_medicion, evaluaciones$perfil, evaluaciones$regla,
    drop = TRUE, lex.order = TRUE
  )
  grupos <- split(seq_len(nrow(evaluaciones)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    primera <- indices[[1L]]
    data.frame(
      id_medicion = evaluaciones$id_medicion[[primera]],
      fecha = evaluaciones$fecha[primera],
      perfil = evaluaciones$perfil[[primera]],
      regla = evaluaciones$regla[[primera]],
      n_medidas = length(indices),
      resultado = mean(evaluaciones$resultado[indices]),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

.resumir_evaluaciones_perfil <- function(evaluaciones) {
  clave <- interaction(
    evaluaciones$id_medicion, evaluaciones$perfil,
    drop = TRUE, lex.order = TRUE
  )
  grupos <- split(seq_len(nrow(evaluaciones)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    primera <- indices[[1L]]
    data.frame(
      id_medicion = evaluaciones$id_medicion[[primera]],
      fecha = evaluaciones$fecha[primera],
      perfil = evaluaciones$perfil[[primera]],
      n_reglas = length(indices),
      resultado = mean(evaluaciones$resultado[indices]),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

#' Evaluar medidas, reglas y perfiles
#'
#' Ejecuta la cadena formal: condición por medida, proporción de medidas que
#' cumplen cada regla y media aritmética simple de las reglas del perfil.
#'
#' @param medicion Data frame producido por `medir()`. Puede reunir varias
#'   corridas si conserva sus `id_medicion`.
#' @param perfil Objeto creado por `perfil_evaluacion()`.
#'
#' @return Objeto `evaluacion_calidad` con tres data frames filtrables:
#'   `medidas`, `reglas` y `perfiles`.
#' @export
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' especifica <- especializar(nucleo$NoNulo)
#' instancia <- instanciar(especifica, "personas", "edad")
#' medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#' regla <- regla_evaluacion("Al menos 90%", function(x) x > 0.9)
#' evaluar(medidas, perfil_evaluacion("Avanzado", regla))
evaluar <- function(medicion, perfil) {
  medicion <- .validar_medicion_evaluacion(medicion)
  if (!inherits(perfil, "perfil_evaluacion")) {
    stop("`perfil` debe provenir de perfil_evaluacion().", call. = FALSE)
  }
  evaluaciones_medidas <- do.call(rbind, lapply(perfil$reglas, function(regla) {
    .evaluar_regla_medidas(medicion, perfil, regla)
  }))
  rownames(evaluaciones_medidas) <- NULL
  evaluaciones_reglas <- .resumir_evaluaciones_regla(evaluaciones_medidas)
  evaluaciones_perfiles <- .resumir_evaluaciones_perfil(evaluaciones_reglas)
  class(evaluaciones_medidas) <- c("evaluacion_medidas", "data.frame")
  class(evaluaciones_reglas) <- c("evaluacion_reglas", "data.frame")
  class(evaluaciones_perfiles) <- c("evaluacion_perfiles", "data.frame")
  estructura <- list(
    medidas = evaluaciones_medidas,
    reglas = evaluaciones_reglas,
    perfiles = evaluaciones_perfiles
  )
  class(estructura) <- "evaluacion_calidad"
  estructura
}

#' Comparar evaluaciones de perfil
#'
#' Calcula el cambio de `EvaluacionPerfil` entre dos corridas. Cada objeto debe
#' contener una sola `id_medicion`; no persiste los resultados. Para una serie
#' de N corridas use [historico_calidad()] y [detectar_deriva_calidad()].
#'
#' @param anterior,actual Objetos creados por `evaluar()`.
#'
#' @return Data frame con resultados anterior y actual, y `delta`.
#' @export
#'
#' @examples
#' # Ver ejemplos de evaluar().
comparar_evaluaciones <- function(anterior, actual) {
  if (!inherits(anterior, "evaluacion_calidad") ||
      !inherits(actual, "evaluacion_calidad")) {
    stop("`anterior` y `actual` deben provenir de evaluar().", call. = FALSE)
  }
  a <- anterior$perfiles
  b <- actual$perfiles
  if (length(unique(a$id_medicion)) != 1L ||
      length(unique(b$id_medicion)) != 1L) {
    stop("Cada evaluaci\u00f3n debe contener una sola corrida.", call. = FALSE)
  }
  combinado <- merge(
    a[c("perfil", "id_medicion", "fecha", "resultado")],
    b[c("perfil", "id_medicion", "fecha", "resultado")],
    by = "perfil", suffixes = c("_anterior", "_actual"), all = TRUE,
    sort = FALSE
  )
  combinado$delta <- combinado$resultado_actual - combinado$resultado_anterior
  combinado
}
