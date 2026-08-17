.columnas_medicion_tablero <- c(
  "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
  "metrica_instanciada", "dimension", "factor", "orientacion",
  "granularidad", "tipo_resultado", "entidad", "atributo", "fila",
  "objeto_medible", "resultado", "agregacion"
)

.tablero_vacio <- function(cobertura = NULL, marco = NULL) {
  resultado <- data.frame(
    componente = character(), dimension = character(), factor = character(),
    metrica = character(), objeto = character(), valor = numeric(),
    orientacion = character(), agregacion = character(), umbral = numeric(),
    universo = character(), stringsAsFactors = FALSE
  )
  class(resultado) <- c("tablero_calidad", "data.frame")
  attr(resultado, "cobertura") <- .cobertura_para_tablero(
    resultado, cobertura, marco
  )
  attr(resultado, "alcance") <- .alcance_tablero(
    attr(resultado, "cobertura", exact = TRUE)
  )
  resultado
}

.marco_para_tablero <- function(medidas, marco = NULL) {
  if (!is.null(marco)) {
    if (!inherits(marco, "marco_calidad")) {
      stop("`marco` debe provenir de marco_calidad().", call. = FALSE)
    }
    return(marco)
  }
  asociado <- attr(medidas, "marco_calidad", exact = TRUE)
  if (inherits(asociado, "marco_calidad")) return(asociado)
  if (inherits(medidas, "data.frame") && nrow(medidas) &&
      all(c("dimension", "factor") %in% names(medidas))) {
    pares <- unique(medidas[c("dimension", "factor")])
    agesic <- marco_agesic()
    claves <- paste(pares$dimension, pares$factor, sep = "\r")
    claves_agesic <- paste(
      agesic$factores$dimension, agesic$factores$factor, sep = "\r"
    )
    if (all(claves %in% claves_agesic)) return(agesic)
    return(marco_calidad("Marco de la medici\u00f3n", pares))
  }
  marco_agesic()
}

.validar_cobertura_tablero <- function(cobertura) {
  requeridas <- c("marco", "dimension", "factor", "estado")
  if (!inherits(cobertura, "data.frame") ||
      !all(requeridas %in% names(cobertura))) {
    stop(
      "`cobertura` debe provenir de cobertura_analisis().",
      call. = FALSE
    )
  }
  estados <- as.character(cobertura$estado)
  validos <- c("medida", "no_declarada", "no_aplica", "fuera_de_alcance")
  if (anyNA(estados) || any(!estados %in% validos)) {
    stop("`cobertura` contiene estados no reconocidos.", call. = FALSE)
  }
  cobertura
}

.cobertura_para_tablero <- function(tablero, cobertura = NULL, marco = NULL) {
  if (is.null(cobertura)) {
    marco <- .marco_para_tablero(tablero, marco)
    factores <- marco$factores
    cobertura <- data.frame(
      marco = marco$nombre,
      dimension = factores$dimension,
      factor = factores$factor,
      estado = ifelse(
        factores$disponibilidad == "fuera_de_alcance",
        "fuera_de_alcance", "no_declarada"
      ),
      motivo = ifelse(
        factores$disponibilidad == "fuera_de_alcance",
        "El factor queda fuera del alcance de esta versi\u00f3n.",
        "No hay una m\u00e9trica medida para este factor."
      ),
      stringsAsFactors = FALSE
    )
  } else {
    cobertura <- .validar_cobertura_tablero(cobertura)
    cobertura <- as.data.frame(cobertura, stringsAsFactors = FALSE)
    cobertura$estado <- as.character(cobertura$estado)
  }
  if (nrow(tablero)) {
    claves_tablero <- unique(paste(tablero$dimension, tablero$factor, sep = "\r"))
    claves <- paste(cobertura$dimension, cobertura$factor, sep = "\r")
    medidas <- claves %in% claves_tablero
    cobertura$estado[medidas] <- "medida"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas] <-
        "El tablero contiene al menos una m\u00e9trica para este factor."
    }
    medidas_perfil <- cobertura$estado == "medida" & !medidas
    cobertura$estado[medidas_perfil] <- "no_declarada"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas_perfil] <-
        "El perfil examin\u00f3 el factor, pero el tablero no contiene una m\u00e9trica."
    }
  } else {
    medidas_perfil <- cobertura$estado == "medida"
    cobertura$estado[medidas_perfil] <- "no_declarada"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas_perfil] <-
        "El perfil examin\u00f3 el factor, pero el tablero no contiene una m\u00e9trica."
    }
  }
  cobertura$estado <- factor(
    cobertura$estado,
    levels = c("medida", "no_declarada", "no_aplica", "fuera_de_alcance")
  )
  rownames(cobertura) <- NULL
  cobertura
}

.alcance_tablero <- function(cobertura) {
  estados <- if (is.null(cobertura)) character() else as.character(cobertura$estado)
  data.frame(
    factores_marco = length(estados),
    factores_medidos = sum(estados == "medida"),
    sin_metrica_declarada = sum(estados == "no_declarada"),
    no_aplican = sum(estados == "no_aplica"),
    fuera_de_alcance = sum(estados == "fuera_de_alcance"),
    stringsAsFactors = FALSE
  )
}

.validar_medidas_tablero <- function(medidas) {
  if (!inherits(medidas, "data.frame") || !nrow(medidas) ||
      !all(.columnas_medicion_tablero %in% names(medidas))) {
    stop(
      "`medidas` debe ser una medici\u00f3n no vac\u00eda producida por medir() o agregar().",
      call. = FALSE
    )
  }
  if (length(unique(medidas$id_medicion)) != 1L) {
    stop("El tablero admite una sola corrida de medici\u00f3n.", call. = FALSE)
  }
  if (!is.numeric(medidas$resultado) || anyNA(medidas$resultado) ||
      any(!is.finite(medidas$resultado))) {
    stop("La medici\u00f3n contiene resultados no num\u00e9ricos o no finitos.",
         call. = FALSE)
  }
  medidas$orientacion <- .orientacion_medidas(medidas)
  campos <- c(
    "metrica", "dimension", "factor", "orientacion", "granularidad",
    "tipo_resultado"
  )
  por_metrica <- split(seq_len(nrow(medidas)), medidas$metrica_instanciada)
  invalidas <- names(por_metrica)[vapply(por_metrica, function(i) {
    any(vapply(medidas[i, campos, drop = FALSE], function(x) {
      length(unique(x)) != 1L
    }, logical(1L)))
  }, logical(1L))]
  if (length(invalidas)) {
    stop(
      "Cada m\u00e9trica instanciada debe conservar un solo contrato: ",
      paste(invalidas, collapse = ", "), ".", call. = FALSE
    )
  }
  medidas
}

.configuracion_agregaciones <- function(medidas, agregaciones, umbrales) {
  metricas <- unique(medidas$metrica_instanciada)
  tipos <- vapply(metricas, function(x) {
    unique(medidas$tipo_resultado[medidas$metrica_instanciada == x])[[1L]]
  }, character(1L))
  elegidas <- ifelse(tipos == "booleano", "ratio", "promedio")
  names(elegidas) <- metricas
  umbral <- stats::setNames(rep(NA_real_, length(metricas)), metricas)

  if (inherits(agregaciones, "data.frame")) {
    requeridas <- c("metrica_instanciada", "agregacion")
    if (!all(requeridas %in% names(agregaciones))) {
      stop(
        "`agregaciones` debe contener `metrica_instanciada` y `agregacion`.",
        call. = FALSE
      )
    }
    if (anyDuplicated(agregaciones$metrica_instanciada)) {
      stop("Cada m\u00e9trica puede declarar una sola agregaci\u00f3n.", call. = FALSE)
    }
    desconocidas <- setdiff(agregaciones$metrica_instanciada, metricas)
    if (length(desconocidas)) {
      stop("Sobran agregaciones para: ", paste(desconocidas, collapse = ", "),
           ".", call. = FALSE)
    }
    elegidas[agregaciones$metrica_instanciada] <- agregaciones$agregacion
    if ("umbral" %in% names(agregaciones)) {
      umbral[agregaciones$metrica_instanciada] <- agregaciones$umbral
    }
  } else if (!is.null(agregaciones)) {
    if (!is.character(agregaciones) || !length(agregaciones)) {
      stop("`agregaciones` debe ser texto con nombres o un data frame.",
           call. = FALSE)
    }
    if (length(agregaciones) == 1L && is.null(names(agregaciones))) {
      elegidas[] <- agregaciones
    } else {
      if (is.null(names(agregaciones)) || any(!nzchar(names(agregaciones))) ||
          anyDuplicated(names(agregaciones))) {
        stop("El vector `agregaciones` debe tener nombres \u00fanicos.", call. = FALSE)
      }
      desconocidas <- setdiff(names(agregaciones), metricas)
      if (length(desconocidas)) {
        stop("Sobran agregaciones para: ", paste(desconocidas, collapse = ", "),
             ".", call. = FALSE)
      }
      elegidas[names(agregaciones)] <- agregaciones
    }
  }
  if (!is.null(umbrales)) {
    if (!is.numeric(umbrales) || is.null(names(umbrales)) ||
        any(!nzchar(names(umbrales))) || anyDuplicated(names(umbrales))) {
      stop("`umbrales` debe ser un vector num\u00e9rico con nombres \u00fanicos.",
           call. = FALSE)
    }
    desconocidos <- setdiff(names(umbrales), metricas)
    if (length(desconocidos)) {
      stop("Sobran umbrales para: ", paste(desconocidos, collapse = ", "), ".",
           call. = FALSE)
    }
    umbral[names(umbrales)] <- umbrales
  }
  validas <- c("ratio", "promedio", "ratio_umbral")
  if (anyNA(elegidas) || any(!elegidas %in% validas)) {
    stop(
      "Las agregaciones admitidas son `ratio`, `promedio` y `ratio_umbral`.",
      call. = FALSE
    )
  }
  if (any(elegidas == "ratio" & tipos != "booleano")) {
    stop("`ratio` s\u00f3lo admite m\u00e9tricas booleanas.", call. = FALSE)
  }
  if (any(elegidas == "ratio_umbral" & tipos != "real")) {
    stop("`ratio_umbral` s\u00f3lo admite m\u00e9tricas reales acotadas.",
         call. = FALSE)
  }
  requieren <- names(elegidas)[elegidas == "ratio_umbral"]
  invalidos <- requieren[
    is.na(umbral[requieren]) | !is.finite(umbral[requieren]) |
      umbral[requieren] < 0 | umbral[requieren] > 1
  ]
  if (length(invalidos)) {
    stop(
      "`ratio_umbral` requiere un umbral en [0, 1] para: ",
      paste(invalidos, collapse = ", "), ".", call. = FALSE
    )
  }
  data.frame(
    metrica_instanciada = metricas,
    agregacion = unname(elegidas[metricas]),
    umbral = unname(umbral[metricas]), stringsAsFactors = FALSE
  )
}

.claves_objeto_tablero <- function(medidas) {
  granularidad <- unique(medidas$granularidad)[[1L]]
  switch(
    granularidad,
    instanciaAtributo = interaction(
      addNA(as.factor(medidas$entidad)), addNA(as.factor(medidas$atributo)),
      drop = TRUE, lex.order = TRUE
    ),
    atributo = interaction(
      addNA(as.factor(medidas$entidad)), addNA(as.factor(medidas$atributo)),
      drop = TRUE, lex.order = TRUE
    ),
    instanciaEntidad = addNA(as.factor(medidas$entidad)),
    entidad = addNA(as.factor(medidas$entidad)),
    factor(medidas$objeto_medible, exclude = NULL)
  )
}

.destino_tablero <- function(granularidad) {
  switch(
    granularidad,
    instanciaAtributo = "atributo",
    instanciaEntidad = "entidad",
    granularidad
  )
}

.objeto_tablero <- function(medidas, i, destino, varias_entidades) {
  primero <- i[[1L]]
  if (destino == "atributo") return(medidas$atributo[[primero]])
  if (destino == "entidad") {
    if (varias_entidades) {
      return(paste0("(tabla: ", medidas$entidad[[primero]], ")"))
    }
    return("(tabla)")
  }
  medidas$objeto_medible[[primero]]
}

.universo_tablero <- function(granularidad) {
  switch(
    granularidad,
    instanciaAtributo = "celdas",
    atributo = "columnas",
    conjuntoAtributos = "conjuntos de columnas",
    instanciaEntidad = "filas",
    entidad = "tablas",
    conjuntoEntidades = "conjuntos de tablas",
    coleccion = "bases de datos",
    granularidad
  )
}

.agregar_medidas_tablero <- function(medidas, configuracion) {
  partes <- list()
  metricas <- unique(medidas$metrica_instanciada)
  varias_entidades <- length(unique(medidas$entidad)) > 1L
  for (nombre in metricas) {
    filas_metrica <- which(medidas$metrica_instanciada == nombre)
    actuales <- medidas[filas_metrica, , drop = FALSE]
    contrato <- configuracion[configuracion$metrica_instanciada == nombre, ]
    grupos <- split(seq_len(nrow(actuales)), .claves_objeto_tablero(actuales),
                    drop = TRUE)
    destino <- .destino_tablero(unique(actuales$granularidad)[[1L]])
    for (i in grupos) {
      primero <- i[[1L]]
      valores <- actuales$resultado[i]
      valor <- switch(
        contrato$agregacion,
        ratio = mean(valores == 1),
        promedio = mean(valores),
        ratio_umbral = mean(valores >= contrato$umbral)
      )
      partes[[length(partes) + 1L]] <- data.frame(
        id_medida = "",
        id_medicion = actuales$id_medicion[[primero]],
        fecha = actuales$fecha[[primero]],
        metrica = actuales$metrica[[primero]],
        metrica_especifica = actuales$metrica_especifica[[primero]],
        metrica_instanciada = actuales$metrica_instanciada[[primero]],
        dimension = actuales$dimension[[primero]],
        factor = actuales$factor[[primero]],
        orientacion = actuales$orientacion[[primero]],
        granularidad = destino,
        tipo_resultado = "real",
        entidad = actuales$entidad[[primero]],
        atributo = if (destino == "atributo") {
          actuales$atributo[[primero]]
        } else NA_character_,
        fila = NA_integer_,
        objeto_medible = .objeto_tablero(
          actuales, i, destino, varias_entidades
        ),
        resultado = valor,
        agregacion = contrato$agregacion,
        umbral_tablero = contrato$umbral,
        universo_tablero = .universo_tablero(
          unique(actuales$granularidad)[[1L]]
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado$id_medida <- paste0(
    resultado$id_medicion, "-tablero-", sprintf("%06d", seq_len(nrow(resultado)))
  )
  class(resultado) <- c("medicion", "data.frame")
  resultado
}

.preparar_tablero <- function(medidas, agregaciones = NULL, umbrales = NULL,
                              marco = NULL, cobertura = NULL) {
  medidas <- .validar_medidas_tablero(medidas)
  ya_agregadas <- all(!is.na(medidas$agregacion)) &&
    all(!medidas$granularidad %in% c("instanciaAtributo", "instanciaEntidad"))
  if (ya_agregadas && (!is.null(agregaciones) || !is.null(umbrales))) {
    stop(
      "Una medici\u00f3n ya agregada conserva su agregaci\u00f3n; para cambiarla use las medidas de origen.",
      call. = FALSE
    )
  }
  agregada <- if (ya_agregadas) {
    medidas$umbral_tablero <- NA_real_
    medidas$universo_tablero <- vapply(
      medidas$granularidad, .universo_tablero, character(1L)
    )
    medidas
  } else {
    configuracion <- .configuracion_agregaciones(
      medidas, agregaciones, umbrales
    )
    .agregar_medidas_tablero(medidas, configuracion)
  }
  varias_entidades <- length(unique(agregada$entidad)) > 1L
  objetos <- vapply(seq_len(nrow(agregada)), function(i) {
    .objeto_tablero(
      agregada, i, agregada$granularidad[[i]], varias_entidades
    )
  }, character(1L))
  tablero <- data.frame(
    componente = paste0("componente-", sprintf("%04d", seq_len(nrow(agregada)))),
    dimension = agregada$dimension,
    factor = agregada$factor,
    metrica = agregada$metrica,
    objeto = objetos,
    valor = agregada$resultado,
    orientacion = agregada$orientacion,
    agregacion = agregada$agregacion,
    umbral = agregada$umbral_tablero,
    universo = agregada$universo_tablero,
    stringsAsFactors = FALSE
  )
  class(tablero) <- c("tablero_calidad", "data.frame")
  marco_elegido <- .marco_para_tablero(medidas, marco)
  cobertura_tablero <- .cobertura_para_tablero(
    tablero, cobertura, marco_elegido
  )
  attr(tablero, "cobertura") <- cobertura_tablero
  attr(tablero, "alcance") <- .alcance_tablero(cobertura_tablero)
  attr(tablero, "marco_calidad") <- marco_elegido
  attr(agregada, "cobertura_tablero") <- cobertura_tablero
  attr(agregada, "marco_calidad") <- marco_elegido
  list(tablero = tablero, medicion = agregada)
}

#' Construir un tablero de calidad
#'
#' Resume una corrida en una fila por métrica y objeto. Las métricas
#' booleanas usan `ratio` por omisión y las reales usan `promedio`; la
#' elección queda siempre en la columna `agregacion`. `ratio_umbral` sólo se
#' aplica cuando se declara también el umbral correspondiente.
#'
#' El objeto conserva la cobertura completa del marco: factores medidos, sin
#' métrica declarada, no aplicables y fuera de alcance. [print()] muestra
#' ambos elementos para que unas pocas filas nunca se lean como cobertura total.
#'
#' @param medidas Objeto creado por [medir()] o [agregar()].
#' @param agregaciones `NULL`, una agregación para todas las métricas, un
#'   vector con nombres de métrica instanciada o un data frame con
#'   `metrica_instanciada`, `agregacion` y, opcionalmente, `umbral`.
#' @param umbrales Vector numérico opcional con nombres de métrica
#'   instanciada. Se exige para cada `ratio_umbral` que no lo declare dentro de
#'   `agregaciones`.
#' @param marco Marco conceptual. Si se omite, usa el asociado a la medición,
#'   el marco AGESIC cuando corresponde o el conjunto de factores medidos.
#' @param cobertura Cobertura opcional creada por [cobertura_analisis()].
#'
#' @return Data frame S3 `tablero_calidad`. Los atributos `alcance` y
#'   `cobertura` conservan los conteos y el detalle del marco.
#' @export
#' @seealso [medir()], [agregar()], [indice_calidad()]
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
#' medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#' tablero_calidad(medidas)
tablero_calidad <- function(medidas, agregaciones = NULL, umbrales = NULL,
                            marco = NULL, cobertura = NULL) {
  if (inherits(medidas, "analisis")) return(medidas$tablero)
  .preparar_tablero(
    medidas, agregaciones, umbrales, marco, cobertura
  )$tablero
}

#' @export
print.tablero_calidad <- function(x, ...) {
  cli::cli_h1("Tablero de calidad")
  print.data.frame(x, row.names = FALSE)
  alcance <- attr(x, "alcance", exact = TRUE)
  if (inherits(alcance, "data.frame") && nrow(alcance)) {
    cli::cli_h2("Alcance del marco")
    print(alcance, row.names = FALSE)
  }
  invisible(x)
}

.validar_pesos_indice <- function(pesos, esperados, etiqueta = "pesos") {
  if (!is.numeric(pesos) || !length(pesos) || is.null(names(pesos)) ||
      anyNA(names(pesos)) || any(!nzchar(names(pesos))) ||
      anyDuplicated(names(pesos))) {
    stop("`", etiqueta, "` debe ser num\u00e9rico y tener nombres \u00fanicos.",
         call. = FALSE)
  }
  if (anyNA(pesos) || any(!is.finite(pesos)) || any(pesos < 0 | pesos > 1)) {
    stop("`", etiqueta, "` debe contener valores en [0, 1].", call. = FALSE)
  }
  faltan <- setdiff(esperados, names(pesos))
  sobran <- setdiff(names(pesos), esperados)
  if (length(faltan)) {
    stop("Faltan ", etiqueta, " para: ", paste(faltan, collapse = ", "), ".",
         call. = FALSE)
  }
  if (length(sobran)) {
    stop("Sobran ", etiqueta, " para: ", paste(sobran, collapse = ", "), ".",
         call. = FALSE)
  }
  if (abs(sum(pesos) - 1) > sqrt(.Machine$double.eps)) {
    stop("Los ", etiqueta, " deben sumar uno.", call. = FALSE)
  }
  pesos[esperados]
}

.pesos_internos_indice <- function(componentes, pesos_internos) {
  conteos <- table(componentes$dimension)
  multiples <- names(conteos)[conteos > 1L]
  resultado <- stats::setNames(rep(1, nrow(componentes)), componentes$componente)
  if (!length(multiples)) {
    if (!is.null(pesos_internos)) {
      stop(
        "Sobran pesos_internos: ninguna dimensi\u00f3n requiere combinaci\u00f3n interna.",
        call. = FALSE
      )
    }
    return(resultado)
  }
  requeridos <- componentes$componente[componentes$dimension %in% multiples]
  if (is.null(pesos_internos)) {
    stop(
      "Las dimensiones con varios componentes requieren `pesos_internos`: ",
      paste(multiples, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!is.numeric(pesos_internos) || is.null(names(pesos_internos)) ||
      anyNA(pesos_internos) || any(!is.finite(pesos_internos)) ||
      any(pesos_internos < 0 | pesos_internos > 1) ||
      any(!nzchar(names(pesos_internos))) || anyDuplicated(names(pesos_internos))) {
    stop("`pesos_internos` debe tener nombres \u00fanicos y valores en [0, 1].",
         call. = FALSE)
  }
  faltan <- setdiff(requeridos, names(pesos_internos))
  sobran <- setdiff(names(pesos_internos), requeridos)
  if (length(faltan)) {
    stop("Faltan pesos_internos para: ", paste(faltan, collapse = ", "), ".",
         call. = FALSE)
  }
  if (length(sobran)) {
    stop("Sobran pesos_internos para: ", paste(sobran, collapse = ", "), ".",
         call. = FALSE)
  }
  resultado[requeridos] <- pesos_internos[requeridos]
  for (dimension in multiples) {
    claves <- componentes$componente[componentes$dimension == dimension]
    if (abs(sum(resultado[claves]) - 1) > sqrt(.Machine$double.eps)) {
      stop("Los pesos_internos de ", dimension, " deben sumar uno.",
           call. = FALSE)
    }
  }
  resultado
}

.cobertura_indice <- function(tablero, componentes) {
  cobertura <- attr(tablero, "cobertura", exact = TRUE)
  total <- if (inherits(cobertura, "data.frame")) nrow(cobertura) else 0L
  pares <- unique(componentes[c("dimension", "factor")])
  nombres <- if (nrow(pares)) {
    paste(pares$dimension, pares$factor, sep = " / ")
  } else character()
  data.frame(
    factores_marco = total,
    factores_en_indice = nrow(pares),
    factores = paste(nombres, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

.nuevo_indice_sin_componentes <- function(tablero) {
  excluidas <- tablero[tablero$orientacion == "no_aplica", , drop = FALSE]
  resultado <- list(
    valor = NA_real_, cobertura = .cobertura_indice(tablero, tablero[0, ]),
    pesos = numeric(), pesos_internos = numeric(), componentes = tablero[0, ],
    dimensiones = data.frame(), invertidas = tablero[0, ],
    excluidas = excluidas, nivel_pesos = "dimensi\u00f3n",
    combinacion_interna = "No hubo componentes combinables.",
    advertencia_universos = paste0(
      "Los componentes salen de universos distintos (por ejemplo, celdas, ",
      "valores con formato reconocible y filas). El \u00edndice s\u00f3lo los combina ",
      "porque quien lo solicit\u00f3 declar\u00f3 los pesos."
    ),
    motivo = paste0(
      "No hay \u00edndice: todas las m\u00e9tricas tienen orientaci\u00f3n ",
      "'no_aplica' y no representan proporciones."
    ),
    tablero = tablero
  )
  class(resultado) <- "indice_calidad"
  resultado
}

#' Calcular un índice de calidad declarado por el usuario
#'
#' Sin `pesos`, devuelve [tablero_calidad()] y nunca un puntaje. Con pesos
#' nombrados por dimensión, transforma las métricas de defecto como
#' `1 - valor`, excluye las de orientación `no_aplica` y conserva cada paso.
#'
#' Cuando una dimensión contiene varios componentes, `pesos_internos` debe
#' declarar una ponderación completa que sume uno dentro de esa dimensión.
#' No existe un promedio interno por omisión. El resultado conserva el tablero,
#' ambas capas de pesos, las inversiones, las exclusiones, los universos y la
#' cobertura del marco.
#'
#' @param medidas Medición, tablero o análisis de `lupa`.
#' @param pesos Vector numérico nombrado por dimensión, en `[0, 1]` y con
#'   suma uno. Si se omite, se devuelve el tablero.
#' @param pesos_internos Vector opcional nombrado por `componente`; es
#'   obligatorio para cada dimensión con más de una fila incluida.
#' @param ... Argumentos enviados a [tablero_calidad()] cuando `medidas` no es
#'   ya un tablero o análisis.
#'
#' @return Sin pesos, un `tablero_calidad`. Con pesos, un objeto S3
#'   `indice_calidad` que nunca se imprime como un número aislado.
#' @export
#' @seealso [tablero_calidad()]
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' instancias <- list(
#'   instanciar(especializar(nucleo$NoNulo), "padron", "codigo"),
#'   instanciar(especializar(nucleo$EntidadDuplicada), "padron")
#' )
#' medidas <- medir(modelo(instancias), data.frame(codigo = c("A", "B", "B")))
#' indice_calidad(medidas)
#' # Pesos propios de este ejemplo, no del paquete:
#' indice_calidad(
#'   medidas,
#'   pesos = c(Completitud = 0.6, Unicidad = 0.4)
#' )
indice_calidad <- function(medidas, pesos, pesos_internos = NULL, ...) {
  tablero <- if (inherits(medidas, "analisis")) {
    medidas$tablero
  } else if (inherits(medidas, "tablero_calidad")) {
    medidas
  } else {
    tablero_calidad(medidas, ...)
  }
  if (missing(pesos) || is.null(pesos)) return(tablero)
  componentes <- tablero[tablero$orientacion != "no_aplica", , drop = FALSE]
  if (!nrow(componentes)) return(.nuevo_indice_sin_componentes(tablero))
  dimensiones <- unique(componentes$dimension)
  pesos <- .validar_pesos_indice(pesos, dimensiones)
  internos <- .pesos_internos_indice(componentes, pesos_internos)
  componentes$transformacion <- ifelse(
    componentes$orientacion == "defecto", "1 - valor", "valor"
  )
  componentes$valor_indice <- ifelse(
    componentes$orientacion == "defecto",
    1 - componentes$valor, componentes$valor
  )
  componentes$peso_interno <- unname(internos[componentes$componente])
  resumen <- lapply(dimensiones, function(dimension) {
    filas <- componentes$dimension == dimension
    valor <- sum(
      componentes$valor_indice[filas] * componentes$peso_interno[filas]
    )
    data.frame(
      dimension = dimension, valor = valor,
      peso = unname(pesos[[dimension]]),
      aporte = valor * unname(pesos[[dimension]]),
      combinacion_interna = if (sum(filas) == 1L) {
        "un componente; sin paso intermedio"
      } else {
        "promedio ponderado con pesos_internos declarados"
      },
      stringsAsFactors = FALSE
    )
  })
  resumen <- do.call(rbind, resumen)
  rownames(resumen) <- NULL
  excluidas <- tablero[tablero$orientacion == "no_aplica", , drop = FALSE]
  if (nrow(excluidas)) {
    excluidas$motivo_exclusion <- paste0(
      "orientaci\u00f3n no_aplica: el valor no es una proporci\u00f3n combinable"
    )
  }
  invertidas <- componentes[
    componentes$orientacion == "defecto", , drop = FALSE
  ]
  resultado <- list(
    valor = sum(resumen$aporte),
    cobertura = .cobertura_indice(tablero, componentes),
    pesos = pesos,
    pesos_internos = internos,
    componentes = componentes,
    dimensiones = resumen,
    invertidas = invertidas,
    excluidas = excluidas,
    nivel_pesos = "dimensi\u00f3n",
    combinacion_interna = paste0(
      "Dentro de cada dimensi\u00f3n se usa un solo componente o los ",
      "pesos_internos declarados; entre dimensiones se usan `pesos`."
    ),
    advertencia_universos = paste0(
      "Los componentes salen de universos distintos (por ejemplo, celdas, ",
      "valores con formato reconocible y filas). El \u00edndice s\u00f3lo los combina ",
      "porque quien lo solicit\u00f3 declar\u00f3 los pesos."
    ),
    motivo = NULL,
    tablero = tablero
  )
  class(resultado) <- "indice_calidad"
  resultado
}

#' @export
print.indice_calidad <- function(x, ...) {
  cli::cli_h1("\u00cdndice de calidad declarado")
  if (is.na(x$valor)) {
    cli::cli_alert_warning(x$motivo)
  } else {
    cli::cli_dl(c("Valor" = format(x$valor, digits = 6)))
  }
  cli::cli_h2("Cobertura del \u00edndice")
  print.data.frame(x$cobertura, row.names = FALSE)
  if (nrow(x$dimensiones)) {
    cli::cli_h2("Dimensiones, pesos y aportes")
    print.data.frame(x$dimensiones, row.names = FALSE)
  }
  if (nrow(x$invertidas)) {
    cli::cli_h2("Componentes de defecto invertidos")
    print.data.frame(x$invertidas, row.names = FALSE)
  }
  if (nrow(x$excluidas)) {
    cli::cli_h2("Componentes excluidos")
    print.data.frame(x$excluidas, row.names = FALSE)
  }
  cli::cli_alert_info(x$combinacion_interna)
  cli::cli_alert_warning(x$advertencia_universos)
  invisible(x)
}
