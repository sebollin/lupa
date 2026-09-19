.columnas_medicion_tablero <- c(
  "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
  "metrica_instanciada", "dimension", "factor", "orientacion",
  "granularidad", "tipo_resultado", "entidad", "atributo", "fila",
  "objeto_medible", "resultado", "agregacion"
)

.tablero_vacio <- function(cobertura = NULL, marco = NULL,
                           cobertura_metricas = NULL) {
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
  if (inherits(cobertura_metricas, "data.frame") &&
      nrow(cobertura_metricas)) {
    attr(resultado, "cobertura_metricas") <- cobertura_metricas
  }
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
    pares <- .seleccionar_columnas(medidas, c("dimension", "factor"))
    pares <- pares[!duplicated(.clave_par_identificador(
      pares$dimension, pares$factor
    )), , drop = FALSE]
    agesic <- marco_agesic()
    claves <- .clave_par_identificador(
      pares$dimension, pares$factor, sep = "\r"
    )
    claves_agesic <- .clave_par_identificador(
      agesic$factores$dimension, agesic$factores$factor, sep = "\r"
    )
    if (all(.identificadores_en(claves, claves_agesic))) return(agesic)
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
  # El alcance que la capa anterior declaro tiene que SOBREVIVIR al cruce. La
  # cobertura publica "Parcial: el perfil obtuvo resumen en N de M columnas"
  # cuando el perfil no pudo resumir todas, y aca los dos caminos reescribian el
  # motivo con un texto fijo: el tablero publicaba "El perfil examino el factor"
  # con noventa y nueve columnas sin resumen. Es la forma exacta que este paquete
  # persigue -declarar algo y que el consumidor siguiente lo tire-, y el
  # `n_evaluados` de los hallazgos no la sufre porque viaja en su propia columna.
  .conservar_alcance_parcial <- function(motivos_previos, motivo_nuevo) {
    parcial <- !is.na(motivos_previos) & grepl("^Parcial: ", motivos_previos)
    alcance <- sub("^Parcial: el perfil obtuvo resumen en ", "", motivos_previos)
    alcance <- sub("\\. En las otras .*$", "", alcance)
    ifelse(
      parcial,
      paste0(motivo_nuevo, " El perfil obtuvo resumen en ", alcance, "."),
      motivo_nuevo
    )
  }

  if (nrow(tablero)) {
    claves_tablero <- .identificadores_unicos(.clave_par_identificador(
      tablero$dimension, tablero$factor, sep = "\r"
    ))
    claves <- .clave_par_identificador(
      cobertura$dimension, cobertura$factor, sep = "\r"
    )
    medidas <- .identificadores_en(claves, claves_tablero)
    cobertura$estado[medidas] <- "medida"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas] <- .conservar_alcance_parcial(
        cobertura$motivo[medidas],
        "El tablero contiene al menos una m\u00e9trica para este factor."
      )
    }
    medidas_perfil <- cobertura$estado == "medida" & !medidas
    cobertura$estado[medidas_perfil] <- "no_declarada"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas_perfil] <- .conservar_alcance_parcial(
        cobertura$motivo[medidas_perfil],
        "El perfil examin\u00f3 el factor, pero el tablero no contiene una m\u00e9trica."
      )
    }
  } else {
    medidas_perfil <- cobertura$estado == "medida"
    cobertura$estado[medidas_perfil] <- "no_declarada"
    if ("motivo" %in% names(cobertura)) {
      cobertura$motivo[medidas_perfil] <- .conservar_alcance_parcial(
        cobertura$motivo[medidas_perfil],
        "El perfil examin\u00f3 el factor, pero el tablero no contiene una m\u00e9trica."
      )
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
  if (!inherits(medidas, "data.frame") ||
      !all(.columnas_medicion_tablero %in% names(medidas))) {
    stop(
      "`medidas` debe ser una medici\u00f3n producida por medir() o agregar().",
      call. = FALSE
    )
  }
  if (!nrow(medidas)) {
    .error_medicion_sin_medidas(medidas, "medidas", "`medir()` o `agregar()`")
  }
  medidas <- .tabla_base(medidas)
  if (length(.identificadores_unicos(medidas$id_medicion)) != 1L) {
    stop("El tablero admite una sola corrida de medici\u00f3n.", call. = FALSE)
  }
  suprimidas <- if ("objeto_medible" %in% names(medidas)) {
    !is.na(medidas$objeto_medible) & grepl(
      "[valor suprimido]", medidas$objeto_medible, fixed = TRUE
    )
  } else {
    rep(FALSE, nrow(medidas))
  }
  if (!is.numeric(medidas$resultado) ||
      anyNA(medidas$resultado) && any(!suprimidas & is.na(medidas$resultado)) ||
      any(!suprimidas & !is.finite(medidas$resultado))) {
    stop("La medici\u00f3n contiene resultados no num\u00e9ricos o no finitos.",
         call. = FALSE)
  }
  medidas$orientacion <- .orientacion_medidas(medidas)
  campos <- c(
    "metrica", "dimension", "factor", "orientacion", "granularidad",
    "tipo_resultado"
  )
  metricas <- .identificadores_unicos(medidas$metrica_instanciada)
  invalidas <- metricas[vapply(metricas, function(metrica) {
    i <- which(.identificadores_en(medidas$metrica_instanciada, metrica))
    any(vapply(.seleccionar_columnas(medidas, campos, filas = i), function(x) {
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
  metricas <- .identificadores_unicos(medidas$metrica_instanciada)
  tipos <- vapply(metricas, function(x) {
    unique(medidas$tipo_resultado[
      .identificadores_en(medidas$metrica_instanciada, x)
    ])[[1L]]
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
    if (anyDuplicated(.nombres_para_operar(agregaciones$metrica_instanciada))) {
      stop("Cada m\u00e9trica puede declarar una sola agregaci\u00f3n.", call. = FALSE)
    }
    desconocidas <- .identificadores_setdiff(
      agregaciones$metrica_instanciada, metricas
    )
    if (length(desconocidas)) {
      stop("Sobran agregaciones para: ", paste(desconocidas, collapse = ", "),
           ".", call. = FALSE)
    }
    indices <- .indice_identificador(agregaciones$metrica_instanciada, metricas)
    elegidas[indices] <- agregaciones$agregacion
    if ("umbral" %in% names(agregaciones)) {
      umbral[indices] <- agregaciones$umbral
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
          anyDuplicated(.nombres_para_operar(names(agregaciones)))) {
        stop("El vector `agregaciones` debe tener nombres \u00fanicos.", call. = FALSE)
      }
      desconocidas <- .identificadores_setdiff(names(agregaciones), metricas)
      if (length(desconocidas)) {
        stop("Sobran agregaciones para: ", paste(desconocidas, collapse = ", "),
             ".", call. = FALSE)
      }
      elegidas[.indice_identificador(names(agregaciones), metricas)] <- agregaciones
    }
  }
  if (!is.null(umbrales)) {
    if (!is.numeric(umbrales) || is.null(names(umbrales)) ||
        any(!nzchar(names(umbrales))) ||
        anyDuplicated(.nombres_para_operar(names(umbrales)))) {
      stop("`umbrales` debe ser un vector num\u00e9rico con nombres \u00fanicos.",
           call. = FALSE)
    }
    desconocidos <- .identificadores_setdiff(names(umbrales), metricas)
    if (length(desconocidos)) {
      stop("Sobran umbrales para: ", paste(desconocidos, collapse = ", "), ".",
           call. = FALSE)
    }
    umbral[.indice_identificador(names(umbrales), metricas)] <- umbrales
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
    agregacion = unname(elegidas[.indice_identificador(metricas, names(elegidas))]),
    umbral = unname(umbral[.indice_identificador(metricas, names(umbral))]),
    stringsAsFactors = FALSE
  )
}

.claves_objeto_tablero <- function(medidas) {
  granularidad <- unique(medidas$granularidad)[[1L]]
  switch(
    granularidad,
    instanciaAtributo = interaction(
      addNA(as.factor(.nombres_para_operar(medidas$entidad))),
      addNA(as.factor(.nombres_para_operar(medidas$atributo))),
      drop = TRUE, lex.order = TRUE
    ),
    atributo = interaction(
      addNA(as.factor(.nombres_para_operar(medidas$entidad))),
      addNA(as.factor(.nombres_para_operar(medidas$atributo))),
      drop = TRUE, lex.order = TRUE
    ),
    instanciaEntidad = addNA(as.factor(.nombres_para_operar(medidas$entidad))),
    entidad = addNA(as.factor(.nombres_para_operar(medidas$entidad))),
    factor(.nombres_para_operar(medidas$objeto_medible), exclude = NULL)
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
  metricas <- .identificadores_unicos(medidas$metrica_instanciada)
  varias_entidades <- length(.identificadores_unicos(medidas$entidad)) > 1L
  for (nombre in metricas) {
    filas_metrica <- which(
      .identificadores_en(medidas$metrica_instanciada, nombre)
    )
    actuales <- medidas[filas_metrica, , drop = FALSE]
    contrato <- configuracion[
      .identificadores_en(configuracion$metrica_instanciada, nombre),
      , drop = FALSE
    ]
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
  cobertura_metricas <- attr(medidas, "cobertura_metricas", exact = TRUE)
  desenlaces <- .desenlaces_de_objeto(medidas)
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
  varias_entidades <- length(.identificadores_unicos(agregada$entidad)) > 1L
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
  if (inherits(cobertura_metricas, "data.frame") &&
      nrow(cobertura_metricas)) {
    attr(tablero, "cobertura_metricas") <- cobertura_metricas
  }
  tablero <- .proteger_tablero_desenlaces(tablero, desenlaces)
  attr(agregada, "cobertura_tablero") <- cobertura_tablero
  attr(agregada, "marco_calidad") <- marco_elegido
  if (inherits(cobertura_metricas, "data.frame") &&
      nrow(cobertura_metricas)) {
    attr(agregada, "cobertura_metricas") <- cobertura_metricas
  }
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
#'   **La correspondencia entre esa cobertura y `medidas` es suya, no del
#'   paquete**, igual que en [cobertura_analisis()]: el tablero cruza las dos
#'   por dimensión y factor y reescribe el estado de un factor cuando la
#'   medición lo cubre, sin verificar que las dos describan la misma tabla. Con
#'   una cobertura de otro análisis, el factor va a figurar como medido por esta
#'   corrida.
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
  if (inherits(medidas, "analisis")) {
    return(.proteger_tablero_desenlaces(
      medidas$tablero, .desenlaces_de_objeto(medidas)
    ))
  }
  .con_cobertura_coleccion(
    .preparar_tablero(
      medidas, agregaciones, umbrales, marco, cobertura
    )$tablero,
    medidas
  )
}

# La cobertura de coleccion viaja PEGADA al numero, y hasta el 2026-09-05 moria
# en el primer consumidor.
#
# `agregar()` a nivel `coleccion` adjunta `cobertura_coleccion` con las tablas
# declaradas, las que entraron al numero, las que no se midieron y una
# advertencia. `NEWS.md` declara que ese es el unico nivel donde la cobertura
# viaja con el numero. Medido: una coleccion de dos tablas donde una esta vacia
# publica `0,667` -que cubre UNA de las dos- y ni `tablero_calidad()` ni
# `indice_calidad()` conservaban la pieza que lo decia.
#
# Es la misma forma que el alcance del resumen y sus lectores: declarar algo en
# un objeto que el siguiente paso descarta es, para quien lee, no declararlo.
.con_cobertura_coleccion <- function(salida, origen) {
  cobertura <- attr(origen, "cobertura_coleccion", exact = TRUE)
  if (!is.null(cobertura)) {
    attr(salida, "cobertura_coleccion") <- cobertura
  }
  salida
}

#' @export
print.tablero_calidad <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Tablero de calidad")
  visible <- .proteger_tablero_desenlaces(x, .desenlaces_de_objeto(x))
  .print_data_frame_bytes(visible, row.names = FALSE)
  alcance <- attr(x, "alcance", exact = TRUE)
  if (inherits(alcance, "data.frame") && nrow(alcance)) {
    cli::cli_h2("Alcance del marco")
    .print_data_frame_bytes(alcance, row.names = FALSE)
  }
  cobertura_metricas <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura_metricas, "data.frame") &&
      nrow(cobertura_metricas)) {
    cli::cli_h2("Cobertura de m\u00e9tricas")
    .print_data_frame_bytes(cobertura_metricas, row.names = FALSE)
  }
  invisible(original)
}

.validar_pesos_indice <- function(pesos, esperados, etiqueta = "pesos") {
  if (!is.numeric(pesos) || !length(pesos) || is.null(names(pesos)) ||
      anyNA(names(pesos)) || any(!nzchar(names(pesos))) ||
      anyDuplicated(.nombres_para_operar(names(pesos)))) {
    stop("`", etiqueta, "` debe ser num\u00e9rico y tener nombres \u00fanicos.",
         call. = FALSE)
  }
  if (anyNA(pesos) || any(!is.finite(pesos)) || any(pesos < 0 | pesos > 1)) {
    stop("`", etiqueta, "` debe contener valores en [0, 1].", call. = FALSE)
  }
  faltan <- .identificadores_setdiff(esperados, names(pesos))
  sobran <- .identificadores_setdiff(names(pesos), esperados)
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
  pesos[.indice_identificador(esperados, names(pesos))]
}

.pesos_internos_indice <- function(componentes, pesos_internos) {
  claves_dimension <- .nombres_para_operar(componentes$dimension)
  conteos <- table(claves_dimension)
  claves_multiples <- names(conteos)[conteos > 1L]
  multiples <- componentes$dimension[
    match(claves_multiples, claves_dimension)
  ]
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
  requeridos <- componentes$componente[
    .identificadores_en(componentes$dimension, multiples)
  ]
  if (is.null(pesos_internos)) {
    stop(
      "Las dimensiones con varios componentes requieren `pesos_internos`: ",
      paste(multiples, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!is.numeric(pesos_internos) || is.null(names(pesos_internos)) ||
      anyNA(pesos_internos) || any(!is.finite(pesos_internos)) ||
      any(pesos_internos < 0 | pesos_internos > 1) ||
      any(!nzchar(names(pesos_internos))) ||
      anyDuplicated(.nombres_para_operar(names(pesos_internos)))) {
    stop("`pesos_internos` debe tener nombres \u00fanicos y valores en [0, 1].",
         call. = FALSE)
  }
  faltan <- .identificadores_setdiff(requeridos, names(pesos_internos))
  sobran <- .identificadores_setdiff(names(pesos_internos), requeridos)
  if (length(faltan)) {
    stop("Faltan pesos_internos para: ", paste(faltan, collapse = ", "), ".",
         call. = FALSE)
  }
  if (length(sobran)) {
    stop("Sobran pesos_internos para: ", paste(sobran, collapse = ", "), ".",
         call. = FALSE)
  }
  resultado[.indice_identificador(requeridos, names(resultado))] <-
    pesos_internos[.indice_identificador(requeridos, names(pesos_internos))]
  for (dimension in multiples) {
    componentes_dimension <- componentes$componente[
      .identificadores_en(componentes$dimension, dimension)
    ]
    indices_componentes <- .indice_identificador(
      componentes_dimension, names(resultado)
    )
    if (abs(sum(resultado[indices_componentes]) - 1) > sqrt(.Machine$double.eps)) {
      stop("Los pesos_internos de ", dimension, " deben sumar uno.",
           call. = FALSE)
    }
  }
  resultado
}

.cobertura_indice <- function(tablero, componentes,
                              cobertura_metricas = NULL) {
  cobertura <- attr(tablero, "cobertura", exact = TRUE)
  total <- if (inherits(cobertura, "data.frame")) nrow(cobertura) else 0L
  pares <- componentes[c("dimension", "factor")]
  pares <- pares[!duplicated(.clave_par_identificador(
    pares$dimension, pares$factor
  )), , drop = FALSE]
  nombres <- if (nrow(pares)) {
    paste(pares$dimension, pares$factor, sep = " / ")
  } else character()
  no_medidas <- if (inherits(cobertura_metricas, "data.frame") &&
                    nrow(cobertura_metricas) &&
                    "metrica_instanciada" %in% names(cobertura_metricas)) {
    as.character(cobertura_metricas$metrica_instanciada)
  } else character()
  data.frame(
    factores_marco = total,
    factores_en_indice = nrow(pares),
    factores = paste(nombres, collapse = "; "),
    metricas_no_medidas = paste(no_medidas, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

.nuevo_indice_sin_componentes <- function(tablero, motivo = NULL) {
  excluidas <- tablero[tablero$orientacion == "no_aplica", , drop = FALSE]
  cobertura_metricas <- attr(tablero, "cobertura_metricas", exact = TRUE)
  motivo <- if (is.null(motivo)) paste0(
    "No hay \u00edndice: todas las m\u00e9tricas tienen orientaci\u00f3n ",
    "'no_aplica' y no representan proporciones."
  ) else motivo
  resultado <- list(
    valor = NA_real_, cobertura = .cobertura_indice(
      tablero, tablero[0, ], cobertura_metricas
    ),
    cobertura_metricas = if (inherits(cobertura_metricas, "data.frame")) {
      cobertura_metricas
    } else data.frame(stringsAsFactors = FALSE),
    pesos = numeric(), pesos_internos = numeric(), componentes = tablero[0, ],
    dimensiones = data.frame(), invertidas = tablero[0, ],
    excluidas = excluidas, nivel_pesos = "dimensi\u00f3n",
    combinacion_interna = "No hubo componentes combinables.",
    advertencia_universos = paste0(
      "Los componentes salen de universos distintos (por ejemplo, celdas, ",
      "valores con formato reconocible y filas). El \u00edndice s\u00f3lo los combina ",
      "porque quien lo solicit\u00f3 declar\u00f3 los pesos."
    ),
    motivo = motivo,
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
  # El indice hereda la cobertura de coleccion de lo que recibe -sea el objeto
  # de `agregar()` o un tablero que ya la traiga-. Ver
  # `.con_cobertura_coleccion()`: la pieza que dice sobre cuantas tablas de la
  # coleccion se calculo el numero tiene que llegar hasta el ultimo consumidor.
  cobertura_coleccion <- attr(medidas, "cobertura_coleccion", exact = TRUE)
  tablero <- if (inherits(medidas, "analisis")) {
    tablero_calidad(medidas)
  } else if (inherits(medidas, "tablero_calidad")) {
    .proteger_tablero_desenlaces(medidas, .desenlaces_de_objeto(medidas))
  } else {
    tablero_calidad(medidas, ...)
  }
  if (!is.null(cobertura_coleccion)) {
    attr(tablero, "cobertura_coleccion") <- cobertura_coleccion
  }
  if (missing(pesos) || is.null(pesos)) return(tablero)
  cobertura_metricas <- attr(tablero, "cobertura_metricas", exact = TRUE)
  componentes <- tablero[tablero$orientacion != "no_aplica", , drop = FALSE]
  if (!nrow(componentes)) {
    motivo <- if (!nrow(tablero)) paste0(
      "No hay \u00edndice: no hubo mediciones combinables para esta corrida. ",
      "La cobertura conserva qu\u00e9 qued\u00f3 sin medir."
    ) else NULL
    return(.nuevo_indice_sin_componentes(tablero, motivo))
  }
  if (!is.numeric(componentes$valor)) {
    return(.nuevo_indice_sin_componentes(
      tablero,
      "No hay \u00edndice: una medida declarada para suprimir no puede publicarse ni combinarse."
    ))
  }
  dimensiones <- .identificadores_unicos(componentes$dimension)
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
    filas <- .identificadores_en(componentes$dimension, dimension)
    valor <- sum(
      componentes$valor_indice[filas] * componentes$peso_interno[filas]
    )
    indice_peso <- .indice_identificador(dimension, names(pesos))
    peso <- unname(pesos[[indice_peso]])
    data.frame(
      dimension = dimension, valor = valor,
      peso = peso,
      aporte = valor * peso,
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
    cobertura = .cobertura_indice(tablero, componentes, cobertura_metricas),
    cobertura_metricas = if (inherits(cobertura_metricas, "data.frame")) {
      cobertura_metricas
    } else data.frame(stringsAsFactors = FALSE),
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
    # La cobertura de la coleccion viaja hasta aca: el indice publica un solo
    # numero, y decir sobre cuantas de las tablas declaradas se calculo es la
    # unica forma de que ese numero se pueda leer.
    cobertura_coleccion = cobertura_coleccion,
    tablero = tablero
  )
  class(resultado) <- "indice_calidad"
  resultado
}

#' @export
print.indice_calidad <- function(x, ...) {
  .validar_objeto_lupa(x, "indice_calidad", character(), "indice_calidad()")
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("\u00cdndice de calidad declarado")
  if (is.na(x$valor)) {
    cli::cli_alert_warning(x$motivo)
  } else {
    cli::cli_dl(c("Valor" = format(x$valor, digits = 6)))
  }
  cli::cli_h2("Cobertura del \u00edndice")
  .print_data_frame_bytes(x$cobertura, row.names = FALSE)
  if (inherits(x$cobertura_metricas, "data.frame") &&
      nrow(x$cobertura_metricas)) {
    cli::cli_h2("Cobertura de m\u00e9tricas")
    .print_data_frame_bytes(x$cobertura_metricas, row.names = FALSE)
  }
  # Sobre cuantas tablas de la coleccion declarada se calculo este numero. Se
  # imprime porque un indice es UN numero: si la cobertura vive solo en un
  # atributo, quien lo lee en pantalla no la ve.
  if (!is.null(x$cobertura_coleccion)) {
    cc <- x$cobertura_coleccion
    cli::cli_h2("Cobertura de la colecci\u00f3n")
    etiquetas <- c(
      "Tablas declaradas" = as.character(cc$tablas_declaradas),
      "Sin medir" = if (length(cc$tablas_sin_medir)) {
        paste(cc$tablas_sin_medir, collapse = ", ")
      } else "ninguna"
    )
    etiquetas <- c(
      etiquetas,
      stats::setNames(
        as.character(cc$tablas_en_el_numero),
        .texto_celda_publicada("En el n\u00famero")
      )
    )
    cli::cli_dl(etiquetas)
    if (!is.null(cc$advertencia)) cli::cli_alert_warning(cc$advertencia)
  }
  if (nrow(x$dimensiones)) {
    cli::cli_h2("Dimensiones, pesos y aportes")
    .print_data_frame_bytes(x$dimensiones, row.names = FALSE)
  }
  if (nrow(x$invertidas)) {
    cli::cli_h2("Componentes de defecto invertidos")
    .print_data_frame_bytes(x$invertidas, row.names = FALSE)
  }
  if (nrow(x$excluidas)) {
    cli::cli_h2("Componentes excluidos")
    .print_data_frame_bytes(x$excluidas, row.names = FALSE)
  }
  cli::cli_alert_info(x$combinacion_interna)
  cli::cli_alert_warning(x$advertencia_universos)
  invisible(original)
}
