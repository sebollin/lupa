.escalas_variables <- c(
  "nominal", "ordinal", "discreta", "continua", "binaria", "temporal",
  "desconocida"
)

.normalizar_metadatos_variables <- function(metadatos, nombres) {
  if (is.null(metadatos)) {
    return(data.frame(columna = character(), stringsAsFactors = FALSE))
  }
  if (!inherits(metadatos, "data.frame") || !"columna" %in% names(metadatos) ||
      anyNA(metadatos$columna) || anyDuplicated(metadatos$columna) ||
      any(!metadatos$columna %in% nombres)) {
    stop("`metadatos` debe tener una fila unica por columna existente.",
         call. = FALSE)
  }
  permitidas <- c("columna", "escala", "rol", "confianza", "confirmada",
                  "unidad", "niveles")
  if (any(!names(metadatos) %in% permitidas)) {
    stop("`metadatos` contiene campos no reconocidos.", call. = FALSE)
  }
  if ("escala" %in% names(metadatos) &&
      any(!is.na(metadatos$escala) & !metadatos$escala %in% .escalas_variables)) {
    stop("`escala` contiene una escala no reconocida.", call. = FALSE)
  }
  if ("confirmada" %in% names(metadatos) &&
      (!is.logical(metadatos$confirmada) || anyNA(metadatos$confirmada))) {
    stop("`confirmada` debe ser logica y no contener NA.", call. = FALSE)
  }
  if ("confianza" %in% names(metadatos) &&
      any(!is.na(metadatos$confianza) &
          (!is.finite(metadatos$confianza) |
             metadatos$confianza < 0 | metadatos$confianza > 1))) {
    stop("`confianza` debe estar en [0, 1].", call. = FALSE)
  }
  metadatos
}

.propuesta_escala <- function(x, tipo_implicito) {
  if (is.matrix(x)) return(list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE,
    evidencia = paste0(
      "La columna es matricial; sus componentes deben separarse antes de ",
      "declarar una escala de medici\u00f3n."
    )
  ))
  medida <- attr(x, "measure", exact = TRUE)
  if (is.character(medida) && length(medida) == 1L &&
      medida %in% c("nominal", "ordinal", "scale")) {
    escala <- c(nominal = "nominal", ordinal = "ordinal", scale = "continua")[[medida]]
    return(list(
      escala = escala, rol = if (escala %in% c("nominal", "ordinal")) {
        "categoria"
      } else "medida",
      confianza = 1, confirmada = TRUE,
      evidencia = paste0("Metadato de escala declarado: ", medida, ".")
    ))
  }
  if (is.ordered(x)) return(list(
    escala = "ordinal", rol = "categoria", confianza = 1, confirmada = TRUE,
    evidencia = "La clase ordered declara niveles y orden."
  ))
  if (is.factor(x)) return(list(
    escala = "nominal", rol = "categoria", confianza = 1, confirmada = TRUE,
    evidencia = "La clase factor declara un dominio sin orden."
  ))
  if (is.logical(x)) return(list(
    escala = "binaria", rol = "indicador", confianza = 1, confirmada = TRUE,
    evidencia = "La clase logical declara dos estados."
  ))
  if (inherits(x, c("Date", "POSIXt"))) return(list(
    escala = "temporal", rol = "fecha", confianza = 1, confirmada = TRUE,
    evidencia = "La clase de fecha declara semantica temporal."
  ))
  unidad <- attr(x, "units", exact = TRUE)
  if (is.numeric(x) && !is.null(unidad)) {
    presentes <- x[!is.na(x) & is.finite(x)]
    discreta <- length(presentes) && all(presentes == floor(presentes))
    return(list(
      escala = if (discreta) "discreta" else "continua", rol = "medida",
      confianza = 1, confirmada = TRUE,
      evidencia = "La unidad declarada confirma una magnitud cuantitativa."
    ))
  }
  if (!is.null(attr(x, "labels", exact = TRUE))) return(list(
    escala = "nominal", rol = "categoria", confianza = 0.85,
    confirmada = FALSE,
    evidencia = "Las etiquetas sugieren categorias, pero no declaran orden."
  ))
  presentes <- x[!is.na(x)]
  distintos <- length(unique(presentes))
  if (tipo_implicito == "identificador") return(list(
    escala = "nominal", rol = "identificador", confianza = 0.9,
    confirmada = FALSE,
    evidencia = "La forma y cardinalidad sugieren un identificador; requiere confirmacion."
  ))
  if (tipo_implicito %in% c("fecha", "fecha-hora")) return(list(
    escala = "temporal", rol = "fecha", confianza = 0.85,
    confirmada = FALSE,
    evidencia = "Los valores son compatibles con fecha; el almacenamiento no lo declara."
  ))
  if (tipo_implicito == "logico" || distintos == 2L) return(list(
    escala = "binaria", rol = "indicador", confianza = 0.8,
    confirmada = FALSE,
    evidencia = "Se observaron dos estados; pueden ser codigos y requieren confirmacion."
  ))
  if (is.integer(x) || (is.numeric(x) && length(presentes) &&
      all(presentes == floor(presentes)))) return(list(
    escala = "discreta", rol = "medida", confianza = 0.65,
    confirmada = FALSE,
    evidencia = "Los valores son enteros; tambien podrian ser codigos nominales."
  ))
  if (is.numeric(x)) return(list(
    escala = "continua", rol = "medida", confianza = 0.65,
    confirmada = FALSE,
    evidencia = "El almacenamiento admite decimales; la semantica no esta declarada."
  ))
  if (is.character(x)) return(list(
    escala = "nominal", rol = "categoria", confianza = 0.55,
    confirmada = FALSE,
    evidencia = "El texto sugiere categorias, pero puede ser texto libre."
  ))
  list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE, evidencia = "El tipo no aporta una escala de medicion."
  )
}

.metricas_por_escala <- function(escala, rol) {
  if (rol == "identificador") return("Unicidad; integridad referencial")
  switch(
    escala,
    nominal = "ValoresPosiblesPorExtension; Formato",
    ordinal = "ValoresPosiblesPorExtension; orden y niveles",
    discreta = "ValoresPosiblesPorComprension; distribucion",
    continua = "Distribucion; ErrorEstandar; Escala",
    binaria = "ValoresPosiblesPorExtension; NoNulo",
    temporal = "Formato; vigencia; oportunidad",
    "Requiere declaracion"
  )
}

#' Proponer escalas y roles de las variables
#'
#' Separa el tipo de almacenamiento, el tipo implícito y la escala de medición.
#' Una propuesta basada sólo en valores nunca queda confirmada: `1, 2, 3` puede
#' ser conteo, código o nivel. Las clases `ordered`, `factor`, `logical`, las
#' clases temporales y el metadato `measure` sí constituyen declaraciones.
#'
#' `metadatos` permite confirmar o corregir la propuesta con una tabla editable.
#' Debe contener `columna` y puede incluir `escala`, `rol`, `confianza`,
#' `confirmada`, `unidad` y una columna de lista `niveles`. Las escalas válidas
#' son nominal, ordinal, discreta, continua, binaria, temporal y desconocida.
#'
#' Los niveles declarados y observados se conservan como columnas de lista. Los
#' niveles ausentes son una observación, no prueba de error. Si la evidencia de
#' dato personal activa la protección, los niveles concretos se protegen.
#'
#' @param datos Tabla que se desea clasificar.
#' @param perfil Perfil opcional de los mismos datos.
#' @param metadatos Declaraciones opcionales por columna.
#' @param max_niveles Máximo de niveles guardados en cada lista.
#' @param muestra Máximo de valores usados para enumerar niveles no declarados;
#'   los niveles ausentes de factores y metadatos se verifican sobre toda la
#'   columna.
#' @param proteger_datos_personales Si se ocultan niveles concretos de columnas
#'   cuya clasificación activa protección automática. Véase [perfilar()].
#'
#' @return Data frame S3 `clasificacion_variables`, editable y filtrable.
#' @export
#' @seealso [analizar()], [distribucion_valores()], [proponer_modelo()]
#'
#' @examples
#' d <- data.frame(
#'   prioridad = ordered(c("baja", "alta"), levels = c("baja", "media", "alta")),
#'   cantidad = c(1L, 2L)
#' )
#' clasificar_variables(d)
clasificar_variables <- function(datos, perfil = NULL, metadatos = NULL,
                                  max_niveles = 100L,
                                  muestra = 1e5,
                                  proteger_datos_personales = TRUE) {
  .validar_datos_tabla(datos)
  .validar_perfil_de(perfil, datos)
  datos <- .tabla_base(datos)
  if (!is.null(metadatos)) metadatos <- .tabla_base(metadatos)
  max_niveles <- .validar_entero_positivo(max_niveles, "max_niveles")
  limite <- .validar_muestra(muestra)
  if (!is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`proteger_datos_personales` debe ser TRUE o FALSE.", call. = FALSE)
  }
  metadatos <- .normalizar_metadatos_variables(metadatos, names(datos))
  personales <- if (proteger_datos_personales) {
    .columnas_personales_rapidas(datos, perfil)
  } else character()
  filas <- vector("list", ncol(datos))
  niveles_declarados <- niveles_observados <- niveles_ausentes <-
    vector("list", ncol(datos))
  for (i in seq_along(datos)) {
    x <- datos[[i]]
    nombre <- names(datos)[[i]]
    tipo_implicito <- if (!is.null(perfil)) {
      perfil$columnas$tipo_inferido[[i]]
    } else if (is.character(x) || is.factor(x)) {
      inferir_tipo(x)$tipo
    } else .tipo_declarado(x)
    propuesta <- .propuesta_escala(x, tipo_implicito)
    meta <- metadatos[metadatos$columna == nombre, , drop = FALSE]
    if (nrow(meta)) {
      declaracion <- FALSE
      if ("escala" %in% names(meta) && !is.na(meta$escala[[1L]])) {
        propuesta$escala <- meta$escala[[1L]]
        declaracion <- TRUE
      }
      if ("rol" %in% names(meta) && !is.na(meta$rol[[1L]])) {
        propuesta$rol <- as.character(meta$rol[[1L]])
        declaracion <- TRUE
      }
      if ("confirmada" %in% names(meta)) {
        propuesta$confirmada <- meta$confirmada[[1L]]
        declaracion <- TRUE
      } else if ("escala" %in% names(meta) && !is.na(meta$escala[[1L]])) {
        propuesta$confirmada <- TRUE
      }
      if ("confianza" %in% names(meta) && !is.na(meta$confianza[[1L]])) {
        propuesta$confianza <- meta$confianza[[1L]]
        declaracion <- TRUE
      } else if (declaracion && propuesta$confirmada) {
        propuesta$confianza <- 1
      }
      if (declaracion) {
        propuesta$evidencia <- "Escala o rol declarados en metadatos provistos."
      }
    }
    declarados <- if (nrow(meta) && "niveles" %in% names(meta) &&
      length(meta$niveles[[1L]])) {
      as.character(meta$niveles[[1L]])
    } else if (is.factor(x)) levels(x) else character()
    guardar_niveles <- propuesta$escala %in% c("nominal", "ordinal", "binaria")
    muestreo <- .muestrear_vector(x, limite)
    muestra_segura <- .texto_analizable(muestreo$valores)$valores
    muestra_texto <- as.character(muestra_segura[!is.na(muestra_segura)])
    observados <- if (!guardar_niveles) {
      character()
    } else if (is.factor(x)) {
      niveles_x <- levels(x)
      niveles_x[tabulate(as.integer(x), nbins = length(niveles_x)) > 0L]
    } else {
      unique(muestra_texto)
    }
    presentes_declarados <- if (length(declarados)) {
      valores_seguros <- .texto_analizable(x)$valores
      declarados %in% as.character(valores_seguros[!is.na(valores_seguros)])
    } else logical()
    ausentes <- declarados[!presentes_declarados]
    observados <- unique(c(declarados[presentes_declarados], observados))
    n_declarados <- length(declarados)
    n_observados <- if (!guardar_niveles) {
      0L
    } else if (!is.null(perfil)) {
      perfil$columnas$n_distintos[[i]]
    } else if (is.factor(x)) {
      sum(tabulate(as.integer(x), nbins = length(levels(x))) > 0L)
    } else length(unique(muestra_texto))
    n_ausentes <- length(ausentes)
    truncado <- any(c(length(declarados), length(observados), length(ausentes)) >
                     max_niveles)
    declarados <- utils::head(declarados, max_niveles)
    observados <- utils::head(observados, max_niveles)
    ausentes <- utils::head(ausentes, max_niveles)
    if (nombre %in% personales) {
      proteger <- function(x) if (length(x)) "[valor protegido]" else character()
      declarados <- proteger(declarados)
      observados <- proteger(observados)
      ausentes <- proteger(ausentes)
    }
    niveles_declarados[[i]] <- declarados
    niveles_observados[[i]] <- observados
    niveles_ausentes[[i]] <- ausentes
    unidad <- if (nrow(meta) && "unidad" %in% names(meta)) {
      as.character(meta$unidad[[1L]])
    } else {
      valor <- attr(x, "units", exact = TRUE)
      if (is.null(valor)) NA_character_ else as.character(valor[[1L]])
    }
    filas[[i]] <- data.frame(
      columna = nombre, tipo_almacenamiento = .tipo_declarado(x),
      tipo_implicito = tipo_implicito, escala_propuesta = propuesta$escala,
      rol = propuesta$rol, confianza = propuesta$confianza,
      evidencia = propuesta$evidencia, confirmada = propuesta$confirmada,
      unidad = unidad, n_niveles_declarados = n_declarados,
      n_niveles_observados = n_observados,
      n_niveles_ausentes = n_ausentes,
      niveles_muestreados = muestreo$muestreado,
      niveles_truncados = truncado,
      metricas_sugeridas = .metricas_por_escala(propuesta$escala, propuesta$rol),
      stringsAsFactors = FALSE
    )
  }
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo_almacenamiento = character(),
    tipo_implicito = character(), escala_propuesta = character(), rol = character(),
    confianza = numeric(), evidencia = character(), confirmada = logical(),
    unidad = character(), n_niveles_declarados = integer(),
    n_niveles_observados = integer(), n_niveles_ausentes = integer(),
    niveles_muestreados = logical(), niveles_truncados = logical(),
    metricas_sugeridas = character(),
    stringsAsFactors = FALSE
  )
  resultado$niveles_declarados <- I(niveles_declarados)
  resultado$niveles_observados <- I(niveles_observados)
  resultado$niveles_ausentes <- I(niveles_ausentes)
  rownames(resultado) <- NULL
  class(resultado) <- c("clasificacion_variables", "data.frame")
  resultado
}
