.validar_limite_duplicados <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
      (!is.infinite(x) && x != floor(x))) {
    stop("`", nombre, "` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  if (is.infinite(x)) Inf else as.integer(x)
}

.stringdist_disponible <- function() {
  requireNamespace("stringdist", quietly = TRUE)
}

.vacio_duplicados_aproximados <- function(
    n_filas, columnas, metodo, umbral, muestra, max_pares, max_resultados,
    disponible = TRUE, razon = "", bloque = 1000L, n_bloques = 0L,
    modo_comparacion = "sin_comparacion") {
  pares <- data.frame(
    fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
    tipo_par = character(), metodo = character(), umbral = numeric(),
    evidencia_1 = character(),
    evidencia_2 = character(), proteccion_evidencia = character(),
    stringsAsFactors = FALSE
  )
  posibles <- as.numeric(n_filas) * (as.numeric(n_filas) - 1) / 2
  alcance <- data.frame(
    n_filas_total = as.numeric(n_filas),
    n_filas_muestra = 0,
    n_filas_validas = 0,
    n_pares_posibles = posibles,
    n_pares_comparados = 0,
    n_pares_sin_comparar = posibles,
    n_pares_hallados = 0,
    n_pares_exactos = 0,
    n_pares_aproximados = 0,
    n_pares_mostrados = 0,
    limite_pares = max_pares,
    limite_resultados = max_resultados,
    muestra = muestra,
    muestra_efectiva = 0,
    estrategia = if (disponible) "sin_pares_comparables" else "no_disponible",
    modo_comparacion = modo_comparacion,
    tamano_bloque = bloque,
    n_bloques = n_bloques,
    comparacion_exhaustiva = FALSE,
    muestreado = FALSE,
    truncado = FALSE,
    disponible = disponible,
    razon = razon,
    stringsAsFactors = FALSE
  )
  hallazgos <- data.frame(
    columna = character(), tipo_hallazgo = character(), severidad = character(),
    descripcion = character(), evidencia = character(), sugerencia = character(),
    stringsAsFactors = FALSE
  )
  estructura <- list(
    pares = pares, hallazgos = hallazgos,
    alcance = alcance, columnas = columnas, metodo = metodo,
    umbral = umbral, disponible = disponible, razon = razon,
    proteccion_aplicada = FALSE
  )
  class(estructura) <- c("duplicados_aproximados", "list")
  estructura
}

.validar_bloque_duplicados <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x < 1 || x != floor(x)) {
    stop("`bloque` debe ser un entero positivo y finito.", call. = FALSE)
  }
  as.integer(x)
}

.nuevo_acumulador_duplicados <- function(max_resultados) {
  list(
    pares = data.frame(
      fila_1 = integer(), fila_2 = integer(), distancia = numeric(),
      stringsAsFactors = FALSE
    ),
    n_hallados = 0, n_exactos = 0, n_aproximados = 0,
    max_resultados = max_resultados
  )
}

#' Acumula un lote de pares candidatos ya filtrados
#'
#' El contrato del generador es entregar cada par una sola vez. El acumulador
#' no guarda un índice de todos los pares vistos: conserva sólo los mejores
#' `max_resultados` y sus contadores. Esto permite alimentar la función con
#' lotes provenientes de teselas o de un generador futuro sin que el estado
#' persistente crezca con el número de candidatos.
#' @noRd
.acumular_pares_duplicados <- function(
    acumulador, fila_1, fila_2, distancia) {
  if (!length(fila_1)) return(acumulador)
  if (length(fila_1) != length(fila_2) ||
      length(fila_1) != length(distancia)) {
    stop("El lote de pares debe tener filas y distancias de igual longitud.",
         call. = FALSE)
  }
  validos <- is.finite(distancia)
  if (!any(validos)) return(acumulador)
  lote <- data.frame(
    fila_1 = as.integer(fila_1[validos]),
    fila_2 = as.integer(fila_2[validos]),
    distancia = as.numeric(distancia[validos]),
    stringsAsFactors = FALSE
  )
  acumulador$n_hallados <- acumulador$n_hallados + nrow(lote)
  acumulador$n_exactos <- acumulador$n_exactos + sum(lote$distancia == 0)
  acumulador$n_aproximados <- acumulador$n_aproximados +
    sum(lote$distancia > 0)
  acumulador$pares <- rbind(acumulador$pares, lote)
  acumulador$pares <- acumulador$pares[
    order(acumulador$pares$distancia,
          acumulador$pares$fila_1, acumulador$pares$fila_2),
    , drop = FALSE
  ]
  limite <- acumulador$max_resultados
  if (!is.infinite(limite) && nrow(acumulador$pares) > limite) {
    acumulador$pares <- acumulador$pares[seq_len(limite), , drop = FALSE]
  }
  rownames(acumulador$pares) <- NULL
  acumulador
}

#' Compara por teselas y conserva sólo los mejores resultados
#'
#' La matriz de distancias de cada tesela se descarta antes de pasar a la
#' siguiente. Así la memoria temporal depende de `bloque`, no de la cantidad
#' total de filas, sin cambiar qué pares se comparan.
#' @noRd
.comparar_bloques_duplicados <- function(
    valores, filas, metodo, umbral, bloque, max_resultados) {
  n <- length(valores)
  if (n < 2L) {
    return(list(
      pares = data.frame(fila_1 = integer(), fila_2 = integer(),
                         distancia = numeric(), stringsAsFactors = FALSE),
      n_hallados = 0, n_exactos = 0, n_aproximados = 0,
      n_bloques = 0L
    ))
  }
  inicios <- seq.int(1L, n, by = bloque)
  acumulador <- .nuevo_acumulador_duplicados(max_resultados)
  n_bloques <- 0L
  for (i in seq_along(inicios)) {
    fin_i <- min(n, inicios[[i]] + bloque - 1L)
    filas_i <- inicios[[i]]:fin_i
    for (j in i:length(inicios)) {
      fin_j <- min(n, inicios[[j]] + bloque - 1L)
      filas_j <- inicios[[j]]:fin_j
      matriz <- as.matrix(stringdist::stringdistmatrix(
        valores[filas_i], valores[filas_j], method = metodo
      ))
      n_bloques <- n_bloques + 1L
      candidatas <- if (i == j) {
        which(upper.tri(matriz) & matriz >= 0 & matriz <= umbral,
              arr.ind = TRUE)
      } else {
        which(matriz >= 0 & matriz <= umbral, arr.ind = TRUE)
      }
      if (!nrow(candidatas)) next
      distancias <- as.numeric(matriz[candidatas])
      f1 <- filas[filas_i[candidatas[, 1L]]]
      f2 <- filas[filas_j[candidatas[, 2L]]]
      acumulador <- .acumular_pares_duplicados(
        acumulador, f1, f2, distancias
      )
    }
  }
  list(
    pares = acumulador$pares, n_hallados = acumulador$n_hallados,
    n_exactos = acumulador$n_exactos,
    n_aproximados = acumulador$n_aproximados, n_bloques = n_bloques
  )
}

.indices_duplicados_aproximados <- function(n, limite) {
  if (!n) return(integer())
  if (is.infinite(limite) || n <= limite) return(seq_len(n))
  limite <- as.integer(limite)
  if (limite <= 3L) return(seq_len(limite))
  # Mantener ambos extremos evita que una tabla ordenada pierda un par
  # duplicado que aparezca al principio o al final; el resto queda distribuido.
  interiores <- limite - 4L
  indices <- if (interiores) {
    c(1L, 2L, n - 1L, n,
      as.integer(round(seq.int(3L, n - 2L, length.out = interiores))))
  } else c(1L, 2L, n - 1L, n)
  sort(unique(indices))
}

.columnas_duplicados_aproximados <- function(datos, columnas) {
  if (is.null(columnas)) {
    analizables <- vapply(datos, function(x) {
      (is.character(x) || is.factor(x)) &&
        !is.matrix(x) && !is.list(x)
    }, logical(1L))
    candidatas <- names(datos)[analizables]
    if (length(candidatas) > 2L) {
      nombres <- tolower(gsub("[^[:alnum:]]+", "_", candidatas, perl = TRUE))
      es_identificador <- grepl(
        "(^|_)(id|identificador|codigo|code|uuid|clave|key|llave|nro|numero)(_|$)",
        nombres, perl = TRUE
      )
      candidatas <- candidatas[!es_identificador]
    }
    if (length(candidatas) > 2L) {
      candidatas <- structure(
        character(),
        motivo_sin_columnas = paste0(
          "Hay ", length(candidatas), " columnas de texto; indique `columnas` ",
          "para evitar combinar campos que pueden diluir la similitud."
        )
      )
    }
    return(candidatas)
  }
  if (!is.character(columnas) || !length(columnas) ||
      anyNA(columnas) || any(!nzchar(columnas)) || anyDuplicated(columnas) ||
      any(!columnas %in% names(datos))) {
    stop("`columnas` debe nombrar columnas atomicas existentes y sin repetir.",
         call. = FALSE)
  }
  if (any(vapply(datos[columnas], function(x) is.matrix(x) || is.list(x),
                 logical(1L)))) {
    stop("Las columnas aproximadas no pueden ser matrices ni listas.",
         call. = FALSE)
  }
  columnas
}

.texto_fila_aproximada <- function(datos, columnas, normalizar) {
  valores <- lapply(datos[columnas], function(x) {
    # Reutilizar el saneamiento del perfil: los bytes UTF-8 invalidos no
    # deben abortar una comparacion ni entrar como evidencia.
    salida <- suppressWarnings(as.character(.texto_analizable(x)$valores))
    salida[is.na(salida)] <- ""
    if (normalizar) {
      salida <- trimws(tolower(salida))
      salida <- gsub("[[:space:]]+", " ", salida, perl = TRUE)
    }
    salida
  })
  filas <- do.call(paste, c(valores, sep = " | "))
  presentes <- vapply(seq_along(filas), function(i) {
    any(vapply(valores, function(x) nzchar(x[[i]]), logical(1L)))
  }, logical(1L))
  list(valores = filas, presentes = presentes)
}

.evidencia_fila_aproximada <- function(datos, columnas, fila, protegidas) {
  valores <- vapply(columnas, function(columna) {
    if (columna %in% protegidas) return("[valor protegido]")
    valor <- suppressWarnings(as.character(datos[[columna]][[fila]]))
    if (!length(valor) || is.na(valor)) "[ausente]" else valor
  }, character(1L))
  paste0(columnas, "=", valores, collapse = "; ")
}

.detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.12,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = TRUE, clasificacion = NULL,
    proteger_datos_personales = TRUE, bloque = 1000L) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  columnas <- .columnas_duplicados_aproximados(datos, columnas)
  muestra <- .validar_limite_duplicados(muestra, "muestra")
  max_pares <- .validar_limite_duplicados(max_pares, "max_pares")
  max_resultados <- .validar_limite_duplicados(max_resultados, "max_resultados")
  bloque <- .validar_bloque_duplicados(bloque)
  if (!is.character(metodo) || length(metodo) != 1L || is.na(metodo) ||
      !nzchar(metodo) || !metodo %in% c(
        "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine",
        "jaccard", "jw", "soundex"
      )) {
    stop(
      "`metodo` debe ser una medida admitida: osa, lv, dl, hamming, lcs, ",
      "qgram, cosine, jaccard, jw o soundex.", call. = FALSE
    )
  }
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral < 0) {
    stop("`umbral` debe ser un numero finito no negativo.", call. = FALSE)
  }
  if (!is.logical(normalizar) || length(normalizar) != 1L || is.na(normalizar) ||
      !is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`normalizar` y `proteger_datos_personales` deben ser l\u00f3gicos escalares.",
         call. = FALSE)
  }
  if (!.stringdist_disponible()) {
    return(.vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = FALSE, bloque = bloque,
      razon = "No esta instalado el paquete opcional 'stringdist'."
    ))
  }
  if (!length(columnas)) {
    motivo <- attr(columnas, "motivo_sin_columnas", exact = TRUE)
    if (is.null(motivo)) {
      motivo <- "No hay columnas de texto comparables; indique `columnas` explicitamente."
    }
    return(.vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque,
      razon = motivo
    ))
  }
  if (is.null(clasificacion)) {
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = TRUE,
      duplicados_aproximados = FALSE
    )
    clasificacion <- perfil$datos_personales
  }
  protegidas <- if (proteger_datos_personales) {
    .columnas_personales_protegidas(clasificacion)
  } else character()
  textos <- .texto_fila_aproximada(datos, columnas, normalizar)
  max_filas_por_pares <- if (is.infinite(max_pares)) Inf else {
    floor((1 + sqrt(1 + 8 * max_pares)) / 2)
  }
  limite_filas <- min(muestra, max_filas_por_pares)
  indices <- .indices_duplicados_aproximados(nrow(datos), limite_filas)
  seleccion <- if (limite_filas <= 3L) "primeras_n_filas" else {
    "muestra_sistematica"
  }
  estrategia <- if (length(indices) >= nrow(datos)) {
    "todas_las_filas_en_bloques"
  } else if (nrow(datos) > muestra &&
             !is.infinite(max_filas_por_pares) &&
             max_filas_por_pares < muestra) {
    paste0(seleccion, "_por_muestra_y_limite_de_pares")
  } else if (nrow(datos) > muestra) {
    paste0(seleccion, "_por_muestra")
  } else {
    paste0(seleccion, "_por_limite_de_pares")
  }
  validos <- indices[textos$presentes[indices]]
  n_pares_comparados <- as.numeric(length(validos)) *
    (as.numeric(length(validos)) - 1) / 2
  posibles <- as.numeric(nrow(datos)) * (as.numeric(nrow(datos)) - 1) / 2
  if (length(validos) < 2L) {
    resultado <- .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque,
      razon = "No hay dos filas con valores comparables."
    )
    resultado$alcance$n_filas_muestra <- length(indices)
    resultado$alcance$n_filas_validas <- length(validos)
    resultado$alcance$n_pares_comparados <- n_pares_comparados
    resultado$alcance$n_pares_sin_comparar <- posibles
    resultado$alcance$muestreado <- length(indices) < nrow(datos)
    resultado$alcance$estrategia <- estrategia
    resultado$alcance$muestra_efectiva <- length(indices)
    resultado$alcance$modo_comparacion <- "sin_pares_comparables"
    resultado$alcance$tamano_bloque <- bloque
    return(resultado)
  }
  bloques <- .comparar_bloques_duplicados(
    textos$valores[validos], validos, metodo, umbral, bloque, max_resultados
  )
  n_hallados <- bloques$n_hallados
  n_exactos <- bloques$n_exactos
  n_aproximados <- bloques$n_aproximados
  mostrados <- nrow(bloques$pares)
  pares <- if (mostrados) {
    distancias <- bloques$pares$distancia
    data.frame(
      fila_1 = bloques$pares$fila_1,
      fila_2 = bloques$pares$fila_2,
      distancia = distancias,
      tipo_par = ifelse(distancias == 0, "exacto", "aproximado"),
      metodo = metodo, umbral = umbral,
      evidencia_1 = vapply(
        bloques$pares$fila_1,
        function(fila) .evidencia_fila_aproximada(
          datos, columnas, fila, protegidas
        ), character(1L)
      ),
      evidencia_2 = vapply(
        bloques$pares$fila_2,
        function(fila) .evidencia_fila_aproximada(
          datos, columnas, fila, protegidas
        ), character(1L)
      ),
      proteccion_evidencia = if (length(protegidas)) {
        rep("[valores personales protegidos]", mostrados)
      } else rep("ninguna", mostrados),
      stringsAsFactors = FALSE
    )
  } else {
    .vacio_duplicados_aproximados(
      nrow(datos), columnas, metodo, umbral, muestra, max_pares,
      max_resultados, disponible = TRUE, bloque = bloque
    )$pares
  }
  hallazgos <- if (nrow(pares)) {
    do.call(rbind, lapply(seq_len(nrow(pares)), function(i) {
      exacto <- identical(pares$tipo_par[[i]], "exacto")
      .nuevo_hallazgo(
        paste(columnas, collapse = ", "),
        if (exacto) "duplicados_exactos_columnas" else "duplicados_aproximados",
        "sospechoso",
        if (exacto) {
          "Dos filas tienen los mismos valores en las columnas comparadas; esto no demuestra identidad."
        } else {
          "Dos filas presentan similitud; esto no demuestra identidad."
        },
        paste0(
          "Filas ", pares$fila_1[[i]], " y ", pares$fila_2[[i]],
          "; distancia ", format(pares$distancia[[i]], digits = 6),
          " con ", pares$metodo[[i]], " (umbral ", pares$umbral[[i]], "). ",
          pares$evidencia_1[[i]], " / ", pares$evidencia_2[[i]]
        ),
        "Revisar manualmente; no eliminar ni fusionar filas por esta senal."
      )
    }))
  } else {
    data.frame(
      columna = character(), tipo_hallazgo = character(), severidad = character(),
      descripcion = character(), evidencia = character(), sugerencia = character(),
      stringsAsFactors = FALSE
    )
  }
  hallazgos$severidad <- factor(
    hallazgos$severidad, levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  alcance <- data.frame(
    n_filas_total = as.numeric(nrow(datos)),
    n_filas_muestra = length(indices), n_filas_validas = length(validos),
    n_pares_posibles = posibles, n_pares_comparados = n_pares_comparados,
    n_pares_sin_comparar = max(0, posibles - n_pares_comparados),
    n_pares_hallados = n_hallados, n_pares_mostrados = mostrados,
    n_pares_exactos = n_exactos,
    n_pares_aproximados = n_aproximados,
    limite_pares = max_pares, limite_resultados = max_resultados,
    muestra = muestra,
    muestra_efectiva = length(indices),
    estrategia = estrategia,
    modo_comparacion = if (length(indices) >= nrow(datos)) {
      "exhaustiva_por_bloques"
    } else "muestreada_por_bloques",
    tamano_bloque = bloque,
    n_bloques = bloques$n_bloques,
    comparacion_exhaustiva = length(indices) >= nrow(datos),
    muestreado = length(indices) < nrow(datos), truncado = mostrados < n_hallados,
    disponible = TRUE, razon = "", stringsAsFactors = FALSE
  )
  estructura <- list(
    pares = pares, hallazgos = hallazgos, alcance = alcance,
    columnas = columnas, metodo = metodo, umbral = umbral,
    disponible = TRUE, razon = "",
    proteccion_aplicada = proteger_datos_personales
  )
  class(estructura) <- c("duplicados_aproximados", "list")
  estructura
}

#' Detectar pares de filas con similitud aproximada
#'
#' Compara filas seleccionadas de una tabla con `stringdist` y devuelve pares cuya
#' distancia esta bajo el umbral. El resultado describe similitud, distancia,
#' medida y alcance; nunca afirma que dos filas representen la misma entidad.
#' Por omision se combinan como maximo dos columnas de texto o factores, despues
#' de excluir nombres que parecen identificadores (`id`, `codigo`, `uuid`, entre
#' otros). Si quedan mas de dos columnas, la funcion pide indicar `columnas`
#' explicitamente en vez de mezclar campos que pueden diluir la similitud. La
#' medida predeterminada es Jaro--Winkler (`"jw"`), adecuada para transposiciones
#' y pequenas erratas; el umbral predeterminado `0.12` es deliberadamente mas
#' conservador que `0.15`, que sobre cadenas cortas y estructuradas produce
#' demasiados pares. Ambos argumentos se pueden cambiar.
#'
#' Los pares con distancia cero se incluyen como `tipo_par = "exacto"`; los
#' restantes son `"aproximado"`. Ninguno demuestra identidad.
#'
#' La comparacion usa teselas de `bloque` filas: cada matriz temporal se
#' descarta antes de continuar, por lo que la memoria no crece con el tamaño
#' de la tabla. El recorrido es exacto para las filas seleccionadas. `muestra`
#' y `max_pares` sólo recortan tablas que superarían el tope temporal medido;
#' el objeto informa cuantos pares eran posibles, cuantos se compararon, el
#' modo, el tamaño de las teselas y los que quedaron fuera. Solo se muestran
#' `max_resultados` coincidencias; el truncamiento tambien queda declarado.
#'
#' `stringdist` es una dependencia opcional. Si no esta instalado, la funcion
#' devuelve un objeto con `disponible = FALSE`, una tabla vacia y el motivo
#' explicito; no falla ni presenta silencio como si se hubieran comparado todos
#' los pares.
#'
#' @param datos Tabla con una fila por entidad observada.
#' @param columnas Columnas atomicas a combinar. `NULL` aplica la seleccion
#'   automatica descrita arriba; no se incluyen matrices ni listas.
#' @param metodo Medida admitida por `stringdist::stringdistmatrix()`. Por
#'   defecto, `"jw"`.
#' @param umbral Distancia maxima para informar un par. Por defecto `0.12`.
#' @param muestra Maximo de filas candidatas; `Inf` usa todas, sujeto a
#'   `max_pares`.
#' @param max_pares Maximo de pares comparados. Por defecto `50000000`, que
#'   permite recorrer exhaustivamente hasta 10.000 filas con el método y el
#'   bloque predeterminados; se puede reducir para limitar el tiempo.
#' @param max_resultados Maximo de pares devueltos. Por defecto `100`.
#' @param bloque Cantidad de filas por tesela de comparación. Por defecto
#'   `1000`; controla la memoria temporal, no el número de pares comparados.
#' @param normalizar Si se recortan espacios, se pasa a minusculas y se
#'   colapsan espacios antes de calcular la distancia.
#' @param perfil Perfil de los mismos datos para reutilizar su clasificacion de
#'   datos personales y no volver a inferirla.
#' @param proteger_datos_personales Si la evidencia de columnas protegidas se
#'   reemplaza por `[valor protegido]`. La supresion queda indicada en cada par.
#'
#' @return Lista de clase `duplicados_aproximados` con `pares`, `hallazgos`,
#'   `alcance`, `columnas`, `metodo`, `umbral`, `disponible` y `razon`.
#' @export
#' @seealso [perfilar()], [reportar()], [planificar_limpieza()]
#'
#' @examples
#' datos <- data.frame(
#'   nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
#'   domicilio = c("Calle 1", "Calle 1", "Calle 9")
#' )
#' pares <- detectar_duplicados_aproximados(datos)
#' if (!pares$disponible) pares$razon
detectar_duplicados_aproximados <- function(
    datos, columnas = NULL, metodo = "jw", umbral = 0.12,
    muestra = Inf, max_pares = 50000000L, max_resultados = 100L,
    normalizar = TRUE, perfil = NULL, proteger_datos_personales = TRUE,
    bloque = 1000L) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
      !identical(names(datos), perfil$columnas$columna))) {
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  if (!is.null(perfil)) {
    return(.detectar_duplicados_aproximados(
      datos, columnas, metodo, umbral, muestra, max_pares, max_resultados,
      normalizar, clasificacion = perfil$datos_personales,
      proteger_datos_personales = proteger_datos_personales, bloque = bloque
    ))
  }
  .detectar_duplicados_aproximados(
    datos, columnas, metodo, umbral, muestra, max_pares, max_resultados,
    normalizar, proteger_datos_personales = proteger_datos_personales,
    bloque = bloque
  )
}
