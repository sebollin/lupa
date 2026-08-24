.umbrales_benford <- function() {
  list(
    minimo_ordenes_magnitud = 3,
    minima_proporcion_positivos = 1,
    minimo_observaciones_utilizables = 100L,
    nivel_significacion = 0.01
  )
}

.columna_candidata_benford <- function(x) {
  is.numeric(x) &&
    !inherits(x, c("Date", "POSIXt", "difftime", "integer64")) &&
    sum(is.finite(x)) >= 50L
}

.primer_digito_significativo <- function(x) {
  exponente <- floor(log10(x))
  digito <- floor(10 ^ (log10(x) - exponente) + 1e-12)
  as.integer(pmin(9, pmax(1, digito)))
}

.texto_distribucion_benford <- function(distribucion) {
  paste0(
    distribucion$digito, ":",
    sprintf("%.3f", distribucion$proporcion_observada), "/",
    sprintf("%.3f", distribucion$proporcion_esperada),
    collapse = "; "
  )
}

.precondiciones_benford <- function(variacion, es_identificador,
                                    n_positivos, proporcion_positivos,
                                    ordenes, umbrales) {
  fallas <- character()
  if (!variacion) {
    fallas <- c(fallas, "sin_variacion")
  }
  if (es_identificador) {
    fallas <- c(fallas, "parece_identificador")
  }
  if (n_positivos < umbrales$minimo_observaciones_utilizables) {
    fallas <- c(fallas, "observaciones_utilizables_insuficientes")
  }
  if (!is.finite(proporcion_positivos) ||
      proporcion_positivos < umbrales$minima_proporcion_positivos) {
    fallas <- c(fallas, "proporcion_positivos_insuficiente")
  }
  if (!is.finite(ordenes) ||
      ordenes < umbrales$minimo_ordenes_magnitud) {
    fallas <- c(fallas, "ordenes_magnitud_insuficientes")
  }
  fallas
}

# Un identificador no es una magnitud, y Benford describe magnitudes que salen
# de procesos multiplicativos. Que `MotId` se desvie de la distribucion es
# cierto y no significa nada.
#
# Esta guarda exigia una corrida consecutiva SIN HUECOS, y un identificador real
# tiene huecos: los que se dieron de baja. Un `MotId` que va de 1 a 4557 sobre
# 3.159 filas no la pasaba, y Benford se corria igual.
#
# Lo que lo describe es la DENSIDAD: una numeracion ocupa un tramo compacto de
# los enteros -0,69 en ese caso- y una magnitud se reparte por varios ordenes de
# magnitud -0,00005 para montos entre 9 y 9.999.999-. La unicidad no sirve: un
# monto tambien es casi unico.

.parece_correlativo_benford <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  valores <- as.numeric(x[is.finite(x)])
  if (length(valores) < 2L) return(FALSE)
  if (any(valores != floor(valores))) return(FALSE)
  distintos <- sort(unique(valores))
  if (length(distintos) < .MIN_DISTINTOS_NUMERACION) return(FALSE)
  rango <- distintos[[length(distintos)]] - distintos[[1L]] + 1
  if (!is.finite(rango) || rango <= 0) return(FALSE)
  if (length(distintos) / rango < .MIN_DENSIDAD_NUMERACION) return(FALSE)
  # La misma segunda senal que usa la guarda de los limites de Tukey: un valor
  # separado del resto por un hueco desproporcionado no es cola de la
  # numeracion, y una columna que lo tiene vuelve a evaluarse.
  huecos <- diff(distintos)
  tipico <- stats::median(huecos)
  if (!is.finite(tipico) || tipico <= 0) tipico <- 1
  !(length(distintos) >= 3L && max(huecos) >= .FACTOR_SALTO_ESCALA * tipico &&
      max(huecos) > 1)
}

.resultado_benford_columna <- function(x, nombre, tipo_inferido,
                                       posible_identificador,
                                       umbrales = .umbrales_benford(),
                                       clave_declarada = FALSE) {
  finitos <- if (is.numeric(x)) {
    as.numeric(x[is.finite(x)])
  } else {
    numeric()
  }
  positivos <- finitos[finitos > 0]
  n_finitos <- length(finitos)
  n_positivos <- length(positivos)
  proporcion_positivos <- if (n_finitos) n_positivos / n_finitos else NA_real_
  variacion <- length(unique(finitos)) > 1L
  ordenes <- if (n_positivos && variacion) {
    log10(max(positivos)) - log10(min(positivos))
  } else {
    NA_real_
  }
  es_identificador <- identical(as.character(tipo_inferido), "identificador") ||
    isTRUE(posible_identificador) || .parece_correlativo_benford(x)

  fallas <- .precondiciones_benford(
    variacion, es_identificador, n_positivos, proporcion_positivos,
    ordenes, umbrales
  )

  base <- list(
    columna = nombre,
    aplica = !length(fallas),
    precondiciones_fallidas = fallas,
    n_finitos = as.integer(n_finitos),
    n_positivos = as.integer(n_positivos),
    proporcion_positivos = proporcion_positivos,
    ordenes_magnitud = ordenes,
    parece_identificador = es_identificador,
    clave_declarada = isTRUE(clave_declarada)
  )
  if (length(fallas)) return(base)

  digitos <- .primer_digito_significativo(positivos)
  observados <- tabulate(digitos, nbins = 9L)
  proporcion_esperada <- log10(1 + 1 / seq_len(9L))
  esperados <- n_positivos * proporcion_esperada
  estadistico <- sum((observados - esperados) ^ 2 / esperados)
  p_valor <- stats::pchisq(estadistico, df = 8L, lower.tail = FALSE)
  distribucion <- data.frame(
    digito = seq_len(9L),
    n_observado = as.integer(observados),
    proporcion_observada = as.numeric(observados / n_positivos),
    proporcion_esperada = as.numeric(proporcion_esperada),
    n_esperado = as.numeric(esperados),
    stringsAsFactors = FALSE
  )
  c(base, list(
    metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
    estadistico = as.numeric(estadistico),
    grados_libertad = 8L,
    p_valor = as.numeric(p_valor),
    distribucion = distribucion,
    desviacion = isTRUE(p_valor < umbrales$nivel_significacion)
  ))
}

.motivo_no_aplica_benford <- function(resultado, umbrales) {
  # «Parece un identificador» y «se declaro como clave» son dos afirmaciones
  # distintas: la primera es una inferencia del paquete y la segunda un hecho que
  # trajo el usuario. Publicar la primera cuando corresponde la segunda le
  # atribuye al paquete una deduccion que no hizo, y ademas invita a discutirle
  # el criterio cuando no hubo criterio.
  etiquetas <- c(
    sin_variacion = "sin variacion",
    parece_identificador = if (isTRUE(resultado$clave_declarada)) {
      # Se probo agregar un aviso cuando la columna declarada como clave se
      # reparte por muchos ordenes de magnitud, para que una declaracion
      # equivocada -un monto declarado como clave- no se lleve el analisis en
      # silencio. **Se retiro midiendo**: una clave dispersa real de 1 a 600.000
      # da 4,3 ordenes y un monto da 3,3, asi que el aviso salta en las dos y
      # deja de significar algo. Es la misma razon por la que la densidad no
      # servia para reconocer una clave.
      #
      # Lo que si cubre el caso comun: si la columna declarada repite valores,
      # `clave_no_unica` lo informa con severidad `error`.
      "la clave fue declarada, asi que la columna identifica filas y no es una magnitud"
    } else {
      "parece un identificador (tipo_inferido, posible_identificador o secuencia correlativa)"
    },
    observaciones_utilizables_insuficientes = paste0(
      "observaciones positivas utilizables ", resultado$n_positivos,
      " < ", umbrales$minimo_observaciones_utilizables
    ),
    proporcion_positivos_insuficiente = paste0(
      "proporcion de positivos ", sprintf("%.3f", resultado$proporcion_positivos),
      " < ", sprintf("%.3f", umbrales$minima_proporcion_positivos)
    ),
    ordenes_magnitud_insuficientes = paste0(
      "ordenes de magnitud log10(max/min) ",
      if (is.finite(resultado$ordenes_magnitud)) {
        sprintf("%.3f", resultado$ordenes_magnitud)
      } else {
        "no calculables"
      },
      " < ", umbrales$minimo_ordenes_magnitud
    )
  )
  paste0(
    "No aplica la ley de Benford. Precondiciones fallidas: ",
    paste(unname(etiquetas[resultado$precondiciones_fallidas]), collapse = "; "),
    "."
  )
}

.diagnosticar_benford <- function(datos, columnas, hallazgos,
                                  clave_declarada = NULL) {
  candidatas <- which(vapply(datos, .columna_candidata_benford, logical(1L)))
  if (!length(candidatas)) {
    return(list(hallazgos = list(), cobertura = .cobertura_diagnosticos_vacia(),
                meta = NULL))
  }

  umbrales <- .umbrales_benford()
  identificadores <- as.character(hallazgos$columna[
    hallazgos$tipo_hallazgo == "posible_identificador"
  ])
  # Una clave que el usuario DECLARO no necesita que se infiera nada: dijo que
  # identifica una fila, y una numeracion no tiene distribucion que analizar.
  #
  # Esto cierra el caso que motivo -y tumbo- la regla de la clave dispersa: ahi
  # se intentaba adivinar cual columna era clave por la forma de sus valores, y
  # el criterio terminaba dependiendo de cuantas filas se habian cargado y
  # callando magnitudes reales. Declarada, la respuesta ya esta, no depende del
  # tamano de la tabla, y no calla nada: si la columna es de verdad una clave,
  # Benford sobre ella no significaba nada.
  #
  # Lo mismo vale para la clave leida del catalogo de la base, que llega por
  # aca cuando quien perfila la pasa.
  identificadores <- unique(c(identificadores, as.character(clave_declarada)))
  resultados <- lapply(candidatas, function(i) {
    .resultado_benford_columna(
      datos[[i]], names(datos)[[i]], columnas$tipo_inferido[[i]],
      names(datos)[[i]] %in% identificadores, umbrales,
      clave_declarada = names(datos)[[i]] %in% as.character(clave_declarada)
    )
  })
  names(resultados) <- make.unique(names(datos)[candidatas])

  cobertura <- lapply(resultados, function(resultado) {
    if (isTRUE(resultado$aplica)) return(NULL)
    .nuevo_diagnostico_no_evaluado(
      "ley_benford", resultado$columna,
      .motivo_no_aplica_benford(resultado, umbrales),
      paste0(
        "No interpretar una distribucion de primeros digitos. Benford requiere ",
        "montos positivos no asignados, al menos ",
        umbrales$minimo_observaciones_utilizables,
        " observaciones y al menos ", umbrales$minimo_ordenes_magnitud,
        " ordenes de magnitud."
      )
    )
  })
  cobertura <- cobertura[!vapply(cobertura, is.null, logical(1L))]
  cobertura <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_diagnosticos_vacia()
  }

  nuevos <- lapply(resultados, function(resultado) {
    if (!isTRUE(resultado$aplica) || !isTRUE(resultado$desviacion)) return(NULL)
    .nuevo_hallazgo(
      resultado$columna, "desviacion_benford", "sospechoso",
      paste0(
        "En muchas magnitudes que crecen por acumulacion -importes, poblaciones, ",
        "areas- el primer digito no aparece con la misma frecuencia: el 1 encabeza ",
        "cerca del 30 % de los valores y el 9 menos del 5 %. En esta columna los ",
        "primeros digitos no siguen ese reparto. Es una senal descriptiva para ",
        "revisar, no evidencia de fraude ni de manipulacion: un tope ",
        "administrativo o un monto fijo la producen igual."
      ),
      paste0(
        resultado$metodo, ": X2=", sprintf("%.3f", resultado$estadistico),
        ", p=", format.pval(resultado$p_valor, digits = 4L),
        "; observado/esperado por digito: ",
        .texto_distribucion_benford(resultado$distribucion), "."
      ),
      paste0(
        "Revisar el proceso y explicaciones inocentes como topes administrativos, ",
        "redondeos, precios psicologicos o subsidios de monto fijo."
      ),
      resultado$n_positivos, NA_real_, "valor_positivo"
    )
  })
  nuevos <- nuevos[!vapply(nuevos, is.null, logical(1L))]

  list(
    hallazgos = nuevos,
    cobertura = cobertura,
    meta = list(
      metodo = "chi-cuadrado de Pearson; 8 grados de libertad",
      umbrales = umbrales,
      resultados = resultados
    )
  )
}
