.validar_entero_positivo <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop("`", nombre, "` debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(x)
}

.columnas_personales_rapidas <- function(datos, perfil = NULL) {
  if (!is.null(perfil)) return(.columnas_personales_protegidas(perfil))
  validadores <- .normalizar_validadores_personales(NULL)
  resultados <- lapply(seq_along(datos), function(i) {
    inferencia <- list(tipo = .tipo_declarado(datos[[i]]))
    .clasificar_dato_personal(
      datos[[i]], names(datos)[[i]], inferencia,
      validadores = validadores
    )
  })
  names(datos)[vapply(resultados, function(x) isTRUE(x$proteger), logical(1L))]
}

.frecuencias_columna <- function(x, max_valores, muestra, protegida) {
  if (is.matrix(x) || (is.list(x) && !is.factor(x))) {
    return(list(tabla = NULL, meta = c(
      analizados = 0, distintos = NA, mostrados = 0, truncado = FALSE,
      muestreado = FALSE, estado = "tipo_no_comparable"
    )))
  }
  muestreo <- .muestrear_vector(x, muestra)
  valores <- muestreo$valores[!is.na(muestreo$valores)]
  if (!length(valores)) {
    return(list(tabla = data.frame(
      valor = character(), frecuencia = integer(), proporcion = numeric(),
      stringsAsFactors = FALSE
    ), meta = c(
      analizados = 0, distintos = 0, mostrados = 0, truncado = FALSE,
      muestreado = muestreo$muestreado, estado = "sin_valores"
    )))
  }
  textos <- .valores_relacion(valores)
  textos <- textos[!is.na(textos)]
  if (!length(textos)) {
    return(list(tabla = data.frame(
      valor = character(), frecuencia = integer(), proporcion = numeric(),
      stringsAsFactors = FALSE
    ), meta = c(
      analizados = 0, distintos = 0, mostrados = 0, truncado = FALSE,
      muestreado = muestreo$muestreado, estado = "sin_valores_analizables"
    )))
  }
  unicos <- unique(textos)
  conteos <- tabulate(match(textos, unicos), nbins = length(unicos))
  orden <- order(-conteos, seq_along(conteos))
  seleccion <- utils::head(orden, max_valores)
  tabla <- data.frame(
    valor = if (protegida) rep("[valor protegido]", length(seleccion)) else {
      unicos[seleccion]
    },
    frecuencia = as.integer(conteos[seleccion]),
    proporcion = as.numeric(conteos[seleccion]) / length(textos),
    stringsAsFactors = FALSE
  )
  list(tabla = tabla, meta = c(
    analizados = length(textos), distintos = length(unicos),
    mostrados = length(seleccion), truncado = length(unicos) > length(seleccion),
    muestreado = muestreo$muestreado, estado = "calculada"
  ))
}

#' Distribuciones de valores y cuantiles por columna
#'
#' Resume frecuencias sin conservar una tabla completa de alta cardinalidad.
#' Cada columna se limita a `max_valores`; `alcance` declara cuántos valores se
#' analizaron, cuántos distintos se observaron y si hubo muestreo o truncamiento.
#' Los cuantiles se calculan sólo para números ordinarios finitos.
#'
#' Cuando una columna tiene evidencia suficiente para activar la protección de
#' datos personales, sus frecuencias y niveles se conservan pero el valor
#' concreto se reemplaza. Los cuantiles
#' mantienen sus filas y probabilidades, pero `valor` queda en `NA` y `estado`
#' informa `"valor_protegido"`: un cuantil, especialmente en tablas pequeñas,
#' puede coincidir exactamente con una observación. Esta protección es
#' independiente de la usada al construir el perfil.
#'
#' @param datos Tabla que se desea examinar.
#' @param perfil Perfil opcional de los mismos datos; evita repetir la
#'   clasificación de posibles datos personales.
#' @param max_valores Máximo de valores mostrados por columna.
#' @param probabilidades Probabilidades de los cuantiles, en `[0, 1]`.
#' @param muestra Máximo de filas por columna; `Inf` desactiva el muestreo.
#' @param proteger_datos_personales Si se ocultan valores de columnas cuya
#'   clasificación activa protección automática. Véase [perfilar()].
#'
#' @return Objeto `distribuciones_perfil`, una lista con data frames
#'   `frecuencias`, `cuantiles` y `alcance`. Todas las proporciones están en
#'   `[0, 1]`.
#' @export
#' @seealso [perfilar()], [analizar()], [clasificar_variables()]
#'
#' @examples
#' d <- data.frame(grupo = c("A", "A", "B"), valor = c(1, 2, 10))
#' distribucion_valores(d)
distribucion_valores <- function(datos, perfil = NULL, max_valores = 20L,
                                 probabilidades = c(0, 0.25, 0.5, 0.75, 1),
                                 muestra = 1e5,
                                 proteger_datos_personales = TRUE) {
  .validar_datos_tabla(datos)
  .validar_perfil_de(perfil, datos)
  max_valores <- .validar_entero_positivo(max_valores, "max_valores")
  limite <- .validar_muestra(muestra)
  if (!is.numeric(probabilidades) || !length(probabilidades) ||
      anyNA(probabilidades) || any(!is.finite(probabilidades)) ||
      any(probabilidades < 0 | probabilidades > 1)) {
    stop("`probabilidades` debe contener valores finitos en [0, 1].",
         call. = FALSE)
  }
  probabilidades <- unique(as.numeric(probabilidades))
  if (!is.logical(proteger_datos_personales) ||
      length(proteger_datos_personales) != 1L ||
      is.na(proteger_datos_personales)) {
    stop("`proteger_datos_personales` debe ser TRUE o FALSE.", call. = FALSE)
  }
  personales <- if (proteger_datos_personales) {
    .columnas_personales_rapidas(datos, perfil)
  } else character()
  frecuencias <- list()
  cuantiles <- list()
  alcance <- vector("list", ncol(datos))
  k <- 0L
  q <- 0L
  for (i in seq_along(datos)) {
    nombre <- names(datos)[[i]]
    resumen <- .frecuencias_columna(
      datos[[i]], max_valores, limite, nombre %in% personales
    )
    if (!is.null(resumen$tabla) && nrow(resumen$tabla)) {
      k <- k + 1L
      resumen$tabla$columna <- nombre
      resumen$tabla$rango <- seq_len(nrow(resumen$tabla))
      frecuencias[[k]] <- resumen$tabla[c(
        "columna", "rango", "valor", "frecuencia", "proporcion"
      )]
    }
    meta <- resumen$meta
    alcance[[i]] <- data.frame(
      columna = nombre, n_total = NROW(datos[[i]]),
      n_analizados = as.numeric(meta[["analizados"]]),
      n_distintos_muestra = as.numeric(meta[["distintos"]]),
      n_mostrados = as.numeric(meta[["mostrados"]]),
      muestreado = as.logical(meta[["muestreado"]]),
      truncado = as.logical(meta[["truncado"]]),
      protegida = nombre %in% personales,
      estado = as.character(meta[["estado"]]), stringsAsFactors = FALSE
    )
    x <- datos[[i]]
    if (is.numeric(x) && !is.matrix(x) &&
        !inherits(x, c("Date", "POSIXt", "integer64"))) {
      muestra_x <- .muestrear_vector(x, limite)
      finitos <- muestra_x$valores[is.finite(muestra_x$valores)]
      if (length(finitos)) {
        valores_q <- stats::quantile(
          finitos, probs = probabilidades, names = FALSE, type = 7
        )
        protegida <- nombre %in% personales
        q <- q + 1L
        cuantiles[[q]] <- data.frame(
          columna = nombre, probabilidad = probabilidades,
          valor = if (protegida) {
            rep(NA_real_, length(valores_q))
          } else as.numeric(valores_q),
          n_analizados = length(finitos), muestreado = muestra_x$muestreado,
          estado = if (protegida) "valor_protegido" else "calculado",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  vacia_f <- data.frame(
    columna = character(), rango = integer(), valor = character(),
    frecuencia = integer(), proporcion = numeric(), stringsAsFactors = FALSE
  )
  vacia_q <- data.frame(
    columna = character(), probabilidad = numeric(), valor = numeric(),
    n_analizados = integer(), muestreado = logical(), estado = character(),
    stringsAsFactors = FALSE
  )
  vacia_a <- data.frame(
    columna = character(), n_total = numeric(), n_analizados = numeric(),
    n_distintos_muestra = numeric(), n_mostrados = numeric(),
    muestreado = logical(), truncado = logical(), protegida = logical(),
    estado = character(), stringsAsFactors = FALSE
  )
  resultado <- list(
    frecuencias = if (length(frecuencias)) do.call(rbind, frecuencias) else vacia_f,
    cuantiles = if (length(cuantiles)) do.call(rbind, cuantiles) else vacia_q,
    alcance = if (length(alcance)) do.call(rbind, alcance) else vacia_a
  )
  rownames(resultado$frecuencias) <- rownames(resultado$cuantiles) <-
    rownames(resultado$alcance) <- NULL
  class(resultado) <- "distribuciones_perfil"
  resultado
}

.tipo_asociacion <- function(x, max_niveles) {
  if (inherits(x, c("Date", "POSIXt", "integer64")) || is.list(x) ||
      is.matrix(x)) return(NA_character_)
  if (is.character(x) || is.factor(x)) x <- .texto_analizable(x)$valores
  presentes <- x[!is.na(x)]
  distintos <- length(unique(presentes))
  if (length(presentes) < 2L || distintos < 2L) return(NA_character_)
  if (is.numeric(x)) return("numerica")
  if ((is.character(x) || is.factor(x) || is.logical(x)) &&
      distintos <= max_niveles && distintos / length(presentes) < 0.8) {
    return("categorica")
  }
  NA_character_
}

.cramer_v <- function(x, y) {
  tabla <- table(as.character(x), as.character(y), useNA = "no")
  n <- sum(tabla)
  if (n == 0L || min(dim(tabla)) < 2L) return(NA_real_)
  esperados <- outer(rowSums(tabla), colSums(tabla)) / n
  chi <- sum((tabla - esperados)^2 / esperados)
  sqrt(chi / (n * min(nrow(tabla) - 1L, ncol(tabla) - 1L)))
}

.eta2 <- function(categoria, numero) {
  media <- mean(numero)
  total <- sum((numero - media)^2)
  if (!is.finite(total) || total == 0) return(NA_real_)
  grupos <- split(numero, categoria, drop = TRUE)
  entre <- sum(vapply(grupos, function(x) length(x) * (mean(x) - media)^2,
                       numeric(1L)))
  entre / total
}

.es_dependencia_exacta <- function(a, b, dependencias) {
  if (is.null(dependencias) || !nrow(dependencias)) return(FALSE)
  any(dependencias$exacta & (
    (dependencias$determinante == a & dependencias$dependiente == b) |
      (dependencias$determinante == b & dependencias$dependiente == a)
  ))
}

#' Detectar asociaciones entre columnas
#'
#' Calcula Pearson —o Spearman, si se pide— entre numéricas, V de Cramér entre
#' categóricas y eta cuadrado entre una categórica y una numérica. Las medidas se
#' informan en `[0, 1]`: la correlación usa su valor absoluto. La tabla declara
#' el método, su supuesto, el soporte y el posible muestreo; no presenta
#' significancia estadística.
#'
#' `metodo_numerico = "spearman"` mide asociación **monótona** sobre los rangos
#' y no supone linealidad, así que reconoce una relación creciente aunque sea
#' curva. Pearson sigue siendo el valor por omisión porque es lo que la mayoría
#' espera de una correlación, y el método elegido viaja en la columna `metodo`
#' de la salida para que ninguna lectura dependa de recordar cuál se pidió.
#'
#' Se descartan constantes, fechas, listas, categóricas de cardinalidad alta y
#' columnas posteriores a `max_columnas` antes de construir pares. Las
#' dependencias funcionales exactas recibidas en `dependencias` no se repiten
#' como asociaciones.
#'
#' @param datos Tabla que se desea examinar.
#' @param dependencias Resultado opcional de [detectar_dependencias()].
#' @param umbral Valor mínimo en `[0, 1]` que se informa.
#' @param muestra Máximo común de filas; `Inf` desactiva el muestreo.
#' @param max_columnas Máximo de columnas analizables.
#' @param max_niveles Máximo de niveles para tratar una columna como categórica.
#' @param max_pares Máximo de asociaciones devueltas después de ordenar.
#' @param metodo_numerico Medida entre columnas numéricas: `"pearson"` por
#'   omisión, o `"spearman"` para asociación monótona sobre los rangos.
#'
#' @return Data frame S3 `asociaciones_columnas`. Sus atributos declaran filas,
#'   columnas y pares examinados, omisiones por dependencia y truncamiento.
#' @export
#' @seealso [detectar_dependencias()], [analizar()]
#'
#' @examples
#' d <- data.frame(x = 1:20, y = 2 * (1:20), grupo = rep(c("A", "B"), 10))
#' detectar_asociaciones(d, umbral = 0)
detectar_asociaciones <- function(datos, dependencias = NULL, umbral = 0.3,
                                  muestra = 1e4, max_columnas = 50L,
                                  max_niveles = 50L, max_pares = 500L,
                                  metodo_numerico = c("pearson", "spearman")) {
  metodo_numerico <- match.arg(metodo_numerico)
  .validar_datos_tabla(datos)
  if (!is.null(dependencias) && !inherits(dependencias, "data.frame")) {
    stop("`dependencias` debe ser NULL o un data frame.", call. = FALSE)
  }
  if (!is.numeric(umbral) || length(umbral) != 1L || is.na(umbral) ||
      !is.finite(umbral) || umbral < 0 || umbral > 1) {
    stop("`umbral` debe ser una proporcion finita en [0, 1].", call. = FALSE)
  }
  limite <- .validar_muestra(muestra)
  max_columnas <- .validar_entero_positivo(max_columnas, "max_columnas")
  max_niveles <- .validar_entero_positivo(max_niveles, "max_niveles")
  max_pares <- .validar_entero_positivo(max_pares, "max_pares")
  muestreo <- .muestrear_vector(seq_len(nrow(datos)), limite)
  muestra_datos <- datos[muestreo$valores, , drop = FALSE]
  tipos <- vapply(muestra_datos, .tipo_asociacion, character(1L),
                  max_niveles = max_niveles)
  analizables <- which(!is.na(tipos))
  seleccion <- utils::head(analizables, max_columnas)
  pares_posibles <- if (length(seleccion) >= 2L) choose(length(seleccion), 2L) else 0
  filas <- list()
  k <- 0L
  omitidos_dependencia <- 0L
  if (length(seleccion) >= 2L) {
    combinaciones <- utils::combn(seleccion, 2L)
    for (p in seq_len(ncol(combinaciones))) {
      i <- combinaciones[1L, p]
      j <- combinaciones[2L, p]
      a <- names(datos)[[i]]
      b <- names(datos)[[j]]
      if (.es_dependencia_exacta(a, b, dependencias)) {
        omitidos_dependencia <- omitidos_dependencia + 1L
        next
      }
      x <- muestra_datos[[i]]
      y <- muestra_datos[[j]]
      if (is.character(x) || is.factor(x)) x <- .texto_analizable(x)$valores
      if (is.character(y) || is.factor(y)) y <- .texto_analizable(y)$valores
      completos <- !is.na(x) & !is.na(y)
      if (sum(completos) < 3L) next
      x <- x[completos]
      y <- y[completos]
      metodo <- if (tipos[[i]] == "numerica" && tipos[[j]] == "numerica") {
        if (identical(metodo_numerico, "spearman")) {
          "spearman_absoluto"
        } else {
          "pearson_absoluto"
        }
      } else if (tipos[[i]] == "categorica" && tipos[[j]] == "categorica") {
        "cramer_v"
      } else {
        "eta2"
      }
      valor <- switch(
        metodo,
        pearson_absoluto = abs(stats::cor(x, y)),
        spearman_absoluto = abs(stats::cor(x, y, method = "spearman")),
        cramer_v = .cramer_v(x, y),
        eta2 = if (tipos[[i]] == "categorica") .eta2(x, y) else .eta2(y, x)
      )
      if (!is.finite(valor) || valor < umbral) next
      k <- k + 1L
      filas[[k]] <- data.frame(
        columna_1 = a, columna_2 = b, tipo_1 = tipos[[i]], tipo_2 = tipos[[j]],
        metodo = metodo,
        supuesto = switch(
          metodo,
          pearson_absoluto =
            "Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.",
          spearman_absoluto = paste(
            "Se mide asociacion monotona sobre los rangos: no supone",
            "linealidad ni que la escala sea de intervalo."
          ),
          cramer_v = "Las columnas se tratan como categorias sin orden.",
          "La columna numerica se trata como cuantitativa y la otra como categoria."
        ),
        asociacion = as.numeric(valor), n_pares = length(x),
        stringsAsFactors = FALSE
      )
    }
  }
  vacia <- data.frame(
    columna_1 = character(), columna_2 = character(), tipo_1 = character(),
    tipo_2 = character(), metodo = character(), supuesto = character(),
    asociacion = numeric(), n_pares = integer(), stringsAsFactors = FALSE
  )
  resultado <- if (length(filas)) do.call(rbind, filas) else vacia
  if (nrow(resultado)) {
    resultado <- resultado[order(-resultado$asociacion, -resultado$n_pares,
                                 resultado$columna_1, resultado$columna_2), , drop = FALSE]
  }
  total_informadas <- nrow(resultado)
  resultado <- utils::head(resultado, max_pares)
  rownames(resultado) <- NULL
  class(resultado) <- c("asociaciones_columnas", "data.frame")
  attr(resultado, "filas_analizadas") <- length(muestreo$valores)
  attr(resultado, "muestreado") <- muestreo$muestreado
  attr(resultado, "columnas_analizadas") <- names(datos)[seleccion]
  attr(resultado, "columnas_no_analizables") <- names(datos)[is.na(tipos)]
  attr(resultado, "columnas_omitidas_limite") <- names(datos)[
    setdiff(analizables, seleccion)
  ]
  attr(resultado, "columnas_omitidas") <- names(datos)[
    setdiff(seq_along(datos), seleccion)
  ]
  attr(resultado, "columnas_candidatas_total") <- length(analizables)
  attr(resultado, "pares_candidatos_total") <- if (length(analizables) >= 2L) {
    choose(length(analizables), 2L)
  } else 0
  attr(resultado, "pares_posibles") <- pares_posibles
  attr(resultado, "pares_omitidos_dependencia") <- omitidos_dependencia
  attr(resultado, "total_informadas") <- total_informadas
  attr(resultado, "truncado_columnas") <- length(analizables) > length(seleccion)
  attr(resultado, "truncado") <- total_informadas > nrow(resultado)
  attr(resultado, "umbral") <- umbral
  resultado
}

.fecha_columna_avanzada <- function(x, formatos = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (!is.character(x) && !is.factor(x)) return(NULL)
  if (is.null(formatos)) formatos <- detectar_formatos_fecha(x)
  if (!nrow(formatos) || !any(formatos$estado == "confirmado")) return(NULL)
  if ("granularidad" %in% names(formatos) &&
      any(formatos$granularidad[formatos$estado == "confirmado"] == "mes")) {
    return(NULL)
  }
  as.Date(.parsear_fechas(x, formatos), tz = "UTC")
}

.dia_semana_iso <- function(x) as.integer(format(x, "%u"))

.grupos_huecos <- function(faltantes) {
  if (!length(faltantes)) return(list())
  cortes <- cumsum(c(TRUE, diff(faltantes) > 1L))
  split(faltantes, cortes)
}

#' Examinar regularidad y cobertura temporal
#'
#' Para cada columna temporal propone una frecuencia en días. La confianza es
#' el mínimo entre la contigüidad de las fechas sobre la grilla propuesta y la
#' cobertura del período: una coincidencia breve dentro de una serie muy
#' dispersa no puede producir confianza alta. Ambas componentes se devuelven
#' para que la propuesta sea auditable. La propuesta nunca queda confirmada
#' automáticamente. `calendario` usa días
#' ISO: 1 es lunes y 7 domingo; así una oficina puede declarar `1:5` sin que la
#' ausencia de fines de semana se interprete como hueco. Las fechas-hora se
#' llevan a fecha civil en UTC para que el resultado no dependa de la zona del
#' equipo que ejecuta el análisis.
#'
#' @param datos Tabla que se desea examinar.
#' @param perfil Perfil opcional de los mismos datos.
#' @param columnas Columnas temporales; `NULL` usa clases e inferencia.
#' @param calendario Días de semana esperados, enteros entre 1 y 7.
#' @param frecuencia_dias Frecuencia entera conocida en días. Si es `NULL`, se
#'   propone la moda de los intervalos positivos.
#' @param max_huecos Máximo de grupos de huecos devueltos por columna.
#' @param max_columnas Máximo de columnas temporales analizadas.
#'
#' @return Objeto `analisis_temporal` con `resumen`, `dias_semana`, `huecos` y
#'   `propuestas`. El recorte de huecos queda en `resumen`; el de columnas, en
#'   atributos del objeto.
#' @export
#' @seealso [detectar_formatos_fecha()], [analizar()]
#'
#' @examples
#' fechas <- as.Date("2026-01-01") + c(0:4, 20:24)
#' analizar_tiempo(data.frame(fecha = fechas))
analizar_tiempo <- function(datos, perfil = NULL, columnas = NULL,
                            calendario = 1:7, frecuencia_dias = NULL,
                            max_huecos = 20L, max_columnas = 50L) {
  .validar_datos_tabla(datos)
  .validar_perfil_de(perfil, datos)
  if (!is.numeric(calendario) || !length(calendario) || anyNA(calendario) ||
      any(calendario < 1 | calendario > 7) || any(calendario != floor(calendario))) {
    stop("`calendario` debe contener dias ISO entre 1 y 7.", call. = FALSE)
  }
  calendario <- sort(unique(as.integer(calendario)))
  if (!is.null(frecuencia_dias) && (!is.numeric(frecuencia_dias) ||
      length(frecuencia_dias) != 1L || is.na(frecuencia_dias) ||
      !is.finite(frecuencia_dias) || frecuencia_dias <= 0 ||
      frecuencia_dias != floor(frecuencia_dias))) {
    stop("`frecuencia_dias` debe ser NULL o un entero positivo.", call. = FALSE)
  }
  max_huecos <- .validar_entero_positivo(max_huecos, "max_huecos")
  max_columnas <- .validar_entero_positivo(max_columnas, "max_columnas")
  if (is.null(columnas)) {
    columnas <- names(datos)[vapply(seq_along(datos), function(i) {
      formatos <- if (!is.null(perfil)) perfil$formatos_fecha[[i]] else NULL
      !is.null(.fecha_columna_avanzada(datos[[i]], formatos))
    }, logical(1L))]
  }
  if (!is.character(columnas) || anyNA(columnas) ||
      any(!columnas %in% names(datos))) {
    stop("`columnas` contiene nombres inexistentes.", call. = FALSE)
  }
  columnas_totales <- unique(columnas)
  columnas <- utils::head(columnas_totales, max_columnas)
  resumen <- list()
  dias <- list()
  huecos <- list()
  propuestas <- list()
  h <- 0L
  for (i in seq_along(columnas)) {
    nombre <- columnas[[i]]
    indice <- match(nombre, names(datos))
    formatos <- if (!is.null(perfil)) perfil$formatos_fecha[[indice]] else NULL
    fechas <- .fecha_columna_avanzada(datos[[indice]], formatos)
    if (is.null(fechas)) next
    presentes <- fechas[!is.na(fechas)]
    unicas <- sort(unique(presentes))
    duplicados <- length(presentes) - length(unicas)
    diferencias <- as.numeric(diff(unicas))
    positivas <- diferencias[diferencias > 0]
    frecuencia <- if (!is.null(frecuencia_dias)) frecuencia_dias else if (
      length(positivas)) {
      valores <- unique(positivas)
      valores[[which.max(tabulate(match(positivas, valores)))]]
    } else NA_real_
    monotona <- if (length(presentes) > 1L) {
      mean(diff(as.numeric(presentes)) >= 0)
    } else NA_real_
    esperadas <- if (length(unicas) && is.finite(frecuencia)) {
      secuencia <- seq.Date(min(unicas), max(unicas), by = frecuencia)
      secuencia[.dia_semana_iso(secuencia) %in% calendario]
    } else as.Date(character())
    faltantes <- setdiff(esperadas, unicas)
    fuera_calendario <- unicas[!.dia_semana_iso(unicas) %in% calendario]
    indices_observados <- match(unicas[unicas %in% esperadas], esperadas)
    contiguidad <- if (length(indices_observados) > 1L) {
      mean(diff(indices_observados) == 1L)
    } else NA_real_
    cobertura <- if (length(esperadas)) {
      sum(esperadas %in% unicas) / length(esperadas)
    } else NA_real_
    confianza <- if (is.finite(contiguidad) && is.finite(cobertura)) {
      min(contiguidad, cobertura)
    } else NA_real_
    truncado <- FALSE
    grupos <- list()
    if (length(faltantes)) {
      posiciones <- match(faltantes, esperadas)
      grupos <- .grupos_huecos(posiciones)
      truncado <- length(grupos) > max_huecos
      for (grupo in utils::head(grupos, max_huecos)) {
        h <- h + 1L
        fechas_grupo <- esperadas[grupo]
        huecos[[h]] <- data.frame(
          columna = nombre, desde = min(fechas_grupo), hasta = max(fechas_grupo),
          n_esperados_ausentes = length(fechas_grupo),
          duracion_dias = as.numeric(max(fechas_grupo) - min(fechas_grupo)) + 1,
          frecuencia_dias = frecuencia, calendario = paste(calendario, collapse = ","),
          stringsAsFactors = FALSE
        )
      }
    }
    conteos_dia <- tabulate(.dia_semana_iso(presentes), nbins = 7L)
    dias[[i]] <- data.frame(
      columna = nombre, dia_iso = 1:7,
      dia = c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado", "domingo"),
      frecuencia = conteos_dia,
      proporcion = if (length(presentes)) conteos_dia / length(presentes) else 0,
      esperado = 1:7 %in% calendario, stringsAsFactors = FALSE
    )
    resumen[[i]] <- data.frame(
      columna = nombre, n_presentes = length(presentes),
      n_fechas_distintas = length(unicas), n_duplicados_temporales = duplicados,
      fecha_minima = if (length(unicas)) min(unicas) else as.Date(NA),
      fecha_maxima = if (length(unicas)) max(unicas) else as.Date(NA),
      monotonicidad = monotona, cobertura_periodo = cobertura,
      n_fechas_esperadas_ausentes = length(faltantes),
      n_fechas_fuera_calendario = length(fuera_calendario),
      n_grupos_huecos = length(grupos), huecos_truncados = truncado,
      stringsAsFactors = FALSE
    )
    propuestas[[i]] <- data.frame(
      columna = nombre, frecuencia_dias = frecuencia, confianza = confianza,
      contiguidad = contiguidad, cobertura_periodo = cobertura,
      calendario = paste(calendario, collapse = ","), confirmada = FALSE,
      evidencia = if (is.finite(frecuencia)) paste0(
        "Moda de intervalos: ", frecuencia, " dias; ", length(positivas),
        " intervalos observados."
      ) else "No hay intervalos suficientes para proponer frecuencia.",
      stringsAsFactors = FALSE
    )
  }
  vacio_resumen <- data.frame(
    columna = character(), n_presentes = integer(), n_fechas_distintas = integer(),
    n_duplicados_temporales = integer(), fecha_minima = as.Date(character()),
    fecha_maxima = as.Date(character()), monotonicidad = numeric(),
    cobertura_periodo = numeric(), n_fechas_esperadas_ausentes = integer(),
    n_fechas_fuera_calendario = integer(),
    n_grupos_huecos = integer(), huecos_truncados = logical(),
    stringsAsFactors = FALSE
  )
  vacio_dias <- data.frame(
    columna = character(), dia_iso = integer(), dia = character(),
    frecuencia = integer(), proporcion = numeric(), esperado = logical(),
    stringsAsFactors = FALSE
  )
  vacio_huecos <- data.frame(
    columna = character(), desde = as.Date(character()), hasta = as.Date(character()),
    n_esperados_ausentes = integer(), duracion_dias = numeric(),
    frecuencia_dias = numeric(), calendario = character(), stringsAsFactors = FALSE
  )
  vacio_prop <- data.frame(
    columna = character(), frecuencia_dias = numeric(), confianza = numeric(),
    contiguidad = numeric(), cobertura_periodo = numeric(),
    calendario = character(), confirmada = logical(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  resultado <- list(
    resumen = if (length(resumen)) do.call(rbind, resumen) else vacio_resumen,
    dias_semana = if (length(dias)) do.call(rbind, dias) else vacio_dias,
    huecos = if (length(huecos)) do.call(rbind, huecos) else vacio_huecos,
    propuestas = if (length(propuestas)) do.call(rbind, propuestas) else vacio_prop
  )
  resultado <- lapply(resultado, function(x) { rownames(x) <- NULL; x })
  class(resultado) <- "analisis_temporal"
  attr(resultado, "columnas_analizadas") <- columnas
  attr(resultado, "columnas_omitidas") <- setdiff(columnas_totales, columnas)
  attr(resultado, "truncado") <- length(columnas_totales) > length(columnas)
  resultado
}
