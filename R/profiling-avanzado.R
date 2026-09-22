.validar_entero_positivo <- function(x, nombre) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x)) {
    stop("`", nombre, "` debe ser un entero positivo.", call. = FALSE)
  }
  as.integer(x)
}

# `detalle = TRUE` devuelve la clasificacion completa de las columnas protegidas
# y no solo sus nombres. Existe porque quien necesita decir POR QUE una columna
# se protege -el tipo clasificado- si no tendria que volver a clasificarla, y dos
# clasificaciones de la misma columna son dos reglas que se pueden separar.
# `declaradas` y `validadores` existen porque el lexico por omision no puede
# conocer lo que solo el usuario sabe: un nombre propio de la organizacion, un
# identificador interno. La regla del paquete es excluir lo que el usuario
# declara, y este camino -el de medir()- no tenia por donde recibirlo.
.columnas_personales_rapidas <- function(datos, perfil = NULL, detalle = FALSE,
                                         declaradas = character(),
                                         validadores = NULL) {
  if (!is.null(perfil)) {
    protegidas <- .columnas_personales_protegidas(perfil)
    if (!detalle) return(protegidas)
    return(perfil$datos_personales[
      .nombres_para_operar(perfil$datos_personales$columna) %in%
        .nombres_para_operar(protegidas), , drop = FALSE
    ])
  }
  validadores <- .normalizar_validadores_personales(validadores)
  # La misma forma declarativa que perfilar(): nombres de columna sueltos, o
  # `columna = "tipo"`. Que las dos puertas acepten lo mismo es lo que permite
  # que describan la columna igual.
  #
  # Sin verificar existencia -de ahi el NULL-, porque `datos` aca es una tabla
  # ya recortada a las columnas que la metrica usa: exigir que la declaracion
  # nombre una de ellas rechazaria una declaracion legitima sobre una columna
  # que esta metrica no mira. Quien verifica es medir(), contra la entrada
  # entera.
  declaradas <- .normalizar_columnas_personales(declaradas, NULL)
  resultados <- lapply(seq_along(datos), function(i) {
    inferencia <- list(tipo = .tipo_declarado(datos[[i]]))
    .clasificar_dato_personal(
      datos[[i]], names(datos)[[i]], inferencia,
      validadores = validadores
    )
  })
  protege <- vapply(resultados, function(x) isTRUE(x$proteger), logical(1L))
  # Lo declarado se protege aunque el lexico no lo reconozca.
  protege <- protege | .nombres_presentes(names(datos), names(declaradas))
  if (!detalle) return(names(datos)[protege])
  if (!any(protege)) {
    return(data.frame(columna = character(), tipo = character(),
                      stringsAsFactors = FALSE))
  }
  tipos <- vapply(resultados[protege], function(x) as.character(x$tipo)[1L],
                  character(1L))
  # Una columna que solo es personal porque el usuario lo dijo no tiene tipo
  # inferido. Se publica el tipo que el usuario declaro -o "declarado" cuando no
  # declaro ninguno-, que es exactamente lo que hace perfilar().
  sin_tipo <- is.na(tipos)
  tipos[sin_tipo] <- unname(
    declaradas[.indice_nombre(
      names(datos)[protege][sin_tipo], names(declaradas)
    )]
  )
  data.frame(
    columna = names(datos)[protege], tipo = tipos, stringsAsFactors = FALSE
  )
}

.frecuencias_columna <- function(x, max_valores, muestra, protegida) {
  if (.es_columna_compuesta(x) || (is.list(x) && !is.factor(x))) {
    return(list(tabla = NULL, meta = c(
      analizados = 0, distintos = NA, mostrados = 0, truncado = FALSE,
      muestreado = FALSE, estado = "tipo_no_comparable"
    )))
  }
  muestreo <- .muestrear_vector(x, muestra)
  # Lo declarado `bytes` se rinde a la forma que muestra la consola ANTES de
  # contar. La tabla de frecuencias se PUBLICA -y de ahi pasa al HTML de
  # `reportar()`, donde los bytes crudos dentro de un documento UTF-8 se
  # renderizan como el caracter-, asi que interpretarlo aca rompia la
  # declaracion en dos salidas a la vez. Medido: la tabla publicaba el
  # caracter mientras `print()` del mismo dato mostraba la forma escapada.
  valores <- .texto_publicable(muestreo$valores)
  valores <- valores[!is.na(valores)]
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
  # Cuantos se perdieron aca, ANTES de descartarlos. `.valores_relacion()`
  # devuelve `NA` para lo que no puede comparar -bytes invalidos, sobre todo- y
  # la linea siguiente los saca. La proporcion se calcula entonces sobre los
  # que sobrevivieron, no sobre la columna.
  #
  # Sin esta cuenta el estado decia `calculada` igual, y el resultado se leia
  # como completo: medido sobre una columna de cuatro filas con tres bytes
  # invalidos, la tabla publicaba `z` con proporcion 1.00 cuando `z` es el 25%
  # de la columna. Informar como completo lo que es parcial es exactamente lo
  # que este paquete promete no hacer.
  #
  # Los numeros ya viajaban en `alcance` -`n_total` y `n_analizados`-; lo que
  # faltaba era que el ESTADO los nombrara, como ya hace la cobertura del
  # analisis con su tercer estado parcial.
  descartados_no_comparables <- sum(is.na(textos)) - sum(is.na(valores))
  if (descartados_no_comparables < 0L) descartados_no_comparables <- 0L
  # Se conserva el valor TAL COMO SE VA A PUBLICAR, apareado con su clave.
  # `.valores_relacion()` termina en `.nombres_para_operar()`, que es la clave
  # de identidad: sirve para AGRUPAR, no para mostrar. Publicando la clave, la
  # tabla sacaba `a\\xc3\\xb1o` -con la barra duplicada por el escape de
  # inyectividad de la clave- en vez de `a\xc3\xb1o`, y peor: la salida
  # dependia de que mas hubiera en la columna, porque el escape solo se nota
  # cuando el valor ya trae barras.
  #
  # Se cuenta por clave y se publica el valor. Son dos preguntas distintas.
  # `as.character()` porque la columna publicada es de texto siempre: sobre una
  # columna `raw`, `valores` es `raw` y `rbind()` de las tablas por columna
  # aborta con "incompatible types (from integer to raw)". Lo atrapo la suite.
  publicables <- as.character(valores)[!is.na(textos)]
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
  indices <- match(textos, unicos)
  conteos <- tabulate(indices, nbins = length(unicos))
  # El primer valor publicable de cada grupo, que es el que representa al grupo.
  representantes <- publicables[match(seq_along(unicos), indices)]
  orden <- order(-conteos, seq_along(conteos))
  seleccion <- utils::head(orden, max_valores)
  tabla <- data.frame(
    valor = if (protegida) rep("[valor protegido]", length(seleccion)) else {
      representantes[seleccion]
    },
    frecuencia = as.integer(conteos[seleccion]),
    proporcion = as.numeric(conteos[seleccion]) / length(textos),
    stringsAsFactors = FALSE
  )
  list(tabla = tabla, meta = c(
    analizados = length(textos), distintos = length(unicos),
    mostrados = length(seleccion), truncado = length(unicos) > length(seleccion),
    muestreado = muestreo$muestreado,
    estado = if (descartados_no_comparables > 0L) {
      "calculada_parcial"
    } else {
      "calculada"
    }
  ))
}

#' Distribuciones de valores y cuantiles por columna
#'
#' Resume frecuencias sin conservar una tabla completa de alta cardinalidad.
#' Cada columna se limita a `max_valores`; `alcance` declara cuántos valores se
#' analizaron, cuántos distintos se observaron y si hubo muestreo o truncamiento.
#' Los cuantiles se calculan sólo para números ordinarios finitos.
#'
#' **Las proporciones de `frecuencias` se calculan sobre los valores que se
#' pudieron analizar, no sobre la columna entera**, y `alcance` dice cuántos
#' fueron: `n_total` contra `n_analizados`. Cuando alguno no se pudo comparar
#' —bytes inválidos, sobre todo— el `estado` de esa columna es
#' `calculada_parcial` en vez de `calculada`, para que un resultado parcial no
#' se lea como completo. Un `NA` no cuenta como descarte: es una ausencia
#' declarada, no un valor que no se pudo analizar. Los otros estados posibles
#' son `sin_valores`, `sin_valores_analizables` y `tipo_no_comparable`.
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
  datos <- .tabla_base(datos)
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
      datos[[i]], max_valores, limite,
      .nombres_para_operar(nombre) %in% .nombres_para_operar(personales)
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
      protegida = .nombres_para_operar(nombre) %in%
        .nombres_para_operar(personales),
      estado = as.character(meta[["estado"]]), stringsAsFactors = FALSE
    )
    x <- datos[[i]]
    if (is.numeric(x) && !.es_columna_compuesta(x) &&
        !inherits(x, c("Date", "POSIXt", "integer64"))) {
      muestra_x <- .muestrear_vector(x, limite)
      finitos <- muestra_x$valores[is.finite(muestra_x$valores)]
      if (length(finitos)) {
        valores_q <- stats::quantile(
          finitos, probs = probabilidades, names = FALSE, type = 7
        )
        protegida <- .nombres_para_operar(nombre) %in%
          .nombres_para_operar(personales)
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
  # El mismo piso que aplica `perfilar()`. Sin esto las dos puertas discrepaban
  # sobre el mismo dato: esta funcion protege POR COLUMNA -si la columna es
  # personal, sus cuantiles van NA- pero una copia que el clasificador no marca
  # publicaba los valores de la protegida. Medido, `cuantiles$valor` traia los
  # cinco cuantiles con el documento entero mientras la columna protegida salia
  # `[valor protegido]` en el mismo objeto.
  identificantes <- .valores_identificantes(
    .valores_publicables_protegidos(datos, personales)
  )
  if (length(identificantes)) {
    resultado <- .proteger_textos_salida(resultado, identificantes)
    resultado <- .proteger_numeros_parametros(resultado, identificantes)
  }
  class(resultado) <- "distribuciones_perfil"
  resultado
}

.tipo_asociacion <- function(x, max_niveles) {
  if (inherits(x, c("Date", "POSIXt", "integer64")) || is.list(x) ||
      .es_columna_compuesta(x)) return(NA_character_)
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
  grupos <- split(numero, .nombres_para_operar(categoria), drop = TRUE)
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
#' Se descartan constantes, fechas, listas, columnas compuestas —matrices o
#' arreglos de más de una dimensión—, categóricas de cardinalidad alta y
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
#'   `pares_omitidos_medicion` conserva los pares que no se pudieron medir,
#'   junto con el motivo y la cantidad de filas completas disponible. Su
#'   atributo `cobertura_diagnosticos` tiene una fila por par omitido. Cuando
#'   [analizar()] integra el resultado, esas filas se agregan a la cobertura
#'   del perfil.
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
  datos <- .tabla_base(datos)
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
  columnas_compuestas <- vapply(datos, .es_columna_compuesta, logical(1L))
  muestreo <- .muestrear_vector(seq_len(nrow(datos)), limite)
  muestra_datos <- datos[muestreo$valores, , drop = FALSE]
  tipos <- vapply(seq_along(muestra_datos), function(i) {
    if (columnas_compuestas[[i]]) return(NA_character_)
    .tipo_asociacion(muestra_datos[[i]], max_niveles = max_niveles)
  }, character(1L))
  analizables <- which(!is.na(tipos))
  seleccion <- utils::head(analizables, max_columnas)
  pares_posibles <- if (length(seleccion) >= 2L) choose(length(seleccion), 2L) else 0
  filas <- list()
  k <- 0L
  omitidos_dependencia <- 0L
  omitidos_medicion <- list()
  k_omitidos_medicion <- 0L
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
      n_completos <- sum(completos)
      if (n_completos < 3L) {
        k_omitidos_medicion <- k_omitidos_medicion + 1L
        omitidos_medicion[[k_omitidos_medicion]] <- data.frame(
          columna_1 = a, columna_2 = b, tipo_1 = tipos[[i]],
          tipo_2 = tipos[[j]], metodo = metodo,
          n_pares_completos = as.integer(n_completos),
          motivo = paste0(
            "Pocas filas completas para medir el par: hay ", n_completos,
            " y se requieren al menos 3."
          ), stringsAsFactors = FALSE
        )
        next
      }
      x <- x[completos]
      y <- y[completos]
      valor <- tryCatch(switch(
          metodo,
          pearson_absoluto = abs(stats::cor(x, y)),
          spearman_absoluto = abs(stats::cor(x, y, method = "spearman")),
          cramer_v = .cramer_v(x, y),
          eta2 = if (tipos[[i]] == "categorica") .eta2(x, y) else .eta2(y, x)
        ), error = function(e) NA_real_
      )
      if (!is.finite(valor)) {
        k_omitidos_medicion <- k_omitidos_medicion + 1L
        omitidos_medicion[[k_omitidos_medicion]] <- data.frame(
          columna_1 = a, columna_2 = b, tipo_1 = tipos[[i]],
          tipo_2 = tipos[[j]], metodo = metodo,
          n_pares_completos = as.integer(n_completos),
          motivo = "La medida de asociacion no fue finita.",
          stringsAsFactors = FALSE
        )
        next
      }
      if (valor < umbral) next
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
  attr(resultado, "pares_omitidos_medicion") <- if (
    length(omitidos_medicion)
  ) {
    do.call(rbind, omitidos_medicion)
  } else {
    data.frame(
      columna_1 = character(), columna_2 = character(), tipo_1 = character(),
      tipo_2 = character(), metodo = character(), n_pares_completos = integer(),
      motivo = character(), stringsAsFactors = FALSE
    )
  }
  omitidos <- attr(resultado, "pares_omitidos_medicion", exact = TRUE)
  attr(resultado, "cobertura_diagnosticos") <- if (nrow(omitidos)) {
    data.frame(
      diagnostico = "asociaciones",
      columna = paste(omitidos$columna_1, omitidos$columna_2, sep = " / "),
      motivo = omitidos$motivo,
      como_resolverlo = paste(
        "Completar al menos 3 filas para el par y revisar que la medida sea",
        "finita antes de interpretar que no hay asociacion."
      ),
      dependencia = NA_character_, stringsAsFactors = FALSE
    )
  } else {
    .cobertura_diagnosticos_vacia()
  }
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
  # Bastaba UN formato de mes confirmado para abandonar la columna entera, y la
  # columna desaparecia del resumen sin dejar rastro: ni fila, ni
  # `columnas_omitidas`, mientras `columnas_analizadas` seguia nombrandola.
  # `perfilar()` sobre esa misma columna la resume bien -700 dias, 100 meses
  # excluidos, `calculados_sobre_dias`- y `man/perfilar.Rd` declara justamente
  # ese trato para las columnas mixtas. Las dos salidas se contradecian.
  #
  # `.parsear_fechas()` ya deja los periodos de mes en `NA`, asi que el caso
  # mixto sale solo: lo que hacia falta era no abortar antes de parsearlo. Se
  # abandona unicamente cuando NO queda ninguna granularidad de dia, que ahi si
  # no hay serie diaria que construir -y el llamador lo declara-.
  granularidades <- if ("granularidad" %in% names(formatos)) {
    formatos$granularidad[formatos$estado == "confirmado"]
  } else rep("dia", sum(formatos$estado == "confirmado"))
  # `NULL` a secas: R no admite atributos sobre `NULL`, y no hacen falta -el
  # llamador ya anota la columna entre las omitidas-.
  if (!any(granularidades == "dia")) return(NULL)
  parseadas <- .parsear_fechas(x, formatos)
  resultado <- as.Date(parseadas, tz = "UTC")
  # El descarte de parseo NO depende del muestreo, y esto estaba escrito como si
  # dependiera: la rama tenia un `else 0L` literal y sólo contaba cuando
  # `muestreado` era TRUE. Es el mismo defecto que `R/columnas.R:92` describe
  # para la conversion de texto a numero, y sobrevivio a su arreglo porque el
  # commit que corrigio la regla llego a la salida de `perfilar()` y no a esta.
  #
  # Medido sobre mil valores presentes -950 que dicen "2024-01-01" y 50
  # "2022-13-99", que ningun formato del catalogo puede validar-, la MISMA
  # columna informaba `n_fechas_excluidas_parseo = 0` con estado `calculados`
  # sin muestrear, y 50 con `calculados_sobre_fechas_parseadas` con
  # `muestra = 50`. Los cincuenta quedaban afuera del resumen en los dos casos;
  # lo unico que cambiaba era si se decia. Y `perfilar()` sobre esa misma tabla
  # ya declaraba los 50, asi que las dos salidas del paquete se contradecian.
  valores <- trimws(as.character(x))
  # Un blanco es un valor PRESENTE que no llego a fecha, igual que un texto
  # ilegible: `trimws()` lo deja en `""` y el `nzchar()` lo descartaba de la
  # cuenta. No es una ausencia declarada -eso es `NA`, y se informa en
  # `n_faltantes`-. Mismo arreglo que en la conversion a numero.
  presentes <- !is.na(x)
  attr(resultado, "n_fechas_excluidas_parseo") <- as.integer(
    sum(presentes & is.na(parseadas))
  )
  # Y aparte, cuantos de esos quedaron afuera por ser periodos de mes y no por
  # ser ilegibles. `perfilar()` distingue las dos cosas -`n_valores_excluidos_
  # resumen` contra `n_fechas_excluidas_granularidad`- y esta salida tiene que
  # decir lo mismo sobre la misma columna, o vuelven a contradecirse.
  attr(resultado, "n_fechas_excluidas_granularidad") <- if (
    any(granularidades == "mes")) {
    as.integer(sum(formatos$n[formatos$estado == "confirmado" &
                                formatos$granularidad == "mes"], na.rm = TRUE))
  } else 0L
  resultado
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
#'   atributos del objeto. `resumen` agrega tres campos sobre el alcance del
#'   resumen, con el mismo vocabulario que usa [perfilar()] sobre la misma
#'   columna: `n_fechas_excluidas_parseo` cuenta los valores **presentes** que
#'   ningún formato confirmado pudo convertir y que por eso quedaron fuera;
#'   `n_fechas_excluidas_granularidad`, cuántos de ésos son períodos de mes —
#'   `2024-02` y sus formas—, que no se convierten a propósito porque no
#'   nombran un día; y `estado_resumen` vale `"calculados_sobre_dias"` cuando
#'   hubo períodos de mes, `"calculados_sobre_fechas_parseadas"` cuando sólo
#'   hubo valores ilegibles, y `"calculados"` cuando no quedó nada afuera. Los
#'   tres se informan siempre, haya muestreo o no, porque el descarte tampoco
#'   depende del muestreo. Un `NA` no entra en esta cuenta: es una ausencia
#'   declarada y se informa como faltante, no como valor que no se pudo leer.
#'
#'   Una columna mixta —días y meses— **sí** se resume, sobre sus fechas
#'   completas. Sólo cuando ningún formato confirmado nombra un día no hay serie
#'   diaria que construir; entonces la columna no aparece en `resumen`, y se
#'   declara en los atributos `columnas_omitidas` y `columnas_sin_serie_diaria`
#'   en vez de desaparecer.
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
  datos <- .tabla_base(datos)
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
  indices_columnas <- if (is.character(columnas) && !anyNA(columnas)) {
    .indice_nombre(columnas, names(datos))
  } else integer()
  if (!is.character(columnas) || anyNA(columnas) ||
      length(indices_columnas) != length(columnas) ||
      anyNA(indices_columnas)) {
    stop("`columnas` contiene nombres inexistentes.", call. = FALSE)
  }
  columnas_totales <- names(datos)[unique(indices_columnas)]
  columnas <- utils::head(columnas_totales, max_columnas)
  resumen <- list()
  dias <- list()
  huecos <- list()
  propuestas <- list()
  abandonadas <- list()
  h <- 0L
  for (i in seq_along(columnas)) {
    nombre <- columnas[[i]]
    indice <- .indice_nombre(nombre, names(datos))
    formatos <- if (!is.null(perfil)) perfil$formatos_fecha[[indice]] else NULL
    fechas <- .fecha_columna_avanzada(datos[[indice]], formatos)
    if (is.null(fechas)) {
      # Se anota que esta columna quedo afuera y por que. Antes se salteaba en
      # silencio: no aparecia en `resumen` ni en `columnas_omitidas`, y
      # `columnas_analizadas` seguia nombrandola. Un objeto que se contradice a
      # si mismo es peor que uno incompleto.
      abandonadas[[length(abandonadas) + 1L]] <- nombre
      next
    }
    n_excluidas_parseo <- attr(
      fechas, "n_fechas_excluidas_parseo", exact = TRUE
    )
    if (is.null(n_excluidas_parseo) || length(n_excluidas_parseo) != 1L ||
        !is.finite(n_excluidas_parseo)) {
      n_excluidas_parseo <- 0L
    }
    n_excluidas_granularidad <- attr(
      fechas, "n_fechas_excluidas_granularidad", exact = TRUE
    )
    if (is.null(n_excluidas_granularidad) ||
        length(n_excluidas_granularidad) != 1L ||
        !is.finite(n_excluidas_granularidad)) {
      n_excluidas_granularidad <- 0L
    }
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
      n_fechas_excluidas_parseo = as.integer(n_excluidas_parseo),
      n_fechas_excluidas_granularidad = as.integer(n_excluidas_granularidad),
      # El mismo vocabulario que `perfilar()` usa sobre esta misma columna:
      # `calculados_sobre_dias` cuando lo que quedo afuera son periodos de mes,
      # y no un estado distinto para el mismo hecho.
      estado_resumen = if (n_excluidas_granularidad > 0L) {
        "calculados_sobre_dias"
      } else if (n_excluidas_parseo > 0L) {
        "calculados_sobre_fechas_parseadas"
      } else "calculados",
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
    n_fechas_excluidas_parseo = integer(), estado_resumen = character(),
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
  # Una columna que se pidio analizar y no produjo fila NO es una columna
  # analizada. Antes lo era, y el objeto afirmaba haberla analizado mientras su
  # resumen no la mencionaba.
  abandonadas <- unlist(abandonadas, use.names = FALSE)
  attr(resultado, "columnas_analizadas") <- columnas[
    !(.nombres_para_operar(columnas) %in% .nombres_para_operar(abandonadas))
  ]
  attr(resultado, "columnas_omitidas") <- unique(c(
    columnas_totales[
      !(.nombres_para_operar(columnas_totales) %in%
          .nombres_para_operar(columnas))
    ], abandonadas
  ))
  attr(resultado, "columnas_sin_serie_diaria") <- if (is.null(abandonadas)) {
    character()
  } else abandonadas
  attr(resultado, "truncado") <- length(columnas_totales) > length(columnas)
  resultado
}
