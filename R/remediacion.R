.plan_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    grupo = character(), decision_grupo = character(),
    recomendacion_grupo = character(),
    estrategia = character(), recomendada = logical(),
    severidad_origen = character(), evidencia = character(),
    justificacion = character(), n_afectadas = numeric(),
    unidad_conteo = character(),
    reversible = logical(), destructiva = logical(),
    estado = character(), estado_reparacion = character(), aplicar = logical(),
    orden = integer(), stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

.nueva_accion <- function(columna, hallazgo, estrategia, recomendada,
                          justificacion, n_afectadas, reversible,
                          estado = "lista", aplicar = recomendada,
                          parametros = list(), orden = 500L,
                          grupo = NA_character_,
                          decision_grupo = NA_character_,
                          recomendacion_grupo = NA_character_,
                          destructiva = FALSE,
                          estado_reparacion = NA_character_,
                          unidad_conteo = NA_character_) {
  estructura <- data.frame(
    id_accion = "", columna = columna, hallazgo = hallazgo,
    grupo = grupo, decision_grupo = decision_grupo,
    recomendacion_grupo = recomendacion_grupo,
    estrategia = estrategia, recomendada = recomendada,
    severidad_origen = NA_character_, evidencia = "",
    justificacion = justificacion,
    n_afectadas = as.numeric(n_afectadas),
    unidad_conteo = as.character(unidad_conteo), reversible = reversible,
    destructiva = destructiva,
    estado = estado, estado_reparacion = estado_reparacion,
    aplicar = aplicar, orden = as.integer(orden),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list(parametros))
  estructura
}

.id_grupo <- function(indice) sprintf("grupo-%04d", indice)

.nombres_snake <- function(nombres) {
  originales <- as.character(nombres)
  originales[is.na(originales)] <- ""
  transliterados <- .transliterar_ascii(.nombres_para_operar(originales))
  salida <- tolower(trimws(transliterados))
  salida <- gsub("[^[:alnum:]]+", "_", salida, perl = TRUE)
  salida <- gsub("^_+|_+$", "", salida, perl = TRUE)
  salida[!nzchar(salida)] <- "x"
  salida[grepl("^[0-9]", salida)] <- paste0(
    "x_", salida[grepl("^[0-9]", salida)]
  )
  .nombres_unicos(salida, sep = "_")
}

.par_columnas_duplicadas <- function(perfil, hallazgo) {
  pares <- perfil$general$columnas_duplicadas
  if (is.null(pares) || !nrow(pares)) return(NULL)
  evidencias <- paste(pares$columna_1, "=", pares$columna_2)
  # La evidencia es prosa para quien lee y puede crecer —hoy declara sobre
  # cuantas filas se comparo—, asi que el vinculo se hace contra su primer
  # tramo, no contra la cadena entera. Atarlo al texto completo hacia que
  # cualquier agregado colapsara dos pares distintos en el mismo.
  evidencia_hallazgo <- trimws(sub(";.*$", "", hallazgo$evidencia[[1L]]))
  mismas_columnas <- .nombres_para_operar(pares$columna_1) %in%
    .nombres_para_operar(hallazgo$columna[[1L]])
  indices <- which(
    mismas_columnas &
      evidencias == evidencia_hallazgo
  )
  if (!length(indices)) {
    indices <- which(mismas_columnas)
  }
  if (!length(indices)) return(NULL)
  unname(unlist(pares[indices[[1L]], c("columna_1", "columna_2")]))
}

.fila_perfil <- function(perfil, columna) {
  indices <- which(.nombres_para_operar(perfil$columnas$columna) %in%
                    .nombres_para_operar(columna))
  if (length(indices) == 1L) perfil$columnas[indices, , drop = FALSE] else NULL
}

.formatos_perfil <- function(perfil, columna) {
  indices <- which(.nombres_para_operar(perfil$columnas$columna) %in%
                    .nombres_para_operar(columna))
  if (length(indices) == 1L) perfil$formatos_fecha[[indices]] else NULL
}

.accion_columna_ambigua <- function(perfil, columna) {
  sum(.nombres_para_operar(perfil$columnas$columna) %in%
        .nombres_para_operar(columna)) != 1L
}

.estado_columna <- function(perfil, columna, estado = "lista") {
  if (.accion_columna_ambigua(perfil, columna)) "bloqueada" else estado
}

.hallazgos_sin_accion_por_columna_ambigua <- function(perfil) {
  # El plan se arma desde `perfil$hallazgos`, y una columna cuyo nombre aparece
  # mas de una vez no se puede identificar: `.fila_perfil()` devuelve NULL y
  # NINGUNA rama del planificador llega a proponer su accion. Medido sobre una
  # tabla con dos columnas llamadas igual y espacios al borde en las dos: el
  # perfil publica los dos hallazgos `espacios_sobrantes`, el plan publica solo
  # las dos estrategias de nombres, y la cobertura sale vacia. Con los nombres
  # distintos, la misma tabla produce las dos acciones de recorte.
  #
  # Es el reverso exacto de lo que ya esta escrito donde la cobertura se cuelga
  # del plan: ahi el silencio era un diagnostico que no se evaluo, aca es un
  # hallazgo medido que no encontro accion. Se declara en la misma puerta donde
  # se decide que limpiar, porque leer el plan y no ver la accion se lee como
  # "no hay nada que hacer".
  hallazgos <- perfil$hallazgos
  vacio <- data.frame(
    hallazgo = character(), columna = character(), motivo = character(),
    como_resolverlo = character(), stringsAsFactors = FALSE
  )
  if (!inherits(hallazgos, "data.frame") || !nrow(hallazgos)) return(vacio)
  ambiguos <- which(!is.na(hallazgos$columna) & vapply(
    hallazgos$columna,
    function(columna) .accion_columna_ambigua(perfil, columna),
    logical(1L)
  ))
  if (!length(ambiguos)) return(vacio)
  data.frame(
    hallazgo = as.character(hallazgos$tipo_hallazgo[ambiguos]),
    columna = as.character(hallazgos$columna[ambiguos]),
    motivo = paste0(
      "El nombre de columna se repite en la tabla, as\u00ed que no hay forma de ",
      "saber sobre cu\u00e1l de ellas act\u00faa la limpieza: el plan no propone acci\u00f3n ",
      "para este hallazgo."
    ),
    como_resolverlo = paste0(
      "Aplicar `normalizar_nombres` -o renombrar las columnas a mano- y volver ",
      "a perfilar: con nombres distintos el plan propone las acciones de este ",
      "hallazgo."
    ),
    stringsAsFactors = FALSE
  )
}

.es_fecha_ambigua <- function(perfil, columna) {
  any(
    .nombres_para_operar(perfil$hallazgos$columna) %in%
      .nombres_para_operar(columna) &
      perfil$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo",
    na.rm = TRUE
  )
}

.es_fecha_mixta <- function(perfil, columna) {
  any(
    .nombres_para_operar(perfil$hallazgos$columna) %in%
      .nombres_para_operar(columna) &
      perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos",
    na.rm = TRUE
  )
}

.agregar_accion <- function(acciones, accion) {
  acciones[[length(acciones) + 1L]] <- accion
  acciones
}

.comprobar_reversibilidad_accion <- function(datos, columna, estrategia,
                                             parametros) {
  sin_datos <- list(
    verificable = FALSE, ejecutable = FALSE, reversible = FALSE,
    n_no_reversibles = 0L,
    justificacion = paste0(
      "La reversibilidad no se puede comprobar sin los datos completos de `",
      columna, "`; pase `datos` a `planificar_limpieza()` para habilitar esta acci\u00f3n."
    ), error = NULL
  )
  if (is.null(datos)) return(sin_datos)
  indice_columna <- if (is.data.frame(datos)) {
    .indice_nombre(columna, names(datos))
  } else NA_integer_
  if (!is.data.frame(datos) || is.na(indice_columna)) {
    return(list(
      verificable = FALSE, ejecutable = FALSE, reversible = FALSE,
      n_no_reversibles = 0L,
      justificacion = paste0(
        "La columna `", columna,
        "` no est\u00e1 disponible para comprobar la reversibilidad."
      ), error = NULL
    ))
  }
  x <- datos[[indice_columna]]
  convertido <- tryCatch({
    valor <- switch(
      estrategia,
      convertir_tipo = .convertir_tipo(x, parametros),
      convertir_numero_regional = .convertir_numero_regional(x, parametros),
      convertir_fecha_confirmada = .convertir_fecha(x, parametros),
      stop("Estrategia de conversi\u00f3n no reconocida.", call. = FALSE)
    )
    if (is.list(valor) && !is.null(valor$valor)) valor$valor else valor
  }, error = function(e) e)
  if (inherits(convertido, "error")) {
    return(list(
      verificable = TRUE, ejecutable = FALSE, reversible = FALSE,
      n_no_reversibles = 0L,
      justificacion = paste0(
        "La conversi\u00f3n no es ejecutable sobre los datos completos: ",
        conditionMessage(convertido),
        " Se conserva como acci\u00f3n destructiva no recomendada."
      ), error = conditionMessage(convertido)
    ))
  }
  evaluacion <- .evaluar_conversion(x, convertido, estrategia, parametros)
  riesgos <- character()
  if (evaluacion$n_ceros_iniciales) {
    riesgos <- c(
      riesgos,
      paste0(evaluacion$n_ceros_iniciales,
             " valores tienen cero inicial y se conserva su identidad textual")
    )
  }
  if (!evaluacion$inyectiva) {
    riesgos <- c(
      riesgos,
      paste0(evaluacion$n_colisionados,
             " valores participan en una conversi\u00f3n no inyectiva")
    )
  }
  # Sin esto la justificacion salia con un punto suelto adelante -"`. Se declara
  # destructiva`"- porque el unico riesgo presente no tenia texto. Declarar algo
  # destructivo sin decir por que deja al usuario sin nada que decidir.
  if (isTRUE(evaluacion$n_redondeados > 0L)) {
    riesgos <- c(
      riesgos,
      paste0(
        evaluacion$n_redondeados,
        " valores tienen m\u00e1s cifras de las que guarda un n\u00famero de ",
        "doble precisi\u00f3n y se redondear\u00edan sin poder recuperarse"
      )
    )
  }
  justificacion <- if (evaluacion$reversible) {
    "La conversi\u00f3n es ejecutable e inyectiva sobre todos los valores presentes."
  } else {
    paste0(
      paste(riesgos, collapse = "; "),
      ". Se declara destructiva y no se recomienda autom\u00e1ticamente."
    )
  }
  list(
    verificable = TRUE, ejecutable = TRUE,
    reversible = evaluacion$reversible,
    n_no_reversibles = evaluacion$n_no_reversibles,
    justificacion = justificacion, error = NULL
  )
}

.evaluar_conversion <- function(original, convertido, estrategia, parametros) {
  antes <- .texto_representacion_conversion(original)
  despues <- .texto_representacion_conversion(convertido)
  presentes <- !is.na(antes) & nzchar(trimws(antes)) & !is.na(despues)
  colisionados <- rep(FALSE, length(antes))
  if (any(presentes)) {
    pares <- unique(data.frame(
      original = .nombres_para_operar(antes[presentes]),
      convertido = .nombres_para_operar(despues[presentes]),
      stringsAsFactors = FALSE
    ))
    por_valor <- split(pares$original, pares$convertido)
    repetidos <- unique(unlist(por_valor[lengths(por_valor) > 1L],
                               use.names = FALSE))
    colisionados <- presentes & .nombres_para_operar(antes) %in% repetidos
  }
  destino <- if (identical(estrategia, "convertir_numero_regional")) {
    "numerico"
  } else {
    parametros$tipo
  }
  numerico <- destino %in% c("numerico", "entero", "doble")
  ceros <- rep(FALSE, length(antes))
  if (numerico) {
    ceros <- !is.na(antes) & grepl("^0[0-9]", trimws(antes), perl = TRUE)
  }
  # Un entero por encima de 2^53 NO entra en un `double` sin redondear, y esa
  # perdida no produce colision: `9007199254740993` se vuelve
  # `9007199254740992` y ningun otro valor termina ahi, asi que la comprobacion
  # de arriba -que compara representaciones textuales- no la ve. El plan
  # publicaba entonces `reversible = TRUE`, `destructiva = FALSE` y
  # `n_no_reversibles = 0` sobre un valor que habia destruido: ningun formato
  # recupera el 3 perdido.
  #
  # Es la misma regla que ya aplica el perfil, que por encima de 2^53 se niega
  # a medir la secuencia entera en vez de publicar un numero redondeado. Se
  # mira el valor CONVERTIDO y no el texto de entrada, para que no dependa de
  # como venia escrito -con puntos de miles, con signo o en notacion regional-.
  #
  # La comprobacion NO puede ser de magnitud. `9007199254740993` redondea a
  # `9007199254740992`, que es 2^53 exacto, asi que `> 2^53` da FALSE sobre el
  # unico valor que hay que atrapar: la guarda se prueba con el valor donde
  # cambia. Se comparan los DIGITOS del original contra los del convertido, que
  # es exacto y ademas no depende de como viniera escrito.
  redondeados <- rep(FALSE, length(antes))
  if (numerico) {
    # La perdida ocurre al parsear, y se mide sobre el texto EXACTO que el
    # conversor le pasa a `as.numeric()`: el que devuelve su propia
    # normalizacion, antes de cualquier unidad. Esta guarda rearmaba antes ese
    # texto por su cuenta, con una rama por formato, y en cinco rondas cada
    # formato nuevo fue un agujero -entero plano, con `%`, cientifica, mantisa
    # con coma, entero es-UY con `,0`-. Con la normalizacion compartida no hay
    # formato que el conversor lea y la guarda no vea.
    texto <- tryCatch(
      if (identical(estrategia, "convertir_numero_regional")) {
        .texto_regional_normalizado(original, parametros)$texto
      } else {
        .texto_tipo_normalizado(original)
      },
      error = function(e) NULL
    )
    if (!is.null(texto) && length(texto) == length(antes)) {
      valores <- suppressWarnings(as.numeric(texto))
      finitos <- presentes & !is.na(valores) & is.finite(valores)
      # Los digitos que cuentan son los de la mantisa: el exponente solo
      # corre la coma. Sin ceros en ninguno de los dos bordes.
      significativos <- function(x) {
        x <- gsub("[^0-9]", "", sub("[eE].*$", "", x))
        sub("0+$", "", sub("^0+", "", x))
      }
      # Un entero se compara contra su valor exacto: por encima de 2^53 el
      # `double` guarda otro entero.
      enteros <- which(finitos & valores == floor(valores))
      if (length(enteros)) {
        antes_sig <- significativos(texto[enteros])
        despues_sig <- significativos(sprintf("%.0f", abs(valores[enteros])))
        redondeados[enteros] <- nzchar(antes_sig) & antes_sig != despues_sig
      }
      # Un decimal se compara con la precision con que VINO ESCRITO. Esta guarda
      # miraba solo enteros, porque compararlos contra la expansion binaria
      # -`1234.56` es `1234.5599999999999...`- acusaria a toda columna con
      # decimales. Pero a su propia precision todo decimal de hasta 15 cifras
      # significativas vuelve exacto, y lo que no vuelve es perdida real:
      # `33.333333333333333333%` -20 cifras- se publicaba reversible y el
      # `double` solo guarda 17.
      decimales <- which(finitos & valores != floor(valores))
      if (length(decimales)) {
        antes_sig <- significativos(texto[decimales])
        cifras <- pmax(1L, nchar(antes_sig))
        despues_sig <- significativos(
          sprintf("%.*g", cifras, abs(valores[decimales]))
        )
        redondeados[decimales] <- nzchar(antes_sig) & antes_sig != despues_sig
      }
    }
  }
  riesgos <- colisionados | ceros | redondeados
  list(
    inyectiva = !any(colisionados),
    reversible = !any(riesgos),
    n_no_reversibles = as.integer(sum(riesgos)),
    n_ceros_iniciales = as.integer(sum(ceros)),
    n_colisionados = as.integer(sum(colisionados)),
    n_redondeados = as.integer(sum(redondeados))
  )
}

# Las acciones que transforman CELDA por celda. Son las unicas que pueden
# respetar el universo aplicable: se aplican y despues se devuelven a su valor
# las celdas de fuera.
#
# Este comentario decia que conservan la clase de la columna, y sobre un FACTOR
# no es cierto: todas devuelven texto -`.resultado_texto()` lo decide en un solo
# lugar, a proposito, para no dejar un factor incompleto-. La afirmacion falsa
# se quedo escrita mientras el plan recomendaba y ACTIVABA `recortar_espacios`
# sobre un factor ordenado, publicando `destructiva = FALSE`; la columna volvia
# `character`, sin orden y sin los niveles sin observaciones. Lo que el usuario
# lee ahora lo dice: ver `.declarar_texto_en_columna_factor()`. Las conversiones de
# tipo no estan aca porque no se pueden aplicar a medias -una columna tiene un
# solo tipo-, y para ellas el plan declara el alcance real.
.estrategias_por_celda <- c(
  "convertir_ausencias_textuales", "recortar_espacios",
  "eliminar_controles_invisibles", "normalizar_espacios_invisibles",
  "decodificar_entidades_html", "reemplazar_separadores",
  "reparar_codificacion", "convertir_minusculas", "convertir_mayusculas",
  "convertir_titulo", "convertir_segun_diccionario",
  "convertir_sentinelas_numericos", "winsorizar_outliers"
)
.declarar_texto_en_columna_factor <- function(plan, perfil) {
  # Una sola vez para todas las acciones por celda, y no una por estrategia: lo
  # que cambia la clase no es la estrategia sino la columna, y el contrato de
  # devolver texto esta escrito en un solo lugar. Enumerar las nueve ramas del
  # planificador que aceptan un factor era enumerar sitios; la pregunta es si la
  # columna declara un factor.
  #
  # Donde se declara: en la justificacion, que es lo que se lee para decidir. Y
  # si el factor declara un ORDEN, la accion queda recomendada pero sin activar,
  # con el mismo idioma que ya usan las acciones contextuales del plan: el orden
  # es algo que el usuario declaro y que el texto no puede llevar, asi que la
  # decision es suya.
  if (!nrow(plan)) return(plan)
  for (j in seq_len(nrow(plan))) {
    if (!plan$estrategia[[j]] %in% .estrategias_por_celda) next
    columna <- plan$columna[[j]]
    if (is.na(columna)) next
    fila <- .fila_perfil(perfil, columna)
    if (is.null(fila)) next
    if (!as.character(fila$tipo_declarado[[1L]]) %in%
          c("factor", "factor-ordenado")) {
      next
    }
    ordenado <- identical(as.character(fila$tipo_declarado[[1L]]),
                          "factor-ordenado")
    plan$justificacion[[j]] <- paste0(
      plan$justificacion[[j]],
      " La columna es un factor", if (ordenado) " ordenado" else "",
      " y la acci\u00f3n devuelve texto: ",
      if (ordenado) "el orden declarado y " else "",
      "los niveles sin observaciones no se conservan.",
      if (ordenado) {
        paste0(
          " Queda recomendada pero sin activar: conservar el orden es una ",
          "decisi\u00f3n del dominio."
        )
      } else {
        ""
      }
    )
    if (ordenado) plan$aplicar[[j]] <- FALSE
  }
  plan
}

.estrategias_cambio_de_tipo <- c(
  "convertir_tipo", "convertir_numero_regional", "convertir_fecha_confirmada"
)
.estrategias_por_fila <- c(
  "marcar_filas_ausentes", "eliminar_filas_ausentes"
)

# Una conversion que se ejecuto sobre los datos y perdio algo es destructiva,
# sea o no segura la columna. La marca se calculaba solo cuando la columna era
# segura: sobre `10%` junto a `0.1` -que no lo es- el plan media cinco valores
# irrecuperables, su propio motivo decia "Se declara destructiva", y la marca
# quedaba en FALSE. Activada a mano, el aviso de acciones destructivas no
# aparecia y el registro repetia FALSE. Lo que no se pudo ejecutar no cuenta:
# una columna ambigua que solo espera configuracion no destruye nada.
.perdida_medida <- function(comprobacion) {
  isTRUE(comprobacion$ejecutable) && isFALSE(comprobacion$reversible)
}

# Lo que la conversion regional hace ademas de cambiar el tipo. Un `%` se
# divide por 100: sin decirlo, "10%" pasaba a 0.1 con una justificacion que
# aseguraba que la columna se convertia "sin elegir entre interpretaciones".
.nota_unidad_numero <- function(parametros) {
  unidad <- parametros$unidad
  moneda <- parametros$moneda
  nota <- character()
  if (length(unidad) == 1L && !is.na(unidad) && nzchar(unidad)) {
    nota <- c(nota, if (identical(unidad, "%")) {
      paste0(
        " Los valores con \"%\" se dividen por 100 y quedan como proporci\u00f3n ",
        "en [0, 1]."
      )
    } else {
      paste0(
        " La unidad ", encodeString(unidad, quote = "\""), " deja de formar ",
        "parte del valor; queda en los par\u00e1metros."
      )
    })
  }
  if (length(moneda) == 1L && !is.na(moneda) && nzchar(moneda)) {
    nota <- c(nota, paste0(
      " El s\u00edmbolo ", encodeString(moneda, quote = "\""), " deja de formar ",
      "parte del valor; queda en los par\u00e1metros."
    ))
  }
  paste(nota, collapse = "")
}

# Por que una columna de numeros como texto no se convierte. Se decia siempre
# que faltaba evidencia para distinguir el separador decimal del de miles,
# tambien en `5` junto a `5 %`, que no tiene separadores: el motivo publicado
# no era el motivo.
.motivo_numero_no_seguro <- function(fila) {
  # Un perfil de base de datos o guardado por una version anterior puede no
  # traer estos campos, o traerlos vacios: se leen sin suponer que estan, y lo
  # que no se sabe no se afirma.
  convencion <- fila[["numero_texto_convencion"]]
  proporcion <- fila[["proporcion_numeros_texto"]]
  convencion <- if (length(convencion)) as.character(convencion[[1L]]) else NA_character_
  proporcion <- if (length(proporcion)) as.numeric(proporcion[[1L]]) else NA_real_
  if (isTRUE(convencion %in% c("ambigua", "mixta"))) {
    paste0(
      "La columna no aporta evidencia suficiente para distinguir el ",
      "separador decimal del separador de miles."
    )
  } else if (isTRUE(proporcion < 1)) {
    paste0(
      "Hay valores presentes que no responden al formato num\u00e9rico de ",
      "la columna; convertirlos exigir\u00eda decidir qu\u00e9 hacer con ellos."
    )
  } else if (isTRUE(proporcion == 1)) {
    paste0(
      "La columna mezcla unidades o monedas, o valores con y sin ellas; ",
      "convertirla exigir\u00eda decidir si un n\u00famero sin unidad est\u00e1 en ",
      "la misma escala que los dem\u00e1s."
    )
  } else {
    paste0(
      "La conversi\u00f3n exige que todos los valores presentes compartan ",
      "convenci\u00f3n decimal, unidad y moneda, y el perfil no lo acredita."
    )
  }
}

# Lleva la regla de aplicabilidad del perfil a las acciones del plan. Sin esto
# la limpieza operaba sobre la columna entera aunque el usuario hubiera
# declarado que una parte de las filas no le corresponde: una `S/D` en una fila
# declarada fuera se volvia `NA`, y el plan que prometia 2 cambios hacia 4.
.aplicabilidad_en_plan <- function(resultado, perfil) {
  reglas <- perfil$meta$reglas_aplicabilidad
  if (!length(reglas) || !nrow(resultado)) return(resultado)
  for (i in seq_len(nrow(resultado))) {
    columna <- resultado$columna[[i]]
    if (is.na(columna) || !columna %in% names(reglas)) next
    estrategia <- as.character(resultado$estrategia[[i]])
    if (estrategia %in% .estrategias_por_celda) {
      parametros <- resultado$parametros[[i]]
      if (is.null(parametros)) parametros <- list()
      parametros$aplicabilidad <- reglas[[columna]]
      resultado$parametros[[i]] <- parametros
    } else if (estrategia %in% .estrategias_por_fila) {
      parametros <- resultado$parametros[[i]]
      if (is.null(parametros)) parametros <- list()
      parametros$aplicabilidad <- reglas[[columna]]
      resultado$parametros[[i]] <- parametros
    } else if (estrategia %in% .estrategias_cambio_de_tipo) {
      # No se puede aplicar a medias: se declara el alcance real.
      fila <- .fila_perfil(perfil, columna)
      if (!is.null(fila) && all(c("n_aplicables", "n_faltantes",
                                  "n_presentes_fuera_de_aplicabilidad") %in% names(fila))) {
        fuera <- fila$n_presentes_fuera_de_aplicabilidad[[1L]]
        if (isTRUE(fuera > 0L)) {
          resultado$n_afectadas[[i]] <- fila$n_aplicables[[1L]] -
            fila$n_faltantes[[1L]] + fuera
          resultado$justificacion[[i]] <- paste0(
            resultado$justificacion[[i]], " La conversi\u00f3n cambia la ",
            "columna entera, incluidas ", fuera, " celdas fuera del universo ",
            "aplicable: una columna tiene un solo tipo."
          )
        }
      }
    }
  }
  resultado
}

# Devuelve a su valor las celdas fuera del universo aplicable despues de que la
# accion las transformo, y recuenta. Es un solo lugar para todos los ejecutores
# por celda, en vez de una copia de esta logica en cada uno.
.restringir_a_aplicabilidad <- function(ejecutada, anterior, accion,
                                        original = anterior) {
  estrategia <- as.character(accion$estrategia[[1L]])
  regla <- accion$parametros[[1L]]$aplicabilidad
  columna <- accion$columna[[1L]]
  if (is.null(regla) || !estrategia %in% .estrategias_por_celda ||
      is.na(columna) || !columna %in% names(anterior)) {
    return(ejecutada)
  }
  # La regla se evalua sobre los datos ORIGINALES, no sobre `anterior`. El
  # universo lo define el perfil, que se calculo sobre los originales. Evaluada
  # sobre el estado ya modificado, una accion previa podia cambiar la columna
  # de la que depende la regla -quitarle un invisible a `cat` y volverla "x"- y
  # meter en el universo una fila que el perfil habia declarado fuera: el
  # recorte tocaba justo esa celda y el registro lo daba por ejecutado.
  if (nrow(original) != nrow(anterior)) {
    # Una accion previa elimino filas y la mascara de los originales ya no se
    # alinea con las que quedan. Rastrear cuales sobrevivieron seria fragil;
    # tocar la columna entera seria el defecto. Se falla y se dice.
    ejecutada$error <- paste(
      "No se puede respetar la aplicabilidad despues de eliminar filas:",
      "aplique las acciones por celda antes de las que eliminan filas."
    )
    return(ejecutada)
  }
  mascara <- tryCatch(
    .evaluar_predicado_aplicabilidad(original, columna, regla),
    error = function(e) e
  )
  if (inherits(mascara, "error")) {
    # Sin poder evaluar la regla sobre estos datos no se puede respetar el
    # universo, y tocar la columna entera seria justo el defecto.
    ejecutada$error <- paste(
      "No se pudo evaluar la regla de aplicabilidad sobre estos datos:",
      conditionMessage(mascara)
    )
    return(ejecutada)
  }
  # Indeterminado es "no se sabe si corresponde": no se toca.
  fuera <- is.na(mascara) | !mascara
  if (!any(fuera)) return(ejecutada)
  viejo <- anterior[[columna]]
  nuevo <- ejecutada$datos[[columna]]
  # Los ejecutores de texto devuelven `character` aunque la entrada sea factor;
  # asignar el factor directo lo convertiria en sus codigos numericos.
  restaurar <- viejo[fuera]
  if (is.factor(restaurar) && !is.factor(nuevo)) restaurar <- as.character(restaurar)
  nuevo[fuera] <- restaurar
  ejecutada$datos[[columna]] <- nuevo
  cambiadas <- sum(.celdas_cambiadas(viejo, nuevo))
  n_anterior <- ejecutada$n
  ejecutada$n <- cambiadas
  # Si la accion contaba como irreversible cada cambio, sigue contando igual.
  if (!is.null(ejecutada$n_no_reversibles) && isTRUE(ejecutada$n_no_reversibles > 0L) &&
      isTRUE(ejecutada$n_no_reversibles == n_anterior)) {
    ejecutada$n_no_reversibles <- cambiadas
  }
  ejecutada
}

#' Construir y aplicar un plan de limpieza auditable
#'
#' `planificar_limpieza()` transforma los hallazgos de un objeto `perfil` en un
#' objeto de datos editable, sin modificar los datos examinados. Cada fila
#' representa una acción propuesta. Sólo se marcan como recomendadas las
#' estrategias correctas con independencia del dominio; algunas, como marcar
#' valores extremos, permanecen inactivas hasta que se decida actuar. Las
#' decisiones contextuales quedan desactivadas y los formatos de fecha ambiguos
#' quedan bloqueados.
#'
#' `aplicar()` ejecuta exclusivamente las filas con `aplicar == TRUE`, sobre una
#' copia de `datos`. Verifica que cada columna siga siendo identificable y que
#' las conversiones sean completas antes de sustituirla. Devuelve los datos
#' nuevos junto con un registro de las acciones y sus parámetros. El mismo
#' registro queda en el atributo `registro_limpieza` de los datos resultantes.
#'
#' Las alternativas para un mismo hallazgo comparten `grupo`; las acciones
#' independientes usan `NA`. Como máximo una alternativa de cada grupo puede
#' tener `aplicar == TRUE`, invariante que `aplicar()` vuelve a validar. No se
#' agrega una fila ficticia para "no hacer nada": `decision_grupo` distingue
#' `pendiente`, `recomendada`, `desactivada`, `elegida` y `omitida`, mientras
#' `recomendacion_grupo = "no_hacer_nada"` representa una recomendación
#' explícita de conservar los datos. Esto permite separar un grupo aún no
#' revisado de una omisión deliberada.
#'
#' `estado` distingue acciones `lista`, `bloqueada` e `informativa`; `orden`
#' fija la secuencia reproducible. Si dos acciones comparten el mismo `orden`,
#' el empate lo resuelve `id_accion`, no la posición de la fila: reordenar el
#' plan no cambia el resultado. Y un grupo marcado como `elegida` sin ninguna
#' acción activa se rechaza, porque no es ni una elección ni una omisión. `n_afectadas` es la estimación del perfil
#' sobre **lo que esta acción tocaría**, que puede ser menos que el conteo del
#' hallazgo que la originó cuando la acción sólo cubre parte del caso: una
#' columna con tres valores de codificación rota, de los cuales uno es
#' reparable, produce un hallazgo con `n_afectados = 3` y una acción
#' `reparar_codificacion` con `n_afectadas = 1`. Las dos cifras son ciertas y
#' cuentan cosas distintas. `unidad_conteo` dice si cuenta filas, columnas o
#' valores distintos —lo
#' declara la acción cuando cuenta en una unidad propia, y sólo si no lo hace se
#' hereda del hallazgo—. El registro informa `n_cambiadas` sobre los datos
#' recibidos.
#'
#' **`n_afectadas` y `n_cambiadas` pueden no coincidir, y las dos son ciertas.**
#' La estimación se calcula sobre los datos que se perfilaron; el registro
#' cuenta lo que pasó al aplicar. Si una acción anterior del mismo plan ya tocó
#' esa columna, la posterior encuentra menos —o más— de lo estimado: con
#' `convertir_sentinelas_numericos` (orden 110) convirtiendo tres `-999` en
#' ausentes, `winsorizar_outliers` (orden 520) recorta dos valores donde el plan
#' estimaba siete. No es un desvío que ocultar: `orden`, `n_afectadas` y
#' `n_cambiadas` se publican los tres, y compararlos es la forma de ver el
#' efecto de la composición. Sólo el caso extremo —la acción no produce **ningún**
#' efecto —también cuando el plan no trae una estimación válida— se registra
#' como `fallida` con su motivo.
#'
#' **Esa comparación sólo lee composición cuando las dos cifras cuentan en la
#' misma unidad.** `n_afectadas` cuenta en la `unidad_conteo` que el plan
#' declara y `n_cambiadas` cuenta lo que la acción tocó al aplicarse, que es
#' su unidad natural. Cuando la acción declara `unidad_conteo =
#' "valor_distinto"` —las conversiones de mayúsculas y minúsculas—, las dos
#' cifras miden poblaciones distintas y su diferencia no dice nada sobre la
#' composición: medido sobre `c("Ana", "ana", "ANA", "Beto", " Ana ")`,
#' `convertir_minusculas` estima `n_afectadas = 3` valores distintos y el
#' registro publica `n_cambiadas = 4` celdas. Las dos son ciertas. La unidad
#' del plan se recupera uniendo el registro con el plan por `id_accion`, que
#' los dos publican. `reversible`
#' indica si la conversión conserva la identidad de cada valor. Las
#' conversiones se comprueban sobre todos los valores de `datos`: las numéricas
#' bloquean ceros iniciales, colisiones no inyectivas y valores con más cifras
#' significativas de las que guarda un número de doble precisión —un decimal se
#' compara con la precisión con que vino escrito—, mientras que fechas,
#' fechas-hora y lógicos sólo bloquean conversiones no ejecutables o no
#' inyectivas. Las fechas pueden cambiar a la representación canónica del tipo
#' sin que eso sea una pérdida. Sin `datos` no se puede hacer la comprobación y
#' la acción queda bloqueada. Cuando **una conversión de tipo** se ejecuta y no
#' es reversible se marca `destructiva` —también si la columna no era segura y
#' se activa a mano—, no se activa por defecto y el registro conserva
#' `n_no_reversibles` y la justificación de la decisión.
#'
#' `destructiva` no es sinónimo de "pierde algo": marca las acciones que el
#' usuario tiene que activar a mano —las que retiran filas o columnas y las
#' conversiones que pierden representación— y por eso ninguna acción
#' `destructiva` puede estar `recomendada`. Hay acciones recomendadas que sí
#' pierden el valor de una celda: `convertir_ausencias_textuales` cambia un
#' marcador por `NA` y `eliminar_controles_invisibles` quita un carácter. Esas
#' lo dicen en su justificación y el registro las cuantifica en
#' `n_no_reversibles`; leer `destructiva = FALSE` no significa que no se haya
#' perdido nada, sino que el paquete pudo recomendar la acción sin conocer el
#' dominio.
#' Sobre una columna **factor** las acciones por celda devuelven texto: el
#' resultado no puede ser un factor incompleto, así que el orden declarado y los
#' niveles sin observaciones no se conservan. La justificación de la acción lo
#' dice, y si el factor es ordenado la acción queda recomendada pero **sin
#' activar**, porque conservar el orden es una decisión del dominio.
#'
#' Dos atributos del plan declaran lo que el plan no cubre.
#' `cobertura_diagnosticos` trae los diagnósticos que el perfil no pudo
#' evaluar, para que leer tres acciones no se confunda con "lo demás está
#' bien". `hallazgos_sin_accion_por_columna_ambigua` trae los hallazgos medidos
#' que no produjeron acción porque su columna comparte nombre con otra y no hay
#' forma de saber sobre cuál actuaría la limpieza; el remedio es normalizar los
#' nombres y volver a perfilar. Los dos se imprimen con el plan.
#'
#' `convertir_numero_regional` sólo se recomienda si todos los valores
#' presentes comparten convención decimal, unidad y moneda. Un valor con `%`
#' se divide por 100 y queda como proporción en `[0, 1]`, la escala con que el
#' paquete publica toda proporción; una unidad o un símbolo de moneda dejan de
#' formar parte del valor y quedan en `parametros`. La justificación lo dice
#' cuando ocurre. Una columna que mezcla `5` con `5 %` no se convierte: habría
#' que decidir si el número sin `%` está en la misma escala.
#' La acción de codificación prueba las tablas congeladas de varias
#' codificaciones y deja en `estado_reparacion` uno de `reparado`,
#' `reparado_parcialmente` o `no_se_pudo`. Una reparación parcial no se activa
#' automáticamente: debe revisarse y seleccionarse de forma explícita. La
#' estrategia se llama `reparar_codificacion` y no limita el motor a latin-1.
#' Unos mismos bytes pueden recibir dos diagnósticos: `c2 80` es a la vez un
#' carácter de control C1 y la huella de un `€` de Windows-1252 leído como
#' latin-1. Cuando el plan propone reparar la codificación y eliminar ese
#' control, manda la reparación —corre primero y restituye el `€`—, y la
#' eliminación queda sin nada que hacer. Es la lectura habitual en texto real;
#' en el caso raro de un control C1 genuino, la reparación lo convierte en `€`.
#' Una celda cuyo texto no se puede leer —declarada `bytes` con bytes que no
#' son UTF-8, o sin marca en una sesión UTF-8, que es lo que deja [read.csv()]
#' sin `fileEncoding` sobre un archivo latin1— no se transforma: el perfil no la
#' midió y la informa como `codificacion_invalida`, cuyo remedio es volver a leer
#' la fuente declarando su codificación. Las acciones de texto trabajan sobre las
#' demás celdas de la columna y cuentan sólo lo que cambiaron.
#' Si se marca una acción que no está `lista`, `aplicar()` aborta antes de
#' modificar la copia y enumera las filas problemáticas. Una acción que sí está
#' lista pero falla se registra con su error y no impide aplicar las siguientes:
#' cada una conserva atomicidad sobre su propia columna o tabla. Las acciones
#' que efectivamente eliminan filas o columnas requieren además
#' `permitir_eliminacion = TRUE`; una conversión `destructiva` requiere selección
#' explícita y deja la pérdida cuantificada. Por defecto, el resultado conserva
#' lo retirado en `eliminados`; use `conservar_eliminados = FALSE` para evitar
#' ese costo de memoria.
#'
#' Los hallazgos `controles_invisibles`, `entidades_html` y `separadores_en_campo`
#' tienen acciones separadas. La detección de invisibles informa tanto los
#' caracteres que se pueden normalizar como los ZWJ/ZWNJ significativos; la
#' normalización actúa sobre un conjunto más pequeño que la detección. La acción
#' `eliminar_controles_invisibles` quita controles C0/C1 que no son separadores
#' y los invisibles Unicode de transporte, y se recomienda por defecto; conserva
#' ZWJ/ZWNJ. `normalizar_espacios_invisibles` colapsa espacios Unicode (incluido
#' NBSP) a un espacio ASCII, no se recomienda por defecto y registra la pérdida
#' de reversibilidad. `decodificar_entidades_html` cubre las entidades
#' con nombre comunes en español y referencias numéricas válidas, pero no se
#' activa sola porque un ampersand puede ser contenido legítimo.
#' `reemplazar_separadores` convierte tabulaciones, saltos de línea, avances de
#' página y tabulaciones verticales (`\\t`, `\\n`, `\\r`, `\\r\\n`, `\\f` y `\\v`)
#' en un espacio y también requiere una decisión explícita. Las tres acciones
#' registran el número de valores cambiados. Una comparación aproximada con
#' `normalizar = TRUE` usa estas mismas clases: colapsa espacios y omite basura
#' de transporte, pero conserva ZWJ/ZWNJ.
#'
#' Las imputaciones por dependencia funcional se ofrecen desactivadas. Aunque
#' una dependencia exacta permite deducir un valor sin usar media, moda o un
#' modelo externo, sigue siendo una regularidad aprendida de una sola entrega y
#' puede reflejar un error sistemático en vez de una regla de negocio. El plan
#' conserva el mapa y su soporte para que el usuario la confirme; sólo entonces
#' se aplica y se vuelve a validar contra los datos recibidos. Si la protección
#' enmascaró alguna clave del mapa, éste no se usa como tabla de cruce: la
#' relación se reconstruye sobre los datos recibidos con el soporte declarado,
#' sin publicar sus valores.
#'
#' `marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada` reproduce la
#' semántica de [duplicated()] y marca sólo las apariciones posteriores;
#' `.grupo_duplicado` identifica a **todas** las filas que participan en cada
#' grupo de contenido idéntico. Marcar no elimina filas: con las dos columnas
#' incluidas, un perfil posterior tampoco vuelve a contar esas filas como
#' duplicadas exactas, porque las marcas las distinguen. Para saber si los
#' duplicados siguen en la tabla hay que quitar las columnas de marca antes de
#' perfilarla.
#'
#' El orden operativo se aparta deliberadamente de la secuencia dimensional
#' frescura–completitud–exactitud–consistencia–unicidad sugerida por el marco.
#' Primero marca duplicados sin borrar, luego normaliza ausencias y texto, y
#' deja los cambios de esquema para el final. Esto evita perder la evidencia
#' original, permite imputar antes de convertir tipos y mantiene identificables
#' las columnas durante todo el plan. Las eliminaciones nunca se activan por
#' defecto, por lo que deduplicar temprano no puede hacer desaparecer registros.
#' `destructiva` también marca una conversión que pierde representación, aunque
#' no elimine filas o columnas. El consentimiento `permitir_eliminacion` sólo
#' se exige para las estrategias que efectivamente retiran filas o columnas;
#' una conversión destructiva requiere que el usuario la active explícitamente
#' y deja su pérdida cuantificada en el registro.
#'
#' **Marcar o eliminar ausentes va después de normalizarlos**, no antes. Una
#' columna con `"N/A"` y `"sin dato"` tiene ausentes disfrazados que
#' `convertir_ausencias_textuales` convierte en `NA` reales; si la marca
#' corriera primero, quedaría una columna `.ausente_x` que dice `FALSE` en
#' filas que terminan en `NA` —una afirmación falsa sobre los datos, publicada
#' con las dos acciones en `ejecutada` y sin error—. Medido sobre siete formas
#' de tabla antes de corregirlo, tres producían esa marca falsa siguiendo el
#' idioma documentado `plan$aplicar <- plan$recomendada`. Por la misma razón,
#' `eliminar_filas_ausentes` también va después: eliminar antes deja sin
#' eliminar las filas cuyo ausente todavía estaba disfrazado.
#'
#' @param perfil Objeto de clase `perfil` creado por [perfilar()].
#' @param datos Datos opcionales que originaron el perfil. Son necesarios para
#'   comprobar la reversibilidad de conversiones sobre todos los valores y para
#'   proponer imputaciones deducidas de dependencias funcionales.
#' @param soporte_minimo_dependencia Cantidad mínima de observaciones
#'   concordantes por valor determinante para proponer una imputación.
#' @param plan Objeto de clase `plan_limpieza` o data frame con el mismo
#'   contrato. Puede filtrarse y editarse antes de aplicarlo.
#' @param datos `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el
#'   plan. El objeto recibido no se modifica.
#'
#' @return `planificar_limpieza()` devuelve un data frame de clase
#'   `plan_limpieza`. `aplicar()` devuelve una lista de clase
#'   `resultado_limpieza` con `datos`, `registro`, `plan_aplicado`, el `plan`
#'   sincronizado y `eliminados`. El `registro` conserva `estado` (`ejecutada`
#'   o `fallida`), `error`, `n_no_reversibles` y la `justificacion` de cada
#'   acción seleccionada, incluso cuando una falla y las siguientes continúan.
#'   `n_no_reversibles` cuenta las celdas cuyo VALOR se perdió y no se puede
#'   recuperar desde el resultado: un centinela `-999` que pasa a ausencia, un
#'   extremo recortado a su límite, un marcador de ausencia convertido, o un
#'   número que se redondeó al convertirlo. Las acciones que **normalizan la
#'   escritura** —recortar espacios, quitar un invisible de transporte,
#'   reemplazar separadores o cambiar mayúsculas— dejan el valor en su lugar y
#'   no cuentan, **salvo cuando la normalización fusiona valores que eran
#'   distintos**: ahí lo que los separaba no queda en ningún lado y esas celdas
#'   sí se cuentan. El criterio se mide sobre el resultado, no por el nombre de
#'   la acción: quitar un guion suave de `PRO<U+00AD>DUCTO-A` no pierde nada,
#'   y quitar un espacio de ancho cero que distinguía dos claves fusiona las
#'   dos.
#'   Si una acción seleccionada no produce ningún efecto, se registra como
#'   `fallida` con el motivo y su copia no se incorpora al resultado. La
#'   comprobación del efecto observa `n_cambiadas` aunque `n_afectadas` sea
#'   `NA` o haya sido editado: una acción seleccionada que no cambia nada queda
#'   `fallida`, porque la ausencia de efecto es observable sin depender de la
#'   estimación.
#'   Si una columna de entrada es un factor, las acciones que transforman su
#'   texto devuelven una columna `character`: no se reconstruyen los niveles
#'   originales, porque una limpieza puede introducir valores nuevos.
#' @export
#' @seealso [perfilar()], [guiar_limpieza()], [detectar_dependencias()]
#'
#' @examples
#' datos <- data.frame(categoria = c(" A", "S/D", "B"))
#' perfil <- perfilar(datos)
#' plan <- planificar_limpieza(perfil, datos)
#' plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
#' resultado <- aplicar(plan, datos)
#' resultado$datos
planificar_limpieza <- function(perfil, datos = NULL,
                                soporte_minimo_dependencia = 2L) {
  if (!inherits(perfil, "perfil")) {
    stop("`perfil` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  if (!is.null(datos) && !inherits(datos, "data.frame")) {
    stop("`datos` debe ser NULL o heredar de data.frame.", call. = FALSE)
  }
  if (!is.null(datos)) datos <- .tabla_base(datos)
  if (length(soporte_minimo_dependencia) != 1L ||
      is.na(soporte_minimo_dependencia) ||
      !is.finite(soporte_minimo_dependencia) ||
      soporte_minimo_dependencia < 1L ||
      soporte_minimo_dependencia != floor(soporte_minimo_dependencia)) {
    stop("`soporte_minimo_dependencia` debe ser un entero positivo.",
         call. = FALSE)
  }
  acciones <- list()
  hallazgos <- perfil$hallazgos

  for (i in seq_len(nrow(hallazgos))) {
    hallazgo <- hallazgos[i, , drop = FALSE]
    tipo <- hallazgo$tipo_hallazgo[[1L]]
    columna <- hallazgo$columna[[1L]]
    grupo_hallazgo <- .id_grupo(i)
    fila <- if (!is.na(columna)) .fila_perfil(perfil, columna) else NULL
    estado_columna <- if (!is.na(columna)) {
      .estado_columna(perfil, columna)
    } else {
      "lista"
    }

    if (identical(tipo, "faltantes") && !is.null(fila) &&
        fila$n_faltantes[[1L]] > 0L) {
      nombre_marca <- paste0(".ausente_", .nombres_make_names(columna))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_filas_ausentes", TRUE,
        paste0(
          "La marca conserva los registros y permite revisar los ausentes ",
          "antes de decidir si corresponde excluirlos."
        ), fila$n_faltantes[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        # Despues de las conversiones que CREAN ausentes -textuales en 100,
        # centinelas numericos en 110-. Marcar antes producia una columna que
        # afirma algo falso: medido sobre siete formas de tabla, en tres de
        # ellas la marca decia FALSE en filas que quedaban NA, y las dos
        # acciones se registraban como ejecutadas sin error. Con el idioma
        # documentado -`plan$aplicar <- plan$recomendada`- alcanzaba para que
        # `.ausente_t` negara dos de los tres ausentes.
        parametros = list(columna_marca = nombre_marca), orden = 115L,
        grupo = grupo_hallazgo, decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_filas_ausentes", FALSE,
        paste0(
          "Eliminar registros puede excluir personas o hechos del an\u00e1lisis; ",
          "s\u00f3lo corresponde cuando el dominio confirma que el ausente invalida la fila."
        ), fila$n_faltantes[[1L]], FALSE, estado = estado_columna,
        # Por la misma razon que la marca: eliminar antes de normalizar deja
        # sin eliminar las filas cuyo ausente todavia estaba disfrazado.
        aplicar = FALSE, orden = 120L, grupo = grupo_hallazgo,
        decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes",
        destructiva = TRUE
      ))
    } else if (identical(tipo, "faltantes_disfrazados") && !is.null(fila)) {
      n_textuales <- fila$n_faltantes_disfrazados_textuales[[1L]]
      n_numericos <- fila$n_faltantes_disfrazados_numericos[[1L]]
      if (n_textuales > 0L) {
        # La frase decia "pueden normalizarse SIN INFERIR EL DOMINIO", y eso
        # es falso justo donde importa: decidir que `NA` es una ausencia y no
        # el codigo de Namibia ES inferir el dominio. El catalogo reconoce el
        # token, no el significado que tiene en esta columna, y el cambio no
        # se puede deshacer. Se dice lo que el paquete sabe y se nombra la
        # comprobacion que solo puede hacer quien conoce los datos.
        justificacion <- paste0(
          "Las representaciones textuales del cat\u00e1logo son marcadores ",
          "habituales de ausencia. El cambio no es reversible: confirmar que ",
          "ninguno de esos textos sea un valor leg\u00edtimo de la columna ",
          "-`NA` es el c\u00f3digo de Namibia, `NULL` puede ser un apellido-."
        )
        declarados <- perfil$meta$cadenas_ausencia
        leidos <- .marcadores_de_ausencia(
          if (!is.null(datos) && columna %in% names(datos)) datos[[columna]] else NULL,
          hallazgo$evidencia[[1L]], n_textuales, declarados
        )
        ambiguos <- .marcadores_ambiguos(leidos$marcadores, declarados)
        # Sin los datos y con la evidencia recortada, no se puede saber si hay
        # un marcador ambiguo fuera de la vista: se pide confirmar.
        confirmar <- length(ambiguos) > 0L || !isTRUE(leidos$completa)
        if (confirmar) {
          justificacion <- if (length(ambiguos)) {
            paste0(
              "Uno de los textos detectados (", paste(ambiguos, collapse = ", "),
              ") podr\u00eda ser un valor leg\u00edtimo de la columna: `NA` es el ",
              "c\u00f3digo de Namibia, `NULL` puede ser un apellido. El cambio no ",
              "es reversible y el paquete no puede decidirlo, as\u00ed que la ",
              "acci\u00f3n queda para activar a mano."
            )
          } else {
            paste0(
              "La evidencia muestra s\u00f3lo los marcadores m\u00e1s frecuentes y ",
              "no se recibieron los datos para ver el resto: puede haber uno que ",
              "sea un valor leg\u00edtimo de la columna. El cambio no es ",
              "reversible, as\u00ed que la acci\u00f3n queda para activar a mano."
            )
          }
        }
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_ausencias_textuales", !confirmar,
          justificacion, n_textuales, FALSE,
          estado = estado_columna,
          # `aplicar` se calcula aparte de `recomendada`, asi que no alcanza
          # con no recomendarla: sin esto la accion se autoaplicaba igual.
          aplicar = !confirmar && identical(estado_columna, "lista"),
          # La documentacion del paquete dice que una accion destructiva
          # requiere que el usuario la active explicitamente. Esto es
          # exactamente eso.
          destructiva = confirmar,
          # La lista acota a lo OBSERVADO, no al catalogo. Con el catalogo
          # completo, un plan hecho sobre una entrega y aplicado a otra
          # convertia marcadores que nunca aparecieron -un `NULL` que puede ser
          # un apellido- sin pasar por la guarda de ambiguedad, que corre al
          # planificar. Lo declarado se suma porque el usuario lo afirmo.
          parametros = list(
            valores = unique(c(leidos$marcadores,
                               tolower(trimws(as.character(declarados))))),
            cadenas_ausencia = declarados
          ), orden = 100L
        ))
      }
      if (n_numericos > 0L) {
        sentinelas <- perfil$meta$sentinelas_numericos
        if (is.null(sentinelas)) sentinelas <- .numeros_na()
        justificacion <- paste0(
          "Un sentinela num\u00e9rico tambi\u00e9n puede ser un valor leg\u00edtimo; ",
          "requiere confirmar el diccionario del campo."
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_sentinelas_numericos", FALSE,
          justificacion, n_numericos, FALSE,
          estado = estado_columna, aplicar = FALSE,
          parametros = list(valores = sentinelas), orden = 110L,
          grupo = grupo_hallazgo, decision_grupo = "pendiente"
        ))
      }
    } else if (identical(tipo, "espacios_sobrantes") && !is.null(fila)) {
      justificacion <- paste0(
        "Los espacios al borde no aportan contenido y separan categor\u00edas ",
        "que visualmente son iguales."
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "recortar_espacios", TRUE, justificacion,
        fila$n_espacios_borde[[1L]], FALSE, estado = estado_columna,
        aplicar = identical(estado_columna, "lista"), orden = 200L
      ))
    } else if (identical(tipo, "controles_invisibles") && !is.null(fila)) {
      n_eliminables <- if ("n_invisibles_eliminables" %in% names(fila)) {
        fila$n_invisibles_eliminables[[1L]]
      } else fila$n_controles_invisibles[[1L]]
      n_espacios <- if ("n_espacios_invisibles" %in% names(fila)) {
        fila$n_espacios_invisibles[[1L]]
      } else 0L
      if (isTRUE(n_eliminables > 0L)) {
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "eliminar_controles_invisibles", TRUE,
          paste0(
            "Los controles C0/C1 y los invisibles Unicode de transporte no ",
            "aportan contenido de negocio y pueden romper cruces, ",
            "comparaciones y exportes. Los ZWJ/ZWNJ se conservan. Quitar el ",
            "car\u00e1cter no se puede deshacer: si al quitarlo dos valores que ",
            "eran distintos quedan iguales, el registro cuenta esas celdas en ",
            "`n_no_reversibles`."
          ), n_eliminables, FALSE,
          estado = estado_columna,
          aplicar = identical(estado_columna, "lista"), orden = 195L
        ))
      }
      if (isTRUE(n_espacios > 0L)) {
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "normalizar_espacios_invisibles", FALSE,
          paste0(
            "Los espacios Unicode se convierten a un espacio comun para ",
            "comparar y exportar; se pierde la distincion del espacio original, ",
            "por lo que la accion es destructiva y requiere confirmacion."
          ), n_espacios, FALSE,
          estado = estado_columna, aplicar = FALSE, orden = 206L,
          destructiva = TRUE
        ))
      }
    } else if (identical(tipo, "entidades_html") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "decodificar_entidades_html", FALSE,
        paste0(
          "Decodificar una entidad puede ser correcto para una fuente web, ",
          "pero un ampersand tambi\u00e9n puede ser contenido leg\u00edtimo."
        ), fila$n_entidades_html[[1L]], FALSE,
        estado = estado_columna, aplicar = FALSE, orden = 207L
      ))
    } else if (identical(tipo, "separadores_en_campo") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "reemplazar_separadores", FALSE,
        paste0(
          "Un salto dentro de un campo puede romper un CSV, pero tambi\u00e9n ",
          "puede ser parte leg\u00edtima de una observaci\u00f3n."
        ), fila$n_separadores_en_campo[[1L]], FALSE,
        estado = estado_columna, aplicar = FALSE, orden = 209L
      ))
    } else if (identical(tipo, "codificacion_rota") && !is.null(fila)) {
      reparable <- fila$n_codificacion_reparable[[1L]] > 0L
      parcial <- isTRUE(fila$estado_codificacion_reparacion[[1L]] ==
        "reparado_parcialmente") ||
        ("n_codificacion_reparable_parcialmente" %in% names(fila) &&
          fila$n_codificacion_reparable_parcialmente[[1L]] > 0L)
      estado_reparacion <- if ("estado_codificacion_reparacion" %in% names(fila)) {
        as.character(fila$estado_codificacion_reparacion[[1L]])
      } else if (reparable) "reparado" else "no_se_pudo"
      puede_aplicar <- reparable && !parcial && identical(estado_columna, "lista")
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, if (reparable) {
          "reparar_codificacion"
        } else {
          "recuperar_codificacion_en_origen"
        }, reparable,
        if (reparable) {
          paste0(
            "Prueba las codificaciones conocidas y se detiene cuando el texto ",
            "deja de parecer mojibake. Los estados parciales no se aplican solos."
          )
        } else {
          paste0(
            "El car\u00e1cter de reemplazo indica que se perdieron bytes; ninguna ",
            "transformaci\u00f3n local puede recuperar el contenido original."
          )
        },
        if (reparable) fila$n_codificacion_reparable[[1L]] else {
          fila$n_codificacion_irreparable[[1L]]
        }, FALSE,
        estado = if (puede_aplicar) estado_columna else "informativa",
        aplicar = puede_aplicar,
        parametros = list(codificacion_intermedia = "ftfy",
                          codificaciones = names(.ftfy_tablas_bytes),
                          max_iteraciones = 20L),
        orden = 180L, estado_reparacion = estado_reparacion
      ))
    } else if (identical(tipo, "numero_como_texto") && !is.null(fila)) {
      seguro <- isTRUE(fila$numero_texto_seguro[[1L]])
      parametros_numero <- list(
        convencion = fila$numero_texto_convencion[[1L]],
        moneda = fila$numero_texto_moneda[[1L]],
        unidad = fila$numero_texto_unidad[[1L]],
        punto_sin_coma = NA_character_, coma_sin_punto = NA_character_
      )
      comprobacion_numero <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_numero_regional", parametros_numero
      )
      conversion_numero_segura <- seguro &&
        isTRUE(comprobacion_numero$reversible)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_numero_regional", conversion_numero_segura,
        if (conversion_numero_segura) {
          paste0(
            "La columna usa una convenci\u00f3n decimal coherente (",
            fila$numero_texto_convencion[[1L]],
            ") y puede convertirse sin elegir entre interpretaciones.",
            .nota_unidad_numero(parametros_numero)
          )
        } else if (seguro) {
          comprobacion_numero$justificacion
        } else {
          .motivo_numero_no_seguro(fila)
        },
        # Cuenta valores presentes, como las otras dos conversiones de tipo:
        # heredaba `n_numeros_texto`, que solo cuenta los que tienen
        # separadores, y el plan prometia 11 donde la conversion cambiaba los
        # 20 -una columna cambia de tipo entera, tambien el "7"-.
        fila$n[[1L]] - fila$n_faltantes[[1L]], comprobacion_numero$reversible,
        estado = if (seguro && comprobacion_numero$verificable) {
          estado_columna
        } else if (seguro) "bloqueada" else estado_columna,
        aplicar = conversion_numero_segura &&
          identical(estado_columna, "lista"),
        parametros = c(parametros_numero, list(
          reversibilidad_comprobada = comprobacion_numero$verificable,
          n_no_reversibles = comprobacion_numero$n_no_reversibles,
          motivo_no_reversible = comprobacion_numero$justificacion
        )),
        unidad_conteo = "valor",
        destructiva = (seguro && !conversion_numero_segura) ||
          .perdida_medida(comprobacion_numero),
        orden = 320L
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
      parametros_fecha <- list(formatos = confirmados, tipo = destino)
      comprobacion_fecha <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_fecha_confirmada", parametros_fecha
      )
      conversion_fecha_segura <- seguro &&
        isTRUE(comprobacion_fecha$reversible)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_fecha_confirmada", conversion_fecha_segura,
        if (conversion_fecha_segura) justificacion else {
          comprobacion_fecha$justificacion
        }, fila$n[[1L]] - fila$n_faltantes[[1L]],
        comprobacion_fecha$reversible,
        estado = if (conversion_fecha_segura) estado else "bloqueada",
        aplicar = conversion_fecha_segura && identical(estado, "lista"),
        parametros = c(parametros_fecha, list(
          reversibilidad_comprobada = comprobacion_fecha$verificable,
          n_no_reversibles = comprobacion_fecha$n_no_reversibles,
          motivo_no_reversible = comprobacion_fecha$justificacion
        )),
        # Cuenta valores presentes, no columnas ni filas: se declara aca en
        # vez de heredar la unidad del hallazgo, que habla de otra cosa.
        unidad_conteo = "valor",
        destructiva = (seguro && !conversion_fecha_segura) ||
          .perdida_medida(comprobacion_fecha),
        orden = 300L
      ))
    } else if (identical(tipo, "tipo_declarado_distinto") && !is.null(fila) &&
               !.es_fecha_ambigua(perfil, columna) &&
               !.es_fecha_mixta(perfil, columna) &&
                 !any(
                 .nombres_para_operar(perfil$hallazgos$columna) %in%
                   .nombres_para_operar(columna) &
                   perfil$hallazgos$tipo_hallazgo == "numero_como_texto",
                 na.rm = TRUE
               )) {
      destino <- fila$tipo_inferido[[1L]]
      soportado <- destino %in% c("entero", "doble", "logico", "fecha", "fecha-hora")
      compatible <- isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1)
      formatos <- .formatos_perfil(perfil, columna)
      confirmados <- if (is.null(formatos)) character() else {
        formatos$formato[formatos$estado == "confirmado"]
      }
      fecha_segura <- !destino %in% c("fecha", "fecha-hora") ||
        (length(confirmados) > 0L && !any(formatos$estado != "confirmado"))
      parametros_tipo <- list(tipo = destino, formatos = confirmados)
      comprobacion_tipo <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_tipo", parametros_tipo
      )
      base_tipo_seguro <- soportado && compatible && fecha_segura &&
        !.accion_columna_ambigua(perfil, columna)
      recomendar <- base_tipo_seguro &&
        isTRUE(comprobacion_tipo$reversible)
      estado <- if (recomendar) estado_columna else "bloqueada"
      justificacion <- if (recomendar) {
        paste0(
          "Todos los valores presentes son compatibles con el tipo inferido y ",
          "la conversi\u00f3n es ejecutable e inyectiva."
        )
      } else if (base_tipo_seguro) {
        comprobacion_tipo$justificacion
      } else {
        paste0(
          "La conversi\u00f3n requiere compatibilidad total, un tipo con conversi\u00f3n ",
          "definida y, para fechas, un formato confirmado."
        )
      }
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_tipo", recomendar, justificacion,
        fila$n[[1L]] - fila$n_faltantes[[1L]], comprobacion_tipo$reversible,
        estado = estado, aplicar = recomendar,
        parametros = c(parametros_tipo, list(
          reversibilidad_comprobada = comprobacion_tipo$verificable,
          n_no_reversibles = comprobacion_tipo$n_no_reversibles,
          motivo_no_reversible = comprobacion_tipo$justificacion
        )), orden = 310L, unidad_conteo = "valor",
        grupo = grupo_hallazgo,
        decision_grupo = if (recomendar) "recomendada" else "pendiente",
        recomendacion_grupo = if (recomendar) "convertir_tipo" else NA_character_,
        destructiva = (base_tipo_seguro && !recomendar) ||
          .perdida_medida(comprobacion_tipo)
      ))
    } else if (identical(tipo, "filas_duplicadas")) {
      n_participantes <- perfil$general$filas_en_grupos_duplicados
      if (is.null(n_participantes)) {
        n_participantes <- perfil$general$filas_duplicadas
      }
      # La deteccion cuenta duplicados con `duplicated.data.frame()`, que
      # tolera columnas-lista, pero el ejecutor agrupa por los codigos de
      # `factor()`, que no las admite, y aborta. El plan recomendaba y activaba
      # una accion que sobre esa tabla no podia ejecutarse nunca: la registraba
      # `fallida` en cada corrida. Se bloquea y se dice por que, que es lo que
      # el paquete ya hace con las conversiones que no puede comprobar.
      con_lista <- inherits(perfil$columnas, "data.frame") &&
        "tipo_declarado" %in% names(perfil$columnas) &&
        any(as.character(perfil$columnas$tipo_declarado) == "lista")
      motivo_lista <- paste0(
        "La tabla tiene columnas de lista y el agrupamiento de duplicados no ",
        "puede compararlas; la acci\u00f3n no se puede ejecutar sobre estos datos. ",
        "Convertir o quitar las columnas de lista para agrupar las filas."
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "marcar_filas_duplicadas", !con_lista,
        if (con_lista) motivo_lista else paste0(
          "Marcar conserva todas las filas, identifica las repeticiones y ",
          "asigna un grupo a todos los registros que participan. No elimina ",
          "filas; mientras las marcas esten incluidas, un perfil posterior no ",
          "vuelve a contar esas filas como duplicadas exactas."
        ), n_participantes, TRUE,
        estado = if (con_lista) "bloqueada" else "lista", aplicar = !con_lista,
        parametros = list(
          columna_marca = ".fila_duplicada",
          columna_grupo = ".grupo_duplicado"
        ), orden = 30L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "conservar_primera_duplicada", FALSE,
        if (con_lista) motivo_lista else paste0(
          "Conserva la primera aparici\u00f3n exacta y elimina las siguientes; ",
          "el orden de entrada pasa a determinar qu\u00e9 registro sobrevive."
        ), perfil$general$filas_duplicadas, FALSE,
        estado = if (con_lista) "bloqueada" else "lista",
        aplicar = FALSE, orden = 35L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas",
        destructiva = TRUE
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "conservar_mas_completa", FALSE,
        paste0(
          "Requiere configurar una clave: entre duplicados exactos todas las ",
          "filas tienen la misma completitud y esta opci\u00f3n ser\u00eda equivalente a la primera."
        ), perfil$general$filas_duplicadas, FALSE, estado = "bloqueada",
        aplicar = FALSE, parametros = list(clave = character()), orden = 36L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas",
        destructiva = TRUE
      ))
    } else if (identical(tipo, "outliers") && !is.null(fila)) {
      winsor_disponible <- fila$tipo_inferido[[1L]] %in% c("entero", "doble") &&
        isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_outliers", TRUE,
        paste0(
          "Un valor extremo puede ser correcto; la marca conserva el dato ",
          "para que el dominio decida c\u00f3mo tratarlo."
        ), fila$n_outliers[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        parametros = list(
          columna_marca = paste0(".outlier_", .nombres_make_names(columna)),
          regla = "Tukey 1,5 x IQR"
        ), orden = 510L, grupo = grupo_hallazgo,
        decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_outliers"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "winsorizar_outliers", FALSE,
        paste0(
          "Sustituye los extremos por los l\u00edmites de Tukey y altera valores ",
          "observados; s\u00f3lo debe elegirse con justificaci\u00f3n anal\u00edtica."
        ), fila$n_outliers[[1L]], FALSE,
        estado = if (winsor_disponible) estado_columna else "bloqueada",
        aplicar = FALSE,
        parametros = list(regla = "Tukey 1,5 x IQR"), orden = 520L,
        grupo = grupo_hallazgo, decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_outliers"
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
          nombres_propuestos = .nombres_make_names(nombres)
        ), orden = 900L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "normalizar_nombres"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "normalizar_nombres_snake_case", FALSE,
        paste0(
          "snake_case es legible y estable, pero cambia may\u00fasculas y signos; ",
          "se ofrece como alternativa expl\u00edcita a make.names()."
        ), nrow(problema), FALSE, estado = "lista", aplicar = FALSE,
        parametros = list(
          nombres_esperados = nombres,
          nombres_propuestos = .nombres_snake(nombres)
        ), orden = 900L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "normalizar_nombres"
      ))
    } else if (identical(tipo, "mayusculas_inconsistentes") && !is.null(fila)) {
      opciones <- list(
        list(
          estrategia = "convertir_minusculas",
          justificacion = "Unifica la columna en min\u00fasculas; puede alterar nombres propios."
        ),
        list(
          estrategia = "convertir_titulo",
          justificacion = paste0(
            "Capitaliza cada palabra; no conoce excepciones ling\u00fc\u00edsticas ni ",
            "convenciones de nombres propios."
          )
        ),
        list(
          estrategia = "convertir_mayusculas",
          justificacion = "Unifica la columna en may\u00fasculas; puede perder matices del texto."
        ),
        list(
          estrategia = "convertir_segun_diccionario",
          justificacion = paste0(
            "Aplica un vector con nombres donde cada nombre es el valor original ",
            "y su contenido es el valor normalizado."
          ),
          parametros = list(diccionario = NULL)
        )
      )
      for (opcion in opciones) {
        requiere_diccionario <- identical(
          opcion$estrategia, "convertir_segun_diccionario"
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, opcion$estrategia, FALSE, opcion$justificacion,
          fila$n_variantes_mayusculas[[1L]], FALSE,
          estado = if (requiere_diccionario) "bloqueada" else estado_columna,
          parametros = if (is.null(opcion$parametros)) list() else opcion$parametros,
          orden = 600L, grupo = grupo_hallazgo,
          decision_grupo = "pendiente"
        ))
      }
    } else if (identical(tipo, "constante") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_columna_constante", FALSE,
        paste0(
          "Eliminarla pierde contexto potencial; dejarla es la recomendaci\u00f3n ",
          "hasta confirmar que no aporta significado administrativo."
        ), 1, FALSE, estado = estado_columna, aplicar = FALSE,
        orden = 710L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "no_hacer_nada", destructiva = TRUE
      ))
    } else if (identical(tipo, "columnas_duplicadas")) {
      par <- .par_columnas_duplicadas(perfil, hallazgo)
      estado_par <- if (
        is.null(par) || any(vapply(par, function(nombre) {
          sum(.nombres_para_operar(perfil$columnas$columna) %in%
                .nombres_para_operar(nombre)) != 1L
        }, logical(1L)))
      ) "bloqueada" else "lista"
      parametros_par <- if (is.null(par)) list() else list(
        columna_1 = par[[1L]], columna_2 = par[[2L]],
        eliminar = par[[2L]]
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_columnas_duplicadas", TRUE,
        paste0(
          "La anotaci\u00f3n conserva ambas columnas y registra expl\u00edcitamente la redundancia."
        ), 1, TRUE, estado = estado_par,
        aplicar = identical(estado_par, "lista"),
        parametros = parametros_par, orden = 700L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_columnas_duplicadas"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_columna_duplicada", FALSE,
        paste0(
          "Eliminar una columna puede romper consumidores que dependan de su ",
          "nombre aunque el contenido sea redundante."
        ), 1, FALSE, estado = estado_par, aplicar = FALSE,
        parametros = parametros_par, orden = 705L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_columnas_duplicadas",
        destructiva = TRUE
      ))
    } else if (tipo %in% c(
      "alta_cardinalidad", "ceros_no_permitidos", "negativos_no_permitidos"
    )) {
      estrategias <- c(
        alta_cardinalidad = "revisar_cardinalidad",
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

  dependencias <- perfil$dependencias
  if (!is.null(datos) && inherits(dependencias, "data.frame") &&
      nrow(dependencias)) {
    if (!identical(
      .nombres_para_operar(names(datos)),
      .nombres_para_operar(perfil$columnas$columna)
    )) {
      stop("Los nombres de `datos` no coinciden con los usados por el perfil.",
           call. = FALSE)
    }
    exactas <- dependencias[dependencias$exacta, , drop = FALSE]
    candidatas <- list()
    for (i in seq_len(nrow(exactas))) {
      determinante <- exactas$determinante[[i]]
      dependiente <- exactas$dependiente[[i]]
      indices_dependencia <- .indice_nombre(
        c(determinante, dependiente), names(datos)
      )
      if (anyNA(indices_dependencia)) next
      determinante <- names(datos)[indices_dependencia[[1L]]]
      dependiente <- names(datos)[indices_dependencia[[2L]]]
      mapa <- .mapa_dependencia(
        datos, determinante, dependiente,
        soporte_minimo = soporte_minimo_dependencia
      )
      if (!nrow(mapa)) next
      indices <- match(
        .valores_relacion(datos[[determinante]]),
        .valores_relacion(mapa$determinante)
      )
      imputables <- is.na(datos[[dependiente]]) & !is.na(indices)
      if (!any(imputables)) next
      candidatas[[length(candidatas) + 1L]] <- list(
        determinante = determinante, dependiente = dependiente,
        mapa = mapa, n = sum(imputables), soporte = exactas$n_evaluados[[i]]
      )
    }
    if (length(candidatas)) {
      por_dependiente <- split(
        seq_along(candidatas),
        .nombres_para_operar(vapply(
          candidatas, `[[`, character(1L), "dependiente"
        ))
      )
      numero_grupo <- nrow(hallazgos)
      for (indices in por_dependiente) {
        numero_grupo <- numero_grupo + 1L
        grupo <- .id_grupo(numero_grupo)
        for (indice in indices) {
          candidata <- candidatas[[indice]]
          estrategia_imputacion <- paste0(
            "imputar_dependencia_funcional__",
            .nombres_make_names(candidata$determinante)
          )
          acciones <- .agregar_accion(acciones, .nueva_accion(
            candidata$dependiente, "faltantes",
            estrategia_imputacion, FALSE,
            paste0(
              "La relaci\u00f3n ", candidata$determinante, " -> ",
              candidata$dependiente, " es exacta en ", candidata$soporte,
              " filas presentes y cada valor usado tiene al menos ",
              soporte_minimo_dependencia, " observaciones de soporte. ",
              "Debe confirmarse como regla antes de imputar."
            ),
            candidata$n, FALSE, estado = "lista", aplicar = FALSE,
            parametros = list(
              determinante = candidata$determinante,
              dependiente = candidata$dependiente,
              mapa = candidata$mapa,
              soporte_minimo = soporte_minimo_dependencia,
              cumplimiento = 1
            ),
            orden = 150L, grupo = grupo,
            decision_grupo = "pendiente",
            recomendacion_grupo = NA_character_
          ))
        }
      }
    }
  }

  if (!length(acciones)) {
    resultado <- .plan_vacio()
  } else {
    resultado <- do.call(rbind, acciones)
    rownames(resultado) <- NULL
    resultado$id_accion <- sprintf("accion-%04d", seq_len(nrow(resultado)))
    for (j in seq_len(nrow(resultado))) {
      indice_hallazgo <- if (!is.na(resultado$grupo[[j]])) {
        suppressWarnings(as.integer(sub("^grupo-", "", resultado$grupo[[j]])))
      } else {
        candidatos <- which(
          hallazgos$tipo_hallazgo == resultado$hallazgo[[j]] &
            ((is.na(hallazgos$columna) & is.na(resultado$columna[[j]])) |
               .nombres_para_operar(hallazgos$columna) %in%
                 .nombres_para_operar(resultado$columna[[j]]))
        )
        if (length(candidatos)) candidatos[[1L]] else NA_integer_
      }
      if (!is.na(indice_hallazgo) && indice_hallazgo <= nrow(hallazgos)) {
        resultado$evidencia[[j]] <- hallazgos$evidencia[[indice_hallazgo]]
        resultado$severidad_origen[[j]] <- as.character(
          hallazgos$severidad[[indice_hallazgo]]
        )
        # Solo se hereda la unidad del hallazgo si la accion NO declaro la
        # suya. Antes se pisaba siempre, y una accion que cuenta en otra unidad
        # quedaba con el par mal: `convertir_tipo` declaraba `n_afectadas = 5`
        # -los valores presentes- con `unidad_conteo = "columna"` heredado del
        # hallazgo, sobre una tabla de UNA columna. Ese par no es cierto en
        # ninguna lectura, y `guiar_limpieza()` lo repetia en pantalla.
        if (is.na(resultado$unidad_conteo[[j]])) {
          resultado$unidad_conteo[[j]] <- as.character(
            hallazgos$unidad_conteo[[indice_hallazgo]]
          )
        }
      } else if (startsWith(resultado$estrategia[[j]],
                            "imputar_dependencia_funcional__")) {
        resultado$unidad_conteo[[j]] <- "fila"
      }
    }
  }
  resultado <- .declarar_texto_en_columna_factor(resultado, perfil)
  resultado$estado <- factor(
    resultado$estado,
    levels = c("lista", "bloqueada", "informativa")
  )
  resultado$decision_grupo <- factor(
    resultado$decision_grupo,
    levels = c(
      "pendiente", "recomendada", "desactivada", "elegida", "omitida"
    )
  )
  resultado <- .aplicabilidad_en_plan(resultado, perfil)
  class(resultado) <- c("plan_limpieza", "data.frame")
  # El plan se arma desde `perfil$hallazgos`, asi que por construccion no puede
  # tener una accion para un diagnostico que no se evaluo. Quien trabaja desde
  # el plan no tenia forma de enterarse de que uno se declino: leia tres
  # acciones y concluia que lo demas estaba bien, cuando lo que habia pasado es
  # que sobre esa columna no se miro. La cobertura viaja con el plan para que
  # ese silencio sea visible desde la misma puerta donde se decide que limpiar.
  attr(resultado, "cobertura_diagnosticos") <- if (
    inherits(perfil$cobertura_diagnosticos, "data.frame")
  ) {
    perfil$cobertura_diagnosticos
  } else {
    .cobertura_diagnosticos_vacia()
  }
  attr(resultado, "hallazgos_sin_accion_por_columna_ambigua") <-
    .hallazgos_sin_accion_por_columna_ambigua(perfil)
  .proteger_plan_limpieza(resultado, perfil, datos)
}

.columnas_plan <- function() {
  c(
    "id_accion", "columna", "hallazgo", "grupo", "decision_grupo",
    "recomendacion_grupo", "estrategia", "recomendada",
    "severidad_origen", "evidencia", "justificacion", "n_afectadas",
    "unidad_conteo",
    "reversible", "destructiva", "estado", "aplicar", "orden", "parametros"
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
  if (!is.character(plan$id_accion) || anyNA(plan$id_accion) ||
      length(plan$id_accion) != nrow(plan)) {
    # Se comprobaba que fueran unicos y no que fueran texto: vaciando la
    # columna, el usuario recibia "los argumentos implican un numero diferente
    # de filas: 0, 1" -un error interno de R al armar el registro- en vez del
    # motivo del plan que el paquete promete.
    stop(
      "`plan$id_accion` debe ser un vector de texto sin NA, con un ",
      "identificador por fila.",
      call. = FALSE
    )
  }
  if (anyDuplicated(plan$id_accion)) {
    stop("`plan$id_accion` debe contener identificadores \u00fanicos.", call. = FALSE)
  }
  if (!is.list(plan$parametros)) {
    stop("`plan$parametros` debe ser una columna de listas.", call. = FALSE)
  }
  if (!is.character(plan$unidad_conteo)) {
    stop("`plan$unidad_conteo` debe ser texto.", call. = FALSE)
  }
  if (!is.logical(plan$recomendada) || anyNA(plan$recomendada) ||
      !is.logical(plan$reversible)) {
    stop(
      "`recomendada` debe ser l\u00f3gica sin NA y `reversible` debe ser l\u00f3gica.",
      call. = FALSE
    )
  }
  if (!is.character(plan$grupo) || !is.logical(plan$destructiva) ||
      anyNA(plan$destructiva)) {
    stop("`grupo` debe ser texto y `destructiva` un l\u00f3gico sin NA.", call. = FALSE)
  }
  if (any(plan$destructiva & plan$recomendada, na.rm = TRUE)) {
    stop("Una acci\u00f3n destructiva nunca puede ser recomendada.", call. = FALSE)
  }
  if (any(plan$destructiva & (is.na(plan$reversible) | plan$reversible))) {
    stop("Toda acci\u00f3n destructiva debe declarar `reversible = FALSE`.", call. = FALSE)
  }
  estados <- as.character(plan$estado)
  if (anyNA(estados) || any(!estados %in% c("lista", "bloqueada", "informativa"))) {
    stop("`estado` contiene un valor no reconocido.", call. = FALSE)
  }
  if (any(plan$aplicar & estados != "lista")) {
    invalidas <- which(plan$aplicar & estados != "lista")
    detalle <- paste(vapply(invalidas, function(i) {
      paste0("fila ", i, " (columna '", as.character(plan$columna[[i]]),
             "', estrategia '", as.character(plan$estrategia[[i]]),
             "', estado '", estados[[i]], "')")
    }, character(1L)), collapse = "; ")
    stop(
      "S\u00f3lo se pueden aplicar acciones con estado 'lista'. ",
      "Acciones no listas: ", detalle, ".",
      call. = FALSE
    )
  }
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    activas <- indices[plan$aplicar[indices]]
    if (length(activas) > 1L) {
      stop(
        "El grupo '", grupo, "' tiene acciones incompatibles activas: ",
        paste(plan$estrategia[activas], collapse = ", "), ".",
        call. = FALSE
      )
    }
    decision <- unique(as.character(plan$decision_grupo[indices]))
    decision <- decision[!is.na(decision)]
    if (!length(activas) && identical(decision, "elegida")) {
      # `decision_grupo` es la distincion entre una eleccion y una omision. Un
      # grupo marcado como elegido y sin ninguna accion activa no es ninguna de
      # las dos: es una edicion contradictoria, y el paquete ya rechaza las
      # otras dos de esta familia en vez de adivinar cual gana.
      stop(
        "El grupo '", grupo, "' esta marcado como elegido y no tiene ninguna ",
        "accion activa. Active la accion elegida o cambie la decision.",
        call. = FALSE
      )
    }
    if (anyDuplicated(plan$estrategia[indices])) {
      stop("El grupo '", grupo, "' repite una estrategia.", call. = FALSE)
    }
    decisiones <- unique(as.character(plan$decision_grupo[indices]))
    if (length(decisiones) != 1L || is.na(decisiones)) {
      stop("El grupo '", grupo, "' debe compartir una sola decisi\u00f3n.", call. = FALSE)
    }
    recomendaciones <- unique(plan$recomendacion_grupo[indices])
    recomendaciones <- recomendaciones[!is.na(recomendaciones)]
    if (length(recomendaciones) > 1L) {
      stop("El grupo '", grupo, "' declara recomendaciones incompatibles.", call. = FALSE)
    }
  }
  invisible(plan)
}

.sincronizar_decisiones <- function(plan) {
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    activas <- indices[plan$aplicar[indices]]
    recomendacion <- unique(plan$recomendacion_grupo[indices])
    recomendacion <- recomendacion[!is.na(recomendacion)]
    if (length(activas) == 1L &&
        (!length(recomendacion) ||
         plan$estrategia[[activas]] != recomendacion[[1L]])) {
      plan$decision_grupo[indices] <- "elegida"
    } else if (!length(activas) && length(recomendacion) &&
               recomendacion[[1L]] != "no_hacer_nada" &&
               as.character(plan$decision_grupo[[indices[[1L]]]]) == "recomendada") {
      plan$decision_grupo[indices] <- "desactivada"
    }
  }
  plan
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
  indices <- which(.nombres_para_operar(names(datos)) %in%
                   .nombres_para_operar(columna))
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
  # Se convierte EXACTAMENTE lo que el detector conto, y no lo que diga un
  # catalogo propio. Antes este ejecutor tenia su propia logica y se habia
  # separado de la deteccion por dos lados:
  #
  #   * el detector saca `sd`, `nc` y `nd` del catalogo cuando la columna tiene
  #     diez o mas valores distintos -para no confundirlos con codigos-, y el
  #     ejecutor usaba el catalogo COMPLETO. Sobre una columna de estados de
  #     EE.UU. el hallazgo declaraba 4 celdas y la accion borraba 7, entre
  #     ellas Dakota del Sur, Carolina del Norte y Dakota del Norte;
  #   * el detector normaliza con `.texto_analizable()`, que tolera valores
  #     marcados `bytes`, y el ejecutor hacia `tolower()` sobre la columna
  #     cruda, que ABORTA con uno solo: la accion se recomendaba y era
  #     imposible de ejecutar.
  #
  # Una regla escrita dos veces. Ahora hay una sola, la del detector, y la
  # lista del plan solo puede ACOTAR: quien edita el plan puede convertir
  # menos, nunca mas de lo que se detecto.
  deteccion <- .detectar_faltantes_disfrazados(
    x, detectar_sentinelas_numericos = FALSE,
    cadenas_ausencia = parametros$cadenas_ausencia
  )
  mascara <- deteccion$mascara_textual
  if (length(parametros$valores)) {
    normalizados <- tolower(trimws(.texto_analizable(x)$valores))
    mascara <- mascara & !is.na(normalizados) &
      normalizados %in% tolower(trimws(as.character(parametros$valores)))
  }
  if (is.factor(x)) x <- as.character(x)
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
  if (is.factor(x)) x <- as.character(x)
  x[mascara] <- NA
  list(valor = x, n = sum(mascara))
}

.imputar_dependencia <- function(datos, parametros) {
  determinante <- parametros$determinante
  dependiente <- parametros$dependiente
  indices_dependencia <- .indice_nombre(
    c(determinante, dependiente), names(datos)
  )
  if (anyNA(indices_dependencia)) {
    stop("La imputaci\u00f3n no conserva un contrato de dependencia v\u00e1lido.",
         call. = FALSE)
  }
  determinante <- names(datos)[indices_dependencia[[1L]]]
  dependiente <- names(datos)[indices_dependencia[[2L]]]
  mapa <- if (isTRUE(parametros$mapa_enmascarado)) {
    soporte <- parametros$soporte_minimo
    if (length(soporte) != 1L || is.na(soporte) ||
        !is.finite(soporte) || soporte < 1L) {
      stop("La imputaci\u00f3n no conserva un soporte de dependencia v\u00e1lido.",
           call. = FALSE)
    }
    .mapa_dependencia(
      datos, determinante, dependiente, soporte_minimo = soporte
    )
  } else {
    parametros$mapa
  }
  if (!inherits(mapa, "data.frame") ||
      !all(c("determinante", "dependiente") %in% names(mapa))) {
    stop("La imputaci\u00f3n no conserva un contrato de dependencia v\u00e1lido.",
         call. = FALSE)
  }
  if (isTRUE(parametros$mapa_enmascarado) && !nrow(mapa)) {
    stop(
      "La dependencia funcional del plan no se puede resolver sobre los datos recibidos.",
      call. = FALSE
    )
  }
  indices <- match(
    .valores_relacion(datos[[determinante]]),
    .valores_relacion(mapa$determinante)
  )
  conocidos <- !is.na(datos[[dependiente]]) & !is.na(indices)
  esperado <- mapa$dependiente[indices[conocidos]]
  if (any(.valores_relacion(datos[[dependiente]][conocidos]) !=
          .valores_relacion(esperado))) {
    stop("Los datos actuales contradicen la dependencia funcional del plan.",
         call. = FALSE)
  }
  imputar <- is.na(datos[[dependiente]]) & !is.na(indices)
  salida <- datos[[dependiente]]
  if (is.factor(salida)) {
    texto <- as.character(salida)
    texto[imputar] <- as.character(mapa$dependiente[indices[imputar]])
    salida <- texto
  } else {
    salida[imputar] <- mapa$dependiente[indices[imputar]]
  }
  datos[[dependiente]] <- salida
  list(datos = datos, n = sum(imputar))
}

# Cuenta las celdas que REALMENTE cambiaron, comparando bytes.
#
# `anterior != nuevo` compara TEXTO, y el texto distingue la marca de
# codificacion: una celda declarada `bytes` que la accion no toco quedaba
# contada como cambiada porque su marca difiere de la del resultado, aunque los
# bytes fueran identicos. Medido: `eliminar_controles_invisibles` y
# `normalizar_espacios_invisibles` informaban `n_cambiadas = 2` donde el plan
# habia anunciado 1, sobre un dato donde solo una celda cambio.
#
# Que el paquete diga haber cambiado una celda que no cambio es informar como
# hecho lo que no hizo, un piso mas arriba de lo que mide.
#
# Se confirma por bytes SOLO donde `!=` acuso, que es un conjunto chico: la
# comprobacion cara no corre sobre la columna entera.
.celdas_cambiadas <- function(anterior, nuevo) {
  anterior <- as.character(anterior)
  nuevo <- as.character(nuevo)
  cambio <- !is.na(anterior) & (is.na(nuevo) | anterior != nuevo)
  sospechosas <- which(cambio & !is.na(nuevo))
  if (length(sospechosas)) {
    iguales <- vapply(
      sospechosas,
      function(i) identical(charToRaw(anterior[[i]]), charToRaw(nuevo[[i]])),
      logical(1L)
    )
    cambio[sospechosas[iguales]] <- FALSE
  }
  cambio
}

.celdas_que_pierden_valor <- function(anterior, nuevo) {
  # Cuando una accion NORMALIZA la escritura -recortar espacios, quitar un
  # control invisible, unificar mayusculas-, el valor sigue en su lugar y la
  # documentacion la exime del conteo por nombre. Pero la misma accion pierde
  # informacion cuando **colapsa valores que eran distintos**: ahi lo que los
  # separaba no esta en ningun lado.
  #
  # Medido, las dos mitades: quitar un guion suave de `PRO<U+00AD>DUCTO-A` deja
  # las celdas identicas a su forma canonica y el cruce contra el catalogo
  # encuentra los cinco valores -no se perdio nada, y el registro contaba 4-; y
  # con claves marcadas por un espacio de ancho cero, la misma accion colapsa
  # cinco claves distintas en tres y fabrica dos duplicados que no existian
  # -ahi si se perdio, y contaba igual-. Una regla por accion no puede acertar
  # en los dos casos. Esta se mide sobre el resultado.
  #
  # `recortar_espacios` tenia el defecto simetrico: sobre `" ana "` y `"ana"`
  # los dos quedan `"ana"` -de cuatro distintos a tres- y el registro publicaba
  # cero irreversibles.
  cambiadas <- .celdas_cambiadas(anterior, nuevo)
  if (!any(cambiadas)) return(cambiadas)
  antes <- as.character(anterior)
  despues <- as.character(nuevo)
  # Volverse ausente es perder el valor aunque no colapse con nadie.
  perdidas <- cambiadas & is.na(despues) & !is.na(antes)
  # Solo pueden colapsar los valores nuevos que se REPITEN, asi que la particion
  # se limita a esos: sobre una columna de valores distintos -el caso comun- no
  # se arma ningun grupo. Partir la columna entera costaba por columna y por
  # accion, y este paquete ya se cuido de eso en el resumen cuantitativo.
  comparables <- !is.na(despues)
  repetidos <- comparables &
    (duplicated(despues) | duplicated(despues, fromLast = TRUE))
  candidatos <- which(repetidos)
  if (length(candidatos)) {
    grupos <- split(candidatos, despues[candidatos])
    for (grupo in grupos) {
      if (length(grupo) < 2L) next
      # Colapsan si los valores ORIGINALES del grupo no eran todos iguales.
      #
      # Se marca solo la celda que TENIA valor: si una celda ausente pasa a
      # valer lo mismo que otra -una imputacion-, no perdio nada, gano un
      # valor, y la accion que lo hizo ya declara lo suyo.
      if (length(unique(antes[grupo])) > 1L) {
        perdidas[grupo] <- perdidas[grupo] |
          (cambiadas[grupo] & !is.na(antes[grupo]))
      }
    }
  }
  perdidas
}

.recortar_texto <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("El recorte de espacios requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  # `trimws()` sobre un vector que contiene UNA cadena marcada `bytes` devuelve
  # marcadas `bytes` tambien a las `latin1` que viajaban al lado, y ahi su
  # contenido deja de ser recuperable: un `ca<f1>o ` valido volvia como
  # `ca<f1>o` declarado `bytes`, que ya no es UTF-8 valido. El destino de una
  # fila lo decidia lo que hubiera en el resto de la tanda. Se recorta cada
  # grupo por separado para que ninguna marca contamine a la vecina.
  nuevo <- .aplicar_por_marca(
    anterior, trimws,
    function(v) trimws(v, whitespace = "[ \t\r\n]")
  )
  mascara <- .celdas_cambiadas(anterior, nuevo)
  list(valor = nuevo, n = sum(mascara),
       n_no_reversibles = sum(.celdas_que_pierden_valor(anterior, nuevo)))
}

.quitar_controles_invisibles <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La eliminaci\u00f3n de controles invisibles requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  # Lo que no se puede decodificar se DEJA COMO ESTA. Antes se lo recorria
  # igual y `paste0(intToUtf8(NA), collapse = "")` devolvia la cadena `"NA"`,
  # que reemplazaba el valor del usuario: sobre una columna que mezcla `latin1`
  # con UTF-8, tres de siete valores se volvian `"NA"` al aplicar una accion
  # marcada como recomendada. No tocarlo tambien es una respuesta, y es la
  # unica honesta cuando no se puede leer el contenido.
  nuevo <- vapply(anterior, function(texto) {
    if (is.na(texto)) return(NA_character_)
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    conservar <- !.codigos_control_eliminable(codigos)
    # Si no habia nada que quitar, se devuelve el texto TAL CUAL. Rearmarlo
    # desde sus puntos de codigo lo reescribe en UTF-8 aunque no haya cambiado
    # nada: una celda `latin1` sin ningun control perdia su declaracion y sus
    # bytes (`f1` -> `c3 b1`) al aplicar una accion que no tenia nada que
    # hacer en ella. Y el registro no lo contaba, porque cuenta cambios de
    # texto y como texto era el mismo valor. Una accion de remediacion tiene
    # que ser minima: lo que no necesita tocar, no se toca.
    if (all(conservar)) return(texto)
    paste0(intToUtf8(codigos[conservar], multiple = TRUE), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
  cambio <- .celdas_cambiadas(anterior, nuevo)
  list(valor = .resultado_texto(x, nuevo), n = sum(cambio),
       n_no_reversibles = sum(.celdas_que_pierden_valor(anterior, nuevo)))
}

.normalizar_espacios_invisibles <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La normalizacion de espacios Unicode requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  # Mismo motivo que en `.quitar_controles_invisibles()`: lo que no se puede
  # decodificar se deja intacto en vez de reemplazarlo por la cadena `"NA"`.
  nuevo <- vapply(anterior, function(texto) {
    if (is.na(texto)) return(NA_character_)
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    afectados <- codigos %in% .codigos_espacios_invisibles
    # Mismo motivo que en `.quitar_controles_invisibles()`: sin nada que
    # normalizar, el valor se devuelve intacto en vez de reescribirse.
    if (!any(afectados)) return(texto)
    codigos[afectados] <- 32L
    paste0(intToUtf8(codigos, multiple = TRUE), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
  cambio <- .celdas_cambiadas(anterior, nuevo)
  list(valor = .resultado_texto(x, nuevo), n = sum(cambio),
       n_no_reversibles = sum(.celdas_que_pierden_valor(anterior, nuevo)))
}

.entidad_html_reemplazo <- function(entidad) {
  if (!.entidad_html_valida(entidad)) return(entidad)
  cuerpo <- substring(entidad, 2L, nchar(entidad) - 1L)
  if (startsWith(cuerpo, "#x") || startsWith(cuerpo, "#X")) {
    punto <- suppressWarnings(strtoi(substring(cuerpo, 3L), base = 16L))
    return(intToUtf8(punto))
  }
  if (startsWith(cuerpo, "#")) {
    punto <- suppressWarnings(as.integer(substring(cuerpo, 2L)))
    return(intToUtf8(punto))
  }
  unname(.entidades_html_comunes[[cuerpo]])
}

.decodificar_entidades_html <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La decodificaci\u00f3n HTML requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  # La misma que usa la deteccion: las dos tienen que coincidir.
  patron <- .PATRON_ENTIDAD_HTML
  nuevo <- vapply(anterior, function(texto) {
    if (is.na(texto)) return(NA_character_)
    coincidencias <- gregexpr(patron, texto, perl = TRUE)[[1L]]
    if (identical(coincidencias[[1L]], -1L)) return(texto)
    encontrados <- regmatches(texto, list(coincidencias))[[1L]]
    reemplazos <- vapply(encontrados, .entidad_html_reemplazo, character(1L))
    regmatches(texto, list(coincidencias)) <- list(reemplazos)
    texto
  }, character(1L), USE.NAMES = FALSE)
  list(valor = .resultado_texto(x, nuevo), n = sum(.celdas_cambiadas(anterior, nuevo)))
}

.reemplazar_separadores <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("El reemplazo de saltos de l\u00ednea requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  patron <- "\\r\\n|[\\t\\n\\r\\f\\v]"
  nuevo <- .aplicar_por_marca(
    anterior,
    function(v) gsub(patron, " ", v, perl = TRUE),
    function(v) gsub(patron, " ", v, perl = TRUE, useBytes = TRUE)
  )
  list(valor = .resultado_texto(x, nuevo), n = sum(.celdas_cambiadas(anterior, nuevo)))
}

.reparar_codificacion <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La reparaci\u00f3n de codificaci\u00f3n requiere una columna de texto.",
         call. = FALSE)
  }
  iteraciones <- parametros$max_iteraciones
  if (is.null(iteraciones)) iteraciones <- 20L
  anterior <- as.character(x)
  # La reparacion tiene que ver LO MISMO que vio el perfil, o el paquete
  # recomienda una accion que despues no puede ejecutar.
  #
  # `perfilar()` analiza sobre la forma saneada -`.texto_analizable()` marca
  # UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito- y
  # sobre esa forma publica `n_codificacion_reparable` y el estado. La
  # aplicacion trabajaba sobre la columna CRUDA, todavia declarada `bytes`, y
  # ahi `.ftfy_decode_inconsistent_utf8()` muere en `nchar()` -que sobre esa
  # marca no puede contar caracteres-.
  #
  # Medido: con mojibake declarado `bytes`, el perfil publicaba
  # `reparable = 2`, estado `reparado`, y el plan una accion `recomendada` y
  # activa con `n_afectadas = 2`; `aplicar()` la daba por `fallida` con
  # `n_cambiadas = 0` y el dato quedaba intacto.
  #
  # Se marca, no se convierte: los bytes no cambian. Lo que no es UTF-8 valido
  # queda como esta -no hay nada que interpretar ahi- y sigue contandose en
  # `n_codificacion_invalida`.
  anterior <- .texto_para_transformar(anterior)
  unicos <- unique(anterior[!is.na(anterior)])
  resultados <- lapply(unicos, .ftfy_reparar_uno, max_iteraciones = iteraciones)
  indice <- match(anterior, unicos)
  candidatos <- rep(NA_character_, length(anterior))
  estados <- rep(NA_character_, length(anterior))
  if (length(unicos)) {
    candidatos[!is.na(indice)] <- vapply(resultados[indice[!is.na(indice)]],
      function(z) z$texto, character(1L))
    estados[!is.na(indice)] <- vapply(resultados[indice[!is.na(indice)]],
      function(z) z$estado, character(1L))
  }
  mascara <- !is.na(candidatos) & !is.na(anterior) & candidatos != anterior &
    estados == "reparado"
  nuevo <- anterior
  nuevo[mascara] <- candidatos[mascara]
  parciales <- !is.na(candidatos) & !is.na(anterior) & candidatos != anterior &
    estados == "reparado_parcialmente"
  estado <- .ftfy_estado_agregado(estados)
  list(valor = .resultado_texto(x, nuevo), n = sum(mascara),
       n_parciales = sum(parciales), estado_reparacion = estado,
       estados = estados)
}

# El texto que `.convertir_numero_regional()` le pasa a `as.numeric()`, con
# moneda y unidad sacadas y los separadores ya resueltos segun la convencion.
#
# Vive aparte porque la guarda de precision de `.evaluar_conversion()` necesita
# exactamente este texto. Antes lo rearmaba por su cuenta a partir del original,
# y cada formato que el conversor aceptaba era un agujero posible en la guarda:
# en cinco rondas aparecieron cinco -entero plano, con `%`, notacion
# cientifica, mantisa con coma, y entero es-UY con `,0` final-. Con una sola
# normalizacion para los dos, lo que el conversor sabe leer la guarda lo ve.
# El texto que `.convertir_tipo()` le pasa a `as.numeric()`. Aparte por el
# mismo motivo que `.texto_regional_normalizado()`.
.texto_tipo_normalizado <- function(x) {
  sub(",", ".", trimws(as.character(x)), fixed = TRUE)
}

.texto_regional_normalizado <- function(x, parametros) {
  partes <- .componentes_numero_texto(x)
  presentes <- !is.na(x) & nzchar(trimws(as.character(x)))
  if (any(presentes & !partes$compatible)) {
    stop("Hay valores presentes que no responden al formato num\u00e9rico regional.",
         call. = FALSE)
  }
  convencion <- parametros$convencion
  if (is.null(convencion) || !length(convencion)) convencion <- "ambigua"
  if (identical(convencion, "es-UY")) convencion <- "decimal_coma"
  texto <- partes$cuerpo
  if (identical(convencion, "decimal_coma")) {
    texto <- gsub(".", "", texto, fixed = TRUE)
    texto <- sub(",", ".", texto, fixed = TRUE)
  } else if (identical(convencion, "decimal_punto")) {
    texto <- gsub(",", "", texto, fixed = TRUE)
  } else if (identical(convencion, "ambigua")) {
    ambiguos_punto <- presentes & partes$punto_tres
    ambiguos_coma <- presentes & partes$coma_tres
    interpretacion_punto <- parametros$punto_sin_coma
    interpretacion_coma <- parametros$coma_sin_punto
    if (any(ambiguos_punto) &&
        (length(interpretacion_punto) != 1L || is.na(interpretacion_punto) ||
         !interpretacion_punto %in% c("miles", "decimal"))) {
      stop(
        "La columna es ambigua; configure `punto_sin_coma` como 'miles' o 'decimal'.",
        call. = FALSE
      )
    }
    if (any(ambiguos_coma) &&
        (length(interpretacion_coma) != 1L || is.na(interpretacion_coma) ||
         !interpretacion_coma %in% c("miles", "decimal"))) {
      stop(
        "La columna es ambigua; configure `coma_sin_punto` como 'miles' o 'decimal'.",
        call. = FALSE
      )
    }
    if (any(ambiguos_punto) && identical(interpretacion_punto, "miles")) {
      texto[ambiguos_punto] <- gsub(".", "", texto[ambiguos_punto], fixed = TRUE)
    }
    if (any(ambiguos_coma)) {
      if (identical(interpretacion_coma, "miles")) {
        texto[ambiguos_coma] <- gsub(",", "", texto[ambiguos_coma], fixed = TRUE)
      } else {
        texto[ambiguos_coma] <- sub(",", ".", texto[ambiguos_coma], fixed = TRUE)
      }
    }
  } else if (!identical(convencion, "sin_separadores")) {
    stop("La convenci\u00f3n num\u00e9rica no est\u00e1 confirmada.", call. = FALSE)
  }
  list(texto = texto, presentes = presentes, partes = partes)
}

.convertir_numero_regional <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La conversi\u00f3n regional requiere una columna de texto.", call. = FALSE)
  }
  normalizado <- .texto_regional_normalizado(x, parametros)
  texto <- normalizado$texto
  presentes <- normalizado$presentes
  partes <- normalizado$partes
  numero <- suppressWarnings(as.numeric(texto))
  if (any(presentes & (!is.finite(numero) | is.na(numero)))) {
    stop("No fue posible convertir todos los valores regionales.", call. = FALSE)
  }
  porcentajes <- presentes & partes$unidad == "%"
  numero[porcentajes] <- numero[porcentajes] / 100
  list(valor = numero, n = sum(presentes))
}

.resultado_texto <- function(original, nuevo) {
  if (!is.factor(original)) return(nuevo)
  # Las acciones pueden introducir valores fuera de los niveles originales;
  # el contrato devuelve texto y no un factor incompleto.
  as.character(nuevo)
}

# Deja el texto en la forma que el PERFIL analizo, para que lo que el plan
# propone se pueda ejecutar sobre lo mismo que se midio.
#
# `perfilar()` analiza sobre la forma saneada -`.texto_analizable()` marca
# UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito, para que
# `tolower()` y las expresiones regulares puedan trabajar- y sobre esa forma
# publica sus cifras y arma el plan. Las acciones trabajaban sobre la columna
# CRUDA, todavia declarada `bytes`, y ahi las mismas primitivas abortan: el
# paquete recomendaba acciones que no podia ejecutar.
#
# Medido sobre una columna con mayusculas inconsistentes y un valor declarado:
# `convertir_minusculas`, `convertir_titulo` y `convertir_mayusculas` quedaban
# en `fallida` con `n_cambiadas = 0`, y `convertir_segun_diccionario` abortaba.
#
# Se MARCA, no se convierte: los bytes no cambian. Lo que no es UTF-8 valido
# queda como esta -no hay nada que interpretar ahi- y sigue contandose en
# `n_codificacion_invalida`.
.texto_para_transformar <- function(x) {
  texto <- as.character(x)
  crudos <- !is.na(texto) & Encoding(texto) == "bytes" & validUTF8(texto)
  if (any(crudos)) {
    marcados <- texto[crudos]
    Encoding(marcados) <- "UTF-8"
    texto[crudos] <- marcados
  }
  texto
}

.transformar_capitalizacion <- function(x, estrategia, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La capitalizaci\u00f3n requiere una columna de texto.", call. = FALSE)
  }
  anterior <- .texto_para_transformar(x)
  if (identical(estrategia, "convertir_minusculas")) {
    nuevo <- .aplicar_por_marca(anterior, tolower)
  } else if (identical(estrategia, "convertir_mayusculas")) {
    nuevo <- .aplicar_por_marca(anterior, toupper)
  } else if (identical(estrategia, "convertir_titulo")) {
    nuevo <- .aplicar_por_marca(anterior, function(v) gsub(
      "\\b([[:alpha:]])", "\\U\\1", tolower(v), perl = TRUE
    ))
  } else {
    diccionario <- parametros$diccionario
    if (is.null(diccionario) || !is.atomic(diccionario) ||
        is.null(names(diccionario)) || any(!nzchar(names(diccionario)))) {
      stop(
        "La capitalizaci\u00f3n por diccionario requiere un vector at\u00f3mico con nombres.",
        call. = FALSE
      )
    }
    nuevo <- anterior
    indices <- match(anterior, names(diccionario))
    reemplazar <- !is.na(indices) & !is.na(anterior)
    nuevo[reemplazar] <- as.character(diccionario[indices[reemplazar]])
  }
  mascara <- .celdas_cambiadas(anterior, nuevo)
  list(valor = .resultado_texto(x, nuevo), n = sum(mascara))
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
  texto <- .texto_tipo_normalizado(x)
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

.limites_outliers <- function(x) {
  if (inherits(x, c("Date", "POSIXt")) || is.numeric(x)) {
    valores <- as.numeric(x)
  } else {
    valores <- suppressWarnings(as.numeric(
      sub(",", ".", trimws(as.character(x)), fixed = TRUE)
    ))
  }
  validos <- is.finite(valores)
  if (!any(validos)) {
    return(list(valores = valores, validos = validos, inferior = NA_real_,
                superior = NA_real_))
  }
  iqr <- stats::IQR(valores[validos], type = 7)
  cuartiles <- stats::quantile(
    valores[validos], c(0.25, 0.75), names = FALSE, type = 7
  )
  list(
    valores = valores, validos = validos,
    inferior = cuartiles[[1L]] - 1.5 * iqr,
    superior = cuartiles[[2L]] + 1.5 * iqr
  )
}

.marca_outliers <- function(x) {
  limites <- .limites_outliers(x)
  mascara <- rep(FALSE, length(x))
  if (!any(limites$validos)) return(mascara)
  mascara[limites$validos] <-
    limites$valores[limites$validos] < limites$inferior |
    limites$valores[limites$validos] > limites$superior
  mascara
}

.winsorizar_outliers <- function(x) {
  limites <- .limites_outliers(x)
  mascara <- .marca_outliers(x)
  salida <- limites$valores
  salida[limites$validos] <- pmin(
    pmax(salida[limites$validos], limites$inferior), limites$superior
  )
  salida[!limites$validos] <- NA_real_
  # Con pocas observaciones el propio reemplazo mueve los cuartiles: el
  # limite de Tukey que saco al extremo puede convertirse en el nuevo extremo
  # y el perfil seguiria emitiendo el mismo hallazgo. Ajustar los residuos al
  # centro de los valores que no son extremos conserva la intencion de
  # winsorizar y hace que la accion sea comprobable al volver a perfilar.
  if (any(mascara)) {
    limite_iteraciones <- max(1L, 2L * length(salida))
    for (iteracion in seq_len(limite_iteraciones)) {
      residuos <- .marca_outliers(salida)
      if (!any(residuos)) break
      centrales <- salida[is.finite(salida) & !residuos]
      if (!length(centrales)) break
      salida[residuos] <- stats::median(centrales)
    }
    if (any(.marca_outliers(salida))) {
      centrales <- salida[is.finite(salida)]
      if (length(centrales)) salida[is.finite(salida)] <- centrales[[1L]]
    }
  }
  cambio <- limites$validos & limites$valores != salida
  list(valor = salida, n = sum(cambio))
}

.grupos_filas_duplicadas <- function(datos) {
  datos_base <- .tabla_base(datos)
  if (!ncol(datos_base)) {
    repetidas <- seq_len(nrow(datos_base)) > 1L
    grupos <- if (nrow(datos_base) > 1L) {
      rep.int(1L, nrow(datos_base))
    } else {
      rep.int(NA_integer_, nrow(datos_base))
    }
    return(list(repetidas = repetidas, grupos = grupos))
  }
  if (any(vapply(datos_base, is.list, logical(1L)))) {
    stop("No se pueden agrupar duplicados con columnas de lista.", call. = FALSE)
  }
  # Todo sale de los MISMOS codigos, y no de `duplicated.data.frame`.
  #
  # Sobre una columna `integer64`, `duplicated(x, fromLast = TRUE)` devuelve lo
  # mismo que sin `fromLast`: el metodo de `bit64` ignora el argumento. Con eso,
  # `participantes` se quedaba con la ultima fila de cada grupo y no con las
  # otras. Medido sobre `as.integer64(c(1, 2, -999, 4, -999))`: el plan declaraba
  # 2 filas, la accion marcaba 1, `.grupo_duplicado` dejaba la fila 3 sin grupo
  # -participa, con contenido identico- y el hallazgo sobrevivia al re-perfilar.
  # La misma tabla en `double` marcaba las dos, que es como se supo que el
  # defecto era del camino `integer64` y no de la regla.
  #
  # `factor()` lleva cualquier tipo a niveles por su representacion, y sobre los
  # codigos enteros que salen de ahi `duplicated()` es el de base y si honra
  # `fromLast`. Ademas es una sola definicion de "misma fila" para las tres
  # cosas que antes usaban dos.
  factores <- lapply(datos_base, function(x) {
    valores <- if (is.character(x) || is.factor(x)) {
      .nombres_para_operar(as.character(x))
    } else x
    factor(valores, exclude = NULL)
  })
  codigos <- as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
  repetidas <- duplicated(codigos)
  participantes <- repetidas | duplicated(codigos, fromLast = TRUE)
  grupos <- rep(NA_integer_, nrow(datos_base))
  if (!any(participantes)) {
    return(list(repetidas = repetidas, grupos = grupos))
  }
  grupos[participantes] <- match(
    codigos[participantes], unique(codigos[participantes])
  )
  list(repetidas = repetidas, grupos = grupos)
}

.filtrar_filas <- function(datos, conservar) {
  if (inherits(datos, "data.table")) {
    salida <- as.data.frame(data.table::copy(datos), stringsAsFactors = FALSE)
    data.table::as.data.table(salida[which(conservar), , drop = FALSE])
  } else {
    datos[conservar, , drop = FALSE]
  }
}

.conservar_mas_completa <- function(datos, clave) {
  datos_base <- .tabla_base(datos)
  if (!length(clave) || anyNA(.indice_nombre(clave, names(datos_base)))) {
    stop(
      "`conservar_mas_completa` requiere configurar nombres de clave existentes.",
      call. = FALSE
    )
  }
  claves <- .seleccionar_columnas(datos_base, clave)
  if (any(vapply(claves, is.list, logical(1L)))) {
    stop("La clave no puede contener columnas de lista.", call. = FALSE)
  }
  factores <- lapply(claves, function(x) {
    valores <- if (is.character(x) || is.factor(x)) {
      .nombres_para_operar(as.character(x))
    } else x
    factor(valores, exclude = NULL)
  })
  codigos <- as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
  grupos <- split(seq_len(nrow(datos_base)), codigos)
  completitud <- rowSums(!is.na(datos_base))
  conservar <- rep(TRUE, nrow(datos_base))
  for (indices in grupos) {
    if (length(indices) > 1L) {
      elegido <- indices[[which.max(completitud[indices])]]
      conservar[setdiff(indices, elegido)] <- FALSE
    }
  }
  conservar
}

.contenido_igual <- function(x, y) {
  .columnas_identicas(x, y)
}

.validar_nombres_iniciales <- function(plan, datos) {
  indices <- which(
    plan$aplicar & plan$estrategia %in%
      c("normalizar_nombres", "normalizar_nombres_snake_case")
  )
  if (!length(indices)) return(invisible(TRUE))
  esperados <- plan$parametros[[indices[[1L]]]]$nombres_esperados
  if (!identical(.nombres_para_operar(names(datos)),
                 .nombres_para_operar(esperados))) {
    stop("Los nombres de los datos no coinciden con los usados por el perfil.", call. = FALSE)
  }
  invisible(TRUE)
}

.agregar_marca <- function(datos, nombre, valor) {
  if (.nombres_para_operar(nombre) %in% .nombres_para_operar(names(datos))) {
    stop("La columna de marca ya existe: ", nombre, ".", call. = FALSE)
  }
  datos[[nombre]] <- valor
  datos
}

# Cuantos valores CAMBIARON de verdad, que es lo que `n_cambiadas` promete.
#
# Las dos conversiones devolvian `sum(!is.na(x))` -los presentes-, y catorce
# acciones de este mismo archivo devuelven `sum(cambio)`. La misma idea escrita
# de dos maneras. Se ve cuando la conversion es una IDENTIDAD: planificar sobre
# una columna de texto y aplicar sobre la misma tabla ya convertida a entero
# dejaba la columna bit a bit igual, con `estado = ejecutada` y
# `n_cambiadas = 4`. La documentacion promete lo contrario: "si una accion no
# produce ningun efecto cuando el plan estimaba alguno, se registra como
# fallida". Con el conteo correcto eso sale solo, porque
# `.motivo_efecto_accion()` ya mira el caso `actual == 0`.
#
# Si cambia la clase, todo valor presente se convirtio de verdad. Si no cambia,
# se comparan las representaciones: un presente que quedo `NA` tambien cambio
# -se perdio-, y cuenta.
.n_valores_cambiados <- function(antes, despues) {
  presentes <- !is.na(antes)
  if (!any(presentes)) return(0L)
  if (!identical(class(antes), class(despues))) {
    return(as.integer(sum(presentes)))
  }
  a <- as.character(antes)[presentes]
  b <- as.character(despues)[presentes]
  as.integer(sum(is.na(b) | a != b))
}

.mascara_aplicabilidad_accion <- function(datos, columna, parametros) {
  regla <- parametros$aplicabilidad
  if (is.null(regla)) return(rep(TRUE, nrow(datos)))
  mascara <- tryCatch(
    .evaluar_predicado_aplicabilidad(datos, columna, regla),
    error = function(e) e
  )
  if (inherits(mascara, "error")) {
    stop(
      "No se pudo respetar la aplicabilidad de `", columna, "`: ",
      conditionMessage(mascara), call. = FALSE
    )
  }
  if (length(mascara) != nrow(datos)) {
    stop(
      "No se pudo respetar la aplicabilidad de `", columna,
      "`: la regla no devolvio una marca por fila.", call. = FALSE
    )
  }
  !is.na(mascara) & mascara
}

.ejecutar_accion <- function(datos, accion, original = datos) {
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  columna <- accion$columna[[1L]]

  if (estrategia %in% c(
    "normalizar_nombres", "normalizar_nombres_snake_case"
  )) {
    anteriores <- names(datos)
    names(datos) <- if (identical(estrategia, "normalizar_nombres")) {
      .nombres_make_names(anteriores)
    } else {
      .nombres_snake(anteriores)
    }
    return(list(
      datos = datos,
      n = sum(.nombres_para_operar(anteriores) !=
                .nombres_para_operar(names(datos)))
    ))
  }
  if (identical(estrategia, "marcar_filas_duplicadas")) {
    marcas <- .grupos_filas_duplicadas(datos)
    datos <- .agregar_marca(datos, parametros$columna_marca, marcas$repetidas)
    datos <- .agregar_marca(datos, parametros$columna_grupo, marcas$grupos)
    return(list(datos = datos, n = sum(!is.na(marcas$grupos))))
  }
  if (identical(estrategia, "conservar_primera_duplicada")) {
    eliminar <- base::duplicated.data.frame(.tabla_base(datos))
    retiradas <- .filtrar_filas(datos, eliminar)
    datos <- .filtrar_filas(datos, !eliminar)
    return(list(
      datos = datos, n = sum(eliminar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(eliminar), n_columnas_eliminadas = 0
    ))
  }
  if (identical(estrategia, "conservar_mas_completa")) {
    conservar <- .conservar_mas_completa(datos, parametros$clave)
    retiradas <- .filtrar_filas(datos, !conservar)
    datos <- .filtrar_filas(datos, conservar)
    return(list(
      datos = datos, n = sum(!conservar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(!conservar), n_columnas_eliminadas = 0
    ))
  }
  if (estrategia %in% c(
    "marcar_columnas_duplicadas", "eliminar_columna_duplicada"
  )) {
    indice_1 <- .indice_columna(datos, parametros$columna_1)
    indice_2 <- .indice_columna(datos, parametros$columna_2)
    if (!.contenido_igual(datos[[indice_1]], datos[[indice_2]])) {
      stop("Las columnas dejaron de tener contenido duplicado.", call. = FALSE)
    }
    if (identical(estrategia, "marcar_columnas_duplicadas")) {
      marcas <- attr(datos, "columnas_duplicadas_marcadas", exact = TRUE)
      nueva <- data.frame(
        columna_1 = parametros$columna_1,
        columna_2 = parametros$columna_2,
        stringsAsFactors = FALSE
      )
      combinadas <- if (is.null(marcas)) {
        nueva
      } else {
        unique(rbind(marcas, nueva))
      }
      attr(datos, "columnas_duplicadas_marcadas") <- combinadas
      # El conteo es el cambio REAL, no un 1 fijo. Con la marca ya puesta
      # -dos acciones iguales en el mismo plan- `unique()` la descarta y no
      # cambia nada, y devolver 1 informaba un efecto que no ocurrio. Con 0,
      # el registro la marca `fallida`, que es lo que promete la documentacion
      # para una accion seleccionada sin efecto.
      return(list(
        datos = datos,
        n = if (identical(combinadas, marcas)) 0 else 1
      ))
    }
    indice <- .indice_columna(datos, parametros$eliminar)
    retirada <- list(
      nombre = names(datos)[[indice]], posicion = indice, valores = datos[[indice]]
    )
    datos[[indice]] <- NULL
    return(list(
      datos = datos, n = 1, columna_eliminada = retirada,
      n_filas_eliminadas = 0, n_columnas_eliminadas = 1
    ))
  }
  if (identical(estrategia, "eliminar_columna_constante")) {
    indice <- .indice_columna(datos, columna)
    x <- datos[[indice]]
    if (length(unique(x[!is.na(x)])) > 1L) {
      stop("La columna dej\u00f3 de ser constante.", call. = FALSE)
    }
    retirada <- list(nombre = columna, posicion = indice, valores = x)
    datos[[indice]] <- NULL
    return(list(
      datos = datos, n = 1, columna_eliminada = retirada,
      n_filas_eliminadas = 0, n_columnas_eliminadas = 1
    ))
  }

  indice <- .indice_columna(datos, columna)
  x <- datos[[indice]]
  if (identical(estrategia, "convertir_ausencias_textuales")) {
    cambio <- .reemplazar_ausencias_textuales(x, parametros)
    datos[[indice]] <- cambio$valor
    # Cada celda que se convierte en ausencia pierde el texto que tenia: `S/D`
    # o `N/A` no quedan en ningun lado. El plan ya declara la accion
    # `reversible = FALSE`, pero el registro informaba `n_no_reversibles = 0`
    # porque este ejecutor no lo devolvia y `aplicar()` completa con cero: dos
    # perdidas reales y ninguna cuantificada. Aca todo cambio es irreversible.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (identical(estrategia, "convertir_sentinelas_numericos")) {
    cambio <- .reemplazar_sentinelas_numericos(x, parametros)
    datos[[indice]] <- cambio$valor
    # Cada celda cambiada pierde su valor original y ningun formato lo
    # recupera. El registro informaba `n_no_reversibles = 0` porque este
    # ejecutor no lo devolvia y `aplicar()` completa con cero: las mismas
    # perdidas que ya se cuentan en `convertir_ausencias_textuales`.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (startsWith(estrategia, "imputar_dependencia_funcional__")) {
    return(.imputar_dependencia(datos, parametros))
  }
  if (identical(estrategia, "recortar_espacios")) {
    cambio <- .recortar_texto(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "eliminar_controles_invisibles")) {
    cambio <- .quitar_controles_invisibles(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "normalizar_espacios_invisibles")) {
    cambio <- .normalizar_espacios_invisibles(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "decodificar_entidades_html")) {
    cambio <- .decodificar_entidades_html(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "reemplazar_separadores")) {
    cambio <- .reemplazar_separadores(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "reparar_codificacion")) {
    cambio <- .reparar_codificacion(x, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n,
                estado_reparacion = cambio$estado_reparacion,
                n_parciales = cambio$n_parciales))
  }
  if (identical(estrategia, "convertir_numero_regional")) {
    cambio <- .convertir_numero_regional(x, parametros)
    datos[[indice]] <- cambio$valor
    evaluacion <- .evaluar_conversion(
      x, cambio$valor, estrategia, parametros
    )
    # Mismo conteo que las otras dos conversiones de tipo: contaba solo los
    # valores no vacios, asi que un `""` que la conversion vuelve `NA` no
    # figuraba entre los cambios.
    return(list(datos = datos, n = .n_valores_cambiados(x, cambio$valor),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  if (identical(estrategia, "marcar_filas_ausentes")) {
    aplicable <- .mascara_aplicabilidad_accion(original, columna, parametros)
    if (length(aplicable) != length(x)) {
      stop(
        "No se pudo respetar la aplicabilidad despues de eliminar filas.",
        call. = FALSE
      )
    }
    marca <- is.na(x) & aplicable
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "eliminar_filas_ausentes")) {
    aplicable <- .mascara_aplicabilidad_accion(original, columna, parametros)
    if (length(aplicable) != length(x)) {
      stop(
        "No se pudo respetar la aplicabilidad despues de eliminar filas.",
        call. = FALSE
      )
    }
    eliminar <- is.na(x) & aplicable
    retiradas <- .filtrar_filas(datos, eliminar)
    datos <- .filtrar_filas(datos, !eliminar)
    return(list(
      datos = datos, n = sum(eliminar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(eliminar), n_columnas_eliminadas = 0
    ))
  }
  if (identical(estrategia, "convertir_fecha_confirmada")) {
    convertido <- .convertir_fecha(x, parametros)
    datos[[indice]] <- convertido
    evaluacion <- .evaluar_conversion(
      x, convertido, estrategia, parametros
    )
    return(list(datos = datos, n = .n_valores_cambiados(x, convertido),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  if (identical(estrategia, "convertir_tipo")) {
    convertido <- .convertir_tipo(x, parametros)
    datos[[indice]] <- convertido
    evaluacion <- .evaluar_conversion(
      x, convertido, estrategia, parametros
    )
    return(list(datos = datos, n = .n_valores_cambiados(x, convertido),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  if (identical(estrategia, "marcar_outliers")) {
    marca <- .marca_outliers(x)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "winsorizar_outliers")) {
    cambio <- .winsorizar_outliers(x)
    datos[[indice]] <- cambio$valor
    # Cada celda cambiada pierde su valor original y ningun formato lo
    # recupera. El registro informaba `n_no_reversibles = 0` porque este
    # ejecutor no lo devolvia y `aplicar()` completa con cero: las mismas
    # perdidas que ya se cuentan en `convertir_ausencias_textuales`.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (estrategia %in% c(
    "convertir_minusculas", "convertir_titulo", "convertir_mayusculas",
    "convertir_segun_diccionario"
  )) {
    cambio <- .transformar_capitalizacion(x, estrategia, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  stop("Estrategia de limpieza no implementada: ", estrategia, ".", call. = FALSE)
}

.registro_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    grupo = character(), decision_grupo = character(), estrategia = character(),
    destructiva = logical(), n_cambiadas = numeric(),
    n_no_reversibles = numeric(), justificacion = character(),
    estado = character(), error = character(),
    estado_reparacion = character(),
    n_filas_eliminadas = numeric(), n_columnas_eliminadas = numeric(),
    fecha_hora = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

.estrategias_eliminatorias <- function() {
  c(
    "conservar_primera_duplicada", "conservar_mas_completa",
    "eliminar_filas_ausentes", "eliminar_columna_duplicada",
    "eliminar_columna_constante"
  )
}

# Quien edita un plan no conoce las funciones internas, y ese es justamente el
# caso de uso que la capa de remediacion declara: un plan editable. Medido
# mutando los veinte nombres de parametro que produce un plan, en seis casos de
# datos: solo los dos que nombran una columna a crear fallaban con el mensaje
# interno de R -"argumento tiene longitud cero" con largo cero, "valor ausente
# donde TRUE/FALSE es necesario" con NA-. Con la cadena vacia era peor que
# fallar: creaba una columna llamada "V3", que no es la que el plan decia.
#
# No se valida mas que eso a proposito. Seis de los veintiocho parametros que
# produce un plan legitimo son de largo cero o todo NA -`clave = character(0)`
# significa "sin clave declarada", y `punto_sin_coma = NA` significa "sin
# determinar"-, asi que una regla general sobre todos los parametros
# rechazaria planes que funcionan.
.parametros_nombre_de_columna <- c("columna_marca", "columna_grupo")

.motivo_parametros_accion <- function(accion) {
  parametros <- accion$parametros[[1L]]
  if (!length(parametros)) return(NULL)
  nombres <- intersect(names(parametros), .parametros_nombre_de_columna)
  for (nombre in nombres) {
    valor <- parametros[[nombre]]
    problema <- if (length(valor) != 1L) {
      paste0("recibi\u00f3 ", length(valor), " valores")
    } else if (is.na(valor)) {
      "recibi\u00f3 NA"
    } else if (!nzchar(trimws(as.character(valor)))) {
      "recibi\u00f3 una cadena vac\u00eda"
    } else {
      NULL
    }
    if (!is.null(problema)) {
      return(paste0(
        "El par\u00e1metro `", nombre, "` de la acci\u00f3n `",
        accion$estrategia[[1L]], "` debe ser el nombre de la columna a crear: ",
        "un \u00fanico texto no vac\u00edo; ", problema, "."
      ))
    }
  }
  NULL
}

.motivo_efecto_accion <- function(accion, ejecutada) {
  actual <- as.numeric(ejecutada$n)
  if (length(actual) != 1L || !is.finite(actual) || actual != 0L) {
    return(NULL)
  }
  esperado <- as.numeric(accion$n_afectadas[[1L]])
  unidad <- as.character(accion$unidad_conteo[[1L]])
  detalle_estimacion <- if (length(esperado) == 1L && is.finite(esperado)) {
    paste0(
      " El plan estimaba ", esperado,
      if (length(unidad) && !is.na(unidad) && nzchar(unidad)) {
        paste0(" ", unidad)
      } else "", "."
    )
  } else {
    " El plan no contiene una estimaci\u00f3n v\u00e1lida; la comprobaci\u00f3n se basa en el efecto observado."
  }
  paste0(
    "La acci\u00f3n `", accion$estrategia[[1L]],
    "` qued\u00f3 sin efecto: no cambi\u00f3 ning\u00fan valor.",
    detalle_estimacion
  )
}

.accion_modifica_clave <- function(accion, clave) {
  if (!length(clave)) return(FALSE)
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  if (estrategia %in% c("normalizar_nombres", "normalizar_nombres_snake_case")) {
    esperados <- parametros$nombres_esperados
    propuestos <- parametros$nombres_propuestos
    if (length(esperados) && length(propuestos) &&
        length(esperados) == length(propuestos)) {
      cambiadas <- .nombres_para_operar(esperados) !=
        .nombres_para_operar(propuestos)
      return(any(.nombres_para_operar(clave) %in%
                 .nombres_para_operar(esperados[cambiadas])))
    }
    return(TRUE)
  }
  if (estrategia %in% c(
    "marcar_filas_duplicadas", "marcar_columnas_duplicadas",
    "marcar_outliers"
  )) return(FALSE)
  if (estrategia %in% c(
    "eliminar_columna_duplicada", "eliminar_columna_constante"
  )) {
    return(isTRUE(.nombres_para_operar(parametros$eliminar) %in%
                  .nombres_para_operar(clave)) ||
      isTRUE(.nombres_para_operar(accion$columna[[1L]]) %in%
             .nombres_para_operar(clave)))
  }
  columna <- accion$columna[[1L]]
  !is.na(columna) && .nombres_para_operar(columna) %in%
    .nombres_para_operar(clave)
}

#' @rdname planificar_limpieza
#' @param permitir_eliminacion Segundo consentimiento obligatorio para ejecutar
#'   acciones que eliminan filas o columnas.
#' @param conservar_eliminados Si se conservan en el resultado las filas y
#'   columnas retiradas. Es `TRUE` de forma predeterminada.
#' @export
aplicar <- function(plan, datos, permitir_eliminacion = FALSE,
                    conservar_eliminados = TRUE) {
  era_plan <- inherits(plan, "plan_limpieza")
  .validar_plan_limpieza(plan)
  plan <- .tabla_base(plan)
  if (era_plan) class(plan) <- unique(c("plan_limpieza", class(plan)))
  plan <- .sincronizar_decisiones(plan)
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  if (!is.logical(permitir_eliminacion) || length(permitir_eliminacion) != 1L ||
      is.na(permitir_eliminacion) || !is.logical(conservar_eliminados) ||
      length(conservar_eliminados) != 1L || is.na(conservar_eliminados)) {
    stop(
      "Los permisos de eliminaci\u00f3n deben ser l\u00f3gicos escalares sin NA.",
      call. = FALSE
    )
  }
  seleccion <- which(plan$aplicar)
  if (length(seleccion)) {
    # El desempate va por `id_accion`, que identifica a la accion y no cambia
    # si el usuario reordena las filas del plan. Desempatar por el indice de
    # fila hacia que el `orden` dejara de fijar una secuencia reproducible:
    # con dos acciones empatadas sobre la misma columna, el mismo plan con las
    # filas invertidas devolvia datos distintos -` A` contra `A`-, que es lo
    # contrario de lo que promete `planificar_limpieza()`.
    seleccion <- seleccion[order(
      plan$orden[seleccion],
      .clave_bytes(as.character(plan$id_accion[seleccion]))
    )]
  }
  # La puerta del consentimiento deriva de la ESTRATEGIA, no de la celda
  # `destructiva`. Combinarlas la volvia evitable con una edicion: poner
  # `destructiva = FALSE` en una fila de `eliminar_filas_ausentes` borraba la
  # mitad de las filas sin `permitir_eliminacion`. Que hace una accion lo sabe
  # el paquete; el plan editado dice que se aplica, no que hace.
  #
  # Se probo tambien sellar las columnas de diagnostico y rechazar su edicion,
  # y se RETIRO: editar `estado` es la palanca sancionada para correr una
  # accion bloqueada, y editar `estrategia` es como se ejercita la rama no
  # implementada. La huella cerraba un agujero que este cambio ya cierra en la
  # raiz, y de paso silenciaba dos usos legitimos.
  destructivas <- seleccion[
    plan$estrategia[seleccion] %in% .estrategias_eliminatorias()
  ]
  if (length(destructivas) && !permitir_eliminacion) {
    stop(
      "El plan contiene acciones destructivas y requiere ",
      "`permitir_eliminacion = TRUE`: ",
      paste(plan$estrategia[destructivas], collapse = ", "), ".",
      call. = FALSE
    )
  }
  .validar_nombres_iniciales(plan, datos)
  salida <- .copiar_datos(datos)
  registros <- vector("list", length(seleccion))
  eliminados <- list(filas = list(), columnas = list())
  for (j in seq_along(seleccion)) {
    accion <- plan[seleccion[[j]], , drop = FALSE]
    clave <- if (inherits(salida, "data.table") &&
                 requireNamespace("data.table", quietly = TRUE)) {
      data.table::key(salida)
    } else {
      character()
    }
    # Cada acción trabaja sobre una copia del estado anterior. Si falla, la
    # columna (o tabla) queda intacta y el resto del plan puede continuar.
    motivo_parametros <- .motivo_parametros_accion(accion)
    ejecutada <- if (!is.null(motivo_parametros)) {
      list(error = motivo_parametros, n = 0, n_no_reversibles = 0)
    } else {
      tryCatch(
        .ejecutar_accion(.copiar_datos(salida), accion, original = datos),
        error = function(e) list(
          error = conditionMessage(e), n = 0, n_no_reversibles = 0
        )
      )
    }
    if (is.null(ejecutada$error)) {
      ejecutada <- .restringir_a_aplicabilidad(ejecutada, salida, accion, datos)
    }
    if (is.null(ejecutada$error)) {
      motivo_efecto <- .motivo_efecto_accion(accion, ejecutada)
      if (!is.null(motivo_efecto)) ejecutada$error <- motivo_efecto
    }
    fallo <- !is.null(ejecutada$error)
    if (!fallo) {
      salida <- ejecutada$datos
      if (length(clave) && .accion_modifica_clave(accion, clave)) {
        data.table::setkey(salida, NULL)
      }
    }
    registro <- data.frame(
      id_accion = accion$id_accion[[1L]],
      columna = accion$columna[[1L]],
      hallazgo = accion$hallazgo[[1L]],
      grupo = accion$grupo[[1L]],
      decision_grupo = as.character(accion$decision_grupo[[1L]]),
      estrategia = accion$estrategia[[1L]],
      destructiva = accion$destructiva[[1L]],
      n_cambiadas = as.numeric(if (fallo) 0 else ejecutada$n),
      n_no_reversibles = as.numeric(if (fallo) 0 else {
        if (is.null(ejecutada$n_no_reversibles)) 0 else ejecutada$n_no_reversibles
      }),
      justificacion = as.character(accion$justificacion[[1L]]),
      estado = if (fallo) "fallida" else "ejecutada",
      error = if (fallo) as.character(ejecutada$error) else NA_character_,
      estado_reparacion = if (is.null(ejecutada$estado_reparacion)) {
        if ("estado_reparacion" %in% names(accion)) {
          as.character(accion$estado_reparacion[[1L]])
        } else NA_character_
      } else as.character(ejecutada$estado_reparacion),
      n_filas_eliminadas = as.numeric(
        if (is.null(ejecutada$n_filas_eliminadas)) 0 else ejecutada$n_filas_eliminadas
      ),
      n_columnas_eliminadas = as.numeric(
        if (is.null(ejecutada$n_columnas_eliminadas)) 0 else ejecutada$n_columnas_eliminadas
      ),
      fecha_hora = as.POSIXct(Sys.time(), tz = "UTC"),
      stringsAsFactors = FALSE
    )
    registro$parametros <- I(list(accion$parametros[[1L]]))
    registros[[j]] <- registro
    if (conservar_eliminados && !is.null(ejecutada$filas_eliminadas)) {
      eliminados$filas[[accion$id_accion[[1L]]]] <- ejecutada$filas_eliminadas
    }
    if (conservar_eliminados && !is.null(ejecutada$columna_eliminada)) {
      eliminados$columnas[[accion$id_accion[[1L]]]] <- ejecutada$columna_eliminada
    }
  }
  registro <- if (length(registros)) do.call(rbind, registros) else .registro_vacio()
  rownames(registro) <- NULL
  attr(salida, "registro_limpieza") <- registro
  estructura <- list(
    datos = salida,
    registro = registro,
    plan_aplicado = plan[seleccion, , drop = FALSE],
    plan = plan,
    eliminados = eliminados
  )
  class(estructura) <- "resultado_limpieza"
  estructura
}

# La evidencia viaja EN EL PLAN, que el usuario edita y vuelve a entregar, asi
# que no puede depender del locale del proceso que lo genero. `encodeString()`
# si depende: el mismo valor salia `"\u00c1"` bajo un locale UTF-8 y
# `"<U+00C1>"` bajo `C`, y dos personas obtenian planes distintos de los mismos
# datos. Se arma el mismo texto sin consultar el locale: los bytes validos se
# declaran, los rotos se escapan en octal, y se escapan a mano la barra, la
# comilla y los controles que `encodeString()` escapaba.
.texto_ejemplo <- function(x) {
  if (length(x) == 0L || is.na(x)) return("<NA>")
  texto <- .clave_bytes(as.character(x))
  texto <- gsub("\\", "\\\\", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub('"', '\\"', texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\n", "\\n", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\r", "\\r", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\t", "\\t", texto, fixed = TRUE, useBytes = TRUE)
  paste0('"', texto, '"')
}

.ejemplos_grupo <- function(acciones, datos, max_ejemplos = 5L,
                            columnas_protegidas = character()) {
  tipo <- acciones$hallazgo[[1L]]
  columna <- acciones$columna[[1L]]
  parametros <- acciones$parametros[[1L]]
  valores <- NULL

  if (identical(tipo, "filas_duplicadas")) {
    grupos <- .grupos_filas_duplicadas(datos)$grupos
    indices <- utils::head(which(!is.na(grupos)), max_ejemplos)
    indices_protegidas <- .indice_nombre(columnas_protegidas, names(datos))
    columnas_protegidas <- names(datos)[
      unique(indices_protegidas[!is.na(indices_protegidas)])
    ]
    valores_protegidos <- .valores_publicables_protegidos(
      datos, columnas_protegidas
    )
    return(vapply(indices, function(i) {
      contenido <- vapply(seq_along(datos), function(j) {
        texto <- .texto_ejemplo(datos[[j]][[i]])
        if (.nombres_para_operar(names(datos)[[j]]) %in%
            .nombres_para_operar(columnas_protegidas)) {
          texto <- .reemplazar_valores_protegidos(
            texto, valores_protegidos
          )
        }
        texto
      }, character(1L))
      paste0("fila ", i, " [grupo ", grupos[[i]], "]: ",
             paste(names(datos), contenido, sep = "=", collapse = ", "))
    }, character(1L)))
  }
  if (identical(tipo, "columnas_duplicadas")) {
    columna <- parametros$columna_1
  }
  if (identical(tipo, "nombres_columnas_problematicos")) {
    problema <- .nombres_columnas_problematicos(names(datos))
    originales <- vapply(problema$original, .texto_ejemplo, character(1L))
    propuestos <- vapply(problema$propuesto, .texto_ejemplo, character(1L))
    return(utils::head(paste(originales, "->", propuestos), max_ejemplos))
  }
  if (is.na(columna)) {
    evidencia <- unique(acciones$evidencia[nzchar(acciones$evidencia)])
    return(utils::head(evidencia, max_ejemplos))
  }
  indice <- .indice_columna(datos, columna)
  # Estos ejemplos se le MUESTRAN al usuario para que decida, asi que lo
  # declarado `bytes` se rinde a la forma que publica la consola. Sin esto,
  # `guiar_limpieza()` -que es exportada- moria con el error crudo de R "no se
  # permite traduccion de cadenas con bytes de codificacion" al hacer
  # `tolower()` sobre la columna cruda, y la corrida entera se perdia SIN
  # declarar nada: a diferencia de `aplicar()`, que anota la falla en su
  # registro, aca no quedaba rastro.
  x <- .texto_publicable(datos[[indice]])
  if (identical(tipo, "mayusculas_inconsistentes")) {
    unicos <- unique(as.character(x[!is.na(x)]))
    base <- tolower(unicos)
    colision <- duplicated(base) | duplicated(base, fromLast = TRUE)
    valores <- unicos[colision]
  } else if (identical(tipo, "faltantes_disfrazados")) {
    numericos <- suppressWarnings(as.numeric(trimws(as.character(x))))
    valores <- x[!is.na(numericos) & numericos %in% parametros$valores]
  } else if (identical(tipo, "outliers")) {
    valores <- x[.marca_outliers(x)]
  } else if (identical(tipo, "faltantes")) {
    valores <- paste0("<NA> en fila ", which(is.na(x)))
  } else if (identical(tipo, "constante")) {
    valores <- unique(x[!is.na(x)])
  } else {
    valores <- x[!is.na(x)]
  }
  utils::head(unique(vapply(valores, .texto_ejemplo, character(1L))), max_ejemplos)
}

.grupos_para_guiar <- function(plan) {
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  grupos[vapply(grupos, function(grupo) {
    indices <- which(plan$grupo == grupo)
    any(as.character(plan$decision_grupo[indices]) == "pendiente") ||
      any(plan$destructiva[indices])
  }, logical(1L))]
}

.resolver_seleccion_guiada <- function(respuesta, acciones, elegibles) {
  # `NA` NO es "no hacer nada". Lo era, y con eso un selector cuya lectura
  # fallara -`as.integer(entrada)` sobre algo que no es un numero devuelve NA-
  # dejaba TODOS los grupos como `omitida`, en silencio: el plan quedaba
  # registrado como revisado y omitido a proposito cuando no se reviso nada, y
  # el `.Rd` reserva `pendiente` para lo no revisado. Un 99, igual de invalido,
  # ya daba un error claro.
  #
  # El camino interactivo no produce NA: `utils::menu()` devuelve 0 al
  # cancelar, que sigue siendo "no hacer nada".
  if (!length(respuesta) ||
      identical(respuesta[[1L]], "no_hacer_nada") ||
      identical(respuesta[[1L]], 0L) || identical(respuesta[[1L]], 0)) {
    return(NA_integer_)
  }
  if (is.na(respuesta[[1L]])) {
    stop(
      "La selecci\u00f3n guiada devolvi\u00f3 NA, que no identifica ninguna opci\u00f3n. ",
      "Para no hacer nada, devolver 0.", call. = FALSE
    )
  }
  if (is.numeric(respuesta) && length(respuesta) == 1L &&
      respuesta == length(elegibles) + 1L) {
    return(NA_integer_)
  }
  if (is.numeric(respuesta) && length(respuesta) == 1L &&
      respuesta >= 1L && respuesta <= length(elegibles)) {
    return(elegibles[[as.integer(respuesta)]])
  }
  respuesta <- as.character(respuesta[[1L]])
  candidatos <- which(
    acciones$id_accion == respuesta | acciones$estrategia == respuesta
  )
  candidatos <- intersect(candidatos, elegibles)
  if (length(candidatos) != 1L) {
    stop("La selecci\u00f3n guiada no identifica una opci\u00f3n disponible.", call. = FALSE)
  }
  candidatos[[1L]]
}

#' Revisar decisiones de limpieza paso a paso
#'
#' Recorre los grupos pendientes y aquellos que contienen alternativas
#' destructivas. Muestra evidencia calculada sobre `datos`, las estrategias y
#' sus justificaciones, y devuelve el plan editado sin aplicarlo. En una sesión
#' no interactiva retorna inmediatamente el plan sin cambios, salvo que se
#' proporcione un `selector` explícito.
#'
#' No se representa "no hacer nada" como una acción ficticia. Elegirlo cambia
#' `decision_grupo` a `"omitida"`; por contraste, un grupo aún no revisado
#' conserva `"pendiente"`. Cuando conservar los datos es la recomendación, esa
#' opción también se muestra con la marca "(Recomendado)" y su justificación.
#' Los diccionarios de capitalización se suministran como una lista con nombre
#' de vectores con nombre.
#'
#' @param plan Objeto `plan_limpieza`.
#' @param datos Datos correspondientes al perfil que originó el plan.
#' @param selector Función opcional que recibe una lista con `grupo`,
#'   `acciones`, `elegibles`, `ejemplos` y `opciones`. Debe devolver la posición,
#'   el identificador o el nombre de una estrategia, o `0` para no hacer nada. `NA` no es un valor
#'   válido: se rechaza con un error, para que una lectura fallida no quede
#'   registrada como una decisión de omitir.
#' @param diccionarios Lista opcional con nombre de diccionarios por columna.
#' @param max_ejemplos Máximo de ejemplos reales mostrados por grupo.
#'
#' @return El mismo `plan_limpieza` recibido, con la columna de decisión
#'   sincronizada según lo elegido, y sin ejecutar ninguna acción sobre los
#'   datos: cambia qué acciones quedan marcadas para `aplicar()`, no la
#'   tabla. En una sesión no interactiva y sin `selector`, devuelve el plan
#'   sin cambios.
#' @export
#' @seealso [planificar_limpieza()], [aplicar()]
#'
#' @examples
#' datos <- data.frame(zona = c("Norte", "NORTE", "sur"))
#' plan <- planificar_limpieza(perfilar(datos), datos)
#' guiado <- guiar_limpieza(plan, datos)
#' identical(plan, guiado) # TRUE en una sesión no interactiva
guiar_limpieza <- function(plan, datos, selector = NULL,
                           diccionarios = list(), max_ejemplos = 5L) {
  era_plan <- inherits(plan, "plan_limpieza")
  .validar_plan_limpieza(plan)
  plan <- .tabla_base(plan)
  if (era_plan) class(plan) <- unique(c("plan_limpieza", class(plan)))
  plan <- .sincronizar_decisiones(plan)
  if (is.null(selector) && !interactive()) return(plan)
  if (!is.null(selector) && !is.function(selector)) {
    stop("`selector` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  datos <- .tabla_base(datos)
  if (!is.list(diccionarios) ||
      (length(diccionarios) &&
       (is.null(names(diccionarios)) || any(!nzchar(names(diccionarios)))))) {
    stop("`diccionarios` debe ser una lista con nombres de columna.", call. = FALSE)
  }
  if (!is.numeric(max_ejemplos) || length(max_ejemplos) != 1L ||
      is.na(max_ejemplos) || max_ejemplos < 1) {
    stop("`max_ejemplos` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  max_ejemplos <- floor(max_ejemplos)

  grupos <- .grupos_para_guiar(plan)
  columnas_protegidas <- attr(
    plan, "columnas_datos_personales_protegidas", exact = TRUE
  )
  if (is.null(columnas_protegidas)) columnas_protegidas <- character()
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    acciones <- plan[indices, , drop = FALSE]
    nombre_columna <- acciones$columna[[1L]]
    diccionario <- if (!is.na(nombre_columna)) {
      indice_diccionario <- .indice_nombre(nombre_columna, names(diccionarios))
      if (is.na(indice_diccionario)) NULL else diccionarios[[indice_diccionario]]
    } else {
      NULL
    }
    indice_diccionario <- which(
      acciones$estrategia == "convertir_segun_diccionario"
    )
    if (length(indice_diccionario) && !is.null(diccionario)) {
      plan$parametros[[indices[indice_diccionario]]] <- list(
        diccionario = diccionario
      )
      plan$estado[indices[indice_diccionario]] <- "lista"
      acciones <- plan[indices, , drop = FALSE]
    }
    elegibles <- which(as.character(acciones$estado) == "lista")
    ejemplos <- .ejemplos_grupo(
      acciones, datos, max_ejemplos, columnas_protegidas
    )
    etiquetas <- paste0(
      acciones$estrategia[elegibles],
      ifelse(acciones$recomendada[elegibles], " (Recomendado)", "")
    )
    no_hacer_recomendado <- any(
      acciones$recomendacion_grupo == "no_hacer_nada", na.rm = TRUE
    )
    etiqueta_no_hacer <- paste0(
      "No hacer nada",
      if (no_hacer_recomendado) " (Recomendado)" else ""
    )

    cli::cli_h2(.cli_literal(.marcar_para_exhibir(paste("Decisi\u00f3n", grupo))))
    cli::cli_text(.cli_literal(.marcar_para_exhibir(
      paste("Hallazgo:", acciones$hallazgo[[1L]])
    )))
    cli::cli_text(.cli_literal(.marcar_para_exhibir(paste(
      "Objeto afectado:",
      if (is.na(acciones$columna[[1L]])) "tabla" else acciones$columna[[1L]]
    ))))
    cantidades <- acciones$n_afectadas[is.finite(acciones$n_afectadas)]
    cantidad <- if (length(cantidades)) max(cantidades) else NA_real_
    unidades <- unique(as.character(acciones$unidad_conteo))
    unidades <- unidades[!is.na(unidades) & nzchar(unidades)]
    unidad <- if (length(unidades)) {
      paste0(" (unidad: ", paste(unidades, collapse = ", "), ")")
    } else {
      ""
    }
    cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0("Cantidad estimada: ", cantidad, unidad))))
    if (length(ejemplos)) {
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste(
        "Ejemplos reales:", paste(ejemplos, collapse = "; ")
      ))))
    }
    for (k in elegibles) {
      marca <- if (acciones$recomendada[[k]]) " (Recomendado)" else ""
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0(
        k, ". ", acciones$estrategia[[k]], marca, " -- ",
        acciones$justificacion[[k]]
      ))))
    }
    bloqueadas <- which(as.character(acciones$estado) == "bloqueada")
    for (k in bloqueadas) {
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0(
        "[bloqueada] ", acciones$estrategia[[k]], " -- ",
        acciones$justificacion[[k]]
      ))))
    }
    explicacion_no_hacer <- if (no_hacer_recomendado) {
      paste0(
        "No hacer nada conserva los datos y es lo recomendado porque el ",
        "hallazgo no justifica por s\u00ed solo una eliminaci\u00f3n."
      )
    } else {
      "No hacer nada conserva los datos y registra la omisi\u00f3n."
    }
    cli::cli_text(.cli_literal(.marcar_para_exhibir(
      paste0(etiqueta_no_hacer, " -- ", explicacion_no_hacer)
    )))

    decision <- list(
      grupo = grupo, acciones = acciones, elegibles = elegibles,
      ejemplos = ejemplos,
      opciones = c(etiquetas, etiqueta_no_hacer)
    )
    respuesta <- if (is.null(selector)) {
      utils::menu(decision$opciones, title = paste("Seleccione para", grupo))
    } else {
      selector(decision)
    }
    elegida_local <- .resolver_seleccion_guiada(respuesta, acciones, elegibles)
    plan$aplicar[indices] <- FALSE
    if (is.na(elegida_local)) {
      plan$decision_grupo[indices] <- "omitida"
    } else {
      plan$aplicar[indices[[elegida_local]]] <- TRUE
      plan$decision_grupo[indices] <- "elegida"
    }
  }
  plan
}

#' @export
print.plan_limpieza <- function(x, ...) {
  .validar_objeto_lupa(x, "plan_limpieza", character(), "planificar_limpieza()")
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Plan de limpieza")
  cli::cli_alert_success(.cli_literal(paste(sum(x$aplicar), "acciones activadas")))
  cli::cli_alert_info(.cli_literal(paste(sum(!x$aplicar), "acciones desactivadas")))
  n_destructivas <- sum(x$aplicar & x$destructiva)
  n_eliminatorias <- sum(
    x$aplicar & x$destructiva &
      x$estrategia %in% .estrategias_eliminatorias()
  )
  if (n_destructivas) {
    cli::cli_alert_danger(.cli_literal(paste(
      n_destructivas,
      "acciones destructivas activas; revise la p\u00e9rdida declarada"
    )))
  }
  if (n_eliminatorias) {
    cli::cli_alert_danger(.cli_literal(paste(
      n_eliminatorias,
      "acciones eliminatorias activas; requieren un segundo consentimiento"
    )))
  }
  # Lo que no se evaluo se dice aca, no solo en el perfil. Un plan que enumera
  # acciones sin avisar que sobre tal columna no se miro invita a leerlo como si
  # lo demas estuviera bien.
  cobertura <- attr(x, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    columnas <- unique(as.character(cobertura$columna))
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(cobertura),
      if (nrow(cobertura) == 1L) {
        " diagn\u00f3stico no se evalu\u00f3"
      } else {
        " diagn\u00f3sticos no se evaluaron"
      },
      " y por eso no hay acci\u00f3n para ellos, en: ",
      # Los nombres se rinden ANTES de pegarlos: `paste()` marca `bytes` la
      # frase entera si un nombre lo esta, y la frase del paquete salia
      # `diagn\xc3\xb3sticos`, escapada junto con el nombre.
      paste(.texto_publicable(columnas), collapse = ", "),
      ". El motivo medido de cada uno esta en ",
      "`attr(plan, \"cobertura_diagnosticos\")`."
    )))
  }
  sin_accion <- attr(x, "hallazgos_sin_accion_por_columna_ambigua",
                     exact = TRUE)
  if (inherits(sin_accion, "data.frame") && nrow(sin_accion)) {
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(sin_accion),
      if (nrow(sin_accion) == 1L) {
        " hallazgo medido no tiene acci\u00f3n"
      } else {
        " hallazgos medidos no tienen acci\u00f3n"
      },
      " porque su columna comparte nombre con otra, en: ",
      paste(.texto_publicable(unique(as.character(sin_accion$columna))),
            collapse = ", "),
      ". El detalle esta en ",
      "`attr(plan, \"hallazgos_sin_accion_por_columna_ambigua\")`."
    )))
  }
  vista <- x[c(
    "id_accion", "grupo", "columna", "estrategia", "decision_grupo",
    "n_afectadas", "unidad_conteo", "orden", "estado", "recomendada",
    "destructiva", "aplicar"
  )]
  .print_data_frame_bytes(vista, row.names = FALSE)
  # Lo que esta impresion oculta es justamente `evidencia` y `justificacion`,
  # que son lo que se lee para decidir si aplicar una accion. Un recorte que no
  # se declara se lee como la tabla entera.
  .avisar_campos_no_mostrados(vista, x, "as.data.frame(plan)")
  invisible(original)
}

#' @export
print.resultado_limpieza <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Resultado de limpieza")
  ejecutadas <- if ("estado" %in% names(x$registro)) {
    sum(x$registro$estado == "ejecutada")
  } else {
    nrow(x$registro)
  }
  fallidas <- if ("estado" %in% names(x$registro)) {
    sum(x$registro$estado == "fallida")
  } else {
    0L
  }
  cli::cli_alert_success(.cli_literal(paste(ejecutadas, "acciones ejecutadas")))
  if (fallidas) {
    cli::cli_alert_danger(.cli_literal(paste(fallidas, "acciones fallidas; revise `registro$error`")))
  }
  cli::cli_text(.cli_literal(paste(sum(x$registro$n_cambiadas), "celdas o marcas afectadas")))
  n_filas <- sum(x$registro$n_filas_eliminadas)
  n_columnas <- sum(x$registro$n_columnas_eliminadas)
  if (n_filas || n_columnas) {
    cli::cli_alert_warning(.cli_literal(paste(
      n_filas, "filas y", n_columnas, "columnas eliminadas"
    )))
  }
  invisible(original)
}
