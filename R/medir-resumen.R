# Medicion resumida: `medir(detalle = "resumen")`, clase `medicion_resumen` y
# adaptadores de consumidores.
#
# Clase propia `c("medicion_resumen", "data.frame")` sin herencia de `medicion`:
# todo consumidor que no la conoce falla cerrado en lugar de leer una fila por
# metrica como una fila por celda.

.COLUMNAS_RESUMEN <- c(
  "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
  "metrica_instanciada", "dimension", "factor", "orientacion", "granularidad",
  "granularidad_origen", "tipo_resultado", "entidad", "atributo",
  "objeto_medible", "resultado", "agregacion", "numerador", "denominador",
  "n_universo", "n_no_evaluados", "motivo_no_evaluados", "unidad_conteo",
  .COLUMNAS_CUENTAS_RESUMEN
)

.TIPOS_NO_ACOTADOS_RESUMEN <- c("duracion", "entero", "numero_real")

.es_medicion_resumen <- function(x) {
  inherits(x, "medicion_resumen")
}

.rechazar_resumen <- function(funcion, porque, que_hacer) {
  .detener(structure(
    class = c("lupa_resumen_no_admitido", "error", "condition"),
    list(
      message = paste0(
        "`", funcion, "()` no acepta un resumen de medici\u00f3n: ",
        porque, " ", que_hacer
      ),
      call = NULL
    )
  ))
}

.resumen_vacio <- function() {
  .data_frame_simple(
    id_medida = character(),
    id_medicion = character(),
    fecha = as.POSIXct(character(), tz = "UTC"),
    metrica = character(),
    metrica_especifica = character(),
    metrica_instanciada = character(),
    dimension = character(),
    factor = character(),
    orientacion = character(),
    granularidad = character(),
    granularidad_origen = character(),
    tipo_resultado = character(),
    entidad = character(),
    atributo = character(),
    objeto_medible = character(),
    resultado = numeric(),
    agregacion = character(),
    numerador = numeric(),
    denominador = numeric(),
    n_universo = numeric(),
    n_no_evaluados = numeric(),
    motivo_no_evaluados = character(),
    unidad_conteo = character(),
    n_sin_valor = numeric(),
    n_filas_leidas = numeric(),
    entidad_referida = character(),
    n_filas_leidas_referida = numeric(),
    estado_cuentas = character(),
    motivo_cuentas = character(),
    diferencia_cuentas = numeric(),
    stringsAsFactors = FALSE
  )
}

.fila_resumen <- function(instancia, id_medicion, fecha, entidad, atributo,
                          objeto, resultado, agregacion, tipo_resultado,
                          numerador, denominador, n_universo,
                          n_no_evaluados, motivo_no_evaluados,
                          n_sin_valor = NA_real_, unidad = NULL,
                          entidad_referida = NA_character_) {
  gran <- as.character(instancia$declaracion$granularidad)[[1L]]
  destino <- .destino_tablero(gran)
  if (is.null(unidad)) {
    unidad <- if (identical(gran, "instanciaAtributo")) "celda" else "fila"
  }
  .data_frame_simple(
    id_medida = "",
    id_medicion = as.character(id_medicion),
    fecha = as.POSIXct(fecha),
    metrica = instancia$declaracion$nombre,
    metrica_especifica = instancia$nombre_especifico,
    metrica_instanciada = instancia$nombre,
    dimension = instancia$declaracion$dimension %||% NA_character_,
    factor = instancia$declaracion$factor %||% NA_character_,
    orientacion = instancia$declaracion$orientacion,
    granularidad = destino,
    granularidad_origen = gran,
    tipo_resultado = tipo_resultado,
    entidad = entidad,
    atributo = if (identical(destino, "atributo")) atributo else NA_character_,
    objeto_medible = objeto,
    resultado = as.double(resultado),
    agregacion = agregacion,
    numerador = as.double(numerador),
    denominador = as.double(denominador),
    n_universo = as.double(n_universo),
    n_no_evaluados = as.double(n_no_evaluados),
    motivo_no_evaluados = motivo_no_evaluados,
    unidad_conteo = unidad,
    n_sin_valor = as.double(n_sin_valor),
    n_filas_leidas = NA_real_,
    entidad_referida = as.character(entidad_referida),
    n_filas_leidas_referida = NA_real_,
    estado_cuentas = NA_character_,
    motivo_cuentas = NA_character_,
    diferencia_cuentas = NA_real_,
    stringsAsFactors = FALSE
  )
}

.universo_instancia_resumen <- function(tablas_instancia, instancia, n_medidas,
                                        mascara_aplicable = NULL) {
  vacio <- list(n = NA_real_, no = NA_real_, motivo = NA_character_,
                sv = NA_real_, unidad = NULL)
  tabla <- tablas_instancia[[instancia$entidad[[1L]]]]
  if (!inherits(tabla, "data.frame")) return(vacio)
  dentro <- tryCatch(
    .mascara_aplicable_instancia(
      tabla, instancia, mascara = mascara_aplicable
    ),
    error = function(e) NULL
  )
  if (is.null(dentro)) return(vacio)
  # La unidad la declara la metrica: `CorrectitudSemDebil` es de atributo y mide
  # la fila del par, y multiplicar por los atributos le daba un universo de 20
  # para 10 filas.
  unidad <- .unidad_universo_instancia(instancia)
  esperadas <- if (identical(unidad, "celda")) {
    sum(dentro) * length(instancia$atributos)
  } else {
    sum(dentro)
  }
  # Los vacios se cuentan siempre -tambien cuando no falta ninguna medida- y
  # sobre las columnas de las que sale el valor, no solo sobre los atributos:
  # el motor los cuenta en la misma consulta y las dos vias tienen que publicar
  # lo mismo.
  sv <- .sin_valor_instancia(tabla, instancia, dentro, unidad)
  ne <- .no_evaluados_resumen(as.double(esperadas), as.double(n_medidas), sv)
  list(
    n = as.double(esperadas), no = ne$no, motivo = ne$motivo, sv = sv,
    unidad = unidad
  )
}

.formula_aplicabilidad_literal <- function(mascara) {
  elementos <- lapply(as.logical(mascara), function(x) {
    if (is.na(x)) quote(NA)
    else if (isTRUE(x)) quote(TRUE)
    else quote(FALSE)
  })
  expr <- as.call(c(list(as.name("c")), elementos))
  stats::as.formula(call("~", expr), env = baseenv())
}

.aplicabilidad_instancia_resuelta <- function(aplicables, instancia) {
  entidad <- instancia$entidad[[1L]]
  mascaras <- aplicables[[entidad]]
  if (is.null(mascaras)) return(NULL)
  atributos <- as.character(unlist(instancia[["atributos"]]))
  reglas <- lapply(atributos, function(atributo) {
    indice <- .indice_nombre(atributo, names(mascaras))
    if (is.na(indice) || is.null(mascaras[[indice]])) return(NULL)
    .formula_aplicabilidad_literal(mascaras[[indice]])
  })
  presentes <- !vapply(reglas, is.null, logical(1L))
  nombres <- atributos[presentes]
  reglas <- reglas[presentes]
  if (!length(reglas)) return(NULL)
  stats::setNames(reglas, nombres)
}

.resumen_especificado <- function(instancia, tablas_instancia, id_medicion,
                                  fecha, mascara_aplicable = NULL) {
  spec <- .especificacion_metrica(instancia)
  if (is.null(spec) || identical(spec$nodo, "abstencion")) return(NULL)
  avisos <- list()
  ev <- tryCatch(
    withCallingHandlers(
      .evaluar_especificacion(
        spec, tablas_instancia, instancia,
        mascara_aplicable = mascara_aplicable
      ),
      warning = function(w) {
        avisos[[length(avisos) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (is.null(ev)) return(NULL)
  if (identical(spec$nodo, "cobertura_referencial")) {
    if (isTRUE(ev$vacia) || is.na(ev$cobertura)) return(NULL)
    dep <- tablas_instancia[[spec$entidad_dep]]
    n_univ <- nrow(dep)
    sin_valor <- as.double(sum(is.na(dep[[spec$fk]])))
    ne <- .no_evaluados_resumen(as.double(n_univ), as.double(ev$denominador), sin_valor)
    fila <- .fila_resumen(
      instancia, id_medicion, fecha, spec$entidad_dep, spec$fk,
      .pegar_texto0(
        spec$entidad_dep, "$", spec$fk, " -> ",
        spec$entidad_ref, "$", spec$pk
      ),
      resultado = ev$cobertura,
      agregacion = "promedio",
      tipo_resultado = "real",
      numerador = ev$numerador,
      denominador = ev$denominador,
      n_universo = n_univ,
      n_no_evaluados = ne$no,
      motivo_no_evaluados = ne$motivo,
      n_sin_valor = sin_valor,
      entidad_referida = spec$entidad_ref
    )
    attr(fila, "avisos_resumen") <- avisos
    return(fila)
  }
  n <- length(ev$cumple)
  if (!n) return(NULL)
  universo <- .universo_instancia_resumen(
    tablas_instancia, instancia, n,
    mascara_aplicable = mascara_aplicable
  )
  k <- sum(ev$cumple)
  atributo <- if (identical(spec$objeto, "celda")) {
    spec$columna %||% spec$columnas
  } else {
    NA_character_
  }
  objeto <- if (identical(spec$objeto, "celda")) {
    .pegar_texto0(spec$entidad, "$", atributo)
  } else {
    spec$entidad
  }
  fila <- .fila_resumen(
    instancia, id_medicion, fecha, spec$entidad, atributo, objeto,
    resultado = k / n,
    agregacion = "ratio",
    tipo_resultado = "real",
    numerador = k,
    denominador = n,
    n_universo = universo$n,
    n_no_evaluados = universo$no,
    motivo_no_evaluados = universo$motivo,
    n_sin_valor = universo$sv,
    unidad = universo$unidad
  )
  attr(fila, "avisos_resumen") <- avisos
  fila
}

.resumen_general <- function(instancia, medidas, tablas_instancia,
                             mascara_aplicable = NULL, alcance = NULL) {
  validadas <- .validar_medidas_tablero(medidas)
  gran <- unique(validadas$granularidad)[[1L]]
  tipo <- unique(validadas$tipo_resultado)[[1L]]
  if (gran %in% c("instanciaAtributo", "instanciaEntidad") &&
      tipo %in% .TIPOS_NO_ACOTADOS_RESUMEN) {
    return(list(no_resumible = TRUE))
  }
  configuracion <- .configuracion_agregaciones(validadas, NULL, NULL)
  agregada <- .agregar_medidas_tablero(
    validadas, configuracion, varias_entidades = FALSE
  )
  grupos <- split(
    seq_len(nrow(validadas)), .claves_objeto_tablero(validadas), drop = TRUE
  )
  es_instancia <- gran %in% c("instanciaAtributo", "instanciaEntidad")
  booleana <- identical(tipo, "booleano")
  # La cobertura de un referencial es una proporcion de claves: su metodo
  # publica las cubiertas y el universo, y el resumen las lleva como numerador y
  # denominador, igual que `medir_dbi()`.
  conteos_entidad <- if (!es_instancia && is.list(alcance) &&
                         is.numeric(alcance$n_cubiertas) &&
                         is.numeric(alcance$n_referencial)) {
    list(num = alcance$n_cubiertas, den = alcance$n_referencial)
  }
  filas <- lapply(seq_along(grupos), function(g) {
    i <- grupos[[g]]
    valores <- validadas$resultado[i]
    a <- agregada[g, , drop = FALSE]
    n <- length(valores)
    universo <- if (es_instancia) {
      .universo_instancia_resumen(
        tablas_instancia, instancia, n,
        mascara_aplicable = mascara_aplicable
      )
    } else {
      list(n = NA_real_, no = NA_real_, motivo = NA_character_,
           sv = NA_real_, unidad = NULL)
    }
    .fila_resumen(
      instancia, a$id_medicion, a$fecha, a$entidad, a$atributo,
      objeto = if (identical(a$granularidad, "atributo")) {
        .pegar_texto0(a$entidad, "$", a$atributo)
      } else if (identical(a$granularidad, "entidad") && es_instancia) {
        a$entidad
      } else {
        validadas$objeto_medible[i[[1L]]]
      },
      resultado = if (booleana && es_instancia) sum(valores == 1) / n else
        a$resultado,
      agregacion = a$agregacion,
      tipo_resultado = a$tipo_resultado,
      numerador = if (booleana && es_instancia) {
        sum(valores == 1)
      } else conteos_entidad$num %||% NA_real_,
      denominador = if (es_instancia) n else conteos_entidad$den %||% NA_real_,
      n_universo = universo$n,
      n_no_evaluados = universo$no,
      motivo_no_evaluados = universo$motivo,
      n_sin_valor = universo$sv,
      unidad = universo$unidad
    )
  })
  list(filas = do.call(rbind, filas))
}

.cobertura_no_resumible <- function(instancia, id_medicion, fecha) {
  .data_frame_simple(
    id_medicion = as.character(id_medicion),
    fecha = as.POSIXct(fecha),
    metrica = instancia$declaracion$nombre,
    metrica_especifica = instancia$nombre_especifico,
    metrica_instanciada = instancia$nombre,
    entidad = paste(instancia$entidad, collapse = "+"),
    atributo = paste(instancia$atributos, collapse = "+"),
    estado = "no_resumible",
    motivo = paste0(
      "La m\u00e9trica `", instancia$nombre, "` se midi\u00f3, pero da un valor `",
      instancia$declaracion$tipo_resultado, "` por ",
      if (identical(instancia$declaracion$granularidad, "instanciaAtributo"))
        "celda" else "fila",
      ", que no es una proporci\u00f3n: el resumen no lo promedia, como no lo ",
      "hace `agregar()`."
    ),
    como_resolverlo = paste(
      "Medir esa m\u00e9trica con `detalle = \"completo\"`. Una m\u00e9trica fuera",
      "del resumen no se interpreta como cero."
    ),
    stringsAsFactors = FALSE
  )
}

.medir_resumen <- function(modelo, datos, id_medicion, fecha, fecha_declarada,
                           aplicabilidad, proteger_datos_personales,
                           columnas_personales, validadores_personales,
                           orden_texto = "idioma", interno_bloque = FALSE) {
  if (!inherits(modelo, "modelo_calidad")) {
    .detener("`modelo` debe provenir de modelo().", call. = FALSE)
  }
  fecha <- .fecha_argumento_utc(fecha)
  .validar_bandera_proteccion(proteger_datos_personales)
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales, .nombres_declarables_medicion(datos)
  )
  validadores_personales <- .normalizar_validadores_personales(
    validadores_personales
  )
  if (is.null(id_medicion)) {
    id_medicion <- .nuevo_id_medicion(fecha)
  }
  if (!.es_texto_escalar(id_medicion)) {
    .detener("`id_medicion` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  tablas <- .normalizar_tablas_modelo(datos, modelo)
  aplicables <- .mascaras_aplicabilidad_medicion(tablas, aplicabilidad)
  filas <- list()
  coberturas <- list()
  alcances <- list()
  alcances_m <- list()
  for (instancia in modelo$metricas) {
    instancia <- .con_orden_texto(instancia, orden_texto)
    tablas_instancia <- .recortar_tablas_aplicables(tablas, aplicables, instancia)
    mascara_instancia <- tryCatch(
      .mascara_aplicable_instancia(
        tablas_instancia[[instancia$entidad[[1L]]]], instancia
      ),
      error = function(e) NULL
    )
    rapida <- .resumen_especificado(
      instancia, tablas_instancia, id_medicion, fecha,
      mascara_aplicable = mascara_instancia
    )
    if (!is.null(rapida)) {
      avisos <- attr(rapida, "avisos_resumen", exact = TRUE)
      attr(rapida, "avisos_resumen") <- NULL
      filas[[length(filas) + 1L]] <- .cuentas_filas_memoria(
        rapida, instancia, tablas
      )
      parcial <- if (rapida$granularidad_origen %in%
                     c("instanciaAtributo", "instanciaEntidad")) {
        .alcance_medidas_instancia(
          tablas_instancia, instancia, rapida$denominador,
          mascara_aplicable = mascara_instancia
        )
      } else NULL
      if (!is.null(parcial)) alcances[[length(alcances) + 1L]] <- parcial
      for (w in avisos) .avisar(w)
      next
    }
    sola <- modelo(list(instancia), marco = modelo$marco)
    aplicabilidad_sola <- .aplicabilidad_instancia_resuelta(
      aplicables, instancia
    )
    medidas <- withCallingHandlers(
      medir(
        sola, datos, id_medicion = id_medicion, fecha = fecha,
        aplicabilidad = aplicabilidad_sola,
        proteger_datos_personales = proteger_datos_personales,
        columnas_personales = columnas_personales,
        validadores_personales = validadores_personales,
        detalle = "completo", orden_texto = orden_texto
      ),
      warning = function(w) {
        if (grepl("porque su m", conditionMessage(w),
                  fixed = TRUE, useBytes = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    cobertura <- attr(medidas, "cobertura_metricas", exact = TRUE)
    if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
      coberturas[[length(coberturas) + 1L]] <- cobertura
    }
    alcance <- attr(medidas, "alcance_medidas", exact = TRUE)
    if (inherits(alcance, "data.frame") && nrow(alcance)) {
      alcances[[length(alcances) + 1L]] <- alcance
    }
    am <- attr(medidas, "alcance_metricas", exact = TRUE)
    if (length(am)) alcances_m <- c(alcances_m, am)
    if (!nrow(medidas)) next
    general <- .resumen_general(
      instancia, medidas, tablas_instancia,
      mascara_aplicable = mascara_instancia,
      alcance = if (length(am)) am[[1L]]
    )
    if (isTRUE(general$no_resumible)) {
      coberturas[[length(coberturas) + 1L]] <- .cobertura_no_resumible(
        instancia, id_medicion, fecha
      )
      next
    }
    filas[[length(filas) + 1L]] <- .cuentas_filas_memoria(
      general$filas, instancia, tablas,
      parcial = if (isTRUE(am[[instancia$nombre]]$truncado)) "truncada" else NA_character_
    )
  }
  resultado <- if (length(filas)) do.call(rbind, filas) else .resumen_vacio()
  rownames(resultado) <- NULL
  if (nrow(resultado)) {
    resultado$id_medida <- .ids_medida(
      resultado$id_medicion, resultado$metrica_instanciada, "resumen-"
    )
  }
  resultado <- resultado[.COLUMNAS_RESUMEN]
  lecturas <- .lecturas_memoria(
    resultado, tablas, id_medicion, huella = !isTRUE(interno_bloque)
  )
  resultado <- .estado_cuentas_lecturas(resultado, lecturas)
  attr(resultado, "lecturas") <- lecturas
  if (!isTRUE(interno_bloque)) .avisar_cuentas_resumen(resultado)
  if (length(coberturas)) {
    cobertura <- do.call(rbind, lapply(coberturas, .ubicar_cobertura_metricas, modelo))
    rownames(cobertura) <- NULL
    attr(resultado, "cobertura_metricas") <- cobertura
    .avisar_no_medibles(
      cobertura[cobertura$estado != "no_resumible", , drop = FALSE],
      length(modelo$metricas)
    )
  }
  if (length(alcances)) {
    unidos <- do.call(rbind, alcances)
    rownames(unidos) <- NULL
    attr(resultado, "alcance_medidas") <- unidos
  }
  if (length(alcances_m)) attr(resultado, "alcance_metricas") <- alcances_m
  attr(resultado, "fecha_declarada") <- fecha_declarada
  attr(resultado, "configuracion_modelo") <- .configuracion_modelo_calidad(modelo)
  if (inherits(modelo$marco, "marco_calidad")) {
    attr(resultado, "marco_calidad") <- modelo$marco
  }
  attr(resultado, "configuracion_aplicabilidad") <-
    .texto_configuracion_calidad(aplicabilidad)
  attr(resultado, "sello_metricas") <- .sello_metricas(
    modelo, id_medicion, aplicabilidad, orden_texto,
    .lector_tablas(tablas)
  )
  attr(resultado, "detalle") <- "resumen"
  if (!isTRUE(proteger_datos_personales) && nrow(resultado)) {
    personales <- unique(unlist(lapply(tablas, function(tabla) {
      if (!inherits(tabla, "data.frame") || !nrow(tabla)) return(character())
      tryCatch(
        .columnas_personales_rapidas(
          tabla, declaradas = columnas_personales,
          validadores = validadores_personales
        ),
        error = function(e) character()
      )
    }), use.names = FALSE))
    if (length(personales)) {
      attr(resultado, "datos_personales_sin_proteger") <- personales
    }
  }
  class(resultado) <- c("medicion_resumen", "data.frame")
  resultado
}

# Reconstruye la tabla agregada que `.agregar_medidas_tablero()` entrega a
# `.armar_tablero()` a partir de un `medicion_resumen`.
.agregada_tablero_desde_resumen <- function(resumen) {
  x <- as.data.frame(unclass(resumen), stringsAsFactors = FALSE)
  n <- nrow(x)
  varias <- length(.identificadores_unicos(x$entidad)) > 1L
  objeto <- if (n) {
    vapply(seq_len(n), function(i) {
      .objeto_tablero(x, i, x$granularidad[[i]], varias)
    }, character(1L))
  } else {
    character()
  }
  universo <- if (n) {
    vapply(x$granularidad_origen, .universo_tablero, character(1L),
           USE.NAMES = FALSE)
  } else {
    character()
  }
  agregada <- .data_frame_simple(
    id_medida = rep("", n),
    id_medicion = x$id_medicion,
    fecha = x$fecha,
    metrica = x$metrica,
    metrica_especifica = x$metrica_especifica,
    metrica_instanciada = x$metrica_instanciada,
    dimension = x$dimension,
    factor = x$factor,
    orientacion = x$orientacion,
    granularidad = x$granularidad,
    tipo_resultado = x$tipo_resultado,
    entidad = x$entidad,
    atributo = x$atributo,
    fila = rep(NA_integer_, n),
    objeto_medible = objeto,
    resultado = x$resultado,
    agregacion = x$agregacion,
    umbral_tablero = rep(NA_real_, n),
    universo_tablero = universo,
    stringsAsFactors = FALSE
  )
  if (n) {
    agregada$id_medida <- .ids_medida(
      agregada$id_medicion, agregada$metrica_instanciada, "tablero-"
    )
  }
  class(agregada) <- c("medicion", "data.frame")
  agregada
}

.tablero_desde_resumen <- function(medidas, agregaciones = NULL,
                                   umbrales = NULL, marco = NULL,
                                   cobertura = NULL, declarar_fuera = FALSE) {
  cobertura_metricas <- attr(medidas, "cobertura_metricas", exact = TRUE)
  # El semaforo deja afuera, diciendolo, lo que el tablero rechaza: una metrica
  # no acotada no tumba la lectura de las demas.
  fuera <- if (isTRUE(declarar_fuera)) {
    character()
  } else if (inherits(cobertura_metricas, "data.frame") && nrow(cobertura_metricas)) {
    cobertura_metricas$metrica_instanciada[
      cobertura_metricas$estado == "no_resumible"
    ]
  } else {
    character()
  }
  if (length(fuera)) {
    .rechazar_resumen(
      "tablero_calidad",
      paste0(
        "el tablero de la medici\u00f3n completa publica el promedio de ",
        paste0("`", fuera, "`", collapse = ", "),
        ", que el resumen no calcula (valores no acotados)."
      ),
      "Medir esas m\u00e9tricas con `detalle = \"completo\"` o quitarlas del modelo."
    )
  }
  if (!is.null(agregaciones) || !is.null(umbrales)) {
    .rechazar_resumen(
      "tablero_calidad",
      "otra agregaci\u00f3n que la del resumen necesita las medidas una por una.",
      "Para elegir la agregaci\u00f3n, medir con `detalle = \"completo\"`."
    )
  }
  if (!nrow(medidas)) {
    .error_medicion_sin_medidas(medidas, "medidas", "`medir()` o `agregar()`")
  }
  agregada <- .agregada_tablero_desde_resumen(medidas)
  agregada <- .validar_medidas_tablero(agregada)
  agregada <- .trasladar_atributos_medicion(agregada, medidas)
  .con_cuentas_y_consolidacion(
    .con_coberturas_frontera(
      .armar_tablero(
        agregada, agregada, marco = marco, cobertura = cobertura,
        cobertura_metricas = cobertura_metricas,
        alcance_medidas = attr(medidas, "alcance_medidas", exact = TRUE),
        desenlaces = .desenlaces_de_objeto(medidas)
      )$tablero,
      medidas
    ),
    medidas
  )
}

#' @export
`[.medicion_resumen` <- function(x, ...) {
  resultado <- NextMethod("[")
  resultado <- .filtrar_descartadas(resultado, x)
  .conservar_atributos_objeto(
    resultado, x,
    por_corrida = c("cobertura_metricas", "alcance_medidas", "sello_metricas",
                    "lecturas")
  )
}

#' @rdname medir
#' @export
rbind.medicion_resumen <- function(..., prioridad = NULL, deparse.level = 1) {
  partes <- list(...)
  if (!all(vapply(partes, .es_medicion_resumen, logical(1L)))) {
    .rechazar_resumen(
      "rbind",
      "un resumen y una medici\u00f3n por celda son niveles distintos.",
      "Unir res\u00famenes entre s\u00ed o mediciones completas entre s\u00ed."
    )
  }
  # Una consolidacion no se vuelve a unir: lo que descarto y la prioridad con
  # la que eligio no tienen como componerse con otra.
  if (any(vapply(partes, function(x) {
    !is.null(attr(x, "consolidacion", exact = TRUE))
  }, logical(1L)))) {
    .rechazar_resumen(
      "rbind",
      "una de las partes ya es una consolidaci\u00f3n, y sus descartadas no se componen con otra.",
      "Consolidar todas las partes de una vez."
    )
  }
  if (!is.null(prioridad)) return(.consolidar_resumenes(partes, prioridad))
  if (length(partes) == 1L) return(partes[[1L]])
  mismo <- function(atributo) {
    valores <- lapply(partes, attr, which = atributo, exact = TRUE)
    all(vapply(valores[-1L], identical, logical(1L), valores[[1L]]))
  }
  if (!mismo("configuracion_aplicabilidad") ||
      !mismo("marco_calidad") ||
      !mismo("fecha_declarada")) {
    .rechazar_resumen(
      "rbind",
      paste(
        "los res\u00famenes provienen de configuraciones de aplicabilidad,",
        "marco o fecha declarada distintas y unirlos perder\u00eda ese contexto."
      ),
      "Medir las m\u00e9tricas bajo la misma aplicabilidad y configuraci\u00f3n."
    )
  }
  ids_parte <- function(p) {
    ids <- unique(as.character(p$id_medicion))
    if (!length(ids)) {
      cob <- attr(p, "cobertura_metricas", exact = TRUE)
      if (inherits(cob, "data.frame") && "id_medicion" %in% names(cob)) {
        ids <- unique(as.character(cob$id_medicion))
      }
    }
    .sort_por_bytes(ids)
  }
  identificadores <- lapply(partes, ids_parte)
  misma_corrida <- all(vapply(
    identificadores[-1L], identical, logical(1L), identificadores[[1L]]
  ))
  config_modelo <- attr(partes[[1L]], "configuracion_modelo", exact = TRUE)
  if (!mismo("configuracion_modelo")) {
    union_modelo <- if (misma_corrida) {
      .unir_configuraciones_modelo(lapply(
        partes, attr, which = "configuracion_modelo", exact = TRUE
      ))
    } else NULL
    if (is.null(union_modelo)) {
      .rechazar_resumen(
        "rbind",
        paste(
          "los res\u00famenes provienen de modelos o corridas incompatibles;",
          "un resumen conserva una sola configuraci\u00f3n de modelo."
        ),
        "Unir partes de la misma corrida o medir el modelo conjunto de una vez."
      )
    }
    config_modelo <- union_modelo
  }
  unidas <- do.call(
    rbind.data.frame,
    c(lapply(partes, function(p) {
      as.data.frame(unclass(p), stringsAsFactors = FALSE)
    }), list(stringsAsFactors = FALSE))
  )
  # Una corrida es un `id_medicion` y una fecha: dos partes con el mismo id y
  # otra fecha se unian, y el tablero rechazaba despues el resultado.
  .validar_fecha_partes(partes)
  .validar_cobertura_partes(partes, unidas)
  clave <- paste(
    unidas$id_medicion, unidas$metrica_instanciada, unidas$entidad,
    unidas$atributo, sep = "\r"
  )
  if (anyDuplicated(clave)) {
    .rechazar_resumen(
      "rbind",
      paste(
        "la misma m\u00e9trica sobre el mismo objeto aparece dos veces en la",
        "misma corrida; sumar sus cuentas supondr\u00eda que miden filas distintas."
      ),
      "Medir la tabla entera de una vez, o dar a cada parte su `id_medicion`."
    )
  }
  rownames(unidas) <- NULL
  attr(unidas, "configuracion_modelo") <- config_modelo
  for (a in c("configuracion_aplicabilidad", "marco_calidad",
              "fecha_declarada", "detalle")) {
    attr(unidas, a) <- attr(partes[[1L]], a, exact = TRUE)
  }
  for (a in c("cobertura_metricas", "alcance_medidas")) {
    tablas <- Filter(
      function(t) inherits(t, "data.frame") && nrow(t),
      lapply(partes, attr, which = a, exact = TRUE)
    )
    if (length(tablas)) {
      tabla <- unique(do.call(rbind.data.frame, tablas))
      rownames(tabla) <- NULL
      attr(unidas, a) <- tabla
    }
  }
  alcances_m <- unlist(
    lapply(partes, attr, which = "alcance_metricas", exact = TRUE),
    recursive = FALSE
  )
  if (length(alcances_m)) {
    attr(unidas, "alcance_metricas") <-
      alcances_m[!duplicated(names(alcances_m))]
  }
  marca <- unique(unlist(lapply(partes, function(p) {
    attr(p, "datos_personales_sin_proteger", exact = TRUE)
  }), use.names = FALSE))
  if (length(marca)) {
    attr(unidas, "datos_personales_sin_proteger") <- marca
  }
  sello <- .unir_sellos_mediciones(partes, "rbind()")
  if (!is.null(sello)) attr(unidas, "sello_metricas") <- sello
  # Las partes pudieron leer la tabla en momentos distintos: C5 se vuelve a
  # mirar sobre la union. Nunca pasa una fila que no cerraba a cerrar.
  lecturas <- .lecturas_de_partes(partes)
  antes <- sum(unidas$estado_cuentas %in% "no_cierran")
  unidas <- .estado_cuentas_lecturas(unidas, lecturas)
  attr(unidas, "lecturas") <- lecturas
  class(unidas) <- c("medicion_resumen", "data.frame")
  if (sum(unidas$estado_cuentas %in% "no_cierran") > antes) {
    .avisar_cuentas_resumen(unidas, "rbind()")
  }
  unidas
}

#' @export
print.medicion_resumen <- function(x, ...) {
  visible <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Resumen de medici\u00f3n")
  n <- nrow(visible)
  cli::cli_alert_info(.cli_literal(paste0(
    n, if (n == 1L) " m\u00e9trica resumida" else " m\u00e9tricas resumidas"
  )))
  cols_mostrar <- c(
    "metrica_instanciada", "entidad", "atributo", "resultado",
    "numerador", "denominador", "n_no_evaluados", "motivo_no_evaluados"
  )
  tabla <- as.data.frame(unclass(visible), stringsAsFactors = FALSE)
  if (all(cols_mostrar %in% names(tabla))) {
    tabla <- tabla[c(cols_mostrar, intersect(c("estado_cuentas", "fuente"), names(tabla)))]
  }
  .print_data_frame_bytes(tabla, row.names = FALSE, ...)
  malas <- .cuentas_que_no_cierran(x)
  if (!is.null(malas)) {
    cli::cli_h2("Cuentas que no cierran")
    .print_data_frame_bytes(malas, row.names = FALSE)
  }
  .imprimir_consolidacion(x)
  cob <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cob, "data.frame") && nrow(cob)) {
    cli::cli_h2("Cobertura de m\u00e9tricas")
    cols_cob <- intersect(c("metrica_instanciada", "estado", "motivo"), names(cob))
    .print_data_frame_bytes(
      .seleccionar_columnas(cob, cols_cob),
      row.names = FALSE
    )
  }
  invisible(x)
}
