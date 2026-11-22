# Partes de una corrida medidas por instrumentos distintos, con una prioridad
# declarada: `rbind(motor = a, memoria = b, prioridad = c("motor", "memoria"))`.
#
# `rbind()` ya une las partes de una corrida -mismo `id_medicion`- cuando no se
# pisan, y rechaza la misma metrica sobre el mismo objeto en dos partes, porque
# elegir una cifra seria elegir por quien llama. Con `prioridad`, quien llama
# elige, y la eleccion queda escrita: cada fila lleva su `fuente` y lo no
# elegido queda en `attr(x, "descartadas")`.
#
# Pero solo se junta lo comparable. Una corrida (el mismo id y la misma fecha
# declarada), la misma definicion de cada metrica (la huella del sello, que
# tiene que ver la definicion entera: `certificacion == "completa"`) y la misma
# instantanea de los datos. El conteo de filas no alcanza para lo ultimo: dos
# tablas distintas con el mismo numero de filas dan cifras distintas con la
# misma huella. La instantanea la prueba el contador de modificaciones del motor
# o, en memoria, la huella de los datos; si no se puede probar, no se junta y se
# dice por que.

.rechazar_consolidacion <- function(porque, que_hacer) {
  .detener(structure(
    class = c("lupa_consolidacion_no_admitida", "lupa_resumen_no_admitido",
              "error", "condition"),
    list(
      message = paste0(
        "`rbind(..., prioridad = )` no junta estas partes: ", porque, " ",
        que_hacer
      ),
      call = NULL
    )
  ))
}

.clave_celda_resumen <- function(x) {
  paste(
    .clave_bytes(x$id_medicion), .clave_bytes(x$metrica_instanciada),
    .clave_bytes(x$entidad), .clave_bytes(x$atributo), sep = "\034"
  )
}

# Si el motor no pudo medir una metrica porque no la traduce -una limitacion del
# instrumento, no un hecho de los datos-. Solo eso deja que otra parte la mida:
# un metodo que fallo o una columna sin valores tendrian que haberse visto igual
# en la otra parte, que leyo la misma instantanea.
.limitacion_instrumento <- function(parte, metrica) {
  vias <- attr(parte, "ejecucion_dbi", exact = TRUE)$vias
  if (is.null(vias) || is.null(names(vias))) return(FALSE)
  i <- .indice_identificador(metrica, names(vias))
  !is.na(i) && identical(unname(vias[[i]]), "no_medible")
}

.validar_prioridad <- function(partes, prioridad) {
  nombres <- names(partes)
  if (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres)) ||
      anyDuplicated(.clave_bytes(nombres))) {
    .rechazar_consolidacion(
      "cada parte tiene que tener un nombre propio, que es su `fuente`.",
      "Nombrarlas en la llamada: `rbind(motor = a, memoria = b, prioridad = ...)`."
    )
  }
  if (!is.character(prioridad) || anyNA(prioridad) ||
      anyDuplicated(.clave_bytes(prioridad)) ||
      !setequal(.clave_bytes(prioridad), .clave_bytes(nombres))) {
    .rechazar_consolidacion(
      paste0(
        "`prioridad` tiene que nombrar, sin repetir, todas las partes (",
        paste(nombres, collapse = ", "), ")."
      ),
      "Declararla de mayor a menor."
    )
  }
  nombres[match(.clave_bytes(prioridad), .clave_bytes(nombres))]
}

.validar_corrida_consolidacion <- function(partes) {
  ids <- unique(unlist(lapply(partes, function(p) {
    c(as.character(p$id_medicion),
      as.character(attr(p, "cobertura_metricas", exact = TRUE)$id_medicion))
  }), use.names = FALSE))
  if (length(ids) != 1L) {
    .rechazar_consolidacion(
      paste0(
        "no son partes de una corrida: traen ", length(ids),
        " `id_medicion` (", paste(ids, collapse = ", "), ")."
      ),
      "Medir las partes con el mismo `id_medicion` y la misma `fecha`."
    )
  }
  fechas <- unique(do.call(c, lapply(partes, function(p) {
    c(p$fecha, attr(p, "cobertura_metricas", exact = TRUE)$fecha)
  })))
  declaradas <- vapply(partes, function(p) {
    isTRUE(attr(p, "fecha_declarada", exact = TRUE))
  }, logical(1L))
  if (length(fechas) != 1L || !all(declaradas)) {
    .rechazar_consolidacion(
      "no comparten una fecha declarada: una corrida es un `id_medicion` y una fecha.",
      "Declarar en las dos mediciones la misma `fecha`, la de la entrega que miden."
    )
  }
  # El marco declarado. Una parte sin marco declarado no choca con una que lo
  # declara: el marco que el tablero deriva de los pares medidos cambia con las
  # metricas de cada parte, y compararlo rechazaba partes con el mismo marco.
  declarados <- Filter(Negate(is.null), lapply(partes, attr, which = "marco_calidad",
                                               exact = TRUE))
  if (length(declarados) > 1L &&
      !all(vapply(declarados[-1L], identical, logical(1L), declarados[[1L]]))) {
    .rechazar_consolidacion(
      "las partes declaran marcos distintos.",
      "Medirlas con el mismo marco."
    )
  }
  invisible(ids)
}

# La misma definicion: el sello de cada parte, completo, y la misma huella y la
# misma version de lupa para cada metrica que aparece en mas de una parte.
.validar_sellos_consolidacion <- function(partes) {
  sellos <- lapply(partes, .sello_de_objeto)
  sin_sello <- names(partes)[vapply(sellos, is.null, logical(1L))]
  if (length(sin_sello)) {
    .rechazar_consolidacion(
      paste0(
        "la parte `", paste(sin_sello, collapse = "`, `"),
        "` no trae el sello de definici\u00f3n de sus m\u00e9tricas, y sin \u00e9l no se ",
        "puede saber si mide lo mismo que las otras."
      ),
      "Volver a medirla con esta versi\u00f3n de lupa."
    )
  }
  for (k in seq_along(sellos)) {
    nivel <- .certificacion_de(sellos[[k]])
    malas <- nivel != "completa"
    if (any(malas)) {
      .rechazar_consolidacion(
        paste0(
          "en la parte `", names(partes)[[k]], "` la definici\u00f3n de `",
          paste(sellos[[k]]$metrica_instanciada[malas], collapse = "`, `"),
          "` no est\u00e1 certificada entera (", paste(unique(nivel[malas]), collapse = ", "),
          "): la huella no prueba que sea la misma en las dos partes."
        ),
        "Medir esas m\u00e9tricas en una sola parte, sin consolidar."
      )
    }
  }
  tabla <- do.call(rbind, lapply(seq_along(sellos), function(k) {
    data.frame(
      parte = names(partes)[[k]],
      metrica = sellos[[k]]$metrica_instanciada,
      huella = sellos[[k]]$huella_definicion,
      version = sellos[[k]]$version_lupa,
      stringsAsFactors = FALSE
    )
  }))
  for (m in unique(tabla$metrica)) {
    t <- tabla[tabla$metrica == m, , drop = FALSE]
    if (nrow(t) > 1L &&
        (length(unique(t$huella)) > 1L || length(unique(t$version)) > 1L)) {
      .rechazar_consolidacion(
        paste0(
          "la m\u00e9trica `", m, "` no tiene la misma definici\u00f3n en todas las ",
          "partes (huella ", paste(.huella_corta(t$huella), collapse = ", "),
          "; lupa ", paste(unique(t$version), collapse = ", "), ")."
        ),
        "Medirla con la misma especificaci\u00f3n en todas, o en una sola parte."
      )
    }
  }
  invisible(NULL)
}

# La misma instantanea para cada tabla leida por mas de una parte.
.validar_instantaneas_consolidacion <- function(partes) {
  tablas <- lapply(seq_along(partes), function(k) {
    l <- attr(partes[[k]], "lecturas", exact = TRUE)
    if (!inherits(l, "data.frame") || !nrow(l)) return(NULL)
    l$parte <- names(partes)[[k]]
    l
  })
  sin_lecturas <- names(partes)[vapply(tablas, is.null, logical(1L)) &
                                   vapply(partes, nrow, integer(1L)) > 0L]
  if (length(sin_lecturas)) {
    .rechazar_consolidacion(
      paste0(
        "la parte `", paste(sin_lecturas, collapse = "`, `"),
        "` no registra sus lecturas, y no hay c\u00f3mo probar que vio los mismos datos."
      ),
      "Volver a medirla con esta versi\u00f3n de lupa."
    )
  }
  todas <- do.call(rbind, Filter(Negate(is.null), tablas))
  if (is.null(todas)) return(invisible(NULL))
  for (e in unique(todas$entidad)) {
    t <- todas[.clave_bytes(todas$entidad) == .clave_bytes(e), , drop = FALSE]
    if (length(unique(t$parte)) < 2L) next
    sin <- unique(t$parte[is.na(t$instantanea)])
    if (length(sin)) {
      .rechazar_consolidacion(
        paste0(
          "la tabla `", e, "` la leyeron varias partes y no hay c\u00f3mo probar que ",
          "vieron la misma instant\u00e1nea de los datos: en `",
          paste(sin, collapse = "`, `"), "` solo se sabe cu\u00e1ntas filas ten\u00eda ",
          "(el motor no tiene un contador de modificaciones que lupa lea, o la ",
          "tabla se ley\u00f3 en partes). Igual cantidad de filas no prueba los ",
          "mismos datos."
        ),
        paste(
          "Medir la tabla en una sola parte. Lo que el motor no traduce se mide",
          "en la misma corrida con `estrategia_no_sql = \"bloques\"`; o quitar la",
          "m\u00e9trica del modelo de la parte que no la puede medir y unir sin",
          "`prioridad`."
        )
      )
    }
    if (length(unique(t$instantanea)) > 1L || length(unique(t$n_filas)) > 1L) {
      .rechazar_consolidacion(
        paste0(
          "la tabla `", e, "` no es la misma en todas las partes: sus lecturas ",
          "tienen otra instant\u00e1nea (filas: ",
          paste(unique(t$n_filas), collapse = ", "), ")."
        ),
        "Medir todas las partes sobre la misma entrega."
      )
    }
  }
  invisible(NULL)
}

.discrepancia_filas <- function(elegida, descartada) {
  con_cuentas <- !is.na(elegida$numerador) && !is.na(descartada$numerador)
  coinciden <- if (con_cuentas) {
    identical(as.numeric(elegida$numerador), as.numeric(descartada$numerador)) &&
      identical(as.numeric(elegida$denominador), as.numeric(descartada$denominador)) &&
      identical(as.numeric(elegida$n_universo), as.numeric(descartada$n_universo))
  } else {
    a <- as.numeric(elegida$resultado)
    b <- as.numeric(descartada$resultado)
    isTRUE(abs(a - b) <= sqrt(.Machine$double.eps) * max(1, abs(a), abs(b)))
  }
  list(
    coinciden = coinciden,
    diferencia = as.numeric(descartada$resultado) - as.numeric(elegida$resultado)
  )
}

.consolidar_resumenes <- function(partes, prioridad) {
  orden <- .validar_prioridad(partes, prioridad)
  partes <- partes[orden]
  .validar_corrida_consolidacion(partes)
  aplic <- lapply(partes, attr, which = "configuracion_aplicabilidad", exact = TRUE)
  if (!all(vapply(aplic[-1L], identical, logical(1L), aplic[[1L]]))) {
    .rechazar_consolidacion(
      "las partes se midieron con otra aplicabilidad: otro universo no es la misma celda.",
      "Medirlas con la misma `aplicabilidad`."
    )
  }
  .validar_sellos_consolidacion(partes)
  .validar_instantaneas_consolidacion(partes)

  filas <- list()
  descartadas <- list()
  vistas <- character()
  for (k in seq_along(partes)) {
    p <- as.data.frame(unclass(partes[[k]]), stringsAsFactors = FALSE)
    p$fuente <- rep(names(partes)[[k]], nrow(p))
    claves <- .clave_celda_resumen(p)
    nuevas <- !claves %in% vistas
    if (any(!nuevas)) {
      perdidas <- p[!nuevas, , drop = FALSE]
      elegidas <- do.call(rbind, filas)
      clave_elegidas <- .clave_celda_resumen(elegidas)
      perdidas$fuente_elegida <- NA_character_
      perdidas$coinciden <- NA
      perdidas$diferencia <- NA_real_
      for (i in seq_len(nrow(perdidas))) {
        e <- elegidas[clave_elegidas == .clave_celda_resumen(perdidas[i, , drop = FALSE]), ,
                      drop = FALSE][1L, , drop = FALSE]
        dif <- .discrepancia_filas(e, perdidas[i, , drop = FALSE])
        perdidas$fuente_elegida[[i]] <- e$fuente
        perdidas$coinciden[[i]] <- dif$coinciden
        perdidas$diferencia[[i]] <- dif$diferencia
      }
      perdidas$motivo_descarte <- "menor_prioridad"
      descartadas[[length(descartadas) + 1L]] <- perdidas
    }
    filas[[length(filas) + 1L]] <- p[nuevas, , drop = FALSE]
    vistas <- c(vistas, claves[nuevas])
  }
  unidas <- do.call(rbind, filas)
  rownames(unidas) <- NULL

  # Las coberturas: una metrica que una parte no pudo medir y otra si midio
  # queda resuelta solo si la primera no la tradujo (una limitacion del
  # instrumento). Si dice que no habia valores, o que el metodo fallo, sobre la
  # misma instantanea, las dos partes se contradicen y no se juntan.
  medidas <- .clave_bytes(paste(unidas$id_medicion, unidas$metrica_instanciada, sep = "\034"))
  coberturas <- list()
  resueltas <- list()
  for (k in seq_along(partes)) {
    cob <- attr(partes[[k]], "cobertura_metricas", exact = TRUE)
    if (!inherits(cob, "data.frame") || !nrow(cob)) next
    cob$fuente <- names(partes)[[k]]
    clave <- .clave_bytes(paste(cob$id_medicion, cob$metrica_instanciada, sep = "\034"))
    medida <- clave %in% medidas
    for (i in which(medida)) {
      if (!.limitacion_instrumento(partes[[k]], cob$metrica_instanciada[[i]])) {
        .rechazar_consolidacion(
          paste0(
            "la m\u00e9trica `", cob$metrica_instanciada[[i]], "` tiene cifra en una ",
            "parte y en `", names(partes)[[k]], "` figura `", cob$estado[[i]],
            "` sobre la misma instant\u00e1nea: las partes se contradicen."
          ),
          "Revisar esa parte; una ausencia no se completa con la cifra de otra."
        )
      }
    }
    if (any(medida)) resueltas[[length(resueltas) + 1L]] <- cob[medida, , drop = FALSE]
    if (any(!medida)) coberturas[[length(coberturas) + 1L]] <- cob[!medida, , drop = FALSE]
  }

  resultado <- unidas
  primera <- partes[[1L]]
  attr(resultado, "configuracion_modelo") <- .unir_configuraciones_modelo(lapply(
    partes, attr, which = "configuracion_modelo", exact = TRUE
  )) %||% attr(primera, "configuracion_modelo", exact = TRUE)
  for (a in c("configuracion_aplicabilidad", "fecha_declarada", "detalle")) {
    attr(resultado, a) <- attr(primera, a, exact = TRUE)
  }
  marco <- Filter(Negate(is.null), lapply(partes, attr, which = "marco_calidad", exact = TRUE))
  if (length(marco)) attr(resultado, "marco_calidad") <- marco[[1L]]
  if (length(coberturas)) {
    cob <- unique(do.call(rbind, coberturas))
    rownames(cob) <- NULL
    attr(resultado, "cobertura_metricas") <- cob
  }
  if (length(resueltas)) {
    cob <- do.call(rbind, resueltas)
    rownames(cob) <- NULL
    attr(resultado, "cobertura_resuelta") <- cob
  }
  # El alcance de cada celda, de la parte que la aporto.
  alcances <- lapply(seq_along(partes), function(k) {
    al <- attr(partes[[k]], "alcance_medidas", exact = TRUE)
    if (!inherits(al, "data.frame") || !nrow(al)) return(NULL)
    mias <- unidas$metrica_instanciada[unidas$fuente == names(partes)[[k]]]
    al[.identificadores_en(al$metrica_instanciada, mias), , drop = FALSE]
  })
  alcances <- Filter(function(t) !is.null(t) && nrow(t), alcances)
  if (length(alcances)) {
    al <- do.call(rbind, alcances)
    rownames(al) <- NULL
    attr(resultado, "alcance_medidas") <- al
  }
  alcances_m <- unlist(lapply(seq_along(partes), function(k) {
    am <- attr(partes[[k]], "alcance_metricas", exact = TRUE)
    if (!length(am)) return(NULL)
    mias <- unidas$metrica_instanciada[unidas$fuente == names(partes)[[k]]]
    am[.identificadores_en(names(am), mias)]
  }), recursive = FALSE)
  if (length(alcances_m)) attr(resultado, "alcance_metricas") <- alcances_m
  sello <- .unir_sellos_mediciones(partes, "rbind()")
  if (!is.null(sello)) attr(resultado, "sello_metricas") <- sello
  lecturas <- .lecturas_de_partes(partes)
  attr(resultado, "lecturas") <- lecturas
  if (length(descartadas)) {
    d <- do.call(rbind, descartadas)
    rownames(d) <- NULL
    attr(resultado, "descartadas") <- d
  }
  attr(resultado, "consolidacion") <- list(
    prioridad = names(partes),
    partes = data.frame(
      fuente = names(partes),
      motor = vapply(partes, function(p) {
        attr(p, "ejecucion_dbi", exact = TRUE)$motor %||% "memoria"
      }, character(1L)),
      celdas_aportadas = vapply(names(partes), function(n) sum(unidas$fuente == n), integer(1L)),
      celdas_descartadas = vapply(names(partes), function(n) {
        if (!length(descartadas)) return(0L)
        sum(do.call(rbind, descartadas)$fuente == n)
      }, integer(1L)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  )
  marca <- unique(unlist(lapply(partes, attr, which = "datos_personales_sin_proteger",
                                exact = TRUE), use.names = FALSE))
  if (length(marca)) attr(resultado, "datos_personales_sin_proteger") <- marca
  resultado <- .estado_cuentas_lecturas(resultado, lecturas)
  class(resultado) <- c("medicion_resumen", "data.frame")
  discrepan <- attr(resultado, "descartadas", exact = TRUE)
  if (!is.null(discrepan) && any(!discrepan$coinciden)) {
    .avisar(
      sum(!discrepan$coinciden),
      if (sum(!discrepan$coinciden) == 1L) " celda en la que " else " celdas en las que ",
      "las partes no coinciden sobre la misma instant\u00e1nea: ",
      paste(unique(discrepan$metrica_instanciada[!discrepan$coinciden]), collapse = ", "),
      ". Se public\u00f3 la de mayor prioridad; la otra est\u00e1 en ",
      "`attr(x, \"descartadas\")`.",
      call. = FALSE
    )
  }
  resultado
}

# Las partes de una corrida comparten la fecha.
.validar_fecha_partes <- function(partes) {
  filas <- do.call(rbind, lapply(partes, function(p) {
    cob <- attr(p, "cobertura_metricas", exact = TRUE)
    rbind(
      data.frame(id = as.character(p$id_medicion), fecha = as.numeric(p$fecha),
                 stringsAsFactors = FALSE),
      if (inherits(cob, "data.frame") && nrow(cob) &&
          all(c("id_medicion", "fecha") %in% names(cob))) {
        data.frame(id = as.character(cob$id_medicion), fecha = as.numeric(cob$fecha),
                   stringsAsFactors = FALSE)
      }
    )
  }))
  if (is.null(filas) || !nrow(filas)) return(invisible(NULL))
  for (id in unique(filas$id)) {
    f <- unique(filas$fecha[filas$id == id])
    if (length(f) > 1L) {
      .rechazar_resumen(
        "rbind",
        paste0(
          "la corrida `", id, "` trae ", length(f), " fechas distintas, y una ",
          "corrida es un `id_medicion` y una fecha."
        ),
        "Dar a cada medici\u00f3n su `id_medicion`, o medirlas con la misma `fecha`."
      )
    }
  }
  invisible(NULL)
}

# Una metrica no puede tener cifra en una parte y figurar sin medir en otra de
# la misma corrida: el resumen y el tablero publicaban las dos cosas.
.validar_cobertura_partes <- function(partes, unidas) {
  medidas <- .clave_bytes(paste(unidas$id_medicion, unidas$metrica_instanciada, sep = "\034"))
  for (p in partes) {
    cob <- attr(p, "cobertura_metricas", exact = TRUE)
    if (!inherits(cob, "data.frame") || !nrow(cob)) next
    clave <- .clave_bytes(paste(cob$id_medicion, cob$metrica_instanciada, sep = "\034"))
    choca <- clave %in% medidas
    if (any(choca)) {
      .rechazar_resumen(
        "rbind",
        paste0(
          "la m\u00e9trica `", cob$metrica_instanciada[choca][[1L]], "` tiene cifra en ",
          "una parte y en otra figura `", cob$estado[choca][[1L]], "`: una corrida no ",
          "puede decir las dos cosas."
        ),
        paste(
          "Quitar la m\u00e9trica del modelo de la parte que no la midi\u00f3, o declarar",
          "qu\u00e9 parte manda con `prioridad`."
        )
      )
    }
  }
  invisible(NULL)
}

# `[` sobre una consolidacion: lo descartado sigue a la celda elegida. Si la
# celda se quito, su descarte tambien.
.filtrar_descartadas <- function(resultado, x) {
  d <- attr(x, "descartadas", exact = TRUE)
  if (!inherits(d, "data.frame") || !inherits(resultado, "data.frame") ||
      !all(c("id_medicion", "metrica_instanciada", "entidad", "atributo") %in%
           names(resultado))) {
    return(resultado)
  }
  quedan <- .clave_celda_resumen(resultado)
  d <- d[.clave_celda_resumen(d) %in% quedan, , drop = FALSE]
  rownames(d) <- NULL
  attr(resultado, "descartadas") <- d
  resultado
}
