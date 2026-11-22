# El referencial que vive en el motor.
#
# `referencial(DBI::Id(...))` -o el nombre de la tabla- no copia la tabla a R.
# `medir_dbi()` resuelve esa tabla en su conexion como resuelve las entidades,
# lee su esquema sin leer filas y mide `CorrectitudSemFuerte`,
# `CorrectitudSemDebil` y `RatioCobertura` en el motor, contando lo que R
# contaria con las dos tablas leidas (`dbReadTable()`) y `medir()`:
#
# - la igualdad de cada par de columnas es la de R: por el texto canonico de
#   cada valor, con la intercalacion binaria explicita para el texto y el
#   entero escrito como texto contra una clave de texto;
# - la normalizacion del referencial se escribe en el motor solo donde es
#   exacta: sobre texto ASCII el pliegue de mayusculas es `LOWER()`, y una
#   guarda cuenta las filas cuyo texto el perfil podria cambiar y el SQL no
#   escribe -lo no ASCII, que R descompone aun sin pasos; blancos y controles;
#   comillas; puntuacion-. Con alguna, la metrica no se mide en SQL;
# - un `NaN` es ausente, como para `.filas_completas()`;
# - la clave del referencial se verifica en cada corrida con una consulta: sin
#   ausentes y unica por bytes, con las frases de `referencial()`.
#
# Lo que esa via no escribe -texto que la guarda aparta, un referencial que
# esta en R- se compara en R sobre los valores DISTINTOS de la entidad, contados
# en el motor, si se pide `estrategia_no_sql = "bloques"`: el metodo de R corre
# sobre esos valores y cada resultado pesa lo que cuenta su grupo. Sin
# `bloques`, la metrica queda `no_medible` con el motivo y el remedio.

.METRICAS_REFERENCIALES_DBI <- c(
  "CorrectitudSemFuerte", "CorrectitudSemDebil", "RatioCobertura"
)

# Solo los motores donde las dos vias estan medidas contra R.
.MOTORES_REFERENCIAL_DBI <- c("sqlite", "duckdb")

.es_metrica_referencial_dbi <- function(instancia) {
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  if (!nombre %in% .METRICAS_REFERENCIALES_DBI) return(FALSE)
  esperado <- switch(
    nombre,
    CorrectitudSemFuerte = .metodo_correctitud_fuerte,
    CorrectitudSemDebil = .metodo_correctitud_debil,
    RatioCobertura = .metodo_ratio_cobertura
  )
  identical(instancia$metodo, esperado)
}

# La tabla del referencial en la conexion de la medicion: en una coleccion, por
# su identificador o su nombre, como las entidades
# (`.fisica_tabla_coleccion_dbi()`); si no, el nombre tal cual.
.fisica_referencial_dbi <- function(referencia, conexion) {
  tabla <- .tabla_motor_dbi(referencia)
  if (inherits(conexion, "coleccion_lupa") && is.character(tabla) &&
      length(tabla) == 1L) {
    fisica <- .fisica_tabla_coleccion_dbi(tabla, conexion)
    if (!is.null(fisica)) return(fisica)
  }
  .nombre_para_conexion_dbi(tabla)
}

# Lee una vez el esquema de cada tabla de referencial de la corrida: lo
# comparten las metricas que la usan, y el plan lo cuenta.
.resolutor_referenciales_dbi <- function(conexion, con, motor, contador, sesion,
                                         error_sesion = NULL) {
  cache <- new.env(parent = emptyenv())
  orden <- character()
  resolver <- function(referencia) {
    fisica <- .fisica_referencial_dbi(referencia, conexion)
    clave <- .texto_tabla_dbi(fisica)
    if (!exists(clave, envir = cache, inherits = FALSE)) {
      orden <<- c(orden, clave)
      valor <- if (!is.null(error_sesion)) {
        list(esq = NULL, motivo = error_sesion)
      } else {
        tryCatch(
          list(esq = .esquema_medicion_dbi(
            con, fisica, motor, contador = contador, sesion = sesion
          ), motivo = NULL),
          error = function(err) {
            list(esq = NULL, motivo = if (
              inherits(err, "lupa_presupuesto_medicion_dbi")
            ) {
              conditionMessage(err)
            } else {
              paste0(
                "la tabla `", clave, "` del referencial `", referencia$nombre,
                "` no est\u00e1 en la base o no se pudo leer su esquema"
              )
            })
          }
        )
      }
      valor$clave <- clave
      assign(clave, valor, envir = cache)
    }
    get(clave, envir = cache, inherits = FALSE)
  }
  list(
    resolver = resolver,
    esquemas = function() {
      salida <- lapply(orden, function(k) get(k, envir = cache)$esq)
      names(salida) <- orden
      Filter(Negate(is.null), salida)
    }
  )
}

# La guarda del pliegue: las filas cuyo texto la normalizacion de R podria
# cambiar y el SQL no escribe. Es el espejo de `.normalizacion_ascii_sin_cambios()`
# menos las mayusculas, que se pliegan con `LOWER()`. Lo no ASCII se aparta
# siempre: R lo descompone aun con un perfil sin pasos. Medido sobre los 127
# caracteres ASCII en tres posiciones, para cada perfil, en SQLite y DuckDB.
.sql_guarda_pliegue_dbi <- function(expr, motor, perfil) {
  casa <- function(glob, regex) {
    switch(
      motor,
      sqlite = paste0("(", expr, " GLOB '", glob, "')"),
      duckdb = paste0("regexp_matches(", expr, ", '", regex, "')")
    )
  }
  partes <- .sql_no_ascii(expr, motor)
  # `espacios` recorta los bordes, junta los blancos repetidos y quita los
  # controles: un blanco suelto entre dos letras no cambia y no se aparta.
  if (isTRUE(perfil$espacios)) {
    partes <- c(
      partes, casa(" *", "^ "), casa("* ", " $"), casa("*  *", "  "),
      casa("*[^ -~]*", "[^ -~]")
    )
  }
  if (isTRUE(perfil$comillas)) {
    partes <- c(partes, casa("*[\"'']*", "[\"'']"))
  }
  if (isTRUE(perfil$puntuacion)) {
    partes <- c(partes, casa("*[!-/:-@[-`{-~]*", "[!-/:-@\\[-`{-~]"))
  }
  paste0("(", paste(partes, collapse = " OR "), ")")
}

.plan_no_medible_referencial <- function(base, motivo, via = "no_medible") {
  c(base, list(via = via, motivo = motivo))
}

# El plan de una metrica referencial. Devuelve el plan de la via SQL
# (`sql_dedicado_referencial` con `referencial_motor = TRUE`), el de la via de
# valores distintos (`distintos_r`) o el `no_medible` con su motivo.
.planificar_referencial_dbi <- function(base, instancia, esquemas, motor,
                                        conexion, con, universos,
                                        estrategia_no_sql, referenciales) {
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  debil <- identical(nombre, "CorrectitudSemDebil")
  ratio <- identical(nombre, "RatioCobertura")
  contrato <- tryCatch({
    referencia <- .exigir_referencial(instancia, valores = debil, completo = ratio)
    columnas_ref <- if (debil) c(referencia$clave, referencia$valor) else referencia$clave
    .validar_vinculo(instancia, 1L, length(columnas_ref))
    list(referencia = referencia, columnas_ref = columnas_ref)
  }, error = function(e) e)
  if (inherits(contrato, "error")) {
    return(.plan_no_medible_referencial(
      base, conditionMessage(contrato), via = "no_medible_metodo"
    ))
  }
  referencia <- contrato$referencia
  columnas_ref <- contrato$columnas_ref
  entidad <- instancia$entidad[[1L]]
  esq <- esquemas[[entidad]]
  bloques <- identical(estrategia_no_sql, "bloques")
  distintos <- function(motivo, esq_ref = NULL, columnas_ref_reales = NULL) {
    c(base, list(
      via = "distintos_r", motivo = motivo, referencial_motor = !is.null(esq_ref),
      esquema_referencial = esq_ref, columnas_referencial = columnas_ref_reales,
      metrica_referencial = nombre
    ))
  }
  if (!motor %in% .MOTORES_REFERENCIAL_DBI) {
    return(.plan_no_medible_referencial(base, paste0(
      "las m\u00e9tricas referenciales se miden en el motor en SQLite y DuckDB; ",
      "en `", motor, "` su equivalencia con R no est\u00e1 verificada"
    )))
  }
  if (!.referencial_en_motor(referencia)) {
    motivo <- paste0(
      "el referencial `", referencia$nombre, "` est\u00e1 en memoria: compararlo ",
      "exige leer los valores de la entidad"
    )
    if (bloques) return(distintos(motivo))
    return(.plan_no_medible_referencial(base, paste0(
      motivo, ". Pedir `estrategia_no_sql = \"bloques\"` (compara en R los ",
      "valores distintos, contados en el motor) o declarar el referencial sobre ",
      "su tabla del motor: `referencial(DBI::Id(...), ...)`"
    )))
  }
  resuelto <- referenciales$resolver(referencia)
  esq_ref <- resuelto$esq
  if (is.null(esq_ref)) {
    return(.plan_no_medible_referencial(base, resuelto$motivo))
  }
  indices <- .indice_nombre(columnas_ref, esq_ref$campos)
  if (anyNA(indices)) {
    return(.plan_no_medible_referencial(base, paste0(
      "No se encontraron columnas del referencial `", referencia$nombre,
      "` en la tabla `", resuelto$clave, "`: ",
      paste(columnas_ref[is.na(indices)], collapse = ", "), "."
    ), via = "no_medible_metodo"))
  }
  columnas_ref_reales <- esq_ref$campos[indices]
  faltan <- setdiff(instancia$atributos, esq$campos)
  if (length(faltan)) {
    return(.plan_no_medible_referencial(base, paste0(
      "No se encontraron atributos ligados: ", paste(faltan, collapse = ", "), "."
    ), via = "no_medible_metodo"))
  }
  compilado <- tryCatch(
    .compilar_referencial_dbi(
      instancia, nombre, referencia, columnas_ref_reales, esq, esq_ref, motor,
      con, universos[[entidad]]
    ),
    lupa_no_traducible_dbi = function(e) e
  )
  if (inherits(compilado, "condition")) {
    motivo <- paste0(
      "sin traducci\u00f3n exacta a SQL (", conditionMessage(compilado), ")"
    )
    if (bloques) return(distintos(motivo, esq_ref, columnas_ref_reales))
    return(.plan_no_medible_referencial(base, paste0(
      motivo, ". Pedir `estrategia_no_sql = \"bloques\"` para comparar en R los ",
      "valores distintos, contados en el motor"
    )))
  }
  c(base, compilado, list(
    via = "sql_dedicado_referencial", referencial_motor = TRUE,
    esquema_referencial = esq_ref, columnas_referencial = columnas_ref_reales,
    metrica_referencial = nombre, admite_distintos = bloques
  ))
}

# Las expresiones de cada par (atributo de la entidad, columna del
# referencial), con la clase con que R lee cada una.
.pares_referencial_dbi <- function(instancia, referencia, columnas_ref, esq,
                                   esq_ref, motor, conexion) {
  atributos <- instancia$atributos
  perfil <- .referencial_normalizacion(instancia, referencia)
  for (a in atributos) .validar_columna_comparacion_dbi(esq, a, motor)
  for (r in columnas_ref) .validar_columna_comparacion_dbi(esq_ref, r, motor)
  fallo <- .motivo_tipos_esquema_dbi(esq, atributos)
  if (!is.null(fallo)) .no_traducible_dbi(fallo)
  fallo <- .motivo_tipos_esquema_dbi(esq_ref, columnas_ref)
  if (!is.null(fallo)) .no_traducible_dbi(fallo)
  lapply(seq_along(atributos), function(i) {
    a <- atributos[[i]]
    r <- columnas_ref[[i]]
    cf <- esq$clases[[a]]
    cp <- esq_ref$clases[[r]]
    perfil_d <- .normalizacion_para_columna(perfil, a)
    perfil_r <- .normalizacion_para_columna(perfil, r)
    fk <- .columna_sql_medicion(conexion, esq, a, motor, alias = "lupa_d",
                                normalizar_nan = TRUE)
    pk <- .columna_sql_medicion(conexion, esq_ref, r, motor, alias = "lupa_r",
                                normalizar_nan = TRUE)
    plegar_d <- identical(cf, "texto") && isTRUE(perfil_d$minusculas)
    plegar_r <- identical(cp, "texto") && isTRUE(perfil_r$minusculas)
    list(
      atributo = a, columna_ref = r, cf = cf, cp = cp, fk = fk, pk = pk,
      plegar_d = plegar_d, plegar_r = plegar_r,
      guarda_d = if (identical(cf, "texto")) .sql_guarda_pliegue_dbi(fk, motor, perfil_d),
      guarda_r = if (identical(cp, "texto")) .sql_guarda_pliegue_dbi(pk, motor, perfil_r)
    )
  })
}

.compilar_referencial_dbi <- function(instancia, nombre, referencia,
                                      columnas_ref, esq, esq_ref, motor,
                                      conexion, universo) {
  pares <- .pares_referencial_dbi(
    instancia, referencia, columnas_ref, esq, esq_ref, motor, conexion
  )
  igualdades <- vapply(pares, function(p) {
    igualdad <- .igualdad_referencial_dbi(
      motor, p$cp, p$cf, p$pk, p$fk, esq_ref, p$columna_ref, esq, p$atributo,
      plegar_ref = p$plegar_r, plegar_dep = p$plegar_d
    )
    if (is.null(igualdad)) {
      .no_traducible_dbi(
        "clave `", p$cp, "` contra `", p$cf,
        "`: R compara por el texto can\u00f3nico de cada valor y el motor no ",
        "escribe igual ese tipo"
      )
    }
    igualdad
  }, character(1L))
  U <- universo(instancia, columnas = instancia$atributos, alias = "lupa_d")
  if (!is.null(U$motivo)) .no_traducible_dbi("aplicabilidad: ", U$motivo)
  if (length(U$guardas)) {
    .no_traducible_dbi(
      "aplicabilidad con guarda: ", U$motivo_guarda %||% "la regla tiene una guarda"
    )
  }
  no_nulos_d <- .sql_y(vapply(pares, function(p) paste0("(", p$fk, " IS NOT NULL)"), ""))
  no_nulos_r <- .sql_y(vapply(pares, function(p) paste0("(", p$pk, " IS NOT NULL)"), ""))
  guardas_d <- Filter(Negate(is.null), lapply(pares, `[[`, "guarda_d"))
  guardas_r <- Filter(Negate(is.null), lapply(pares, `[[`, "guarda_r"))
  guarda_d <- if (length(guardas_d)) {
    .sql_suma(.sql_y(U$sql, .sql_o(paste0("(", unlist(guardas_d), ")"))), motor)
  }
  guarda_r <- if (length(guardas_r)) {
    paste0("(SELECT ", .sql_suma(.sql_o(paste0("(", unlist(guardas_r), ")")), motor),
           " FROM ", esq_ref$sql, " lupa_r)")
  }
  evaluable <- .sql_y(U$sql, no_nulos_d)
  # La validacion de la clave del referencial: filas con alguna columna de la
  # clave ausente y grupos repetidos por bytes. Se comparte entre las metricas
  # de la corrida que usan la misma tabla y la misma clave.
  n_clave <- length(referencia$clave)
  pares_clave <- pares[seq_len(n_clave)]
  grupo <- vapply(pares_clave, function(p) {
    if (identical(p$cp, "texto")) .sql_texto_binario_dbi(p$pk, motor) else p$pk
  }, character(1L))
  nn_clave <- .sql_y(vapply(pares_clave, function(p) paste0("(", p$pk, " IS NOT NULL)"), ""))
  validacion <- paste0(
    "SELECT ", .sql_conteo_total(motor), " AS n, ",
    .sql_suma(paste0("NOT (", nn_clave, ")"), motor), " AS ausentes, ",
    "(SELECT COUNT(*) FROM (SELECT 1 AS lupa_u FROM ", esq_ref$sql,
    " lupa_r WHERE ", nn_clave, " GROUP BY ", paste(grupo, collapse = ", "),
    " HAVING COUNT(*) > 1) lupa_g) AS repetidas FROM ", esq_ref$sql, " lupa_r"
  )
  clave_validacion <- paste(
    c(esq_ref$sql, columnas_ref[seq_len(n_clave)]), collapse = "\r"
  )
  guardas <- c(guarda_d, guarda_r)
  guarda <- if (length(guardas)) {
    paste0(", (", paste(paste0("COALESCE(", guardas, ", 0)"), collapse = " + "),
           ") AS guarda")
  } else ""
  if (identical(nombre, "RatioCobertura")) {
    # El universo es el de las claves DISTINTAS del referencial, con la misma
    # identidad con que se empareja: `A` y `a` son una sola si el perfil pliega.
    lados <- vapply(pares, function(p) {
      if (identical(p$cp, "texto")) {
        .sql_texto_binario_dbi(
          if (isTRUE(p$plegar_r)) paste0("LOWER(", p$pk, ")") else p$pk, motor
        )
      } else p$pk
    }, character(1L))
    igual_u <- vapply(seq_along(pares), function(i) {
      p <- pares[[i]]
      u <- paste0("lupa_u.lupa_k", i)
      if (identical(p$cp, "entero") && identical(p$cf, "texto")) {
        u <- .sql_texto_binario_dbi(
          .cast_entero_a_texto_dbi(u, esq, p$atributo, motor), motor
        )
      }
      fk <- if (identical(p$cf, "texto")) {
        .sql_texto_binario_dbi(
          if (isTRUE(p$plegar_d)) paste0("LOWER(", p$fk, ")") else p$fk, motor
        )
      } else if (identical(p$cf, "entero") && identical(p$cp, "texto")) {
        .sql_texto_binario_dbi(
          .cast_entero_a_texto_dbi(p$fk, esq_ref, p$columna_ref, motor), motor
        )
      } else p$fk
      paste0(fk, " = ", u)
    }, character(1L))
    guarda_sub <- c(
      if (length(guardas_d)) {
        paste0("(SELECT ", guarda_d, " FROM ", esq$sql, " lupa_d)")
      },
      guarda_r
    )
    guarda_ratio <- if (length(guarda_sub)) {
      paste0(", (", paste(paste0("COALESCE(", guarda_sub, ", 0)"), collapse = " + "),
             ") AS guarda")
    } else ""
    consulta <- paste0(
      "SELECT ", .sql_conteo_total(motor), " AS universo, ",
      .sql_suma(paste0(
        "EXISTS (SELECT 1 FROM ", esq$sql, " lupa_d WHERE ",
        .sql_y(evaluable, paste(igual_u, collapse = " AND ")), ")"
      ), motor), " AS cubiertas, ",
      "(SELECT ", .sql_suma(evaluable, motor), " FROM ", esq$sql,
      " lupa_d) AS evaluables, (SELECT ", .sql_conteo_total(motor), " FROM ",
      esq$sql, " lupa_d) AS n_filas", guarda_ratio,
      " FROM (SELECT DISTINCT ", paste0(lados, " AS lupa_k", seq_along(lados), collapse = ", "),
      " FROM ", esq_ref$sql, " lupa_r WHERE ", no_nulos_r, ") lupa_u"
    )
  } else {
    existe <- paste0(
      "EXISTS (SELECT 1 FROM ", esq_ref$sql, " lupa_r WHERE ",
      .sql_y(no_nulos_r, paste(igualdades, collapse = " AND ")), ")"
    )
    unidad <- .unidad_universo_instancia(instancia)
    sin_valor <- if (identical(unidad, "fila")) {
      .sql_suma(.sql_y(
        U$sql,
        paste0("(", paste(vapply(pares, function(p) {
          paste0("(", p$fk, " IS NULL)")
        }, character(1L)), collapse = " OR "), ")")
      ), motor)
    } else {
      paste(vapply(pares, function(p) {
        .sql_suma(.sql_y(U$sql, paste0("(", p$fk, " IS NULL)")), motor)
      }, character(1L)), collapse = " + ")
    }
    consulta <- paste0(
      "SELECT ", .sql_suma(.sql_y(evaluable, existe), motor), " AS num, ",
      .sql_suma(evaluable, motor), " AS den, ",
      .sql_suma(.sql_y(U$sql), motor), " AS univ, ",
      "(", sin_valor, ") AS sin_valor, ",
      .sql_conteo_total(motor), " AS n_filas", guarda,
      " FROM ", esq$sql, " lupa_d"
    )
  }
  list(
    consulta = consulta, consulta_validacion = validacion,
    clave_validacion = clave_validacion,
    motivo_guarda = paste0(
      "el texto de la entidad o del referencial tiene caracteres que la ",
      "normalizaci\u00f3n de R podr\u00eda cambiar y el motor no escribe igual (no ASCII, ",
      "blancos, comillas o puntuaci\u00f3n seg\u00fan el perfil)"
    )
  )
}

.frases_validacion_referencial <- c(
  ausentes = "La clave del referencial no puede contener valores ausentes.",
  repetidas = "La clave del referencial debe identificar un\u00edvocamente cada fila."
)

.validar_referencial_dbi <- function(x, con, contador, validaciones) {
  clave <- x$clave_validacion
  if (exists(clave, envir = validaciones, inherits = FALSE)) {
    return(get(clave, envir = validaciones, inherits = FALSE))
  }
  q <- tryCatch(
    .consultar_medicion_dbi(con, x$consulta_validacion, contador),
    lupa_presupuesto_medicion_dbi = function(e) e,
    error = function(e) e
  )
  salida <- if (inherits(q, "error")) {
    list(motivo = if (inherits(q, "lupa_presupuesto_medicion_dbi")) {
      conditionMessage(q)
    } else .mensaje_motor(q), en_base = TRUE)
  } else if (as.numeric(q$ausentes) > 0) {
    list(motivo = .frases_validacion_referencial[["ausentes"]], en_base = FALSE,
         n = as.numeric(q$n))
  } else if (as.numeric(q$repetidas) > 0) {
    list(motivo = .frases_validacion_referencial[["repetidas"]], en_base = FALSE,
         n = as.numeric(q$n))
  } else {
    list(motivo = NULL, n = as.numeric(q$n))
  }
  assign(clave, salida, envir = validaciones)
  salida
}

# Las filas y coberturas de una medicion referencial a partir de sus conteos:
# las mismas que da `medir(detalle = "resumen")` sobre las tablas leidas.
.salida_conteos_referencial_dbi <- function(x, conteos, id_medicion, fecha) {
  instancia <- x$instancia
  entidad <- instancia$entidad[[1L]]
  atributos <- instancia$atributos
  salida <- list(fila = NULL, cobertura = NULL, alcance = NULL)
  if (identical(x$metrica_referencial, "RatioCobertura")) {
    if (!conteos$universo) {
      referencia <- instancia$referencial
      salida$cobertura <- .cobertura_metrica_abstenida(
        instancia, id_medicion, fecha, list(
          motivo = paste0(
            "El referencial `", referencia$nombre, "` no tiene claves: la ",
            "cobertura de un universo vac\u00edo no es una proporci\u00f3n. No es un ",
            "fallo, es un alcance vac\u00edo."
          ),
          como_resolverlo = paste(
            "Revisar que el padr\u00f3n se haya cargado completo; si de verdad no",
            "tiene claves, no hay cobertura que medir."
          ),
          estado = "sin_valores"
        )
      )
      return(salida)
    }
    if (!conteos$evaluables) {
      salida$cobertura <- .cobertura_metrica_desde_conteos(
        instancia, id_medicion, fecha, list(
          entidad_dependiente_vacia = NULL, entidad_presente = TRUE,
          n_recortada = conteos$n_filas, n_original = conteos$n_filas,
          aplicable_vacio = FALSE, sin_valores_atributos = TRUE
        )
      )
      return(salida)
    }
    salida$fila <- .fila_resumen(
      instancia, id_medicion, fecha, entidad, paste(atributos, collapse = "+"),
      paste0(entidad, " respecto de ", instancia$referencial$nombre),
      resultado = conteos$cubiertas / conteos$universo,
      agregacion = "promedio", tipo_resultado = "real",
      numerador = conteos$cubiertas, denominador = conteos$universo,
      n_universo = NA_real_, n_no_evaluados = NA_real_,
      motivo_no_evaluados = NA_character_
    )
    return(salida)
  }
  univ <- conteos$univ
  den <- conteos$den
  if (!den) {
    salida$cobertura <- .cobertura_metrica_desde_conteos(
      instancia, id_medicion, fecha, list(
        entidad_dependiente_vacia = NULL, entidad_presente = TRUE,
        n_recortada = univ, n_original = conteos$n_filas,
        aplicable_vacio = FALSE, sin_valores_atributos = univ > 0
      )
    )
    return(salida)
  }
  unidad <- .unidad_universo_instancia(instancia)
  esperadas <- if (identical(unidad, "celda")) univ * length(atributos) else univ
  no <- esperadas - den
  sv <- conteos$sin_valor
  salida$alcance <- .alcance_medidas_desde_conteos(instancia, esperadas, den, sv)
  salida$fila <- .fila_resumen(
    instancia, id_medicion, fecha, entidad, paste(atributos, collapse = "+"),
    .pegar_texto0(entidad, "$", paste(atributos, collapse = "+")),
    resultado = conteos$num / den, agregacion = "ratio", tipo_resultado = "real",
    numerador = conteos$num, denominador = den, n_universo = esperadas,
    n_no_evaluados = no,
    motivo_no_evaluados = if (!no) NA_character_ else if (!is.na(sv) && sv == no)
      "sin_valor" else "sin_medida", n_sin_valor = sv,
    unidad = unidad
  )
  salida
}

.ejecutar_referencial_dbi <- function(x, con, contador, id_medicion, fecha,
                                      validaciones) {
  instancia <- x$instancia
  validacion <- .validar_referencial_dbi(x, con, contador, validaciones)
  if (!is.null(validacion$motivo)) {
    return(list(cobertura = .no_medible_dbi(
      instancia, id_medicion, fecha, validacion$motivo,
      en_base = isTRUE(validacion$en_base)
    )))
  }
  q <- tryCatch(
    .consultar_medicion_dbi(con, x$consulta, contador),
    lupa_presupuesto_medicion_dbi = function(e) e,
    error = function(e) e
  )
  if (inherits(q, "error")) {
    msg <- if (inherits(q, "lupa_presupuesto_medicion_dbi")) {
      conditionMessage(q)
    } else .mensaje_motor(q)
    return(list(cobertura = .no_medible_dbi(
      instancia, id_medicion, fecha, msg, en_base = TRUE
    )))
  }
  if (!is.null(q$guarda) && as.numeric(q$guarda) > 0) {
    return(list(guarda = TRUE, motivo = x$motivo_guarda))
  }
  conteos <- lapply(as.list(q), as.numeric)
  salida <- .salida_conteos_referencial_dbi(x, conteos, id_medicion, fecha)
  salida$n_filas <- as.numeric(q$n_filas)
  salida
}

# ---- Valores distintos contados en el motor y comparados en R ---------------

# `SELECT <atributos>, COUNT(*) ... GROUP BY <atributos>` con la intercalacion
# binaria del texto: el grupo es al menos tan fino como la lectura de R -dos
# filas de un grupo R las lee iguales-, asi que el metodo de R sobre los
# representantes, con el peso de cada grupo, da lo que daria sobre la tabla.
.consulta_distintos_referencial_dbi <- function(esq, columnas, motor, conexion,
                                                donde = "", alias = "lupa_d") {
  expr <- vapply(columnas, function(cn) {
    e <- .columna_sql_medicion(conexion, esq, cn, motor, alias = alias,
                               normalizar_nan = TRUE)
    if (identical(esq$clases[[cn]], "texto")) .sql_texto_binario_dbi(e, motor) else e
  }, character(1L))
  nombres <- vapply(columnas, function(cn) .citar_col_dbi(conexion, cn), character(1L))
  paste0(
    "SELECT ", paste0(expr, " AS ", nombres, collapse = ", "),
    ", ", .sql_conteo_total(motor), " AS lupa_n FROM ", esq$sql, " ", alias,
    if (nzchar(donde)) paste0(" WHERE ", donde) else "",
    " GROUP BY ", paste(expr, collapse = ", ")
  )
}

.ejecutar_distintos_referencial_dbi <- function(x, con, contador, id_medicion,
                                                fecha, esquemas, universos,
                                                motor) {
  instancia <- x$instancia
  entidad <- instancia$entidad[[1L]]
  esq <- esquemas[[entidad]]
  atributos <- instancia$atributos
  no_medible <- function(motivo, en_base = TRUE) {
    list(cobertura = .no_medible_dbi(instancia, id_medicion, fecha, motivo,
                                     en_base = en_base))
  }
  comprobado <- tryCatch({
    for (a in atributos) .validar_columna_comparacion_dbi(esq, a, motor)
    fallo <- .motivo_tipos_esquema_dbi(esq, atributos)
    if (!is.null(fallo)) .no_traducible_dbi(fallo)
    if (!is.null(x$esquema_referencial)) {
      for (r in x$columnas_referencial) {
        .validar_columna_comparacion_dbi(x$esquema_referencial, r, motor)
      }
      fallo <- .motivo_tipos_esquema_dbi(x$esquema_referencial, x$columnas_referencial)
      if (!is.null(fallo)) .no_traducible_dbi(fallo)
    }
    U <- universos[[entidad]](instancia, columnas = atributos, alias = "lupa_d")
    if (!is.null(U$motivo)) .no_traducible_dbi("aplicabilidad: ", U$motivo)
    if (length(U$guardas)) .no_traducible_dbi("aplicabilidad con guarda")
    U
  }, lupa_no_traducible_dbi = function(e) e)
  if (inherits(comprobado, "condition")) return(no_medible(conditionMessage(comprobado)))
  consultar <- function(sql) {
    tryCatch(
      .consultar_medicion_dbi(con, sql, contador),
      lupa_presupuesto_medicion_dbi = function(e) e,
      error = function(e) e
    )
  }
  grupos <- consultar(.consulta_distintos_referencial_dbi(
    esq, atributos, motor, con, donde = comprobado$sql
  ))
  if (inherits(grupos, "error")) {
    return(no_medible(if (inherits(grupos, "lupa_presupuesto_medicion_dbi")) {
      conditionMessage(grupos)
    } else .mensaje_motor(grupos)))
  }
  n_filas_q <- consultar(paste0(
    "SELECT ", .sql_conteo_total(motor), " AS n FROM ", esq$sql, " lupa_d"
  ))
  if (inherits(n_filas_q, "error")) return(no_medible(.mensaje_motor(n_filas_q)))
  referencia <- instancia$referencial
  if (.referencial_en_motor(referencia)) {
    esq_ref <- x$esquema_referencial
    filas_ref <- consultar(.consulta_distintos_referencial_dbi(
      esq_ref, x$columnas_referencial, motor, con, alias = "lupa_r"
    ))
    if (inherits(filas_ref, "error")) return(no_medible(.mensaje_motor(filas_ref)))
    tabla_ref <- filas_ref[setdiff(names(filas_ref), "lupa_n")]
    names(tabla_ref) <- c(referencia$clave, referencia$valor)
    if (any(as.numeric(filas_ref$lupa_n) > 1)) {
      return(no_medible(.frases_validacion_referencial[["repetidas"]], en_base = FALSE))
    }
    en_r <- tryCatch(
      referencial(
        tabla_ref, referencia$clave, referencia$valor,
        completo = referencia$completo,
        alcance = if (is.na(referencia$alcance)) NULL else referencia$alcance,
        nombre = referencia$nombre, normalizar = referencia$normalizar
      ),
      error = function(e) e
    )
    if (inherits(en_r, "error")) return(no_medible(conditionMessage(en_r), en_base = FALSE))
    instancia$referencial <- en_r
  }
  pesos <- as.numeric(grupos$lupa_n)
  tabla <- grupos[atributos]
  tablas <- stats::setNames(list(tabla), entidad)
  medida <- tryCatch(instancia$metodo(tablas, instancia), error = function(e) e)
  if (inherits(medida, "error")) {
    return(no_medible(conditionMessage(medida), en_base = FALSE))
  }
  abstencion <- attr(medida, "abstencion", exact = TRUE)
  if (!is.null(abstencion)) {
    return(list(cobertura = .cobertura_metrica_abstenida(
      instancia, id_medicion, fecha, abstencion
    )))
  }
  n_filas <- as.numeric(n_filas_q$n)
  if (identical(x$metrica_referencial, "RatioCobertura")) {
    alcance <- attr(medida, "alcance", exact = TRUE)
    conteos <- list(
      universo = as.numeric(alcance$n_referencial %||% NA_real_),
      cubiertas = as.numeric(alcance$n_cubiertas %||% NA_real_),
      evaluables = if (nrow(medida)) 1 else 0, n_filas = n_filas
    )
    if (!nrow(medida)) conteos$evaluables <- 0
    salida <- .salida_conteos_referencial_dbi(x, conteos, id_medicion, fecha)
    salida$n_filas <- n_filas
    return(salida)
  }
  filas <- as.integer(medida$fila)
  cumple <- as.numeric(medida$resultado) == 1
  unidad <- .unidad_universo_instancia(instancia)
  sin_valor <- if (identical(unidad, "fila")) {
    sum(pesos[!stats::complete.cases(tabla[atributos])])
  } else {
    sum(vapply(atributos, function(a) {
      sum(pesos[is.na(tabla[[a]])])
    }, numeric(1L)))
  }
  conteos <- list(
    num = sum(pesos[filas[cumple]]), den = sum(pesos[filas]),
    univ = sum(pesos), sin_valor = sin_valor, n_filas = n_filas
  )
  salida <- .salida_conteos_referencial_dbi(x, conteos, id_medicion, fecha)
  salida$n_filas <- n_filas
  salida
}

# Un referencial del motor se resuelve en la conexion de la medicion: el mismo
# nombre en otra base es otra tabla. El sello de la medicion lleva la huella del
# lugar de esa conexion (archivo, anfitrion, puerto, base; sin usuario ni
# credenciales, y sin la ruta en claro: `.lugar_conexion_dbi()`), asi comparar
# corridas de dos bases avisa que cambio la definicion. Solo en la copia del
# modelo que firma el sello: el modelo del usuario no cambia.
.fuente_referenciales_dbi <- function(modelo, conexion) {
  metricas <- .subset2(modelo, "metricas")
  en_motor <- vapply(metricas, function(i) {
    .referencial_en_motor(.subset2(i, "referencial"))
  }, logical(1L))
  if (!any(en_motor)) return(modelo)
  lugar <- tryCatch(.lugar_conexion_dbi(conexion), error = function(e) NA_character_)
  for (k in which(en_motor)) {
    instancia <- metricas[[k]]
    referencia <- .subset2(instancia, "referencial")
    referencia$fuente_motor <- lugar
    instancia$referencial <- referencia
    metricas[[k]] <- instancia
  }
  modelo$metricas <- metricas
  modelo
}
