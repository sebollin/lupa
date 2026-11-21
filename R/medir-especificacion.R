# Especificacion unica de metricas traducibles y evaluador seguro de formulas.
#
# Cada metrica traducible se describe una sola vez mediante un nodo y sus
# parametros (`.especificacion_metrica()`). De esa especificacion salen las
# tres vias: la medida por celda de `medir(detalle = "completo")`, el resumen
# en memoria (`medir(detalle = "resumen")`) y la compilacion a SQL de
# `medir_dbi()` / `plan_medicion_dbi()`.

# Ruta declarada de cada metrica del catalogo (`metricas_nucleo()` y
# `metricas_referencial()`). La puerta de catalogo comprueba que toda metrica
# publicada aparezca en esta tabla.
.RUTAS_CATALOGO_METRICAS <- data.frame(
  metrica = c(
    "NoNulo", "Formato", "ValoresPosiblesPorExtension",
    "ReglaIntegridadIntraEntidad", "ReglaIntegridadInterEntidad",
    "ErrorEstandar", "Escala", "ValoresPosiblesPorComprension",
    "AtributoDuplicado", "ConjuntoAtributosDuplicado", "EntidadDuplicada",
    "EntidadContradictoria", "DesactualizacionPorFormato",
    "DesactualizacionPorFecha", "DesactualizacionPorCambios",
    "OportunidadAtributoPorFecha", "OportunidadAtributoPorIntervalo",
    "GradoOportunidadAtributoPorFecha", "GradoOportunidadAtributoPorIntervalo",
    "OportunidadEntPorFecha", "OportunidadEntPorIntervalo",
    "DensidadPonderada", "CorrectitudSemFuerte", "CorrectitudSemDebil",
    "RatioCobertura"
  ),
  especificada = c(
    "siempre", "con diccionario", "siempre",
    "con formula", "siempre",
    "no", "no", "con rango",
    "siempre", "siempre", "no",
    "no", "no",
    "no", "no",
    "con limite escalar", "con limites escalares",
    "no", "no",
    "con limite escalar", "con limites escalares",
    "no", "no", "no",
    "no"
  ),
  ruta_sin_especificacion = c(
    NA_character_, "bloques (regex o validador de R)", NA_character_,
    "bloques (funcion de R)", NA_character_,
    "no_medible (no separable: dispersion de la columna entera)",
    "bloques (contrato de escala por celda)",
    "bloques (predicado de R)",
    NA_character_, NA_character_,
    "no_medible (no separable: compara filas entre si)",
    "no_medible (no separable: compara filas entre si)",
    "bloques (regex o validador de R)",
    "no_resumible (duracion por celda, no acotada)",
    "no_resumible (entero por celda, no acotado)",
    "bloques (limite por fila)", "bloques (limites por fila)",
    "bloques (real por celda)", "bloques (real por celda)",
    "bloques (limite por fila)", "bloques (limites por fila)",
    "bloques (real por fila)",
    "no_medible (referencial externo en memoria)",
    "no_medible (referencial externo en memoria)",
    "no_medible (referencial externo en memoria)"
  ),
  stringsAsFactors = FALSE
)

.metodo_esperado_especificacion <- function(nombre) {
  switch(
    nombre,
    NoNulo = .metodo_no_nulo,
    ValoresPosiblesPorExtension = .metodo_valores_posibles,
    Formato = .metodo_formato,
    ValoresPosiblesPorComprension = .metodo_valores_comprension,
    OportunidadAtributoPorFecha = .metodo_oportunidad_fecha,
    OportunidadAtributoPorIntervalo = .metodo_oportunidad_intervalo,
    OportunidadEntPorFecha = .metodo_oportunidad_entidad_fecha,
    OportunidadEntPorIntervalo = .metodo_oportunidad_entidad_intervalo,
    AtributoDuplicado = .metodo_atributo_duplicado,
    ConjuntoAtributosDuplicado = .metodo_conjunto_duplicado,
    ReglaIntegridadIntraEntidad = .metodo_regla_intra,
    ReglaIntegridadInterEntidad = .metodo_regla_inter,
    NULL
  )
}

.especificacion_metrica <- function(instancia) {
  if (!.metodo_del_paquete(instancia$metodo)) return(NULL)
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  esperado <- .metodo_esperado_especificacion(nombre)
  if (is.null(esperado) || !identical(instancia$metodo, esperado)) return(NULL)
  cfg <- instancia$configuracion
  entidad <- if (length(instancia$entidad)) instancia$entidad[[1L]] else NA_character_
  atributos <- as.character(unlist(instancia[["atributos"]]))
  base <- list(
    metrica = nombre,
    entidad = entidad,
    atributos = atributos,
    granularidad = as.character(instancia$declaracion$granularidad)[[1L]]
  )
  escalar <- function(x) length(x) == 1L
  primer_atr <- if (length(atributos)) atributos[[1L]] else NA_character_
  spec <- switch(
    nombre,
    NoNulo = list(
      nodo = "no_ausente",
      columna = primer_atr,
      valores_nulos = cfg$valores_nulos,
      aplicable = cfg$aplicable,
      opcional = isTRUE(cfg$opcional),
      objeto = "celda",
      unidad = "celda"
    ),
    ValoresPosiblesPorExtension = list(
      nodo = "en_conjunto",
      columna = primer_atr,
      conjunto = cfg$valores,
      objeto = "celda",
      unidad = "celda"
    ),
    Formato = if (!is.null(cfg$diccionario)) list(
      nodo = "en_conjunto",
      columna = primer_atr,
      conjunto = cfg$diccionario,
      objeto = "celda",
      unidad = "celda"
    ),
    ValoresPosiblesPorComprension = if (is.null(cfg$predicado)) list(
      nodo = "en_rango",
      columna = primer_atr,
      minimo = cfg$minimo,
      maximo = cfg$maximo,
      inclusivo = cfg$inclusivo,
      objeto = "celda",
      unidad = "celda"
    ),
    OportunidadAtributoPorFecha = if (escalar(cfg$fecha_limite)) list(
      nodo = "fecha_en_rango",
      columna = primer_atr,
      desde = NULL,
      hasta = cfg$fecha_limite,
      contrato_columna = FALSE,
      objeto = "celda",
      unidad = "celda"
    ),
    OportunidadAtributoPorIntervalo = if (escalar(cfg$inicio_vigencia) &&
                                          escalar(cfg$fin_vigencia)) list(
      nodo = "fecha_en_rango",
      columna = primer_atr,
      desde = cfg$inicio_vigencia,
      hasta = cfg$fin_vigencia,
      contrato_columna = FALSE,
      objeto = "celda",
      unidad = "celda"
    ),
    OportunidadEntPorFecha = {
      v <- cfg$vigencia
      if (is.null(v$fecha_limite)) {
        list(nodo = "abstencion", intervalo = FALSE)
      } else if (escalar(v$fecha_limite)) {
        list(
          nodo = "fecha_en_rango",
          columna = v$columna_actualizacion,
          desde = NULL,
          hasta = v$fecha_limite,
          contrato_columna = TRUE,
          objeto = "fila",
          unidad = "fila"
        )
      }
    },
    OportunidadEntPorIntervalo = {
      v <- cfg$vigencia
      if (is.null(v$inicio_intervalo) || is.null(v$fin_intervalo)) {
        list(nodo = "abstencion", intervalo = TRUE)
      } else if (escalar(v$inicio_intervalo) && escalar(v$fin_intervalo)) {
        list(
          nodo = "fecha_en_rango",
          columna = v$columna_actualizacion,
          desde = v$inicio_intervalo,
          hasta = v$fin_intervalo,
          contrato_columna = TRUE,
          objeto = "fila",
          unidad = "fila"
        )
      }
    },
    AtributoDuplicado = list(
      nodo = "duplicado",
      columnas = primer_atr,
      excluir_na = TRUE,
      objeto = "celda",
      unidad = "celda"
    ),
    ConjuntoAtributosDuplicado = list(
      nodo = "duplicado",
      columnas = atributos,
      excluir_na = FALSE,
      objeto = "fila_conjunto",
      unidad = "fila"
    ),
    ReglaIntegridadIntraEntidad = if (inherits(cfg$regla, "formula")) list(
      nodo = "predicado_fila",
      formula = cfg$regla,
      objeto = "fila",
      unidad = "fila"
    ),
    ReglaIntegridadInterEntidad = list(
      nodo = "cobertura_referencial",
      entidad_ref = if (length(instancia$entidad) >= 1L) instancia$entidad[[1L]] else NA_character_,
      pk = if (length(atributos) >= 1L) atributos[[1L]] else NA_character_,
      entidad_dep = if (length(instancia$entidad) >= 2L) instancia$entidad[[2L]] else NA_character_,
      fk = if (length(atributos) >= 2L) atributos[[2L]] else NA_character_,
      objeto = "relacion",
      unidad = "fila"
    ),
    NULL
  )
  if (is.null(spec)) return(NULL)
  c(base, spec)
}

# ---------------------------------------------------------------------------
# Recorrido seguro del arbol de formulas (sin `eval()` sobre codigo arbitrario)
# ---------------------------------------------------------------------------
# Una formula nunca se evalua tal cual en R: primero se recorre el arbol entero
# aceptando solo columnas visibles, literales escritos en la formula y una
# lista cerrada de operadores y constructores puros de constantes; recien
# despues se interpreta nodo por nodo llamando directamente a los operadores
# de `base` (y `%in%` del paquete).

.OPERADORES_FORMULA_FILA <- c(
  "(", "!", "&", "|", "==", "!=", "<", "<=", ">", ">=", "%in%", "is.na",
  "+", "-", "*", "/", "^", "%%", "%/%"
)

.CONSTRUCTORES_CONSTANTE_FORMULA <- c(
  "c", "character", "integer", "numeric", "double", "logical",
  "as.Date", "as.POSIXct", ":", "["
)

.detener_ast_formula <- function(...) {
  .detener(structure(
    class = c("lupa_formula_no_admitida", "error", "condition"),
    list(message = .mensaje_condicion(...), call = NULL)
  ))
}

.valor_dato_formula_valido <- function(val) {
  if (is.function(val) || is.environment(val) || is.call(val) ||
      is.expression(val) || inherits(val, "formula") || is.pairlist(val) ||
      is.name(val)) {
    return(FALSE)
  }
  is.null(val) || is.atomic(val) ||
    inherits(val, c("Date", "POSIXct", "POSIXlt", "factor", "integer64"))
}

.LITERALES_NOMBRE_FORMULA <- c(
  "TRUE", "FALSE", "NA", "NA_integer_", "NA_real_", "NA_character_",
  "Inf", "NaN"
)

.valor_literal_formula <- function(nombre) {
  if (!nombre %in% .LITERALES_NOMBRE_FORMULA) {
    .detener_ast_formula(
      "el s\u00edmbolo `", nombre,
      "` no es un atributo ligado ni un literal escrito en la f\u00f3rmula."
    )
  }
  switch(
    nombre,
    "TRUE" = TRUE,
    "FALSE" = FALSE,
    "NA" = NA,
    "NA_integer_" = NA_integer_,
    "NA_real_" = NA_real_,
    "NA_character_" = NA_character_,
    "Inf" = Inf,
    "NaN" = NaN
  )
}

.buscar_simbolo_formula_seguro <- function(nombre, entorno) {
  .valor_literal_formula(nombre)
}

# Valida el arbol entero sin ejecutar ninguna funcion del entorno. Devuelve
# `list(usa_columna = logical(1L))`.
.validar_ast_formula <- function(expr, columnas_visibles, entorno) {
  if (is.null(expr)) return(list(usa_columna = FALSE))
  if (is.atomic(expr)) return(list(usa_columna = FALSE))
  if (is.symbol(expr)) {
    nombre <- as.character(expr)
    if (!nzchar(nombre)) {
      .detener_ast_formula("la f\u00f3rmula contiene un argumento vac\u00edo.")
    }
    if (!is.na(.indice_nombre(nombre, columnas_visibles))) {
      return(list(usa_columna = TRUE))
    }
    .valor_literal_formula(nombre)
    return(list(usa_columna = FALSE))
  }
  if (!is.call(expr)) {
    .detener_ast_formula(
      "la f\u00f3rmula contiene un nodo no admitido de clase `",
      class(expr)[[1L]], "`."
    )
  }
  cabeza <- expr[[1L]]
  if (!is.symbol(cabeza)) {
    .detener_ast_formula(
      "la f\u00f3rmula no admite llamadas indirectas o calificadas."
    )
  }
  op <- as.character(cabeza)
  if (!op %in% c(.OPERADORES_FORMULA_FILA, .CONSTRUCTORES_CONSTANTE_FORMULA)) {
    .detener_ast_formula(
      "la f\u00f3rmula no admite `", op,
      "()`: s\u00f3lo se admiten comparaciones, `&`, `|`, `!`, aritm\u00e9tica, ",
      "`%in%`, `is.na`, par\u00e9ntesis y literales."
    )
  }
  args <- as.list(expr)[-1L]
  nombres_args <- names(args)
  if (is.null(nombres_args)) nombres_args <- rep("", length(args))
  if (op %in% c("(", "!", "is.na") && length(args) != 1L) {
    .detener_ast_formula("`", op, "` requiere un argumento.")
  }
  if (op %in% c("&", "|", "==", "!=", "<", "<=", ">", ">=", "%in%",
                "*", "/", "^", "%%", "%/%", ":") && length(args) != 2L) {
    .detener_ast_formula("`", op, "` requiere dos argumentos.")
  }
  if (op %in% c("+", "-") && !length(args) %in% c(1L, 2L)) {
    .detener_ast_formula("`", op, "` requiere uno o dos argumentos.")
  }
  if (op %in% c("character", "integer", "numeric", "double", "logical") &&
      !length(args) %in% c(0L, 1L)) {
    .detener_ast_formula("`", op, "()` requiere cero o un argumento.")
  }
  if (op %in% c("as.Date", "as.POSIXct")) {
    if (!length(args)) {
      .detener_ast_formula("`", op, "()` requiere al menos un argumento.")
    }
    permitidos <- c("", "x", "format", "tryFormats", "origin", "tz")
    malos <- setdiff(nombres_args, permitidos)
    if (length(malos)) {
      .detener_ast_formula(
        "`", op, "()` no admite el argumento `", malos[[1L]], "` en la f\u00f3rmula."
      )
    }
  }
  if (identical(op, "[") && length(args) != 2L) {
    .detener_ast_formula("`[` en una f\u00f3rmula s\u00f3lo admite un \u00edndice constante.")
  }
  # Se recorren todos los argumentos antes de cualquier otra comprobacion para
  # que ninguna rama invalida quede sin revisar.
  usa <- FALSE
  for (i in seq_along(args)) {
    sub <- .validar_ast_formula(args[[i]], columnas_visibles, entorno)
    if (isTRUE(sub$usa_columna)) usa <- TRUE
  }
  if (op %in% .CONSTRUCTORES_CONSTANTE_FORMULA && usa) {
    .detener_ast_formula(
      "`", op, "()` en una f\u00f3rmula s\u00f3lo se admite sobre constantes."
    )
  }
  list(usa_columna = usa)
}

# Interpreta un arbol ya validado por `.validar_ast_formula()`, sin llamar a
# `eval()`. Si `datos` es NULL, el arbol no puede nombrar columnas.
.evaluar_ast_validado <- function(expr, datos, entorno) {
  if (is.null(expr) || is.atomic(expr)) return(expr)
  if (is.symbol(expr)) {
    nombre <- as.character(expr)
    if (!is.null(datos)) {
      pos <- .indice_nombre(nombre, names(datos))
      if (!is.na(pos)) return(datos[[pos]])
    }
    return(.valor_literal_formula(nombre))
  }
  op <- as.character(expr[[1L]])
  args <- as.list(expr)[-1L]
  valores <- lapply(args, .evaluar_ast_validado, datos = datos, entorno = entorno)
  if (!is.null(names(args))) names(valores) <- names(args)
  switch(
    op,
    "(" = valores[[1L]],
    "!" = base::"!"(valores[[1L]]),
    "&" = base::"&"(valores[[1L]], valores[[2L]]),
    "|" = base::"|"(valores[[1L]], valores[[2L]]),
    "==" = base::"=="(valores[[1L]], valores[[2L]]),
    "!=" = base::"!="(valores[[1L]], valores[[2L]]),
    "<" = base::"<"(valores[[1L]], valores[[2L]]),
    "<=" = base::"<="(valores[[1L]], valores[[2L]]),
    ">" = base::">"(valores[[1L]], valores[[2L]]),
    ">=" = base::">="(valores[[1L]], valores[[2L]]),
    "%in%" = valores[[1L]] %in% valores[[2L]],
    "is.na" = base::is.na(valores[[1L]]),
    "+" = if (length(valores) == 1L) base::"+"(valores[[1L]]) else
      base::"+"(valores[[1L]], valores[[2L]]),
    "-" = if (length(valores) == 1L) base::"-"(valores[[1L]]) else
      base::"-"(valores[[1L]], valores[[2L]]),
    "*" = base::"*"(valores[[1L]], valores[[2L]]),
    "/" = base::"/"(valores[[1L]], valores[[2L]]),
    "^" = base::"^"(valores[[1L]], valores[[2L]]),
    "%%" = base::"%%"(valores[[1L]], valores[[2L]]),
    "%/%" = base::"%/%"(valores[[1L]], valores[[2L]]),
    "c" = do.call(base::c, valores),
    "character" = , "integer" = , "numeric" = , "double" = , "logical" = {
      n <- if (!length(valores)) 0L else valores[[1L]]
      if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
          n < 0 || n > 100000 || n != floor(n)) {
        .detener_ast_formula(
          "`", op, "()` requiere un largo entero no negativo acotado."
        )
      }
      fn <- switch(
        op,
        character = base::character,
        integer = base::integer,
        numeric = base::numeric,
        double = base::double,
        logical = base::logical
      )
      fn(as.integer(n))
    },
    "as.Date" = do.call(base::as.Date, valores),
    "as.POSIXct" = do.call(base::as.POSIXct, valores),
    ":" = {
      a <- valores[[1L]]
      b <- valores[[2L]]
      if (!is.numeric(a) || !is.numeric(b) || length(a) != 1L ||
          length(b) != 1L || is.na(a) || is.na(b) || !is.finite(a) ||
          !is.finite(b) || abs(b - a) > 100000) {
        .detener_ast_formula("`:` requiere dos extremos num\u00e9ricos finitos.")
      }
      base::":"(a, b)
    },
    "[" = {
      base_vec <- valores[[1L]]
      idx <- valores[[2L]]
      if (!is.atomic(base_vec) || is.object(base_vec)) {
        .detener_ast_formula(
          "`[` en una f\u00f3rmula s\u00f3lo admite vectores at\u00f3micos sin clase."
        )
      }
      if (!is.numeric(idx) && !is.logical(idx)) {
        .detener_ast_formula(
          "`[` en una f\u00f3rmula requiere un \u00edndice num\u00e9rico o l\u00f3gico."
        )
      }
      base::"["(base_vec, idx)
    },
    .detener_ast_formula("operador no admitido: `", op, "`.")
  )
}

.evaluar_constante_formula_segura <- function(expr, entorno) {
  info <- .validar_ast_formula(expr, character(), entorno)
  if (isTRUE(info$usa_columna)) {
    .detener_ast_formula("la subexpresi\u00f3n no es constante.")
  }
  .evaluar_ast_validado(expr, datos = NULL, entorno = entorno)
}

# La formula de ReglaIntegridadIntraEntidad ve solo los atributos ligados,
# igual que `function(d)` recibe `.seleccionar_columnas(tabla, atributos)`.
.evaluar_regla_formula <- function(datos_regla, formula) {
  expr <- formula[[2L]]
  entorno <- environment(formula)
  .validar_ast_formula(expr, names(datos_regla), entorno)
  crudo <- .evaluar_ast_validado(expr, datos = datos_regla, entorno = entorno)
  if (is.logical(crudo) && length(crudo) == 1L && nrow(datos_regla) != 1L) {
    crudo <- rep(crudo, nrow(datos_regla))
  }
  crudo
}

# ---------------------------------------------------------------------------
# Evaluador en R de la especificacion (fuente unica para memoria y resumen)
# ---------------------------------------------------------------------------

.MENSAJES_MEZCLA_HUSOS <- list(
  no_ausente = paste0(
    "as\u00ed que el centinela no coincide con ning\u00fan valor y la m\u00e9trica publica ",
    "como presentes las ausencias disfrazadas"
  ),
  en_conjunto_extension = paste0(
    "as\u00ed que un valor que S\u00cd est\u00e1 en el dominio no coincide con ninguno y la ",
    "m\u00e9trica publica un incumplimiento que no existe"
  ),
  en_conjunto_formato = paste0(
    "as\u00ed que ning\u00fan valor coincide con el diccionario y la m\u00e9trica publica ",
    "un incumplimiento que no existe"
  ),
  en_rango = paste0(
    "as\u00ed que el rango no se puede comparar con la columna y la m\u00e9trica ",
    "publica que ning\u00fan valor lo cumple"
  )
)

.evaluar_especificacion <- function(spec, tablas, instancia,
                                    mascara_aplicable = NULL) {
  nodo <- spec$nodo
  if (identical(nodo, "cobertura_referencial")) {
    return(.r_cobertura_referencial(spec, tablas, instancia))
  }
  if (identical(nodo, "abstencion")) {
    return(list(abstencion = .r_abstencion_oportunidad(spec, tablas, instancia)))
  }
  tabla <- .obtener_tabla_modelo(tablas, spec$entidad)
  switch(
    nodo,
    no_ausente = .r_no_ausente(
      spec, tabla, instancia, mascara_aplicable = mascara_aplicable
    ),
    en_conjunto = .r_en_conjunto(spec, tabla, instancia),
    en_rango = .r_en_rango(spec, tabla, instancia),
    fecha_en_rango = .r_fecha_en_rango(spec, tabla, instancia),
    duplicado = .r_duplicado(spec, tabla, instancia),
    predicado_fila = .r_predicado_fila(spec, tabla, instancia),
    .detener("Nodo de especificaci\u00f3n sin evaluador: ", nodo, call. = FALSE)
  )
}

.r_abstencion_oportunidad <- function(spec, tablas, instancia) {
  # Valida entidad y columna antes de abstenerse, igual que las demas metricas
  # de vigencia.
  .actualizaciones_vigencia(tablas, instancia)
  if (isTRUE(spec$intervalo)) {
    return(.abstener_metodo(
      instancia,
      paste0(
        "OportunidadEntPorIntervalo no se midi\u00f3: el contrato de vigencia ",
        "no trae el intervalo -`inicio_intervalo` y `fin_intervalo`- contra el ",
        "que se compara la actualizaci\u00f3n."
      ),
      paste0(
        "Declarar el intervalo en `vigencia()`. Una m\u00e9trica que se ",
        "abstiene no se interpreta como cero."
      )
    ))
  }
  .abstener_metodo(
    instancia,
    paste0(
      "OportunidadEntPorFecha no se midi\u00f3: el contrato de vigencia no ",
      "trae `fecha_limite`, que es la fecha contra la que se compara la ",
      "actualizaci\u00f3n."
    ),
    paste0(
      "Declarar `fecha_limite` en `vigencia()`. Una m\u00e9trica que se ",
      "abstiene no se interpreta como cero."
    )
  )
}

.r_no_ausente <- function(spec, tabla, instancia, mascara_aplicable = NULL) {
  .validar_vinculo(instancia, 1L, 1L)
  x <- .obtener_columna_modelo(tabla, spec$columna, spec$entidad)
  filas <- seq_len(nrow(tabla))
  dentro <- .mascara_aplicable_instancia(
    tabla, instancia, spec$entidad, mascara = mascara_aplicable
  )
  if (!all(dentro)) {
    filas <- filas[dentro]
    x <- x[dentro]
  }
  ausente <- is.na(x)
  valores_nulos <- spec$valores_nulos
  if (length(valores_nulos)) {
    .declarar_mezcla_de_husos(
      instancia$declaracion$nombre, spec$columna, x, list(valores_nulos),
      consecuencia = .MENSAJES_MEZCLA_HUSOS$no_ausente
    )
    es_texto <- is.character(x) || is.factor(x) ||
      is.character(valores_nulos) || is.factor(valores_nulos)
    coincide <- if (es_texto) {
      .minusculas_para_comparar(.texto_analizable(x)$valores) %in%
        .minusculas_para_comparar(.texto_analizable(valores_nulos)$valores)
    } else {
      .en_conjunto_por_valor(x, valores_nulos)
    }
    ausente <- ausente | (!is.na(x) & coincide)
  }
  list(filas = filas, cumple = !ausente)
}

.r_en_conjunto <- function(spec, tabla, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  x <- .obtener_columna_modelo(tabla, spec$columna, spec$entidad)
  texto <- is.character(x) || is.factor(x)
  if (texto) x <- .texto_analizable(x)$valores
  filas <- which(!is.na(x))
  valores <- x[!is.na(x)]
  conjunto <- spec$conjunto
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, spec$columna, valores, list(conjunto),
    consecuencia = if (identical(spec$metrica, "Formato")) {
      .MENSAJES_MEZCLA_HUSOS$en_conjunto_formato
    } else {
      .MENSAJES_MEZCLA_HUSOS$en_conjunto_extension
    }
  )
  cumple <- if (texto || is.character(conjunto) || is.factor(conjunto)) {
    .clave_bytes(valores) %in% .clave_bytes(.texto_analizable(conjunto)$valores)
  } else {
    .en_conjunto_por_valor(valores, conjunto)
  }
  list(filas = filas, cumple = cumple)
}

.r_en_rango <- function(spec, tabla, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  x <- .obtener_columna_modelo(tabla, spec$columna, spec$entidad)
  x <- .texto_analizable(x)$valores
  filas <- which(!is.na(x))
  valores <- x[filas]
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, spec$columna, valores,
    list(spec$minimo, spec$maximo),
    consecuencia = .MENSAJES_MEZCLA_HUSOS$en_rango
  )
  izquierda <- if (spec$inclusivo[[1L]]) {
    valores >= spec$minimo
  } else {
    valores > spec$minimo
  }
  derecha <- if (spec$inclusivo[[2L]]) {
    valores <= spec$maximo
  } else {
    valores < spec$maximo
  }
  cumple <- izquierda & derecha
  if (!is.logical(cumple) || anyNA(cumple)) {
    .detener("El rango no se puede comparar con el atributo ligado.",
             call. = FALSE)
  }
  list(filas = filas, cumple = cumple)
}

.r_fecha_en_rango <- function(spec, tabla, instancia) {
  if (isTRUE(spec$contrato_columna)) {
    if (length(instancia$entidad) != 1L) {
      .detener(instancia$declaracion$nombre, " requiere una entidad.",
               call. = FALSE)
    }
  } else {
    .validar_vinculo(instancia, 1L, 1L)
  }
  x <- .obtener_columna_modelo(tabla, spec$columna, spec$entidad)
  if (!.es_fecha_modelo(x)) {
    .detener(
      if (isTRUE(spec$contrato_columna)) {
        "La columna de actualizaci\u00f3n debe ser Date o POSIXt."
      } else {
        "La oportunidad requiere un atributo Date o POSIXt."
      },
      call. = FALSE
    )
  }
  filas <- which(!is.na(x))
  entrega <- .fecha_numerica(x[filas])
  .declarar_mezcla_de_husos(
    instancia$declaracion$nombre, spec$columna, x,
    Filter(Negate(is.null), list(spec$desde, spec$hasta))
  )
  cumple <- rep(TRUE, length(filas))
  if (!is.null(spec$desde)) cumple <- entrega >= .fecha_numerica(spec$desde)
  if (!is.null(spec$hasta)) {
    cumple <- if (is.null(spec$desde)) {
      entrega <= .fecha_numerica(spec$hasta)
    } else {
      cumple & entrega <= .fecha_numerica(spec$hasta)
    }
  }
  list(filas = filas, cumple = cumple)
}

.r_duplicado <- function(spec, tabla, instancia) {
  if (isTRUE(spec$excluir_na)) {
    .validar_vinculo(instancia, 1L, 1L)
    x <- .obtener_columna_modelo(tabla, spec$columnas, spec$entidad)
    x <- .texto_analizable(x)$valores
    filas <- which(!is.na(x))
    return(list(filas = filas, cumple = .duplicados_completos(x[filas])))
  }
  if (length(instancia$entidad) != 1L) {
    .detener(
      "ConjuntoAtributosDuplicado requiere una entidad y al menos dos atributos.",
      call. = FALSE
    )
  }
  .validar_atributos_tabla(tabla, instancia, minimo = 2L)
  valores <- .seleccionar_columnas(tabla, spec$columnas)
  list(filas = seq_len(nrow(tabla)), cumple = .duplicados_completos(valores))
}

.r_predicado_fila <- function(spec, tabla, instancia) {
  if (length(instancia$entidad) != 1L || !length(instancia$atributos)) {
    .detener(
      "ReglaIntegridadIntraEntidad requiere una entidad y al menos un atributo.",
      call. = FALSE
    )
  }
  indices <- .indice_nombre(instancia$atributos, names(tabla))
  faltantes <- instancia$atributos[is.na(indices)]
  if (length(faltantes)) {
    .detener(
      "No se encontraron atributos ligados: ",
      paste(faltantes, collapse = ", "), ".", call. = FALSE
    )
  }
  datos_regla <- .seleccionar_columnas(tabla, instancia$atributos)
  cumple <- .resultado_validador(
    .evaluar_regla_formula(datos_regla, spec$formula),
    nrow(tabla),
    "ReglaIntegridadIntraEntidad"
  )
  list(filas = seq_len(nrow(tabla)), cumple = cumple)
}

.r_cobertura_referencial <- function(spec, tablas, instancia) {
  .validar_vinculo(instancia, 2L, 2L)
  ref <- .obtener_tabla_modelo(tablas, spec$entidad_ref)
  dep <- .obtener_tabla_modelo(tablas, spec$entidad_dep)
  pk <- .obtener_columna_modelo(ref, spec$pk, spec$entidad_ref)
  fk <- .obtener_columna_modelo(dep, spec$fk, spec$entidad_dep)
  if (!nrow(dep)) return(list(vacia = TRUE))
  .cobertura_referencial_par(
    pk, fk, muestra = instancia$configuracion$muestra
  )
}

.objeto_por_fila <- function(spec, filas) {
  switch(
    spec$objeto,
    celda = .pegar_texto0(
      spec$entidad, "$", spec$columna %||% spec$columnas, "[", filas, "]"
    ),
    fila = paste0(spec$entidad, "[", filas, ",]"),
    fila_conjunto = paste0(
      spec$entidad, "[", filas, ",",
      paste(spec$columnas, collapse = "+"), "]"
    )
  )
}

.metodo_especificado <- function(tablas, instancia) {
  spec <- .especificacion_metrica(instancia)
  ev <- .evaluar_especificacion(spec, tablas, instancia)
  if (!is.null(ev$abstencion)) return(ev$abstencion)
  if (identical(spec$nodo, "cobertura_referencial")) {
    if (isTRUE(ev$vacia)) {
      return(.salida_metodo(
        numeric(), spec$entidad_dep, spec$fk, integer(), character()
      ))
    }
    return(.salida_metodo(
      ev$cobertura, spec$entidad_dep, spec$fk, NA_integer_,
      .pegar_texto0(
        spec$entidad_dep, "$", spec$fk, " -> ",
        spec$entidad_ref, "$", spec$pk
      )
    ))
  }
  atributo <- switch(
    spec$objeto,
    celda = spec$columna %||% spec$columnas,
    fila = if (identical(spec$nodo, "fecha_en_rango")) spec$columna else
      NA_character_,
    fila_conjunto = NA_character_
  )
  .salida_metodo(
    ev$cumple, spec$entidad, atributo, ev$filas,
    .objeto_por_fila(spec, ev$filas)
  )
}
