# El universo aplicable de una columna.
#
# `lupa` contaba toda celda vacia como ausencia, y eso confunde dos cosas que no
# son la misma: la celda que deberia tener un valor y no lo tiene, y la celda que
# no corresponde que lo tenga. Un historial con vigencia abierta, una encuesta
# con salto de patron, un modelo entidad-atributo-valor y una tabla con columnas
# excluyentes por subtipo llenan de vacios legitimos una tabla sana, y el
# diagnostico los informaba como defecto.
#
# La decision es que lo declara quien conoce el dato. `lupa` no adivina el
# modelo: si nadie declara nada, toda la columna aplica y el comportamiento es
# el de siempre.
#
# Dos formas, de menor a mayor alcance:
#
#   columnas_opcionales = "valido_hasta"
#     la ausencia nunca es defecto en esa columna.
#
#   aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
#     la columna solo corresponde en las filas donde el predicado es verdadero.
#
# El predicado puede ser indeterminado, porque la columna que discrimina tambien
# puede faltar. Esas filas no se cuentan como aplicables ni como no aplicables:
# se declaran aparte, que es la unica lectura honesta.

.formula_a_texto <- function(f) {
  paste(deparse(f), collapse = " ")
}

.funciones_aplicabilidad_no_fila <- c(
  "all", "any", "length", "max", "mean", "median", "min", "nrow",
  "quantile", "range", "sd", "sort", "sum", "table", "unique", "var"
)

.aplicabilidad_es_predicado_fila <- function(expr) {
  nombres <- tryCatch(
    all.names(expr, functions = TRUE),
    error = function(e) character()
  )
  !any(nombres %in% .funciones_aplicabilidad_no_fila)
}

.validar_aplicabilidad <- function(nombres, columnas_opcionales, aplicabilidad) {
  if (!is.character(columnas_opcionales)) {
    stop("`columnas_opcionales` debe ser un vector de nombres de columnas.",
         call. = FALSE)
  }
  indices_opcionales <- .indice_nombre(columnas_opcionales, nombres)
  desconocidas <- columnas_opcionales[is.na(indices_opcionales)]
  if (length(desconocidas)) {
    stop("`columnas_opcionales` nombra columnas inexistentes: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(aplicabilidad)) return(invisible(NULL))
  if (!is.list(aplicabilidad) || is.null(names(aplicabilidad)) ||
      any(!nzchar(names(aplicabilidad)))) {
    stop("`aplicabilidad` debe ser una lista con nombre por columna.",
         call. = FALSE)
  }
  indices_aplicabilidad <- .indice_nombre(names(aplicabilidad), nombres)
  desconocidas <- names(aplicabilidad)[is.na(indices_aplicabilidad)]
  if (length(desconocidas)) {
    stop("`aplicabilidad` nombra columnas inexistentes: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  no_formulas <- names(aplicabilidad)[
    !vapply(aplicabilidad, function(f) inherits(f, "formula") && length(f) == 2L,
            logical(1L))
  ]
  if (length(no_formulas)) {
    stop(
      "Cada elemento de `aplicabilidad` debe ser una formula de un solo lado, ",
      "como `~ tiene_auto == \"Si\"`. No lo son: ",
      paste(no_formulas, collapse = ", "), ".", call. = FALSE
    )
  }
  ambas <- nombres[intersect(
    indices_opcionales[!is.na(indices_opcionales)],
    indices_aplicabilidad[!is.na(indices_aplicabilidad)]
  )]
  if (length(ambas)) {
    stop(
      "Estas columnas estan declaradas en `columnas_opcionales` y en ",
      "`aplicabilidad` a la vez: ", paste(ambas, collapse = ", "),
      ". Corresponde una sola de las dos.", call. = FALSE
    )
  }
  invisible(NULL)
}

.entorno_aplicabilidad <- function(datos, expr, padre) {
  entorno <- new.env(parent = padre)
  simbolos <- unique(all.names(expr, functions = FALSE))
  nombres <- names(datos)
  indices <- .indice_nombre(simbolos, nombres)
  indices <- indices[!is.na(indices)]
  if (!length(indices)) {
    return(list(entorno = entorno, expresion = expr))
  }
  indices <- unique(indices)
  alias <- paste0(".lupa_aplicabilidad_", indices)
  for (i in seq_along(indices)) {
    assign(alias[[i]], datos[[indices[[i]]]], envir = entorno)
  }
  reemplazar <- function(x) {
    if (is.symbol(x)) {
      posicion <- .indice_nombre(as.character(x), nombres)
      if (!is.na(posicion)) return(as.name(alias[[match(posicion, indices)]]))
      return(x)
    }
    if (is.call(x)) return(as.call(lapply(as.list(x), reemplazar)))
    x
  }
  list(entorno = entorno, expresion = reemplazar(expr))
}

.evaluar_predicado_aplicabilidad <- function(datos, columna, f) {
  n <- nrow(datos)
  if (!.aplicabilidad_es_predicado_fila(f[[2L]])) {
    stop(
      "aplicabilidad_no_fila:", .formula_a_texto(f),
      ". La regla debe evaluar una condicion por fila; no puede calcular un ",
      "estadistico global.", call. = FALSE
    )
  }
  # `eval(..., envir = data.frame)` y `list2env(as.list(datos))` intentan
  # traducir los nombres de la tabla a la codificacion nativa. Bajo
  # `LC_CTYPE = "C"` eso vuelve a advertir con un nombre UTF-8 valido,
  # aunque la regla no lo mencione. Un entorno poblado con `assign()` conserva
  # los nombres byte a byte y permite que una formula que si los referencia
  # (con backticks) siga funcionando.
  preparado <- .entorno_aplicabilidad(datos, f[[2L]], environment(f))
  valor <- tryCatch(
    eval(preparado$expresion, envir = preparado$entorno,
         enclos = environment(f)),
    error = function(e) e
  )
  if (inherits(valor, "condition")) {
    stop(
      "No se pudo evaluar la regla de aplicabilidad de `", columna, "`: ",
      conditionMessage(valor), call. = FALSE
    )
  }
  if (!is.logical(valor)) {
    stop(
      "La regla de aplicabilidad de `", columna,
      "` debe dar un valor logico y dio ", class(valor)[[1L]], ".", call. = FALSE
    )
  }
  if (length(valor) == 1L) valor <- rep(valor, n)
  if (length(valor) != n) {
    stop(
      "La regla de aplicabilidad de `", columna, "` dio ", length(valor),
      " valores y la tabla tiene ", n, " filas.", call. = FALSE
    )
  }
  valor
}

.resolver_aplicabilidad <- function(datos, nombres, columnas_opcionales = character(),
                                    aplicabilidad = NULL) {
  .validar_aplicabilidad(nombres, columnas_opcionales, aplicabilidad)
  columnas_opcionales <- .nombres_resueltos(columnas_opcionales, nombres)
  if (!is.null(aplicabilidad)) {
    indices <- .indice_nombre(names(aplicabilidad), nombres)
    names(aplicabilidad) <- nombres[indices]
  }
  n_columnas <- length(nombres)
  mascaras <- vector("list", n_columnas)
  reglas <- list()

  for (i in seq_len(n_columnas)) {
    columna <- nombres[[i]]
    if (.nombres_para_operar(columna) %in%
        .nombres_para_operar(columnas_opcionales)) {
      # Declarada opcional: aplica donde hay valor, y la ausencia no es defecto.
      mascara <- !is.na(datos[[i]])
      indeterminados <- 0L
      origen <- "columnas_opcionales"
      regla <- "la ausencia no es defecto en esta columna"
    } else if (!is.null(aplicabilidad) &&
               !is.na(.indice_nombre(columna, names(aplicabilidad)))) {
      indice_aplicabilidad <- .indice_nombre(columna, names(aplicabilidad))
      crudo <- .evaluar_predicado_aplicabilidad(
        datos, columna, aplicabilidad[[indice_aplicabilidad]]
      )
      indeterminados <- sum(is.na(crudo))
      mascara <- !is.na(crudo) & crudo
      origen <- "aplicabilidad"
      regla <- .formula_a_texto(aplicabilidad[[indice_aplicabilidad]])
    } else {
      next
    }
    # El conteo de indeterminados viaja con la mascara para que la fila del
    # perfil no los confunda con filas donde la columna no corresponde: no
    # saber no es lo mismo que no aplicar.
    attr(mascara, "n_indeterminados") <- indeterminados
    # Y la mascara POR FILA tambien, porque el conteo solo no alcanza: quien
    # cuenta "valores presentes fuera del universo" necesita saber CUALES filas
    # son indeterminadas para no tragarselas. Sin esto, las filas donde el
    # predicado no se pudo decidir se contaban dos veces -como indeterminadas,
    # bien, y como presentes fuera del universo, mal- y disparaban un hallazgo
    # que dice "la regla declarada dice que no corresponde", que sobre una fila
    # indeterminada es falso.
    attr(mascara, "indeterminados_por_fila") <- if (indeterminados > 0L) {
      is.na(crudo)
    } else {
      rep(FALSE, length(mascara))
    }
    mascaras[[i]] <- mascara
    reglas[[length(reglas) + 1L]] <- data.frame(
      columna = columna, origen = origen, regla = regla,
      n_aplicables = sum(mascara),
      n_no_aplica = sum(!mascara) - indeterminados,
      n_indeterminados = indeterminados,
      stringsAsFactors = FALSE
    )
  }

  list(
    mascaras = mascaras,
    reglas = if (length(reglas)) {
      do.call(rbind, reglas)
    } else {
      data.frame(
        columna = character(), origen = character(), regla = character(),
        n_aplicables = integer(), n_no_aplica = integer(),
        n_indeterminados = integer(), stringsAsFactors = FALSE
      )
    }
  )
}

# Las reglas declaradas van a `cobertura_diagnosticos`, que es donde el usuario
# busca lo que no se midio y por que. Un universo recortado sin constancia seria
# el mismo defecto al reves.
.cobertura_aplicabilidad <- function(reglas) {
  if (!nrow(reglas)) return(NULL)
  motivo <- ifelse(
    reglas$origen == "columnas_opcionales",
    paste0(
      "La completitud se midi\u00f3 sobre ", reglas$n_aplicables,
      " celdas presentes: la columna se declar\u00f3 opcional, as\u00ed que la ausencia ",
      "no se cuenta como defecto."
    ),
    paste0(
      "La completitud se midi\u00f3 sobre ", reglas$n_aplicables,
      " filas aplicables de ", reglas$n_aplicables + reglas$n_no_aplica +
        reglas$n_indeterminados,
      "; la regla declarada es `", reglas$regla, "`",
      ifelse(
        reglas$n_indeterminados > 0L,
        paste0(" y en ", reglas$n_indeterminados,
               " filas la regla no se pudo determinar."),
        "."
      )
    )
  )
  data.frame(
    diagnostico = "faltantes",
    columna = reglas$columna,
    motivo = motivo,
    como_resolverlo = paste(
      "Revisar la regla declarada si el universo no es el esperado; sin",
      "declaraci\u00f3n, la columna se mide sobre todas las filas."
    ),
    dependencia = NA_character_,
    stringsAsFactors = FALSE
  )
}
