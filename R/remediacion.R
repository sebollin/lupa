.plan_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    grupo = character(), decision_grupo = character(),
    recomendacion_grupo = character(),
    estrategia = character(), recomendada = logical(),
    severidad_origen = character(), evidencia = character(),
    justificacion = character(), n_afectadas = numeric(),
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
                          estado_reparacion = NA_character_) {
  estructura <- data.frame(
    id_accion = "", columna = columna, hallazgo = hallazgo,
    grupo = grupo, decision_grupo = decision_grupo,
    recomendacion_grupo = recomendacion_grupo,
    estrategia = estrategia, recomendada = recomendada,
    severidad_origen = NA_character_, evidencia = "",
    justificacion = justificacion,
    n_afectadas = as.numeric(n_afectadas), reversible = reversible,
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
  transliterados <- iconv(originales, from = "UTF-8", to = "ASCII//TRANSLIT")
  transliterados[is.na(transliterados)] <- originales[is.na(transliterados)]
  salida <- tolower(trimws(transliterados))
  salida <- gsub("[^[:alnum:]]+", "_", salida, perl = TRUE)
  salida <- gsub("^_+|_+$", "", salida, perl = TRUE)
  salida[!nzchar(salida)] <- "x"
  salida[grepl("^[0-9]", salida)] <- paste0(
    "x_", salida[grepl("^[0-9]", salida)]
  )
  make.unique(salida, sep = "_")
}

.par_columnas_duplicadas <- function(perfil, hallazgo) {
  pares <- perfil$general$columnas_duplicadas
  if (is.null(pares) || !nrow(pares)) return(NULL)
  evidencias <- paste(pares$columna_1, "=", pares$columna_2)
  indices <- which(
    pares$columna_1 == hallazgo$columna[[1L]] &
      evidencias == hallazgo$evidencia[[1L]]
  )
  if (!length(indices)) {
    indices <- which(pares$columna_1 == hallazgo$columna[[1L]])
  }
  if (!length(indices)) return(NULL)
  unname(unlist(pares[indices[[1L]], c("columna_1", "columna_2")]))
}

.fila_perfil <- function(perfil, columna) {
  indices <- which(perfil$columnas$columna == columna)
  if (length(indices) == 1L) perfil$columnas[indices, , drop = FALSE] else NULL
}

.formatos_perfil <- function(perfil, columna) {
  indices <- which(perfil$columnas$columna == columna)
  if (length(indices) == 1L) perfil$formatos_fecha[[indices]] else NULL
}

.accion_columna_ambigua <- function(perfil, columna) {
  sum(perfil$columnas$columna == columna) != 1L
}

.estado_columna <- function(perfil, columna, estado = "lista") {
  if (.accion_columna_ambigua(perfil, columna)) "bloqueada" else estado
}

.es_fecha_ambigua <- function(perfil, columna) {
  any(
    perfil$hallazgos$columna == columna &
      perfil$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo",
    na.rm = TRUE
  )
}

.es_fecha_mixta <- function(perfil, columna) {
  any(
    perfil$hallazgos$columna == columna &
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
  if (!is.data.frame(datos) || !columna %in% names(datos)) {
    return(list(
      verificable = FALSE, ejecutable = FALSE, reversible = FALSE,
      n_no_reversibles = 0L,
      justificacion = paste0(
        "La columna `", columna,
        "` no est\u00e1 disponible para comprobar la reversibilidad."
      ), error = NULL
    ))
  }
  x <- datos[[columna]]
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
  comparacion <- .comparar_representacion_conversion(x, convertido)
  justificacion <- if (comparacion$reversible) {
    "La conversi\u00f3n y su representaci\u00f3n inversa reproducen todos los valores."
  } else {
    paste0(
      "La conversi\u00f3n cambia la representaci\u00f3n textual de ",
      comparacion$n_no_reversibles,
      " valores; se declara destructiva y no se recomienda autom\u00e1ticamente."
    )
  }
  list(
    verificable = TRUE, ejecutable = TRUE,
    reversible = comparacion$reversible,
    n_no_reversibles = comparacion$n_no_reversibles,
    justificacion = justificacion, error = NULL
  )
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
#' fija la secuencia reproducible. `n_afectadas` es la estimación del perfil y
#' el registro informa `n_cambiadas` sobre los datos recibidos. `reversible`
#' indica si el resultado puede deshacerse sólo con los datos transformados.
#' Las conversiones de tipo, número regional y fecha sólo se recomiendan cuando
#' se comprueban sobre todos los valores de `datos` y la representación textual
#' vuelve a ser idéntica valor por valor. Sin `datos` no se puede hacer esa
#' comprobación y la acción queda bloqueada. Cuando no es reversible se marca
#' `destructiva`, no se activa por defecto y el registro conserva
#' `n_no_reversibles` y la justificación de la decisión.
#' La acción de codificación prueba las tablas congeladas de varias
#' codificaciones y deja en `estado_reparacion` uno de `reparado`,
#' `reparado_parcialmente` o `no_se_pudo`. Una reparación parcial no se activa
#' automáticamente: debe revisarse y seleccionarse de forma explícita. La
#' estrategia nueva se llama `reparar_codificacion`. El nombre histórico
#' `reparar_codificacion_latin1` se acepta como alias para planes guardados,
#' aunque ya no limita el motor a latin-1.
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
#' Las imputaciones por dependencia funcional se ofrecen desactivadas. Aunque
#' una dependencia exacta permite deducir un valor sin usar media, moda o un
#' modelo externo, sigue siendo una regularidad aprendida de una sola entrega y
#' puede reflejar un error sistemático en vez de una regla de negocio. El plan
#' conserva el mapa y su soporte para que el usuario la confirme; sólo entonces
#' se aplica y se vuelve a validar contra los datos recibidos.
#'
#' `marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada` reproduce la
#' semántica de [duplicated()] y marca sólo las apariciones posteriores;
#' `.grupo_duplicado` identifica a **todas** las filas que participan en cada
#' grupo de contenido idéntico.
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
#'   Si una columna de entrada es un factor, las acciones que transforman su
#'   texto devuelven una columna `character`: no se reconstruyen los niveles
#'   originales, porque una limpieza puede introducir valores nuevos.
#' @export
#' @seealso [perfilar()], [guiar_limpieza()], [detectar_dependencias()]
#'
#' @examples
#' datos <- data.frame(categoria = c(" A", "S/D", "B"))
#' perfil <- perfilar(datos)
#' plan <- planificar_limpieza(perfil)
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
      nombre_marca <- paste0(".ausente_", make.names(columna))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_filas_ausentes", TRUE,
        paste0(
          "La marca conserva los registros y permite revisar los ausentes ",
          "antes de decidir si corresponde excluirlos."
        ), fila$n_faltantes[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        parametros = list(columna_marca = nombre_marca), orden = 40L,
        grupo = grupo_hallazgo, decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_filas_ausentes", FALSE,
        paste0(
          "Eliminar registros puede excluir personas o hechos del an\u00e1lisis; ",
          "s\u00f3lo corresponde cuando el dominio confirma que el ausente invalida la fila."
        ), fila$n_faltantes[[1L]], FALSE, estado = estado_columna,
        aplicar = FALSE, orden = 45L, grupo = grupo_hallazgo,
        decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes",
        destructiva = TRUE
      ))
    } else if (identical(tipo, "faltantes_disfrazados") && !is.null(fila)) {
      n_textuales <- fila$n_faltantes_disfrazados_textuales[[1L]]
      n_numericos <- fila$n_faltantes_disfrazados_numericos[[1L]]
      if (n_textuales > 0L) {
        justificacion <- paste0(
          "Las representaciones textuales del cat\u00e1logo son marcadores ",
          "expl\u00edcitos de ausencia y pueden normalizarse sin inferir el dominio."
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_ausencias_textuales", TRUE,
          justificacion, n_textuales, FALSE,
          estado = estado_columna,
          aplicar = identical(estado_columna, "lista"),
          parametros = list(valores = .cadenas_na()), orden = 100L
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
            ") y puede convertirse sin elegir entre interpretaciones."
          )
        } else if (seguro) {
          comprobacion_numero$justificacion
        } else {
          paste0(
            "La columna no aporta evidencia suficiente para distinguir el ",
            "separador decimal del separador de miles."
          )
        },
        fila$n_numeros_texto[[1L]], comprobacion_numero$reversible,
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
        destructiva = seguro && !conversion_numero_segura,
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
        estado = if (seguro && comprobacion_fecha$verificable) estado else {
          if (seguro) "bloqueada" else estado
        },
        aplicar = conversion_fecha_segura && identical(estado, "lista"),
        parametros = c(parametros_fecha, list(
          reversibilidad_comprobada = comprobacion_fecha$verificable,
          n_no_reversibles = comprobacion_fecha$n_no_reversibles,
          motivo_no_reversible = comprobacion_fecha$justificacion
        )),
        destructiva = seguro && !conversion_fecha_segura,
        orden = 300L
      ))
    } else if (identical(tipo, "tipo_declarado_distinto") && !is.null(fila) &&
               !.es_fecha_ambigua(perfil, columna) &&
               !.es_fecha_mixta(perfil, columna) &&
               !any(
                 perfil$hallazgos$columna == columna &
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
      estado <- if (base_tipo_seguro && comprobacion_tipo$verificable) {
        estado_columna
      } else if (base_tipo_seguro) {
        "bloqueada"
      } else {
        "bloqueada"
      }
      justificacion <- if (recomendar) {
        paste0(
          "Todos los valores presentes son compatibles con el tipo inferido y ",
          "la conversi\u00f3n conserva su representaci\u00f3n textual."
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
        )), orden = 310L,
        grupo = grupo_hallazgo,
        decision_grupo = if (recomendar) "recomendada" else "pendiente",
        recomendacion_grupo = if (recomendar) "convertir_tipo" else NA_character_,
        destructiva = base_tipo_seguro && !recomendar
      ))
    } else if (identical(tipo, "filas_duplicadas")) {
      n_participantes <- perfil$general$filas_en_grupos_duplicados
      if (is.null(n_participantes)) {
        n_participantes <- perfil$general$filas_duplicadas
      }
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "marcar_filas_duplicadas", TRUE,
        paste0(
          "Marcar conserva todas las filas, identifica las repeticiones y ",
          "asigna un grupo a todos los registros que participan."
        ), n_participantes, TRUE, estado = "lista", aplicar = TRUE,
        parametros = list(
          columna_marca = ".fila_duplicada",
          columna_grupo = ".grupo_duplicado"
        ), orden = 30L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "conservar_primera_duplicada", FALSE,
        paste0(
          "Conserva la primera aparici\u00f3n exacta y elimina las siguientes; ",
          "el orden de entrada pasa a determinar qu\u00e9 registro sobrevive."
        ), perfil$general$filas_duplicadas, FALSE, estado = "lista",
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
          columna_marca = paste0(".outlier_", make.names(columna)),
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
          nombres_propuestos = make.names(nombres, unique = TRUE)
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
          sum(perfil$columnas$columna == nombre) != 1L
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
    if (!identical(names(datos), perfil$columnas$columna)) {
      stop("Los nombres de `datos` no coinciden con los usados por el perfil.",
           call. = FALSE)
    }
    exactas <- dependencias[dependencias$exacta, , drop = FALSE]
    candidatas <- list()
    for (i in seq_len(nrow(exactas))) {
      determinante <- exactas$determinante[[i]]
      dependiente <- exactas$dependiente[[i]]
      if (!all(c(determinante, dependiente) %in% names(datos))) next
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
        vapply(candidatas, `[[`, character(1L), "dependiente")
      )
      numero_grupo <- nrow(hallazgos)
      for (indices in por_dependiente) {
        numero_grupo <- numero_grupo + 1L
        grupo <- .id_grupo(numero_grupo)
        for (indice in indices) {
          candidata <- candidatas[[indice]]
          estrategia_imputacion <- paste0(
            "imputar_dependencia_funcional__", make.names(candidata$determinante)
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
               hallazgos$columna == resultado$columna[[j]])
        )
        if (length(candidatos)) candidatos[[1L]] else NA_integer_
      }
      if (!is.na(indice_hallazgo) && indice_hallazgo <= nrow(hallazgos)) {
        resultado$evidencia[[j]] <- hallazgos$evidencia[[indice_hallazgo]]
        resultado$severidad_origen[[j]] <- as.character(
          hallazgos$severidad[[indice_hallazgo]]
        )
      }
    }
  }
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
  class(resultado) <- c("plan_limpieza", "data.frame")
  resultado
}

.columnas_plan <- function() {
  c(
    "id_accion", "columna", "hallazgo", "grupo", "decision_grupo",
    "recomendacion_grupo", "estrategia", "recomendada",
    "severidad_origen", "evidencia", "justificacion", "n_afectadas",
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
  if (anyDuplicated(plan$id_accion)) {
    stop("`plan$id_accion` debe contener identificadores \u00fanicos.", call. = FALSE)
  }
  if (!is.list(plan$parametros)) {
    stop("`plan$parametros` debe ser una columna de listas.", call. = FALSE)
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
  indices <- which(names(datos) == columna)
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
  normalizados <- tolower(trimws(as.character(x)))
  mascara <- !is.na(x) & normalizados %in% parametros$valores
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
  mapa <- parametros$mapa
  if (!all(c(determinante, dependiente) %in% names(datos)) ||
      !inherits(mapa, "data.frame") ||
      !all(c("determinante", "dependiente") %in% names(mapa))) {
    stop("La imputaci\u00f3n no conserva un contrato de dependencia v\u00e1lido.",
         call. = FALSE)
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

.recortar_texto <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("El recorte de espacios requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  nuevo <- trimws(anterior)
  mascara <- !is.na(anterior) & anterior != nuevo
  list(valor = nuevo, n = sum(mascara))
}

.reparar_codificacion <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La reparaci\u00f3n de codificaci\u00f3n requiere una columna de texto.",
         call. = FALSE)
  }
  iteraciones <- parametros$max_iteraciones
  if (is.null(iteraciones)) iteraciones <- 20L
  anterior <- as.character(x)
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

.convertir_numero_regional <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La conversi\u00f3n regional requiere una columna de texto.", call. = FALSE)
  }
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

.transformar_capitalizacion <- function(x, estrategia, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    stop("La capitalizaci\u00f3n requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  if (identical(estrategia, "convertir_minusculas")) {
    nuevo <- tolower(anterior)
  } else if (identical(estrategia, "convertir_mayusculas")) {
    nuevo <- toupper(anterior)
  } else if (identical(estrategia, "convertir_titulo")) {
    nuevo <- gsub(
      "\\b([[:alpha:]])", "\\U\\1", tolower(anterior), perl = TRUE
    )
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
  mascara <- !is.na(anterior) & anterior != nuevo
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
  texto <- sub(",", ".", trimws(as.character(x)), fixed = TRUE)
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
  list(valor = salida, n = sum(mascara))
}

.grupos_filas_duplicadas <- function(datos) {
  if (!ncol(datos)) {
    repetidas <- seq_len(nrow(datos)) > 1L
    grupos <- if (nrow(datos) > 1L) {
      rep.int(1L, nrow(datos))
    } else {
      rep.int(NA_integer_, nrow(datos))
    }
    return(list(repetidas = repetidas, grupos = grupos))
  }
  repetidas <- duplicated(datos)
  participantes <- repetidas | duplicated(datos, fromLast = TRUE)
  grupos <- rep(NA_integer_, nrow(datos))
  if (!any(participantes)) {
    return(list(repetidas = repetidas, grupos = grupos))
  }
  if (any(vapply(datos, is.list, logical(1L)))) {
    stop("No se pueden agrupar duplicados con columnas de lista.", call. = FALSE)
  }
  factores <- lapply(datos, function(x) factor(x, exclude = NULL))
  codigos <- as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
  grupos[participantes] <- match(
    codigos[participantes], unique(codigos[participantes])
  )
  list(repetidas = repetidas, grupos = grupos)
}

.filtrar_filas <- function(datos, conservar) {
  if (inherits(datos, "data.table")) {
    datos[which(conservar), ]
  } else {
    datos[conservar, , drop = FALSE]
  }
}

.conservar_mas_completa <- function(datos, clave) {
  if (!length(clave) || any(!clave %in% names(datos))) {
    stop(
      "`conservar_mas_completa` requiere configurar nombres de clave existentes.",
      call. = FALSE
    )
  }
  claves <- datos[clave]
  if (any(vapply(claves, is.list, logical(1L)))) {
    stop("La clave no puede contener columnas de lista.", call. = FALSE)
  }
  factores <- lapply(claves, function(x) factor(x, exclude = NULL))
  codigos <- as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
  grupos <- split(seq_len(nrow(datos)), codigos)
  completitud <- rowSums(!is.na(datos))
  conservar <- rep(TRUE, nrow(datos))
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
  if (!identical(names(datos), esperados)) {
    stop("Los nombres de los datos no coinciden con los usados por el perfil.", call. = FALSE)
  }
  invisible(TRUE)
}

.agregar_marca <- function(datos, nombre, valor) {
  if (nombre %in% names(datos)) {
    stop("La columna de marca ya existe: ", nombre, ".", call. = FALSE)
  }
  datos[[nombre]] <- valor
  datos
}

.ejecutar_accion <- function(datos, accion) {
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  columna <- accion$columna[[1L]]

  if (estrategia %in% c(
    "normalizar_nombres", "normalizar_nombres_snake_case"
  )) {
    anteriores <- names(datos)
    names(datos) <- if (identical(estrategia, "normalizar_nombres")) {
      make.names(anteriores, unique = TRUE)
    } else {
      .nombres_snake(anteriores)
    }
    return(list(datos = datos, n = sum(anteriores != names(datos))))
  }
  if (identical(estrategia, "marcar_filas_duplicadas")) {
    marcas <- .grupos_filas_duplicadas(datos)
    datos <- .agregar_marca(datos, parametros$columna_marca, marcas$repetidas)
    datos <- .agregar_marca(datos, parametros$columna_grupo, marcas$grupos)
    return(list(datos = datos, n = sum(!is.na(marcas$grupos))))
  }
  if (identical(estrategia, "conservar_primera_duplicada")) {
    eliminar <- duplicated(datos)
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
      attr(datos, "columnas_duplicadas_marcadas") <- if (is.null(marcas)) {
        nueva
      } else {
        unique(rbind(marcas, nueva))
      }
      return(list(datos = datos, n = 1))
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
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "convertir_sentinelas_numericos")) {
    cambio <- .reemplazar_sentinelas_numericos(x, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (startsWith(estrategia, "imputar_dependencia_funcional__")) {
    return(.imputar_dependencia(datos, parametros))
  }
  if (identical(estrategia, "recortar_espacios")) {
    cambio <- .recortar_texto(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
  }
  if (estrategia %in% c("reparar_codificacion_latin1", "reparar_codificacion")) {
    cambio <- .reparar_codificacion(x, parametros)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n,
                estado_reparacion = cambio$estado_reparacion,
                n_parciales = cambio$n_parciales))
  }
  if (identical(estrategia, "convertir_numero_regional")) {
    cambio <- .convertir_numero_regional(x, parametros)
    datos[[indice]] <- cambio$valor
    comparacion <- .comparar_representacion_conversion(x, cambio$valor)
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = comparacion$n_no_reversibles))
  }
  if (identical(estrategia, "marcar_filas_ausentes")) {
    marca <- is.na(x)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "eliminar_filas_ausentes")) {
    eliminar <- is.na(x)
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
    comparacion <- .comparar_representacion_conversion(x, convertido)
    return(list(datos = datos, n = sum(!is.na(x)),
                n_no_reversibles = comparacion$n_no_reversibles))
  }
  if (identical(estrategia, "convertir_tipo")) {
    convertido <- .convertir_tipo(x, parametros)
    datos[[indice]] <- convertido
    comparacion <- .comparar_representacion_conversion(x, convertido)
    return(list(datos = datos, n = sum(!is.na(x)),
                n_no_reversibles = comparacion$n_no_reversibles))
  }
  if (identical(estrategia, "marcar_outliers")) {
    marca <- .marca_outliers(x)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "winsorizar_outliers")) {
    cambio <- .winsorizar_outliers(x)
    datos[[indice]] <- cambio$valor
    return(list(datos = datos, n = cambio$n))
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

#' @rdname planificar_limpieza
#' @param permitir_eliminacion Segundo consentimiento obligatorio para ejecutar
#'   acciones que eliminan filas o columnas.
#' @param conservar_eliminados Si se conservan en el resultado las filas y
#'   columnas retiradas. Es `TRUE` de forma predeterminada.
#' @export
aplicar <- function(plan, datos, permitir_eliminacion = FALSE,
                    conservar_eliminados = TRUE) {
  .validar_plan_limpieza(plan)
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
    seleccion <- seleccion[order(plan$orden[seleccion], seleccion)]
  }
  destructivas <- seleccion[
    plan$destructiva[seleccion] &
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
    # Cada acción trabaja sobre una copia del estado anterior. Si falla, la
    # columna (o tabla) queda intacta y el resto del plan puede continuar.
    ejecutada <- tryCatch(
      .ejecutar_accion(.copiar_datos(salida), accion),
      error = function(e) list(
        error = conditionMessage(e), n = 0, n_no_reversibles = 0
      )
    )
    fallo <- !is.null(ejecutada$error)
    if (!fallo) salida <- ejecutada$datos
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

.texto_ejemplo <- function(x) {
  if (length(x) == 0L || is.na(x)) return("<NA>")
  encodeString(as.character(x), quote = '"')
}

.ejemplos_grupo <- function(acciones, datos, max_ejemplos = 5L) {
  tipo <- acciones$hallazgo[[1L]]
  columna <- acciones$columna[[1L]]
  parametros <- acciones$parametros[[1L]]
  valores <- NULL

  if (identical(tipo, "filas_duplicadas")) {
    grupos <- .grupos_filas_duplicadas(datos)$grupos
    indices <- utils::head(which(!is.na(grupos)), max_ejemplos)
    return(vapply(indices, function(i) {
      contenido <- vapply(datos, function(x) .texto_ejemplo(x[[i]]), character(1L))
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
  x <- datos[[indice]]
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
  if (!length(respuesta) || is.na(respuesta[[1L]]) ||
      identical(respuesta[[1L]], "no_hacer_nada") ||
      identical(respuesta[[1L]], 0L) || identical(respuesta[[1L]], 0)) {
    return(NA_integer_)
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
#'   el identificador o el nombre de una estrategia, o `0` para no hacer nada.
#' @param diccionarios Lista opcional con nombre de diccionarios por columna.
#' @param max_ejemplos Máximo de ejemplos reales mostrados por grupo.
#'
#' @return El plan editado, sin ejecutar acciones.
#' @export
#' @seealso [planificar_limpieza()], [aplicar()]
#'
#' @examples
#' datos <- data.frame(zona = c("Norte", "NORTE", "sur"))
#' plan <- planificar_limpieza(perfilar(datos))
#' guiado <- guiar_limpieza(plan, datos)
#' identical(plan, guiado) # TRUE en una sesión no interactiva
guiar_limpieza <- function(plan, datos, selector = NULL,
                           diccionarios = list(), max_ejemplos = 5L) {
  .validar_plan_limpieza(plan)
  plan <- .sincronizar_decisiones(plan)
  if (is.null(selector) && !interactive()) return(plan)
  if (!is.null(selector) && !is.function(selector)) {
    stop("`selector` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
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
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    acciones <- plan[indices, , drop = FALSE]
    nombre_columna <- acciones$columna[[1L]]
    diccionario <- if (!is.na(nombre_columna)) {
      diccionarios[[nombre_columna]]
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
    ejemplos <- .ejemplos_grupo(acciones, datos, max_ejemplos)
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

    cli::cli_h2(paste("Decisi\u00f3n", grupo))
    cli::cli_text(paste("Hallazgo:", acciones$hallazgo[[1L]]))
    cli::cli_text(paste("Objeto afectado:",
                        if (is.na(acciones$columna[[1L]])) "tabla" else acciones$columna[[1L]]))
    cantidades <- acciones$n_afectadas[is.finite(acciones$n_afectadas)]
    cantidad <- if (length(cantidades)) max(cantidades) else NA_real_
    cli::cli_text(paste("Cantidad estimada:", cantidad))
    if (length(ejemplos)) {
      cli::cli_text(paste("Ejemplos reales:", paste(ejemplos, collapse = "; ")))
    }
    for (k in elegibles) {
      marca <- if (acciones$recomendada[[k]]) " (Recomendado)" else ""
      cli::cli_text(paste0(
        k, ". ", acciones$estrategia[[k]], marca, " -- ",
        acciones$justificacion[[k]]
      ))
    }
    bloqueadas <- which(as.character(acciones$estado) == "bloqueada")
    for (k in bloqueadas) {
      cli::cli_text(paste0(
        "[bloqueada] ", acciones$estrategia[[k]], " -- ",
        acciones$justificacion[[k]]
      ))
    }
    explicacion_no_hacer <- if (no_hacer_recomendado) {
      paste0(
        "No hacer nada conserva los datos y es lo recomendado porque el ",
        "hallazgo no justifica por s\u00ed solo una eliminaci\u00f3n."
      )
    } else {
      "No hacer nada conserva los datos y registra la omisi\u00f3n."
    }
    cli::cli_text(paste0(etiqueta_no_hacer, " -- ", explicacion_no_hacer))

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
  cli::cli_h1("Plan de limpieza")
  cli::cli_alert_success(paste(sum(x$aplicar), "acciones activadas"))
  cli::cli_alert_info(paste(sum(!x$aplicar), "acciones desactivadas"))
  n_destructivas <- sum(x$aplicar & x$destructiva)
  n_eliminatorias <- sum(
    x$aplicar & x$destructiva &
      x$estrategia %in% .estrategias_eliminatorias()
  )
  if (n_destructivas) {
    cli::cli_alert_danger(paste(
      n_destructivas,
      "acciones destructivas activas; revise la p\\u00e9rdida declarada"
    ))
  }
  if (n_eliminatorias) {
    cli::cli_alert_danger(paste(
      n_eliminatorias,
      "acciones eliminatorias activas; requieren un segundo consentimiento"
    ))
  }
  vista <- x[c(
    "id_accion", "grupo", "columna", "estrategia", "decision_grupo",
    "estado", "recomendada", "destructiva", "aplicar"
  )]
  print.data.frame(vista, row.names = FALSE)
  invisible(x)
}

#' @export
print.resultado_limpieza <- function(x, ...) {
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
  cli::cli_alert_success(paste(ejecutadas, "acciones ejecutadas"))
  if (fallidas) {
    cli::cli_alert_danger(paste(fallidas, "acciones fallidas; revise `registro$error`"))
  }
  cli::cli_text(paste(sum(x$registro$n_cambiadas), "celdas o marcas afectadas"))
  n_filas <- sum(x$registro$n_filas_eliminadas)
  n_columnas <- sum(x$registro$n_columnas_eliminadas)
  if (n_filas || n_columnas) {
    cli::cli_alert_warning(paste(
      n_filas, "filas y", n_columnas, "columnas eliminadas"
    ))
  }
  invisible(x)
}
