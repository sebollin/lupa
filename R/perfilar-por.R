#' Perfilar una tabla por grupos de filas
#'
#' Aplica [perfilar()] a cada grupo de filas por separado y devuelve los
#' hallazgos de todos los grupos en una sola tabla. Es la respuesta al formato
#' largo: una tabla donde cada fila describe un atributo distinto no es una
#' tabla, son muchas apiladas, y perfilarla como si fuera una sola mezcla
#' dominios que no tienen nada que ver entre sí.
#'
#' Dentro de cada grupo se descartan las columnas enteramente ausentes antes de
#' perfilar. En un modelo entidad-atributo-valor bien formado eso deja viva
#' exactamente la columna de valor que corresponde al atributo del grupo, y es
#' lo que evita informar como falta lo que es la forma del dato. El descarte se
#' declara en la cobertura.
#'
#' La función no adivina cuál es la columna de agrupación: la declara quien
#' conoce el dato, igual que [perfilar()] no adivina claves ni jerarquías.
#'
#' @param datos Data frame a perfilar.
#' @param por Nombre de una columna atómica cuyos valores definen los grupos.
#'   Los ausentes forman un grupo propio.
#' @param clave Nombres de columnas de identidad que se conservan en cada grupo
#'   aunque estén enteramente ausentes. Importa: sin la clave de entidad, el
#'   diagnóstico de filas duplicadas informa como duplicada cada repetición del
#'   valor del atributo.
#' @param min_filas Grupos con menos filas que este número no se perfilan y se
#'   declaran en la cobertura. El valor por omisión evita conclusiones sobre
#'   grupos donde ningún diagnóstico tiene soporte.
#' @param ... Argumentos enviados a [perfilar()] para cada grupo.
#'
#' @return Data frame de clase `hallazgos_por_grupo` con las columnas de
#'   `hallazgos` de [perfilar()] precedidas por `grupo` y `n_filas_grupo`. El
#'   atributo `cobertura_grupos` declara los grupos no perfilados y las columnas
#'   descartadas por grupo.
#'
#'   El atributo `etiquetas_personales` declara si la columna de agrupación
#'   lleva datos personales. Las etiquetas de grupo **son** valores de esa
#'   columna, así que la salida los publica —en los hallazgos y en las dos
#'   tablas de cobertura— aunque [perfilar()] enmascare esa misma columna. No se
#'   enmascaran aquí porque la etiqueta es el eje del resultado y sin ella los
#'   grupos no se distinguen; pero tampoco ocurre en silencio: se avisa al
#'   ejecutar y queda declarado en el objeto. Para que no se publiquen, agrupe
#'   por una columna seudonimizada. El atributo queda vacío cuando la columna no
#'   lleva datos personales, y también cuando `proteger_datos_personales` es
#'   `FALSE`, porque entonces ya está declarado que se quieren los valores.
#'
#'   El atributo `cobertura_diagnosticos` declara, **por grupo**, los
#'   diagnósticos que no se evaluaron y por qué. Cada grupo se perfila por
#'   separado, así que cada uno declina los suyos: una columna puede tener
#'   bastantes filas en un grupo y muy pocas en otro. Sin esa tabla, un grupo sin
#'   hallazgos se lee como un grupo sano, cuando puede ser un grupo sobre el que
#'   no se miró.
#' @export
#' @seealso [perfilar()], [cobertura_analisis()]
#'
#' @examples
#' largo <- data.frame(
#'   entidad = rep(1:40, each = 2),
#'   atributo = rep(c("pais", "edad"), 40),
#'   valor = c(rbind(sample(c("UY", "AR"), 40, TRUE), as.character(20:59)))
#' )
#' hallazgos <- perfilar_por(largo, "atributo", clave = "entidad", min_filas = 10)
#' head(hallazgos[, c("grupo", "columna", "tipo_hallazgo")])
perfilar_por <- function(datos, por, clave = NULL, min_filas = 30L, ...) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data frame.", call. = FALSE)
  }
  datos <- .tabla_base(datos)
  if (!is.character(por) || length(por) != 1L || is.na(por)) {
    stop("`por` debe ser el nombre de una sola columna.", call. = FALSE)
  }
  if (!por %in% names(datos)) {
    stop("`por` nombra una columna inexistente: ", por, ".", call. = FALSE)
  }
  if (!is.atomic(datos[[por]])) {
    stop("`por` debe nombrar una columna atomica.", call. = FALSE)
  }
  if (!is.null(clave)) {
    if (!is.character(clave)) {
      stop("`clave` debe ser un vector de nombres de columnas.", call. = FALSE)
    }
    desconocidas <- setdiff(clave, names(datos))
    if (length(desconocidas)) {
      stop("`clave` nombra columnas inexistentes: ",
           paste(desconocidas, collapse = ", "), ".", call. = FALSE)
    }
  }
  min_filas <- as.integer(min_filas)
  if (length(min_filas) != 1L || is.na(min_filas) || min_filas < 1L) {
    stop("`min_filas` debe ser un entero positivo.", call. = FALSE)
  }

  etiquetas <- as.character(datos[[por]])
  etiquetas[is.na(datos[[por]])] <- "(ausente)"
  grupos <- split(seq_len(nrow(datos)), factor(etiquetas, levels = unique(etiquetas)))

  # La etiqueta de cada grupo ES un valor de la columna `por`. Si esa columna
  # lleva datos personales, las etiquetas los publican -en los hallazgos y en
  # las dos tablas de cobertura- aunque `perfilar()` sobre la misma columna
  # enmascare su moda. La columna se recorta de cada rebanada antes de perfilar,
  # asi que la capa de proteccion nunca la ve y no puede taparlo.
  #
  # No se enmascara: agrupar por una columna es pedir explicitamente que la
  # salida se organice por sus valores, y una etiqueta ilegible dejaria el
  # resultado sin eje. Pero tampoco se hace en silencio, que es lo que pasaba.
  # Se avisa y se declara en el objeto, para que quien publique la salida sepa
  # que lleva.
  #
  # La clasificacion la decide `perfilar()` sobre esa sola columna, no una copia
  # de la regla aca: una regla duplicada se arregla en un lado y no en el otro,
  # que es como aparecio el defecto de `analizar_tiempo()`.
  extras <- list(...)
  proteger <- if ("proteger_datos_personales" %in% names(extras)) {
    isTRUE(extras$proteger_datos_personales)
  } else {
    isTRUE(eval(formals(perfilar)$proteger_datos_personales))
  }
  etiquetas_personales <- NULL
  if (proteger) {
    sonda <- tryCatch(
      # `.seleccionar_columnas()` y no `datos[, por, drop = FALSE]`: con
      # `data.table` la semantica de `[` depende de si quien llama tiene el
      # paquete entre sus imports, y el proyecto concentra las selecciones en
      # esa primitiva por eso. El test de la ronda 160 cuenta los sitios
      # sueltos y me caso este mismo.
      perfilar(.seleccionar_columnas(datos, por),
               proteger_datos_personales = TRUE,
               analizar_dependencias = FALSE),
      error = function(e) NULL
    )
    clasificada <- if (!is.null(sonda) && nrow(sonda$datos_personales)) {
      sonda$datos_personales[sonda$datos_personales$columna == por &
                               isTRUE(sonda$datos_personales$proteger), ,
                             drop = FALSE]
    } else NULL
    if (!is.null(clasificada) && nrow(clasificada)) {
      etiquetas_personales <- data.frame(
        columna = por,
        tipo = as.character(clasificada$tipo[1L]),
        n_grupos = length(grupos),
        motivo = paste0(
          "Las etiquetas de grupo son valores de `", por, "`, clasificada como ",
          clasificada$tipo[1L], ". `perfilar()` enmascara esa columna; aca no, ",
          "porque la etiqueta es el eje del resultado. La salida lleva esos ",
          "valores: agrupe por una columna seudonimizada si no deben publicarse."
        ),
        stringsAsFactors = FALSE
      )
      cli::cli_alert_warning(etiquetas_personales$motivo)
    }
  }

  hallazgos <- list()
  cobertura <- list()
  cobertura_diagnosticos <- list()

  for (nombre_grupo in names(grupos)) {
    filas <- grupos[[nombre_grupo]]
    if (length(filas) < min_filas) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = paste0(
          "El grupo tiene ", length(filas), " filas y `min_filas` es ",
          min_filas, ": no se perfilo."
        ),
        columnas_descartadas = NA_character_, stringsAsFactors = FALSE
      )
      next
    }
    rebanada <- .seleccionar_columnas(
      datos, setdiff(names(datos), por), filas = filas
    )
    # Las columnas enteramente ausentes dentro del grupo se descartan: son las
    # que no corresponden a este atributo, y contarlas como falta era el defecto.
    # La clave declarada nunca se descarta, porque de ella dependen los
    # diagnosticos de unicidad.
    vacias <- vapply(rebanada, function(x) all(is.na(x)), logical(1L))
    descartables <- setdiff(names(rebanada)[vacias], clave)
    if (length(descartables)) {
      rebanada <- .seleccionar_columnas(
        rebanada, setdiff(names(rebanada), descartables)
      )
    }
    if (!ncol(rebanada)) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = "Todas las columnas del grupo estan enteramente ausentes.",
        columnas_descartadas = paste(descartables, collapse = ", "),
        stringsAsFactors = FALSE
      )
      next
    }
    perfil <- perfilar(rebanada, ...)
    # Cada grupo se perfila por separado, asi que cada uno declina sus propios
    # diagnosticos: una columna puede tener bastantes filas en un grupo y muy
    # pocas en otro. Sin juntar esas declaraciones, quien mira los hallazgos por
    # grupo no tiene forma de saber que sobre tal grupo no se miro, y leeria un
    # grupo sin hallazgos como un grupo sano.
    cb_grupo <- perfil$cobertura_diagnosticos
    if (inherits(cb_grupo, "data.frame") && nrow(cb_grupo)) {
      cobertura_diagnosticos[[length(cobertura_diagnosticos) + 1L]] <- cbind(
        data.frame(
          grupo = nombre_grupo, n_filas_grupo = length(filas),
          stringsAsFactors = FALSE
        ),
        cb_grupo
      )
    }
    if (nrow(perfil$hallazgos)) {
      fila <- cbind(
        data.frame(
          grupo = nombre_grupo, n_filas_grupo = length(filas),
          stringsAsFactors = FALSE
        ),
        perfil$hallazgos
      )
      hallazgos[[length(hallazgos) + 1L]] <- fila
    }
    if (length(descartables)) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = paste0(
          "Se descartaron ", length(descartables),
          " columnas enteramente ausentes en este grupo antes de perfilar."
        ),
        columnas_descartadas = paste(descartables, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }

  salida <- if (length(hallazgos)) {
    do.call(rbind, hallazgos)
  } else {
    data.frame(
      grupo = character(), n_filas_grupo = integer(), stringsAsFactors = FALSE
    )
  }
  rownames(salida) <- NULL
  attr(salida, "cobertura_grupos") <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    data.frame(
      grupo = character(), n_filas_grupo = integer(), motivo = character(),
      columnas_descartadas = character(), stringsAsFactors = FALSE
    )
  }
  attr(salida, "cobertura_diagnosticos") <- if (length(cobertura_diagnosticos)) {
    do.call(rbind, cobertura_diagnosticos)
  } else {
    cbind(
      data.frame(
        grupo = character(), n_filas_grupo = integer(),
        stringsAsFactors = FALSE
      ),
      .cobertura_diagnosticos_vacia()
    )
  }
  attr(salida, "n_grupos") <- length(grupos)
  attr(salida, "columna_grupo") <- por
  # Vacia cuando la columna de agrupacion no lleva datos personales, o cuando la
  # proteccion se desactivo: en ese caso el usuario ya declaro que los quiere.
  attr(salida, "etiquetas_personales") <- if (is.null(etiquetas_personales)) {
    data.frame(columna = character(), tipo = character(),
               n_grupos = integer(), motivo = character(),
               stringsAsFactors = FALSE)
  } else etiquetas_personales
  class(salida) <- c("hallazgos_por_grupo", "data.frame")
  salida
}
