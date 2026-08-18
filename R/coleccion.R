# Nivel 7 del marco: la coleccion, una base de datos entera.
#
# La granularidad estaba declarada y no se media, y el mensaje decia que faltaba
# implementarla. Lo que faltaba era el objeto: una base es un conjunto de tablas
# y nadie le decia a `lupa` cuales la componen.
#
# El diseno esta gobernado por lo que se midio en bases reales, y las cifras
# rompen cualquier enfoque exploratorio: una coleccion puede tener ~1.730 tablas
# en 64 esquemas, y otra ~1.291 tablas en 21 esquemas con miles de millones de
# filas. Explorar todos los pares serian 1,5 millones de comparaciones.
#
# De ahi salen las cinco decisiones que sostienen todo lo demas:
#
#   1. La frontera la DECLARA el usuario. `lupa` no recorre el catalogo para
#      descubrir tablas: eso convertiria un error de permisos en un resultado.
#   2. El esquema es parte de la identidad de la tabla, no un adorno.
#   3. Los permisos parciales son el caso normal, no el borde: una credencial
#      que lee 9 tablas de 897 objetos es lo habitual. Lo que no se pudo leer va
#      a `cobertura_coleccion` con su motivo, nunca a cero.
#   4. Cada tabla declara su propio muestreo. No se promedian alcances distintos
#      como si fueran uno.
#   5. No hay snapshot: perfilar una coleccion son muchas consultas y la base
#      puede cambiar entre ellas. Cada tabla declara el momento en que se midio,
#      y el objeto declara que no hubo lectura instantanea.

.normalizar_tablas_coleccion <- function(tablas) {
  if (inherits(tablas, "data.frame")) {
    if (!"tabla" %in% names(tablas) || !nrow(tablas)) {
      stop(
        "`tablas` como data.frame debe traer una columna `tabla` y al menos ",
        "una fila.", call. = FALSE
      )
    }
    esquema <- if ("esquema" %in% names(tablas)) {
      as.character(tablas$esquema)
    } else {
      rep(NA_character_, nrow(tablas))
    }
    tipo <- if ("tipo" %in% names(tablas)) {
      as.character(tablas$tipo)
    } else {
      rep("tabla", nrow(tablas))
    }
    return(data.frame(
      esquema = esquema, tabla = as.character(tablas$tabla), tipo = tipo,
      stringsAsFactors = FALSE
    ))
  }
  if (!is.character(tablas) || !length(tablas) || anyNA(tablas) ||
      !all(nzchar(tablas))) {
    stop(
      "`tablas` debe ser un vector de nombres o un data.frame con columnas ",
      "`esquema` y `tabla`.", call. = FALSE
    )
  }
  partes <- strsplit(tablas, ".", fixed = TRUE)
  data.frame(
    esquema = vapply(partes, function(x) {
      if (length(x) > 1L) x[[1L]] else NA_character_
    }, character(1L)),
    tabla = vapply(partes, function(x) x[[length(x)]], character(1L)),
    tipo = rep("tabla", length(tablas)),
    stringsAsFactors = FALSE
  )
}

.identificador_tabla <- function(fila) {
  if (is.na(fila$esquema)) fila$tabla else paste0(fila$esquema, ".", fila$tabla)
}

#' Declarar la frontera de una colección
#'
#' Una colección es una base de datos entera: el séptimo nivel de granularidad
#' del marco. `lupa` no la descubre recorriendo el catálogo — **la frontera la
#' declara quien conoce la base**, igual que los pesos de [indice_calidad()] o
#' el marco de [cobertura_analisis()]. Explorar el catálogo convertiría un error
#' de permisos en un resultado, y en bases reales de más de mil tablas
#' recorrerlas todas no es viable.
#'
#' Esta función no consulta nada: sólo declara. Lo que se mide viene después,
#' con [perfilar_coleccion()].
#'
#' El **esquema es parte de la identidad de la tabla**. Se puede declarar como
#' `"esquema.tabla"` o con un data frame de columnas `esquema` y `tabla`. Una
#' tercera columna `tipo` permite declarar qué es cada objeto —`"tabla"`,
#' `"vista"`, `"temporal"`—, porque el conteo bruto de un catálogo mezcla tablas
#' base con vistas, índices y secuencias, y no todas se perfilan igual.
#'
#' @param conexion Conexión abierta compatible con DBI.
#' @param tablas Nombres de las tablas que componen la colección, como vector
#'   `"esquema.tabla"` o como data frame con columnas `esquema`, `tabla` y
#'   opcionalmente `tipo`.
#' @param nombre Etiqueta de la colección.
#'
#' @return Objeto de clase `coleccion_lupa`.
#' @export
#' @seealso [perfilar_coleccion()], [perfilar_dbi()], [granularidades()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   coleccion(con, "personas", nombre = "padron")
#'   DBI::dbDisconnect(con)
#' }
coleccion <- function(conexion, tablas, nombre = NULL) {
  .requerir_dbi()
  if (!DBI::dbIsValid(conexion)) {
    stop("`conexion` debe ser una conexion DBI abierta y valida.", call. = FALSE)
  }
  declaradas <- .normalizar_tablas_coleccion(tablas)
  if (anyDuplicated(declaradas[, c("esquema", "tabla")])) {
    stop("`tablas` repite una tabla.", call. = FALSE)
  }
  if (!is.null(nombre) && !.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser NULL o una cadena no vacia.", call. = FALSE)
  }
  declaradas$identificador <- vapply(
    seq_len(nrow(declaradas)),
    function(i) .identificador_tabla(declaradas[i, , drop = FALSE]),
    character(1L)
  )
  estructura <- list(
    nombre = if (is.null(nombre)) "coleccion" else nombre,
    conexion = conexion,
    tablas = declaradas,
    motor = tryCatch(
      as.character(class(conexion)[[1L]]), error = function(e) NA_character_
    ),
    n_declaradas = nrow(declaradas)
  )
  class(estructura) <- "coleccion_lupa"
  estructura
}

#' @export
print.coleccion_lupa <- function(x, ...) {
  cli::cli_text("Colecci\u00f3n declarada: {.strong {x$nombre}}")
  cli::cli_text("Motor: {x$motor}")
  cli::cli_text("Tablas declaradas: {x$n_declaradas}")
  esquemas <- unique(x$tablas$esquema[!is.na(x$tablas$esquema)])
  if (length(esquemas)) {
    cli::cli_text("Esquemas: {paste(esquemas, collapse = ', ')}")
  }
  cli::cli_text(
    "La frontera es declarada: `lupa` no recorre el cat\u00e1logo."
  )
  invisible(x)
}

.cobertura_coleccion_vacia <- function() {
  data.frame(
    tabla = character(), esquema = character(), tipo = character(),
    motivo = character(), como_resolverlo = character(),
    stringsAsFactors = FALSE
  )
}

#' Perfilar una colección declarada
#'
#' Recorre las tablas declaradas en [coleccion()] y devuelve **una fila por
#' tabla** con su resumen exacto, más la cobertura de lo que no se pudo medir.
#'
#' Conserva el resumen calculado en SQL sobre toda la tabla, que es acotado, y
#' **no** retiene el perfil de la muestra de cada tabla, que es el objeto
#' pesado: con cientos de tablas eso no entra en memoria. `conservar_perfiles`
#' permite retenerlos cuando la colección es chica y se los necesita.
#'
#' **Lo que no se pudo medir se declara.** Una tabla que la credencial no puede
#' leer, un objeto que no es una tabla base, un motor que rechaza un agregado:
#' cada caso va a `cobertura_coleccion` con su motivo y su `como_resolverlo`,
#' y **nunca a cero**. En bases institucionales los permisos parciales son el caso
#' normal, no el borde.
#'
#' **No hay lectura instantánea.** Perfilar una colección son muchas consultas y
#' la base puede cambiar entre ellas: una tabla puede truncarse después de que se
#' contaron sus filas. Por eso cada fila declara el `momento` en que se midió y
#' `meta$snapshot` declara que no lo hubo.
#'
#' @param coleccion Objeto creado por [coleccion()].
#' @param muestra Filas solicitadas por tabla para el bloque en memoria.
#' @param conservar_perfiles Si se retienen los objetos `perfil_dbi` completos.
#'   Por omisión `FALSE`.
#' @param ... Argumentos enviados a [perfilar_dbi()].
#'
#' @return Objeto de clase `perfil_coleccion` con `resumen_coleccion`,
#'   `cobertura_coleccion`, `meta` y, si se pidió, `perfiles`.
#' @export
#' @seealso [coleccion()], [perfilar_dbi()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
#'   perfilar_coleccion(coleccion(con, "personas", nombre = "padron"))
#'   DBI::dbDisconnect(con)
#' }
perfilar_coleccion <- function(coleccion, muestra = 1000L,
                               conservar_perfiles = FALSE, ...) {
  if (!inherits(coleccion, "coleccion_lupa")) {
    stop(
      "`coleccion` debe ser una coleccion declarada por coleccion().",
      call. = FALSE
    )
  }
  if (!is.logical(conservar_perfiles) || length(conservar_perfiles) != 1L ||
      is.na(conservar_perfiles)) {
    stop("`conservar_perfiles` debe ser TRUE o FALSE.", call. = FALSE)
  }
  conexion <- coleccion$conexion
  if (!DBI::dbIsValid(conexion)) {
    stop("La conexion de la coleccion ya no es valida.", call. = FALSE)
  }
  declaradas <- coleccion$tablas
  inicio <- Sys.time()
  cobertura <- list()
  resumenes <- list()
  perfiles <- list()

  for (i in seq_len(nrow(declaradas))) {
    fila <- declaradas[i, , drop = FALSE]
    identificador <- fila$identificador
    if (!identical(fila$tipo, "tabla")) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        tabla = fila$tabla, esquema = fila$esquema, tipo = fila$tipo,
        motivo = paste0(
          "El objeto se declaro como '", fila$tipo,
          "' y no como tabla base; no se perfilo."
        ),
        como_resolverlo = paste(
          "Declarar el objeto con tipo 'tabla' si corresponde perfilarlo, o",
          "perfilar la tabla base que lo respalda."
        ),
        stringsAsFactors = FALSE
      )
      next
    }
    momento <- Sys.time()
    perfil <- tryCatch(
      perfilar_dbi(conexion, identificador, muestra = muestra, ...),
      error = function(e) e
    )
    if (inherits(perfil, "condition")) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        tabla = fila$tabla, esquema = fila$esquema, tipo = fila$tipo,
        motivo = paste0(
          "No se pudo perfilar la tabla: ", conditionMessage(perfil)
        ),
        como_resolverlo = paste(
          "Comprobar que la tabla existe y que la credencial la puede leer;",
          "los permisos parciales son frecuentes y no se suplen adivinando."
        ),
        stringsAsFactors = FALSE
      )
      next
    }
    resumen <- perfil$resumen_tabla
    filas_tabla <- resumen$meta$filas
    analizadas <- perfil$perfil_muestra$meta$filas_analizadas
    prop_faltantes <- resumen$columnas$prop_faltantes
    resumenes[[length(resumenes) + 1L]] <- data.frame(
      tabla = fila$tabla,
      esquema = fila$esquema,
      identificador = identificador,
      n_columnas = as.numeric(nrow(resumen$columnas)),
      n_filas = as.numeric(
        if (length(filas_tabla)) filas_tabla[[1L]] else NA_real_
      ),
      # Agregados exactos sobre toda la tabla, calculados en SQL: se conservan
      # porque son acotados. El perfil de la muestra, que es el objeto pesado,
      # no se retiene salvo que se pida.
      prop_faltantes_maxima = if (length(prop_faltantes)) {
        suppressWarnings(max(prop_faltantes, na.rm = TRUE))
      } else NA_real_,
      n_columnas_sin_faltantes = if (length(prop_faltantes)) {
        sum(prop_faltantes %in% 0)
      } else NA_real_,
      muestra_solicitada = as.numeric(muestra),
      muestra_analizada = as.numeric(
        if (length(analizadas)) analizadas[[1L]] else NA_real_
      ),
      momento = momento,
      stringsAsFactors = FALSE
    )
    if (conservar_perfiles) perfiles[[identificador]] <- perfil
  }

  resumen_coleccion <- if (length(resumenes)) {
    do.call(rbind, resumenes)
  } else {
    data.frame(
      tabla = character(), esquema = character(), identificador = character(),
      n_columnas = numeric(), n_filas = numeric(),
      prop_faltantes_maxima = numeric(), n_columnas_sin_faltantes = numeric(),
      muestra_solicitada = numeric(), muestra_analizada = numeric(),
      momento = as.POSIXct(character()), stringsAsFactors = FALSE
    )
  }
  cobertura_coleccion <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    .cobertura_coleccion_vacia()
  }
  rownames(resumen_coleccion) <- NULL
  rownames(cobertura_coleccion) <- NULL

  estructura <- list(
    resumen_coleccion = resumen_coleccion,
    cobertura_coleccion = cobertura_coleccion,
    meta = list(
      nombre = coleccion$nombre,
      motor = coleccion$motor,
      n_declaradas = nrow(declaradas),
      n_perfiladas = nrow(resumen_coleccion),
      n_sin_perfilar = nrow(cobertura_coleccion),
      inicio = inicio,
      fin = Sys.time(),
      snapshot = FALSE,
      nota_snapshot = paste(
        "No hubo lectura instantanea: cada tabla se midio en su propio",
        "momento y la base pudo cambiar entre consultas. La columna `momento`",
        "de `resumen_coleccion` declara cuando se midio cada una."
      ),
      frontera = "declarada por el usuario"
    )
  )
  if (conservar_perfiles) estructura$perfiles <- perfiles
  class(estructura) <- "perfil_coleccion"
  estructura
}

#' @export
print.perfil_coleccion <- function(x, ...) {
  cli::cli_text("Perfil de colecci\u00f3n: {.strong {x$meta$nombre}}")
  cli::cli_text(
    "Tablas: {x$meta$n_perfiladas} perfiladas de {x$meta$n_declaradas} declaradas"
  )
  if (x$meta$n_sin_perfilar) {
    cli::cli_text(
      "Sin perfilar: {x$meta$n_sin_perfilar} (ver `cobertura_coleccion`)"
    )
  }
  cli::cli_text("Sin lectura instant\u00e1nea: cada tabla trae su `momento`.")
  invisible(x)
}
