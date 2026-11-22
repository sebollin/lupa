# Tablas de codigos de una coleccion y lo que sus etiquetas dicen de la ausencia.
#
# Una tabla de codigos es chica, tiene una columna que identifica cada fila -el
# codigo- y la usan columnas de otras tablas. Su etiqueta dice que significa cada
# codigo, y algunas dicen "sin dato", "no sabe", "no corresponde". El detector
# lee esas etiquetas y PROPONE declarar esos codigos como ausencia en cada columna
# que los usa, con su evidencia: la etiqueta y cuantas filas lo usan. No aplica
# nada y no hay puntaje: la confirmacion es del usuario (`incluir`), y
# `ausencias_desde_propuesta()` la convierte en la declaracion por columna de
# `perfilar()`.

# El lexico es otro que `.cadenas_na()`: aquella contesta si una CELDA es una
# marca de ausencia; este, si una ETIQUETA describe un codigo de ausencia.
# `None` es una marca y es una categoria valida ("ninguna discapacidad"); por eso
# `none`, `ninguno`, `otro` y `no` no estan.
.LEXICO_ETIQUETAS_AUSENCIA <- list(
  sin_dato = c(
    "sin dato", "sin datos", "sin informacion", "s/d", "s/i", "sd", "n/d",
    "no disponible", "desconocido", "desconocida", "ignorado", "ignorada",
    "no consta", "no registrado", "no registrada", "no especificado",
    "no especificada", "sin especificar", "no informado", "no informada",
    "no declarado", "no declarada", "missing", "unknown", "not stated",
    "not recorded", "not available", "sem informacao", "desconhecido"
  ),
  no_responde = c(
    "no sabe", "no responde", "no contesta", "ns/nc", "nsnc", "ns", "nc",
    "no sabe no contesta", "no sabe no responde", "refused", "don't know",
    "dont know", "no answer", "prefiere no responder", "nao sabe",
    "nao respondeu"
  ),
  no_corresponde = c(
    "no corresponde", "no aplica", "n/c", "not applicable", "nao se aplica",
    "nao aplicavel"
  ),
  ambigua = c("n/a", "na")
)

.lexico_etiquetas_ausencia <- function(etiquetas_ausencia = NULL) {
  perfil <- .normalizacion_para_columna(.resolver_normalizacion(TRUE), "etiqueta")
  normalizar <- function(x) .normalizacion_aplicar(x, perfil)
  lexico <- .LEXICO_ETIQUETAS_AUSENCIA
  if (length(etiquetas_ausencia)) lexico$declarada <- etiquetas_ausencia
  valores <- unlist(lapply(lexico, function(v) unique(normalizar(v))), use.names = FALSE)
  clases <- rep(names(lexico), vapply(lexico, function(v) {
    length(unique(normalizar(v)))
  }, integer(1L)))
  # Lo declarado manda sobre lo incorporado si repite una entrada.
  ultima <- !duplicated(valores, fromLast = TRUE)
  list(valores = valores[ultima], clases = clases[ultima], normalizar = normalizar)
}

# La clase de una etiqueta: `exacta` si la etiqueta entera esta en el lexico
# -sin un prefijo de codigo como `99 - ` y sin la puntuacion de los bordes-,
# `compuesta` si todas sus partes lo estan (`No sabe / No responde`), `parcial`
# si alguna (`Otro / sin dato`). `NA` si ninguna.
.clasificar_etiqueta_ausencia <- function(etiquetas, lexico) {
  texto <- .texto_analizable(as.character(etiquetas))$valores
  n <- length(texto)
  semantica <- rep(NA_character_, n)
  coincidencia <- rep(NA_character_, n)
  normalizadas <- lexico$normalizar(texto)
  for (i in seq_len(n)) {
    e <- normalizadas[[i]]
    if (is.na(e)) next
    e <- sub("^[0-9]+ *[-.:)] *", "", e, perl = TRUE)
    e <- gsub("^[[:punct:] ]+|[[:punct:] ]+$", "", e, perl = TRUE)
    if (!nzchar(e)) next
    k <- match(e, lexico$valores)
    if (!is.na(k)) {
      semantica[[i]] <- lexico$clases[[k]]
      coincidencia[[i]] <- "exacta"
      next
    }
    partes <- trimws(strsplit(e, " */ *| *, *| *; *| o | y | or |\\(|\\)", perl = TRUE)[[1L]])
    partes <- partes[nzchar(partes)]
    en <- match(partes, lexico$valores)
    if (!length(partes) || all(is.na(en))) next
    clases <- unique(lexico$clases[en[!is.na(en)]])
    semantica[[i]] <- paste(clases, collapse = "+")
    coincidencia[[i]] <- if (length(partes) > 1L && !anyNA(en)) "compuesta" else "parcial"
  }
  list(semantica = semantica, coincidencia = coincidencia)
}

.validar_argumentos_tablas_codigos <- function(coleccion, perfil, relaciones,
                                               max_filas, etiquetas_ausencia,
                                               umbral_cobertura = 0.9) {
  if (!is.numeric(umbral_cobertura) || length(umbral_cobertura) != 1L ||
      is.na(umbral_cobertura) || umbral_cobertura < 0 || umbral_cobertura > 1) {
    .detener("`umbral_cobertura` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!inherits(coleccion, "coleccion_lupa")) {
    .detener("`coleccion` debe venir de coleccion().", call. = FALSE)
  }
  if (!inherits(perfil, "perfil_coleccion")) {
    .detener(
      "`perfil` debe venir de perfilar_coleccion() sobre la misma colecci\u00f3n.",
      call. = FALSE
    )
  }
  if (!is.null(relaciones) && !inherits(relaciones, "relaciones_coleccion")) {
    .detener("`relaciones` debe ser NULL o venir de relaciones_coleccion().",
             call. = FALSE)
  }
  if (!is.numeric(max_filas) || length(max_filas) != 1L || is.na(max_filas) ||
      max_filas < 1 || max_filas != floor(max_filas) || !is.finite(max_filas)) {
    .detener("`max_filas` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.null(etiquetas_ausencia) &&
      (!is.character(etiquetas_ausencia) || anyNA(etiquetas_ausencia) ||
       any(!nzchar(trimws(.texto_analizable(etiquetas_ausencia)$valores))))) {
    .detener(
      "`etiquetas_ausencia` debe ser un vector de texto sin ausentes ni cadenas ",
      "vac\u00edas.", call. = FALSE
    )
  }
  invisible(NULL)
}

.tablas_codigos_vacias <- function() {
  data.frame(
    tabla = character(), columna_codigo = character(), n_filas = numeric(),
    n_columnas = integer(), columnas_texto = character(),
    n_usos = integer(), cobertura_maxima = numeric(), estado = character(),
    motivo = character(), stringsAsFactors = FALSE
  )
}

.usos_codigos_vacios <- function() {
  data.frame(
    tabla_codigos = character(), columna_codigo = character(),
    tabla = character(), columna = character(), origen = character(),
    validada = logical(), cobertura = numeric(), muestreado = logical(),
    stringsAsFactors = FALSE
  )
}

.propuesta_ausencias_vacia <- function() {
  data.frame(
    id_propuesta = character(), tabla_codigos = character(),
    columna_codigo = character(), codigo = character(),
    columna_etiqueta = character(), etiqueta = character(),
    semantica = character(), coincidencia = character(), tabla = character(),
    columna = character(), argumento = character(), n_filas = numeric(),
    motivo = character(), incluir = logical(), stringsAsFactors = FALSE
  )
}

# Las claves foraneas declaradas de una tabla: del perfil conservado si lo hay,
# y si no, del catalogo del motor (una o dos consultas de metadatos).
.claves_foraneas_tabla_codigos <- function(conexion, fila, perfil, consultas) {
  conservado <- if (is.list(perfil$perfiles)) perfil$perfiles[[fila$identificador]]
  fks <- conservado$resumen_tabla$meta$catalogo$claves_foraneas
  if (is.data.frame(fks)) return(fks)
  consultas$n <- consultas$n + 1L
  leido <- tryCatch(
    .catalogo_garantias_dbi(conexion, fila$tabla, fila$esquema),
    error = function(e) NULL
  )
  fks <- leido$catalogo$claves_foraneas
  if (is.data.frame(fks)) fks else .claves_foraneas_vacias_dbi()
}

#' Detectar tablas de códigos y proponer qué códigos quieren decir «sin dato»
#'
#' Busca en una colección las tablas de códigos —chicas, con una columna que
#' identifica cada fila y usadas por columnas de otras tablas— y lee sus
#' etiquetas. Cuando una etiqueta dice que el código significa «sin dato»,
#' «no sabe» o «no corresponde», **propone** declarar ese código como ausencia
#' en cada columna que lo usa, con su evidencia: la etiqueta y cuántas filas de
#' esa columna lo usan. No aplica nada ni calcula un puntaje: cada propuesta
#' nace con `incluir = FALSE`, el usuario marca las que confirma y
#' [ausencias_desde_propuesta()] las convierte en la declaración por columna
#' que reciben [perfilar()] y [perfilar_coleccion()].
#'
#' **Qué es una tabla de códigos.** Una tabla con a lo sumo `max_filas` filas
#' (según `perfil$resumen_coleccion`), leída entera, con alguna columna
#' completa y única (el código). Su **uso** sale de las claves foráneas que el
#' catálogo declara (de los perfiles conservados o, si no se conservaron, de
#' una lectura del catálogo por tabla) y de las relaciones que mida
#' [relaciones_coleccion()], si se pasan en `relaciones`. Es `candidata` sólo
#' si alguna columna la usa con cobertura de al menos `umbral_cobertura`: la
#' proporción de las filas con valor de esa columna que son un código de la
#' tabla. La de una clave foránea declarada se mide en el motor
#' (`CorrectitudSemFuerte` contra la tabla de códigos como [referencial()] del
#' motor, sin normalizar); la de una relación medida es la de
#' [relaciones_coleccion()]. La forma sola, una clave foránea con huérfanos o
#' una cobertura que no se pudo medir dejan la tabla `posible`, con el motivo
#' y, si nada la usa, en `meta$pares_sugeridos` los pares para medir sus
#' relaciones; una chica sin columna completa y única queda `sin_clave`. La
#' cobertura no distingue una tabla de códigos de una tabla chica de negocio
#' que otra referencia sin huérfanos (`clientes` y `pedidos`): las dos quedan
#' `candidata`, y sus etiquetas dicen cuál propone algo. Cuando una tabla tiene dos
#' columnas únicas —un `id` y un `codigo`—, el código es la que el uso apunta.
#'
#' **Qué etiqueta cuenta.** Toda columna de texto de la tabla de códigos que no
#' es el código se lee como etiqueta. El léxico incorporado (castellano, inglés
#' y portugués) reconoce `sin dato`, `sin información`, `desconocido`,
#' `ignorado`, `no sabe`, `no responde`, `no corresponde`, `no aplica`,
#' `unknown`, `not applicable` y sus variantes, comparadas sin mayúsculas, sin
#' acentos y sin un prefijo de código (`99 - Ignorado`). `Ninguno`, `None`,
#' `Otro` o `No` son categorías válidas y no se proponen. `etiquetas_ausencia`
#' agrega entradas, como `cadenas_ausencia` agrega a la lista de marcas. Cada
#' propuesta dice su `semantica` (`sin_dato`, `no_responde`, `no_corresponde`,
#' `ambigua` o `declarada`) y su `coincidencia`: `exacta`, `compuesta` (todas
#' sus partes están en el léxico, `No sabe / No responde`) o `parcial`
#' (alguna, `Otro / sin dato`). Declarar un código `no_corresponde` como
#' ausencia lo cuenta como faltante; si la pregunta no aplica a esas filas, el
#' instrumento es `aplicabilidad`.
#'
#' **La evidencia.** `n_filas` son las celdas de esa columna que el perfil
#' contará como esa ausencia si se confirma: se mide con [medir_dbi()] y
#' `NoNulo(valores_nulos = <código>)`, que cuenta con la regla del perfil —un
#' número por su valor, un texto sin mayúsculas ni blancos de los bordes—, una
#' consulta por tabla. El argumento lo decide la columna que usa el código, no
#' la tabla de códigos: `sentinelas_numericos` si R la lee como número,
#' `cadenas_ausencia` si la lee como texto; una tabla de códigos que usan una
#' columna entera y una de texto da una fila para cada una. Lo que no se pudo
#' contar queda con `n_filas = NA` y su `motivo`.
#'
#' **Un código válido que el perfil cuenta como ausencia.** Si un código está
#' en la lista incorporada de marcas (`NC`, `999`) y su etiqueta no es de
#' ausencia («Nivel completo»), la fila sale con `semantica =
#' "codigo_valido_contado"` e `incluir = NA`: el perfil lo cuenta como faltante
#' disfrazado y hoy no hay cómo retirar una marca incorporada de una columna.
#'
#' @param coleccion Objeto creado por [coleccion()].
#' @param perfil Objeto creado por [perfilar_coleccion()] sobre esa colección.
#'   Con `conservar_perfiles = TRUE` se reusa su catálogo.
#' @param relaciones Resultado opcional de [relaciones_coleccion()].
#' @param max_filas Tope de filas de una tabla de códigos: las de más no se
#'   leen.
#' @param etiquetas_ausencia Etiquetas que se agregan al léxico incorporado.
#' @param umbral_cobertura Cobertura mínima de una columna que usa la tabla
#'   para que ésta sea `candidata`, como en [relaciones_coleccion()]. Sólo los
#'   usos que la alcanzan reciben propuestas.
#'
#' @return Objeto `tablas_codigos`: una lista con `tablas` (una fila por tabla
#'   chica y columna código, con su `estado`, su `cobertura_maxima` y su
#'   `motivo`), `usos` (una fila por columna que usa un código, con su `origen`
#'   y su `cobertura`), `propuesta` (data frame de
#'   clase `propuesta_ausencias`, una fila por código, etiqueta y columna que
#'   lo usa), `cobertura` (lo que no se evaluó y por qué) y `meta`.
#' @export
#'
#' @seealso [ausencias_desde_propuesta()], [perfilar_coleccion()],
#'   [relaciones_coleccion()], [referencial()]
#'
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbExecute(con, "CREATE TABLE cond (cod INTEGER PRIMARY KEY, etiqueta TEXT)")
#'   DBI::dbExecute(con, "INSERT INTO cond VALUES (1, 'Ocupado'), (9, 'No sabe')")
#'   DBI::dbExecute(con, paste(
#'     "CREATE TABLE personas (id INTEGER,",
#'     "cond INTEGER REFERENCES cond(cod))"
#'   ))
#'   DBI::dbExecute(con, "INSERT INTO personas VALUES (1, 1), (2, 9), (3, 9)")
#'   col <- coleccion(con, c("cond", "personas"))
#'   tc <- detectar_tablas_codigos(col, perfilar_coleccion(col))
#'   tc$propuesta[, c("codigo", "etiqueta", "tabla", "columna", "n_filas")]
#'   tc$propuesta$incluir <- TRUE
#'   ausencias_desde_propuesta(tc$propuesta)
#'   DBI::dbDisconnect(con)
#' }
detectar_tablas_codigos <- function(coleccion, perfil, relaciones = NULL,
                                    max_filas = 1000L,
                                    etiquetas_ausencia = NULL,
                                    umbral_cobertura = 0.9) {
  .fijar_numerica_c()
  .argumento_dbi(.validar_argumentos_tablas_codigos(
    coleccion, perfil, relaciones, max_filas, etiquetas_ausencia,
    umbral_cobertura
  ))
  max_filas <- as.integer(max_filas)
  conexion <- coleccion$conexion
  .validar_conexion_dbi(conexion, accion = "detectar tablas de c\u00f3digos")
  declaradas <- .completar_columnas_coleccion(coleccion$tablas)
  resumen <- perfil$resumen_coleccion
  consultas <- new.env(parent = emptyenv())
  consultas$n <- 0L
  cobertura <- list()
  cubrir <- function(tabla, alcance, motivo) {
    cobertura[[length(cobertura) + 1L]] <<- data.frame(
      tabla = tabla, alcance = alcance, motivo = motivo, stringsAsFactors = FALSE
    )
  }
  ids <- as.character(declaradas$identificador)
  n_filas <- stats::setNames(rep(NA_real_, length(ids)), ids)
  if (is.data.frame(resumen) && nrow(resumen)) {
    k <- match(ids, as.character(resumen$identificador))
    n_filas[!is.na(k)] <- as.numeric(resumen$n_filas[k[!is.na(k)]])
  }
  for (id in ids[is.na(n_filas)]) {
    cubrir(id, "sin_conteo", paste0(
      "El perfil de la colecci\u00f3n no trae el conteo de filas de `", id,
      "`: no se sabe si es una tabla chica."
    ))
  }
  chicas <- ids[!is.na(n_filas) & n_filas <= max_filas]
  lector <- .lector_tablas_coleccion(
    conexion, coleccion, muestra = max_filas, orden = NULL, tope_cache_mb = 64
  )
  formas <- list()
  for (id in chicas) {
    datos <- lector$leer(id)
    if (inherits(datos, "condition") || !is.data.frame(datos)) {
      cubrir(id, "lectura", paste0(
        "No se pudo leer la tabla `", id, "`: ",
        if (inherits(datos, "condition")) conditionMessage(datos) else "sin datos"
      ))
      next
    }
    if (nrow(datos) < n_filas[[id]]) {
      cubrir(id, "lectura_parcial", paste0(
        "Se leyeron ", nrow(datos), " de ", n_filas[[id]], " filas de `", id,
        "`: sus claves no se pueden afirmar sobre una lectura parcial."
      ))
      next
    }
    claves <- names(datos)[vapply(seq_along(datos), function(i) {
      nrow(datos) > 0L && isTRUE(.es_clave(datos, i))
    }, logical(1L))]
    textos <- names(datos)[vapply(datos, function(x) {
      is.character(x) || is.factor(x)
    }, logical(1L))]
    formas[[id]] <- list(datos = datos, claves = claves, textos = textos)
  }
  consultas$n <- consultas$n + as.integer(length(chicas))
  # Los usos: claves foraneas declaradas y relaciones medidas.
  usos <- list()
  agregar_uso <- function(tabla_codigos, columna_codigo, tabla, columna, origen,
                          validada = NA, cobertura_uso = NA_real_,
                          muestreado = NA) {
    if (identical(tabla_codigos, tabla)) return(invisible(NULL))
    if (!tabla_codigos %in% names(formas)) return(invisible(NULL))
    clave <- .nombres_resueltos(columna_codigo, formas[[tabla_codigos]]$claves)
    if (!length(clave) || is.na(clave)) return(invisible(NULL))
    usos[[length(usos) + 1L]] <<- data.frame(
      tabla_codigos = tabla_codigos, columna_codigo = clave, tabla = tabla,
      columna = columna, origen = origen, validada = as.logical(validada),
      cobertura = as.numeric(cobertura_uso), muestreado = as.logical(muestreado),
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(declaradas))) {
    fila <- declaradas[i, , drop = FALSE]
    fks <- .claves_foraneas_tabla_codigos(conexion, fila, perfil, consultas)
    for (k in seq_len(nrow(fks))) {
      columnas <- as.character(fks$columnas[[k]])
      referenciadas <- as.character(fks$columnas_referenciadas[[k]])
      destino <- .fisica_identificador_codigos(fks$tabla_referenciada[[k]], declaradas)
      if (is.na(destino)) next
      if (length(columnas) != 1L || length(referenciadas) != 1L) {
        cubrir(destino, "clave_compuesta", paste0(
          "La clave for\u00e1nea `", fks$nombre[[k]], "` de `", fila$identificador,
          "` es compuesta: no se propone por columna."
        ))
        next
      }
      agregar_uso(destino, referenciadas, fila$identificador, columnas,
                  "clave_foranea_declarada", validada = fks$validada[[k]])
    }
  }
  if (!is.null(relaciones) && is.data.frame(relaciones$relaciones) &&
      nrow(relaciones$relaciones)) {
    r <- relaciones$relaciones
    for (k in seq_len(nrow(r))) {
      card <- as.character(r$cardinalidad[[k]])
      if (!card %in% c("1:1", "1:m", "m:1")) next
      t1 <- as.character(r$tabla_1[[k]])
      t2 <- as.character(r$tabla_2[[k]])
      c1 <- as.character(r$columna_tabla1[[k]])
      c2 <- as.character(r$columna_tabla2[[k]])
      if (card %in% c("1:1", "1:m")) {
        agregar_uso(t1, c1, t2, c2, "relacion_medida",
                    cobertura_uso = r$cobertura_tabla2_en_tabla1[[k]],
                    muestreado = r$muestreado_2[[k]] %||% NA)
      }
      if (card %in% c("1:1", "m:1")) {
        agregar_uso(t2, c2, t1, c1, "relacion_medida",
                    cobertura_uso = r$cobertura_tabla1_en_tabla2[[k]],
                    muestreado = r$muestreado_1[[k]] %||% NA)
      }
    }
  }
  usos <- if (length(usos)) do.call(rbind, usos) else .usos_codigos_vacios()
  rownames(usos) <- NULL
  # La cobertura de cada uso declarado se mide en el motor: la proporcion de las
  # filas de la columna con valor que son un codigo de la tabla. Una clave
  # foranea declarada no dice cuantas filas la cumplen.
  usos <- .cobertura_usos_codigos(usos, coleccion, consultas)
  # Las tablas, una fila por tabla chica y columna codigo.
  filas_tablas <- list()
  pares_sugeridos <- list()
  for (id in names(formas)) {
    forma <- formas[[id]]
    if (!length(forma$claves)) {
      filas_tablas[[length(filas_tablas) + 1L]] <- data.frame(
        tabla = id, columna_codigo = NA_character_, n_filas = n_filas[[id]],
        n_columnas = ncol(forma$datos),
        columnas_texto = paste(forma$textos, collapse = ", "),
        n_usos = 0L, cobertura_maxima = NA_real_, estado = "sin_clave",
        motivo = "Ninguna columna est\u00e1 completa y es \u00fanica.",
        stringsAsFactors = FALSE
      )
      next
    }
    usadas <- unique(usos$columna_codigo[usos$tabla_codigos == id])
    candidatas <- if (length(usadas)) usadas else forma$claves
    for (cc in candidatas) {
      propios <- usos$tabla_codigos == id & usos$columna_codigo == cc
      n_usos <- sum(propios)
      cobertura_maxima <- if (n_usos && any(!is.na(usos$cobertura[propios]))) {
        max(usos$cobertura[propios], na.rm = TRUE)
      } else NA_real_
      # La forma no alcanza: `candidata` exige que otra tabla la use con la
      # cobertura pedida. Una clave foranea con huerfanos, o una cobertura que
      # no se pudo medir, deja la tabla `posible` con el motivo.
      suficiente <- isTRUE(cobertura_maxima >= umbral_cobertura)
      if (!n_usos) {
        pares_sugeridos[[length(pares_sugeridos) + 1L]] <- data.frame(
          tabla_1 = id, tabla_2 = setdiff(ids, id), stringsAsFactors = FALSE
        )
      }
      filas_tablas[[length(filas_tablas) + 1L]] <- data.frame(
        tabla = id, columna_codigo = cc, n_filas = n_filas[[id]],
        n_columnas = ncol(forma$datos),
        columnas_texto = paste(setdiff(forma$textos, cc), collapse = ", "),
        n_usos = as.integer(n_usos), cobertura_maxima = cobertura_maxima,
        estado = if (suficiente) "candidata" else "posible",
        motivo = if (suficiente) NA_character_ else if (n_usos) paste0(
          "La usan ", n_usos, " columnas, pero ninguna con cobertura de al menos ",
          umbral_cobertura, if (is.na(cobertura_maxima)) {
            " (no se pudo medir)"
          } else paste0(" (la mayor es ", signif(cobertura_maxima, 4), ")"), "."
        ) else paste(
          "Tiene forma de tabla de c\u00f3digos, pero ninguna clave for\u00e1nea",
          "declarada ni relaci\u00f3n medida la usa: pasar `relaciones` de",
          "relaciones_coleccion() con `meta$pares_sugeridos`."
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  tablas <- if (length(filas_tablas)) do.call(rbind, filas_tablas) else .tablas_codigos_vacias()
  rownames(tablas) <- NULL
  propuesta <- .propuesta_tablas_codigos(
    formas, tablas, usos, coleccion, etiquetas_ausencia, consultas,
    umbral_cobertura
  )
  pares <- if (length(pares_sugeridos)) unique(do.call(rbind, pares_sugeridos)) else
    data.frame(tabla_1 = character(), tabla_2 = character(), stringsAsFactors = FALSE)
  rownames(pares) <- NULL
  estructura <- list(
    tablas = tablas, usos = usos, propuesta = propuesta,
    cobertura = if (length(cobertura)) do.call(rbind, cobertura) else data.frame(
      tabla = character(), alcance = character(), motivo = character(),
      stringsAsFactors = FALSE
    ),
    meta = list(
      max_filas = max_filas, umbral_cobertura = umbral_cobertura,
      n_tablas = length(ids),
      n_chicas = length(chicas),
      n_grandes = sum(!is.na(n_filas) & n_filas > max_filas),
      n_etiquetas_leidas = attr(propuesta, "n_etiquetas_leidas") %||% 0L,
      consultas = consultas$n + (attr(propuesta, "consultas") %||% 0L),
      pares_sugeridos = pares,
      lecturas = lector$bitacora()
    )
  )
  attr(estructura$propuesta, "n_etiquetas_leidas") <- NULL
  attr(estructura$propuesta, "consultas") <- NULL
  class(estructura) <- "tablas_codigos"
  estructura
}

# El identificador de la coleccion de una tabla que el catalogo nombra.
.fisica_identificador_codigos <- function(nombre, declaradas) {
  if (length(nombre) != 1L || is.na(nombre)) return(NA_character_)
  i <- match(nombre, as.character(declaradas$identificador))
  if (is.na(i)) i <- match(nombre, as.character(declaradas$tabla))
  if (is.na(i)) NA_character_ else as.character(declaradas$identificador[[i]])
}

.propuesta_tablas_codigos <- function(formas, tablas, usos, coleccion,
                                      etiquetas_ausencia, consultas,
                                      umbral_cobertura = 0.9) {
  lexico <- .lexico_etiquetas_ausencia(etiquetas_ausencia)
  filas <- list()
  n_leidas <- 0L
  conexion <- coleccion$conexion
  motor <- .motor_medicion_dbi(conexion, "auto")
  clases_de <- list()
  clase_columna <- function(tabla, columna) {
    if (is.null(clases_de[[tabla]])) {
      fisica <- .fisica_tabla_coleccion_dbi(tabla, coleccion)
      esq <- tryCatch(
        .esquema_medicion_dbi(conexion, fisica, motor),
        error = function(e) NULL
      )
      consultas$n <- consultas$n + as.integer(esq$n_consultas_esquema %||% 1L)
      clases_de[[tabla]] <<- if (is.null(esq)) list() else as.list(esq$clases)
    }
    clase <- clases_de[[tabla]][[columna]]
    if (is.null(clase)) NA_character_ else clase
  }
  marcas <- .cadenas_na()
  for (k in seq_len(nrow(tablas))) {
    fila_tabla <- tablas[k, , drop = FALSE]
    # Una tabla que nada usa no tiene donde declarar un codigo: queda en
    # `tablas` como `posible`, sin propuesta.
    if (is.na(fila_tabla$columna_codigo) || fila_tabla$estado != "candidata") next
    id <- fila_tabla$tabla
    cc <- fila_tabla$columna_codigo
    datos <- formas[[id]]$datos
    etiquetas_cols <- setdiff(formas[[id]]$textos, cc)
    if (!length(etiquetas_cols)) next
    codigos <- .texto_identidad(.valores_relacion(datos[[cc]]))
    # Solo los usos con la cobertura pedida: una columna con muchos huerfanos no
    # es una que use esta tabla de codigos.
    usos_tabla <- usos[usos$tabla_codigos == id & usos$columna_codigo == cc &
                         !is.na(usos$cobertura) &
                         usos$cobertura >= umbral_cobertura, , drop = FALSE]
    usos_tabla <- usos_tabla[!duplicated(paste(usos_tabla$tabla, usos_tabla$columna, sep = "\r")), , drop = FALSE]
    for (ec in etiquetas_cols) {
      etiquetas <- as.character(datos[[ec]])
      n_leidas <- n_leidas + sum(!is.na(etiquetas))
      clase <- .clasificar_etiqueta_ausencia(etiquetas, lexico)
      # Un codigo que esta en las marcas incorporadas -o en los centinelas por
      # omision- con una etiqueta que no es de ausencia: el perfil lo cuenta como
      # faltante disfrazado y es un valor valido.
      es_marca <- .minusculas_para_comparar(codigos) %in% marcas |
        suppressWarnings(as.numeric(codigos)) %in% .numeros_na_locales
      valido_contado <- is.na(clase$semantica) & es_marca & !is.na(etiquetas)
      cuales <- which(!is.na(clase$semantica) | valido_contado)
      for (i in cuales) {
        semantica <- if (valido_contado[[i]]) "codigo_valido_contado" else clase$semantica[[i]]
        destinos <- usos_tabla
        for (u in seq_len(nrow(destinos))) {
          tabla_u <- destinos$tabla[[u]]
          columna_u <- destinos$columna[[u]]
          clase_u <- clase_columna(tabla_u, columna_u)
          argumento <- if (clase_u %in% c("entero", "doble", "entero64")) {
            "sentinelas_numericos"
          } else if (identical(clase_u, "texto")) "cadenas_ausencia" else NA_character_
          motivo <- if (is.na(argumento)) {
            paste0("La columna `", columna_u, "` no es n\u00famero ni texto para R.")
          } else if (valido_contado[[i]]) {
            paste0(
              "El c\u00f3digo est\u00e1 en la lista incorporada de marcas de ausencia y la ",
              "tabla de c\u00f3digos lo etiqueta como un valor: el perfil puede contarlo ",
              "como faltante disfrazado. Hoy no hay c\u00f3mo retirar una marca ",
              "incorporada de una columna."
            )
          } else NA_character_
          filas[[length(filas) + 1L]] <- data.frame(
            id_propuesta = paste0(id, ".", cc, "=", codigos[[i]], "@",
                                  tabla_u %||% NA, ".", columna_u %||% NA,
                                  "#", ec),
            tabla_codigos = id, columna_codigo = cc, codigo = codigos[[i]],
            columna_etiqueta = ec, etiqueta = etiquetas[[i]],
            semantica = semantica,
            coincidencia = if (valido_contado[[i]]) NA_character_ else clase$coincidencia[[i]],
            tabla = tabla_u, columna = columna_u, argumento = argumento,
            n_filas = NA_real_, motivo = motivo,
            incluir = if (is.na(argumento) || valido_contado[[i]]) NA else FALSE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  propuesta <- if (length(filas)) do.call(rbind, filas) else .propuesta_ausencias_vacia()
  rownames(propuesta) <- NULL
  conteo <- .contar_codigos_propuesta(propuesta, coleccion)
  propuesta$n_filas <- conteo$n_filas
  sin_contar <- is.na(propuesta$n_filas) & !is.na(conteo$motivo)
  propuesta$motivo[sin_contar & is.na(propuesta$motivo)] <- conteo$motivo[
    sin_contar & is.na(propuesta$motivo)
  ]
  class(propuesta) <- c("propuesta_ausencias", "data.frame")
  attr(propuesta, "n_etiquetas_leidas") <- n_leidas
  attr(propuesta, "consultas") <- conteo$consultas
  propuesta
}

# Cuantas celdas de cada columna llevan el codigo, con la regla del perfil:
# `NoNulo(valores_nulos = codigo)` frente a `NoNulo` sin el, por `medir_dbi()`,
# que consolida una consulta por tabla.
.contar_codigos_propuesta <- function(propuesta, coleccion) {
  n <- nrow(propuesta)
  salida <- list(n_filas = rep(NA_real_, n), motivo = rep(NA_character_, n),
                 consultas = 0L)
  medibles <- which(!is.na(propuesta$argumento))
  if (!length(medibles)) return(salida)
  # Dos filas con el mismo codigo en la misma columna -dos etiquetas que casan-
  # son una sola medida: el modelo no admite dos instancias con la misma
  # definicion.
  clave_medida <- paste(propuesta$tabla, propuesta$columna, propuesta$argumento,
                        propuesta$codigo, sep = "\r")
  primera <- match(clave_medida, clave_medida)
  nucleo <- metricas_nucleo()
  instancias <- list()
  bases <- character()
  base_de <- function(i) {
    paste0("lupa_b", match(
      paste(propuesta$tabla[[i]], propuesta$columna[[i]], sep = "\r"), bases
    ))
  }
  for (i in medibles[primera[medibles] == medibles]) {
    valor <- if (identical(propuesta$argumento[[i]], "sentinelas_numericos")) {
      as.numeric(propuesta$codigo[[i]])
    } else propuesta$codigo[[i]]
    instancias[[length(instancias) + 1L]] <- instanciar(
      especializar(nucleo$NoNulo, valores_nulos = valor),
      propuesta$tabla[[i]], propuesta$columna[[i]], nombre = paste0("lupa_c", i)
    )
    clave_base <- paste(propuesta$tabla[[i]], propuesta$columna[[i]], sep = "\r")
    if (!clave_base %in% bases) {
      bases <- c(bases, clave_base)
      instancias[[length(instancias) + 1L]] <- instanciar(
        especializar(nucleo$NoNulo), propuesta$tabla[[i]], propuesta$columna[[i]],
        nombre = base_de(i)
      )
    }
  }
  medicion <- tryCatch(
    suppressWarnings(medir_dbi(do.call(modelo, instancias), coleccion)),
    error = function(e) e
  )
  if (inherits(medicion, "error")) {
    salida$motivo[medibles] <- paste0(
      "No se pudo contar en el motor: ", conditionMessage(medicion)
    )
    return(salida)
  }
  salida$consultas <- as.integer(attr(medicion, "ejecucion_dbi")$consultas %||% 0L)
  numerador <- stats::setNames(as.numeric(medicion$numerador), medicion$metrica_instanciada)
  cobertura <- attr(medicion, "cobertura_metricas")
  for (i in medibles) {
    con_codigo <- numerador[paste0("lupa_c", primera[[i]])]
    base <- numerador[base_de(i)]
    if (!is.na(con_codigo) && !is.na(base)) {
      salida$n_filas[[i]] <- base - con_codigo
    } else if (is.data.frame(cobertura) && nrow(cobertura)) {
      motivo <- cobertura$motivo[
        cobertura$metrica_instanciada %in% paste0("lupa_c", primera[[i]])
      ]
      salida$motivo[[i]] <- if (length(motivo)) motivo[[1L]] else
        "No se pudo contar en el motor."
    }
  }
  salida
}

#' @export
print.tablas_codigos <- function(x, ...) {
  cat("Tablas de c\u00f3digos\n")
  .cat_publicado(
    "  Tablas: ", x$meta$n_tablas, " (", x$meta$n_chicas, " con a lo sumo ",
    x$meta$max_filas, " filas)\n"
  )
  estados <- table(factor(x$tablas$estado, levels = c("candidata", "posible", "sin_clave")))
  .cat_publicado(
    "  Candidatas: ", estados[["candidata"]], "; posibles: ", estados[["posible"]],
    "; sin clave: ", estados[["sin_clave"]], "\n"
  )
  .cat_publicado("  Usos: ", nrow(x$usos), "\n")
  .cat_publicado(
    "  Propuestas de ausencia: ", sum(!is.na(x$propuesta$incluir)),
    " (ninguna se aplica sola: marcar `incluir` y usar ausencias_desde_propuesta())\n"
  )
  if (nrow(x$cobertura)) {
    .cat_publicado("  Sin evaluar: ", nrow(x$cobertura), " (ver `cobertura`)\n")
  }
  invisible(x)
}

#' Convertir una propuesta de ausencias en la declaración por columna
#'
#' Toma las filas de una propuesta de [detectar_tablas_codigos()] que el
#' usuario marcó con `incluir = TRUE` y devuelve las listas que reciben
#' `sentinelas_numericos` y `cadenas_ausencia`: por tabla para
#' [perfilar_coleccion()] o, con `tabla`, por columna para [perfilar()] y
#' [perfilar_dbi()]. Cada código va al argumento que dice su fila: un número
#' para una columna que R lee como número, un texto para una de texto.
#'
#' En una columna con entrada propia, `sentinelas_numericos` reemplaza la lista
#' por omisión, como el vector la reemplaza en todas: en una columna gobernada
#' por una tabla de códigos, la tabla dice qué significa cada valor y uno que no
#' está es un huérfano, no un centinela. Las columnas sin filas incluidas no
#' aparecen y reciben el valor por omisión.
#'
#' @param propuesta El componente `propuesta` de [detectar_tablas_codigos()], o
#'   el objeto entero.
#' @param tabla Identificador de una tabla de la colección. Con él, la salida
#'   son las listas por columna de esa tabla.
#'
#' @return Lista con `sentinelas_numericos` y `cadenas_ausencia`, cada una
#'   presente solo si tiene alguna entrada, lista para `do.call()`.
#' @export
#'
#' @seealso [detectar_tablas_codigos()], [perfilar()], [perfilar_coleccion()]
ausencias_desde_propuesta <- function(propuesta, tabla = NULL) {
  if (inherits(propuesta, "tablas_codigos")) propuesta <- propuesta$propuesta
  necesarias <- c("tabla", "columna", "argumento", "codigo", "incluir")
  if (!is.data.frame(propuesta) || !all(necesarias %in% names(propuesta))) {
    .detener(
      "`propuesta` debe venir de detectar_tablas_codigos(): faltan las columnas ",
      paste(setdiff(necesarias, names(propuesta)), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.logical(propuesta$incluir)) {
    .detener("`incluir` debe ser l\u00f3gica.", call. = FALSE)
  }
  incluidas <- which(propuesta$incluir %in% TRUE)
  sin_destino <- incluidas[is.na(propuesta$argumento[incluidas]) |
                            is.na(propuesta$tabla[incluidas]) |
                            is.na(propuesta$columna[incluidas])]
  if (length(sin_destino)) {
    .detener(
      "Una fila incluida no tiene d\u00f3nde declararse (sin tabla, columna o ",
      "argumento): ", paste(propuesta$id_propuesta[sin_destino] %||% sin_destino,
                            collapse = ", "), ".", call. = FALSE
    )
  }
  if (!is.null(tabla) && (!is.character(tabla) || length(tabla) != 1L || is.na(tabla))) {
    .detener("`tabla` debe ser NULL o un identificador de tabla.", call. = FALSE)
  }
  if (!length(incluidas)) {
    .avisar(
      "Ninguna fila de la propuesta tiene `incluir = TRUE`: la declaraci\u00f3n ",
      "queda vac\u00eda. Marcar las filas que se confirman.", call. = FALSE
    )
  }
  armar <- function(argumento) {
    filas <- propuesta[incluidas[propuesta$argumento[incluidas] == argumento], , drop = FALSE]
    if (!nrow(filas)) return(NULL)
    valores <- if (identical(argumento, "sentinelas_numericos")) {
      as.numeric(filas$codigo)
    } else as.character(filas$codigo)
    # Agrupado sin `split()`, que ordena por la intercalacion del locale: el
    # orden de las tablas y las columnas es el de sus bytes.
    agrupar <- function(indices, por, f) {
      grupos <- .sort_por_bytes(unique(por[indices]))
      stats::setNames(lapply(grupos, function(g) f(indices[por[indices] == g])), grupos)
    }
    agrupar(seq_len(nrow(filas)), filas$tabla, function(i) {
      agrupar(i, filas$columna, function(j) .sort_por_bytes(unique(valores[j])))
    })
  }
  salida <- list(
    sentinelas_numericos = armar("sentinelas_numericos"),
    cadenas_ausencia = armar("cadenas_ausencia")
  )
  if (!is.null(tabla)) {
    salida <- lapply(salida, function(x) {
      if (is.null(x)) return(NULL)
      i <- .indice_nombre(tabla, names(x))
      if (is.na(i)) NULL else x[[i]]
    })
  }
  Filter(Negate(is.null), salida)
}

# La cobertura de los usos que no la traen -las claves foraneas declaradas-:
# `CorrectitudSemFuerte` contra la tabla de codigos como referencial del motor,
# con la identidad de la relacion (`normalizar = FALSE`), una consulta por uso.
.cobertura_usos_codigos <- function(usos, coleccion, consultas) {
  faltan <- which(is.na(usos$cobertura))
  if (!length(faltan)) return(usos)
  fuerte <- metricas_referencial()$CorrectitudSemFuerte
  instancias <- lapply(faltan, function(i) {
    ref <- referencial(
      usos$tabla_codigos[[i]], usos$columna_codigo[[i]], normalizar = FALSE,
      nombre = usos$tabla_codigos[[i]]
    )
    instanciar(especializar(fuerte), usos$tabla[[i]], usos$columna[[i]],
               referencial = ref, nombre = paste0("lupa_u", i))
  })
  medicion <- tryCatch(
    suppressWarnings(medir_dbi(do.call(modelo, instancias), coleccion)),
    error = function(e) NULL
  )
  if (is.null(medicion)) return(usos)
  consultas$n <- consultas$n + as.integer(attr(medicion, "ejecucion_dbi")$consultas %||% 0L)
  resultado <- stats::setNames(as.numeric(medicion$resultado), medicion$metrica_instanciada)
  for (i in faltan) {
    valor <- resultado[paste0("lupa_u", i)]
    if (length(valor) && !is.na(valor)) {
      usos$cobertura[[i]] <- unname(valor)
      usos$muestreado[[i]] <- FALSE
    }
  }
  usos
}
