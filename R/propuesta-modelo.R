.propuesta_vacia <- function() {
  x <- data.frame(
    id_sugerencia = character(), prioridad = character(), incluir = logical(),
    metrica = character(), entidad = character(), atributos = character(),
    origen = character(), justificacion = character(), estado = character(),
    stringsAsFactors = FALSE
  )
  x$configuracion <- I(list())
  x$entidades_ligadas <- I(list())
  x$atributos_ligados <- I(list())
  x$referencial <- I(list())
  x
}

.nueva_sugerencia <- function(prioridad, incluir, metrica, entidad, atributos,
                              origen, justificacion, configuracion = list(),
                              entidades_ligadas = entidad,
                              atributos_ligados = atributos,
                              referencial = NULL, estado = "lista") {
  x <- data.frame(
    id_sugerencia = "", prioridad = prioridad, incluir = incluir,
    metrica = metrica, entidad = .pegar_nombres(entidad),
    atributos = .pegar_nombres(atributos), origen = origen,
    justificacion = justificacion, estado = estado,
    stringsAsFactors = FALSE
  )
  x$configuracion <- I(list(configuracion))
  x$entidades_ligadas <- I(list(entidades_ligadas))
  x$atributos_ligados <- I(list(atributos_ligados))
  x$referencial <- I(list(referencial))
  x
}

.escapar_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x, perl = TRUE)
}

.patron_a_regex <- function(patron) {
  caracteres <- strsplit(patron, "", fixed = TRUE)[[1L]]
  salida <- character()
  i <- 1L
  while (i <= length(caracteres)) {
    token <- caracteres[[i]]
    if (token %in% c("9", "a", "A")) {
      pieza <- switch(token, `9` = "[0-9]", a = "[[:lower:]]", A = "[[:upper:]]")
      if (i < length(caracteres) && caracteres[[i + 1L]] == "+") {
        pieza <- paste0(pieza, "+")
        i <- i + 1L
      }
      salida <- c(salida, pieza)
    } else {
      salida <- c(salida, .escapar_regex(token))
    }
    i <- i + 1L
  }
  paste0("^", paste0(salida, collapse = ""), "$")
}

.regla_desde_dependencia <- function(datos, determinante, dependiente) {
  indices <- .indice_nombre(c(determinante, dependiente), names(datos))
  if (anyNA(indices)) return(function(df) rep(FALSE, nrow(df)))
  determinante <- names(datos)[indices[[1L]]]
  dependiente <- names(datos)[indices[[2L]]]
  mapa <- .mapa_dependencia(datos, determinante, dependiente, soporte_minimo = 1L)
  claves <- .valores_relacion(mapa$determinante)
  valores <- .valores_relacion(mapa$dependiente)
  function(df) {
    indices_df <- .indice_nombre(c(determinante, dependiente), names(df))
    if (anyNA(indices_df)) return(rep(FALSE, nrow(df)))
    x <- .valores_relacion(df[[indices_df[[1L]]]])
    y <- .valores_relacion(df[[indices_df[[2L]]]])
    indice <- match(x, claves)
    ausente <- is.na(df[[indices_df[[1L]]]]) |
      is.na(df[[indices_df[[2L]]]])
    resultado <- !is.na(indice) & y == valores[indice]
    resultado[ausente] <- TRUE
    resultado[is.na(resultado)] <- FALSE
    resultado
  }
}

# El id de una sugerencia se arma con lo que la identifica, nunca con su
# posicion (decision D3, ronda 32). Era `sugerencia-NNNN`, numerado tras ordenar
# por nombre de columna: una columna nueva corria los numeros y una regla que
# nombraba `sugerencia-0002` -la `NoNulo` de `edad`- pasaba a medir la de
# `apellido` en la corrida siguiente. Medido: de siete sugerencias comunes a dos
# corridas, una conservaba su id, y la deriva publicaba un deterioro `error` que
# era del id y no de los datos.
#
# El id es `Metrica:atributo,atributo`: la metrica y sus atributos en su orden
# -una dependencia `a -> b` no es `b -> a`-. Cuando la sugerencia liga mas de una
# entidad -una regla entre tablas- cada atributo lleva su entidad delante
# (`personas.id`), porque la direccion es lo que distingue las dos sugerencias
# de una misma relacion. La entidad de una sugerencia de una sola tabla NO entra:
# es el nombre del perfil, que sale de la expresion perfilada (`enero`,
# `febrero`) y cambiaria el id de una corrida a otra sobre la misma tabla.
#
# Ningun parametro de la configuracion entra: la propuesta no ofrece dos
# instancias de la misma metrica sobre los mismos atributos, y meter el patron
# de un `Formato` haria que una regla perdiera su metrica cuando el patron
# dominante cambia. Ese cambio es de configuracion y el historico lo declara en
# `configuracion_modelo`.
#
# Los signos que el id usa como sintaxis se escapan en cada nombre como en una
# URL -`%` es `%25`, `,` es `%2C` y `#` es `%23`; y `.` es `%2E` en la
# entidad-, asi que dos contenidos distintos no dan el mismo id. Un nombre `NA`
# se escribe `%NA`, que ningun nombre escapado produce.
#
# El id tiene que ser texto que el resto del paquete pueda unir y comparar, y
# un nombre de columna puede no serlo: con un nombre declarado `bytes` -los
# bytes latin1 de `nota` con la e con tilde- el id lo heredaba y `medir()`
# abortaba en su `rbind()` -"translating strings with bytes encoding is not
# allowed"- (lo atrapo `test-O139`). Por eso el id se arma por BYTES: lo que es
# UTF-8 valido queda como esta, marcado UTF-8, sea cual sea su marca -la marca no
# es parte del nombre, sus bytes si; es la regla de `.nombres_para_operar()`-; lo
# latin1 declarado se convierte primero; y en un nombre que no es UTF-8 valido
# cada byte alto se escribe `%XX`. El resultado es UTF-8 valido o ASCII, nunca
# `bytes`, igual en cualquier locale, y sigue siendo inyectivo: el `%` de un
# nombre real ya se escribio `%25`.
.componente_id_sugerencia <- function(x, entidad = FALSE) {
  x <- as.character(x)
  especiales <- as.integer(charToRaw(paste0(
    "%,#", if (entidad) "." else ""
  )))
  vapply(x, function(nombre) {
    if (is.na(nombre)) return("%NA")
    if (identical(Encoding(nombre), "latin1")) {
      nombre <- iconv(nombre, from = "latin1", to = "UTF-8")
    }
    bytes <- charToRaw(nombre)
    codigos <- as.integer(bytes)
    crudo <- rawToChar(bytes)
    escapar <- codigos %in% especiales
    if (!validUTF8(crudo)) escapar <- escapar | codigos >= 128L
    salida <- if (any(escapar)) {
      piezas <- lapply(seq_along(bytes), function(k) {
        if (escapar[[k]]) charToRaw(sprintf("%%%02X", codigos[[k]])) else bytes[[k]]
      })
      rawToChar(do.call(c, piezas))
    } else crudo
    if (any(charToRaw(salida) >= as.raw(128L))) Encoding(salida) <- "UTF-8"
    salida
  }, character(1L), USE.NAMES = FALSE)
}

.id_contenido_sugerencia <- function(metrica, entidades, atributos) {
  atributos <- as.character(atributos)
  if (!length(atributos)) return(as.character(metrica))
  componentes <- .componente_id_sugerencia(atributos)
  entidades <- as.character(entidades)
  if (length(entidades) > 1L && length(entidades) == length(atributos)) {
    componentes <- .pegar_texto0(
      .componente_id_sugerencia(entidades, entidad = TRUE), ".", componentes
    )
  }
  .pegar_texto0(metrica, ":", .pegar_texto(componentes, collapse = ","))
}

# Dos sugerencias con el mismo contenido -una tabla que repite el nombre de una
# columna, una relacion pasada dos veces- se distinguen por su ORDINAL entre las
# de igual contenido, en el orden en que se generan: el de las columnas de la
# tabla. La primera conserva el id sin sufijo y las siguientes llevan `#2`,
# `#3`. Agregar o quitar otra columna no lo mueve; lo unico que lo mueve es
# agregar otra columna con ese mismo nombre antes, y ahi no hay otro dato que
# las distinga.
.ids_sugerencias <- function(resultado) {
  if (!nrow(resultado)) return(character())
  base <- vapply(seq_len(nrow(resultado)), function(i) {
    .id_contenido_sugerencia(
      resultado$metrica[[i]], resultado$entidades_ligadas[[i]],
      resultado$atributos_ligados[[i]]
    )
  }, character(1L))
  clave <- .clave_bytes(.nombres_para_operar(base))
  ordinal <- vapply(seq_along(clave), function(i) {
    sum(clave[seq_len(i)] == clave[[i]])
  }, integer(1L))
  repetido <- ordinal > 1L
  if (any(repetido)) {
    base[repetido] <- .pegar_texto0(base[repetido], "#", ordinal[repetido])
  }
  base
}

# Los faltantes disfrazados de una columna, para contarlos en su `NoNulo`
# (decision D6, ronda 32). La ronda 31 dejaba esa `NoNulo` sin medir; Sebastian
# lo revirtio: primero normalizar, y si no se puede, medir igual con los valores
# crudos. Normalizar es declarar en `valores_nulos` los valores que el perfil
# conto como disfrazados, y la mascara sale de la MISMA funcion que usa el perfil
# (`.faltantes_disfrazados_columna()`, por `.contexto_casi_clave()`) con la
# politica que el perfil declaro -sus `sentinelas_numericos` y sus
# `cadenas_ausencia`-. Como la mascara decide por valor, `x %in% valores_nulos`
# marca las mismas filas. Se ordenan por sus bytes para que dos corridas con
# los mismos valores declaren la misma configuracion.
.valores_nulos_disfrazados <- function(perfil, datos, columna) {
  if (is.null(datos)) return(NULL)
  indice <- .indice_nombre(columna, names(datos))
  if (length(indice) != 1L || is.na(indice)) return(NULL)
  x <- datos[[indice]]
  if (.es_columna_compuesta(x) || (is.list(x) && !is.factor(x))) return(NULL)
  mascara <- tryCatch(
    .contexto_casi_clave(x, perfil)$disfrazados,
    error = function(e) NULL
  )
  if (!is.logical(mascara) || length(mascara) != length(x)) return(NULL)
  mascara[is.na(mascara)] <- FALSE
  if (!any(mascara)) return(NULL)
  valores <- x[mascara]
  if (is.factor(valores)) valores <- as.character(valores)
  valores <- valores[!duplicated(valores)]
  valores[.orden_por_bytes(valores)]
}

# La `NoNulo` de una columna declarada opcional en el perfil se mide sobre las
# celdas presentes, el universo del perfil. Con un `NA` y tres `-999` en 40, el
# perfil contaba 3 de 39 y la metrica que la decision D6 propone por los
# disfrazados media 0,900 = 36/40: contaba como faltante el `NA` que la
# declaracion legitima (refutacion 33-D). La declaracion viaja en la propiedad
# `opcional` de la metrica -no en una formula `aplicable`, que tendria que
# nombrar la columna como simbolo y bajo `LC_CTYPE = "C"` no puede con un nombre
# no ASCII-, asi que vale igual por `medir()` directo, por `analizar()` y
# sobre la entrega siguiente.
.columna_declarada_opcional <- function(perfil, columna) {
  opcionales <- perfil$meta[["columnas_opcionales"]]
  is.character(opcionales) && length(opcionales) &&
    .nombres_para_operar(columna) %in% .nombres_para_operar(opcionales)
}

#' Proponer un modelo de calidad desde el profiling
#'
#' Construye el puente entre el examen de datos y la medición. El resultado no
#' ejecuta métricas: es una tabla editable donde cada fila conserva el hallazgo
#' o diagnóstico que la originó, la justificación, la configuración y una marca
#' `incluir`. [modelo_desde_propuesta()] materializa únicamente las filas que el
#' usuario deja activas.
#'
#' Las reglas observadas que pueden sobreajustarse a una entrega —dominios y
#' patrones dominantes— se proponen inactivas. Los controles estructurales
#' directos, como `NoNulo`, duplicación exacta y dependencias funcionales
#' exactas, se activan. `max_sugerencias` recorta después de ordenar por
#' prioridad y el objeto declara el total en sus atributos. Dentro de cada
#' prioridad las filas se ordenan por métrica, entidad y atributos según sus
#' bytes —no según la intercalación del locale—: el mismo orden en cualquier
#' sesión.
#'
#' `id_sugerencia` se arma con el **contenido** de la sugerencia, nunca con su
#' posición: la métrica y sus atributos en su orden —`NoNulo:edad`,
#' `ReglaIntegridadIntraEntidad:depto,codigo`— y, cuando la sugerencia liga dos
#' tablas, cada atributo con su entidad delante
#' (`ReglaIntegridadInterEntidad:personas.id,visitas.persona_id`). Una
#' sugerencia sin atributos lleva sólo la métrica (`EntidadDuplicada`). Así una
#' columna que se agrega o se quita no cambia el id de las demás, y una regla de
#' [regla_evaluacion()] que lo nombra sigue midiendo lo mismo en la corrida
#' siguiente: [modelo_desde_propuesta()] lo usa como nombre de la instancia, que
#' es la `metrica_instanciada` de [medir()], del tablero y del histórico. La
#' entidad de una sugerencia de una sola tabla no entra, porque es el nombre del
#' perfil y cambia con la expresión perfilada; la configuración tampoco: un
#' cambio de patrón o de dominio lo declara el histórico en
#' `configuracion_modelo`. Dentro de cada nombre se escribe `%25`, `%2C` y `%23`
#' en lugar de `%`, `,` y `#` —y `%2E` en lugar de `.` en una entidad—, y un
#' nombre `NA` se escribe `%NA`, de modo que dos contenidos distintos no dan el
#' mismo id. Un nombre que no es UTF-8 válido escribe cada byte alto como `%XX`
#' —los bytes latin1 de «notaé» sin declarar dan `nota%E9`—, así que el id es
#' siempre texto UTF-8 o ASCII, el mismo en cualquier locale. Si dos sugerencias
#' tienen el mismo contenido —una tabla que repite
#' el nombre de una columna—, la segunda lleva `#2` y la tercera `#3`, en el
#' orden de las columnas de la tabla.
#'
#' Si una columna contiene faltantes disfrazados, `NoNulo` se propone activa,
#' con estado `lista`, y **se mide contándolos**: los valores que el perfil contó
#' como disfrazados —con la misma regla y la misma declaración de
#' `sentinelas_numericos` y `cadenas_ausencia`— se declaran en `valores_nulos` de
#' su `configuracion`, y la justificación lo dice. Sobre cuarenta valores con un
#' `NA` y tres `-999` mide 0,90, no 0,975. Si no se pueden identificar —sin
#' `datos`, la propuesta no tiene de dónde sacarlos—, se mide igual con los
#' valores crudos, y la justificación dice que cuenta sólo los `NA` y que
#' sobrestima la completitud. [analizar()] la mide y [modelo_desde_propuesta()]
#' la materializa como a cualquier otra sugerencia activa. Si la columna se
#' declaró en `columnas_opcionales` de [perfilar()], la `NoNulo` lleva además
#' `opcional = TRUE` y se mide sobre las celdas presentes, el mismo universo
#' que el perfil: con un `NA` y tres `-999` en cuarenta celdas mide
#' 36/39 = 0,923, que es `1 - prop_faltantes_totales`.
#' Una sugerencia sobre una columna cuyo nombre la tabla repite sale inactiva y
#' con estado `nombre_repetido`: una métrica se liga por nombre, y ese nombre no
#' dice cuál de las columnas mide. [modelo_desde_propuesta()] no la materializa
#' y [analizar()] no la mide hasta que las columnas se renombren.
#'
#' @param perfil Objeto creado por [perfilar()].
#' @param datos Datos originales opcionales. Son necesarios para proponer
#'   dominios observados, materializar reglas de dependencia funcional e
#'   identificar los faltantes disfrazados que cuenta `NoNulo`.
#' @param relaciones Resultado opcional de [detectar_relaciones()].
#' @param entidades_relacion Dos nombres de entidad correspondientes a las
#'   tablas usadas en `relaciones`.
#' @param max_valores_dominio Máximo de valores para proponer un dominio por
#'   extensión.
#' @param max_sugerencias Máximo de filas devueltas.
#'
#' @return Data frame S3 de clase `propuesta_modelo`; las columnas de listas
#'   contienen la configuración y los vínculos sin convertirlos en texto. Su
#'   método `print()` ya no falla por codificación en ningún locale: la copia
#'   que se exhibe convierte lo que trae codificación declarada, declara como
#'   UTF-8 los bytes válidos sin declarar —R los escapa como `<U+00F1>` donde el
#'   locale no los represente— y deja el resto tal cual mientras R pueda
#'   imprimirlo. Bajo un locale UTF-8 la salida es exactamente la de R; bajo uno
#'   que no puede representar un carácter difiere a propósito, publicando el
#'   punto de código donde R escaparía los bytes. Sólo cuando R no puede
#'   imprimir se reintenta con su propio octal, `\377`. Eso no convierte a `print()` en infalible: si una
#'   clase suya tiene un método `format()` que da error, ese error le llega sin
#'   alterar. Y un `format()` que consulte `Encoding()` verá las marcas de la
#'   copia, no las del objeto original.
#' @export
#'
#' @seealso [perfilar()], [detectar_dependencias()], [modelo_desde_propuesta()],
#'   [modelo()]
#'
#' @examples
#' datos <- data.frame(
#'   codigo = rep(1:3, each = 4),
#'   categoria = rep(c("A", "B", "C"), each = 4),
#'   valor = c(1:11, NA)
#' )
#' propuesta <- proponer_modelo(perfilar(datos), datos)
#' propuesta[, c("metrica", "origen", "incluir")]
proponer_modelo <- function(perfil, datos = NULL, relaciones = NULL,
                            entidades_relacion = character(),
                            max_valores_dominio = 20L,
                            max_sugerencias = 100L) {
  if (!inherits(perfil, "perfil")) {
    .detener("`perfil` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  if (!is.null(datos) && !inherits(datos, "data.frame")) {
    .detener("`datos` debe ser NULL o heredar de data.frame.", call. = FALSE)
  }
  if (!is.null(datos)) datos <- .tabla_base(datos)
  max_valores_dominio <- .validar_entero_positivo(
    max_valores_dominio, "max_valores_dominio"
  )
  max_sugerencias <- .validar_entero_positivo(max_sugerencias, "max_sugerencias")
  entidad <- perfil$meta$nombre
  if (!.es_texto_escalar(entidad)) entidad <- "datos"
  if (!is.null(datos) && !identical(
    .nombres_para_operar(names(datos)),
    .nombres_para_operar(perfil$columnas$columna)
  )) {
    .detener("Los nombres de `datos` no coinciden con los usados por el perfil.",
         call. = FALSE)
  }
  sugerencias <- list()
  agregar_sugerencia <- function(x) {
    sugerencias[[length(sugerencias) + 1L]] <<- x
  }

  for (i in seq_len(nrow(perfil$columnas))) {
    fila <- perfil$columnas[i, , drop = FALSE]
    columna <- fila$columna[[1L]]
    if (isTRUE(fila$n_faltantes_totales[[1L]] > 0L)) {
      hallazgo <- perfil$hallazgos[
        .nombres_para_operar(perfil$hallazgos$columna) %in%
          .nombres_para_operar(columna) &
          perfil$hallazgos$tipo_hallazgo %in%
            c("faltantes", "faltantes_disfrazados"), , drop = FALSE
      ]
      tipo_origen <- if (any(hallazgo$tipo_hallazgo == "faltantes")) {
        "faltantes"
      } else if (nrow(hallazgo)) {
        "faltantes_disfrazados"
      } else {
        ""
      }
      origen <- if (nzchar(tipo_origen)) {
        paste0("hallazgo:", tipo_origen)
      } else {
        "perfil:n_faltantes_totales"
      }
      n_disfrazados <- fila$n_faltantes_disfrazados[[1L]]
      # Con ausentes disfrazados la `NoNulo` SE MIDE (decision D6, ronda 32;
      # revierte la de la ronda 31, que la dejaba sin medir con estado
      # `faltantes_disfrazados`). El paquete existe para dar la medicion. Primero
      # se normaliza: los valores que el perfil conto como disfrazados van a
      # `valores_nulos`, y la metrica los cuenta como faltantes -0,90 en vez de
      # 0,975 sobre 1 `NA` y 3 `-999`-. Si no se pueden identificar -sin
      # `datos`, no hay de donde sacarlos-, se mide igual con los valores crudos
      # y la justificacion dice que cuenta solo los `NA`.
      disfrazados_presentes <- isTRUE(n_disfrazados > 0L)
      valores_nulos <- if (disfrazados_presentes) {
        .valores_nulos_disfrazados(perfil, datos, columna)
      } else NULL
      # Declarada opcional: se mide sobre las celdas presentes, como el perfil.
      # Ver `.columna_declarada_opcional()`.
      opcional <- .columna_declarada_opcional(perfil, columna)
      nota_opcional <- if (opcional) {
        paste0(
          " La columna se declar\u00f3 opcional: se mide sobre sus ",
          fila$n_aplicables[[1L]], " celdas presentes y sus `NA` no cuentan ",
          "como faltantes."
        )
      } else ""
      agregar_sugerencia(.nueva_sugerencia(
        "alta", TRUE, "NoNulo", entidad, columna, origen,
        if (disfrazados_presentes && length(valores_nulos)) {
          paste0(
            "La columna contiene ", fila$n_faltantes_totales[[1L]],
            " ausentes totales, incluidos ", n_disfrazados,
            " disfrazados. Se normalizan: sus valores se declaran en ",
            "`valores_nulos` y la m\u00e9trica los cuenta como faltantes.",
            nota_opcional
          )
        } else if (disfrazados_presentes) {
          paste0(
            "La columna contiene ", fila$n_faltantes_totales[[1L]],
            " ausentes totales, incluidos ", n_disfrazados,
            if (is.null(datos)) {
              " disfrazados que no se pueden identificar sin los datos."
            } else {
              " disfrazados que no se pudieron identificar en los datos recibidos."
            },
            " Se mide con los valores crudos: cuenta como faltantes s\u00f3lo los ",
            "`NA` (", fila$n_faltantes[[1L]], ") y sobrestima la completitud. ",
            "Para contarlos, declararlos en `valores_nulos`.",
            nota_opcional
          )
        } else {
          paste0(
            "La columna contiene ", fila$n_faltantes[[1L]],
            " valores ausentes; la completitud debe medirse por separado."
          )
        },
        configuracion = c(
          list(),
          if (length(valores_nulos)) list(valores_nulos = valores_nulos),
          if (opcional) list(opcional = TRUE)
        )
      ))
    }
    patrones <- perfil$patrones[[i]]
    if (!is.null(patrones) && nrow(patrones) &&
        is.finite(patrones$proporcion[[1L]]) &&
        patrones$proporcion[[1L]] >= 0.8) {
      patron <- patrones$patron[[1L]]
      agregar_sugerencia(.nueva_sugerencia(
        "media", FALSE, "Formato", entidad, columna,
        paste0("perfil:patron_dominante:", patron),
        paste0(
          "El patr\u00f3n ", patron, " cubre ",
          .formatear_decimal_publicado(patrones$proporcion[[1L]]),
          " de los valores analizados; debe confirmarse como requisito, no s\u00f3lo como costumbre."
        ),
        configuracion = list(expresion_regular = .patron_a_regex(patron))
      ))
    }
    if (!is.null(datos) &&
        fila$tipo_declarado[[1L]] %in% c("texto", "factor", "factor-ordenado") &&
        fila$n_distintos[[1L]] >= 2L &&
        fila$n_distintos[[1L]] <= max_valores_dominio &&
        is.finite(fila$tasa_distintos[[1L]]) && fila$tasa_distintos[[1L]] <= 0.5) {
      indice_columna <- .indice_nombre(columna, names(datos))
      valores_dominio <- .texto_analizable(datos[[indice_columna]])$valores
      valores <- unique(valores_dominio[!is.na(valores_dominio)])
      # El dominio son los valores que el paquete puede LEER: una columna cuyo
      # texto no es UTF-8 valido tiene distintos en el perfil -que cuenta por
      # identidad- y ninguno decodificable, y la metrica propuesta no media
      # nada: `analizar()` avisaba que su metodo fallo, en cualquier locale
      # (ronda 30-L, variante de bytes latin1 sin marca).
      if (length(valores) >= 2L) agregar_sugerencia(.nueva_sugerencia(
        "baja", FALSE, "ValoresPosiblesPorExtension", entidad, columna,
        "perfil:dominio_observado",
        paste0(
          "Se observaron ", length(valores),
          " categor\u00edas; una entrega no demuestra que el dominio est\u00e9 completo."
        ), configuracion = list(valores = valores)
      ))
    }
  }

  duplicados <- perfil$hallazgos$tipo_hallazgo == "filas_duplicadas"
  if (any(duplicados)) {
    agregar_sugerencia(.nueva_sugerencia(
      "alta", TRUE, "EntidadDuplicada", entidad, character(),
      "hallazgo:filas_duplicadas",
      "El perfil encontr\u00f3 filas completas id\u00e9nticas; la m\u00e9trica permite seguir su evoluci\u00f3n."
    ))
  }

  dependencias <- perfil$dependencias
  if (!is.null(dependencias) && nrow(dependencias)) {
    for (i in seq_len(nrow(dependencias))) {
      dependencia <- dependencias[i, , drop = FALSE]
      atributos <- c(dependencia$determinante[[1L]], dependencia$dependiente[[1L]])
      puede_materializar <- !is.null(datos)
      configuracion <- if (puede_materializar) {
        list(regla = .regla_desde_dependencia(datos, atributos[[1L]], atributos[[2L]]))
      } else {
        list()
      }
      agregar_sugerencia(.nueva_sugerencia(
        if (dependencia$exacta[[1L]]) "alta" else "media",
        dependencia$exacta[[1L]] && puede_materializar,
        "ReglaIntegridadIntraEntidad", entidad, atributos,
        .pegar_texto("dependencia_funcional:", atributos[[1L]], "->",
                     atributos[[2L]], sep = ""),
        paste0(
          "La dependencia cumple en ",
          .formatear_decimal_publicado(dependencia$cumplimiento[[1L]], 4L),
          " de ",
          dependencia$n_evaluados[[1L]], " pares presentes."
        ), configuracion = configuracion,
        estado = if (puede_materializar) "lista" else "requiere_datos"
      ))
    }
  }

  if (!is.null(relaciones)) {
    if (!inherits(relaciones, "data.frame") || length(entidades_relacion) != 2L ||
        anyNA(entidades_relacion) || any(!nzchar(entidades_relacion))) {
      .detener(
        "`relaciones` requiere dos nombres en `entidades_relacion`.",
        call. = FALSE
      )
    }
    relaciones <- .tabla_base(relaciones)
    for (i in seq_len(nrow(relaciones))) {
      relacion <- relaciones[i, , drop = FALSE]
      direcciones <- list(
        list(cobertura = relacion$cobertura_tabla2_en_tabla1[[1L]],
             entidades = entidades_relacion,
             atributos = c(relacion$columna_tabla1[[1L]],
                           relacion$columna_tabla2[[1L]])),
        list(cobertura = relacion$cobertura_tabla1_en_tabla2[[1L]],
             entidades = rev(entidades_relacion),
             atributos = c(relacion$columna_tabla2[[1L]],
                           relacion$columna_tabla1[[1L]]))
      )
      for (direccion in direcciones) {
        if (!is.finite(direccion$cobertura) || direccion$cobertura < 0.8) next
        agregar_sugerencia(.nueva_sugerencia(
          if (direccion$cobertura == 1) "alta" else "media",
          direccion$cobertura == 1, "ReglaIntegridadInterEntidad",
          direccion$entidades, direccion$atributos,
          paste0("relacion_detectada:cobertura=",
                 .formatear_decimal_publicado(direccion$cobertura, 4L)),
          "La cobertura observada sugiere una relaci\u00f3n PK/FK que debe confirmarse.",
          configuracion = list(muestra = Inf),
          entidades_ligadas = direccion$entidades,
          atributos_ligados = direccion$atributos
        ))
      }
    }
  }

  resultado <- if (length(sugerencias)) do.call(rbind, sugerencias) else .propuesta_vacia()
  # Un nombre de columna que la tabla repite no identifica una columna, y una
  # metrica se liga por nombre: mediria siempre la primera. Con dos columnas `x`
  # con faltantes, las dos sugerencias `NoNulo` sobre `x` eran el mismo
  # instrumento y `analizar()` abortaba entero (medido en la ronda 30). Se
  # proponen sin activar y con estado `nombre_repetido`: no se materializan
  # hasta renombrar, como el plan de limpieza omite las acciones por columna.
  nombres_perfil <- as.character(perfil$columnas$columna)
  repetidos <- .nombres_para_operar(nombres_perfil)[
    .nombres_repetidos(nombres_perfil)
  ]
  if (length(repetidos) && nrow(resultado)) {
    afectada <- vapply(seq_len(nrow(resultado)), function(i) {
      entidades <- resultado$entidades_ligadas[[i]]
      length(entidades) && all(entidades %in% entidad) &&
        any(.nombres_para_operar(resultado$atributos_ligados[[i]]) %in%
              repetidos)
    }, logical(1L))
    resultado$estado[afectada] <- "nombre_repetido"
    resultado$incluir[afectada] <- FALSE
    resultado$justificacion[afectada] <- paste(
      resultado$justificacion[afectada],
      "La tabla repite el nombre de la columna y una m\u00e9trica se liga por",
      "nombre: renombrarlas -por ejemplo con `make.unique()`- y volver a",
      "proponer."
    )
  }
  # El id sale del contenido y se asigna ANTES de ordenar y de recortar: no
  # depende de la posicion, ni de `max_sugerencias`, ni del orden. Ver
  # `.ids_sugerencias()`.
  resultado$id_sugerencia <- .ids_sugerencias(resultado)
  if (nrow(resultado)) {
    orden_prioridad <- match(resultado$prioridad, c("alta", "media", "baja"))
    # Por los bytes de los nombres (`.orden_por_bytes()`): con la intercalacion
    # del locale la propuesta salia en otro orden bajo `C` que en una sesion
    # UTF-8 (ronda 30-L). La clave de bytes tampoco aborta sobre un nombre
    # marcado `bytes`, que tumbaba `order()` y con el `analizar()` (ronda 27).
    orden <- .orden_por_bytes(
      orden_prioridad, resultado$metrica, resultado$entidad, resultado$atributos
    )
    resultado <- resultado[orden, , drop = FALSE]
    rownames(resultado) <- NULL
  }
  total <- nrow(resultado)
  resultado <- utils::head(resultado, max_sugerencias)
  resultado$prioridad <- factor(
    resultado$prioridad, levels = c("baja", "media", "alta"), ordered = TRUE
  )
  class(resultado) <- c("propuesta_modelo", "data.frame")
  attr(resultado, "total_sugerencias") <- total
  attr(resultado, "truncado") <- total > nrow(resultado)
  resultado
}

#' @export
print.propuesta_modelo <- function(x, ...) {
  .print_data_frame_bytes(x, ...)
  invisible(x)
}

#' Materializar una propuesta de modelo de calidad
#'
#' Convierte las filas con `incluir == TRUE` en métricas instanciadas y las
#' reúne mediante [modelo()]. No vuelve a examinar los datos ni ejecuta una
#' medición.
#'
#' @param propuesta Objeto creado por [proponer_modelo()].
#'
#' @return Objeto `modelo_calidad` con las métricas que la propuesta declaró,
#'   ya instanciadas y reunidas por [modelo()], listo para [medir()]. No
#'   vuelve a examinar los datos ni ejecuta ninguna medición.
#' @export
#'
#' @seealso [proponer_modelo()], [modelo()], [medir()]
#'
#' @examples
#' datos <- data.frame(x = c(1, NA, 3))
#' propuesta <- proponer_modelo(perfilar(datos), datos)
#' modelo_propuesto <- modelo_desde_propuesta(propuesta)
#' medir(modelo_propuesto, datos)
modelo_desde_propuesta <- function(propuesta) {
  requeridas <- c(
    "id_sugerencia", "incluir", "metrica", "configuracion",
    "entidades_ligadas", "atributos_ligados", "referencial", "estado"
  )
  if (!inherits(propuesta, "data.frame") || !all(requeridas %in% names(propuesta)) ||
      !is.logical(propuesta$incluir) || anyNA(propuesta$incluir)) {
    .detener("`propuesta` no cumple el contrato de una propuesta de modelo.",
         call. = FALSE)
  }
  seleccion <- which(propuesta$incluir)
  if (!length(seleccion)) {
    .detener("La propuesta no contiene sugerencias activas.", call. = FALSE)
  }
  if (any(propuesta$estado[seleccion] != "lista")) {
    .detener("S\u00f3lo se pueden materializar sugerencias con estado 'lista'.",
         call. = FALSE)
  }
  nucleo <- metricas_nucleo()
  instancias <- lapply(seleccion, function(i) {
    nombre <- propuesta$metrica[[i]]
    indice_nucleo <- .indice_identificador(nombre, names(nucleo))
    if (is.na(indice_nucleo)) {
      .detener("No existe una m\u00e9trica materializable llamada '", nombre, "'.",
           call. = FALSE)
    }
    configuracion <- propuesta$configuracion[[i]]
    especifica <- do.call(
      especializar,
      c(list(metrica = nucleo[[indice_nucleo]],
             nombre_especifico = paste0(nombre, "Propuesto")), configuracion)
    )
    instanciar(
      especifica,
      entidad = propuesta$entidades_ligadas[[i]],
      atributos = propuesta$atributos_ligados[[i]],
      nombre_instancia = propuesta$id_sugerencia[[i]],
      referencial = propuesta$referencial[[i]]
    )
  })
  modelo(instancias)
}

#' @export
`[.propuesta_modelo` <- function(x, ...) {
  resultado <- NextMethod("[")
  if (inherits(resultado, "data.frame") &&
      all(c("id_sugerencia", "incluir", "configuracion") %in% names(resultado))) {
    class(resultado) <- unique(c("propuesta_modelo", class(resultado)))
  } else if (inherits(resultado, "data.frame")) {
    class(resultado) <- setdiff(class(resultado), "propuesta_modelo")
  }
  resultado
}
