#' Perfiles de normalizacion para comparar valores
#'
#' La normalizacion afecta unicamente la representacion usada para comparar;
#' nunca modifica los datos de entrada. La descomposicion canonica y el orden
#' de sus marcas son siempre activos para que NFC y NFD sean equivalentes en el
#' subconjunto latino cubierto por lupa: las letras latinas precompuestas, las
#' marcas combinantes de U+0300 a U+0332 ordenadas por su clase combinante
#' canónica, y los signos Kelvin y Ångström, que Unicode declara iguales a la
#' `K` y a la `Å`. Las tablas se generan desde Unicode
#' (`data-raw/normalizacion_unicode.R`). Los pasos optativos de ligaduras y ancho
#' completo tambien se aplican por punto de codigo, despues de declarar como
#' UTF-8 los bytes validos, por lo que no dependen de `LC_CTYPE`. Bajar a
#' minusculas tampoco depende del locale: usa un mapa explicito para el latin
#' acentuado, el griego, el cirilico, el armenio y el ancho completo, y lo que
#' queda fuera de ese mapa se conserva. Para comparar, la sigma final griega se
#' iguala a la sigma, como en el plegado de caja de Unicode: una palabra griega
#' en mayúsculas y la misma en minúsculas, escrita con su sigma final, son el
#' mismo valor.
#'
#' El paso `espacios` lleva primero al espacio común la tabulación, el salto de
#' línea, el retorno de carro, el avance de página, la tabulación vertical y los
#' espacios Unicode, y borra los controles de transporte; al final colapsa y
#' recorta los blancos. Como va antes que `comillas`, una comilla junto a una
#' tabulación o un salto de línea se trata igual que junto a un espacio.
#'
#' El paso `comillas` quita una comilla de apertura cuando abre una palabra —la
#' precede un blanco o el inicio y la sigue una letra— y una de cierre o un
#' apóstrofo cuando la cierra —la precede una letra y la sigue un blanco o el
#' final—; los apóstrofos tipográficos que quedan se escriben `'`. La comilla
#' simple ASCII cierra pero no abre, porque también es el apóstrofo de una
#' elisión: `'Piedras'` se compara como `'piedras`, mientras que la misma
#' palabra entre comillas simples tipográficas se compara como `piedras`.
#'
#' @param minusculas,espacios,acentos,comillas,puntuacion,ligaduras,ancho
#'   Activan el paso correspondiente.
#' @param proteger Grafemas cuyas marcas deben conservarse al quitar acentos.
#'   Puede incluir una base seguida de una o más marcas combinantes, como
#'   la secuencia `g` seguida por una tilde combinante para la letra guaraní.
#'   Se declaran en minúscula y valen también en mayúscula: la eñe de `PEÑA` se
#'   conserva igual que la de `peña`.
#' @return Objeto de clase `normalizacion_lupa`: una lista con un elemento
#'   logico por paso -`minusculas`, `espacios`, `acentos`, `comillas`,
#'   `puntuacion`, `ligaduras`, `ancho`- y el vector `proteger` con los
#'   grafemas cuyas marcas se conservan. Es una declaracion de como comparar:
#'   no toca los datos, y la descomposicion canonica se aplica siempre.
#' @export
#' @examples
#' # El perfil por omisión: minúsculas, espacios, acentos y comillas.
#' normalizacion()
#'
#' # Comparar sin quitar acentos, para que "papa" y "papá" no se fusionen. La
#' # eñe y la diéresis ya están protegidas en el perfil por omisión: "pena" y
#' # "peña" no se fusionan nunca.
#' perfil <- normalizacion(acentos = FALSE)
#' perfil$acentos
#'
#' # La normalización sólo afecta la representación usada para COMPARAR; no
#' # modifica los datos ni los conteos. Estas tres formas siguen siendo tres
#' # valores distintos, y así se informan: lo que la normalización habilita es
#' # que se reconozcan como variantes de la misma cosa al buscar duplicados.
#' datos <- data.frame(ciudad = c("Montevideo", "MONTEVIDEO", "montevideo"))
#' salida <- perfilar(datos, normalizar = normalizacion())
#' salida$columnas$n_distintos  # 3, no 1
normalizacion <- function(minusculas = TRUE, espacios = TRUE, acentos = TRUE,
                          comillas = TRUE, puntuacion = FALSE,
                          ligaduras = FALSE, ancho = FALSE,
                          proteger = c("\u00f1", "\u00fc",
                                       intToUtf8(c(103L, 771L)))) {
  valores <- list(
    minusculas = minusculas, espacios = espacios, acentos = acentos,
    comillas = comillas, puntuacion = puntuacion, ligaduras = ligaduras,
    ancho = ancho
  )
  if (any(!vapply(valores, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1L)))) {
    .detener("Los pasos de normalizacion() deben ser logicos escalares.", call. = FALSE)
  }
  if (!is.character(proteger) || anyNA(proteger) || any(!nzchar(proteger))) {
    .detener("proteger debe ser un vector de caracteres no vacio ni NA.", call. = FALSE)
  }
  if (any(vapply(proteger, function(x) {
    codigos <- .normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    )
    !length(codigos) || .normalizacion_clase(codigos[[1L]]) != 0L ||
      any(vapply(codigos[-1L], .normalizacion_clase, integer(1L)) == 0L)
  }, logical(1L)))) {
    .detener("Cada elemento de proteger debe ser un grafema Unicode valido.",
         call. = FALSE)
  }
  structure(c(valores, list(proteger = unique(proteger))),
            class = c("normalizacion_lupa", "list"))
}

#' @export
print.normalizacion_lupa <- function(x, ...) {
  .validar_objeto_lupa(
    x, "normalizacion_lupa",
    c("minusculas", "espacios", "acentos", "comillas", "puntuacion",
      "ligaduras", "ancho"),
    "normalizacion()"
  )
  cat("Perfil de normalizacion de lupa\n")
  pasos <- c("minusculas", "espacios", "acentos", "comillas", "puntuacion",
             "ligaduras", "ancho")
  .cat_publicado(
    paste0(
      "  ", pasos, " = ", vapply(x[pasos], as.character, character(1L)),
      collapse = "\n"
    ), "\n"
  )
  .cat_publicado(
    "  proteger = ", paste(x$proteger, collapse = ", "), "\n"
  )
  invisible(x)
}
.normalizacion_preset <- function(nombre) {
  if (identical(nombre, "amplio")) {
    return(normalizacion(puntuacion = TRUE, ligaduras = TRUE, ancho = TRUE))
  }
  .detener("El preset de normalizar debe ser amplio.", call. = FALSE)
}
.normalizacion_resuelta <- function(general, por_columna = list()) {
  estructura <- list(general = general, por_columna = por_columna)
  class(estructura) <- c("normalizacion_resuelta_lupa", "list")
  estructura
}
.resolver_normalizacion <- function(normalizar = TRUE, perfil = NULL) {
  if (is.null(normalizar)) {
    heredada <- if (!is.null(perfil) && inherits(perfil, "perfil")) {
      perfil$meta$normalizacion
    } else NULL
    normalizar <- if (is.null(heredada)) TRUE else heredada
  }
  if (inherits(normalizar, "normalizacion_resuelta_lupa")) return(normalizar)
  if (inherits(normalizar, "normalizacion_lupa")) {
    return(.normalizacion_resuelta(normalizar))
  }
  if (is.logical(normalizar) && length(normalizar) == 1L && !is.na(normalizar)) {
    return(.normalizacion_resuelta(normalizacion(
      minusculas = normalizar, espacios = normalizar, acentos = normalizar,
      comillas = normalizar
    )))
  }
  if (is.logical(normalizar)) {
    .detener("normalizar debe ser un vector de l\u00f3gicos escalares sin NA.",
         call. = FALSE)
  }
  if (is.character(normalizar) && length(normalizar) == 1L && !is.na(normalizar)) {
    return(.normalizacion_resuelta(.normalizacion_preset(normalizar)))
  }
  if (is.list(normalizar)) {
    nombres <- names(normalizar)
    if (is.null(nombres) || anyNA(nombres) || any(!nzchar(nombres)) ||
        anyDuplicated(.nombres_para_operar(nombres))) {
      .detener("La lista de normalizar debe tener nombres de columnas unicos.", call. = FALSE)
    }
    perfiles <- lapply(normalizar, function(x) {
      if (is.character(x) && length(x) == 1L) x <- .normalizacion_preset(x)
      if (is.logical(x) && length(x) == 1L && !is.na(x)) {
        x <- normalizacion(minusculas = x, espacios = x, acentos = x,
                           comillas = x)
      }
      if (!inherits(x, "normalizacion_lupa")) {
        .detener("Cada valor de la lista normalizar debe ser un perfil valido.",
             call. = FALSE)
      }
      x
    })
    return(.normalizacion_resuelta(normalizacion(), perfiles))
  }
  .detener("normalizar debe ser TRUE, FALSE, amplio, un perfil o una lista nombrada.",
       call. = FALSE)
}
.normalizacion_para_columna <- function(resuelta, columna) {
  if (!inherits(resuelta, "normalizacion_resuelta_lupa")) {
    resuelta <- .resolver_normalizacion(resuelta)
  }
  indice <- if (length(resuelta$por_columna)) {
    .indice_nombre(columna, names(resuelta$por_columna))
  } else NA_integer_
  if (!is.na(indice)) {
    resuelta$por_columna[[indice]]
  } else resuelta$general
}
.normalizacion_resumen <- function(resuelta) {
  if (!inherits(resuelta, "normalizacion_resuelta_lupa")) {
    resuelta <- .resolver_normalizacion(resuelta)
  }
  list(general = unclass(resuelta$general),
       por_columna = lapply(resuelta$por_columna, unclass))
}

.normalizacion_pasos_configurables <- c(
  "espacios", "ancho", "ligaduras", "comillas", "puntuacion",
  "acentos", "minusculas"
)
.normalizacion_tiene_pasos <- function(perfil) {
  any(vapply(perfil[.normalizacion_pasos_configurables], isTRUE, logical(1L)))
}
.normalizacion_tiene_pasos_resuelta <- function(resuelta, columnas = NULL) {
  if (!inherits(resuelta, "normalizacion_resuelta_lupa")) {
    resuelta <- .resolver_normalizacion(resuelta)
  }
  perfiles <- list(resuelta$general)
  if (length(resuelta$por_columna)) {
    if (is.null(columnas)) {
      perfiles <- c(perfiles, unname(resuelta$por_columna))
    } else {
      perfiles <- lapply(columnas, function(columna) {
        .normalizacion_para_columna(resuelta, columna)
      })
    }
  }
  any(vapply(perfiles, .normalizacion_tiene_pasos, logical(1L)))
}
.normalizacion_salida <- function(resuelta, fusiones = NULL) {
  salida <- .normalizacion_resumen(resuelta)
  if (!is.null(fusiones)) salida$fusiones <- fusiones
  salida
}

# `.normalizacion_decomposiciones` y `.normalizacion_clases_combinantes` viven
# en `R/normalizacion-unicode.R`, generado desde Unicode por
# `data-raw/normalizacion_unicode.R`. La tabla de clases estaba escrita a mano y
# 25 de sus 51 valores no eran los de Unicode: el orden canonico de las marcas
# salia mal y NFC y NFD del mismo texto no se igualaban con `acentos = FALSE`.
.normalizacion_descomponer <- function(codigos) {
  expandir <- function(codigo) {
    valor <- .normalizacion_decomposiciones[[as.character(codigo)]]
    if (is.null(valor)) return(as.integer(codigo))
    unlist(lapply(valor, expandir), use.names = FALSE)
  }
  unlist(lapply(codigos, expandir), use.names = FALSE)
}
.normalizacion_clase <- function(codigo) {
  valor <- unname(.normalizacion_clases_combinantes[as.character(codigo)])
  if (!length(valor) || is.na(valor)) 0L else as.integer(valor)
}
# El orden canonico solo mueve marcas: cada tramo maximo de marcas combinantes
# -clase distinta de 0- se ordena por clase, de forma estable, y la base que lo
# precede queda en su lugar. La salida se armaba creciendo un vector con
# `salida <- c(salida, ...)` en cada vuelta del `while`, una vuelta por caracter:
# cuadratico en el largo. `detectar_discordancias()`, que normaliza la celda
# entera, tardaba 1,7 / 4,6 / 15,4 s con celdas de 25.000 / 50.000 / 100.000
# caracteres. Medido. Ahora se reordena en su lugar, tramo por
# tramo, sobre una copia: la misma salida, en tiempo lineal.
.normalizacion_ordenar <- function(codigos) {
  if (length(codigos) < 2L) return(codigos)
  clases <- vapply(codigos, .normalizacion_clase, integer(1L))
  salida <- codigos
  tramos <- rle(clases != 0L)
  fines <- cumsum(tramos$lengths)
  inicios <- fines - tramos$lengths + 1L
  for (k in which(tramos$values & tramos$lengths > 1L)) {
    bloque <- inicios[[k]]:fines[[k]]
    salida[bloque] <- codigos[bloque][
      .orden_por_bytes(clases[bloque], seq_along(bloque), method = "radix")
    ]
  }
  salida
}
# Lo protegido se declara en minuscula -`n` con tilde, `u` con dieresis- y se
# comparaba tal cual contra el texto, ANTES de bajar la caja: la mayuscula no
# casaba y perdia la marca. "AÑO" daba "ano" mientras "año" daba "año", asi que
# `CAÑADA` no encontraba a `Cañada` en el referencial y `PEÑA` se juntaba con
# `Pena` en vez de con `Peña`. Se agrega la variante mayuscula de cada grafema
# cuya base es una letra ASCII, sin `toupper()`, que depende del locale.
.proteger_en_ambas_cajas <- function(proteger) {
  if (!length(proteger)) return(proteger)
  variantes <- vapply(proteger, function(x) {
    codigos <- .normalizacion_ordenar(.normalizacion_descomponer(utf8ToInt(x)))
    base <- codigos[[1L]]
    if (base >= 97L && base <= 122L) {
      codigos[[1L]] <- base - 32L
      .normalizacion_a_texto(codigos)
    } else NA_character_
  }, character(1L), USE.NAMES = FALSE)
  unique(c(proteger, variantes[!is.na(variantes)]))
}

.normalizacion_protecciones <- function(perfil) {
  lapply(.proteger_en_ambas_cajas(perfil$proteger), function(x) {
    codigos <- .normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    )
    list(base = codigos[[1L]], marcas = codigos[-1L])
  })
}
# La misma construccion que `.normalizacion_ordenar()` -crecer la salida con
# `c()` en un `while`, una vuelta por caracter- y el mismo costo cuadratico:
# 0,8 / 1,9 / 5,9 s con 25.000 / 50.000 / 100.000 caracteres, en el perfil de
# normalizacion por omision. Medido. La regla no cambia: toda base
# se conserva; un tramo de marcas se conserva solo si, con la base que lo
# precede, es un grafema protegido; las marcas sin base se descartan. Se decide
# por tramo y se toma de una vez.
.normalizacion_quitar_acentos <- function(codigos, perfil, protecciones = NULL) {
  if (is.null(protecciones)) protecciones <- .normalizacion_protecciones(perfil)
  if (!length(codigos)) return(integer())
  clases <- vapply(codigos, .normalizacion_clase, integer(1L))
  conservar <- clases == 0L
  tramos <- rle(!conservar)
  fines <- cumsum(tramos$lengths)
  inicios <- fines - tramos$lengths + 1L
  for (k in which(tramos$values & inicios > 1L)) {
    tramo <- inicios[[k]]:fines[[k]]
    base <- codigos[[inicios[[k]] - 1L]]
    marcas <- codigos[tramo]
    protegido <- any(vapply(protecciones, function(p) {
      identical(as.integer(p$base), as.integer(base)) &&
        identical(as.integer(p$marcas), as.integer(marcas))
    }, logical(1L)))
    if (protegido) conservar[tramo] <- TRUE
  }
  codigos[conservar]
}
# La regla de comillas existe UNA vez, en `.normalizacion_comillas_vector()`, y el
# recorrido escalar la usa sobre el texto de sus puntos de codigo. Estaba escrita
# dos veces -un bucle aca y dos expresiones alla- y las dos versiones no
# coincidian: la expresion casaba `$` antes de un salto de linea final y su
# pasada de aperturas borraba la comilla que la de cierres despues no veia. Como
# un valor con dos o mas marcas combinantes va por el camino escalar, `Pena"`
# con salto final se igualaba a `Pena"` y `Nunoa"` con salto final no -lo mismo
# al reves con una apertura y un cierre tipograficos sueltos-: la clave dependia
# de cuantas enies tenia la palabra.
.normalizacion_comillas <- function(codigos) {
  if (!length(codigos)) return(codigos)
  utf8ToInt(.normalizacion_comillas_vector(.normalizacion_a_texto(codigos)))
}
.normalizacion_ancho <- function(codigos) {
  vapply(codigos, function(x) {
    if (x >= 0xFF01 && x <= 0xFF5E) x - 0xFEE0L else
      if (x == 0x3000) 32L else x
  }, integer(1L))
}
.normalizacion_ligaduras <- function(codigos) {
  mapa <- list("64256" = c(102L, 102L), "64257" = c(102L, 105L),
               "64258" = c(102L, 108L), "64259" = c(102L, 102L, 105L),
               "64260" = c(102L, 102L, 108L), "64261" = c(115L, 116L),
               "64262" = c(115L, 116L))
  unlist(lapply(codigos, function(x) {
    valor <- mapa[[as.character(x)]]
    if (is.null(valor)) x else valor
  }), use.names = FALSE)
}
.normalizacion_puntuacion <- function(codigos) {
  quitar <- vapply(codigos, function(x) {
    (x >= 33L && x <= 47L) || (x >= 58L && x <= 64L) ||
      (x >= 91L && x <= 96L) || (x >= 123L && x <= 126L) ||
      (x >= 0x2000 && x <= 0x206F) || (x >= 0x3001 && x <= 0x303F)
  }, logical(1L))
  codigos[!quitar]
}
# Bajar caja para COMPARAR. Usa el mapa de minusculas del paquete y ademas
# pliega la sigma final (U+03C2) a la sigma (U+03C3), como el plegado de caja de
# Unicode y como ya lo hace `.plegar_para_comparar()`: el mapa baja la mayuscula
# U+03A3 siempre a U+03C3, asi que `TREIS` en mayusculas y `treis` escrito con
# su sigma final no se igualaban. El pliegue va aca y no en el mapa, porque el
# mapa tambien ESCRIBE -`convertir_minusculas` de `aplicar()`- y ahi una sigma
# final bien escrita no se toca. Es la unica funcion del paso: el camino
# vectorial y el escalar la llaman los dos.
.normalizacion_minusculas <- function(texto) {
  chartr("\u03c2", "\u03c3", .normalizacion_minusculas_vector(texto))
}

# El texto del usuario en la forma con que se lo COMPARA contra un vocabulario
# -los literales logicos de la inferencia y de `aplicar()`, los marcadores de
# ausencia del perfil, del plan y de la proteccion-: declarado, recortado y en
# minusculas con el mapa del paquete. Con `tolower()` de base, bajo `C` la I con
# tilde no bajaba: una columna `SI`/`NO` con tilde era `logico` en una sesion
# UTF-8 y `texto` en un `Rscript` de cron, y `SIN INFORMACION` con tilde no se
# contaba como faltante disfrazado. Como es para comparar, pliega
# tambien la sigma final; los dos lados de la comparacion pasan por aca.
#
# La declaracion va antes del recorte (`.textos_para_plegar()`), y el pliegue se
# hace una vez por valor distinto: los detectores lo llaman sobre la columna
# entera.
.minusculas_para_comparar <- function(textos, recortar = TRUE) {
  textos <- .textos_para_plegar(as.character(textos))
  if (!length(textos)) return(textos)
  unicos <- unique(textos)
  plegados <- if (recortar) trimws(unicos) else unicos
  plegados <- .normalizacion_minusculas(plegados)
  plegados[match(textos, unicos)]
}
# Los blancos que el primer paso de `espacios` lleva al espacio ASCII antes de
# que comillas y puntuacion vean el texto: los separadores ASCII -tabulacion,
# salto de linea, tabulacion vertical, avance de pagina y retorno, 9 a 13- y los
# espacios Unicode. Sin los ASCII, `"Juan"` seguido de una tabulacion conservaba
# su comilla -la regla de comillas exige un espacio o el borde- y no se igualaba
# con el mismo valor seguido de un espacio, aunque el paso promete colapsar los
# blancos y el paso final los colapsaba igual. Una sola lista para los dos
# caminos.
.normalizacion_codigos_blancos <- function() {
  c(.codigos_salto_linea_set, .codigos_espacios_invisibles)
}
.normalizacion_espacios_codigos <- function(codigos) {
  if (!length(codigos)) return(codigos)
  codigos[codigos %in% .normalizacion_codigos_blancos()] <- 32L
  codigos[!(codigos %in% .codigos_control_eliminable_set)]
}
.normalizacion_a_texto <- function(codigos) {
  paste0(intToUtf8(codigos, multiple = TRUE), collapse = "")
}
.normalizacion_regex_codigos <- function(codigos) {
  if (!length(codigos)) return("(?!)")
  paste0("(*UTF)[",
         paste0("\\x{", sprintf("%04X", as.integer(codigos)), "}",
                collapse = ""), "]")
}

# La tabla es pequeña, pero construir la alternancia y sus reemplazos para
# cada llamada era una parte apreciable del costo del perfil de fusiones. Se
# guarda una sola vez y se aplica con gregexpr/regmatches sobre el vector
# completo. Los reemplazos pueden tener distinta longitud (por ejemplo, una
# letra vietnamita se descompone en tres puntos de código), por eso chartr no
# alcanza para esta tabla.
.normalizacion_tabla_vectorizada <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      codigos <- as.integer(names(.normalizacion_decomposiciones))
      caracteres <- intToUtf8(codigos, multiple = TRUE)
      reemplazos <- vapply(
        codigos,
        function(codigo) .normalizacion_a_texto(
          .normalizacion_ordenar(.normalizacion_descomponer(codigo))
        ),
        character(1L)
      )
      # El mismo alfabeto escrito dos veces a proposito: una vez en codepoints
      # -para `grepl()`, que decide si hace falta trabajar- y otra en bytes,
      # para extraer y reensamblar.
      #
      # Por que en bytes: `regmatches()` indexa por CARACTER, y en una cadena
      # UTF-8 eso obliga a recorrerla desde el byte cero en cada coincidencia.
      # Medido sobre 80.000 bytes con una coincidencia cada dos: 9,609 s en
      # UTF-8 contra 0,001 s en ASCII y 0,001 s con `useBytes = TRUE`. El costo
      # no estaba en buscar ni en reemplazar, sino en contar caracteres.
      #
      # Es equivalente porque cada clave es un unico codepoint: en UTF-8 ninguna
      # secuencia valida es prefijo de otra ni empieza en medio de otra -las
      # continuaciones son 10xxxxxx-, asi que la alternancia en bytes encuentra
      # exactamente las mismas coincidencias.
      en_bytes <- vapply(caracteres, function(caracter) {
        paste0("\\x", .mayusculas_ascii(as.character(charToRaw(caracter))), collapse = "")
      }, character(1L), USE.NAMES = FALSE)
      cache <<- list(
        patron = paste0("(*UTF)(?:", paste0(
          "\\x{", sprintf("%04X", codigos), "}", collapse = "|"
        ), ")"),
        patron_bytes = paste0("(?:", paste0(en_bytes, collapse = "|"), ")"),
        reemplazos = stats::setNames(reemplazos, caracteres)
      )
    }
    cache
  }
})

.normalizacion_reemplazar_tabla <- function(textos) {
  tabla <- .normalizacion_tabla_vectorizada()
  aciertos <- !is.na(textos) & grepl(tabla$patron, textos, perl = TRUE)
  if (!any(aciertos)) return(textos)
  seleccion <- which(aciertos)
  afectados <- textos[seleccion]
  coincidencias <- gregexpr(
    tabla$patron_bytes, afectados, perl = TRUE, useBytes = TRUE
  )
  encontrados <- regmatches(afectados, coincidencias)
  reemplazos <- lapply(encontrados, function(x) {
    if (!length(x)) return(character())
    # Lo extraido vuelve sin marca de codificacion; el indice de reemplazos
    # esta indexado por el caracter UTF-8, asi que hay que devolverle la marca
    # antes de buscar. Sin esto la busqueda no encuentra y devuelve NA.
    Encoding(x) <- "UTF-8"
    unname(tabla$reemplazos[x])
  })
  regmatches(afectados, coincidencias) <- reemplazos
  # El reensamblado por bytes deja la cadena sin marca: se le repone, porque el
  # resto del paquete cuenta caracteres y no bytes.
  Encoding(afectados) <- "UTF-8"
  textos[seleccion] <- afectados
  textos
}

.normalizacion_proteger_vector <- function(textos, perfil) {
  if (!isTRUE(perfil$acentos) || !length(perfil$proteger)) {
    return(list(textos = textos, marcadores = character(),
                colision = rep(FALSE, length(textos))))
  }
  grafemas <- vapply(.proteger_en_ambas_cajas(perfil$proteger), function(x) {
    .normalizacion_a_texto(.normalizacion_ordenar(
      .normalizacion_descomponer(utf8ToInt(x))
    ))
  }, character(1L))
  orden <- .orden_por_bytes(nchar(grafemas, type = "chars"), decreasing = TRUE,
                 method = "radix")
  grafemas <- grafemas[orden]
  marcadores <- intToUtf8(0xF0000L + seq_along(grafemas), multiple = TRUE)
  colision <- rep(FALSE, length(textos))
  for (marcador in marcadores) {
    colision <- colision | (!is.na(textos) &
      grepl(marcador, textos, fixed = TRUE, useBytes = FALSE))
  }
  for (i in seq_along(grafemas)) {
    # No proteger un prefijo de un grafema con marcas adicionales: por
    # ejemplo, `ü` no debe conservarse dentro de `ǘ` (u + diéresis + agudo),
    # que el camino escalar considera un grafema distinto.
    patron <- paste0(
      "(*UTF)",
      paste0("\\x{", sprintf("%04X", utf8ToInt(grafemas[[i]])), "}",
             collapse = ""),
      "(?![\\x{0300}-\\x{0332}])"
    )
    textos <- gsub(patron, marcadores[[i]], textos, perl = TRUE)
  }
  list(textos = textos, marcadores = stats::setNames(marcadores, grafemas),
       colision = colision)
}

.normalizacion_ancho_vector <- local({
  origen <- paste0(intToUtf8(0xFF01:0xFF5E, multiple = TRUE),
                   collapse = "")
  destino <- paste0(intToUtf8(0x0021:0x007E, multiple = TRUE),
                    collapse = "")
  function(textos) {
    chartr(origen, destino, gsub("\u3000", " ", textos, fixed = TRUE))
  }
})

.normalizacion_ligaduras_vector <- function(textos) {
  nombres <- names(textos)
  textos <- .textos_para_plegar(textos)
  salida <- vapply(textos, function(texto) {
    if (is.na(texto)) return(NA_character_)
    .normalizacion_a_texto(.normalizacion_ligaduras(utf8ToInt(texto)))
  }, character(1L), USE.NAMES = FALSE)
  names(salida) <- nombres
  salida
}

.normalizacion_puntuacion_vector <- function(textos) {
  patron <- paste0(
    "(*UTF)[",
    "\\x{0021}-\\x{002F}",
    "\\x{003A}-\\x{0040}",
    "\\x{005B}-\\x{0060}",
    "\\x{007B}-\\x{007E}",
    "\\x{2000}-\\x{206F}",
    "\\x{3001}-\\x{303F}]"
  )
  gsub(patron, "", textos, perl = TRUE)
}

.normalizacion_comillas_vector <- function(textos) {
  letra <- "[A-Za-z\\x{0080}-\\x{10FFFF}]"
  # Una comilla de apertura se quita cuando abre una palabra: la precede un
  # espacio o el inicio -`(?<![^ ])`- y la sigue una letra. Una de cierre o un
  # apostrofo, cuando la cierra: la precede una letra y la sigue un espacio o el
  # FINAL -`(?![^ ])`, no `$`, que en PCRE casa tambien antes de un salto de
  # linea final-. La comilla simple ASCII cierra pero no abre: es tambien el
  # apostrofo de una elision.
  #
  # Todo en UNA sola pasada, que decide sobre el texto original. En dos, la
  # segunda veia lo que la primera ya habia borrado: en "abc" seguido de una
  # comilla simple y una doble de cierre, la primera quitaba la doble y la
  # segunda, ahora al final, la simple; y una apertura borrada dejaba sin letra
  # a la izquierda al cierre que la seguia. Es la unica escritura de la regla:
  # `.normalizacion_comillas()` la usa para el camino escalar.
  apertura <- paste0("[\"\\x{00AB}\\x{2018}\\x{201B}\\x{201C}",
                     "\\x{201E}\\x{2039}]")
  cierre <- paste0("[\"\\x{00BB}\\x{2019}\\x{201D}\\x{201F}",
                   "\\x{203A}\\x{0027}\\x{201A}]")
  textos <- gsub(
    paste0("(*UTF)(?:(?<![^ ])", apertura, "(?=", letra, ")|(?<=", letra, ")",
           cierre, "(?![^ ]))"),
    "", textos, perl = TRUE
  )
  gsub("[\u2019\u201A\u201B]", "'", textos, fixed = FALSE)
}

# La normalizacion por omision esta pensada para texto humano, pero los
# vocabularios de codigos suelen ser ASCII. En ese universo, acentos,
# descomposicion canonica, ancho y ligaduras no pueden hacer nada. Esta guarda
# ademas descarta el caso en que alguno de los pasos configurables si puede
# cambiar un byte ASCII; los falsos positivos solo conservan la ruta completa.
.normalizacion_ascii_sin_cambios <- function(textos, perfil) {
  if (!.es_ascii(textos)) return(FALSE)
  if (isTRUE(perfil$minusculas) && any(grepl(
    "[A-Z]", textos, useBytes = TRUE, perl = TRUE
  ))) return(FALSE)
  if (isTRUE(perfil$espacios) && any(grepl(
    "[[:space:]]|[\\x00-\\x08\\x0E-\\x1F\\x7F]",
    textos, useBytes = TRUE, perl = TRUE
  ))) return(FALSE)
  if (isTRUE(perfil$comillas) && any(grepl(
    "[\"']", textos, useBytes = TRUE, perl = TRUE
  ))) return(FALSE)
  if (isTRUE(perfil$puntuacion) && any(grepl(
    "[[:punct:]]", textos, useBytes = TRUE, perl = TRUE
  ))) return(FALSE)
  TRUE
}

.normalizacion_vector_rapida <- function(textos, perfil) {
  original <- .textos_para_plegar(as.character(textos))
  nombres <- names(original)
  salida <- original
  names(salida) <- nombres
  no_na <- !is.na(salida)
  if (!any(no_na)) return(salida)
  if (.normalizacion_ascii_sin_cambios(salida, perfil)) return(salida)

  # Las sustituciones de tablas, puntuación, ancho y comillas se aplican sobre
  # el vector completo. Sólo las cadenas con marcas combinantes múltiples o
  # colisiones de marcadores necesitan el ordenamiento escalar al final.
  lento <- rep(FALSE, length(salida))
  if (isTRUE(perfil$espacios)) {
    # Este es el primer paso del contrato escalar: un espacio Unicode se
    # vuelve espacio ASCII antes de que comillas y puntuación vean el texto.
    salida <- gsub(.normalizacion_regex_codigos(.normalizacion_codigos_blancos()),
                   " ", salida, perl = TRUE)
    salida <- gsub(.normalizacion_regex_codigos(
      .codigos_control_eliminable_set
    ), "", salida, perl = TRUE)
  }
  if (isTRUE(perfil$ancho)) {
    salida <- .normalizacion_ancho_vector(salida)
  }
  if (isTRUE(perfil$ligaduras)) {
    salida <- .normalizacion_ligaduras_vector(salida)
  }
  if (isTRUE(perfil$comillas)) {
    necesita_comillas <- !is.na(salida) & grepl(
      "(*UTF)[\\\"'\\x{00AB}\\x{00BB}\\x{2018}-\\x{201F}\\x{2039}-\\x{203A}]",
      salida, perl = TRUE
    )
    if (any(necesita_comillas)) {
      salida[necesita_comillas] <- .normalizacion_comillas_vector(
        salida[necesita_comillas]
      )
    }
  }
  if (isTRUE(perfil$puntuacion)) {
    salida <- .normalizacion_puntuacion_vector(salida)
  }

  salida <- .normalizacion_reemplazar_tabla(salida)
  if (isTRUE(perfil$acentos)) {
    protegidos <- .normalizacion_proteger_vector(salida, perfil)
    salida <- protegidos$textos
    lento <- lento | protegidos$colision
  } else {
    protegidos <- list(marcadores = character())
  }

  if (isTRUE(perfil$acentos)) {
    salida <- gsub("(*UTF)[\\x{0300}-\\x{0332}]", "", salida,
                   perl = TRUE)
  }
  if (length(protegidos$marcadores)) {
    for (grafema in names(protegidos$marcadores)) {
      salida <- gsub(unname(protegidos$marcadores[[grafema]]), grafema,
                     salida, fixed = TRUE, useBytes = FALSE)
    }
  }
  if (isTRUE(perfil$minusculas)) {
    salida <- .normalizacion_minusculas(salida)
  }
  if (isTRUE(perfil$espacios)) {
    salida <- trimws(gsub("[[:space:]]+", " ", salida, perl = TRUE))
  }

  # Dos o más marcas requieren el orden canónico estable; las comillas y los
  # pliegues optativos requieren el contexto de la implementación escalar.
  marcas <- "[\\x{0300}-\\x{0332}]"
  complejas <- !is.na(salida) & grepl(
    paste0("(*UTF)", marcas, ".*", marcas), salida, perl = TRUE
  )
  lento <- lento | complejas
  if (any(lento & no_na)) {
    indices <- which(lento & no_na)
    salida[indices] <- vapply(
      original[indices], .normalizacion_uno, character(1L),
      perfil = perfil, protecciones = NULL, USE.NAMES = FALSE
    )
  }
  salida
}
.normalizacion_etapas <- function(texto, perfil, protecciones = NULL) {
  if (is.na(texto)) return(stats::setNames(NA_character_, "entrada"))
  codigos <- utf8ToInt(as.character(texto))
  etapas <- list(entrada = .normalizacion_a_texto(codigos))
  agregar <- function(nombre, nuevos) {
    etapas[[nombre]] <<- .normalizacion_a_texto(nuevos)
    codigos <<- nuevos
  }
  if (isTRUE(perfil$espacios)) {
    agregar("espacios", .normalizacion_espacios_codigos(codigos))
  }
  if (isTRUE(perfil$ancho)) {
    agregar("ancho", .normalizacion_ancho(codigos))
  }
  if (isTRUE(perfil$ligaduras)) {
    agregar("ligaduras", .normalizacion_ligaduras(codigos))
  }
  if (isTRUE(perfil$comillas)) {
    agregar("comillas", .normalizacion_comillas(codigos))
  }
  if (isTRUE(perfil$puntuacion)) {
    agregar("puntuacion", .normalizacion_puntuacion(codigos))
  }
  codigos <- .normalizacion_ordenar(.normalizacion_descomponer(codigos))
  agregar("descomposicion_canonica", codigos)
  if (isTRUE(perfil$acentos)) {
    if (is.null(protecciones)) protecciones <- .normalizacion_protecciones(perfil)
    agregar("acentos", .normalizacion_quitar_acentos(
      codigos, perfil, protecciones))
  }
  if (isTRUE(perfil$minusculas)) {
    texto_actual <- .normalizacion_minusculas(.normalizacion_a_texto(codigos))
    codigos <- utf8ToInt(texto_actual)
    etapas[["minusculas"]] <- texto_actual
  }
  if (isTRUE(perfil$espacios)) {
    texto_actual <- trimws(gsub("[[:space:]]+", " ",
      .normalizacion_a_texto(codigos), perl = TRUE))
    etapas[["espacios_finales"]] <- texto_actual
  }
  etapas
}
.normalizacion_uno <- function(texto, perfil, protecciones = NULL) {
  etapas <- .normalizacion_etapas(texto, perfil, protecciones)
  unname(etapas[[length(etapas)]])
}
.normalizacion_aplicar <- function(textos, perfil) {
  .normalizacion_vector_rapida(textos, perfil)
}
.normalizacion_fusiones <- function(textos, perfil) {
  if (!length(textos)) {
    return(list(pasos = list(), n_distintos_normalizados = 0L))
  }
  nombres <- c("espacios", "ancho", "ligaduras", "comillas",
               "puntuacion", "acentos", "minusculas")
  activos <- nombres[vapply(perfil[nombres], isTRUE, logical(1L))]
  if (.normalizacion_ascii_sin_cambios(textos, perfil)) {
    return(list(
      pasos = stats::setNames(as.list(rep.int(0L, length(activos))), activos),
      n_distintos_normalizados = length(unique(textos))
    ))
  }
  completo <- .normalizacion_aplicar(textos, perfil)
  n_completo <- length(unique(completo))
  pasos <- list()
  for (nombre in activos) {
    sin_paso <- perfil
    sin_paso[[nombre]] <- FALSE
    sin_normalizar <- .normalizacion_aplicar(textos, sin_paso)
    pasos[[nombre]] <- max(0L,
      length(unique(sin_normalizar)) - n_completo)
  }
  list(pasos = pasos, n_distintos_normalizados = n_completo)
}
.normalizacion_fusiones_vocabulario <- function(textos, perfil) {
  n_distintos <- length(textos)
  fusiones <- .normalizacion_fusiones(textos, perfil)
  list(
    estado = "exacto",
    n_distintos = n_distintos,
    n_usados = n_distintos,
    n_distintos_normalizados = fusiones$n_distintos_normalizados,
    pasos = fusiones$pasos
  )
}
# La forma con que un valor de texto entra a la normalizacion para decidir si dos
# valores son el mismo: su clave de bytes. Lo declarado `bytes` entra con cada
# byte alto escrito `\xHH` -no se interpreta como texto, como documenta
# `?detectar_duplicados_aproximados`-, `latin1` y UTF-8 del mismo texto entran
# igual, y la barra invertida se escapa para que el escape no choque con un
# literal tecleado.
#
# `perfilar()` usaba otra regla para el mismo campo: marcaba UTF-8 lo `bytes`
# valido. Sobre {"Cafe Central", el mismo con tilde declarado `bytes`, "CAFE
# CENTRAL"}, `meta$normalizacion_fusiones` publicaba 1 normalizado y
# `detectar_claves()` 2, los dos en un campo llamado `n_distintos_normalizados`;
# y `detectar_duplicados_aproximados()`, que puede reutilizar el informe del
# perfil, fundia lo que sus propios pares separaban. `detectar_duplicados_
# aproximados()` arma la misma clave por su cuenta, sobre cualquier tipo.
.clave_para_normalizar <- function(x) {
  # Un instante entra por su valor y no por su texto en la hora de la sesion: en
  # America/Adak dos instantes distintos se escribian igual y se fundian.
  if (inherits(x, "POSIXt")) return(.texto_instante_utc(x))
  if (is.character(x) || is.factor(x)) x <- .clave_bytes(x)
  suppressWarnings(as.character(.texto_analizable(x)$valores))
}

.normalizacion_fusiones_tabla <- function(datos, resuelta) {
  nombres <- names(datos)
  if (is.null(nombres)) nombres <- paste0("V", seq_along(datos))
  salida <- lapply(seq_along(datos), function(i) {
    perfil <- .normalizacion_para_columna(resuelta, nombres[[i]])
    if (!.normalizacion_tiene_pasos(perfil)) return(NULL)
    x <- datos[[i]]
    if (is.list(x) || .es_columna_compuesta(x)) return(NULL)
    if (is.character(x) || is.factor(x)) x <- unique(as.character(x))
    # Lo que no se puede leer queda afuera -se informa como codificacion
    # invalida-, y eso se decide sobre el valor tal como llego: la clave de un
    # valor ilegible es un escape ASCII perfectamente comparable.
    legibles <- !is.na(suppressWarnings(as.character(
      .texto_analizable(x)$valores
    )))
    valores <- .clave_para_normalizar(x)
    valores <- unique(valores[legibles & !is.na(valores)])
    .normalizacion_fusiones_vocabulario(
      valores, perfil
    )
  })
  names(salida) <- nombres
  salida <- salida[!vapply(salida, is.null, logical(1L))]
  if (length(salida)) salida else NULL
}
