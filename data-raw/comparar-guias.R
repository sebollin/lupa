# Compara cada guía en inglés del sitio con su pareja en castellano.
#
#   Rscript data-raw/comparar-guias.R
#
# La raíz del paquete es la carpeta que contiene a data-raw/, así que el guion
# también se puede llamar con una ruta desde otro directorio. Corre en
# .github/workflows/pkgdown.yaml antes de construir el sitio.
#
# Las guías en inglés son las de la sección «Guides in English» de
# _pkgdown.yml. Viven sólo en el sitio: están en vignettes/articles/, que
# .Rbuildignore deja fuera del paquete. Cada una declara su pareja en su YAML,
# con la ruta desde vignettes/ (`pareja: lupa.Rmd`).
#
# La comparación no mira la prosa. Cada pareja tiene que coincidir en:
#   - los trozos de código: los mismos, en el mismo orden, con la misma
#     etiqueta y las mismas opciones (de las opciones que llevan texto, como
#     fig.cap, sólo se mira que estén);
#   - el código de cada trozo, sin comentarios ni espacios: los comentarios se
#     traducen, el código no;
#   - el código en línea, contado con sus repeticiones. La guía en inglés puede
#     envolver una expresión en una de 2 traducciones de formato, que se quitan
#     antes de comparar: chartr(".,", ",.", x) cambia las marcas de miles y de
#     decimales del castellano por las del inglés, y
#     sub(" y ", " and ", x, fixed = TRUE) cambia la conjunción de una lista;
#   - los títulos: la misma cantidad, con los mismos niveles y en el mismo
#     orden, y un ancla explícita en los mismos títulos;
#   - las funciones del paquete nombradas en el texto (`perfilar()`);
#   - los enlaces a la referencia: los que apuntan a reference/ y las menciones
#     `?tema`.
# Además, cada guía en inglés enlaza a su pareja, y sus enlaces a un ancla de
# la misma página (#ancla) llegan a un ancla explícita.
#
# También falla si una guía de la sección no existe, no declara su pareja o la
# pareja no existe, y si un .Rmd de vignettes/ declara una pareja sin estar en
# la sección. Sale con código 1 si algo falla. Sólo usa yaml; no carga lupa.

falla <- function(...) {
  message("comparar-guias: ", ...)
  quit(save = "no", status = 1L)
}

seccion_ingles <- "Guides in English"

# --- La raíz ---------------------------------------------------------------
argumentos <- commandArgs(trailingOnly = FALSE)
guion <- sub("^--file=", "", grep("^--file=", argumentos, value = TRUE))
raiz <- if (length(guion) == 1L) dirname(dirname(guion)) else "."
if (!file.exists(file.path(raiz, "DESCRIPTION")) ||
    !file.exists(file.path(raiz, "_pkgdown.yml"))) {
  falla("no encuentro DESCRIPTION y _pkgdown.yml en ", raiz)
}
if (!requireNamespace("yaml", quietly = TRUE)) falla("falta el paquete yaml")
dir_vinetas <- file.path(raiz, "vignettes")

leer <- function(ruta) readLines(ruta, warn = FALSE, encoding = "UTF-8")

# --- Partes de un .Rmd -------------------------------------------------------
# El YAML del principio, los trozos (con su cabecera y su código) y la prosa:
# las líneas que no son YAML ni código, con los bloques en blanco para que los
# números de línea sigan valiendo.
partes_rmd <- function(lineas) {
  yaml_meta <- list()
  prosa <- lineas
  i <- 1L
  if (length(lineas) && grepl("^---\\s*$", lineas[1L])) {
    fin <- which(grepl("^(---|\\.\\.\\.)\\s*$", lineas))
    fin <- fin[fin > 1L]
    if (length(fin)) {
      if (fin[1L] > 2L) {
        yaml_meta <- yaml::yaml.load(paste(lineas[2L:(fin[1L] - 1L)], collapse = "\n"))
      }
      if (!is.list(yaml_meta)) yaml_meta <- list()
      prosa[1L:fin[1L]] <- ""
      i <- fin[1L] + 1L
    }
  }
  trozos <- list()
  while (i <= length(lineas)) {
    apertura <- regmatches(lineas[i], regexec("^\\s*(`{3,})\\s*(.*?)\\s*$",
                                              lineas[i], perl = TRUE))[[1L]]
    if (!length(apertura)) {
      i <- i + 1L
      next
    }
    cerca <- nchar(apertura[2L])
    cierre <- i + 1L
    patron_cierre <- sprintf("^\\s*`{%d,}\\s*$", cerca)
    while (cierre <= length(lineas) && !grepl(patron_cierre, lineas[cierre])) {
      cierre <- cierre + 1L
    }
    cuerpo <- if (cierre > i + 1L) lineas[(i + 1L):(cierre - 1L)] else character()
    cabecera <- apertura[3L]
    es_r <- grepl("^\\{\\s*r\\b", cabecera, perl = TRUE)
    trozos[[length(trozos) + 1L]] <- list(
      linea = i,
      es_r = es_r,
      cabecera = if (es_r) sub("^\\{\\s*r(.*)\\}$", "\\1", cabecera, perl = TRUE) else cabecera,
      codigo = cuerpo
    )
    prosa[i:min(cierre, length(lineas))] <- ""
    i <- cierre + 1L
  }
  list(yaml = yaml_meta, trozos = trozos, prosa = prosa)
}

# Etiqueta y opciones de un trozo. La etiqueta es lo primero si no lleva `=`;
# las opciones se leen con el analizador de R y se escriben normalizadas.
opciones_con_texto <- c("fig.cap", "fig.alt", "fig.scap", "fig.subcap")
leer_cabecera <- function(texto) {
  texto <- sub("^\\s*,?\\s*", "", texto, perl = TRUE)
  etiqueta <- ""
  primera <- regmatches(texto, regexec("^([^,=]+?)\\s*(,|$)", texto, perl = TRUE))[[1L]]
  if (length(primera)) {
    etiqueta <- gsub("^[\"']|[\"']$", "", trimws(primera[2L]))
    texto <- substring(texto, nchar(primera[1L]) + 1L)
  }
  opciones <- tryCatch(
    as.list(parse(text = paste0("alist(", texto, ")"), keep.source = FALSE)[[1L]])[-1L],
    error = function(e) NULL
  )
  if (is.null(opciones)) return(list(etiqueta = etiqueta, opciones = paste("<ilegible>", texto)))
  if ("label" %in% names(opciones)) {
    etiqueta <- as.character(opciones[["label"]])
    opciones[["label"]] <- NULL
  }
  if (!length(opciones)) return(list(etiqueta = etiqueta, opciones = character()))
  valores <- vapply(names(opciones), function(n) {
    if (n %in% opciones_con_texto) return("<texto>")
    paste(deparse(opciones[[n]], width.cutoff = 500L), collapse = "")
  }, character(1L))
  normalizadas <- paste0(names(opciones), "=", valores)
  list(etiqueta = etiqueta, opciones = sort(normalizadas, method = "radix"))
}

# El código sin comentarios ni espacios, con el analizador de R. Lo que no se
# puede analizar (un trozo con eval = FALSE que no es R válido) se compara
# quitando las líneas de comentario y todo espacio.
normalizar_codigo <- function(lineas) {
  if (!length(lineas)) return("")
  datos <- tryCatch({
    expresiones <- parse(text = lineas, keep.source = TRUE)
    utils::getParseData(expresiones)
  }, error = function(e) NULL)
  if (is.null(datos) || !nrow(datos)) {
    lineas <- lineas[!grepl("^\\s*#", lineas)]
    return(gsub("\\s+", "", paste(lineas, collapse = "")))
  }
  datos <- datos[datos$terminal & datos$token != "COMMENT", ]
  datos <- datos[order(datos$line1, datos$col1), ]
  paste(datos$text, collapse = "")
}

# El código en línea (`r ...`), cada expresión normalizada con deparse(). En la
# guía en inglés se quitan antes las 2 traducciones de formato.
es_cadena <- function(x, valor) is.character(x) && length(x) == 1L && identical(x, valor)
quitar_formato_ingles <- function(e) {
  if (is.call(e) && identical(e[[1L]], as.name("chartr")) && length(e) == 4L &&
      is.null(names(e)) && es_cadena(e[[2L]], ".,") && es_cadena(e[[3L]], ",.")) {
    return(e[[4L]])
  }
  if (is.call(e) && identical(e[[1L]], as.name("sub")) && length(e) == 5L &&
      identical(names(e), c("", "", "", "", "fixed")) &&
      es_cadena(e[[2L]], " y ") && es_cadena(e[[3L]], " and ") &&
      isTRUE(e[[5L]])) {
    return(e[[4L]])
  }
  e
}
en_linea <- function(texto, ingles) {
  crudas <- regmatches(texto, gregexpr("`r[ #]([^`]+)`", texto, perl = TRUE))[[1L]]
  crudas <- sub("^`r[ #]", "", sub("`$", "", crudas))
  vapply(crudas, function(x) {
    e <- tryCatch(parse(text = x, keep.source = FALSE)[[1L]], error = function(err) NULL)
    if (is.null(e)) return(paste("<ilegible>", gsub("\\s+", "", x)))
    if (ingles) e <- quitar_formato_ingles(e)
    paste(deparse(e, width.cutoff = 500L), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

# Los títulos de la prosa: su nivel y su ancla explícita, si la tienen.
titulos <- function(prosa) {
  lineas <- grep("^#{1,6}\\s+\\S", prosa, value = TRUE, perl = TRUE)
  nivel <- nchar(sub("^(#+).*$", "\\1", lineas))
  ancla <- ifelse(
    grepl("\\{[^}]*#[^}\\s]+[^}]*\\}\\s*$", lineas, perl = TRUE),
    sub("^.*\\{[^}]*#([^}\\s]+)[^}]*\\}\\s*$", "\\1", lineas, perl = TRUE),
    NA_character_
  )
  data.frame(nivel = nivel, ancla = ancla, texto = lineas, stringsAsFactors = FALSE)
}

# Los fragmentos de código de la prosa (`...`), sin el código en línea. Se
# quita primero el código en línea: si no, su comilla de cierre abriría un
# fragmento falso hasta la comilla siguiente. Un fragmento puede cruzar un
# salto de línea.
fragmentos <- function(texto) {
  texto <- gsub("`r[ #][^`]+`", " ", texto, perl = TRUE)
  x <- regmatches(texto, gregexpr("(?s)(`+)(.+?)\\1", texto, perl = TRUE))[[1L]]
  trimws(gsub("\\s+", " ", gsub("^`+|`+$", "", x), perl = TRUE))
}

exportadas <- local({
  ns <- leer(file.path(raiz, "NAMESPACE"))
  x <- regmatches(ns, regexec("^export\\(([^)]+)\\)", ns))
  x <- vapply(x[lengths(x) > 0L], `[`, character(1L), 2L)
  gsub("^[\"'`]|[\"'`]$", "", x)
})

# Las funciones del paquete nombradas como llamada: `perfilar()`,
# `lupa::perfilar(clave = ...)`.
funciones_nombradas <- function(fragmentos) {
  llamadas <- grep("^([A-Za-z.][A-Za-z0-9._]*::)?[A-Za-z.][A-Za-z0-9._]*\\(", fragmentos,
                   value = TRUE, perl = TRUE)
  nombres <- sub("^(lupa::)?([A-Za-z.][A-Za-z0-9._]*)\\(.*$", "\\2", llamadas, perl = TRUE)
  nombres <- nombres[!grepl("::", llamadas, fixed = TRUE) | startsWith(llamadas, "lupa::")]
  sort(unique(nombres[nombres %in% exportadas]), method = "radix")
}

# Los enlaces a la referencia: un enlace que pasa por reference/ y la mención
# `?tema`, que pkgdown enlaza sola.
enlaces_referencia <- function(texto, fragmentos) {
  destinos <- regmatches(texto, gregexpr("\\]\\(([^)\\s]+)\\)", texto, perl = TRUE))[[1L]]
  destinos <- sub("^\\]\\(", "", sub("\\)$", "", destinos))
  a_referencia <- sub("^.*reference/", "reference/", grep("(^|/)reference/", destinos,
                                                           value = TRUE, perl = TRUE))
  ayuda <- grep("^\\?[A-Za-z.][A-Za-z0-9._]*$", fragmentos, value = TRUE, perl = TRUE)
  sort(unique(c(a_referencia, ayuda)), method = "radix")
}

destinos_de_enlaces <- function(texto) {
  x <- regmatches(texto, gregexpr("\\]\\(([^)\\s]+)\\)", texto, perl = TRUE))[[1L]]
  sub("^\\]\\(", "", sub("\\)$", "", x))
}

describir <- function(ruta, ingles) {
  p <- partes_rmd(leer(ruta))
  texto <- paste(p$prosa, collapse = "\n")
  frag <- fragmentos(texto)
  r <- Filter(function(t) t$es_r, p$trozos)
  otros <- Filter(function(t) !t$es_r, p$trozos)
  list(
    yaml = p$yaml,
    trozos = lapply(r, function(t) {
      c(leer_cabecera(t$cabecera), list(codigo = normalizar_codigo(t$codigo), linea = t$linea))
    }),
    bloques = vapply(otros, function(t) gsub("\\s+", "", paste(t$codigo, collapse = "")),
                     character(1L)),
    en_linea = sort(en_linea(texto, ingles), method = "radix"),
    titulos = titulos(p$prosa),
    funciones = funciones_nombradas(frag),
    referencia = enlaces_referencia(texto, frag),
    destinos = destinos_de_enlaces(texto)
  )
}

diferencia_conjuntos <- function(que, es, en) {
  solo_es <- setdiff(es, en)
  solo_en <- setdiff(en, es)
  c(
    if (length(solo_es)) sprintf("%s sólo en castellano: %s", que, paste(solo_es, collapse = ", ")),
    if (length(solo_en)) sprintf("%s sólo en inglés: %s", que, paste(solo_en, collapse = ", "))
  )
}

# Dónde empiezan a diferir 2 textos, para mostrar un recorte de cada uno.
recorte <- function(a, b) {
  n <- min(nchar(a), nchar(b))
  i <- 1L
  while (i <= n && substr(a, i, i) == substr(b, i, i)) i <- i + 1L
  desde <- max(1L, i - 20L)
  sprintf("castellano «%s» / inglés «%s»", substr(a, desde, desde + 60L),
          substr(b, desde, desde + 60L))
}

comparar <- function(es, en, nombre_pareja) {
  problemas <- character()
  # Trozos de R, uno por uno y en orden.
  n_es <- length(es$trozos)
  n_en <- length(en$trozos)
  if (n_es != n_en) {
    problemas <- c(problemas, sprintf("%d trozos en castellano y %d en inglés", n_es, n_en))
  }
  for (k in seq_len(min(n_es, n_en))) {
    a <- es$trozos[[k]]
    b <- en$trozos[[k]]
    donde <- sprintf("trozo %d (castellano línea %d, inglés línea %d)", k, a$linea, b$linea)
    if (!identical(a$etiqueta, b$etiqueta)) {
      problemas <- c(problemas, sprintf("%s: etiqueta «%s» y «%s»", donde, a$etiqueta,
                                        b$etiqueta))
    }
    if (!identical(a$opciones, b$opciones)) {
      problemas <- c(problemas, sprintf("%s: opciones [%s] y [%s]", donde,
                                        paste(a$opciones, collapse = ", "),
                                        paste(b$opciones, collapse = ", ")))
    }
    if (!identical(a$codigo, b$codigo)) {
      problemas <- c(problemas, sprintf("%s: el código difiere: %s", donde,
                                        recorte(a$codigo, b$codigo)))
    }
  }
  if (!identical(es$bloques, en$bloques)) {
    problemas <- c(problemas, "los bloques que no son de R difieren")
  }
  # Código en línea, con repeticiones.
  if (!identical(es$en_linea, en$en_linea)) {
    tabla_es <- table(es$en_linea)
    tabla_en <- table(en$en_linea)
    todas <- union(names(tabla_es), names(tabla_en))
    cuenta <- function(t, x) if (x %in% names(t)) as.integer(t[[x]]) else 0L
    for (x in todas) {
      if (cuenta(tabla_es, x) != cuenta(tabla_en, x)) {
        problemas <- c(problemas, sprintf(
          "código en línea `r %s`: %d vez/veces en castellano y %d en inglés", x,
          cuenta(tabla_es, x), cuenta(tabla_en, x)
        ))
      }
    }
  }
  # Títulos: niveles y anclas explícitas en el mismo lugar.
  t_es <- es$titulos
  t_en <- en$titulos
  if (!identical(t_es$nivel, t_en$nivel)) {
    problemas <- c(problemas, sprintf(
      "los títulos difieren: niveles %s en castellano y %s en inglés",
      paste(t_es$nivel, collapse = ""), paste(t_en$nivel, collapse = "")
    ))
  } else {
    sin_par <- which(is.na(t_es$ancla) != is.na(t_en$ancla))
    for (k in sin_par) {
      problemas <- c(problemas, sprintf(
        "título %d: ancla explícita en un solo idioma («%s» / «%s»)", k, t_es$texto[k],
        t_en$texto[k]
      ))
    }
  }
  problemas <- c(problemas,
                 diferencia_conjuntos("funciones nombradas", es$funciones, en$funciones),
                 diferencia_conjuntos("enlaces a la referencia", es$referencia, en$referencia))
  # La guía en inglés enlaza a su pareja.
  pagina <- paste0(sub("\\.[Rr]md$", "", basename(nombre_pareja)), ".html")
  destinos <- sub("#.*$", "", en$destinos)
  if (!any(basename(destinos) == pagina)) {
    problemas <- c(problemas, sprintf("la guía en inglés no enlaza a su pareja (%s)", pagina))
  }
  # Sus enlaces a un ancla de la misma página llegan a un ancla explícita.
  internos <- sub("^#", "", grep("^#", en$destinos, value = TRUE))
  perdidos <- setdiff(internos, t_en$ancla[!is.na(t_en$ancla)])
  if (length(perdidos)) {
    problemas <- c(problemas, sprintf(
      "enlaces a anclas que la guía en inglés no declara: %s",
      paste0("#", perdidos, collapse = ", ")
    ))
  }
  problemas
}

# --- Las guías en inglés -----------------------------------------------------
# yaml::read_yaml() convierte el archivo a la codificación de la sesión, y bajo
# un locale que no es UTF-8 corta la lectura en la primera letra que no puede
# convertir: la sección desaparecería sin error. Se lee como UTF-8, sin
# convertir.
config <- yaml::yaml.load(paste(leer(file.path(raiz, "_pkgdown.yml")), collapse = "\n"))
secciones <- config[["articles"]]
titulos_secciones <- vapply(secciones, function(s) {
  if (is.null(s[["title"]])) "" else as.character(s[["title"]])
}, character(1L))
en_seccion <- unlist(lapply(secciones[titulos_secciones == seccion_ingles], `[[`, "contents"))
en_seccion <- as.character(en_seccion)

# Todo .Rmd de vignettes/ que declare una pareja.
todas_rmd <- list.files(dir_vinetas, pattern = "\\.[Rr]md$", recursive = TRUE)
con_pareja <- todas_rmd[vapply(todas_rmd, function(f) {
  !is.null(partes_rmd(leer(file.path(dir_vinetas, f)))$yaml[["pareja"]])
}, logical(1L))]
nombre_articulo <- function(f) sub("\\.[Rr]md$", "", f)

problemas <- character()
fuera <- setdiff(nombre_articulo(con_pareja), en_seccion)
if (length(fuera)) {
  problemas <- c(problemas, sprintf(
    "declaran una pareja y no están en la sección «%s» de _pkgdown.yml: %s",
    seccion_ingles, paste(fuera, collapse = ", ")
  ))
}
if (!length(en_seccion) && !length(con_pareja)) {
  cat("comparar-guias: no hay guías en inglés\n")
  quit(save = "no", status = 0L)
}
if (!any(titulos_secciones == seccion_ingles)) {
  problemas <- c(problemas, sprintf("_pkgdown.yml no tiene la sección «%s»", seccion_ingles))
}

for (articulo in en_seccion) {
  candidatas <- file.path(dir_vinetas, paste0(articulo, c(".Rmd", ".rmd")))
  ruta_en <- candidatas[file.exists(candidatas)]
  if (!length(ruta_en)) {
    problemas <- c(problemas, sprintf("%s: está en la sección y no existe el .Rmd", articulo))
    next
  }
  en <- describir(ruta_en[1L], ingles = TRUE)
  pareja <- en$yaml[["pareja"]]
  if (is.null(pareja) || !is.character(pareja) || length(pareja) != 1L) {
    problemas <- c(problemas, sprintf("%s: el YAML no declara su pareja (`pareja:`)", articulo))
    next
  }
  ruta_es <- file.path(dir_vinetas, pareja)
  if (!file.exists(ruta_es)) {
    problemas <- c(problemas, sprintf("%s: su pareja %s no existe", articulo, pareja))
    next
  }
  es <- describir(ruta_es, ingles = FALSE)
  if (!is.null(es$yaml[["pareja"]])) {
    problemas <- c(problemas, sprintf("%s: su pareja %s también declara una pareja",
                                      articulo, pareja))
  }
  propios <- comparar(es, en, pareja)
  if (length(propios)) {
    problemas <- c(problemas, paste0(articulo, " <-> ", pareja, ": ", propios))
  } else {
    cat(sprintf(
      "ok: %s <-> %s (%d trozos, %d expresiones en línea, %d títulos, %d funciones, %d enlaces a la referencia)\n",
      articulo, pareja, length(en$trozos), length(en$en_linea), nrow(en$titulos),
      length(en$funciones), length(en$referencia)
    ))
  }
}

if (length(problemas)) {
  falla(length(problemas), " problemas:\n  ", paste(problemas, collapse = "\n  "))
}
cat("comparar-guias: todo en orden\n")
