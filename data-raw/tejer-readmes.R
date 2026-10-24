# Teje los dos README de lupa con el paquete cargado desde el árbol.
#
#   Rscript data-raw/tejer-readmes.R
#
# La raíz del paquete es la carpeta que contiene a data-raw/, así que el guion
# también se puede llamar con una ruta desde otro directorio. Teje
# README.es.Rmd en README.es.md y, si existe, README.Rmd en README.md. El código
# de los ejemplos sale de data-raw/readme-trozos.R, que los dos leen con
# knitr::read_chunk().
#
# El paquete se carga con pkgload::load_all(export_all = FALSE): mide el árbol,
# no una instalación anterior, y un ejemplo que usara una función interna falla
# acá igual que fallaría a quien lo copie.
#
# No teje si el locale no es UTF-8, si falta un paquete que el tejido usa o si
# el paquete cargado no es el de la raíz. Después de tejer revisa cada .md:
# ninguna línea de salida de más de 100 caracteres, ninguna fecha del día,
# ninguna ruta temporal; y que los dos .Rmd tengan las mismas secciones y los
# mismos trozos. Sale con código 1 si algo falla.

falla <- function(...) {
  message("tejer-readmes: ", ...)
  quit(save = "no", status = 1L)
}

# --- La raíz y lo que el tejido necesita -------------------------------------
argumentos <- commandArgs(trailingOnly = FALSE)
guion <- sub("^--file=", "", grep("^--file=", argumentos, value = TRUE))
raiz <- if (length(guion) == 1L) dirname(dirname(guion)) else "."
if (!file.exists(file.path(raiz, "DESCRIPTION")) ||
    !file.exists(file.path(raiz, "data-raw", "readme-trozos.R"))) {
  falla("no encuentro DESCRIPTION y data-raw/readme-trozos.R en ", raiz)
}
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  falla("el locale no es UTF-8 (", Sys.getlocale("LC_CTYPE"), "): las salidas ",
        "cambiarían con el locale de quien teje")
}
necesarios <- c("pkgload", "knitr", "rmarkdown", "DBI", "RSQLite")
faltan <- necesarios[!vapply(necesarios, requireNamespace, logical(1L),
                             quietly = TRUE)]
if (length(faltan)) falla("faltan paquetes: ", paste(faltan, collapse = ", "))
if (!rmarkdown::pandoc_available("2.0")) falla("falta pandoc (2.0 o posterior)")

# --- El paquete del árbol ----------------------------------------------------
suppressMessages(pkgload::load_all(raiz, quiet = TRUE, export_all = FALSE,
                                   helpers = FALSE))
cargado <- normalizePath(getNamespaceInfo("lupa", "path"), winslash = "/")
if (!identical(cargado, normalizePath(raiz, winslash = "/"))) {
  falla("el paquete cargado está en ", cargado, ", no en la raíz")
}

# Desde acá se trabaja en la raíz. render() también se pasa ahí, y un temporal
# de sesión con ruta relativa dejaría de existir: en ese caso se rehace dentro
# de data-raw/ (fuera del paquete construido) y R lo borra al salir.
inicial <- getwd()
setwd(raiz)
if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", tempdir())) {
  viejo <- file.path(inicial, tempdir())
  Sys.setenv(TMPDIR = "data-raw")
  nuevo <- tempdir(check = TRUE)
  if (!dir.exists(nuevo)) falla("no pude rehacer el temporal de sesión")
  unlink(viejo, recursive = TRUE)
}

# Salidas que no cambien con la terminal de quien teje.
options(cli.num_colors = 1L, cli.unicode = TRUE, crayon.enabled = FALSE,
        width = 90L)

# --- Tejer -------------------------------------------------------------------
fuentes <- c(es = "README.es.Rmd", en = "README.Rmd")
fuentes <- fuentes[file.exists(fuentes)]
if (!"es" %in% names(fuentes)) falla("no existe README.es.Rmd")

hoy <- format(Sys.Date())
problemas <- character()
for (idioma in names(fuentes)) {
  fuente <- fuentes[[idioma]]
  t0 <- proc.time()[["elapsed"]]
  salida <- tryCatch(
    rmarkdown::render(
      fuente,
      output_format = rmarkdown::github_document(
        html_preview = FALSE, pandoc_args = "--wrap=preserve"
      ),
      envir = new.env(parent = globalenv()), quiet = TRUE, encoding = "UTF-8"
    ),
    error = function(e) falla("no se teje ", fuente, ": ", conditionMessage(e))
  )
  lineas <- readLines(salida, warn = FALSE, encoding = "UTF-8")
  ancho <- which(startsWith(lineas, "#>") & nchar(lineas, type = "width") > 100L)
  for (i in ancho) {
    problemas <- c(problemas, sprintf("%s:%d: salida de más de 100 caracteres",
                                      basename(salida), i))
  }
  for (i in grep(hoy, lineas, fixed = TRUE)) {
    problemas <- c(problemas, sprintf("%s:%d: fecha del día en la salida",
                                      basename(salida), i))
  }
  for (i in grep("Rtmp|/tmp/|\\\\Temp\\\\", lineas)) {
    problemas <- c(problemas, sprintf("%s:%d: ruta temporal en la salida",
                                      basename(salida), i))
  }
  cat(sprintf("tejido: %s -> %s (%d líneas, %.1f s)\n", fuente,
              basename(salida), length(lineas),
              proc.time()[["elapsed"]] - t0))
}

# --- Los dos idiomas, con la misma forma -------------------------------------
if (length(fuentes) == 2L) {
  forma <- lapply(fuentes, function(f) {
    l <- readLines(f, warn = FALSE, encoding = "UTF-8")
    list(secciones = length(grep("^#{1,3} ", l)),
         trozos = grep("^```\\{r +[A-Za-z0-9_.-]+", l, value = TRUE))
  })
  if (!identical(forma$es$secciones, forma$en$secciones)) {
    problemas <- c(problemas, sprintf(
      "los README tienen %d y %d secciones", forma$es$secciones,
      forma$en$secciones
    ))
  }
  if (!identical(forma$es$trozos, forma$en$trozos)) {
    problemas <- c(problemas, "los README no usan los mismos trozos en el mismo orden")
  }
}

# --- La portada del sitio, en castellano ---------------------------------------
# (El guion ya esta parado en la raiz del paquete: ver `setwd(raiz)` arriba.)
# El sitio esta en castellano (las guias, la referencia, la interfaz), asi que su
# portada es el README castellano y no el ingles. pkgdown la toma de
# `pkgdown/index.md`. Los README enlazan los archivos del repositorio por su
# direccion en GitHub -un enlace relativo a un archivo que no viaja en el tarball
# es un URI invalido para CRAN-; en el sitio, NEWS va a su pagina de novedades.
castellano <- "README.es.md"
if (file.exists(castellano) && !length(problemas)) {
  portada <- readLines(castellano, warn = FALSE, encoding = "UTF-8")
  repositorio <- "https://github.com/sebollin/lupa/blob/main/"
  portada <- gsub(paste0("](", repositorio, "NEWS.md)"), "](news/index.html)",
                  portada, fixed = TRUE)
  portada <- gsub("](README.md)", paste0("](", repositorio, "README.md)"),
                  portada, fixed = TRUE)
  portada <- sub("README.es.md se genera desde README.es.Rmd",
                 "pkgdown/index.md se genera desde README.es.Rmd", portada, fixed = TRUE)
  dir.create("pkgdown", showWarnings = FALSE)
  con <- file(file.path("pkgdown", "index.md"), open = "wb")
  writeLines(enc2utf8(portada), con, useBytes = TRUE)
  close(con)
  cat("portada del sitio: pkgdown/index.md\n")
}

if (length(problemas)) falla(length(problemas), " problemas:\n  ",
                             paste(problemas, collapse = "\n  "))
cat("tejer-readmes: todo en orden\n")
