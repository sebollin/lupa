#!/usr/bin/env Rscript

# Genera el informe que se muestra en los README y, en modo normal, captura su
# primer viewport con Firefox. La carpeta de trabajo se recibe como argumento;
# no se usa para conservar la imagen final, que pertenece a man/figures/.

argumentos <- commandArgs(trailingOnly = TRUE)
solo_html <- identical(argumentos[[1L]], "--solo-html")
if (solo_html) argumentos <- argumentos[-1L]
if (length(argumentos) != 1L) {
  stop(
    "Uso: Rscript scripts/capturar-informe-html.R [--solo-html] carpeta-salida",
    call. = FALSE
  )
}

# Se crea antes de normalizar: `normalizePath()` de una carpeta que todavia no
# existe devuelve la ruta relativa, y Firefox no encuentra un perfil relativo.
dir.create(argumentos[[1L]], recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(argumentos[[1L]])) stop("No se pudo crear la carpeta de salida.", call. = FALSE)
salida <- normalizePath(argumentos[[1L]], winslash = "/", mustWork = TRUE)

perfil_firefox <- file.path(salida, "perfil-firefox")
home_firefox <- file.path(salida, "home-firefox")
tmp_firefox <- file.path(salida, "tmp-firefox")
invisible(lapply(
  c(perfil_firefox, home_firefox, tmp_firefox),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

# Firefox hereda estas variables y queda confinado a subcarpetas de la salida.
Sys.setenv(HOME = home_firefox, TMPDIR = tmp_firefox)

raiz <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(raiz, "DESCRIPTION"))) {
  stop("El guion debe ejecutarse desde la raíz del proyecto.", call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Hace falta el paquete sugerido `pkgload`.", call. = FALSE)
}
pkgload::load_all(raiz, quiet = TRUE, export_all = FALSE, helpers = FALSE)

archivo_html <- file.path(salida, "informe-lupa.html")
analisis <- lupa::analizar(lupa::datos_operativos)
lupa::reportar(
  analisis,
  archivo = archivo_html,
  sobrescribir = TRUE,
  titulo = "Informe de calidad de datos"
)
if (!file.exists(archivo_html) || file.size(archivo_html) == 0) {
  stop("`reportar()` no produjo el HTML esperado.", call. = FALSE)
}
message("HTML: ", archivo_html)
if (solo_html) quit(save = "no", status = 0L)

firefox <- Sys.which("firefox")
if (!nzchar(firefox)) {
  stop("No se encontró `firefox` en PATH.", call. = FALSE)
}

captura <- file.path(salida, "informe-html-captura.png")
url <- paste0(
  "file://",
  normalizePath(archivo_html, winslash = "/", mustWork = TRUE)
)
# Una ventana alta muestra el encabezado, las advertencias de alcance, el
# resumen del perfil y el comienzo de los hallazgos; despues se reduce al 60 %
# con una paleta de 256 colores sin tramado (el tramado ensucia los fondos
# lisos). Asi queda en unos 45 KB.
argumentos_firefox <- c(
  "--headless",
  "--no-remote",
  "--profile", shQuote(perfil_firefox),
  "--window-size=1280,1560",
  "--screenshot", shQuote(captura),
  shQuote(url)
)
estado <- system2(firefox, args = argumentos_firefox)
if (!identical(estado, 0L) || !file.exists(captura)) {
  stop("Firefox no produjo la captura.", call. = FALSE)
}

convert <- Sys.which("convert")
if (!nzchar(convert)) {
  stop("Hace falta `convert` (ImageMagick) para reducir la captura.", call. = FALSE)
}
reducida <- file.path(salida, "informe-html-reducido.png")
estado <- system2(
  convert,
  args = c(
    shQuote(captura), "-crop", "1280x1560+0+0", "+repage", "-resize", "60%",
    "-strip", "+dither", "-colors", "256",
    "-define", "png:compression-level=9", shQuote(reducida)
  )
)
if (!identical(estado, 0L) || !file.exists(reducida)) {
  stop("`convert` no pudo reducir la captura.", call. = FALSE)
}
captura <- reducida
maximo <- 150 * 1024

figura <- file.path(raiz, "man", "figures", "informe-html.png")
dir.create(dirname(figura), recursive = TRUE, showWarnings = FALSE)
if (file.size(captura) > maximo) {
  stop(
    "La captura reducida pesa más de 150 KB: revisar la ventana o la reducción.",
    call. = FALSE
  )
}
if (!file.copy(captura, figura, overwrite = TRUE)) {
  stop("No se pudo copiar la captura a man/figures/.", call. = FALSE)
}
message("PNG: ", figura, " (", file.size(figura), " bytes)")
