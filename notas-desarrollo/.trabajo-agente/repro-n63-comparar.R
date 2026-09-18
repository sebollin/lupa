args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("uso: Rscript repro-n63-comparar.R LOCALE ARCHIVO")
locale <- args[[1L]]
archivo <- args[[2L]]

for (categoria in c("LC_CTYPE", "LC_COLLATE")) {
  suppressWarnings(Sys.setlocale(categoria, locale))
  if (!identical(Sys.getlocale(categoria), locale)) {
    stop("no se pudo fijar ", categoria, "=", locale)
  }
}

pkgload::load_all(".", quiet = TRUE)
historico <- suppressWarnings(leer_historico(archivo))
avisos <- character()
deriva <- withCallingHandlers(
  detectar_deriva_calidad(historico),
  warning = function(condicion) {
    avisos <<- c(avisos, conditionMessage(condicion))
    invokeRestart("muffleWarning")
  }
)
cat("locale=", Sys.getlocale("LC_CTYPE"), "|", Sys.getlocale("LC_COLLATE"),
    "| deriva=\n", sep = "")
print(deriva[, c(
  "id_medicion_anterior", "id_medicion_actual", "delta", "direccion",
  "severidad", "aspecto"
)])
cat("avisos=", length(avisos), "\n", sep = "")
if (length(avisos)) cat(paste(avisos, collapse = "\n"), "\n", sep = "")
