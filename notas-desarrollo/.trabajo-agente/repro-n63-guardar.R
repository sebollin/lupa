args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("uso: Rscript repro-n63-guardar.R LOCALE ARCHIVO")
locale <- args[[1L]]
archivo <- args[[2L]]

for (categoria in c("LC_CTYPE", "LC_COLLATE")) {
  suppressWarnings(Sys.setlocale(categoria, locale))
  if (!identical(Sys.getlocale(categoria), locale)) {
    stop("no se pudo fijar ", categoria, "=", locale)
  }
}

pkgload::load_all(".", quiet = TRUE)

sin_marca <- function(x) rawToChar(charToRaw(x))
id_a <- sin_marca("año_1")
perfil_nombre <- sin_marca("Básico")
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN63"),
  "personas", "dato"
)
modelo_n63 <- modelo(instancia)
perfil_n63 <- perfil_evaluacion(
  perfil_nombre,
  regla_evaluacion("Presente", function(x) x == 1)
)
crear_corrida <- function(id, valores) {
  medidas <- medir(
    modelo_n63, data.frame(dato = valores), id_medicion = id,
    fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC")
  )
  evaluar(medidas, perfil_n63)
}

historico <- suppressWarnings(historico_calidad(
  crear_corrida("Zebra_2", c(1, 1, NA, NA)),
  crear_corrida(id_a, c(1, 1, 1, NA))
))
guardar_historico(historico, archivo, sobrescribir = TRUE)
cat("locale=", Sys.getlocale("LC_CTYPE"), "|", Sys.getlocale("LC_COLLATE"),
    "| filas=", nrow(historico), "\n", sep = "")
