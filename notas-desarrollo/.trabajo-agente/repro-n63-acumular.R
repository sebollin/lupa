args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("uso: Rscript repro-n63-acumular.R LOCALE ARCHIVO_GUARDADO ARCHIVO_NUEVO")
}
locale <- args[[1L]]
archivo_guardado <- args[[2L]]
archivo_nuevo <- args[[3L]]

for (categoria in c("LC_CTYPE", "LC_COLLATE")) {
  suppressWarnings(Sys.setlocale(categoria, locale))
  if (!identical(Sys.getlocale(categoria), locale)) {
    stop("no se pudo fijar ", categoria, "=", locale)
  }
}

pkgload::load_all(".", quiet = TRUE)
sin_marca <- function(x) rawToChar(charToRaw(x))
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN63"),
  "personas", "dato"
)
modelo_n63 <- modelo(instancia)
perfil_n63 <- perfil_evaluacion(
  sin_marca("Básico"),
  regla_evaluacion("Presente", function(x) x == 1)
)
crear_corrida <- function(id, valores) {
  medidas <- medir(
    modelo_n63, data.frame(dato = valores), id_medicion = id,
    fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC")
  )
  evaluar(medidas, perfil_n63)
}
nuevo <- suppressWarnings(historico_calidad(
  crear_corrida("Zebra_2", c(1, 1, NA, NA)),
  crear_corrida(sin_marca("año_1"), c(1, 1, 1, NA))
))
guardar_historico(nuevo, archivo_nuevo, sobrescribir = TRUE)
guardado <- suppressWarnings(leer_historico(archivo_guardado))
resultado <- tryCatch(
  list(ok = TRUE, filas = nrow(acumular_historico(guardado, nuevo)),
       mensaje = ""),
  error = function(condicion) list(ok = FALSE, filas = NA_integer_,
                                   mensaje = conditionMessage(condicion))
)
cat("locale=", Sys.getlocale("LC_CTYPE"), "| ok=", resultado$ok,
    "| filas=", resultado$filas, "| mensaje=", resultado$mensaje, "\n",
    sep = "")
