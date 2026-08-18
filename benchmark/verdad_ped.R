## Construye la verdad de PED desde difference.csv.
## dirty.csv y clean.csv se usan para conservar los datos a perfilar y para
## comprobar que la diferencia publicada coincide con el par alineado.
## El repositorio no declara una licencia para los datos: se baja para medir y
## no se redistribuye con lupa.

.args_ped <- commandArgs(trailingOnly = FALSE)
.file_ped <- .args_ped[startsWith(.args_ped, "--file=")]
.dir_ped <- if (length(.file_ped) == 1L) {
  dirname(normalizePath(sub("--file=", "", .file_ped), mustWork = TRUE))
} else getwd()
source(file.path(.dir_ped, "_comun_bancos.R"), local = FALSE)

.ped_datasets <- Sys.getenv(
  "PED_DATASETS", unset = "Flight,Hospital,MIMIC,Plane,Soccer"
)
.ped_datasets <- trimws(unlist(strsplit(.ped_datasets, ",", fixed = TRUE)))
.ped_datasets <- .ped_datasets[nzchar(.ped_datasets)]
.ped_local <- Sys.getenv("PED_DATA_DIR", unset = "")
.ped_base <- "https://raw.githubusercontent.com/twinklelittlestars/PED/main/data/"
.ped_temp <- tempfile("lupa-ped-")
dir.create(.ped_temp)

.construir_ped <- function(dataset) {
  nombre <- tolower(dataset)
  url_dirty <- paste0(.ped_base, dataset, "/dirty.csv")
  url_clean <- paste0(.ped_base, dataset, "/clean.csv")
  url_difference <- paste0(.ped_base, dataset, "/difference.csv")
  dirty <- .obtener_archivo(
    url_dirty, .ped_local,
    c(file.path("data", dataset, "dirty.csv"),
      file.path("data", nombre, "dirty.csv"),
      file.path(dataset, "dirty.csv"), file.path(nombre, "dirty.csv"),
      paste0(dataset, "_dirty.csv"), paste0(nombre, "_dirty.csv")),
    file.path(.ped_temp, paste0(nombre, "_dirty.csv")), "dirty.csv"
  )
  clean <- .obtener_archivo(
    url_clean, .ped_local,
    c(file.path("data", dataset, "clean.csv"),
      file.path("data", nombre, "clean.csv"),
      file.path(dataset, "clean.csv"), file.path(nombre, "clean.csv"),
      paste0(dataset, "_clean.csv"), paste0(nombre, "_clean.csv")),
    file.path(.ped_temp, paste0(nombre, "_clean.csv")), "clean.csv"
  )
  if (!isTRUE(dirty$ok) || !isTRUE(clean$ok)) {
    razon <- paste(
      c(if (!dirty$ok) paste0("dirty: ", dirty$razon) else NULL,
        if (!clean$ok) paste0("clean: ", clean$razon) else NULL),
      collapse = "; "
    )
    return(.no_disponible(paste0("PED/", dataset), .ped_base, razon))
  }
  sucia <- tryCatch(.leer_csv_texto(dirty$ruta), error = function(e) e)
  limpia <- tryCatch(.leer_csv_texto(clean$ruta), error = function(e) e)
  if (inherits(sucia, "error") || inherits(limpia, "error")) {
    razon <- paste(
      c(if (inherits(sucia, "error")) conditionMessage(sucia) else NULL,
        if (inherits(limpia, "error")) conditionMessage(limpia) else NULL),
      collapse = "; "
    )
    return(.no_disponible(paste0("PED/", dataset), .ped_base, razon))
  }
  referencia <- tryCatch(
    .comparar_dirty_clean(
      paste0("PED/", dataset), sucia, limpia,
      versiones = rbind(.version_archivo(dirty), .version_archivo(clean)),
      fuente = .ped_base
    ),
    error = function(e) .no_disponible(paste0("PED/", dataset), .ped_base,
                                       conditionMessage(e))
  )
  if (!isTRUE(referencia$disponible)) return(referencia)
  referencia$dataset <- dataset
  diferencia <- .obtener_archivo(
    url_difference, .ped_local,
    c(file.path("data", dataset, "difference.csv"),
      file.path("data", nombre, "difference.csv"),
      file.path(dataset, "difference.csv"),
      file.path(nombre, "difference.csv"),
      paste0(dataset, "_difference.csv"), paste0(nombre, "_difference.csv")),
    file.path(.ped_temp, paste0(nombre, "_difference.csv")), "difference.csv"
  )
  referencia$difference_disponible <- isTRUE(diferencia$ok)
  referencia$difference_consistente <- NA
  if (!isTRUE(diferencia$ok)) {
    return(.no_disponible(
      paste0("PED/", dataset), .ped_base,
      paste0("difference.csv: ", diferencia$razon)
    ))
  }
  mascara <- .mascara_ped_difference(
    diferencia$ruta, nrow(sucia), ncol(sucia), names(sucia)
  )
  if (is.null(mascara)) {
    return(.no_disponible(
      paste0("PED/", dataset), .ped_base,
      "difference.csv no tiene filas Index/Attribute alineadas con dirty.csv"
    ))
  }
  mascara_par <- matrix(FALSE, nrow = nrow(sucia), ncol = ncol(sucia))
  if (nrow(referencia$verdad)) {
    mascara_par[cbind(
      referencia$verdad$fila, referencia$verdad$columna_indice
    )] <- TRUE
  }
  referencia$difference_consistente <- all(mascara == mascara_par)
  if (!isTRUE(referencia$difference_consistente)) {
    mascara_alineada <- .mascara_ped_difference(
      diferencia$ruta, nrow(sucia), ncol(sucia), names(sucia),
      desplazamiento_fila = 0L
    )
    if (!is.null(mascara_alineada) && all(mascara_alineada == mascara_par)) {
      mascara <- mascara_alineada
      referencia$difference_consistente <- TRUE
    }
  }
  if (!isTRUE(referencia$difference_consistente)) {
    return(.no_disponible(
      paste0("PED/", dataset), .ped_base,
      "difference.csv no coincide con las celdas distintas de dirty.csv y clean.csv"
    ))
  }
  referencia$difference_celdas <- sum(mascara)
  referencia <- .aplicar_mascara_verdad(referencia, mascara, sucia)
  referencia$versiones <- rbind(referencia$versiones,
                                .version_archivo(diferencia))
  referencia
}

verdad_ped <- setNames(lapply(.ped_datasets, .construir_ped), .ped_datasets)
if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("PED: difference.csv es la verdad; dirty/clean es comprobacion auxiliar\n")
  invisible(lapply(verdad_ped, .imprimir_estado))
}
