## Construye la verdad de PED a partir de dirty.csv y clean.csv.
## difference.csv se baja como comprobacion adicional cuando esta disponible.
## El repositorio de datos no se redistribuye con lupa.

.args_ped <- commandArgs(trailingOnly = FALSE)
.file_ped <- .args_ped[startsWith(.args_ped, "--file=")]
.dir_ped <- if (length(.file_ped) == 1L) {
  dirname(normalizePath(sub("--file=", "", .file_ped), mustWork = TRUE))
} else getwd()
source(file.path(.dir_ped, "_comun_bancos.R"), local = FALSE)

.ped_datasets <- Sys.getenv("PED_DATASETS", unset = "Flight,Hospital")
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
    c(file.path(dataset, "dirty.csv"), file.path(nombre, "dirty.csv"),
      paste0(dataset, "_dirty.csv"), paste0(nombre, "_dirty.csv")),
    file.path(.ped_temp, paste0(nombre, "_dirty.csv")), "dirty.csv"
  )
  clean <- .obtener_archivo(
    url_clean, .ped_local,
    c(file.path(dataset, "clean.csv"), file.path(nombre, "clean.csv"),
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
  referencia$dataset <- dataset
  diferencia <- .obtener_archivo(
    url_difference, .ped_local,
    c(file.path(dataset, "difference.csv"), file.path(nombre, "difference.csv"),
      paste0(dataset, "_difference.csv"), paste0(nombre, "_difference.csv")),
    file.path(.ped_temp, paste0(nombre, "_difference.csv")), "difference.csv"
  )
  referencia$difference_disponible <- isTRUE(diferencia$ok)
  referencia$difference_consistente <- NA
  if (isTRUE(diferencia$ok)) {
    mascara <- .mascara_verdad_larga(
      diferencia$ruta, nrow(sucia), ncol(sucia)
    )
    if (!is.null(mascara)) {
      referencia$difference_consistente <- identical(
        which(mascara, arr.ind = TRUE),
        which(as.matrix(referencia$por_columna) > 0, arr.ind = TRUE)
      )
      referencia$difference_celdas <- sum(mascara)
    }
    referencia$versiones <- rbind(referencia$versiones,
                                  .version_archivo(diferencia))
  }
  referencia
}

verdad_ped <- setNames(lapply(.ped_datasets, .construir_ped), .ped_datasets)
if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("PED: dirty/clean por celda; difference.csv es una comprobacion auxiliar\n")
  invisible(lapply(verdad_ped, .imprimir_estado))
}
