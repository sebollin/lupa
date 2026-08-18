## Construye la verdad de RIOLU para anomalias de patron.
## El ground truth usa -1 para nulo, 1 para anomalia y 0 para normal.
## El codigo del repositorio tiene licencia MIT; la licencia de los datos
## reutilizados no esta declarada en la fuente y no se redistribuye.

.args_riolu <- commandArgs(trailingOnly = FALSE)
.file_riolu <- .args_riolu[startsWith(.args_riolu, "--file=")]
.dir_riolu <- if (length(.file_riolu) == 1L) {
  dirname(normalizePath(sub("--file=", "", .file_riolu), mustWork = TRUE))
} else getwd()
source(file.path(.dir_riolu, "_comun_bancos.R"), local = FALSE)

.riolu_datasets <- Sys.getenv(
  "RIOLU_DATASETS",
  unset = "flights,hosp_100k,hosp_10k,hosp_1k,movies"
)
.riolu_datasets <- trimws(unlist(strsplit(.riolu_datasets, ",", fixed = TRUE)))
.riolu_datasets <- .riolu_datasets[nzchar(.riolu_datasets)]
.riolu_local <- Sys.getenv("RIOLU_DATA_DIR", unset = "")
.riolu_base <-
  "https://raw.githubusercontent.com/mooselab/Discover-Data-Quality-With-RIOLU/main/"
.riolu_temp <- tempfile("lupa-riolu-")
dir.create(.riolu_temp)

.candidatos_riolu <- function(dataset, prefijo, archivo) {
  c(file.path("test_anomaly_detection", dataset, paste0(prefijo, archivo)),
    file.path(dataset, paste0(prefijo, archivo)),
    paste0(prefijo, archivo), archivo)
}

.construir_riolu <- function(dataset) {
  dirty_nombre <- paste0("dirty_", dataset, ".csv")
  clean_nombre <- paste0("clean_", dataset, ".csv")
  truth_nombre <- paste0("gt_", dataset, ".csv")
  dirty <- .obtener_archivo(
    paste0(.riolu_base, "test_anomaly_detection/", dataset, "/", dirty_nombre),
    .riolu_local, .candidatos_riolu(dataset, "", dirty_nombre),
    file.path(.riolu_temp, dirty_nombre), dirty_nombre
  )
  clean <- .obtener_archivo(
    paste0(.riolu_base, "test_anomaly_detection/", dataset, "/", clean_nombre),
    .riolu_local, .candidatos_riolu(dataset, "", clean_nombre),
    file.path(.riolu_temp, clean_nombre), clean_nombre
  )
  verdad <- .obtener_archivo(
    paste0(.riolu_base, "ground_truth_anomaly_detection/", truth_nombre),
    .riolu_local,
    c(file.path("ground_truth_anomaly_detection", truth_nombre), truth_nombre),
    file.path(.riolu_temp, truth_nombre), truth_nombre
  )
  fallas <- c(
    if (!dirty$ok) paste0("dirty: ", dirty$razon) else NULL,
    if (!clean$ok) paste0("clean: ", clean$razon) else NULL,
    if (!verdad$ok) paste0("ground truth: ", verdad$razon) else NULL
  )
  if (length(fallas)) {
    return(.no_disponible(paste0("RIOLU/", dataset), .riolu_base,
                          paste(fallas, collapse = "; ")))
  }
  sucia <- tryCatch(.leer_csv_texto(dirty$ruta), error = function(e) e)
  limpia <- tryCatch(.leer_csv_texto(clean$ruta), error = function(e) e)
  if (inherits(sucia, "error") || inherits(limpia, "error")) {
    return(.no_disponible(
      paste0("RIOLU/", dataset), .riolu_base,
      paste(c(if (inherits(sucia, "error")) conditionMessage(sucia) else NULL,
              if (inherits(limpia, "error")) conditionMessage(limpia) else NULL),
            collapse = "; ")
    ))
  }
  referencia <- tryCatch(
    .comparar_dirty_clean(
      paste0("RIOLU/", dataset), sucia, limpia,
      versiones = rbind(.version_archivo(dirty), .version_archivo(clean)),
      fuente = .riolu_base
    ),
    error = function(e) .no_disponible(paste0("RIOLU/", dataset),
                                       .riolu_base, conditionMessage(e))
  )
  if (!isTRUE(referencia$disponible)) return(referencia)
  referencia$dataset <- dataset
  mascara <- .mascara_verdad_larga(
    verdad$ruta, nrow(sucia), ncol(sucia), names(sucia)
  )
  if (is.null(mascara)) {
    return(.no_disponible(
      paste0("RIOLU/", dataset), .riolu_base,
      "el archivo de ground truth no tiene dimensiones ni columnas reconocibles"
    ))
  }
  referencia <- .aplicar_mascara_verdad(referencia, mascara, sucia)
  referencia$versiones <- rbind(referencia$versiones,
                                .version_archivo(verdad))
  referencia
}

verdad_riolu <- setNames(
  lapply(.riolu_datasets, .construir_riolu), .riolu_datasets
)
if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("RIOLU: ground truth de anomalias de patron; -1 no es anomalia\n")
  invisible(lapply(verdad_riolu, .imprimir_estado))
}
