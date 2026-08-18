## Construye la verdad de TableEG a partir de dirty.csv y clean.csv.
## Las anotaciones se conservan como metadato si vienen en la copia local,
## pero la verdad de celdas se valida contra el par alineado dirty/clean.
## El repositorio no publica una licencia de datos: no se redistribuye.

.args_tableeg <- commandArgs(trailingOnly = FALSE)
.file_tableeg <- .args_tableeg[startsWith(.args_tableeg, "--file=")]
.dir_tableeg <- if (length(.file_tableeg) == 1L) {
  dirname(normalizePath(sub("--file=", "", .file_tableeg), mustWork = TRUE))
} else getwd()
source(file.path(.dir_tableeg, "_comun_bancos.R"), local = FALSE)

.tableeg_datasets <- Sys.getenv("TABLEEG_DATASETS", unset = "Beers,Hospital,Flights")
.tableeg_datasets <- trimws(unlist(strsplit(.tableeg_datasets, ",", fixed = TRUE)))
.tableeg_datasets <- .tableeg_datasets[nzchar(.tableeg_datasets)]
.tableeg_local <- Sys.getenv("TABLEEG_DATA_DIR", unset = "")
.tableeg_archive <- Sys.getenv("TABLEEG_ARCHIVE_URL", unset = "")
.tableeg_source <-
  "https://drive.google.com/drive/folders/10LdB9LGgymbI6W8D2936uRF6eFRL24xy?usp=sharing"
.tableeg_temp <- tempfile("lupa-tableeg-")
dir.create(.tableeg_temp)

.extraer_tableeg_archive <- function() {
  if (!nzchar(.tableeg_archive)) return("")
  archivo <- file.path(.tableeg_temp, "tableeg.zip")
  intento <- tryCatch(
    utils::download.file(.tableeg_archive, archivo, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(intento, "error") || !identical(as.integer(intento), 0L) ||
      !file.exists(archivo)) return("")
  extraido <- file.path(.tableeg_temp, "extraido")
  dir.create(extraido)
  ok <- tryCatch(utils::unzip(archivo, exdir = extraido), error = function(e) e)
  if (inherits(ok, "error")) return("")
  extraido
}

.tableeg_extraido <- .extraer_tableeg_archive()
.tableeg_raices <- unique(c(.tableeg_local, .tableeg_extraido))
.tableeg_raices <- .tableeg_raices[nzchar(.tableeg_raices)]

.candidatos_tableeg <- function(dataset, archivo) {
  nombre <- tolower(dataset)
  c(file.path("source", dataset, archivo), file.path("source", nombre, archivo),
    file.path(dataset, archivo), file.path(nombre, archivo),
    paste0(dataset, "_", archivo), paste0(nombre, "_", archivo), archivo)
}

.construir_tableeg <- function(dataset) {
  if (!length(.tableeg_raices)) {
    return(.no_disponible(
      paste0("TableEG/", dataset), .tableeg_source,
      paste0(
        "la fuente ofrece una carpeta de Google Drive, no una URL directa; ",
        "defina TABLEEG_DATA_DIR con una copia extraida o TABLEEG_ARCHIVE_URL"
      )
    ))
  }
  raiz <- .tableeg_raices[[1L]]
  dirty <- .obtener_archivo(
    .tableeg_source, raiz, .candidatos_tableeg(dataset, "dirty.csv"),
    file.path(.tableeg_temp, paste0(tolower(dataset), "_dirty.csv")), "dirty.csv"
  )
  clean <- .obtener_archivo(
    .tableeg_source, raiz, .candidatos_tableeg(dataset, "clean.csv"),
    file.path(.tableeg_temp, paste0(tolower(dataset), "_clean.csv")), "clean.csv"
  )
  if (!isTRUE(dirty$ok) || !isTRUE(clean$ok)) {
    razon <- paste(
      c(if (!dirty$ok) paste0("dirty: ", dirty$razon) else NULL,
        if (!clean$ok) paste0("clean: ", clean$razon) else NULL),
      collapse = "; "
    )
    return(.no_disponible(paste0("TableEG/", dataset), .tableeg_source, razon))
  }
  sucia <- tryCatch(.leer_csv_texto(dirty$ruta), error = function(e) e)
  limpia <- tryCatch(.leer_csv_texto(clean$ruta), error = function(e) e)
  if (inherits(sucia, "error") || inherits(limpia, "error")) {
    razon <- paste(
      c(if (inherits(sucia, "error")) conditionMessage(sucia) else NULL,
        if (inherits(limpia, "error")) conditionMessage(limpia) else NULL),
      collapse = "; "
    )
    return(.no_disponible(paste0("TableEG/", dataset), .tableeg_source, razon))
  }
  referencia <- tryCatch(
    .comparar_dirty_clean(
      paste0("TableEG/", dataset), sucia, limpia,
      versiones = rbind(.version_archivo(dirty), .version_archivo(clean)),
      fuente = .tableeg_source
    ),
    error = function(e) .no_disponible(paste0("TableEG/", dataset),
                                       .tableeg_source, conditionMessage(e))
  )
  referencia$dataset <- dataset
  anotacion <- .obtener_archivo(
    .tableeg_source, raiz,
    c(.candidatos_tableeg(dataset, paste0(dataset, "_annotation.jsonl")),
      .candidatos_tableeg(dataset,
                          paste0(tolower(dataset), "_annotation.jsonl"))),
    file.path(.tableeg_temp, paste0(tolower(dataset), "_annotation.jsonl")),
    "annotation.jsonl"
  )
  referencia$anotacion_disponible <- isTRUE(anotacion$ok)
  if (isTRUE(anotacion$ok)) {
    referencia$versiones <- rbind(referencia$versiones,
                                  .version_archivo(anotacion))
  }
  referencia
}

verdad_tableeg <- setNames(
  lapply(.tableeg_datasets, .construir_tableeg), .tableeg_datasets
)
if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("TableEG: dirty/clean por celda; anotacion JSONL es metadato auxiliar\n")
  invisible(lapply(verdad_tableeg, .imprimir_estado))
}
