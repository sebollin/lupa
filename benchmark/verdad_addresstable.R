## Construye una muestra alineada de AddressTable para medir escala.
## No descarga por defecto el archivo completo: el registro reporta varios GB.
## Se usa ADDRESSTABLE_DATA_DIR o ADDRESSTABLE_FILE_URL para una copia local
## o una URL directa de una muestra autorizada.

.args_address <- commandArgs(trailingOnly = FALSE)
.file_address <- .args_address[startsWith(.args_address, "--file=")]
.dir_address <- if (length(.file_address) == 1L) {
  dirname(normalizePath(sub("--file=", "", .file_address), mustWork = TRUE))
} else getwd()
source(file.path(.dir_address, "_comun_bancos.R"), local = FALSE)

.address_local <- Sys.getenv("ADDRESSTABLE_DATA_DIR", unset = "")
.address_url <- Sys.getenv("ADDRESSTABLE_FILE_URL", unset = "")
.address_record <- "https://zenodo.org/records/20841898"
.address_max_rows <- suppressWarnings(as.integer(
  Sys.getenv("ADDRESSTABLE_MAX_ROWS", unset = "10000")
))
if (is.na(.address_max_rows) || .address_max_rows < 1L) .address_max_rows <- 10000L
.address_temp <- tempfile("lupa-addresstable-")
dir.create(.address_temp)

.construir_addresstable <- function() {
  if (!nzchar(.address_local) && !nzchar(.address_url)) {
    return(.no_disponible(
      "AddressTable", .address_record,
      paste0(
        "el registro contiene archivos de escala y no se baja el completo ",
        "sin una URL directa de muestra; defina ADDRESSTABLE_DATA_DIR o ",
        "ADDRESSTABLE_FILE_URL"
      )
    ))
  }
  url_dirty <- if (nzchar(.address_url)) .address_url else .address_record
  url_clean <- Sys.getenv("ADDRESSTABLE_CLEAN_URL", unset = .address_url)
  dirty <- .obtener_archivo(
    url_dirty, .address_local,
    c("dirty.csv", "full-named_dirty.csv", "dirty/full-named.csv",
      "full_named_dirty.csv"),
    file.path(.address_temp, "dirty.csv"), "dirty.csv"
  )
  clean <- .obtener_archivo(
    url_clean, .address_local,
    c("clean.csv", "full-named_clean.csv", "clean/full-named.csv",
      "full_named_clean.csv"),
    file.path(.address_temp, "clean.csv"), "clean.csv"
  )
  if (!isTRUE(dirty$ok) || !isTRUE(clean$ok)) {
    return(.no_disponible(
      "AddressTable", .address_record,
      paste(c(if (!dirty$ok) paste0("dirty: ", dirty$razon) else NULL,
              if (!clean$ok) paste0("clean: ", clean$razon) else NULL),
            collapse = "; ")
    ))
  }
  sucia <- tryCatch(.leer_csv_texto(dirty$ruta, .address_max_rows),
                    error = function(e) e)
  limpia <- tryCatch(.leer_csv_texto(clean$ruta, .address_max_rows),
                     error = function(e) e)
  if (inherits(sucia, "error") || inherits(limpia, "error")) {
    return(.no_disponible(
      "AddressTable", .address_record,
      paste(c(if (inherits(sucia, "error")) conditionMessage(sucia) else NULL,
              if (inherits(limpia, "error")) conditionMessage(limpia) else NULL),
            collapse = "; ")
    ))
  }
  referencia <- tryCatch(
    .comparar_dirty_clean(
      "AddressTable", sucia, limpia,
      versiones = rbind(.version_archivo(dirty), .version_archivo(clean)),
      fuente = .address_record
    ),
    error = function(e) .no_disponible("AddressTable", .address_record,
                                       conditionMessage(e))
  )
  referencia$dataset <- "full-named (muestra inicial)"
  referencia$alcance <- paste0("primeras ", nrow(sucia), " filas; max_rows=",
                               .address_max_rows)
  referencia
}

verdad_addresstable <- .construir_addresstable()
if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("AddressTable: comparacion dirty/clean sobre muestra declarada\n")
  .imprimir_estado(verdad_addresstable)
}
