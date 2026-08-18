## Mide cobertura por columna de lupa sobre los pares dirty/clean de Raha.
## Sólo usa R base, utils y el paquete lupa instalado.

if (sys.nframe() > 0L) {
  stop(
    paste0(
      "Este script no se ejecuta con source(). ",
      "Desde la ra\u00edz del repositorio use: ",
      "Rscript benchmark/medir_lupa.R"
    ),
    call. = FALSE
  )
}

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}

descripcion_lupa <- utils::packageDescription("lupa")
version_lupa <- as.character(descripcion_lupa[["Version"]])
built_lupa <- as.character(descripcion_lupa[["Built"]])
cat("lupa medida: version ", version_lupa, "\n", sep = "")
cat("Built: ", built_lupa, "\n", sep = "")

capacidades_faltantes <- character()
if (!"umbral_solapamiento_orden" %in% names(formals(lupa::perfilar))) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    "perfilar() no acepta `umbral_solapamiento_orden`"
  )
}
perfil_sonda <- tryCatch(
  lupa::perfilar(
    data.frame(id = 1:3),
    proteger_datos_personales = FALSE
  ),
  error = function(e) e
)
if (inherits(perfil_sonda, "error")) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    paste0(
      "no se pudo comprobar `secuencia_entera_densa`: ",
      conditionMessage(perfil_sonda)
    )
  )
} else if (!"secuencia_entera_densa" %in% names(perfil_sonda$columnas)) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    "el perfil no declara el campo `secuencia_entera_densa`"
  )
}
if (length(capacidades_faltantes)) {
  stop(
    paste0(
      "La instalaci\u00f3n de lupa no tiene las capacidades requeridas por ",
      "este benchmark:\n- ",
      paste(capacidades_faltantes, collapse = "\n- "),
      "\nInstale un build de estas mismas fuentes con ",
      "`R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz` y vuelva a ",
      "ejecutar el script."
    ),
    call. = FALSE
  )
}
if (!nzchar(system.file(package = "stringdist"))) {
  stop(
    paste0(
      "Este benchmark requiere la capacidad opcional stringdist de lupa. ",
      "Instale las dependencias sugeridas del paquete."
    ),
    call. = FALSE
  )
}

argumentos <- commandArgs(trailingOnly = FALSE)
marca_archivo <- "--file="
rutas_script <- argumentos[startsWith(argumentos, marca_archivo)]
if (length(rutas_script) != 1L) {
  stop(
    paste0(
      "No se pudo determinar la ruta de este script. ",
      "Desde la ra\u00edz del repositorio use: ",
      "Rscript benchmark/medir_lupa.R"
    ),
    call. = FALSE
  )
}
ruta_script <- sub(marca_archivo, "", rutas_script, fixed = TRUE)
ruta_script <- gsub("~+~", " ", ruta_script, fixed = TRUE)
directorio_script <- dirname(normalizePath(ruta_script, mustWork = TRUE))

opcion_anterior <- getOption("lupa.benchmark.silencioso")
options(lupa.benchmark.silencioso = TRUE)
tryCatch(
  source(file.path(directorio_script, "verdad_raha.R"), local = TRUE),
  finally = options(lupa.benchmark.silencioso = opcion_anterior)
)

.columnas_senaladas <- function(perfil, nombres) {
  hallazgos <- perfil$hallazgos
  hallazgos <- hallazgos[
    !is.na(hallazgos$columna) &
      as.character(hallazgos$severidad) != "ok", , drop = FALSE
  ]
  partes <- strsplit(as.character(hallazgos$columna), ",", fixed = TRUE)
  candidatas <- unique(trimws(unlist(partes, use.names = FALSE)))
  intersect(candidatas, nombres)
}

mediciones_lupa <- lapply(verdad_raha, function(referencia) {
  perfil <- lupa::perfilar(
    referencia$sucia,
    proteger_datos_personales = FALSE
  )
  senaladas <- .columnas_senaladas(perfil, names(referencia$sucia))
  afectadas <- referencia$columnas_afectadas
  data.frame(
    dataset = referencia$dataset,
    columnas = ncol(referencia$sucia),
    columnas_afectadas = length(afectadas),
    afectadas_con_hallazgo = length(intersect(afectadas, senaladas)),
    columnas_senaladas = length(senaladas),
    columnas_adicionales = length(setdiff(senaladas, afectadas)),
    no_cubiertas = paste(setdiff(afectadas, senaladas), collapse = ", "),
    adicionales = paste(setdiff(senaladas, afectadas), collapse = ", "),
    stringsAsFactors = FALSE
  )
})
resultado_lupa <- do.call(rbind, mediciones_lupa)
rownames(resultado_lupa) <- NULL

cat("Raha: comparacion celda a celda de dirty contra clean\n")
for (i in seq_len(nrow(resumen_raha))) {
  x <- resumen_raha[i, ]
  cat(sprintf(
    "%-9s %4d x %2d  celdas dif.: %4d  filas afect.: %4d  tasa: %6.3f %%\n",
    x$dataset, x$filas, x$columnas, x$celdas_diferentes,
    x$filas_afectadas, 100 * x$tasa
  ))
}

cat("\nCobertura por columna\n")
for (i in seq_len(nrow(resultado_lupa))) {
  x <- resultado_lupa[i, ]
  cat(sprintf(
    "%-9s %2d de %2d afectadas con hallazgo; %2d senaladas; %d adicionales\n",
    x$dataset, x$afectadas_con_hallazgo, x$columnas_afectadas,
    x$columnas_senaladas, x$columnas_adicionales
  ))
}
cat(sprintf(
  "total     %2d de %2d afectadas con hallazgo; %2d senaladas; %d adicionales\n",
  sum(resultado_lupa$afectadas_con_hallazgo),
  sum(resultado_lupa$columnas_afectadas),
  sum(resultado_lupa$columnas_senaladas),
  sum(resultado_lupa$columnas_adicionales)
))

cat("\nColumnas adicionales (observaciones apoyadas, no falsos positivos):\n")
for (i in seq_len(nrow(resultado_lupa))) {
  cat(resultado_lupa$dataset[[i]], ": ",
      resultado_lupa$adicionales[[i]], "\n", sep = "")
}

cat("\nVersion de los archivos obtenidos (bytes y Adler-32):\n")
for (i in seq_len(nrow(versiones_raha))) {
  x <- versiones_raha[i, ]
  cat(sprintf(
    "%-9s %-9s %6d bytes  %s\n",
    x$dataset, x$archivo, x$bytes, x$adler32
  ))
}
