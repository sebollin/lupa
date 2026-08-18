## Mide los bancos externos contra sus verdades declaradas.
## PED y TableEG usan la diferencia dirty/clean; RIOLU usa solo la mascara
## de anomalias de patron; AddressTable usa la muestra indicada.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_bancos.R.",
    call. = FALSE
  )
}

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}

descripcion_lupa <- utils::packageDescription("lupa")
cat("lupa medida: version ", descripcion_lupa[["Version"]], "\n", sep = "")
cat("Built: ", descripcion_lupa[["Built"]], "\n", sep = "")

capacidades_faltantes <- character()
if (!"umbral_solapamiento_orden" %in% names(formals(lupa::perfilar))) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    "perfilar() no acepta `umbral_solapamiento_orden`"
  )
}
sonda <- tryCatch(
  lupa::perfilar(data.frame(id = 1:3), proteger_datos_personales = FALSE),
  error = function(e) e
)
if (inherits(sonda, "error")) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    paste0("no se pudo ejecutar la sonda de perfilado: ",
           conditionMessage(sonda))
  )
} else if (!"secuencia_entera_densa" %in% names(sonda$columnas)) {
  capacidades_faltantes <- c(
    capacidades_faltantes,
    "el perfil no declara el campo `secuencia_entera_densa`"
  )
}
if (length(capacidades_faltantes)) {
  stop(
    paste0(
      "La instalacion de lupa no tiene las capacidades requeridas:\n- ",
      paste(capacidades_faltantes, collapse = "\n- "),
      "\nInstale un build de estas fuentes y vuelva a ejecutar el script."
    ),
    call. = FALSE
  )
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Este benchmark requiere la capacidad opcional stringdist de lupa.",
       call. = FALSE)
}

.args_bancos <- commandArgs(trailingOnly = FALSE)
.file_bancos <- .args_bancos[startsWith(.args_bancos, "--file=")]
if (length(.file_bancos) != 1L) {
  stop("No se pudo determinar la ruta del script.", call. = FALSE)
}
.dir_bancos <- dirname(normalizePath(
  sub("--file=", "", .file_bancos), mustWork = TRUE
))
source(file.path(.dir_bancos, "_comun_bancos.R"), local = FALSE)

.cargar_verdad <- function(nombre) {
  ambiente <- new.env(parent = globalenv())
  sys.source(file.path(.dir_bancos, paste0("verdad_", nombre, ".R")),
             envir = ambiente)
  get(paste0("verdad_", nombre), envir = ambiente)
}

opcion_anterior <- getOption("lupa.benchmark.silencioso")
options(lupa.benchmark.silencioso = TRUE)
verdad_ped <- .cargar_verdad("ped")
verdad_tableeg <- .cargar_verdad("tableeg")
verdad_riolu <- .cargar_verdad("riolu")
verdad_addresstable <- .cargar_verdad("addresstable")
options(lupa.benchmark.silencioso = opcion_anterior)

.fila_no_medida <- function(referencia, razon = referencia$razon) {
  ## Un solo elemento vacio aca rompia el `data.frame` entero y con el la
  ## corrida completa: justamente en el camino que existe para declarar que algo
  ## NO se pudo medir. El caso de fallo no puede ser el que falla.
  primero <- function(x, alternativa) {
    if (is.null(x) || !length(x) || all(is.na(x)) ||
        !any(nzchar(as.character(x)))) {
      alternativa
    } else {
      as.character(x)[[1L]]
    }
  }
  nombre_banco <- primero(referencia$banco,
                          primero(referencia$resumen$banco, "desconocido"))
  nombre_dataset <- primero(referencia$dataset, nombre_banco)
  razon <- primero(razon, "sin razon declarada")
  data.frame(
    estado = "no medido",
    banco = nombre_banco,
    dataset = nombre_dataset,
    alcance = NA_character_,
    filas = NA_integer_, columnas = NA_integer_,
    celdas_verdad = NA_integer_, celdas_con_hallazgo_trazable = NA_integer_,
    celdas_acertadas_trazables = NA_integer_,
    precision_celdas_trazables = NA_real_,
    cobertura_celdas_verdad = NA_real_,
    columnas_verdad = NA_integer_, columnas_con_hallazgo = NA_integer_,
    columnas_verdad_con_hallazgo = NA_integer_,
    motivo = as.character(razon),
    stringsAsFactors = FALSE
  )
}

.medir_una <- function(referencia, banco, tipos = NULL, alcance = "completo") {
  if (!isTRUE(referencia$disponible)) return(.fila_no_medida(referencia))
  datos <- referencia$sucia
  mascara <- if (!is.null(referencia$mascara_patron)) {
    referencia$mascara_patron
  } else {
    salida <- matrix(FALSE, nrow = nrow(datos), ncol = ncol(datos))
    if (nrow(referencia$verdad)) {
      salida[cbind(referencia$verdad$fila,
                   referencia$verdad$columna_indice)] <- TRUE
    }
    salida
  }
  if (!identical(dim(mascara), dim(datos))) {
    return(.fila_no_medida(
      referencia,
      "la mascara de verdad no esta alineada con dirty.csv"
    ))
  }
  medido <- tryCatch(
    .medir_contra_mascara(
      referencia, mascara, datos, tipos = tipos, alcance = alcance
    ),
    error = function(e) e
  )
  if (inherits(medido, "error")) {
    return(.fila_no_medida(referencia, conditionMessage(medido)))
  }
  medido$estado <- "medido"
  medido$banco <- banco
  medido$motivo <- NA_character_
  medido[, c(
    "estado", "banco", "dataset", "alcance", "filas", "columnas",
    "celdas_verdad", "celdas_con_hallazgo_trazable",
    "celdas_acertadas_trazables", "precision_celdas_trazables",
    "cobertura_celdas_verdad", "columnas_verdad", "columnas_con_hallazgo",
    "columnas_verdad_con_hallazgo", "motivo"
  )]
}

mediciones <- list()
for (referencia in verdad_ped) {
  mediciones[[length(mediciones) + 1L]] <-
    .medir_una(referencia, "PED", tipos = NULL, alcance = "dirty-clean")
}
for (referencia in verdad_tableeg) {
  mediciones[[length(mediciones) + 1L]] <- .medir_una(
    referencia, "TableEG", tipos = NULL, alcance = "dirty-clean"
  )
}
for (referencia in verdad_riolu) {
  mediciones[[length(mediciones) + 1L]] <- .medir_una(
    referencia, "RIOLU", tipos = "patron_raro", alcance = "anomalias_de_patron"
  )
}
mediciones[[length(mediciones) + 1L]] <- .medir_una(
  verdad_addresstable, "AddressTable", tipos = NULL,
  alcance = verdad_addresstable$alcance %||% "muestra"
)
resultado_bancos <- do.call(rbind, mediciones)
rownames(resultado_bancos) <- NULL

cat("\nResultados de los bancos externos\n")
print(resultado_bancos, row.names = FALSE)
cat("\nLas celdas trazables son la union de filas que lupa adjunta a hallazgos no ok.\n")
cat("No son un conteo interno de celdas medidas por lupa ni un recall diagnostico.\n")
