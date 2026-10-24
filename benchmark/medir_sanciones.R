## Verifica los hallazgos de severidad `error` sobre un registro publico real:
## cada uno se compara con la misma cuenta hecha aparte, en R base, sobre el
## mismo archivo. Si el paquete y la cuenta no coinciden, uno de los dos esta mal.
##
## Es el bloque "Registro real de sanciones" del articulo "Evidencia y
## rendimiento". Necesita red, salvo que LUPA_SANCIONES_CSV nombre una copia
## local del archivo. El CSV no entra al repositorio: el registro publicado cambia
## con el tiempo, asi que cada corrida guarda su fecha, sus filas y la suma
## SHA-256 del archivo que midio, y las comprobaciones se calculan sobre ese
## archivo, no contra conteos escritos de antemano.
##
## Uso:
##   Rscript benchmark/medir_sanciones.R
##   LUPA_SANCIONES_CSV=ruta/al/registro.csv Rscript benchmark/medir_sanciones.R
##
## Escribe benchmark/datos/medir-sanciones.csv. No publica valores del registro:
## solo cuentas.

.archivo_script <- grep("^--file=", commandArgs(), value = TRUE)
.archivo_script <- if (length(.archivo_script)) {
  sub("^--file=", "", .archivo_script[[1L]])
} else {
  file.path(getwd(), "benchmark", "medir_sanciones.R")
}
.raiz_paquete <- normalizePath(
  file.path(dirname(normalizePath(.archivo_script, mustWork = FALSE)), ".."),
  mustWork = TRUE
)
if (file.exists(file.path(.raiz_paquete, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.raiz_paquete, quiet = TRUE)
} else if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("No se encuentra la fuente ni una instalacion de lupa.", call. = FALSE)
}

# La revision medida es la huella del codigo de R/, como en verificar_motor.R:
# sobrevive a reescribir la historia y se rehace desde cualquier copia.
.revision_paquete <- function() {
  declarada <- Sys.getenv("LUPA_REVISION", unset = "")
  if (nzchar(declarada)) return(declarada)
  archivos <- sort(list.files(file.path(.raiz_paquete, "R"), "\\.R$",
                              full.names = TRUE))
  lista <- tempfile()
  on.exit(unlink(lista), add = TRUE)
  writeLines(paste(unname(tools::md5sum(archivos)), basename(archivos)), lista)
  paste0(as.character(utils::packageVersion("lupa")), "; R/ md5 ",
         substr(unname(tools::md5sum(lista)), 1L, 12L))
}

## ---- El registro ---------------------------------------------------------
##
## "Registro de Sanciones a Empresas", catalogo nacional de datos abiertos de
## Uruguay.
URL <- paste0(
  "https://catalogodatos.gub.uy/dataset/",
  "8fa60d47-b81e-4780-86dc-8985ab3589d7/resource/",
  "4c6cf3a6-488a-41a7-a7e3-4061f682d2eb/download/",
  "registro-de-sanciones-marzo-2025.csv"
)
destino <- Sys.getenv("LUPA_SANCIONES_CSV", unset = "")
if (!nzchar(destino)) {
  destino <- file.path(tempdir(), "registro-de-sanciones.csv")
  cat("bajando el registro...\n")
  utils::download.file(URL, destino, quiet = TRUE, mode = "wb")
}
huella <- unname(tools::sha256sum(destino))
datos <- utils::read.csv(
  destino, stringsAsFactors = FALSE, colClasses = "character"
)
names(datos) <- make.names(names(datos))
cat("filas: ", nrow(datos), " | columnas: ", ncol(datos),
    " | sha256: ", huella, "\n\n", sep = "")

## Este archivo llega con `Encoding()` en `unknown` y con acentos, que es como
## `read.csv()` entrega cualquier CSV en castellano.
perfil <- lupa::perfilar(
  datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
)
h <- lupa::hallazgos(perfil)
errores <- h[as.character(h$severidad) == "error", , drop = FALSE]

.afectados <- function(columna, tipo) {
  fila <- errores[errores$columna %in% columna & errores$tipo_hallazgo == tipo, ,
                  drop = FALSE]
  if (nrow(fila) != 1L) return(NA_integer_)
  as.integer(fila$n_afectados)
}

## ---- Las cuentas en R base -------------------------------------------------
.blancos <- function(x) sum(is.na(x) | trimws(x) == "")
.guiones <- function(x) sum(trimws(x) == "-", na.rm = TRUE)
fechas <- trimws(datos$Fecha)
formatos_r <- c(
  iso = sum(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fechas)),
  dma = sum(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", fechas))
)
formatos_lupa <- lupa::detectar_formatos_fecha(datos$Fecha)
formatos_lupa <- c(
  iso = sum(formatos_lupa$n[formatos_lupa$formato == "%Y-%m-%d"]),
  dma = sum(formatos_lupa$n[formatos_lupa$formato == "%d/%m/%Y"])
)
clave <- do.call(paste, c(datos, sep = "\r"))
repetidas <- table(clave)
repetidas <- repetidas[repetidas > 1L]

comprobaciones <- list(
  list("Fecha: celdas en blanco", "faltantes_disfrazados",
       .afectados("Fecha", "faltantes_disfrazados"), .blancos(datos$Fecha)),
  list("Fecha: formatos ISO y dd/mm/aaaa", "formatos_fecha_mixtos",
       paste(formatos_lupa, collapse = ","), paste(formatos_r, collapse = ",")),
  list("Empresa: filas con un espacio invisible", "controles_invisibles",
       .afectados("Empresa", "controles_invisibles"),
       sum(grepl(" ", datos$Empresa, fixed = TRUE))),
  list("Sancion.aplicada: guiones", "faltantes_disfrazados",
       .afectados("Sancion.aplicada", "faltantes_disfrazados"),
       .guiones(datos$Sancion.aplicada)),
  list("Importe: celdas en blanco", "faltantes_disfrazados",
       .afectados("Importe", "faltantes_disfrazados"), .blancos(datos$Importe)),
  list("Moneda: guiones y blancos", "faltantes_disfrazados",
       .afectados("Moneda", "faltantes_disfrazados"),
       .guiones(datos$Moneda) + .blancos(datos$Moneda)),
  list("Sector: celdas en blanco", "faltantes_disfrazados",
       .afectados("Sector", "faltantes_disfrazados"), .blancos(datos$Sector)),
  list("Motivo: celdas en blanco", "faltantes_disfrazados",
       .afectados("Motivo", "faltantes_disfrazados"), .blancos(datos$Motivo)),
  list("filas en grupos duplicados", "filas_duplicadas",
       .afectados(NA_character_, "filas_duplicadas"), as.integer(sum(repetidas)))
)

salida <- do.call(rbind, lapply(comprobaciones, function(caso) {
  data.frame(
    comprobacion = caso[[1L]], tipo_hallazgo = caso[[2L]],
    lupa = as.character(caso[[3L]]), r_base = as.character(caso[[4L]]),
    coincide = identical(as.character(caso[[3L]]), as.character(caso[[4L]])),
    stringsAsFactors = FALSE
  )
}))
cubiertos <- nrow(errores) == nrow(salida)
salida <- cbind(
  data.frame(
    fecha = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    revision_paquete = .revision_paquete(), version_R = R.version.string,
    filas_registro = nrow(datos), sha256_registro = huella,
    errores_lupa = nrow(errores), stringsAsFactors = FALSE
  ),
  salida
)
print(salida[, c("comprobacion", "lupa", "r_base", "coincide")], row.names = FALSE)
cat("\n", sum(salida$coincide), " de ", nrow(salida),
    " comprobaciones: el paquete y R base dan lo mismo. Hallazgos de severidad ",
    "error: ", nrow(errores), ".\n", sep = "")

directorio <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(salida, file.path(directorio, "medir-sanciones.csv"),
                 row.names = FALSE)
if (!all(salida$coincide) || !cubiertos) quit(status = 1)
