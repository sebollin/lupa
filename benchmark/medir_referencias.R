## Rehace las cifras de referencia que publican los README:
## tablas anchas (500 filas x 50/300/1000 columnas) y valores largos
## (2.000 filas x 1.000/20.000/200.000 caracteres).
##
## Uso, desde la raiz del repositorio:
##   LUPA_RUTA=. Rscript benchmark/medir_referencias.R
##
## La salida queda en benchmark/datos/medir-referencias.csv, con fecha,
## entorno, huella del codigo y cada medicion.

.ruta <- Sys.getenv("LUPA_RUTA")
if (!nzchar(.ruta)) stop("falta LUPA_RUTA")
.raiz_paquete <- normalizePath(.ruta, mustWork = TRUE)
if (file.exists(file.path(.raiz_paquete, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.raiz_paquete, quiet = TRUE)
} else if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("No se encuentra la fuente ni una instalacion de lupa.", call. = FALSE)
}

.archivos_r <- sort(list.files(
  file.path(.raiz_paquete, "R"), "[.]R$", full.names = TRUE
))
.revision <- paste0(
  as.character(utils::packageVersion("lupa")), "; R/ md5 ",
  local({
    # La suma de la LISTA de sumas, como benchmark/_firma.R: los primeros 12
    # caracteres de las sumas pegadas eran la suma del primer archivo y nada mas.
    lista <- tempfile()
    on.exit(unlink(lista))
    writeLines(paste(unname(tools::md5sum(.archivos_r)), basename(.archivos_r)), lista)
    substr(unname(tools::md5sum(lista)), 1L, 12L)
  })
)
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.entorno <- list(
  fecha = .fecha,
  revision_paquete = .revision,
  huella_codigo = .revision,
  version_paquete = as.character(utils::packageVersion("lupa")),
  version_R = R.version.string,
  plataforma = R.version$platform,
  sistema = unname(Sys.info()[["sysname"]]),
  version_sistema = unname(Sys.info()[["release"]]),
  maquina = unname(Sys.info()[["machine"]]),
  nucleos = parallel::detectCores(logical = TRUE)
)

cat("paquete:", .raiz_paquete, "\n")
cat("R:", as.character(getRversion()), "| fecha:", format(Sys.Date()), "\n\n")

## La primera corrida calienta -compila, llena caches- y se descarta.
medir <- function(datos, ...) {
  invisible(suppressWarnings(lupa::perfilar(utils::head(datos, 20), ...)))
  system.time(suppressWarnings(lupa::perfilar(datos, ...)))[["elapsed"]]
}

.mediciones <- list()
.agregar <- function(escenario, medida, valor, unidad, n_filas,
                     n_columnas, largo = NA_integer_) {
  .mediciones[[length(.mediciones) + 1L]] <<- data.frame(
    entorno = "Rscript",
    as.data.frame(.entorno, stringsAsFactors = FALSE),
    escenario = escenario, medida = medida, valor = as.numeric(valor),
    unidad = unidad, n_filas = n_filas, n_columnas = n_columnas,
    largo = largo, stringsAsFactors = FALSE
  )
}

set.seed(20260830)

cat("== tablas anchas: 500 filas ==\n")
for (nc in c(50L, 300L, 1000L)) {
  datos <- as.data.frame(setNames(
    lapply(seq_len(nc), function(i) {
      if (i %% 3 == 0) sample(c("alfa", "beta", "gama", NA), 500, TRUE)
      else round(runif(500, 0, 1000), 2)
    }),
    paste0("c", seq_len(nc))
  ), stringsAsFactors = FALSE)
  segundos <- medir(datos)
  cat(sprintf("  %4d columnas: %6.2f s\n", nc, segundos))
  .agregar("tabla_ancha", "perfilar", segundos, "segundos", 500L, nc)
  .agregar(
    "tabla_ancha", "celdas_por_segundo", 500 * nc / segundos,
    "celdas_por_segundo", 500L, nc
  )
}

cat("\n== valores largos: 2.000 filas, 3 columnas ==\n")
for (largo in c(1000L, 20000L, 200000L)) {
  base <- vapply(seq_len(40), function(i)
    paste(sample(letters, largo, TRUE), collapse = ""), "")
  datos <- data.frame(
    id = seq_len(2000),
    texto = sample(base, 2000, TRUE),
    monto = round(runif(2000, 0, 100), 2),
    stringsAsFactors = FALSE
  )
  segundos <- medir(datos)
  cat(sprintf("  %6d caracteres: %6.2f s\n", largo, segundos))
  .agregar("valores_largos", "perfilar", segundos, "segundos", 2000L, 3L,
           largo)
}

cat("\nLas referencias de MEMORIA de motor remoto (0,13 GB/M filas, 7 GB por\n")
cat("4,5 M procesadas, 19 GB por 12,8 M) NO se rehacen aca: salieron de un\n")
cat("motor de produccion al que este banco no accede. En los README tienen que\n")
cat("estar fechadas como referencia de esa corrida, no como promesa.\n")

salida <- do.call(rbind, .mediciones)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(directorio_datos, "medir-referencias.csv")
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("Salida:", archivo_salida, "\n")
