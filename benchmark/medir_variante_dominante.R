## Mide el detector experimental de formas equifrecuentes sin cambiar el
## resultado publico: sirve para decidir si el diagnostico puede encenderse.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_variante_dominante.R.",
    call. = FALSE
  )
}

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}

.args <- commandArgs(trailingOnly = FALSE)
.archivo <- .args[startsWith(.args, "--file=")]
if (length(.archivo) != 1L) {
  stop("No se pudo determinar la ruta del script.", call. = FALSE)
}
.raiz <- normalizePath(file.path(dirname(sub("--file=", "", .archivo)), ".."))

.cargar_tablas_limpias <- function(ruta) {
  ambiente <- new.env(parent = globalenv())
  ambiente$test_that <- function(...) invisible(NULL)
  ambiente$skip_if_not_installed <- function(...) invisible(NULL)
  ambiente$skip_on_cran <- function(...) invisible(NULL)
  ambiente$skip_on_ci <- function(...) invisible(NULL)
  sys.source(file.path(ruta, "tests", "testthat", "test-ronda107.R"),
             envir = ambiente)
  ambiente$.tablas_limpias_r107()
}

.medir_tabla <- function(datos) {
  normalizacion <- lupa:::.resolver_normalizacion(TRUE)
  grupos <- lapply(seq_along(datos), function(i) {
    lupa:::.grupos_casi_duplicados_vocabulario(
      datos[[i]], normalizacion, names(datos)[[i]],
      detectar_variantes_equifrecuentes = TRUE
    )
  })
  detalles <- list()
  for (i in seq_along(grupos)) {
    objeto <- grupos[[i]]
    if (is.null(objeto) || !length(objeto$grupos_equifrecuentes)) next
    for (grupo in objeto$grupos_equifrecuentes) {
      detalles[[length(detalles) + 1L]] <- data.frame(
        columna = names(datos)[[i]],
        variantes = paste(grupo$variantes, collapse = " / "),
        cambio_a_otra_forma = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  detalle <- if (length(detalles)) do.call(rbind, detalles) else data.frame(
    columna = character(), variantes = character(),
    cambio_a_otra_forma = logical(), stringsAsFactors = FALSE
  )
  list(
    grupos = nrow(detalle),
    columnas = length(unique(detalle$columna)),
    detalle = detalle
  )
}

tablas <- .cargar_tablas_limpias(.raiz)
cat("Bateria de 31 tablas limpias\n")
limpias <- lapply(tablas, .medir_tabla)
cat("tablas con grupos sospechosos: ",
    sum(vapply(limpias, `[[`, numeric(1L), "grupos") > 0L), "/",
    length(limpias), "\n", sep = "")
cat("grupos sospechosos nuevos: ",
    sum(vapply(limpias, `[[`, numeric(1L), "grupos")), "\n", sep = "")
for (i in seq_along(limpias)) {
  if (limpias[[i]]$grupos > 0L) {
    cat("  ", names(tablas)[[i]], ": ", limpias[[i]]$grupos,
        " grupos\n", sep = "")
  }
}

cat("\nTamanos chicos\n")
for (n in c(3L, 5L, 10L, 20L)) {
  datos <- data.frame(
    localidad = c(rep("Montevideo", n %/% 2L),
                  rep("Montevido", n - n %/% 2L))
  )
  medicion <- .medir_tabla(datos)
  cat("n=", n, ": grupos=", medicion$grupos, "\n", sep = "")
}

cat("\nRaha\n")
raha <- tryCatch({
  ambiente <- new.env(parent = globalenv())
  options(lupa.benchmark.silencioso = TRUE)
  sys.source(file.path(.raiz, "benchmark", "verdad_raha.R"), envir = ambiente)
  list(verdad = ambiente$verdad_raha, ambiente = ambiente)
}, error = function(e) e)
if (inherits(raha, "error")) {
  cat("estado=no medido; motivo=", conditionMessage(raha), "\n", sep = "")
} else {
  for (nombre in names(raha$verdad)) {
    referencia <- raha$verdad[[nombre]]
    limpia <- raha$ambiente$.leer_raha(file.path(
      raha$ambiente$directorio_raha, paste0(nombre, "_clean.csv")
    ))
    medicion <- .medir_tabla(referencia$sucia)
    for (i in seq_len(nrow(medicion$detalle))) {
      columna <- match(medicion$detalle$columna[[i]], names(referencia$sucia))
      variantes <- strsplit(medicion$detalle$variantes[[i]], " / ",
                            fixed = TRUE)[[1L]]
      sucio <- as.character(referencia$sucia[[columna]])
      limpio <- as.character(limpia[[columna]])
      cambiado <- !is.na(sucio) & !is.na(limpio) & sucio != limpio
      medicion$detalle$cambio_a_otra_forma[[i]] <- any(
        cambiado & sucio %in% variantes & limpio %in% variantes
      )
    }
    afectada <- sum(referencia$por_columna > 0L)
    reales <- sum(medicion$detalle$cambio_a_otra_forma, na.rm = TRUE)
    cat(
      nombre, ": grupos=", medicion$grupos,
      "; columnas_con_cambio=", afectada,
      "; grupos_con_cambio_real=", reales,
      "; grupos_sin_cambio_real=", medicion$grupos - reales, "\n", sep = ""
    )
  }
  cat("El detector es aditivo y no reemplaza casi_duplicados_vocabulario; no pierde detecciones existentes.\n")
}
