## Mide la discriminacion de Jaro-Winkler y el costo por celdas de tablas
## anchas. Los resultados publicados dependen de la maquina y quedan en un CSV.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_topes_valores_ancho.R.",
    call. = FALSE
  )
}

.argumentos <- commandArgs(trailingOnly = FALSE)
.marca_archivo <- "--file="
.rutas_script <- .argumentos[startsWith(.argumentos, .marca_archivo)]
if (length(.rutas_script) != 1L) {
  stop("No se pudo determinar la ruta del script.", call. = FALSE)
}
.archivo_script <- sub(.marca_archivo, "", .rutas_script[[1L]], fixed = TRUE)
.raiz_paquete <- normalizePath(
  file.path(dirname(normalizePath(.archivo_script, mustWork = TRUE)), ".."),
  mustWork = TRUE
)
if (file.exists(file.path(.raiz_paquete, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.raiz_paquete, quiet = TRUE)
} else if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("No se encuentra la fuente ni una instalacion de lupa.", call. = FALSE)
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Instale stringdist antes de ejecutar este benchmark.", call. = FALSE)
}

.revision <- Sys.getenv("LUPA_REVISION", unset = "")
if (!nzchar(.revision)) {
  .archivos_r <- sort(list.files(
    file.path(.raiz_paquete, "R"), "[.]R$", full.names = TRUE
  ))
  .revision <- paste0(
    as.character(utils::packageVersion("lupa")), "; R/ md5 ",
    substr(paste(unname(tools::md5sum(.archivos_r)), collapse = ""), 1L, 12L)
  )
}
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.sello <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
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

.formales <- formals(lupa::detectar_duplicados_aproximados)
.p_medido <- eval(.formales[["p"]])
if (!is.numeric(.p_medido) || length(.p_medido) != 1L ||
    !is.finite(.p_medido)) {
  stop("No se pudo leer el p predeterminado desde el codigo de lupa.",
       call. = FALSE)
}

.fila <- function(escenario, medicion, unidad = NA_character_,
                  n_filas = NA_integer_, n_columnas = NA_integer_,
                  celdas = NA_real_, largo = NA_integer_,
                  cambios = NA_integer_, repeticion = NA_integer_,
                  segundos = NA_real_, celdas_por_segundo = NA_real_,
                  valor = NA_real_) {
  data.frame(
    entorno = "Rscript",
    as.data.frame(.entorno, stringsAsFactors = FALSE),
    escenario = escenario, medicion = medicion, unidad = unidad,
    n_filas = n_filas, n_columnas = n_columnas, celdas = celdas,
    largo = largo, cambios = cambios, repeticion = repeticion,
    p = .p_medido, segundos = segundos,
    celdas_por_segundo = celdas_por_segundo, valor = valor,
    stringsAsFactors = FALSE
  )
}

.par <- function(largo, cambios, semilla) {
  set.seed(semilla)
  base <- paste(sample(letters, largo, replace = TRUE), collapse = "")
  base_vector <- strsplit(base, "", fixed = TRUE)[[1L]]
  una <- base_vector
  una[[floor(largo / 2L)]] <- if (una[[floor(largo / 2L)]] == "a") "b" else "a"
  muchas <- base_vector
  posiciones <- sample(seq_len(largo), cambios)
  muchas[posiciones] <- ifelse(muchas[posiciones] == "a", "b", "a")
  c(
    una_diferencia = stringdist::stringdist(
      base, paste0(una, collapse = ""), method = "jw", p = .p_medido
    ),
    muchas_diferencias = stringdist::stringdist(
      base, paste0(muchas, collapse = ""), method = "jw", p = .p_medido
    )
  )
}

.mediciones <- list()
.agregar <- function(x) {
  .mediciones[[length(.mediciones) + 1L]] <<- x
}

cat("Discriminacion Jaro-Winkler; mediana de cinco semillas; p = ",
    .p_medido, ".\n", sep = "")
cat("largo\tuna_diferencia\t1000_diferencias\n")
for (largo in c(10000L, 20000L, 25000L, 50000L)) {
  resultados <- vapply(1:5, function(semilla) {
    .par(largo, 1000L, semilla)
  }, numeric(2L))
  mediana <- apply(resultados, 1L, median)
  cat(largo, "\t", format(mediana[[1L]], digits = 6), "\t",
      format(mediana[[2L]], digits = 6), "\n", sep = "")
  for (repeticion in seq_len(ncol(resultados))) {
    .agregar(.fila(
      "jaro_winkler", "una_diferencia", "distancia", largo = largo,
      cambios = 1L, repeticion = repeticion, valor = resultados[1L, repeticion]
    ))
    .agregar(.fila(
      "jaro_winkler", "muchas_diferencias", "distancia", largo = largo,
      cambios = 1000L, repeticion = repeticion, valor = resultados[2L, repeticion]
    ))
  }
}

.tabla_ancha <- function(n_filas, n_columnas) {
  valores <- paste0("valor_", seq_len(n_filas))
  as.data.frame(
    setNames(
      rep(list(valores), n_columnas), paste0("V", seq_len(n_columnas))
    ),
    stringsAsFactors = FALSE
  )
}

cat("\nCosto de tablas anchas; sin aviso interactivo; tres repeticiones.\n")
cat("filas\tcolumnas\tceldas\trepeticion\tsegundos\tceldas/segundo\n")
for (n_columnas in c(50L, 300L, 1000L)) {
  datos <- .tabla_ancha(500L, n_columnas)
  tiempos <- numeric(3L)
  for (repeticion in seq_along(tiempos)) {
    tiempos[[repeticion]] <- system.time(
      lupa::perfilar(
        datos, analizar_dependencias = FALSE,
        casi_duplicados_vocabulario = FALSE,
        avisar_costo_tabla_ancha = FALSE
      )
    )[["elapsed"]]
    celdas <- 500L * n_columnas
    velocidad <- celdas / tiempos[[repeticion]]
    cat(500L, "\t", n_columnas, "\t", celdas, "\t", repeticion, "\t",
        format(tiempos[[repeticion]], digits = 6), "\t",
        format(velocidad, digits = 8), "\n", sep = "")
    .agregar(.fila(
      "tabla_ancha", "perfilar", "celdas_por_segundo",
      n_filas = 500L, n_columnas = n_columnas, celdas = celdas,
      repeticion = repeticion, segundos = tiempos[[repeticion]],
      celdas_por_segundo = velocidad, valor = velocidad
    ))
  }
}

salida <- do.call(rbind, .mediciones)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(
  directorio_datos, paste0("medir-topes-valores-ancho-", .sello, ".csv")
)
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("Salida:", archivo_salida, "\n")
