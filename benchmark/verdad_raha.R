## Construye la referencia dirty/clean de Raha a nivel de celda.
## Sólo usa R base y utils. Los CSV viven en un directorio temporal.

datasets_raha <- c("hospital", "flights", "beers")
base_raha <- paste0(
  "https://raw.githubusercontent.com/BigDaMa/raha/master/datasets/"
)

.huella_adler32 <- function(ruta) {
  conexion <- file(ruta, open = "rb")
  on.exit(close(conexion), add = TRUE)
  a <- 1
  b <- 0
  repeat {
    bloque <- readBin(conexion, what = "raw", n = 5552L)
    if (!length(bloque)) break
    for (valor in as.integer(bloque)) {
      a <- (a + valor) %% 65521
      b <- (b + a) %% 65521
    }
  }
  sprintf("%04x%04x", b, a)
}

.leer_raha <- function(ruta) {
  read.csv(
    ruta, colClasses = "character", na.strings = NULL,
    check.names = FALSE
  )
}

.obtener_archivo_raha <- function(dataset, estado, directorio) {
  nombre <- paste0(dataset, "_", estado, ".csv")
  origen_local <- Sys.getenv("RAHA_DATA_DIR", unset = "")
  url <- paste0(base_raha, dataset, "/", estado, ".csv")
  destino <- file.path(directorio, nombre)

  if (nzchar(origen_local)) {
    ruta <- file.path(origen_local, nombre)
    if (!file.exists(ruta)) {
      stop("No existe el archivo local: ", ruta, call. = FALSE)
    }
    ok <- file.copy(ruta, destino, overwrite = TRUE)
    if (!ok) stop("No se pudo copiar ", ruta, call. = FALSE)
  } else {
    utils::download.file(url, destino, mode = "wb", quiet = TRUE)
  }

  info <- file.info(destino)
  list(
    ruta = destino,
    version = data.frame(
      dataset = dataset,
      archivo = paste0(estado, ".csv"),
      url = url,
      bytes = as.numeric(info$size),
      adler32 = .huella_adler32(destino),
      stringsAsFactors = FALSE
    )
  )
}

.comparar_raha <- function(dataset, directorio) {
  archivo_sucio <- .obtener_archivo_raha(dataset, "dirty", directorio)
  archivo_limpio <- .obtener_archivo_raha(dataset, "clean", directorio)
  sucia <- .leer_raha(archivo_sucio$ruta)
  limpia <- .leer_raha(archivo_limpio$ruta)

  if (!identical(dim(sucia), dim(limpia))) {
    stop(dataset, ": dirty y clean tienen dimensiones distintas.", call. = FALSE)
  }
  ## Raha conserva las mismas posiciones, aunque algún encabezado también
  ## haya sido limpiado. La unidad publicada aquí es la celda de datos.
  names(limpia) <- names(sucia)

  matriz_sucia <- as.matrix(sucia)
  matriz_limpia <- as.matrix(limpia)
  distintas <- matriz_sucia != matriz_limpia
  faltante_sucio <- is.na(matriz_sucia)
  faltante_limpio <- is.na(matriz_limpia)
  distintas[is.na(distintas)] <-
    (faltante_sucio != faltante_limpio)[is.na(distintas)]
  por_columna <- colSums(distintas)

  list(
    dataset = dataset,
    dimension = dim(sucia),
    celdas_diferentes = sum(distintas),
    filas_afectadas = sum(rowSums(distintas) > 0L),
    tasa = sum(distintas) / length(distintas),
    columnas_afectadas = names(por_columna)[por_columna > 0L],
    por_columna = por_columna,
    duplicados_sucio = sum(duplicated(sucia)),
    vacias_sucio_con_limpio = sum(
      matriz_sucia == "" & matriz_limpia != "", na.rm = TRUE
    ),
    sucia = sucia,
    versiones = rbind(archivo_sucio$version, archivo_limpio$version)
  )
}

directorio_raha <- tempfile("lupa-raha-")
dir.create(directorio_raha)

verdad_raha <- setNames(lapply(
  datasets_raha, .comparar_raha, directorio = directorio_raha
), datasets_raha)

resumen_raha <- do.call(rbind, lapply(verdad_raha, function(x) {
  data.frame(
    dataset = x$dataset,
    filas = x$dimension[[1L]],
    columnas = x$dimension[[2L]],
    celdas_diferentes = x$celdas_diferentes,
    filas_afectadas = x$filas_afectadas,
    tasa = x$tasa,
    columnas_afectadas = length(x$columnas_afectadas),
    duplicados_sucio = x$duplicados_sucio,
    vacias_sucio_con_limpio = x$vacias_sucio_con_limpio,
    stringsAsFactors = FALSE
  )
}))
versiones_raha <- do.call(rbind, lapply(verdad_raha, `[[`, "versiones"))
rownames(resumen_raha) <- NULL
rownames(versiones_raha) <- NULL

if (!isTRUE(getOption("lupa.benchmark.silencioso"))) {
  cat("Raha: comparacion celda a celda de dirty contra clean\n")
  for (i in seq_len(nrow(resumen_raha))) {
    x <- resumen_raha[i, ]
    cat(sprintf(
      "%-9s %4d x %2d  celdas dif.: %4d  filas afect.: %4d  tasa: %6.3f %%\n",
      x$dataset, x$filas, x$columnas, x$celdas_diferentes,
      x$filas_afectadas, 100 * x$tasa
    ))
    por_columna <- verdad_raha[[as.character(x$dataset)]]$por_columna
    cat("  columnas con diferencias: ", paste(
      paste(names(por_columna)[por_columna > 0L],
            por_columna[por_columna > 0L], sep = "="),
      collapse = ", "
    ), "\n", sep = "")
    cat("  vacias en dirty con valor en clean: ",
        x$vacias_sucio_con_limpio, "; duplicados exactos en dirty: ",
        x$duplicados_sucio, "\n", sep = "")
  }
  cat("\nVersion de los archivos obtenidos (bytes y Adler-32):\n")
  for (i in seq_len(nrow(versiones_raha))) {
    x <- versiones_raha[i, ]
    cat(sprintf(
      "%-9s %-9s %6d bytes  %s\n",
      x$dataset, x$archivo, x$bytes, x$adler32
    ))
  }
}
