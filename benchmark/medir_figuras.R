## Mide las cifras reproducibles que alimentan las figuras de lupa.
## Solo usa R base, utils, stats y el paquete lupa instalado.

if (sys.nframe() > 0L) {
  stop(
    paste0(
      "Este script no se ejecuta con source(). ",
      "Desde la raiz del repositorio use: ",
      "Rscript benchmark/medir_figuras.R"
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
      "Desde la raiz del repositorio use: ",
      "Rscript benchmark/medir_figuras.R"
    ),
    call. = FALSE
  )
}
ruta_script <- sub(marca_archivo, "", rutas_script, fixed = TRUE)
ruta_script <- gsub("~+~", " ", ruta_script, fixed = TRUE)
directorio_script <- dirname(normalizePath(ruta_script, mustWork = TRUE))
directorio_repositorio <- dirname(directorio_script)

args <- commandArgs(trailingOnly = TRUE)
tope_filas <- if (length(args)) suppressWarnings(as.integer(args[[1L]])) else 500000L
if (is.na(tope_filas) || tope_filas < 1L) {
  stop("tope_filas debe ser un entero positivo.", call. = FALSE)
}

solo_baratas <- identical(Sys.getenv("LUPA_FIGURAS_SOLO_BARATAS"), "1")
sin_hilos <- identical(Sys.getenv("LUPA_FIGURAS_SIN_HILOS"), "1")
hilos_benchmark <- 2L

# El commit que firma cada CSV, y la guarda que impide firmarlo con una
# instalacion que no lo contiene. Las dos cosas viven en `_firma.R` porque
# `perdida_lsh.R` firma sus CSV igual y la logica copiada se arregla en una
# copia sola.
source(file.path(directorio_script, "_firma.R"), local = TRUE)
rutas_firma <- c(.rutas_firma("medir_figuras.R"), "benchmark/_padron_sintetico.R")
commit_actual <- .commit_actual(directorio_repositorio, rutas_firma)

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}

descripcion_lupa <- utils::packageDescription("lupa")
version_lupa <- as.character(descripcion_lupa[["Version"]])
built_lupa <- descripcion_lupa[["Built"]]
built_lupa <- if (length(built_lupa) && !is.na(built_lupa) && nzchar(built_lupa)) {
  as.character(built_lupa)
} else {
  NA_character_
}
ruta_lupa <- tryCatch(find.package("lupa"), error = function(e) NA_character_)

cat("lupa medida: version ", version_lupa, "\n", sep = "")
cat("Built: ", built_lupa, "\n", sep = "")
cat("instalacion: ", ruta_lupa, "\n", sep = "")
if (is.na(built_lupa)) {
  stop(
    "La instalacion medida de lupa no tiene el sello Built.",
    call. = FALSE
  )
}

guarda_built <- .guarda_built(
  built_lupa, directorio_repositorio, rutas_firma, ruta_lupa, commit_actual
)
if (isTRUE(guarda_built)) {
  commit_actual <- paste0(commit_actual, "+singuarda")
}

ruta_stringdist <- system.file(package = "stringdist")
if (!length(ruta_stringdist) || is.na(ruta_stringdist) || !nzchar(ruta_stringdist)) {
  stop(
    paste0(
      "Este benchmark requiere la capacidad opcional stringdist de lupa. ",
      "Instale las dependencias sugeridas del paquete."
    ),
    call. = FALSE
  )
}

source(file.path(directorio_script, "_padron_sintetico.R"), local = TRUE)

# La medida final se toma de los valores por omision de la instalacion medida,
# no de una cifra escrita aca: si el paquete cambia el umbral o el peso del
# prefijo de Jaro-Winkler, el techo y el rotulo cambian con el.
formales_lupa <- formals(lupa::detectar_duplicados_aproximados)
metodo_medido <- eval(formales_lupa[["metodo"]])
umbral_medido <- eval(formales_lupa[["umbral"]])
p_medido <- if (is.null(formales_lupa[["p"]])) 0 else eval(formales_lupa[["p"]])
cat("medida final: ", metodo_medido, ", umbral ", umbral_medido, ", p ", p_medido,
    "\n", sep = "")

lib_medida <- dirname(normalizePath(ruta_lupa, mustWork = TRUE))
datos_dir <- file.path(directorio_script, "datos")
dir.create(datos_dir, recursive = TRUE, showWarnings = FALSE)

rss_mib <- function() {
  z <- tryCatch(
    readLines("/proc/self/status", warn = FALSE),
    error = function(e) character()
  )
  z <- grep("^VmHWM:", z, value = TRUE)
  if (!length(z)) return(NA_real_)
  as.numeric(gsub("[^0-9]", "", z[[1L]])) / 1024
}
if (is.na(rss_mib())) {
  message(
    "Sin /proc/self/status: la memoria (rss_mib) quedara NA en los CSV y la ",
    "figura de escala no dibujara el panel de memoria."
  )
}

cpu_model <- function() {
  z <- tryCatch(
    readLines("/proc/cpuinfo", warn = FALSE),
    error = function(e) character()
  )
  z <- grep("^model name[[:space:]]*:", z, value = TRUE)
  if (!length(z)) return(NA_character_)
  sub("^model name[[:space:]]*:[[:space:]]*", "", z[[1L]])
}

ram_gib <- function() {
  z <- tryCatch(
    readLines("/proc/meminfo", warn = FALSE),
    error = function(e) character()
  )
  z <- grep("^MemTotal:", z, value = TRUE)
  if (!length(z)) return(NA_real_)
  partes <- strsplit(trimws(z[[1L]]), "[[:space:]]+")[[1L]]
  if (length(partes) < 2L) return(NA_real_)
  as.numeric(partes[[2L]]) / 1024^2
}

fmt <- function(x) format(
  x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE
)

seg <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  v <- force(expr)
  list(v = v, t = proc.time()[["elapsed"]] - t0)
}

escribir_csv <- function(nombre, tabla) {
  tabla <- as.data.frame(tabla, stringsAsFactors = FALSE)
  tabla$commit <- rep(commit_actual, nrow(tabla))
  utils::write.csv(
    tabla,
    file = file.path(datos_dir, nombre),
    row.names = FALSE
  )
}

entorno <- data.frame(
  fecha = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE),
  version_lupa = version_lupa,
  built = built_lupa,
  # De donde salio la instalacion que se midio. El CSV traia el sello `Built`
  # pero no decia de que biblioteca venia, y con dos instalaciones en la maquina
  # -la del usuario y la del banco- esa es justo la diferencia que importa. Se
  # publica con el hogar abreviado: identifica la biblioteca sin publicar el
  # nombre de la cuenta.
  instalacion = sub(paste0("^", path.expand("~")), "~", lib_medida),
  r_version = R.version.string,
  cpu = cpu_model(),
  nucleos_disponibles = parallel::detectCores(),
  ram_gib = ram_gib(),
  hilos_stringdist = hilos_benchmark,
  metodo = metodo_medido,
  umbral = umbral_medido,
  p_jw = p_medido,
  stringsAsFactors = FALSE
)
escribir_csv("entorno.csv", entorno)

cat("commit: ", commit_actual, "\n", sep = "")
cat("R: ", R.version.string, "\n", sep = "")
cat("hilos stringdist: ", hilos_benchmark, "\n\n", sep = "")

medir_escala_tres <- function(datos, techo, lib, nucleos = hilos_benchmark) {
  # Cada corrida vive en un proceso R independiente para aislar el RSS. El hijo
  # hereda las bibliotecas del padre (con la medida primero) y comprueba que
  # carga la misma instalacion de lupa que el padre midio y que stringdist
  # esta: si no, sale con un codigo propio y el padre lo informa.
  # Se normaliza ANTES de pasarselo al hijo, que lo compara como literal: con
  # una barra final, la comparacion fallaria sobre la MISMA instalacion y el
  # mensaje acusaria un cambio de biblioteca que no ocurrio. Con esta linea ese
  # falso positivo no puede pasar; sin ella, pasaba.
  lib <- normalizePath(lib, mustWork = TRUE)
  script <- file.path(tempdir(), "lupa-benchmark-escala.R")
  writeLines(c(
    "a <- commandArgs(TRUE)",
    ".libPaths(c(strsplit(a[[1L]], .Platform$path.sep, fixed = TRUE)[[1L]], .libPaths()))",
    "if (!requireNamespace('stringdist', quietly = TRUE)) quit(status = 3L)",
    "if (!identical(dirname(normalizePath(find.package('lupa'), mustWork = TRUE)), a[[6L]])) quit(status = 4L)",
    "suppressMessages(library(lupa))",
    "datos <- readRDS(a[[2L]])",
    "techo <- readRDS(a[[3L]])",
    "nucleos <- as.integer(a[[5L]])",
    "rss_mib <- function() { z <- tryCatch(readLines('/proc/self/status', warn = FALSE), error = function(e) character()); z <- grep('^VmHWM:', z, value = TRUE); if (!length(z)) return(NA_real_); as.numeric(gsub('[^0-9]', '', z[[1L]])) / 1024 }",
    "t0 <- proc.time()[['elapsed']]",
    "z <- suppressMessages(lupa::detectar_duplicados_aproximados(datos, columnas = 'v', estrategia = 'lsh', max_resultados = Inf, nucleos = nucleos))",
    "t <- proc.time()[['elapsed']] - t0",
    "k <- paste(z$pares$fila_1, z$pares$fila_2)",
    "rec <- if (nrow(techo)) mean(paste(techo$fila_1, techo$fila_2) %in% k) else NA_real_",
    "saveRDS(list(t = t, cand = z$alcance$lsh_candidatos_unicos, hallados = nrow(z$pares), rec = rec, rss = rss_mib(), prev = z$estimacion$candidatos_previstos), a[[4L]])"
  ), script)
  datos_file <- tempfile("lupa-bench-datos-", fileext = ".rds")
  techo_file <- tempfile("lupa-bench-techo-", fileext = ".rds")
  out_file <- tempfile("lupa-bench-salida-", fileext = ".rds")
  on.exit(unlink(c(script, datos_file, techo_file, out_file)), add = TRUE)
  saveRDS(datos, datos_file)
  saveRDS(techo, techo_file)
  rscript <- file.path(R.home("bin"), "Rscript")
  bibliotecas <- paste(unique(c(lib, .libPaths())), collapse = .Platform$path.sep)
  corridas <- lapply(seq_len(3L), function(i) {
    unlink(out_file)
    estado <- system2(
      rscript,
      c("--vanilla", shQuote(script), shQuote(bibliotecas), shQuote(datos_file),
        shQuote(techo_file), shQuote(out_file), as.character(nucleos),
        shQuote(lib)),
      stdout = FALSE,
      stderr = ""
    )
    if (!identical(estado, 0L)) {
      motivo <- switch(
        as.character(estado),
        "3" = "el proceso hijo no encuentra stringdist",
        "4" = "el proceso hijo carga otra instalacion de lupa que la medida",
        paste0("el proceso hijo termino con estado ", estado)
      )
      stop("fallo una corrida del benchmark de escala: ", motivo, call. = FALSE)
    }
    readRDS(out_file)
  })
  # Los conteos son deterministas: si cambian entre corridas, el hijo no midio
  # lo mismo tres veces y las medianas de tiempo no describen una configuracion.
  # `prev` entra: es la estimacion previa que alimenta `error_estimacion`, se
  # toma de la corrida 1 y sin comprobarla el CSV describiria una sola de las
  # tres si el paquete dejara de ser determinista ahi.
  for (campo in c("cand", "hallados", "rec", "prev")) {
    valores <- vapply(corridas, function(x) as.numeric(x[[campo]]), numeric(1L))
    if (length(unique(valores[!is.na(valores)])) > 1L) {
      stop(
        "las corridas de una misma configuracion no coinciden en ", campo, ": ",
        paste(valores, collapse = ", "),
        call. = FALSE
      )
    }
  }
  tiempos <- vapply(corridas, function(x) x[["t"]], numeric(1L))
  list(v = corridas[[1L]], tiempos = tiempos, t = stats::median(tiempos))
}

# --------------------------------------------------------------- 1. escala
cat(sprintf("== 1. escala sobre padron sintetico (LSH, %s, umbral %s, p %s) ==\n",
            metodo_medido, fmt(umbral_medido), fmt(p_medido)))
cat(sprintf("%10s %16s %10s %10s %10s %8s %6s\n",
            "filas", "candidatos", "tiempo_s", "RSS_MiB", "hallados", "recall", "hilos"))
tamanos <- c(20000L, 100000L, 200000L, 500000L)
if (tope_filas >= 1000000L) tamanos <- c(tamanos, 1000000L)
tamanos <- tamanos[tamanos <= tope_filas]
if (solo_baratas) tamanos <- tamanos[tamanos <= 20000L]
escala <- list()
for (n in tamanos) {
  p <- padron(n)
  te <- techo(p, umbral_medido, metodo_medido, p_medido)
  r <- medir_escala_tres(p$datos, te, lib_medida, hilos_benchmark)
  cat(sprintf("%10s %16s %10.1f %10.0f %10s %8.4f %6d\n",
      fmt(n), fmt(r$v$cand), r$t, r$v$rss, fmt(r$v$hallados), r$v$rec,
      hilos_benchmark))
  cat(sprintf("  corridas de tiempo: %s\n",
      paste(format(r$tiempos, digits = 5, decimal.mark = ","), collapse = ", ")))
  escala[[length(escala) + 1L]] <- data.frame(
    filas = n,
    candidatos = as.numeric(r$v$cand),
    previstos = as.numeric(r$v$prev),
    error_estimacion = if (is.finite(r$v$prev) && r$v$cand) {
      r$v$prev / r$v$cand - 1
    } else NA_real_,
    pares_informados = as.numeric(r$v$hallados),
    recall_techo = as.numeric(r$v$rec),
    tiempo_s_1 = r$tiempos[[1L]],
    tiempo_s_2 = r$tiempos[[2L]],
    tiempo_s_3 = r$tiempos[[3L]],
    tiempo_s_mediana = r$t,
    rss_mib = as.numeric(r$v$rss),
    hilos = hilos_benchmark,
    stringsAsFactors = FALSE
  )
  rm(p, te, r)
  invisible(gc())
}
if (length(escala)) {
  escala_df <- do.call(rbind, escala)
} else {
  escala_df <- data.frame(
    filas = integer(), candidatos = numeric(), previstos = numeric(),
    error_estimacion = numeric(), pares_informados = numeric(),
    recall_techo = numeric(), tiempo_s_1 = numeric(), tiempo_s_2 = numeric(),
    tiempo_s_3 = numeric(), tiempo_s_mediana = numeric(), rss_mib = numeric(),
    hilos = integer(), stringsAsFactors = FALSE
  )
}
escribir_csv("escala.csv", escala_df)

# ------------------------------------------------ 2. precision de estimacion
cat("\n== 2. estimacion previa de candidatos contra el conteo real ==\n")
cat(sprintf("%10s %16s %16s %9s\n", "filas", "previstos", "reales", "error"))
for (z in escala) {
  if (is.finite(z$previstos)) cat(sprintf(
    "%10s %16s %16s %+8.2f %%\n",
    fmt(z$filas), fmt(round(z$previstos)), fmt(z$candidatos),
    100 * z$error_estimacion
  ))
}

# ------------------------------------------------------------ 3. cardinalidad
cat("\n== 3. la cardinalidad cambia la constante, no el orden (20.000 filas) ==\n")
reloj_cardinalidad <- proc.time()[["elapsed"]]
descripciones <- c(
  alta = "vocabulario amplio + nro.",
  media = "padron sintetico",
  baja = "mitad con el mismo valor"
)
cardinalidad <- list()
base_cand <- NA_real_
cat(sprintf("%-34s %16s %12s %8s %6s\n", "nivel", "candidatos", "por fila", "veces", "hilos"))
for (nv in c("alta", "media", "baja")) {
  d <- gen_cardinalidad(20000L, nv)
  r <- suppressMessages(lupa::detectar_duplicados_aproximados(
    d,
    columnas = "v",
    estrategia = "lsh",
    max_resultados = 5L,
    lsh_muestra_estimacion = 1L,
    nucleos = hilos_benchmark
  ))
  cc <- as.numeric(r$alcance$lsh_candidatos_unicos)
  if (nv == "alta") base_cand <- cc
  veces <- cc / base_cand
  cat(sprintf("%-34s %16s %12.1f %7.1fx %6d\n",
    switch(nv, alta = "alta  (vocabulario amplio + nro.)",
               media = "media (padron sintetico)",
               baja = "baja  (mitad con el mismo valor)"),
    fmt(cc), cc / 20000, veces, hilos_benchmark))
  cardinalidad[[length(cardinalidad) + 1L]] <- data.frame(
    nivel = nv,
    descripcion = unname(descripciones[[nv]]),
    filas = 20000L,
    candidatos = cc,
    candidatos_por_fila = cc / 20000,
    veces_respecto_alta = veces,
    stringsAsFactors = FALSE
  )
}
cat(sprintf("  tiempo de la seccion: %.2f s\n",
            proc.time()[["elapsed"]] - reloj_cardinalidad))
escribir_csv("cardinalidad.csv", do.call(rbind, cardinalidad))

# --------------------------------------------------- 4. presupuesto y bloqueo
cat("\n== 4. el presupuesto corta antes de comparar ==\n")
set.seed(3)
n <- 4000L
d <- data.frame(
  v = paste(sample(c("Ana", "Luis", "Jose"), n, TRUE),
            sample(c("Perez", "Sosa"), n, TRUE),
            sample(100:999, n, TRUE)),
  anio = sample(2019:2024, n, TRUE),
  stringsAsFactors = FALSE
)
tc <- seg(tryCatch(
  lupa::detectar_duplicados_aproximados(
    d, columnas = "v", max_resultados = 5L,
    presupuesto_pares = 100000, nucleos = hilos_benchmark
  ),
  error = function(e) "corta"
))
tf <- seg(suppressMessages(lupa::detectar_duplicados_aproximados(
  d, columnas = "v", max_resultados = 5L, nucleos = hilos_benchmark
)))
pares_posibles <- as.numeric(tf$v$alcance$n_pares_comparados)
cat(sprintf("  4.000 filas, %s pares posibles (%d hilos)\n",
            fmt(pares_posibles), hilos_benchmark))
cat(sprintf("  con presupuesto 100.000: corta en %.2f s | corrida completa: %.2f s  (%.0fx)\n",
            tc$t, tf$t, tf$t / tc$t))
presupuesto <- data.frame(
  filas = n,
  pares_posibles = pares_posibles,
  presupuesto_pares = 100000,
  tiempo_con_presupuesto_s = tc$t,
  tiempo_completo_s = tf$t,
  stringsAsFactors = FALSE
)
escribir_csv("presupuesto.csv", presupuesto)

cat("\n== 5. el bloqueo estima lo que pierde ==\n")
reloj_bloqueo <- proc.time()[["elapsed"]]
sin <- suppressMessages(lupa::detectar_duplicados_aproximados(
  d, columnas = "v", max_resultados = Inf, nucleos = hilos_benchmark
))
con <- suppressMessages(lupa::detectar_duplicados_aproximados(
  d, columnas = "v", bloquear_por = "anio", max_resultados = Inf,
  nucleos = hilos_benchmark
))
real <- nrow(sin$pares) - nrow(con$pares)
est <- as.numeric(con$alcance$bloqueo_candidatos_perdidos_estimados[[1L]])
# Sin perdida real el error relativo no esta definido: queda NA, no Inf.
error_bloqueo <- if (is.finite(est) && real > 0) est / real - 1 else NA_real_
cat(sprintf("  informados sin bloqueo %s | con bloqueo %s (%d hilos)\n",
    fmt(nrow(sin$pares)), fmt(nrow(con$pares)), hilos_benchmark))
cat(sprintf("  perdidos reales %s | estimados %s | error %s\n",
    fmt(real), fmt(round(est)),
    if (is.na(error_bloqueo)) "NA" else sprintf("%+.2f %%", 100 * error_bloqueo)))
bloqueo <- data.frame(
  filas = n,
  clave = "anio",
  pares_sin_bloqueo = as.numeric(nrow(sin$pares)),
  pares_con_bloqueo = as.numeric(nrow(con$pares)),
  perdidos_reales = as.numeric(real),
  perdidos_estimados = est,
  error_estimacion = error_bloqueo,
  stringsAsFactors = FALSE
)
cat(sprintf("  tiempo de la seccion: %.2f s\n",
            proc.time()[["elapsed"]] - reloj_bloqueo))
escribir_csv("bloqueo.csv", bloqueo)

cat("\n== 6. el loteo no pierde pares ==\n")
lo <- seg(suppressMessages(lupa::detectar_duplicados_aproximados(
  d, columnas = "v", max_resultados = Inf, lotes = TRUE,
  nucleos = hilos_benchmark
)))
pares_identicos <- identical(sin$pares, lo$v$pares)
cat(sprintf("  pares identicos al recorrido sin lotes: %s | parciales: %d | %.2f s\n",
    pares_identicos, lo$v$lotes$n_parciales, lo$t))
loteo <- data.frame(
  filas = n,
  pares_identicos = pares_identicos,
  n_parciales = as.numeric(lo$v$lotes$n_parciales),
  tiempo_s = lo$t,
  stringsAsFactors = FALSE
)
escribir_csv("loteo.csv", loteo)

# --------------------------------------------- 10.bis. costo de los hilos
if (solo_baratas || sin_hilos) {
  cat("\n== 10.bis. costo de los hilos: salteado ==\n")
} else {
  cat("\n== 10.bis. costo de los hilos (100.000 filas) ==\n")
  nucleos_disponibles <- parallel::detectCores()
  configuraciones_hilos <- c(2L, 4L, 8L, 16L)
  if (length(nucleos_disponibles) && is.finite(nucleos_disponibles) &&
      nucleos_disponibles > 1L) {
    configuraciones_hilos <- c(configuraciones_hilos, nucleos_disponibles - 1L)
  }
  configuraciones_hilos <- unique(configuraciones_hilos)
  p_hilos <- padron(100000L)
  te_hilos <- techo(p_hilos, umbral_medido, metodo_medido, p_medido)
  hilos <- lapply(configuraciones_hilos, function(nucleos) {
    r <- medir_escala_tres(p_hilos$datos, te_hilos, lib_medida, nucleos)
    cat(sprintf("  %2d hilos: mediana %.2f s | candidatos %s | pares %s\n",
      nucleos, r$t, fmt(r$v$cand), fmt(r$v$hallados)))
    data.frame(
      filas = 100000L,
      nucleos = nucleos,
      tiempo_s_1 = r$tiempos[[1L]],
      tiempo_s_2 = r$tiempos[[2L]],
      tiempo_s_3 = r$tiempos[[3L]],
      tiempo_s_mediana = r$t,
      candidatos = as.numeric(r$v$cand),
      pares_informados = as.numeric(r$v$hallados),
      stringsAsFactors = FALSE
    )
  })
  escribir_csv("hilos.csv", do.call(rbind, hilos))
  rm(p_hilos, te_hilos, hilos)
  invisible(gc())
}

cat("\n== fin ==\n")
