## Mide `perfilar()` sobre tres tablas de texto numerico de 200.000 filas
## -decimales con coma, importes con moneda y una mezcla de listas, codigos e
## importes- y sobre `datos_operativos`. Es la medicion que citan las entradas
## de rendimiento de NEWS.md: se corre una vez por version, cada una instalada
## en su propia biblioteca, alternando, y las cifras se comparan por pares.
##
##   R_LIBS=<biblioteca> Rscript benchmark/medir_texto_numerico.R <etiqueta> [corrida]
##
## Agrega una fila por tabla a benchmark/datos/medir-texto-numerico.csv.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_texto_numerico.R.",
    call. = FALSE
  )
}
argumentos <- commandArgs(TRUE)
if (!length(argumentos)) {
  stop("Uso: Rscript benchmark/medir_texto_numerico.R <etiqueta> [corrida]",
       call. = FALSE)
}
etiqueta <- argumentos[[1L]]
corrida <- if (length(argumentos) > 1L) as.integer(argumentos[[2L]]) else 1L
if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("La medicion se hace en una sesion UTF-8.", call. = FALSE)
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n", sep = "")
cat("desde:  ", dirname(dirname(attr(.descripcion, "file"))), "\n\n", sep = "")

formato_regional <- function(v, decimales = 2L) {
  formatC(v, format = "f", digits = decimales, big.mark = ".",
          decimal.mark = ",")
}

tabla_decimales <- function(n) {
  salida <- lapply(seq_len(10L), function(j) {
    entero <- sample.int(10L^(1L + j %% 4L), n, replace = TRUE) - 1L
    decimal <- sample.int(99L, n, replace = TRUE)
    sprintf("%d,%d", entero, decimal)
  })
  names(salida) <- sprintf("dec_%02d", seq_len(10L))
  as.data.frame(salida, stringsAsFactors = FALSE)
}

tabla_importes <- function(n) {
  euro <- "\u20ac"
  envoltorios <- list(
    c("$ ", ""), c("USD ", ""), c("", " UYU"), c("U$S ", ""),
    c("", paste0(" ", euro)), c("$", ""), c("", " EUR"),
    c(paste0(euro, " "), ""), c("", ""), c("ARS ", "")
  )
  salida <- lapply(seq_len(10L), function(j) {
    v <- round(stats::runif(n, 0, 10^(3 + j %% 4)), 2)
    paste0(envoltorios[[j]][1L], formato_regional(v), envoltorios[[j]][2L])
  })
  names(salida) <- sprintf("imp_%02d", seq_len(10L))
  as.data.frame(salida, stringsAsFactors = FALSE)
}

tabla_mezcla <- function(n) {
  lista_numeros <- function(n, k) {
    vapply(seq_len(n), function(i) {
      paste(sample(10:99, k[i]), collapse = ", ")
    }, character(1L))
  }
  k <- ifelse(stats::runif(n) < 0.3, sample(2:4, n, replace = TRUE), 1L)
  col_listas <- lista_numeros(n, k)
  k2 <- ifelse(stats::runif(n) < 0.25, 2L, 1L)
  alnum <- function(m) paste0(sample(LETTERS, m, TRUE), sample(1:9, m, TRUE))
  col_alnum <- vapply(k2, function(m) paste(alnum(m), collapse = ";"),
                      character(1L))
  k3 <- ifelse(stats::runif(n) < 0.2, 2L, 1L)
  col_tubos <- vapply(k3, function(m) {
    paste(sample(1000:9999, m), collapse = "|")
  }, character(1L))
  col_decimal <- sprintf("%d,%d", sample.int(100L, n, TRUE),
                         sample.int(9L, n, TRUE))
  col_mixta <- ifelse(stats::runif(n) < 0.5, col_decimal,
                      lista_numeros(n, rep(2L, n)))
  col_codigos <- sprintf("AB-%03d", sample.int(999L, n, TRUE))
  col_nombres <- ifelse(stats::runif(n) < 0.4,
                        paste(sample(c("Ana", "Luis", "Eva"), n, TRUE),
                              sample(c("Sosa", "Rey", "Paz"), n, TRUE),
                              sep = ", "),
                        sample(c("Ana", "Luis", "Eva"), n, TRUE))
  col_ids <- ifelse(stats::runif(n) < 0.15,
                    paste(sprintf("2023-%03d", sample.int(999L, n, TRUE)),
                          sprintf("2024-%03d", sample.int(999L, n, TRUE)),
                          sep = "; "),
                    sprintf("2023-%03d", sample.int(999L, n, TRUE)))
  col_importe <- paste0("$ ", formato_regional(stats::runif(n, 0, 5000)))
  col_vacios <- ifelse(stats::runif(n) < 0.3, NA_character_,
                       ifelse(stats::runif(n) < 0.5, "",
                              lista_numeros(n, sample(1:2, n, TRUE))))
  data.frame(
    listas = col_listas, alnum = col_alnum, tubos = col_tubos,
    decimal = col_decimal, mixta = col_mixta, codigos = col_codigos,
    nombres = col_nombres, ids = col_ids, importe = col_importe,
    vacios = col_vacios, stringsAsFactors = FALSE
  )
}

tablas <- list(
  decimales = function() { set.seed(101L); tabla_decimales(200000L) },
  importes = function() { set.seed(101L); tabla_importes(200000L) },
  mezcla = function() { set.seed(101L); tabla_mezcla(200000L) }
)

fecha <- as.POSIXct("2026-01-15", tz = "UTC")
filas <- list()
for (nombre in names(tablas)) {
  datos <- tablas[[nombre]]()
  invisible(gc())
  segundos <- system.time(
    lupa::perfilar(datos, fecha = fecha)
  )[["elapsed"]]
  cat(sprintf("%-10s %8.1f s\n", nombre, segundos))
  filas[[nombre]] <- data.frame(tabla = nombre, filas = nrow(datos),
                                segundos = segundos)
}
## El costo fijo: 40 llamadas sobre la tabla de ejemplo, por llamada.
invisible(lupa::perfilar(lupa::datos_operativos, fecha = fecha))
segundos <- system.time(
  for (i in seq_len(40L)) lupa::perfilar(lupa::datos_operativos, fecha = fecha)
)[["elapsed"]] / 40
cat(sprintf("%-10s %8.3f s por llamada\n", "operativos", segundos))
filas$operativos <- data.frame(tabla = "datos_operativos",
                               filas = nrow(lupa::datos_operativos),
                               segundos = segundos)

salida <- do.call(rbind, filas)
salida <- cbind(
  etiqueta = etiqueta, corrida = corrida,
  version = as.character(utils::packageVersion("lupa")),
  armado = .descripcion$Built, salida,
  r = paste(R.version$major, R.version$minor, sep = "."),
  sistema = Sys.info()[["sysname"]],
  medido = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  row.names = NULL
)
destino <- file.path("benchmark", "datos", "medir-texto-numerico.csv")
utils::write.table(salida, destino, sep = ",", row.names = FALSE,
                   col.names = !file.exists(destino), append = file.exists(destino),
                   qmethod = "double")
cat("\nagregado a ", destino, "\n", sep = "")
