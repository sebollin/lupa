# Diagnostico: por que perfilar() es unas 8 veces mas lento en Windows que en
# Linux, cuando el resto del check lo es unas 4. Corre en los dos sistemas a la par.
suppressMessages(library(lupa))
d <- datos_administrativos
cat("R:", R.version.string, "| lupa:", as.character(packageVersion("lupa")),
    "| sistema:", Sys.info()[["sysname"]], "| CTYPE:", Sys.getlocale("LC_CTYPE"), "\n")
mediana <- function(f, n = 5) {
  median(vapply(seq_len(n), function(i) {
    system.time(suppressMessages(suppressWarnings(f())))[["elapsed"]]
  }, 0))
}
invisible(suppressMessages(perfilar(d)))
rscript <- file.path(R.home("bin"), "Rscript")
arranque <- system.time(system2(rscript, c("-e", shQuote("library(lupa)")), stdout = FALSE))
cat(sprintf("%-38s %8.3f s\n", "Rscript -e library(lupa)", arranque[["elapsed"]]))
r <- c(
  perfilar = mediana(function() perfilar(d)),
  perfilar_sin_proteccion = mediana(function() perfilar(d, proteger_datos_personales = FALSE)),
  perfilar_sin_dependencias = mediana(function() perfilar(d, analizar_dependencias = FALSE)),
  analizar = mediana(function() analizar(d), 3),
  reportar = mediana(function() reportar(perfilar(d), archivo = tempfile(fileext = ".html")), 3)
)
for (k in names(r)) cat(sprintf("%-38s %8.3f s\n", k, r[[k]]))

set.seed(1)
x <- rep(c("José Pérez", "MONTEVIDEO", "2024-01-31", "123.456-7", "ñandú"), 2000)
patron <- lupa:::.PATRON_NUMERICO_DE_ESTRUCTURA
primitivas <- list(
  grepl_tre_patron_largo_x2000 = function() for (i in 1:2000) grepl(patron, "minimo"),
  grepl_perl_utf8 = function() grepl("[^\\p{L}\\p{N}]", x, perl = TRUE),
  gsub_perl_utf8 = function() gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE),
  gsub_tre_utf8 = function() gsub("[[:space:]]+", " ", x),
  grepl_fixed = function() grepl("e", x, fixed = TRUE),
  chartr_utf8 = function() chartr("áéíóúñ", "aeioun", x),
  tolower_utf8 = function() tolower(x),
  strptime_x10000 = function() strptime(rep("2024-01-31", 10000), "%Y-%m-%d", tz = "UTC"),
  iconv_latin1 = function() iconv(x, "UTF-8", "latin1"),
  enc2utf8 = function() enc2utf8(x),
  validUTF8 = function() validUTF8(x),
  utf8ToInt_x2000 = function() for (i in 1:2000) utf8ToInt("José"),
  format_numeros = function() format(runif(10000)),
  do_call_data_frame = function() do.call(data.frame, list(a = 1:2000, b = rep(letters, length.out = 2000))),
  Sys_time_x10000 = function() for (i in 1:10000) Sys.time(),
  proc_time_x10000 = function() for (i in 1:10000) proc.time(),
  Sys_getlocale_x2000 = function() for (i in 1:2000) Sys.getlocale("LC_COLLATE"),
  Sys_setlocale_x500 = function() for (i in 1:500) Sys.setlocale("LC_COLLATE", Sys.getlocale("LC_COLLATE")),
  options_x10000 = function() for (i in 1:10000) options(lupa.prueba = i),
  environment_x10000 = function() for (i in 1:10000) new.env()
)
cat("\n-- primitivas (mediana de 3, ms)\n")
for (k in names(primitivas)) cat(sprintf("%-38s %10.2f\n", k, 1000 * mediana(primitivas[[k]], 3)))

archivo <- tempfile()
Rprof(archivo, interval = 0.01)
for (i in 1:10) invisible(suppressMessages(perfilar(d)))
Rprof(NULL)
p <- summaryRprof(archivo)
cat("\n-- Rprof sobre 10 perfilar(): total muestreado", p$sampling.time, "s\n")
tt <- p$by.total
print(head(tt[grepl('^"\\.', rownames(tt)), c("total.time", "total.pct", "self.pct")], 30))
cat("\n-- tiempo propio\n")
print(head(p$by.self, 25))
