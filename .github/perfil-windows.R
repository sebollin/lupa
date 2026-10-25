# Diagnostico 2: el costo de cada grepl(perl = TRUE) en Windows. La primera
# corrida mostro que toda la diferencia Windows/Linux de perfilar() es grepl.
suppressMessages(library(lupa))
d <- datos_administrativos
cat("R:", R.version.string, "| sistema:", Sys.info()[["sysname"]],
    "| PCRE:", extSoftVersion()[["PCRE"]], "| JIT:", isTRUE(getOption("PCRE_use_JIT", TRUE)),
    "| study:", format(getOption("PCRE_study")), "\n")
reloj <- function(f, n) {
  t0 <- proc.time()[["elapsed"]]
  for (i in seq_len(n)) f()
  1e6 * (proc.time()[["elapsed"]] - t0) / n
}
corto <- "minimo"
largo <- rep(c("minimo", "n_distintos", "media", "(tabla)"), 5)
casos <- list(
  perl_1 = function() grepl("^\\(tabla(: [^()]+)?\\)$", corto, perl = TRUE, useBytes = TRUE),
  perl_20 = function() grepl("^\\(tabla(: [^()]+)?\\)$", largo, perl = TRUE, useBytes = TRUE),
  tre_1 = function() grepl("^\\(tabla(: [^()]+)?\\)$", corto, useBytes = TRUE),
  tre_20 = function() grepl("^\\(tabla(: [^()]+)?\\)$", largo, useBytes = TRUE),
  fixed_20 = function() grepl("(tabla", largo, fixed = TRUE),
  noascii_perl_20 = function() grepl("[^\\001-\\177]", largo, useBytes = TRUE, perl = TRUE),
  noascii_tre_20 = function() grepl("[^\\001-\\177]", largo, useBytes = TRUE),
  strsplit_perl_1 = function() strsplit("a, b", "\\s*(,|\\+|\\||->)\\s*", perl = TRUE, useBytes = TRUE)
)
for (jit in c(TRUE, FALSE)) {
  options(PCRE_use_JIT = jit)
  cat("\n-- PCRE_use_JIT =", jit, "(microsegundos por llamada, 20000 llamadas)\n")
  for (k in names(casos)) cat(sprintf("%-22s %10.1f\n", k, reloj(casos[[k]], 20000)))
  invisible(suppressMessages(perfilar(d)))
  t <- vapply(1:5, function(i) system.time(suppressMessages(perfilar(d)))[["elapsed"]], 0)
  cat(sprintf("%-22s %10.3f s (mediana de 5)\n", "perfilar()", median(t)))
  t <- vapply(1:3, function(i) system.time(suppressMessages(analizar(d)))[["elapsed"]], 0)
  cat(sprintf("%-22s %10.3f s (mediana de 3)\n", "analizar()", median(t)))
}
options(PCRE_use_JIT = TRUE, PCRE_study = FALSE)
cat("\n-- PCRE_use_JIT = TRUE, PCRE_study = FALSE\n")
for (k in c("perl_20", "noascii_perl_20")) cat(sprintf("%-22s %10.1f\n", k, reloj(casos[[k]], 20000)))
t <- vapply(1:5, function(i) system.time(suppressMessages(perfilar(d)))[["elapsed"]], 0)
cat(sprintf("%-22s %10.3f s (mediana de 5)\n", "perfilar()", median(t)))
