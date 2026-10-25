# Diagnostico 3: con la proteccion apagada por omision, que queda del exceso de
# Windows y donde.
suppressMessages(library(lupa))
d <- datos_administrativos
cat("R:", R.version.string, "| sistema:", Sys.info()[["sysname"]], "\n")
mediana <- function(f, n = 7) median(vapply(seq_len(n), function(i) system.time(suppressMessages(suppressWarnings(f())))[["elapsed"]], 0))
invisible(suppressMessages(perfilar(d)))
r <- c(perfilar = mediana(function() perfilar(d)),
       perfilar_protegido = mediana(function() perfilar(d, proteger_datos_personales = TRUE)),
       analizar = mediana(function() analizar(d), 3),
       reportar = mediana(function() reportar(perfilar(d), archivo = tempfile(fileext = ".html")), 3))
for (k in names(r)) cat(sprintf("%-24s %8.3f s\n", k, r[[k]]))
for (proteger in c(FALSE, TRUE)) {
  archivo <- tempfile()
  Rprof(archivo, interval = 0.01)
  for (i in 1:20) invisible(suppressMessages(perfilar(d, proteger_datos_personales = proteger)))
  Rprof(NULL)
  p <- summaryRprof(archivo)
  cat("\n-- Rprof, 20 perfilar(proteger_datos_personales =", proteger, "): total", p$sampling.time, "s\n")
  tt <- p$by.total
  print(head(tt[grepl('^"\\.', rownames(tt)), c("total.time", "self.time")], 25))
  cat("-- tiempo propio\n"); print(head(p$by.self[, c("self.time", "self.pct")], 12))
}
