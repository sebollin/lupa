## Rehace las cifras de referencia que publican los README y PENDIENTES:
## tablas anchas (500 filas x 50/300/1000 columnas) y valores largos
## (2.000 filas x 1.000/20.000/200.000 caracteres).
##
## Uso, desde la raiz del repositorio:
##   LUPA_RUTA=. Rscript benchmark/medir_referencias.R
##
## Por que existe: estas cifras aparecen en los README y una cifra publicada
## sin reproductor envejece en silencio -el codigo cambia, la cifra queda, y
## nadie sabe si sigue siendo cierta-.
##
## La cifra que este guion imprime NO reemplaza a la publicada a mano: se
## compara contra ella. Si difieren mucho, la publicada esta vieja y hay que
## actualizarla citando esta corrida.

ruta <- Sys.getenv("LUPA_RUTA")
if (!nzchar(ruta)) stop("falta LUPA_RUTA")
suppressMessages(pkgload::load_all(ruta, quiet = TRUE))
cat("paquete:", ruta, "\n")
cat("R:", as.character(getRversion()), "| fecha:", format(Sys.Date()), "\n\n")

## La primera corrida calienta -compila, llena caches- y se descarta: ya
## invirtio una conclusion en otra medicion de este proyecto.
medir <- function(datos, ...) {
  invisible(suppressWarnings(perfilar(utils::head(datos, 20), ...)))
  t <- system.time(suppressWarnings(perfilar(datos, ...)))[["elapsed"]]
  round(t, 2)
}

set.seed(20260830)

cat("== tablas anchas: 500 filas ==\n")
for (nc in c(50, 300, 1000)) {
  datos <- as.data.frame(setNames(
    lapply(seq_len(nc), function(i) {
      if (i %% 3 == 0) sample(c("alfa", "beta", "gama", NA), 500, TRUE)
      else round(runif(500, 0, 1000), 2)
    }),
    paste0("c", seq_len(nc))
  ), stringsAsFactors = FALSE)
  cat(sprintf("  %4d columnas: %6.2f s\n", nc, medir(datos)))
}

cat("\n== valores largos: 2.000 filas, 3 columnas ==\n")
for (largo in c(1000, 20000, 200000)) {
  base <- vapply(seq_len(40), function(i)
    paste(sample(letters, largo, TRUE), collapse = ""), "")
  datos <- data.frame(
    id = seq_len(2000),
    texto = sample(base, 2000, TRUE),
    monto = round(runif(2000, 0, 100), 2),
    stringsAsFactors = FALSE
  )
  cat(sprintf("  %6d caracteres: %6.2f s\n", largo, medir(datos)))
}

cat("\nLas referencias de MEMORIA de motor remoto (0,13 GB/M filas, 7 GB por\n")
cat("4,5 M procesadas, 19 GB por 12,8 M) NO se rehacen aca: salieron de un\n")
cat("motor de produccion al que este banco no accede. En los README tienen que\n")
cat("estar fechadas como referencia de esa corrida, no como promesa.\n")
