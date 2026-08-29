## Mide la discriminacion de la distancia en cadenas largas y el costo por
## celdas de tablas anchas. Los resultados publicados dependen de la maquina.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_topes_valores_ancho.R.",
    call. = FALSE
  )
}
if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Instale stringdist antes de ejecutar este benchmark.", call. = FALSE)
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
      base, paste0(una, collapse = ""), method = "jw"
    ),
    muchas_diferencias = stringdist::stringdist(
      base, paste0(muchas, collapse = ""), method = "jw"
    )
  )
}

cat("Discriminacion Jaro-Winkler; mediana de cinco semillas.\n")
cat("largo\tuna_diferencia\t1000_diferencias\n")
for (largo in c(10000L, 20000L, 25000L, 50000L)) {
  resultados <- vapply(1:5, function(semilla) {
    .par(largo, 1000L, semilla)
  }, numeric(2L))
  mediana <- apply(resultados, 1L, median)
  cat(largo, "\t", format(mediana[[1L]], digits = 6), "\t",
      format(mediana[[2L]], digits = 6), "\n", sep = "")
}
cat(paste(
  "El tope predeterminado de lupa es 10000 caracteres: deja margen antes",
  "del cruce en que 1000 diferencias caen bajo umbral 0.10.\n"
))

.tabla_ancha <- function(n_filas, n_columnas) {
  valores <- paste0("valor_", seq_len(n_filas))
  as.data.frame(
    setNames(rep(list(valores), n_columnas), paste0("V", seq_len(n_columnas))),
    stringsAsFactors = FALSE
  )
}

cat("\nCosto de tablas anchas; sin aviso interactivo.\n")
cat("filas\tcolumnas\tceldas\tsegundos\n")
for (n_columnas in c(50L, 300L, 1000L)) {
  datos <- .tabla_ancha(500L, n_columnas)
  tiempo <- system.time(
    lupa::perfilar(
      datos, analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      avisar_costo_tabla_ancha = FALSE
    )
  )[["elapsed"]]
  cat(500L, "\t", n_columnas, "\t", 500L * n_columnas, "\t",
      format(tiempo, digits = 6), "\n", sep = "")
}
