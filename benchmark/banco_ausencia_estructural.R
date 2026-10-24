## Rehace el banco de especificidad y sensibilidad de `posible_ausencia_estructural`
## que citan los README: veinte conjuntos reales que vienen con R y sesenta
## tablas al azar con ausencia independiente (cero senales esperadas), mas los
## casos que SI deben disparar y el ruido que debe callar.
##
## Uso, desde la raiz del repositorio:
##   LUPA_RUTA=. Rscript benchmark/banco_ausencia_estructural.R
##
## La corrida original (2026-08-20) no dejo guion; este lo repone. La eleccion
## de los veinte es determinista -los primeros veinte `data.frame` del paquete
## `datasets`, por orden alfabetico- para que dos corridas cuenten lo mismo.

ruta <- Sys.getenv("LUPA_RUTA")
if (!nzchar(ruta)) stop("falta LUPA_RUTA")
suppressMessages(pkgload::load_all(ruta, quiet = TRUE))

contar <- function(datos, ...) {
  p <- suppressWarnings(suppressMessages(perfilar(datos, ...)))
  h <- p$hallazgos
  sum(h$tipo_hallazgo == "posible_ausencia_estructural")
}

## --- 1. Veinte conjuntos reales -------------------------------------------
nombres <- data(package = "datasets")$results[, "Item"]
nombres <- sub(" .*", "", nombres)          # "beaver1 (beavers)" -> "beaver1"
es_df <- vapply(nombres, function(n) {
  o <- tryCatch(get(n, envir = asNamespace("datasets")), error = function(e) NULL)
  is.data.frame(o) && nrow(o) >= 20
}, logical(1))
veinte <- sort(unique(nombres[es_df]))[1:20]
cat("== veinte conjuntos reales (datasets de R) ==\n")
senales_reales <- 0L
for (n in veinte) {
  k <- contar(get(n, envir = asNamespace("datasets")))
  if (k > 0) cat(sprintf("  %-18s %d senal(es)  <<< dispara\n", n, k))
  senales_reales <- senales_reales + k
}
cat(sprintf("  total: %d senales en %d conjuntos (esperado: 0)\n\n",
            senales_reales, length(veinte)))

## --- 2. Sesenta tablas al azar, ausencia independiente --------------------
set.seed(20260820)
senales_azar <- 0L
for (i in 1:60) {
  nf <- sample(100:400, 1); nc <- sample(4:10, 1)
  datos <- as.data.frame(lapply(seq_len(nc), function(j) {
    v <- switch(1 + (j %% 3),
                round(runif(nf, 0, 100), 2),
                sample(letters[1:6], nf, TRUE),
                sample(1:1000, nf, TRUE))
    v[runif(nf) < runif(1, 0.05, 0.3)] <- NA   # ausencia INDEPENDIENTE
    v
  }))
  names(datos) <- paste0("v", seq_len(nc))
  senales_azar <- senales_azar + contar(datos)
}
cat(sprintf("== sesenta tablas al azar: %d senales (esperado: 0) ==\n\n", senales_azar))

## --- 3. Los positivos: tienen que disparar --------------------------------
n <- 200
eav <- data.frame(tipo = rep(c("A", "B"), each = n / 2),
                  valor_a = c(round(runif(n / 2), 2), rep(NA, n / 2)),
                  valor_b = c(rep(NA, n / 2), round(runif(n / 2), 2)))
salto <- data.frame(trabaja = rep(c("si", "no"), each = n / 2),
                    ocupacion = c(sample(letters[1:8], n / 2, TRUE), rep(NA, n / 2)),
                    edad = sample(18:80, n, TRUE))
excl <- data.frame(monto_pesos = c(round(runif(n / 2, 1, 9), 2), rep(NA, n / 2)),
                   monto_dolares = c(rep(NA, n / 2), round(runif(n / 2, 1, 9), 2)),
                   otra = round(runif(n), 2))
cat("== positivos (esperado: disparan) ==\n")
cat(sprintf("  entidad-atributo-valor : %s\n", if (contar(eav) > 0) "dispara" else "NO dispara <<<"))
cat(sprintf("  salto de encuesta      : %s\n", if (contar(salto) > 0) "dispara" else "NO dispara <<<"))
cat(sprintf("  columnas excluyentes   : %s\n", if (contar(excl) > 0) "dispara" else "NO dispara <<<"))

## --- 4. El ruido del 10 %: tiene que callar -------------------------------
ruido <- salto
idx <- sample(which(ruido$trabaja == "no"), n / 2 * 0.2)
ruido$ocupacion[idx] <- sample(letters[1:8], length(idx), TRUE)
cat(sprintf("\n== 10%% de ruido (esperado: calla): %s ==\n",
            if (contar(ruido) == 0) "calla" else "dispara <<<"))
