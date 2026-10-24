## Mide que gobierna la memoria de `perfilar_dbi()`: si escala con las filas o
## con las celdas. La respuesta decide como leer el costo de una tabla ancha.
##
## El caso que lo origino: una corrida contra bases reales perfilo una tabla de
## 334.000 filas por 158 columnas y uso 4,8 GB. Leida por filas parecia una tabla
## mediana; leida por celdas son casi 53 millones, y ahi el numero deja de
## sorprender. Antes de discutir si `muestra = Inf` puede ser el valor por
## omision, hacia falta saber contra que escala el costo.
##
## Lo que este guion **si** contesta: la forma de la ley -filas o celdas-, que se
## comprueba con dos tablas de la misma cantidad de celdas repartidas distinto.
##
## Lo que **no** contesta: cuanta memoria va a usar una tabla concreta de otra
## persona. La tasa por celda depende de los datos: estos fixtures usan letras
## sueltas, que R guarda con cache global de cadenas, asi que salen mas baratos
## por celda que texto real con muchos valores distintos. Aplicada a la tabla de
## 158 columnas de arriba, la tasa de este guion sobreestima al doble. Para
## predecir una tabla, la tasa hay que sacarla de una medicion sobre datos
## parecidos, no de aca.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_memoria_por_celdas.R.",
    call. = FALSE
  )
}

for (paquete in c("lupa", "DBI", "RSQLite")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Instale ", paquete, " antes de ejecutar este benchmark.", call. = FALSE)
  }
}

## Este guion mide el camino de "traer la tabla entera", que es lo que hace
## `muestra = Inf`. Contra una version instalada que todavia traiga una muestra
## acotada mide otra cosa **y no lo dice**: la primera corrida de este benchmark
## salio con la memoria plana en 85 Mb y tiempos diez veces menores, porque la
## instalada era de la vispera y traia 1.000 filas. Un numero medido contra la
## biblioteca equivocada es peor que ningun numero, porque parece un resultado.
##
## Por eso se imprime contra que se mide y se corta si no corresponde.
cat("lupa instalada -> Built: ",
    as.character(utils::packageDescription("lupa")$Built), "
", sep = "")
.muestra_por_omision <- formals(lupa::perfilar_dbi)$muestra
cat("muestra por omision: ", format(.muestra_por_omision), "

", sep = "")
if (is.finite(.muestra_por_omision)) {
  stop(
    "Esta version de `lupa` trae una muestra acotada por omision (",
    format(.muestra_por_omision), "), asi que este guion no mediria el camino ",
    "de la tabla entera. Instale las fuentes actuales antes de medir.",
    call. = FALSE
  )
}

.mb_usados <- function() sum(gc()[, "used"] * c(56, 8) / 1e6)
.mb_pico <- function() sum(gc()[, "max used"] * c(56, 8) / 1e6)

medir_caso <- function(filas, columnas) {
  set.seed(5)
  gc(reset = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(id = seq_len(filas))
  for (k in seq_len(columnas)) {
    datos[[paste0("c", k)]] <- sample(letters, filas, TRUE)
  }
  DBI::dbWriteTable(con, "t", datos)
  rm(datos)
  gc()
  base <- .mb_usados()
  segundos <- system.time(lupa::perfilar_dbi(con, "t"))[["elapsed"]]
  data.frame(
    filas = filas, columnas = columnas + 1L,
    celdas = filas * (columnas + 1L),
    segundos = segundos, mb = .mb_pico() - base,
    stringsAsFactors = FALSE
  )
}

# Los dos del medio tienen casi las mismas celdas repartidas al reves: son el
# par que contesta la pregunta.
#
# Se repite cada caso porque la contabilidad de memoria de `gc()` es ruidosa: la
# primera version de este guion corria cada caso UNA vez y de ahi salio la
# conclusion de que la memoria escalaba con celdas y no con filas. Una segunda
# corrida no la reprodujo -173 Mb contra 257 para casi las mismas celdas-. Una
# sola corrida no es una medicion, y con dos hipotesis en juego el ruido decide.
VECES <- 3L
casos <- list(c(20000L, 10L), c(20000L, 40L), c(80000L, 10L), c(80000L, 40L))

medir_repetido <- function(filas, columnas) {
  corridas <- do.call(rbind, lapply(
    seq_len(VECES), function(i) medir_caso(filas, columnas)
  ))
  data.frame(
    filas = filas, columnas = columnas + 1L,
    celdas = filas * (columnas + 1L),
    segundos = stats::median(corridas$segundos),
    mb = stats::median(corridas$mb),
    mb_min = min(corridas$mb), mb_max = max(corridas$mb),
    stringsAsFactors = FALSE
  )
}

salida <- do.call(rbind, lapply(casos, function(x) medir_repetido(x[[1L]], x[[2L]])))
salida$mb_por_millon <- salida$mb / (salida$celdas / 1e6)

cat("Mediana de ", VECES, " corridas por caso.\n\n", sep = "")
print(salida, row.names = FALSE)

pareja <- salida[salida$celdas > 8e5 & salida$celdas < 9e5, ]
if (nrow(pareja) == 2L) {
  cat("\nMismas celdas repartidas distinto:\n",
      paste(sprintf("  %6d x %2d = %.2fM -> %.0f Mb  (entre %.0f y %.0f)",
                    pareja$filas, pareja$columnas, pareja$celdas / 1e6,
                    pareja$mb, pareja$mb_min, pareja$mb_max), collapse = "\n"),
      "\n\nSi las dos bandas se solapan, el par no distingue las dos hipotesis:\n",
      "hace falta mas repeticion o un caso con mas contraste.\n", sep = "")
}
