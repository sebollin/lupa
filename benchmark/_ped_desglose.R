## Rehace el desglose por diagnostico de las celdas trazables de PED/Hospital,
## que es la seccion "Por que la precision de PED/Hospital es baja" de
## `resultados.md`.
##
## Existia como numero sin reproductor: se midio el 2026-08-18, quedo en la
## tabla, y cuando el detector de vocabulario empezo a trazar sus filas el
## desglose paso a describir un paquete que ya no existia -sumaba 4.035 celdas
## contra 11.795-. Marcar la tabla como vieja no alcanza: marcar no es corregir.
##
## Una celda trazable es un par (fila, columna). Contar filas da 1.000 -todas- y
## sumar los hallazgos da 13.892, porque dos diagnosticos pueden senalar la misma
## celda. La cifra que publica la tabla es la UNION: 11.795.
##
## Se corre desde este directorio: `cd benchmark && Rscript _ped_desglose.R`.
## Necesita red; los CSV se bajan a un temporal y no entran al repositorio.

Sys.setenv(PED_DATASETS = "Hospital")
source("_comun_bancos.R")
sys.source("verdad_ped.R", envir = globalenv())
r <- verdad_ped[["Hospital"]]
p <- lupa::perfilar(r$sucia)
h <- p$hallazgos
h <- h[as.character(h$severidad) != "ok", , drop = FALSE]

## Una celda trazable es un par (fila, columna). Contar filas da 1.000 -todas- y
## sumar por hallazgo cuenta dos veces la celda que dos diagnosticos tocan.
por <- list(); todas <- character()
for (i in seq_len(nrow(h))) {
  tr <- h$trazabilidad[[i]]
  idx <- if (!is.null(tr$indices_fila)) as.integer(tr$indices_fila) else integer()
  col <- as.character(h$columna[[i]])
  if (!length(idx) || is.na(col)) next
  celdas <- paste0(col, "\r", idx)
  d <- as.character(h$tipo_hallazgo[[i]])
  por[[d]] <- union(por[[d]], celdas)
  todas <- union(todas, celdas)
}
agg <- data.frame(diagnostico = names(por),
                  celdas = vapply(por, length, integer(1L)),
                  stringsAsFactors = FALSE)
agg <- agg[order(-agg$celdas), ]
print(agg, row.names = FALSE)
cat("\nUNION de celdas trazables:", length(todas), "\n")
