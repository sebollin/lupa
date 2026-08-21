## Mide el costo del detector de vocabulario segun el largo de los valores, con
## y sin el presupuesto por trabajo. Sirve para dos cosas: comprobar que el
## recorte hace lo que promete, y recalibrar `max_trabajo_vocabulario` si el
## dia de manana cambia la implementacion de la distancia.
##
## El caso que lo origino: una tabla del catalogo de PostGIS -3.912 filas y 5
## columnas- tardaba 243 segundos, y el 99,6 % del costo era este detector.
## No era geometria: eran cadenas largas, WKT de proyecciones.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_costo_texto.R.",
    call. = FALSE
  )
}

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Instale stringdist: sin el no hay distancia que medir.", call. = FALSE)
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n", sep = "")
cat("desde:  ", dirname(dirname(attr(.descripcion, "file"))), "\n\n", sep = "")

.vocabulario <- get(
  ".grupos_casi_duplicados_vocabulario", envir = asNamespace("lupa")
)

.valores_de <- function(n, largo, semilla = 1L) {
  set.seed(semilla)
  base <- vapply(
    seq_len(n),
    function(i) paste(sample(letters, largo, TRUE), collapse = ""),
    character(1L)
  )
  rep(base, each = 3L)
}

## Dos formas de medir, y no son intercambiables.
##
## `.aislado()` apaga `max_pares` y `max_valores` a proposito, para ver el
## efecto del presupuesto por trabajo sin que otro tope se lleve el credito.
## Sirve para calibrar, no para saber que recibe un usuario.
##
## `.por_omision()` no apaga nada: es lo que pasa de verdad. La diferencia no es
## un detalle: con los topes por omision una columna de 3.000 valores cortos se
## recorta por `max_pares`, que acota en 2.000 formas sin mirar el largo, y una
## medicion que apaga ese tope no lo ve.
.medir_con <- function(x, argumentos) {
  inicio <- proc.time()[["elapsed"]]
  salida <- do.call(
    .vocabulario, c(list(x, NULL, "columna"), argumentos)
  )
  list(
    segundos = proc.time()[["elapsed"]] - inicio,
    alcance = salida$alcance
  )
}

.aislado <- function(x, ...) {
  .medir_con(x, c(list(max_valores = 1e6, max_pares = Inf), list(...)))
}

.por_omision <- function(x, ...) .medir_con(x, list(...))

.miles <- function(x) {
  format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE,
         trim = TRUE)
}

## ---- 1. El escalado, que es lo que importa --------------------------------
##
## Sin presupuesto el costo es cuadratico en valores distintos y crece con el
## cuadrado del largo. Duplicar las filas cuadruplica el tiempo.

casos <- list(
  c(200, 900), c(400, 900), c(800, 900),
  c(800, 300), c(800, 100),
  c(2000, 80), c(1500, 40), c(3000, 20)
)

cat("Sin presupuesto (max_trabajo = Inf)\n")
cat(sprintf(
  "%7s %7s %18s %9s %16s\n",
  "valores", "largo", "trabajo", "segundos", "unidades/seg"
))
tasas <- numeric()
for (caso in casos) {
  medida <- .aislado(.valores_de(caso[[1L]], caso[[2L]]), max_trabajo = Inf)
  trabajo <- medida$alcance$trabajo_estimado
  tasas <- c(tasas, trabajo / medida$segundos)
  cat(sprintf(
    "%7d %7d %18s %9.2f %16s\n",
    caso[[1L]], caso[[2L]], .miles(trabajo), medida$segundos,
    .miles(trabajo / medida$segundos)
  ))
}
cat(sprintf(
  "\nDispersion de la tasa: %.2f veces (%s a %s unidades/seg).\n",
  max(tasas) / min(tasas), .miles(min(tasas)), .miles(max(tasas))
))
cat(paste(
  "La unidad es la comparacion de un caracter contra otro. La dispersion que",
  "queda es a favor de las columnas de valores cortos, que son el caso comun:",
  "con el mismo presupuesto disponen de mas tiempo antes de recortarse.\n"
))

## ---- 2. Con y sin el presupuesto ------------------------------------------

cat("\nCon el presupuesto por omision\n")
cat(sprintf(
  "%7s %7s %9s %9s %9s %12s\n",
  "valores", "largo", "sin tope", "con tope", "mejora", "comparado"
))
for (caso in list(c(400, 900), c(500, 900), c(800, 900), c(2000, 80))) {
  x <- .valores_de(caso[[1L]], caso[[2L]])
  sin_tope <- .aislado(x, max_trabajo = Inf)
  con_tope <- .aislado(x)
  proporcion <- con_tope$alcance$n_unidades_comparadas /
    con_tope$alcance$n_unidades_normalizadas
  cat(sprintf(
    "%7d %7d %9.2f %9.2f %8.1fx %11.1f%%\n",
    caso[[1L]], caso[[2L]], sin_tope$segundos, con_tope$segundos,
    sin_tope$segundos / max(con_tope$segundos, 1e-6), 100 * proporcion
  ))
}

## ---- 3. Que el recorte no toque el caso comun -----------------------------
##
## El riesgo del arreglo era recortar columnas normales. Una columna de codigos
## o de nombres tiene que compararse entera aunque tenga muchos valores.

cat("\nColumnas corrientes, con TODOS los topes por omision\n")
cat(sprintf(
  "%7s %7s %10s %10s %14s\n",
  "valores", "largo", "recorta", "por cual", "comparadas"
))
for (caso in list(c(500, 20), c(1000, 30), c(2000, 80), c(3000, 20))) {
  alcance <- .por_omision(.valores_de(caso[[1L]], caso[[2L]]))$alcance
  cat(sprintf(
    "%7d %7d %10s %10s %14s\n",
    caso[[1L]], caso[[2L]], if (alcance$truncado) "SI" else "no",
    if (nzchar(alcance$motivo_presupuesto)) alcance$motivo_presupuesto else "-",
    paste0(.miles(alcance$n_unidades_comparadas), " de ",
           .miles(alcance$n_unidades_normalizadas))
  ))
}
cat(paste(
  "\nEl renglon de 3.000 valores recorta por `max_pares`, que acota en 2.000",
  "formas sin mirar el largo. No lo hace el presupuesto por trabajo: es el",
  "tope viejo, que sigue puesto porque acota la memoria de la matriz de pares.",
  "Se declara igual, pero conviene saber cual de los dos recorto.\n"
))

cat("\nListo.\n")
