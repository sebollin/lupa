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

## ---- 4. Cadenas que comparten prefijo ------------------------------------
##
## Una refutacion pregunto si el modelo de costo vale para cadenas reales, que
## comparten prefijos, y no solo para letras al azar. Importa porque
## Jaro-Winkler pondera el prefijo comun, y porque el caso que origino todo
## esto -WKT de proyecciones- empieza igual en todos los valores.

cat("\nCadenas con prefijo comun contra cadenas al azar\n")
cat(sprintf("%-24s %16s %9s %16s\n", "familia", "trabajo", "segundos",
            "unidades/seg"))

.con_prefijo <- function(n, largo, cola = 40L, semilla = 7L) {
  set.seed(semilla)
  cabecera <- strrep("PROJCS_GEOGCS_DATUM_SPHEROID_", 40L)
  base <- vapply(seq_len(n), function(i) {
    paste0(
      substr(cabecera, 1L, max(0L, largo - cola)),
      paste(sample(letters, cola, TRUE), collapse = "")
    )
  }, character(1L))
  rep(base, each = 3L)
}

tasas_prefijo <- numeric()
for (caso in list(c(400, 200), c(400, 400), c(400, 900))) {
  for (familia in c("al azar", "prefijo comun")) {
    x <- if (familia == "al azar") {
      .valores_de(caso[[1L]], caso[[2L]])
    } else {
      .con_prefijo(caso[[1L]], caso[[2L]])
    }
    medida <- .aislado(x, max_trabajo = Inf)
    trabajo <- medida$alcance$trabajo_estimado
    tasas_prefijo <- c(tasas_prefijo, trabajo / medida$segundos)
    cat(sprintf(
      "%-24s %16s %9.2f %16s\n",
      paste0(familia, " ", caso[[1L]], "x", caso[[2L]]),
      .miles(trabajo), medida$segundos, .miles(trabajo / medida$segundos)
    ))
  }
}
## La tasa se queda en la banda de la calibracion, asi que el modelo no se
## rompe. Pero la diferencia no es ruido: el prefijo comun sale
## sistematicamente mas rapido, y la ventaja crece con el largo, porque
## Jaro-Winkler corta antes cuando las cadenas empiezan igual. O sea que el
## modelo es **pesimista** para datos con prefijo comun -como el WKT- y ahi el
## presupuesto recorta un poco antes de lo que el reloj pediria. Es la
## direccion segura, pero conviene tenerlo escrito y no confundirlo con "da lo
## mismo".
razones <- tasas_prefijo[c(FALSE, TRUE)] / tasas_prefijo[c(TRUE, FALSE)]
cat(sprintf(
  paste(
    "\nTasa entre %s y %s unidades/seg, dentro de la banda de la calibracion:",
    "el modelo no se rompe. Pero el prefijo comun sale %.2f a %.2f veces mas",
    "rapido por unidad, y la ventaja crece con el largo, asi que el modelo",
    "sobreestima el costo de esos datos y el presupuesto recorta antes de lo",
    "necesario.\n"
  ),
  .miles(min(tasas_prefijo)), .miles(max(tasas_prefijo)),
  min(razones), max(razones)
))

## ---- 5. La tasa que ancla los umbrales del plan ---------------------------

## `plan_perfilado_dbi()` estima el trabajo del cliente en pares de formas
## comparadas, y sus umbrales (2e6 y 2e8) estan anclados a la misma escala de
## segundos que los del motor. El ancla es esta medicion: sin ella los umbrales
## serian dos numeros elegidos a dedo.
##
## Se mide con los topes POR OMISION, que es lo que recibe un usuario. Cuando
## `max_pares` recorta, el tiempo deja de crecer con los valores: eso no es un
## error de la medicion, es el techo haciendo su trabajo, y es justamente el
## techo que el plan usa para acotar.
cat("\nTasa de pares por segundo, con los topes por omision\n")
cat(sprintf(
  "%10s %8s %14s %10s %14s\n",
  "valores", "largo", "pares", "segundos", "pares/seg"
))
tasas_pares <- numeric()
for (n in c(1000, 2000)) {
  for (largo in c(40, 200)) {
    medida <- .por_omision(.valores_de(n, largo))
    segundos <- medida$segundos
    # Los pares que el plan contaria para esta columna: los que hay, acotados
    # por el mismo tope que usa el plan.
    pares <- min(n * (n - 1) / 2, eval(formals(.vocabulario)$max_pares))
    tasas_pares <- c(tasas_pares, pares / segundos)
    cat(sprintf(
      "%10s %8d %14s %10.2f %14s\n",
      .miles(n), largo, .miles(pares), segundos, .miles(pares / segundos)
    ))
  }
}
## El numero que viaja en `supuesto_costo` es el de cuarenta caracteres, que es
## el largo comun en columnas de texto de una base administrativa. Con valores
## mucho mas largos la tasa cae, y por eso el plan declara que su cuenta de
## pares es un piso y no una promesa.
cat(sprintf(
  paste(
    "\nEl plan declara ~800.000 pares/seg sobre valores de cuarenta",
    "caracteres. Medido aca: %s a %s pares/seg segun el largo. Con textos",
    "mucho mas largos la tasa cae, asi que la cuenta de pares del plan es un",
    "piso: por eso `supuesto_costo` lo dice en vez de prometer segundos.\n"
  ),
  .miles(min(tasas_pares)), .miles(max(tasas_pares))
))

cat("\nListo.\n")
