## Mide cuanto cambia el resultado del detector de vocabulario segun el orden de
## las filas. Es el banco del que salen los numeros de NEWS y de la vinieta.
##
## El caso que lo origino: la tercera vuelta contra bases reales dejo sin
## verificar la afirmacion "el recorte toma las primeras formas y puede costar
## detecciones", porque no se encontro una columna real que la ejercitara. La
## columna existia, y es publica.
##
## Necesita red. Los CSV no entran al repositorio: se bajan a un temporal.

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instalar `lupa` antes de correr el banco.")
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("El detector de proximidad necesita `stringdist`.")
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n\n", sep = "")

## ---- La columna ----------------------------------------------------------
##
## "Ejes de vias de circulacion", catalogo nacional de datos abiertos de
## Uruguay. La columna `nombre` mezcla nombres de calle con codigos
## `UYMOMVD_SN_####` para las vias sin nombre, y los codigos vienen agrupados:
## esa es la propiedad que hace que el orden importe.
URL_MONTEVIDEO <- paste0(
  "https://catalogodatos.gub.uy/dataset/",
  "e57a27f2-5206-42c9-8b1d-34171efd5561/resource/",
  "a167537d-a7fa-4abd-8a6b-a1001357a3e8/download/montevideo.csv"
)

destino <- file.path(tempdir(), "montevideo.csv")
if (!file.exists(destino)) {
  cat("bajando la columna...\n")
  utils::download.file(URL_MONTEVIDEO, destino, quiet = TRUE)
}
## El archivo viene en ISO-8859-1, no en UTF-8.
crudo <- utils::read.csv(
  destino, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE,
  colClasses = "character"
)
via <- crudo$nombre
cat("filas: ", length(via), " | formas distintas: ", length(unique(via)),
    "\n\n", sep = "")

## La columna se llama `nombre`, y ese nombre dispara la proteccion de datos
## personales, que enmascara la evidencia. Se renombra para poder medir: son
## nombres de calle, no datos de personas.
.grupos_de <- function(x) {
  perfil <- lupa::perfilar(
    data.frame(via = x, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgos <- perfil$hallazgos
  fila <- hallazgos[hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario", ,
                    drop = FALSE]
  if (!nrow(fila)) return(0L)
  evidencia <- as.character(fila$evidencia[[1L]])
  encontrado <- regmatches(evidencia, regexpr("grupos: [0-9]+", evidencia))
  if (!length(encontrado)) return(0L)
  as.integer(sub("grupos: ", "", encontrado))
}

## ---- El barrido ----------------------------------------------------------
##
## Los mismos datos, cinco ordenes. Si el veredicto cambia, el perfilador esta
## midiendo la forma fisica de la tabla y no los datos.
cat(sprintf("%-34s %10s %10s\n", "orden de las filas", "grupos", "segundos"))
ordenes <- list(
  "tal como viene el archivo" = function() via,
  "desordenado (semilla 11)"  = function() { set.seed(11);   sample(via) },
  "desordenado (semilla 202)" = function() { set.seed(202);  sample(via) },
  "desordenado (semilla 7777)"= function() { set.seed(7777); sample(via) },
  "alfabetico"                = function() sort(via)
)
resultados <- integer()
for (etiqueta in names(ordenes)) {
  x <- ordenes[[etiqueta]]()
  inicio <- proc.time()[["elapsed"]]
  n <- .grupos_de(x)
  segundos <- proc.time()[["elapsed"]] - inicio
  resultados <- c(resultados, n)
  cat(sprintf("%-34s %10d %10.1f\n", etiqueta, n, segundos))
}

cat("\n")
if (length(unique(resultados)) == 1L) {
  cat("Los cinco ordenes dan el mismo resultado: ", resultados[[1L]],
      " grupos. El veredicto no depende de como venga ordenado el archivo.\n",
      sep = "")
} else {
  cat("ATENCION: el resultado cambia con el orden de las filas (",
      paste(resultados, collapse = ", "),
      "). Antes de ordenar el vocabulario, esto daba 26 en el orden de llegada",
      " y 148 alfabetico.\n", sep = "")
}

cat("\nListo.\n")
