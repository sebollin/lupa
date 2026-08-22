## Verifica los hallazgos de severidad `error` sobre un registro publico real,
## contra comprobaciones independientes escritas a mano en R base.
##
## Es la fila "Registro real de sanciones" de la tabla de evidencia de los
## README. Existia como numero desde una ronda vieja y **no habia forma de
## reproducirlo desde el repositorio**: se midio una vez, quedo en notas que no
## se publican, y el numero viajo al README. Un numero publicado que nadie puede
## comprobar se vuelve mentira sin que nadie se entere.
##
## Necesita red. El CSV no entra al repositorio: se baja a un temporal.

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instalar `lupa` antes de correr el banco.", call. = FALSE)
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n\n", sep = "")

## ---- El registro ---------------------------------------------------------
##
## "Registro de Sanciones a Empresas", catalogo nacional de datos abiertos de
## Uruguay: 2.556 filas por 8 columnas.
URL <- paste0(
  "https://catalogodatos.gub.uy/dataset/",
  "8fa60d47-b81e-4780-86dc-8985ab3589d7/resource/",
  "4c6cf3a6-488a-41a7-a7e3-4061f682d2eb/download/",
  "registro-de-sanciones-marzo-2025.csv"
)
destino <- file.path(tempdir(), "registro-de-sanciones.csv")
if (!file.exists(destino)) {
  cat("bajando el registro...\n")
  utils::download.file(URL, destino, quiet = TRUE)
}
datos <- utils::read.csv(
  destino, stringsAsFactors = FALSE, colClasses = "character"
)
names(datos) <- make.names(names(datos))
cat("filas: ", nrow(datos), " | columnas: ", ncol(datos), "\n\n", sep = "")

## Este archivo llega con `Encoding()` en `unknown` y con acentos, que es como
## `read.csv()` entrega cualquier CSV en espanol. Fue el que destapo que el
## orden por bytes rompia el perfil entero, asi que el banco tambien sirve de
## regresion de eso.

perfil <- lupa::perfilar(
  datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
)
hallazgos <- perfil$hallazgos
errores <- hallazgos[as.character(hallazgos$severidad) == "error", , drop = FALSE]
cat("hallazgos de severidad error: ", nrow(errores), "\n\n", sep = "")

## ---- Las comprobaciones, escritas aparte ---------------------------------
##
## Cada una mide lo mismo que el hallazgo, con R base y sin usar `lupa`. Si el
## paquete y la comprobacion no coinciden, uno de los dos esta mal.
.blancos <- function(x) sum(trimws(x) == "" | is.na(x))
.guiones <- function(x) sum(trimws(x) == "-")
.ESPACIO_INVISIBLE <- " "

comprobaciones <- list(
  list("Fecha: un unico valor en blanco", 1L, .blancos(datos$Fecha)),
  list(
    "Fecha: 2.552 en ISO y 2 en dd/mm/aaaa", c(2552L, 2L),
    c(sum(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trimws(datos$Fecha))),
      sum(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", trimws(datos$Fecha))))
  ),
  list(
    "Empresa: al menos un espacio invisible", TRUE,
    any(grepl(.ESPACIO_INVISIBLE, datos$Empresa, fixed = TRUE))
  ),
  list("Sancion.aplicada: 2 guiones", 2L, .guiones(datos$Sancion.aplicada)),
  list("Importe: 2 en blanco", 2L, .blancos(datos$Importe)),
  list(
    "Moneda: 993 guiones y 1 en blanco", c(993L, 1L),
    c(.guiones(datos$Moneda), .blancos(datos$Moneda))
  ),
  list("Sector: 7 en blanco", 7L, .blancos(datos$Sector)),
  list("Motivo: 6 en blanco", 6L, .blancos(datos$Motivo))
)
clave <- do.call(paste, c(datos, sep = "|"))
repetidas <- table(clave)
repetidas <- repetidas[repetidas > 1L]
comprobaciones[[length(comprobaciones) + 1L]] <- list(
  "filas duplicadas: 16 filas, 8 excedentes", c(16L, 8L),
  c(sum(repetidas), sum(repetidas) - length(repetidas))
)

cat(sprintf("%-42s %-14s %s\n", "comprobacion independiente", "medido", "coincide"))
aciertos <- 0L
for (caso in comprobaciones) {
  esperado <- paste(caso[[2L]], collapse = ",")
  obtenido <- paste(caso[[3L]], collapse = ",")
  coincide <- identical(esperado, obtenido)
  aciertos <- aciertos + as.integer(coincide)
  cat(sprintf("%-42s %-14s %s\n", caso[[1L]], obtenido,
              if (coincide) "si" else "NO"))
}

cat("\n", aciertos, " de ", length(comprobaciones),
    " comprobaciones coinciden con lo que informa el paquete.\n", sep = "")
if (aciertos != length(comprobaciones) ||
    nrow(errores) != length(comprobaciones)) {
  cat(
    "ATENCION: el registro publico pudo cambiar desde la medicion, o el paquete\n",
    "cambio lo que informa. Hay que mirar cual de los dos antes de publicar el\n",
    "numero de la tabla de evidencia.\n", sep = ""
  )
}
cat("\nListo.\n")
