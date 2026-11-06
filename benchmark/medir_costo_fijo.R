## Rehace las cifras del costo fijo sobre tablas chicas: perfilar() y analizar(),
## cada una sin y con proteger_datos_personales = TRUE, sobre la tabla
## de ejemplo del README (`datos_operativos`, 13 filas y 10 columnas) y sobre
## sus tres primeras columnas.
##
## - En caliente: tres llamadas de calentamiento y la mediana de 30.
## - En frio: la primera llamada en un proceso R nuevo, mediana de 5 procesos.
##   Es lo que paga cada ejemplo del check.
##
## Mide una instalacion de ESTAS fuentes: antes de medir las instala en una
## biblioteca temporal, asi el codigo medido es el de la huella y esta
## compilado a bytes como en el check. Con `pkgload::load_all()` no lo esta, y
## la primera llamada pagaria la compilacion.
##
## Uso, desde cualquier carpeta (con TMPDIR sin espacios):
##   Rscript benchmark/medir_costo_fijo.R
##
## La salida queda en benchmark/datos/medir-costo-fijo.csv, con las mismas
## columnas que medir-referencias.csv: fecha, entorno, huella del codigo y una
## fila por medicion, mas una fila `<funcion>_mediana` por caso.

.argumentos <- commandArgs(trailingOnly = TRUE)

.tablas <- function() {
  datos <- lupa::datos_operativos
  list(
    "10" = datos,
    "3" = datos[, c("id_registro", "codigo_usuario", "fecha_evento")]
  )
}

.llamadas <- list(
  perfilar = function(d) lupa::perfilar(d),
  perfilar_protegido = function(d) {
    lupa::perfilar(d, proteger_datos_personales = TRUE)
  },
  analizar = function(d) lupa::analizar(d),
  analizar_protegido = function(d) {
    lupa::analizar(d, proteger_datos_personales = TRUE)
  }
)

.cronometrar <- function(f) {
  system.time(suppressWarnings(f()))[["elapsed"]]
}

## Modo de un solo proceso: la primera llamada de una funcion sobre una tabla.
if (length(.argumentos) && identical(.argumentos[[1L]], "--frio")) {
  .biblioteca <- .argumentos[[4L]]
  suppressPackageStartupMessages(library(lupa, lib.loc = .biblioteca))
  if (!startsWith(normalizePath(find.package("lupa")),
                  normalizePath(.biblioteca))) {
    stop("Se cargo otra instalacion de lupa.", call. = FALSE)
  }
  datos <- .tablas()[[.argumentos[[3L]]]]
  llamada <- .llamadas[[.argumentos[[2L]]]]
  cat("segundos=", .cronometrar(function() llamada(datos)), "\n", sep = "")
  quit(save = "no")
}

if (sys.nframe() > 0L) {
  stop("Este script se ejecuta con Rscript benchmark/medir_costo_fijo.R.",
       call. = FALSE)
}
.todos <- commandArgs(trailingOnly = FALSE)
.marca_archivo <- "--file="
.rutas_script <- .todos[startsWith(.todos, .marca_archivo)]
if (length(.rutas_script) != 1L) {
  stop("No se pudo determinar la ruta del script.", call. = FALSE)
}
# Rscript escribe los espacios de la ruta como `~+~` en `--file=`.
.archivo_script <- normalizePath(gsub(
  "~+~", " ", sub(.marca_archivo, "", .rutas_script[[1L]], fixed = TRUE),
  fixed = TRUE
), mustWork = TRUE)
.raiz_paquete <- normalizePath(file.path(dirname(.archivo_script), ".."),
                               mustWork = TRUE)
if (!file.exists(file.path(.raiz_paquete, "DESCRIPTION"))) {
  stop("No se encuentran las fuentes de lupa junto al script.", call. = FALSE)
}

## Instalar las fuentes en una biblioteca temporal.
.biblioteca <- tempfile("lupa-costo-fijo-")
dir.create(.biblioteca)
.registro <- file.path(tempdir(), "instalar-lupa.log")
.estado <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-docs", "--no-html", "--no-multiarch",
    "--no-test-load", "-l", shQuote(.biblioteca), shQuote(.raiz_paquete)),
  stdout = .registro, stderr = .registro
)
if (!identical(.estado, 0L)) {
  cat(utils::tail(readLines(.registro), 20L), sep = "\n")
  stop("No se pudo instalar lupa desde ", .raiz_paquete, call. = FALSE)
}
suppressPackageStartupMessages(library(lupa, lib.loc = .biblioteca))
if (!startsWith(normalizePath(find.package("lupa")),
                normalizePath(.biblioteca))) {
  stop("Se cargo otra instalacion de lupa.", call. = FALSE)
}

.archivos_r <- sort(list.files(
  file.path(.raiz_paquete, "R"), "[.]R$", full.names = TRUE
))
.revision <- paste0(
  as.character(utils::packageVersion("lupa")), "; R/ md5 ",
  local({
    # La suma de la LISTA de sumas, como benchmark/_firma.R: los primeros 12
    # caracteres de las sumas pegadas eran la suma del primer archivo y nada mas.
    lista <- tempfile()
    on.exit(unlink(lista))
    writeLines(paste(unname(tools::md5sum(.archivos_r)), basename(.archivos_r)), lista)
    substr(unname(tools::md5sum(lista)), 1L, 12L)
  })
)
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.entorno <- list(
  fecha = .fecha,
  revision_paquete = .revision,
  huella_codigo = .revision,
  version_paquete = as.character(utils::packageVersion("lupa")),
  version_R = R.version.string,
  plataforma = R.version$platform,
  sistema = unname(Sys.info()[["sysname"]]),
  version_sistema = unname(Sys.info()[["release"]]),
  maquina = unname(Sys.info()[["machine"]]),
  nucleos = parallel::detectCores(logical = TRUE)
)

cat("paquete:", .raiz_paquete, "\n")
cat("huella:", .revision, "\n")
cat("R:", as.character(getRversion()), "| fecha:", .fecha, "\n")
if (file.exists("/proc/loadavg")) {
  cat("carga de la maquina:", readLines("/proc/loadavg", n = 1L), "\n")
}
cat("\n")

.mediciones <- list()
.agregar <- function(escenario, medida, valor, n_columnas) {
  .mediciones[[length(.mediciones) + 1L]] <<- data.frame(
    entorno = "Rscript",
    as.data.frame(.entorno, stringsAsFactors = FALSE),
    escenario = escenario, medida = medida, valor = as.numeric(valor),
    unidad = "segundos", n_filas = 13L, n_columnas = n_columnas,
    largo = NA_integer_, stringsAsFactors = FALSE
  )
}

.calentamiento <- 3L
.repeticiones <- 30L
.procesos_frio <- 5L
.tablas_medidas <- .tablas()
.resumen <- list()

for (nc in names(.tablas_medidas)) {
  datos <- .tablas_medidas[[nc]]
  for (nombre in names(.llamadas)) {
    llamada <- .llamadas[[nombre]]
    for (i in seq_len(.calentamiento)) invisible(.cronometrar(function() llamada(datos)))
    caliente <- vapply(seq_len(.repeticiones), function(i) {
      .cronometrar(function() llamada(datos))
    }, numeric(1L))
    for (valor in caliente) .agregar("caliente", nombre, valor, as.integer(nc))
    .agregar("caliente", paste0(nombre, "_mediana"), stats::median(caliente),
             as.integer(nc))

    frio <- vapply(seq_len(.procesos_frio), function(i) {
      salida <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(shQuote(.archivo_script), "--frio", nombre, nc, shQuote(.biblioteca)),
        stdout = TRUE, stderr = FALSE
      )
      linea <- grep("^segundos=", salida, value = TRUE)
      if (length(linea) != 1L) {
        stop("La medicion en frio de ", nombre, " no devolvio su tiempo.",
             call. = FALSE)
      }
      as.numeric(sub("^segundos=", "", linea))
    }, numeric(1L))
    for (valor in frio) .agregar("frio", nombre, valor, as.integer(nc))
    .agregar("frio", paste0(nombre, "_mediana"), stats::median(frio),
             as.integer(nc))

    .resumen[[length(.resumen) + 1L]] <- data.frame(
      columnas = as.integer(nc), funcion = nombre,
      caliente_mediana_s = round(stats::median(caliente), 3),
      frio_mediana_s = round(stats::median(frio), 3)
    )
  }
}

cat(sprintf(
  "Mediana en caliente de %d llamadas tras %d de calentamiento; en frio, de %d procesos:\n",
  .repeticiones, .calentamiento, .procesos_frio
))
print(do.call(rbind, .resumen), row.names = FALSE)

salida <- do.call(rbind, .mediciones)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(directorio_datos, "medir-costo-fijo.csv")
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("\nSalida:", archivo_salida, "\n")
