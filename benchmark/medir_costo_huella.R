## Mide el tiempo y el pico de memoria de `perfilar()`, `analizar()`,
## `descubrir_patrones()` y `perfilar_dbi()` sobre tablas que se leen enteras y
## sobre tablas que superan `muestra`, donde la seleccion por huella recorre
## todas las filas. Es la medicion que cita la entrada de la huella en NEWS.md:
## se corre una vez por version, cada una instalada en su propia biblioteca, un
## escenario por proceso, alternando las versiones en cada corrida, y se
## comparan las medianas por pares.
##
##   R_LIBS=<biblioteca> Rscript benchmark/medir_costo_huella.R <etiqueta> <escenario> [corrida]
##
## Con `todos` como escenario lista los nombres. Agrega una fila a
## benchmark/datos/medir-costo-huella.csv. La memoria es el pico de lo que R
## tiene en uso durante la llamada (`gc()`, columna "max used"), menos lo que
## ya ocupaban los datos.

if (sys.nframe() > 0L) {
  stop(
    "Este script se ejecuta con Rscript benchmark/medir_costo_huella.R.",
    call. = FALSE
  )
}
argumentos <- commandArgs(TRUE)
if (length(argumentos) < 2L) {
  stop("Uso: Rscript benchmark/medir_costo_huella.R <etiqueta> <escenario> [corrida]",
       call. = FALSE)
}
etiqueta <- argumentos[[1L]]
escenario <- argumentos[[2L]]
corrida <- if (length(argumentos) > 2L) as.integer(argumentos[[3L]]) else 1L
fecha <- as.POSIXct("2026-01-01", tz = "UTC")
quieto <- function(expr) suppressMessages(suppressWarnings(expr))

ancha <- function(n, k, texto) {
  d <- as.data.frame(replicate(
    k, if (texto) sprintf("id%07d", sample.int(n)) else stats::rnorm(n),
    simplify = FALSE
  ))
  names(d) <- paste0("x", seq_len(k))
  d
}
mixta <- function(n) {
  data.frame(
    id = sample.int(n), codigo = sprintf("REG-%09d", sample.int(n)),
    nombre = sprintf("N%07d", sample.int(n)),
    monto = round(stats::rnorm(n, 500, 100), 2), x2 = stats::runif(n),
    x3 = stats::rpois(n, 4), cat = sample(c("A", "B", "C"), n, TRUE),
    cat2 = sample(letters, n, TRUE),
    fecha = as.Date("2020-01-01") + sample(0:2000, n, TRUE),
    stringsAsFactors = FALSE
  )
}
perfil <- function(d) function() quieto(lupa::perfilar(d, fecha = fecha))

## Cada escenario arma sus datos fuera de la medicion y devuelve la llamada.
escenarios <- list(
  entera_numerica_50k = function() perfil(ancha(50000L, 10L, FALSE)),
  entera_texto_50k = function() perfil(ancha(50000L, 10L, TRUE)),
  entera_mixta_50k = function() perfil(mixta(50000L)),
  entera_factor_50k = function() {
    n <- 50000L
    perfil(data.frame(id = seq_len(n),
                      nivel = factor(sprintf("N%06d", sample.int(n))),
                      x = stats::rnorm(n)))
  },
  ancha_20k_num = function() perfil(ancha(20000L, 200L, FALSE)),
  ancha_20k_text = function() perfil(ancha(20000L, 200L, TRUE)),
  ancha_100k_num = function() perfil(ancha(100000L, 50L, FALSE)),
  ancha_100k_text = function() perfil(ancha(100000L, 50L, TRUE)),
  mixta_150k = function() perfil(mixta(150000L)),
  grande_1m = function() perfil(mixta(1e6)),
  grande_3m = function() perfil(mixta(3e6)),
  texto_largo_200k = function() {
    n <- 200000L
    nota <- vapply(seq_len(n), function(i) {
      paste(sample(c(letters, " "), 2000L, TRUE), collapse = "")
    }, character(1L))
    perfil(data.frame(id = seq_len(n), nota = nota, stringsAsFactors = FALSE))
  },
  analizar_120k = function() {
    d <- mixta(120000L)
    function() quieto(lupa::analizar(d, fecha = fecha))
  },
  patrones_2m = function() {
    x <- sprintf("T%09d", sample.int(1e9, 2e6))
    function() quieto(lupa::descubrir_patrones(x))
  },
  dbi_duckdb_1m = function() {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("dbi_duckdb_1m necesita duckdb.", call. = FALSE)
    }
    con <- DBI::dbConnect(duckdb::duckdb())
    d <- as.data.frame(replicate(10L, stats::rnorm(1e6), simplify = FALSE))
    names(d) <- paste0("x", seq_len(10L))
    DBI::dbWriteTable(con, "t", d)
    function() {
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      quieto(lupa::perfilar_dbi(con, "t"))
    }
  }
)
if (identical(escenario, "todos")) {
  cat(names(escenarios), sep = "\n")
  quit(save = "no")
}
if (!escenario %in% names(escenarios)) {
  stop("Escenario desconocido: ", escenario, call. = FALSE)
}

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instale lupa antes de ejecutar este benchmark.", call. = FALSE)
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n", sep = "")
cat("desde:  ", dirname(dirname(attr(.descripcion, "file"))), "\n\n", sep = "")

set.seed(7600L)
llamada <- escenarios[[escenario]]()
base <- sum(gc(reset = TRUE)[, 2L])
inicio <- proc.time()[["elapsed"]]
invisible(llamada())
segundos <- proc.time()[["elapsed"]] - inicio
memoria_mb <- sum(gc()[, 6L]) - base
cat(sprintf("%-20s %8.2f s %8.0f MB\n", escenario, segundos, memoria_mb))

salida <- data.frame(
  etiqueta = etiqueta, corrida = corrida, escenario = escenario,
  version = as.character(utils::packageVersion("lupa")),
  armado = .descripcion$Built, segundos = round(segundos, 3),
  memoria_mb = round(memoria_mb),
  r = paste(R.version$major, R.version$minor, sep = "."),
  sistema = Sys.info()[["sysname"]],
  medido = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)
destino <- file.path("benchmark", "datos", "medir-costo-huella.csv")
utils::write.table(salida, destino, sep = ",", row.names = FALSE,
                   col.names = !file.exists(destino), append = file.exists(destino),
                   qmethod = "double")
cat("agregado a ", destino, "\n", sep = "")
