## Mide como escala con la cantidad de hilos la deteccion de duplicados
## aproximados, y publica la tabla que la vinieta `escala-y-duplicados` usa
## para justificar el valor por omision de dos hilos.
##
## Esa tabla existia como numero -133,28 s con dos hilos, 70,31 con dieciseis,
## sobre 100.000 filas del padron dificil con 140.097.499 candidatos- medido una
## vez contra un conjunto que no esta en el repositorio, asi que **nadie podia
## rehacerlo**. Es el mismo defecto que la tabla de evidencia del README cerro
## para sus propias filas: un numero publicado que nadie puede comprobar se
## vuelve mentira sin que nadie se entere.
##
## El padron sintetico no reproduce los segundos del original -otro conjunto,
## otra maquina-. Lo que reproduce es **la forma de la curva**, que es lo que la
## vinieta afirma: la ganancia se agota mucho antes de usar todos los nucleos, y
## el resultado no cambia con los hilos.
##
## Necesita la maquina libre. Con otra cosa corriendo los tiempos no significan
## nada, y este banco existe para medir tiempos.

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instalar `lupa` antes de correr el banco.", call. = FALSE)
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Instalar `stringdist` antes de correr el banco.", call. = FALSE)
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n", sep = "")
cat("nucleos de la maquina: ", parallel::detectCores(), "\n", sep = "")
cat("R: ", R.version.string, "\n\n", sep = "")

## ---- El padron dificil ----------------------------------------------------
##
## Dificil quiere decir que los nombres se parecen entre si: sin eso el
## bloqueo descarta casi todo y la medicion no ejercita la comparacion, que es
## lo que se quiere cronometrar.
N_FILAS <- 100000L
set.seed(20260823L)

nombres <- c("ana", "juan", "maria", "jose", "luis", "carmen", "pedro", "rosa",
             "carlos", "marta", "jorge", "silvia", "raul", "elena", "diego")
apellidos <- c("perez", "gomez", "rodriguez", "fernandez", "lopez", "martinez",
               "gonzalez", "sanchez", "romero", "suarez", "alvarez", "torres")
calles <- c("rivera", "artigas", "sarandi", "colonia", "mercedes", "canelones",
            "durazno", "maldonado", "soriano", "cerrito")

.variar <- function(x) {
  # Erratas del tipo que produce la carga manual: una letra cambiada, una
  # duplicada, un acento perdido. Son las que tienen que caer en el mismo grupo.
  i <- sample.int(nchar(x), 1L)
  switch(
    sample(3L, 1L),
    paste0(substr(x, 1L, i - 1L), sample(letters, 1L), substr(x, i + 1L, nchar(x))),
    paste0(substr(x, 1L, i), substr(x, i, nchar(x))),
    sub("a", "", x, fixed = TRUE)
  )
}

base_nombres <- paste(
  sample(nombres, N_FILAS, replace = TRUE),
  sample(apellidos, N_FILAS, replace = TRUE)
)
# Una de cada diez filas es una variante de otra: son los pares que hay que
# encontrar, y los que hacen que el trabajo no sea trivial.
variantes <- sample.int(N_FILAS, N_FILAS %/% 10L)
base_nombres[variantes] <- vapply(base_nombres[variantes], .variar, character(1L),
                                  USE.NAMES = FALSE)

padron <- data.frame(
  nombre = base_nombres,
  domicilio = paste(
    sample(calles, N_FILAS, replace = TRUE),
    sample.int(3000L, N_FILAS, replace = TRUE)
  ),
  stringsAsFactors = FALSE
)

cat("padron: ", nrow(padron), " filas, ",
    length(unique(padron$nombre)), " nombres distintos\n\n", sep = "")

## ---- La medicion ----------------------------------------------------------
##
## Tres corridas por configuracion y se informa la mediana: una sola corrida
## mide tambien lo que el sistema operativo estuviera haciendo en ese momento.
HILOS <- c(2L, 4L, 8L, 16L, max(2L, parallel::detectCores() - 1L))
REPETICIONES <- 3L

medir <- function(nucleos) {
  tiempos <- numeric(REPETICIONES)
  pares <- NA_integer_
  for (i in seq_len(REPETICIONES)) {
    reloj <- system.time(
      resultado <- lupa::detectar_duplicados_aproximados(
        padron, umbral = 0.15, nucleos = nucleos, max_resultados = 1000L
      )
    )
    tiempos[[i]] <- reloj[["elapsed"]]
    pares <- nrow(resultado)
  }
  list(mediana = stats::median(tiempos), pares = pares,
       alcance = attr(resultado, "alcance"))
}

cat(sprintf("%6s %14s %14s %12s\n", "hilos", "mediana (s)", "relativo a 2", "pares"))
resultados <- list()
for (nucleos in HILOS) {
  medida <- medir(nucleos)
  resultados[[as.character(nucleos)]] <- medida
  base <- resultados[["2"]]$mediana
  cat(sprintf("%6d %14.2f %13.2fx %12d\n", nucleos, medida$mediana,
              medida$mediana / base, medida$pares))
}

## ---- Lo que la vinieta afirma ---------------------------------------------
##
## Que la cantidad de hilos cambia el reloj y no el resultado. Si esto fallara,
## la tabla de tiempos seria lo de menos.
pares <- vapply(resultados, function(x) x$pares, integer(1L))
cat("\npares informados por configuracion: ",
    paste(pares, collapse = ", "), "\n", sep = "")
cat("el resultado no depende de los hilos: ",
    if (length(unique(pares)) == 1L) "SI" else "NO", "\n", sep = "")

mejor <- names(resultados)[[which.min(vapply(resultados, function(x) x$mediana,
                                             numeric(1L)))]]
cat("configuracion mas rapida: ", mejor, " hilos\n", sep = "")
cat("ganancia de 2 a ", mejor, " hilos: ",
    sprintf("%.2fx", resultados[["2"]]$mediana /
              resultados[[mejor]]$mediana), "\n", sep = "")
