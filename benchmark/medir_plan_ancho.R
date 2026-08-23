## Mide el costo que declara `plan_perfilado_dbi()` sobre una tabla ancha, y
## cuantas de esas consultas tocan la tabla entera.
##
## Es la afirmacion "el costo se planifica antes de pagarlo" de los README.
## Existia como numero -158 columnas, 623 consultas, 777 de 778- medido una vez
## contra una base que no esta en el repositorio, asi que **nadie podia
## rehacerlo**. Peor: el numero se leia como una propiedad de "158 columnas",
## cuando depende de la composicion. Una tabla de 158 columnas casi todas de
## texto cuesta la mitad que una casi toda numerica, porque la mediana pide un
## orden total por columna numerica.
##
## Este banco arma la tabla, publica el desglose por clase de consulta y separa
## las que escanean u ordenan la tabla completa de las que no. No necesita red
## ni base externa: SQLite en memoria alcanza, porque el plan se predice sin
## ejecutar ninguna de las consultas que cuenta.

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instalar `lupa` antes de correr el banco.", call. = FALSE)
}
for (paquete in c("DBI", "RSQLite")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Instalar `", paquete, "` antes de correr el banco.", call. = FALSE)
  }
}

## Medir contra la biblioteca equivocada ya costo una vez publicar un numero
## falso, asi que el script dice contra que esta midiendo antes de medir.
.descripcion <- utils::packageDescription("lupa")
cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("armado: ", .descripcion$Built, "\n\n", sep = "")

## ---- La tabla ------------------------------------------------------------
##
## 158 columnas con la mezcla de una tabla administrativa: mas texto que
## numeros, algunos enteros de codigo y algunas fechas guardadas como texto.
set.seed(1)
N_FILAS <- 200L
columnas <- list()
for (i in seq_len(60L)) {
  columnas[[paste0("num", i)]] <- round(stats::rnorm(N_FILAS) * 100, 2)
}
for (i in seq_len(60L)) {
  columnas[[paste0("txt", i)]] <- sample(letters, N_FILAS, replace = TRUE)
}
for (i in seq_len(20L)) {
  columnas[[paste0("ent", i)]] <- sample(seq_len(50L), N_FILAS, replace = TRUE)
}
for (i in seq_len(18L)) {
  columnas[[paste0("fec", i)]] <- as.character(
    as.Date("2026-01-01") - sample(seq_len(900L), N_FILAS, replace = TRUE)
  )
}
tabla <- as.data.frame(columnas, stringsAsFactors = FALSE)

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(conexion), add = TRUE)
DBI::dbWriteTable(conexion, "ancha", tabla)

## ---- El plan -------------------------------------------------------------
plan <- lupa::plan_perfilado_dbi(conexion, "ancha", modo = "exacto")

total <- attr(plan, "total")
techo <- attr(plan, "total_lotes_rechazados")

## Las clases que no tocan la tabla entera: las sondas de apertura y la lectura
## de la muestra. El resto escanea, ordena o agrupa toda la tabla, y ese es el
## punto: `muestra` acota lo que se trae a R, no el trabajo del motor.
alcance <- as.character(plan$alcance)
sin_escanear <- alcance %in% c("una vez", "lee las filas pedidas")
tabla_entera <- sum(plan$n_consultas[!sin_escanear])

cat("columnas: ", ncol(tabla), " sobre ", N_FILAS, " filas\n", sep = "")
cat("consultas si ningun lote se rechaza: ", total, "\n", sep = "")
cat("consultas si se rechazan todos los lotes: ", techo, "\n", sep = "")
cat("de las ", total, ", escanean u ordenan la tabla entera: ", tabla_entera,
    "\n\n", sep = "")
print(plan[, c("clase_consulta", "n_consultas", "alcance")])

## ---- Lo que cambia con la composicion -------------------------------------
##
## La misma cantidad de columnas, todas de texto: sin mediana ni desvio ni
## MIN/MAX numerico, el costo cae. Es la razon por la que el numero del README
## no puede presentarse como una propiedad de "158 columnas".
solo_texto <- as.data.frame(
  stats::setNames(
    lapply(seq_len(158L), function(i) sample(letters, N_FILAS, replace = TRUE)),
    paste0("txt", seq_len(158L))
  ),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(conexion, "ancha_texto", solo_texto)
plan_texto <- lupa::plan_perfilado_dbi(conexion, "ancha_texto", modo = "exacto")
cat("\n158 columnas todas de texto: ", attr(plan_texto, "total"),
    " consultas\n", sep = "")

## ---- Lo que acota `muestra` ----------------------------------------------
plan_muestreado <- lupa::plan_perfilado_dbi(
  conexion, "ancha", modo = "muestreado"
)
cat("modo muestreado sobre la misma tabla: ", attr(plan_muestreado, "total"),
    " consultas\n", sep = "")
