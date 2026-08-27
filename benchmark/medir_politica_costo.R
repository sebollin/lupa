## Mide, en consultas y no en segundos, cuanto ahorra la politica optativa
## sobre una tabla ancha reproducible. La politica por omision sigue siendo
## "todas"; este guion mide la alternativa sin cambiar el resultado por defecto.

if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("Instalar `lupa` antes de correr el banco.", call. = FALSE)
}
for (paquete in c("DBI", "RSQLite")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Instalar `", paquete, "` antes de correr el banco.", call. = FALSE)
  }
}

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

medir <- function(politica) {
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "ancha", tabla)
  lupa::perfilar_dbi(
    conexion, "ancha", modo = "exacto",
    bloque_muestra = "solo_agregados", instrumentar = FALSE,
    politica_costo = politica, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, duplicados_aproximados = FALSE
  )
}

todas <- medir("todas")
por_cardinalidad <- medir("por_cardinalidad")
decisiones <- por_cardinalidad$resumen_tabla$meta$decisiones_costo
omitidas <- names(decisiones)[vapply(
  decisiones, function(x) !isTRUE(x$moda), logical(1L)
)]
consultas_todas <- todas$resumen_tabla$meta$consultas$emitidas
consultas_selectivas <- por_cardinalidad$resumen_tabla$meta$consultas$emitidas

cat(
  "columnas: ", ncol(tabla), " (",
  sum(vapply(tabla, is.numeric, logical(1L))), " numericas)\n", sep = ""
)
cat("columnas afectadas por la politica: ", length(omitidas), "\n", sep = "")
cat(
  "modas omitidas: ",
  sum(vapply(decisiones, function(x) !isTRUE(x$moda), logical(1L))), "\n",
  sep = ""
)
cat(
  "medianas omitidas: ",
  sum(vapply(decisiones, function(x) !isTRUE(x$mediana), logical(1L))), "\n",
  sep = ""
)
cat("consultas con todas: ", consultas_todas, "\n", sep = "")
cat("consultas con cardinalidad: ", consultas_selectivas, "\n", sep = "")
cat("ahorro de consultas: ", consultas_todas - consultas_selectivas, "\n", sep = "")
