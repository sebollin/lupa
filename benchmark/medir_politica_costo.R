## Mide, en consultas y no en segundos, cuanto cambia la politica de costo
## sobre una tabla ancha reproducible. La politica por omision sigue siendo
## "todas"; este guion mide la alternativa sin cambiar ese valor.

.archivo_script <- grep("^--file=", commandArgs(), value = TRUE)
.archivo_script <- if (length(.archivo_script)) {
  sub("^--file=", "", .archivo_script[[1L]])
} else {
  file.path(getwd(), "benchmark", "medir_politica_costo.R")
}
.raiz_paquete <- normalizePath(
  file.path(dirname(normalizePath(.archivo_script, mustWork = FALSE)), ".."),
  mustWork = TRUE
)
if (file.exists(file.path(.raiz_paquete, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.raiz_paquete, quiet = TRUE)
} else if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("No se encuentra la fuente ni una instalación de lupa.", call. = FALSE)
}
for (paquete in c("DBI", "RSQLite")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Falta el paquete ", paquete, ".", call. = FALSE)
  }
}

# La revision medida es la huella del codigo de R/, como en verificar_motor.R:
# sobrevive a reescribir la historia y se rehace desde cualquier copia.
.revision <- Sys.getenv("LUPA_REVISION", unset = "")
if (!nzchar(.revision)) {
  .archivos_r <- sort(list.files(file.path(.raiz_paquete, "R"), "\\.R$",
                                 full.names = TRUE))
  .lista_r <- tempfile()
  writeLines(paste(unname(tools::md5sum(.archivos_r)), basename(.archivos_r)), .lista_r)
  .revision <- paste0(as.character(utils::packageVersion("lupa")), "; R/ md5 ",
                      substr(unname(tools::md5sum(.lista_r)), 1L, 12L))
  unlink(.lista_r)
}
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.sello <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
.entorno <- data.frame(
  fecha = .fecha,
  revision_paquete = .revision,
  version_paquete = as.character(utils::packageVersion("lupa")),
  version_R = R.version.string,
  plataforma = R.version$platform,
  sistema = unname(Sys.info()[["sysname"]]),
  version_sistema = unname(Sys.info()[["release"]]),
  maquina = unname(Sys.info()[["machine"]]),
  nucleos = parallel::detectCores(logical = TRUE),
  stringsAsFactors = FALSE
)

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
    conexion, "ancha", universo = "tabla_completa",
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

cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("revision: ", .revision, "\n", sep = "")
cat("fecha: ", .fecha, "\n", sep = "")
cat("columnas: ", ncol(tabla), " (",
    sum(vapply(tabla, is.numeric, logical(1L))), " numericas)\n", sep = "")
cat("columnas afectadas por la politica: ", length(omitidas), "\n", sep = "")
cat("modas omitidas: ",
    sum(vapply(decisiones, function(x) !isTRUE(x$moda), logical(1L))),
    "\n", sep = "")
cat("medianas omitidas: ",
    sum(vapply(decisiones, function(x) !isTRUE(x$mediana), logical(1L))),
    "\n", sep = "")
cat("consultas con todas: ", consultas_todas, "\n", sep = "")
cat("consultas con cardinalidad: ", consultas_selectivas, "\n", sep = "")
cat("ahorro de consultas: ", consultas_todas - consultas_selectivas,
    "\n", sep = "")

.fila <- function(politica, medida, valor, unidad = NA_character_,
                  columna = NA_character_) {
  data.frame(
    politica = politica, medida = medida, valor = as.character(valor),
    unidad = unidad, columna = columna, stringsAsFactors = FALSE
  )
}

resumen <- rbind(
  .fila("todas", "columnas", ncol(tabla), "columna"),
  .fila("por_cardinalidad", "columnas_afectadas", length(omitidas), "columna"),
  .fila("por_cardinalidad", "modas_omitidas",
        sum(vapply(decisiones, function(x) !isTRUE(x$moda), logical(1L))),
        "columna"),
  .fila("por_cardinalidad", "medianas_omitidas",
        sum(vapply(decisiones, function(x) !isTRUE(x$mediana), logical(1L))),
        "columna"),
  .fila("todas", "consultas", consultas_todas, "consulta"),
  .fila("por_cardinalidad", "consultas", consultas_selectivas, "consulta"),
  .fila("por_cardinalidad", "ahorro_consultas",
        consultas_todas - consultas_selectivas, "consulta")
)
detalle <- do.call(rbind, lapply(names(decisiones), function(columna) {
  decision <- decisiones[[columna]]
  rbind(
    .fila("por_cardinalidad", "moda", isTRUE(decision$moda),
          "se_ejecuta", columna),
    .fila("por_cardinalidad", "mediana", isTRUE(decision$mediana),
          "se_ejecuta", columna),
    .fila("por_cardinalidad", "distintos", isTRUE(decision$distintos),
          "se_ejecuta", columna)
  )
}))
salida <- rbind(resumen, detalle)
salida <- cbind(.entorno[rep(1L, nrow(salida)), , drop = FALSE], salida)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(
  directorio_datos, paste0("medir-politica-costo-", .sello, ".csv")
)
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("Salida:", archivo_salida, "\n")
