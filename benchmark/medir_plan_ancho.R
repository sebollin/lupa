## Mide el costo declarado por plan_perfilado_dbi() sobre una tabla ancha.
##
## El resultado no ejecuta las consultas del plan. Compara una tabla mixta,
## una tabla sólo de texto y un plan con muestra del motor, y deja un CSV
## fechado junto con el entorno que lo produjo.

.archivo_script <- grep("^--file=", commandArgs(), value = TRUE)
.archivo_script <- if (length(.archivo_script)) {
  sub("^--file=", "", .archivo_script[[1L]])
} else {
  file.path(getwd(), "benchmark", "medir_plan_ancho.R")
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

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(conexion), add = TRUE)
DBI::dbWriteTable(conexion, "ancha", tabla)

plan <- lupa::plan_perfilado_dbi(
  conexion, "ancha", universo = "tabla_completa"
)
solo_texto <- as.data.frame(
  stats::setNames(
    lapply(seq_len(158L), function(i) sample(letters, N_FILAS, replace = TRUE)),
    paste0("txt", seq_len(158L))
  ),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(conexion, "ancha_texto", solo_texto)
plan_texto <- lupa::plan_perfilado_dbi(
  conexion, "ancha_texto", universo = "tabla_completa"
)
plan_muestreado <- lupa::plan_perfilado_dbi(
  conexion, "ancha", universo = "muestra_motor",
  muestra_motor = 100L, muestra = 100L
)

.alcance <- as.character(plan$alcance)
sin_escanear <- .alcance %in% c("una vez", "lee las filas pedidas")
tabla_entera <- sum(plan$n_consultas[!sin_escanear])

cat("lupa ", as.character(utils::packageVersion("lupa")), "\n", sep = "")
cat("revision: ", .revision, "\n", sep = "")
cat("fecha: ", .fecha, "\n", sep = "")
cat("columnas: ", ncol(tabla), " sobre ", N_FILAS, " filas\n", sep = "")
cat("consultas tabla completa: ", attr(plan, "total"), "\n", sep = "")
cat("consultas si se rechazan todos los lotes: ",
    attr(plan, "total_lotes_rechazados"), "\n", sep = "")
cat("consultas que escanean u ordenan la tabla entera: ",
    tabla_entera, "\n", sep = "")
cat("158 columnas todas de texto: ", attr(plan_texto, "total"),
    " consultas\n", sep = "")
cat("muestra del motor: ", attr(plan_muestreado, "total"),
    " consultas\n", sep = "")
print(plan[, c("clase_consulta", "n_consultas", "alcance")])

.fila <- function(escenario, medida, valor, unidad = NA_character_,
                  clase = NA_character_, n_consultas = NA_real_,
                  alcance = NA_character_) {
  data.frame(
    escenario = escenario, medida = medida, valor = as.character(valor),
    unidad = unidad, clase_consulta = clase, n_consultas = n_consultas,
    alcance = alcance, stringsAsFactors = FALSE
  )
}

resumen <- rbind(
  .fila("ancha_tabla_completa", "columnas", ncol(tabla), "columna"),
  .fila("ancha_tabla_completa", "filas_fixture", N_FILAS, "fila"),
  .fila("ancha_tabla_completa", "consultas", attr(plan, "total"), "consulta"),
  .fila("ancha_tabla_completa", "consultas_maximas",
        attr(plan, "total_lotes_rechazados"), "consulta"),
  .fila("ancha_tabla_completa", "consultas_tabla_entera",
        tabla_entera, "consulta"),
  .fila("ancha_solo_texto", "columnas", ncol(solo_texto), "columna"),
  .fila("ancha_solo_texto", "consultas", attr(plan_texto, "total"), "consulta"),
  .fila("ancha_muestra_motor", "muestra_motor", 100L, "fila"),
  .fila("ancha_muestra_motor", "consultas", attr(plan_muestreado, "total"),
        "consulta")
)
detalle <- rbind(
  .fila(
    "ancha_tabla_completa", "plan", NA_character_, NA_character_,
    as.character(plan$clase_consulta), plan$n_consultas, plan$alcance
  ),
  .fila(
    "ancha_solo_texto", "plan", NA_character_, NA_character_,
    as.character(plan_texto$clase_consulta), plan_texto$n_consultas,
    plan_texto$alcance
  ),
  .fila(
    "ancha_muestra_motor", "plan", NA_character_, NA_character_,
    as.character(plan_muestreado$clase_consulta),
    plan_muestreado$n_consultas, plan_muestreado$alcance
  )
)
salida <- rbind(resumen, detalle)
salida <- cbind(.entorno[rep(1L, nrow(salida)), , drop = FALSE], salida)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(
  directorio_datos, paste0("medir-plan-ancho-", .sello, ".csv")
)
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("Salida:", archivo_salida, "\n")
