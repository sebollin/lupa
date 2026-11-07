## Mide cuanto trae a R la muestra del motor (`universo = "muestra_motor"`)
## sobre tablas de valores muy largos, y deja un CSV fechado en
## benchmark/datos/ con el entorno y la huella del codigo.
##
## Uso:
##   Rscript benchmark/medir_tope_muestra.R
##   Rscript benchmark/medir_tope_muestra.R postgres16
##   Rscript benchmark/medir_tope_muestra.R --motores=postgres16,sqlserver2022
##
## Sin argumentos mide PostgreSQL 16 y SQL Server 2022, o los que nombre
## LUPA_MOTOR. Las conexiones son las de benchmark/verificar_motor.R, leidas del
## guion sin correrlo: los mismos contenedores locales y las mismas variables de
## entorno (LUPA_MSSQL_PWD, LUPA_DB_HOST, LUPA_DB_PORT, ...). Las contrasenas no
## forman parte del guion ni del CSV.
##
## Dos escenarios por motor, sobre tablas que el guion crea y borra
## (`lupa_tope_muestra`):
##
##   - `valores_largos`: 200 filas con un texto de 1.000.000 de caracteres y un
##     binario de 250.000 bytes, con los topes por omision. Los valores largos
##     viajan como su largo y su MD5, no enteros.
##   - `tope_bytes`: 2.000 filas con un texto de 8.992 caracteres (por debajo de
##     `max_largo_valor_vocabulario`, asi que viaja entero) con
##     `max_bytes_muestra = max_bytes_materializacion = 1e6`. Manda el tope de
##     bytes.
##
## Sale con 1 si un motor pedido no se pudo medir: una corrida que no midio no
## se lee como verde.

if (sys.nframe() > 0L) {
  stop("Este script se ejecuta con Rscript benchmark/medir_tope_muestra.R.",
       call. = FALSE)
}

.argumentos <- commandArgs(trailingOnly = TRUE)
.archivo_script <- grep("^--file=", commandArgs(), value = TRUE)
.archivo_script <- if (length(.archivo_script)) {
  # Rscript escribe los espacios de la ruta como `~+~` en `--file=`.
  gsub("~+~", " ", sub("^--file=", "", .archivo_script[[1L]]), fixed = TRUE)
} else {
  file.path(getwd(), "benchmark", "medir_tope_muestra.R")
}
.dir_bancos <- dirname(normalizePath(.archivo_script, mustWork = TRUE))
.raiz_paquete <- normalizePath(file.path(.dir_bancos, ".."), mustWork = TRUE)
if (file.exists(file.path(.raiz_paquete, "DESCRIPTION")) &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(.raiz_paquete, quiet = TRUE)
} else if (!requireNamespace("lupa", quietly = TRUE)) {
  stop("No se encuentra la fuente ni una instalacion de lupa.", call. = FALSE)
}
if (!requireNamespace("DBI", quietly = TRUE)) {
  stop("Falta el paquete DBI.", call. = FALSE)
}

# La huella es la de benchmark/_firma.R: la suma md5 de los archivos de R/,
# con `+sucio` si el codigo, este guion o la firma tienen cambios sin
# commitear.
source(file.path(.dir_bancos, "_firma.R"), local = TRUE)
.revision <- Sys.getenv("LUPA_REVISION", unset = "")
if (!nzchar(.revision)) {
  .revision <- paste0(
    as.character(utils::packageVersion("lupa")), "; ",
    .commit_actual(.raiz_paquete, .rutas_firma("medir_tope_muestra.R"))
  )
}

# Las conexiones de verificar_motor.R, sin correr el guion: se evaluan solo las
# definiciones que las arman. Una copia aca se quedaria vieja la primera vez
# que cambie un puerto o un controlador.
.motores <- new.env(parent = globalenv())
for (.expresion in parse(file.path(.dir_bancos, "verificar_motor.R"),
                         keep.source = FALSE)) {
  if (is.call(.expresion) && identical(.expresion[[1L]], as.name("<-")) &&
      as.character(.expresion[[2L]]) %in%
        c(".parametro", ".texto_seguro", ".normalizar", ".especificaciones")) {
    eval(.expresion, envir = .motores)
  }
}
if (!exists(".especificaciones", envir = .motores, inherits = FALSE)) {
  stop("benchmark/verificar_motor.R no define .especificaciones.",
       call. = FALSE)
}
.especificaciones <- get(".especificaciones", envir = .motores)
.texto_seguro <- get(".texto_seguro", envir = .motores)
.normalizar <- get(".normalizar", envir = .motores)

# Los motores que saben armar las tablas del banco. Los demas de la matriz no
# tienen un tipo de texto de un millon de caracteres con el mismo costo.
.CREAR <- list(
  postgres16 = list(
    valores_largos = c(
      "CREATE TABLE lupa_tope_muestra AS SELECT g AS id,",
      "repeat(md5(g::text), 31250) AS texto,",
      "decode(repeat(md5(g::text || 'b'), 15625), 'hex') AS binario",
      "FROM generate_series(1, 200) AS g"
    ),
    tope_bytes = c(
      "CREATE TABLE lupa_tope_muestra AS SELECT g AS id,",
      "repeat(md5(g::text), 281) AS texto",
      "FROM generate_series(1, 2000) AS g"
    ),
    peso_valores_largos = c(
      "SELECT SUM(octet_length(texto)) + SUM(octet_length(binario)) AS bytes,",
      "COUNT(*) AS filas FROM lupa_tope_muestra"
    ),
    peso_tope_bytes = c(
      "SELECT SUM(octet_length(texto)) AS bytes, COUNT(*) AS filas",
      "FROM lupa_tope_muestra"
    )
  ),
  sqlserver2022 = list(
    valores_largos = c(
      "SELECT TOP (200) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS id,",
      "REPLICATE(CONVERT(varchar(max), CONVERT(varchar(32),",
      "HASHBYTES('MD5', CAST(NEWID() AS varchar(36))), 2)), 31250) AS texto,",
      "CONVERT(varbinary(max), REPLICATE(CONVERT(varchar(max),",
      "'abcdefghijklmnop'), 15625)) AS binario",
      "INTO lupa_tope_muestra FROM sys.all_objects"
    ),
    tope_bytes = c(
      "SELECT TOP (2000) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS id,",
      "REPLICATE(CONVERT(varchar(max), CONVERT(varchar(32),",
      "HASHBYTES('MD5', CAST(NEWID() AS varchar(36))), 2)), 281) AS texto",
      "INTO lupa_tope_muestra FROM sys.all_objects a CROSS JOIN sys.all_objects b"
    ),
    peso_valores_largos = c(
      "SELECT SUM(CAST(DATALENGTH(texto) AS bigint)) +",
      "SUM(CAST(DATALENGTH(binario) AS bigint)) AS bytes,",
      "COUNT(*) AS filas FROM lupa_tope_muestra"
    ),
    peso_tope_bytes = c(
      "SELECT SUM(CAST(DATALENGTH(texto) AS bigint)) AS bytes,",
      "COUNT(*) AS filas FROM lupa_tope_muestra"
    )
  )
)
.ESCENARIOS <- list(
  valores_largos = list(muestra_motor = 200L, topes = list()),
  tope_bytes = list(
    muestra_motor = 2000L,
    topes = list(max_bytes_muestra = 1e6, max_bytes_materializacion = 1e6)
  )
)

.seleccionar <- function(argumentos) {
  argumentos <- argumentos[nzchar(argumentos)]
  if (!length(argumentos)) return(names(.CREAR))
  valores <- unlist(strsplit(
    sub("^--motores?=", "", argumentos), "[,[:space:]]+", perl = TRUE
  ), use.names = FALSE)
  valores <- valores[nzchar(valores)]
  ids <- vapply(valores, function(valor) {
    cual <- names(.especificaciones)[vapply(.especificaciones, function(x) {
      .normalizar(valor) %in% .normalizar(x$alias)
    }, logical(1L))]
    if (length(cual) != 1L) stop("No se reconoce el motor: ", valor, call. = FALSE)
    cual
  }, character(1L))
  fuera <- setdiff(ids, names(.CREAR))
  if (length(fuera)) {
    stop("El banco no sabe armar sus tablas en: ", paste(fuera, collapse = ", "),
         call. = FALSE)
  }
  unique(ids)
}

.seleccionados <- .seleccionar(
  if (length(.argumentos)) .argumentos else Sys.getenv("LUPA_MOTOR", unset = "")
)
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.sello <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
.entorno <- data.frame(
  fecha = .fecha,
  revision_paquete = .revision,
  huella_codigo = .revision,
  version_paquete = as.character(utils::packageVersion("lupa")),
  version_R = R.version.string,
  plataforma = R.version$platform,
  sistema = unname(Sys.info()[["sysname"]]),
  version_sistema = unname(Sys.info()[["release"]]),
  maquina = unname(Sys.info()[["machine"]]),
  nucleos = parallel::detectCores(logical = TRUE),
  stringsAsFactors = FALSE
)

.fila <- function(motor, version_servidor = NA_character_, escenario = NA_character_,
                  estado = "medido", motivo = NA_character_, filas_tabla = NA_real_,
                  bytes_tabla = NA_real_, muestra_motor = NA_real_,
                  max_bytes_muestra = NA_real_, max_bytes_materializacion = NA_real_,
                  filas_leidas = NA_real_, bytes_spool = NA_real_,
                  tope_que_mando = NA_character_, segundos = NA_real_,
                  pico_memoria_r_mb = NA_real_) {
  especificacion <- .especificaciones[[motor]]
  version_controlador <- if (requireNamespace(especificacion$paquete, quietly = TRUE)) {
    as.character(utils::packageVersion(especificacion$paquete))
  } else NA_character_
  data.frame(
    .entorno,
    motor = especificacion$etiqueta, version_servidor = version_servidor,
    controlador = especificacion$controlador,
    version_controlador = version_controlador,
    escenario = escenario, estado = estado, motivo = motivo,
    filas_tabla = filas_tabla, bytes_tabla = bytes_tabla,
    muestra_motor = muestra_motor, max_bytes_muestra = max_bytes_muestra,
    max_bytes_materializacion = max_bytes_materializacion,
    filas_leidas = filas_leidas, bytes_spool = bytes_spool,
    tope_que_mando = tope_que_mando, segundos = segundos,
    pico_memoria_r_mb = pico_memoria_r_mb,
    stringsAsFactors = FALSE
  )
}

.borrar <- function(conexion) {
  try(DBI::dbExecute(conexion, "DROP TABLE lupa_tope_muestra"), silent = TRUE)
}

.medir_escenario <- function(conexion, motor, version, escenario) {
  sql <- .CREAR[[motor]]
  configuracion <- .ESCENARIOS[[escenario]]
  .borrar(conexion)
  on.exit(.borrar(conexion), add = TRUE)
  DBI::dbExecute(conexion, paste(sql[[escenario]], collapse = " "))
  peso <- DBI::dbGetQuery(
    conexion, paste(sql[[paste0("peso_", escenario)]], collapse = " ")
  )
  topes <- configuracion$topes
  gc(reset = TRUE)
  inicio <- proc.time()[["elapsed"]]
  perfil <- suppressWarnings(suppressMessages(do.call(
    lupa::perfilar_dbi,
    c(list(conexion, "lupa_tope_muestra", universo = "muestra_motor",
           muestra_motor = configuracion$muestra_motor,
           proteger_datos_personales = FALSE), topes)
  )))
  segundos <- proc.time()[["elapsed"]] - inicio
  pico <- sum(gc()[, 6L])
  meta <- perfil$resumen_tabla$meta
  numero <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 1L) x else NA_real_
  }
  .fila(
    motor, version_servidor = version, escenario = escenario,
    filas_tabla = numero(peso$filas), bytes_tabla = numero(peso$bytes),
    muestra_motor = configuracion$muestra_motor,
    max_bytes_muestra = topes$max_bytes_muestra %||% lupa:::.MAX_BYTES_MUESTRA,
    max_bytes_materializacion =
      topes$max_bytes_materializacion %||% lupa:::.MAX_BYTES_MUESTRA,
    filas_leidas = numero(meta$muestreo$filas_obtenidas),
    bytes_spool = numero(meta$materializacion$bytes),
    tope_que_mando = as.character(meta$materializacion$tope_que_mando %||%
                                    NA_character_)[[1L]],
    segundos = segundos, pico_memoria_r_mb = pico
  )
}

.medir_motor <- function(motor) {
  especificacion <- .especificaciones[[motor]]
  if (!requireNamespace(especificacion$paquete, quietly = TRUE)) {
    return(.fila(motor, estado = "no_medido",
                 motivo = paste0("falta el paquete ", especificacion$paquete)))
  }
  if (!is.null(especificacion$variable) &&
      !nzchar(Sys.getenv(especificacion$variable)) &&
      !nzchar(Sys.getenv("LUPA_DB_PASS"))) {
    return(.fila(motor, estado = "no_medido",
                 motivo = paste0("falta ", especificacion$variable)))
  }
  conexion <- tryCatch(especificacion$conectar(), error = function(e) e)
  if (inherits(conexion, "error")) {
    return(.fila(motor, estado = "no_medido",
                 motivo = .texto_seguro(conexion)))
  }
  on.exit(try(DBI::dbDisconnect(conexion), silent = TRUE), add = TRUE)
  version <- tryCatch(
    as.character(DBI::dbGetQuery(conexion, especificacion$version_sql)[[1L]][[1L]]),
    error = function(e) NA_character_
  )
  version <- gsub("[[:space:]]+", " ", version)
  filas <- lapply(names(.ESCENARIOS), function(escenario) {
    tryCatch(
      .medir_escenario(conexion, motor, version, escenario),
      error = function(e) .fila(
        motor, version_servidor = version, escenario = escenario,
        estado = "no_medido", motivo = .texto_seguro(e)
      )
    )
  })
  do.call(rbind, filas)
}

salida <- do.call(rbind, lapply(.seleccionados, .medir_motor))
rownames(salida) <- NULL
print(salida[, c("motor", "escenario", "estado", "filas_tabla", "bytes_tabla",
                 "filas_leidas", "bytes_spool", "tope_que_mando", "segundos",
                 "pico_memoria_r_mb")], row.names = FALSE)
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(
  directorio_datos, paste0("medir-tope-muestra-", .sello, ".csv")
)
# Los bytes de la tabla (2,5e8) se escriben enteros: el CSV se lee tambien a ojo.
options(scipen = 100)
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
cat("Salida:", archivo_salida, "\n")
if (any(salida$estado != "medido")) quit(status = 1L)
