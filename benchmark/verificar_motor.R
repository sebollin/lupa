## Comprueba la superficie DBI de lupa contra los motores de la matriz.
##
## Uso:
##   Rscript benchmark/verificar_motor.R
##   Rscript benchmark/verificar_motor.R postgres16 mysql84
##   Rscript benchmark/verificar_motor.R --motores=postgres16,sqlite
##
## Las contrasenas no forman parte del guion. Para MySQL, MariaDB y SQL Server
## se leen de LUPA_MYSQL_PWD, LUPA_MARIADB_PWD y LUPA_MSSQL_PWD.
##
## Sin argumentos mide los siete motores, o los que nombre LUPA_MOTOR. Por omision
## se conecta a los contenedores locales; otro servidor se declara con
## LUPA_DB_HOST, LUPA_DB_PORT, LUPA_DB_NAME, LUPA_DB_USER y LUPA_DB_PASS, que valen
## para todos los motores con servidor que se pidan (el flujo de CI las fija para
## el unico que levanta). Sale con 1 si algun motor fallo o no se pudo medir: una
## corrida que no midio no se lee como verde.

.argumentos <- commandArgs(trailingOnly = TRUE)
.archivo_script <- grep("^--file=", commandArgs(), value = TRUE)
.archivo_script <- if (length(.archivo_script)) {
  sub("^--file=", "", .archivo_script[[1L]])
} else {
  file.path(getwd(), "benchmark", "verificar_motor.R")
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

# La revision medida es la huella del codigo de R/: la version del paquete y la
# suma md5 de sus archivos. Un hash de commit deja de existir si la historia se
# reescribe, y cambia al commitear el propio CSV aunque el codigo sea el mismo;
# la huella se rehace desde cualquier copia del paquete, tambien sin git.
.revision_paquete <- function() {
  declarada <- Sys.getenv("LUPA_REVISION", unset = "")
  if (nzchar(declarada)) return(declarada)
  archivos <- sort(list.files(file.path(.raiz_paquete, "R"), "\\.R$",
                              full.names = TRUE))
  sumas <- unname(tools::md5sum(archivos))
  lista <- tempfile()
  on.exit(unlink(lista), add = TRUE)
  writeLines(paste(sumas, basename(archivos)), lista)
  paste0(as.character(utils::packageVersion("lupa")), "; R/ md5 ",
         substr(unname(tools::md5sum(lista)), 1L, 12L))
}

.parametro <- function(nombre, por_omision) {
  valor <- Sys.getenv(paste0("LUPA_DB_", nombre), unset = "")
  if (nzchar(valor)) valor else por_omision
}

.texto_seguro <- function(x) {
  texto <- if (inherits(x, "condition")) conditionMessage(x) else as.character(x)
  for (nombre in c("LUPA_MYSQL_PWD", "LUPA_MARIADB_PWD", "LUPA_MSSQL_PWD",
                   "LUPA_DB_PASS")) {
    secreto <- Sys.getenv(nombre, unset = "")
    if (nzchar(secreto)) texto <- gsub(secreto, "<omitido>", texto, fixed = TRUE)
  }
  gsub("(PWD|Password|password)[[:space:]]*=[[:space:]]*[^;[:space:]]+",
       "\\1=<omitido>", texto, perl = TRUE)
}

.normalizar <- function(x) {
  x <- iconv(tolower(trimws(x)), to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]+", "", x)
}

.especificaciones <- list(
  postgres16 = list(
    etiqueta = "PostgreSQL 16", alias = c("postgres16", "postgresql16", "pg16",
                                           "postgres"),
    paquete = "RPostgres", controlador = "RPostgres",
    version_sql = "SELECT version() AS version_servidor",
    conectar = function() DBI::dbConnect(
      RPostgres::Postgres(), host = .parametro("HOST", "127.0.0.1"),
      port = as.integer(.parametro("PORT", "55432")),
      dbname = .parametro("NAME", "lupa"), user = .parametro("USER", "postgres"),
      password = .parametro("PASS", "lupa"),
      connect_timeout = 5
    )
  ),
  postgres93 = list(
    etiqueta = "PostgreSQL 9.3", alias = c("postgres93", "postgresql93", "pg93",
                                           "postgresql925"),
    paquete = "RPostgres", controlador = "RPostgres",
    version_sql = "SELECT version() AS version_servidor",
    conectar = function() DBI::dbConnect(
      RPostgres::Postgres(), host = .parametro("HOST", "127.0.0.1"),
      port = as.integer(.parametro("PORT", "55493")),
      dbname = .parametro("NAME", "lupa"), user = .parametro("USER", "postgres"),
      password = .parametro("PASS", "lupa"),
      connect_timeout = 5
    )
  ),
  mysql84 = list(
    etiqueta = "MySQL 8.4", alias = c("mysql84", "mysql8", "mysql"),
    paquete = "RMySQL", controlador = "RMySQL",
    variable = "LUPA_MYSQL_PWD",
    version_sql = "SELECT VERSION() AS version_servidor",
    conectar = function() DBI::dbConnect(
      RMySQL::MySQL(), host = .parametro("HOST", "127.0.0.1"),
      port = as.integer(.parametro("PORT", "33307")),
      dbname = .parametro("NAME", "lupa"), username = .parametro("USER", "root"),
      password = .parametro("PASS", Sys.getenv("LUPA_MYSQL_PWD"))
    )
  ),
  mariadb118 = list(
    etiqueta = "MariaDB 11.8", alias = c("mariadb118", "mariadb11", "mariadb"),
    paquete = "RMariaDB", controlador = "RMariaDB",
    variable = "LUPA_MARIADB_PWD",
    version_sql = "SELECT VERSION() AS version_servidor",
    conectar = function() DBI::dbConnect(
      RMariaDB::MariaDB(), host = .parametro("HOST", "127.0.0.1"),
      port = as.integer(.parametro("PORT", "33306")),
      dbname = .parametro("NAME", "lupa"), username = .parametro("USER", "root"),
      password = .parametro("PASS", Sys.getenv("LUPA_MARIADB_PWD"))
    )
  ),
  sqlserver2022 = list(
    etiqueta = "SQL Server 2022", alias = c("sqlserver2022", "sqlserver", "mssql"),
    paquete = "odbc", controlador = "odbc/FreeTDS",
    variable = "LUPA_MSSQL_PWD",
    version_sql = "SELECT @@VERSION AS version_servidor",
    conectar = function() DBI::dbConnect(
      odbc::odbc(), Driver = "FreeTDS", Server = .parametro("HOST", "127.0.0.1"),
      Port = as.integer(.parametro("PORT", "1433")), UID = .parametro("USER", "sa"),
      PWD = .parametro("PASS", Sys.getenv("LUPA_MSSQL_PWD")),
      Database = .parametro("NAME", "lupa_pruebas"), TDS_Version = "8.0",
      TrustServerCertificate = "yes", timeout = 5
    )
  ),
  sqlite = list(
    etiqueta = "SQLite", alias = c("sqlite"),
    paquete = "RSQLite", controlador = "RSQLite",
    version_sql = "SELECT sqlite_version() AS version_servidor",
    conectar = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ),
  duckdb = list(
    etiqueta = "DuckDB", alias = c("duckdb"),
    paquete = "duckdb", controlador = "duckdb",
    version_sql = "SELECT version() AS version_servidor",
    conectar = function() DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  )
)

.indice_especificacion <- function(nombre) {
  normalizado <- .normalizar(nombre)
  vapply(.especificaciones, function(x) {
    normalizado %in% .normalizar(x$alias)
  }, logical(1L))
}

.seleccionar <- function(argumentos) {
  if (any(argumentos %in% c("--help", "-h"))) {
    cat("Uso: Rscript benchmark/verificar_motor.R [motores separados por coma]\n")
    cat("Motores:", paste(names(.especificaciones), collapse = ", "), "\n")
    quit(status = 0)
  }
  argumentos <- argumentos[nzchar(argumentos)]
  if (!length(argumentos)) return(names(.especificaciones))
  valores <- unlist(strsplit(
    sub("^--motores?=", "", argumentos), "[,[:space:]]+", perl = TRUE
  ), use.names = FALSE)
  valores <- valores[nzchar(valores)]
  indices <- vapply(valores, .indice_especificacion, logical(length(.especificaciones)))
  if (any(colSums(indices) != 1L)) {
    desconocidos <- valores[colSums(indices) == 0L]
    ambiguos <- valores[colSums(indices) > 1L]
    mensaje <- c(
      if (length(desconocidos)) paste("No se reconoce:", paste(desconocidos, collapse = ", ")),
      if (length(ambiguos)) paste("Ambiguos:", paste(ambiguos, collapse = ", "))
    )
    stop(paste(mensaje, collapse = " "), call. = FALSE)
  }
  ids <- vapply(seq_len(ncol(indices)), function(i) {
    names(.especificaciones)[which(indices[, i])[1L]]
  }, character(1L))
  unique(ids)
}

.seleccionados <- .seleccionar(
  if (length(.argumentos)) .argumentos else Sys.getenv("LUPA_MOTOR", unset = "")
)
.revision <- .revision_paquete()
.fecha <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
.fixture <- paste(
  "tramites: cinco columnas; padron: clave primaria id_persona",
  sep = ""
)
.columnas_resultado <- c(
  "motor", "version_servidor", "controlador", "version_controlador",
  "fixture", "revision_paquete", "fecha", "estado_motor", "motivo",
  "cinco_columnas", "dialecto", "media", "clave_primaria", "cobertura"
)

.fila_base <- function(especificacion) {
  version_controlador <- if (requireNamespace(especificacion$paquete, quietly = TRUE)) {
    as.character(utils::packageVersion(especificacion$paquete))
  } else NA_character_
  as.data.frame(
    setNames(
      list(
        especificacion$etiqueta, NA_character_, especificacion$controlador,
        version_controlador, .fixture, .revision, .fecha, NA_character_,
        NA_character_, NA_character_, NA_character_, NA_character_,
        NA_character_, NA_character_
      ),
      .columnas_resultado
    ),
    stringsAsFactors = FALSE
  )
}

.fila_no_medido <- function(especificacion, motivo) {
  fila <- .fila_base(especificacion)
  motivo <- paste("no responde:", .texto_seguro(motivo))
  fila$estado_motor <- "no_medido"
  fila$motivo <- motivo
  for (columna in c("cinco_columnas", "dialecto", "media",
                    "clave_primaria", "cobertura")) {
    fila[[columna]] <- motivo
  }
  fila
}

.datos_fixture <- function(n = 500L) {
  set.seed(11)
  data.frame(
    id_tramite = seq_len(n),
    documento = sample(10000000:49999999, n, replace = TRUE),
    categoria = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    monto = round(stats::rlnorm(n, 9, 1.1), 2),
    edad = c(sample(18:85, n - 20L, replace = TRUE), rep(NA_integer_, 20L)),
    stringsAsFactors = FALSE
  )
}

.insertar_sql <- function(conexion, tabla, datos) {
  valor_sql <- function(x) {
    if (is.na(x)) return("NULL")
    if (is.character(x)) return(as.character(DBI::dbQuoteString(conexion, x)))
    format(x, scientific = FALSE, trim = TRUE, decimal.mark = ".")
  }
  columnas <- paste(
    as.character(DBI::dbQuoteIdentifier(conexion, names(datos))),
    collapse = ", "
  )
  filas <- vapply(seq_len(nrow(datos)), function(i) {
    paste0("(", paste(vapply(datos[i, , drop = FALSE], valor_sql,
                              character(1L)), collapse = ", "), ")")
  }, character(1L))
  DBI::dbExecute(
    conexion,
    paste0(
      "INSERT INTO ", as.character(DBI::dbQuoteIdentifier(conexion, tabla)),
      " (", columnas, ") VALUES ", paste(filas, collapse = ", ")
    )
  )
}

.preparar_fixture <- function(conexion, datos, motor) {
  suppressWarnings(try(DBI::dbRemoveTable(conexion, "tramites"), silent = TRUE))
  suppressWarnings(try(DBI::dbRemoveTable(conexion, "padron"), silent = TRUE))
  if (identical(motor, "mysql84")) {
    DBI::dbExecute(
      conexion,
      paste(
        "CREATE TABLE tramites (id_tramite INTEGER, documento INTEGER,",
        "categoria VARCHAR(1), monto DOUBLE, edad INTEGER)"
      )
    )
    .insertar_sql(conexion, "tramites", datos)
  } else {
    DBI::dbWriteTable(conexion, "tramites", datos)
  }
  DBI::dbExecute(
    conexion,
    paste(
      "CREATE TABLE padron (id_persona INTEGER NOT NULL PRIMARY KEY,",
      "nombre VARCHAR(60))"
    )
  )
  padron <- data.frame(
    id_persona = 1:3, nombre = c("A", "B", "C"), stringsAsFactors = FALSE
  )
  if (identical(motor, "mysql84")) {
    .insertar_sql(conexion, "padron", padron)
  } else {
    DBI::dbWriteTable(conexion, "padron", padron, append = TRUE)
  }
  invisible(NULL)
}

.check <- function(expr) {
  salida <- tryCatch({
    valor <- suppressWarnings(force(expr))
    if (!isTRUE(valor)) stop("la condición no se cumplió", call. = FALSE)
    "ok"
  }, error = function(e) paste0("fallo: ", .texto_seguro(e)))
  salida
}

.medir_motor <- function(id) {
  especificacion <- .especificaciones[[id]]
  fila <- .fila_base(especificacion)
  if (!requireNamespace(especificacion$paquete, quietly = TRUE)) {
    return(.fila_no_medido(
      especificacion,
      paste("falta el controlador R", especificacion$paquete)
    ))
  }
  if (!is.null(especificacion$variable) &&
      !nzchar(Sys.getenv(especificacion$variable, unset = "")) &&
      !nzchar(Sys.getenv("LUPA_DB_PASS", unset = ""))) {
    return(.fila_no_medido(
      especificacion,
      paste("falta la variable", especificacion$variable)
    ))
  }
  conexion <- tryCatch(especificacion$conectar(), error = function(e) e)
  if (inherits(conexion, "condition")) {
    return(.fila_no_medido(especificacion, conexion))
  }
  on.exit(try(DBI::dbDisconnect(conexion), silent = TRUE), add = TRUE)

  version <- tryCatch(
    DBI::dbGetQuery(conexion, especificacion$version_sql)[[1L]][[1L]],
    error = function(e) NA_character_
  )
  fila$version_servidor <- as.character(version)
  datos <- .datos_fixture()
  fixture_error <- tryCatch(
    .preparar_fixture(conexion, datos, id),
    error = function(e) e
  )
  if (inherits(fixture_error, "condition")) {
    motivo <- paste("no se pudo preparar el fixture:", .texto_seguro(fixture_error))
    fila$estado_motor <- "fallo"
    fila$motivo <- motivo
    for (columna in c("cinco_columnas", "dialecto", "media",
                      "clave_primaria", "cobertura")) {
      fila[[columna]] <- paste0("fallo: ", motivo)
    }
    return(fila)
  }

  perfil <- NULL
  resumen <- NULL
  fila$cinco_columnas <- .check({
    perfil <- lupa::perfilar_dbi(
      conexion, "tramites", muestra = 500L,
      analizar_dependencias = FALSE, duplicados_aproximados = FALSE,
      proteger_datos_personales = FALSE
    )
    resumen <- perfil$resumen_tabla
    nrow(resumen$columnas) == 5L
  })
  fila$dialecto <- .check({
    !is.null(resumen) && !is.null(resumen$meta$dialecto$nombre) &&
      nzchar(as.character(resumen$meta$dialecto$nombre))
  })
  fila$media <- .check({
    real <- DBI::dbGetQuery(conexion, "SELECT monto FROM tramites")[[1L]]
    columna <- resumen$columnas[
      resumen$columnas$columna == "monto", , drop = FALSE
    ]
    nrow(columna) == 1L && is.finite(as.numeric(columna$media[[1L]])) &&
      isTRUE(all.equal(
        as.numeric(columna$media[[1L]]), mean(real), tolerance = 1e-8
      ))
  })
  fila$clave_primaria <- .check({
    if (!exists(".clave_primaria_dbi", asNamespace("lupa"),
                inherits = FALSE)) {
      stop("la función de lectura de clave primaria no está disponible",
           call. = FALSE)
    }
    leer_clave <- get(".clave_primaria_dbi", asNamespace("lupa"))
    clave <- leer_clave(conexion, "padron")
    identical(tolower(as.character(clave$columnas)), "id_persona")
  })
  fila$cobertura <- .check(
    inherits(resumen$cobertura, "data.frame")
  )
  resultados <- fila[c("cinco_columnas", "dialecto", "media",
                       "clave_primaria", "cobertura")]
  fallos <- vapply(resultados, function(x) startsWith(x, "fallo:"), logical(1L))
  fila$estado_motor <- if (any(fallos)) "fallo" else "medido"
  fila$motivo <- if (any(fallos)) {
    paste(names(resultados)[fallos], resultados[fallos], sep = ": ",
          collapse = "; ")
  } else ""
  fila
}

cat("Motores solicitados:", paste(.seleccionados, collapse = ", "), "\n")
filas <- lapply(.seleccionados, .medir_motor)
salida <- do.call(rbind, filas)
salida <- salida[, .columnas_resultado, drop = FALSE]
directorio_datos <- file.path(.raiz_paquete, "benchmark", "datos")
dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
archivo_salida <- file.path(directorio_datos, "verificar-motor.csv")
utils::write.csv(salida, archivo_salida, row.names = FALSE, na = "")
print(salida[, c("motor", "version_servidor", "estado_motor",
                 "cinco_columnas", "dialecto", "media",
                 "clave_primaria", "cobertura")], row.names = FALSE)
cat("Salida:", archivo_salida, "\n")

if (any(salida$estado_motor %in% c("fallo", "no_medido"))) quit(status = 1)
