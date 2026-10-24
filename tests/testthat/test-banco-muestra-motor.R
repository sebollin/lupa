# Banco de integracion I2. No forma parte de la corrida normal: crea 4,5 M de
# filas y se activa explicitamente con LUPA_RUN_BANCO_MUESTRA_DBI=1. El log se
# conserva fuera del arbol del paquete por el guion de corrida del banco.

skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.ddl_banco_muestra_motor <- function(conexion, tabla, motor) {
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  tipos <- switch(
    motor,
    postgresql = c(id = "integer", n = "double precision", t = "text"),
    sqlite = c(id = "INTEGER", n = "REAL", t = "TEXT"),
    mariadb = c(id = "int", n = "double", t = "varchar(50)"),
    mysql = c(id = "int", n = "double", t = "varchar(50)"),
    sqlserver = c(id = "int", n = "float", t = "varchar(50)"),
    duckdb = c(id = "INTEGER", n = "DOUBLE", t = "VARCHAR"),
    stop("Motor no contemplado por la fixture I2: ", motor, call. = FALSE)
  )
  columnas <- c(
    paste0("id ", tipos[["id"]], " PRIMARY KEY"),
    paste0(paste0("n", 1:4), " ", tipos[["n"]]),
    paste0(paste0("t", 1:4), " ", tipos[["t"]])
  )
  sql <- paste0(
    "CREATE TABLE ", tabla_sql, " (",
    paste(columnas, collapse = ", "), ")"
  )
  DBI::dbExecute(conexion, sql)
  invisible(sql)
}

.datos_banco_muestra_motor <- function(inicio, fin) {
  i <- as.numeric(seq.int(inicio, fin))
  data.frame(
    id = as.integer(i),
    n1 = i * 1.5, n2 = i * 2.5, n3 = i * 3.5, n4 = i * 4.5,
    t1 = paste0("txt", as.integer(i %% 997)),
    t2 = paste0("abc", as.integer(i %% 991)),
    t3 = paste0("zzz", as.integer(i %% 983)),
    t4 = paste0("kkk", as.integer(i %% 977)),
    stringsAsFactors = FALSE
  )
}

.cargar_banco_muestra_motor <- function(conexion, tabla, total = 4500000L,
                                         bloque = 100000L) {
  DBI::dbWithTransaction(conexion, {
    for (inicio in seq.int(1L, total, by = bloque)) {
      fin <- min(total, inicio + bloque - 1L)
      DBI::dbAppendTable(
        conexion, tabla, .datos_banco_muestra_motor(inicio, fin)
      )
    }
  })
  invisible(total)
}

.conexion_banco_muestra_motor <- function(motor) {
  if (identical(motor, "sqlite")) {
    ruta <- tempfile("lupa-banco-i2-", fileext = ".sqlite")
    conexion <- DBI::dbConnect(RSQLite::SQLite(), ruta)
    attr(conexion, "lupa_ruta_banco") <- ruta
    return(conexion)
  }
  skip_if_not_installed("RPostgres")
  puerto <- suppressWarnings(as.integer(Sys.getenv("LUPA_PG_PORT", "55432")))
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"), port = puerto,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
      user = Sys.getenv("LUPA_PG_USER", "postgres"),
      password = Sys.getenv("LUPA_PG_PASSWORD", "lupa")
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    skip("PostgreSQL real no disponible para el banco I2.")
  }
  conexion
}

.rss_banco_muestra_motor <- function() {
  if (file.exists("/proc/self/status")) {
    texto <- tryCatch(readLines("/proc/self/status", warn = FALSE),
                      error = function(e) character())
    fila <- grep("^VmRSS:", texto, value = TRUE)
    if (length(fila)) {
      valor <- suppressWarnings(as.numeric(sub("^VmRSS:[[:space:]]*", "",
                                               fila[[1L]])))
      if (is.finite(valor)) return(valor * 1024)
    }
  }
  NA_real_
}

.margen_driver_banco_muestra_motor <- function(conexion, tabla, bloque) {
  # Calibracion medida: se trae un bloque con el mismo driver y se compara RSS
  # antes/despues con el tamano de la respuesta. El piso evita que un RSS
  # perezoso de Linux convierta una muestra de una sola lectura en margen cero.
  antes <- .rss_banco_muestra_motor()
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  consulta <- DBI::dbSendQuery(conexion, paste0(
    "SELECT * FROM ", tabla_sql, " ORDER BY id LIMIT ", bloque
  ))
  datos <- DBI::dbFetch(consulta, n = bloque)
  DBI::dbClearResult(consulta)
  despues <- .rss_banco_muestra_motor()
  delta <- if (is.finite(antes) && is.finite(despues)) max(0, despues - antes) else 0
  max(64 * 1024^2, delta - as.numeric(object.size(datos)))
}

test_that("[banco I2] 4,5 M x 9 conserva filas, RSS y spool de muestra_motor", {
  if (!identical(Sys.getenv("LUPA_RUN_BANCO_MUESTRA_DBI"), "1")) {
    skip("Banco I2 desactivado; usar LUPA_RUN_BANCO_MUESTRA_DBI=1.")
  }
  motor <- tolower(Sys.getenv("LUPA_BANCO_MUESTRA_DBI_MOTOR", "sqlite"))
  if (!motor %in% c("sqlite", "postgresql")) {
    skip("El banco local implementado en esta corrida usa SQLite o PostgreSQL.")
  }
  conexion <- .conexion_banco_muestra_motor(motor)
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  tabla <- paste0("lupa_banco_i2_", Sys.getpid())
  on.exit(try(DBI::dbExecute(
    conexion, paste0("DROP TABLE IF EXISTS ",
                     as.character(DBI::dbQuoteIdentifier(conexion, tabla)))
  ), silent = TRUE), add = TRUE)

  ddl <- .ddl_banco_muestra_motor(conexion, tabla, motor)
  .cargar_banco_muestra_motor(conexion, tabla)
  total <- 4500000
  bloque <- 100000L
  margen_driver_medido <- .margen_driver_banco_muestra_motor(
    conexion, tabla, bloque
  )
  limite <- 512 * 1024^2
  perfil <- lupa::perfilar_dbi(
    conexion, tabla, metricas = "validos", bloque_filas = bloque,
    bloque_muestra = "solo_agregados",
    max_bytes_procesamiento = limite,
    proteger_datos_personales = FALSE
  )
  resumen <- perfil$resumen_tabla
  bloques <- resumen$meta$bloques
  bytes <- resumen$meta$bytes
  criterio <- limite + bytes$max_bloque + margen_driver_medido
  conteo <- DBI::dbGetQuery(
    conexion,
    paste0(
      "SELECT COUNT(*) AS n, MIN(id) AS primero, MAX(id) AS ultimo FROM ",
      as.character(DBI::dbQuoteIdentifier(conexion, tabla))
    )
  )

  expect_match(ddl, "id integer PRIMARY KEY|id INTEGER PRIMARY KEY", perl = TRUE)
  expect_equal(conteo$n[[1L]], total)
  expect_equal(conteo$primero[[1L]], 1)
  expect_equal(conteo$ultimo[[1L]], total)
  expect_true(isTRUE(resumen$meta$fuente_bloques$disponible))
  expect_equal(bloques$filas_vistas, total)
  expect_equal(bloques$primer_ordinal, 1)
  expect_equal(bloques$ultimo_ordinal, total)
  expect_true(isTRUE(bloques$sin_solapamiento))
  expect_equal(bloques$recorridos, ceiling(total / bloque))
  expect_true(is.finite(bytes$rss_maximo))
  expect_lte(bytes$rss_maximo, criterio)
  expect_lte(bytes$retenidos, limite)

  cat(
    "BANCO_I2", "motor=", motor, "filas=", bloques$filas_vistas,
    "bloques=", bloques$recorridos, "max_bloque=", bytes$max_bloque,
    "retenidos=", bytes$retenidos, "rss_maximo=", bytes$rss_maximo,
    "max_bytes_procesamiento=", limite,
    "margen_driver_medido=", margen_driver_medido,
    "criterio=", criterio, "ddl=", ddl, "\n", sep = ""
  )
})
