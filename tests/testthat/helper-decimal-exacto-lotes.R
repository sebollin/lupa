# Conexiones y comprobaciones de las pruebas de las sumas exactas por lotes
# (`test-decimal-exacto-lotes.R` y la de MySQL y MariaDB, aparte porque sus
# controladores no estan en Suggests).

.dlot_pg <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  con
}

.dlot_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.dlot_duck <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
}

# Crea `tabla` con `columnas` (nombre = tipo) y `valores` (nombre = textos, con
# `NA` para un nulo).
.dlot_tabla <- function(con, tabla, columnas, valores) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (",
    paste(names(columnas), columnas, collapse = ", "), ")"
  ))
  filas <- vapply(seq_along(valores[[1L]]), function(k) {
    celdas <- vapply(names(columnas), function(nombre) {
      v <- valores[[nombre]][[k]]
      if (is.na(v)) "NULL" else v
    }, character(1L))
    paste0("(", paste(celdas, collapse = ", "), ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
  invisible(tabla)
}

.dlot_perfil <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-07 12:00:00", tz = "UTC"), ...
  )))
}

# La media y el desvio de `columna` son los esperados y estan publicados.
.dlot_esperar <- function(perfil, columna, media = NULL, desvio = NULL,
                          info = "") {
  columnas <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  esperadas <- list(media = media, desvio = desvio)
  for (metrica in names(esperadas)) {
    if (is.null(esperadas[[metrica]])) next
    estado <- sql$estado[sql$columna == columna & sql$metrica == metrica]
    expect_true(length(estado) > 0L &&
                  all(estado %in% c("calculado", "estimado")),
                info = paste(info, columna, metrica))
    expect_identical(
      as.numeric(columnas[[metrica]][columnas$columna == columna]),
      esperadas[[metrica]], info = paste(info, columna, metrica)
    )
  }
}

# El caso de escala 30: tres valores de 9e7 con 30 decimales.
.dlot_treinta <- c(
  "90000000.123456789012345678901234567890",
  "90000000.123456789012345678901234567891",
  "89999999.876543210987654321098765432109"
)
.dlot_treinta_media <- 0x1.5752a002a23d2p+26
.dlot_treinta_desvio <- 0x1.23f433877bfc1p-3

# Los subconjuntos de `metricas` que piden la media o el desvio y no traian la
# cuenta, la media o los extremos con los que se abria la reparacion.
.dlot_metricas <- list(
  c("basicos", "desvio"), "desvio", c("validos", "desvio"), "basicos"
)

.dlot_probar_metricas <- function(con, tabla, info) {
  for (metricas in .dlot_metricas) {
    for (incluir in c(TRUE, FALSE)) {
      perfil <- .dlot_perfil(con, tabla, metricas = metricas,
                             incluir_valores = incluir)
      .dlot_esperar(
        perfil, "x",
        media = if ("basicos" %in% metricas) .dlot_treinta_media,
        desvio = if ("desvio" %in% metricas) .dlot_treinta_desvio,
        info = paste(info, paste(metricas, collapse = "+"), incluir)
      )
    }
  }
}

# Con `max_consultas` en el total del plan, la media y el desvio exactos salen.
.dlot_probar_presupuesto <- function(con, tabla, info) {
  argumentos <- list(con, tabla, metricas = c("validos", "basicos", "desvio"),
                     bloque_muestra = "solo_agregados")
  plan <- do.call(plan_perfilado_dbi, argumentos)
  perfil <- .dlot_perfil(
    con, tabla, metricas = c("validos", "basicos", "desvio"),
    bloque_muestra = "solo_agregados", max_consultas = attr(plan, "total")
  )
  .dlot_esperar(perfil, "x", .dlot_treinta_media, .dlot_treinta_desvio,
                info = paste(info, "max_consultas"))
  expect_identical(
    as.numeric(perfil$resumen_tabla$meta$consultas$emitidas),
    as.numeric(attr(plan, "total")), info = paste(info, "emitidas")
  )
}
