# Ayudantes de las pruebas de NaN y magnitudes extremas en las bases
# (`test-nan-y-extremos-dbi.R` y, en la maquina de desarrollo,
# `test-local-nan-y-extremos-mysql.R`). Viven aca para que las dos las usen.

.nf_fila <- function(perfil, campo = "x") {
  perfil$resumen_tabla$columnas[
    perfil$resumen_tabla$columnas$columna == campo, , drop = FALSE
  ]
}

.nf_memoria <- function(x) {
  perfilar(
    data.frame(x = x), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )$columnas
}

.nf_dbi <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC"), ...
  )))
}

.nf_drop <- function(con, tabla) {
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
      silent = TRUE)
}

.nf_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
}

.nf_crear_nan <- function(con, tabla, valores, tipo = "DOUBLE") {
  .nf_drop(con, tabla)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INTEGER, x ", tipo, ")"
  ))
  filas <- paste0(
    "(", seq_along(valores), ", ", valores, ")"
  )
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
  invisible(NULL)
}

.nf_comparar_nan <- function(con, tabla, valores, esperados,
                             tipo = "DOUBLE") {
  .nf_crear_nan(con, tabla, valores, tipo = tipo)
  dbi <- .nf_dbi(con, tabla)
  memoria <- .nf_memoria(esperados)
  observado <- .nf_fila(dbi)
  campos <- c(
    "n", "n_faltantes", "n_distintos", "moda", "frecuencia_moda",
    "minimo", "maximo", "media", "mediana", "desvio"
  )
  fields <- intersect(campos, names(observado))
  for (campo in fields) {
    expect_equal(observado[[campo]], memoria[[campo]], tolerance = 0,
                 info = campo)
  }
  expect_equal(observado$n_validos, observado$n - observado$n_faltantes)
  expect_true("valores_no_finitos" %in% dbi$perfil_muestra$hallazgos$tipo_hallazgo)
  invisible(dbi)
}

.nf_tipo <- function(nombre) {
  if (identical(nombre, "mssql")) return("float")
  if (startsWith(nombre, "postgres_")) return("DOUBLE PRECISION")
  "DOUBLE"
}

.nf_extremos <- function() {
  x <- 1:40
  list(
    alto = 1e200 * (1 + x / 100),
    bajo = 1e-200 * (1 + x / 100)
  )
}

.nf_literal <- function(x) sprintf("%.17g", x)

# El desvio de valores de magnitud extrema sale calculado y finito en cada
# conexion de `motores`, con la llamada por omision, sin muestra y pidiendo
# solo el desvio.
.nf_comprobar_extremos <- function(motores) {
  on.exit(lapply(motores, function(con) {
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }), add = TRUE)

  for (nombre in names(motores)) {
    con <- motores[[nombre]]
    tabla <- paste0("nf_extremos_", gsub("[^A-Za-z0-9]", "_", nombre))
    on.exit(.nf_drop(con, tabla), add = TRUE)
    .nf_drop(con, tabla)
    for (caso in .nf_extremos()) {
      .nf_drop(con, tabla)
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla, " (id INTEGER, x ", .nf_tipo(nombre), ")"
      ))
      filas <- paste0(
        "(", seq_along(caso), ", ", vapply(caso, .nf_literal, character(1L)), ")"
      )
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
      ))
      for (opcion in list(
        list(),
        list(bloque_muestra = "solo_agregados"),
        list(metricas = "desvio")
      )) {
        perfil <- do.call(.nf_dbi, c(list(con, tabla), opcion))
        fila <- .nf_fila(perfil)
        sql <- perfil$resumen_tabla$sql[
          perfil$resumen_tabla$sql$columna == "x" &
            perfil$resumen_tabla$sql$metrica == "desvio", , drop = FALSE
        ]
        expect_identical(as.character(sql$estado), "calculado", info = nombre)
        expect_true(is.finite(fila$desvio), info = nombre)
        expect_false(isTRUE(fila$desvio == 0), info = nombre)
      }
    }
    .nf_drop(con, tabla)
  }
}
