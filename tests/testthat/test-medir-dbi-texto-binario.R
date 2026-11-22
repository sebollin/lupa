# R compara el texto por bytes. En SQLite y DuckDB cada comparacion y cada
# agrupacion de texto de medir_dbi() lleva la intercalacion binaria explicita,
# asi que la que una vista hereda sin que el catalogo la publique no cambia la
# cifra.

.con_texto_binario <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

.cerrar_texto_binario <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.fecha_texto_binario <- as.POSIXct("2026-11-22 12:00:00", tz = "UTC")

.modelo_texto_binario <- function(entidad) {
  nuc <- metricas_nucleo()
  inst <- function(metrica, nombre, atributos, ent = entidad, ...) {
    instanciar(especializar(metrica, nombre, ...), ent, atributos,
               nombre_instancia = nombre)
  }
  modelo(
    inst(nuc$ValoresPosiblesPorExtension, "E", "x", valores = c("A", "B")),
    inst(nuc$NoNulo, "N", "x", valores_nulos = "na"),
    inst(nuc$AtributoDuplicado, "D", "x"),
    inst(nuc$ConjuntoAtributosDuplicado, "CD", c("x", "y")),
    inst(nuc$ReglaIntegridadIntraEntidad, "P", "x",
         regla = ~ !is.na(x) & x == "A"),
    inst(nuc$ReglaIntegridadIntraEntidad, "Pin", "x",
         regla = ~ is.na(x) | x %in% c("A", "b")),
    inst(nuc$ReglaIntegridadIntraEntidad, "Pxy", c("x", "y"),
         regla = ~ !is.na(x) & !is.na(y) & x == y),
    inst(nuc$ReglaIntegridadInterEntidad, "FK", c("id", "x"), ent = c("r", entidad))
  )
}

# Las cifras de medir_dbi() y de medir() sobre lo que R lee de la base.
.cifras_texto_binario <- function(con, entidad, ...) {
  mod <- .modelo_texto_binario(entidad)
  datos <- list(r = DBI::dbReadTable(con, "r"))
  datos[[entidad]] <- DBI::dbReadTable(con, entidad)
  en_r <- suppressWarnings(medir(mod, datos, id_medicion = "t",
                                 fecha = .fecha_texto_binario,
                                 detalle = "resumen"))
  en_base <- suppressWarnings(medir_dbi(mod, con, id_medicion = "t",
                                        fecha = .fecha_texto_binario,
                                        instrumentar = FALSE, ...))
  cifra <- function(x) {
    d <- as.data.frame(unclass(x))
    stats::setNames(paste0(d$numerador, "/", d$denominador),
                    d$metrica_instanciada)
  }
  list(r = cifra(en_r), base = cifra(en_base), medicion = en_base)
}

.preparar_texto_binario <- function(con, ddl, vista = TRUE, previo = character()) {
  DBI::dbExecute(con, ddl)
  DBI::dbExecute(con, paste(
    "INSERT INTO base VALUES ('A', 'a'), ('a', 'a'), ('B', 'B'), ('NA', 'x'),",
    "(NULL, 'z'), ('b', 'b')"
  ))
  DBI::dbExecute(con, "CREATE TABLE r (id VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO r VALUES ('A'), ('B')")
  if (vista) {
    DBI::dbExecute(con, "CREATE VIEW v (x, y) AS SELECT t AS x, u AS y FROM base")
  }
  for (s in previo) DBI::dbExecute(con, s)
}

test_that("una vista que hereda NOCASE se mide como en R en SQLite y DuckDB", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_texto_binario(motor)
    tipo <- if (identical(motor, "sqlite")) "TEXT" else "VARCHAR"
    .preparar_texto_binario(con, sprintf(
      "CREATE TABLE base (t %s COLLATE NOCASE, u %s COLLATE NOCASE)", tipo, tipo
    ))
    cifras <- .cifras_texto_binario(con, "v")
    # Las ocho reglas se miden en la base y dan lo que da R.
    expect_identical(cifras$base, cifras$r, info = motor)
    expect_identical(length(cifras$base), 8L, info = motor)
    .cerrar_texto_binario(con)
  }
})

test_that("case_sensitive_like de SQLite no cambia ninguna cifra", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .con_texto_binario("sqlite")
  on.exit(.cerrar_texto_binario(con), add = TRUE)
  .preparar_texto_binario(
    con, "CREATE TABLE base (t TEXT COLLATE NOCASE, u TEXT)",
    previo = "PRAGMA case_sensitive_like = ON"
  )
  cifras <- .cifras_texto_binario(con, "v")
  expect_identical(cifras$base, cifras$r)
  expect_identical(length(cifras$base), 8L)
})

test_that("una columna NOCASE declarada no publica una cifra distinta de R", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_texto_binario(motor)
    tipo <- if (identical(motor, "sqlite")) "TEXT" else "VARCHAR"
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE base (x %s COLLATE NOCASE, y %s)", tipo, tipo
    ))
    DBI::dbExecute(con, paste(
      "INSERT INTO base VALUES ('A', 'a'), ('a', 'a'), ('B', 'B'), ('NA', 'x'),",
      "(NULL, 'z'), ('b', 'b')"
    ))
    DBI::dbExecute(con, "CREATE TABLE r (id VARCHAR)")
    DBI::dbExecute(con, "INSERT INTO r VALUES ('A'), ('B')")
    cifras <- .cifras_texto_binario(con, "base")
    comunes <- intersect(names(cifras$base), names(cifras$r))
    expect_identical(cifras$base[comunes], cifras$r[comunes], info = motor)
    .cerrar_texto_binario(con)
  }
})
