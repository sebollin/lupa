skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.lob_prueba <- new.env(parent = emptyenv())
.lob_prueba$sql <- character()

if (!methods::isClass("ConexionLobLupa")) {
  setClass("ConexionLobLupa", contains = "SQLiteConnection")
}

.envolver_lob <- function(con) {
  salida <- methods::new("ConexionLobLupa")
  for (ranura in methods::slotNames(con)) {
    methods::slot(salida, ranura) <- methods::slot(con, ranura)
  }
  salida
}

# Reproduce lo que hace un controlador ODBC contra SQL Server: declara la
# columna como `text` y responde 07009 al intentar traerla en una lectura
# corriente. Una sola columna asi se llevaba puesta la muestra entera.
#
# El tipo se cambia sobre una subclase de resultado, no sobre `SQLiteResult`:
# tocar el metodo de la clase real lo cambia para todo el proceso.
if (!methods::isClass("ResultadoLobLupa")) {
  setClass("ResultadoLobLupa", contains = "SQLiteResult")
}

setMethod(
  "dbSendQuery", c("ConexionLobLupa", "character"),
  function(conn, statement, ...) {
    .lob_prueba$sql <- c(.lob_prueba$sql, statement)
    # El nombre se busca sin comillas a proposito: cada dialecto cita distinto
    # -SQLite usa acentos graves, no comillas dobles- y una sonda que solo
    # reconoce una de las formas no ejercita nada.
    if (grepl("nota", statement, fixed = TRUE) &&
        !grepl("COUNT|MIN|MAX|AVG|SUM|GROUP BY|WHERE 1 = 0", statement)) {
      stop("07009: Invalid descriptor index", call. = FALSE)
    }
    resultado <- methods::callNextMethod()
    envuelto <- methods::new("ResultadoLobLupa")
    for (ranura in methods::slotNames(resultado)) {
      methods::slot(envuelto, ranura) <- methods::slot(resultado, ranura)
    }
    envuelto
  }
)

setMethod("dbColumnInfo", "ResultadoLobLupa", function(res, ...) {
  info <- methods::callNextMethod()
  info$type[info$name == "nota"] <- "text"
  info
})

.tabla_con_lob <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, monto REAL, nota TEXT)")
  for (i in 1:20) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO t VALUES (%d, %f, 'texto %d')", i, i * 1.5, i
    ))
  }
  con
}

test_that("el patron reconoce los tipos que un controlador no sabe traer", {
  largos <- c("text", "NTEXT", "image", "varchar(max)", "NVARCHAR (MAX)",
              "varbinary(max)", "nvarbinary(max)", "clob", "nclob", "blob",
              "bytea", "longtext", "jsonb", "xmltype", "LONG", "LONG RAW",
              "bfile", "geometry")
  expect_equal(.columnas_de_tipo_largo_dbi(largos), seq_along(largos))
  # El borde importa: "varchar(50)" y "varchar(max)" se distinguen solo por lo
  # que hay entre parentesis, y "longitud" empieza igual que "long".
  cortos <- c("integer", "double", "varchar(50)", "nvarchar(4000)",
              "numeric(12,3)", "date", "character", "bigint", "longitud",
              "textual", "point")
  expect_length(.columnas_de_tipo_largo_dbi(cortos), 0L)
  expect_length(.columnas_de_tipo_largo_dbi(NULL), 0L)
  expect_length(.columnas_de_tipo_largo_dbi(NA_character_), 0L)
})

test_that("una columna que el controlador rechaza no se lleva puesta la muestra", {
  cruda <- .tabla_con_lob()
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  con <- .envolver_lob(cruda)
  .lob_prueba$sql <- character()

  perfil <- perfilar_dbi(con, "t", muestra = 10)

  # Lo que importa: la muestra existe, con las columnas que si se pudieron leer.
  expect_false(is.null(perfil$perfil_muestra))
  expect_setequal(perfil$perfil_muestra$columnas$columna, c("id", "monto"))
  # Y el resumen por columna las cubre a las tres: lo que falta es el perfil por
  # fila de la rechazada, no la columna entera.
  expect_equal(nrow(perfil$resumen_tabla$columnas), 3L)

  cobertura <- perfil$resumen_tabla$cobertura
  fila <- cobertura[cobertura$bloque == "perfil_muestra", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$estado, "alcance_distinto")
  expect_match(fila$motivo, "nota")
  expect_match(fila$motivo, "tipo largo")
  # El motivo original del motor se conserva: el diagnostico no reemplaza la
  # evidencia.
  expect_match(fila$motivo, "07009")
  # Y no se afirma una causa que no se comprobo. El reintento salta ante
  # cualquier fallo de lectura habiendo columnas de tipo largo declaradas; que
  # esas columnas sean el motivo es probable, no medido.
  expect_match(fila$motivo, "No se comprob")
  expect_false(grepl("que el controlador rechazo", fila$motivo, fixed = TRUE))

  # `meta` es donde se mira para saber que se hizo, asi que no puede quedar
  # congelado con la lectura que fallo. Declaraba haber leido justamente la
  # columna que no se pudo leer, y publicaba el SQL original en vez del que se
  # emitio: informar como medido lo que no se midio, en el peor lugar.
  muestreo <- perfil$perfil_muestra$meta$origen_dbi$muestreo
  expect_setequal(muestreo$columnas_leidas, c("id", "monto"))
  expect_false(grepl("nota", muestreo$sql_muestra, fixed = TRUE))
  expect_equal(muestreo$columnas_omitidas, "nota")
  expect_match(muestreo$motivo_columnas_omitidas, "07009")
  # Y lo que dice `meta` es lo que trae la muestra, no una lista aparte.
  expect_setequal(
    muestreo$columnas_leidas, perfil$perfil_muestra$columnas$columna
  )
})

test_that("sin columnas rechazadas la muestra sale entera y sin aviso", {
  con <- .tabla_con_lob()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  perfil <- perfilar_dbi(con, "t", muestra = 10)
  expect_false(is.null(perfil$perfil_muestra))
  expect_equal(nrow(perfil$perfil_muestra$columnas), 3L)
  cobertura <- perfil$resumen_tabla$cobertura
  expect_equal(sum(cobertura$bloque == "perfil_muestra"), 0L)
})
