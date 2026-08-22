skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.lob_prueba <- new.env(parent = emptyenv())
.lob_prueba$sql <- character()
# El tipo que declara el controlador es un parametro del banco, no una
# constante: la vuelta contra base real mostro que `{SQL Server}` informa
# codigos ODBC ("-1") y no nombres, y un banco que solo habla en nombres no
# ejercita el caso que rompe.
.lob_prueba$tipo_nota <- "text"
.lob_prueba$rechazar_todo <- FALSE

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
    # `LIMIT 0` y `WHERE 1 = 0` son las consultas de metadatos: piden la forma,
    # no los datos, y un controlador que no sabe traer un LOB las contesta
    # igual. Rechazarlas seria simular una base caida, que es otro caso.
    lectura <- grepl("`t`", statement, fixed = TRUE) &&
      !grepl("COUNT|MIN|MAX|AVG|SUM|GROUP BY|WHERE 1 = 0|LIMIT 0", statement)
    if (lectura && (.lob_prueba$rechazar_todo ||
                    grepl("nota", statement, fixed = TRUE))) {
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
  info$type[info$name == "nota"] <- .lob_prueba$tipo_nota
  info
})

# Deja el banco como estaba: si un test cambia el tipo declarado y no lo
# repone, el siguiente mide otra cosa y el fallo aparece lejos de la causa.
.con_tipo_declarado <- function(tipo, expr) {
  previo <- .lob_prueba$tipo_nota
  on.exit(.lob_prueba$tipo_nota <- previo, add = TRUE)
  .lob_prueba$tipo_nota <- tipo
  force(expr)
}

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

test_that("el patron reconoce los codigos ODBC, no solo los nombres", {
  # Un controlador no tiene por que informar nombres. `odbc` resuelve
  # `dbColumnInfo()` con `nanodbc::result::column_datatype()`, que devuelve el
  # codigo numerico: contra `{SQL Server}` noventa columnas `varchar(max)`
  # llegan como noventa veces "-1". Con el patron de solo nombres, reconocia
  # cero de noventa.
  expect_equal(.columnas_de_tipo_largo_dbi(c("-1", "-4", "-10")), 1:3)
  # Los codigos que NO son de tipo largo no pueden colarse: -2 y -3 son
  # `BINARY` y `VARBINARY`, -8 y -9 son `WCHAR` y `WVARCHAR`, 12 es `VARCHAR`.
  expect_length(
    .columnas_de_tipo_largo_dbi(c("12", "1", "4", "-2", "-3", "-8", "-9")), 0L
  )
  # Y las variantes de nombre que el patron viejo tampoco veia.
  nombres <- c("LONG VARCHAR", "SQL_LONGVARCHAR", "wlongvarchar",
               "longvarbinary", "sql_longvarbinary")
  expect_equal(.columnas_de_tipo_largo_dbi(nombres), seq_along(nombres))
})

test_that("un controlador que informa el tipo como codigo ODBC no pierde la muestra", {
  # Este es el caso que rompio contra base real: el arreglo existia, la prueba
  # pasaba, y contra el driver de produccion no se disparaba nunca porque el
  # banco hablaba en nombres y el driver en codigos.
  .con_tipo_declarado("-1", {
    cruda <- .tabla_con_lob()
    on.exit(DBI::dbDisconnect(cruda), add = TRUE)
    con <- .envolver_lob(cruda)

    perfil <- perfilar_dbi(con, "t", muestra = 10)

    expect_false(is.null(perfil$perfil_muestra))
    expect_setequal(perfil$perfil_muestra$columnas$columna, c("id", "monto"))
    muestreo <- perfil$perfil_muestra$meta$origen_dbi$muestreo
    expect_equal(muestreo$columnas_omitidas, "nota")
    # Lo reconocio el atajo, asi que la omision es supuesta y se declara asi.
    expect_false(muestreo$omision_comprobada)
    expect_match(muestreo$motivo_columnas_omitidas, "tipo largo")
  })
})

test_that("un tipo que nadie reconoce se aisla por descarte y se declara comprobado", {
  # La garantia que importa: que el reintento funcione SIN reconocer el tipo.
  # Mientras dependa de un patron, cada controlador nuevo vuelve a perder la
  # muestra en silencio.
  .con_tipo_declarado("un_tipo_que_ningun_patron_conoce", {
    cruda <- .tabla_con_lob()
    on.exit(DBI::dbDisconnect(cruda), add = TRUE)
    con <- .envolver_lob(cruda)

    expect_length(.columnas_de_tipo_largo_dbi("un_tipo_que_ningun_patron_conoce"), 0L)
    perfil <- perfilar_dbi(con, "t", muestra = 10)

    expect_false(is.null(perfil$perfil_muestra))
    expect_setequal(perfil$perfil_muestra$columnas$columna, c("id", "monto"))

    muestreo <- perfil$perfil_muestra$meta$origen_dbi$muestreo
    expect_equal(muestreo$columnas_omitidas, "nota")
    expect_setequal(muestreo$columnas_leidas, c("id", "monto"))
    expect_false(grepl("nota", muestreo$sql_muestra, fixed = TRUE))
    # Aca si se comprobo: la columna fallo sola y el resto se leyo junto.
    expect_true(muestreo$omision_comprobada)
    expect_gt(muestreo$sondas_descarte, 0)

    fila <- perfil$resumen_tabla$cobertura
    fila <- fila[fila$bloque == "perfil_muestra", ]
    expect_equal(fila$estado, "alcance_distinto")
    expect_match(fila$motivo, "descarte")
    expect_match(fila$motivo, "sonda")
    expect_match(fila$motivo, "07009")
    # Y no se rebaja a suposicion lo que si se midio.
    expect_false(grepl("No se comprob", fila$motivo, fixed = TRUE))
  })
})

test_that("si el fallo no es de una columna, el descarte lo dice y no inventa una", {
  previo <- .lob_prueba$rechazar_todo
  on.exit(.lob_prueba$rechazar_todo <- previo, add = TRUE)
  .lob_prueba$rechazar_todo <- TRUE

  cruda <- .tabla_con_lob()
  on.exit(DBI::dbDisconnect(cruda), add = TRUE)
  con <- .envolver_lob(cruda)

  # El aviso es parte del contrato: la muestra no esta y se dice en voz alta,
  # ademas de quedar en la cobertura.
  expect_warning(
    perfil <- perfilar_dbi(con, "t", muestra = 10),
    "no queda ningun subconjunto legible"
  )

  # Sin subconjunto legible no hay muestra, y eso se declara: lo que no puede
  # pasar es que se omita una columna cualquiera para poder devolver algo.
  expect_null(perfil$perfil_muestra)
  fila <- perfil$resumen_tabla$cobertura
  fila <- fila[fila$bloque == "perfil_muestra", ]
  expect_equal(fila$estado, "no_disponible")
  expect_match(fila$motivo, "sondearon")
  expect_match(fila$motivo, "todas fallan por si solas")
  # Y el resumen por columna sale igual: el fallo de la muestra no se lleva
  # puesto el resto.
  expect_equal(nrow(perfil$resumen_tabla$columnas), 3L)
})

test_that("el aislamiento por descarte cuesta lo que promete y respeta el saldo", {
  # Biseccion sobre n columnas: a lo sumo 2n-1 sondas, que es el tamano del
  # arbol cuando todas las hojas son culpables. El tope de 2n no es un numero
  # elegido a dedo.
  ilegibles <- c(3L, 7L)
  sondear <- function(indices) !any(indices %in% ilegibles)
  resultado <- .aislar_ilegibles_dbi(sondear, function() TRUE, 8L, 16L)
  expect_equal(resultado$culpables, ilegibles)
  expect_false(resultado$agotado)
  expect_lte(resultado$sondas, 2L * 8L - 1L)

  # Todas culpables es el peor caso, y tiene que entrar en 2n-1.
  todas <- .aislar_ilegibles_dbi(function(i) FALSE, function() TRUE, 8L, 16L)
  expect_equal(todas$culpables, 1:8)
  expect_lte(todas$sondas, 2L * 8L - 1L)

  # Ninguna culpable: el conjunto entero se lee y no se acusa a nadie.
  ninguna <- .aislar_ilegibles_dbi(function(i) TRUE, function() TRUE, 8L, 16L)
  expect_length(ninguna$culpables, 0L)
  expect_equal(ninguna$sondas, 1L)

  # El tope corta, y cortar se declara. Sin esto un aislamiento parcial se
  # leeria como completo.
  corto <- .aislar_ilegibles_dbi(sondear, function() TRUE, 8L, 3L)
  expect_true(corto$agotado)
  expect_equal(corto$sondas, 3L)

  # Y una sonda rechazada por falta de saldo no puede leerse como "esta
  # columna no se puede leer": el saldo se consulta antes de sondear.
  restantes <- 2L
  saldo <- function() {
    if (restantes <= 0L) return(FALSE)
    restantes <<- restantes - 1L
    TRUE
  }
  sin_saldo <- .aislar_ilegibles_dbi(function(i) FALSE, saldo, 8L, 16L)
  expect_true(sin_saldo$agotado)
  expect_lte(sin_saldo$sondas, 2L)
})
