# Las colecciones, las transacciones y la codificacion de los valores en la via
# DBI (A2, A4, A5, M3, B1, B3, B4).
#
# Lo que pide un motor de contenedor (PostgreSQL, SQL Server) se midio en esos
# motores. Aca, una conexion fingida sobre duckdb que se comporta como
# PostgreSQL en las dos cosas que importan -RPostgres lee `0` en toda celda
# `money`, y un error aborta la transaccion abierta- y que atraviesa las
# funciones exportadas enteras.

# ---- La conexion fingida -------------------------------------------------
#
# Un `DECIMAL(12,2)` en el resultado de una consulta es un `money` de
# PostgreSQL: el controlador lo entrega en `0` (lo que hace RPostgres) y
# `dbColumnInfo()$.typname` dice `money`. `MIN`/`MAX` lo conservan, como en
# PostgreSQL; `CAST(... AS NUMERIC)` lo convierte. Con una
# transaccion abierta, un error -al preparar o al traer filas- la aborta: toda
# sentencia siguiente falla hasta `ROLLBACK` o `ROLLBACK TO SAVEPOINT`. `now()`
# es el comienzo de la transaccion, y fuera de una cambia entre sentencias. Y
# como PostgreSQL, nombra `CAST("x" AS NUMERIC)` de una proyeccion con el nombre
# de la columna (duckdb lo nombra con la expresion).
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r35x2 <- new.env()
.R35X2_ABORTADA <- paste(
  "ERROR:  current transaction is aborted, commands ignored until end of",
  "transaction block"
)

.r35x2_definir <- function() {
  if (isTRUE(.r35x2$definida)) return(invisible())
  donde <- new.env(parent = asNamespace("DBI"))
  .r35x2$donde <- donde
  methods::setClass(
    "LupaR35x2PgFalsa", contains = "DBIConnection",
    slots = c(real = "ANY", estado = "environment"), where = donde
  )
  methods::setClass(
    "LupaR35x2PgFalsaResultado", contains = "DBIResult",
    slots = c(real = "ANY", estado = "environment", money = "integer",
              tipos = "character", sql = "character"),
    where = donde
  )
  fallar <- function(estado, error) {
    if (estado$en_tx) estado$abortada <- TRUE
    stop(conditionMessage(error), call. = FALSE)
  }
  ahora <- function(estado, sql) {
    momento <- if (estado$en_tx) estado$inicio else Sys.time()
    sql <- gsub("now()", paste0("TIMESTAMP '", format(momento, "%Y-%m-%d %H:%M:%OS6"), "'"),
                sql, fixed = TRUE)
    gsub("(CAST\\(\"([^\"]+)\" AS [A-Z ]+\\))(?=\\s*,|\\s+FROM\\s)",
         "\\1 AS \"\\2\"", sql, perl = TRUE)
  }
  tipo_motor <- function(tipo) {
    if (identical(tipo, "DECIMAL(12,2)")) return("money")
    if (startsWith(tipo, "DECIMAL")) return("numeric")
    switch(tipo, INTEGER = "int4", BIGINT = "int8", DOUBLE = "float8",
           FLOAT = "float4", VARCHAR = "text", DATE = "date",
           BOOLEAN = "bool", tolower(tipo))
  }
  methods::setMethod("dbGetInfo", "LupaR35x2PgFalsa", function(dbObj, ...) {
    list(dbname = "lupa", dbms.name = "PostgreSQL", db.version = "16.0")
  }, where = donde)
  methods::setMethod("dbIsValid", "LupaR35x2PgFalsa", function(dbObj, ...) {
    DBI::dbIsValid(dbObj@real)
  }, where = donde)
  methods::setMethod("dbDisconnect", "LupaR35x2PgFalsa", function(conn, ...) {
    DBI::dbDisconnect(conn@real, shutdown = TRUE)
  }, where = donde)
  methods::setMethod(
    "dbSendQuery", c("LupaR35x2PgFalsa", "character"),
    function(conn, statement, ...) {
      estado <- conn@estado
      estado$log <- c(estado$log, statement)
      if (estado$abortada) stop(.R35X2_ABORTADA, call. = FALSE)
      sql <- ahora(estado, statement)
      tipos <- tryCatch(
        as.character(DBI::dbGetQuery(conn@real, paste("DESCRIBE", sql))$column_type),
        error = function(e) character()
      )
      real <- tryCatch(DBI::dbSendQuery(conn@real, sql),
                       error = function(e) fallar(estado, e))
      methods::new(
        "LupaR35x2PgFalsaResultado", real = real, estado = estado,
        money = as.integer(which(tipos == "DECIMAL(12,2)")), tipos = tipos,
        sql = statement
      )
    }, where = donde
  )
  methods::setMethod("dbFetch", "LupaR35x2PgFalsaResultado", function(res, n = -1, ...) {
    estado <- res@estado
    if (is.infinite(n)) n <- -1
    datos <- tryCatch(DBI::dbFetch(res@real, n = n),
                      error = function(e) fallar(estado, e))
    if (length(res@money) && nrow(datos)) {
      estado$crudas <- c(estado$crudas, res@sql)
      for (k in res@money) {
        datos[[k]] <- ifelse(is.na(datos[[k]]), NA_real_, 0)
      }
    }
    datos
  }, where = donde)
  methods::setMethod("dbClearResult", "LupaR35x2PgFalsaResultado", function(res, ...) {
    DBI::dbClearResult(res@real)
  }, where = donde)
  methods::setMethod("dbHasCompleted", "LupaR35x2PgFalsaResultado", function(res, ...) {
    DBI::dbHasCompleted(res@real)
  }, where = donde)
  methods::setMethod("dbColumnInfo", "LupaR35x2PgFalsaResultado", function(res, ...) {
    info <- DBI::dbColumnInfo(res@real)
    tipos <- res@tipos
    info$.typname <- if (length(tipos) == nrow(info)) {
      vapply(tipos, tipo_motor, character(1L), USE.NAMES = FALSE)
    } else rep(NA_character_, nrow(info))
    info
  }, where = donde)
  methods::setMethod(
    "dbGetQuery", c("LupaR35x2PgFalsa", "character"),
    function(conn, statement, ...) {
      res <- DBI::dbSendQuery(conn, statement)
      on.exit(DBI::dbClearResult(res))
      DBI::dbFetch(res, n = -1)
    }, where = donde
  )
  methods::setMethod(
    "dbExecute", c("LupaR35x2PgFalsa", "character"),
    function(conn, statement, ...) {
      estado <- conn@estado
      estado$log <- c(estado$log, statement)
      orden <- toupper(trimws(statement))
      nombre <- function(prefijo) tolower(trimws(sub(prefijo, "", orden)))
      if (grepl("^(BEGIN|START TRANSACTION)", orden)) {
        estado$en_tx <- TRUE
        estado$abortada <- FALSE
        estado$puntos <- character()
        estado$inicio <- Sys.time()
        return(0L)
      }
      if (grepl("^ROLLBACK TO ", orden)) {
        k <- match(nombre("^ROLLBACK TO (SAVEPOINT )?"), estado$puntos)
        if (!estado$en_tx || is.na(k)) {
          if (estado$en_tx) estado$abortada <- TRUE
          stop("ERROR:  savepoint does not exist", call. = FALSE)
        }
        estado$puntos <- estado$puntos[seq_len(k)]
        estado$abortada <- FALSE
        return(0L)
      }
      if (grepl("^(COMMIT|END|ROLLBACK|ABORT)", orden)) {
        estado$en_tx <- FALSE
        estado$abortada <- FALSE
        estado$puntos <- character()
        return(0L)
      }
      if (estado$abortada) stop(.R35X2_ABORTADA, call. = FALSE)
      if (grepl("^SAVEPOINT ", orden)) {
        if (!estado$en_tx) {
          stop("ERROR:  SAVEPOINT can only be used in transaction blocks",
               call. = FALSE)
        }
        estado$puntos <- c(estado$puntos, nombre("^SAVEPOINT "))
        return(0L)
      }
      if (grepl("^RELEASE ", orden)) {
        k <- match(nombre("^RELEASE (SAVEPOINT )?"), estado$puntos)
        if (is.na(k)) {
          if (estado$en_tx) estado$abortada <- TRUE
          stop("ERROR:  savepoint does not exist", call. = FALSE)
        }
        estado$puntos <- estado$puntos[seq_len(k - 1L)]
        return(0L)
      }
      if (grepl("^SET TRANSACTION", orden)) return(0L)
      tryCatch(DBI::dbExecute(conn@real, statement),
               error = function(e) fallar(estado, e))
    }, where = donde
  )
  methods::setMethod("dbBegin", "LupaR35x2PgFalsa", function(conn, ...) {
    DBI::dbExecute(conn, "BEGIN")
    invisible(TRUE)
  }, where = donde)
  methods::setMethod("dbRollback", "LupaR35x2PgFalsa", function(conn, ...) {
    DBI::dbExecute(conn, "ROLLBACK")
    invisible(TRUE)
  }, where = donde)
  # Los metadatos: RPostgres los pregunta con una consulta, que dentro de una
  # transaccion abortada tambien falla.
  for (nombre in c("character", "Id")) {
    methods::setMethod("dbListFields", c("LupaR35x2PgFalsa", nombre), function(conn, name, ...) {
      if (conn@estado$abortada) stop(.R35X2_ABORTADA, call. = FALSE)
      DBI::dbListFields(conn@real, name)
    }, where = donde)
    methods::setMethod("dbExistsTable", c("LupaR35x2PgFalsa", nombre), function(conn, name, ...) {
      if (conn@estado$abortada) stop(.R35X2_ABORTADA, call. = FALSE)
      DBI::dbExistsTable(conn@real, name)
    }, where = donde)
  }
  .r35x2$definida <- TRUE
  invisible()
}

.r35x2_pg_falsa <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  .r35x2_definir()
  real <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  estado <- new.env(parent = emptyenv())
  estado$en_tx <- FALSE
  estado$abortada <- FALSE
  estado$puntos <- character()
  estado$log <- character()
  estado$crudas <- character()
  methods::new("LupaR35x2PgFalsa", real = real, estado = estado)
}

# Las tablas del caso, chicas: dos importes `money` sin un
# valor comun y un `numeric` con los 60 primeros de `fac`.
.r35x2_importes <- function(con) {
  real <- con@real
  DBI::dbExecute(real, "CREATE TABLE fac (id INTEGER, importe DECIMAL(12,2))")
  DBI::dbExecute(real, "INSERT INTO fac SELECT g, 1000 + g * 17.35 FROM range(1, 81) t(g)")
  DBI::dbExecute(real, "CREATE TABLE cobro (nro INTEGER, monto DECIMAL(14,2))")
  DBI::dbExecute(real, "INSERT INTO cobro SELECT 9000 + g, 1000 + g * 17.35 FROM range(1, 61) t(g)")
  DBI::dbExecute(real, "CREATE TABLE nota (nro INTEGER, importe DECIMAL(12,2))")
  DBI::dbExecute(real, "INSERT INTO nota SELECT 7000 + g, -50 - g * 2.5 FROM range(1, 41) t(g)")
  DBI::dbExecute(real, "CREATE TABLE base (id INTEGER, grupo VARCHAR, monto DOUBLE)")
  DBI::dbExecute(real, "INSERT INTO base SELECT g, 'g' || (g % 5), g * 1.5 FROM range(1, 201) t(g)")
  invisible(con)
}

test_that("A4: la coleccion lee un money convertido, como perfilar_dbi(), y no inventa ni poda relaciones", {
  con <- .r35x2_pg_falsa()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .r35x2_importes(con)
  # A mano, leyendo los valores por otro camino -duckdb directo-: `cobro` y
  # `fac` comparten 60 importes; `fac` y `nota` ninguno.
  f <- DBI::dbGetQuery(con@real, "SELECT importe FROM fac")$importe
  cb <- DBI::dbGetQuery(con@real, "SELECT monto FROM cobro")$monto
  nt <- DBI::dbGetQuery(con@real, "SELECT importe FROM nota")$importe
  expect_identical(length(intersect(f, cb)), 60L)
  expect_identical(length(intersect(f, nt)), 0L)
  col <- coleccion(con, c("fac", "cobro", "nota"))
  r <- suppressWarnings(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "fac", tabla_2 = c("cobro", "nota"))
  ))
  rel <- r$relaciones
  fila <- rel[rel$tabla_2 == "cobro" & rel$columna_tabla1 == "importe" &
                rel$columna_tabla2 == "monto", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_equal(fila$n_valores_comunes, 60)
  expect_equal(fila$cobertura_tabla2_en_tabla1, mean(cb %in% f))
  expect_equal(fila$cobertura_tabla1_en_tabla2, mean(f %in% cb))
  # La inventada: dos columnas de ceros con cobertura 1.
  expect_false(any(rel$tabla_2 == "nota" & rel$columna_tabla1 == "importe" &
                     rel$columna_tabla2 == "importe"))
  expect_false(any(r$cobertura_podas$columna_tabla1 == "importe" &
                     r$cobertura_podas$columna_tabla2 == "monto"))
})

test_that("A4 por construccion: ninguna puerta lee un money crudo", {
  # El recorrido: cada consulta que la via manda a la conexion fingida y que
  # trae filas con una columna `money` sin convertir queda anotada. Se recorren
  # las cuatro puertas exportadas que leen valores; lo que se anota es lo que
  # se mando, no una lista de sitios.
  con <- .r35x2_pg_falsa()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .r35x2_importes(con)
  p <- suppressWarnings(perfilar_dbi(con, "fac"))
  importe <- p$resumen_tabla$columnas[p$resumen_tabla$columnas$columna == "importe", ]
  expect_equal(importe$maximo, 1000 + 80 * 17.35)
  expect_equal(importe$media, mean(1000 + (1:80) * 17.35))
  invisible(suppressWarnings(perfilar_dbi(con, "fac", bloque_filas = 30L)))
  invisible(suppressWarnings(plan_perfilado_dbi(con, "fac")))
  col <- coleccion(con, c("fac", "cobro", "nota"))
  invisible(suppressWarnings(perfilar_coleccion(col)))
  invisible(suppressWarnings(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "fac", tabla_2 = c("cobro", "nota"))
  )))
  invisible(suppressWarnings(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "fac", tabla_2 = "cobro"),
    columnas_candidatas = list(fac = "importe", cobro = "monto"),
    orden = list(fac = "id", cobro = "nro"), muestra = 50
  )))
  expect_gt(length(con@estado$log), 0L)
  expect_identical(con@estado$crudas, character())
})

test_that("A5: dentro de una transaccion las cuatro puertas perfilan y la transaccion sigue", {
  con <- .r35x2_pg_falsa()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .r35x2_importes(con)
  estado <- con@estado
  col <- coleccion(con, c("base", "fac", "cobro"))
  pares <- data.frame(tabla_1 = "fac", tabla_2 = "cobro")
  fuera <- suppressWarnings(perfilar_dbi(con, "base"))
  fuera_rel <- suppressWarnings(relaciones_coleccion(col, pares, muestra = Inf))
  # Fuera de una transaccion la via no agrega ningun punto de guardado.
  expect_false(any(grepl("SAVEPOINT", estado$log, fixed = TRUE)))
  DBI::dbBegin(con)
  desde <- length(estado$log)
  dentro <- suppressWarnings(perfilar_dbi(con, "base"))
  plan <- suppressWarnings(plan_perfilado_dbi(con, "base"))
  pc <- suppressWarnings(perfilar_coleccion(col))
  dentro_rel <- suppressWarnings(relaciones_coleccion(col, pares, muestra = Inf))
  enviadas <- estado$log[-seq_len(desde)]
  # Lo mismo que fuera: 200 filas de `base`, media de 1:200 = 100,5.
  medidas <- c("columna", "n_validos", "n_faltantes", "minimo", "maximo", "media")
  expect_equal(dentro$resumen_tabla$columnas[, medidas],
               fuera$resumen_tabla$columnas[, medidas])
  expect_equal(dentro$resumen_tabla$columnas$media[[1L]], mean(1:200))
  expect_s3_class(plan, "plan_perfilado_dbi")
  expect_identical(nrow(pc$resumen_coleccion), 3L)
  expect_identical(nrow(pc$cobertura_coleccion[grepl("aborted", pc$cobertura_coleccion$motivo), ]), 0L)
  expect_equal(dentro_rel$relaciones[, c("columna_tabla1", "columna_tabla2", "n_valores_comunes")],
               fuera_rel$relaciones[, c("columna_tabla1", "columna_tabla2", "n_valores_comunes")])
  # La consulta siguiente del usuario, en su transaccion, responde.
  expect_false(estado$abortada)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM base")$n, 200)
  # El paquete no abrio ni cerro la transaccion del usuario, solto todos sus
  # puntos de guardado, y algo fallo adentro: la sonda de dialecto.
  expect_false(any(grepl("^\\s*(BEGIN|COMMIT|END|ABORT|ROLLBACK)\\b", enviadas,
                         ignore.case = TRUE) &
                     !grepl("^\\s*ROLLBACK TO SAVEPOINT", enviadas, ignore.case = TRUE)))
  expect_identical(estado$puntos, character())
  expect_gt(sum(grepl("^ROLLBACK TO SAVEPOINT", enviadas)), 0L)
  # Y `muestra = Inf` no manda `LIMIT Inf`, que todo motor rechaza.
  expect_false(any(grepl("LIMIT Inf", estado$log, fixed = TRUE)))
  expect_true(estado$en_tx)
  DBI::dbRollback(con)
})

test_that("M3: la FK con la referencia truncada se declara aunque otra columna del par alcance el umbral", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cliente <- data.frame(id = 1:40, activo = (1:40) %% 2L)
  pedido <- data.frame(nro = 1:20, cliente_id = 41L - (((1:20) * 7L) %% 40L),
                       activo = (1:20) %% 2L)
  DBI::dbWriteTable(con, "cliente", cliente)
  DBI::dbWriteTable(con, "pedido", pedido)
  # En la base, los 20 `cliente_id` estan en `cliente$id`; sobre las 20 filas
  # que se leen de la referencia, la cota es la fraccion que cae en 1..20.
  expect_true(all(pedido$cliente_id %in% cliente$id))
  cota <- mean(pedido$cliente_id %in% cliente$id[1:20])
  expect_gt(cota, 0)
  expect_lt(cota, 0.9)
  col <- coleccion(con, c("cliente", "pedido"))
  r <- relaciones_coleccion(
    col, data.frame(tabla_1 = "cliente", tabla_2 = "pedido"), muestra = 20,
    orden = list(cliente = "id", pedido = "nro")
  )
  # La vecina trivial alcanza el umbral y se publica.
  expect_true(any(r$relaciones$columna_tabla1 == "activo" &
                    r$relaciones$columna_tabla2 == "activo"))
  # Y la FK queda declarada sin conclusion, con su cota, en una sola fila.
  expect_identical(nrow(r$cobertura_pares), 1L)
  motivo <- r$cobertura_pares$motivo
  expect_match(motivo, "^Sin conclusion")
  expect_match(motivo, paste0("`id` -> `cliente_id` ", signif(cota, 4)),
               fixed = TRUE)
  expect_identical(r$meta$pares_sin_conclusion, 1L)
})

test_that("A2: el texto que el controlador entrega en latin1 con la marca UTF-8 se lee como latin1 y se declara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # RSQLite entrega los bytes latin1 de un TEXT con la marca UTF-8 y sin
  # convertirlos: lo mismo que odbc con FreeTDS bajo `C` sobre un `varchar`
  # latin1. Se simula el controlador odbc -la conexion es en
  # memoria-; la copia sin el arreglo no distingue el controlador y lee igual.
  if (exists(".controlador_odbc_dbi", envir = asNamespace("lupa"),
             inherits = FALSE)) {
    local_mocked_bindings(.controlador_odbc_dbi = function(conexion) TRUE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nombres <- c("Artigas", paste0("Paysand", "\u00fa"), paste0("R", "\u00ed", "o Negro"),
               "Salto", paste0("San Jos", "\u00e9"))
  DBI::dbWriteTable(con, "depto", data.frame(codigo = 1:5, nombre = nombres))
  DBI::dbExecute(con, "CREATE TABLE localidad (id INTEGER, departamento TEXT)")
  latin1 <- vapply(nombres, function(x) {
    paste(sprintf("%02x", as.integer(charToRaw(iconv(x, "UTF-8", "latin1")))), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
  for (k in 1:20) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO localidad VALUES (", k, ", CAST(X'", latin1[(k %% 5) + 1], "' AS TEXT))"
    ))
  }
  crudo <- DBI::dbGetQuery(con, "SELECT departamento FROM localidad")$departamento
  # La lectura cruda: marca UTF-8, bytes latin1 (12 de 20 con tilde).
  expect_identical(sum(!validUTF8(crudo)), 12L)
  # A mano: leidos como latin1, los 20 estan en `depto$nombre`.
  leidos <- crudo
  Encoding(leidos) <- "latin1"
  expect_true(all(enc2utf8(leidos) %in% nombres))
  p <- suppressWarnings(perfilar_dbi(con, "localidad"))
  hallazgos <- p$perfil_muestra$hallazgos
  expect_false(any(grepl("UTF-8 v", hallazgos$descripcion, fixed = TRUE)))
  cobertura <- p$resumen_tabla$cobertura
  fila <- cobertura[cobertura$elemento == "departamento" &
                      cobertura$estado == "degradado", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_match(fila$motivo, "latin1", fixed = TRUE)
  expect_equal(p$perfil_muestra$columnas$n_distintos[
    p$perfil_muestra$columnas$columna == "departamento"], 5)
  col <- coleccion(con, c("depto", "localidad"))
  r <- relaciones_coleccion(col, data.frame(tabla_1 = "depto", tabla_2 = "localidad"))
  fila <- r$relaciones[r$relaciones$columna_tabla1 == "nombre" &
                         r$relaciones$columna_tabla2 == "departamento", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_equal(fila$cobertura_tabla2_en_tabla1, 1)
  lectura <- r$meta$lecturas$motivo[r$meta$lecturas$tabla == "localidad"]
  expect_match(lectura, "latin1", fixed = TRUE)
})

test_that("B1: una columna de orden_muestra que la sesion no puede escribir da el motivo de la codificacion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ano <- paste0("a", "\u00f1", "o")
  bytes_ano <- rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f)))
  # La sesion de la prueba escribe el nombre; se simula la que no -la de odbc
  # bajo `C`-, con la regla de la guarda: no se manda lo que no se puede
  # escribir.
  local_mocked_bindings(.sentencia_representable_dbi = function(conexion, sql) {
    !grepl(bytes_ano, sql, fixed = TRUE, useBytes = TRUE)
  })
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- data.frame(id = 1:6, x = 2020:2025)
  names(d)[2] <- ano
  DBI::dbWriteTable(con, "visita", d)
  error <- tryCatch(
    suppressWarnings(perfilar_dbi(con, "visita", muestra = 3, orden_muestra = ano)),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_codificacion_dbi")
  expect_match(conditionMessage(error), "locale UTF-8", fixed = TRUE)
  expect_false(grepl("rechazo", conditionMessage(error), fixed = TRUE))
})

test_that("B3: el motivo de una lectura nombra la tabla y la columna de orden que declaro el usuario", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "padron", data.frame(id = 1:5))
  DBI::dbWriteTable(con, "visita", data.frame(padron_id = 1:5))
  col <- coleccion(con, c("padron", "visita"))
  r <- relaciones_coleccion(col, data.frame(tabla_1 = "padron", tabla_2 = "visita"),
                            orden = "no_existe")
  motivo <- r$cobertura_pares$motivo
  expect_identical(length(motivo), 1L)
  expect_false(grepl("[literal omitido]", motivo, fixed = TRUE))
  expect_match(motivo, "\"no_existe\"", fixed = TRUE)
})

test_that("B4: columnas_candidatas sigue la regla de mayusculas de orden, y un argumento malo tiene la clase de la via", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fac", data.frame(id = 1:5, importe = c(10, 20, 30, 40, 50)))
  DBI::dbWriteTable(con, "cobro", data.frame(nro = 1:3, monto = c(10, 20, 30)))
  col <- coleccion(con, c("fac", "cobro"))
  pares <- data.frame(tabla_1 = "fac", tabla_2 = "cobro")
  # SQLite, como SQL Server, no distingue mayusculas en los nombres: `IMPORTE`
  # es `importe`, como lo acepta `orden`.
  r <- relaciones_coleccion(col, pares,
                            columnas_candidatas = list(fac = "IMPORTE", cobro = "MONTO"))
  expect_identical(r$relaciones$columna_tabla1, "importe")
  expect_identical(r$relaciones$columna_tabla2, "monto")
  expect_equal(r$relaciones$cobertura_tabla2_en_tabla1, 1)
  error <- tryCatch(
    relaciones_coleccion(col, pares, columnas_candidatas = list(fac = "no_hay")),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_argumento_dbi")
  expect_match(conditionMessage(error), "no_hay", fixed = TRUE)
})

test_that("Por construccion: las filas del motor entran por un punto y todo resultado se cierra por otro", {
  # `.traer_filas_dbi()` lee el texto que el controlador entrega mal marcado
  # (A2) y `.limpiar_consulta_dbi()` cierra el punto de guardado de la
  # transaccion del usuario (A5). Un `dbFetch()` o un `dbClearResult()` directo
  # se saltearia una de las dos. Se recorre el espacio de nombres entero.
  ns <- asNamespace("lupa")
  sitios <- list(dbFetch = character(), dbClearResult = character())
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- e[[1L]]
      llamada <- if (is.name(cabeza)) as.character(cabeza) else if (
        is.call(cabeza) && identical(cabeza[[1L]], as.name("::"))) {
        as.character(cabeza[[3L]])
      } else ""
      if (llamada %in% names(sitios)) {
        sitios[[llamada]] <<- c(sitios[[llamada]], nombre)
      }
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]], nombre)
      }
    }
    invisible()
  }
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto)) recorrer(body(objeto), nombre)
  }
  expect_identical(unique(sitios$dbFetch), ".traer_filas_dbi")
  expect_identical(unique(sitios$dbClearResult), ".limpiar_consulta_dbi")
})
