# Ronda 34-X: las piezas compartidas de la ronda 33, cruzadas. Cada bloque es un
# hallazgo de la refutacion, con el caso que lo reproducia y la cifra rehecha a
# mano. Lo que necesita un motor de contenedor (SQL Server, MySQL) no va aca: se
# midio con sondas y queda anotado; aca va la regla que lo decide, con un
# controlador que la maquina de CRAN tiene. El archivo es ASCII: lo que no lo es
# se arma con `intToUtf8()` o `rawToChar()`.

# Dos tablas con dos relaciones reales: `personas.id -> visitas.persona_id` y
# `personas.departamento -> visitas.depto_visita`. A mano: `persona_id` toma
# 1:10 (10 valores, todos en `id` = 1:12) y `depto_visita` toma el departamento
# de esas personas, `(1:10) %% 5 + 1` -> 5 valores, todos en `departamento`.
# `visita` (101:115) no comparte valores con nada.
.coleccion_dos_relaciones_o163 <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(
    id = 1:12, departamento = sprintf("D%02d", (1:12) %% 5 + 1)
  ))
  k <- rep(1:10, length.out = 15)
  DBI::dbWriteTable(con, "visitas", data.frame(
    visita = 101:115, persona_id = k, depto_visita = sprintf("D%02d", k %% 5 + 1)
  ))
  list(con = con, col = coleccion(con, c("personas", "visitas")))
}

.relaciones_o163 <- function(r) {
  x <- r$relaciones[, c("columna_tabla1", "columna_tabla2", "n_valores_comunes",
                        "cobertura_tabla2_en_tabla1")]
  x <- x[order(x$columna_tabla1), , drop = FALSE]
  rownames(x) <- NULL
  x
}

test_that("H1: orden sin columnas_candidatas ordena la lectura y no la recorta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .coleccion_dos_relaciones_o163()
  on.exit(DBI::dbDisconnect(base$con), add = TRUE)
  pares <- data.frame(tabla_1 = "personas", tabla_2 = "visitas")
  sin_orden <- suppressWarnings(relaciones_coleccion(base$col, pares))
  con_orden <- suppressWarnings(relaciones_coleccion(
    base$col, pares, orden = list(personas = "id", visitas = "visita")
  ))
  esperado <- data.frame(
    columna_tabla1 = c("departamento", "id"),
    columna_tabla2 = c("depto_visita", "persona_id"),
    n_valores_comunes = c(5, 10), cobertura_tabla2_en_tabla1 = c(1, 1),
    stringsAsFactors = FALSE
  )
  expect_equal(.relaciones_o163(sin_orden), esperado, ignore_attr = TRUE)
  # Antes: 0 relaciones, 0 pares sin leer, `estable = TRUE` y la consulta
  # `SELECT id FROM personas ORDER BY id`.
  expect_equal(.relaciones_o163(con_orden), esperado, ignore_attr = TRUE)
  expect_true(con_orden$meta$estable)
  sql <- con_orden$meta$lecturas$sql
  expect_true(all(grepl("^SELECT \\* FROM", sql)))
  expect_true(all(grepl("ORDER BY", sql, fixed = TRUE)))
  # Candidatas declaradas para UNA tabla: la otra conserva todas sus columnas
  # tambien con `orden` (antes `visitas` se leia solo por `visita`).
  una <- suppressWarnings(relaciones_coleccion(
    base$col, pares, orden = list(personas = "id", visitas = "visita"),
    columnas_candidatas = list(personas = c("id", "departamento"))
  ))
  expect_equal(.relaciones_o163(una), esperado, ignore_attr = TRUE)
  lectura_visitas <- una$meta$lecturas$sql[una$meta$lecturas$tabla == "visitas"]
  expect_true(grepl("^SELECT \\* FROM", lectura_visitas))
  # Y con candidatas en las dos, la de orden se suma a la lectura y no a la
  # comparacion: `visita` no aparece en ninguna relacion.
  dos <- suppressWarnings(relaciones_coleccion(
    base$col, pares, orden = list(personas = "id", visitas = "visita"),
    columnas_candidatas = list(personas = "id", visitas = "persona_id")
  ))
  expect_identical(dos$relaciones$columna_tabla2, "persona_id")
})

# ---- H3: el controlador se reconoce por la clase, no por el texto -----------
# Medido con MySQL 8.4 abierto con `RMariaDB::MariaDB()` (clase
# `MySQLConnection` del paquete RMariaDB): `coleccion()` leia "mysql" en
# `dbGetInfo()`, pedia RMySQL y lo cargaba; las dos clases se llaman igual y la
# conexion del usuario quedaba inservible (sin RMySQL, `coleccion()` se negaba).
# Aca, una conexion de RSQLite cuya clase -y por lo tanto el texto que lee el
# detector- dice "mysql": el mismo cruce entre lo que la clase dice y lo que dice
# el texto, con un controlador que esta en Suggests.

test_that("H3: coleccion() pide el paquete de la clase de la conexion y no el que sugiere el texto", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  entorno <- new.env()
  methods::setClass("ConexionMysqlO163", contains = "SQLiteConnection",
                    where = entorno)
  on.exit(methods::removeClass("ConexionMysqlO163", where = entorno), add = TRUE)
  base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(base), add = TRUE)
  DBI::dbWriteTable(base, "t1", data.frame(id = 1:4, x = c(2, 4, 6, 8)))
  DBI::dbWriteTable(base, "t2", data.frame(t1_id = c(1L, 2L, 2L, 4L)))
  con <- methods::new("ConexionMysqlO163", base)
  pedidos <- character()
  local_mocked_bindings(.hay_paquete = function(nombre) {
    pedidos <<- c(pedidos, nombre)
    if (identical(nombre, "RMySQL")) return(FALSE)
    isTRUE(base::requireNamespace(nombre, quietly = TRUE))
  })
  # Antes: `lupa_error_requisito_motor` "el motor MySQL requiere el paquete R
  # `RMySQL`".
  col <- coleccion(con, c("t1", "t2"))
  expect_s3_class(col, "coleccion_lupa")
  expect_false("RMySQL" %in% pedidos)
  expect_true("RSQLite" %in% pedidos)
  p <- suppressWarnings(perfilar_coleccion(col))
  expect_identical(nrow(p$resumen_coleccion), 2L)
  rel <- suppressWarnings(relaciones_coleccion(
    col, data.frame(tabla_1 = "t1", tabla_2 = "t2")
  ))
  # A mano: `t1_id` toma 1, 2 y 4, los tres en `id`.
  fila <- rel$relaciones[rel$relaciones$columna_tabla2 == "t1_id", , drop = FALSE]
  expect_identical(fila$columna_tabla1, "id")
  expect_equal(fila$n_valores_comunes, 3)
  expect_false("RMySQL" %in% pedidos)
  # La conexion del usuario sigue sana.
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS uno")$uno, 1)
})

# ---- H2: el nombre que va a la conexion se normaliza y pasa por la guarda ----
# Medido con SQL Server (odbc + FreeTDS) bajo C.UTF-8: un nombre de tabla marcado
# latin1 -el de `read.csv(encoding = "latin1")`- o con esos bytes sin marca
# llegaba crudo a `dbExistsTable()`, antes de la guarda de la ronda 33; odbc lo
# pasa sin convertir, la conexion quedaba rota y la consulta siguiente del
# usuario hacia caer a R. Bajo C, `dbQuoteIdentifier()` lo escapaba a texto
# ASCII (`a<f1>o`) y pasaba la guarda: otro identificador. Lo que necesita el
# contenedor no va aca: se prueba que el nombre llega normalizado a TODA llamada
# que toca la conexion, y que ninguna lo lleva si la guarda lo rechaza.

.ano_o163 <- function() paste0("o163_a", intToUtf8(0xf1L), "o")
.ano_latin1_o163 <- function() {
  x <- rawToChar(as.raw(c(0x6f, 0x31, 0x36, 0x33, 0x5f, 0x61, 0xf1, 0x6f)))
  Encoding(x) <- "latin1"
  x
}
.ano_bytes_o163 <- function() {
  rawToChar(as.raw(c(0x6f, 0x31, 0x36, 0x33, 0x5f, 0x61, 0xf1, 0x6f)))
}

# Los nombres que recibieron `dbExistsTable()` y `dbListFields()` mientras corria
# `codigo`, en hexadecimal.
.nombres_a_la_conexion_o163 <- function(codigo) {
  vistos <- new.env()
  vistos$nombres <- character()
  anotar <- bquote({
    nombre_visto <- if (is.character(name)) name else tryCatch(
      paste(name@name, collapse = "."), error = function(e) "?"
    )
    assign("nombres", c(get("nombres", envir = .(vistos)),
                        paste(as.character(charToRaw(nombre_visto)), collapse = " ")),
           envir = .(vistos))
  })
  for (generica in c("dbExistsTable", "dbListFields")) {
    suppressMessages(trace(generica, tracer = anotar,
                           where = asNamespace("DBI"), print = FALSE))
  }
  on.exit(for (generica in c("dbExistsTable", "dbListFields")) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)
  valor <- tryCatch(force(codigo), error = function(e) e)
  list(valor = valor, nombres = vistos$nombres)
}

test_that("H2: perfilar_dbi(), plan_perfilado_dbi() y coleccion() mandan a la conexion el nombre latin1 normalizado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, .ano_o163(), data.frame(id = 1:3, valor = c(10, 20, 30)))
  utf8 <- paste(as.character(charToRaw(.ano_o163())), collapse = " ")
  latin1 <- .ano_latin1_o163()
  expect_identical(Encoding(latin1), "latin1")
  perfil <- .nombres_a_la_conexion_o163(suppressWarnings(perfilar_dbi(con, latin1)))
  # La tabla se lee: media de 10, 20, 30 = 20.
  columnas <- perfil$valor$resumen_tabla$columnas
  expect_equal(columnas$media[columnas$columna == "valor"], 20)
  # Antes llegaban los bytes latin1 (`... 61 f1 6f`) a `dbExistsTable()`.
  expect_gt(length(perfil$nombres), 0L)
  expect_true(all(perfil$nombres == utf8))
  plan <- .nombres_a_la_conexion_o163(suppressWarnings(plan_perfilado_dbi(con, latin1)))
  expect_false(inherits(plan$valor, "condition"))
  expect_true(all(plan$nombres == utf8))
  # Por `data.frame` -una planilla latin1 leida con `read.csv()`- tambien.
  col <- coleccion(con, data.frame(tabla = latin1, stringsAsFactors = FALSE))
  expect_identical(paste(as.character(charToRaw(col$tablas$tabla)), collapse = " "),
                   utf8)
  pc <- .nombres_a_la_conexion_o163(suppressWarnings(perfilar_coleccion(col)))
  expect_identical(nrow(pc$valor$resumen_coleccion), 1L)
  expect_true(all(pc$nombres == utf8))
})

test_that("H2: un nombre que la sesion no puede escribir no llega a ninguna llamada de la conexion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # La sesion que no puede escribirlo -odbc bajo C- se simula con la misma
  # regla sobre cualquier texto con `a\u00f1o`, como en la ronda 33.
  bytes_ano <- rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f)))
  local_mocked_bindings(.sentencia_representable_dbi = function(conexion, sql) {
    !grepl(bytes_ano, sql, fixed = TRUE, useBytes = TRUE)
  })
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, .ano_o163(), data.frame(id = 1:3))
  for (nombre in list(.ano_o163(), .ano_latin1_o163())) {
    r <- .nombres_a_la_conexion_o163(perfilar_dbi(con, nombre))
    expect_s3_class(r$valor, "lupa_error_codificacion_dbi")
    # Antes: `dbExistsTable()` ya lo habia recibido.
    expect_identical(r$nombres, character())
    p <- .nombres_a_la_conexion_o163(plan_perfilado_dbi(con, nombre))
    expect_s3_class(p$valor, "lupa_error_codificacion_dbi")
    expect_identical(p$nombres, character())
  }
  # Y la conexion sigue sana.
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS uno")$uno, 1)
})

test_that("H2: bajo UTF-8 la guarda de odbc rechaza bytes sin codificacion declarada, con su remedio", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "la sesion de la prueba no es UTF-8")
  odbc_falso <- structure(list(), class = "OdbcConnection")
  bytes <- .ano_bytes_o163()
  expect_false(validUTF8(bytes))
  # Antes: TRUE para todo bajo un locale UTF-8.
  expect_false(.sentencia_representable_dbi(odbc_falso, paste0("SELECT * FROM ", bytes)))
  expect_true(.sentencia_representable_dbi(odbc_falso, paste0("SELECT * FROM ", .ano_o163())))
  # El nombre normalizado: lo latin1 pasa a UTF-8; los bytes sin marca quedan.
  expect_identical(charToRaw(.nombre_para_conexion_dbi(.ano_latin1_o163())),
                   charToRaw(.ano_o163()))
  expect_identical(charToRaw(.nombre_para_conexion_dbi(bytes)), charToRaw(bytes))
  # Y no se manda: el objeto falso no tiene conexion; si se mandara, el error
  # seria otro.
  error <- tryCatch(.existe_tabla_dbi(odbc_falso, bytes), error = function(e) e)
  expect_s3_class(error, "lupa_error_codificacion_dbi")
  # Correr con un locale UTF-8 no lo arregla: el remedio es declarar la
  # codificacion al leer.
  expect_true(grepl("fileEncoding", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("LC_ALL=C.UTF-8", conditionMessage(error), fixed = TRUE))
  # Los componentes de un `DBI::Id`, igual.
  skip_if_not_installed("DBI")
  id <- .nombre_para_conexion_dbi(DBI::Id(schema = "dbo", table = .ano_latin1_o163()))
  expect_identical(charToRaw(id@name[["table"]]), charToRaw(.ano_o163()))
})

test_that("H2: coleccion() conserva los bytes de un nombre sin codificacion declarada en vez de volverlo 'NA'", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "la sesion de la prueba no es UTF-8")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  bytes <- .ano_bytes_o163()
  avisos <- character()
  col <- withCallingHandlers(
    coleccion(con, c(bytes, paste0("esq.", bytes))),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # Antes: el nombre era el texto "NA" y R avisaba "input string 1 is invalid
  # in this locale"; el segundo, `NA.NA`.
  expect_identical(charToRaw(col$tablas$tabla[[1L]]), charToRaw(bytes))
  expect_identical(charToRaw(col$tablas$tabla[[2L]]), charToRaw(bytes))
  expect_identical(col$tablas$esquema[[2L]], "esq")
  expect_identical(avisos, character())
})

test_that("Guarda H2: toda pregunta de metadatos con un nombre sale por los envoltorios que miran la guarda", {
  ns <- asNamespace("lupa")
  preguntas <- c("dbExistsTable", "dbListFields")
  sitios <- character()
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- e[[1L]]
      llamada <- if (is.name(cabeza)) as.character(cabeza) else if (
        is.call(cabeza) && identical(cabeza[[1L]], as.name("::"))) {
        as.character(cabeza[[3L]])
      } else ""
      if (llamada %in% preguntas) sitios <<- c(sitios, nombre)
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
  expect_identical(sort(unique(sitios)),
                   c(".existe_tabla_dbi", ".listar_campos_dbi"))
})

# ---- H4: los nombres de una lectura salen de la fuente del paquete ----------
# Medido con SQL Server (FreeTDS) bajo C: `SELECT *` devuelve los nombres en el
# juego de caracteres del cliente (latin1, `61 f1 6f`) y `dbListFields()` en
# UTF-8 (`61 c3 b1 6f`). `relaciones_coleccion()` publicaba los de `SELECT *`.
# Aca el controlador que devuelve asi los nombres se simula en el envio de la
# consulta, sobre RSQLite.

test_that("H4: relaciones_coleccion() publica los nombres de dbListFields() aunque la lectura los traiga en bytes latin1", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ano <- paste0("a", intToUtf8(0xf1L), "o")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d1 <- data.frame(id = 1:4, x = c(2001L, 2002L, 2003L, 2004L))
  names(d1)[2] <- ano
  d2 <- data.frame(persona_id = c(1L, 2L, 4L), x = c(2001L, 2002L, 2004L))
  names(d2)[2] <- ano
  DBI::dbWriteTable(con, "o163_per", d1)
  DBI::dbWriteTable(con, "o163_vis", d2)
  col <- coleccion(con, c("o163_per", "o163_vis"))
  traer <- .traer_consulta_dbi
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql) {
    datos <- traer(conexion, sql)
    nombres <- names(datos)
    no_ascii <- grepl("[^\\001-\\177]", nombres, useBytes = TRUE, perl = TRUE)
    nombres[no_ascii] <- vapply(nombres[no_ascii], function(n) {
      rawToChar(charToRaw(iconv(n, "UTF-8", "latin1")))
    }, character(1L))
    names(datos) <- nombres
    datos
  })
  rel <- suppressWarnings(relaciones_coleccion(
    col, data.frame(tabla_1 = "o163_per", tabla_2 = "o163_vis")
  ))
  # A mano: `persona_id` (1, 2, 4) en `id`, y los tres anios en `ano`.
  fila <- rel$relaciones[rel$relaciones$columna_tabla2 != "persona_id", ,
                         drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_equal(fila$n_valores_comunes, 3)
  # Antes: `61 f1 6f`, que no es UTF-8 valido.
  expect_identical(charToRaw(fila$columna_tabla1), charToRaw(ano))
  expect_identical(charToRaw(fila$columna_tabla2), charToRaw(ano))
  campos <- DBI::dbListFields(con, "o163_per")
  expect_true(all(.identificadores_en(rel$relaciones$columna_tabla1, campos)))
  # Y devuelto como candidata, vuelve a encontrar la relacion.
  ida <- suppressWarnings(relaciones_coleccion(
    col, data.frame(tabla_1 = "o163_per", tabla_2 = "o163_vis"),
    columnas_candidatas = list(o163_per = fila$columna_tabla1,
                               o163_vis = fila$columna_tabla2)
  ))
  expect_identical(nrow(ida$relaciones), 1L)
})

# ---- H5 y H10: una columna que no se puede escribir no se lleva la tabla -----
# Medido con SQL Server bajo C: `columnas_candidatas = list(t = c("id",
# "a\u00f1o"))` o `orden = list(t = "a\u00f1o")` perdian el par entero -tambien
# `id -> persona_id`- con un motivo tapado dos veces y un remedio de permisos.
# Aca la sesion que no puede escribir el nombre se simula con la misma regla.

test_that("H5: una candidata o un orden que la sesion no puede escribir se declaran y el resto del par se compara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ano <- paste0("a", intToUtf8(0xf1L), "o")
  bytes_ano <- rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f)))
  local_mocked_bindings(.sentencia_representable_dbi = function(conexion, sql) {
    !grepl(bytes_ano, sql, fixed = TRUE, useBytes = TRUE)
  })
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d1 <- data.frame(id = 1:4, x = c(2001L, 2002L, 2003L, 2004L))
  names(d1)[2] <- ano
  DBI::dbWriteTable(con, "o163_per", d1)
  DBI::dbWriteTable(con, "o163_vis", data.frame(persona_id = c(1L, 2L, 4L)))
  col <- coleccion(con, c("o163_per", "o163_vis"))
  pares <- data.frame(tabla_1 = "o163_per", tabla_2 = "o163_vis")
  con_candidata <- suppressWarnings(relaciones_coleccion(
    col, pares,
    columnas_candidatas = list(o163_per = c("id", ano), o163_vis = "persona_id")
  ))
  # A mano: `persona_id` (1, 2, 4) en `id` -> 3 comunes, cobertura 1. Antes: 0
  # relaciones y el par entero en `cobertura_pares`.
  expect_identical(con_candidata$relaciones$columna_tabla1, "id")
  expect_equal(con_candidata$relaciones$n_valores_comunes, 3)
  expect_identical(nrow(con_candidata$cobertura_pares), 0L)
  poda <- con_candidata$cobertura_podas
  poda <- poda[poda$motivo == "columna_no_escribible", , drop = FALSE]
  expect_identical(nrow(poda), 1L)
  expect_identical(charToRaw(poda$columna_tabla1), charToRaw(ano))
  expect_identical(poda$columna_tabla2, "persona_id")
  lectura <- con_candidata$meta$lecturas
  expect_true(grepl("no se puede escribir en la codificacion",
                    lectura$motivo[lectura$tabla == "o163_per"], fixed = TRUE))
  # `orden` por esa columna: la lectura va sin `ORDER BY`, lo declara, y
  # `estable` no se afirma.
  con_orden <- suppressWarnings(relaciones_coleccion(
    col, pares, orden = list(o163_per = ano, o163_vis = "persona_id")
  ))
  expect_true("id" %in% con_orden$relaciones$columna_tabla1)
  expect_false(con_orden$meta$estable)
  lectura <- con_orden$meta$lecturas
  fila <- lectura$tabla == "o163_per"
  expect_true(lectura$ok[fila])
  expect_false(lectura$orden_declarado[fila])
  expect_false(grepl("ORDER BY", lectura$sql[fila], fixed = TRUE))
  # Sin ninguna candidata escribible el par no se lee, y el motivo sale legible
  # -una sola pasada- con el remedio de la codificacion, no el de permisos.
  sin_nada <- suppressWarnings(relaciones_coleccion(
    col, pares, columnas_candidatas = list(o163_per = ano)
  ))
  motivo <- sin_nada$cobertura_pares$motivo
  expect_identical(length(motivo), 1L)
  expect_true(grepl("no se puede escribir en la codificacion", motivo, fixed = TRUE))
  expect_false(grepl("literal omitido", motivo, fixed = TRUE))
  expect_true(grepl("locale UTF-8", sin_nada$cobertura_pares$como_resolverlo,
                    fixed = TRUE))
})

test_that("H10: un orden con bytes sin codificacion declarada no aborta: la lectura va sin ORDER BY y lo dice", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o163_t1", data.frame(id = 1:4))
  DBI::dbWriteTable(con, "o163_t2", data.frame(t1_id = c(1L, 2L, 2L, 4L)))
  col <- coleccion(con, c("o163_t1", "o163_t2"))
  bytes <- rawToChar(as.raw(c(0x61, 0xf1, 0x6f)))
  # Antes, bajo UTF-8: `simpleError` "input string 1 is invalid" desde
  # `dbQuoteIdentifier()`.
  rel <- suppressWarnings(relaciones_coleccion(
    col, data.frame(tabla_1 = "o163_t1", tabla_2 = "o163_t2"),
    orden = list(o163_t1 = bytes, o163_t2 = "t1_id")
  ))
  expect_identical(rel$relaciones$columna_tabla1, "id")
  expect_false(rel$meta$estable)
  motivo <- rel$meta$lecturas$motivo[rel$meta$lecturas$tabla == "o163_t1"]
  expect_true(validUTF8(motivo))
  expect_true(grepl("fileEncoding", motivo, fixed = TRUE))
  expect_true(grepl("a\\xf1o", motivo, fixed = TRUE))
})

# ---- Bajas de la misma construccion: H6, H8 y H9 ----------------------------

test_that("H6: analizar() lee aplicabilidad abreviada igual en el perfil y en el tablero", {
  # 100 filas; `edad_hijo` aplica en las 30 con `tiene_hijos == "si"` y 3 de
  # ellas faltan. A mano: completitud 27 / 30 = 0,9 sobre el universo, y
  # 3 / 30 = 0,1 de faltantes. Sobre la tabla entera seria 27 / 100 = 0,27.
  tiene <- rep(c("si", "no"), c(30, 70))
  edad <- ifelse(tiene == "si", rep(1:10, length.out = 100), NA)
  edad[1:3] <- NA
  d <- data.frame(id = 1:100, tiene_hijos = tiene, edad_hijo = edad,
                  stringsAsFactors = FALSE)
  a <- suppressWarnings(suppressMessages(analizar(
    d, nombre = "t", fecha = "2026-01-01",
    argumentos_perfil = list(aplic = list(edad_hijo = ~ tiene_hijos == "si"))
  )))
  columnas <- a$perfil$columnas
  expect_equal(columnas$prop_faltantes[columnas$columna == "edad_hijo"], 0.1)
  tablero <- as.data.frame(a$tablero)
  fila <- tablero$metrica_instanciada == "NoNulo:edad_hijo"
  # Antes: 0,27, sin aviso.
  expect_equal(tablero$valor[fila], 0.9)
})

test_that("H8: un prefijo de varios argumentos dice que es ambiguo y cuales casan", {
  d <- data.frame(g = rep(c("a", "b"), each = 3), x = 1:6)
  error <- tryCatch(perfilar_por(d, "g", n = TRUE), error = function(e) e)
  # Antes: "`n` no es un argumento de `perfilar()`".
  expect_true(grepl("abrevia varios argumentos", conditionMessage(error),
                    fixed = TRUE))
  expect_true(grepl("`nombre`", conditionMessage(error), fixed = TRUE))
  expect_true(grepl("`normalizar`", conditionMessage(error), fixed = TRUE))
  # Lo que no casa con nada sigue diciendo eso.
  otro <- tryCatch(perfilar_por(d, "g", zzz = TRUE), error = function(e) e)
  expect_true(grepl("no es un argumento de `perfilar()`", conditionMessage(otro),
                    fixed = TRUE))
})

test_that("H9: .detener() y .avisar() descartan un argumento NULL como stop() de base", {
  expect_identical(
    tryCatch(.detener("a", NULL, "b", c("x", "y")), error = conditionMessage),
    tryCatch(base::stop("a", NULL, "b", c("x", "y")), error = conditionMessage)
  )
  expect_identical(
    tryCatch(.avisar("a", NULL, "b"), warning = conditionMessage), "ab"
  )
  # El sitio que lo mostraba: una medida repetida una sola vez.
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "edad"))
  m <- medir(mod, data.frame(edad = c(20, NA, 35, 41)), id_medicion = "m",
             fecha = "2026-03-01")
  pe <- perfil_evaluacion("basico", regla_evaluacion("presente", function(x) x > 0.5))
  error <- tryCatch(evaluar(rbind(m, m[1, , drop = FALSE]), pe),
                    error = function(e) e)
  expect_true(grepl("repite la medida", conditionMessage(error), fixed = TRUE))
  # Antes: "...-000001`character(0): cada `id_medida`...".
  expect_false(grepl("character(0)", conditionMessage(error), fixed = TRUE))
})
