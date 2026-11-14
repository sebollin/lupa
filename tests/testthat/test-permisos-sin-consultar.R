# Un catalogo de permisos que no contesta. Si la consulta de columnas y
# mascaras de SQL Server falla con un error comun -un sinonimo, una tabla de
# otra base, un catalogo que la credencial no lee-, la lectura vuelve a la de
# siempre y una columna enmascarada se lee como dato: `perfilar_dbi()`
# publicaba `n_distintos = 1` de la mascara y `relaciones_coleccion()`
# relacionaba dos mascaras con cobertura 1, sin decir nada. La lectura no
# cambia -no hay de donde saber que esta enmascarado-, pero las tres puertas lo
# declaran. SQL Server y PostgreSQL no estan en la maquina de CRAN: el motor se
# finge sobre RSQLite, como en las pruebas de permisos. ASCII.
testthat::skip_on_cran()

# `falla`: "ninguna" (el catalogo contesta), "catalogo" (las dos consultas de
# SQL Server fallan, o la de `pg_attribute`), "mascaras" (solo
# `sys.masked_columns` falla). `catalogo` y `mascaras`: lo que contesta el
# motor por tabla; una tabla que no esta contesta cero filas, como un sinonimo.
.emular_catalogo_psc <- function(catalogo = list(), mascaras = list(),
                                 falla = "ninguna",
                                 senas = "microsoft sql server odbc",
                                 sin_listado = character(),
                                 env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  listar <- get(".listar_campos_dbi", envir = ns)
  registro <- new.env()
  registro$sql <- character()
  nombra <- function(sql, tabla) {
    grepl(paste0("[`\"]", tabla, "[`\"]"), sql, perl = TRUE)
  }
  motor <- function(conexion, sql, original) {
    registro$sql <- c(registro$sql, sql)
    if (grepl("sys.masked_columns", sql, fixed = TRUE, useBytes = TRUE)) {
      if (falla %in% c("catalogo", "mascaras")) stop("catalogo rechazado (fingido)")
      for (t in names(mascaras)) if (nombra(sql, t)) return(mascaras[[t]])
      return(data.frame(columna = character(), mascara = character(),
                        sin_mascara_columna = integer(),
                        sin_mascara_base = integer()))
    }
    if (grepl("sys.columns|pg_attribute", sql, perl = TRUE)) {
      if (identical(falla, "catalogo")) stop("catalogo rechazado (fingido)")
      for (t in names(catalogo)) if (nombra(sql, t)) return(catalogo[[t]])
      return(data.frame(posicion = integer(), columna = character(),
                        legible = integer()))
    }
    original(conexion, sql)
  }
  ligaduras <- list(
    .traer_consulta_dbi = function(conexion, sql) motor(conexion, sql, traer),
    .enviar_consulta_dbi = function(conexion, sql) motor(conexion, sql, enviar),
    .senas_conexion_dbi = function(conexion) senas,
    .listar_campos_dbi = function(conexion, nombre) {
      if (as.character(nombre) %in% sin_listado) {
        stop("Invalid object name (fingido)")
      }
      listar(conexion, nombre)
    }
  )
  do.call(testthat::local_mocked_bindings,
          c(ligaduras, list(.package = "lupa", .env = env)))
  registro
}

# Lo que lee una credencial sin UNMASK: la mascara en las dos tablas, sin un
# solo valor real en comun.
.tablas_psc <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "cliente", data.frame(
    id = 1:40, cedula = rep("xxxx", 40), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "contacto", data.frame(
    nro = 901:910, cedula = rep("xxxx", 10), stringsAsFactors = FALSE
  ))
  con
}

.catalogo_psc <- function() {
  list(
    cliente = data.frame(posicion = 1:2, columna = c("id", "cedula"), legible = 1L),
    contacto = data.frame(posicion = 1:2, columna = c("nro", "cedula"), legible = 1L)
  )
}

.mascaras_psc <- function() {
  m <- data.frame(columna = "cedula", mascara = "default()",
                  sin_mascara_columna = 0L, sin_mascara_base = 0L)
  list(cliente = m, contacto = m)
}

# Las filas de `resumen_tabla$cobertura` sobre la tabla entera -no sobre una
# columna- con estado `degradado`.
.filas_tabla_psc <- function(perfil) {
  cb <- perfil$resumen_tabla$cobertura
  cb[cb$bloque == "resumen_tabla" & cb$elemento == perfil$resumen_tabla$meta$tabla &
       cb$estado == "degradado", , drop = FALSE]
}

.perfilar_psc <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE, ...
  )))
}

test_that("perfilar_dbi declara la tabla cuyo catalogo de permisos no contesto", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc(), falla = "catalogo")
  p <- .perfilar_psc(con, "cliente")
  # La lectura es la de siempre: la mascara se mide como dato.
  columnas <- p$resumen_tabla$columnas
  expect_equal(columnas$n_distintos[columnas$columna == "cedula"], 1)
  expect_identical(p$resumen_tabla$meta$permisos_sin_consultar, "columnas")
  fila <- .filas_tabla_psc(p)
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$elemento, "cliente")
  expect_true(nzchar(fila$motivo) && nzchar(fila$como_resolverlo))
})

test_that("tambien si solo falla la consulta de mascaras", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc(), falla = "mascaras")
  p <- .perfilar_psc(con, "cliente")
  expect_identical(p$resumen_tabla$meta$permisos_sin_consultar, "mascaras")
  fila <- .filas_tabla_psc(p)
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$elemento, "cliente")
})

test_that("tambien un sinonimo: ni el controlador ni el catalogo lo ven", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motivo <- local({
    .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc(), falla = "catalogo")
    .filas_tabla_psc(.perfilar_psc(con, "cliente"))$motivo
  })
  expect_length(motivo, 1L)
  # `dbListFields()` falla y `sys.columns` no tiene filas para el nombre: los
  # nombres salen de la consulta de cero filas, que la cobertura ya declaraba.
  .emular_catalogo_psc(list(), list(), sin_listado = "cliente")
  p <- .perfilar_psc(con, "cliente")
  expect_identical(p$resumen_tabla$meta$permisos_sin_consultar, "columnas")
  expect_equal(sum(.filas_tabla_psc(p)$motivo == motivo), 1L)
})

test_that("tambien en PostgreSQL, y tambien por la via de bloques", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motivo <- local({
    .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc(), falla = "catalogo")
    .filas_tabla_psc(.perfilar_psc(con, "cliente"))$motivo
  })
  expect_length(motivo, 1L)
  # Sobre RSQLite, las consultas propias de PostgreSQL que no son del catalogo
  # de permisos tambien fallan y se declaran aparte: se busca la fila por su
  # motivo, el mismo que en SQL Server.
  .emular_catalogo_psc(.catalogo_psc(), falla = "catalogo", senas = "postgresql")
  sql <- .perfilar_psc(con, "cliente")
  expect_identical(sql$resumen_tabla$meta$permisos_sin_consultar, "columnas")
  expect_equal(sum(.filas_tabla_psc(sql)$motivo == motivo), 1L)
  bloques <- .perfilar_psc(con, "cliente", bloque_filas = 10)
  expect_identical(bloques$resumen_tabla$meta$permisos_sin_consultar, "columnas")
  expect_equal(sum(.filas_tabla_psc(bloques)$motivo == motivo), 1L)
})

test_that("con el catalogo que contesta, o sin catalogo, no se declara nada", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Control: SQLite no tiene catalogo de permisos y no hay nada que declarar.
  sqlite <- .perfilar_psc(con, "cliente")
  expect_null(sqlite$resumen_tabla$meta$permisos_sin_consultar)
  expect_equal(nrow(.filas_tabla_psc(sqlite)), 0L)
  local({
    .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc())
    p <- .perfilar_psc(con, "cliente")
    expect_null(p$resumen_tabla$meta$permisos_sin_consultar)
    expect_equal(nrow(.filas_tabla_psc(p)), 0L)
    # Y la enmascarada que el catalogo nombra sigue declarada por columna.
    cb <- p$resumen_tabla$cobertura
    expect_true(any(cb$elemento == "cedula" & cb$estado == "no_disponible"))
  })
})

test_that("perfilar_coleccion y relaciones_coleccion lo declaran por tabla", {
  skip_if_not_installed("RSQLite")
  con <- .tablas_psc()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- suppressMessages(coleccion(con, c("cliente", "contacto")))
  .emular_catalogo_psc(.catalogo_psc(), .mascaras_psc(), falla = "catalogo")
  motivo <- .filas_tabla_psc(.perfilar_psc(con, "cliente"))$motivo
  expect_length(motivo, 1L)

  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    col, proteger_datos_personales = FALSE
  )))
  cc <- pc$cobertura_coleccion
  filas <- cc[cc$alcance == "permisos_columnas", , drop = FALSE]
  expect_setequal(filas$tabla, c("cliente", "contacto"))
  expect_true(all(filas$motivo == motivo))
  # Las dos tablas se perfilaron: la fila no las cuenta como sin perfilar.
  expect_equal(pc$meta$n_sin_perfilar, 0L)

  rc <- suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "contacto", tabla_2 = "cliente")
  )))
  lecturas <- rc$meta$lecturas
  expect_setequal(lecturas$tabla, c("cliente", "contacto"))
  expect_true(all(startsWith(lecturas$motivo, motivo)))
  # La lectura no cambia: las dos mascaras se siguen relacionando, ahora dicho.
  rel <- rc$relaciones
  expect_equal(sum(rel$columna_tabla1 == "cedula" & rel$columna_tabla2 == "cedula"), 1L)
})
