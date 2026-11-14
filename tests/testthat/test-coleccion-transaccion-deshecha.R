# La lectura de la coleccion cuando el motor deshace la transaccion del usuario
# en la consulta de columnas y permisos. `.columnas_tabla_dbi()` resuelve adentro
# la falla del listado y la del catalogo, y deja salir a proposito una sola
# condicion, `lupa_error_transaccion_dbi`: `perfilar_dbi()` y
# `perfilar_coleccion()` se detienen con ella, como promete `?perfilar_dbi`
# ("Dentro de una transaccion", que dice lo mismo de `relaciones_coleccion()`).
# `relaciones_coleccion()` la tragaba: leia las dos tablas con `SELECT *`, ya
# fuera de la transaccion, y relacionaba dos columnas enmascaradas sin un solo
# valor real en comun con cobertura 1. SQL Server no esta en la maquina de CRAN:
# su catalogo se finge sobre RSQLite, como en `test-O165-permisos.R`. ASCII.
testthat::skip_on_cran()

# `deshecha`: si la consulta al catalogo levanta la condicion de la via.
.catalogo_sqlserver_txd <- function(estado, env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  condicion <- get(".condicion_dbi", envir = ns)
  catalogo <- function(columnas) data.frame(
    posicion = seq_along(columnas), columna = columnas, legible = 1L
  )
  motor <- function(conexion, sql, original) {
    estado$sql <- c(estado$sql, sql)
    if (grepl("sys.columns|sys.masked_columns", sql, perl = TRUE)) {
      if (isTRUE(estado$deshecha)) {
        stop(condicion(
          "lupa_error_transaccion_dbi",
          "SQL Server deshizo la transaccion del usuario durante la consulta."
        ))
      }
      if (grepl("sys.masked_columns", sql, fixed = TRUE, useBytes = TRUE)) {
        return(data.frame(columna = "cedula", mascara = "default()",
                          sin_mascara_columna = 0L, sin_mascara_base = 0L))
      }
      if (grepl("[`\"]cliente[`\"]", sql, perl = TRUE)) {
        return(catalogo(c("id", "cedula")))
      }
      return(catalogo(c("nro", "cedula")))
    }
    original(conexion, sql)
  }
  testthat::local_mocked_bindings(
    .traer_consulta_dbi = function(conexion, sql) motor(conexion, sql, traer),
    .enviar_consulta_dbi = function(conexion, sql) motor(conexion, sql, enviar),
    .senas_conexion_dbi = function(conexion) "microsoft sql server odbc",
    .package = "lupa", .env = env
  )
  invisible(estado)
}

# Lo que lee una credencial sin UNMASK: la mascara, en las dos tablas.
.tablas_enmascaradas_txd <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "cliente", data.frame(
    id = seq_len(40L), cedula = rep("xxxx", 40L)
  ))
  DBI::dbWriteTable(con, "contacto", data.frame(
    nro = 900L + seq_len(10L), cedula = rep("xxxx", 10L)
  ))
  con
}

test_that("una transaccion deshecha en el catalogo detiene relaciones_coleccion como detiene perfilar_dbi", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .tablas_enmascaradas_txd()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  estado <- new.env()
  estado$deshecha <- FALSE
  estado$sql <- character()
  .catalogo_sqlserver_txd(estado)
  col <- suppressMessages(coleccion(con, c("cliente", "contacto")))
  pares <- data.frame(tabla_1 = "contacto", tabla_2 = "cliente")

  # Con el catalogo sano las mascaras no se comparan: el motor fingido decide
  # lo que tiene que decidir.
  sano <- suppressWarnings(suppressMessages(relaciones_coleccion(col, pares = pares)))
  expect_false(any(sano$relaciones$columna_tabla1 == "cedula" &
                     sano$relaciones$columna_tabla2 == "cedula"))
  expect_true(any(sano$cobertura_podas$motivo == "columna_enmascarada"))

  estado$deshecha <- TRUE
  expect_error(
    suppressWarnings(suppressMessages(perfilar_dbi(con, "cliente"))),
    class = "lupa_error_transaccion_dbi"
  )
  estado$sql <- character()
  expect_error(
    suppressWarnings(suppressMessages(relaciones_coleccion(col, pares = pares))),
    class = "lupa_error_transaccion_dbi"
  )
  # Despues de la falla no se mando ninguna lectura de datos.
  expect_false(any(grepl("FROM [`\"](cliente|contacto)[`\"]", estado$sql, perl = TRUE)))

  # La validacion de `columnas_candidatas` consulta el mismo catalogo.
  estado$sql <- character()
  expect_error(
    suppressWarnings(suppressMessages(relaciones_coleccion(
      col, pares = pares,
      columnas_candidatas = list(cliente = c("id", "cedula"), contacto = c("nro", "cedula"))
    ))),
    class = "lupa_error_transaccion_dbi"
  )
  expect_false(any(grepl("FROM [`\"](cliente|contacto)[`\"]", estado$sql, perl = TRUE)))
})
