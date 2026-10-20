# O169: las cinco decisiones de la refutacion DBI, ejercidas por las puertas
# publicas sobre RSQLite con respuestas de motor fingidas. El archivo es ASCII.

.r36x_catalogo <- function(columnas, legible = 1L) {
  data.frame(
    posicion = seq_along(columnas), columna = columnas,
    legible = rep_len(as.integer(legible), length(columnas)),
    stringsAsFactors = FALSE
  )
}

.r36x_finge_catalogo <- function(catalogos, listados, senas,
                                 comprueba_citas = FALSE,
                                 env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  listar <- get(".listar_campos_dbi", envir = ns)
  registro <- new.env(parent = emptyenv())
  registro$sql <- character()
  tabla_en_sql <- function(sql) {
    nombres <- names(catalogos)
    encontrados <- nombres[vapply(nombres, function(nombre) {
      grepl(nombre, sql, fixed = TRUE)
    }, logical(1L))]
    if (length(encontrados)) encontrados[[1L]] else NA_character_
  }
  motor <- function(conexion, sql, original) {
    registro$sql <- c(registro$sql, sql)
    tabla <- tabla_en_sql(sql)
    if (grepl("sys.columns", sql, fixed = TRUE)) {
      respuesta <- if (!is.na(tabla)) catalogos[[tabla]] else {
        data.frame(posicion = integer(), columna = character(),
                   legible = integer(), stringsAsFactors = FALSE)
      }
      if (isTRUE(comprueba_citas) &&
          !grepl("QUOTENAME(c.name)", sql, fixed = TRUE)) {
        respuesta$legible <- 0L
      }
      return(respuesta)
    }
    if (grepl("sys.masked_columns", sql, fixed = TRUE)) {
      return(data.frame(columna = character(), mascara = character(),
                        sin_mascara_columna = integer(),
                        sin_mascara_base = integer(),
                        stringsAsFactors = FALSE))
    }
    original(conexion, sql)
  }
  ligaduras <- list(
    .traer_consulta_dbi = function(conexion, sql) {
      motor(conexion, sql, traer)
    },
    .enviar_consulta_dbi = function(conexion, sql) {
      motor(conexion, sql, enviar)
    },
    .listar_campos_dbi = function(conexion, nombre) {
      texto <- tryCatch(
        if (isS4(nombre)) nombre@name[["table"]] else as.character(nombre)[[1L]],
        error = function(e) ""
      )
      if (length(texto) && texto %in% names(listados)) {
        return(listados[[texto]])
      }
      listar(conexion, nombre)
    },
    .senas_conexion_dbi = function(conexion) senas
  )
  do.call(testthat::local_mocked_bindings,
          c(ligaduras, list(.package = "lupa", .env = env)))
  registro
}

.r36x_fila <- function(resultado, columna) {
  resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == columna, , drop = FALSE
  ]
}

test_that("O169 A1: perfilar_dbi se detiene si la consulta destruye la transaccion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "r36x_a1", data.frame(id = 1:3, valor = 4:6))
  estado <- new.env(parent = emptyenv())
  estado$fallo <- FALSE
  estado$consultas <- character()
  original <- DBI::dbGetQuery
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "microsoft sql server",
    .package = "lupa"
  )
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      estado$consultas <- c(estado$consultas, statement)
      if (grepl("@@TRANCOUNT", statement, fixed = TRUE)) {
        return(data.frame(
          lupa_trancount = if (estado$fallo) 0 else 1,
          lupa_xact_state = if (estado$fallo) 0 else 1
        ))
      }
      if (grepl("r36x_a1", statement, fixed = TRUE) &&
          !grepl("WHERE 1 = 0", statement, fixed = TRUE) &&
          !grepl("sqlite_master", statement, fixed = TRUE)) {
        estado$fallo <- TRUE
        stop("conversion invalida", call. = FALSE)
      }
      original(conn, statement, ...)
    },
    .package = "DBI"
  )
  expect_error(
    perfilar_dbi(con, "r36x_a1", metricas = "validos", incluir_valores = FALSE,
                 instrumentar = FALSE),
    class = "lupa_error_transaccion_dbi"
  )
  estados <- estado$consultas[grepl("@@TRANCOUNT", estado$consultas, fixed = TRUE)]
  expect_true(any(grepl("lupa_trancount", estados, fixed = TRUE)))
  expect_true(isTRUE(estado$fallo))
  expect_true(length(estados) >= 2L)
})

test_that("O169 A2: las columnas con punto corchete y comillas siguen legibles", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  columnas <- c("id", "a.b", paste0("a", rawToChar(as.raw(0x5d)), "b"),
                paste0("a", rawToChar(as.raw(0x22)), "b"))
  datos <- data.frame(id = 1:3, uno = 11:13, dos = 21:23, tres = 31:33)
  names(datos) <- columnas
  DBI::dbWriteTable(con, "r36x_a2", datos)
  DBI::dbWriteTable(con, "r36x_a2_b", datos)
  catalogos <- list(
    r36x_a2 = .r36x_catalogo(columnas),
    r36x_a2_b = .r36x_catalogo(columnas)
  )
  registro <- .r36x_finge_catalogo(
    catalogos, lapply(catalogos, function(x) columnas),
    "microsoft sql server", comprueba_citas = TRUE
  )
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "r36x_a2", metricas = "validos",
    incluir_valores = FALSE, instrumentar = FALSE,
    proteger_datos_personales = FALSE
  )))
  columnas_r <- r$resumen_tabla$columnas
  expect_setequal(columnas_r$columna, columnas)
  expect_true(all(columnas_r$n_validos == 3))
  rel <- suppressWarnings(suppressMessages(relaciones_coleccion(
    coleccion(con, c("r36x_a2", "r36x_a2_b")),
    pares = data.frame(tabla_1 = "r36x_a2", tabla_2 = "r36x_a2_b",
                       stringsAsFactors = FALSE),
    muestra = 3, umbral_cobertura = 1
  )))
  expect_true(any(rel$relaciones$columna_tabla1 == "a.b" &
                   rel$relaciones$columna_tabla2 == "a.b"))
  expect_gt(length(registro$sql), 0L)
})

test_that("O169 M1: el perfil usa las columnas del objeto del catalogo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "r36x_m1", data.frame(id = 1:4, a = 11:14))
  catalogo <- .r36x_catalogo(c("id", "a"))
  .r36x_finge_catalogo(
    list(r36x_m1 = catalogo),
    list(r36x_m1 = c("id", "a", "fantasma", "de_otro_esquema")),
    "microsoft sql server"
  )
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "r36x_m1", metricas = "validos",
    incluir_valores = FALSE, instrumentar = FALSE,
    proteger_datos_personales = FALSE
  )))
  columnas <- r$resumen_tabla$columnas
  expect_setequal(columnas$columna, c("id", "a"))
  expect_true(all(columnas$n_validos == 4))
})

.r36x_finge_columna_rota <- function(estado, env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  es_rota <- function(sql) {
    grepl("r36x_m2", sql, fixed = TRUE) &&
      grepl("cod", sql, fixed = TRUE) &&
      !grepl("WHERE 1 = 0", sql, fixed = TRUE)
  }
  rechazar <- function(sql) {
    if (es_rota(sql)) {
      estado$rechazos <- estado$rechazos + 1L
      stop("conversion invalida en cod", call. = FALSE)
    }
  }
  ligaduras <- list(
    .traer_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      DBI::dbGetQuery(conexion, sql)
    },
    .enviar_consulta_dbi = function(conexion, sql) {
      rechazar(sql)
      enviar(conexion, sql)
    }
  )
  do.call(testthat::local_mocked_bindings,
          c(ligaduras, list(.package = "lupa", .env = env)))
  invisible(traer)
}

test_that("O169 M2: las tres vias aislan solo la columna de la fila rota", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "r36x_m2", data.frame(
    id = 1:5, cod = 11:15, nombre = paste0("n", 1:5),
    stringsAsFactors = FALSE
  ))
  estado <- new.env(parent = emptyenv())
  estado$rechazos <- 0L
  .r36x_finge_columna_rota(estado)
  args <- list(
    conexion = con, tabla = "r36x_m2", metricas = "validos",
    incluir_valores = FALSE, instrumentar = FALSE,
    proteger_datos_personales = FALSE
  )
  sql <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(args, list(universo = "muestra_motor", muestra_motor = 3,
                               muestra = 3, bloque_muestra = "solo_agregados"))
  )))
  spool <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(args, list(universo = "muestra_motor", muestra_motor = 3,
                               muestra = 3))
  )))
  bloques <- suppressWarnings(suppressMessages(do.call(
    perfilar_dbi, c(args, list(bloque_filas = 2))
  )))
  for (r in list(sql, spool, bloques)) {
    id <- .r36x_fila(r, "id")
    cod <- .r36x_fila(r, "cod")
    nombre <- .r36x_fila(r, "nombre")
    expect_equal(id$n_validos, if (identical(r, bloques)) 5 else 3)
    expect_equal(nombre$n_validos, if (identical(r, bloques)) 5 else 3)
    expect_true(is.na(cod$n_validos))
  }
  expect_gte(estado$rechazos, 3L)
})

test_that("O169 H3: la relacion conserva texto latin1 bajo la locale C", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
  if (is.na(Sys.setlocale("LC_CTYPE", "C"))) skip("locale C no disponible")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  valor <- intToUtf8(c(0x00c9, 0x00bb))
  DBI::dbWriteTable(con, "r36x_h3_a", data.frame(
    tramo = rep(valor, 3), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "r36x_h3_b", data.frame(
    tramo_nv = rep(valor, 3), stringsAsFactors = FALSE
  ))
  traer_filas <- get(".traer_filas_dbi", envir = asNamespace("lupa"))
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "sqlite",
    .controlador_odbc_dbi = function(conexion) TRUE,
    .traer_consulta_dbi = function(conexion, sql) {
      datos <- DBI::dbGetQuery(conexion, sql)
      tipos <- rep("4", ncol(datos))
      es_a <- grepl('"r36x_h3_a"', sql, fixed = TRUE)
      for (i in seq_along(datos)) {
        if (!is.character(datos[[i]])) next
        if (es_a) {
          latin1 <- iconv(datos[[i]], from = "UTF-8", to = "latin1")
          Encoding(latin1) <- "UTF-8"
          datos[[i]] <- latin1
          tipos[[i]] <- "12"
        } else {
          tipos[[i]] <- "-9"
        }
      }
      attr(datos, "lupa_tipos_odbc") <- tipos
      .texto_del_controlador_dbi(conexion, datos)
    },
    .traer_filas_dbi = function(resultado, n = -1L, conexion) {
      datos <- traer_filas(resultado, n = n, conexion = conexion)
      tipos <- rep("4", ncol(datos))
      for (i in seq_along(datos)) {
        if (!is.character(datos[[i]])) next
        latin1 <- iconv(datos[[i]], from = "UTF-8", to = "latin1")
        Encoding(latin1) <- "UTF-8"
        datos[[i]] <- latin1
        tipos[[i]] <- "12"
      }
      attr(datos, "lupa_tipos_odbc") <- tipos
      .texto_del_controlador_dbi(conexion, datos)
    },
    .package = "lupa"
  )
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "r36x_h3_a", metricas = "validos",
    incluir_valores = FALSE, instrumentar = FALSE,
    proteger_datos_personales = FALSE, muestra = 3
  )))
  leido <- perfil$perfil_muestra$columnas$moda[[1L]]
  expect_true(is.character(leido) && length(leido) == 1L,
              info = "el perfil publico no devolvio la columna de texto")
  expect_identical(charToRaw(leido),
                   as.raw(c(0xc3, 0x89, 0xc2, 0xbb)))
  relaciones <- suppressWarnings(suppressMessages(relaciones_coleccion(
    coleccion(con, c("r36x_h3_a", "r36x_h3_b")),
    pares = data.frame(tabla_1 = "r36x_h3_a", tabla_2 = "r36x_h3_b",
                       stringsAsFactors = FALSE),
    muestra = 3, umbral_cobertura = 1
  )))
  fila <- relaciones$relaciones[
    relaciones$relaciones$columna_tabla1 == "tramo" &
      relaciones$relaciones$columna_tabla2 == "tramo_nv", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_valores_comunes, 1)
  falsa <- structure(list(), class = "OdbcConnection")
  bytes <- rawToChar(as.raw(c(0xc9, 0xbb)))
  Encoding(bytes) <- "UTF-8"
  entrega <- data.frame(tramo = bytes, stringsAsFactors = FALSE)
  attr(entrega, "lupa_tipos_odbc") <- "12"
  corregida <- .texto_del_controlador_dbi(falsa, entrega)
  expect_identical(charToRaw(corregida$tramo[[1L]]),
                   as.raw(c(0xc3, 0x89, 0xc2, 0xbb)))
})
