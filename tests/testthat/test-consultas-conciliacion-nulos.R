testthat::skip_on_cran()

test_that("una tabla sin valores concilia, y explica las consultas que no hicieron falta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  suppressPackageStartupMessages(library(DBI))
  clase <- "LupaNulosFalla"
  if (!methods::isClass(clase)) {
    methods::setClass(clase, contains = "SQLiteConnection")
    rechazar <- function(sql) {
      patrones <- getOption("lupa.nulos.patrones")
      if (length(patrones)) {
        for (patron in patrones) {
          if (grepl(patron, sql, fixed = TRUE, useBytes = TRUE)) {
            stop("rechazada por prueba: ", patron, call. = FALSE)
          }
        }
      }
    }
    for (nombre in c(
      "dbGetQuery", "dbSendQuery", "dbExecute", "dbSendStatement"
    )) {
      methods::setMethod(nombre, signature(clase, "character"),
        function(conn, statement, ...) {
          rechazar(statement)
          callNextMethod()
        })
    }
  }
  conexion_base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion_base), add = TRUE)
  DBI::dbWriteTable(conexion_base, "nulos", data.frame(
    a = rep(NA_real_, 10), b = rep(NA_character_, 10),
    c = rep(NA_integer_, 10)
  ))
  conexion <- methods::new(clase, conexion_base)
  opciones <- options(lupa.nulos.patrones = c(
    "information_schema.table_constraints", "STDDEV_SAMP"
  ))
  on.exit(options(opciones), add = TRUE)

  configuraciones <- list(
    list(), list(bloque_muestra = "solo_agregados")
  )
  for (argumentos in configuraciones) {
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, c(list(conexion, "nulos"), argumentos)
    )))
    consultas <- perfil$resumen_tabla$meta$consultas
    conciliacion <- consultas$conciliacion
    # Las medianas de columnas sin ningun valor no hacen falta y no se mandan.
    # Eso no es un desfase: la cuenta concilia con el plan que publica la
    # corrida, que es lo que se mando.
    expect_identical(conciliacion$estado, "conciliado")
    expect_equal(as.numeric(conciliacion$diferencia), 0)
    expect_equal(as.numeric(conciliacion$anunciadas),
                 as.numeric(consultas$emitidas))
    expect_equal(
      sum(as.numeric(perfil$resumen_tabla$meta$plan$n_consultas)),
      as.numeric(consultas$emitidas)
    )
    expect_equal(anyDuplicated(conciliacion$detalle$clase_consulta), 0L)
    # Lo anunciado antes de saberlo queda aparte: cuanto era, que clase no
    # corrio -la de las medianas, no una clase de arrastre- y por que.
    previo <- conciliacion$plan_previo
    expect_identical(previo$estado, "desfasado")
    expect_equal(as.numeric(previo$diferencia), -2)
    expect_true(grepl("valores no nulos", previo$motivo,
                      fixed = TRUE, useBytes = TRUE))
    omitidas <- previo$detalle[previo$detalle$diferencia < 0, , drop = FALSE]
    expect_identical(nrow(omitidas), 1L)
    expect_true(grepl("^mediana \\(", omitidas$clase_consulta,
                      perl = TRUE, useBytes = TRUE))
    expect_equal(as.numeric(omitidas$diferencia), -2)
    motivos <- paste(as.character(previo$detalle$motivo), collapse = " ")
    expect_true(grepl("no hay valores no nulos", motivos,
                      fixed = TRUE, useBytes = TRUE))
    patron_interno <- paste0("clave_primaria|", paste0("son", "da_desvio"))
    expect_false(grepl(patron_interno, motivos,
                       perl = TRUE, useBytes = TRUE))
    expect_true(grepl("no hacen falta", motivos,
                      fixed = TRUE, useBytes = TRUE))
    expect_equal(anyDuplicated(previo$detalle$clase_consulta), 0L)
  }
})

test_that("vacia o toda nula, con o sin el bloque de muestra, la cuenta concilia", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  abrir <- list(
    sqlite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = function() {
      skip_if_not_installed("duckdb")
      suppressMessages(DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:"))
    }
  )
  for (motor in names(abrir)) {
    conexion <- abrir[[motor]]()
    DBI::dbExecute(conexion, "CREATE TABLE lupa_sin_filas (id INTEGER, x DOUBLE, s VARCHAR(20))")
    DBI::dbExecute(conexion, "CREATE TABLE lupa_toda_nula (id INTEGER, x DOUBLE, s VARCHAR(20))")
    DBI::dbExecute(conexion, paste(
      "INSERT INTO lupa_toda_nula VALUES",
      "(NULL, NULL, NULL), (NULL, NULL, NULL), (NULL, NULL, NULL)"
    ))
    # El control: con una fila con valores las medianas se mandan.
    DBI::dbExecute(conexion, "CREATE TABLE lupa_con_valores (id INTEGER, x DOUBLE, s VARCHAR(20))")
    DBI::dbExecute(conexion, "INSERT INTO lupa_con_valores VALUES (1, 0.5, 'a'), (2, 1.5, 'b'), (3, 2.5, 'c')")
    for (bloque in c("con_muestra", "solo_agregados")) {
      emitidas <- numeric()
      for (tabla in c("lupa_sin_filas", "lupa_toda_nula", "lupa_con_valores")) {
        perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
          conexion, tabla, bloque_muestra = bloque,
          fecha = as.POSIXct("2026-03-04 12:00:00", tz = "UTC")
        )))
        consultas <- perfil$resumen_tabla$meta$consultas
        rotulo <- paste(motor, tabla, bloque)
        expect_identical(consultas$conciliacion$estado, "conciliado", info = rotulo)
        expect_equal(as.numeric(consultas$conciliacion$anunciadas),
                     as.numeric(consultas$emitidas), info = rotulo)
        expect_equal(
          sum(as.numeric(perfil$resumen_tabla$meta$plan$n_consultas)),
          as.numeric(consultas$emitidas), info = rotulo
        )
        emitidas[[tabla]] <- as.numeric(consultas$emitidas)
      }
      # Sin valores nunca se manda mas que con valores, y en SQLite -una
      # mediana por columna- se manda menos: no concilia por contar lo mismo,
      # sino porque el plan publicado es lo que se mando.
      rotulo <- paste(motor, bloque)
      expect_lte(emitidas[["lupa_sin_filas"]], emitidas[["lupa_con_valores"]],
                 label = paste("emitidas sin filas,", rotulo))
      expect_lte(emitidas[["lupa_toda_nula"]], emitidas[["lupa_con_valores"]],
                 label = paste("emitidas toda nula,", rotulo))
      if (identical(motor, "sqlite")) {
        expect_lt(emitidas[["lupa_sin_filas"]], emitidas[["lupa_con_valores"]])
        expect_lt(emitidas[["lupa_toda_nula"]], emitidas[["lupa_con_valores"]])
      }
    }
    if (identical(motor, "duckdb")) {
      DBI::dbDisconnect(conexion, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(conexion)
    }
  }
})

test_that("una clase repetida queda una sola vez en el detalle", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  suppressPackageStartupMessages(library(DBI))
  clase <- "LupaNulosFalla"
  if (!methods::isClass(clase)) {
    methods::setClass(clase, contains = "SQLiteConnection")
  }
  conexion_base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion_base), add = TRUE)
  DBI::dbWriteTable(conexion_base, "t", data.frame(
    id = 1:60, x = seq_len(60) / 10,
    g = rep(letters[1:4], length.out = 60),
    stringsAsFactors = FALSE
  ))
  conexion <- methods::new(clase, conexion_base)
  opciones <- options(lupa.nulos.patrones = c("COUNT(DISTINCT", "MAX("))
  on.exit(options(opciones), add = TRUE)

  for (maximo in list(NULL, 12L)) {
    argumentos <- list(conexion, "t")
    if (!is.null(maximo)) argumentos$max_consultas <- maximo
    perfil <- suppressWarnings(suppressMessages(do.call(
      perfilar_dbi, argumentos
    )))
    consultas <- perfil$resumen_tabla$meta$consultas
    detalle <- consultas$conciliacion$detalle
    expect_equal(anyDuplicated(detalle$clase_consulta), 0L)
    expect_equal(
      sum(as.numeric(detalle$diferencia)),
      as.numeric(consultas$conciliacion$diferencia)
    )
    if (!is.null(maximo)) {
      agregados <- detalle[grepl("agregados planos", detalle$clase_consulta,
                                 fixed = TRUE, useBytes = TRUE), , drop = FALSE]
      expect_equal(nrow(agregados), 1L)
      expect_identical(agregados$direccion, "mixta")
      expect_true(grepl("presupuesto", agregados$motivo,
                        fixed = TRUE, useBytes = TRUE))
      expect_true(grepl("respaldo", agregados$motivo,
                        fixed = TRUE, useBytes = TRUE))
    }
  }
})
