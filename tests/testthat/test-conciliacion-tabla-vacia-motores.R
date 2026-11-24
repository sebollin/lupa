# Una tabla vacia, o con filas pero sin ningun valor no nulo, concilia su
# cuenta de consultas con el plan de la corrida en cada motor, con el bloque de
# muestra y sin el. PostgreSQL y SQL Server calculan el desvio en una sentencia
# aparte, y las medianas van en una o dos segun el motor: sin valores no hacen
# falta y no se mandan. El plan rehecho las seguia contando y la cuenta quedaba
# desfasada en una o dos consultas, atribuidas a los portones. Se saltean si el
# motor no esta disponible en el entorno local.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.ctv_conectar_pg <- function(puerto) {
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"), port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.ctv_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin clave de SQL Server.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.ctv_comprobar <- function(con, doble) {
  tabla <- "lupa_ctv_vacia"
  suppressWarnings(try(DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id INTEGER, x ", doble, ", s VARCHAR(20))"
  ))
  on.exit(suppressWarnings(try(
    DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
  )), add = TRUE)
  llenar <- c(
    vacia = NA_character_,
    toda_nula = "(NULL, NULL, NULL), (NULL, NULL, NULL), (NULL, NULL, NULL)",
    con_valores = "(1, 0.5, 'a'), (2, 1.5, 'b'), (3, 2.5, 'c')"
  )
  emitidas <- numeric()
  for (caso in names(llenar)) {
    if (!is.na(llenar[[caso]])) {
      DBI::dbExecute(con, paste("DELETE FROM", tabla))
      DBI::dbExecute(con, paste("INSERT INTO", tabla, "VALUES", llenar[[caso]]))
    }
    for (bloque in c("con_muestra", "solo_agregados")) {
      rotulo <- paste(caso, bloque)
      perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, bloque_muestra = bloque,
        fecha = as.POSIXct("2026-03-04 12:00:00", tz = "UTC")
      )))
      consultas <- perfil$resumen_tabla$meta$consultas
      conciliacion <- consultas$conciliacion
      expect_identical(conciliacion$estado, "conciliado", info = rotulo)
      expect_equal(as.numeric(conciliacion$diferencia), 0, info = rotulo)
      expect_equal(as.numeric(conciliacion$anunciadas),
                   as.numeric(consultas$emitidas), info = rotulo)
      # El plan publicado suma lo mismo que se mando.
      expect_equal(
        sum(as.numeric(perfil$resumen_tabla$meta$plan$n_consultas)),
        as.numeric(consultas$emitidas), info = rotulo
      )
      previo <- conciliacion$plan_previo
      if (identical(caso, "con_valores")) {
        expect_null(previo, info = rotulo)
      } else {
        # Lo anunciado antes de saber que no habia valores queda aparte, con
        # su cuenta, y la clase que no corrio no es la de arrastre.
        expect_identical(previo$estado, "desfasado", info = rotulo)
        omitidas <- previo$detalle$clase_consulta[previo$detalle$diferencia < 0]
        expect_gt(length(omitidas), 0L, label = paste("clases omitidas,", rotulo))
        expect_false(any(grepl("^portones", omitidas)), info = rotulo)
      }
      emitidas[[rotulo]] <- as.numeric(consultas$emitidas)
    }
  }
  # Con valores se manda mas: la sentencia del desvio y las medianas.
  for (bloque in c("con_muestra", "solo_agregados")) {
    con_valores <- emitidas[[paste("con_valores", bloque)]]
    expect_gt(con_valores, emitidas[[paste("vacia", bloque)]])
    expect_gt(con_valores, emitidas[[paste("toda_nula", bloque)]])
  }
}

test_that("PostgreSQL: una tabla vacia o toda nula concilia con el plan de la corrida", {
  con <- .ctv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar(con, "DOUBLE PRECISION")
})

test_that("PostgreSQL 9.3: una tabla vacia o toda nula concilia con el plan de la corrida", {
  con <- .ctv_conectar_pg(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar(con, "DOUBLE PRECISION")
})

test_that("SQL Server: una tabla vacia o toda nula concilia con el plan de la corrida", {
  con <- .ctv_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar(con, "FLOAT")
})

# ---- Una muestra de la grilla de formas de columna ------------------------
#
# En uso por omision sobre una tabla sana, la cuenta concilia sea cual sea la
# forma de las columnas (la grilla entera corre en SQLite y DuckDB, en
# `test-consultas-conciliacion-formas.R`). Aca, las formas que fallaban en los
# servidores: una columna numerica toda nula entre otras con valores, todas
# nulas menos una de texto, un entero que no cabe exacto en un doble y una
# tabla comun con una de cada cosa.
.ctv_formas <- function(doble) {
  ancho <- sprintf("%.0f", 9007199254740992 + 2 * 1:12)
  list(
    una_numerica_nula = list(
      columnas = paste0("id INTEGER, x ", doble, ", s VARCHAR(20)"),
      filas = paste0("(", 1:12, ", NULL, 'v", 1:12 %% 3L, "')")
    ),
    todas_nulas_menos_texto = list(
      columnas = paste0("a ", doble, ", b BIGINT, d DATE, s VARCHAR(20)"),
      filas = paste0("(NULL, NULL, NULL, 'v", 1:12 %% 2L, "')")
    ),
    entero_ancho = list(
      columnas = "c1 BIGINT", filas = paste0("(", ancho, ")")
    ),
    comun = list(
      columnas = paste0(
        "id INTEGER, s VARCHAR(20), x ", doble, ", d DATE, m DECIMAL(12,2)"
      ),
      filas = paste0(
        "(", 1:12, ", 'v", 1:12 %% 2L, "', ", c("1.5", rep("NULL", 11L)),
        ", '2024-01-02', NULL)"
      )
    )
  )
}

.ctv_comprobar_formas <- function(con, doble, con_previo) {
  tabla <- "lupa_ctv_forma"
  quitar <- function() {
    suppressWarnings(try(
      DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
    ))
  }
  on.exit(quitar(), add = TRUE)
  formas <- .ctv_formas(doble)
  for (nombre in names(formas)) {
    quitar()
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (", formas[[nombre]]$columnas, ")"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ",
      paste(formas[[nombre]]$filas, collapse = ", ")
    ))
    for (bloque in c("con_muestra", "solo_agregados")) {
      rotulo <- paste(nombre, bloque)
      perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, bloque_muestra = bloque,
        fecha = as.POSIXct("2026-03-04 12:00:00", tz = "UTC")
      )))
      consultas <- perfil$resumen_tabla$meta$consultas
      conciliacion <- consultas$conciliacion
      expect_identical(conciliacion$estado, "conciliado", info = rotulo)
      expect_equal(as.numeric(conciliacion$anunciadas),
                   as.numeric(consultas$emitidas), info = rotulo)
      expect_equal(
        sum(as.numeric(perfil$resumen_tabla$meta$plan$n_consultas)),
        as.numeric(consultas$emitidas), info = rotulo
      )
      previo <- conciliacion$plan_previo
      if (nombre %in% con_previo) {
        # Lo anunciado antes de leer los datos queda aparte, con la clase que
        # cambio -nunca la de arrastre- y un motivo que no cita una falla.
        expect_identical(previo$estado, "desfasado", info = rotulo)
        cambios <- previo$detalle[previo$detalle$diferencia != 0, , drop = FALSE]
        expect_gt(nrow(cambios), 0L, label = paste("clases que cambiaron,", rotulo))
        expect_false(any(grepl("^portones", cambios$clase_consulta)), info = rotulo)
        expect_false(any(grepl("fall", cambios$motivo)), info = rotulo)
      }
    }
  }
}

test_that("PostgreSQL: la cuenta concilia sea cual sea la forma de las columnas", {
  con <- .ctv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar_formas(
    con, "DOUBLE PRECISION",
    c("una_numerica_nula", "todas_nulas_menos_texto", "entero_ancho", "comun")
  )
})

test_that("PostgreSQL 9.3: la cuenta concilia sea cual sea la forma de las columnas", {
  con <- .ctv_conectar_pg(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar_formas(
    con, "DOUBLE PRECISION",
    c("una_numerica_nula", "todas_nulas_menos_texto", "entero_ancho", "comun")
  )
})

test_that("SQL Server: la cuenta concilia sea cual sea la forma de las columnas", {
  con <- .ctv_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Ahi las medianas van en una sola sentencia: con una columna nula entre
  # otras no cambia nada y no hay plan previo.
  .ctv_comprobar_formas(
    con, "FLOAT", c("todas_nulas_menos_texto", "entero_ancho", "comun")
  )
})

# Lo mismo con cualquier seleccion de `metricas`, tambien sobre la tabla vacia.
# Sin `validos` la corrida no cuenta los valores de cada columna, y los mismos
# datos deciden otras cosas: el promedio nulo hace preguntar por infinitos, el
# desvio y la mediana de una columna sin valores no se mandan, y la mediana de
# un entero que pasa de 2^53 no se paga.
.ctv_comprobar_metricas <- function(con, doble, con_infinitos = FALSE) {
  tabla <- "lupa_ctv_metrica"
  quitar <- function() {
    suppressWarnings(try(
      DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE
    ))
  }
  on.exit(quitar(), add = TRUE)
  formas <- c(
    list(vacia = list(
      columnas = paste0("id INTEGER, x ", doble, ", d DATE, s VARCHAR(20)"),
      filas = character()
    )),
    .ctv_formas(doble)
  )
  if (con_infinitos) {
    # Una columna que guarda un infinito: sus magnitudes se vuelven a pedir
    # sobre los valores finitos, y eso el plan no lo cuenta.
    formas$con_infinito <- list(
      columnas = paste0("id INTEGER, x ", doble),
      filas = paste0("(", 1:12, ", ", c("'Infinity'", 2:12 / 4), ")")
    )
  }
  selecciones <- list(
    "basicos", "desvio", "mediana", c("basicos", "mediana"),
    c("basicos", "desvio"), c("validos", "distintos", "moda"),
    c("validos", "basicos", "desvio"),
    c("validos", "distintos", "moda", "basicos", "mediana", "desvio")
  )
  perfilar <- function(seleccion) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = seleccion,
      fecha = as.POSIXct("2026-03-04 12:00:00", tz = "UTC")
    )))
  }
  desfasadas <- character()
  for (nombre in names(formas)) {
    quitar()
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (", formas[[nombre]]$columnas, ")"
    ))
    if (length(formas[[nombre]]$filas)) {
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla, " VALUES ",
        paste(formas[[nombre]]$filas, collapse = ", ")
      ))
    }
    for (seleccion in selecciones) {
      consultas <- perfilar(seleccion)$resumen_tabla$meta$consultas
      if (!identical(consultas$conciliacion$estado, "conciliado") ||
          !isTRUE(as.numeric(consultas$conciliacion$anunciadas) ==
                    as.numeric(consultas$emitidas))) {
        desfasadas <- c(
          desfasadas, paste(nombre, paste(seleccion, collapse = "+"))
        )
      }
    }
    if (identical(nombre, "vacia") && con_infinitos) {
      # El plan anunciaba de menos: no contaba la pregunta por los infinitos
      # que el promedio nulo de la tabla vacia dispara. Queda en el plan
      # previo, en su clase y con su causa.
      previo <- perfilar("basicos")$resumen_tabla$meta$consultas$
        conciliacion$plan_previo
      expect_identical(previo$estado, "desfasado")
      de_mas <- previo$detalle[previo$detalle$diferencia > 0, , drop = FALSE]
      expect_identical(nrow(de_mas), 1L)
      expect_match(de_mas$clase_consulta, "^agregados planos")
      expect_match(de_mas$motivo, "guarda infinitos")
    }
  }
  expect_identical(desfasadas, character())
}

test_that("PostgreSQL: la cuenta concilia con cualquier seleccion de metricas", {
  con <- .ctv_conectar_pg(Sys.getenv("LUPA_PG_PORT", "55432"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar_metricas(con, "DOUBLE PRECISION", con_infinitos = TRUE)
})

test_that("PostgreSQL 9.3: la cuenta concilia con cualquier seleccion de metricas", {
  con <- .ctv_conectar_pg(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar_metricas(con, "DOUBLE PRECISION", con_infinitos = TRUE)
})

test_that("SQL Server: la cuenta concilia con cualquier seleccion de metricas", {
  con <- .ctv_conectar_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ctv_comprobar_metricas(con, "FLOAT")
})
