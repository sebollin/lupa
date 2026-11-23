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
