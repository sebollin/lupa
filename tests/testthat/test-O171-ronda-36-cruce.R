# O171: lo que aparecio al unir los frentes de la ronda 36. La lectura de los
# float8 de PostgreSQL (36-D) se proyecta con un alias propio, y la proteccion
# de la coleccion (36-P) usa la lectura publicada dentro de
# `SELECT ... AS valor ... GROUP BY`. Con el alias adentro la consulta no se
# podia preparar: la coleccion declaraba la proteccion sin verificar y tapaba
# columnas que no son personales. El archivo es ASCII.

test_that("O171: la lectura publicada de un float8 no lleva el alias de la proyeccion", {
  con <- structure(list(), class = "PqConnection")
  lectura <- lupa:::.lectura_convertida_dbi(
    con, c("cedula", "monto"), c("\"cedula\"", "\"monto\""),
    c("float8", "money")
  )
  expect_false(grepl("AS \"cedula\"", lectura$campos_sql[[1L]], fixed = TRUE))
  expect_true(endsWith(lectura$proyeccion_sql[[1L]], " AS \"cedula\""))
  publicada <- lectura$convertidas$lectura[[1L]]
  expect_false(grepl("AS \"cedula\"", publicada, fixed = TRUE))
  expect_true(endsWith(publicada, " END"))
  expect_identical(lectura$convertidas$lectura[[2L]], "CAST(\"monto\" AS NUMERIC)")
})

test_that("O171: una coleccion con float8 en PostgreSQL verifica la proteccion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"),
      user = "postgres", password = Sys.getenv("LUPA_PG_PASSWORD", "lupa")
    ),
    error = function(e) NULL
  )
  if (is.null(con)) skip("PostgreSQL real no disponible en el puerto 55432.")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ced <- c(46854679, 24997162, 10194331, 37192853, 51289041, 19283745,
           28374658, 34567890, 41122335, 54605371)
  personas <- data.frame(
    id = 1:10, cedula = ced,
    nombre = c("Ana Perez", "Juan Diaz", "Rosa Gomez", "Luis Silva",
               "Marta Ruiz", "Pedro Sosa", "Laura Rios", "Diego Mora",
               "Elena Vega", "Jorge Pena")
  )
  tramites <- data.frame(
    id_tramite = 1:11, doc_ref = ced[c(1, 1:10)], monto = (1:11) * 1234.5
  )
  tablas <- c("o171_personas", "o171_tramites")
  limpiar <- function() {
    for (t in tablas) try(DBI::dbRemoveTable(con, t), silent = TRUE)
  }
  limpiar()
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbWriteTable(con, tablas[[1L]], personas)
  DBI::dbWriteTable(con, tablas[[2L]], tramites)
  tipos <- DBI::dbGetQuery(con, paste0(
    "SELECT column_name, data_type FROM information_schema.columns ",
    "WHERE table_name = 'o171_tramites'"
  ))
  expect_true(all(tipos$data_type[tipos$column_name != "id_tramite"] ==
                    "double precision"))
  col <- perfilar_coleccion(coleccion(con, tablas), conservar_perfiles = TRUE)
  p <- col$perfiles$o171_tramites
  otras <- paste(unlist(p$resumen_tabla$meta$proteccion_personal$otras_tablas),
                 collapse = " ")
  expect_false(grepl("no se pudieron leer", otras, fixed = TRUE))
  fila <- function(columna) {
    p$resumen_tabla$columnas[p$resumen_tabla$columnas$columna == columna, ,
                             drop = FALSE]
  }
  # La copia de la cedula sigue tapada y el importe, que no es personal, publica.
  expect_identical(as.character(fila("doc_ref")$moda), "[valor protegido]")
  expect_equal(as.numeric(fila("monto")$minimo), 1234.5)
  expect_equal(as.numeric(fila("monto")$maximo), 13579.5)
})
