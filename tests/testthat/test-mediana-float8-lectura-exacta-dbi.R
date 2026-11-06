# En PostgreSQL antes de la 12, o con `extra_float_digits <= 0`, un `float8`
# viaja con 15 cifras, y el paquete lo lee como NUMERIC exacto por sus ocho
# bytes. La via de dos consultas promedia las dos centrales en doble y lee el
# resultado tambien por sus bytes. La CTE de ventanas (con `dialecto =
# "portable"`) y la subconsulta escalar de la muestra promediaban el NUMERIC con
# `AVG()`, que PostgreSQL devuelve redondeado a unas 16 cifras: la mediana de un
# numero par de valores salia a un ulp de `median()`. Estos pares son de los que
# lo mostraban.

# Fuera de CRAN: necesita PostgreSQL y perfila varias veces.
testthat::skip_on_cran()

.mfle_pares <- function() {
  pares <- list(
    c("-0x1.85e282899ed1fp-2", "-0x1.8d25a85fbbd7cp-2"),
    c("0x1.80e91941c2a6p+1", "0x1.997ddee8d7acep-1"),
    c("-0x1.be5e23b068496p-1", "0x1.0164134e6f135p+0"),
    c("0x1.000006f4586c2p+0", "0x1.0000a6bc37c3bp+0"),
    c("0x1.0000a43bcdf07p+0", "0x1.00007a42d986ap+0")
  )
  datos <- as.data.frame(lapply(pares, as.numeric))
  names(datos) <- paste0("p", seq_along(pares))
  datos
}

.mfle_conectar <- function(puerto, efd = NULL) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  if (!is.null(efd)) {
    DBI::dbExecute(con, paste0("SET extra_float_digits = ", efd))
  }
  con
}

.mfle_comprobar <- function(con, metodo_esperado) {
  tabla <- "mfle_pares"
  datos <- .mfle_pares()
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbWriteTable(con, tabla, datos)
  on.exit(try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
              silent = TRUE), add = TRUE)
  # Control: lo que quedo en la tabla es exactamente lo escrito.
  leido <- DBI::dbGetQuery(con, paste0(
    "SELECT encode(float8send(p1), 'hex') AS h FROM ", tabla
  ))$h
  expect_identical(
    leido,
    vapply(datos$p1, function(v) {
      paste(rev(as.character(writeBin(v, raw()))), collapse = "")
    }, character(1L), USE.NAMES = FALSE)
  )
  caminos <- list(
    portable = list(dialecto = "portable"),
    muestra = list(universo = "muestra_motor", muestra_motor = 10L,
                   bloque_muestra = "solo_agregados")
  )
  for (camino in names(caminos)) {
    perfil <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(
      list(con, tabla, proteger_datos_personales = FALSE), caminos[[camino]]
    ))))
    sql <- perfil$resumen_tabla$sql
    columnas <- perfil$resumen_tabla$columnas
    for (columna in names(datos)) {
      fila <- sql[sql$metrica == "mediana" & sql$columna == columna, , drop = FALSE]
      expect_match(fila$metodo, metodo_esperado[[camino]], info = camino)
      expect_identical(
        as.numeric(columnas$mediana[columnas$columna == columna]),
        stats::median(datos[[columna]]), info = paste(camino, columna)
      )
    }
  }
}

test_that("PostgreSQL 9.3 promedia las dos centrales como median()", {
  con <- .mfle_conectar(55493L)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .mfle_comprobar(con, list(portable = "cte_ventana",
                            muestra = "subconsulta_escalar"))
})

test_that("con extra_float_digits = 0 la lectura exacta promedia igual", {
  con <- .mfle_conectar(as.integer(Sys.getenv("LUPA_PG_PORT", "55432")), efd = 0)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .mfle_comprobar(con, list(portable = "cte_ventana",
                            muestra = "subconsulta_escalar"))
})
