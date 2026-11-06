# El catalogo de SQL Server publica la escala de un decimal.
#
# `TYPE_NAME()` da `decimal` a secas, y sin la escala la guarda del desvio de un
# decimal leido como doble (`.desvio_mal_condicionado_dbi()`) no sabia que un
# `decimal(18,0)` guarda enteros: por bloques y por la muestra del motor negaba
# el desvio de 5e14 + k, cuyos dobles son los valores. PostgreSQL, MySQL y
# MariaDB ya lo publicaban. Un decimal con escala sigue negado: ahi los dobles
# no son los valores.
testthat::skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("odbc")

.esq_conectar <- function() {
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
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

test_that("SQL Server publica decimal(p,s) y la guarda deja pasar la escala 0", {
  con <- .esq_conectar()
  tabla <- "lupa_esq_decimal"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla,
    " (entero decimal(18,0), ancho numeric(38,0), centavos decimal(18,2), m money)"
  ))
  k <- c(0, 3, 7, 1, 9, 4, 2, 8, 5)
  filas <- vapply(k, function(x) {
    g <- sprintf("%.0f", 5e14 + x)
    paste0("(", g, ", ", g, ", ", g, ".25, ", sprintf("%.0f", 5e13 + x), ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
  permisos <- lupa:::.permisos_columnas_dbi(
    con, as.character(DBI::dbQuoteIdentifier(con, tabla))
  )
  expect_identical(
    permisos$tipo_motor,
    c("decimal(18,0)", "numeric(38,0)", "decimal(18,2)", "money")
  )
  vias <- list(
    bloques = list(bloque_filas = 2L, bloque_muestra = "solo_agregados"),
    muestra = list(universo = "muestra_motor", muestra_motor = 9L,
                   bloque_muestra = "solo_agregados")
  )
  for (via in names(vias)) {
    perfil <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(
      list(con, tabla, proteger_datos_personales = FALSE), vias[[via]]
    ))))
    sql <- perfil$resumen_tabla$sql
    columnas <- perfil$resumen_tabla$columnas
    for (columna in c("entero", "ancho")) {
      estado <- sql$estado[sql$columna == columna & sql$metrica == "desvio"]
      expect_true(utils::tail(estado, 1L) %in% c("calculado", "estimado"),
                  info = paste(via, columna))
      valor <- columnas$desvio[columnas$columna == columna]
      expect_lt(abs(valor - stats::sd(k)) / stats::sd(k), 1e-12)
    }
    for (columna in c("centavos", "m")) {
      estado <- sql$estado[sql$columna == columna & sql$metrica == "desvio"]
      expect_identical(utils::tail(estado, 1L), "no_disponible",
                       info = paste(via, columna))
    }
  }
})
