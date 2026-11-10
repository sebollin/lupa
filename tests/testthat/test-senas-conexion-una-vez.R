# `perfilar_dbi()` reconoce el motor por las senas de la conexion, y las
# consultaba al armar cada sentencia: 184 llamadas a `DBI::dbGetInfo()` sobre
# una tabla de tres columnas. En la mayoria de los controladores es gratis, pero
# en duckdb 1.1 cada una manda `pragma_version()` al motor: 176 sentencias que
# el plan no anunciaba y `max_consultas` no contaba, y la prueba del plan fallaba
# en el minimo declarado de R. Las senas se leen una vez por corrida y el estado
# se limpia al salir, tambien cuando la corrida termina en error.
testthat::skip_on_cran()

.scuv_contar <- function(codigo) {
  estado <- new.env()
  estado$n <- 0L
  suppressMessages(trace(
    "dbGetInfo", where = asNamespace("DBI"), print = FALSE,
    tracer = bquote(assign("n", get("n", envir = .(estado)) + 1L,
                           envir = .(estado)))
  ))
  on.exit(suppressMessages(untrace("dbGetInfo", where = asNamespace("DBI"))),
          add = TRUE)
  force(codigo)
  estado$n
}

.scuv_tabla <- data.frame(
  id = 1:60, t = paste0("v", 1:60 %% 7), x = (1:60) * 0.25
)

test_that("las senas de la conexion se leen una vez por perfil y por plan", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", .scuv_tabla)
  expect_identical(.scuv_contar(suppressWarnings(suppressMessages(
    perfilar_dbi(con, "t", proteger_datos_personales = FALSE)
  ))), 1L)
  expect_identical(.scuv_contar(suppressWarnings(suppressMessages(
    plan_perfilado_dbi(con, "t")
  ))), 1L)
  expect_null(lupa:::.CONTEXTO_SENAS$conexion)
})

test_that("en DuckDB tambien, y el estado no queda tras un error", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", .scuv_tabla)
  expect_identical(.scuv_contar(suppressWarnings(suppressMessages(
    perfilar_dbi(con, "t", proteger_datos_personales = FALSE)
  ))), 1L)
  expect_error(suppressWarnings(suppressMessages(
    perfilar_dbi(con, "no_existe", proteger_datos_personales = FALSE)
  )))
  expect_null(lupa:::.CONTEXTO_SENAS$conexion)
})
