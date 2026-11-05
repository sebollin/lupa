testthat::test_that("n_filas nombra las tablas de una coleccion", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3))
  DBI::dbWriteTable(con, "hogares", data.frame(id = 1:2))

  perfil <- lupa::perfilar_coleccion(
    lupa::coleccion(con, c("personas", "hogares"))
  )
  salida <- lupa::n_filas(perfil)

  testthat::expect_named(salida, c("personas", "hogares"))
  testthat::expect_equal(unname(salida), c(3, 2))
})
