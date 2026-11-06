# RSQLite entrega `integer` a un bloque de valores chicos e `integer64` al que
# trae uno que no entra en 32 bits. El mapa de distintos de la via por bloques
# unia los dos con `c()`, que despacha por el primero y pegaba los bits del
# `integer64` como doble: `n_distintos`, la moda y la mediana salian mal y
# `calculado`, aunque ningun valor pasara de 2^53.

test_that("la via por bloques no mezcla un bloque integer con uno integer64", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("bit64")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE bcc (v INTEGER)")
  for (orden in list(c("1", "2", "3000000000", "3000000000"),
                     c("3000000000", "3000000000", "1", "2"))) {
    DBI::dbExecute(conexion, "DELETE FROM bcc")
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO bcc VALUES ", paste0("(", orden, ")", collapse = ", ")
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      conexion, "bcc", bloque_muestra = "solo_agregados", bloque_filas = 2L,
      proteger_datos_personales = FALSE
    )))
    fila <- perfil$resumen_tabla$columnas
    info <- paste(orden, collapse = ",")
    expect_equal(as.numeric(fila$n_distintos), 3, info = info)
    expect_identical(as.character(fila$moda), "3000000000", info = info)
    expect_equal(as.numeric(fila$frecuencia_moda), 2, info = info)
    expect_identical(as.numeric(fila$mediana), 1500000001, info = info)
    sql <- perfil$resumen_tabla$sql
    estados <- sql$estado[sql$metrica %in% c("n_distintos", "moda", "mediana")]
    expect_true(all(estados == "calculado"), info = info)
  }
})

test_that("unir enteros de 32 y de 64 bits conserva cada valor", {
  skip_if_not_installed("bit64")
  grande <- bit64::as.integer64("3000000000")
  unidos <- lupa:::.append_typed_R(1:2, grande)
  expect_s3_class(unidos, "integer64")
  expect_identical(as.character(unidos), c("1", "2", "3000000000"))
  unidos <- lupa:::.append_typed_R(grande, 7L)
  expect_identical(as.character(unidos), c("3000000000", "7"))
  # Un doble entero por debajo de 2^53 tiene `integer64` exacto; uno con
  # decimales no, y la union no se hace.
  expect_identical(
    as.character(lupa:::.append_typed_R(c(1, 2), grande)),
    c("1", "2", "3000000000")
  )
  expect_null(lupa:::.append_typed_R(c(1.5, 2), grande))
  # Sin `integer64` de por medio, lo de siempre.
  expect_identical(lupa:::.append_typed_R(1:2, 3L), 1:3)
})
