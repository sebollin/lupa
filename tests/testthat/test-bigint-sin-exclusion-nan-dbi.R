testthat::skip_on_cran()

# Una columna entera no guarda `NaN` y no recibe la exclusion
# `NULLIF(x, CAST('NaN' AS DOUBLE PRECISION))`: PostgreSQL resuelve esa
# expresion con el tipo comun de los dos argumentos, que sobre un `BIGINT` es
# doble. Con la exclusion, cuatro enteros distintos desde 2^53 se contaban como
# uno y el minimo volvia redondeado a un valor que no esta en la columna; en
# PostgreSQL 9.3, que escribe el doble con quince cifras, tambien un `BIGINT`
# desde 10^15. Todo publicado como `calculado`.

test_that("la exclusion de NaN decide por el tipo del motor, no por la clase de R", {
  skip_if_not_installed("bit64")
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "pqconnection postgresql",
    .package = "lupa"
  )
  conexion <- structure(list(), class = "PqConnection")
  # Lo que entrega RPostgres: `dbColumnInfo()$type` es `double` para `int8`,
  # `float8`, `float4` y `numeric` con cualquier valor de `bigint` (`numeric` y
  # `money` ya llegan corregidos con el nombre del motor), y `.typname` es el
  # tipo del motor.
  campos <- c("b", "f8", "f4", "n", "m", "i")
  declarados <- c(
    b = "double", f8 = "double", f4 = "double", n = "numeric",
    m = "money", i = "integer"
  )
  motor <- c(
    b = "int8", f8 = "float8", f4 = "float4", n = "numeric",
    m = "money", i = "int4"
  )
  prototipos_int8 <- list(
    integer64 = bit64::integer64(), integer = integer(), numeric = numeric(),
    character = character()
  )
  for (bigint in names(prototipos_int8)) {
    prototipo <- list(
      b = prototipos_int8[[bigint]], f8 = numeric(), f4 = numeric(),
      n = numeric(), m = numeric(), i = integer()
    )
    expect_identical(
      lupa:::.columnas_candidatas_nan_dbi(
        conexion, campos, rep(TRUE, length(campos)), declarados, prototipo,
        motor
      ),
      c("f8", "f4", "n"),
      info = bigint
    )
  }
})

test_that("sin tipo, un prototipo integer64 no recibe la exclusion de NaN", {
  skip_if_not_installed("bit64")
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "pqconnection postgresql",
    .package = "lupa"
  )
  conexion <- structure(list(), class = "PqConnection")
  # Sin ningun tipo decide el prototipo: un `integer64` se guarda en un doble
  # -`is.double()` dice que si-, pero es un entero.
  expect_identical(
    lupa:::.columnas_candidatas_nan_dbi(
      conexion, c("b", "d"), c(TRUE, TRUE), c(b = NA, d = NA),
      list(b = bit64::integer64(), d = numeric())
    ),
    "d"
  )
})

test_that("un BIGINT de PostgreSQL desde 2^53 conserva sus distintos y declara lo que un doble no representa", {
  skip_if_not_installed("bit64")
  disponible <- FALSE
  for (puerto in c(55432L, 55493L)) {
    con <- .nf_pg(puerto)
    if (is.null(con)) next
    disponible <- TRUE
    tabla <- paste0("nf_bigint_nan_pg_", puerto)
    local({
      on.exit(.nf_drop(con, tabla), add = TRUE)
      .nf_drop(con, tabla)
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla,
        " (id BIGINT, ts BIGINT, x DOUBLE PRECISION)"
      ))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla, " VALUES ",
        "(1234567890123456789, 1700000000000001, 'NaN'::double precision), ",
        "(1234567890123456790, 1700000000000003, 1.5), ",
        "(1234567890123456791, 1700000000000007, 2.5), ",
        "(1234567890123456792, 1700000000000009, NULL)"
      ))
      for (opcion in list(
        list(), list(muestra = 2), list(bloque_muestra = "solo_agregados")
      )) {
        info <- paste(puerto, paste(names(opcion), collapse = ""))
        perfil <- do.call(.nf_dbi, c(list(con, tabla), opcion))
        sql <- perfil$resumen_tabla$sql
        estado <- function(campo, metrica) {
          as.character(unique(
            sql$estado[sql$columna == campo & sql$metrica == metrica]
          ))
        }
        id <- .nf_fila(perfil, "id")
        ts <- .nf_fila(perfil, "ts")
        x <- .nf_fila(perfil, "x")

        expect_identical(as.numeric(id$n_distintos), 4, info = info)
        expect_identical(estado("id", "n_distintos"), "calculado", info = info)
        # Cuatro valores con frecuencia 1: no hay moda que publicar.
        expect_true(is.na(id$moda), info = info)
        # Un doble no representa estos enteros: se declara, no se redondea.
        for (metrica in c("minimo", "maximo")) {
          expect_true(is.na(id[[metrica]]), info = paste(info, metrica))
          expect_identical(
            estado("id", metrica), "no_disponible", info = paste(info, metrica)
          )
        }
        # Desde 10^15 el valor es exacto en un doble y se publica exacto.
        expect_identical(as.numeric(ts$minimo), 1700000000000001, info = info)
        expect_identical(as.numeric(ts$maximo), 1700000000000009, info = info)
        expect_identical(as.numeric(ts$media), 1700000000000005, info = info)
        expect_identical(estado("ts", "minimo"), "calculado", info = info)
        # La columna doble sigue contando su `NaN` como faltante.
        expect_identical(as.numeric(x$n_faltantes), 2, info = info)
        expect_identical(as.numeric(x$n_distintos), 2, info = info)
      }
    })
    DBI::dbDisconnect(con)
  }
  if (!disponible) {
    skip("PostgreSQL no disponible en 55432 ni 55493.")
  }
})
