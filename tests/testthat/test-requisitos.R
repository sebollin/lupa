test_that("el catalogo conserva los motores y estados de los README", {
  catalogo <- lupa:::requisitos_motor()

  expect_s3_class(catalogo, "requisitos_motor")
  expect_s3_class(catalogo, "data.frame")
  expect_true(all(c(
    "PostgreSQL", "MySQL", "SQL Server", "SQLite", "DuckDB", "MariaDB",
    "Oracle"
  ) %in% catalogo$motor))
  expect_equal(
    catalogo$dialecto[catalogo$motor == "PostgreSQL" & catalogo$version == "16"],
    "limit"
  )
  expect_equal(
    catalogo$estado_prueba[catalogo$motor == "PostgreSQL" & catalogo$version == "16"],
    "probado"
  )
  # MariaDB 11 se verifico contra motor real. Oracle no: sus dos variantes estan
  # en `esperado` porque no se conserva ningun log de una corrida contra un
  # servidor Oracle, y el unico informe que existe declara que no pudo medir.
  expect_equal(
    catalogo$estado_prueba[catalogo$motor == "MariaDB"],
    "probado"
  )
  expect_equal(
    catalogo$estado_prueba[catalogo$motor == "Oracle"],
    c("esperado", "esperado")
  )
  expect_equal(sum(catalogo$estado_biblioteca_sistema == "no_requerida"), 3L)
  expect_true(all(catalogo$estado_biblioteca_sistema %in% c(
    "no_requerida", "no_comprobada"
  )))
  expect_true(all(is.logical(catalogo$paquete_r_instalado)))
  expect_true(is.na(catalogo$paquete_r_instalado[catalogo$id_motor == "otro_dbi"]))
  expect_true(all(catalogo$diagnostico_biblioteca_sistema != ""))
})

test_that("el filtro de Oracle devuelve las dos variantes y su resolucion local", {
  oracle <- lupa:::requisitos_motor("oracle")

  expect_equal(nrow(oracle), 2L)
  expect_setequal(oracle$version, c("Free 23 (12c+)", "11 y anteriores"))
  expect_true(all(oracle$paquete_r == "ROracle"))
  expect_true(all(grepl("OCI_LIB", oracle$alternativa_sin_administrador, fixed = TRUE)))
  expect_true(all(grepl("Instant Client", oracle$biblioteca_sistema, fixed = TRUE)))
  salida <- capture.output(lupa:::print.requisitos_motor(oracle), type = "message")
  expect_true(any(grepl("Oracle", salida, fixed = TRUE)))
  expect_true(any(grepl("ROracle", salida, fixed = TRUE)))
})

test_that("la ausencia de un paquete de motor nombra el motor y su instalacion", {
  local_mocked_bindings(
    .hay_paquete = function(nombre) {
      if (identical(nombre, "ROracle")) return(FALSE)
      isTRUE(base::requireNamespace(nombre, quietly = TRUE))
    },
    .package = "lupa"
  )

  error <- tryCatch(
    lupa:::.requerir_paquete_motor("oracle", accion = "abrir una conexion"),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_requisito_motor")
  expect_match(conditionMessage(error), "Oracle", fixed = TRUE)
  expect_match(conditionMessage(error), "ROracle", fixed = TRUE)
  expect_match(conditionMessage(error), "install.packages", fixed = TRUE)
  expect_match(conditionMessage(error), "OCI_LIB", fixed = TRUE)
})

test_that("la ausencia de DBI explica como instalarlo", {
  local_mocked_bindings(
    .hay_paquete = function(nombre) FALSE,
    .package = "lupa"
  )

  error <- tryCatch(
    lupa:::.requerir_dbi_accionable(),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_requisito_dbi")
  expect_match(conditionMessage(error), "DBI", fixed = TRUE)
  expect_match(conditionMessage(error), "install.packages", fixed = TRUE)
})

test_that("un fallo de biblioteca del controlador queda traducido sin afirmar que se comprobo", {
  original <- simpleError(
    "Error: cannot load shared object 'libmariadb.so': No such file or directory"
  )
  error <- tryCatch(
    lupa:::.detener_error_conexion_dbi(
      original, motor = "mariadb", accion = "abrir una conexion"
    ),
    error = function(e) e
  )

  expect_s3_class(error, "lupa_error_conexion_dbi")
  expect_s3_class(error, "lupa_error_dbi")
  expect_match(conditionMessage(error), "biblioteca externa", fixed = TRUE)
  expect_match(conditionMessage(error), "No se comprobo", fixed = TRUE)
  expect_match(conditionMessage(error), "libmariadb.so", fixed = TRUE)
  expect_match(conditionMessage(error), "libmariadb-dev", fixed = TRUE)
})
