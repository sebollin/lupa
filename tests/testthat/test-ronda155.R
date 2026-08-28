# The key checks answer two different questions: uniqueness and missing values.
# These fixtures use only ASCII names and values so the tests do not depend on
# a locale or on a real organization or database.

.ronda155_argumentos <- function() {
  list(
    analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE,
    duplicados_aproximados = FALSE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

.ronda155_perfilar <- function(datos, clave) {
  do.call(
    lupa::perfilar,
    c(list(datos = datos, nombre = "tabla", clave = clave),
      .ronda155_argumentos())
  )
}

.ronda155_capturar_warning <- function(expr) {
  mensaje <- NULL
  valor <- withCallingHandlers(
    expr,
    warning = function(warning) {
      mensaje <<- conditionMessage(warning)
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, mensaje = mensaje)
}

test_that("a valid key keeps the complete historical object", {
  datos <- data.frame(
    id = seq_len(6L), value = letters[seq_len(6L)],
    stringsAsFactors = FALSE
  )

  # This is the pre-check path: it is used only as a complete-object reference.
  referencia <- testthat::with_mocked_bindings(
    .ronda155_perfilar(datos, "id"),
    .evaluar_clave_declarada = function(...) NULL,
    .package = "lupa"
  )
  actual <- .ronda155_perfilar(datos, "id")

  expect_identical(actual, referencia)
  expect_null(actual$meta$clave)
})

test_that("the five key cases publish separate states", {
  casos <- list(
    una_ausencia = list(
      datos = data.frame(k = c("a", "b", NA_character_, "d")),
      unicidad = "verificada", ausencia = "refutada",
      advertencia = "ausencia de nulos"
    ),
    dos_ausencias = list(
      datos = data.frame(
        k1 = c("a", "b", NA_character_, "d"),
        k2 = c(1L, NA_integer_, 3L, 4L)
      ),
      unicidad = "verificada", ausencia = "refutada",
      advertencia = "ausencia de nulos"
    ),
    duplicada_sin_ausencias = list(
      datos = data.frame(k = c("a", "b", "b", "d")),
      unicidad = "refutada", ausencia = "verificada",
      advertencia = "no es unica"
    ),
    duplicada_con_ausencias = list(
      datos = data.frame(k = c("a", NA_character_, NA_character_, "d")),
      unicidad = "refutada", ausencia = "refutada",
      advertencia = "colision"
    ),
    valida = list(
      datos = data.frame(k = c("a", "b", "c", "d")),
      unicidad = "verificada", ausencia = "verificada",
      advertencia = NULL
    )
  )

  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    capturado <- .ronda155_capturar_warning(
      .ronda155_perfilar(caso$datos, names(caso$datos))
    )
    perfil <- capturado$valor
    if (is.null(caso$advertencia)) {
      expect_null(capturado$mensaje, info = nombre)
    } else {
      expect_match(capturado$mensaje, caso$advertencia, info = nombre)
    }
    if (nombre == "valida") {
      next
    }
    expect_true(is.list(perfil$meta$clave), info = nombre)
    expect_identical(
      perfil$meta$clave$unicidad$estado, caso$unicidad, info = nombre
    )
    expect_identical(
      perfil$meta$clave$ausencia_nulos$estado, caso$ausencia, info = nombre
    )
    expect_identical(
      perfil$meta$clave$unicidad$semantica, "R", info = nombre
    )
    expect_identical(
      perfil$meta$clave$trazabilidad$semantica, "R", info = nombre
    )
  }
})

test_that("a duplicated key with missing values reports both axes", {
  datos <- data.frame(k = c("a", NA_character_, NA_character_, "d"))
  capturado <- .ronda155_capturar_warning(
    .ronda155_perfilar(datos, "k")
  )
  perfil <- capturado$valor
  expect_match(capturado$mensaje, "no es unica")
  expect_match(capturado$mensaje, "ausencia de nulos")
  expect_match(capturado$mensaje, "colision")
  meta <- perfil$meta$clave
  expect_equal(meta$unicidad$filas_repetidas, 1)
  expect_equal(meta$unicidad$filas_en_colision, 2)
  expect_equal(meta$ausencia_nulos$valores_ausentes, 2)
  expect_true(meta$trazabilidad$colisiona_con_ausentes)
  expect_equal(meta$trazabilidad$filas_colision_con_ausentes, 2)

  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "clave_no_unica", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 2:3)
})

test_that("a missing value is not blamed for another collision", {
  datos <- data.frame(k = c("a", "a", NA_character_, "d"))
  capturado <- .ronda155_capturar_warning(
    .ronda155_perfilar(datos, "k")
  )
  meta <- capturado$valor$meta$clave
  expect_false(meta$trazabilidad$colisiona_con_ausentes)
  expect_equal(meta$trazabilidad$filas_colision_con_ausentes, 0)
  expect_match(capturado$mensaje, "no es unica")
  expect_match(capturado$mensaje, "ausencia de nulos")
})

.ronda155_clave_simulada <- function(conexion, datos) {
  testthat::with_mocked_bindings(
    lupa:::.clave_primaria_dbi(conexion, "tabla"),
    .consultar_dbi = function(...) {
      list(ok = TRUE, datos = datos, motivo = NA_character_)
    },
    .package = "lupa"
  )
}

.ronda155_clave_no_consultable <- function(conexion) {
  testthat::with_mocked_bindings(
    lupa:::.clave_primaria_dbi(conexion, "tabla"),
    .consultar_dbi = function(...) {
      list(ok = FALSE, datos = NULL, motivo = "sin permisos")
    },
    .package = "lupa"
  )
}

test_that("Oracle reads status and validation before declaring a guarantee", {
  oracle <- structure(list(), class = "OraConnection")
  casos <- list(
    aplicada_validada = list(
      datos = data.frame(
        column_name = "id", ordinal_position = 1L,
        constraint_status = "ENABLED", constraint_validated = "VALIDATED"
      ),
      garantia = "garantizada"
    ),
    deshabilitada = list(
      datos = data.frame(
        column_name = "id", ordinal_position = 1L,
        constraint_status = "DISABLED", constraint_validated = "VALIDATED"
      ),
      garantia = "declarada_no_garantizada"
    ),
    no_validada = list(
      datos = data.frame(
        column_name = "id", ordinal_position = 1L,
        constraint_status = "ENABLED", constraint_validated = "NOT VALIDATED"
      ),
      garantia = "declarada_no_garantizada"
    ),
    estado_desconocido = list(
      datos = data.frame(column_name = "id", ordinal_position = 1L),
      garantia = "desconocida"
    )
  )

  for (nombre in names(casos)) {
    resultado <- .ronda155_clave_simulada(oracle, casos[[nombre]]$datos)
    expect_equal(resultado$columnas, "id", info = nombre)
    expect_identical(resultado$fuente, "all_constraints", info = nombre)
    expect_identical(resultado$garantia, casos[[nombre]]$garantia, info = nombre)
  }
})

test_that("catalogue SQL exposes only the states each motor has", {
  consultas <- lupa:::.consultas_clave_primaria()
  estandar <- consultas[[1L]]$sql
  oracle <- consultas[[4L]]$sql
  pragma <- consultas[[2L]]$sql
  pg_catalogo <- consultas[[5L]]$sql

  mysql <- estandar(NA_character_, "tabla", motor = "mysql")
  mariadb <- estandar(NA_character_, "tabla", motor = "mariadb")
  sqlserver <- estandar(NA_character_, "tabla", motor = "sqlserver")
  postgres <- pg_catalogo(NA_character_, "tabla")
  expect_match(postgres, "pg_catalog.pg_constraint", fixed = TRUE)
  expect_match(postgres, "pg_catalog.pg_class", fixed = TRUE)
  expect_match(postgres, "pg_catalog.pg_namespace", fixed = TRUE)
  expect_match(postgres, "convalidated", ignore.case = TRUE)
  expect_false(grepl("information_schema", postgres, fixed = TRUE))
  expect_match(mysql, "enforced", ignore.case = TRUE)
  expect_false(grepl("enforced", mariadb, ignore.case = TRUE))
  expect_false(grepl("enforced", sqlserver, ignore.case = TRUE))
  expect_match(oracle(NA_character_, "tabla"), "status", ignore.case = TRUE)
  expect_match(oracle(NA_character_, "tabla"), "validated", ignore.case = TRUE)
  expect_match(pragma(NA_character_, "tabla"), "notnull", ignore.case = TRUE)
})

test_that("engines without a state do not inherit a false guarantee", {
  datos <- data.frame(column_name = "id", ordinal_position = 1L)
  motores <- list(
    mariadb = structure(list(), class = "MariaDBConnection"),
    sqlserver = structure(list(), class = "SQLServerConnection"),
    desconocido = structure(list(), class = "MotorInventado")
  )
  for (nombre in names(motores)) {
    resultado <- .ronda155_clave_simulada(motores[[nombre]], datos)
    expect_identical(resultado$garantia, "desconocida", info = nombre)
    expect_true(is.na(resultado$estado$aplicada), info = nombre)
    expect_true(is.na(resultado$estado$validada), info = nombre)
  }
})

test_that("PostgreSQL and MySQL use the states they expose", {
  postgres <- structure(list(), class = "PqConnection")
  pg_data <- data.frame(
    column_name = "id", ordinal_position = 1L,
    constraint_enforced = TRUE, constraint_validated = TRUE
  )
  pg <- .ronda155_clave_simulada(postgres, pg_data)
  expect_identical(pg$fuente, "pg_catalog")
  expect_identical(pg$garantia, "garantizada")
  expect_true(pg$estado$aplicada)
  expect_true(pg$estado$validada)

  mysql <- structure(list(), class = "MySQLConnection")
  mysql_data <- data.frame(
    column_name = "id", ordinal_position = 1L,
    constraint_enforced = "YES"
  )
  resultado <- .ronda155_clave_simulada(mysql, mysql_data)
  expect_identical(resultado$fuente, "information_schema")
  expect_identical(resultado$garantia, "garantizada")
  expect_true(resultado$estado$aplicada)
  expect_true(is.na(resultado$estado$validada))
})

test_that("an unconsultable catalogue keeps state unknown", {
  oracle <- structure(list(), class = "OraConnection")
  resultado <- .ronda155_clave_no_consultable(oracle)
  expect_identical(resultado$fuente, "all_constraints")
  expect_identical(resultado$garantia, "desconocida")
  expect_true(is.na(resultado$estado$visible))
  expect_true(is.na(resultado$estado$aplicada))
  expect_true(is.na(resultado$estado$validada))
  expect_match(resultado$motivo, "sin permisos")
})
