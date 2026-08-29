# Corridas contra los dos servidores locales. Se omiten fuera del entorno que
# los tiene disponibles; la logica que no necesita servidor esta en el archivo
# de dobles de arriba.

.conexion_pg_estimacion <- function(port, user = "postgres",
                                    password = Sys.getenv(
                                      "LUPA_PG_PASSWORD", "lupa"
                                    )) {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  conexion <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = port,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), user = user,
      password = password
    ),
    error = function(e) NULL
  )
  if (is.null(conexion)) {
    testthat::skip(paste("No se pudo conectar a PostgreSQL en", port))
  }
  conexion
}

.nombre_tabla_estimacion <- function(prefijo) {
  paste0("public.", prefijo, "_", Sys.getpid())
}

test_that("una columna sin privilegio no produce un no-derrame", {
  admin <- .conexion_pg_estimacion(55432)
  on.exit(DBI::dbDisconnect(admin), add = TRUE)
  tabla <- .nombre_tabla_estimacion("lupa_permiso_derrame")
  nombre <- sub("^public\\.", "", tabla)
  DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla))
  on.exit(try(DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla)),
             silent = TRUE), add = TRUE)
  DBI::dbExecute(admin, paste0(
    "CREATE TABLE ", tabla, " (visible integer, oculto text)"
  ))
  DBI::dbExecute(admin, paste0(
    "INSERT INTO ", tabla, " VALUES (1, 'a'), (2, 'b')"
  ))
  DBI::dbExecute(admin, paste0("ANALYZE ", tabla))
  rol <- "lupa_sin_privilegios"
  DBI::dbExecute(admin, paste0("REVOKE ALL ON ", tabla, " FROM ", rol))
  DBI::dbExecute(admin, paste0("GRANT USAGE ON SCHEMA public TO ", rol))
  DBI::dbExecute(admin, paste0(
    "GRANT SELECT (visible) ON ", tabla, " TO ", rol
  ))
  restringida <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), user = rol,
      password = Sys.getenv("LUPA_RESTRINGIDO_PASSWORD", "lupa")
    ),
    error = function(e) NULL
  )
  if (is.null(restringida)) testthat::skip(
    "No se pudo conectar con lupa_sin_privilegios"
  )
  on.exit(DBI::dbDisconnect(restringida), add = TRUE)

  estimacion <- lupa:::.estimar_derrame_postgresql_dbi(
    restringida, tabla, "oculto", lupa:::.presupuesto_dbi(Inf),
    TRUE, "exacto", 1L
  )

  expect_identical(estimacion$estado, "no_disponible")
  expect_true(grepl("pg_stats", estimacion$motivo, fixed = TRUE))
  expect_false(grepl("no derrama", estimacion$motivo, fixed = TRUE))
  expect_false(isTRUE(estimacion$supera_memoria))
  expect_true(nchar(nombre) > 0L)
})

test_that("una tabla sin ANALYZE queda sin estimacion", {
  admin <- .conexion_pg_estimacion(55432)
  on.exit(DBI::dbDisconnect(admin), add = TRUE)
  tabla <- .nombre_tabla_estimacion("lupa_sin_analyze_derrame")
  DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla))
  on.exit(try(DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla)),
             silent = TRUE), add = TRUE)
  DBI::dbExecute(admin, paste0("CREATE TABLE ", tabla, " (x text)"))
  DBI::dbExecute(admin, paste0(
    "INSERT INTO ", tabla, " SELECT g::text FROM generate_series(1, 20) g"
  ))

  estimacion <- lupa:::.estimar_derrame_postgresql_dbi(
    admin, tabla, "x", lupa:::.presupuesto_dbi(Inf), TRUE, "exacto", 1L
  )

  expect_identical(estimacion$estado, "no_disponible")
  expect_match(estimacion$motivo, "No se pudo estimar")
  expect_match(estimacion$motivo, "ANALYZE")
  expect_false(grepl("no derrama", estimacion$motivo, fixed = TRUE))
})

test_that("PostgreSQL 16 recoge estadisticas de las particiones hijas", {
  admin <- .conexion_pg_estimacion(55432)
  on.exit(DBI::dbDisconnect(admin), add = TRUE)
  tabla <- .nombre_tabla_estimacion("lupa_particion_derrame")
  DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla, " CASCADE"))
  on.exit(try(DBI::dbExecute(admin, paste0(
    "DROP TABLE IF EXISTS ", tabla, " CASCADE"
  )), silent = TRUE), add = TRUE)
  DBI::dbExecute(admin, paste0(
    "CREATE TABLE ", tabla,
    " (x integer, etiqueta text) PARTITION BY RANGE (x)"
  ))
  DBI::dbExecute(admin, paste0(
    "CREATE TABLE ", tabla, "_a PARTITION OF ", tabla,
    " FOR VALUES FROM (1) TO (101)"
  ))
  DBI::dbExecute(admin, paste0(
    "CREATE TABLE ", tabla, "_b PARTITION OF ", tabla,
    " FOR VALUES FROM (101) TO (201)"
  ))
  DBI::dbExecute(admin, paste0(
    "INSERT INTO ", tabla, " SELECT g, g::text FROM generate_series(1, 200) g"
  ))
  DBI::dbExecute(admin, paste0("ANALYZE ", tabla, "_a"))
  DBI::dbExecute(admin, paste0("ANALYZE ", tabla, "_b"))

  estimacion <- lupa:::.estimar_derrame_postgresql_dbi(
    admin, tabla, "x", lupa:::.presupuesto_dbi(Inf), TRUE, "exacto", 1L
  )

  expect_identical(estimacion$estado, "estimado")
  expect_identical(estimacion$columnas$n_relaciones[[1L]], 2L)
  expect_identical(estimacion$columnas$n_distintos_estimados[[1L]], 200)
  expect_match(estimacion$fuente, "pg_stats")
})

test_that("PostgreSQL 9.3 no necesita hash_mem_multiplier", {
  admin <- .conexion_pg_estimacion(55493)
  on.exit(DBI::dbDisconnect(admin), add = TRUE)
  tabla <- .nombre_tabla_estimacion("lupa_pg93_derrame")
  DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla))
  on.exit(try(DBI::dbExecute(admin, paste0("DROP TABLE IF EXISTS ", tabla)),
             silent = TRUE), add = TRUE)
  DBI::dbExecute(admin, paste0("CREATE TABLE ", tabla, " (x integer)"))
  DBI::dbExecute(admin, paste0(
    "INSERT INTO ", tabla, " SELECT g FROM generate_series(1, 1000) g"
  ))
  DBI::dbExecute(admin, paste0("ANALYZE ", tabla))

  estimacion <- lupa:::.estimar_derrame_postgresql_dbi(
    admin, tabla, "x", lupa:::.presupuesto_dbi(Inf), TRUE, "exacto", 1L
  )

  expect_identical(estimacion$estado, "estimado")
  expect_false(estimacion$hash_mem_multiplier_disponible)
  expect_identical(estimacion$hash_mem_multiplier, 1)
  expect_identical(estimacion$memoria_efectiva_bytes, estimacion$work_mem_bytes)
  expect_false(grepl("hash_mem_multiplier", estimacion$fuente, fixed = TRUE))
})
