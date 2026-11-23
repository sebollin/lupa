# La clave de cada lectura del motor es la tabla que el motor resuelve -su
# esquema, explicito aunque se omita, y su nombre como lo guarda el catalogo-,
# no el rotulo con que se la nombra. `Sucursales` y `sucursales`, o
# `DBI::Id(schema = "main", table = "sucursales")` y el nombre pelado, son la
# misma tabla: sus lecturas se cruzan, y si cambio entre ellas la cifra no
# cierra. Antes cada rotulo era una tabla: la metrica directa decia
# `una_lectura` y se pintaba con el valor viejo.

.ident_con <- function(motor) {
  con <- if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  DBI::dbWriteTable(con, "sucursales", data.frame(
    id_sucursal = 1:4, zona = c("Norte", "Sur", NA, "Oeste")
  ))
  DBI::dbWriteTable(con, "ventas", data.frame(
    id_venta = 1:6, id_sucursal = c(1, 2, 3, 9, NA, 1),
    monto = c(1, NA, 3, 4, 5, 6)
  ))
  con
}

.ident_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.ident_fuerte <- function(tabla) {
  padron <- referencial(tabla, clave = "id_sucursal", valor = "zona",
                        completo = TRUE, alcance = "ejemplo")
  instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "Existe"),
             "ventas", "id_sucursal", referencial = padron)
}

.ident_directa <- function(entidad) {
  instanciar(especializar(metricas_nucleo()$NoNulo, "Zona"), entidad, "zona")
}

.ident_cortes <- list(Completitud = c(0.5, 0.9), Exactitud = c(0.5, 0.9))

.ident_motores <- function() {
  c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")
}

test_that("el motor resuelve el esquema y el nombre de la tabla de cada lectura", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .ident_motores()) {
    con <- .ident_con(motor)
    esperado <- if (identical(motor, "duckdb")) "memory.main.sucursales" else
      "`main`.`sucursales`"
    for (tabla in list("sucursales", "Sucursales", DBI::Id(table = "SUCURSALES"),
                       DBI::Id(schema = "main", table = "sucursales"))) {
      esq <- .esquema_medicion_dbi(con, .nombre_para_conexion_dbi(tabla), motor)
      expect_identical(esq$identidad, esperado, label = paste(motor, format(tabla)))
    }
    .ident_cerrar(con, motor)
  }
})

test_that("otro rotulo de la misma tabla cruza sus lecturas en la misma corrida", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .ident_motores()) {
    rotulos <- list(
      list(entidad = "Sucursales", tabla = DBI::Id(table = "sucursales")),
      list(entidad = "sucursales", tabla = DBI::Id(schema = "main", table = "sucursales"))
    )
    for (r in rotulos) {
      etiqueta <- paste(motor, r$entidad, format(r$tabla))
      con <- .ident_con(motor)
      m <- suppressWarnings(medir_dbi(
        modelo(.ident_fuerte(r$tabla), .ident_directa(r$entidad)), con,
        id_medicion = "j", fecha = "2026-06-30"
      ))
      .ident_cerrar(con, motor)
      l <- attr(m, "lecturas", exact = TRUE)
      expect_identical(unique(l$tabla[l$n_filas == 4]),
                       if (identical(motor, "duckdb")) "memory.main.sucursales" else
                         "`main`.`sucursales`", label = etiqueta)
      # La tabla referida toma el nombre de la entidad que es la misma tabla.
      expect_identical(m$entidad_referida[m$entidad == "ventas"], r$entidad,
                       label = etiqueta)
      s <- semaforo_calidad(m, cortes = .ident_cortes)
      directa <- s$entidad == r$entidad
      expect_identical(s$control_lecturas[directa], "limitado", label = etiqueta)
    }
  }
})

test_that("si la tabla cambia entre dos llamadas con otro rotulo, la cifra no cierra", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in .ident_motores()) {
    con <- .ident_con(motor)
    directa <- medir_dbi(modelo(.ident_directa("Sucursales")), con,
                         id_medicion = "j", fecha = "2026-06-30")
    DBI::dbExecute(con, "INSERT INTO sucursales VALUES (5, NULL)")
    referida <- suppressWarnings(medir_dbi(
      modelo(.ident_fuerte(DBI::Id(table = "sucursales"))), con,
      id_medicion = "j", fecha = "2026-06-30"
    ))
    .ident_cerrar(con, motor)
    u <- suppressWarnings(rbind(directa = directa, referida = referida))
    expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), label = motor)
    expect_identical(u$motivo_cuentas,
                     rep("tabla_cambio_entre_lecturas", 2L), label = motor)
    s <- semaforo_calidad(u, cortes = .ident_cortes)
    expect_true(all(is.na(s$nivel)), label = motor)
  }
})

test_that("un referencial con esquema en SQLite se mide como el nombre pelado", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .ident_con("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  con_esquema <- medir_dbi(
    modelo(.ident_fuerte(DBI::Id(schema = "main", table = "sucursales"))), con,
    id_medicion = "j", fecha = "2026-06-30"
  )
  pelado <- medir_dbi(
    modelo(.ident_fuerte(DBI::Id(table = "sucursales"))), con,
    id_medicion = "j", fecha = "2026-06-30"
  )
  expect_null(attr(con_esquema, "cobertura_metricas", exact = TRUE))
  expect_identical(con_esquema$resultado, pelado$resultado)
  expect_identical(con_esquema$numerador, pelado$numerador)
})

test_that("la misma tabla en otro esquema es otra tabla, y no se cruza", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  con <- .ident_con("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA otro")
  DBI::dbExecute(con, paste(
    "CREATE TABLE otro.sucursales AS SELECT * FROM main.sucursales",
    "UNION ALL SELECT 5, 'Centro'"
  ))
  m <- suppressWarnings(medir_dbi(
    modelo(.ident_fuerte(DBI::Id(schema = "otro", table = "sucursales")),
           .ident_directa("sucursales")),
    con, id_medicion = "j", fecha = "2026-06-30"
  ))
  l <- attr(m, "lecturas", exact = TRUE)
  expect_setequal(unique(l$tabla[l$entidad != "ventas"]),
                  c("memory.main.sucursales", "memory.otro.sucursales"))
  # Cinco filas en una y cuatro en la otra: no es un cambio entre lecturas.
  expect_identical(m$estado_cuentas, c("cierran", "cierran"))
  expect_false(identical(m$entidad_referida[m$entidad == "ventas"], "sucursales"))
})
