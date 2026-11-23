# Las metricas contra un referencial del motor leen dos tablas -la entidad y la
# del referencial- y registran esas lecturas como las demas vias de
# `medir_dbi()`: en `attr(, "lecturas")`, con `entidad_referida` y
# `n_filas_leidas_referida` en el resumen. Sin ellas `rbind(prioridad = )`
# rechazaba la parte ("no registra sus lecturas"), y una tabla leida en dos
# sentencias -la consolidada y la del referencial- se publicaba como
# `una_lectura`.

.lect_ref_con <- function(motor = "sqlite") {
  con <- if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  DBI::dbWriteTable(con, "sucursales", data.frame(
    id_sucursal = 1:4, zona = c("Norte", "Sur", "Este", "Oeste")
  ))
  DBI::dbWriteTable(con, "ventas", data.frame(
    id_venta = 1:6, id_sucursal = c(1, 2, 3, 9, NA, 1),
    zona = c("Norte", "Sur", "Este", "Norte", "Sur", "Norte"),
    monto = c(1, NA, 3, 4, 5, 6)
  ))
  con
}

.lect_ref_metricas <- function() {
  nucleo <- metricas_nucleo()
  ref <- metricas_referencial()
  padron <- referencial(DBI::Id(table = "sucursales"), clave = "id_sucursal",
                        valor = "zona", completo = TRUE, alcance = "ejemplo")
  list(
    fuerte = instanciar(especializar(ref$CorrectitudSemFuerte, "Existe"),
                        "ventas", "id_sucursal", referencial = padron),
    debil = instanciar(especializar(ref$CorrectitudSemDebil, "Coincide"),
                       "ventas", c("id_sucursal", "zona"), referencial = padron),
    cobertura = instanciar(especializar(ref$RatioCobertura, "Cubre"),
                           "ventas", "id_sucursal", referencial = padron),
    no_nulo = instanciar(especializar(nucleo$NoNulo, "Monto"), "ventas", "monto")
  )
}

.lect_ref_planilla <- function() {
  nucleo <- metricas_nucleo()
  medir(
    modelo(instanciar(especializar(nucleo$NoNulo, "Horario"), "planilla", "h")),
    list(planilla = data.frame(h = c("a", NA))),
    id_medicion = "junio", fecha = "2026-06-30", detalle = "resumen"
  )
}

test_that("cada metrica del referencial registra la lectura de sus dos tablas", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in c("sqlite", "duckdb")) {
    if (identical(motor, "duckdb")) skip_if_not_installed("duckdb")
    con <- .lect_ref_con(motor)
    metricas <- .lect_ref_metricas()
    for (nombre in c("fuerte", "debil", "cobertura")) {
      for (estrategia in c("no_medible", "bloques")) {
        etiqueta <- paste(motor, nombre, estrategia)
        m <- medir_dbi(modelo(metricas[[nombre]]), con, id_medicion = "junio",
                       fecha = "2026-06-30", estrategia_no_sql = estrategia)
        l <- attr(m, "lecturas", exact = TRUE)
        expect_setequal(unique(l$entidad), c("ventas", "sucursales"))
        expect_identical(unique(l$n_filas[l$entidad == "ventas"]), 6, label = etiqueta)
        expect_identical(unique(l$n_filas[l$entidad == "sucursales"]), 4,
                         label = etiqueta)
        expect_identical(m$entidad_referida, "sucursales", label = etiqueta)
        expect_identical(m$n_filas_leidas, 6, label = etiqueta)
        expect_identical(m$n_filas_leidas_referida, 4, label = etiqueta)
        unido <- tryCatch(
          rbind(motor = m, memoria = .lect_ref_planilla(),
                prioridad = c("motor", "memoria")),
          error = function(e) conditionMessage(e)
        )
        expect_s3_class(unido, "medicion_resumen")
      }
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("una tabla leida por la consolidada y por el referencial no es una_lectura", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .lect_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  metricas <- .lect_ref_metricas()
  m <- medir_dbi(modelo(metricas$no_nulo, metricas$fuerte), con,
                 id_medicion = "junio", fecha = "2026-06-30")
  l <- attr(m, "lecturas", exact = TRUE)
  expect_length(unique(l$lectura[l$entidad == "ventas"]), 2L)
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9),
                                         Exactitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, c("limitado", "limitado"))
})

test_that("el control: sin referencial, la tabla leida una vez sigue en una_lectura", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .lect_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- medir_dbi(modelo(.lect_ref_metricas()$no_nulo), con,
                 id_medicion = "junio", fecha = "2026-06-30")
  expect_identical(nrow(attr(m, "lecturas", exact = TRUE)), 1L)
  expect_true(is.na(m$entidad_referida))
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, "una_lectura")
})

test_that("si el referencial cambia entre sus lecturas, la cifra no cierra", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .lect_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  original <- .validar_referencial_dbi
  insertado <- FALSE
  local_mocked_bindings(.validar_referencial_dbi = function(...) {
    salida <- original(...)
    if (!insertado) {
      DBI::dbExecute(con, "INSERT INTO sucursales VALUES (5, 'Centro')")
      insertado <<- TRUE
    }
    salida
  })
  m <- suppressWarnings(medir_dbi(
    modelo(.lect_ref_metricas()$fuerte), con,
    id_medicion = "junio", fecha = "2026-06-30"
  ))
  expect_true(insertado)
  expect_identical(m$estado_cuentas, "no_cierran")
  expect_identical(m$motivo_cuentas, "tabla_cambio_entre_lecturas")
  expect_identical(m$diferencia_cuentas, 1)
})

test_that("la guarda del pliegue registra su lectura y la de los valores distintos", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .lect_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Un blanco al final: la guarda aparta la consulta del motor y, con
  # `bloques`, la comparacion se hace en R sobre los valores distintos.
  DBI::dbExecute(con, "UPDATE ventas SET zona = 'Sur ' WHERE id_venta = 2")
  m <- medir_dbi(modelo(.lect_ref_metricas()$debil), con, id_medicion = "junio",
                 fecha = "2026-06-30", estrategia_no_sql = "bloques")
  expect_identical(unname(attr(m, "ejecucion_dbi")$vias), "distintos_r")
  l <- attr(m, "lecturas", exact = TRUE)
  # La validacion, la consulta que encontro la guarda (las dos tablas), los
  # grupos, el conteo de la entidad y los valores del referencial.
  expect_identical(sum(l$entidad == "ventas"), 3L)
  expect_identical(sum(l$entidad == "sucursales"), 3L)
  expect_setequal(unique(l$n_filas), c(4, 6))
  expect_identical(m$entidad_referida, "sucursales")
  expect_identical(m$n_filas_leidas_referida, 4)
  expect_identical(m$estado_cuentas, "cierran")
})

# El modelo llama `sucursales` a la tabla `padron`; la base tiene ademas una
# tabla `sucursales`, que es otra, y `zonas`, que no es de ninguna entidad. Mide
# una referencial contra `tabla_referencial` y la entidad `sucursales`.
.lect_ref_referida <- function(motor, tabla_referencial) {
  con <- if (identical(motor, "duckdb")) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  on.exit(if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbWriteTable(con, "padron", data.frame(
    id_sucursal = 1:4, zona = c("Norte", "Sur", "Este", "Oeste")
  ))
  DBI::dbWriteTable(con, "sucursales", data.frame(
    id_sucursal = 1:2, zona = c("Norte", "Sur")
  ))
  DBI::dbWriteTable(con, "zonas", data.frame(
    id_sucursal = 1:3, zona = c("Norte", "Sur", "Este")
  ))
  DBI::dbWriteTable(con, "ventas", data.frame(
    id_venta = 1:6, id_sucursal = c(1, 2, 3, 9, NA, 1)
  ))
  padron <- referencial(DBI::Id(table = tabla_referencial), clave = "id_sucursal",
                        valor = "zona", completo = TRUE, alcance = "ejemplo")
  suppressWarnings(medir_dbi(
    modelo(
      instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "Existe"),
                 "ventas", "id_sucursal", referencial = padron),
      instanciar(especializar(metricas_nucleo()$NoNulo, "Zona"), "sucursales", "zona")
    ),
    con, tablas = list(ventas = "ventas", sucursales = "padron"),
    id_medicion = "junio", fecha = "2026-06-30"
  ))
}

.lect_ref_cortes <- list(Exactitud = c(0.5, 0.9), Completitud = c(0.5, 0.9))

test_that("la tabla referida toma el nombre de la entidad que es la misma tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  for (motor in c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")) {
    # La tabla de una entidad: el nombre de la entidad, y sus lecturas son
    # lecturas de la entidad (la consolidada y las del referencial).
    m <- .lect_ref_referida(motor, "padron")
    referencial <- m$entidad == "ventas"
    expect_identical(m$entidad_referida[referencial], "sucursales", label = motor)
    expect_identical(m$n_filas_leidas_referida[referencial], 4, label = motor)
    s <- semaforo_calidad(m, cortes = .lect_ref_cortes)
    expect_identical(s$control_lecturas[s$entidad == "sucursales"], "limitado",
                     label = motor)
    # El nombre de una entidad que es OTRA tabla: sus dos filas no se comparan
    # con las cuatro de la entidad, y la entidad sigue en una lectura.
    m <- .lect_ref_referida(motor, "sucursales")
    expect_identical(m$n_filas_leidas_referida[m$entidad == "ventas"], 2, label = motor)
    expect_identical(m$n_filas_leidas[m$entidad == "sucursales"], 4, label = motor)
    expect_identical(m$estado_cuentas, c("cierran", "cierran"), label = motor)
    # Ninguna de las dos: el texto de la tabla.
    m <- .lect_ref_referida(motor, "zonas")
    expect_identical(m$entidad_referida[m$entidad == "ventas"], "zonas", label = motor)
    expect_identical(m$n_filas_leidas_referida[m$entidad == "ventas"], 3, label = motor)
  }
  # Donde la conexion cita el nombre, la otra tabla se publica citada y no
  # lleva el nombre de la entidad.
  m <- .lect_ref_referida("sqlite", "sucursales")
  expect_identical(m$entidad_referida[m$entidad == "ventas"], "`sucursales`")
  s <- semaforo_calidad(m, cortes = .lect_ref_cortes)
  expect_identical(s$control_lecturas[s$entidad == "sucursales"], "una_lectura")
})

test_that("sin la tabla resuelta, la tabla referida se reconoce por la sentencia", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.identidad_tabla_dbi = function(...) NA_character_)
  # La misma sentencia FROM que una entidad: el nombre de la entidad.
  m <- .lect_ref_referida("sqlite", "padron")
  expect_true(all(is.na(attr(m, "lecturas", exact = TRUE)$tabla)))
  expect_identical(m$entidad_referida[m$entidad == "ventas"], "sucursales")
  expect_identical(m$estado_cuentas, c("cierran", "cierran"))
  # El nombre de una entidad que es otra tabla: el identificador citado, y sus
  # dos filas no se comparan con las cuatro de la entidad.
  m <- .lect_ref_referida("sqlite", "sucursales")
  expect_identical(m$entidad_referida[m$entidad == "ventas"], "`sucursales`")
  expect_identical(m$estado_cuentas, c("cierran", "cierran"))
})
