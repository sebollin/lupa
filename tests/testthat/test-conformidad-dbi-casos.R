# No conformar es que el motor cuenta y da distinto que R. Una abstencion de
# medir_dbi() con los valores inventados no es no conformar, y la bateria nunca
# le quita a medir_dbi() una regla que mediria bien con los datos reales.

.con_conformidad_casos <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

.cerrar_conformidad_casos <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.fecha_conformidad_casos <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

# Con nombre propio: las pruebas eligen las filas de la bateria por regla.
.instancia_casos <- function(metrica, nombre, entidad, atributos, ...) {
  instanciar(especializar(metrica, nombre, ...), entidad, atributos,
             nombre_instancia = nombre)
}

.medir_conformidad_casos <- function(mod, con, ...) {
  suppressWarnings(medir_dbi(
    mod, con, id_medicion = "c", fecha = .fecha_conformidad_casos,
    instrumentar = FALSE, ...
  ))
}

# El valor y todos los avisos, sin dejar escapar ninguno.
.con_avisos_conformidad <- function(expr) {
  avisos <- list()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos[[length(avisos) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

.avisos_de_conformidad <- function(avisos) {
  Filter(function(w) inherits(w, "lupa_aviso_conformidad_dbi"), avisos)
}

test_that("los acentos inventados no le quitan a medir_dbi() las reglas de texto ASCII", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$NoNulo, "N1", "d", "t", valores_nulos = ""),
    .instancia_casos(nuc$NoNulo, "N2", "d", "t", valores_nulos = c("N/A", "-")),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "x")),
    .instancia_casos(nuc$Formato, "F", "d", "t", diccionario = c("A", "B")),
    .instancia_casos(nuc$ReglaIntegridadInterEntidad, "FK", c("r", "d"), c("id", "t"))
  )
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_conformidad_casos(motor)
    DBI::dbExecute(con, "CREATE TABLE d (t VARCHAR)")
    DBI::dbExecute(con, paste(
      "INSERT INTO d VALUES ('A'), (''), ('NA'), ('N/A'), ('-'), (' '),",
      "('B'), (NULL), ('x')"
    ))
    DBI::dbExecute(con, "CREATE TABLE r (id VARCHAR)")
    DBI::dbExecute(con, "INSERT INTO r VALUES ('A'), ('B'), ('1')")
    bateria <- conformidad_dbi(con, mod)
    expect_false(any(bateria$conformidad == "no_conforme"), info = motor)
    expect_true(all(bateria$conformidad[bateria$caso == "limites_texto"] ==
                      "conforme"), info = motor)
    sin <- .medir_conformidad_casos(mod, con)
    con_bateria <- .medir_conformidad_casos(mod, con, conformidad = bateria)
    expect_identical(nrow(sin), 5L, info = motor)
    expect_identical(con_bateria, sin, info = motor)
    .cerrar_conformidad_casos(con)
  }
})

test_that("las columnas se tipan con el esquema real y la intercalacion entra", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$NoNulo, "N", "d", "t"),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "x")),
    .instancia_casos(nuc$AtributoDuplicado, "D", "d", "t")
  )
  ap <- list(t = ~ !is.na(n) & n > 2)
  con <- .con_conformidad_casos("duckdb")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t VARCHAR, n DOUBLE)")
  DBI::dbExecute(con, paste(
    "INSERT INTO d VALUES ('A', 1), ('A', 3), ('x', 4), (NULL, 5), ('', 6),",
    "('A', NULL), ('z', 9)"
  ))
  bateria <- conformidad_dbi(con, mod, aplicabilidad = ap)
  expect_true(all(bateria$esquema == "tabla"))
  expect_true(all(bateria$conformidad == "conforme"))
  expect_identical(
    .medir_conformidad_casos(mod, con, aplicabilidad = ap, conformidad = bateria),
    .medir_conformidad_casos(mod, con, aplicabilidad = ap)
  )

  # Con el tipo real, R cuenta sobre lo que la columna guarda: un UBIGINT
  # redondea 9.9 a 10 y un FLOAT guarda 0.1 en precision simple. Contar sobre el
  # valor inventado daba una no conformidad que con los datos reales no existe.
  numeros <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "R", "u", "n",
                     minimo = 0, maximo = 10),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "F", "u", "x",
                     valores = c(0.1, 0.3))
  )
  DBI::dbExecute(con, "CREATE TABLE u (n UBIGINT, x FLOAT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO u VALUES (1, 0.1), (2, 0.3), (NULL, NULL), (11, 0.2)"
  ))
  bn <- conformidad_dbi(con, numeros)
  expect_true(all(bn$esquema == "tabla"))
  expect_false(any(bn$conformidad == "no_conforme"))
  expect_identical(.medir_conformidad_casos(numeros, con, conformidad = bn),
                   .medir_conformidad_casos(numeros, con))

  # Con la sesion en `nocase`, `t == "A"` cuenta tambien "a": la bateria no
  # puede decir que conforma. Con la tabla y sin ella, el cotejamiento entra.
  DBI::dbExecute(con, "SET default_collation = 'nocase'")
  pred <- modelo(
    .instancia_casos(nuc$ReglaIntegridadIntraEntidad, "P", "d", "t",
                     regla = ~ !is.na(t) & t == "A"),
    .instancia_casos(nuc$ReglaIntegridadIntraEntidad, "Q", "sin_tabla", "t",
                     regla = ~ !is.na(t) & t == "A")
  )
  bp <- conformidad_dbi(con, pred)
  expect_identical(attr(bp, "intercalacion", exact = TRUE), "nocase")
  expect_false(any(bp$conformidad == "conforme"))
  expect_true(all(bp$conformidad == "abstencion"))
})

test_that("columna contra columna se cuenta con dos columnas de literales", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$ReglaIntegridadIntraEntidad, "Pab", "d", c("a", "b"),
                     regla = ~ !is.na(a) & !is.na(b) & a > b),
    .instancia_casos(nuc$ReglaIntegridadIntraEntidad, "Pfg", "d", c("f", "g"),
                     regla = ~ !is.na(f) & !is.na(g) & f < g)
  )
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_conformidad_casos(motor)
    DBI::dbExecute(con, "CREATE TABLE d (a DOUBLE, b DOUBLE, f DATE, g DATE)")
    DBI::dbExecute(con, paste(
      "INSERT INTO d VALUES (1, 2, '2026-01-01', '2026-02-01'),",
      "(3, 2, '2026-03-01', '2026-02-01'), (NULL, 1, NULL, NULL),",
      "(2, 2, '2027-01-01', '2027-01-01')"
    ))
    bateria <- conformidad_dbi(con, mod)
    expect_true(all(bateria$conformidad == "conforme"), info = motor)
    # Cada valor de `a` se cruza con cada valor de `b`.
    expect_true(all(bateria$denominador_motor > 20), info = motor)
    sin <- .medir_conformidad_casos(mod, con)
    expect_identical(nrow(sin), 2L, info = motor)
    expect_identical(.medir_conformidad_casos(mod, con, conformidad = bateria),
                     sin, info = motor)
    .cerrar_conformidad_casos(con)
  }
  # Sin la tabla, las columnas sin clase prueban tambien las numericas.
  con <- .con_conformidad_casos("duckdb")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  bateria <- conformidad_dbi(con, mod)
  expect_true(all(bateria$esquema == "supuesto"))
  expect_true(any(bateria$conformidad == "conforme"))
  expect_false(any(bateria$conformidad == "no_conforme"))
})

test_that("lo que falla solo con NULL queda condicionado a los NULL de la tabla real", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(.instancia_casos(nuc$ReglaIntegridadIntraEntidad, "P", "d", "t",
                                 regla = ~ t == "A"))
  con <- .con_conformidad_casos("duckdb")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t VARCHAR)")
  descompuesta <- intToUtf8(c(0x61L, 0x301L))
  compuesta <- intToUtf8(0xE1L)
  DBI::dbWriteTable(con, "d", data.frame(t = c(descompuesta, compuesta, "A"),
                                         stringsAsFactors = FALSE),
                    append = TRUE)
  bateria <- conformidad_dbi(con, mod)
  expect_true(any(bateria$condicion == "con_nulos"))
  expect_true(all(bateria$conformidad[bateria$condicion == "con_nulos"] ==
                    "abstencion"))
  sin_nulos <- bateria[grepl("_sin_nulos$", bateria$caso), , drop = FALSE]
  expect_gt(nrow(sin_nulos), 0L)
  expect_true(all(sin_nulos$conformidad == "conforme"))
  sin <- .medir_conformidad_casos(mod, con)
  expect_identical(c(sin$numerador, sin$denominador), c(1, 3))
  expect_identical(.medir_conformidad_casos(mod, con, conformidad = bateria), sin)
})

test_that("medir_dbi() aplica una no conformidad con NULL solo si la tabla tiene NULL", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  mod <- modelo(.instancia_casos(nuc$ReglaIntegridadIntraEntidad, "P", "d", "t",
                                 regla = ~ is.na(t) | t == "A"))
  con <- .con_conformidad_casos("sqlite")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t TEXT)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('A'), ('B')")
  bateria <- conformidad_dbi(con, mod)
  # Una diferencia que el motor tuviera solo en las filas con NULL.
  i <- which(bateria$caso == "limites_texto")
  bateria$conformidad[i] <- "no_conforme"
  bateria$condicion[i] <- "con_nulos"
  sin <- .medir_conformidad_casos(mod, con)
  expect_identical(.medir_conformidad_casos(mod, con, conformidad = bateria), sin)
  DBI::dbExecute(con, "INSERT INTO d VALUES (NULL)")
  con_nulo <- .medir_conformidad_casos(mod, con, conformidad = bateria)
  expect_identical(nrow(con_nulo), 0L)
  cobertura <- attr(con_nulo, "cobertura_metricas", exact = TRUE)
  expect_true(grepl("NULL", cobertura$motivo[[1L]], fixed = TRUE))
  # La misma fila sin condicion quita la regla aunque no haya NULL.
  DBI::dbExecute(con, "DELETE FROM d WHERE t IS NULL")
  bateria$condicion[i] <- "siempre"
  expect_identical(nrow(.medir_conformidad_casos(mod, con, conformidad = bateria)),
                   0L)
})

test_that("la bateria no aborta y cada regla que no evalua queda no_evaluada", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "B")),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "Enan", "d", "n",
                     valores = c(NaN, Inf, -Inf, 1)),
    .instancia_casos(nuc$NoNulo, "Nnan", "d", "n", valores_nulos = c(NaN, Inf)),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "El", "d", "b", valores = TRUE),
    .instancia_casos(nuc$ReglaIntegridadIntraEntidad, "Pl", "d", "b",
                     regla = ~ is.na(b) | b == TRUE),
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "Rt", "d", "t",
                     minimo = "A", maximo = "Z"),
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "Rmax", "d", "n",
                     minimo = 2147483647L, maximo = 2147483647L)
  )
  nombres <- vapply(mod$metricas, `[[`, character(1L), "nombre")
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_conformidad_casos(motor)
    DBI::dbExecute(con, "CREATE TABLE d (t VARCHAR, n DOUBLE, b BOOLEAN)")
    DBI::dbExecute(con, "INSERT INTO d VALUES ('A', 1, TRUE), ('B', 2, FALSE)")
    expect_no_warning(bateria <- conformidad_dbi(con, mod))
    expect_setequal(unique(bateria$metrica_instanciada), nombres)
    expect_false(any(bateria$conformidad == "no_conforme"), info = motor)
    expect_true(all(bateria$conformidad[bateria$metrica_instanciada == "E" &
                                          bateria$caso == "limites_texto"] ==
                      "conforme"), info = motor)
    expect_true(all(bateria$conformidad[bateria$metrica_instanciada == "Rt"] ==
                      "abstencion"), info = motor)
    .cerrar_conformidad_casos(con)
  }

  # Una falla propia de la bateria en una regla deja esa regla no_evaluada, la
  # bateria sigue con las demas y medir_dbi() la mide como siempre, con aviso.
  con <- .con_conformidad_casos("sqlite")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t TEXT, n REAL)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('A', 1), ('B', 5)")
  dos <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "B")),
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "R", "d", "n", minimo = 0, maximo = 3)
  )
  original <- get(".esperado_caso_conformidad", envir = asNamespace("lupa"))
  local_mocked_bindings(
    .esperado_caso_conformidad = function(spec, ...) {
      if (identical(spec$nodo, "en_rango")) .detener("falla de prueba")
      original(spec, ...)
    },
    .package = "lupa"
  )
  bateria <- conformidad_dbi(con, dos)
  expect_true(all(bateria$conformidad[bateria$metrica_instanciada == "R"] ==
                    "no_evaluada"))
  expect_true(any(grepl("falla de prueba",
                        bateria$motivo[bateria$metrica_instanciada == "R"],
                        fixed = TRUE)))
  expect_true(any(bateria$conformidad[bateria$metrica_instanciada == "E"] ==
                    "conforme"))
  medida <- .con_avisos_conformidad(medir_dbi(
    dos, con, id_medicion = "c", fecha = .fecha_conformidad_casos,
    instrumentar = FALSE, conformidad = bateria
  ))
  expect_length(.avisos_de_conformidad(medida$avisos), 1L)
  expect_identical(medida$valor, .medir_conformidad_casos(dos, con))
})

test_that("una no conformidad le quita la regla a medir_dbi() y solo a esa", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "B")),
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "R", "d", "n", minimo = 0, maximo = 3)
  )
  con <- .con_conformidad_casos("sqlite")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t TEXT, n REAL)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('A', 1), ('B', 5)")
  original <- get(".esperado_caso_conformidad", envir = asNamespace("lupa"))
  local_mocked_bindings(
    .esperado_caso_conformidad = function(spec, ...) {
      r <- original(spec, ...)
      # R contaria otra cosa que el motor en la regla de rango.
      if (identical(spec$nodo, "en_rango") && !is.null(r$num)) r$num <- r$num + 1
      r
    },
    .package = "lupa"
  )
  bateria <- conformidad_dbi(con, mod)
  expect_true(all(bateria$conformidad[bateria$metrica_instanciada == "R"] ==
                    "no_conforme"))
  con_bateria <- .medir_conformidad_casos(mod, con, conformidad = bateria)
  expect_identical(con_bateria$metrica_instanciada, "E")
  cobertura <- attr(con_bateria, "cobertura_metricas", exact = TRUE)
  expect_identical(cobertura$metrica_instanciada, "R")
  expect_true(grepl("conformidad", cobertura$motivo[[1L]], fixed = TRUE))
})

test_that("medir_dbi() no aplica una bateria de otra conexion, modelo o configuracion", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  preparar <- function(con) {
    DBI::dbExecute(con, "CREATE TABLE d (t VARCHAR, n DOUBLE)")
    DBI::dbExecute(con, paste(
      "INSERT INTO d VALUES ('A', 1), ('B', 2), ('x', 3), (NULL, 4), ('', 5)"
    ))
  }
  con <- .con_conformidad_casos("duckdb")
  otra <- .con_conformidad_casos("sqlite")
  on.exit({
    .cerrar_conformidad_casos(con)
    .cerrar_conformidad_casos(otra)
  }, add = TRUE)
  preparar(con)
  preparar(otra)
  a <- modelo(
    .instancia_casos(nuc$NoNulo, "NN", "d", "t", valores_nulos = c("", "NA")),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "B"))
  )
  b <- modelo(
    .instancia_casos(nuc$NoNulo, "NN", "d", "n"),
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "n", valores = c(1, 2))
  )
  bateria <- conformidad_dbi(con, a)
  # Las reglas de la bateria, marcadas como no conformes: si medir_dbi()
  # la aplicara donde no corresponde, las quitaria.
  falsa <- bateria
  falsa$conformidad[] <- "no_conforme"
  aplicar <- function(mod, conexion, conformidad, ...) {
    .con_avisos_conformidad(medir_dbi(
      mod, conexion, id_medicion = "c", fecha = .fecha_conformidad_casos,
      instrumentar = FALSE, conformidad = conformidad, ...
    ))
  }
  # Mismos nombres, otras reglas: no estan cubiertas.
  r <- aplicar(b, con, falsa)
  expect_length(.avisos_de_conformidad(r$avisos), 1L)
  expect_identical(r$valor, .medir_conformidad_casos(b, con))
  # Otra conexion y otro motor.
  r <- aplicar(a, otra, falsa)
  expect_length(.avisos_de_conformidad(r$avisos), 1L)
  expect_identical(r$valor, .medir_conformidad_casos(a, otra))
  # Otra configuracion.
  r <- aplicar(a, con, falsa, max_valores_in = 10L)
  expect_length(.avisos_de_conformidad(r$avisos), 1L)
  expect_identical(r$valor, .medir_conformidad_casos(a, con, max_valores_in = 10L))
  # Una regla que la bateria no cubrio se mide como siempre, con aviso.
  c_mod <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t", valores = c("A", "B")),
    .instancia_casos(nuc$AtributoDuplicado, "NUEVA", "d", "t")
  )
  r <- aplicar(c_mod, con, bateria)
  avisos <- .avisos_de_conformidad(r$avisos)
  expect_length(avisos, 1L)
  expect_true(grepl("NUEVA", conditionMessage(avisos[[1L]]), fixed = TRUE))
  expect_identical(r$valor, .medir_conformidad_casos(c_mod, con))
  # Lo no verificado nunca quita una regla.
  sin_verificar <- bateria
  sin_verificar$conformidad[] <- "no_verificado"
  r <- aplicar(a, con, sin_verificar)
  expect_length(.avisos_de_conformidad(r$avisos), 1L)
  expect_identical(r$valor, .medir_conformidad_casos(a, con))
  # La misma bateria, aplicada donde corresponde, si quita las reglas.
  expect_identical(nrow(.medir_conformidad_casos(a, con, conformidad = falsa)), 0L)
  # Una tabla armada a mano se rechaza.
  a_mano <- data.frame(metrica_instanciada = "E", estado = "conforme",
                       coincide = TRUE)
  expect_error(.medir_conformidad_casos(a, con, conformidad = a_mano),
               "conformidad_dbi")
  con_clase <- structure(data.frame(unclass(bateria)),
                         class = c("conformidad_dbi", "data.frame"))
  expect_error(.medir_conformidad_casos(a, con, conformidad = con_clase),
               "conformidad_dbi")
})

test_that("la columna dice conformidad y la intercalacion por omision se publica", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  con <- .con_conformidad_casos("duckdb")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  bateria <- conformidad_dbi(con, modelo(
    .instancia_casos(nuc$NoNulo, "N", "d", "t")
  ))
  expect_true(all(c("conformidad", "esquema", "condicion") %in% names(bateria)))
  expect_false("estado" %in% names(bateria))
  expect_true(all(bateria$conformidad %in% c(
    "conforme", "no_conforme", "abstencion", "no_evaluada", "no_verificado"
  )))
  expect_identical(attr(bateria, "intercalacion", exact = TRUE), "binary")
  expect_identical(attr(bateria, "intercalacion_motivo", exact = TRUE),
                   NA_character_)
})

test_that("con estrategia bloques, lo que no conforma se mide por bloques en R", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  mod <- modelo(
    .instancia_casos(nuc$ValoresPosiblesPorExtension, "E", "d", "t",
                     valores = c("A", "B")),
    .instancia_casos(nuc$ValoresPosiblesPorComprension, "R", "d", "n",
                     minimo = 0, maximo = 3)
  )
  con <- .con_conformidad_casos("sqlite")
  on.exit(.cerrar_conformidad_casos(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t TEXT, n REAL)")
  DBI::dbExecute(con, paste(
    "INSERT INTO d VALUES ('A', 1), ('B', 5), ('x', NULL), ('A', 2.5), ('B', -1)"
  ))
  bateria <- local({
    original <- get(".esperado_caso_conformidad", envir = asNamespace("lupa"))
    local_mocked_bindings(
      .esperado_caso_conformidad = function(spec, ...) {
        r <- original(spec, ...)
        # R contaria otra cosa que el motor en la regla de rango.
        if (identical(spec$nodo, "en_rango") && !is.null(r$num)) r$num <- r$num + 1
        r
      },
      .package = "lupa"
    )
    conformidad_dbi(con, mod)
  })
  expect_true(all(bateria$conformidad[bateria$metrica_instanciada == "R"] ==
                    "no_conforme"))
  en_r <- suppressWarnings(medir(
    mod, list(d = DBI::dbReadTable(con, "d")), id_medicion = "c",
    fecha = .fecha_conformidad_casos, detalle = "resumen"
  ))
  bloques <- suppressWarnings(medir_dbi(
    mod, con, id_medicion = "c", fecha = .fecha_conformidad_casos,
    estrategia_no_sql = "bloques", conformidad = bateria
  ))
  expect_identical(
    attr(bloques, "ejecucion_dbi", exact = TRUE)$vias[["R"]], "bloques_r"
  )
  r_bloques <- bloques[bloques$metrica_instanciada == "R", , drop = FALSE]
  r_memoria <- en_r[en_r$metrica_instanciada == "R", , drop = FALSE]
  expect_identical(nrow(r_bloques), 1L)
  expect_identical(c(r_bloques$numerador, r_bloques$denominador),
                   c(r_memoria$numerador, r_memoria$denominador))
  expect_identical(r_bloques$resultado, r_memoria$resultado)
  # Sin bloques, la misma regla queda no_medible con el motivo de la bateria.
  sin_bloques <- .medir_conformidad_casos(mod, con, conformidad = bateria)
  expect_identical(sin_bloques$metrica_instanciada, "E")
  cobertura <- attr(sin_bloques, "cobertura_metricas", exact = TRUE)
  expect_true(grepl("conformidad", cobertura$motivo[[1L]], fixed = TRUE))

  # Una no conformidad que solo aparece con NULL, con NULL en la tabla real: por
  # bloques tambien, con la cifra de R.
  pred <- modelo(.instancia_casos(nuc$ReglaIntegridadIntraEntidad, "P", "d", "t",
                                  regla = ~ is.na(t) | t == "A"))
  bp <- conformidad_dbi(con, pred)
  i <- which(bp$caso == "limites_texto")
  bp$conformidad[i] <- "no_conforme"
  bp$condicion[i] <- "con_nulos"
  DBI::dbExecute(con, "INSERT INTO d VALUES (NULL, 1)")
  p_r <- suppressWarnings(medir(
    pred, list(d = DBI::dbReadTable(con, "d")), id_medicion = "c",
    fecha = .fecha_conformidad_casos, detalle = "resumen"
  ))
  p_bloques <- suppressWarnings(medir_dbi(
    pred, con, id_medicion = "c", fecha = .fecha_conformidad_casos,
    estrategia_no_sql = "bloques", conformidad = bp
  ))
  expect_identical(attr(p_bloques, "ejecucion_dbi", exact = TRUE)$vias[["P"]],
                   "bloques_r")
  expect_identical(c(p_bloques$numerador, p_bloques$denominador),
                   c(p_r$numerador, p_r$denominador))
})
