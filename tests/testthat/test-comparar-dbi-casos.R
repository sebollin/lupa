# comparar_perfiles() con perfiles de una base: ninguna fila rotulada
# `tabla_completa` sale de una cifra de muestra, y lo que no se compara dice el
# motivo verdadero. Cada caso fue una deriva falsa o un silencio medido sobre
# la primera version de la comparacion de perfiles de una base.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.cdc_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cdc_febrero <- as.POSIXct("2026-02-01", tz = "UTC")
.cdc_metricas <- c("validos", "distintos", "basicos")

.cdc_sqlite <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

.cdc_duckdb <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  DBI::dbConnect(.duckdb_prueba(), ":memory:")
}

.cdc_dbi <- function(con, tabla, fecha, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, fecha = fecha, instrumentar = FALSE,
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE, ...
  )))
}

.cdc_comparar <- function(...) {
  as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(...))))
}

.cdc_insertar <- function(con, tabla, filas) {
  DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ",
                             paste(filas, collapse = ",")))
}

test_that("sin el total de la fuente, las filas de la muestra no son las de la tabla", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  columnas <- sprintf("c%d", 1:8)
  DBI::dbExecute(con, paste0("CREATE TABLE t (id INTEGER, ",
                             paste(sprintf("%s INTEGER", columnas), collapse = ", "), ")"))
  filas <- vapply(1:2000, function(i) {
    paste0("(", i, ", ", paste((i * 1:8) %% 100L, collapse = ", "), ")")
  }, character(1L))
  .cdc_insertar(con, "t", filas)
  metricas <- c("validos", "distintos", "moda", "basicos", "mediana", "desvio")
  completa <- .cdc_dbi(con, "t", .cdc_enero, muestra = 100L, orden_muestra = "id",
                       metricas = metricas)
  corta <- .cdc_dbi(con, "t", .cdc_febrero, muestra = 100L, orden_muestra = "id",
                    metricas = metricas, max_consultas = 12)
  # La premisa: el presupuesto alcanza para la muestra y no para el conteo.
  expect_false(is.null(corta$perfil_muestra))
  expect_true(is.na(corta$perfil_muestra$meta$filas_totales_fuente))
  deriva <- .cdc_comparar(completa, corta)
  fila <- deriva[deriva$aspecto == "filas", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$cambio, "no_comparable")
  expect_true(is.na(fila$universo_actual))
  expect_false(any(deriva$cambio == "modificado" &
                     deriva$universo_actual %in% "tabla_completa" &
                     deriva$universo_anterior %in% "tabla_completa"))
})

test_that("el tipo de una columna NUMERIC no cambia porque cambia la muestra", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, x NUMERIC)")
  .cdc_insertar(con, "t", sprintf("(%d,%s)", 1:3000,
                                  c(as.character(1:1000), rep("1.5", 2000))))
  cien <- .cdc_dbi(con, "t", .cdc_enero, muestra = 100L, orden_muestra = "id",
                   metricas = .cdc_metricas)
  todas <- .cdc_dbi(con, "t", .cdc_febrero, muestra = Inf, orden_muestra = "id",
                    metricas = .cdc_metricas)
  # La premisa: el controlador lee `entero` con cien filas y `doble` con todas.
  expect_false(identical(cien$perfil_muestra$columnas$tipo_declarado[[2L]],
                         todas$perfil_muestra$columnas$tipo_declarado[[2L]]))
  expect_identical(cien$resumen_tabla$meta$tipos_motor$tipo,
                   todas$resumen_tabla$meta$tipos_motor$tipo)
  deriva <- .cdc_comparar(cien, todas)
  expect_false(any(deriva$aspecto %in% c("tipo_declarado", "tipo_inferido")))
})

test_that("con aplicabilidad, los NULL del motor no se suman a la muestra", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE ap (id INTEGER, grupo TEXT, v INTEGER, orden INTEGER)")
  orden <- integer(3000)
  orden[1:100] <- c(1:50, 101:150)
  orden[101:3000] <- c(51:100, 151:3000)
  .cdc_insertar(con, "ap", sprintf(
    "(%d,'%s',%s,%d)", 1:3000, ifelse(1:3000 <= 100, "A", "B"),
    ifelse(1:3000 <= 100, as.character(1:100), "NULL"), orden
  ))
  regla <- list(v = ~ grupo == "A")
  por_orden <- .cdc_dbi(con, "ap", .cdc_enero, muestra = 100L, orden_muestra = "orden",
                        metricas = .cdc_metricas, aplicabilidad = regla)
  por_id <- .cdc_dbi(con, "ap", .cdc_febrero, muestra = 100L, orden_muestra = "id",
                     metricas = .cdc_metricas, aplicabilidad = regla)
  # La premisa: los NULL del motor son de filas fuera de la regla.
  expect_equal(por_id$resumen_tabla$columnas$prop_faltantes[[3L]], 2900 / 3000)
  deriva <- .cdc_comparar(por_orden, por_id)
  expect_false(any(deriva$aspecto == "faltantes"))
  expect_false(any(c(deriva$universo_anterior, deriva$universo_actual) %in% "mixto"))
})

test_that("el rango no se compara entre tipos distintos, y la fila de tipo lo dice", {
  sqlite <- .cdc_sqlite()
  duck <- .cdc_duckdb()
  on.exit({
    DBI::dbDisconnect(sqlite)
    DBI::dbDisconnect(duck, shutdown = TRUE)
  }, add = TRUE)
  DBI::dbExecute(sqlite, "CREATE TABLE u (id INTEGER, flag INTEGER)")
  DBI::dbExecute(duck, "CREATE TABLE u (id INTEGER, flag BOOLEAN)")
  .cdc_insertar(sqlite, "u", sprintf("(%d,%d)", 1:3, c(0L, 1L, 1L)))
  .cdc_insertar(duck, "u", sprintf("(%d,%s)", 1:3, c("false", "true", "true")))
  deriva <- .cdc_comparar(
    .cdc_dbi(sqlite, "u", .cdc_enero, muestra = 2L, orden_muestra = "id",
             metricas = .cdc_metricas),
    .cdc_dbi(duck, "u", .cdc_febrero, muestra = 2L, orden_muestra = "id",
             metricas = .cdc_metricas)
  )
  expect_false(any(deriva$aspecto == "rango" & deriva$cambio == "modificado"))
  rango <- deriva$evidencia[deriva$aspecto == "rango" & deriva$cambio == "no_comparable"]
  expect_true(grepl("flag: tipo_distinto:numero_vs_logico", rango, fixed = TRUE,
                    useBytes = TRUE))
  tipo <- deriva[deriva$aspecto == "tipo_declarado" & deriva$columna %in% "flag", ,
                 drop = FALSE]
  expect_equal(nrow(tipo), 1L)
  expect_identical(c(tipo$universo_anterior, tipo$universo_actual),
                   c("muestra", "muestra"))
  # Un entero que pasa a texto con las mismas cifras, en el mismo motor.
  DBI::dbExecute(sqlite, "CREATE TABLE a (id INTEGER, x INTEGER)")
  DBI::dbExecute(sqlite, "CREATE TABLE b (id INTEGER, x TEXT)")
  cifras <- c(rep("NULL", 2900), as.character(1:100))
  .cdc_insertar(sqlite, "a", sprintf("(%d,%s)", 1:3000, cifras))
  .cdc_insertar(sqlite, "b", sprintf("(%d,%s)", 1:3000,
                                     ifelse(cifras == "NULL", "NULL", paste0("'", cifras, "'"))))
  texto <- .cdc_comparar(
    .cdc_dbi(sqlite, "a", .cdc_enero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas),
    .cdc_dbi(sqlite, "b", .cdc_febrero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas)
  )
  expect_false(any(texto$aspecto == "rango" & texto$cambio == "modificado"))
  expect_true(any(grepl("x: tipo_distinto:numero_vs_texto", texto$evidencia,
                        fixed = TRUE, useBytes = TRUE)))
  expect_true(any(texto$aspecto == "tipo_declarado" & texto$columna %in% "x"))
})

test_that("la cardinalidad de un texto se compara solo con el mismo cotejamiento", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE a (id INTEGER, correo TEXT COLLATE NOCASE)")
  DBI::dbExecute(con, "CREATE TABLE b (id INTEGER, correo TEXT)")
  correos <- c("a@x", "A@x", "b@x", "B@x", "c@x", "C@x")
  .cdc_insertar(con, "a", sprintf("(%d,'%s')", 1:6, correos))
  .cdc_insertar(con, "b", sprintf("(%d,'%s')", 1:6, correos))
  nocase <- .cdc_dbi(con, "a", .cdc_enero, muestra = 3L, orden_muestra = "id",
                     metricas = .cdc_metricas)
  binario <- .cdc_dbi(con, "b", .cdc_febrero, muestra = 3L, orden_muestra = "id",
                      metricas = .cdc_metricas)
  expect_identical(nocase$resumen_tabla$meta$cotejamientos$cotejamiento[[2L]], "nocase")
  deriva <- .cdc_comparar(nocase, binario)
  expect_false(any(deriva$aspecto == "cardinalidad" & deriva$cambio == "modificado"))
  expect_true(any(grepl("correo: cotejamiento_distinto:nocase_vs_binary",
                        deriva$evidencia, fixed = TRUE, useBytes = TRUE)))
  # Con el mismo cotejamiento, un cambio real se publica.
  DBI::dbExecute(con, "CREATE TABLE c (id INTEGER, correo TEXT)")
  .cdc_insertar(con, "c", sprintf("(%d,'%s')", 1:6, rep(c("a@x", "b@x"), 3L)))
  real <- .cdc_comparar(binario, .cdc_dbi(con, "c", as.POSIXct("2026-03-01", tz = "UTC"),
                                          muestra = 3L, orden_muestra = "id",
                                          metricas = .cdc_metricas))
  fila <- real[real$aspecto == "cardinalidad" & real$columna %in% "correo", , drop = FALSE]
  expect_identical(fila$cambio, "modificado")
  expect_identical(fila$universo_actual, "tabla_completa")
})

test_that("un texto contra una fecha dice que el tipo es distinto, no el cotejamiento", {
  sqlite <- .cdc_sqlite()
  duck <- .cdc_duckdb()
  on.exit({
    DBI::dbDisconnect(sqlite)
    DBI::dbDisconnect(duck, shutdown = TRUE)
  }, add = TRUE)
  fechas <- as.character(as.Date("2020-01-01") + 0:99)
  DBI::dbExecute(sqlite, "CREATE TABLE t (id INTEGER, f TEXT)")
  DBI::dbExecute(duck, "CREATE TABLE t (id INTEGER, f DATE)")
  .cdc_insertar(sqlite, "t", sprintf("(%d,'%s')", 1:100, fechas))
  .cdc_insertar(duck, "t", sprintf("(%d, DATE '%s')", 1:100, fechas))
  deriva <- .cdc_comparar(
    .cdc_dbi(sqlite, "t", .cdc_enero, muestra = 10L, orden_muestra = "id",
             metricas = .cdc_metricas),
    .cdc_dbi(duck, "t", .cdc_febrero, muestra = 10L, orden_muestra = "id",
             metricas = .cdc_metricas)
  )
  cardinalidad <- deriva$evidencia[deriva$aspecto == "cardinalidad" &
                                     deriva$cambio == "no_comparable"]
  expect_identical(cardinalidad, "f: tipo_distinto:texto_vs_fecha")
  expect_true(all(deriva$universo_anterior[deriva$aspecto %in% c("tipo_declarado", "unidad")] ==
                    "muestra"))
})

test_that("dos muestras de la misma tabla no comparan su tipo inferido", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, orden INTEGER, x TEXT)")
  .cdc_insertar(con, "t", sprintf("(%d,%d,'%s')", 1:3000, 3001L - (1:3000),
                                  ifelse(1:3000 <= 1000, as.character(1:3000), "N/D")))
  deriva <- .cdc_comparar(
    .cdc_dbi(con, "t", .cdc_enero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas),
    .cdc_dbi(con, "t", .cdc_febrero, muestra = 100L, orden_muestra = "orden",
             metricas = .cdc_metricas)
  )
  expect_false(any(deriva$aspecto == "tipo_inferido"))
  # Los faltantes disfrazados se estiman en la muestra, y la fila lo dice.
  faltantes <- deriva[deriva$aspecto == "faltantes", , drop = FALSE]
  expect_true(all(faltantes$universo_actual == "mixto"))
})

test_that("comparar_equivalencia no compara dos muestras del motor independientes", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE m (id INTEGER, v INTEGER)")
  set.seed(7)
  valores <- c(rep(1L, 2700), sample(1000:1100, 300, replace = TRUE))
  .cdc_insertar(con, "m", sprintf("(%d,%d)", 1:3000, valores))
  motor <- function(fecha) {
    .cdc_dbi(con, "m", fecha, universo = "muestra_motor", muestra_motor = 300,
             muestra = 100L, orden_muestra = "id", metricas = .cdc_metricas)
  }
  uno <- motor(.cdc_enero)
  otro <- motor(.cdc_febrero)
  e <- as.data.frame(comparar_equivalencia(uno, otro, tolerancia = 0.01))
  expect_false(any(e$veredicto == "materialmente_distinto"))
  expect_true(any(e$motivo == "universo_distinto:muestras_independientes"))
  # La misma muestra contra si misma si se compara.
  mismo <- as.data.frame(comparar_equivalencia(uno, uno, tolerancia = 0))
  expect_false(any(mismo$motivo == "universo_distinto:muestras_independientes"))
})

test_that("lo que un lado no pidio o el motor no calcula se declara con su metrica", {
  con <- .cdc_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE o1 (id INTEGER, v INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE o2 (id INTEGER, v INTEGER)")
  .cdc_insertar(con, "o1", sprintf("(%d,%d)", 1:1000, 1:1000))
  .cdc_insertar(con, "o2", sprintf("(%d,%d)", 1:1000, 901:1900))
  deriva <- .cdc_comparar(
    .cdc_dbi(con, "o1", .cdc_enero, muestra = 100L, orden_muestra = "id",
             metricas = c("validos", "distintos")),
    .cdc_dbi(con, "o2", .cdc_febrero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas)
  )
  rango <- deriva$evidencia[deriva$aspecto == "rango" & deriva$cambio == "no_comparable"]
  expect_true(grepl("v: estado_sql:no_solicitado_vs_calculado", rango, fixed = TRUE,
                    useBytes = TRUE))
  expect_true(grepl("`basicos`", rango, fixed = TRUE, useBytes = TRUE))
  # Una fecha guardada como texto: el motor no calcula sus extremos, y se dice.
  DBI::dbExecute(con, "CREATE TABLE f1 (id INTEGER, f TEXT)")
  DBI::dbExecute(con, "CREATE TABLE f2 (id INTEGER, f TEXT)")
  dias <- as.Date("2020-01-01") + 0:2999
  .cdc_insertar(con, "f1", sprintf("(%d,'%s')", 1:3000, as.character(dias)))
  .cdc_insertar(con, "f2", sprintf("(%d,'%s')", 1:3000,
                                   as.character(c(dias[-3000], as.Date("2044-08-22")))))
  fechas <- .cdc_comparar(
    .cdc_dbi(con, "f1", .cdc_enero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas),
    .cdc_dbi(con, "f2", .cdc_febrero, muestra = 100L, orden_muestra = "id",
             metricas = .cdc_metricas)
  )
  rango <- fechas$evidencia[fechas$aspecto == "rango" & fechas$cambio == "no_comparable"]
  expect_true(grepl("f: sin_cifra_sql:el_resumen_sql_no_calcula_minimo_fecha",
                    rango, fixed = TRUE, useBytes = TRUE))
})
