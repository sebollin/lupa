# Periodos y resumen de fechas en el motor: lo que se cuenta en R por bloques es
# lo que cuenta `perfilar()` sobre la misma tabla, o no se publica.
#
# - Por bloques, lo mismo que la memoria: el universo aplicable, las fechas
#   declaradas, el formato de un texto decidido con la columna entera, sin
#   depender del orden de las filas ni del tamano del bloque.
# - Un periodo se publica solo sobre columnas que `perfilar()` lee como fecha.
# - Evaluar una regla de `aplicabilidad` no cambia los datos que se perfilan.
# - Los topes de la lectura por bloques cortan la lectura.

.precision_fecha <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

.precision_cifras <- c(
  "n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
  "n_fin_abierto_centinela", "n_fin_centinela_declarado",
  "n_fin_centinela_sin_declarar", "n_sin_inicio_con_fin",
  "n_ambos_ausentes_o_centinela"
)

.precision_par <- c(inicio = "lupa_desde", fin = "lupa_hasta")

.precision_saltar <- function(motor) {
  skip_on_cran()
  skip_if_not_installed("DBI")
  if (identical(motor, "sqlite")) skip_if_not_installed("RSQLite")
  if (identical(motor, "duckdb")) skip_if_not_installed("duckdb")
}

.precision_conectar <- function(motor, ...) {
  if (identical(motor, "sqlite")) {
    return(DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE))
  }
  DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:", ...)
}

.precision_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.precision_periodo <- function(x) {
  x$resumen_tabla$meta$periodos
}

# El periodo de `perfilar()` sobre la tabla en memoria: la referencia.
.precision_memoria <- function(datos, ..., periodos = .precision_par) {
  suppressWarnings(perfilar(
    datos, fecha = .precision_fecha, analizar_dependencias = FALSE,
    periodos = periodos, ...
  ))$general$periodos
}

.precision_motor <- function(con, tabla, ..., periodos = .precision_par) {
  .precision_periodo(suppressWarnings(perfilar_dbi(
    con, tabla, fecha = .precision_fecha, periodos = periodos, ...
  )))
}

# La fila de `resumen_tabla$sql` de un campo del resumen de fechas.
.precision_fila_fechas <- function(perfil, columna, metrica = "n_fechas_centinela") {
  s <- perfil$resumen_tabla$sql
  s[s$columna == columna & s$metrica == metrica, , drop = FALSE]
}

# Cuenta las filas que trae la lectura por bloques de fechas, que se reconoce
# por su tamano de bloque. Devuelve una funcion que da la cuenta.
.precision_contar_filas <- function(bloque, entorno = parent.frame()) {
  leidas <- 0
  original <- .traer_filas_dbi
  testthat::local_mocked_bindings(
    .traer_filas_dbi = function(resultado, n = -1L, conexion) {
      b <- original(resultado, n = n, conexion)
      if (identical(as.integer(n), as.integer(bloque))) leidas <<- leidas + nrow(b)
      b
    },
    .package = "lupa", .env = entorno
  )
  function() leidas
}

# Un controlador que entrega el resultado sin filas con las fechas como numeros,
# aunque las filas llegan como fechas (asi lo hace RMariaDB).
.precision_prototipo_sin_fechas <- function(entorno = parent.frame()) {
  original <- .traer_filas_dbi
  testthat::local_mocked_bindings(
    .traer_filas_dbi = function(resultado, n = -1L, conexion) {
      b <- original(resultado, n = n, conexion)
      if (identical(as.integer(n), 0L)) {
        for (campo in names(b)) {
          if (inherits(b[[campo]], "Date")) b[[campo]] <- as.numeric(b[[campo]])
        }
      }
      b
    },
    .package = "lupa", .env = entorno
  )
}

.precision_contratos <- function() {
  data.frame(
    lupa_id = 1:10,
    lupa_estado = c("A", "A", "B", "A", "B", "A", NA, "A", "B", "A"),
    lupa_desde = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-03-01", NA, "2021-01-01",
      "2021-05-05", "2022-01-01", "2019-01-01", "2023-01-01", "2023-06-01"
    )),
    lupa_hasta = as.Date(c(
      "2020-06-01", "2020-01-15", NA, "2020-05-01", "9999-12-31",
      "2021-06-01", "2022-02-01", NA, "2022-01-01", "2099-12-31"
    )),
    stringsAsFactors = FALSE
  )
}

.precision_como_texto <- function(datos) {
  datos$lupa_desde <- as.character(datos$lupa_desde)
  datos$lupa_hasta <- as.character(datos$lupa_hasta)
  datos
}

# ---- Las declaraciones llegan al periodo contado en R ----------------------

test_that("F5: un dominio llega al periodo contado por bloques", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(
    lupa_desde = c("2020-01-01", "2020-02-01"),
    lupa_hasta = c("9999-12-31", "2020-03-01"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "lupa_precision_f5", datos)
  dominio <- list(lupa_hasta = list(centinelas = "9999-12-31"))
  fila <- .precision_motor(con, "lupa_precision_f5", dominios = dominio,
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(fila$n_fin_centinela_declarado, 1)
  expect_identical(fila[.precision_cifras],
                   .precision_memoria(datos, dominios = dominio)[.precision_cifras])
})

test_that("F6: las centinelas de fecha llegan al periodo contado por bloques", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(
    lupa_desde = c("2020-01-01", "2020-02-01"),
    lupa_hasta = c("2099-12-31", "2020-03-01"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "lupa_precision_f6", datos)
  fila <- .precision_motor(con, "lupa_precision_f6", centinelas_fecha = "2099-12-31",
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(fila$n_fin_centinela_declarado, 1)
  expect_identical(
    fila[.precision_cifras],
    .precision_memoria(datos, centinelas_fecha = "2099-12-31")[.precision_cifras]
  )
})

# ---- Lo que el motor no cuenta como R se declara ----------------------------

test_that("F8: un motor sin cuenta medida declara el resumen", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .MOTORES_RESUMEN_FECHAS_DBI = character(), .package = "lupa"
  )
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f8", data.frame(
    lupa_fecha = as.Date(c("2020-01-01", "2020-02-01"))
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f8", fecha = .precision_fecha,
    resumen_fechas = TRUE
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_fecha")
  expect_identical(fila$estado, "no_disponible")
})

test_that("F9: un TIMESTAMP con huso no saltea la guarda del resumen", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .timestamp_con_huso_dbi = function(...) TRUE, .package = "lupa"
  )
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f9", data.frame(
    lupa_fecha = as.Date(c("2020-01-01", "2020-02-01"))
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f9", fecha = .precision_fecha,
    resumen_fechas = TRUE
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_fecha")
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "TIMESTAMP", fixed = TRUE)
})

test_that("F10: la clase de una fecha sale tambien del tipo que declara el controlador", {
  .precision_saltar("sqlite")
  .precision_prototipo_sin_fechas()
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(lupa_fecha = as.Date(c(
    "1900-01-01", "1900-01-01", "1900-01-01", "2020-01-01", "2020-02-01",
    "2021-03-01", "2022-04-01", "2023-05-01", "2024-06-01", "2025-07-01"
  )))
  DBI::dbWriteTable(con, "lupa_precision_f10", datos)
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f10", fecha = .precision_fecha,
    resumen_fechas = TRUE
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_fecha")
  expect_identical(fila$estado, "calculado")
  memoria <- suppressWarnings(perfilar(
    datos, fecha = .precision_fecha, analizar_dependencias = FALSE,
    resumen_fechas = TRUE
  ))
  columnas <- perfil$resumen_tabla$columnas
  expect_identical(
    columnas$n_fechas_centinela[columnas$columna == "lupa_fecha"],
    memoria$columnas$n_fechas_centinela[memoria$columnas$columna == "lupa_fecha"]
  )
})

test_that("F11: una fecha de la muestra que el controlador no entrega se declara", {
  .precision_saltar("sqlite")
  .precision_prototipo_sin_fechas()
  # Y tampoco la declara: ni el resultado sin filas ni su tipo dicen que es fecha.
  testthat::local_mocked_bindings(
    .clase_fecha_columna_dbi = function(...) NA_character_, .package = "lupa"
  )
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f11", data.frame(
    lupa_fecha = as.Date(c("2020-01-01", "2020-02-01", "2021-03-01"))
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f11", fecha = .precision_fecha,
    resumen_fechas = TRUE
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_fecha")
  expect_identical(nrow(fila), 1L)
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "controlador", fixed = TRUE)
})

test_that("F12: el plan reserva la lectura del periodo por bloques", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f12", data.frame(
    lupa_desde = as.Date(c("2020-01-01", "2020-02-01")),
    lupa_hasta = as.Date(c("2020-03-01", "2020-04-01"))
  ))
  plan <- suppressWarnings(plan_perfilado_dbi(
    con, "lupa_precision_f12", periodos = .precision_par,
    estrategia_no_sql = "bloques"
  ))
  fila <- plan[plan$clase_consulta == "periodos (particion contra literales)", , drop = FALSE]
  expect_true(nrow(fila) == 1L)
  expect_true(fila$n_consultas_max >= 2)
})

test_that("el plan no preve la consulta del motor de un periodo con universo declarado", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_plan", .precision_contratos())
  fila_del_plan <- function(...) {
    plan <- suppressWarnings(plan_perfilado_dbi(
      con, "lupa_precision_plan", periodos = .precision_par, ...
    ))
    plan[plan$clase_consulta == "periodos (particion contra literales)", , drop = FALSE]
  }
  expect_identical(fila_del_plan()$n_consultas, 1)
  expect_identical(
    fila_del_plan(aplicabilidad = list(lupa_hasta = ~ lupa_estado == "A"))$n_consultas, 0
  )
  expect_identical(fila_del_plan(columnas_opcionales = "lupa_hasta")$n_consultas, 0)
})

test_that("F13: muestra_motor no publica el periodo de la tabla entera", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f13", data.frame(
    lupa_desde = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    lupa_hasta = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01"))
  ))
  fila <- .precision_motor(con, "lupa_precision_f13", universo = "muestra_motor",
                           muestra_motor = 2)
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "muestra_motor", fixed = TRUE)
})

test_that("muestra_motor tampoco lo publica en un motor sin cuenta medida y con bloques", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .MOTORES_RESUMEN_FECHAS_DBI = character(), .package = "lupa"
  )
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_m2", data.frame(
    lupa_desde = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    lupa_hasta = as.Date(c("2020-03-01", "2020-04-01", "2020-05-01"))
  ))
  fila <- .precision_motor(con, "lupa_precision_m2", universo = "muestra_motor",
                           muestra_motor = 2, estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "no_disponible")
  expect_true(all(is.na(unlist(fila[.precision_cifras]))))
  expect_match(fila$motivo, "muestra_motor", fixed = TRUE)
})

# ---- El universo del periodo es el del perfil --------------------------------

test_that("M1: el periodo por bloques cuenta el universo aplicable de la memoria", {
  for (motor in c("sqlite", "duckdb")) {
    .precision_saltar(motor)
    for (forma in c("fecha", "texto")) {
      datos <- .precision_contratos()
      if (identical(forma, "texto")) datos <- .precision_como_texto(datos)
      con <- .precision_conectar(motor)
      DBI::dbWriteTable(con, "lupa_precision_m1", datos)
      etiqueta <- paste(motor, forma)
      regla <- list(lupa_hasta = ~ lupa_estado == "A")
      memoria <- .precision_memoria(datos, aplicabilidad = regla)
      expect_lt(memoria$n, nrow(datos))
      fila <- .precision_motor(con, "lupa_precision_m1", aplicabilidad = regla,
                               estrategia_no_sql = "bloques")
      expect_identical(fila$estado, "calculado", label = etiqueta)
      expect_identical(fila[.precision_cifras], memoria[.precision_cifras],
                       label = etiqueta)
      # `columnas_opcionales` es la otra forma de declarar el mismo universo.
      memoria <- .precision_memoria(datos, columnas_opcionales = "lupa_hasta")
      expect_lt(memoria$n, nrow(datos))
      fila <- .precision_motor(con, "lupa_precision_m1",
                               columnas_opcionales = "lupa_hasta",
                               estrategia_no_sql = "bloques")
      expect_identical(fila[.precision_cifras], memoria[.precision_cifras],
                       label = etiqueta)
      .precision_cerrar(con, motor)
    }
  }
})

test_that("M1: sin bloques, un periodo con universo declarado no se cuenta en el motor", {
  for (motor in c("sqlite", "duckdb")) {
    .precision_saltar(motor)
    con <- .precision_conectar(motor)
    DBI::dbWriteTable(con, "lupa_precision_m1b", .precision_contratos())
    for (declarado in list(
      list(aplicabilidad = list(lupa_hasta = ~ lupa_estado == "A")),
      list(columnas_opcionales = "lupa_hasta")
    )) {
      fila <- do.call(.precision_motor, c(list(con, "lupa_precision_m1b"), declarado))
      expect_identical(fila$estado, "no_disponible", label = motor)
      expect_true(all(is.na(unlist(fila[.precision_cifras]))), label = motor)
    }
    # El control: una regla sobre otra columna no cambia el universo del periodo.
    fila <- .precision_motor(con, "lupa_precision_m1b",
                             aplicabilidad = list(lupa_id = ~ lupa_estado == "A"))
    expect_identical(fila$estado, "calculado", label = motor)
    expect_identical(fila$n, 10)
    .precision_cerrar(con, motor)
  }
})

test_that("M1: en el resumen de fechas la aplicabilidad manda sobre la guarda del TIMESTAMP", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .timestamp_con_huso_dbi = function(...) TRUE, .package = "lupa"
  )
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_m1c", data.frame(
    lupa_estado = c("A", "A", "B", "B"),
    lupa_alta = as.Date(c("2024-01-01", "2024-02-01", "2027-01-01", "2027-02-01")),
    stringsAsFactors = FALSE
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_m1c", fecha = .precision_fecha, resumen_fechas = TRUE,
    aplicabilidad = list(lupa_alta = ~ lupa_estado == "A"),
    estrategia_no_sql = "bloques"
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_alta", "n_fechas_futuras")
  # Con bloques la guarda del TIMESTAMP cuenta en R todas las filas: aca no.
  expect_identical(fila$estado, "no_disponible")
  columnas <- perfil$resumen_tabla$columnas
  expect_true(is.na(columnas$n_fechas_futuras[columnas$columna == "lupa_alta"]))
})

test_that("B4: dominios FALSE no rompe el conteo de periodos", {
  .precision_saltar("sqlite")
  datos <- data.frame(
    lupa_desde = as.Date(c("2020-01-01", "2020-02-01")),
    lupa_hasta = as.Date(c("2020-03-01", NA))
  )
  memoria <- .precision_memoria(datos, dominios = FALSE)
  expect_identical(memoria$n, 2)
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_b4", .precision_como_texto(datos))
  fila <- .precision_motor(con, "lupa_precision_b4", dominios = FALSE,
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(
    fila[.precision_cifras],
    .precision_memoria(.precision_como_texto(datos), dominios = FALSE)[.precision_cifras]
  )
})

test_that("una tabla sin filas se cuenta por bloques como en memoria", {
  .precision_saltar("sqlite")
  datos <- .precision_contratos()[0, ]
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_vacia", datos)
  fila <- .precision_motor(con, "lupa_precision_vacia",
                           columnas_opcionales = "lupa_hasta",
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(
    fila[.precision_cifras],
    .precision_memoria(datos, columnas_opcionales = "lupa_hasta")[.precision_cifras]
  )
})

# ---- El formato de un texto lo decide la columna entera ---------------------

# Con un bloque de dos filas, cada tabla se lee en varios bloques y el primero
# no dice el formato de la columna.
.precision_textos_por_bloques <- list(
  "el fin no tiene valor en el primer bloque" = data.frame(
    lupa_desde = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                   "2020-05-01", "2020-06-01"),
    lupa_hasta = c(NA, NA, "2020-03-15", "2020-03-01", "9999-12-31", "2020-06-30"),
    stringsAsFactors = FALSE
  ),
  "otro formato despues del primer bloque" = data.frame(
    lupa_desde = c("2020-01-01", "2020-02-01", "13/03/2020", "25/04/2020",
                   "31/05/2020", "15/06/2020"),
    lupa_hasta = c("2020-01-31", "2020-01-15", "30/03/2020", "20/04/2020",
                   "31/12/9999", "30/06/2020"),
    stringsAsFactors = FALSE
  ),
  "dia y mes ambiguos en el primer bloque" = data.frame(
    lupa_desde = c("03/04/2020", "05/06/2020", "13/07/2020", "25/08/2020",
                   "31/10/2020", "15/11/2020"),
    lupa_hasta = c("04/05/2020", "01/02/2020", "30/07/2020", "20/08/2020",
                   "31/12/9999", "30/11/2020"),
    stringsAsFactors = FALSE
  )
)

test_that("bloques: el formato de un texto se decide con la columna entera, no con el primer bloque", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(.BLOQUE_FECHAS_DBI = 2L, .package = "lupa")
  for (caso in names(.precision_textos_por_bloques)) {
    datos <- .precision_textos_por_bloques[[caso]]
    memoria <- .precision_memoria(datos)
    expect_identical(memoria$n, 6, label = caso)
    for (orden in list(seq_len(6), rev(seq_len(6)))) {
      con <- .precision_conectar("sqlite")
      DBI::dbWriteTable(con, "lupa_precision_a1", datos[orden, , drop = FALSE])
      fila <- .precision_motor(con, "lupa_precision_a1",
                               estrategia_no_sql = "bloques")
      DBI::dbDisconnect(con)
      expect_identical(fila$estado, "calculado", label = caso)
      # El orden de las filas no cambia ninguna cifra.
      expect_identical(fila[.precision_cifras], memoria[.precision_cifras],
                       label = caso)
    }
  }
})

# ---- Solo columnas que `perfilar()` lee como fecha ---------------------------

test_that("bloques: un periodo sobre columnas que la memoria no lee como fecha no se cuenta", {
  .precision_saltar("sqlite")
  no_fechas <- list(
    "texto con valores que no son fecha" = data.frame(
      lupa_desde = c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01",
                     "2020-05-01", "2020-06-01", "sin dato", "pendiente",
                     "a confirmar", "ver nota"),
      lupa_hasta = c("2020-06-01", "2020-01-15", "2020-09-01", "2020-05-01",
                     "9999-12-31", "2021-06-01", "2020-12-01", "no aplica",
                     "2021-01-01", "vigente"),
      stringsAsFactors = FALSE
    ),
    "enteros con la forma de una fecha" = data.frame(
      lupa_desde = c(20200101L, 20200201L, 20200301L, NA, 20210101L),
      lupa_hasta = c(20200601L, 20200115L, NA, 20200501L, 99991231L)
    )
  )
  for (caso in names(no_fechas)) {
    datos <- no_fechas[[caso]]
    # La memoria lo rechaza.
    expect_error(.precision_memoria(datos), "fechas", label = caso)
    con <- .precision_conectar("sqlite")
    DBI::dbWriteTable(con, "lupa_precision_a2", datos)
    fila <- .precision_motor(con, "lupa_precision_a2", estrategia_no_sql = "bloques")
    DBI::dbDisconnect(con)
    expect_identical(fila$estado, "no_disponible", label = caso)
    expect_true(all(is.na(unlist(fila[.precision_cifras]))), label = caso)
    # El motivo cita la causa una sola vez y cierra con un solo punto.
    expect_false(grepl("..", fila$motivo, fixed = TRUE), label = caso)
  }
})

# ---- Evaluar una regla no cambia los datos que se perfilan ------------------

.precision_ventas <- function() {
  data.frame(
    lupa_id = 1:12,
    lupa_activo = rep(c(1, 0, 1, NA), 3),
    lupa_monto = c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27),
    lupa_desde = as.Date("2020-01-01") + 0:11,
    lupa_hasta = as.Date("2020-06-01") + 0:11
  )
}

# Corre una expresion juntando sus avisos, y cuenta los que dicen que una regla
# de `aplicabilidad` no se aplico.
.precision_con_avisos <- function(expresion) {
  avisos <- list()
  valor <- withCallingHandlers(expresion, warning = function(w) {
    avisos[[length(avisos) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

.precision_avisos_de_regla <- function(avisos) {
  sum(vapply(avisos, function(w) {
    inherits(w, "lupa_aviso_muestra_motor") &&
      grepl("no da un valor", conditionMessage(w), fixed = TRUE, useBytes = TRUE)
  }, logical(1L)))
}

.precision_fila_muestra <- function(x, columna) {
  columnas <- x$perfil_muestra$columnas
  salida <- columnas[columnas$columna == columna, , drop = FALSE]
  rownames(salida) <- NULL
  salida
}

.precision_cobertura_regla <- function(x, columna) {
  cobertura <- x$resumen_tabla$cobertura
  cobertura[cobertura$bloque == "perfil_muestra" &
              cobertura$elemento == columna, , drop = FALSE]
}

test_that("regla no logica: no se aplica y se declara", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f3", .precision_ventas())
  regla <- list(lupa_monto = ~ lupa_activo)
  # `perfilar()` rechaza la regla entera.
  expect_error(suppressWarnings(perfilar(
    .precision_ventas(), fecha = .precision_fecha, aplicabilidad = regla
  )))
  corrida <- .precision_con_avisos(perfilar_dbi(
    con, "lupa_precision_f3", fecha = .precision_fecha, aplicabilidad = regla
  ))
  con_regla <- corrida$valor
  sin_regla <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f3", fecha = .precision_fecha
  ))
  # La muestra se perfila, y la regla no toca ninguna columna: ni la que
  # nombra ni la que la lleva.
  expect_false(is.null(con_regla$perfil_muestra))
  expect_identical(.precision_fila_muestra(con_regla, "lupa_activo"),
                   .precision_fila_muestra(sin_regla, "lupa_activo"))
  expect_identical(.precision_fila_muestra(con_regla, "lupa_monto"),
                   .precision_fila_muestra(sin_regla, "lupa_monto"))
  # Se declara para su columna, con como escribirla, y se avisa una vez.
  propia <- .precision_cobertura_regla(con_regla, "lupa_monto")
  expect_identical(nrow(propia), 1L)
  expect_identical(propia$estado, "degradado")
  expect_match(propia$como_resolverlo, "~ lupa_activo == 1", fixed = TRUE)
  expect_identical(.precision_avisos_de_regla(corrida$avisos), 1L)
  # De una regla que no es una columna sola no se sabe que comparacion quiso
  # decir: se declara igual, sin proponer una.
  corrida <- .precision_con_avisos(perfilar_dbi(
    con, "lupa_precision_f3", fecha = .precision_fecha,
    aplicabilidad = list(lupa_monto = ~ lupa_activo + 1)
  ))
  propia <- .precision_cobertura_regla(corrida$valor, "lupa_monto")
  expect_identical(nrow(propia), 1L)
  expect_false(grepl("== 1", propia$como_resolverlo, fixed = TRUE, useBytes = TRUE))
  expect_identical(.precision_avisos_de_regla(corrida$avisos), 1L)
  expect_false(any(vapply(corrida$avisos, function(w) {
    grepl("== 1", conditionMessage(w), fixed = TRUE, useBytes = TRUE)
  }, logical(1L))))
})

test_that("regla no logica: el control, una regla logica se sigue aplicando", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f3c", .precision_ventas())
  regla <- list(lupa_monto = ~ lupa_activo == 1)
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_f3c", fecha = .precision_fecha, aplicabilidad = regla
  ))
  memoria <- suppressWarnings(perfilar(
    .precision_ventas(), fecha = .precision_fecha, aplicabilidad = regla
  ))
  columnas <- perfil$perfil_muestra$columnas
  expect_identical(
    columnas$n_distintos[columnas$columna == "lupa_monto"],
    memoria$columnas$n_distintos[memoria$columnas$columna == "lupa_monto"]
  )
  cobertura <- perfil$resumen_tabla$cobertura
  expect_identical(
    sum(cobertura$bloque == "perfil_muestra" & cobertura$elemento == "lupa_monto"),
    0L
  )
})

test_that("regla no logica: un periodo con una regla asi no se cuenta", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f3p", .precision_ventas())
  regla <- list(lupa_hasta = ~ lupa_activo)
  expect_error(.precision_memoria(.precision_ventas(), aplicabilidad = regla))
  for (estrategia in c("no_medible", "bloques")) {
    fila <- .precision_motor(con, "lupa_precision_f3p", aplicabilidad = regla,
                             estrategia_no_sql = estrategia)
    expect_identical(fila$estado, "no_disponible", label = estrategia)
    expect_true(all(is.na(unlist(fila[.precision_cifras]))), label = estrategia)
    expect_match(fila$motivo, "~ lupa_activo == 1", fixed = TRUE, label = estrategia)
  }
})

# Una columna cuyo tipo el motor no declara: la expresion de una vista de SQLite
# llega sin filas como logica y con filas como entero. Solo al leer se sabe que
# la regla que la nombra no da un valor logico.
.precision_vista_sin_tipo <- function(con, base, vista) {
  DBI::dbWriteTable(con, base, .precision_ventas())
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", vista, " AS SELECT lupa_id, lupa_activo = 1 AS lupa_marca, ",
    "lupa_monto, lupa_desde, lupa_hasta FROM ", base
  ))
}

test_that("regla no logica: lo que solo las filas muestran tampoco deja a la tabla sin perfil de muestra", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .precision_vista_sin_tipo(con, "lupa_precision_f3b", "lupa_precision_f3v")
  expect_true(is.logical(DBI::dbGetQuery(
    con, "SELECT lupa_marca FROM lupa_precision_f3v WHERE 1 = 0"
  )$lupa_marca))
  expect_true(is.integer(DBI::dbGetQuery(
    con, "SELECT lupa_marca FROM lupa_precision_f3v"
  )$lupa_marca))
  regla <- list(lupa_monto = ~ lupa_marca)
  vias <- list(
    muestra = list(),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 100)
  )
  for (via in names(vias)) {
    correr <- function(...) {
      do.call(perfilar_dbi, c(list(
        con, "lupa_precision_f3v", fecha = .precision_fecha, ...
      ), vias[[via]]))
    }
    corrida <- .precision_con_avisos(correr(aplicabilidad = regla))
    con_regla <- corrida$valor
    sin_regla <- suppressWarnings(correr())
    expect_false(is.null(con_regla$perfil_muestra), label = via)
    expect_identical(.precision_fila_muestra(con_regla, "lupa_monto"),
                     .precision_fila_muestra(sin_regla, "lupa_monto"), label = via)
    expect_identical(.precision_fila_muestra(con_regla, "lupa_marca"),
                     .precision_fila_muestra(sin_regla, "lupa_marca"), label = via)
    propia <- .precision_cobertura_regla(con_regla, "lupa_monto")
    expect_identical(nrow(propia), 1L, label = via)
    expect_identical(propia$estado, "degradado", label = via)
    expect_match(propia$como_resolverlo, "~ lupa_marca == 1", fixed = TRUE, label = via)
    expect_identical(.precision_avisos_de_regla(corrida$avisos), 1L, label = via)
  }
})

test_that("regla no logica: el periodo por bloques declara la que solo las filas muestran", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .precision_vista_sin_tipo(con, "lupa_precision_f3d", "lupa_precision_f3w")
  fila <- .precision_motor(
    con, "lupa_precision_f3w", aplicabilidad = list(lupa_hasta = ~ lupa_marca),
    estrategia_no_sql = "bloques"
  )
  expect_identical(fila$estado, "no_disponible")
  expect_true(all(is.na(unlist(fila[.precision_cifras]))))
  expect_match(fila$motivo, "~ lupa_marca == 1", fixed = TRUE)
})

# Una regla valida puede no mostrar su clase sin filas: `sapply()` sobre cero
# elementos da una lista. No se la toma por una regla que no da un valor logico.
test_that("regla no logica: una regla valida que sin filas no muestra su clase se sigue aplicando", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f3s", .precision_ventas())
  predicado <- ~ sapply(lupa_activo, function(v) isTRUE(v == 1))
  regla <- list(lupa_monto = predicado)
  memoria <- suppressWarnings(perfilar(
    .precision_ventas(), fecha = .precision_fecha, aplicabilidad = regla
  ))
  sin_regla <- suppressWarnings(perfilar(.precision_ventas(), fecha = .precision_fecha))
  distintos <- function(columnas) columnas$n_distintos[columnas$columna == "lupa_monto"]
  expect_lt(distintos(memoria$columnas), distintos(sin_regla$columnas))
  corrida <- .precision_con_avisos(perfilar_dbi(
    con, "lupa_precision_f3s", fecha = .precision_fecha, aplicabilidad = regla
  ))
  expect_identical(distintos(corrida$valor$perfil_muestra$columnas),
                   distintos(memoria$columnas))
  expect_identical(nrow(.precision_cobertura_regla(corrida$valor, "lupa_monto")), 0L)
  expect_identical(.precision_avisos_de_regla(corrida$avisos), 0L)
  # Y el periodo por bloques la aplica como la memoria.
  regla_periodo <- list(lupa_hasta = predicado)
  en_memoria <- .precision_memoria(.precision_ventas(), aplicabilidad = regla_periodo)
  expect_lt(en_memoria$n, 12)
  fila <- .precision_motor(con, "lupa_precision_f3s", aplicabilidad = regla_periodo,
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(fila[.precision_cifras], en_memoria[.precision_cifras])
})

# Decidir si una regla da un valor logico la evalua una vez mas. Lo que la
# regla avise al evaluarse no se repite por eso.
test_that("regla no logica: decidir sobre una regla no repite sus avisos", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_f3a", .precision_ventas())
  lupa_precision_avisa <- function(v) {
    warning("lupa_precision_regla_evaluada")
    v == 1
  }
  regla <- list(lupa_monto = ~ lupa_precision_avisa(lupa_activo))
  de_la_regla <- function(avisos) {
    sum(vapply(avisos, function(w) {
      grepl("lupa_precision_regla_evaluada", conditionMessage(w),
            fixed = TRUE, useBytes = TRUE)
    }, logical(1L)))
  }
  en_memoria <- .precision_con_avisos(perfilar(
    .precision_ventas(), fecha = .precision_fecha, aplicabilidad = regla
  ))
  en_motor <- .precision_con_avisos(perfilar_dbi(
    con, "lupa_precision_f3a", fecha = .precision_fecha, aplicabilidad = regla
  ))
  expect_false(is.null(en_motor$valor$perfil_muestra))
  expect_gt(de_la_regla(en_memoria$avisos), 0L)
  expect_identical(de_la_regla(en_motor$avisos), de_la_regla(en_memoria$avisos))
})

# ---- Los topes cortan la lectura ---------------------------------------------

test_that("B5: pasado el tope, el periodo se declara y la lectura se corta", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .TOPE_FILAS_TEXTO_FECHAS_DBI = 20L, .BLOQUE_FECHAS_DBI = 7L,
    .package = "lupa"
  )
  leidas <- .precision_contar_filas(7L)
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_b5", data.frame(
    lupa_desde = rep("2020-01-01", 100),
    lupa_hasta = rep("2020-02-01", 100), stringsAsFactors = FALSE
  ))
  fila <- .precision_motor(con, "lupa_precision_b5", estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "20 filas", fixed = TRUE)
  # Tres bloques de siete filas cruzan el tope de veinte: el cuarto no se pide.
  expect_identical(leidas(), 21)
})

test_that("B5: pasado el tope, el resumen de un texto se declara y la lectura se corta", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .TOPE_FILAS_TEXTO_FECHAS_DBI = 20L, .BLOQUE_FECHAS_DBI = 7L,
    .package = "lupa"
  )
  leidas <- .precision_contar_filas(7L)
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_b5t", data.frame(
    lupa_alta = rep(c("2020-01-01", "2021-06-01"), 50), stringsAsFactors = FALSE
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_b5t", fecha = .precision_fecha, resumen_fechas = TRUE,
    estrategia_no_sql = "bloques"
  ))
  fila <- .precision_fila_fechas(perfil, "lupa_alta")
  expect_identical(fila$estado, "no_disponible")
  expect_identical(leidas(), 21)
})

test_that("B5: pasado el tope de formas distintas, el resumen de un texto deja de leer", {
  .precision_saltar("sqlite")
  testthat::local_mocked_bindings(
    .TOPE_VOCABULARIO_FECHAS_DBI = 5L, .BLOQUE_FECHAS_DBI = 7L,
    .package = "lupa"
  )
  leidas <- .precision_contar_filas(7L)
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "lupa_precision_b5v", data.frame(
    lupa_alta = as.character(as.Date("2020-01-01") + 0:99), stringsAsFactors = FALSE
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_b5v", fecha = .precision_fecha, resumen_fechas = TRUE,
    estrategia_no_sql = "bloques"
  ))
  expect_identical(.precision_fila_fechas(perfil, "lupa_alta")$estado, "no_disponible")
  # El primer bloque ya trae mas de cinco formas: el segundo no se pide.
  expect_identical(leidas(), 7)
})

test_that("con periodos y sin resumen_fechas no se consulta ni se lee el resumen de fechas", {
  .precision_saltar("sqlite")
  con <- .precision_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Las consultas emitidas son las que anuncia el plan.
  DBI::dbWriteTable(con, "lupa_precision_plan_fechas", .precision_contratos())
  plan <- suppressWarnings(plan_perfilado_dbi(
    con, "lupa_precision_plan_fechas", periodos = .precision_par
  ))
  perfil <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_plan_fechas", fecha = .precision_fecha,
    periodos = .precision_par
  ))
  expect_identical(perfil$resumen_tabla$meta$consultas$emitidas, attr(plan, "total"))
  # Y con `"bloques"`, las columnas de texto se leen una sola vez: la del periodo.
  testthat::local_mocked_bindings(.BLOQUE_FECHAS_DBI = 7L, .package = "lupa")
  leidas <- .precision_contar_filas(7L)
  datos <- .precision_como_texto(.precision_contratos())
  DBI::dbWriteTable(con, "lupa_precision_una_lectura", datos)
  fila <- .precision_motor(con, "lupa_precision_una_lectura",
                           estrategia_no_sql = "bloques")
  expect_identical(fila$estado, "calculado")
  expect_identical(leidas(), nrow(datos) + 0)
})

# ---- DuckDB -------------------------------------------------------------------

# DuckDB entrega `infinity` como la fecha mas lejana que cabe en el tipo de la
# columna, un numero finito, y asi lo cuenta `perfilar()` sobre lo que R lee de
# la tabla. La memoria de estas pruebas es esa lectura, no una tabla armada en
# R: armarla con `as.Date(Inf)` es probar contra algo que el controlador no
# entrega.
.precision_infinitos <- function(con, tabla, tipo_desde, tipo_hasta) {
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (lupa_id INTEGER, lupa_desde ", tipo_desde,
    ", lupa_hasta ", tipo_hasta, ")"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    "(1, '2020-01-01', '2020-06-01'), (2, 'infinity', 'infinity'), ",
    "(3, '2020-03-01', '2020-02-01'), (4, NULL, '2020-05-01'), ",
    "(5, '2020-04-01', 'infinity'), (6, 'infinity', '2020-07-01'), ",
    "(7, '-infinity', '2020-08-01'), (8, '2020-09-01', '-infinity'), ",
    "(9, '-infinity', 'infinity'), (10, '-infinity', '-infinity'), ",
    "(11, 'infinity', NULL), (12, NULL, 'infinity')"
  ))
  DBI::dbReadTable(con, tabla)
}

test_that("DuckDB: un infinity se cuenta como R lo lee si las dos columnas guardan lo mismo", {
  .precision_saltar("duckdb")
  con <- .precision_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  pares <- list(
    fecha = c("DATE", "DATE"), instante = c("TIMESTAMP", "TIMESTAMP"),
    con_huso = c("TIMESTAMP", "TIMESTAMPTZ")
  )
  for (par in names(pares)) {
    tabla <- paste0("lupa_precision_infinito_", par)
    leido <- .precision_infinitos(con, tabla, pares[[par]][[1L]], pares[[par]][[2L]])
    # R no lee un infinito: lee una fecha, y la cuenta como tal.
    presentes <- unclass(leido$lupa_hasta)[!is.na(leido$lupa_hasta)]
    expect_identical(length(presentes), 11L, label = par)
    expect_true(all(is.finite(presentes)), label = par)
    memoria <- .precision_memoria(leido)
    expect_identical(memoria$n_cerrados_validos, 6, label = par)
    expect_identical(memoria$n_fin_antes_inicio, 3, label = par)
    for (estrategia in c("no_medible", "bloques")) {
      fila <- .precision_motor(con, tabla, estrategia_no_sql = estrategia)
      expect_identical(fila$estado, "calculado", label = paste(par, estrategia))
      expect_identical(fila[.precision_cifras], memoria[.precision_cifras],
                       label = paste(par, estrategia))
    }
  }
})

test_that("DuckDB: un infinity entre columnas de tipos distintos no lo cuenta el motor", {
  .precision_saltar("duckdb")
  con <- .precision_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  leido <- .precision_infinitos(con, "lupa_precision_infinito_mixto", "DATE", "TIMESTAMP")
  # En R el infinito de una fecha y el de un instante son dos fechas distintas;
  # para el motor son iguales. La fila con los dos infinitos se cuenta distinto.
  memoria <- .precision_memoria(leido)
  expect_identical(memoria$n_cerrados_validos, 5)
  expect_identical(memoria$n_fin_antes_inicio, 4)
  declarado <- .precision_motor(con, "lupa_precision_infinito_mixto")
  expect_identical(declarado$estado, "no_disponible")
  expect_true(all(is.na(unlist(declarado[.precision_cifras]))))
  expect_match(declarado$motivo, "infinita", fixed = TRUE)
  contado <- .precision_motor(con, "lupa_precision_infinito_mixto",
                              estrategia_no_sql = "bloques")
  expect_identical(contado$estado, "calculado")
  expect_identical(contado[.precision_cifras], memoria[.precision_cifras])
  # El control: sin infinitos, los mismos dos tipos los cuenta el motor.
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_precision_infinito_control AS SELECT * FROM",
    "lupa_precision_infinito_mixto WHERE lupa_id IN (1, 3, 4)"
  ))
  control <- .precision_motor(con, "lupa_precision_infinito_control")
  expect_identical(control$estado, "calculado")
  expect_identical(
    control[.precision_cifras],
    .precision_memoria(DBI::dbReadTable(con, "lupa_precision_infinito_control"))[.precision_cifras]
  )
})

test_that("DuckDB: con el huso forzado no se cuenta por literales, y por bloques se cuenta lo que lee R", {
  .precision_saltar("duckdb")
  con <- .precision_conectar(
    "duckdb", timezone_out = "America/Montevideo", tz_out_convert = "force"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_precision_force (lupa_desde TIMESTAMP, lupa_hasta TIMESTAMP)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_precision_force VALUES",
    "(TIMESTAMP '2020-01-01 00:00:00', TIMESTAMP '2020-03-01 00:00:00'),",
    "(TIMESTAMP '2020-02-01 00:00:00', TIMESTAMP '9999-12-31 00:00:00'),",
    "(TIMESTAMP '2020-03-01 00:00:00', TIMESTAMP '9999-12-31 00:00:00'),",
    "(TIMESTAMP '2020-04-01 00:00:00', TIMESTAMP '9999-12-31 00:00:00'),",
    "(TIMESTAMP '2025-12-31 22:00:00', TIMESTAMP '2025-12-31 23:00:00'),",
    "(TIMESTAMP '2020-05-01 00:00:00', NULL)"
  ))
  periodo <- list(inicio = "lupa_desde", fin = "lupa_hasta", fin_abierto = "9999-12-31")
  # Lo que R lee por esta conexion no son los instantes que guarda el motor: el
  # fin abierto ya no es la fecha declarada, y hay fechas que pasan a futuras.
  leido <- DBI::dbReadTable(con, "lupa_precision_force")
  memoria <- suppressWarnings(perfilar(
    leido, fecha = .precision_fecha, analizar_dependencias = FALSE,
    resumen_fechas = TRUE, periodos = periodo
  ))
  expect_identical(memoria$general$periodos$n_fin_abierto_centinela, 0)
  declarado <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_force", fecha = .precision_fecha,
    resumen_fechas = TRUE, periodos = periodo
  ))
  expect_identical(.precision_periodo(declarado)$estado, "no_disponible")
  expect_identical(
    .precision_fila_fechas(declarado, "lupa_hasta", "n_fechas_futuras")$estado,
    "no_disponible"
  )
  contado <- suppressWarnings(perfilar_dbi(
    con, "lupa_precision_force", fecha = .precision_fecha,
    resumen_fechas = TRUE, periodos = periodo, estrategia_no_sql = "bloques"
  ))
  expect_identical(.precision_periodo(contado)[.precision_cifras],
                   memoria$general$periodos[.precision_cifras])
  columnas <- contado$resumen_tabla$columnas
  expect_identical(
    columnas$n_fechas_futuras[columnas$columna == "lupa_hasta"],
    memoria$columnas$n_fechas_futuras[memoria$columnas$columna == "lupa_hasta"]
  )
  # Y el plan no preve la consulta por literales de ninguno de los dos.
  plan <- suppressWarnings(plan_perfilado_dbi(
    con, "lupa_precision_force", resumen_fechas = TRUE, periodos = periodo
  ))
  expect_identical(
    plan$n_consultas[plan$clase_consulta == "resumen de fechas (cuentas contra literales)"], 0
  )
  expect_identical(
    plan$n_consultas[plan$clase_consulta == "periodos (particion contra literales)"], 0
  )
})
