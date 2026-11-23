# El resumen de fechas (`resumen_fechas = TRUE`), las fechas centinela que se
# declaran para toda la tabla (`centinelas_fecha`) y los periodos (`periodos`),
# en memoria y en el motor. La promesa es una sola: con los mismos datos tal
# como R los lee, `perfilar()` y `perfilar_dbi()` publican las mismas cuentas,
# o el motor declara por que no puede.

.fm_fecha <- as.POSIXct("2026-11-22 12:00:00", tz = "UTC")

.fm_conectar <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

.fm_saltar <- function(motor) {
  skip_on_cran()
  skip_if_not_installed("DBI")
  if (identical(motor, "sqlite")) skip_if_not_installed("RSQLite")
  if (identical(motor, "duckdb")) skip_if_not_installed("duckdb")
}

# Las dos vias sobre la misma tabla: memoria sobre lo que R lee del motor.
.fm_dos_vias <- function(con, tabla = "t", ..., estrategia_no_sql = "no_medible",
                         fecha = .fm_fecha) {
  leido <- suppressWarnings(DBI::dbReadTable(con, tabla))
  memoria <- suppressWarnings(perfilar(
    leido, nombre = tabla, fecha = fecha, resumen_fechas = TRUE,
    analizar_dependencias = FALSE, ...
  ))
  motor <- suppressWarnings(perfilar_dbi(
    con, tabla, fecha = fecha, resumen_fechas = TRUE,
    estrategia_no_sql = estrategia_no_sql, ...
  ))
  list(memoria = memoria, motor = motor)
}

.fm_campo <- function(x, columna, campo) {
  columnas <- if (inherits(x, "perfil_dbi")) x$resumen_tabla$columnas else x$columnas
  columnas[[campo]][[match(columna, columnas$columna)]]
}

# Las diferencias reales entre las dos vias en los campos del resumen: un `NA`
# de los dos lados no es una diferencia, y lo que el motor declara tampoco.
.fm_diferencias <- function(vias) {
  e <- as.data.frame(comparar_equivalencia(vias$memoria, vias$motor, tolerancia = 0))
  e <- e[e$campo %in% .CAMPOS_RESUMEN_FECHAS, , drop = FALSE]
  e[e$veredicto != "identico" & !(e$motivo %in% "sin_valor_medible_en_ambos"),
    , drop = FALSE]
}

.fm_estado_sql <- function(perfil_dbi, columna, metrica = "n_fechas_centinela") {
  s <- perfil_dbi$resumen_tabla$sql
  s$estado[s$columna == columna & s$metrica == metrica]
}

.fm_hallazgos <- function(perfil, columna, tipo = "fecha_centinela") {
  h <- perfil$hallazgos
  h <- h[which(h$columna == columna & h$tipo_hallazgo == tipo), , drop = FALSE]
  stats::setNames(h$n_afectados, as.character(h$severidad))
}

# Nacimientos entre 1940 y 2005 con 30 `1900-01-01` y 10 `0001-01-01`.
.fm_nacimientos <- function(semilla = 3) {
  set.seed(semilla)
  n <- 1000
  nac <- as.Date("1940-01-01") + sample(0:23000, n, TRUE)
  nac[1:30] <- as.Date("1900-01-01")
  nac[31:40] <- as.Date("0001-01-01")
  data.frame(fecha_nacimiento = nac)
}

test_that("A1: las dos vias comparan el instante exacto, no el dia civil", {
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    set.seed(3)
    t <- as.POSIXct("2010-01-01", tz = "UTC") + sample(0:1e8, 400)
    t[1:4] <- as.POSIXct("1970-01-01 12:00:00", tz = "UTC")
    t[5:8] <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
    DBI::dbWriteTable(con, "t", data.frame(fecha_alta = t))
    vias <- .fm_dos_vias(con, dominios = list(fecha_alta = list(centinelas = "1970-01-01")))
    expect_identical(.fm_campo(vias$motor, "fecha_alta", "n_fechas_centinela"), 4)
    expect_identical(.fm_campo(vias$memoria, "fecha_alta", "n_fechas_centinela"), 4)
    expect_equal(nrow(.fm_diferencias(vias)), 0L)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("A2: una centinela escrita con hora vale lo mismo en las dos vias", {
  .fm_saltar("duckdb")
  con <- .fm_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(4)
  t <- as.POSIXct("2010-01-01", tz = "UTC") + sample(0:1e8, 300)
  t[1:5] <- as.POSIXct("1980-01-01 00:00:00", tz = "UTC")
  DBI::dbWriteTable(con, "t", data.frame(fecha_alta = t))
  # La hora del texto se descarta al leer la centinela (`.dominio_fecha_numero()`):
  # la declarada es la medianoche, en memoria y en el motor.
  vias <- .fm_dos_vias(con, centinelas_fecha = "1980-01-01 12:00:00")
  expect_identical(.fm_campo(vias$motor, "fecha_alta", "n_fechas_centinela_declaradas"), 5)
  expect_equal(nrow(.fm_diferencias(vias)), 0L)
})

test_that("A3: centinelas_fecha se suma sin quitar la reserva de las incorporadas", {
  set.seed(5)
  f <- as.Date("2000-01-01") + sample(0:5000, 300, TRUE)
  f[1] <- as.Date("1900-01-01")
  f[2:3] <- as.Date("1970-01-01")
  f[4] <- as.Date("0001-01-01")
  p <- suppressWarnings(perfilar(
    data.frame(fecha_alta = f), fecha = .fm_fecha, analizar_dependencias = FALSE,
    centinelas_fecha = "0001-01-01", resumen_fechas = TRUE
  ))
  expect_identical(.fm_hallazgos(p, "fecha_alta"), c(error = 1))
  expect_identical(.fm_campo(p, "fecha_alta", "n_fechas_centinela"), 1)
  expect_identical(.fm_campo(p, "fecha_alta", "n_fechas_centinela_declaradas"), 1)
})

test_that("A4 y N8: solo lo declarado abre el fin; una centinela conocida se avisa aparte", {
  set.seed(4)
  ini <- as.Date("2015-01-01") + sample(0:2000, 200, TRUE)
  fin <- ini + 30
  fin[1:20] <- as.Date("9999-12-31")
  fin[21:25] <- as.Date("1900-01-01")
  fin[26:40] <- NA
  fin[41] <- as.Date("1970-01-01")
  datos <- data.frame(fecha_inicio = ini, fecha_fin = fin)
  par <- c(inicio = "fecha_inicio", fin = "fecha_fin")
  p <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, analizar_dependencias = FALSE,
                                 periodos = par))
  tabla <- p$general$periodos
  expect_identical(tabla$n_fin_abierto_centinela, 0)
  expect_identical(tabla$n_fin_centinela_sin_declarar, 26)
  expect_identical(tabla$n_fin_abierto_nulo, 15)
  expect_identical(tabla$n_fin_antes_inicio, 0)
  expect_match(tabla$aviso, "Posible fin abierto sin declarar", fixed = TRUE)
  suma <- tabla$n_cerrados_validos + tabla$n_fin_antes_inicio + tabla$n_fin_abierto_nulo +
    tabla$n_fin_abierto_centinela + tabla$n_fin_centinela_declarado +
    tabla$n_fin_centinela_sin_declarar + tabla$n_sin_inicio_con_fin +
    tabla$n_ambos_ausentes_o_centinela
  expect_identical(suma, tabla$n)
  abierto <- suppressWarnings(perfilar(
    datos, fecha = .fm_fecha, analizar_dependencias = FALSE,
    periodos = list(inicio = "fecha_inicio", fin = "fecha_fin", fin_abierto = "9999-12-31")
  ))$general$periodos
  expect_identical(abierto$n_fin_abierto_centinela, 20)
  expect_identical(abierto$n_fin_centinela_sin_declarar, 6)
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    DBI::dbWriteTable(con, "t", datos)
    d <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, periodos = par))
    columnas <- grep("^n", names(tabla), value = TRUE)
    expect_equal(unlist(d$resumen_tabla$meta$periodos[columnas]), unlist(tabla[columnas]))
    expect_identical(d$resumen_tabla$meta$periodos$aviso, tabla$aviso)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("M1: la actualidad saca solo lo declarado y lo futuro", {
  datos <- data.frame(fecha_dato = as.Date(c(
    "2020-01-01", "2024-05-01", "2025-06-01", "2025-06-01", "2027-01-01",
    "1900-01-01", "1900-01-01", "1900-01-01", as.character(as.Date("2010-01-01") + 0:40)
  )))
  p <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, analizar_dependencias = FALSE,
                                 resumen_fechas = TRUE, centinelas_fecha = "2025-06-01"))
  actual <- p$general$actualidad$columnas
  expect_identical(actual$ultimo_dato_fecha, "2024-05-01")
  expect_identical(actual$n_futuras, 1)
  expect_identical(actual$n_centinelas_declaradas_no_futuras, 2)
  # Las tres 1900-01-01 son una incorporada: no se declararon y siguen en la cuenta.
  expect_identical(actual$n_evaluadas, nrow(datos) - 3)
})

test_that("M2: futuras por nombre y calendario, igual en las dos vias", {
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    DBI::dbWriteTable(con, "t", data.frame(
      fecha_vencimiento = as.Date("2026-11-22") + c(-5:5, 100),
      fecha_dia = seq(as.Date("2026-11-15"), by = "day", length.out = 12)
    ))
    vias <- .fm_dos_vias(con)
    expect_identical(.fm_campo(vias$motor, "fecha_vencimiento", "lectura_fechas_futuras"), "admitidas")
    expect_identical(.fm_campo(vias$motor, "fecha_dia", "lectura_fechas_futuras"), "calendario")
    expect_length(.fm_hallazgos(vias$memoria, "fecha_vencimiento", "fecha_futura"), 0L)
    expect_equal(nrow(.fm_diferencias(vias)), 0L)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("M3 y N5: el texto leido como fecha se decide con la columna entera", {
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    set.seed(12)
    f <- as.Date("2000-01-01") + sample(0:6000, 600, TRUE)
    ambiguas <- as.POSIXlt(f[1:100])
    ambiguas$mday <- sample(1:12, 100, TRUE)
    f[1:100] <- as.Date(ambiguas)
    f[101:105] <- as.Date("1900-01-01")
    DBI::dbWriteTable(con, "t", data.frame(fecha_txt = format(f, "%d/%m/%Y"),
                                           stringsAsFactors = FALSE))
    leido <- DBI::dbReadTable(con, "t")
    memoria <- suppressWarnings(perfilar(leido, fecha = .fm_fecha, resumen_fechas = TRUE,
                                         analizar_dependencias = FALSE))
    sin <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, resumen_fechas = TRUE,
                                         muestra = 100))
    expect_identical(.fm_estado_sql(sin, "fecha_txt"), "no_disponible")
    con_bloques <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, resumen_fechas = TRUE,
                                                 muestra = 100, estrategia_no_sql = "bloques"))
    expect_identical(.fm_campo(memoria, "fecha_txt", "n_fechas_centinela"), 5)
    expect_identical(.fm_campo(con_bloques, "fecha_txt", "n_fechas_centinela"), 5)
    expect_identical(.fm_estado_sql(con_bloques, "fecha_txt"), "calculado")
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("M4 y N7: sin los argumentos nuevos la salida es la de siempre", {
  .fm_saltar("duckdb")
  con <- .fm_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(fecha_alta = as.Date("2020-01-01") + 0:9, x = 1:10))
  d <- suppressWarnings(perfilar_dbi(con, "t"))
  expect_null(d$resumen_tabla$meta$fecha_hora)
  expect_null(d$resumen_tabla$meta$actualidad)
  expect_false(any(.CAMPOS_RESUMEN_FECHAS %in% names(d$resumen_tabla$columnas)))
  expect_false(any(.CAMPOS_RESUMEN_FECHAS %in% names(d$perfil_muestra$columnas)))
  expect_identical(.METRICAS_DBI, c("validos", "distintos", "moda", "basicos", "mediana", "desvio"))
  plan <- plan_perfilado_dbi(con, "t")
  plan_fechas <- plan_perfilado_dbi(con, "t", resumen_fechas = TRUE)
  expect_identical(attr(plan_fechas, "total"), attr(plan, "total") + 1)
  # Con `resumen_fechas` y sin `fecha`, una sola fecha para el resumen y la
  # muestra, y las dos partes dicen que no se declaro.
  r <- suppressWarnings(perfilar_dbi(con, "t", resumen_fechas = TRUE))
  expect_identical(r$resumen_tabla$meta$fecha_hora, r$perfil_muestra$meta$fecha_hora)
  expect_false(r$resumen_tabla$meta$fecha_declarada)
  expect_false(r$perfil_muestra$meta$fecha_declarada)
  expect_identical(r$resumen_tabla$meta$consultas$emitidas,
                   attr(plan_fechas, "total"))
  # Una abreviatura de `fecha` sigue llegando a `fecha`.
  a <- suppressWarnings(perfilar(data.frame(fecha_alta = as.Date("2020-01-01") + 0:9),
                                 fech = as.Date("2026-11-22"), analizar_dependencias = FALSE))
  expect_identical(a$meta$fecha_hora, as.POSIXct("2026-11-22", tz = "UTC"))
})

test_that("B1: lo que `dominios` declara rige tambien en el resumen del motor", {
  .fm_saltar("sqlite")
  con <- .fm_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(6)
  f <- as.Date("2000-01-01") + sample(0:5000, 200, TRUE)
  f[1] <- as.Date("1990-01-01")
  DBI::dbWriteTable(con, "t", data.frame(fecha_alta = f))
  vias <- .fm_dos_vias(con, dominios = list(fecha_alta = list(centinelas = "1990-01-01")))
  expect_identical(.fm_campo(vias$motor, "fecha_alta", "n_fechas_centinela_declaradas"), 1)
  expect_equal(nrow(.fm_diferencias(vias)), 0L)
})

test_that("B2: los periodos se declaran con `periodos` y `vigencia()` no cambia", {
  expect_true(all(c("centinelas_fecha", "periodos", "resumen_fechas") %in% names(formals(perfilar))))
  expect_identical(names(formals(vigencia)), c(
    "columna_actualizacion", "fecha_acceso", "fecha_ultimo_cambio",
    "fecha_limite", "inicio_intervalo", "fin_intervalo", "frecuencia_cambio"
  ))
  expect_error(
    perfilar(data.frame(a = as.Date("2020-01-01")), periodos = c(inicio = "a", fin = "b")),
    "inexistentes"
  )
})

test_that("B3: en una columna Date la clase de la fecha de referencia no cambia la cuenta", {
  f <- data.frame(fecha_dato = as.Date("2026-11-22") + (-10:10))
  a <- suppressWarnings(perfilar(f, fecha = as.Date("2026-11-22"), resumen_fechas = TRUE,
                                 analizar_dependencias = FALSE))
  b <- suppressWarnings(perfilar(f, fecha = .fm_fecha, resumen_fechas = TRUE,
                                 analizar_dependencias = FALSE))
  expect_identical(.fm_campo(a, "fecha_dato", "n_fechas_futuras"), 10)
  expect_identical(.fm_campo(b, "fecha_dato", "n_fechas_futuras"), 10)
})

test_that("N1: las cinco formas de guardar una fecha en SQLite", {
  .fm_saltar("sqlite")
  con <- .fm_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  set.seed(7)
  otras <- format(as.Date("1990-01-01") + sample(0:11000, 197, TRUE))
  formas <- list(
    texto_iso = list(tipo = "DATE", v = c(rep("'1970-01-01'", 3), sprintf("'%s'", otras)), texto = TRUE),
    mezcla = list(tipo = "DATE", v = c(rep("'1970-01-01'", 2), "0", sprintf("'%s'", otras)), texto = TRUE),
    con_t = list(tipo = "DATETIME", v = c(rep("'1970-01-01T00:00:00'", 3),
                                          sprintf("'%s 10:00:00'", otras)), texto = TRUE),
    variantes = list(tipo = "DATE", v = c("'1970-1-1'", "' 1970-01-01'", "'1970-01-01 00:00:00'",
                                          sprintf("'%s'", otras)), texto = TRUE),
    numerica = list(tipo = "DATE", v = c(rep("0", 3), as.character(as.integer(as.Date(otras)))),
                    texto = FALSE)
  )
  for (nombre in names(formas)) {
    forma <- formas[[nombre]]
    DBI::dbExecute(con, "DROP TABLE IF EXISTS s")
    DBI::dbExecute(con, paste0("CREATE TABLE s(fecha_alta ", forma$tipo, ")"))
    DBI::dbExecute(con, paste0("INSERT INTO s VALUES ", paste0("(", forma$v, ")", collapse = ",")))
    declarada <- .fm_dos_vias(con, "s")
    expect_identical(
      .fm_estado_sql(declarada$motor, "fecha_alta"),
      if (forma$texto) "no_disponible" else "calculado", label = nombre
    )
    if (forma$texto) {
      expect_match(declarada$motor$resumen_tabla$sql$motivo[
        declarada$motor$resumen_tabla$sql$metrica == "n_fechas_centinela"
      ], "typeof", fixed = TRUE, label = nombre)
    }
    por_bloques <- .fm_dos_vias(con, "s", estrategia_no_sql = "bloques")
    expect_equal(nrow(.fm_diferencias(por_bloques)), 0L, label = nombre)
    expect_identical(
      .fm_campo(por_bloques$motor, "fecha_alta", "n_fechas_centinela"),
      .fm_campo(por_bloques$memoria, "fecha_alta", "n_fechas_centinela"), label = nombre
    )
  }
})

test_that("N1: medir_dbi() no da por cumplido un NoNulo de fechas guardadas como texto", {
  .fm_saltar("sqlite")
  con <- .fm_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d(f DATE)")
  set.seed(7)
  otras <- format(as.Date("1990-01-01") + sample(0:11000, 17, TRUE))
  DBI::dbExecute(con, paste0("INSERT INTO d VALUES ",
                             paste0("('", c(rep("1970-01-01", 3), otras), "')", collapse = ",")))
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN", valores_nulos = as.Date("1970-01-01")), "d", "f"))
  declarada <- suppressWarnings(medir_dbi(mod, con))
  expect_equal(nrow(as.data.frame(declarada)), 0L)
  cobertura <- attr(declarada, "cobertura_metricas")
  expect_identical(cobertura$estado[[1L]], "no_medible")
  expect_match(cobertura$motivo[[1L]], "typeof", fixed = TRUE)
  en_bloques <- suppressWarnings(medir_dbi(mod, con, estrategia_no_sql = "bloques"))
  en_memoria <- suppressWarnings(medir(mod, list(d = DBI::dbReadTable(con, "d"))))
  expect_equal(as.data.frame(en_bloques)$resultado, mean(as.data.frame(en_memoria)$resultado))
})

test_that("N2: fracciones de segundo y candidatas a medianoche y al mediodia, sin aritmetica", {
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    set.seed(8)
    t <- as.POSIXct("2015-01-01", tz = "UTC") + sample(0:3e8, 300) + runif(300)
    t[1:4] <- as.POSIXct("1970-01-01", tz = "UTC") + 0.5
    t[5:8] <- as.POSIXct("1970-01-01", tz = "UTC")
    t[9:11] <- as.POSIXct("2026-11-22 18:00:00", tz = "UTC")
    DBI::dbWriteTable(con, "t", data.frame(ts_x_captura = t))
    for (fecha in list(.fm_fecha, as.Date("2026-11-22"))) {
      vias <- .fm_dos_vias(con, fecha = fecha)
      expect_equal(nrow(.fm_diferencias(vias)), 0L)
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("N3: una fecha declarada no esconde a una incorporada ni cuenta para su mayoria", {
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    DBI::dbWriteTable(con, "t", .fm_nacimientos())
    vias <- .fm_dos_vias(con, centinelas_fecha = "0001-01-01")
    expect_identical(.fm_hallazgos(vias$memoria, "fecha_nacimiento"),
                     c(sospechoso = 30, error = 10))
    expect_identical(.fm_campo(vias$motor, "fecha_nacimiento", "n_fechas_centinela"), 40)
    expect_identical(.fm_campo(vias$motor, "fecha_nacimiento", "n_fechas_centinela_declaradas"), 10)
    expect_equal(nrow(.fm_diferencias(vias)), 0L)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  set.seed(9)
  mayoria <- data.frame(fecha_nac = c(rep(as.Date("0001-01-01"), 450), rep(as.Date("1900-01-01"), 100),
                                      as.Date("1950-01-01") + sample(0:20000, 450, TRUE)))
  p <- suppressWarnings(perfilar(mayoria, fecha = .fm_fecha, analizar_dependencias = FALSE,
                                 centinelas_fecha = "0001-01-01", resumen_fechas = TRUE))
  expect_identical(.fm_campo(p, "fecha_nac", "lectura_fechas_centinela"), "fila")
})

test_that("N4: una sola definicion de futura para la cuenta, el hallazgo y la actualidad", {
  horas <- as.POSIXct(c("2026-11-21 23:00:00", "2026-11-22 03:00:00", "2026-11-22 13:00:00",
                        "2026-11-22 23:59:59", "2026-11-23 00:00:00", "2026-11-25 10:00:00"), tz = "UTC")
  datos <- data.frame(ts_dato = c(horas, as.POSIXct("2024-01-01", tz = "UTC") + seq(0, 3e7, length.out = 40)))
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    DBI::dbWriteTable(con, "t", datos)
    vias <- .fm_dos_vias(con, fecha = as.Date("2026-11-22"))
    expect_identical(.fm_hallazgos(vias$memoria, "ts_dato", "fecha_futura"), c(sospechoso = 2))
    expect_identical(.fm_campo(vias$motor, "ts_dato", "n_fechas_futuras"), 2)
    expect_identical(.fm_campo(vias$motor, "ts_dato", "ultimo_dato_fecha"), "2026-11-22 23:59:59 UTC")
    expect_identical(vias$motor$resumen_tabla$meta$actualidad$columnas$n_futuras, 2)
    expect_equal(nrow(.fm_diferencias(vias)), 0L)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("N6: los campos del resumen se comparan, y los de la fecha solo con la misma fecha", {
  a <- data.frame(columna = c("x", "y"), n_fechas_centinela = c(3, 0), n_fechas_futuras = c(1, 0))
  b <- a
  b$n_fechas_centinela <- c(4, 0)
  e <- as.data.frame(comparar_equivalencia(a, b, tolerancia = 0))
  expect_identical(as.character(e$veredicto[e$columna == "x" & e$campo == "n_fechas_centinela"]), "materialmente_distinto")
  expect_false("n_fechas_centinela" %in% attr(comparar_equivalencia(a, b, tolerancia = 0), "campos_no_comparables"))
  datos <- data.frame(fecha_dato = as.Date("2026-11-20") + 0:5)
  p1 <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, resumen_fechas = TRUE, analizar_dependencias = FALSE))
  p2 <- suppressWarnings(perfilar(datos, fecha = .fm_fecha + 86400, resumen_fechas = TRUE, analizar_dependencias = FALSE))
  r <- comparar_equivalencia(p1, p2, tolerancia = 0)
  expect_true("n_fechas_futuras" %in% attr(r, "campos_no_comparables"))
  detalle <- attr(r, "detalle_campos_no_comparables")
  expect_identical(detalle$motivo[detalle$campo == "n_fechas_futuras"], "fecha_de_corrida_distinta")
})

test_that("N9: centinelas_fecha llega a cada grupo de perfilar_por() y a cada tabla de una coleccion", {
  set.seed(1)
  datos <- data.frame(fecha_alta = as.Date("2020-01-01") + sample(0:2000, 300, TRUE),
                      grupo = sample(c("a", "b"), 300, TRUE), stringsAsFactors = FALSE)
  datos$fecha_alta[datos$grupo == "a"][1] <- as.Date("1999-09-09")
  datos$fecha_alta[datos$grupo == "b"][1:2] <- as.Date("1999-09-09")
  por <- suppressWarnings(perfilar_por(datos, por = "grupo", min_filas = 10L, analizar_dependencias = FALSE,
                                       centinelas_fecha = "1999-09-09"))
  h <- por[which(por$tipo_hallazgo == "fecha_centinela"), ]
  expect_identical(stats::setNames(h$n_afectados, h$grupo), c(a = 1, b = 2))
  expect_true(all(as.character(h$severidad) == "error"))
  .fm_saltar("sqlite")
  con <- .fm_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  col <- suppressWarnings(perfilar_coleccion(coleccion(con, "t"), fecha = .fm_fecha,
                                             centinelas_fecha = "1999-09-09", resumen_fechas = TRUE,
                                             conservar_perfiles = TRUE))
  expect_true(length(col$perfiles) >= 1L)
  resumen <- col$perfiles[[1L]]$resumen_tabla$columnas
  expect_identical(resumen$n_fechas_centinela_declaradas[resumen$columna == "fecha_alta"], 3)
})

test_that("los microsegundos junto a una centinela lejana se declaran en DuckDB", {
  .fm_saltar("duckdb")
  con <- .fm_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t(ts_dato TIMESTAMP)")
  otras <- format(as.POSIXct("2015-01-01", tz = "UTC") + (1:60) * 86400 * 30, "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con, paste0(
    "INSERT INTO t VALUES (TIMESTAMP '9999-12-31 00:00:00'), ",
    "(TIMESTAMP '9999-12-31 00:00:00.000001'), (TIMESTAMP '9999-12-31 00:00:00.00001'), ",
    paste0("(TIMESTAMP '", otras, "')", collapse = ", ")
  ))
  vias <- .fm_dos_vias(con)
  # R lee los tres como 9999-12-31 y los acusa; el motor no puede contarlos asi.
  expect_identical(.fm_campo(vias$memoria, "ts_dato", "n_fechas_centinela"), 3)
  expect_identical(.fm_estado_sql(vias$motor, "ts_dato"), "no_disponible")
  bloques <- .fm_dos_vias(con, estrategia_no_sql = "bloques")
  expect_identical(.fm_campo(bloques$motor, "ts_dato", "n_fechas_centinela"), 3)
})

test_that("DATE 'infinity' de DuckDB se cuenta como R lo lee", {
  .fm_saltar("duckdb")
  con <- .fm_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t(fecha_dato DATE)")
  DBI::dbExecute(con, paste(
    "INSERT INTO t VALUES (DATE 'infinity'), (DATE '-infinity'), (DATE '2020-01-01'),",
    "(DATE '1900-01-01'), (DATE '1900-01-01'), (DATE '2021-01-01'), (DATE '2022-01-01')"
  ))
  vias <- .fm_dos_vias(con)
  expect_identical(.fm_campo(vias$motor, "fecha_dato", "n_fechas_futuras"), 1)
  expect_equal(nrow(.fm_diferencias(vias)), 0L)
})

test_that("la validacion de los argumentos nuevos", {
  d <- data.frame(fecha_alta = as.Date("2020-01-01") + 0:3)
  expect_error(perfilar(d, centinelas_fecha = NA_character_), "centinelas_fecha")
  expect_error(perfilar(d, centinelas_fecha = "no es fecha"), "ilegible")
  expect_error(perfilar(d, centinelas_fecha = 1), "centinelas_fecha")
  expect_error(perfilar(d, resumen_fechas = NA), "resumen_fechas")
  expect_error(perfilar(d, periodos = c(inicio = "fecha_alta", fin = "fecha_alta")), "distintas")
})

test_that("sin muestra el texto se declara, y sin valores no se publica el ultimo dato", {
  .fm_saltar("duckdb")
  con <- .fm_conectar("duckdb")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(fecha_alta = as.Date("2020-01-01") + 0:20,
                                         nota = format(as.Date("2020-01-01") + 0:20),
                                         stringsAsFactors = FALSE))
  d <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, resumen_fechas = TRUE,
                                     bloque_muestra = "solo_agregados"))
  expect_identical(.fm_estado_sql(d, "nota"), "no_disponible")
  expect_identical(.fm_estado_sql(d, "fecha_alta"), "calculado")
  oculto <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, resumen_fechas = TRUE,
                                          incluir_valores = FALSE))
  expect_true(is.na(.fm_campo(oculto, "fecha_alta", "ultimo_dato_fecha")))
  expect_identical(.fm_estado_sql(oculto, "fecha_alta", "ultimo_dato_fecha"), "omitido_por_privacidad")
  expect_true(is.na(oculto$resumen_tabla$meta$actualidad$columnas$antiguedad_dias[[1L]]))
  expect_identical(.fm_campo(oculto, "fecha_alta", "n_fechas_futuras"), 0)
})

test_that("comparar_perfiles() declara un cambio de centinelas_fecha con su fila", {
  datos <- .fm_nacimientos()
  a <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, analizar_dependencias = FALSE))
  b <- suppressWarnings(perfilar(datos, fecha = .fm_fecha + 60, analizar_dependencias = FALSE,
                                 centinelas_fecha = "0001-01-01"))
  r <- suppressWarnings(comparar_perfiles(a, b))
  fila <- r[r$aspecto == "configuracion_centinelas_fecha", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$valor_actual, "0001-01-01", fixed = TRUE)
  igual <- suppressWarnings(comparar_perfiles(b, b))
  expect_false("configuracion_centinelas_fecha" %in% igual$aspecto)
})

# Un contrato por fila: cerrados, abiertos por la marca, sin fin y una inversion.
.fm_contratos <- function() {
  n <- 60
  dia <- as.Date("2025-01-01") + (seq_len(n) * 11) %% 500
  d <- data.frame(id_contrato = seq_len(n), desde = dia, hasta = dia + 90)
  d$hasta[c(3, 9, 15, 21)] <- as.Date("9999-12-31")
  d$hasta[c(4, 10)] <- NA
  d$hasta[5] <- d$desde[5] - 10
  d
}
.fm_periodo <- list(inicio = "desde", fin = "hasta", fin_abierto = "9999-12-31")
.fm_cifras_periodo <- c("n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
                        "n_fin_abierto_centinela", "n_fin_centinela_declarado",
                        "n_fin_centinela_sin_declarar", "n_sin_inicio_con_fin",
                        "n_ambos_ausentes_o_centinela", "prop_fin_abierto")

test_that("la tabla de periodos es el mismo objeto en memoria y en el motor", {
  datos <- .fm_contratos()
  memoria <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, analizar_dependencias = FALSE,
                                       periodos = .fm_periodo))$general$periodos
  expect_type(memoria$n, "double")
  expect_type(memoria$n_cerrados_validos, "double")
  for (motor in c("sqlite", "duckdb")) {
    .fm_saltar(motor)
    con <- .fm_conectar(motor)
    DBI::dbWriteTable(con, "t", datos)
    en_motor <- suppressWarnings(perfilar_dbi(con, "t", fecha = .fm_fecha, periodos = .fm_periodo))
    expect_identical(en_motor$resumen_tabla$meta$periodos, memoria, label = motor)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("un periodo que el motor no cuenta no publica cifras, y con bloques se cuenta en R", {
  .fm_saltar("sqlite")
  # Fechas escritas como texto, en una conexion sin tipos extendidos: el motor
  # tiene texto y R las lee como fechas.
  datos <- data.frame(
    id = 1:6,
    desde = c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01", "2025-05-01", "2025-06-01"),
    hasta = c("2025-01-31", "9999-12-31", NA, "2025-03-01", "2025-05-31", "2025-06-30"),
    stringsAsFactors = FALSE
  )
  memoria <- suppressWarnings(perfilar(datos, fecha = .fm_fecha, analizar_dependencias = FALSE,
                                       periodos = .fm_periodo))$general$periodos
  expect_identical(memoria$n, 6)
  expect_identical(memoria$n_fin_antes_inicio, 1)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", datos)
  declarado <- suppressWarnings(perfilar_dbi(con, "d", fecha = .fm_fecha, periodos = .fm_periodo))
  tabla <- declarado$resumen_tabla$meta$periodos
  expect_identical(tabla$estado, "no_disponible")
  expect_true(all(is.na(unlist(tabla[.fm_cifras_periodo]))))
  expect_match(tabla$motivo, "bloques", fixed = TRUE)
  contado <- suppressWarnings(perfilar_dbi(con, "d", fecha = .fm_fecha, periodos = .fm_periodo,
                                           estrategia_no_sql = "bloques"))
  tabla <- contado$resumen_tabla$meta$periodos
  expect_identical(tabla$estado, "calculado")
  expect_identical(tabla[.fm_cifras_periodo], memoria[.fm_cifras_periodo])
})

test_that("un periodo sobre fechas guardadas como texto en una columna DATE de SQLite", {
  .fm_saltar("sqlite")
  con <- .fm_conectar("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE p(desde DATE, hasta DATE)")
  DBI::dbExecute(con, paste(
    "INSERT INTO p VALUES ('2025-01-01', '2025-01-31'), ('2025-02-01', '9999-12-31'),",
    "('2025-03-01', NULL), ('2025-04-01', '2025-03-01'), ('2025-05-01', '2025-05-31')"
  ))
  memoria <- suppressWarnings(perfilar(DBI::dbReadTable(con, "p"), fecha = .fm_fecha,
                                       analizar_dependencias = FALSE,
                                       periodos = .fm_periodo))$general$periodos
  expect_identical(memoria$n_fin_abierto_centinela, 1)
  declarado <- suppressWarnings(perfilar_dbi(con, "p", fecha = .fm_fecha, periodos = .fm_periodo))
  tabla <- declarado$resumen_tabla$meta$periodos
  expect_identical(tabla$estado, "no_disponible")
  expect_true(all(is.na(unlist(tabla[.fm_cifras_periodo]))))
  expect_match(tabla$motivo, "typeof", fixed = TRUE)
  contado <- suppressWarnings(perfilar_dbi(con, "p", fecha = .fm_fecha, periodos = .fm_periodo,
                                           estrategia_no_sql = "bloques"))
  expect_identical(contado$resumen_tabla$meta$periodos[.fm_cifras_periodo],
                   memoria[.fm_cifras_periodo])
})
