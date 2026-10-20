# Ronda 35-D: las mediciones, refutadas despues de la ronda 34 (y la M1 de la
# refutacion 35-X, que toca la misma funcion de la corroboracion).
#
# Lo que solo se ve contra un motor de contenedor -PostgreSQL 9.3 y 16, SQL
# Server por odbc/FreeTDS- se verifico con sondas y no entra aca: estas pruebas
# usan duckdb y RSQLite en memoria, o los bloques armados a mano.

.f4_O164 <- function(z) {
  readBin(writeBin(as.numeric(z), raw(), size = 4), "double", size = 4,
          n = length(z))
}

.desvio_sql_O164 <- function(p, columna) {
  s <- p$resumen_tabla$sql
  s[s$columna == columna & s$metrica == "desvio", , drop = FALSE]
}

.divergencias_O164 <- function(p) {
  cb <- p$resumen_tabla$cobertura
  as.character(cb$elemento[cb$bloque == "corroboracion" &
                             cb$estado == "divergencia"])
}

test_that("M1: el real de PostgreSQL se lee por su forma corta calculada en el motor, no por el texto de la sesion", {
  skip_if_not_installed("DBI")
  # A mano: 1234501 es exacto en float4 (< 2^24). Antes de PostgreSQL 12 -o con
  # `extra_float_digits <= 0`- el servidor escribe seis cifras, y los 200
  # valores 1234501..1234700 leidos por ese texto son 21 distintos, con un
  # minimo 1234500 que no esta en la columna.
  expect_identical(sprintf("%.6g", 1234501), "1.2345e+06")
  expect_identical(
    length(unique(as.numeric(sprintf("%.6g", 1234500 + 1:200)))), 21L
  )
  expect_identical(min(as.numeric(sprintf("%.6g", 1234500 + 1:200))), 1234500)
  # La forma corta: el primer redondeo a 6, 7, 8 o 9 cifras que vuelve al mismo
  # float4. Es lo que la expresion calcula en el motor.
  corta <- function(x) {
    for (cifras in 6:9) {
      candidato <- signif(x, cifras)
      if (.f4_O164(candidato) == x) return(candidato)
    }
    x
  }
  expect_identical(corta(.f4_O164(1234501)), 1234501)
  expect_identical(corta(.f4_O164(21.3)), 21.3)
  expect_identical(
    length(unique(vapply(.f4_O164(1234500 + 1:200), corta, numeric(1)))), 200L
  )
  # El paquete: sin version conocida -o antes de la 12- la lectura del `real`
  # no pasa por el texto de la sesion; redondea a 6, 7, 8 y 9 cifras.
  con <- structure(list(), class = "PqConnection")
  lectura <- lupa:::.lectura_convertida_dbi(
    con, c("v", "m"), c("\"v\"", "\"m\""), c("float4", "money")
  )
  expr <- lectura$campos_sql[[1L]]
  expect_false(grepl("AS TEXT", expr, fixed = TRUE))
  for (exponente in 5:8) {
    expect_true(grepl(
      paste0("CAST(", exponente, " - FLOOR(LOG(ABS("), expr, fixed = TRUE
    ), info = exponente)
  }
  # El nombre de la proyeccion lo da el `ELSE`, que es la columna.
  expect_true(endsWith(expr, "ELSE CAST(\"v\" AS DOUBLE PRECISION) END"))
  expect_identical(lectura$campos_sql[[2L]], "CAST(\"m\" AS NUMERIC)")
  expect_identical(lectura$convertidas$lectura[[1L]], expr)
  # Desde la 12 el texto va primero -con el valor por omision es la forma corta,
  # la cifra de la ronda 34- y el redondeo queda para cuando no vuelve al valor.
  moderna <- lupa:::.lectura_real_postgresql_dbi("\"v\"", 160015L)
  expect_true(startsWith(
    moderna, "CASE WHEN CAST(CAST(\"v\" AS TEXT) AS REAL) = \"v\" THEN"
  ))
  expect_true(grepl(expr, moderna, fixed = TRUE))
  expect_identical(lupa:::.lectura_real_postgresql_dbi("\"v\"", 90325L), expr)
})

test_that("M2: el desvio centrado de una columna real resta en doble, y una decimal en su tipo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # duckdb resta `FLOAT - DECIMAL` en float4, como SQL Server `real - decimal`:
  # el desvio centrado se apartaba 1,4e-8 del de los valores guardados.
  set.seed(5)
  n <- 400
  v <- round(1000 + stats::runif(n, -10, 10), 3)
  v[sample(n, 2)] <- round(stats::runif(2, 0, 10), 3)
  guardados <- .f4_O164(v)
  # A mano: la media pasa diez veces el desvio, asi que se recentra.
  expect_gt(mean(guardados) / stats::sd(guardados), 10)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v REAL)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO t VALUES ",
    paste0("(", formatC(v, format = "f", digits = 3), ")", collapse = ", ")
  ))
  p <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  rc <- p$resumen_tabla$columnas
  expect_lt(abs(rc$desvio - stats::sd(guardados)) / stats::sd(guardados), 1e-12)
  expect_match(.desvio_sql_O164(p, "v")$sql, "CAST(v AS DOUBLE) - ", fixed = TRUE)
  expect_false("v::desvio" %in% .divergencias_O164(p))
  # SQL Server: un `real` (codigo ODBC 7) se convierte antes de restar y su
  # centro va como literal `float`; un `decimal` (2) resta en su tipo con el
  # literal decimal, que es exacto.
  alias <- function(nombre) "a"
  formas_real <- lupa:::.formas_desvio_dbi(
    "t", "[v]", alias, "microsoft sql server", centro = 995.082487864971,
    tipo = "7"
  )
  expect_true(all(grepl(
    "(CAST([v] AS FLOAT) - 9.9508248786497097e+02)", unlist(formas_real),
    fixed = TRUE
  )))
  formas_decimal <- lupa:::.formas_desvio_dbi(
    "t", "[d]", alias, "microsoft sql server", centro = 995.5, tipo = "2"
  )
  expect_true(all(grepl("([d] - 995.5)", unlist(formas_decimal), fixed = TRUE)))
  # PostgreSQL ya resta un `real` en doble: no cambia.
  formas_pg <- lupa:::.formas_desvio_dbi(
    "t", "\"v\"", alias, "pqconnection postgresql", centro = 995.5,
    tipo = "float4"
  )
  expect_true(all(grepl("(\"v\" - 995.5)", unlist(formas_pg), fixed = TRUE)))
})

test_that("B1: el centro de una columna float de SQL Server no tiene el tope de 38 cifras de un decimal", {
  # A mano: la media 1e40 escrita como decimal tiene 41 cifras, y 1e-25, 39
  # decimales; SQL Server rechazaba la consulta centrada ("maximum precision
  # 38") y quedaba, sin decirlo, el desvio de una pasada.
  expect_identical(nchar(lupa:::.literal_centro_desvio_dbi(1.0000005588543958e40)), 41L)
  alias <- function(nombre) "a"
  for (media in c(1.0000005588543958e40, 1.0000005137130312e-25)) {
    formas <- lupa:::.formas_desvio_dbi(
      "t", "[f]", alias, "microsoft sql server", centro = media, tipo = "8"
    )
    literal <- lupa:::.literal_centro_desvio_dbi(media, flotante = TRUE)
    expect_lte(nchar(literal), 23L)
    expect_identical(as.numeric(literal), media)
    expect_true(all(grepl(paste0("([f] - ", literal, ")"), unlist(formas),
                          fixed = TRUE)))
  }
  # Con coma decimal en la sesion, el literal sigue con punto.
  local({
    anteriores <- options(OutDec = ",")
    on.exit(options(anteriores), add = TRUE)
    expect_identical(
      lupa:::.literal_centro_desvio_dbi(-0.000125, flotante = TRUE),
      "-1.2500000000000000e-04"
    )
  })
})

test_that("B3: un desvio 0 sobre valores distintos se vuelve a pedir centrado aunque los extremos como dobles coincidan", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # 1000 + g * 1e-17: 60 decimales distintos que como dobles son todos 1000.
  DBI::dbExecute(con, paste(
    "CREATE TABLE t AS SELECT CAST('1000.' || lpad(CAST(g AS VARCHAR), 17, '0')",
    "AS DECIMAL(38, 20)) AS v FROM range(1, 61) r(g)"
  ))
  expect_identical(length(unique(1000 + (1:60) * 1e-17)), 1L)
  p <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  # A mano: el desvio de 1..60 por 1e-17.
  esperado <- stats::sd(1:60) * 1e-17
  fila <- .desvio_sql_O164(p, "v")
  expect_identical(fila$estado, "calculado")
  expect_lt(abs(p$resumen_tabla$columnas$desvio - esperado) / esperado, 1e-12)
  expect_match(fila$motivo, "respondio 0 sobre una columna", fixed = TRUE)
  expect_true(lupa:::.riesgo_cancelacion_desvio_dbi(1000, 0, 1000, 1000, 60))
  expect_false(lupa:::.riesgo_cancelacion_desvio_dbi(1000, 0, 1000, 1000, 1))
})

test_that("M3: un decimal desde 2^53 retiene sus metricas de magnitud aunque el controlador no publique la precision", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # A mano: los 60 decimales 90071992547409001..060, cada uno en su doble mas
  # cercano, caen en 4 dobles.
  expect_identical(length(unique(as.numeric(
    paste0("900719925474090", sprintf("%02d", 1:60))
  ))), 4L)
  # RPostgres publica `numeric` sin precision por `.typname`, odbc el codigo
  # ODBC 2 (3 es `money`): los dos son decimales; un doble no.
  expect_identical(
    lupa:::.tipo_declarado_decimal_dbi(list("numeric")), TRUE
  )
  esperados <- c(numeric = TRUE, `2` = TRUE, `3` = TRUE,
                 `decimal(38,2)` = TRUE, `NUMBER(20)` = TRUE,
                 double = FALSE, `8` = FALSE, float4 = FALSE, bigint = FALSE)
  for (tipo in names(esperados)) {
    expect_identical(lupa:::.tipo_declarado_decimal_dbi(tipo), esperados[[tipo]],
                     info = tipo)
  }
  expect_identical(
    lupa:::.tipos_declarados_con_motor_dbi(
      c("double", "double", "integer"), c("numeric", "float8", "int4")
    ),
    c("numeric", "double", "integer")
  )
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE t AS SELECT CAST(90071992547409000 + g AS DECIMAL(38, 2))",
    "AS d FROM range(1, 61) r(g)"
  ))
  p <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  rc <- p$resumen_tabla$columnas
  sql <- p$resumen_tabla$sql
  magnitud <- c("minimo", "maximo", "media", "n_ceros", "n_negativos",
                "mediana", "desvio")
  for (metrica in magnitud) {
    expect_true(is.na(rc[[metrica]]), info = metrica)
    fila <- sql[sql$columna == "d" & sql$metrica == metrica, , drop = FALSE]
    expect_identical(fila$estado, "no_disponible", info = metrica)
    expect_match(fila$motivo, "decimal con un valor de valor absoluto de al menos 2^53",
                 fixed = TRUE, info = metrica)
  }
  # Lo que el motor cuenta sobre el valor exacto se publica.
  expect_identical(as.numeric(rc$n_distintos), 60)
  # La corroboracion no acusa las magnitudes que el resumen retuvo, y si la
  # cuenta de distintos que la muestra hizo sobre los dobles.
  divergencias <- .divergencias_O164(p)
  expect_false(any(paste0("d::", magnitud) %in% divergencias))
  expect_true("d::n_distintos" %in% divergencias)
})

test_that("M3: con bloque_filas, un decimal desde 2^53 no publica distintos ni moda contados sobre dobles", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # SQLite no tiene decimales: el tipo `numeric` que publicaria RPostgres se
  # declara a mano, y los valores llegan como los dobles que entregaria el
  # controlador.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = 1:60, d = 90071992547409000 + 1:60
  ))
  testthat::local_mocked_bindings(
    .tipos_declarados_con_motor_dbi = function(tipos, tipos_motor) {
      c("integer", "numeric")
    },
    .package = "lupa"
  )
  p <- suppressWarnings(perfilar_dbi(
    con, "t", bloque_filas = 20, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  ))
  rc <- p$resumen_tabla$columnas
  sql <- p$resumen_tabla$sql
  retenidas <- c("minimo", "maximo", "media", "n_ceros", "n_negativos",
                 "mediana", "desvio", "n_distintos", "moda", "frecuencia_moda")
  for (metrica in retenidas) {
    expect_true(is.na(rc[[metrica]][rc$columna == "d"]), info = metrica)
    fila <- sql[sql$columna == "d" & sql$metrica == metrica, , drop = FALSE]
    expect_identical(fila$estado, "no_disponible", info = metrica)
  }
  expect_match(sql$motivo[sql$columna == "d" & sql$metrica == "n_distintos"],
               "Con `bloque_filas` tampoco `n_distintos`", fixed = TRUE)
  # Control: el entero chico de al lado se publica.
  expect_identical(as.numeric(rc$n_distintos[rc$columna == "id"]), 60)
})

test_that("M1-X: la corroboracion no compara lo que un bloque no calculo, y lo que si difiere sigue saliendo", {
  corroborar <- function() {
    resumen <- list(
      columnas = data.frame(
        columna = c("datos", "x", "y"), n_distintos = c(NA, 7, 5),
        desvio = c(NA, NA, 1.5), stringsAsFactors = FALSE
      ),
      sql = data.frame(
        columna = c("datos", "x", "x", "y", "y"),
        metrica = c("n_distintos", "n_distintos", "desvio", "n_distintos", "desvio"),
        estado = c("no_disponible", "calculado", "no_disponible", "calculado",
                   "calculado"),
        motivo = c(
          "could not identify an equality operator for type json",
          NA, lupa:::.motivo_nan_dbi("desvio"), NA, NA
        ),
        sql = "SELECT 1", stringsAsFactors = FALSE
      ),
      meta = list(universo = "tabla_completa", filas = 30)
    )
    perfil <- list(
      columnas = data.frame(
        columna = c("datos", "x", "y"), tipo_inferido = c("texto", "doble", "doble"),
        n_distintos = c(3, 7, 7), desvio = c(NA, 2.5, 1.5),
        stringsAsFactors = FALSE
      ),
      meta = list(filas_analizadas = 30)
    )
    lupa:::.cobertura_corroboracion_bloques_dbi(resumen, perfil)
  }
  salida <- corroborar()
  cb <- salida$cobertura
  divergencias <- cb$elemento[cb$bloque == "corroboracion" & cb$estado == "divergencia"]
  # El motor rechazo `COUNT(DISTINCT)` sobre `json`: no hay dos cifras.
  expect_false("datos::n_distintos" %in% divergencias)
  nc <- salida$meta$no_comparadas
  expect_identical(
    nc$causa[nc$columna == "datos" & nc$metrica == "n_distintos"],
    "no_disponible_en_resumen"
  )
  # El `NaN` del motor se sigue comparando (decision documentada), y una
  # diferencia entre dos cifras calculadas tambien.
  expect_true("x::desvio" %in% divergencias)
  expect_true("y::n_distintos" %in% divergencias)
})

test_that("M1-X: la moda de una columna de blobs que la muestra no cuenta no es una divergencia", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, b BLOB)")
  for (g in 1:60) {
    DBI::dbExecute(con, "INSERT INTO t VALUES (?, ?)", params = list(
      g, if (g %% 10 == 0) list(NULL) else list(as.raw(g %% 3))
    ))
  }
  p <- suppressWarnings(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  ))
  pm <- p$perfil_muestra$columnas
  # A mano: la muestra no cuenta la moda de una lista -frecuencia `NA`, no 0-.
  expect_true(is.na(pm$frecuencia_moda[pm$columna == "b"]))
  expect_false(any(c("b::moda", "b::frecuencia_moda") %in% .divergencias_O164(p)))
  nc <- p$resumen_tabla$meta$corroboracion_bloques$no_comparadas
  expect_true(all(c("moda", "frecuencia_moda") %in%
                    nc$metrica[nc$columna == "b" &
                                 grepl("moda_no_calculada_en_muestra", nc$causa)]))
})

# ---- M4 y B2: todos los argumentos de perfilar() en la comparacion ----------

# Para cada argumento que habilita un diagnostico, el valor que lo enciende; y
# para cada vara, uno distinto del de omision. Un argumento nuevo de esas dos
# clases sin su valor aca hace fallar la prueba de abajo.
.encendidos_O164 <- list(
  clave = "id", columnas_sin_ceros = "monto", columnas_no_negativas = "monto",
  columnas_personales = "ciudad", ausencia_estructural = TRUE,
  casi_duplicados_vocabulario = TRUE,
  variantes_equifrecuentes_vocabulario = TRUE,
  duplicados_aproximados = list(columnas = c("ciudad", "depto")),
  aplicabilidad = list(depto = ~ ciudad != "Salto")
)
.apagados_O164 <- list(
  ausencia_estructural = FALSE, casi_duplicados_vocabulario = FALSE,
  variantes_equifrecuentes_vocabulario = FALSE
)
.varas_O164 <- list(
  umbral_faltantes_sospechoso = 0.2, umbral_faltantes_error = 0.5,
  umbral_alta_cardinalidad = 0.9, umbral_patron_dominante = 0.6,
  umbral_patron_raro = 0.01, columnas_sin_ceros = "monto",
  columnas_no_negativas = "monto", columnas_codigo = "id",
  datos_personales_permitidos = FALSE, validadores_personales = FALSE,
  umbral_documento_verificado = 0.95, muestra_validadores = 500L,
  umbral_orden_columnas = 0.99, max_columnas_orden = 5L,
  umbral_solapamiento_orden = 0.2, umbral_aritmetica = 0.95,
  min_filas_aritmetica = 10L, tolerancia_aritmetica = 1e-6,
  max_columnas_aritmetica = 5L, umbral_variante_rara_vocabulario = 0.1,
  min_asimetria_vocabulario_corto = 20, min_asimetria_vocabulario = 3,
  min_participacion_dominante_vocabulario_corto = 0.6,
  max_asimetria_equifrecuente_vocabulario = 3
)

.datos_O164 <- function() {
  n <- 120L
  d <- data.frame(
    id = seq_len(n),
    ciudad = rep(c("Montevideo", "Salto", "Paysandu", "montevideo",
                   "Montevideo "), c(76, 20, 16, 4, 4)),
    depto = rep(c("Canelones", "Canelons"), n / 2),
    monto = round(100 + (seq_len(n) * 37) %% 101, 2),
    stringsAsFactors = FALSE
  )
  d$monto[1:2] <- c(0, -5)
  d$id[2] <- 1L
  d$conyuge <- ifelse(seq_len(n) %% 2 == 0, "x", NA)
  d$estado_civil <- ifelse(is.na(d$conyuge), "soltero", "casado")
  d
}

test_that("M4: cada argumento de perfilar() tiene su lugar en comparar_perfiles()", {
  registro <- lupa:::.ARGUMENTOS_PERFIL_DERIVA
  # La lista es la de los argumentos, ni uno mas ni uno menos.
  expect_setequal(names(registro), names(formals(perfilar)))
  clases <- c("identidad", "politica", "habilita", "vara", "limite",
              "sin_hallazgo")
  for (argumento in names(registro)) {
    x <- registro[[argumento]]
    expect_true(length(x$clase) >= 1L && all(x$clase %in% clases),
                info = argumento)
    if ("habilita" %in% x$clase) {
      expect_true(x$forma %in% c("tabla", "interruptor", "columnas", "duplicados"),
                  info = argumento)
      # Lo que la comparacion lee: un campo de `meta`, o -`duplicados`- lo que
      # corrio en `perfil$duplicados_aproximados`.
      expect_true(!is.null(x$meta) || identical(x$forma, "duplicados"),
                  info = argumento)
      expect_true(length(x$hallazgos) >= 1L, info = argumento)
      expect_true(argumento %in% names(.encendidos_O164), info = argumento)
    }
    if ("vara" %in% x$clase) {
      expect_true(argumento %in% names(.varas_O164), info = argumento)
    }
    if ("politica" %in% x$clase) expect_true(nzchar(x$fila), info = argumento)
  }
  # `perfilar()` registra en `meta` todo campo que la comparacion lee.
  p <- suppressWarnings(perfilar(data.frame(a = 1:3)))
  campos <- unlist(lapply(registro, function(x) {
    if (any(c("habilita", "vara") %in% x$clase)) x$meta
  }), use.names = FALSE)
  faltan <- setdiff(setdiff(campos, "declaracion_clave"), names(p$meta))
  expect_identical(faltan, character())
})

test_that("M4: apagar un diagnostico o retirar su declaracion no publica sus hallazgos como resueltos", {
  d <- .datos_O164()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  encendido <- suppressWarnings(do.call(
    perfilar, c(list(d, fecha = fecha), .encendidos_O164)
  ))
  apagado <- suppressWarnings(do.call(
    perfilar, c(list(d, fecha = fecha + 60), .apagados_O164)
  ))
  habilitan <- Filter(function(x) "habilita" %in% x$clase,
                      lupa:::.ARGUMENTOS_PERFIL_DERIVA)
  tipos <- unique(unlist(lapply(habilitan, function(x) x$hallazgos)))
  # La guarda no es vacia: el perfil encendido tiene un hallazgo de cada
  # argumento -sin `stringdist`, las variantes equifrecuentes y los duplicados
  # aproximados, que miden distancia de edicion, pueden no correr-.
  sin_distancia <- if (nzchar(system.file(package = "stringdist"))) {
    character()
  } else c("variantes_equifrecuentes_vocabulario", "duplicados_aproximados")
  presentes <- unique(as.character(encendido$hallazgos$tipo_hallazgo))
  for (argumento in setdiff(names(habilitan), sin_distancia)) {
    expect_true(any(habilitan[[argumento]]$hallazgos %in% presentes),
                info = argumento)
  }
  esperados <- intersect(tipos, presentes)
  # Los mismos datos: nada se resolvio.
  r <- suppressWarnings(comparar_perfiles(encendido, apagado))
  h <- r[r$aspecto == "hallazgo" & r$valor_anterior %in% tipos, , drop = FALSE]
  expect_identical(sort(unique(as.character(h$cambio))), "no_evaluado")
  expect_setequal(unique(h$valor_anterior), esperados)
  expect_true(all(as.character(h$severidad) == "sospechoso"))
  fila <- h[h$valor_anterior == "casi_duplicados_vocabulario" & h$columna == "ciudad", ]
  expect_match(fila$descripcion, "`casi_duplicados_vocabulario`", fixed = TRUE)
  # Control: un perfil con todo encendido contra si mismo no tiene esas filas, y
  # un hallazgo que no depende de ninguna declaracion sigue su regla.
  r2 <- suppressWarnings(comparar_perfiles(encendido, encendido))
  expect_false(any(r2$aspecto == "hallazgo"))
})

test_that("B2: toda vara de perfilar() que cambia se declara en configuracion_umbrales", {
  d <- .datos_O164()[1:40, ]
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  base <- suppressWarnings(perfilar(d, fecha = fecha))
  movida <- suppressWarnings(do.call(
    perfilar, c(list(d, fecha = fecha + 60), .varas_O164)
  ))
  r <- suppressWarnings(comparar_perfiles(base, movida))
  fila <- r[r$aspecto == "configuracion_umbrales", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  varas <- lupa:::.argumentos_perfil_deriva_de_clase("vara")
  for (vara in varas) {
    expect_true(grepl(paste0(vara, "="), fila$valor_anterior, fixed = TRUE),
                info = vara)
  }
  # Un perfil de una version que no registraba una vara no publica un cambio
  # que no hubo.
  viejo <- base
  viejo$meta$umbral_aritmetica <- NULL
  r_viejo <- suppressWarnings(comparar_perfiles(
    viejo, suppressWarnings(perfilar(d, fecha = fecha + 120))
  ))
  expect_false(any(r_viejo$aspecto == "configuracion_umbrales"))
})

# ---- Segunda vuelta: la refutacion externa de 35-D ---------------------------

test_that("segunda vuelta: una clave declarada sobre otra columna no resuelve la anterior", {
  # Los mismos datos: `id` repetida, `ci` unica.
  d <- data.frame(id = rep(1:30, 2), ci = seq_len(60),
                  ciudad = rep(c("Salto", "Rivera"), 30), stringsAsFactors = FALSE)
  expect_true(anyDuplicated(d$id) > 0L)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  con_id <- suppressWarnings(perfilar(d, fecha = fecha, clave = "id"))
  con_ci <- suppressWarnings(perfilar(d, fecha = fecha + 60, clave = "ci"))
  expect_true("clave_no_unica" %in% con_id$hallazgos$tipo_hallazgo)
  expect_false("clave_no_unica" %in% con_ci$hallazgos$tipo_hallazgo)
  r <- suppressWarnings(comparar_perfiles(con_id, con_ci))
  fila <- r[r$aspecto == "hallazgo" & r$valor_anterior == "clave_no_unica", , drop = FALSE]
  expect_identical(as.character(fila$cambio), "no_evaluado")
  expect_match(fila$descripcion, "`clave`", fixed = TRUE)
  # Al reves -antes `ci`, despues `id`-: el hallazgo aparece, y la descripcion
  # dice que el perfil anterior no lo evaluaba.
  antes_ci <- suppressWarnings(perfilar(d, fecha = fecha, clave = "ci"))
  despues_id <- suppressWarnings(perfilar(d, fecha = fecha + 60, clave = "id"))
  r2 <- suppressWarnings(comparar_perfiles(antes_ci, despues_id))
  fila2 <- r2[r2$aspecto == "hallazgo" & r2$valor_actual == "clave_no_unica", , drop = FALSE]
  expect_identical(as.character(fila2$cambio), "aparecido")
  expect_match(fila2$descripcion, "el perfil anterior no evaluaba", fixed = TRUE)
  # Control: la misma clave escrita en otro orden es la misma declaracion.
  d$otra <- d$ciudad
  ab <- suppressWarnings(perfilar(d, fecha = fecha, clave = c("id", "otra")))
  ba <- suppressWarnings(perfilar(d, fecha = fecha + 60, clave = c("otra", "id")))
  expect_false(any(suppressWarnings(comparar_perfiles(ab, ba))$aspecto == "hallazgo"))
})

test_that("segunda vuelta: apagar duplicados_aproximados o cambiar sus columnas no resuelve sus pares", {
  # Sin `stringdist`, `detectar_duplicados_aproximados()` no corre.
  skip_if_not_installed("stringdist")
  d <- .datos_O164()[1:60, ]
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  pares <- c("ciudad", "depto")
  con <- suppressWarnings(perfilar(d, fecha = fecha,
                                   duplicados_aproximados = list(columnas = pares)))
  tipos <- c("duplicados_exactos_columnas", "duplicados_exactos_normalizados",
             "duplicados_aproximados")
  # A mano: el hallazgo nombra el par de columnas como una sola, `ciudad, depto`.
  columnas_pares <- unique(con$hallazgos$columna[con$hallazgos$tipo_hallazgo %in% tipos])
  expect_identical(columnas_pares, "ciudad, depto")
  # Apagado: con los mismos datos, el par no se resolvio. Antes no salia
  # ninguna fila, ni `resuelto` ni `no_evaluado`.
  sin <- suppressWarnings(perfilar(d, fecha = fecha + 60))
  r <- suppressWarnings(comparar_perfiles(con, sin))
  h <- r[r$aspecto == "hallazgo" & r$valor_anterior %in% tipos, , drop = FALSE]
  expect_gt(nrow(h), 0L)
  expect_identical(unique(as.character(h$cambio)), "no_evaluado")
  expect_match(h$descripcion[[1L]], "`duplicados_aproximados`", fixed = TRUE)
  # Sobre otras columnas: los pares de antes no se volvieron a evaluar, y la
  # fila de configuracion lo dice.
  otras <- suppressWarnings(perfilar(d, fecha = fecha + 120,
                                    duplicados_aproximados = list(columnas = "ciudad")))
  r2 <- suppressWarnings(comparar_perfiles(con, otras))
  h2 <- r2[r2$aspecto == "hallazgo" & r2$columna %in% "ciudad, depto", , drop = FALSE]
  expect_identical(unique(as.character(h2$cambio)), "no_evaluado")
  expect_true("configuracion_duplicados_aproximados" %in% r2$aspecto)
  # Las mismas columnas con otro umbral: se volvieron a evaluar, y el cambio de
  # parametro se declara.
  umbral <- suppressWarnings(perfilar(d, fecha = fecha + 180,
                                      duplicados_aproximados = list(columnas = pares, umbral = 0.05)))
  r3 <- suppressWarnings(comparar_perfiles(con, umbral))
  fila <- r3[r3$aspecto == "configuracion_duplicados_aproximados", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_match(fila$valor_anterior, "umbral=", fixed = TRUE)
  expect_false(grepl("columnas=", fila$valor_anterior, fixed = TRUE))
})

test_that("segunda vuelta: un hallazgo de varias columnas que se resolvio en los datos sale resuelto", {
  # Con el diagnostico encendido en las dos corridas y los datos cambiados, el
  # par duplicado y la relacion aritmetica desaparecian sin ninguna fila.
  n <- 60L
  d <- data.frame(id = seq_len(n), nombre = rep(c("Ana", "Juan", "Pedro"), 20),
                  ciudad = rep(c("Salto", "Rivera", "Minas", "Melo"), 15),
                  monto = round(10 + (seq_len(n) * 37) %% 91, 2), stringsAsFactors = FALSE)
  d$doble <- d$monto * 2
  d2 <- d
  d2$nombre <- paste0("n", seq_len(n))
  d2$doble <- round(10 + (seq_len(n) * 53) %% 197, 2)
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  argumentos <- list(duplicados_aproximados = list(columnas = c("nombre", "ciudad")))
  a <- suppressWarnings(do.call(perfilar, c(list(d, fecha = fecha), argumentos)))
  b <- suppressWarnings(do.call(perfilar, c(list(d2, fecha = fecha + 60), argumentos)))
  compuestas <- unique(a$hallazgos[grepl(",", a$hallazgos$columna),
                                   c("columna", "tipo_hallazgo")])
  # Sin `stringdist` los duplicados entre columnas no corren; la relacion si.
  tipos <- c(
    if (nzchar(system.file(package = "stringdist"))) "duplicados_exactos_columnas",
    "relacion_aritmetica_columnas"
  )
  expect_true(all(tipos %in% compuestas$tipo_hallazgo))
  r <- suppressWarnings(comparar_perfiles(a, b))
  for (tipo in tipos) {
    fila <- r[r$aspecto == "hallazgo" & r$valor_anterior %in% tipo, , drop = FALSE]
    expect_identical(as.character(fila$cambio), "resuelto", info = tipo)
  }
  # Control: si se retira una de las columnas del par, el hallazgo no se
  # presenta como resuelto -la columna desaparecida tiene su propia fila-.
  b_sin <- suppressWarnings(do.call(perfilar, c(
    list(d2[setdiff(names(d2), "doble")], fecha = fecha + 60), argumentos
  )))
  r_sin <- suppressWarnings(comparar_perfiles(a, b_sin))
  expect_false(any(r_sin$aspecto == "hallazgo" &
                     r_sin$valor_anterior %in% "relacion_aritmetica_columnas"))
  expect_true(any(r_sin$aspecto == "columna" & r_sin$cambio == "desaparecida"))
})

test_that("segunda vuelta: la clase de cada argumento explica lo que cambia al moverlo", {
  skip_on_cran()
  # Que el registro tenga todos los argumentos no ve una clase equivocada:
  # `duplicados_aproximados` figuraba `sin_hallazgo` y produce hallazgos. Esta
  # guarda perfila un dato fijo con cada argumento movido y exige que la clase
  # explique el cambio: un `sin_hallazgo` no cambia ningun hallazgo, todo
  # hallazgo que cambia tiene su fila en la comparacion -los de varias columnas
  # desaparecian sin ninguna-, y ningun `resuelto` sale sin una fila de
  # configuracion que lo explique, en los dos sentidos.
  d <- .datos_O164()[c(1:36, 77:86, 97:104, 113:120), ]
  d$doble <- d$monto * 2
  d$doble[3] <- 7
  movidos <- list(
    muestra = 50, max_patrones = 2L, distinguir_mayusculas = FALSE,
    expandir = TRUE, umbral_alta_cardinalidad = 0.9,
    umbral_faltantes_sospechoso = 0.3, umbral_faltantes_error = 0.6,
    umbral_patron_raro = 0.01, umbral_patron_dominante = 0.6, clave = "id",
    columnas_sin_ceros = "monto", columnas_no_negativas = "monto",
    columnas_opcionales = "conyuge", columnas_codigo = "id",
    aplicabilidad = list(depto = ~ ciudad != "Salto"),
    ausencia_estructural = FALSE, sentinelas_numericos = numeric(),
    cadenas_ausencia = "zz", analizar_dependencias = FALSE,
    umbral_dependencia = 1, umbral_casi_clave_dependencia = 0.99,
    max_columnas_dependencias = 2L, datos_personales_permitidos = FALSE,
    proteger_datos_personales = FALSE, columnas_personales = "ciudad",
    validadores_personales = FALSE, umbral_documento_verificado = 1,
    muestra_validadores = 10L,
    duplicados_aproximados = list(columnas = c("ciudad", "depto")),
    normalizar = FALSE, max_filas_hallazgo = 1L, umbral_orden_columnas = 1,
    max_columnas_orden = 2L, umbral_solapamiento_orden = 0,
    umbral_aritmetica = 0.999999, min_filas_aritmetica = 100000L,
    tolerancia_aritmetica = 1e-20, max_columnas_aritmetica = 2L,
    casi_duplicados_vocabulario = FALSE, max_proporcion_grupo_vocabulario = 0.01,
    umbral_variante_rara_vocabulario = 0.5, min_asimetria_vocabulario_corto = 1000,
    min_asimetria_vocabulario = 1000,
    min_participacion_dominante_vocabulario_corto = 0.99,
    variantes_equifrecuentes_vocabulario = TRUE,
    max_asimetria_equifrecuente_vocabulario = 1,
    max_comparaciones_dependencias = 1L, max_trabajo_vocabulario = 1,
    max_trabajo_dependencias = 1, max_largo_valor_vocabulario = 1L,
    max_celdas_muestra = 200, max_bytes_muestra = 2000,
    avisar_costo_tabla_ancha = FALSE, umbral_celdas_aviso_tabla_ancha = 1
  )
  registro <- lupa:::.ARGUMENTOS_PERFIL_DERIVA
  no_identidad <- names(registro)[!vapply(
    registro, function(x) identical(x$clase, "identidad"), logical(1L)
  )]
  # Un argumento nuevo tiene que traer su valor movido.
  expect_setequal(names(movidos), no_identidad)
  claves <- function(p) {
    h <- p$hallazgos
    unique(paste(ifelse(is.na(h$columna), "<tabla>", h$columna), h$tipo_hallazgo,
                 sep = "\034"))
  }
  publicadas <- function(r, lado) {
    h <- r[r$aspecto == "hallazgo", , drop = FALSE]
    tipo <- if (identical(lado, "anterior")) h$valor_anterior else h$valor_actual
    paste(ifelse(is.na(h$columna), "<tabla>", h$columna), tipo, sep = "\034")
  }
  # Sin `fecha`: la direccion la decide el orden de los argumentos.
  base <- suppressWarnings(suppressMessages(perfilar(d)))
  for (argumento in names(movidos)) {
    movido <- suppressWarnings(suppressMessages(do.call(
      perfilar, c(list(d), stats::setNames(list(movidos[[argumento]]), argumento))
    )))
    clase <- registro[[argumento]]$clase
    desaparecen <- setdiff(claves(base), claves(movido))
    aparecen <- setdiff(claves(movido), claves(base))
    if (identical(clase, "sin_hallazgo")) {
      expect_identical(c(desaparecen, aparecen), character(), info = argumento)
    }
    for (sentido in c("mueve", "vuelve")) {
      r <- if (sentido == "mueve") {
        suppressWarnings(comparar_perfiles(base, movido))
      } else suppressWarnings(comparar_perfiles(movido, base))
      se_fueron <- if (sentido == "mueve") desaparecen else aparecen
      llegaron <- if (sentido == "mueve") aparecen else desaparecen
      info <- paste(argumento, sentido)
      expect_true(all(se_fueron %in% publicadas(r, "anterior")), info = info)
      expect_true(all(llegaron %in% publicadas(r, "actual")), info = info)
      configuracion <- any(grepl("^configuracion_|^proteccion$", r$aspecto))
      resueltos <- r$aspecto == "hallazgo" & r$cambio == "resuelto"
      expect_true(!any(resueltos) || configuracion, info = info)
    }
  }
})
