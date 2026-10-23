# Defectos de las mediciones. Cada prueba rehace su cifra a mano y falla contra
# el codigo anterior al arreglo.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.fecha_O162 <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("M5: el perfil de analizar() sin `fecha` no la declara y dos entregas fuera de orden se comparan", {
  # Enero con 5 `NA` de 50 en `monto` y febrero con 20: faltantes 0,1 -> 0,4,
  # un deterioro. Se analiza febrero primero y enero despues, sin `fecha`: la
  # hora de la corrida de enero es posterior y no dice nada sobre las entregas.
  set.seed(10)
  enero <- data.frame(id = 1:50, monto = c(rep(NA, 5), runif(45)))
  febrero <- data.frame(id = 1:50, monto = c(rep(NA, 20), runif(30)))
  a_feb <- suppressWarnings(analizar(febrero, nombre = "t",
                                     proteger_datos_personales = FALSE))
  a_ene <- suppressWarnings(analizar(enero, nombre = "t",
                                     proteger_datos_personales = FALSE))
  expect_false(a_ene$perfil$meta$fecha_declarada)
  expect_false(a_feb$perfil$meta$fecha_declarada)
  # Lo mismo que dice la medicion del mismo objeto.
  expect_false(attr(a_ene$medicion, "fecha_declarada", exact = TRUE))
  cmp <- as.data.frame(comparar_perfiles(a_ene$perfil, a_feb$perfil))
  fila <- cmp[cmp$columna %in% "monto" & cmp$aspecto == "faltantes", ,
              drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.numeric(fila$valor_anterior), 5 / 50)
  expect_equal(as.numeric(fila$valor_actual), 20 / 50)
  expect_identical(as.character(fila$severidad), "error")

  # Control: con `fecha` declarada en las dos, el orden se sigue exigiendo.
  b_feb <- suppressWarnings(analizar(febrero, nombre = "t",
                                     fecha = "2026-02-01",
                                     proteger_datos_personales = FALSE))
  b_ene <- suppressWarnings(analizar(enero, nombre = "t",
                                     fecha = "2026-01-01",
                                     proteger_datos_personales = FALSE))
  expect_true(b_ene$perfil$meta$fecha_declarada)
  expect_error(comparar_perfiles(b_feb$perfil, b_ene$perfil), "fechado")
})

test_that("M4: comparar_perfiles() declara el cambio de `columnas_opcionales` como cambio de metodo", {
  # Los mismos datos en las dos corridas: `cedula` solo para personas fisicas y
  # `rut` solo para juridicas, complementarias. Sin declarar, los faltantes de
  # `cedula` son las filas juridicas; declarada opcional, su universo son las
  # celdas presentes y los faltantes son 0. Cambio el metodo, no el dato.
  tipo <- rep(c("fisica", "juridica", "fisica", "fisica"), 10)
  d <- data.frame(id = 1:40, tipo = tipo, stringsAsFactors = FALSE)
  d$cedula <- ifelse(tipo == "fisica", sprintf("C%05d", 1:40), NA)
  d$rut <- ifelse(tipo == "juridica", sprintf("R%05d", 1:40), NA)
  perfil <- function(fecha, ...) {
    perfilar(d, nombre = "t", fecha = fecha, analizar_dependencias = FALSE,
             proteger_datos_personales = FALSE, ...)
  }
  f2 <- as.POSIXct("2026-02-01", tz = "UTC")
  sin <- perfil(.fecha_O162)
  con <- perfil(f2, columnas_opcionales = c("rut", "cedula"))

  cmp <- as.data.frame(comparar_perfiles(sin, con))
  fila <- cmp[cmp$aspecto == "configuracion_columnas_opcionales", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "error")
  expect_identical(fila$valor_anterior, "ninguna")
  expect_identical(fila$valor_actual, "cedula; rut")
  # Los faltantes se siguen comparando, como con `aplicabilidad`: a mano, 30 de
  # 40 fisicas, asi que `cedula` pasa de 10/40 a 0.
  faltantes <- cmp[cmp$columna %in% "cedula" & cmp$aspecto == "faltantes", ,
                   drop = FALSE]
  expect_equal(as.numeric(faltantes$valor_anterior), 10 / 40)
  expect_equal(as.numeric(faltantes$valor_actual), 0)

  # Al retirar la declaracion: el deterioro de faltantes ya no sale solo.
  vuelta <- as.data.frame(comparar_perfiles(
    perfil(.fecha_O162, columnas_opcionales = c("cedula", "rut")), perfil(f2)
  ))
  fila <- vuelta[vuelta$aspecto == "configuracion_columnas_opcionales", ,
                 drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$valor_anterior, "cedula; rut")
  expect_identical(fila$valor_actual, "ninguna")

  # Control: la misma declaracion, escrita en otro orden, no es un cambio.
  igual <- as.data.frame(comparar_perfiles(
    perfil(.fecha_O162, columnas_opcionales = c("cedula", "rut")), con
  ))
  expect_equal(sum(igual$aspecto == "configuracion_columnas_opcionales"), 0L)
  # La guarda que recorre: cada declaracion que cambia lo que el perfil mide
  # -el universo, la vara o lo que cuenta como ausencia- y queda registrada en
  # `meta` se declara como cambio de metodo cuando cambia entre dos corridas.
  d$monto <- c(-999, seq_len(39))
  d$cod <- rep(1:8, 5)
  declaraciones <- list(
    aplicabilidad = list(aplicabilidad = list(cedula = ~ tipo == "fisica")),
    columnas_opcionales = list(columnas_opcionales = "cedula"),
    columnas_codigo = list(columnas_codigo = "cod"),
    sentinelas_numericos = list(sentinelas_numericos = -999),
    columnas_sin_ceros = list(columnas_sin_ceros = "monto"),
    columnas_no_negativas = list(columnas_no_negativas = "monto"),
    umbral_faltantes_error = list(umbral_faltantes_error = 0.9),
    cadenas_ausencia = list(cadenas_ausencia = "S/D")
  )
  sin_nada <- perfil(.fecha_O162)
  for (nombre in names(declaraciones)) {
    declarada <- do.call(perfil, c(list(f2), declaraciones[[nombre]]))
    filas <- as.data.frame(comparar_perfiles(sin_nada, declarada))
    expect_true(any(grepl("^configuracion_", filas$aspecto) &
                      as.character(filas$severidad) == "error"),
                info = nombre)
  }

  # Y un perfil viejo, sin el campo, no inventa el cambio.
  viejo <- sin
  viejo$meta$columnas_opcionales <- NULL
  expect_equal(sum(as.data.frame(comparar_perfiles(viejo, con))$aspecto ==
                     "configuracion_columnas_opcionales"), 0L)
})

test_that("M6: un codigo declarado escrito con separador decimal no se convierte a numero", {
  d <- data.frame(
    id = 1:12, rubro = rep(c("1.1", "1.2", "1.3", "2.1", "2.2", "3.1"), 2),
    tramo = rep(c("1,5", "10,5", "2,5", "20,5", "3,5", "30,5"), 2),
    stringsAsFactors = FALSE
  )
  codigos <- c("rubro", "tramo")
  p <- perfilar(d, nombre = "t", fecha = .fecha_O162, columnas_codigo = codigos,
                analizar_dependencias = FALSE, proteger_datos_personales = FALSE)
  h <- p$hallazgos[p$hallazgos$columna %in% codigos, , drop = FALSE]
  expect_false(any(h$tipo_hallazgo %in% c(
    "numero_como_texto", "tipo_declarado_distinto", "unidades_mixtas",
    "monedas_mixtas"
  )))
  # Lo que no se evaluo se declara, columna por columna.
  cob <- p$cobertura_diagnosticos
  cob <- cob[cob$diagnostico == "numero_como_texto", , drop = FALSE]
  expect_setequal(cob$columna, codigos)
  expect_true(all(grepl("columnas_codigo", cob$motivo, fixed = TRUE)))

  pl <- planificar_limpieza(p, d)
  expect_false(any(pl$columna %in% codigos &
                     pl$estrategia %in% c("convertir_numero_regional",
                                          "convertir_tipo")))
  salida <- aplicar(pl, d)
  salida <- if (is.data.frame(salida)) salida else salida$datos
  expect_identical(salida$rubro, d$rubro)
  expect_identical(salida$tramo, d$tramo)

  # Por `analizar()`, que arma el plan con el mismo perfil.
  a <- suppressWarnings(analizar(d, nombre = "t", fecha = .fecha_O162,
                                 columnas_codigo = codigos,
                                 proteger_datos_personales = FALSE))
  expect_false(any(a$plan_limpieza$columna %in% codigos &
                     a$plan_limpieza$estrategia == "convertir_numero_regional"))

  # La guarda que recorre las formas de un codigo: entero, con cero a la
  # izquierda, factor, decimal con punto y con coma, con moneda. Declarado, ninguna
  # recibe un hallazgo de magnitud ni una accion que lo convierta o lo recorte.
  formas <- list(
    entero = rep(c(101L, 205L, 309L, 412L, 518L, 623L), 2),
    cero = rep(c("006", "012", "018", "024", "030", "036"), 2),
    factor = factor(rep(c("01", "02", "03", "11", "12", "13"), 2)),
    punto = rep(c("1.11", "10.21", "2.31", "20.41", "3.51", "30.61"), 2),
    coma = rep(c("1,5", "10,5", "2,5", "20,5", "3,5", "30,5"), 2),
    moneda = rep(c("$10", "$20", "$30", "$40", "$50", "$60"), 2)
  )
  de_magnitud <- c(
    "numero_como_texto", "tipo_declarado_distinto", "unidades_mixtas",
    "monedas_mixtas", "outliers", "posible_centinela_numerico",
    "relacion_aritmetica_columnas"
  )
  convierte <- c(
    "convertir_numero_regional", "convertir_tipo", "marcar_outliers",
    "winsorizar_outliers"
  )
  for (forma in names(formas)) {
    df <- data.frame(id = 1:12, x = 1:12)
    df$x <- formas[[forma]]
    pf <- perfilar(df, nombre = "t", fecha = .fecha_O162, columnas_codigo = "x",
                   analizar_dependencias = FALSE,
                   proteger_datos_personales = FALSE)
    hx <- pf$hallazgos[pf$hallazgos$columna %in% "x", , drop = FALSE]
    expect_false(any(hx$tipo_hallazgo %in% de_magnitud), info = forma)
    px <- planificar_limpieza(pf, df)
    expect_false(any(px$columna %in% "x" & px$estrategia %in% convierte),
                 info = forma)
  }

  # Control: sin la declaracion, el mismo texto si es un numero escrito como
  # texto y el plan lo convierte -la guarda no apago el diagnostico-.
  sin <- perfilar(d, nombre = "t", fecha = .fecha_O162,
                  analizar_dependencias = FALSE, proteger_datos_personales = FALSE)
  expect_true(any(sin$hallazgos$columna == "rubro" &
                    sin$hallazgos$tipo_hallazgo == "numero_como_texto"))
  pl_sin <- planificar_limpieza(sin, d)
  expect_true(any(pl_sin$columna == "rubro" &
                    pl_sin$estrategia == "convertir_numero_regional" &
                    pl_sin$aplicar))
  # Y un perfil calculado antes del arreglo -con el hallazgo y la declaracion-
  # tampoco le propone la conversion: la guarda es del plan, no solo del perfil.
  viejo <- sin
  viejo$meta$columnas_codigo <- codigos
  pl_viejo <- planificar_limpieza(viejo, d)
  expect_false(any(pl_viejo$columna %in% codigos &
                     pl_viejo$estrategia %in% c("convertir_numero_regional",
                                                "convertir_tipo")))
})

test_that("B2: una relacion aritmetica entre codigos declarados no se publica", {
  # `cod_localidad = cod_depto * 100`: proporcional fila por fila, k = 0,01.
  depto <- rep(c(1L, 5L, 9L, 12L, 19L), 6)
  d <- data.frame(cod_depto = depto, cod_localidad = depto * 100L,
                  monto = round(seq(10, 300, length.out = 30), 2))
  sin <- perfilar(d, fecha = .fecha_O162, analizar_dependencias = FALSE,
                  proteger_datos_personales = FALSE)
  rel <- sin$hallazgos[sin$hallazgos$tipo_hallazgo ==
                         "relacion_aritmetica_columnas", , drop = FALSE]
  expect_equal(nrow(rel), 1L)
  con <- perfilar(d, fecha = .fecha_O162, analizar_dependencias = FALSE,
                  columnas_codigo = c("cod_depto", "cod_localidad"),
                  proteger_datos_personales = FALSE)
  expect_false(any(con$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"))
  cob <- con$cobertura_diagnosticos
  cob <- cob[cob$diagnostico == "relacion_aritmetica_columnas", , drop = FALSE]
  expect_setequal(cob$columna, c("cod_depto", "cod_localidad"))
})

# Una tabla de SQLite con tres declaraciones: `v` con tres
# centinelas `-999` y 37 valores 1,5 .. 55,5; `w` solo donde `tipo == "a"` (30
# de 40 filas; 10 `NA`); y `t`, de afinidad INTEGER, con dos textos que RSQLite
# lee como 0 -la divergencia real que tiene que seguir saliendo-.
.tabla_politicas_O162 <- function() {
  tipo <- rep(c("a", "b", "a", "a"), 10)
  d <- data.frame(
    id = 1:40, tipo = tipo,
    v = c(-999, -999, -999, seq(1.5, 37 * 1.5, by = 1.5)),
    stringsAsFactors = FALSE
  )
  d$w <- ifelse(tipo == "a", round(seq(0.01, 0.4, length.out = 40), 3), NA)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "t", d)
  DBI::dbExecute(con, "CREATE TABLE m (id INTEGER, t INTEGER)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO m VALUES ",
    paste0("(", 1:12, ", ", c(1:10, "'abc'", "'abc'"), ")", collapse = ", ")
  ))
  con
}

.divergencias_O162 <- function(p) {
  cb <- p$resumen_tabla$cobertura
  sort(cb$elemento[cb$bloque == "corroboracion" & cb$estado == "divergencia"])
}

test_that("M1: la corroboracion no publica como divergencia lo que una declaracion mide distinto", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .tabla_politicas_O162()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  perfil <- function(...) {
    suppressWarnings(perfilar_dbi(con, "t", proteger_datos_personales = FALSE,
                                  analizar_dependencias = FALSE, ...))
  }
  no_comparadas <- function(p) {
    x <- p$resumen_tabla$meta$corroboracion_bloques$no_comparadas
    paste(x$columna, x$metrica, x$causa, sep = ":")
  }

  sin <- perfil()
  expect_identical(.divergencias_O162(sin), character())

  # Centinelas declarados: a mano, el SQL ve -999 como minimo y la media
  # (3 * -999 + 1,5 * (1 + ... + 37)) / 40 = -48,5625; la muestra los saca:
  # minimo 1,5 y media 1,5 * 19 = 28,5. Las dos cifras son correctas en su
  # politica, y la cobertura ya declara `sentinelas_numericos` degradado.
  cen <- perfil(sentinelas_numericos = -999)
  rc <- cen$resumen_tabla$columnas; pm <- cen$perfil_muestra$columnas
  expect_equal(rc$minimo[rc$columna == "v"], -999)
  expect_equal(rc$media[rc$columna == "v"], (3 * -999 + 1.5 * sum(1:37)) / 40)
  expect_equal(pm$minimo[pm$columna == "v"], 1.5)
  expect_equal(pm$media[pm$columna == "v"], 28.5)
  expect_identical(.divergencias_O162(cen), character())
  expect_true(all(paste0("v:", c("media", "mediana", "minimo", "maximo",
                                 "desvio", "n_ceros", "n_negativos"),
                         ":sentinelas_numericos") %in% no_comparadas(cen)))
  # `n_distintos` y la moda se cuentan con los centinelas en los dos lados:
  # se siguen corroborando.
  expect_false(any(grepl("^v:n_distintos", no_comparadas(cen))))

  # Universo declarado: el SQL cuenta los 10 `NA` de `w`; la muestra, 0.
  for (args in list(list(aplicabilidad = list(w = ~ tipo == "a")),
                    list(columnas_opcionales = "w"))) {
    p <- do.call(perfil, args)
    expect_equal(p$resumen_tabla$columnas$n_faltantes[
      p$resumen_tabla$columnas$columna == "w"], 10)
    expect_equal(p$perfil_muestra$columnas$n_faltantes[
      p$perfil_muestra$columnas$columna == "w"], 0)
    expect_identical(.divergencias_O162(p), character())
    expect_identical(no_comparadas(p), paste0("w:n_faltantes:", names(args)))
  }

  # `perfilar_coleccion()` ya no sube la tabla a `alcance = "divergencias"`.
  col <- suppressWarnings(perfilar_coleccion(
    coleccion(con, tablas = "t"), columnas_opcionales = "w",
    proteger_datos_personales = FALSE
  ))
  expect_false(any(col$cobertura_coleccion$alcance %in% "divergencias"))

  # La guarda que recorre: toda politica que el resumen SQL declara no honrar
  # -una fila `degradado` en su cobertura- tiene su caso en esta prueba. Una
  # politica nueva entra como argumento de esa funcion, y esta linea falla hasta
  # que alguien decida que metricas cambia y se lo diga a la corroboracion.
  expect_setequal(
    names(formals(lupa:::.cobertura_politicas_muestra_dbi)),
    c("aplicabilidad", "columnas_opcionales", "sentinelas_numericos")
  )
  expect_setequal(
    lupa:::.cobertura_politicas_muestra_dbi(
      aplicabilidad = list(w = ~ TRUE), columnas_opcionales = "w",
      sentinelas_numericos = -999
    )$elemento,
    c("aplicabilidad", "columnas_opcionales", "sentinelas_numericos")
  )

  # Control: la divergencia real sigue saliendo, con los centinelas declarados.
  # RSQLite lee como 0 los dos textos de `t`: la muestra publica `minimo = 0`
  # y el SQL no lo puede leer como numero.
  real <- suppressWarnings(perfilar_dbi(
    con, "m", sentinelas_numericos = -999, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  ))
  expect_true("t::minimo" %in% .divergencias_O162(real))
})

test_that("M1: el BIGINT desde 2^53 no publica divergencias que la guarda crea a proposito", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Doce identificadores tipo snowflake, `15` y 17 cifras: todos > 1,5e18, asi
  # que a mano hay 0 ceros y 0 negativos -lo que cuenta la muestra, exacta sobre
  # `integer64`-, y el resumen SQL no publica sus siete metricas de magnitud.
  ids <- paste0("15", sprintf("%017d", 100000000 + 1:12 * 7919))
  DBI::dbExecute(con, "CREATE TABLE big (id INTEGER, v INTEGER)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO big VALUES ", paste0("(", 1:12, ", ", ids, ")", collapse = ", ")
  ))
  for (bloque in list(NULL, 5L)) {
    p <- suppressWarnings(perfilar_dbi(
      con, "big", bloque_filas = bloque, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE
    ))
    pm <- p$perfil_muestra$columnas
    expect_equal(pm$n_ceros[pm$columna == "v"], 0)
    expect_equal(pm$n_negativos[pm$columna == "v"], 0)
    expect_true(is.na(p$resumen_tabla$columnas$n_ceros[
      p$resumen_tabla$columnas$columna == "v"]))
    expect_identical(.divergencias_O162(p), character())
    nc <- p$resumen_tabla$meta$corroboracion_bloques$no_comparadas
    expect_true(all(c("n_ceros", "n_negativos") %in%
                      nc$metrica[nc$causa == "entero_64_bits"]))
  }
  # B3, misma construccion: el controlador ya entrego `integer64`, y el motivo
  # no dice que llego como doble ni recomienda pedir lo que ya se tiene.
  sql <- suppressWarnings(perfilar_dbi(con, "big", proteger_datos_personales = FALSE,
                                       analizar_dependencias = FALSE))$resumen_tabla$sql
  motivo <- sql$motivo[sql$columna == "v" & sql$metrica == "maximo"]
  expect_match(motivo, "como `integer64`", fixed = TRUE)
  expect_false(grepl("entrego como doble", motivo, fixed = TRUE))
  expect_false(grepl("Conectar pidiendo", motivo, fixed = TRUE))
})

test_that("B3: con el BIGINT entregado como doble, el consejo nombra el argumento de cada controlador", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint = "numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE big (v BIGINT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO big VALUES (9007199254740993), (4503599627370497), (42)"
  ))
  sql <- suppressWarnings(perfilar_dbi(con, "big", proteger_datos_personales = FALSE))$resumen_tabla$sql
  motivo <- sql$motivo[sql$columna == "v" & sql$metrica == "maximo"]
  expect_match(motivo, "driver entrego como doble", fixed = TRUE)
  expect_match(motivo, "`DBI::dbConnect()` en RSQLite, RPostgres, RMariaDB y odbc",
               fixed = TRUE)
})

test_that("M2: media y desvio se corroboran con la tolerancia de una suma acumulada, y una divergencia real sigue saliendo", {
  # La corroboracion entera, sobre dos bloques armados con las cifras medidas en
  # SQL Server: 61 enteros de un millon, `STDEV` del motor
  # 31312.270967375953 contra `sd()` de R 31312.270967376771 -la exacta-,
  # 2,6e-14 relativo, por encima de `64 * eps` = 1,4e-14.
  corroborar <- function(metrica, sql, muestra) {
    resumen <- list(
      columnas = data.frame(columna = "v", stringsAsFactors = FALSE),
      sql = data.frame(columna = "v", metrica = metrica, estado = "calculado",
                       motivo = NA_character_, sql = "SELECT 1",
                       stringsAsFactors = FALSE),
      meta = list(universo = "tabla_completa", filas = 61)
    )
    resumen$columnas[[metrica]] <- sql
    perfil <- list(
      columnas = data.frame(columna = "v", tipo_inferido = "entero",
                            stringsAsFactors = FALSE),
      meta = list(filas_analizadas = 61)
    )
    perfil$columnas[[metrica]] <- muestra
    lupa:::.cobertura_corroboracion_bloques_dbi(resumen, perfil)$cobertura
  }
  expect_equal(nrow(corroborar("desvio", 31312.270967375953,
                               31312.270967376771)), 0L)
  # Una divergencia real es de otro orden y sigue saliendo: la media de
  # `{1e16, 1, -1e16}` que un motor da 0 y es 1/3, y el desvio de una pasada de
  # SQL Server sobre 1e12 + runif(61, 0, 1000) -0 contra 272,27-.
  expect_equal(nrow(corroborar("media", 0, 1 / 3)), 1L)
  expect_equal(nrow(corroborar("desvio", 0, 272.2665190159255)), 1L)
  expect_equal(nrow(corroborar("desvio", 26.127890589687233,
                               27.177095788901457)), 1L)
  # Un valor LEIDO no tiene suma que lo aparte: el minimo de un entero de once
  # cifras que cambio en una unidad -1e-11 relativo- es una divergencia.
  expect_equal(nrow(corroborar("minimo", 10000000001, 1e10)), 1L)
})

# 61 marcas de tiempo en milisegundos de una ventana de un segundo -lo que guarda
# cualquier sistema que escribe epoch en ms- junto a dos columnas comunes y una
# constante. El desvio de una pasada pierde las cifras de `ms`: SQLite publicaba
# 314.77257450919274 contra 314.77258856778... de R (4,5e-8), y SQL Server 0.
.tabla_desvio_O162 <- function() {
  set.seed(9)
  data.frame(
    id = 1:61, ms = 1760000000000 + round(stats::runif(61, 0, 1000)),
    comun = round(seq(-50, 70, length.out = 61), 1), fijo = rep(5, 61)
  )
}

test_that("el desvio de una columna de media grande frente a su ancho sale exacto por SQL y por bloques", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  d <- .tabla_desvio_O162()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  # Con una muestra parcial -donde la corroboracion no puede avisar- y por
  # bloques. A mano, `sd()` de R, la exacta en doble.
  for (bloque in list(NULL, 10L)) {
    p <- suppressWarnings(perfilar_dbi(
      con, "t", muestra = 20, bloque_filas = bloque,
      proteger_datos_personales = FALSE, analizar_dependencias = FALSE
    ))
    rc <- p$resumen_tabla$columnas
    desvio <- function(columna) rc$desvio[rc$columna == columna]
    expect_lt(abs(desvio("ms") - sd(d$ms)) / sd(d$ms), 1e-12)
    expect_equal(rc$media[rc$columna == "ms"], mean(d$ms), tolerance = 1e-15)
    # Los datos comunes no cambian, y la constante sigue siendo 0.
    expect_equal(desvio("comun"), sd(d$comun), tolerance = 1e-15)
    expect_equal(desvio("id"), sd(d$id), tolerance = 1e-15)
    expect_identical(desvio("fijo"), 0)
  }
  # Por SQL, solo la columna con riesgo pidio el desvio otra vez, centrado.
  p <- suppressWarnings(perfilar_dbi(con, "t", muestra = 20,
                                     proteger_datos_personales = FALSE,
                                     analizar_dependencias = FALSE))
  s <- p$resumen_tabla$sql
  s <- s[s$metrica == "desvio", , drop = FALSE]
  expect_match(s$motivo[s$columna == "ms"], "centrados en la media", fixed = TRUE)
  expect_true(all(is.na(s$motivo[s$columna %in% c("id", "comun", "fijo")])))
})

test_that("la expresion del desvio se centra en todos los dialectos", {
  centro <- lupa:::.literal_centro_desvio_dbi(1760000000524.8701)
  expect_identical(centro, "1760000000524.87")
  # El literal no depende de la marca decimal de la sesion ni usa exponente.
  local({
    anteriores <- options(OutDec = ",")
    on.exit(options(anteriores), add = TRUE)
    expect_identical(lupa:::.literal_centro_desvio_dbi(-0.000125), "-0.000125")
  })
  expect_null(lupa:::.literal_centro_desvio_dbi(NA_real_))
  expect_null(lupa:::.literal_centro_desvio_dbi(Inf))
  alias <- function(nombre) "a"
  for (senas in c("pqconnection postgresql", "microsoft sql server",
                  "mariadb mysql", "sqliteconnection", "duckdb_connection")) {
    formas <- lupa:::.formas_desvio_dbi("t", "\"v\"", alias, senas,
                                       centro = centro)
    sin <- lupa:::.formas_desvio_dbi("t", "\"v\"", alias, senas)
    # Las tres formas -la nativa, la de T-SQL y la de dos pasadas- operan sobre
    # la columna centrada; sin centro, quedan como estaban.
    expect_true(all(grepl("(\"v\" - 1760000000524.87)", unlist(formas),
                          fixed = TRUE)), info = senas)
    expect_false(any(grepl(" - 1760000000524.87", unlist(sin), fixed = TRUE)),
                 info = senas)
  }
  # El riesgo: la media diez veces el desvio, o un desvio 0 con variacion.
  expect_true(lupa:::.riesgo_cancelacion_desvio_dbi(1760000000524, 314.77))
  expect_false(lupa:::.riesgo_cancelacion_desvio_dbi(10, 35.5))
  expect_true(lupa:::.riesgo_cancelacion_desvio_dbi(1e12, 0, 1e12, 1e12 + 5))
  expect_false(lupa:::.riesgo_cancelacion_desvio_dbi(5, 0, 5, 5))
})

test_that("un desvio 0 sobre una columna con variacion no se publica", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  d <- .tabla_desvio_O162()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  # El motor responde lo que respondia SQL Server con una pasada: desvio 0.
  testthat::local_mocked_bindings(
    .desvio_columna_dbi = function(...) {
      list(ok = TRUE, valor = 0, sql = "SELECT STDEV(ms) FROM t")
    },
    .package = "lupa"
  )
  p <- suppressWarnings(perfilar_dbi(con, "t", muestra = 20,
                                     proteger_datos_personales = FALSE,
                                     analizar_dependencias = FALSE))
  rc <- p$resumen_tabla$columnas
  s <- p$resumen_tabla$sql
  fila <- s[s$columna == "ms" & s$metrica == "desvio", , drop = FALSE]
  # A mano: minimo < maximo, asi que un desvio 0 es imposible.
  expect_lt(rc$minimo[rc$columna == "ms"], rc$maximo[rc$columna == "ms"])
  expect_true(is.na(rc$desvio[rc$columna == "ms"]))
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "imposible", fixed = TRUE)
  # Control: la constante -minimo igual al maximo- conserva su desvio 0.
  expect_identical(rc$desvio[rc$columna == "fijo"], 0)
})
