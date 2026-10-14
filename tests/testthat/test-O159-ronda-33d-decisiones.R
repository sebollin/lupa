# Ronda 33-D: lo que la refutacion de las decisiones de la ronda 32 encontro.
# Cada prueba rehace su cifra a mano y falla contra el codigo anterior al arreglo.

.fecha_O159 <- as.POSIXct("2026-01-01", tz = "UTC")

# Cuarenta celdas: 36 valores, un `NA` y tres `-999` disfrazados.
.datos_opcional_O159 <- function() {
  data.frame(id = 1:40, v = as.numeric(c(1:36, NA, -999, -999, -999)))
}

.proporcion_instancia_O159 <- function(med, instancia) {
  r <- med[!is.na(med$fila) & med$metrica_instanciada == instancia, ,
           drop = FALSE]
  list(n = nrow(r), valor = mean(as.numeric(unlist(r$resultado))))
}

test_that("H1: la NoNulo de una columna opcional se mide sobre las celdas presentes", {
  d <- .datos_opcional_O159()
  # A mano: la declaracion saca el NA del universo; quedan 39 celdas presentes y
  # tres son disfrazadas.
  esperado <- 36 / 39
  p <- perfilar(d, columnas_opcionales = "v", fecha = .fecha_O159,
                proteger_datos_personales = FALSE)
  fila <- p$columnas[p$columnas$columna == "v", ]
  expect_equal(1 - fila$prop_faltantes_totales, esperado)
  expect_identical(p$meta$columnas_opcionales, "v")
  pr <- proponer_modelo(p, datos = d)
  nn <- pr[pr$metrica == "NoNulo", , drop = FALSE]
  expect_identical(nrow(nn), 1L)
  expect_true(isTRUE(nn$configuracion[[1L]]$opcional))
  expect_match(nn$justificacion, "opcional", fixed = TRUE)
  med <- medir(modelo_desde_propuesta(pr), d, fecha = .fecha_O159)
  medido <- .proporcion_instancia_O159(med, "NoNulo:v")
  expect_identical(medido$n, 39L)
  expect_equal(medido$valor, esperado)
  expect_null(attr(med, "alcance_medidas", exact = TRUE))
  a <- analizar(d, fecha = .fecha_O159,
                argumentos_perfil = list(columnas_opcionales = "v"))
  expect_equal(
    a$tablero$valor[a$tablero$metrica_instanciada == "NoNulo:v"], esperado
  )
})

test_that("H1: sin declarar, el NA sigue contando, y sin datos mide sobre las presentes", {
  d <- .datos_opcional_O159()
  p0 <- perfilar(d, fecha = .fecha_O159, proteger_datos_personales = FALSE)
  med0 <- medir(modelo_desde_propuesta(proponer_modelo(p0, datos = d)), d,
                fecha = .fecha_O159)
  # A mano: un NA y tres disfrazados sobre cuarenta.
  expect_equal(.proporcion_instancia_O159(med0, "NoNulo:v")$valor, 36 / 40)
  p <- perfilar(d, columnas_opcionales = "v", fecha = .fecha_O159,
                proteger_datos_personales = FALSE)
  # Sin datos los disfrazados no se identifican: 39 presentes, ninguna cuenta.
  med <- medir(modelo_desde_propuesta(proponer_modelo(p)), d,
               fecha = .fecha_O159)
  expect_equal(.proporcion_instancia_O159(med, "NoNulo:v")$valor, 39 / 39)
})

test_that("H1: NoNulo valida `opcional` y lo mide igual bajo C con un nombre no ASCII", {
  expect_error(
    especializar(metricas_nucleo()$NoNulo, opcional = "si"),
    "opcional"
  )
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")))) {
    skip("No se pudo fijar el locale C.")
  }
  d <- .datos_opcional_O159()
  nombre <- "a\u00f1o v"
  names(d)[2L] <- nombre
  avisos <- character()
  medido <- withCallingHandlers({
    p <- perfilar(d, columnas_opcionales = nombre, fecha = .fecha_O159,
                  analizar_dependencias = FALSE,
                  proteger_datos_personales = FALSE)
    pr <- proponer_modelo(p, datos = d)
    med <- medir(modelo_desde_propuesta(pr), d, fecha = .fecha_O159)
    r <- med[!is.na(med$fila) & med$metrica == "NoNulo", , drop = FALSE]
    mean(as.numeric(unlist(r$resultado)))
  }, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_equal(medido, 36 / 39)
  expect_length(avisos, 0L)
})

test_that("H2: perfilar_dbi no corrobora lo que columnas_codigo manda no calcular", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- data.frame(
    cod = c(rep(1:20, 3L), 284L),
    monto = c(seq(100, 160, length.out = 60L), 900)
  )
  d$cod2 <- d$cod
  DBI::dbWriteTable(con, "t", d)
  divergencias_cod <- function(r) {
    cb <- r$resumen_tabla$cobertura
    cb[cb$bloque == "corroboracion" &
         grepl("cod::(media|mediana|desvio)", cb$elemento), , drop = FALSE]
  }
  r <- perfilar_dbi(con, "t", columnas_codigo = "cod",
                    proteger_datos_personales = FALSE)
  expect_identical(nrow(divergencias_cod(r)), 0L)
  expect_identical(r$resumen_tabla$meta$corroboracion_bloques$divergencias, 0L)
  columnas <- r$resumen_tabla$columnas
  expect_true(is.na(columnas$media[columnas$columna == "cod"]))
  # A mano: la gemela sin declarar conserva la media que el SQL calcula, y el
  # resumen y la muestra coinciden en ella.
  expect_equal(columnas$media[columnas$columna == "cod2"], mean(d$cod))
  cb <- r$resumen_tabla$cobertura
  expect_true(any(cb$bloque == "resumen_tabla" & cb$elemento == "cod" &
                    cb$estado == "no_aplica"))
  # Con una muestra parcial el lado que falta tambien era divergencia.
  rm <- suppressWarnings(perfilar_dbi(con, "t", columnas_codigo = "cod",
                                      muestra = 30,
                                      proteger_datos_personales = FALSE))
  expect_identical(nrow(divergencias_cod(rm)), 0L)
  co <- perfilar_coleccion(coleccion(con, "t", nombre = "c"),
                           columnas_codigo = "cod",
                           proteger_datos_personales = FALSE)
  # La coleccion no sube a `divergencias` una tabla cuyas cifras coinciden: sin
  # nada que declarar, la tabla no aparece en `cobertura_coleccion`.
  expect_false(any(co$cobertura_coleccion$alcance %in% "divergencias"))
  expect_identical(nrow(co$cobertura_coleccion), 0L)
  expect_identical(as.character(co$resumen_coleccion$tabla), "t")
})

test_that("H3: la evaluacion de analizar() conserva la configuracion y separa el cambio de modelo", {
  pe <- perfil_evaluacion(
    "Operativo", regla_evaluacion("Completitud", function(x) x > 0.8)
  )
  set.seed(8)
  d1 <- data.frame(id = ifelse(stats::runif(60) < 0.1, NA, 1:60),
                   x = sample(1:3, 60, TRUE))
  d2 <- d1
  d2$x[1:36] <- NA
  f2 <- as.POSIXct("2026-02-01", tz = "UTC")
  a1 <- analizar(d1, nombre = "personas", fecha = .fecha_O159,
                 id_medicion = "c1", perfil_evaluacion = pe)
  a2 <- analizar(d2, nombre = "personas", fecha = f2,
                 id_medicion = "c2", perfil_evaluacion = pe)
  # A mano: la segunda entrega agrega `NoNulo:x`; el conjunto de metricas cambio.
  expect_identical(
    setdiff(a2$tablero$metrica_instanciada, a1$tablero$metrica_instanciada),
    "NoNulo:x"
  )
  for (a in list(a1, a2)) {
    expect_false(is.null(attr(a$evaluacion, "configuracion_modelo", exact = TRUE)))
    expect_false(is.null(
      attr(a$evaluacion, "configuracion_aplicabilidad", exact = TRUE)
    ))
    expect_true(isTRUE(attr(a$evaluacion, "fecha_declarada", exact = TRUE)))
  }
  h <- historico_calidad(a1$evaluacion, a2$evaluacion)
  expect_identical(
    sort(unique(stats::na.omit(as.character(h$identidad_tabla)))), "personas"
  )
  dv <- detectar_deriva_calidad(h, nivel = "regla")
  expect_identical(as.character(dv$aspecto), "configuracion_modelo")
  expect_identical(as.character(dv$cambio), "no_comparable")
  expect_identical(as.character(dv$identidad_tabla), "personas")
})

test_that("H3: la configuracion del modelo es la del modelo entero y la fecha no declarada no se afirma", {
  pe <- perfil_evaluacion(
    "Operativo", regla_evaluacion("Completitud", function(x) x > 0.8)
  )
  d <- data.frame(id = c(1:9, NA), x = c(1, NA, 3:10))
  a <- analizar(d, nombre = "visitas", id_medicion = "v1",
                perfil_evaluacion = pe)
  expect_false(isTRUE(attr(a$evaluacion, "fecha_declarada", exact = TRUE)))
  # A mano: el modelo que analizar() midio, medido por la puerta manual.
  pr <- a$propuesta_modelo
  pr$incluir <- as.character(pr$estado) == "lista"
  manual <- medir(modelo_desde_propuesta(pr), d, fecha = .fecha_O159)
  config_a <- attr(a$medicion, "configuracion_modelo", exact = TRUE)
  config_m <- attr(manual, "configuracion_modelo", exact = TRUE)
  expect_identical(sort(names(config_a$metricas)), c("NoNulo:id", "NoNulo:x"))
  expect_identical(
    config_a$metricas[sort(names(config_a$metricas))],
    config_m$metricas[sort(names(config_m$metricas))]
  )
  expect_identical(config_a$entidades, "visitas")
  # Por el camino que conserva el detalle, la misma configuracion, y las dos
  # mediciones del objeto dicen lo mismo de la fecha.
  ad <- analizar(d, nombre = "visitas", id_medicion = "v2",
                 perfil_evaluacion = pe, conservar_detalle_medicion = TRUE)
  expect_identical(attr(ad$evaluacion, "configuracion_modelo", exact = TRUE),
                   config_a)
  expect_false(isTRUE(attr(ad$evaluacion, "fecha_declarada", exact = TRUE)))
  expect_false(isTRUE(attr(ad$detalle_medicion, "fecha_declarada", exact = TRUE)))
})

.datos_coma_O159 <- function() {
  set.seed(3)
  ent <- sample(10:500, 60, TRUE)
  dec <- sample(0:99, 60, TRUE)
  data.frame(
    punto = c(sprintf("%d.%02d", ent, dec), rep("-999.00", 3L)),
    coma = c(sprintf("%d,%02d", ent, dec), rep("-999,00", 3L)),
    stringsAsFactors = FALSE
  )
}

test_that("H4: un centinela escrito con la coma decimal de la columna es un disfrazado", {
  d <- .datos_coma_O159()
  p <- perfilar(d, fecha = .fecha_O159, proteger_datos_personales = FALSE)
  coma <- p$columnas[p$columnas$columna == "coma", ]
  expect_identical(coma$numero_texto_convencion, "decimal_coma")
  expect_equal(coma$minimo, -999)
  # A mano: as.numeric(sub(",", ".", "-999,00")) es -999, de la lista por
  # omision; tres disfrazados, como la misma cifra escrita con punto.
  expect_identical(as.numeric(sub(",", ".", "-999,00", fixed = TRUE)), -999)
  expect_identical(coma$n_faltantes_disfrazados, 3L)
  expect_identical(
    coma$n_faltantes_disfrazados,
    p$columnas$n_faltantes_disfrazados[p$columnas$columna == "punto"]
  )
  expect_true(any(p$hallazgos$columna %in% "coma" &
                    p$hallazgos$tipo_hallazgo == "faltantes_disfrazados"))
  pr <- proponer_modelo(p, datos = d)
  expect_true("NoNulo:coma" %in% pr$id_sugerencia)
  med <- medir(modelo_desde_propuesta(pr), d, fecha = .fecha_O159)
  expect_equal(.proporcion_instancia_O159(med, "NoNulo:coma")$valor, 60 / 63)
  # Y la accion del plan convierte las mismas tres celdas.
  plan <- planificar_limpieza(p, d)
  sel <- plan$estrategia == "convertir_sentinelas_numericos" &
    plan$columna %in% "coma"
  expect_identical(sum(sel), 1L)
  expect_equal(plan$n_afectadas[sel], 3)
  plan$aplicar <- sel
  res <- aplicar(plan, d)
  expect_identical(sum(is.na(res$datos$coma)), 3L)
  expect_identical(which(is.na(res$datos$coma)), 61:63)
})

.datos_codigo_O159 <- function() {
  set.seed(33)
  cod <- sample(c(1:20, 284L), 120, replace = TRUE,
                prob = c(rep(1, 20), 0.2))
  data.frame(cod = cod, monto = cod * 10 + stats::rnorm(120))
}

test_that("H6: dentro de analizar(), columnas_codigo llega a variables y asociaciones", {
  d <- .datos_codigo_O159()
  a <- analizar(d, fecha = .fecha_O159,
                argumentos_perfil = list(columnas_codigo = "cod"))
  v <- a$variables[a$variables$columna == "cod", ]
  expect_identical(v$escala_propuesta, "nominal")
  expect_identical(v$rol, "categoria")
  expect_true(v$confirmada)
  m <- a$variables[a$variables$columna == "monto", ]
  expect_identical(m$rol, "medida")
  as <- a$asociaciones
  par <- as[(as$columna_1 == "cod" & as$columna_2 == "monto") |
              (as$columna_1 == "monto" & as$columna_2 == "cod"), ]
  expect_identical(nrow(par), 1L)
  expect_identical(as.character(par$metodo), "eta2")
  # A mano: eta cuadrado del monto por codigo.
  g <- factor(d$cod)
  eta2 <- sum(tapply(d$monto, g, length) *
                (tapply(d$monto, g, mean) - mean(d$monto))^2) /
    sum((d$monto - mean(d$monto))^2)
  expect_equal(par$asociacion, eta2, tolerance = 1e-8)
})

test_that("H6: un codigo declarado guardado como texto no pide convertirse a numero", {
  set.seed(6)
  d <- data.frame(cod = sprintf("%03d", sample(1:30, 80, TRUE)),
                  v = stats::rnorm(80), stringsAsFactors = FALSE)
  tipos <- function(p) p$hallazgos$tipo_hallazgo[p$hallazgos$columna %in% "cod"]
  p0 <- perfilar(d, fecha = .fecha_O159, proteger_datos_personales = FALSE)
  expect_true("tipo_declarado_distinto" %in% tipos(p0))
  p <- perfilar(d, columnas_codigo = "cod", fecha = .fecha_O159,
                proteger_datos_personales = FALSE)
  expect_false("tipo_declarado_distinto" %in% tipos(p))
  expect_identical(
    p$columnas$estado_resumen_cuantitativo[p$columnas$columna == "cod"],
    "omitidos_codigo"
  )
})

# Segunda vuelta: la media y el desvio que el motor calcula en `numeric`.
# SQL Server devuelve `AVG(int * 1.0)` con seis decimales y PostgreSQL
# `STDDEV_SAMP(int * 1.0)` con cuatro en magnitudes grandes; la corroboracion
# publicaba una divergencia que no existe. Los dos motores son de contenedor:
# aca se prueba la expresion que se les pide y, con las senas de SQL Server, la
# puerta entera sobre SQLite.
.datos_media_O159 <- function() {
  set.seed(4)
  data.frame(entero = sample(1:30, 61, TRUE),
             grande = sample(1:30, 61, TRUE) * 1000003L)
}

test_that("H7: en T-SQL la media se pide como doble y en los demas motores como antes", {
  expect_identical(
    lupa:::.media_sql_dbi("\"x\"", "microsoft sql server odbc"),
    "AVG(CAST(\"x\" AS FLOAT))"
  )
  for (senas in c("sqliteconnection rsqlite", "duckdb_connection duckdb",
                  "pqconnection rpostgres", "")) {
    expect_identical(lupa:::.media_sql_dbi("\"x\"", senas), "AVG(\"x\" * 1.0)")
  }
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # La clase con que odbc arma una conexion a SQL Server se lee como T-SQL.
  expect_true(lupa:::.es_transact_sql_dbi(lupa:::.senas_conexion_dbi(
    structure(list(), class = "Microsoft SQL Server")
  )))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- .datos_media_O159()
  DBI::dbWriteTable(con, "t", d)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "microsoft sql server",
    .package = "lupa"
  )
  r <- perfilar_dbi(con, "t", proteger_datos_personales = FALSE)
  sql <- r$resumen_tabla$sql$sql
  expect_true(any(grepl("AVG(CAST(`entero` AS FLOAT))", sql, fixed = TRUE)))
  expect_false(any(grepl("AVG(`entero` * 1.0)", sql, fixed = TRUE)))
  columnas <- r$resumen_tabla$columnas
  # A mano: la media de los enteros, entera, como la lee la muestra.
  expect_identical(columnas$media[columnas$columna == "entero"], mean(d$entero))
  expect_identical(columnas$media[columnas$columna == "grande"], mean(d$grande))
  cb <- r$resumen_tabla$cobertura
  expect_false(any(cb$bloque %in% "corroboracion" & cb$estado %in% "divergencia"))
})

test_that("H7: en PostgreSQL el desvio se pide como doble", {
  alias <- function(nombre) nombre
  expect_identical(
    lupa:::.formas_desvio_dbi("t", "\"x\"", alias, "pqconnection rpostgres")[[1L]],
    "STDDEV_SAMP(CAST(\"x\" AS DOUBLE PRECISION)) AS desvio"
  )
  expect_identical(
    lupa:::.formas_desvio_dbi("t", "\"x\"", alias, "duckdb_connection duckdb")[[1L]],
    "STDDEV_SAMP(\"x\" * 1.0) AS desvio"
  )
  # La forma de respaldo calcula su media con la misma regla que `basicos`.
  expect_match(
    lupa:::.formas_desvio_dbi("t", "\"x\"", alias, "microsoft sql server")[[3L]],
    "AVG(CAST(\"x\" AS FLOAT))", fixed = TRUE
  )
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # La expresion es SQL que un motor con `DOUBLE PRECISION` ejecuta, y da el
  # desvio de R.
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  d <- .datos_media_O159()
  DBI::dbWriteTable(con, "t", d)
  expresion <- lupa:::.formas_desvio_dbi(
    "t", "grande", alias, "pqconnection rpostgres"
  )[[1L]]
  valor <- DBI::dbGetQuery(con, paste("SELECT", expresion, "FROM t"))$desvio
  expect_equal(valor, stats::sd(d$grande), tolerance = 1e-12)
})

# Tercera vuelta: MySQL y MariaDB. `AVG(int * 1.0)` es `decimal(16,5)`: la
# media publicada de una `int` era 16.93443 (MariaDB 11.8 y MySQL 8.4). Ahi se
# multiplica por `1e0`, que es `DOUBLE` en cualquier version.
test_that("H8: en MySQL y MariaDB la media se pide como doble con un literal DOUBLE", {
  for (senas in c("mariadbconnection rmariadb", "mysqlconnection rmariadb",
                  "mysqlconnection rmysql")) {
    expect_identical(lupa:::.media_sql_dbi("`x`", senas), "AVG(`x` * 1e0)")
  }
  # Los demas motores, como en la segunda vuelta.
  expect_identical(lupa:::.media_sql_dbi("\"x\"", "microsoft sql server"),
                   "AVG(CAST(\"x\" AS FLOAT))")
  for (senas in c("sqliteconnection rsqlite", "duckdb_connection duckdb",
                  "pqconnection rpostgres")) {
    expect_identical(lupa:::.media_sql_dbi("\"x\"", senas), "AVG(\"x\" * 1.0)")
  }
  expect_match(
    lupa:::.formas_desvio_dbi("t", "`x`", function(n) n,
                              "mysqlconnection rmariadb")[[3L]],
    "AVG(`x` * 1e0)", fixed = TRUE
  )
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # Las clases con que RMariaDB y RMySQL arman la conexion se leen como MySQL.
  for (clase in c("MariaDBConnection", "MySQLConnection")) {
    expect_true(lupa:::.es_mysql_dbi(lupa:::.senas_conexion_dbi(
      structure(list(), class = clase)
    )))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- .datos_media_O159()
  DBI::dbWriteTable(con, "t", d)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "mariadbconnection rmariadb",
    .package = "lupa"
  )
  r <- perfilar_dbi(con, "t", proteger_datos_personales = FALSE)
  sql <- r$resumen_tabla$sql$sql
  expect_true(any(grepl("AVG(`entero` * 1e0)", sql, fixed = TRUE)))
  columnas <- r$resumen_tabla$columnas
  # A mano: la media de los enteros, entera.
  expect_identical(columnas$media[columnas$columna == "entero"], mean(d$entero))
  expect_identical(columnas$media[columnas$columna == "grande"], mean(d$grande))
  cb <- r$resumen_tabla$cobertura
  expect_false(any(cb$bloque %in% "corroboracion" & cb$estado %in% "divergencia"))
})
