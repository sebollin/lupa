# comparar_perfiles() con objetos de perfilar_dbi(): cada cifra sale del
# universo mas amplio que la mide, y cada fila dice de que filas sale.
#
# Antes habia que pasar `$perfil_muestra`, y con una muestra que no cubre la
# tabla eso tenia dos defectos medidos: 3000 filas nuevas con la mitad de
# `monto` en NULL no aparecian -la muestra ordenada por `id` era la misma de
# enero, y el resumen SQL de los dos lados lo tenia exacto-, y el mismo
# contenido entregado dos veces en otro orden daba deriva de faltantes.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.cpd_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cpd_febrero <- as.POSIXct("2026-02-01", tz = "UTC")

.cpd_duckdb <- function(tablas) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), ":memory:")
  for (nombre in names(tablas)) DBI::dbWriteTable(con, nombre, tablas[[nombre]])
  con
}

.cpd_cerrar <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.cpd_dbi <- function(con, tabla, fecha, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, fecha = fecha, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, ...
  )))
}

.cpd_perfil <- function(datos, fecha, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, ...
  )))
}

.cpd_comparar <- function(...) {
  as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(...))))
}

# 3000 filas sin faltantes en `monto`, y las mismas 3000 mas otras 3000 con
# `monto` NULL en la mitad, `estado` con un texto de ausencia y otro formato
# de codigo. Deterministas: los conteos se pueden afirmar.
.cpd_entregas_crecen <- function() {
  n <- 3000L
  base <- data.frame(
    id = seq_len(n),
    edad = 18L + (seq_len(n) %% 70L),
    estado = c("activo", "baja", "suspendido")[1L + seq_len(n) %% 3L],
    codigo = sprintf("AB-%04d", seq_len(n)),
    monto = 10 + (seq_len(n) %% 997) / 4,
    stringsAsFactors = FALSE
  )
  nuevas <- base
  nuevas$id <- n + seq_len(n)
  nuevas$codigo <- sprintf("XX/%05d", seq_len(n))
  nuevas$monto[c(TRUE, FALSE)] <- NA
  nuevas$estado[seq(1L, n, 5L)] <- "N/D"
  list(enero = base, febrero = rbind(base, nuevas))
}

test_that("con muestra que no cubre, los NULL nuevos salen del resumen SQL", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  a <- .cpd_dbi(con, "enero", .cpd_enero, muestra = 500L, orden_muestra = "id")
  b <- .cpd_dbi(con, "febrero", .cpd_febrero, muestra = 500L, orden_muestra = "id")
  deriva <- .cpd_comparar(a, b)
  fila <- deriva[deriva$aspecto == "faltantes" & deriva$columna %in% "monto", ,
                 drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.numeric(fila$valor_anterior), 0)
  expect_equal(as.numeric(fila$valor_actual), 0.25)
  expect_equal(fila$delta, 0.25)
  expect_identical(as.character(fila$severidad), "error")
  expect_identical(c(fila$universo_anterior, fila$universo_actual), c("mixto", "mixto"))
  # El rango y la cardinalidad tambien son de la tabla entera.
  rango <- deriva[deriva$aspecto == "rango" & deriva$columna %in% "id", , drop = FALSE]
  expect_identical(rango$valor_actual, "[1, 6000]")
  expect_identical(c(rango$universo_anterior, rango$universo_actual),
                   c("tabla_completa", "tabla_completa"))
  # 997 valores distintos de `monto` sobre 3000 y sobre 4500 validos.
  tasa <- deriva[deriva$aspecto == "cardinalidad" & deriva$columna %in% "monto", ,
                 drop = FALSE]
  expect_equal(nrow(tasa), 1L)
  expect_equal(as.numeric(tasa$valor_actual), 997 / 4500)
  expect_identical(c(tasa$universo_anterior, tasa$universo_actual),
                   c("tabla_completa", "tabla_completa"))
  # Lo que solo mide la muestra sigue sin compararse, y la fila lo dice, con lo
  # que queda sin ver: el texto de ausencia de las filas nuevas.
  muestra <- deriva[deriva$aspecto == "muestra", , drop = FALSE]
  expect_equal(nrow(muestra), 1L)
  expect_true(grepl("disfrazado", muestra$descripcion, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("(reproducible)", muestra$evidencia, fixed = TRUE, useBytes = TRUE))
  expect_false(any(deriva$aspecto %in% c("patron", "hallazgo", "severidad_hallazgo")))
  expect_false(any(deriva$aspecto == "faltantes" & deriva$columna %in% "estado"))
})

test_that("un NULL que pasa a centinela o a texto de ausencia no es una mejora", {
  n <- 3000L
  anterior <- data.frame(
    id = seq_len(n), edad = 18L + (seq_len(n) %% 70L),
    estado = c("activo", "baja", "suspendido")[1L + seq_len(n) %% 3L],
    stringsAsFactors = FALSE
  )
  anterior$edad[seq(5L, n, 5L)] <- NA
  anterior$estado[seq(4L, n, 4L)] <- NA
  actual <- anterior
  actual$edad[is.na(actual$edad)] <- -999L
  actual$estado[is.na(actual$estado)] <- "N/D"
  con <- .cpd_duckdb(list(ene = anterior, feb = actual))
  on.exit(.cpd_cerrar(con), add = TRUE)
  deriva <- .cpd_comparar(
    .cpd_dbi(con, "ene", .cpd_enero, muestra = 500L, orden_muestra = "id"),
    .cpd_dbi(con, "feb", .cpd_febrero, muestra = 500L, orden_muestra = "id")
  )
  # El resumen SQL solo ve los NULL: 0,2 y 0,25 bajarian a 0. Con los
  # disfrazados de la muestra la proporcion de faltantes no cambia.
  expect_false(any(deriva$aspecto == "faltantes"))
  rango <- deriva[deriva$aspecto == "rango" & deriva$columna %in% "edad", , drop = FALSE]
  expect_identical(rango$valor_actual, "[-999, 87]")
  expect_identical(as.character(rango$severidad), "sospechoso")
})

test_that("la cardinalidad de un texto no se compara entre el motor y memoria", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  correos <- sprintf("u%03d@x.uy", rep(1:200, each = 2L))
  correos[c(TRUE, FALSE)] <- toupper(correos[c(TRUE, FALSE)])
  datos <- data.frame(
    id = seq_along(correos), correo = correos,
    monto = seq(10.5, 4000.5, length.out = 400L), stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, correo TEXT COLLATE NOCASE, monto REAL)")
  DBI::dbAppendTable(con, "t", datos)
  base <- .cpd_dbi(con, "t", .cpd_febrero, muestra = 200L, orden_muestra = "id")
  # La premisa: el motor cuenta con su cotejamiento.
  expect_equal(base$resumen_tabla$columnas$tasa_distintos[
    base$resumen_tabla$columnas$columna == "correo"], 0.5)
  deriva <- .cpd_comparar(.cpd_perfil(datos, .cpd_enero), base)
  expect_false(any(deriva$aspecto == "cardinalidad" & deriva$cambio == "modificado"))
  declarada <- deriva[deriva$aspecto == "cardinalidad" & deriva$cambio == "no_comparable", ,
                      drop = FALSE]
  expect_equal(nrow(declarada), 1L)
  expect_true(grepl("correo", declarada$evidencia, fixed = TRUE, useBytes = TRUE))
  # Un numero no depende del cotejamiento.
  expect_false(grepl("monto", declarada$evidencia, fixed = TRUE, useBytes = TRUE))
})

test_that("con muestra que cubre en los dos lados, la salida es la de los perfiles de muestra", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  a <- .cpd_dbi(con, "enero", .cpd_enero)
  b <- .cpd_dbi(con, "febrero", .cpd_febrero)
  directa <- suppressWarnings(suppressMessages(comparar_perfiles(a, b)))
  por_muestra <- suppressWarnings(suppressMessages(
    comparar_perfiles(a$perfil_muestra, b$perfil_muestra)
  ))
  expect_identical(directa, por_muestra)
  expect_gt(nrow(directa), 0L)
  expect_true(all(c(directa$universo_anterior, directa$universo_actual) %in%
                    c("tabla_completa", NA)))
  expect_true(any(directa$aspecto == "patron"))
})

test_that("una cifra SQL que un lado no midio se declara y no se compara", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  a <- .cpd_dbi(con, "enero", .cpd_enero, muestra = 500L, orden_muestra = "id")
  b <- .cpd_dbi(con, "febrero", .cpd_febrero, muestra = 500L, orden_muestra = "id",
                metricas = c("validos", "basicos"))
  deriva <- .cpd_comparar(a, b)
  expect_false(any(deriva$aspecto == "cardinalidad" & deriva$cambio == "modificado"))
  declarada <- deriva[deriva$aspecto == "cardinalidad" & deriva$cambio == "no_comparable", ,
                      drop = FALSE]
  expect_equal(nrow(declarada), 1L)
  expect_true(grepl("estado_sql:calculado_vs_no_solicitado", declarada$evidencia,
                    fixed = TRUE, useBytes = TRUE))
  # El rango si se midio en los dos lados.
  expect_true(any(deriva$aspecto == "rango" & deriva$columna %in% "id"))
})

test_that("contra una muestra del motor no hay cifra de tabla entera que comparar", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  a <- .cpd_dbi(con, "enero", .cpd_enero, muestra = 500L, orden_muestra = "id")
  b <- .cpd_dbi(con, "febrero", .cpd_febrero, universo = "muestra_motor",
                muestra_motor = 400)
  deriva <- .cpd_comparar(a, b)
  expect_false(any(deriva$aspecto %in% c("rango", "cardinalidad") &
                     deriva$cambio == "modificado"))
  declaradas <- deriva$evidencia[deriva$cambio == "no_comparable" &
                                   deriva$aspecto %in% c("rango", "cardinalidad")]
  expect_length(declaradas, 2L)
  expect_true(all(grepl("universo_distinto:tabla_completa_vs_muestra", declaradas,
                        fixed = TRUE, useBytes = TRUE)))
  faltantes <- deriva[deriva$aspecto == "faltantes", , drop = FALSE]
  expect_true(all(faltantes$universo_actual == "muestra"))
})

test_that("entre dos perfiles de memoria las columnas de universo dicen de que filas salio cada cifra", {
  set.seed(7301)
  n <- 200L
  anterior <- data.frame(
    codigo = sprintf("AB-%03d", sample(100:999, n, TRUE)),
    monto = round(runif(n, 1, 100), 2), stringsAsFactors = FALSE
  )
  actual <- anterior
  actual$codigo[1:40] <- sprintf("ZZ%03d", 1:40)
  actual$monto[1:30] <- NA
  entera <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero),
                          .cpd_perfil(actual, .cpd_febrero))
  expect_true(all(c("universo_anterior", "universo_actual") %in% names(entera)))
  expect_true(any(entera$aspecto == "patron"))
  expect_true(all(entera$universo_actual[entera$aspecto %in%
                                           c("patron", "faltantes", "rango")] ==
                    "tabla_completa"))
  # Con una muestra de patrones menor que la tabla, los patrones son de la
  # muestra; los faltantes siguen contando todas las filas.
  parcial <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero, muestra = 50),
                           .cpd_perfil(actual, .cpd_febrero, muestra = 50))
  expect_true(all(parcial$universo_actual[parcial$aspecto == "patron"] == "muestra"))
  expect_true(all(parcial$universo_actual[parcial$aspecto == "faltantes"] ==
                    "tabla_completa"))
  # Una fila de configuracion no describe filas.
  config <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero),
                          .cpd_perfil(actual, .cpd_febrero, umbral_faltantes_sospechoso = 0.3))
  fila <- config[config$aspecto == "configuracion_umbrales", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(is.na(fila$universo_anterior) && is.na(fila$universo_actual))
})

test_that("un perfil_dbi sin muestra se rechaza nombrando bloque_muestra", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  agregados <- .cpd_dbi(con, "enero", .cpd_enero, bloque_muestra = "solo_agregados")
  completo <- .cpd_dbi(con, "febrero", .cpd_febrero, muestra = 500L, orden_muestra = "id")
  expect_error(comparar_perfiles(agregados, completo), "bloque_muestra",
               fixed = TRUE, useBytes = TRUE)
  expect_error(comparar_perfiles(completo, agregados), "`actual`",
               fixed = TRUE, useBytes = TRUE)
})

test_that("rbind une una deriva de antes, sin columnas de universo, con una de ahora", {
  set.seed(7302)
  datos <- data.frame(x = round(runif(60), 3), y = sample(letters[1:4], 60, TRUE),
                      stringsAsFactors = FALSE)
  ahora <- suppressMessages(comparar_perfiles(
    .cpd_perfil(datos, .cpd_enero), .cpd_perfil(datos[-(1:20), ], .cpd_febrero)
  ))
  antes <- ahora
  antes$universo_anterior <- NULL
  antes$universo_actual <- NULL
  expect_false("universo_anterior" %in% names(antes))
  unida <- rbind(antes, ahora)
  expect_s3_class(unida, "deriva_perfil")
  expect_true(all(c("universo_anterior", "universo_actual") %in% names(unida)))
  expect_equal(nrow(unida), 2L * nrow(ahora))
  expect_true(all(is.na(unida$universo_anterior[seq_len(nrow(antes))])))
  expect_identical(unida$universo_actual[nrow(antes) + seq_len(nrow(ahora))],
                   ahora$universo_actual)
  expect_true(is.data.frame(attr(unida, "campos_protegidos")))
})

test_that("un rango que la proteccion omitio en el resumen SQL se declara protegido", {
  n <- 400L
  datos <- data.frame(id = seq_len(n), doc = 10000000 + seq_len(n) * 7,
                      monto = seq_len(n) / 3)
  con <- .cpd_duckdb(list(ene = datos, feb = datos[-(1:30), ]))
  on.exit(.cpd_cerrar(con), add = TRUE)
  protegido <- function(tabla, fecha) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, fecha = fecha, proteger_datos_personales = TRUE,
      columnas_personales = "doc", muestra = 100L, orden_muestra = "id",
      analizar_dependencias = FALSE
    )))
  }
  a <- protegido("ene", .cpd_enero)
  b <- protegido("feb", .cpd_febrero)
  expect_true(all(a$resumen_tabla$sql$estado[a$resumen_tabla$sql$columna == "doc" &
    a$resumen_tabla$sql$metrica %in% c("minimo", "maximo")] == "omitido_por_privacidad"))
  deriva <- suppressWarnings(suppressMessages(comparar_perfiles(a, b)))
  expect_false(any(deriva$aspecto == "rango" & deriva$columna %in% "doc"))
  protegidos <- attr(deriva, "campos_protegidos")
  doc <- protegidos[protegidos$columna == "doc" & protegidos$campo == "rango", ,
                    drop = FALSE]
  expect_setequal(doc$lado, c("anterior", "actual"))
  expect_false(anyDuplicated(protegidos) > 0L)
  # Una columna sin proteccion compara su rango de tabla entera.
  expect_true(any(deriva$aspecto == "rango" & deriva$columna %in% "id"))
})

test_that("dos muestras que cubren la tabla con muestra declarada distinta comparan patrones", {
  n <- 400L
  anterior <- data.frame(codigo = sprintf("AB-%03d", seq_len(n) %% 900L + 100L),
                         stringsAsFactors = FALSE)
  actual <- anterior
  actual$codigo[1:60] <- sprintf("ZZ%03d", 1:60)
  # `perfilar()` declara 1e5 por omision y `perfilar_dbi()` pasa Inf: con 400
  # filas los dos buscan los patrones en las mismas 400.
  deriva <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero),
                          .cpd_perfil(actual, .cpd_febrero, muestra = Inf))
  expect_false(any(deriva$aspecto == "configuracion_patrones"))
  expect_true(any(deriva$aspecto == "patron" & deriva$cambio == "aparecido"))
  # El control: una muestra que no cubre sigue siendo otra configuracion.
  parcial <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero, muestra = 100),
                           .cpd_perfil(actual, .cpd_febrero, muestra = Inf))
  expect_true(any(parcial$aspecto == "configuracion_patrones"))
  expect_false(any(parcial$aspecto == "patron"))
  # Y la base contra el archivo, con los valores por omision de cada uno.
  con <- .cpd_duckdb(list(t = actual))
  on.exit(.cpd_cerrar(con), add = TRUE)
  base <- .cpd_comparar(.cpd_perfil(anterior, .cpd_enero),
                        .cpd_dbi(con, "t", .cpd_febrero))
  expect_false(any(base$aspecto == "configuracion_patrones"))
  expect_true(any(base$aspecto == "patron" & base$cambio == "aparecido"))
})

test_that("comparar_equivalencia no compara cifras SQL de otro universo o sin valor", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  completa <- .cpd_dbi(con, "enero", .cpd_enero)
  motor <- .cpd_dbi(con, "enero", .cpd_febrero, universo = "muestra_motor",
                    muestra_motor = 400)
  sin_presupuesto <- .cpd_dbi(con, "enero", .cpd_febrero, max_consultas = 3)
  e <- as.data.frame(comparar_equivalencia(completa, motor, tolerancia = 0.01))
  validos <- e[e$columna == "monto" & e$campo %in% c("n_validos", "n_distintos", "minimo"), ]
  expect_true(all(validos$veredicto == "no_comparable"))
  expect_true(all(validos$motivo == "universo_distinto:tabla_completa_vs_muestra"))
  # El conteo de filas es de la tabla entera en los dos.
  expect_identical(as.character(e$veredicto[e$columna == "monto" & e$campo == "n"]),
                   "identico")
  agotado <- as.data.frame(comparar_equivalencia(completa, sin_presupuesto,
                                                 tolerancia = 0.01))
  expect_false(any(agotado$veredicto %in% c("identico", "equivalente",
                                            "materialmente_distinto")))
  numericas <- agotado[agotado$columna == "monto", , drop = FALSE]
  expect_true(all(numericas$motivo == "estado_sql:calculado_vs_no_disponible"))
  # El control: la misma tabla en el mismo universo se compara como siempre.
  igual <- as.data.frame(comparar_equivalencia(completa, completa, tolerancia = 0))
  expect_false(any(grepl("^(estado_sql|universo_distinto):", igual$motivo)))
})

test_that("faltantes y cardinalidad se graduan igual desde el perfil y desde el SQL", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  memoria <- .cpd_comparar(.cpd_perfil(entregas$enero, .cpd_enero),
                           .cpd_perfil(entregas$febrero, .cpd_febrero))
  base <- .cpd_comparar(
    .cpd_dbi(con, "enero", .cpd_enero, muestra = 500L, orden_muestra = "id"),
    .cpd_dbi(con, "febrero", .cpd_febrero, muestra = 500L, orden_muestra = "id")
  )
  # La regla es la misma; las cifras pueden diferir en el ultimo bit segun
  # quien divida, y por eso los valores se comparan con tolerancia.
  regla <- c("cambio", "severidad", "significativo", "descripcion")
  for (caso in list(c("faltantes", "monto"), c("cardinalidad", "monto"),
                    c("cardinalidad", "edad"))) {
    etiqueta <- paste(caso, collapse = "/")
    de_memoria <- memoria[memoria$aspecto == caso[[1L]] & memoria$columna %in% caso[[2L]], ,
                          drop = FALSE]
    de_base <- base[base$aspecto == caso[[1L]] & base$columna %in% caso[[2L]], ,
                    drop = FALSE]
    expect_equal(nrow(de_memoria), 1L, label = etiqueta)
    expect_equal(nrow(de_base), 1L, label = etiqueta)
    expect_identical(as.list(de_base[regla]), as.list(de_memoria[regla]), label = etiqueta)
    expect_equal(as.numeric(de_base$valor_actual), as.numeric(de_memoria$valor_actual),
                 label = etiqueta)
    expect_equal(de_base$delta, de_memoria$delta, label = etiqueta)
  }
  # Un cambio significativo entra en el caso: la tasa de `monto` baja once puntos.
  expect_identical(
    as.character(base$severidad[base$aspecto == "cardinalidad" & base$columna %in% "monto"]),
    "sospechoso"
  )
})

test_that("perfilar_dbi() guarda la fecha declarada tambien sin muestra", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas["enero"])
  on.exit(.cpd_cerrar(con), add = TRUE)
  agregados <- .cpd_dbi(con, "enero", .cpd_enero, bloque_muestra = "solo_agregados")
  expect_null(agregados$perfil_muestra)
  expect_s3_class(agregados$resumen_tabla$meta$fecha_hora, "POSIXct")
  expect_equal(as.numeric(agregados$resumen_tabla$meta$fecha_hora),
               as.numeric(.cpd_enero))
  expect_true(agregados$resumen_tabla$meta$fecha_declarada)
  con_muestra <- .cpd_dbi(con, "enero", .cpd_febrero, muestra = 100L, orden_muestra = "id")
  expect_identical(con_muestra$resumen_tabla$meta$fecha_hora,
                   con_muestra$perfil_muestra$meta$fecha_hora)
  # Sin fecha declarada no se inventa una.
  sin_fecha <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "enero", bloque_muestra = "solo_agregados"
  )))
  expect_null(sin_fecha$resumen_tabla$meta$fecha_hora)
})

test_that("dos estimaciones del motor no se comparan como cardinalidad de la tabla", {
  entregas <- .cpd_entregas_crecen()
  con <- .cpd_duckdb(entregas)
  on.exit(.cpd_cerrar(con), add = TRUE)
  aproximado <- function(tabla, fecha) {
    .cpd_dbi(con, tabla, fecha, muestra = 500L, orden_muestra = "id",
             estrategia_distintos = "aproximada_motor")
  }
  a <- aproximado("enero", .cpd_enero)
  b <- aproximado("febrero", .cpd_febrero)
  # La premisa: el motor estima los distintos.
  expect_true(any(a$resumen_tabla$sql$estado == "estimado_motor"))
  deriva <- .cpd_comparar(a, b)
  expect_false(any(deriva$aspecto == "cardinalidad" & deriva$cambio == "modificado"))
  declarada <- deriva$evidencia[deriva$aspecto == "cardinalidad" &
                                  deriva$cambio == "no_comparable"]
  expect_length(declarada, 1L)
  expect_true(grepl("estado_sql:estimado_motor_vs_estimado_motor", declarada,
                    fixed = TRUE, useBytes = TRUE))
})
