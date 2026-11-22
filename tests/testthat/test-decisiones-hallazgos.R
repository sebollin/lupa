testthat::skip_on_cran()

# Una tabla con hallazgos de varias clases: valores fuera de rango, faltantes y
# variantes de escritura.
.tabla_triaje <- function(semilla = 11L, n = 300L, extremo = 1e6) {
  set.seed(semilla)
  data.frame(
    id = seq_len(n),
    monto = c(stats::rnorm(n - 2L), extremo, -extremo),
    ciudad = sample(c("Montevideo", "montevideo", "Salto", NA), n, TRUE),
    stringsAsFactors = FALSE
  )
}

.perfil_triaje <- function(datos = .tabla_triaje(), nombre = "viviendas") {
  perfilar(datos, nombre = nombre, clave = "id", analizar_dependencias = FALSE)
}

test_that("el id de un hallazgo no depende del orden de las filas ni de la llamada", {
  datos <- .tabla_triaje()
  a <- identificar_hallazgos(.perfil_triaje(datos))
  b <- identificar_hallazgos(.perfil_triaje(datos[rev(seq_len(nrow(datos))), ]))
  expect_setequal(a$id_hallazgo, b$id_hallazgo)
  expect_false(anyDuplicated(a$id_hallazgo) > 0L)
  # Dos llamadas distintas sobre la misma tabla dan los mismos ids.
  expect_identical(a$id_hallazgo, identificar_hallazgos(.perfil_triaje(datos))$id_hallazgo)
})

test_that("dos tablas con los mismos hallazgos tienen ids que no chocan al unirlos", {
  datos <- .tabla_triaje()
  a <- identificar_hallazgos(.perfil_triaje(datos, "tabla_a"))
  b <- identificar_hallazgos(.perfil_triaje(datos, "tabla_b"))
  expect_identical(a$columna, b$columna)
  expect_length(intersect(a$id_hallazgo, b$id_hallazgo), 0L)
  expect_false(anyDuplicated(c(a$id_hallazgo, b$id_hallazgo)) > 0L)
  # Y una decision sobre una no toca a la otra.
  decisiones <- decidir_hallazgos(
    .perfil_triaje(datos, "tabla_a"), "falso_positivo",
    motivo = "revisado", quien = "equipo", tipo_hallazgo = "outliers"
  )
  sobre_b <- hallazgos(aplicar_decisiones(.perfil_triaje(datos, "tabla_b"), decisiones))
  expect_true(all(sobre_b$estado_decision == "pendiente"))
})

test_that("una decision descartada no cuenta como pendiente pero sigue visible con su motivo", {
  perfil <- .perfil_triaje()
  decisiones <- decidir_hallazgos(
    perfil, "propiedad_legitima", motivo = "El campo es opcional.",
    quien = "equipo", tipo_hallazgo = "faltantes", columna = "ciudad",
    fecha = as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  )
  nuevo <- aplicar_decisiones(perfil, decisiones)
  h <- hallazgos(nuevo)
  fila <- h[h$tipo_hallazgo == "faltantes", ]
  expect_identical(fila$estado_decision, "descartado_propiedad_legitima")
  expect_identical(fila$motivo_decision, "El campo es opcional.")
  expect_identical(fila$decidido_por, "equipo")
  expect_identical(fila$fecha_decision, "2026-01-02T03:04:05Z")
  expect_identical(nrow(h), nrow(hallazgos(perfil)))
  resumen <- attr(summary(nuevo), "decisiones")
  expect_identical(resumen$total[resumen$estado_decision == "descartado_propiedad_legitima"], 1L)
  salida <- paste(cli::ansi_strip(testthat::capture_messages(print(nuevo))), collapse = " ")
  expect_match(salida, "Descartados: 1")
  expect_match(salida, "Pendientes de revisar: 3")
  # Sin decisiones, la impresion y el resumen son los de siempre.
  expect_false("estado_decision" %in% names(hallazgos(perfil)))
  expect_null(attr(summary(perfil), "decisiones"))
})

test_that("un hallazgo que cambio entre corridas vuelve a revisar y no se aplica en silencio", {
  datos <- .tabla_triaje()
  decisiones <- decidir_hallazgos(
    .perfil_triaje(datos), "aceptado", motivo = "los dos extremos son conocidos",
    quien = "equipo", tipo_hallazgo = "outliers", datos = datos
  )
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  guardar_decisiones(decisiones, archivo)
  # La misma tabla: la decision vale en la corrida siguiente, leida del archivo.
  igual <- hallazgos(aplicar_decisiones(.perfil_triaje(datos), archivo, datos = datos))
  expect_identical(igual$estado_decision[igual$tipo_hallazgo == "outliers"], "aceptado")
  # Un tercer valor extremo: otra cantidad de filas afectadas.
  cambiada <- datos
  cambiada$monto[1L] <- 7e6
  h <- hallazgos(aplicar_decisiones(.perfil_triaje(cambiada), archivo))
  fila <- h[h$tipo_hallazgo == "outliers", ]
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "n_afectados")
  # Lo demas sigue como estaba.
  expect_true(all(h$estado_decision[h$tipo_hallazgo != "outliers"] == "pendiente"))
  # Otra severidad, con las mismas cifras, tambien se marca.
  alterada <- decisiones
  alterada$severidad <- "error"
  h2 <- hallazgos(aplicar_decisiones(.perfil_triaje(datos), alterada))
  expect_identical(h2$estado_decision[h2$tipo_hallazgo == "outliers"], "revisar")
  expect_match(h2$cambio_desde_decision[h2$tipo_hallazgo == "outliers"], "severidad")
  # Y el conteo pendiente vuelve a incluirlo.
  salida <- paste(cli::ansi_strip(testthat::capture_messages(
    print(aplicar_decisiones(.perfil_triaje(cambiada), archivo))
  )), collapse = " ")
  expect_match(salida, "A revisar de nuevo porque cambiaron: 1")
})

test_that("las decisiones se guardan en un CSV editable y se leen sin perder nada", {
  perfil <- .perfil_triaje()
  motivo <- paste0("Revisi", intToUtf8(243L), "n con \"comillas\", y coma")
  decisiones <- decidir_hallazgos(
    perfil, "falso_positivo", motivo = motivo, quien = "equipo",
    columna = "monto"
  )
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = "se acepta", quien = "otra",
    tipo_hallazgo = "faltantes", decisiones = decisiones
  )
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  guardar_decisiones(decisiones, archivo)
  expect_error(guardar_decisiones(decisiones, archivo), "sobrescribir")
  leidas <- leer_decisiones(archivo)
  expect_identical(as.data.frame(leidas), as.data.frame(decisiones))
  expect_identical(leidas$motivo[[1L]], motivo)
  # Una decision nueva sobre el mismo hallazgo reemplaza a la anterior.
  reemplazo <- decidir_hallazgos(
    perfil, "aceptado", motivo = "cambio de criterio", quien = "equipo",
    columna = "monto", decisiones = decisiones
  )
  expect_identical(nrow(reemplazo), nrow(decisiones))
  expect_identical(reemplazo$decision[reemplazo$columna == "monto"], "aceptado")
})

test_that("un archivo de decisiones incompleto o invalido se rechaza entero", {
  perfil <- .perfil_triaje()
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = "m", quien = "q", tipo_hallazgo = "faltantes"
  )
  sin_motivo <- decisiones
  sin_motivo$motivo <- NA_character_
  expect_error(aplicar_decisiones(perfil, sin_motivo), "motivo")
  mala <- decisiones
  mala$decision <- "tal_vez"
  expect_error(aplicar_decisiones(perfil, mala), "decision")
  repetida <- rbind(decisiones, decisiones)
  expect_error(aplicar_decisiones(perfil, repetida), "repetido")
  expect_error(decidir_hallazgos(perfil, "aceptado", motivo = "m", quien = "q"),
               "Indicar")
  expect_error(
    decidir_hallazgos(perfil, "aceptado", motivo = "m", quien = "q",
                      columna = "no_existe"), "Ning")
  expect_error(decidir_hallazgos(perfil, "aceptado", motivo = "", quien = "q",
                                 columna = "id"), "motivo")
  expect_error(leer_decisiones(tempfile(fileext = ".csv")), "existente")
})

test_that("las decisiones valen en el resultado de analizar() y salen en el informe", {
  datos <- .tabla_triaje()
  analisis <- analizar(datos, nombre = "viviendas", conservar_datos = TRUE,
                       argumentos_perfil = list(clave = "id", analizar_dependencias = FALSE))
  decisiones <- decidir_hallazgos(
    analisis, "falso_positivo", motivo = "extremos de prueba", quien = "equipo",
    tipo_hallazgo = "outliers"
  )
  nuevo <- aplicar_decisiones(analisis, decisiones)
  h <- hallazgos(nuevo)
  expect_identical(h$estado_decision[h$tipo_hallazgo == "outliers"],
                   "descartado_falso_positivo")
  archivo <- reportar(nuevo, archivo = tempfile(fileext = ".html"))
  on.exit(unlink(archivo), add = TRUE)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_match(html, "Decisiones sobre hallazgos", fixed = TRUE)
  expect_match(html, "extremos de prueba", fixed = TRUE)
  perfil <- aplicar_decisiones(.perfil_triaje(datos), decisiones, datos = datos)
  expect_identical(
    hallazgos(perfil)$estado_decision[hallazgos(perfil)$tipo_hallazgo == "outliers"],
    "descartado_falso_positivo"
  )
  # Sin decisiones el informe no trae la seccion.
  sin <- reportar(perfil <- .perfil_triaje(datos), archivo = tempfile(fileext = ".html"))
  on.exit(unlink(sin), add = TRUE)
  expect_false(grepl("Decisiones sobre hallazgos",
                     paste(readLines(sin, warn = FALSE), collapse = "\n"), fixed = TRUE))
})

test_that("las decisiones valen en perfilar_por()", {
  datos <- .tabla_triaje()
  por_grupo <- perfilar_por(datos, por = "ciudad", clave = "id")
  # El resultado no guarda el nombre de la tabla: se pide en vez de dejarlo vacio.
  expect_error(identificar_hallazgos(por_grupo), "nombre_tabla")
  ids <- identificar_hallazgos(por_grupo, nombre_tabla = "viviendas")
  expect_false(anyDuplicated(ids$id_hallazgo) > 0L)
  decisiones <- decidir_hallazgos(
    por_grupo, "propiedad_legitima", motivo = "es asi por grupo", quien = "equipo",
    columna = "monto", nombre_tabla = "viviendas"
  )
  expect_false(anyNA(decisiones$tabla))
  nuevo <- aplicar_decisiones(por_grupo, decisiones, nombre_tabla = "viviendas")
  # Las filas de `perfilar_por()` son las del grupo: no hay huella de valores, y
  # un hallazgo de valores no se puede verificar.
  expect_true(all(nuevo$estado_decision[nuevo$columna == "monto"] ==
                    "decidido_sin_verificar"))
  expect_true(all(nuevo$decision[nuevo$columna == "monto"] == "propiedad_legitima"))
  expect_true(all(nuevo$estado_decision[nuevo$columna != "monto"] == "pendiente"))
  # Aplicar dos veces no duplica las columnas.
  expect_identical(names(aplicar_decisiones(nuevo, decisiones)), names(nuevo))
  # Otra tabla con los mismos grupos y columnas no hereda la decision.
  otra <- aplicar_decisiones(por_grupo, decisiones, nombre_tabla = "locales")
  expect_true(all(otra$estado_decision == "pendiente"))
})

test_that("las decisiones valen en perfilar_dbi() y en perfilar_coleccion() con ids que no chocan", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  datos <- .tabla_triaje()
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "uno", datos)
  DBI::dbWriteTable(con, "dos", datos)
  perfil_dbi <- perfilar_dbi(con, "uno")
  decisiones <- decidir_hallazgos(
    perfil_dbi, "falso_positivo", motivo = "visto en el motor", quien = "equipo",
    tipo_hallazgo = "faltantes"
  )
  h <- hallazgos(aplicar_decisiones(perfil_dbi, decisiones))
  expect_true(any(h$decision == "falso_positivo", na.rm = TRUE))
  coleccion <- perfilar_coleccion(
    coleccion(con, tablas = c("uno", "dos")), conservar_perfiles = TRUE
  )
  ids <- identificar_hallazgos(coleccion)
  expect_false(anyDuplicated(ids$id_hallazgo) > 0L)
  expect_setequal(unique(ids$tabla), c("uno", "dos"))
  sobre_uno <- decidir_hallazgos(
    coleccion, "aceptado", motivo = "solo la primera", quien = "equipo",
    tabla = "uno"
  )
  h <- hallazgos(aplicar_decisiones(coleccion, sobre_uno))
  expect_true(all(h$decision[h$tabla == "uno"] == "aceptado"))
  expect_true(all(h$estado_decision[h$tabla == "dos"] == "pendiente"))
})

test_that("el perfil sin decisiones no cambia", {
  datos <- .tabla_triaje()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  perfil <- perfilar(datos, nombre = "t", fecha = fecha, analizar_dependencias = FALSE)
  expect_false("decisiones_hallazgos" %in% names(perfil))
  expect_false("estado_decision" %in% names(hallazgos(perfil)))
  aplicado <- aplicar_decisiones(perfil, decidir_hallazgos(
    perfil, "aceptado", motivo = "m", quien = "q", columna = "id"
  ))
  aplicado$decisiones_hallazgos <- NULL
  expect_identical(aplicado, perfil)
})

# ---- Identidad con esquema -------------------------------------------------

test_that("DBI::Id: las tablas de esquemas distintos no comparten identidad", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH ':memory:' AS s1")
  DBI::dbExecute(con, "ATTACH ':memory:' AS s2")
  datos <- data.frame(a = c(rep(NA, 10), 1:90), b = 1:100)
  datos$a[11:30] <- NA
  for (id in list(c("s1", "t"), c("s2", "t"), c("s1", "u"))) {
    DBI::dbWriteTable(con, DBI::Id(schema = id[[1L]], table = id[[2L]]), datos)
  }
  p1 <- perfilar_dbi(con, DBI::Id(schema = "s1", table = "t"))
  p2 <- perfilar_dbi(con, DBI::Id(schema = "s2", table = "t"))
  p3 <- perfilar_dbi(con, DBI::Id(schema = "s1", table = "u"))
  expect_identical(p1$resumen_tabla$meta$tabla, "s1.t")
  ids <- lapply(list(p1, p2, p3), function(p) identificar_hallazgos(p)$id_hallazgo)
  expect_length(intersect(ids[[1L]], ids[[2L]]), 0L)
  expect_length(intersect(ids[[1L]], ids[[3L]]), 0L)
  decisiones <- decidir_hallazgos(
    p1, "falso_positivo", motivo = "m", quien = "q", tabla = "s1.t",
    columna = "a"
  )
  for (otra in list(p2, p3)) {
    h <- hallazgos(aplicar_decisiones(otra, decisiones))
    expect_true(all(h$estado_decision == "pendiente"))
  }
  h1 <- hallazgos(aplicar_decisiones(p1, decisiones))
  expect_true(any(h1$decision == "falso_positivo", na.rm = TRUE))
})

test_that("perfilar_por(): dos tablas con los mismos grupos no comparten decisiones", {
  datos <- .tabla_triaje()
  otra <- datos
  otra$monto[1:30] <- NA
  a <- perfilar_por(datos, por = "ciudad", clave = "id")
  b <- perfilar_por(otra, por = "ciudad", clave = "id")
  decisiones <- decidir_hallazgos(
    a, "falso_positivo", motivo = "m", quien = "q", columna = "monto",
    nombre_tabla = "tabla_a"
  )
  expect_true(all(decisiones$tabla == "tabla_a"))
  sobre_b <- aplicar_decisiones(b, decisiones, nombre_tabla = "tabla_b")
  expect_true(all(sobre_b$estado_decision == "pendiente"))
  expect_error(aplicar_decisiones(b, decisiones), "nombre_tabla")
})

# ---- Cambios que la cuenta no ve -------------------------------------------

test_that("cambian los valores extremos con la misma cantidad: vuelve a revisar con los datos", {
  datos <- .tabla_triaje()
  decisiones <- decidir_hallazgos(
    .perfil_triaje(datos), "aceptado", motivo = "extremos conocidos", quien = "q",
    tipo_hallazgo = "outliers", datos = datos
  )
  expect_false(is.na(decisiones$huella_valores))
  cambiada <- datos
  cambiada$monto[cambiada$monto == 1e6] <- 5e6
  cambiada$monto[cambiada$monto == -1e6] <- -5e6
  p <- .perfil_triaje(cambiada)
  igual <- hallazgos(aplicar_decisiones(p, decisiones, datos = cambiada))
  fila <- igual[igual$tipo_hallazgo == "outliers", ]
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "valores")
  # Los mismos datos: la decision vale, aunque las filas lleguen en otro orden.
  rev <- datos[rev(seq_len(nrow(datos))), ]
  ok <- hallazgos(aplicar_decisiones(.perfil_triaje(rev), decisiones, datos = rev))
  expect_identical(ok$estado_decision[ok$tipo_hallazgo == "outliers"], "aceptado")
  # Sin los datos, lo que se vio al decidir ya no se puede confirmar: revisar.
  sin <- hallazgos(aplicar_decisiones(p, decisiones))
  fila <- sin[sin$tipo_hallazgo == "outliers", ]
  # Lo que existia al decidir y ya no se puede calcular no es un cambio: queda
  # sin verificar, y lo dice.
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "valores o estructura: no se pudo calcular ahora")
})

test_that("con clave, las mismas cifras sobre otras filas vuelven a revisar", {
  n <- 200L
  a <- data.frame(id = seq_len(n), x = seq_len(n) * 1.5)
  a$x[1:30] <- NA
  b <- a
  b$x[1:30] <- seq_len(30) * 1.5
  b$x[171:200] <- NA
  pa <- perfilar(a, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  pb <- perfilar(b, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    pa, "propiedad_legitima", motivo = "las primeras 30 son de prueba",
    quien = "q", tipo_hallazgo = "faltantes"
  )
  expect_false(is.na(decisiones$huella_filas))
  h <- hallazgos(aplicar_decisiones(pb, decisiones))
  fila <- h[h$tipo_hallazgo == "faltantes", ]
  expect_identical(fila$n_afectados, hallazgos(pa)$n_afectados[hallazgos(pa)$tipo_hallazgo == "faltantes"])
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "filas")
  # Las mismas filas, tabla en otro orden: la decision vale.
  r <- a[rev(seq_len(n)), ]
  pr <- perfilar(r, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  hr <- hallazgos(aplicar_decisiones(pr, decisiones))
  expect_identical(hr$estado_decision[hr$tipo_hallazgo == "faltantes"],
                   "descartado_propiedad_legitima")
})

test_that("OutDec y el orden de los ejemplos no marcan como cambiado un hallazgo igual", {
  datos <- .tabla_triaje()
  perfil <- .perfil_triaje(datos)
  decisiones <- decidir_hallazgos(
    perfil, "propiedad_legitima", motivo = "m", quien = "q", columna = "id"
  )
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = "m", quien = "q",
    tipo_hallazgo = "mayusculas_inconsistentes", decisiones = decisiones
  )
  anterior <- options(OutDec = ",")
  on.exit(options(anterior), add = TRUE)
  coma <- .perfil_triaje(datos)
  permutada <- .perfil_triaje(datos[rev(seq_len(nrow(datos))), ])
  options(anterior)
  for (p in list(coma, permutada)) {
    h <- hallazgos(aplicar_decisiones(p, decisiones))
    expect_false(any(h$estado_decision == "revisar"))
    expect_true(any(h$estado_decision != "pendiente"))
  }
})

test_that("columnas homonimas: sin huella que comparar, la decision vuelve a revisar", {
  n <- 60L
  armar <- function(orden) {
    cols <- list(
      c(stats::rnorm(n - 2L), 1e6, -1e6), c(stats::rnorm(n - 2L), 2e6, -2e6)
    )[orden]
    d <- data.frame(cols[[1L]], cols[[2L]], seq_len(n))
    names(d) <- c("x", "x", "y")
    d
  }
  set.seed(5)
  d1 <- armar(1:2)
  p1 <- perfilar(d1, nombre = "t", analizar_dependencias = FALSE)
  ids <- identificar_hallazgos(p1)
  ids <- ids[ids$tipo_hallazgo == "outliers", ]
  expect_length(unique(ids$id_hallazgo), 2L)
  decisiones <- decidir_hallazgos(
    p1, "aceptado", motivo = "m", quien = "q", id_hallazgo = ids$id_hallazgo[[1L]],
    datos = d1
  )
  d2 <- d1[c(2L, 1L, 3L)]
  names(d2) <- c("x", "x", "y")
  p2 <- perfilar(d2, nombre = "t", analizar_dependencias = FALSE)
  # Con los datos: la columna que se decidio esta ahora en segundo lugar.
  h <- hallazgos(aplicar_decisiones(p2, decisiones, datos = d2))
  fila <- h[h$id_hallazgo == ids$id_hallazgo[[1L]], ]
  expect_identical(fila$estado_decision, "revisar")
  # Sin los datos, no hay con que distinguirlas: tampoco se aplica en silencio.
  sin <- suppressWarnings(hallazgos(aplicar_decisiones(p2, decisiones)))
  expect_identical(sin$estado_decision[sin$id_hallazgo == ids$id_hallazgo[[1L]]],
                   "revisar")
  # Con las columnas como estaban, la decision vale.
  ok <- hallazgos(aplicar_decisiones(p1, decisiones, datos = d1))
  expect_identical(ok$estado_decision[ok$id_hallazgo == ids$id_hallazgo[[1L]]],
                   "aceptado")
})

# ---- El archivo ------------------------------------------------------------

.decisiones_para_archivo <- function() {
  perfil <- .perfil_triaje()
  motivo <- paste0("Revisi", intToUtf8(243L), "n con ", intToUtf8(241L))
  decidir_hallazgos(
    perfil, "falso_positivo", motivo = motivo, quien = "equipo",
    columna = c("monto", "ciudad")
  )
}

test_that("leer_decisiones() lee Windows-1252, punto y coma y BOM, y nunca devuelve vacio en silencio", {
  decisiones <- .decisiones_para_archivo()
  esperado <- as.data.frame(decisiones)
  base <- tempfile(fileext = ".csv")
  on.exit(unlink(base), add = TRUE)
  # Punto y coma, como lo escribe `guardar_decisiones()` para una planilla.
  guardar_decisiones(decisiones, base, separador = ";")
  crudo <- readBin(base, "raw", file.info(base)$size)
  expect_identical(crudo[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
  expect_identical(as.data.frame(leer_decisiones(base)), esperado)
  # Coma sin marca de orden de bytes.
  sin_bom <- tempfile(fileext = ".csv")
  on.exit(unlink(sin_bom), add = TRUE)
  guardar_decisiones(decisiones, sin_bom, sobrescribir = TRUE)
  escribir <- readBin(sin_bom, "raw", file.info(sin_bom)$size)
  writeBin(escribir[-(1:3)], sin_bom)
  expect_identical(as.data.frame(leer_decisiones(sin_bom)), esperado)
  # Windows-1252: se lee y se avisa; no son cero filas.
  cp <- tempfile(fileext = ".csv")
  on.exit(unlink(cp), add = TRUE)
  texto <- rawToChar(readBin(base, "raw", file.info(base)$size)[-(1:3)])
  Encoding(texto) <- "UTF-8"
  writeBin(charToRaw(iconv(texto, "UTF-8", "CP1252")), cp)
  expect_warning(leido <- leer_decisiones(cp), "Windows-1252")
  expect_identical(nrow(leido), nrow(decisiones))
  expect_identical(leido$motivo, decisiones$motivo)
  # Basura que no es un CSV de decisiones: error, no una tabla vacia.
  malo <- tempfile(fileext = ".csv")
  on.exit(unlink(malo), add = TRUE)
  writeBin(as.raw(c(0xff, 0xfe, 0x00, 0x41, 0x00)), malo)
  expect_error(suppressWarnings(leer_decisiones(malo)))
  vacio <- tempfile(fileext = ".csv")
  on.exit(unlink(vacio), add = TRUE)
  file.create(vacio)
  expect_error(leer_decisiones(vacio), "vac")
})

test_that("el retorno de carro y el salto de linea del motivo vuelven byte a byte", {
  perfil <- .perfil_triaje()
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  for (motivo in c("uno\ndos", "uno\r\ndos", "uno\rdos")) {
    decisiones <- decidir_hallazgos(
      perfil, "aceptado", motivo = motivo, quien = "q", columna = "id"
    )
    guardar_decisiones(decisiones, archivo, sobrescribir = TRUE)
    expect_identical(leer_decisiones(archivo)$motivo, rep(motivo, nrow(decisiones)))
    # El archivo se escribe como bytes: un salto dentro de un campo no se
    # convierte en retorno de carro mas salto (en Windows, un archivo en modo
    # texto lo hacia), y ningun registro termina en retorno de carro.
    crudo <- readBin(archivo, "raw", n = file.info(archivo)$size)
    if (!grepl("\r", motivo, fixed = TRUE)) {
      expect_false(any(crudo == as.raw(0x0d)))
    }
  }
  # Un acento del motivo vuelve igual, tambien con la sesion en `C`.
  motivo <- paste0("acento ", intToUtf8(0xf1L))
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = motivo, quien = "q", columna = "id"
  )
  guardar_decisiones(decisiones, archivo, sobrescribir = TRUE)
  expect_identical(leer_decisiones(archivo)$motivo[[1L]], motivo)
  local({
    viejo_locale <- Sys.getlocale("LC_CTYPE")
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", viejo_locale)), add = TRUE)
    suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
    guardar_decisiones(decisiones, archivo, sobrescribir = TRUE)
  })
  expect_identical(leer_decisiones(archivo)$motivo[[1L]], motivo)
  # Un archivo de planilla, con CRLF entre registros: los registros se leen igual.
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = "m", quien = "q", columna = c("id", "monto")
  )
  guardar_decisiones(decisiones, archivo, sobrescribir = TRUE)
  texto <- readChar(archivo, file.info(archivo)$size, useBytes = TRUE)
  writeBin(charToRaw(gsub("\n", "\r\n", texto, fixed = TRUE)), archivo)
  expect_identical(as.data.frame(leer_decisiones(archivo)), as.data.frame(decisiones))
})

# ---- Nada se aplica en silencio sin verificar ------------------------------

test_that("sin datos ni clave la decision queda decidida sin verificar, y se cuenta aparte", {
  datos <- .tabla_triaje()
  p <- perfilar(datos, nombre = "viviendas", analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    p, "aceptado", motivo = "m", quien = "q", tipo_hallazgo = "outliers"
  )
  expect_true(is.na(decisiones$huella_valores) && is.na(decisiones$huella_filas))
  nuevo <- aplicar_decisiones(p, decisiones)
  h <- hallazgos(nuevo)
  fila <- h[h$tipo_hallazgo == "outliers", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_identical(fila$decision, "aceptado")
  expect_match(fila$verificacion, "no se puede calcular")
  resumen <- attr(summary(nuevo), "decisiones")
  expect_identical(resumen$total[resumen$estado_decision == "decidido_sin_verificar"], 1L)
  expect_identical(resumen$total[resumen$estado_decision == "aceptado"], 0L)
  salida <- paste(cli::ansi_strip(testthat::capture_messages(print(nuevo))), collapse = " ")
  expect_match(salida, "Decididos sin verificar: 1")
  expect_match(salida, "Aceptados: 0")
  # Con datos al decidir y al aplicar, la misma decision queda verificada.
  verificada <- decidir_hallazgos(
    p, "aceptado", motivo = "m", quien = "q", tipo_hallazgo = "outliers", datos = datos
  )
  h2 <- hallazgos(aplicar_decisiones(p, verificada, datos = datos))
  expect_identical(h2$estado_decision[h2$tipo_hallazgo == "outliers"], "aceptado")
  # Un archivo sin las columnas de huella no se puede verificar: tampoco se aplica
  # en silencio.
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  guardar_decisiones(verificada, archivo)
  texto <- readLines(archivo, warn = FALSE)
  crudo <- utils::read.csv(archivo, colClasses = "character", na.strings = "",
                           fileEncoding = "UTF-8-BOM")
  crudo$huella_valores <- NULL
  crudo$huella_filas <- NULL
  utils::write.csv(crudo, archivo, row.names = FALSE, na = "")
  h3 <- hallazgos(aplicar_decisiones(p, archivo, datos = datos))
  expect_identical(h3$estado_decision[h3$tipo_hallazgo == "outliers"],
                   "decidido_sin_verificar")
})

test_that("la traza truncada no se verifica en silencio", {
  n <- 400L
  datos <- data.frame(id = seq_len(n), x = seq_len(n) * 1.5)
  datos$x[1:200] <- NA
  p <- perfilar(datos, nombre = "t", clave = "id", max_filas_hallazgo = 50L,
                analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    p, "propiedad_legitima", motivo = "m", quien = "q", tipo_hallazgo = "faltantes"
  )
  h <- hallazgos(aplicar_decisiones(p, decisiones))
  fila <- h[h$tipo_hallazgo == "faltantes", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "trunca")
})

test_that("la huella distingue tipo, NA, representacion y separadores", {
  huella <- function(...) {
    lupa:::.huella_multiconjunto(lupa:::.tuplas_huella(list(...)))
  }
  expect_false(identical(huella(NA_character_), huella("NA")))
  expect_false(identical(huella(1), huella("1")))
  expect_false(identical(huella(1L), huella(1)))
  expect_false(identical(huella(as.Date("2026-01-02")), huella("2026-01-02")))
  expect_false(identical(huella(factor("a")), huella("a")))
  expect_false(identical(huella(0.3), huella(0.1 + 0.2)))
  expect_false(identical(huella(NaN), huella("NaN")))
  expect_false(identical(huella(NaN), huella(NA_real_)))
  expect_false(identical(huella("a\037b", "c"), huella("a", "b\037c")))
  expect_false(identical(huella("a\037b"), huella("a", "b")))
  expect_false(identical(huella("ab", "c"), huella("a", "bc")))
  # Lo que es igual, da lo mismo, en cualquier orden.
  expect_identical(huella(c("x", "y", NA)), huella(c(NA, "y", "x")))
  expect_false(identical(huella(c("x", "x")), huella(c("x", "y"))))
  skip_if_not_installed("bit64")
  expect_false(identical(huella(bit64::as.integer64(5)), huella("5")))
})

test_that("un nombre generico o heredado de una version anterior pide nombre_tabla", {
  datos <- .tabla_triaje()
  p <- perfilar(datos, nombre = "datos", analizar_dependencias = FALSE)
  expect_error(identificar_hallazgos(p), "nombre_tabla")
  expect_error(decidir_hallazgos(p, "aceptado", motivo = "m", quien = "q",
                                 columna = "id"), "nombre_tabla")
  expect_true(nrow(identificar_hallazgos(p, nombre_tabla = "viviendas")) > 0L)
  viejo <- .perfil_triaje()
  viejo$meta$nombre <- "<S4 class 'Id' [package \"DBI\"] with 1 slot>"
  expect_error(identificar_hallazgos(viejo), "nombre_tabla")
  # Con nombre propio, las decisiones se recuerdan al aplicar.
  d <- decidir_hallazgos(p, "aceptado", motivo = "m", quien = "q",
                         columna = "id", nombre_tabla = "viviendas")
  h <- hallazgos(aplicar_decisiones(p, d, nombre_tabla = "viviendas"))
  expect_true(any(h$decision == "aceptado", na.rm = TRUE))
})

test_that("DBI::Id con un punto en el componente no se confunde con esquema y tabla", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH ':memory:' AS s1")
  datos <- data.frame(a = c(rep(NA, 20), 1:80))
  DBI::dbWriteTable(con, DBI::Id(schema = "s1", table = "t"), datos)
  DBI::dbWriteTable(con, DBI::Id(table = "s1.t"), datos)
  p1 <- perfilar_dbi(con, DBI::Id(schema = "s1", table = "t"))
  p2 <- perfilar_dbi(con, DBI::Id(table = "s1.t"))
  expect_false(identical(p1$resumen_tabla$meta$tabla, p2$resumen_tabla$meta$tabla))
  expect_length(intersect(identificar_hallazgos(p1)$id_hallazgo,
                          identificar_hallazgos(p2)$id_hallazgo), 0L)
})

# ---- Lo que devuelve una planilla -------------------------------------------

test_that("n_afectados y fecha de planilla se leen bien o fallan claro", {
  perfil <- .perfil_triaje()
  decisiones <- decidir_hallazgos(
    perfil, "aceptado", motivo = "m", quien = "q", columna = "monto",
    fecha = as.POSIXct("2026-01-25 10:30:00", tz = "UTC")
  )
  expect_identical(decisiones$fecha, rep("2026-01-25T10:30:00Z", nrow(decisiones)))
  lectura <- function(n = NULL, fecha = NULL) {
    d <- as.data.frame(decisiones)
    if (!is.null(n)) d$n_afectados <- n
    if (!is.null(fecha)) d$fecha <- fecha
    archivo <- tempfile(fileext = ".csv")
    on.exit(unlink(archivo), add = TRUE)
    utils::write.table(d, archivo, sep = ";", row.names = FALSE, na = "",
                       qmethod = "double")
    leer_decisiones(archivo)
  }
  expect_identical(lectura(n = "1.000")$n_afectados[[1L]], 1000)
  expect_identical(lectura(n = "12.345.678")$n_afectados[[1L]], 12345678)
  expect_identical(lectura(n = "0,5")$n_afectados[[1L]], 0.5)
  expect_identical(lectura(n = "7")$n_afectados[[1L]], 7)
  expect_error(lectura(n = "1 000"), "n_afectados")
  expect_error(lectura(n = "mil"), "n_afectados")
  expect_identical(lectura(fecha = "25/01/2026 10:30")$fecha[[1L]], "2026-01-25T10:30:00Z")
  expect_identical(lectura(fecha = "2026-01-25 07:30:00-03:00")$fecha[[1L]],
                   "2026-01-25T10:30:00Z")
  expect_identical(lectura(fecha = "2026-01-25T10:30:00Z")$fecha[[1L]],
                   "2026-01-25T10:30:00Z")
  expect_error(lectura(fecha = "02/01/2026"), "ambigua")
  expect_error(lectura(fecha = "46047.4375"), "reconoce")
  expect_error(lectura(fecha = "enero"), "reconoce")
})

# ---- Uso real --------------------------------------------------------------

test_that("hallazgos() acepta el resultado de perfilar_por(), con y sin decisiones", {
  datos <- .tabla_triaje()
  por <- perfilar_por(datos, por = "ciudad", clave = "id")
  h <- hallazgos(por)
  expect_s3_class(h, "tabla_lupa")
  expect_identical(nrow(h), nrow(por))
  expect_true(all(c("grupo", "n_filas_grupo", "columna", "tipo_hallazgo") %in% names(h)))
  expect_false("estado_decision" %in% names(h))
  decisiones <- decidir_hallazgos(
    por, "propiedad_legitima", motivo = "m", quien = "q", columna = "monto",
    nombre_tabla = "viviendas"
  )
  con <- hallazgos(aplicar_decisiones(por, decisiones, nombre_tabla = "viviendas"))
  expect_true("estado_decision" %in% names(con))
  expect_true(all(con$estado_decision[con$columna == "monto"] != "pendiente"))
})

test_that("una celda que pasa de NA a centinela vuelve a revisar aunque la cifra sea la misma", {
  skip_if_not_installed("RSQLite")
  datos <- data.frame(id = seq_len(200L), x = c(rep(NA_real_, 30L), rep(1, 170L)))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  perfilar_t <- function() {
    perfilar_dbi(con, "t", muestra = 40L, orden_muestra = "id", clave = "id",
                 analizar_dependencias = FALSE)
  }
  p1 <- perfilar_t()
  decisiones <- decidir_hallazgos(
    p1, "propiedad_legitima", motivo = "campo opcional", quien = "q",
    tipo_hallazgo = "faltantes"
  )
  expect_false(is.na(decisiones$detalle))
  h1 <- hallazgos(aplicar_decisiones(p1, decisiones))
  expect_identical(h1$estado_decision[h1$tipo_hallazgo == "faltantes"],
                   "descartado_propiedad_legitima")
  DBI::dbExecute(con, "UPDATE t SET x = 999 WHERE id = 1")
  p2 <- perfilar_t()
  f2 <- hallazgos(p2)
  expect_identical(f2$n_afectados[f2$tipo_hallazgo == "faltantes"], 30)
  h2 <- hallazgos(aplicar_decisiones(p2, decisiones))
  fila <- h2[h2$tipo_hallazgo == "faltantes", ]
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "detalle")
})

test_that("una coleccion sin perfiles conservados avisa en vez de fallar con un error generico", {
  skip_if_not_installed("duckdb")
  datos <- .tabla_triaje()
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "uno", datos)
  coleccion_sin <- perfilar_coleccion(coleccion(con, tablas = "uno"))
  decisiones <- decidir_hallazgos(
    perfilar_coleccion(coleccion(con, tablas = "uno"), conservar_perfiles = TRUE),
    "aceptado", motivo = "m", quien = "q", tabla = "uno"
  )
  expect_warning(devuelto <- aplicar_decisiones(coleccion_sin, decisiones),
                 "conservar_perfiles")
  expect_identical(devuelto, coleccion_sin)
  expect_error(
    suppressWarnings(decidir_hallazgos(coleccion_sin, "aceptado", motivo = "m",
                                       quien = "q", tabla = "uno")),
    "conservar_perfiles"
  )
})

test_that("un hallazgo descartado deja sus acciones del plan sin aplicar, con una nota", {
  n <- 200L
  set.seed(3)
  datos <- data.frame(
    id = seq_len(n), monto = c(stats::rnorm(n - 2L), 1e6, -1e6),
    ciudad = sample(c("Montevideo", "Montevideo ", "Salto"), n, TRUE),
    stringsAsFactors = FALSE
  )
  analisis <- analizar(datos, nombre = "viviendas", conservar_datos = TRUE,
                       argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  plan0 <- analisis$plan_limpieza
  expect_true(any(plan0$hallazgo == "outliers"))
  expect_true(any(plan0$hallazgo == "espacios_sobrantes" & plan0$aplicar))
  por_tipo <- function(tipo, decision) {
    decidir_hallazgos(
      analisis, decision, motivo = "extremos de verdad", quien = "equipo",
      tipo_hallazgo = tipo, fecha = as.POSIXct("2026-01-02", tz = "UTC")
    )
  }
  # Descartado: las acciones de ese hallazgo no se aplican y lo dicen.
  descartado <- aplicar_decisiones(analisis, por_tipo("outliers", "propiedad_legitima"))
  plan <- descartado$plan_limpieza
  de_outliers <- plan$hallazgo == "outliers"
  expect_false(any(plan$aplicar[de_outliers]))
  expect_true(all(grepl("propiedad_legitima", plan$justificacion[de_outliers])))
  expect_identical(plan$aplicar[!de_outliers], plan0$aplicar[!de_outliers])
  # Aceptado: el plan no cambia.
  aceptado <- aplicar_decisiones(analisis, por_tipo("outliers", "aceptado"))
  expect_identical(aceptado$plan_limpieza, plan0)
  # Sin verificar (sin clave en el analisis): tampoco cambia.
  sin_clave <- analizar(datos, nombre = "viviendas", argumentos_perfil = list(
    analizar_dependencias = FALSE
  ))
  d <- decidir_hallazgos(sin_clave, "falso_positivo", motivo = "m", quien = "q",
                         tipo_hallazgo = "outliers")
  sin_verificar <- aplicar_decisiones(sin_clave, d)$plan_limpieza
  expect_identical(sin_verificar$aplicar, sin_clave$plan_limpieza$aplicar)
  expect_true(all(grepl("revisala antes de limpiar",
                        sin_verificar$justificacion[sin_verificar$hallazgo == "outliers"])))
  # Aplicar dos veces no repite la nota.
  dos <- aplicar_decisiones(descartado, por_tipo("outliers", "propiedad_legitima"))
  expect_identical(dos$plan_limpieza$justificacion, plan$justificacion)
})

# ---- El plan se deriva; cada componente se compara o se declara ------------

test_that("aplicar_decisiones() sobre un perfilar_por() vacio devuelve la tabla sin hallazgos", {
  datos <- .tabla_triaje()
  vacio <- perfilar_por(datos, por = "ciudad", min_filas = 1000L)
  expect_identical(nrow(vacio), 0L)
  decisiones <- decidir_hallazgos(
    .perfil_triaje(datos), "aceptado", motivo = "m", quien = "q", columna = "id"
  )
  salida <- aplicar_decisiones(vacio, decisiones, nombre_tabla = "viviendas")
  expect_identical(nrow(salida), 0L)
  expect_identical(nrow(hallazgos(salida)), 0L)
})

test_that("perfilar_por() con decisiones se imprime, resume, informa y devuelve como un perfil", {
  datos <- .tabla_triaje()
  por <- perfilar_por(datos, por = "ciudad", clave = "id")
  decisiones <- decidir_hallazgos(
    por, "propiedad_legitima", motivo = "es asi", quien = "q", columna = "monto",
    nombre_tabla = "viviendas"
  )
  con <- aplicar_decisiones(por, decisiones, nombre_tabla = "viviendas")
  perfil <- aplicar_decisiones(
    .perfil_triaje(datos), decidir_hallazgos(
      .perfil_triaje(datos), "propiedad_legitima", motivo = "es asi", quien = "q",
      columna = "monto"
    )
  )
  # La misma forma de tabla: clase y atributos.
  h <- hallazgos(con)
  expect_identical(class(h), class(hallazgos(perfil)))
  expect_identical(
    setdiff(names(attributes(h)), c("names", "row.names", "class")),
    setdiff(names(attributes(hallazgos(perfil))), c("names", "row.names", "class"))
  )
  salida <- paste(cli::ansi_strip(capture.output(print(con), type = "message")),
                  collapse = " ")
  salida <- paste(salida, paste(cli::ansi_strip(capture.output(print(con))),
                                collapse = " "))
  expect_match(salida, "Decisiones sobre hallazgos")
  expect_match(salida, "Pendientes de revisar")
  resumen <- summary(con)
  expect_false(is.null(attr(resumen, "decisiones")))
  archivo <- reportar(con, archivo = tempfile(fileext = ".html"))
  on.exit(unlink(archivo), add = TRUE)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")
  expect_match(html, "Decisiones sobre hallazgos", fixed = TRUE)
  # Sin decisiones, la impresion es la de siempre.
  expect_identical(capture.output(print(por)), capture.output(print.data.frame(por)))
  expect_null(attr(summary(por), "decisiones"))
})

test_that("un cambio entre centinelas con la misma cuenta vuelve a revisar", {
  skip_if_not_installed("RSQLite")
  datos <- data.frame(id = seq_len(200L), x = c(NA, rep(NA_real_, 29L), rep(1, 170L)))
  datos$x[1L] <- 999
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  perfilar_t <- function() {
    perfilar_dbi(con, "t", muestra = 40L, orden_muestra = "id", clave = "id",
                 analizar_dependencias = FALSE)
  }
  p1 <- perfilar_t()
  decisiones <- decidir_hallazgos(
    p1, "propiedad_legitima", motivo = "m", quien = "q",
    tipo_hallazgo = c("faltantes", "faltantes_disfrazados")
  )
  expect_true(all(!is.na(decisiones$detalle)))
  DBI::dbExecute(con, "UPDATE t SET x = -9 WHERE id = 1")
  h <- hallazgos(aplicar_decisiones(perfilar_t(), decisiones))
  expect_identical(h$estado_decision[h$tipo_hallazgo == "faltantes"], "revisar")
  # `faltantes_disfrazados` requiere los valores: el detalle que cambio no es un
  # requerido, se declara, y sin datos no se puede verificar.
  fd <- h[h$tipo_hallazgo == "faltantes_disfrazados", ]
  expect_identical(fd$estado_decision, "decidido_sin_verificar")
  expect_match(fd$verificacion, "detalle: cambiaron \\(no requerido")
  # Con la misma tabla, sigue vigente.
  ok <- hallazgos(aplicar_decisiones(p1, decisiones))
  expect_false(any(ok$estado_decision == "revisar"))
  # Un archivo sin `detalle` (o con el detalle vacio) no vale como coincidencia:
  # se declara que falta ese componente.
  sin_detalle <- as.data.frame(decisiones)
  sin_detalle$detalle <- NULL
  h2 <- hallazgos(aplicar_decisiones(p1, sin_detalle))
  fila <- h2[h2$tipo_hallazgo == "faltantes", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "detalle")
  vacio <- as.data.frame(decisiones)
  vacio$detalle <- NA_character_
  h3 <- hallazgos(aplicar_decisiones(p1, vacio))
  expect_identical(h3$estado_decision[h3$tipo_hallazgo == "faltantes"],
                   "decidido_sin_verificar")
})

test_that("un centinela de texto distinto con la misma cuenta se ve con los datos", {
  n <- 100L
  a <- data.frame(id = seq_len(n), t = c("N/A", rep("ok", n - 1L)))
  b <- a
  b$t[1L] <- "NULL"
  pa <- perfilar(a, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  pb <- perfilar(b, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    pa, "propiedad_legitima", motivo = "m", quien = "q",
    tipo_hallazgo = "faltantes_disfrazados", datos = a
  )
  h <- hallazgos(aplicar_decisiones(pb, decisiones, datos = b))
  expect_identical(h$estado_decision[h$tipo_hallazgo == "faltantes_disfrazados"],
                   "revisar")
})

test_that("los hallazgos sin filas se verifican por la estructura y desactivan su accion", {
  n <- 60L
  datos <- data.frame(
    id = seq_len(n), codigo_num = as.character(seq_len(n) * 3L),
    `con espacio` = seq_len(n), check.names = FALSE
  )
  analisis <- analizar(datos, nombre = "t", argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  tipos <- c("tipo_declarado_distinto", "nombres_columnas_problematicos")
  presentes <- intersect(tipos, analisis$perfil$hallazgos$tipo_hallazgo)
  expect_true(length(presentes) >= 1L)
  decisiones <- decidir_hallazgos(
    analisis, "propiedad_legitima", motivo = "es asi", quien = "q",
    tipo_hallazgo = presentes
  )
  expect_true(all(!is.na(decisiones$huella_estructura)))
  nuevo <- aplicar_decisiones(analisis, decisiones)
  h <- hallazgos(nuevo)
  expect_true(all(h$estado_decision[h$tipo_hallazgo %in% presentes] ==
                    "descartado_propiedad_legitima"))
  plan <- nuevo$plan_limpieza
  expect_false(any(plan$aplicar[plan$hallazgo %in% presentes]))
  # Una estructura distinta de la que se decidio vuelve a revisar.
  otra <- as.data.frame(decisiones)
  otra$huella_estructura <- "h2.1.1.1"
  h2 <- hallazgos(aplicar_decisiones(analisis, otra))
  fila <- h2[h2$tipo_hallazgo == "tipo_declarado_distinto", ]
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "estructura")
})

test_that("el plan se deriva de las decisiones: aplicar de nuevo o cambiar la decision no acumula", {
  n <- 100L
  datos <- data.frame(id = seq_len(n), x = seq_len(n) * 1.5)
  datos <- rbind(datos, datos[1:4, ])
  expect_warning(
    analisis <- analizar(datos, nombre = "t", argumentos_perfil = list(
      clave = "id", analizar_dependencias = FALSE
    )),
    "clave declarada no es"
  )
  plan0 <- analisis$plan_limpieza
  expect_true(any(plan0$hallazgo == "filas_duplicadas" & plan0$aplicar))
  por <- function(decision) {
    decidir_hallazgos(analisis, decision, motivo = "m", quien = "q",
                      tipo_hallazgo = "filas_duplicadas", datos = datos,
                      fecha = as.POSIXct("2026-01-02", tz = "UTC"))
  }
  descartado <- aplicar_decisiones(analisis, por("falso_positivo"), datos = datos)
  p1 <- descartado$plan_limpieza
  de <- p1$hallazgo == "filas_duplicadas"
  expect_false(any(p1$aplicar[de]))
  expect_true(all(p1$decision_grupo[de] != "recomendada" | !p1$aplicar[de]))
  expect_false(any(p1$decision_grupo[de & !is.na(p1$grupo)] == "recomendada" &
                     !p1$aplicar[de & !is.na(p1$grupo)]))
  # De descartado a aceptado: el plan vuelve al original, sin la nota.
  aceptado <- aplicar_decisiones(descartado, por("aceptado"), datos = datos)
  expect_identical(aceptado$plan_limpieza, plan0)
  # Aplicar dos veces da lo mismo que una.
  dos <- aplicar_decisiones(descartado, por("falso_positivo"), datos = datos)
  expect_identical(dos$plan_limpieza, p1)
})

test_that("con la proteccion activa los datos conservados enmascarados no se usan para la huella", {
  n <- 40L
  datos <- data.frame(id = seq_len(n), nombre = paste0(" n", seq_len(n), " "),
                      stringsAsFactors = FALSE)
  analisis <- analizar(
    datos, nombre = "t", conservar_datos = TRUE, proteger_datos_personales = TRUE,
    argumentos_perfil = list(columnas_personales = "nombre",
                             analizar_dependencias = FALSE)
  )
  decisiones <- decidir_hallazgos(
    analisis, "falso_positivo", motivo = "m", quien = "q",
    tipo_hallazgo = "espacios_sobrantes"
  )
  expect_true(is.na(decisiones$huella_valores))
  h <- hallazgos(aplicar_decisiones(analisis, decisiones))
  fila <- h[h$tipo_hallazgo == "espacios_sobrantes", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "enmascarados")
})

test_that("descartar un hallazgo hermano no estampa la nota en el otro", {
  n <- 80L
  fechas <- as.Date("2024-01-01") + seq_len(n)
  fechas[1:6] <- as.Date("9999-12-31")
  fechas[7:12] <- as.Date("1900-01-01")
  datos <- data.frame(id = seq_len(n), fecha_fin = fechas)
  analisis <- analizar(datos, nombre = "t", argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  h0 <- analisis$perfil$hallazgos
  hermanos <- which(h0$tipo_hallazgo == "fecha_centinela")
  skip_if(length(hermanos) < 2L, "no salieron dos hallazgos hermanos")
  ids <- identificar_hallazgos(analisis)$id_hallazgo
  decisiones <- decidir_hallazgos(
    analisis, "propiedad_legitima", motivo = "fin abierto valido", quien = "q",
    id_hallazgo = ids[hermanos[[1L]]], datos = datos
  )
  nuevo <- aplicar_decisiones(analisis, decisiones, datos = datos)
  plan <- nuevo$plan_limpieza
  de <- plan[plan$hallazgo == "fecha_centinela", ]
  expect_identical(sum(grepl("fin abierto valido", de$justificacion)), 1L)
})

test_that("una decision de perfilar_dbi() aplicada a una coleccion de la misma tabla no vuelve a revisar", {
  skip_if_not_installed("duckdb")
  datos <- data.frame(id = seq_len(100L), x = c(rep(NA_real_, 20L), seq_len(80L)))
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "uno", datos)
  p <- perfilar_dbi(con, "uno", analizar_dependencias = FALSE)
  col <- perfilar_coleccion(coleccion(con, tablas = "uno"), conservar_perfiles = TRUE)
  decisiones <- decidir_hallazgos(
    p, "propiedad_legitima", motivo = "m", quien = "q", tipo_hallazgo = "faltantes"
  )
  h <- hallazgos(aplicar_decisiones(col, decisiones))
  fila <- h[h$tipo_hallazgo == "faltantes", ]
  expect_false(identical(fila$estado_decision, "revisar"))
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
})

test_that("planificar_limpieza() respeta las decisiones aplicadas al perfil", {
  datos <- data.frame(id = 1:60, nombre = paste0(" n", 1:60, " "))
  perfil <- perfilar(datos, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    perfil, "falso_positivo", motivo = "m", quien = "q",
    tipo_hallazgo = "espacios_sobrantes", datos = datos
  )
  sin <- planificar_limpieza(perfil, datos)
  con <- planificar_limpieza(aplicar_decisiones(perfil, decisiones, datos = datos), datos)
  expect_true(any(sin$hallazgo == "espacios_sobrantes" & sin$aplicar))
  expect_false(any(con$aplicar[con$hallazgo == "espacios_sobrantes"]))
})

test_that("una coleccion sin ninguna tabla perfilada lo dice como es", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "uno", .tabla_triaje())
  col <- perfilar_coleccion(coleccion(con, tablas = "uno"), conservar_perfiles = TRUE)
  decisiones <- decidir_hallazgos(col, "aceptado", motivo = "m", quien = "q",
                                  tabla = "uno")
  col$perfiles <- list()
  col$meta$n_perfiladas <- 0L
  expect_warning(aplicar_decisiones(col, decisiones), "ninguna tabla perfilada")
  expect_error(decidir_hallazgos(col, "aceptado", motivo = "m", quien = "q",
                                 tabla = "uno"), "ninguna tabla perfilada")
})

# ---- El componente requerido decide el estado -------------------------------

test_that("clave al decidir y clave mas datos al aplicar: faltantes queda verificado, outliers no", {
  datos <- .tabla_triaje()
  datos$monto[1:60] <- NA
  p <- .perfil_triaje(datos)
  decisiones <- decidir_hallazgos(
    p, "propiedad_legitima", motivo = "m", quien = "q",
    tipo_hallazgo = c("faltantes", "outliers")
  )
  h <- hallazgos(aplicar_decisiones(p, decisiones, datos = datos))
  expect_identical(h$estado_decision[h$tipo_hallazgo == "faltantes" & h$columna == "monto"],
                   "descartado_propiedad_legitima")
  fila <- h[h$tipo_hallazgo == "outliers", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "valores")
  # El componente extra que no se guardo se declara, y no cambia el estado.
  faltantes <- h[h$tipo_hallazgo == "faltantes" & h$columna == "monto", ]
  expect_true(is.na(faltantes$verificacion) ||
                grepl("no requerido", faltantes$verificacion))
})

test_that("C3: los mismos extremos en cantidad con otros valores vuelven a revisar con datos en los dos lados", {
  datos <- .tabla_triaje()
  decisiones <- decidir_hallazgos(
    .perfil_triaje(datos), "aceptado", motivo = "m", quien = "q",
    tipo_hallazgo = "outliers", datos = datos
  )
  otros <- datos
  otros$monto[otros$monto == 1e6] <- 2e6
  otros$monto[otros$monto == -1e6] <- -2e6
  p <- .perfil_triaje(otros)
  cuenta <- function(x) hallazgos(x)$n_afectados[hallazgos(x)$tipo_hallazgo == "outliers"]
  expect_identical(cuenta(p), cuenta(.perfil_triaje(datos)))
  h <- hallazgos(aplicar_decisiones(p, decisiones, datos = otros))
  fila <- h[h$tipo_hallazgo == "outliers", ]
  expect_identical(fila$estado_decision, "revisar")
  expect_match(fila$cambio_desde_decision, "valores")
})

test_that("faltantes_disfrazados requiere valores: sin datos queda sin verificar, tambien en texto", {
  n <- 100L
  a <- data.frame(id = seq_len(n), t = c("N/A", rep("ok", n - 1L)))
  pa <- perfilar(a, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  decisiones <- decidir_hallazgos(
    pa, "propiedad_legitima", motivo = "m", quien = "q",
    tipo_hallazgo = "faltantes_disfrazados"
  )
  b <- a
  b$t[1L] <- "NULL"
  h <- hallazgos(aplicar_decisiones(
    perfilar(b, nombre = "t", clave = "id", analizar_dependencias = FALSE),
    decisiones
  ))
  fila <- h[h$tipo_hallazgo == "faltantes_disfrazados", ]
  expect_identical(fila$estado_decision, "decidido_sin_verificar")
  expect_match(fila$verificacion, "valores")
})

test_that("todo tipo de hallazgo que publica el paquete tiene sus componentes requeridos", {
  skip_on_covr()
  raiz <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if(!file.exists(file.path(raiz, "R")), "las fuentes del paquete no estan a la vista")
  fuentes <- vapply(
    list.files(file.path(raiz, "R"), pattern = "[.]R$", full.names = TRUE),
    function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    character(1L)
  )
  todo <- paste(fuentes, collapse = "\n")
  tipos <- regmatches(
    todo, gregexpr('\\.nuevo_hallazgo\\(\\s*[^,]+?,\\s*"[a-z_]+"', todo, perl = TRUE)
  )[[1L]]
  # Los que se construyen con el tipo en una variable no los ve la expresion
  # anterior: se buscan por su nombre.
  relaciones <- unique(regmatches(
    todo, gregexpr('"relacion_[a-z_]+_columnas"', todo)
  )[[1L]])
  tipos <- unique(c(sub('^.*"([a-z_]+)"$', "\\1", tipos), gsub('"', "", relaciones),
                    lupa:::.TIPOS_HALLAZGO_DOMINIO, lupa:::.TIPOS_HALLAZGO_FORMATO_FECHA))
  sin_fila <- setdiff(tipos, names(lupa:::.COMPONENTES_REQUERIDOS))
  expect_identical(sin_fila, character())
  # Y la tabla no nombra tipos que ya no existen.
  expect_identical(setdiff(names(lupa:::.COMPONENTES_REQUERIDOS), tipos), character())
})

# ---- Un descarte sin verificar no modifica datos ----------------------------

test_that("un descarte sin verificar deja las acciones sin aplicar; un revisar no aplica la decision", {
  n <- 300L
  datos <- data.frame(id = seq_len(n), a = rep(c(" x ", "y"), length.out = n),
                      stringsAsFactors = FALSE)
  # Sin `conservar_datos`: la decision no se puede verificar.
  analisis <- analizar(datos, nombre = "t", argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  plan0 <- analisis$plan_limpieza
  expect_true(any(plan0$hallazgo == "espacios_sobrantes" & plan0$aplicar))
  decisiones <- decidir_hallazgos(
    analisis, "falso_positivo", motivo = "los espacios son parte del codigo",
    quien = "q", tipo_hallazgo = "espacios_sobrantes"
  )
  nuevo <- aplicar_decisiones(analisis, decisiones)
  expect_identical(hallazgos(nuevo)$estado_decision[hallazgos(nuevo)$tipo_hallazgo == "espacios_sobrantes"],
                   "decidido_sin_verificar")
  plan <- nuevo$plan_limpieza
  de <- plan$hallazgo == "espacios_sobrantes"
  expect_false(any(plan$aplicar[de]))
  expect_true(all(grepl("revisala antes de limpiar", plan$justificacion[de])))
  # `aplicar()` por omision no toca lo que el usuario dijo que es legitimo.
  limpio <- aplicar(plan, datos)
  expect_identical(limpio$datos$a, datos$a)
  # Un revisar: el plan queda como el original, con una nota.
  otros <- datos
  otros$a[1:5] <- "z"
  cambiado <- analizar(otros, nombre = "t", conservar_datos = TRUE, argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  d2 <- decidir_hallazgos(
    analisis, "falso_positivo", motivo = "m", quien = "q",
    tipo_hallazgo = "espacios_sobrantes", datos = datos
  )
  rev <- aplicar_decisiones(cambiado, d2)
  expect_identical(hallazgos(rev)$estado_decision[hallazgos(rev)$tipo_hallazgo == "espacios_sobrantes"],
                   "revisar")
  pr <- rev$plan_limpieza
  dr <- pr$hallazgo == "espacios_sobrantes"
  expect_identical(pr$aplicar[dr], cambiado$plan_limpieza$aplicar[dr])
  expect_true(all(grepl("no aplicada porque cambi", pr$justificacion[dr])))
})

test_that("las ediciones a mano del plan sobreviven a reaplicar, tambien tras guardar y leer en otro proceso", {
  n <- 100L
  datos <- data.frame(id = seq_len(n), x = seq_len(n) * 1.5,
                      obs = c(rep("N/A", 14L), rep("ok", n - 14L)),
                      a = rep(c(" x ", "y"), length.out = n), stringsAsFactors = FALSE)
  analisis <- analizar(datos, nombre = "t", conservar_datos = TRUE, argumentos_perfil = list(
    clave = "id", analizar_dependencias = FALSE
  ))
  fecha <- as.POSIXct("2026-01-02", tz = "UTC")
  decidir <- function(tipo, decision) decidir_hallazgos(
    analisis, decision, motivo = "m", quien = "q", tipo_hallazgo = tipo,
    fecha = fecha
  )
  # Una edicion del usuario sobre una fila que las decisiones no tocan.
  editado <- analisis
  fila_obs <- which(editado$plan_limpieza$columna == "obs" &
                      editado$plan_limpieza$aplicar)[[1L]]
  editado$plan_limpieza$aplicar[[fila_obs]] <- FALSE
  uno <- aplicar_decisiones(editado, decidir("espacios_sobrantes", "falso_positivo"))
  expect_false(uno$plan_limpieza$aplicar[[fila_obs]])
  # Cambiar la decision y reaplicar: lo editado sigue, lo descartado cambia.
  dos <- aplicar_decisiones(uno, decidir("espacios_sobrantes", "aceptado"))
  expect_false(dos$plan_limpieza$aplicar[[fila_obs]])
  esp <- dos$plan_limpieza$hallazgo == "espacios_sobrantes"
  expect_identical(dos$plan_limpieza$aplicar[esp], editado$plan_limpieza$aplicar[esp])
  expect_identical(dos$plan_limpieza$justificacion[esp],
                   editado$plan_limpieza$justificacion[esp])
  # Reaplicar lo mismo da lo mismo.
  tres <- aplicar_decisiones(uno, decidir("espacios_sobrantes", "falso_positivo"))
  expect_identical(tres$plan_limpieza, uno$plan_limpieza)
  # Guardar, leer en otro proceso y reaplicar.
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(uno, archivo, version = 2L)
  guion <- tempfile(fileext = ".R")
  on.exit(unlink(guion), add = TRUE, after = FALSE)
  salida <- tempfile(fileext = ".rds")
  on.exit(unlink(salida), add = TRUE)
  decisiones <- decidir("espacios_sobrantes", "aceptado")
  archivo_dec <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo_dec), add = TRUE)
  saveRDS(decisiones, archivo_dec, version = 2L)
  writeLines(c(
    sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(normalizePath(
      testthat::test_path("..", ".."), mustWork = TRUE))),
    sprintf("x <- readRDS(%s)", deparse(archivo)),
    sprintf("d <- readRDS(%s)", deparse(archivo_dec)),
    sprintf("saveRDS(aplicar_decisiones(x, d), %s, version = 2L)", deparse(salida))
  ), guion)
  skip_if(!nzchar(Sys.which("Rscript")), "no hay Rscript")
  estado <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(
    paste0("source(", deparse(guion), ")")
  )), stdout = FALSE, stderr = FALSE)
  skip_if(!identical(as.integer(estado), 0L), "no se pudo cargar el paquete en otro proceso")
  otro <- readRDS(salida)
  expect_identical(otro$plan_limpieza$aplicar, dos$plan_limpieza$aplicar)
  expect_false(otro$plan_limpieza$aplicar[[fila_obs]])
})

# ---- perfilar_por() con datos, y tipos con contenido propio -----------------

test_that("perfilar_por() con datos verifica por grupo", {
  set.seed(4)
  n <- 400L
  datos <- data.frame(
    id = seq_len(n), g = rep(c("a", "b"), each = n / 2L),
    monto = c(stats::rnorm(n - 2L), 1e6, -1e6), stringsAsFactors = FALSE
  )
  por <- perfilar_por(datos, por = "g", clave = "id")
  decisiones <- decidir_hallazgos(
    por, "aceptado", motivo = "m", quien = "q", columna = "monto",
    nombre_tabla = "t", datos = datos
  )
  expect_true(all(!is.na(decisiones$huella_valores)))
  con <- aplicar_decisiones(por, decisiones, datos = datos, nombre_tabla = "t")
  expect_true(all(con$estado_decision[con$columna == "monto" &
                                        con$tipo_hallazgo == "outliers"] == "aceptado"))
  otros <- datos
  otros$monto[otros$monto == 1e6] <- 2e6
  por2 <- perfilar_por(otros, por = "g", clave = "id")
  con2 <- aplicar_decisiones(por2, decisiones, datos = otros, nombre_tabla = "t")
  expect_true(any(con2$estado_decision == "revisar"))
})

test_that("constante, casi_clave y columnas duplicadas se verifican con lo que el perfil publica", {
  n <- 60L
  datos <- data.frame(id = seq_len(n), k = rep("k", n), a = seq_len(n) * 2, b = seq_len(n) * 2)
  p <- perfilar(datos, nombre = "t", clave = "id", analizar_dependencias = FALSE)
  tipos <- intersect(c("constante", "columnas_duplicadas"), hallazgos(p)$tipo_hallazgo)
  expect_true(length(tipos) == 2L)
  decisiones <- decidir_hallazgos(p, "propiedad_legitima", motivo = "m", quien = "q",
                                  tipo_hallazgo = tipos)
  expect_true(all(!is.na(decisiones$detalle) | !is.na(decisiones$huella_estructura)))
  h <- hallazgos(aplicar_decisiones(p, decisiones))
  expect_true(all(h$estado_decision[h$tipo_hallazgo %in% tipos] ==
                    "descartado_propiedad_legitima"))
  # El valor constante cambia con la misma cuenta.
  otros <- datos
  otros$k <- rep("z", n)
  h2 <- hallazgos(aplicar_decisiones(
    perfilar(otros, nombre = "t", clave = "id", analizar_dependencias = FALSE), decisiones
  ))
  expect_identical(h2$estado_decision[h2$tipo_hallazgo == "constante"], "revisar")
  # La pareja de una columna duplicada cambia: a = b pasa a a = d.
  par <- data.frame(id = seq_len(n), a = seq_len(n) * 2, d = seq_len(n) * 2)
  h3 <- hallazgos(aplicar_decisiones(
    perfilar(par, nombre = "t", clave = "id", analizar_dependencias = FALSE), decisiones
  ))
  expect_false(any(h3$estado_decision[h3$tipo_hallazgo == "columnas_duplicadas"] ==
                     "descartado_propiedad_legitima"))
})

test_that("ningun tipo cae en el comodin sin decirlo", {
  set.seed(8)
  n <- 120L
  a <- stats::rnorm(n)
  datos <- data.frame(id = seq_len(n), a = a, b = a + 1, c = 2 * a + 3,
                      f = rep(c("x", "x", "y"), length.out = n))
  p <- perfilar(datos, nombre = "t", clave = "id")
  tipos <- unique(hallazgos(p)$tipo_hallazgo)
  expect_true(all(tipos %in% names(lupa:::.COMPONENTES_REQUERIDOS)))
  # Y los tipos que se construyen con el tipo en una variable.
  expect_true(all(c("relacion_orden_columnas", "relacion_aritmetica_columnas") %in%
                    names(lupa:::.COMPONENTES_REQUERIDOS)))
  d <- decidir_hallazgos(p, "aceptado", motivo = "m", quien = "q",
                         tipo_hallazgo = tipos, datos = datos)
  h <- hallazgos(aplicar_decisiones(p, d, datos = datos))
  expect_false(any(grepl("sin componente requerido", h$verificacion), na.rm = TRUE))
})
