# Los patrones de ausencia que no son una regla casi determinista: columnas que
# faltan juntas, una columna que falta mas en un grupo que en otro y filas sin
# ningun valor. Cada prueba mide el hecho y la traza; los controles exigen
# silencio cuando el patron es trivial o no existe.
testthat::skip_on_cran()

.opciones_patrones <- function(...) {
  list(
    fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE,
    normalizar = FALSE,
    ...
  )
}

.perfilar_patrones <- function(datos, ...) {
  do.call(perfilar, c(list(datos), .opciones_patrones(...)))
}

.hallazgos_tipo <- function(perfil, tipo) {
  h <- perfil$hallazgos
  h[as.character(h$tipo_hallazgo) == tipo, , drop = FALSE]
}

.sin_patrones_nuevos <- function(perfil) {
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  !any(tipos %in% c(
    "faltantes_conjuntos", "ausencia_dependiente", "filas_vacias"
  ))
}

.tabla_conjunta <- function(n = 600L, comunes = 150L,
                            extra_1 = 6L, extra_2 = 6L) {
  elegidas <- seq_len(comunes)
  resto <- setdiff(seq_len(n), elegidas)
  extra_1 <- utils::head(resto, extra_1)
  extra_2 <- utils::head(resto[-seq_along(extra_1)], extra_2)
  x1 <- rep("ok", n)
  x2 <- rep("ok", n)
  x1[c(elegidas, extra_1)] <- NA_character_
  x2[c(elegidas, extra_2)] <- NA_character_
  data.frame(
    id = seq_len(n), x1 = x1, x2 = x2, stringsAsFactors = FALSE
  )
}

test_that("las columnas que faltan juntas se publican una vez y con sus filas", {
  datos <- .tabla_conjunta()
  expect_equal(sum(is.na(datos$x1)), 156L)
  expect_equal(sum(is.na(datos$x2)), 156L)
  perfil <- .perfilar_patrones(datos)
  h <- .hallazgos_tipo(perfil, "faltantes_conjuntos")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$columna), "x1,x2")
  expect_identical(as.character(h$severidad), "ok")
  expect_match(h$descripcion, "bloque que no se carg", fixed = TRUE)
  expect_match(h$descripcion, "s\u00f3lo se completan", fixed = TRUE)
  expect_equal(h$n_afectados, 150)
  traza <- h$trazabilidad[[1L]]
  expect_identical(traza$estado, "disponible")
  expect_identical(traza$indices_fila, seq_len(150L))
  expect_equal(traza$total, 150)
  expect_setequal(traza$columnas, c("x1", "x2"))
  expect_match(h$evidencia, "156 ausentes", fixed = TRUE)
  # El `faltantes` de cada columna nombra el patron conjunto.
  faltantes <- .hallazgos_tipo(perfil, "faltantes")
  expect_true(all(grepl(
    "faltantes_conjuntos", faltantes$evidencia, fixed = TRUE
  )))
})

test_that("un bloque comun se lee aunque cada columna tenga ausencias propias", {
  comunes <- seq_len(20L)
  mascaras <- list(
    c(comunes, 21:100), c(comunes, 101:180), c(comunes, 181:260)
  )
  datos <- data.frame(id = seq_len(1000L))
  for (i in seq_len(3L)) {
    x <- rep("ok", 1000L)
    x[mascaras[[i]]] <- NA_character_
    datos[[paste0("x", i)]] <- x
  }
  perfil <- .perfilar_patrones(datos)
  h <- .hallazgos_tipo(perfil, "faltantes_conjuntos")
  expect_equal(nrow(h), 1L)
  expect_setequal(
    strsplit(as.character(h$columna), ",", fixed = TRUE)[[1L]],
    c("x1", "x2", "x3")
  )
  expect_equal(h$n_afectados, 20)
  expect_identical(h$trazabilidad[[1L]]$indices_fila, comunes)
  expect_identical(as.character(h$severidad), "ok")
})

test_that("los controles de faltantes conjuntos no acusan coincidencias triviales", {
  datos <- .tabla_conjunta()
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(
    datos, columnas_opcionales = c("x1", "x2")
  )))
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(
    datos, aplicabilidad = list(x1 = ~ id > 0)
  )))

  # Casi siempre vacias, pero independientes: la coincidencia es la que produce
  # el azar con dos columnas de 95 % de ausencias.
  set.seed(101)
  n <- 600L
  casi_1 <- stats::runif(n) < 0.95
  casi_2 <- stats::runif(n) < 0.95
  casi <- data.frame(
    id = seq_len(n),
    a = ifelse(casi_1, NA_character_, "a"),
    b = ifelse(casi_2, NA_character_, "b"),
    stringsAsFactors = FALSE
  )
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(casi)))

  # Faltantes al azar, no conjuntos.
  set.seed(102)
  azar <- data.frame(
    id = seq_len(n),
    x1 = ifelse(stats::runif(n) < 0.25, NA_character_, "a"),
    x2 = ifelse(stats::runif(n) < 0.25, NA_character_, "b"),
    stringsAsFactors = FALSE
  )
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(azar)))
})

test_that("la ausencia que depende de un grupo publica el efecto y la traza", {
  set.seed(103)
  grupo <- rep(c("A", "B"), each = 300L)
  ingreso <- rep(100, 600L)
  ingreso[grupo == "A"] <- ifelse(
    stats::runif(300L) < 0.05, NA, ingreso[grupo == "A"]
  )
  ingreso[grupo == "B"] <- ifelse(
    stats::runif(300L) < 0.50, NA, ingreso[grupo == "B"]
  )
  datos <- data.frame(grupo = grupo, ingreso = ingreso)
  perfil <- .perfilar_patrones(datos)
  h <- .hallazgos_tipo(perfil, "ausencia_dependiente")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$columna), "ingreso")
  expect_identical(as.character(h$severidad), "ok")
  expect_match(h$descripcion, "bloque que no se carg", fixed = TRUE)
  expect_match(h$descripcion, "s\u00f3lo se completa", fixed = TRUE)
  expect_equal(h$n_afectados, sum(is.na(datos$ingreso)))
  traza <- h$trazabilidad[[1L]]
  expect_identical(
    traza$indices_fila, which(is.na(datos$ingreso))
  )
  expect_match(h$evidencia, "cociente de riesgos", fixed = TRUE)
  expect_false(grepl("p-valor", h$evidencia, fixed = TRUE))
})

test_that("la ausencia en el tope de una numerica tambien se mide", {
  set.seed(104)
  monto <- c(rep(1000, 100L), sample(1:500, 500L, replace = TRUE))
  tope <- monto == 1000
  meta <- rep(1, 600L)
  meta[tope] <- ifelse(stats::runif(sum(tope)) < 0.60, NA, 1)
  meta[!tope] <- ifelse(stats::runif(sum(!tope)) < 0.05, NA, 1)
  perfil <- .perfilar_patrones(data.frame(monto = monto, meta = meta))
  h <- .hallazgos_tipo(perfil, "ausencia_dependiente")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$columna), "meta")
  expect_match(h$evidencia, "ximo", fixed = TRUE)
})

test_that("los controles de dependencia dejan callar grupos chicos y azar", {
  # Un grupo de diez filas no alcanza para comparar proporciones.
  grupo <- c(rep("A", 590L), rep("B", 10L))
  objetivo <- rep(1, 600L)
  objetivo[grupo == "B"] <- NA
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(
    data.frame(grupo = grupo, objetivo = objetivo)
  )))

  # Una categorica de alta cardinalidad no es un grupo.
  set.seed(105)
  alta <- data.frame(
    id = sprintf("ID%04d", seq_len(600L)),
    objetivo = ifelse(stats::runif(600L) < 0.30, NA, 1),
    stringsAsFactors = FALSE
  )
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(alta)))

  # Faltantes al azar: no hay dependencia.
  set.seed(106)
  azar <- data.frame(
    grupo = rep(c("A", "B"), each = 300L),
    objetivo = ifelse(stats::runif(600L) < 0.30, NA, 1)
  )
  expect_true(.sin_patrones_nuevos(.perfilar_patrones(azar)))

  # La ausencia estructural ya tiene su propio hallazgo y no se duplica.
  estructural <- data.frame(
    grupo = rep(c("A", "B"), each = 300L),
    valor = ifelse(rep(c("A", "B"), each = 300L) == "A", "dato", NA),
    stringsAsFactors = FALSE
  )
  perfil <- .perfilar_patrones(estructural)
  expect_equal(nrow(.hallazgos_tipo(perfil, "ausencia_dependiente")), 0L)
  expect_true(any(
    as.character(perfil$hallazgos$tipo_hallazgo) ==
      "posible_ausencia_estructural"
  ))
})

test_that("un identificador casi clave no agrupa la ausencia", {
  id <- c(rep(1000, 20L), seq_len(380L))
  objetivo <- rep(1, 400L)
  objetivo[seq_len(15L)] <- NA
  objetivo[21:36] <- NA
  perfil <- .perfilar_patrones(data.frame(id = id, objetivo = objetivo))
  expect_equal(nrow(.hallazgos_tipo(perfil, "ausencia_dependiente")), 0L)
})

test_that("un identificador casi clave se reconoce por debajo del 95 %", {
  # 377 valores distintos en 400 filas: el descarte no empieza en el 95 %. La
  # deteccion de casi-clave del perfil lo reconoce por las colisiones, y el
  # tope del identificador no se usa como grupo.
  id <- c(rep(999999, 24L), seq_len(376L))
  objetivo <- rep(1, 400L)
  objetivo[seq_len(20L)] <- NA
  objetivo[25:44] <- NA
  perfil <- .perfilar_patrones(data.frame(id = id, objetivo = objetivo))
  expect_equal(nrow(.hallazgos_tipo(perfil, "ausencia_dependiente")), 0L)

  # Una fecha con el mismo tope repetido no es una clave: sigue agrupando.
  fecha <- as.Date("2020-01-01") + c(rep(1000L, 24L), seq_len(376L))
  perfil <- .perfilar_patrones(data.frame(fecha = fecha, objetivo = objetivo))
  expect_equal(nrow(.hallazgos_tipo(perfil, "ausencia_dependiente")), 1L)
})

test_that("un bloque conjunto por debajo del 5 % tambien se publica", {
  n <- 1000L
  comunes <- seq_len(40L)
  datos <- data.frame(
    id = seq_len(n),
    x1 = rep("ok", n), x2 = rep("ok", n), x3 = rep("ok", n),
    stringsAsFactors = FALSE
  )
  datos[comunes, c("x1", "x2", "x3")] <- NA_character_
  perfil <- .perfilar_patrones(datos)
  h <- .hallazgos_tipo(perfil, "faltantes_conjuntos")
  expect_equal(nrow(h), 1L)
  expect_setequal(
    strsplit(as.character(h$columna), ",", fixed = TRUE)[[1L]],
    c("x1", "x2", "x3")
  )
  expect_equal(h$n_afectados, 40)
  expect_identical(h$trazabilidad[[1L]]$indices_fila, comunes)
})

test_that("los faltantes al azar en muchas columnas no arman grupos", {
  n <- 1000L
  for (tasa in c(0.02, 0.05, 0.30)) {
    set.seed(20261120L + round(tasa * 100))
    columnas <- lapply(seq_len(50L), function(i) {
      ifelse(stats::runif(n) < tasa, NA_real_, 1)
    })
    names(columnas) <- sprintf("v%02d", seq_len(50L))
    perfil <- .perfilar_patrones(as.data.frame(columnas))
    expect_equal(nrow(.hallazgos_tipo(perfil, "faltantes_conjuntos")), 0L)
  }
})

test_that("una fila vacia se cuenta aunque sea una sola", {
  datos <- data.frame(
    a = rep(1, 60L), b = rep("x", 60L), stringsAsFactors = FALSE
  )
  datos[37L, ] <- NA
  perfil <- .perfilar_patrones(datos)
  h <- .hallazgos_tipo(perfil, "filas_vacias")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "error")
  expect_equal(h$n_afectados, 1)
  traza <- h$trazabilidad[[1L]]
  expect_identical(traza$indices_fila, 37L)
  expect_equal(traza$total, 1)
})

test_that("las filas vacias no se convierten en una orden de borrar", {
  set.seed(107)
  datos <- data.frame(
    a = stats::runif(60L), b = sample(letters, 60L, replace = TRUE),
    stringsAsFactors = FALSE
  )
  datos[10L, ] <- NA
  perfil <- .perfilar_patrones(datos)
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(
    as.character(plan$estrategia) == "eliminar_filas_ausentes"
  ))
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_true(
    inherits(sin_accion, "data.frame") &&
      any(as.character(sin_accion$hallazgo) == "filas_vacias")
  )
})

test_that("los patrones nuevos se apagan con ausencia_estructural", {
  datos <- .tabla_conjunta()
  perfil <- .perfilar_patrones(datos, ausencia_estructural = FALSE)
  expect_true(.sin_patrones_nuevos(perfil))
  # Los mismos datos con el interruptor encendido si los publican.
  expect_false(.sin_patrones_nuevos(.perfilar_patrones(datos)))
})

test_that("perfilar_por conserva el patron y traduce sus filas", {
  set.seed(108)
  datos <- data.frame(
    grupo = rep(c("A", "B"), each = 80L),
    id = seq_len(160L),
    x1 = "ok", x2 = "ok", stringsAsFactors = FALSE
  )
  comunes_a <- which(datos$grupo == "A")[seq_len(20L)]
  comunes_b <- which(datos$grupo == "B")[seq_len(20L)]
  datos$x1[c(comunes_a, comunes_b)] <- NA
  datos$x2[c(comunes_a, comunes_b)] <- NA
  salida <- do.call(perfilar_por, c(
    list(datos = datos, por = "grupo", min_filas = 1L),
    .opciones_patrones()
  ))
  h <- salida[as.character(salida$tipo_hallazgo) ==
                "faltantes_conjuntos", , drop = FALSE]
  expect_equal(nrow(h), 2L)
  for (i in seq_len(nrow(h))) {
    indices <- h$trazabilidad[[i]]$indices_fila
    expect_true(all(datos$grupo[indices] == h$grupo[[i]]))
    expect_equal(length(indices), 20L)
  }
})

test_that("perfilar_por no inventa filas vacias ni pierde el grupo conjunto", {
  datos <- data.frame(
    grupo = rep(c("A", "B"), each = 120L),
    x1 = rep("ok", 240L), x2 = rep("ok", 240L),
    stringsAsFactors = FALSE
  )
  filas <- c(seq_len(20L), 120L + seq_len(20L))
  datos$x1[filas] <- NA_character_
  datos$x2[filas] <- NA_character_
  salida <- do.call(perfilar_por, c(
    list(datos = datos, por = "grupo", min_filas = 30L),
    .opciones_patrones()
  ))
  conjuntos <- salida[
    as.character(salida$tipo_hallazgo) == "faltantes_conjuntos", ,
    drop = FALSE
  ]
  expect_equal(nrow(conjuntos), 2L)
  expect_equal(sum(conjuntos$n_afectados), 40)
  expect_equal(nrow(salida[
    as.character(salida$tipo_hallazgo) == "filas_vacias", , drop = FALSE
  ]), 0L)

  # La fila que si esta vacia en la tabla entera se sigue publicando.
  datos[1L, ] <- NA
  datos[50L, "grupo"] <- NA
  salida <- do.call(perfilar_por, c(
    list(datos = datos, por = "grupo", min_filas = 1L),
    .opciones_patrones()
  ))
  vacias <- salida[
    as.character(salida$tipo_hallazgo) == "filas_vacias", , drop = FALSE
  ]
  expect_equal(nrow(vacias), 1L)
  expect_identical(vacias$trazabilidad[[1L]]$indices_fila, 1L)
})

test_that("analizar y la deriva ven los patrones nuevos", {
  datos <- .tabla_conjunta(n = 120L, comunes = 30L, extra_1 = 3L, extra_2 = 3L)
  datos$vacia <- "x"
  datos$vacia[1L] <- NA
  datos[1L, c("id", "x1", "x2", "vacia")] <- NA
  analisis <- do.call(analizar, c(list(datos), .opciones_patrones()))
  tipos <- as.character(analisis$perfil$hallazgos$tipo_hallazgo)
  expect_true("faltantes_conjuntos" %in% tipos)
  expect_true("filas_vacias" %in% tipos)

  encendido <- .perfilar_patrones(datos)
  apagado <- .perfilar_patrones(datos, ausencia_estructural = FALSE)
  deriva <- comparar_perfiles(encendido, apagado)
  fila <- deriva[as.character(deriva$aspecto) == "hallazgo" &
                   as.character(deriva$cambio) == "no_evaluado", , drop = FALSE]
  expect_true(nrow(fila) >= 1L)
  expect_true(any(as.character(fila$valor_anterior) %in%
                    c("faltantes_conjuntos", "filas_vacias")))
})

test_that("perfilar_dbi publica los patrones en la muestra de SQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- .tabla_conjunta(n = 120L, comunes = 30L, extra_1 = 3L, extra_2 = 3L)
  datos$vacia <- "x"
  datos[1L, ] <- NA
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", datos)
  resultado <- do.call(perfilar_dbi, c(
    list(conexion = con, tabla = "d", muestra = Inf),
    .opciones_patrones()
  ))
  tipos <- as.character(resultado$perfil_muestra$hallazgos$tipo_hallazgo)
  expect_true("faltantes_conjuntos" %in% tipos)
  expect_true("filas_vacias" %in% tipos)

  solo_agregados <- do.call(perfilar_dbi, c(
    list(
      conexion = con, tabla = "d", muestra = Inf,
      bloque_muestra = "solo_agregados"
    ),
    .opciones_patrones()
  ))
  expect_null(solo_agregados$perfil_muestra)
  cobertura <- solo_agregados$resumen_tabla$cobertura
  expect_true(any(as.character(cobertura$bloque) == "perfil_muestra"))
  expect_true(any(as.character(cobertura$estado) == "no_solicitado"))
})

test_that("perfilar_dbi publica los patrones en la muestra de DuckDB", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  datos <- .tabla_conjunta(n = 120L, comunes = 30L, extra_1 = 3L, extra_2 = 3L)
  datos$vacia <- "x"
  datos[1L, ] <- NA
  con <- DBI::dbConnect(.duckdb_prueba())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "d", datos)
  resultado <- do.call(perfilar_dbi, c(
    list(conexion = con, tabla = "d", muestra = Inf),
    .opciones_patrones()
  ))
  tipos <- as.character(resultado$perfil_muestra$hallazgos$tipo_hallazgo)
  expect_true("faltantes_conjuntos" %in% tipos)
  expect_true("filas_vacias" %in% tipos)
})

test_that("los topes de columnas se declaran en la cobertura", {
  set.seed(109)
  n <- 100L
  comunes <- seq_len(20L)
  columnas <- lapply(seq_len(45L), function(i) {
    x <- rep(sprintf("v%02d", i), n)
    x[comunes] <- NA_character_
    x
  })
  names(columnas) <- sprintf("c%02d", seq_along(columnas))
  datos <- as.data.frame(columnas, stringsAsFactors = FALSE)
  datos$id <- seq_len(n)
  perfil <- .perfilar_patrones(datos)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[as.character(cobertura$diagnostico) ==
                      "faltantes_conjuntos" &
                      grepl("se detiene en 40", cobertura$motivo, fixed = TRUE), ,
                    drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_true(grepl("c41", fila$columna, fixed = TRUE))
  h <- .hallazgos_tipo(perfil, "faltantes_conjuntos")
  expect_equal(nrow(h), 1L)
  expect_equal(length(strsplit(as.character(h$columna), ",", fixed = TRUE)[[1L]]), 40L)
  expect_match(h$evidencia, "5 columnas m", fixed = TRUE)
})

test_that("sin columnas candidatas la cobertura no inventa filas", {
  datos <- data.frame(id = seq_len(40L), bloque = I(matrix(1, 40L, 2L)))
  perfil <- .perfilar_patrones(datos)
  nuevos <- c(
    "faltantes_conjuntos", "ausencia_dependiente", "filas_vacias"
  )
  cobertura <- perfil$cobertura_diagnosticos
  expect_false(any(as.character(cobertura$diagnostico) %in% nuevos))
})

test_that("una fila de centinelas no es una fila vacia", {
  datos <- data.frame(x = c(999, 1:9))
  perfil <- .perfilar_patrones(datos)
  expect_equal(
    nrow(.hallazgos_tipo(perfil, "filas_vacias")), 0L
  )
  expect_equal(perfil$columnas$n_faltantes_disfrazados, 1L)
})
