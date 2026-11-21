# La ausencia explicada por dos determinantes se publica solo cuando la tabla
# de celdas tiene soporte suficiente y la explicacion simple no alcanza.
testthat::skip_on_cran()

.opciones_interaccion <- function(...) {
  list(
    fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE,
    proteger_datos_personales = FALSE,
    normalizar = FALSE,
    ...
  )
}

.perfilar_interaccion <- function(datos, ...) {
  do.call(perfilar, c(list(datos), .opciones_interaccion(...)))
}

.hallazgos_interaccion <- function(perfil, tipo) {
  perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == tipo, , drop = FALSE
  ]
}

.datos_interaccion_diagonal <- function(n_por_celda = 250L) {
  datos <- expand.grid(a = c("A", "B"), b = c("A", "B"))
  datos <- datos[rep(seq_len(4L), each = n_por_celda), , drop = FALSE]
  datos$valor <- 1
  datos$valor[(datos$a == "A" & datos$b == "A") |
               (datos$a == "B" & datos$b == "B")] <- NA_real_
  datos
}

test_that("la ausencia que depende de dos columnas publica la interaccion", {
  datos <- .datos_interaccion_diagonal()
  perfil <- .perfilar_interaccion(datos)
  h <- .hallazgos_interaccion(perfil, "ausencia_dependiente")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "ok")
  expect_identical(as.character(h$columna), "valor")
  expect_equal(h$n_afectados, 500)
  expect_match(h$descripcion, "combinaci\u00f3n de dos columnas", fixed = TRUE)
  expect_match(h$evidencia, "100.0 puntos porcentuales", fixed = TRUE)
  expect_match(h$evidencia, "cociente de riesgos", fixed = TRUE)
  traza <- h$trazabilidad[[1L]]
  expect_setequal(traza$columnas, c("valor", "a", "b"))
  expect_identical(traza$columna_objetivo, "valor")
  expect_identical(
    traza$indices_fila,
    which(is.na(datos$valor))
  )
  expect_equal(traza$total, 500)
  faltantes <- .hallazgos_interaccion(perfil, "faltantes")
  expect_equal(nrow(faltantes), 1L)
  expect_match(faltantes$evidencia, "ausencia_dependiente", fixed = TRUE)
  expect_match(faltantes$evidencia, "valores de otras columnas", fixed = TRUE)
  expect_false(grepl("valor de otra columna", faltantes$evidencia,
                     fixed = TRUE))
})

test_that("las puertas de perfilado conservan la interaccion", {
  base <- .datos_interaccion_diagonal(125L)
  datos <- rbind(base, base)
  datos$grupo <- rep(c("G1", "G2"), each = nrow(base))
  datos <- datos[, c("grupo", "a", "b", "valor")]
  directo <- .perfilar_interaccion(base)
  apagado <- .perfilar_interaccion(base, ausencia_estructural = FALSE)
  por <- do.call(perfilar_por, c(
    list(datos = datos, por = "grupo", min_filas = 1L),
    .opciones_interaccion()
  ))
  expect_equal(nrow(.hallazgos_interaccion(directo, "ausencia_dependiente")), 1L)
  expect_equal(sum(as.character(por$tipo_hallazgo) == "ausencia_dependiente"), 2L)

  plan <- planificar_limpieza(directo, base)
  expect_false(any(as.character(plan$hallazgo) == "ausencia_dependiente"))
  deriva <- comparar_perfiles(directo, apagado)
  expect_true(any(as.character(deriva$valor_anterior) == "ausencia_dependiente" &
                    as.character(deriva$cambio) == "no_evaluado"))

  skip_if_not_installed("DBI")
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con_sqlite), add = TRUE)
    DBI::dbWriteTable(con_sqlite, "d", base)
    sqlite <- do.call(perfilar_dbi, c(
      list(conexion = con_sqlite, tabla = "d", muestra = Inf),
      .opciones_interaccion()
    ))
    expect_equal(
      nrow(.hallazgos_interaccion(sqlite$perfil_muestra,
                                  "ausencia_dependiente")), 1L
    )
  }
  if (requireNamespace("duckdb", quietly = TRUE)) {
    con_duckdb <- DBI::dbConnect(.duckdb_prueba())
    on.exit(DBI::dbDisconnect(con_duckdb, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con_duckdb, "d", base)
    duckdb <- do.call(perfilar_dbi, c(
      list(conexion = con_duckdb, tabla = "d", muestra = Inf),
      .opciones_interaccion()
    ))
    expect_equal(
      nrow(.hallazgos_interaccion(duckdb$perfil_muestra,
                                  "ausencia_dependiente")), 1L
    )
  }
})

test_that("las celdas vacias y chicas no fabrican una interaccion", {
  n <- 600L
  a <- rep(sprintf("A%02d", seq_len(10L)), length.out = n)
  b <- rep(sprintf("B%02d", seq_len(10L)), length.out = n)
  # Solo diez combinaciones aparecen: la grilla de 10 por 10 no tiene la
  # cobertura minima aunque cada nivel marginal tenga muchas filas.
  objetivo <- rep(1, n)
  objetivo[a == "A01" & b == "B01"] <- NA_real_
  datos <- data.frame(a = a, b = b, objetivo = objetivo,
                      stringsAsFactors = FALSE)
  perfil <- .perfilar_interaccion(datos)
  expect_equal(nrow(.hallazgos_interaccion(
    perfil, "ausencia_dependiente"
  )), 0L)

  a <- rep(c("A", "B"), each = 300L)
  b <- rep(c("A", "B"), times = 300L)
  objetivo <- rep(1, n)
  celda_chica <- which(a == "A" & b == "A")[seq_len(10L)]
  objetivo[celda_chica] <- NA_real_
  perfil <- .perfilar_interaccion(data.frame(
    a = a, b = b, objetivo = objetivo
  ))
  expect_equal(nrow(.hallazgos_interaccion(
    perfil, "ausencia_dependiente"
  )), 0L)
})

test_that("el azar no publica interacciones en 50 tablas", {
  set.seed(20261120L)
  falsas <- integer(50L)
  for (i in seq_along(falsas)) {
    n <- 800L
    datos <- data.frame(
      a = sample(c("A", "B"), n, replace = TRUE),
      b = sample(c("A", "B"), n, replace = TRUE),
      c = sample(c("A", "B"), n, replace = TRUE),
      d = sample(c("A", "B"), n, replace = TRUE),
      objetivo = ifelse(stats::runif(n) < 0.30, NA_real_, 1)
    )
    perfil <- .perfilar_interaccion(datos)
    falsas[[i]] <- nrow(.hallazgos_interaccion(
      perfil, "ausencia_dependiente"
    ))
  }
  expect_equal(sum(falsas), 0L)
})

test_that("la ausencia al azar no vence la correccion global en 50 tablas", {
  set.seed(20261122L)
  falsas <- integer(50L)
  for (i in seq_along(falsas)) {
    n <- 500L
    determinantes <- lapply(seq_len(6L), function(j) {
      sample(paste0("L", 1:3), n, replace = TRUE)
    })
    names(determinantes) <- paste0("d", seq_along(determinantes))
    datos <- as.data.frame(determinantes, stringsAsFactors = FALSE)
    datos$objetivo <- ifelse(stats::runif(n) < 0.30, NA_real_, 1)
    perfil <- .perfilar_interaccion(datos)
    falsas[[i]] <- sum(as.character(perfil$hallazgos$tipo_hallazgo) ==
      "ausencia_dependiente")
  }
  expect_equal(sum(falsas > 0L), 0L)
})

test_that("40 columnas categoricas respetan el tope de parejas", {
  set.seed(20261121L)
  n <- 800L
  columnas <- lapply(seq_len(40L), function(i) {
    sample(c("A", "B"), n, replace = TRUE)
  })
  names(columnas) <- sprintf("d%02d", seq_along(columnas))
  datos <- as.data.frame(columnas, stringsAsFactors = FALSE)
  datos$objetivo <- ifelse(stats::runif(n) < 0.30, NA_real_, 1)
  perfil <- .perfilar_interaccion(datos)
  expect_equal(nrow(.hallazgos_interaccion(
    perfil, "ausencia_dependiente"
  )), 0L)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[
    as.character(cobertura$diagnostico) == "ausencia_dependiente" &
      grepl("parejas de determinantes", cobertura$motivo, fixed = TRUE,
            useBytes = TRUE), , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "120", fixed = TRUE)
})

