# El lector de las columnas de un hallazgo y las acciones de nombres: la
# trazabilidad manda sobre el texto de `columna`, una columna sin nombre no es
# "la tabla", y una accion de nombres que no corresponde a los datos no tira el
# plan entero.

fecha_lector <- as.POSIXct("2026-01-01", tz = "UTC")

relacion_con_columna_literal <- function(literales) {
  set.seed(9)
  a <- round(stats::runif(60, 1e9, 1e10), 2)
  b <- round(a * 0.5, 2)
  b[c(4, 9)] <- b[c(4, 9)] + 1
  d <- data.frame(a = a, b = b)
  for (k in seq_len(literales)) d[[paste0("lit", k)]] <- c(" x", rep("y", 59))
  names(d)[2L + seq_len(literales)] <- "a,b"
  d
}

test_that("una relacion entre a y b no se confunde con una columna llamada 'a,b'", {
  for (literales in 1:2) {
    d <- relacion_con_columna_literal(literales)
    perfil <- suppressWarnings(perfilar(d, fecha = fecha_lector))
    plan <- planificar_limpieza(perfil, d)
    amb <- attr(plan, "hallazgos_sin_accion_por_columna_ambigua")
    sa <- attr(plan, "hallazgos_sin_accion")
    expect_false("relacion_aritmetica_columnas" %in% amb$hallazgo, info = literales)
    fila <- sa[sa$hallazgo == "relacion_aritmetica_columnas", ]
    expect_equal(nrow(fila), 1L, info = literales)
    expect_match(fila$motivo, "varias columnas (`a`, `b`)", fixed = TRUE, info = literales)
  }
})

test_that("una columna sin nombre no se llama 'de la tabla'", {
  d <- data.frame(c(" x", NA, "y ", NA, "z"), 1:5, stringsAsFactors = FALSE)
  names(d) <- c(NA, "b")
  perfil <- suppressWarnings(perfilar(d, fecha = fecha_lector))
  plan <- planificar_limpieza(perfil, d)
  amb <- attr(plan, "hallazgos_sin_accion_por_columna_ambigua")
  sa <- attr(plan, "hallazgos_sin_accion")
  expect_true(all(c("faltantes", "espacios_sobrantes") %in% amb$hallazgo))
  expect_true(all(grepl("no tiene nombre", amb$motivo[is.na(amb$columna)], fixed = TRUE)))
  expect_false(any(grepl("de la tabla", sa$motivo, fixed = TRUE)))
})

test_that("una accion de nombres para otros nombres se bloquea en el plan y no tira aplicar()", {
  d2 <- data.frame(a = c(" p", "q"), b = c("r", "s"), stringsAsFactors = FALSE)
  names(d2) <- c("x", "x")
  perfil <- suppressWarnings(perfilar(d2, fecha = fecha_lector))
  d1 <- data.frame(x = c(" p", "q"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfil, d1)
  nombres <- plan$estrategia %in% c("normalizar_nombres", "normalizar_nombres_snake_case")
  expect_true(any(nombres))
  expect_true(all(as.character(plan$estado[nombres]) == "bloqueada"))
  expect_true(all(grepl("no son los que el perfil vio", plan$justificacion[nombres], fixed = TRUE)))
  # Sin datos el plan no lo sabe y la deja lista: aplicar() la registra fallida y
  # sigue con las demas.
  sin_datos <- planificar_limpieza(perfil)
  activas <- sin_datos
  activas$aplicar <- as.character(activas$estado) == "lista" & activas$estrategia == "normalizar_nombres"
  r <- aplicar(activas, d1)
  fila <- r$registro[r$registro$estrategia == "normalizar_nombres", ]
  expect_equal(fila$estado, "fallida")
  expect_match(fila$error, "no coinciden", fixed = TRUE)
})

test_that("una columna de nombre vacio que falta en los datos bloquea su grupo", {
  d <- data.frame(a = 1:5, x = 1:5)
  names(d) <- c("", "x")
  perfil <- suppressWarnings(perfilar(d, fecha = fecha_lector))
  plan <- planificar_limpieza(perfil, data.frame(x = 1:5))
  dup <- plan$hallazgo == "columnas_duplicadas"
  expect_true(any(dup))
  expect_true(all(as.character(plan$estado[dup]) == "bloqueada"))
  expect_true(inherits(aplicar(plan, data.frame(x = 1:5)), "resultado_limpieza"))
})

test_that("los tipos de hallazgo de la tabla son los que se publican con columna NA", {
  ruta <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(ruta))
  tipos <- character()
  recorrer <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name(".nuevo_hallazgo")) && length(e) >= 3L &&
          identical(e[[2L]], quote(NA_character_)) && is.character(e[[3L]])) {
        tipos <<- c(tipos, e[[3L]])
      }
      for (x in as.list(e)[-1L]) if (!missing(x)) recorrer(x)
    }
  }
  for (f in list.files(ruta, pattern = "[.]R$", full.names = TRUE)) {
    for (e in parse(f, keep.source = FALSE)) recorrer(e)
  }
  expect_setequal(unique(tipos), lupa:::.TIPOS_HALLAZGO_DE_LA_TABLA)
})

test_that("un hallazgo de varias columnas con una sin nombre no se puede identificar", {
  d <- data.frame(u = 1:20, v = 2 * (1:20), w = 4)
  names(d) <- c("u", NA, "w")
  d[[2L]][20L] <- 999
  p <- perfilar(d, analizar_dependencias = FALSE,
                fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_true(any(p$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"))
  pl <- planificar_limpieza(p, datos = d)
  ambiguas <- attr(pl, "hallazgos_sin_accion_por_columna_ambigua")
  fila <- ambiguas[ambiguas$hallazgo == "relacion_aritmetica_columnas", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_match(fila$motivo, "Una de las columnas del hallazgo no tiene nombre",
               fixed = TRUE)
  sin_accion <- attr(pl, "hallazgos_sin_accion")
  expect_false(any(sin_accion$hallazgo == "relacion_aritmetica_columnas"))
  # Una columna llamada con el texto "NA" tiene nombre, y el plan actua sobre ella.
  d2 <- data.frame(u = 1:20, x = 7)
  names(d2) <- c("u", "NA")
  p2 <- perfilar(d2, analizar_dependencias = FALSE,
                 fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  pl2 <- planificar_limpieza(p2, datos = d2)
  expect_true(any(pl2$columna %in% "NA"))
  expect_false(any(attr(pl2, "hallazgos_sin_accion_por_columna_ambigua")$columna %in% "NA"))
})

test_that("unos parametros que no son una lista fallan con un motivo del dominio", {
  d <- data.frame(x = c(1:19, -999), y = 1:20)
  p <- perfilar(d, analizar_dependencias = FALSE,
                fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  pl <- planificar_limpieza(p, datos = d)
  i <- which(pl$estrategia == "convertir_sentinelas_numericos")[[1L]]
  pl$parametros[[i]] <- c(-9999)
  pl$aplicar[i] <- TRUE
  r <- aplicar(pl, d)
  registro <- as.data.frame(r$registro)
  expect_identical(registro$estado[registro$estrategia == "convertir_sentinelas_numericos"],
                   "fallida")
  error <- registro$error[registro$estrategia == "convertir_sentinelas_numericos"]
  expect_match(error, "no tienen la forma que la acci", fixed = TRUE)
  expect_false(grepl("operator", error, fixed = TRUE))
  expect_identical(r$datos$x, d$x)
})
