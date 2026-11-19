# Corre en la integracion continua, no en el tiempo de CRAN.
testthat::skip_on_cran()

# Una columna enteramente ausente en toda la tabla se descarta en cada grupo
# (en un formato largo es la columna de valor de un atributo que el extracto no
# trae). Se informa una vez, a nivel de tabla, en `attr(, "hallazgos_tabla")`:
# una fila que no es un grupo no puede vivir en la tabla de grupos sin romper a
# quien suma `n_filas_grupo`, cuenta grupos o los separa por `grupo`.

.fecha_vacia <- as.POSIXct("2026-01-15", tz = "UTC")

.por_silencio <- function(...) suppressWarnings(suppressMessages(perfilar_por(...)))

.tabla_vacia <- function(x = NA_real_) {
  data.frame(g = rep(c("A", "B"), each = 40L), y = 1:80, x = x)
}

test_that("la columna vacia en toda la tabla se informa una vez, fuera de la tabla de grupos", {
  d <- .tabla_vacia()
  por <- .por_silencio(d, "g", fecha = .fecha_vacia)
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  expect_identical(nrow(ht), 1L)
  expect_identical(ht$columna, "x")
  expect_identical(ht$tipo_hallazgo, "faltantes")
  expect_identical(ht$unidad_conteo, "fila")
  expect_identical(ht$n_afectados, 80)
  expect_match(ht$sugerencia, "no trae", fixed = TRUE)
  # La tabla de grupos queda como antes: ninguna fila sobre `x`.
  expect_false(any(as.data.frame(por)$columna == "x"))
  expect_false(anyNA(por$grupo))
  # Lo declarado por grupo no cambia.
  cobertura <- attr(por, "cobertura_grupos", exact = TRUE)
  expect_true(all(grepl("x", cobertura$columnas_descartadas, fixed = TRUE)))
})

test_that("el libro de filas y la invariante de los grupos valen con una columna vacia", {
  # Es la invariante de test-fugas-proteccion: sumar `n_filas_grupo` de los
  # grupos distintos da las filas de la tabla y ningun grupo tiene cero filas.
  d <- .tabla_vacia()
  por <- .por_silencio(d, "g", fecha = .fecha_vacia)
  distintos <- unique(por[, c("grupo", "n_filas_grupo")])
  expect_identical(sum(distintos$n_filas_grupo), nrow(d))
  expect_true(all(distintos$n_filas_grupo > 0L))
  expect_true(all(is.finite(por$n_afectados / por$n_filas_grupo)))
  expect_identical(length(unique(por$grupo)), 2L)
  expect_identical(
    sum(vapply(unique(por$grupo), function(g) {
      por$n_filas_grupo[por$grupo == g][[1L]]
    }, integer(1L))),
    80L
  )
})

test_that("la severidad sale de los umbrales de perfilar() y coincide con un solo grupo", {
  d <- data.frame(g = "A", x = NA_real_, y = 1:80)
  p <- perfilar(d[, c("x", "y")], fecha = .fecha_vacia, analizar_dependencias = FALSE)
  esperado <- p$hallazgos[p$hallazgos$columna == "x" &
                            p$hallazgos$tipo_hallazgo == "faltantes", ]
  por <- .por_silencio(d, "g", min_filas = 1L, fecha = .fecha_vacia)
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  expect_identical(as.character(ht$severidad), as.character(esperado$severidad))
  expect_identical(ht$evidencia, esperado$evidencia)
  # Con los umbrales en 1 el diagnostico se apaga, tambien el aviso.
  apagado <- .por_silencio(.tabla_vacia(), "g", fecha = .fecha_vacia,
                           umbral_faltantes_sospechoso = 1,
                           umbral_faltantes_error = 1)
  expect_identical(nrow(attr(apagado, "hallazgos_tabla", exact = TRUE)), 0L)
})

test_that("un extracto largo estrecho no recibe faltantes por grupo", {
  n <- 60L
  a <- rep(c("pais", "edad"), times = n)
  eav <- data.frame(
    entidad = rep(seq_len(n), each = 2L), atributo = a,
    valor_texto = ifelse(a == "pais", rep(c("UY", "AR"), length.out = 2L * n),
                         NA_character_),
    valor_num = ifelse(a == "edad", rep(20:79, length.out = 2L * n), NA_real_),
    valor_fecha = as.Date(rep(NA_real_, 2L * n)),
    stringsAsFactors = FALSE
  )
  por <- .por_silencio(eav, "atributo", clave = "entidad", min_filas = 30L,
                       fecha = .fecha_vacia)
  expect_false(any(por$tipo_hallazgo == "faltantes"))
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  expect_identical(ht$columna[ht$tipo_hallazgo == "faltantes"], "valor_fecha")
})

test_that("el aviso se anuncia al ejecutar", {
  expect_message(
    suppressWarnings(perfilar_por(.tabla_vacia(), "g", fecha = .fecha_vacia)),
    "hallazgos_tabla"
  )
})

test_that("el mensaje nombra las columnas y calla si solo hay informativos", {
  d <- .tabla_vacia()
  expect_message(
    suppressWarnings(perfilar_por(d, "g", fecha = .fecha_vacia)),
    "`x`", fixed = TRUE
  )
  # Una columna opcional vacia deja solo `regla_silencia_ausencia` / ok.
  expect_no_message(suppressWarnings(
    perfilar_por(d, "g", columnas_opcionales = "x", fecha = .fecha_vacia)
  ))
  # Muchas columnas: las primeras y "y N mas"; un nombre con llaves no se evalua.
  muchas <- data.frame(g = rep(c("A", "B"), each = 40L), y = 1:80)
  for (k in 1:7) muchas[[paste0("v", k)]] <- NA_real_
  names(muchas)[3L] <- "{1+1}"
  expect_message(
    suppressWarnings(perfilar_por(muchas, "g", fecha = .fecha_vacia)),
    "`{1+1}`", fixed = TRUE
  )
  expect_message(
    suppressWarnings(perfilar_por(muchas, "g", fecha = .fecha_vacia)),
    "y 2 m", fixed = TRUE
  )
})

test_that("con todos los grupos por debajo de min_filas la columna igual se nombra", {
  d <- data.frame(g = rep(c("A", "B"), each = 5L), x = NA_real_, y = 1:10)
  por <- .por_silencio(d, "g", min_filas = 30L, fecha = .fecha_vacia)
  expect_identical(attr(por, "hallazgos_tabla", exact = TRUE)$columna, "x")
})

test_that("una clave vacia se nombra si ningun grupo se perfilo, y no si alguno si", {
  chica <- data.frame(g = rep(c("A", "B"), each = 5L), k = NA_integer_, y = 1:10)
  sin_grupos <- .por_silencio(chica, "g", clave = "k", min_filas = 30L,
                              fecha = .fecha_vacia)
  expect_true("k" %in% attr(sin_grupos, "hallazgos_tabla", exact = TRUE)$columna)
  d <- data.frame(g = rep(c("A", "B"), each = 40L), k = NA_integer_, y = 1:80)
  con_grupos <- .por_silencio(d, "g", clave = "k", fecha = .fecha_vacia)
  expect_false("k" %in% attr(con_grupos, "hallazgos_tabla", exact = TRUE)$columna)
  expect_true(any(con_grupos$columna == "k" & con_grupos$tipo_hallazgo == "faltantes"))
})

test_that("una columna de NaN conserva valores_no_finitos", {
  por <- .por_silencio(.tabla_vacia(NaN), "g", fecha = .fecha_vacia)
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  expect_true(all(c("faltantes", "valores_no_finitos") %in% ht$tipo_hallazgo))
})

test_that("aplicabilidad: se excluye si la regla deja cero filas, no si hace aplicable la columna", {
  d <- .tabla_vacia()
  todas <- .por_silencio(d, "g", aplicabilidad = list(x = ~ y > 0),
                         fecha = .fecha_vacia)
  expect_true("faltantes" %in% attr(todas, "hallazgos_tabla", exact = TRUE)$tipo_hallazgo)
  constante <- .por_silencio(d, "g", aplicabilidad = list(x = ~ TRUE),
                             fecha = .fecha_vacia)
  expect_true("faltantes" %in% attr(constante, "hallazgos_tabla", exact = TRUE)$tipo_hallazgo)
  ninguna <- .por_silencio(d, "g", aplicabilidad = list(x = ~ y > 1000),
                           fecha = .fecha_vacia)
  expect_false("faltantes" %in% attr(ninguna, "hallazgos_tabla", exact = TRUE)$tipo_hallazgo)
})

test_that("lo declarado opcional no recibe faltantes y una columna compuesta no recibe aviso", {
  d <- .tabla_vacia()
  opcional <- .por_silencio(d, "g", columnas_opcionales = "x", fecha = .fecha_vacia)
  expect_false("faltantes" %in% attr(opcional, "hallazgos_tabla", exact = TRUE)$tipo_hallazgo)
  d$m <- matrix(NA_real_, 80L, 2L)
  por <- .por_silencio(d, "g", fecha = .fecha_vacia)
  expect_false("m" %in% attr(por, "hallazgos_tabla", exact = TRUE)$columna)
})

test_that("sin columnas vacias el atributo existe y esta vacio, con el esquema de los hallazgos", {
  d <- data.frame(g = rep(c("A", "B"), each = 40L), y = 1:80)
  por <- perfilar_por(d, "g", fecha = .fecha_vacia)
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  expect_identical(nrow(ht), 0L)
  expect_identical(names(ht), names(.hallazgos_vacios()))
})

test_that("una columna con nombre vacio no rompe el aviso", {
  d <- .tabla_vacia()
  names(d)[3L] <- ""
  expect_no_error(.por_silencio(d, "g", fecha = .fecha_vacia))
})

test_that("el aviso de tabla coincide campo a campo con perfilar() sobre la tabla entera", {
  # Mismos conteos, severidad, evidencia, unidad y traza; solo la sugerencia
  # agrega la lectura de formato largo.
  tipos <- c("faltantes", "valores_no_finitos", "regla_silencia_ausencia")
  comparar <- function(d, ...) {
    por <- .por_silencio(d, "g", fecha = .fecha_vacia, ...)
    ht <- attr(por, "hallazgos_tabla", exact = TRUE)
    p <- perfilar(d[, setdiff(names(d), "g"), drop = FALSE], fecha = .fecha_vacia,
                  analizar_dependencias = FALSE, ...)
    h <- p$hallazgos
    h <- h[h$columna == "x" & h$tipo_hallazgo %in% tipos, , drop = FALSE]
    expect_gt(nrow(ht), 0L)
    expect_identical(nrow(ht), nrow(h))
    ht <- ht[order(ht$tipo_hallazgo), ]
    h <- h[order(h$tipo_hallazgo), ]
    for (campo in setdiff(names(h), c("sugerencia"))) {
      expect_identical(unname(unclass(ht[[campo]])), unname(unclass(h[[campo]])),
                       info = campo)
    }
    expect_true(all(startsWith(ht$sugerencia, h$sugerencia)))
  }
  comparar(.tabla_vacia())
  # Varias columnas vacias del mismo tipo comparten el perfilado: cada una
  # sigue llevando su nombre, su conteo y su traza.
  varias <- .tabla_vacia()
  varias$x2 <- NA_real_
  varias$x3 <- NaN
  por <- .por_silencio(varias, "g", fecha = .fecha_vacia)
  ht <- attr(por, "hallazgos_tabla", exact = TRUE)
  p <- perfilar(varias[, c("y", "x", "x2", "x3")], fecha = .fecha_vacia,
                analizar_dependencias = FALSE)
  for (col in c("x", "x2", "x3")) {
    a <- ht[ht$columna == col & ht$tipo_hallazgo == "faltantes", ]
    b <- p$hallazgos[p$hallazgos$columna == col &
                       p$hallazgos$tipo_hallazgo == "faltantes", ]
    expect_identical(nrow(a), 1L, info = col)
    for (campo in setdiff(names(b), "sugerencia")) {
      expect_identical(unname(unclass(a[[campo]])), unname(unclass(b[[campo]])),
                       info = paste(col, campo))
    }
  }
  comparar(.tabla_vacia(NaN))
  comparar(.tabla_vacia(), aplicabilidad = list(x = ~ y > 0))
  comparar(.tabla_vacia(), columnas_opcionales = "x")
  # El tope de la traza se respeta.
  comparar(.tabla_vacia(), max_filas_hallazgo = 10L)
})
