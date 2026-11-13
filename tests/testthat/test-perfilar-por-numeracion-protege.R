# Sobre la tabla entera, una numeracion densa protege a sus valores: un `999`
# dentro de `1:2000` no se cuenta como faltante disfrazado, ni en
# `faltantes_disfrazados` ni en `faltantes`. Partida en grupos, cada grupo tiene
# que contar igual, y lo que la numeracion le protege queda declarado en la
# cobertura.
testthat::skip_on_cran()

.opciones_numeracion <- function() {
  list(
    fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE,
    normalizar = FALSE
  )
}

# El grupo `A` tiene 50 filas: `n_ausentes` NA reales, dos 999 y el resto
# numeros de la numeracion; `B` tiene lo que falta de `1:2000`. En la tabla
# entera la columna es una numeracion densa y el 999 cae adentro de su rango.
.tabla_numeracion <- function(n_ausentes, extra = numeric()) {
  n_numeros <- 48L - n_ausentes - length(extra)
  a <- c(seq_len(n_numeros), extra, 999, 999, rep(NA, n_ausentes))
  b <- setdiff(seq.int(n_numeros + 1L, 2000L), 999)
  data.frame(
    grupo = c(rep("A", length(a)), rep("B", length(b))),
    x = c(a, b),
    stringsAsFactors = FALSE
  )
}

.del_grupo <- function(resultado, grupo, tipo) {
  resultado[resultado$grupo == grupo & resultado$columna == "x" &
              resultado$tipo_hallazgo == tipo, , drop = FALSE]
}

.cobertura_numeracion <- function(resultado, grupo) {
  cobertura <- attr(resultado, "cobertura_diagnosticos", exact = TRUE)
  cobertura[cobertura$grupo == grupo & cobertura$columna == "x" &
              cobertura$diagnostico == "centinelas_numericos", , drop = FALSE]
}

# Las tres formas de guardar la misma numeracion, mas el factor.
.formas_numeracion <- function(datos) {
  formas <- list(
    numero = datos,
    texto = within(datos, x <- as.character(x)),
    factor = within(datos, {
      x <- as.character(x)
      x <- factor(x, levels = unique(x[!is.na(x)]))
    })
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    formas$integer64 <- within(datos, x <- bit64::as.integer64(x))
  }
  formas
}

test_that("lo que la numeracion protege no hace cruzar el umbral de faltantes", {
  datos <- .tabla_numeracion(4L)
  entero <- perfilar(datos, fecha = "2026-01-15")
  expect_true(isTRUE(
    entero$columnas$secuencia_entera_densa[entero$columnas$columna == "x"]
  ))
  expect_identical(
    as.integer(entero$columnas$n_faltantes_disfrazados[
      entero$columnas$columna == "x"
    ]),
    0L
  )
  resultado <- perfilar_por(datos, "grupo", min_filas = 10L,
                            fecha = "2026-01-15")
  # 4 de 50 es 0,08: debajo del umbral de 0,1. Los dos 999 son codigos de la
  # numeracion y no cuentan.
  expect_equal(nrow(.del_grupo(resultado, "A", "faltantes")), 0L)
  expect_equal(nrow(.del_grupo(resultado, "A", "faltantes_disfrazados")), 0L)
  # Lo propio del grupo sigue saliendo: los dos 999 son extremos del grupo.
  extremos <- .del_grupo(resultado, "A", "outliers")
  expect_equal(nrow(extremos), 1L)
  expect_equal(extremos$n_afectados[[1L]], 2)
  # Y nada se pierde en silencio: la cobertura dice que `faltantes` no se
  # publica y por que.
  declarado <- .cobertura_numeracion(resultado, "A")
  expect_equal(nrow(declarado), 1L)
  expect_match(declarado$motivo[[1L]], "`faltantes` no se publica",
               fixed = TRUE)
  expect_match(declarado$motivo[[1L]], "0.080", fixed = TRUE)
  expect_match(declarado$motivo[[1L]], "999", fixed = TRUE)
})

test_that("arriba del umbral, faltantes se publica solo con los ausentes reales", {
  datos <- .tabla_numeracion(6L)
  resultado <- perfilar_por(datos, "grupo", min_filas = 10L,
                            fecha = "2026-01-15")
  publicado <- .del_grupo(resultado, "A", "faltantes")
  expect_equal(nrow(publicado), 1L)
  expect_equal(publicado$n_afectados[[1L]], 6)
  expect_identical(publicado$evidencia[[1L]],
                   "6 ausentes reales y 0 disfrazados (0.120 del total)")
  expect_equal(nrow(.del_grupo(resultado, "A", "faltantes_disfrazados")), 0L)
  declarado <- .cobertura_numeracion(resultado, "A")
  expect_equal(nrow(declarado), 1L)
  expect_false(grepl("no se publica", declarado$motivo[[1L]], fixed = TRUE))
})

test_that("texto, factor e integer64 protegen lo mismo que el numero", {
  opciones <- .opciones_numeracion()
  for (n_ausentes in c(4L, 6L)) {
    for (forma in names(.formas_numeracion(.tabla_numeracion(n_ausentes)))) {
      datos <- .formas_numeracion(.tabla_numeracion(n_ausentes))[[forma]]
      resultado <- do.call(perfilar_por, c(
        list(datos, "grupo", min_filas = 10L), opciones
      ))
      info <- paste(forma, n_ausentes)
      publicado <- .del_grupo(resultado, "A", "faltantes")
      if (n_ausentes == 4L) {
        expect_equal(nrow(publicado), 0L, info = info)
      } else {
        expect_equal(publicado$n_afectados, 6, info = info)
      }
      expect_equal(nrow(.del_grupo(resultado, "A", "faltantes_disfrazados")),
                   0L, info = info)
      expect_equal(nrow(.cobertura_numeracion(resultado, "A")), 1L,
                   info = info)
    }
  }
})

test_that("en un hallazgo mixto lo textual se publica y lo protegido no", {
  # `"0"` es una ausencia escrita con texto -la declara quien conoce el dato- y
  # se lee como numero, asi que la columna entera sigue siendo la numeracion
  # `0:2000`. El `999` del grupo es un codigo de esa numeracion.
  a <- c(as.character(1:41), "0", "0", "999", "999", rep(NA, 5L))
  b <- as.character(setdiff(42:2000, 999))
  datos <- data.frame(
    grupo = c(rep("A", length(a)), rep("B", length(b))),
    x = c(a, b), stringsAsFactors = FALSE
  )
  opciones <- c(.opciones_numeracion(), list(cadenas_ausencia = "0"))
  entero <- do.call(perfilar, c(list(datos), opciones))
  de_la_columna <- entero$hallazgos[
    entero$hallazgos$columna == "x" &
      entero$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_equal(de_la_columna$n_afectados, 2)
  resultado <- do.call(perfilar_por, c(
    list(datos, "grupo", min_filas = 10L), opciones
  ))
  textual <- .del_grupo(resultado, "A", "faltantes_disfrazados")
  expect_equal(nrow(textual), 1L)
  expect_equal(textual$n_afectados[[1L]], 2)
  expect_identical(textual$evidencia[[1L]], "0 (2)")
  # 5 ausentes reales y los dos "0": 7 de 50.
  publicado <- .del_grupo(resultado, "A", "faltantes")
  expect_equal(publicado$n_afectados, 7)
  expect_equal(nrow(.cobertura_numeracion(resultado, "A")), 1L)
})

test_that("con la guarda abierta, el grupo cuenta solo lo que la abre", {
  # El -9 cae fuera del rango de la numeracion y abre la guarda en la tabla
  # entera; el 999 sigue adentro y sigue protegido. `perfilar()` cuenta el -9 y
  # no el 999; el grupo, igual.
  datos <- .tabla_numeracion(4L, extra = -9)
  opciones <- .opciones_numeracion()
  entero <- do.call(perfilar, c(list(datos), opciones))
  de_la_columna <- entero$hallazgos[
    entero$hallazgos$columna == "x" &
      entero$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_identical(de_la_columna$evidencia, "-9 (1)")
  resultado <- do.call(perfilar_por, c(
    list(datos, "grupo", min_filas = 10L), opciones
  ))
  disfrazados <- .del_grupo(resultado, "A", "faltantes_disfrazados")
  expect_identical(disfrazados$evidencia, "-9 (1)")
  expect_equal(disfrazados$n_afectados, 1)
  # 4 NA y el -9: 5 de 50, que no supera 0,1.
  expect_equal(nrow(.del_grupo(resultado, "A", "faltantes")), 0L)
  expect_equal(nrow(.cobertura_numeracion(resultado, "A")), 1L)
})

test_that("con un solo grupo, perfilar_por() publica lo mismo que perfilar()", {
  opciones <- .opciones_numeracion()
  firma <- function(x, columnas) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x <- x[as.character(x$columna) != "grupo", , drop = FALSE]
    if (!nrow(x)) return(character())
    .sort_por_bytes(vapply(seq_len(nrow(x)), function(i) {
      paste(vapply(intersect(columnas, names(x)), function(nombre) {
        paste(capture.output(dput(x[[nombre]][i])), collapse = "")
      }, character(1L)), collapse = "|")
    }, character(1L)))
  }
  campos_hallazgo <- c("columna", "tipo_hallazgo", "severidad", "n_afectados",
                       "evidencia")
  campos_cobertura <- c("diagnostico", "columna", "motivo")
  for (forma in names(.formas_numeracion(.tabla_numeracion(4L)))) {
    datos <- .formas_numeracion(.tabla_numeracion(4L))[[forma]]
    datos$grupo <- "todo"
    entero <- do.call(perfilar, c(list(datos), opciones))
    por <- do.call(perfilar_por, c(
      list(datos, "grupo", min_filas = 1L), opciones
    ))
    expect_identical(firma(por, campos_hallazgo),
                     firma(entero$hallazgos, campos_hallazgo), info = forma)
    expect_identical(
      firma(attr(por, "cobertura_diagnosticos", exact = TRUE),
            campos_cobertura),
      firma(entero$cobertura_diagnosticos, campos_cobertura), info = forma
    )
  }
})

test_that("fuera de perfilar_por() el grupo vuelve a decidir solo", {
  # La decision de la columna entera vale mientras se perfila cada grupo. Si
  # `perfilar_por()` aborta, un `perfilar()` posterior sobre la rebanada cuenta
  # con su propia guarda, como siempre.
  datos <- .tabla_numeracion(4L)
  expect_error(perfilar_por(datos, "grupo", min_filas = 10L,
                            umbral_faltantes_sospechoso = 2))
  solo <- perfilar(datos[datos$grupo == "A", "x", drop = FALSE],
                   fecha = "2026-01-15", analizar_dependencias = FALSE)
  expect_identical(as.integer(solo$columnas$n_faltantes_disfrazados), 2L)
})
