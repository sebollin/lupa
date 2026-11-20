# Validez de dominio: lo que cae fuera de una regla inferida por el nombre
# siempre produce algo. Con pocos valores fuera se acusan las filas; con muchos
# la columna no sostiene la regla y el aviso es de la columna, con las dos
# lecturas y sin acusar filas.

testthat::skip_on_cran()

.F_O184c <- as.Date("2026-11-19")
.tipos_O184c <- c("valor_fuera_dominio", "fecha_futura", "fecha_centinela")

.dominio_de_O184c <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184c, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% .tipos_O184c, , drop = FALSE]
}

testthat::test_that("un error sistematico deja un aviso de columna, no silencio", {
  casos <- list(
    edad_130_149 = data.frame(edad = 130:149),
    pct_101_120 = data.frame(pct = 101:120),
    registro_2030 = data.frame(
      fecha_registro = as.Date("2030-01-01") + 0:19
    ),
    pocas_filas = data.frame(edad = c(30, 40, 50, 60, 999))
  )
  for (nombre in names(casos)) {
    h <- .dominio_de_O184c(casos[[nombre]])
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_identical(as.character(h$severidad), "ok", info = nombre)
    testthat::expect_true(h$n_afectados > 0, info = nombre)
    testthat::expect_true(grepl("unidad", h$descripcion, fixed = TRUE),
                          info = nombre)
    testthat::expect_true(grepl("sistem", h$descripcion, fixed = TRUE),
                          info = nombre)
  }
})

testthat::test_that("el aviso es monotono con el tamano y la proporcion", {
  esperado <- function(n, k) if (n < 5L || k * 10L <= n) "sospechoso" else "ok"
  tamanos <- c(3L, 4L, 5L, 6L, 9L, 10L, 11L, 20L, 50L, 100L)
  proporciones <- c(0.01, 0.05, 0.10, 0.11, 0.15, 0.25, 0.5, 1)
  for (n in tamanos) {
    ks <- unique(pmin(n, pmax(1L, ceiling(proporciones * n - 1e-9))))
    for (k in ks) {
      edad <- c(rep(40, n - k), rep(150, k))
      h <- .dominio_de_O184c(data.frame(edad = edad))
      info <- paste0("n=", n, " k=", k)
      testthat::expect_equal(nrow(h), 1L, info = info)
      testthat::expect_identical(
        as.character(h$severidad), esperado(n, k), info = info
      )
      testthat::expect_equal(h$n_afectados, k, info = info)
      fechas <- c(rep(.F_O184c - 400L, n - k), rep(.F_O184c + 400L, k))
      hf <- .dominio_de_O184c(data.frame(fecha_registro = fechas))
      testthat::expect_equal(nrow(hf), 1L, info = info)
      testthat::expect_identical(
        as.character(hf$severidad), esperado(n, k), info = info
      )
    }
  }
})

testthat::test_that("un nombre que anuncia otra unidad o valores esperados no infiere", {
  set.seed(1901)
  n <- 60L
  datos <- data.frame(
    longitud_cable = sample(200:5000, n, TRUE),
    edad_paciente_dias = sample(0:30000, n, TRUE),
    pct_change = sample(120:400, n, TRUE),
    fecha_prevista = .F_O184c + seq_len(n),
    longitud_texto = sample(1:500, n, TRUE),
    pct_of_sales = sample(0:300, n, TRUE),
    yoy_pct = sample(-50:300, n, TRUE)
  )
  testthat::expect_equal(nrow(.dominio_de_O184c(datos)), 0L)
  # Un valor suelto lejos del dominio, con la columna adentro, si se acusa.
  suelto <- data.frame(edad = c(sample(0:90, 59L, TRUE), 999))
  h <- .dominio_de_O184c(suelto)
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
})

testthat::test_that("perfilar_por decide el sosten con la tabla entera", {
  # 40 filas, 4 errores (10%): la tabla los acusa. El grupo A, de 10 filas,
  # tiene 4 (40%), y el grupo B ninguno: A tiene que acusar igual.
  set.seed(1902)
  t <- data.frame(
    grupo = rep(c("A", "B"), c(10L, 30L)),
    edad = c(c(sample(0:90, 6L, TRUE), rep(150, 4L)), sample(0:90, 30L, TRUE))
  )
  global <- .dominio_de_O184c(t[c("edad")])
  testthat::expect_identical(as.character(global$severidad), "sospechoso")
  por <- suppressWarnings(perfilar_por(
    t, por = "grupo", min_filas = 1L, fecha = .F_O184c,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_equal(nrow(d), 1L)
  testthat::expect_identical(as.character(d$severidad), "sospechoso")
  testthat::expect_equal(d$n_afectados, 4)
  # Un grupo de 4 filas y uno de 5 con el mismo valor suelto leen igual.
  chico <- data.frame(
    grupo = rep(c("A", "B", "C"), c(4L, 5L, 91L)),
    edad = c(c(30, 40, 50, 150), c(30, 40, 50, 60, 150),
             sample(0:90, 91L, TRUE))
  )
  por <- suppressWarnings(perfilar_por(
    chico, por = "grupo", min_filas = 1L, fecha = .F_O184c,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_setequal(as.character(d$grupo), c("A", "B"))
  testthat::expect_true(all(d$severidad == "sospechoso"))
})

testthat::test_that("comparar un 10% con un 15% fuera es un cambio, no una resolucion", {
  set.seed(1903)
  base <- sample(0:90, 20L, TRUE)
  p10 <- suppressWarnings(perfilar(
    data.frame(edad = c(base[1:18], 150, 150)), fecha = .F_O184c,
    analizar_dependencias = FALSE
  ))
  p15 <- suppressWarnings(perfilar(
    data.frame(edad = c(base[1:17], 150, 150, 150)), fecha = .F_O184c,
    analizar_dependencias = FALSE
  ))
  cmp <- comparar_perfiles(p10, p15)
  testthat::expect_false(any(
    cmp$cambio == "resuelto" & cmp$valor_anterior == "valor_fuera_dominio"
  ))
  fila <- cmp[cmp$aspecto == "severidad_hallazgo" &
                cmp$columna == "edad", , drop = FALSE]
  testthat::expect_equal(nrow(fila), 1L)
  testthat::expect_identical(as.character(fila$cambio), "modificado")
})
