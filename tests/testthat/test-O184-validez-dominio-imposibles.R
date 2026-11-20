# Validez de dominio: lo imposible en cualquier unidad se acusa por fila aunque
# la columna no sostenga la regla; un nombre ambiguo no infiere nada; y un
# centinela que es la mayoria de la columna es una convencion, no un valor suelto.

testthat::skip_on_cran()

.F_O184d <- as.Date("2026-11-19")

.dom_O184d <- function(datos) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184d, analizar_dependencias = FALSE
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

testthat::test_that("lo imposible se acusa por fila con cualquier proporcion", {
  set.seed(1911)
  pasadas <- .F_O184d - sample(3000:20000, 100L)
  for (nombre in c("birth_date", "date_of_birth", "dob", "fecha_nacimiento")) {
    for (k in c(4L, 10L, 20L)) {
      f <- c(pasadas[seq_len(20L - k)], .F_O184d + seq_len(k) * 100L)
      h <- .dom_O184d(setNames(data.frame(f), nombre))
      h <- h[h$tipo_hallazgo == "fecha_futura", , drop = FALSE]
      info <- paste(nombre, k)
      testthat::expect_equal(nrow(h), 1L, info = info)
      testthat::expect_identical(as.character(h$severidad), "sospechoso",
                                 info = info)
      testthat::expect_equal(h$n_afectados, k, info = info)
    }
  }
  h <- .dom_O184d(data.frame(edad = -20:-1))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  h <- .dom_O184d(data.frame(edad = c(sample(0:90, 85L, TRUE), rep(-1, 15L))))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_equal(h$n_afectados, 15)
  h <- .dom_O184d(data.frame(latitud = 91:110))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
  testthat::expect_equal(h$n_afectados, 20)
  # Lo que depende de la escala sigue siendo un aviso de columna.
  h <- .dom_O184d(data.frame(edad = 131:150))
  testthat::expect_identical(as.character(h$severidad), "ok")
  h <- .dom_O184d(data.frame(pct = 101:120))
  testthat::expect_identical(as.character(h$severidad), "ok")
  # Reescalado consistente (microgrados): el aviso de la columna, no filas.
  h <- .dom_O184d(data.frame(latitud = round(runif(40, -35, -30) * 1e6)))
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_identical(as.character(h$severidad), "ok")
})

testthat::test_that("un nombre ambiguo no infiere dominio, ni por fila", {
  set.seed(1912)
  for (nombre in c("pct_of_sales", "porcentaje_sobre_costo")) {
    for (k in c(1L, 4L)) {
      v <- c(sample(0:100, 20L - k, TRUE), rep(150, k))
      testthat::expect_equal(
        nrow(.dom_O184d(setNames(data.frame(v), nombre))), 0L,
        info = paste(nombre, k)
      )
    }
  }
  # Con de, del o por el nombre es ambiguo: solo avisa de la columna (`ok`), sin
  # acusar filas (ver test-O184-validez-dominio-matices.R).
  for (nombre in c("porcentaje_del_costo", "porcentaje_de_ventas",
                   "pct_por_categoria")) {
    v <- c(sample(0:100, 19L, TRUE), 150)
    h <- .dom_O184d(setNames(data.frame(v), nombre))
    testthat::expect_true(all(h$severidad == "ok"), info = nombre)
  }
})

testthat::test_that("un centinela que es la mayoria es una convencion de la columna", {
  set.seed(1913)
  vig <- as.character(.F_O184d - sample(1:900, 20L))
  casos <- list(
    fecha_desde = c(rep("1900-01-01", 40L), vig),
    fecha_inicio = c(rep("1900-01-01", 40L), vig),
    fecha_hasta = c(rep("9999-12-31", 40L), vig),
    fecha_fin = c(rep("9999-12-31", 20L), vig[1:10])
  )
  for (nombre in names(casos)) {
    h <- .dom_O184d(setNames(data.frame(casos[[nombre]]), nombre))
    h <- h[h$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
    testthat::expect_equal(nrow(h), 1L, info = nombre)
    testthat::expect_identical(as.character(h$severidad), "ok", info = nombre)
    testthat::expect_true(grepl("intervalo abierto|sin fin", h$descripcion),
                          info = nombre)
  }
  # Dos 1970-01-01 reales no son un centinela.
  ref <- c("1970-01-01", "1970-01-01",
           as.character(as.Date("2020-01-01") + sample(0:300, 18L)))
  testthat::expect_equal(nrow(.dom_O184d(data.frame(fecha_ref = ref))), 0L)
  # Un centinela suelto sigue acusando sus filas.
  nac <- c("1900-01-01", "1900-01-01",
           as.character(.F_O184d - sample(5000:20000, 48L)))
  h <- .dom_O184d(data.frame(fecha_nac = nac))
  testthat::expect_identical(as.character(h$severidad), "sospechoso")
})
