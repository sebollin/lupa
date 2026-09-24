# O40: `perfilar_por()` devolvia dos columnas cuando ningun grupo producia
# hallazgos y trece cuando si, con la misma clase. Quien lo consume no podia
# distinguir "no se miro" de "salio limpio" por la forma del objeto:
# `x$tipo_hallazgo` devolvia NULL y `x[, "columna"]` rompia.

test_that("sin hallazgos el objeto por grupo conserva su esquema", {
  datos <- data.frame(
    g = rep(c("a", "b"), each = 10L),
    v = as.numeric(seq_len(20L))
  )

  vacio <- perfilar_por(datos, "g", analizar_dependencias = FALSE)
  tabla <- as.data.frame(vacio)

  expect_equal(nrow(tabla), 0L)
  expect_true(all(c("grupo", "n_filas_grupo", "columna", "tipo_hallazgo",
                    "severidad", "n_evaluados", "n_afectados",
                    "trazabilidad") %in% names(tabla)))
  expect_false(is.null(vacio$tipo_hallazgo))
  expect_true(is.ordered(tabla$severidad))
  expect_equal(levels(tabla$severidad), c("ok", "sospechoso", "error"))
})

test_that("con hallazgos el esquema es el mismo", {
  # Control: la forma no depende del contenido, en las dos direcciones.
  datos <- data.frame(
    g = rep(c("a", "b"), each = 30L),
    v = c(rep(" x ", 30L), rep("y", 30L)),
    stringsAsFactors = FALSE
  )

  con_hallazgos <- perfilar_por(datos, "g", analizar_dependencias = FALSE,
                                proteger_datos_personales = FALSE)
  sin_hallazgos <- perfilar_por(
    data.frame(g = rep(c("a", "b"), each = 10L), v = as.numeric(seq_len(20L))),
    "g", analizar_dependencias = FALSE
  )

  expect_true(nrow(as.data.frame(con_hallazgos)) > 0L)
  expect_equal(names(as.data.frame(con_hallazgos)),
               names(as.data.frame(sin_hallazgos)))
  expect_equal(class(con_hallazgos), class(sin_hallazgos))
})

test_that("el perfil sin hallazgos tambien trae el esquema", {
  # La misma tabla vacia la usa `perfilar()`: una tabla sin un solo hallazgo
  # sigue publicando las columnas con las que se la consulta.
  datos <- data.frame(v = as.numeric(seq_len(10L)))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  tabla <- as.data.frame(hallazgos(perfil))

  expect_true(all(c("columna", "tipo_hallazgo", "severidad", "n_evaluados",
                    "n_afectados", "trazabilidad") %in% names(tabla)))
  expect_true(is.ordered(tabla$severidad))
})
