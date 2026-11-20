# Validez de dominio: una convencion de longitud tolera un valor de mas, los
# porcentajes con `de`/`del`/`por` solo avisan, los centinelas de fecha no se
# esconden entre si, los factores se nombran como posibilidad, y los nombres de
# pareja y de edad se reconocen sin falsos.

testthat::skip_on_cran()

.F_O184h <- as.Date("2026-11-19")

.dom_O184h <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .F_O184h, analizar_dependencias = FALSE, ...
  ))
  h <- perfil$hallazgos
  h[h$tipo_hallazgo %in% c(
    "valor_fuera_dominio", "fecha_futura", "fecha_centinela"
  ), , drop = FALSE]
}

.long_O184h <- function(v) {
  h <- .dom_O184h(data.frame(longitud = v, latitud = runif(length(v), -40, 40)))
  h[h$columna == "longitud", , drop = FALSE]
}

testthat::test_that("la convencion de longitud tolera un valor fuera de toda convencion", {
  set.seed(1961)
  h <- .long_O184h(c(190:350, 400))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 1)
  testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 161)
  h <- .long_O184h(c(runif(90, 0, 170), runif(10, 200, 300), 400))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 1)
  testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 10)
  # Dos convenciones mezcladas: un aviso de consistencia, no filas sospechosas.
  h <- .long_O184h(c(-170:-161, 190:199))
  testthat::expect_equal(nrow(h), 1L)
  testthat::expect_identical(as.character(h$severidad), "ok")
  testthat::expect_true(grepl("mezcla las dos convenciones", h$evidencia,
                              fixed = TRUE))
  # Un -10 suelto entre 190:350 tambien es la mezcla, no 99 filas imposibles.
  h <- .long_O184h(c(190:350, -10))
  testthat::expect_identical(as.character(h$severidad), "ok")
})

testthat::test_that("un porcentaje con de, del o por solo avisa de la columna", {
  set.seed(1962)
  for (nombre in c("porcentaje_de_avance", "porcentaje_del_costo",
                   "pct_por_categoria")) {
    for (k in c(1L, 4L)) {
      v <- c(sample(0:100, 20L - k, TRUE), rep(150, k))
      h <- .dom_O184h(setNames(data.frame(v), nombre))
      testthat::expect_equal(nrow(h), 1L, info = paste(nombre, k))
      testthat::expect_identical(as.character(h$severidad), "ok",
                                 info = paste(nombre, k))
      testthat::expect_true(grepl("si es un porcentaje acotado",
                                  h$evidencia, fixed = TRUE))
    }
  }
  # Los relativos por `of`, `sobre`... siguen sin inferir.
  v <- c(sample(0:100, 16L, TRUE), rep(150, 4L))
  testthat::expect_equal(nrow(.dom_O184h(data.frame(pct_of_sales = v))), 0L)
})

testthat::test_that("un centinela no esconde a otro al medir el aislamiento", {
  set.seed(1963)
  reales <- as.character(as.Date("1985-01-01") + sample(1:12000, 57L))
  h <- .dom_O184h(data.frame(
    fecha_hasta = c("9999-12-31", "1900-01-01", "1900-01-01", reales)
  ))
  h <- h[h$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
  testthat::expect_true(any(grepl("1900-01-01", h$evidencia, fixed = TRUE)))
  testthat::expect_true(any(grepl("9999-12-31", h$evidencia, fixed = TRUE)))
  reales <- as.character(as.Date("1985-01-01") + sample(1:12000, 20L))
  h <- .dom_O184h(data.frame(
    fecha_inicio = c(rep("1900-01-01", 20L), rep("9999-12-31", 20L), reales)
  ))
  h <- h[h$tipo_hallazgo == "fecha_centinela", , drop = FALSE]
  testthat::expect_true(any(grepl("1900-01-01", h$evidencia, fixed = TRUE)))
})

testthat::test_that("los factores que explican se nombran como posibilidad", {
  h <- .dom_O184h(data.frame(edad = 0:240))
  testthat::expect_true(grepl("Al dividir por 12", h$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("meses", h$evidencia, fixed = TRUE))
  h <- .dom_O184h(data.frame(edad = 100:200))
  testthat::expect_true(grepl("meses", h$evidencia, fixed = TRUE))
  h <- .dom_O184h(data.frame(edad = 0:87600))
  testthat::expect_true(grepl("horas", h$evidencia, fixed = TRUE))
  testthat::expect_false(grepl("multiplicada", h$evidencia, fixed = TRUE))
  # Una edad nunca esta "multiplicada por 1000".
  h <- .dom_O184h(data.frame(edad = sample(0:900000, 200L, TRUE)))
  testthat::expect_false(grepl("multiplicada", h$evidencia, fixed = TRUE))
})

testthat::test_that("una escala redondeada a pocos niveles sigue siendo una escala", {
  h <- .dom_O184h(data.frame(latitud = rep(c(400, 450, 500, 550, 600), 20L)))
  testthat::expect_identical(as.character(h$severidad), "ok")
  testthat::expect_true(grepl("multiplicada por 10", h$evidencia, fixed = TRUE))
  h <- .dom_O184h(data.frame(latitud = c(rep(400, 60L), rep(500, 40L))))
  testthat::expect_identical(as.character(h$severidad), "ok")
  # Con un centinela conocido, son codigos.
  set.seed(1964)
  h <- .dom_O184h(data.frame(
    pct = c(rep(500, 45L), rep(999, 45L), sample(0:100, 10L, TRUE))
  ))
  testthat::expect_true(any(grepl("500 y 999", h$evidencia, fixed = TRUE)))
})

testthat::test_that("el texto de los codigos nombra los otros valores de afuera", {
  set.seed(1965)
  h <- .dom_O184h(data.frame(
    pct = c(rep(500, 2L), rep(999, 9L), sample(0:100, 89L, TRUE))
  ))
  testthat::expect_true(grepl("Quedan afuera 2 valores m\u00e1s", h$evidencia,
                              fixed = TRUE))
})

testthat::test_that("el aviso en perfilar_por dice que el modo viene de la tabla", {
  set.seed(1966)
  t <- data.frame(
    g = rep(c("a", "b"), c(10L, 90L)),
    edad = c(c(sample(0:90, 7L, TRUE), rep(150, 3L)),
             c(sample(0:90, 70L, TRUE), rep(150, 20L)))
  )
  por <- suppressWarnings(perfilar_por(
    t, por = "g", min_filas = 1L, fecha = .F_O184h,
    analizar_dependencias = FALSE
  ))
  d <- por[por$tipo_hallazgo == "valor_fuera_dominio" & por$grupo == "a", ,
           drop = FALSE]
  testthat::expect_true(grepl("decidida con la tabla entera", d$evidencia,
                              fixed = TRUE))
  testthat::expect_true(grepl("en este grupo, 3 de 10", d$evidencia,
                              fixed = TRUE))
})

testthat::test_that("la pareja tiene que ser una coordenada sin calificativo", {
  set.seed(1967)
  for (par in list(c("lat", "longitud_cable"), c("lat", "longitud_texto"),
                   c("lon", "latitud_rel"), c("coord_lat", "longitud_cable"))) {
    d <- data.frame(c(rep(95, 5L), runif(95L, -40, -30)),
                    c(rep(195, 5L), runif(95L, -60, -50)))
    names(d) <- par
    testthat::expect_equal(nrow(.dom_O184h(d)), 0L, info = paste(par, collapse = "+"))
  }
  # La pareja de verdad sigue funcionando.
  d <- data.frame(lat = c(rep(95, 5L), runif(95L, -40, -30)),
                  longitud = runif(100L, -60, -50))
  testthat::expect_equal(nrow(.dom_O184h(d)), 1L)
})

testthat::test_that("se reconocen mas nombres de edad de personas", {
  set.seed(1968)
  nombres <- c("edad_madre", "edad_padre", "edad_hijo", "edad_alumno",
               "edad_empleado", "employee_age", "student_age", "mother_age",
               "person_age", "child_age")
  for (nombre in nombres) {
    v <- setNames(data.frame(c(rep(200, 5L), sample(1:80, 95L, TRUE))), nombre)
    h <- .dom_O184h(v)
    testthat::expect_equal(h$n_afectados, 5, info = nombre)
  }
})

testthat::test_that("lo que no cabe en ninguna convencion se acusa en cualquier proporcion", {
  set.seed(1971)
  for (k in c(1L, 5L, 6L, 10L)) {
    h <- .long_O184h(c(seq(0, 179, length.out = 89L), 181:190, rep(400, k)))
    testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], k,
                           info = k)
    testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 10, info = k)
  }
  h <- .long_O184h(c(seq(0, 179, length.out = 13L), 181:185, 400, 400))
  testthat::expect_equal(h$n_afectados[h$severidad == "sospechoso"], 2)
  testthat::expect_equal(h$n_afectados[h$severidad == "ok"], 5)
})

testthat::test_that("el texto de un porcentaje ambiguo dice de que lado quedan los valores", {
  set.seed(1972)
  v <- c(sample(0:100, 18L, TRUE), -20, -5)
  h <- .dom_O184h(data.frame(porcentaje_del_costo = v))
  testthat::expect_true(grepl("2 valores quedan por debajo de 0 y 0 por encima",
                              h$evidencia, fixed = TRUE))
  testthat::expect_false(grepl("pasan de", paste(h$evidencia, h$descripcion),
                               fixed = TRUE))
  h <- .dom_O184h(data.frame(
    porcentaje_de_avance = c(sample(0:100, 19L, TRUE), 150)
  ))
  testthat::expect_false(grepl("minor\u00eda", h$evidencia, fixed = TRUE))
  testthat::expect_true(grepl("queda afuera", h$evidencia, fixed = TRUE))
})
