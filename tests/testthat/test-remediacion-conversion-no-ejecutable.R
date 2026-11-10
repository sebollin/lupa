# Una conversion que el plan no puede ejecutar dice cuantos valores no se leen
# y una frase que corresponde a su estado; y una accion sobre una columna que no
# esta en el perfil o en los datos se bloquea diciendo cual falta.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

fecha_fija <- as.POSIXct("2026-01-01", tz = "UTC")

plan_de <- function(d, datos = d) {
  perfil <- suppressWarnings(perfilar(d, fecha = fecha_fija))
  planificar_limpieza(perfil, datos)
}

conversion <- function(plan, estrategia = "convertir_numero_regional") {
  k <- which(plan$estrategia == estrategia)
  expect_length(k, 1L)
  list(estado = as.character(plan$estado[[k]]),
       justificacion = plan$justificacion[[k]],
       parametros = plan$parametros[[k]])
}

test_that("una conversion bloqueada por un telefono entre importes cuenta lo que no lee", {
  d <- data.frame(v = c(rep("1.234,56", 9), "099.123.456"), stringsAsFactors = FALSE)
  accion <- conversion(plan_de(d))
  expect_equal(accion$estado, "bloqueada")
  expect_equal(accion$parametros$n_no_reversibles, 1L)
  expect_match(accion$parametros$motivo_no_reversible,
               "1 de 10 valores presentes no se lee.", fixed = TRUE)
  expect_match(accion$parametros$motivo_no_reversible, "queda bloqueada", fixed = TRUE)
  expect_false(grepl("Se conserva como", accion$parametros$motivo_no_reversible,
                     fixed = TRUE))
  expect_false(grepl("Se conserva como", accion$justificacion, fixed = TRUE))
})

test_that("lo mismo con un valor que no responde al formato y con una columna raw", {
  d <- data.frame(v = c("1234,56", "0x1A", "12,5", "7,25", "3,1"),
                  stringsAsFactors = FALSE)
  accion <- conversion(plan_de(d))
  expect_equal(accion$estado, "bloqueada")
  expect_equal(accion$parametros$n_no_reversibles, 1L)
  expect_match(accion$parametros$motivo_no_reversible,
               "1 de 5 valores presentes no se lee.", fixed = TRUE)
  texto <- data.frame(v = c("1.234,56", "2.345,67", "3.456,78", "4.567,89", "5.678,90"),
                      stringsAsFactors = FALSE)
  crudo <- texto
  crudo$v <- as.raw(1:5)
  accion <- conversion(plan_de(texto, crudo))
  expect_equal(accion$estado, "bloqueada")
  expect_equal(accion$parametros$n_no_reversibles, 5L)
  expect_match(accion$parametros$motivo_no_reversible,
               "5 de 5 valores presentes no se leen.", fixed = TRUE)
})

test_that("una conversion ambigua queda lista y se ejecuta una vez configurada", {
  d <- data.frame(v = sprintf("%d.%03d", 1:40, (1:40) * 7L %% 1000L),
                  otra = c(" a", rep("b", 39)), stringsAsFactors = FALSE)
  plan <- plan_de(d)
  accion <- conversion(plan)
  expect_equal(accion$estado, "lista")
  expect_equal(accion$parametros$n_no_reversibles, 0L)
  expect_match(accion$parametros$motivo_no_reversible,
               "queda lista sin activar", fixed = TRUE)
  expect_false(grepl("destructiva", accion$parametros$motivo_no_reversible,
                     fixed = TRUE))
  # El contrato: una accion `lista` que falla se registra con su error y no
  # impide aplicar las siguientes.
  k <- which(plan$estrategia == "convertir_numero_regional")
  activado <- plan
  activado$aplicar[as.character(activado$estado) == "lista"] <- TRUE
  r <- aplicar(activado, d)
  fila <- r$registro[r$registro$estrategia == "convertir_numero_regional", ]
  expect_equal(fila$estado, "fallida")
  expect_match(fila$error, "configure `punto_sin_coma`", fixed = TRUE)
  expect_true(any(r$registro$estado == "ejecutada"))
  expect_type(r$datos$v, "character")
  # Configurada, se ejecuta.
  activado$parametros[[k]]$punto_sin_coma <- "miles"
  r <- aplicar(activado, d)
  fila <- r$registro[r$registro$estrategia == "convertir_numero_regional", ]
  expect_equal(fila$estado, "ejecutada")
  expect_equal(r$datos$v[1:3], c(1007, 2014, 3021))
})

test_that("un grupo de columnas iguales con un miembro fuera de los datos se bloquea y dice cual", {
  d1 <- data.frame(id = 1:6, A = c(1, 2, 3, 4, 5, 6), B = c(1, 2, 3, 4, 5, 6),
                   C = c(1, 2, 3, 4, 5, 6))
  perfil <- perfilar(d1, fecha = fecha_fija)
  completo <- planificar_limpieza(perfil, d1)
  duplicadas <- grepl("columna", completo$estrategia) &
    completo$hallazgo == "columnas_duplicadas"
  expect_true(all(as.character(completo$estado[duplicadas]) == "lista"))
  # Un perfil de otra version de los datos: `C` se renombro.
  d2 <- d1
  names(d2)[names(d2) == "C"] <- "C2"
  plan <- planificar_limpieza(perfil, d2)
  duplicadas <- plan$hallazgo == "columnas_duplicadas"
  expect_equal(sum(duplicadas), 2L)
  expect_true(all(as.character(plan$estado[duplicadas]) == "bloqueada"))
  expect_false(any(plan$aplicar[duplicadas]))
  expect_true(all(grepl("La columna `C` no est", plan$justificacion[duplicadas],
                        fixed = TRUE)))
  expect_true(all(grepl("en los datos", plan$justificacion[duplicadas], fixed = TRUE)))
  r <- aplicar(plan, d2)
  expect_false(any(r$registro$estado == "fallida"))
  # Y un perfil cuyas columnas no incluyen a un miembro del hallazgo.
  recortado <- perfil
  recortado$columnas <- recortado$columnas[recortado$columnas$columna != "C", , drop = FALSE]
  plan <- planificar_limpieza(recortado)
  duplicadas <- plan$hallazgo == "columnas_duplicadas"
  expect_true(all(as.character(plan$estado[duplicadas]) == "bloqueada"))
  expect_true(all(grepl("en el perfil", plan$justificacion[duplicadas], fixed = TRUE)))
})

test_that("una accion de una columna sobre una columna que falta en los datos se bloquea igual", {
  d1 <- data.frame(id = 1:6, x = c(" a", "b ", "c", "d", "e", "f"),
                   z = c("u", "v", "w", "x", "y", "z"), stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(d1, fecha = fecha_fija))
  completo <- planificar_limpieza(perfil, d1)
  sobre_x <- as.character(completo$columna) %in% "x"
  expect_true(any(as.character(completo$estado[sobre_x]) == "lista"))
  # Un perfil de otra version de los datos: `x` se renombro.
  d2 <- d1
  names(d2)[names(d2) == "x"] <- "x2"
  plan <- planificar_limpieza(perfil, d2)
  sobre_x <- as.character(plan$columna) %in% "x"
  expect_true(any(sobre_x))
  ejecutables <- sobre_x & as.character(plan$estado) != "informativa"
  expect_true(all(as.character(plan$estado[ejecutables]) == "bloqueada"))
  expect_false(any(plan$aplicar[sobre_x]))
  expect_true(all(grepl("La columna `x` no est", plan$justificacion[ejecutables],
                        fixed = TRUE)))
  r <- aplicar(plan, d2)
  expect_false(any(r$registro$estado == "fallida"))
  # Las acciones sobre las columnas que si estan no cambian.
  otras <- !as.character(completo$columna) %in% "x"
  expect_identical(as.character(plan$estado[otras]), as.character(completo$estado[otras]))
})
