# Una celda que el motor repara solo en parte no apaga la reparacion de las
# celdas de la misma columna que se reparan enteras. Bastaba una parcial para
# que la accion saliera sin marcar, y el plan por omision no reparaba ninguna.

testthat::skip_on_cran()

# El signo de grado pegado delante impide reparar la primera letra rota y la
# segunda se repara: una reparacion parcial verdadera, sin el caracter de
# reemplazo. Si el motor aprende a repararla entera, hay que buscar otra.
.parcial_mixta <- paste0(
  "a", intToUtf8(0xB0L), intToUtf8(c(0xC3L, 0xA9L)),
  "l", intToUtf8(c(0xC3L, 0xA8L)), "ve"
)
# Lo que la reparacion parcial deja: la segunda letra arreglada, la primera no.
.parcial_mixta_reparada <- paste0(
  "a", intToUtf8(0xB0L), intToUtf8(c(0xC3L, 0xA9L)),
  "l", intToUtf8(0xE8L), "ve"
)
# Tres valores que se reparan enteros, leidos una vez como latin-1.
.enteras_mixta <- c(
  paste0("Paysand", intToUtf8(c(0xC3L, 0xBAL))),
  paste0("Se", intToUtf8(c(0xC3L, 0xB1L)), "or"),
  paste0("Jos", intToUtf8(c(0xC3L, 0xA9L)))
)
.enteras_mixta_reparadas <- c(
  paste0("Paysand", intToUtf8(0xFAL)),
  paste0("Se", intToUtf8(0xF1L), "or"),
  paste0("Jos", intToUtf8(0xE9L))
)

test_that("una parcial no apaga la reparacion de las enteras de su columna", {
  datos <- data.frame(
    t = c(.parcial_mixta, .enteras_mixta, "ok"), stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  fila <- perfil$columnas[perfil$columnas$columna == "t", , drop = FALSE]
  # La columna es mixta: una parcial y tres enteras.
  expect_identical(as.integer(fila$n_codificacion_reparable_parcialmente), 1L)
  expect_identical(as.integer(fila$n_codificacion_reparable), 3L)

  plan <- planificar_limpieza(perfil, datos)
  i <- which(plan$estrategia == "reparar_codificacion")
  expect_length(i, 1L)
  # Sale como en una columna sin parciales: recomendada y marcada.
  expect_true(plan$recomendada[[i]])
  expect_identical(as.character(plan$estado[[i]]), "lista")
  expect_true(plan$aplicar[[i]])
  expect_false(isTRUE(plan$parametros[[i]]$aplicar_parciales))
  # El plan cuenta lo que la accion, asi marcada, va a cambiar.
  expect_identical(as.numeric(plan$n_afectadas[[i]]), 3)
  expect_identical(as.character(plan$estado_reparacion[[i]]), "reparado")
  # Y dice cuantas parciales quedan y como pedirlas.
  expect_true(grepl("parciales (1)", plan$justificacion[[i]],
                    fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("`aplicar_parciales = TRUE`", plan$justificacion[[i]],
                    fixed = TRUE, useBytes = TRUE))

  # El plan por omision repara las enteras y deja la parcial como estaba.
  r <- suppressWarnings(aplicar(plan, datos))
  registro <- r$registro[r$registro$estrategia == "reparar_codificacion", ,
                         drop = FALSE]
  expect_identical(registro$estado, "ejecutada")
  expect_identical(as.numeric(registro$n_cambiadas),
                   as.numeric(plan$n_afectadas[[i]]))
  expect_identical(as.character(registro$estado_reparacion), "reparado")
  expect_identical(
    r$datos$t, c(.parcial_mixta, .enteras_mixta_reparadas, "ok")
  )

  # Pedidas las parciales, se repara tambien la parcial.
  plan_parciales <- plan
  plan_parciales$parametros[[i]]$aplicar_parciales <- TRUE
  r_parciales <- suppressWarnings(aplicar(plan_parciales, datos))
  registro <- r_parciales$registro[
    r_parciales$registro$estrategia == "reparar_codificacion", , drop = FALSE
  ]
  expect_identical(as.numeric(registro$n_cambiadas), 4)
  expect_identical(as.character(registro$estado_reparacion),
                   "reparado_parcialmente")
  expect_identical(
    r_parciales$datos$t,
    c(.parcial_mixta_reparada, .enteras_mixta_reparadas, "ok")
  )
})

test_that("una columna solo con parciales sigue pidiendo la seleccion explicita", {
  datos <- data.frame(t = c(.parcial_mixta, "ok"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  i <- which(plan$estrategia == "reparar_codificacion")
  expect_length(i, 1L)
  expect_false(plan$recomendada[[i]])
  expect_identical(as.character(plan$estado[[i]]), "lista")
  expect_false(plan$aplicar[[i]])
  expect_true(isTRUE(plan$parametros[[i]]$aplicar_parciales))
  expect_identical(as.numeric(plan$n_afectadas[[i]]), 1)
  expect_identical(as.character(plan$estado_reparacion[[i]]),
                   "reparado_parcialmente")

  # Por omision no se toca.
  r <- suppressWarnings(aplicar(plan, datos))
  expect_identical(r$datos$t, datos$t)
  # Marcada, aplica la parcial.
  plan$aplicar[] <- FALSE
  plan$aplicar[[i]] <- TRUE
  r <- suppressWarnings(aplicar(plan, datos))
  expect_identical(r$datos$t, c(.parcial_mixta_reparada, "ok"))
})
