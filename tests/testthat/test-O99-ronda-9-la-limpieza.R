# Ronda 9 sobre el plan de limpieza: tres casos donde lo aplicado o lo registrado
# no era lo que el plan decia.

test_that("guiar la limpieza con un diccionario conserva la regla de aplicabilidad", {
  datos <- data.frame(grupo = c("a", "a", "b", "b", "a", "a"),
                      txt = c("ANA", "ana", "ANA", "Ana", "ana", "ANA"),
                      stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(
    datos, aplicabilidad = list(txt = ~ grupo == "a")
  ))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  i <- which(plan$estrategia == "convertir_segun_diccionario")
  expect_length(i, 1L)
  expect_false(is.null(plan$parametros[[i]]$aplicabilidad))
  # `guiar_limpieza()` imprime el menu de cada grupo; aca no hace falta verlo.
  invisible(utils::capture.output(guiado <- suppressWarnings(guiar_limpieza(
    plan, datos, selector = function(d) 1L,
    diccionarios = list(txt = c(ANA = "ana", Ana = "ana", ana = "ana"))
  ))))
  # Antes: el diccionario reemplazaba los parametros y la regla se perdia.
  expect_false(is.null(guiado$parametros[[i]]$aplicabilidad))
  expect_false(is.null(guiado$parametros[[i]]$diccionario))
  guiado$aplicar <- FALSE
  guiado$aplicar[i] <- TRUE
  guiado$decision_grupo <- "pendiente"
  resultado <- suppressWarnings(aplicar(guiado, datos))
  # Las filas del grupo b, fuera del universo, quedan como estaban.
  expect_identical(resultado$datos$txt[3:4], c("ANA", "Ana"))
  expect_identical(resultado$datos$txt[c(1, 6)], c("ana", "ana"))
})

test_that("las irreversibles se cuentan sobre el resultado restaurado", {
  datos <- data.frame(grupo = c("a", "a", "b"), txt = c("y", " z", " y"),
                      stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(
    datos, aplicabilidad = list(txt = ~ grupo == "a")
  ))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  plan$aplicar <- plan$estrategia == "recortar_espacios"
  resultado <- suppressWarnings(aplicar(plan, datos))
  registro <- resultado$registro[resultado$registro$estrategia == "recortar_espacios", ]
  # " y" quedo fuera del universo y se restauro: no colapsa con nadie. Antes: 1.
  expect_identical(resultado$datos$txt, c("y", "z", " y"))
  expect_equal(registro$n_cambiadas, 1)
  expect_equal(registro$n_no_reversibles, 0)

  # Control: el colapso DENTRO del universo sigue contando.
  adentro <- data.frame(grupo = c("a", "a", "b"), txt = c("y", " y", "q"),
                        stringsAsFactors = FALSE)
  perfil2 <- suppressWarnings(perfilar(
    adentro, aplicabilidad = list(txt = ~ grupo == "a")
  ))
  plan2 <- suppressWarnings(planificar_limpieza(perfil2, adentro))
  plan2$aplicar <- plan2$estrategia == "recortar_espacios"
  resultado2 <- suppressWarnings(aplicar(plan2, adentro))
  registro2 <- resultado2$registro[resultado2$registro$estrategia == "recortar_espacios", ]
  expect_equal(registro2$n_no_reversibles, 1)
})

test_that("eliminar filas ausentes funciona despues de otra eliminacion sin regla", {
  datos <- data.frame(nombre = c("Ana", "Ana", NA, "Luis"),
                      edad = c("1", "1", "2", "3"), stringsAsFactors = FALSE)
  plan <- suppressWarnings(planificar_limpieza(
    suppressWarnings(perfilar(datos)), datos
  ))
  plan$aplicar <- plan$estrategia %in%
    c("conservar_primera_duplicada", "eliminar_filas_ausentes")
  expect_equal(sum(plan$aplicar), 2L)
  resultado <- suppressWarnings(aplicar(plan, datos, permitir_eliminacion = TRUE))
  registro <- resultado$registro
  # Antes: la segunda fallaba culpando a una aplicabilidad que no existia.
  pedidas <- registro$estrategia %in%
    c("conservar_primera_duplicada", "eliminar_filas_ausentes")
  expect_equal(sum(pedidas), 2L)
  expect_true(all(registro$estado[pedidas] == "ejecutada"))
  expect_equal(nrow(resultado$datos), 2L)
  expect_false(anyNA(resultado$datos$nombre))
})
