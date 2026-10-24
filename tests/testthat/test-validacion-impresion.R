# Cada puerta comprobaba un subconjunto distinto de lo que un objeto necesita, y
# ninguna estaba completa. Medido quitando un componente por vez:
# `comparar_perfiles()` rechaza un perfil sin `columnas`, `meta` o `hallazgos` y
# ACEPTA uno sin `general`; `print.perfil()` no miraba nada y sin embargo
# necesita `general`, asi que fallaba con "attempt to set an attribute on NULL".
test_that("un objeto danado falla con el mensaje del paquete", {
  datos <- data.frame(
    x = c(1, 2, NA, 4, 5), t = c("a", "b", "a", "c", NA),
    stringsAsFactors = FALSE
  )
  sin_general <- perfilar(datos)
  sin_general$general <- NULL
  expect_error(print(sin_general), "falta general")
  expect_error(print(sin_general), "perfilar\\(\\)")

  sin_campo <- normalizacion()
  sin_campo$acentos <- NULL
  expect_error(print(sin_campo), "normalizacion_lupa")

  # Llamar al metodo directamente con algo que no es su objeto: la clase se
  # nombra y se dice con que producirlo.
  expect_error(print.perfil(NULL), "clase `perfil`")
  expect_error(print.plan_limpieza(1:3), "clase `plan_limpieza`")
  expect_error(print.referencial(list()), "clase `referencial`")
  expect_error(print.normalizacion_lupa(NULL), "clase `normalizacion_lupa`")
})

# El control decide el alcance: las guardas no pueden estorbar a los objetos
# sanos, incluidos los degenerados que el paquete produce legitimamente.
test_that("los objetos sanos y los degenerados siguen imprimiendo", {
  datos <- data.frame(
    x = c(1, 2, NA, 4, 5), t = c("a", "b", "a", "c", NA),
    stringsAsFactors = FALSE
  )
  callado <- function(expr) {
    salida <- textConnection("basura", "w", local = TRUE)
    sink(salida)
    on.exit({ sink(); close(salida) }, add = TRUE)
    force(expr)
    invisible(TRUE)
  }
  expect_true(callado(print(perfilar(datos))))
  expect_true(callado(print(planificar_limpieza(perfilar(datos)))))
  expect_true(callado(print(normalizacion())))
  expect_true(callado(print(referencial(data.frame(k = c("a", "b")), "k"))))

  # Degenerados que el paquete produce y tiene que poder imprimir.
  expect_true(callado(print(perfilar(data.frame()))))
  expect_true(callado(print(perfilar(data.frame(x = numeric(0))))))
  expect_true(callado(print(perfilar(data.frame(x = c(NA, NA))))))
})
