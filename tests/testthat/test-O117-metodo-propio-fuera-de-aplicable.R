# Un metodo propio que mide filas fuera del universo que su metrica declara con
# `aplicable`. Residuo de la ronda 9: la medicion publicaba esas medidas, las
# metia en el agregado y `alcance_medidas` decia "4 de 5" cuando adentro se
# habian medido 3.

.datos_O117 <- function() {
  data.frame(
    cod = c("A1", "A2", "A3", NA, NA, "B1", NA, NA, "C9", "D1"),
    tiene_auto = c("Si", "Si", "Si", "No", "Si", "No", "Si", "No", "Si", "No"),
    clase = c("k", "k", "j", "j", "k", "j", "k", "j", "k", "k"),
    stringsAsFactors = FALSE
  )
}

.modelo_O117 <- function(respeta) {
  metodo <- function(tablas, instancia) {
    entidad <- instancia$entidad[[1L]]
    tabla <- tablas[[entidad]]
    atributo <- instancia$atributos[[1L]]
    x <- tabla[[atributo]]
    filas <- which(!is.na(x) & (!respeta | tabla$clase == "k"))
    data.frame(
      resultado = grepl("^[A-Z][0-9]$", x[filas]),
      entidad = rep(entidad, length(filas)),
      atributo = rep(atributo, length(filas)),
      fila = filas,
      objeto = paste0(entidad, "$", atributo, "[", filas, "]"),
      stringsAsFactors = FALSE
    )
  }
  mi <- metrica(
    "MiFormatoO117", "omite NA", "instanciaAtributo", "booleano",
    propiedades = "aplicable", metodo = metodo, orientacion = "conformidad",
    dimension = "Completitud", factor = "Densidad"
  )
  modelo(instanciar(
    especializar(mi, "MIF", aplicable = ~ clase == "k"), "t", "cod",
    nombre_instancia = "mif"
  ))
}

test_that("un metodo propio que mide fuera de su universo aplicable no se publica", {
  datos <- .datos_O117()
  expect_warning(
    medicion <- medir(.modelo_O117(respeta = FALSE), datos,
                      aplicabilidad = list(cod = ~ tiene_auto == "Si")),
    "aplicable"
  )
  # Antes: cuatro medidas -la fila 3 es de clase "j"- y "4 de 5".
  expect_identical(nrow(medicion), 0L)
  expect_null(attr(medicion, "alcance_medidas", exact = TRUE))
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_identical(cobertura$estado, "no_medible")
  # La fila se cita en la tabla del usuario.
  expect_true(grepl("`fila` 3-", cobertura$motivo, fixed = TRUE))

  # Sin `aplicabilidad`, la misma regla.
  expect_warning(
    medicion <- medir(.modelo_O117(respeta = FALSE), datos),
    "aplicable"
  )
  expect_identical(nrow(medicion), 0L)
  expect_true(grepl("`fila` 3, 6-",
                    attr(medicion, "cobertura_metricas")$motivo, fixed = TRUE))
})

test_that("un metodo propio que respeta su universo publica la cuenta correcta", {
  medicion <- medir(.modelo_O117(respeta = TRUE), .datos_O117(),
                    aplicabilidad = list(cod = ~ tiene_auto == "Si"))
  expect_identical(as.integer(medicion$fila), c(1L, 2L, 9L))
  alcance <- attr(medicion, "alcance_medidas", exact = TRUE)
  expect_identical(alcance$medidas, 3)
  expect_identical(alcance$en_el_universo, 5)
})

test_that("un metodo que no devuelve medidas sobre un universo con valores es no_medible", {
  vacio <- function(tablas, instancia) {
    data.frame(resultado = logical(), entidad = character(),
               atributo = character(), fila = integer(), objeto = character())
  }
  m <- metrica("VaciaO117", "no mide", "instanciaAtributo", "booleano",
               metodo = vacio, orientacion = "conformidad",
               dimension = "Completitud", factor = "Densidad")
  instancia <- instanciar(especializar(m, "V"), "t", "x")
  # Antes: `sin_valores` y "aportar valores no nulos", con tres valores en `x`.
  expect_warning(medicion <- medir(modelo(instancia), data.frame(x = 1:3)),
                 "no devolvi")
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_identical(cobertura$estado, "no_medible")
  expect_false(grepl("Aportar valores", cobertura$como_resolverlo, fixed = TRUE))
  # Control: sin valores de verdad sigue siendo `sin_valores`, y sin aviso.
  expect_silent(medicion <- medir(modelo(instancia),
                                  data.frame(x = c(NA_integer_, NA_integer_))))
  expect_identical(attr(medicion, "cobertura_metricas")$estado, "sin_valores")
})
