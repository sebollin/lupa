# O24. Dos afirmaciones que el propio objeto desmiente.
#
#   * `comparar_perfiles()` y `comparar_evaluaciones()` publican en cada fila la
#     fecha de cada corrida, pero no las miraban para validar el orden: con los
#     argumentos cambiados, un hallazgo que se agravo sale `atenuado`, un patron
#     que aparecio sale `desaparecido` y un delta que subio sale negativo. La
#     direccion publicada contradice a las fechas de su propia fila.
#
#   * La guarda de coherencia entre un hallazgo y su trazabilidad corria al
#     construir el perfil y no al publicarlo: un objeto editado a mano
#     -`n_afectados <- 0` sobre un hallazgo cuya traza sigue nombrando dos
#     filas- salia por `hallazgos()` y por el informe sin que nada dijera que la
#     cifra y su respaldo no coinciden.

.o24_perfil <- function(valores, fecha) {
  perfilar(data.frame(v = valores, stringsAsFactors = FALSE),
           fecha = as.POSIXct(fecha, tz = "UTC"))
}

test_that("comparar_perfiles exige que anterior sea anterior", {
  enero <- .o24_perfil(c(1, NA, 3, 4), "2026-01-01")
  febrero <- .o24_perfil(c(1, NA, NA, 4), "2026-02-01")

  # El orden correcto compara como siempre.
  derecho <- comparar_perfiles(enero, febrero)
  expect_gt(nrow(derecho), 0L)

  # El invertido no publica la direccion al reves: para y dice como arreglarlo.
  expect_error(
    comparar_perfiles(febrero, enero),
    "Invierta los argumentos", fixed = TRUE
  )
  expect_error(comparar_perfiles(febrero, enero), "2026-02-01", fixed = TRUE)

  # Control: dos perfiles de la misma fecha se siguen comparando.
  otro_enero <- .o24_perfil(c(1, NA, 3, 4), "2026-01-01")
  expect_no_error(comparar_perfiles(enero, otro_enero))
})

test_that("comparar_evaluaciones exige el mismo orden", {
  skip_if_not(exists("evaluar"), "evaluar no disponible")
  perfil <- perfil_evaluacion("Op", regla_evaluacion("Pasa", function(x) x >= 0.78))
  metodo <- function(valor) function(tablas, instancia) data.frame(
    resultado = valor, entidad = instancia$entidad[[1L]],
    atributo = instancia$atributos[[1L]], fila = NA_integer_, objeto = "ok"
  )
  hacer <- function(valor, id, fecha) {
    m <- instanciar(especializar(metrica(
      "MideA", "a", "atributo", "real", dimension = "D", factor = "F",
      metodo = metodo(valor)
    )), "t", "x")
    evaluar(
      medir(modelo(m), data.frame(x = 1), id_medicion = id,
            fecha = as.POSIXct(fecha, tz = "UTC")),
      perfil
    )
  }
  enero <- hacer(0.75, "enero", "2026-01-31")
  febrero <- hacer(0.8, "febrero", "2026-02-28")

  derecho <- comparar_evaluaciones(enero, febrero)
  # La regla pasa con 0,8 y no con 0,75: la serie sube.
  expect_gt(derecho$delta[[1L]], 0)
  expect_error(
    comparar_evaluaciones(febrero, enero),
    "Invierta los argumentos", fixed = TRUE
  )
})

test_that("un hallazgo editado a mano no se publica en silencio", {
  datos <- data.frame(edad = c(30, NA, 40, NA, 50, 60, 70, 80, 90, 25))
  perfil <- perfilar(datos)
  fila <- which(perfil$hallazgos$tipo_hallazgo == "faltantes")
  expect_length(fila, 1L)

  # Control: el perfil intacto no avisa nada, ni al publicar ni al reportar.
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_no_warning(hallazgos(perfil))
  expect_no_warning(reportar(perfil, archivo = archivo, sobrescribir = TRUE))

  editado <- perfil
  editado$hallazgos$n_afectados[fila] <- 0
  expect_warning(hallazgos(editado), "trazabilidad inconsistente", fixed = TRUE)
  expect_warning(
    reportar(editado, archivo = archivo, sobrescribir = TRUE),
    "trazabilidad inconsistente", fixed = TRUE
  )
})

test_that("dos corridas de la misma sesion se siguen comparando", {
  # `fecha` por omision es la hora de LA CORRIDA, no la de la entrega: dos
  # perfiles hechos seguidos quedan a segundos uno del otro, en el orden en que
  # se corrieron, y ese orden no dice nada. La guarda de orden solo tiene
  # sentido cuando las dos fechas las declaro quien llama. Lo atrapo la suite:
  # la primera version paraba sobre pruebas que perfilan dos veces seguidas.
  primero <- perfilar(data.frame(v = c(1, 2, 3)))
  segundo <- perfilar(data.frame(v = c(1, NA, 3)))
  expect_false(isTRUE(primero$meta$fecha_declarada))
  expect_no_error(comparar_perfiles(segundo, primero))
  expect_no_error(comparar_perfiles(primero, segundo))

  # Y declarada de un solo lado tampoco alcanza para exigir el orden.
  declarado <- perfilar(data.frame(v = c(1, NA, 3)),
                        fecha = as.POSIXct("2020-01-01", tz = "UTC"))
  expect_no_error(comparar_perfiles(primero, declarado))
})
