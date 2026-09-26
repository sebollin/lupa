# `detectar_deriva_calidad()` existe para no confundir un cambio de datos con un
# cambio de modelo -lo dice el comentario que encabeza `.texto_configuracion_calidad()`-
# y hacia exactamente eso en el caso mas comun: seguir el mismo modelo en el tiempo.
#
# `vigencia()` trae `fecha_acceso = Sys.time()` por omision, asi que dos corridas
# del mismo codigo llevaban dos configuraciones distintas. Medido, dos corridas
# separadas 1,2 segundos publicaban:
#
#   aspecto = configuracion_modelo, cambio = no_comparable, severidad = error
#
# sin que hubiera cambiado nada. El principio del arreglo: LO QUE QUIEN LLAMA NO
# DECLARO NO ES PARTE DEL MODELO.

.o65_contrato_sin_declarar <- function() {
  vigencia("actualizado", fecha_limite = as.Date("2026-01-31"))
}

.o65_texto <- function(contrato) {
  lupa:::.texto_configuracion_calidad(list(vigencia = contrato))
}

test_that("dos contratos sin declarar fecha_acceso comparan iguales", {
  primero <- .o65_contrato_sin_declarar()
  # El formato de la serializacion no lleva fracciones de segundo, asi que dos
  # llamadas dentro del mismo segundo darian el mismo texto TAMBIEN sin el
  # arreglo: la prueba pasaria sin haber probado nada. Se espera lo suficiente y
  # se comprueba que los dos momentos crudos DIFIEREN.
  Sys.sleep(1.1)
  segundo <- .o65_contrato_sin_declarar()
  expect_false(identical(primero$fecha_acceso, segundo$fecha_acceso))
  expect_gte(as.numeric(difftime(segundo$fecha_acceso, primero$fecha_acceso,
                                 units = "secs")), 1)

  expect_identical(.o65_texto(primero), .o65_texto(segundo))
  expect_true(grepl("fecha_acceso=<sin declarar>", .o65_texto(primero),
                    fixed = TRUE))
})

test_that("declarar fecha_acceso lo devuelve a la comparacion", {
  momento <- as.POSIXct("2026-02-15 10:00:00", tz = "UTC")
  uno <- vigencia("actualizado", fecha_limite = as.Date("2026-01-31"),
                  fecha_acceso = momento)
  otro <- vigencia("actualizado", fecha_limite = as.Date("2026-01-31"),
                   fecha_acceso = momento)
  distinto <- vigencia("actualizado", fecha_limite = as.Date("2026-01-31"),
                       fecha_acceso = momento + 86400)
  # El mismo momento declarado compara igual...
  expect_identical(.o65_texto(uno), .o65_texto(otro))
  # ...y OTRO momento declarado SI es un cambio de modelo. Sin esta mitad, una
  # version que ignorara `fecha_acceso` siempre pasaria la prueba de arriba.
  expect_false(identical(.o65_texto(uno), .o65_texto(distinto)))
  expect_true(grepl("fecha_acceso=POSIXt:", .o65_texto(uno), fixed = TRUE))
})

test_that("el contrato registra los campos que no le declararon", {
  sin_nada <- vigencia("actualizado")
  campos <- attr(sin_nada, "campos_sin_declarar", exact = TRUE)
  expect_true(all(c("fecha_acceso", "fecha_ultimo_cambio", "fecha_limite",
                    "inicio_intervalo", "fin_intervalo",
                    "frecuencia_cambio") %in% campos))
  # `frecuencia_cambio` se guarda convertido a segundos y con otro nombre: el
  # conjunto tiene que traer el nombre GUARDADO o la serializacion no lo encuentra.
  expect_true("frecuencia_cambio_segundos" %in% campos)
  # Y `columna_actualizacion`, que si vino declarada, NO esta.
  expect_false("columna_actualizacion" %in% campos)

  con_todo <- vigencia("actualizado", fecha_acceso = as.Date("2026-02-01"),
                       fecha_ultimo_cambio = as.Date("2026-01-01"),
                       fecha_limite = as.Date("2026-01-31"),
                       inicio_intervalo = as.Date("2026-01-01"),
                       fin_intervalo = as.Date("2026-03-01"),
                       frecuencia_cambio = 30)
  expect_equal(attr(con_todo, "campos_sin_declarar", exact = TRUE), character(0))
})

test_that("la deriva no acusa cambio de modelo entre dos corridas iguales", {
  datos <- data.frame(actualizado = as.Date(c("2026-01-10", "2026-01-31")),
                      v = c(1, NA))
  perfil <- perfil_evaluacion("P", regla_evaluacion("R", function(x) x > 0.5))
  una_corrida <- function(id) {
    contrato <- .o65_contrato_sin_declarar()
    modelo_calidad <- modelo(list(
      instanciar(especializar(metricas_nucleo()$OportunidadEntPorFecha,
                              vigencia = contrato), "t"),
      instanciar(especializar(metricas_nucleo()$NoNulo), "t", "v")
    ))
    medicion <- medir(modelo_calidad, list(t = datos), id_medicion = id)
    historico_calidad(medicion, evaluacion = evaluar(medicion, perfil))
  }
  primera <- una_corrida("a")
  Sys.sleep(1.1)
  segunda <- una_corrida("b")
  deriva <- as.data.frame(
    detectar_deriva_calidad(acumular_historico(primera, segunda))
  )
  expect_equal(sum(deriva$aspecto %in% "configuracion_modelo", na.rm = TRUE), 0L)
  # Y que la deriva haya medido algo: una tabla vacia tambien daria cero.
  expect_gt(nrow(deriva), 0L)
  expect_true(any(deriva$aspecto %in% "resultado"))
})
