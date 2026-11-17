# O12. Lo que la muestra fabrica no se publica como propiedad de la tabla, y
# una columna constante de fecha-hora no acusa al paquete.
#
#   * Las dependencias se buscan sobre una muestra comun de filas completas. La
#     prioridad por contenido y aparicion evita que las filas agrupadas en
#     corridas consecutivas hagan parecer una clave a un determinante que no lo
#     es: la dependencia se conserva para verificarla sobre la tabla completa.
#
#   * La traza del hallazgo `constante` comparaba el texto del vector contra el
#     de la moda; en fecha-hora la moda publicada lleva la zona y el vector no,
#     la traza salia vacia y el paquete avisaba "Es un problema de `lupa`" sobre
#     cualquier columna constante de fecha-hora.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.o12_agrupada <- function(n = 2000L) {
  datos <- data.frame(
    grupo = rep(seq_len(n / 2L), each = 2L),
    valor = paste0("v", rep(seq_len(n / 2L), each = 2L)),
    stringsAsFactors = FALSE
  )
  datos$valor[[n]] <- "rara"
  datos
}

test_that("la muestra de dependencias no fabrica una casi-clave por bloques", {
  datos <- .o12_agrupada()
  # La premisa: sobre la tabla completa la dependencia existe.
  completa <- detectar_dependencias(datos, muestra = Inf)
  expect_true(any(completa$determinante == "grupo" & completa$dependiente == "valor"))

  muestreada <- detectar_dependencias(datos, muestra = 1000)
  expect_true(attr(muestreada, "muestreado"))
  descartadas <- attr(muestreada, "columnas_descartadas")
  expect_false(any(
    descartadas$columna == "grupo" &
      descartadas$motivo == "casi_clave_solo_en_muestra"
  ))

  perfil <- perfilar(datos, muestra = 1000, analizar_dependencias = TRUE)
  relacion <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "relacion_funcional_columnas", ,
    drop = FALSE
  ]
  expect_true(any(grepl("grupo -> valor", relacion$descripcion, fixed = TRUE)))
  expect_false(any(
    perfil$cobertura_diagnosticos$diagnostico == "dependencias_funcionales"
  ))
})

test_that("una clave de verdad sigue descartada como clave, sin aviso", {
  datos <- data.frame(id = seq_len(2000L), z = rep(letters[1:4], 500L))
  resultado <- detectar_dependencias(datos, muestra = 1000)
  descartadas <- attr(resultado, "columnas_descartadas")
  expect_identical(descartadas$motivo[descartadas$columna == "id"], "casi_clave")
  perfil <- perfilar(datos, muestra = 1000, analizar_dependencias = TRUE)
  cobertura <- perfil$cobertura_diagnosticos
  expect_false(any(
    cobertura$diagnostico == "dependencias_funcionales" &
      grepl("solo_en_muestra", cobertura$motivo, fixed = TRUE)
  ))
})

test_that("una columna constante de fecha-hora tiene traza y no acusa al paquete", {
  for (zona in c("UTC", "America/Montevideo")) {
    fechas <- as.POSIXct(rep("2024-01-01 10:00", 3L), tz = zona)
    h <- expect_no_warning(hallazgos(perfilar(data.frame(f = fechas))))
    k <- which(h$tipo_hallazgo == "constante")
    expect_length(k, 1L)
    traza <- h$trazabilidad[[k]]
    expect_identical(traza$estado, "disponible", info = zona)
    expect_identical(traza$indices_fila, 1:3, info = zona)
    expect_equal(traza$total, h$n_afectados[[k]], info = zona)
  }
})
