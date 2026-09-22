# O12. Lo que la muestra fabrica no se publica como propiedad de la tabla, y
# una columna constante de fecha-hora no acusa al paquete.
#
#   * Las dependencias se buscan sobre una muestra sistematica, y los descartes
#     por casi-clave se decidian sobre ESA muestra. Con los datos agrupados en
#     filas consecutivas -`1,1,2,2,...`- la muestra toma una fila por grupo, el
#     determinante parece una clave y se descarta con el motivo "casi_clave",
#     que en la tabla es falso. La tabla de dependencias salia vacia y se leia
#     como "no hay dependencias", cuando con `muestra = Inf` la dependencia
#     aparecia con su violacion.
#
#   * La traza del hallazgo `constante` comparaba el texto del vector contra el
#     de la moda; en fecha-hora la moda publicada lleva la zona y el vector no,
#     la traza salia vacia y el paquete avisaba "Es un problema de `lupa`" sobre
#     cualquier columna constante de fecha-hora.

.o12_agrupada <- function(n = 2000L) {
  datos <- data.frame(
    grupo = rep(seq_len(n / 2L), each = 2L),
    valor = paste0("v", rep(seq_len(n / 2L), each = 2L)),
    stringsAsFactors = FALSE
  )
  datos$valor[[n]] <- "rara"
  datos
}

test_that("un descarte que solo produce la muestra se declara como tal", {
  datos <- .o12_agrupada()
  # La premisa: sobre la tabla completa la dependencia existe.
  completa <- detectar_dependencias(datos, muestra = Inf)
  expect_true(any(completa$determinante == "grupo" & completa$dependiente == "valor"))

  muestreada <- detectar_dependencias(datos, muestra = 1000)
  expect_true(attr(muestreada, "muestreado"))
  descartadas <- attr(muestreada, "columnas_descartadas")
  expect_identical(
    descartadas$motivo[descartadas$columna == "grupo"],
    "casi_clave_solo_en_muestra"
  )

  perfil <- perfilar(datos, muestra = 1000, analizar_dependencias = TRUE)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "dependencias_funcionales", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "grupo (casi_clave_solo_en_muestra)", fixed = TRUE)
  expect_match(fila$motivo, "no significa que no las haya", fixed = TRUE)
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
