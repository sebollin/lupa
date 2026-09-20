# Dos formas canonicas de romper la invariante, encontradas en la pasada final
# sobre la API publica entera: un `0` donde corresponde `NA`, y una fila de
# medida sobre una celda que nadie midio.

test_that("sin largo medido se publica NA y no cero", {
  # `largo_maximo = 0` es una medicion, y falsa. El manual lo dice textualmente:
  # con el tope en `Inf` no se mide ningun largo y `largo_maximo` vale `NA`, no
  # cero. La doctrina estaba escrita en el propio archivo, a ciento veinte lineas
  # del `else 0` que la contradecia.
  vacio <- data.frame(x = character(), stringsAsFactors = FALSE)
  con_filas <- data.frame(
    x = c("Juan Perez", "Juan Peres", "Ana Gomez"), stringsAsFactors = FALSE
  )
  alcance_de <- function(datos, tope) {
    as.data.frame(suppressWarnings(detectar_duplicados_aproximados(
      datos, columnas = "x", max_largo_valor = tope
    ))$alcance)
  }
  # Los tres casos donde NO se midio ningun largo. Valen con `stringdist` o sin
  # el: si el paquete opcional falta, la comparacion no corre y tampoco hay
  # largos, que es la misma respuesta por otro camino.
  expect_true(is.na(alcance_de(vacio, Inf)$largo_maximo[[1L]]))
  expect_true(is.na(alcance_de(vacio, 100)$largo_maximo[[1L]]))
  expect_true(is.na(alcance_de(con_filas, Inf)$largo_maximo[[1L]]))

  # Y el control -cuando SI se mide, se publica la medicion- necesita
  # `stringdist`, que es Suggests. Sin el, `detectar_duplicados_aproximados()`
  # declara `disponible = FALSE` y no mide largos: `largo_maximo` es `NA` con
  # razon. Esta mitad se salta, no se afloja: aflojarla convertiria el control en
  # una afirmacion que se cumple sola.
  skip_if_not_installed("stringdist")
  medido <- alcance_de(con_filas, 100)
  expect_true(isTRUE(medido$limite_largo_valor_aplica[[1L]]))
  expect_equal(medido$largo_maximo[[1L]], 10)

  # `estimar_costo()` publica el mismo alcance por el mismo camino.
  costo <- as.data.frame(suppressWarnings(estimar_costo(
    vacio, columnas = "x", max_largo_valor = Inf
  ))$alcance)
  expect_true(is.na(costo$largo_maximo[[1L]]))
})

test_that("una estimacion vacia no se publica como medida", {
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  desde <- function(estimaciones) {
    medicion_desde_estimaciones(
      estimaciones, entidad = "poblacion", atributo = "celda",
      fuente = "fuente externa", fecha = fecha
    )
  }

  # Una columna presente pero sin un solo valor utilizable no es una estimacion:
  # publicaba dos filas de medida con `resultado = NA` y dejaba la medicion
  # inservible, porque `evaluar()` la rechaza entera.
  expect_error(
    desde(data.frame(celda = "A", cv = NA_real_, n = NA_integer_)),
    "sin ningun valor utilizable"
  )
  # Y el mensaje distingue esta causa de la otra, porque la accion del usuario
  # es distinta: aca las columnas se reconocieron.
  expect_error(
    desde(data.frame(celda = "A", otra = 1)),
    "no trae ningun estadistico reconocido"
  )

  # La celda vacia dentro de una columna que si trae datos se descarta, y el
  # resto se publica: no se pierde la estimacion buena por culpa de la ausente.
  mixto <- desde(data.frame(celda = c("A", "B"), cv = c(0.1, NA)))
  filas <- as.data.frame(mixto)
  expect_equal(nrow(filas), 1L)
  expect_equal(filas$resultado[[1L]], 0.1)
  expect_false(anyNA(filas$resultado))

  # Y la medicion resultante la consume la capa siguiente, que es la prueba de
  # que sirve: antes `evaluar()` la rechazaba entera.
  evaluada <- evaluar(
    mixto, perfil_evaluacion("p", regla_evaluacion("r", function(v) v >= 0))
  )
  expect_s3_class(evaluada, "evaluacion_calidad")

  # Control: con todas las estimaciones presentes no se descarta ninguna.
  completo <- as.data.frame(desde(data.frame(celda = c("A", "B"), cv = c(0.1, 0.2))))
  expect_equal(nrow(completo), 2L)
  expect_equal(completo$resultado, c(0.1, 0.2))
})
