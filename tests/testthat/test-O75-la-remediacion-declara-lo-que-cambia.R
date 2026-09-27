# Tres cosas que la capa de remediacion hacia y no declaraba.
#
# Las promesas centrales de la capa aguantaron una ronda de refutacion completa
# -planificar no toca los datos, aplicar obedece el plan recibido, lo que no se
# puede aplicar se declara, `permitir_eliminacion = FALSE` no elimina nada-. Lo que
# fallaba estaba en tres fronteras que el paquete no habia escrito.

test_that("winsorizar una columna entera declara que deja de ser entera", {
  # Los limites de Tukey son cuartiles: sobre una columna entera la sustitucion
  # devuelve dobles. La fila salia `destructiva = FALSE`, la justificacion hablaba
  # solo de valores y ninguno de los campos del registro nombraba el tipo, mientras
  # la MISMA accion sobre `Date` sale `bloqueada` -o sea que el plan si mira el tipo
  # cuando le importa-.
  datos <- data.frame(x = c(1:30, 500L))
  plan <- planificar_limpieza(perfilar(datos), datos)
  fila <- which(plan$estrategia == "winsorizar_outliers")
  expect_length(fila, 1L)

  expect_true(plan$destructiva[[fila]])
  expect_match(plan$justificacion[[fila]], "deja de ser entera")
  expect_identical(plan$parametros[[fila]]$tipo_original, "entero")
  expect_identical(plan$parametros[[fila]]$tipo_resultante, "doble")

  plan$aplicar <- FALSE
  plan$aplicar[[fila]] <- TRUE
  resultado <- aplicar(plan, datos)
  expect_true(resultado$registro$destructiva[[1L]])
  expect_identical(
    resultado$registro$parametros[[1L]]$tipo_resultante, "doble"
  )
  # Y la columna efectivamente cambia de tipo: sin esto la declaracion seria falsa
  # en la otra direccion.
  expect_identical(class(datos$x), "integer")
  expect_identical(class(resultado$datos$x), "numeric")
})

test_that("winsorizar una columna doble no declara ningun cambio de tipo", {
  # Mitad de control. La declaracion tiene que aparecer SOLO donde la conversion
  # ocurre: si se pusiera siempre, no diria nada.
  datos <- data.frame(x = c(as.numeric(1:30), 500.5))
  plan <- planificar_limpieza(perfilar(datos), datos)
  fila <- which(plan$estrategia == "winsorizar_outliers")

  expect_false(plan$destructiva[[fila]])
  expect_false(grepl("deja de ser entera", plan$justificacion[[fila]], fixed = TRUE))
  expect_null(plan$parametros[[fila]]$tipo_resultante)
})

test_that("guiar_limpieza devuelve el plan editado sin cambios", {
  # La documentacion lo promete dos veces y el ejemplo del roxygen lo ilustra con
  # `identical()`. Con un plan EDITADO -la edicion que la propia capa invita a
  # hacer- se reescribia `decision_grupo` de `recomendada` a `desactivada`, que en
  # la taxonomia del paquete es otro estado: afirma que quien llama desactivo la
  # recomendacion cuando solo la desmarco, y esa afirmacion viaja con el plan.
  datos <- data.frame(a = c(1, 1), b = c("x", "x"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)
  plan$aplicar[plan$estrategia == "marcar_filas_duplicadas"] <- FALSE

  guiado <- guiar_limpieza(plan, datos)

  expect_identical(plan, guiado)
  expect_identical(
    as.character(guiado$decision_grupo), as.character(plan$decision_grupo)
  )
})

test_that("guiar_limpieza sigue devolviendo intacto un plan sin editar", {
  # El control del caso que ya pasaba, que es el que el ejemplo del roxygen corre.
  datos <- data.frame(zona = c("Norte", "NORTE", "sur"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)

  expect_identical(plan, guiar_limpieza(plan, datos))
})

test_that("una accion de texto declara cuantas celdas cambiaron de codificacion", {
  # Una accion de texto sobre celdas `latin1` devuelve UTF-8 y solo en las celdas
  # que toco: la columna queda con marcas mixtas y el registro no lo decia. El
  # valor semantico se conserva -el paquete trata `latin1` como convertible-, pero
  # los bytes cambian y eso se declara.
  #
  # La cadena se construye con `rawToChar()`: un literal acentuado en un archivo de
  # prueba depende de la codificacion con que se lea el archivo.
  crudos <- list(
    c(0x63, 0x61, 0x66, 0xE9, 0x20),
    c(0x61, 0xF1, 0x6F),
    c(0x6E, 0x69, 0xF1, 0x6F, 0x20)
  )
  texto <- vapply(crudos, function(b) rawToChar(as.raw(b)), character(1L))
  Encoding(texto) <- "latin1"
  datos <- data.frame(texto = texto, stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)
  plan$aplicar <- plan$recomendada

  resultado <- aplicar(plan, datos)
  fila <- resultado$registro[
    resultado$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]

  expect_identical(nrow(fila), 1L)
  expect_identical(as.numeric(fila$n_codificacion_normalizada), 2)
  # Y la mezcla de marcas que el campo declara existe de verdad.
  expect_identical(
    Encoding(resultado$datos$texto), c("UTF-8", "latin1", "UTF-8")
  )
})

test_that("una accion de texto sobre UTF-8 declara cero recodificaciones", {
  # Mitad de control: el campo nuevo no puede contar de mas donde no hubo cambio
  # de codificacion. Cambia lo mismo -dos celdas- y el campo dice cero.
  datos <- data.frame(
    texto = c("hola ", "chau", "adios "), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos), datos)
  plan$aplicar <- plan$recomendada

  resultado <- aplicar(plan, datos)
  fila <- resultado$registro[
    resultado$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]

  expect_identical(as.numeric(fila$n_cambiadas), 2)
  expect_identical(as.numeric(fila$n_codificacion_normalizada), 0)
})

test_that("el registro conserva su forma con y sin acciones aplicadas", {
  # El campo nuevo tambien esta en el registro VACIO: sin eso la tabla tendria una
  # columna que aparece y desaparece segun si hubo algo que aplicar.
  datos <- data.frame(
    texto = c("hola ", "chau", "adios "), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos), datos)
  con_acciones <- aplicar(plan, datos)$registro
  plan$aplicar <- FALSE
  sin_acciones <- aplicar(plan, datos)$registro

  expect_identical(names(sin_acciones), names(con_acciones))
  expect_true("n_codificacion_normalizada" %in% names(sin_acciones))
})
