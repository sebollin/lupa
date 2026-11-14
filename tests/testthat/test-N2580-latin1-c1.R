testthat::skip_on_cran()

.n2580_latin1 <- function(codigo, sufijo = "") {
  texto <- paste0(" borde ", intToUtf8(codigo), sufijo, " ")
  salida <- iconv(texto, from = "UTF-8", to = "latin1")
  Encoding(salida) <- "latin1"
  salida
}

.n2580_codigos <- function() c(seq.int(0x80L, 0xffL))

test_that("recortar_espacios trata igual latin1 y UTF-8 del mismo texto", {
  codigos <- .n2580_codigos()
  latin1 <- vapply(codigos, .n2580_latin1, character(1L))
  utf8 <- vapply(latin1, lupa:::.latin1_a_utf8, character(1L))
  datos_latin1 <- data.frame(
    texto = latin1,
    stringsAsFactors = FALSE
  )
  datos_utf8 <- data.frame(
    texto = utf8,
    stringsAsFactors = FALSE
  )
  ejecutar <- function(datos) {
    plan <- planificar_limpieza(
      perfilar(datos, fecha = "2026-01-15"), datos
    )
    plan$aplicar <- as.character(plan$estrategia) == "recortar_espacios"
    list(plan = plan, resultado = aplicar(plan, datos))
  }
  latin1_ejecutado <- ejecutar(datos_latin1)
  utf8_ejecutado <- ejecutar(datos_utf8)
  plan_latin1 <- latin1_ejecutado$plan
  plan_utf8 <- utf8_ejecutado$plan
  resultado_latin1 <- latin1_ejecutado$resultado
  resultado_utf8 <- utf8_ejecutado$resultado
  expect_identical(
    as.character(plan_latin1$estrategia),
    as.character(plan_utf8$estrategia)
  )
  expect_identical(
    as.numeric(plan_latin1$n_afectadas),
    as.numeric(plan_utf8$n_afectadas)
  )
  fila_latin1 <- resultado_latin1$registro[
    resultado_latin1$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]
  fila_utf8 <- resultado_utf8$registro[
    resultado_utf8$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]
  expect_identical(nrow(fila_latin1), 1L)
  expect_identical(as.character(fila_latin1$estado), "ejecutada")
  expect_identical(
    as.numeric(plan_latin1$n_afectadas[
      plan_latin1$estrategia == "recortar_espacios"
    ]),
    as.numeric(fila_latin1$n_cambiadas)
  )
  expect_identical(
    as.numeric(plan_utf8$n_afectadas[
      plan_utf8$estrategia == "recortar_espacios"
    ]),
    as.numeric(fila_utf8$n_cambiadas)
  )
  # 126 y no 127: un espacio duro al borde es un espacio al borde. En
  # ` borde ` + U+00C2 + ` `, la `A` con circunflejo y el espacio son un espacio
  # duro mal convertido; recortar el espacio de la punta cambia lo que la
  # reparacion puede rehacer, y la proteccion deja la celda entera. Y
  # ` borde ` + U+00A0 + ` ` queda `borde`, que es ASCII: no hay codificacion que
  # cambiar, y no cuenta como normalizada, ni en `latin1` (125) ni en UTF-8 (0).
  expect_identical(as.numeric(fila_latin1$n_cambiadas), 126)
  expect_identical(as.numeric(fila_latin1$n_codificacion_normalizada), 125)
  expect_identical(as.numeric(fila_utf8$n_cambiadas), 126)
  expect_identical(as.numeric(fila_utf8$n_codificacion_normalizada), 0)
  expect_identical(
    enc2utf8(resultado_latin1$datos$texto),
    enc2utf8(resultado_utf8$datos$texto)
  )
  expect_true(all(vapply(
    codigos,
    function(codigo) {
      i <- codigo - 0x7fL
      esperados <- utf8ToInt(utf8[[i]])
      obtenidos <- utf8ToInt(enc2utf8(resultado_latin1$datos$texto[[i]]))
      # Salvo el espacio duro, que al borde es un espacio y se recorta.
      esperados <- esperados[esperados > 0x7fL & esperados != 0xa0L]
      all(esperados %in% obtenidos)
    },
    logical(1L)
  )))
})

test_that("las acciones textuales reciben latin1 en una forma segura", {
  codigos <- .n2580_codigos()
  textos <- vapply(
    codigos,
    function(codigo) .n2580_latin1(codigo, sufijo = "x"),
    character(1L)
  )
  acciones <- list(
    recortar_espacios = function(x) .recortar_texto(x)$valor,
    eliminar_controles_invisibles = function(x) {
      .quitar_controles_invisibles(x)$valor
    },
    normalizar_espacios_invisibles = function(x) {
      .normalizar_espacios_invisibles(x)$valor
    },
    decodificar_entidades_html = function(x) {
      .decodificar_entidades_html(x)$valor
    },
    reemplazar_separadores = function(x) {
      .reemplazar_separadores(x)$valor
    },
    reparar_codificacion = function(x) {
      .reparar_codificacion(x, list(max_iteraciones = 2L))$valor
    },
    convertir_minusculas = function(x) {
      .transformar_capitalizacion(x, "convertir_minusculas", list())$valor
    }
  )
  for (nombre in names(acciones)) {
    expect_no_error(suppressWarnings(acciones[[nombre]](textos)))
  }
})
