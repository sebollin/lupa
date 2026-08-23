# Tres diagnosticos que decidian con una sola senal teniendo otra al lado, ya
# calculada, sin usar. Los tres se arreglaron cruzandolas.

.perfil_r119 <- function(datos) {
  perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
}

.tiene_r119 <- function(perfil, tipo) {
  tipo %in% as.character(perfil$hallazgos$tipo_hallazgo)
}

test_that("una columna toda en `N/A` no se informa como constante", {
  # `n_distintos` cuenta los valores tal como estan guardados y no descuenta
  # los que dicen ausencia sin ser NA. El hallazgo afirmaba, con estas
  # palabras, que la columna contiene "un unico valor no ausente". Era falso:
  # no contiene ninguno.
  perfil <- .perfil_r119(
    data.frame(x = rep("N/A", 50L), stringsAsFactors = FALSE)
  )
  expect_equal(perfil$columnas$n_distintos, 1L)
  expect_equal(perfil$columnas$n_faltantes_disfrazados, 50L)

  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "constante", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  descripcion <- as.character(hallazgo$descripcion[[1L]])
  expect_match(descripcion, "no tiene ningun valor")
  expect_false(grepl("no ausente", descripcion, fixed = TRUE))
})

test_that("una columna constante de verdad se sigue informando como tal", {
  perfil <- .perfil_r119(
    data.frame(x = rep("Montevideo", 50L), stringsAsFactors = FALSE)
  )
  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "constante", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  expect_match(as.character(hallazgo$descripcion[[1L]]), "\u00fanico valor")
})

test_that("una clave sana con filas en `SIN DATO` no se informa como rota", {
  # Esas filas colisionan entre si, la concentracion da 1 y el hallazgo decia
  # que la columna no sirve como identificador. La lectura correcta es la
  # contraria: la clave esta bien y faltan treinta documentos.
  set.seed(2)
  documentos <- c(
    sprintf("%08d", sample(1e7, 970L)), rep("SIN DATO", 30L)
  )
  perfil <- .perfil_r119(
    data.frame(documento = documentos, stringsAsFactors = FALSE)
  )
  expect_false(.tiene_r119(perfil, "casi_clave"))
  # Y lo que sí pasa se informa por su nombre.
  expect_true(.tiene_r119(perfil, "faltantes_disfrazados"))
})

test_that("una clave rota se informa, con o sin filas en `SIN DATO`", {
  # El error iba en los dos sentidos: las filas disfrazadas tambien diluian la
  # concentracion de una clave realmente rota y la callaban.
  set.seed(3)
  base <- sprintf("%08d", sample(1e7, 990L))

  rota <- .perfil_r119(
    data.frame(documento = c(base, rep(base[[1L]], 10L)),
               stringsAsFactors = FALSE)
  )
  expect_true(.tiene_r119(rota, "casi_clave"))

  mixta <- .perfil_r119(
    data.frame(
      documento = c(base[1:960], rep(base[[1L]], 10L), rep("SIN DATO", 30L)),
      stringsAsFactors = FALSE
    )
  )
  expect_true(.tiene_r119(mixta, "casi_clave"))
})

test_that("las dos preguntas sobre si algo es una numeracion se contestan igual", {
  # `posible_identificador` exigia `secuencia_entera_densa` -densidad 0,8 y
  # veinte distintos- mientras la guarda que calla Benford y los limites de
  # Tukey usa 0,5, cinco distintos y la ausencia de salto de escala. Un `MotId`
  # de 1 a 4557 sobre 3.159 filas tiene densidad 0,694: las guardas lo trataban
  # como numeracion y callaban dos diagnosticos, y el perfil nunca decia que
  # habia detectado una numeracion.
  set.seed(4)
  perfil <- .perfil_r119(data.frame(MotId = sort(sample(4557L, 3159L))))
  fila <- perfil$columnas

  expect_false(fila$secuencia_entera_densa)
  expect_lt(fila$densidad_secuencia_entera, 0.8)
  expect_gt(fila$densidad_secuencia_entera, 0.5)

  # La guarda lo reconoce...
  expect_true(lupa:::.parece_identificador_numerico(fila))
  # ...y ahora el perfil tambien lo dice.
  expect_true(.tiene_r119(perfil, "posible_identificador"))
  # Con severidad `ok`: es una lectura de la columna, no un defecto.
  severidad <- perfil$hallazgos$severidad[
    as.character(perfil$hallazgos$tipo_hallazgo) == "posible_identificador"
  ]
  expect_equal(as.character(severidad), "ok")
})
