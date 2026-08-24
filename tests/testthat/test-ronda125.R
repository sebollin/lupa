# El minimo y el maximo de una columna protegida se recuperaban exactos a partir
# de dos campos que no muestran ningun valor de celda.

test_that("el rango de una columna protegida no se despeja desde el perfil", {
  # `n_posiciones` es `maximo - minimo + 1` y los ordenes de magnitud que guarda
  # Benford son `log10(maximo/minimo)`: dos ecuaciones con dos incognitas. Sobre
  # una columna de cedulas devolvian 27 y 599.917, que eran los valores reales,
  # mientras `minimo` y `maximo` salian en NA como corresponde.
  set.seed(3)
  clave <- sort(sample.int(600000L, 10000L))
  perfil <- perfilar(data.frame(cedula = clave))
  columnas <- perfil$columnas
  expect_equal(as.character(columnas$tipo_dato_personal), "documento_identidad")
  expect_true(is.na(columnas$minimo))
  expect_true(is.na(columnas$maximo))
  # Lo que cierra el despeje: ninguno de los campos que codifican el rango.
  expect_true(is.na(columnas$n_posiciones_secuencia_entera))
  expect_true(is.na(columnas$n_huecos_secuencia_entera))
  expect_true(is.na(columnas$densidad_secuencia_entera))
  expect_true(is.na(perfil$meta$benford$resultados$cedula$ordenes_magnitud))
})

test_that("el motivo de la cobertura no publica la razon entre los extremos", {
  # El texto imprimia `log10(max/min)` con su valor medido. No es un dato de
  # celda, y por eso se paso por alto: lo que se filtra es la relacion entre dos
  # datos, que con otra del mismo perfil se despeja.
  set.seed(9)
  cedulas <- c(sample(4000000:4009999, 400L), rep(NA_integer_, 5L))
  perfil <- perfilar(data.frame(cedula = cedulas))
  cobertura <- perfil$cobertura_diagnosticos
  motivos <- as.character(cobertura$motivo)
  expect_gt(length(motivos), 0L)
  expect_false(any(grepl("log10\\(max/min\\)[[:space:]]*[0-9]", motivos)))
  expect_true(any(grepl("[valor protegido]", motivos, fixed = TRUE)))
})

test_that("sin datos personales los campos del rango siguen publicandose", {
  # La otra direccion: proteger no puede costar la medicion en una columna que
  # no es personal, porque esos campos son los que sostienen el diagnostico de
  # numeracion.
  perfil <- perfilar(
    data.frame(codigo = 1:500), analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_true(is.na(perfil$columnas$tipo_dato_personal))
  expect_equal(perfil$columnas$n_posiciones_secuencia_entera, 500)
  expect_equal(perfil$columnas$densidad_secuencia_entera, 1)
})
