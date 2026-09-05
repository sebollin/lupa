test_that("declarar una columna personal gana sobre el lexico", {
  set.seed(2)
  n <- 120L
  datos <- data.frame(
    cod_benef = sprintf("%08d", sample(1e7, n)),
    apodo = sample(c("Pepe", "Tita", "Coco"), n, replace = TRUE),
    monto = seq_len(n),
    stringsAsFactors = FALSE
  )
  # El lexico no reconoce ninguno de los dos nombres: ese es el limite que
  # `columnas_personales` existe para cubrir.
  sin <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false("apodo" %in% sin$datos_personales$columna)

  con <- perfilar(
    datos, analizar_dependencias = FALSE,
    columnas_personales = c(cod_benef = "documento_identidad", apodo = "nombre")
  )
  expect_setequal(con$datos_personales$columna, c("cod_benef", "apodo"))
  expect_setequal(con$datos_personales$tipo, c("documento_identidad", "nombre"))
  expect_true(all(con$datos_personales$proteger))
  expect_true(all(con$datos_personales$poder_discriminante == "declarado"))
  expect_true(all(
    con$columnas$moda[con$columnas$columna %in% c("cod_benef", "apodo")] ==
      "[valor protegido]"
  ))
})

test_that("la forma sin tipo tambien vale", {
  datos <- data.frame(apodo = rep(c("Pepe", "Tita"), 30L),
                      stringsAsFactors = FALSE)
  p <- perfilar(datos, columnas_personales = "apodo",
                analizar_dependencias = FALSE)
  expect_equal(p$datos_personales$tipo, "declarado")
  expect_true(p$datos_personales$proteger)
})

test_that("una declaracion que no se puede cumplir se rechaza", {
  datos <- data.frame(a = 1:10)
  expect_error(
    perfilar(datos, columnas_personales = "no_existe"),
    "nombra columnas inexistentes"
  )
  expect_error(
    perfilar(datos, columnas_personales = c("a", "a")),
    "repite una columna"
  )
  expect_error(
    perfilar(datos, columnas_personales = c(a = "")),
    "declara un tipo vac"
  )
  expect_error(
    perfilar(datos, columnas_personales = 1L),
    "debe ser un vector de texto"
  )
})

test_that("el campo de proteccion dice lo que paso, no lo que se pensaba hacer", {
  datos <- data.frame(correo = paste0("p", 1:120, "@ejemplo.uy"),
                      stringsAsFactors = FALSE)
  con <- perfilar(datos, analizar_dependencias = FALSE,
                  proteger_datos_personales = TRUE)
  sin <- perfilar(datos, analizar_dependencias = FALSE,
                  proteger_datos_personales = FALSE)
  expect_true(con$columnas$dato_personal_protegido)
  expect_equal(con$columnas$moda, "[valor protegido]")
  expect_false(sin$columnas$dato_personal_protegido)
  expect_false(sin$columnas$moda == "[valor protegido]")
  # La intencion de la clasificacion no se pierde: sigue estando donde estaba.
  expect_true(sin$datos_personales$proteger)
})

test_that("un correo ofuscado sigue siendo un correo", {
  ofuscados <- list(
    paste0("persona", 1:60, " at ejemplo.com"),
    paste0("otra", 1:60, " (at) sitio.uy"),
    paste0("tercera", 1:60, " arroba sitio punto uy")
  )
  for (valores in ofuscados) {
    p <- perfilar(data.frame(v = valores, stringsAsFactors = FALSE),
                  analizar_dependencias = FALSE)
    expect_equal(p$datos_personales$tipo, "correo")
    expect_equal(p$datos_personales$fundamento,
                 "forma de correo ofuscada dominante")
    expect_true(p$datos_personales$proteger)
  }
})

test_that("una frase con `at` en el medio no es un correo", {
  for (valores in list(rep(c("lunes at casa", "martes at casa"), 30L),
                       paste("comentario numero", 1:60))) {
    p <- perfilar(data.frame(v = valores, stringsAsFactors = FALSE),
                  analizar_dependencias = FALSE)
    expect_equal(nrow(p$datos_personales), 0L)
  }
})

test_that("la confirmacion de un validador declara su alcance cuando es parcial", {
  textos <- rep(c("1.234.567-2", "2.345.678-3", "3.456.789-4"), 200L)
  completo <- .proporcion_validadores(
    textos, validadores_uruguay(), 0.9, muestra = 10L
  )
  expect_equal(attr(completo, "n_evaluados")[[1L]], length(textos))
  expect_equal(.alcance_validacion(completo, 1L), "")

  parcial <- .proporcion_validadores(
    textos, validadores_uruguay(), 0.9, muestra = 10L, max_completo = 50L
  )
  expect_equal(attr(parcial, "n_evaluados")[[1L]], 50L)
  expect_equal(attr(parcial, "n_total"), length(textos))
  expect_equal(.alcance_validacion(parcial, 1L), " sobre 50 de 600 valores")
})

test_that("una matriz es una tabla y se acepta declarando la conversion", {
  m <- matrix(c(1:50, seq(0.5, 25, by = 0.5)), ncol = 2L)
  p <- perfilar(m, analizar_dependencias = FALSE)
  expect_equal(nrow(p$columnas), 2L)
  expect_equal(p$columnas$columna, c("V1", "V2"))
  expect_match(p$meta$entrada_convertida, "matriz de 50 por 2")

  dimnames(m) <- list(NULL, c("edad", "peso"))
  expect_equal(perfilar(m, analizar_dependencias = FALSE)$columnas$columna,
               c("edad", "peso"))

  # Un data frame no declara conversion porque no hubo ninguna.
  expect_true(is.na(
    perfilar(data.frame(a = 1:5), analizar_dependencias = FALSE)$meta$entrada_convertida
  ))
  expect_error(perfilar(list(a = 1)), "data.frame, tibble, data.table o una matriz")
})

test_that("un tipo que no se puede agrupar se declara, no revienta", {
  datos <- data.frame(a = 1:50, b = letters[1:50 %% 26 + 1],
                      stringsAsFactors = FALSE)
  datos$r <- as.raw(1:50)
  # Antes producia un error crudo de R -"tipo no implementado 'raw' en
  # 'orderVector1'"- que abortaba el perfil entero.
  perfil <- expect_no_error(perfilar(datos))
  expect_equal(nrow(perfil$columnas), 3L)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "dependencias_funcionales", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$columna, "r")
  expect_match(fila$motivo, "no se puede agrupar")
  # El motivo distingue el tipo del tope: subir `max_columnas` no resuelve esto.
  expect_no_match(fila$motivo, "primeras")
})

test_that("el tope de columnas y el tipo se declaran por separado", {
  datos <- as.data.frame(matrix(seq_len(500L), ncol = 10L))
  datos$r <- as.raw(1:50)
  perfil <- perfilar(datos, max_columnas_dependencias = 3L)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "dependencias_funcionales", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "no se puede agrupar")
  expect_match(fila$motivo, "primeras 3")
})

# El lexico de nombres no puede ser completo -y el codigo lo dice-, pero si
# tiene que ser coherente consigo mismo. El patron de fecha de nacimiento
# abreviaba la PRIMERA palabra (`f_nacimiento`) y no la segunda, asi que
# `fecha_nac` no se clasificaba: una de las formas mas frecuentes en registros
# administrativos de la region quedaba sin proteger.
#
# Se enumeran las cinco formas de escribir lo mismo en vez de agregar la que
# aparecio. Y va con su mitad de control: los nombres que NO deben clasificarse,
# porque un lexico que crece sin control enmascara columnas que no son
# personales, y eso rompe el analisis en silencio.
test_that("las abreviaturas de fecha de nacimiento y de defuncion se clasifican", {
  fechas <- as.Date("1980-01-01") + seq(0L, 3000L, by = 100L)

  clasifica <- function(nombre) {
    datos <- data.frame(x = fechas)
    names(datos) <- nombre
    perfil <- perfilar(datos, analizar_dependencias = FALSE)
    nrow(perfil$datos_personales) > 0L
  }

  personales <- c(
    "fecha_nacimiento", "f_nacimiento", "fec_nacimiento", "nacimiento",
    "fecha_nac", "f_nac", "fec_nac", "fechanac",
    "fecha_defuncion", "f_defuncion", "fec_defuncion", "fecha_deceso"
  )
  for (nombre in personales) {
    expect_true(clasifica(nombre), info = nombre)
  }

  # La mitad que hace que esto pruebe algo: `fecha_fall` no esta en el lexico a
  # proposito, porque `fall` es tambien "falla" en datos de mantenimiento.
  neutros <- c(
    "nacionalidad", "fecha_facturacion", "fecha_falla", "fecha_fall_equipo",
    "fecha_alta", "nac_id"
  )
  for (nombre in neutros) {
    expect_false(clasifica(nombre), info = nombre)
  }
})
