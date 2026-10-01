# Ronda 9 sobre `medir()` y `modelo()`: siete casos donde lo que el paquete
# publicaba no era lo que decia, cada uno con su control.

test_that("un atributo unido con + se reconoce aunque una columna traiga +", {
  tabla <- data.frame(num = 1:5, "a+b" = as.character(1:5), c = letters[1:5],
                      v = as.character(10:14), check.names = FALSE)
  ref <- referencial(
    datos = data.frame("a+b" = c("1", "2", "9"), c = c("p", "q", "z"),
                       v = c("10", "20", "90"), check.names = FALSE),
    clave = c("a+b", "c"), valor = "v"
  )
  debil <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemDebil, "CSD"),
    "t", c("a+b", "c", "v"), referencial = ref
  )
  otra <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "num")
  # Antes: rechazado como "no es una columna" y `medir()` abortaba entero.
  medicion <- medir(modelo(debil, otra), list(t = tabla))
  expect_equal(sum(medicion$metrica == "CorrectitudSemDebil"), 5L)
  expect_equal(sum(medicion$metrica == "NoNulo"), 5L)
  expect_null(attr(medicion, "cobertura_metricas", exact = TRUE))
  es <- lupa:::.atributo_de_columnas
  expect_true(es("a+b+c+v", c("num", "a+b", "c", "v")))
  expect_true(es("x+a+", c("x", "a+")))
  # Y el control: un pedazo que no es columna sigue sin serlo.
  expect_false(es("a+b+x", c("a+b", "c")))
})

test_that("los mismos centinelas en otro orden son el mismo instrumento", {
  nn <- metricas_nucleo()$NoNulo
  a <- instanciar(especializar(nn, "A", valores_nulos = c("", "NA")), "t", "x",
                  nombre_instancia = "a")
  b <- instanciar(especializar(nn, "B", valores_nulos = c("NA", "")), "t", "x",
                  nombre_instancia = "b")
  expect_error(modelo(a, b), "mismo instrumento")
  # Control: con otros centinelas no es el mismo, y en una propiedad donde el
  # orden importa -`coeficientes`- el orden distinto tampoco se rechaza.
  c_ <- instanciar(especializar(nn, "C", valores_nulos = c("", "-")), "t", "x",
                   nombre_instancia = "c")
  expect_s3_class(modelo(a, c_), "modelo_calidad")
})

test_that("un separador dentro de un nombre no funde dos objetos", {
  propia <- metrica("Dos", "Dos objetos.", "entidad", "booleano")
  metodo <- function(tablas, instancia) {
    data.frame(resultado = c(TRUE, TRUE), entidad = c("a", "a\034b"),
               atributo = c("b\034c", "c"), fila = NA_integer_,
               objeto = c("z", "z"))
  }
  tablas <- list(a = data.frame(`b\034c` = 1, check.names = FALSE),
                 `a\034b` = data.frame(c = 1))
  instancia <- instanciar(especializar(propia), c("a", "a\034b"),
                          metodo = metodo)
  salida <- metodo(tablas, instancia)
  # Antes: "mas de una observacion para el mismo objeto".
  expect_no_error(lupa:::.validar_salida_medicion(salida, instancia, tablas))
  # Control: la repeticion real sigue detectandose.
  repetida <- salida[c(1, 1), ]
  expect_error(lupa:::.validar_salida_medicion(repetida, instancia, tablas),
               "exactamente una")
})

test_that("fila tiene que ser una posicion entera", {
  propia <- metrica("ConFila", "Por celda.", "instanciaAtributo", "booleano")
  con_fila <- function(fila) {
    function(tablas, instancia) {
      data.frame(resultado = c(TRUE, FALSE, TRUE), entidad = "t",
                 atributo = "x", fila = fila,
                 objeto = paste0("t$x[", 1:3, "]"), stringsAsFactors = FALSE)
    }
  }
  datos <- data.frame(x = 1:3)
  medir_con <- function(fila) {
    medir(modelo(instanciar(especializar(propia), "t", "x",
                            metodo = con_fila(fila))), datos)
  }
  # Antes: el factor publicaba sus codigos y el texto salia NA.
  .expect_no_medible(medir_con(factor(c("10", "20", "30"))), "fila")
  .expect_no_medible(medir_con(c("1a", "2b", "3c")), "fila")
  .expect_no_medible(medir_con(c(1.5, 2, 3)), "posici")
  # Control: enteros y dobles enteros miden.
  expect_equal(medir_con(1:3)$fila, 1:3)
  expect_equal(medir_con(c(1, 2, 3))$fila, 1:3)
})

test_that("el motivo de no_medible no publica un valor personal", {
  valor <- "secreto-12345678901"
  datos <- data.frame(dni = c(valor, "otro-98765432109", "mas-11122233344"),
                      ok = c("a", "b", "c"), stringsAsFactors = FALSE)
  propia <- metrica("Falla", "Falla con valor.", "atributo", "real")
  metodo <- function(tablas, instancia) {
    stop("No se puede medir el valor ", valor, ": instrumento roto")
  }
  falla <- instanciar(especializar(propia), "t", "dni", metodo = metodo)
  otra <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "ok")
  medicion <- suppressWarnings(medir(
    modelo(falla, otra), list(t = datos), columnas_personales = "dni"
  ))
  motivo <- attr(medicion, "cobertura_metricas", exact = TRUE)$motivo
  expect_false(grepl(valor, motivo, fixed = TRUE))
  expect_match(motivo, "valor protegido", fixed = TRUE)
  # Control: sin proteccion pedida, el mensaje sale como lo escribio el metodo.
  abierta <- suppressWarnings(medir(
    modelo(falla, otra), list(t = datos), proteger_datos_personales = FALSE
  ))
  expect_true(grepl(valor, attr(abierta, "cobertura_metricas")$motivo,
                    fixed = TRUE))
})

test_that("un universo vacio no se publica como una tabla vacia", {
  nn <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "edad")
  datos <- data.frame(edad = c(20, NA, 35), tiene_auto = c("Si", "No", "Si"))
  vacio <- medir(modelo(nn), datos,
                 aplicabilidad = list(edad = ~ tiene_auto == "Nunca"))
  motivo <- attr(vacio, "cobertura_metricas", exact = TRUE)$motivo
  expect_false(grepl("tiene cero filas", motivo, fixed = TRUE))
  expect_match(motivo, "universo aplicable", fixed = TRUE)
  # Lo mismo con el universo que declara la metrica con `aplicable`.
  propio <- instanciar(especializar(metricas_nucleo()$NoNulo, "NA2",
                                    aplicable = ~ tiene_auto == "Nunca"),
                       "t", "edad")
  motivo <- attr(medir(modelo(propio), datos), "cobertura_metricas")$motivo
  expect_match(motivo, "`aplicable`", fixed = TRUE)
  # Control: la tabla realmente vacia sigue diciendo que esta vacia.
  vacia <- medir(modelo(nn), datos[0, , drop = FALSE])
  expect_match(attr(vacia, "cobertura_metricas")$motivo, "tiene cero filas",
               fixed = TRUE)
})

test_that("con aplicabilidad, la fila de un metodo propio es la de la tabla original", {
  datos <- data.frame(
    tiene = c("Si", "Si", "Si", "No", "Si", "No", "Si", "No", "Si", "No"),
    cod = c("A1", "B2", "C3", "D4", NA, "F6", NA, "H8", "J9", "K0"),
    stringsAsFactors = FALSE
  )
  propia <- metrica("Formato2", "Formato propio.", "instanciaAtributo",
                    "booleano")
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos[[1L]]]]
    filas <- which(!is.na(x))
    data.frame(resultado = grepl("^[A-Z][0-9]$", x[filas]), entidad = "t",
               atributo = "cod", fila = filas,
               objeto = paste0("t$cod[", filas, "]"))
  }
  medicion <- medir(
    modelo(instanciar(especializar(propia), "t", "cod", metodo = metodo)),
    datos, aplicabilidad = list(cod = ~ tiene == "Si")
  )
  # El recorte deja las filas 1, 2, 3, 5, 7, 9; con valor: 1, 2, 3, 9.
  # Antes: 1, 2, 3, 6 -posiciones del recorte-.
  expect_equal(medicion$fila, c(1L, 2L, 3L, 9L))
  # Control: el metodo del paquete ya publicaba la fila original.
  formato <- instanciar(
    especializar(metricas_nucleo()$Formato, expresion_regular = "^[A-Z][0-9]$"),
    "t", "cod"
  )
  del_paquete <- medir(modelo(formato), datos,
                       aplicabilidad = list(cod = ~ tiene == "Si"))
  expect_equal(del_paquete$fila, c(1L, 2L, 3L, 9L))
})
