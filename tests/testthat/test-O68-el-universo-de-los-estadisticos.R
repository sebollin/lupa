# Cuatro cosas de la ronda O67, todas de la misma familia: un estadistico que se
# calculo sobre parte de los valores -o que no se pudo calcular- tiene que decirlo.
# Las tres primeras eran la MISMA pregunta contestada dos veces, con las dos ramas
# en desacuerdo; la cuarta rompia.

test_that("una columna que no admite cuantiles interpolados no rompe el analisis", {
  # El caso real que lo motivo es la clase Period del paquete lubridate: R la
  # declara numerica, y `quantile(type = 7)` interpola multiplicando por una
  # fraccion, cosa que su aritmetica rechaza. Medido a mano, eso
  # mataba `distribucion_valores()` y con el `analizar()` entero, mientras
  # `perfilar()` sobre la misma tabla sobrevivia. La guarda excluia por una LISTA de
  # clases -`Date`, `POSIXt`, `integer64`- y `Period` era la cuarta.
  #
  # Aca se prueba el CONTRATO de la guarda y no la clase: si el cuantil falla, se
  # declara y el analisis sigue. Por eso se simula la falla en lugar de traer ese
  # paquete, que no esta en `Suggests`: usar uno no declarado pasa la suite y rompe
  # el check con "import not declared", y esta misma prueba lo hizo en su primera
  # version.
  datos <- data.frame(v = c(10, 20, 30), w = c(1, 2, 3))
  local_mocked_bindings(
    quantile = function(x, ...) {
      stop("periods must have integer values", call. = FALSE)
    },
    .package = "stats"
  )
  resultado <- distribucion_valores(datos, proteger_datos_personales = FALSE)
  cuantiles <- as.data.frame(resultado$cuantiles)

  expect_equal(nrow(cuantiles), 10L)
  expect_true(all(is.na(cuantiles$valor)))
  expect_true(all(grepl("^no_interpolable:", cuantiles$estado)))
  # El estado NOMBRA la clase que no se pudo interpolar: sin eso, quien lee no
  # sabe de que columna desconfiar.
  expect_true(all(grepl("numeric", cuantiles$estado, fixed = TRUE)))
  # Y las filas siguen: una probabilidad por columna, para que el universo del
  # objeto no cambie segun si el calculo salio.
  expect_setequal(unique(cuantiles$columna), c("v", "w"))

  # `analizar()` NO se comprueba con la falla simulada, y el motivo es una medicion:
  # el mock rompe `stats::quantile` para todo el proceso, y `analizar()` la usa
  # tambien para los outliers, donde no hay -ni corresponde- una guarda de clase.
  # Con la clase real que lo motivo, `analizar()` completo termina bien; eso quedo
  # medido a mano y anotado en PENDIENTES 2.460, no se afirma desde aca.
})

test_that("con el cuantil andando, el estado sigue siendo calculado", {
  # La mitad de control: sin la falla simulada, la misma tabla calcula. Una guarda
  # que se disparara siempre pasaria las comprobaciones de arriba sin distinguir
  # nada.
  datos <- data.frame(v = c(10, 20, 30), w = c(1, 2, 3))
  cuantiles <- as.data.frame(
    distribucion_valores(datos, proteger_datos_personales = FALSE)$cuantiles
  )
  expect_true(all(cuantiles$estado == "calculado"))
  expect_equal(cuantiles$valor[cuantiles$columna == "v" &
                                 cuantiles$probabilidad == 0.5], 20)
})

test_that("una fecha no finita se cuenta como excluida en las dos clases", {
  fecha <- as.Date(c("2020-01-01", "2020-01-03", "2020-01-02"))
  fecha[2] <- Inf
  hora <- as.POSIXct(c("2020-01-01", "2020-01-03", "2020-01-02"), tz = "UTC")
  hora[2] <- Inf

  de <- function(col) {
    as.data.frame(columnas(perfilar(data.frame(f = col),
                                    analizar_dependencias = FALSE)))
  }
  para_date <- de(fecha)
  para_posix <- de(hora)

  # El conteo estaba escrito dos veces -`sum(!is.na(x))` para `Date`,
  # `sum(is.finite(x))` para `POSIXt`- y sobre el mismo dato una decia 3 y la otra
  # 2. Ahora las dos dicen 2, y las dos declaran la exclusion: antes ninguna lo
  # hacia.
  for (fila in list(para_date, para_posix)) {
    expect_equal(fila$n, 3L)
    expect_equal(fila$n_fechas_resumidas, 2L)
    expect_equal(fila$n_valores_excluidos_resumen, 1L)
    expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_valores")
  }
  expect_equal(para_date$n_fechas_resumidas, para_posix$n_fechas_resumidas)

  # Los controles: sin valores no finitos no se declara nada, y un `NA` no es un
  # valor excluido del resumen -es una ausencia, y se cuenta aparte-.
  limpia <- de(as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
  expect_equal(limpia$n_valores_excluidos_resumen, 0L)
  expect_equal(limpia$estado_resumen_cuantitativo, "calculados")
  con_na <- de(as.Date(c("2020-01-01", NA, "2020-01-03")))
  expect_equal(con_na$n_valores_excluidos_resumen, 0L)
  expect_equal(con_na$estado_resumen_cuantitativo, "calculados")
})

test_that("integer64 sin ningun valor que sobreviva dice sin_valores", {
  skip_if_not_installed("bit64")
  # El estado se escribia apenas habia excluidos, ANTES de saber si quedaba algo:
  # con seis centinelas de seis publicaba `calculados_sobre_valores` junto a cuatro
  # `NA`. La rama `double` sobre el mismo dato ya decia `sin_valores`.
  grande <- data.frame(k = bit64::as.integer64(rep(-99, 6)))
  doble <- data.frame(k = rep(-99, 6))
  de <- function(datos) {
    as.data.frame(columnas(perfilar(datos, analizar_dependencias = FALSE,
                                     sentinelas_numericos = -99)))
  }
  fila64 <- de(grande)
  filad <- de(doble)
  expect_equal(fila64$estado_resumen_cuantitativo, "sin_valores")
  expect_equal(fila64$estado_resumen_cuantitativo,
               filad$estado_resumen_cuantitativo)
  expect_true(is.na(fila64$media))
  expect_equal(fila64$n_valores_excluidos_resumen, 6L)

  # El control: con un valor que sobrevive, el estado SI es
  # `calculados_sobre_valores` y la media es la de ese valor.
  sobrevive <- de(data.frame(k = bit64::as.integer64(c(rep(-99, 5), 7))))
  expect_equal(sobrevive$estado_resumen_cuantitativo, "calculados_sobre_valores")
  expect_equal(sobrevive$media, 7)
  expect_equal(sobrevive$n_valores_excluidos_resumen, 5L)
})

test_that("las longitudes declaran sobre cuantos valores se calcularon", {
  # `nchar(type = "chars", allowNA = TRUE)` devuelve `NA` sobre bytes que no son
  # UTF-8 valido, y esos se descartaban sin contarlos: la fila publicaba
  # `longitud_media = 4.5` -el promedio de DOS de tres- y ningun campo lo decia.
  #
  # El fixture se construye con `rawToChar` para que el archivo quede en ASCII.
  invalido <- rawToChar(as.raw(c(0xe1, 0xbd)))
  texto <- c("hola", "mundo", invalido)
  Encoding(texto) <- "UTF-8"
  fila <- as.data.frame(columnas(perfilar(data.frame(txt = texto),
                                          analizar_dependencias = FALSE)))
  expect_equal(fila$n, 3L)
  expect_equal(fila$longitud_media, 4.5)
  expect_equal(fila$n_longitudes_resumidas, 2L)
  # La CAUSA ya viajaba aparte y sigue ahi: el universo lo declara el campo nuevo,
  # el motivo lo declara este.
  expect_equal(fila$n_codificacion_invalida, 1L)
  # Y el numero se rehace: (4 + 5) / 2.
  expect_equal(fila$longitud_media,
               mean(nchar(c("hola", "mundo"), type = "chars")))

  # Un `NA` NO es un valor no medible: es una ausencia. Con cuatro valores y uno
  # ausente, las longitudes se calculan sobre tres y eso es lo que se declara. El
  # primer intento de este arreglo contaba las dos cosas juntas y lo delato un
  # fixture de la suite que tiene un `NA_character_`.
  con_na <- as.data.frame(columnas(perfilar(
    data.frame(t = c("a", "abcd", NA_character_, "xy")),
    analizar_dependencias = FALSE)))
  expect_equal(con_na$n, 4L)
  expect_equal(con_na$n_longitudes_resumidas, 3L)
  expect_equal(con_na$n_codificacion_invalida, 0L)

  # Controles: texto medible entero declara su universo completo, y una columna sin
  # longitudes deja el campo en NA.
  limpio <- as.data.frame(columnas(perfilar(
    data.frame(t = c("hola", "mundo")), analizar_dependencias = FALSE)))
  expect_equal(limpio$n_longitudes_resumidas, 2L)
  expect_equal(limpio$n, 2L)
  numerica <- as.data.frame(columnas(perfilar(
    data.frame(x = c(1, 2, 3)), analizar_dependencias = FALSE)))
  expect_true(is.na(numerica$n_longitudes_resumidas))
})
