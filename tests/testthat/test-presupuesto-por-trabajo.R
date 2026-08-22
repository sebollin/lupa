skip_if_not_installed("stringdist")

# Una tabla del catalogo de PostGIS -3.912 filas, 5 columnas- tardaba 243
# segundos. No era geometria: eran cadenas largas, WKT de proyecciones. El
# detector de vocabulario era el 99,6 % del costo, y sus dos topes contaban
# unidades sin mirar cuanto costaba cada una: 800 valores son 319.600 pares,
# muy por debajo del tope de dos millones, pero cada comparacion es una
# Jaro-Winkler sobre 900 caracteres.

.valores_de <- function(n, largo, semilla = 1L) {
  set.seed(semilla)
  base <- vapply(
    seq_len(n),
    function(i) paste(sample(letters, largo, TRUE), collapse = ""),
    character(1L)
  )
  rep(base, each = 3L)
}

# Apaga los otros dos topes a proposito, para que lo que se vea sea el efecto
# del presupuesto por trabajo y no el de un tope vecino. No sirve para saber
# que recibe un usuario: para eso esta `.por_omision()`.
.vocabulario <- function(x, ...) {
  .grupos_casi_duplicados_vocabulario(
    x, NULL, "columna", max_valores = 1e6, max_pares = Inf, ...
  )
}

.por_omision <- function(x, ...) {
  .grupos_casi_duplicados_vocabulario(x, NULL, "columna", ...)
}

test_that("el trabajo se mide por caracteres comparados y no por pares", {
  # Esta es la comprobacion que separa los dos modelos de costo. Con el mismo
  # numero de valores distintos, decuplicar el largo multiplica el trabajo por
  # cien y no por diez, porque comparar dos cadenas cuesta del orden del
  # producto de sus largos. Contra el modelo que cuenta pares por largo medio
  # esta comprobacion falla: da diez.
  corto <- .vocabulario(.valores_de(60, 10), max_trabajo = Inf)
  largo <- .vocabulario(.valores_de(60, 100), max_trabajo = Inf)
  razon <- largo$alcance$trabajo_estimado / corto$alcance$trabajo_estimado
  expect_gt(razon, 90)
  expect_lt(razon, 110)
  expect_equal(corto$alcance$unidad_trabajo, "comparaciones de caracter")
})

test_that("el trabajo declarado es exacto, no una aproximacion", {
  # La suma de L_i x L_j sobre todos los pares se calcula en tiempo lineal
  # como ((suma)^2 - suma de cuadrados) / 2. Si esa identidad se rompe, el
  # presupuesto corta donde no corresponde.
  valores <- c("ab", "cde", "f", "ghij", "klmno")
  alcance <- .vocabulario(valores, max_trabajo = Inf)$alcance
  largos <- nchar(valores)
  esperado <- sum(outer(largos, largos)[upper.tri(diag(length(largos)))])
  expect_equal(alcance$trabajo_estimado, esperado)
})

test_that("una columna de valores largos se recorta y lo dice", {
  alcance <- .vocabulario(.valores_de(400, 900))$alcance
  expect_true(alcance$truncado)
  expect_lt(alcance$n_unidades_comparadas, alcance$n_unidades_normalizadas)
  expect_gt(alcance$n_unidades_sin_comparar, 0L)
  expect_gt(alcance$trabajo_sin_comparar, 0)
  expect_equal(alcance$motivo_presupuesto, "max_trabajo")
  # Lo comparado mas lo que quedo afuera es el total: si no cierran, uno de los
  # tres numeros esta informando de mas.
  expect_equal(
    alcance$trabajo_comparado + alcance$trabajo_sin_comparar,
    alcance$trabajo_estimado
  )
  expect_lte(alcance$trabajo_comparado, 2e10)
})

test_that("una columna de valores cortos no se recorta por el tope nuevo", {
  # El riesgo del arreglo era recortar el caso comun. Dos mil valores
  # distintos de ochenta caracteres son una columna corriente y tienen que
  # compararse enteros: son 1.999.000 pares, mas que los 79.800 de la columna
  # de arriba que si se recorta.
  alcance <- .vocabulario(.valores_de(2000, 80))$alcance
  expect_false(alcance$truncado)
  expect_equal(alcance$n_unidades_comparadas, alcance$n_unidades_normalizadas)
  expect_equal(alcance$trabajo_sin_comparar, 0)
  expect_equal(alcance$motivo_presupuesto, "")
  expect_gt(alcance$n_pares_comparados, 1900000)
})

test_that("con los topes por omision manda `max_pares` sobre vocabularios grandes", {
  # Esto no lo veia ninguna medicion propia, porque todas apagaban `max_pares`
  # para aislar el tope nuevo, y despues se leyo esa medicion como si fuera lo
  # que recibe un usuario. Con los topes reales, tres mil formas cortas se
  # recortan a dos mil, y no por el presupuesto por trabajo.
  alcance <- .por_omision(.valores_de(3000, 20))$alcance
  expect_true(alcance$truncado)
  expect_equal(alcance$motivo_presupuesto, "max_pares")
  expect_equal(alcance$n_unidades_comparadas, 2000L)
  expect_equal(alcance$n_unidades_sin_comparar, 1000L)
  # Y con `max_pares` apagado no se recorta: el tope nuevo no tiene nada que
  # ver con este caso, que es exactamente lo que hay que poder distinguir.
  expect_false(.vocabulario(.valores_de(3000, 20))$alcance$truncado)
})

test_that("el tope se puede mover y el efecto es el esperado", {
  valores <- .valores_de(300, 200)
  entero <- .vocabulario(valores, max_trabajo = Inf)$alcance
  expect_false(entero$truncado)

  apretado <- .vocabulario(valores, max_trabajo = 1e6)$alcance
  expect_true(apretado$truncado)
  expect_lt(apretado$n_unidades_comparadas, entero$n_unidades_comparadas)
  expect_lte(apretado$trabajo_comparado, 1e6)

  medio <- .vocabulario(valores, max_trabajo = 1e7)$alcance
  expect_gte(medio$n_unidades_comparadas, apretado$n_unidades_comparadas)
  expect_lte(medio$n_unidades_comparadas, entero$n_unidades_comparadas)
})

test_that("el tope de pares y el de trabajo se declaran por separado", {
  valores <- .valores_de(300, 200)
  ambos <- .grupos_casi_duplicados_vocabulario(
    valores, NULL, "columna", max_valores = 1e6,
    max_pares = 100L, max_trabajo = 1e6
  )$alcance
  # Cuando los dos aprietan, el motivo nombra a los dos: saber cual recorto no
  # es un detalle, porque el usuario tiene que elegir cual aflojar.
  expect_match(ambos$motivo_presupuesto, "max_trabajo")
  expect_match(ambos$motivo_presupuesto, "max_pares")

  solo_pares <- .grupos_casi_duplicados_vocabulario(
    valores, NULL, "columna", max_valores = 1e6,
    max_pares = 100L, max_trabajo = Inf
  )$alcance
  expect_equal(solo_pares$motivo_presupuesto, "max_pares")
})

test_that("un tope invalido se rechaza en vez de degenerar", {
  for (malo in list(0, -1, NA, NA_real_, "mucho", c(1, 2), 1.5)) {
    expect_error(
      .vocabulario(.valores_de(20, 10), max_trabajo = malo),
      "max_trabajo",
      fixed = TRUE
    )
  }
})

test_that("el recorte llega al usuario por cobertura_diagnosticos", {
  datos <- data.frame(texto = .valores_de(400, 900), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "proximidad_vocabulario", ]
  expect_gte(nrow(fila), 1L)
  # Lo que no se comparo se cuenta, y se dice como cubrirlo. Un diagnostico
  # recortado que no lo declara es peor que uno que no corre.
  expect_match(fila$motivo[[1L]], "sin comparar")
  expect_match(fila$motivo[[1L]], "max_trabajo")
  expect_match(fila$como_resolverlo[[1L]], "max_trabajo_vocabulario")
})

test_that("el recorte dice que no es una muestra, porque no lo es", {
  datos <- data.frame(texto = .valores_de(400, 900), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "proximidad_vocabulario", ]
  # El corte toma las primeras formas del alfabeto. Decir cuantas quedaron
  # afuera sin decir cuales deja suponer que se comparo una muestra al azar, y
  # lo que queda afuera es el tramo final del alfabeto.
  expect_match(fila$motivo[[1L]], "primeras en orden alfabetico")
  expect_match(fila$como_resolverlo[[1L]], "no una muestra")
  expect_match(fila$como_resolverlo[[1L]], "no depende del orden de las filas")
})

test_that("el orden de las filas ya no cambia lo que entra en el recorte", {
  # Esta prueba media lo contrario y pasaba: los mismos 300 valores daban ocho
  # formas comparadas si los largos venian primero y ciento cincuenta si venian
  # primero los cortos. Eso estaba declarado en la documentacion como una
  # advertencia al usuario -"si la tabla viene ordenada, conviene desordenarla
  # antes"-, cuando en realidad era un defecto: el veredicto dependia de la
  # forma fisica de la tabla.
  largos <- .valores_de(150, 400, semilla = 3L)
  cortos <- .valores_de(150, 20, semilla = 4L)
  primero_largos <- .vocabulario(c(largos, cortos), max_trabajo = 5e6)$alcance
  primero_cortos <- .vocabulario(c(cortos, largos), max_trabajo = 5e6)$alcance
  expect_equal(
    primero_largos$n_unidades_comparadas,
    primero_cortos$n_unidades_comparadas
  )
  expect_equal(primero_largos$n_pares_comparados,
               primero_cortos$n_pares_comparados)
  # El trabajo total posible es el mismo en los dos, y ahora tambien lo es el
  # que entra en el presupuesto.
  expect_equal(
    primero_largos$trabajo_estimado, primero_cortos$trabajo_estimado
  )
})

test_that("cuando las formas caras caen primero en el alfabeto, entran menos", {
  # El presupuesto sigue siendo de trabajo, asi que cuantas formas entran
  # depende de cuanto cuesta cada una. Lo que cambio es que ya no depende de
  # como venga ordenado el archivo: si las caras estan al principio del
  # alfabeto, entran menos, y eso pasa siempre igual.
  #
  # Es la misma limitacion de antes, dejada de ser una loteria. El usuario que
  # necesite mas alcance sube `max_trabajo_vocabulario`, y el motivo se lo dice.
  caras <- paste0("AAA", .valores_de(60, 400, semilla = 3L))
  baratas <- paste0("ZZZ", .valores_de(60, 20, semilla = 4L))
  alcance <- .vocabulario(c(baratas, caras), max_trabajo = 5e6)$alcance
  expect_true(isTRUE(alcance$truncado))
  con_mas_presupuesto <- .vocabulario(
    c(baratas, caras), max_trabajo = 5e11
  )$alcance
  expect_gt(
    con_mas_presupuesto$n_unidades_comparadas, alcance$n_unidades_comparadas
  )
})

test_that("el presupuesto por trabajo evita el costo cuadratico", {
  skip_on_cran()
  # El arreglo tiene que verse en el reloj y no solo en los atributos.
  valores <- .valores_de(500, 900)
  con_tope <- system.time(.vocabulario(valores))[["elapsed"]]
  sin_tope <- system.time(.vocabulario(valores, max_trabajo = Inf))[["elapsed"]]
  expect_lt(con_tope * 2, sin_tope)
})
