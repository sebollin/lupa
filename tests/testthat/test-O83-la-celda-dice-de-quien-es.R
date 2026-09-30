# La celda del tablero tiene que identificar sus medidas, y el indice no puede
# afirmar sobre universos lo que sus propios componentes desmienten.
#
# La identidad publicada era `(metrica, objeto)` y `metrica` es el nombre
# GENERICO de la metrica. Con eso, dos celdas distintas publicaban la misma
# identidad por dos caminos: dos tablas con una columna del mismo nombre, y dos
# especializaciones de la misma generica sobre la misma columna. Rehacer la celda
# con las claves publicadas mezclaba las medidas de las dos y no reproducia
# ninguna.

.o83_medidas_dos_entidades <- function() {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloO83")
  medir(
    modelo(list(
      instanciar(no_nulo, "t1", "cod"), instanciar(no_nulo, "t2", "cod")
    )),
    list(
      t1 = data.frame(cod = c(1, 2)),
      t2 = data.frame(cod = c(NA, NA, 3, 4))
    ),
    id_medicion = "o83"
  )
}

.o83_clave <- function(tablero) {
  paste(
    tablero$metrica_instanciada, tablero$entidad, tablero$objeto,
    sep = "\u0001"
  )
}

test_that("dos tablas con la misma columna publican celdas distinguibles", {
  medidas <- .o83_medidas_dos_entidades()
  tablero <- tablero_calidad(medidas)
  expect_equal(nrow(tablero), 2L)
  # El defecto exacto: antes las dos filas eran `(NoNulo, cod)`.
  expect_equal(anyDuplicated(.o83_clave(tablero)), 0L)
  expect_setequal(tablero$entidad, c("t1", "t2"))
  # La etiqueta que lee una persona tambien distingue, en el idioma que la propia
  # funcion ya usaba para la granularidad de tabla.
  expect_setequal(tablero$objeto, c("cod (tabla: t1)", "cod (tabla: t2)"))

  # Y la prueba que importa: cada celda se rehace con las claves que publica.
  for (k in seq_len(nrow(tablero))) {
    propias <- medidas$metrica_instanciada ==
      paste0("NoNuloO83@", tablero$entidad[[k]], ".cod")
    expect_equal(
      tablero$valor[[k]], mean(medidas$resultado[propias] == 1),
      info = paste("la celda de", tablero$entidad[[k]], "no se rehace")
    )
  }
  # El numero que salia de la identidad incompleta -las seis medidas juntas- no
  # es ninguna de las dos celdas.
  expect_false(any(abs(tablero$valor - mean(medidas$resultado == 1)) < 1e-9))
})

test_that("dos especializaciones sobre la misma columna se distinguen", {
  nucleo <- metricas_nucleo()
  dos_digitos <- especializar(
    nucleo$Formato, nombre_especifico = "DosDigitosO83",
    expresion_regular = "^[0-9]{2}$"
  )
  alfabetico <- especializar(
    nucleo$Formato, nombre_especifico = "AlfabeticoO83",
    expresion_regular = "^[a-z]+$"
  )
  medidas <- medir(
    modelo(list(
      instanciar(dos_digitos, "t", "cod"), instanciar(alfabetico, "t", "cod")
    )),
    data.frame(cod = c("12", "ab", NA, "34"), stringsAsFactors = FALSE),
    id_medicion = "o83b"
  )
  tablero <- tablero_calidad(medidas)
  expect_equal(nrow(tablero), 2L)
  # Las dos publican `metrica = "Formato"`: lo que las separa es la instancia.
  expect_equal(unique(tablero$metrica), "Formato")
  expect_equal(anyDuplicated(.o83_clave(tablero)), 0L)
  expect_setequal(
    tablero$metrica_instanciada,
    c("DosDigitosO83@t.cod", "AlfabeticoO83@t.cod")
  )
})

test_that("sobre un agregado la entidad es lo unico que separa las celdas", {
  # Este es el caso que muestra por que `metrica_instanciada` sola no alcanzaba:
  # el agregado renombra la metrica a `agregada:ratio:<especifica>`, igual para
  # todas las filas.
  medidas <- .o83_medidas_dos_entidades()
  tablero <- tablero_calidad(agregar(medidas, "atributo", "ratio"))
  expect_equal(nrow(tablero), 2L)
  expect_equal(length(unique(tablero$metrica_instanciada)), 1L)
  expect_setequal(tablero$entidad, c("t1", "t2"))
  expect_equal(anyDuplicated(.o83_clave(tablero)), 0L)
})

test_that("con una sola entidad la etiqueta no se califica", {
  # El control del lado que no debe cambiar: la calificacion aparece cuando hace
  # falta, no siempre. Sin esto, la prueba de arriba pasaria igual con una
  # etiqueta que siempre arrastra el nombre de la tabla.
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloO83c")
  medidas <- medir(
    modelo(list(
      instanciar(no_nulo, "t", "cod"), instanciar(no_nulo, "t", "otra")
    )),
    data.frame(cod = c(1, NA), otra = c(2, 3)),
    id_medicion = "o83c"
  )
  tablero <- tablero_calidad(medidas)
  expect_setequal(tablero$objeto, c("cod", "otra"))
  expect_equal(unique(tablero$entidad), "t")
})

test_that("el texto de universos sale de los universos que el indice publica", {
  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(list(
      instanciar(especializar(nucleo$NoNulo, "NoNuloO83d"), "t", "cod"),
      instanciar(
        especializar(
          nucleo$Formato, "FormatoO83d", expresion_regular = "^[0-9]{2}$"
        ),
        "t", "cod"
      )
    )),
    data.frame(cod = c("12", "ab", NA, "34"), stringsAsFactors = FALSE),
    id_medicion = "o83d"
  )
  indice <- indice_calidad(
    medidas, pesos = c(Completitud = 0.5, Exactitud = 0.5)
  )
  # El dato: los dos componentes salen del mismo universo.
  expect_equal(unique(indice$componentes$universo), "celdas")
  # Asi que el texto NO puede afirmar lo contrario, que es lo que hacia siempre.
  expect_false(grepl("universos distintos", indice$advertencia_universos))
  expect_match(indice$advertencia_universos, "mismo universo")
  expect_match(indice$advertencia_universos, "celdas")
})

test_that("con universos distintos el texto nombra cada uno y sus componentes", {
  # La otra mitad: la prueba de arriba sola no distingue un texto calculado de un
  # texto fijo que diga siempre "mismo universo".
  componentes <- data.frame(
    componente = c("componente-0001", "componente-0002", "componente-0003"),
    universo = c("celdas", "filas", "celdas"),
    stringsAsFactors = FALSE
  )
  texto <- lupa:::.advertencia_universos(componentes)
  expect_match(texto, "2 universos distintos")
  expect_match(texto, "celdas: componente-0001, componente-0003")
  expect_match(texto, "filas: componente-0002")

  # Y sin componentes no se afirma nada: la rama por la que el texto fijo tambien
  # salia, donde no hay ningun componente del que hablar.
  sin <- lupa:::.advertencia_universos(componentes[0, ])
  expect_false(grepl("universos distintos", sin))
  expect_match(sin, "no se afirma nada")
})

.o83_medicion_armada <- function(granularidad, entidades, objetos, valores,
                                 instanciada, agregacion = NA_character_,
                                 tipo = "booleano") {
  # Se parte de una medicion real y se le cambian los campos: armar el data.frame
  # a mano dejaria fuera columnas que el validador exige, y el objeto tiene que
  # ser el que el paquete acepta.
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloO83e")
  base <- as.data.frame(
    medir(
      modelo(instanciar(no_nulo, "t1", "v")), data.frame(v = c(1, NA)),
      id_medicion = "o83e"
    )
  )[1, , drop = FALSE]
  armada <- base[rep(1L, length(valores)), , drop = FALSE]
  armada$granularidad <- granularidad
  armada$metrica_instanciada <- instanciada
  armada$entidad <- entidades
  armada$objeto_medible <- objetos
  armada$tipo_resultado <- tipo
  armada$resultado <- valores
  armada$agregacion <- agregacion
  armada$atributo <- NA_character_
  armada$fila <- NA_integer_
  armada$id_medida <- paste0("o83e-", seq_along(valores))
  class(armada) <- c("medicion", "data.frame")
  armada
}

test_that("la agrupacion usa la identidad entera, no solo el objeto", {
  # Para las granularidades sin rama propia, la clave era SOLO `objeto_medible`, y
  # la celda publicaba la entidad de la primera fila del grupo: dos entidades con
  # el mismo objeto se fundian en UNA celda con `valor = 0.5` -el promedio de 0 y
  # 1- y la identidad de la segunda no quedaba en ninguna columna. Agrupar por
  # menos de lo que se publica como identidad es fundir dos cosas que el objeto
  # declara distintas.
  armada <- .o83_medicion_armada(
    "conjuntoAtributos", c("padron", "secundario"), "c1, c2", c(0, 1),
    "NoNuloO83e@ca"
  )
  tablero <- as.data.frame(tablero_calidad(armada))
  expect_equal(nrow(tablero), 2L)
  expect_setequal(tablero$entidad, c("padron", "secundario"))
  # Cada celda conserva SU valor: fundirlas publicaba el promedio.
  expect_equal(
    tablero$valor[tablero$entidad == "padron"], 0
  )
  expect_equal(
    tablero$valor[tablero$entidad == "secundario"], 1
  )
  expect_false(any(abs(tablero$valor - 0.5) < 1e-9))
})

test_that("dos celdas con la misma identidad se rechazan nombrando el caso", {
  # La ruta de mediciones YA agregadas convertia cada fila en celda sin verificar
  # nada: dos filas con la misma metrica instanciada, la misma entidad y el mismo
  # objeto salian como dos celdas de identidad identica y valores 0,9 y 0,3, que
  # es el defecto exacto que la documentacion cita como ejemplo.
  armada <- .o83_medicion_armada(
    "coleccion", "padron", "t1, t3", c(0.9, 0.3),
    "agregada:promedio:NoNuloO83e", agregacion = "promedio", tipo = "real"
  )
  expect_error(tablero_calidad(armada), "misma identidad")
  # El mensaje nombra los valores que chocan: sin eso no se sabe cual revisar.
  expect_error(tablero_calidad(armada), "0.9")

  # Control: una sola fila por identidad pasa, asi que la guarda no rechaza la
  # ruta legitima.
  una <- armada[1L, , drop = FALSE]
  class(una) <- c("medicion", "data.frame")
  expect_equal(nrow(as.data.frame(tablero_calidad(una))), 1L)
})
