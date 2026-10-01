# Lo que una medicion declara y su agregado no puede tirar.
#
# `agregar()` arrastraba los atributos de `medir()` con una lista escrita a mano,
# y la lista fue la guarda: se perdio `cobertura_metricas`, se arreglo, y en la
# misma lista quedaron afuera `alcance_medidas` y `fecha_declarada`. El segundo
# no es una etiqueta: sin el, la guarda del orden temporal se apaga y
# `comparar_evaluaciones()` publica el delta con el signo al reves.
#
# Asi que la prueba central de este archivo NO es "estos dos atributos viajan":
# es un RECORRIDO sobre los atributos que `medir()` pone de verdad, que exige que
# cada uno este decidido. Un atributo nuevo no puede volver a perderse callado.

.o82_metrica_tipo <- function(tipo, nombre = paste0("M", tipo)) {
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    filas <- seq_along(x)
    data.frame(
      resultado = as.numeric(x), entidad = instancia$entidad,
      atributo = instancia$atributos, fila = filas,
      objeto = paste0(
        instancia$entidad, "$", instancia$atributos, "[", filas, "]"
      ),
      stringsAsFactors = FALSE
    )
  }
  metrica(
    nombre, "Metrica de prueba.", "instanciaAtributo", tipo, metodo = metodo,
    dimension = "Frescura", factor = "Actualidad", orientacion = "no_aplica"
  )
}

.o82_medicion <- function(fecha = NULL, valores = c("12", "ab", NA, "34")) {
  nucleo <- metricas_nucleo()
  formato <- especializar(
    nucleo$Formato, nombre_especifico = "FormatoO82",
    expresion_regular = "^[0-9]{2}$"
  )
  modelo_o82 <- modelo(instanciar(formato, "t", "cod"))
  datos <- data.frame(cod = valores, stringsAsFactors = FALSE)
  if (is.null(fecha)) {
    medir(modelo_o82, datos, id_medicion = "o82")
  } else {
    medir(modelo_o82, datos, id_medicion = "o82", fecha = fecha)
  }
}

test_that("todo atributo que pone medir() esta decidido en agregar()", {
  # El recorrido: no se revisa un atributo, se enumeran TODOS los que la
  # medicion pone en varias configuraciones, y cada uno tiene que estar en la
  # lista de los que viajan o en la de los que a proposito no viajan.
  configuraciones <- list(
    sin_fecha = .o82_medicion(),
    con_fecha = .o82_medicion(fecha = as.POSIXct("2026-01-01", tz = "UTC")),
    # Una columna sin ningun valor manda la metrica a `cobertura_metricas`, que
    # es el atributo cuya perdida se arreglo antes que las otras dos.
    sin_valores = .o82_medicion(valores = c(NA, NA))
  )
  decididos <- c(
    lupa:::.ATRIBUTOS_TRASLADADOS_AGREGACION,
    lupa:::.ATRIBUTOS_NO_TRASLADADOS_AGREGACION
  )
  vistos <- character()
  for (nombre in names(configuraciones)) {
    medidas <- configuraciones[[nombre]]
    propios <- setdiff(
      names(attributes(medidas)), c("names", "row.names", "class")
    )
    vistos <- unique(c(vistos, propios))
    expect_equal(
      setdiff(propios, decididos), character(),
      info = paste(
        "la configuracion", nombre, "pone un atributo que agregar() no decide"
      )
    )
  }
  # Y el recorrido cuenta lo que ejercio: un recorrido que no toco ningun
  # atributo pasaria igual y no habria medido nada.
  expect_gte(length(vistos), 4L)
  expect_true(all(
    c("alcance_medidas", "fecha_declarada", "configuracion_modelo") %in% vistos
  ))
  # Las dos listas no se pisan: un atributo no puede viajar y no viajar.
  expect_equal(
    intersect(
      lupa:::.ATRIBUTOS_TRASLADADOS_AGREGACION,
      lupa:::.ATRIBUTOS_NO_TRASLADADOS_AGREGACION
    ),
    character()
  )
})

test_that("el alcance de las medidas llega al agregado y al tablero", {
  medidas <- .o82_medicion()
  alcance <- attr(medidas, "alcance_medidas", exact = TRUE)
  expect_equal(alcance$medidas, 3)
  expect_equal(alcance$en_el_universo, 4)

  agregado <- agregar(medidas, "atributo", "ratio")
  arrastrado <- attr(agregado, "alcance_medidas", exact = TRUE)
  expect_false(is.null(arrastrado))
  # Y dice lo mismo: no se recalcula ni se resume, son las medidas que entraron.
  expect_equal(arrastrado$medidas, alcance$medidas)
  expect_equal(arrastrado$en_el_universo, alcance$en_el_universo)
  # La clave que apareja con el agregado tambien viaja, porque sin ella el "3 de
  # 4" no se puede atribuir a ninguna fila.
  expect_true(all(
    arrastrado$metrica_instanciada %in% agregado$metrica_instanciada
  ))

  expect_false(is.null(
    attr(tablero_calidad(agregado), "alcance_medidas", exact = TRUE)
  ))
})

test_that("la guarda del orden temporal dispara tambien por el agregado", {
  # La mitad que importa es la que FALLA: una guarda que solo se vio dar OK no se
  # distingue de una que no mide. Asi que se construye el caso invertido y se
  # exige el error por los dos caminos.
  nucleo <- metricas_nucleo()
  formato <- especializar(
    nucleo$Formato, nombre_especifico = "FormatoO82b",
    expresion_regular = "^[0-9]{2}$"
  )
  modelo_o82 <- modelo(instanciar(formato, "t", "cod"))
  perfil <- perfil_evaluacion(
    "Basico", regla_evaluacion("Presente", function(x) x >= 0.9)
  )
  medir_en <- function(valores, fecha, id) {
    medir(
      modelo_o82, data.frame(cod = valores, stringsAsFactors = FALSE),
      id_medicion = id, fecha = fecha
    )
  }
  viejo <- medir_en(
    c("12", "34"), as.POSIXct("2026-03-01", tz = "UTC"), "o82_viejo"
  )
  nuevo <- medir_en(
    c("ab", "cd"), as.POSIXct("2026-01-01", tz = "UTC"), "o82_nuevo"
  )
  evaluado <- function(x) suppressWarnings(evaluar(x, perfil))

  expect_error(
    comparar_evaluaciones(evaluado(viejo), evaluado(nuevo)),
    "fechado despu"
  )
  agregado_viejo <- agregar(viejo, "atributo", "ratio")
  agregado_nuevo <- agregar(nuevo, "atributo", "ratio")
  expect_true(isTRUE(attr(agregado_viejo, "fecha_declarada", exact = TRUE)))
  expect_error(
    comparar_evaluaciones(evaluado(agregado_viejo), evaluado(agregado_nuevo)),
    "fechado despu"
  )
  # Y la otra mitad: en el orden correcto la comparacion sale, para que la guarda
  # no este simplemente rechazando todo.
  comparado <- comparar_evaluaciones(
    evaluado(agregado_nuevo), evaluado(agregado_viejo)
  )
  expect_equal(nrow(comparado), 1L)
})

test_that("las dos agregaciones promediadoras rechazan los tipos no acotados", {
  # La guarda que existia era por VALOR -"deben estar en [0, 1]"- y se leia como
  # si fuera por tipo. Con los valores dentro de [0, 1] la guarda por valor no
  # tapa nada y se ve cual es la que falta.
  esperado <- list(
    duracion = c(0.25, 0.75), numero_real = c(0.25, 0.75), entero = c(1, 0)
  )
  for (tipo in names(esperado)) {
    instancia <- instanciar(
      especializar(
        .o82_metrica_tipo(tipo, paste0("O82", tipo)),
        paste0("O82esp", tipo)
      ),
      "t", "v"
    )
    medidas <- medir(
      modelo(instancia), data.frame(v = esperado[[tipo]]),
      id_medicion = paste0("o82", tipo)
    )
    expect_equal(unique(medidas$tipo_resultado), tipo)
    for (funcion in c("promedio", "promedio_ponderado")) {
      expect_error(
        agregar(medidas, "atributo", funcion),
        "no admite m",
        info = paste(funcion, "acepto el tipo", tipo)
      )
    }
    # El mensaje nombra la metrica que lo viola: sin eso, en un modelo de treinta
    # instancias no se sabe cual hay que cambiar.
    expect_error(
      agregar(medidas, "atributo", "promedio"),
      paste0("O82esp", tipo)
    )
  }
})

test_that("las dos que si estan acotadas siguen pasando", {
  # El control del lado que la regla no debe tocar: sin esto una guarda que
  # rechaza todo pasaria la prueba de arriba.
  medidas <- .o82_medicion()
  expect_equal(unique(medidas$tipo_resultado), "booleano")
  expect_equal(agregar(medidas, "atributo", "promedio")$resultado, 2 / 3)

  instancia <- instanciar(
    especializar(.o82_metrica_tipo("real", "O82real"), "O82espreal"), "t", "v"
  )
  reales <- medir(
    modelo(instancia), data.frame(v = c(0.25, 0.75)), id_medicion = "o82real"
  )
  expect_equal(agregar(reales, "atributo", "promedio")$resultado, 0.5)
})

test_that("el peso cero se declara sin depender del destino y los pesos viajan", {
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloO82")
  medidas <- medir(
    modelo(list(
      instanciar(no_nulo, "t1", "a"), instanciar(no_nulo, "t2", "b")
    )),
    list(t1 = data.frame(a = c(1, 2)), t2 = data.frame(b = NA_real_)),
    id_medicion = "o82pesos"
  )
  por_entidad <- agregar(
    agregar(medidas, "atributo", "ratio"), "entidad", "promedio"
  )
  conjunto <- agregar(
    por_entidad, "conjuntoEntidades", "promedio_ponderado",
    pesos = c(t1 = 0, t2 = 1)
  )
  # `conjuntoEntidades` no tiene atributo de cobertura de frontera donde colgar
  # la declaracion, y por eso el peso cero se callaba justo aca.
  expect_equal(
    attr(conjunto, "partes_con_peso_cero", exact = TRUE), "t1"
  )
  pesos <- attr(conjunto, "pesos_declarados", exact = TRUE)
  expect_false(is.null(pesos))
  # La etiqueta vuelve al valor: sin nombres, un peso publicado no dice de que
  # parte es y el numero no se puede rehacer.
  expect_setequal(names(pesos), c("t1", "t2"))
  expect_equal(unname(pesos[["t1"]]), 0)
  rehecho <- sum(
    por_entidad$resultado * pesos[as.character(por_entidad$entidad)]
  )
  expect_equal(unname(rehecho), conjunto$resultado)

  # Y sin ningun peso cero no se declara nada: la declaracion tiene que
  # distinguir el caso, no aparecer siempre.
  con_pesos <- agregar(
    por_entidad, "conjuntoEntidades", "promedio_ponderado",
    pesos = c(t1 = 0.5, t2 = 0.5)
  )
  expect_null(attr(con_pesos, "partes_con_peso_cero", exact = TRUE))
  expect_false(is.null(attr(con_pesos, "pesos_declarados", exact = TRUE)))
})

test_that("el alcance se reexpresa en la clave del agregado y se suma por nivel", {
  # El defecto que esta prueba fija lo encontro la prueba de arriba: el alcance
  # arrastrado tal cual estaba indexado por `Formato@t.cod`, y el agregado
  # renombra la metrica a `agregada:ratio:Formato`. La tabla era cierta sobre una
  # clave que no aparecia en ninguna fila del objeto.
  nucleo <- metricas_nucleo()
  formato <- especializar(
    nucleo$Formato, nombre_especifico = "FormatoO82c",
    expresion_regular = "^[0-9]{2}$"
  )
  medidas <- medir(
    modelo(
      instanciar(formato, "t", "cod"), instanciar(formato, "t", "cod2")
    ),
    data.frame(
      cod = c("12", "ab", NA, "34"), cod2 = c("11", "22", "33", NA),
      stringsAsFactors = FALSE
    ),
    id_medicion = "o82c"
  )
  por_atributo <- agregar(medidas, "atributo", "ratio")
  alcance <- attr(por_atributo, "alcance_medidas", exact = TRUE)
  expect_true(all(
    alcance$metrica_instanciada %in% por_atributo$metrica_instanciada
  ))
  # Cada fila del alcance nombra la columna a la que pertenece: son dos, y cada
  # una midio 3 de 4.
  expect_setequal(alcance$atributo, c("cod", "cod2"))
  expect_equal(sort(alcance$medidas), c(3, 3))
  expect_equal(unique(alcance$en_el_universo), 4)

  # Un nivel mas arriba las dos instancias caen en el mismo grupo y los conteos
  # se SUMAN, en la unidad que las dos comparten.
  por_entidad <- agregar(por_atributo, "entidad", "promedio")
  alcance_entidad <- attr(por_entidad, "alcance_medidas", exact = TRUE)
  expect_equal(nrow(alcance_entidad), 1L)
  expect_equal(alcance_entidad$medidas, 6)
  expect_equal(alcance_entidad$en_el_universo, 8)
  expect_equal(alcance_entidad$unidad, "celda")
  expect_true(all(
    alcance_entidad$metrica_instanciada %in% por_entidad$metrica_instanciada
  ))
})

test_that("con unidades distintas en un grupo el alcance declara y no suma", {
  # Esta rama se ejercita contra la funcion interna a proposito: por la via
  # publica las medidas de un grupo comparten `metrica_especifica` y hoy eso
  # implica una sola unidad, asi que la rama no se alcanza desde afuera. Sin esta
  # prueba seria una rama que nunca disparo, y una rama que nunca disparo puede
  # esconder una cifra equivocada y no solo una decision.
  alcance <- data.frame(
    metrica_instanciada = c("M@t.a", "M@t.b"),
    entidad = c("t", "t"), atributo = c("a", "b"),
    unidad = c("celda", "fila"),
    en_el_universo = c(4, 10), medidas = c(3, 9),
    motivo = c("x", "y"), stringsAsFactors = FALSE
  )
  # Con `entidad`: el alcance se empareja por el par metrica-entidad desde la
  # ronda 11, y las medidas reales siempre la traen.
  medidas <- data.frame(
    metrica_instanciada = c("M@t.a", "M@t.b"), entidad = c("t", "t"),
    stringsAsFactors = FALSE
  )
  resultado <- data.frame(
    metrica_instanciada = "agregada:promedio:M", entidad = "t",
    atributo = NA_character_, stringsAsFactors = FALSE
  )
  reexpresado <- lupa:::.alcance_agregado(
    alcance, medidas, resultado, list(1:2)
  )
  expect_equal(nrow(reexpresado), 1L)
  # Lo que NO tiene que pasar es publicar 12 de 14: ese numero no esta en ninguna
  # unidad.
  expect_true(is.na(reexpresado$medidas))
  expect_true(is.na(reexpresado$en_el_universo))
  expect_true(is.na(reexpresado$unidad))
  expect_match(reexpresado$motivo, "unidades distintas")
  expect_match(reexpresado$motivo, "celda")
  expect_match(reexpresado$motivo, "fila")
})

test_that("los pesos por posicion publican una etiqueta que vuelve a su parte", {
  # La via posicional esta documentada -"sin nombres se leen por posicion"- y su
  # etiqueta caia al defecto `medidas$entidad`. Sobre un origen `instancia*`, donde
  # la misma entidad ocupa varias filas, eso publicaba seis pesos con DOS nombres,
  # y alinear por el nombre publicado -el unico camino que la promesa declara- daba
  # otro numero que el publicado.
  nucleo <- metricas_nucleo()
  no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloO82p")
  medidas <- medir(
    modelo(list(
      instanciar(no_nulo, "t1", "v"), instanciar(no_nulo, "t3", "v")
    )),
    list(t1 = data.frame(v = c(1, NA, 2, 3)), t3 = data.frame(v = c(4, NA))),
    id_medicion = "o82pos"
  )
  por_fila <- agregar(medidas, "instanciaEntidad", "promedio")
  expect_equal(nrow(por_fila), 6L)
  # La entidad se repite: es justo lo que hacia inservible la etiqueta.
  expect_equal(length(unique(as.character(por_fila$entidad))), 2L)

  agregado <- agregar(
    por_fila, "entidad", "promedio_ponderado",
    pesos = c(0.1, 0.2, 0.3, 0.4, 0.6, 0.4)
  )
  pesos <- attr(agregado, "pesos_declarados", exact = TRUE)
  # Una etiqueta por fila, todas distintas.
  expect_length(pesos, 6L)
  expect_equal(length(unique(names(pesos))), 6L)

  # Y la prueba que importa: el numero se rehace con lo que el objeto publica.
  rehecho <- tapply(
    por_fila$resultado * pesos[as.character(por_fila$objeto_medible)],
    as.character(por_fila$entidad), sum
  )
  esperado <- rehecho[as.character(agregado$entidad)]
  expect_equal(unname(as.numeric(esperado)), agregado$resultado)

  # El peso cero nombra la FILA que no aporto, no la entidad que si aporto con
  # sus otras filas.
  con_cero <- agregar(
    por_fila, "entidad", "promedio_ponderado",
    pesos = c(0, 0.25, 0.35, 0.4, 0.6, 0.4)
  )
  sin_peso <- attr(con_cero, "partes_con_peso_cero", exact = TRUE)
  expect_length(sin_peso, 1L)
  expect_false(identical(sin_peso, "t1"))
  expect_true(sin_peso %in% as.character(por_fila$objeto_medible))
})
