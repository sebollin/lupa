test_that("la proximidad protege candidatos personales por omision", {
  skip_if_not_installed("stringdist")
  padron <- referencial(
    data.frame(
      dni = c("55555555", "66666666"),
      nombre = c("ALFA BETA", "GAMMA DELTA"),
      stringsAsFactors = FALSE
    ),
    clave = "dni", valor = "nombre"
  )
  metrica <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemDebil),
    "ventas", c("dni", "nombre"), referencial = padron
  )
  medicion <- medir(
    modelo(metrica),
    data.frame(dni = "55555555", nombre = "ALFA BETX",
               stringsAsFactors = FALSE),
    id_medicion = "proteccion"
  )

  objeto <- medicion$objeto_medible[[1L]]
  expect_match(objeto, "candidato_referencial=\\[valor protegido\\]")
  expect_match(objeto, "distancia=")
  expect_false(grepl("55555555|ALFA BETA", objeto))
  historico <- historico_calidad(medicion)
  expect_identical(historico$objeto_medible[[1L]], objeto)

  visible <- medir(
    modelo(metrica),
    data.frame(dni = "55555555", nombre = "ALFA BETX",
               stringsAsFactors = FALSE),
    id_medicion = "sin-proteccion", proteger_datos_personales = FALSE
  )
  expect_match(visible$objeto_medible[[1L]], "55555555", fixed = TRUE)
  expect_match(visible$objeto_medible[[1L]], "ALFA BETA", fixed = TRUE)
})

test_that("la evidencia referencial no personal conserva su candidato", {
  skip_if_not_installed("stringdist")
  referencia <- referencial(
    data.frame(departamento = c("Montevideo", "Canelones")),
    clave = "departamento"
  )
  metrica <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte),
    "ventas", "departamento", referencial = referencia
  )
  medicion <- medir(
    modelo(metrica), data.frame(departamento = "Montevido"),
    id_medicion = "evidencia-util"
  )
  expect_match(medicion$objeto_medible[[1L]], "Montevideo", fixed = TRUE)
  expect_match(medicion$objeto_medible[[1L]], "distancia=")
})

test_that("una metrica sin valores queda en cobertura y no produce un uno", {
  datos <- data.frame(
    dni = c("1", "2", "3", "4"),
    email = rep(NA_character_, 4L),
    stringsAsFactors = FALSE
  )
  nucleo <- metricas_nucleo()
  m1 <- instanciar(especializar(nucleo$NoNulo), "clientes", "dni")
  m2 <- instanciar(
    especializar(nucleo$Formato, expresion_regular = "^[^@]+@[^@]+$"),
    "clientes", "email"
  )
  medicion <- medir(
    modelo(m1, m2), datos, id_medicion = "cobertura",
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_equal(nrow(medicion), 4L)
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_true(is.data.frame(cobertura))
  expect_equal(cobertura$metrica_instanciada, m2$nombre)
  expect_equal(cobertura$estado, "sin_valores")
  expect_match(cobertura$motivo, "no se pudo medir", ignore.case = TRUE)
  expect_match(cobertura$motivo, "sin valores no nulos", ignore.case = TRUE)
  historico_medicion <- historico_calidad(medicion)
  expect_equal(
    sum(historico_medicion$nivel == "metrica_no_evaluada"), 1L
  )

  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("Todo medido pasa", function(x) x >= 1)
  )
  evaluacion <- evaluar(medicion, perfil)
  expect_true(is.na(evaluacion$reglas$resultado[[1L]]))
  expect_true(is.na(evaluacion$reglas$n_medidas[[1L]]))
  expect_true(is.na(evaluacion$perfiles$resultado[[1L]]))
  expect_true(is.na(evaluacion$perfiles$n_reglas[[1L]]))
  expect_identical(
    attr(evaluacion, "cobertura_metricas", exact = TRUE), cobertura
  )
  historico <- historico_calidad(evaluacion)
  no_evaluada <- historico[historico$nivel == "metrica_no_evaluada", ,
                            drop = FALSE]
  expect_equal(nrow(no_evaluada), 1L)
  expect_match(no_evaluada$objeto_medible, "no se pudo medir", ignore.case = TRUE)

  tablero <- tablero_calidad(medicion)
  indice <- indice_calidad(tablero, pesos = c(Completitud = 1))
  expect_equal(
    attr(tablero, "cobertura_metricas", exact = TRUE)$metrica_instanciada,
    m2$nombre
  )
  expect_equal(indice$cobertura_metricas$metrica_instanciada, m2$nombre)
  expect_match(indice$cobertura$metricas_no_medidas, m2$nombre, fixed = TRUE)
  salida_tablero <- testthat::capture_messages(
    capture.output(print(tablero))
  )
  expect_match(paste(salida_tablero, collapse = "\n"),
               "Cobertura de m\u00e9tricas", fixed = TRUE)
})

test_that("una clave foranea sin filas dependientes queda en cobertura explicita", {
  nucleo <- metricas_nucleo()
  metrica <- instanciar(
    especializar(nucleo$ReglaIntegridadInterEntidad),
    c("clientes", "ventas"), c("id", "cliente_id")
  )
  medicion <- medir(
    modelo(metrica),
    list(clientes = data.frame(id = 1), ventas = data.frame(cliente_id = character())),
    id_medicion = "fk-vacio"
  )
  expect_equal(nrow(medicion), 0L)
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  expect_equal(cobertura$estado, "sin_valores")
  expect_match(cobertura$motivo, "dependiente `ventas`.*cero filas")
})

# Los perfiles de madurez de fabrica ignoraban la orientacion de la medida, y la
# regla quedaba al reves justo donde importa: una tabla 100 % duplicada da
# `EntidadDuplicada = 1`, y `Resultado > 0.5` la daba por CUMPLIDA en los tres
# perfiles mientras la tabla limpia -`0`- no cumplia ninguno.
#
# El mecanismo para consultarla ya existia: `regla_evaluacion()` documenta que la
# condicion puede declarar un segundo argumento `orientacion`. La fabrica no lo
# usaba. La inversion es `1 - valor`, la misma convencion que `tablero_calidad()`
# ya aplica a sus componentes de defecto.
test_that("los perfiles de madurez respetan la orientacion de la medida", {
  nucleo <- metricas_nucleo()

  evaluar_perfil <- function(datos, metrica, ...) {
    instancia <- instanciar(especializar(metrica), "t", ...)
    medida <- medir(modelo(instancia), datos, id_medicion = "x")
    list(
      medida = medida,
      resultado = evaluar(
        medida, perfiles_madurez(medida$metrica_instanciada)$Basico
      )$perfiles$resultado
    )
  }

  # --- orientacion `defecto`: mas alto es PEOR ---
  duplicada <- evaluar_perfil(
    data.frame(codigo = c("A", "A", "A", "A")), nucleo$EntidadDuplicada
  )
  limpia <- evaluar_perfil(
    data.frame(codigo = c("A", "B", "C", "D")), nucleo$EntidadDuplicada
  )

  # Primera mitad: la orientacion es la que se cree, y las medidas son opuestas.
  expect_equal(unique(duplicada$medida$orientacion), "defecto")
  expect_equal(unique(duplicada$medida$resultado), 1)
  expect_equal(unique(limpia$medida$resultado), 0)

  # La tabla mala NO cumple y la limpia SI. Antes era al reves.
  expect_equal(duplicada$resultado, 0)
  expect_equal(limpia$resultado, 1)

  # --- control: `conformidad` NO se invierte ---
  no_nulo <- evaluar_perfil(data.frame(a = c(1, 2, NA, 4)), nucleo$NoNulo, "a")
  expect_equal(unique(no_nulo$medida$orientacion), "conformidad")
  expect_equal(no_nulo$resultado, 0.75)
})

test_that("un perfil de madurez no juzga una metrica no acotada", {
  # Una metrica `no_aplica` es, por definicion del paquete, no acotada. Un
  # umbral en [0, 1] no puede juzgarla: `30 > 0.5` es cierto y no significa
  # nada, y antes esta fabrica devolvia "cumple" para 30, 60 y 90 dias de
  # atraso por igual. Ahora para, nombrando el porque.
  atraso <- metrica(
    "AtrasoDias", "atraso en dias", "instanciaAtributo", "duracion",
    dimension = "Frescura", factor = "Actualidad",
    metodo = function(tablas, instancia) {
      entidad <- instancia$entidad[[1L]]
      atributo <- instancia$atributos[[1L]]
      tabla <- .obtener_tabla_modelo(tablas, entidad)
      x <- .obtener_columna_modelo(tabla, atributo, entidad)
      filas <- which(!is.na(x))
      .salida_metodo(x[filas], entidad, atributo, filas,
                     paste0(entidad, "[", filas, ",]"))
    }
  )
  medida <- medir(
    modelo(instanciar(especializar(atraso), "t", "dias")),
    data.frame(dias = c(30, 60, 90)), id_medicion = "dur"
  )

  # Primera mitad: la medida existe, es no acotada y esta declarada como tal.
  expect_equal(unique(medida$orientacion), "no_aplica")
  expect_equal(medida$resultado, c(30, 60, 90))

  expect_error(
    evaluar(medida, perfiles_madurez(medida$metrica_instanciada)$Basico),
    "no acotada", fixed = TRUE
  )
})

# `agregar()` a nivel `coleccion` adjunta `cobertura_coleccion`: las tablas
# declaradas, las que entraron al numero, las que no se midieron y una
# advertencia. `NEWS.md` declara que ese es el unico nivel donde la cobertura
# viaja pegada al numero.
#
# Y moria en el primer consumidor: ni `tablero_calidad()` ni `indice_calidad()`
# la conservaban. Una coleccion de dos tablas con una vacia publica `0,667`
# -que cubre UNA de las dos- sin nada que lo dijera.
test_that("la cobertura de la coleccion llega hasta el tablero y el indice", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "a", data.frame(x = c(1, NA, 3)))
  DBI::dbWriteTable(conexion, "b", data.frame(x = numeric()))

  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x"),
           instanciar(especializar(nucleo$NoNulo), "b", "x")),
    list(a = DBI::dbReadTable(conexion, "a"),
         b = DBI::dbReadTable(conexion, "b"))
  )
  por_coleccion <- agregar(
    agregar(agregar(medidas, "atributo", "ratio"), "entidad", "promedio"),
    "coleccion", "promedio_ponderado",
    coleccion = coleccion(conexion, c("a", "b"), nombre = "c1"), pesos = 1
  )

  # Primera mitad: la cobertura existe y dice lo que se cree, con una tabla
  # declarada que no entro al numero. Sin eso, propagar nada pasaria el test.
  cobertura <- attr(por_coleccion, "cobertura_coleccion", exact = TRUE)
  expect_false(is.null(cobertura))
  expect_equal(cobertura$tablas_declaradas, 2)
  expect_equal(cobertura$tablas_en_el_numero, 1)
  expect_equal(cobertura$tablas_sin_medir, "b")

  tablero <- tablero_calidad(por_coleccion)
  expect_equal(attr(tablero, "cobertura_coleccion", exact = TRUE), cobertura)

  indice <- indice_calidad(por_coleccion, pesos = c(Completitud = 1))
  expect_equal(indice$cobertura_coleccion, cobertura)
  # Y se ve al imprimirlo: un indice es UN numero, y la cobertura que vive solo
  # en un atributo no la lee nadie.
  #
  # `cli` NO pasa por `expect_output()` ni por `capture.output()`. Este proyecto
  # ya se comio dos "no filtra" falsos por no tenerlo en cuenta. Desviar los dos
  # flujos con `sink()` tampoco alcanza: dentro de `testthat` la salida de `cli`
  # queda atrapada como CONDICION antes de llegar a ningun flujo, y el archivo
  # salia vacio -esta prueba fallaba corrida sola y pasaba dentro de la suite-.
  impreso <- salida_cli(try(print(indice), silent = TRUE))
  expect_match(impreso, "Cobertura de la colecci", fixed = TRUE)
  expect_match(impreso, "Sin medir", fixed = TRUE)

  # Control: una corrida sin coleccion no inventa el atributo.
  expect_null(attr(tablero_calidad(medidas), "cobertura_coleccion",
                   exact = TRUE))
})

# `medir()` declara en `cobertura_metricas` las metricas que NO se pudieron
# medir y por que -"la entidad dependiente `b` tiene cero filas"-, y `agregar()`
# lo descartaba en el primer salto: solo copiaba `configuracion_modelo` y
# `configuracion_aplicabilidad`. De ahi en mas esa tabla era invisible y el
# conjunto se reportaba como si nunca hubiera existido.
test_that("la cobertura de metricas sobrevive a la agregacion", {
  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x"),
           instanciar(especializar(nucleo$NoNulo), "b", "x")),
    list(a = data.frame(x = c(1, NA, 3)), b = data.frame(x = numeric()))
  )

  # Primera mitad: hay una metrica no medida y esta declarada con su motivo.
  cobertura <- attr(medidas, "cobertura_metricas", exact = TRUE)
  expect_true(inherits(cobertura, "data.frame") && nrow(cobertura) >= 1L)
  expect_match(paste(cobertura$motivo, collapse = " "), "cero filas",
               fixed = TRUE)

  por_atributo <- agregar(medidas, "atributo", "ratio")
  por_entidad <- agregar(por_atributo, "entidad", "promedio")
  expect_equal(attr(por_atributo, "cobertura_metricas", exact = TRUE), cobertura)
  expect_equal(attr(por_entidad, "cobertura_metricas", exact = TRUE), cobertura)
  expect_false(is.null(attr(tablero_calidad(por_entidad), "cobertura_metricas",
                            exact = TRUE)))

  # Control: una corrida donde todo se pudo medir no inventa la cobertura.
  completa <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x")),
    list(a = data.frame(x = c(1, NA, 3)))
  )
  expect_null(attr(agregar(completa, "atributo", "ratio"),
                   "cobertura_metricas", exact = TRUE))
})

# El informe es la salida que MAS LEJOS llega: es la que se comparte. Y no
# publicaba ninguna de las coberturas que el objeto trae, asi que una medicion
# donde una tabla tenia cero filas producia un informe identico al de una donde
# todo se midio.
test_that("el informe publica las coberturas que el objeto trae", {
  nucleo <- metricas_nucleo()
  medidas <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x"),
           instanciar(especializar(nucleo$NoNulo), "b", "x")),
    list(a = data.frame(x = c(1, NA, 3)), b = data.frame(x = numeric()))
  )

  # Primera mitad: la cobertura existe y dice por que no se pudo medir.
  cobertura <- attr(medidas, "cobertura_metricas", exact = TRUE)
  expect_true(inherits(cobertura, "data.frame") && nrow(cobertura) >= 1L)

  leer <- function(objeto) {
    archivo <- tempfile(fileext = ".html")
    on.exit(unlink(archivo), add = TRUE)
    invisible(reportar(objeto, archivo = archivo))
    paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  }

  expect_true(grepl("cero filas", leer(medidas), fixed = TRUE))
  evaluacion <- evaluar(
    medidas, perfiles_madurez(medidas$metrica_instanciada)$Basico
  )
  expect_true(grepl("cero filas", leer(evaluacion), fixed = TRUE))

  # Control: una corrida donde todo se midio no inventa la seccion. Sin esta
  # mitad, imprimir siempre el encabezado pasaria el test.
  completa <- medir(
    modelo(instanciar(especializar(nucleo$NoNulo), "a", "x")),
    list(a = data.frame(x = c(1, NA, 3)))
  )
  expect_false(grepl("Cobertura de m", leer(completa), fixed = TRUE))
})
