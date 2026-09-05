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
