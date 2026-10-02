# Ronda 22-B: el modelo de calidad -medir(), evaluar(), el historico y la
# deriva-.

.fecha_O124 <- function(dia) as.POSIXct(dia, tz = "UTC")

test_that("con aplicabilidad, toda metrica del paquete cita la fila de la tabla original", {
  nucleo <- metricas_nucleo()
  datos <- data.frame(
    cond = c("no", "si", "no", "si", "no", "si"),
    x = c(5, 5, 7, 7, 50, 200),
    f = as.Date(c("2020-01-01", "2020-01-01", "2030-01-01", "2030-01-01",
                  "2020-01-01", "2030-01-01")),
    y = c("a", "a", "b", "b", "c", "a"), stringsAsFactors = FALSE
  )
  nueva <- function(metrica, atributos, nombre, ...) {
    instanciar(especializar(nucleo[[metrica]], nombre_especifico = nombre, ...),
               "t", atributos)
  }
  instancias <- list(
    nueva("NoNulo", "x", "NN"),
    nueva("AtributoDuplicado", "x", "AD"),
    nueva("ValoresPosiblesPorComprension", "x", "VPC", minimo = 0, maximo = 100),
    nueva("ValoresPosiblesPorExtension", "x", "VPE", valores = c(5, 7)),
    nueva("Formato", "x", "FMT", expresion_regular = "^[0-9]$"),
    nueva("DesactualizacionPorFormato", "x", "DF", expresion_regular = "^[0-9]$"),
    nueva("Escala", "x", "ESC", escala = escala(1)),
    nueva("OportunidadAtributoPorFecha", "f", "OPF",
          fecha_limite = as.Date("2025-01-01")),
    nueva("ConjuntoAtributosDuplicado", c("x", "y"), "CAD"),
    nueva("EntidadDuplicada", "x", "EDk"),
    nueva("DensidadPonderada", c("x", "y"), "DP",
          coeficientes = c(x = 0.5, y = 0.5))
  )
  medicion <- suppressWarnings(medir(
    modelo(instancias), datos,
    aplicabilidad = list(x = ~ cond == "si", f = ~ cond == "si"),
    id_medicion = "r", fecha = .fecha_O124("2026-01-01")
  ))
  # Antes: ocho de las once publicaban las filas 1, 2 y 3 del recorte.
  for (nombre in unique(medicion$metrica_especifica)) {
    filas <- medicion$fila[medicion$metrica_especifica == nombre]
    expect_identical(as.integer(filas), c(2L, 4L, 6L), info = nombre)
    objetos <- medicion$objeto_medible[medicion$metrica_especifica == nombre]
    expect_true(all(grepl("[246][],]", objetos)), info = nombre)
  }
  vpc <- medicion[medicion$metrica_especifica == "VPC", ]
  expect_identical(as.numeric(vpc$resultado), c(1, 1, 0))
  # Y una metrica de varias columnas no depende del orden de sus atributos.
  datos_b <- data.frame(a = c(1, NA, 3, NA, 5), b = c(1, NA, NA, 4, NA),
                        cond = c("si", "no", "si", "no", NA))
  densidad <- function(atributos) {
    m <- suppressWarnings(medir(
      modelo(list(nueva("DensidadPonderada", atributos, "DP",
                        coeficientes = c(a = 0.5, b = 0.5)))),
      datos_b, aplicabilidad = list(b = ~ cond == "si"),
      id_medicion = "r", fecha = .fecha_O124("2026-01-01")
    ))
    m$fila
  }
  expect_identical(densidad(c("a", "b")), densidad(c("b", "a")))
})

.medicion_O124 <- function(edad, id, fecha) {
  instancias <- list(
    instanciar(especializar(metricas_nucleo()$NoNulo, nombre_especifico = "NNe"),
               "t", "edad"),
    instanciar(especializar(metricas_nucleo()$ErrorEstandar, nombre_especifico = "EE"),
               "t", "edad")
  )
  suppressWarnings(medir(modelo(instancias), list(t = data.frame(edad = edad)),
                         id_medicion = id, fecha = .fecha_O124(fecha)))
}

test_that("[, subset() y rbind() conservan lo que la medicion y el historico declaran", {
  medicion_a <- .medicion_O124(c(20, NA, 35, 50), "A", "2026-01-01")
  # B sin dos valores numericos: ErrorEstandar no se midio.
  medicion_b <- .medicion_O124(c(20, NA, NA, NA), "B", "2026-02-01")
  perfil <- perfil_evaluacion(
    "P", regla_evaluacion("R_e", function(x) x > 0.5, metricas = "NNe@t.edad"),
    regla_evaluacion("R_ee", function(x) x < 100, metricas = "EE@t.edad")
  )
  perfil_b <- function(medicion) {
    evaluacion <- suppressWarnings(evaluar(medicion, perfil))
    evaluacion$perfiles$resultado[evaluacion$perfiles$id_medicion == "B"]
  }
  # Antes: rbind(A, B) daba 0.625 -la regla sin medidas fuera del promedio- y
  # rbind(B, A) NA; y subset() abortaba con un mensaje que no nombraba el problema.
  expect_true(is.na(perfil_b(medicion_b)))
  expect_true(is.na(perfil_b(rbind(medicion_a, medicion_b))))
  expect_true(is.na(perfil_b(rbind(medicion_b, medicion_a))))
  expect_true(is.na(perfil_b(subset(medicion_b, TRUE))))
  expect_true(is.na(perfil_b(medicion_b[, names(medicion_b)])))
  # Dos modelos distintos no se juntan en una medicion.
  otro <- suppressWarnings(medir(
    modelo(list(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "edad"))),
    list(t = data.frame(edad = 1:3)), id_medicion = "C",
    fecha = .fecha_O124("2026-03-01")
  ))
  expect_error(rbind(medicion_a, otro), "modelos o universos distintos")
  # El historico: rbind() acumula, y un filtro conserva la configuracion.
  historico_a <- historico_calidad(medicion_a)
  historico_b <- historico_calidad(medicion_b)
  unido <- rbind(historico_a, historico_b)
  expect_identical(
    sort(unique(attr(unido, "configuracion_evaluacion")$id_medicion)), c("A", "B")
  )
  filtrado <- subset(unido, id_medicion == "A")
  expect_identical(attr(filtrado, "configuracion_evaluacion")$id_medicion, "A")
  # Y volver a acumular la corrida quitada no choca con su configuracion vieja.
  expect_no_error(acumular_historico(unido[unido$id_medicion == "A", ], medicion_b))
})

test_that("la supresion declarada sobrevive a acumular la medicion despues", {
  instancias <- list(
    instanciar(especializar(metricas_nucleo()$NoNulo, nombre_especifico = "NNe"),
               "t", "edad")
  )
  medicion <- medir(modelo(instancias), list(t = data.frame(edad = c(20, NA, 135, 40))),
                    id_medicion = "A", fecha = .fecha_O124("2026-01-01"))
  evaluacion <- evaluar(medicion, perfil_evaluacion("P", regla_evaluacion(
    "Publicable", function(x) x > 0.5, desenlace = "suprimir"
  )))
  suprimidas <- evaluacion$desenlaces$id_medida
  expect_length(suprimidas, 1L)
  publica <- function(historico) {
    filas <- historico[historico$nivel == "medida" &
                         historico$id_medida %in% suprimidas, ]
    sum(!is.na(filas$resultado))
  }
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_historico(historico_calidad(evaluacion), archivo)
  # Antes: los dos ultimos caminos publicaban la medida suprimida con su valor.
  expect_identical(publica(historico_calidad(medicion, evaluacion)), 0L)
  expect_identical(publica(historico_calidad(evaluacion, medicion)), 0L)
  expect_identical(publica(acumular_historico(historico_calidad(evaluacion), medicion)), 0L)
  expect_identical(publica(acumular_historico(leer_historico(archivo), medicion)), 0L)
})

test_that("AtributoDuplicado marca todas las apariciones tambien en integer64", {
  skip_if_not_installed("bit64")
  instancia <- instanciar(especializar(metricas_nucleo()$AtributoDuplicado), "t", "id")
  valores <- c(7, 3, 3, 1e5, 2e6, 99999)
  doble <- medir(modelo(list(instancia)), data.frame(id = valores))$resultado
  entero64 <- medir(modelo(list(instancia)),
                    data.frame(id = bit64::as.integer64(valores)))$resultado
  # Antes: 0 0 1 0 0 0 -solo la segunda aparicion-.
  expect_identical(as.numeric(entero64), c(0, 1, 1, 0, 0, 0))
  expect_identical(as.numeric(entero64), as.numeric(doble))
})

test_that("los conjuntos declarados emparejan integer64 con un doble por su valor", {
  skip_if_not_installed("bit64")
  datos <- data.frame(id = bit64::as.integer64(c(7, 3, 3, 1e5, 2e6, 99999)))
  medida <- function(metrica, ...) {
    instancia <- instanciar(especializar(metricas_nucleo()[[metrica]], ...), "t", "id")
    as.numeric(medir(modelo(list(instancia)), datos)$resultado)
  }
  # Antes: 1e5 y 2e6 no se encontraban en la columna integer64.
  expect_identical(medida("NoNulo", valores_nulos = 1e5), c(1, 1, 1, 0, 1, 1))
  expect_identical(medida("ValoresPosiblesPorExtension", valores = c(3, 1e5, 2e6)),
                   c(0, 1, 1, 1, 1, 0))
  referencia <- data.frame(pk = c(1e5, 2e5, 3e5))
  dependiente <- data.frame(fk = bit64::as.integer64(c(1e5, 2e5, 4e5)))
  regla <- instanciar(especializar(metricas_nucleo()$ReglaIntegridadInterEntidad),
                      c("r", "d"), c("pk", "fk"))
  cobertura <- medir(modelo(list(regla)), list(r = referencia, d = dependiente))$resultado
  expect_equal(as.numeric(cobertura), 2 / 3)
})

test_that("ErrorEstandar no descarta un infinito en silencio", {
  instancia <- instanciar(especializar(metricas_nucleo()$ErrorEstandar), "t", "x")
  medicion <- suppressWarnings(medir(
    modelo(list(instancia,
                instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x"))),
    data.frame(x = c(1, 2, Inf, NA))
  ))
  cobertura <- attr(medicion, "cobertura_metricas")
  # Antes: 0.7071 -la dispersion de 1 y 2-.
  expect_false(any(grepl("ErrorEstandar", medicion$metrica_instanciada)))
  expect_true(any(grepl("no finitos", cobertura$motivo, fixed = TRUE)))
})

test_that("evaluar mira la cobertura de cada corrida", {
  medicion_a <- .medicion_O124(c(20, NA, 35, 50), "A", "2026-01-01")
  medicion_b <- .medicion_O124(c(20, NA, NA, NA), "B", "2026-02-01")
  perfil <- perfil_evaluacion(
    "P", regla_evaluacion("R_ee", function(x) x < 100, metricas = "EE@t.edad"),
    regla_evaluacion("R_e", function(x) x > 0.5, metricas = "NNe@t.edad")
  )
  avisos <- character()
  evaluacion <- withCallingHandlers(
    evaluar(rbind(medicion_a, medicion_b), perfil),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # Antes: unida a A, la corrida B no avisaba que R_ee no tiene medidas.
  expect_true(any(grepl("R_ee sin EE@t.edad", avisos, fixed = TRUE)))
  cobertura <- attr(evaluacion, "cobertura_reglas")
  expect_identical(cobertura$id_medicion, "B")
})

test_that("un historico exportado a CSV conserva sus horas y su deriva", {
  historico <- do.call(historico_calidad, lapply(
    list(c("C", "2026-02-01 00:00:00"), c("B", "2026-03-01 09:00:00"),
         c("A", "2026-03-01 15:00:00")),
    function(par) {
      medicion <- .medicion_O124(
        switch(par[[1L]], C = c(1, 2, 3, 4), B = c(1, 2, 3, NA), A = c(1, NA, NA, NA)),
        par[[1L]], par[[2L]]
      )
      suppressWarnings(evaluar(medicion, perfil_evaluacion(
        "P", regla_evaluacion("R", function(x) x > 0.5, metricas = "NNe@t.edad")
      )))
    }
  ))
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  utils::write.csv(historico, archivo, row.names = FALSE)
  leido <- utils::read.csv(archivo, stringsAsFactors = FALSE)
  orden <- function(h) {
    deriva <- as.data.frame(detectar_deriva_calidad(h))
    paste(deriva$id_medicion_anterior, deriva$id_medicion_actual)
  }
  # Antes: el CSV perdia las horas y la deriva iba de A a B, al reves.
  expect_identical(orden(leido), orden(historico))
  expect_identical(orden(leido), c("C B", "B A"))
})

test_that("una fecha de texto se lee en UTC en cualquier sesion", {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x")
  fecha_en <- function(huso) {
    viejo <- Sys.getenv("TZ", unset = NA)
    on.exit(if (is.na(viejo)) Sys.unsetenv("TZ") else Sys.setenv(TZ = viejo))
    Sys.setenv(TZ = huso)
    as.numeric(medir(modelo(list(instancia)), data.frame(x = 1:2),
                     id_medicion = "r", fecha = "2026-03-01")$fecha[[1L]])
  }
  # Antes: 03:00 UTC con el huso de Montevideo y 00:00 con UTC.
  expect_identical(fecha_en("America/Montevideo"), fecha_en("UTC"))
  expect_identical(fecha_en("UTC"), as.numeric(.fecha_O124("2026-03-01")))
})

test_that("el resumen por regla no funde nombres que se pegan con un punto", {
  medida <- function(id, valores) {
    medir(modelo(list(instanciar(especializar(metricas_nucleo()$NoNulo,
                                              nombre_especifico = "NN"), "t", "x"))),
          data.frame(x = valores), id_medicion = id,
          fecha = .fecha_O124(if (id == "x") "2026-01-01" else "2026-02-01"))
  }
  medicion <- rbind(medida("x", c(1, NA, NA, NA)), medida("x.P", c(1, 2, 3, 4)))
  evaluacion <- evaluar(medicion, perfil_evaluacion(
    "P", regla_evaluacion("P.y", function(v) v > 0.5),
    regla_evaluacion("y", function(v) v > 0.5)
  ))
  # Antes: "x" + "P.y" y "x.P" + "y" daban la misma clave, y se fundian.
  expect_identical(nrow(evaluacion$reglas), 4L)
  expect_true(all(evaluacion$reglas$n_medidas == 4L))
})

test_that("evaluar rechaza una medida repetida o sin valor nombrandola", {
  medicion <- .medicion_O124(c(20, NA, 35, 50), "A", "2026-01-01")
  perfil <- perfil_evaluacion("P", regla_evaluacion("R", function(x) x > 0.5,
                                                    metricas = "NNe@t.edad"))
  # Antes: el doble de medidas y de desenlaces.
  expect_error(evaluar(rbind(medicion, medicion), perfil), "repite la medida")
  con_na <- medicion
  con_na$resultado[[1L]] <- NA
  # Antes: "no respetan su tipo declarado".
  expect_error(evaluar(con_na, perfil), "resultado` NA")
})
