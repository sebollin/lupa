# Cada medicion sella, por metrica instanciada, la version de lupa que la midio
# y una huella de su definicion. Comparar dos mediciones lee el sello: sin el,
# la serie de una metrica cuya regla, referencial u orden de texto cambio se
# leia como un cambio de los datos.
#
# La huella tiene que ser estable -entre sesiones, procesos, locales y opciones-
# y cambiar si y solo si cambia la definicion. Las dos mitades se prueban: cada
# parte de la definicion la mueve, y lo que no es definicion no la mueve.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.se_fecha <- as.POSIXct("2026-05-01", tz = "UTC")

.se_datos <- function() {
  data.frame(
    x = c(1, NA, 3, 7, 12),
    t = c("a", "B", "z", "m", NA),
    y = c("si", "no", "si", "si", "no"),
    stringsAsFactors = FALSE
  )
}

# `orden_texto` se pasa solo si se pide: asi una prueba del sello que no lo usa
# falla, sobre una version sin sello, por la falta del sello y no por el
# argumento.
.se_medir <- function(..., aplicabilidad = NULL, orden_texto = NULL,
                      datos = .se_datos(), id = "c", fecha = .se_fecha) {
  argumentos <- list(
    modelo(...), list(t = datos), id_medicion = id, fecha = fecha,
    aplicabilidad = aplicabilidad
  )
  if (!is.null(orden_texto)) argumentos$orden_texto <- orden_texto
  do.call(medir, argumentos)
}

# La huella de una metrica del sello. Falla si la medicion no trae sello: una
# prueba que compara dos huellas `NULL` no probaria nada.
.se_huella <- function(medicion, nombre = NULL) {
  sello <- attr(medicion, "sello_metricas", exact = TRUE)
  expect_s3_class(sello, "data.frame")
  if (is.null(nombre)) nombre <- sello$metrica_instanciada[[1L]]
  huella <- sello$huella_definicion[sello$metrica_instanciada == nombre]
  expect_length(huella, 1L)
  expect_match(huella, "^[0-9a-f]{32}$")
  huella
}

.se_nucleo <- metricas_nucleo()

.se_rango <- function(minimo = 0, maximo = 10, nombre = "R", atributo = "x", ...) {
  instanciar(especializar(
    .se_nucleo$ValoresPosiblesPorComprension, nombre,
    minimo = minimo, maximo = maximo, ...
  ), "t", atributo)
}

test_that("cada metrica del modelo lleva su sello, tambien la que no se midio", {
  m <- suppressWarnings(.se_medir(
    .se_rango(),
    instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "no_esta")
  ))
  sello <- attr(m, "sello_metricas", exact = TRUE)
  expect_s3_class(sello, "data.frame")
  expect_identical(sello$metrica_instanciada, c("R@t.x", "NN@t.no_esta"))
  expect_identical(sello$id_medicion, c("c", "c"))
  expect_identical(sello$metrica_especifica, c("R", "NN"))
  expect_identical(
    sello$version_lupa,
    rep(as.character(utils::packageVersion("lupa")), 2L)
  )
  expect_true(all(grepl("^[0-9a-f]{32}$", sello$huella_definicion)))
  # El resumen y la base sellan con la misma huella: la definicion no depende
  # de donde se mide.
  resumen <- suppressWarnings(medir(
    modelo(.se_rango()), list(t = .se_datos()), id_medicion = "c",
    fecha = .se_fecha, detalle = "resumen"
  ))
  expect_identical(.se_huella(resumen), .se_huella(m, "R@t.x"))
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", .se_datos())
  base <- medir_dbi(modelo(.se_rango()), con, id_medicion = "c", fecha = .se_fecha)
  expect_identical(.se_huella(base), .se_huella(m, "R@t.x"))
})

test_that("la huella es la misma en otra sesion: su valor esta escrito", {
  # Este numero es la promesa: una huella calculada en otra maquina, en otro
  # proceso o con otra version de lupa tiene que dar esto mismo mientras la
  # definicion no cambie. Si una version nueva lo mueve, toda serie existente
  # acusaria un cambio de definicion que no hubo.
  # Medido en tres procesos: bajo `es_UY.UTF-8`, bajo `LC_ALL=C` y con
  # `--vanilla`; la formula con una enie, tambien bajo `C`.
  m <- .se_medir(instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x"))
  expect_identical(.se_huella(m), "c41052aed935e0d16ea7a71cb8cf44f0")
  formula <- instanciar(especializar(
    .se_nucleo$ReglaIntegridadIntraEntidad, "F",
    regla = eval(bquote(~ t >= "a" & t <= .(intToUtf8(241L))))
  ), "t", "t")
  expect_identical(
    .se_huella(suppressWarnings(.se_medir(formula, orden_texto = "bytes"))),
    "34588be488e57766c7deddb6409c6b9b"
  )
  # La suma por un temporal -la de R anterior a 4.5.0- es la misma.
  texto <- "lupa"
  expect_identical(
    lupa:::.md5_textos(texto, via_archivo = TRUE),
    lupa:::.md5_textos(texto, via_archivo = FALSE)
  )
})

test_that("ni el locale, ni las opciones, ni el entorno de la regla mueven la huella", {
  regla_formula <- function() {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "F", regla = ~ x > 1.5 | is.na(x)
    ), "t", "x")
  }
  base <- .se_huella(.se_medir(regla_formula()))
  anterior <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", anterior)), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))
  expect_identical(.se_huella(.se_medir(regla_formula())), base)
  suppressWarnings(Sys.setlocale("LC_COLLATE", anterior))
  local({
    opciones <- options(scipen = 100, digits = 3, OutDec = ",")
    on.exit(options(opciones), add = TRUE)
    expect_identical(.se_huella(.se_medir(regla_formula())), base)
  })
  # La misma formula creada en otro entorno.
  otra <- local({
    u <- 99
    regla_formula()
  })
  expect_identical(.se_huella(.se_medir(otra)), base)
  # Una funcion vale por su texto: con o sin `srcref`, y con otro entorno.
  con_fuente <- local({
    opciones <- options(keep.source = TRUE)
    on.exit(options(opciones), add = TRUE)
    eval(parse(text = "function(d) {\n  # comentario\n  !is.na(d$x)\n}"))
  })
  sin_fuente <- function(d) {
    !is.na(d$x)
  }
  funcion <- function(f) {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "G", regla = f
    ), "t", "x")
  }
  expect_identical(
    .se_huella(.se_medir(funcion(con_fuente))),
    .se_huella(.se_medir(funcion(sin_fuente)))
  )
})

test_that("ni el orden de los argumentos ni las etiquetas mueven la huella", {
  base <- .se_huella(.se_medir(.se_rango(minimo = 0, maximo = 10)))
  al_reves <- instanciar(especializar(
    .se_nucleo$ValoresPosiblesPorComprension, "R", maximo = 10, minimo = 0
  ), "t", "x")
  expect_identical(.se_huella(.se_medir(al_reves)), base)
  # El nombre de la especializacion y el de la instancia son etiquetas.
  otro_nombre <- instanciar(especializar(
    .se_nucleo$ValoresPosiblesPorComprension, "OtroNombre", minimo = 0, maximo = 10
  ), "t", "x", nombre_instancia = "Etiqueta")
  expect_identical(.se_huella(.se_medir(otro_nombre)), base)
  # Una propiedad que se usa como conjunto: el orden de sus valores no cuenta.
  dominio <- function(valores) {
    instanciar(especializar(
      .se_nucleo$ValoresPosiblesPorExtension, "D", valores = valores
    ), "t", "y")
  }
  expect_identical(
    .se_huella(.se_medir(dominio(c("si", "no")))),
    .se_huella(.se_medir(dominio(c("no", "si", "no"))))
  )
  # La semantica es prosa: reescribirla no cambia lo que se mide.
  propia <- function(semantica) {
    metrica(
      "Propia", semantica, "instanciaAtributo", "booleano",
      metodo = function(tablas, instancia) {
        x <- tablas[[instancia$entidad]][[instancia$atributos]]
        lupa:::.salida_metodo(!is.na(x), instancia$entidad, instancia$atributos,
                              seq_along(x), paste0("t[", seq_along(x), ",]"))
      }
    )()(entidad = "t", atributos = "x")
  }
  expect_identical(
    .se_huella(.se_medir(propia("Una descripcion."))),
    .se_huella(.se_medir(propia("Otra redaccion de lo mismo.")))
  )
})

test_that("cada parte de la definicion mueve la huella", {
  base <- .se_huella(.se_medir(.se_rango()))
  distinta <- function(...) {
    huella <- .se_huella(suppressWarnings(.se_medir(...)))
    expect_false(identical(huella, base))
  }
  # Un parametro.
  distinta(.se_rango(maximo = 11))
  # El atributo, y la entidad.
  distinta(instanciar(especializar(
    .se_nucleo$ValoresPosiblesPorComprension, "R", minimo = 0, maximo = 10
  ), "t", "t", nombre_instancia = "R@t.x"))
  # La metrica.
  distinta(instanciar(especializar(.se_nucleo$NoNulo, "R"), "t", "x"))
  # La aplicabilidad de su atributo; la de otra columna no.
  distinta(.se_rango(), aplicabilidad = list(x = ~ y == "si"))
  expect_identical(
    .se_huella(.se_medir(.se_rango(), aplicabilidad = list(y = ~ x > 0))),
    base
  )
  # La formula de una regla.
  regla <- function(f) {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "F", regla = f
    ), "t", "x")
  }
  expect_false(identical(
    .se_huella(.se_medir(regla(~ x > 1 | is.na(x)))),
    .se_huella(.se_medir(regla(~ x >= 1 | is.na(x))))
  ))
  # El orden de los atributos, que la regla recibe en ese orden.
  dos <- function(atributos) {
    instanciar(especializar(
      .se_nucleo$ConjuntoAtributosDuplicado, "Dup"
    ), "t", atributos, nombre_instancia = "Dup")
  }
  expect_false(identical(
    .se_huella(.se_medir(dos(c("x", "y")))),
    .se_huella(.se_medir(dos(c("y", "x"))))
  ))
  # Un metodo declarado por quien llama.
  metodo <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    lupa:::.salida_metodo(!is.na(x), instancia$entidad, instancia$atributos,
                          seq_along(x), paste0("t[", seq_along(x), ",]"))
  }
  nn <- function(...) instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x", ...)
  expect_false(identical(
    .se_huella(.se_medir(nn())), .se_huella(.se_medir(nn(metodo = metodo)))
  ))
  # Campos de la declaracion: orientacion, granularidad, tipo, dimension.
  propia <- function(...) {
    metrica("Propia", "Igual.", metodo = metodo, ...)()(entidad = "t", atributos = "x")
  }
  huellas <- vapply(list(
    propia(granularidad = "instanciaAtributo", tipo_resultado = "booleano"),
    propia(granularidad = "instanciaAtributo", tipo_resultado = "booleano",
           orientacion = "defecto"),
    propia(granularidad = "instanciaAtributo", tipo_resultado = "real"),
    propia(granularidad = "instanciaAtributo", tipo_resultado = "booleano",
           dimension = "Completitud")
  ), function(i) .se_huella(suppressWarnings(.se_medir(i))), character(1L))
  expect_identical(length(unique(huellas)), 4L)
})

test_that("el referencial es parte de la definicion; su nombre y el orden de sus filas no", {
  padron <- data.frame(t = c("a", "z", "m"), codigo = 1:3, stringsAsFactors = FALSE)
  con_ref <- function(ref) {
    instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"),
               "t", "t", referencial = ref)
  }
  base <- .se_huella(suppressWarnings(.se_medir(con_ref(
    referencial(padron, clave = "t", valor = "codigo")
  ))))
  otro_orden <- padron[c(3, 1, 2), ]
  expect_identical(.se_huella(suppressWarnings(.se_medir(con_ref(
    referencial(otro_orden, clave = "t", valor = "codigo", nombre = "otro")
  )))), base)
  cambiado <- padron
  cambiado$codigo[[2L]] <- 9L
  expect_false(identical(.se_huella(suppressWarnings(.se_medir(con_ref(
    referencial(cambiado, clave = "t", valor = "codigo")
  )))), base))
})

test_that("el orden de texto entra en la huella solo de lo que ordena texto", {
  texto <- function() .se_rango(minimo = "a", maximo = "m", nombre = "T", atributo = "t")
  numero <- function() .se_rango()
  nn <- function() instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x")
  idioma <- .se_medir(texto(), numero(), nn())
  bytes <- .se_medir(texto(), numero(), nn(), orden_texto = "bytes")
  expect_false(identical(.se_huella(idioma, "T@t.t"), .se_huella(bytes, "T@t.t")))
  expect_identical(.se_huella(idioma, "R@t.x"), .se_huella(bytes, "R@t.x"))
  expect_identical(.se_huella(idioma, "NN@t.x"), .se_huella(bytes, "NN@t.x"))
})

# --- Comparar dos mediciones ---------------------------------------------------

.se_texto <- function() .se_rango(minimo = "a", maximo = "m", nombre = "T", atributo = "t")

.se_par <- function(orden_actual = "bytes") {
  list(
    ene = .se_medir(.se_texto(), id = "ene"),
    feb = .se_medir(
      .se_texto(), id = "feb", fecha = as.POSIXct("2026-06-01", tz = "UTC"),
      orden_texto = if (identical(orden_actual, "bytes")) "bytes"
    )
  )
}

.se_avisos <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  }, lupa_aviso_sello_entorno = function(m) {
    avisos <<- c(avisos, sub("\n$", "", conditionMessage(m)))
    invokeRestart("muffleMessage")
  })
  list(valor = valor, avisos = avisos)
}

test_that("rbind() avisa cuando una metrica cambio de definicion, y une los sellos", {
  par <- .se_par()
  r <- .se_avisos(rbind(par$ene, par$feb))
  expect_length(r$avisos, 1L)
  expect_match(r$avisos, "T@t.t", fixed = TRUE)
  expect_match(r$avisos, "definici", fixed = TRUE)
  sello <- attr(r$valor, "sello_metricas", exact = TRUE)
  expect_identical(sello$id_medicion, c("ene", "feb"))
  expect_false(identical(sello$huella_definicion[[1L]], sello$huella_definicion[[2L]]))
  # Control: la misma definicion no avisa.
  igual <- .se_par(orden_actual = "idioma")
  expect_length(.se_avisos(rbind(igual$ene, igual$feb))$avisos, 0L)
  # Y `[` deja el sello de las corridas que quedan.
  solo_feb <- r$valor[r$valor$id_medicion == "feb", ]
  expect_identical(attr(solo_feb, "sello_metricas", exact = TRUE)$id_medicion, "feb")
})

test_that("una medicion sin sello se une como antes y se avisa que no lo trae", {
  par <- .se_par()
  vieja <- par$ene
  attr(vieja, "sello_metricas") <- NULL
  r <- .se_avisos(rbind(vieja, par$feb))
  expect_length(r$avisos, 1L)
  expect_match(r$avisos, "`ene`", fixed = TRUE)
  expect_identical(
    as.data.frame(r$valor)$resultado,
    c(as.data.frame(par$ene)$resultado, as.data.frame(par$feb)$resultado)
  )
  expect_identical(attr(r$valor, "sello_metricas", exact = TRUE)$id_medicion, "feb")
})

.se_perfil <- perfil_evaluacion(
  "P", regla_evaluacion("alcanza", function(x) x >= 0.5)
)

.se_evaluar <- function(medicion) evaluar(agregar(medicion, "atributo", "ratio"), .se_perfil)

test_that("la deriva no compara una metrica que cambio de definicion sin declararlo", {
  par <- .se_par()
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_length(h$avisos, 1L)
  deriva <- as.data.frame(detectar_deriva_calidad(h$valor))
  expect_false("resultado" %in% deriva$aspecto)
  fila <- deriva[deriva$aspecto == "definicion_metrica", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_identical(fila$cambio, "no_comparable")
  expect_identical(as.character(fila$severidad), "error")
  expect_match(fila$evidencia, "T@t.t", fixed = TRUE)
  # `comparar_evaluaciones()` lee la deriva: sin delta, y avisa con su nombre.
  comparada <- .se_avisos(comparar_evaluaciones(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_true(is.na(comparada$valor$delta))
  expect_length(comparada$avisos, 1L)
  expect_match(comparada$avisos, "^comparar_evaluaciones\\(\\)")
  # Un CSV conserva el sello, y la deriva lo sigue leyendo.
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  utils::write.csv(h$valor, archivo, row.names = FALSE)
  releida <- as.data.frame(detectar_deriva_calidad(
    utils::read.csv(archivo, stringsAsFactors = FALSE)
  ))
  expect_identical(releida$aspecto, deriva$aspecto)
  expect_identical(releida$cambio, deriva$cambio)
})

test_that("sin cambio de definicion no se declara nada de mas", {
  # La mitad de control: con las dos corridas selladas y la misma definicion no
  # hay aviso, ni fila, ni diagnostico. Que esten selladas es lo que hace que el
  # control mida algo: sin sello tampoco habria nada que declarar.
  igual <- .se_par(orden_actual = "idioma")
  h <- .se_avisos(historico_calidad(.se_evaluar(igual$ene), .se_evaluar(igual$feb)))
  sellos <- attr(h$valor, "configuracion_evaluacion", exact = TRUE)$sello_metricas
  expect_length(sellos, 2L)
  expect_false(anyNA(sellos))
  expect_length(h$avisos, 0L)
  deriva <- detectar_deriva_calidad(h$valor)
  expect_identical(as.data.frame(deriva)$aspecto, "resultado")
  expect_identical(nrow(attr(deriva, "cobertura_diagnosticos", exact = TRUE)), 0L)
})

test_that("un cambio que la configuracion ya declara conserva la comparacion", {
  ene <- .se_medir(.se_rango(), id = "ene")
  feb <- medir(modelo(.se_rango()), list(t = .se_datos()), id_medicion = "feb",
               fecha = as.POSIXct("2026-06-01", tz = "UTC"),
               aplicabilidad = list(x = ~ y == "si"))
  h <- suppressWarnings(historico_calidad(.se_evaluar(ene), .se_evaluar(feb)))
  deriva <- as.data.frame(detectar_deriva_calidad(h))
  expect_true("resultado" %in% deriva$aspecto)
  expect_true("configuracion_aplicabilidad" %in% deriva$aspecto)
  fila <- deriva[deriva$aspecto == "definicion_metrica", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_true(is.na(fila$cambio))
})

test_that("una corrida sin sello se compara como antes y la deriva lo dice", {
  par <- .se_par()
  vieja <- .se_evaluar(par$ene)
  attr(vieja, "sello_metricas") <- NULL
  h <- .se_avisos(historico_calidad(vieja, .se_evaluar(par$feb)))
  expect_length(h$avisos, 1L)
  expect_match(h$avisos, "sello", fixed = TRUE)
  deriva <- detectar_deriva_calidad(h$valor)
  expect_identical(as.data.frame(deriva)$aspecto, "resultado")
  diagnosticos <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)
  expect_true(any(startsWith(as.character(diagnosticos$motivo), "sin_sello")))
  # Un historico de antes del sello -sin la columna- se lee igual.
  sin_columna <- as.data.frame(h$valor)
  sin_columna$sello_metricas <- NULL
  configuracion <- attr(sin_columna, "configuracion_evaluacion", exact = TRUE)
  configuracion$sello_metricas <- NULL
  attr(sin_columna, "configuracion_evaluacion") <- configuracion
  deriva_vieja <- detectar_deriva_calidad(sin_columna)
  expect_identical(as.data.frame(deriva_vieja)$aspecto, "resultado")
})

test_that("acumular una corrida mas no repite el aviso de un cambio ya avisado", {
  par <- .se_par()
  h <- suppressWarnings(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  mar <- medir(modelo(.se_texto()), list(t = .se_datos()), id_medicion = "mar",
               fecha = as.POSIXct("2026-07-01", tz = "UTC"), orden_texto = "bytes")
  r <- .se_avisos(acumular_historico(h, .se_evaluar(mar)))
  expect_length(r$avisos, 0L)
  expect_identical(
    attr(r$valor, "configuracion_evaluacion", exact = TRUE)$id_medicion,
    c("ene", "feb", "mar")
  )
})

test_that("un perfil que no evalua la metrica que cambio no se declara", {
  otra <- function() instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x")
  ene <- .se_medir(.se_texto(), otra(), id = "ene")
  feb <- medir(modelo(.se_texto(), otra()), list(t = .se_datos()), id_medicion = "feb",
               fecha = as.POSIXct("2026-06-01", tz = "UTC"), orden_texto = "bytes")
  perfil_nn <- perfil_evaluacion(
    "SoloNN", regla_evaluacion("alcanza", function(x) x >= 0.5, metricas = "NN@t.x")
  )
  h <- suppressWarnings(historico_calidad(evaluar(ene, perfil_nn), evaluar(feb, perfil_nn)))
  deriva <- as.data.frame(detectar_deriva_calidad(h))
  expect_identical(deriva$aspecto, "resultado")
  # Control: el perfil que si la evalua la declara.
  perfil_t <- perfil_evaluacion(
    "SoloT", regla_evaluacion("alcanza", function(x) x >= 0.5, metricas = "T@t.t")
  )
  h <- suppressWarnings(historico_calidad(evaluar(ene, perfil_t), evaluar(feb, perfil_t)))
  expect_true("definicion_metrica" %in% as.data.frame(detectar_deriva_calidad(h))$aspecto)
})

test_that("una corrida guardada sin sello se vuelve a acumular sellada sin chocar", {
  par <- .se_par()
  evaluacion <- .se_evaluar(par$ene)
  viejo <- historico_calidad(evaluacion)
  # Como lo guardaba una version anterior: ni la columna ni el sello en la
  # configuracion.
  viejo$sello_metricas <- NULL
  configuracion <- attr(viejo, "configuracion_evaluacion", exact = TRUE)
  configuracion$sello_metricas <- NULL
  attr(viejo, "configuracion_evaluacion") <- configuracion
  ampliado <- acumular_historico(viejo, evaluacion)
  sellos <- attr(ampliado, "configuracion_evaluacion", exact = TRUE)$sello_metricas
  expect_length(sellos, 1L)
  expect_false(is.na(sellos))
  # Y la deriva por regla lee el mismo sello que la de perfil.
  h <- suppressWarnings(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  por_regla <- as.data.frame(detectar_deriva_calidad(h, nivel = "regla"))
  expect_identical(por_regla$aspecto, "definicion_metrica")
  expect_identical(por_regla$cambio, "no_comparable")
})

# --- Lo que la huella tiene que ver y lo que no ------------------------------

.se_crear_regla <- function(u) {
  force(u)
  function(d) d$x > u
}
.se_crear_aplicabilidad <- function(u) {
  force(u)
  ~ x > u
}
.se_crear_metodo <- function(u) {
  force(u)
  function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    lupa:::.salida_metodo(
      !is.na(x) & x > u, instancia$entidad, instancia$atributos,
      seq_along(x), paste0(instancia$entidad, "[", seq_along(x), ",]")
    )
  }
}
.se_regla_funcion <- function(f) {
  instanciar(especializar(
    .se_nucleo$ReglaIntegridadIntraEntidad, "G", regla = f
  ), "t", "x")
}

test_that("lo que una funcion del usuario lee de su entorno entra en la huella", {
  # Mismo texto, otro valor de `u`: miden distinto y tenian una sola huella.
  datos <- data.frame(x = c(1, 2, 3))
  medir_con <- function(...) suppressWarnings(.se_medir(..., datos = datos))
  expect_false(identical(
    .se_huella(medir_con(.se_regla_funcion(.se_crear_regla(1)))),
    .se_huella(medir_con(.se_regla_funcion(.se_crear_regla(2))))
  ))
  expect_false(identical(
    .se_huella(medir_con(.se_rango(), aplicabilidad = list(x = .se_crear_aplicabilidad(1)))),
    .se_huella(medir_con(.se_rango(), aplicabilidad = list(x = .se_crear_aplicabilidad(2))))
  ))
  # La fabrica se llama con la constante: llamada con una variable, su argumento
  # es una promesa que el sello no lee (ver la prueba de las promesas).
  metodo <- function(f) {
    instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x", metodo = f)
  }
  expect_false(identical(
    .se_huella(medir_con(metodo(.se_crear_metodo(1)))),
    .se_huella(medir_con(metodo(.se_crear_metodo(2))))
  ))
  # Control: el mismo valor en otro entorno es la misma definicion.
  expect_identical(
    .se_huella(medir_con(.se_regla_funcion(.se_crear_regla(1)))),
    .se_huella(medir_con(.se_regla_funcion(.se_crear_regla(1))))
  )
  # Y la deriva ya no lo publica como un cambio de los datos.
  uno <- .se_medir(metodo(.se_crear_metodo(1)), datos = datos, id = "uno")
  dos <- .se_medir(metodo(.se_crear_metodo(2)), datos = datos, id = "dos",
                   fecha = as.POSIXct("2026-06-01", tz = "UTC"))
  h <- suppressWarnings(historico_calidad(
    evaluar(uno, .se_perfil), evaluar(dos, .se_perfil)
  ))
  deriva <- as.data.frame(detectar_deriva_calidad(h))
  expect_identical(deriva$aspecto, "definicion_metrica")
  expect_identical(deriva$cambio, "no_comparable")
})

test_that("una funcion que se llama a si misma tiene huella, y una cadena sin fin la deja parcial", {
  cuenta <- function(n) if (n <= 0) 0 else 1 + cuenta(n - 1)
  regla <- function(d) cuenta(1) > 0 & !is.na(d$x)
  sello <- attr(suppressWarnings(.se_medir(.se_regla_funcion(regla))),
                "sello_metricas", exact = TRUE)
  expect_match(sello$huella_definicion, "^[0-9a-f]{32}$")
  # Una regla escrita como funcion es parcial siempre; la recursion no agrega
  # motivo.
  expect_identical(sello$certificacion, "parcial")
  expect_false(grepl("cadena", sello$motivo_huella, fixed = TRUE))
  # Diez funciones distintas encadenadas pasan el tope.
  cadena <- local({
    f <- function(d) !is.na(d$x)
    for (i in seq_len(10L)) {
      f <- local({
        siguiente <- f
        function(d) siguiente(d)
      })
    }
    f
  })
  sello <- attr(suppressWarnings(.se_medir(.se_regla_funcion(cadena))),
                "sello_metricas", exact = TRUE)
  expect_match(sello$huella_definicion, "^[0-9a-f]{32}$")
  expect_identical(sello$certificacion, "parcial")
  expect_match(sello$motivo_huella, "cadena", fixed = TRUE)
})

test_that("un valor del entorno que no se puede certificar no calla", {
  datos <- data.frame(x = c(1, 2, 3))
  con_entorno <- function(u) {
    configuracion <- new.env()
    configuracion$u <- u
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "G",
      regla = function(d) d$x > configuracion$u
    ), "t", "x")
  }
  uno <- suppressWarnings(.se_medir(con_entorno(1), datos = datos, id = "uno"))
  sello <- attr(uno, "sello_metricas", exact = TRUE)
  expect_identical(sello$huella_definicion, "no_certificable")
  expect_match(sello$motivo_huella, "entorno", fixed = TRUE)
  dos <- suppressWarnings(.se_medir(
    con_entorno(2), datos = datos, id = "dos",
    fecha = as.POSIXct("2026-06-01", tz = "UTC")
  ))
  r <- .se_avisos(rbind(uno, dos))
  expect_length(r$avisos, 1L)
  expect_match(r$avisos, "no se puede saber", fixed = TRUE)
  h <- .se_avisos(historico_calidad(
    evaluar(uno, .se_perfil), evaluar(dos, .se_perfil)
  ))
  expect_length(h$avisos, 1L)
  expect_match(h$avisos, "no se puede saber", fixed = TRUE)
  deriva <- detectar_deriva_calidad(h$valor)
  # Se compara como antes, y se dice que no se sabe.
  expect_identical(as.data.frame(deriva)$aspecto, "resultado")
  diagnosticos <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)
  expect_true(any(startsWith(as.character(diagnosticos$motivo), "no_certificable")))
})

test_that("un nombre de columna de la aplicabilidad no se lee del entorno", {
  # La formula de `aplicabilidad` busca `y` en la tabla: una variable del mismo
  # nombre en su entorno no cambia lo que mide, ni su huella.
  formula_con <- function(valor) {
    y <- valor
    ~ y == "si"
  }
  expect_identical(
    .se_huella(.se_medir(.se_rango(), aplicabilidad = list(x = formula_con(1)))),
    .se_huella(.se_medir(.se_rango(), aplicabilidad = list(x = formula_con(2))))
  )
})

test_that("rbind() no une dos sellos distintos de la misma corrida", {
  padron <- data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla"),
                       stringsAsFactors = FALSE)
  otro <- padron
  otro$nombre[[3L]] <- "Otra"
  datos <- data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla"),
                      stringsAsFactors = FALSE)
  medir_ref <- function(p) {
    inst <- instanciar(
      especializar(metricas_referencial()$CorrectitudSemDebil, "Ref"),
      "t", c("id", "nombre"),
      referencial = referencial(p, clave = "id", valor = "nombre")
    )
    suppressWarnings(medir(modelo(inst), list(t = datos), id_medicion = "mismo",
                           fecha = .se_fecha))
  }
  a <- medir_ref(padron)
  b <- medir_ref(otro)
  expect_error(rbind(a, b), "mismo", fixed = TRUE)
  # Control: la misma medicion dos veces se une, con un sello.
  dos_veces <- rbind(a, a)
  expect_identical(nrow(attr(dos_veces, "sello_metricas", exact = TRUE)), 1L)
})

test_that("orden_texto entra solo si la regla ordena texto en esa medicion", {
  datos <- data.frame(n = c(0, 2, 0, 2), t = c("a", "b", "b", "a"),
                      stringsAsFactors = FALSE)
  regla <- function(f, atributos = c("n", "t")) {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "F", regla = f
    ), "t", atributos, nombre_instancia = "F")
  }
  par <- function(f, ...) {
    c(
      .se_huella(suppressWarnings(.se_medir(regla(f, ...), datos = datos))),
      .se_huella(suppressWarnings(.se_medir(regla(f, ...), datos = datos,
                                            orden_texto = "bytes")))
    )
  }
  # El texto solo en una igualdad, o en un atributo que la formula no usa.
  huellas <- par(~ n > 1 & t == "a")
  expect_identical(huellas[[1L]], huellas[[2L]])
  huellas <- par(~ n > 1)
  expect_identical(huellas[[1L]], huellas[[2L]])
  # Control: un orden sobre la columna de texto si cambia.
  huellas <- par(~ n > 1 & t > "a")
  expect_false(identical(huellas[[1L]], huellas[[2L]]))
})

test_that("lo que no cambia el significado no mueve la huella", {
  regla <- function(f, atributos = "x") {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "F", regla = f
    ), "t", atributos, nombre_instancia = "F")
  }
  h <- function(...) .se_huella(suppressWarnings(.se_medir(...)))
  # Un parentesis de mas es la identidad.
  expect_identical(h(regla(~ x > 1)), h(regla(~ (x > 1))))
  # En una funcion tambien, y un `{` de una sola expresion.
  expect_identical(
    h(.se_regla_funcion(function(d) (d$x > 1))),
    h(.se_regla_funcion(function(d) {
      d$x > 1
    }))
  )
  # El orden de los atributos que una formula nombra.
  dos <- data.frame(x = c(1, 2, 3), y = c(3, 2, 1))
  expect_identical(
    h(regla(~ x > y, c("x", "y")), datos = dos),
    h(regla(~ x > y, c("y", "x")), datos = dos)
  )
  # Control: una funcion recibe las columnas en ese orden, y ahi si cuenta.
  posicional <- function(atributos) {
    instanciar(especializar(
      .se_nucleo$ReglaIntegridadIntraEntidad, "G", regla = function(d) d[[1L]] > d[[2L]]
    ), "t", atributos, nombre_instancia = "G")
  }
  expect_false(identical(
    h(posicional(c("x", "y")), datos = dos), h(posicional(c("y", "x")), datos = dos)
  ))
  # Un entero y el doble del mismo valor.
  expect_identical(h(.se_rango(minimo = 0L, maximo = 10L)), h(.se_rango()))
  expect_false(identical(h(.se_rango(maximo = 11)), h(.se_rango())))
  # El orden de `proteger` en la normalizacion del referencial.
  padron <- data.frame(t = c("a", "z"), codigo = c(1L, 2L), stringsAsFactors = FALSE)
  con_proteger <- function(proteger) {
    instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"),
               "t", "t", referencial = referencial(
                 padron, clave = "t", valor = "codigo",
                 normalizar = normalizacion(proteger = proteger)
               ))
  }
  protegidos <- c(intToUtf8(241L), intToUtf8(252L))
  expect_identical(h(con_proteger(protegidos)), h(con_proteger(rev(protegidos))))
})

test_that("el tipo de una columna del referencial no mueve la huella si mide lo mismo", {
  datos <- data.frame(t = c("a", "z", "m", NA), stringsAsFactors = FALSE)
  medir_ref <- function(p) {
    inst <- instanciar(
      especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"),
      "t", "t", referencial = referencial(p, clave = "k", valor = "v")
    )
    suppressWarnings(medir(modelo(inst), list(t = datos), id_medicion = "c",
                           fecha = .se_fecha))
  }
  caracter <- data.frame(k = c("a", "z", "b"), v = c(1L, 2L, 3L),
                         stringsAsFactors = FALSE)
  doble <- caracter
  doble$v <- as.double(doble$v)
  factor_k <- caracter
  factor_k$k <- factor(factor_k$k, levels = c("z", "b", "a", "q"))
  medidas <- lapply(list(caracter, doble, factor_k), medir_ref)
  # Primero que midan lo mismo: si no, la huella tendria que distinguirlas.
  resultados <- lapply(medidas, function(m) as.data.frame(m)$resultado)
  expect_identical(resultados[[2L]], resultados[[1L]])
  expect_identical(resultados[[3L]], resultados[[1L]])
  huellas <- vapply(medidas, .se_huella, character(1L))
  expect_identical(length(unique(huellas)), 1L)
})

test_that("el metodo por omision de una metrica propia entra por su texto", {
  # Como el de una metrica del paquete: es la implementacion de la metrica, que
  # se identifica por su nombre. Lo que arrastra de su entorno no hace de dos
  # corridas de la misma metrica dos definiciones (la serie de
  # test-O24-orden-y-objeto-editado.R se sigue comparando).
  metodo <- function(valor) function(tablas, instancia) data.frame(
    resultado = valor, entidad = instancia$entidad[[1L]],
    atributo = instancia$atributos[[1L]], fila = NA_integer_, objeto = "ok"
  )
  propia <- function(valor) {
    instanciar(especializar(metrica(
      "MideA", "a", "atributo", "real", dimension = "D", factor = "F",
      metodo = metodo(valor)
    )), "t", "x")
  }
  expect_identical(
    .se_huella(.se_medir(propia(0.75), datos = data.frame(x = 1))),
    .se_huella(.se_medir(propia(0.8), datos = data.frame(x = 1)))
  )
})

# --- Sellar no ejecuta codigo del usuario, y lo que no ve lo declara ---------

.se_metodo_en <- function(entorno, cuerpo_x) {
  f <- function(tablas, instancia) {
    x <- tablas[[instancia$entidad]][[instancia$atributos]]
    lupa:::.salida_metodo(
      !is.na(x), instancia$entidad, instancia$atributos, seq_along(x),
      paste0("t[", seq_along(x), ",]")
    )
  }
  body(f)[[3L]] <- cuerpo_x
  environment(f) <- entorno
  f
}

.se_sello_metodo <- function(f, datos = data.frame(x = c(1, 2, 3))) {
  m <- suppressWarnings(medir(
    modelo(instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x", metodo = f)),
    list(t = datos), id_medicion = "c", fecha = .se_fecha
  ))
  attr(m, "sello_metricas", exact = TRUE)
}

test_that("sellar no evalua una promesa del entorno: ni su error, ni su demora, ni sus efectos", {
  archivo <- tempfile()
  opciones <- options(lupa_sello_prueba = NULL)
  on.exit(options(opciones), add = TRUE)
  promesa <- function(expresion) {
    entorno <- new.env(parent = globalenv())
    do.call(delayedAssign, list("u", expresion, environment(), entorno))
    # La rama que lee `u` no corre al medir.
    .se_metodo_en(entorno, quote(if (length(x) < 0L) u))
  }
  efecto <- bquote({
    writeLines("evaluada", .(archivo))
    options(lupa_sello_prueba = "tocada")
    1
  })
  sello <- .se_sello_metodo(promesa(efecto))
  expect_false(file.exists(archivo))
  expect_null(getOption("lupa_sello_prueba"))
  # La huella describe el codigo de la promesa, sin evaluarlo, y es parcial.
  expect_identical(sello$certificacion, "parcial")
  expect_match(sello$motivo_huella, "sin evaluar", fixed = TRUE)
  expect_no_error(.se_sello_metodo(promesa(quote(stop("no se evalua")))))
  tiempo <- system.time(.se_sello_metodo(promesa(quote({ Sys.sleep(5); 1 }))))
  expect_lt(tiempo[["elapsed"]], 4)
  # El argumento de una fabrica llamada con una variable es una promesa: entra
  # su codigo, no su valor, y se dice por que.
  fabrica <- function(u) {
    force(u)
    .se_metodo_en(environment(), quote(x > u))
  }
  limite <- 2
  sello <- .se_sello_metodo(fabrica(limite))
  expect_identical(sello$certificacion, "parcial")
  expect_match(sello$motivo_huella, "f\u00e1brica", fixed = TRUE)
  # Control: llamada con la constante, entra su valor.
  expect_match(.se_sello_metodo(fabrica(2))$huella_definicion, "^[0-9a-f]{32}$")
})

test_that("sellar no dispara un enlace activo", {
  llamadas <- 0L
  entorno <- new.env(parent = globalenv())
  makeActiveBinding("u", function() {
    llamadas <<- llamadas + 1L
    1
  }, entorno)
  sello <- .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) u)))
  expect_identical(llamadas, 0L)
  expect_identical(sello$huella_definicion, "no_certificable")
  expect_match(sello$motivo_huella, "enlace activo", fixed = TRUE)
})

test_that("una lectura dinamica no se certifica, y una funcion comun si", {
  entorno <- new.env(parent = globalenv())
  entorno$nombre <- "sum"
  lecturas <- list(
    get = quote(get("limite_sello", envir = globalenv())),
    mget = quote(mget("limite_sello", envir = globalenv())),
    dynGet = quote(dynGet("limite_sello", ifnotfound = 0)),
    parse = quote(eval(parse(text = "limite_sello"))),
    getOption = quote(getOption("limite_sello", 0)),
    Sys.getenv = quote(Sys.getenv("LIMITE_SELLO")),
    exists = quote(exists("limite_sello")),
    sys.function = quote(sys.function()),
    base = quote(base::get("limite_sello")),
    do.call = quote(do.call(nombre, list(x)))
  )
  for (lectura in names(lecturas)) {
    sello <- .se_sello_metodo(.se_metodo_en(
      entorno, call("if", quote(length(x) < 0L), lecturas[[lectura]])
    ))
    expect_identical(sello$huella_definicion, "no_certificable", info = lectura)
    expect_match(sello$motivo_huella, "lectura din\u00e1mica", fixed = TRUE,
                 info = lectura)
  }
  # Tambien si la lectura esta en una funcion que la regla llama.
  entorno$ayudante <- function() getOption("limite_sello", 0)
  sello <- .se_sello_metodo(.se_metodo_en(entorno, quote(y <- ayudante())))
  expect_identical(sello$huella_definicion, "no_certificable")
  # Control: una funcion comun, y `do.call()` con el nombre escrito.
  for (cuerpo in list(
    quote(y <- sum(x) / length(x)),
    quote(y <- vapply(x, function(v) v * 2, numeric(1L))),
    quote(y <- do.call("sum", list(x))),
    quote(y <- paste(stats::median(x), collapse = ""))
  )) {
    sello <- .se_sello_metodo(.se_metodo_en(entorno, cuerpo))
    expect_match(sello$huella_definicion, "^[0-9a-f]{32}$", info = deparse(cuerpo))
  }
})

test_that("una funcion de un paquete externo entra con la version del paquete", {
  skip_if_not_installed("jsonlite")
  entorno <- new.env(parent = globalenv())
  externo <- .se_metodo_en(entorno, quote(y <- jsonlite::toJSON(x)))
  de_r <- .se_metodo_en(entorno, quote(y <- stats::median(x)))
  antes <- list(.se_sello_metodo(externo), .se_sello_metodo(de_r))
  local_mocked_bindings(.version_de_paquete = function(paquete) "99.0.0",
                        .package = "lupa")
  despues <- list(.se_sello_metodo(externo), .se_sello_metodo(de_r))
  expect_match(antes[[1L]]$huella_definicion, "^[0-9a-f]{32}$")
  expect_false(identical(antes[[1L]]$huella_definicion, despues[[1L]]$huella_definicion))
  # Un paquete de R se identifica por su nombre: su version es la de R.
  expect_identical(antes[[2L]]$huella_definicion, despues[[2L]]$huella_definicion)
})

test_that("una tabla grande en el entorno se resume rapido y por su contenido", {
  tabla <- function(valores) data.frame(id = seq_along(valores), valor = valores)
  # La tabla es una variable del entorno, no el argumento de una fabrica
  # llamada con una expresion: ese seria una promesa, que no se lee.
  fabrica <- function(valores) {
    entorno <- new.env(parent = globalenv())
    entorno$ref <- valores
    .se_metodo_en(entorno, quote(y <- nrow(ref)))
  }
  n <- 1000000L
  tiempo <- system.time(
    grande <- .se_sello_metodo(fabrica(tabla(as.double(seq_len(n)))))
  )
  expect_lt(tiempo[["elapsed"]], 3)
  expect_match(grande$huella_definicion, "^[0-9a-f]{32}$")
  # El mismo contenido en otro objeto, o en enteros, da la misma huella; un
  # valor distinto, otra.
  expect_identical(.se_sello_metodo(fabrica(tabla(seq_len(n))))$huella_definicion,
                   grande$huella_definicion)
  otra <- as.double(seq_len(n))
  otra[[n]] <- 0
  expect_false(identical(.se_sello_metodo(fabrica(tabla(otra)))$huella_definicion,
                         grande$huella_definicion))
})


# --- Sellar no despacha, no falla, y ve las lecturas escondidas -------------

test_that("sellar no ejecuta los metodos de una clase de quien llama", {
  llamadas <- new.env()
  llamadas$n <- 0L
  contar <- function(x, ...) {
    llamadas$n <- llamadas$n + 1L
    NextMethod()
  }
  genericos <- c("length", "[[", "[", "$", "names", "as.character", "as.double",
                 "as.vector", "dim", "format", "unique", "as.list")
  for (generico in genericos) {
    registerS3method(generico, "claseSelloPrueba", contar, envir = baseenv())
  }
  registerS3method("format", "fechaSelloPrueba", contar, envir = baseenv())
  valores <- list(
    lista = structure(list(a = 1, b = "x"), class = "claseSelloPrueba"),
    atomico = structure(c(1, 2, 3), class = "claseSelloPrueba"),
    fecha = structure(as.Date("2026-01-01"), class = c("fechaSelloPrueba", "Date")),
    tabla = data.frame(id = 1:3, v = structure(c(1, 2, 3), class = "claseSelloPrueba")),
    grande = structure(as.double(seq_len(20000L)), class = "claseSelloPrueba")
  )
  for (nombre in names(valores)) {
    entorno <- new.env(parent = globalenv())
    entorno$objeto <- valores[[nombre]]
    llamadas$n <- 0L
    sello <- .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) objeto)))
    expect_identical(llamadas$n, 0L, info = nombre)
    expect_match(sello$huella_definicion, "^[0-9a-f]{32}$", info = nombre)
  }
  # La clase es parte de lo que se describe: sin ella, otra huella.
  con_clase <- new.env(parent = globalenv())
  con_clase$objeto <- valores$atomico
  sin_clase <- new.env(parent = globalenv())
  sin_clase$objeto <- c(1, 2, 3)
  expect_false(identical(
    .se_sello_metodo(.se_metodo_en(con_clase, quote(if (length(x) < 0L) objeto)))$huella_definicion,
    .se_sello_metodo(.se_metodo_en(sin_clase, quote(if (length(x) < 0L) objeto)))$huella_definicion
  ))
})

test_that("un metodo de quien llama que falla o tarda no toca la medicion", {
  registerS3method("length", "claseSelloFalla", function(x) stop("no se llama"),
                   envir = baseenv())
  registerS3method("length", "claseSelloLenta", function(x) {
    Sys.sleep(5)
    1L
  }, envir = baseenv())
  for (clase in c("claseSelloFalla", "claseSelloLenta")) {
    entorno <- new.env(parent = globalenv())
    entorno$objeto <- structure(c(1, 2), class = clase)
    tiempo <- system.time(
      sello <- .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) objeto)))
    )
    expect_lt(tiempo[["elapsed"]], 4)
    expect_match(sello$huella_definicion, "^[0-9a-f]{32}$", info = clase)
  }
})

test_that("un error al sellar deja la metrica no_certificable y la medicion sigue", {
  local_mocked_bindings(
    .texto_definicion_metrica = function(...) stop("falla al describir"),
    .package = "lupa"
  )
  m <- .se_medir(.se_rango())
  expect_identical(nrow(as.data.frame(m)), 4L)
  sello <- attr(m, "sello_metricas", exact = TRUE)
  expect_identical(sello$huella_definicion, "no_certificable")
  expect_match(sello$motivo_huella, "falla al describir", fixed = TRUE)
})

test_that("las lecturas del entorno escondidas tambien se declaran", {
  assign("limite_sello_prueba", 1, envir = globalenv())
  on.exit(rm("limite_sello_prueba", envir = globalenv()), add = TRUE)
  entorno <- new.env(parent = globalenv())
  entorno$alias <- get
  entorno$otra <- local({
    limite <- 1
    function() 0
  })
  entorno$generica <- function(v) UseMethod("generica")
  escondidas <- list(
    .GlobalEnv = quote(y <- .GlobalEnv$limite_sello_prueba),
    globalenv = quote(y <- globalenv()[["limite_sello_prueba"]]),
    environment = quote(y <- environment(otra)$limite),
    as.list = quote(y <- as.list(.GlobalEnv)$limite_sello_prueba),
    ls = quote(y <- ls()),
    source = quote(source("archivo.R", local = TRUE)),
    alias = quote(y <- alias("limite_sello_prueba", envir = globalenv())),
    sapply_texto = quote(y <- sapply("limite_sello_prueba", "get")),
    sapply_simbolo = quote(y <- sapply("limite_sello_prueba", get)),
    lapply_nombrado = quote(y <- lapply(X = "limite_sello_prueba", "get")),
    UseMethod = quote(y <- generica(1))
  )
  for (lectura in names(escondidas)) {
    sello <- .se_sello_metodo(.se_metodo_en(
      entorno, call("if", quote(length(x) < 0L), escondidas[[lectura]])
    ))
    expect_identical(sello$huella_definicion, "no_certificable", info = lectura)
  }
  # Cada lectura se nombra una vez en el motivo.
  motivo <- .se_sello_metodo(.se_metodo_en(
    entorno, quote(if (length(x) < 0L) y <- eval(parse(text = "1")))
  ))$motivo_huella
  expect_identical(lengths(regmatches(motivo, gregexpr("eval()", motivo, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(motivo, gregexpr("parse()", motivo, fixed = TRUE))), 1L)
  # Control: una funcion de quien llama nombrada por texto se resuelve, y su
  # cuerpo entra en la huella.
  con_cuerpo <- function(valor) {
    e <- new.env(parent = globalenv())
    e$fn <- eval(bquote(function(v) v > .(valor)))
    .se_sello_metodo(.se_metodo_en(e, quote(y <- sapply(x, "fn"))))$huella_definicion
  }
  expect_match(con_cuerpo(1), "^[0-9a-f]{32}$")
  expect_false(identical(con_cuerpo(1), con_cuerpo(2)))
})

test_that("la funcion de un paquete externo entra con su cuerpo, no solo su version", {
  skip_if_not_installed("jsonlite")
  entorno <- new.env(parent = globalenv())
  metodo <- .se_metodo_en(entorno, quote(y <- jsonlite::toJSON(x)))
  loadNamespace("jsonlite")
  antes <- .se_sello_metodo(metodo)$huella_definicion
  # El mismo numero de version y otro codigo, como un paquete reinstalado.
  local_mocked_bindings(toJSON = function(x, ...) "otro", .package = "jsonlite")
  despues <- .se_sello_metodo(metodo)$huella_definicion
  expect_match(antes, "^[0-9a-f]{32}$")
  expect_false(identical(antes, despues))
})

test_that("el argumento de una fabrica entra por su codigo, sin evaluarlo, y la huella es parcial", {
  en <- function(v) {
    force(v)
    .se_metodo_en(environment(), quote(y <- v))
  }
  sello <- function(f) .se_sello_metodo(f)
  h <- function(f) sello(f)$huella_definicion
  # El codigo distingue lo que esta escrito distinto.
  expect_false(identical(h(en(c("a", "bb"))), h(en(c("a", "cc")))))
  expect_false(identical(h(en(c("a", "bb"))), h(en(letters[1:3]))))
  validos <- c("a", "bb")
  for (f in list(en(c("a", "bb")), en(letters[1:3]), en(as.numeric("2")),
                 en(validos), lapply(1:2, function(u) en(u))[[1L]])) {
    s <- sello(f)
    expect_match(s$huella_definicion, "^[0-9a-f]{32}$")
    expect_identical(s$certificacion, "parcial")
    expect_match(s$motivo_huella, "sin evaluar", fixed = TRUE)
  }
  # Una variable da la misma huella con cualquier valor: por eso es parcial.
  uno <- h(en(validos))
  validos <- c("a", "cc")
  expect_identical(h(en(validos)), uno)
  # Nunca se evalua: ni mil millones de elementos cuestan.
  tiempo <- system.time(s <- sello(en(seq_len(1e9))))
  expect_lt(tiempo[["elapsed"]], 4)
  # `..1` de los `...` de una fabrica sigue sin certificarse.
  puntos <- function(...) .se_metodo_en(environment(), quote(y <- ..1))
  expect_identical(h(puntos(1)), "no_certificable")
})

test_that("un conjunto se ordena sin despachar, tambien uno grande", {
  ve <- function(valores) {
    instanciar(especializar(.se_nucleo$ValoresPosiblesPorExtension, "VE",
                            valores = valores), "t", "y")
  }
  h <- function(valores) .se_huella(.se_medir(ve(valores)))
  grande <- sprintf("v%05d", seq_len(20000L))
  expect_identical(h(c("si", "no")), h(c("no", "si", "no")))
  expect_identical(h(c(grande, "si")), h(rev(c(grande, "si"))))
  expect_false(identical(h(c(grande, "si")), h(c(grande, "no"))))
  # Un conjunto con clase de quien llama: el modelo se sella sin medir, y
  # sellar no llama a ninguno de sus metodos.
  llamadas <- new.env()
  llamadas$n <- 0L
  for (generico in c("[", "as.character", "duplicated", "unique", "length", "names")) {
    registerS3method(generico, "claseConjuntoSello", function(x, ...) {
      llamadas$n <- llamadas$n + 1L
      NextMethod()
    }, envir = baseenv())
  }
  uno <- modelo(ve(structure(c("si", "no"), class = "claseConjuntoSello")))
  otro <- modelo(ve(structure(c("no", "si"), class = "claseConjuntoSello")))
  sellar <- function(m) {
    .sello_metricas(m, "c", NULL, "idioma", .lector_tablas(list(t = .se_datos())))
  }
  llamadas$n <- 0L
  sello_uno <- sellar(uno)
  sello_otro <- sellar(otro)
  expect_identical(llamadas$n, 0L)
  expect_match(sello_uno$huella_definicion, "^[0-9a-f]{32}$")
  expect_identical(sello_uno$huella_definicion, sello_otro$huella_definicion)
})

test_that("un referencial con clave integer64 no depende del orden de sus filas", {
  skip_if_not_installed("bit64")
  datos <- data.frame(k = bit64::as.integer64(c("3", "7")))
  sellar <- function(padron) {
    inst <- instanciar(
      especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"), "t", "k",
      referencial = referencial(padron, clave = "k", valor = "codigo")
    )
    .sello_metricas(modelo(inst), "c", NULL, "idioma",
                    .lector_tablas(list(t = datos)))$huella_definicion
  }
  padron <- data.frame(
    k = bit64::as.integer64(c("9007199254740993", "3", "7")), codigo = 1:3
  )
  base <- sellar(padron)
  expect_match(base, "^[0-9a-f]{32}$")
  expect_identical(sellar(padron[c(3L, 1L, 2L), ]), base)
  vecino <- padron
  vecino$k <- bit64::as.integer64(c("9007199254740992", "3", "7"))
  expect_false(identical(sellar(vecino), base))
})

test_that("un valor descrito sin despachar se distingue por su valor, sus nombres y su clase", {
  h <- function(valor) {
    entorno <- new.env(parent = globalenv())
    entorno$v <- valor
    .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) v)))$huella_definicion
  }
  distintos <- list(
    fecha = list(as.Date("2026-01-01"), as.Date("2026-01-02")),
    factor = list(factor(c("a", "b")), factor(c("a", "c"))),
    niveles = list(factor(c("a", "b")), factor(c("a", "b"), levels = c("b", "a"))),
    factor_texto = list(factor(c("a", "b")), c("a", "b")),
    unidades = list(as.difftime(1, units = "hours"), as.difftime(1, units = "mins")),
    nombres = list(c(edad = 18, peso = 50), c(peso = 18, edad = 50)),
    dimension = list(matrix(1:4, 2L), matrix(1:4, 1L)),
    atributo = list(structure(1, meta = 1), structure(1, meta = 2)),
    columna = list(data.frame(a = 1:3), data.frame(a = c(1L, 2L, 4L))),
    nombre_columna = list(data.frame(a = 1:3), data.frame(b = 1:3)),
    tabla_grande = list(data.frame(a = as.double(1:20000)),
                        data.frame(a = c(as.double(1:19999), 0)))
  )
  for (caso in names(distintos)) {
    a <- h(distintos[[caso]][[1L]])
    expect_match(a, "^[0-9a-f]{32}$", info = caso)
    expect_false(identical(a, h(distintos[[caso]][[2L]])), info = caso)
  }
  # El mismo instante en otro huso no: `format()` lo escribe distinto, y una
  # regla puede leerlo.
  expect_false(identical(
    h(as.POSIXct("2026-01-01 00:00", tz = "UTC")),
    h(as.POSIXct("2025-12-31 21:00", tz = "America/Montevideo"))
  ))
  skip_if_not_installed("bit64")
  expect_false(identical(
    h(bit64::as.integer64("9007199254740993")), h(bit64::as.integer64("9007199254740992"))
  ))
})

test_that("una promesa no se evalua aunque quien llama haya redefinido sus nombres", {
  en <- function(v) {
    force(v)
    .se_metodo_en(environment(), quote(y <- v))
  }
  sello <- function(f) .se_sello_metodo(f)
  antes <- sello(en(letters[1:2]))
  despues <- sello(local({
    letters <- c("p", "q")
    en(letters[1:2])
  }))
  # El mismo codigo da la misma huella, y las dos dicen que es parcial: no se
  # afirma que valgan lo mismo.
  expect_identical(despues$huella_definicion, antes$huella_definicion)
  expect_identical(c(antes$certificacion, despues$certificacion), c("parcial", "parcial"))
})

# --- Niveles de certificacion ---------------------------------------------------

test_that("cada metrica dice cuanto ve su huella: completa, parcial o no certificable", {
  padron <- data.frame(t = c("a", "z"), codigo = 1:2, stringsAsFactors = FALSE)
  m <- suppressWarnings(.se_medir(
    .se_rango(),
    instanciar(especializar(.se_nucleo$NoNulo, "NN", aplicable = ~ y == "si"), "t", "x"),
    instanciar(especializar(.se_nucleo$ReglaIntegridadIntraEntidad, "F",
                            regla = ~ x > 1), "t", "x"),
    instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"),
               "t", "t", referencial = referencial(padron, clave = "t", valor = "codigo")),
    .se_regla_funcion(function(d) !is.na(d$x)),
    instanciar(especializar(.se_nucleo$NoNulo, "NF", aplicable = ~ format(x) == "1"),
               "t", "x"),
    instanciar(especializar(.se_nucleo$ReglaIntegridadIntraEntidad, "GD",
                            regla = function(d) d$x > get("x", envir = globalenv())),
               "t", "x"),
    aplicabilidad = list(x = ~ y %in% c("si", "no")), orden_texto = "bytes"
  ))
  sello <- attr(m, "sello_metricas", exact = TRUE)
  expect_identical(
    names(sello),
    c("id_medicion", "metrica_instanciada", "metrica_especifica", "version_lupa",
      "huella_definicion", "certificacion", "motivo_huella")
  )
  nivel <- stats::setNames(sello$certificacion, sello$metrica_instanciada)
  # Lo que es dato: la especificacion, sus parametros, la formula de la regla,
  # el referencial, la aplicabilidad que solo compara columnas.
  expect_identical(unname(nivel[c("R@t.x", "NN@t.x", "F@t.x", "Ref@t.t")]),
                   rep("completa", 4L))
  expect_true(all(is.na(sello$motivo_huella[nivel == "completa"])))
  # Una funcion de quien llama, o una formula que llama a otra funcion: parcial.
  parciales <- sello[sello$certificacion == "parcial", , drop = FALSE]
  expect_identical(parciales$metrica_instanciada, c("G@t.x", "NF@t.x"))
  expect_match(parciales$motivo_huella[[1L]], "funci\u00f3n de quien llama", fixed = TRUE)
  expect_match(parciales$motivo_huella[[2L]], "`format()`", fixed = TRUE)
  expect_match(parciales$huella_definicion, "^[0-9a-f]{32}$")
  expect_identical(sum(nivel == "no_certificable"), 1L)
})

.se_par_funcion <- function(u_ene, u_feb) {
  # Un metodo propio que lee `u` de su entorno: la huella es parcial.
  crear <- function(u) {
    entorno <- new.env(parent = globalenv())
    entorno$u <- u
    metodo <- eval(quote(function(tablas, instancia) {
      x <- tablas[[instancia$entidad]][[instancia$atributos]]
      if (length(x) < 0L) u
      lupa:::.salida_metodo(
        !is.na(x), instancia$entidad, instancia$atributos, seq_along(x),
        paste0("t[", seq_along(x), ",]")
      )
    }), entorno)
    instanciar(especializar(.se_nucleo$NoNulo, "NN"), "t", "x", metodo = metodo)
  }
  ene <- suppressWarnings(.se_medir(crear(u_ene), id = "ene"))
  feb <- suppressWarnings(medir(
    modelo(crear(u_feb)), list(t = .se_datos()),
    id_medicion = "feb", fecha = as.POSIXct("2026-06-01", tz = "UTC")
  ))
  list(ene = ene, feb = feb)
}

test_that("una huella parcial que no cambia se declara en la deriva y en el historico, sin aviso", {
  par <- .se_par_funcion(1, 1)
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_length(h$avisos, 0L)
  cobertura <- attr(h$valor, "cobertura_sello", exact = TRUE)
  expect_s3_class(cobertura, "data.frame")
  expect_identical(cobertura$metrica_instanciada, "NN@t.x")
  expect_identical(cobertura$id_medicion, "feb")
  expect_identical(cobertura$id_medicion_anterior, "ene")
  expect_identical(cobertura$certificacion, "parcial")
  expect_match(cobertura$descripcion, "huella parcial", fixed = TRUE)
  deriva <- detectar_deriva_calidad(h$valor)
  # La comparacion sigue: no se afirma que cambio.
  expect_true("resultado" %in% as.data.frame(deriva)$aspecto)
  diagnosticos <- attr(deriva, "cobertura_diagnosticos", exact = TRUE)
  expect_true(any(grepl("huella parcial", as.character(unlist(diagnosticos)), fixed = TRUE)))
  # Se conserva al recortar, y `rbind()` y `acumular_historico()` la recalculan.
  expect_s3_class(attr(h$valor[h$valor$id_medicion == "feb", ], "cobertura_sello"), "data.frame")
  acumulado <- acumular_historico(historico_calidad(.se_evaluar(par$ene)), .se_evaluar(par$feb))
  expect_identical(attr(acumulado, "cobertura_sello")$id_medicion, "feb")
  unido <- rbind(historico_calidad(.se_evaluar(par$ene)), historico_calidad(.se_evaluar(par$feb)))
  expect_identical(attr(unido, "cobertura_sello")$id_medicion, "feb")
})

test_that("una huella parcial que cambia avisa como siempre, y una completa igual no declara nada", {
  par <- .se_par_funcion(1, 2)
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_length(h$avisos, 1L)
  expect_match(h$avisos, "cambi\u00f3 de definici\u00f3n", fixed = TRUE)
  expect_null(attr(h$valor, "cobertura_sello", exact = TRUE))
  deriva <- as.data.frame(detectar_deriva_calidad(h$valor))
  expect_true("definicion_metrica" %in% deriva$aspecto)
  # Completa en los dos lados y la misma huella: como antes, nada.
  igual <- .se_par(orden_actual = "idioma")
  h <- .se_avisos(historico_calidad(.se_evaluar(igual$ene), .se_evaluar(igual$feb)))
  expect_null(attr(h$valor, "cobertura_sello", exact = TRUE))
  expect_identical(
    nrow(attr(detectar_deriva_calidad(h$valor), "cobertura_diagnosticos", exact = TRUE)), 0L
  )
})

test_that("el nivel viaja en el texto del sello del historico, y el sello anterior se lee parcial", {
  par <- .se_par_funcion(1, 1)
  h <- suppressWarnings(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  textos <- attr(h, "configuracion_evaluacion", exact = TRUE)$sello_metricas
  expect_true(all(startsWith(textos, "sello-2;")))
  expect_true(all(endsWith(textos, "|parcial")))
  # Un sello de la forma anterior, sin el nivel, no se lee como completo.
  viejo <- sub("|parcial", "", textos, fixed = TRUE)
  viejo <- sub("^sello-2;", "sello-1;", viejo)
  largo <- .sello_largo(viejo)
  expect_identical(largo$certificacion, c("parcial", "parcial"))
  expect_identical(.sello_largo(textos)$huella, largo$huella)
})

# --- Lo que el analisis estatico no ve deja la huella parcial --------------------

test_that("lo que una regla lee sin que la huella lo vea no queda como una huella completa", {
  assign("u_sello_prueba", 1, envir = globalenv())
  assign("fn_sello_prueba", function(v) v > 1, envir = globalenv())
  on.exit(rm(list = c("u_sello_prueba", "fn_sello_prueba"), envir = globalenv()), add = TRUE)
  reglas <- list(
    body_texto = function(d) eval(body("fn_sello_prueba"), list(v = d$x)),
    base_vapply = function(d) base::vapply(d$x, FUN = "fn_sello_prueba", logical(1L)),
    negate = function(d) !Negate("fn_sello_prueba")(d$x),
    local_sombra = function(d) {
      local({
        u_sello_prueba <- 99
      })
      d$x > u_sello_prueba
    },
    assign_global = function(d) {
      assign("u_sello_prueba", 1, pos = 1)
      d$x > u_sello_prueba
    },
    puntos_elt = (function(...) function(d) d$x > ...elt(1L))(1)
  )
  for (caso in names(reglas)) {
    sello <- attr(suppressWarnings(.se_medir(.se_regla_funcion(reglas[[caso]]))),
                  "sello_metricas", exact = TRUE)
    expect_true(sello$certificacion %in% c("parcial", "no_certificable"), info = caso)
    expect_false(identical(sello$certificacion, "completa"), info = caso)
  }
})

test_that("los atributos de una funcion y los datos que la huella no distinguia entran", {
  h <- function(valor) {
    entorno <- new.env(parent = globalenv())
    entorno$v <- valor
    .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) v)))$huella_definicion
  }
  con_atributo <- function(u) structure(function(d) !is.na(d$x), u = u)
  hf <- function(f) {
    attr(suppressWarnings(.se_medir(.se_regla_funcion(f))), "sello_metricas",
         exact = TRUE)$huella_definicion
  }
  expect_false(identical(hf(con_atributo(1)), hf(con_atributo(2))))
  nombres_fila <- data.frame(a = 1:3)
  otros_nombres <- nombres_fila
  rownames(otros_nombres) <- c("x", "y", "z")
  distintos <- list(
    nombres_de_fila = list(nombres_fila, otros_nombres),
    atributos_de_lista = list(structure(list(1, 2), u = 1), structure(list(1, 2), u = 2)),
    dim_de_lista = list(structure(list(1, 2), dim = c(1L, 2L)), list(1, 2)),
    comentario = list(structure(1, class = "k", comment = "a"),
                      structure(1, class = "k", comment = "b")),
    complejo_nan = list(complex(real = 1, imaginary = NaN), complex(real = 2, imaginary = NaN)),
    cero_negativo = list(0, -0),
    cero_negativo_grande = list(c(rep(1, 20000), 0), c(rep(1, 20000), -0))
  )
  for (caso in names(distintos)) {
    expect_false(identical(h(distintos[[caso]][[1L]]), h(distintos[[caso]][[2L]])),
                 info = caso)
  }
})

# --- Nunca materializar ni demorar -------------------------------------------------

test_that("un vector enorme del entorno no se lee: se describe por su tamano, y la huella es parcial", {
  enorme <- 1:1e9
  columna <- data.frame(a = 1:2e6)
  for (valor in list(enorme, columna, as.list(seq_len(2e5)))) {
    entorno <- new.env(parent = globalenv())
    entorno$v <- valor
    tiempo <- system.time(
      sello <- .se_sello_metodo(.se_metodo_en(entorno, quote(if (length(x) < 0L) v)))
    )
    expect_lt(tiempo[["elapsed"]], 3)
    expect_identical(sello$certificacion, "parcial")
    expect_match(sello$motivo_huella, "elementos", fixed = TRUE)
  }
  # El tamano y el tipo si entran.
  otro <- new.env(parent = globalenv())
  otro$v <- 1:2e9
  mismo <- new.env(parent = globalenv())
  mismo$v <- enorme
  expect_false(identical(
    .se_sello_metodo(.se_metodo_en(otro, quote(if (length(x) < 0L) v)))$huella_definicion,
    .se_sello_metodo(.se_metodo_en(mismo, quote(if (length(x) < 0L) v)))$huella_definicion
  ))
})

# --- Sin despacho en normalizacion, niveles y conjuntos ----------------------------

test_that("una normalizacion, unos niveles o un conjunto con clase se describen sin despachar", {
  llamadas <- new.env()
  llamadas$n <- 0L
  contar <- function(x, ...) {
    llamadas$n <- llamadas$n + 1L
    NextMethod()
  }
  for (generico in c("[[<-", "[", "as.character", "length", "duplicated")) {
    registerS3method(generico, "claseNormaSello", contar, envir = baseenv())
  }
  norma <- normalizacion(proteger = c("b", "a"))
  subclase <- norma
  class(subclase) <- c("claseNormaSello", class(norma))
  protegida <- norma
  protegida$proteger <- structure(c("b", "a"), class = "claseNormaSello")
  niveles <- factor(c("a", "b"))
  attr(niveles, "levels") <- structure(c("a", "b"), class = "claseNormaSello")
  entorno <- new.env(parent = globalenv())
  entorno$v <- list(subclase, protegida, niveles)
  metodo <- .se_metodo_en(entorno, quote(if (length(x) < 0L) v))
  conjunto <- modelo(instanciar(especializar(
    .se_nucleo$ValoresPosiblesPorExtension, "VE",
    valores = structure(c(1, 1, 2), dim = c(3L, 1L), class = "claseMatrizSello")
  ), "t", "x"))
  llamadas$n <- 0L
  sello <- .se_sello_metodo(metodo)
  sello_conjunto <- .sello_metricas(conjunto, "c", NULL, "idioma",
                                    .lector_tablas(list(t = .se_datos())))
  expect_identical(llamadas$n, 0L)
  expect_match(sello$huella_definicion, "^[0-9a-f]{32}$")
  # El conjunto con clase y dimensiones ya no falla al quitar repetidos: es
  # parcial por su clase propia, no por un error.
  expect_identical(sello_conjunto$certificacion, "parcial")
  expect_match(sello_conjunto$motivo_huella, "valor con clase propia", fixed = TRUE)
})

# --- Lo compartido se describe una vez ---------------------------------------------

test_that("un referencial y un factor que comparten varias metricas se describen una vez", {
  padron <- data.frame(t = sprintf("v%05d", 1:20000), codigo = 1:20000,
                       stringsAsFactors = FALSE)
  ref <- referencial(padron, clave = "t", valor = "codigo")
  grande <- factor(sprintf("n%05d", 1:20000))
  entorno <- new.env(parent = globalenv())
  entorno$g <- grande
  regla <- eval(quote(function(d) d$x > 0 & length(g) > 0), entorno)
  columnas <- c("x", "t", "y")
  instancias <- c(
    lapply(1:3, function(i) {
      instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, paste0("Ref", i)),
                 "t", columnas[[i]], referencial = ref, nombre_instancia = paste0("Ref", i))
    }),
    lapply(1:3, function(i) {
      instanciar(especializar(.se_nucleo$ReglaIntegridadIntraEntidad, paste0("G", i),
                              regla = regla), "t", columnas[[i]],
                 nombre_instancia = paste0("G", i))
    })
  )
  original_ref <- lupa:::.referencial_para_huella
  original_factor <- lupa:::.texto_de_factor
  cuenta <- new.env()
  cuenta$ref <- 0L
  cuenta$factor <- 0L
  local_mocked_bindings(
    .referencial_para_huella = function(referencial, contexto) {
      cuenta$ref <- cuenta$ref + 1L
      original_ref(referencial, contexto)
    },
    .texto_de_factor = function(x) {
      cuenta$factor <- cuenta$factor + 1L
      original_factor(x)
    },
    .package = "lupa"
  )
  sello <- .sello_metricas(do.call(modelo, instancias), "c", NULL, "idioma",
                           .lector_tablas(list(t = .se_datos())))
  expect_identical(cuenta$ref, 1L)
  expect_identical(cuenta$factor, 1L)
  expect_identical(length(unique(sello$huella_definicion[1:3])), 3L)
})

# --- Completa solo con valores de clases de dato --------------------------------

test_that("un parametro con una clase propia deja la huella parcial; las clases de dato no", {
  nivel <- function(valores_nulos) {
    m <- suppressWarnings(.se_medir(instanciar(
      especializar(.se_nucleo$NoNulo, "NN", valores_nulos = valores_nulos), "t", "t"
    )))
    attr(m, "sello_metricas", exact = TRUE)
  }
  propio <- nivel(structure("a", class = "claseParametroSello"))
  expect_identical(propio$certificacion, "parcial")
  expect_match(propio$motivo_huella, "valor con clase propia", fixed = TRUE)
  expect_match(propio$motivo_huella, "claseParametroSello", fixed = TRUE)
  # Las clases que el paquete lee como dato siguen completas.
  dato <- list(
    factor = factor("a"), fecha = as.Date("2026-01-01"),
    instante = as.POSIXct("2026-01-01", tz = "UTC"),
    duracion = as.difftime(1, units = "days")
  )
  for (caso in names(dato)) {
    expect_identical(nivel(dato[[caso]])$certificacion, "completa", info = caso)
  }
  # Una tabla con columnas de esas clases, como referencial, tambien.
  padron <- data.frame(t = c("a", "z"), dia = as.Date(c("2026-01-01", "2026-01-02")),
                       stringsAsFactors = FALSE)
  m <- suppressWarnings(.se_medir(instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"), "t", "t",
    referencial = referencial(padron, clave = "t", valor = "dia")
  )))
  expect_identical(attr(m, "sello_metricas", exact = TRUE)$certificacion, "completa")
})

test_that("un metodo de quien llama para una clase de dato o del paquete deja la huella parcial", {
  padron <- data.frame(t = c("a", "z"), stringsAsFactors = FALSE)
  instancia <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte, "Ref"), "t", "t",
    referencial = referencial(padron, clave = "t")
  )
  nivel <- function() {
    attr(suppressWarnings(.se_medir(instancia)), "sello_metricas", exact = TRUE)
  }
  expect_identical(nivel()$certificacion, "completa")
  # El metodo no cambia nada; alcanza con que exista para que no se afirme.
  assign("$.referencial", function(x, name) .subset2(unclass(x), name),
         envir = globalenv())
  on.exit(rm("$.referencial", envir = globalenv()), add = TRUE)
  con_metodo <- nivel()
  expect_identical(con_metodo$certificacion, "parcial")
  expect_match(con_metodo$motivo_huella, "`referencial`", fixed = TRUE)
  expect_match(con_metodo$motivo_huella, "escrito por quien llama", fixed = TRUE)
  rm("$.referencial", envir = globalenv())
  # Registrado en una tabla de metodos, para una clase del paquete, tambien.
  tabla <- get(".__S3MethodsTable__.", envir = baseenv())
  assign("$.referencial", function(x, name) .subset2(unclass(x), name), envir = tabla)
  registrado <- tryCatch(nivel(), finally = rm("$.referencial", envir = tabla))
  expect_identical(registrado$certificacion, "parcial")
  expect_identical(nivel()$certificacion, "completa")
  assign("$.referencial", function(x, name) .subset2(unclass(x), name),
         envir = globalenv())
  # Una fecha con un `format()` de quien llama, igual.
  assign("format.Date", function(x, ...) "otra", envir = globalenv())
  on.exit(rm("format.Date", envir = globalenv()), add = TRUE)
  fecha <- suppressWarnings(.se_medir(instanciar(
    especializar(.se_nucleo$NoNulo, "NN", valores_nulos = as.Date("2026-01-01")), "t", "t"
  )))
  expect_identical(attr(fecha, "sello_metricas", exact = TRUE)$certificacion, "parcial")
})

test_that("las formulas comunes siguen completas", {
  formulas <- list(
    ~ y == "si", ~ y %in% c("si", "no"), ~ grepl("^s", y),
    ~ ifelse(is.na(x), FALSE, x > 1), ~ nchar(t) > 0 & !is.na(t)
  )
  for (i in seq_along(formulas)) {
    m <- suppressWarnings(.se_medir(instanciar(
      especializar(.se_nucleo$NoNulo, "NN", aplicable = formulas[[i]]), "t", "x"
    )))
    expect_identical(attr(m, "sello_metricas", exact = TRUE)$certificacion, "completa",
                     info = i)
  }
  reglas <- list(~ x > 1, ~ t %in% c("a", "b"), ~ is.na(x) | x >= 0)
  for (i in seq_along(reglas)) {
    m <- suppressWarnings(.se_medir(instanciar(
      especializar(.se_nucleo$ReglaIntegridadIntraEntidad, "F", regla = reglas[[i]]),
      "t", c("x", "t")
    )))
    expect_identical(attr(m, "sello_metricas", exact = TRUE)$certificacion, "completa",
                     info = i)
  }
})

test_that("un vector compacto y su copia comun tienen la misma huella", {
  compacto <- 1:20000
  comun <- compacto
  comun[1L] <- 1L
  sello <- function(v) {
    .sello_metricas(
      modelo(instanciar(especializar(.se_nucleo$NoNulo, "NN", valores_nulos = v), "t", "x")),
      "c", NULL, "idioma", .lector_tablas(list(t = .se_datos()))
    )
  }
  expect_identical(sello(compacto)$huella_definicion, sello(comun)$huella_definicion)
  expect_identical(sello(compacto)$certificacion, "completa")
})

# --- El entorno de la sesion -------------------------------------------------------

.se_par_entorno <- function(cambiar, restaurar) {
  # La misma definicion medida dos veces; entre una y otra cambia el entorno.
  datos <- data.frame(t = c("100000", "1", NA), stringsAsFactors = FALSE)
  modelo_nn <- modelo(instanciar(
    especializar(.se_nucleo$NoNulo, "NN", valores_nulos = 100000), "t", "t"
  ))
  ene <- medir(modelo_nn, list(t = datos), id_medicion = "ene",
               fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  cambiar()
  on.exit(restaurar())
  feb <- medir(modelo_nn, list(t = datos), id_medicion = "feb",
               fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  list(ene = ene, feb = feb)
}

test_that("el sello registra el entorno de la sesion, aparte de la huella", {
  m <- .se_medir(.se_rango(), id = "c")
  entorno <- attr(attr(m, "sello_metricas", exact = TRUE), "entorno", exact = TRUE)
  expect_identical(names(entorno), "c")
  for (parte in c("R=", ";scipen=", ";OutDec=", ";LC_COLLATE=", ";LC_CTYPE=",
                  ";LC_TIME=", ";TZ=")) {
    expect_match(entorno, parte, fixed = TRUE, info = parte)
  }
  # Mismo entorno, misma definicion: la huella no lo lleva.
  anterior <- options(scipen = 100)
  otro <- .se_medir(.se_rango(), id = "c")
  options(anterior)
  expect_identical(.se_huella(otro), .se_huella(m))
  expect_false(identical(
    attr(attr(otro, "sello_metricas"), "entorno"), entorno
  ))
})

test_that("misma definicion en otro entorno: se avisa y se declara, sin retirar la comparacion", {
  anterior <- NULL
  par <- .se_par_entorno(
    function() anterior <<- options(scipen = 100),
    function() options(anterior)
  )
  # La premisa: el entorno cambia lo medido con la misma definicion.
  expect_false(identical(as.data.frame(par$ene)$resultado, as.data.frame(par$feb)$resultado))
  expect_identical(.se_huella(par$ene), .se_huella(par$feb))
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_length(h$avisos, 1L)
  expect_match(h$avisos, "misma definici\u00f3n, otro entorno", fixed = TRUE)
  expect_match(h$avisos, "scipen: 0 -> 100", fixed = TRUE)
  cobertura <- attr(h$valor, "cobertura_sello", exact = TRUE)
  expect_identical(cobertura$aspecto, "entorno")
  expect_identical(cobertura$id_medicion, "feb")
  expect_match(cobertura$descripcion, "scipen: 0 -> 100", fixed = TRUE)
  deriva <- detectar_deriva_calidad(h$valor)
  expect_true("resultado" %in% as.data.frame(deriva)$aspecto)
  expect_false("definicion_metrica" %in% as.data.frame(deriva)$aspecto)
  diagnosticos <- as.character(unlist(attr(deriva, "cobertura_diagnosticos", exact = TRUE)))
  expect_true(any(grepl("otro entorno", diagnosticos, fixed = TRUE)))
  # `rbind()` de las dos mediciones avisa lo mismo.
  unido <- .se_avisos(rbind(par$ene, par$feb))
  expect_length(unido$avisos, 1L)
  expect_match(unido$avisos, "otro entorno", fixed = TRUE)
  expect_identical(
    names(attr(attr(unido$valor, "sello_metricas"), "entorno")), c("ene", "feb")
  )
  # El texto del historico lleva el entorno, y un CSV lo conserva.
  textos <- attr(h$valor, "configuracion_evaluacion", exact = TRUE)$sello_metricas
  expect_true(all(grepl(";entorno=", textos, fixed = TRUE)))
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  utils::write.csv(h$valor, archivo, row.names = FALSE)
  releida <- detectar_deriva_calidad(utils::read.csv(archivo, stringsAsFactors = FALSE))
  expect_true(any(grepl("otro entorno", as.character(unlist(
    attr(releida, "cobertura_diagnosticos", exact = TRUE)
  )), fixed = TRUE)))
})

test_that("el huso y el separador decimal tambien son parte del entorno", {
  huso <- Sys.getenv("TZ", unset = NA)
  par <- .se_par_entorno(
    function() Sys.setenv(TZ = "Asia/Tokyo"),
    function() if (is.na(huso)) Sys.unsetenv("TZ") else Sys.setenv(TZ = huso)
  )
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_match(h$avisos, "TZ: ", fixed = TRUE)
  expect_match(h$avisos, "-> Asia/Tokyo", fixed = TRUE)
  anterior <- NULL
  par <- .se_par_entorno(
    function() anterior <<- options(OutDec = ","),
    function() options(anterior)
  )
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_match(h$avisos, "OutDec: . -> ,", fixed = TRUE)
})

test_that("un sello sin entorno se lee como entorno desconocido, sin aviso", {
  par <- .se_par_entorno(function() NULL, function() NULL)
  sin_entorno <- par$ene
  sello <- attr(sin_entorno, "sello_metricas", exact = TRUE)
  attr(sello, "entorno") <- NULL
  attr(sin_entorno, "sello_metricas") <- sello
  h <- .se_avisos(historico_calidad(.se_evaluar(sin_entorno), .se_evaluar(par$feb)))
  expect_length(h$avisos, 0L)
  cobertura <- attr(h$valor, "cobertura_sello", exact = TRUE)
  expect_identical(cobertura$aspecto, "entorno_desconocido")
  expect_match(cobertura$descripcion, "entorno desconocido", fixed = TRUE)
  diagnosticos <- as.character(unlist(attr(
    detectar_deriva_calidad(h$valor), "cobertura_diagnosticos", exact = TRUE
  )))
  expect_true(any(grepl("entorno desconocido", diagnosticos, fixed = TRUE)))
  # Mismo entorno conocido: nada que declarar.
  h <- .se_avisos(historico_calidad(.se_evaluar(par$ene), .se_evaluar(par$feb)))
  expect_length(h$avisos, 0L)
  expect_null(attr(h$valor, "cobertura_sello", exact = TRUE))
})

test_that("las funciones de base que solo leen sus argumentos y el entorno registrado dejan la formula completa", {
  nivel <- function(formula) {
    m <- suppressWarnings(.se_medir(instanciar(
      especializar(.se_nucleo$NoNulo, "NN", aplicable = formula), "t", "x"
    )))
    attr(m, "sello_metricas", exact = TRUE)$certificacion
  }
  completas <- list(
    ~ paste0(t, "x") == "ax", ~ sprintf("%s", t) == "a", ~ gsub("[0-9]", "", t) == "a",
    ~ sub("a", "b", t) == "b", ~ regexpr("a", t) > 0, ~ x %in% which(x > 1),
    ~ is.numeric(x), ~ is.character(t), ~ x > mean(x, na.rm = TRUE),
    ~ x > median(x, na.rm = TRUE), ~ x > sd(x, na.rm = TRUE), ~ rev(x) > 1,
    ~ x > length(table(t)), ~ cumsum(!is.na(x)) > 1, ~ sort(t)[1] == t
  )
  for (i in seq_along(completas)) {
    expect_identical(nivel(completas[[i]]), "completa", info = i)
  }
  # Lo que lee algo que no se registra -`digits`, el reloj- o aplica otra
  # funcion, no.
  parciales <- list(~ format(x) == "1", ~ Sys.Date() > as.Date("2020-01-01"),
                    ~ sapply(strsplit(t, ""), length) > 1)
  for (i in seq_along(parciales)) {
    expect_identical(nivel(parciales[[i]]), "parcial", info = i)
  }
})

test_that("la misma corrida acumulada desde otra sesion no choca por su entorno", {
  e <- .se_evaluar(.se_medir(.se_rango(), id = "ene"))
  base <- historico_calidad(e)
  anterior <- options(scipen = 100)
  otra_sesion <- historico_calidad(.se_evaluar(.se_medir(.se_rango(), id = "ene")))
  options(anterior)
  juntos <- expect_no_error(acumular_historico(base, otra_sesion))
  expect_identical(nrow(juntos), nrow(base))
  # Se queda el entorno que ya estaba.
  expect_identical(
    attr(juntos, "configuracion_evaluacion", exact = TRUE)$sello_metricas,
    attr(base, "configuracion_evaluacion", exact = TRUE)$sello_metricas
  )
})
