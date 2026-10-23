# Las piezas compartidas, cruzadas. Cada bloque es un defecto, con el caso que
# lo reproducia y la cifra rehecha a mano. Lo que necesita un locale que la
# maquina no tiene se salta. El archivo es ASCII: lo que no lo es se arma con
# `intToUtf8()`.

# Un `LC_NUMERIC` con coma decimal, el primero que la maquina tenga.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.con_numerica_o160 <- function(codigo) {
  anterior <- Sys.getlocale("LC_NUMERIC")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", anterior)), add = TRUE)
  for (candidato in c("es_UY.UTF-8", "es_UY.utf8", "de_DE.UTF-8", "de_DE.utf8",
                      "es_ES.UTF-8", "es_ES.utf8", "fr_FR.UTF-8", "fr_FR.utf8")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_NUMERIC", candidato), error = function(e) ""
    ))
    if (length(puesto) == 1L && !is.na(puesto) && nzchar(puesto) &&
        identical(Sys.localeconv()[["decimal_point"]], ",")) {
      return(force(codigo))
    }
  }
  skip("no hay un locale con coma decimal para LC_NUMERIC")
}

# Diez instantes con fraccion de segundo, como texto escrito a mano -no con
# `format()`, que con coma escribiria `10:00:00,250`-. Los desplazamientos desde
# las 10:00:00 son los del texto: 0, 0.25, 0.5, 1, 1.75, 2, 2.5, 3, 3.25 y 4.
.desplazamientos_o160 <- c(0, 0.25, 0.5, 1, 1.75, 2, 2.5, 3, 3.25, 4)
.textos_fraccion_o160 <- c(
  "2026-03-01 10:00:00.000", "2026-03-01 10:00:00.250",
  "2026-03-01 10:00:00.500", "2026-03-01 10:00:01.000",
  "2026-03-01 10:00:01.750", "2026-03-01 10:00:02.000",
  "2026-03-01 10:00:02.500", "2026-03-01 10:00:03.000",
  "2026-03-01 10:00:03.250", "2026-03-01 10:00:04.000"
)
# Las 10:00:00 del 2026-03-01 en UTC, sin leer texto: dias desde 1970 por
# `as.Date()` -que no lee fraccion- y 36000 segundos.
.diez_utc_o160 <- function() as.numeric(as.Date("2026-03-01")) * 86400 + 36000

test_that("H1: un plan armado en C y aplicado con LC_NUMERIC en coma escribe la fraccion de segundo", {
  d <- data.frame(t = .textos_fraccion_o160, stringsAsFactors = FALSE)
  plan <- suppressMessages(suppressWarnings(
    planificar_limpieza(perfilar(d), datos = d)
  ))
  fila <- which(plan$columna == "t" & plan$estrategia == "convertir_tipo")
  expect_length(fila, 1L)
  expect_identical(as.character(plan$estado[fila]), "lista")
  avisos <- character()
  resultado <- .con_numerica_o160(withCallingHandlers(
    aplicar(plan, d),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  escritos <- as.numeric(resultado$datos$t)
  expect_s3_class(resultado$datos$t, "POSIXct")
  # Con la fraccion leida con la marca de la sesion se escribian
  # floor(desplazamientos): 5 instantes distintos de 10, sin aviso.
  expect_equal(escritos - .diez_utc_o160(), .desplazamientos_o160,
               tolerance = 1e-6)
  expect_identical(length(unique(escritos)), 10L)
  expect_identical(avisos, character())
})

test_that("H1: con LC_NUMERIC en coma el plan declara la conversion reversible, como en C", {
  d <- data.frame(t = .textos_fraccion_o160, stringsAsFactors = FALSE)
  planear <- function() {
    plan <- suppressMessages(suppressWarnings(
      planificar_limpieza(perfilar(d), datos = d)
    ))
    fila <- which(plan$columna == "t" & plan$estrategia == "convertir_tipo")
    list(estado = as.character(plan$estado[fila]),
         reversible = plan$reversible[fila])
  }
  en_c <- planear()
  en_coma <- .con_numerica_o160(planear())
  expect_identical(en_c, list(estado = "lista", reversible = TRUE))
  # Antes: `bloqueada`, "9 valores participan en una conversion no inyectiva".
  expect_identical(en_coma, en_c)
})

test_that("H1: una fecha de texto con fraccion se guarda con la fraccion con LC_NUMERIC en coma", {
  # 10:00:05.25: 5.25 segundos despues de las 10:00:00. Se compara el
  # desplazamiento: la tolerancia de `expect_equal()` es relativa, y sobre el
  # instante entero 0.25 segundos no la superan.
  esperado <- 5.25
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "e", "x"))
  d <- data.frame(x = c(1, NA))
  medido <- .con_numerica_o160(list(
    medir = as.numeric(unique(medir(mod, d, id_medicion = "m",
                                    fecha = "2026-03-01 10:00:05.25")$fecha)) -
      .diez_utc_o160(),
    perfilar = as.numeric(suppressMessages(
      perfilar(d, fecha = "2026-03-01 10:00:05.25")
    )$meta$fecha_hora) - .diez_utc_o160(),
    numerica = Sys.getlocale("LC_NUMERIC")
  ))
  expect_equal(medido$medir, esperado, tolerance = 1e-6)
  expect_equal(medido$perfilar, esperado, tolerance = 1e-6)
  # La llamada deja el `LC_NUMERIC` como lo encontro.
  expect_false(medido$numerica %in% c("C", "POSIX"))
})

test_that("H1: el id_medicion por omision se escribe con punto con LC_NUMERIC en coma", {
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "e", "x"))
  d <- data.frame(x = c(1, NA))
  fecha <- as.POSIXct(.diez_utc_o160() + 5.25, origin = "1970-01-01", tz = "UTC")
  id <- .con_numerica_o160(unique(medir(mod, d, fecha = fecha)$id_medicion))
  expect_true(startsWith(id, "medicion-20260301T100005.250000-"))
})

test_that("Guarda H1: toda lectura de texto con formato pasa por el lector que fija LC_NUMERIC", {
  # Por construccion: un `strptime()` -o un `as.POSIXct()`/`as.POSIXlt()` con
  # `format` o `tryFormats`, que llaman a `strptime()`- fuera del lector comun
  # leeria la fraccion de `%OS` con la marca de la sesion. Lo que queda fuera:
  # `as.POSIXct()` de un texto sin formato en `.rango_perfil()` -lee extremos que
  # el perfil escribe con `%S`, sin fraccion- y en
  # `.rango_desde_agregados_coleccion()` -solo desde `relaciones_coleccion()`,
  # que ya fija "C" en la entrada-.
  ns <- asNamespace("lupa")
  lectores <- character()
  visitar <- function(e, nombre) {
    if (!is.call(e) || !is.name(e[[1L]])) return(invisible())
    cabeza <- as.character(e[[1L]])
    con_formato <- cabeza %in% c("as.POSIXct", "as.POSIXlt") &&
      any(c("format", "tryFormats") %in% names(e))
    if (identical(cabeza, "strptime") || con_formato) {
      lectores <<- c(lectores, nombre)
    }
    invisible()
  }
  recorrer <- function(e, nombre) {
    visitar(e, nombre)
    if (is.call(e)) {
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]], nombre)
      }
    }
    invisible()
  }
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto)) {
      recorrer(body(objeto), nombre)
    }
  }
  expect_identical(unique(lectores), ".strptime_utc")
  # Y el lector fija la marca antes de leer.
  cuerpo <- as.list(body(ns[[".strptime_utc"]]))
  expect_identical(cuerpo[[2L]], quote(.fijar_numerica_c()))
})

test_that("H2: una configuracion escrita con 15 cifras declara el cambio de modelo una sola vez", {
  # Limitacion declarada en `?detectar_deriva_calidad`, no un arreglo: la version
  # anterior escribia `maximo = 1/3` con 15 cifras, que no vuelven a su valor, y
  # la actual con las 16 que si. Rehecho a mano:
  vieja <- "0.333333333333333"
  nueva <- "0.3333333333333333"
  expect_false(as.numeric(vieja) == 1 / 3)
  expect_true(as.numeric(nueva) == 1 / 3)
  n <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(n$ValoresPosiblesPorComprension, nombre_especifico = "Rango",
                 minimo = 0, maximo = 1 / 3),
    "personas", "p"
  ))
  d <- data.frame(p = c(0.1, 0.2, 0.3, 0.25))
  pe <- perfil_evaluacion("Basico", regla_evaluacion("Presente", function(x) x > 0.5))
  corrida <- function(id, fecha) {
    m <- medir(mod, d, id_medicion = id, fecha = fecha)
    historico_calidad(m, evaluar(m, pe))
  }
  enero <- corrida("enero", "2026-01-31")
  expect_true(any(grepl(paste0("double[", nueva, "]"), enero$configuracion_modelo,
                        fixed = TRUE)))
  # La corrida de enero, como la habria guardado la version anterior.
  a_vieja <- function(x) sub(paste0("double[", nueva, "]"),
                             paste0("double[", vieja, "]"), x, fixed = TRUE)
  enero$configuracion_modelo <- a_vieja(enero$configuracion_modelo)
  configuracion <- attr(enero, "configuracion_evaluacion")
  configuracion$configuracion_modelo <- a_vieja(configuracion$configuracion_modelo)
  attr(enero, "configuracion_evaluacion") <- configuracion
  h <- acumular_historico(
    acumular_historico(enero, corrida("febrero", "2026-02-28")),
    corrida("marzo", "2026-03-31")
  )
  deriva <- as.data.frame(detectar_deriva_calidad(h))
  cambios <- deriva[deriva$aspecto == "configuracion_modelo", ]
  # Una vez, contra la primera corrida nueva; desde ahi la serie aparea.
  expect_identical(nrow(cambios), 1L)
  expect_identical(c(cambios$id_medicion_anterior, cambios$id_medicion_actual),
                   c("enero", "febrero"))
  # Y la fila de resultado del par conserva su veredicto.
  resultados <- deriva[deriva$aspecto == "resultado", ]
  expect_identical(resultados$id_medicion_actual, c("febrero", "marzo"))
  expect_identical(as.character(resultados$severidad), c("ok", "ok"))
})

# Bajo `LC_CTYPE` "C", con los avisos "unable to translate" que deje la llamada.
.traducciones_bajo_c_o160 <- function(codigo) {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", anterior)), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", "C"), error = function(e) ""
  ))
  if (!identical(puesto, "C")) skip("no se pudo fijar LC_CTYPE en C")
  avisos <- character()
  valor <- withCallingHandlers(
    suppressMessages(codigo),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, traducciones = grep("translate", avisos, value = TRUE))
}

test_that("H3: evaluar() bajo C con un perfil y una regla no ASCII no avisa unable to translate", {
  basico <- paste0("B", intToUtf8(0xe1L), "sico")
  presente <- paste0("Presente", intToUtf8(0xe9L))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "edad"))
  m <- medir(mod, data.frame(edad = c(20, NA, 35, 41)), id_medicion = "m",
             fecha = "2026-03-01")
  pe <- perfil_evaluacion(basico, regla_evaluacion(presente, function(x) x > 0.5))
  en_utf8 <- evaluar(m, pe)
  bajo_c <- .traducciones_bajo_c_o160(evaluar(m, pe))
  # Antes: dos avisos por perfil y regla -`do.call(rbind, partes)` con las
  # partes nombradas por los grupos-, y con `warn = 2` un error.
  expect_identical(bajo_c$traducciones, character())
  expect_identical(bajo_c$valor$reglas, en_utf8$reglas)
  expect_identical(bajo_c$valor$perfiles, en_utf8$perfiles)
  # NoNulo mide por fila sobre c(20, NA, 35, 41): 1, 0, 1, 1; la regla x > 0.5
  # se cumple en 3 de 4.
  expect_identical(bajo_c$valor$reglas$resultado, 3 / 4)
})

test_that("H3: aplicar() bajo C con columnas no ASCII no avisa unable to translate", {
  d <- data.frame(x = c(20, 20, 35), y = c("a", "a", "b"), stringsAsFactors = FALSE)
  names(d) <- c(paste0("a", intToUtf8(0xf1L), "o"),
                paste0("ciudad", intToUtf8(0xe9L)))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(perfilar(d), datos = d)))
  fila <- which(plan$estrategia == "conservar_primera_duplicada")
  expect_length(fila, 1L)
  plan$aplicar <- FALSE
  plan$aplicar[fila] <- TRUE
  bajo_c <- .traducciones_bajo_c_o160(aplicar(plan, d, permitir_eliminacion = TRUE))
  expect_identical(bajo_c$traducciones, character())
  # Las filas 1 y 2 son iguales: queda la primera de las dos y la tercera.
  expect_identical(nrow(bajo_c$valor$datos), 2L)
})

test_that("H3: perfilar_dbi() por bloques y relaciones_coleccion() bajo C con nombres no ASCII no avisan", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  pais <- paste0("pa", intToUtf8(0xedL), "s")
  d <- data.frame(id = 1:4, x = c(1.5, 2.5, 1.5, 3))
  names(d)[2] <- paste0("a", intToUtf8(0xf1L), "o")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, pais, d)
  DBI::dbWriteTable(con, paste0(pais, "2"), d[1:2, ])
  bloques <- .traducciones_bajo_c_o160(perfilar_dbi(con, pais, bloque_filas = 2))
  expect_identical(bloques$traducciones, character())
  # media de c(1.5, 2.5, 1.5, 3) = 8.5 / 4 = 2.125
  columnas <- bloques$valor$resumen_tabla$columnas
  expect_equal(columnas$media[columnas$columna == names(d)[2]], 2.125)
  col <- suppressMessages(coleccion(con, c(pais, paste0(pais, "2"))))
  pares <- data.frame(tabla_1 = pais, tabla_2 = paste0(pais, "2"),
                      stringsAsFactors = FALSE)
  relaciones <- .traducciones_bajo_c_o160(relaciones_coleccion(col, pares = pares))
  expect_identical(relaciones$traducciones, character())
})

test_that("H4: los errores de argumento de perfilar_dbi() heredan de lupa_error_dbi", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o160_t", data.frame(id = 1:3, x = c(0.5, 1.5, 2.5)))
  # `?perfilar_dbi`, seccion Fallo parcial: "Los stop() de esta via llevan clase
  # de condicion propia -todas heredan de lupa_error_dbi-". Estos salian
  # `simpleError` (o, `incluir_valores = NA`, el error de R).
  casos <- list(
    list(universo = "x"), list(estrategia_mediana = NA),
    list(bloque_muestra = "x"), list(dialecto = "x"),
    list(estrategia_distintos = "x"), list(muestra = -1),
    list(max_celdas_muestra = -1), list(max_bytes_muestra = -1),
    list(incluir_valores = NA), list(proteger_datos_personales = NA)
  )
  for (caso in casos) {
    error <- tryCatch(
      do.call(perfilar_dbi, c(list(con, "o160_t"), caso)),
      error = function(e) e
    )
    expect_s3_class(error, "lupa_error_argumento_dbi")
    expect_s3_class(error, "lupa_error_dbi")
    # El mensaje nombra el argumento.
    expect_true(grepl(names(caso), conditionMessage(error), fixed = TRUE),
                info = names(caso))
  }
})

test_that("Guarda H4: ningun argumento de perfilar_dbi() en NA aborta sin la clase de la via", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o160_t", data.frame(id = 1:3, x = c(0.5, 1.5, 2.5)))
  # Por construccion: cada formal, tambien uno nuevo, con `NA`. Lo que lo acepta
  # pasa; lo que lo rechaza tiene que hacerlo con la clase de la via.
  formales <- setdiff(names(formals(perfilar_dbi)), c("conexion", "tabla", "..."))
  rechazados <- 0L
  for (formal in formales) {
    argumentos <- list(con, "o160_t")
    argumentos[formal] <- list(NA)
    error <- tryCatch({
      suppressWarnings(suppressMessages(do.call(perfilar_dbi, argumentos)))
      NULL
    }, error = function(e) e)
    if (is.null(error)) next
    rechazados <- rechazados + 1L
    expect_s3_class(error, "lupa_error_dbi")
  }
  # Que la guarda midio: la mayoria de los formales rechaza `NA` (30 de 33 al
  # escribirla).
  expect_gt(rechazados, length(formales) / 2)
})

test_that("H5: perfilar_por() lee igual la bandera abreviada y la entera", {
  d <- data.frame(
    correo = rep(c("ana@ejemplo.com", "luis@ejemplo.com"), each = 3),
    x = c(1, 2, NA, 4, 5, 6), stringsAsFactors = FALSE
  )
  abreviada <- suppressWarnings(suppressMessages(
    perfilar_por(d, "correo", min_filas = 1, proteger = FALSE)
  ))
  entera <- suppressWarnings(suppressMessages(
    perfilar_por(d, "correo", min_filas = 1, proteger_datos_personales = FALSE)
  ))
  # `perfilar()` recibia `proteger_datos_personales = FALSE` -R completa el
  # nombre- y `perfilar_por()` leia la bandera por el nombre exacto: ausente,
  # protegia lo suyo.
  expect_identical(abreviada, entera)
})

test_that("H5: perfilar_dbi() lee igual la bandera abreviada y la entera, y la valida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o160_p", data.frame(
    id = 1:6, correo = paste0("ana", c(1, 1, 1, 2, 3, 4), "@ejemplo.com"),
    stringsAsFactors = FALSE
  ))
  estados <- function(...) {
    r <- suppressWarnings(suppressMessages(perfilar_dbi(con, "o160_p", ...)))
    sql <- r$resumen_tabla$sql
    list(
      muestra = r$perfil_muestra$meta$proteger_datos_personales,
      moda_correo = sql$estado[sql$columna == "correo" & sql$metrica == "moda"]
    )
  }
  entera <- estados(proteger_datos_personales = FALSE)
  # Antes la muestra salia sin proteccion y la moda del resumen SQL
  # `omitido_por_privacidad`: dos protecciones en un objeto.
  expect_identical(estados(proteger = FALSE), entera)
  expect_false(entera$muestra)
  expect_false("omitido_por_privacidad" %in% entera$moda_correo)
  # Y un valor invalido abreviado da el error del paquete, no el fallo parcial.
  error <- tryCatch(perfilar_dbi(con, "o160_p", proteger = "si"),
                    error = function(e) e)
  expect_s3_class(error, "lupa_error_argumento_dbi")
  expect_true(grepl("proteger_datos_personales", conditionMessage(error),
                    fixed = TRUE))
})

test_that("H6: perfilar_por() valida la fecha y los nombres de ... aunque ningun grupo llegue a min_filas", {
  d <- data.frame(a = 1:10, g = rep(c("x", "y"), 5))
  # 5 filas por grupo: con `min_filas = 100` `perfilar()` no corre, y antes la
  # llamada pasaba en silencio. Con `min_filas = 1` ya daba estos errores.
  expect_error(perfilar_por(d, "g", min_filas = 100, fecha = "ayer"),
               "`fecha` debe contener una fecha y hora v", fixed = TRUE)
  expect_error(perfilar_por(d, "g", min_filas = 100, fecha = c("2026-01-01", "2026-01-02")),
               "`fecha` debe contener una fecha y hora v", fixed = TRUE)
  expect_error(perfilar_por(d, "g", min_filas = 100, no_existe = 1),
               "`no_existe` no es un argumento de `perfilar()`", fixed = TRUE)
  # Una fecha valida y un nombre abreviado de un formal siguen pasando.
  expect_no_error(suppressWarnings(suppressMessages(
    perfilar_por(d, "g", min_filas = 100, fecha = "2026-03-01", max_pat = 5)
  )))
})

test_that("H6: perfilar_dbi() y perfilar_coleccion() validan la fecha y los nombres de ... sin muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o160_t", data.frame(a = 1:10, b = letters[1:10]))
  DBI::dbWriteTable(con, "o160_u", data.frame(a = 1:5))
  error <- tryCatch(
    perfilar_dbi(con, "o160_t", bloque_muestra = "solo_agregados", fecha = "ayer"),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_argumento_dbi")
  error <- tryCatch(
    perfilar_dbi(con, "o160_t", bloque_muestra = "solo_agregados", no_existe = 1),
    error = function(e) e
  )
  expect_s3_class(error, "lupa_error_argumento_dbi")
  # La coleccion lo valida una vez, en la entrada: tabla por tabla, cada
  # `perfilar_dbi()` habria fallado entero y la coleccion perdia los resumenes.
  declarada <- suppressMessages(coleccion(con, c("o160_t", "o160_u")))
  expect_error(perfilar_coleccion(declarada, fecha = "ayer"),
               "`fecha` debe contener una fecha y hora v", fixed = TRUE)
  # Un argumento de `perfilar_dbi()` por `...` no es de `perfilar()`, y pasa.
  expect_no_error(suppressWarnings(suppressMessages(
    perfilar_coleccion(declarada, bloque_filas = 3)
  )))
})

test_that("H7: con proteccion, el motivo de un error de argumento de la muestra sale legible", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o160_t", data.frame(a = 1:10, b = letters[1:10]))
  r <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "o160_t", max_columnas_orden = Inf)
  ))
  expect_null(r$perfil_muestra)
  motivos <- r$resumen_tabla$cobertura$motivo
  # Antes: "`[literal omitido]` debe ser un entero [literal omitido] de al
  # [literal omitido] 2." El texto es del paquete y se repite sin ninguna fila.
  expect_true(any(grepl(
    "`max_columnas_orden` debe ser un entero finito de al menos 2.",
    motivos, fixed = TRUE
  )))
})

test_that("H7: un error que no se repite sin filas sigue pasando por el piso entero", {
  # Control del criterio: con argumentos validos `perfilar()` sobre la muestra
  # sin filas no falla, asi que un error que cita una celda -aca, armado a mano-
  # no es de los argumentos y se tapa como el mensaje de un motor.
  d <- data.frame(a = 1:3, b = c("Gonzalez", "b", "c"), stringsAsFactors = FALSE)
  motivo <- .motivo_perfilado_muestra_dbi(
    simpleError("la celda Gonzalez no se pudo leer"), d, list()
  )
  expect_false(grepl("Gonzalez", motivo, fixed = TRUE))
  # Y el que se repite sin filas sale como lo escribio el paquete.
  propio <- tryCatch(perfilar(d, max_columnas_orden = Inf), error = function(e) e)
  motivo <- .motivo_perfilado_muestra_dbi(propio, d, list(max_columnas_orden = Inf))
  expect_identical(motivo, conditionMessage(propio))
})

# ---- El segfault bajo C ----------------------------------------------------
# Medido con SQL Server (odbc + FreeTDS): bajo `LC_ALL=C` una consulta con una
# columna `a\u00f1o` -los bytes UTF-8 sin marca de `dbListFields()`- no se puede
# pasar a la codificacion nativa; odbc manda "NA", deja la conexion rota y la
# consulta siguiente hace caer a R (violacion de segmento en `dbSendQuery()`).
# Lo que necesita el contenedor no va aca: se prueba la guarda que decide no
# mandar la consulta, con un nombre que la sesion no puede escribir.

test_that("Segfault: bajo C la guarda no deja mandar por odbc un nombre que la sesion no puede escribir", {
  # La caida se midio en Linux con FreeTDS. En Windows, bajo C, la sesion lee como
  # nativos los bytes UTF-8 sin marca y la guarda los da por escribibles: la premisa
  # de esta prueba no vale ahi, y no hay un odbc real en Windows con que medir si
  # hay algo que guardar.
  skip_on_os("windows")
  odbc_falso <- structure(list(), class = "OdbcConnection")
  otra <- structure(list(), class = "SQLiteConnection")
  marcado <- paste0("SELECT [a", intToUtf8(0xf1L), "o] FROM t WHERE 1 = 0")
  sin_marca <- paste0("SELECT [", rawToChar(as.raw(c(0x61, 0xc3, 0xb1, 0x6f))),
                      "] FROM t WHERE 1 = 0")
  ascii <- "SELECT [id] FROM t WHERE 1 = 0"
  bajo_c <- .traducciones_bajo_c_o160(list(
    utf8 = isTRUE(l10n_info()[["UTF-8"]]),
    marcado = .sentencia_representable_dbi(odbc_falso, marcado),
    sin_marca = .sentencia_representable_dbi(odbc_falso, sin_marca),
    ascii = .sentencia_representable_dbi(odbc_falso, ascii),
    otro_driver = .sentencia_representable_dbi(otra, sin_marca),
    enviar = tryCatch(.enviar_consulta_dbi(odbc_falso, sin_marca),
                      error = function(e) e),
    traer = tryCatch(.traer_consulta_dbi(odbc_falso, marcado),
                     error = function(e) e)
  ))$valor
  expect_false(bajo_c$utf8)
  expect_false(bajo_c$marcado)
  expect_false(bajo_c$sin_marca)
  expect_true(bajo_c$ascii)
  # RPostgres, RSQLite y duckdb reciben UTF-8: medido bajo C, leen el nombre.
  expect_true(bajo_c$otro_driver)
  # No se manda: un error de la via, antes de tocar la conexion (el objeto falso
  # no tiene conexion: si se mandara, el error seria otro).
  expect_s3_class(bajo_c$enviar, "lupa_error_codificacion_dbi")
  expect_s3_class(bajo_c$traer, "lupa_error_dbi")
  expect_true(grepl("locale UTF-8", conditionMessage(bajo_c$enviar), fixed = TRUE))
  # Con un locale UTF-8 la misma consulta se manda.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "la sesion de la prueba no es UTF-8")
  expect_true(.sentencia_representable_dbi(odbc_falso, marcado))
})

test_that("Guarda segfault: toda consulta de la via sale por los dos envios que miran la codificacion", {
  # Por construccion: un `dbSendQuery()` o `dbGetQuery()` directo saltearia la
  # guarda.
  ns <- asNamespace("lupa")
  envios <- c("dbSendQuery", "dbGetQuery", "dbExecute", "dbSendStatement")
  sitios <- character()
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- e[[1L]]
      llamada <- if (is.name(cabeza)) as.character(cabeza) else if (
        is.call(cabeza) && identical(cabeza[[1L]], as.name("::"))) {
        as.character(cabeza[[3L]])
      } else ""
      if (llamada %in% envios) sitios <<- c(sitios, nombre)
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]], nombre)
      }
    }
    invisible()
  }
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto)) recorrer(body(objeto), nombre)
  }
  # `.orden_transaccion_dbi()` y `.estado_transaccion_sqlserver_dbi()` mandan
  # solo sentencias fijas, en ASCII, del punto de guardado y de la vigilancia
  # de SQL Server: no llevan texto que la guarda de codificacion deba mirar.
  expect_identical(sort(unique(sitios)),
                   c(".enviar_consulta_dbi", ".estado_transaccion_sqlserver_dbi",
                     ".extra_float_digits_postgresql_dbi",
                     ".orden_transaccion_dbi",
                     ".traer_consulta_dbi"))
})

test_that("Segfault: una columna que la sesion no puede escribir se declara, por la via SQL y por bloques, sin mandarla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ano <- paste0("a", intToUtf8(0xf1L), "o")
  bytes_ano <- as.raw(c(0x61, 0xc3, 0xb1, 0x6f))
  # La sesion de la prueba lee el nombre (SQLite y UTF-8): se simula la que no
  # puede escribirlo, con la misma regla -no se manda lo que no se puede
  # escribir- sobre las consultas que nombran `a\u00f1o`.
  local_mocked_bindings(.sentencia_representable_dbi = function(conexion, sql) {
    !grepl(rawToChar(bytes_ano), sql, fixed = TRUE, useBytes = TRUE)
  })
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- data.frame(id = 1:3, x = c(2020L, 2021L, 2022L))
  names(d)[2] <- ano
  DBI::dbWriteTable(con, "o160_s", d)
  enviadas <- new.env()
  enviadas$sql <- character()
  anotar <- bquote(assign("sql", c(get("sql", envir = .(enviadas)), statement),
                          envir = .(enviadas)))
  for (generica in c("dbSendQuery", "dbGetQuery")) {
    suppressMessages(trace(generica, tracer = anotar,
                           where = asNamespace("DBI"), print = FALSE))
  }
  on.exit(for (generica in c("dbSendQuery", "dbGetQuery")) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)
  for (bloque_filas in list(NULL, 2L)) {
    r <- suppressWarnings(suppressMessages(
      perfilar_dbi(con, "o160_s", bloque_filas = bloque_filas)
    ))
    columnas <- r$resumen_tabla$columnas
    # La columna esta, sin medir; `id` se mide entero: media de 1:3 = 2.
    expect_true(ano %in% columnas$columna)
    expect_true(is.na(columnas$n_validos[columnas$columna == ano]))
    expect_equal(columnas$media[columnas$columna == "id"], 2)
    cobertura <- r$resumen_tabla$cobertura
    fila <- cobertura[cobertura$elemento == ano, , drop = FALSE]
    expect_identical(fila$estado, "no_disponible")
    expect_true(grepl("no se puede escribir en la codificacion", fila$motivo,
                      fixed = TRUE))
    expect_true(grepl("locale UTF-8", fila$como_resolverlo, fixed = TRUE))
  }
  # Ninguna consulta mandada nombra la columna.
  expect_gt(length(enviadas$sql), 0L)
  expect_false(any(grepl(rawToChar(bytes_ano), enviadas$sql, fixed = TRUE,
                         useBytes = TRUE)))
  # Y una tabla con un nombre asi da el error de la via, no un aborto.
  DBI::dbWriteTable(con, paste0("o160_", ano), d[, 1L, drop = FALSE])
  error <- tryCatch(perfilar_dbi(con, paste0("o160_", ano)),
                    error = function(e) e)
  expect_s3_class(error, "lupa_error_codificacion_dbi")
})
