# Ronda 32-S: la misma respuesta en cualquier sesion, segunda vuelta. La ronda 31
# cerro `scipen` y `OutDec` en lo que es codigo e identificador; quedaban tres
# perillas que tocan la misma construccion -`LC_NUMERIC`, `digits.secs` y el
# `deparse()` bajo el locale `C`- y cinco sitios sueltos. Cada bloque es un
# hallazgo de la refutacion, con el caso que lo reproducia y la cifra rehecha a
# mano. Lo que necesita un locale que la maquina no tiene se salta; las guardas
# por construccion recorren el espacio de nombres. El archivo es ASCII: lo que
# no lo es se arma con `intToUtf8()`.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.con_ctype_o154 <- function(locale, codigo) {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", anterior)), add = TRUE)
  puesto <- suppressWarnings(tryCatch(
    Sys.setlocale("LC_CTYPE", locale), error = function(e) ""
  ))
  if (length(puesto) != 1L || is.na(puesto) || !nzchar(puesto)) {
    skip(paste("no se pudo fijar LC_CTYPE", locale))
  }
  force(codigo)
}

# Un `LC_NUMERIC` con coma decimal, el primero que la maquina tenga.
.con_numerica_o154 <- function(codigo) {
  anterior <- Sys.getlocale("LC_NUMERIC")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", anterior)), add = TRUE)
  for (candidato in c("de_DE.UTF-8", "de_DE.utf8", "es_UY.utf8", "es_UY.UTF-8",
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

.recorrer_llamadas_o154 <- function(visitar) {
  ns <- asNamespace("lupa")
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
  invisible()
}

.si_o154 <- function() paste0("S", intToUtf8(0xedL))

test_that("A1: una conversion a fecha-hora con fraccion de segundo es inyectiva en la sesion por omision", {
  d <- data.frame(ID = 1:6, TS = c(
    "2026-03-08 02:30:00.25", "2026-03-08 02:30:00.75", "2026-03-09 10:00:00.10",
    "2026-03-09 10:00:00.90", "2026-03-10 11:11:11.50", "2026-03-11 12:00:00.00"
  ), stringsAsFactors = FALSE)
  anterior <- options(digits.secs = NULL)
  on.exit(options(anterior), add = TRUE)
  plan <- planificar_limpieza(
    perfilar(d, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
             analizar_dependencias = FALSE),
    d
  )
  i <- which(plan$columna == "TS" & plan$estrategia == "convertir_tipo")
  expect_length(i, 1L)
  # A mano: al segundo entero chocan {00.25, 00.75} y {00.10, 00.90}, cuatro
  # valores; con la fraccion los seis son distintos y la conversion los
  # conserva. La base bloqueaba con "4 valores participan en una conversion no
  # inyectiva"; con `digits.secs = 6` la dejaba lista.
  expect_identical(as.character(plan$estado[i]), "lista")
  expect_false(plan$destructiva[i])
  expect_equal(plan$parametros[[i]]$n_no_reversibles, 0)
})

test_that("A1: el texto que compara una conversion escribe el instante entero, con cualquier digits.secs", {
  x <- .POSIXct(c(1772937000.25, 1772937000.75), tz = "UTC")
  textos <- lapply(list(NULL, 0, 6), function(digitos) {
    anterior <- options(digits.secs = digitos)
    on.exit(options(anterior), add = TRUE)
    lupa:::.texto_representacion_conversion(x)
  })
  # A mano: 1772937000 s = 2026-03-08 02:30:00 UTC.
  esperado <- c("2026-03-08 02:30:00.25 UTC", "2026-03-08 02:30:00.75 UTC")
  for (texto in textos) expect_identical(texto, esperado)
})

test_that("A2: deparse() del paquete escribe punto con LC_NUMERIC en coma y la deja como estaba", {
  resultado <- .con_numerica_o154({
    antes <- Sys.getlocale("LC_NUMERIC")
    texto <- lupa:::.deparse_por_omision(quote(valor > 0.9 & peso <= 12.5))
    list(texto = texto, antes = antes, despues = Sys.getlocale("LC_NUMERIC"))
  })
  # `deparse()` de base escribe `valor > 0,9 & peso <= 12,5` en esa sesion.
  expect_identical(resultado$texto, "valor > 0.9 & peso <= 12.5")
  expect_identical(resultado$despues, resultado$antes)
})

test_that("A2: bajo C deparse() del paquete escribe el texto no ASCII como una sesion UTF-8", {
  si <- .si_o154()
  barra <- "\\"
  funcion <- eval(parse(text = paste0("function(x) x == \"S", barra, "u00ed\"")))
  formula <- eval(parse(text = paste0(
    "~ estado == \"S", barra, "u00ed\" & peso > 0.5"
  )))
  simbolo <- call("~", call(">", as.name(paste0("a", intToUtf8(0xf1L), "o")), 3))
  escritos <- .con_ctype_o154("C", {
    if (!lupa:::.poner_ctype_utf8()) {
      skip("la maquina no tiene un locale UTF-8 para LC_CTYPE")
    }
    Sys.setlocale("LC_CTYPE", "C")
    list(
      funcion = lupa:::.deparse_por_omision(funcion),
      formula = lupa:::.deparse_por_omision(formula),
      simbolo = lupa:::.deparse_por_omision(simbolo),
      ctype = Sys.getlocale("LC_CTYPE")
    )
  })
  bytes <- function(x) charToRaw(paste(x, collapse = "\n"))
  # A mano: la i con tilde son los dos bytes c3 ad; escapada seria `<U+00ED>`
  # (formula) o `\303\255` (funcion), y el simbolo iria entre comillas.
  expect_identical(bytes(escritos$funcion),
                   bytes(c("function (x) ", paste0("x == \"", si, "\""))))
  expect_identical(bytes(escritos$formula),
                   bytes(paste0("~estado == \"", si, "\" & peso > 0.5")))
  expect_identical(bytes(escritos$simbolo),
                   bytes(paste0("~a", intToUtf8(0xf1L), "o > 3")))
  expect_identical(escritos$ctype, "C")
})

metodo_o154 <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  filas <- seq_along(x)
  data.frame(
    resultado = !is.na(x) & x > 0.5, entidad = instancia$entidad,
    atributo = instancia$atributos, fila = filas,
    objeto = paste0(instancia$entidad, "$", instancia$atributos, "[", filas, "]")
  )
}

.evaluacion_o154 <- function(valor, mes) {
  nucleo <- metricas_nucleo()
  sobre <- metrica(
    "SobreUmbral", "Valor sobre el umbral.", "instanciaAtributo", "booleano",
    orientacion = "conformidad", dimension = "Exactitud", factor = "Rango",
    metodo = metodo_o154
  )
  contrato <- vigencia("actualizado",
                       fecha_acceso = .POSIXct(1771200000.5, tz = "UTC"))
  instancias <- list(
    instanciar(especializar(sobre), "t", "valor", metodo = metodo_o154),
    instanciar(especializar(nucleo$DesactualizacionPorFecha, vigencia = contrato),
               "t", "actualizado"),
    instanciar(especializar(nucleo$NoNulo), "t", "valor")
  )
  datos <- data.frame(
    valor = valor, peso = c(0.4, 0.6, 0.8, 1),
    estado = c(rep(.si_o154(), 3L), "No"),
    actualizado = as.Date(c("2026-01-01", "2026-01-20", "2026-02-10", "2026-02-01")),
    stringsAsFactors = FALSE
  )
  aplicable <- eval(parse(text = paste0(
    "~ estado == \"S", "\\", "u00ed\" & peso > 0.5"
  )))
  medicion <- medir(
    modelo(instancias), list(t = datos), aplicabilidad = list(valor = aplicable),
    id_medicion = paste0("m-", mes), fecha = as.POSIXct(mes, tz = "UTC")
  )
  evaluar(medicion, perfiles_madurez("NoNulo@t.valor")$Avanzado)
}

.sin_cambio_de_configuracion_o154 <- function(enero, febrero) {
  comparacion <- comparar_evaluaciones(enero, febrero)$comparacion
  deriva <- detectar_deriva_calidad(historico_calidad(enero, febrero),
                                    nivel = "regla")
  # A mano: aplicable donde `estado == "Si-tilde" & peso > 0.5`, filas 2 y 3;
  # enero `0.7, NA` y febrero `0.3, NA`: 1 de 2 presentes las dos veces, delta 0.
  expect_identical(nrow(deriva), 1L)
  expect_equal(deriva$delta, 0)
  expect_false(any(grepl("modelo de calidad", comparacion, fixed = TRUE)))
  expect_false(any(grepl("aplicabilidad", comparacion, fixed = TRUE)))
}

test_that("A2: una evaluacion escrita bajo C aparea con una de una sesion UTF-8", {
  enero <- .evaluacion_o154(c(0.2, 0.7, NA, 0.9), "2026-01-31")
  febrero <- .con_ctype_o154("C", .evaluacion_o154(c(0.2, 0.3, NA, 0.9), "2026-02-28"))
  .sin_cambio_de_configuracion_o154(enero, febrero)
})

test_that("A2: una evaluacion escrita con LC_NUMERIC en coma aparea con una en C", {
  enero <- .evaluacion_o154(c(0.2, 0.7, NA, 0.9), "2026-01-31")
  febrero <- .con_numerica_o154(.evaluacion_o154(c(0.2, 0.3, NA, 0.9), "2026-02-28"))
  .sin_cambio_de_configuracion_o154(enero, febrero)
})

test_that("A2: una evaluacion escrita con digits.secs = 6 aparea con la de la sesion por omision", {
  enero <- .evaluacion_o154(c(0.2, 0.7, NA, 0.9), "2026-01-31")
  anterior <- options(digits.secs = 6)
  febrero <- .evaluacion_o154(c(0.2, 0.3, NA, 0.9), "2026-02-28")
  options(anterior)
  .sin_cambio_de_configuracion_o154(enero, febrero)
})

test_that("A2: un instante de la configuracion se escribe como antes y con su fraccion", {
  configuracion <- function(x) lupa:::.texto_configuracion_calidad(x)
  # Lo que guardaba la ronda 31 en la sesion por omision, y se sigue guardando:
  # 1771200000 s = 2026-02-16 00:00:00 UTC (medianoche: la fecha sola) y
  # 1771218007 s = 2026-02-16 05:00:07 UTC.
  expect_identical(configuracion(.POSIXct(1771200000, tz = "UTC")),
                   "POSIXt:2026-02-16 UTC")
  expect_identical(configuracion(.POSIXct(1771218007, tz = "UTC")),
                   "POSIXt:2026-02-16 05:00:07 UTC")
  # Con fraccion, la misma en cualquier `digits.secs`; la base cortaba el medio
  # segundo con el valor por omision y lo escribia `.500000` con 6.
  for (digitos in list(NULL, 6)) {
    anterior <- options(digits.secs = digitos)
    texto <- configuracion(.POSIXct(1771200000.5, tz = "UTC"))
    options(anterior)
    expect_identical(texto, "POSIXt:2026-02-16 00:00:00.5 UTC")
  }
  # Y un doble con las cifras que vuelven a SU valor: 0.1 + 0.2 no es 0.3.
  expect_identical(configuracion(c(0.1 + 0.2, 0.3, 1e5)),
                   "double[0.30000000000000004,0.3,1e+05]")
})

test_that("M1: el porcentaje de TABLESAMPLE se escribe con punto con OutDec en coma", {
  dialecto <- lupa:::.dialectos_dbi()$limit
  candidatos <- lupa:::.candidatos_muestreo_dbi(NULL, dialecto)
  formas <- c("tablesample_system", "tablesample_bernoulli", "tablesample_percent")
  anterior <- options(OutDec = ",", scipen = -9)
  on.exit(options(anterior), add = TRUE)
  sentencias <- vapply(formas, function(nombre) {
    lupa:::.fuente_muestreada_dbi(
      "t", c("x", "y"), 5, 40, dialecto, list(candidato = candidatos[[nombre]])
    )$sql
  }, character(1L))
  options(anterior)
  # A mano: 5 de 40 filas son el 12.5 por ciento. La base escribia `12,5`.
  expect_true(all(grepl("12.5", sentencias, fixed = TRUE)))
  expect_false(any(grepl("12,5", sentencias, fixed = TRUE)))
})

test_that("M1: el porcentaje de TABLESAMPLE se escribe con punto con LC_NUMERIC en coma", {
  escrito <- .con_numerica_o154(lupa:::.decimal_sql_dbi(c(12.5, 100, 1 / 3 * 100)))
  # A mano: con 8 cifras significativas, 1/3 * 100 = 33.333333.
  expect_identical(trimws(escrito), c("12.5", "100", "33.333333"))
})

test_that("M2: distribucion_valores() agrupa una duracion por su valor y la escribe como un doble", {
  numeros <- c(1.5, 1.5, 2.25, 0.1 + 0.2, 0.3, 100000)
  d <- data.frame(dur = as.difftime(numeros, units = "mins"), num = numeros)
  frecuencias <- lapply(
    list(list(), list(OutDec = ","), list(scipen = -9)),
    function(opciones) {
      anterior <- options(opciones)
      on.exit(options(anterior), add = TRUE)
      as.data.frame(distribucion_valores(d)$frecuencias)
    }
  )
  for (tabla in frecuencias) {
    dur <- tabla[tabla$columna == "dur", , drop = FALSE]
    num <- tabla[tabla$columna == "num", , drop = FALSE]
    # A mano: 1.5 dos veces; 0.1 + 0.2, 0.3, 2.25 y 1e5 una vez cada uno. La
    # base juntaba 0.1 + 0.2 con 0.3 -"0.3" con 2- y agregaba un NA con 0.
    expect_identical(dur$valor, c("1.5", "0.3", "0.30000000000000004", "2.25", "1e+05"))
    expect_equal(dur$frecuencia, c(2, 1, 1, 1, 1))
    expect_identical(dur$valor, num$valor)
  }
})

test_that("B1: con scipen negativo la fuente de la mediana no sale en notacion cientifica", {
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  fuente <- lupa:::.proyectar_costo_mediana_dbi(
    NULL, 1e6, 1, usar_referencia_banco = TRUE
  )$fuente
  options(anterior)
  # A mano: la referencia de banco son 68 ms por millon de filas.
  expect_identical(
    fuente, "referencia de banco de otra corrida: 68 ms por millon de filas"
  )
})

test_that("B1: con scipen negativo el motivo de perfilar_coleccion() cuenta sin notacion cientifica", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE t (id INTEGER PRIMARY KEY, alta TIMESTAMP)")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO t VALUES (1, '2026-03-08 02:30:00'), ",
    "(2, '2026-11-01 01:30:00'), (3, '2025-12-31 23:59:59')"
  ))
  anterior <- options(scipen = -9)
  perfil <- suppressWarnings(perfilar_coleccion(coleccion(conexion, "t", nombre = "R32S")))
  options(anterior)
  motivo <- perfil$cobertura_coleccion$motivo
  motivo <- motivo[grepl("No se calcularon", motivo, fixed = TRUE)]
  expect_length(motivo, 1L)
  # A mano: las filas `no_disponible` de la cobertura de metricas son las que no
  # se calcularon; la base las escribia `7e+00` y el total `3e+01`.
  no_calculadas <- sum(perfil$cobertura_metricas$estado == "no_disponible")
  expect_true(grepl(paste0("No se calcularon ", no_calculadas, " de [0-9]+ agregados"),
                    motivo))
  expect_false(grepl("e+0", motivo, fixed = TRUE))
})

test_that("B2: el separador de un rango no choca con la coma que traen sus extremos", {
  # Con `LC_NUMERIC` en coma las cifras llegan con coma aunque `OutDec` sea
  # punto: la base publicaba `[0,125, 0,75]`, tres comas para dos numeros.
  expect_identical(lupa:::.texto_rango_deriva("0,125", "0,75"), "[0,125; 0,75]")
  expect_identical(lupa:::.texto_rango_deriva("0.125", "0.75"), "[0.125, 0.75]")
})

test_that("B2: con LC_NUMERIC en coma detectar_relaciones() separa los extremos con punto y coma", {
  d <- data.frame(id = 1:12, p = c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75,
                                   0.875, 1, 0.1, 0.2, 0.3, 0.4))
  detalle <- .con_numerica_o154({
    podas <- attr(detectar_relaciones(d, d[1:6, , drop = FALSE]), "podas")
    unique(podas$detalle[grepl("[", podas$detalle, fixed = TRUE)])
  })
  # A mano: `id` va de 1 a 12 y de 1 a 6; `p` de 0.1 a 1 y de 0.125 a 0.75.
  # Cada rango con un solo separador, el mismo en los dos, como con `OutDec`.
  expect_true(length(detalle) > 0L)
  expect_true(any(grepl("[0,125; 0,75]", detalle, fixed = TRUE)))
  expect_false(any(grepl("[0,125, 0,75]", detalle, fixed = TRUE)))
})

test_that("B3: sf instalado que no se puede cargar no se publica como paquete ausente", {
  d <- data.frame(id = 1:3, GEO = c("POINT (1 2)", "POINT (3 4)",
                                    "LINESTRING (0 0, 1 1)"),
                  stringsAsFactors = FALSE)
  instalado <- nzchar(system.file(package = "sf"))
  # `requireNamespace()` da FALSE tambien cuando sf esta instalado y no carga
  # -con `LC_NUMERIC` en coma falla `units`-; se simula esa respuesta.
  testthat::local_mocked_bindings(.sf_disponible = function() FALSE,
                                  .package = "lupa")
  perfil <- perfilar(d, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
                     analizar_dependencias = FALSE)
  fila <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "perfil_geometria", ,
    drop = FALSE
  ]
  expect_identical(nrow(fila), 1L)
  if (instalado) {
    expect_true(grepl("esta instalado pero no se pudo cargar", fila$motivo, fixed = TRUE))
    expect_false(grepl("Instalar", fila$como_resolverlo, fixed = TRUE))
  } else {
    expect_true(grepl("falta el paquete opcional 'sf'", fila$motivo, fixed = TRUE))
    expect_true(grepl("Instalar", fila$como_resolverlo, fixed = TRUE))
  }
})

test_that("B4: bajo C un aviso del paquete llega a quien lo atrapa con sus bytes", {
  ns <- asNamespace("lupa")
  expect_true(exists(".avisar", envir = ns, inherits = FALSE))
  expect_false(exists("warning", envir = ns, inherits = FALSE))
  esperado <- paste0("num", intToUtf8(0xe9L), "rico")
  avisar <- function() .avisar("num", intToUtf8(0xe9L), "rico", call. = FALSE)
  environment(avisar) <- ns
  mensaje <- .con_ctype_o154("C", tryCatch(avisar(), warning = conditionMessage))
  # A mano: 9 bytes, la e con tilde en dos; escapada serian 15.
  expect_identical(charToRaw(mensaje), charToRaw(esperado))
  # Una condicion ya armada pasa como esta.
  condicion <- simpleWarning("x")
  reavisar <- function(condicion) .avisar(condicion)
  environment(reavisar) <- ns
  expect_identical(tryCatch(reavisar(condicion), warning = identity), condicion)
})

test_that("B4: bajo C el aviso de medir() sobre una metrica que falla no escapa su texto", {
  falla <- function(tablas, instancia) base::stop("falla a proposito")
  rota <- metrica("Rota", "Siempre falla.", "instanciaAtributo", "booleano",
                  orientacion = "conformidad", dimension = "Exactitud",
                  factor = "Rango", metodo = falla)
  modelo_roto <- modelo(list(
    instanciar(especializar(rota), "t", "x"),
    instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x")
  ))
  avisos <- .con_ctype_o154("C", {
    textos <- character()
    withCallingHandlers(
      medir(modelo_roto, list(t = data.frame(x = c(1, NA, 3))), fecha = .POSIXct(0)),
      warning = function(w) {
        textos <<- c(textos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    textos
  })
  expect_length(avisos, 1L)
  # A mano: "1 m" y la e con tilde, bytes 31 20 6d c3 a9; la base daba `<U+00E9>`.
  expect_identical(charToRaw(substr(avisos, 1, 4)),
                   charToRaw(paste0("1 m", intToUtf8(0xe9L))))
})

test_that("B5: el ejemplo de ?pack_validadores da la misma respuesta con scipen negativo", {
  rd <- testthat::test_path("..", "..", "man", "pack_validadores.Rd")
  skip_if_not(file.exists(rd), "sin las fuentes de la ayuda")
  ejemplo <- tempfile(fileext = ".R")
  on.exit(unlink(ejemplo), add = TRUE)
  tools::Rd2ex(tools::parse_Rd(rd), ejemplo)
  respuestas <- lapply(c(0, -9), function(penalizacion) {
    anterior <- options(scipen = penalizacion)
    on.exit(options(anterior), add = TRUE)
    entorno <- new.env(parent = asNamespace("lupa"))
    utils::capture.output(sys.source(ejemplo, envir = entorno))
    entorno$chile$rut(c("12.345.678-5", "12.345.678-4"))
  })
  # A mano: la suma ponderada de 12345678 es 138, 11 - 138 %% 11 = 5: el
  # primero es valido y el segundo no. Con `scipen = -9` la base comparaba
  # "5e+00" contra "5".
  for (respuesta in respuestas) {
    expect_identical(unname(respuesta), c(TRUE, FALSE))
  }
})

test_that("Guarda: ninguna funcion del paquete escribe un instante con format() sin formato ni cifras", {
  # Por construccion: `format()` de una fecha-hora sin formato -o con `%OS` sin
  # cifras- toma los decimales de `options(digits.secs)`. Se marca la llamada a
  # `format()` que nombra una zona (`tz`, `usetz`) y no trae formato -con
  # nombre o como segundo argumento-, y todo formato con `%OS` sin cifras pasado
  # a `format()` o `strftime()`. `digits` no alcanza: `format.POSIXlt()` no lo
  # tiene en R 4.1 -lo manda a `...` y lo ignora- y alli `format(x, tz = "UTC",
  # digits = 0L)` escribia `00:00:00.5` con `digits.secs = 6`.
  sin_formato <- character()
  os_sin_cifras <- character()
  .recorrer_llamadas_o154(function(e, nombre) {
    if (!is.call(e) || !is.name(e[[1L]])) return(invisible())
    funcion <- as.character(e[[1L]])
    if (!funcion %in% c("format", "format.POSIXct", "format.POSIXlt", "strftime")) {
      return(invisible())
    }
    argumentos <- as.list(e)[-1L]
    nombres <- names(argumentos)
    if (is.null(nombres)) nombres <- rep("", length(argumentos))
    con_zona <- any(nombres %in% c("tz", "usetz"))
    con_formato <- any(nombres %in% "format") || sum(!nzchar(nombres)) >= 2L
    if (con_zona && !con_formato) sin_formato <<- c(sin_formato, nombre)
    textos <- Filter(is.character, argumentos)
    if (any(vapply(textos, function(t) any(grepl("%OS(?![0-9])", t, perl = TRUE)),
                   logical(1L)))) {
      os_sin_cifras <<- c(os_sin_cifras, nombre)
    }
  })
  expect_identical(unique(sin_formato), character())
  expect_identical(unique(os_sin_cifras), character())
})

test_that("Guarda: ninguna funcion del paquete levanta un aviso con warning() de base", {
  # Por construccion: `.avisar()` arma la condicion sin pasar por `gettext()`;
  # `base::warning()` solo puede llamarlo ese ayudante.
  directos <- character()
  .recorrer_llamadas_o154(function(e, nombre) {
    if (is.call(e) && identical(e[[1L]], quote(base::warning)) &&
        !identical(nombre, ".avisar")) {
      directos <<- c(directos, nombre)
    }
  })
  expect_identical(unique(directos), character())
})

test_that("D4: el paquete no tapa stop() ni warning() y no los llama de base", {
  # Decision D4 de Sebastian (ronda 32): los errores y avisos del paquete van por
  # `.detener()` y `.avisar()`, con nombre propio, en vez de un `stop()` y un
  # `warning()` que tapaban a los de base en el espacio de nombres. Ningun
  # nombre de base queda tapado, y ninguna funcion del paquete llama a `stop()`
  # ni a `warning()` de base -que pasan el texto por `gettext()` y bajo `C` lo
  # escapan- fuera de esos dos ayudantes.
  ns <- asNamespace("lupa")
  expect_false(exists("stop", envir = ns, inherits = FALSE))
  expect_false(exists("warning", envir = ns, inherits = FALSE))
  directos <- character()
  .recorrer_llamadas_o154(function(e, nombre) {
    if (!is.call(e)) return(invisible())
    cabeza <- e[[1L]]
    es_base <- identical(cabeza, quote(stop)) || identical(cabeza, quote(warning)) ||
      identical(cabeza, quote(base::stop)) || identical(cabeza, quote(base::warning))
    if (es_base && !(nombre %in% c(".detener", ".avisar"))) {
      directos <<- c(directos, nombre)
    }
  })
  expect_identical(unique(directos), character())
})

# Un `LC_NUMERIC` que no es "C": con coma decimal si la maquina lo tiene y si no
# cualquier otro nombre que acepte. Lo que se mide es que la llamada lo fije en
# "C" mientras lee y lo devuelva, no la coma.
.con_numerica_no_c_o154 <- function(codigo) {
  anterior <- Sys.getlocale("LC_NUMERIC")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", anterior)), add = TRUE)
  for (candidato in c("de_DE.UTF-8", "de_DE.utf8", "es_UY.utf8", "es_UY.UTF-8",
                      "es_ES.UTF-8", "es_ES.utf8", "fr_FR.UTF-8", "fr_FR.utf8",
                      "C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_NUMERIC", candidato), error = function(e) ""
    ))
    if (length(puesto) == 1L && !is.na(puesto) && nzchar(puesto) &&
        !puesto %in% c("C", "POSIX")) {
      return(force(codigo))
    }
  }
  skip("no hay un LC_NUMERIC distinto de C en esta maquina")
}

test_that("Guarda: toda exportada que lee de una conexion lee con LC_NUMERIC en C y lo devuelve", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # Por construccion. Los controladores leen el texto del motor con `strtod()`
  # de C, que sigue a `LC_NUMERIC`: con coma RPostgres leia 2.5 como 2 -y odbc
  # un `decimal` de SQL Server- y `perfilar_dbi()` publicaba media 3, mediana
  # 2, minimo 0 y maximo 10 sobre c(0.5, 1.5, 2.5, 3.75, 10.25), cuya media es
  # (0.5 + 1.5 + 2.5 + 3.75 + 10.25) / 5 = 18.5 / 5 = 3.7, la mediana 2.5, el
  # minimo 0.5 y el maximo 10.25. RSQLite y duckdb leen bien, asi que aca se
  # mide la construccion: cada exportada que recibe una conexion -o una
  # coleccion, que la lleva- hace TODA lectura con `LC_NUMERIC` en "C" y lo
  # devuelve como estaba al salir, tambien si termina en error.
  ns <- asNamespace("lupa")
  lectoras <- sort(Filter(function(nombre) {
    f <- get(nombre, envir = ns)
    if (!is.function(f) || is.primitive(f)) return(FALSE)
    formales <- names(formals(f))
    any(c("conexion", "con", "conn") %in% formales) ||
      identical(formales[1L], "coleccion")
  }, getNamespaceExports("lupa")))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o154_a",
                    data.frame(id = 1:5, x = c(0.5, 1.5, 2.5, 3.75, 10.25)))
  DBI::dbWriteTable(con, "o154_b",
                    data.frame(id = c(1L, 2L, 9L), x = c(0.5, 1.5, 99.5)))
  tablas <- c("o154_a", "o154_b")
  declarada <- suppressMessages(coleccion(con, tablas))
  pares <- data.frame(tabla_1 = "o154_a", tabla_2 = "o154_b",
                      stringsAsFactors = FALSE)
  llamadas <- list(
    coleccion = function() coleccion(con, tablas),
    perfilar_dbi = function() perfilar_dbi(con, "o154_a"),
    plan_perfilado_dbi = function() plan_perfilado_dbi(con, "o154_a"),
    perfilar_coleccion = function() perfilar_coleccion(declarada),
    estimar_costo_coleccion = function() estimar_costo_coleccion(declarada),
    relaciones_coleccion = function() relaciones_coleccion(declarada, pares = pares)
  )
  # Una exportada nueva que recibe una conexion tiene que entrar a esta lista.
  expect_identical(lectoras, sort(names(llamadas)))

  vistos <- new.env()
  vistos$locales <- character()
  genericas <- c("dbGetQuery", "dbSendQuery", "dbFetch", "dbListFields",
                 "dbExistsTable", "dbExecute", "dbReadTable", "dbGetInfo",
                 "dbColumnInfo")
  anotar <- bquote(assign(
    "locales", c(get("locales", envir = .(vistos)), Sys.getlocale("LC_NUMERIC")),
    envir = .(vistos)
  ))
  for (generica in genericas) {
    suppressMessages(trace(generica, tracer = anotar,
                           where = asNamespace("DBI"), print = FALSE))
  }
  on.exit(for (generica in genericas) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)

  for (nombre in names(llamadas)) {
    vistos$locales <- character()
    medida <- .con_numerica_no_c_o154({
      antes <- Sys.getlocale("LC_NUMERIC")
      suppressWarnings(suppressMessages(llamadas[[nombre]]()))
      list(antes = antes, despues = Sys.getlocale("LC_NUMERIC"))
    })
    expect_true(length(vistos$locales) > 0L, info = nombre)
    expect_identical(unique(vistos$locales), "C", info = nombre)
    expect_identical(medida$despues, medida$antes, info = nombre)
  }

  # Y si la llamada termina en error, sale con el `LC_NUMERIC` con que entro.
  medida <- .con_numerica_no_c_o154({
    antes <- Sys.getlocale("LC_NUMERIC")
    error <- tryCatch(suppressMessages(perfilar_dbi(con, "o154_no_existe")),
                      error = function(e) e)
    list(antes = antes, despues = Sys.getlocale("LC_NUMERIC"), error = error)
  })
  expect_s3_class(medida$error, "error")
  expect_identical(medida$despues, medida$antes)
})
