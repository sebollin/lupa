# Ronda 31-S: la misma respuesta en cualquier sesion. Cada bloque es un hallazgo
# de la refutacion -locale turco, `C`, zona horaria, `OutDec`, `scipen`-, con el
# caso que lo reproducia y la cifra rehecha a mano. Las que cambian el locale o
# la zona los restauran; las que necesitan un locale que la maquina no tiene se
# saltan, y la guarda por construccion que viaja recorre el espacio de nombres.
# El archivo es ASCII: lo que no lo es se arma con escapes.

.locale_turco_o152 <- function() {
  for (candidato in c("tr_TR.UTF-8", "tr_TR.utf8")) {
    puesto <- suppressWarnings(tryCatch(
      Sys.setlocale("LC_CTYPE", candidato), error = function(e) ""
    ))
    if (length(puesto) == 1L && !is.na(puesto) && nzchar(puesto) &&
        isTRUE(l10n_info()[["UTF-8"]])) {
      return(candidato)
    }
  }
  NA_character_
}

.recorrer_llamadas_o152 <- function(visitar) {
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
    } else if (is.character(objeto)) {
      recorrer(objeto, nombre)
    } else if (is.list(objeto)) {
      for (elemento in unlist(objeto, use.names = FALSE)) {
        if (is.character(elemento)) recorrer(elemento, nombre)
      }
    }
  }
  invisible()
}

test_that("H1: ninguna funcion del paquete pliega la caja con ignore.case ni con (?i)", {
  # Por construccion: sobre texto ASCII, `ignore.case = TRUE` y `(?i)` pliegan
  # con las tablas de caja del locale, y bajo uno turco la `I` y la `i` no se
  # pliegan. El vocabulario se casa con `.regex_sin_caja()`, que escribe las dos
  # cajas de cada letra. Se miran el argumento con nombre, el tercero sin nombre
  # de las funciones de base que lo aceptan, y los textos de los patrones.
  funciones <- c("grep", "grepl", "sub", "gsub", "regexpr", "gregexpr",
                 "regexec", "gregexec", "agrep", "agrepl", "grepRaw")
  con_caja <- character()
  con_modificador <- character()
  .recorrer_llamadas_o152(function(e, nombre) {
    if (is.character(e)) {
      if (any(grepl("\\(\\?[A-Za-z^-]*[iI]", e, perl = TRUE, useBytes = TRUE))) {
        con_modificador <<- c(con_modificador, nombre)
      }
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    if ("ignore.case" %in% names(as.list(e))) con_caja <<- c(con_caja, nombre)
    cabeza <- e[[1L]]
    if (is.symbol(cabeza) && as.character(cabeza) %in% funciones) {
      llamada <- tryCatch(
        match.call(get(as.character(cabeza), envir = baseenv()), e,
                   expand.dots = FALSE),
        error = function(err) NULL
      )
      if (!is.null(llamada) && "ignore.case" %in% names(as.list(llamada))) {
        con_caja <<- c(con_caja, nombre)
      }
    }
    invisible()
  })
  expect_identical(unique(con_caja), character())
  expect_identical(unique(con_modificador), character())
})

test_that("H1: el patron sin caja casa las dos cajas ASCII y rechaza lo que no puede traducir", {
  patron <- lupa:::.regex_sin_caja("^(?:abril|mayo)[[:space:]]+[0-9]{4}$")
  # A mano: las dos cajas de cada letra, en cualquier locale; la i sin punto
  # (U+0131) no es una `i` ASCII y no casa.
  x <- c("ABRIL 2024", "Abril 2024", "abril 2024", "MAYO 2021",
         paste0("abr", intToUtf8(0x131L), "l 2024"), "ABRILES 2024")
  expect_identical(grepl(patron, x, perl = TRUE),
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(grepl(patron, x), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  # Las clases y los escapes quedan como estaban; las referencias tambien.
  expect_identical(lupa:::.regex_sin_caja("\\bFROM\\b"), "\\b[fF][rR][oO][mM]\\b")
  expect_identical(lupa:::.regex_sin_caja("[0-9Xx]\\3"), "[0-9Xx]\\3")
  expect_error(lupa:::.regex_sin_caja("(?i)abril"), "modificador")
  expect_error(lupa:::.regex_sin_caja("[a-z]+"), "dos cajas")
})

test_that("H1: bajo un locale turco ABRIL 2024 es fecha y point(1 2) es WKT, como bajo cualquier otro", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", anterior)), add = TRUE)
  if (is.na(.locale_turco_o152())) skip("la maquina no tiene un locale turco")
  meses <- c("ABRIL 2024", "JULIO 2023", "JUNIO 2022", "MAYO 2021")
  formatos <- detectar_formatos_fecha(meses)
  # A mano: los cuatro son mes y anio -tres llevan I-: 4 de 4.
  expect_identical(formatos$formato[[1L]], "%B %Y")
  expect_equal(formatos$proporcion[[1L]], 1)
  expect_identical(inferir_tipo(meses)$tipo, "fecha")
  geo <- data.frame(geo = c(
    "point(1 2)", "point(3 4)", "linestring(0 0, 1 1)",
    "polygon((0 0, 1 0, 1 1, 0 0))"
  ))
  perfil <- perfilar(geo, analizar_dependencias = FALSE,
                     fecha = as.POSIXct("2026-01-15", tz = "UTC"))
  expect_identical(columnas(perfil)$representacion_geometria, "WKT")
  skip_if_not_installed("sf")
  # Con sf, las cuatro se convierten: GDAL recibe las palabras en mayusculas.
  expect_identical(columnas(perfil)$n_geometrias_analizadas, 4L)
})

.evaluacion_o152 <- function(edad, mes) {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "edad")
  medicion <- medir(
    modelo(instancia), data.frame(edad = edad),
    id_medicion = paste0("m-", mes), fecha = as.POSIXct(mes, tz = "UTC")
  )
  evaluar(medicion, perfiles_madurez("NoNulo@personas.edad")$Avanzado)
}

test_that("H2: el nombre de una regla de perfiles_madurez() no depende de OutDec ni de scipen", {
  anterior <- options(OutDec = ",", scipen = -9)
  on.exit(options(anterior), add = TRUE)
  perfiles <- perfiles_madurez("NoNulo@personas.edad")
  # A mano: los umbrales de fabrica son 0.5, 0.7 y 0.9, con punto decimal.
  expect_identical(names(perfiles$Basico$reglas), "Resultado > 0.5")
  expect_identical(names(perfiles$Intermedio$reglas), "Resultado > 0.7")
  expect_identical(names(perfiles$Avanzado$reglas), "Resultado > 0.9")
})

test_that("H2: una evaluacion de otra sesion se aparea por regla y no declara un cambio de perfil", {
  enero <- .evaluacion_o152(c(20, NA, 30, 40), "2026-01-31")
  for (opciones in list(list(OutDec = ","), list(scipen = -9))) {
    anterior <- options(opciones)
    febrero <- .evaluacion_o152(c(20, NA, NA, 40), "2026-02-28")
    deriva <- detectar_deriva_calidad(historico_calidad(enero, febrero),
                                      nivel = "regla")
    comparacion <- comparar_evaluaciones(enero, febrero)
    options(anterior)
    # A mano: enero 3 de 4 presentes, 0.75; febrero 2 de 4, 0.5; delta -0.25.
    expect_identical(nrow(deriva), 1L)
    expect_equal(deriva$delta, 0.5 - 0.75)
    expect_false(any(grepl("perfil de evaluacion", comparacion$comparacion,
                           fixed = TRUE)))
  }
})

test_that("H2: propiedades_regla() escribe el umbral con punto y todas sus cifras", {
  regla <- regla_evaluacion(
    "cobertura minima", function(x, minimo) x >= minimo,
    umbrales = list(minimo = 0.123456789)
  )
  anterior <- options(OutDec = ",", scipen = -9)
  on.exit(options(anterior), add = TRUE)
  propiedades <- propiedades_regla(regla)
  expect_identical(
    propiedades$valor[propiedades$propiedad == "umbral:minimo"], "0.123456789"
  )
})

test_that("H2: la configuracion de un perfil de evaluacion no depende de scipen", {
  perfil <- perfiles_madurez("NoNulo@personas.edad")$Avanzado
  referencia <- lupa:::.configuracion_perfil_evaluacion(perfil)
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  expect_identical(lupa:::.configuracion_perfil_evaluacion(perfil), referencia)
})

test_that("H2: ninguna funcion del paquete llama a deparse() fuera de su envoltorio", {
  # Por construccion: `deparse()` escribe los numeros con `scipen`.
  usa <- character()
  .recorrer_llamadas_o152(function(e, nombre) {
    if (is.call(e) && identical(e[[1L]], as.name("deparse")) &&
        !identical(nombre, ".deparse_por_omision")) {
      usa <<- c(usa, nombre)
    }
  })
  expect_identical(unique(usa), character())
})

test_that("H3: con scipen negativo el piso no tapa los estadisticos de una columna no protegida", {
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE,
                     fecha = as.POSIXct("2026-01-15", tz = "UTC"))
  fila <- columnas(perfil)[columnas(perfil)$columna == "id_copia", ]
  # A mano: id_copia = 1..10, 10, 1, 11; suma 77 en 13 valores; el 7.o
  # ordenado es 6; extremos 1 y 11.
  expect_identical(datos_administrativos$id_copia, c(1:10, 10, 1, 11) + 0)
  expect_equal(fila$minimo, 1)
  expect_equal(fila$maximo, 11)
  expect_equal(fila$media, 77 / 13)
  expect_equal(fila$mediana, 6)
})

test_that("H4: la corroboracion compara el valor, no su escritura en la sesion", {
  comparar <- lupa:::.comparar_celda_corroboracion_dbi
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  # A mano: 12 distintos de un lado como entero y del otro como doble.
  expect_false(comparar(list(12L), list(12), FALSE)$difiere)
  expect_false(comparar(list(0L), list(0), FALSE)$difiere)
  options(scipen = 0)
  # Y en la sesion por omision, 100000 entero contra 1e5 doble.
  expect_false(comparar(list(100000L), list(1e5), FALSE)$difiere)
  expect_true(comparar(list(12L), list(13), FALSE)$difiere)
})

test_that("H4: perfilar_dbi() con scipen negativo no publica divergencias entre cifras iguales", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "r31s_dev_t", data.frame(
    id = 1:12, estado = c("SI", "NO", rep("NO", 4), rep("SI", 6)),
    monto = seq(10, 120, 10)
  ))
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  perfil <- suppressWarnings(perfilar_dbi(con, "r31s_dev_t"))
  cb <- as.data.frame(cobertura(perfil))
  # A mano: SQL y la muestra (12 de 12 filas) dan los mismos conteos.
  expect_identical(sum(cb$estado == "divergencia"), 0L)
})

test_that("H4: un centinela doble excluye su valor en una columna integer64 con cualquier scipen", {
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64(c(1, 2, 99999, 99999, 100000))
  for (sc in c(0, -9)) {
    anterior <- options(scipen = sc)
    con_99999 <- lupa:::.resumen_integer64(x, sentinelas_numericos = 99999)
    con_1e5 <- lupa:::.resumen_integer64(x, sentinelas_numericos = 1e5)
    options(anterior)
    # A mano: 99999 aparece dos veces y 100000 una.
    expect_identical(con_99999$n_valores_excluidos_resumen, 2L)
    expect_identical(con_1e5$n_valores_excluidos_resumen, 1L)
  }
})

.con_ctype_o152 <- function(locale, codigo) {
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

test_that("H5: bajo C un error del paquete llega a quien lo atrapa con sus bytes", {
  # Por construccion: `.detener()` arma la condicion sin pasar por `gettext()` ni
  # por la traduccion al nativo del `stop()` de base (desde la ronda 32 con nombre
  # propio: el paquete ya no tapa a `stop()`, decision D4).
  ns <- asNamespace("lupa")
  expect_true(exists(".detener", envir = ns, inherits = FALSE))
  expect_false(exists("stop", envir = ns, inherits = FALSE))
  esperado <- paste0("num", intToUtf8(0xe9L), "rico")
  levantar <- function() .detener("num", intToUtf8(0xe9L), "rico", call. = FALSE)
  environment(levantar) <- ns
  mensaje <- .con_ctype_o152("C", tryCatch(levantar(), error = conditionMessage))
  # A mano: 9 bytes, la e con tilde en dos; escapada serian 15.
  expect_identical(charToRaw(mensaje), charToRaw(esperado))
  expect_identical(length(charToRaw(mensaje)), 9L)
  # Una condicion ya armada pasa como esta.
  condicion <- simpleError("x")
  relevantar <- function(condicion) .detener(condicion)
  environment(relevantar) <- ns
  expect_identical(tryCatch(relevantar(condicion), error = identity), condicion)
})

test_that("H5: bajo C el motivo del plan y el registro de aplicar() no escapan el texto", {
  d <- data.frame(ID = 1:12, txt = c(
    "1.234,56", "1,5", "100.000", "1e5", "2,5", "3,25", "0,1",
    "1.000.000,00", "12,00", "7", "8,5", "9,75"
  ), stringsAsFactors = FALSE)
  esperado <- paste0(
    "Hay valores presentes que no responden al formato num", intToUtf8(0xe9L),
    "rico regional."
  )
  resultado <- .con_ctype_o152("C", {
    plan <- planificar_limpieza(
      perfilar(d, fecha = as.POSIXct("2026-01-15", tz = "UTC")), d
    )
    i <- which(plan$estrategia == "convertir_numero_regional")
    motivo <- plan$parametros[[i]]$motivo_no_reversible
    estado <- as.character(plan$estado[i])
    # Desde la ronda 31 la conversion que no se ejecuta sale `bloqueada`; quien
    # edita el plan y la fuerza ve el error de la ejecucion en el registro.
    plan$aplicar <- FALSE
    plan$aplicar[i] <- TRUE
    plan$estado[i] <- "lista"
    registro <- aplicar(plan, d)$registro
    list(motivo = motivo, estado = estado,
         error = registro$error[!is.na(registro$error)])
  })
  expect_identical(resultado$estado, "bloqueada")
  # A mano: 68 caracteres, 69 bytes -la e con tilde ocupa dos-.
  expect_identical(charToRaw(resultado$error), charToRaw(esperado))
  expect_true(grepl(esperado, resultado$motivo, fixed = TRUE, useBytes = TRUE))
  expect_false(grepl("<U+", resultado$motivo, fixed = TRUE, useBytes = TRUE))
})

.con_zona_o152 <- function(zona, codigo) {
  anterior <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(anterior)) Sys.unsetenv("TZ") else Sys.setenv(TZ = anterior),
          add = TRUE)
  Sys.setenv(TZ = zona)
  force(codigo)
}

.instantes_o152 <- function() {
  # Fijados por su numero de segundos: 10:30 y 11:30 UTC del 2026-11-01 -la
  # misma hora local, 01:30, en America/Adak, antes y despues del cambio de
  # hora- y los dos lados de la medianoche UTC del ano nuevo.
  segundos <- c(1793529000, 1793532600, 1767225600, 1767225599)
  data.frame(id = 1:4, ts = .POSIXct(segundos, tz = ""))
}

test_that("H6: distribucion_valores() escribe cada instante en UTC, igual en cualquier zona", {
  # A mano: 1793529000 s = 2026-11-01 10:30:00 UTC; + 3600 = 11:30:00;
  # 1767225600 = 2026-01-01 00:00:00 UTC, y un segundo antes.
  esperadas <- c("2025-12-31 23:59:59 UTC", "2026-01-01 00:00:00 UTC",
                 "2026-11-01 10:30:00 UTC", "2026-11-01 11:30:00 UTC")
  for (zona in c("America/Adak", "Pacific/Kiritimati", "UTC")) {
    frecuencias <- .con_zona_o152(zona, distribucion_valores(.instantes_o152())$frecuencias)
    etiquetas <- frecuencias$valor[frecuencias$columna == "ts"]
    expect_identical(sort(etiquetas, method = "radix"), esperadas)
  }
})

test_that("H6: en America/Adak la normalizacion no funde dos instantes distintos", {
  resultado <- .con_zona_o152("America/Adak", {
    perfil <- perfilar(.instantes_o152(), analizar_dependencias = FALSE,
                       fecha = as.POSIXct("2026-01-15", tz = "UTC"))
    por <- perfilar_por(.instantes_o152(), por = "ts",
                        fecha = as.POSIXct("2026-01-15", tz = "UTC"))
    list(
      fusiones = perfil$meta$normalizacion_fusiones$ts$n_distintos,
      columnas = columnas(perfil)$n_distintos[columnas(perfil)$columna == "ts"],
      grupos = attr(por, "cobertura_grupos")$grupo
    )
  })
  # A mano: cuatro instantes distintos, cuatro grupos con la escritura en UTC.
  expect_identical(as.integer(resultado$fusiones), 4L)
  expect_identical(as.integer(resultado$columnas), 4L)
  expect_setequal(resultado$grupos, c(
    "2025-12-31 23:59:59 UTC", "2026-01-01 00:00:00 UTC",
    "2026-11-01 10:30:00 UTC", "2026-11-01 11:30:00 UTC"
  ))
})

test_that("H6: la moda de una fecha-hora se escribe igual en el perfil y en la distribucion", {
  # H7 de la ronda 31-R: columna en Asia/Tokyo, el instante 23:30 JST es 14:30 UTC.
  tokio <- as.POSIXct(c("2024-03-10 23:30:00", "2024-03-10 23:30:00",
                        "2024-03-10 23:30:00", "2024-03-11 08:00:00"),
                      tz = "Asia/Tokyo")
  d <- data.frame(tokio = tokio)
  perfil <- perfilar(d, analizar_dependencias = FALSE,
                     fecha = as.POSIXct("2026-01-15", tz = "UTC"))
  frecuencias <- distribucion_valores(d)$frecuencias
  expect_identical(columnas(perfil)$moda, "2024-03-10 14:30:00 UTC")
  expect_identical(frecuencias$valor[[1L]], "2024-03-10 14:30:00 UTC")
  expect_identical(frecuencias$frecuencia[[1L]], 3L)
})

test_that("H7: con scipen negativo las cifras publicadas como texto no salen en notacion cientifica", {
  fecha <- as.POSIXct("2026-01-15", tz = "UTC")
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  # comparar_perfiles(): 12 centinelas excluidos antes, 11 ahora.
  a <- perfilar(data.frame(x = c(rep(-999, 12), 1:20)), sentinelas_numericos = -999,
                fecha = fecha, analizar_dependencias = FALSE)
  b <- perfilar(data.frame(x = c(rep(-999, 11), 1:20)), sentinelas_numericos = -999,
                fecha = fecha, analizar_dependencias = FALSE)
  cambios <- as.data.frame(comparar_perfiles(a, b))
  fila <- cambios[cambios$aspecto == "alcance_resumen", ]
  expect_identical(c(fila$valor_anterior, fila$valor_actual), c("12", "11"))
  expect_identical(fila$evidencia, "Valores excluidos del resumen: 12 antes, 11 ahora.")
  # analizar_tiempo(): diez dias seguidos, nueve intervalos de un dia.
  tiempo <- analizar_tiempo(data.frame(d = as.Date("2026-01-01") + 0:9))
  expect_identical(tiempo$propuestas$evidencia,
                   "Moda de intervalos: 1 dias; 9 intervalos observados.")
  # El reporte: 8448 bytes son 8.25 Kb, que la consola redondea a 8.2.
  expect_identical(lupa:::.memoria_publicada_reporte(8448), "8448 bytes (8.2 Kb)")
  muestra <- list(estado = "disponible", indices_fila = 1:12, mostrados = 12L,
                  total = 12, alcance = "completo")
  expect_identical(lupa:::.resumir_valor_reporte(muestra),
                   "estado=disponible; filas mostradas=12 de 12; alcance=completo")
})

test_that("H7: un duplicado aproximado publica su distancia y su umbral sin exponente", {
  skip_if_not_installed("stringdist")
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  pares <- detectar_duplicados_aproximados(
    data.frame(n = c("Juan Perez", "Juan Perez ", "Maria"))
  )
  # A mano: las dos primeras filas coinciden tras recortar: distancia 0.
  expect_true(grepl("distancia 0 con jw (umbral 0.1)", pares$hallazgos$evidencia[[1L]],
                    fixed = TRUE))
})

test_that("H7: con coma decimal los extremos de un rango disjunto se separan con punto y coma", {
  anterior <- options(OutDec = ",")
  on.exit(options(anterior), add = TRUE)
  relaciones <- detectar_relaciones(data.frame(x = c(1, 12, 5)),
                                    data.frame(y = c(0.1, 0.75, 0.5)), podar = TRUE)
  podas <- attr(relaciones, "podas")
  # A mano: [1, 12] contra [0.1, 0.75], cada extremo con sus cifras.
  expect_identical(podas$detalle[podas$motivo == "rangos_disjuntos"],
                   "[1; 12] y [0,1; 0,75]")
})

test_that("H8: perfilar_dbi() no deja escapar el aviso crudo de bit64 y declara la perdida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  grande <- bit64::as.integer64("9007199254740993")
  avisos <- character()
  perdida <- withCallingHandlers(
    lupa:::.entero_perdido_en_conversion_dbi(list(grande)),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # A mano: 2^53 + 1 no entra en un doble; 12 si.
  expect_true(perdida)
  expect_false(lupa:::.entero_perdido_en_conversion_dbi(list(bit64::as.integer64(12))))
  expect_identical(avisos, character())
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE r31s_dev_g (ID INTEGER, B BIGINT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO r31s_dev_g VALUES (1, 9007199254740993), (2, 1), (3, 2),",
    "(4, -9007199254740993)"
  ))
  anterior <- options(warn = 0)
  on.exit(options(anterior), add = TRUE)
  crudos <- character()
  perfil <- withCallingHandlers(
    perfilar_dbi(con, "r31s_dev_g"),
    warning = function(w) {
      if (grepl("precision lost", conditionMessage(w), fixed = TRUE)) {
        crudos <<- c(crudos, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(crudos, character())
  columnas_dbi <- as.data.frame(perfil$resumen_tabla$columnas)
  expect_true(is.na(columnas_dbi$minimo[columnas_dbi$columna == "B"]))
})

test_that("H7: la etiqueta de un centinela y los conteos del HTML no dependen de scipen", {
  anterior <- options(scipen = -9)
  on.exit(options(anterior), add = TRUE)
  perfil <- perfilar(data.frame(monto = c(-99, 10, 20, 30, 40, 50)),
                     analizar_dependencias = FALSE,
                     fecha = as.POSIXct("2026-01-15", tz = "UTC"))
  evidencia <- hallazgos(perfil)$evidencia[
    hallazgos(perfil)$tipo_hallazgo == "faltantes_disfrazados"
  ]
  # A mano: el centinela por omision -99 aparece una vez.
  expect_identical(evidencia, "-99 (1)")
  expect_identical(lupa:::.html_texto(c(100, 101)), c("100", "101"))
})
