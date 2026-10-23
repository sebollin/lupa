# Ronda 32-E: la misma tabla por cualquier puerta, y los bordes de la entrada.
# Un bloque por hallazgo de la refutacion, con el caso que lo reproducia y la
# cifra rehecha a mano. Los que necesitan DBI, RSQLite o tibble se saltan sin
# ellos; los que cambian la zona horaria la restauran. El archivo es ASCII: lo
# que no lo es se arma con `intToUtf8()`.

# 2026-01-15 00:00:00 UTC: 20468 dias desde 1970 por 86400 segundos.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.fecha_fija_o153 <- function() .POSIXct(1768435200, tz = "UTC")

.tabla_o153 <- function() {
  data.frame(
    id = 1:6, zona = c("a", "b", "b", "c", "a", "S/D"),
    v = c(1, 2, 3, 4, NA, 100), w = c(2, 4, 6, 8, 10, 12),
    stringsAsFactors = FALSE
  )
}

.correos_o153 <- function() {
  c("ana.perez@example.org", "luis.diaz@example.org",
    "maria.gomez@example.org", "jose.ruiz@example.org",
    "carla.sosa@example.org", "pedro.lima@example.org")
}

.estimaciones_o153 <- function() {
  data.frame(celda = c("M", "I"), stat = c(0.42, 0.38), cv = c(0.08, 0.34),
             n = c(1200L, 90L))
}

# Corre la expresion y devuelve el error -o NULL- y los avisos que dio antes.
.condicion_o153 <- function(expr) {
  avisos <- character()
  error <- tryCatch(
    withCallingHandlers(
      {
        force(expr)
        NULL
      },
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    ),
    error = function(e) e
  )
  list(error = error, avisos = avisos)
}

# El error del paquete: sin la llamada de R -`call. = FALSE`-, con el mensaje
# esperado y sin un aviso de R antes.
.expect_error_paquete_o153 <- function(expr, mensaje, etiqueta) {
  r <- .condicion_o153(expr)
  expect_true(inherits(r$error, "error"), info = etiqueta)
  if (inherits(r$error, "error")) {
    expect_null(conditionCall(r$error), info = etiqueta)
    expect_true(
      grepl(mensaje, conditionMessage(r$error), fixed = TRUE),
      info = paste0(etiqueta, ": ", conditionMessage(r$error))
    )
  }
  expect_identical(r$avisos, character(), info = etiqueta)
}

.con_zona_o153 <- function(zona, codigo) {
  anterior <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(anterior)) Sys.unsetenv("TZ") else Sys.setenv(TZ = anterior),
          add = TRUE)
  Sys.setenv(TZ = zona)
  force(codigo)
}

.etiqueta_o153 <- function(nombre, valor) {
  paste0(nombre, " = ", paste(deparse(valor), collapse = ""))
}

test_that("H1: las funciones con la bandera de proteccion la rechazan igual", {
  # `detectar_discordancias()` y `descubrir_patrones()` la leian con `isTRUE()`:
  # NA, NULL, "si", "TRUE", dos valores o -1 APAGABAN la proteccion y publicaban
  # el correo crudo, mientras las otras once la rechazaban (dos con otro
  # mensaje). Una sola validacion, con un solo mensaje.
  d <- data.frame(correo = .correos_o153(), copia = .correos_o153(),
                  stringsAsFactors = FALSE)
  d$copia[[3L]] <- "otra.persona@example.org"
  s <- senal_redundante(c("correo", "copia"))
  f <- .fecha_fija_o153()
  p <- perfilar(d, fecha = f, analizar_dependencias = FALSE)
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "correo"))
  destino <- file.path(tempdir(), "o153-no-se-escribe")
  llamadas <- list(
    analizar = function(v) analizar(d, fecha = f, proteger_datos_personales = v),
    analizar_tiempo = function(v) analizar_tiempo(d, proteger_datos_personales = v),
    clasificar_variables = function(v) {
      clasificar_variables(d, proteger_datos_personales = v)
    },
    descubrir_patrones = function(v) {
      descubrir_patrones(d$correo, proteger_datos_personales = v)
    },
    detectar_dependencias = function(v) {
      detectar_dependencias(d, proteger_datos_personales = v)
    },
    detectar_discordancias = function(v) {
      detectar_discordancias(d, s, proteger_datos_personales = v)
    },
    detectar_duplicados_aproximados = function(v) {
      detectar_duplicados_aproximados(d, columnas = "correo",
                                      proteger_datos_personales = v)
    },
    distribucion_valores = function(v) {
      distribucion_valores(d, proteger_datos_personales = v)
    },
    estimar_costo = function(v) {
      estimar_costo(d, columnas = "correo", proteger_datos_personales = v)
    },
    # La bandera se valida antes de leer el objeto: alcanza con su clase.
    guardar_analisis = function(v) {
      guardar_analisis(structure(list(), class = "analisis"),
                       paste0(destino, ".rds"), proteger_datos_personales = v)
    },
    medir = function(v) medir(mod, d, fecha = f, proteger_datos_personales = v),
    perfilar = function(v) perfilar(d, fecha = f, proteger_datos_personales = v),
    reportar = function(v) {
      reportar(p, archivo = paste0(destino, ".html"),
               proteger_datos_personales = v)
    }
  )
  # Por construccion: una funcion exportada nueva con la bandera tiene que
  # entrar en la lista, o esta prueba falla.
  ns <- asNamespace("lupa")
  exportadas <- getNamespaceExports("lupa")
  con_bandera <- exportadas[vapply(exportadas, function(nombre) {
    objeto <- get(nombre, envir = ns)
    is.function(objeto) && "proteger_datos_personales" %in% names(formals(objeto))
  }, logical(1L))]
  expect_setequal(con_bandera, names(llamadas))
  invalidos <- list(NA, NULL, "si", "TRUE", c(TRUE, TRUE), -1)
  for (nombre in names(llamadas)) {
    for (valor in invalidos) {
      .expect_error_paquete_o153(
        llamadas[[nombre]](valor),
        "`proteger_datos_personales` debe ser TRUE o FALSE.",
        paste0(nombre, "(", .etiqueta_o153("proteger_datos_personales", valor), ")")
      )
    }
  }
  expect_false(file.exists(paste0(destino, ".rds")))
  expect_false(file.exists(paste0(destino, ".html")))
})

test_that("H1: TRUE enmascara y FALSE publica, en las dos que fallaban abiertas", {
  correos <- .correos_o153()
  # Los ejemplos de un patron van unidos en un texto: se busca cada correo.
  ejemplos_publican <- function(v) {
    texto <- paste(unlist(
      descubrir_patrones(correos, proteger_datos_personales = v)$ejemplos
    ), collapse = " ")
    any(vapply(correos, function(correo) grepl(correo, texto, fixed = TRUE),
               logical(1L)))
  }
  expect_false(ejemplos_publican(TRUE))
  expect_true(ejemplos_publican(FALSE))
  d <- data.frame(correo = correos, copia = correos, stringsAsFactors = FALSE)
  d$copia[[3L]] <- "otra.persona@example.org"
  s <- senal_redundante(c("correo", "copia"))
  publica <- function(v) {
    r <- detectar_discordancias(d, s, proteger_datos_personales = v)
    texto <- unlist(lapply(as.data.frame(r), as.character))
    any(grepl("maria.gomez@example.org", texto, fixed = TRUE)) ||
      any(grepl("otra.persona@example.org", texto, fixed = TRUE))
  }
  expect_false(publica(TRUE))
  expect_true(publica(FALSE))
})

test_that("H1: las puertas que la reciben por ... la validan igual", {
  mensaje <- "`proteger_datos_personales` debe ser TRUE o FALSE."
  # Ningun grupo llega a `min_filas`: la bandera no llegaba a `perfilar()` y
  # el NA pasaba en silencio.
  .expect_error_paquete_o153(
    perfilar_por(.tabla_o153(), "zona", min_filas = 100,
                 proteger_datos_personales = NA),
    mensaje, "perfilar_por(proteger_datos_personales = NA)"
  )
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "o153_correos",
                    data.frame(id = 1:6, correo = .correos_o153()))
  # Por DBI el resumen SQL la leia con `isTRUE()` y calculaba y guardaba el
  # valor que con TRUE declara omitido por privacidad.
  for (valor in list(NA, "si", c(TRUE, TRUE))) {
    .expect_error_paquete_o153(
      perfilar_dbi(con, "o153_correos", proteger_datos_personales = valor),
      mensaje, paste0("perfilar_dbi(", .etiqueta_o153("proteger", valor), ")")
    )
    .expect_error_paquete_o153(
      perfilar_coleccion(coleccion(con, "o153_correos"),
                         proteger_datos_personales = valor),
      mensaje, paste0("perfilar_coleccion(", .etiqueta_o153("proteger", valor), ")")
    )
  }
})

test_that("H2: medicion_desde_estimaciones() lee la fecha de texto en UTC, como medir()", {
  est <- .estimaciones_o153()
  # A mano: 2026-03-01 00:00:00 UTC = 20513 dias * 86400 = 1772323200 s. Con
  # `as.POSIXct()` en la sesion daba 03:00 UTC en Montevideo y las 15:00 del
  # dia anterior en Tokio, con el mismo id.
  for (zona in c("America/Montevideo", "Asia/Tokyo", "UTC")) {
    m <- .con_zona_o153(zona, medicion_desde_estimaciones(
      est, entidad = "e", atributo = "celda", fuente = "f", fecha = "2026-03-01"
    ))
    expect_identical(unique(as.numeric(m$fecha)), 1772323200, info = zona)
    expect_identical(unique(m$id_medicion), "estimaciones-20260301T000000-e",
                     info = zona)
  }
  # Una sola fecha valida, como en `medir()`: NA y dos fechas se aceptaban, y
  # NULL, un texto ilegible o TRUE abortaban con un error de R.
  for (malo in list(NA, NULL, as.Date(c("2026-03-01", "2026-03-02")), "ayer",
                    TRUE, character(0))) {
    .expect_error_paquete_o153(
      medicion_desde_estimaciones(est, entidad = "e", atributo = "celda",
                                  fuente = "f", fecha = malo),
      "`fecha` debe contener una fecha y hora v", .etiqueta_o153("fecha", malo)
    )
  }
})

test_that("H3: analizar() acepta la fecha de texto que aceptan perfilar() y medir()", {
  d <- data.frame(x = c(1, 2, 3), y = c("a", "b", "a"), stringsAsFactors = FALSE)
  # Abortaba con "argumento 'trim' invalido": el texto crudo llegaba al id.
  an <- .con_zona_o153("America/Montevideo", analizar(d, fecha = "2026-03-01"))
  expect_identical(as.numeric(an$meta$fecha), 1772323200)
  expect_identical(as.numeric(an$perfil$meta$fecha_hora), 1772323200)
})

test_that("H4: un nombre de columna de mas de 2500 bytes no aborta", {
  # A mano: la clave por bytes escribe cada byte con su codigo decimal y un
  # separador -hasta cuatro caracteres por byte- y se usaba entera como nombre
  # de variable, que R limita a 10.000 bytes: "k" + 2501 * 3 + 2500 = 10.004.
  largo <- strrep("n", 2501L)
  acentuado <- strrep(intToUtf8(233L), 1251L)
  expect_identical(nchar(acentuado, type = "bytes"), 2502L)
  for (nombre in list(largo, acentuado)) {
    d <- data.frame(a = c(1, NA, 3))
    names(d) <- nombre
    p <- perfilar(d, fecha = .fecha_fija_o153(), analizar_dependencias = FALSE)
    expect_identical(charToRaw(columnas(p)$columna), charToRaw(nombre))
  }
  # 2500 bytes caben ("k" + 2500 * 3 + 2499 = 10.000), pero el candidato
  # `nombre.1` del repetido ya no.
  n2500 <- strrep("n", 2500L)
  expect_identical(lupa:::.nombres_unicos(c(n2500, n2500)),
                   c(n2500, paste0(n2500, ".1")))
  # Control: los nombres cortos dan lo mismo que antes. A mano: el segundo "a"
  # toma "a.1"; el "a.1" declarado, ya usado, toma "a.1.1"; el tercer "a"
  # sigue en "a.2"; NA queda NA y el segundo "" toma ".1".
  expect_identical(
    lupa:::.nombres_unicos(c("a", "a", "a.1", "b", NA, NA, "a", "", "")),
    c("a", "a.1", "a.1.1", "b", NA, NA, "a.2", "", ".1")
  )
  # `medir()` nombraba los argumentos de `do.call(rbind)` con la metrica
  # instanciada, que lleva el nombre de la columna.
  n10000 <- strrep("v", 10000L)
  d <- data.frame(v = c(1, NA, 3))
  names(d) <- n10000
  m <- medir(modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", n10000)),
             d, fecha = .fecha_fija_o153())
  expect_identical(nrow(m), 3L)
  expect_identical(as.numeric(m$resultado), c(1, 0, 1))
})

test_that("H5: inferir_tipo() rechaza una tabla como sus hermanas", {
  v <- c("2026-01-01", "2026-01-02", "2026-01-03", "x")
  # A mano: 3 de 4 valores son fecha, 0,75: texto con el umbral 0,8 y fecha
  # con 0,7. Sobre la tabla respondia el nombre de su clase.
  expect_identical(inferir_tipo(v)$tipo, "texto")
  expect_identical(inferir_tipo(v, umbral = 0.7)$tipo, "fecha")
  df <- data.frame(f = v, stringsAsFactors = FALSE)
  una <- df[, "f", drop = FALSE]
  mensaje <- "`x` debe ser un vector, no un data.frame."
  .expect_error_paquete_o153(inferir_tipo(una), mensaje, "data.frame")
  .expect_error_paquete_o153(inferir_tipo(data.table::as.data.table(una)),
                             mensaje, "data.table")
  # La hermana que ya la rechazaba, con el mismo mensaje.
  .expect_error_paquete_o153(detectar_formatos_fecha(una), mensaje,
                             "detectar_formatos_fecha")
  skip_if_not_installed("tibble")
  .expect_error_paquete_o153(inferir_tipo(tibble::as_tibble(una)), mensaje,
                             "tibble")
})

test_that("H6: Inf en un tope entero da el error del paquete que nombra el argumento", {
  # `as.integer(Inf)` es NA con un aviso de R, y despues `head()` o `if`
  # abortaban. Ninguno de los seis admite Inf por diseno: se rechaza.
  d <- .tabla_o153()
  s <- senal_redundante(c("v", "w"))
  f <- .fecha_fija_o153()
  .expect_error_paquete_o153(sugerir_clave(d, maximo = Inf),
                             "`maximo` debe ser un entero positivo.", "sugerir_clave")
  .expect_error_paquete_o153(elegir_clave(d, maximo = Inf),
                             "`maximo` debe ser un entero positivo.", "elegir_clave")
  .expect_error_paquete_o153(
    detectar_discordancias(d, s, max_ejemplos = Inf),
    "`max_ejemplos` debe ser un entero finito de al menos 0.", "detectar_discordancias"
  )
  .expect_error_paquete_o153(
    perfilar(d, fecha = f, max_columnas_orden = Inf),
    "`max_columnas_orden` debe ser un entero finito de al menos 2.", "max_columnas_orden"
  )
  .expect_error_paquete_o153(
    perfilar(d, fecha = f, max_columnas_aritmetica = Inf),
    "`max_columnas_aritmetica` debe ser un entero finito de al menos 2.",
    "max_columnas_aritmetica"
  )
  .expect_error_paquete_o153(perfilar_por(d, "zona", min_filas = Inf),
                             "`min_filas` debe ser un entero positivo.", "perfilar_por")
  # Control: el tope de lo que se muestra sigue admitiendo Inf, y cero ejemplos
  # vale -no cita filas-.
  expect_s3_class(
    perfilar(d, fecha = f, max_patrones = Inf, analizar_dependencias = FALSE),
    "perfil"
  )
  sin_ejemplos <- detectar_discordancias(d, s, max_ejemplos = 0)
  expect_s3_class(sin_ejemplos, "data.frame")
})

test_that("H7: NULL, largo cero, NA o el tipo equivocado dan el error del paquete", {
  d <- .tabla_o153()
  f <- .fecha_fija_o153()
  p <- perfilar(d, fecha = f, analizar_dependencias = FALSE)
  p2 <- perfilar(d[1:4, ], fecha = f + 86400, analizar_dependencias = FALSE)
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "v"))
  m <- medir(mod, d, fecha = f)
  entorno <- environment()
  fecha <- "`fecha` debe contener una fecha y hora v"
  umbrales_perfil <- "Todos los umbrales deben estar entre 0 y 1."
  casos <- list(
    list(quote(medir(mod, d, fecha = TRUE)), fecha),
    list(quote(medir(mod, d, fecha = "ayer")), fecha),
    list(quote(reportar(p, archivo = NULL)), "`archivo` debe ser una ruta no vac"),
    list(quote(reportar(p, archivo = NA)), "`archivo` debe ser una ruta no vac"),
    list(quote(reportar(p, archivo = 1)), "`archivo` debe ser una ruta no vac"),
    list(quote(metrica("M", "s", "instanciaAtributo", "booleano", dimension = NULL)),
         "`dimension` debe ser una cadena no vac"),
    list(quote(metrica("M", "s", "instanciaAtributo", "booleano", dimension = list())),
         "`dimension` debe ser una cadena no vac"),
    list(quote(metrica("M", "s", "instanciaAtributo", "booleano", factor = character(0))),
         "`factor` debe ser una cadena no vac"),
    list(quote(metrica("M", "s", "instanciaAtributo", "booleano", factor = c("a", "b"))),
         "`factor` debe ser una cadena no vac"),
    list(quote(comparar_perfiles(p, p2, umbral_cambio = NULL)),
         "Los umbrales deben estar en [0, 1]"),
    list(quote(comparar_perfiles(p, p2, umbral_error = NULL)),
         "Los umbrales deben estar en [0, 1]"),
    list(quote(perfilar(d, fecha = f, umbral_faltantes_sospechoso = NULL)), umbrales_perfil),
    list(quote(perfilar(d, fecha = f, umbral_faltantes_error = NULL)), umbrales_perfil),
    list(quote(perfilar(d, fecha = f, umbral_patron_dominante = list())), umbrales_perfil),
    list(quote(perfilar(d, fecha = f, umbral_patron_raro = TRUE)), umbrales_perfil),
    list(quote(perfilar(d, fecha = f, max_columnas_dependencias = NULL)),
         "`max_columnas_dependencias` debe ser un entero positivo."),
    list(quote(perfilar(d, fecha = f, max_columnas_dependencias = c(1, 2))),
         "`max_columnas_dependencias` debe ser un entero positivo."),
    list(quote(perfilar(d, fecha = f, max_patrones = "5")),
         "`max_patrones` debe ser un entero positivo o Inf."),
    list(quote(perfilar(d, fecha = f, nombre = function(x) x)),
         "`nombre` debe ser una cadena no vac"),
    list(quote(perfilar(d, fecha = f, nombre = c("a", "b"))),
         "`nombre` debe ser una cadena no vac"),
    list(quote(analizar(d, fecha = f, nombre = function(x) x)),
         "`nombre` debe ser una cadena no vac"),
    list(quote(detectar_dependencias(d, min_observaciones = NULL)),
         "`min_observaciones` debe ser un entero positivo."),
    list(quote(detectar_dependencias(d, min_observaciones = c(2, 3))),
         "`min_observaciones` debe ser un entero positivo."),
    list(quote(detectar_dependencias(d, max_columnas = NULL)),
         "`max_columnas` debe ser un entero positivo."),
    list(quote(detectar_dependencias(d, umbral = list())),
         "Los umbrales deben ser proporciones finitas en [0, 1]."),
    list(quote(proponer_modelo(p, d, max_sugerencias = NULL)),
         "`max_sugerencias` debe ser un entero positivo."),
    list(quote(proponer_modelo(p, d, max_valores_dominio = c(5, 6))),
         "`max_valores_dominio` debe ser un entero positivo."),
    list(quote(descubrir_patrones(d$zona, na.rm = NA)), "`na.rm` debe ser TRUE o FALSE."),
    list(quote(descubrir_patrones(d$zona, na.rm = NULL)), "`na.rm` debe ser TRUE o FALSE."),
    list(quote(descubrir_patrones(d$zona, na.rm = c(1, 2))), "`na.rm` debe ser TRUE o FALSE."),
    list(quote(descubrir_patrones(d$zona, distinguir_mayusculas = NA)),
         "`distinguir_mayusculas` debe ser TRUE o FALSE."),
    list(quote(descubrir_patrones(d$zona, expandir = "si")),
         "`expandir` debe ser TRUE o FALSE."),
    # Con otra funcion los pesos no pesaban y se publicaban igual; con un largo
    # distinto abortaba al nombrarlos.
    list(quote(agregar(m, "atributo", "ratio", pesos = 1)),
         "se usa con `promedio_ponderado`"),
    list(quote(agregar(m, "atributo", "ratio", pesos = c(0, 0, 0, 0, 0, 1))),
         "se usa con `promedio_ponderado`")
  )
  for (caso in casos) {
    .expect_error_paquete_o153(eval(caso[[1L]], entorno), caso[[2L]],
                               paste(deparse(caso[[1L]]), collapse = ""))
  }
  # Control: los pesos pesan con `promedio_ponderado`. A mano: NoNulo da
  # 1 1 1 1 0 1 por fila; con todo el peso en la sexta da 1, y sin pesos el
  # ratio es 5 / 6.
  ponderado <- agregar(m, "atributo", "promedio_ponderado", pesos = c(0, 0, 0, 0, 0, 1))
  expect_identical(ponderado$resultado, 1)
  expect_equal(agregar(m, "atributo", "ratio")$resultado, 5 / 6)
})

test_that("H8: las opciones de texto dan el error del paquete que nombra el argumento", {
  # `match.arg()` crudo: con NA o un numero decia "'arg' must be NULL or a
  # character vector", y con dos textos "'arg' must be of length 1".
  d <- .tabla_o153()
  f <- .fecha_fija_o153()
  m <- medir(modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "v")),
             d, fecha = f)
  h <- historico_calidad(m)
  casos <- list(
    list("metodo_asociacion_numerica",
         function(v) analizar(d, fecha = f, metodo_asociacion_numerica = v)),
    list("metodo_numerico", function(v) detectar_asociaciones(d, metodo_numerico = v)),
    list("estrategia", function(v) {
      detectar_duplicados_aproximados(d, columnas = "zona", estrategia = v)
    }),
    list("estrategia", function(v) estimar_costo(d, columnas = "zona", estrategia = v)),
    list("tipo", function(v) escala(0.5, tipo = v)),
    list("tipo", function(v) validar_iso3166("UY", tipo = v)),
    list("funcion", function(v) agregar(m, "atributo", funcion = v)),
    list("detalle", function(v) historico_calidad(m, detalle = v)),
    list("detalle", function(v) acumular_historico(h, m, detalle = v)),
    list("nivel", function(v) detectar_deriva_calidad(h, nivel = v))
  )
  for (caso in casos) {
    for (valor in list(NA, 1, c("a", "b"))) {
      .expect_error_paquete_o153(
        caso[[2L]](valor), paste0("`", caso[[1L]], "` debe ser '"),
        .etiqueta_o153(caso[[1L]], valor)
      )
    }
  }
  expect_error(escala(0.5, tipo = "otro"),
               "`tipo` debe ser 'absoluto' o 'relativo'.", fixed = TRUE)
  # El envoltorio responde lo mismo que `match.arg()` en todo lo que este
  # acepta: NULL y el vector entero dan la primera opcion, un prefijo unico se
  # completa.
  nueva <- function(x = c("uno", "dos", "tres")) lupa:::.elegir_opcion(x)
  original <- function(x = c("uno", "dos", "tres")) match.arg(x)
  expect_identical(nueva(), original())
  for (valor in list(NULL, c("uno", "dos", "tres"), "dos", "tr")) {
    expect_identical(nueva(valor), original(valor))
  }
  expect_error(nueva("cuatro"), "`x` debe ser 'uno', 'dos' o 'tres'.", fixed = TRUE)
})

test_that("H8: control por la API publica, igual que antes", {
  m <- medir(modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "v")),
             .tabla_o153(), fecha = .fecha_fija_o153())
  expect_identical(escala(0.5, tipo = "rel"), escala(0.5, tipo = "relativo"))
  expect_identical(escala(0.5, tipo = NULL), escala(0.5))
  expect_identical(validar_iso3166(c("858", "999"), tipo = "num"),
                   validar_iso3166(c("858", "999"), tipo = "numerico"))
  expect_identical(historico_calidad(m, detalle = "comp"),
                   historico_calidad(m, detalle = "completo"))
})

test_that("H9: aplicar() devuelve la data.table con su espacio reservado", {
  dt <- data.table::data.table(a = c(" x", "y ", "z"), b = 1:3)
  plan <- planificar_limpieza(
    perfilar(dt, fecha = .fecha_fija_o153(), analizar_dependencias = FALSE), dt
  )
  plan$aplicar <- plan$estado == "lista"
  expect_true(any(plan$aplicar))
  res <- aplicar(plan, dt)$datos
  expect_s3_class(res, "data.table")
  expect_identical(res$a, c("x", "y", "z"))
  # `[[<-.data.frame` la copiaba sin reservar columnas: truelength 0.
  expect_gt(data.table::truelength(res), ncol(res))
  # El primer `:=` del usuario -en un entorno que conoce data.table- avisaba
  # "Se tomo una copia superficial".
  usuario <- new.env(parent = globalenv())
  usuario$res <- res
  r <- .condicion_o153(evalq(res[, nueva := 1L], usuario))
  expect_null(r$error)
  expect_identical(r$avisos, character())
  expect_identical(usuario$res$nueva, c(1L, 1L, 1L))
  expect_identical(names(dt), c("a", "b"))
  expect_identical(dt$a, c(" x", "y ", "z"))
})

test_that("H10 (decision): los nombres de fila cuentan en max_bytes_muestra y el recorte se declara", {
  # Decidido por el precedente: el paquete trata los nombres de fila como
  # datos y la muestra los materializa. La ayuda de `max_bytes_muestra` lo
  # dice; esta prueba fija la decision y que el recorte no es silencioso.
  d <- data.frame(x = c(seq_len(360) / 7, rep(NA, 40)),
                  g = rep(c("u", "v", "w"), length.out = 400L),
                  stringsAsFactors = FALSE)
  con_nombres <- d
  row.names(con_nombres) <- sprintf("registro-numero-%06d", 1:400)
  tope <- 25000
  # A mano: 400 dobles y 400 punteros a tres textos son unos 7.400 bytes; 400
  # textos distintos de 22 caracteres suman mas de 35.000.
  expect_lt(as.numeric(utils::object.size(d)), tope)
  expect_gt(as.numeric(utils::object.size(con_nombres)), tope)
  f <- .fecha_fija_o153()
  sin <- perfilar(d, fecha = f, max_bytes_muestra = tope, analizar_dependencias = FALSE)
  con <- perfilar(con_nombres, fecha = f, max_bytes_muestra = tope,
                  analizar_dependencias = FALSE)
  c_sin <- columnas(sin)
  c_con <- columnas(con)
  expect_equal(c_sin$n_filas_analizadas_tipo[c_sin$columna == "g"], 400)
  expect_false(any(c_sin$muestreado_tipo_inferido))
  expect_true(all(c_con$muestreado_tipo_inferido))
  expect_lt(c_con$n_filas_analizadas_tipo[c_con$columna == "g"], 400)
  expect_false("muestra_perfilado" %in% sin$cobertura_diagnosticos$diagnostico)
  fila <- con$cobertura_diagnosticos[
    con$cobertura_diagnosticos$diagnostico %in% "muestra_perfilado", ]
  expect_identical(nrow(fila), 1L)
  expect_true(grepl("manda el tope de bytes", fila$motivo, fixed = TRUE))
})

# El algoritmo anterior, tal como estaba: la salida nueva tiene que ser la misma.
.ordenar_referencia_o153 <- function(codigos) {
  if (length(codigos) < 2L) return(codigos)
  clases <- vapply(codigos, lupa:::.normalizacion_clase, integer(1L))
  salida <- integer()
  inicio <- 1L
  while (inicio <= length(codigos)) {
    fin <- inicio
    while (fin < length(codigos) && clases[[fin + 1L]] != 0L) fin <- fin + 1L
    if (fin > inicio) {
      bloque <- inicio:fin
      if (clases[[inicio]] == 0L) {
        marcas <- (inicio + 1L):fin
        orden <- c(inicio, marcas[order(clases[marcas], seq_along(marcas),
                                        method = "radix")])
      } else {
        orden <- bloque[order(clases[bloque], seq_along(bloque), method = "radix")]
      }
      salida <- c(salida, codigos[orden])
    } else {
      salida <- c(salida, codigos[inicio:fin])
    }
    inicio <- fin + 1L
  }
  salida
}

.quitar_acentos_referencia_o153 <- function(codigos, protecciones) {
  clase <- lupa:::.normalizacion_clase
  salida <- integer()
  i <- 1L
  while (i <= length(codigos)) {
    if (clase(codigos[[i]]) != 0L) {
      i <- i + 1L
      next
    }
    j <- i
    while (j < length(codigos) && clase(codigos[[j + 1L]]) != 0L) j <- j + 1L
    marcas <- if (j > i) codigos[(i + 1L):j] else integer()
    conservar <- any(vapply(protecciones, function(p) {
      identical(as.integer(p$base), as.integer(codigos[[i]])) &&
        identical(as.integer(p$marcas), as.integer(marcas))
    }, logical(1L)))
    salida <- c(salida, codigos[[i]], if (conservar) marcas else integer())
    i <- j + 1L
  }
  salida
}

test_that("H11: la normalizacion da la misma salida que el algoritmo anterior", {
  # Bases -con las tres protegidas por omision, en las dos cajas- y marcas de
  # clases 230, 220, 216 y 202, tambien al principio y fuera de orden.
  alfabeto <- c(97L, 110L, 78L, 117L, 103L, 771L, 776L, 769L, 790L, 795L, 801L)
  entradas <- list(integer())
  for (largo in 1:3) {
    grilla <- as.matrix(expand.grid(rep(list(alfabeto), largo),
                                    KEEP.OUT.ATTRS = FALSE))
    entradas <- c(entradas, lapply(seq_len(nrow(grilla)), function(i) {
      as.integer(unname(grilla[i, ]))
    }))
  }
  # Mas largas, con un generador congruencial fijo: sin tocar la semilla de R.
  estado <- 20261013
  siguiente <- function(modulo) {
    estado <<- (69069 * estado + 1) %% 4294967296
    as.integer(estado %% modulo)
  }
  for (k in 1:150) {
    largo <- 5L + siguiente(36L)
    entradas[[length(entradas) + 1L]] <- alfabeto[1L + vapply(
      seq_len(largo), function(j) siguiente(length(alfabeto)), integer(1L)
    )]
  }
  expect_identical(
    lapply(entradas, lupa:::.normalizacion_ordenar),
    lapply(entradas, .ordenar_referencia_o153)
  )
  perfil <- normalizacion()
  protecciones <- lupa:::.normalizacion_protecciones(perfil)
  expect_identical(
    lapply(entradas, lupa:::.normalizacion_quitar_acentos, perfil = perfil,
           protecciones = protecciones),
    lapply(entradas, .quitar_acentos_referencia_o153, protecciones = protecciones)
  )
  # A mano: la n con tilde se conserva -protegida-, la a con tilde la pierde, y
  # la marca sin base se descarta.
  expect_identical(
    lupa:::.normalizacion_quitar_acentos(c(771L, 110L, 771L, 97L, 769L), perfil,
                                         protecciones),
    c(110L, 771L, 97L)
  )
})

test_that("H11: la normalizacion es lineal en el largo de la celda", {
  skip_on_cran()
  # Crecia la salida con `c()` en un `while`, una vuelta por caracter:
  # cuadratica. Se compara con el costo de clasificar los mismos codigos, que
  # las dos versiones pagan y es lineal: la anterior tardaba mas de diez veces
  # eso con 50.000 caracteres; la nueva, lo mismo.
  codigos <- utf8ToInt(strrep("abcdefghij", 5000L))
  perfil <- normalizacion()
  protecciones <- lupa:::.normalizacion_protecciones(perfil)
  tiempo <- function(f) {
    min(vapply(1:2, function(i) system.time(f())[["elapsed"]], numeric(1L)))
  }
  base <- tiempo(function() vapply(codigos, lupa:::.normalizacion_clase, integer(1L)))
  ordenar <- tiempo(function() lupa:::.normalizacion_ordenar(codigos))
  quitar <- tiempo(function() {
    lupa:::.normalizacion_quitar_acentos(codigos, perfil, protecciones)
  })
  expect_lt(ordenar / base, 4)
  expect_lt(quitar / base, 4)
})
