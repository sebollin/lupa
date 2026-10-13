# O155: ronda 32-P, la refutacion de la proteccion tras la ronda 31. Seis
# hallazgos (H1 a H6), dos decisiones (D7, D9) y una fuga que aparecio al medir
# D9 (N1). Cada bloque rehace a mano la cifra de la sonda y falla contra el
# commit de la ronda 31 (40eb58d), salvo H6, que es documentacion: fija el
# mecanismo que la ayuda declara ahora.

# SQL Server con la sesion en sueco cita la celda sin comillas.
.r32_sueco <- function(x) {
  paste0("[FreeTDS][SQL Server]Konvertering av nvarchar vardet av typen ", x,
         " till datatypen int misslyckades.")
}

# El error del motor como lo arma odbc: `cli::cli_abort()` con cada linea del
# mensaje del motor como un elemento.
.r32_odbc <- function(texto_motor) {
  lineas <- strsplit(texto_motor, "\n", fixed = TRUE)[[1L]]
  tryCatch(
    cli::cli_abort(c(
      "!" = "ODBC failed with error 00000 from \\\n  [FreeTDS][SQL Server].",
      stats::setNames(c(lineas, "<SQL> 'SELECT 1'"),
                      c("x", rep("*", length(lineas)))),
      i = "From {.file nanodbc/nanodbc.cpp:1802}."
    ), class = "odbc_database_error", call = NULL),
    error = identity
  )
}

# Las hojas de un objeto, por tipo: los numeros y los textos, recorriendo
# listas y marcos de datos.
.r32_hojas <- function(x) {
  numeros <- numeric()
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.numeric(y)) {
      numeros <<- c(numeros, as.numeric(y))
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(x)
  list(numeros = numeros, textos = textos)
}

test_that("H1: el mensaje del motor se lee como lo dijo el motor, sin lo que agrega cli", {
  skip_if_not_installed("rlang")
  # Un numero partido en dos lineas es UN numero para la regla de digitos: el
  # fijo de ocho cifras, la cedula, el celular, y con el fin de linea de
  # Windows. cli ponia una vineta despues de cada salto y salian dos o tres
  # numeros cortos, que la segunda pasada publicaba.
  for (valor in c("2901\n1234", "4.685\n467-9", "099\n123\n456",
                  "2901\r\n1234", "2901\n\n1234")) {
    salida <- .mensaje_motor(.r32_odbc(.r32_sueco(valor)))
    for (tramo in strsplit(valor, "[\r\n]+")[[1L]]) {
      expect_false(grepl(tramo, salida, fixed = TRUE), info = valor)
    }
    # El diagnostico queda: el tipo de la celda y el de destino.
    expect_true(grepl("nvarchar", salida, fixed = TRUE), info = valor)
    expect_true(grepl("int", salida, fixed = TRUE), info = valor)
    expect_false(grepl("\\", salida, fixed = TRUE), info = valor)
  }
  # Y cli cortaba la linea al ancho de la consola, con dos espacios delante:
  # `2901 1234` salia partido con 48 a 52 columnas, un panel angosto.
  con_fuga <- 0L
  for (ancho in 40:60) {
    viejas <- options(cli.width = ancho)
    salida <- .mensaje_motor(.r32_odbc(.r32_sueco("2901 1234")))
    options(viejas)
    if (grepl("2901", salida, fixed = TRUE) ||
        grepl("1234", salida, fixed = TRUE)) {
      con_fuga <- con_fuga + 1L
    }
  }
  expect_identical(con_fuga, 0L)
  # Con el valor conocido -la coleccion leyo la base-, el piso lo encuentra
  # entero: `099;123;\n456`, con un signo que no une.
  previos_ids <- .fijar_identificadores_motor(NULL)
  previos <- .activar_valores_motor()
  on.exit({
    .fijar_valores_motor(previos)
    .fijar_identificadores_motor(previos_ids)
  }, add = TRUE)
  .sumar_valores_motor("099;123;\n456")
  salida <- .mensaje_motor(.r32_odbc(.r32_sueco("099;123;\n456")))
  expect_false(grepl("123", salida, fixed = TRUE))
  expect_false(grepl("456", salida, fixed = TRUE))
  # Un error que no es de rlang da el mismo texto que antes.
  simple <- simpleError(.r32_sueco("099;123;456"))
  expect_identical(.texto_condicion_motor(simple), conditionMessage(simple))
})

test_that("H2: un valor que termina en un salto y un ^ se guarda y se tapa", {
  previos_ids <- .fijar_identificadores_motor(NULL)
  previos <- .activar_valores_motor()
  on.exit({
    .fijar_valores_motor(previos)
    .fijar_identificadores_motor(previos_ids)
  }, add = TRUE)
  mensaje <- function(valor, columnas_vista) {
    .fijar_identificadores_motor(NULL)
    .activar_valores_motor()
    .sumar_identificadores_motor(c("clientes", "id", "nombre", "celular"))
    .sumar_valores_motor(valor)
    .sumar_identificadores_motor(c("vista", "id", columnas_vista))
    .sin_literales_motor(.r32_sueco(valor))
  }
  # Los seis de la sonda: la linea del `^` sigue con el texto del motor, y la
  # regla de la linea que el motor repite no corre en el mensaje.
  for (valor in c("099;123;456\n^", "099;123;456\n ^ ", "099|123|456\n^",
                  "099   123   456\n^", "4;685;467-9\n^",
                  "4111|1111|1111|1111\n^")) {
    salida <- mensaje(valor, "monto")
    expect_false(grepl("123", salida, fixed = TRUE), info = valor)
    expect_false(grepl("685", salida, fixed = TRUE), info = valor)
    expect_false(grepl("1111", salida, fixed = TRUE), info = valor)
  }
  salida <- mensaje("Florencia Martinez\n^", c("Florencia", "Martinez"))
  expect_false(grepl("Florencia", salida, fixed = TRUE))
  expect_false(grepl("Martinez", salida, fixed = TRUE))
  # La seleccion y la segunda pasada leen el valor con la misma regla: la del
  # peor contexto, que no omite la linea sobre un `^`.
  sola <- .publicar_lo_permitido_motor("099;123;456\n^", toda_palabra = TRUE)
  expect_true(grepl("123", sola, fixed = TRUE))
  expect_identical(.valores_que_lleva_el_motor("099;123;456\n^"),
                   "099;123;456\n^")
})

test_that("H3: el celular E.164 de planilla con punto medio o agrupado indio se tapa", {
  protegido <- "095 271 823"
  escrituras <- c(
    "59\u00b7895\u00b7271\u00b7823", "59\u00b7895\u00b7271\u00b7823,00",
    "59\u2219895\u2219271\u2219823", "59\u30fb895\u30fb271\u30fb823",
    "59,89,52,71,823", "59,89,52,71,823.00"
  )
  # La misma cifra entera que con puntos.
  expect_identical(
    unique(.cifra_entera_escrita(.normalizar_signos_digitos(escrituras))),
    "59895271823"
  )
  textos <- paste("whatsapp", escrituras)
  tapados <- .reemplazar_valores_protegidos(textos, protegido)
  expect_identical(sum(grepl("895", tapados, fixed = TRUE) |
                         grepl("89,52", tapados, fixed = TRUE)), 0L)
  # Controles: los separadores de la ronda 31, y otro celular que no es el
  # protegido, que queda.
  expect_false(grepl("895", .reemplazar_valores_protegidos(
    "whatsapp 59'895'271'823", protegido), fixed = TRUE))
  expect_identical(
    .reemplazar_valores_protegidos("whatsapp 59\u00b7896\u00b7271\u00b7823",
                                   protegido),
    "whatsapp 59\u00b7896\u00b7271\u00b7823"
  )
  # Y de punta a punta, en la copia de `analizar()`.
  set.seed(3212)
  n <- 6L
  nucleo <- sprintf("9%d%06d", sample(1:9, n, TRUE), sample(0:999999, n))
  d <- data.frame(
    id = seq_len(n), nombre = sprintf("Cliente Apellidoso %03d", seq_len(n)),
    celular = paste0("0", substr(nucleo, 1, 2), " ", substr(nucleo, 3, 5), " ",
                     substr(nucleo, 6, 8)),
    nota = paste("whatsapp", paste0("59\u00b78", substr(nucleo, 1, 2),
                                    "\u00b7", substr(nucleo, 3, 5), "\u00b7",
                                    substr(nucleo, 6, 8))),
    stringsAsFactors = FALSE
  )
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = c(nombre = "nombre_persona", celular = "telefono"),
    conservar_datos = TRUE
  )))
  en_claro <- vapply(seq_len(n), function(i) {
    grepl(nucleo[[i]], gsub("[^0-9]", "", an$datos$nota[[i]]), fixed = TRUE)
  }, logical(1L))
  expect_identical(sum(en_claro), 0L)
})

test_that("H4: los avisos de la coleccion pasan por el piso tambien si se corta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "clientes", data.frame(
    id = 1:12, nombre = c("Campos Cadena", sprintf("Cliente Numero %03d", 2:12)),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "otra", data.frame(id = 1:5, monto = 1:5 * 10))
  original <- perfilar_dbi
  for (conservar in c(TRUE, FALSE)) {
    # La primera tabla se lee y avisa citando su celda -el aviso de una vista
    # que cita la base-; en la segunda la corrida se corta, como con Ctrl+C: un
    # error de una tabla la coleccion lo registra y sigue.
    local_mocked_bindings(perfilar_dbi = function(conexion, tabla, ...) {
      if (grepl("otra", paste(format(tabla), collapse = ""), fixed = TRUE)) {
        # Una interrupcion, sin rlang (no esta en Suggests): una condicion de
        # clase `interrupt` que no es un error, asi que la coleccion no la
        # registra como el fallo de una tabla.
        stop(structure(class = c("interrupt", "condition"),
                       list(message = "", call = NULL)))
      }
      perfil <- original(conexion, tabla, ...)
      aviso <- structure(class = c("lupa_aviso_dbi", "warning", "condition"),
                         list(message = paste(
                           "El resumen SQL se calculo y se devuelve, pero la",
                           "muestra no: typen Campos Cadena till int"
                         ), call = NULL))
      warning(aviso)
      perfil
    })
    avisos <- character()
    resultado <- withCallingHandlers(
      tryCatch(
        perfilar_coleccion(coleccion(con, c("clientes", "otra")),
                           conservar_perfiles = conservar,
                           columnas_personales = "nombre"),
        interrupt = function(i) "cortada"
      ),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(resultado, "cortada")
    salieron <- grepl("El resumen SQL se calculo", avisos, fixed = TRUE)
    expect_true(any(salieron), info = paste("conservar_perfiles =", conservar))
    expect_false(any(grepl("Campos", avisos, fixed = TRUE)), info = paste("conservar_perfiles =", conservar))
    expect_false(any(grepl("Cadena", avisos, fixed = TRUE)), info = paste("conservar_perfiles =", conservar))
  }
})

test_that("H5: el entorno de la copia con atributos o con algo que no se lee", {
  cedula <- "4.451.537-2"
  ids <- .valores_identificantes(c(cedula, "Juana Perez"))
  con_nombre <- new.env()
  attr(con_nombre, "name") <- cedula
  con_nota <- new.env()
  con_nota$k <- 1
  attr(con_nota, "nota") <- cedula
  funcion <- local(function() 1)
  attr(environment(funcion), "nota") <- cedula
  promesa <- new.env()
  promesa$doc <- cedula
  delayedAssign("boom", stop("no se lee"), assign.env = promesa)
  activa <- new.env()
  activa$doc <- cedula
  makeActiveBinding("boom", function() stop("no se lee"), activa)
  for (x in list(con_nombre, con_nota, funcion, promesa, activa)) {
    expect_identical(.piso_valor_conservado(x, ids), "[valor protegido]")
  }
  # Controles: el entorno sin valores protegidos queda el mismo objeto, con su
  # atributo y una promesa que se lee.
  limpio <- new.env()
  limpio$k <- 1
  attr(limpio, "nota") <- "sin datos"
  delayedAssign("tarde", 2, assign.env = limpio)
  expect_identical(.piso_valor_conservado(limpio, ids), limpio)
  # De punta a punta: la copia de `analizar()` no lleva la cedula.
  d <- data.frame(id = 1:8, cedula = c(cedula, sprintf("1.%03d.%03d-1", 1:7, 1:7)),
                  nombre = c("Juana Perez", sprintf("Persona Numero %d", 2:8)),
                  stringsAsFactors = FALSE)
  d$extra <- I(c(list(con_nombre), rep(list(NULL), 7)))
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = c("cedula", "nombre"), conservar_datos = TRUE
  )))
  expect_false(isTRUE(grepRaw(cedula, serialize(an$datos, NULL), fixed = TRUE) > 0))
})

test_that("H6: el fijo de nadie de planilla y el entero que empieza con 9, como la ayuda", {
  # Documentacion: la ayuda declara ahora las dos familias. Se fija el
  # mecanismo que declara. El fijo `22877006` protegido se tapa en todas las
  # escrituras de su numero de planilla con el prefijo; uno que no es de nadie,
  # en ninguna, salvo que sus primeras ocho cifras sean una cedula entera, y eso
  # solo pasa con los miles y los decimales en cero o con espacios.
  escrituras <- function(cifras) {
    c(pegado = paste0("+", cifras),
      miles = paste(substr(cifras, 1, 2), substr(cifras, 3, 5),
                    substr(cifras, 6, 8), substr(cifras, 9, 11), sep = "."),
      miles_00 = paste0(paste(substr(cifras, 1, 2), substr(cifras, 3, 5),
                              substr(cifras, 6, 8), substr(cifras, 9, 11),
                              sep = "."), ",00"),
      espacio = paste(substr(cifras, 1, 2), substr(cifras, 3, 5),
                      substr(cifras, 6, 8), substr(cifras, 9, 11)))
  }
  tapa <- function(x, valores) .reemplazar_valores_protegidos(x, valores) != x
  expect_identical(unname(tapa(escrituras("59822877006"), "22877006")),
                   rep(TRUE, 4L))
  expect_identical(unname(tapa(escrituras("59822877006"), "21234567")),
                   rep(FALSE, 4L))
  expect_identical(unname(tapa(escrituras("59822877006"), "5.982.287-7")),
                   c(FALSE, FALSE, TRUE, TRUE))
  # El entero de ocho cifras que empieza con 9 es un celular sin su cero.
  expect_identical(unname(tapa(c("98276375", "98.276.375,00"), "098 276 375")),
                   c(TRUE, TRUE))
})

test_that("D7: la coleccion guarda huellas de los valores, no sus textos", {
  previos <- .activar_valores_motor()
  on.exit(.fijar_valores_motor(previos), add = TRUE)
  set.seed(3207)
  nombres <- vapply(seq_len(300L), function(i) {
    paste(paste0(sample(LETTERS, 1L), paste(sample(letters, 6L), collapse = "")),
          paste0(sample(LETTERS, 1L), paste(sample(letters, 8L), collapse = "")))
  }, character(1L))
  .sumar_valores_motor(nombres)
  indice <- .valores_motor()
  bytes <- serialize(indice, NULL)
  # Ninguno de los textos queda guardado...
  expect_identical(sum(vapply(nombres[1:50], function(v) {
    length(grepRaw(v, bytes, fixed = TRUE)) > 0L
  }, logical(1L))), 0L)
  # ...ocho bytes por valor, y no crece con su largo...
  expect_lt(as.numeric(utils::object.size(indice)), 300 * 8 + 2000)
  # ...y no se pierde ninguno: cada uno se encuentra citado en un mensaje, con
  # otra caja y otro separador, y se tapa.
  citados <- paste("typen", toupper(sub(" ", ".", nombres)), "till int")
  tapados <- .sin_literales_motor(citados)
  expect_identical(sum(vapply(seq_along(nombres), function(i) {
    grepl(strsplit(toupper(nombres[[i]]), " ")[[1L]][[1L]], tapados[[i]],
          fixed = TRUE)
  }, logical(1L))), 0L)
  # Lo que no se guardo no se tapa por el piso de los conocidos.
  expect_identical(.valores_conocidos_en("typen Nadie Nunca till int"),
                   character())
})

test_that("D9: el desvio de una columna protegida no se publica, por ninguna via", {
  set.seed(909)
  d <- data.frame(id = 1:40, cedula = as.numeric(sample(10000000:59999999, 40L)),
                  monto = round(stats::runif(40L, 100, 900), 2))
  desvio <- stats::sd(d$cedula)
  p <- suppressWarnings(perfilar(d, columnas_personales = "cedula",
                                 analizar_dependencias = FALSE))
  fila <- p$columnas[p$columnas$columna == "cedula", , drop = FALSE]
  expect_true(is.na(fila$desvio))
  expect_identical(as.character(fila$detalle_proteccion_personal),
                   "[estadisticos de orden y momentos protegidos]")
  # La columna que no se protege conserva su desvio.
  expect_true(is.finite(p$columnas$desvio[p$columnas$columna == "monto"]))
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  for (bloque in list(NULL, 13L)) {
    pd <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "t", columnas_personales = "cedula", bloque_filas = bloque
    )))
    columnas <- pd$resumen_tabla$columnas
    expect_true(is.na(columnas$desvio[columnas$columna == "cedula"]))
    sql <- pd$resumen_tabla$sql
    expect_identical(
      as.character(sql$estado[sql$columna == "cedula" & sql$metrica == "desvio"]),
      "omitido_por_privacidad"
    )
    numeros <- .r32_hojas(unclass(pd))$numeros
    expect_true(length(numeros) > 100L)
    expect_false(any(abs(numeros - desvio) < 1, na.rm = TRUE))
  }
})

test_that("N1: la via por bloques no publica los valores protegidos en meta$familias", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(5)
  d <- data.frame(
    id = 1:30, nota = round(stats::runif(30L, 1, 9), 3),
    cedula = as.numeric(sample(10000000:59999999, 30L)),
    nombre = paste("Persona", sample(c("Florencia", "Martina", "Lucia"), 30L,
                                     TRUE), sample(c("Rodriguez", "Pereyra"),
                                                   30L, TRUE)),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", columnas_personales = c("cedula", "nombre", "nota"),
    bloque_filas = 11L
  )))
  familias <- pd$resumen_tabla$meta$familias
  expect_true(length(familias) > 0L)
  hojas <- .r32_hojas(familias)
  expect_false(any(hojas$numeros %in% d$cedula))
  expect_false(any(hojas$numeros %in% d$nota))
  expect_false(any(hojas$textos %in% d$nombre))
  cuantitativos <- familias$nota$cuantitativos$resultado
  expect_true(all(is.na(unlist(cuantitativos[c("minimo", "maximo", "media",
                                                "desvio")]))))
  # Lo que no es un valor queda: los conteos, y los valores distintos de la
  # columna que no se protege.
  expect_identical(familias$nota$conteos$resultado$n_validos, 30)
  expect_true(all(familias$id$distintos$resultado$representante %in% d$id))
})
