# Reanudar una corrida larga de perfilar_coleccion() con `directorio_reanudacion`.
#
# La promesa: una corrida cortada -un proceso matado, una interrupcion, la
# conexion caida a mitad de una tabla- se sigue con `reanudar = TRUE`, y el
# objeto que devuelve es el mismo que el de una corrida sin cortes, fuera de lo
# que describe la ejecucion. Sin `directorio_reanudacion` nada cambia.

# Pesada para CRAN: procesos hijos y varias corridas de una coleccion.
testthat::skip_on_cran()

.rc_fecha <- as.POSIXct("2026-01-15 12:00:00", tz = "UTC")

.rc_tabla <- function(n, semilla) {
  set.seed(semilla)
  data.frame(
    id = seq_len(n),
    monto = round(stats::runif(n, 0, 1000), 2),
    nombre = sample(c("Ana", "Luis", "Marta", "Pedro", NA, "S/D"), n, TRUE),
    alta = format(as.Date("2020-01-01") + sample(0:2000, n, TRUE)),
    activo = sample(c(0L, 1L, NA), n, TRUE),
    codigo = sprintf("C%04d", sample(1:300, n, TRUE)),
    stringsAsFactors = FALSE
  )
}

# Una base en archivos: siete tablas en la principal, una mas en una base
# adjunta, una vacia y una con una columna toda NA. `t1` tiene fechas de
# nacimiento, que es donde `fecha` cambia lo que se publica.
.rc_base <- function(motor = "sqlite") {
  skip_if_not_installed("DBI")
  if (motor == "sqlite") skip_if_not_installed("RSQLite") else skip_if_not_installed("duckdb")
  carpeta <- tempfile("rc-base-")
  dir.create(carpeta)
  t1 <- .rc_tabla(900, 1)
  set.seed(11)
  t1$fecha_nacimiento <- format(as.Date("1950-01-01") + sample(0:30000, 900, TRUE))
  tablas <- list(
    t1 = t1, t2 = .rc_tabla(700, 2),
    t3 = data.frame(a = rep(NA_real_, 300), b = seq_len(300) / 7),
    t4 = .rc_tabla(5, 4)[0, ], t5 = .rc_tabla(600, 5), t6 = .rc_tabla(650, 6),
    t7 = .rc_tabla(500, 7)
  )
  if (motor == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(carpeta, "col.sqlite"))
    for (n in names(tablas)) DBI::dbWriteTable(con, n, tablas[[n]])
    DBI::dbDisconnect(con)
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(carpeta, "aux.sqlite"))
    DBI::dbWriteTable(con, "t8", .rc_tabla(400, 8))
    DBI::dbDisconnect(con)
  } else {
    con <- DBI::dbConnect(duckdb::duckdb(), file.path(carpeta, "col.duckdb"))
    for (n in names(tablas)) DBI::dbWriteTable(con, n, tablas[[n]])
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  list(carpeta = carpeta, motor = motor)
}

.rc_conectar <- function(base) {
  if (base$motor == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(base$carpeta, "col.sqlite"))
    DBI::dbExecute(con, paste0(
      "ATTACH DATABASE '", file.path(base$carpeta, "aux.sqlite"), "' AS aux"
    ))
    con
  } else {
    DBI::dbConnect(duckdb::duckdb(), file.path(base$carpeta, "col.duckdb"))
  }
}

.rc_desconectar <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.rc_declaradas <- function(motor) {
  if (motor == "sqlite") {
    data.frame(
      esquema = c(NA, NA, NA, NA, NA, NA, NA, "aux"),
      tabla = c("t1", "t2", "vista_x", "t3", "t4", "t5", "t6", "t8"),
      tipo = c("tabla", "tabla", "vista", "tabla", "tabla", "tabla", "tabla", "tabla"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      esquema = NA, tabla = c("t1", "t2", "vista_x", "t3", "t4", "t5", "t6", "t7"),
      tipo = c("tabla", "tabla", "vista", "tabla", "tabla", "tabla", "tabla", "tabla"),
      stringsAsFactors = FALSE
    )
  }
}

# La corrida, con sus avisos juntados (clase y texto) y sus mensajes callados.
# `cobertura_metricas = "completa"` con un tope que se agota a mitad de la
# coleccion: el tope se consume en orden y una tabla reusada lo tiene que
# consumir igual.
.rc_correr <- function(col, ...) {
  avisos <- list()
  x <- withCallingHandlers(
    perfilar_coleccion(col, fecha = .rc_fecha, cobertura_metricas = "completa",
                       tope_cobertura_metricas = 250, ...),
    warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- list(clase = class(w),
                                            texto = conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(x = x, avisos = avisos)
}

# Lo que describe la ejecucion: cuando se midio cada tabla, cuando empezo y
# termino la llamada, cuanto tardo cada consulta, y la corrida misma.
.rc_sin_ejecucion <- function(x) {
  x$resumen_coleccion$momento <- NULL
  x$meta$inicio <- NULL
  x$meta$fin <- NULL
  x$meta$reanudacion <- NULL
  if (is.data.frame(x$meta$lecturas)) {
    x$meta$lecturas$duracion_ms <- NULL
    x$meta$lecturas$cpu_ms <- NULL
  }
  for (k in seq_along(x$perfiles)) {
    x$perfiles[[k]]$resumen_tabla$sql$duracion_ms <- NULL
    x$perfiles[[k]]$resumen_tabla$sql$cpu_ms <- NULL
    x$perfiles[[k]]$resumen_tabla$tiempos <- NULL
    for (costo in c("costo_distintos", "costo_moda", "costo_mediana")) {
      x$perfiles[[k]]$resumen_tabla$meta[[costo]] <- NULL
    }
  }
  x
}

.rc_sin_avisos_de_reanudacion <- function(avisos) {
  Filter(function(w) !"lupa_aviso_reanudacion" %in% w$clase, avisos)
}

.rc_esperar_igual <- function(B, A) {
  expect_identical(.rc_sin_ejecucion(B$x), .rc_sin_ejecucion(A$x))
  expect_identical(.rc_sin_avisos_de_reanudacion(B$avisos), A$avisos)
}

.rc_corrida <- function(directorio) {
  corridas <- list.dirs(directorio, recursive = FALSE)
  corridas[grepl("^corrida-", basename(corridas), useBytes = TRUE)]
}

.rc_lineas <- function(corrida) readLines(file.path(corrida, "manifiesto.txt"))

# Lo que queda en el disco si el proceso muere despues de comprometer `n`
# tablas: el manifiesto hasta esa linea, sin el final. Los resultados de las
# tablas siguientes siguen en la carpeta, ahora sin su linea: no se tienen que
# adoptar.
.rc_cortar_manifiesto <- function(corrida, n) {
  lineas <- .rc_lineas(corrida)
  tablas <- which(startsWith(lineas, "tabla\t"))
  writeLines(lineas[seq_len(tablas[[n]])], file.path(corrida, "manifiesto.txt"))
}

# Un proceso hijo que carga el mismo paquete que esta sesion: el instalado bajo
# `R CMD check`, el fuente con `load_all()` en la suite local.
.rc_hijo <- function(base, directorio, declaradas) {
  skip_on_os("windows")
  ruta <- getNamespaceInfo("lupa", "path")
  instalado <- file.exists(file.path(ruta, "R", "lupa.rdb"))
  if (!instalado) skip_if_not_installed("pkgload")
  guion <- tempfile("rc-hijo-", fileext = ".R")
  archivo_pid <- tempfile("rc-pid-")
  writeLines(c(
    if (instalado) {
      sprintf("suppressPackageStartupMessages(library(lupa, lib.loc = %s))",
              deparse(dirname(ruta)))
    } else {
      sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(ruta))
    },
    sprintf("base <- %s", paste(deparse(base), collapse = " ")),
    sprintf("declaradas <- %s", paste(deparse(declaradas), collapse = " ")),
    if (base$motor == "sqlite") c(
      "con <- DBI::dbConnect(RSQLite::SQLite(), file.path(base$carpeta, 'col.sqlite'))",
      "DBI::dbExecute(con, paste0(\"ATTACH DATABASE '\", file.path(base$carpeta, 'aux.sqlite'), \"' AS aux\"))"
    ) else "con <- DBI::dbConnect(duckdb::duckdb(), file.path(base$carpeta, 'col.duckdb'))",
    "col <- coleccion(con, declaradas)",
    sprintf("writeLines(as.character(Sys.getpid()), %s)", deparse(archivo_pid)),
    sprintf(paste0(
      "suppressWarnings(perfilar_coleccion(col, fecha = as.POSIXct('2026-01-15 12:00:00', tz = 'UTC'), ",
      "cobertura_metricas = 'completa', tope_cobertura_metricas = 250, ",
      "directorio_reanudacion = %s))"), deparse(directorio))
  ), guion)
  # En primer plano dentro de un proceso bifurcado, y no con `wait = FALSE`: un
  # comando que el shell lanza en segundo plano hereda `SIGINT` ignorado, y la
  # interrupcion no le llegaba (medido: la corrida terminaba entera).
  trabajo <- parallel::mcparallel(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(guion),
    stdout = FALSE, stderr = FALSE
  ))
  list(guion = guion, archivo_pid = archivo_pid, trabajo = trabajo)
}

.rc_vivo <- function(pid) isTRUE(tools::pskill(pid, 0L))

# Espera a que el hijo haya comprometido `n` tablas y le manda la senal.
.rc_cortar_hijo <- function(hijo, directorio, senal, n = 2L) {
  limite <- Sys.time() + 180
  repeat {
    corrida <- .rc_corrida(directorio)
    listas <- if (length(corrida) == 1L && file.exists(file.path(corrida, "manifiesto.txt"))) {
      sum(startsWith(.rc_lineas(corrida), "tabla\t"))
    } else 0L
    if (listas >= n && file.exists(hijo$archivo_pid)) break
    if (Sys.time() > limite) stop("el proceso hijo no llego a comprometer tablas")
    Sys.sleep(0.02)
  }
  pid <- as.integer(readLines(hijo$archivo_pid))
  tools::pskill(pid, senal)
  parallel::mccollect(hijo$trabajo, wait = TRUE)
  expect_false(.rc_vivo(pid))
  pid
}

test_that("sin directorio_reanudacion la llamada es la de siempre", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite"))
  # Mismas consultas a la base, contadas en las funciones de DBI que van al
  # motor.
  cuenta <- new.env()
  cuenta$n <- 0L
  contar <- function(original) {
    force(original)
    function(...) {
      cuenta$n <- cuenta$n + 1L
      original(...)
    }
  }
  testthat::local_mocked_bindings(
    dbGetQuery = contar(DBI::dbGetQuery), dbSendQuery = contar(DBI::dbSendQuery),
    dbExistsTable = contar(DBI::dbExistsTable),
    dbListFields = contar(DBI::dbListFields), .package = "DBI"
  )
  A <- .rc_correr(col)
  n_a <- cuenta$n
  cuenta$n <- 0L
  B <- .rc_correr(col, directorio_reanudacion = NULL, reanudar = FALSE)
  expect_identical(cuenta$n, n_a)
  .rc_esperar_igual(B, A)
  expect_null(B$x$meta$reanudacion)
  expect_error(perfilar_coleccion(col, reanudar = TRUE),
               class = "lupa_error_reanudacion")
  expect_error(perfilar_coleccion(col, directorio_reanudacion = NA_character_),
               class = "lupa_error_reanudacion")
})

test_that("guardar la corrida no toca el generador de numeros de la sesion", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:2, ])
  set.seed(42)
  invisible(.rc_correr(col))
  sin <- .Random.seed
  set.seed(42)
  invisible(.rc_correr(col, directorio_reanudacion = file.path(base$carpeta, "r")))
  expect_identical(.Random.seed, sin)
})

test_that("kill -9 de un proceso hijo: se reanuda y da lo mismo que sin cortes", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  declaradas <- .rc_declaradas("sqlite")
  directorio <- file.path(base$carpeta, "reanudacion")
  hijo <- .rc_hijo(base, directorio, declaradas)
  pid <- .rc_cortar_hijo(hijo, directorio, tools::SIGKILL)
  corrida <- .rc_corrida(directorio)
  expect_length(corrida, 1L)
  # Un proceso matado no corre su `on.exit()`: el bloqueo queda tomado y
  # reanudar se niega diciendo de quien es y que borrar.
  expect_true(dir.exists(file.path(corrida, "bloqueo")))
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, declaradas)
  error <- tryCatch(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_s3_class(error, "lupa_error_reanudacion")
  expect_identical(error$motivo, "bloqueada")
  expect_true(grepl(as.character(pid), conditionMessage(error), fixed = TRUE, useBytes = TRUE))
  expect_true(grepl(file.path(corrida, "bloqueo"), conditionMessage(error), fixed = TRUE,
                    useBytes = TRUE))
  unlink(error$datos$bloqueo, recursive = TRUE)

  A <- .rc_correr(col)
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
  tablas <- B$x$meta$reanudacion$tablas
  expect_true(any(tablas$origen == "reusada"))
  expect_true(any(tablas$origen == "perfilada"))
  expect_false(dir.exists(file.path(corrida, "bloqueo")))
  # Ya no queda nada que reanudar.
  expect_error(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
               class = "lupa_error_reanudacion")
})

test_that("una interrupcion suelta el bloqueo y la reanudacion da lo mismo (DuckDB)", {
  base <- .rc_base("duckdb")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  declaradas <- .rc_declaradas("duckdb")
  directorio <- file.path(base$carpeta, "reanudacion")
  hijo <- .rc_hijo(base, directorio, declaradas)
  .rc_cortar_hijo(hijo, directorio, tools::SIGINT)
  corrida <- .rc_corrida(directorio)
  expect_false(dir.exists(file.path(corrida, "bloqueo")))
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, declaradas)
  A <- .rc_correr(col)
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
  expect_true(any(B$x$meta$reanudacion$tablas$origen == "reusada"))
})

test_that("dos procesos que reanudan la misma corrida: uno sigue y el otro se niega", {
  skip_on_os("windows")
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  declaradas <- .rc_declaradas("sqlite")
  directorio <- file.path(base$carpeta, "reanudacion")
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, declaradas)
  invisible(.rc_correr(col, directorio_reanudacion = directorio))
  .rc_cortar_manifiesto(.rc_corrida(directorio), 1L)
  ruta <- getNamespaceInfo("lupa", "path")
  instalado <- file.exists(file.path(ruta, "R", "lupa.rdb"))
  if (!instalado) skip_if_not_installed("pkgload")
  largada <- tempfile("rc-largada-")
  salidas <- c(tempfile("rc-salida-"), tempfile("rc-salida-"))
  for (salida in salidas) {
    guion <- tempfile("rc-carrera-", fileext = ".R")
    writeLines(c(
      if (instalado) {
        sprintf("suppressPackageStartupMessages(library(lupa, lib.loc = %s))",
                deparse(dirname(ruta)))
      } else sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(ruta)),
      sprintf("base <- %s", paste(deparse(base), collapse = " ")),
      sprintf("declaradas <- %s", paste(deparse(declaradas), collapse = " ")),
      "con <- DBI::dbConnect(RSQLite::SQLite(), file.path(base$carpeta, 'col.sqlite'))",
      "DBI::dbExecute(con, paste0(\"ATTACH DATABASE '\", file.path(base$carpeta, 'aux.sqlite'), \"' AS aux\"))",
      "col <- coleccion(con, declaradas)",
      sprintf("writeLines('listo', %s)", deparse(paste0(salida, ".listo"))),
      sprintf("while (!file.exists(%s)) Sys.sleep(0.005)", deparse(largada)),
      "r <- tryCatch({",
      "  suppressMessages(suppressWarnings(perfilar_coleccion(col,",
      "    fecha = as.POSIXct('2026-01-15 12:00:00', tz = 'UTC'),",
      "    cobertura_metricas = 'completa', tope_cobertura_metricas = 250,",
      sprintf("    directorio_reanudacion = %s, reanudar = TRUE)))", deparse(directorio)),
      "  'reanudo'",
      "}, lupa_error_reanudacion = function(e) e$motivo)",
      sprintf("writeLines(r, %s)", deparse(salida))
    ), guion)
    system2(file.path(R.home("bin"), "Rscript"), shQuote(guion), wait = FALSE,
            stdout = FALSE, stderr = FALSE)
  }
  limite <- Sys.time() + 180
  while (!all(file.exists(paste0(salidas, ".listo"))) && Sys.time() < limite) Sys.sleep(0.05)
  file.create(largada)
  while (!all(file.exists(salidas)) && Sys.time() < limite) Sys.sleep(0.1)
  resultados <- vapply(salidas, function(s) readLines(s)[[1L]], character(1L))
  expect_setequal(unname(resultados), c("reanudo", "bloqueada"))
})

test_that("la conexion que se cae a mitad de una tabla: lo que sigue se rehace al reanudar", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  declaradas <- .rc_declaradas("sqlite")
  col <- coleccion(con, declaradas)
  A <- .rc_correr(col)
  directorio <- file.path(base$carpeta, "reanudacion")
  # Desde la sexta consulta que nombra `t3`, toda llamada al motor falla como
  # falla una conexion caida, con `dbIsValid()` en TRUE.
  caida <- new.env()
  caida$vistas <- 0L
  caer <- function(original) {
    force(original)
    function(conn, ...) {
      pedido <- list(...)
      pedido <- if (length(pedido)) pedido[[1L]] else NULL
      if (is.character(pedido) &&
          any(grepl("t3", pedido, fixed = TRUE, useBytes = TRUE))) {
        caida$vistas <- caida$vistas + 1L
      }
      if (caida$vistas >= 6L) stop("server closed the connection unexpectedly", call. = FALSE)
      original(conn, ...)
    }
  }
  local({
    testthat::local_mocked_bindings(
      dbGetQuery = caer(DBI::dbGetQuery), dbSendQuery = caer(DBI::dbSendQuery),
      dbExistsTable = caer(DBI::dbExistsTable), dbListFields = caer(DBI::dbListFields),
      dbExecute = caer(DBI::dbExecute), .package = "DBI"
    )
    caida$con_directorio <- .rc_correr(col, directorio_reanudacion = directorio)
    caida$vistas <- 0L
    caida$sin_directorio <- .rc_correr(col)
  })
  # La corrida con la conexion caida devuelve lo de siempre.
  expect_identical(.rc_sin_ejecucion(caida$con_directorio$x),
                   .rc_sin_ejecucion(caida$sin_directorio$x))
  estados <- caida$con_directorio$x$meta$reanudacion$tablas$estado
  expect_true(any(estados %in% c("degradada", "fallida")))
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
  rehechas <- B$x$meta$reanudacion$tablas
  expect_setequal(rehechas$motivo[rehechas$origen == "perfilada"],
                  unique(estados[estados != "limpia"]))
})

test_that("un manifiesto danado: linea cortada, suma distinta, huerfanos", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite"))
  directorio <- file.path(base$carpeta, "reanudacion")
  A <- .rc_correr(col, conservar_perfiles = TRUE)
  invisible(.rc_correr(col, conservar_perfiles = TRUE, directorio_reanudacion = directorio))
  corrida <- .rc_corrida(directorio)
  .rc_cortar_manifiesto(corrida, 3L)
  manifiesto <- file.path(corrida, "manifiesto.txt")
  cat("tabla\t7\tlimpia\ttabla-000007-001.rds\t0123", file = manifiesto, append = TRUE)
  segundo <- file.path(corrida, "tabla-000002-001.rds")
  bytes <- readBin(segundo, "raw", file.size(segundo))
  bytes[length(bytes) - 3L] <- xor(bytes[length(bytes) - 3L], as.raw(1L))
  writeBin(bytes, segundo)
  writeBin(as.raw(1:10), file.path(corrida, ".tmp-huerfano.rds"))
  B <- .rc_correr(col, conservar_perfiles = TRUE, directorio_reanudacion = directorio,
                  reanudar = TRUE)
  .rc_esperar_igual(B, A)
  tablas <- B$x$meta$reanudacion$tablas
  expect_identical(tablas$motivo[tablas$identificador == "t2"], "suma_distinta")
  expect_identical(tablas$origen[tablas$identificador == "t1"], "reusada")
  # Los resultados que quedaron sin su linea no se adoptan.
  expect_true(all(tablas$origen[tablas$identificador %in% c("t5", "t6", "aux.t8")] == "perfilada"))
  expect_gte(B$x$meta$reanudacion$lineas_descartadas, 1L)
  expect_false(file.exists(file.path(corrida, ".tmp-huerfano.rds")))
  lineas <- .rc_lineas(corrida)
  nuevas <- lineas[seq.int(which(grepl("0123$", lineas, useBytes = TRUE)) + 1L, length(lineas))]
  expect_true(all(endsWith(nuevas, "\tFIN")))
})

# En Windows `cat(file =)` escribia el manifiesto en modo texto: cada salto con
# retorno, la ficha final quedaba `FIN\r` y ninguna tabla se reusaba. Ahora se
# escribe en bytes, y uno escrito asi se lee igual.
test_that("el manifiesto se escribe en bytes y uno con retorno de carro se lee igual", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:4, ])
  directorio <- file.path(base$carpeta, "reanudacion")
  A <- .rc_correr(col, conservar_perfiles = TRUE)
  invisible(.rc_correr(col, conservar_perfiles = TRUE, directorio_reanudacion = directorio))
  corrida <- .rc_corrida(directorio)
  manifiesto <- file.path(corrida, "manifiesto.txt")
  expect_false(any(readBin(manifiesto, "raw", file.size(manifiesto)) == as.raw(13L)))
  .rc_cortar_manifiesto(corrida, 2L)
  lineas <- .rc_lineas(corrida)
  writeBin(charToRaw(paste0(paste(lineas, collapse = "\r\n"), "\r\n")), manifiesto)
  B <- .rc_correr(col, conservar_perfiles = TRUE, directorio_reanudacion = directorio,
                  reanudar = TRUE)
  .rc_esperar_igual(B, A)
  tablas <- B$x$meta$reanudacion$tablas
  expect_identical(sum(tablas$origen == "reusada"), 2L)
  expect_identical(B$x$meta$reanudacion$lineas_descartadas, 0L)
})

test_that("la reanudacion usa la fecha de la corrida original y lo avisa", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite"))
  directorio <- file.path(base$carpeta, "reanudacion")
  otra <- as.POSIXct("1990-06-01 00:00:00", tz = "UTC")
  # Con los perfiles conservados, que es donde estan los hallazgos de fechas
  # posteriores a la del perfil.
  A <- .rc_correr(col, conservar_perfiles = TRUE)
  # El control: con otra fecha la corrida sin cortes publica otra cosa.
  con_otra <- withCallingHandlers(
    perfilar_coleccion(col, fecha = otra, cobertura_metricas = "completa",
                       tope_cobertura_metricas = 250, conservar_perfiles = TRUE),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_false(identical(.rc_sin_ejecucion(con_otra), .rc_sin_ejecucion(A$x)))
  invisible(.rc_correr(col, conservar_perfiles = TRUE, directorio_reanudacion = directorio))
  .rc_cortar_manifiesto(.rc_corrida(directorio), 2L)
  avisos <- list()
  B <- withCallingHandlers(
    perfilar_coleccion(col, fecha = otra, cobertura_metricas = "completa",
                       tope_cobertura_metricas = 250, conservar_perfiles = TRUE,
                       directorio_reanudacion = directorio, reanudar = TRUE),
    warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- list(clase = class(w), texto = conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  .rc_esperar_igual(list(x = B, avisos = avisos), A)
  expect_true(any(vapply(avisos, function(w) "lupa_aviso_reanudacion" %in% w$clase,
                         logical(1L))))
})

test_that("una escritura que falla corta la llamada y deja la corrida reanudable", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite"))
  directorio <- file.path(base$carpeta, "reanudacion")
  A <- .rc_correr(col)
  original <- lupa:::.escribir_rds_reanudacion
  local({
    testthat::local_mocked_bindings(.escribir_rds_reanudacion = function(objeto, final) {
      if (grepl("tabla-000004", basename(final), fixed = TRUE, useBytes = TRUE)) {
        return("No space left on device")
      }
      original(objeto, final)
    })
    error <- tryCatch(.rc_correr(col, directorio_reanudacion = directorio),
                      lupa_error_reanudacion = function(e) e)
    expect_s3_class(error, "lupa_error_reanudacion")
    expect_identical(error$motivo, "escritura")
  })
  corrida <- .rc_corrida(directorio)
  expect_false(dir.exists(file.path(corrida, "bloqueo")))
  expect_length(list.files(corrida, pattern = "^[.]tmp-", all.files = TRUE), 0L)
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
})

test_that("se reusa la corrida compatible mas reciente y se dice por que no otra", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:4, ])
  directorio <- file.path(base$carpeta, "reanudacion")
  expect_error(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
               class = "lupa_error_reanudacion")
  vieja <- .rc_correr(col, directorio_reanudacion = directorio)$x$meta$reanudacion$corrida
  Sys.sleep(1.1)
  nueva <- .rc_correr(col, directorio_reanudacion = directorio)$x$meta$reanudacion$corrida
  expect_false(identical(vieja, nueva))
  # Las dos sin terminar: se sigue la mas reciente.
  for (corrida in .rc_corrida(directorio)) .rc_cortar_manifiesto(corrida, 1L)
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  expect_identical(B$x$meta$reanudacion$corrida, nueva)
  # La mas reciente termino y la vieja no: no se vuelve a la vieja.
  error <- tryCatch(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_identical(error$motivo, "terminada")
  # Otra `muestra` u otro argumento de `...` no es la misma corrida, y se dice.
  error <- tryCatch(.rc_correr(col, muestra = 100, directorio_reanudacion = directorio,
                               reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_identical(error$motivo, "sin_corrida")
  expect_true(grepl("`muestra`", conditionMessage(error), fixed = TRUE, useBytes = TRUE))
  error <- tryCatch(.rc_correr(col, max_consultas = 500, directorio_reanudacion = directorio,
                               reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_true(grepl("`max_consultas`", conditionMessage(error), fixed = TRUE, useBytes = TRUE))
})

test_that("la misma coleccion en otro archivo no es la misma corrida", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  copia <- list(carpeta = tempfile("rc-copia-"), motor = "sqlite")
  dir.create(copia$carpeta)
  on.exit(unlink(copia$carpeta, recursive = TRUE), add = TRUE)
  file.copy(list.files(base$carpeta, full.names = TRUE), copia$carpeta)
  directorio <- file.path(base$carpeta, "reanudacion")
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  invisible(.rc_correr(coleccion(con, .rc_declaradas("sqlite")[1:2, ]),
                       directorio_reanudacion = directorio))
  .rc_cortar_manifiesto(.rc_corrida(directorio), 1L)
  otra <- .rc_conectar(copia)
  on.exit(.rc_desconectar(otra), add = TRUE, after = FALSE)
  error <- tryCatch(.rc_correr(coleccion(otra, .rc_declaradas("sqlite")[1:2, ]),
                               directorio_reanudacion = directorio, reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_identical(error$motivo, "sin_corrida")
  expect_true(grepl("otro archivo", conditionMessage(error), fixed = TRUE, useBytes = TRUE))
  # El mismo archivo nombrado con `./` si es la misma base.
  mismo <- DBI::dbConnect(RSQLite::SQLite(), file.path(base$carpeta, ".", "col.sqlite"))
  on.exit(DBI::dbDisconnect(mismo), add = TRUE, after = FALSE)
  expect_identical(
    lupa:::.identidad_fuente_reanudacion(mismo, data.frame(catalogo = NA, esquema = NA)),
    lupa:::.identidad_fuente_reanudacion(con, data.frame(catalogo = NA, esquema = NA))
  )
})

# Una base que cambio entre el corte y la reanudacion no se reanuda: las tablas
# guardadas la describen como era y las que faltan la describirian como es, y
# el resultado no seria el de ninguna corrida. Una corrida nueva la mide entera.
.rc_esperar_base_cambiada <- function(col, directorio, archivo) {
  error <- tryCatch(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
                    lupa_error_reanudacion = function(e) e)
  expect_s3_class(error, "lupa_error_reanudacion")
  expect_identical(error$motivo, "base_cambiada")
  # El nombre y no la ruta: el controlador la normaliza, y en macOS `tempfile()`
  # da `/var/...` y la ruta normalizada `/private/var/...`.
  expect_true(grepl(basename(archivo), conditionMessage(error), fixed = TRUE,
                    useBytes = TRUE))
  expect_true(grepl("reanudar = FALSE", conditionMessage(error), fixed = TRUE,
                    useBytes = TRUE))
  # Una segunda llamada no se acostumbra al cambio: se niega igual.
  otra <- tryCatch(.rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE),
                   lupa_error_reanudacion = function(e) e)
  expect_identical(otra$motivo, "base_cambiada")
  nueva <- .rc_correr(col, directorio_reanudacion = directorio)
  .rc_esperar_igual(nueva, .rc_correr(col))
}

test_that("si la base se escribio desde el corte, no se reanuda", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:4, ])
  directorio <- file.path(base$carpeta, "reanudacion")
  invisible(.rc_correr(col, directorio_reanudacion = directorio))
  .rc_cortar_manifiesto(.rc_corrida(directorio), 1L)
  Sys.sleep(1.1)
  DBI::dbExecute(con, "INSERT INTO t1 (id) VALUES (999999)")
  .rc_esperar_base_cambiada(col, directorio, file.path(base$carpeta, "col.sqlite"))
})

test_that("si el archivo se reemplazo por otra base en la misma ruta, no se reanuda", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  directorio <- file.path(base$carpeta, "reanudacion")
  con <- .rc_conectar(base)
  invisible(.rc_correr(coleccion(con, .rc_declaradas("sqlite")[1:4, ]),
                       directorio_reanudacion = directorio))
  .rc_desconectar(con)
  .rc_cortar_manifiesto(.rc_corrida(directorio), 1L)
  # Otra base con las mismas tablas, copiada encima.
  otra <- tempfile("rc-otra-", fileext = ".sqlite")
  on.exit(unlink(otra), add = TRUE)
  con_otra <- DBI::dbConnect(RSQLite::SQLite(), otra)
  t1 <- .rc_tabla(950, 21)
  t1$fecha_nacimiento <- "2000-01-01"
  DBI::dbWriteTable(con_otra, "t1", t1)
  DBI::dbWriteTable(con_otra, "t2", .rc_tabla(700, 22))
  DBI::dbWriteTable(con_otra, "t3", data.frame(a = 1:300, b = seq_len(300) / 3))
  DBI::dbDisconnect(con_otra)
  Sys.sleep(1.1)
  expect_true(file.copy(otra, file.path(base$carpeta, "col.sqlite"), overwrite = TRUE))
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  .rc_esperar_base_cambiada(coleccion(con, .rc_declaradas("sqlite")[1:4, ]), directorio,
                            file.path(base$carpeta, "col.sqlite"))
})

test_that("abrir y cerrar la base entre el corte y la reanudacion no es un cambio", {
  # En modo WAL el `-wal` se borra al cerrar y se vuelve a crear al abrir, con
  # otra fecha y la base igual: no puede negar la reanudacion.
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  DBI::dbExecute(con, "PRAGMA main.journal_mode = WAL")
  .rc_desconectar(con)
  directorio <- file.path(base$carpeta, "reanudacion")
  con <- .rc_conectar(base)
  invisible(.rc_correr(coleccion(con, .rc_declaradas("sqlite")[1:4, ]),
                       directorio_reanudacion = directorio))
  .rc_desconectar(con)
  .rc_cortar_manifiesto(.rc_corrida(directorio), 1L)
  Sys.sleep(1.1)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:4, ])
  A <- .rc_correr(col)
  B <- .rc_correr(col, directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
  expect_true(any(B$x$meta$reanudacion$tablas$origen == "reusada"))
})

test_that("una opcion que declara degradado sin fallar no hace rehacer las tablas", {
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, .rc_declaradas("sqlite")[1:4, ])
  directorio <- file.path(base$carpeta, "reanudacion")
  A <- .rc_correr(col, columnas_opcionales = "fecha_nacimiento")
  invisible(.rc_correr(col, columnas_opcionales = "fecha_nacimiento",
                       directorio_reanudacion = directorio))
  .rc_cortar_manifiesto(.rc_corrida(directorio), 3L)
  B <- .rc_correr(col, columnas_opcionales = "fecha_nacimiento",
                  directorio_reanudacion = directorio, reanudar = TRUE)
  .rc_esperar_igual(B, A)
  expect_gt(length(A$avisos), 0L)
  expect_identical(sum(B$x$meta$reanudacion$tablas$origen == "reusada"), 3L)
})

test_that("lo que no se puede reanudar se rechaza antes de perfilar, sin escribir nada", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  directorio <- tempfile("rc-rechazo-")
  for (con in list(DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
                   DBI::dbConnect(RSQLite::SQLite(), ""),
                   DBI::dbConnect(duckdb::duckdb()))) {
    DBI::dbWriteTable(con, "t1", .rc_tabla(20, 1))
    error <- tryCatch(perfilar_coleccion(coleccion(con, "t1"), directorio_reanudacion = directorio),
                      lupa_error_reanudacion = function(e) e)
    expect_identical(error$motivo, "identidad")
    .rc_desconectar(con)
  }
  expect_false(dir.exists(directorio))
  base <- .rc_base("sqlite")
  on.exit(unlink(base$carpeta, recursive = TRUE), add = TRUE)
  con <- .rc_conectar(base)
  on.exit(.rc_desconectar(con), add = TRUE, after = FALSE)
  col <- coleccion(con, "t1")
  for (argumentos in list(list(proteger_datos_personales = TRUE),
                          list(incluir_valores = FALSE))) {
    error <- tryCatch(do.call(perfilar_coleccion, c(list(col), argumentos,
                                                    list(directorio_reanudacion = directorio))),
                      lupa_error_reanudacion = function(e) e)
    expect_identical(error$motivo, "proteccion")
  }
  error <- tryCatch(perfilar_coleccion(col, columnas_opcionales = new.env(),
                                       directorio_reanudacion = directorio),
                    lupa_error_reanudacion = function(e) e)
  expect_identical(error$motivo, "configuracion")
  expect_true(grepl("columnas_opcionales", conditionMessage(error), fixed = TRUE, useBytes = TRUE))
  expect_false(dir.exists(directorio))
  # Un servidor, hasta que su identidad se compruebe.
  error <- tryCatch(lupa:::.identidad_fuente_reanudacion(
    structure(list(), class = "PqConnection"), data.frame(catalogo = NA, esquema = NA)
  ), lupa_error_reanudacion = function(e) e)
  expect_identical(error$motivo, "identidad")
})

test_that("la forma canonica distingue lo que cambia el resultado", {
  canonica <- lupa:::.forma_canonica_reanudacion
  # Clausuras con su propio ambiente, hijo de base: lo que capturan es solo `u`.
  clausura <- function(u) {
    local({ function(x) x > u }, envir = list2env(list(u = u), parent = baseenv()))
  }
  expect_false(identical(canonica(clausura(0.5), "f"), canonica(clausura(0.9), "f")))
  expect_identical(canonica(clausura(0.5), "f"), canonica(clausura(0.5), "f"))
  expect_false(identical(canonica(factor(c("a", "b")), "x"),
                         canonica(factor(c("a", "b"), levels = c("b", "a")), "x")))
  global <- function(x) x > .rc_umbral_global
  environment(global) <- globalenv()
  assign(".rc_umbral_global", 1, envir = globalenv())
  on.exit(rm(".rc_umbral_global", envir = globalenv()), add = TRUE)
  antes <- canonica(global, "g")
  assign(".rc_umbral_global", 2, envir = globalenv())
  expect_false(identical(antes, canonica(global, "g")))
  propio <- new.env(parent = baseenv())
  assign("recursiva", function(n) if (n > 0) recursiva(n - 1) else 0, envir = propio)
  environment(propio$recursiva) <- propio
  expect_type(canonica(propio$recursiva, "r"), "list")
  expect_error(canonica(new.env(), "e"), class = "lupa_error_reanudacion")
})
