# La conexion que se pierde a mitad de una llamada corta la llamada entera.
#
# Antes cada sitio que atrapaba un error del motor para degradar una metrica
# atrapaba tambien este: `perfilar_dbi()` devolvia un perfil parcial con fallas
# `rechazada`, `perfilar_coleccion()` salteaba la tabla y seguia, y con un
# controlador que reabre la sesion sola lo que seguia se leia en otra sesion.
# Ahora toda llamada al controlador pasa por un punto unico: reconoce la
# conexion perdida en el error mismo, corta con una condicion que nadie por
# encima puede tragarse, y no manda nada mas por esa conexion. Las pruebas
# afirman lo que ve quien llama: `lupa_error_conexion_dbi` en el borde de la
# funcion exportada, y ni una llamada al controlador despues del corte.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cpd_es_corte <- function(x) inherits(x, "lupa_error_conexion_dbi")

# Barre posiciones: en cada una la conexion se cierra justo antes de esa
# llamada al controlador. Devuelve, por posicion, que paso.
.cpd_barrer <- function(abrir, codigo, comparar, cada = 1L,
                        ademas = integer()) {
  referencia <- .cp_correr(abrir, codigo)
  expect_false(inherits(referencia$valor, "condition"))
  total <- referencia$k
  posiciones <- unique(c(seq(1L, total, by = cada), ademas, total))
  filas <- lapply(posiciones, function(k) {
    r <- .cp_correr(abrir, codigo, .cp_gancho_cierre(k))
    estado <- if (inherits(r$valor, "condition")) {
      if (.cpd_es_corte(r$valor)) "corte" else paste(class(r$valor)[[1L]])
    } else if (identical(comparar(r$valor), comparar(referencia$valor))) {
      "identico"
    } else "otro resultado"
    data.frame(k = k, estado = estado, tras = r$tras,
               pila_vacia = r$pila_vacia, stringsAsFactors = FALSE)
  })
  list(total = total, filas = do.call(rbind, filas))
}

.cpd_esperar_barrido <- function(barrido, minimo) {
  filas <- barrido$filas
  expect_gte(barrido$total, minimo)
  # Nunca otra clase de error ni otro valor.
  expect_identical(
    setdiff(unique(filas$estado), c("corte", "identico")), character()
  )
  expect_gte(sum(filas$estado == "corte"), 1L)
  # Despues del corte no llega ninguna llamada al controlador. La posicion 1
  # no es un corte: la conexion ya estaba cerrada al entrar.
  expect_identical(sum(filas$tras[filas$k > 1L]), 0L)
  # Y la corrida no queda abierta.
  expect_true(all(filas$pila_vacia))
}

test_that("perfilar_dbi() corta en cualquier llamada al controlador (SQLite)", {
  skip_if_not_installed("RSQLite")
  barrido <- .cpd_barrer(
    .cp_abrir_sqlite,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA),
    function(p) p$resumen_tabla$columnas
  )
  .cpd_esperar_barrido(barrido, 20L)
})

test_that("perfilar_dbi() corta en cualquier llamada al controlador (DuckDB)", {
  skip_if_not_installed("duckdb")
  barrido <- .cpd_barrer(
    .cp_abrir_duckdb,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA),
    function(p) p$resumen_tabla$columnas, cada = 3L
  )
  .cpd_esperar_barrido(barrido, 20L)
})

test_that("medir_dbi() corta y no publica una medicion vacia", {
  skip_if_not_installed("RSQLite")
  modelo_cp <- .cp_modelo()
  barrido <- .cpd_barrer(
    .cp_abrir_sqlite,
    function(con) {
      medir_dbi(modelo_cp, con, fecha = .CP_FECHA, id_medicion = "fijo")
    },
    function(m) as.data.frame(m)[c("metrica_instanciada", "valor")]
  )
  .cpd_esperar_barrido(barrido, 3L)
})

test_that("perfilar_coleccion() no saltea la tabla y sigue: corta", {
  skip_if_not_installed("RSQLite")
  barrido <- .cpd_barrer(
    .cp_abrir_sqlite,
    function(con) {
      perfilar_coleccion(
        coleccion(con, c("lupa_t", "lupa_u")), fecha = .CP_FECHA
      )
    },
    function(p) p$resumen_coleccion[c("tabla", "n_filas", "n_columnas")],
    # Las posiciones 2 y 3 son las que siguen a la validacion de entrada: la
    # conexion ya dijo que era valida y todavia no contesto ninguna sentencia.
    # Ahi la coleccion devolvia todas sus tablas como no perfiladas.
    cada = 4L, ademas = 2:3
  )
  .cpd_esperar_barrido(barrido, 40L)
  # Ninguna posicion devuelve una coleccion con tablas marcadas como fallidas.
  expect_false(any(barrido$filas$estado == "otro resultado"))
})

test_that("relaciones_coleccion() y detectar_tablas_codigos() tambien cortan", {
  skip_if_not_installed("RSQLite")
  relaciones <- .cpd_barrer(
    .cp_abrir_sqlite,
    function(con) {
      relaciones_coleccion(
        coleccion(con, c("lupa_t", "lupa_u")),
        pares = data.frame(tabla_1 = "lupa_t", tabla_2 = "lupa_u")
      )
    },
    function(r) r$relaciones, cada = 2L
  )
  .cpd_esperar_barrido(relaciones, 6L)
  abrir_con_perfil <- function() {
    con <- .cp_abrir_sqlite()
    DBI::dbExecute(
      con, "CREATE TABLE lupa_cod (cod INTEGER PRIMARY KEY, etiqueta TEXT)"
    )
    DBI::dbExecute(con, "INSERT INTO lupa_cod VALUES (0, 'cero'), (1, 'uno')")
    con
  }
  codigos <- .cpd_barrer(
    abrir_con_perfil,
    function(con) {
      col <- coleccion(con, c("lupa_t", "lupa_cod"))
      detectar_tablas_codigos(col, perfilar_coleccion(col, fecha = .CP_FECHA))
    },
    function(d) d$propuesta, cada = 6L
  )
  .cpd_esperar_barrido(codigos, 20L)
})

test_that("el mensaje se arma una vez, con el motivo del primer corte", {
  skip_if_not_installed("RSQLite")
  # Cada llamada posterior al corte levantaria OTRA condicion de conexion: si
  # alguna llegara al controlador -la limpieza de un resultado, un `on.exit()`-
  # su texto reemplazaria al primero.
  # El corte fabricado es el que escribe el cliente de PostgreSQL: con RSQLite
  # el texto solo no alcanza, lo confirma `dbIsValid()`.
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  n <- 0L
  gancho <- function(est, operacion, conexion, sql) {
    if (est$k >= 9L) {
      n <<- n + 1L
      est$lanzar <- simpleError(paste0(
        "Failed to prepare query : server closed the connection unexpectedly",
        " (corte numero ", n, ")"
      ))
    }
  }
  r <- .cp_correr(
    .cp_abrir_sqlite,
    function(con) {
      perfilar_coleccion(
        coleccion(con, c("lupa_t", "lupa_u")), fecha = .CP_FECHA
      )
    },
    gancho
  )
  expect_s3_class(r$valor, "lupa_error_conexion_dbi")
  expect_s3_class(r$valor, "error")
  mensaje <- conditionMessage(r$valor)
  encabezado <- paste0("La conexi", .CP_O, "n dej", .CP_O, " de estar disponible")
  expect_identical(
    lengths(regmatches(mensaje, gregexpr(encabezado, mensaje, fixed = TRUE))),
    1L
  )
  expect_true(grepl("(corte numero 1)", mensaje, fixed = TRUE))
  expect_false(grepl("corte numero 2", mensaje, fixed = TRUE))
  expect_identical(r$tras, 0L)
  # El corte no es un error que la coleccion pueda tragarse para seguir con la
  # tabla siguiente: despues de el no se intenta empezar ninguna otra tabla (la
  # primera pregunta de cada tabla es si la conexion es valida).
  expect_gte(r$k, 9L)
  expect_false("dbIsValid" %in% r$ops[seq_along(r$ops) > 9L])
  # Con tildes, y sin la palabra sin tilde.
  expect_true(grepl(paste0("otra sesi", .CP_O, "n"), mensaje, fixed = TRUE))
  expect_false(grepl("conexion|sesion|coleccion", mensaje, perl = TRUE))
})

test_that("un resultado ya cerrado no es una conexion perdida", {
  skip_if_not_installed("RSQLite")
  con <- .cp_abrir_sqlite()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  resultado <- DBI::dbSendQuery(con, "SELECT id FROM lupa_t")
  DBI::dbClearResult(resultado)
  # El resultado es invalido y la conexion no.
  expect_false(DBI::dbIsValid(resultado))
  expect_true(DBI::dbIsValid(con))
  fallo <- tryCatch(
    .dbi("dbFetch", con, DBI::dbFetch(resultado, n = 1L)),
    error = function(e) e
  )
  expect_false(.cpd_es_corte(fallo))
  expect_identical(.reconocido_dbi(fallo), "rechazada")
  # Y la conexion sigue sirviendo para una llamada entera.
  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
  ))
  expect_s3_class(perfil, "perfil_dbi")
})

test_that("una conexion invalida al entrar da el error de la validacion, no un corte", {
  skip_if_not_installed("RSQLite")
  con <- .cp_abrir_sqlite()
  DBI::dbDisconnect(con)
  fallo <- tryCatch(perfilar_dbi(con, "lupa_t"), error = function(e) e)
  expect_s3_class(fallo, "lupa_error_conexion_dbi")
  expect_true(grepl("abierta y v", conditionMessage(fallo), fixed = TRUE))
  fallo <- tryCatch(coleccion(con, "lupa_t"), error = function(e) e)
  expect_s3_class(fallo, "lupa_error_conexion_dbi")
  expect_false(isTRUE(fallo$datos$perdida))
})

# ---- La sesion que el controlador reabre sin avisar ----------------------
#
# RPostgres reabre la sesion sola despues de un corte, y RMariaDB con
# `reconnect = TRUE` tambien: no hay error que reconocer, y lo que sigue se lee
# en otra sesion. Lo unico que lo dice es el identificador de sesion que el
# controlador guarda del lado del cliente. Aca una conexion de SQLite lo
# publica como `pid` y la prueba lo cambia.
.cpd_sesion <- new.env(parent = emptyenv())
.cpd_sesion$pid <- 100L

if (requireNamespace("RSQLite", quietly = TRUE)) {
  methods::setClass("LupaConexionConSesion", contains = "SQLiteConnection")
  methods::setMethod(
    DBI::dbGetInfo, "LupaConexionConSesion",
    function(dbObj, ...) {
      heredado <- methods::selectMethod(DBI::dbGetInfo, "SQLiteConnection")
      c(heredado(dbObj, ...), list(pid = .cpd_sesion$pid))
    }
  )
}

.cpd_abrir_con_sesion <- function() {
  .cpd_sesion$pid <- 100L
  methods::new("LupaConexionConSesion", .cp_abrir_sqlite())
}

.cpd_gancho_reapertura <- function(objetivo) {
  function(est, operacion, conexion, sql) {
    if (est$k == objetivo) .cpd_sesion$pid <- 200L
  }
}

test_that("si el identificador de sesion no cambia, la llamada da lo de siempre", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  r <- .cp_correr(
    .cpd_abrir_con_sesion,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
  )
  expect_s3_class(r$valor, "perfil_dbi")
  simple <- .cp_correr(
    .cp_abrir_sqlite,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
  )
  expect_identical(
    r$valor$resumen_tabla$columnas, simple$valor$resumen_tabla$columnas
  )
})

test_that("una sesion reabierta sin error corta al salir y no publica", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  referencia <- .cp_correr(
    .cpd_abrir_con_sesion,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
  )
  for (k in c(3L, referencia$k %/% 2L, referencia$k)) {
    r <- .cp_correr(
      .cpd_abrir_con_sesion,
      function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA),
      .cpd_gancho_reapertura(k)
    )
    expect_s3_class(r$valor, "lupa_error_conexion_dbi")
    expect_true(grepl(
      paste0("reabri", .CP_O, " la sesi", .CP_O, "n"),
      conditionMessage(r$valor), fixed = TRUE
    ), info = paste("posicion", k))
  }
})

test_that("en una coleccion la sesion reabierta corta en el borde de la tabla", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  codigo <- function(con) {
    perfilar_coleccion(coleccion(con, c("lupa_t", "lupa_u")), fecha = .CP_FECHA)
  }
  referencia <- .cp_correr(.cpd_abrir_con_sesion, codigo)
  # La sesion cambia justo en la ultima sentencia de la primera tabla: no hay
  # despues ningun error ni ningun cierre de resultado que lo delate, solo el
  # borde entre las dos tablas.
  ultima_de_t <- max(which(grepl("lupa_t", referencia$sql, fixed = TRUE)))
  expect_identical(referencia$ops[[ultima_de_t]], "dbGetQuery")
  expect_lt(ultima_de_t, referencia$k)
  r <- .cp_correr(
    .cpd_abrir_con_sesion, codigo, .cpd_gancho_reapertura(ultima_de_t)
  )
  expect_s3_class(r$valor, "lupa_error_conexion_dbi")
  # De la segunda tabla no llega nada al controlador: se leeria en la sesion
  # nueva.
  expect_identical(r$llegaron, ultima_de_t)
})

test_that("cerrar un resultado en una sesion reabierta corta ahi mismo", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  codigo <- function(con) {
    perfilar_dbi(con, "lupa_t", muestra = 10, fecha = .CP_FECHA)
  }
  referencia <- .cp_correr(.cpd_abrir_con_sesion, codigo)
  cierres <- which(referencia$ops == "dbClearResult")
  expect_gte(length(cierres), 1L)
  objetivo <- cierres[[1L]]
  r <- .cp_correr(
    .cpd_abrir_con_sesion, codigo, .cpd_gancho_reapertura(objetivo)
  )
  expect_s3_class(r$valor, "lupa_error_conexion_dbi")
  # Nada llega al controlador despues de ese cierre.
  expect_identical(r$llegaron, objetivo)
})

test_that("un error cualquiera en una sesion reabierta es un corte", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  # El error que da la tabla temporal que la sesion nueva ya no tiene.
  gancho <- function(est, operacion, conexion, sql) {
    if (est$k == 8L) {
      .cpd_sesion$pid <- 200L
      est$lanzar <- simpleError(
        "Failed to prepare query : ERROR:  relation \"lupa_tmp\" does not exist\n"
      )
    }
  }
  r <- .cp_correr(
    .cpd_abrir_con_sesion,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA), gancho
  )
  expect_s3_class(r$valor, "lupa_error_conexion_dbi")
  expect_identical(r$tras, 0L)
})

# ---- La conexion cortada no se vuelve a usar -----------------------------
#
# lupa no vuelve a usar una conexion en la que reconocio un corte, hasta que se
# cierre, con ningun controlador. Con odbc `dbIsValid()` sigue en `TRUE` y la
# sentencia siguiente hace caer a R; RPostgres, y RMariaDB con `reconnect =
# TRUE`, reabren la sesion dentro del mismo objeto, y la llamada siguiente
# devolvia un perfil leido en otra sesion sin decir nada. La conexion se
# reconoce por su puntero externo: ni por `dbGetInfo()` -dos conexiones sanas
# dan lo mismo- ni por la direccion que guarda el puntero, que se reutiliza.
.CPD_ERROR_FREETDS <- structure(
  class = c("nanodbc::database_error", "C++Error", "error", "condition"),
  list(
    message = "nanodbc/nanodbc.cpp:1591: 00000\n[FreeTDS][SQL Server]Unknown error ",
    call = NULL
  )
)

.cpd_perder <- function(con) {
  r <- .cp_correr(
    function() con,
    function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA),
    function(est, operacion, conexion, sql) {
      if (est$k == 7L) est$lanzar <- .CPD_ERROR_FREETDS
    }
  )
  r
}

.cpd_abrir_archivo <- function(archivo) {
  con <- DBI::dbConnect(RSQLite::SQLite(), archivo)
  if (!DBI::dbExistsTable(con, "lupa_t")) {
    DBI::dbWriteTable(con, "lupa_t", .cp_tabla())
  }
  con
}

test_that("una conexion cortada no se vuelve a usar, y una nueva y sana nunca se toma por cortada", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "odbc")
  archivo <- tempfile(fileext = ".sqlite")
  # El archivo existe antes de la primera conexion: RSQLite publica la ruta
  # resuelta de un archivo que existe y la pedida de uno que no, y si la
  # carpeta temporal es un enlace (macOS) la primera y la segunda diferirian.
  file.create(archivo)
  on.exit(unlink(archivo), add = TRUE)
  con <- .cpd_abrir_archivo(archivo)
  otra <- .cpd_abrir_archivo(archivo)
  on.exit(suppressWarnings(try(DBI::dbDisconnect(otra), silent = TRUE)),
          add = TRUE)
  # Dos conexiones sanas al mismo archivo declaran lo mismo.
  expect_identical(DBI::dbGetInfo(con), DBI::dbGetInfo(otra))

  # `.cp_correr()` cierra la conexion al salir: aca tiene que seguir abierta,
  # como la deja odbc.
  perder <- function(con) {
    est <- .cp_estado()
    .cp_imitar_dbi(est, function(est, operacion, conexion, sql) {
      if (est$k == 7L) est$lanzar <- .CPD_ERROR_FREETDS
    })
    tryCatch(
      suppressWarnings(suppressMessages(
        perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
      )),
      error = function(e) e
    )
  }
  contar <- function(con) {
    est <- .cp_estado()
    .cp_imitar_dbi(est)
    valor <- tryCatch(
      suppressWarnings(suppressMessages(
        perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
      )),
      error = function(e) e
    )
    list(valor = valor, llegaron = est$llegaron)
  }
  primera <- perder(con)
  expect_s3_class(primera, "lupa_error_conexion_dbi")
  expect_true(grepl("Unknown error", conditionMessage(primera), fixed = TRUE))
  expect_true(DBI::dbIsValid(con))

  # La misma conexion, otra llamada: corta sin llegar al controlador.
  segunda <- contar(con)
  expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
  expect_identical(segunda$llegaron, 0L)
  mensaje <- conditionMessage(segunda$valor)
  expect_true(grepl("llamada anterior", mensaje, fixed = TRUE))
  # Dice que se perdio con la sesion y que hay que hacer.
  expect_true(grepl(paste0("la transacci", .CP_O, "n abierta"), mensaje,
                    fixed = TRUE))
  expect_true(grepl("las tablas temporales", mensaje, fixed = TRUE))
  expect_true(grepl("`DBI::dbDisconnect()` y abrir otra", mensaje, fixed = TRUE))
  expect_true(isTRUE(segunda$valor$datos$recordada))
  expect_false(isTRUE(primera$datos$recordada))
  # Un alias es la misma conexion.
  alias <- con
  expect_s3_class(contar(alias)$valor, "lupa_error_conexion_dbi")
  # La otra conexion al mismo archivo, sana, no se toma por perdida.
  expect_s3_class(contar(otra)$valor, "perfil_dbi")
  # Un `dbConnect()` nuevo sobre la misma variable, tampoco.
  vieja <- con
  con <- .cpd_abrir_archivo(archivo)
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)),
          add = TRUE)
  expect_identical(DBI::dbGetInfo(con), DBI::dbGetInfo(vieja))
  expect_s3_class(contar(con)$valor, "perfil_dbi")
  # Cerrada, sigue sin usarse y sin que nada llegue al controlador.
  suppressWarnings(DBI::dbDisconnect(vieja))
  cerrada <- contar(vieja)
  expect_s3_class(cerrada$valor, "lupa_error_conexion_dbi")
  expect_identical(cerrada$llegaron, 0L)
})

test_that("la regla vale con cualquier controlador, tambien con los que reabren la sesion", {
  skip_if_not_installed("RSQLite")
  cortes <- list(
    RPostgres = "Failed to prepare query : FATAL:  terminating connection due to administrator command\n",
    RMariaDB = "Lost connection to MySQL server during query [2013]",
    odbc = "nanodbc/nanodbc.cpp:1591: 00000\n[FreeTDS][SQL Server]Unknown error "
  )
  # RSQLite y duckdb no entran con un corte fabricado: ahi el texto se confirma
  # con `dbIsValid()`, y con la conexion abierta no es un corte (mas abajo).
  for (controlador in names(cortes)) {
    local({
      local_mocked_bindings(.controlador_dbi = function(conexion) controlador)
      con <- .cp_abrir_sqlite()
      on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
      llamar <- function(gancho = NULL) {
        est <- .cp_estado()
        .cp_imitar_dbi(est, gancho)
        valor <- tryCatch(
          suppressWarnings(suppressMessages(
            perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
          )),
          error = function(e) e
        )
        list(valor = valor, llegaron = est$llegaron)
      }
      primera <- llamar(function(est, operacion, conexion, sql) {
        if (est$k == 7L) est$lanzar <- simpleError(cortes[[controlador]])
      })
      expect_s3_class(primera$valor, "lupa_error_conexion_dbi")
      # La conexion sigue abierta y serviria: es la que el controlador
      # reabriria. No se la vuelve a usar.
      expect_true(DBI::dbIsValid(con), info = controlador)
      segunda <- llamar()
      expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
      expect_identical(segunda$llegaron, 0L, info = controlador)
      expect_true(isTRUE(segunda$valor$datos$recordada), info = controlador)
      # Tampoco por otra funcion ni por una coleccion.
      coleccion_despues <- tryCatch(coleccion(con, "lupa_t"), error = function(e) e)
      expect_s3_class(coleccion_despues, "lupa_error_conexion_dbi")
    })
  }
})

# Las diez funciones publicas que reciben una conexion o una coleccion.
.cpd_puertas <- function(modelo_cp) {
  list(
    perfilar_dbi = function(con, col, perfil) {
      perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
    },
    plan_perfilado_dbi = function(con, col, perfil) plan_perfilado_dbi(con, "lupa_t"),
    medir_dbi = function(con, col, perfil) {
      medir_dbi(modelo_cp, con, fecha = .CP_FECHA, id_medicion = "fijo")
    },
    plan_medicion_dbi = function(con, col, perfil) plan_medicion_dbi(modelo_cp, con),
    conformidad_dbi = function(con, col, perfil) conformidad_dbi(con, modelo_cp),
    coleccion = function(con, col, perfil) coleccion(con, c("lupa_t", "lupa_u")),
    perfilar_coleccion = function(con, col, perfil) {
      perfilar_coleccion(col, fecha = .CP_FECHA)
    },
    estimar_costo_coleccion = function(con, col, perfil) estimar_costo_coleccion(col),
    relaciones_coleccion = function(con, col, perfil) {
      relaciones_coleccion(
        col, pares = data.frame(tabla_1 = "lupa_t", tabla_2 = "lupa_u")
      )
    },
    detectar_tablas_codigos = function(con, col, perfil) {
      detectar_tablas_codigos(col, perfil)
    }
  )
}

test_that("toda funcion que recibe una conexion rechaza la cortada sin llegar al controlador", {
  skip_if_not_installed("RSQLite")
  modelo_cp <- .cp_modelo()
  funciones <- .cpd_puertas(modelo_cp)
  con <- .cp_abrir_sqlite()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  # La coleccion y su perfil se arman con la conexion sana: las funciones que
  # reciben la coleccion llegan a la conexion por ella.
  col <- coleccion(con, c("lupa_t", "lupa_u"))
  perfil <- suppressWarnings(suppressMessages(
    perfilar_coleccion(col, fecha = .CP_FECHA)
  ))
  llamar <- function(codigo, gancho = NULL) {
    est <- .cp_estado()
    .cp_imitar_dbi(est, gancho)
    valor <- tryCatch(
      suppressWarnings(suppressMessages(codigo(con, col, perfil))),
      error = function(e) e
    )
    list(valor = valor, llegaron = est$llegaron)
  }
  # El control: con la conexion sana cada una contesta y llega al controlador.
  for (nombre in names(funciones)) {
    sana <- llamar(funciones[[nombre]])
    expect_false(inherits(sana$valor, "condition"), info = nombre)
    expect_gt(sana$llegaron, 0L, label = paste("llamadas de", nombre))
  }
  # El corte fabricado es el que escribe el cliente de PostgreSQL; las demas
  # llamadas van con el controlador de verdad.
  corte <- local({
    local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
    llamar(funciones$perfilar_dbi, function(est, operacion, conexion, sql) {
      if (est$k == 7L) {
        est$lanzar <- simpleError(
          "Failed to prepare query : server closed the connection unexpectedly\n"
        )
      }
    })
  })
  expect_s3_class(corte$valor, "lupa_error_conexion_dbi")
  expect_false(isTRUE(corte$valor$datos$recordada))
  for (nombre in names(funciones)) {
    despues <- llamar(funciones[[nombre]])
    expect_s3_class(despues$valor, "lupa_error_conexion_dbi")
    expect_identical(despues$llegaron, 0L, info = nombre)
    expect_true(isTRUE(despues$valor$datos$recordada), info = nombre)
  }
})

test_that("con RPostgres decide la gravedad: un ERROR con el texto de un corte no corta ni marca", {
  skip_if_not_installed("RSQLite")
  # Lo que mide: un error que el servidor informo -una funcion que levanta el
  # texto de un corte, una tabla foranea cuyo remoto se cayo- deja la sesion
  # sana. El mismo texto sin gravedad lo escribio el cliente, y es un corte.
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  textos <- c(
    "server closed the connection unexpectedly",
    "no connection to the server",
    "could not send data to server: Broken pipe",
    "could not receive data from server: Connection reset by peer",
    "SSL connection has been closed unexpectedly",
    "connection to server was lost",
    "Lost connection to database"
  )
  for (texto in textos) {
    local({
      con <- .cp_abrir_sqlite()
      on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
      llamar <- function(mensaje = NULL) {
        est <- .cp_estado()
        .cp_imitar_dbi(est, function(est, operacion, conexion, sql) {
          if (!is.null(mensaje) && est$k == 7L) est$lanzar <- simpleError(mensaje)
        })
        valor <- tryCatch(
          suppressWarnings(suppressMessages(
            perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
          )),
          error = function(e) e
        )
        list(valor = valor, llegaron = est$llegaron)
      }
      informado <- llamar(paste0(
        "Failed to fetch row : ERROR:  ", texto,
        "\nCONTEXT:  PL/pgSQL function lupa_f(text) line 1 at RAISE\n"
      ))
      expect_false(inherits(informado$valor, "lupa_error_conexion_dbi"), info = texto)
      expect_null(.marca_conexion_cortada_dbi(con), info = texto)
      # La llamada siguiente sobre la misma conexion anda.
      siguiente <- llamar()
      expect_s3_class(siguiente$valor, "perfil_dbi")
      expect_gt(siguiente$llegaron, 0L)
      # Sin gravedad lo escribio el cliente: corta y marca.
      del_cliente <- llamar(paste0("Failed to fetch row : ", texto, "\n"))
      expect_s3_class(del_cliente$valor, "lupa_error_conexion_dbi")
      expect_false(is.null(.marca_conexion_cortada_dbi(con)), info = texto)
    })
  }
})

test_that("con RSQLite y duckdb el texto de un corte se confirma: con la conexion abierta no corta ni marca", {
  skip_if_not_installed("RSQLite")
  fecha <- .CP_FECHA
  # SQLite: el `RAISE(ABORT, ...)` de un disparador. Las sentencias que manda
  # el paquete solo leen y no lo disparan: se manda por el punto unico, dentro
  # de una corrida, que es por donde pasa toda sentencia.
  con <- .cp_abrir_sqlite()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_disparo (x INTEGER)")
  DBI::dbExecute(con, paste(
    "CREATE TRIGGER lupa_tr BEFORE INSERT ON lupa_disparo BEGIN",
    "SELECT RAISE(ABORT, 'Invalid or closed connection'); END"
  ))
  r <- tryCatch(
    .con_corrida_dbi(con, "probar", .dbi(
      "dbExecute", con,
      DBI::dbExecute(con, "INSERT INTO lupa_disparo VALUES (1)")
    )),
    error = function(e) e
  )
  expect_s3_class(r, "error")
  expect_false(inherits(r, "lupa_error_conexion_dbi"))
  expect_identical(conditionMessage(r), "Invalid or closed connection")
  expect_null(.marca_conexion_cortada_dbi(con))
  expect_s3_class(suppressWarnings(suppressMessages(
    perfilar_dbi(con, "lupa_t", fecha = fecha)
  )), "perfil_dbi")

  # DuckDB, por la funcion publica: una vista que falla con ese texto.
  skip_if_not_installed("duckdb")
  pato <- .cp_abrir_duckdb()
  on.exit(suppressWarnings(DBI::dbDisconnect(pato)), add = TRUE)
  DBI::dbExecute(pato, "CREATE VIEW lupa_v_error AS SELECT error('Invalid connection') AS x")
  crudo <- tryCatch(
    DBI::dbGetQuery(pato, "SELECT * FROM lupa_v_error"), error = function(e) e
  )
  expect_s3_class(crudo, "error")
  expect_identical(.reconocer_error_dbi("duckdb", crudo), "conexion")
  expect_identical(
    .reconocer_en_conexion_dbi("duckdb", pato, crudo), "rechazada"
  )
  perfil <- tryCatch(
    suppressWarnings(suppressMessages(
      perfilar_dbi(pato, "lupa_v_error", fecha = fecha)
    )),
    error = function(e) e
  )
  expect_false(inherits(perfil, "lupa_error_conexion_dbi"))
  expect_null(.marca_conexion_cortada_dbi(pato))
  expect_s3_class(suppressWarnings(suppressMessages(
    perfilar_dbi(pato, "lupa_t", fecha = fecha)
  )), "perfil_dbi")
})

test_that("una transaccion que ya estaba abortada se dice como tal, y la conexion no queda marcada", {
  skip_if_not_installed("RSQLite")
  # En PostgreSQL, con la transaccion de quien llama abortada, el motor rechaza
  # toda sentencia. Cada puerta de entrada lo contaba a su manera -la tabla no
  # existe, o una coleccion con todas sus tablas fallidas-.
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  abortada <- paste0(
    "Failed to prepare query : ERROR:  current transaction is aborted, ",
    "commands ignored until end of transaction block\n"
  )
  rechaza_todo <- function(est, operacion, conexion, sql) {
    if (operacion %in% c("dbGetQuery", "dbSendQuery", "dbExecute",
                         "dbExistsTable", "dbListFields")) {
      est$lanzar <- simpleError(abortada)
    }
  }
  modelo_cp <- .cp_modelo()
  funciones <- .cpd_puertas(modelo_cp)
  con <- .cp_abrir_sqlite()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  col <- coleccion(con, c("lupa_t", "lupa_u"))
  perfil <- suppressWarnings(suppressMessages(
    perfilar_coleccion(col, fecha = .CP_FECHA)
  ))
  llamar <- function(codigo, gancho = NULL) {
    est <- .cp_estado()
    .cp_imitar_dbi(est, gancho)
    valor <- tryCatch(
      suppressWarnings(suppressMessages(codigo(con, col, perfil))),
      error = function(e) e
    )
    list(valor = valor, tras = est$tras, llegaron = est$llegaron)
  }
  for (nombre in names(funciones)) {
    r <- llamar(funciones[[nombre]], rechaza_todo)
    if (identical(nombre, "coleccion")) {
      # Con los nombres dados no le pregunta nada al motor.
      expect_s3_class(r$valor, "coleccion_lupa")
    } else {
      expect_s3_class(r$valor, "lupa_error_transaccion_dbi")
      expect_true(isTRUE(r$valor$datos$abortada), info = nombre)
      mensaje <- conditionMessage(r$valor)
      expect_true(grepl("abortada", mensaje, fixed = TRUE), info = nombre)
      expect_true(grepl("`DBI::dbRollback()`", mensaje, fixed = TRUE), info = nombre)
      expect_true(grepl("current transaction is aborted", mensaje, fixed = TRUE),
                  info = nombre)
      # Despues de la primera respuesta del motor no se manda nada mas.
      expect_identical(r$tras, 0L, info = nombre)
    }
    # No es un corte: la conexion no queda marcada.
    expect_null(.marca_conexion_cortada_dbi(con), info = nombre)
  }
  # Deshecha la transaccion, la misma conexion sirve.
  expect_s3_class(llamar(funciones$perfilar_dbi)$valor, "perfil_dbi")
  # El control: si en la corrida ya contesto alguna sentencia, la transaccion
  # no estaba abortada al entrar, y no se dice eso.
  despues <- llamar(funciones$perfilar_dbi, function(est, operacion, conexion, sql) {
    if (est$k == 9L) est$lanzar <- simpleError(abortada)
  })
  expect_false(inherits(despues$valor, "lupa_error_transaccion_dbi"))
})

test_that("la sesion que el controlador reabrio no se usa en la llamada siguiente", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  codigo <- function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
  con <- .cpd_abrir_con_sesion()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  llamar <- function(gancho = NULL) {
    est <- .cp_estado()
    .cp_imitar_dbi(est, gancho)
    valor <- tryCatch(
      suppressWarnings(suppressMessages(codigo(con))), error = function(e) e
    )
    list(valor = valor, llegaron = est$llegaron)
  }
  # La primera llamada termina en otra sesion: corta al salir.
  primera <- llamar(.cpd_gancho_reapertura(5L))
  expect_s3_class(primera$valor, "lupa_error_conexion_dbi")
  # La sesion nueva contesta bien y el identificador ya no cambia: sin la
  # regla, la segunda llamada devolvia un perfil.
  segunda <- llamar()
  expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
  expect_identical(segunda$llegaron, 0L)
})

# Avisa cuando el recolector se lleva la conexion.
.cp_avisar_recoleccion <- function(con, recolectada) {
  reg.finalizer(con@ptr, local({
    marca <- recolectada
    function(p) marca$si <- TRUE
  }))
  invisible(NULL)
}

test_that("la marca no retiene la conexion: suelta sin cerrar, se recolecta", {
  skip_if_not_installed("RSQLite")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  recolectada <- new.env(parent = emptyenv())
  recolectada$si <- FALSE
  usar <- function() {
    con <- .cp_abrir_sqlite()
    .cp_avisar_recoleccion(con, recolectada)
    est <- .cp_estado()
    .cp_imitar_dbi(est, function(est, operacion, conexion, sql) {
      if (est$k == 7L) {
        est$lanzar <- simpleError(
          "Failed to prepare query : server closed the connection unexpectedly\n"
        )
      }
    })
    clases <- character()
    for (i in 1:2) {
      valor <- tryCatch(
        suppressWarnings(suppressMessages(
          perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
        )),
        error = function(e) e
      )
      clases <- c(clases, class(valor)[[1L]])
    }
    clases
  }
  expect_identical(usar(), rep("lupa_error_conexion_dbi", 2L))
  .cp_recolectar()
  expect_true(recolectada$si)
})

test_that("una conexion sana que se suelta sin cerrar se recolecta", {
  skip_if_not_installed("RSQLite")
  # Sin ninguna envoltura: el manejador que atrapa el error del controlador
  # tiene que ser el del paquete. SQLite rechaza una forma candidata en cada
  # perfil, y los controladores dejan retenido el manejador del ultimo error
  # suyo que se atrapo: armado en el marco que tiene la conexion, la conexion
  # no se recolectaba despues de una llamada que habia terminado bien.
  recolectada <- new.env(parent = emptyenv())
  recolectada$si <- FALSE
  usar <- function() {
    con <- .cp_abrir_sqlite()
    .cp_avisar_recoleccion(con, recolectada)
    perfil <- suppressWarnings(suppressMessages(
      perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
    ))
    list(
      clase = class(perfil)[[1L]],
      rechazadas = nrow(perfil$resumen_tabla$meta$consultas$sondas_fallidas)
    )
  }
  visto <- usar()
  expect_identical(visto$clase, "perfil_dbi")
  # El caso solo prueba algo si el controlador rechazo una sentencia.
  expect_gte(visto$rechazadas, 1L)
  .cp_recolectar()
  expect_true(recolectada$si)
})

test_that("sin puntero no hay como reconocer la conexion, y no se recuerda nada", {
  skip_if_not_installed("RSQLite")
  # El limite declarado de RMySQL: su conexion son dos enteros, que una
  # conexion nueva puede repetir. Ahi la llamada siguiente no se rechaza de
  # memoria; con RMySQL de verdad vuelve a cortar sola, porque no reabre.
  local_mocked_bindings(
    .controlador_dbi = function(conexion) "RMySQL",
    .puntero_conexion_dbi = function(conexion) NULL
  )
  con <- .cp_abrir_sqlite()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  est <- .cp_estado()
  .cp_imitar_dbi(est, function(est, operacion, conexion, sql) {
    if (est$k == 7L) {
      est$lanzar <- simpleError("could not run statement: MySQL server has gone away")
    }
  })
  primera <- tryCatch(
    suppressWarnings(suppressMessages(perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA))),
    error = function(e) e
  )
  expect_s3_class(primera, "lupa_error_conexion_dbi")
  expect_null(.marca_conexion_cortada_dbi(con))
})

test_that("el error crudo de FreeTDS con un resultado abierto no es la conexion perdida", {
  skip_if_not_installed("RSQLite")
  # Mandar una sentencia con otro resultado abierto cancela ese resultado y
  # falla con `Unknown error`, sobre una conexion sana: la vigilancia de la
  # transaccion en SQL Server lo hace en cada consulta. Sin resultado abierto,
  # o al leer el resultado, el mismo error si es la conexion.
  local_mocked_bindings(.controlador_dbi = function(conexion) "odbc")
  con <- .cp_abrir_sqlite()
  on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)
  fallar <- function(operacion) {
    tryCatch(.dbi(operacion, con, stop(.CPD_ERROR_FREETDS)), error = function(e) e)
  }
  con_abierto <- .con_corrida_dbi(con, "perfilar una tabla", {
    resultado <- .dbi("dbSendQuery", con, DBI::dbSendQuery(con, "SELECT 1 AS uno"))
    tolerados <- lapply(c("dbGetQuery", "dbExecute", "dbSendQuery"), fallar)
    sigue <- .dbi("dbFetch", con, DBI::dbFetch(resultado))
    .dbi("dbClearResult", con, DBI::dbClearResult(resultado))
    list(tolerados = tolerados, sigue = sigue)
  })
  for (tolerado in con_abierto$tolerados) {
    expect_s3_class(tolerado, "error")
    expect_false(.cpd_es_corte(tolerado))
    expect_identical(.reconocido_dbi(tolerado), "rechazada")
  }
  expect_identical(con_abierto$sigue$uno, 1L)
  # Ya cerrado el resultado, el mismo error corta.
  sin_abierto <- tryCatch(.con_corrida_dbi(con, "perfilar una tabla", {
    resultado <- .dbi("dbSendQuery", con, DBI::dbSendQuery(con, "SELECT 1 AS uno"))
    .dbi("dbClearResult", con, DBI::dbClearResult(resultado))
    fallar("dbGetQuery")
    "siguio"
  }), error = function(e) e)
  expect_s3_class(sin_abierto, "lupa_error_conexion_dbi")
  # Y en la lectura del resultado abierto, tambien. En otra conexion: la de
  # arriba ya quedo cortada.
  suppressWarnings(DBI::dbDisconnect(con))
  con <- .cp_abrir_sqlite()
  al_leer <- tryCatch(.con_corrida_dbi(con, "perfilar una tabla", {
    resultado <- .dbi("dbSendQuery", con, DBI::dbSendQuery(con, "SELECT 1 AS uno"))
    on.exit(suppressWarnings(DBI::dbClearResult(resultado)), add = TRUE)
    fallar("dbFetch")
    "siguio"
  }), error = function(e) e)
  expect_s3_class(al_leer, "lupa_error_conexion_dbi")
})

test_that("dos punteros con la misma direccion no son la misma conexion", {
  # Dos punteros externos nulos guardan la misma direccion: `identical()` los
  # da por iguales. Es lo que pasa cuando el sistema ubica una conexion nueva
  # donde estaba una vieja.
  nulo <- function() unserialize(serialize(methods::new("externalptr"), NULL))
  a <- nulo()
  b <- nulo()
  expect_true(identical(a, b))
  expect_false(.mismo_puntero_dbi(a, b))
  expect_true(.mismo_puntero_dbi(a, a))
})

# ---- La reanudacion despues de un corte ----------------------------------

test_that("lo perfilado antes del corte queda guardado y la coleccion se retoma", {
  skip_if_not_installed("RSQLite")
  archivo <- tempfile(fileext = ".sqlite")
  carpeta <- tempfile("lupa_reanudar_")
  completa <- tempfile("lupa_completa_")
  on.exit(unlink(c(archivo, carpeta, completa), recursive = TRUE), add = TRUE)
  abrir <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), archivo)
    if (!DBI::dbExistsTable(con, "lupa_t")) {
      DBI::dbWriteTable(con, "lupa_t", .cp_tabla())
      DBI::dbWriteTable(con, "lupa_u", data.frame(id = 1:9, nota = letters[1:9]))
    }
    con
  }
  codigo <- function(directorio, reanudar = FALSE) {
    function(con) {
      perfilar_coleccion(
        coleccion(con, c("lupa_t", "lupa_u")), fecha = .CP_FECHA,
        directorio_reanudacion = directorio, reanudar = reanudar
      )
    }
  }
  entera <- .cp_correr(abrir, codigo(completa))
  expect_s3_class(entera$valor, "perfil_coleccion")
  primera_de_u <- min(which(grepl("lupa_u", entera$sql, fixed = TRUE) &
                              grepl("COUNT", entera$sql, fixed = TRUE)))
  cortada <- .cp_correr(
    abrir, codigo(carpeta), .cp_gancho_cierre(primera_de_u)
  )
  expect_s3_class(cortada$valor, "lupa_error_conexion_dbi")
  expect_true(grepl(
    paste0("directorio de reanudaci", .CP_O, "n"),
    conditionMessage(cortada$valor), fixed = TRUE
  ))
  retomada <- .cp_correr(abrir, codigo(carpeta, reanudar = TRUE))
  expect_s3_class(retomada$valor, "perfil_coleccion")
  expect_identical(
    retomada$valor$resumen_coleccion[c("tabla", "n_filas", "n_columnas")],
    entera$valor$resumen_coleccion[c("tabla", "n_filas", "n_columnas")]
  )
  # La primera tabla no se vuelve a leer.
  expect_lt(retomada$llegaron, entera$llegaron)
  # Sin directorio de reanudacion el mensaje no promete retomar nada.
  sin_directorio <- .cp_correr(
    abrir, codigo(NULL), .cp_gancho_cierre(primera_de_u)
  )
  expect_s3_class(sin_directorio$valor, "lupa_error_conexion_dbi")
  expect_false(grepl("retomarse", conditionMessage(sin_directorio$valor),
                     fixed = TRUE))
})

if (methods::isClass("LupaConexionConSesion")) {
  suppressWarnings(try(
    methods::removeMethod(DBI::dbGetInfo, "LupaConexionConSesion"),
    silent = TRUE
  ))
  suppressWarnings(try(
    methods::removeClass("LupaConexionConSesion"), silent = TRUE
  ))
}
