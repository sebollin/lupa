# La conexion perdida, con un servidor de verdad: PostgreSQL -que reabre la
# sesion sola- y SQL Server con odbc y FreeTDS -donde la sentencia siguiente a
# un corte hace caer a R-. La sesion se termina desde otra conexion justo antes
# de una llamada del paquete al controlador. Se saltean si el motor no esta
# disponible en el entorno local.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.cpm_conectar_pg <- function(host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
                             ...) {
  skip_if_not_installed("RPostgres")
  puerto <- Sys.getenv("LUPA_PG_PORT", "55432")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = host, port = as.integer(puerto),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64", ...
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en puerto ", puerto, "."))
  con
}

.cpm_conectar_mssql <- function() {
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin clave de SQL Server.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

.cpm_nada <- function(e) NULL

# Termina la sesion `id` desde `adm` justo antes de la llamada `objetivo`.
.cpm_gancho_matar <- function(adm, id, objetivo, orden) {
  function(est, operacion, conexion, sql) {
    if (!est$cortada && est$k == objetivo) {
      est$cortada <- TRUE
      suppressWarnings(try(DBI::dbExecute(adm, sprintf(orden, id)), silent = TRUE))
      Sys.sleep(0.3)
    }
  }
}

.cpm_correr <- function(con, codigo, gancho = NULL) {
  est <- .cp_estado()
  .cp_imitar_dbi(est, gancho)
  valor <- tryCatch(
    suppressWarnings(suppressMessages(codigo(con))), error = function(e) e
  )
  list(valor = valor, k = est$k, tras = est$tras, ops = est$ops, sql = est$sql,
       llegaron = est$llegaron)
}

.cpm_preparar_pg <- function(adm) {
  for (orden in c(
    "DROP SCHEMA IF EXISTS lupa_cp CASCADE",
    "DROP TABLE IF EXISTS public.lupa_cp_t",
    "DROP TABLE IF EXISTS public.lupa_cp_u",
    "CREATE SCHEMA lupa_cp",
    # Las mismas tablas en dos esquemas, con datos distintos: la sesion de la
    # prueba ve las de `lupa_cp`; una sesion reabierta veria las de `public`.
    "CREATE TABLE lupa_cp.lupa_cp_t AS SELECT g AS id, (1000 + g)::float8 AS x FROM generate_series(1, 150) g",
    "CREATE TABLE lupa_cp.lupa_cp_u AS SELECT g AS id, 'a' || g AS s FROM generate_series(1, 30) g",
    "CREATE TABLE public.lupa_cp_t AS SELECT g AS id, g::float8 AS x FROM generate_series(1, 40) g",
    "CREATE TABLE public.lupa_cp_u AS SELECT g AS id, 'p' || g AS s FROM generate_series(1, 9) g"
  )) {
    suppressMessages(DBI::dbExecute(adm, orden))
  }
  invisible(NULL)
}

.cpm_limpiar_pg <- function(adm) {
  for (orden in c(
    "DROP SCHEMA IF EXISTS lupa_cp CASCADE",
    "DROP TABLE IF EXISTS public.lupa_cp_t",
    "DROP TABLE IF EXISTS public.lupa_cp_u"
  )) {
    suppressMessages(try(DBI::dbExecute(adm, orden), silent = TRUE))
  }
}

.cpm_sesion_pg <- function() {
  con <- .cpm_conectar_pg()
  DBI::dbExecute(con, "SET search_path TO lupa_cp, public")
  con
}

test_that("PostgreSQL: la sesion terminada corta, y nada se lee en la que el controlador reabre", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  llamadas <- list(
    perfil = function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA),
    muestra = function(con) {
      perfilar_dbi(con, "lupa_cp_t", muestra = 20, fecha = .CP_FECHA)
    },
    coleccion = function(con) {
      perfilar_coleccion(
        coleccion(con, c("lupa_cp_t", "lupa_cp_u")), fecha = .CP_FECHA
      )
    }
  )
  for (nombre in names(llamadas)) {
    con <- .cpm_sesion_pg()
    referencia <- .cpm_correr(con, llamadas[[nombre]])
    DBI::dbDisconnect(con)
    expect_false(inherits(referencia$valor, "condition"), info = nombre)
    posiciones <- unique(c(
      2L, referencia$k %/% 3L, referencia$k %/% 2L, referencia$k - 1L,
      which(referencia$ops == "dbClearResult")[1L]
    ))
    for (k in posiciones[!is.na(posiciones)]) {
      con <- .cpm_sesion_pg()
      id <- as.numeric(DBI::dbGetInfo(con)$pid)
      r <- .cpm_correr(con, llamadas[[nombre]], .cpm_gancho_matar(
        adm, id, k, "SELECT pg_terminate_backend(%.0f)"
      ))
      DBI::dbDisconnect(con)
      expect_s3_class(r$valor, "lupa_error_conexion_dbi")
      expect_identical(r$tras, 0L, info = paste(nombre, k))
    }
  }
})

test_that("PostgreSQL: la conexion cortada no se vuelve a usar, y el controlador no la reabre", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  # La sesion se reconoce en el servidor por su nombre de aplicacion, que la
  # reapertura del controlador conserva.
  nombre <- paste0("lupa_cp_", Sys.getpid())
  sesiones <- function() {
    as.numeric(DBI::dbGetQuery(adm, paste0(
      "SELECT pid FROM pg_stat_activity WHERE application_name = '", nombre, "'"
    ))$pid)
  }
  codigo <- function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  for (en_transaccion in c(FALSE, TRUE)) {
    con <- .cpm_conectar_pg(application_name = nombre)
    DBI::dbExecute(con, "SET search_path TO lupa_cp, public")
    if (en_transaccion) DBI::dbBegin(con)
    expect_identical(length(sesiones()), 1L)
    for (pid in sesiones()) {
      DBI::dbGetQuery(adm, sprintf("SELECT pg_terminate_backend(%.0f)", pid))
    }
    Sys.sleep(0.3)
    primera <- .cpm_correr(con, codigo)
    expect_s3_class(primera$valor, "lupa_error_conexion_dbi")
    # Sin la regla, la segunda llamada devolvia un perfil: el de la tabla del
    # mismo nombre en `public`, leido en la sesion que el controlador reabrio
    # sin el `search_path` ni la transaccion de quien llama.
    segunda <- .cpm_correr(con, codigo)
    expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
    expect_identical(segunda$llegaron, 0L, info = paste("en transaccion:", en_transaccion))
    expect_true(isTRUE(segunda$valor$datos$recordada))
    expect_true(grepl("llamada anterior", conditionMessage(segunda$valor),
                      fixed = TRUE))
    # Rechazar no le pregunta nada al controlador: no hay sesion reabierta.
    expect_identical(length(sesiones()), 0L, info = paste("en transaccion:", en_transaccion))
    suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE))
    rm(con, primera, segunda)

    # Lo mismo sin la envoltura que cuenta las llamadas, para que el manejador
    # que atrapa el error del controlador sea el del paquete. Si quien llama
    # usa la conexion por su cuenta el controlador la reabre; soltada sin
    # cerrar se recolecta y la sesion reabierta no queda viva.
    llamar_dos_veces <- function(con) {
      vapply(1:2, function(i) {
        class(tryCatch(
          suppressWarnings(suppressMessages(codigo(con))), error = function(e) e
        ))[[1L]]
      }, character(1L))
    }
    usar <- function(con) {
      tryCatch(DBI::dbGetQuery(con, "SELECT 1"), error = .cpm_nada)
      tryCatch(DBI::dbGetQuery(con, "SELECT 1"), error = .cpm_nada)
      length(sesiones())
    }
    con <- .cpm_conectar_pg(application_name = nombre)
    if (en_transaccion) DBI::dbBegin(con)
    for (pid in sesiones()) {
      DBI::dbGetQuery(adm, sprintf("SELECT pg_terminate_backend(%.0f)", pid))
    }
    Sys.sleep(0.3)
    expect_identical(llamar_dos_veces(con), rep("lupa_error_conexion_dbi", 2L))
    expect_identical(length(sesiones()), 0L, info = paste("en transaccion:", en_transaccion))
    expect_identical(usar(con), 1L, info = paste("en transaccion:", en_transaccion))
    rm(con)
    .cp_recolectar()
    Sys.sleep(0.5)
    expect_identical(length(sesiones()), 0L, info = paste("en transaccion:", en_transaccion))
    # Una conexion nueva sirve.
    nueva <- .cpm_sesion_pg()
    expect_s3_class(.cpm_correr(nueva, codigo)$valor, "perfil_dbi")
    DBI::dbDisconnect(nueva)
  }
})

.CPM_TEXTOS_DE_CORTE <- c(
  "server closed the connection unexpectedly",
  "no connection to the server",
  "could not send data to server: Broken pipe",
  "could not receive data from server: Connection reset by peer",
  "SSL connection has been closed unexpectedly",
  "connection to server was lost",
  "Lost connection to database"
)

.cpm_callar <- function(codigo) {
  tryCatch(suppressWarnings(suppressMessages(codigo)), error = function(e) e)
}

test_that("PostgreSQL: un error que el servidor informa con el texto de un corte no corta ni marca", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  # Una funcion del usuario levanta cada texto que, escrito por el cliente,
  # es un corte. El servidor lo informa como ERROR: la sesion sigue.
  DBI::dbExecute(adm, paste(
    "CREATE FUNCTION lupa_cp.lupa_cp_f_texto(t text) RETURNS integer",
    "LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION '%', t; END $$"
  ))
  for (i in seq_along(.CPM_TEXTOS_DE_CORTE)) {
    DBI::dbExecute(adm, sprintf(
      "CREATE VIEW lupa_cp.lupa_cp_v_%d AS SELECT lupa_cp.lupa_cp_f_texto('%s') AS x",
      i, .CPM_TEXTOS_DE_CORTE[[i]]
    ))
  }
  con <- .cpm_sesion_pg()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)
  pid <- as.numeric(DBI::dbGetInfo(con)$pid)
  for (i in seq_along(.CPM_TEXTOS_DE_CORTE)) {
    texto <- .CPM_TEXTOS_DE_CORTE[[i]]
    vista <- sprintf("lupa_cp_v_%d", i)
    crudo <- tryCatch(
      DBI::dbGetQuery(con, paste("SELECT * FROM", vista)), error = function(e) e
    )
    expect_true(grepl(paste0("ERROR:  ", texto), conditionMessage(crudo),
                      fixed = TRUE), info = texto)
    r <- .cpm_callar(perfilar_dbi(con, vista, fecha = .CP_FECHA))
    expect_s3_class(r, "lupa_error_dbi")
    expect_false(inherits(r, "lupa_error_conexion_dbi"), info = texto)
    expect_null(.marca_conexion_cortada_dbi(con), info = texto)
    # La llamada siguiente sobre la misma conexion anda, en la misma sesion.
    expect_s3_class(
      .cpm_callar(perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)), "perfil_dbi"
    )
    expect_identical(as.numeric(DBI::dbGetInfo(con)$pid), pid, info = texto)
  }
})

test_that("PostgreSQL 9.3: una tabla foranea cuyo remoto se corta en medio de la consulta no corta ni marca", {
  skip_if_not_installed("RPostgres")
  abrir <- function() {
    tryCatch(DBI::dbConnect(
      RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
      port = 55493L, user = Sys.getenv("LUPA_PG_USER", "postgres"),
      password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
      dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa"), bigint = "integer64"
    ), error = function(e) NULL)
  }
  adm <- abrir()
  skip_if(is.null(adm), "PostgreSQL 9.3 no disponible en puerto 55493.")
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  extension <- tryCatch(
    suppressMessages(DBI::dbExecute(adm, "CREATE EXTENSION IF NOT EXISTS postgres_fdw")),
    error = function(e) e
  )
  skip_if(inherits(extension, "condition"), "postgres_fdw no disponible.")
  quitar_servidor <- function() {
    suppressMessages(try(
      DBI::dbExecute(adm, "DROP SERVER IF EXISTS lupa_cp_srv CASCADE"), silent = TRUE
    ))
  }
  quitar_servidor()
  .cpm_preparar_pg(adm)
  on.exit({
    quitar_servidor()
    .cpm_limpiar_pg(adm)
  }, add = TRUE, after = FALSE)
  # El remoto es este mismo servidor, y la vista remota termina su propia
  # sesion al llegar a una fila: el remoto se corta en medio de la consulta, y
  # la sesion que se corta es la que abrio la tabla foranea. La sesion local
  # recibe el texto del cliente de PostgreSQL como un ERROR suyo, y sigue.
  dato <- function(sql) as.character(DBI::dbGetQuery(adm, sql)[[1L]][[1L]])
  DBI::dbExecute(adm, paste(
    "CREATE VIEW lupa_cp.lupa_cp_remota AS SELECT id, CASE WHEN id = 100",
    "THEN CAST(pg_terminate_backend(pg_backend_pid()) AS integer) ELSE id END AS x",
    "FROM lupa_cp.lupa_cp_t"
  ))
  DBI::dbExecute(adm, sprintf(
    paste("CREATE SERVER lupa_cp_srv FOREIGN DATA WRAPPER postgres_fdw",
          "OPTIONS (dbname '%s', port '%s', application_name 'lupa_cp_fdw')"),
    dato("SELECT current_database()"), dato("SHOW port")
  ))
  DBI::dbExecute(adm, sprintf(
    "CREATE USER MAPPING FOR CURRENT_USER SERVER lupa_cp_srv OPTIONS (user '%s', password '%s')",
    dato("SELECT current_user"), Sys.getenv("LUPA_PG_PASSWORD", "lupa")
  ))
  DBI::dbExecute(adm, paste(
    "CREATE FOREIGN TABLE lupa_cp.lupa_cp_foranea (id integer, x integer)",
    "SERVER lupa_cp_srv OPTIONS (schema_name 'lupa_cp', table_name 'lupa_cp_remota')"
  ))
  con <- abrir()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)
  DBI::dbExecute(con, "SET search_path TO lupa_cp, public")
  pid <- as.numeric(DBI::dbGetInfo(con)$pid)
  crudo <- tryCatch(
    DBI::dbGetQuery(con, "SELECT COUNT(x) AS n FROM lupa_cp_foranea"),
    error = function(e) e
  )
  expect_s3_class(crudo, "error")
  expect_true(grepl("ERROR:  server closed the connection unexpectedly",
                    conditionMessage(crudo), fixed = TRUE))
  r <- .cpm_callar(perfilar_dbi(con, "lupa_cp_foranea", fecha = .CP_FECHA))
  expect_false(inherits(r, "lupa_error_conexion_dbi"))
  expect_null(.marca_conexion_cortada_dbi(con))
  expect_s3_class(
    .cpm_callar(perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)), "perfil_dbi"
  )
  expect_identical(as.numeric(DBI::dbGetInfo(con)$pid), pid)
})

test_that("PostgreSQL: una transaccion ya abortada se dice como tal en cada puerta de entrada", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  nucleo <- metricas_nucleo()
  reglas <- modelo(
    instanciar(especializar(nucleo$NoNulo, "NN_x"), "lupa_cp_t", "x")
  )
  puertas <- list(
    perfilar_dbi = function(con, col, perfil) {
      perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
    },
    plan_perfilado_dbi = function(con, col, perfil) plan_perfilado_dbi(con, "lupa_cp_t"),
    medir_dbi = function(con, col, perfil) {
      medir_dbi(reglas, con, fecha = .CP_FECHA, id_medicion = "fijo")
    },
    plan_medicion_dbi = function(con, col, perfil) plan_medicion_dbi(reglas, con),
    conformidad_dbi = function(con, col, perfil) conformidad_dbi(con, reglas),
    perfilar_coleccion = function(con, col, perfil) {
      perfilar_coleccion(col, fecha = .CP_FECHA)
    },
    estimar_costo_coleccion = function(con, col, perfil) estimar_costo_coleccion(col),
    relaciones_coleccion = function(con, col, perfil) {
      relaciones_coleccion(
        col, pares = data.frame(tabla_1 = "lupa_cp_t", tabla_2 = "lupa_cp_u")
      )
    },
    detectar_tablas_codigos = function(con, col, perfil) {
      detectar_tablas_codigos(col, perfil)
    }
  )
  con <- .cpm_sesion_pg()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)
  col <- coleccion(con, c("lupa_cp_t", "lupa_cp_u"))
  perfil <- .cpm_callar(perfilar_coleccion(col, fecha = .CP_FECHA))
  expect_s3_class(perfil, "perfil_coleccion")
  for (nombre in names(puertas)) {
    DBI::dbBegin(con)
    tryCatch(
      DBI::dbGetQuery(con, "SELECT * FROM lupa_cp_no_existe"), error = .cpm_nada
    )
    r <- .cpm_callar(puertas[[nombre]](con, col, perfil))
    # Antes: "la tabla no existe o no hay permiso", o una coleccion con todas
    # sus tablas fallidas.
    expect_s3_class(r, "lupa_error_transaccion_dbi")
    expect_true(grepl("`DBI::dbRollback()`", conditionMessage(r), fixed = TRUE),
                info = nombre)
    expect_true(isTRUE(r$datos$abortada), info = nombre)
    # No es un corte: la conexion no queda marcada, y deshecha la transaccion
    # la misma llamada anda.
    expect_null(.marca_conexion_cortada_dbi(con), info = nombre)
    DBI::dbRollback(con)
    despues <- .cpm_callar(puertas[[nombre]](con, col, perfil))
    expect_false(inherits(despues, "condition"), info = nombre)
  }
})

test_that("PostgreSQL: con una transaccion del usuario, la limpieza no manda nada tras el corte", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  codigo <- function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  con <- .cpm_sesion_pg()
  DBI::dbBegin(con)
  referencia <- .cpm_correr(con, codigo)
  DBI::dbRollback(con)
  DBI::dbDisconnect(con)
  # Dentro de la transaccion cada sentencia va entre un punto de guardado y su
  # cierre: son esas ordenes las que la limpieza mandaria.
  expect_true(any(grepl("^SAVEPOINT", referencia$sql)))
  con <- .cpm_sesion_pg()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)
  DBI::dbBegin(con)
  id <- as.numeric(DBI::dbGetInfo(con)$pid)
  r <- .cpm_correr(con, codigo, .cpm_gancho_matar(
    adm, id, referencia$k %/% 2L, "SELECT pg_terminate_backend(%.0f)"
  ))
  expect_s3_class(r$valor, "lupa_error_conexion_dbi")
  expect_identical(r$tras, 0L)
  # El motivo publicado es el del primer corte.
  expect_true(grepl("terminating connection", conditionMessage(r$valor),
                    fixed = TRUE))
})

test_that("PostgreSQL: una tabla sana no manda sentencias de identidad, y el lugar es del servidor", {
  adm <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .cpm_preparar_pg(adm)
  on.exit(.cpm_limpiar_pg(adm), add = TRUE, after = FALSE)
  con <- .cpm_sesion_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  r <- .cpm_correr(con, function(con) {
    perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  })
  expect_s3_class(r$valor, "perfil_dbi")
  expect_false(any(grepl("pg_backend_pid|pg_control_system|inet_server",
                         r$sql, perl = TRUE)))
  expect_null(r$valor$resumen_tabla$meta$consultas$sondas_fallidas_detalle)
  # El mismo servidor por dos caminos es el mismo lugar.
  por_nombre <- tryCatch(.cpm_conectar_pg("localhost"), error = function(e) NULL)
  skip_if(is.null(por_nombre), "No se pudo conectar por `localhost`.")
  on.exit(DBI::dbDisconnect(por_nombre), add = TRUE)
  expect_identical(.lugar_conexion_dbi(con), .lugar_conexion_dbi(por_nombre))
})

test_that("PostgreSQL: cancelacion, permiso y capacidad ausente se reconocen en el error", {
  con <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  falla <- function(sql) {
    tryCatch({
      .traer_consulta_dbi(con, sql)
      NULL
    }, error = function(e) e)
  }
  DBI::dbExecute(con, "SET statement_timeout = 50")
  cancelada <- falla("SELECT pg_sleep(2)")
  DBI::dbExecute(con, "SET statement_timeout = 0")
  expect_identical(.reconocer_falla_dbi(con, cancelada), "cancelada")
  expect_identical(.reconocer_falla_dbi(con, falla("SELECT STDEV(1.0)")),
                   "capacidad")
  # Un dato con la frase no es una cancelacion.
  expect_identical(
    .reconocer_falla_dbi(con, falla(
      "SELECT CAST('canceling statement due to x 57014' AS integer)"
    )),
    "rechazada"
  )
  DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_cp_permiso")
  DBI::dbExecute(con, "CREATE TABLE lupa_cp_permiso (id integer)")
  on.exit(suppressWarnings(try({
    DBI::dbExecute(con, "RESET ROLE")
    DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_cp_permiso")
    DBI::dbExecute(con, "DROP ROLE IF EXISTS lupa_cp_sin_permiso")
  }, silent = TRUE)), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "DROP ROLE IF EXISTS lupa_cp_sin_permiso")
  DBI::dbExecute(con, "CREATE ROLE lupa_cp_sin_permiso NOLOGIN")
  DBI::dbExecute(con, "SET ROLE lupa_cp_sin_permiso")
  expect_identical(
    .reconocer_falla_dbi(con, falla("SELECT id FROM lupa_cp_permiso")),
    "permiso"
  )
  negada <- tryCatch(
    perfilar_dbi(con, "lupa_cp_permiso"), error = function(e) e
  )
  expect_s3_class(negada, "lupa_error_tabla_dbi")
  expect_true(grepl("no tiene permiso para verla: el", conditionMessage(negada),
                    fixed = TRUE))
})

test_that("PostgreSQL: una columna `money` no deja consultas fallidas", {
  con <- .cpm_conectar_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_cp_dinero")
  on.exit(DBI::dbExecute(con, "DROP TABLE IF EXISTS lupa_cp_dinero"),
          add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE lupa_cp_dinero AS SELECT g AS id,",
    "(g * 10.5)::numeric::money AS importe FROM generate_series(1, 40) g"
  ))
  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "lupa_cp_dinero", fecha = .CP_FECHA)
  ))
  expect_identical(nrow(perfil$resumen_tabla$meta$consultas$fallas), 0L)
})

test_that("SQL Server: tras el corte no se manda nada, tampoco en la llamada siguiente", {
  adm <- .cpm_conectar_mssql()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  suppressWarnings(try(DBI::dbExecute(adm, "DROP TABLE lupa_cp_t"), silent = TRUE))
  DBI::dbExecute(adm, "CREATE TABLE lupa_cp_t (id INTEGER, x FLOAT, s VARCHAR(20))")
  DBI::dbExecute(adm, paste0(
    "INSERT INTO lupa_cp_t VALUES ",
    paste0("(", 1:60, ", ", 1:60 / 4, ", 'v", 1:60 %% 5, "')", collapse = ", ")
  ))
  on.exit(suppressWarnings(try(
    DBI::dbExecute(adm, "DROP TABLE lupa_cp_t"), silent = TRUE
  )), add = TRUE, after = FALSE)
  codigo <- function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  con <- .cpm_conectar_mssql()
  referencia <- .cpm_correr(con, codigo)
  DBI::dbDisconnect(con)
  expect_s3_class(referencia$valor, "perfil_dbi")
  # Posiciones del principio y del medio. Las ultimas no son un corte: lo que
  # queda por llamar ya no viaja al servidor, y el perfil esta completo.
  for (k in unique(c(3L, referencia$k %/% 3L, referencia$k %/% 2L))) {
    con <- .cpm_conectar_mssql()
    id <- as.numeric(DBI::dbGetQuery(con, "SELECT @@SPID AS id")$id)
    r <- .cpm_correr(con, codigo, .cpm_gancho_matar(adm, id, k, "KILL %.0f"))
    expect_s3_class(r$valor, "lupa_error_conexion_dbi")
    expect_identical(r$tras, 0L, info = paste("posicion", k))
    # El controlador sigue diciendo que la conexion es valida...
    expect_true(DBI::dbIsValid(con))
    # ...y la llamada siguiente corta sin llegar a el: ahi R se caia.
    segunda <- .cpm_correr(con, codigo)
    expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
    expect_identical(segunda$llegaron, 0L, info = paste("posicion", k))
    # Otra conexion, nueva, no se toma por perdida.
    nueva <- .cpm_conectar_mssql()
    expect_s3_class(.cpm_correr(nueva, codigo)$valor, "perfil_dbi")
    DBI::dbDisconnect(nueva)
    # Cerrar la cortada es seguro.
    suppressWarnings(DBI::dbDisconnect(con))
  }
})

test_that("SQL Server: lo que una vista puede levantar no llega con el texto de un corte", {
  # Con FreeTDS el texto es lo unico que dice que la conexion se perdio, y no
  # reconocer un corte real hace caer a R en la sentencia siguiente: un lote
  # con `RAISERROR('Communication link failure', ...)` se tomaria por corte.
  # Las sentencias que manda el paquete no pueden recibirlo: el motor no deja
  # levantar un error propio desde una funcion, y lo que una vista puede
  # levantar lleva el texto adentro, no al principio.
  adm <- .cpm_conectar_mssql()
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  limpiar <- function() {
    for (orden in c("DROP VIEW lupa_cp_v_conv", "DROP FUNCTION dbo.lupa_cp_f_throw",
                    "DROP TABLE lupa_cp_buena")) {
      suppressWarnings(try(DBI::dbExecute(adm, orden), silent = TRUE))
    }
  }
  limpiar()
  on.exit(limpiar(), add = TRUE, after = FALSE)
  DBI::dbExecute(adm, "CREATE TABLE lupa_cp_buena (id INTEGER, x INTEGER)")
  DBI::dbExecute(adm, "INSERT INTO lupa_cp_buena VALUES (1, 1), (2, 2), (3, 3)")
  funcion <- tryCatch(DBI::dbExecute(adm, paste(
    "CREATE FUNCTION dbo.lupa_cp_f_throw() RETURNS INT AS BEGIN",
    "THROW 50000, 'Communication link failure', 1; RETURN 1; END"
  )), error = function(e) e)
  expect_s3_class(funcion, "error")
  expect_true(grepl("side-effecting operator", conditionMessage(funcion), fixed = TRUE))
  DBI::dbExecute(adm, paste(
    "CREATE VIEW lupa_cp_v_conv AS",
    "SELECT CAST('Communication link failure' AS INT) AS x"
  ))
  con <- .cpm_conectar_mssql()
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)
  crudo <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM lupa_cp_v_conv"), error = function(e) e
  )
  expect_s3_class(crudo, "error")
  expect_true(grepl("Communication link failure", conditionMessage(crudo), fixed = TRUE))
  r <- .cpm_callar(perfilar_dbi(con, "lupa_cp_v_conv", fecha = .CP_FECHA))
  expect_false(inherits(r, "lupa_error_conexion_dbi"))
  expect_null(.marca_conexion_cortada_dbi(con))
  expect_s3_class(
    .cpm_callar(perfilar_dbi(con, "lupa_cp_buena", fecha = .CP_FECHA)), "perfil_dbi"
  )
})
