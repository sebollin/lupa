# Pruebas locales de la conexion perdida en MySQL y MariaDB: la sesion se
# termina con `KILL` desde otra conexion justo antes de una llamada del paquete
# al controlador. RMariaDB con `reconnect = TRUE` reabre la sesion sin avisar:
# no hay error que reconocer, y lo dice el identificador de sesion del cliente.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.lcp_conectar <- function(maria, ...) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD", unset = "")
  skip_if(!nzchar(clave), paste0("Sin clave para ", if (maria) "MariaDB" else "MySQL", "."))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave, ...
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.lcp_correr <- function(con, codigo, gancho = NULL) {
  est <- .cp_estado()
  .cp_imitar_dbi(est, gancho)
  valor <- tryCatch(
    suppressWarnings(suppressMessages(codigo(con))), error = function(e) e
  )
  list(valor = valor, k = est$k, tras = est$tras, ops = est$ops,
       llegaron = est$llegaron)
}

.lcp_gancho_matar <- function(adm, id, objetivo) {
  function(est, operacion, conexion, sql) {
    if (!est$cortada && est$k == objetivo) {
      est$cortada <- TRUE
      suppressWarnings(try(
        DBI::dbExecute(adm, sprintf("KILL %.0f", id)), silent = TRUE
      ))
      Sys.sleep(0.3)
    }
  }
}

.lcp_preparar <- function(adm) {
  suppressWarnings(try(DBI::dbExecute(adm, "DROP TABLE lupa_cp_t"), silent = TRUE))
  DBI::dbExecute(adm, "CREATE TABLE lupa_cp_t (id INTEGER, x DOUBLE, s VARCHAR(20))")
  DBI::dbExecute(adm, paste0(
    "INSERT INTO lupa_cp_t VALUES ",
    paste0("(", 1:60, ", ", 1:60 / 4, ", 'v", 1:60 %% 5, "')", collapse = ", ")
  ))
  invisible(NULL)
}

.lcp_probar_corte <- function(maria) {
  adm <- .lcp_conectar(maria)
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .lcp_preparar(adm)
  on.exit(suppressWarnings(try(
    DBI::dbExecute(adm, "DROP TABLE lupa_cp_t"), silent = TRUE
  )), add = TRUE, after = FALSE)
  codigo <- function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  con <- .lcp_conectar(maria)
  referencia <- .lcp_correr(con, codigo)
  DBI::dbDisconnect(con)
  expect_s3_class(referencia$valor, "perfil_dbi")
  for (k in unique(c(2L, referencia$k %/% 2L, referencia$k - 1L))) {
    con <- .lcp_conectar(maria)
    id <- as.numeric(DBI::dbGetQuery(con, "SELECT CONNECTION_ID() AS id")$id)
    r <- .lcp_correr(con, codigo, .lcp_gancho_matar(adm, id, k))
    expect_s3_class(r$valor, "lupa_error_conexion_dbi")
    expect_identical(r$tras, 0L, info = paste("posicion", k))
    # La llamada siguiente sobre la misma conexion tampoco publica nada.
    segunda <- .lcp_correr(con, codigo)
    expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
    if (maria) {
      # RMariaDB: la conexion se reconoce por su puntero, y no llega nada.
      expect_identical(segunda$llegaron, 0L, info = paste("posicion", k))
      expect_true(isTRUE(segunda$valor$datos$recordada))
    } else {
      # RMySQL no tiene puntero -su conexion son dos enteros que una conexion
      # nueva puede repetir- y no se recuerda nada. Tampoco reabre: la llamada
      # vuelve a cortar en sus primeros intentos, sin que nada llegue al
      # servidor.
      expect_false(isTRUE(segunda$valor$datos$recordada))
      expect_lte(segunda$llegaron, 3L)
    }
    suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE))
  }
}

test_that("MariaDB: la sesion terminada corta la llamada", {
  .lcp_probar_corte(TRUE)
})

test_that("MySQL: la sesion terminada corta la llamada", {
  .lcp_probar_corte(FALSE)
})

test_that("MariaDB con `reconnect = TRUE`: la sesion reabierta sin error corta", {
  adm <- .lcp_conectar(TRUE)
  on.exit(DBI::dbDisconnect(adm), add = TRUE)
  .lcp_preparar(adm)
  on.exit(suppressWarnings(try(
    DBI::dbExecute(adm, "DROP TABLE lupa_cp_t"), silent = TRUE
  )), add = TRUE, after = FALSE)
  codigo <- function(con) perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
  con <- .lcp_conectar(TRUE, reconnect = TRUE)
  referencia <- .lcp_correr(con, codigo)
  DBI::dbDisconnect(con)
  expect_s3_class(referencia$valor, "perfil_dbi")
  for (k in unique(c(3L, referencia$k %/% 2L, referencia$k - 1L))) {
    con <- .lcp_conectar(TRUE, reconnect = TRUE)
    id <- as.numeric(DBI::dbGetInfo(con)$thread.id)
    r <- .lcp_correr(con, codigo, .lcp_gancho_matar(adm, id, k))
    # El controlador reabrio: el identificador ya es otro.
    expect_false(identical(as.numeric(DBI::dbGetInfo(con)$thread.id), id))
    expect_s3_class(r$valor, "lupa_error_conexion_dbi")
    # La sesion reabierta contesta bien, y no se la usa: sin la regla, la
    # llamada siguiente devolvia un perfil leido ahi.
    segunda <- .lcp_correr(con, codigo)
    expect_s3_class(segunda$valor, "lupa_error_conexion_dbi")
    expect_identical(segunda$llegaron, 0L, info = paste("posicion", k))
    expect_identical(nrow(DBI::dbGetQuery(con, "SELECT 1 AS uno")), 1L)
    DBI::dbDisconnect(con)
  }
})

test_that("MariaDB y MySQL: la cancelacion se reconoce por su codigo", {
  maria <- .lcp_conectar(TRUE)
  on.exit(DBI::dbDisconnect(maria), add = TRUE)
  falla <- function(con, sql) {
    tryCatch({
      .traer_consulta_dbi(con, sql)
      NULL
    }, error = function(e) e)
  }
  expect_identical(
    .reconocer_falla_dbi(maria, falla(
      maria, "SET STATEMENT max_statement_time = 0.05 FOR SELECT SLEEP(2)"
    )),
    "cancelada"
  )
  expect_identical(
    .reconocer_falla_dbi(maria, falla(maria, "SELECT STDEV(1.0)")), "capacidad"
  )
  # Un nombre de columna con el numero entre corchetes no es el codigo.
  expect_identical(
    .reconocer_falla_dbi(maria, falla(maria, "SELECT `x [2013]` FROM DUAL")),
    "rechazada"
  )
  mysql <- .lcp_conectar(FALSE)
  on.exit(DBI::dbDisconnect(mysql), add = TRUE)
  expect_identical(
    .reconocer_falla_dbi(mysql, falla(mysql, "SELECT STDEV(1.0)")), "capacidad"
  )
  expect_identical(
    .reconocer_falla_dbi(mysql, falla(mysql, "SELECT `x [2013]` FROM DUAL")),
    "rechazada"
  )
})

test_that("dos servidores con una base del mismo nombre no son el mismo lugar", {
  # Los dos servidores con el MISMO controlador y la misma clase de conexion,
  # y una base que se llama igual: lo que la conexion declara no los distingue.
  skip_if_not_installed("RMySQL")
  clave_maria <- Sys.getenv("LUPA_MARIADB_PWD", unset = "")
  clave_mysql <- Sys.getenv("LUPA_MYSQL_PWD", unset = "")
  skip_if(!nzchar(clave_maria) || !nzchar(clave_mysql), "Sin clave de los dos servidores.")
  abrir <- function(host, puerto, base, usuario, clave) {
    tryCatch(DBI::dbConnect(
      RMySQL::MySQL(), host = host, port = as.integer(puerto), dbname = base,
      username = usuario, password = clave
    ), error = function(e) NULL)
  }
  uno <- abrir(
    Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"), Sys.getenv("LUPA_MARIA_PORT", "33306"),
    Sys.getenv("LUPA_MARIA_NAME", "lupa"), Sys.getenv("LUPA_MARIA_USER", "root"),
    clave_maria
  )
  dos <- abrir(
    Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"), Sys.getenv("LUPA_MYSQL_PORT", "33307"),
    Sys.getenv("LUPA_MYSQL_NAME", "lupa"), Sys.getenv("LUPA_MYSQL_USER", "root"),
    clave_mysql
  )
  skip_if(is.null(uno) || is.null(dos), "No se pudo abrir los dos servidores con RMySQL.")
  on.exit({
    DBI::dbDisconnect(uno)
    DBI::dbDisconnect(dos)
  }, add = TRUE)
  base_de <- function(con) as.character(DBI::dbGetQuery(con, "SELECT DATABASE() AS b")$b)
  skip_if(!identical(base_de(uno), base_de(dos)), "Las bases no se llaman igual.")
  expect_identical(class(uno), class(dos))
  expect_false(identical(.lugar_conexion_dbi(uno), .lugar_conexion_dbi(dos)))

  # La consecuencia, por la funcion publica: una bateria de conformidad armada
  # en un servidor no se aplica al medir en el otro. Con el lugar tomado de lo
  # que la conexion declara, la identidad de los dos servidores era la misma.
  otra_del_primero <- abrir(
    Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"), Sys.getenv("LUPA_MARIA_PORT", "33306"),
    Sys.getenv("LUPA_MARIA_NAME", "lupa"), Sys.getenv("LUPA_MARIA_USER", "root"),
    clave_maria
  )
  on.exit(DBI::dbDisconnect(otra_del_primero), add = TRUE)
  for (con in list(uno, dos)) {
    suppressWarnings(try(DBI::dbExecute(con, "DROP TABLE lupa_cp_lugar"), silent = TRUE))
    DBI::dbExecute(con, "CREATE TABLE lupa_cp_lugar (id INTEGER, x DOUBLE)")
    DBI::dbExecute(con, "INSERT INTO lupa_cp_lugar VALUES (1, 0.5), (2, NULL)")
  }
  on.exit(for (con in list(uno, dos)) {
    suppressWarnings(try(DBI::dbExecute(con, "DROP TABLE lupa_cp_lugar"), silent = TRUE))
  }, add = TRUE, after = FALSE)
  nucleo <- metricas_nucleo()
  reglas <- modelo(
    instanciar(especializar(nucleo$NoNulo, "NN_x"), "lupa_cp_lugar", "x")
  )
  bateria <- suppressWarnings(suppressMessages(conformidad_dbi(uno, reglas)))
  # Lo que la medicion dice que difiere entre la bateria y la conexion.
  difiere <- function(con) {
    visto <- character()
    medicion <- withCallingHandlers(
      suppressMessages(medir_dbi(
        reglas, con, conformidad = bateria, fecha = .CP_FECHA,
        id_medicion = "fijo"
      )),
      warning = function(aviso) {
        if (inherits(aviso, "lupa_aviso_conformidad_dbi")) {
          visto <<- c(visto, as.character(aviso$datos$difiere))
        }
        invokeRestart("muffleWarning")
      }
    )
    expect_s3_class(medicion, "medicion_resumen")
    visto
  }
  expect_identical(difiere(otra_del_primero), character())
  expect_identical(difiere(dos), "servidor")
})

test_that("MariaDB y MySQL: el lugar es el servidor y la base, no como se llego", {
  for (maria in c(TRUE, FALSE)) {
    una <- .lcp_conectar(maria)
    otra <- .lcp_conectar(maria)
    expect_identical(.lugar_conexion_dbi(una), .lugar_conexion_dbi(otra))
    DBI::dbDisconnect(otra)
    DBI::dbDisconnect(una)
  }
})

test_that("MariaDB y MySQL: una tabla vacia o toda nula concilia con el plan de la corrida", {
  for (maria in c(TRUE, FALSE)) {
    con <- .lcp_conectar(maria)
    motor <- if (maria) "MariaDB" else "MySQL"
    suppressWarnings(try(DBI::dbExecute(con, "DROP TABLE lupa_cp_vacia"), silent = TRUE))
    DBI::dbExecute(con, "CREATE TABLE lupa_cp_vacia (id INTEGER, x DOUBLE, s VARCHAR(20))")
    for (caso in c("vacia", "toda_nula")) {
      if (identical(caso, "toda_nula")) {
        DBI::dbExecute(con, paste(
          "INSERT INTO lupa_cp_vacia VALUES",
          "(NULL, NULL, NULL), (NULL, NULL, NULL), (NULL, NULL, NULL)"
        ))
      }
      for (bloque in c("con_muestra", "solo_agregados")) {
        rotulo <- paste(motor, caso, bloque)
        perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
          con, "lupa_cp_vacia", bloque_muestra = bloque, fecha = .CP_FECHA
        )))
        consultas <- perfil$resumen_tabla$meta$consultas
        expect_identical(consultas$conciliacion$estado, "conciliado", info = rotulo)
        expect_equal(as.numeric(consultas$conciliacion$anunciadas),
                     as.numeric(consultas$emitidas), info = rotulo)
        # Lo que no corrio queda en el plan previo, con su clase: las medianas.
        previo <- consultas$conciliacion$plan_previo
        omitidas <- previo$detalle$clase_consulta[previo$detalle$diferencia < 0]
        expect_identical(previo$estado, "desfasado", info = rotulo)
        expect_true(all(grepl("^mediana \\(", omitidas)), info = rotulo)
        expect_identical(length(omitidas), 1L, info = rotulo)
      }
    }
    suppressWarnings(try(DBI::dbExecute(con, "DROP TABLE lupa_cp_vacia"), silent = TRUE))
    DBI::dbDisconnect(con)
  }
})

test_that("MariaDB y MySQL: un error del servidor con el texto de un corte y su propio numero no corta ni marca", {
  # Ahi decide el numero del error, no el texto: una funcion que levanta el
  # texto de una conexion perdida con el numero de un error del usuario es un
  # error comun. (Queda declarado que una rutina que levante a proposito el
  # numero de un error del cliente se toma por corte: el controlador no dice,
  # sin mandar nada, si la conexion sigue abierta.)
  for (maria in c(TRUE, FALSE)) {
    motor <- if (maria) "MariaDB" else "MySQL"
    adm <- .lcp_conectar(maria)
    .lcp_preparar(adm)
    limpiar <- function() {
      for (orden in c("DROP VIEW IF EXISTS lupa_cp_v_senal",
                      "DROP FUNCTION IF EXISTS lupa_cp_f_senal",
                      "DROP TABLE IF EXISTS lupa_cp_t")) {
        suppressWarnings(try(DBI::dbExecute(adm, orden), silent = TRUE))
      }
    }
    suppressWarnings(try(DBI::dbExecute(adm, "DROP VIEW IF EXISTS lupa_cp_v_senal"), silent = TRUE))
    suppressWarnings(try(DBI::dbExecute(adm, "DROP FUNCTION IF EXISTS lupa_cp_f_senal"), silent = TRUE))
    DBI::dbExecute(adm, paste(
      "CREATE FUNCTION lupa_cp_f_senal() RETURNS INT DETERMINISTIC NO SQL BEGIN",
      "SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT =",
      "'Lost connection to MySQL server during query'; RETURN 1; END"
    ))
    DBI::dbExecute(adm, "CREATE VIEW lupa_cp_v_senal AS SELECT lupa_cp_f_senal() AS x")
    con <- .lcp_conectar(maria)
    crudo <- tryCatch(
      DBI::dbGetQuery(con, "SELECT * FROM lupa_cp_v_senal"), error = function(e) e
    )
    expect_true(grepl("Lost connection to MySQL server during query",
                      conditionMessage(crudo), fixed = TRUE), info = motor)
    r <- tryCatch(suppressWarnings(suppressMessages(
      perfilar_dbi(con, "lupa_cp_v_senal", fecha = .CP_FECHA)
    )), error = function(e) e)
    expect_s3_class(r, "lupa_error_dbi")
    expect_false(inherits(r, "lupa_error_conexion_dbi"), info = motor)
    siguiente <- tryCatch(suppressWarnings(suppressMessages(
      perfilar_dbi(con, "lupa_cp_t", fecha = .CP_FECHA)
    )), error = function(e) e)
    expect_s3_class(siguiente, "perfil_dbi")
    DBI::dbDisconnect(con)
    limpiar()
    DBI::dbDisconnect(adm)
  }
})
