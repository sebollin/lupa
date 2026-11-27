# medir_dbi() en MySQL (RMySQL) y MariaDB (RMariaDB). Las pruebas necesitan
# esos controladores, que el paquete no declara: viven en un archivo local
# (`test-local-*`, fuera del tarball) y se saltean solas sin el motor.
#
# 1. La tabla de cada lectura es la que resuelve el catalogo del motor (el
#    esquema y el nombre que guarda), como en los demas motores: dos llamadas
#    sobre la misma tabla con otro rotulo, unidas, no cierran si la tabla cambio
#    entre ellas. Un `Id` con catalogo es la tabla de esa base, y en MariaDB una
#    temporal que tapa a la permanente no se registra como la permanente.
# 2. Una columna BOOLEAN es TINYINT(1) y R la lee entera. En la pertenencia por
#    valor -un dominio o un centinela logico- `TRUE` es `1` y `FALSE` es `0`
#    entre un logico y un numero, en R como en el motor: medir_dbi() la cuenta en
#    el motor (1 de 2 con `1, 0, NULL` y `valores = TRUE`), por bloques da lo
#    mismo, y las dos dan lo que medir() sobre la tabla leida.

testthat::skip_on_cran()
skip_if_not_installed("DBI")

.idmy_conectar <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  puerto_def <- if (maria) "33306" else "33307"
  puerto_var <- if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT"
  puerto <- as.integer(Sys.getenv(puerto_var, Sys.getenv("LUPA_DB_PORT", puerto_def)))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = Sys.getenv("LUPA_MARIADB_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MARIADB_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MARIADB_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = Sys.getenv("LUPA_MYSQL_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MYSQL_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MYSQL_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.idmy_fecha <- as.Date("2026-01-01")

.idmy_citar <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

.idmy_tabla_nueva <- function(con, tabla, columnas, valores) {
  borrar <- function() {
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", .idmy_citar(con, tabla))),
        silent = TRUE)
  }
  borrar()
  DBI::dbExecute(con, paste0("CREATE TABLE ", .idmy_citar(con, tabla), " (", columnas, ")"))
  DBI::dbExecute(con, paste0("INSERT INTO ", .idmy_citar(con, tabla), " VALUES ", valores))
  borrar
}

.idmy_medir_nn <- function(con, tablas) {
  nuc <- metricas_nucleo()
  instancias <- lapply(names(tablas), function(e) {
    instanciar(especializar(nuc$NoNulo, paste0("N", e)), e, "x")
  })
  suppressWarnings(medir_dbi(do.call(modelo, instancias), con, tablas = tablas,
                             id_medicion = "j", fecha = .idmy_fecha))
}

.idmy_probar_identidad <- function(maria) {
  con <- .idmy_conectar(maria)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_mdi_ident_", motor)
  borrar <- .idmy_tabla_nueva(con, tabla, "x INT", "(1), (2), (NULL)")
  on.exit(borrar(), add = TRUE, after = FALSE)
  esquema <- as.character(DBI::dbGetQuery(con, "SELECT DATABASE() AS e")$e)
  con_esquema <- DBI::Id(schema = esquema, table = tabla)
  m <- .idmy_medir_nn(con, list(Nombre = tabla, ConEsquema = con_esquema))
  lecturas <- attr(m, "lecturas", exact = TRUE)
  expect_false(anyNA(lecturas$tabla), info = motor)
  expect_identical(length(unique(lecturas$tabla)), 1L, info = motor)
  # La base y el nombre citados; delante va la huella del servidor.
  expect_true(endsWith(unique(lecturas$tabla), paste0(".", .idmy_citar(con, con_esquema))),
              info = motor)
  expect_identical(m$estado_cuentas, c("cierran", "cierran"), info = motor)
  s <- semaforo_calidad(m, cortes = list(Completitud = c(0.5, 0.9)))
  expect_false(any(s$control_lecturas == "desconocido"), info = motor)

  a <- .idmy_medir_nn(con, list(Nombre = tabla))
  DBI::dbExecute(con, paste0("INSERT INTO ", .idmy_citar(con, tabla), " VALUES (4)"))
  b <- .idmy_medir_nn(con, list(ConEsquema = con_esquema))
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"), info = motor)
  expect_identical(u$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L),
                   info = motor)
}

test_that("MySQL: la tabla de cada lectura es la que resuelve el motor", {
  .idmy_probar_identidad(FALSE)
})

test_that("MariaDB: la tabla de cada lectura es la que resuelve el motor", {
  .idmy_probar_identidad(TRUE)
})

.idmy_probar_tinyint <- function(maria) {
  con <- .idmy_conectar(maria)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motor <- if (maria) "mariadb" else "mysql"
  nuc <- metricas_nucleo()
  cifras <- function(r, nombres) {
    i <- match(nombres, r$metrica_instanciada)
    paste0(r$numerador[i], "/", r$denominador[i])
  }
  # El caso: `1, 0, NULL` con `valores = TRUE` es 1 de 2, por omision (en el
  # motor) y por bloques (en R), como medir() sobre la tabla leida.
  tabla <- paste0("lupa_mdi_tiny_", motor)
  borrar <- .idmy_tabla_nueva(con, tabla, "b BOOLEAN", "(1), (0), (NULL)")
  on.exit(borrar(), add = TRUE, after = FALSE)
  logica <- modelo(instanciar(especializar(nuc$ValoresPosiblesPorExtension, "El",
                                           valores = TRUE),
                              tabla, "b", nombre_instancia = "El"))
  leida <- stats::setNames(list(DBI::dbReadTable(con, tabla)), tabla)
  expect_identical(class(leida[[1L]]$b), "integer", info = motor)
  en_r <- suppressWarnings(medir(logica, leida, id_medicion = "c", fecha = .idmy_fecha,
                                 detalle = "resumen"))
  expect_identical(cifras(en_r, "El"), "1/2", info = motor)
  omision <- suppressWarnings(medir_dbi(logica, con, id_medicion = "c",
                                        fecha = .idmy_fecha))
  expect_identical(attr(omision, "ejecucion_dbi", exact = TRUE)$vias[["El"]],
                   "sql_consolidado", info = motor)
  expect_identical(cifras(omision, "El"), "1/2", info = motor)
  # Por bloques: con una bateria que marca la regla, se mide en R.
  bateria <- conformidad_dbi(con, logica)
  expect_true(all(bateria$conformidad == "conforme"), info = motor)
  bateria$conformidad[] <- "no_conforme"
  bateria$condicion[] <- "siempre"
  bloques <- suppressWarnings(medir_dbi(logica, con, id_medicion = "c",
                                        fecha = .idmy_fecha, conformidad = bateria,
                                        estrategia_no_sql = "bloques"))
  expect_identical(attr(bloques, "ejecucion_dbi", exact = TRUE)$vias[["El"]],
                   "bloques_r", info = motor)
  expect_identical(cifras(bloques, "El"), "1/2", info = motor)

  # Con un 2 (TINYINT(1) lo guarda, y no es `1`) y el resto de las
  # construcciones: todas en el motor, con la cifra de medir().
  tabla2 <- paste0("lupa_mdi_tiny2_", motor)
  borrar2 <- .idmy_tabla_nueva(con, tabla2, "b BOOLEAN", "(1), (0), (NULL), (2)")
  on.exit(borrar2(), add = TRUE, after = FALSE)
  inst <- function(metrica, nombre, ...) {
    instanciar(especializar(metrica, nombre, ...), tabla2, "b", nombre_instancia = nombre)
  }
  todas <- c("El", "Elf", "Nf", "Pe", "Pin", "Ei", "Ni", "Rl")
  mod <- modelo(
    inst(nuc$ValoresPosiblesPorExtension, "El", valores = TRUE),
    inst(nuc$ValoresPosiblesPorExtension, "Elf", valores = c(TRUE, FALSE)),
    inst(nuc$NoNulo, "Nf", valores_nulos = FALSE),
    inst(nuc$ReglaIntegridadIntraEntidad, "Pe", regla = ~ is.na(b) | b == TRUE),
    inst(nuc$ReglaIntegridadIntraEntidad, "Pin", regla = ~ b %in% TRUE),
    inst(nuc$ValoresPosiblesPorExtension, "Ei", valores = 1L),
    inst(nuc$NoNulo, "Ni", valores_nulos = 0L),
    inst(nuc$ValoresPosiblesPorComprension, "Rl", minimo = FALSE, maximo = TRUE)
  )
  leida2 <- stats::setNames(list(DBI::dbReadTable(con, tabla2)), tabla2)
  en_r2 <- suppressWarnings(medir(mod, leida2, id_medicion = "c", fecha = .idmy_fecha,
                                  detalle = "resumen"))
  expect_identical(cifras(en_r2, c("El", "Elf", "Nf")), c("1/3", "2/3", "2/4"),
                   info = motor)
  m2 <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .idmy_fecha))
  vias <- attr(m2, "ejecucion_dbi", exact = TRUE)$vias
  expect_true(all(unlist(vias[todas]) == "sql_consolidado"), info = motor)
  expect_identical(cifras(m2, todas), cifras(en_r2, todas), info = motor)
}

test_that("MySQL: un dominio o centinela logico sobre TINYINT(1) cuenta en el motor como en R", {
  .idmy_probar_tinyint(FALSE)
})

test_that("MariaDB: un dominio o centinela logico sobre TINYINT(1) cuenta en el motor como en R", {
  .idmy_probar_tinyint(TRUE)
})

# Un `DBI::Id(catalog = "b", table = "t")` se cita `b`.`t` y el motor lee `t`
# de la base `b`: la identidad y los tipos son los de esa tabla, no los de la
# homonima de la base de la sesion. La homonima tiene `b INT` y la otra base
# `b TINYINT(1)`: la formula `b == TRUE` solo se traduce con el tipo de la otra.
.idmy_probar_otra_base <- function(maria) {
  con <- .idmy_conectar(maria)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motor <- if (maria) "mariadb" else "mysql"
  otra <- paste0("lupa_mdi_otra_", motor)
  tabla <- "lupa_mdi_homonima"
  try(DBI::dbExecute(con, paste0("DROP DATABASE IF EXISTS ", .idmy_citar(con, otra))),
      silent = TRUE)
  creada <- tryCatch({
    DBI::dbExecute(con, paste0("CREATE DATABASE ", .idmy_citar(con, otra)))
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creada, "La credencial no puede crear una base.")
  on.exit(try(DBI::dbExecute(con, paste0("DROP DATABASE IF EXISTS ", .idmy_citar(con, otra))),
              silent = TRUE), add = TRUE, after = FALSE)
  borrar <- .idmy_tabla_nueva(con, tabla, "x INT, b INT", "(1, 1), (NULL, 2), (3, 3)")
  on.exit(borrar(), add = TRUE, after = FALSE)
  en_otra <- DBI::Id(schema = otra, table = tabla)
  DBI::dbExecute(con, paste0("CREATE TABLE ", .idmy_citar(con, en_otra),
                             " (x INT, b TINYINT(1))"))
  DBI::dbExecute(con, paste0("INSERT INTO ", .idmy_citar(con, en_otra),
                             " VALUES (1, 1), (2, 0), (3, NULL), (4, 1), (NULL, 0)"))
  nuc <- metricas_nucleo()
  formula <- instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "Pe",
                                     regla = ~ is.na(b) | b == TRUE),
                        "Otra", "b", nombre_instancia = "Pe")
  rotulos <- list(Otra = DBI::Id(catalog = otra, table = tabla))
  instancias <- list(
    instanciar(especializar(nuc$NoNulo, "NOtra"), "Otra", "x", nombre_instancia = "NOtra"),
    formula
  )
  d <- suppressWarnings(medir_dbi(do.call(modelo, instancias), con, tablas = rotulos,
                                  id_medicion = "j", fecha = .idmy_fecha))
  expect_true(endsWith(unique(attr(d, "lecturas", exact = TRUE)$tabla),
                       paste0(".", .idmy_citar(con, en_otra))), info = motor)
  expect_identical(d$n_filas_leidas[d$metrica_instanciada == "NOtra"], 5, info = motor)
  expect_identical(attr(d, "ejecucion_dbi", exact = TRUE)$vias[["Pe"]], "sql_consolidado",
                   info = motor)
  leida <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", .idmy_citar(con, en_otra)))
  en_r <- suppressWarnings(medir(modelo(formula), list(Otra = leida), id_medicion = "j",
                                 fecha = .idmy_fecha, detalle = "resumen"))
  expect_identical(c(d$numerador[d$metrica_instanciada == "Pe"],
                     d$denominador[d$metrica_instanciada == "Pe"]),
                   c(en_r$numerador, en_r$denominador), info = motor)
  # Unida con la homonima de la sesion: son dos tablas y no se comparan.
  a <- .idmy_medir_nn(con, list(Sesion = tabla))
  u <- suppressWarnings(rbind(a, d))
  expect_identical(u$estado_cuentas, rep("cierran", nrow(u)), info = motor)
}

test_that("MySQL: un Id con catalogo lee y describe la tabla de esa base", {
  .idmy_probar_otra_base(FALSE)
})

test_that("MariaDB: un Id con catalogo lee y describe la tabla de esa base", {
  .idmy_probar_otra_base(TRUE)
})

# Una TEMPORARY tapa a la permanente del mismo nombre. MariaDB la lista en
# `information_schema.tables`: la lectura de la temporal no se registra con la
# tabla de la permanente, y unida con una lectura de la permanente con las
# mismas filas su control es `desconocido`, no `completo`. (MySQL no lista las
# temporales: ahi es una limitacion declarada en la ayuda.)
test_that("MariaDB: la lectura de una temporal que tapa a la permanente queda sin tabla resuelta", {
  con <- .idmy_conectar(TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tabla <- "lupa_mdi_tapada"
  borrar <- .idmy_tabla_nueva(con, tabla, "x INT", "(1), (NULL), (3)")
  on.exit(borrar(), add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0("CREATE TEMPORARY TABLE ", .idmy_citar(con, tabla), " (x INT)"))
  on.exit(try(DBI::dbExecute(con, paste0("DROP TEMPORARY TABLE IF EXISTS ",
                                         .idmy_citar(con, tabla))), silent = TRUE),
          add = TRUE, after = FALSE)
  DBI::dbExecute(con, paste0("INSERT INTO ", .idmy_citar(con, tabla), " VALUES (4), (NULL), (6)"))
  lista <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM information_schema.tables WHERE table_schema = DATABASE() ",
    "AND table_name = ", DBI::dbQuoteString(con, tabla), " AND table_type = 'TEMPORARY'"
  ))
  skip_if(as.numeric(as.character(lista$n)) < 1, "Este MariaDB no lista las temporales.")
  temporal <- .idmy_medir_nn(con, list(Temporal = tabla))
  expect_true(all(is.na(attr(temporal, "lecturas", exact = TRUE)$tabla)))
  DBI::dbExecute(con, paste0("DROP TEMPORARY TABLE ", .idmy_citar(con, tabla)))
  permanente <- .idmy_medir_nn(con, list(Permanente = tabla))
  expect_false(anyNA(attr(permanente, "lecturas", exact = TRUE)$tabla))
  u <- suppressWarnings(rbind(temporal, permanente))
  s <- semaforo_calidad(u, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas[s$entidad == "Temporal"], "desconocido")
  expect_false(identical(s$control_lecturas[s$entidad == "Permanente"], "completo"))
})

# La identidad lleva el servidor: la misma `base.tabla` en MySQL y en MariaDB son
# dos tablas, y unidas no se comparan (antes salian `no_cierran`, como si la
# tabla hubiera cambiado entre lecturas, tambien con las mismas filas y otro
# contenido, por el contador de cada servidor). La misma tabla por otra conexion
# al mismo servidor sigue siendo una.
test_that("MySQL y MariaDB: la misma tabla en dos servidores son dos tablas", {
  uno <- .idmy_conectar(FALSE)
  on.exit(DBI::dbDisconnect(uno), add = TRUE)
  otro <- .idmy_conectar(TRUE)
  on.exit(DBI::dbDisconnect(otro), add = TRUE)
  tabla <- "lupa_mdi_dos_servidores"
  base_uno <- as.character(DBI::dbGetQuery(uno, "SELECT DATABASE() AS b")$b)
  base_otro <- as.character(DBI::dbGetQuery(otro, "SELECT DATABASE() AS b")$b)
  skip_if(!identical(base_uno, base_otro), "Las dos conexiones no usan una base del mismo nombre.")
  borrar_uno <- .idmy_tabla_nueva(uno, tabla, "x INT", "(1), (2), (3)")
  on.exit(borrar_uno(), add = TRUE, after = FALSE)
  borrar_otro <- .idmy_tabla_nueva(otro, tabla, "x INT", "(7), (8), (9)")
  on.exit(borrar_otro(), add = TRUE, after = FALSE)
  a <- .idmy_medir_nn(uno, list(Uno = tabla))
  b <- .idmy_medir_nn(otro, list(Otro = tabla))
  ta <- unique(attr(a, "lecturas", exact = TRUE)$tabla)
  tb <- unique(attr(b, "lecturas", exact = TRUE)$tabla)
  expect_false(anyNA(c(ta, tb)))
  expect_false(identical(ta, tb))
  u <- suppressWarnings(rbind(a, b))
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  tres <- .idmy_conectar(FALSE)
  on.exit(DBI::dbDisconnect(tres), add = TRUE, after = FALSE)
  c3 <- .idmy_medir_nn(tres, list(Tres = tabla))
  expect_identical(unique(attr(c3, "lecturas", exact = TRUE)$tabla), ta)
})

# MySQL `BIT(1)` por RMySQL: el controlador entrega `0` aunque el bit este
# encendido. La traduccion ya se abstenia; la lectura por bloques mediria lo
# que llego (con un dominio `FALSE`, 4 de 4 donde la tabla tiene 2 de 4): la
# misma guarda la deja `no_medible`, con el mismo motivo.
test_that("MySQL: un BIT leido con RMySQL no se mide por sus valores ni por bloques", {
  con <- .idmy_conectar(FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  skip_if(!inherits(con, "MySQLConnection"), "La conexion no es RMySQL.")
  tabla <- "lupa_mdi_bit_mysql"
  borrar <- .idmy_tabla_nueva(con, tabla, "b BIT(1)", "(b'1'), (b'0'), (NULL), (b'1'), (b'0')")
  on.exit(borrar(), add = TRUE, after = FALSE)
  nuc <- metricas_nucleo()
  inst <- function(metrica, nombre, ...) {
    instanciar(especializar(metrica, nombre, ...), tabla, "b", nombre_instancia = nombre)
  }
  mod <- modelo(
    inst(nuc$ValoresPosiblesPorExtension, "Ef", valores = FALSE),
    inst(nuc$NoNulo, "Nf", valores_nulos = FALSE),
    inst(nuc$NoNulo, "N")
  )
  for (estrategia in c("no_medible", "bloques")) {
    m <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .idmy_fecha,
                                    estrategia_no_sql = estrategia))
    vias <- attr(m, "ejecucion_dbi", exact = TRUE)$vias
    expect_identical(unname(vias[c("Ef", "Nf")]), c("no_medible", "no_medible"),
                     info = estrategia)
    cob <- attr(m, "cobertura_metricas", exact = TRUE)
    motivos <- cob$motivo[cob$metrica_instanciada %in% c("Ef", "Nf")]
    expect_true(length(motivos) == 2L &&
                  all(grepl("RMySQL no la lee con fidelidad", motivos, fixed = TRUE,
                            useBytes = TRUE)), info = estrategia)
    # Contar presentes no depende del valor: se sigue midiendo.
    expect_identical(m$numerador[m$metrica_instanciada == "N"], 4, info = estrategia)
  }
})

# Un dominio o centinela logico con clase (`I(TRUE)`) no se iguala con un
# numero en R, tampoco en un `TINYINT(1)`: la traduccion aplica la misma regla
# que la memoria y no cuenta los `1` (antes publicaba 2 de 4 donde medir() da
# 0 de 4).
.idmy_probar_logico_con_clase <- function(maria) {
  con <- .idmy_conectar(maria)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  motor <- if (maria) "mariadb" else "mysql"
  tabla <- paste0("lupa_mdi_asis_", motor)
  borrar <- .idmy_tabla_nueva(con, tabla, "b TINYINT(1)", "(1), (0), (2), (NULL), (1)")
  on.exit(borrar(), add = TRUE, after = FALSE)
  nuc <- metricas_nucleo()
  inst <- function(metrica, nombre, ...) {
    instanciar(especializar(metrica, nombre, ...), tabla, "b", nombre_instancia = nombre)
  }
  mod <- modelo(
    inst(nuc$ValoresPosiblesPorExtension, "Ea", valores = I(TRUE)),
    inst(nuc$Formato, "Fa", diccionario = I(TRUE)),
    inst(nuc$NoNulo, "Na", valores_nulos = I(TRUE))
  )
  nombres <- c("Ea", "Fa", "Na")
  cifras <- function(r) {
    i <- match(nombres, r$metrica_instanciada)
    paste0(r$numerador[i], "/", r$denominador[i])
  }
  leida <- stats::setNames(list(DBI::dbReadTable(con, tabla)), tabla)
  en_r <- suppressWarnings(medir(mod, leida, id_medicion = "c", fecha = .idmy_fecha,
                                 detalle = "resumen"))
  expect_identical(cifras(en_r), c("0/4", "0/4", "4/5"), info = motor)
  omision <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .idmy_fecha))
  expect_false(any(nombres %in% omision$metrica_instanciada), info = motor)
  bloques <- suppressWarnings(medir_dbi(mod, con, id_medicion = "c", fecha = .idmy_fecha,
                                        estrategia_no_sql = "bloques"))
  expect_identical(cifras(bloques), cifras(en_r), info = motor)
}

test_that("MySQL: un logico con clase no cuenta los 1 de un TINYINT(1), en R ni en el motor", {
  .idmy_probar_logico_con_clase(FALSE)
})

test_that("MariaDB: un logico con clase no cuenta los 1 de un TINYINT(1), en R ni en el motor", {
  .idmy_probar_logico_con_clase(TRUE)
})
