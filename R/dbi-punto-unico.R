# ---- El punto unico de la via DBI ----------------------------------------
#
# Toda llamada al controlador -consultas, metadatos, filas, el estado de un
# resultado- va escrita como argumento de `.dbi()`. Lo que resuelve, una vez y
# por construccion:
#
# * La conexion que se pierde. El error del controlador se reconoce ahi mismo
#   (`.reconocer_error_dbi()`), y si dice que la conexion ya no sirve se corta
#   la corrida entera. Antes, cada sitio que atrapaba un error para degradar
#   una metrica atrapaba tambien este: la tabla volvia como un perfil parcial,
#   la coleccion la salteaba y seguia, y con RPostgres -que reabre la sesion
#   sola- lo que seguia se leia en OTRA sesion. Con odbc + FreeTDS la sentencia
#   siguiente a un corte hace caer a R.
# * Nadie por encima puede tragarse el corte: se senala con una condicion que
#   NO hereda de `error`, asi que ningun `tryCatch(error = )` ni `try()` del
#   paquete la atrapa. La atrapa solo el borde de la corrida
#   (`.con_corrida_dbi()`), que la convierte en `lupa_error_conexion_dbi`.
# * Despues del corte no se manda nada mas por esa conexion: el pestillo de la
#   corrida hace que la llamada siguiente corte sin llegar al controlador.
#
# No se manda ninguna sentencia para saberlo: el corte sale del error mismo, de
# `dbIsValid()` cuando el paquete ya lo preguntaba, y del identificador de
# sesion que el controlador guarda del lado del cliente.

.CORRIDAS_DBI <- new.env(parent = emptyenv())
.CORRIDAS_DBI$actual <- NULL

# El paquete que implementa la conexion. La clase no alcanza: RMariaDB entrega
# `MySQLConnection` cuando el servidor es MySQL, la misma clase que RMySQL, y
# sus errores no se parecen.
.CONTROLADORES_DBI <- c(
  "RPostgres", "RMariaDB", "RMySQL", "odbc", "duckdb", "RSQLite"
)

.controlador_dbi <- function(conexion) {
  paquete <- tryCatch(
    attr(class(conexion), "package", exact = TRUE), error = function(e) NULL
  )
  if (length(paquete) == 1L && !is.na(paquete) &&
      paquete %in% .CONTROLADORES_DBI) {
    return(as.character(paquete))
  }
  clases <- c(
    PqConnection = "RPostgres", MariaDBConnection = "RMariaDB",
    OdbcConnection = "odbc", duckdb_connection = "duckdb",
    SQLiteConnection = "RSQLite"
  )
  for (clase in names(clases)) {
    if (inherits(conexion, clase)) return(clases[[clase]])
  }
  ""
}

# Dos conexiones son la misma si son el mismo objeto. `identical()` compara los
# punteros externos por la direccion que guardan: dos conexiones cerradas, o una
# nueva que el sistema ubico donde estaba otra, darian igual. El puntero se
# compara ademas como referencia.
.puntero_conexion_dbi <- function(conexion) {
  # `ptr` en RPostgres, RMariaDB, RSQLite y odbc; `conn_ref` en duckdb; y si
  # no, la primera ranura que sea un puntero externo. RMySQL no tiene ninguna:
  # su conexion son dos enteros.
  campos <- tryCatch(names(attributes(conexion)), error = function(e) NULL)
  for (campo in unique(c("ptr", "conn_ref", campos))) {
    puntero <- tryCatch(
      attr(conexion, campo, exact = TRUE), error = function(e) NULL
    )
    if (identical(typeof(puntero), "externalptr")) return(puntero)
  }
  NULL
}

.mismo_puntero_dbi <- function(a, b) {
  !is.null(a) && !is.null(b) && identical(a, b, extptr.as.ref = TRUE)
}

.misma_conexion_dbi <- function(a, b) {
  if (!identical(a, b)) return(FALSE)
  pa <- .puntero_conexion_dbi(a)
  pb <- .puntero_conexion_dbi(b)
  if (is.null(pa) || is.null(pb)) return(TRUE)
  .mismo_puntero_dbi(pa, pb)
}

# ---- La corrida ----------------------------------------------------------
#
# Una corrida es lo que dura la llamada exportada mas externa sobre una
# conexion. Guarda lo que se lee una sola vez -`dbGetInfo()`, las senas, el
# lugar-, el pestillo de la conexion perdida y el identificador de sesion del
# cliente. Una llamada anidada sobre la misma conexion -el plan dentro del
# perfil, cada tabla de una coleccion- se suma a la que ya esta.

.corrida_dbi <- function(conexion) {
  corrida <- .CORRIDAS_DBI$actual
  while (!is.null(corrida)) {
    if (.misma_conexion_dbi(corrida$conexion, conexion)) return(corrida)
    corrida <- corrida$anterior
  }
  NULL
}

# El identificador de sesion que el controlador tiene del lado del cliente: no
# cuesta una sentencia. RPostgres reabre la sesion sin avisar y RMariaDB con
# `reconnect = TRUE` tambien; en los dos el identificador cambia. Los demas
# controladores no reabren, o no lo publican.
.id_sesion_cliente_dbi <- function(controlador, info) {
  campo <- switch(
    controlador,
    RPostgres = "pid", RMariaDB = "thread.id", RMySQL = "threadId", NULL
  )
  if (is.null(campo) || !is.list(info)) return(NA_real_)
  valor <- tryCatch(info[[campo]], error = function(e) NULL)
  if (length(valor) != 1L || is.na(valor)) return(NA_real_)
  suppressWarnings(as.numeric(valor))
}

.sesion_cambio_dbi <- function(corrida) {
  if (is.null(corrida) || is.na(corrida$id_sesion)) return(FALSE)
  info <- tryCatch(DBI::dbGetInfo(corrida$conexion), error = .nulo_ante_el_fallo_dbi)
  ahora <- .id_sesion_cliente_dbi(corrida$controlador, info)
  # Si el controlador ya no lo dice, no es la sesion con la que se empezo.
  is.na(ahora) || !identical(ahora, corrida$id_sesion)
}

.MOTIVO_SESION_REABIERTA_DBI <- paste0(
  "el controlador reabri\u00f3 la sesi\u00f3n sin avisar, y lo le\u00eddo antes",
  " y despu\u00e9s no viene de la misma sesi\u00f3n"
)

.MOTIVO_CONEXION_INVALIDA_DBI <- paste0(
  "la conexi\u00f3n dej\u00f3 de ser v\u00e1lida (`DBI::dbIsValid()` devolvi\u00f3",
  " `FALSE`)"
)

.marcar_corrida_reanudable_dbi <- function(conexion) {
  corrida <- .corrida_dbi(conexion)
  if (!is.null(corrida)) corrida$reanudable <- TRUE
  invisible(NULL)
}

# En el borde de cada tabla de una coleccion, sin ningun resultado abierto: si
# la sesion no es la del comienzo, la tabla siguiente se leeria en otra.
.comprobar_sesion_corrida_dbi <- function(conexion) {
  corrida <- .corrida_dbi(conexion)
  if (is.null(corrida) || isTRUE(corrida$perdida)) return(invisible(NULL))
  if (.sesion_cambio_dbi(corrida)) {
    .cortar_dbi(corrida, conexion, motivo = .MOTIVO_SESION_REABIERTA_DBI)
  }
  invisible(NULL)
}

.abrir_corrida_dbi <- function(conexion, accion) {
  corrida <- new.env(parent = emptyenv())
  corrida$conexion <- conexion
  corrida$accion <- accion
  corrida$controlador <- .controlador_dbi(conexion)
  corrida$anterior <- .CORRIDAS_DBI$actual
  corrida$perdida <- FALSE
  corrida$motivo <- NA_character_
  corrida$traducida <- FALSE
  corrida$recordada <- FALSE
  corrida$abriendo <- TRUE
  corrida$llamadas <- 0L
  corrida$probada <- FALSE
  corrida$contesto <- FALSE
  corrida$fallo <- FALSE
  corrida$causa <- "conexion"
  corrida$abiertos <- 0L
  corrida$reanudable <- FALSE
  corrida$senas <- NULL
  corrida$lugar <- NULL
  corrida$guarda_duckdb <- 0L
  corrida$info <- NULL
  corrida$id_sesion <- NA_real_
  corrida$senas_previas <- list(
    conexion = .CONTEXTO_SENAS$conexion, info = .CONTEXTO_SENAS$info
  )
  .CORRIDAS_DBI$actual <- corrida
  corrida
}

.cerrar_corrida_dbi <- function(corrida) {
  .CONTEXTO_SENAS$conexion <- corrida$senas_previas$conexion
  .CONTEXTO_SENAS$info <- corrida$senas_previas$info
  # Se saca esta corrida y cualquiera que haya quedado encima.
  actual <- .CORRIDAS_DBI$actual
  while (!is.null(actual) && !identical(actual, corrida)) {
    actual <- actual$anterior
  }
  .CORRIDAS_DBI$actual <- if (is.null(actual)) NULL else corrida$anterior
  invisible(NULL)
}

# Evalua el cuerpo de una llamada exportada dentro de su corrida. El cuerpo
# llega sin evaluar y se evalua en el marco de quien llama, asi que `return()`,
# `on.exit()`, `missing()` y `match.call()` valen lo mismo que afuera.
.con_corrida_dbi <- function(conexion, accion, cuerpo) {
  if (!.es_conexion_dbi(conexion) || !is.null(.corrida_dbi(conexion))) {
    return(cuerpo)
  }
  corrida <- .abrir_corrida_dbi(conexion, accion)
  on.exit({
    # Un cuerpo que sale con `return()` no pasa por lo que sigue al
    # `tryCatch()`: la comprobacion de salida se hace aca, y ninguna salida
    # -un valor, otro error- puede publicar lo leido con la conexion perdida.
    if (!isTRUE(corrida$perdida) && !isTRUE(corrida$traducida) &&
        .sesion_cambio_dbi(corrida)) {
      .poner_pestillo_dbi(corrida, .MOTIVO_SESION_REABIERTA_DBI)
    }
    .cerrar_corrida_dbi(corrida)
    if (isTRUE(corrida$perdida) && !isTRUE(corrida$traducida)) {
      corrida$traducida <- TRUE
      .detener(.condicion_conexion_perdida_dbi(corrida))
    }
  }, add = TRUE)
  tryCatch({
    .preparar_corrida_dbi(corrida)
    cuerpo
  }, lupa_corte_conexion = function(corte) {
    corrida$traducida <- TRUE
    .detener(.condicion_conexion_perdida_dbi(corrida))
  })
}

# Lo que se hace una vez al abrir: saber si en esta conexion ya se habia
# reconocido un corte -y entonces no se manda nada-, leer `dbGetInfo()` si hace
# falta y, en DuckDB, negarse si hay una transaccion abierta.
.preparar_corrida_dbi <- function(corrida) {
  conexion <- corrida$conexion
  cortada <- .corte_recordado_dbi(conexion)
  if (!is.null(cortada)) {
    corrida$perdida <- TRUE
    corrida$recordada <- TRUE
    corrida$motivo <- cortada
    .cortar_dbi(corrida, conexion)
  }
  # `dbGetInfo()` se lee una vez por corrida y recien cuando alguien la pide
  # (`.info_dbi_corrida()`): una llamada que no mira las senas no la pregunta.
  # Solo los controladores que publican el identificador de sesion la leen al
  # abrir, porque es el punto de partida de la comparacion; en ellos no cuesta
  # una sentencia.
  if (corrida$controlador %in% c("RPostgres", "RMariaDB", "RMySQL")) {
    .leer_info_corrida_dbi(corrida)
  }
  if (identical(corrida$controlador, "duckdb")) {
    .negar_transaccion_duckdb_dbi(corrida)
  }
  corrida$abriendo <- FALSE
  invisible(NULL)
}

# No pasa por `.dbi()`: una conexion que ya no servia al entrar no se perdio en
# esta corrida, y su error es el de la validacion de siempre. Solo se guarda
# una lectura que funciono: si falla, cada sitio sigue preguntando y recibe el
# error como antes.
.leer_info_corrida_dbi <- function(corrida) {
  if (!is.null(corrida$info) || isTRUE(corrida$perdida)) return(corrida$info)
  conexion <- corrida$conexion
  info <- tryCatch(DBI::dbGetInfo(conexion), error = .nulo_ante_el_fallo_dbi)
  if (is.null(info)) return(NULL)
  corrida$info <- info
  corrida$id_sesion <- .id_sesion_cliente_dbi(corrida$controlador, info)
  if (identical(corrida, .CORRIDAS_DBI$actual)) {
    .CONTEXTO_SENAS$conexion <- conexion
    .CONTEXTO_SENAS$info <- info
  }
  info
}

# El mensaje se arma una sola vez, en el borde. Dice que se perdio con la
# sesion y que hay que hacer: cerrarla y abrir otra.
.AVISO_CONEXION_CORTADA_DBI <- paste0(
  " lupa no vuelve a usar una conexi\u00f3n en la que reconoci\u00f3 un corte, aunque",
  " el controlador la reabra: la sesi\u00f3n nueva ya no tiene la transacci\u00f3n",
  " abierta, las tablas temporales, el esquema o la base elegidos, el rol ni",
  " los par\u00e1metros de sesi\u00f3n de la que se cort\u00f3. Cerrarla con",
  " `DBI::dbDisconnect()` y abrir otra."
)

.condicion_conexion_perdida_dbi <- function(corrida) {
  motivo <- corrida$motivo
  if (length(motivo) != 1L || is.na(motivo) || !nzchar(motivo)) {
    motivo <- "el controlador no dijo por qu\u00e9"
  }
  motivo <- sub("[[:space:].]+$", "", motivo, perl = TRUE, useBytes = TRUE)
  if (identical(corrida$causa, "transaccion")) {
    return(.condicion_dbi(
      "lupa_error_transaccion_dbi",
      paste0(
        "La transacci\u00f3n abierta en esta conexi\u00f3n est\u00e1 abortada: un error",
        " anterior la dej\u00f3 as\u00ed, y el motor rechaza toda sentencia hasta que",
        " termine (", motivo, "). No es la tabla ni la conexi\u00f3n, que sigue",
        " sirviendo: deshacer la transacci\u00f3n con `DBI::dbRollback()` y volver",
        " a llamar."
      ),
      datos = list(abortada = TRUE, accion = corrida$accion, motivo = motivo)
    ))
  }
  recordada <- isTRUE(corrida$recordada)
  reanudar <- if (!recordada && isTRUE(corrida$reanudable) &&
                  .reanudable_conexion_dbi(corrida)) {
    paste0(
      " Lo ya perfilado qued\u00f3 guardado: la colecci\u00f3n puede retomarse",
      " desde su directorio de reanudaci\u00f3n con una conexi\u00f3n nueva."
    )
  } else ""
  cabeza <- if (recordada) {
    paste0(
      "Esta conexi\u00f3n se perdi\u00f3 en una llamada anterior (", motivo,
      "): no se manda nada por ella."
    )
  } else {
    paste0(
      "La conexi\u00f3n dej\u00f3 de estar disponible mientras se la le\u00eda: ", motivo,
      ". Se corta para no devolver un resultado parcial ni lo le\u00eddo en otra",
      " sesi\u00f3n."
    )
  }
  .condicion_dbi(
    "lupa_error_conexion_dbi",
    paste0(cabeza, .AVISO_CONEXION_CORTADA_DBI, reanudar),
    datos = list(perdida = TRUE, recordada = recordada,
                 accion = corrida$accion, motivo = motivo)
  )
}

# Retomar exige volver a abrir la misma base: solo un archivo de SQLite o de
# DuckDB se puede reconocer como el mismo lugar sin preguntarle al motor que
# se acaba de perder.
.reanudable_conexion_dbi <- function(corrida) {
  if (!corrida$controlador %in% c("RSQLite", "duckdb")) return(FALSE)
  informacion <- corrida$info
  if (!is.list(informacion)) return(FALSE)
  nombres <- .minusculas_ascii(gsub("[^A-Za-z0-9]", "", names(informacion)))
  posiciones <- which(nombres %in% c("dbname", "dbdir", "database"))
  if (!length(posiciones)) return(FALSE)
  valores <- as.character(unlist(informacion[posiciones], use.names = FALSE))
  any(!is.na(valores) & nzchar(trimws(valores)) &
        !.minusculas_ascii(trimws(valores)) %in% c("memory", ":memory:", "<na>"))
}

# ---- El corte ------------------------------------------------------------

.cortar_dbi <- function(corrida, conexion, fallo = NULL, motivo = NULL,
                        causa = "conexion") {
  if (is.null(motivo) && !is.null(fallo)) {
    motivo <- tryCatch(.mensaje_motor(fallo), error = function(e) NA_character_)
  }
  if (is.null(corrida)) {
    # Fuera de una corrida no hay borde que convierta el corte.
    suelta <- .abrir_corrida_dbi(conexion, "leer de la conexi\u00f3n")
    cortada <- .marca_conexion_cortada_dbi(conexion)
    if (is.null(cortada)) {
      .poner_pestillo_dbi(suelta, motivo %||% NA_character_)
    } else {
      suelta$perdida <- TRUE
      suelta$recordada <- TRUE
      suelta$motivo <- cortada
    }
    .cerrar_corrida_dbi(suelta)
    .detener(.condicion_conexion_perdida_dbi(suelta))
  }
  if (!isTRUE(corrida$perdida)) {
    if (identical(causa, "transaccion")) {
      # La corrida no sigue -toda sentencia daria el mismo error, tambien las
      # de limpieza-, pero la conexion sirve: no se marca.
      corrida$perdida <- TRUE
      corrida$motivo <- motivo %||% NA_character_
      corrida$causa <- "transaccion"
    } else {
      .poner_pestillo_dbi(corrida, motivo %||% NA_character_)
    }
  }
  # Ya convertido -un `on.exit()` de la llamada exportada que todavia lee-: no
  # queda borde, y la condicion tiene que ser un error.
  if (isTRUE(corrida$traducida)) {
    .detener(.condicion_conexion_perdida_dbi(corrida))
  }
  .detener(structure(
    class = c("lupa_corte_conexion", "condition"),
    list(message = "La conexi\u00f3n DBI se perdi\u00f3.", call = NULL)
  ))
}

# ---- La conexion cortada no se vuelve a usar -----------------------------
#
# lupa no vuelve a usar una conexion en la que reconocio un corte, hasta que se
# cierre. Con odbc `dbIsValid()` sigue diciendo `TRUE` despues del corte y la
# sentencia siguiente hace caer a R (medido con FreeTDS). RPostgres siempre, y
# RMariaDB con `reconnect = TRUE`, reabren la sesion dentro del MISMO objeto:
# la sesion nueva sirve, pero ya no es la de quien llama -perdio la transaccion
# abierta, las tablas temporales, `search_path`, el rol y los parametros de
# sesion-, y un bucle que atrapa el error de una tabla y sigue con la otra
# leeria las siguientes ahi, sin enterarse.
#
# La marca va en el puntero externo de la conexion, como un atributo. Un
# puntero externo no se copia: el atributo queda en el objeto mismo, lo ven
# todos los nombres de esa conexion -un alias es el mismo puntero- y no lo ve
# ninguna otra, porque un `dbConnect()` nuevo es siempre otro puntero. No hay
# registro aparte ni nada que conserve el objeto: si quien llama lo suelta sin
# cerrarlo, se recolecta como siempre y su controlador cierra la sesion que
# hubiera reabierto. Ni marcar ni rechazar manda nada al motor.
#
# RMySQL no tiene puntero -su conexion son dos enteros, que una conexion nueva
# puede repetir-, asi que ahi no se recuerda nada; tampoco reabre: la llamada
# siguiente vuelve a cortar en su primer intento.
.MARCA_CONEXION_CORTADA_DBI <- "lupa_conexion_cortada"

.marca_conexion_cortada_dbi <- function(conexion) {
  puntero <- .puntero_conexion_dbi(conexion)
  if (is.null(puntero)) return(NULL)
  marca <- attr(puntero, .MARCA_CONEXION_CORTADA_DBI, exact = TRUE)
  if (is.character(marca) && length(marca) == 1L) marca else NULL
}

.marcar_conexion_cortada_dbi <- function(conexion, motivo) {
  puntero <- .puntero_conexion_dbi(conexion)
  if (is.null(puntero)) return(invisible(FALSE))
  # El motivo es el del primer corte.
  if (!is.null(.marca_conexion_cortada_dbi(conexion))) return(invisible(TRUE))
  if (length(motivo) != 1L || is.na(motivo) || !nzchar(motivo)) {
    motivo <- "el controlador no dijo por qu\u00e9"
  }
  attr(puntero, .MARCA_CONEXION_CORTADA_DBI) <- as.character(motivo)
  invisible(TRUE)
}

# El pestillo de la corrida y la marca de la conexion se ponen juntos.
.poner_pestillo_dbi <- function(corrida, motivo) {
  corrida$perdida <- TRUE
  corrida$motivo <- motivo
  .marcar_conexion_cortada_dbi(corrida$conexion, motivo)
  invisible(NULL)
}

# El motivo del corte que esta conexion ya tuvo, o `NULL`. Es leer un
# atributo: no se le pregunta nada al controlador, tampoco si sigue siendo
# valida. Con RPostgres, una vez que el cliente vio el corte, `dbGetInfo()` y
# cualquier sentencia reabren la sesion (medido; `dbIsValid()` no), y esa
# sesion quedaria viva en el servidor sin que nadie la haya pedido; con odbc la
# sentencia siguiente a un corte hace caer a R. No tocarla no depende de saber
# que pregunta es inocua en cada controlador. Por eso una conexion marcada se
# rechaza igual despues de cerrada.
.corte_recordado_dbi <- function(conexion) {
  .marca_conexion_cortada_dbi(conexion)
}

# ---- La llamada ----------------------------------------------------------
#
# `llamada` es la llamada al controlador, sin evaluar: se evalua aca adentro.
# `operacion` la nombra. Con el pestillo puesto, cerrar un resultado y
# preguntar por su estado contestan sin llegar al controlador -para que los
# `on.exit()` y los bucles de lectura terminen-, y cualquier otra corta.
.OPERACIONES_SIN_CONTROLADOR_DBI <- c(
  "dbClearResult", "dbHasCompleted", "dbIsValid"
)

# Las que, si contestan, prueban que la conexion servia en esta corrida.
.OPERACIONES_QUE_PRUEBAN_DBI <- c(
  "dbGetQuery", "dbSendQuery", "dbExecute", "dbFetch", "dbListFields"
)

# Las que le hacen una pregunta al motor: si alguna contesto, la transaccion
# de quien llama no estaba abortada al entrar.
.OPERACIONES_QUE_CONTESTAN_DBI <- c(
  .OPERACIONES_QUE_PRUEBAN_DBI, "dbExistsTable"
)

# Los manejadores que atrapan un error del controlador son funciones del
# espacio de nombres, no funciones armadas en el marco que tiene la conexion.
# RSQLite, RPostgres y RMariaDB dejan retenido el manejador del ultimo error
# suyo que se atrapo -con su entorno- hasta el error siguiente: con un
# manejador armado ahi adentro, la conexion que quien llama solto sin cerrar
# no se recolectaba, y con RPostgres su sesion seguia viva en el servidor
# (medido sin el paquete: `tryCatch(dbGetQuery(con, ...), error = function(e)
# NULL)` dentro de una funcion que recibe `con`).
.fallo_del_controlador_dbi <- function(e) {
  structure(list(condicion = e), class = "lupa_fallo_del_controlador")
}

.nulo_ante_el_fallo_dbi <- function(e) NULL

# Reconocer un corte cuesta la conexion entera, asi que el texto solo no
# alcanza donde el controlador puede confirmarlo sin mandar nada. Medido
# despues de un corte real y despues de un error que solo lleva el texto -el
# `RAISE(ABORT, ...)` de un disparador, la funcion `error()`, la extension que
# no se pudo cargar-: RSQLite y duckdb contestan `dbIsValid()` `FALSE` en el
# primero y `TRUE` en el segundo. RPostgres, RMariaDB, RMySQL y odbc contestan
# `TRUE` las dos veces -es una bandera del cliente, no el estado del enchufe-:
# ahi no confirma nada y no se pregunta. En RPostgres decide la gravedad; en
# RMariaDB y RMySQL, el numero del error; con odbc y FreeTDS no hay nada mas
# que el texto, y no reconocer un corte real hace caer a R en la sentencia
# siguiente.
.CONTROLADORES_QUE_CONFIRMAN_DBI <- c("RSQLite", "duckdb")

.corte_desmentido_dbi <- function(controlador, conexion) {
  if (!isTRUE(controlador %in% .CONTROLADORES_QUE_CONFIRMAN_DBI)) return(FALSE)
  isTRUE(tryCatch(DBI::dbIsValid(conexion), error = .nulo_ante_el_fallo_dbi))
}

# Lo que dice el error, con la confirmacion del controlador donde la hay.
.reconocer_en_conexion_dbi <- function(controlador, conexion, condicion,
                                       excepcion = NULL) {
  dicho <- .reconocer_error_dbi(controlador, condicion, excepcion)
  if (identical(dicho, "conexion") &&
      .corte_desmentido_dbi(controlador, conexion)) {
    return("rechazada")
  }
  dicho
}

.dbi <- function(operacion, conexion, llamada) {
  corrida <- .corrida_dbi(conexion)
  recordada <- if (is.null(corrida)) .corte_recordado_dbi(conexion)
  if (isTRUE(corrida$perdida) || !is.null(recordada)) {
    if (operacion %in% .OPERACIONES_SIN_CONTROLADOR_DBI) {
      return(!identical(operacion, "dbIsValid"))
    }
    .cortar_dbi(corrida, conexion, motivo = recordada)
  }
  if (!is.null(corrida)) corrida$llamadas <- corrida$llamadas + 1L
  valor <- tryCatch(llamada, error = .fallo_del_controlador_dbi)
  fallo <- if (inherits(valor, "lupa_fallo_del_controlador")) valor$condicion
  if (!is.null(fallo)) {
    # El tope de tiempo de R no es una respuesta del motor.
    if (.es_interrupcion_dbi(fallo)) .detener(fallo)
    controlador <- if (is.null(corrida)) {
      .controlador_dbi(conexion)
    } else corrida$controlador
    reconocido <- .reconocer_en_conexion_dbi(
      controlador, conexion, fallo,
      .excepcion_cliente_dbi(controlador, conexion)
    )
    if (identical(reconocido, "conexion") && .cancelo_resultado_abierto_dbi(
      corrida, controlador, operacion, fallo
    )) {
      reconocido <- "rechazada"
    }
    if (identical(operacion, "dbClearResult") && !is.null(corrida)) {
      corrida$abiertos <- max(0L, corrida$abiertos - 1L)
    }
    # Mientras la corrida se abre, una conexion que no sirve no se perdio en
    # ella: el error sigue y lo decide quien pregunto.
    if (identical(reconocido, "conexion") && !isTRUE(corrida$abriendo)) {
      .cortar_dbi(corrida, conexion, fallo)
    }
    # Un error que el reconocedor no toma por conexion perdida puede venir de
    # una sesion que el controlador reabrio: la tabla temporal que ya no esta,
    # el ajuste de sesion que se perdio.
    if (.sesion_cambio_dbi(corrida)) {
      .cortar_dbi(corrida, conexion, motivo = .MOTIVO_SESION_REABIERTA_DBI)
    }
    # La transaccion de quien llama ya estaba abortada al entrar: es lo
    # primero que el motor dice en la corrida, y lo diria de toda sentencia.
    # Sin esto cada puerta de entrada lo contaba a su manera -"la tabla no
    # existe o no hay permiso", o una coleccion con todas sus tablas
    # fallidas-, que es falso. Si en esta corrida ya contesto una sentencia, o
    # ya fallo otra, la transaccion se aborto despues y vale lo de siempre.
    if (identical(reconocido, "transaccion") && !is.null(corrida) &&
        !isTRUE(corrida$abriendo) && !isTRUE(corrida$contesto) &&
        !isTRUE(corrida$fallo)) {
      .cortar_dbi(corrida, conexion, fallo, causa = "transaccion")
    }
    if (!is.null(corrida)) corrida$fallo <- TRUE
    attr(fallo, "lupa_reconocido") <- reconocido
    .detener(fallo)
  }
  if (!is.null(corrida)) {
    if (operacion %in% .OPERACIONES_QUE_CONTESTAN_DBI) corrida$contesto <- TRUE
    if (identical(operacion, "dbSendQuery")) {
      corrida$abiertos <- corrida$abiertos + 1L
    } else if (identical(operacion, "dbClearResult")) {
      corrida$abiertos <- max(0L, corrida$abiertos - 1L)
    }
    if (operacion %in% .OPERACIONES_QUE_PRUEBAN_DBI ||
        (identical(operacion, "dbIsValid") && isTRUE(valor))) {
      corrida$probada <- TRUE
    } else if (identical(operacion, "dbIsValid") && identical(valor, FALSE) &&
               isTRUE(corrida$probada)) {
      # Invalida al entrar es el error de siempre; invalida despues de haber
      # servido en esta misma corrida -contesto una sentencia, o dijo que era
      # valida-, se perdio en el medio. `dbIsValid()` se pregunta siempre por
      # la conexion, nunca por un resultado: un resultado ya cerrado es
      # invalido con la conexion sana.
      .cortar_dbi(corrida, conexion, motivo = .MOTIVO_CONEXION_INVALIDA_DBI)
    } else if (identical(operacion, "dbClearResult") &&
               .sesion_cambio_dbi(corrida)) {
      # RPostgres cierra sin error un resultado con filas pendientes aunque el
      # servidor haya cortado la sesion, y la reabre: el identificador del
      # cliente es lo unico que lo dice.
      .cortar_dbi(corrida, conexion, motivo = .MOTIVO_SESION_REABIERTA_DBI)
    }
  }
  valor
}

# Con odbc y FreeTDS, mandar una sentencia mientras otro resultado sigue
# abierto cancela ese resultado y la sentencia falla con el error crudo
# `Unknown error`: el mismo texto que da la primera sentencia sobre una
# conexion muerta. Pasa en una conexion sana -la vigilancia de la transaccion
# en SQL Server pregunta `@@TRANCOUNT` con el resultado de la consulta todavia
# abierto, y esa pregunta fallida se tolera-: tomarlo por conexion perdida
# cortaba `plan_perfilado_dbi(compilar = TRUE)` dentro de una transaccion del
# usuario, sobre una conexion que servia. Con un resultado abierto, ese error
# en una sentencia NUEVA no dice nada de la conexion; en la lectura del
# resultado, o sin ninguno abierto, si.
.OPERACIONES_QUE_MANDAN_DBI <- c("dbGetQuery", "dbSendQuery", "dbExecute")

.cancelo_resultado_abierto_dbi <- function(corrida, controlador, operacion,
                                           fallo) {
  if (is.null(corrida) || !identical(controlador, "odbc") ||
      !operacion %in% .OPERACIONES_QUE_MANDAN_DBI ||
      !isTRUE(corrida$abiertos > 0L)) {
    return(FALSE)
  }
  partes <- .partes_error_dbi("odbc", fallo)
  isTRUE(partes$crudo) && grepl(
    "^Unknown error", partes$texto, perl = TRUE, useBytes = TRUE
  )
}

# Lo que el reconocedor dijo de un error que paso por `.dbi()`.
.reconocido_dbi <- function(condicion) {
  valor <- tryCatch(
    attr(condicion, "lupa_reconocido", exact = TRUE), error = function(e) NULL
  )
  if (length(valor) == 1L && is.character(valor) && !is.na(valor)) {
    valor
  } else NA_character_
}

# Lo que se sabe de un error atrapado sobre una conexion: lo que dijo el punto
# unico si el error paso por el, o el reconocedor sobre la condicion.
.reconocer_falla_dbi <- function(conexion, condicion) {
  dicho <- .reconocido_dbi(condicion)
  if (!is.na(dicho)) return(dicho)
  if (!inherits(condicion, "condition") ||
      inherits(condicion, c("lupa_error_dbi", "lupa_aviso_dbi"))) {
    return(NA_character_)
  }
  tryCatch(
    .reconocer_en_conexion_dbi(.controlador_dbi(conexion), conexion, condicion),
    error = function(e) NA_character_
  )
}

# RMySQL no escribe el codigo en el mensaje: lo deja del lado del cliente, y es
# lo unico que dice que paso cuando un fetch cortado solo avisa.
.excepcion_cliente_dbi <- function(controlador, conexion) {
  if (!identical(controlador, "RMySQL")) return(NULL)
  tryCatch(DBI::dbGetException(conexion), error = .nulo_ante_el_fallo_dbi)
}

# ---- El reconocedor ------------------------------------------------------
#
# Que dice el error de un controlador, leido donde el controlador lo escribe:
# el codigo estructurado si la condicion lo trae; si no, el codigo o la frase
# del encabezado, con el SQL pegado y los literales entre comillas quitados
# antes. Un nombre de columna, un literal o un dato con `57014`, `1969`,
# "connection reset" o "canceling statement" no deciden nada: se leyeron
# clasificados como cancelacion, y una conexion sana tomada por perdida.
#
# Formas medidas (RPostgres 1.4.10 con PostgreSQL 16 y 9.3; RMariaDB 1.3.5 con
# MariaDB y MySQL; RMySQL 0.11.3; odbc 1.7.0 con FreeTDS; duckdb 2.0.0;
# RSQLite):
#
# * RPostgres: `simpleError` sin codigo, `Failed to <prepare query|fetch row> :
#   <GRAVEDAD>:  <linea primaria>` y despues `LINE n:` con el SQL. Una caida de
#   red no trae gravedad: `server closed the connection unexpectedly`.
# * RMariaDB: `<texto> [<numero>]`, con el numero al FINAL del mensaje.
# * RMySQL: `could not run statement: <texto>`, sin numero; el numero queda en
#   `dbGetException()`.
# * odbc: una condicion armada con el estado en `message` y el texto del motor
#   en `body["x"]` (el SQL va aparte, en `body["*"]`), o el error crudo de
#   nanodbc `nanodbc/nanodbc.cpp:<linea>: <estado>` y en la linea siguiente el
#   texto. FreeTDS pone siempre el estado `00000`: ahi decide el texto.
# * duckdb: `error_type` y `raw_message` como campos.
# * RSQLite: `simpleError`, solo texto.
#
# Devuelve `conexion`, `cancelada`, `permiso`, `transaccion`, `transitoria`,
# `capacidad` o `rechazada`. Limites que no se pueden cerrar desde aca, dichos
# en la ayuda: RPostgres con el servidor en un idioma que no sea ingles o
# castellano, y FreeTDS, que no entrega el estado.
.primer_campo_condicion_dbi <- function(condicion, campos) {
  for (campo in campos) {
    valor <- tryCatch(condicion[[campo]], error = function(e) NULL)
    if (is.character(valor) && length(valor) >= 1L && !is.na(valor[[1L]]) &&
        nzchar(valor[[1L]])) {
      return(valor[[1L]])
    }
  }
  NULL
}

.primera_linea_dbi <- function(texto) {
  if (length(texto) != 1L || is.na(texto)) return("")
  lineas <- strsplit(texto, "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  if (length(lineas)) lineas[[1L]] else ""
}

# FreeTDS antepone `[FreeTDS][SQL Server]` al texto del motor. Si TODO el texto
# son corchetes, el mensaje es el ultimo; un corchete en el medio es del
# usuario -SQL Server cita asi los nombres- y no se toca.
.sin_corchetes_odbc_dbi <- function(texto) {
  if (length(texto) != 1L || is.na(texto)) return("")
  texto <- trimws(texto)
  resto <- trimws(sub("^(\\[[^]]*\\])+", "", texto, perl = TRUE, useBytes = TRUE))
  if (nzchar(resto)) return(resto)
  ultimo <- regmatches(
    texto, regexec("\\[([^]]*)\\]$", texto, perl = TRUE, useBytes = TRUE)
  )[[1L]]
  if (length(ultimo) == 2L) ultimo[[2L]] else ""
}

.captura_dbi <- function(patron, texto) {
  partes <- regmatches(
    texto, regexec(patron, texto, perl = TRUE, useBytes = TRUE)
  )[[1L]]
  if (length(partes) > 1L) partes[-1L] else NULL
}

.partes_error_dbi <- function(controlador, condicion, excepcion = NULL) {
  mensaje <- tryCatch(
    paste(conditionMessage(condicion), collapse = "\n"),
    error = function(e) ""
  )
  if (is.na(mensaje)) mensaje <- ""
  partes <- list(
    estado = NA_character_, codigo = NA_character_, gravedad = NA_character_,
    texto = "", crudo = FALSE
  )
  estado <- .primer_campo_condicion_dbi(
    condicion, c("sqlstate", "SQLSTATE", "sqlState", "state")
  )
  if (!is.null(estado)) partes$estado <- .mayusculas_ascii(estado)
  if (inherits(condicion, "duckdb_error")) {
    tipo <- .primer_campo_condicion_dbi(condicion, "error_type")
    if (!is.null(tipo)) partes$codigo <- .mayusculas_ascii(tipo)
    crudo <- .primer_campo_condicion_dbi(condicion, "raw_message")
    partes$texto <- .primera_linea_dbi(if (is.null(crudo)) mensaje else crudo)
    return(partes)
  }
  if (inherits(condicion, "odbc_database_error")) {
    encabezado <- paste(
      tryCatch(as.character(condicion$message), error = function(e) ""),
      collapse = " "
    )
    estado <- .captura_dbi("error ([0-9A-Z]{5}) from", encabezado)
    if (!is.null(estado) && !identical(estado[[1L]], "00000")) {
      partes$estado <- estado[[1L]]
    }
    cuerpo <- tryCatch(condicion$body, error = function(e) NULL)
    del_motor <- if (is.character(cuerpo) && !is.null(names(cuerpo))) {
      cuerpo[names(cuerpo) == "x"]
    } else character()
    if (length(del_motor) && !is.na(del_motor[[1L]])) {
      partes$texto <- .sin_corchetes_odbc_dbi(unname(del_motor[[1L]]))
      return(partes)
    }
    # Sin la linea del motor: el error crudo que odbc guarda como causa.
    padre <- tryCatch(condicion$parent, error = function(e) NULL)
    if (inherits(padre, "condition")) {
      del_padre <- .partes_error_dbi("odbc", padre)
      if (is.na(del_padre$estado)) del_padre$estado <- partes$estado
      return(del_padre)
    }
    return(partes)
  }
  if (identical(controlador, "odbc")) {
    lineas <- strsplit(mensaje, "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
    estado <- if (length(lineas)) .captura_dbi(
      "^nanodbc/nanodbc\\.cpp:[0-9]+: ([0-9A-Z]{5})", lineas[[1L]]
    )
    if (!is.null(estado)) {
      partes$crudo <- TRUE
      if (!identical(estado[[1L]], "00000")) partes$estado <- estado[[1L]]
      partes$texto <- if (length(lineas) >= 2L) {
        .sin_corchetes_odbc_dbi(lineas[[2L]])
      } else ""
    }
    return(partes)
  }
  if (identical(controlador, "RMariaDB")) {
    final <- .captura_dbi("^([\\s\\S]*) \\[([0-9]+)\\]$", mensaje)
    if (!is.null(final)) {
      partes$codigo <- final[[2L]]
      partes$texto <- .primera_linea_dbi(final[[1L]])
    } else partes$texto <- .primera_linea_dbi(mensaje)
    return(partes)
  }
  if (identical(controlador, "RMySQL")) {
    partes$texto <- sub(
      "^could not run statement: ", "", mensaje, perl = TRUE, useBytes = TRUE
    )
    numero <- tryCatch(excepcion$errorNum, error = function(e) NULL)
    dicho <- tryCatch(excepcion$errorMsg, error = function(e) NULL)
    # El codigo del cliente vale solo si es el de ESTE error: su texto es el
    # del mensaje, o el controlador solo aviso que el fetch se corto.
    if (length(numero) == 1L && !is.na(numero) && isTRUE(numero > 0) &&
        is.character(dicho) && length(dicho) == 1L && !is.na(dicho) && (
          endsWith(partes$texto, dicho) ||
            identical(partes$texto, "error while fetching rows")
        )) {
      partes$codigo <- as.character(as.integer(numero))
    }
    partes$texto <- .primera_linea_dbi(partes$texto)
    return(partes)
  }
  if (identical(controlador, "RPostgres")) {
    primera <- .primera_linea_dbi(sub(
      "^[A-Za-z ]{1,40} : ", "", mensaje, perl = TRUE, useBytes = TRUE
    ))
    gravedad <- .captura_dbi(
      "^([^:[:space:]]{3,12}):[[:space:]]+(.*)$", primera
    )
    if (is.null(gravedad)) {
      partes$texto <- primera
    } else {
      partes$gravedad <- gravedad[[1L]]
      partes$texto <- gravedad[[2L]]
    }
    return(partes)
  }
  partes$texto <- .primera_linea_dbi(mensaje)
  partes
}

# Un estado SQLSTATE estructurado dice lo mismo en cualquier motor.
.reconocer_estado_dbi <- function(estado) {
  if (length(estado) != 1L || is.na(estado) ||
      !grepl("^[0-9A-Z]{5}$", estado, perl = TRUE, useBytes = TRUE)) {
    return(NA_character_)
  }
  if (startsWith(estado, "08") ||
      estado %in% c("57P01", "57P02", "57P03")) return("conexion")
  if (estado %in% c("57014", "HYT00", "HY008")) return("cancelada")
  if (identical(estado, "42501")) return("permiso")
  if (estado %in% c("40001", "40P01", "55P03")) return("transitoria")
  if (identical(estado, "25P02")) return("transaccion")
  if (estado %in% c("42883", "42601", "42704", "0A000")) return("capacidad")
  NA_character_
}

.reconocer_error_dbi <- function(controlador, condicion, excepcion = NULL) {
  partes <- .partes_error_dbi(controlador, condicion, excepcion)
  por_estado <- .reconocer_estado_dbi(partes$estado)
  if (!is.na(por_estado)) return(por_estado)
  # Lo que va entre comillas es del usuario: un nombre, un literal, un dato.
  # Salvo que TODO el texto sea una sola cita: ahi no hay nada del usuario
  # dentro de un mensaje del motor, es el mensaje entero citado por una capa
  # intermedia, y quitarlo dejaba sin reconocer una conexion perdida.
  texto <- trimws(partes$texto)
  entero <- regmatches(texto, regexec(
    "^(?:\"([^\"]*)\"|'([^']*)'|`([^`]*)`)$", texto,
    perl = TRUE, useBytes = TRUE
  ))[[1L]]
  if (length(entero) == 4L) {
    texto <- paste(entero[-1L], collapse = "")
  }
  texto <- gsub(
    "\"[^\"]*\"|'[^']*'|`[^`]*`", "\"\"", texto,
    perl = TRUE, useBytes = TRUE
  )
  codigo <- partes$codigo
  es <- function(patron) grepl(patron, texto, perl = TRUE, useBytes = TRUE)
  if (identical(controlador, "RPostgres")) {
    # Un mensaje con gravedad lo informo el servidor, y si pudo informarlo con
    # `ERROR` la sesion sigue: solo `FATAL` y `PANIC` la terminan. Con `ERROR`
    # no es un corte, diga lo que diga su texto: una funcion del usuario, o una
    # tabla foranea cuyo servidor remoto se cayo, levantan `ERROR:  server
    # closed the connection unexpectedly` con la sesion local sana (medido), y
    # tomarlo por corte dejaba inservible una conexion que servia. Las frases
    # de corte valen solo SIN gravedad: las escribe el cliente cuando se le cae
    # el enchufe, y nada de lo que hay en la base llega ahi.
    if (!is.na(partes$gravedad)) {
      if (partes$gravedad %in% c("FATAL", "PANIC")) return("conexion")
    } else if (es(paste0(
      "^(server closed the connection unexpectedly|",
      "no connection to the server|could not (send|receive) data|",
      "SSL connection has been closed|connection to server was lost|",
      "Lost connection to database)"
    ))) return("conexion")
    if (es("^(canceling statement due to|cancelando la sentencia debido a)")) {
      return("cancelada")
    }
    if (es("^(permission denied for|permiso denegado)")) return("permiso")
    if (es(paste0(
      "^(deadlock detected|could not serialize access|",
      "out of (shared )?memory)"
    ))) return("transitoria")
    if (es("^current transaction is aborted")) return("transaccion")
    if (es(paste0(
      "^(function .* does not exist$|syntax error at or near|",
      "operator does not exist|type .* does not exist)"
    ))) return("capacidad")
    return("rechazada")
  }
  if (controlador %in% c("RMariaDB", "RMySQL")) {
    if (!is.na(codigo)) {
      if (codigo %in% c("2013", "2006", "2002", "2003", "2055", "1927",
                        "4031")) return("conexion")
      if (codigo %in% c("1317", "1969", "3024")) return("cancelada")
      if (codigo %in% c("1142", "1143", "1044", "1227", "1370")) {
        return("permiso")
      }
      if (codigo %in% c("1205", "1213")) return("transitoria")
      if (codigo %in% c("1305", "1064", "1630", "1582", "1583", "1584",
                        "1235")) return("capacidad")
      return("rechazada")
    }
    if (es(paste0(
      "^(Lost connection to (MySQL )?server|MySQL server has gone away|",
      "The client was disconnected by the server because of inactivity)"
    ))) return("conexion")
    if (es("^Query execution was interrupted")) return("cancelada")
    if (es(paste0(
      "^[A-Z ]+ command denied to user|^Access denied; you need|",
      "^execute command denied"
    ))) return("permiso")
    return("rechazada")
  }
  if (identical(controlador, "odbc")) {
    if (es(paste0(
      "^(Communication link failure|Unexpected EOF from the server|",
      "Cannot continue the execution because the session is in the kill ",
      "state|Write to the server failed|Read from the server failed)"
    ))) return("conexion")
    if (isTRUE(partes$crudo) && es("^Unknown error")) return("conexion")
    if (es("^(Query timeout expired|Operation canceled)")) return("cancelada")
    if (es("^(The [A-Z]+ permission was denied|Se deneg.{1,2} el permiso)")) {
      return("permiso")
    }
    if (es(paste0(
      "^(Lock request time out period exceeded|",
      "Transaction \\(Process ID [0-9]+\\) was deadlocked)"
    ))) return("transitoria")
    if (es(paste0(
      "is not a recognized built-in function name\\.$|",
      "^Incorrect syntax near|must have an OVER clause\\.$"
    ))) return("capacidad")
    return("rechazada")
  }
  if (identical(controlador, "duckdb")) {
    if (es("^Invalid connection")) return("conexion")
    if (isTRUE(codigo == "INTERRUPT")) return("cancelada")
    if (isTRUE(codigo == "TRANSACTION")) return("transaccion")
    # Capacidad ausente es solo la funcion que no existe, la firma que no casa
    # y la sintaxis que el motor no entiende. Una columna o una tabla que no
    # existe -tambien `CATALOG` y `BINDER`- es una consulta mal armada.
    if (isTRUE(codigo == "CATALOG") &&
        es("^([A-Za-z]+ )?Function with name .* does not exist")) {
      return("capacidad")
    }
    if (isTRUE(codigo == "BINDER") && es("^No function matches")) {
      return("capacidad")
    }
    if (isTRUE(codigo %in% c("PARSER", "NOT_IMPLEMENTED"))) return("capacidad")
    return("rechazada")
  }
  if (identical(controlador, "RSQLite")) {
    # `Invalid result set` es un resultado ya cerrado, con la conexion sana.
    if (es("^Invalid or closed connection")) return("conexion")
    if (es("^interrupted$")) return("cancelada")
    if (es(paste0(
      "^no such function:|^near .*: syntax error|",
      "^wrong number of arguments to function"
    ))) return("capacidad")
    return("rechazada")
  }
  "rechazada"
}

# ---- La transaccion abierta de DuckDB ------------------------------------
#
# En DuckDB cualquier error del motor -tambien el de una funcion que no existe,
# que es la respuesta esperada de un sondeo de capacidad- aborta la transaccion
# abierta sin avisar, el `dbCommit()` siguiente devuelve `TRUE` y pierde lo
# hecho en ella, y no hay `SAVEPOINT` para aislarlo (medido con duckdb 2.0.0).
# Con una transaccion abierta, o sin poder saberlo, la corrida se niega antes de
# mandar nada mas.
#
# Se sabe con `txid_current()` dos veces: fuera de una transaccion cada
# sentencia es la suya y el identificador cambia; adentro es el mismo. No se usa
# `now()`: con `tz_out_convert = "force"` llega truncado al segundo, dos
# lecturas seguidas dan igual y una conexion sin transaccion se rechazaba. El
# identificador se pide como texto para que no dependa de como la conexion
# entrega los enteros de 64 bits. Son las dos unicas sentencias que la via manda
# de mas, solo en DuckDB, y el plan las cuenta.
.SQL_TRANSACCION_DUCKDB_DBI <-
  "SELECT CAST(txid_current() AS VARCHAR) AS lupa_transaccion"

.transaccion_abierta_duckdb_dbi <- function(conexion) {
  leer <- function() {
    as.character(.orden_transaccion_dbi(
      conexion, .SQL_TRANSACCION_DUCKDB_DBI, traer = TRUE
    )[[1L]][[1L]])
  }
  primera <- NULL
  lectura <- tryCatch({
    primera <- leer()
    list(primera, leer())
  }, error = function(e) e)
  if (inherits(lectura, "error")) {
    if (.es_interrupcion_dbi(lectura)) .detener(lectura)
    if (identical(.reconocido_dbi(lectura), "conexion")) {
      # Si la primera lectura contesto, la conexion servia y se perdio aca.
      if (!is.null(primera)) {
        .cortar_dbi(.corrida_dbi(conexion), conexion, lectura)
      }
      # Si no, no servia al entrar: el error de siempre, el de la validacion.
      return(NA)
    }
    .detener_dbi(
      "lupa_error_transaccion_dbi",
      paste0(
        "La v\u00eda DBI rechaza la corrida: no se pudo saber si la conexi\u00f3n tiene ",
        "una transacci\u00f3n abierta, porque la consulta de `txid_current()` ",
        "fall\u00f3, y sin saberlo la lectura podr\u00eda tocarla. Si hay una ",
        "transacci\u00f3n abortada, cerrarla con `DBI::dbRollback()`; si est\u00e1 ",
        "abierta y se quiere conservar, cerrarla con `DBI::dbCommit()`. Motivo ",
        "del motor: ", .mensaje_motor(lectura)
      )
    )
  }
  isTRUE(length(lectura[[1L]]) == 1L && !is.na(lectura[[1L]]) &&
           identical(lectura[[1L]], lectura[[2L]]))
}

.MENSAJE_TRANSACCION_DUCKDB_DBI <- paste(
  "La v\u00eda DBI rechaza la corrida dentro de una transacci\u00f3n abierta de",
  "DuckDB: un error del motor -tambi\u00e9n el de una sentencia que el motor",
  "rechaza, como las de un sondeo de capacidad- la aborta sin aviso y un",
  "`DBI::dbCommit()` posterior pierde lo hecho en ella, y DuckDB no tiene",
  "`SAVEPOINT` para aislarla. Cerrarla antes con `DBI::dbCommit()` o con",
  "`DBI::dbRollback()`, o leer por otra conexi\u00f3n."
)

.negar_transaccion_duckdb_dbi <- function(corrida) {
  conexion <- corrida$conexion
  abierta <- .transaccion_abierta_duckdb_dbi(conexion)
  if (is.na(abierta)) return(invisible(NULL))
  corrida$guarda_duckdb <- 2L
  if (abierta) {
    .detener_dbi(
      "lupa_error_transaccion_dbi", .MENSAJE_TRANSACCION_DUCKDB_DBI
    )
  }
  corrida$sin_transaccion_duckdb <- TRUE
  invisible(NULL)
}

# Las dos sentencias de la guarda las manda la corrida al abrirse, una vez. Las
# cuenta la llamada que abrio la corrida, si es de una tabla o de una medicion:
# su plan las anuncia y su tope las descuenta. En una coleccion se mandan una
# vez para todas las tablas y no entran en la cuenta de ninguna.
.ACCIONES_PERFIL_DBI <- c("perfilar una tabla", "planificar un perfil")
.ACCIONES_MEDICION_DBI <- c("medir", "planificar una medici\u00f3n")

.guarda_duckdb_contada_dbi <- function(conexion, acciones) {
  corrida <- .corrida_dbi(conexion)
  if (is.null(corrida) || !corrida$accion %in% acciones) return(0L)
  as.integer(corrida$guarda_duckdb)
}
