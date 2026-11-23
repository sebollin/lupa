# El punto unico de la via DBI, por construccion.
#
# Que una conexion perdida corte la llamada entera depende de tres propiedades
# del codigo, y ninguna se ve corriendolo sobre una tabla sana:
#
# 1. Toda llamada al controlador esta escrita dentro de un `.dbi(...)`: una
#    sola que quede afuera no ve el pestillo, y con odbc la sentencia siguiente
#    a un corte hace caer a R.
# 2. Nadie atrapa el corte: la condicion no hereda de `error`, asi que solo una
#    captura general de `condition` podria tragarsela.
# 3. La limpieza -un `on.exit()` que cierra un resultado o vuelve a un punto
#    de guardado- llega al controlador solo por el punto unico.
#
# Se recorre el arbol sintactico de cada funcion del espacio de nombres; no se
# busca texto.
testthat::skip_on_cran()

.pu_espacio <- function() asNamespace("lupa")

.pu_funciones <- function() {
  espacio <- .pu_espacio()
  nombres <- ls(espacio, all.names = TRUE)
  objetos <- lapply(nombres, get, envir = espacio)
  son <- vapply(objetos, function(x) is.function(x) && !is.primitive(x),
                logical(1L))
  stats::setNames(objetos[son], nombres[son])
}

# El nombre de la funcion llamada: `f(...)`, `paquete::f(...)`.
.pu_cabeza <- function(e) {
  cabeza <- e[[1L]]
  if (is.name(cabeza)) return(as.character(cabeza))
  if (is.call(cabeza) && length(cabeza) == 3L &&
      (identical(cabeza[[1L]], as.name("::")) ||
         identical(cabeza[[1L]], as.name(":::")))) {
    return(as.character(cabeza[[3L]]))
  }
  ""
}

.pu_hijos <- function(e) {
  if (is.call(e) || is.pairlist(e) || is.expression(e)) {
    hijos <- as.list(e)
    hijos[!vapply(hijos, function(x) identical(x, quote(expr = )), logical(1L))]
  } else list()
}

# Las llamadas que viajan al controlador o le preguntan por su estado.
.PU_CONTROLADOR <- c(
  "dbGetQuery", "dbSendQuery", "dbSendStatement", "dbExecute", "dbFetch",
  "dbClearResult", "dbColumnInfo", "dbHasCompleted", "dbExistsTable",
  "dbListFields", "dbListTables", "dbReadTable", "dbWriteTable",
  "dbGetInfo", "dbIsValid", "dbGetException", "dbBegin", "dbCommit",
  "dbRollback", "dbGetRowCount", "dbGetRowsAffected", "dbBind",
  "postgresIsTransacting", ".jcall"
)

# Las que se hacen sobre la conexion: su primer argumento tiene que ser la
# conexion que `.dbi()` recibe, que es la del pestillo.
.PU_SOBRE_LA_CONEXION <- c(
  "dbGetQuery", "dbSendQuery", "dbExecute", "dbExistsTable", "dbListFields",
  "dbGetInfo", "dbIsValid", "postgresIsTransacting"
)

test_that("toda llamada al controlador esta escrita dentro de `.dbi()`", {
  fuera <- character()
  operaciones <- character()
  mal_armadas <- character()
  recorrer <- function(e, nombre, dentro) {
    if (!is.call(e)) {
      for (hijo in .pu_hijos(e)) recorrer(hijo, nombre, dentro)
      return(invisible())
    }
    cabeza <- .pu_cabeza(e)
    if (cabeza %in% .PU_CONTROLADOR && !dentro) {
      fuera <<- c(fuera, paste(nombre, cabeza))
    }
    if (identical(cabeza, ".dbi") && length(e) == 4L) {
      operacion <- e[[2L]]
      llamada <- e[[4L]]
      if (!is.character(operacion) || !is.call(llamada)) {
        mal_armadas <<- c(mal_armadas, nombre)
      } else {
        operaciones <<- c(operaciones, operacion)
        interna <- .pu_cabeza(llamada)
        esperada <- if (identical(operacion, "jcall")) ".jcall" else operacion
        if (!identical(interna, esperada)) {
          mal_armadas <<- c(mal_armadas, paste(nombre, operacion, interna))
        } else if (operacion %in% .PU_SOBRE_LA_CONEXION &&
                   !identical(llamada[[2L]], e[[3L]])) {
          # Se pregunta por otra cosa que la conexion del pestillo: un
          # resultado ya cerrado es invalido con la conexion sana.
          mal_armadas <<- c(mal_armadas, paste(nombre, operacion, "otro objeto"))
        }
      }
      recorrer(e[[3L]], nombre, dentro)
      recorrer(llamada, nombre, TRUE)
      return(invisible())
    }
    for (hijo in .pu_hijos(e)) recorrer(hijo, nombre, dentro)
    invisible()
  }
  funciones <- .pu_funciones()
  for (nombre in names(funciones)) {
    recorrer(body(funciones[[nombre]]), nombre, FALSE)
    recorrer(formals(funciones[[nombre]]), nombre, FALSE)
  }
  # Las unicas fuera son del propio punto unico: la confirmacion de un corte
  # donde el controlador la da sin mandar nada, el codigo que RMySQL deja del
  # lado del cliente, la lectura con que la corrida abre y la comparacion del
  # identificador de sesion.
  expect_identical(
    .sort_por_bytes(unique(fuera)),
    c(".corte_desmentido_dbi dbIsValid",
      ".excepcion_cliente_dbi dbGetException",
      ".leer_info_corrida_dbi dbGetInfo",
      ".sesion_cambio_dbi dbGetInfo")
  )
  expect_identical(mal_armadas, character())
  # El recorrido ve los sitios: no pasa por no encontrar nada.
  expect_gte(length(operaciones), 25L)
  expect_true(all(c(
    "dbGetQuery", "dbSendQuery", "dbExecute", "dbFetch", "dbClearResult",
    "dbColumnInfo", "dbHasCompleted", "dbExistsTable", "dbListFields",
    "dbGetInfo", "dbIsValid"
  ) %in% operaciones))
})

# Los paquetes cuyas funciones le hablan a una conexion.
.PU_PAQUETES <- c(
  "DBI", "odbc", "RPostgres", "RMariaDB", "RMySQL", "RSQLite", "duckdb",
  "rJava", "RJDBC", "bigrquery", "pool", "dbplyr"
)

# Las que no viajan: arman un texto o un nombre del lado del cliente.
.PU_DEL_CLIENTE <- c("Id", "dbQuoteIdentifier", "dbQuoteString")

# El nombre de la funcion en `paquete::funcion`, si el paquete es de esos.
.pu_referencia <- function(e) {
  if (is.call(e) && length(e) == 3L &&
      (identical(e[[1L]], as.name("::")) ||
         identical(e[[1L]], as.name(":::"))) &&
      is.name(e[[2L]]) && as.character(e[[2L]]) %in% .PU_PAQUETES) {
    as.character(e[[3L]])
  } else {
    NA_character_
  }
}

test_that("toda funcion de DBI o de un controlador que el paquete usa esta clasificada", {
  # La prueba de arriba parte de una lista de funciones que viajan: una que no
  # estuviera en la lista quedaria fuera del punto unico sin que nadie la vea.
  # Aca se enumeran todas las referencias `paquete::funcion` a DBI y a los
  # controladores, y cada nombre tiene que estar en una de las dos listas: las
  # que viajan -y entonces la prueba de arriba les exige el punto unico- o las
  # que arman texto del lado del cliente. Una funcion nueva hace fallar esta
  # prueba hasta que se la clasifique.
  nombres <- character()
  sueltas <- character()
  recorrer <- function(e, nombre, cabeza) {
    referencia <- .pu_referencia(e)
    if (!is.na(referencia)) {
      nombres <<- c(nombres, referencia)
      if (!cabeza) sueltas <<- c(sueltas, paste(nombre, referencia))
      return(invisible())
    }
    hijos <- .pu_hijos(e)
    for (i in seq_along(hijos)) {
      recorrer(hijos[[i]], nombre, is.call(e) && i == 1L)
    }
    invisible()
  }
  funciones <- .pu_funciones()
  for (nombre in names(funciones)) {
    recorrer(body(funciones[[nombre]]), nombre, FALSE)
    recorrer(formals(funciones[[nombre]]), nombre, FALSE)
  }
  expect_identical(
    setdiff(unique(nombres), c(.PU_CONTROLADOR, .PU_DEL_CLIENTE)), character()
  )
  # Una funcion que viaja no se pasa como valor -`do.call(DBI::dbGetQuery,
  # ...)`-: la prueba de arriba mira la cabeza de cada llamada y no la veria.
  expect_identical(
    sueltas[sub("^.* ", "", sueltas) %in% .PU_CONTROLADOR], character()
  )
  # El recorrido ve las referencias: no pasa por no encontrar nada.
  expect_gte(length(nombres), 60L)
  expect_true(all(.PU_DEL_CLIENTE %in% nombres))
})

test_that("ninguna captura general puede tragarse el corte", {
  capturas_generales <- character()
  capturas_del_corte <- character()
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- .pu_cabeza(e)
      if (cabeza %in% c("tryCatch", "withCallingHandlers", "try")) {
        argumentos <- names(e)
        if ("lupa_corte_conexion" %in% argumentos) {
          capturas_del_corte <<- c(capturas_del_corte, nombre)
        }
        if ("condition" %in% argumentos) {
          manejador <- e[["condition"]]
          llamadas <- if (is.call(manejador)) all.names(manejador) else ""
          vuelve_a_senalar <- if (identical(cabeza, "tryCatch")) {
            # Una captura que sale del `tryCatch()` tiene que volver a
            # levantar la condicion que recibio.
            any(c(".detener", "stop", "signalCondition") %in% llamadas)
          } else {
            # Un manejador que solo mira deja seguir la condicion, salvo que
            # la desvie a un reinicio.
            !any(c("invokeRestart", "tryInvokeRestart", "computeRestarts") %in%
                   llamadas)
          }
          if (!vuelve_a_senalar) {
            capturas_generales <<- c(capturas_generales, nombre)
          }
        }
      }
    }
    for (hijo in .pu_hijos(e)) recorrer(hijo, nombre)
    invisible()
  }
  funciones <- .pu_funciones()
  for (nombre in names(funciones)) recorrer(body(funciones[[nombre]]), nombre)
  expect_identical(capturas_generales, character())
  # El corte se convierte en un solo lugar: el borde de la corrida.
  expect_identical(capturas_del_corte, ".con_corrida_dbi")
})

test_that("la prueba de las capturas ve una captura que se traga la condicion", {
  # El control del caso que debe fallar: la misma regla, sobre codigo que la
  # viola y sobre codigo que la cumple.
  traga <- quote(tryCatch(f(), condition = function(c) NULL))
  devuelve <- quote(tryCatch(f(), condition = function(c) .detener(c)))
  mira <- quote(withCallingHandlers(f(), condition = function(c) visto <<- TRUE))
  desvia <- quote(withCallingHandlers(
    f(), condition = function(c) invokeRestart("muffleWarning")
  ))
  regla <- function(e) {
    llamadas <- all.names(e[["condition"]])
    if (identical(.pu_cabeza(e), "tryCatch")) {
      any(c(".detener", "stop", "signalCondition") %in% llamadas)
    } else {
      !any(c("invokeRestart", "tryInvokeRestart", "computeRestarts") %in%
             llamadas)
    }
  }
  expect_false(regla(traga))
  expect_true(regla(devuelve))
  expect_true(regla(mira))
  expect_false(regla(desvia))
  # Y el corte no es un error: `try()` y `tryCatch(error = )` lo dejan pasar.
  corte <- structure(
    class = c("lupa_corte_conexion", "condition"),
    list(message = "corte", call = NULL)
  )
  visto <- tryCatch(
    {
      try(.detener(corte), silent = TRUE)
      "tragado por try"
    },
    lupa_corte_conexion = function(c) "paso"
  )
  expect_identical(visto, "paso")
  visto <- tryCatch(
    tryCatch(.detener(corte), error = function(e) "tragado por error"),
    lupa_corte_conexion = function(c) "paso"
  )
  expect_identical(visto, "paso")
})

test_that("la limpieza llega al controlador solo por el punto unico", {
  funciones <- .pu_funciones()
  nombres <- names(funciones)
  llama <- lapply(funciones, function(f) {
    intersect(all.names(body(f)), nombres)
  })
  # Las funciones desde las que se llega a `.dbi()`.
  alcanzan <- ".dbi"
  repeat {
    nuevas <- setdiff(
      nombres[vapply(llama, function(x) any(x %in% alcanzan), logical(1L))],
      alcanzan
    )
    if (!length(nuevas)) break
    alcanzan <- c(alcanzan, nuevas)
  }
  directas <- list()
  dinamicas <- list()
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- .pu_cabeza(e)
      if (identical(cabeza, "on.exit") && length(e) >= 2L) {
        directas[[length(directas) + 1L]] <<- list(nombre = nombre, expr = e[[2L]])
      }
      if (identical(cabeza, "do.call") && length(e) >= 2L &&
          identical(e[[2L]], as.name("on.exit"))) {
        dinamicas[[length(dinamicas) + 1L]] <<- nombre
      }
    }
    for (hijo in .pu_hijos(e)) recorrer(hijo, nombre)
    invisible()
  }
  for (nombre in nombres) recorrer(body(funciones[[nombre]]), nombre)
  expect_gte(length(directas), 40L)
  llega <- vapply(directas, function(x) {
    any(all.names(x$expr) %in% alcanzan)
  }, logical(1L))
  # Hay limpieza que alcanza al controlador -cerrar un resultado, volver a un
  # punto de guardado, revertir una compilacion-...
  expect_gte(sum(llega), 8L)
  # ...y ninguna lo nombra directamente: pasa por `.dbi()`, que con el
  # pestillo puesto no manda nada.
  directo <- vapply(directas[llega], function(x) {
    any(all.names(x$expr) %in% setdiff(.PU_CONTROLADOR, ".jcall"))
  }, logical(1L))
  expect_false(any(directo))
  # Los `on.exit()` armados con `do.call()` -la proteccion del mensaje, el
  # bloqueo de la reanudacion, `LC_NUMERIC`- registran una expresion hecha con
  # `substitute()`; ninguna alcanza al controlador.
  expect_identical(
    .sort_por_bytes(unlist(dinamicas)),
    c(".fijar_numerica_c", ".fijar_proteccion_motor",
      ".preparar_reanudacion_coleccion")
  )
  registradas <- unlist(lapply(unlist(dinamicas), function(nombre) {
    encontradas <- character()
    buscar <- function(e) {
      if (is.call(e) && identical(.pu_cabeza(e), "substitute") &&
          length(e) >= 2L) {
        encontradas <<- c(encontradas, all.names(e[[2L]]))
      }
      for (hijo in .pu_hijos(e)) buscar(hijo)
      invisible()
    }
    buscar(body(funciones[[nombre]]))
    encontradas
  }))
  expect_gte(length(registradas), 3L)
  expect_false(any(registradas %in% alcanzan))
})

test_that("el manejador que atrapa al controlador no se arma donde esta la conexion", {
  # RSQLite, RPostgres y RMariaDB dejan retenido el manejador del ultimo error
  # suyo que se atrapo, con su entorno: armado en el marco que tiene la
  # conexion, la conexion que quien llama solto sin cerrar no se recolectaba.
  # En las funciones que llaman al controlador, el manejador de `error` es un
  # nombre del espacio de nombres y no una funcion escrita ahi.
  funciones <- .pu_funciones()
  armados <- character()
  vistos <- 0L
  recorrer <- function(e, nombre) {
    if (is.call(e) && identical(.pu_cabeza(e), "tryCatch") &&
        "error" %in% names(e)) {
      cuerpo <- e[[2L]]
      llama <- is.call(cuerpo) && any(all.names(cuerpo) %in% .PU_CONTROLADOR)
      if (identical(nombre, ".dbi") || llama) {
        vistos <<- vistos + 1L
        if (!is.name(e[["error"]])) armados <<- c(armados, nombre)
      }
    }
    for (hijo in .pu_hijos(e)) recorrer(hijo, nombre)
    invisible()
  }
  for (nombre in c(".dbi", ".sesion_cambio_dbi", ".leer_info_corrida_dbi",
                   ".excepcion_cliente_dbi", ".corte_desmentido_dbi")) {
    recorrer(body(funciones[[nombre]]), nombre)
  }
  expect_gte(vistos, 5L)
  expect_identical(armados, character())
})
