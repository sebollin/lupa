# ---- Reanudar una corrida larga de perfilar_coleccion() --------------------
#
# Una coleccion de trescientas tablas puede tardar una noche, y si el proceso
# muere en la tabla doscientos -se corta la luz, se mata el proceso, se cae la
# conexion y las que siguen fallan- hoy se pierde todo lo medido. Con
# `directorio_reanudacion`, cada tabla que se perfilo sin consultas fallidas se
# guarda en el disco en cuanto termina, y `reanudar = TRUE` sigue desde ahi.
#
# La unidad es la tabla, y lo que se guarda es lo que el bucle de
# `perfilar_coleccion()` recibe de ella: el perfil, su `momento` y sus avisos.
# Al reanudar, el bucle recibe lo guardado en lugar de llamar a
# `perfilar_dbi()`, y TODO lo demas -las piezas, las coberturas, el tope de
# `cobertura_metricas`, que se consume en orden, el piso final, los avisos-
# corre igual. Asi el objeto reanudado es el mismo que el de una corrida sin
# cortes, fuera de lo que describe la ejecucion: se midio con cortes reales
# (un proceso matado, una interrupcion, la conexion caida) en SQLite y DuckDB.
#
# No se intenta reconocer una conexion caida: una tabla con alguna consulta
# solicitada fallida, o que no devolvio perfil, no se guarda como lista y se
# vuelve a perfilar al reanudar. Medido en cada punto de una tabla donde la
# conexion se puede caer: todos terminan asi. No toda fila `degradado` de la
# cobertura es una falla: `columnas_opcionales`, `aplicabilidad` y los
# centinelas declarados dejan una en cada tabla sin que nada haya fallado, y con
# esa regla una coleccion con esas opciones se volvia a perfilar entera.
#
# En el disco, un subdirectorio por corrida:
#
#   corrida-AAAAMMDD-HHMMSS-<sufijo>/
#     corrida.rds      la cabecera: lo que hace compatible a una llamada
#     bloqueo/         tomado con dir.create(), exclusivo entre procesos
#     manifiesto.txt   una linea por hecho, solo se agregan
#     tabla-<posicion>-<intento>.rds   el resultado de una tabla limpia
#
# Cada archivo se escribe en un temporal del mismo directorio y se renombra a un
# nombre que no existe; recien despues va su linea al manifiesto, con su suma.
# Nada se renombra encima de otro archivo ni se reescribe: no hace falta que el
# reemplazo sea atomico en ningun sistema.

.FORMATO_REANUDACION <- 1L

.condicion_reanudacion <- function(motivo, mensaje, datos = list()) {
  condicion <- list(message = mensaje, call = NULL, motivo = motivo,
                    datos = datos)
  class(condicion) <- c("lupa_error_reanudacion", "error", "condition")
  condicion
}

.detener_reanudacion <- function(motivo, ..., datos = list()) {
  .detener(.condicion_reanudacion(motivo, .mensaje_condicion(...), datos))
}

.avisar_reanudacion <- function(...) {
  aviso <- list(message = .mensaje_condicion(...), call = NULL)
  class(aviso) <- c("lupa_aviso_reanudacion", "warning", "condition")
  .avisar(aviso)
}

# Los dos argumentos se validan al entrar, antes de cualquier consulta.
.validar_argumentos_reanudacion <- function(directorio, reanudar) {
  if (!is.logical(reanudar) || length(reanudar) != 1L || is.na(reanudar)) {
    .detener_reanudacion("argumento", "`reanudar` debe ser TRUE o FALSE.")
  }
  if (is.null(directorio)) {
    if (reanudar) {
      .detener_reanudacion(
        "argumento",
        "`reanudar = TRUE` necesita `directorio_reanudacion`: es donde ",
        "qued\u00f3 guardada la corrida que se quiere seguir."
      )
    }
    return(invisible(NULL))
  }
  if (!.es_texto_escalar(directorio)) {
    .detener_reanudacion(
      "argumento", "`directorio_reanudacion` debe ser NULL o una ruta de texto."
    )
  }
  invisible(directorio)
}

# Lo que se hace con el resultado de una tabla. `limpia`: un perfil sin
# consultas solicitadas fallidas, que se guarda y se reusa. `degradada`: alguna
# consulta fallo -la fila "Fallaron N consultas solicitadas" de la cobertura-.
# `fallida`: no hubo perfil, o no tiene la forma que el bucle lee. Las dos
# ultimas se vuelven a perfilar al reanudar.
.estado_resultado_reanudacion <- function(perfil) {
  tryCatch({
    if (!inherits(perfil, "perfil_dbi") ||
        is.null(perfil$resumen_tabla$columnas)) {
      return("fallida")
    }
    if (NROW(perfil$resumen_tabla$meta$consultas$fallas) > 0L) {
      return("degradada")
    }
    "limpia"
  }, error = function(e) "fallida")
}

# ---- Identidad de la fuente ------------------------------------------------
#
# Para un archivo, la ruta absoluta que da el motor de cada base que nombra la
# coleccion: `PRAGMA database_list` en SQLite -resuelve `./` y los enlaces
# simbolicos-, `duckdb_databases()` en DuckDB. No `dbGetInfo()$dbname`, que en
# SQLite devuelve la ruta como se paso, ni `current_database()`, que en DuckDB
# es el nombre del archivo sin carpeta: dos archivos homonimos en carpetas
# distintas daban lo mismo. Una base en memoria o temporal no tiene archivo
# -SQLite da `""`, DuckDB `NA`- y no sobrevive al proceso: se rechaza. En SQLite
# entran ademas `application_id` y `user_version`, que el dueno de la base fija
# a proposito. No hay mas: el motor no da otra identidad del archivo y R no
# expone el inodo; un archivo reemplazado en la misma ruta se ve por su tamano o
# su fecha de modificacion (ver `.estado_archivos_reanudacion()`), y entonces no
# se reanuda.
#
# Un servidor se identifica por lo que el servidor declara de si mismo, y eso
# todavia no se comprobo motor por motor: hasta entonces se rechaza, porque una
# identidad mal tomada mezclaria tablas de dos bases. Nunca se leen los datos de
# la conexion -usuario, clave, DSN-.
.identidad_fuente_reanudacion <- function(conexion, tablas) {
  rechazo <- function(...) {
    .detener_reanudacion(
      "identidad", "No se puede reanudar esta colecci\u00f3n: ", ..., "."
    )
  }
  consultar <- function(sql) {
    respuesta <- .consultar_dbi(conexion, sql, NULL,
                                etapa = "identidad_reanudacion")
    if (!isTRUE(respuesta$ok)) {
      rechazo("no se pudo leer la identidad de la base (", respuesta$motivo, ")")
    }
    respuesta$datos
  }
  sqlite <- inherits(conexion, "SQLiteConnection")
  duckdb <- inherits(conexion, "duckdb_connection")
  if (!sqlite && !duckdb) {
    rechazo(
      "la identidad de la base no est\u00e1 comprobada para este motor (",
      as.character(class(conexion)[[1L]]), "). Por ahora la reanudaci\u00f3n ",
      "s\u00f3lo distingue una base de otra en SQLite y DuckDB, por su archivo; ",
      "sin esa comprobaci\u00f3n podr\u00eda mezclar tablas de dos bases"
    )
  }
  if (sqlite) {
    adjuntas <- consultar("PRAGMA database_list")
    nombres <- as.character(adjuntas$name)
    archivos <- as.character(adjuntas$file)
    porque <- " de SQLite est\u00e1 en memoria o es temporal: no sobrevive al proceso"
    pedidas <- ifelse(!is.na(tablas$catalogo), tablas$catalogo,
                      ifelse(!is.na(tablas$esquema), tablas$esquema, "main"))
  } else {
    adjuntas <- consultar(paste(
      "SELECT database_name, path FROM duckdb_databases() WHERE NOT internal"
    ))
    actual <- as.character(consultar("SELECT current_database() AS base")[[1L]])
    nombres <- as.character(adjuntas$database_name)
    archivos <- as.character(adjuntas$path)
    porque <- " de DuckDB est\u00e1 en memoria: no sobrevive al proceso"
    pedidas <- ifelse(!is.na(tablas$catalogo), tablas$catalogo, actual)
  }
  pedidas <- .sort_por_bytes(unique(as.character(pedidas)))
  filas <- lapply(pedidas, function(base) {
    k <- match(.minusculas_ascii(base), .minusculas_ascii(nombres))
    if (is.na(k)) {
      rechazo("la base `", base, "` que nombra la colecci\u00f3n no est\u00e1 ",
              "adjunta a la conexi\u00f3n")
    }
    archivo <- archivos[[k]]
    if (is.na(archivo) || !nzchar(archivo)) rechazo("la base `", base, "`", porque)
    fila <- data.frame(
      nombre = nombres[[k]],
      archivo = normalizePath(archivo, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
    if (sqlite) {
      cita <- as.character(DBI::dbQuoteIdentifier(conexion, nombres[[k]]))
      fila$application_id <- as.numeric(
        consultar(paste0("PRAGMA ", cita, ".application_id"))[[1L]]
      )
      fila$user_version <- as.numeric(
        consultar(paste0("PRAGMA ", cita, ".user_version"))[[1L]]
      )
    }
    fila
  })
  list(motor = if (sqlite) "sqlite" else "duckdb", bases = do.call(rbind, filas))
}

# El tamano y la fecha de modificacion de cada archivo de la identidad, y de su
# diario de escritura si existe (`-wal` en SQLite, `.wal` en DuckDB), al empezar
# cada intento. Si cambiaron desde el intento anterior, la base se escribio o se
# reemplazo entre el corte y la reanudacion: las tablas guardadas la describen
# como era y las que faltan la describirian como es, y el resultado no seria el
# de ninguna corrida. Ver `.cambios_archivos_reanudacion()`.
.estado_archivos_reanudacion <- function(identidad) {
  diario <- if (identical(identidad$motor, "sqlite")) "-wal" else ".wal"
  texto <- function(ruta) {
    info <- file.info(ruta)
    if (is.na(info$size)) return(c("-", "-"))
    c(format(info$size, scientific = FALSE),
      sprintf("%.6f", as.numeric(info$mtime)))
  }
  t(vapply(identidad$bases$archivo, function(ruta) {
    c(texto(ruta), texto(paste0(ruta, diario)))
  }, character(4L), USE.NAMES = FALSE))
}

# Lo que cambio entre dos estados, dicho: tamano y fecha de modificacion del
# archivo, y tamano de su diario. Medido entre dos procesos, sin escribir datos:
# abrir, leer, perfilar y matar el proceso no cambian ninguno de los tres en
# SQLite -con y sin WAL- ni en DuckDB. La fecha del diario si: en modo WAL, el
# `-wal` se borra al cerrar la ultima conexion y se vuelve a crear vacio al
# abrir, con otra fecha y la base igual, asi que no se mira. Un diario que no
# esta y uno vacio son lo mismo.
.cambios_archivos_reanudacion <- function(anteriores, ahora, archivos, diario) {
  instante <- function(x) {
    if (identical(x, "-")) return("no exist\u00eda")
    format(as.POSIXct(as.numeric(x), origin = "1970-01-01"),
           "%Y-%m-%d %H:%M:%OS3 %Z")
  }
  bytes <- function(x) if (identical(x, "-")) "0" else x
  cambios <- character()
  for (k in seq_len(nrow(ahora))) {
    antes <- if (k <= length(anteriores)) anteriores[[k]] else rep("-", 4L)
    partes <- character()
    if (!identical(antes[[1L]], ahora[k, 1L])) {
      partes <- c(partes, paste0("el tama\u00f1o, de ", antes[[1L]], " a ",
                                 ahora[k, 1L], " bytes"))
    }
    if (!identical(antes[[2L]], ahora[k, 2L])) {
      partes <- c(partes, paste0("la fecha de modificaci\u00f3n, de ",
                                 instante(antes[[2L]]), " a ",
                                 instante(ahora[k, 2L])))
    }
    if (!identical(bytes(antes[[3L]]), bytes(ahora[k, 3L]))) {
      partes <- c(partes, paste0("el tama\u00f1o de `", basename(archivos[[k]]),
                                 diario, "`, de ", bytes(antes[[3L]]), " a ",
                                 bytes(ahora[k, 3L]), " bytes"))
    }
    if (length(partes)) {
      cambios <- c(cambios, paste0("en `", archivos[[k]], "`, ",
                                   paste(partes, collapse = " y ")))
    }
  }
  cambios
}

# ---- Lo que hace compatible a una llamada ----------------------------------
#
# Cada argumento en una forma que se compara con `identical()`. Un valor va tal
# cual, con sus atributos -dos factores con otros niveles difieren, y la forma
# de texto del modelo de calidad los daba iguales-. Una funcion, por sus
# formales, su cuerpo y lo que su ambiente le da: de un espacio de nombres, el
# nombre y la version; del global, los valores de los nombres que usa su cuerpo
# -una regla `x > umbral` cambia si cambia `umbral`-; de uno local, sus
# ligaduras. Lo que no se puede comparar entre dos sesiones -un ambiente suelto,
# un objeto S4, un puntero externo- se rechaza nombrando el argumento.
.forma_canonica_reanudacion <- function(x, nombre, vistos = new.env()) {
  rechazar <- function(porque) {
    .detener_reanudacion(
      "configuracion", "El argumento `", nombre, "` ", porque,
      ": la reanudaci\u00f3n no puede comprobar que sea el mismo de la corrida ",
      "original. Perfilar sin `directorio_reanudacion`, o pasarlo como un valor."
    )
  }
  if (is.null(vistos$lista)) vistos$lista <- list()
  if (is.function(x)) {
    if (is.primitive(x)) {
      return(list(tipo = "primitiva", texto = .deparse_por_omision(x)))
    }
    for (visto in vistos$lista) {
      if (identical(visto, x)) return(list(tipo = "ciclo"))
    }
    vistos$lista[[length(vistos$lista) + 1L]] <- x
    return(list(
      tipo = "funcion", formales = .deparse_por_omision(formals(x)),
      cuerpo = .deparse_por_omision(body(x)),
      ambiente = .ambiente_canonico_reanudacion(environment(x), x, nombre, vistos)
    ))
  }
  if (is.environment(x)) rechazar("es un ambiente")
  if (isS4(x)) rechazar("es un objeto S4")
  if (typeof(x) %in% c("externalptr", "weakref", "bytecode", "promise")) {
    rechazar(paste0("es de tipo `", typeof(x), "`"))
  }
  atributos <- attributes(x)
  if (inherits(x, "formula")) {
    return(list(tipo = "formula", texto = .deparse_por_omision(x),
                ambiente = .ambiente_canonico_reanudacion(
                  attr(x, ".Environment", exact = TRUE), NULL, nombre, vistos
                )))
  }
  de_atributos <- function() {
    stats::setNames(lapply(names(atributos), function(a) {
      .forma_canonica_reanudacion(atributos[[a]], paste0(nombre, "@", a), vistos)
    }), names(atributos))
  }
  if (is.list(x)) {
    elementos <- unclass(x)
    return(list(tipo = "lista", elementos = lapply(seq_along(elementos), function(i) {
      .forma_canonica_reanudacion(elementos[[i]], paste0(nombre, "[[", i, "]]"),
                                  vistos)
    }), atributos = de_atributos()))
  }
  if (is.language(x)) {
    return(list(tipo = "expresion", texto = .deparse_por_omision(x)))
  }
  if (length(atributos) && any(vapply(atributos, function(a) {
    is.function(a) || is.environment(a) || is.list(a)
  }, logical(1L)))) {
    valor <- x
    attributes(valor) <- NULL
    return(list(tipo = "valor", valor = valor, atributos = de_atributos()))
  }
  x
}

.ambiente_canonico_reanudacion <- function(ambiente, funcion, nombre, vistos) {
  if (is.null(ambiente)) return(list(tipo = "nulo"))
  if (identical(ambiente, globalenv())) {
    usados <- if (is.function(funcion)) all.names(body(funcion)) else character()
    usados <- setdiff(
      intersect(usados, ls(globalenv(), all.names = TRUE, sorted = FALSE)),
      names(formals(funcion))
    )
    usados <- .sort_por_bytes(usados)
    return(list(tipo = "global", ligaduras = stats::setNames(
      lapply(usados, function(n) {
        .forma_canonica_reanudacion(get(n, envir = globalenv()),
                                    paste0(nombre, "$", n), vistos)
      }), usados
    )))
  }
  if (isNamespace(ambiente)) {
    return(list(tipo = "espacio", nombre = getNamespaceName(ambiente),
                version = as.character(getNamespaceVersion(ambiente))))
  }
  nombre_ambiente <- environmentName(ambiente)
  if (identical(ambiente, baseenv()) || identical(ambiente, emptyenv()) ||
      startsWith(nombre_ambiente, "package:")) {
    return(list(tipo = "nombrado", nombre = nombre_ambiente))
  }
  for (visto in vistos$lista) {
    if (identical(visto, ambiente)) return(list(tipo = "ciclo"))
  }
  vistos$lista[[length(vistos$lista) + 1L]] <- ambiente
  ligados <- .sort_por_bytes(ls(ambiente, all.names = TRUE, sorted = FALSE))
  list(
    tipo = "local",
    ligaduras = stats::setNames(lapply(ligados, function(n) {
      .forma_canonica_reanudacion(get(n, envir = ambiente),
                                  paste0(nombre, "$", n), vistos)
    }), ligados),
    padre = .ambiente_canonico_reanudacion(parent.env(ambiente), funcion,
                                           nombre, vistos)
  )
}

# El codigo que perfila: la version sola no alcanza, porque todo el desarrollo
# entre dos versiones lleva el mismo numero. Instalado, la suma de su base de
# codigo; cargado desde el fuente, la de cada `R/*.R`.
.codigo_reanudacion <- function() {
  nombre <- .nombre_paquete()
  ruta <- tryCatch(getNamespaceInfo(nombre, "path"),
                   error = function(e) NA_character_)
  base <- file.path(ruta, "R", paste0(nombre, ".rdb"))
  archivos <- if (isTRUE(file.exists(base))) base else {
    .sort_por_bytes(list.files(file.path(ruta, "R"), pattern = "[.][Rr]$",
                               full.names = TRUE))
  }
  sumas <- if (length(archivos)) tools::md5sum(archivos) else character()
  names(sumas) <- basename(archivos)
  list(version = .version_paquete(), sumas = sumas)
}

# Lo que difiere entre la cabecera de una corrida y esta llamada, para decirlo.
.diferencias_reanudacion <- function(cabecera, actual) {
  etiquetas <- c(formato = "el formato del directorio",
                 codigo = "la versi\u00f3n de lupa", r = "la versi\u00f3n de R",
                 controlador = "el controlador de la base",
                 identidad = "la base (otro archivo)",
                 coleccion = "las tablas declaradas")
  campos <- names(etiquetas)
  difieren <- etiquetas[!vapply(campos, function(k) {
    identical(cabecera[[k]], actual[[k]])
  }, logical(1L))]
  argumentos <- .sort_por_bytes(union(names(cabecera$configuracion),
                                      names(actual$configuracion)))
  argumentos <- argumentos[!vapply(argumentos, function(k) {
    identical(cabecera$configuracion[[k]], actual$configuracion[[k]])
  }, logical(1L))]
  c(unname(difieren), if (length(argumentos)) paste0("`", argumentos, "`"))
}

# ---- Escritura ----------------------------------------------------------------

# Un RDS en su lugar: temporal en el mismo directorio y `file.rename()` a un
# nombre que no existe. Medido: quien mira el nombre final nunca encontro un
# archivo a medias, y un proceso matado a mitad deja solo el temporal. Formato
# 2, como `guardar_historico()`: el 3 anota la codificacion de quien escribe y
# traduce al leer. Devuelve TRUE o el motivo.
.escribir_rds_reanudacion <- function(objeto, final) {
  temporal <- tempfile(".tmp-", tmpdir = dirname(final), fileext = ".rds")
  hecho <- tryCatch({
    saveRDS(objeto, temporal, version = 2L)
    isTRUE(file.rename(temporal, final))
  }, error = function(e) e)
  if (isTRUE(hecho)) return(TRUE)
  unlink(temporal)
  if (inherits(hecho, "condition")) conditionMessage(hecho) else {
    "no se pudo renombrar el archivo temporal"
  }
}

# El manifiesto se escribe en bytes: `cat(file =)` abre el archivo en modo
# texto, y en Windows cada salto se vuelve retorno mas salto. La lectura tolera
# igual el retorno, para los manifiestos que se escribieron asi.
.agregar_al_manifiesto <- function(corrida, texto) {
  conexion <- file(file.path(corrida, "manifiesto.txt"), open = "ab")
  on.exit(close(conexion), add = TRUE)
  writeBin(charToRaw(texto), conexion)
  invisible(TRUE)
}

# Una linea del manifiesto: campos ASCII separados por tabulador y la ficha
# final `FIN`, asi ningun pedazo de una linea cortada parece una linea entera.
.anotar_reanudacion <- function(corrida, campos) {
  campos <- gsub("[^ -~]", "?", gsub("\t", " ", as.character(campos),
                                    fixed = TRUE, useBytes = TRUE),
                 useBytes = TRUE)
  linea <- paste0(paste(c(campos, "FIN"), collapse = "\t"), "\n")
  hecho <- tryCatch({
    .agregar_al_manifiesto(corrida, linea)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(hecho)) {
    .detener_reanudacion(
      "escritura", "No se pudo escribir el manifiesto de la corrida en ",
      corrida, ": ", hecho, "."
    )
  }
  invisible(TRUE)
}

# ---- Lectura del manifiesto --------------------------------------------------
#
# Vale la ultima linea valida de cada tabla. Una linea vale si termina en `FIN`
# y tiene sus campos con su forma; lo demas se cuenta como descartado. La ultima
# linea sin salto es un corte: se descarta, y antes de escribir se agrega el
# salto para que la siguiente no se le pegue.
.leer_manifiesto_reanudacion <- function(corrida, n_filas) {
  ruta <- file.path(corrida, "manifiesto.txt")
  salida <- list(tablas = list(), intentos = 0L, fines = integer(),
                 bases = list(), inicios = list(), descartadas = 0L,
                 cortada = FALSE)
  tamano <- file.size(ruta)
  if (is.na(tamano) || tamano == 0) return(salida)
  bytes <- readBin(ruta, "raw", n = tamano)
  salida$cortada <- bytes[[length(bytes)]] != as.raw(10L)
  bytes[bytes == as.raw(0L)] <- as.raw(63L)
  lineas <- strsplit(rawToChar(bytes), "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  lineas <- sub("\r$", "", lineas, useBytes = TRUE)
  if (salida$cortada && length(lineas)) {
    lineas <- lineas[-length(lineas)]
    salida$descartadas <- 1L
  }
  entero <- function(x) {
    if (!grepl("^[0-9]{1,9}$", x, useBytes = TRUE)) return(NA_integer_)
    as.integer(x)
  }
  for (linea in lineas) {
    campos <- strsplit(linea, "\t", fixed = TRUE, useBytes = TRUE)[[1L]]
    k <- length(campos)
    tipo <- if (k) campos[[1L]] else ""
    valida <- k >= 2L && identical(campos[[k]], "FIN")
    if (valida && identical(tipo, "tabla") && k == 7L) {
      pos <- entero(campos[[2L]])
      intento <- entero(campos[[6L]])
      estado <- campos[[3L]]
      valida <- !is.na(pos) && pos >= 1L && pos <= n_filas && !is.na(intento) &&
        estado %in% c("limpia", "degradada", "fallida") &&
        (!identical(estado, "limpia") ||
           (grepl("^tabla-[0-9]{6}-[0-9]{3}[.]rds$", campos[[4L]], useBytes = TRUE) &&
              grepl("^[0-9a-f]{32}$", campos[[5L]], useBytes = TRUE)))
      if (valida) {
        salida$tablas[[as.character(pos)]] <- list(
          estado = estado, archivo = campos[[4L]], suma = campos[[5L]],
          intento = intento
        )
      }
    } else if (valida && identical(tipo, "intento") && k == 6L) {
      intento <- entero(campos[[2L]])
      valida <- !is.na(intento)
      if (valida) {
        salida$intentos <- max(salida$intentos, intento)
        salida$inicios[[as.character(intento)]] <- campos[[5L]]
      }
    } else if (valida && identical(tipo, "base") && k == 8L) {
      intento <- entero(campos[[2L]])
      orden <- entero(campos[[3L]])
      valida <- !is.na(intento) && !is.na(orden)
      if (valida) {
        clave <- as.character(intento)
        actual <- salida$bases[[clave]]
        if (is.null(actual)) actual <- list()
        actual[[orden]] <- campos[4:7]
        salida$bases[[clave]] <- actual
      }
    } else if (valida && identical(tipo, "fin") && k == 5L) {
      intento <- entero(campos[[2L]])
      valida <- !is.na(intento)
      if (valida) salida$fines <- c(salida$fines, intento)
    } else {
      valida <- FALSE
    }
    if (!valida) salida$descartadas <- salida$descartadas + 1L
  }
  salida
}

# Que se reusa y que se vuelve a perfilar. Una tabla se reusa si su ultima linea
# dice `limpia`, el archivo esta y su suma coincide; si no, el motivo. Un
# resultado sin linea -el proceso murio entre el rename y la linea- no se
# adopta: la tabla se vuelve a perfilar con otro numero de intento.
.revisar_resultados_reanudacion <- function(corrida, manifiesto, posiciones) {
  motivos <- vapply(posiciones, function(pos) {
    linea <- manifiesto$tablas[[as.character(pos)]]
    if (is.null(linea)) return("sin_resultado")
    if (!identical(linea$estado, "limpia")) return(linea$estado)
    ruta <- file.path(corrida, linea$archivo)
    if (!file.exists(ruta)) return("archivo_ausente")
    if (!identical(unname(tools::md5sum(ruta)), linea$suma)) {
      return("suma_distinta")
    }
    NA_character_
  }, character(1L))
  names(motivos) <- as.character(posiciones)
  motivos
}

# ---- Bloqueo -------------------------------------------------------------------
#
# Un directorio `bloqueo/` por corrida, tomado con `dir.create()`: medido entre
# ocho procesos que lo pedian a la vez, uno solo lo toma, y `file.create()` no
# es exclusivo. Lo suelta `on.exit()` -al terminar, ante un error y ante una
# interrupcion, medido-; un proceso matado o que recibe `SIGTERM` lo deja
# tomado. No se roba: no se puede saber si un proceso de otra maquina sigue
# vivo ni si su numero lo reuso otro. El mensaje dice de quien es y que borrar.
.duenio_bloqueo_reanudacion <- function(bloqueo) {
  duenio <- tryCatch(readRDS(file.path(bloqueo, "duenio.rds")),
                     error = function(e) NULL)
  if (is.null(duenio)) {
    return("un proceso que no dej\u00f3 sus datos (pudo morir justo al tomarlo)")
  }
  pista <- ""
  # En Windows `pskill()` siempre termina el proceso: ahi no se pregunta.
  if (.Platform$OS.type == "unix" &&
      identical(duenio$maquina, Sys.info()[["nodename"]])) {
    pista <- if (isTRUE(tools::pskill(duenio$pid, 0L))) {
      " (en esta m\u00e1quina hay un proceso con ese n\u00famero; puede ser otro)"
    } else {
      " (en esta m\u00e1quina no hay ning\u00fan proceso con ese n\u00famero)"
    }
  }
  paste0("el proceso ", duenio$pid, " de la m\u00e1quina ", duenio$maquina,
         " desde ", format(duenio$desde, "%Y-%m-%d %H:%M:%S %Z"), pista)
}

.tomar_bloqueo_reanudacion <- function(corrida, cabecera, estado) {
  bloqueo <- file.path(corrida, "bloqueo")
  estado$bloqueo <- bloqueo
  estado$tomado <- dir.create(bloqueo, showWarnings = FALSE)
  if (!isTRUE(estado$tomado)) {
    .detener_reanudacion(
      "bloqueada", "La corrida ", basename(corrida), " (empezada el ",
      format(cabecera$creada, "%Y-%m-%d %H:%M:%S %Z"), ") est\u00e1 tomada por ",
      .duenio_bloqueo_reanudacion(bloqueo), ". Si ese proceso ya no corre -se ",
      "lo mat\u00f3, se cort\u00f3 la luz, o ese n\u00famero es ahora de otro ",
      "programa-, borrar el directorio ", bloqueo, " y volver a llamar. ",
      "lupa no lo borra solo: no puede saber si el proceso ",
      "sigue vivo en otra m\u00e1quina ni si su n\u00famero se reus\u00f3.",
      datos = list(bloqueo = bloqueo)
    )
  }
  .escribir_rds_reanudacion(
    list(pid = Sys.getpid(), maquina = Sys.info()[["nodename"]],
         desde = Sys.time(), ficha = basename(tempfile(""))),
    file.path(bloqueo, "duenio.rds")
  )
  invisible(bloqueo)
}

.soltar_bloqueo_reanudacion <- function(estado) {
  if (isTRUE(estado$tomado) && !is.null(estado$bloqueo)) {
    unlink(estado$bloqueo, recursive = TRUE)
    estado$tomado <- FALSE
  }
  invisible(NULL)
}

# ---- Corridas ------------------------------------------------------------------

# Un subdirectorio nuevo, con `dir.create()` exclusivo. El sufijo sale de
# `tempfile()` y no de `sample()`: el generador de numeros de la sesion no se
# toca.
.crear_corrida_reanudacion <- function(directorio) {
  for (i in seq_len(20L)) {
    corrida <- tempfile(
      paste0("corrida-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-"),
      tmpdir = directorio
    )
    if (dir.create(corrida, showWarnings = FALSE)) return(corrida)
  }
  .detener_reanudacion(
    "escritura", "No se pudo crear una corrida en ", directorio, "."
  )
}

# La corrida compatible mas reciente. Si esta terminada no hay que reanudar,
# aunque haya una mas vieja sin terminar: reanudar esa publicaria datos mas
# viejos que una medicion completa mas nueva.
.elegir_corrida_reanudacion <- function(directorio, actual) {
  candidatas <- list.dirs(directorio, recursive = FALSE, full.names = TRUE)
  candidatas <- candidatas[grepl("^corrida-", basename(candidatas),
                                 useBytes = TRUE)]
  cabeceras <- lapply(candidatas, function(d) {
    tryCatch(readRDS(file.path(d, "corrida.rds")), error = function(e) NULL)
  })
  legibles <- which(!vapply(cabeceras, is.null, logical(1L)))
  diferencias <- lapply(cabeceras[legibles], .diferencias_reanudacion,
                        actual = actual)
  compatibles <- legibles[lengths(diferencias) == 0L]
  if (!length(compatibles)) {
    detalle <- if (length(legibles)) {
      paste0(
        " Hay ", length(legibles),
        if (length(legibles) == 1L) " corrida" else " corridas",
        " en el directorio y ninguna es compatible con esta llamada: ",
        paste(utils::head(vapply(seq_along(legibles), function(j) {
          paste0(basename(candidatas[[legibles[[j]]]]), " difiere en ",
                 paste(diferencias[[j]], collapse = ", "))
        }, character(1L)), 5L), collapse = "; "),
        "."
      )
    } else {
      " No hay corridas en el directorio."
    }
    .detener_reanudacion(
      "sin_corrida", "No hay una corrida que reanudar en ", directorio, ".",
      detalle, " Para empezar una, `reanudar = FALSE`."
    )
  }
  creadas <- vapply(cabeceras[compatibles], function(x) {
    as.numeric(x$creada)
  }, numeric(1L))
  elegida <- compatibles[.orden_por_bytes(-creadas,
                                          basename(candidatas[compatibles]))[[1L]]]
  list(corrida = candidatas[[elegida]], cabecera = cabeceras[[elegida]])
}

# ---- La preparacion ------------------------------------------------------------
#
# Devuelve NULL sin directorio: entonces `perfilar_coleccion()` no emite una
# consulta ni escribe un byte de mas. Con directorio, rechaza lo que no se puede
# reanudar ANTES de perfilar, toma el bloqueo -que suelta el `on.exit()` del
# marco de quien llama- y devuelve lo que el bucle usa: `leer()` lo guardado de
# una tabla, `comprometer()` el resultado de una nueva, `cerrar()` al final.
.preparar_reanudacion_coleccion <- function(directorio, reanudar, conexion,
                                            declaradas, configuracion, extras,
                                            nombres_extras, proteger,
                                            incluir_valores,
                                            marco = parent.frame()) {
  if (is.null(directorio)) return(NULL)
  # Con la proteccion del motor -`proteger_datos_personales = TRUE` o
  # `incluir_valores = FALSE`- la coleccion junta los mensajes del motor como
  # llegaron, tambien de las tablas limpias, y al final los vuelve a leer con
  # todo lo leido. Reponerlos obligaria a escribirlos en el disco, que es lo que
  # esa proteccion promete no publicar.
  if (isTRUE(proteger) || isFALSE(incluir_valores)) {
    .detener_reanudacion(
      "proteccion",
      "La reanudaci\u00f3n no se combina con `proteger_datos_personales = TRUE` ",
      "ni con `incluir_valores = FALSE`: para que lo reanudado sea igual a una ",
      "corrida sin cortes habr\u00eda que guardar en `directorio_reanudacion` los ",
      "mensajes del motor tal como llegaron y los valores protegidos de cada ",
      "tabla, que es lo que esa protecci\u00f3n promete no publicar. Perfilar sin ",
      "`directorio_reanudacion`, o sin esa protecci\u00f3n."
    )
  }
  posiciones <- which(declaradas$tipo %in% "tabla")
  identidad <- .identidad_fuente_reanudacion(
    conexion, declaradas[posiciones, , drop = FALSE]
  )
  # `fecha` no decide la compatibilidad: la corrida reanudada usa la de la
  # original -declarada o no-, porque cambia los hallazgos de fechas y lo
  # reanudado tiene que ser lo mismo que una corrida sin cortes.
  en_fecha <- which(nombres_extras %in% "fecha")
  fecha <- list(declarada = length(en_fecha) > 0L,
                valor = if (length(en_fecha)) extras[[en_fecha[[1L]]]] else NULL)
  otros <- setdiff(seq_along(extras), en_fecha)
  # Un argumento sin nombre se compara por su posicion: con nombre vacio,
  # `[[""]]` no encuentra nada y dos valores distintos se daban por iguales.
  claves <- nombres_extras
  claves[is.na(claves) | !nzchar(claves)] <- paste0(
    "..", seq_along(extras)
  )[is.na(claves) | !nzchar(claves)]
  argumentos <- c(configuracion, stats::setNames(lapply(otros, function(i) {
    .forma_canonica_reanudacion(extras[[i]], claves[[i]])
  }), claves[otros]))
  argumentos <- argumentos[.orden_por_bytes(names(argumentos))]
  controlador <- attr(class(conexion), "package", exact = TRUE)
  if (is.null(controlador)) controlador <- NA_character_
  actual <- list(
    formato = .FORMATO_REANUDACION, codigo = .codigo_reanudacion(),
    r = R.version.string,
    controlador = list(
      paquete = controlador,
      version = tryCatch(as.character(utils::packageVersion(controlador)),
                         error = function(e) NA_character_)
    ),
    identidad = identidad,
    coleccion = data.frame(lapply(
      declaradas[, c("catalogo", "esquema", "tabla", "tipo", "identificador")],
      as.character
    ), stringsAsFactors = FALSE),
    configuracion = argumentos
  )
  if (!dir.exists(directorio) &&
      !dir.create(directorio, recursive = TRUE, showWarnings = FALSE)) {
    .detener_reanudacion(
      "escritura", "No se pudo crear `directorio_reanudacion` (", directorio, ")."
    )
  }
  estado <- new.env(parent = emptyenv())
  estado$tomado <- FALSE
  do.call(on.exit, list(
    substitute(.soltar_bloqueo_reanudacion(e), list(e = estado)), add = TRUE
  ), envir = marco)
  if (!reanudar) {
    corrida <- .crear_corrida_reanudacion(directorio)
    cabecera <- c(actual, list(id = basename(corrida), creada = Sys.time(),
                               fecha = fecha))
    .tomar_bloqueo_reanudacion(corrida, cabecera, estado)
    escrito <- .escribir_rds_reanudacion(cabecera,
                                         file.path(corrida, "corrida.rds"))
    if (!isTRUE(escrito)) {
      .detener_reanudacion(
        "escritura", "No se pudo escribir en ", corrida, ": ", escrito, "."
      )
    }
  } else {
    elegida <- .elegir_corrida_reanudacion(directorio, actual)
    corrida <- elegida$corrida
    cabecera <- elegida$cabecera
    .tomar_bloqueo_reanudacion(corrida, cabecera, estado)
  }
  # Los temporales de un corte -un `saveRDS()` a medias- no son de nadie: con el
  # bloqueo tomado, se borran.
  unlink(list.files(corrida, pattern = "^[.]tmp-", all.files = TRUE,
                    full.names = TRUE))
  manifiesto <- .leer_manifiesto_reanudacion(corrida, nrow(declaradas))
  motivos <- .revisar_resultados_reanudacion(corrida, manifiesto, posiciones)
  previo <- manifiesto$intentos
  if (reanudar && previo %in% manifiesto$fines && !any(!is.na(motivos))) {
    .detener_reanudacion(
      "terminada", "La corrida compatible m\u00e1s reciente, ", cabecera$id,
      " (empezada el ", format(cabecera$creada, "%Y-%m-%d %H:%M:%S %Z"),
      "), termin\u00f3 con todas sus tablas: no hay nada que reanudar. Para ",
      "medir de nuevo, `reanudar = FALSE`."
    )
  }
  reusables <- names(motivos)[is.na(motivos)]
  archivos_ahora <- .estado_archivos_reanudacion(identidad)
  # Antes de anotar este intento: si se anotara, el siguiente compararia contra
  # la base ya cambiada y reanudaria.
  anteriores <- manifiesto$bases[[as.character(previo)]]
  if (reanudar && length(reusables) && length(anteriores)) {
    cambios <- .cambios_archivos_reanudacion(
      anteriores, archivos_ahora, identidad$bases$archivo,
      if (identical(identidad$motor, "sqlite")) "-wal" else ".wal"
    )
    if (length(cambios)) {
      .detener_reanudacion(
        "base_cambiada", "No se reanuda la corrida ", cabecera$id,
        ": desde su intento anterior (",
        manifiesto$inicios[[as.character(previo)]] %||% "sin fecha",
        ") cambi\u00f3 la base: ", paste(cambios, collapse = "; "),
        ". Las tablas guardadas la describen como era y las que faltan la ",
        "describir\u00edan como es ahora: el resultado no ser\u00eda el de ",
        "ninguna corrida. Una corrida nueva, con `reanudar = FALSE`, la mide ",
        "entera como est\u00e1.",
        datos = list(cambios = cambios)
      )
    }
  }
  if (manifiesto$cortada) {
    .agregar_al_manifiesto(corrida, "\n")
  }
  intento <- previo + 1L
  .anotar_reanudacion(corrida, c(
    "intento", intento, Sys.getpid(), Sys.info()[["nodename"]],
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
  for (k in seq_len(nrow(archivos_ahora))) {
    .anotar_reanudacion(corrida, c("base", intento, k, archivos_ahora[k, ]))
  }
  if (reanudar) {
    cli::cli_alert_info(.cli_literal(paste0(
      "Se reanuda la corrida ", cabecera$id, ", empezada el ",
      format(cabecera$creada, "%Y-%m-%d %H:%M:%S %Z"), ": se reusan ",
      length(reusables), if (length(reusables) == 1L) " tabla" else " tablas",
      " y se vuelven a perfilar ", sum(!is.na(motivos)), "."
    )))
    if (!identical(cabecera$fecha, fecha)) {
      .avisar_reanudacion(
        "La corrida ", cabecera$id, " se hizo ",
        if (isTRUE(cabecera$fecha$declarada)) {
          paste0("con `fecha = ", paste(format(cabecera$fecha$valor),
                                        collapse = ", "), "`")
        } else "sin declarar `fecha`",
        "; se usa esa y no la de esta llamada, para que el perfil reanudado sea ",
        "el mismo que el de una corrida sin cortes."
      )
    }
  }
  # Lo que el bucle pasa a `perfilar_dbi()`: lo de esta llamada, con la `fecha`
  # de la corrida.
  pasar <- c(extras[otros],
             if (isTRUE(cabecera$fecha$declarada)) list(fecha = cabecera$fecha$valor))
  registro <- new.env(parent = emptyenv())
  registro$origen <- rep(NA_character_, nrow(declaradas))
  registro$motivo <- rep(NA_character_, nrow(declaradas))
  registro$estado <- rep(NA_character_, nrow(declaradas))
  registro$motivo[posiciones] <- if (reanudar) motivos else "corrida_nueva"
  leer <- function(i) {
    clave <- as.character(i)
    if (!clave %in% reusables) return(NULL)
    linea <- manifiesto$tablas[[clave]]
    guardado <- tryCatch(readRDS(file.path(corrida, linea$archivo)),
                         error = function(e) NULL)
    if (is.null(guardado) ||
        !identical(guardado$formato, .FORMATO_REANUDACION) ||
        !identical(guardado$posicion, as.integer(i)) ||
        !identical(guardado$identificador, declaradas$identificador[[i]])) {
      registro$motivo[[i]] <- "registro_ilegible"
      return(NULL)
    }
    registro$origen[[i]] <- "reusada"
    registro$estado[[i]] <- "limpia"
    guardado
  }
  comprometer <- function(i, momento, perfil, avisos) {
    estado_tabla <- .estado_resultado_reanudacion(perfil)
    archivo <- "-"
    suma <- "-"
    if (identical(estado_tabla, "limpia")) {
      archivo <- sprintf("tabla-%06d-%03d.rds", as.integer(i), intento)
      ruta <- file.path(corrida, archivo)
      escrito <- .escribir_rds_reanudacion(list(
        formato = .FORMATO_REANUDACION, posicion = as.integer(i),
        identificador = declaradas$identificador[[i]], momento = momento,
        perfil = perfil, avisos = avisos
      ), ruta)
      if (!isTRUE(escrito)) {
        .detener_reanudacion(
          "escritura", "No se pudo guardar el resultado de `",
          declaradas$identificador[[i]], "` en ", corrida, ": ", escrito,
          ". Lo perfilado hasta la tabla anterior queda guardado: ",
          "`reanudar = TRUE` sigue desde ah\u00ed."
        )
      }
      suma <- unname(tools::md5sum(ruta))
    }
    .anotar_reanudacion(corrida, c("tabla", as.integer(i), estado_tabla,
                                   archivo, suma, intento))
    registro$origen[[i]] <- "perfilada"
    registro$estado[[i]] <- estado_tabla
    invisible(estado_tabla)
  }
  cerrar <- function() {
    reusadas <- sum(registro$origen %in% "reusada")
    perfiladas <- sum(registro$origen %in% "perfilada")
    hecho <- tryCatch({
      .anotar_reanudacion(corrida, c("fin", intento, reusadas, perfiladas))
      TRUE
    }, error = function(e) FALSE)
    if (!hecho) {
      .avisar_reanudacion(
        "No se pudo anotar el final de la corrida ", cabecera$id, " en ",
        corrida, ": queda como sin terminar."
      )
    }
    list(
      corrida = cabecera$id, directorio = directorio, creada = cabecera$creada,
      intento = intento, reanudada = reanudar,
      tablas = data.frame(
        identificador = declaradas$identificador[posiciones],
        origen = registro$origen[posiciones],
        motivo = ifelse(registro$origen[posiciones] %in% "reusada",
                        NA_character_, registro$motivo[posiciones]),
        estado = registro$estado[posiciones],
        stringsAsFactors = FALSE
      ),
      lineas_descartadas = manifiesto$descartadas,
      fecha = cabecera$fecha
    )
  }
  list(argumentos = pasar, leer = leer, comprometer = comprometer,
       cerrar = cerrar)
}
