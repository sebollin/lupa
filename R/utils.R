# El orden por bytes (`method = "radix"`) exige que la codificacion este
# DECLARADA: R rechaza con "Character encoding must be UTF-8, Latin-1 or bytes"
# una cadena marcada `unknown` que contenga bytes no ASCII, aunque sean UTF-8
# perfectamente validos. Y asi llega cualquier CSV en espanol leido con
# `read.csv()`: `"Combustibles liquidos"` con tilde sale con `Encoding()` en
# `unknown` y rompe el perfil entero.
#
# Se marca la codificacion antes de ordenar. Lo que despues de eso siga sin ser
# UTF-8 valido se pasa por `iconv(sub = "byte")`, que reemplaza el byte suelto
# por su codigo: no es bonito, pero es determinista y ordenable, que es lo que
# el orden necesita. La alternativa -caer al orden del entorno- devolveria la
# dependencia de la maquina que este orden existe para sacar.
# La clave de ordenamiento por bytes, separada del ordenamiento en si. Existe
# porque hay desempates que ordenan por DOS criterios -primero la frecuencia,
# despues el texto- y ahi no sirve una funcion que ya devuelva el vector
# ordenado: hace falta la clave para pasarsela a `order()`.
.es_ascii <- function(x) {
  if (!length(x)) return(TRUE)
  !any(grepl("[^\\001-\\177]", x, useBytes = TRUE, perl = TRUE))
}

.clave_bytes <- function(x) {
  crudo <- tryCatch(as.character(x), error = function(e) NULL)
  # En ASCII no hay marca ni conversion que pueda cambiar la representacion.
  # La sonda es vectorizada y se paga una sola vez; no se entra a `validUTF8()`
  # ni a `iconv()` para el caso que domina los perfiles.
  if (!is.null(crudo) && .es_ascii(crudo)) return(crudo)
  clave <- if (is.null(crudo)) {
    tryCatch(enc2utf8(as.character(x)), error = function(e) as.character(x))
  } else {
    tryCatch(enc2utf8(crudo), error = function(e) crudo)
  }
  invalidos <- !is.na(clave) & !validUTF8(clave)
  if (any(invalidos)) {
    clave[invalidos] <- iconv(clave[invalidos], to = "UTF-8", sub = "byte")
  }
  clave
}

# Los nombres son datos del usuario, y no se pueden comparar con `==`, `match()`
# o `setdiff()` directamente: esas operaciones consultan `LC_CTYPE` cuando una
# cadena UTF-8 llega con marca `unknown`. La marca no forma parte del nombre que
# el usuario escribio; sus bytes, si. Esta funcion devuelve una representacion
# de trabajo en la que todo UTF-8 valido tiene la misma marca y lo que no es
# UTF-8 se representa por sus bytes, sin adivinar una codificacion. Por eso la
# clave es estable entre locales y sigue siendo inyectiva: dos secuencias de
# bytes distintas no se convierten en el mismo nombre.
.nombres_para_operar <- function(nombres) {
  nombres <- as.character(nombres)
  if (!length(nombres)) return(nombres)
  # En el caso habitual (nombres ASCII) R ya considera iguales las marcas
  # `unknown` y `UTF-8`; evitar el resto de la preparacion mantiene barata la
  # ruta que se ejecuta en cada columna de una tabla grande.
  no_ascii <- grepl("[^\\001-\\177]", nombres, useBytes = TRUE, perl = TRUE)
  reservado <- rep(FALSE, length(nombres))
  indices_ascii <- which(!is.na(nombres) & !no_ascii)
  if (length(indices_ascii)) {
    reservado[indices_ascii] <- grepl(
      "^<lupa-byte:[0-9A-F]+>$", nombres[indices_ascii], perl = TRUE
    )
  }
  if (!any(no_ascii) && !any(reservado)) return(nombres)
  salida <- nombres
  validos <- !is.na(nombres) & validUTF8(nombres)
  if (any(validos)) {
    trozo <- nombres[validos]
    Encoding(trozo) <- "UTF-8"
    salida[validos] <- trozo
    # La representacion de bytes invalidos usa un prefijo reservado. Si un
    # nombre UTF-8 real lo contiene literalmente, se escapa con otra marca
    # para que la clave siga siendo inyectiva incluso en ese caso extremo.
    indices_validos <- which(validos)
    reservados <- indices_validos[reservado[indices_validos]]
    if (length(reservados)) {
      salida[reservados] <- vapply(salida[reservados], function(nombre) {
        bytes <- as.integer(charToRaw(nombre))
        paste0("<lupa-text:", paste(sprintf("%02X", bytes), collapse = ""), ">")
      }, character(1L))
    }
  }
  invalidos <- !is.na(nombres) & !validUTF8(nombres)
  if (any(invalidos)) {
    salida[invalidos] <- vapply(nombres[invalidos], function(nombre) {
      bytes <- as.integer(charToRaw(nombre))
      paste0("<lupa-byte:", paste(sprintf("%02X", bytes), collapse = ""), ">")
    }, character(1L))
  }
  salida
}

# Estos ayudantes extienden la misma política a identificadores que no son
# nombres de columna: dimensiones, métricas, reglas y fronteras declaradas.
# Devuelven los originales para que el texto que el usuario escribió siga
# siendo el que se publica; sólo la decisión de pertenencia usa la clave estable.
.identificadores_en <- function(x, y) {
  .nombres_para_operar(x) %in% .nombres_para_operar(y)
}

.identificadores_unicos <- function(x) {
  originales <- as.character(x)
  originales[!duplicated(.nombres_para_operar(originales))]
}

.identificadores_setdiff <- function(x, y) {
  originales <- as.character(x)
  originales[!.identificadores_en(originales, y)]
}

.identificadores_intersect <- function(x, y) {
  originales <- as.character(x)
  originales[.identificadores_en(originales, y)]
}

.identificadores_ordenados <- function(x) {
  originales <- as.character(x)
  originales[order(.nombres_para_operar(originales), method = "radix")]
}

.indice_identificador <- function(pedidos, nombres) {
  match(.nombres_para_operar(pedidos), .nombres_para_operar(nombres))
}

.clave_par_identificador <- function(a, b, sep = "|") {
  paste(.nombres_para_operar(a), .nombres_para_operar(b), sep = sep)
}

# `make.unique()` se usa para claves internas y para nombres de listas. No se le
# entrega el nombre del usuario: bajo `C` puede publicar `<U+....>` y, peor,
# producir una cadena que no existe en la tabla. La desambiguacion se hace sobre
# la clave estable y conserva el nombre original mientras no haya colision.
.nombres_unicos <- function(nombres, sep = ".") {
  originales <- as.character(nombres)
  if (!length(originales)) return(originales)
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("`sep` debe ser una cadena de longitud uno.", call. = FALSE)
  }
  unicos <- originales
  usados <- new.env(hash = TRUE, parent = emptyenv())
  siguientes <- new.env(hash = TRUE, parent = emptyenv())
  clave_hash <- function(clave) {
    if (is.na(clave)) return(NA_character_)
    paste0("k", paste(as.integer(charToRaw(clave)), collapse = "_"))
  }
  ya_usada <- function(clave) {
    llave <- clave_hash(clave)
    !is.na(llave) && exists(llave, envir = usados, inherits = FALSE)
  }
  registrar <- function(clave) {
    llave <- clave_hash(clave)
    if (!is.na(llave)) assign(llave, TRUE, envir = usados)
  }
  for (i in seq_along(originales)) {
    candidato <- originales[[i]]
    clave <- .nombres_para_operar(candidato)
    repetido <- ya_usada(clave)
    if (repetido) {
      base <- candidato
      llave_base <- clave_hash(clave)
      sufijo <- if (exists(llave_base, envir = siguientes, inherits = FALSE)) {
        get(llave_base, envir = siguientes, inherits = FALSE)
      } else 1L
      repeat {
        candidato <- paste0(base, sep, sufijo)
        clave <- .nombres_para_operar(candidato)
        if (!ya_usada(clave)) break
        sufijo <- sufijo + 1L
      }
      assign(llave_base, sufijo + 1L, envir = siguientes)
    }
    unicos[[i]] <- candidato
    registrar(clave)
  }
  unicos
}

.nombres_make_names <- function(nombres) {
  originales <- .nombres_para_operar(nombres)
  if (!length(originales)) return(originales)
  sintacticos <- vapply(originales, function(nombre) {
    if (is.na(nombre) || !nzchar(nombre)) return("X")
    codigos <- utf8ToInt(nombre)
    texto <- vapply(codigos, intToUtf8, character(1L))
    es_letra <- vapply(texto, function(caracter) {
      grepl("(*UTF)^[\\p{L}\\p{Nl}]$", caracter, perl = TRUE)
    }, logical(1L))
    es_numero <- vapply(texto, function(caracter) {
      grepl("(*UTF)^[\\p{N}]$", caracter, perl = TRUE)
    }, logical(1L))
    permitidos <- es_letra | es_numero | texto %in% c(".", "_")
    texto[!permitidos] <- "."
    primero_valido <- es_letra[[1L]] ||
      (codigos[[1L]] == utf8ToInt(".") && length(texto) > 1L &&
         !es_numero[[2L]])
    salida <- paste0(texto, collapse = "")
    if (!primero_valido) salida <- paste0("X", salida)
    # Igual que `make.names()`, las palabras reservadas no quedan como nombres
    # desnudos. La lista es parte del contrato de base R y es ASCII.
    if (salida %in% c(
      "if", "else", "repeat", "while", "function", "for", "in", "next",
      "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
      "NA_real_", "NA_complex_", "NA_character_"
    )) salida <- paste0(salida, ".")
    salida
  }, character(1L))
  .nombres_unicos(sintacticos)
}

.indice_nombre <- function(pedidos, nombres) {
  .indice_identificador(pedidos, nombres)
}

.nombres_resueltos <- function(pedidos, nombres) {
  nombres[.indice_nombre(pedidos, nombres)]
}

.nombres_presentes <- function(pedidos, nombres) {
  !is.na(.indice_nombre(pedidos, nombres))
}

.matriz_ausentes <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(is.na(tabla))
  if (!ncol(tabla)) return(matrix(logical(), nrow = nrow(tabla), ncol = 0L))
  do.call(cbind, unname(lapply(tabla, is.na)))
}

# El orden de un vector cualquiera, sin depender de lo que el usuario tenga
# adjunto ni de la configuracion regional.
#
# `bit64` ENMASCARA `order()` cuando esta adjunto, y dentro del espacio de
# nombres de un paquete no lo esta: ahi `order()` es el de base, que sobre un
# `integer64` ordena por los bits del `double` subyacente y manda los negativos
# al final. Medido: `c(1, 2, -999, 4, 5)` daba `1,2,4,5,3`. Con eso, ante un
# empate la moda de una columna `integer64` salia `1` donde la misma columna en
# `double` daba `-999`: el mismo dato, dos modas, decididas por el
# almacenamiento.
#
# `rank.integer64()` si esta exportada por `bit64` -`order.integer64` no-, y
# ordenar por el rango da el orden correcto incluso por encima de 2^53, sin
# convertir a `double` y sin perder precision.
#
# Es la misma familia que el desempate por locale y que el import de
# `data.table`: una dependencia que cambia el significado de una funcion basica
# segun donde se la llame.
.orden_seguro <- function(x) {
  if (inherits(x, "integer64")) {
    # Si la columna es `integer64`, `bit64` esta necesariamente cargado: el
    # objeto no podria existir si no. Aun asi se cae con elegancia.
    if (requireNamespace("bit64", quietly = TRUE)) {
      return(tryCatch(order(bit64::rank.integer64(x)),
                      error = function(e) seq_along(x)))
    }
    return(seq_along(x))
  }
  if (is.character(x)) {
    return(order(.clave_bytes(x), method = "radix"))
  }
  clave <- if (is.raw(x)) as.integer(x) else x
  tryCatch(order(clave), error = function(e) seq_along(x))
}

.ordenar_por_bytes <- function(x) {
  if (!length(x)) return(x)
  x[order(.clave_bytes(x), method = "radix")]
}

# `.columnas_duplicadas()` y `.pares_redundantes()` eran el mismo bloque escrito
# dos veces en archivos distintos, con una sola diferencia: que columnas se
# comparan -todas, o un subconjunto de claves-. Corregir un detalle obligaba a
# acordarse de hacerlo en los dos lados. Los dos nombres se conservan porque
# dicen cosas distintas en su contexto, pero ahora son envoltorios de esto.
.pares_de_columnas_identicas <- function(datos, indices, nombres) {
  vacio <- data.frame(
    columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
  )
  if (length(indices) < 2L) return(vacio)
  pares <- utils::combn(indices, 2L)
  iguales <- apply(pares, 2L, function(indice) {
    .columnas_identicas(datos[[indice[[1L]]]], datos[[indice[[2L]]]])
  })
  pares <- pares[, iguales, drop = FALSE]
  if (!ncol(pares)) return(vacio)
  data.frame(
    columna_1 = nombres[pares[1L, ]],
    columna_2 = nombres[pares[2L, ]],
    stringsAsFactors = FALSE
  )
}

# `muestra = 1.5` se aceptaba y se perfilaba UNA fila, en silencio. Un usuario
# que pide una muestra y medio recibe un perfil sobre una fila sin enterarse, y
# la misma llamada por la via DBI da error. Ahora las dos rechazan el no entero.
#
# La unica diferencia que queda entre las dos vias es deliberada y esta
# documentada: `Inf` vale en memoria -significa "todas las filas"- y no vale
# contra un motor, donde hay que decir cuantas filas traer.
# Las dos comprobaciones de entrada que mas se repetian: once veces la de `datos`
# y seis la de `perfil`. La de `perfil` habia derivado en dos redacciones que
# conviven en el mismo archivo a trescientas lineas de distancia -«a las columnas
# de `datos`» y «a `datos`»-, que es lo que pasa cuando la misma regla se escribe
# muchas veces: no diverge de golpe, diverge de a poco.
.validar_datos_tabla <- function(datos, nombre = "datos") {
  if (!inherits(datos, "data.frame")) {
    stop("`", nombre, "` debe heredar de data.frame.", call. = FALSE)
  }
  invisible(datos)
}

# Copia las subclases con semantica propia de `[` a un data.frame base antes
# de ejecutar codigo que selecciona columnas o agrega columnas.
.tabla_base <- function(tabla) {
  if (inherits(tabla, "data.table")) {
    return(as.data.frame(data.table::copy(tabla), stringsAsFactors = FALSE))
  }
  if (inherits(tabla, "tbl_df")) {
    return(as.data.frame(tabla, stringsAsFactors = FALSE))
  }
  tabla
}

# Equivalente a `data.frame[, columnas, drop = FALSE]`, sin depender de la
# clase de `datos` ni del estado del espacio de nombres. La seleccion de filas
# es opcional para los llamadores que necesitan ambas dimensiones.
.seleccionar_columnas <- function(datos, columnas, filas = NULL) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  if (is.character(columnas) && !is.null(names(datos)) &&
      !anyNA(columnas)) {
    indices <- .indice_nombre(columnas, names(datos))
    if (!anyNA(indices)) columnas <- indices
  }
  salida <- if (is.null(filas)) {
    base::`[.data.frame`(datos, , columnas, drop = FALSE)
  } else {
    base::`[.data.frame`(datos, filas, columnas, drop = FALSE)
  }
  # La clase de la entrada no forma parte de esta primitiva: los llamadores
  # trabajan con tablas base despues de la frontera y no necesitan propagar
  # subclases con otra semantica de `[`. La seleccion ya fue materializada.
  class(salida) <- "data.frame"
  salida
}

# `perfil` es opcional en casi todos los llamadores: si viene, tiene que
# corresponder a las mismas columnas y en el mismo orden, porque los indices de
# columna del perfil se usan para leer `datos`.
.validar_perfil_de <- function(perfil, datos) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
        !identical(.nombres_para_operar(names(datos)),
                   .nombres_para_operar(perfil$columnas$columna)))) {
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  invisible(perfil)
}

.validar_muestra <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      muestra < 1) {
    stop("`muestra` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  if (is.finite(muestra) && muestra != floor(muestra)) {
    stop(
      "`muestra` debe ser un entero positivo, o `Inf` para no muestrear.",
      call. = FALSE
    )
  }
  floor(muestra)
}

.muestrear_vector <- function(x, muestra) {
  limite_solicitado <- .validar_muestra(muestra)

  total <- length(x)
  limite <- min(total, limite_solicitado)
  if (total <= limite) {
    return(list(
      valores = x,
      total = total,
      analizados = total,
      muestreado = FALSE
    ))
  }

  indices <- unique(as.integer(round(seq.int(1, total, length.out = limite))))
  list(
    valores = x[indices],
    total = total,
    analizados = length(indices),
    muestreado = TRUE
  )
}

.tipo_declarado <- function(x) {
  if (inherits(x, "sfc")) {
    return(class(x)[[1L]])
  }
  if (is.matrix(x)) {
    return("matriz")
  }
  if (inherits(x, "integer64")) {
    return("integer64")
  }
  if (inherits(x, "POSIXt")) {
    return("fecha-hora")
  }
  if (inherits(x, "Date")) {
    return("fecha")
  }
  if (is.ordered(x)) {
    return("factor-ordenado")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.logical(x)) {
    return("logico")
  }
  if (is.integer(x)) {
    return("entero")
  }
  if (is.double(x)) {
    return("doble")
  }
  if (is.character(x)) {
    return("texto")
  }
  if (is.list(x)) {
    return("lista")
  }
  class(x)[[1L]]
}

.callar_moda_sin_empate <- function(columnas) {
  # No hay moda cuando todos los valores validos aparecen una sola vez: lo que
  # se publicaria es el que gana un desempate de n vias, y el desempate sigue
  # el orden de ordenamiento, que depende de como este guardada la columna.
  # Medido: `c(-5.5, -1, 0, 3.75)` daba moda `-5.5` guardada como doble y `-1`
  # guardada como texto, con `frecuencia_moda = 1` en los dos casos. El dato es
  # el mismo y la moda publicada, distinta.
  #
  # Con un solo valor distinto SI hay moda, aunque su frecuencia sea 1: ahi no
  # hay empate que resolver y el valor publicado no depende de nada.
  #
  # `frecuencia_moda` NO se toca: que la frecuencia maxima sea 1 es cierto y es
  # justamente la evidencia de por que la moda quedo callada.
  necesarios <- c("moda", "frecuencia_moda", "n_distintos")
  if (!is.data.frame(columnas) || !nrow(columnas) ||
      !all(necesarios %in% names(columnas))) {
    return(columnas)
  }
  frecuencia <- suppressWarnings(as.numeric(columnas[["frecuencia_moda"]]))
  distintos <- suppressWarnings(as.numeric(columnas[["n_distintos"]]))
  sin_moda <- !is.na(frecuencia) & !is.na(distintos) &
    frecuencia == 1 & distintos > 1
  # Una columna protegida queda como esta. El marcador `[valor protegido]` no
  # afirma una moda, asi que no hay nada que corregir; y cambiarlo por NA SI
  # diria algo del dato -- que esa columna personal no tiene ningun valor
  # repetido -- justo lo que la proteccion existe para no decir. Se comprueba
  # por el campo y tambien por el marcador, para que el resultado no dependa de
  # si la proteccion ya corrio o corre despues.
  if ("dato_personal_protegido" %in% names(columnas)) {
    protegida <- !is.na(columnas[["dato_personal_protegido"]]) &
      as.logical(columnas[["dato_personal_protegido"]])
    sin_moda <- sin_moda & !protegida
  }
  marcada <- !is.na(columnas[["moda"]]) &
    as.character(columnas[["moda"]]) == "[valor protegido]"
  sin_moda <- sin_moda & !marcada
  if (any(sin_moda)) {
    columnas[["moda"]][sin_moda] <- NA
  }
  columnas
}

.sellar_perfil_dbi <- function(estructura) {
  # Unico lugar donde un objeto pasa a ser `perfil_dbi`. Existe porque la clase
  # se asignaba en cuatro sitios distintos, y una regla que se aplica "al
  # salir" se olvidaba en tres: con `bloque_muestra = "solo_agregados"` el
  # perfil publicaba una moda que `perfilar()` ya callaba sobre el mismo dato.
  # Que un camino nuevo se olvide de la regla ahora es imposible: para tener la
  # clase tiene que pasar por aca.
  if (is.list(estructura) &&
      is.data.frame(estructura$resumen_tabla$columnas)) {
    estructura$resumen_tabla$columnas <- .callar_moda_sin_empate(
      estructura$resumen_tabla$columnas
    )
  }
  class(estructura) <- "perfil_dbi"
  estructura
}

.zona_horaria_origen <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_character_)
  zona <- attr(x, "tzone", exact = TRUE)
  if (length(zona) && !is.na(zona[[1L]]) && nzchar(zona[[1L]])) {
    as.character(zona[[1L]])
  } else {
    "sin_declarar"
  }
}

.fechas_civiles_distintas_utc <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_integer_)
  zona <- .zona_horaria_origen(x)
  if (identical(zona, "sin_declarar")) return(NA_integer_)
  presentes <- !is.na(x)
  if (!any(presentes)) return(0L)
  civil_origen <- format(x[presentes], "%Y-%m-%d", tz = zona)
  civil_utc <- format(x[presentes], "%Y-%m-%d", tz = "UTC")
  as.integer(sum(civil_origen != civil_utc, na.rm = TRUE))
}

# `deparse()` de una expresion larga devuelve VARIAS lineas, y con
# `do.call(perfilar, list(data.frame(...)))` la expresion es la tabla entera: el
# nombre salia con ocho elementos y `reportar()` reventaba con "values must be
# length 1". `do.call()` es una forma corriente de llamar a esto -pasar los
# argumentos en una lista es lo natural cuando se perfila en un bucle-, asi que
# el nombre tiene que sobrevivirla.
#
# Si la expresion no cabe en una linea no sirve como nombre: no lo escribio
# nadie, es una tabla deparseada. En ese caso se usa una etiqueta generica.
#
# Vivia DENTRO del cuerpo de `perfilar()`, asi que era local: `analizar()`, que
# tiene el mismo problema y llega por la misma puerta, no podia usarla y
# publicaba el nombre deparseado en varios elementos -con `[valor protegido]`
# incrustado, porque la capa de proteccion enmascaraba los numeros del propio
# nombre-. Vive aca, que es donde se ve.
.nombre_de_los_datos <- function(expresion) {
  texto <- tryCatch(deparse(expresion), error = function(e) character())
  if (length(texto) != 1L || !nzchar(trimws(texto)) || nchar(texto) > 120L) {
    return("datos")
  }
  texto
}

.texto_valor <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) {
    return(NA_character_)
  }
  if (inherits(x, "POSIXt")) {
    return(format(x[[1L]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x[[1L]], "%Y-%m-%d"))
  }
  as.character(x[[1L]])
}

# ¿La clase de `x` sabe convertirse a texto por sí misma? Si define un método
# de `as.character()`, la conversión produce los valores del dato. Si no, la
# coerción cae en `as.character.default()`.
.tiene_metodo_as_character <- function(x) {
  for (clase in class(x)) {
    metodo <- tryCatch(
      utils::getS3method("as.character", clase, optional = TRUE),
      error = function(e) NULL
    )
    if (!is.null(metodo)) return(TRUE)
  }
  FALSE
}

# Una columna se puede analizar como texto cuando aplicarle una primitiva de
# texto devuelve sus valores. Sobre una lista de objetos —una `sfc`, una columna
# de WKB crudo, una lista de vectores— `as.character.default()` no devuelve
# valores: deparsa cada elemento a código fuente de R. Ese texto no describe el
# dato, es más largo que el dato mismo y se rehace una vez por cada etapa que lo
# toca, así que el costo del perfilado queda dominado por una conversión que
# nadie pidió y que nadie puede leer.
#
# Las listas de escalares atómicos sí se coercionan a sus valores y siguen
# siendo analizables: el corte separa lo que se convierte de lo que se deparsa,
# no lo que es lista de lo que no lo es.
.analizable_como_texto <- function(x) {
  if (!is.list(x)) return(TRUE)
  if (inherits(x, "sfc") || inherits(x, "sfg")) return(FALSE)
  if (.tiene_metodo_as_character(x)) return(TRUE)
  n <- length(x)
  if (!n) return(TRUE)
  largos <- tryCatch(lengths(x), error = function(e) NULL)
  if (is.null(largos) || length(largos) != n || anyNA(largos) ||
      !all(largos == 1L)) {
    return(FALSE)
  }
  all(vapply(x, is.atomic, logical(1L)))
}

.motivo_no_analizable_texto <- function(x) {
  paste0(
    "La columna es una lista de objetos (", .tipo_declarado(x),
    "); las etapas de texto no la evaluaron porque convertirla no produce sus ",
    "valores sino su representaci\u00f3n como c\u00f3digo."
  )
}

#' Preparar una columna para las etapas de texto
#'
#' Separa dos usos que hasta ahora viajaban juntos y que no son el mismo:
#' aplicar una primitiva de texto sobre los valores, y compararlos por
#' igualdad.
#'
#' @param x Columna que se va a analizar.
#'
#' @return Lista con cinco elementos.
#'   `valores` es lo que puede recibir una primitiva de texto. Cuando la
#'   columna no es analizable como texto, son ausentes declarados: la no
#'   evaluación se declara en vez de dejar pasar la columna intacta para que
#'   cada etapa la deparse por su cuenta.
#'   `valores_identidad` es lo que puede recibir una comparación por igualdad
#'   —`unique()`, `match()`, la moda, la inferencia de tipo—. Para una columna
#'   analizable que no necesita coercion es el mismo vector que `valores`; una
#'   columna atomica que necesita coercion —por ejemplo `raw`— conserva aqui su
#'   vector original.
#'   `invalidos` y `posiciones` marcan los bytes UTF-8 inválidos aislados.
#'   `analizable` declara si la columna pasa por las etapas de texto y `motivo`
#'   dice por qué no, cuando corresponde.
#' @noRd
.texto_analizable <- function(x) {
  if (is.raw(x)) {
    return(list(
      valores = as.character(x), invalidos = rep(FALSE, length(x)),
      posiciones = integer(), analizable = TRUE, valores_identidad = x,
      motivo = NA_character_
    ))
  }
  if (!is.character(x) && !is.factor(x)) {
    if (!.analizable_como_texto(x)) {
      n <- length(x)
      return(list(
        valores = rep(NA_character_, n),
        invalidos = rep(FALSE, n),
        posiciones = integer(),
        analizable = FALSE,
        valores_identidad = x,
        motivo = .motivo_no_analizable_texto(x)
      ))
    }
    return(list(
      valores = x, invalidos = rep(FALSE, length(x)), posiciones = integer(),
      analizable = TRUE, valores_identidad = x, motivo = NA_character_
    ))
  }
  valores <- as.character(x)
  # Lo que R sabe convertir se convierte, no se descarta.
  #
  # `validUTF8()` mira los bytes, y los de un texto marcado `latin1` no son UTF-8
  # validos. Sin este paso, una columna de un CSV viejo en espanol -el caso mas
  # comun que hay en datos publicos de la region- perdia todos sus valores
  # acentuados: `CAFE`, `ANO` y `NUMERO` sobrevivian y `CAFE con tilde`, `ANO con
  # tilde` y `NUMERO con tilde` se volvian NA. El perfil informaba entonces
  # `n_distintos = 2` sobre cinco valores distintos y `n_faltantes = 0`, sin
  # declarar nada: informar como medido lo que se descarto, que es exactamente lo
  # que el paquete promete no hacer.
  #
  # `Encoding()` dice `latin1` cuando R conoce la codificacion, y ahi `enc2utf8()`
  # convierte sin perder nada. Lo que queda invalido despues de eso es texto cuya
  # codificacion nadie declaro y no se puede adivinar; eso si se descarta, y se
  # informa en `invalidos` y `posiciones`.
  # `bytes` no declara una codificacion: declara que R no debe traducir la
  # cadena. Eso es correcto para bytes arbitrarios, pero una cadena marcada asi
  # puede contener UTF-8 valido. En ese caso se puede tratar sin perdida:
  # marcarla como UTF-8 antes de que lleguen `tolower()`, `trimws()` o las
  # expresiones regulares. Los bytes invalidos siguen el camino de texto no
  # descifrable de abajo.
  bytes_validos <- !is.na(valores) & Encoding(valores) == "bytes" &
    validUTF8(valores)
  if (any(bytes_validos)) {
    marcados <- valores[bytes_validos]
    Encoding(marcados) <- "UTF-8"
    valores[bytes_validos] <- marcados
  }
  declarados <- Encoding(valores) %in% c("latin1", "UTF-8")
  if (any(declarados)) {
    convertidos <- suppressWarnings(
      tryCatch(enc2utf8(valores[declarados]), error = function(e) NULL)
    )
    if (!is.null(convertidos) && length(convertidos) == sum(declarados)) {
      valores[declarados] <- convertidos
    }
  }
  invalidos <- !is.na(valores) & !validUTF8(valores)
  posiciones <- which(invalidos)
  if (length(posiciones)) valores[posiciones] <- NA_character_
  # Un texto con bytes UTF-8 validos puede llegar con marca `unknown` (por
  # ejemplo, desde `read.csv()`); bajo `LC_CTYPE=C`, `trimws()`, `tolower()` y
  # varias primitivas de R intentan traducirlo y advierten o comparan distinto.
  # La marca no cambia los bytes ni el texto publicado: sólo fija la forma de
  # trabajo, igual que `.nombres_para_operar()` para los identificadores.
  validos <- !is.na(valores) & validUTF8(valores)
  if (any(validos)) {
    marcados <- valores[validos]
    Encoding(marcados) <- "UTF-8"
    valores[validos] <- marcados
  }
  list(
    valores = valores, invalidos = invalidos, posiciones = posiciones,
    # La igualdad y la descripcion textual tienen universos distintos. La
    # primera puede contar bytes invalidos sin decodificarlos; la segunda los
    # excluye y los declara mediante `posiciones`.
    analizable = TRUE, valores_identidad = x, motivo = NA_character_
  )
}

# Copia operativa de una tabla: los factores se interpretan como texto para
# que los métodos no dependan de la versión de R ni de stringsAsFactors.
# El perfil conserva la columna original; esta función sólo se usa al ejecutar
# contratos que leen valores.
.normalizar_columnas_texto <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(tabla)
  tabla <- .tabla_base(tabla)
  factores <- vapply(tabla, is.factor, logical(1L))
  if (!any(factores)) return(tabla)
  salida <- tabla
  for (nombre in names(salida)[factores]) {
    salida[[nombre]] <- .texto_analizable(salida[[nombre]])$valores
  }
  salida
}

.columnas_identicas <- function(x, y) {
  identical(class(x), class(y)) &&
    identical(is.na(x), is.na(y)) &&
    identical(x, y)
}

.nombre_paquete <- function() {
  nombre <- utils::packageName()
  if (is.null(nombre)) "lupa" else nombre
}

.version_paquete <- function() {
  nombre <- .nombre_paquete()
  tryCatch(
    as.character(utils::packageVersion(nombre)),
    error = function(e) "desarrollo"
  )
}

.pegar_nombres <- function(x) {
  paste(x, collapse = " + ")
}

# Una corrida larga y callada no se distingue de una colgada, pero una barra en
# la salida de un guion es ruido que despues hay que filtrar. La decision es la
# misma en todos los caminos, asi que vive en un solo lugar: `interactive()`
# decide y `options(lupa.progreso = )` manda sobre eso en los dos sentidos.
.progreso_activo <- function(total, minimo) {
  if (!isTRUE(getOption("lupa.progreso", interactive()))) return(FALSE)
  # Sin total conocido no hay porcentaje que mostrar, y un porcentaje contra un
  # total inventado es peor que ninguno. Debajo del minimo la corrida termina
  # antes de que la barra sirva.
  is.numeric(total) && length(total) == 1L && !is.na(total) &&
    is.finite(total) && total >= minimo
}

# Cada puerta comprobaba un subconjunto distinto de lo que un objeto necesita, y
# ninguna estaba completa. Medido quitando un componente por vez:
# `comparar_perfiles()` rechaza un perfil sin `columnas`, `meta` o `hallazgos` y
# ACEPTA uno sin `general`; `print.perfil()` no mira nada y sin embargo necesita
# `general`, asi que fallaba con "attempt to set an attribute on NULL". No hay
# una definicion unica de objeto valido aplicada en las dos.
#
# Esto la da, y las dos la usan. El mensaje nombra la funcion que lo produce,
# que es lo que el usuario necesita para saber que hacer.
.validar_objeto_lupa <- function(x, clase, componentes, origen) {
  if (!inherits(x, clase)) {
    stop("`x` debe ser un objeto de clase `", clase, "`, producido por ",
         origen, ".", call. = FALSE)
  }
  if (!length(componentes)) return(invisible(x))
  if (!is.list(x)) {
    stop("`x` no tiene la estructura de un objeto `", clase,
         "`: falta ", paste(componentes, collapse = ", "), ".", call. = FALSE)
  }
  faltan <- componentes[!componentes %in% names(x)]
  if (length(faltan)) {
    stop("`x` no tiene la estructura de un objeto `", clase, "`: falta ",
         paste(faltan, collapse = ", "), ". Producirlo con ", origen, ".",
         call. = FALSE)
  }
  invisible(x)
}

# Una medicion vacia que SI viene de `medir()` no es una entrada invalida: es una
# corrida donde ninguna metrica fue aplicable, y `medir()` ya declaro por que en
# su `cobertura_metricas`. Tres validaciones distintas la rechazaban con "debe
# ser ... producido por medir()", que afirma algo falso teniendo el motivo
# verdadero a mano. El texto se arma en UN solo lugar para que las tres digan lo
# mismo: tenerlo escrito tres veces es como se desincronizan.
.error_medicion_sin_medidas <- function(medicion, argumento, origen) {
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  motivo <- if (is.data.frame(cobertura) && nrow(cobertura) &&
                "motivo" %in% names(cobertura)) {
    paste0(" Motivo declarado por `medir()`: ",
           paste(unique(as.character(cobertura$motivo)), collapse = "; "))
  } else {
    ""
  }
  stop(
    "`", argumento, "` viene de ", origen,
    " pero no tiene ninguna medida que evaluar.", motivo,
    " Se puede revisar `attr(", argumento, ", \"cobertura_metricas\")`.",
    call. = FALSE
  )
}

# Declara la codificacion de las columnas de texto que no la declaran. NO cambia
# los bytes: dice lo que ya son.
#
# Por que hace falta en la ENTRADA y no en cada sitio: bajo un locale que no es
# UTF-8, una cadena con `Encoding()` en `unknown` se interpreta segun el locale,
# y ahi cualquier operacion que exija UTF-8 aborta. Se encontraron DOS caminos
# distintos que morian asi -`chartr()` en el pliegue de mayusculas y un
# `gsub(..., perl = TRUE)` con `(*UTF)` en la proteccion de grafemas-, y buscar
# el tercero de a uno es perseguir la cola. Marcar una vez, al entrar, los cubre
# todos.
#
# `read.csv()` deja `unknown` en cualquier CSV con tildes: es el caso mas comun
# que existe en espanol, no un borde.
#
# Solo se marca lo que YA es UTF-8 valido. Un texto realmente latin1 tiene bytes
# que no lo son, `validUTF8()` da `FALSE` y se lo deja como esta: el paquete lo
# trata despues por el camino de `texto_no_descifrable`, que existe para eso.
.marcar_utf8_textos <- function(textos) {
  # Sin esta guarda, una tabla con `names()` en `NULL` moria en `Encoding(NULL)`
  # con "a character vector argument expected", un error de R que no dice nada
  # del dato. El paquete ya tiene su propio mensaje para ese caso -"`names` debe
  # ser un vector de caracteres"- y este marcado, que corre antes que la
  # validacion, se lo estaba comiendo.
  if (!is.character(textos) || !length(textos)) return(textos)
  sin_marca <- !is.na(textos) & Encoding(textos) == "unknown"
  if (!any(sin_marca)) return(textos)
  marcables <- sin_marca & validUTF8(textos)
  if (!any(marcables)) return(textos)
  trozo <- textos[marcables]
  Encoding(trozo) <- "UTF-8"
  textos[marcables] <- trozo
  textos
}

# `identical()` no sirve para decidir si el marcado cambio algo: su respuesta
# DEPENDE DEL LOCALE. Con los mismos bytes, una cadena `unknown` y otra `UTF-8`
# son identicas bajo un locale UTF-8 -ahi lo nativo ya es UTF-8- y distintas bajo
# `C`. Medido en las dos: `TRUE` bajo `es_UY.UTF-8`, `FALSE` bajo `LC_ALL=C`.
#
# O sea que la condicion acertaba justo donde hacia falta y era un no-op donde no,
# por casualidad. Dejar una guarda cuya respuesta depende del locale, dentro del
# marcado que existe para que el resultado NO dependa del locale, es contradecir
# el motivo de la funcion. Se compara la marca, que es lo que el marcado cambia.
.marca_cambio <- function(despues, antes) {
  # `Encoding()` exige un vector de caracteres. Lo que no lo es no se marco, asi
  # que no cambio: contestar `FALSE` deja pasar el dato intacto y deja que lo
  # rechace la validacion del paquete, con su mensaje, y no esta funcion.
  if (!is.character(despues) || !is.character(antes)) return(FALSE)
  !identical(Encoding(despues), Encoding(antes))
}

.marcar_utf8_tabla <- function(tabla) {
  if (!inherits(tabla, "data.frame") || !ncol(tabla)) return(tabla)
  # `names()` son datos publicados del usuario, no texto que el paquete pueda
  # declarar de nuevo. Las comparaciones y los ordenamientos que los necesitan
  # usan `.nombres_para_operar()`; conservarlos aqui deja bytes y marca iguales
  # a la entrada por construccion, y hace que un nombre publicado siga
  # indexando la tabla que lo produjo incluso bajo `LC_CTYPE = "C"`.
  for (i in seq_along(tabla)) {
    columna <- tabla[[i]]
    if (!length(columna)) next
    # Los NIVELES de un factor son cadenas igual, y sin esto quedaban afuera: la
    # misma columna como `character` no emitia un aviso y como `factor` emitia
    # veintiuno bajo un locale que no es UTF-8. Recorrer solo `is.character()`
    # dejaba la mitad del problema sin tocar.
    if (is.factor(columna)) {
      niveles <- levels(columna)
      if (!length(niveles)) next
      marcados <- .marcar_utf8_textos(niveles)
      if (.marca_cambio(marcados, niveles)) {
        levels(tabla[[i]]) <- marcados
      }
      next
    }
    if (!is.character(columna)) next
    marcados <- .marcar_utf8_textos(columna)
    if (.marca_cambio(marcados, columna)) tabla[[i]] <- marcados
  }
  tabla
}
