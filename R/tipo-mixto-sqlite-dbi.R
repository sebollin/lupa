# ---- Columnas de SQLite que guardan valores de tipos distintos ------------
#
# SQLite deja guardar en una columna valores de cualquier tipo, y RSQLite decide
# la clase de lo que lee por el primer valor no nulo que llega y convierte el
# resto (medido con RSQLite 3.53.3): un `'ND'` despues de un numero se lee como
# `0` y el mismo `'ND'` primero hace leer todo como texto; un entero de 64 bits
# primero hace leer `2.5` como `2`, y un real primero redondea el entero a un
# doble. Con un `ORDER BY RANDOM()`, con la tabla en otro orden o con otra tanda
# de filas, la misma columna daba cifras distintas, siempre como calculadas.
#
# El aviso del controlador ("mixed type") no alcanza para decidir: depende de
# las filas que lleguen juntas, no reconoce todos los nombres de columna y no
# llega cuando la mezcla cae en tandas distintas. La mezcla se mide en el motor,
# una sentencia por tabla para todas las columnas que la necesitan, y de esa
# medicion sale la unica forma de leer cada columna mezclada, la misma para la
# muestra, la muestra del motor, la lectura por bloques, `medir_dbi()` y las
# colecciones:
#
# * solo numeros (entero y real): como doble (`CAST(x AS REAL)`); si algun
#   entero no cabe exacto en un doble, se declara como un entero ancho;
# * numeros y texto: como texto, con cada numero escrito en R por su valor
#   (ver `.texto_real_sqlite_dbi()`) y cada texto tal cual;
# * con algun BLOB: `CAST(x AS BLOB)` en todas las filas, que no corta bytes.
#
# Las columnas con tipo declarado entran en la misma medicion: la afinidad de su
# tipo (`.afinidad_sqlite_dbi()`) solo dice que valores convierte SQLite al
# guardarlos, no que guarde un solo tipo -un texto que no se convierte a numero
# queda como texto en una columna `INTEGER`, `NUMERIC` o `BOOLEAN`, y una columna
# generada guarda lo que su expresion devuelva-, y su controlador las lee por el
# tipo declarado, con lo que ese texto llegaba como `0` o como faltante. Lo que
# decide la lectura es lo que el motor guarda de verdad (`typeof()`): una
# columna tipada que guarda un solo tipo de almacenamiento no cambia de lectura.
# Solo cuenta como mezcla lo que la afinidad no resuelve: numeros con texto,
# numeros con BLOB o texto con BLOB; los enteros y los reales de una columna
# tipada los une el controlador en un doble, como siempre.

# El primer entero que un doble no representa con exactitud: la frontera de
# toda la via DBI (`.MAX_ENTERO_EXACTO_DBI`), escrita como literal de SQLite.
.SQL_ENTERO_EXACTO_SQLITE_DBI <- "9007199254740992"

# El primer entero que no entra en un `integer` de R (2^31): el controlador lo
# lee como `integer64` y trunca el real que llegue despues.
.SQL_ENTERO_32_SQLITE_DBI <- "2147483648"

# Los nombres de las columnas auxiliares con que se leen aparte el real y el
# BLOB de una columna mezclada. Cada nombre se elige en la medicion para que no
# coincida con ninguna columna de la tabla (`.nombres_auxiliares_tipo_mixto_dbi()`),
# y el armado solo consume las auxiliares de esa lista
# (`.armar_tipo_mixto_sqlite_dbi()`): una columna del usuario que se llame asi
# se lee como cualquier otra.
.PREFIJO_REAL_TIPO_MIXTO_DBI <- "__lupa_tipo_mixto_real_"
.PREFIJO_BLOB_TIPO_MIXTO_DBI <- "__lupa_tipo_mixto_blob_"

.es_sqlite_tipo_mixto_dbi <- function(conexion) {
  grepl("sqlite", .senas_conexion_dbi(conexion), fixed = TRUE, useBytes = TRUE)
}

# La afinidad de un tipo declarado de SQLite, por las cinco reglas de su
# documentacion y en su orden: contiene `INT` -> entera; `CHAR`, `CLOB` o `TEXT`
# -> texto; `BLOB` o sin tipo -> ninguna; `REAL`, `FLOA` o `DOUB` -> real; el
# resto -> numerica. Devuelve `NA` para el tipo que no se conoce.
.afinidad_sqlite_dbi <- function(declarado) {
  declarado <- .mayusculas_ascii(trimws(as.character(declarado)))
  tiene <- function(partes) {
    Reduce(`|`, lapply(partes, function(p) {
      grepl(p, declarado, fixed = TRUE, useBytes = TRUE)
    }))
  }
  salida <- rep("numerica", length(declarado))
  salida[tiene(c("REAL", "FLOA", "DOUB"))] <- "real"
  salida[!nzchar(declarado) | tiene("BLOB")] <- "ninguna"
  salida[tiene(c("CHAR", "CLOB", "TEXT"))] <- "texto"
  salida[tiene("INT")] <- "entera"
  # `ANY` guarda el valor tal cual en una tabla `STRICT` y tiene afinidad
  # numerica en una que no lo es. Saber si la tabla es `STRICT` exigiria pasar
  # la identidad de la tabla a cada quien mide y leer su `CREATE TABLE`: la
  # afinidad queda `desconocida`, que se mide como la sin afinidad -cada tipo
  # cuenta aparte- y no se afirma en la cobertura.
  salida[declarado %in% "ANY"] <- "desconocida"
  salida[is.na(declarado)] <- NA_character_
  salida
}

# La afinidad de cada una de `campos`, que decide cuales se miden: todas las que
# la tabla devuelve, porque ninguna afinidad impide que SQLite guarde otro tipo
# -`NA` para la que no se puede conocer, que no se mide-. `tipos_pragma` trae el
# tipo declarado de cada columna, con `""` para la que no lo tiene. `PRAGMA
# table_info` no lista las columnas generadas (`x AS (...)`), que la consulta si
# devuelve: para una columna que la tabla devuelve y el catalogo no nombra decide
# lo que el controlador dice de ella sin filas (`prototipo`), que sigue su tipo
# declarado: sin tipo declarado llega `logical`, con `BLOB`, `blob`, y con un
# tipo con afinidad, su clase.
.afinidades_sqlite_dbi <- function(tipos_pragma, campos, prototipo = NULL) {
  salida <- rep(NA_character_, length(campos))
  if (is.null(tipos_pragma) || !length(tipos_pragma) ||
      is.null(names(tipos_pragma)) || !length(campos)) {
    return(salida)
  }
  posicion <- match(
    .nombres_para_operar(as.character(campos)),
    .nombres_para_operar(names(tipos_pragma))
  )
  declarado <- trimws(as.character(tipos_pragma))[posicion]
  salida <- .afinidad_sqlite_dbi(declarado)
  fuera_del_catalogo <- is.na(posicion)
  if (any(fuera_del_catalogo) && is.data.frame(prototipo)) {
    en_prototipo <- match(
      .nombres_para_operar(as.character(campos)),
      .nombres_para_operar(names(prototipo))
    )
    salida[fuera_del_catalogo] <- vapply(
      which(fuera_del_catalogo), function(i) {
        j <- en_prototipo[[i]]
        if (is.na(j)) return(NA_character_)
        valor <- prototipo[[j]]
        if (is.logical(valor) || .es_columna_blob(valor)) return("ninguna")
        if (is.character(valor)) return("texto")
        if (is.integer(valor) || inherits(valor, "integer64")) return("entera")
        "numerica"
      }, character(1L)
    )
  }
  salida
}

# La sentencia que mide la mezcla. Por columna, una suma de los codigos distintos de los tipos que guarda: cada tipo
# tiene una potencia de dos -entero 1 (32 si su valor absoluto es de al menos
# 2^31, que no entra en un `integer` de R, 64 si es exactamente -2^31, que R usa
# como faltante, y 16 si es mayor que 2^53, que un doble no representa), real 2, texto 4, BLOB 8; el nulo no cuenta-,
# asi que la suma es el conjunto de tipos, y la columna esta mezclada si guarda
# mas de uno: `COUNT(DISTINCT typeof(x)) > 1`. Un recorrido de la tabla, sin
# ordenar ni traer filas, y una columna de resultado por columna medida: una
# tabla de SQLite no tiene mas columnas de las que una sentencia puede devolver
# (2000), asi que una sentencia mide la tabla entera. Medido en SQLite con
# 100.000 filas y cinco columnas:
# 29 ms, contra 97 ms de contar los tipos distintos con tres banderas aparte y
# 59 ms de leer la tabla (`SELECT *`).
#
# Con `tipo_declarado_sql` -una expresion por columna que da su tipo declarado,
# ver `.sql_tipo_declarado_sqlite_dbi()`- la misma sentencia trae tambien el
# tipo de cada columna, para quien no lo conoce antes de medir.
.sql_deteccion_tipo_mixto_dbi <- function(tabla_sql, columnas_sql,
                                          tipo_declarado_sql = NULL) {
  limite <- .SQL_ENTERO_EXACTO_SQLITE_DBI
  partes <- vapply(seq_along(columnas_sql), function(i) {
    x <- columnas_sql[[i]]
    paste0(
      "SUM(DISTINCT CASE typeof(", x, ") WHEN 'integer' THEN CASE WHEN ", x,
      " > ", limite, " OR ", x, " < -", limite, " THEN 16 WHEN ", x,
      " = -", .SQL_ENTERO_32_SQLITE_DBI, " THEN 64 WHEN ", x,
      " >= ", .SQL_ENTERO_32_SQLITE_DBI, " OR ", x, " <= -",
      .SQL_ENTERO_32_SQLITE_DBI, " THEN 32 ELSE 1 END ",
      "WHEN 'real' THEN 2 WHEN 'text' THEN 4 WHEN 'blob' THEN 8 END) AS ",
      "lupa_tm_tipos_", i,
      if (length(tipo_declarado_sql) == length(columnas_sql)) {
        paste0(", ", tipo_declarado_sql[[i]], " AS lupa_tm_decl_", i)
      } else ""
    )
  }, character(1L))
  paste0("SELECT ", paste(partes, collapse = ", "), " FROM ", tabla_sql)
}

# El tipo declarado de cada una de `columnas` de la tabla `referencia`, como
# una subconsulta escalar sobre `pragma_table_xinfo()` -que lista tambien las
# columnas generadas-, para ir dentro de la sentencia que mide la mezcla: quien
# no tiene el catalogo de la tabla -`relaciones_coleccion()`- lo lee sin una
# sentencia mas. `NULL` si el nombre de la tabla no se puede escribir.
.sql_tipo_declarado_sqlite_dbi <- function(conexion, referencia, columnas) {
  piezas <- tryCatch(
    .piezas_tabla_cardinalidad_dbi(referencia), error = function(e) NULL
  )
  if (is.null(piezas) || length(piezas$tabla) != 1L || is.na(piezas$tabla)) {
    return(NULL)
  }
  literal <- function(x) as.character(DBI::dbQuoteString(conexion, x))
  argumentos <- paste0(
    literal(piezas$tabla),
    if (length(piezas$esquema) == 1L && !is.na(piezas$esquema)) {
      paste0(", ", literal(piezas$esquema))
    } else ""
  )
  vapply(columnas, function(columna) {
    paste0(
      "(SELECT type FROM pragma_table_xinfo(", argumentos, ") WHERE name = ",
      literal(columna), " COLLATE NOCASE)"
    )
  }, character(1L), USE.NAMES = FALSE)
}

.lectura_tipo_mixto_vacia_dbi <- function() {
  data.frame(
    columna = character(), clase = character(), ancho = logical(),
    lectura = character(), fuente = character(), afinidad = character(),
    auxiliar_real = character(), auxiliar_blob = character(),
    stringsAsFactors = FALSE
  )
}

# Los nombres de las auxiliares de cada fila de `tipo_mixto`: uno para el real
# y otro para el BLOB, que no coinciden con ninguna columna de la tabla
# (`nombres_tabla`) ni entre si.
.auxiliares_tipo_mixto_dbi <- function(tipo_mixto, nombres_tabla) {
  if (!NROW(tipo_mixto)) return(tipo_mixto)
  atributos <- attributes(tipo_mixto)[c("ok", "sql", "medidas")]
  tipo_mixto$auxiliar_real <- .nombres_auxiliares_tipo_mixto_dbi(
    .PREFIJO_REAL_TIPO_MIXTO_DBI, nrow(tipo_mixto), nombres_tabla
  )
  tipo_mixto$auxiliar_blob <- .nombres_auxiliares_tipo_mixto_dbi(
    .PREFIJO_BLOB_TIPO_MIXTO_DBI, nrow(tipo_mixto),
    c(nombres_tabla, tipo_mixto$auxiliar_real)
  )
  for (nombre in names(atributos)) {
    if (!is.null(atributos[[nombre]])) attr(tipo_mixto, nombre) <- atributos[[nombre]]
  }
  tipo_mixto
}

# Un nombre por columna mezclada para una auxiliar, que no coincide con ninguna
# columna de la tabla (`nombres_tabla`) ni con otra auxiliar, sin distinguir
# mayusculas: SQLite no las distingue.
.nombres_auxiliares_tipo_mixto_dbi <- function(prefijo, n, nombres_tabla) {
  ocupados <- .minusculas_ascii(as.character(nombres_tabla))
  salida <- character(n)
  for (k in seq_len(n)) {
    nombre <- paste0(prefijo, k)
    while (.minusculas_ascii(nombre) %in% ocupados) nombre <- paste0(nombre, "_")
    ocupados <- c(ocupados, .minusculas_ascii(nombre))
    salida[[k]] <- nombre
  }
  salida
}

# Que clase de mezcla guarda una columna, segun el conjunto de tipos que mide
# la sentencia (`mascara`, ver `.sql_deteccion_tipo_mixto_dbi()`) y la afinidad
# de su tipo declarado: `NULL` si no es una mezcla. Sin afinidad -sin tipo
# declarado o `BLOB`- cada tipo cuenta aparte (entero, real, texto, BLOB); con
# afinidad, los numeros son uno solo, que el controlador junta en un doble, y
# la mezcla es de numeros con texto, de numeros con BLOB o de texto con BLOB.
# `clase` es `numeros`, `texto` o `blob`, y `ancho` dice si guarda un entero
# que un doble no representa.
.clase_mezcla_tipo_mixto_dbi <- function(mascara, afinidad) {
  tiene <- function(bit) bitwAnd(mascara, bit) > 0L
  entero <- tiene(1L) || tiene(16L) || tiene(32L) || tiene(64L)
  tipos <- if (afinidad %in% c("ninguna", "desconocida")) {
    entero + tiene(2L) + tiene(4L) + tiene(8L)
  } else {
    # Los numeros son uno solo, salvo un entero que no entra en 32 bits junto
    # a un real: el controlador lee `integer64` y trunca el real.
    (entero || tiene(2L)) + (tiene(2L) && (tiene(16L) || tiene(32L))) +
      tiene(4L) + tiene(8L)
  }
  # `-2^31` es el faltante de los enteros de R: el controlador lo entrega como
  # `NA`, aunque sea el unico tipo que guarda la columna.
  if (tipos < 2L && !tiene(64L)) return(NULL)
  clase <- if (tiene(8L)) "blob" else if (tiene(4L)) "texto" else "numeros"
  list(clase = clase, ancho = identical(clase, "numeros") && tiene(16L))
}

# Mide la mezcla de `campos` (con sus expresiones `campos_sql`) en `tabla_sql`
# y devuelve una fila por columna mezclada: `clase` es `numeros`, `texto` o
# `blob`, `ancho` dice si guarda un entero que un doble no representa,
# `lectura` es la expresion con que se lee, `fuente` la columna de
# `campos_sql` que la lectura reemplaza y `afinidad` la de su tipo declarado
# (ver `.afinidad_sqlite_dbi()`); `auxiliar_real` y `auxiliar_blob` son los
# nombres de sus columnas auxiliares, que no coinciden con ninguna de
# `nombres_tabla`. `consultar(sql)` manda la sentencia
# con la cuenta de quien llama -el presupuesto de `perfilar_dbi()`, el contador
# de `medir_dbi()`- y devuelve sus filas o `NULL`. Sin columnas que medir no
# manda nada; si la sentencia no se pudo leer, la lectura queda como estaba y
# `attr(, "ok")` es `FALSE`.
#
# `afinidad` trae la afinidad de cada columna (`.afinidades_sqlite_dbi()`), con
# `NA` para la que no se mide. Sin ella, quien la manda es la sentencia: con
# `tipo_declarado_sql` trae el tipo de cada columna y la afinidad sale de ahi,
# y una columna sin tipo legible no se mide.
.detectar_tipo_mixto_sqlite_dbi <- function(conexion, tabla_sql, campos,
                                            campos_sql, afinidad, consultar,
                                            nombres_tabla = campos,
                                            tipo_declarado_sql = NULL) {
  salida <- .lectura_tipo_mixto_vacia_dbi()
  attr(salida, "ok") <- TRUE
  medir <- if (is.null(afinidad)) rep(TRUE, length(campos)) else {
    !is.na(as.character(afinidad))
  }
  if (!.es_sqlite_tipo_mixto_dbi(conexion) || !length(campos) ||
      length(medir) != length(campos) || !any(medir) ||
      length(tabla_sql) != 1L || is.na(tabla_sql)) {
    return(salida)
  }
  indices <- which(medir)
  declarado_sql <- if (length(tipo_declarado_sql) == length(campos)) {
    tipo_declarado_sql[indices]
  } else NULL
  sql <- .sql_deteccion_tipo_mixto_dbi(
    tabla_sql, campos_sql[indices], declarado_sql
  )
  attr(salida, "sql") <- sql
  fila <- consultar(sql)
  if (!is.data.frame(fila) || nrow(fila) != 1L) {
    attr(salida, "ok") <- FALSE
    return(salida)
  }
  for (k in seq_along(indices)) {
    celda <- fila[[paste0("lupa_tm_tipos_", k)]]
    mascara <- if (is.null(celda) || !length(celda)) NA_integer_ else {
      suppressWarnings(as.integer(as.character(celda[[1L]])))
    }
    if (is.na(mascara)) next
    i <- indices[[k]]
    afinidad_i <- if (!is.null(afinidad)) as.character(afinidad[[i]]) else {
      # El tipo viene en la misma sentencia; sin tipo legible, no se mide.
      declarado <- fila[[paste0("lupa_tm_decl_", k)]]
      if (is.null(declarado) || !length(declarado) || is.na(declarado[[1L]])) {
        NA_character_
      } else .afinidad_sqlite_dbi(as.character(declarado[[1L]]))
    }
    if (is.na(afinidad_i)) next
    mezcla <- .clase_mezcla_tipo_mixto_dbi(mascara, afinidad_i)
    if (is.null(mezcla)) next
    salida <- rbind(salida, data.frame(
      columna = as.character(campos[[i]]), clase = mezcla$clase,
      ancho = mezcla$ancho,
      lectura = .expresion_tipo_mixto_dbi(campos_sql[[i]], mezcla$clase),
      fuente = as.character(campos_sql[[i]]), afinidad = afinidad_i,
      auxiliar_real = NA_character_, auxiliar_blob = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  salida <- .auxiliares_tipo_mixto_dbi(salida, nombres_tabla)
  attr(salida, "ok") <- TRUE
  attr(salida, "sql") <- sql
  salida
}

# La lista SELECT con que se lee una columna mezclada, sin el alias de la
# columna. La de numeros y texto son dos columnas: el texto -los enteros con
# sus digitos, que `CAST` conserva exactos, y el texto tal cual- y el real
# como real, que R escribe (`.armar_tipo_mixto_sqlite_dbi()`).
#
# Para comparar valores entre tablas (`comparar`, la de `relaciones_coleccion()`)
# la que guarda algun BLOB se lee en tres columnas -el texto, el real y el
# BLOB-, y el armado escribe cada valor con su clase de almacenamiento
# (`.clave_almacenamiento_dbi()`): R no compara BLOB por sus bytes, y el motor
# no iguala un BLOB con un texto.
.expresion_tipo_mixto_dbi <- function(x, clase, comparar = FALSE) {
  if (identical(clase, "blob") && isTRUE(comparar)) clase <- "texto"
  switch(
    clase,
    numeros = paste0("CAST(", x, " AS REAL)"),
    blob = paste0("CAST(", x, " AS BLOB)"),
    texto = paste0(
      "CASE typeof(", x, ") WHEN 'integer' THEN CAST(", x, " AS TEXT) ",
      "WHEN 'text' THEN ", x, " END"
    )
  )
}

# La proyeccion de lectura con las columnas mezcladas leidas por su forma.
# `expresiones` es la proyeccion de cada columna; solo se reemplaza la que es
# la misma columna que se midio -una con otra conversion ya tiene su propia
# lectura-. El nombre de la columna no cambia, y la de numeros y texto lleva
# detras su auxiliar del real (y, al comparar, la del BLOB), con los nombres de
# la medicion. `comparar`: ver `.expresion_tipo_mixto_dbi()`.
.proyeccion_tipo_mixto_dbi <- function(conexion, campos, expresiones,
                                       tipo_mixto, comparar = FALSE) {
  if (!NROW(tipo_mixto) || !length(campos) ||
      length(expresiones) != length(campos)) {
    return(expresiones)
  }
  posiciones <- match(
    .nombres_para_operar(as.character(tipo_mixto$columna)),
    .nombres_para_operar(as.character(campos))
  )
  for (k in seq_along(posiciones)) {
    i <- posiciones[[k]]
    if (is.na(i)) next
    x <- as.character(tipo_mixto$fuente[[k]])
    if (!identical(as.character(expresiones[[i]]), x)) next
    clase <- as.character(tipo_mixto$clase[[k]])
    con_blob <- identical(clase, "blob") && isTRUE(comparar)
    con_real <- identical(clase, "texto") || con_blob
    expresiones[[i]] <- paste0(
      .expresion_tipo_mixto_dbi(x, clase, comparar = comparar), " AS ", x,
      if (con_real) paste0(
        ", CASE WHEN typeof(", x, ") = 'real' THEN ", x, " END AS ",
        .citar_identificador_dbi(conexion, tipo_mixto$auxiliar_real[[k]])
      ) else "",
      if (con_blob) paste0(
        ", CASE WHEN typeof(", x, ") = 'blob' THEN ", x, " END AS ",
        .citar_identificador_dbi(conexion, tipo_mixto$auxiliar_blob[[k]])
      ) else ""
    )
  }
  expresiones
}

# Como escribe R un real leido de una columna de numeros y texto: un real
# entero con sus digitos -`1`, `1.0` y `1e15` real coinciden con el entero que
# vale lo mismo-, `0` para el cero con signo y, el resto, `as.character()`. R
# escribe con 15 cifras significativas: `as.character(0.1 + 0.2)` es `"0.3"`,
# igual que el de `0.3`. Si ese texto no vuelve al mismo doble se escriben 17,
# que siempre vuelven: dos reales distintos no quedan como uno.
.texto_real_sqlite_dbi <- function(x) {
  x <- as.numeric(x)
  salida <- rep(NA_character_, length(x))
  presentes <- !is.na(x)
  if (!any(presentes)) return(salida)
  v <- x[presentes]
  v[v == 0] <- 0
  entero <- is.finite(v) & v == trunc(v) & abs(v) < 2^63
  texto <- character(length(v))
  texto[entero] <- sprintf("%.0f", v[entero])
  otros <- which(!entero)
  if (length(otros)) {
    corto <- as.character(v[otros])
    vuelve <- !is.finite(v[otros]) |
      suppressWarnings(as.numeric(corto)) == v[otros]
    corto[!vuelve] <- sprintf("%.17g", v[otros][!vuelve])
    texto[otros] <- corto
  }
  salida[presentes] <- texto
  salida
}

# La lectura mezclada vigente: la tabla de `.detectar_tipo_mixto_sqlite_dbi()`
# de la tabla que se esta leyendo, y si se lee para comparar. La fija quien
# arma la proyeccion, mientras lee esa tabla, y devuelve la anterior.
.fijar_lectura_tipo_mixto_dbi <- function(tipo_mixto, comparar = FALSE) {
  previa <- .CONTEXTO_MOTOR$lectura_tipo_mixto
  .CONTEXTO_MOTOR$lectura_tipo_mixto <- if (NROW(tipo_mixto)) {
    list(tipo_mixto = tipo_mixto, comparar = isTRUE(comparar))
  } else NULL
  invisible(previa)
}

.restaurar_lectura_tipo_mixto_dbi <- function(previa) {
  .CONTEXTO_MOTOR$lectura_tipo_mixto <- previa
  invisible(NULL)
}

# Junta en su columna lo que la lectura de una columna mezclada trae aparte: el
# real, que escribe R, y al comparar el BLOB, con su clase de almacenamiento.
# Cada valor que llega del motor pasa por aca (`.traer_filas_dbi()`,
# `.traer_consulta_dbi()`). Solo consume las auxiliares de la lectura vigente
# (`.fijar_lectura_tipo_mixto_dbi()`), por su nombre exacto y pegadas detras de
# su columna: una columna del usuario no se toca.
.armar_tipo_mixto_sqlite_dbi <- function(conexion, datos) {
  vigente <- .CONTEXTO_MOTOR$lectura_tipo_mixto
  if (is.null(vigente) || !is.data.frame(datos) || ncol(datos) < 2L ||
      !.es_sqlite_tipo_mixto_dbi(conexion)) {
    return(datos)
  }
  tipo_mixto <- vigente$tipo_mixto
  quitar <- integer()
  nombres <- as.character(names(datos))
  for (k in seq_len(nrow(tipo_mixto))) {
    principal <- which(.nombres_para_operar(nombres) %in%
                         .nombres_para_operar(tipo_mixto$columna[[k]]))
    if (length(principal) != 1L) next
    p_real <- principal + 1L
    if (p_real > length(nombres) ||
        !identical(nombres[[p_real]], tipo_mixto$auxiliar_real[[k]])) next
    p_blob <- principal + 2L
    con_blob <- isTRUE(vigente$comparar) &&
      identical(as.character(tipo_mixto$clase[[k]]), "blob") &&
      p_blob <= length(nombres) &&
      identical(nombres[[p_blob]], tipo_mixto$auxiliar_blob[[k]])
    texto <- datos[[principal]]
    salida <- if (is.character(texto)) texto else {
      rep(NA_character_, nrow(datos))
    }
    real <- datos[[p_real]]
    es_real <- !is.na(real)
    if (any(es_real)) {
      salida[es_real] <- .texto_real_sqlite_dbi(real[es_real])
    }
    if (con_blob) {
      salida <- .clave_almacenamiento_dbi(salida)
      blob <- datos[[p_blob]]
      if (is.list(blob)) {
        es_blob <- !vapply(blob, is.null, logical(1L))
        if (any(es_blob)) {
          salida[es_blob] <- .clave_almacenamiento_dbi(blob)[es_blob]
        }
      }
      quitar <- c(quitar, p_blob)
    }
    datos[[principal]] <- salida
    quitar <- c(quitar, p_real)
  }
  if (!length(quitar)) return(datos)
  tipos <- attr(datos, "lupa_tipos_odbc", exact = TRUE)
  datos <- datos[-quitar]
  if (!is.null(tipos)) attr(datos, "lupa_tipos_odbc") <- tipos
  datos
}

# La clave con que se compara un valor cuando la clase de almacenamiento
# cuenta: un BLOB (`b:` y sus bytes en hexadecimal) solo es igual a un BLOB de
# los mismos bytes, y todo lo demas (`v:` y su valor) se compara por valor
# como siempre -un numero con un numero o con un texto que se escribe igual-.
# Toda clave lleva su clase, asi que ningun texto del usuario puede escribirse
# como un BLOB. Acepta un vector de texto, numerico, de fechas, o una lista de
# BLOB (`blob`, o una lista de `raw` y otros valores); `NA` para el nulo.
.clave_almacenamiento_dbi <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    return(vapply(x, function(v) {
      if (is.null(v)) return(NA_character_)
      if (is.raw(v)) return(paste0("b:", paste(as.character(v), collapse = "")))
      if (length(v) != 1L || is.na(v)) return(NA_character_)
      .clave_almacenamiento_dbi(v)
    }, character(1L), USE.NAMES = FALSE))
  }
  valores <- if (is.double(x) && !inherits(x, c("integer64", "Date",
                                                  "POSIXt", "difftime"))) {
    .texto_real_sqlite_dbi(x)
  } else {
    v <- .valores_relacion(x)
    if (is.character(v)) v else as.character(v)
  }
  salida <- paste0("v:", valores)
  salida[is.na(valores)] <- NA_character_
  salida
}

# La lectura de las columnas de una tabla de SQLite que `perfilar_dbi()` va a leer: la mide con el presupuesto de la
# corrida, en la preparacion -el plan, que prepara igual, la anuncia en
# `presupuesto$tipos_anunciados`, que suma a los portones, sin ejecutarla (`ejecutar = FALSE`)- y devuelve la tabla de `.detectar_tipo_mixto_sqlite_dbi()`.
.lectura_tipo_mixto_perfil_dbi <- function(conexion, tabla_sql, campos,
                                           campos_sql, tipos_pragma,
                                           presupuesto = NULL,
                                           prototipo = NULL,
                                           ejecutar = TRUE) {
  afinidad <- .afinidades_sqlite_dbi(tipos_pragma, campos, prototipo)
  if (!isTRUE(ejecutar)) {
    # El plan anuncia la sentencia -la cuenta entre las que la corrida va a
    # mandar, en las mismas condiciones en que
    # `.detectar_tipo_mixto_sqlite_dbi()` la manda- sin ejecutarla: recorre la
    # tabla entera, y un plan no lee filas. La lectura que depende de su
    # resultado se decide en la corrida; el plan queda con la de siempre.
    salida <- .lectura_tipo_mixto_vacia_dbi()
    attr(salida, "ok") <- TRUE
    if (.es_sqlite_tipo_mixto_dbi(conexion) && length(campos) &&
        any(!is.na(afinidad)) && length(tabla_sql) == 1L &&
        !is.na(tabla_sql)) {
      # Aparte de `usadas`: lo que la corrida cuenta como emitido es lo que se
      # manda, y esta sentencia el plan no la manda (ver
      # `plan_perfilado_dbi()`).
      if (!is.null(presupuesto)) presupuesto$tipos_anunciados <- 1
      attr(salida, "sql") <- .sql_deteccion_tipo_mixto_dbi(
        tabla_sql, campos_sql[!is.na(afinidad)]
      )
      attr(salida, "anunciada") <- TRUE
    }
    return(salida)
  }
  .detectar_tipo_mixto_sqlite_dbi(
    conexion, tabla_sql, campos, campos_sql,
    afinidad = afinidad,
    consultar = function(sql) {
      respuesta <- .consultar_dbi(
        conexion, sql, presupuesto, etapa = "tipos_mezclados"
      )
      if (isTRUE(respuesta$ok)) respuesta$datos else NULL
    }
  )
}

# Lo que se declara de cada columna mezclada: que se midio en el motor, como
# se lee, de donde salen sus cifras y como quedan los valores que se escriben
# igual. Una fila por columna, en el bloque `bloque`.
.cobertura_tipo_mixto_dbi <- function(tipo_mixto, bloque = "perfil_muestra",
                                      sql = NA_character_,
                                      resumen_leido = FALSE) {
  if (!NROW(tipo_mixto)) return(.cobertura_dbi_vacia())
  filas <- lapply(seq_len(nrow(tipo_mixto)), function(k) {
    columna <- as.character(tipo_mixto$columna[[k]])
    clase <- as.character(tipo_mixto$clase[[k]])
    ancho <- isTRUE(tipo_mixto$ancho[[k]])
    nombre <- paste0("`", .comillas_sin_locale(columna), "`")
    afinidad <- if ("afinidad" %in% names(tipo_mixto)) {
      as.character(tipo_mixto$afinidad[[k]])
    } else "ninguna"
    # Con tipo declarado, SQLite convierte al guardar lo que puede, y el
    # controlador lee por ese tipo lo que quedo guardado como otro.
    tipada <- !is.na(afinidad) && !afinidad %in% c("ninguna", "desconocida")
    medida <- if (tipada) paste0(
      " (medido en el motor con `typeof()`), y su controlador la lee por ",
      "su tipo declarado: lo que no es de ese tipo llegaba convertido, un ",
      "texto como `0` o como faltante seg\u00fan la v\u00eda. "
    ) else paste0(
      " (medido en el motor con `typeof()`), y su controlador tipa lo que ",
      "lee por la primera fila que llega: la misma columna daba cifras ",
      "distintas seg\u00fan el orden de las filas. "
    )
    guarda <- function(que) {
      if (tipada) paste0(
        "SQLite guarda en ", nombre, ", de afinidad ", afinidad,
        " por su tipo declarado, ", que
      ) else NULL
    }
    motivo <- switch(
      clase,
      numeros = paste0(
        guarda(paste0(
          "enteros que no entran en 32 bits junto a reales, o el entero ",
          "-2^31, que R usa como faltante,"
        )) %||% paste0(
          "SQLite guarda enteros y reales mezclados en ", nombre,
          " (o el entero -2^31, que R usa como faltante)"
        ),
        medida,
        "Se lee como doble (`CAST(", nombre, " AS REAL)`), igual en cualquier ",
        "orden, y sus cifras son las de esos dobles.",
        if (ancho) paste0(
          " Guarda alg\u00fan entero de valor absoluto mayor que 2^53, que ",
          "un doble no representa con exactitud: como en un ",
          .FRAGMENTO_ENTERO_ANCHO_DBI, ", m\u00ednimo, m\u00e1ximo, media, ",
          "n_ceros, n_negativos, mediana y desv\u00edo no se publican como ",
          "calculados, y `n_distintos` y la moda se cuentan sobre los dobles, ",
          "que pueden unir dos enteros distintos."
        ) else " Cada entero cabe exacto en un doble: no se pierde ning\u00fan valor."
      ),
      texto = paste0(
        guarda("n\u00fameros y texto a la vez") %||%
          paste0("SQLite guarda n\u00fameros y texto mezclados en ", nombre),
        medida, "Se lee como texto, igual en cualquier orden: cada entero con sus ",
        "d\u00edgitos, cada real escrito por R (`as.character()`, con 17 cifras ",
        "si las 15 no alcanzan, y sin decimales si es entero: `1`, `1.0` y ",
        "`1e15` real valen lo mismo que el entero) y cada texto tal cual. Un ",
        "texto que se escribe igual que un n\u00famero (`'2'` y `2`, `'Inf'` y ",
        "el real `Inf`) cuenta como ese mismo valor, y uno que se escribe ",
        "distinto (`'2.0'` y `2`, `'1e3'` y `1000`) como otro. La columna se ",
        "perfila como texto, igual que en `perfilar()`: sus cifras de ",
        "magnitud, si se publican, salen de ese texto le\u00eddo como ",
        "n\u00famero, y un texto como `'NaN'` o `'NA'` cuenta como faltante ",
        "disfrazado."
      ),
      blob = paste0(
        guarda("BLOB junto con valores de otros tipos") %||%
          paste0("SQLite guarda BLOB mezclados con otros tipos en ", nombre),
        medida, "Se lee como BLOB (`CAST(", nombre, " AS BLOB)`), igual en cualquier ",
        "orden y sin cortar ning\u00fan byte: un n\u00famero pasa a los bytes ",
        "del texto con que SQLite lo escribe y un texto a sus bytes, as\u00ed ",
        "que un texto y un BLOB con los mismos bytes cuentan como el mismo ",
        "valor. La columna se perfila como BLOB."
      )
    )
    if (isTRUE(resumen_leido)) {
      motivo <- paste(
        motivo, "El resumen de la tabla sale de esta misma lectura."
      )
    }
    .registro_cobertura_dbi(
      bloque, columna,
      if (identical(clase, "numeros") && !ancho) "advertido" else "degradado",
      motivo,
      paste(
        "Para perfilar cada valor con su tipo, separar los tipos en el origen",
        "o leer la columna desde una vista que la convierta."
      ),
      sql
    )
  })
  do.call(rbind, filas)
}

# Las expresiones con que se miden en el motor los extremos (`MIN`/`MAX`) de
# las columnas mezcladas, por nombre: la columna de solo numeros, sobre su
# lectura como doble; la que guarda texto o BLOB no tiene extremo numerico
# -SQLite ordena todo texto despues de todo numero y todo BLOB despues de todo
# texto, asi que su `MAX` es un texto o un BLOB aunque la columna guarde
# numeros- y queda en `NA`, como la que guarda un entero ancho, que un doble no
# representa. Quien mide un rango no lo mide donde aca hay `NA`.
.expresion_rango_tipo_mixto_dbi <- function(tipo_mixto) {
  if (!NROW(tipo_mixto)) return(stats::setNames(character(), character()))
  numeros <- as.character(tipo_mixto$clase) == "numeros" &
    !(tipo_mixto$ancho %in% TRUE)
  stats::setNames(
    ifelse(numeros, as.character(tipo_mixto$lectura), NA_character_),
    as.character(tipo_mixto$columna)
  )
}

.MOTIVO_RANGO_TIPO_MIXTO_DBI <- paste(
  "MIN/MAX: SQLite guarda en la columna valores de tipos distintos (medido en",
  "el motor con `typeof()`) y su extremo no es el de un rango num\u00e9rico:",
  "todo texto ordena despu\u00e9s de todo n\u00famero."
)

# Las columnas mezcladas que una comparacion de `relaciones_coleccion()` no
# puede usar: las de solo numeros con un entero ancho, que leidas como doble
# pueden unir dos enteros distintos. Las de texto y las de BLOB se comparan
# sobre su lectura unica, que no convierte ningun texto en numero.
.podar_tipo_mixto_dbi <- function(tipo_mixto) {
  if (!NROW(tipo_mixto)) return(stats::setNames(character(), character()))
  ancho <- tipo_mixto$ancho %in% TRUE
  stats::setNames(
    as.character(tipo_mixto$clase[ancho]), as.character(tipo_mixto$columna[ancho])
  )
}

# El motivo con que una columna mezclada queda fuera de una comparacion de
# `relaciones_coleccion()` (ver `.podar_tipo_mixto_dbi()`).
.motivo_poda_tipo_mixto_dbi <- function() {
  paste(
    "SQLite guarda enteros y reales mezclados en la columna, con alg\u00fan",
    "entero de valor absoluto mayor que 2^53 (medido en el motor con",
    "`typeof()`): se lee como doble, que no lo representa con exactitud, y dos",
    "enteros distintos pueden quedar como uno, as\u00ed que una relaci\u00f3n",
    "sobre esos valores no describir\u00eda lo guardado."
  )
}

# La fila de `cobertura_pares` de un par de tablas que compara columnas
# mezcladas (`columnas_1`, `columnas_2`: las de cada tabla). La comparacion es
# por valor sobre la lectura unica -un real `2.0` y el texto `'2'` cuentan como
# el mismo valor, como en `detectar_relaciones()` y entre columnas con tipo
# declarado-, mientras que el motor, sin una afinidad que los convierta, no
# iguala un REAL con un TEXT. Un BLOB solo aparea con un BLOB de los mismos
# bytes.
.fila_par_tipo_mixto_dbi <- function(tabla_1, tabla_2, columnas_1,
                                     columnas_2) {
  nombrar <- function(columnas, tabla) {
    if (!length(columnas)) return(character())
    paste0(
      paste0("`", .comillas_sin_locale(columnas), "`", collapse = ", "),
      " (de `", .comillas_sin_locale(tabla), "`)"
    )
  }
  data.frame(
    tabla_1 = tabla_1, tabla_2 = tabla_2,
    motivo = paste0(
      "Se compar\u00f3 por valor con la lectura \u00fanica de las columnas ",
      "que guardan valores de tipos distintos en SQLite (medido en el motor con ",
      "`typeof()`): ", paste(c(nombrar(columnas_1, tabla_1),
                               nombrar(columnas_2, tabla_2)), collapse = " y "),
      ". Un n\u00famero y un texto que se escriben igual (`2.0` y `'2'`) ",
      "cuentan como el mismo valor, aunque en el motor, sin una afinidad que ",
      "los convierta, un REAL y un TEXT no son iguales; un BLOB solo aparea con un ",
      "BLOB de los mismos bytes."
    ),
    como_resolverlo = paste(
      "Para comparar con la regla del motor, declarar el tipo de las columnas",
      "o comparar en SQL (`IN (SELECT ...)`)."
    ),
    stringsAsFactors = FALSE
  )
}

# Las columnas de numeros que guardan un entero ancho: sus magnitudes no se
# publican como calculadas en lo que se calcula en R sobre lo leido
# -`perfil_muestra` y, con `resumen_leido`, el resumen de la muestra del motor y
# de la lectura por bloques-, como las de un entero ancho entregado como doble.
.tapar_entero_ancho_tipo_mixto_dbi <- function(resultado, tipo_mixto,
                                               resumen_leido = FALSE) {
  if (!NROW(tipo_mixto) || !is.list(resultado)) return(resultado)
  columnas <- as.character(tipo_mixto$columna[
    as.character(tipo_mixto$clase) == "numeros" & tipo_mixto$ancho %in% TRUE
  ])
  if (!length(columnas)) return(resultado)
  perfil <- resultado$perfil_muestra
  if (is.list(perfil) && is.data.frame(perfil$columnas)) {
    perfil$columnas <- .enmascarar_campos_muestra_dbi(perfil$columnas, columnas)
    resultado$perfil_muestra <- perfil
  }
  resumen <- resultado$resumen_tabla
  if (isTRUE(resumen_leido) && is.list(resumen)) {
    if (is.data.frame(resumen$columnas)) {
      resumen$columnas <- .enmascarar_campos_muestra_dbi(
        resumen$columnas, columnas
      )
    }
    sql <- resumen$sql
    if (is.data.frame(sql) && nrow(sql) &&
        all(c("columna", "metrica", "estado", "motivo") %in% names(sql))) {
      tocar <- .nombres_presentes(sql$columna, columnas) &
        as.character(sql$metrica) %in% .CAMPOS_MAGNITUD_MUESTRA_DBI &
        as.character(sql$estado) %in% .ESTADOS_QUE_AFIRMAN_VALOR_DBI
      sql$estado[tocar] <- "no_disponible"
      sql$motivo[tocar] <- paste(
        "La columna mezcla enteros y reales y se lee como doble, y guarda un",
        .FRAGMENTO_ENTERO_ANCHO_DBI, "de valor absoluto mayor que 2^53 que",
        "un doble no representa: no se publica como calculada."
      )
      resumen$sql <- sql
    }
    resultado$resumen_tabla <- resumen
  }
  resultado
}
