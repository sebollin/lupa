# Columnas BLOB: una sola lectura en todo el paquete.
#
# Un BLOB son bytes. Llega de dos maneras -la clase `blob` que entrega RSQLite,
# o una lista de `raw` sin clase, la de DuckDB o la armada a mano- y las dos son
# la misma columna. Tres reglas, cada una escrita una sola vez:
#
#   - que columna es BLOB lo dice `.es_columna_blob()`, y nada mas;
#   - en una lista BLOB el `NULL` es faltante (`.normalizar_blob_ausentes()`);
#   - toda comparacion de valores entre dos columnas, o entre una columna y un
#     dominio, pasa por `.comparar_valores()`: un BLOB solo es igual a un BLOB
#     de los mismos bytes, y nunca a un texto o a un numero, aunque el texto
#     diga lo mismo que la escritura interna de los bytes.
#
# Antes cada sitio resolvia el BLOB a su manera: `as.character()` escribe
# `<raw>` para cualquier valor de la clase `blob`, y la escritura `b:<hex>` con
# que se comparaban los bytes era un texto mas, que un texto del usuario podia
# igualar. Medido: un referencial de texto `b:01` declaraba conforme al BLOB
# `01`, y `detectar_claves()` declaraba redundantes un BLOB y un texto.

# El faltante de una lista BLOB. El controlador lo entrega como `NULL`, que
# `is.na()` no ve: la columna perdia sus faltantes en el perfil, la medicion y la
# clave. La entrada lo reescribe con este `NA` marcado, que `is.na()` ve y que no
# se confunde con un `NA` que el usuario haya puesto en una lista: asi una
# columna BLOB toda faltante -recien creada, o la muestra que solo toco filas
# nulas- sigue siendo BLOB, tambien despues de elegir filas.
.BLOB_AUSENTE <- structure(NA, lupa_blob_ausente = TRUE)

# Un BLOB: la clase `blob`, una `vctrs_list_of` de `raw`, o una lista -sin
# clase, con la de `I()` o con la clase `list` escrita- cuyos elementos son todos
# `raw` o faltantes (`NULL`, o el faltante de arriba). Una lista toda `NULL` es
# BLOB: es lo que DuckDB entrega para una columna binaria sin ningun valor. Una
# lista vacia lo es solo si la lectura del motor la declaro binaria
# (`.marcar_blob_vacio_dbi()`): sin filas, el contenido no decide nada. Otra
# clase -una `vctrs_list_of` de otro tipo, un registro, una geometria- no es BLOB.
.es_columna_blob <- function(x) {
  if (inherits(x, "blob")) return(TRUE)
  if (inherits(x, "vctrs_list_of")) {
    return(is.raw(attr(x, "ptype", exact = TRUE)))
  }
  if (!is.list(x)) return(FALSE)
  clase <- oldClass(x)
  if (!is.null(clase) && !identical(clase, "AsIs") &&
      !identical(clase, "list")) {
    return(FALSE)
  }
  if (!length(x)) return(isTRUE(attr(x, "lupa_blob", exact = TRUE)))
  # El primer elemento decide casi siempre: una lista de textos o de numeros
  # sale aca sin recorrer la columna.
  primero <- x[[1L]]
  if (!is.raw(primero) && !.es_faltante_blob(primero)) return(FALSE)
  crudos <- vapply(x, is.raw, logical(1L), USE.NAMES = FALSE)
  if (all(crudos)) return(TRUE)
  all(vapply(x[!crudos], .es_faltante_blob, logical(1L), USE.NAMES = FALSE))
}

.es_faltante_blob <- function(v) is.null(v) || identical(v, .BLOB_AUSENTE)

# El `NULL` de las listas BLOB de una tabla, escrito como faltante. Solo copia
# la columna que tiene un `NULL`; la clase `blob` y la `vctrs_list_of` ya lo
# leen como faltante -y no admiten otro valor que bytes-.
.normalizar_blob_ausentes <- function(tabla) {
  if (!is.data.frame(tabla) || !length(tabla)) return(tabla)
  for (i in seq_along(tabla)) {
    x <- .subset2(tabla, i)
    if (!is.list(x) || inherits(x, c("blob", "vctrs_list_of")) ||
        !.es_columna_blob(x)) next
    nulos <- vapply(x, is.null, logical(1L), USE.NAMES = FALSE)
    if (!any(nulos)) next
    x[nulos] <- list(.BLOB_AUSENTE)
    tabla[[i]] <- x
  }
  tabla
}

# Las columnas de una muestra del motor que llegaron vacias como `list()` y que
# el motor declara binarias: DuckDB entrega asi un `BLOB` sin filas, y el perfil
# las publicaba `lista` donde SQLite, con la misma tabla, dice `blob`. Como la
# columna toda `NULL`, es BLOB porque el controlador lo declara.
.marcar_blob_vacio_dbi <- function(datos, tipos_declarados = NULL,
                                   tipos_motor = NULL) {
  if (!is.data.frame(datos) || nrow(datos) || !length(datos)) return(datos)
  binario <- function(tipo) {
    if (is.null(tipo) || !length(tipo) || is.na(tipo[[1L]])) return(FALSE)
    base <- .tipo_base_muestra_dbi(tipo[[1L]])
    !is.na(base) && base %in% c(
      "raw", "blob", "bytea", "binary", "varbinary", "image", "tinyblob",
      "mediumblob", "longblob"
    )
  }
  for (i in seq_along(datos)) {
    x <- .subset2(datos, i)
    if (!is.list(x) || length(x) || !is.null(oldClass(x))) next
    columna <- names(datos)[[i]]
    if (binario(.atributo_columna_dbi(tipos_motor, columna, i)) ||
        binario(.atributo_columna_dbi(tipos_declarados, columna, i))) {
      attr(x, "lupa_blob") <- TRUE
      datos[[i]] <- x
    }
  }
  datos
}

# Los bytes de cada valor BLOB, como clave interna: `b:` y su hexadecimal
# (`.clave_almacenamiento_dbi()`, la escritura de las columnas mezcladas de
# SQLite), `NA` para el faltante. Las dos representaciones dan la misma clave.
# Es una clave para comparar BLOB entre si: nunca se compara con un texto, eso
# lo garantiza `.comparar_valores()`.
.claves_blob <- function(x) {
  .clave_almacenamiento_dbi(lapply(unclass(x), function(v) {
    if (is.raw(v)) v else NULL
  }))
}

# Las columnas BLOB de una tabla escritas con sus claves de bytes, para buscar
# claves DENTRO de cada columna: un identificador `BINARY(16)` -la clave binaria
# comun- es candidato, y su unicidad es la de sus bytes. Esta tabla no sirve para
# comparar una columna con otra: ahi un texto `b:01` seria igual al BLOB `01`.
# Quien compara columnas lo hace sobre la tabla original y con
# `.comparar_valores()`.
.blob_como_clave <- function(tabla) {
  if (!is.data.frame(tabla) || !length(tabla)) return(tabla)
  for (i in seq_along(tabla)) {
    if (.es_columna_blob(.subset2(tabla, i))) {
      tabla[[i]] <- .claves_blob(.subset2(tabla, i))
    }
  }
  tabla
}

# Si dos columnas -o una columna y un dominio- se pueden igualar: las dos BLOB o
# las dos no BLOB.
.mismo_lado_blob <- function(x, y) {
  .es_columna_blob(x) == .es_columna_blob(y)
}

# La comparacion de valores entre dos columnas, o entre una columna `x` y un
# dominio `y`. Todo sitio que compara valores de dos lados la llama:
#
#   - un BLOB y algo que no lo es: ninguno coincide, sin escribir claves -el
#     motor tampoco iguala un BLOB con un texto-;
#   - dos BLOB: por sus bytes, con la clave de `.claves_blob()`;
#   - dos que no son BLOB: `sin_blob(x, y)`, la comparacion propia de cada
#     sitio, que queda como estaba.
#
# `como` dice que se pregunta: `pertenencia` (que valores de `x` estan en `y`,
# como `%in%`), `posicion` (donde, como `match()`), `igualdad` (fila a fila,
# como `==`, con `NA` donde falta un lado), `identicas` (si las dos columnas son
# la misma, como `identical()`) o `claves` (las dos claves, `list(x, y)`, para
# quien arma con ellas una comparacion de varias columnas; `NULL` si no hay par
# posible, y quien la pide declara que ninguna fila coincide).
.comparar_valores <- function(x, y, sin_blob,
                              como = c("pertenencia", "posicion", "igualdad",
                                       "identicas", "claves")) {
  como <- como[[1L]]
  blob_x <- .es_columna_blob(x)
  if (blob_x != .es_columna_blob(y)) {
    return(switch(
      como,
      pertenencia = rep(FALSE, length(x)),
      posicion = rep(NA_integer_, length(x)),
      igualdad = rep(FALSE, max(length(x), length(y))),
      identicas = FALSE,
      claves = NULL
    ))
  }
  if (!blob_x) return(sin_blob(x, y))
  clave_x <- .claves_blob(x)
  clave_y <- .claves_blob(y)
  switch(
    como,
    pertenencia = clave_x %in% clave_y,
    posicion = match(clave_x, clave_y),
    igualdad = clave_x == clave_y,
    identicas = identical(clave_x, clave_y),
    claves = list(x = clave_x, y = clave_y)
  )
}

# Lo que lee texto o magnitudes -una expresion regular, una distancia entre
# escrituras, un rango- no se aplica a bytes: sobre un BLOB se evaluaba
# `as.character()` (`<raw>` para toda la clase `blob`, el `deparse` de una
# lista) y `^<raw>$` declaraba conforme o vigente cualquier BLOB. La metrica no
# se mide y lo dice; `remedio` dice que declarar en su lugar.
.rechazar_sobre_blob <- function(x, atributo, que, remedio) {
  if (!.es_columna_blob(x)) return(invisible(NULL))
  .detener(
    que, " no se aplica a bytes: la columna `", atributo, "` es BLOB. ",
    remedio, call. = FALSE
  )
}

# La moda de un BLOB no se publica: son bytes, y escritos como texto se cortaban
# en el primer `00` -se publicaba un valor que no es el modal-. El resumen SQL
# publica la frecuencia que cuenta el motor; el perfil en R no la cuenta, como en
# toda columna de listas. Los dos lo declaran en la cobertura.
.MOTIVO_MODA_BLOB <- paste(
  "La moda de un BLOB no se publica: son bytes y no se escriben como texto;",
  "`frecuencia_moda` es la que cuenta el motor."
)
.MOTIVO_MODA_BLOB_MEMORIA <- paste(
  "La moda de un BLOB no se publica: son bytes y no se escriben como texto;",
  "en R tampoco se cuenta su frecuencia (`frecuencia_moda` es `NA`)."
)
.REMEDIO_MODA_BLOB <- paste(
  "Para ver el valor, agruparlo en el motor (`GROUP BY`) o comparar sus bytes",
  "en R con `identical()`."
)

# Una fila de `cobertura_diagnosticos` por cada columna BLOB con valores: su
# moda no se publica.
.cobertura_moda_blob <- function(datos, nombres) {
  blob <- which(vapply(datos, function(x) {
    .es_columna_blob(x) && !all(is.na(x))
  }, logical(1L), USE.NAMES = FALSE))
  if (!length(blob)) return(NULL)
  do.call(rbind, lapply(blob, function(i) {
    .nuevo_diagnostico_no_evaluado(
      "moda", nombres[[i]], .MOTIVO_MODA_BLOB_MEMORIA, .REMEDIO_MODA_BLOB,
      columnas = nombres[[i]]
    )
  }))
}
