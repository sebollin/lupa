# Triaje de hallazgos: decisiones que sobreviven a la corrida siguiente.
#
# Un analisis real sobre una base grande publica decenas de miles de hallazgos.
# Sin una forma de decir "este lo revise y lo acepto / lo descarto, por tal
# motivo" y que eso valga en la corrida que viene, el resultado no se puede
# usar: cada entrega vuelve a mostrar lo ya revisado.
#
# Tres piezas, y cada una vive en un solo lugar:
#
# * la IDENTIDAD de un hallazgo (`.ids_hallazgos()`), armada con lo que lo
#   identifica -tabla, grupo, columna, tipo- y no con su posicion en la tabla ni
#   con un numero de llamada: un id secuencial choca al unir dos llamadas y
#   cambia cuando se reordena;
# * el ARCHIVO de decisiones, un CSV en UTF-8 que se edita y se versiona, con
#   lo que el revisor vio cuando decidio (severidad, filas afectadas,
#   evidencia);
# * el ESTADO (`.estados_hallazgos()`), que cruza las decisiones con los
#   hallazgos de esta corrida. Una decision se aplica sola si lo que se vio
#   sigue igual; si cambio, el hallazgo vuelve a `revisar`: nunca se aplica en
#   silencio sobre algo distinto de lo que se decidio.

.DECISIONES_HALLAZGO <- c("aceptado", "falso_positivo", "propiedad_legitima")

# El estado que se publica por cada decision.
.ESTADO_POR_DECISION <- c(
  aceptado = "aceptado",
  falso_positivo = "descartado_falso_positivo",
  propiedad_legitima = "descartado_propiedad_legitima"
)

.ESTADOS_DECISION <- c(
  "pendiente", "revisar", "decidido_sin_verificar", unname(.ESTADO_POR_DECISION)
)

# Los nombres con que el paquete bautiza una tabla cuando no recibe uno.
.NOMBRES_TABLA_GENERICOS <- c("datos")

.COLUMNAS_DECISIONES <- c(
  "id_hallazgo", "tabla", "grupo", "columna", "tipo_hallazgo", "decision",
  "motivo", "quien", "fecha", "severidad", "n_afectados", "evidencia",
  "huella_valores", "huella_filas", "detalle", "huella_estructura"
)

.COLUMNAS_ESTADO_DECISION <- c(
  "id_hallazgo", "estado_decision", "decision", "motivo_decision",
  "decidido_por", "fecha_decision", "cambio_desde_decision", "verificacion"
)

# ---------------------------------------------------------------------------
# Identidad
# ---------------------------------------------------------------------------

# La barra vertical separa las partes del id, asi que va escapada dentro de
# ellas (y el porciento, que es el escape); un nombre de columna con `|` no
# puede confundirse con dos partes.
.escapar_parte_id_hallazgo <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("%", "%25", x, fixed = TRUE, useBytes = TRUE)
  gsub("|", "%7C", x, fixed = TRUE, useBytes = TRUE)
}

.texto_o_vacio <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# La tabla de hallazgos de un objeto y el nombre de la tabla a la que pertenecen.
# Es la misma fuente que usa `hallazgos()`, sin el agregado de las decisiones.
#
# La tabla es parte de la identidad, y tiene que NOMBRARSE: una cadena vacia o un
# `NA` hacia que dos tablas con los mismos grupos, columnas y tipos compartieran
# ids y que la decision de una se aplicara en la otra sin avisar. Por eso, si no
# hay con que nombrarla (el resultado de `perfilar_por()` no guarda el nombre de
# la tabla), se pide en vez de inventar uno.
.base_hallazgos_decision <- function(x, nombre_tabla = NULL) {
  if (!is.null(nombre_tabla) &&
      (!is.character(nombre_tabla) || length(nombre_tabla) != 1L ||
       is.na(nombre_tabla) || !nzchar(trimws(nombre_tabla)))) {
    .detener("`nombre_tabla` debe ser un texto no vac\u00edo.", call. = FALSE)
  }
  if (inherits(x, "hallazgos_por_grupo")) {
    nombre <- if (is.null(nombre_tabla)) {
      attr(x, "nombre_tabla_decisiones", exact = TRUE)
    } else nombre_tabla
    if (is.null(nombre)) {
      .detener(
        "El resultado de perfilar_por() no guarda el nombre de la tabla que se ",
        "perfil\u00f3, y sin \u00e9l los hallazgos de dos tablas con los mismos ",
        "grupos y columnas tendr\u00edan la misma identidad. Indicarlo con ",
        "`nombre_tabla`.", call. = FALSE
      )
    }
    return(list(h = x, tabla = rep(nombre, nrow(x)), nombre = nombre,
                nombres_columnas = character(), datos = NULL, columnas = NULL))
  }
  if (!inherits(x, c("perfil", "analisis", "perfil_dbi", "perfil_coleccion"))) {
    .detener(
      "`x` debe ser un objeto de perfilar(), analizar(), perfilar_dbi(), ",
      "perfilar_coleccion() o el resultado de perfilar_por().", call. = FALSE
    )
  }
  h <- .fuente_hallazgos(x)
  if (is.null(h)) h <- .hallazgos_sin_filas()
  tabla <- if (inherits(x, "perfil")) {
    x$meta$nombre
  } else if (inherits(x, "analisis")) {
    x$perfil$meta$nombre
  } else if (inherits(x, "perfil_dbi")) {
    x$resumen_tabla$meta$tabla
  } else {
    # El identificador de la coleccion ya lleva el catalogo y el esquema.
    if ("identificador" %in% names(h)) h$identificador else h$tabla
  }
  if (!is.null(nombre_tabla)) tabla <- nombre_tabla
  tabla <- .texto_o_vacio(tabla)
  # Un nombre que no identifica: el que el paquete pone cuando no hay uno
  # (`datos`) lo comparten todas las tablas que se perfilan dentro de una
  # funcion, y la descripcion de la clase S4 es lo que publicaba una version
  # anterior para un `DBI::Id`. Dos tablas a las que el usuario da el MISMO
  # nombre son, por declaracion, la misma tabla.
  if (is.null(nombre_tabla) && nrow(h) &&
      any((inherits(x, c("perfil", "analisis")) &
             tabla %in% .NOMBRES_TABLA_GENERICOS) |
            startsWith(tabla, "<S4 class"))) {
    .detener(
      "El nombre de esta tabla (", paste0("\"", tabla[[1L]], "\""),
      ") no la identifica: es el que se usa cuando no hay uno, o lo ",
      "dej\u00f3 un perfil guardado por una versi\u00f3n anterior. Indicar ",
      "un nombre propio con `nombre_tabla`; dos tablas con el mismo nombre se ",
      "tratan como la misma.", call. = FALSE
    )
  }
  if (nrow(h) && (!length(tabla) || any(!nzchar(trimws(tabla))))) {
    .detener(
      "No hay con qu\u00e9 nombrar la tabla de estos hallazgos. Indicarlo con ",
      "`nombre_tabla`.", call. = FALSE
    )
  }
  nombres_columnas <- if (inherits(x, "perfil")) {
    as.character(x$columnas$columna)
  } else if (inherits(x, "analisis")) {
    as.character(x$perfil$columnas$columna)
  } else character()
  datos <- if (inherits(x, "analisis")) x[["datos"]] else NULL
  # Con la proteccion de datos personales activa, `x$datos` esta enmascarado, y
  # una huella calculada sobre valores enmascarados no dice nada. La marca es la
  # bandera que el perfil guarda al perfilar.
  enmascarados <- inherits(x, "analisis") && is.data.frame(datos) &&
    isTRUE(x$perfil$meta$proteger_datos_personales)
  if (enmascarados) datos <- NULL
  columnas <- if (inherits(x, "perfil")) {
    x$columnas
  } else if (inherits(x, "analisis")) {
    x$perfil$columnas
  } else if (inherits(x, "perfil_dbi")) {
    x$perfil_muestra$columnas
  } else if (inherits(x, "perfil_coleccion")) {
    # Las columnas de cada tabla, con la clave con que `hallazgos()` las nombra.
    lapply(x$perfiles, function(p) p$perfil_muestra$columnas)
  } else NULL
  list(h = h, tabla = rep_len(tabla, nrow(h)), nombres_columnas = nombres_columnas,
       datos = if (is.data.frame(datos)) datos else NULL,
       datos_enmascarados = enmascarados,
       columnas = if (is.data.frame(columnas) || is.list(columnas)) columnas else NULL)
}

# Las filas que localiza `perfilar_por()` son las posiciones en la TABLA, no en
# el grupo, asi que con la tabla (`datos`) la huella de valores se calcula como en
# `perfilar()`. La estructura sale de los nombres y las clases de las columnas
# (sin la de agrupacion, que no se perfila): `perfilar_por()` no guarda una tabla
# de columnas.
.preparar_por_grupo <- function(base, x, datos) {
  base$datos <- NULL
  datos <- .datos_para_huella(datos, NULL)
  if (!is.data.frame(datos)) return(base)
  por <- attr(x, "columna_grupo", exact = TRUE)
  base$datos <- datos
  rebanada <- datos[setdiff(names(datos), if (is.character(por)) por else character())]
  base$columnas <- data.frame(
    columna = names(rebanada),
    tipo_declarado = vapply(rebanada, function(v) class(v)[[1L]], character(1L)),
    tipo_inferido = NA_character_, stringsAsFactors = FALSE
  )
  base
}

# Lo que el hallazgo cuenta y no se ve en sus filas: en `faltantes` y
# `faltantes_disfrazados`, cuantas son ausencias declaradas y cuantas
# centinelas, y CUALES son los centinelas numericos. Un valor que pasa de `NA` a
# `999`, o de `999` a `-9`, es la misma cifra sobre las mismas filas y otro
# contenido. Sale de lo que el perfil publica por columna (la tabla de columnas
# y la traza de `faltantes_disfrazados`, con sus centinelas ordenados), no del
# texto de la evidencia. **Limite:** los centinelas de texto (`"N/A"`,
# `"NULL"`) el perfil los publica solo como cuenta y en la evidencia impresa:
# cambiar uno por otro con la misma cuenta se ve con `datos` (huella de valores)
# y no sin ellos.
.columnas_de_fila <- function(base, i) {
  columnas <- base$columnas
  if (is.data.frame(columnas)) return(columnas)
  if (is.list(columnas)) return(columnas[[base$tabla[[i]]]])
  NULL
}

.TIPOS_CON_CONTENIDO_DE_COLUMNA <- c(
  "constante", "casi_constante", "casi_clave", "alta_cardinalidad",
  "posible_identificador"
)

.contenido_de_columna <- function(columnas, j) {
  campo <- function(nombre) {
    if (nombre %in% names(columnas)) columnas[[nombre]][[j]] else NA
  }
  moda <- campo("moda")
  paste0(
    "valor=", if (is.na(moda)) "" else .clave_bytes(as.character(moda)),
    ";frecuencia=", sprintf("%.0f", as.numeric(campo("frecuencia_moda"))),
    ";distintos=", sprintf("%.0f", as.numeric(campo("n_distintos"))),
    ";n=", sprintf("%.0f", as.numeric(campo("n")))
  )
}

.detalle_hallazgos <- function(h, base, filas = seq_len(nrow(h))) {
  n <- nrow(h)
  salida <- rep(NA_character_, n)
  if (!n) return(salida)
  tipos <- as.character(h$tipo_hallazgo)
  disfrazados <- which(tipos == "faltantes_disfrazados" & !is.na(h$columna))
  clave_dis <- paste(.clave_bytes(base$tabla[disfrazados]),
                     .clave_bytes(h$columna[disfrazados]), sep = "\037")
  # Lo que el perfil publica por columna y define a estos hallazgos: el valor
  # constante, la moda y su frecuencia, los distintos. Cambian con la misma
  # cuenta de filas afectadas.
  for (i in intersect(filas, which(tipos %in% .TIPOS_CON_CONTENIDO_DE_COLUMNA &
                                     !is.na(h$columna)))) {
    columnas <- .columnas_de_fila(base, i)
    if (is.data.frame(columnas) && "columna" %in% names(columnas)) {
      c1 <- .clave_bytes(columnas$columna)
      j <- match(.clave_bytes(h$columna[[i]]), c1)
      if (!is.na(j) && sum(c1 == c1[[j]]) == 1L) {
        salida[[i]] <- .contenido_de_columna(columnas, j)
      }
    }
  }
  for (i in intersect(filas, which(tipos %in% c("faltantes",
                                                "faltantes_disfrazados") &
                                     !is.na(h$columna)))) {
    clave <- paste(.clave_bytes(base$tabla[[i]]), .clave_bytes(h$columna[[i]]),
                   sep = "\037")
    centinelas <- numeric()
    for (j in disfrazados[clave_dis == clave]) {
      traza <- h$trazabilidad[[j]]
      if (is.list(traza) && is.numeric(traza$sentinelas_contados)) {
        centinelas <- c(centinelas, as.numeric(traza$sentinelas_contados))
      }
    }
    centinelas <- unique(centinelas)
    centinelas <- centinelas[.orden_por_bytes(centinelas)]
    partes <- character()
    columnas <- .columnas_de_fila(base, i)
    if (is.data.frame(columnas) &&
        all(c("columna", "n_faltantes", "n_faltantes_disfrazados") %in%
              names(columnas))) {
      c1 <- .clave_bytes(columnas$columna)
      j <- match(.clave_bytes(h$columna[[i]]), c1)
      if (!is.na(j) && !(sum(c1 == c1[[j]]) > 1L)) {
        cuenta <- function(campo) {
          if (campo %in% names(columnas)) columnas[[campo]][[j]] else NA_real_
        }
        partes <- sprintf(
          "reales=%.0f;disfrazados=%.0f;numericos=%.0f;textuales=%.0f",
          columnas$n_faltantes[[j]], columnas$n_faltantes_disfrazados[[j]],
          cuenta("n_faltantes_disfrazados_numericos"),
          cuenta("n_faltantes_disfrazados_textuales")
        )
      }
    }
    salida[[i]] <- paste0(
      if (length(partes)) paste0(partes, ";") else "",
      "centinelas=", paste(sprintf("%.17g", centinelas), collapse = ",")
    )
  }
  salida
}

# Huella de la estructura, para los hallazgos que no tienen filas (un tipo
# declarado distinto del inferido, columnas duplicadas, nombres problematicos):
# los nombres, tipos declarados y tipos inferidos de las columnas involucradas
# -la columna del hallazgo o, si no tiene una, toda la tabla-. Sale de la tabla de
# columnas del perfil, sin datos, y no depende del orden de las columnas.
.estructura_hallazgos <- function(h, base, filas = seq_len(nrow(h))) {
  n <- nrow(h)
  salida <- rep(NA_character_, n)
  if (!n || !"trazabilidad" %in% names(h)) return(salida)
  for (i in filas) {
    traza <- h$trazabilidad[[i]]
    if (length(traza$indices_fila)) next
    columnas <- .columnas_de_fila(base, i)
    if (!is.data.frame(columnas) || !"columna" %in% names(columnas)) next
    # Las columnas involucradas: las que el hallazgo nombra en su traza (una
    # relacion entre varias), la suya, o toda la tabla si no tiene una o es de
    # `columnas_duplicadas`, cuya pareja no se publica como dato sino en la
    # evidencia: cambiar la pareja cambia la estructura de la tabla.
    nombradas <- traza$columnas
    involucradas <- if (length(nombradas)) {
      which(.clave_bytes(columnas$columna) %in% .clave_bytes(nombradas))
    } else if (!is.na(h$columna[[i]]) &&
               !identical(as.character(h$tipo_hallazgo[[i]]), "columnas_duplicadas")) {
      which(.clave_bytes(columnas$columna) == .clave_bytes(h$columna[[i]]))
    } else seq_len(nrow(columnas))
    if (!length(involucradas)) next
    texto <- function(campo) {
      if (campo %in% names(columnas)) {
        as.character(columnas[[campo]][involucradas])
      } else rep(NA_character_, length(involucradas))
    }
    salida[[i]] <- .huella_multiconjunto(.tuplas_huella(list(
      as.character(columnas$columna[involucradas]), texto("tipo_declarado"),
      texto("tipo_inferido")
    )))
  }
  salida
}

# Un hallazgo sobre una columna cuyo nombre esta repetido en la tabla.
.hallazgo_de_columna_repetida <- function(h, nombres_columnas) {
  if (!length(nombres_columnas) || !nrow(h)) return(rep(FALSE, nrow(h)))
  clave <- .clave_bytes(nombres_columnas)
  repetidas <- clave[duplicated(clave)]
  !is.na(h$columna) & .clave_bytes(h$columna) %in% repetidas
}

# Dos hallazgos del mismo tipo en la misma columna (un detector que informa
# varios casos a la vez) se distinguen por su lugar en el orden de sus bytes, no
# por el orden en que salieron: el texto de la evidencia, sin sus cifras, fija el
# desempate.
#
# Con columnas homonimas el desempate es la POSICION de la columna: los
# hallazgos salen en el orden de las columnas, asi que el k-esimo hallazgo de un
# tipo en un nombre repetido es el de la k-esima columna con ese nombre, y no
# cambia si se reordenan otras columnas. Si se reordenan las homonimas entre si,
# el id no lo ve: por eso esos hallazgos exigen una huella de contenido para
# aplicar una decision (ver `.estados_hallazgos()`).
.ids_hallazgos <- function(h, tabla, repetida = rep(FALSE, nrow(h))) {
  n <- nrow(h)
  if (!n) return(character())
  grupo <- if ("grupo" %in% names(h)) h$grupo else rep("", n)
  clave <- paste(
    .escapar_parte_id_hallazgo(tabla), .escapar_parte_id_hallazgo(grupo),
    .escapar_parte_id_hallazgo(h$columna), .escapar_parte_id_hallazgo(h$tipo_hallazgo),
    sep = "|"
  )
  hermano <- clave %in% clave[duplicated(clave)]
  if (!any(hermano)) return(clave)
  evidencia <- .texto_o_vacio(h$evidencia)
  forma <- gsub("[0-9.,]+", "#", evidencia, useBytes = TRUE)
  aparicion <- sprintf("%012d", seq_len(n))
  orden <- .orden_por_bytes(
    clave, ifelse(repetida, aparicion, forma), ifelse(repetida, "", evidencia)
  )
  puesto <- integer(n)
  puesto[orden] <- stats::ave(seq_len(n), clave[orden], FUN = seq_along)
  clave[hermano] <- paste0(clave[hermano], "|#", puesto[hermano])
  clave
}

# ---------------------------------------------------------------------------
# Huella del contenido afectado
# ---------------------------------------------------------------------------
#
# Una cifra no alcanza para saber que el hallazgo es el mismo: 30 ausentes en las
# primeras 30 filas y 30 ausentes en las ultimas 30 dan la misma cifra, y dos
# extremos de 1e6 y de 5e6 dicen igual "2 valores". Al decidir se guarda una
# huella de lo que el hallazgo toca, que no depende del orden de las filas:
#
# * `huella_valores`: los valores afectados (requiere los datos);
# * `huella_filas`: las claves de las filas afectadas (requiere clave declarada
#   o del catalogo; sin clave, las filas se nombran por su posicion y esa
#   posicion depende del orden, asi que no se hace: es un limite declarado, y
#   ahi queda la cuenta).
#
# Cada valor se codifica con su tipo y su largo en bytes, y cada tupla con su
# cantidad de componentes: sin separadores que el dato pueda contener, y sin
# fundir lo que `as.character()` funde. Distinguen `NA` de `"NA"`, un numero de
# su texto, una fecha de su texto, un `integer64` de su texto, un factor de su
# etiqueta y `0.3` de `0.1 + 0.2` (el doble se escribe con 17 cifras, que lo
# identifican exacto y no dependen de la plataforma ni de `OutDec`).
.ficha_huella <- function(tipo, carga) {
  paste0(tipo, nchar(carga, type = "bytes"), ":", carga)
}

.codificar_columna_huella <- function(x) {
  n <- length(x)
  salida <- character(n)
  nan <- if (is.double(x) && !inherits(x, c("integer64", "POSIXt", "Date"))) {
    is.nan(x)
  } else rep(FALSE, n)
  ausente <- is.na(x) & !nan
  if (is.factor(x)) {
    tipo <- "f"
    carga <- .clave_bytes(as.character(x))
  } else if (inherits(x, "integer64")) {
    tipo <- "I"
    carga <- as.character(x)
  } else if (inherits(x, "POSIXt")) {
    tipo <- "P"
    carga <- sprintf("%.17g", as.numeric(x) + 0)
  } else if (inherits(x, "Date")) {
    tipo <- "D"
    carga <- sprintf("%.17g", as.numeric(x) + 0)
  } else if (is.integer(x)) {
    tipo <- "i"
    carga <- as.character(x)
  } else if (is.logical(x)) {
    tipo <- "l"
    carga <- as.character(x)
  } else if (is.numeric(x)) {
    tipo <- "d"
    carga <- sprintf("%.17g", as.numeric(x) + 0)
  } else if (is.character(x)) {
    tipo <- "s"
    carga <- .clave_bytes(x)
  } else {
    tipo <- "o"
    carga <- .clave_bytes(as.character(x))
  }
  carga[ausente] <- ""
  salida <- .ficha_huella(tipo, carga)
  salida[ausente] <- "N0:"
  salida[nan] <- "n0:"
  salida
}

# Una tupla por fila: la cantidad de componentes y las fichas de cada una.
.tuplas_huella <- function(columnas) {
  fichas <- lapply(columnas, .codificar_columna_huella)
  do.call(paste0, c(list(paste0("t", length(columnas), "|")), unname(fichas)))
}

# Hash de multiconjunto -la suma de los hashes de cada elemento-: no depende del
# orden y su costo es lineal en los elementos, no en la tabla.
.huella_multiconjunto <- function(texto) {
  if (!length(texto)) return(NA_character_)
  hash <- .hash_texto_huella(texto)
  sprintf(
    "h2.%d.%.0f.%.0f", length(texto), sum(hash$h1) %% 4294967296,
    sum(hash$h2) %% 4294967296
  )
}

# Devuelve la huella de valores, la de filas y, si falta alguna por una razon
# que se puede decir, cual es.
.huella_de_hallazgo <- function(traza, columna, tipo, datos, posicion_columna) {
  salida <- c(valores = NA_character_, filas = NA_character_,
              motivo = NA_character_)
  if (!is.list(traza) || identical(traza$estado, "no_disponible") ||
      is.null(traza$estado)) {
    salida[["motivo"]] <- "el hallazgo no localiza filas"
    return(salida)
  }
  if (!identical(traza$estado, "disponible")) {
    salida[["motivo"]] <- "la traza esta truncada (mas filas afectadas que el limite)"
    return(salida)
  }
  indices <- as.integer(traza$indices_fila)
  indices <- indices[!is.na(indices)]
  if (!length(indices)) {
    salida[["motivo"]] <- "el hallazgo no localiza filas"
    return(salida)
  }
  claves <- traza$claves
  if (is.data.frame(claves) && nrow(claves) == length(indices) && ncol(claves)) {
    salida[["filas"]] <- .huella_multiconjunto(.tuplas_huella(as.list(claves)))
  }
  if (is.data.frame(datos) && max(indices) <= nrow(datos)) {
    if (!is.na(posicion_columna) && posicion_columna <= ncol(datos)) {
      valores <- datos[[posicion_columna]][indices]
      # Los ausentes no distinguen a una fila de otra: la huella de valores de
      # `faltantes` seria siempre la misma y pareceria una verificacion.
      if (!all(is.na(valores)) &&
          (is.atomic(valores) || is.factor(valores))) {
        salida[["valores"]] <- .huella_multiconjunto(.tuplas_huella(list(valores)))
      }
    } else if (is.na(columna) && identical(as.character(tipo), "filas_duplicadas")) {
      # Un hallazgo de la fila entera: se huellan las filas completas, que se
      # pueden recomputar desde los datos.
      salida[["valores"]] <- .huella_multiconjunto(.tuplas_huella(
        lapply(seq_len(ncol(datos)), function(j) datos[[j]][indices])
      ))
    }
  }
  if (is.na(salida[["valores"]]) && is.na(salida[["filas"]])) {
    salida[["motivo"]] <- if (is.null(datos) && is.na(salida[["filas"]])) {
      "no hay datos ni clave con que verificar el contenido"
    } else "no hay clave, y los valores afectados no distinguen las filas"
  }
  salida
}

# Las huellas de las filas `filas` de `h` (NA donde no se pueden calcular).
.huellas_hallazgos <- function(h, base, filas = seq_len(nrow(h)),
                               repetida = rep(FALSE, nrow(h))) {
  n <- nrow(h)
  salida <- data.frame(
    huella_valores = rep(NA_character_, n),
    huella_filas = rep(NA_character_, n),
    huella_motivo = rep(NA_character_, n), stringsAsFactors = FALSE
  )
  if (!n || !"trazabilidad" %in% names(h)) return(salida)
  datos <- base$datos
  nombres <- if (is.data.frame(datos)) names(datos) else character()
  clave_nombres <- .clave_bytes(nombres)
  # El orden de aparicion dentro de cada columna repetida: la posicion fisica.
  rango <- stats::ave(seq_len(n), .clave_bytes(h$columna), h$tipo_hallazgo,
                      FUN = seq_along)
  for (i in filas) {
    posicion <- NA_integer_
    if (!is.na(h$columna[[i]]) && length(nombres)) {
      candidatas <- which(clave_nombres == .clave_bytes(h$columna[[i]]))
      posicion <- if (length(candidatas) == 1L) candidatas else if (
        isTRUE(repetida[[i]]) && rango[[i]] <= length(candidatas)) {
        candidatas[[rango[[i]]]]
      } else NA_integer_
    }
    hu <- .huella_de_hallazgo(
      h$trazabilidad[[i]], h$columna[[i]], h$tipo_hallazgo[[i]],
      datos, posicion
    )
    salida$huella_valores[[i]] <- hu[["valores"]]
    salida$huella_filas[[i]] <- hu[["filas"]]
    salida$huella_motivo[[i]] <- hu[["motivo"]]
  }
  salida
}

# ---------------------------------------------------------------------------
# Estado
# ---------------------------------------------------------------------------

.mismo_numero <- function(a, b) {
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

.mismo_texto <- function(a, b) {
  .clave_bytes(.texto_o_vacio(a)) == .clave_bytes(.texto_o_vacio(b))
}

# Dos huellas difieren solo si las dos existen: una que falta no prueba nada.
.huella_difiere <- function(actual, decidida) {
  !is.na(actual) & !is.na(decidida) & actual != decidida
}

# Los componentes que verifican un hallazgo, por tipo: los que pueden cambiar sin
# que cambie la cuenta. Una tabla cerrada, escrita una vez; un tipo que el
# paquete publica y no esta aca lo avisa una prueba. Cada elemento es una
# conjuncion (`&`) de alternativas (`|`):
#
# * hallazgos de valores: los valores afectados, o la estructura si el hallazgo
#   no localiza filas;
# * `faltantes`: las filas y el detalle (ausencias declaradas y centinelas);
# * `faltantes_disfrazados`: los valores;
# * hallazgos de filas: las filas, o los valores de las filas completas;
# * hallazgos estructurales: la estructura de las columnas involucradas.
.tipos_de_valores <- c(
  "outliers", "valor_fuera_dominio", "fecha_futura", "fecha_centinela",
  "patron_raro", "posible_centinela_numerico", "casi_duplicados_vocabulario",
  "variantes_equifrecuentes_vocabulario", "formato_fecha_ambiguo",
  "formatos_fecha_mixtos", "anio_de_dos_digitos", "celdas_multivaluadas",
  "ceros_no_permitidos", "negativos_no_permitidos", "codificacion_invalida",
  "codificacion_rota", "controles_invisibles", "entidades_html",
  "espacios_sobrantes", "mayusculas_inconsistentes", "normalizacion_unicode",
  "numero_como_texto", "separadores_en_campo", "texto_truncado",
  "valores_no_finitos", "valores_subnormales",
  "digito_verificador_invalido", "largo_codigo_inconsistente", "escala_mixta",
  "monedas_mixtas", "unidades_mixtas", "valor_concentrado",
  "valor_fuera_de_aplicabilidad", "zona_horaria_fecha_hora",
  "coordenada_fuera_dominio", "geometria_invalida", "geometria_vacia",
  "tipos_geometria_mixtos", "serie_acumulada_retrocede"
)
.tipos_de_estructura <- c(
  "tipo_declarado_distinto", "columnas_duplicadas",
  "nombres_columnas_problematicos", "relacion_funcional_columnas",
  "relacion_orden_columnas", "relacion_aritmetica_columnas",
  "crs_no_declarado", "dato_personal_posible", "tipo_compuesto_no_analizado"
)
# Lo que el perfil publica por columna -el valor constante, la moda, los
# distintos- y que cambia con la misma cuenta de filas afectadas.
.tipos_de_contenido <- c(
  "constante", "casi_constante", "alta_cardinalidad", "posible_identificador"
)
.tipos_de_filas_o_estructura <- c(
  "clave_con_ausentes", "identificador_con_ausentes", "ausencia_dependiente",
  "regla_silencia_ausencia", "posible_ausencia_estructural", "apilamiento",
  "bloqueo_por_con_perdida"
)
.COMPONENTES_REQUERIDOS <- c(
  stats::setNames(rep("valores|estructura", length(.tipos_de_valores)),
                  .tipos_de_valores),
  stats::setNames(rep("estructura", length(.tipos_de_estructura)),
                  .tipos_de_estructura),
  stats::setNames(rep("filas|estructura", length(.tipos_de_filas_o_estructura)),
                  .tipos_de_filas_o_estructura),
  stats::setNames(rep("detalle", length(.tipos_de_contenido)), .tipos_de_contenido),
  faltantes = "filas&detalle",
  casi_clave = "filas|detalle",
  fecha_partida_columnas = "valores",
  desviacion_benford = "valores",
  faltantes_disfrazados = "valores",
  filas_duplicadas = "filas|valores",
  filas_vacias = "filas|valores",
  clave_no_unica = "filas"
)

# Las alternativas requeridas de un tipo, como lista de vectores. Un tipo que
# la tabla no conoce se verifica con cualquiera de las tres huellas.
.componentes_requeridos <- function(tipo) {
  texto <- .COMPONENTES_REQUERIDOS[as.character(tipo)]
  if (is.na(texto)) texto <- "valores|filas|estructura"
  lapply(strsplit(texto, "&", fixed = TRUE)[[1L]], function(g) {
    strsplit(g, "|", fixed = TRUE)[[1L]]
  })
}

# Lo que se compara es siempre un campo estructurado -severidad, cantidad de
# filas afectadas, huellas-, nunca el texto impreso de la evidencia: ese cambia
# con `OutDec` y con el orden de los ejemplos.
#
# El estado lo decide el componente REQUERIDO del tipo de hallazgo (ver
# `.COMPONENTES_REQUERIDOS`), no cualquier componente que se pueda calcular:
#
# * `revisar`: algun componente comparado difiere (requerido o no), o uno
#   requerido existia al decidir y ahora no se puede calcular;
# * `decidido_sin_verificar`: algun requerido no se guardo al decidir o no se puede
#   calcular de ningun lado; `verificacion` dice cual;
# * decidido y verificado: todo requerido se comparo y coincide, y todo extra
#   comparable de los dos lados tambien.
#
# Un componente no requerido que falta de un lado solo se declara en
# `verificacion`, y no cambia el estado.
.estados_hallazgos <- function(h, tabla, decisiones, base = NULL) {
  n <- nrow(h)
  if (is.null(base)) base <- list(datos = NULL, columnas = NULL, tabla = tabla)
  repetida <- .hallazgo_de_columna_repetida(h, base$nombres_columnas)
  ids <- .ids_hallazgos(h, tabla, repetida)
  salida <- data.frame(
    id_hallazgo = ids,
    estado_decision = rep("pendiente", n),
    decision = rep(NA_character_, n),
    motivo_decision = rep(NA_character_, n),
    decidido_por = rep(NA_character_, n),
    fecha_decision = rep(NA_character_, n),
    cambio_desde_decision = rep(NA_character_, n),
    verificacion = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  if (!n || is.null(decisiones) || !nrow(decisiones)) return(salida)
  i <- match(.clave_bytes(ids), .clave_bytes(decisiones$id_hallazgo))
  con <- which(!is.na(i))
  if (!length(con)) return(salida)
  d <- decisiones[i[con], , drop = FALSE]
  huellas <- .huellas_hallazgos(h, base, con, repetida)[con, , drop = FALSE]
  actual <- list(
    valores = huellas$huella_valores, filas = huellas$huella_filas,
    estructura = .estructura_hallazgos(h, base, con)[con],
    detalle = .detalle_hallazgos(h, base, con)[con]
  )
  guardado <- list(
    valores = d$huella_valores, filas = d$huella_filas,
    estructura = d$huella_estructura, detalle = d$detalle
  )
  componentes <- c("valores", "filas", "estructura", "detalle")
  por <- function(f) vapply(componentes, function(k) f(actual[[k]], guardado[[k]]),
                            logical(length(con)))
  if (length(con) == 1L) {
    por <- function(f) matrix(
      vapply(componentes, function(k) f(actual[[k]], guardado[[k]]), logical(1L)),
      nrow = 1L, dimnames = list(NULL, componentes)
    )
  }
  difiere <- por(function(a, g) !is.na(a) & !is.na(g) & a != g)
  coincide <- por(function(a, g) !is.na(a) & !is.na(g) & a == g)
  perdida <- por(function(a, g) is.na(a) & !is.na(g))
  nueva <- por(function(a, g) !is.na(a) & is.na(g))
  tipos <- as.character(h$tipo_hallazgo[con])
  grupos <- lapply(tipos, .componentes_requeridos)
  # Solo los componentes REQUERIDOS del tipo deciden el estado: uno que no lo es
  # cambia sin que el hallazgo haya cambiado en lo que lo define, y solo se
  # declara en `verificacion`.
  requerido <- t(vapply(grupos, function(g) componentes %in% unlist(g),
                        logical(length(componentes))))
  if (length(con) == 1L) requerido <- matrix(requerido, nrow = 1L)
  colnames(requerido) <- componentes
  cambio <- cbind(
    severidad = !.mismo_texto(h$severidad[con], d$severidad),
    n_afectados = !.mismo_numero(h$n_afectados[con], d$n_afectados),
    difiere & requerido
  )
  # Por cada hallazgo: lo requerido que se comparo y coincide, lo requerido que
  # existia al decidir y ahora no se puede calcular, y por que falta el resto.
  evaluacion <- lapply(seq_along(con), function(k) {
    perdidos <- character()
    faltas <- character()
    satisfecho <- TRUE
    for (g in grupos[[k]]) {
      if (any(coincide[k, g])) next
      satisfecho <- FALSE
      perdidos <- c(perdidos, g[perdida[k, g]])
      faltas <- c(faltas, paste0(
        paste(g, collapse = " o "), ": ",
        if (any(perdida[k, g])) "no se pudo calcular ahora" else
          if (any(nueva[k, g])) "no se guardo al decidir" else
          "no se puede calcular (sin datos, sin clave o con la traza truncada)"
      ))
    }
    requeridos <- unlist(grupos[[k]])
    extras <- setdiff(componentes, requeridos)
    nuevos <- extras[nueva[k, extras]]
    perdidos_extra <- extras[perdida[k, extras]]
    cambiados_extra <- extras[difiere[k, extras]]
    notas <- c(
      if (length(cambiados_extra)) {
        paste0(cambiados_extra, ": cambiaron (no requerido para este tipo)")
      },
      if (length(nuevos)) paste0(nuevos, " (no requerido): no se guardo al decidir"),
      if (length(perdidos_extra)) {
        paste0(perdidos_extra, " (no requerido): no se puede calcular ahora")
      }
    )
    if (is.na(.COMPONENTES_REQUERIDOS[tipos[[k]]])) {
      notas <- c(notas, "tipo sin componente requerido declarado")
    }
    list(satisfecho = satisfecho, perdidos = unique(perdidos), faltas = faltas,
         notas = notas)
  })
  satisfecho <- vapply(evaluacion, `[[`, logical(1L), "satisfecho")
  perdio_requerido <- vapply(evaluacion, function(e) length(e$perdidos) > 0L,
                             logical(1L))
  # `revisar` es solo "cambio": un requerido que existia y ya no se puede
  # calcular no es un cambio sino algo que no se puede confirmar, y deja la
  # decision sin verificar.
  cambiado <- rowSums(cambio) > 0L
  # Una columna con nombre repetido solo se distingue por su contenido.
  sin_huella <- !cambiado & !satisfecho & repetida[con]
  cambiado <- cambiado | sin_huella
  sin_verificar <- !cambiado & !satisfecho
  estado <- ifelse(
    cambiado, "revisar",
    ifelse(sin_verificar, "decidido_sin_verificar",
           unname(.ESTADO_POR_DECISION[d$decision]))
  )
  texto_cambio <- vapply(seq_along(con), function(k) {
    partes <- colnames(cambio)[cambio[k, ]]
    if (sin_huella[[k]] && !length(partes)) partes <- "columna_repetida_sin_huella"
    paste(partes, collapse = ", ")
  }, character(1L))
  declaracion <- vapply(seq_along(con), function(k) {
    partes <- c(if (sin_verificar[[k]]) evaluacion[[k]]$faltas,
                evaluacion[[k]]$notas)
    if (sin_verificar[[k]] && !is.na(huellas$huella_motivo[[k]])) {
      partes <- c(partes, huellas$huella_motivo[[k]])
    }
    if (sin_verificar[[k]] && isTRUE(base$datos_enmascarados)) {
      partes <- c(partes, "los datos conservados estan enmascarados (proteger_datos_personales)")
    }
    if (length(partes)) paste(partes, collapse = "; ") else NA_character_
  }, character(1L))
  salida$estado_decision[con] <- estado
  salida$decision[con] <- d$decision
  salida$motivo_decision[con] <- d$motivo
  salida$decidido_por[con] <- d$quien
  salida$fecha_decision[con] <- d$fecha
  salida$cambio_desde_decision[con] <- ifelse(cambiado, texto_cambio, NA_character_)
  salida$verificacion[con] <- declaracion
  salida
}

.decisiones_adjuntas <- function(x) {
  adjunto <- if (inherits(x, "hallazgos_por_grupo")) {
    attr(x, "decisiones_hallazgos", exact = TRUE)
  } else if (is.list(x)) x[["decisiones_hallazgos"]] else NULL
  adjunto
}

# Los estados de las filas de `hallazgos(x)`, o NULL si no hay decisiones. Se
# calcularon al aplicar (ahi estan los datos, si se pasaron) y se vuelven a
# alinear por id con los hallazgos del objeto.
.estados_de_objeto <- function(x) {
  adjunto <- .decisiones_adjuntas(x)
  if (is.null(adjunto)) return(NULL)
  base <- .base_hallazgos_decision(x, adjunto$nombre_tabla)
  repetida <- .hallazgo_de_columna_repetida(base$h, base$nombres_columnas)
  ids <- .ids_hallazgos(base$h, base$tabla, repetida)
  i <- match(.clave_bytes(ids), .clave_bytes(adjunto$estados$id_hallazgo))
  salida <- adjunto$estados[i, , drop = FALSE]
  salida$id_hallazgo <- ids
  falta <- is.na(i)
  salida$estado_decision[falta] <- "pendiente"
  rownames(salida) <- NULL
  salida
}

# Lo que agrega `hallazgos()` cuando el objeto trae decisiones: el id y el
# estado, al final de la tabla, para que las columnas de siempre no se muevan.
.hallazgos_con_estado <- function(tabla, x) {
  estados <- .estados_de_objeto(x)
  if (is.null(estados) || nrow(estados) != nrow(tabla)) return(tabla)
  for (campo in .COLUMNAS_ESTADO_DECISION) tabla[[campo]] <- estados[[campo]]
  tabla
}

# El resumen que usan las impresiones, el resumen y el informe.
.resumen_estados_decision <- function(x) {
  estados <- .estados_de_objeto(x)
  if (is.null(estados)) return(NULL)
  base <- .base_hallazgos_decision(x, .decisiones_adjuntas(x)$nombre_tabla)
  severidad <- as.character(base$h$severidad)
  decisiones <- .decisiones_adjuntas(x)$decisiones
  pendiente <- estados$estado_decision %in% c("pendiente", "revisar")
  sin_hallazgo <- setdiff(
    .clave_bytes(decisiones$id_hallazgo), .clave_bytes(estados$id_hallazgo)
  )
  list(
    estados = estados,
    n_error_pendiente = sum(pendiente & severidad == "error", na.rm = TRUE),
    n_sospechoso_pendiente = sum(pendiente & severidad == "sospechoso",
                                 na.rm = TRUE),
    # Un hallazgo `ok` informa y no pide revision: no es un pendiente.
    n_pendiente = sum(estados$estado_decision == "pendiente" &
                        severidad != "ok", na.rm = TRUE),
    n_revisar = sum(estados$estado_decision == "revisar"),
    n_aceptado = sum(estados$estado_decision == "aceptado"),
    n_descartado = sum(startsWith(estados$estado_decision, "descartado")),
    n_sin_verificar = sum(estados$estado_decision == "decidido_sin_verificar"),
    n_sin_hallazgo = length(sin_hallazgo)
  )
}

.tabla_conteo_estados <- function(x) {
  r <- .resumen_estados_decision(x)
  if (is.null(r)) return(NULL)
  base <- .base_hallazgos_decision(x, .decisiones_adjuntas(x)$nombre_tabla)
  conteo <- table(
    factor(r$estados$estado_decision, levels = .ESTADOS_DECISION),
    factor(as.character(base$h$severidad), levels = c("error", "sospechoso", "ok"))
  )
  salida <- data.frame(
    estado_decision = .ESTADOS_DECISION,
    error = as.integer(conteo[, "error"]),
    sospechoso = as.integer(conteo[, "sospechoso"]),
    ok = as.integer(conteo[, "ok"]),
    stringsAsFactors = FALSE
  )
  salida$total <- salida$error + salida$sospechoso + salida$ok
  salida
}

# Las lineas de consola: lo pendiente aparte de lo decidido.
.imprimir_decisiones <- function(x) {
  r <- .resumen_estados_decision(x)
  if (is.null(r)) return(invisible(NULL))
  cli::cli_h2("Decisiones sobre hallazgos")
  cli::cli_alert_info(.cli_literal(paste0(
    "Pendientes de revisar: ", r$n_pendiente, ". A revisar de nuevo porque ",
    "cambiaron: ", r$n_revisar, ". Aceptados: ", r$n_aceptado,
    ". Descartados: ", r$n_descartado, ". Decididos sin verificar: ",
    r$n_sin_verificar,
    " (ver hallazgos(): estado_decision y verificacion)."
  )))
  if (r$n_sin_hallazgo > 0L) {
    cli::cli_alert_info(.cli_literal(paste0(
      r$n_sin_hallazgo, if (r$n_sin_hallazgo == 1L) {
        " decisi\u00f3n sin hallazgo en esta corrida (ya no aparece)."
      } else " decisiones sin hallazgo en esta corrida (ya no aparecen)."
    )))
  }
  invisible(NULL)
}

# La seccion del informe HTML.
.seccion_decisiones_reporte <- function(x, max_filas) {
  conteo <- .tabla_conteo_estados(x)
  if (is.null(conteo)) return("")
  estados <- .estados_de_objeto(x)
  base <- .base_hallazgos_decision(x, .decisiones_adjuntas(x)$nombre_tabla)
  vista <- data.frame(
    id_hallazgo = estados$id_hallazgo,
    columna = .texto_o_vacio(base$h$columna),
    tipo_hallazgo = .texto_o_vacio(base$h$tipo_hallazgo),
    severidad = as.character(base$h$severidad),
    estado_decision = estados$estado_decision,
    motivo_decision = estados$motivo_decision,
    decidido_por = estados$decidido_por,
    fecha_decision = estados$fecha_decision,
    cambio_desde_decision = estados$cambio_desde_decision,
    verificacion = estados$verificacion,
    stringsAsFactors = FALSE
  )
  vista <- vista[vista$estado_decision != "pendiente", , drop = FALSE]
  paste0(
    "<h3>Decisiones sobre hallazgos</h3>",
    "<p class=\"nota\">Lo decidido no cuenta como pendiente pero se muestra con ",
    "su motivo. Un hallazgo que cambi\u00f3 desde la decisi\u00f3n queda ",
    "&laquo;revisar&raquo;: la decisi\u00f3n no se aplica sola.</p>",
    .html_tabla(conteo, Inf),
    if (nrow(vista)) .html_tabla(vista, max_filas) else ""
  )
}

# ---------------------------------------------------------------------------
# Interfaz publica
# ---------------------------------------------------------------------------

#' Identificar los hallazgos de un perfil entre corridas
#'
#' Devuelve una fila por hallazgo con su **identidad estable**, `id_hallazgo`:
#' la tabla, el grupo (en el resultado de [perfilar_por()]), la columna y el
#' tipo de hallazgo, separados por `|`. No depende del orden de las filas ni de
#' un número de llamada, así que el mismo hallazgo tiene el mismo id en cada
#' corrida y en tablas distintas los ids no chocan. Dos hallazgos del mismo tipo
#' en la misma columna se distinguen con un sufijo `|#k` que sale del orden de
#' sus bytes, no del orden de aparición.
#'
#' Con columnas de nombre repetido, el sufijo es la posición de la columna
#' entre las homónimas, que no cambia si se reordenan otras columnas.
#'
#' Es lo que une el archivo de decisiones con la corrida: ver
#' [decidir_hallazgos()] y [aplicar_decisiones()].
#'
#' @param x Resultado de [perfilar()], [analizar()], [perfilar_dbi()],
#'   [perfilar_coleccion()] o [perfilar_por()].
#' @param nombre_tabla Nombre de la tabla que se perfiló, para la identidad.
#'   Es obligatorio con el resultado de [perfilar_por()], que no guarda el
#'   nombre: sin él, dos tablas con los mismos grupos y columnas compartirían
#'   ids. En los demás objetos se toma del objeto (`nombre` de [perfilar()], la
#'   tabla calificada con su esquema en [perfilar_dbi()] y las colecciones) y
#'   sólo hace falta para cambiarlo.
#'
#' @return Un `data.frame` con `id_hallazgo`, `tabla`, `grupo` (`NA` fuera de
#'   [perfilar_por()]), `columna`, `tipo_hallazgo`, `severidad` y
#'   `n_afectados`. Si `x` trae decisiones aplicadas, agrega las columnas de
#'   estado de [aplicar_decisiones()].
#' @seealso [decidir_hallazgos()], [aplicar_decisiones()]
#' @export
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' head(identificar_hallazgos(perfil))
identificar_hallazgos <- function(x, nombre_tabla = NULL) {
  base <- .base_hallazgos_decision(x, nombre_tabla)
  h <- base$h
  estados <- if (is.null(nombre_tabla)) .estados_de_objeto(x) else NULL
  ids <- if (is.null(estados)) {
    .ids_hallazgos(
      h, base$tabla, .hallazgo_de_columna_repetida(h, base$nombres_columnas)
    )
  } else estados$id_hallazgo
  salida <- data.frame(
    id_hallazgo = ids,
    tabla = base$tabla,
    grupo = if ("grupo" %in% names(h)) as.character(h$grupo) else {
      rep(NA_character_, nrow(h))
    },
    columna = as.character(h$columna),
    tipo_hallazgo = as.character(h$tipo_hallazgo),
    severidad = as.character(h$severidad),
    n_afectados = as.numeric(h$n_afectados),
    stringsAsFactors = FALSE
  )
  if (!is.null(estados)) {
    for (campo in setdiff(.COLUMNAS_ESTADO_DECISION, "id_hallazgo")) {
      salida[[campo]] <- estados[[campo]]
    }
  }
  .tabla_lupa(salida)
}

.decisiones_vacias <- function() {
  salida <- data.frame(
    id_hallazgo = character(), tabla = character(), grupo = character(),
    columna = character(), tipo_hallazgo = character(),
    decision = character(), motivo = character(), quien = character(),
    fecha = character(), severidad = character(),
    n_afectados = numeric(), evidencia = character(),
    huella_valores = character(), huella_filas = character(),
    detalle = character(), huella_estructura = character(),
    stringsAsFactors = FALSE
  )
  class(salida) <- c("decisiones_hallazgos", "tabla_lupa", "data.frame")
  salida
}

# Los datos con que se calcula la huella: los que se pasan o, en `analizar()`, los
# que conserva el objeto.
.datos_para_huella <- function(datos, conservados) {
  if (is.null(datos)) return(conservados)
  if (!is.data.frame(datos)) datos <- tryCatch(as.data.frame(datos),
                                                error = function(e) NULL)
  if (is.null(datos)) {
    .detener("`datos` debe ser la tabla que se perfil\u00f3.", call. = FALSE)
  }
  datos
}

# `n_afectados` como lo devuelve una planilla: `1.000` con la configuracion de
# Uruguay es mil, y `0,5` es medio. El paquete no escribe ceros de cola, asi que
# un punto seguido de tres cifras es un separador de miles. Lo que no se puede
# leer sin adivinar se rechaza: una cifra equivocada cambiaria el veredicto.
.leer_cuentas_decision <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  salida <- rep(NA_real_, length(x))
  presente <- !is.na(x) & nzchar(x)
  miles <- presente & grepl("^-?[0-9]{1,3}(\\.[0-9]{3})+(,[0-9]+)?$", x)
  coma <- presente & !miles & grepl("^-?[0-9]+,[0-9]+$", x)
  llano <- presente & !miles & !coma &
    grepl("^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$", x)
  malo <- presente & !miles & !coma & !llano
  if (any(malo)) {
    .detener(
      "`n_afectados` no se pudo leer como n\u00famero: \"", x[malo][[1L]],
      "\". Guardarlo como n\u00famero sin separador de miles.", call. = FALSE
    )
  }
  salida[miles] <- as.numeric(sub(",", ".", gsub(".", "", x[miles], fixed = TRUE),
                                  fixed = TRUE))
  salida[coma] <- as.numeric(sub(",", ".", x[coma], fixed = TRUE))
  salida[llano] <- as.numeric(x[llano])
  salida
}

# La fecha se guarda como texto ISO 8601 con huso (`2026-01-02T03:04:05Z`, en
# UTC) y se lee cualquier forma de esa familia, con o sin huso (sin huso es UTC)
# y la que una planilla devuelve cuando la reinterpreta: dia/mes/anio. Si el dia
# y el mes caben los dos en 1 a 12 no se sabe cual es cual y se rechaza, en vez
# de adivinar; un numero de serie de la planilla, tambien.
.normalizar_fecha_decision <- function(x) {
  x <- trimws(as.character(x))
  una <- function(t) {
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    iso <- regmatches(t, regexec(paste0(
      "^([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[T ]([0-9]{2}):([0-9]{2})",
      "(?::([0-9]{2})(?:[.,][0-9]+)?)?)? ?(Z|[-+][0-9]{2}(?::?[0-9]{2})?)?$"
    ), t, perl = TRUE))[[1L]]
    barra <- regmatches(t, regexec(paste0(
      "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})(?: +([0-9]{1,2}):([0-9]{2})",
      "(?::([0-9]{2}))?)?$"
    ), t, perl = TRUE))[[1L]]
    if (length(iso)) {
      anio <- iso[[2L]]; mes <- iso[[3L]]; dia <- iso[[4L]]
      hora <- iso[[5L]]; minuto <- iso[[6L]]; segundo <- iso[[7L]]
      huso <- iso[[8L]]
    } else if (length(barra)) {
      a <- as.integer(barra[[2L]]); b <- as.integer(barra[[3L]])
      if (a <= 12L && b <= 12L && a != b) {
        .detener(
          "La fecha \"", t, "\" es ambigua (d\u00eda/mes o mes/d\u00eda). ",
          "Guardarla como texto ISO 8601, por ejemplo 2026-01-02T03:04:05Z.",
          call. = FALSE
        )
      }
      if (a > 12L) { dia <- a; mes <- b } else { dia <- b; mes <- a }
      anio <- barra[[4L]]; hora <- barra[[5L]]; minuto <- barra[[6L]]
      segundo <- barra[[7L]]; huso <- ""
    } else {
      .detener(
        "La fecha \"", t, "\" no se reconoce. Guardarla como texto ISO 8601, ",
        "por ejemplo 2026-01-02T03:04:05Z.", call. = FALSE
      )
    }
    num <- function(v) if (is.na(v) || !nzchar(v)) 0 else as.numeric(v)
    instante <- tryCatch(
      ISOdatetime(
        as.numeric(anio), as.numeric(mes), as.numeric(dia), num(hora),
        num(minuto), num(segundo), tz = "UTC"
      ), error = function(e) NA
    )
    if (is.na(instante)) {
      .detener("La fecha \"", t, "\" no es una fecha v\u00e1lida.", call. = FALSE)
    }
    if (nzchar(huso) && !identical(huso, "Z")) {
      limpio <- gsub(":", "", sub("^[-+]", "", huso))
      desplazamiento <- as.numeric(substr(limpio, 1L, 2L)) * 3600 +
        if (nchar(limpio) > 2L) as.numeric(substr(limpio, 3L, 4L)) * 60 else 0
      instante <- instante - (if (startsWith(huso, "-")) -1 else 1) * desplazamiento
    }
    format(instante, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  vapply(x, una, character(1L), USE.NAMES = FALSE)
}

.validar_decisiones <- function(decisiones, argumento = "decisiones") {
  if (!is.data.frame(decisiones)) {
    .detener("`", argumento, "` debe ser una tabla de decisiones: ",
             "decidir_hallazgos() o leer_decisiones().", call. = FALSE)
  }
  obligatorias <- c("id_hallazgo", "decision", "motivo", "quien", "fecha")
  faltan <- setdiff(obligatorias, names(decisiones))
  if (length(faltan)) {
    .detener("A `", argumento, "` le faltan las columnas: ",
             paste(faltan, collapse = ", "), ".", call. = FALSE)
  }
  d <- as.data.frame(decisiones, stringsAsFactors = FALSE)
  for (campo in setdiff(.COLUMNAS_DECISIONES, "n_afectados")) {
    d[[campo]] <- if (campo %in% names(d)) as.character(d[[campo]]) else {
      rep(NA_character_, nrow(d))
    }
  }
  d$n_afectados <- if ("n_afectados" %in% names(decisiones)) {
    .leer_cuentas_decision(decisiones$n_afectados)
  } else rep(NA_real_, nrow(d))
  d <- d[.COLUMNAS_DECISIONES]
  vacio <- function(x) is.na(x) | !nzchar(trimws(x))
  if (any(vacio(d$id_hallazgo))) {
    .detener("Hay decisiones sin `id_hallazgo`.", call. = FALSE)
  }
  malas <- !d$decision %in% .DECISIONES_HALLAZGO
  if (any(malas)) {
    .detener(
      "`decision` debe ser una de: ",
      paste(.DECISIONES_HALLAZGO, collapse = ", "), ". Fila ",
      which(malas)[[1L]], ": ", paste0("\"", .texto_o_vacio(d$decision[malas][[1L]]), "\""),
      ".", call. = FALSE
    )
  }
  if (any(vacio(d$motivo))) {
    .detener("Toda decisi\u00f3n lleva `motivo`: la fila ",
             which(vacio(d$motivo))[[1L]], " no lo tiene.", call. = FALSE)
  }
  if (any(vacio(d$quien)) || any(vacio(d$fecha))) {
    .detener("Toda decisi\u00f3n lleva `quien` y `fecha`.", call. = FALSE)
  }
  d$fecha <- .normalizar_fecha_decision(d$fecha)
  repetidos <- duplicated(.clave_bytes(d$id_hallazgo))
  if (any(repetidos)) {
    .detener("`id_hallazgo` repetido en las decisiones: ",
             d$id_hallazgo[repetidos][[1L]],
             ". Cada hallazgo tiene una sola decisi\u00f3n vigente.", call. = FALSE)
  }
  rownames(d) <- NULL
  class(d) <- c("decisiones_hallazgos", "tabla_lupa", "data.frame")
  d
}

#' Decidir sobre hallazgos: aceptarlos o descartarlos con su motivo
#'
#' Registra qué se resolvió de uno o varios hallazgos, con el motivo, quién y
#' cuándo, en una tabla que se guarda en un archivo editable
#' ([guardar_decisiones()]) y se aplica en la corrida siguiente
#' ([aplicar_decisiones()]).
#'
#' Hay tres decisiones:
#'
#' * `"aceptado"`: el hallazgo es un defecto real que se reconoce y se acepta
#'   (no se corrige ahora, o se corrige por otra vía).
#' * `"falso_positivo"`: el paquete se equivocó; el dato está bien.
#' * `"propiedad_legitima"`: el hallazgo describe algo cierto del dato que no es
#'   un defecto (por ejemplo, una columna casi vacía porque así es el proceso).
#'
#' Cada decisión guarda, además, lo que se vio al decidir: la severidad, la
#' cantidad de filas afectadas y una **huella del contenido afectado** que no
#' depende del orden de las filas. Es lo que permite, en la corrida siguiente,
#' saber si el hallazgo **cambió**: si cambia cualquiera de ellas, la decisión no
#' se aplica en silencio y el hallazgo vuelve como `revisar`. La evidencia se
#' guarda como referencia para quien revisa, pero no se compara: su texto cambia
#' con `OutDec` o con el orden de los ejemplos.
#'
#' La huella es de dos clases, y cada una necesita algo:
#'
#' * de **valores**: los valores afectados. Hace falta la tabla (`datos`, o un
#'   [analizar()] con `conservar_datos = TRUE`); sin ella, la decisión guarda sólo
#'   las cuentas.
#' * de **filas**: las claves de las filas afectadas. Hace falta una clave
#'   (`clave` de [perfilar()] o la del catálogo): sin clave las filas sólo se
#'   nombran por su posición, y la posición depende del orden, así que no se
#'   hace. **Límite:** sin clave, un hallazgo de filas (`faltantes`,
#'   `filas_duplicadas`) cuyas filas cambian pero conserva la cifra no se
#'   detecta. Tampoco hay huella si el hallazgo localiza más filas que el límite
#'   de la trazabilidad.
#'
#' Es un hash de las partes, de costo lineal en las filas afectadas.
#'
#' En `faltantes` y `faltantes_disfrazados` se guarda además el **detalle**:
#' cuántas ausencias son declaradas y cuántas centinelas, y cuáles son los
#' centinelas numéricos (ordenados), tomados de lo que el perfil publica por
#' columna. Una celda que pasa de `NA` a `999`, o de `999` a `-9`, es la misma
#' cifra sobre las mismas filas con otro contenido, y la decisión vuelve a
#' `revisar`. **Límite:** los centinelas de texto (`"N/A"`, `"NULL"`) el perfil
#' los publica sólo como cuenta: cambiar uno por otro con la misma cuenta se ve
#' con `datos`, no sin ellos. Los hallazgos que no tienen filas
#' (`tipo_declarado_distinto`, `columnas_duplicadas`,
#' `nombres_columnas_problematicos`) se verifican con una huella de la
#' **estructura** —nombres y tipos de las columnas involucradas—, que sale del
#' perfil y no necesita datos.
#'
#' El estado lo decide el **componente requerido del tipo de hallazgo**, los que
#' pueden cambiar sin que cambie la cuenta: en los hallazgos de valores
#' (`outliers`, dominios, patrones, `faltantes_disfrazados`, casi duplicados...),
#' los valores; en `faltantes`, las filas y el detalle; en `filas_duplicadas` y
#' otros de filas, las filas (o los valores de las filas completas); en los
#' estructurales, la estructura. Está escrito en una tabla cerrada, y una
#' prueba avisa si el paquete publica un tipo que no está. Con todo lo requerido
#' comparado y coincidente la decisión queda verificada; si algún componente
#' comparado difiere, es `revisar` (sólo significa «cambió»); si algún
#' requerido no se guardó al decidir, no se pudo calcular ahora o no se puede
#' calcular de ningún lado, es `decidido_sin_verificar` y `verificacion` dice
#' cuál. Un componente no requerido que falta de un lado sólo se declara en
#' `verificacion`. Así, decidir con clave y aplicar con clave y `datos` verifica
#' `faltantes`, pero deja sin verificar a `outliers` (que necesita los valores
#' al decidir); y el cambio de un centinela de texto por otro con la misma
#' cuenta se ve sólo con `datos` en los dos lados, porque
#' `faltantes_disfrazados` requiere los valores. En el resultado de
#' [perfilar_por()] las filas son posiciones de la tabla: con `datos` la huella
#' de valores se calcula como en [perfilar()] y la estructura sale de los
#' nombres y clases de sus columnas; sin `datos`, sus hallazgos de valores quedan
#' sin verificar. Los hallazgos que cambian con la misma cuenta sin filas
#' (`constante`, `casi_constante`, `alta_cardinalidad`, `casi_clave`,
#' `posible_identificador`) se comparan por lo que el perfil publica de la
#' columna: el valor constante o la moda, su frecuencia y los distintos.
#' Con la protección de datos personales activa,
#' los datos que conserva [analizar()] están enmascarados y no se usan para la
#' huella de valores. **Límites:** en [perfilar_dbi()] sin `orden_muestra`, la
#' muestra puede cambiar de filas entre corridas y el detalle con ella; el
#' motivo de una decisión se publica tal cual en [reportar()], también con
#' `proteger_datos_personales`: no conviene escribir en él valores de columnas
#' personales.
#'
#' Se elige a qué hallazgos se aplica con `id_hallazgo` (los de
#' [identificar_hallazgos()], o la tabla misma filtrada) y/o con los filtros
#' `tabla`, `columna` y `tipo_hallazgo`, que se combinan con «y». Hace falta al
#' menos un criterio, y debe coincidir al menos un hallazgo.
#'
#' @param x Resultado de [perfilar()], [analizar()], [perfilar_dbi()],
#'   [perfilar_coleccion()] o [perfilar_por()].
#' @param decision Una de `"aceptado"`, `"falso_positivo"` o
#'   `"propiedad_legitima"`.
#' @param motivo Texto con el motivo; no puede estar vacío.
#' @param quien Texto: quién decide.
#' @param id_hallazgo Ids de [identificar_hallazgos()], o una tabla con la
#'   columna `id_hallazgo`.
#' @param tabla,columna,tipo_hallazgo Filtros de texto (uno o varios valores).
#' @param fecha Fecha y hora de la decisión; por omisión, ahora.
#' @param decisiones Decisiones anteriores (de [decidir_hallazgos()] o
#'   [leer_decisiones()]) a las que se agregan las nuevas. Una decisión nueva
#'   sobre un hallazgo ya decidido reemplaza a la anterior.
#' @param datos La tabla que se perfiló, para la huella de los valores
#'   afectados. Un [analizar()] con `conservar_datos = TRUE` la trae.
#' @param nombre_tabla Nombre de la tabla que se perfiló. Obligatorio con el
#'   resultado de [perfilar_por()], que no lo guarda; ver
#'   [identificar_hallazgos()].
#'
#' @return Una tabla `decisiones_hallazgos`: una fila por hallazgo decidido, con
#'   `id_hallazgo`, `tabla`, `grupo`, `columna`, `tipo_hallazgo`, `decision`,
#'   `motivo`, `quien`, `fecha` y lo visto al decidir (`severidad`,
#'   `n_afectados`, `evidencia`).
#' @seealso [aplicar_decisiones()], [guardar_decisiones()],
#'   [identificar_hallazgos()]
#' @export
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' decisiones <- decidir_hallazgos(
#'   perfil, "propiedad_legitima",
#'   motivo = "La columna es opcional por diseño del formulario.",
#'   quien = "equipo de datos", tipo_hallazgo = "faltantes"
#' )
#' decisiones
decidir_hallazgos <- function(x, decision = c("aceptado", "falso_positivo",
                                              "propiedad_legitima"),
                              motivo, quien, id_hallazgo = NULL, tabla = NULL,
                              columna = NULL, tipo_hallazgo = NULL,
                              fecha = Sys.time(), decisiones = NULL,
                              datos = NULL, nombre_tabla = NULL) {
  decision <- match.arg(decision)
  if (!.es_texto_escalar(motivo) || !nzchar(trimws(motivo))) {
    .detener("`motivo` debe ser un texto no vac\u00edo.", call. = FALSE)
  }
  if (!.es_texto_escalar(quien) || !nzchar(trimws(quien))) {
    .detener("`quien` debe ser un texto no vac\u00edo.", call. = FALSE)
  }
  fecha <- format(.fecha_argumento_utc(fecha), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.data.frame(id_hallazgo)) id_hallazgo <- id_hallazgo$id_hallazgo
  criterios <- list(
    id_hallazgo = id_hallazgo, tabla = tabla, columna = columna,
    tipo_hallazgo = tipo_hallazgo
  )
  usados <- !vapply(criterios, is.null, logical(1L))
  if (!any(usados)) {
    .detener("Indicar a qu\u00e9 hallazgos se aplica la decisi\u00f3n: `id_hallazgo`, ",
             "`tabla`, `columna` o `tipo_hallazgo`.", call. = FALSE)
  }
  previas <- if (is.null(decisiones)) .decisiones_vacias() else {
    .validar_decisiones(decisiones)
  }
  motivo_vacia <- .motivo_coleccion_sin_perfiles(x)
  if (!is.null(motivo_vacia)) {
    .detener("Esta colecci\u00f3n ", motivo_vacia, call. = FALSE)
  }
  base <- .base_hallazgos_decision(x, nombre_tabla)
  base$datos <- .datos_para_huella(datos, base$datos)
  if (inherits(x, "hallazgos_por_grupo")) base <- .preparar_por_grupo(base, x, datos)
  h <- base$h
  repetida <- .hallazgo_de_columna_repetida(h, base$nombres_columnas)
  ids <- .ids_hallazgos(h, base$tabla, repetida)
  elegido <- rep(TRUE, nrow(h))
  if (!is.null(id_hallazgo)) {
    elegido <- elegido & .clave_bytes(ids) %in% .clave_bytes(id_hallazgo)
  }
  if (!is.null(tabla)) {
    elegido <- elegido & .clave_bytes(base$tabla) %in% .clave_bytes(tabla)
  }
  if (!is.null(columna)) {
    elegido <- elegido & .clave_bytes(h$columna) %in% .clave_bytes(columna)
  }
  if (!is.null(tipo_hallazgo)) {
    elegido <- elegido &
      .clave_bytes(h$tipo_hallazgo) %in% .clave_bytes(tipo_hallazgo)
  }
  if (!any(elegido)) {
    .detener("Ning\u00fan hallazgo coincide con los criterios indicados.", call. = FALSE)
  }
  k <- which(elegido)
  huellas <- .huellas_hallazgos(h, base, k, repetida)[k, , drop = FALSE]
  nuevas <- data.frame(
    id_hallazgo = ids[k],
    tabla = base$tabla[k],
    grupo = if ("grupo" %in% names(h)) as.character(h$grupo[k]) else {
      rep(NA_character_, length(k))
    },
    columna = as.character(h$columna[k]),
    tipo_hallazgo = as.character(h$tipo_hallazgo[k]),
    decision = decision, motivo = motivo, quien = quien, fecha = fecha,
    severidad = as.character(h$severidad[k]),
    n_afectados = as.numeric(h$n_afectados[k]),
    evidencia = as.character(h$evidencia[k]),
    huella_valores = huellas$huella_valores,
    huella_filas = huellas$huella_filas,
    detalle = .detalle_hallazgos(h, base, k)[k],
    huella_estructura = .estructura_hallazgos(h, base, k)[k],
    stringsAsFactors = FALSE
  )
  conservar <- !.clave_bytes(previas$id_hallazgo) %in% .clave_bytes(nuevas$id_hallazgo)
  unidas <- rbind(
    as.data.frame(previas, stringsAsFactors = FALSE)[conservar, , drop = FALSE],
    nuevas
  )
  .validar_decisiones(unidas)
}

#' Guardar y leer decisiones sobre hallazgos
#'
#' Escribe la tabla de [decidir_hallazgos()] como un CSV en UTF-8, que se edita
#' a mano y se versiona junto con el proyecto, y la vuelve a leer. Una fila por
#' hallazgo decidido; las columnas son las de la tabla de decisiones. Para
#' retirar una decisión, se borra su fila.
#'
#' El archivo se escribe en UTF-8 con marca de orden de bytes, que es lo que una
#' planilla necesita para abrirlo con sus acentos. Al leer se acepta UTF-8 con o
#' sin marca, con coma o punto y coma; si los bytes no son UTF-8 válido se leen
#' como Windows-1252 (lo que guarda una planilla de Windows) y se avisa. La
#' fecha se guarda como texto ISO 8601 con huso y se lee en esa forma o como
#' día/mes/año de una planilla (si el día y el mes son ambiguos, se rechaza); un
#' `n_afectados` con separador de miles de planilla (`1.000`) se lee como mil, y
#' lo que no se puede leer sin adivinar es un error. Se valida
#' todo: las decisiones válidas, un motivo, quién y cuándo en cada fila, y ningún
#' `id_hallazgo` repetido. Un archivo que no se puede leer entero se rechaza con
#' un error, nunca se devuelve vacío en silencio; los finales de línea dentro de
#' un motivo se conservan tal cual.
#'
#' @param decisiones Tabla de [decidir_hallazgos()].
#' @param archivo Ruta del CSV.
#' @param sobrescribir Si es `FALSE` (por omisión), no reemplaza un archivo
#'   existente.
#' @param separador `","` (por omisión) o `";"`, que es el que abre bien una
#'   planilla con configuración regional en español. `leer_decisiones()` detecta
#'   cuál se usó.
#'
#' @return `guardar_decisiones()` devuelve invisiblemente la ruta normalizada;
#'   `leer_decisiones()`, una tabla `decisiones_hallazgos` validada.
#' @seealso [decidir_hallazgos()], [aplicar_decisiones()]
#' @export
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' decisiones <- decidir_hallazgos(
#'   perfil, "falso_positivo", motivo = "Revisado en la fuente.",
#'   quien = "equipo de datos", tipo_hallazgo = "faltantes"
#' )
#' archivo <- tempfile(fileext = ".csv")
#' guardar_decisiones(decisiones, archivo)
#' leer_decisiones(archivo)
#' unlink(archivo)
guardar_decisiones <- function(decisiones, archivo, sobrescribir = FALSE,
                              separador = c(",", ";")) {
  decisiones <- .validar_decisiones(decisiones)
  separador <- match.arg(separador)
  if (!.es_texto_escalar(archivo)) {
    .detener("`archivo` debe ser una ruta no vac\u00eda.", call. = FALSE)
  }
  if (!is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      is.na(sobrescribir)) {
    .detener("`sobrescribir` debe ser TRUE o FALSE.", call. = FALSE)
  }
  directorio <- .validar_destino_archivo(archivo, sobrescribir)
  temporal <- tempfile(".lupa-decisiones-", tmpdir = directorio)
  on.exit(unlink(temporal), add = TRUE)
  plano <- as.data.frame(decisiones, stringsAsFactors = FALSE)
  # La marca de orden de bytes (BOM) es lo que le dice a una planilla que el
  # archivo es UTF-8: sin ella, Excel lo abre como Windows-1252 y los acentos se
  # rompen. `leer_decisiones()` la quita al leer.
  # El CSV se arma en memoria y se escribe como bytes: un archivo abierto en modo
  # texto convierte en Windows cada salto de linea, tambien el que va dentro de un
  # campo entre comillas, en un retorno de carro mas un salto, y el motivo volvia
  # cambiado. Los registros terminan en `\n` en todo sistema.
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), .csv_bytes_decisiones(plano, separador)), temporal)
  if (!file.copy(temporal, archivo, overwrite = sobrescribir)) {
    .detener("No se pudo guardar las decisiones en el destino indicado.",
             call. = FALSE)
  }
  invisible(normalizePath(archivo, mustWork = TRUE))
}

# El CSV de las decisiones como bytes UTF-8, con la forma que daba
# `write.table()`: texto entre comillas dobles (una comilla se escribe doble),
# numeros y logicos sin comillas, `NA` vacio, registros terminados en `\n`. Se
# arma a mano porque `write.table()` pasa el texto al encoding de la sesion antes
# de escribir, y bajo `C` un acento del motivo volvia escapado.
.csv_bytes_decisiones <- function(plano, separador) {
  citar <- function(v) {
    v <- enc2utf8(as.character(v))
    salida <- paste0("\"", gsub("\"", "\"\"", v, fixed = TRUE), "\"")
    salida[is.na(v)] <- ""
    salida
  }
  campo <- function(v) {
    if (is.factor(v) || is.character(v)) return(citar(v))
    salida <- as.character(v)
    salida[is.na(v)] <- ""
    salida
  }
  encabezado <- paste(citar(names(plano)), collapse = separador)
  filas <- if (nrow(plano)) {
    do.call(paste, c(unname(lapply(plano, campo)), list(sep = separador)))
  } else {
    character()
  }
  lineas <- enc2utf8(c(encabezado, filas))
  unlist(lapply(lineas, function(l) c(charToRaw(l), as.raw(0x0a))), use.names = FALSE)
}

# La marca para el retorno de carro DENTRO de un campo entre comillas: la
# lectura de base parte y normaliza los finales de linea, y el motivo vuelve con
# otro caracter. Se cambia por un byte de control antes de leer y se restituye
# despues; el que termina un registro, fuera de comillas, se descarta.
.BYTE_CR_MARCADO <- as.raw(1L)

.leer_decisiones_texto <- function(archivo) {
  crudo <- readBin(archivo, "raw", n = file.info(archivo)$size)
  if (length(crudo) >= 3L && identical(crudo[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    crudo <- crudo[-(1:3)]
  }
  if (!length(crudo)) {
    .detener("El archivo de decisiones est\u00e1 vac\u00edo.", call. = FALSE)
  }
  if (!any(crudo == .BYTE_CR_MARCADO)) {
    dentro <- (cumsum(crudo == as.raw(0x22)) %% 2L) == 1L
    es_cr <- crudo == as.raw(0x0d)
    crudo[es_cr & dentro] <- .BYTE_CR_MARCADO
    crudo <- crudo[!(es_cr & !dentro)]
    marcado <- any(es_cr & dentro)
  } else marcado <- FALSE
  texto <- rawToChar(crudo)
  if (validUTF8(texto)) {
    Encoding(texto) <- "UTF-8"
  } else {
    # Una planilla de Windows guarda en Windows-1252. Se lee asi y se dice, en
    # vez de devolver cero filas o texto roto.
    convertido <- iconv(texto, "CP1252", "UTF-8")
    if (is.na(convertido)) convertido <- iconv(texto, "latin1", "UTF-8")
    if (is.na(convertido)) {
      .detener(
        "El archivo de decisiones no es UTF-8 v\u00e1lido ni Windows-1252. ",
        "Guardarlo como CSV UTF-8.", call. = FALSE
      )
    }
    .avisar(
      "El archivo de decisiones no era UTF-8: se ley\u00f3 como Windows-1252. ",
      "Guardarlo de nuevo con guardar_decisiones() lo deja en UTF-8.",
      call. = FALSE
    )
    texto <- convertido
  }
  list(texto = texto, marcado = marcado)
}

#' @rdname guardar_decisiones
#' @export
leer_decisiones <- function(archivo) {
  if (!.es_texto_escalar(archivo) || !file.exists(archivo) ||
      dir.exists(archivo)) {
    .detener("`archivo` debe identificar un CSV de decisiones existente.",
             call. = FALSE)
  }
  leido <- .leer_decisiones_texto(archivo)
  texto <- leido$texto
  lineas <- strsplit(texto, "\n", fixed = TRUE)[[1L]]
  encabezado <- if (length(lineas)) lineas[[1L]] else ""
  n_coma <- lengths(regmatches(encabezado, gregexpr(",", encabezado, fixed = TRUE)))
  n_punto_coma <- lengths(regmatches(encabezado, gregexpr(";", encabezado, fixed = TRUE)))
  separador <- if (n_punto_coma > n_coma) ";" else ","
  # Nunca cero decisiones sin decirlo: cualquier aviso de la lectura es un
  # error, y un archivo con filas que no produce ninguna se rechaza.
  plano <- tryCatch(
    withCallingHandlers(
      utils::read.csv(
        text = texto, sep = separador, colClasses = "character", na.strings = "",
        check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
      ),
      warning = function(w) {
        .detener("No se pudo leer el archivo de decisiones: ",
                 conditionMessage(w), call. = FALSE)
      }
    ),
    error = function(e) {
      .detener("No se pudo leer el archivo de decisiones: ",
               conditionMessage(e), call. = FALSE)
    }
  )
  names(plano) <- sub("^\ufeff", "", names(plano))
  con_datos <- sum(nzchar(trimws(lineas[-1L])))
  if (!nrow(plano) && con_datos > 0L) {
    .detener("El archivo de decisiones tiene filas que no se pudieron leer.",
             call. = FALSE)
  }
  if (isTRUE(leido$marcado)) {
    for (campo in names(plano)) {
      plano[[campo]] <- gsub(rawToChar(.BYTE_CR_MARCADO), "\r", plano[[campo]],
                             fixed = TRUE, useBytes = TRUE)
    }
  }
  .validar_decisiones(plano, "archivo")
}

#' Aplicar las decisiones guardadas a una corrida
#'
#' Cruza las decisiones de corridas anteriores con los hallazgos de esta. Cada
#' hallazgo queda en uno de estos estados, en la columna `estado_decision` de
#' [hallazgos()] y de [identificar_hallazgos()]:
#'
#' * `"pendiente"`: sin decisión.
#' * `"aceptado"`, `"descartado_falso_positivo"`,
#'   `"descartado_propiedad_legitima"`: decidido, con su motivo, quién y cuándo
#'   (`motivo_decision`, `decidido_por`, `fecha_decision`). **No desaparece**:
#'   sigue en la tabla, pero deja de contarse como pendiente.
#' * `"decidido_sin_verificar"`: hay una decisión pero **no se pudo verificar**
#'   que el hallazgo sea el mismo, porque no hay con qué comparar su contenido
#'   (la decisión se tomó sin `datos` ni clave, o el archivo no trae las huellas,
#'   o la trazabilidad está truncada). Se muestra con su motivo y lo que faltó
#'   en `verificacion`, y se cuenta **aparte** de lo decidido y verificado: la
#'   decisión no se aplica en silencio. Pasar `datos` o una `clave` al decidir
#'   y al aplicar la convierte en una decisión verificada.
#' * `"revisar"`: hay una decisión, pero el hallazgo **cambió** desde que se
#'   decidió —otra severidad, otra cantidad de filas afectadas, otros valores o
#'   otras filas afectadas (ver la huella en [decidir_hallazgos()])— y la
#'   decisión no se aplica en silencio; `cambio_desde_decision` dice qué
#'   cambió. También es `revisar` si al decidir había huella y ahora no se puede
#'   calcular (por ejemplo, se aplica sin `datos`): no se puede confirmar que
#'   sea el mismo. Con otra muestra de la misma tabla las cifras cambian y el
#'   hallazgo vuelve a `revisar`: es lo honesto. Un hallazgo sobre una columna
#'   de nombre repetido sólo se distingue por su contenido: sin huella que
#'   comparar, vuelve a `revisar`.
#'
#' El estado del plan se **deriva** de las decisiones, no se acumula. La capa de
#' decisiones registra qué filas del plan tocó y qué valor tenían antes; al
#' reaplicar devuelve sólo esas filas a ese valor y aplica las decisiones
#' vigentes, así que aplicar dos veces, o con decisiones cambiadas, da lo mismo
#' que aplicar una vez las últimas, y las ediciones a mano de las demás filas
#' del plan se conservan.
#' [planificar_limpieza()] sobre un perfil con decisiones aplicadas las respeta
#' igual. Convertir el tipo de una columna (`convertir_tipo`) quita sus espacios
#' aunque se haya descartado `espacios_sobrantes`: es inherente a la conversión,
#' y la nota de esa acción lo dice.
#'
#' Sobre un [analizar()], las acciones del plan de limpieza de un hallazgo
#' **descartado** (falso positivo o propiedad legítima) quedan con
#' `aplicar = FALSE` y una nota en su justificación que nombra la decisión, para
#' que [aplicar()] por omisión no limpie lo que se dijo que es legítimo. Un
#' descarte **sin verificar** también deja sus acciones sin aplicar, con la nota
#' «revisala antes de limpiar»: nunca se modifica un dato sobre una decisión que
#' no se confirmó. Un `revisar` no aplica la decisión: el plan queda como estaba,
#' con una nota que nombra la decisión anterior y qué cambió. Las acciones de un
#' hallazgo aceptado no cambian. Sobre una colección que no conservó los perfiles de sus tablas no
#' hay hallazgos por tabla: se avisa y se devuelve la colección tal cual.
#'
#' Los hallazgos de la tabla entera de [perfilar_por()] (`hallazgos_tabla`, el
#' atributo del resultado) no se pueden decidir: las decisiones valen sobre las
#' filas de la tabla de hallazgos por grupo.
#'
#' Las decisiones que ya no encuentran su hallazgo (el defecto se corrigió, o la
#' tabla cambió de nombre) no se pierden: se cuentan en la impresión.
#'
#' `print()` muestra los conteos de error y sospechoso sólo con lo pendiente (lo
#' pendiente y lo que hay que revisar de nuevo), y aparte lo decidido;
#' `summary()` de un perfil trae el cruce de estado por severidad en el atributo
#' `decisiones`; [reportar()] agrega una sección con lo decidido y su motivo. El
#' perfil sin decisiones aplicadas es idéntico al de siempre.
#'
#' Las huellas distinguen el tipo y el largo de cada valor (`NA` de `"NA"`, un
#' número de su texto, una fecha de su texto, `0.3` de `0.1 + 0.2`).
#'
#' La identidad de un hallazgo incluye el nombre de la tabla (`nombre` en
#' [perfilar()]; el nombre de la tabla en [perfilar_dbi()] y las colecciones):
#' con otro nombre, las decisiones no se aplican y los hallazgos vuelven como
#' pendientes. Dos tablas a las que se da el mismo nombre son, por declaración,
#' la misma: las decisiones de una valen en la otra. Por eso un nombre que no
#' identifica —`datos`, el que se usa cuando no hay uno, o el que dejó un perfil
#' de [perfilar_dbi()] guardado por una versión anterior con un `DBI::Id`— se
#' rechaza y se pide `nombre_tabla`.
#'
#' @param x Resultado de [perfilar()], [analizar()], [perfilar_dbi()],
#'   [perfilar_coleccion()] o [perfilar_por()].
#' @param decisiones Tabla de [decidir_hallazgos()] o ruta de un CSV de
#'   [guardar_decisiones()].
#' @param datos La tabla que se perfiló, para comprobar la huella de los valores
#'   afectados. Si las decisiones se tomaron con ella y no se pasa, el hallazgo
#'   vuelve a `revisar`.
#' @param nombre_tabla Nombre de la tabla que se perfiló; obligatorio con el
#'   resultado de [perfilar_por()].
#'
#' @return `x`, con las decisiones adjuntas (en el campo `decisiones_hallazgos`;
#'   en el resultado de [perfilar_por()], en el atributo del mismo nombre y con
#'   las columnas de estado agregadas a la tabla).
#' @seealso [decidir_hallazgos()], [guardar_decisiones()],
#'   [identificar_hallazgos()]
#' @export
#' @examples
#' perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
#' decisiones <- decidir_hallazgos(
#'   perfil, "falso_positivo", motivo = "Revisado en la fuente.",
#'   quien = "equipo de datos", tipo_hallazgo = "faltantes"
#' )
#' nuevo <- aplicar_decisiones(perfil, decisiones)
#' table(hallazgos(nuevo)$estado_decision)
aplicar_decisiones <- function(x, decisiones, datos = NULL, nombre_tabla = NULL) {
  if (is.character(decisiones)) decisiones <- leer_decisiones(decisiones)
  decisiones <- .validar_decisiones(decisiones)
  motivo_vacia <- .motivo_coleccion_sin_perfiles(x)
  if (!is.null(motivo_vacia)) {
    # Como `hallazgos()`: dice lo que pasa y devuelve el objeto, sin inventar
    # hallazgos por tabla donde no los hay.
    .avisar("Esta colecci\u00f3n ", motivo_vacia, " No hay a qu\u00e9 aplicar las decisiones.",
            call. = FALSE)
    return(x)
  }
  anterior <- .decisiones_adjuntas(x)
  if (inherits(x, "hallazgos_por_grupo")) {
    # Se quitan de a una con `[[<-`, que conserva los atributos de la tabla; un
    # subconjunto con `[` los perdia.
    for (campo in .COLUMNAS_ESTADO_DECISION) x[[campo]] <- NULL
    base <- .base_hallazgos_decision(x, nombre_tabla)
    base <- .preparar_por_grupo(base, x, datos)
    estados <- .estados_hallazgos(x, base$tabla, decisiones, base)
    for (campo in .COLUMNAS_ESTADO_DECISION) x[[campo]] <- estados[[campo]]
    attr(x, "nombre_tabla_decisiones") <- base$nombre
    attr(x, "decisiones_hallazgos") <- structure(
      list(decisiones = decisiones, estados = estados, nombre_tabla = base$nombre),
      class = "decisiones_aplicadas"
    )
    return(x)
  }
  base <- .base_hallazgos_decision(x, nombre_tabla)
  base$datos <- .datos_para_huella(datos, base$datos)
  estados <- .estados_hallazgos(base$h, base$tabla, decisiones, base)
  adjunto <- structure(
    list(decisiones = decisiones, estados = estados,
         nombre_tabla = nombre_tabla),
    class = "decisiones_aplicadas"
  )
  if (inherits(x, "analisis")) {
    # El estado del plan se DERIVA, no se acumula: se parte del plan actual con
    # las filas que toco la capa de decisiones devueltas a lo que tenian, y se
    # aplican las decisiones vigentes. Las ediciones del usuario sobre otras
    # filas no se pierden.
    plan <- .restaurar_filas_tocadas(x$plan_limpieza, anterior$plan_tocado)
    nuevo <- .plan_desde_decisiones(plan, base$h, estados, base)
    adjunto$plan_tocado <- .filas_tocadas_del_plan(plan, nuevo)
    x$plan_limpieza <- nuevo
  }
  x[["decisiones_hallazgos"]] <- adjunto
  x
}

# Un hallazgo descartado (falso positivo o propiedad legitima del dato) no se
# limpia: sus acciones del plan quedan con `aplicar = FALSE` y una nota que
# nombra la decision, para que `aplicar()` por omision no toque lo que el
# usuario dijo que es legitimo. Una decision descartada que no se pudo verificar
# tambien deja sus acciones sin aplicar -nunca se modifica un dato sobre una
# decision que no se confirmo-, con la nota "revisala antes de limpiar". Un
# `revisar` no aplica la decision: el plan queda como estaba, con una nota que
# nombra la decision anterior y que cambio. Las de un aceptado -un defecto que se
# reconoce- no cambian.
#
# Las acciones se cruzan con el hallazgo por columna, tipo y evidencia -la que el
# plan copia del hallazgo-, no solo por columna y tipo: dos hallazgos hermanos
# (dos `fecha_centinela` de la misma columna) no se contagian. Si dos hermanos
# son indistinguibles para el plan, solo se tocan sus acciones si los dos tienen
# la misma disposicion.
.plan_desde_decisiones <- function(plan, h, estados, base) {
  if (!is.data.frame(plan) || !nrow(plan) || !nrow(h) ||
      !all(c("columna", "hallazgo", "aplicar", "justificacion", "evidencia") %in%
             names(plan))) {
    return(plan)
  }
  repetida <- .hallazgo_de_columna_repetida(h, base$nombres_columnas)
  descarte <- !is.na(estados$decision) &
    estados$decision %in% c("falso_positivo", "propiedad_legitima") & !repetida
  disposicion <- rep(NA_character_, nrow(h))
  disposicion[descarte & startsWith(estados$estado_decision, "descartado")] <- "descartado"
  disposicion[descarte & estados$estado_decision == "decidido_sin_verificar"] <- "sin_verificar"
  disposicion[descarte & estados$estado_decision == "revisar"] <- "revisar"
  firma <- function(columna, tipo, evidencia) {
    paste(.clave_bytes(.texto_o_vacio(columna)), is.na(columna),
          .clave_bytes(as.character(tipo)), .clave_bytes(.texto_o_vacio(evidencia)),
          sep = "\037")
  }
  firma_h <- firma(h$columna, h$tipo_hallazgo, h$evidencia)
  firma_p <- firma(plan$columna, plan$hallazgo, plan$evidencia)
  plan$justificacion <- as.character(plan$justificacion)
  tocadas <- rep(FALSE, nrow(plan))
  for (i in which(!is.na(disposicion))) {
    hermanos <- which(firma_h == firma_h[[i]])
    if (!all(disposicion[hermanos] %in% disposicion[[i]])) next
    coincide <- firma_p == firma_h[[i]]
    if (!any(coincide)) next
    cabeza <- paste0(
      estados$decision[[i]], " (", estados$motivo_decision[[i]], "; ",
      estados$decidido_por[[i]], ", ", estados$fecha_decision[[i]], ")"
    )
    texto <- switch(
      disposicion[[i]],
      descartado = paste0(" Decisi\u00f3n sobre el hallazgo: ", cabeza, ": no se limpia."),
      sin_verificar = paste0(
        " Decisi\u00f3n sin verificar sobre el hallazgo: ", cabeza,
        ": revisala antes de limpiar; mientras tanto no se limpia."
      ),
      revisar = paste0(
        " Decisi\u00f3n anterior sobre el hallazgo, no aplicada porque cambi\u00f3 (",
        estados$cambio_desde_decision[[i]], "): ", cabeza, "."
      )
    )
    if (!identical(disposicion[[i]], "revisar")) plan$aplicar[coincide] <- FALSE
    plan$justificacion[coincide] <- paste0(plan$justificacion[coincide], texto)
    tocadas <- tocadas | coincide
    # Convertir el tipo de una columna quita los espacios de sus valores: es
    # inherente a la conversion, y la accion lo dice.
    if (identical(disposicion[[i]], "descartado") &&
        identical(as.character(h$tipo_hallazgo[[i]]), "espacios_sobrantes") &&
        !is.na(h$columna[[i]])) {
      conversion <- as.character(plan$estrategia) == "convertir_tipo" &
        !is.na(plan$columna) &
        .clave_bytes(plan$columna) == .clave_bytes(h$columna[[i]])
      plan$justificacion[conversion] <- paste0(
        plan$justificacion[conversion],
        " Se descart\u00f3 `espacios_sobrantes` en esta columna, pero convertir el ",
        "tipo quita sus espacios de todos modos."
      )
    }
  }
  # El grupo de acciones queda coherente con lo desactivado, y solo el de las
  # acciones tocadas: el resto del plan sigue como lo dejo `analizar()`.
  if (any(tocadas) && "grupo" %in% names(plan)) {
    sincronizado <- .sincronizar_decisiones(plan)
    en_grupo <- tocadas | (!is.na(plan$grupo) &
                             plan$grupo %in% plan$grupo[tocadas])
    plan$decision_grupo[en_grupo] <- sincronizado$decision_grupo[en_grupo]
  }
  plan
}

# Las columnas del plan que las decisiones modifican.
.COLUMNAS_PLAN_DECISIONES <- c("aplicar", "justificacion", "decision_grupo")

# Que filas del plan toco la capa de decisiones y que valor tenian antes (el del
# usuario, editado o no). Al reaplicar, solo esas filas vuelven a ese valor: las
# ediciones a mano del resto del plan se conservan, y aplicar de nuevo, o con
# otras decisiones, da lo mismo que aplicar una vez las ultimas.
.filas_tocadas_del_plan <- function(antes, despues) {
  distinto <- rep(FALSE, nrow(antes))
  for (campo in .COLUMNAS_PLAN_DECISIONES) {
    if (!campo %in% names(antes)) next
    a <- as.character(antes[[campo]])
    b <- as.character(despues[[campo]])
    distinto <- distinto | !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  }
  salida <- as.data.frame(antes[distinto, intersect(
    c("id_accion", .COLUMNAS_PLAN_DECISIONES), names(antes)
  ), drop = FALSE], stringsAsFactors = FALSE)
  rownames(salida) <- NULL
  salida
}

.restaurar_filas_tocadas <- function(plan, tocadas) {
  if (!is.data.frame(tocadas) || !nrow(tocadas) || !"id_accion" %in% names(plan)) {
    return(plan)
  }
  i <- match(as.character(tocadas$id_accion), as.character(plan$id_accion))
  for (campo in intersect(.COLUMNAS_PLAN_DECISIONES, names(tocadas))) {
    if (!campo %in% names(plan)) next
    destino <- i[!is.na(i)]
    valor <- tocadas[[campo]][!is.na(i)]
    plan[[campo]][destino] <- if (is.logical(plan[[campo]])) as.logical(valor) else {
      if (is.factor(plan[[campo]])) factor(valor, levels = levels(plan[[campo]])) else valor
    }
  }
  plan
}

# Lo que usa `planificar_limpieza()` cuando el perfil trae decisiones aplicadas.
.respetar_decisiones_en_plan <- function(plan, perfil) {
  if (is.null(.decisiones_adjuntas(perfil))) return(plan)
  base <- .base_hallazgos_decision(perfil, .decisiones_adjuntas(perfil)$nombre_tabla)
  .plan_desde_decisiones(plan, base$h, .estados_de_objeto(perfil), base)
}

# El resultado de `perfilar_por()` es una tabla; con decisiones aplicadas se
# imprime y se resume como un perfil: lo pendiente aparte de lo decidido. Sin
# decisiones, es la impresion y el resumen de siempre.
#' @export
print.hallazgos_por_grupo <- function(x, ...) {
  if (is.null(.decisiones_adjuntas(x))) return(NextMethod())
  NextMethod()
  .imprimir_decisiones(x)
  invisible(x)
}

#' @export
summary.hallazgos_por_grupo <- function(object, ...) {
  salida <- NextMethod()
  conteo <- .tabla_conteo_estados(object)
  if (!is.null(conteo)) attr(salida, "decisiones") <- conteo
  salida
}

# La seccion del informe para el resultado de `perfilar_por()` con decisiones.
.seccion_por_grupo_decisiones <- function(x, max_filas) {
  paste0(
    "<section><h2>Hallazgos por grupo</h2>",
    "<h3>Hallazgos</h3>", .html_tabla(hallazgos(x), max_filas),
    .seccion_decisiones_reporte(x, max_filas),
    "</section>"
  )
}

# Por que una coleccion no tiene hallazgos por tabla, o NULL si los tiene. Que
# no se hayan conservado los perfiles y que ninguna tabla se haya perfilado son
# dos cosas distintas, y cada una se dice como es.
.motivo_coleccion_sin_perfiles <- function(x) {
  if (!inherits(x, "perfil_coleccion") || length(x$perfiles)) return(NULL)
  if (isTRUE(x$meta$n_perfiladas > 0)) {
    paste0(
      "no conserv\u00f3 los perfiles de sus tablas, as\u00ed que no hay hallazgos ",
      "por tabla. Se conservan con perfilar_coleccion(..., ",
      "conservar_perfiles = TRUE)."
    )
  } else {
    "no tiene ninguna tabla perfilada, as\u00ed que no hay hallazgos por tabla."
  }
}
