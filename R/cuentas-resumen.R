# Las cuentas de cada cifra del resumen y si cierran.
#
# Un resumen publica, por metrica y objeto, cuantos elementos tenia su universo
# (`n_universo`), cuantos evaluo (`denominador`) y cuantos cumplieron
# (`numerador`). Esas cifras se leian sin preguntar si cerraban, y podian no
# cerrar: un metodo que da dos medidas por fila, uno que saltea celdas con
# valor, o una tabla que cambio entre dos lecturas de la misma corrida -medido:
# la misma tabla leida con 10 filas por una consulta y con 13 por la siguiente,
# publicada como una corrida-. Un color o un indice sobre esas cifras mentiria.
#
# El estado lo escribe quien produce la cifra, que es quien sabe como la leyo, y
# queda en columnas: las columnas cruzan `[` y `rbind()` sin una lista de
# atributos que mantener. `tablero_calidad()`, `indice_calidad()` y
# `semaforo_calidad()` leen la columna; no recalculan.

.COLUMNAS_CUENTAS_RESUMEN <- c(
  "n_sin_valor", "n_filas_leidas", "entidad_referida",
  "n_filas_leidas_referida", "estado_cuentas", "motivo_cuentas",
  "diferencia_cuentas"
)

.ESTADOS_CUENTAS <- c("cierran", "no_cierran", "parcial", "sin_cuentas")

# Que elementos declara evaluar una metrica del catalogo: los que tienen valor
# en sus columnas de valor (`excluye`), o tambien los vacios (`incluye`, como
# `NoNulo`, cuyo vacio es lo que mide). La declaracion vive en
# `.RUTAS_CATALOGO_METRICAS`; para una metrica escrita con `metrica()` no hay
# declaracion y el universo se infiere de sus cuentas.
.universo_metrica <- function(instancia) {
  nombre <- as.character(instancia$declaracion$nombre)[[1L]]
  if (!.metodo_del_paquete(instancia$metodo)) {
    return(list(vacios = NA_character_, unidad = NA_character_))
  }
  i <- match(nombre, .RUTAS_CATALOGO_METRICAS$metrica)
  if (is.na(i)) return(list(vacios = NA_character_, unidad = NA_character_))
  list(
    vacios = .RUTAS_CATALOGO_METRICAS$universo_vacios[[i]],
    unidad = .RUTAS_CATALOGO_METRICAS$universo_unidad[[i]]
  )
}

# Las columnas de las que sale el valor de una metrica. Los atributos y, en las
# metricas de una vigencia, su columna de actualizacion: `OportunidadEntPorFecha`
# no declara atributos y lee la fecha de la vigencia, y contar sus vacios solo
# sobre los atributos publicaba `sin_medida` para filas que estaban vacias.
.columnas_valor_instancia <- function(instancia) {
  columnas <- as.character(unlist(instancia$atributos))
  vigencia <- instancia$configuracion$vigencia
  if (is.list(vigencia) && length(vigencia$columna_actualizacion)) {
    columnas <- c(columnas, as.character(vigencia$columna_actualizacion))
  }
  unique(columnas[!is.na(columnas) & nzchar(columnas)])
}

# Si el universo de una instancia se cuenta por celda o por fila. Por la
# granularidad, salvo que el catalogo declare otra unidad: `CorrectitudSemDebil`
# es de atributo pero mide la fila del par clave-valor, y contar celdas daba un
# universo de 20 para 10 filas.
.unidad_universo_instancia <- function(instancia) {
  unidad <- .universo_metrica(instancia)$unidad
  if (!is.na(unidad)) return(unidad)
  gran <- as.character(instancia$declaracion$granularidad)[[1L]]
  if (identical(gran, "instanciaAtributo")) "celda" else "fila"
}

# Celdas o filas del universo sin valor en las columnas de valor. `NA` si la
# metrica no tiene columnas de valor (`EntidadDuplicada` sin atributos) o si
# alguna no esta en la tabla.
.sin_valor_universo <- function(tabla, columnas, dentro, unidad) {
  if (!length(columnas)) return(NA_real_)
  indices <- .indice_nombre(columnas, names(tabla))
  if (anyNA(indices)) return(NA_real_)
  celdas <- lapply(indices, function(i) is.na(tabla[[i]])[dentro])
  if (identical(unidad, "celda")) {
    as.double(sum(vapply(celdas, sum, numeric(1L))))
  } else {
    as.double(sum(Reduce(`|`, celdas)))
  }
}

# Lo que una instancia cuenta como sin valor: el `NA` de sus columnas de valor
# o, en una metrica que compara textos, el texto que su normalizacion deja
# vacio. Se pregunta con la misma funcion que usa la metrica, para que las dos
# cuentas no se aparten.
.sin_valor_instancia <- function(tabla, instancia, dentro, unidad) {
  if (identical(.universo_metrica(instancia)$vacios, "excluye_sin_texto") &&
      length(instancia$atributos) == 1L &&
      !is.na(.indice_nombre(instancia$atributos, names(tabla)))) {
    textos <- tryCatch(
      .texto_fila_aproximada(
        .seleccionar_columnas(tabla, instancia$atributos), instancia$atributos,
        instancia$configuracion$normalizar
      ),
      error = function(e) NULL
    )
    if (!is.null(textos)) return(as.double(sum(!textos$presentes[dentro])))
  }
  .sin_valor_universo(tabla, .columnas_valor_instancia(instancia), dentro, unidad)
}

# Una sola regla para lo no evaluado y su motivo. Estaba escrita cinco veces y
# solo dos guardaban el negativo: una fila que mezclaba dos lecturas publico
# `n_no_evaluados = -1`. Mas medidas que universo es `medidas_repetidas`.
.no_evaluados_resumen <- function(n_universo, denominador, n_sin_valor) {
  if (is.na(n_universo) || is.na(denominador)) {
    return(list(no = NA_real_, motivo = NA_character_))
  }
  no <- n_universo - denominador
  if (no < 0) return(list(no = NA_real_, motivo = "medidas_repetidas"))
  list(
    no = as.double(no),
    motivo = if (!no) {
      NA_character_
    } else if (!is.na(n_sin_valor) && n_sin_valor == no) {
      "sin_valor"
    } else {
      "sin_medida"
    }
  )
}

# El estado de una fila con lo que solo sabe quien la produjo: si las medidas
# caben en el universo (C3) y si lo no evaluado es lo que la metrica declara no
# evaluar (C4). `parcial` lo marca el productor cuando la cifra se calculo, por
# declaracion del procedimiento, sobre una parte: un vocabulario truncado.
.estado_cuentas_fila <- function(fila, instancia, parcial = NA_character_) {
  den <- fila$denominador[[1L]]
  univ <- fila$n_universo[[1L]]
  no <- fila$n_no_evaluados[[1L]]
  sv <- fila$n_sin_valor[[1L]]
  estado <- function(e, motivo = NA_character_, diferencia = NA_real_) {
    list(estado = e, motivo = motivo, diferencia = as.double(diferencia))
  }
  if (is.na(den) || is.na(univ)) return(estado("sin_cuentas", "sin_cuentas"))
  if (identical(fila$motivo_no_evaluados[[1L]], "medidas_repetidas") || is.na(no)) {
    return(estado("no_cierran", "medidas_repetidas", den - univ))
  }
  vacios <- .universo_metrica(instancia)$vacios
  sv0 <- if (is.na(sv)) 0 else sv
  admitido <- if (identical(vacios, "incluye")) {
    no == 0
  } else if (vacios %in% c("excluye", "excluye_sin_texto")) {
    no == sv0
  } else {
    no == 0 || no == sv0
  }
  if (!is.na(parcial)) {
    return(estado("parcial", parcial, if (admitido) 0 else no - sv0))
  }
  if (!admitido) {
    esperado <- if (identical(vacios, "incluye")) 0 else sv0
    return(estado("no_cierran", "no_evaluados_sin_explicar", no - esperado))
  }
  estado("cierran")
}

# Las columnas de cuentas de una fila recien construida, en su forma vacia.
.cuentas_vacias_fila <- function() {
  list(
    n_sin_valor = NA_real_, n_filas_leidas = NA_real_,
    entidad_referida = NA_character_, n_filas_leidas_referida = NA_real_,
    estado_cuentas = NA_character_,
    motivo_cuentas = NA_character_, diferencia_cuentas = NA_real_
  )
}

.lecturas_vacias <- function() {
  data.frame(
    id_medicion = character(), entidad = character(), n_filas = numeric(),
    instantanea = character(), control = character(), lectura = character(),
    tabla = character(), stringsAsFactors = FALSE
  )
}

# `lectura` identifica la sentencia: dos items de una misma consulta consolidada
# son una lectura, y dos consultas con el mismo conteo son dos. Sin el
# identificador, dos lecturas iguales se fundian en una y una tabla leida dos
# veces se publicaba como leida una vez.
#
# `tabla` es la tabla que resolvio el motor -su esquema, explicito aunque se
# haya omitido, y su nombre como lo guarda el catalogo-: dos rotulos de la misma
# tabla (`Sucursales` y `sucursales`, o `DBI::Id(schema = "main", ...)` y el
# nombre pelado) son una tabla, y sus lecturas se comparan. En memoria no hay
# motor que la resuelva y queda `NA`: cuenta el nombre de la entidad. Para un
# referencial en R es su identidad (`.identidad_referencial_memoria()`).
#
# `control` dice que puede afirmar la lectura: con la huella de los datos
# (`huella_de_datos`) o un contador del motor (`contador_del_motor`), dos
# lecturas con el mismo valor vieron la misma tabla; con el conteo de filas
# (`solo_filas_agregadas_o_quitadas`) solo se ve que se agregaron o quitaron
# filas: un `UPDATE` entre dos lecturas no cambia el conteo.
.lectura <- function(id_medicion, entidad, n_filas, instantanea = NA_character_,
                     control = "solo_filas_agregadas_o_quitadas",
                     lectura = NA_character_, tabla = NA_character_) {
  data.frame(
    id_medicion = as.character(id_medicion), entidad = as.character(entidad),
    n_filas = as.double(n_filas), instantanea = as.character(instantanea),
    control = as.character(control), lectura = as.character(lectura),
    tabla = as.character(tabla), stringsAsFactors = FALSE
  )
}

# Las lecturas de antes de `lectura` o de `tabla` no las traen: se completan.
.completar_lecturas <- function(l) {
  for (columna in c("lectura", "tabla")) {
    if (!columna %in% names(l)) l[[columna]] <- rep(NA_character_, nrow(l))
  }
  l[names(.lecturas_vacias())]
}

# Lo que identifica a cada lectura:
# - `t`, una tabla que resolvio el motor (`tabla`);
# - `r`, un referencial en R: su nombre y la huella de sus datos, que van
#   tambien en `tabla` y se reconocen por el control `huella_de_datos`;
# - `e`, solo el rotulo: una entidad en memoria, o una lectura del motor cuya
#   tabla no se resolvio (otro motor, o un resumen medido antes de ese campo).
.clase_lecturas <- function(lecturas) {
  ifelse(
    is.na(lecturas$tabla), "e",
    ifelse(lecturas$control %in% "huella_de_datos", "r", "t")
  )
}

# La clave de un rotulo -la corrida y el nombre de la entidad-, que es tambien
# la de una lectura sin otra identidad.
.clave_rotulo_lecturas <- function(id_medicion, entidad) {
  .clave_bytes(paste(id_medicion, paste0("e\035", entidad), sep = "\034"))
}

# La clave de cada lectura: la corrida y lo que la identifica. Las tres clases
# no se confunden aunque un nombre se escriba como una tabla.
.clave_tabla_lecturas <- function(lecturas) {
  tabla <- as.character(lecturas$tabla)
  .clave_bytes(paste(
    lecturas$id_medicion,
    paste0(.clase_lecturas(lecturas), "\035",
           ifelse(is.na(tabla), lecturas$entidad, tabla)),
    sep = "\034"
  ))
}

# La identidad de un referencial en R: su nombre y la huella de sus datos. Dos
# huellas bajo un nombre son dos referenciales, no uno que cambio -el nombre
# por omision de `referencial(<expresion>)` es siempre `referencial`-, y el
# resultado no depende de si van en una llamada o en dos partes.
.identidad_referencial_memoria <- function(nombre, huella) {
  paste0("referencial en R ", nombre, " [", huella, "]")
}

# Las lecturas de las partes de un `rbind()`, con el identificador de la parte:
# la misma lectura de dos partes son dos lecturas.
.lecturas_de_partes <- function(partes) {
  .unir_lecturas(lapply(seq_along(partes), function(k) {
    l <- attr(partes[[k]], "lecturas", exact = TRUE)
    if (!inherits(l, "data.frame") || !nrow(l)) return(NULL)
    l <- .completar_lecturas(l)
    # Una lectura sin identificador sigue sin el: con la parte delante, dos
    # lecturas de una tabla sin identificador se contaban como una sola y
    # sumaban `una_lectura`.
    l$lectura <- ifelse(is.na(l$lectura), NA_character_,
                        paste0("parte-", k, ":", l$lectura))
    l
  }))
}

.unir_lecturas <- function(tablas) {
  tablas <- Filter(function(t) inherits(t, "data.frame") && nrow(t), tablas)
  if (!length(tablas)) return(.lecturas_vacias())
  unidas <- do.call(rbind, lapply(unname(tablas), .completar_lecturas))
  unidas <- unidas[!duplicated(unidas), , drop = FALSE]
  rownames(unidas) <- NULL
  unidas
}

# Cuanto respaldan a una cifra las lecturas de una tabla, en tres palabras:
# `una_lectura` -se leyo en una sola sentencia, o es un objeto en memoria: no
# hay dos lecturas que puedan discrepar-; `completo` -varias lecturas, todas
# con la instantanea probada por el contador del motor o la huella de los
# datos-; `limitado` -varias lecturas y alguna sin instantanea: solo se comparo
# el numero de filas, que no ve un `UPDATE` que no lo cambia-.
.alcance_de_lecturas <- function(l) {
  n <- length(unique(l$lectura))
  if (!n) return("limitado")
  if (n == 1L && !anyNA(l$lectura)) return("una_lectura")
  if (!anyNA(l$instantanea) &&
      all(l$control %in% c("huella_de_datos", "contador_del_motor"))) {
    return("completo")
  }
  "limitado"
}

# Lo que las lecturas dicen de cada fila del resumen: si alguna de sus tablas
# cambio entre lecturas (`cambia`, con la `diferencia` de filas) y cuanto
# respalda el control a su cifra (`alcance`). Una sola regla para las dos
# cosas: el estado y el control leen lo mismo de las mismas lecturas.
#
# El rotulo de la fila -su entidad, o su tabla referida- solo dice donde buscar:
# las lecturas con ese rotulo dicen que se leyo.
# - Una sola tabla: sus lecturas y las filas que la leyeron se comparan por el
#   numero de filas y, donde hay, por la instantanea.
# - Varias tablas que el motor resolvio -otra base adjunta con el mismo alias
#   en otra parte-: son tablas distintas y no se comparan entre si. La fila no
#   dice cual leyo: el control es `desconocido`, y si alguna de ellas cambio
#   entre sus propias lecturas, la fila no cierra.
# - Lecturas sin identidad comparable -una parte en memoria y otra en el motor,
#   o una lectura del motor sin tabla resuelta junto a otra resuelta-: se
#   comparan ademas por el rotulo, como antes de que las lecturas trajeran su
#   tabla. Con otras filas u otra instantanea, la tabla cambio; con las mismas,
#   no se sabe si es la misma tabla y el control es `desconocido`.
# - Un referencial en R (solo en la tabla referida) es el de su nombre y su
#   huella, y sus filas son parte de esa identidad: entre varios con el mismo
#   nombre, la fila leyo el que tiene sus filas. No cambia entre lecturas.
.respaldo_lecturas_filas <- function(resumen, lecturas) {
  n_resumen <- nrow(resumen)
  columna <- function(nombre, vacio) {
    if (nombre %in% names(resumen)) resumen[[nombre]] else rep(vacio, n_resumen)
  }
  ids <- as.character(columna("id_medicion", NA_character_))
  hay <- nrow(lecturas) > 0L
  clase <- .clase_lecturas(lecturas)
  clave <- .clave_tabla_lecturas(lecturas)
  rotulo <- .clave_rotulo_lecturas(lecturas$id_medicion, lecturas$entidad)
  # Una lectura del motor sin tabla resuelta no prueba nada.
  sin_tabla <- unique(
    clave[clase == "e" & !(lecturas$control %in% "huella_de_datos")]
  )
  lado <- function(i, entidad, n, referida) {
    if (is.na(entidad)) return(NULL)
    propio <- .clave_rotulo_lecturas(ids[[i]], entidad)
    bajo <- rotulo == propio
    tablas <- unique(clave[bajo & clase != "r"])
    refs <- if (referida) unique(clave[bajo & clase == "r"]) else character()
    if (length(refs)) {
      con_filas <- unique(clave[
        bajo & clase == "r" & !is.na(lecturas$n_filas) & !is.na(n) &
          lecturas$n_filas == n
      ])
      # Ningun referencial con esas filas: la fila leyo la tabla.
      if (length(con_filas)) refs <- con_filas else if (length(tablas)) refs <- character()
    }
    # Sin lecturas con ese rotulo, cuenta el nombre.
    if (!length(tablas) && !length(refs)) tablas <- propio
    list(
      fila = i, n = n, rotulo = propio, tablas = tablas, refs = refs,
      mixto = length(tablas) > 1L && any(clase[match(tablas, clave)] == "e")
    )
  }
  lados <- c(
    lapply(seq_len(n_resumen), function(i) {
      lado(i, as.character(columna("entidad", NA_character_)[[i]]),
           as.double(columna("n_filas_leidas", NA_real_)[[i]]), FALSE)
    }),
    lapply(seq_len(n_resumen), function(i) {
      lado(i, as.character(columna("entidad_referida", NA_character_)[[i]]),
           as.double(columna("n_filas_leidas_referida", NA_real_)[[i]]), TRUE)
    })
  )
  lados <- Filter(Negate(is.null), lados)
  # El conteo de una fila va a la tabla que leyo, si se sabe cual es; si su
  # rotulo da lecturas sin identidad comparable, al rotulo; si no, a ninguna.
  a_tabla <- vapply(lados, function(l) {
    if (!length(l$refs) && length(l$tablas) == 1L) l$tablas else NA_character_
  }, character(1L))
  a_rotulo <- vapply(lados, function(l) {
    if (!length(l$refs) && l$mixto) l$rotulo else NA_character_
  }, character(1L))
  conteo <- vapply(lados, function(l) l$n, numeric(1L))
  comparar <- function(n, instantanea) {
    n <- n[!is.na(n)]
    fichas <- unique(instantanea[!is.na(instantanea)])
    list(
      cambia = length(unique(n)) > 1L || length(fichas) > 1L,
      diferencia = if (length(n)) max(n) - min(n) else NA_real_
    )
  }
  # Cada tabla y cada rotulo se miran una vez, no una por fila.
  vistas <- unique(unlist(lapply(lados, function(l) c(l$tablas, l$refs))))
  mixtos <- unique(unlist(lapply(lados, function(l) if (l$mixto) l$rotulo)))
  grupo_tabla <- lapply(vistas, function(k) {
    suyas <- clave == k
    comparar(c(conteo[a_tabla %in% k], lecturas$n_filas[suyas]),
             lecturas$instantanea[suyas])
  })
  grupo_rotulo <- lapply(mixtos, function(r) {
    suyas <- rotulo == r & clase != "r"
    comparar(c(conteo[a_rotulo %in% r], lecturas$n_filas[suyas]),
             lecturas$instantanea[suyas])
  })
  alcance_tabla <- vapply(vistas, function(k) {
    if (k %in% sin_tabla) return("desconocido")
    .alcance_de_lecturas(lecturas[clave == k, , drop = FALSE])
  }, character(1L), USE.NAMES = FALSE)
  de_tabla <- function(k) grupo_tabla[[match(k, vistas)]]
  de_rotulo <- function(r) grupo_rotulo[[match(r, mixtos)]]
  alcance_de_tabla <- function(k) alcance_tabla[[match(k, vistas)]]
  alcance_de_lado <- function(l) {
    if (!hay) return("limitado")
    if (length(l$refs) && length(l$tablas)) {
      # Un referencial en R con el nombre de una tabla de otra parte: la fila
      # no dice cual leyo. Si todas sus lecturas tienen la misma huella, son
      # los mismos datos y da igual.
      suyas <- lecturas[clave %in% c(l$refs, l$tablas), , drop = FALSE]
      if (anyNA(suyas$instantanea) || length(unique(suyas$instantanea)) != 1L) {
        return("desconocido")
      }
      return(.alcance_de_lecturas(suyas))
    }
    if (length(l$refs)) {
      # Varios referenciales con el mismo nombre y las mismas filas: se sabe
      # solo si a todos los respalda lo mismo.
      alcances <- unique(vapply(l$refs, alcance_de_tabla, character(1L)))
      return(if (length(alcances) == 1L) alcances else "desconocido")
    }
    if (length(l$tablas) > 1L) return("desconocido")
    alcance_de_tabla(l$tablas)
  }
  veredictos <- lapply(lados, function(l) {
    grupos <- c(
      lapply(l$tablas, de_tabla),
      if (l$mixto) list(de_rotulo(l$rotulo))
    )
    diferencias <- unlist(lapply(grupos, `[[`, "diferencia"))
    list(
      cambia = any(vapply(grupos, function(g) isTRUE(g$cambia), logical(1L))),
      diferencia = diferencias[!is.na(diferencias)],
      alcance = alcance_de_lado(l)
    )
  })
  de_fila <- vapply(lados, function(l) l$fila, integer(1L))
  salida <- data.frame(
    cambia = rep(FALSE, n_resumen), diferencia = rep(NA_real_, n_resumen),
    alcance = rep("limitado", n_resumen), stringsAsFactors = FALSE
  )
  for (i in seq_len(n_resumen)) {
    suyos <- veredictos[de_fila == i]
    if (!length(suyos)) next
    salida$cambia[[i]] <- any(vapply(suyos, `[[`, logical(1L), "cambia"))
    diferencias <- unlist(lapply(suyos, `[[`, "diferencia"))
    if (length(diferencias)) salida$diferencia[[i]] <- max(diferencias)
    alcances <- vapply(suyos, `[[`, character(1L), "alcance")
    # Una fila referencial toma el peor de sus dos tablas.
    salida$alcance[[i]] <- if ("desconocido" %in% alcances) {
      "desconocido"
    } else if ("limitado" %in% alcances) {
      "limitado"
    } else if (all(alcances == "una_lectura")) {
      "una_lectura"
    } else "completo"
  }
  salida
}

# C5: todas las lecturas de una tabla en una corrida vieron la misma tabla. Se
# compara el conteo de filas de cada lectura -la de cada fila del resumen, la
# de la tabla referida de una referencial y las que el productor registro
# aparte- y, donde hay, la instantanea (el contador del motor o la huella de
# los datos en memoria). El control que se publica dice cual se pudo usar.
# Monotono: una fila que ya no cerraba no pasa a cerrar.
.estado_cuentas_lecturas <- function(resumen, lecturas = NULL) {
  if (!nrow(resumen)) return(resumen)
  respaldo <- .respaldo_lecturas_filas(resumen, .unir_lecturas(list(lecturas)))
  for (i in which(respaldo$cambia)) {
    if (identical(resumen$estado_cuentas[[i]], "no_cierran")) next
    resumen$estado_cuentas[[i]] <- "no_cierran"
    resumen$motivo_cuentas[[i]] <- "tabla_cambio_entre_lecturas"
    resumen$diferencia_cuentas[[i]] <- respaldo$diferencia[[i]]
  }
  resumen
}

# Las columnas que identifican la lectura de una fila. Un resumen recortado
# (`x[, c("atributo", "estado_cuentas")]`) puede no tenerlas: el control no se
# puede leer y se declara `desconocido`. Imprimir no aborta por eso.
.COLUMNAS_CONTROL_LECTURAS <- c("id_medicion", "entidad", "entidad_referida")

.control_lecturas_legible <- function(resumen) {
  all(.COLUMNAS_CONTROL_LECTURAS %in% names(resumen))
}

# El control de lecturas que respalda cada fila (`.alcance_de_lecturas()`), o
# `desconocido` si no se puede saber que lecturas la respaldan
# (`.respaldo_lecturas_filas()`). Es un dato de la tabla y no de la fila, y
# vive en `attr(resumen, "lecturas")`: como columna, la misma cifra medida en
# memoria y en el motor dejaba de ser identica sin que cambiara nada medido.
.alcance_lecturas_filas <- function(resumen) {
  if (!nrow(resumen)) return(character())
  if (!.control_lecturas_legible(resumen)) return(rep("desconocido", nrow(resumen)))
  lecturas <- attr(resumen, "lecturas", exact = TRUE)
  hay <- inherits(lecturas, "data.frame") && nrow(lecturas) > 0L &&
    "lectura" %in% names(lecturas)
  lecturas <- if (hay) .completar_lecturas(lecturas) else .lecturas_vacias()
  .respaldo_lecturas_filas(resumen, lecturas)$alcance
}

# El aviso de quien produce las cifras: una vez, con cuantas no cierran y por
# que. Las parciales no avisan: su alcance ya lo declara la metrica.
.avisar_cuentas_resumen <- function(resumen, donde = "medir()") {
  if (!nrow(resumen) || !"estado_cuentas" %in% names(resumen)) {
    return(invisible(NULL))
  }
  malas <- !is.na(resumen$estado_cuentas) & resumen$estado_cuentas == "no_cierran"
  if (!any(malas)) return(invisible(NULL))
  motivos <- table(resumen$motivo_cuentas[malas])
  textos <- c(
    medidas_repetidas = "m\u00e1s medidas que elementos en su universo",
    no_evaluados_sin_explicar = "lo no evaluado no son los vac\u00edos",
    tabla_cambio_entre_lecturas = "la tabla cambi\u00f3 entre lecturas de la corrida"
  )
  partes <- paste0(
    unname(textos[names(motivos)]), " (", as.integer(motivos), ")"
  )
  .avisar(
    sum(malas),
    if (sum(malas) == 1L) " cifra cuyas cuentas no cierran: " else
      " cifras cuyas cuentas no cierran: ",
    paste(partes, collapse = "; "),
    ". No se pintan ni entran al \u00edndice; el detalle est\u00e1 en ",
    "`estado_cuentas` y `motivo_cuentas`.",
    call. = FALSE
  )
  invisible(NULL)
}

# Las filas cuyas cuentas no representan la tabla, en la forma en que las
# publican el tablero y el indice.
.cuentas_que_no_cierran <- function(resumen) {
  if (!inherits(resumen, "data.frame") || !nrow(resumen) ||
      !"estado_cuentas" %in% names(resumen)) {
    return(NULL)
  }
  malas <- !is.na(resumen$estado_cuentas) &
    resumen$estado_cuentas %in% c("no_cierran", "parcial")
  if (!any(malas)) return(NULL)
  # Con `[[` y una columna faltante como `NA`: en un resumen recortado `$`
  # tomaria por coincidencia parcial `entidad_referida` por `entidad`.
  columna <- function(nombre, vacio) {
    if (nombre %in% names(resumen)) resumen[[nombre]][malas] else rep(vacio, sum(malas))
  }
  salida <- data.frame(
    metrica_instanciada = columna("metrica_instanciada", NA_character_),
    entidad = columna("entidad", NA_character_),
    atributo = columna("atributo", NA_character_),
    estado_cuentas = resumen[["estado_cuentas"]][malas],
    motivo_cuentas = columna("motivo_cuentas", NA_character_),
    diferencia_cuentas = columna("diferencia_cuentas", NA_real_),
    control_lecturas = .alcance_lecturas_filas(resumen)[malas],
    stringsAsFactors = FALSE
  )
  rownames(salida) <- NULL
  salida
}

# Huella de los datos de una tabla en memoria: dice si dos mediciones leyeron
# la misma tabla, que el conteo de filas no puede decir. Columna por columna,
# para no serializar la tabla entera de una vez, y en formato 2 de `serialize()`
# para no depender de la codificacion nativa de quien mide.
.huella_tabla_memoria <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(NA_character_)
  tryCatch({
    piezas <- vapply(seq_along(tabla), function(i) {
      .md5_crudos(list(serialize(tabla[[i]], NULL, version = 2L)))
    }, character(1L))
    .md5_textos(paste(
      c(as.character(nrow(tabla)), .clave_bytes(names(tabla)), piezas),
      collapse = "\034"
    ))
  }, error = function(e) NA_character_)
}

# Las columnas de cuentas de las filas que una instancia produjo en memoria. Las
# filas leidas salen de la tabla tal como se recibio, no de la recortada por la
# aplicabilidad: con `aplicabilidad = list(x = ~ l)` la tabla de `x` tiene menos
# filas, y comparar ese numero con el de otra metrica de la misma tabla acusaba
# un cambio que no hubo.
.cuentas_filas_memoria <- function(filas, instancia, tablas,
                                   parcial = NA_character_, referida = NULL) {
  if (!inherits(filas, "data.frame") || !nrow(filas)) return(filas)
  n_tabla <- function(entidad) {
    if (is.na(entidad)) return(NA_real_)
    tabla <- tablas[[entidad]]
    if (inherits(tabla, "data.frame")) as.double(nrow(tabla)) else NA_real_
  }
  # El referencial en R es la tabla referida de la fila (`.referidas_memoria()`).
  del_referencial <- if (!is.null(referida)) {
    is.na(filas$entidad_referida)
  } else rep(FALSE, nrow(filas))
  filas$entidad_referida[del_referencial] <- referida$rotulo
  for (i in seq_len(nrow(filas))) {
    filas$n_filas_leidas[[i]] <- n_tabla(filas$entidad[[i]])
    filas$n_filas_leidas_referida[[i]] <- if (del_referencial[[i]]) {
      referida$n
    } else n_tabla(filas$entidad_referida[[i]])
    estado <- .estado_cuentas_fila(filas[i, , drop = FALSE], instancia, parcial)
    filas$estado_cuentas[[i]] <- estado$estado
    filas$motivo_cuentas[[i]] <- estado$motivo
    filas$diferencia_cuentas[[i]] <- estado$diferencia
  }
  filas
}

# Los referenciales en R de un modelo, como tablas referidas: su lectura se
# registra con la huella de sus datos. Se identifican por su nombre y su huella
# (`.identidad_referencial_memoria()`): dos referenciales distintos con el
# mismo nombre son dos, cada uno con su lectura, vayan en una llamada o en dos
# partes. La huella se toma siempre: sin ella dos referenciales del mismo
# tamano serian una lectura. El rotulo es el nombre, salvo que sea el de una
# entidad: entonces `referencial <nombre>`.
.referidas_memoria <- function(modelo, entidades) {
  salida <- list()
  for (instancia in modelo$metricas) {
    ref <- instancia$referencial
    if (!inherits(ref, "referencial") || .referencial_en_motor(ref) ||
        !inherits(ref$datos, "data.frame")) next
    huella <- .huella_tabla_memoria(ref$datos)
    if (is.na(huella)) {
      huella <- .md5_crudos(list(serialize(ref$datos, NULL, version = 2L)))
    }
    salida[[instancia$nombre]] <- list(
      nombre = ref$nombre, n = as.double(nrow(ref$datos)), huella = huella,
      identidad = .identidad_referencial_memoria(ref$nombre, huella)
    )
  }
  if (!length(salida)) return(salida)
  lapply(salida, function(r) {
    r$rotulo <- if (.clave_bytes(r$nombre) %in% .clave_bytes(entidades)) {
      paste0("referencial ", r$nombre)
    } else r$nombre
    r
  })
}

# Una lectura por tabla en memoria, con la huella de sus datos: dos partes de
# una corrida sobre la misma tabla se reconocen por la huella, no por el conteo.
.lecturas_memoria <- function(resumen, tablas, id_medicion, huella = TRUE,
                              referidas = list()) {
  entidades <- unique(c(resumen$entidad, resumen$entidad_referida))
  entidades <- entidades[!is.na(entidades)]
  partes <- lapply(entidades, function(e) {
    tabla <- tablas[[e]]
    if (!inherits(tabla, "data.frame")) return(NULL)
    huella <- if (isTRUE(huella)) .huella_tabla_memoria(tabla) else NA_character_
    .lectura(
      id_medicion, e, nrow(tabla),
      if (is.na(huella)) NA_character_ else paste0("huella:", huella),
      "huella_de_datos", lectura = "memoria"
    )
  })
  usadas <- .clave_bytes(resumen$entidad_referida)
  for (r in referidas) {
    if (!.clave_bytes(r$rotulo) %in% usadas) next
    partes[[length(partes) + 1L]] <- .lectura(
      id_medicion, r$rotulo, r$n,
      if (is.na(r$huella)) NA_character_ else paste0("huella:", r$huella),
      "huella_de_datos", lectura = "memoria", tabla = r$identidad
    )
  }
  .unir_lecturas(partes)
}

# La dimension y el factor de cada metrica sin medir, de su declaracion en el
# modelo: la misma de la que sale la columna `dimension` de una fila medida.
# Sin esto, el conteo por dimension de un semaforo no sabia donde poner una
# metrica que no se pudo medir.
.ubicar_cobertura_metricas <- function(cobertura, modelo) {
  if (!inherits(cobertura, "data.frame") ||
      all(c("dimension", "factor") %in% names(cobertura))) {
    return(cobertura)
  }
  nombres <- vapply(modelo$metricas, function(i) as.character(i$nombre)[[1L]],
                    character(1L))
  declarado <- function(campo) {
    vapply(modelo$metricas, function(i) {
      valor <- i$declaracion[[campo]]
      if (is.null(valor) || !length(valor)) NA_character_ else as.character(valor)[[1L]]
    }, character(1L))
  }
  indice <- .indice_identificador(as.character(cobertura$metrica_instanciada), nombres)
  cobertura$dimension <- if (nrow(cobertura)) declarado("dimension")[indice] else character()
  cobertura$factor <- if (nrow(cobertura)) declarado("factor")[indice] else character()
  cobertura
}

# Un `NaN` de una columna doble pasa a `NA`. Solo en dobles sin clase: un
# `integer64` con el patron de bits de un `NaN` es un entero.
.nan_como_vacio <- function(x) {
  if (is.double(x) && !is.object(x)) x[is.nan(x)] <- NA_real_
  x
}

# Las filas leidas y el estado de una fila del motor.
.cerrar_fila_dbi <- function(fila, instancia, n_filas, n_filas_ref = NA_real_,
                             parcial = NA_character_) {
  fila$n_filas_leidas <- suppressWarnings(as.numeric(n_filas))[1L]
  if (!is.na(fila$entidad_referida[[1L]])) {
    fila$n_filas_leidas_referida <- suppressWarnings(as.numeric(n_filas_ref))[1L]
  }
  estado <- .estado_cuentas_fila(fila, instancia, parcial)
  fila$estado_cuentas <- estado$estado
  fila$motivo_cuentas <- estado$motivo
  fila$diferencia_cuentas <- estado$diferencia
  fila
}

# Lo que el tablero de un resumen tiene que decir de sus cuentas y de su
# consolidacion. El tablero no cambia sus celdas: agrega `cuentas` -las celdas
# cuyas cifras no cierran o son parciales- solo si hay alguna, como
# `alcance_medidas`, para que el tablero de un resumen que cierra siga siendo el
# de la medicion completa. Y si el resumen es una consolidacion, su `fuente` y lo
# descartado: declarar algo en un objeto que el paso siguiente tira es, para
# quien lee, no declararlo.
.con_cuentas_y_consolidacion <- function(tablero, resumen) {
  if (!inherits(tablero, "data.frame") || !inherits(resumen, "data.frame")) {
    return(tablero)
  }
  if ("fuente" %in% names(resumen) && nrow(tablero) == nrow(resumen)) {
    tablero$fuente <- as.character(resumen$fuente)
  }
  malas <- .cuentas_que_no_cierran(resumen)
  if (!is.null(malas) && nrow(tablero) == nrow(resumen)) {
    i <- match(.clave_celda_cuentas(malas), .clave_celda_cuentas(resumen))
    malas <- cbind(
      data.frame(componente = tablero$componente[i], stringsAsFactors = FALSE),
      malas
    )
    attr(tablero, "cuentas") <- malas
  }
  for (a in c("descartadas", "consolidacion", "cobertura_resuelta")) {
    valor <- attr(resumen, a, exact = TRUE)
    if (!is.null(valor)) attr(tablero, a) <- valor
  }
  tablero
}

.clave_celda_cuentas <- function(x) {
  paste(
    .clave_bytes(x$metrica_instanciada), .clave_bytes(x$entidad),
    .clave_bytes(x$atributo), sep = "\034"
  )
}

# La impresion de lo mismo, en el tablero, el indice y el semaforo.
.imprimir_cuentas_y_consolidacion <- function(x) {
  cuentas <- attr(x, "cuentas", exact = TRUE)
  if (inherits(cuentas, "data.frame") && nrow(cuentas)) {
    cli::cli_h2("Cuentas que no cierran")
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(cuentas),
      if (nrow(cuentas) == 1L) " cifra no representa" else " cifras no representan",
      " la tabla: no se pintan ni entran al \u00edndice."
    )))
    .print_data_frame_bytes(cuentas, row.names = FALSE)
  }
  .imprimir_consolidacion(x)
}

.imprimir_consolidacion <- function(x) {
  consolidacion <- attr(x, "consolidacion", exact = TRUE)
  if (is.list(consolidacion)) {
    cli::cli_h2("Consolidaci\u00f3n")
    cli::cli_alert_info(.cli_literal(paste0(
      "Prioridad declarada: ", paste(consolidacion$prioridad, collapse = " > "), "."
    )))
    .print_data_frame_bytes(consolidacion$partes, row.names = FALSE)
    d <- attr(x, "descartadas", exact = TRUE)
    if (inherits(d, "data.frame") && nrow(d) && any(!d$coinciden)) {
      cli::cli_alert_warning(.cli_literal(paste0(
        sum(!d$coinciden),
        if (sum(!d$coinciden) == 1L) " celda en la que" else " celdas en las que",
        " las partes no coinciden: ",
        paste(unique(d$metrica_instanciada[!d$coinciden]), collapse = ", "), "."
      )))
    }
  }
  invisible(NULL)
}
