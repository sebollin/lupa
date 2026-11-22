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

# Lo que cada control de lecturas puede afirmar. Con un contador del motor, dos
# lecturas con el mismo valor vieron la misma tabla; con el conteo de filas
# solo se ve que se agregaron o quitaron filas: un `UPDATE` entre dos lecturas
# no cambia el conteo.
.CONTROLES_LECTURAS <- c(
  "huella_de_datos", "contador_del_motor", "solo_filas_agregadas_o_quitadas"
)

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
    stringsAsFactors = FALSE
  )
}

# `lectura` identifica la sentencia: dos items de una misma consulta consolidada
# son una lectura, y dos consultas con el mismo conteo son dos. Sin el
# identificador, dos lecturas iguales se fundian en una y una tabla leida dos
# veces se publicaba como leida una vez.
.lectura <- function(id_medicion, entidad, n_filas, instantanea = NA_character_,
                     control = "solo_filas_agregadas_o_quitadas",
                     lectura = NA_character_) {
  data.frame(
    id_medicion = as.character(id_medicion), entidad = as.character(entidad),
    n_filas = as.double(n_filas), instantanea = as.character(instantanea),
    control = as.character(control), lectura = as.character(lectura),
    stringsAsFactors = FALSE
  )
}

# Las lecturas de las partes de un `rbind()`, con el identificador de la parte:
# la misma lectura de dos partes son dos lecturas.
.lecturas_de_partes <- function(partes) {
  .unir_lecturas(lapply(seq_along(partes), function(k) {
    l <- attr(partes[[k]], "lecturas", exact = TRUE)
    if (!inherits(l, "data.frame") || !nrow(l)) return(NULL)
    if (!"lectura" %in% names(l)) l$lectura <- NA_character_
    l$lectura <- paste0("parte-", k, ":", ifelse(is.na(l$lectura), "", l$lectura))
    l
  }))
}

.unir_lecturas <- function(tablas) {
  tablas <- Filter(function(t) inherits(t, "data.frame") && nrow(t), tablas)
  if (!length(tablas)) return(.lecturas_vacias())
  unidas <- do.call(rbind, unname(tablas))
  unidas <- unidas[!duplicated(unidas), , drop = FALSE]
  rownames(unidas) <- NULL
  unidas
}

# C5: todas las lecturas de una tabla en una corrida vieron la misma tabla. Se
# compara el conteo de filas de cada lectura -la de cada fila del resumen, la
# de la tabla referida de una referencial y las que el productor registro
# aparte- y, donde hay, la instantanea (el contador del motor o la huella de
# los datos en memoria). El control que se publica dice cual se pudo usar.
# Monotono: una fila que ya no cerraba no pasa a cerrar.
.estado_cuentas_lecturas <- function(resumen, lecturas = NULL) {
  if (!nrow(resumen)) return(resumen)
  lecturas <- .unir_lecturas(list(lecturas))
  ids <- as.character(resumen$id_medicion)
  claves <- function(id, entidad) .clave_bytes(paste(id, entidad, sep = "\034"))
  filas_ent <- data.frame(
    clave = c(claves(ids, resumen$entidad),
              claves(ids, resumen$entidad_referida)),
    n = c(resumen$n_filas_leidas, resumen$n_filas_leidas_referida),
    stringsAsFactors = FALSE
  )
  filas_ent <- filas_ent[!is.na(c(resumen$entidad, resumen$entidad_referida)), ,
                         drop = FALSE]
  lect <- data.frame(
    clave = claves(lecturas$id_medicion, lecturas$entidad),
    n = lecturas$n_filas, instantanea = lecturas$instantanea,
    control = lecturas$control, stringsAsFactors = FALSE
  )
  todas <- unique(c(filas_ent$clave, lect$clave))
  info <- lapply(todas, function(k) {
    n <- c(filas_ent$n[filas_ent$clave == k], lect$n[lect$clave == k])
    n <- n[!is.na(n)]
    l <- lect[lect$clave == k, , drop = FALSE]
    tokens <- unique(l$instantanea[!is.na(l$instantanea)])
    list(
      cambia = length(unique(n)) > 1L || length(tokens) > 1L,
      diferencia = if (length(n)) max(n) - min(n) else NA_real_
    )
  })
  names(info) <- todas
  clave_e <- claves(ids, resumen$entidad)
  clave_r <- claves(ids, resumen$entidad_referida)
  for (i in seq_len(nrow(resumen))) {
    e <- info[[clave_e[[i]]]]
    r <- if (!is.na(resumen$entidad_referida[[i]])) info[[clave_r[[i]]]] else NULL
    cambia <- isTRUE(e$cambia) || isTRUE(r$cambia)
    if (cambia && !identical(resumen$estado_cuentas[[i]], "no_cierran")) {
      resumen$estado_cuentas[[i]] <- "no_cierran"
      resumen$motivo_cuentas[[i]] <- "tabla_cambio_entre_lecturas"
      diferencias <- c(e$diferencia, r$diferencia)
      diferencias <- diferencias[!is.na(diferencias)]
      resumen$diferencia_cuentas[[i]] <- if (length(diferencias)) {
        max(diferencias)
      } else NA_real_
    }
  }
  resumen
}

# El control de lecturas que respalda cada fila: el mas debil entre el de su
# tabla y el de su tabla referida. Es un dato de la tabla y no de la fila, y
# vive en `attr(resumen, "lecturas")`: como columna, la misma cifra medida en
# memoria y en el motor dejaba de ser identica sin que cambiara nada medido.
.control_lecturas_filas <- function(resumen) {
  if (!nrow(resumen)) return(character())
  lecturas <- attr(resumen, "lecturas", exact = TRUE)
  orden <- .CONTROLES_LECTURAS
  control_de <- function(id, entidad) {
    if (is.na(entidad)) return(NA_character_)
    if (!inherits(lecturas, "data.frame") || !nrow(lecturas)) {
      return("solo_filas_agregadas_o_quitadas")
    }
    l <- lecturas[.clave_bytes(lecturas$id_medicion) == .clave_bytes(id) &
                    .clave_bytes(lecturas$entidad) == .clave_bytes(entidad), ,
                  drop = FALSE]
    if (nrow(l) && !anyNA(l$instantanea) && length(unique(l$control)) == 1L &&
        l$control[[1L]] %in% orden[1:2]) {
      l$control[[1L]]
    } else {
      "solo_filas_agregadas_o_quitadas"
    }
  }
  vapply(seq_len(nrow(resumen)), function(i) {
    a <- control_de(resumen$id_medicion[[i]], resumen$entidad[[i]])
    b <- control_de(resumen$id_medicion[[i]], resumen$entidad_referida[[i]])
    controles <- c(a, b)
    controles <- controles[!is.na(controles)]
    if (!length(controles)) return("solo_filas_agregadas_o_quitadas")
    orden[[max(match(controles, orden))]]
  }, character(1L))
}

# Cuanto respalda el control de lecturas a cada fila, en tres palabras:
# `una_lectura` -la tabla se leyo en una sola sentencia, o es un objeto en
# memoria: no hay dos lecturas que puedan discrepar-; `completo` -varias
# lecturas, todas con la instantanea probada por el contador del motor o la
# huella de los datos-; `limitado` -varias lecturas y alguna sin instantanea:
# solo se comparo el numero de filas, que no ve un `UPDATE` que no lo cambia-.
# Una fila referencial toma el peor de sus dos tablas.
.alcance_lecturas_filas <- function(resumen) {
  if (!nrow(resumen)) return(character())
  lecturas <- attr(resumen, "lecturas", exact = TRUE)
  mecanismo <- .control_lecturas_filas(resumen)
  alcance_de <- function(id, entidad) {
    if (is.na(entidad)) return(NA_character_)
    if (!inherits(lecturas, "data.frame") || !nrow(lecturas) ||
        !"lectura" %in% names(lecturas)) {
      return("limitado")
    }
    l <- lecturas[.clave_bytes(lecturas$id_medicion) == .clave_bytes(id) &
                    .clave_bytes(lecturas$entidad) == .clave_bytes(entidad), ,
                  drop = FALSE]
    n <- length(unique(l$lectura))
    if (!n) return("limitado")
    if (n == 1L && !anyNA(l$lectura)) return("una_lectura")
    if (!anyNA(l$instantanea) &&
        all(l$control %in% c("huella_de_datos", "contador_del_motor"))) {
      return("completo")
    }
    "limitado"
  }
  vapply(seq_len(nrow(resumen)), function(i) {
    a <- alcance_de(resumen$id_medicion[[i]], resumen$entidad[[i]])
    b <- alcance_de(resumen$id_medicion[[i]], resumen$entidad_referida[[i]])
    alcances <- c(a, b)
    alcances <- alcances[!is.na(alcances)]
    if (!length(alcances) || "limitado" %in% alcances) return("limitado")
    if (all(alcances == "una_lectura")) return("una_lectura")
    "completo"
  }, character(1L))
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
  salida <- data.frame(
    metrica_instanciada = resumen$metrica_instanciada[malas],
    entidad = resumen$entidad[malas],
    atributo = resumen$atributo[malas],
    estado_cuentas = resumen$estado_cuentas[malas],
    motivo_cuentas = resumen$motivo_cuentas[malas],
    diferencia_cuentas = resumen$diferencia_cuentas[malas],
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
                                   parcial = NA_character_) {
  if (!inherits(filas, "data.frame") || !nrow(filas)) return(filas)
  n_tabla <- function(entidad) {
    if (is.na(entidad)) return(NA_real_)
    tabla <- tablas[[entidad]]
    if (inherits(tabla, "data.frame")) as.double(nrow(tabla)) else NA_real_
  }
  for (i in seq_len(nrow(filas))) {
    filas$n_filas_leidas[[i]] <- n_tabla(filas$entidad[[i]])
    filas$n_filas_leidas_referida[[i]] <- n_tabla(filas$entidad_referida[[i]])
    estado <- .estado_cuentas_fila(filas[i, , drop = FALSE], instancia, parcial)
    filas$estado_cuentas[[i]] <- estado$estado
    filas$motivo_cuentas[[i]] <- estado$motivo
    filas$diferencia_cuentas[[i]] <- estado$diferencia
  }
  filas
}

# Una lectura por tabla en memoria, con la huella de sus datos: dos partes de
# una corrida sobre la misma tabla se reconocen por la huella, no por el conteo.
.lecturas_memoria <- function(resumen, tablas, id_medicion, huella = TRUE) {
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
