# Resumen de fechas (`resumen_fechas = TRUE`), fechas centinela declaradas para
# toda la tabla (`centinelas_fecha`) y periodos declarados (`periodos`).
#
# Memoria y motor publican los mismos campos con los mismos nombres: las cuentas
# las decide la regla de `dominios.R` en las dos vias (memoria sobre los valores,
# el motor sobre agregados, ver `.resumen_fechas_dbi()`), y
# `comparar_equivalencia()` los compara. Nada de esto se publica si no se pide:
# sin `resumen_fechas`, `centinelas_fecha` ni `periodos` el perfil es el de
# siempre.

# Los campos que se agregan a `columnas` -del perfil y de `resumen_tabla`- con
# `resumen_fechas = TRUE`, en este orden.
.CAMPOS_RESUMEN_FECHAS <- c(
  "n_fechas_centinela", "n_fechas_centinela_declaradas", "n_fechas_fin_abierto",
  "lectura_fechas_centinela", "n_fechas_futuras", "lectura_fechas_futuras",
  "ultimo_dato_fecha"
)

# Los que dependen de la fecha de la corrida: con los mismos datos cambian
# porque paso el tiempo, asi que solo se comparan entre dos perfiles con la
# misma `fecha` (ver `comparar_equivalencia()`).
.CAMPOS_RESUMEN_FECHAS_DE_LA_FECHA <- c(
  "n_fechas_futuras", "lectura_fechas_futuras", "ultimo_dato_fecha"
)

.normalizar_centinelas_fecha <- function(centinelas_fecha) {
  if (is.null(centinelas_fecha) || !length(centinelas_fecha)) return(character())
  if (!is.character(centinelas_fecha) &&
      !inherits(centinelas_fecha, c("Date", "POSIXt"))) {
    .detener(
      "`centinelas_fecha` debe contener fechas como texto, Date o POSIXt.",
      call. = FALSE
    )
  }
  texto <- .dominio_texto_centinelas(centinelas_fecha)
  if (anyNA(texto) || any(!nzchar(trimws(texto)))) {
    .detener(
      "`centinelas_fecha` no puede contener ausentes ni textos vac\u00edos.",
      call. = FALSE
    )
  }
  texto <- unique(texto)
  invisible(.dominio_fecha_numero(texto, "centinelas_fecha"))
  texto
}

.validar_resumen_fechas <- function(resumen_fechas) {
  if (!is.logical(resumen_fechas) || length(resumen_fechas) != 1L ||
      is.na(resumen_fechas)) {
    .detener("`resumen_fechas` debe ser TRUE o FALSE.", call. = FALSE)
  }
  resumen_fechas
}

# Agrega los campos del resumen a `columnas`, por nombre de columna: `NA` en las
# que no son fecha.
.agregar_resumen_fechas_columnas <- function(columnas, fechas) {
  if (!is.data.frame(columnas)) return(columnas)
  posicion <- if (is.data.frame(fechas) && nrow(fechas)) {
    match(.nombres_para_operar(as.character(columnas$columna)),
          .nombres_para_operar(as.character(fechas$columna)))
  } else rep(NA_integer_, nrow(columnas))
  for (campo in .CAMPOS_RESUMEN_FECHAS) {
    valores <- if (is.data.frame(fechas) && campo %in% names(fechas)) {
      fechas[[campo]][posicion]
    } else {
      rep(if (grepl("^n_", campo, useBytes = TRUE)) NA_real_ else NA_character_,
          nrow(columnas))
    }
    columnas[[campo]] <- valores
  }
  columnas
}

# La actualidad de la tabla, una fila por columna de fecha. No hay una fecha
# ganadora entre columnas distintas (un nacimiento y una actualizacion no
# miden lo mismo) ni un puntaje: cada columna dice su ultimo dato, cuanto hace
# de el y que se dejo afuera para calcularlo.
.actualidad_desde_resumen <- function(fechas, fecha_hora, fecha_declarada,
                                      fecha_es_date) {
  referencia <- suppressWarnings(as.numeric(fecha_hora))
  de_fecha <- if (is.data.frame(fechas) && nrow(fechas)) {
    fechas[!is.na(fechas$clase_fecha), , drop = FALSE]
  } else NULL
  columnas <- if (is.null(de_fecha) || !nrow(de_fecha)) {
    data.frame(
      columna = character(), clase = character(),
      ultimo_dato_fecha = character(), antiguedad_dias = numeric(),
      n_evaluadas = numeric(), n_futuras = numeric(),
      n_centinelas_declaradas_no_futuras = numeric(),
      admite_futuras = logical(), estado = character(), motivo = character(),
      stringsAsFactors = FALSE
    )
  } else {
    n_valores <- de_fecha$n_evaluadas_actualidad + de_fecha$n_fechas_futuras +
      de_fecha$n_centinelas_declaradas_no_futuras
    estado <- ifelse(
      is.na(de_fecha$n_evaluadas_actualidad), "no_disponible",
      ifelse(n_valores == 0, "sin_valores",
             ifelse(de_fecha$n_evaluadas_actualidad == 0,
                    "sin_valores_utilizables", "calculado"))
    )
    motivo <- rep(NA_character_, nrow(de_fecha))
    motivo[estado == "sin_valores_utilizables"] <- paste(
      "Todas las fechas presentes son posteriores a la fecha del perfil o",
      "centinelas declaradas."
    )
    motivo[de_fecha$ultimo_es_incorporada %in% TRUE] <- paste(
      "El \u00faltimo dato es una fecha que suele ser centinela (la lista",
      "incorporada) y se repite aislada: no se excluye porque no se declar\u00f3;",
      "declararla en `centinelas_fecha` la saca."
    )
    if ("motivo_motor" %in% names(de_fecha)) {
      hay <- !is.na(de_fecha$motivo_motor)
      motivo[hay] <- de_fecha$motivo_motor[hay]
    }
    data.frame(
      columna = de_fecha$columna, clase = de_fecha$clase_fecha,
      ultimo_dato_fecha = de_fecha$ultimo_dato_fecha,
      antiguedad_dias = (referencia - de_fecha$ultimo_dato_seg) / 86400,
      n_evaluadas = de_fecha$n_evaluadas_actualidad,
      n_futuras = de_fecha$n_fechas_futuras,
      n_centinelas_declaradas_no_futuras =
        de_fecha$n_centinelas_declaradas_no_futuras,
      admite_futuras = de_fecha$admite_futuras,
      estado = estado, motivo = motivo, stringsAsFactors = FALSE
    )
  }
  rownames(columnas) <- NULL
  list(
    fecha_referencia = fecha_hora,
    fecha_declarada = isTRUE(fecha_declarada),
    fecha_es_date = isTRUE(fecha_es_date),
    columnas = columnas
  )
}

# ---- Periodos ---------------------------------------------------------------
#
# Un periodo es un par de columnas de fecha que el usuario declara (inicio,
# fin). Nunca se adivina por el nombre. Un fin abierto es un fin ausente o una
# fecha declarada en `fin_abierto`; una fecha centinela conocida en el fin que
# no se declaro no es un periodo cerrado valido ni una inversion: es un posible
# fin abierto sin declarar, y se cuenta aparte con un aviso.

.normalizar_periodos <- function(periodos, nombres) {
  if (is.null(periodos)) return(NULL)
  uno <- function(p, etiqueta) {
    if (is.character(p) && !is.null(names(p))) p <- as.list(p)
    if (!is.list(p) || is.null(names(p)) ||
        !all(c("inicio", "fin") %in% names(p))) {
      .detener(
        "Cada periodo de `periodos` necesita `inicio` y `fin` (nombres de columnas).",
        call. = FALSE
      )
    }
    desconocidos <- setdiff(names(p), c("inicio", "fin", "fin_abierto"))
    if (length(desconocidos)) {
      .detener(
        "`periodos` tiene campos desconocidos: ",
        paste(desconocidos, collapse = ", "), ".", call. = FALSE
      )
    }
    for (campo in c("inicio", "fin")) {
      if (!.es_texto_escalar(p[[campo]])) {
        .detener("`", campo, "` de `periodos` debe ser un nombre de columna.",
                 call. = FALSE)
      }
    }
    indices <- .indice_nombre(c(p$inicio, p$fin), nombres)
    if (anyNA(indices)) {
      .detener(
        "`periodos` nombra columnas inexistentes: ",
        paste(c(p$inicio, p$fin)[is.na(indices)], collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (indices[[1L]] == indices[[2L]]) {
      .detener("`inicio` y `fin` de un periodo deben ser columnas distintas.",
               call. = FALSE)
    }
    fin_abierto <- if (is.null(p$fin_abierto)) character() else
      .normalizar_centinelas_fecha(p$fin_abierto)
    list(periodo = etiqueta, inicio = nombres[[indices[[1L]]]],
         fin = nombres[[indices[[2L]]]], fin_abierto = fin_abierto)
  }
  simple <- (is.character(periodos) && !is.null(names(periodos)) &&
               all(c("inicio", "fin") %in% names(periodos))) ||
    (is.list(periodos) && !is.null(names(periodos)) &&
       all(c("inicio", "fin") %in% names(periodos)) &&
       !is.list(periodos$inicio))
  if (simple) return(list(uno(periodos, "periodo")))
  if (!is.list(periodos) || !length(periodos) || is.null(names(periodos)) ||
      any(!nzchar(names(periodos)))) {
    .detener(
      "`periodos` debe ser `c(inicio = ..., fin = ...)`, una lista con esos ",
      "campos o una lista nombrada de periodos.", call. = FALSE
    )
  }
  lapply(seq_along(periodos), function(k) uno(periodos[[k]], names(periodos)[[k]]))
}

# Las fechas declaradas como centinela en la regla de una columna (por
# `centinelas_fecha` o por `dominios`), en segundos.
.centinelas_declaradas_regla <- function(regla, columna) {
  if (is.null(regla) || isFALSE(regla) || !length(regla$centinelas)) return(numeric())
  marcas <- if (identical(regla$fuente, "declarado")) {
    rep(TRUE, length(regla$centinelas))
  } else {
    rep_len(regla$centinelas_declaradas %||% FALSE, length(regla$centinelas))
  }
  if (!any(marcas)) return(numeric())
  .dominio_fecha_numero(regla$centinelas[marcas], columna)
}

# La particion de las filas de un periodo, en segundos. La usan memoria y el
# motor por bloques. Cada fila cae en exactamente una categoria.
.particion_periodo <- function(inicio, fin, declaradas_inicio, declaradas_fin,
                               marcas_abierto, conocidas) {
  inicio_valido <- !is.na(inicio) & !(inicio %in% declaradas_inicio)
  fin_nulo <- is.na(fin)
  fin_marca <- !fin_nulo & fin %in% marcas_abierto
  fin_declarado <- !fin_nulo & !fin_marca & fin %in% declaradas_fin
  fin_conocido <- !fin_nulo & !fin_marca & !fin_declarado & fin %in% conocidas
  fin_valido <- !fin_nulo & !fin_marca & !fin_declarado & !fin_conocido
  # Cuentas como dobles, como las demas del resumen de fechas: el motor las
  # trae asi, y las dos vias publican el mismo objeto.
  cuenta <- function(x) as.numeric(sum(x))
  list(
    n = as.numeric(length(inicio)),
    n_cerrados_validos = cuenta(inicio_valido & fin_valido & inicio <= fin),
    n_fin_antes_inicio = cuenta(inicio_valido & fin_valido & fin < inicio),
    n_fin_abierto_nulo = cuenta(inicio_valido & fin_nulo),
    n_fin_abierto_centinela = cuenta(inicio_valido & fin_marca),
    n_fin_centinela_declarado = cuenta(inicio_valido & fin_declarado),
    n_fin_centinela_sin_declarar = cuenta(inicio_valido & fin_conocido),
    n_sin_inicio_con_fin = cuenta(!inicio_valido & fin_valido),
    n_ambos_ausentes_o_centinela = cuenta(!inicio_valido & !fin_valido),
    conocidas_observadas = .sort_por_bytes(unique(fin[inicio_valido & fin_conocido]))
  )
}

# La fila publicada de un periodo, desde la particion: con su proporcion de fin
# abierto y el aviso cuando el fin trae una centinela conocida sin declarar.
.fila_periodo <- function(p, particion, estado = "calculado", motivo = NA_character_) {
  cuentas <- particion[setdiff(names(particion), "conocidas_observadas")]
  base <- cuentas$n_cerrados_validos + cuentas$n_fin_antes_inicio +
    cuentas$n_fin_abierto_nulo + cuentas$n_fin_abierto_centinela +
    cuentas$n_fin_centinela_declarado + cuentas$n_fin_centinela_sin_declarar
  abiertos <- cuentas$n_fin_abierto_nulo + cuentas$n_fin_abierto_centinela
  aviso <- NA_character_
  if (isTRUE(cuentas$n_fin_centinela_sin_declarar > 0)) {
    fechas <- particion$conocidas_observadas
    aviso <- paste0(
      "Posible fin abierto sin declarar: ", cuentas$n_fin_centinela_sin_declarar,
      if (cuentas$n_fin_centinela_sin_declarar == 1L) " fila tiene" else " filas tienen",
      " como fin una fecha que suele ser centinela (",
      if (length(fechas)) paste(fechas, collapse = ", ") else "sin detalle",
      "). No se cuentan como periodos cerrados ni como inversiones; declararla ",
      "en `fin_abierto` (vigente) o en `centinelas_fecha` (sin dato) lo hace expl\u00edcito."
    )
  }
  if (isTRUE(abiertos > 0) && isTRUE(cuentas$n_fin_abierto_nulo > 0) &&
      isTRUE(cuentas$n_fin_abierto_centinela > 0)) {
    mixto <- paste(
      "El fin abierto mezcla dos convenciones: fines ausentes y fines con la",
      "fecha declarada en `fin_abierto`."
    )
    aviso <- if (is.na(aviso)) mixto else paste(aviso, mixto)
  }
  data.frame(
    periodo = p$periodo, inicio = p$inicio, fin = p$fin,
    as.data.frame(cuentas, stringsAsFactors = FALSE),
    prop_fin_abierto = if (base > 0) abiertos / base else NA_real_,
    estado = estado, motivo = motivo, aviso = aviso,
    stringsAsFactors = FALSE
  )
}

.evaluar_periodos <- function(datos, nombres, resultados, periodos, reglas,
                              aplicables) {
  if (is.null(periodos)) return(list(tabla = NULL, hallazgos = list()))
  conocidas_base <- .dominio_fecha_numero(.CENTINELAS_FECHA_DOMINIO, "periodos")
  filas <- lapply(periodos, function(p) {
    i <- .indice_nombre(p$inicio, nombres)
    j <- .indice_nombre(p$fin, nombres)
    tipos <- vapply(c(i, j), function(k) {
      as.character(resultados[[k]]$inferencia$tipo)[[1L]]
    }, character(1L))
    if (!all(tipos %in% c("fecha", "fecha-hora"))) {
      .detener(
        "Las columnas de un periodo deben ser fechas; `", p$inicio, "` es `",
        tipos[[1L]], "` y `", p$fin, "` es `", tipos[[2L]], "`.", call. = FALSE
      )
    }
    universo <- .dominio_universo(datos, resultados, aplicables, i) &
      .dominio_universo(datos, resultados, aplicables, j)
    vi <- .dominio_valores(datos[[i]], resultados[[i]], universo)
    vf <- .dominio_valores(datos[[j]], resultados[[j]], universo)
    filas_universo <- which(universo)
    inicio <- vi$valores[filas_universo]
    fin <- vf$valores[filas_universo]
    inicio[!is.finite(inicio)] <- NA_real_
    fin[!is.finite(fin)] <- NA_real_
    regla_inicio <- if (length(reglas) >= i) reglas[[i]] else NULL
    regla_fin <- if (length(reglas) >= j) reglas[[j]] else NULL
    declaradas_fin <- .centinelas_declaradas_regla(regla_fin, p$fin)
    marcas <- if (length(p$fin_abierto)) .dominio_fecha_numero(p$fin_abierto, p$fin) else numeric()
    particion <- .particion_periodo(
      inicio, fin, .centinelas_declaradas_regla(regla_inicio, p$inicio),
      .centinelas_declaradas_regla(regla_fin, p$fin), marcas,
      setdiff(conocidas_base, c(declaradas_fin, marcas))
    )
    particion$conocidas_observadas <- vapply(
      particion$conocidas_observadas, .fecha_resumida, character(1L), clase = "fecha"
    )
    .fila_periodo(p, particion)
  })
  tabla <- .apilar_filas(filas)
  rownames(tabla) <- NULL
  list(tabla = tabla, hallazgos = list())
}
