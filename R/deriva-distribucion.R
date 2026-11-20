# Deriva de la distribucion entre dos entregas del mismo perfil.
#
# `comparar_perfiles()` comparaba extremos, faltantes y vocabulario, pero no la
# forma de la distribucion: una media corrida 25 con desvio 15 salia solo como
# "rango modificado", y un reparto 50/50 que pasa a 80/20 no salia. Aca se mide
# con lo que el perfil ya guarda (media, desvio, n y las frecuencias del
# vocabulario), con una medida de TAMANO por clase de columna y una puerta de
# ruido: el tamano solo acusa ruido de muestreo con n chico, y el p-valor solo
# acusa trivialidades con n grande, asi que se piden las dos cosas.

# z = qnorm(0.9995): p = 0,001 bilateral. Una tabla ancha compara cientos de
# columnas; con 0,05 el azar acusaria algunas en cada comparacion.
.Z_RUIDO_DERIVA <- 3.2905267314919255

# Convenciones de cada medida. Diferencia de medias estandarizada: 0,2 y 0,8 son
# el efecto chico y el grande de Cohen (1988). Razon de desvios: 1,5 y 2.
# PSI: 0,1 y 0,25, los cortes de uso corriente (Siddiqi, 2006).
.UMBRALES_DERIVA_MEDIA <- c(sospechoso = 0.2, error = 0.8)
.UMBRALES_DERIVA_DISPERSION <- c(sospechoso = 1.5, error = 2)
.UMBRALES_DERIVA_PSI <- c(sospechoso = 0.1, error = 0.25)

# El ruido de un desvio muestral depende de la curtosis: el error tipico de
# ln(s) es sqrt((k - 1) / (4 n)), con k = 3 en una normal y mucho mas en una
# variable asimetrica o con valores extremos. El perfil no guarda la curtosis,
# pero guarda con que acotarla: como (x - m)^4 <= max|x - m|^2 (x - m)^2, la
# curtosis muestral no pasa de (max|x - m| / s)^2. Es una cota, no una
# estimacion: con ella nunca se subestima el ruido (una columna casi constante
# con unos pocos valores lejanos, o una cola pesada, acusaban deriva de
# dispersion entre dos muestras de la misma poblacion).
.curtosis_acotada_deriva <- function(fila, media, desvio) {
  extremos <- c(
    .valor_columna_deriva(fila, "minimo"), .valor_columna_deriva(fila, "maximo")
  )
  if (!all(is.finite(extremos))) return(NA_real_)
  max(3, (max(abs(extremos - media)) / desvio)^2)
}

# La dispersion se compara con al menos 50 valores por lado (el error tipico de
# un desvio de pocos datos no es normal), y sin que un solo valor explique mas
# de esta parte de la suma de cuadrados.
.N_MINIMO_DISPERSION_DERIVA <- 50
.CONCENTRACION_MAXIMA_DERIVA <- 0.25

# Los hallazgos cuyo conteo no se compara entre entregas. `faltantes`,
# `faltantes_disfrazados` y `alta_cardinalidad` ya tienen su aspecto propio:
# contar de nuevo sus filas duplicaria la alarma. `outliers` no cuenta una
# propiedad fija de las filas sino lo que cae fuera de los limites de Tukey de
# cada entrega, y esos limites se mueven con los cuartiles de los datos: en una
# columna con mucha masa en un valor (70 % de ceros) la misma poblacion da entre
# 14 y 22 % de atipicos de una entrega a otra, sin cambio alguno. Un cambio de la
# distribucion lo dicen la media, la dispersion y el rango.
.TIPOS_HALLAZGO_SIN_CONTEO_DERIVA <- c(
  "faltantes", "faltantes_disfrazados", "alta_cardinalidad", "outliers",
  "espacios_sobrantes", "mayusculas_inconsistentes", "normalizacion_unicode",
  # Cuentan coincidencias entre filas: filas repetidas, filas en colision de una
  # casi clave y la frecuencia de la moda. Con la misma distribucion, mas filas
  # son mas coincidencias, asi que la proporcion crece con el tamano de la
  # tabla. Los demas hallazgos de fila cuentan una propiedad de cada fila.
  "filas_duplicadas", "casi_clave", "valor_concentrado"
)

# Hallazgos cuyo conteo depende de la fecha de la corrida: `fecha_futura` cuenta
# contra ella, y con los mismos datos cambia porque paso el tiempo.
.TIPOS_HALLAZGO_DE_LA_FECHA_DERIVA <- "fecha_futura"

# `outliers` no cuenta una propiedad fija de las filas, y los tipos de forma
# -espacios, mayusculas, unicode- cuentan como estan escritos los valores: lo
# mismo que `deriva_categorias` no toma como deriva.

# Un conteo no se compara si cambio el universo sobre el que se cuenta: los
# faltantes de la columna (con menos ausentes hay mas valores para repetirse o
# salir de dominio) o, en un hallazgo de la tabla, los de cualquier columna; ni
# si cambio la resolucion de una columna numerica, que cambia los repetidos.
.universo_del_conteo_cambio_deriva <- function(columna, mapa_a, mapa_b,
                                               resolucion, faltantes, tipo) {
  if (is.na(columna)) {
    return(length(faltantes) > 0L ||
             (identical(tipo, "filas_duplicadas") && length(resolucion) > 0L))
  }
  clave <- mapa_b$clave[match(.nombres_para_operar(columna),
                              .nombres_para_operar(mapa_b$original))]
  isTRUE(clave %in% faltantes) || isTRUE(clave %in% resolucion)
}

.severidad_por_umbrales_deriva <- function(valor, umbrales) {
  if (!is.finite(valor)) return("error")
  if (.alcanza_umbral_deriva(valor, umbrales[["error"]])) return("error")
  if (.alcanza_umbral_deriva(valor, umbrales[["sospechoso"]])) {
    return("sospechoso")
  }
  "ok"
}

# Cuantos valores entraron en el resumen cuantitativo de una columna. `NA` si el
# perfil no lo dice o la columna no tiene un resumen calculado.
.n_resumen_deriva <- function(fila) {
  estado <- if ("estado_resumen_cuantitativo" %in% names(fila)) {
    as.character(fila[["estado_resumen_cuantitativo"]][[1L]])
  } else NA_character_
  if (!isTRUE(estado %in% c("calculados", "calculados_sobre_valores"))) {
    return(NA_real_)
  }
  excluidos <- .valor_columna_deriva(fila, "n_valores_excluidos_resumen")
  if (is.na(excluidos)) excluidos <- 0
  # Un `NaN` esta en `n_faltantes` y tambien en los excluidos del resumen: se
  # descuenta una sola vez. Los infinitos y los centinelas estan solo en los
  # excluidos.
  nan <- .valor_columna_deriva(fila, "n_nan")
  if (is.na(nan)) nan <- 0
  n <- .valor_columna_deriva(fila, "n_aplicables") -
    .valor_columna_deriva(fila, "n_faltantes") - max(excluidos - nan, 0)
  if (!isTRUE(is.finite(n)) || n < 2) return(NA_real_)
  n
}

# La resolucion con que se escribio una columna numerica: el paso medio entre
# sus valores distintos, `(maximo - minimo) / (distintos - 1)`. Dos entregas a 2
# decimales y a 0 tienen pasos muy distintos, y el desvio de la que redondea
# incluye el error de redondeo. Si el paso cambia 4 veces o mas -un cambio de
# dispersion con la misma resolucion mueve el paso como mucho a la par del
# rango- se declara cambio de resolucion. Es una heuristica con lo que el perfil
# guarda, no una medicion de los decimales.
.paso_distintos_deriva <- function(fila) {
  distintos <- .valor_columna_deriva(fila, "n_distintos")
  extremos <- c(
    .valor_columna_deriva(fila, "minimo"), .valor_columna_deriva(fila, "maximo")
  )
  if (!all(is.finite(c(distintos, extremos))) || distintos < 3) return(NA_real_)
  (extremos[[2L]] - extremos[[1L]]) / (distintos - 1)
}

.resolucion_cambio_deriva <- function(a, b) {
  paso <- c(.paso_distintos_deriva(a), .paso_distintos_deriva(b))
  # Con menos de tres valores distintos en un solo lado el paso no se calcula, y
  # esa entrega puede ser la otra redondeada a dos valores: no se puede decidir
  # que la resolucion no cambio, y se trata como cambiada. Si no se calcula en
  # ninguno de los dos, no hay un lado del que pueda venir un redondeo.
  if (xor(is.finite(paso[[1L]]), is.finite(paso[[2L]]))) return(TRUE)
  if (!all(is.finite(paso)) || any(paso <= 0)) return(FALSE)
  max(paso) / min(paso) >= 4
}

# Lo que se sabe de la unidad al publicar un corrimiento de la media. Sin unidad
# declarada en los dos perfiles no se puede afirmar que sea otra cosa que un
# cambio de los valores: se dice lo observado.
.nota_unidad_deriva <- function(a, b, m_a, m_b) {
  ua <- if ("unidad" %in% names(a)) as.character(a[["unidad"]][[1L]]) else NA_character_
  ub <- if ("unidad" %in% names(b)) as.character(b[["unidad"]][[1L]]) else NA_character_
  if (!is.na(ua) && !is.na(ub) && identical(ua, ub)) {
    return(paste0(" La unidad declarada es la misma en las dos (", ua, ")."))
  }
  razon <- if (m_a != 0) m_b / m_a else NA_real_
  if (isTRUE(is.finite(razon)) && (abs(razon) >= 5 || abs(razon) <= 0.2)) {
    return(paste0(
      " La media se multiplic\u00f3 por ", .texto_numero_deriva(signif(razon, 3L)),
      " y la columna no declara su unidad en los dos perfiles: puede ser un ",
      "cambio de unidad."
    ))
  }
  " La columna no declara su unidad en los dos perfiles."
}

.fecha_en_fila_deriva <- function(fila) {
  campos <- c("minimo_fecha", "maximo_fecha")
  campos <- campos[campos %in% names(fila)]
  any(!is.na(unlist(fila[campos])))
}

# Media y dispersion de una columna numerica en los dos lados. Devuelve una
# lista de filas candidatas (cada una con `aspecto`, `severidad`, `medida`,
# `valor_anterior`, `valor_actual`, `evidencia`, `descripcion`); vacia si no
# hay deriva o si el perfil no alcanza para afirmarla.
.deriva_numerica_deriva <- function(a, b) {
  if (.fecha_en_fila_deriva(a) || .fecha_en_fila_deriva(b)) return(list())
  n_a <- .n_resumen_deriva(a)
  n_b <- .n_resumen_deriva(b)
  m_a <- .valor_columna_deriva(a, "media")
  m_b <- .valor_columna_deriva(b, "media")
  s_a <- .valor_columna_deriva(a, "desvio")
  s_b <- .valor_columna_deriva(b, "desvio")
  if (!all(is.finite(c(n_a, n_b, m_a, m_b, s_a, s_b)))) return(list())
  if (s_a == 0 && s_b == 0) {
    # Una constante que pasa a otra: no hay desvio con que estandarizar.
    if (m_a == m_b) return(list())
    return(list(list(
      aspecto = "deriva_media", severidad = "sospechoso", medida = NA_real_,
      valor_anterior = paste0("constante ", .texto_numero_deriva(signif(m_a, 6L))),
      valor_actual = paste0("constante ", .texto_numero_deriva(signif(m_b, 6L))),
      descripcion = paste0(
        "La columna era constante en ", .texto_numero_deriva(signif(m_a, 6L)),
        " y ahora es constante en ", .texto_numero_deriva(signif(m_b, 6L)), ".",
        .nota_unidad_deriva(a, b, m_a, m_b)
      ),
      evidencia = paste0(
        "Sin dispersi\u00f3n en ninguna de las dos entregas (desv\u00edo 0): no hay ",
        "diferencia de medias estandarizada; n ", .texto_numero_deriva(n_a),
        " y ", .texto_numero_deriva(n_b), "."
      )
    )))
  }
  if (s_a <= 0 || s_b <= 0) return(list())
  fmt <- function(x) .formatear_decimal_publicado(x, 4L)
  cifras <- function(m, s, n) {
    paste0(
      "media ", .texto_numero_deriva(signif(m, 6L)), ", desv\u00edo ",
      .texto_numero_deriva(signif(s, 6L)), ", n ", .texto_numero_deriva(n)
    )
  }
  filas <- list()
  s_comun <- sqrt((s_a^2 + s_b^2) / 2)
  d <- (m_b - m_a) / s_comun
  z_media <- abs(m_b - m_a) / sqrt(s_a^2 / n_a + s_b^2 / n_b)
  # Con pocos valores el cuantil es el de una t de Welch, no el normal.
  v_a <- s_a^2 / n_a
  v_b <- s_b^2 / n_b
  grados <- (v_a + v_b)^2 / (v_a^2 / (n_a - 1) + v_b^2 / (n_b - 1))
  critico_media <- stats::qt(0.9995, df = grados)
  severidad <- .severidad_por_umbrales_deriva(abs(d), .UMBRALES_DERIVA_MEDIA)
  if (severidad != "ok" && z_media >= critico_media) {
    filas[[length(filas) + 1L]] <- list(
      aspecto = "deriva_media", severidad = severidad, medida = d,
      valor_anterior = cifras(m_a, s_a, n_a),
      valor_actual = cifras(m_b, s_b, n_b),
      descripcion = paste0(
        "Cambi\u00f3 la media de la columna entre las dos entregas: ",
        .texto_numero_deriva(signif(m_a, 6L)), " -> ",
        .texto_numero_deriva(signif(m_b, 6L)), ", medida en desv\u00edos.",
        .nota_unidad_deriva(a, b, m_a, m_b)
      ),
      evidencia = paste0(
        "Diferencia de medias estandarizada = ", fmt(d),
        " (umbrales: ", fmt(.UMBRALES_DERIVA_MEDIA[["sospechoso"]]),
        " sospechoso, ", fmt(.UMBRALES_DERIVA_MEDIA[["error"]]),
        " error); distancia a su ruido de muestreo: ", fmt(z_media),
        " errores t\u00edpicos (se exige ", fmt(critico_media), ")."
      )
    )
  }
  razon <- max(s_b / s_a, s_a / s_b)
  k_a <- .curtosis_acotada_deriva(a, m_a, s_a)
  k_b <- .curtosis_acotada_deriva(b, m_b, s_b)
  if (!all(is.finite(c(k_a, k_b)))) return(filas)
  # Si un solo valor puede explicar una cuarta parte de la suma de cuadrados, el
  # desvio es ese valor y no la dispersion de la columna: no se compara. Es el
  # caso de una columna casi constante con un valor lejano.
  if (max(k_a / (n_a - 1), k_b / (n_b - 1)) > .CONCENTRACION_MAXIMA_DERIVA ||
      min(n_a, n_b) < .N_MINIMO_DISPERSION_DERIVA) {
    return(filas)
  }
  if (.resolucion_cambio_deriva(a, b)) return(filas)
  se_log <- sqrt((k_a - 1) / (4 * n_a) + (k_b - 1) / (4 * n_b))
  z_desvio <- abs(log(s_b / s_a)) / se_log
  severidad <- .severidad_por_umbrales_deriva(razon, .UMBRALES_DERIVA_DISPERSION)
  if (severidad != "ok" && z_desvio >= .Z_RUIDO_DERIVA) {
    filas[[length(filas) + 1L]] <- list(
      aspecto = "deriva_dispersion", severidad = severidad,
      medida = s_b / s_a,
      valor_anterior = cifras(m_a, s_a, n_a),
      valor_actual = cifras(m_b, s_b, n_b),
      descripcion = paste(
        "La dispersi\u00f3n de la columna cambi\u00f3 respecto de la entrega",
        "anterior, aunque la media pueda seguir donde estaba."
      ),
      evidencia = paste0(
        "Raz\u00f3n entre el desv\u00edo actual y el anterior = ", fmt(s_b / s_a),
        " (la mayor de la raz\u00f3n y su inversa: ", fmt(razon), "; umbrales: ",
        fmt(.UMBRALES_DERIVA_DISPERSION[["sospechoso"]]), " sospechoso, ",
        fmt(.UMBRALES_DERIVA_DISPERSION[["error"]]),
        " error); distancia a su ruido de muestreo: ", fmt(z_desvio),
        " errores t\u00edpicos (se exige ", fmt(.Z_RUIDO_DERIVA), ")."
      )
    )
  }
  filas
}

# PSI entre dos tablas de vocabulario (`valor`, `frecuencia`), sobre los valores
# que estan en las dos. Los que aparecen o desaparecen los publica la fila
# `vocabulario`. `NULL` si no hay deriva medible.
.deriva_categorica_deriva <- function(tabla_a, tabla_b) {
  # Los valores que difieren solo en espacios de los bordes o en mayusculas son
  # el mismo valor escrito de otra manera, como en la fila de vocabulario: se
  # suman antes de comparar.
  por_forma <- function(tabla) {
    clave <- .clave_forma_texto(tabla$valor)
    unicas <- !duplicated(clave)
    data.frame(
      valor = as.character(tabla$valor)[unicas],
      frecuencia = as.numeric(tapply(
        as.numeric(tabla$frecuencia), factor(clave, levels = clave[unicas]), sum
      )),
      clave = clave[unicas], stringsAsFactors = FALSE
    )
  }
  tabla_a <- por_forma(tabla_a)
  tabla_b <- por_forma(tabla_b)
  clave_a <- tabla_a$clave
  clave_b <- tabla_b$clave
  comunes <- intersect(clave_a, clave_b)
  if (length(comunes) < 2L) return(NULL)
  f_a <- as.numeric(tabla_a$frecuencia[match(comunes, clave_a)])
  f_b <- as.numeric(tabla_b$frecuencia[match(comunes, clave_b)])
  valores <- as.character(tabla_a$valor[match(comunes, clave_a)])
  total_a <- sum(f_a)
  total_b <- sum(f_b)
  if (!all(is.finite(c(total_a, total_b))) || total_a < 1 || total_b < 1) {
    return(NULL)
  }
  # Las clases que esperarian menos de cinco filas en algun lado se juntan: el
  # PSI y el chi-cuadrado de homogeneidad no sirven con celdas casi vacias
  # (Cochran, 1954), y un vocabulario de muchas clases raras acusaria el ruido.
  agrupar <- function(f_a, f_b, valores) {
    pool <- (f_a + f_b) / (total_a + total_b)
    raras <- pmin(pool * total_a, pool * total_b) < 5
    if (!any(raras)) return(list(a = f_a, b = f_b, v = valores))
    a <- c(f_a[!raras], sum(f_a[raras]))
    b <- c(f_b[!raras], sum(f_b[raras]))
    v <- c(valores[!raras], "(otras)")
    pool <- (a + b) / (total_a + total_b)
    if (length(a) > 1L && min(pool[length(a)] * total_a,
                              pool[length(a)] * total_b) < 5) {
      destino <- which.min(replace(pool, length(a), Inf))
      a[destino] <- a[destino] + a[length(a)]
      b[destino] <- b[destino] + b[length(b)]
      a <- a[-length(a)]
      b <- b[-length(b)]
      v <- v[-length(v)]
    }
    list(a = a, b = b, v = v)
  }
  g <- agrupar(f_a, f_b, valores)
  k <- length(g$a)
  if (k < 2L) return(NULL)
  p_a <- g$a / total_a
  p_b <- g$b / total_b
  # Con una clase en cero en un lado el PSI no esta definido; el suavizado de
  # 0,5 por celda lo acota sin mover el resultado cuando hay frecuencias.
  q_a <- (g$a + 0.5) / (total_a + 0.5 * k)
  q_b <- (g$b + 0.5) / (total_b + 0.5 * k)
  psi <- sum((q_a - q_b) * log(q_a / q_b))
  ruido <- stats::qchisq(0.999, df = k - 1L) * (1 / total_a + 1 / total_b)
  severidad <- .severidad_por_umbrales_deriva(psi, .UMBRALES_DERIVA_PSI)
  if (severidad == "ok" || psi <= ruido) return(NULL)
  orden <- .orden_por_bytes(-abs(p_a - p_b), seq_len(k))
  orden <- orden[seq_len(min(5L, k))]
  fmt <- function(x) .formatear_decimal_publicado(x, 4L)
  texto <- function(p) {
    paste0(g$v[orden], " (", fmt(p[orden]), ")", collapse = " | ")
  }
  list(
    severidad = severidad, medida = psi,
    valor_anterior = texto(p_a), valor_actual = texto(p_b),
    evidencia = paste0(
      "PSI = ", fmt(psi), " (umbrales: ", fmt(.UMBRALES_DERIVA_PSI[["sospechoso"]]),
      " sospechoso, ", fmt(.UMBRALES_DERIVA_PSI[["error"]]),
      " error); ruido esperado de muestreo: ", fmt(ruido),
      "; distancia de variaci\u00f3n total: ", fmt(sum(abs(p_a - p_b)) / 2),
      ". Se compara el reparto entre los valores que est\u00e1n en las dos ",
      "entregas (", length(comunes), " valores", if (k < length(comunes)) {
        paste0(", ", k, " clases tras juntar las que esperan menos de cinco filas")
      } else "", "); los que aparecen o desaparecen los dice la fila de ",
      "vocabulario. Se muestran las proporciones de las clases que m\u00e1s ",
      "cambiaron."
    )
  )
}

# El cambio de conteo de un hallazgo que sigue con la misma severidad. Compara
# la proporcion de filas afectadas. `NULL` si no es material o si cabe en el
# ruido de muestreo.
.cambio_conteo_hallazgo_deriva <- function(afectados_a, evaluados_a,
                                           afectados_b, evaluados_b,
                                           umbral_cambio, umbral_error) {
  cifras <- suppressWarnings(as.numeric(
    c(afectados_a, evaluados_a, afectados_b, evaluados_b)
  ))
  if (length(cifras) != 4L || anyNA(cifras) || any(!is.finite(cifras)) ||
      cifras[[2L]] < 1 || cifras[[4L]] < 1 || any(cifras < 0) ||
      cifras[[1L]] > cifras[[2L]] || cifras[[3L]] > cifras[[4L]]) {
    return(NULL)
  }
  p_a <- cifras[[1L]] / cifras[[2L]]
  p_b <- cifras[[3L]] / cifras[[4L]]
  delta <- p_b - p_a
  if (!.alcanza_umbral_deriva(delta, umbral_cambio)) return(NULL)
  conjunta <- (cifras[[1L]] + cifras[[3L]]) / (cifras[[2L]] + cifras[[4L]])
  error_tipico <- sqrt(conjunta * (1 - conjunta) *
                         (1 / cifras[[2L]] + 1 / cifras[[4L]]))
  if (!isTRUE(error_tipico > 0)) return(NULL)
  z <- abs(delta) / error_tipico
  if (z < .Z_RUIDO_DERIVA) return(NULL)
  agrava <- delta > 0
  severidad <- if (!agrava) {
    "ok"
  } else if (.alcanza_umbral_deriva(delta, umbral_error)) {
    "error"
  } else "sospechoso"
  fmt <- function(x) .formatear_decimal_publicado(x, 4L)
  texto <- function(k, n) {
    paste0(.texto_numero_deriva(k), " de ", .texto_numero_deriva(n))
  }
  list(
    cambio = if (agrava) "agravado" else "atenuado", severidad = severidad,
    anterior = texto(cifras[[1L]], cifras[[2L]]),
    actual = texto(cifras[[3L]], cifras[[4L]]), delta = delta,
    evidencia = paste0(
      "Proporci\u00f3n de filas afectadas: ", fmt(p_a), " -> ", fmt(p_b),
      " (cambio de ", fmt(delta), "; umbral ", fmt(umbral_cambio),
      "); distancia a su ruido de muestreo: ", fmt(z),
      " errores t\u00edpicos (se exige ", fmt(.Z_RUIDO_DERIVA), ")."
    )
  )
}
