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

# La dispersion se compara con al menos 20 valores por lado; con menos de 50 el
# error tipico de ln(s) usa la varianza de Bonett (2006) y no la sencilla. Y sin
# que un solo valor explique mas de esta parte de la suma de cuadrados.
.N_MINIMO_DISPERSION_DERIVA <- 50
.N_MINIMO_ABSOLUTO_DISPERSION_DERIVA <- 20
.CONCENTRACION_MAXIMA_DERIVA <- 0.25

# Con menos de 50 valores la curtosis muestral no dice la de la poblacion: una
# cola pesada o una asimetria fuerte casi no asoman en 30 valores. Piso de la
# curtosis que se usa alli (medido: con 3 pasaban 5 de 40.000 pares nulos de
# colas pesadas, con 6 pasan 2, y la potencia con 40 valores y desvios 1 y 3
# baja de 87 a 85 %).
.CURTOSIS_MINIMA_CHICA_DERIVA <- 6

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
  # Sus filas son las que `faltantes` ya cuenta en la misma columna.
  "identificador_con_ausentes",
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

# La grilla en la que caen los valores de una columna: 10^-d, con d los decimales
# de sus extremos y de su moda. El perfil no guarda los decimales de la columna
# entera, pero un redondeo a d decimales deja esos valores con d. No es la
# distancia media entre valores distintos: unos enteros dispersos en un rango
# ancho tienen grilla 1 aunque esten separados por 7. Es una estimacion con tres
# valores: si los tres caen en una grilla mas gruesa que la de los datos, se
# descuenta de mas (el descuento solo se aplica con valores repetidos y un paso
# mayor que el desvio); si el redondeo fue a multiplos mayores (de 5, de 1.000),
# se descuenta de menos. Devuelve 0 si no caen en una grilla de hasta 8
# decimales (valores continuos): no hay redondeo que descontar.
.grilla_deriva <- function(fila) {
  extremos <- c(
    .valor_columna_deriva(fila, "minimo"), .valor_columna_deriva(fila, "maximo")
  )
  if (!all(is.finite(extremos))) return(0)
  # La moda tambien es un valor de la columna: si trae mas decimales que los
  # extremos, la grilla es mas fina de lo que ellos muestran.
  moda <- if ("moda" %in% names(fila)) {
    suppressWarnings(as.numeric(as.character(fila[["moda"]][[1L]])))
  } else NA_real_
  valores <- c(extremos, if (is.finite(moda)) moda)
  for (d in 0:8) {
    e <- valores * 10^d
    if (all(abs(e - round(e)) <= pmax(1e-9, 4 * .Machine$double.eps * abs(e)))) {
      return(10^-d)
    }
  }
  0
}

# Una cola que el desvio y su cota de curtosis no recogen: al menos dos valores
# fuera de los limites de Tukey y el 2 % o mas de los valores. Es lo que dejan un
# centinela (999, 9999) o una cola pesada en una columna chica: segun cuantas
# veces caiga en cada muestra, el desvio cambia por mucho sin que cambie la
# poblacion. Sin el recuento de atipicos en el perfil, no se puede decir.
.cola_pesada_deriva <- function(fila, n) {
  atipicos <- .valor_columna_deriva(fila, "n_outliers")
  isTRUE(is.finite(atipicos) && atipicos >= 2 && atipicos / n >= 0.02)
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
  texto_grilla <- function(g) {
    if (g > 0) .texto_numero_deriva(signif(g, 3L)) else "ninguna visible"
  }
  # El paso con que se escribio cada columna, si cambio entre las entregas. El
  # error de redondear con paso h no pasa de h / 2 en ningun valor: la media
  # observada queda a lo sumo a h / 2 de la de los valores sin redondear, y el
  # desvio tambien (desigualdad de Minkowski). Con otro paso en cada lado, un
  # corrimiento menor que la suma de las dos mitades puede ser solo redondeo, y
  # el tamano y la prueba se evaluan descontandolo, en la direccion que menos
  # acusa. Con el mismo paso no se toca nada.
  cambio_resolucion <- .resolucion_cambio_deriva(a, b)
  paso_ab <- c(.paso_distintos_deriva(a), .paso_distintos_deriva(b))
  grilla <- c(.grilla_deriva(a), .grilla_deriva(b))
  # Solo cuenta como redondeo el paso de un lado con valores repetidos: en una
  # columna de valores todos distintos el "paso" medio es el rango sobre n, no
  # una grilla, y descontarlo taparia un corrimiento real.
  # Y solo si el paso es mayor que el desvio de ese lado: con una grilla fina
  # frente a la dispersion el error de redondeo promedia casi cero (su
  # promedio tiene desvio h / sqrt(12 n)) y no corre la
  # media; con una gruesa los valores se amontonan en pocos puntos y el
  # redondeo puede mover la media hasta medio paso.
  con_grilla <- c(
    .valor_columna_deriva(a, "n_distintos") <= n_a / 2,
    .valor_columna_deriva(b, "n_distintos") <= n_b / 2
  )
  # La grilla cambio de verdad si una entrega no tiene (valores continuos) y la
  # otra si, o si una es cuatro veces la otra. Dos lados con la misma grilla no
  # cambiaron, aunque uno tenga dos valores distintos y el otro tres, o valores
  # dispersos en un rango ancho.
  cambio_paso <- max(grilla) > 0 && (min(grilla) == 0 || max(grilla) / min(grilla) >= 4)
  pasos_media <- if (cambio_paso) {
    grilla * (con_grilla & c(s_a, s_b) < grilla)
  } else c(0, 0)
  s_ef <- c(s_a, s_b) + pasos_media / 2
  s_comun <- sqrt((s_a^2 + s_b^2) / 2)
  d <- (m_b - m_a) / s_comun
  salto <- max(abs(m_b - m_a) - sum(pasos_media) / 2, 0)
  d_cota <- salto / sqrt((s_ef[[1L]]^2 + s_ef[[2L]]^2) / 2)
  z_media <- salto / sqrt(s_ef[[1L]]^2 / n_a + s_ef[[2L]]^2 / n_b)
  # Con pocos valores el cuantil es el de una t de Welch, no el normal.
  v_a <- s_ef[[1L]]^2 / n_a
  v_b <- s_ef[[2L]]^2 / n_b
  grados <- (v_a + v_b)^2 / (v_a^2 / (n_a - 1) + v_b^2 / (n_b - 1))
  critico_media <- stats::qt(0.9995, df = grados)
  severidad <- .severidad_por_umbrales_deriva(d_cota, .UMBRALES_DERIVA_MEDIA)
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
        " errores t\u00edpicos (se exige ", fmt(critico_media), ").",
        if (any(pasos_media > 0)) paste0(
          " Grilla de los valores (paso del redondeo): entrega anterior, ",
          texto_grilla(grilla[[1L]]), "; actual, ",
          texto_grilla(grilla[[2L]]), ". El corrimiento se ",
          "evalu\u00f3 descontando lo que el redondeo puede explicar (a lo sumo ",
          .texto_numero_deriva(signif(sum(pasos_media) / 2, 3L)), "), y qued\u00f3 en ",
          fmt(d_cota), " desv\u00edos."
        )
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
      min(n_a, n_b) < .N_MINIMO_ABSOLUTO_DISPERSION_DERIVA) {
    return(filas)
  }
  # Cada desvio se acota por el redondeo con que se escribieron los valores: el
  # error de redondear con paso h no pasa de h / 2 en ningun valor, asi que su
  # desvio tampoco, y el desvio de los valores sin redondear queda a lo sumo a
  # h / 2 del observado (desigualdad de Minkowski). No supone una densidad
  # suave, como la correccion de Sheppard, que con una distribucion de pocos
  # grupos (99,4 y 100,6 que pasan a 99 y 101) descuenta mucho menos de lo que
  # el redondeo agrego. Con el mismo paso en los dos lados no se descuenta nada.
  # Con pocos valores y menos de tres valores distintos en un lado el redondeo
  # puede ser todo lo que mueve el desvio (dos grupos que pasan de 99,4 y 100,6 a
  # 99 y 101) y no hay paso con que acotarlo: no se compara.
  if (min(n_a, n_b) < .N_MINIMO_DISPERSION_DERIVA &&
      !all(is.finite(c(.paso_distintos_deriva(a), .paso_distintos_deriva(b))))) {
    return(filas)
  }
  # Con pocos valores o con otro paso, una cola que mueve el desvio segun cuantas
  # veces caiga en cada muestra (un centinela, una cola pesada) y que la cota de
  # la curtosis no ve: no se compara.
  if ((min(n_a, n_b) < .N_MINIMO_DISPERSION_DERIVA || cambio_resolucion) &&
      (.cola_pesada_deriva(a, n_a) || .cola_pesada_deriva(b, n_b))) {
    return(filas)
  }
  # Con el paso medio incalculable en un lado (dos valores distintos contra tres o
  # mas) no se puede decidir que el redondeo no explica el desvio: no se compara.
  if (cambio_resolucion && !all(is.finite(paso_ab))) return(filas)
  # El redondeo se descuenta si la grilla cambio de verdad, y solo en un lado con
  # valores repetidos.
  pasos <- if (cambio_resolucion && cambio_paso) grilla * con_grilla else c(0, 0)
  # La razon mas baja que cabe una vez descontado el redondeo, en la direccion
  # que menos acusa.
  s_menor <- pmax(c(s_a, s_b) - pasos / 2, 0)
  s_mayor <- c(s_a, s_b) + pasos / 2
  razon_cota <- if (s_b >= s_a) s_menor[[2L]] / s_mayor[[1L]] else s_menor[[1L]] / s_mayor[[2L]]
  if (!isTRUE(razon_cota > 1)) return(filas)
  # Con menos de 50 valores por lado el error tipico de ln(s) usa la varianza
  # de Bonett (2006), (k - (n - 3) / n) / (n - 1) para ln(s^2), con la cota de
  # la curtosis en lugar de la muestral; con 50 o mas se conserva la formula
  # sencilla.
  var_log <- function(k, n) {
    if (n >= .N_MINIMO_DISPERSION_DERIVA) return((k - 1) / (4 * n))
    k <- max(k, .CURTOSIS_MINIMA_CHICA_DERIVA)
    (k - (n - 3) / n) / (4 * (n - 1))
  }
  se_log <- sqrt(var_log(k_a, n_a) + var_log(k_b, n_b))
  z_desvio <- log(razon_cota) / se_log
  severidad <- .severidad_por_umbrales_deriva(
    razon_cota, .UMBRALES_DERIVA_DISPERSION
  )
  if (severidad != "ok" && z_desvio >= .Z_RUIDO_DERIVA) {
    nota_resolucion <- if (any(pasos > 0)) {
      paste0(
        " Grilla de los valores (paso del redondeo): entrega anterior, ",
        texto_grilla(grilla[[1L]]), "; actual, ",
        texto_grilla(grilla[[2L]]), ". La raz\u00f3n ",
        "se evalu\u00f3 descontando lo ",
        "que el redondeo puede explicar, a lo sumo la mitad del paso en cada ",
        "desv\u00edo."
      )
    } else ""
    filas[[length(filas) + 1L]] <- list(
      aspecto = "deriva_dispersion", severidad = severidad,
      medida = s_b / s_a,
      valor_anterior = cifras(m_a, s_a, n_a),
      valor_actual = cifras(m_b, s_b, n_b),
      descripcion = paste0(
        "La dispersi\u00f3n de la columna cambi\u00f3 respecto de la entrega ",
        "anterior, aunque la media pueda seguir donde estaba.", nota_resolucion
      ),
      evidencia = paste0(
        "Raz\u00f3n entre el desv\u00edo actual y el anterior = ", fmt(s_b / s_a),
        " (la mayor de la raz\u00f3n y su inversa: ", fmt(razon),
        if (any(pasos > 0)) paste0("; descontado el redondeo: ", fmt(razon_cota)),
        "; umbrales: ",
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
  # Con pocas filas las clases raras se juntan (mas abajo) y un cambio real de una
  # categoria chica queda dentro de "(otras)" o debajo del ruido del PSI. Se
  # mira entonces cada categoria contra el resto con la prueba exacta de Fisher
  # (no supone muestras grandes), con Bonferroni sobre las categorias que se
  # comparan para sostener el nivel 0,001 de toda la columna, y con el tamano
  # del efecto en el mismo PSI y sus mismos cortes, calculado sobre la tabla de
  # dos clases: un reparto que no cambia lo bastante no se publica aunque la
  # prueba lo detecte. Como juntar clases solo puede bajar el PSI, esta via no
  # acusa nada que el PSI de la columna entera no acuse con mas filas.
  exacta <- function() {
    k0 <- length(comunes)
    psi_dos <- function(x, y) {
      q_a <- c(x + 0.5, total_a - x + 0.5) / (total_a + 1)
      q_b <- c(y + 0.5, total_b - y + 0.5) / (total_b + 1)
      sum((q_a - q_b) * log(q_a / q_b))
    }
    psi_i <- mapply(psi_dos, f_a, f_b)
    candidatas <- which(psi_i >= .UMBRALES_DERIVA_PSI[["sospechoso"]])
    if (!length(candidatas)) return(NULL)
    nivel <- 0.001 / k0
    p_i <- vapply(candidatas, function(i) {
      stats::fisher.test(matrix(
        c(f_a[i], total_a - f_a[i], f_b[i], total_b - f_b[i]), nrow = 2L
      ))$p.value
    }, numeric(1L))
    quedan <- candidatas[p_i < nivel]
    if (!length(quedan)) return(NULL)
    quedan <- quedan[.orden_por_bytes(-psi_i[quedan], seq_along(quedan))]
    psi <- psi_i[[quedan[[1L]]]]
    severidad <- .severidad_por_umbrales_deriva(psi, .UMBRALES_DERIVA_PSI)
    fmt <- function(x) .formatear_decimal_publicado(x, 4L)
    mostradas <- quedan[seq_len(min(5L, length(quedan)))]
    texto <- function(f, total) {
      paste0(valores[mostradas], " (", fmt(f[mostradas] / total), ")",
             collapse = " | ")
    }
    list(
      severidad = severidad, medida = psi,
      valor_anterior = texto(f_a, total_a), valor_actual = texto(f_b, total_b),
      descripcion = paste(
        "Cambi\u00f3 el peso de una o m\u00e1s categor\u00edas de la columna entre",
        "las dos entregas, con pocas filas: la prueba exacta de cada categor\u00eda",
        "contra el resto supera el ruido de muestreo al nivel 0,001 de la",
        "columna."
      ),
      evidencia = paste0(
        "Prueba exacta de Fisher de cada categor\u00eda contra el resto, con ",
        "Bonferroni sobre ", k0, " categor\u00edas: p = ",
        .formatear_decimal_publicado(min(p_i[p_i < nivel]), 6L),
        " (se exige menos de ", .formatear_decimal_publicado(nivel, 6L),
        "); PSI de la categor\u00eda contra el resto = ", fmt(psi), " (umbrales: ",
        fmt(.UMBRALES_DERIVA_PSI[["sospechoso"]]), " sospechoso, ",
        fmt(.UMBRALES_DERIVA_PSI[["error"]]), " error). Se muestran las ",
        "proporciones de las categor\u00edas que pasan la prueba; las que ",
        "aparecen o desaparecen los dice la fila de vocabulario."
      )
    )
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
  if (k < 2L) return(exacta())
  p_a <- g$a / total_a
  p_b <- g$b / total_b
  # Con una clase en cero en un lado el PSI no esta definido; el suavizado de
  # 0,5 por celda lo acota sin mover el resultado cuando hay frecuencias.
  q_a <- (g$a + 0.5) / (total_a + 0.5 * k)
  q_b <- (g$b + 0.5) / (total_b + 0.5 * k)
  psi <- sum((q_a - q_b) * log(q_a / q_b))
  ruido <- stats::qchisq(0.999, df = k - 1L) * (1 / total_a + 1 / total_b)
  severidad <- .severidad_por_umbrales_deriva(psi, .UMBRALES_DERIVA_PSI)
  if (severidad == "ok" || psi <= ruido) return(exacta())
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
