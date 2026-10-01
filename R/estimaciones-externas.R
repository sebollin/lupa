# `lupa` no estima. Recibe estimaciones ya calculadas —por `survey`, por
# `calidad` del INE de Chile, o por cualquier otra fuente— y las lleva al
# contrato de `medir()` para poder evaluarlas contra un marco declarado.
#
# La distincion no es formal: estimar sobre un diseno muestral complejo es otra
# disciplina y otra dependencia. Lo que este paquete sabe hacer es evaluar
# contra un marco, y eso es lo que ofrece. Por eso el adaptador **declara la
# procedencia en cada medida**: para que nadie lea el resultado como si `lupa`
# lo hubiera calculado.

# Los siete estadisticos habituales de una estimacion por muestreo, con su
# orientacion y su tipo. La orientacion es la que decide si un valor alto es
# bueno o malo, y sin ella el numero no se puede evaluar.
.catalogo_estimaciones <- function() {
  data.frame(
    estadistico = c("stat", "se", "cv", "n", "df", "deff", "ess"),
    metrica = c(
      "Estimacion", "ErrorEstandar", "CoeficienteVariacion",
      "TamanoMuestra", "GradosLibertad", "EfectoDiseno",
      "TamanoMuestraEfectivo"
    ),
    # `real` es una proporcion en [0, 1] y una estimacion no lo es: una media de
    # ingresos, un total, 30 grados de libertad o un coeficiente de variacion de
    # 1,5 se declaraban `real` y `evaluar()` rechazaba la medicion entera. Las
    # pruebas no lo veian porque sus estimaciones eran proporciones.
    tipo_resultado = c(
      "numero_real", "numero_real", "numero_real", "entero", "numero_real",
      "numero_real", "numero_real"
    ),
    orientacion = c(
      "no_aplica", "defecto", "defecto",
      "conformidad", "conformidad", "defecto", "conformidad"
    ),
    unidad = c(
      "unidad de la estimacion", "unidad de la estimacion", "proporcion",
      "casos", "grados", "razon", "casos"
    ),
    stringsAsFactors = FALSE
  )
}

# El valor numerico de una columna de estimaciones. Un factor se lee por su texto:
# `as.numeric()` sobre un factor devuelve el CODIGO del nivel, y una columna
# `factor(c("0.42", "0.38"))` -lo que da `read.csv(stringsAsFactors = TRUE)`- se
# publicaba como las estimaciones 2 y 1.
.valores_estimacion <- function(valores) {
  if (is.factor(valores)) valores <- as.character(valores)
  suppressWarnings(as.numeric(valores))
}

# Que valores finitos caben en el dominio de cada estadistico. Un tamano de
# muestra es un entero no negativo y un error estandar, un coeficiente de
# variacion, unos grados de libertad, un efecto de diseno o un tamano efectivo no
# son negativos. Fuera de eso no hay estimacion que publicar: `TamanoMuestra =
# 0.5` contradecia su propio `tipo_resultado = "entero"` y `evaluar()` rechazaba
# la medicion ENTERA, incluidas las estimaciones validas.
.en_dominio_estimacion <- function(estadistico, x) {
  switch(
    estadistico,
    n = x >= 0 & x == floor(x),
    se = , cv = , df = , deff = , ess = x >= 0,
    rep(TRUE, length(x))
  )
}

#' Catálogo de estadísticos de estimación reconocidos
#'
#' Los siete estadísticos que [medicion_desde_estimaciones()] sabe llevar al
#' contrato de [medir()], con la métrica a la que corresponden, su tipo, su
#' unidad y su **orientación**: si un valor alto es conformidad o defecto. Sin
#' la orientación el número no se puede evaluar, porque un coeficiente de
#' variación de `0,30` y un tamaño de muestra de `0,30` no se leen igual.
#'
#' @return Data frame con `estadistico`, `metrica`, `tipo_resultado`,
#'   `orientacion` y `unidad`.
#' @export
#' @seealso [medicion_desde_estimaciones()], [evaluar()]
#'
#' @examples
#' estadisticos_estimacion()
estadisticos_estimacion <- function() {
  salida <- .catalogo_estimaciones()
  rownames(salida) <- NULL
  salida
}

#' Llevar estimaciones ya calculadas al contrato de medición
#'
#' `lupa` **no estima**: eso necesita un diseño muestral, estimación de varianza
#' y otra disciplina. Lo que sabe hacer es evaluar contra un marco declarado.
#' Esta función recibe estimaciones calculadas por otra herramienta —`survey`,
#' el paquete [`calidad`](https://github.com/inesscc/calidad) del INE de Chile,
#' o cualquier otra— y las convierte en una medición que [evaluar()] entiende.
#'
#' Cada estadístico se convierte en **una medida canónica propia**, con su
#' métrica, su tipo y su orientación, porque los siete tienen unidades y
#' dominios distintos: un coeficiente de variación y un tamaño de muestra no se
#' evalúan con la misma regla. Los reconocidos están en
#' [estadisticos_estimacion()].
#'
#' Cada medida declara su procedencia en `fuente`, de modo que nadie lea el
#' resultado como si `lupa` lo hubiera calculado. Los estadísticos que la tabla
#' no traiga simplemente no producen medidas: no se rellenan con ceros ni se
#' estiman.
#'
#' **Una columna vacía tampoco produce medidas.** Un estadístico cuya columna
#' está presente pero no trae ningún valor utilizable no es una estimación: no
#' se publica con `resultado = NA`, porque eso sería informar como medido lo que
#' nadie midió —y además deja la medición inservible, porque [evaluar()] la
#' rechaza entera—. Esos estadísticos se declaran en el atributo
#' `estadisticos_sin_valores`, separados de `estadisticos_ausentes`: no es lo
#' mismo no mandar la columna que mandarla sin datos, y la acción de quien la
#' preparó es distinta en cada caso. Dentro de una columna que sí trae datos, la
#' celda vacía se descarta por la misma razón, y el resto de las estimaciones se
#' publica.
#'
#' **Una celda que no es una estimación también se descarta, y se declara.** Un
#' texto que no es un número, o un número fuera del dominio de su estadístico
#' —un tamaño de muestra que no es un entero no negativo; un error estándar, un
#' coeficiente de variación, unos grados de libertad, un efecto de diseño o un
#' tamaño efectivo negativos— no se publica: se avisa y queda en el atributo
#' `celdas_descartadas`, con la métrica, la celda, el valor recibido y el motivo
#' (`no_numerica` o `fuera_de_dominio`). Una columna `factor` se lee por el texto
#' de sus niveles, no por sus códigos.
#'
#' @param estimaciones Data frame con una fila por estimación y una columna por
#'   estadístico. Los nombres reconocidos son los de
#'   [estadisticos_estimacion()]; se puede renombrar con `columnas`.
#' @param entidad Nombre de la entidad estimada, por ejemplo el tabulado o la
#'   población de referencia.
#' @param fuente Texto que declara quién calculó las estimaciones. Es
#'   obligatorio: sin él, el resultado no dice de dónde viene.
#' @param atributo Columna opcional de `estimaciones` que nombra el atributo o
#'   la celda estimada. Cuando falta, se numeran las filas.
#' @param columnas Vector con nombres para traducir columnas de `estimaciones` a
#'   estadísticos reconocidos, en la forma `c(cv = "coef_var")`.
#' @param fecha Fecha y hora de la medición.
#'
#' @return Data frame `medicion_calidad` con el contrato de [medir()], más las
#'   columnas `fuente` y `unidad`.
#' @export
#' @seealso [estadisticos_estimacion()], [evaluar()], [marco_cepal()]
#'
#' @examples
#' estimaciones <- data.frame(
#'   celda = c("Montevideo", "Interior"),
#'   stat = c(0.42, 0.38),
#'   cv = c(0.08, 0.34),
#'   n = c(1200L, 90L)
#' )
#' medicion_desde_estimaciones(
#'   estimaciones, entidad = "ech2024", atributo = "celda",
#'   fuente = "survey 4.4, diseno complejo declarado por el equipo"
#' )
medicion_desde_estimaciones <- function(estimaciones, entidad, fuente,
                                        atributo = NULL, columnas = NULL,
                                        fecha = Sys.time()) {
  if (!inherits(estimaciones, "data.frame") || !nrow(estimaciones)) {
    stop("`estimaciones` debe ser un data.frame no vacio.", call. = FALSE)
  }
  estimaciones <- .tabla_base(estimaciones)
  if (!.es_texto_escalar(entidad)) {
    stop("`entidad` debe ser una cadena no vacia.", call. = FALSE)
  }
  if (!.es_texto_escalar(fuente)) {
    stop(
      "`fuente` debe declarar quien calculo las estimaciones: sin eso, el ",
      "resultado no dice de donde viene.", call. = FALSE
    )
  }
  if (!is.null(columnas) &&
      (!is.character(columnas) || is.null(names(columnas)) ||
       anyNA(columnas) || !all(nzchar(columnas)))) {
    stop("`columnas` debe ser un vector con nombres.", call. = FALSE)
  }
  catalogo <- .catalogo_estimaciones()
  if (!is.null(columnas)) {
    desconocidos <- .identificadores_setdiff(
      names(columnas), catalogo$estadistico
    )
    if (length(desconocidos)) {
      stop(
        "`columnas` nombra estadisticos que no se reconocen: ",
        paste(desconocidos, collapse = ", "),
        ". Reconocidos: ", paste(catalogo$estadistico, collapse = ", "), ".",
        call. = FALSE
      )
    }
    indices_columnas <- .indice_nombre(unname(columnas), names(estimaciones))
    ausentes <- unname(columnas)[is.na(indices_columnas)]
    if (length(ausentes)) {
      stop(
        "`columnas` apunta a columnas que no estan en `estimaciones`: ",
        paste(ausentes, collapse = ", "), ".", call. = FALSE
      )
    }
    valores_resueltos <- names(estimaciones)[indices_columnas]
    columnas[!is.na(indices_columnas)] <- valores_resueltos[!is.na(indices_columnas)]
  }
  etiquetas <- if (is.null(atributo)) {
    sprintf("estimacion-%04d", seq_len(nrow(estimaciones)))
  } else {
    indice_atributo <- if (.es_texto_escalar(atributo)) {
      .indice_nombre(atributo, names(estimaciones))
    } else NA_integer_
    if (is.na(indice_atributo)) {
      stop(
        "`atributo` debe nombrar una columna de `estimaciones`. Disponibles: ",
        paste(names(estimaciones), collapse = ", "), ".", call. = FALSE
      )
    }
    as.character(estimaciones[[indice_atributo]])
  }

  origen_de <- function(estadistico) {
    if (!is.null(columnas)) {
      indice_columna <- .indice_identificador(estadistico, names(columnas))
      if (!is.na(indice_columna)) {
        return(unname(columnas[[indice_columna]]))
      }
    }
    indice <- .indice_nombre(estadistico, names(estimaciones))
    if (!is.na(indice)) return(names(estimaciones)[[indice]])
    NULL
  }
  presentes <- catalogo[
    vapply(catalogo$estadistico, function(x) !is.null(origen_de(x)),
           logical(1L)), , drop = FALSE
  ]
  # Una columna que EXISTE pero no trae ningun valor utilizable no es una
  # estimacion: publicaba una fila de medida con `resultado = NA`, que es
  # informar como medido lo que nadie midio, y ademas dejaba la medicion
  # inservible -`evaluar()` la rechaza entera por no respetar el tipo declarado-.
  # La promesa escrita es que los estadisticos que la tabla no traiga «no
  # producen medidas: no se rellenan con ceros ni se estiman», y una columna
  # vacia no los trae. Se declara aparte de los ausentes, porque no es lo mismo
  # no mandar la columna que mandarla sin datos: la accion del usuario es
  # distinta.
  con_valores <- vapply(presentes$estadistico, function(x) {
    valores <- .valores_estimacion(estimaciones[[origen_de(x)]])
    any(is.finite(valores))
  }, logical(1L))
  sin_valores <- presentes$estadistico[!con_valores]
  presentes <- presentes[con_valores, , drop = FALSE]
  if (!nrow(presentes)) {
    # Dos causas distintas y dos acciones distintas del usuario: o la columna no
    # esta, o esta y viene vacia. Decir "no trae ningun estadistico reconocido"
    # cuando SI se reconocieron las columnas manda a revisar los nombres, que es
    # justamente lo que ya estaba bien.
    if (length(sin_valores)) {
      stop(
        "`estimaciones` trae ", paste(sin_valores, collapse = ", "),
        " pero sin ningun valor utilizable: no hay nada que medir. Una columna ",
        "vacia no produce medidas, y no se rellena con ceros ni se estima.",
        call. = FALSE
      )
    }
    stop(
      "`estimaciones` no trae ningun estadistico reconocido. Reconocidos: ",
      paste(catalogo$estadistico, collapse = ", "), ".", call. = FALSE
    )
  }

  fecha <- as.POSIXct(fecha)
  id_medicion <- paste0(
    "estimaciones-", format(fecha, "%Y%m%dT%H%M%S"), "-", entidad
  )
  filas <- lapply(seq_len(nrow(presentes)), function(k) {
    definicion <- presentes[k, , drop = FALSE]
    crudos <- estimaciones[[origen_de(definicion$estadistico)]]
    valores <- .valores_estimacion(crudos)
    finitos <- is.finite(valores)
    en_dominio <- finitos &
      .en_dominio_estimacion(definicion$estadistico, valores) %in% TRUE
    # Una celda vacia se cae en silencio -lo documentado-; una con algo que no es
    # un numero, o con un numero fuera del dominio, se cae y se declara.
    presentes_crudos <- !is.na(crudos) & nzchar(trimws(as.character(crudos)))
    motivo_descarte <- ifelse(
      en_dominio | !presentes_crudos, NA_character_,
      ifelse(finitos, "fuera_de_dominio", "no_numerica")
    )
    data.frame(
      id_medida = sprintf(
        "%s-%s-%04d", id_medicion, definicion$metrica,
        seq_len(nrow(estimaciones))
      ),
      id_medicion = id_medicion,
      fecha = fecha,
      metrica = definicion$metrica,
      metrica_especifica = definicion$metrica,
      metrica_instanciada = paste0(definicion$metrica, "@", entidad),
      dimension = "Precision",
      factor = definicion$metrica,
      orientacion = definicion$orientacion,
      granularidad = "conjuntoEntidades",
      tipo_resultado = definicion$tipo_resultado,
      entidad = entidad,
      atributo = etiquetas,
      fila = seq_len(nrow(estimaciones)),
      objeto_medible = paste0(entidad, "$", etiquetas),
      resultado = valores,
      # `.fila_utilizable` y `.motivo_descarte` no viajan en la salida: marcan
      # las filas que se quedan y por que se cae cada una de las otras.
      .fila_utilizable = en_dominio,
      .motivo_descarte = motivo_descarte,
      .valor_crudo = as.character(crudos),
      agregacion = NA_character_,
      unidad = definicion$unidad,
      fuente = fuente,
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  # Y la celda vacia dentro de una columna que si trae datos tampoco es una
  # medida: se cae la fila, no se publica con `NA`.
  descartadas <- salida[!is.na(salida$.motivo_descarte), , drop = FALSE]
  celdas_descartadas <- data.frame(
    metrica = descartadas$metrica, atributo = descartadas$atributo,
    valor = descartadas$.valor_crudo, motivo = descartadas$.motivo_descarte,
    stringsAsFactors = FALSE
  )
  rownames(celdas_descartadas) <- NULL
  salida <- salida[salida$.fila_utilizable, , drop = FALSE]
  salida$.fila_utilizable <- NULL
  salida$.motivo_descarte <- NULL
  salida$.valor_crudo <- NULL
  rownames(salida) <- NULL
  if (nrow(celdas_descartadas)) {
    warning(
      "Se descartaron ", nrow(celdas_descartadas), " celda",
      if (nrow(celdas_descartadas) > 1L) "s" else "",
      " que no son una estimaci\u00f3n: ",
      sum(celdas_descartadas$motivo == "no_numerica"), " no num\u00e9rica",
      if (sum(celdas_descartadas$motivo == "no_numerica") != 1L) "s" else "",
      " y ", sum(celdas_descartadas$motivo == "fuera_de_dominio"),
      " fuera del dominio de su estad\u00edstico (un tama\u00f1o de muestra es un ",
      "entero no negativo; un error est\u00e1ndar, un coeficiente de variaci\u00f3n, ",
      "unos grados de libertad, un efecto de dise\u00f1o y un tama\u00f1o efectivo ",
      "no son negativos). Est\u00e1n en `attr(, \"celdas_descartadas\")`.",
      call. = FALSE
    )
  }
  attr(salida, "estadisticos_ausentes") <- .identificadores_setdiff(
    catalogo$estadistico, c(presentes$estadistico, sin_valores)
  )
  attr(salida, "estadisticos_sin_valores") <- sin_valores
  attr(salida, "celdas_descartadas") <- celdas_descartadas
  attr(salida, "fuente") <- fuente
  class(salida) <- c("medicion_calidad", "data.frame")
  salida
}
