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
    tipo_resultado = c(
      "real", "real", "real", "entero", "real", "real", "real"
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
    valores <- suppressWarnings(as.numeric(estimaciones[[origen_de(x)]]))
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
    valores <- estimaciones[[origen_de(definicion$estadistico)]]
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
      resultado = as.numeric(valores),
      # `.fila_utilizable` no viaja en la salida: marca las filas que se quedan.
      .fila_utilizable = is.finite(suppressWarnings(as.numeric(valores))),
      agregacion = NA_character_,
      unidad = definicion$unidad,
      fuente = fuente,
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  # Y la celda vacia dentro de una columna que si trae datos tampoco es una
  # medida: se cae la fila, no se publica con `NA`.
  salida <- salida[salida$.fila_utilizable, , drop = FALSE]
  salida$.fila_utilizable <- NULL
  rownames(salida) <- NULL
  attr(salida, "estadisticos_ausentes") <- .identificadores_setdiff(
    catalogo$estadistico, c(presentes$estadistico, sin_valores)
  )
  attr(salida, "estadisticos_sin_valores") <- sin_valores
  attr(salida, "fuente") <- fuente
  class(salida) <- c("medicion_calidad", "data.frame")
  salida
}
