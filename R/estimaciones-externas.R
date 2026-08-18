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
    desconocidos <- setdiff(names(columnas), catalogo$estadistico)
    if (length(desconocidos)) {
      stop(
        "`columnas` nombra estadisticos que no se reconocen: ",
        paste(desconocidos, collapse = ", "),
        ". Reconocidos: ", paste(catalogo$estadistico, collapse = ", "), ".",
        call. = FALSE
      )
    }
    ausentes <- setdiff(unname(columnas), names(estimaciones))
    if (length(ausentes)) {
      stop(
        "`columnas` apunta a columnas que no estan en `estimaciones`: ",
        paste(ausentes, collapse = ", "), ".", call. = FALSE
      )
    }
  }
  etiquetas <- if (is.null(atributo)) {
    sprintf("estimacion-%04d", seq_len(nrow(estimaciones)))
  } else {
    if (!.es_texto_escalar(atributo) || !atributo %in% names(estimaciones)) {
      stop(
        "`atributo` debe nombrar una columna de `estimaciones`. Disponibles: ",
        paste(names(estimaciones), collapse = ", "), ".", call. = FALSE
      )
    }
    as.character(estimaciones[[atributo]])
  }

  origen_de <- function(estadistico) {
    if (!is.null(columnas) && estadistico %in% names(columnas)) {
      return(unname(columnas[[estadistico]]))
    }
    if (estadistico %in% names(estimaciones)) return(estadistico)
    NULL
  }
  presentes <- catalogo[
    vapply(catalogo$estadistico, function(x) !is.null(origen_de(x)),
           logical(1L)), , drop = FALSE
  ]
  if (!nrow(presentes)) {
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
      agregacion = NA_character_,
      unidad = definicion$unidad,
      fuente = fuente,
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  rownames(salida) <- NULL
  attr(salida, "estadisticos_ausentes") <- setdiff(
    catalogo$estadistico, presentes$estadistico
  )
  attr(salida, "fuente") <- fuente
  class(salida) <- c("medicion_calidad", "data.frame")
  salida
}
