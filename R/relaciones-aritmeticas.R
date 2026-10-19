.es_columna_aritmetica <- function(x) {
  # `is.null(dim(x))`: una columna con dimensiones -una matriz o un arreglo
  # guardados en un `data.frame` con `I()`- pasa `is.numeric()` y entraba al
  # analisis. Dos consecuencias, las dos medidas:
  #   - `perfilar()` ABORTA cuando la tabla trae una matriz y un arreglo de otra
  #     dimension: `is.finite(x) & is.finite(y)` da "arreglos de dimension no
  #     compatibles". Un aborto con datos legitimos del usuario.
  #   - y con una sola matriz no aborta: publica una relacion aritmetica sobre
  #     una columna que el mismo perfil declara `tipo_compuesto_no_analizado`.
  # Una relacion aritmetica habla de magnitudes POR FILA, y una columna con
  # dimensiones no es una magnitud por fila. El paquete ya lo dice en la fila de
  # esa columna; esto lo hace valer en el analisis.
  # `!.es_columna_compuesta(x)` y no `is.null(dim(x))`: un arreglo de UNA
  # dimension -`I(array(x, 60))`- tiene `dim` y sigue siendo una magnitud por
  # fila, con una fila por elemento. Excluirlo por tener `dim` dejaba al paquete
  # con dos criterios otra vez: el perfil lo resumia como `doble` y Benford lo
  # analizaba, mientras el alcance aritmetico lo excluia. La pregunta es si tiene
  # MAS de una dimension, y esa pregunta ya esta escrita una sola vez.
  # `.es_numerico_pelado(x)` en vez de la lista `Date`/`POSIXt`/`difftime`/
  # `integer64` que estaba aca: la lista dejaba entrar a `lubridate::Period` y a
  # `units`, y con esas dos el cociente `respuesta / base` de
  # `.hallazgo_proporcional()` ABORTABA `perfilar()`. El criterio esta escrito
  # una sola vez, en `utils.R`, con el porque.
  .es_numerico_pelado(x) && !.es_columna_compuesta(x)
}

.texto_tolerancia_aritmetica <- function(tolerancia) {
  format(tolerancia, scientific = TRUE, trim = TRUE, digits = 15L)
}

.dentro_tolerancia_aritmetica <- function(observado, esperado, tolerancia) {
  abs(observado - esperado) <=
    tolerancia * pmax(1, abs(observado), abs(esperado))
}

.varia_columna_aritmetica <- function(x, tolerancia) {
  # En doble: `extremos[[2]] - extremos[[1]]` sobre un `integer` de -2e9 a 2e9
  # desborda a `NA`, y el `if` que lo consume aborta.
  x <- as.double(x[is.finite(x)])
  if (length(x) < 3L) return(FALSE)
  extremos <- range(x)
  (extremos[[2L]] - extremos[[1L]]) >
    tolerancia * max(1, abs(extremos))
}

.cumple_criterio_aritmetica <- function(proporcion, umbral) {
  is.finite(proporcion) && proporcion >= umbral
}

# **Un sumando que la tolerancia absorbe no se puede verificar.** La tolerancia
# es relativa a la escala del total, asi que con montos de 1e9 vale 10 a 20
# unidades, y cualquier columna que viva por debajo de eso "completa" una suma
# entre dos columnas casi iguales. Medido: `bruto` en 1e9..2e9, `neto = bruto -
# comision` con comision de 1 a 9, y una columna ajena `hijos` de 0 a 5; ademas
# de la relacion real se publicaba `[sospechoso] neto + hijos ~= bruto`, que
# acusaba de romper una regla inexistente a las cinco filas sanas donde `hijos`
# valia mas de 20. Con marcas epoch en segundos la tolerancia equivale a 17 s y
# `inicio + intentos ~= fin` salia `ok`.
#
# Es el problema que la relacion de orden ya resolvio con su filtro de magnitud:
# un orden explicado solo por escalas separadas no es una restriccion fila a
# fila, y se descarta salvo que una brecha exacta lo sostenga. Aca la pregunta
# equivalente es si la identidad se explica sola sin uno de sus sumandos: si
# `x ~= z` ya alcanza el criterio con la misma tolerancia, el tercer termino
# esta por debajo de la resolucion de la comparacion. Entonces se descarta,
# salvo que la identidad se cumpla **exacta** -como la brecha de IQR cero del
# orden, sin tolerancia oculta- en la proporcion que pide el criterio. Exacta
# quiere decir salvo el redondeo del tipo double: tres decimales leidos de un
# texto y sumados difieren del total en hasta dos unidades en la ultima cifra,
# y medido sobre decimales de dos cifras en escalas de 1e3 a 1e12 solo tres de
# cada cuatro filas daban diferencia cero, mientras todas quedaban dentro de dos
# epsilon relativos. Se usan cuatro. Lo descartado se cuenta en
# `identidades_descartadas_escala`.
.resolucion_exacta_aritmetica <- function() 4 * .Machine$double.eps

.exacta_salvo_redondeo <- function(observado, esperado) {
  abs(observado - esperado) <= .resolucion_exacta_aritmetica() *
    pmax(1, abs(observado), abs(esperado))
}

# Para cada sumando, si la identidad sin el ya alcanza el criterio.
.sumandos_absorbidos <- function(x, y, z, umbral, tolerancia) {
  c(
    .cumple_criterio_aritmetica(
      mean(.dentro_tolerancia_aritmetica(z, y, tolerancia)), umbral
    ),
    .cumple_criterio_aritmetica(
      mean(.dentro_tolerancia_aritmetica(z, x, tolerancia)), umbral
    )
  )
}

.texto_criterio_aritmetica <- function(umbral, min_filas) {
  paste0(
    "Criterio de reconocimiento declarado: cumplimiento >= ",
    format(umbral, scientific = FALSE, trim = TRUE, digits = 15L),
    " dentro de la tolerancia, con al menos ", min_filas,
    " filas comparables. Una vez reconocida la relaci\u00f3n, se informan ",
    "todas sus discrepancias. "
  )
}

.alcance_aritmetica_columnas <- function(datos, numericas, seleccion,
                                          umbral, min_filas, tolerancia,
                                          max_columnas) {
  n_numericas <- length(numericas)
  n_analizadas <- length(seleccion)
  # Las clases que quedaron fuera se MIDEN sobre esta tabla. Publicar la lista
  # fija `Date`/`POSIXt`/`difftime`/`integer64` era publicar el criterio viejo:
  # el de ahora no es una lista sino la pregunta de si la columna trae clase
  # declarada, y la lista fija no nombraba ni a `units` ni a `Period`.
  fuera <- setdiff(seq_along(datos), numericas)
  clases <- vapply(fuera, function(i) {
    x <- datos[[i]]
    if (is.numeric(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
      .clase_declarada_columna(x)
    } else {
      NA_character_
    }
  }, character(1L))
  clases_excluidas <- unique(clases[!is.na(clases)])
  # Las numericas con clase se declaran una por una en la cobertura: son las
  # que un lector esperaria ver analizadas -el perfil publica su minimo, su
  # maximo y su media- y la exclusion silenciosa las dejaba sin rastro.
  con_clase <- fuera[vapply(fuera, function(i) .es_numerica_con_clase(datos[[i]]),
                            logical(1L))]
  excluidas_por_clase <- stats::setNames(
    vapply(con_clase, function(i) .clase_declarada_columna(datos[[i]]),
           character(1L)),
    names(datos)[con_clase]
  )
  identidades <- function(n) if (n < 3L) 0 else 3 * choose(n, 3L)
  pares <- function(n) if (n < 2L) 0 else choose(n, 2L)
  list(
    filas_totales = as.numeric(nrow(datos)),
    columnas_numericas = names(datos)[numericas],
    columnas_analizadas = names(datos)[seleccion],
    columnas_omitidas = names(datos)[setdiff(numericas, seleccion)],
    identidades_aditivas_posibles = as.numeric(identidades(n_numericas)),
    identidades_aditivas_analizadas = as.numeric(identidades(n_analizadas)),
    pares_proporcionales_posibles = as.numeric(pares(n_numericas)),
    pares_proporcionales_analizados = as.numeric(pares(n_analizadas)),
    truncado = n_numericas > max_columnas,
    max_columnas = as.integer(max_columnas),
    umbral_cumplimiento = as.numeric(umbral),
    tolerancia = as.numeric(tolerancia),
    criterio_tolerancia = paste0(
      "|observado - esperado| <= tolerancia * ",
      "max(1, |observado|, |esperado|)"
    ),
    minimo_filas_comparables = as.integer(min_filas),
    identidades_descartadas_escala = 0,
    resolucion_exacta = .resolucion_exacta_aritmetica(),
    clases_excluidas = clases_excluidas,
    columnas_excluidas_por_clase = excluidas_por_clase
  )
}

.cobertura_aritmetica_columnas <- function(alcance) {
  # Sin filas suficientes la busqueda no puede correr. Callarse dejaria un
  # diagnostico ausente sin rastro, que es justo lo que el paquete no hace.
  #
  # Solo se declara cuando habia algo que buscar: si no hay combinaciones de
  # columnas numericas candidatas, no hay diagnostico que dejar de evaluar y
  # anunciarlo seria ruido en vez de alcance.
  filas <- list()
  habia_que_buscar <- alcance$identidades_aditivas_posibles > 0 ||
    alcance$pares_proporcionales_posibles > 0
  sin_filas <- habia_que_buscar &&
    is.finite(alcance$filas_totales) &&
    alcance$filas_totales < alcance$minimo_filas_comparables
  if (sin_filas) {
    faltante <- .nuevo_diagnostico_no_evaluado(
      "relacion_aritmetica_columnas",
      paste(alcance$columnas_numericas, collapse = ","),
      paste0(
        "No se buscaron relaciones aritmeticas: la tabla tiene ",
        alcance$filas_totales,
        if (identical(alcance$filas_totales, 1)) " fila" else " filas",
        " y se necesitan al menos ",
        alcance$minimo_filas_comparables, " filas comparables."
      ),
      paste0(
        "Perfilar una tabla con al menos ",
        alcance$minimo_filas_comparables,
        " filas, o bajar `min_filas_aritmetica`."
      )
    )
    if (!isTRUE(alcance$truncado)) filas <- c(filas, list(faltante))
  }
  filas <- c(filas, .cobertura_clases_aritmetica(alcance))
  if (!isTRUE(alcance$truncado)) {
    if (!length(filas)) return(.cobertura_diagnosticos_vacia())
    return(do.call(rbind, filas))
  }
  omitidas <- alcance$columnas_omitidas
  filas <- c(filas, list(.nuevo_diagnostico_no_evaluado(
    "relacion_aritmetica_columnas",
    paste(omitidas, collapse = ","),
    paste0(
      "La b\u00fasqueda aritm\u00e9tica se limit\u00f3 a las primeras ",
      alcance$max_columnas, " columnas num\u00e9ricas: analiz\u00f3 ",
      alcance$identidades_aditivas_analizadas, " de ",
      alcance$identidades_aditivas_posibles, " identidades aditivas y ",
      alcance$pares_proporcionales_analizados, " de ",
      alcance$pares_proporcionales_posibles, " pares proporcionales."
    ),
    paste0(
      "Aumentar `max_columnas_aritmetica` o perfilar por bloques si las ",
      "columnas omitidas deben intervenir en el diagn\u00f3stico."
    )
  )))
  do.call(rbind, filas)
}

.cobertura_clases_aritmetica <- function(alcance) {
  # Una columna que el perfil resume como numero -publica su minimo, su maximo
  # y su media- y que no entra a este diagnostico tiene que decir por que. Sin
  # esto, la ausencia de relacion aritmetica sobre una columna `units` o
  # `integer64` se leia como "se busco y no hay".
  excluidas <- alcance$columnas_excluidas_por_clase
  if (!length(excluidas)) return(list())
  lapply(seq_along(excluidas), function(i) {
    .nuevo_diagnostico_no_evaluado(
      "relacion_aritmetica_columnas",
      names(excluidas)[[i]],
      paste0(
        "La columna declara la clase `", excluidas[[i]],
        "` y la b\u00fasqueda de relaciones aritm\u00e9ticas s\u00f3lo opera sobre ",
        "n\u00fameros sin clase: sumar o dividir valores de esa clase no es la ",
        "aritm\u00e9tica de un doble."
      ),
      paste0(
        "No leer la ausencia de relaci\u00f3n como ausencia de relaci\u00f3n. Para ",
        "evaluarla, convertir la columna a `double` -por ejemplo con ",
        "`as.numeric()`- si su magnitud entra en ese tipo."
      )
    )
  })
}

.evidencia_filas_aritmetica <- function(datos, indices, columnas, esperado,
                                         etiqueta_esperado,
                                         nombres_texto = names(datos)) {
  ejemplos <- utils::head(indices, 5L)
  if (!length(ejemplos)) return("sin filas discrepantes")
  paste(vapply(seq_along(ejemplos), function(i) {
    fila <- ejemplos[[i]]
    valores <- paste(vapply(columnas, function(columna) {
      paste0(nombres_texto[[columna]], "=",
             .texto_valor(datos[[columna]][fila]))
    }, character(1L)), collapse = "; ")
    paste0(
      "fila ", fila, ": ", valores, "; ", etiqueta_esperado, "=",
      .texto_valor(esperado[[i]])
    )
  }, character(1L)), collapse = " | ")
}

.hallazgo_identidad_aditiva <- function(datos, sumandos, total, umbral,
                                         min_filas, tolerancia,
                                         nombres_texto = names(datos)) {
  # En doble y no en el tipo guardado: dos `integer` que suman mas de
  # `.Machine$integer.max` daban `NA`, `mean(cumple)` daba `NA` y la relacion
  # desaparecia sin fila de cobertura, con el aviso base de enteros excedidos
  # filtrado al usuario. Medido: `total == a + b` en las 600 filas y cero
  # relaciones porque una fila desbordaba; por DBI, RSQLite devuelve `INTEGER`
  # como `integer` y el mismo importe en centesimos se callaba en 81 de 500
  # filas. Que la relacion exista no puede depender del tipo de
  # almacenamiento, que en una base lo decide el controlador. Un `integer`
  # entra exacto en un doble.
  x <- as.double(datos[[sumandos[[1L]]]])
  y <- as.double(datos[[sumandos[[2L]]]])
  z <- as.double(datos[[total]])
  comparables <- is.finite(x) & is.finite(y) & is.finite(z)
  n_evaluados <- sum(comparables)
  if (n_evaluados < min_filas ||
      !all(vapply(list(x[comparables], y[comparables], z[comparables]),
                  .varia_columna_aritmetica, logical(1L),
                  tolerancia = tolerancia))) {
    return(NULL)
  }
  esperado <- x[comparables] + y[comparables]
  cumple <- .dentro_tolerancia_aritmetica(
    z[comparables], esperado, tolerancia
  )
  proporcion <- mean(cumple)
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  absorbidos <- .sumandos_absorbidos(
    x[comparables], y[comparables], z[comparables], umbral, tolerancia
  )
  texto_escala <- ""
  if (any(absorbidos)) {
    exactas <- .exacta_salvo_redondeo(z[comparables], esperado)
    if (!.cumple_criterio_aritmetica(mean(exactas), umbral)) {
      return("descartada_escala")
    }
    texto_escala <- paste0(
      "Con la tolerancia declarada, ",
      paste0("`", nombres_texto[sumandos[absorbidos]], "`", collapse = " y "),
      " no se distingue de cero frente a `", nombres_texto[[total]],
      "`: la identidad sin ese sumando ya alcanza el criterio. Se publica ",
      "porque se cumple exacta, salvo el redondeo del tipo double, en ",
      sum(exactas), " de ", n_evaluados, " filas comparables. "
    )
  }
  filas_comparables <- which(comparables)
  indices_incumplen <- filas_comparables[!cumple]
  esperados_incumplen <- esperado[!cumple]
  nombres <- names(datos)
  expresion <- paste0(
    nombres_texto[[sumandos[[1L]]]], " + ",
    nombres_texto[[sumandos[[2L]]]], " ~= ", nombres_texto[[total]]
  )
  alternativa <- paste0(
    nombres_texto[[sumandos[[1L]]]], " ~= ", nombres_texto[[total]], " - ",
    nombres_texto[[sumandos[[2L]]]]
  )
  tolerancia_texto <- .texto_tolerancia_aritmetica(tolerancia)
  ejemplos <- .evidencia_filas_aritmetica(
    datos, indices_incumplen, c(sumandos, total), esperados_incumplen,
    paste0(nombres_texto[[total]], " esperado"), nombres_texto
  )
  hallazgo <- .nuevo_hallazgo(
    paste(nombres[c(sumandos, total)], collapse = ","),
    "relacion_aritmetica_columnas",
    if (length(indices_incumplen)) "sospechoso" else "ok",
    paste0(
      "Se observ\u00f3 la regularidad aritm\u00e9tica ", expresion,
      if (length(indices_incumplen)) {
        " y se rompe en una minor\u00eda de las filas comparables."
      } else " en todas las filas comparables."
    ),
    paste0(
      .formatear_decimal_publicado(proporcion),
      " de cumplimiento; ", length(indices_incumplen), " de ", n_evaluados,
      " filas discrepantes (universo: ", n_evaluados, " de ", nrow(datos),
      " filas con valores finitos en las tres columnas). ",
      .texto_criterio_aritmetica(umbral, min_filas),
      "Forma equivalente observada: ", alternativa, ". ",
      "Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ", tolerancia_texto,
      " * max(1, |observado|, |esperado|). ", texto_escala, ejemplos
    ),
    paste0(
      "Evaluar si la regularidad observada corresponde a una regla del ",
      "dominio; ",
      if (length(indices_incumplen)) {
        "revisar las filas se\u00f1aladas antes de corregirlas."
      } else "no convertirla en regla sin confirmaci\u00f3n del dominio."
    ),
    n_evaluados, length(indices_incumplen), "fila"
  )
  hallazgo$trazabilidad[[1L]] <- .trazabilidad_indices(
    indices_incumplen, "filas_finitas_en_las_columnas_involucradas",
    limite = Inf
  )
  attr(hallazgo, "columnas_aritmetica") <- list(
    sumandos = sumandos, total = total
  )
  hallazgo
}

.hallazgo_proporcional <- function(datos, par, umbral, min_filas,
                                    tolerancia, nombres_texto = names(datos)) {
  # Sin `as.double()`, a diferencia de la identidad aditiva: aca no hay suma de
  # enteros -el cociente y `base * constante` ya salen en doble- y no hay nada
  # que desborde.
  primero <- datos[[par[[1L]]]]
  segundo <- datos[[par[[2L]]]]
  base <- primero
  respuesta <- segundo
  indice_base <- par[[1L]]
  indice_respuesta <- par[[2L]]
  utilizables <- is.finite(base) & is.finite(respuesta) & base != 0
  if (sum(utilizables) < min_filas) return(NULL)
  constante <- stats::median(respuesta[utilizables] / base[utilizables])
  if (!is.finite(constante) || constante == 0) return(NULL)
  if (abs(constante) > 1) {
    base <- segundo
    respuesta <- primero
    indice_base <- par[[2L]]
    indice_respuesta <- par[[1L]]
    utilizables <- is.finite(base) & is.finite(respuesta) & base != 0
    if (sum(utilizables) < min_filas) return(NULL)
    constante <- stats::median(respuesta[utilizables] / base[utilizables])
  }
  if (!is.finite(constante) ||
      abs(abs(constante) - 1) <= tolerancia ||
      !.varia_columna_aritmetica(base[utilizables], tolerancia) ||
      !.varia_columna_aritmetica(respuesta[utilizables], tolerancia)) {
    return(NULL)
  }
  comparables <- is.finite(base) & is.finite(respuesta)
  n_evaluados <- sum(comparables)
  if (n_evaluados < min_filas) return(NULL)
  esperado <- base[comparables] * constante
  cumple <- .dentro_tolerancia_aritmetica(
    respuesta[comparables], esperado, tolerancia
  )
  proporcion <- mean(cumple)
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  filas_comparables <- which(comparables)
  indices_incumplen <- filas_comparables[!cumple]
  esperados_incumplen <- esperado[!cumple]
  nombres <- names(datos)
  constante_texto <- format(
    constante, scientific = FALSE, trim = TRUE, digits = 15L
  )
  expresion <- paste0(
    nombres_texto[[indice_respuesta]], " ~= ", nombres_texto[[indice_base]], " * ",
    constante_texto
  )
  inversa <- paste0(
    nombres_texto[[indice_base]], " ~= ", nombres_texto[[indice_respuesta]], " / ",
    constante_texto
  )
  tolerancia_texto <- .texto_tolerancia_aritmetica(tolerancia)
  ejemplos <- .evidencia_filas_aritmetica(
    datos, indices_incumplen, c(indice_base, indice_respuesta),
    esperados_incumplen, paste0(nombres_texto[[indice_respuesta]], " esperado"),
    nombres_texto
  )
  hallazgo <- .nuevo_hallazgo(
    paste(nombres[c(indice_base, indice_respuesta)], collapse = ","),
    "relacion_aritmetica_columnas",
    if (length(indices_incumplen)) "sospechoso" else "ok",
    paste0(
      "Se observ\u00f3 la regularidad proporcional ", expresion,
      if (length(indices_incumplen)) {
        " y se rompe en una minor\u00eda de las filas comparables."
      } else " en todas las filas comparables."
    ),
    paste0(
      .formatear_decimal_publicado(proporcion),
      " de cumplimiento; ", length(indices_incumplen), " de ", n_evaluados,
      " filas discrepantes (universo: ", n_evaluados, " de ", nrow(datos),
      " filas con valores finitos en ambas columnas). ",
      .texto_criterio_aritmetica(umbral, min_filas),
      "Constante observada k=", constante_texto,
      " (mediana de respuesta/base en ", sum(utilizables),
      " filas finitas con base distinta de cero); forma equivalente: ",
      inversa, ". Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ", tolerancia_texto,
      " * max(1, |observado|, |esperado|). ", ejemplos
    ),
    paste0(
      "Evaluar si la proporcionalidad observada corresponde a una regla del ",
      "dominio; ",
      if (length(indices_incumplen)) {
        "revisar las filas se\u00f1aladas antes de corregirlas."
      } else "no convertirla en regla sin confirmaci\u00f3n del dominio."
    ),
    n_evaluados, length(indices_incumplen), "fila"
  )
  hallazgo$trazabilidad[[1L]] <- .trazabilidad_indices(
    indices_incumplen, "filas_finitas_en_las_columnas_involucradas",
    limite = Inf
  )
  hallazgo
}

.detectar_aritmetica_columnas <- function(datos, umbral = 0.9,
                                           min_filas = 3L,
                                           tolerancia = 1e-8,
                                           max_columnas = 20L,
                                           nombres_texto = names(datos),
                                           columnas_codigo = character()) {
  numericas <- which(vapply(datos, .es_columna_aritmetica, logical(1L)))
  # Una relacion aritmetica habla de magnitudes por fila, y una columna declarada
  # en `columnas_codigo` no lo es: `cod_localidad = cod_depto * 100` se publicaba
  # como "regularidad proporcional" igual con las dos declaradas (refutacion
  # 34-D). Sale del analisis como sale de Tukey y de Benford, y se declara.
  codigos <- numericas[.nombres_para_operar(names(datos)[numericas]) %in%
                         .nombres_para_operar(as.character(columnas_codigo))]
  numericas <- setdiff(numericas, codigos)
  seleccion <- utils::head(numericas, max_columnas)
  alcance <- .alcance_aritmetica_columnas(
    datos, numericas, seleccion, umbral, min_filas, tolerancia, max_columnas
  )
  alcance$columnas_excluidas_codigo <- names(datos)[codigos]
  cobertura <- .cobertura_aritmetica_columnas(alcance)
  if (length(codigos)) {
    cobertura <- rbind(cobertura, do.call(rbind, lapply(codigos, function(i) {
      .nuevo_diagnostico_no_evaluado(
        "relacion_aritmetica_columnas", names(datos)[[i]],
        paste(
          "No se buscaron relaciones aritmeticas con la columna: se declaro en",
          "`columnas_codigo`, asi que es un codigo y no una magnitud por fila."
        ),
        paste(
          "Si la columna es una magnitud y no un codigo, quitarla de",
          "`columnas_codigo`."
        )
      )
    })))
  }
  hallazgos_aditivos <- list()
  objetivos_aditivos <- list()
  if (length(seleccion) >= 3L && nrow(datos)) {
    ternas <- utils::combn(seleccion, 3L, simplify = FALSE)
    for (terna in ternas) {
      candidatos <- lapply(seq_len(3L), function(objetivo) {
        .hallazgo_identidad_aditiva(
          datos, terna[-objetivo], terna[[objetivo]], umbral,
          min_filas, tolerancia, nombres_texto
        )
      })
      descartadas <- vapply(candidatos, is.character, logical(1L))
      alcance$identidades_descartadas_escala <-
        alcance$identidades_descartadas_escala + sum(descartadas)
      candidatos <- candidatos[!descartadas]
      candidatos <- Filter(Negate(is.null), candidatos)
      if (!length(candidatos)) next
      afectados <- vapply(candidatos, function(x) x$n_afectados[[1L]], numeric(1L))
      elegido <- candidatos[[which.min(afectados)]]
      columnas_elegidas <- attr(elegido, "columnas_aritmetica", exact = TRUE)
      attr(elegido, "columnas_aritmetica") <- NULL
      hallazgos_aditivos[[length(hallazgos_aditivos) + 1L]] <- elegido
      objetivos_aditivos[[length(objetivos_aditivos) + 1L]] <- columnas_elegidas
    }
  }

  hallazgos_proporcionales <- list()
  if (length(seleccion) >= 2L && nrow(datos)) {
    pares <- utils::combn(seleccion, 2L, simplify = FALSE)
    for (par in pares) {
      # Si una identidad aditiva ya vincula la terna, se omiten las dos
      # proporcionalidades redundantes que incluyen su total. La relación
      # entre los sumandos se conserva (por ejemplo, iva = neto * 0.22).
      redundante <- any(vapply(objetivos_aditivos, function(x) {
        x$total %in% par && any(x$sumandos %in% par)
      }, logical(1L)))
      if (redundante) next
      hallazgo <- .hallazgo_proporcional(
        datos, par, umbral, min_filas, tolerancia, nombres_texto
      )
      if (!is.null(hallazgo)) {
        hallazgos_proporcionales[[length(hallazgos_proporcionales) + 1L]] <-
          hallazgo
      }
    }
  }
  list(
    hallazgos = c(hallazgos_aditivos, hallazgos_proporcionales),
    alcance = alcance,
    cobertura = cobertura
  )
}
