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

# `cota` es un tope absoluto para la tolerancia relativa: ver
# `.cota_cuanto_aritmetica()`. Con `Inf` -lo que llega cuando no hay cota- el
# `pmin()` devuelve la tolerancia relativa sin tocarla, bit a bit.
#
# `escala` es la magnitud de los operandos de una suma, por fila. **El error de
# sumar en doble es relativo a los sumandos, no al resultado**: con
# `a = 1e9 + x`, `b = -1e9 + y` y `c = x + y`, la suma `a + b` arrastra el
# redondeo de 1e9 -hasta 1,1e-7- y la tolerancia anclada al total, que vale
# unas centenas o menos, acusaba 16 de 300 filas de una identidad verdadera. Con
# `0` -lo que llega fuera de las identidades aditivas- no cambia nada.
.dentro_tolerancia_aritmetica <- function(observado, esperado, tolerancia,
                                          cota = Inf, escala = 0) {
  abs(observado - esperado) <=
    pmin(tolerancia * pmax(1, abs(observado), abs(esperado), escala), cota)
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

.exacta_salvo_redondeo <- function(observado, esperado, escala = 0) {
  abs(observado - esperado) <= .resolucion_exacta_aritmetica() *
    pmax(1, abs(observado), abs(esperado), escala)
}

# **La tolerancia relativa absorbia centavos en los importes grandes.** Con
# `1e-8` relativo, una identidad sobre importes de 1e8 tolera un peso entero:
# medido, `comision + neto ~= bruto` con 30 de 300 filas rotas por 1, -1 o 30
# centavos publicaba `sospechoso 30` con importes de 1e4, `sospechoso 21` con
# 1e6 y `ok 0` -"en todas las filas comparables"- con 1e8. El mismo dato roto
# era roto o sano segun la escala.
#
# Lo que la tolerancia tiene que absorber es el error de representacion, no
# una diferencia que esta en el dato. Cuando las tres columnas de una identidad
# aditiva guardan decimales fijos -todos sus valores son multiplos de un mismo
# cuanto decimal `q`, centavos o enteros-, la diferencia entre el total y la
# suma es un multiplo de `q` salvo el redondeo del doble, asi que cualquier
# diferencia de `q / 2` o mas es una diferencia del dato. Esa mitad acota la
# tolerancia relativa, que sigue mandando donde es menor.
#
# No se reemplaza la tolerancia por una mas chica porque no hay una que sirva
# en todas las escalas (medido en `1e2` a `1e12`, quince formas de relacion
# verdadera y rota): cuatro epsilon relativos -la resolucion exacta de arriba-
# rompen 34 a 49 de 400 filas sanas de una suma que paso por un CSV de 15
# cifras, y `1e-12` deja de publicar `iva = round(neto * 0.22, 2)` sobre
# importes de 1e8, que la tolerancia de siempre publica bien.
#
# Tres condiciones, y las tres se midieron:
#   - **solo con la tolerancia por omision.** Una tolerancia declarada es una
#     decision del usuario -por ejemplo absorber el redondeo independiente de
#     los tres importes- y la cota se la quitaria;
#   - **solo identidades aditivas.** En una proporcionalidad el esperado
#     `base * k` no cae en la reticula del cuanto y `k` se estima con la
#     mediana: con la cota, `iva = round(neto * 0.22, 2)` sobre 1e8 ganaba 8
#     filas rotas falsas, porque `k` difiere de 0,22 en 4,5e-12 y por `base`
#     eso suma 4e-4 a una diferencia que ya llegaba a `q / 2`;
#   - **solo si el total guarda exactamente los decimales de la suma, o si la
#     identidad se cumple exacta en la mayoria de las filas.** Tres columnas
#     redondeadas cada una a nueve u once cifras significativas tambien son
#     multiplos de un cuanto -el de sus valores mas chicos- y su suma difiere en
#     hasta unidades de la ultima cifra de los mas grandes: con la cota dejaban
#     de publicarse. Ahi el total tiene menos decimales que un sumando (vive en
#     una decada mas alta) o mas (en una cancelacion), y se cumplen exactas 8 a
#     12 de cada 100 filas. Pedir solo la mayoria exacta dejaba afuera a los
#     decimales fijos con mas de la mitad de las filas rotas: con 153 de 300
#     filas corridas un centavo sobre importes de 1e7, la identidad salia `ok`
#     con "0 filas discrepantes". Con centavos en las tres columnas el total
#     tiene los decimales de la suma, y la cota se aplica igual.
# El cuanto se reconoce solo si los valores tienen menos de 15 cifras
# significativas en ese cuanto: 15 son las de un texto exportado -R y las
# planillas escriben 15-, y ahi la ultima cifra es el redondeo del texto y no un
# decimal guardado; ademas, desde unas 1e14 unidades del cuanto la comparacion
# en doble ya no distingue un multiplo de lo que no lo es. El piso anterior -1024
# veces la resolucion exacta, unas doce cifras- dejaba los centavos sin cota
# desde importes de 1,1e10, y la tolerancia relativa volvia a tragarlos: 30 filas
# rotas por un centavo salian `ok` sobre importes de 7e9 a 1e12. Ahora los
# centavos se reconocen hasta 1e12 y los enteros hasta 1e14; por encima la
# evidencia dice que la tolerancia no se acota y cuanto vale.
.TOLERANCIA_ARITMETICA_OMISION <- 1e-8

# El cuanto mas grueso del que todos los valores son multiplos, de 10^k a
# 10^-15. Tambien los gruesos -10, 100-: nueve cifras significativas sobre 1e11
# son multiplos de 100, y detenerse en el entero las hacia parecer columnas con
# el mismo cuanto que su total (ver la regla del total arriba).
.CIFRAS_MAXIMAS_CUANTO <- 1e14

.cuanto_decimal_aritmetica <- function(x) {
  x <- as.double(x[is.finite(x)])
  if (!length(x)) return(NA_real_)
  magnitud <- max(1, abs(x))
  # Una cabeza corta descarta enseguida las columnas sin cuanto -casi todas las
  # de dobles calculados- sin recorrer la columna entera por cada decimal.
  cabeza <- utils::head(x, 64L)
  # Por una potencia de diez exacta: con decimales negativos se divide, porque
  # `0.1` no es exacto en doble y `v * 0.1` correria el resultado.
  en_reticula <- function(v, decimales) {
    w <- if (decimales >= 0L) v * 10^decimales else v / 10^(-decimales)
    all(abs(w - round(w)) <= 8 * .Machine$double.eps * pmax(1, abs(w)))
  }
  for (decimales in seq.int(-floor(log10(magnitud)), 15L)) {
    cuanto <- 10^-decimales
    if (magnitud / cuanto >= .CIFRAS_MAXIMAS_CUANTO) return(NA_real_)
    if (en_reticula(cabeza, decimales) && en_reticula(x, decimales)) {
      return(cuanto)
    }
  }
  NA_real_
}

# La cota de una identidad aditiva: la mitad del cuanto mas fino de sus tres
# columnas, o `Inf` si alguna no tiene cuanto reconocido. `cuantos` va en el
# orden sumando, sumando, total. `total_en_reticula` dice si el total tiene
# exactamente los decimales de la suma: ni menos -un total redondeado mas
# grueso que un sumando- ni mas -un total mas fino que lo que la suma puede
# dar-. Devuelve tambien el cuanto, para la evidencia.
.cota_cuanto_aritmetica <- function(cuantos) {
  if (length(cuantos) != 3L || anyNA(cuantos)) {
    return(list(cuanto = NA_real_, cota = Inf, total_en_reticula = FALSE))
  }
  cuanto <- min(cuantos)
  list(
    cuanto = cuanto, cota = cuanto / 2,
    total_en_reticula = abs(cuantos[[3L]] / min(cuantos[1:2]) - 1) < 1e-9
  )
}

# Para cada sumando, si la identidad sin el ya alcanza el criterio.
.sumandos_absorbidos <- function(x, y, z, umbral, tolerancia, cota = Inf) {
  c(
    .cumple_criterio_aritmetica(
      mean(.dentro_tolerancia_aritmetica(z, y, tolerancia, cota)), umbral
    ),
    .cumple_criterio_aritmetica(
      mean(.dentro_tolerancia_aritmetica(z, x, tolerancia, cota)), umbral
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
    # Las que la tolerancia relativa reconocia y la cota del cuanto no: ver
    # `.hallazgo_identidad_aditiva()`.
    identidades_descartadas_cuanto = 0,
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
        "No se buscaron relaciones aritm\u00e9ticas: la tabla tiene ",
        alcance$filas_totales,
        if (identical(alcance$filas_totales, 1)) " fila" else " filas",
        " y se necesitan al menos ",
        alcance$minimo_filas_comparables,
        if (alcance$minimo_filas_comparables == 1L) " fila comparable." else " filas comparables."
      ),
      paste0(
        "Perfilar una tabla con al menos ",
        alcance$minimo_filas_comparables,
        if (alcance$minimo_filas_comparables == 1L) " fila, o bajar `min_filas_aritmetica`." else
          " filas, o bajar `min_filas_aritmetica`."
      ),
      columnas = alcance$columnas_numericas
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
      alcance$max_columnas, if (alcance$max_columnas == 1L) " columna num\u00e9rica: analiz\u00f3 " else " columnas num\u00e9ricas: analiz\u00f3 ",
      alcance$identidades_aditivas_analizadas, " de ",
      alcance$identidades_aditivas_posibles,
      if (alcance$identidades_aditivas_posibles == 1L) " identidad aditiva y " else " identidades aditivas y ",
      alcance$pares_proporcionales_analizados, " de ",
      alcance$pares_proporcionales_posibles,
      if (alcance$pares_proporcionales_posibles == 1L) " par proporcional." else " pares proporcionales."
    ),
    paste0(
      "Aumentar `max_columnas_aritmetica` o perfilar por bloques si las ",
      "columnas omitidas deben intervenir en el diagn\u00f3stico."
    ),
    columnas = omitidas
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
                                         nombres_texto = names(datos),
                                         cuantos = NULL) {
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
  # La magnitud de los operandos: ver `.dentro_tolerancia_aritmetica()`. Entra
  # solo donde supera al total y al esperado, o sea donde hay cancelacion.
  escala <- pmax(abs(x[comparables]), abs(y[comparables]))
  cumple <- .dentro_tolerancia_aritmetica(
    z[comparables], esperado, tolerancia, escala = escala
  )
  tolerancia_relativa <- tolerancia *
    pmax(1, abs(z[comparables]), abs(esperado), escala)
  # La cota del cuanto, con las tres condiciones de
  # `.TOLERANCIA_ARITMETICA_OMISION`. Solo se usa si achica la tolerancia en
  # alguna fila: si no, el criterio aplicado es el relativo de siempre y la
  # evidencia lo dice igual que antes.
  cuanto <- .cota_cuanto_aritmetica(cuantos)
  cota <- Inf
  mayoria_exacta <- FALSE
  if (is.finite(cuanto$cota) && any(cumple) &&
      any(tolerancia_relativa > cuanto$cota)) {
    exactas_relativas <- .exacta_salvo_redondeo(
      z[comparables], esperado, escala
    ) & cumple
    mayoria_exacta <- sum(exactas_relativas) > sum(cumple) / 2
    if (isTRUE(cuanto$total_en_reticula) || mayoria_exacta) {
      cota <- cuanto$cota
      reconocida_sin_cota <- .cumple_criterio_aritmetica(mean(cumple), umbral)
      cumple <- .dentro_tolerancia_aritmetica(
        z[comparables], esperado, tolerancia, cota, escala
      )
      # La tolerancia relativa la reconocia y la cota no: la regularidad se
      # sostenia solo porque a esa escala la tolerancia traga el cuanto. Se
      # descarta y se cuenta, como las de un sumando absorbido.
      if (reconocida_sin_cota &&
          !.cumple_criterio_aritmetica(mean(cumple), umbral)) {
        return("descartada_cuanto")
      }
    }
  }
  proporcion <- mean(cumple)
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  absorbidos <- .sumandos_absorbidos(
    x[comparables], y[comparables], z[comparables], umbral, tolerancia, cota
  )
  # Lo mismo del lado del total: con la tolerancia anclada a los operandos, un
  # total que vive por debajo de ella se cumple con cualquier valor chico, como
  # un sumando absorbido. Si la identidad tambien vale con el total en cero, se
  # publica solo si es exacta.
  total_absorbido <- .cumple_criterio_aritmetica(mean(
    .dentro_tolerancia_aritmetica(0, esperado, tolerancia, cota, escala)
  ), umbral)
  texto_escala <- ""
  if (any(absorbidos) || total_absorbido) {
    exactas <- .exacta_salvo_redondeo(z[comparables], esperado, escala)
    if (!.cumple_criterio_aritmetica(mean(exactas), umbral)) {
      return("descartada_escala")
    }
    texto_escala <- paste0(
      "Con la tolerancia declarada, ",
      if (any(absorbidos)) {
        paste0(
          paste0("`", nombres_texto[sumandos[absorbidos]], "`", collapse = " y "),
          " no se distingue de cero frente a `", nombres_texto[[total]],
          "`: la identidad sin ese sumando ya alcanza el criterio. "
        )
      },
      if (total_absorbido) {
        paste0(
          if (any(absorbidos)) "Y " else "",
          "`", nombres_texto[[total]], "` no se distingue de cero frente a ",
          "los sumandos: la identidad con el total en cero ya alcanza el ",
          "criterio. "
        )
      },
      "Se publica porque se cumple exacta, salvo el redondeo del tipo double, en ",
      sum(exactas), " de ", n_evaluados,
      if (n_evaluados == 1L) " fila comparable. " else " filas comparables. "
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
  # Los sumandos entran en la magnitud solo donde la mueven: si no, el texto
  # es el de siempre.
  texto_magnitud <- if (any(escala > pmax(1, abs(z[comparables]),
                                          abs(esperado)))) {
    "max(1, |observado|, |esperado|, |sumandos|)"
  } else "max(1, |observado|, |esperado|)"
  # Sin cota y sobre magnitudes donde un decimal guardado ya tiene 15 cifras
  # significativas, la tolerancia relativa puede tragar diferencias que estan
  # en el dato, y la evidencia lo dice con su valor.
  texto_limite <- ""
  magnitud_maxima <- max(abs(c(x[comparables], y[comparables],
                               z[comparables])))
  if (identical(as.double(tolerancia), .TOLERANCIA_ARITMETICA_OMISION) &&
      magnitud_maxima >= .CIFRAS_MAXIMAS_CUANTO * 0.01) {
    texto_limite <- paste0(
      "No se acota por un cuanto decimal: con valores de ",
      .formatear_numero_publicado(signif(magnitud_maxima, 3)),
      " un decimal de dos cifras ya tiene 15 cifras significativas, las de un ",
      "texto exportado, y no se distingue de su redondeo; la tolerancia llega ",
      "a ", .formatear_numero_publicado(signif(max(tolerancia_relativa), 3)),
      " en estas filas, y una diferencia menor no se cuenta como discrepancia. "
    )
  }
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
      if (length(indices_incumplen) == 1L) " fila discrepante (universo: " else " filas discrepantes (universo: ",
      n_evaluados, " de ", nrow(datos),
      if (nrow(datos) == 1L) " fila con valores finitos en las tres columnas). " else
        " filas con valores finitos en las tres columnas). ",
      .texto_criterio_aritmetica(umbral, min_filas),
      "Forma equivalente observada: ", alternativa, ". ",
      "Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ",
      if (is.finite(cota)) {
        paste0(
          "min(", tolerancia_texto, " * ", texto_magnitud, ", ",
          .formatear_numero_publicado(cota), "): la cota es la mitad del ",
          "cuanto decimal com\u00fan de las tres columnas (",
          .formatear_numero_publicado(cuanto$cuanto), "), porque entre valores ",
          "con ese cuanto una diferencia es un m\u00faltiplo de \u00e9l, y ",
          if (mayoria_exacta) {
            "la identidad se cumple exacta en la mayor\u00eda de las filas. "
          } else {
            "el total guarda exactamente los decimales de la suma. "
          }
        )
      } else {
        paste0(tolerancia_texto, " * ", texto_magnitud, ". ", texto_limite)
      },
      texto_escala, ejemplos
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
  hallazgo$trazabilidad[[1L]]$columnas <- nombres[c(sumandos, total)]
  attr(hallazgo, "columnas_aritmetica") <- list(
    sumandos = sumandos, total = total, cota_cuanto = is.finite(cota)
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
      if (length(indices_incumplen) == 1L) " fila discrepante (universo: " else " filas discrepantes (universo: ",
      n_evaluados, if (n_evaluados == 1L) " de " else " de ", nrow(datos),
      if (nrow(datos) == 1L) " fila con valores finitos en ambas columnas). " else
        " filas con valores finitos en ambas columnas). ",
      .texto_criterio_aritmetica(umbral, min_filas),
      "Constante observada k=", constante_texto,
      " (mediana de respuesta/base en ", sum(utilizables),
      if (sum(utilizables) == 1L) " fila finita con base distinta de cero); forma equivalente: " else
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
  hallazgo$trazabilidad[[1L]]$columnas <-
    nombres[c(indice_base, indice_respuesta)]
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
  # como "regularidad proporcional" igual con las dos declaradas. Sale del
  # analisis como sale de Tukey y de Benford, y se declara.
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
          "No se buscaron relaciones aritm\u00e9ticas con la columna: se declar\u00f3 en",
          "`columnas_codigo`, as\u00ed que es un c\u00f3digo y no una magnitud por fila."
        ),
        paste(
          "Si la columna es una magnitud y no un c\u00f3digo, quitarla de",
          "`columnas_codigo`."
        )
      )
    })))
  }
  hallazgos_aditivos <- list()
  objetivos_aditivos <- list()
  usa_cota_cuanto <- FALSE
  if (length(seleccion) >= 3L && nrow(datos)) {
    # El cuanto de cada columna se mide una vez, y solo con la tolerancia por
    # omision: una declarada no se acota (ver `.TOLERANCIA_ARITMETICA_OMISION`).
    cuantos <- rep(NA_real_, length(datos))
    if (identical(as.double(tolerancia), .TOLERANCIA_ARITMETICA_OMISION)) {
      cuantos[seleccion] <- vapply(
        seleccion, function(i) .cuanto_decimal_aritmetica(datos[[i]]),
        numeric(1L)
      )
    }
    ternas <- utils::combn(seleccion, 3L, simplify = FALSE)
    for (terna in ternas) {
      candidatos <- lapply(seq_len(3L), function(objetivo) {
        .hallazgo_identidad_aditiva(
          datos, terna[-objetivo], terna[[objetivo]], umbral,
          min_filas, tolerancia, nombres_texto,
          cuantos = cuantos[c(terna[-objetivo], terna[[objetivo]])]
        )
      })
      descartadas <- vapply(candidatos, is.character, logical(1L))
      por_cuanto <- vapply(candidatos, identical, logical(1L),
                           "descartada_cuanto")
      alcance$identidades_descartadas_escala <-
        alcance$identidades_descartadas_escala + sum(descartadas & !por_cuanto)
      alcance$identidades_descartadas_cuanto <-
        alcance$identidades_descartadas_cuanto + sum(por_cuanto)
      candidatos <- candidatos[!descartadas]
      candidatos <- Filter(Negate(is.null), candidatos)
      if (!length(candidatos)) next
      afectados <- vapply(candidatos, function(x) x$n_afectados[[1L]], numeric(1L))
      elegido <- candidatos[[which.min(afectados)]]
      columnas_elegidas <- attr(elegido, "columnas_aritmetica", exact = TRUE)
      attr(elegido, "columnas_aritmetica") <- NULL
      usa_cota_cuanto <- usa_cota_cuanto || isTRUE(columnas_elegidas$cota_cuanto)
      hallazgos_aditivos[[length(hallazgos_aditivos) + 1L]] <- elegido
      objetivos_aditivos[[length(objetivos_aditivos) + 1L]] <- columnas_elegidas
    }
  }
  # El criterio de la tabla dice lo que se aplico: si alguna identidad
  # publicada uso la cota del cuanto, el criterio relativo solo ya no la
  # describe. Sin cota el texto queda como siempre.
  if (usa_cota_cuanto) {
    alcance$criterio_tolerancia <- paste0(
      alcance$criterio_tolerancia, "; en las identidades aditivas con la ",
      "tolerancia por omisi\u00f3n, acotado a la mitad del cuanto decimal com\u00fan ",
      "de sus tres columnas cuando la identidad se cumple exacta en la mayor\u00eda ",
      "de las filas"
    )
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
