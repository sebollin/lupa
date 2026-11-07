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

# Una columna que tiene valores distintos pero varia menos que la tolerancia
# relativa de su magnitud: con tres valores o mas, y sin ser constante. Es la
# que `.varia_columna_aritmetica()` toma por constante sin serlo, y la que
# tiene que decir por que no entro.
.varia_bajo_tolerancia_aritmetica <- function(x) {
  x <- as.double(x[is.finite(x)])
  length(x) >= 3L && diff(range(x)) > 0
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
  # El error admitido es el de la magnitud de la COLUMNA y no el de cada valor:
  # un total calculado en R como `x + y`, con sumandos de cientos que se
  # cancelan, queda cerca de cero con el error de redondeo de los cientos
  # -`0.30 - 0.29` da `0.010000000000000009`-, y medido contra su propia
  # magnitud no era un multiplo del centavo. Sin cuanto la relativa mandaba, y
  # entre 1e6 y 1e10 los centavos rotos de esa suma salian `ok`.
  escala <- function(decimales) {
    if (decimales >= 0L) magnitud * 10^decimales else magnitud / 10^(-decimales)
  }
  en_reticula <- function(v, decimales) {
    w <- if (decimales >= 0L) v * 10^decimales else v / 10^(-decimales)
    all(abs(w - round(w)) <= 8 * .Machine$double.eps * max(1, escala(decimales)))
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

# **La resolucion de una columna: el redondeo que muestra.** El cuanto de arriba
# es la potencia de diez de sus decimales, y para una identidad alcanza: entre
# valores de una misma reticula la suma de dos es exacta y cualquier diferencia
# es un multiplo, asi que medio cuanto de la potencia de diez -mas fino que el
# verdadero- separa igual lo sano de lo roto, y ademas cuenta como rota una
# diferencia que se sale de la reticula. Para una proporcionalidad no: la
# respuesta difiere del producto en su redondeo, y el redondeo es al cuanto que
# la columna usa. Con la respuesta redondeada a multiplos de 0,05 el residuo
# llega a 0,025; medido con la potencia de diez de sus decimales (0,01) la
# cota quedaba en 0,005, las 100 filas sanas no la alcanzaban y la
# proporcionalidad se descartaba. Lo mismo con 0,25, 0,5, 5 o 25.
#
# La resolucion es la potencia de diez de los decimales multiplicada por el
# maximo comun de los valores llevados a enteros en esa escala. Una fila rota
# fuera de la reticula -un centavo en una columna de multiplos de 0,05- haria
# caer ese maximo comun a 1, y con el la cota, y las filas sanas pasarian a
# acusarse o la relacion a descartarse. Por eso vale el `q` del que son
# multiplos casi todos: al menos `.FRACCION_RESOLUCION` de los valores, la
# misma proporcion que pide el criterio por omision -si mas de una de cada
# diez filas esta fuera de la reticula, la relacion no se reconoceria igual-, y
# solo si la reticula no se explica por azar: con valores cuya ultima cifra es
# cualquiera, la probabilidad de que tantos caigan en una reticula `m` veces
# mas gruesa es a lo sumo `choose(n, fuera) * m^-dentro`, y se pide menor que
# 1e-6. Asi tres centavos pares no hacen una columna de 0,02 (1/8), ni nueve de
# diez (1 %), y cien multiplos de 0,05 si (5^-100). Sin filas fuera de la
# reticula de potencia de diez rige la regla de siempre del cuanto. Los
# multiplos que se prueban ademas del maximo comun son los de las
# resoluciones decimales usuales -2, 4, 5, 8, 20, 25, 40, 50, 125, 200, 250 y
# 500 veces la potencia de diez-, que son los que sobreviven a una fila rota.
.FRACCION_RESOLUCION <- 0.9

# Las bases y respuestas distintas que pide una proporcionalidad leida por
# debajo de la compuerta: ver `.lectura_proporcional_aritmetica()`.
.MINIMO_DISTINTOS_PROPORCIONAL <- 20L

# La cota de que `dentro` de `n` valores caigan por azar en una reticula
# `veces` mas gruesa que la de sus cifras, contra 1e-6.
.reticula_no_es_azar <- function(n, dentro, veces) {
  lchoose(n, n - dentro) - dentro * log(veces) < log(1e-6)
}

.multiplos_resolucion <- c(2, 4, 5, 8, 20, 25, 40, 50, 125, 200, 250, 500)

.resolucion_columna_aritmetica <- function(x) {
  x <- as.double(x[is.finite(x)])
  n <- length(x)
  if (!n) return(NA_real_)
  magnitud <- max(1, abs(x))
  minimo <- ceiling(.FRACCION_RESOLUCION * n)
  for (decimales in seq.int(-floor(log10(magnitud)), 15L)) {
    potencia <- 10^-decimales
    if (magnitud / potencia >= .CIFRAS_MAXIMAS_CUANTO) return(NA_real_)
    # Por una potencia de diez exacta, como `.cuanto_decimal_aritmetica()`.
    w <- if (decimales >= 0L) x * 10^decimales else x / 10^(-decimales)
    escala <- if (decimales >= 0L) magnitud * 10^decimales else
      magnitud / 10^(-decimales)
    entero <- round(w)
    dentro <- abs(w - entero) <= 8 * .Machine$double.eps * max(1, escala)
    # Todos en la reticula es la regla de siempre del cuanto; casi todos, solo
    # si no se explica por azar.
    if (sum(dentro) < minimo ||
        (sum(dentro) < n && !.reticula_no_es_azar(n, sum(dentro), 10))) {
      next
    }
    enteros <- abs(entero[dentro])
    enteros <- enteros[enteros > 0]
    comun <- if (length(enteros)) Reduce(.maximo_comun_divisor, enteros) else 1
    veces <- 1
    for (candidato in unique(c(comun, .multiplos_resolucion))) {
      if (candidato <= veces) next
      en_candidato <- dentro & (abs(entero) %% candidato == 0)
      if (sum(en_candidato) >= minimo &&
          .reticula_no_es_azar(n, sum(en_candidato), candidato)) {
        veces <- candidato
      }
    }
    return(veces * potencia)
  }
  NA_real_
}

.maximo_comun_divisor <- function(a, b) {
  while (b > 0) {
    resto <- a %% b
    a <- b
    b <- resto
  }
  a
}

# Si cada valor esta en la reticula de `resolucion`, con el mismo margen del
# doble que la mide.
.en_resolucion_aritmetica <- function(x, resolucion) {
  x <- as.double(x)
  w <- x / resolucion
  magnitud <- max(1, abs(x[is.finite(x)]))
  is.finite(x) &
    abs(w - round(w)) <= 8 * .Machine$double.eps * max(1, magnitud / resolucion)
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
    # Las proporcionalidades que la tolerancia relativa reconocia y la
    # resolucion de sus columnas no: ver `.lectura_proporcional_aritmetica()`.
    proporcionalidades_descartadas_cuanto = 0,
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

# Lo que devuelve una candidata que no es un hallazgo: `NA` para un hallazgo o
# `NULL`, y si no el motivo -`descartada_escala`, `descartada_cuanto`,
# `sin_variacion`- sin los atributos que lo acompanan.
.marca_aritmetica <- function(x) {
  if (is.character(x)) as.vector(x)[[1L]] else NA_character_
}

# Una fila por columna que varia menos que la tolerancia relativa de su
# magnitud y dejo sin publicar una relacion que esa tolerancia reconocia: una
# relacion que no se evalua dice por que. Por columna y no por relacion: un par
# que se cancela -`1e11 + x` y `-1e11 + y`- "completa" con cualquier columna
# chica, y una fila por terna seria una por cada columna de la tabla.
.cobertura_sin_variacion_aritmetica <- function(registro, nombres,
                                                nombres_texto, tolerancia) {
  tolerancia_texto <- .texto_tolerancia_aritmetica(tolerancia)
  do.call(rbind, lapply(registro, function(r) {
    magnitud <- signif(max(1, r$magnitud), 3)
    total <- r$identidades + r$pares
    cuantas <- c(
      if (r$identidades) {
        paste0(r$identidades, if (r$identidades == 1) " identidad aditiva" else
          " identidades aditivas")
      },
      if (r$pares) {
        paste0(r$pares, if (r$pares == 1) " proporcionalidad" else
          " proporcionalidades")
      }
    )
    .nuevo_diagnostico_no_evaluado(
      "relacion_aritmetica_columnas", nombres[[r$columna]],
      paste0(
        "La tolerancia relativa reconoc\u00eda ", paste(cuantas, collapse = " y "),
        " con la columna `", nombres_texto[[r$columna]], "` (",
        if (total > 1) "por ejemplo ", r$ejemplo, "), y ",
        if (total == 1) "no se public\u00f3" else "no se publicaron",
        ": entre las filas comparables la columna var\u00eda ",
        .formatear_numero_publicado(signif(r$rango, 3)),
        " sobre valores de hasta ", .formatear_numero_publicado(magnitud),
        ", menos que esa tolerancia (", tolerancia_texto, " * ",
        .formatear_numero_publicado(magnitud), " = ",
        .formatear_numero_publicado(signif(tolerancia * magnitud, 3)),
        "). Frente a ella la columna es constante, y la relaci\u00f3n se ",
        "cumplir\u00eda igual con un valor fijo en su lugar."
      ),
      paste0(
        "Si esas diferencias son del dato, declarar una ",
        "`tolerancia_aritmetica` menor que ",
        .texto_tolerancia_aritmetica(signif(r$rango / magnitud, 2)),
        ". Con la tolerancia por omisi\u00f3n, una identidad aditiva cuyas tres ",
        "columnas guardan decimales fijos de menos de 15 cifras significativas ",
        "se eval\u00faa con la mitad de su cuanto."
      )
    )
  }))
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
  if (n_evaluados < min_filas) return(NULL)
  columnas_terna <- list(x[comparables], y[comparables], z[comparables])
  varian <- vapply(columnas_terna, .varia_columna_aritmetica, logical(1L),
                   tolerancia = tolerancia)
  # Una columna constante de verdad -o con menos de tres valores- sale como
  # siempre, sin rastro: el perfil ya dice que es constante.
  if (!all(varian) &&
      !all(vapply(columnas_terna[!varian], .varia_bajo_tolerancia_aritmetica,
                  logical(1L)))) {
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
  cuanto <- .cota_cuanto_aritmetica(cuantos)
  # **Una columna que varia menos que la tolerancia relativa de su magnitud.**
  # Con `a = 1e11 + x` y `b = -1e11 + y` -una suma con cancelacion- cada
  # sumando varia unos cientos sobre 1e11, y frente a la tolerancia de 1e3
  # unidades es constante: la identidad se cumpliria igual con un valor fijo
  # en su lugar, y antes se descartaba sin rastro. Si la tolerancia relativa no
  # la reconoce, no hay relacion y no hay nada que declarar. Si la reconoce y
  # las tres columnas guardan decimales fijos, se evalua con la cota del cuanto,
  # donde una diferencia de medio cuanto ya es del dato; si no, se devuelve el
  # motivo para la cobertura.
  sin_variacion <- NULL
  if (!all(varian)) {
    if (!.cumple_criterio_aritmetica(mean(cumple), umbral)) return(NULL)
    sin_variacion <- structure(
      "sin_variacion",
      columnas = c(sumandos, total)[!varian],
      valores = columnas_terna[!varian],
      expresion = paste0(
        nombres_texto[[sumandos[[1L]]]], " + ", nombres_texto[[sumandos[[2L]]]],
        " ~= ", nombres_texto[[total]]
      )
    )
    if (!is.finite(cuanto$cota)) return(sin_variacion)
  }
  # La cota del cuanto, con las tres condiciones de
  # `.TOLERANCIA_ARITMETICA_OMISION`. Solo se usa si achica la tolerancia en
  # alguna fila: si no, el criterio aplicado es el relativo de siempre y la
  # evidencia lo dice igual que antes.
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
      # descarta y se cuenta, como las de un sumando absorbido. Con una columna
      # sin variacion frente a la tolerancia, en cambio, la relativa no decia
      # nada: la cota es la unica evaluacion, y si no la alcanza no hay
      # relacion.
      if (reconocida_sin_cota &&
          !.cumple_criterio_aritmetica(mean(cumple), umbral)) {
        if (!is.null(sin_variacion)) return(NULL)
        return("descartada_cuanto")
      }
    }
  }
  if (!is.null(sin_variacion) && !is.finite(cota)) return(sin_variacion)
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

# **La proporcionalidad con la respuesta redondeada en importes grandes.** Con
# `iva = round(neto * 0.22, 2)` y la tolerancia relativa, 30 de 300 filas
# corridas uno o treinta centavos salian `ok` con 0 discrepancias sobre netos
# de 1e9 a 1e12 (la tolerancia vale de 2 a 2.000 unidades), y sobre netos de
# 1e7 -donde la tolerancia de los valores chicos queda por debajo del redondeo
# a centavos- un producto sano acusaba 8 filas. La idea de la cota de las
# identidades, llevada a un producto: una columna con decimales fijos -todos
# sus valores son multiplos de un cuanto `q`, tambien los mas grandes- difiere
# del valor exacto en su redondeo, menos de `q / 2`, y una diferencia mayor
# esta en el dato. Tres diferencias con la identidad, todas medidas:
#   - el esperado `base * k` no cae en la reticula, asi que la cota es la
#     tolerancia de todas las filas y no un tope: si no, los valores chicos,
#     con la relativa por debajo del redondeo, se acusan;
#   - `k` no puede ser la mediana de los cocientes: difiere de 0,22 en 4,5e-12
#     sobre 1e8, y por la base eso corre medio cuanto (ver
#     `.constante_por_intervalos_aritmetica()`);
#   - la columna redondeada puede ser cualquiera de las dos, y se lee como
#     respuesta. Con `|k| <= 1`, `pesos = round(dolares * 39.8734, 2)` deja la
#     redondeada como base, y medido en la otra columna su redondeo es
#     `|k| * q / 2`: la cota de la respuesta, cuarenta veces mas ancha, dejaba
#     pasar todas las filas corridas. Por eso se prueban las dos lecturas,
#     cada una con su respuesta, su cota y su `k`.
#   - de las lecturas que alcanzan el criterio, la de la constante con menos
#     cifras: es la del dominio (`1.22`, no `0.8196721311...`), y su columna
#     es la que se calculo y redondeo. Leer `pesos = round(dolares * 1.22)`
#     al reves media su redondeo con `|1 / 1.22| * 0.5`, calculado con la
#     mediana, y una fila sana cuyo redondeo cae justo en la mitad -`4075 *
#     1.22 = 4971.5`, que `round()` lleva a `4972`- quedaba afuera por el
#     recorte de la constante: con enteros y `k = 1.22`, una a dos filas sanas
#     acusadas por tabla. Entre dos constantes igual de largas, la cota mas
#     fina medida en la misma columna.
# La cota es inclusiva: un redondeo a la mitad difiere del producto en `q / 2`
# exacto, y el margen cubre la representacion en doble de los valores, de `k`
# y del producto (ver `.constante_por_intervalos_aritmetica()`).
# Decimales fijos quiere decir que la mitad superior de la columna tiene el
# mismo cuanto que la columna entera: una respuesta redondeada a nueve u once
# cifras significativas sobre dos decadas es multiplo del cuanto de sus valores
# chicos, pero los grandes difieren del producto en unidades de su ultima
# cifra, y con la cota la relacion sana se descartaba.
.lectura_proporcional_aritmetica <- function(base, respuesta, comparables,
                                             cuanto, tolerancia, umbral,
                                             min_filas, reconocida,
                                             con_fallas = FALSE) {
  no_aplica <- list(aplica = FALSE, alcanza = FALSE)
  no_alcanza <- list(aplica = TRUE, alcanza = FALSE)
  if (is.null(cuanto) || !isTRUE(cuanto$fijo)) return(no_aplica)
  # Un valor fuera de la reticula de la respuesta no es un redondeo a su
  # resolucion: no entra al barrido y no cumple (ver
  # `.resolucion_columna_aritmetica()`). Una fila sana siempre esta adentro.
  en_reticula <- .en_resolucion_aritmetica(respuesta, cuanto$cuanto)
  utilizables <- is.finite(base) & is.finite(respuesta) & base != 0
  if (sum(utilizables) < min_filas) return(no_aplica)
  mediana <- stats::median(respuesta[utilizables] / base[utilizables])
  if (!is.finite(mediana) || mediana == 0) return(no_aplica)
  cota <- cuanto$cuanto / 2
  r <- respuesta[comparables]
  b <- base[comparables]
  esperado_mediana <- b * mediana
  # Donde la relativa supera al redondeo en alguna fila, o ya reconoce la
  # relacion acusando filas -con la respuesta a 0,05 sobre 2,2e6 la relativa
  # vale 0,022 y acusaba las 8 filas sanas cuyo redondeo caia entre 0,022 y
  # 0,025-, la lectura se intenta siempre. Por debajo -respuestas chicas
  # frente a su cuanto-, si la relativa reconoce la relacion sin acusar nada
  # queda como estaba; si no la reconoce, la lectura se intenta con dos
  # guardas, porque ahi la mitad del cuanto es ancha frente a los valores y
  # casi cualquier par chico "cumple". Medido sobre 14.094 pares de columnas
  # independientes con decimales fijos (enteros de 1 a 5 y de 1 a 100,
  # importes, porcentajes a 0,05, cantidades, edades, una binaria, una sesgada
  # 1/2 al 95 %, una base de rango estrecho, una respuesta con un valor en el
  # 92 % de las filas; de 10 a 10.000 filas, con y sin `NA`), sin guardas se
  # publicaban 229 proporcionalidades espurias: 30 con respuestas de dos
  # valores -una binaria "es" la mitad de una columna casi siempre 1- y 199
  # entre una base estrecha y una respuesta dominante, que la "explica" con
  # `k = 0.05` en las filas de la moda. Con al menos 20 bases y 20 respuestas
  # distintas, y la condicion de explicar mas filas que la moda de la
  # respuesta sola -la de las dependencias-, ninguna; las relaciones reales de
  # las auditorias tienen cientos de valores distintos y una moda de a lo sumo
  # el 8 %.
  debajo <- !any(tolerancia * pmax(1, abs(r), abs(esperado_mediana)) > cota)
  guardado <- debajo && !con_fallas
  if (guardado && reconocida) return(no_aplica)
  if (guardado) {
    leidos <- utilizables & en_reticula
    if (length(unique(base[leidos])) < .MINIMO_DISTINTOS_PROPORCIONAL ||
        length(unique(respuesta[leidos])) < .MINIMO_DISTINTOS_PROPORCIONAL) {
      return(no_aplica)
    }
  }
  # El barrido ordena dos extremos por fila. Si la relativa no reconocia la
  # relacion, se ordena solo si con la mediana -cuyo error corre el esperado
  # en proporcion a la base- ya hay suficientes filas cerca: dos columnas sin
  # relacion no pagan un orden por par.
  if (!reconocida) {
    magnitud <- abs(b)
    relativa_base <- pmax(1, magnitud / max(stats::median(magnitud),
                                            .Machine$double.xmin))
    if (!.cumple_criterio_aritmetica(mean(
      abs(r - esperado_mediana) <= 2 * cota * relativa_base
    ), umbral)) {
      return(no_alcanza)
    }
  }
  barrido <- utilizables & en_reticula
  if (sum(barrido) < min_filas) return(no_alcanza)
  constante <- .constante_por_intervalos_aritmetica(
    as.double(base[barrido]), as.double(respuesta[barrido]), cota, mediana
  )
  esperado <- b * constante
  # El margen del doble va sobre el de los intervalos: una fila que el barrido
  # conto adentro no queda afuera por el redondeo de `k` ni del producto.
  cumple <- abs(r - esperado) <= cota +
    8 * .Machine$double.eps * pmax(1, abs(r), abs(esperado)) &
    en_reticula[comparables]
  if (!.cumple_criterio_aritmetica(mean(cumple), umbral)) return(no_alcanza)
  if (guardado) {
    valores <- r[is.finite(r)]
    moda <- if (length(valores)) max(tabulate(match(valores, unique(valores)))) else 0L
    if (sum(cumple) <= moda) return(no_aplica)
  }
  list(
    aplica = TRUE, alcanza = TRUE,
    constante = as.vector(constante),
    cifras = attr(constante, "cifras", exact = TRUE),
    mediana = mediana, esperado = esperado, cumple = cumple, cota = cota,
    cuanto = cuanto$cuanto, utilizables = utilizables,
    relativa_maxima = max(tolerancia * pmax(1, abs(r), abs(esperado_mediana)))
  )
}

# La constante corta que deja casi tantas filas como la que mas a menos de
# `semiancho` de la respuesta. Cada fila admite las `k` de un intervalo,
# `[(r - s) / b, (r + s) / b]`, y un barrido sobre sus extremos cuenta cuantos
# intervalos cubren cada tramo. De los tramos que cubren tantas filas como el
# mejor, o a lo sumo un 1 % menos, el que contiene el decimal con menos cifras;
# entre esos, el que mas cubre, y despues el mas cercano a `referencia` -la
# mediana-. El tramo de cobertura maxima no alcanza: una fila sana cuyo redondeo
# cae justo en la mitad toca el borde de su intervalo en la constante
# verdadera, y correrse un poco la pierde y gana una fila rota que estaba cerca
# del otro lado; los dos tramos cubren lo mismo, y elegir el que no contiene a
# `0.105` acusaba la sana y callaba la rota (medido con enteros sobre 1e11). La
# constante del dominio es corta y explica las filas sanas; una constante mas
# corta lejos de ella no cubre casi ninguna. El atributo `cifras` dice cuantas
# tiene la elegida.
.constante_por_intervalos_aritmetica <- function(base, respuesta, semiancho,
                                                 referencia) {
  holgura <- semiancho + 4 * .Machine$double.eps *
    pmax(1, abs(respuesta), abs(base * referencia))
  a <- (respuesta - holgura) / base
  b <- (respuesta + holgura) / base
  puntos <- c(pmin(a, b), pmax(a, b))
  pasos <- rep(c(1L, -1L), each = length(base))
  # Con el mismo extremo, la apertura antes que el cierre: intervalos cerrados.
  orden <- .orden_por_bytes(puntos, -pasos)
  puntos <- puntos[orden]
  conteo <- cumsum(pasos[orden])
  tramos <- which(conteo >= max(conteo) - max(1, ceiling(0.01 * length(base))) &
                    conteo > 0L)
  tramos <- tramos[tramos < length(puntos)]
  inferior <- puntos[tramos]
  superior <- puntos[tramos + 1L]
  centro <- (inferior + superior) / 2
  cifras <- rep(17L, length(tramos))
  for (k in 15:1) {
    corta <- signif(centro, k)
    cifras[corta >= inferior & corta <= superior] <- k
  }
  distancia <- pmax(0, inferior - referencia, referencia - superior)
  elegido <- .orden_por_bytes(cifras, -conteo[tramos], distancia)[[1L]]
  constante <- if (cifras[[elegido]] <= 15L) {
    signif(centro[[elegido]], cifras[[elegido]])
  } else centro[[elegido]]
  structure(constante, cifras = cifras[[elegido]])
}

.hallazgo_proporcional <- function(datos, par, umbral, min_filas,
                                    tolerancia, nombres_texto = names(datos),
                                    cuanto_de = NULL) {
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
  if (!is.finite(constante) || abs(abs(constante) - 1) <= tolerancia) {
    return(NULL)
  }
  varian <- c(
    .varia_columna_aritmetica(base[utilizables], tolerancia),
    .varia_columna_aritmetica(respuesta[utilizables], tolerancia)
  )
  # Una columna constante de verdad sale sin rastro, como siempre; una que
  # varia menos que la tolerancia relativa, solo si la relativa tampoco
  # reconoce la relacion (ver `.hallazgo_identidad_aditiva()`).
  valores_par <- list(base[utilizables], respuesta[utilizables])
  if (!all(varian) &&
      !all(vapply(valores_par[!varian], .varia_bajo_tolerancia_aritmetica,
                  logical(1L)))) {
    return(NULL)
  }
  comparables <- is.finite(base) & is.finite(respuesta)
  n_evaluados <- sum(comparables)
  if (n_evaluados < min_filas) return(NULL)
  mediana <- constante
  esperado <- base[comparables] * constante
  cumple <- .dentro_tolerancia_aritmetica(
    respuesta[comparables], esperado, tolerancia
  )
  proporcion <- mean(cumple)
  if (!all(varian)) {
    if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
    return(structure(
      "sin_variacion",
      columnas = c(indice_base, indice_respuesta)[!varian],
      valores = valores_par[!varian],
      expresion = paste0(
        nombres_texto[[indice_respuesta]], " ~= ", nombres_texto[[indice_base]],
        " * k"
      )
    ))
  }
  # La cota de una columna con decimales fijos, en las dos lecturas: ver
  # `.lectura_proporcional_aritmetica()`.
  cota <- Inf
  cota_elegida <- NULL
  relativa_maxima <- max(tolerancia * pmax(
    1, abs(respuesta[comparables]), abs(esperado)
  ))
  if (is.function(cuanto_de)) {
    reconocida_sin_cota <- .cumple_criterio_aritmetica(proporcion, umbral)
    con_fallas <- reconocida_sin_cota && proporcion < 1
    lectura_directa <- .lectura_proporcional_aritmetica(
      base, respuesta, comparables, cuanto_de(indice_respuesta), tolerancia,
      umbral, min_filas, reconocida_sin_cota, con_fallas
    )
    lectura_inversa <- .lectura_proporcional_aritmetica(
      respuesta, base, comparables, cuanto_de(indice_base), tolerancia,
      umbral, min_filas, reconocida_sin_cota, con_fallas
    )
    # Medidas en la misma columna -la respuesta de la lectura directa- para
    # desempatar dos constantes igual de largas.
    cota_comun <- function(lectura, invertida) {
      if (invertida) lectura$cota * abs(constante) else lectura$cota
    }
    # Si las dos alcanzan el criterio: la de la constante claramente mas
    # corta -tres cifras o mas de diferencia, la reciproca de un decimal corto
    # que no termina pide todas las que da el tramo-; si no, la de la cota mas
    # fina. Con `k = 2.5` las dos son cortas (`2.5` y `0.4`), y la mas fina
    # -medio entero de la respuesta, `0.2` de la base- es la que distingue un
    # entero corrido; con `k = 0.95` la reciproca alcanza el criterio con 95 %
    # de las filas y su cota es mas fina, pero es larga y acusaria el 5 % sano.
    elegida <- NULL
    invertir <- FALSE
    if (lectura_directa$alcanza) elegida <- lectura_directa
    if (lectura_inversa$alcanza) {
      if (is.null(elegida)) {
        invertir <- TRUE
      } else if (abs(lectura_inversa$cifras - elegida$cifras) >= 3L) {
        invertir <- lectura_inversa$cifras < elegida$cifras
      } else {
        invertir <- cota_comun(lectura_inversa, TRUE) <
          cota_comun(elegida, FALSE)
      }
      if (invertir) elegida <- lectura_inversa
    }
    if (is.null(elegida)) {
      # Una lectura posible -una columna con decimales fijos y la relativa
      # por encima de su redondeo- que no alcanza el criterio: la relativa la
      # reconocia y la resolucion de las columnas no. Se descarta y se cuenta,
      # como la identidad con la cota del cuanto. Sin lectura posible, la
      # relativa de siempre.
      if (lectura_directa$aplica || lectura_inversa$aplica) {
        if (reconocida_sin_cota) return("descartada_cuanto")
        return(NULL)
      }
    } else {
      if (invertir) {
        intercambio <- base
        base <- respuesta
        respuesta <- intercambio
        intercambio <- indice_base
        indice_base <- indice_respuesta
        indice_respuesta <- intercambio
      }
      utilizables <- elegida$utilizables
      mediana <- elegida$mediana
      constante <- elegida$constante
      esperado <- elegida$esperado
      cumple <- elegida$cumple
      proporcion <- mean(cumple)
      cota <- elegida$cota
      cota_elegida <- list(cuanto = elegida$cuanto)
      relativa_maxima <- elegida$relativa_maxima
    }
  }
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
      if (is.finite(cota)) {
        paste0(
          " (la de menos cifras entre las que dejan m\u00e1s filas dentro del ",
          "criterio de abajo, sobre ", sum(utilizables),
          if (sum(utilizables) == 1L) " fila finita" else " filas finitas",
          " con base distinta de cero; la mediana de respuesta/base es ",
          format(mediana, scientific = FALSE, trim = TRUE, digits = 15L),
          "); forma equivalente: "
        )
      } else {
        paste0(
          " (mediana de respuesta/base en ", sum(utilizables),
          if (sum(utilizables) == 1L) " fila finita con base distinta de cero); forma equivalente: " else
            " filas finitas con base distinta de cero); forma equivalente: "
        )
      },
      inversa, ". Tolerancia declarada: ", tolerancia_texto,
      "; criterio |observado - esperado| <= ",
      if (is.finite(cota)) {
        paste0(
          .formatear_numero_publicado(cota),
          ": la mitad del cuanto decimal de `", nombres_texto[[indice_respuesta]],
          "` (", .formatear_numero_publicado(cota_elegida$cuanto), "), porque ",
          "una respuesta con decimales fijos difiere del producto en su ",
          "redondeo, a lo sumo medio cuanto, y la tolerancia relativa llegaba a ",
          .formatear_numero_publicado(signif(relativa_maxima, 3)),
          " en estas filas. "
        )
      } else {
        paste0(tolerancia_texto, " * max(1, |observado|, |esperado|). ",
               .texto_limite_proporcional_aritmetica(
                 respuesta[comparables], esperado, tolerancia
               ))
      },
      ejemplos
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
  attr(hallazgo, "cota_proporcional") <- is.finite(cota)
  hallazgo
}

# Sin cota y sobre una respuesta donde un decimal de dos cifras ya tiene 15
# cifras significativas, la tolerancia relativa traga diferencias del dato: la
# evidencia lo dice con su valor, como en la identidad aditiva.
.texto_limite_proporcional_aritmetica <- function(respuesta, esperado,
                                                  tolerancia) {
  magnitud <- max(abs(respuesta))
  if (!identical(as.double(tolerancia), .TOLERANCIA_ARITMETICA_OMISION) ||
      magnitud < .CIFRAS_MAXIMAS_CUANTO * 0.01) {
    return("")
  }
  paste0(
    "No se acota por un cuanto decimal: con valores de ",
    .formatear_numero_publicado(signif(magnitud, 3)),
    " un decimal de dos cifras ya tiene 15 cifras significativas, las de un ",
    "texto exportado, y no se distingue de su redondeo; la tolerancia llega ",
    "a ", .formatear_numero_publicado(signif(max(
      tolerancia * pmax(1, abs(respuesta), abs(esperado))
    ), 3)),
    " en estas filas, y una diferencia menor no se cuenta como discrepancia. "
  )
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
  # Las columnas que varian menos que la tolerancia relativa y dejaron sin
  # evaluar una relacion que la relativa reconocia: ver
  # `.cobertura_sin_variacion_aritmetica()`.
  sin_variacion <- list()
  registrar_sin_variacion <- function(marca, tipo) {
    columnas <- attr(marca, "columnas", exact = TRUE)
    valores <- attr(marca, "valores", exact = TRUE)
    for (j in seq_along(columnas)) {
      clave <- as.character(columnas[[j]])
      previo <- sin_variacion[[clave]]
      if (is.null(previo)) {
        previo <- list(columna = columnas[[j]], identidades = 0, pares = 0,
                       ejemplo = attr(marca, "expresion", exact = TRUE),
                       rango = 0, magnitud = 0)
      }
      previo[[tipo]] <- previo[[tipo]] + 1
      rango <- diff(range(valores[[j]]))
      if (rango > previo$rango) {
        previo$rango <- rango
        previo$magnitud <- max(abs(valores[[j]]))
      }
      sin_variacion[[clave]] <<- previo
    }
  }
  # El cuanto de cada columna se mide una vez, y solo con la tolerancia por
  # omision: una declarada no se acota (ver `.TOLERANCIA_ARITMETICA_OMISION`).
  cuantos <- rep(NA_real_, length(datos))
  if (length(seleccion) >= 2L && nrow(datos) &&
      identical(as.double(tolerancia), .TOLERANCIA_ARITMETICA_OMISION)) {
    cuantos[seleccion] <- vapply(
      seleccion, function(i) .cuanto_decimal_aritmetica(datos[[i]]),
      numeric(1L)
    )
  }
  if (length(seleccion) >= 3L && nrow(datos)) {
    ternas <- utils::combn(seleccion, 3L, simplify = FALSE)
    for (terna in ternas) {
      candidatos <- lapply(seq_len(3L), function(objetivo) {
        .hallazgo_identidad_aditiva(
          datos, terna[-objetivo], terna[[objetivo]], umbral,
          min_filas, tolerancia, nombres_texto,
          cuantos = cuantos[c(terna[-objetivo], terna[[objetivo]])]
        )
      })
      marcas <- vapply(candidatos, .marca_aritmetica, character(1L))
      sin_var <- marcas %in% "sin_variacion"
      descartadas <- !is.na(marcas) & !sin_var
      por_cuanto <- marcas %in% "descartada_cuanto"
      alcance$identidades_descartadas_escala <-
        alcance$identidades_descartadas_escala + sum(descartadas & !por_cuanto)
      alcance$identidades_descartadas_cuanto <-
        alcance$identidades_descartadas_cuanto + sum(por_cuanto)
      publicables <- Filter(Negate(is.null), candidatos[is.na(marcas)])
      # Una terna se declara sin evaluar una sola vez, y solo si ninguna de sus
      # tres orientaciones se publico.
      if (!length(publicables) && any(sin_var)) {
        registrar_sin_variacion(candidatos[[which(sin_var)[[1L]]]], "identidades")
      }
      candidatos <- publicables
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
  usa_cota_proporcional <- FALSE
  # El cuanto de la respuesta y si es de decimales fijos, una vez por columna
  # y solo cuando una proporcionalidad lo pide.
  fijos <- new.env(parent = emptyenv())
  # La resolucion de la columna, no el cuanto de la potencia de diez de las
  # identidades: ver `.resolucion_columna_aritmetica()`.
  por_omision <- identical(as.double(tolerancia), .TOLERANCIA_ARITMETICA_OMISION)
  cuanto_de <- function(i) {
    if (!por_omision) return(NULL)
    clave <- as.character(i)
    if (is.null(fijos[[clave]])) {
      x <- as.double(datos[[i]])
      x <- abs(x[is.finite(x)])
      resolucion <- .resolucion_columna_aritmetica(x)
      # Decimales fijos: la misma resolucion en la mitad de valores grandes y en
      # la de los chicos. Con la resolucion de casi todos, mirar solo la mitad
      # grande ya no alcanza: once cifras significativas sobre dos decadas dejan
      # nueve de cada diez valores en la decada alta, con un decimal menos, y la
      # resolucion de casi todos es la de esos; los de la decada baja no son
      # filas rotas sino valores con mas cifras, y estan todos en la mitad chica.
      mitades <- if (is.finite(resolucion)) {
        mediana <- stats::median(x)
        c(.resolucion_columna_aritmetica(x[x >= mediana]),
          .resolucion_columna_aritmetica(x[x <= mediana]))
      } else NA_real_
      fijos[[clave]] <- if (!is.finite(resolucion)) list(cuanto = NA_real_, fijo = FALSE) else list(
        cuanto = resolucion,
        fijo = all(is.finite(mitades)) && all(abs(mitades / resolucion - 1) < 1e-9)
      )
    }
    if (!is.finite(fijos[[clave]]$cuanto)) return(NULL)
    fijos[[clave]]
  }
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
        datos, par, umbral, min_filas, tolerancia, nombres_texto,
        cuanto_de = cuanto_de
      )
      marca <- .marca_aritmetica(hallazgo)
      if (identical(marca, "descartada_cuanto")) {
        alcance$proporcionalidades_descartadas_cuanto <-
          alcance$proporcionalidades_descartadas_cuanto + 1
        next
      }
      if (identical(marca, "sin_variacion")) {
        registrar_sin_variacion(hallazgo, "pares")
        next
      }
      if (!is.null(hallazgo)) {
        usa_cota_proporcional <- usa_cota_proporcional ||
          isTRUE(attr(hallazgo, "cota_proporcional", exact = TRUE))
        attr(hallazgo, "cota_proporcional") <- NULL
        hallazgos_proporcionales[[length(hallazgos_proporcionales) + 1L]] <-
          hallazgo
      }
    }
  }
  if (usa_cota_proporcional) {
    alcance$criterio_tolerancia <- paste0(
      alcance$criterio_tolerancia, "; en las proporcionalidades con la ",
      "tolerancia por omisi\u00f3n y una respuesta con decimales fijos, la mitad ",
      "del cuanto decimal de la respuesta"
    )
  }
  if (length(sin_variacion)) {
    cobertura <- rbind(cobertura, .cobertura_sin_variacion_aritmetica(
      sin_variacion, names(datos), nombres_texto, tolerancia
    ))
  }
  list(
    hallazgos = c(hallazgos_aditivos, hallazgos_proporcionales),
    alcance = alcance,
    cobertura = cobertura
  )
}
