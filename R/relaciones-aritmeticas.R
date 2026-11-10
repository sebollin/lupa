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

.resolucion_columna_aritmetica <- function(x, fraccion = .FRACCION_RESOLUCION,
                                           fuera = NULL, filas = NULL) {
  # El cero esta en toda reticula y no dice nada de la resolucion: con 60 % de
  # netos en cero, la mitad chica de un iva a centavos era toda ceros, su
  # "resolucion" salia gruesa y la columna no pasaba por de decimales fijos; dos
  # centavos corridos salian `ok`. Con 95 % de ceros, ademas, cualquier
  # reticula gruesa juntaba el 90 % que pide la de casi todos.
  # Y se mide en los valores distintos: un valor repetido es un solo punto de
  # la reticula. Con 34 de 50 filas en el par (1000, 1105) y las otras 16 en
  # `y = round(1.105 * x, 2)`, la mitad chica de `y` -las 34 copias de 1105 y
  # ocho centavos pares- salia en pasos de 0,02, y la de casi todos, en pasos
  # de 0,02 o de 1105: la columna no pasaba por de decimales fijos, su lectura
  # no se intentaba y la relacion se leia al reves, `x ~= y * 0.90497737`, con
  # una fila sana acusada. Contadas como 34 observaciones, ademas, las copias
  # pasaban la cota de azar que con una sola no pasan.
  # Pero los que quedan fuera de la reticula se cuentan por fila: la de casi
  # todos deja fuera hasta `1 - fraccion` de las filas, y con valores chicos
  # repetidos tres rotas de 30 filas eran tres de 27 valores, mas de uno de cada
  # diez, y la resolucion de casi todos no salia. `fuera` fija cuantos valores
  # distintos pueden quedar afuera (ver `.decimales_fijos_aritmetica()`).
  # La cota de azar, en cambio, cuenta cuantas filas tiene cada valor: quien
  # llama le pasa una fila por registro distinto de la tabla, asi que las 34
  # copias de un mismo par son una, pero cien importes distintos que el
  # redondeo a 5 junta en cinco valores son cien. Contados como cinco, la
  # reticula de 5 "se explicaba por azar" y la relacion se descartaba.
  # `filas` es el total de filas de la columna, con las repetidas: el permiso
  # de `1 - fraccion` es de filas, como el criterio.
  valores <- abs(as.double(x[is.finite(x) & x != 0]))
  registros <- length(valores)
  if (is.null(filas)) filas <- registros
  x <- unique(valores)
  n <- length(x)
  if (!n) return(NA_real_)
  peso <- tabulate(match(valores, x), nbins = n)
  magnitud <- max(1, abs(x))
  if (is.null(fuera)) fuera <- floor((1 - fraccion) * filas + 1e-9)
  minimo <- max(1, n - fuera)
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
        (sum(dentro) < n &&
           !.reticula_no_es_azar(registros, sum(peso[dentro]), 10))) {
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
          .reticula_no_es_azar(registros, sum(peso[en_candidato]),
                               candidato)) {
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

# Decimales fijos: la misma resolucion en la mitad de valores grandes y en la de
# los chicos. Con la resolucion de casi todos, mirar solo la mitad grande no
# alcanza: once cifras significativas sobre dos decadas dejan nueve de cada
# diez valores en la decada alta, con un decimal menos, y la resolucion de casi
# todos es la de esos; los de la decada baja no son filas rotas sino valores
# con mas cifras, y estan todos en la mitad chica. Pero esa diferencia solo
# existe si los valores abarcan una decada o mas: dentro de una decada cifras
# significativas y decimales fijos son lo mismo, y comparar mitades de once
# valores encontraba por la estructura de los datos -una serie a paso fijo- una
# mitad en una reticula mas gruesa, y la relacion real se descartaba.
# Y lo que se compara es la potencia de diez de cada resolucion -cuantos
# decimales-, no la resolucion entera: con la base en medios, `0.18 * x` cae
# en pasos de 0,09, y la mitad sin la fila rota salia en 0,09 contra 0,01 de
# la columna. Los decimales son los mismos; el 9 es la estructura de los datos.
# La respuesta no pasaba por de decimales fijos, su lectura (`y ~= x * 0.18`)
# no se intentaba y quedaba la reciproca, `x ~= y * 5.55556`, con una fila
# sana acusada.
.decimales_fijos_aritmetica <- function(x, resolucion, fraccion,
                                        filas = NULL) {
  # Las mitades, de los valores distintos: ver `.resolucion_columna_aritmetica()`.
  # Cada mitad puede dejar afuera las filas que la columna deja afuera, no un
  # decimo de las suyas: con las tres rotas entre las filas grandes, la mitad
  # grande quedaba con tres de 15 valores fuera de la reticula, salia en
  # 0,0003 contra 0,36 de la chica, y la respuesta no pasaba por de decimales
  # fijos; quedaba la lectura gruesa de la base, que se traga las rotas.
  valores <- abs(x[is.finite(x) & x != 0])
  if (is.null(filas)) filas <- length(valores)
  fuera <- floor((1 - fraccion) * filas + 1e-9)
  x <- unique(valores)
  if (length(x) < 2L || max(x) / min(x) < 10) return(TRUE)
  # El permiso es para filas rotas, que caen entre las sanas. Si las que quedan
  # fuera de la reticula son la mayoria de los valores hasta la mas grande de
  # ellas, es la firma de las cifras significativas -la decada baja con un
  # decimal mas, salvo los que terminan en cero- y no hay permiso: con once
  # cifras sobre dos decadas son una de cada diez filas, y con el permiso la
  # base pasaba por de decimales fijos.
  afuera <- !.en_resolucion_aritmetica(x, resolucion)
  if (fuera > 0 && any(afuera) && mean(afuera[x <= max(x[afuera])]) >= 0.5) {
    fuera <- 0
  }
  mediana <- stats::median(x)
  mitades <- c(
    .resolucion_columna_aritmetica(valores[valores >= mediana], fraccion,
                                   fuera = fuera),
    .resolucion_columna_aritmetica(valores[valores <= mediana], fraccion,
                                   fuera = fuera)
  )
  all(is.finite(mitades)) &&
    all(.exponente_decimal_aritmetica(mitades) ==
          .exponente_decimal_aritmetica(resolucion))
}

# La potencia de diez de una resolucion: el mayor `e` con `r / 10^e` entero
# -0,09 y 0,25 dan -2; 0,5, -1; 25, 0; 1000, 3-. Por una potencia exacta, como
# `.resolucion_columna_aritmetica()`.
.exponente_decimal_aritmetica <- function(r) {
  vapply(r, function(v) {
    for (e in 15:-15) {
      w <- if (e >= 0L) v / 10^e else v * 10^(-e)
      if (abs(round(w)) >= 1 &&
          abs(w - round(w)) <= 8 * .Machine$double.eps * abs(w)) {
        return(as.integer(e))
      }
    }
    NA_integer_
  }, integer(1L))
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
    # Las que alcanzaban el criterio sin soporte en las filas que distinguen
    # `k`: ver `.soporte_proporcional_aritmetica()`. Sin fila de cobertura.
    proporcionalidades_descartadas_soporte = 0,
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
# `descartada_soporte`, `sin_variacion`- sin los atributos que lo acompanan.
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

# **Un descarte por el cuanto dice por que.** El conteo en
# `meta$aritmetica_columnas` no se ve al leer los hallazgos ni el plan: una
# relacion real de 22 filas que la tolerancia relativa reconocia y que se
# descartaba dejaba la tabla muda. Una fila por relacion descartada, con su
# expresion.
.cobertura_descartes_cuanto_aritmetica <- function(descartes, nombres) {
  do.call(rbind, lapply(descartes, function(marca) {
    columnas <- nombres[attr(marca, "columnas", exact = TRUE)]
    .nuevo_diagnostico_no_evaluado(
      "relacion_aritmetica_columnas", paste(columnas, collapse = ","),
      paste0(
        "La tolerancia relativa reconoc\u00eda la relaci\u00f3n ",
        attr(marca, "expresion", exact = TRUE), " y la resoluci\u00f3n de sus ",
        "columnas no: con la mitad del cuanto de la columna redondeada no ",
        "alcanza el criterio, y no se publica. Puede tener m\u00e1s de una fila de ",
        "cada diez corrida un cuanto, o sostenerse solo porque a esa escala la ",
        "tolerancia relativa traga el cuanto."
      ),
      paste0(
        "Revisar las filas de la relaci\u00f3n: si es del dominio, las que no ",
        "cumplen con su resoluci\u00f3n son discrepancias. Declarar ",
        "`tolerancia_aritmetica` para evaluarla con la tolerancia relativa sola."
      ),
      columnas = columnas
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
        return(structure(
          "descartada_cuanto", columnas = c(sumandos, total),
          expresion = paste0(
            nombres_texto[[sumandos[[1L]]]], " + ", nombres_texto[[sumandos[[2L]]]],
            " ~= ", nombres_texto[[total]]
          )
        ))
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
  # Si la relativa la reconoce sin acusar nada, la relacion se publica con la
  # relativa, como siempre; la lectura se mide igual, pero solo para saber si
  # la resolucion de esta columna es compatible con la relacion: una lectura
  # compatible impide descartar la relacion por otra lectura que no lo es.
  solo_consistencia <- guardado && reconocida
  if (guardado && !reconocida) {
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
    as.double(base[barrido]), as.double(respuesta[barrido]), cota, mediana,
    umbral
  )
  esperado <- b * constante
  # El margen del doble va sobre el de los intervalos: una fila que el barrido
  # conto adentro no queda afuera por el redondeo de `k` ni del producto.
  cumple <- abs(r - esperado) <= cota +
    8 * .Machine$double.eps * pmax(1, abs(r), abs(esperado)) &
    en_reticula[comparables]
  # Y cada cifra de la constante, sostenida por una fila por lo menos: con 90 %
  # de ceros quedaban cuatro filas que distinguen `k`, y una constante de doce
  # cifras las explicaba exactas. Con `1.105` las mismas cuatro alcanzan.
  if (!.cumple_criterio_aritmetica(mean(cumple), umbral) ||
      sum(cumple & barrido[comparables]) < attr(constante, "cifras")) {
    if (solo_consistencia) return(no_aplica)
    return(no_alcanza)
  }
  if (solo_consistencia) return(list(aplica = FALSE, alcanza = FALSE, consistente = TRUE))
  if (guardado) {
    valores <- r[is.finite(r)]
    moda <- if (length(valores)) max(tabulate(match(valores, unique(valores)))) else 0L
    if (sum(cumple) <= moda) return(no_aplica)
  }
  list(
    aplica = TRUE, alcanza = TRUE,
    constante = as.vector(constante),
    cifras = attr(constante, "cifras", exact = TRUE),
    reciproca = attr(constante, "reciproca", exact = TRUE),
    mediana = mediana, esperado = esperado, cumple = cumple, cota = cota,
    cuanto = cuanto$cuanto, utilizables = utilizables,
    relativa_maxima = max(tolerancia * pmax(1, abs(r), abs(esperado_mediana)))
  )
}

# La constante que deja filas a menos de `semiancho` de la respuesta. Cada
# fila admite las `k` de un intervalo, `[(r - s) / b, (r + s) / b]`, y un
# barrido sobre sus extremos cuenta cuantos intervalos cubren cada tramo; cada
# tramo ofrece el decimal con menos cifras que contiene. **Cada cifra de mas
# tiene que explicar al menos una fila mas**: se elige el tramo con mas filas
# menos cifras; en un empate, el que mas filas cubre -una cifra que explica una
# fila se paga-; despues, el de menos cifras, y el mas cercano a `referencia`
# -la mediana-. El tramo de cobertura maxima no alcanza: una fila sana cuyo
# redondeo cae justo en la mitad toca el borde de su intervalo en la constante
# verdadera, y correrse un poco la pierde y gana una fila rota del otro lado
# (medido con enteros sobre 1e11); y con importes grandes una constante larga
# se traga filas rotas por un centavo -con 15 filas en (1000, 1105) y cinco
# rotas, `1.10500001` dejaba adentro 47 contra 45 de `1.105` y callaba dos-:
# cinco cifras por dos filas no se pagan. Tampoco dos rotas por el mismo lado
# y de magnitud parecida, que caen en el intervalo de una misma constante
# larga: `0.21999992` contra `0.22`. La regla anterior -la mas corta entre los
# tramos de hasta un 1 % menos que el mejor- hacia lo contrario con una
# constante que no termina: `5.55556` dejaba afuera una fila sana que
# `5.555556` explicaba, porque una fila es la holgura. Y una constante real
# larga, como un tipo de cambio, gana a su truncado cuando cada cifra explica
# filas grandes que el truncado deja afuera, salvo con pocas filas grandes:
# ahi lo salva la excepcion de las filas lejanas, abajo. El atributo `cifras`
# dice cuantas tiene la elegida.
.constante_por_intervalos_aritmetica <- function(base, respuesta, semiancho,
                                                 referencia, umbral = 0.9) {
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
  # Un tramo con 17 filas menos que el mejor no le gana con ninguna cifra, y
  # uno que no explica `umbral` de las filas del mejor no es la constante de la
  # relacion: con cuatro filas, una sola con `k = 5` "ganaba" por cifras.
  # Y si alguno alcanza el criterio en las filas del barrido, solo compiten los
  # que lo alcanzan: con `y = round(x * 0.012345, 2)` sobre x de 100 a 1000,
  # `1/81` -dos cifras y la division- explicaba 25 de 30 y le ganaba por
  # cifras a `0.012345`, que explicaba las 27 sanas, y la relacion se callaba.
  piso <- max(max(conteo) - 16L, umbral * max(conteo))
  if (max(conteo) >= umbral * length(base)) {
    piso <- max(piso, umbral * length(base) - 1e-9)
  }
  tramos <- which(conteo >= piso & conteo > 0L)
  tramos <- tramos[tramos < length(puntos)]
  inferior <- puntos[tramos]
  superior <- puntos[tramos + 1L]
  centro <- (inferior + superior) / 2
  cifras <- rep(17L, length(tramos))
  for (k in 15:1) {
    corta <- signif(centro, k)
    cifras[corta >= inferior & corta <= superior] <- k
  }
  # **La reciproca corta tambien es una constante corta.** `y = round(x / 7, 2)`
  # cabe en `0.142857...`, que en decimal tiene seis cifras, y `0.143` -tres,
  # dos sanas afuera- le ganaba; `1/3` perdia contra `0.3334`, que acusaba la
  # fila grande sana. Cada tramo ofrece tambien el decimal mas corto `c` con
  # `1/c` adentro, y cuesta las cifras de `c`, como `k` cuesta las suyas; con
  # costos iguales gana la forma directa.
  cifras_reciproca <- rep(17L, length(tramos))
  for (k in 15:1) {
    corta <- signif(1 / centro, k)
    recuperada <- 1 / corta
    cifras_reciproca[is.finite(recuperada) & recuperada >= inferior &
                       recuperada <= superior] <- k
  }
  reciproca <- cifras_reciproca < cifras
  forma_reciproca <- signif(1 / centro, pmin(cifras_reciproca, 15L))
  cifras <- pmin(cifras, cifras_reciproca)
  distancia <- pmax(0, inferior - referencia, referencia - superior)
  puntaje <- conteo[tramos] - cifras
  preferencia <- .orden_por_bytes(-puntaje, -conteo[tramos], cifras, distancia)
  constantes <- ifelse(reciproca, 1 / forma_reciproca,
                       ifelse(cifras <= 15L, signif(centro, cifras), centro))
  elegido <- preferencia[[1L]]
  # **El empate se lo lleva la mas larga solo si sus filas de mas son las de
  # mayor magnitud.** Una constante truncada falla primero las filas de mayor
  # `|base|`, porque su error crece con la magnitud: `5.555556` explica la fila
  # grande que `5.55556` deja afuera, y con un tipo de cambio largo y un solo
  # importe grande pasa lo mismo. Una rota suelta en el medio no compra
  # cifras: con `pesos = 100 * cant` y la fila 5 corrida 51, `0.0099` explicaba
  # las 20 filas contra 19 de `0.01`, una cifra por una fila, y se publicaba
  # sin filas acusadas. Tampoco la compra la rota de la fila mas grande si la
  # corta explica exactas todas las demas: un truncado no deja residuos cero.
  empate <- which(puntaje == puntaje[[elegido]])
  if (length(empate) > 1L) {
    corta <- empate[.orden_por_bytes(cifras[empate], distancia[empate])[[1L]]]
    desvio_corta <- abs(respuesta - base * constantes[[corta]])
    cubre_corta <- desvio_corta <= holgura
    exacta <- all(desvio_corta[cubre_corta] <= 8 * .Machine$double.eps *
                    pmax(1, abs(respuesta[cubre_corta]),
                         abs(base[cubre_corta] * constantes[[corta]])))
    # Cada una de las que gana tiene que ser de las de mayor magnitud o quedar
    # cerca de la corta. De las mayores: en el cuarto de magnitudes mas altas
    # de las que explica la larga. No necesariamente la mas grande: con una
    # constante truncada, cual de las filas grandes cae justo afuera depende de
    # su residuo, y `0.01235` dejaba afuera la tercera de 27 que `0.012347`
    # explica. La fila 5 de 20, o la 25 de 50, quedan en el medio.
    # Cerca: a menos de dos medios cuantos de la corta. Una fila lejos es una
    # rota, y una que la corta deja afuera por un pelo es la huella de un
    # truncado. `0.905` dejaba afuera, entre 20 filas sanas, una que `1/1.105`
    # explica y que no era de las mayores; con magnitudes sesgadas `0.9` dejaba
    # afuera dos grandes, lejos, y una chica por un pelo, las tres sanas.
    # Y sin perder ninguna de las que explica la corta, salvo las que le
    # quedan lejos a la larga: la constante entera explica todo lo que explica
    # su truncado, y lo que el truncado explica de mas son rotas. `1/81.004`
    # explicaba dos rotas grandes mas que `0.012345` y perdia una sana; `0.6214`
    # explicaba una rota que `0.62137`, la entera, deja a mas de un cuanto.
    paga <- function(j) {
      if (j == corta || exacta) return(FALSE)
      desvio_larga <- abs(respuesta - base * constantes[[j]])
      cubre <- desvio_larga <= holgura
      gana <- cubre & !cubre_corta
      pierde <- cubre_corta & !cubre & desvio_larga <= semiancho + holgura
      if (!any(gana) || any(pierde)) return(FALSE)
      all(desvio_corta[gana] <= semiancho + holgura[gana] |
            abs(base[gana]) >=
              stats::quantile(abs(base[cubre]), 0.75, names = FALSE, type = 1))
    }
    largas <- empate[vapply(empate, paga, logical(1L))]
    elegido <- if (length(largas)) {
      largas[[which.min(match(largas, preferencia))]]
    } else corta
  }
  # **Una que explica mas filas que otra igual de corta o mas, y pierde algunas
  # de las de esa, paga dos veces cada fila que pierde.** Una constante que se
  # corre para tragarse filas rotas deja afuera sanas que la verdadera explica;
  # la entera de un truncado no pierde ninguna. `1/81.004` explicaba dos rotas
  # grandes mas que `0.012345` y perdia una sana: le ganaba por una fila, con
  # las mismas cinco cifras. Salvo que alguna de las que solo explica la
  # elegida le quede cerca a la rival: entonces se corrio la rival, que deja
  # afuera sanas por un pelo y se traga una rota. `0.9143` explicaba una rota
  # que `0.9144`, la verdadera, deja a mas de un cuanto, y perdia dos sanas.
  repeat {
    cubre_elegida <- abs(respuesta - base * constantes[[elegido]]) <= holgura
    rivales <- which(cifras <= cifras[[elegido]] &
                       conteo[tramos] < conteo[tramos][[elegido]])
    gana_rival <- vapply(rivales, function(j) {
      desvio_rival <- abs(respuesta - base * constantes[[j]])
      cubre_rival <- desvio_rival <= holgura
      pierde <- sum(cubre_rival & !cubre_elegida)
      solo_elegida <- cubre_elegida & !cubre_rival
      pierde > 0 && puntaje[[elegido]] - puntaje[[j]] <= pierde &&
        !any(desvio_rival[solo_elegida] <= semiancho + holgura[solo_elegida])
    }, logical(1L))
    if (!any(gana_rival)) break
    rivales <- rivales[gana_rival]
    elegido <- rivales[[which.min(match(rivales, preferencia))]]
  }
  # Salvo que otra explique casi todas las filas que la elegida deja lejos -a
  # mas de dos medios cuantos, fuera de su ventana por otra ventana entera-:
  # todas salvo `1 - umbral`, y al menos dos. Un truncado deja lejos todas las
  # filas grandes y la constante entera las explica: `0.266` contra
  # `0.2666667`, con tres importes grandes. Una constante larga que se traga
  # rotas explica solo las que caen en su intervalo: con las tres rotas entre
  # las mayores, dos del mismo lado y de magnitud parecida caian en
  # `0.21999992` y la tercera no. Pedir dos, o pagar con filas las cifras de
  # mas, no las distinguia.
  repeat {
    mas <- which(conteo[tramos] > conteo[tramos][[elegido]])
    if (!length(mas)) break
    # Con el margen del doble: una rota justo un cuanto entero afuera no es
    # lejana, y el redondeo no la vuelve lejana o cercana al azar.
    desvio <- abs(respuesta - base * constantes[[elegido]])
    lejos <- desvio > semiancho + holgura
    cubre <- desvio <= holgura
    explica <- vapply(mas, function(j) {
      otra <- abs(respuesta - base * constantes[[j]]) <= holgura
      # Y sin perder ninguna de las que la elegida explica: la constante
      # entera explica todo lo que explica su truncado. Con las tres rotas
      # grandes del mismo lado, `1.08250004` se las tragaba a las tres y
      # perdia una sana que `1.0825` explicaba.
      if (any(cubre & !otra)) 0 else sum(lejos & otra)
    }, numeric(1L))
    candidatos <- mas[explica >= max(2, umbral * sum(lejos) - 1e-9)]
    if (!length(candidatos)) break
    elegido <- candidatos[[which.min(match(candidatos, preferencia))]]
  }
  # **Una corta que explica exactas todas sus filas no la gana una mas larga
  # que las contiene**, ni por puntaje ni por las lejanas: un truncado no deja
  # residuos cero, y lo que la larga agrega son rotas. Con x en medios e
  # `y = 0.18 * x` exacto a centavos, `1/5.555` -cuatro cifras- se tragaba las
  # tres rotas grandes y le ganaba por un punto a `0.18`, que las acusa.
  cubre_elegida <- abs(respuesta - base * constantes[[elegido]]) <= holgura
  exacta_contenida <- vapply(seq_along(tramos), function(j) {
    if (cifras[[j]] >= cifras[[elegido]]) return(FALSE)
    desvio_j <- abs(respuesta - base * constantes[[j]])
    cubre_j <- desvio_j <= holgura
    any(cubre_j) && !any(cubre_j & !cubre_elegida) &&
      all(desvio_j[cubre_j] <= 8 * .Machine$double.eps *
            pmax(1, abs(respuesta[cubre_j]),
                 abs(base[cubre_j] * constantes[[j]])))
  }, logical(1L))
  if (any(exacta_contenida)) {
    exactas <- which(exacta_contenida)
    elegido <- exactas[[which.min(match(exactas, preferencia))]]
  }
  # El centro del tramo lo fijan las filas de los extremos; los minimos
  # cuadrados de las filas que explica, todas.
  if (reciproca[[elegido]]) {
    # Salvo que la reciproca termine: `1/0.4` es `2.5`, y `resp ~= base / 0.4`
    # dice lo mismo que `resp ~= base * 2.5` con una division de mas. Hasta
    # doce cifras, y que su reciproca devuelva `0.4`: con quince, `1/1.22`
    # coincide por azar con su redondeo.
    valor <- constantes[[elegido]]
    termina <- which(signif(valor, 1:12) == valor &
                       1 / signif(valor, 1:12) == forma_reciproca[[elegido]])
    if (length(termina)) {
      return(structure(signif(valor, termina[[1L]]),
                       cifras = cifras[[elegido]]))
    }
    return(structure(constantes[[elegido]], cifras = cifras[[elegido]],
                     reciproca = forma_reciproca[[elegido]]))
  }
  cubre <- abs(respuesta - base * constantes[[elegido]]) <= holgura
  centrada <- sum(respuesta[cubre] * base[cubre]) / sum(base[cubre]^2)
  if (!is.finite(centrada)) centrada <- constantes[[elegido]]
  structure(
    .decimal_centrado_aritmetica(
      inferior[[elegido]], superior[[elegido]], cifras[[elegido]], centrada,
      constantes[[elegido]], base[cubre], respuesta[cubre], semiancho
    ),
    cifras = cifras[[elegido]]
  )
}

# Entre los decimales de `cifras` cifras de un tramo -todos explican las
# mismas filas, el tramo es eso-: primero el que deja mas filas justo en la
# mitad del cuanto, y despues el mas cercano a la constante de minimos
# cuadrados de esas filas. El redondeo del centro del tramo, que fijan las
# filas de los extremos, elegia `0.0724` en `round(cant * 0.0725, 2)`, cuyo
# tramo contiene a los dos: con `0.0725` cinco filas caen exactas en el empate
# del redondeo, como cae un producto decimal verdadero; con `0.0724`, ninguna.
# Sin empates, los minimos cuadrados dejan los residuos centrados (`0.012345` y
# no `0.012344`). `referencia` es la que devuelve sin candidatos.
.decimal_centrado_aritmetica <- function(inferior, superior, cifras, centrada,
                                         referencia, base, respuesta,
                                         semiancho) {
  if (cifras > 15L || !is.finite(referencia) || referencia == 0) {
    return(referencia)
  }
  e <- cifras - 1L - floor(log10(abs(referencia)))
  if (e < 0L || e > 15L) return(referencia)
  escala <- 10^e
  desde <- ceiling(min(inferior, superior) * escala - 1e-9)
  hasta <- floor(max(inferior, superior) * escala + 1e-9)
  if (!is.finite(desde) || !is.finite(hasta) || hasta < desde ||
      hasta - desde > 1000) {
    return(referencia)
  }
  valor <- seq(desde, hasta) / escala
  # Sin salirse del tramo por el redondeo de la division.
  valor <- valor[valor != 0 & valor >= min(inferior, superior) &
                   valor <= max(inferior, superior)]
  if (!length(valor)) return(referencia)
  if (length(valor) == 1L) return(valor)
  empates <- vapply(valor, function(k) {
    esperado <- base * k
    margen <- 16 * .Machine$double.eps * pmax(1, abs(respuesta), abs(esperado))
    sum(margen <= 1e-6 * semiancho &
          abs(abs(respuesta - esperado) - semiancho) <= margen)
  }, numeric(1L))
  valor[[.orden_por_bytes(-empates, abs(valor - centrada))[[1L]]]]
}

# **El soporte de una proporcionalidad son las filas que distinguen `k`.** Una
# fila con la base en cero cumple `0 = 0 * k` con cualquier `k`, y muchas filas
# con el mismo par de valores son un solo punto, por el que pasa cualquier
# recta. Con `x` e `y` en cero en 180 de 200 filas y sin relacion en las otras
# 20, se publicaba `x ~= y * 0.095` `sospechoso` con 20 discrepantes: las 20
# filas que podian decir algo, todas en contra. Lo mismo con 180 filas en el
# par (1, 2) y 20 codigos sin relacion. La relacion tiene que alcanzar el
# criterio en las filas con base distinta de cero, donde el par mas repetido
# pesa a lo sumo `1 - umbral` de ellas -la fraccion que el criterio deja
# fallar-, y con al menos `min_filas` filas fuera de ese par cumpliendo. Sacar
# el par entero no sirve: con 30 rotas de 300 la relacion cumple justo 0.9, y
# sin las 2 a 7 filas del par repetido de netos sanos queda en 0.899 y se
# callaba. Con el tope, un par chico no cambia nada y uno que domina obliga a
# las demas filas a cumplir `(2 * umbral - 1) / umbral` (0.889) por su cuenta.
# Una relacion real con muchos ceros -`iva = round(neto * 0.22, 2)` con netos
# en cero- se sigue publicando: sus filas con neto distinto de cero cumplen.
.soporte_proporcional_aritmetica <- function(base, respuesta, comparables, cumple,
                                             umbral, min_filas) {
  b <- as.double(base[comparables])
  r <- as.double(respuesta[comparables])
  distingue <- b != 0
  if (!any(distingue)) return(FALSE)
  pares <- sprintf("%.17g|%.17g", b[distingue], r[distingue])
  distintos <- unique(pares)
  veces <- tabulate(match(pares, distintos), nbins = length(distintos))
  cumple <- cumple[distingue]
  fuera_de_la_moda <- rep(TRUE, length(pares))
  if (max(veces) > 1L) {
    fuera_de_la_moda <- pares != distintos[[which.max(veces)]]
  }
  resto <- sum(fuera_de_la_moda)
  cumple_resto <- sum(cumple[fuera_de_la_moda])
  if (cumple_resto < min_filas) return(FALSE)
  # El par mas repetido pesa a lo sumo `1 - umbral` del soporte.
  moda <- length(pares) - resto
  tope <- if (umbral >= 1) 0 else floor((1 - umbral) / umbral * resto + 1e-9)
  peso <- min(moda, tope)
  cumple_moda <- if (moda > 0L) cumple[!fuera_de_la_moda][[1L]] else FALSE
  .cumple_criterio_aritmetica(
    (cumple_resto + if (cumple_moda) peso else 0) / (resto + peso), umbral
  )
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
  # Una que alcanza el criterio y no tiene soporte no se publica ni deja fila
  # de cobertura -en una tabla rala, cada par de columnas con ceros en las
  # mismas filas dejaria una-, pero se cuenta en
  # `proporcionalidades_descartadas_soporte`.
  sin_soporte <- structure(
    "descartada_soporte", columnas = c(indice_base, indice_respuesta)
  )
  if (!all(varian)) {
    if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
    if (!.soporte_proporcional_aritmetica(base, respuesta, comparables, cumple,
                                          umbral, min_filas)) {
      return(sin_soporte)
    }
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
  reciproca_elegida <- NULL
  relativa_maxima <- max(tolerancia * pmax(
    1, abs(respuesta[comparables]), abs(esperado)
  ))
  if (is.function(cuanto_de)) {
    alcanza_relativa <- .cumple_criterio_aritmetica(proporcion, umbral)
    reconocida_sin_cota <- alcanza_relativa &&
      .soporte_proporcional_aritmetica(base, respuesta, comparables, cumple,
                                       umbral, min_filas)
    con_fallas <- reconocida_sin_cota && proporcion < 1
    # Cada lectura con sus resoluciones, la mas fina primero: la primera que
    # alcanza el criterio.
    leer <- function(b, r, opciones) {
      resultado <- list(aplica = FALSE, alcanza = FALSE)
      consistente <- FALSE
      for (opcion in opciones) {
        lectura <- .lectura_proporcional_aritmetica(
          b, r, comparables, opcion, tolerancia, umbral, min_filas,
          reconocida_sin_cota, con_fallas
        )
        consistente <- consistente || isTRUE(lectura$consistente)
        if (lectura$alcanza) return(lectura)
        if (lectura$aplica) resultado$aplica <- TRUE
      }
      resultado$consistente <- consistente
      resultado
    }
    lectura_directa <- leer(base, respuesta, cuanto_de(indice_respuesta))
    lectura_inversa <- leer(respuesta, base, cuanto_de(indice_base))
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
    # Con cotas distintas -una el doble o mas que la otra, en la misma columna-
    # y las dos con soporte, se comparan con la fina: la constante de la
    # gruesa, llevada a la fina, tiene que explicar mas filas que la de la
    # fina, y pagar con ellas las cifras que agrega; si no, gana la fina. Una
    # fila que solo cae adentro con la cota gruesa no cuenta a favor de la
    # gruesa. Con dolares chicos a
    # centavos y dos importes grandes, la lectura de los dolares leia `0.0253`
    # -medio centavo de dolar es ancho frente a importes de un dolar- y ganaba
    # por cifras a `39.4567`, acusando las dos grandes. Y con `k = 2.5` y las dos
    # columnas a centavos, `0.4` explica en la fina las mismas filas que `2.5`:
    # gana la fina, que es la que ve el centavo corrido. Sin soporte en alguna
    # -una lectura que alcanza el criterio por las filas en cero o por el par
    # repetido- rige la regla de las cifras: con 90 % de netos en cero, la de
    # los netos sobre el iva "alcanzaba" con `k = 4.545...` y era la fina; y
    # con cinco rotas entre 35 filas, la del iva no tiene soporte y la de los
    # netos lo tiene porque su constante larga se traga dos rotas.
    con_soporte <- function(lectura, invertida, cumple = lectura$cumple) {
      b <- if (invertida) respuesta else base
      r <- if (invertida) base else respuesta
      .soporte_proporcional_aritmetica(b, r, comparables, cumple,
                                       umbral, min_filas)
    }
    con_la_fina <- function(gruesa, invertida, fina) {
      r_f <- if (invertida) base[comparables] else respuesta[comparables]
      b_f <- if (invertida) respuesta[comparables] else base[comparables]
      # Con la cota y la reticula de la fina: un valor fuera de su reticula no
      # es un redondeo a su resolucion (ver `.lectura_proporcional_aritmetica()`).
      en_reticula_f <- .en_resolucion_aritmetica(r_f, fina$cuanto)
      # Una fila que en la gruesa cae justo en su medio cuanto es un empate del
      # redondeo, y cuenta como explicada: con `y = round(0.9 * x)` en enteros,
      # R redondea al par y cinco filas de 50 quedan a 0,5 exacto de `0.9 * x`;
      # en la lectura de `x` quedaban a 0,56, y `x ~= y * 1.11` las acusaba.
      # Solo donde el doble distingue el empate: con importes de 1e10 el error
      # de la resta es mil veces el que separa un empate de un redondeo comun,
      # y filas cualquiera pasaban por empates. Y solo con cotas parecidas: con
      # una el doble de la otra, una rota por un cuanto de la fina cae justo en
      # el medio cuanto de la gruesa. Con `y = x / 10` y tres filas corridas
      # cinco centavos, `x ~= y * 10` las contaba como empates y las callaba.
      r_g <- if (invertida) respuesta[comparables] else base[comparables]
      margen_g <- 16 * .Machine$double.eps *
        pmax(1, abs(r_g), abs(gruesa$esperado))
      empate_g <- max(cota_d, cota_i) < 2 * min(cota_d, cota_i) &
        gruesa$cumple & margen_g <= 1e-6 * gruesa$cota &
        abs(abs(r_g - gruesa$esperado) - gruesa$cota) <= margen_g
      explica <- function(esperado_f) {
        (abs(r_f - esperado_f) <= fina$cota +
           8 * .Machine$double.eps * pmax(1, abs(r_f), abs(esperado_f)) &
           en_reticula_f) | empate_g
      }
      cuenta <- function(esperado_f) sum(explica(esperado_f))
      # La de la gruesa en la fina: su reciproca, y si la gruesa es ya una
      # reciproca `1/c`, `c` tal cual -dividir por `1/c` corre el ultimo bit y,
      # con importes de 1e11, una fila del borde-.
      explicadas <- explica(if (!is.null(gruesa$reciproca)) {
        b_f * gruesa$reciproca
      } else b_f / gruesa$constante)
      filas <- sum(explicadas)
      # Y sus cifras, las de su forma corta en la fina: la de la reciproca
      # -`7` de la lectura de `x` es `1/7` en la de `y`, una cifra- o la del
      # decimal mas corto que, como constante de la fina, explica esas mismas
      # filas. `81.004` es `1/81.004` en la lectura de `y`, cinco cifras, como
      # `0.012345`; la fila que pierde la decide la regla de las filas perdidas.
      if (!is.null(gruesa$reciproca)) {
        return(list(filas = filas, cifras = gruesa$cifras,
                    explicadas = explicadas, en_reticula = en_reticula_f))
      }
      reciproca <- 1 / gruesa$constante
      cifras <- gruesa$cifras
      for (d in seq_len(min(15L, cifras - 1L))) {
        if (cuenta(b_f * signif(reciproca, d)) >= filas) {
          cifras <- d
          break
        }
      }
      list(filas = filas, cifras = cifras, explicadas = explicadas,
           en_reticula = en_reticula_f)
    }
    if (lectura_directa$alcanza) elegida <- lectura_directa
    if (lectura_inversa$alcanza) {
      cota_d <- if (!is.null(elegida)) cota_comun(lectura_directa, FALSE)
      cota_i <- cota_comun(lectura_inversa, TRUE)
      soporte_d <- !is.null(elegida) && con_soporte(lectura_directa, FALSE)
      soporte_i <- con_soporte(lectura_inversa, TRUE)
      fina_inversa <- !is.null(elegida) && cota_i < cota_d
      fina <- if (fina_inversa) lectura_inversa else lectura_directa
      gruesa <- if (fina_inversa) lectura_directa else lectura_inversa
      # Con cotas parecidas, el soporte de la apenas mas gruesa se cuenta con
      # las filas que explica con la fina, mas sus empates: con 15 filas en
      # (1000, 1105) y cinco rotas, la lectura de `x` -una tolerancia 10 % mas
      # ancha en la columna de `y`- se tragaba una rota, solo con ella tenia
      # soporte, y le ganaba a `1.105` por tenerlo.
      #
      # Salvo que la gruesa sea la redondeada. **Si la columna redondeada es la
      # de la gruesa, sus residuos llenan su ventana**, y una fraccion
      # `1 - fina / gruesa` de sus filas cae entre las dos tolerancias -la
      # banda-; si la redondeada es la de la fina, en la banda solo caen filas
      # rotas. Con `y = round(x / 1.105)` en enteros, `y` es la redondeada,
      # 0,5 su tolerancia contra 0,45 de la de `x` llevada a `y`, y uno de cada
      # diez residuos cae en la banda: contadas sin ella, las filas de `y`
      # perdian contra `x ~= y * 1.105`, que acusaba esas filas sanas. Salvo que
      # la banda se lea como rotas (abajo), la gruesa cuenta sus filas enteras.
      cuentan_gruesa <- NULL
      cuentan_fina <- NULL
      en_la_fina <- NULL
      if (!is.null(elegida) && max(cota_d, cota_i) < 2 * min(cota_d, cota_i)) {
        en_la_fina <- con_la_fina(gruesa, fina_inversa, fina)
        # Una fila fuera de la reticula de cualquiera de las dos respuestas no
        # es un redondeo y no cuenta para ninguna: con `x` en medios y
        # `k = 0.22` las dos cotas valen 0,055, y las tres rotas fuera de la
        # reticula de `y` las explicaba la lectura de `x`.
        r_g <- if (fina_inversa) respuesta[comparables] else base[comparables]
        en_reticula_g <- .en_resolucion_aritmetica(r_g, gruesa$cuanto)
        # La banda es redondeo de la gruesa o son rotas que cayeron ahi: si la
        # redondeada es la de la gruesa, cae en la banda una fraccion
        # `1 - fina / gruesa` de sus filas; si es la de la fina, solo rotas, y
        # tantas como caen justo afuera de la ventana gruesa -hasta un cuanto
        # mas lejos- en un ancho igual al de la banda. Gana la explicacion mas
        # verosimil, con conteos de Poisson, y sin rotas cerca del borde la
        # banda es redondeo. Con `y = round(x / 1.09)` sobre 300 enteros caian
        # 10 filas en la banda contra 25 esperadas y ninguna justo afuera, y
        # pedir la mitad de las esperadas acusaba las 10 sanas; con
        # `y = round(1.22 * x)` y cinco rotas por una unidad, una cae en la
        # banda y cuatro justo afuera: es una rota.
        #
        # Los residuos, con la constante mas precisa de las dos -la de mas
        # cifras; con las mismas, la de la gruesa-: la deriva de un truncado
        # corre filas sanas hacia la banda. Con la base y la respuesta en pasos
        # de 25 hasta 1e5, `0.905` -el truncado de `1/1.105`- llenaba la banda
        # de sanas corridas, y la gruesa se tragaba una rota. Una reciproca de
        # la fina no cuenta como mas precisa: `1/0.8466` se acomoda a las filas
        # que explica, y con ella la banda de `y = round(x / 1.18)` parecia de
        # rotas.
        angosta <- 1 - min(cota_d, cota_i) / max(cota_d, cota_i)
        en_ambas <- en_reticula_g & en_la_fina$en_reticula
        if (fina$cifras > gruesa$cifras && is.null(fina$reciproca)) {
          b_g <- if (fina_inversa) base[comparables] else respuesta[comparables]
          segun_fina <- if (!is.null(fina$reciproca)) {
            b_g * fina$reciproca
          } else b_g / fina$constante
          desvio_g <- abs(r_g - segun_fina) / gruesa$cota
          banda <- sum(!fina$cumple & desvio_g <= 1 & en_ambas)
          esperada <- sum(desvio_g <= 1 & en_ambas) * angosta
        } else {
          desvio_g <- abs(r_g - gruesa$esperado) / gruesa$cota
          banda <- sum(gruesa$cumple & !en_la_fina$explicadas & en_ambas)
          esperada <- sum(gruesa$cumple) * angosta
        }
        rotas_cerca <- angosta / 2 *
          sum(desvio_g > 1 & desvio_g <= 3 & en_ambas)
        excluir_banda <- banda > 0 && rotas_cerca > 0 &&
          banda * log(rotas_cerca / esperada) + esperada - rotas_cerca > 0
        cuentan_gruesa <- gruesa$cumple & en_la_fina$en_reticula &
          (en_la_fina$explicadas | !excluir_banda)
        cuentan_fina <- fina$cumple & en_reticula_g
        soporte_gruesa <- con_soporte(gruesa, !fina_inversa, cuentan_gruesa)
        if (fina_inversa) {
          soporte_d <- soporte_gruesa
        } else {
          soporte_i <- soporte_gruesa
        }
      }
      if (is.null(elegida)) {
        invertir <- TRUE
      } else if (soporte_d && soporte_i &&
                 max(cota_d, cota_i) >= 2 * min(cota_d, cota_i)) {
        filas_fina <- sum(fina$cumple)
        en_la_fina <- con_la_fina(gruesa, fina_inversa, fina)
        gana_gruesa <- en_la_fina$filas > filas_fina &&
          en_la_fina$filas - en_la_fina$cifras > filas_fina - fina$cifras
        invertir <- if (gana_gruesa) !fina_inversa else fina_inversa
      } else if (abs(lectura_inversa$cifras - elegida$cifras) >= 3L) {
        invertir <- lectura_inversa$cifras < elegida$cifras
      } else if (soporte_d != soporte_i) {
        # Con cifras parecidas, la que tiene soporte: con 90 % de ceros, la
        # de los netos sobre el iva alcanzaba con `4.547` por los ceros, era
        # la de cota fina, y la relacion se callaba.
        invertir <- soporte_i
      } else if (soporte_d) {
        # Con cotas parecidas y cifras parecidas, antes que la cota, la misma
        # cuenta con la cota fina: `x ~= y * 0.26666` dejaba afuera las tres
        # filas grandes que `y ~= x * 3.75` explica, y con la misma cota ganaba
        # por ser la directa. Con la cota de cada una, la apenas mas gruesa
        # explicaba una rota mas y ganaba; y sin pagar las cifras, `0.904977`
        # se tragaba una rota que `1.105` acusa. Con cotas parecidas no hace
        # falta que la gruesa explique mas filas: basta con que pague mejor sus
        # cifras, y en un empate gana la de menos. La lectura de `x` de
        # `y = round(1.22 * x / 0.05) * 0.05` es un 2 % mas fina, leia `1/1.22`
        # y dejaba afuera una fila sana que `1.22` explica en la de `y`.
        # Y con su propia tolerancia, salvo la banda cuando no parece de la
        # redondeada (arriba): con `y = round(x / 1.105)` en enteros la de `y`
        # explica todas las filas, y contada sin la banda perdia contra
        # `x ~= y * 1.105`, que acusaba las sanas de la banda. En un empate,
        # la de menos cifras, y despues la que acusa mas: con la misma
        # evidencia, se acusa en vez de callar.
        # Y con todo igual, la directa: `y ~= x * 3.75` y no `x ~= y / 3.75`.
        clave <- function(lectura, cuentan, cifras_pagadas) {
          c(-(sum(cuentan) - cifras_pagadas), lectura$cifras,
            sum(lectura$cumple), !is.null(lectura$reciproca))
        }
        clave_gruesa <- clave(gruesa, cuentan_gruesa, en_la_fina$cifras)
        clave_fina <- clave(fina, cuentan_fina, fina$cifras)
        distinta <- which(clave_gruesa != clave_fina)
        gana_gruesa <- length(distinta) > 0L &&
          clave_gruesa[[distinta[[1L]]]] < clave_fina[[distinta[[1L]]]]
        invertir <- if (gana_gruesa) !fina_inversa else fina_inversa
      } else {
        invertir <- fina_inversa
      }
      if (invertir) elegida <- lectura_inversa
    }
    if (is.null(elegida)) {
      # Una lectura posible -una columna con decimales fijos y la relativa
      # por encima de su redondeo- que no alcanza el criterio: la relativa la
      # reconocia y la resolucion de las columnas no. Se descarta y se cuenta,
      # como la identidad con la cota del cuanto. Sin lectura posible, la
      # relativa de siempre.
      # Salvo que otra lectura -una que la compuerta deja afuera porque la
      # relativa ya la reconoce sin acusar nada- sea compatible con la
      # relacion: entonces la relativa tenia razon, y la que no alcanzo era la
      # lectura equivocada. Una relacion real de 25 filas con la respuesta a
      # multiplos de 5 se descartaba porque la base, a centavos, no la explicaba.
      if ((lectura_directa$aplica || lectura_inversa$aplica) &&
          !isTRUE(lectura_directa$consistente) &&
          !isTRUE(lectura_inversa$consistente)) {
        if (reconocida_sin_cota) {
          return(structure(
            "descartada_cuanto", columnas = c(indice_base, indice_respuesta),
            expresion = paste0(
              nombres_texto[[indice_respuesta]], " ~= ",
              nombres_texto[[indice_base]], " * ",
              format(constante, scientific = FALSE, trim = TRUE, digits = 15L)
            )
          ))
        }
        # La relativa alcanzaba el criterio y no tenia soporte.
        if (alcanza_relativa) return(sin_soporte)
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
      reciproca_elegida <- elegida$reciproca
      esperado <- elegida$esperado
      cumple <- elegida$cumple
      proporcion <- mean(cumple)
      cota <- elegida$cota
      cota_elegida <- list(cuanto = elegida$cuanto)
      relativa_maxima <- elegida$relativa_maxima
    }
  }
  if (!.cumple_criterio_aritmetica(proporcion, umbral)) return(NULL)
  # El soporte, en todos los caminos: ver `.soporte_proporcional_aritmetica()`.
  if (!.soporte_proporcional_aritmetica(base, respuesta, comparables, cumple,
                                        umbral, min_filas)) {
    return(sin_soporte)
  }
  filas_comparables <- which(comparables)
  indices_incumplen <- filas_comparables[!cumple]
  esperados_incumplen <- esperado[!cumple]
  nombres <- names(datos)
  constante_texto <- format(
    constante, scientific = FALSE, trim = TRUE, digits = 15L
  )
  # Con la reciproca corta, la relacion se escribe con la division: `y ~= x / 7`
  # y no `y ~= x * 0.142857142857143`.
  operador <- c(" * ", " / ")
  factor_texto <- constante_texto
  if (!is.null(reciproca_elegida)) {
    operador <- rev(operador)
    factor_texto <- format(
      reciproca_elegida, scientific = FALSE, trim = TRUE, digits = 15L
    )
    constante_texto <- paste0("1/", factor_texto)
  }
  expresion <- paste0(
    nombres_texto[[indice_respuesta]], " ~= ", nombres_texto[[indice_base]],
    operador[[1L]], factor_texto
  )
  inversa <- paste0(
    nombres_texto[[indice_base]], " ~= ", nombres_texto[[indice_respuesta]],
    operador[[2L]], factor_texto
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
  # Lo que la cota del cuanto descarta: ademas del conteo en el alcance, una
  # fila de cobertura por relacion (ver `.cobertura_descartes_cuanto_aritmetica()`).
  descartes <- list()
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
      # tres orientaciones se publico. Lo mismo un descarte por el cuanto.
      if (!length(publicables) && any(sin_var)) {
        registrar_sin_variacion(candidatos[[which(sin_var)[[1L]]]], "identidades")
      }
      if (!length(publicables) && any(por_cuanto)) {
        descartes[[length(descartes) + 1L]] <- candidatos[[which(por_cuanto)[[1L]]]]
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
  # **Dos resoluciones por columna, la mas fina primero.** La de todos los
  # valores y la de casi todos (ver `.resolucion_columna_aritmetica()`). La de
  # casi todos sobrevive a unas filas rotas fuera de la reticula, pero tambien
  # puede ser una estructura de los datos: con la base en pasos de 25 salvo
  # cuatro filas a centavos, `y = round(1.22 * x, 2)` cae en pasos de 0,5 en 36
  # de 40 filas, y leida con medio 0,5 la compuerta no se abria, la relacion se
  # leia al reves con `k = 0.8196...` y un centavo corrido en `y` salia `ok`. Se
  # prueba primero la de todos -medio centavo-, y la de casi todos solo si la
  # fina no alcanza el criterio.
  # Una fila por registro distinto de las columnas numericas: un registro
  # repetido es un solo punto de la reticula (ver
  # `.resolucion_columna_aritmetica()`).
  registro_distinto <- if (por_omision && length(seleccion) && nrow(datos)) {
    !duplicated(as.data.frame(datos)[seleccion])
  }
  cuanto_de <- function(i) {
    if (!por_omision) return(NULL)
    clave <- as.character(i)
    if (is.null(fijos[[clave]])) {
      todas <- as.double(datos[[i]])
      filas <- sum(is.finite(todas) & todas != 0)
      x <- todas[registro_distinto]
      x <- abs(x[is.finite(x)])
      opciones <- list()
      for (fraccion in c(1, .FRACCION_RESOLUCION)) {
        resolucion <- .resolucion_columna_aritmetica(x, fraccion, filas = filas)
        if (!is.finite(resolucion)) next
        if (length(opciones) &&
            abs(opciones[[length(opciones)]]$cuanto / resolucion - 1) < 1e-9) next
        opciones[[length(opciones) + 1L]] <- list(
          cuanto = resolucion,
          fijo = .decimales_fijos_aritmetica(x, resolucion, fraccion,
                                             filas = filas)
        )
      }
      fijos[[clave]] <- opciones
    }
    if (!length(fijos[[clave]])) return(NULL)
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
      hallazgo <- .hallazgo_proporcional(
        datos, par, umbral, min_filas, tolerancia, nombres_texto,
        cuanto_de = cuanto_de
      )
      marca <- .marca_aritmetica(hallazgo)
      # La redundante no se publica, pero si alcanzaba el criterio sin soporte
      # se cuenta igual: con una identidad entre tres columnas con 180 ceros,
      # las dos proporcionalidades que incluyen su total se omitian sin
      # evaluarse y `proporcionalidades_descartadas_soporte` quedaba en 0.
      if (redundante && !identical(marca, "descartada_soporte")) next
      if (identical(marca, "descartada_cuanto")) {
        alcance$proporcionalidades_descartadas_cuanto <-
          alcance$proporcionalidades_descartadas_cuanto + 1
        descartes[[length(descartes) + 1L]] <- hallazgo
        next
      }
      if (identical(marca, "descartada_soporte")) {
        alcance$proporcionalidades_descartadas_soporte <-
          alcance$proporcionalidades_descartadas_soporte + 1
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
  if (length(descartes)) {
    cobertura <- rbind(cobertura, .cobertura_descartes_cuanto_aritmetica(
      descartes, names(datos)
    ))
  }
  list(
    hallazgos = c(hallazgos_aditivos, hallazgos_proporcionales),
    alcance = alcance,
    cobertura = cobertura
  )
}
