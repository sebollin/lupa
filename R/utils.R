# La dispersion tambien puede perderse aunque la magnitud absoluta sea comun:
# si el rango es menor que la raiz del menor doble normal, sus cuadrados caen
# en subnormales y una suma de cuadrados pierde cifras. Un rango nulo no activa
# la guarda: una columna constante no necesita cambiar de consulta.
.rango_requiere_escala <- function(rango) {
  rango <- suppressWarnings(as.numeric(rango))
  length(rango) == 1L && is.finite(rango) && rango > 0 &&
    rango < sqrt(.Machine$double.xmin)
}

# El orden por bytes (`method = "radix"`) exige que la codificacion este
# DECLARADA: R rechaza con "Character encoding must be UTF-8, Latin-1 or bytes"
# una cadena marcada `unknown` que contenga bytes no ASCII, aunque sean UTF-8
# perfectamente validos. Y asi llega cualquier CSV en espanol leido con
# `read.csv()`: `"Combustibles liquidos"` con tilde sale con `Encoding()` en
# `unknown` y rompe el perfil entero.
#
# Se marca la codificacion antes de ordenar. Lo que despues de eso siga sin ser
# UTF-8 valido se pasa por `iconv(sub = "byte")`, que reemplaza el byte suelto
# por su codigo: no es bonito, pero es determinista y ordenable, que es lo que
# el orden necesita. La alternativa -caer al orden del entorno- devolveria la
# dependencia de la maquina que este orden existe para sacar.
# La clave de ordenamiento por bytes, separada del ordenamiento en si. Existe
# porque hay desempates que ordenan por DOS criterios -primero la frecuencia,
# despues el texto- y ahi no sirve una funcion que ya devuelva el vector
# ordenado: hace falta la clave para pasarsela a `order()`.
.es_ascii <- function(x) {
  if (!length(x)) return(TRUE)
  !any(.no_ascii(x), na.rm = TRUE)
}

# Que elementos traen algun byte fuera de ASCII, mirando los bytes y no los
# caracteres: no traduce, no depende del locale, y un `NA` da FALSE. Una cadena
# ASCII no puede llevar marca de codificacion -R no se la pone- ni contener
# ningun caracter que no sea ASCII, asi que todo paso que solo mira o cambia
# caracteres no ASCII (declarar UTF-8, plegar acentos, proteger grafemas) la
# deja igual. Con esta mascara esos pasos recorren solo lo que pueden cambiar:
# sobre una columna ASCII de 200.000 filas, declarar la codificacion costaba lo
# mismo que si cada celda la necesitara, y se pedia decenas de veces por perfil.
#
# Por lo mismo, una cadena con marca (`UTF-8`, `latin1`, `bytes`) es no ASCII
# sin mirarla, y los bytes se leen solo en las que no la tienen: despues de la
# primera declaracion de una columna eso es casi siempre lo ASCII, y una columna
# no ASCII ya declarada no paga la lectura.
.no_ascii <- function(x) {
  if (!is.character(x)) {
    return(grepl("[^\\001-\\177]", x, useBytes = TRUE, perl = TRUE))
  }
  salida <- Encoding(x) != "unknown"
  sin_marca <- !salida
  if (any(sin_marca)) {
    salida[sin_marca] <- grepl(
      "[^\\001-\\177]", if (all(sin_marca)) x else x[sin_marca],
      useBytes = TRUE, perl = TRUE
    )
  }
  salida
}

# Las celdas que tiene que recorrer un paso que solo lee o cambia caracteres no
# ASCII: la mascara que llega -la de `.no_ascii()` o una que la contiene: una
# celda de mas solo se recorre de mas-, o la de `.no_ascii()`. Con pocas celdas
# se recorren todas, como antes: leer los bytes es otra llamada a PCRE, con un
# costo fijo (unos 47 microsegundos en Windows) que en un vector chico pesa mas
# que lo que ahorra, y sumaba llamadas de un solo valor donde no las habia.
.MIN_CELDAS_MASCARA_NO_ASCII <- 64L
.mascara_no_ascii <- function(textos, no_ascii = NULL) {
  if (!is.null(no_ascii)) return(no_ascii)
  if (length(textos) < .MIN_CELDAS_MASCARA_NO_ASCII) {
    return(rep(TRUE, length(textos)))
  }
  .no_ascii(textos)
}

# Aplica `f` -un paso que solo lee o cambia caracteres no ASCII- a las celdas
# con algun byte fuera de ASCII, y deja las demas como estan: sobre ellas el paso
# no hace nada. `no_ascii`: ver `.mascara_no_ascii()`. Si la mascara las incluye
# todas, `f` recibe el vector entero, como antes, sin copiarlo.
.en_celdas_no_ascii <- function(textos, f, no_ascii = NULL) {
  no_ascii <- .mascara_no_ascii(textos, no_ascii)
  if (!any(no_ascii)) return(textos)
  if (all(no_ascii)) return(f(textos))
  textos[no_ascii] <- f(textos[no_ascii])
  textos
}

# R representa un byte que no puede mostrar con su octal -`\\377`-, y esa es la
# forma que hay que usar. Se escapan SOLO los bytes que no forman una secuencia
# UTF-8 valida, no todos los altos: una celda con texto acentuado y un byte roto
# al final salia como puro ruido octal, mientras base conserva la parte legible
# y escapa solo lo roto. Lo que queda despues de escapar ya es UTF-8 valido, y
# por eso se declara. Elegir `<ff>` parecia mas legible y era un defecto:
# un usuario puede escribir literalmente `A<ff>B`, y entonces su texto y el byte
# crudo 0xff publicaban lo MISMO, con base distinguiendolos. Medido:
#
#   crudo 0xff    -> paquete `A<ff>B`   base `A\\377B`
#   literal <ff>  -> paquete `A<ff>B`   base `A<ff>B`
#
# Con el octal la colision se reduce a la que tiene el propio R: sigue habiendo
# ambiguedad si el usuario escribe literalmente `\\377`, y no mas que esa.
# Se escapa aca y no con `encodeString()`, que consulta el locale.
# Escapa TODO byte alto, sin mirar si forma una secuencia UTF-8 valida.
#
# Se distingue de `.escapar_bytes_altos()` en eso justamente: aquel conserva lo
# que es UTF-8 valido -porque su trabajo es rescatar texto legible de una
# cadena con bytes rotos- y este NO puede conservarlo, porque su entrada son
# bytes que alguien declaro que no se interpretaran. Si conservara la secuencia
# valida, un valor declarado `bytes` y el mismo texto declarado `UTF-8` darian
# la misma clave, que es el defecto que este helper existe para cerrar.
.escapar_byte_a_byte <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    crudo <- as.integer(charToRaw(s))
    if (!length(crudo) || all(crudo < 128L)) return(s)
    paste(
      vapply(
        crudo,
        # Hexadecimal y no octal, y eso es lo que separa esta clave de la de
        # `.escapar_bytes_altos()`. Sobre una cadena TODA invalida las dos
        # escapaban cada byte y daban el mismo resultado, asi que los mismos
        # bytes declarados `bytes` y sin declarar terminaban con la misma clave
        # -medido: `unique()` de R veia 3 valores y el perfil publicaba 1, y el
        # hallazgo `constante` decia 3 filas con una traza que respaldaba 1-.
        #
        # `\xHH` no es una convencion inventada para desempatar: es COMO R
        # imprime una cadena declarada `bytes` -`A\xffB`-, mientras que para los
        # bytes rotos sin declarar usa el octal. La clave dice lo mismo que la
        # consola, que es la regla que rige todo este camino.
        function(b) if (b < 128L) rawToChar(as.raw(b)) else sprintf("\\x%02x", b),
        character(1L)
      ),
      collapse = ""
    )
  }, character(1L), USE.NAMES = FALSE)
}

.escapar_bytes_altos <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    crudo <- charToRaw(s)
    if (!length(crudo) || all(as.integer(crudo) < 128L)) return(s)
    piezas <- character()
    i <- 1L
    n <- length(crudo)
    while (i <= n) {
      primero <- as.integer(crudo[[i]])
      largo <- if (primero < 0x80L) 1L else
               if (primero >= 0xC2L && primero <= 0xDFL) 2L else
               if (primero >= 0xE0L && primero <= 0xEFL) 3L else
               if (primero >= 0xF0L && primero <= 0xF4L) 4L else 0L
      trozo <- if (largo > 0L && i + largo - 1L <= n) {
        rawToChar(crudo[i:(i + largo - 1L)])
      } else NULL
      if (!is.null(trozo) && isTRUE(validUTF8(trozo))) {
        piezas <- c(piezas, trozo)
        i <- i + largo
      } else {
        piezas <- c(piezas, sprintf("\\%03o", primero))
        i <- i + 1L
      }
    }
    paste0(piezas, collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

.clave_bytes <- function(x) {
  crudo <- tryCatch(as.character(x), error = function(e) NULL)
  # La barra invertida se escapa ANTES que nada, y ese orden es el arreglo.
  #
  # Sin este paso el escape describia una secuencia de bytes con un texto que el
  # usuario puede escribir, y los dos daban la misma clave:
  #
  #   los bytes 61.c3.b1.6f declarados `bytes`      -> a\303\261o
  #   el texto literal de diez ASCII "a\303\261o"   -> a\303\261o
  #
  # R los distingue -`==` da FALSE, `unique()` los cuenta aparte- y la clave los
  # fundia: `perfilar()` publicaba `n_distintos = 2` donde `unique()` ve 3. Es la
  # misma contradiccion que esta clave existe para cerrar, mudada al texto que
  # DESCRIBE la secuencia. El comentario viejo la aceptaba como residual -"y no
  # mas que esa"-; no era residual y no era una sola.
  #
  # Y va antes de la via rapida de ASCII, no despues. Puesto despues, la misma
  # cadena daba DOS claves segun que la acompanara en el vector: sola -todo
  # ASCII- salia por la via rapida sin escapar, y en un vector mixto se
  # escapaba. Una clave decidida por la tanda y no por el valor.
  if (!is.null(crudo)) {
    escapables <- !is.na(crudo)
    if (any(escapables)) {
      # `useBytes = TRUE` por dos razones, las dos medidas. Primera: sin el,
      # `gsub()` ABORTA sobre una cadena con bytes invalidos -"input string 1
      # is invalid in this locale"- y `perfilar()` moria sobre una columna que
      # el paquete sabe describir. Segunda: sin el, `gsub()` elige su estrategia
      # segun QUE MAS haya en el vector, asi que la misma cadena podia salir de
      # dos maneras segun sus vecinas. Lo que se escapa es un BYTE -0x5C-, no un
      # caracter, y en UTF-8 ese byte no aparece como continuacion, asi que
      # reemplazar por bytes es exacto. La marca se conserva: medido sobre
      # `unknown`, `bytes`, `UTF-8` y `NA`.
      crudo[escapables] <- gsub(
        "\\", "\\\\", crudo[escapables], fixed = TRUE, useBytes = TRUE
      )
    }
  }
  # En ASCII no hay marca ni conversion que pueda cambiar la representacion.
  # La sonda es vectorizada y se paga una sola vez; no se entra a `validUTF8()`
  # ni a `iconv()` para el caso que domina los perfiles.
  if (!is.null(crudo) && .es_ascii(crudo)) return(crudo)
  if (is.null(crudo)) return(crudo)
  # `enc2utf8()` consulta `LC_CTYPE` cuando la cadena esta marcada `unknown`:
  # trata sus bytes como si estuvieran en la codificacion NATIVA, y bajo `C` lo
  # nativo es ASCII, asi que escapa cada byte no representable. Medido sobre
  # `categoria` con tilde, sin marca, bytes `..72.c3.ad.61`:
  #
  #   es_UY.UTF-8 -> ..72.c3.ad.61                  (intacto)
  #   C           -> ..72.3c.63.33.3e.3c.61.64.3e.61  ("<c3><ad>", texto ASCII)
  #
  # La cadena deja de contener la letra y pasa a contener la DESCRIPCION de sus
  # bytes, y el resultado queda marcado `UTF-8`, asi que parece correcto.
  #
  # `validUTF8()` en cambio NO depende del locale: contesta `TRUE` en los dos.
  # Por eso una guarda que valide con el y convierta con `enc2utf8()` pasa la
  # validacion y rompe el dato. Pero aca NO se convierte: se MARCA, y marcar no
  # toca un byte.
  #
  # Y por eso el criterio tiene que ser `validUTF8()` y no `iconv()`. Esta clave
  # decide si dos registros son el mismo, asi que tiene que dar lo mismo en toda
  # maquina; `iconv()` no es el mismo programa en todas: `win_iconv` rechaza
  # secuencias que glibc acepta. Medido en R-hub Windows sobre `a7b5c89`, con
  # `LC_CTYPE=English_United States.utf8`:
  #
  #   - la huella de configuracion cambiaba entre `C` y un locale UTF-8, y
  #     `comparar_perfiles()` publicaba una deriva de configuracion inexistente;
  #   - `acumular_historico()` rechazaba su PROPIA corrida guardada, con
  #     "un registro ya existente tiene contenido diferente" sobre una clave con
  #     tilde -`Basico`-.
  #
  # Los dos salian de lo mismo: dos funciones contestando la misma pregunta con
  # dos criterios. `.marcar_para_exhibir()` -que es la via de EXHIBIR- usa los
  # dos criterios a proposito, porque lo que publica va despues a la impresion.
  # La clave no publica: compara. `validUTF8()` es el criterio de R, es mas
  # estricto, y es igual en todas las plataformas.
  # Una cadena marcada `latin1` y la misma marcada `UTF-8` son EL MISMO TEXTO
  # con bytes distintos, y esta clave decide si dos registros son el mismo. Sin
  # este paso daban claves distintas -medido, bajo los dos locales-:
  #
  #   "B\u00e1sico" marcada UTF-8 o sin marca -> =B%C3%A1sico
  #   "B\u00e1sico" marcada latin1            -> =B%5C341sico
  #
  # porque los bytes latin1 `42.e1.73...` no son UTF-8 valido, caian en el
  # escape por bytes, y el texto quedaba descrito en vez de representado.
  #
  # Se convierte SOLO lo que declara su codificacion. Sobre `unknown` seria el
  # error que el comentario de arriba documenta: `enc2utf8()` consultaria
  # `LC_CTYPE` y bajo `C` devolveria `B<c3><a1>sico` -la descripcion de los
  # bytes- dejando la marca en `unknown`, asi que ni siquiera se nota. Sobre
  # `latin1` no consulta nada, porque la codificacion ya esta dicha: medido,
  # da los mismos bytes bajo `es_UY.UTF-8` y bajo `C`.
  #
  # `bytes` queda afuera a proposito: `enc2utf8()` aborta sobre esa marca, y
  # debe hacerlo, porque son bytes que nadie declaro como texto.
  #
  # Y se convierte con la lectura de las acciones de texto, no con `enc2utf8()`
  # a secas: los cinco bytes que Windows-1252 no define salian como el texto
  # `<81>`, y la celda `latin1` daba una clave distinta de la misma en UTF-8
  # -`n_distintos` contaba dos valores donde las acciones leen uno- e igual a la
  # del texto de cuatro letras `<81>`. Ver `.latin1_a_utf8()`.
  declarada <- !is.na(crudo) & Encoding(crudo) == "latin1"
  if (any(declarada)) crudo[declarada] <- .latin1_a_utf8(crudo[declarada])

  # `bytes` NO es una codificacion mas: es la declaracion explicita de que eso
  # no se interprete como texto. El paquete la respeta al imprimir -lo fija
  # `test-N68`, "se publica como lo publica R"- y esta clave la ignoraba:
  # marcaba UTF-8 todo lo que fuera UTF-8 valido, viniera declarado o no.
  #
  # Medido sobre `c(utf8, bytes, "z", utf8)` con el mismo `a\u00f1o`:
  #
  #   unique() de R                     -> 3
  #   n_distintos que publicaba el perfil -> 2
  #   detectar_duplicados_aproximados()   -> 3 pares, donde hay 1
  #
  # y `print()` del propio paquete los muestra distintos. Tres salidas y dos
  # respuestas sobre el mismo dato: la identidad fundia lo que la consola
  # separaba, y lo publicado contradecia a `unique()` de R.
  #
  # Se escapa byte a byte y no con `.escapar_bytes_altos()`: aquel conserva las
  # secuencias UTF-8 validas -su trabajo es rescatar texto legible- y conservar
  # es justamente interpretar.
  bytes_declarados <- !is.na(crudo) & Encoding(crudo) == "bytes"
  validos <- !is.na(crudo) & !bytes_declarados & validUTF8(crudo)
  validos[is.na(validos)] <- FALSE
  clave <- crudo
  if (any(validos)) {
    trozo <- crudo[validos]
    Encoding(trozo) <- "UTF-8"
    clave[validos] <- trozo
  }
  if (any(bytes_declarados)) {
    clave[bytes_declarados] <- .escapar_byte_a_byte(crudo[bytes_declarados])
  }
  invalidos <- !validos & !bytes_declarados & !is.na(crudo)
  if (any(invalidos)) {
    clave[invalidos] <- .escapar_bytes_altos(crudo[invalidos])
  }
  clave
}

# Lo que llega SIN MARCA y no es UTF-8 valido. Es lo que deja `read.csv()` sin
# `fileEncoding` sobre un archivo latin1, el caso mas comun del mundo real, y no
# es texto: `nchar()`, `trimws()` y las expresiones regulares abortan sobre eso.
# El paquete lo trataba como texto porque solo miraba la marca `bytes`, y el
# mismo defecto aparecio en dos sitios -la limpieza y el reporte-: por eso la
# regla vive en un solo lugar.
#
# Y no mira el locale. Decia que en una sesion que no es UTF-8
# "sin marca" es la codificacion nativa y es legible, y bajo `C` eso hacia que
# `aplicar()` transformara por bytes la celda que el perfil -que decide con
# `validUTF8()` en cualquier locale- declaro `codificacion_invalida` y no midio,
# y que el reporte escribiera el nombre `Tel<e9>fono` donde una sesion UTF-8
# escribe `Tel\xe9fono`. Lo nativo de `C` es ASCII: un byte alto tampoco es
# texto ahi.
.sin_marca_ilegible <- function(x) {
  if (!is.character(x) || !length(x)) return(logical(length(x)))
  !is.na(x) & Encoding(x) == "unknown" & !validUTF8(x)
}

# Rinde un valor para PUBLICARLO. Lo declarado `bytes` pasa a la forma que R
# muestra -`a\xc3\xb1o`-, que es ASCII; todo lo demas queda intacto.
#
# Existe porque `Encoding() == "bytes"` declara que eso no se interprete como
# texto, y el paquete honraba esa declaracion en un solo lugar -la impresion,
# que fija `test-N68`- mientras el resto la ignoraba. Tres consecuencias
# medidas, todas sobre el mismo dato:
#
#   reportar()             abortaba: "number of characters is not computable in
#                          bytes encoding" -`nchar(type = "chars")`-
#   comparar_equivalencia() comparaba bien y su resultado NO se podia imprimir:
#                          "width is not computable in bytes encoding"
#   perfil$patrones        publicaba el CARACTER, mientras `print()` del mismo
#                          dato mostraba la forma escapada
#
# Los tres son el mismo defecto: medir o mostrar como texto lo que se declaro
# que no lo es. Rendido a su forma imprimible, `nchar()` funciona, el formateo
# funciona y lo publicado coincide con la consola.
#
# `justify = "none"` no es un detalle: `format()` sobre un vector RELLENA a
# ancho comun -medido: 23 bytes donde el valor tiene 10- y eso alteraria el
# valor publicado.
#
# Lo que NO hace: tocar el camino de ANALISIS. `.texto_analizable()` sigue
# marcando UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito,
# para que `tolower()`, `trimws()` y las expresiones regulares puedan trabajar.
# La regla es por destino: interpretar para analizar, no para publicar.
.texto_publicable <- function(x) {
  # Un factor guarda su texto en los NIVELES, asi que `is.character()` da FALSE
  # y la columna salia de aca SIN RENDIR. Los mismos datos como factor y como
  # character daban tablas distintas: la de patrones perdia las filas del valor
  # declarado -cinco de veinte- y publicaba `proporcion = 1.000` para el unico
  # patron que quedaba, o sea informaba como completo lo que era parcial. El
  # paquete promete que el veredicto no depende de la forma fisica de la tabla.
  #
  # Se rinden los niveles y se devuelve un factor, que es lo que el llamador
  # espera. Si dos niveles rinden a la misma cadena, `levels<-` los funde: es
  # correcto, porque desde ese momento son indistinguibles al publicarlos.
  if (is.factor(x)) {
    if (!length(levels(x))) return(x)
    levels(x) <- .texto_publicable(levels(x))
    return(x)
  }
  if (!is.character(x) || !length(x)) return(x)
  crudos <- !is.na(x) & Encoding(x) == "bytes"
  if (any(crudos)) x[crudos] <- format(x[crudos], justify = "none")
  x
}

# Publica lo sin marca que no es UTF-8 como lo muestra la consola: `caf\xe9`.
# Es SOLO para los sumideros de publicacion -el reporte, la consola-, donde sin
# esto `nchar()` y glue abortaban sobre un perfil que el propio paquete habia
# producido. `.texto_publicable()` NO lo hace, y a proposito: corre tambien
# antes de medir, y el camino de analisis descarta esos valores y declara el
# alcance parcial -`test-N89` lo fija-, porque sin marca no se sabe que bytes
# son. Se probo ponerlo alli y cuatro pruebas lo tumbaron: los valores pasaban a
# contarse como texto en patrones, duplicados y distribuciones.
.publicar_sin_marca_ilegible <- function(x) {
  if (!is.character(x) || !length(x)) return(x)
  sin_marca <- .sin_marca_ilegible(x)
  if (any(sin_marca)) {
    declarados <- x[sin_marca]
    Encoding(declarados) <- "bytes"
    x[sin_marca] <- format(declarados, justify = "none")
  }
  x
}

# Convierte una celda a texto para componer una frase. Queda de la epoca del
# volcado por bytes, que se DESCARTO: aquel comentario decia que la salida
# tabular "no necesita alineacion para ser util", y hoy el paquete sostiene lo
# contrario -la tabla se publica formateada, con `print.data.frame()`, y el
# escape solo entra donde R no puede-. Una nota vieja es una afirmacion falsa,
# asi que se corrige en vez de dejarse.
#
# Sobrevive porque `R/tablero-calidad.R` la usa para armar una frase, que es un
# uso legitimo y distinto del que la motivo.
.texto_celda_publicada <- function(x) {
  if (is.null(x) || !length(x)) return("")
  if (is.factor(x)) return(.texto_celda_publicada(as.character(x)))
  if (is.data.frame(x)) return("<data.frame>")
  if (is.function(x)) return("<funci\u00f3n>")
  if (is.list(x)) {
    resultado <- paste(
      vapply(x, .texto_celda_publicada, character(1L)), collapse = "; "
    )
    Encoding(resultado) <- "unknown"
    return(resultado)
  }
  valores <- tryCatch(as.character(x), error = function(e) "<no representable>")
  if (!length(valores)) return("")
  valores[is.na(valores)] <- "NA"
  resultado <- paste(valores, collapse = ", ")
  Encoding(resultado) <- "unknown"
  resultado
}

# Los cuatro metodos `print.*` que publican prosa por `stdout` pasan por aca.
# Forzaba `Encoding <- "unknown"` -la politica del volcado por bytes, que se
# descarto- y el resultado era prosa a medias bajo un locale que no puede
# representar el texto: la etiqueta del paquete legible y el valor del usuario
# en bytes crudos. Ahora rige la misma politica que el canal de `cli`: se
# declara lo que se puede declarar y se escapa lo que no.
#
# La politica CONTRARIA -no marcar- sigue valiendo para los datos que el
# paquete devuelve, porque ahi los bytes son del usuario. La diferencia es
# quien habla: esto es el paquete, no el dato.
.cat_publicado <- function(...) {
  partes <- lapply(list(...), function(parte) {
    if (is.null(parte)) return(character())
    .marcar_para_exhibir(as.character(parte))
  })
  do.call(cat, c(partes, list(sep = "")))
}

# print.data.frame() delega las columnas de listas en toString(), que usa
# strtrim() y aborta cuando una cadena UTF-8 sin marca llega bajo
# LC_CTYPE = "C". La causa no es la alineacion sino la marca: si la copia que
# se exhibe declara UTF-8, R sabe que hacer con ella en cualquier locale y la
# tabla formateada se conserva.
#
# NO hay volcado por bytes: existio como camino alternativo y se quito entero,
# porque un camino que se elige "cuando fallo" termina tragandose fallos
# ajenos. Lo que hay es un REINTENTO con los bytes rotos escapados, y si ese
# tampoco puede, el error original se relanza.
.marcar_para_exhibir <- function(x, escapar = TRUE) {
  if (is.character(x)) {
    # Tres casos, y el orden importa.
    #
    # 1. Lo que viene con codificacion DECLARADA -`latin1` o `UTF-8`- se
    #    convierte con `enc2utf8()`, que es lo que hace R. "Sin perdida" seria
    #    falso y no se afirma: medido sobre los 255 bytes, un `latin1`
    #    declarado mapea 0x80 a U+20AC -tabla de CP1252, no de ISO-8859-1- y
    #    convierte 0x81, 0x8D, 0x8F, 0x90 y 0x9D en el texto `<81>` y compania.
    #    Base hace exactamente lo mismo: no es divergencia del paquete, es como
    #    se comporta R, y el paquete lo sigue en vez de inventar otra cosa. Escaparlo destruia el
    #    caracter: con `latin1` declarado, base imprimia `año` bajo un
    #    locale UTF-8 y el paquete publicaba `a<f1>o`, que ademas desmentia la
    #    promesa de que bajo UTF-8 la salida es exactamente la de R.
    # 2. Lo que no declara nada pero SON bytes UTF-8 validos se declara UTF-8, y
    #    R lo escapa como <U+00F1> donde el locale no lo represente.
    # 3. Lo que queda no se puede declarar: se escapa con el octal que usa el
    #    propio R, no con una forma inventada que el usuario pueda escribir.
    #
    # El resultado es que la tabla SIEMPRE se puede formatear, sin un camino
    # alternativo que, por existir, terminaba tragandose errores ajenos.
    marca <- Encoding(x)
    # `Encoding() == "bytes"` NO es una codificacion mas: es la declaracion
    # explicita de que eso no se interprete como texto. Marcarlo o convertirlo
    # es desobedecerla. Base publica `a\xc3\xb1o` -los bytes escapados- y el
    # paquete publicaba `a\u00f1o`, interpretando justo lo que se pidio no
    # interpretar. Se devuelve intacto y R hace lo suyo.
    intocable <- marca == "bytes" & !is.na(x)
    declarado <- marca %in% c("latin1", "UTF-8") & !is.na(x)
    if (any(declarado)) x[declarado] <- enc2utf8(x[declarado])
    resto <- !declarado & !intocable & !is.na(x)
    if (any(resto)) {
      trozo <- x[resto]
      # Dos criterios y hacen falta los dos. `iconv()` acepta secuencias fuera
      # del rango de Unicode -`F4 90 80 80`, mas alla de U+10FFFF- que
      # `validUTF8()` rechaza, que es el criterio de R. Declarando UTF-8 con el
      # de `iconv()` a secas, el paquete publicaba `<U+00110000>`: un punto de
      # codigo que no existe.
      validas <- validUTF8(trozo) &
        !is.na(iconv(trozo, from = "UTF-8", to = "UTF-8", sub = NA))
      if (any(validas)) Encoding(trozo[validas]) <- "UTF-8"
      # El escape es el ULTIMO recurso y por eso es optativo: donde
      # `print.data.frame()` puede con los bytes tal cual, hay que dejarselos,
      # porque escapar de mas tambien es una diferencia. Medido: base publica
      # `A\xffB` en una columna simple y nosotros publicabamos `A\\377B`, con
      # la barra escapada dos veces.
      if (escapar) {
        # La barra se duplica en TODO el vector, no solo en lo que se escapa.
        # Si no, el byte 0xff y el texto `\377` que un usuario escribio
        # publican lo mismo y el escape deja de ser reversible: el mismo
        # defecto que tenia la forma `<ff>`, mudado al camino del reintento.
        # Esto transforma el marco ENTERO, tambien las columnas que no tenian
        # nada malo: una columna con el texto literal `\377` se publica con la
        # barra duplicada aunque el byte invalido este en otra. Es el precio de
        # que la representacion sea reversible, y solo se paga en un marco que
        # R no podia imprimir de ninguna manera.
        # `useBytes` no es optativo: `gsub()` valida la codificacion antes de
        # operar y aborta justo sobre las cadenas invalidas, que son las que
        # este camino existe para atender.
        trozo <- gsub("\\", "\\\\", trozo, fixed = TRUE, useBytes = TRUE)
        if (any(!validas)) {
          escapado <- .escapar_bytes_altos(trozo[!validas])
          # Escapado solo lo roto, el resto ya es UTF-8 valido: se declara para
          # que R publique los acentos que sobrevivieron y no sus bytes.
          recuperado <- validUTF8(escapado)
          if (any(recuperado)) Encoding(escapado[recuperado]) <- "UTF-8"
          trozo[!validas] <- escapado
        }
      }
      x[resto] <- trozo
    }
    return(x)
  }
  if (is.factor(x)) {
    levels(x) <- .marcar_para_exhibir(levels(x), escapar)
    return(x)
  }
  # Un environment tiene semantica de REFERENCIA: escribirle los atributos
  # marcados le cambia el objeto al usuario, no a una copia. Medido contra su
  # control: `print()` de base deja la marca en `unknown` y este recorrido la
  # pasaba a `UTF-8`. Lo mismo vale para lo que no se puede copiar por valor.
  if (is.environment(x) || is.symbol(x) ||
      typeof(x) %in% c("externalptr", "weakref")) {
    return(x)
  }
  if (is.list(x)) {
    atributos <- attributes(x)
    cuerpo <- lapply(unclass(x), .marcar_para_exhibir, escapar = escapar)
    # En el reintento se escapan tambien los nombres de lo que va adentro: una
    # columna-lista puede llevar un `data.frame` -las claves de la traza- cuyo
    # nombre es el de una columna con bytes que no son UTF-8, y `deparse()`
    # aborta sobre ese nombre. Solo en el reintento, como el resto del escape:
    # donde base puede, su salida no cambia.
    if (escapar && !is.null(atributos$names)) {
      atributos$names <- .marcar_para_exhibir(atributos$names, escapar)
    }
    if (escapar && is.character(atributos$row.names)) {
      atributos$row.names <- .marcar_para_exhibir(atributos$row.names, escapar)
    }
    if (!is.null(atributos)) attributes(cuerpo) <- atributos
    return(cuerpo)
  }
  x
}

# La salida de `cli` no siempre recibe una columna: varios metodos interpolan
# campos de un objeto, incluidos atributos que no forman parte del cuerpo de un
# data.frame. Esta copia recorre cuerpo y atributos para que esos campos lleguen
# declarados como UTF-8, sin cambiar el objeto que conserva el usuario.
# La decision de que bytes son marcables vive en `.marcar_para_exhibir()`.
.marcar_objeto_para_exhibir <- function(x) {
  if (is.character(x) || is.factor(x)) {
    return(.marcar_para_exhibir(x))
  }
  # Igual que en `.marcar_para_exhibir()`: una referencia no se copia, asi que
  # escribirle los atributos marcados le cambia el objeto al usuario.
  if (is.environment(x) || is.symbol(x) ||
      typeof(x) %in% c("externalptr", "weakref")) {
    return(x)
  }
  atributos <- attributes(x)
  if (is.list(x)) {
    cuerpo <- lapply(unclass(x), .marcar_objeto_para_exhibir)
    if (!is.null(atributos)) {
      atributos <- lapply(atributos, .marcar_objeto_para_exhibir)
      attributes(cuerpo) <- atributos
    }
    return(cuerpo)
  }
  if (!is.null(atributos)) {
    atributos <- lapply(atributos, .marcar_objeto_para_exhibir)
    attributes(x) <- atributos
  }
  x
}

.data_frame_para_exhibir <- function(x, escapar = TRUE) {
  atributos <- attributes(x)
  cuerpo <- lapply(unclass(x), .marcar_para_exhibir, escapar = escapar)
  atributos$names <- .marcar_para_exhibir(atributos$names, escapar)
  if (is.character(atributos$row.names)) {
    atributos$row.names <- .marcar_para_exhibir(atributos$row.names, escapar)
  }
  attributes(cuerpo) <- atributos
  cuerpo
}


.print_data_frame_bytes <- function(x, row.names = TRUE, ...) {
  if (!inherits(x, "data.frame")) {
    .detener("x debe ser un data.frame.", call. = FALSE)
  }
  imprimir <- function(marco) utils::capture.output(
    print.data.frame(marco, row.names = row.names, ...)
  )
  # Se intenta primero SIN escapar, para que donde `print.data.frame()` puede la
  # salida sea exactamente la suya; escapar de mas tambien es una diferencia.
  # Si no puede, se reintenta con los bytes invalidos escapados, que es el unico
  # caso que no puede en ningun locale. Y si tampoco puede asi, el fallo no era
  # de codificacion: es del usuario -su `format()`, su columna- y se relanza el
  # error ORIGINAL, no el del segundo intento, que hablaria de otra cosa.
  salida <- tryCatch(
    imprimir(.data_frame_para_exhibir(x, escapar = FALSE)),
    error = function(condicion) {
      segunda <- tryCatch(
        imprimir(.data_frame_para_exhibir(x, escapar = TRUE)),
        error = function(otra) NULL
      )
      if (is.null(segunda)) .detener(condicion)
      segunda
    }
  )
  if (length(salida)) {
    Encoding(salida) <- "unknown"
    cat(paste0(salida, "\n"), sep = "")
  }
  invisible(x)
}

# Los nombres son datos del usuario, y no se pueden comparar con `==`, `match()`
# o `setdiff()` directamente: esas operaciones consultan `LC_CTYPE` cuando una
# cadena UTF-8 llega con marca `unknown`. La marca no forma parte del nombre que
# el usuario escribio; sus bytes, si. Esta funcion devuelve una representacion
# de trabajo en la que todo UTF-8 valido tiene la misma marca y lo que no es
# UTF-8 se representa por sus bytes, sin adivinar una codificacion. Por eso la
# clave es estable entre locales y sigue siendo inyectiva: dos secuencias de
# bytes distintas no se convierten en el mismo nombre.
.nombres_para_operar <- function(nombres) {
  nombres <- as.character(nombres)
  if (!length(nombres)) return(nombres)
  # En el caso habitual (nombres ASCII) R ya considera iguales las marcas
  # `unknown` y `UTF-8`; evitar el resto de la preparacion mantiene barata la
  # ruta que se ejecuta en cada columna de una tabla grande.
  # La marca reservada se busca con TRE: sus entradas son solo ASCII, y en
  # Windows cada `grepl(perl = TRUE)` cuesta unos 47 us fijos. La deteccion de lo
  # no ASCII sigue en PCRE: con TRE y el patron por bytes, R intenta traducirlo
  # bajo un locale que no es UTF-8 y avisa (medido en test-escapes-en-mensajes).
  no_ascii <- grepl("[^\\001-\\177]", nombres, useBytes = TRUE, perl = TRUE)
  reservado <- rep(FALSE, length(nombres))
  indices_ascii <- which(!is.na(nombres) & !no_ascii)
  if (length(indices_ascii)) {
    reservado[indices_ascii] <- grepl(
      "^<lupa-byte:[0-9A-F]+>$", nombres[indices_ascii]
    )
  }
  if (!any(no_ascii) && !any(reservado)) return(nombres)
  claves <- .clave_bytes(nombres)
  salida <- nombres
  # Igual que en `.clave_bytes()`, lo que no se puede usar aca es la conversion
  # que consulta al locale. La marca de la clave canonica dice si los bytes eran
  # UTF-8 sin volver a preguntarle a `LC_CTYPE`, que es lo que hacia que la
  # misma secuencia se tratara distinto en un perfil guardado y en uno nuevo.
  # Los nombres ASCII siguen siendo válidos aunque su marca sea
  # "unknown"; sólo las cadenas no ASCII necesitan la marca UTF-8
  # normalizada para cruzar locales.
  validos <- !is.na(nombres) & (!no_ascii | Encoding(claves) == "UTF-8")
  if (any(validos)) {
    # Lo ASCII queda como en la via rapida de arriba. `.clave_bytes()` duplica
    # la barra invertida, y tomar su clave tambien para lo ASCII hacia que el
    # mismo `a\b` saliera intacto en un vector todo ASCII y con la barra
    # duplicada en uno con algun acento: una clave decidida por la tanda.
    # Medido: `detectar_relaciones()` no encontraba `a\b` entre
    # dos tablas porque solo una tenia una enie. Lo ASCII no puede chocar
    # con lo que no lo es, y lo reservado se escapa abajo igual.
    salida[validos & no_ascii] <- claves[validos & no_ascii]
    # La representacion de bytes invalidos usa un prefijo reservado. Si un
    # nombre UTF-8 real lo contiene literalmente, se escapa con otra marca
    # para que la clave siga siendo inyectiva incluso en ese caso extremo.
    indices_validos <- which(validos)
    reservados <- indices_validos[reservado[indices_validos]]
    if (length(reservados)) {
      salida[reservados] <- vapply(salida[reservados], function(nombre) {
        bytes <- as.integer(charToRaw(nombre))
        paste0("<lupa-text:", paste(sprintf("%02X", bytes), collapse = ""), ">")
      }, character(1L))
    }
  }
  invalidos <- !is.na(nombres) & !validos
  if (any(invalidos)) {
    salida[invalidos] <- vapply(nombres[invalidos], function(nombre) {
      bytes <- as.integer(charToRaw(nombre))
      paste0("<lupa-byte:", paste(sprintf("%02X", bytes), collapse = ""), ">")
    }, character(1L))
  }
  salida
}

# Los nombres compuestos se muestran con comas por compatibilidad, pero una
# coma tambien puede ser parte de un nombre real. Las salidas nuevas guardan la
# lista; este parser queda para entradas que aun no la traen.
.separar_nombres_columnas_historico <- function(texto, sep = ",") {
  texto <- as.character(texto)
  if (length(texto) != 1L || is.na(texto) || !nzchar(texto)) {
    return(character())
  }
  partes <- strsplit(texto, sep, fixed = TRUE, useBytes = TRUE)[[1L]]
  ilegibles <- vapply(partes, .bytes_sin_declarar, logical(1L))
  if (any(ilegibles) || any(Encoding(partes) == "bytes")) {
    partes <- gsub("^[ \\t]+", "", partes, perl = TRUE, useBytes = TRUE)
    partes <- gsub("[ \\t]+$", "", partes, perl = TRUE, useBytes = TRUE)
    return(partes)
  }
  trimws(partes)
}

.separar_nombres_columnas <- function(texto, nombres, sep = ",") {
  texto <- as.character(texto)
  nombres <- as.character(nombres)
  if (length(texto) != 1L || is.na(texto) || !nzchar(texto)) {
    return(character())
  }
  nombres <- nombres[!is.na(nombres) & nzchar(nombres)]
  if (!length(nombres)) {
    return(.separar_nombres_columnas_historico(texto, sep = sep))
  }

  nombres_originales <- nombres
  nombres_operativos <- .nombres_para_operar(nombres_originales)
  texto_operativo <- .nombres_para_operar(texto)
  longitud_texto <- .nchar_texto_seguro(texto_operativo)[[1L]]
  if (is.na(longitud_texto)) {
    return(.separar_nombres_columnas_historico(texto, sep = sep))
  }

  buscar <- function(separador) {
    n <- longitud_texto
    resultados <- list()
    recorrer <- function(pos, partes) {
      if (length(resultados) > 20L) return(invisible(NULL))
      if (pos > n) {
        resultados[[length(resultados) + 1L]] <<- partes
        return(invisible(NULL))
      }
      restante <- substr(texto_operativo, pos, n)
      candidatas <- which(startsWith(restante, nombres_operativos))
      if (!length(candidatas)) return(invisible(NULL))
      for (indice in candidatas) {
        siguiente <- pos + .nchar_texto_seguro(
          nombres_operativos[[indice]]
        )[[1L]]
        al_final <- siguiente > n
        tiene_sep <- !al_final && startsWith(
          substr(texto_operativo, siguiente, n), separador
        )
        if (!al_final && !tiene_sep) next
        recorrer(
          if (al_final) n + 1L else siguiente + nchar(separador),
          c(partes, nombres_originales[[indice]])
        )
      }
      invisible(NULL)
    }
    recorrer(1L, character())
    resultados
  }
  candidatas <- unlist(lapply(unique(c(sep, ",", ", ")), buscar),
                       recursive = FALSE)
  if (length(candidatas)) {
    claves <- vapply(candidatas, paste, character(1L), collapse = "\001")
    candidatas <- candidatas[!duplicated(claves)]
  }
  if (length(candidatas) == 1L) return(candidatas[[1L]])
  if (length(candidatas) > 1L) {
    return(structure(character(), ambigua = TRUE))
  }
  partes <- .separar_nombres_columnas_historico(texto, sep = sep)
  partes[nzchar(partes)]
}

.anotar_columnas_cobertura <- function(cobertura, nombres) {
  if (!is.data.frame(cobertura) || !"columna" %in% names(cobertura)) {
    return(cobertura)
  }
  existentes <- if ("columnas" %in% names(cobertura)) {
    cobertura$columnas
  } else {
    vector("list", nrow(cobertura))
  }
  internas <- lapply(seq_len(nrow(cobertura)), function(i) {
    partes <- if (length(existentes) >= i) existentes[[i]] else NULL
    if (length(partes)) as.character(partes) else {
      .separar_nombres_columnas(
        as.character(cobertura$columna[[i]]), nombres, sep = ","
      )
    }
  })
  cobertura$columnas <- I(internas)
  attr(cobertura, "columnas_internas") <- NULL
  cobertura
}

.columnas_cobertura <- function(cobertura, indice, nombres) {
  if ("columnas" %in% names(cobertura)) {
    internas <- cobertura$columnas
    if (is.list(internas) && length(internas) >= indice) {
      return(as.character(internas[[indice]]))
    }
  }
  historico <- .separar_nombres_columnas_historico(
    as.character(cobertura$columna[[indice]]), sep = ","
  )
  if (length(historico)) return(historico)
  .separar_nombres_columnas(
    as.character(cobertura$columna[[indice]]), nombres, sep = ","
  )
}

# Estos ayudantes extienden la misma política a identificadores que no son
# nombres de columna: dimensiones, métricas, reglas y fronteras declaradas.
# Devuelven los originales para que el texto que el usuario escribió siga
# siendo el que se publica; sólo la decisión de pertenencia usa la clave estable.
.identificadores_en <- function(x, y) {
  .nombres_para_operar(x) %in% .nombres_para_operar(y)
}

.identificadores_unicos <- function(x) {
  originales <- as.character(x)
  originales[!duplicated(.nombres_para_operar(originales))]
}

.identificadores_setdiff <- function(x, y) {
  originales <- as.character(x)
  originales[!.identificadores_en(originales, y)]
}

.identificadores_intersect <- function(x, y) {
  originales <- as.character(x)
  originales[.identificadores_en(originales, y)]
}

.identificadores_ordenados <- function(x) {
  originales <- as.character(x)
  originales[.orden_por_bytes(.clave_bytes(originales), method = "radix")]
}

.indice_identificador <- function(pedidos, nombres) {
  match(.nombres_para_operar(pedidos), .nombres_para_operar(nombres))
}

.clave_par_identificador <- function(a, b, sep = "|") {
  # Un par de identificadores suele terminar como nombre de lista o clave de
  # agrupacion. Normalizar cada componente y el resultado evita que `paste()`
  # vuelva a consultar el locale al armar la cadena compuesta.
  .clave_bytes(paste(
    .clave_bytes(a), .clave_bytes(b), sep = sep
  ))
}

# `make.unique()` se usa para claves internas y para nombres de listas. No se le
# entrega el nombre del usuario: bajo `C` puede publicar `<U+....>` y, peor,
# producir una cadena que no existe en la tabla. La desambiguacion se hace sobre
# la clave estable y conserva el nombre original mientras no haya colision.
.nombres_unicos <- function(nombres, sep = ".") {
  originales <- as.character(nombres)
  if (!length(originales)) return(originales)
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    .detener("`sep` debe ser una cadena de longitud uno.", call. = FALSE)
  }
  unicos <- originales
  usados <- new.env(hash = TRUE, parent = emptyenv())
  siguientes <- new.env(hash = TRUE, parent = emptyenv())
  clave_hash <- function(clave) {
    if (is.na(clave)) return(NA_character_)
    paste0("k", paste(as.integer(charToRaw(clave)), collapse = "_"))
  }
  # Los dos entornos son conjuntos -y un mapa- sobre la clave por bytes, que
  # ocupa hasta cuatro caracteres por byte del nombre. La clave se usaba ENTERA
  # como nombre de variable del entorno, y R no admite uno de mas de 10.000
  # bytes: un nombre de columna de 2.501 bytes abortaba `perfilar()` y otras
  # cuatro con "nombres de variables se limitan a 10000 bytes", un limite del
  # instrumento que no es de los datos ni esta documentado. El caso real
  # medido: un CSV de 300 columnas separado por `;` leido con `sep = ","`
  # deja una sola columna cuyo nombre es la cabecera entera, justo la tabla mal
  # leida que hay que poder diagnosticar.
  #
  # La variable del entorno es ahora un PREFIJO acotado de la clave, y guarda
  # las claves enteras que lo comparten -con su sufijo, en `siguientes`-. La
  # pertenencia se sigue decidiendo por la clave entera, byte por byte, asi que
  # el conjunto y la salida son los mismos para cualquier largo: los nombres
  # cortos caen solos en su cubeta.
  cubeta <- function(llave) substr(llave, 1L, 4000L)
  leer <- function(entorno, llave) {
    get0(cubeta(llave), envir = entorno, inherits = FALSE)[[llave]]
  }
  escribir <- function(entorno, llave, valor) {
    nombre <- cubeta(llave)
    valores <- get0(nombre, envir = entorno, inherits = FALSE)
    if (is.null(valores)) valores <- list()
    valores[[llave]] <- valor
    assign(nombre, valores, envir = entorno)
  }
  ya_usada <- function(clave) {
    llave <- clave_hash(clave)
    !is.na(llave) && !is.null(leer(usados, llave))
  }
  registrar <- function(clave) {
    llave <- clave_hash(clave)
    if (!is.na(llave)) escribir(usados, llave, TRUE)
  }
  for (i in seq_along(originales)) {
    candidato <- originales[[i]]
    clave <- .nombres_para_operar(candidato)
    repetido <- ya_usada(clave)
    if (repetido) {
      base <- candidato
      llave_base <- clave_hash(clave)
      sufijo <- leer(siguientes, llave_base)
      if (is.null(sufijo)) sufijo <- 1L
      repeat {
        candidato <- paste0(base, sep, sufijo)
        clave <- .nombres_para_operar(candidato)
        if (!ya_usada(clave)) break
        sufijo <- sufijo + 1L
      }
      escribir(siguientes, llave_base, sufijo + 1L)
    }
    unicos[[i]] <- candidato
    registrar(clave)
  }
  unicos
}

.nombres_make_names <- function(nombres) {
  originales <- .nombres_para_operar(nombres)
  if (!length(originales)) return(originales)
  sintacticos <- vapply(originales, function(nombre) {
    if (is.na(nombre) || !nzchar(nombre)) return("X")
    codigos <- utf8ToInt(nombre)
    texto <- vapply(codigos, intToUtf8, character(1L))
    es_letra <- vapply(texto, function(caracter) {
      grepl("(*UTF)^[\\p{L}\\p{Nl}]$", caracter, perl = TRUE)
    }, logical(1L))
    es_numero <- vapply(texto, function(caracter) {
      grepl("(*UTF)^[\\p{N}]$", caracter, perl = TRUE)
    }, logical(1L))
    permitidos <- es_letra | es_numero | texto %in% c(".", "_")
    texto[!permitidos] <- "."
    primero_valido <- es_letra[[1L]] ||
      (codigos[[1L]] == utf8ToInt(".") && length(texto) > 1L &&
         !es_numero[[2L]])
    salida <- paste0(texto, collapse = "")
    if (!primero_valido) salida <- paste0("X", salida)
    # Igual que `make.names()`, las palabras reservadas no quedan como nombres
    # desnudos. La lista es parte del contrato de base R y es ASCII.
    if (salida %in% c(
      "if", "else", "repeat", "while", "function", "for", "in", "next",
      "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
      "NA_real_", "NA_complex_", "NA_character_"
    )) salida <- paste0(salida, ".")
    salida
  }, character(1L))
  .nombres_unicos(sintacticos)
}

.indice_nombre <- function(pedidos, nombres) {
  .indice_identificador(pedidos, nombres)
}

.nombres_resueltos <- function(pedidos, nombres) {
  nombres[.indice_nombre(pedidos, nombres)]
}

.nombres_presentes <- function(pedidos, nombres) {
  !is.na(.indice_nombre(pedidos, nombres))
}

.matriz_ausentes <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(is.na(tabla))
  if (!ncol(tabla)) return(matrix(logical(), nrow = nrow(tabla), ncol = 0L))
  do.call(cbind, unname(lapply(tabla, is.na)))
}

# El orden de un vector cualquiera, sin depender de lo que el usuario tenga
# adjunto ni de la configuracion regional.
#
# `bit64` ENMASCARA `order()` cuando esta adjunto, y dentro del espacio de
# nombres de un paquete no lo esta: ahi `order()` es el de base, que sobre un
# `integer64` ordena por los bits del `double` subyacente y manda los negativos
# al final. Medido: `c(1, 2, -999, 4, 5)` daba `1,2,4,5,3`. Con eso, ante un
# empate la moda de una columna `integer64` salia `1` donde la misma columna en
# `double` daba `-999`: el mismo dato, dos modas, decididas por el
# almacenamiento.
#
# `rank.integer64()` si esta exportada por `bit64` -`order.integer64` no-, y
# ordenar por el rango da el orden correcto incluso por encima de 2^53, sin
# convertir a `double` y sin perder precision.
#
# Es la misma familia que el desempate por locale y que el import de
# `data.table`: una dependencia que cambia el significado de una funcion basica
# segun donde se la llame.
.orden_seguro <- function(x) {
  if (inherits(x, "integer64")) {
    # Si la columna es `integer64`, `bit64` esta necesariamente cargado: el
    # objeto no podria existir si no. Aun asi se cae con elegancia.
    if (requireNamespace("bit64", quietly = TRUE)) {
      return(tryCatch(order(bit64::rank.integer64(x)),
                      error = function(e) seq_along(x)))
    }
    return(seq_along(x))
  }
  if (is.character(x)) {
    return(order(.clave_bytes(x), method = "radix"))
  }
  clave <- if (is.raw(x)) as.integer(x) else x
  tryCatch(order(clave), error = function(e) seq_along(x))
}

# `match()` y `%in%` del paquete: los de base, salvo que un `integer64` se
# empareja por su texto.
#
# Eso es lo que hace base con bit64 4.6.0 o posterior: su `mtfrm.integer64()`
# es `as.character(x)`. Con una version anterior no hay ese metodo, y
# `mtfrm.default()` aplica `as.vector()`, que quita la clase y deja los bits del
# `double`, en el que todos los negativos de -2^52 a -1 son `NaN`: base los
# iguala entre si. Medido en R 4.2.0 con bit64 4.0.5: `match(x, unique(x))`
# sobre -1, -1, -2, -3, 5 daba 1 1 1 1 4; por bloques `n_distintos` salia 46
# donde habia 50, y las dependencias funcionales y los hallazgos cambiaban.
#
# Llevan el nombre de base dentro del espacio de nombres para que valgan en las
# mas de mil llamadas del paquete sin reescribir cada una: un recorrido de todas
# las exportadas encontro 8 sitios que reciben una columna `integer64`, y nada
# asegura que sean todos. No se exportan ni se registran como metodo, asi que la
# sesion del usuario no cambia. Se empareja por el texto y no con
# `bit64::match.integer64()` porque el texto da lo MISMO que base con bit64
# nuevo tambien cuando el otro argumento no es `integer64` -un `double`, que
# `match.integer64()` truncaria a entero-, y no pierde cifras por encima de 2^53.
#
# No cubren las funciones de base que llaman a `match()` por dentro con el objeto
# intacto: `merge()` por una clave `integer64` y `factor()` con niveles
# `integer64` dan distinto segun la version de bit64. El paquete no las llama
# asi: su unico `merge()` es por una clave de texto y sus `factor()` con niveles
# los tienen de texto.
#
# Llaman a `base::match()` por su nombre: copiar la funcion de base dentro del
# paquete (`base_match <- base::match`) lleva su `.Internal()` al espacio de
# nombres, y R CMD check lo marca con un WARNING.
match <- function(x, table, nomatch = NA_integer_, incomparables = NULL) {
  if (is.object(x) && inherits(x, "integer64")) x <- as.character(x)
  if (is.object(table) && inherits(table, "integer64")) {
    table <- as.character(table)
  }
  base::match(x, table, nomatch, incomparables)
}

`%in%` <- function(x, table) {
  if (is.object(x) && inherits(x, "integer64")) x <- as.character(x)
  if (is.object(table) && inherits(table, "integer64")) {
    table <- as.character(table)
  }
  base::match(x, table, 0L) > 0L
}

# Con el `match()` de arriba, lo unico que una columna `integer64` necesita es
# `bit64`: sin el no hay `as.character()` que lea sus valores -saldria el texto
# de los bits-, ni orden, ni nada que los distinga. La version no importa.
#
# Un `integer64` puede llegar sin `bit64` cargado -lo trae `readRDS()`, por
# ejemplo-, y sus metodos se registran al CARGARLO: se carga antes de mirar,
# como hace el perfil con sus otros metodos (`.bit64_disponible()`). Sin la
# carga, la guarda se negaba con bit64 4.8 instalado.
.integer64_con_match_confiable <- function() {
  .bit64_disponible()
}

# La guarda de entrada: `x` es una tabla o una columna. Con `via_dbi`, la
# condicion hereda ademas de `lupa_error_dbi`, como todas las de esa via.
.exigir_integer64_confiable <- function(x, via_dbi = FALSE) {
  if (inherits(x, "integer64")) {
    columnas <- character()
  } else {
    if (!is.list(x) || !length(x)) return(invisible(x))
    es_integer64 <- vapply(x, inherits, logical(1L), "integer64")
    if (!any(es_integer64)) return(invisible(x))
    columnas <- names(x)[es_integer64]
  }
  if (.integer64_con_match_confiable()) return(invisible(x))
  nombradas <- if (length(columnas)) {
    paste0(" (", paste0("`", columnas, "`", collapse = ", "), ")")
  } else ""
  mensaje <- paste0(
    "Las columnas `integer64`", nombradas, " necesitan el paquete bit64. Sin",
    " \u00e9l, R lee sus valores como los bits de un `double` e iguala entre s\u00ed",
    " todos los negativos, y los distintos, la moda y la mediana saldr\u00edan mal",
    " sin aviso. Instalar bit64 desde CRAN; o convertir la columna con",
    " `as.numeric()` si sus valores no pasan de 2^53, o desde una base conecte",
    " con `bigint = \"numeric\"`."
  )
  condicion <- list(message = mensaje, call = NULL)
  class(condicion) <- c("lupa_error_integer64",
                        if (isTRUE(via_dbi)) "lupa_error_dbi",
                        "error", "condition")
  .detener(condicion)
}

.ordenar_por_bytes <- function(x) {
  if (!length(x)) return(x)
  x[order(.clave_bytes(x), method = "radix")]
}

# `order()` y `sort()` del paquete, sin la intercalacion del locale. Sobre
# texto, `order()` de base usa la de `LC_COLLATE`: el desempate de
# `formatos_fecha` ponia `%Y-%m-%d` primero bajo `C` y `%d/%m/%Y` bajo
# `es_UY.UTF-8`, y la propuesta de modelo se ordenaba por el nombre de la
# columna ANTES de numerar `sugerencia-NNNN`, asi que el mismo id media
# `Fecha de Alta` en un `Rscript` de cron y `depto` en RStudio, y una regla que
# nombraba la metrica por su id daba 1 en uno y 0 en el otro.
#
# El texto se ordena por sus bytes -la clave de `.clave_bytes()`, que marca el
# UTF-8 valido y describe lo que no lo es: `method = "radix"` aborta sobre una
# cadena sin marca con bytes altos-; un `integer64` por su rango (ver
# `.orden_seguro()`), un factor por sus codigos y lo demas como esta, todo con
# `method = "radix"`. Todas las llamadas del paquete pasan por aca: una prueba
# recorre el espacio de nombres y no encuentra otro `order()` ni `sort()`.
.orden_por_bytes <- function(..., na.last = TRUE, decreasing = FALSE,
                             method = "radix") {
  if (!identical(method, "radix")) {
    .detener("`.orden_por_bytes()` s\u00f3lo ordena con `method = \"radix\"`.", call. = FALSE)
  }
  claves <- lapply(list(...), .clave_de_orden)
  do.call(order, c(unname(claves), list(
    na.last = na.last, decreasing = decreasing, method = "radix"
  )))
}

.clave_de_orden <- function(x) {
  if (inherits(x, "integer64")) {
    if (requireNamespace("bit64", quietly = TRUE)) {
      rango <- bit64::rank.integer64(x)
      rango[is.na(x)] <- NA
      return(rango)
    }
    return(unclass(x))
  }
  if (is.factor(x)) return(as.integer(x))
  if (is.character(x)) return(.clave_bytes(x))
  if (is.raw(x)) return(as.integer(x))
  x
}

.sort_por_bytes <- function(x, decreasing = FALSE, na.last = NA, ...) {
  if (!length(x)) return(x)
  x[.orden_por_bytes(x, decreasing = decreasing, na.last = na.last)]
}

# `factor()`, `split()`, `table()` y `tapply()` ordenan los niveles de un texto
# con la intercalacion del locale, igual que `sort()`: el resumen de
# hallazgos de `comparar_perfiles()` salia en otro orden bajo `C` que en una
# sesion UTF-8. Estos ayudantes arman el factor con los niveles por bytes; lo
# que no es texto queda como lo deja base, que ahi no consulta el locale.
.factor_por_bytes <- function(x, exclude = NA) {
  if (is.factor(x)) return(x)
  if (!is.character(x)) return(factor(x, exclude = exclude))
  niveles <- .sort_por_bytes(unique(x[!is.na(x)]))
  if (is.null(exclude) && anyNA(x)) niveles <- c(niveles, NA_character_)
  factor(x, levels = niveles, exclude = exclude)
}

.dividir_por_bytes <- function(x, f, drop = FALSE) {
  f <- if (is.list(f)) lapply(f, .factor_por_bytes) else .factor_por_bytes(f)
  split(x, f, drop = drop)
}

# `.columnas_duplicadas()` y `.pares_redundantes()` eran el mismo bloque escrito
# dos veces en archivos distintos, con una sola diferencia: que columnas se
# comparan -todas, o un subconjunto de claves-. Corregir un detalle obligaba a
# acordarse de hacerlo en los dos lados. Los dos nombres se conservan porque
# dicen cosas distintas en su contexto, pero ahora son envoltorios de esto.
#
# Comparar todos los pares era cuadratico en llamadas: con 1.000 columnas,
# 499.500 llamadas a `.columnas_identicas()`, cada una normalizando las dos
# columnas otra vez -unos 10 s medidos, iguales o distintas-. Ahora cada columna se
# normaliza una vez, se agrupa por una huella barata y solo se compara dentro de
# su grupo, contra un representante de cada clase ya vista. Es exacto por dos
# propiedades: la huella es la misma para dos columnas que la comparacion da
# por iguales (nunca separa un par verdadero; si junta dos distintas, la
# comparacion las separa), y la igualdad es una relacion de equivalencia
# -`identical()`, o los bits para `integer64`-, asi que igual al representante
# es igual a toda la clase.
#
# Devuelve las clases con dos o mas columnas, como posiciones dentro de
# `indices`: cada clase en orden creciente y las clases por su primera columna.
.clases_de_columnas_identicas <- function(datos, indices) {
  if (length(indices) < 2L) return(list())
  columnas <- lapply(indices, function(i) .normalizar_columna_igualdad(datos[[i]]))
  huellas <- vapply(columnas, .huella_columna_identica, character(1L))
  grupo_de <- match(huellas, unique(huellas))
  clase <- seq_along(columnas)
  for (miembros in split(seq_along(columnas), grupo_de)) {
    if (length(miembros) < 2L) next
    representantes <- miembros[[1L]]
    for (k in miembros[-1L]) {
      igual_a <- 0L
      for (r in representantes) {
        if (.columnas_normalizadas_identicas(columnas[[r]], columnas[[k]])) {
          igual_a <- r
          break
        }
      }
      if (igual_a) {
        clase[[k]] <- igual_a
      } else {
        representantes <- c(representantes, k)
      }
    }
  }
  unname(Filter(function(m) length(m) > 1L, split(seq_along(clase), clase)))
}

# Todos los pares de cada clase, en el orden de `combn()`. Es la forma que
# consume `detectar_claves()`, que publica para cada clave las demas de su
# clase.
.pares_de_columnas_identicas <- function(datos, indices, nombres) {
  vacio <- .data_frame_simple(
    columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
  )
  con_pares <- .clases_de_columnas_identicas(datos, indices)
  if (!length(con_pares)) return(vacio)
  pares <- do.call(cbind, lapply(con_pares, function(m) utils::combn(m, 2L)))
  orden <- .orden_por_bytes(pares[1L, ], pares[2L, ])
  primero <- indices[pares[1L, orden]]
  segundo <- indices[pares[2L, orden]]
  .data_frame_simple(
    columna_1 = nombres[primero],
    columna_2 = nombres[segundo],
    stringsAsFactors = FALSE
  )
}

# Cada columna repetida junto a la PRIMERA de su clase, en el orden de los
# datos: k columnas iguales son k - 1 filas, no k (k - 1) / 2. Es la forma de
# `duplicated()` y de `.fila_duplicada`, que tambien marcan cada repeticion y
# no cada par; con dos columnas es la misma fila que daba el par. La clase de
# cada fila va en el atributo `grupo`, y las posiciones de sus dos columnas en
# `posicion_1` y `posicion_2`, porque dos columnas pueden llamarse igual y el
# nombre no alcanza para separar dos clases ni para decir cual es cual.
.grupos_de_columnas_identicas <- function(datos, indices, nombres) {
  vacio <- .data_frame_simple(
    columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
  )
  attr(vacio, "grupo") <- integer()
  attr(vacio, "posicion_1") <- integer()
  attr(vacio, "posicion_2") <- integer()
  clases <- .clases_de_columnas_identicas(datos, indices)
  if (!length(clases)) return(vacio)
  primero <- unlist(lapply(clases, function(m) rep.int(m[[1L]], length(m) - 1L)),
                    use.names = FALSE)
  segundo <- unlist(lapply(clases, function(m) m[-1L]), use.names = FALSE)
  grupo <- rep.int(seq_along(clases), lengths(clases) - 1L)
  salida <- .data_frame_simple(
    columna_1 = nombres[indices[primero]],
    columna_2 = nombres[indices[segundo]],
    stringsAsFactors = FALSE
  )
  attr(salida, "grupo") <- grupo
  attr(salida, "posicion_1") <- as.integer(indices[primero])
  attr(salida, "posicion_2") <- as.integer(indices[segundo])
  salida
}

# Los grupos de columnas iguales que guarda un perfil en
# `general$columnas_duplicadas`. Ahi cada columna repetida va junto a la primera
# de su grupo; un perfil guardado por una version anterior trae, en cambio,
# todos los pares. La igualdad es una equivalencia, asi que las dos formas dan
# los mismos grupos si se unen por componentes, y con eso el plan y la deriva
# leen igual un perfil viejo y uno nuevo. Cada grupo sale en el orden de las
# columnas del perfil, y los grupos por su primera columna. `grupo` dice a que
# grupo pertenece cada `clave` (los nombres como los compara el paquete).
.grupos_columnas_duplicadas_perfil <- function(perfil) {
  vacio <- list(grupos = list(), claves = character(), grupo = integer())
  pares <- if (is.list(perfil) && is.list(perfil$general)) {
    perfil$general$columnas_duplicadas
  } else NULL
  if (!is.data.frame(pares) || !nrow(pares) ||
      !all(c("columna_1", "columna_2") %in% names(pares))) {
    return(vacio)
  }
  extremo_1 <- as.character(pares$columna_1)
  extremo_2 <- as.character(pares$columna_2)
  validos <- !is.na(extremo_1) & !is.na(extremo_2)
  if (!any(validos)) return(vacio)
  extremo_1 <- extremo_1[validos]
  extremo_2 <- extremo_2[validos]
  originales <- c(extremo_1, extremo_2)
  claves_extremos <- .nombres_para_operar(originales)
  claves <- unique(claves_extremos)
  originales <- originales[match(claves, claves_extremos)]
  n_pares <- length(extremo_1)
  componente <- .unir_componentes_vocabulario(length(claves), cbind(
    match(claves_extremos[seq_len(n_pares)], claves),
    match(claves_extremos[n_pares + seq_len(n_pares)], claves)
  ))
  columnas_perfil <- if (is.data.frame(perfil$columnas)) {
    .nombres_para_operar(as.character(perfil$columnas$columna))
  } else character()
  posicion <- match(claves, columnas_perfil)
  orden <- .orden_por_bytes(posicion, seq_along(claves))
  # Los grupos se numeran por su primera columna, en ese mismo orden.
  numero <- match(componente, unique(componente[orden]))
  grupos <- unname(split(originales[orden], numero[orden]))
  list(grupos = grupos, claves = claves, grupo = numero)
}

# Los nombres de un hallazgo `columnas_duplicadas`, del grupo que guarda el
# perfil. Se busca por `columna`, que es una columna del grupo -la primera en
# un perfil nuevo, el primer extremo del par en uno viejo- y no por la
# evidencia, que es prosa y puede estar protegida. Si el perfil no trae los
# grupos, la evidencia es lo unico que queda: `A = B = C; comparadas...`.
.miembros_columnas_duplicadas <- function(perfil, hallazgo, grupos = NULL) {
  if (is.null(grupos)) grupos <- .grupos_columnas_duplicadas_perfil(perfil)
  g <- .grupo_de_columna_duplicada(grupos, hallazgo$columna[[1L]])
  if (is.na(g)) NULL else grupos$grupos[[g]]
}

.grupo_de_columna_duplicada <- function(grupos, columna) {
  columna <- as.character(columna)
  if (!length(grupos$grupos) || length(columna) != 1L || is.na(columna)) {
    return(NA_integer_)
  }
  grupos$grupo[match(.nombres_para_operar(columna), grupos$claves)]
}

.miembros_columnas_duplicadas_evidencia <- function(evidencia) {
  evidencia <- as.character(evidencia)
  if (length(evidencia) != 1L || is.na(evidencia)) return(character())
  # Una evidencia con los nombres entre comillas es la de nombres que no se
  # pueden partir por " = " -tienen el separador, se repiten o estan vacios-:
  # no se adivina.
  if (startsWith(evidencia, "\"")) return(character())
  tramo <- trimws(sub(";.*$", "", evidencia))
  partes <- strsplit(tramo, " = ", fixed = TRUE)[[1L]]
  if (length(partes) < 2L) character() else partes
}

# Huella para agrupar columnas antes de compararlas. Solo tiene que cumplir una
# cosa: dos columnas que `.columnas_normalizadas_identicas()` da por iguales
# tienen la misma huella. Por eso usa nada mas que lo que esa comparacion exige
# igual -clase, tipo, largo, faltantes- y unos pocos valores, cada uno en una
# forma que no separa lo que `identical()` junta: `as.character()` da "0" para
# `0` y para `-0`, y "NA" o "NaN" segun `identical()` los distingue. Un texto
# solo aporta sus bytes si es ASCII; uno con bytes altos aporta una marca,
# porque `identical()` puede igualar dos codificaciones del mismo texto (y
# hacerlo distinto segun el locale), y una clave por bytes las separaria. Un
# `integer64` aporta sus bits, que es lo que la comparacion mira.
.huella_columna_identica <- function(x) {
  n <- length(x)
  cabeza <- paste(c(typeof(x), class(x), n), collapse = "\r")
  crudo <- unclass(x)
  if (!is.atomic(crudo) || !is.null(dim(crudo))) {
    faltantes <- if (is.list(crudo) && is.null(dim(crudo))) {
      sum(is.na(crudo))
    } else NA
    return(paste(cabeza, faltantes, sep = "\n"))
  }
  faltantes <- sum(is.na(crudo))
  if (!n) return(paste(cabeza, faltantes, sep = "\n"))
  posiciones <- unique(round(seq(1, n, length.out = min(n, 8L))))
  muestra <- crudo[posiciones]
  valores <- if (inherits(x, "integer64")) {
    as.character(writeBin(as.double(muestra), raw()))
  } else if (is.character(muestra)) {
    vapply(muestra, function(s) {
      if (is.na(s)) return("\001")
      bytes <- charToRaw(s)
      marca <- if (identical(Encoding(s), "bytes")) "\002" else ""
      if (any(bytes > as.raw(0x7f))) paste0(marca, "\003") else paste0(marca, s)
    }, character(1L), USE.NAMES = FALSE)
  } else {
    as.character(muestra)
  }
  if (is.factor(x)) valores <- c(valores, length(levels(x)))
  paste(c(cabeza, faltantes, valores), collapse = "\n")
}

# `muestra = 1.5` se aceptaba y se perfilaba UNA fila, en silencio. Un usuario
# que pide una muestra y medio recibe un perfil sobre una fila sin enterarse, y
# la misma llamada por la via DBI da error. Ahora las dos rechazan el no entero.
#
# La unica diferencia que queda entre las dos vias es deliberada y esta
# documentada: `Inf` vale en memoria -significa "todas las filas"- y no vale
# contra un motor, donde hay que decir cuantas filas traer.
# Las dos comprobaciones de entrada que mas se repetian: once veces la de `datos`
# y seis la de `perfil`. La de `perfil` habia derivado en dos redacciones que
# conviven en el mismo archivo a trescientas lineas de distancia -«a las columnas
# de `datos`» y «a `datos`»-, que es lo que pasa cuando la misma regla se escribe
# muchas veces: no diverge de golpe, diverge de a poco.
.validar_datos_tabla <- function(datos, nombre = "datos") {
  if (!inherits(datos, "data.frame")) {
    .detener("`", nombre, "` debe heredar de data.frame.", call. = FALSE)
  }
  invisible(datos)
}

# Las funciones de vector reciben UNA columna. `tabla[, "f"]` da un vector en
# un data.frame y una tabla de una columna en un tibble, una data.table o un
# grouped_df: la misma expresion, otra respuesta. `inferir_tipo()` contestaba
# sobre la tabla -`tipo = "tbl_df"`, "4 de 4 valores compatibles"- donde el
# vector daba `texto`, mientras sus hermanas la rechazaban. Medido. Se rechaza
# con el mensaje que ya daba `detectar_formatos_fecha()`.
.validar_vector_no_tabla <- function(x) {
  if (inherits(x, "data.frame")) {
    .detener("`x` debe ser un vector, no un data.frame.", call. = FALSE)
  }
  invisible(x)
}

# La bandera de proteccion, en UN lugar. Estaba escrita once veces y faltaba en
# dos: `detectar_discordancias()` y `descubrir_patrones()` la leian con
# `isTRUE()`, asi que `NA`, `NULL`, `"si"`, `"TRUE"` -el texto que deja un
# archivo de configuracion-, `c(TRUE, TRUE)` o `-1` APAGABAN la proteccion y
# publicaban los correos crudos, mientras las once hermanas rechazaban esos
# mismos valores. Una proteccion que ante la duda abre falla del lado que
# expone. Medido. Las puertas que la reciben por `...`
# (`perfilar_por()`, `perfilar_dbi()`, `perfilar_coleccion()`) la validan aca
# tambien cuando se pasa: por DBI el resumen SQL la leia con `isTRUE()` y
# publicaba el valor que el mismo resumen ocultaba con `TRUE`.
.validar_bandera_proteccion <- function(x) {
  .validar_bandera(x, "proteger_datos_personales")
}

# Un logico escalar sin NA, con el mensaje que el paquete ya da en sus
# interruptores ("`x` debe ser TRUE o FALSE.").
.validar_bandera <- function(x, nombre) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .detener("`", nombre, "` debe ser TRUE o FALSE.", call. = FALSE)
  }
  invisible(x)
}

# `match.arg()` con el error del paquete. Sobre `NA`, un numero, `TRUE` o
# `list()` R decia "'arg' must be NULL or a character vector", y con dos textos
# "'arg' must be of length 1": ninguno nombraba el argumento ni sus opciones.
# Medido en diez argumentos exportados. Se usa como
# `match.arg()` -las opciones salen de los formales de quien llama, como alli,
# o se pasan- y sobre todo valor que `match.arg()` acepta devuelve lo mismo:
# `NULL` y el vector entero de opciones dan la primera, y un prefijo unico se
# completa. Solo cambia el error.
.elegir_opcion <- function(arg, opciones) {
  nombre <- as.character(substitute(arg))
  if (missing(opciones)) {
    llamador <- sys.parent()
    opciones <- eval(formals(sys.function(llamador))[[nombre]],
                     envir = sys.frame(llamador))
  }
  elegido <- if (is.null(arg) || (is.character(arg) && !anyNA(arg) &&
                                  (length(arg) == 1L ||
                                   identical(arg, opciones)))) {
    tryCatch(match.arg(arg, opciones), error = function(e) NULL)
  }
  if (is.null(elegido)) {
    citadas <- paste0("'", opciones, "'")
    lista <- if (length(citadas) > 1L) {
      paste0(paste(utils::head(citadas, -1L), collapse = ", "), " o ",
             utils::tail(citadas, 1L))
    } else citadas
    .detener("`", nombre, "` debe ser ", lista, ".", call. = FALSE)
  }
  elegido
}

# Las comprobaciones que validan varios umbrales juntos los unian con `c()`, y
# `c()` borra un `NULL` o un `list()`, junta un `TRUE` como 1 y aplana un vector
# de dos: el valor malo pasaba la guarda y abortaba despues con un error de R
# -"argumento tiene longitud cero", "valor ausente donde TRUE/FALSE es
# necesario", "unsupported type" en `formatC()`-. Medido. Antes
# de unirlos, cada uno tiene que ser UN numero.
.son_numeros_escalares <- function(valores) {
  all(vapply(valores, function(v) is.numeric(v) && length(v) == 1L,
             logical(1L)))
}

# La misma bandera cuando llega por `...`: solo si se paso. Ausente vale la
# omision de cada funcion, que desde el 2026-10-25 no protege.
.validar_bandera_proteccion_en <- function(argumentos) {
  if ("proteger_datos_personales" %in% names(argumentos)) {
    .validar_bandera_proteccion(argumentos[["proteger_datos_personales"]])
  }
  invisible(argumentos)
}

# Si la llamada protege, leido de los argumentos que viajan en `...`: ausente
# vale la omision, que desde el 2026-10-25 no protege -la proteccion es un
# complemento que se pide-. Por el nombre exacto, como la validacion de
# arriba: `$` casaba tambien un nombre que empieza con este. La regla estaba
# escrita cuatro veces en la via DBI -`perfilar_dbi()`, las dos del spool y la
# via por bloques-, y hace falta tambien en la entrada de
# `perfilar_dbi()` y de `perfilar_coleccion()`, para el mensaje del motor
# (`.fijar_proteccion_motor()`): una sola, para que no se separen.
.protege_argumentos <- function(argumentos) {
  isTRUE(argumentos[["proteger_datos_personales"]])
}

# Los argumentos que viajan por `...` hasta `perfilar()` se leen con el nombre
# COMPLETO del formal que R les asigna. `do.call(perfilar, ...)`
# completa un nombre abreviado -`proteger = FALSE` es
# `proteger_datos_personales = FALSE`-, y quien leia la lista antes lo leia por
# el nombre exacto: `perfilar_dbi(proteger = FALSE)` perfilaba la muestra sin
# proteccion y tapaba el resumen SQL, `proteger = "si"` no daba el error del
# paquete sino el fallo parcial, y `perfilar_por()` lee asi seis argumentos mas.
# Dos lectores de la misma lista que no contestaban lo mismo. Los nombres se
# completan aca con la regla de R para los nombrados -primero los exactos,
# despues el prefijo que casa con un solo formal de los que quedan: `pmatch()`
# sin duplicados- contra los formales de `perfilar()`, que no tiene `...`, y
# todos leen lo que `perfilar()` va a recibir. Un nombre que no casa, o que
# casa con varios, queda como llego: `perfilar()` da su error.
.argumentos_para_perfilar <- function(argumentos) {
  nombres <- names(argumentos)
  if (!length(argumentos) || is.null(nombres)) return(argumentos)
  candidatos <- which(!is.na(nombres) & nzchar(nombres) &
    !grepl("[^\\001-\\177]", nombres, useBytes = TRUE, perl = TRUE))
  if (!length(candidatos)) return(argumentos)
  formales <- names(formals(perfilar))
  casados <- pmatch(nombres[candidatos], formales, duplicates.ok = FALSE)
  nombres[candidatos[!is.na(casados)]] <- formales[casados[!is.na(casados)]]
  names(argumentos) <- nombres
  argumentos
}

# Lo que viaja por `...` hasta `perfilar()` se valida en la ENTRADA, haya o no
# algo que perfilar. Sin grupos que lleguen a `min_filas`, o con
# `bloque_muestra = "solo_agregados"`, `perfilar()` no corre y nada se
# validaba: pasaban en silencio una `fecha` invalida -`"ayer"`, `NA`, dos
# fechas- y un nombre que `perfilar()` no tiene -`no_existe = 1`, una errata-.
# Se validan aca la bandera, la `fecha`
# -la misma regla de las cinco puertas, `.fecha_argumento_utc()`- y los nombres,
# ya completados por `.argumentos_para_perfilar()` -los que la via toma para si,
# `propios`, tambien valen-. Los demas valores los valida
# `perfilar()` al perfilar: limitacion declarada en `?perfilar_por` y
# `?perfilar_dbi`.
.validar_argumentos_para_perfilar <- function(argumentos, propios = character()) {
  nombres <- names(argumentos)
  if (!is.null(nombres)) {
    formales <- c(names(formals(perfilar)), propios)
    desconocidos <- nombres[!is.na(nombres) & nzchar(nombres) &
                              !nombres %in% formales]
    if (length(desconocidos)) {
      # Un prefijo de varios formales no es "un argumento que no existe": es
      # ambiguo, y `perfilar()` directo lo dice asi. Se nombran los
      # que casan para que quien llama sepa cual escribir.
      primero <- desconocidos[[1L]]
      posibles <- if (!grepl("[^\\001-\\177]", primero, useBytes = TRUE,
                             perl = TRUE)) {
        formales[startsWith(formales, primero)]
      } else character()
      if (length(posibles) > 1L) {
        .detener(
          "`", primero, "` abrevia varios argumentos de `perfilar()` (",
          paste0("`", posibles, "`", collapse = ", "),
          "): escribirlo completo.", call. = FALSE
        )
      }
      .detener("`", primero, "` no es un argumento de `perfilar()`.",
               call. = FALSE)
    }
  }
  .validar_bandera_proteccion_en(argumentos)
  if ("fecha" %in% nombres) .fecha_argumento_utc(argumentos[["fecha"]])
  invisible(argumentos)
}

# Copia las subclases con semantica propia de `[` a un data.frame base antes
# de ejecutar codigo que selecciona columnas o agrega columnas. Es tambien la
# entrada comun de las funciones que reciben una tabla, y por eso lleva la guarda
# de `integer64`: un recorrido de las exportadas encontro cinco que solo pasan
# por aca.
.tabla_base <- function(tabla) {
  if (inherits(tabla, "data.frame")) .exigir_integer64_confiable(tabla)
  if (inherits(tabla, "data.table")) {
    return(as.data.frame(data.table::copy(tabla), stringsAsFactors = FALSE))
  }
  if (inherits(tabla, "tbl_df")) {
    return(as.data.frame(tabla, stringsAsFactors = FALSE))
  }
  tabla
}

# Equivalente a `data.frame[, columnas, drop = FALSE]`, sin depender de la
# clase de `datos` ni del estado del espacio de nombres. La seleccion de filas
# es opcional para los llamadores que necesitan ambas dimensiones.
.seleccionar_columnas <- function(datos, columnas, filas = NULL) {
  if (!inherits(datos, "data.frame")) {
    .detener("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  # Por posicion, tambien un nombre `NA`: una tabla de R puede tener una
  # columna llamada asi, y `[.data.frame` no la selecciona por nombre
  # ("columnas no definidas seleccionadas"). `perfilar_por()` abortaba con
  # ella. Un nombre que la tabla no tiene sigue
  # cayendo en `[` y su error.
  if (is.character(columnas) && !is.null(names(datos))) {
    indices <- .indice_nombre(columnas, names(datos))
    if (!anyNA(indices)) columnas <- indices
  }
  salida <- if (is.null(filas)) {
    base::`[.data.frame`(datos, , columnas, drop = FALSE)
  } else {
    base::`[.data.frame`(datos, filas, columnas, drop = FALSE)
  }
  # La clase de la entrada no forma parte de esta primitiva: los llamadores
  # trabajan con tablas base despues de la frontera y no necesitan propagar
  # subclases con otra semantica de `[`. La seleccion ya fue materializada.
  class(salida) <- "data.frame"
  salida
}

# `perfil` es opcional en casi todos los llamadores: si viene, tiene que
# corresponder a las mismas columnas y en el mismo orden, porque los indices de
# columna del perfil se usan para leer `datos`.
.validar_perfil_de <- function(perfil, datos) {
  if (!is.null(perfil) && (!inherits(perfil, "perfil") ||
        !identical(.nombres_para_operar(names(datos)),
                   .nombres_para_operar(perfil$columnas$columna)))) {
    .detener("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  invisible(perfil)
}

.validar_muestra <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      muestra < 1) {
    .detener("`muestra` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  if (is.finite(muestra) && muestra != floor(muestra)) {
    .detener(
      "`muestra` debe ser un entero positivo, o `Inf` para no muestrear.",
      call. = FALSE
    )
  }
  floor(muestra)
}

.muestrear_vector <- function(x, muestra) {
  limite_solicitado <- .validar_muestra(muestra)

  total <- length(x)
  limite <- min(total, limite_solicitado)
  if (total <= limite) {
    return(list(
      valores = x,
      total = total,
      analizados = total,
      muestreado = FALSE
    ))
  }

  indices <- unique(as.integer(round(seq.int(1, total, length.out = limite))))
  list(
    valores = x[indices],
    total = total,
    analizados = length(indices),
    muestreado = TRUE
  )
}

# Una muestra de filas que no se alinea con una periodicidad de los datos. La
# sistematica de `.muestrear_vector()` toma filas a paso fijo, y un paso que cae
# justo en el periodo de un panel toma siempre los mismos meses: con 599.995
# filas y 1e5 de muestra, solo los meses 1 y 7 de cada doce. Aca la tabla se
# parte en `limite` tramos consecutivos -los de la sistematica- y de cada uno
# se toma una fila en una posicion que sale de un hash del numero de tramo: el
# mismo hash entero biyectivo y determinista de las firmas MinHash
# (`.hash_lsh()`), que no usa el generador de numeros aleatorios de R ni su
# estado. Conserva lo que la sistematica tenia -una fila por tramo, repartida
# en toda la tabla, la misma muestra en cada corrida- y la fase de cada fila
# dentro del periodo queda repartida en vez de fija. Cuesta lo que la muestra,
# no lo que la tabla.
.muestrear_filas_dispersas <- function(total, muestra) {
  limite <- min(total, .validar_muestra(muestra))
  if (total <= limite) {
    return(list(
      valores = seq_len(total), total = total, analizados = total,
      muestreado = FALSE
    ))
  }
  tramo <- seq_len(limite) - 1
  inicio <- floor(tramo * total / limite)
  ancho <- floor((tramo + 1) * total / limite) - inicio
  desplazamiento <- floor(.hash_lsh(tramo, 1L) / .D32_LSH * ancho)
  indices <- as.integer(inicio + desplazamiento + 1)
  list(
    valores = indices, total = total, analizados = length(indices),
    muestreado = TRUE
  )
}

.es_numerico_pelado <- function(x) {
  # Un numero pelado es un `integer` o un `double` SIN clase declarada. El
  # paquete ya se escribio esta leccion dos veces -en `.n_subnormales()` y aca
  # abajo en `.tipo_declarado()`-: `Date`, `POSIXct`, `difftime`, `integer64`,
  # `units` y `hms` son todos `double` por debajo, y ENUMERAR LAS CLASES A MANO
  # DEJA AFUERA LA PROXIMA. La proxima llego, y no fue una cifra mal publicada
  # sino un ABORTO con datos legitimos: `is.numeric()` es TRUE para
  # `lubridate::Period` y para `units`, asi que las dos entraban a la busqueda
  # de relaciones proporcionales, y ahi el cociente entre dos columnas ejecuta
  # la aritmetica de la clase: "invalid class Period object: periods must have
  # integer values" y "both operands of the expression should be units
  # objects". `perfilar()` moria sobre una tabla que el usuario podia construir
  # sin hacer nada raro.
  #
  # Preguntar por la clase -y no por una lista de clases- cubre a las que hoy
  # existen y a las que se inventen manana, porque lo que estos diagnosticos
  # necesitan no es "algo que responda `is.numeric()`" sino una magnitud que
  # sume y divida como un doble.
  #
  # `AsIs` se descarta por la misma razon que en `.tipo_declarado()`: `I()` es
  # como el dato viajo dentro del `data.frame`, no una declaracion sobre el
  # dato.
  is.numeric(x) && !length(setdiff(oldClass(x), "AsIs"))
}

.clase_declarada_columna <- function(x) {
  clase <- setdiff(oldClass(x), "AsIs")
  if (length(clase)) clase[[1L]] else NA_character_
}

.es_numerica_con_clase <- function(x) {
  # Las columnas que R declara numericas pero traen una clase propia. Son las
  # que quedan fuera de los diagnosticos que necesitan un numero pelado, y por
  # eso hay que DECLARARLAS: la exclusion silenciosa es la que hacia que la
  # misma columna produjera una fila de cobertura guardada como `entero` y nada
  # guardada como `integer64`.
  # `is.numeric()` responde FALSE sobre un `difftime`, asi que esa clase no era ni
  # "pelada" ni "con clase": se caia por el hueco entre las dos y NINGUN
  # diagnostico la declaraba. Medido con la misma columna en tres clases: `units`
  # recibia su fila de cobertura en `ley_benford` y en
  # `relacion_aritmetica_columnas`, el `doble` recibia la de Benford, y el
  # `difftime` no recibia ninguna.
  #
  # Importa desde que `difftime` publica `minimo`, `maximo` y `media` como
  # cualquier numero: el silencio dejo de ser legible. Un lector que ve las cifras
  # espera que los diagnosticos numericos hayan corrido, y la regla del paquete es
  # que un diagnostico que no corre se declara.
  #
  # `Date` y `POSIXt` quedan afuera A PROPOSITO y no por olvido: tambien son
  # numeros con clase, pero no publican `media` como numero pelado, asi que su
  # silencio se lee distinto, y agregarlas pondria una fila de cobertura en casi
  # todo perfil con fechas. Eso es una decision de salida publicada y esta anotada
  # como tal, no resuelta de costado.
  (is.numeric(x) || inherits(x, "difftime")) && !.es_numerico_pelado(x)
}

.tipo_declarado <- function(x) {
  if (inherits(x, "sfc")) {
    return(class(x)[[1L]])
  }
  # Una tabla anidada es compuesta, pero no es una matriz: declara su propia
  # clase -`data.frame`, `tbl_df`-, que se publica tal cual mas abajo.
  if (.es_arreglo_compuesto(x)) {
    return("matriz")
  }
  if (inherits(x, "integer64")) {
    return("integer64")
  }
  if (inherits(x, "POSIXt")) {
    return("fecha-hora")
  }
  if (inherits(x, "Date")) {
    return("fecha")
  }
  if (is.ordered(x)) {
    return("factor-ordenado")
  }
  if (is.factor(x)) {
    return("factor")
  }
  # El paquete ya se escribio esta leccion en `hallazgos.R`: `Date`, `POSIXct`,
  # `difftime`, `integer64`, `units` y `hms` son todos `double` por debajo, y
  # **enumerar las clases a mano deja afuera la proxima**. Aca la enumeracion
  # dejaba afuera justamente a `difftime`: una columna de duraciones en minutos
  # publicaba `tipo_declarado = "doble"`, que no es lo que la columna declara de
  # si misma -y el paquete lo sabe, porque dos predicados la excluyen de Benford
  # y de las relaciones aritmeticas por `inherits(x, "difftime")`-. Publicando la
  # clase, la proxima entra sola.
  #
  # `AsIs` se descarta: `I()` es como el dato viajo dentro del `data.frame`, no
  # una declaracion sobre el dato. Sin esto, `I(1:10)` pasaria de `entero` a
  # `AsIs`, que no le dice nada a nadie.
  clase_declarada <- setdiff(oldClass(x), "AsIs")
  if (length(clase_declarada)) {
    return(clase_declarada[[1L]])
  }
  if (is.logical(x)) {
    return("logico")
  }
  if (is.integer(x)) {
    return("entero")
  }
  if (is.double(x)) {
    return("doble")
  }
  if (is.character(x)) {
    return("texto")
  }
  if (is.list(x)) {
    return("lista")
  }
  class(x)[[1L]]
}

.callar_moda_sin_empate <- function(columnas) {
  # No hay moda cuando todos los valores validos aparecen una sola vez: lo que
  # se publicaria es el que gana un desempate de n vias, y el desempate sigue
  # el orden de ordenamiento, que depende de como este guardada la columna.
  # Medido: `c(-5.5, -1, 0, 3.75)` daba moda `-5.5` guardada como doble y `-1`
  # guardada como texto, con `frecuencia_moda = 1` en los dos casos. El dato es
  # el mismo y la moda publicada, distinta.
  #
  # Con un solo valor distinto SI hay moda, aunque su frecuencia sea 1: ahi no
  # hay empate que resolver y el valor publicado no depende de nada.
  #
  # `frecuencia_moda` NO se toca: que la frecuencia maxima sea 1 es cierto y es
  # justamente la evidencia de por que la moda quedo callada.
  necesarios <- c("moda", "frecuencia_moda", "n_distintos")
  if (!is.data.frame(columnas) || !nrow(columnas) ||
      !all(necesarios %in% names(columnas))) {
    return(columnas)
  }
  frecuencia <- suppressWarnings(as.numeric(columnas[["frecuencia_moda"]]))
  distintos <- suppressWarnings(as.numeric(columnas[["n_distintos"]]))
  validos <- if ("n_validos" %in% names(columnas)) {
    suppressWarnings(as.numeric(columnas[["n_validos"]]))
  } else rep(NA_real_, nrow(columnas))
  # Sin `n_distintos` -no se pidio, o su consulta se corto por tiempo- la
  # frecuencia 1 sobre dos o mas valores validos dice lo mismo: ninguno se
  # repite. Antes esa columna publicaba como moda el primero del orden.
  varios <- (!is.na(distintos) & distintos > 1) |
    (is.na(distintos) & !is.na(validos) & validos > 1)
  sin_moda <- !is.na(frecuencia) & frecuencia == 1 & varios
  # Tambien en una columna protegida. La proteccion tapa un valor; no cambia
  # el hecho de que no lo hay. Antes la protegida quedaba con el marcador
  # `[valor protegido]` -la misma escritura que una columna que SI tiene moda-
  # con el argumento de que el NA diria que la columna no repite ningun valor.
  # Pero eso ya lo dicen `frecuencia_moda = 1` y `n_distintos`, que se publican
  # con o sin proteccion: el marcador no ocultaba nada y afirmaba una moda que
  # no existe. La proteccion corre antes que esto y solo tapa una
  # moda presente, asi que el NA no se vuelve a tapar.
  if (any(sin_moda)) {
    columnas[["moda"]][sin_moda] <- NA
  }
  columnas
}

.sellar_perfil_dbi <- function(estructura) {
  # Unico lugar donde un objeto pasa a ser `perfil_dbi`. Existe porque la clase
  # se asignaba en cuatro sitios distintos, y una regla que se aplica "al
  # salir" se olvidaba en tres: con `bloque_muestra = "solo_agregados"` el
  # perfil publicaba una moda que `perfilar()` ya callaba sobre el mismo dato.
  # Que un camino nuevo se olvide de la regla ahora es imposible: para tener la
  # clase tiene que pasar por aca.
  if (is.list(estructura) &&
      is.data.frame(estructura$resumen_tabla$columnas)) {
    estructura$resumen_tabla$columnas <- .callar_moda_sin_empate(
      estructura$resumen_tabla$columnas
    )
  }
  class(estructura) <- "perfil_dbi"
  estructura
}

.zona_horaria_origen <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_character_)
  zona <- attr(x, "tzone", exact = TRUE)
  if (length(zona) && !is.na(zona[[1L]]) && nzchar(zona[[1L]])) {
    as.character(zona[[1L]])
  } else {
    "sin_declarar"
  }
}

.fechas_civiles_distintas_utc <- function(x) {
  if (!inherits(x, "POSIXt")) return(NA_integer_)
  # Un `POSIXlt` se cuenta como su `POSIXct`: sus campos son la hora civil de
  # su propia zona y no se pueden comparar con UTC campo por campo.
  if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
  zona <- .zona_horaria_origen(x)
  if (identical(zona, "sin_declarar")) return(NA_integer_)
  presentes <- !is.na(x)
  if (!any(presentes)) return(0L)
  civil_origen <- format(x[presentes], "%Y-%m-%d", tz = zona)
  civil_utc <- format(x[presentes], "%Y-%m-%d", tz = "UTC")
  as.integer(sum(civil_origen != civil_utc, na.rm = TRUE))
}

# `deparse()` de una expresion larga devuelve VARIAS lineas, y con
# `do.call(perfilar, list(data.frame(...)))` la expresion es la tabla entera: el
# nombre salia con ocho elementos y `reportar()` reventaba con "values must be
# length 1". `do.call()` es una forma corriente de llamar a esto -pasar los
# argumentos en una lista es lo natural cuando se perfila en un bucle-, asi que
# el nombre tiene que sobrevivirla.
#
# Si la expresion no cabe en una linea no sirve como nombre: no lo escribio
# nadie, es una tabla deparseada. En ese caso se usa una etiqueta generica.
#
# Vivia DENTRO del cuerpo de `perfilar()`, asi que era local: `analizar()`, que
# tiene el mismo problema y llega por la misma puerta, no podia usarla y
# publicaba el nombre deparseado en varios elementos -con `[valor protegido]`
# incrustado, porque la capa de proteccion enmascaraba los numeros del propio
# nombre-. Vive aca, que es donde se ve.
.nombre_de_los_datos <- function(expresion) {
  texto <- tryCatch(.deparse_por_omision(expresion), error = function(e) character())
  if (length(texto) != 1L || !nzchar(trimws(texto)) || nchar(texto) > 120L) {
    return("datos")
  }
  texto
}

.texto_valor <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) {
    return(NA_character_)
  }
  if (inherits(x, "POSIXt")) {
    return(format(x[[1L]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(format(x[[1L]], "%Y-%m-%d"))
  }
  if (is.numeric(x)) {
    return(.formatear_numero_publicado(x[[1L]]))
  }
  as.character(x[[1L]])
}

# La moda numerica, como la promete `?perfilar`: notacion fija, la precision
# COMPLETA e independiente de `scipen` y `digits`, con la marca de `OutDec`.
# `.formatear_numero_publicado()` escribe 15 cifras, que alcanzan casi siempre
# pero no siempre: la moda `0.1 + 0.2` salia "0.3" y `1/3` "0.333333333333333",
# que no vuelven a su valor, y dos tablas con moda 0.3 y 0.1 + 0.2 -distintas,
# `0.3 == 0.1 + 0.2` es FALSE- se declaraban identicas en
# `comparar_equivalencia()` en el mismo par donde el minimo salia distinto. Se
# escriben las cifras que hacen falta para volver -15, 16 o 17, la regla de
# `.cifras_reversibles()`-. Los tres caminos de `perfilar_dbi()` y `perfilar()`
# escriben la moda con esta funcion: la via por bloques usaba `as.character()` y
# publicaba "1e-20" donde las otras publicaban "0.00000000000000000001".
.texto_moda <- function(x) {
  if (length(x) == 0L || is.na(x[[1L]])) return(NA_character_)
  if (!is.double(x) || is.object(x)) return(.texto_valor(x))
  valor <- x[[1L]]
  texto <- .formatear_numero_publicado(valor)
  cifras <- .cifras_reversibles(valor, texto)
  if (!is.na(cifras)) {
    texto <- format(valor, scientific = FALSE, trim = TRUE, digits = cifras)
  }
  texto
}

# Los numeros que entran en texto publicado no deben obedecer las preferencias
# de IMPRESION de la sesion, que son dos y hay que cerrar las dos:
#
#   `scipen`  elige notacion cientifica. Con `scipen = -5` un `3.5` se publicaba
#             `3.5e+00`, y un umbral `3` del propio paquete, `3e+00`.
#   `digits`  elige cuantas cifras significativas. Este es el que se abrio al
#             cerrar el anterior: `format(x, scientific = FALSE)` obedece a
#             `digits`, y `as.character()` -lo que habia antes- no. Medido:
#             `123456.789012345` se publicaba `123456.8` **sin tocar ninguna
#             opcion**, y `123457` con `digits = 3`. Peor que el defecto que se
#             estaba arreglando, porque `perfilar.Rd` promete el valor "tal como
#             llego, sin reinterpretarlo".
#
# `digits = 15` es la precision que `as.character()` usa siempre, asi que fija
# la salida al valor completo y la vuelve inmune a las dos opciones. Comprobado
# sobre 3.5, 123456.789012345, 1/3, 1e-7, 1e20, 0.1+0.2, 2^53 y -1.23456789e-7,
# cruzando `digits` en {3, 7, 15} con `scipen` en {0, -5, 100}: identico en los
# nueve estados.
#
# Lo que NO se toca es `OutDec`: es el locale que eligio la persona, y en un
# paquete en español para esta region `3,5` es lo correcto. Comprobado que con
# `OutDec = ","`, `scipen = -5` y `digits = 3` a la vez sale `3,5` y
# `123456,789012345`.
# La etiqueta de un grupo no es un numero PUBLICADO: es un identificador, y tiene
# que volver al valor del que salio. `as.character()` sobre un doble usa 15 cifras
# significativas, que no alcanzan para distinguir dos dobles distintos: `1e17` y
# `1e17 + 32` dan los dos "1e+17", y `perfilar_por()` los fundia en un solo grupo de
# 80 filas cuyos numeros no correspondian a ninguno de los dos -20 filas duplicadas
# contra 14 y 6 de cada valor por separado-, sin declarar la fusion.
#
# No se reusa `.formatear_numero_publicado()` porque contesta otra pregunta -"como se
# muestra este numero", con 15 cifras y sin notacion cientifica- y las dos respuestas
# no pueden ser la misma: una etiqueta que no vuelve al valor no identifica al grupo.
#
# Se busca la forma MAS CORTA que vuelve: 15 cifras si alcanzan -y alcanzan para la
# enorme mayoria, asi que `0.1` sigue siendo `"0.1"`-, y si no, 16 y 17. Con 17 cifras
# significativas todo doble finito vuelve exacto.
# `as.character()` sobre un doble depende de dos opciones de la sesion: `scipen`
# -`1e+07` o `10000000`- y `OutDec` -`0,5`-. Lo demas no la mueve (medido:
# `digits` y `width` no). Lo que se guarda, se compara entre sesiones o se publica
# como clave no puede depender de como imprime la sesion que lo escribio: dos
# corridas identicas medidas con otro `scipen` no se podian acumular -"Difiere en:
# configuracion_perfil"- y la deriva publicaba un cambio de configuracion como
# error. Se fijan las dos en sus valores por omision, que reproduce la escritura
# de siempre: lo guardado con las opciones por omision sigue coincidiendo.
.caracter_por_omision <- function(x) {
  if (!is.double(x) || inherits(x, "integer64")) return(as.character(x))
  opciones <- options(scipen = 0, OutDec = ".")
  on.exit(options(opciones), add = TRUE)
  .marca_decimal_c(as.character(x))
}

# Lo mismo para `deparse()`, que escribe los numeros de una expresion con
# `scipen` -no con `OutDec` ni con `digits`-: con `scipen = -9`, `valor > 0.5`
# se escribe `valor > 5e-01`, y la configuracion de un perfil de evaluacion, la
# formula de una aplicabilidad o el nombre de los datos cambiaban con la sesion;
# la deriva publicaba "Cambio el perfil de evaluacion" sobre el mismo perfil.
# Todo `deparse()` del paquete pasa por aca.
#
# Y no es la unica perilla. `deparse()` escribe la marca decimal
# de `LC_NUMERIC` -`x > 0,9` con `Sys.setlocale("LC_NUMERIC", "es_UY.utf8")`- y
# bajo el locale `C` escapa el texto no ASCII: `"S<U+00ED>"` en una formula,
# `"S\303\255"` en una funcion y el simbolo entre comillas invertidas. Lo que
# sale de aca es una CLAVE que se compara entre procesos -la configuracion de una
# medicion y de un perfil-, y una corrida de cron (`C`) contra una de RStudio
# afirmaba un cambio de modelo, de aplicabilidad y de perfil que no hubo. Se
# escribe con `LC_NUMERIC` en "C" y, bajo `C`, con `LC_CTYPE` en un locale UTF-8
# mientras dura la escritura: sale lo mismo que en una sesion UTF-8, byte por
# byte y con los mismos cortes de linea -que `deparse()` decide por bytes, asi
# que reemplazar el texto por una marca ASCII los movia-. En una sesion UTF-8 no
# se toca nada, y lo guardado antes sigue apareando.
.deparse_por_omision <- function(expr, ...) {
  opciones <- options(scipen = 0)
  on.exit(options(opciones), add = TRUE)
  numerica <- Sys.getlocale("LC_NUMERIC")
  if (nzchar(numerica) && !numerica %in% c("C", "POSIX")) {
    Sys.setlocale("LC_NUMERIC", "C")
    # Volver a un `LC_NUMERIC` que no es "C" avisa siempre; bajo `warn = 2` el
    # aviso seria un error del paquete.
    on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", numerica)), add = TRUE)
  }
  caracteres <- Sys.getlocale("LC_CTYPE")
  if (caracteres %in% c("C", "POSIX")) {
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", caracteres)), add = TRUE)
    if (.poner_ctype_utf8()) {
      return(.declarar_utf8_valido(deparse(expr, ...)))
    }
  }
  deparse(expr, ...)
}

# Un locale UTF-8 para `LC_CTYPE`, el primero que el sistema acepte. Si no hay
# ninguno, `deparse()` escribe como siempre bajo `C`.
.poner_ctype_utf8 <- function() {
  for (candidato in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8",
                      "UTF-8")) {
    puesto <- suppressWarnings(Sys.setlocale("LC_CTYPE", candidato))
    if (length(puesto) == 1L && !is.na(puesto) && nzchar(puesto) &&
        isTRUE(l10n_info()[["UTF-8"]])) {
      return(TRUE)
    }
  }
  FALSE
}

# Lo que se LEE de una conexion tampoco puede depender de `LC_NUMERIC`. Los
# controladores convierten el texto que devuelve el
# motor con `strtod()` de C, que sigue la marca decimal del locale: con
# `Sys.setlocale("LC_NUMERIC", "es_UY.utf8")` RPostgres leia `2.5` como `2` -y
# odbc, un `decimal` de SQL Server-, y `perfilar_dbi()` publicaba en silencio
# media 3, mediana 2, minimo 0 y maximo 10 sobre una columna de media 3.7,
# mediana 2.5, minimo 0.5 y maximo 10.25. El paquete no arregla el
# controlador, pero fija la marca en la ENTRADA: toda exportada que lee de una
# conexion llama a esto primero; pone "C" y registra en el marco de quien
# llama la vuelta al valor anterior, que corre al salir, tambien en error. Si
# ya esta en "C" no toca nada: una exportada que llama a otra no la fija dos
# veces ni la devuelve antes de tiempo.
.fijar_numerica_c <- function(marco = parent.frame()) {
  numerica <- Sys.getlocale("LC_NUMERIC")
  if (!nzchar(numerica) || numerica %in% c("C", "POSIX")) {
    return(invisible(FALSE))
  }
  Sys.setlocale("LC_NUMERIC", "C")
  # Volver a un `LC_NUMERIC` que no es "C" avisa siempre; bajo `warn = 2` el
  # aviso seria un error del paquete.
  volver <- substitute(
    suppressWarnings(Sys.setlocale("LC_NUMERIC", anterior)),
    list(anterior = numerica)
  )
  do.call(on.exit, list(volver, add = TRUE), envir = marco)
  invisible(TRUE)
}

# Un texto con fraccion de segundo tambien se LEE con `LC_NUMERIC`.
# `strptime()` lee la fraccion de `%OS` con el `strtod()` de C, que sigue la
# marca decimal del locale: con `Sys.setlocale("LC_NUMERIC", "es_UY.utf8")`
# `"10:00:05.25"` se leia `10:00:05`, sin aviso. Un plan armado en una sesion
# "C" salia `lista` y `aplicar()` en una sesion con coma escribia 7 instantes
# distintos de 12; en la misma sesion el plan bloqueaba la conversion como "no
# inyectiva", y una `fecha` de texto con fraccion se guardaba al segundo en las
# cinco puertas. `as.numeric()` no tiene ese problema: lee con el analizador
# propio de R. La lectura de un texto con formato es una sola, aca, con
# `LC_NUMERIC` en "C" mientras dura -la construccion de la lectura por DBI-; 31
# exportadas llegan a ella por `.es_fecha_valida()`, `.parsear_fechas()`,
# `.detectar_formatos_fecha_interno()` y `.fecha_utc()`. Una prueba recorre el
# espacio de nombres y rechaza otro `strptime()` u otro `as.POSIXct(format = )`.
.strptime_utc <- function(x, formato, tz = "UTC") {
  .fijar_numerica_c()
  strptime(x, format = formato, tz = tz)
}

# Hay una tercera perilla, y no es una opcion: `LC_NUMERIC`. R arranca siempre en
# "C", pero quien la cambia con `Sys.setlocale()` cambia la marca que escriben
# `as.character()` y `sprintf()`, con `OutDec = "."` y todo. Medido con
# `de_DE.UTF-8`: `as.character(1.5)` daba "1,5", la comprobacion de
# `.cifras_reversibles()` no lo reconocia como numero y avisaba "NAs
# introducidos por coercion", y `0.1 + 0.2` salia "0.3", sin las cifras que lo
# devuelven a su valor. Es la construccion que la sugerencia de ausencia
# estructural ya habia cubierto con `Sys.localeconv()`: se detecta igual y se
# devuelve el punto, aca, donde se escribe para todos.
.marca_decimal_c <- function(texto) {
  marca <- Sys.localeconv()[["decimal_point"]]
  if (length(marca) != 1L || is.na(marca) || !nzchar(marca) ||
      identical(marca, ".")) {
    return(texto)
  }
  gsub(marca, ".", texto, fixed = TRUE)
}

# Un instante se publica de una sola forma: en UTC, con el rotulo `UTC`, al
# segundo y con la fraccion exacta si la tiene -la escritura de las claves de
# relacion, `.escritura_exacta_temporal()`-, que vuelve a su valor. Sin zona,
# `as.character()` y `format()` escriben en la hora de la sesion: la etiqueta de
# `distribucion_valores()` y de `perfilar_por()` cambiaba con `TZ`, y en
# America/Adak dos instantes distintos -10:30 y 11:30 UTC del dia del cambio de
# hora- salian los dos `01:30:00`; la clave de la normalizacion los fundia.
# La zona declarada de la columna se publica aparte, en
# `zona_horaria_origen`.
#
# Es tambien la escritura de un instante que se COMPARA: `format()`
# sin formato toma los decimales de `options(digits.secs)`, asi que con el valor
# por omision dos instantes del mismo segundo salian iguales. El plan contaba
# "10 valores participan en una conversion no inyectiva" y bloqueaba una
# conversion que conserva cada valor -con `digits.secs = 6` la dejaba `lista`-,
# y la configuracion de una medicion cambiaba con la opcion. `compacta = TRUE`
# es la forma que guardaba esa configuracion -la de `format()` sin formato en la
# sesion por omision: la fecha sola si todos caen a medianoche, si no al
# segundo-, con la fraccion exacta agregada: lo guardado antes sin fraccion
# sigue apareando. El formato se elige aca y se pasa explicito: el argumento
# `digits` de `format.POSIXlt()` no existe en R 4.1 -va a `...` y se ignora-,
# y alli `format()` seguia a `digits.secs` (medido en 4.1.3: `00:00:00.5`, y la
# fraccion exacta se agregaba dos veces).
.texto_instante_utc <- function(x, compacta = FALSE) {
  x <- as.POSIXct(x)
  formato <- "%Y-%m-%d %H:%M:%S"
  if (isTRUE(compacta)) {
    partes <- as.POSIXlt(x, tz = "UTC")
    reloj <- c(partes$sec, partes$min, partes$hour)
    if (all(reloj[is.finite(reloj)] == 0)) formato <- "%Y-%m-%d"
  }
  texto <- format(x, formato, tz = "UTC")
  salida <- paste(.escritura_exacta_temporal(texto, as.numeric(x)), "UTC")
  salida[is.na(x)] <- NA_character_
  salida
}

.etiqueta_numero_reversible <- function(x) {
  # Tres clases que `is.double()` da por dobles y no lo son para esto: un
  # `integer64` guarda sus bits en un doble -`sprintf()` los leia crudos y la
  # etiqueta salia `4.45e-308`-, y una fecha o fecha-hora salia como su numero de
  # dias o de segundos. Cada una tiene su escritura reversible.
  if (inherits(x, "integer64")) return(as.character(x))
  if (inherits(x, "Date")) return(as.character(x))
  if (inherits(x, "POSIXt")) return(.texto_instante_utc(x))
  # Y el complejo no entraba: `as.character()` le da 15 cifras a cada parte y
  # `1e17` y `1e17 + 32` caian en un solo grupo, el defecto que esta funcion
  # cerro para los dobles.
  if (is.complex(x)) {
    real <- .etiqueta_numero_reversible(Re(x))
    imaginaria <- .etiqueta_numero_reversible(Im(x))
    salida <- paste0(real, ifelse(Im(x) < 0, "", "+"), imaginaria, "i")
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  salida <- .caracter_por_omision(x)
  if (!is.double(x)) return(salida)
  cifras <- .cifras_reversibles(x, salida)
  faltan <- !is.na(cifras)
  if (any(faltan)) {
    salida[faltan] <- .marca_decimal_c(
      sprintf(paste0("%.", cifras[faltan], "g"), x[faltan])
    )
  }
  salida
}

# Cuantas cifras significativas le faltan al texto de un doble para volver a SU
# valor: `NA` si el que ya tiene vuelve -o no es un numero finito-; si no, 16 o
# 17, las que necesitan `1/3` o `0.1 + 0.2`. Es la regla de la etiqueta
# reversible y la de las cifras del informe -que las escribe sin exponente-, en
# un solo lugar. El texto se lee con la marca decimal de la sesion -la de
# `OutDec` y la de `LC_NUMERIC`-; las cifras se cuentan con `sprintf()`, que no
# depende de `OutDec` ni de `scipen` pero si de `LC_NUMERIC`.
.cifras_reversibles <- function(x, texto = sprintf("%.15g", x)) {
  x <- as.double(x)
  cifras <- rep(NA_integer_, length(x))
  marca <- getOption("OutDec", ".")
  if (!identical(marca, ".")) texto <- chartr(marca, ".", texto)
  texto <- .marca_decimal_c(texto)
  leido <- suppressWarnings(as.numeric(texto))
  faltan <- !is.na(x) & is.finite(x) & (is.na(leido) | leido != x)
  if (!any(faltan)) return(cifras)
  vuelve <- as.numeric(.marca_decimal_c(sprintf("%.16g", x[faltan]))) ==
    x[faltan]
  cifras[faltan] <- ifelse(vuelve, 16L, 17L)
  cifras
}

# Texto que identifica un valor para emparejarlo con otro: dos valores iguales dan
# el mismo texto aunque los guarden clases distintas, y dos distintos dan textos
# distintos. `as.character()` -y `factor()`, que lo usa para sus niveles- no cumple
# ninguna de las dos: escribe 15 cifras, asi que `0.1 + 0.2` y `0.3` daban los dos
# "0.3" y el referencial declaraba conforme un identificador que el padron no
# tenia; y elige la notacion por clase, asi que `1e5` da "1e+05" y `100000L` da
# "100000", y el mismo numero guardado como entero en una tabla y como doble en la
# otra no se encontraba.
#
# Un doble entero se escribe con todas sus cifras, como un entero o un `integer64`
# -hasta 2^63, el mayor `integer64`-; uno con fraccion, con la etiqueta mas corta
# que vuelve a el. El cero negativo es el cero.
.texto_identidad <- function(x) {
  if (inherits(x, "integer64")) return(as.character(x))
  if (!is.double(x) || inherits(x, c("Date", "POSIXt"))) {
    return(as.character(x))
  }
  salida <- .etiqueta_numero_reversible(x)
  enteros <- !is.na(x) & is.finite(x) & x == trunc(x) & abs(x) < 2^63
  # `+ 0` lleva el cero negativo al positivo: `sprintf()` escribe "-0".
  salida[enteros] <- sprintf("%.0f", x[enteros] + 0)
  salida
}

# La representacion decimal exacta de un doble, como piezas de signo, cifras y
# escala. Las cifras son enteras en texto; no se usa un doble para comparar dos
# decimales que pueden diferir despues de la lectura.
.piezas_decimal <- function(signo, digitos, escala) {
  digitos <- sub("^0+", "", digitos, perl = TRUE)
  if (!nzchar(digitos)) {
    return(list(signo = 0L, digitos = "0", escala = 0L))
  }
  escala <- as.integer(escala)
  if (escala < 0L) {
    digitos <- paste0(digitos, strrep("0", -escala))
    escala <- 0L
  }
  while (escala > 0L && substr(digitos, nchar(digitos), nchar(digitos)) == "0") {
    digitos <- substr(digitos, 1L, nchar(digitos) - 1L)
    escala <- escala - 1L
  }
  list(signo = if (signo < 0) -1L else 1L,
       digitos = digitos, escala = escala)
}

.piezas_decimal_texto <- function(texto) {
  if (length(texto) != 1L || is.na(texto)) return(NULL)
  texto <- as.character(texto)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$",
             texto, perl = TRUE)) {
    return(NULL)
  }
  signo <- if (startsWith(texto, "-")) -1L else 1L
  cuerpo <- sub("^[+-]", "", texto)
  exponente <- 0L
  if (grepl("[eE]", cuerpo, perl = TRUE)) {
    exponente_texto <- sub("^.*[eE]", "", cuerpo, perl = TRUE)
    exponente <- suppressWarnings(as.integer(exponente_texto))
    if (is.na(exponente)) return(NULL)
    cuerpo <- sub("[eE].*$", "", cuerpo, perl = TRUE)
  }
  antes <- sub("\\..*$", "", cuerpo, perl = TRUE)
  despues <- if (grepl("\\.", cuerpo, perl = TRUE)) {
    sub("^.*\\.", "", cuerpo, perl = TRUE)
  } else ""
  .piezas_decimal(signo, paste0(antes, despues),
                  nchar(despues, type = "bytes") - exponente)
}

.multiplicar_entero_decimal <- function(digitos, multiplicador) {
  cifras <- as.integer(strsplit(digitos, "", fixed = TRUE)[[1L]])
  salida <- integer(length(cifras))
  acarreo <- 0L
  for (i in rev(seq_along(cifras))) {
    producto <- cifras[[i]] * multiplicador + acarreo
    salida[[i]] <- producto %% 10L
    acarreo <- producto %/% 10L
  }
  while (acarreo > 0L) {
    salida <- c(acarreo %% 10L, salida)
    acarreo <- acarreo %/% 10L
  }
  paste0(salida, collapse = "")
}

.hexadecimal_a_decimal <- function(digitos) {
  mapa <- c(as.character(0:9), letters[1:6], LETTERS[1:6])
  salida <- "0"
  caracteres <- strsplit(digitos, "", fixed = TRUE)[[1L]]
  for (caracter in caracteres) {
    salida <- .multiplicar_entero_decimal(salida, 16L)
    valor <- match(caracter, mapa) - 1L
    if (valor >= 16L && valor <= 21L) valor <- valor - 6L
    if (valor > 0L) salida <- .sumar_entero_decimal(salida, valor)
  }
  salida
}

.sumar_entero_decimal <- function(digitos, sumando) {
  cifras <- as.integer(strsplit(digitos, "", fixed = TRUE)[[1L]])
  salida <- integer(length(cifras))
  acarreo <- as.integer(sumando)
  for (i in rev(seq_along(cifras))) {
    suma <- cifras[[i]] + acarreo
    salida[[i]] <- suma %% 10L
    acarreo <- suma %/% 10L
  }
  while (acarreo > 0L) {
    salida <- c(acarreo %% 10L, salida)
    acarreo <- acarreo %/% 10L
  }
  paste0(salida, collapse = "")
}

.piezas_decimal_doble <- function(valor) {
  if (length(valor) != 1L || is.na(valor) || !is.finite(valor)) {
    return(NULL)
  }
  if (valor == 0) return(.piezas_decimal(1L, "0", 0L))
  if (isTRUE(valor == trunc(valor)) && abs(valor) < 2^63) {
    return(.piezas_decimal(if (valor < 0) -1L else 1L,
                           sub("^-", "", .texto_identidad(valor)), 0L))
  }
  hexadecimal <- sprintf("%a", valor)
  signo <- if (startsWith(hexadecimal, "-")) -1L else 1L
  hexadecimal <- sub("^[+-]", "", hexadecimal)
  partes <- strsplit(hexadecimal, "p", fixed = TRUE)[[1L]]
  mantisa <- sub("^0x", "", partes[[1L]])
  exponente <- suppressWarnings(as.integer(partes[[2L]]))
  antes <- sub("\\..*$", "", mantisa)
  despues <- if (grepl("\\.", mantisa, perl = TRUE)) {
    sub("^.*\\.", "", mantisa, perl = TRUE)
  } else ""
  digitos <- .hexadecimal_a_decimal(paste0(antes, despues))
  desplazamiento <- exponente - 4L * nchar(despues, type = "bytes")
  if (desplazamiento >= 0L) {
    for (i in seq_len(desplazamiento)) {
      digitos <- .multiplicar_entero_decimal(digitos, 2L)
    }
    escala <- 0L
  } else {
    escala <- -desplazamiento
    for (i in seq_len(escala)) {
      digitos <- .multiplicar_entero_decimal(digitos, 5L)
    }
  }
  .piezas_decimal(signo, digitos, escala)
}

.mismos_decimales <- function(a, b) {
  !is.null(a) && !is.null(b) && identical(a$signo, b$signo) &&
    identical(a$digitos, b$digitos) && identical(a$escala, b$escala)
}

.cifras_significativas_decimal <- function(texto) {
  base <- sub("[eE].*$", "", texto, perl = TRUE)
  base <- sub("^[+-]", "", base)
  digitos <- gsub("[^0-9]", "", base, perl = TRUE)
  digitos <- sub("^0+", "", digitos, perl = TRUE)
  if (!nzchar(digitos)) 1L else nchar(digitos, type = "bytes")
}

# Identifica por la escritura de la celda, no por el doble que quedo despues de
# leerla. La primera regla compara el decimal con la fraccion exacta del doble;
# la segunda conserva los literales cortos cuya lectura de R coincide. El
# resultado es la posicion del centinela declarado, o NA si no coincide.
.identidades_sentinelas_texto <- function(texto, sentinelas_numericos,
                                          convencion = NULL,
                                          valores_leidos = NULL) {
  texto <- trimws(as.character(texto))
  salida <- rep(NA_integer_, length(texto))
  if (!length(texto) || !length(sentinelas_numericos)) return(salida)
  if (is.null(valores_leidos)) {
    valores_leidos <- .leer_numeros_texto(texto, convencion)
  }
  if (length(valores_leidos) != length(texto)) return(salida)
  candidatos <- rep(FALSE, length(texto))
  finitos <- !is.na(valores_leidos) & is.finite(valores_leidos)
  if (!any(finitos)) return(salida)
  for (centinela in sentinelas_numericos) {
    if (is.na(centinela) || !is.finite(centinela)) next
    diferencia <- abs(valores_leidos - centinela)
    tolerancia <- 4 * .Machine$double.eps * max(1, abs(centinela))
    candidatos <- candidatos | (finitos & is.finite(diferencia) &
                                  diferencia <= tolerancia)
  }
  indices <- which(candidatos)
  if (!length(indices)) return(salida)
  normalizados <- .normalizar_numeros_texto(texto, convencion)
  piezas_texto <- lapply(normalizados[indices], .piezas_decimal_texto)
  piezas_sentinelas <- lapply(sentinelas_numericos, .piezas_decimal_doble)
  for (j in seq_along(sentinelas_numericos)) {
    centinela <- sentinelas_numericos[[j]]
    if (is.na(centinela) || !is.finite(centinela)) next
    exactos <- vapply(piezas_texto, .mismos_decimales,
                      logical(1L), b = piezas_sentinelas[[j]])
    cortos <- vapply(normalizados[indices], function(valor) {
      !is.null(.piezas_decimal_texto(valor)) &&
        .cifras_significativas_decimal(valor) <= 17L
    }, logical(1L))
    leidos <- !is.na(valores_leidos[indices]) &
      valores_leidos[indices] == centinela
    cuales <- exactos | (cortos & leidos)
    nuevos <- indices[cuales & is.na(salida[indices])]
    salida[nuevos] <- as.integer(j)
  }
  salida
}

# Que nombres de columna aparecen mas de una vez, comparados por contenido como en
# el resto del paquete. Marca TODAS las apariciones, no solo las segundas: ninguna
# de las dos columnas se puede nombrar sin ambiguedad.
.nombres_repetidos <- function(nombres) {
  clave <- .nombres_para_operar(nombres)
  duplicated(clave) | duplicated(clave, fromLast = TRUE)
}

.formatear_numero_publicado <- function(x) {
  # Misma razon que en `.formatear_decimal_publicado()`: un vector devolvia
  # `NA` y quien llamaba publicaba ese `NA` como si fuera el numero.
  if (!length(x)) return(character())
  salida <- rep(NA_character_, length(x))
  utiles <- !is.na(x)
  if (any(utiles)) {
    salida[utiles] <- format(
      x[utiles], scientific = FALSE, trim = TRUE, digits = 15
    )
  }
  salida
}

# `sprintf()` usa el formateador de C y por eso ignora `OutDec`. Los números
# que viven dentro de una evidencia tienen que usar la misma marca que las
# columnas publicadas; `formatC()` conserva la precisión fija y respeta esa
# opción. `digits` es el número de decimales, no el de cifras significativas.
# Vectorizado a proposito. La guarda `length(x) != 1L` devolvia `NA` para
# CUALQUIER vector, y el llamador no se enteraba: la evidencia de Benford pasaba
# las nueve proporciones juntas y publicaba `1:NA/NA; ... 9:NA/NA` en TODA
# corrida, al lado de un chi-cuadrado con su p-valor. Un formateador que calla
# asi convierte cada llamada nueva en un defecto silencioso; ahora un vector
# devuelve un vector y solo los `NA` de la entrada salen `NA`.
.formatear_decimal_publicado <- function(x, digits = 3L, signo = FALSE) {
  if (!length(x)) return(character())
  salida <- rep(NA_character_, length(x))
  utiles <- !is.na(x)
  if (any(utiles)) {
    salida[utiles] <- formatC(
      x[utiles], format = "f", digits = as.integer(digits),
      flag = if (isTRUE(signo)) "+" else ""
    )
  }
  salida
}

# ¿La clase de `x` sabe convertirse a texto por sí misma? Si define un método
# de `as.character()`, la conversión produce los valores del dato. Si no, la
# coerción cae en `as.character.default()`.
.tiene_metodo_as_character <- function(x) {
  for (clase in class(x)) {
    metodo <- tryCatch(
      utils::getS3method("as.character", clase, optional = TRUE),
      error = function(e) NULL
    )
    if (!is.null(metodo)) return(TRUE)
  }
  FALSE
}

# Una columna se puede analizar como texto cuando aplicarle una primitiva de
# texto devuelve sus valores. Sobre una lista de objetos —una `sfc`, una columna
# de WKB crudo, una lista de vectores— `as.character.default()` no devuelve
# valores: deparsa cada elemento a código fuente de R. Ese texto no describe el
# dato, es más largo que el dato mismo y se rehace una vez por cada etapa que lo
# toca, así que el costo del perfilado queda dominado por una conversión que
# nadie pidió y que nadie puede leer.
#
# Las listas de escalares atómicos sí se coercionan a sus valores y siguen
# siendo analizables: el corte separa lo que se convierte de lo que se deparsa,
# no lo que es lista de lo que no lo es.
.analizable_como_texto <- function(x) {
  # Antes que todo: una clase que tiene metodo de `as.character()` y aborta al
  # usarlo -la `vctrs_rcrd`- no se puede analizar como texto. Ver
  # `.es_columna_sin_escritura()`.
  if (.es_columna_sin_escritura(x)) return(FALSE)
  if (!is.list(x)) return(TRUE)
  if (inherits(x, "sfc") || inherits(x, "sfg")) return(FALSE)
  if (.tiene_metodo_as_character(x)) return(TRUE)
  n <- length(x)
  if (!n) return(TRUE)
  largos <- tryCatch(lengths(x), error = function(e) NULL)
  if (is.null(largos) || length(largos) != n || anyNA(largos) ||
      !all(largos == 1L)) {
    return(FALSE)
  }
  all(vapply(x, is.atomic, logical(1L)))
}

.motivo_no_analizable_texto <- function(x) {
  if (.es_columna_sin_escritura(x)) {
    return(paste0(
      "Los valores de la columna (", .tipo_declarado(x), ") no se pueden ",
      "escribir como texto; las etapas de texto no la evaluaron."
    ))
  }
  paste0(
    "La columna es una lista de objetos (", .tipo_declarado(x),
    "); las etapas de texto no la evaluaron porque convertirla no produce sus ",
    "valores sino su representaci\u00f3n como c\u00f3digo."
  )
}

#' Preparar una columna para las etapas de texto
#'
#' Separa dos usos que no son el mismo:
#' aplicar una primitiva de texto sobre los valores, y compararlos por
#' igualdad.
#'
#' @param x Columna que se va a analizar.
#'
#' @return Lista con cinco elementos.
#'   `valores` es lo que puede recibir una primitiva de texto. Cuando la
#'   columna no es analizable como texto, son ausentes declarados: la no
#'   evaluación se declara en vez de dejar pasar la columna intacta para que
#'   cada etapa la deparse por su cuenta.
#'   `valores_identidad` es lo que puede recibir una comparación por igualdad
#'   —`unique()`, `match()`, la moda, la inferencia de tipo—. Para una columna
#'   analizable que no necesita coercion es el mismo vector que `valores`; una
#'   columna atomica que necesita coercion —por ejemplo `raw`— conserva aqui su
#'   vector original.
#'   `invalidos` y `posiciones` marcan los bytes UTF-8 inválidos aislados.
#'   `analizable` declara si la columna pasa por las etapas de texto y `motivo`
#'   dice por qué no, cuando corresponde.
#' @noRd
.texto_analizable <- function(x) {
  if (is.raw(x)) {
    return(list(
      valores = as.character(x), invalidos = rep(FALSE, length(x)),
      posiciones = integer(), analizable = TRUE, valores_identidad = x,
      motivo = NA_character_
    ))
  }
  if (!is.character(x) && !is.factor(x)) {
    if (!.analizable_como_texto(x)) {
      n <- length(x)
      return(list(
        valores = rep(NA_character_, n),
        invalidos = rep(FALSE, n),
        posiciones = integer(),
        analizable = FALSE,
        valores_identidad = x,
        motivo = .motivo_no_analizable_texto(x)
      ))
    }
    return(list(
      valores = x, invalidos = rep(FALSE, length(x)), posiciones = integer(),
      analizable = TRUE, valores_identidad = x, motivo = NA_character_
    ))
  }
  valores <- as.character(x)
  # Lo que R sabe convertir se convierte, no se descarta.
  #
  # `validUTF8()` mira los bytes, y los de un texto marcado `latin1` no son UTF-8
  # validos. Sin este paso, una columna de un CSV viejo en espanol -el caso mas
  # comun que hay en datos publicos de la region- perdia todos sus valores
  # acentuados: `CAFE`, `ANO` y `NUMERO` sobrevivian y `CAFE con tilde`, `ANO con
  # tilde` y `NUMERO con tilde` se volvian NA. El perfil informaba entonces
  # `n_distintos = 2` sobre cinco valores distintos y `n_faltantes = 0`, sin
  # declarar nada: informar como medido lo que se descarto, que es exactamente lo
  # que el paquete promete no hacer.
  #
  # `Encoding()` dice `latin1` cuando R conoce la codificacion. La conversion
  # comun conserva sus caracteres, incluidos los cinco puntos C1 que no tienen
  # correspondencia con nombre en Windows-1252. Lo que queda invalido despues
  # de eso es texto cuya codificacion nadie declaro y no se puede adivinar; eso
  # si se descarta, y se informa en `invalidos` y `posiciones`.
  # `bytes` no declara una codificacion: declara que R no debe traducir la
  # cadena. Eso es correcto para bytes arbitrarios, pero una cadena marcada asi
  # puede contener UTF-8 valido. En ese caso se puede tratar sin perdida:
  # marcarla como UTF-8 antes de que lleguen `tolower()`, `trimws()` o las
  # expresiones regulares. Los bytes invalidos siguen el camino de texto no
  # descifrable de abajo.
  #
  # Lo mismo vale para un texto con bytes UTF-8 validos que llega con marca
  # `unknown` (por ejemplo, desde `read.csv()`); bajo `LC_CTYPE=C`, `trimws()`,
  # `tolower()` y varias primitivas de R intentan traducirlo y advierten o
  # comparan distinto. La marca no cambia los bytes ni el texto publicado: solo
  # fija la forma de trabajo, igual que `.nombres_para_operar()` para los
  # identificadores. Es la declaracion compartida con la normalizacion y con las
  # acciones de texto, `.declarar_utf8_valido()`: marcar lo `unknown` antes de
  # convertir no cambia nada, porque `enc2utf8()` deja intacto lo que ya es UTF-8.
  valores <- .texto_para_operar(valores)
  invalidos <- !is.na(valores) & !validUTF8(valores)
  posiciones <- which(invalidos)
  if (length(posiciones)) valores[posiciones] <- NA_character_
  list(
    valores = valores, invalidos = invalidos, posiciones = posiciones,
    # La igualdad y la descripcion textual tienen universos distintos. La
    # primera puede contar bytes invalidos sin decodificarlos; la segunda los
    # excluye y los declara mediante `posiciones`.
    analizable = TRUE, valores_identidad = x, motivo = NA_character_
  )
}

# Un nivel `NA` de un factor -el que dejan `addNA()` o `factor(exclude = NULL)`-
# es un FALTANTE. El valor de esas celdas es `NA`: `as.character()` lo dice, la
# medicion lo lee asi (los factores se miden como texto), y tambien
# `distribucion_valores()` e `inferir_tipo()`. Pero `is.na()` sobre el factor
# dice FALSE, porque el codigo apunta a un nivel, y el perfil y las
# dependencias cuentan presencia con `is.na()`. Medido: un factor
# con 13 de 40 celdas en el nivel `NA` se leia de cinco maneras en un mismo
# analisis: el perfil 0 faltantes -y entonces no se proponia `NoNulo` y esas
# celdas no entraban a ninguna completitud- y `n_distintos = NA`;
# `distribucion_valores()`, `Formato` e `inferir_tipo()` sobre 27; la dependencia
# sobre 39 pares "presentes".
#
# La respuesta es una: es un faltante, porque una celda cuyo valor es `NA` no
# informa nada, y es la lectura que ya hace la medicion de la calidad. Se aplica
# en la ENTRADA -el codigo del nivel pasa a `NA` y el nivel se retira- y no en
# cada `is.na()`: lo que viene despues ve un factor comun con faltantes. La copia
# es interna; los datos del usuario no se tocan.
.factor_sin_nivel_na <- function(x) {
  if (!is.factor(x)) return(x)
  niveles <- levels(x)
  nivel_na <- which(is.na(niveles))
  if (!length(nivel_na)) return(x)
  atributos <- attributes(x)
  conservados <- niveles[-nivel_na]
  codigos <- match(niveles, conservados)[as.integer(x)]
  atributos$levels <- conservados
  attributes(codigos) <- atributos
  codigos
}

# La entrada de una tabla a los diagnosticos que leen presencia y valores: dos
# formas de guardar el mismo dato quedan en una, sobre la copia interna.
#
#   - El nivel `NA` de un factor pasa a ausente (arriba).
#   - Un `POSIXlt` -lo que deja `strptime()`- pasa a `POSIXct`, que es el mismo
#     instante en la misma zona. Por dentro es una lista, y el perfil lo leia
#     distinto que a la misma columna `POSIXct`: medido, `moda` y
#     `frecuencia_moda` en `NA` sin declararlo (la `POSIXct` publica el instante
#     y 10), `n_filas_fecha_civil_distinta_utc = 0` donde la `POSIXct` en
#     `America/Montevideo` dice 30 -y su hallazgo de zona horaria no salia-, y
#     las dependencias la declaraban "lista" y no la buscaban. La misma columna
#     en las dos clases da ahora la misma respuesta. Las relaciones y la
#     identidad de filas del plan ya iban por el instante.
.normalizar_columnas_entrada <- function(tabla) {
  if (!inherits(tabla, "data.frame") || !ncol(tabla)) return(tabla)
  .exigir_integer64_confiable(tabla)
  for (i in seq_along(tabla)) {
    columna <- tabla[[i]]
    if ((is.factor(columna) && anyNA(levels(columna))) ||
        inherits(columna, "POSIXlt")) {
      tabla[[i]] <- .normalizar_columna_entrada(columna)
    }
  }
  tabla
}

# La misma regla para una sola columna, para quien compara columnas sueltas
# -la igualdad de contenido de `aplicar()`, `distribucion_valores()`- y no
# recibe la tabla entera.
.normalizar_columna_entrada <- function(x) {
  .exigir_integer64_confiable(x)
  if (is.factor(x) && anyNA(levels(x))) return(.factor_sin_nivel_na(x))
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x))
  x
}

# Copia operativa de una tabla: los factores se interpretan como texto para
# que los métodos no dependan de la versión de R ni de stringsAsFactors.
# El perfil conserva la columna original; esta función sólo se usa al ejecutar
# contratos que leen valores.
.normalizar_columnas_texto <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(tabla)
  tabla <- .tabla_base(tabla)
  .exigir_integer64_confiable(tabla)
  # `medir()` tambien recibe tablas directamente, sin pasar por `perfilar()`.
  # La misma frontera debe declarar los bytes UTF-8 validos antes de que un
  # metodo de contrato los compare o los entregue a una funcion del usuario.
  tabla <- .marcar_utf8_tabla(tabla)
  factores <- vapply(tabla, is.factor, logical(1L))
  if (!any(factores)) return(tabla)
  salida <- tabla
  for (nombre in names(salida)[factores]) {
    salida[[nombre]] <- .texto_analizable(salida[[nombre]])$valores
  }
  salida
}

.columnas_identicas <- function(x, y) {
  # Dos columnas son el mismo contenido con la regla de la entrada del perfil:
  # el nivel `NA` de un factor es un faltante y un `POSIXlt` es su instante.
  # El perfil compara la tabla ya normalizada, y `aplicar()` comparaba las
  # columnas crudas: `addNA(f)` contra `f` -o un `POSIXlt` contra su
  # `POSIXct`- salian "duplicadas" en el perfil y en el plan, y `aplicar()` las
  # registraba `fallida` con "dejaron de tener contenido duplicado" sobre la
  # tabla original. La comparacion es una sola, aca.
  .columnas_normalizadas_identicas(
    .normalizar_columna_igualdad(x), .normalizar_columna_igualdad(y)
  )
}

# Lo que la igualdad de columnas mira es lo que el perfil lee como contenido.
# A la regla de la entrada -el nivel `NA` es un faltante, un `POSIXlt` es su
# instante- se suma, solo para comparar columnas entre si, el orden de los
# niveles de un factor sin orden: el perfil lo lee como texto -mismo tipo
# declarado, mismos valores, misma moda- y en ninguna salida publica ese orden,
# asi que `factor(x, levels = c("a", "b"))` y `factor(x, levels = c("b", "a"))`
# son la misma columna. Con dos columnas asi el perfil no las agrupaba, y
# `comparar_perfiles()` publicaba `resuelto` un grupo cuyos valores seguian
# iguales. El tipo si cuenta: un entero y un doble, o un logico y un entero,
# tienen tipos declarados distintos en el perfil. Y en un factor ordenado el
# orden de los niveles es el orden de los valores. No se toca la entrada de la
# tabla: los demas diagnosticos ven el factor como llego.
.normalizar_columna_igualdad <- function(x) {
  x <- .normalizar_columna_entrada(x)
  # El texto, con la lectura de las acciones: una columna `latin1` y su gemela
  # UTF-8 son la misma columna. Ver `.leer_latin1_sin_windows_1252()`.
  x <- .leer_latin1_sin_windows_1252(x)
  if (is.factor(x) && !is.ordered(x)) x <- .factor_niveles_por_bytes(x)
  x
}

# El mismo factor con los niveles en orden de bytes, que no depende del locale.
# Cambian los codigos, no las etiquetas de ninguna fila.
.factor_niveles_por_bytes <- function(x) {
  niveles <- levels(x)
  orden <- .orden_por_bytes(niveles)
  if (identical(orden, seq_along(niveles))) return(x)
  atributos <- attributes(x)
  codigos <- match(as.integer(x), orden)
  atributos$levels <- niveles[orden]
  attributes(codigos) <- atributos
  codigos
}

# La comparacion sobre columnas ya normalizadas, para quien normaliza una vez y
# compara muchas (`.pares_de_columnas_identicas()`).
.columnas_normalizadas_identicas <- function(x, y) {
  # `identical()` iguala todos los NaN, y un `integer64` negativo ES un NaN en
  # sus bits: `-1` y `-2` salian identicos. Comparar los bits es exacto para
  # `integer64` -los bits son el valor-.
  if (inherits(x, "integer64") && inherits(y, "integer64")) {
    return(identical(class(x), class(y)) &&
      identical(x, y, num.eq = FALSE, single.NA = FALSE))
  }
  identical(class(x), class(y)) &&
    identical(is.na(x), is.na(y)) &&
    identical(x, y)
}

.nombre_paquete <- function() {
  nombre <- utils::packageName()
  if (is.null(nombre)) "lupa" else nombre
}

.version_paquete <- function() {
  nombre <- .nombre_paquete()
  tryCatch(
    as.character(utils::packageVersion(nombre)),
    error = function(e) "desarrollo"
  )
}

# Una etiqueta que junta nombres de columna -`fecha + descripcion`-. Un solo
# nombre se publica tal como llego, con sus bytes y su marca (`?perfilar`): asi
# sigue indexando la tabla. Varios se pegan con `.pegar_texto()`.
.pegar_nombres <- function(x, sep = " + ") {
  x <- as.character(x)
  if (length(x) == 1L && !is.na(x)) return(x)
  .pegar_texto(x, collapse = sep)
}

# `paste()` que no depende del locale. `paste()` de base traduce al
# nativo lo marcado `latin1` cuando nada del vector esta marcado UTF-8, y bajo
# `C` lo nativo es ASCII: el nombre `descripcion` con tilde en latin1 salia como
# el texto `descripci<f3>n`, que no indexa la tabla, y una columna llamada como
# una persona salia `[valor protegido]` porque la etiqueta ya no era el nombre.
# Con algo marcado UTF-8 al lado, un `unknown` con bytes altos se escapaba igual.
# Aca lo `latin1` se convierte -sobre `latin1` la conversion no mira el locale-
# y se pega por bytes; el resultado se declara UTF-8 si lo es, y si no queda
# con sus bytes, sin marca, en los dos locales.
.pegar_texto <- function(..., sep = " ", collapse = NULL) {
  textos <- lapply(list(...), as.character)
  # Lo declarado `bytes` sigue declarado en lo que se arma con el, como en
  # `paste()` de base: es lo que hace que la consola lo muestre `a\\xc3\\xb1o`
  # (test-N87).
  declarados <- lapply(textos, function(x) !is.na(x) & Encoding(x) == "bytes")
  piezas <- lapply(textos, function(x) {
    x <- .latin1_a_utf8(x)
    Encoding(x) <- "bytes"
    x
  })
  salida <- do.call(paste, c(piezas, list(sep = sep, collapse = collapse)))
  con_bytes <- rep(FALSE, length(salida))
  if (any(unlist(declarados, use.names = FALSE))) {
    marcas <- do.call(paste, c(
      lapply(declarados, function(b) ifelse(b, "1", "")),
      list(sep = "", collapse = if (is.null(collapse)) NULL else "")
    ))
    con_bytes <- nzchar(marcas)
  }
  if (any(!con_bytes)) {
    libres <- salida[!con_bytes]
    Encoding(libres) <- "unknown"
    salida[!con_bytes] <- .declarar_utf8_valido(libres)
  }
  salida
}

# `paste0()` del paquete sobre texto del usuario: los `objeto_medible` de
# `medir()` -`tabla$columna[fila]`- y las evidencias `columna=valor` de
# duplicados. Con `paste0()` de base, bajo `C` un nombre latin1 salia
# `df$descripci<f3>n[1]`.
.pegar_texto0 <- function(..., collapse = NULL) {
  .pegar_texto(..., sep = "", collapse = collapse)
}

# Parte cada texto por un separador ASCII y recorta los blancos de cada pieza,
# por bytes, y devuelve a las piezas la marca del texto de origen. Es la regla
# de los nombres de columna: lo que no es UTF-8 valido se
# corta por bytes, porque `strsplit()` y `trimws()` sin `useBytes` avisan o
# abortan sobre el segun el locale -bajo un locale UTF-8 "input string N is
# invalid in this locale", que con `warn = 2` tumbaba `perfilar()`; bajo `C`,
# nada-. Cortar por un separador ASCII es exacto en UTF-8 y en latin1, pero el
# corte por bytes pierde la marca, y sin ella un nombre `latin1` deja de ser el
# mismo nombre.
.partir_conservando_marca <- function(x, separador) {
  x <- as.character(x)
  marcas <- Encoding(x)
  piezas <- strsplit(x, separador, fixed = TRUE, useBytes = TRUE)
  lapply(seq_along(piezas), function(i) {
    p <- .recortar_bytes(piezas[[i]])
    if (length(p) && marcas[[i]] != "unknown") Encoding(p) <- marcas[[i]]
    p
  })
}

# Una corrida larga y callada no se distingue de una colgada, pero una barra en
# la salida de un guion es ruido que despues hay que filtrar. La decision es la
# misma en todos los caminos, asi que vive en un solo lugar: `interactive()`
# decide y `options(lupa.progreso = )` manda sobre eso en los dos sentidos.
.progreso_activo <- function(total, minimo) {
  if (!isTRUE(getOption("lupa.progreso", interactive()))) return(FALSE)
  # Sin total conocido no hay porcentaje que mostrar, y un porcentaje contra un
  # total inventado es peor que ninguno. Debajo del minimo la corrida termina
  # antes de que la barra sirva.
  is.numeric(total) && length(total) == 1L && !is.na(total) &&
    is.finite(total) && total >= minimo
}

# Cada puerta comprobaba un subconjunto distinto de lo que un objeto necesita, y
# ninguna estaba completa. Medido quitando un componente por vez:
# `comparar_perfiles()` rechaza un perfil sin `columnas`, `meta` o `hallazgos` y
# ACEPTA uno sin `general`; `print.perfil()` no mira nada y sin embargo necesita
# `general`, asi que fallaba con "attempt to set an attribute on NULL". No hay
# una definicion unica de objeto valido aplicada en las dos.
#
# Esto la da, y las dos la usan. El mensaje nombra la funcion que lo produce,
# que es lo que el usuario necesita para saber que hacer.
.validar_objeto_lupa <- function(x, clase, componentes, origen) {
  if (!inherits(x, clase)) {
    .detener("`x` debe ser un objeto de clase `", clase, "`, producido por ",
         origen, ".", call. = FALSE)
  }
  if (!length(componentes)) return(invisible(x))
  if (!is.list(x)) {
    .detener("`x` no tiene la estructura de un objeto `", clase,
         "`: falta ", paste(componentes, collapse = ", "), ".", call. = FALSE)
  }
  faltan <- componentes[!componentes %in% names(x)]
  if (length(faltan)) {
    .detener("`x` no tiene la estructura de un objeto `", clase, "`: falta ",
         paste(faltan, collapse = ", "), ". Producirlo con ", origen, ".",
         call. = FALSE)
  }
  invisible(x)
}

# Una medicion vacia que SI viene de `medir()` no es una entrada invalida: es una
# corrida donde ninguna metrica fue aplicable, y `medir()` ya declaro por que en
# su `cobertura_metricas`. Tres validaciones distintas la rechazaban con "debe
# ser ... producido por medir()", que afirma algo falso teniendo el motivo
# verdadero a mano. El texto se arma en UN solo lugar para que las tres digan lo
# mismo: tenerlo escrito tres veces es como se desincronizan.
.error_medicion_sin_medidas <- function(medicion, argumento, origen) {
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  motivo <- if (is.data.frame(cobertura) && nrow(cobertura) &&
                "motivo" %in% names(cobertura)) {
    paste0(" Motivo declarado por `medir()`: ",
           paste(unique(as.character(cobertura$motivo)), collapse = "; "))
  } else {
    ""
  }
  .detener(
    "`", argumento, "` viene de ", origen,
    " pero no tiene ninguna medida que evaluar.", motivo,
    " Se puede revisar `attr(", argumento, ", \"cobertura_metricas\")`.",
    call. = FALSE
  )
}

# Declara la codificacion de las columnas de texto que no la declaran. NO cambia
# los bytes: dice lo que ya son.
#
# Por que hace falta en la ENTRADA y no en cada sitio: bajo un locale que no es
# UTF-8, una cadena con `Encoding()` en `unknown` se interpreta segun el locale,
# y ahi cualquier operacion que exija UTF-8 aborta. Se encontraron DOS caminos
# distintos que morian asi -`chartr()` en el pliegue de mayusculas y un
# `gsub(..., perl = TRUE)` con `(*UTF)` en la proteccion de grafemas-, y buscar
# el tercero de a uno es perseguir la cola. Marcar una vez, al entrar, los cubre
# todos.
#
# `read.csv()` deja `unknown` en cualquier CSV con tildes: es el caso mas comun
# que existe en espanol, no un borde.
#
# Solo se marca lo que YA es UTF-8 valido. Un texto realmente latin1 tiene bytes
# que no lo son, `validUTF8()` da `FALSE` y se lo deja como esta: el paquete lo
# trata despues por el camino de `texto_no_descifrable`, que existe para eso.
.marcar_utf8_textos <- function(textos) {
  # Sin esta guarda, una tabla con `names()` en `NULL` moria en `Encoding(NULL)`
  # con "a character vector argument expected", un error de R que no dice nada
  # del dato. El paquete ya tiene su propio mensaje para ese caso -"`names` debe
  # ser un vector de caracteres"- y este marcado, que corre antes que la
  # validacion, se lo estaba comiendo.
  if (!is.character(textos) || !length(textos)) return(textos)
  sin_marca <- !is.na(textos) & Encoding(textos) == "unknown"
  if (!any(sin_marca)) return(textos)
  # `validUTF8()` suele bastar, pero la frontera DBI debe validar como UTF-8 sin
  # pedirle al locale que interprete una cadena `unknown`. `iconv()` con origen
  # y destino UTF-8 hace justamente esa comprobacion y conserva los bytes.
  convertidos <- suppressWarnings(
    tryCatch(
      iconv(textos, from = "UTF-8", to = "UTF-8", sub = NA),
      error = function(e) rep(NA_character_, length(textos))
    )
  )
  marcables <- sin_marca & !is.na(convertidos)
  if (!any(marcables)) return(textos)
  trozo <- textos[marcables]
  Encoding(trozo) <- "UTF-8"
  textos[marcables] <- trozo
  textos
}

# `identical()` no sirve para decidir si el marcado cambio algo: su respuesta
# DEPENDE DEL LOCALE. Con los mismos bytes, una cadena `unknown` y otra `UTF-8`
# son identicas bajo un locale UTF-8 -ahi lo nativo ya es UTF-8- y distintas bajo
# `C`. Medido en las dos: `TRUE` bajo `es_UY.UTF-8`, `FALSE` bajo `LC_ALL=C`.
#
# O sea que la condicion acertaba justo donde hacia falta y era un no-op donde no,
# por casualidad. Dejar una guarda cuya respuesta depende del locale, dentro del
# marcado que existe para que el resultado NO dependa del locale, es contradecir
# el motivo de la funcion. Se compara la marca, que es lo que el marcado cambia.
.marca_cambio <- function(despues, antes) {
  # `Encoding()` exige un vector de caracteres. Lo que no lo es no se marco, asi
  # que no cambio: contestar `FALSE` deja pasar el dato intacto y deja que lo
  # rechace la validacion del paquete, con su mensaje, y no esta funcion.
  if (!is.character(despues) || !is.character(antes)) return(FALSE)
  !identical(Encoding(despues), Encoding(antes))
}

.marcar_utf8_tabla <- function(tabla) {
  if (!inherits(tabla, "data.frame") || !ncol(tabla)) return(tabla)
  # `names()` son datos publicados del usuario, no texto que el paquete pueda
  # declarar de nuevo. Las comparaciones y los ordenamientos que los necesitan
  # usan `.nombres_para_operar()`; conservarlos aqui deja bytes y marca iguales
  # a la entrada por construccion, y hace que un nombre publicado siga
  # indexando la tabla que lo produjo incluso bajo `LC_CTYPE = "C"`.
  for (i in seq_along(tabla)) {
    columna <- tabla[[i]]
    if (!length(columna)) next
    # Los NIVELES de un factor son cadenas igual, y sin esto quedaban afuera: la
    # misma columna como `character` no emitia un aviso y como `factor` emitia
    # veintiuno bajo un locale que no es UTF-8. Recorrer solo `is.character()`
    # dejaba la mitad del problema sin tocar.
    if (is.factor(columna)) {
      niveles <- levels(columna)
      if (!length(niveles)) next
      marcados <- .marcar_utf8_textos(niveles)
      if (.marca_cambio(marcados, niveles)) {
        levels(tabla[[i]]) <- marcados
      }
      next
    }
    if (!is.character(columna)) next
    marcados <- .marcar_utf8_textos(columna)
    if (.marca_cambio(marcados, columna)) tabla[[i]] <- marcados
  }
  tabla
}

# Para cada FORMA analizada, el valor original que la representa al publicarla.
#
# Publicar y analizar son dos usos distintos del mismo dato. Se analiza la
# forma interpretada -comparar texto exige que `tolower()` vea caracteres y no
# octetos- y se publica el original, porque `Encoding(x) == "bytes"` declara
# que el contenido no debe interpretarse.
#
# La salvedad decide la correccion: una forma puede juntar SECUENCIAS DE BYTES
# DISTINTAS -`ni<f1>o` en latin1 y `ni<c3><b1>o` en bytes se interpretan
# igual- y se cuentan juntas, que es lo correcto porque el conteo es de
# caracteres. Pero entonces la etiqueta no puede ser la de UN original:
# publicar `ni\xc3\xb1o (3)` afirmaria que esa secuencia aparece tres veces
# cuando aparece una, y cual se publicaba dependeria de cual llegara primero.
# Cuando la forma no es homogenea se publica la forma interpretada, que es la
# unidad en la que se conto. La homogeneidad se decide con `.clave_bytes()`,
# que es el instrumento con el que el paquete ya decide identidad.
#
# `formas` son las formas analizadas distintas; `textos` y `publicables` son
# dos vistas del MISMO vector de valores, de igual largo.
.publicables_por_forma <- function(formas, textos, publicables) {
  if (!length(formas)) return(formas)
  indice <- match(textos, formas)
  representante <- match(seq_along(formas), indice)
  salida <- publicables[representante]
  claves <- .clave_bytes(publicables)
  clave_representante <- claves[representante][indice]
  mezcladas <- !is.na(indice) & !is.na(claves) &
    !is.na(clave_representante) & claves != clave_representante
  if (any(mezcladas)) {
    formas_mezcladas <- unique(indice[mezcladas])
    salida[formas_mezcladas] <- formas[formas_mezcladas]
  }
  # Una forma sin representante no puede quedar vacia: se informa la forma
  # interpretada, que es fea pero no miente.
  sin_representante <- is.na(salida) & !is.na(formas)
  if (any(sin_representante)) salida[sin_representante] <- formas[sin_representante]
  salida
}

# Declara que unos conteos salieron de la muestra y no de la columna entera.
# Patrones y formatos de fecha se descubren sobre una muestra -lo documenta
# `perfilar()`-, pero sus hallazgos publicaban `n_afectados = 2` y
# `%d/%m/%Y (50)` como cifras de la tabla cuando la tabla tenia 10 y 5.000, sin
# decirlo en la evidencia ni en la consola. `tipo_declarado_distinto` ya lo
# decia con esta forma; ahora lo dicen todos los que cuentan sobre la muestra.
.nota_conteo_muestral <- function(objeto) {
  if (!isTRUE(attr(objeto, "muestreado", exact = TRUE))) return("")
  analizados <- attr(objeto, "analizados", exact = TRUE)
  total <- attr(objeto, "total", exact = TRUE)
  if (!length(analizados) || !length(total)) {
    return("; medido sobre una muestra, no sobre la columna entera: los conteos son de la muestra")
  }
  paste0(
    "; medido sobre una muestra de ", .miles_trabajo(analizados), " de ",
    .miles_trabajo(total), if (total == 1L) " valor, no sobre la columna entera: los " else
      " valores, no sobre la columna entera: los ",
    "conteos son de la muestra"
  )
}

# Texto para la PLANTILLA de una llamada a cli. cli lee su primer argumento como
# plantilla de glue: todo `{...}` adentro es una expresion que se EVALUA. El
# paquete armaba ese argumento con `paste()` e incluia texto del usuario
# -nombres de columna, el `nombre` de la tabla, valores de ejemplo, nombres de
# tablas de una coleccion-, asi que `perfilar(d, nombre = "tabla{1+1}")` se
# imprimia `tabla2`, y una columna llamada `tasa{Sys.getenv("USER")}` se
# imprimia con el nombre del usuario: el encabezado de un CSV ejecutaba codigo
# al imprimir el plan. Y un nombre declarado `bytes` abortaba la impresion,
# porque glue lo traduce.
#
# Se rinde para exhibir y se duplican las llaves, que es como cli escribe una
# llave literal. Ninguna plantilla compuesta del paquete lleva marcado propio;
# el marcado (`{.code x}`) va en plantillas literales con el valor como
# variable, y esas no pasan por aca: glue no vuelve a leer lo que sustituye.
.cli_literal <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  nombres <- names(x)
  # Primero se rinde lo que no es texto -declarado `bytes`, o sin marca y no
  # UTF-8- como lo muestra la consola: `.marcar_para_exhibir()` deja la marca
  # `bytes` a proposito, para `print.data.frame()`, y glue aborta al traducirla.
  x <- .marcar_para_exhibir(
    .publicar_sin_marca_ilegible(.texto_publicable(as.character(x)))
  )
  x <- gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
  names(x) <- nombres
  x
}

# La vista publicable de un vector: el original donde esta declarado `bytes`,
# la forma analizada en todo lo demas. `latin1` NO entra: ahi R conoce la
# codificacion y `.texto_analizable()` la convierte sin perder nada, asi que la
# forma analizada ES la correcta para publicarla.
.vista_publicable <- function(valores_identidad, textos) {
  publicables <- suppressWarnings(as.character(valores_identidad))
  if (length(publicables) != length(textos)) return(textos)
  desde_original <- !is.na(publicables) & Encoding(publicables) == "bytes"
  publicables[!desde_original] <- textos[!desde_original]
  publicables
}

# `encodeString()` sin consultar el locale. La de base escapa como
# `\uXXXX` todo lo que el locale no imprime, y bajo `C` eso es todo lo que no
# es ASCII: la evidencia citaba `Jos\u00e9` en un `Rscript` de cron y la e con
# tilde en RStudio -33 bytes contra 29-, el plan y el reporte heredaban la
# diferencia, y el ejemplo dejaba de casar con el de la tabla de patrones.
#
# Escribe lo que `encodeString()` escribe en una sesion UTF-8: el texto valido
# tal cual; la barra y la comilla escapadas; los controles ASCII como `\n`,
# `\t` o en octal; los C1 y los separadores de linea y de parrafo Unicode
# como `\uXXXX`; y cada byte que no forma UTF-8 valido como `\xNN`,
# conservando las letras validas de al lado. Lo `latin1` se convierte primero, y
# lo declarado `bytes` sale como lo escribe base, que en eso no mira el locale.
.comillas_sin_locale <- function(x, quote = "") {
  x <- as.character(x)
  if (!length(x)) return(character())
  vapply(x, .comillas_sin_locale_uno, character(1L), quote = quote,
         USE.NAMES = FALSE)
}

.comillas_sin_locale_uno <- function(texto, quote) {
  if (is.na(texto)) return("NA")
  if (Encoding(texto) == "bytes") return(encodeString(texto, quote = quote))
  texto <- .latin1_a_utf8(texto)
  bytes <- as.integer(charToRaw(texto))
  comilla <- if (nzchar(quote)) utf8ToInt(quote) else -1L
  partes <- character()
  i <- 1L
  n <- length(bytes)
  while (i <= n) {
    b <- bytes[[i]]
    largo <- if (b < 0x80L) 1L else if (b >= 0xC2L && b <= 0xDFL) 2L else
      if (b >= 0xE0L && b <= 0xEFL) 3L else if (b >= 0xF0L && b <= 0xF4L) 4L else 0L
    codigo <- NA_integer_
    if (largo >= 1L && i + largo - 1L <= n) {
      trozo <- as.raw(bytes[i:(i + largo - 1L)])
      if (largo == 1L || validUTF8(rawToChar(trozo))) {
        codigo <- if (largo == 1L) b else utf8ToInt(rawToChar(trozo))
      }
    }
    if (is.na(codigo)) {
      partes <- c(partes, sprintf("\\x%02x", b))
      i <- i + 1L
      next
    }
    partes <- c(partes, .caracter_citado(codigo, comilla))
    i <- i + largo
  }
  salida <- paste0(quote, paste0(partes, collapse = ""), quote)
  Encoding(salida) <- "unknown"
  .declarar_utf8_valido(salida)
}

.caracter_citado <- function(codigo, comilla) {
  if (codigo == 0x5CL) return("\\\\")
  if (codigo == comilla) return(paste0("\\", intToUtf8(codigo)))
  if (codigo < 0x20L || codigo == 0x7FL) {
    nombrados <- c(`7` = "a", `8` = "b", `9` = "t", `10` = "n", `11` = "v",
                   `12` = "f", `13` = "r")
    letra <- nombrados[as.character(codigo)]
    if (!is.na(letra)) return(paste0("\\", letra))
    return(sprintf("\\%03o", codigo))
  }
  if ((codigo >= 0x80L && codigo <= 0x9FL) || codigo %in% c(0x2028L, 0x2029L)) {
    return(sprintf("\\u%04x", codigo))
  }
  intToUtf8(codigo)
}

# Cita un valor para publicarlo entre comillas sin volver a escapar lo que ya
# se rindio. `encodeString()` escapa la barra del propio `\xNN`, asi que un
# valor declarado `bytes` salia `ca\\xc3\\xb1` en la evidencia mientras la
# tabla de patrones -que sigue el precedente de rendirlo con
# `.texto_publicable()`- publicaba `ca\xc3\xb1`. Quien quisiera casar el valor
# de un hallazgo con el de la tabla no podia: las dos cadenas difieren.
.citar_publicable <- function(x) {
  if (!length(x)) return(character())
  salida <- rep(NA_character_, length(x))
  presentes <- which(!is.na(x))
  for (i in presentes) {
    texto <- x[[i]]
    if (Encoding(texto) == "bytes") {
      # Declarado: se rinde a su forma imprimible, que es la misma que publican
      # `moda` y la tabla de patrones.
      salida[[i]] <- paste0('"', .texto_publicable(texto), '"')
      next
    }
    # `encodeString()` NO escapa un NBSP ni un espacio fino: los considera
    # imprimibles y los deja crudos. Citar con el solo alcanzaba para las
    # comillas y las barras, y devolvia `"x y"` con el NBSP intacto, o sea que
    # la evidencia declaraba haber escapado algo que no habia escapado. Para
    # eso esta el renderizador del paquete, que es el mismo que usa el resto de
    # la evidencia.
    if (.hay_escapes_al_publicar(texto)) {
      visible <- .escapar_texto_visible(texto)
      # El renderizador no escapa la comilla, y aca el valor va entre comillas.
      salida[[i]] <- paste0('"', gsub('"', '\\"', visible, fixed = TRUE), '"')
      next
    }
    salida[[i]] <- .comillas_sin_locale(texto, quote = '"')
  }
  salida
}

# Los puntos de codigo de un texto, o NULL si no se puede decodificar.
#
# `utf8ToInt()` devuelve `NA` -no vacio, no error- sobre lo que no decodifica,
# y ese `NA` no se queda quieto: en el render llegaba a un `if` y ABORTABA, y en
# la remediacion `paste0(intToUtf8(NA), collapse = "")` producia la cadena
# literal `"NA"`, que REEMPLAZABA el valor del usuario. Tres valores `latin1`
# de una columna se volvian `"NA"` al aplicar una accion que el propio paquete
# marca como recomendada.
#
# Rendir antes de decodificar cierra las dos. `latin1` se convierte sin perder
# nada -R conoce la codificacion- y una cadena marcada `bytes` que ES UTF-8
# valido se puede tratar sin perdida. Lo que queda no se puede describir como
# caracteres, y para eso esta el `NULL`: quien llama decide, y lo unico que no
# puede decidir es inventar un valor.
.codigos_decodificables <- function(texto) {
  if (is.na(texto)) return(NULL)
  if (Encoding(texto) == "latin1") {
    texto <- .latin1_a_utf8(texto)
  } else if (Encoding(texto) == "bytes" && validUTF8(texto)) {
    Encoding(texto) <- "UTF-8"
  }
  if (validUTF8(texto)) {
    if (Encoding(texto) == "bytes") Encoding(texto) <- "UTF-8"
  } else {
    return(NULL)
  }
  codigos <- tryCatch(utf8ToInt(texto), error = function(e) NULL)
  if (is.null(codigos) || anyNA(codigos)) return(NULL)
  codigos
}

# TRUE si publicar estos valores exige escaparlos.
#
# La evidencia de un hallazgo y las tablas del perfil usan dos convenciones
# distintas, y las dos son legitimas: la tabla muestra el valor, y la evidencia
# lo ESCAPA para que el defecto se vea. Un `controles_invisibles` que publicara
# el NBSP crudo mandaria a verificar a una superficie donde ese caracter se ve
# como un espacio comun: el defecto que el hallazgo denuncia seria invisible
# justo donde el lector va a buscarlo.
#
# Lo que el paquete no puede hacer es callar cual uso. Esto decide cuando
# decirlo: sobre texto corriente no se agrega ruido, y sobre texto que si se
# escapo se declara, para que nadie busque en sus datos un `<U+00A0>` literal.
.hay_escapes_al_publicar <- function(x) {
  if (!length(x)) return(FALSE)
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  any(vapply(x, function(texto) {
    if (Encoding(texto) == "bytes") return(TRUE)
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(TRUE)
    if (any(codigos %in% c(9L, 10L, 11L, 12L, 13L))) return(TRUE)
    any(.codigos_control_invisible(codigos))
  }, logical(1L), USE.NAMES = FALSE))
}

# La nota que declara la convencion, vacia cuando no hubo nada que escapar. El
# texto vive en una constante porque `detectar_discordancias()` escapa por su
# cuenta -por punto de codigo, solo en la fila que lo necesita- y tiene que
# decirlo con las mismas palabras.
.NOTA_ESCAPES <- " [valores escapados para que se vean; en los datos est\u00e1n sin escapar]"

.nota_escapes <- function(x) {
  if (.hay_escapes_al_publicar(x)) .NOTA_ESCAPES else ""
}

# Aplica una transformacion de texto sin que la marca de un valor decida el
# destino de sus vecinos.
#
# Una operacion vectorizada sobre un vector que contiene UN valor marcado
# `bytes` cambia de modo para todo el vector: `trimws()` y `gsub(perl = TRUE)`
# devuelven marcadas `bytes` tambien a las celdas `latin1` de al lado -y ahi su
# contenido deja de ser legible-, y `tolower()`/`toupper()` ABORTAN. Pasaba en
# tres ejecutores distintos; en el primero se arreglo con una copia propia de
# esta logica y en los otros dos quedo el defecto, porque una regla escrita en
# un solo sitio no protege a los demas.
#
# `en_texto` se aplica a lo que no esta declarado `bytes`, que asi viaja solo.
# `en_bytes` se aplica a lo declarado `bytes`, y solo sirve para operaciones
# que no necesitan leer el texto -recortar o reemplazar caracteres ASCII-. Si
# es `NULL`, lo declarado se deja intacto: no se puede pasar a minusculas lo
# que no se puede leer.
.aplicar_por_marca <- function(x, en_texto, en_bytes = NULL) {
  if (!is.character(x) || !length(x)) return(en_texto(x))
  marca <- Encoding(x)
  presentes <- !is.na(x)
  declarados <- presentes & marca == "bytes"
  # Lo ilegible no es solo lo declarado `bytes`. `read.csv()` sin
  # `fileEncoding` sobre un archivo latin1 -el caso mas comun del mundo real-
  # deja los bytes en cadenas SIN MARCA, y en una sesion UTF-8 eso tampoco es
  # texto valido: `trimws()` moria con "input string 3 is invalid UTF-8" y la
  # accion que el plan recomienda y aplica sola quedaba `fallida`.
  aparte <- declarados | .sin_marca_ilegible(x)
  # Lo que va a `en_texto` se DECLARA antes: un `unknown` cuyos bytes son UTF-8
  # valido es UTF-8 (`.declarar_utf8_valido()`). Sin eso, bajo `LC_CTYPE=C`
  # `trimws()` y `gsub(perl = TRUE)` lo leian como bytes de la sesion, y sobre
  # una columna con marcas mezcladas avisaban "unable to translate". Lo que la
  # transformacion deja igual vuelve como llego: declarar es marcar, y una celda
  # intacta no tiene por que cambiar de marca. La igualdad se mide en bytes, sin
  # traducir nada.
  como_texto <- function(v) {
    declarado <- .texto_para_operar(v, marcas = "unknown")
    nuevo <- en_texto(declarado)
    a <- nuevo
    b <- declarado
    Encoding(a) <- "bytes"
    Encoding(b) <- "bytes"
    intactas <- !is.na(v) & !is.na(nuevo) & a == b
    nuevo[intactas] <- v[intactas]
    nuevo
  }
  if (!any(aparte)) return(como_texto(x))
  salida <- x
  if (any(!aparte)) salida[!aparte] <- como_texto(x[!aparte])
  # Por bytes se transforma solo lo que el perfil midio: lo declarado `bytes`
  # cuyos bytes son UTF-8 valido. Lo que no decodifica queda intacto, como ya
  # lo deja `eliminar_controles_invisibles`: el perfil no lo midio -lo informa
  # como `codificacion_invalida`, con el remedio de releer la fuente- y
  # recortarlo igual hacia que el plan estimara 1 y el registro contara 3.
  por_bytes <- declarados & validUTF8(x)
  if (!is.null(en_bytes) && any(por_bytes)) {
    transformados <- en_bytes(x[por_bytes])
    # La marca se vuelve a poner en vez de confiar en que la operacion la
    # conserve. En R 4.1 no la conserva: medido, sobre
    # una celda `bytes` cuyos bytes son UTF-8 valido, `trimws()` y
    # `gsub(fixed = TRUE, useBytes = TRUE)` devuelven la cadena recortada como
    # `unknown`, mientras en R 4.6 sale `bytes`. Una celda que el paquete
    # declara no interpretada pasaba a ser texto sin marca, que es justo lo que
    # `.aplicar_por_marca()` existe para impedir, y solo en la version que el
    # `DESCRIPTION` declara soportar.
    Encoding(transformados) <- "bytes"
    salida[por_bytes] <- transformados
  }
  salida
}

# La unidad declarada de una columna, en UNA sola forma y sin perder nada.
#
# Un objeto `units` guarda la unidad en un `symbolic_units`, que es una lista con
# `numerator` y `denominator`, y el numerador es un VECTOR. Leerlo con `[[1L]]`
# tenia dos consecuencias medidas sobre la API publica:
#
#   - `km/h` publicaba `km`: no una unidad ausente, una unidad EQUIVOCADA, con la
#     misma forma que la correcta. Una velocidad se leia como una distancia.
#   - `m^2` guarda `c("m", "m")` y `kg*m/s^2` guarda `c("kg", "m")`: con largo 2
#     la fila de esa columna pasaba a tener dos filas y `clasificar_variables()`
#     moria con "replacement has 2 rows, data has 3".
#
# `as.character()` sobre el `symbolic_units` devuelve siempre un solo valor y en
# forma canonica -`km/h`, `m^2`, `kg*m/s^2`, `1/s`-. `format()` queda descartado
# porque devuelve `m, m`, que no es una unidad, y `deparse_unit()` pierde
# legibilidad (`m2`).
#
# Ningun fixture del paquete usaba un objeto `units` de verdad -todos declaraban
# `attr(x, "units") <- "kg"`, una cadena de largo 1, donde `[[1L]]` acierta-, asi
# que las dos puertas eran invisibles para la suite entera.
.unidad_declarada <- function(x) {
  valor <- attr(x, "units", exact = TRUE)
  if (is.null(valor)) return(NA_character_)
  texto <- tryCatch(as.character(valor), error = function(e) NA_character_)
  texto <- texto[!is.na(texto) & nzchar(texto)]
  if (!length(texto)) return(NA_character_)
  texto <- unique(texto)
  # Mas de una unidad declarada no es una unidad: se publican todas, porque
  # quedarse con la primera es exactamente el defecto que esto cierra.
  if (length(texto) == 1L) texto else paste(texto, collapse = ", ")
}

# La unidad en la que esta expresado el resumen cuantitativo de una columna.
#
# Contesta UNA pregunta: en que unidad estan las cifras que la fila publica. Para
# `units` y `difftime` es la unidad declarada; para `Period`, `Date` y `POSIXt` es
# el segundo, que es la unidad en la que hoy sale `desvio` y que hasta ahora solo
# estaba dicha en prosa. Donde no hay ninguna, queda `NA`: el campo no inventa.
#
# Sin este campo, `set_units(c(1,2,3), "m")` y `"km"` publicaban filas IDENTICAS
# -`media = 2`, `desvio = 1`- y un `Period` de dos dias publicaba `172800` sin
# decir que eran segundos.
.unidad_del_resumen <- function(x) {
  if (inherits(x, c("units", "difftime"))) return(.unidad_declarada(x))
  if (inherits(x, c("Period", "Duration", "Date", "POSIXt"))) {
    return("segundos")
  }
  .unidad_declarada(x)
}

# `data.frame()` cobra un precio fijo por columna aunque la columna ya traiga
# nombre: cada argumento pasa por `as.data.frame.character()` o
# `as.data.frame.vector()`, que hacen `deparse1(substitute(x))` para inventarle
# un nombre que despues se descarta. La fila de cada columna del perfil tiene
# unas 150 columnas y se arma una vez por columna perfilada: medido sobre la
# tabla del README, ese deparse y lo que lo rodea eran un tercio de
# `perfilar()`, y en Windows el costo fijo se multiplica.
#
# Esta version arma la tabla directamente cuando todas las columnas tienen
# nombre y cada una es un vector atomico sin nombres ni dimensiones, sin
# atributos o con la clase exacta de un `factor`, `Date` o `POSIXct` -las que
# `data.frame()` guarda tal cual y sabe reciclar-, de largo comun o de largo 1.
# Para ese caso hace lo mismo que `data.frame()`: corrige los nombres con
# `make.names(unique = TRUE)`, recicla con `rep(length.out = )` y deja nombres
# de fila automaticos; una prueba lo compara con `identical()`. Cualquier otra
# cosa -una columna sin nombre, una lista, un vector con nombres o con otros
# atributos, una clase propia, largos que no casan, `stringsAsFactors = TRUE`-
# va a `data.frame()`, que la resuelve o la rechaza con su propio mensaje.
.data_frame_simple <- function(..., stringsAsFactors = FALSE,
                               check.names = TRUE) {
  columnas <- list(...)
  n_filas <- .filas_data_frame_simple(columnas)
  if (is.na(n_filas) || !identical(stringsAsFactors, FALSE) ||
      !(isTRUE(check.names) || isFALSE(check.names))) {
    return(data.frame(..., stringsAsFactors = stringsAsFactors,
                      check.names = check.names))
  }
  cortas <- lengths(columnas) != n_filas
  if (any(cortas)) {
    columnas[cortas] <- lapply(columnas[cortas], rep, length.out = n_filas)
  }
  if (check.names) {
    # `make.names(unique = TRUE)` ordena para desambiguar aunque no haya nada
    # repetido, y ese orden era casi la mitad de lo que costaba esta funcion.
    # Sin repetidos el resultado es el de `make.names()` a secas.
    nombres <- make.names(names(columnas))
    if (anyDuplicated(nombres)) {
      nombres <- make.names(names(columnas), unique = TRUE)
    }
    names(columnas) <- nombres
  }
  list2DF(columnas, nrow = n_filas)
}

.CLASES_DATA_FRAME_SIMPLE <- list(
  "factor", c("ordered", "factor"), "Date", c("POSIXct", "POSIXt")
)

.columna_con_clase_simple <- function(v) {
  atributos <- attributes(v)
  clase <- atributos[["class"]]
  !is.null(clase) && is.null(atributos[["names"]]) &&
    is.null(atributos[["dim"]]) && is.null(atributos[["dimnames"]]) &&
    any(vapply(.CLASES_DATA_FRAME_SIMPLE, identical, logical(1L), clase))
}

# El largo de la tabla si `.data_frame_simple()` puede armarla sin
# `data.frame()`, o `NA` si no.
.filas_data_frame_simple <- function(columnas) {
  nombres <- names(columnas)
  if (!length(columnas) || is.null(nombres) || anyNA(nombres) ||
      !all(nzchar(nombres))) {
    return(NA_integer_)
  }
  # Con primitivas primero: casi todas las columnas son vectores sin atributos,
  # y una funcion por columna costaba mas que lo que se ahorraba.
  if (!all(vapply(columnas, is.atomic, logical(1L), USE.NAMES = FALSE)) ||
      any(vapply(columnas, is.null, logical(1L), USE.NAMES = FALSE))) {
    return(NA_integer_)
  }
  con_atributos <- !vapply(lapply(columnas, attributes), is.null, logical(1L),
                           USE.NAMES = FALSE)
  if (any(con_atributos) &&
      !all(vapply(columnas[con_atributos], .columna_con_clase_simple,
                  logical(1L), USE.NAMES = FALSE))) {
    return(NA_integer_)
  }
  largos <- lengths(columnas, use.names = FALSE)
  n_filas <- max(largos)
  if (all(largos == n_filas) || (n_filas > 0L && all(largos %in% c(1L, n_filas)))) {
    n_filas
  } else {
    NA_integer_
  }
}

# Lo mismo que `do.call(rbind, filas)` seguido de `rownames(x) <- NULL`.
#
# `rbind.data.frame()` paga por pieza y por columna la reconciliacion de
# factores, matrices y nombres de fila. La tabla de columnas del perfil apila
# una fila de unas 150 columnas por columna perfilada, y ese apilado era el
# 3 % de `perfilar()` sobre la tabla del README, creciendo con el ancho.
#
# Cuando todas las piezas son `data.frame` sin otros atributos, con al menos una
# fila, los mismos nombres de columna y columnas atomicas sin atributos, cada
# columna del resultado es la concatenacion de sus piezas. `rbind()` llega al
# mismo vector asignando pieza por pieza (`value[[j]][filas] <- pieza`), y eso
# coerciona en cadena: por eso se exige que el tipo de cada columna sea el mismo
# en todas las piezas, o que todas sean logicas, enteras o reales -donde
# coercionar en cadena o de una vez da los mismos valores; con texto no:
# `TRUE` pasa a `1L` y despues a `"1"`, y de una vez a `"TRUE"`-. Los nombres de
# fila no se reproducen: se descartan, como lo hace quien la llama. Cualquier
# otra forma va a `rbind()`.
.apilar_filas <- function(filas) {
  columnas <- .columnas_apiladas(filas)
  if (is.null(columnas)) {
    resultado <- do.call(rbind, filas)
    rownames(resultado) <- NULL
    return(resultado)
  }
  list2DF(columnas, nrow = sum(vapply(filas, .row_names_info, integer(1L), 2L)))
}

.columnas_apiladas <- function(filas) {
  if (!length(filas) || !is.list(filas)) return(NULL)
  primera <- filas[[1L]]
  if (!is.data.frame(primera)) return(NULL)
  nombres <- names(primera)
  if (!length(nombres)) return(NULL)
  tipos <- vector("list", length(filas))
  for (i in seq_along(filas)) {
    pieza <- filas[[i]]
    if (!identical(oldClass(pieza), "data.frame") ||
        !identical(names(pieza), nombres) ||
        !setequal(names(attributes(pieza)), c("names", "class", "row.names")) ||
        .row_names_info(pieza, 2L) < 1L) {
      return(NULL)
    }
    # Sin la clase: `lengths()` sobre un data frame pasa por `[[.data.frame`
    # columna por columna, y eso era casi todo el costo de esta funcion.
    contenido <- unclass(pieza)
    if (any(lengths(contenido, use.names = FALSE) != .row_names_info(pieza, 2L)) ||
        !all(vapply(lapply(contenido, attributes), is.null, logical(1L),
                    USE.NAMES = FALSE))) {
      return(NULL)
    }
    tipos[[i]] <- vapply(contenido, typeof, character(1L), USE.NAMES = FALSE)
  }
  tipos <- matrix(unlist(tipos, use.names = FALSE), nrow = length(nombres))
  if (!all(tipos %in% c("logical", "integer", "double", "character"))) {
    return(NULL)
  }
  uniformes <- rowSums(tipos == tipos[, 1L]) == ncol(tipos)
  numericas <- rowSums(tipos != "character") == ncol(tipos)
  if (!all(uniformes | numericas)) return(NULL)
  columnas <- lapply(seq_along(nombres), function(j) {
    unlist(lapply(filas, .subset2, j), use.names = FALSE)
  })
  names(columnas) <- nombres
  columnas
}
