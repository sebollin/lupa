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
  !any(grepl("[^\\001-\\177]", x, useBytes = TRUE, perl = TRUE),
       na.rm = TRUE)
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
  # En ASCII no hay marca ni conversion que pueda cambiar la representacion.
  # La sonda es vectorizada y se paga una sola vez; no se entra a `validUTF8()`
  # ni a `iconv()` para el caso que domina los perfiles.
  if (!is.null(crudo) && .es_ascii(crudo)) return(crudo)
  if (is.null(crudo)) crudo <- tryCatch(as.character(x), error = function(e) NULL)
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
  declarada <- !is.na(crudo) & Encoding(crudo) == "latin1"
  if (any(declarada)) crudo[declarada] <- enc2utf8(crudo[declarada])
  validos <- !is.na(crudo) & validUTF8(crudo)
  validos[is.na(validos)] <- FALSE
  clave <- crudo
  if (any(validos)) {
    trozo <- crudo[validos]
    Encoding(trozo) <- "UTF-8"
    clave[validos] <- trozo
  }
  invalidos <- !validos
  if (any(invalidos)) {
    clave[invalidos] <- .escapar_bytes_altos(crudo[invalidos])
  }
  clave
}

# Declara UTF-8 el texto que YA es UTF-8, en todo el objeto y sin tocar un byte.
#
# Existe por una propiedad de la serializacion de R, no por una del paquete. El
# formato RDS version 3 -el que usa `saveRDS()` por omision- guarda en la
# cabecera la codificacion NATIVA de quien escribio, y al leer traduce desde
# ella las cadenas SIN MARCA. Medido, escribiendo bajo `LC_CTYPE=C`:
#
#   cabecera del archivo: "ANSI_X3.4-1968"
#   al leer bajo UTF-8  : input string 'B\u00e1sico' cannot be translated
#                         from 'ANSI_X3.4-1968' to UTF-8, but is valid UTF-8
#
# En glibc esa traduccion FALLA, R nota que los bytes ya son UTF-8 validos y los
# deja intactos: el dato se salva por el camino del error. En Windows
# `win_iconv` desde esa codificacion NO falla -apaga el bit alto y devuelve
# algo-, asi que el rescate nunca se dispara y `B\u00e1sico` se guarda como
# `BC!sico`: `c3`->`43`, `a1`->`21`. Texto plausible, silenciosamente distinto.
#
# Consecuencia medida en R-hub Windows: `comparar_perfiles()` publicaba una
# deriva de configuracion inexistente, y `acumular_historico()` rechazaba su
# propia corrida guardada por un contenido que nadie habia cambiado.
#
# Una cadena MARCADA `UTF-8` no se traduce: R la guarda declarada y la lee
# igual. Marcar no cambia un byte -declara lo que ya hay-, asi que el dato del
# usuario sigue siendo el suyo.
#
# POR QUE AL SELLAR Y NO AL ENTRAR. Bajo `C`, marcar NO es inocuo para comparar:
#
#   marcada == sin marca -> FALSE      match(sin marca, marcada) -> NA
#   unique(c(marcada, sin marca)) -> 2 elementos
#
# Marcar en la entrada dejaria los nombres del objeto marcados y los del usuario
# sin marcar, y toda busqueda por nombre fallaria bajo `C`, en silencio. Por eso
# el trabajo interno no se toca y se marca al final, cuando el objeto ya esta
# armado: adentro se compara sin marca contra sin marca, y lo que sale -lo que
# se serializa- va declarado.
#
# Lo que NO se toca, a proposito:
#   - `bytes`: es la declaracion de que eso no se interprete como texto;
#   - `latin1` y `UTF-8`: ya declaran, y RDS los respeta;
#   - bytes que no son UTF-8 valido: declararlos UTF-8 seria mentir. Siguen
#     expuestos a la traduccion, y no hay forma honesta de evitarlo sin saber
#     en que codificacion estan.
.marcar_texto_estable <- function(x) {
  if (is.character(x)) {
    if (!length(x)) return(x)
    # El caso que domina -todo ASCII- se resuelve con una sola comprobacion
    # vectorizada y no entra a `validUTF8()`.
    if (.es_ascii(x)) return(x)
    candidata <- !is.na(x) & Encoding(x) == "unknown"
    if (any(candidata)) {
      validas <- candidata
      validas[candidata] <- validUTF8(x[candidata])
      validas[is.na(validas)] <- FALSE
      if (any(validas)) {
        trozo <- x[validas]
        Encoding(trozo) <- "UTF-8"
        x[validas] <- trozo
      }
    }
    return(x)
  }
  if (is.list(x)) {
    # `is.list()` cubre el `data.frame`, que es donde vive casi todo el texto.
    #
    # Los `NULL` se saltean y no por prolijidad: `x[[i]] <- NULL` BORRA el
    # elemento, la lista se encoge, y el indice siguiente se sale de rango. Un
    # perfil trae campos nulos -`clave`, `dependencias` cuando no se pidieron-,
    # asi que el recorrido abortaba con "subindice fuera de los limites".
    for (i in seq_along(x)) {
      if (is.null(x[[i]])) next
      x[[i]] <- .marcar_texto_estable(x[[i]])
    }
  }
  atributos <- attributes(x)
  if (!is.null(atributos)) {
    # `names` incluido: el nombre de una columna es texto del usuario y viaja
    # en el mismo RDS. `class` y `row.names` no se tocan.
    for (a in setdiff(names(atributos), c("class", "row.names"))) {
      marcado <- .marcar_texto_estable(atributos[[a]])
      attr(x, a) <- marcado
    }
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
  if (is.function(x)) return("<funcion>")
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
    stop("x debe ser un data.frame.", call. = FALSE)
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
      if (is.null(segunda)) stop(condicion)
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
  no_ascii <- grepl("[^\\001-\\177]", nombres, useBytes = TRUE, perl = TRUE)
  reservado <- rep(FALSE, length(nombres))
  indices_ascii <- which(!is.na(nombres) & !no_ascii)
  if (length(indices_ascii)) {
    reservado[indices_ascii] <- grepl(
      "^<lupa-byte:[0-9A-F]+>$", nombres[indices_ascii], perl = TRUE
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
    salida[validos] <- claves[validos]
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
  originales[order(.clave_bytes(originales), method = "radix")]
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
    stop("`sep` debe ser una cadena de longitud uno.", call. = FALSE)
  }
  unicos <- originales
  usados <- new.env(hash = TRUE, parent = emptyenv())
  siguientes <- new.env(hash = TRUE, parent = emptyenv())
  clave_hash <- function(clave) {
    if (is.na(clave)) return(NA_character_)
    paste0("k", paste(as.integer(charToRaw(clave)), collapse = "_"))
  }
  ya_usada <- function(clave) {
    llave <- clave_hash(clave)
    !is.na(llave) && exists(llave, envir = usados, inherits = FALSE)
  }
  registrar <- function(clave) {
    llave <- clave_hash(clave)
    if (!is.na(llave)) assign(llave, TRUE, envir = usados)
  }
  for (i in seq_along(originales)) {
    candidato <- originales[[i]]
    clave <- .nombres_para_operar(candidato)
    repetido <- ya_usada(clave)
    if (repetido) {
      base <- candidato
      llave_base <- clave_hash(clave)
      sufijo <- if (exists(llave_base, envir = siguientes, inherits = FALSE)) {
        get(llave_base, envir = siguientes, inherits = FALSE)
      } else 1L
      repeat {
        candidato <- paste0(base, sep, sufijo)
        clave <- .nombres_para_operar(candidato)
        if (!ya_usada(clave)) break
        sufijo <- sufijo + 1L
      }
      assign(llave_base, sufijo + 1L, envir = siguientes)
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

.ordenar_por_bytes <- function(x) {
  if (!length(x)) return(x)
  x[order(.clave_bytes(x), method = "radix")]
}

# `.columnas_duplicadas()` y `.pares_redundantes()` eran el mismo bloque escrito
# dos veces en archivos distintos, con una sola diferencia: que columnas se
# comparan -todas, o un subconjunto de claves-. Corregir un detalle obligaba a
# acordarse de hacerlo en los dos lados. Los dos nombres se conservan porque
# dicen cosas distintas en su contexto, pero ahora son envoltorios de esto.
.pares_de_columnas_identicas <- function(datos, indices, nombres) {
  vacio <- data.frame(
    columna_1 = character(), columna_2 = character(), stringsAsFactors = FALSE
  )
  if (length(indices) < 2L) return(vacio)
  pares <- utils::combn(indices, 2L)
  iguales <- apply(pares, 2L, function(indice) {
    .columnas_identicas(datos[[indice[[1L]]]], datos[[indice[[2L]]]])
  })
  pares <- pares[, iguales, drop = FALSE]
  if (!ncol(pares)) return(vacio)
  data.frame(
    columna_1 = nombres[pares[1L, ]],
    columna_2 = nombres[pares[2L, ]],
    stringsAsFactors = FALSE
  )
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
    stop("`", nombre, "` debe heredar de data.frame.", call. = FALSE)
  }
  invisible(datos)
}

# Copia las subclases con semantica propia de `[` a un data.frame base antes
# de ejecutar codigo que selecciona columnas o agrega columnas.
.tabla_base <- function(tabla) {
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
    stop("`datos` debe heredar de data.frame.", call. = FALSE)
  }
  if (is.character(columnas) && !is.null(names(datos)) &&
      !anyNA(columnas)) {
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
    stop("`perfil` debe corresponder a las columnas de `datos`.", call. = FALSE)
  }
  invisible(perfil)
}

.validar_muestra <- function(muestra) {
  if (!is.numeric(muestra) || length(muestra) != 1L || is.na(muestra) ||
      muestra < 1) {
    stop("`muestra` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  if (is.finite(muestra) && muestra != floor(muestra)) {
    stop(
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

.tipo_declarado <- function(x) {
  if (inherits(x, "sfc")) {
    return(class(x)[[1L]])
  }
  if (.es_columna_compuesta(x)) {
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
  sin_moda <- !is.na(frecuencia) & !is.na(distintos) &
    frecuencia == 1 & distintos > 1
  # Una columna protegida queda como esta. El marcador `[valor protegido]` no
  # afirma una moda, asi que no hay nada que corregir; y cambiarlo por NA SI
  # diria algo del dato -- que esa columna personal no tiene ningun valor
  # repetido -- justo lo que la proteccion existe para no decir. Se comprueba
  # por el campo y tambien por el marcador, para que el resultado no dependa de
  # si la proteccion ya corrio o corre despues.
  if ("dato_personal_protegido" %in% names(columnas)) {
    protegida <- !is.na(columnas[["dato_personal_protegido"]]) &
      as.logical(columnas[["dato_personal_protegido"]])
    sin_moda <- sin_moda & !protegida
  }
  marcada <- !is.na(columnas[["moda"]]) &
    as.character(columnas[["moda"]]) == "[valor protegido]"
  sin_moda <- sin_moda & !marcada
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
  texto <- tryCatch(deparse(expresion), error = function(e) character())
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
.formatear_numero_publicado <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  format(x, scientific = FALSE, trim = TRUE, digits = 15)
}

# `sprintf()` usa el formateador de C y por eso ignora `OutDec`. Los números
# que viven dentro de una evidencia tienen que usar la misma marca que las
# columnas publicadas; `formatC()` conserva la precisión fija y respeta esa
# opción. `digits` es el número de decimales, no el de cifras significativas.
.formatear_decimal_publicado <- function(x, digits = 3L, signo = FALSE) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  formatC(
    x, format = "f", digits = as.integer(digits),
    flag = if (isTRUE(signo)) "+" else ""
  )
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
  paste0(
    "La columna es una lista de objetos (", .tipo_declarado(x),
    "); las etapas de texto no la evaluaron porque convertirla no produce sus ",
    "valores sino su representaci\u00f3n como c\u00f3digo."
  )
}

#' Preparar una columna para las etapas de texto
#'
#' Separa dos usos que hasta ahora viajaban juntos y que no son el mismo:
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
  # `Encoding()` dice `latin1` cuando R conoce la codificacion, y ahi `enc2utf8()`
  # convierte sin perder nada. Lo que queda invalido despues de eso es texto cuya
  # codificacion nadie declaro y no se puede adivinar; eso si se descarta, y se
  # informa en `invalidos` y `posiciones`.
  # `bytes` no declara una codificacion: declara que R no debe traducir la
  # cadena. Eso es correcto para bytes arbitrarios, pero una cadena marcada asi
  # puede contener UTF-8 valido. En ese caso se puede tratar sin perdida:
  # marcarla como UTF-8 antes de que lleguen `tolower()`, `trimws()` o las
  # expresiones regulares. Los bytes invalidos siguen el camino de texto no
  # descifrable de abajo.
  bytes_validos <- !is.na(valores) & Encoding(valores) == "bytes" &
    validUTF8(valores)
  if (any(bytes_validos)) {
    marcados <- valores[bytes_validos]
    Encoding(marcados) <- "UTF-8"
    valores[bytes_validos] <- marcados
  }
  declarados <- Encoding(valores) %in% c("latin1", "UTF-8")
  if (any(declarados)) {
    convertidos <- suppressWarnings(
      tryCatch(enc2utf8(valores[declarados]), error = function(e) NULL)
    )
    if (!is.null(convertidos) && length(convertidos) == sum(declarados)) {
      valores[declarados] <- convertidos
    }
  }
  invalidos <- !is.na(valores) & !validUTF8(valores)
  posiciones <- which(invalidos)
  if (length(posiciones)) valores[posiciones] <- NA_character_
  # Un texto con bytes UTF-8 validos puede llegar con marca `unknown` (por
  # ejemplo, desde `read.csv()`); bajo `LC_CTYPE=C`, `trimws()`, `tolower()` y
  # varias primitivas de R intentan traducirlo y advierten o comparan distinto.
  # La marca no cambia los bytes ni el texto publicado: sólo fija la forma de
  # trabajo, igual que `.nombres_para_operar()` para los identificadores.
  validos <- !is.na(valores) & validUTF8(valores)
  if (any(validos)) {
    marcados <- valores[validos]
    Encoding(marcados) <- "UTF-8"
    valores[validos] <- marcados
  }
  list(
    valores = valores, invalidos = invalidos, posiciones = posiciones,
    # La igualdad y la descripcion textual tienen universos distintos. La
    # primera puede contar bytes invalidos sin decodificarlos; la segunda los
    # excluye y los declara mediante `posiciones`.
    analizable = TRUE, valores_identidad = x, motivo = NA_character_
  )
}

# Copia operativa de una tabla: los factores se interpretan como texto para
# que los métodos no dependan de la versión de R ni de stringsAsFactors.
# El perfil conserva la columna original; esta función sólo se usa al ejecutar
# contratos que leen valores.
.normalizar_columnas_texto <- function(tabla) {
  if (!inherits(tabla, "data.frame")) return(tabla)
  tabla <- .tabla_base(tabla)
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

.pegar_nombres <- function(x) {
  paste(x, collapse = " + ")
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
    stop("`x` debe ser un objeto de clase `", clase, "`, producido por ",
         origen, ".", call. = FALSE)
  }
  if (!length(componentes)) return(invisible(x))
  if (!is.list(x)) {
    stop("`x` no tiene la estructura de un objeto `", clase,
         "`: falta ", paste(componentes, collapse = ", "), ".", call. = FALSE)
  }
  faltan <- componentes[!componentes %in% names(x)]
  if (length(faltan)) {
    stop("`x` no tiene la estructura de un objeto `", clase, "`: falta ",
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
  stop(
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
