# Señales redundantes: varias columnas que codifican el mismo hecho y se
# contradicen dentro de la misma fila.
#
# Es un defecto que **ninguna columna por separado muestra**: el año de la
# fecha, el año fiscal y el año del archivo pueden ser los tres valores
# perfectamente plausibles y aun así uno contradecir a los otros.
#
# El grupo lo declara quien conoce los datos. `lupa` no adivina qué columnas
# codifican el mismo hecho: dos columnas de año pueden ser el año de nacimiento
# y el año de ingreso, y no tienen por qué coincidir. Suponerlo sería inventar
# conocimiento del dominio, que es justo lo que este paquete no hace.

.validar_transformacion_senal <- function(transformacion, columnas) {
  if (is.null(transformacion)) return(NULL)
  if (!is.list(transformacion) || is.null(names(transformacion)) ||
      anyNA(names(transformacion)) || !all(nzchar(names(transformacion)))) {
    stop("`transformacion` debe ser una lista con nombres.", call. = FALSE)
  }
  indices <- .indice_nombre(names(transformacion), columnas)
  sobrantes <- names(transformacion)[is.na(indices)]
  if (length(sobrantes)) {
    stop(
      "`transformacion` nombra columnas que no estan en la senal: ",
      paste(sobrantes, collapse = ", "), ".", call. = FALSE
    )
  }
  if (!all(vapply(transformacion, is.function, logical(1L)))) {
    stop("Cada elemento de `transformacion` debe ser una funcion.",
         call. = FALSE)
  }
  names(transformacion) <- columnas[indices]
  transformacion
}

#' Declarar una señal redundante entre columnas
#'
#' Declara que varias columnas de la misma tabla codifican **el mismo hecho**,
#' de modo que [detectar_discordancias()] pueda informar las filas donde no
#' concuerdan. El grupo lo declara quien conoce los datos: `lupa` no lo adivina,
#' porque dos columnas de año pueden ser el año de nacimiento y el año de
#' ingreso y no tienen por qué coincidir.
#'
#' `transformacion` permite comparar columnas que guardan el hecho de formas
#' distintas —por ejemplo extraer el año de una fecha para compararlo con una
#' columna de año—. Cada función recibe la columna entera y devuelve un vector
#' de la misma longitud.
#'
#' `ventana` es la tolerancia, **en las unidades del valor transformado**: con
#' años, `ventana = 1` acepta un año de diferencia. La unidad no se adivina; es
#' la del resultado de la transformación.
#'
#' La ventana **sólo gobierna una comparación numérica**. Si alguna de las
#' columnas comparadas no es numérica —fechas, texto, factores—, la comparación es
#' de igualdad exacta sobre el texto y la ventana no se aplica: eso queda dicho en
#' la evidencia (`comparacion textual exacta`) y, si se declaró una ventana mayor
#' que cero, se avisa al medir. Para comparar fechas con tolerancia hay que
#' llevarlas con `transformacion` a un número en la unidad de la ventana.
#'
#' Esa igualdad exacta es **de valores, no de su impresión**. Una fecha-hora se
#' compara por su instante: el mismo instante guardado en dos zonas horarias
#' concuerda. Se escribe en UTC, al segundo y con su fracción exacta si la tiene
#' —la misma escritura que usa [detectar_relaciones()]—, así que comparada con
#' una columna de texto, el texto tiene que estar en UTC; para otra zona, se la
#' lleva con `transformacion`. Un número se compara con todas sus cifras —`1e5`
#' y el texto `"100000"` concuerdan; `0.1 + 0.2` y `0.3` no—. Si todas las
#' columnas son duraciones (`difftime`), se comparan en segundos: una hora y
#' sesenta minutos concuerdan. Un texto se compara por sus bytes, así que una
#' palabra con la tilde compuesta y la misma con la tilde descompuesta son dos
#' textos distintos.
#'
#' @param columnas Nombres de al menos dos columnas que codifican el mismo
#'   hecho.
#' @param ventana Tolerancia máxima admitida entre los valores, en las unidades
#'   del valor comparado. Por omisión `0`: coincidencia exacta. Sólo se aplica si
#'   todas las columnas comparadas son numéricas; con fechas o texto la
#'   comparación es de igualdad exacta y se avisa.
#' @param transformacion Lista opcional con nombres de columna y funciones que
#'   las llevan a una escala comparable.
#' @param nombre Etiqueta de la señal. Por omisión, las columnas unidas.
#'
#' @return Objeto de clase `senal_redundante`: una lista con `nombre` -el
#'   declarado, o las columnas unidas por `=`-, el vector `columnas` que la
#'   señal relaciona, la `ventana` numérica y la `transformacion` validada.
#'   Describe la relación esperada; no la evalúa.
#' @export
#' @seealso [detectar_discordancias()], [detectar_dependencias()]
#'
#' @examples
#' senal_redundante(c("anio_fiscal", "anio_archivo"))
#' senal_redundante(
#'   c("fecha", "anio_fiscal"),
#'   transformacion = list(fecha = function(x) as.integer(format(x, "%Y")))
#' )
senal_redundante <- function(columnas, ventana = 0, transformacion = NULL,
                             nombre = NULL) {
  if (!is.character(columnas) || length(columnas) < 2L ||
      anyNA(columnas) || !all(nzchar(columnas))) {
    stop("`columnas` debe nombrar al menos dos columnas.", call. = FALSE)
  }
  if (anyDuplicated(.nombres_para_operar(columnas))) {
    stop("`columnas` repite una columna.", call. = FALSE)
  }
  if (!is.numeric(ventana) || length(ventana) != 1L || is.na(ventana) ||
      !is.finite(ventana) || ventana < 0) {
    stop("`ventana` debe ser un numero finito no negativo.", call. = FALSE)
  }
  transformacion <- .validar_transformacion_senal(transformacion, columnas)
  if (!is.null(nombre) && !.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser NULL o una cadena no vacia.", call. = FALSE)
  }
  estructura <- list(
    nombre = if (is.null(nombre)) paste(columnas, collapse = " = ") else nombre,
    columnas = columnas,
    ventana = as.numeric(ventana),
    transformacion = transformacion
  )
  class(estructura) <- "senal_redundante"
  estructura
}

#' @export
print.senal_redundante <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_text("Se\u00f1al redundante: {.strong {x$nombre}}")
  cli::cli_text("Columnas: {paste(x$columnas, collapse = ', ')}")
  cli::cli_text("Ventana: {x$ventana}")
  if (length(x$transformacion)) {
    cli::cli_text(
      "Transformadas: {paste(names(x$transformacion), collapse = ', ')}"
    )
  }
  invisible(original)
}

.valores_senal <- function(datos, senal) {
  indices <- .indice_nombre(senal$columnas, names(datos))
  lapply(seq_along(indices), function(i) {
    columna <- names(datos)[[indices[[i]]]]
    valores <- datos[[indices[[i]]]]
    indice_transformacion <- .indice_nombre(
      columna, names(senal$transformacion)
    )
    funcion <- if (is.na(indice_transformacion)) {
      NULL
    } else {
      senal$transformacion[[indice_transformacion]]
    }
    if (!is.null(funcion)) valores <- funcion(valores)
    if (length(valores) != nrow(datos)) {
      stop(
        "La transformacion de `", columna, "` devolvio ", length(valores),
        " valores para ", nrow(datos), " filas.", call. = FALSE
      )
    }
    valores
  })
}

.discordancia_vacia <- function() {
  data.frame(
    senal = character(), columnas = character(), n_filas = numeric(),
    n_evaluadas = numeric(), n_discordantes = numeric(),
    proporcion = numeric(), ventana = numeric(), evidencia = character(),
    stringsAsFactors = FALSE
  )
}

# La identidad de cada valor en la comparacion NO numerica de una senal. Se
# comparan valores, no su impresion: `as.character()` escribe cada fecha-hora en
# la zona de SU columna, asi que el mismo instante guardado en UTC y en
# `America/Montevideo` salia discordante en todas las filas -medido en la ronda
# 27: 3 de 3, con 0 a mano-; y escribe un doble con 15 cifras y `1e5` como
# "1e+05", asi que dos numeros distintos salian iguales y el mismo numero
# guardado como doble y como texto, distinto. Es la propiedad que
# `.valores_relacion()` ya resolvio para las relaciones entre tablas, y se usa
# su misma escritura:
#
#   - una fecha-hora es su instante: UTC, al segundo y con la fraccion exacta si
#     la tiene. Comparada con un texto, el texto tiene que estar en UTC; para
#     otra zona, `transformacion`;
#   - un numero, su texto de identidad: todas las cifras si es entero, la forma
#     mas corta que vuelve al valor si tiene fraccion;
#   - una duracion, su numero en la unidad de su columna, salvo que TODAS las
#     columnas sean duraciones: entonces se comparan en segundos, porque una
#     hora y sesenta minutos son la misma duracion en dos unidades;
#   - un texto, sus bytes con la codificacion declarada resuelta: la igualdad
#     textual exacta que documenta la ayuda. Las formas NFC y NFD de una palabra
#     siguen siendo dos textos, y eso es decision, no defecto.
.claves_senal <- function(valores) {
  duraciones <- all(vapply(valores, inherits, logical(1L), "difftime"))
  lapply(valores, function(x) {
    if (inherits(x, "difftime")) {
      return(.texto_identidad(
        if (duraciones) as.numeric(x, units = "secs") else as.numeric(x)
      ))
    }
    if (inherits(x, c("POSIXt", "Date"))) return(.valores_relacion(x))
    if (is.numeric(x)) return(.texto_identidad(x))
    if (is.character(x) || is.factor(x)) return(.valores_relacion(x))
    as.character(x)
  })
}

# La escritura de un valor que lo distingue de los que el veredicto separo. Solo
# se usa en una fila citada donde la escritura de `.texto_valor()` -una
# fecha-hora al segundo, un doble con 15 cifras, un texto tal cual- junta dos
# valores distintos.
.texto_que_distingue_senal <- function(x) {
  if (inherits(x, "POSIXt")) return(paste(.valores_relacion(x), "UTC"))
  if (inherits(x, "difftime")) {
    return(paste(.etiqueta_numero_reversible(as.numeric(x)), units(x)))
  }
  if (inherits(x, c("Date", "integer64"))) return(.texto_valor(x))
  if (is.double(x)) return(.etiqueta_numero_reversible(x))
  if (is.character(x) || is.factor(x)) {
    return(.texto_por_puntos_de_codigo(as.character(x)))
  }
  .texto_valor(x)
}

# Un texto con cada caracter fuera del ASCII imprimible escrito `<U+XXXX>`, la
# convencion de `.escapar_texto_visible()`. Dos textos con los mismos caracteres
# visibles y distintos puntos de codigo -una tilde compuesta y una descompuesta,
# un espacio duro- quedan escritos distinto. Lo que no decodifica como UTF-8, o
# se declaro `bytes`, se deja como lo publica `.texto_valor()`: esas formas ya
# llevan sus escapes.
.texto_por_puntos_de_codigo <- function(texto) {
  if (length(texto) != 1L || is.na(texto)) return(.texto_valor(texto))
  if (Encoding(texto) == "bytes") return(.texto_valor(texto))
  if (Encoding(texto) == "latin1") texto <- enc2utf8(texto)
  if (!isTRUE(validUTF8(texto))) return(.texto_valor(texto))
  codigos <- tryCatch(utf8ToInt(texto), error = function(e) NA_integer_)
  if (!length(codigos)) return("")
  if (anyNA(codigos)) return(.texto_valor(texto))
  visibles <- codigos >= 32L & codigos < 127L
  partes <- character(length(codigos))
  partes[visibles] <- vapply(
    codigos[visibles], intToUtf8, character(1L), USE.NAMES = FALSE
  )
  partes[!visibles] <- sprintf("<U+%04X>", codigos[!visibles])
  paste0(partes, collapse = "")
}

# Como se VE un texto: su descomposicion canonica, sin los caracteres que no se
# ven. Dos textos con la misma forma visible y distintos bytes se imprimen
# iguales, que es lo que la evidencia no puede hacer con dos valores que el
# veredicto separo. Lo que no decodifica se deja tal cual.
.texto_visible_senal <- function(texto) {
  if (length(texto) != 1L || is.na(texto) || Encoding(texto) == "bytes") {
    return(.texto_valor(texto))
  }
  if (Encoding(texto) == "latin1") texto <- enc2utf8(texto)
  if (!isTRUE(validUTF8(texto))) return(.texto_valor(texto))
  codigos <- tryCatch(utf8ToInt(texto), error = function(e) NA_integer_)
  if (anyNA(codigos)) return(.texto_valor(texto))
  codigos <- codigos[!.codigos_control_invisible(codigos)]
  .normalizacion_a_texto(
    .normalizacion_ordenar(.normalizacion_descomponer(codigos))
  )
}

#' Detectar filas donde señales redundantes se contradicen
#'
#' Recorre las señales declaradas con [senal_redundante()] e informa las filas
#' donde las columnas que deberían codificar el mismo hecho no concuerdan dentro
#' de la ventana declarada.
#'
#' El valor de este diagnóstico es que **ninguna columna por separado lo
#' muestra**: los tres años de una fila pueden ser todos plausibles y aun así
#' contradecirse entre sí.
#'
#' Una fila donde alguna de las columnas comparadas está ausente **no se cuenta
#' como discordante ni como concordante**: se excluye del universo evaluado, y
#' `n_evaluadas` lo declara. Ausencia no es desacuerdo.
#'
#' Una fila con el mismo valor en todas las columnas concuerda, también si ese
#' valor es infinito: `Inf` contra `Inf` es coincidencia exacta.
#'
#' Una fila citada no muestra iguales dos valores que el veredicto separó. Si la
#' escritura corriente los junta —dos dobles que coinciden en 15 cifras, dos
#' instantes del mismo segundo, dos textos que se ven iguales con distintos
#' puntos de código—, esa fila los cita con la escritura que los distingue: el
#' doble con las cifras que lo devuelven a su valor, el instante con su fracción
#' y el texto con cada carácter fuera del ASCII imprimible escrito `<U+XXXX>`,
#' con una nota que lo dice.
#'
#' @param datos Tabla que se desea examinar.
#' @param senales Una señal creada por [senal_redundante()] o una lista de
#'   señales.
#' @param max_ejemplos Máximo de filas concretas que se citan como evidencia.
#' @param proteger_datos_personales Si los valores de las columnas clasificadas
#'   como dato personal se enmascaran en `evidencia`. `TRUE` por omisión; el
#'   número de fila y el nombre de la columna se conservan igual. La
#'   clasificación se hace aquí por léxico y por forma —esta función no recibe un
#'   perfil—, así que lo que sólo quien conoce los datos sabe personal entra por
#'   `columnas_personales`.
#' @param columnas_personales Columnas que traen datos personales, declaradas
#'   con la misma forma que acepta [perfilar()]: nombres de columna, o un vector
#'   con nombre donde el nombre es la columna y el valor es el tipo. Sin ella, una columna que el léxico no reconoce
#'   —un legajo interno— publica sus valores en `evidencia`. Sólo
#'   tiene efecto con `proteger_datos_personales = TRUE`.
#'
#' @return Data frame con una fila por señal: `senal`, `columnas`, `n_filas`,
#'   `n_evaluadas`, `n_discordantes`, `proporcion`, `ventana` y `evidencia`.
#'
#'   `evidencia` cita filas concretas, y **no publica los valores de una columna
#'   clasificada como dato personal** —salvo que se pida lo contrario con
#'   `proteger_datos_personales = FALSE`—: esos salen como `[valor protegido]`. El
#'   número de fila y el nombre de la columna se conservan, que es lo que hace
#'   falta para ir a corregirla. Las columnas que no son personales se citan
#'   enteras, salvo lo que en ellas repite un valor de una columna personal de
#'   la tabla —una observación que cita la cédula—: eso se tapa con el mismo
#'   piso que aplica [perfilar()], aunque la columna personal no esté en la
#'   señal.
#' @export
#' @seealso [senal_redundante()], [detectar_dependencias()],
#'   [detectar_relaciones()]
#'
#' @examples
#' d <- data.frame(
#'   anio_fiscal = c(2023L, 2023L, 2022L),
#'   anio_archivo = c(2023L, 2023L, 2023L)
#' )
#' detectar_discordancias(d, senal_redundante(c("anio_fiscal", "anio_archivo")))
detectar_discordancias <- function(datos, senales, max_ejemplos = 5L,
                                   proteger_datos_personales = TRUE,
                                   columnas_personales = character()) {
  .validar_datos_tabla(datos)
  datos <- .tabla_base(datos)
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales, names(datos)
  )
  if (inherits(senales, "senal_redundante")) senales <- list(senales)
  if (!is.list(senales) || !length(senales) ||
      !all(vapply(senales, inherits, logical(1L), "senal_redundante"))) {
    stop(
      "`senales` debe ser una senal_redundante o una lista de ellas.",
      call. = FALSE
    )
  }
  if (!is.numeric(max_ejemplos) || length(max_ejemplos) != 1L ||
      is.na(max_ejemplos) || max_ejemplos < 0) {
    stop("`max_ejemplos` debe ser un entero no negativo.", call. = FALSE)
  }
  max_ejemplos <- as.integer(max_ejemplos)

  filas <- lapply(senales, function(senal) {
    indices <- .indice_nombre(senal$columnas, names(datos))
    faltantes <- senal$columnas[is.na(indices)]
    if (length(faltantes)) {
      stop(
        "La senal '", senal$nombre, "' nombra columnas que no estan en los ",
        "datos: ", paste(faltantes, collapse = ", "),
        ". Disponibles: ", paste(names(datos), collapse = ", "), ".",
        call. = FALSE
      )
    }
    senal <- senal
    senal$columnas <- names(datos)[indices]
    if (length(senal$transformacion)) {
      indices_transformacion <- .indice_nombre(
        names(senal$transformacion), senal$columnas
      )
      names(senal$transformacion) <- senal$columnas[indices_transformacion]
    }
    valores <- .valores_senal(datos, senal)
    completos <- Reduce(`&`, lapply(valores, function(x) !is.na(x)))
    n_evaluadas <- sum(completos)
    if (!n_evaluadas) {
      return(data.frame(
        senal = senal$nombre,
        columnas = paste(senal$columnas, collapse = ", "),
        n_filas = as.numeric(nrow(datos)), n_evaluadas = 0,
        n_discordantes = NA_real_, proporcion = NA_real_,
        ventana = senal$ventana,
        evidencia = paste(
          "No hubo filas con todas las columnas presentes: la senal no se",
          "evaluo."
        ),
        stringsAsFactors = FALSE
      ))
    }
    comparables <- lapply(valores, function(x) x[completos])
    numericos <- all(vapply(comparables, is.numeric, logical(1L)))
    # La ventana solo puede gobernar una comparacion numerica, y un `Date` no es
    # `is.numeric()`: la fila publicaba `ventana = 1` al lado de una comparacion
    # por igualdad exacta. Medido, dos columnas `Date` a un dia con `ventana = 1`
    # dan `n_discordantes = 2` y el control numerico con las mismas distancias da
    # 0. La evidencia ya lo declaraba -"comparacion textual exacta"-; el aviso es
    # para quien lee la tabla y no la evidencia.
    if (!numericos && senal$ventana > 0) {
      warning(
        "La se\u00f1al `", senal$nombre, "` declara `ventana = ", senal$ventana,
        "` y sus columnas no son num\u00e9ricas: se compar\u00f3 por igualdad exacta y ",
        "la ventana no se aplic\u00f3. Para comparar fechas con tolerancia, ",
        "transformarlas a un n\u00famero en la unidad de la ventana con ",
        "`transformacion`.",
        call. = FALSE
      )
    }
    discordante <- if (numericos) {
      # Una fila con el MISMO infinito en todas las columnas concuerda, y la
      # resta lo perdia: `Inf - Inf` es `NaN`, `NaN > ventana` es `NA`, y ese
      # `NA` entraba a `indices`. Con `max_ejemplos` por omision la cita
      # abortaba -"subindice fuera de los limites"-, y con `max_ejemplos = 0` la
      # fila concordante se contaba discordante. Medido en la ronda 27: `a = b =
      # Inf` en una fila de cinco publicaba 3 discordantes donde hay 2. Valores
      # iguales concuerdan sin restar; entre distintos la resta nunca es `NaN`.
      maximo <- do.call(pmax, comparables)
      minimo <- do.call(pmin, comparables)
      maximo != minimo & maximo - minimo > senal$ventana
    } else {
      claves <- .claves_senal(comparables)
      Reduce(`|`, lapply(claves[-1L], function(x) x != claves[[1L]]))
    }
    indices <- which(completos)[discordante]
    ejemplos <- utils::head(indices, max_ejemplos)
    # La evidencia cita filas concretas, y sobre una columna de documentos eso
    # es publicar documentos: `perfilar()` enmascara esa misma columna -su moda
    # sale `[valor protegido]` y sus extremos `NA`- y por aca salian literales,
    # sin ningun aviso. Se clasifica con la misma herramienta que usa el resto
    # del paquete, `.columnas_personales_rapidas()`, asi que se enmascara
    # exactamente lo mismo que enmascara `perfilar()` y nada mas: una columna
    # que el paquete no clasifica como personal se sigue publicando entera.
    #
    # No le quita utilidad: esta funcion existe para decir QUE FILAS discrepan, y
    # el numero de fila ya las localiza. Saber que la fila 5 discrepa y en que
    # par de columnas es todo lo que hace falta para ir a corregirla.
    # La primitiva concentra las selecciones de tablas de entrada; seleccionar a
    # mano con `[, , drop = FALSE]` esta topeado y auditado por `test-ronda160`.
    personales <- if (isTRUE(proteger_datos_personales)) {
      .columnas_personales_rapidas(
        .seleccionar_columnas(datos, senal$columnas),
        declaradas = columnas_personales
      )
    } else {
      character()
    }
    escapo_texto <- FALSE
    detalle <- if (length(ejemplos)) {
      paste(vapply(ejemplos, function(fila) {
        celdas <- lapply(valores, function(x) x[fila])
        textos <- vapply(celdas, .texto_valor, character(1L))
        # Dos dobles distintos que se escriben igual con 15 cifras -`0.1 + 0.2` y
        # `0.3`- citaban `a=0.3; b=0.3` en una fila declarada discordante, y quien
        # leia no podia rehacer el veredicto. Si la fila tiene mas valores que
        # escrituras, se citan con la escritura que los distingue.
        #
        # La ronda 15 lo cerro solo para los dobles, y es una propiedad, no un
        # tipo: dos instantes a 0,3 s se citaban los dos `12:00:00 UTC`, y `cafe`
        # con la tilde compuesta y descompuesta, los dos igual. Los valores se
        # cuentan con la misma clave que decide el veredicto -en la via numerica
        # tambien, que con dobles da lo mismo que contar los numeros-. Y se
        # pregunta si alguna escritura junta dos valores, no si hay menos
        # escrituras que valores: con tres columnas, `(a, b, b)` escritos
        # `(x, x, y)` tienen dos de cada y citaban iguales dos valores distintos.
        #
        # Y la escritura se compara como SE VE, no como cadena: la tilde
        # compuesta y la descompuesta son dos cadenas para `unique()` y una sola
        # para quien lee. Un texto se compara por su forma visible -descompuesta
        # y sin invisibles-.
        claves_fila <- unlist(.claves_senal(celdas), use.names = FALSE)
        vistas <- vapply(seq_along(celdas), function(k) {
          x <- celdas[[k]]
          if (is.character(x) || is.factor(x)) {
            .texto_visible_senal(as.character(x))
          } else textos[[k]]
        }, character(1L))
        junta <- length(unique(vistas)) <
          nrow(unique(data.frame(clave = claves_fila, vista = vistas)))
        protegidas <- .nombres_para_operar(senal$columnas) %in%
          .nombres_para_operar(personales)
        if (junta) {
          distintos <- vapply(celdas, .texto_que_distingue_senal, character(1L))
          de_texto <- vapply(celdas, function(x) {
            is.character(x) || is.factor(x)
          }, logical(1L))
          if (any(de_texto & !protegidas & distintos != textos, na.rm = TRUE)) {
            escapo_texto <<- TRUE
          }
          textos <- distintos
        }
        textos[protegidas] <- "[valor protegido]"
        partes <- paste0(senal$columnas, "=", textos)
        paste0("fila ", fila, ": ", paste(partes, collapse = "; "))
      }, character(1L)), collapse = " | ")
    } else if (length(indices)) {
      # Con `max_ejemplos = 0` no se cita ninguna fila, y la evidencia decia "sin
      # filas discordantes" al lado de `n_discordantes = 2` en la misma fila.
      paste0(
        length(indices),
        if (length(indices) == 1L) {
          " fila discordante, no citada"
        } else " filas discordantes, ninguna citada",
        " (`max_ejemplos = ", max_ejemplos, "`)"
      )
    } else {
      "sin filas discordantes"
    }
    data.frame(
      senal = senal$nombre,
      columnas = paste(senal$columnas, collapse = ", "),
      n_filas = as.numeric(nrow(datos)),
      n_evaluadas = as.numeric(n_evaluadas),
      n_discordantes = as.numeric(length(indices)),
      proporcion = length(indices) / n_evaluadas,
      ventana = senal$ventana,
      evidencia = paste0(
        detalle,
        if (escapo_texto) .NOTA_ESCAPES else "",
        "; universo: ", n_evaluadas, " de ", nrow(datos),
        " filas con todas las columnas presentes",
        if (!numericos) "; comparacion textual exacta" else "",
        if (senal$ventana > 0 && numericos) {
          paste0("; ventana declarada: ", senal$ventana)
        } else ""
      ),
      stringsAsFactors = FALSE
    )
  })
  salida <- do.call(rbind, filas)
  if (is.null(salida)) salida <- .discordancia_vacia()
  rownames(salida) <- NULL
  # El mismo piso que aplican `perfilar()`, `analizar()` y
  # `distribucion_valores()`: la mascara de arriba es por columna, y el
  # documento protegido escrito en una columna de la senal -`obs`, un reclamo
  # que cita la cedula- salia en `evidencia` mientras las tres hermanas lo
  # tapaban sobre la misma tabla. Medido en la ronda 25. Necesita las columnas
  # personales de la tabla ENTERA, tambien las que ninguna senal nombra, y
  # clasificarlas costaba diez veces la funcion -2,5 s contra 0,2 s sobre
  # 50.000 filas y 43 columnas-: se hace solo si la evidencia cita filas, que es
  # el unico lugar por donde sale un valor de celda.
  cita_filas <- isTRUE(proteger_datos_personales) && max_ejemplos > 0L &&
    isTRUE(any(salida$n_discordantes > 0, na.rm = TRUE))
  if (cita_filas) {
    personales_tabla <- .columnas_personales_rapidas(
      datos, declaradas = columnas_personales
    )
    identificantes <- .valores_identificantes(
      .valores_publicables_protegidos(datos, personales_tabla),
      .fechas_que_identifican(datos, personales_tabla)
    )
    if (length(identificantes)) {
      salida <- .proteger_textos_salida(
        salida, identificantes, intocables = names(datos)
      )
    }
  }
  class(salida) <- c("discordancias_senales", "data.frame")
  salida
}
