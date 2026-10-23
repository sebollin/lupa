# La representacion numerica que alimenta un patron no puede quedar a merced de
# `options(scipen)` ni de `options(digits)`: una misma columna no puede cambiar
# de categoria porque la sesion eligio notacion cientifica. `OutDec` tambien se
# fija aqui porque esta conversion es para decidir una forma, no para publicar
# un numero; `.formatear_numero_publicado()` sigue siendo la unica conversion
# de numeros del paquete.
.textos_para_clasificacion_personal <- function(x) {
  textos <- .texto_analizable(x)$valores
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    opciones <- options(OutDec = ".")
    on.exit(options(opciones), add = TRUE)
    textos <- if (is.double(x) && !inherits(x, "integer64") || is.integer(x)) {
      .formatear_numeros_uno_a_uno(x)
    } else {
      vapply(
        seq_along(x), function(i) .formatear_numero_publicado(x[[i]]),
        character(1L), USE.NAMES = FALSE
      )
    }
  }
  trimws(textos)
}

# Lo mismo que `.formatear_numero_publicado()` valor por valor -15 cifras, sin
# notacion cientifica, cada numero con sus propias cifras- pero en una pasada.
# Se llamaba una vez POR VALOR: en una tabla de 200.000 filas, el 20 % del tiempo
# de `perfilar()` se iba en formatear numeros para clasificarlos como dato
# personal. `as.character()` con `scipen` alto da el mismo texto -medido sobre
# 1,3 millones de valores de todas las magnitudes y signos: cero diferencias-,
# salvo en los valores no nulos por debajo de 1e-4, donde `format()` deja ceros
# finales; esos, pocos, siguen por el camino de uno en uno. Un `integer64` no
# entra aca: `as.double()` lo redondearia.
.formatear_numeros_uno_a_uno <- function(x) {
  salida <- rep(NA_character_, length(x))
  utiles <- !is.na(x)
  if (!any(utiles)) return(salida)
  valores <- as.double(x[utiles]) + 0
  # `OutDec` NO se toca aca: quien llama decide la marca -la clasificacion la fija
  # en "." y la cosecha de valores protegidos usa la de la sesion, que es con la
  # que salen publicados-. `as.character()` y `format()` la respetan las dos.
  opciones <- options(scipen = 999)
  on.exit(options(opciones), add = TRUE)
  texto <- as.character(valores)
  chicos <- is.finite(valores) & valores != 0 & abs(valores) < 1e-4
  if (any(chicos)) {
    texto[chicos] <- vapply(
      valores[chicos], .formatear_numero_publicado, character(1L)
    )
  }
  salida[utiles] <- texto
  salida
}

# La proporcion se calcula sobre los valores que se pueden juzgar -un blanco no
# tiene forma que comparar contra un patron- y por eso los excluye. Pero ese
# denominador se publicaba sin decirse: una columna con ocho documentos y treinta
# y dos blancos publicaba `proporcion_compatible = 1`, o sea "100 % compatible",
# calculado sobre ocho de cuarenta valores.
#
# No se cambia el denominador -incluir los blancos convertiria toda columna
# medio vacia en incompatible, que es un falso positivo peor-. Se declara sobre
# cuantos se calculo, que es lo que faltaba para poder leer el numero.
.proporcion_compatible <- function(x, patron) {
  valores <- .textos_para_clasificacion_personal(x)
  presentes <- !is.na(valores) & nzchar(valores)
  if (!any(presentes)) {
    return(structure(NA_real_, evaluados = 0L, totales = length(valores)))
  }
  structure(
    mean(grepl(patron, valores[presentes], perl = TRUE)),
    evaluados = as.integer(sum(presentes)),
    totales = length(valores)
  )
}

.normalizar_validadores_personales <- function(validadores = NULL) {
  if (is.null(validadores)) validadores <- validadores_uruguay()
  if (identical(validadores, FALSE) ||
      (is.numeric(validadores) && !length(validadores))) {
    return(list())
  }
  if (!is.list(validadores) || is.null(names(validadores)) ||
      !length(validadores) || anyNA(names(validadores)) ||
      any(!nzchar(names(validadores))) ||
      anyDuplicated(.nombres_para_operar(names(validadores))) ||
      !all(vapply(validadores, is.function, logical(1L)))) {
    .detener("`validadores_personales` debe ser un pack o una lista con nombres de funciones.",
         call. = FALSE)
  }
  validadores
}

# Cuantos valores puede recorrer la confirmacion completa. Cuando la muestra
# supera el umbral, el validador se vuelve a correr para confirmar sobre la
# columna entera, y sobre una tabla de millones de filas eso es una pasada que
# nadie pidio. El tope la acota, y lo que se evaluo viaja con el resultado.
.max_validacion_completa <- 200000L

# Un alcance parcial se dice; uno completo no necesita decirse.
.alcance_validacion <- function(proporciones, indice) {
  evaluados <- attr(proporciones, "n_evaluados", exact = TRUE)
  total <- attr(proporciones, "n_total", exact = TRUE)
  if (is.null(evaluados) || is.null(total) || is.na(evaluados[[indice]]) ||
      evaluados[[indice]] >= total) {
    return("")
  }
  paste0(" sobre ", evaluados[[indice]], " de ", total, " valores")
}

.proporcion_validadores <- function(textos, validadores, umbral,
                                    muestra = 1000L,
                                    max_completo = .max_validacion_completa) {
  if (!length(validadores) || !length(textos)) return(numeric())
  indices <- if (length(textos) <= muestra) seq_along(textos) else {
    unique(round(seq(1, length(textos), length.out = muestra)))
  }
  evaluados <- rep(NA_integer_, length(validadores))
  proporciones <- vapply(seq_along(validadores), function(k) {
    validador <- validadores[[k]]
    parcial <- validador(textos[indices])
    if (!is.logical(parcial) || length(parcial) != length(indices)) {
      .detener("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    parcial <- mean(parcial %in% TRUE)
    evaluados[[k]] <<- length(indices)
    if (!is.finite(parcial) || parcial < umbral || length(indices) == length(textos)) {
      return(parcial)
    }
    confirmacion <- if (length(textos) <= max_completo) {
      seq_along(textos)
    } else {
      unique(round(seq(1, length(textos), length.out = max_completo)))
    }
    completo <- validador(textos[confirmacion])
    if (!is.logical(completo) || length(completo) != length(confirmacion)) {
      .detener("Cada validador personal debe devolver un vector l\u00f3gico de igual longitud.",
           call. = FALSE)
    }
    evaluados[[k]] <<- length(confirmacion)
    mean(completo %in% TRUE)
  }, numeric(1L))
  names(proporciones) <- names(validadores)
  # El conteo viaja con la proporcion para que el fundamento pueda decir sobre
  # cuantos valores se confirmo: una proporcion medida sobre una parte no es
  # una proporcion de la columna.
  attr(proporciones, "n_evaluados") <- evaluados
  attr(proporciones, "n_total") <- length(textos)
  proporciones
}

# Una palabra de PERSONA en el nombre de la columna -`cliente`, `socio`,
# `usuario`- dice de quien habla la columna, no que guarde su nombre. Hasta la
# ronda 33 alcanzaba en cualquier posicion, y una clave foranea (`cliente_id`),
# una categoria (`tipo_cliente`, `estado_socio`) o una fecha (`cliente_desde`)
# se protegian como "nombre": se tapaban su moda, sus extremos y sus ejemplos,
# y sus categorias -"mayorista", "empresa"- entraban al piso y se tapaban en
# columnas que no tenian nada que ver, por las dos puertas. Medido en la ronda
# 34 (H4). La ayuda dice que `medio` es "el nombre de la columna expresa una
# categoria personal", y `tipo_cliente` no la expresa: el resto del nombre
# dice que es otra cosa.
#
# Ahora la palabra de persona clasifica la columna como `nombre` solo si
# ninguna otra regla de nombre la reconocio -`direccion_cliente` es un
# domicilio, `telefono_socio` un telefono-, si el resto del nombre no la
# califica como clave, tipo, estado, categoria, fecha, cantidad o importe, y si
# sus valores pueden ser nombres: una columna de numeros, fechas o logicos no
# es el nombre de nadie. Una palabra de nombre -`nombre`, `apellido`- protege
# como siempre: `nombre_cliente` es un nombre.
#
# Y es la MISMA propiedad en las demas reglas de nombre, medida en la misma
# ronda: `tipo_documento` ("CI", "PASAPORTE") se protegia como documento con
# poder `alto`, `fecha_contacto` y `medio_contacto` como telefono,
# `estado_correo` como correo, `tipo_domicilio` como domicilio. Una regla, no
# siete sitios: la palabra del tipo personal clasifica la columna salvo que el
# RESTO del nombre -lo que queda al sacarle esa palabra- la califique como otra
# cosa (`.CALIFICADORES_DE_NOMBRE`), con los calificadores que valen para cada
# tipo (`.CALIFICADORES_POR_TIPO`), o que sus valores no puedan ser de ese tipo
# (`.TIPOS_QUE_NO_SON_POR_TIPO`). Que vale para cada tipo se decidio para el
# lado cerrado: `nro_documento`, `numero_celular` y `documento_id` siguen
# siendo el documento y el telefono -la clave no los califica-; `barrio_id` y
# `cod_barrio` siguen siendo un domicilio -el codigo de un barrio dice lo mismo
# que el barrio-; una fecha de nacimiento solo deja de serlo si el nombre dice
# que es una categoria, y ningun tipo de valor la saca; `segundo_nombre` y
# `nombre_medio` son nombres. Un calificador que falte deja la columna
# protegida como antes.
.PATRON_PERSONA_EN_NOMBRE <- paste0(
  "(^|_)(persona|cliente|paciente|socio|beneficiario|titular|funcionario|",
  "usuario|solicitante|responsable|contribuyente)($|_)"
)

.CALIFICADORES_DE_NOMBRE <- list(
  categoria = paste0(
    "tipo|tipos|estado|estados|categoria|clase|segmento|grupo|nivel|canal|",
    "motivo|causa|causas|resultado|resultados|cantidad|cant|total|monto|",
    "importe|saldo"
  ),
  # Y la forma o el metodo -`forma_pago_cliente`, `metodo_pago_cliente`-
  # (ronda 37).
  medio = "medio|medios|via|forma|formas|metodo|metodos|modo|modalidad",
  fecha = "fecha|fec|desde|hasta|alta|baja",
  # Ronda 36: el lugar de un nacimiento o de una defuncion no es su fecha
  # (`pais_nacimiento` con `"Uruguay"`).
  lugar = paste0(
    "pais|lugar|ciudad|localidad|departamento|depto|dpto|provincia|region|",
    "nacionalidad|municipio"
  ),
  # Ronda 37: una parte de la fecha no es la fecha -`edad_fallecimiento`,
  # `anio_nacimiento`-.
  parte = "edad|edades|anio|anios|ano|anos|year|mes|dia",
  # Y una medida o un plazo no es un telefono, un correo ni un domicilio
  # (`telefono_plazo_dias`, `calle_metros`; ronda 37).
  medida = "plazo|plazos|dias|duracion|tiempo|minutos|segundos|horas|metros",
  # Ronda 37: la direccion de una red o de un organismo no es un domicilio
  # (`dir_ip`, `dir_web`, `direccion_general` con `"Direccion de Salud"`).
  red = "web|url|ip|ipv4|ipv6|mac|host|servidor|ftp|http|https|red",
  institucion = "general|nacional|departamental|ejecutiva|tecnica|regional",
  clave = "id|ids|cod|codigo|nro|num|numero|uuid|key"
)

.CALIFICADORES_POR_TIPO <- list(
  documento_identidad = c("categoria", "fecha"),
  correo = c("categoria", "medio", "fecha", "clave", "medida"),
  telefono = c("categoria", "medio", "fecha", "medida"),
  fecha_nacimiento = c("categoria", "lugar", "parte"),
  fecha_fallecimiento = c("categoria", "lugar", "parte"),
  # Ronda 37: el nombre de un departamento o de una localidad es la categoria
  # de una base publica (`nombre_departamento`, `nom_depto`), y pide la senal
  # de los valores, como la palabra de persona con un lugar.
  nombre = "lugar",
  # Ronda 36: tambien el medio -`medio_pago_cliente`, `via_ingreso_usuario`-,
  # como en las demas palabras.
  # Ronda 37: y el lugar -`pais_cliente`, `nacionalidad_socio`-.
  persona = c("categoria", "medio", "fecha", "clave", "lugar"),
  domicilio = c("categoria", "medio", "fecha", "lugar", "medida", "red",
                "institucion")
)

# Los tipos -inferidos o declarados- cuyos valores no son de ese tipo personal:
# una fecha o un logico no es un nombre, un correo ni un domicilio; un numero no
# es un correo ni el nombre de nadie. El documento y el telefono no salen por el
# tipo: un telefono fijo escrito `2901-12-12` tiene forma de fecha, y la
# inferencia no los distingue.
.TIPOS_QUE_NO_SON_POR_TIPO <- local({
  fechas <- c("fecha", "fecha-hora", "Date", "POSIXct", "POSIXlt", "difftime",
              "hms", "logico")
  numeros <- c("entero", "doble", "integer64", "numeric", "integer")
  list(
    documento_identidad = character(), correo = c(fechas, numeros),
    telefono = character(), fecha_nacimiento = character(),
    fecha_fallecimiento = character(), nombre = c(fechas, numeros),
    persona = c(fechas, numeros), domicilio = fechas
  )
})

# Que dice la palabra de `patron` sobre la columna `normalizado`, como tipo
# personal `tipo`: `"no"` si no esta o el tipo de sus valores la excluye; `"si"`
# si la clasifica; `"calificada"` si el resto del nombre -lo que queda al
# sacarle la palabra- la califica como otra cosa, y entonces `calificadores`
# dice con que palabras. Quien decide con los valores es
# `.tipo_por_valores_calificada()`.
# Si los valores de `x` dejan que el nombre diga que es un documento: algun
# valor tiene las seis cifras de un documento -la cedula de seis sin su
# verificador es el mas corto que reconoce la regla de la copia-, o no hay
# valores bastantes para desmentirlo. Con la representacion de texto del
# clasificador, que escribe tambien un `integer64` o un numero de mas de quince
# cifras con todas sus cifras.
#
# Ronda 38 (P-H5): sin valores, la ronda 37 desmentia el nombre. La via DBI
# clasifica sobre la MUESTRA y publica el resumen de la tabla entera: la cedula
# del conyuge o del tutor, rala, con `muestra = 200` no traia un solo valor, la
# columna dejaba de protegerse y el resumen publicaba su minimo, su maximo y su
# mediana -cedulas-, y por bloques cada valor. Desmienten el nombre al menos
# `.MIN_VALORES_DESMIENTEN_DOCUMENTO` valores, de dos distintos o mas, que
# digan algo -no vacios ni solo ceros-: sin valores, con pocos o con un solo
# valor repetido -el `0` o el `"S/D"` del que no tiene- decide el nombre, como
# en la ronda 36.
.tiene_cifras_de_documento <- function(x) {
  if (inherits(x, c("Date", "POSIXt", "difftime")) || is.list(x)) return(TRUE)
  if (is.factor(x)) x <- as.character(x)
  presentes <- x[!is.na(x)]
  distintos <- unique(presentes)
  textos <- tryCatch(.textos_para_clasificacion_personal(distintos),
                     error = function(e) NULL)
  if (is.null(textos) || length(textos) != length(distintos)) return(TRUE)
  cifras <- nchar(gsub("[^0-9]", "", textos, useBytes = TRUE), type = "bytes")
  if (any(!is.na(textos) & cifras >= 6L)) return(TRUE)
  dice <- !is.na(textos) & grepl("[1-9A-Za-z]", textos, perl = TRUE,
                                 useBytes = TRUE)
  if (sum(dice) < 2L) return(TRUE)
  sum(dice[match(presentes, distintos)]) < .MIN_VALORES_DESMIENTEN_DOCUMENTO
}

.MIN_VALORES_DESMIENTEN_DOCUMENTO <- 20L

# El nombre de una columna como lo lee el clasificador: lo que no es UTF-8
# valido -el encabezado `Direcci\xf3n` de un CSV de Windows leido sin declarar la
# codificacion, con `read.csv(check.names = FALSE)` o `data.table::fread()`-, y
# el `latin1` marcado, se leen como `latin1`, como sus valores
# (`.ascii_para_forma()`). Solo para decidir si la columna es personal: la
# identidad del nombre sigue siendo la de sus bytes (`.nombres_para_operar()`).
# Ronda 38 (P-H4): la clave `<lupa-byte:..>` dejaba `direcci_..._n`, que ningun
# patron reconoce, y `Direccion`, `Telefono`, `Cedula` o `Fecha de defuncion`
# con su tilde publicaban sus valores en cualquier sesion.
.nombre_para_clasificar <- function(nombre) {
  nombre <- as.character(nombre)
  if (!length(nombre)) return(nombre)
  latin <- !is.na(nombre) &
    grepl("[^\\x01-\\x7f]", nombre, perl = TRUE, useBytes = TRUE) &
    (Encoding(nombre) == "latin1" | !validUTF8(nombre))
  if (!any(latin)) return(nombre)
  leidos <- tryCatch(iconv(nombre[latin], "latin1", "UTF-8", sub = "?"),
                     error = function(e) NULL)
  if (length(leidos) != sum(latin) || anyNA(leidos)) return(nombre)
  nombre[latin] <- leidos
  nombre
}

# `NombreCliente`, `nroDoc`, `eMail`: una mayuscula despues de una minuscula o de
# una cifra separa palabras, como el guion bajo. Por bytes ASCII: un nombre con
# letras de otra escritura queda como esta.
.separar_camel <- function(nombre) {
  nombre <- as.character(nombre)
  tryCatch(
    gsub("([a-z0-9])([A-Z])", "\\1_\\2", nombre, perl = TRUE,
         useBytes = TRUE),
    error = function(e) nombre
  )
}

# Las siglas de un documento de identidad en la lista del nombre.
.SIGLAS_DE_DOCUMENTO <- c("ci", "dni", "rut", "curp")

# Ronda 39 (H-08): una sigla escrita con un punto o un guion bajo tras cada
# letra -`C.I.`, como se encabeza la cedula en una planilla uruguaya, y `C.I`,
# `c.i.`, `C. I.`, `Nro. C.I.`, `C.I. Titular`, `D.N.I.`; y `c_i`,
# `nro_c_i`, como lo deja `janitor::clean_names()`- llegaba al patron con un
# separador entre cada letra (`c_i_`), el patron pide la sigla entera, y el
# encabezado no aportaba: una columna de cedulas sin su digito verificador se
# clasificaba documento `debil` y publicaba sus valores. Se junta la sigla
# ENTERA, de la primera letra con separador a la ultima, y solo si es una de
# `.SIGLAS_DE_DOCUMENTO`: `C.I.I.U.` -la actividad economica- es `ciiu` y
# sigue sin ser un documento. La ultima letra lleva su punto o va pegada al
# separador anterior: en `C.I. N` seguido del signo de numero la `N` es el
# numero, no otra letra de la sigla. Limitacion: `n_c_i` -el mismo encabezado
# con el numero delante, pasado por `clean_names()`- es una sigla de tres
# letras y no se reconoce.
.juntar_siglas_de_documento <- function(nombre) {
  nombre <- as.character(nombre)
  if (!length(nombre)) return(nombre)
  patron <- paste0(
    "(?<![A-Za-z0-9])[A-Za-z][._](?: ?[A-Za-z][._])*",
    "(?:[A-Za-z](?![A-Za-z0-9]))?"
  )
  tryCatch({
    marcas <- gregexpr(patron, nombre, perl = TRUE)
    piezas <- regmatches(nombre, marcas)
    regmatches(nombre, marcas) <- lapply(piezas, function(p) {
      if (!length(p)) return(p)
      letras <- gsub("[^A-Za-z]", "", p, perl = TRUE)
      sigla <- nchar(letras, type = "bytes") >= 2L &
        .minusculas_ascii(letras) %in% .SIGLAS_DE_DOCUMENTO
      ifelse(sigla, paste0(" ", letras, " "), p)
    })
    nombre
  }, error = function(e) nombre)
}

.palabra_de_tipo <- function(normalizado, patron, tipo, tipo_valores,
                             x = NULL) {
  no <- list(estado = "no", calificadores = character())
  if (!isTRUE(grepl(patron, normalizado, perl = TRUE))) return(no)
  # Excluida por el tipo de sus valores: no es de ESTE tipo -un numero no es el
  # nombre de nadie-, pero sus valores pueden mostrar otro: `usuario_canal` con
  # `"0059899123456"`, que la inferencia lee como entero, es un telefono (ronda
  # 36). Lo decide `.tipo_por_valores_calificada()`.
  excluida <- list(estado = "excluida", calificadores = character())
  if (any(as.character(tipo_valores) %in% .TIPOS_QUE_NO_SON_POR_TIPO[[tipo]])) {
    return(excluida)
  }
  resto <- sub(patron, "_", normalizado, perl = TRUE)
  # Una columna de numeros es un domicilio solo si el resto del nombre dice que
  # es su codigo -`barrio_id`, `cod_barrio`, decision de la ronda 34-: los
  # metros de una calle no lo son (`calle_metros`, ronda 36).
  if (identical(tipo, "domicilio") &&
      any(as.character(tipo_valores) %in%
            c("entero", "doble", "integer64", "numeric", "integer")) &&
      !isTRUE(grepl(paste0("(^|_)(", .CALIFICADORES_DE_NOMBRE$clave, ")($|_)"),
                    resto, perl = TRUE))) {
    return(excluida)
  }
  # Un documento tiene cifras: una columna sin un solo valor de seis cifras o
  # mas -`cedula_valida` con `"S"`/`"N"`, `cod_documento` con `1`, `2`, `9`- no
  # guarda documentos, es una marca o un codigo de su tipo (ronda 37).
  if (identical(tipo, "documento_identidad") && !is.null(x) &&
      !isTRUE(tryCatch(.tiene_cifras_de_documento(x), error = function(e) TRUE))) {
    return(excluida)
  }
  palabras <- unlist(lapply(.CALIFICADORES_POR_TIPO[[tipo]], function(clase) {
    strsplit(.CALIFICADORES_DE_NOMBRE[[clase]], "|", fixed = TRUE)[[1L]]
  }), use.names = FALSE)
  usadas <- palabras[vapply(palabras, function(palabra) {
    isTRUE(grepl(paste0("(^|_)", palabra, "($|_)"), resto, perl = TRUE))
  }, logical(1L))]
  if (!length(usadas)) return(list(estado = "si", calificadores = character()))
  list(estado = "calificada", calificadores = usadas)
}

# Ronda 35 (H3). La ronda 34 hizo que el resto del nombre calificara la palabra
# del tipo -`tipo_cliente` es una categoria, `cliente_desde` una fecha- y decidia
# solo por el nombre: el tipo de los valores servia para EXCLUIR -numeros,
# fechas, logicos- y nunca para ver si el calificador decia la verdad.
# `usuario_alta`, `funcionario_baja`, `responsable_grupo` con nombres,
# `celular_estado` con celulares o `direccion_tipo` con domicilios dejaron de
# protegerse: moda, ejemplos, evidencia y, por bloques, cada valor.
#
# Una columna CALIFICADA es personal si sus valores lo dicen, con el tipo de los
# VALORES y no el de la palabra -`contacto_alta` con nombres es un nombre,
# `usuario_canal` con celulares un telefono- (`.tipo_personal_de_valores()`).
# La forma sola no alcanza: la primera version tomaba por nombre de persona
# cualquier texto de dos palabras con mayuscula -`"Gran Empresa"`,
# `"Cerro Largo"`, `"Arroz Premium"`- y por domicilio cualquier palabra con un
# numero -`"Oficina 101"`-, y tapaba categorias que la ronda 34 publicaba
# (segunda refutacion de la ronda 35). Cada forma pide una senal que una
# categoria no tiene: un nombre de pila conocido, una `@`, un telefono escrito
# como telefono, una marca de calle.
#
# Y la palabra de quien ACTUA en el registro -`usuario`, `funcionario`,
# `responsable`, `solicitante`- con una de auditoria -`alta`, `baja`- dice quien
# dio el alta o la baja: ahi el calificador de fecha cae con cualquier texto que
# no sea una fecha (`.textos_que_no_son_fecha()`), porque un login `jperez` o un
# apellido solo no se distinguen por su forma de una categoria, pero no son la
# fecha que el nombre anuncia. La palabra de quien es OBJETO del registro
# -`cliente_desde`, `paciente_alta`- pide la senal fuerte: `"Sin fecha"` o
# `"Traslado"` siguen publicandose.
.tipo_por_valores_calificada <- function(x, palabras, persona, normalizado) {
  calificados <- names(palabras)[vapply(palabras, function(p) {
    identical(p$estado, "calificada")
  }, logical(1L))]
  if (identical(persona$estado, "calificada")) {
    calificados <- c(calificados, "nombre")
  }
  # Las excluidas por el tipo de sus valores no ponen su tipo (`propios`), pero
  # sus valores se miran igual: un telefono escrito como telefono lo es aunque la
  # palabra fuera de persona.
  excluidas <- vapply(c(palabras, list(persona = persona)), function(p) {
    identical(p$estado, "excluida")
  }, logical(1L))
  if (!length(calificados) && !any(excluidas)) return(character())
  if (!length(calificados)) {
    tipo <- tryCatch(.tipo_personal_de_valores(x, propios = character(),
                                               peladas = FALSE),
                     error = function(e) NA_character_)
    return(if (length(tipo) == 1L && !is.na(tipo)) tipo else character())
  }
  # Una fecha de nacimiento o de defuncion calificada como categoria -
  # `nacimiento_estado`, `defuncion_tipo`- con FECHAS por valores es esa fecha:
  # una categoria no se escribe con fechas (ronda 36).
  fechas <- intersect(c("fecha_nacimiento", "fecha_fallecimiento"), calificados)
  if (length(fechas) &&
      isTRUE(tryCatch(.son_fechas(x), error = function(e) TRUE))) {
    return(fechas[[1L]])
  }
  # Las cifras sueltas son un telefono solo con la palabra del telefono, y no si
  # el resto del nombre dice que son un importe o una cantidad
  # (`importe_telefono`, `monto_celular`; ronda 36).
  peladas <- "telefono" %in% calificados &&
    !any(palabras$telefono$calificadores %in% .PALABRAS_DE_IMPORTE)
  # Si mirar los valores falla, la palabra protege, como hasta la ronda 33:
  # ante la duda, el lado cerrado.
  tipo <- tryCatch(.tipo_personal_de_valores(x, propios = calificados,
                                             peladas = peladas),
                   error = function(e) calificados[[1L]])
  if (length(tipo) == 1L && !is.na(tipo)) return(tipo)
  quien_actua <- identical(persona$estado, "calificada") &&
    isTRUE(grepl(.PATRON_QUIEN_ACTUA, normalizado, perl = TRUE)) &&
    all(persona$calificadores %in% c("alta", "baja"))
  if (quien_actua && isTRUE(tryCatch(.textos_que_no_son_fecha(x),
                                     error = function(e) TRUE))) {
    return("nombre")
  }
  character()
}

.PALABRAS_DE_IMPORTE <- c("cantidad", "cant", "total", "monto", "importe",
                          "saldo")

# Si los valores de una columna son fechas: de clase fecha, o textos que la
# inferencia, sobre sus distintos, lee como fecha.
.son_fechas <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(TRUE)
  if (!(is.character(x) || is.factor(x))) return(FALSE)
  textos <- .distintos_forma_personal(x)
  if (is.null(textos) || !length(textos)) return(FALSE)
  inferido <- tryCatch(.inferir_tipo_interno(textos)$tipo,
                       error = function(e) NA_character_)
  length(inferido) > 0L && isTRUE(inferido[[1L]] %in% c("fecha", "fecha-hora"))
}

.PATRON_QUIEN_ACTUA <- "(^|_)(usuario|funcionario|responsable|solicitante)($|_)"

# Si los valores de una columna son textos que no son fechas: la inferencia,
# sobre sus distintos, no los lee como fecha ni como numero -sobre los valores y
# no sobre el tipo que llega, para que `medir()`, que no infiere, decida igual
# que `perfilar()`-, y el 80 % de sus distintos lleva letras.
.textos_que_no_son_fecha <- function(x) {
  if (!(is.character(x) || is.factor(x))) return(FALSE)
  textos <- .distintos_forma_personal(x)
  if (is.null(textos) || !length(textos)) return(FALSE)
  inferido <- tryCatch(.inferir_tipo_interno(textos)$tipo,
                       error = function(e) NA_character_)
  if (!length(inferido) || is.na(inferido[[1L]]) ||
      inferido[[1L]] %in% c("fecha", "fecha-hora", "entero", "doble",
                            "logico")) {
    return(FALSE)
  }
  letras <- nchar(gsub("[^A-Za-z]", "", textos, perl = TRUE), type = "bytes")
  # Ni un estado del registro -`"Sin baja"`, `"Baja definitiva"`, `"Activo"`
  # en `usuario_baja` (ronda 36)-: un texto con una palabra de categoria o de
  # estado no dice quien dio el alta o la baja. Las iniciales y el `O'` no son
  # palabras de categoria (`"A. Rodriguez"`, `"O'Connor"`; ronda 37).
  #
  # Ronda 38 (P-H1, P-H3): el estado y el motivo, por una LISTA cerrada
  # (`.PALABRAS_DE_ESTADO`, en singular o plural) y no por su forma. La ronda 37
  # los reconocia tambien por la terminacion -`-ado`, `-ente`, `-cion`- y por la
  # frase -`"Fin de contrato"`-, y un login es UNA palabra: `lmachado`,
  # `pfuentes`, `"MACHADO, L."` sin la inicial y `"Juan perez"`, como lo deja el
  # autocapitalizado de un formulario, pasaban por estados; con cuatro
  # funcionarios uno bastaba para que la columna no llegara al 80 % y publicara
  # los logins de todos. Lo que no esta en la lista cuenta como una persona,
  # como en la ronda 36: un motivo que falta tapa de mas una columna de motivos
  # (`"Ingresos superiores"`), y un login que pasara por estado publicaria los
  # de todos.
  minusculas <- .minusculas_ascii(.sin_iniciales(textos))
  palabras <- regmatches(minusculas, gregexpr("[a-z]+", minusculas, perl = TRUE))
  estado <- vapply(palabras, function(p) {
    any(p %in% .PALABRAS_QUE_NO_VAN_EN_UN_NOMBRE) ||
      any(.singular_de_estado(p) %in% .PALABRAS_DE_ESTADO)
  }, logical(1L))
  mean(letras >= 3L & !estado) >= .PROPORCION_FORMA_PERSONAL
}

# Cada palabra y sus dos lecturas en singular -sin la `s` tras una vocal, sin
# el `es` tras una consonante-: `"Canceladas"`, `"JUBILACIONES"`, `"Ceses"`.
# Vale la que este en la lista.
.singular_de_estado <- function(palabras) {
  c(palabras, sub("(?<=[aeiou])s$", "", palabras, perl = TRUE),
    sub("(?<=[^aeiou])es$", "", palabras, perl = TRUE))
}

# Las palabras de un estado del registro o del motivo de un alta o de una baja,
# en singular, en minusculas y sin acentos. Una lista CERRADA (ronda 38): una
# palabra que falta tapa de mas, una de mas puede publicar logins. No estan las
# que son o parecen apellidos o nombres -`amado`, `prado`, `delgado`, `rosado`,
# `cortes`- ni los usuarios del sistema -`sistema`, `migracion`, `batch`, `web`-,
# que conviven con los logins en la misma columna: `"SISTEMA"` junto a tres
# logins dejaria la columna en el 75 %.
.PALABRAS_DE_ESTADO <- c(
  # estados del registro (ronda 36)
  "alta", "baja", "activo", "activa", "inactivo", "inactiva", "vigente",
  "pendiente", "anulado", "anulada", "suspendido", "suspendida", "definitiva",
  "definitivo", "temporal", "provisoria", "provisorio",
  # estados de un tramite, de una cuenta o de un vinculo (ronda 38)
  "cancelado", "cancelada", "rechazado", "rechazada", "bloqueado", "bloqueada",
  "desbloqueado", "desbloqueada", "confirmado", "confirmada", "aprobado",
  "aprobada", "denegado", "denegada", "autorizado", "autorizada", "habilitado",
  "habilitada", "deshabilitado", "deshabilitada", "inhabilitado",
  "inhabilitada", "finalizado", "finalizada", "cerrado", "cerrada", "abierto",
  "abierta", "vencido", "vencida", "caducado", "caducada", "expirado",
  "expirada", "eliminado", "eliminada", "revocado", "revocada", "rescindido",
  "rescindida", "cesado", "cesada", "cesante", "jubilado", "jubilada",
  "fallecido", "fallecida", "trasladado", "trasladada", "desvinculado",
  "desvinculada", "despedido", "despedida", "retirado", "retirada", "egresado",
  "egresada", "archivado", "archivada", "terminado", "terminada", "reactivado",
  "reactivada", "desactivado", "desactivada", "duplicado", "duplicada",
  "procesado", "procesada", "ingresado", "ingresada", "registrado",
  "registrada", "validado", "validada", "verificado", "verificada",
  "recibido", "recibida", "enviado", "enviada", "entregado", "entregada",
  "pagado", "pagada", "impago", "impaga", "otorgado", "otorgada", "concedido",
  "concedida", "iniciado", "iniciada", "completado", "completada", "resuelto",
  "resuelta", "observado", "observada", "devuelto", "devuelta",
  "voluntario", "voluntaria", "mutuo",
  # motivos de un alta o de una baja (ronda 38)
  "renuncia", "jubilacion", "despido", "fallecimiento", "defuncion", "deceso",
  "traslado", "cese", "cesantia", "retiro", "abandono", "pase", "destitucion",
  "rescision", "vencimiento", "expiracion", "cancelacion", "anulacion",
  "suspension", "finalizacion", "fin", "termino", "desvinculacion", "licencia",
  "incapacidad", "invalidez", "sancion", "contrato", "acuerdo"
)

# La proporcion de los valores distintos que tiene que tener la forma: la de las
# demas formas dominantes del clasificador (correo, documento).
.PROPORCION_FORMA_PERSONAL <- 0.8

# Los valores distintos no vacios de `x`, como texto ASCII para decidir una
# forma: el `latin1` se pasa a UTF-8, se transliteran los acentos
# (`.transliterar_ascii()`: `"Jose"` con o sin acento es el mismo nombre) y
# cada byte no ASCII que quede pasa a `A`, una letra. No aborta sobre texto que
# no es UTF-8 valido ni depende del locale. Un numero entero da sus cifras.
# `NULL` si la columna no es de texto ni de numeros.
.distintos_forma_personal <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, c("Date", "POSIXt", "difftime")) || is.list(x) ||
      is.logical(x)) {
    return(NULL)
  }
  if (is.numeric(x)) {
    x <- .cifras_de_numeros(x)
  } else if (!is.character(x)) {
    return(NULL)
  }
  x <- .recortar_bytes(.ascii_para_forma(unique(x[!is.na(x)])))
  unique(x[nzchar(x)])
}

.ascii_para_forma <- function(x) {
  x <- as.character(x)
  if (!length(x)) return(x)
  ascii <- !grepl("[^\\x01-\\x7f]", x, perl = TRUE, useBytes = TRUE)
  if (all(ascii)) return(x)
  # El `latin1` marcado, y el que llega SIN marca y no es UTF-8 valido -un CSV
  # de Windows leido bajo `C`-, se pasan a UTF-8 (ronda 36: `"Jos\xe9"` sin
  # marca quedaba `"JosA"` y no era un nombre de pila).
  latin <- !ascii & (Encoding(x) == "latin1" | !validUTF8(x))
  if (any(latin)) x[latin] <- iconv(x[latin], "latin1", "UTF-8", sub = "A")
  validos <- !ascii & !is.na(x) & validUTF8(x)
  if (any(validos)) {
    Encoding(x[validos]) <- "UTF-8"
    # Los signos de Unicode, como sus parecidos ASCII, con la regla de los
    # digitos: el espacio duro, la raya, el punto medio entre los grupos de un
    # telefono o entre las palabras de un nombre (ronda 36: `"Juan\u00a0Perez"`
    # quedaba `"JuanAAPerez"`).
    signos <- tryCatch(.normalizar_signos_digitos(x[validos]),
                       error = function(e) NULL)
    if (length(signos) == sum(validos)) {
      signos[is.na(signos)] <- x[validos][is.na(signos)]
      x[validos] <- gsub("\u00b7", ".", signos, fixed = TRUE)
    }
    x[validos] <- tryCatch(.transliterar_ascii(x[validos]),
                           error = function(e) x[validos])
  }
  gsub("[^\\x01-\\x7f]", "A", x, perl = TRUE, useBytes = TRUE)
}

# El tipo personal que los valores de una columna calificada muestran, o `NA`.
# En este orden, cada uno con su senal, en el 80 % de los valores distintos:
#
# - correo: un texto con una `@` entre dos partes sin espacios -tambien el de
#   una intranet, `"jperez@intranet"`-;
# - telefono: de siete a quince cifras, en una columna que no es de fechas y
#   sin centavos -`"12345.10"` es un importe-, escrito como telefono: con `+`,
#   con un cero inicial, con separadores entre las cifras o con un interno
#   (`"2901 1234 int 101"`). Con la palabra del telefono en el nombre
#   (`propios`), tambien las cifras peladas: `celular_estado` con `91234567`;
# - domicilio: una marca de calle -`av`, `calle`, `bvar`, `ruta`, `esq`,
#   `apto`...- en al menos el 30 % de los distintos, y la calle con su numero o
#   una marca en el 80 %: `"Av. Italia 2345"`, `"Rivera esq. Soca"`; no
#   `"Oficina 101"`;
# - nombre: un nombre de pila conocido (`.NOMBRES_DE_PILA`), sin cifras, sin una
#   palabra que no va en un nombre -`sin`, `en`, `por`- y sin empezar como un
#   lugar -`San`, `Villa`, `Puerto`-: `"Perez Joaquin"`, `"FERNANDEZ, MARIA"`;
#   no `"Gran Empresa"`, `"Sin Asignar"`, `"Arroz Premium"`. Desde un valor: dos
#   funcionarios son dos personas.
#
# Un documento lo confirma su verificador en el clasificador, no esto.
.tipo_personal_de_valores <- function(x, propios = character(),
                                      peladas = "telefono" %in% propios) {
  textos <- .distintos_forma_personal(x)
  if (is.null(textos) || !length(textos)) return(NA_character_)
  basta <- function(forma) {
    any(forma) && mean(forma) >= .PROPORCION_FORMA_PERSONAL
  }
  minusculas <- .minusculas_ascii(textos)
  # Una lista -dos telefonos o dos correos unidos por una barra, un punto y
  # coma o una barra vertical- tiene la forma si la tiene cada pieza.
  piezas <- strsplit(minusculas, "[[:space:]]*[/;|][[:space:]]*", perl = TRUE)
  en_cada_pieza <- function(forma) {
    vapply(piezas, function(p) {
      p <- p[nzchar(p)]
      length(p) > 0L && all(forma(p))
    }, logical(1L))
  }
  correo <- function(p) grepl("^[^@[:space:]]+@[^@[:space:]]+$", p, perl = TRUE)
  if (basta(en_cada_pieza(correo))) return("correo")
  if (!.columna_de_fechas(minusculas) &&
      basta(en_cada_pieza(function(p) {
        .forma_telefono(x, p, peladas = peladas)
      }))) {
    return("telefono")
  }
  if (.forma_domicilio(textos, minusculas)) return("domicilio")
  if (basta(.forma_nombre_de_persona(minusculas, textos))) return("nombre")
  NA_character_
}

# Si cada texto de `minusculas` tiene la forma de un telefono (ver
# `.tipo_personal_de_valores()`). Por valor; quien llama mira la columna.
.forma_telefono <- function(x, minusculas, peladas = FALSE) {
  # Un numero no es un telefono escrito: solo con la palabra del telefono
  # (`peladas`) o si es el de Uruguay con su prefijo pegado (`59899123456`,
  # ronda 37).
  if (is.numeric(x) && !peladas) {
    return(grepl(.PATRON_TELEFONO_UY_PEGADO, minusculas, perl = TRUE))
  }
  # El `.0` de planilla no es parte del numero (`"59899123456.0"`, ronda 36), ni
  # el `.0000` de un `DECIMAL(15,4)` (ronda 37), si antes no hay otro punto ni
  # otra coma: en `"098.765.000"` el `.000` es el ultimo grupo de un celular
  # escrito de a tres (ronda 38).
  texto <- sub("^([^.,]*)[.,]0+$", "\\1", minusculas, perl = TRUE)
  sin_interno <- sub(
    "[[:space:]]*(int|ext|interno|anexo|x)[.]?[[:space:]]*[0-9]{1,5}$", "",
    texto, perl = TRUE
  )
  interno <- sin_interno != texto
  limpio <- gsub("[ ().-]", "", sin_interno, perl = TRUE)
  cifras <- grepl("^[+]?[0-9]{7,15}$", limpio, perl = TRUE)
  # Lo que no es un telefono aunque tenga sus cifras:
  # - un importe: con signo menos (`"-12345.10"`, `"-1.250.000"`); con centavos
  #   -un solo separador y de una a cuatro cifras, o una parte decimal que
  #   termina en cero, `"12345.10"`, `"1234.5000"`-; con separadores de miles y
  #   centavos (`"12 345.50"`); o con separadores de miles (`"1.250.000"`,
  #   `"1 250 000"`; un telefono con grupos de tres empieza con cero);
  # - un codigo: con un ano entre guiones o barras (`"2024-00001"`) o con un
  #   grupo de cuatro cifras o mas rellenado con ceros despues de un guion, una
  #   barra o un punto (`"10-0012345"`, `"2024.00001"`);
  # - un documento escrito como documento (`"4.685.467-9"`), que confirma su
  #   verificador.
  # Ronda 37: los importes negativos, con cuatro decimales o con miles con
  # espacio, y los codigos con ano o con ceros, se tomaban por telefonos.
  # Ronda 38 (P-H2): un importe con separadores de miles no empieza con cero,
  # como en la forma de los miles de abajo, y separa los decimales con otro
  # signo que los miles -`"1.456.651,89"`, `"1,234.5000"`, `"12 345.50"`-: el
  # celular escrito de a tres con su cero, `"099.123.456"` o `"099 123.456"`, es
  # `"099"` y dos grupos, y el fijo `"4.732.1234"` un grupo de cuatro; la forma
  # de la ronda 37 los leia como importes con tres o cuatro decimales.
  importe <- grepl("^-", sin_interno, perl = TRUE) |
    grepl("^[0-9]+[.,][0-9]{1,2}$", sin_interno, perl = TRUE) |
    grepl("^[0-9]+[.,][0-9]*0$", sin_interno, perl = TRUE) |
    grepl(paste0(
      "^(?:(?:0|[1-9][0-9]{0,2})[.,]|",
      "[1-9][0-9]{0,2}(?:(?:[.][0-9]{3})+,|(?:,[0-9]{3})+[.]|",
      "(?: [0-9]{3})+[.,]))[0-9]{1,4}$"
    ), sin_interno, perl = TRUE)
  miles <- grepl("^[1-9][0-9]{0,2}(?:[., ][0-9]{3})+$", sin_interno,
                 perl = TRUE)
  # Con la palabra del telefono, el celular de Uruguay escrito de a tres sin su
  # cero (`"99.123.456"`, `"99 123 456"`) es un celular, no un importe.
  celular <- peladas & grepl("^9[1-9][., ][0-9]{3}[., ][0-9]{3}$", sin_interno,
                             perl = TRUE)
  miles <- miles & !celular
  importe <- importe & !celular
  codigo <- (grepl("[-/]", sin_interno, perl = TRUE) &
               grepl("(^|[-/])(19|20)[0-9]{2}([-/]|$)", sin_interno,
                     perl = TRUE)) |
    grepl("[-/.]00[0-9]{2,}", sin_interno, perl = TRUE)
  documento <- grepl(paste0(
    "^(?:[0-9]{1,2}\\.[0-9]{3}\\.[0-9]{3}-[0-9k]|",
    "[0-9]{2}(?:[ .][0-9]{3}){2}[ .][0-9]{4})$"
  ), sin_interno, perl = TRUE)
  # Ni una direccion IP: cuatro grupos de una a tres cifras hasta 255, sin cero
  # inicial (`"10.23.45.67"`; `"099.12.34.56"` es un celular).
  ip <- grepl("^(?:(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])[.]){3}(?:25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])$",
              sin_interno, perl = TRUE)
  # Y tres ceros no son un prefijo de nada (`"0001-2345"`).
  no_es <- importe | miles | codigo | documento | ip |
    grepl("^000", limpio, perl = TRUE)
  # Escrito como telefono: con `+`, con el prefijo internacional `00` o el de
  # Uruguay pegado (`"0059899123456"`, `"59899123456"`), con un cero inicial
  # -con las cifras pegadas, desde nueve: `"0123456"` es un codigo rellenado-,
  # con separadores entre las cifras o con un interno. Las cifras sueltas, solo
  # con la palabra del telefono (`peladas`), y no un numero redondo, que es un
  # importe (`1250000`).
  escrito <- interno |
    grepl("^([+]|00[1-9])", sin_interno, perl = TRUE) |
    (grepl("^0[1-9]", limpio, perl = TRUE) &
       nchar(limpio, type = "bytes") >= 9L) |
    grepl(.PATRON_TELEFONO_UY_PEGADO, limpio, perl = TRUE) |
    grepl("[0-9][ ().-]+[0-9]", sin_interno, perl = TRUE)
  suelto <- peladas & !grepl("000$", limpio, perl = TRUE)
  cifras & !no_es & (escrito | suelto)
}

# Las marcas de calle, como palabra: castellano, portugues, ingles y frances.
# Ronda 36: las abreviaturas corrientes en Uruguay y la region -`Br.`, `Cno.`,
# `Rbla.`, `Pje.`, `Gral.`, `Dpto.`, `Mza.`-. El `km` cuenta solo seguido de un
# numero (`"Ruta 8 km 23"`; `"Radio 5 km"` es una categoria).
.MARCAS_DE_CALLE <- c(
  "av", "avda", "avenida", "calle", "bv", "bvar", "br", "bulevar", "boulevard",
  "ruta", "camino", "cno", "pasaje", "psje", "pje", "rambla", "rbla", "esq",
  "esquina", "apto", "apartamento", "dpto", "depto", "gral", "manzana", "mza",
  "solar", "padron", "street", "road", "avenue", "rua", "rue"
)

.forma_domicilio <- function(textos, minusculas) {
  marca <- grepl(paste0("(^|[^a-z])(", paste(.MARCAS_DE_CALLE, collapse = "|"),
                        ")([^a-z]|$)"), minusculas, perl = TRUE) |
    grepl("(^|[^a-z])km[.]?[ ]*[0-9]", minusculas, perl = TRUE)
  numero <- grepl("[a-z]{3,}", minusculas, perl = TRUE) &
    grepl("(^|[^0-9])[0-9]{1,5}($|[^0-9])", minusculas, perl = TRUE) &
    grepl("[ ]", minusculas, perl = TRUE)
  # Un domicilio no empieza con una palabra de categoria: `"En ruta"`,
  # `"En camino"`, `"Sin calle"` son estados (ronda 36). Al principio y no en
  # cualquier lugar: `"Rivera entre Soca y Ellauri"`, `"Cno. El Dorado 123"`,
  # `"Calle A 1234"` son domicilios (ronda 37).
  palabras <- regmatches(minusculas, gregexpr("[a-z]+", minusculas, perl = TRUE))
  categoria <- vapply(palabras, function(p) {
    length(p) > 0L && p[[1L]] %in% c("en", "sin", "por", "para", "con", "no",
                                     "a", "al")
  }, logical(1L))
  marca <- marca & !categoria
  forma <- (marca | numero) & !categoria & !.forma_de_fecha(textos)
  any(forma) && mean(forma) >= .PROPORCION_FORMA_PERSONAL &&
    mean(marca) >= 0.3
}

# Las palabras que van en una categoria y no en el nombre de una persona, y las
# que empiezan el nombre de un lugar. Las particulas de un nombre -`de`, `del`,
# `la`, `y`, `da`, `dos`- no estan: `"Maria de los Campos"` es una persona.
.PALABRAS_QUE_NO_VAN_EN_UN_NOMBRE <- c(
  "sin", "con", "por", "para", "en", "el", "al", "no", "ni", "a", "o", "u",
  "un", "una", "uno", "otro", "otra", "otros", "otras", "sobre", "entre",
  "segun", "sem", "com", "em", "of", "the", "and", "or", "for", "with", "to",
  "in", "on", "at", "by", "et", "du", "des", "sur", "avec", "pour"
)
.PREFIJOS_DE_LUGAR <- c(
  "san", "santa", "santo", "sao", "villa", "puerto", "nueva", "nuevo", "cerro",
  "rio", "paso", "punta", "playa", "barrio", "parque", "colonia", "estacion",
  "pueblo", "ciudad", "arroyo", "laguna", "isla", "monte", "valle", "cabo",
  "bahia", "fray"
)

.forma_nombre_de_persona <- function(minusculas, textos = minusculas) {
  sin_iniciales <- .minusculas_ascii(.sin_iniciales(textos))
  palabras <- regmatches(sin_iniciales, gregexpr("[a-z]+", sin_iniciales,
                                                 perl = TRUE))
  sin_cifras <- !grepl("[0-9@]", minusculas, perl = TRUE)
  # Una frase escrita como frase -la primera palabra con mayuscula y otra
  # despues en minuscula, `"Dolores musculares agudos"`, `"Victoria local"`- no
  # es un nombre aunque empiece con uno (ronda 37), si ese nombre de pila es
  # tambien una palabra corriente (`.NOMBRES_QUE_SON_PALABRAS`) y no sigue otro.
  # Ronda 38 (P-H3): la ronda 37 lo decidia con cualquier nombre de pila, y
  # `"Juan perez"` o `"Juan carlos Perez"` -lo que deja el autocapitalizado de un
  # celular o de un formulario- dejaban de ser nombres.
  frase <- .es_frase(.sin_iniciales(textos))
  vapply(seq_along(minusculas), function(i) {
    p <- palabras[[i]]
    frase_inequivoca <- frase[[i]] && length(p) >= 2L &&
      p[[1L]] %in% .NOMBRES_QUE_SON_PALABRAS &&
      !any(p[-1L] %in% .NOMBRES_DE_PILA)
    sin_cifras[[i]] && !frase_inequivoca && length(p) >= 1L && length(p) <= 8L &&
      !p[[1L]] %in% .PREFIJOS_DE_LUGAR &&
      !any(p %in% .PALABRAS_QUE_NO_VAN_EN_UN_NOMBRE) &&
      any(p %in% .NOMBRES_DE_PILA)
  }, logical(1L))
}

# Las iniciales -`"Juan A. Perez"`, `"Juan M. A. Perez"`, `"Gomez, A."`- y el
# `O'` de un apellido -`"O'Connor"`, `"Maria E. O'Neill"`- fuera: no son la
# conjuncion `"o"` ni la preposicion `"a"`. Ronda 36 quitaba una sola inicial (la
# regla consumia el espacio que separaba la siguiente) y la regla de quien
# actua ninguna (ronda 37).
# Y la mayuscula sola entre palabras, sin punto -`"Juan A Perez"`-: en un texto
# en minusculas no se distingue de la preposicion, y queda.
# Ronda 39 (H-01): y la letra sola PEGADA a una palabra por un guion bajo, un
# guion o un punto, delante o detras -el login `a_perez`, `a-perez`,
# `perez_a`, `perez.a`, `A_PEREZ`-: una preposicion o una conjuncion va entre
# espacios, no pegada. Con el `j.perez` ya se sacaba; con el `_` y el `-` la
# `a`, la `o` o la `u` se leia como la palabra de `.PALABRAS_QUE_NO_VAN_EN_UN_NOMBRE`,
# el login contaba como un estado y, con un quinto de los nombres de pila que
# empiezan asi, la columna de quien actua no llegaba al 80 % en un tercio de las
# tablas y publicaba los logins de todos.
.sin_iniciales <- function(x) {
  x <- gsub("(?<![A-Za-z])[A-Za-z][.'](?=[ ,]|[A-Za-z]|$)", " ", x, perl = TRUE)
  x <- gsub("(?<![A-Za-z])[A-Za-z][_-](?=[A-Za-z])", " ", x, perl = TRUE)
  x <- gsub("(?<=[A-Za-z][._-])[A-Za-z](?![A-Za-z])", " ", x, perl = TRUE)
  gsub("(?<![A-Za-z'.])[A-Z](?![A-Za-z'.])", " ", x, perl = TRUE)
}

# Si cada texto es una frase: dos palabras o mas, la primera con mayuscula
# inicial y alguna de las siguientes, de tres letras o mas y que no es una
# particula de nombre, toda en minusculas.
.es_frase <- function(textos) {
  palabras <- regmatches(textos, gregexpr("[A-Za-z]+", textos, perl = TRUE))
  vapply(palabras, function(p) {
    if (length(p) < 2L || !grepl("^[A-Z]", p[[1L]], perl = TRUE)) return(FALSE)
    resto <- p[-1L]
    resto <- resto[nchar(resto, type = "bytes") >= 3L &
                     !.minusculas_ascii(resto) %in% .PARTICULAS_DE_NOMBRE]
    any(grepl("^[a-z]+$", resto, perl = TRUE))
  }, logical(1L))
}

.PARTICULAS_DE_NOMBRE <- c("de", "del", "la", "las", "los", "y", "e", "da",
                           "das", "do", "dos", "di", "van", "von", "der", "den",
                           "le")

# Los nombres de pila que empiezan tambien una frase corriente de una categoria:
# un sintoma (`"Dolores musculares agudos"`) o un resultado (`"Victoria
# local"`). Una lista CERRADA (ronda 38): con otro nombre de pila la frase sigue
# siendo un nombre (`"Juan perez"`), y una frase que empieza con otro
# -`"Mercedes centro"`- se lee como un nombre, como en la ronda 36.
.NOMBRES_QUE_SON_PALABRAS <- c("dolores", "victoria")

# Nombres de pila frecuentes en castellano, portugues, ingles, italiano, frances
# y aleman, sin acentos -se comparan transliterados-. No pretende ser completa:
# alcanza con que la mayoria de los valores de una columna de personas tenga
# uno, y un nombre que falte deja la columna como la publicaba la ronda 34. No
# estan los que son tambien palabras corrientes de una categoria -`paz`, `luz`,
# `mar`, `flor`, `esperanza`, `gloria`; y desde la ronda 36 `pilar`, `marco`,
# `norma`, `franco`, `domingo`, `julio`, `blanca`, `celeste`, `rosa` y `max`:
# `"Primer Pilar"`, `"Convenio Marco"`, `"Plan Max"` se tapaban-. Limitacion conocida: una localidad que
# lleva el nombre de una persona -`"Jose Ignacio"`, `"Juan Lacaze"`- se lee como
# un nombre.
.NOMBRES_DE_PILA <- c(
  # castellano y portugues
  "adolfo", "adrian", "adriana", "agustin", "agustina", "aida", "alan",
  "alba", "alberto", "alejandra", "alejandro", "alexis", "alfonso", "alfredo",
  "alicia", "alvaro", "amalia", "ana", "analia", "andrea", "andres", "angel",
  "angela", "angeles", "antonella", "antonia", "antonio", "araceli", "ariel",
  "armando", "arturo", "aurora", "bautista", "beatriz", "belen", "benjamin",
  "bernabe", "bernardo", "brenda", "bruno", "camila", "camilo",
  "candela", "carina", "carla", "carlos", "carmen", "carolina", "catalina",
  "cecilia", "cesar", "claudia", "claudio", "clara", "cristian",
  "cristina", "cristobal", "damian", "daniel", "daniela", "dario", "david",
  "delfina", "diana", "diego", "dolores", "eduardo", "elena",
  "elisa", "eliana", "emilia", "emiliano", "emilio", "enrique", "ernesto",
  "esteban", "estela", "estefania", "eugenia", "eva", "ezequiel", "fabian",
  "fabiana", "facundo", "federico", "felipe", "fernanda", "fernando", "florencia",
  "francisco", "gabriel", "gabriela", "gaston", "german", "gerardo",
  "gimena", "gonzalo", "graciela", "guadalupe", "guillermo", "gustavo",
  "hector", "horacio", "hugo", "ignacio", "ines", "irene", "isabel", "ismael",
  "ivan", "jaime", "javier", "jesus", "jimena", "joaquin", "joao", "jonathan",
  "jorge", "jose", "josefina", "juan", "juana", "julia", "julian", "juliana",
  "karina", "laura", "lautaro", "leandro", "leonardo", "leonel",
  "leticia", "lorena", "lorenzo", "lourdes", "lucas", "lucia", "luciana",
  "luciano", "lucila", "luis", "luisa", "macarena", "magdalena", "maite",
  "manuel", "manuela", "marcela", "marcelo", "marcos", "margarita", "maria",
  "mariana", "mariano", "maribel", "marina", "mario", "marta", "martin",
  "martina", "mateo", "matias", "mauricio", "maximiliano", "mercedes",
  "micaela", "miguel", "milagros", "miriam", "monica", "nadia", "natalia",
  "nicolas", "noelia", "nora", "octavio", "olga", "omar", "oscar",
  "osvaldo", "pablo", "pamela", "patricia", "patricio", "paula", "paulo",
  "pedro", "rafael", "ramiro", "ramon", "raquel", "raul", "renata",
  "renzo", "ricardo", "roberto", "rocio", "rodrigo", "romina",
  "rosario", "ruben", "sabrina", "salvador", "samuel", "sandra", "santiago",
  "sara", "sebastian", "sergio", "silvana", "silvia", "simon", "sofia",
  "sol", "soledad", "susana", "tatiana", "teresa", "thiago", "tomas",
  "valentin", "valentina", "valeria", "vanesa", "vanessa", "veronica",
  "victor", "victoria", "virginia", "viviana", "walter", "washington",
  "wilson", "ximena", "yolanda",
  # Ronda 37: nombres corrientes en Uruguay y la region que faltaban, y los que
  # terminan como un estado (`Vicente`, `Clemente`, `Dante`, `Conrado`): una
  # palabra de la lista nunca es un estado por su forma.
  "alcides", "bryan", "clemente", "conrado", "danilo", "dante", "dinorah",
  "elbio", "gisela", "gladys", "heber", "hebert", "jhonatan", "lilian",
  "mabel", "maicol", "milton", "mirta", "nelly", "nelson", "nestor", "nilda",
  "shirley", "ulises", "vicente", "wilmar", "yamandu", "yanina",
  # ingles
  "anne", "barbara", "betty", "brian", "carol", "charles", "christopher",
  "donald", "donna", "edward", "elizabeth", "emily", "emma", "frank", "george",
  "helen", "henry", "jack", "james", "jennifer", "jessica", "john", "joseph",
  "karen", "kevin", "linda", "lisa", "mark", "mary", "matthew", "michael",
  "michelle", "nancy", "olivia", "paul", "peter", "richard", "robert", "ruth",
  "sarah", "sharon", "steven", "susan", "thomas", "william",
  # italiano, frances y aleman
  "alessandro", "chiara", "francesca", "francesco", "giovanni", "giulia",
  "giuseppe", "luca", "paolo", "stefano", "camille", "claire",
  "jacques", "jean", "julie", "louis", "marie", "michel", "nathalie",
  "philippe", "pierre", "sophie", "hans", "heidi", "ingrid", "karl", "klaus",
  "monika", "petra", "sabine", "ursula", "wolfgang", "mia", "zoe"
)

.clasificar_dato_personal <- function(x, nombre, inferencia,
                                      validadores = list(),
                                      umbral_verificado = 0.9,
                                      muestra_validadores = 1000L) {
  if (.es_columna_compuesta(x)) {
    return(list(
      tipo = NA_character_, proporcion = NA_real_, fundamento = "",
      poder_discriminante = NA_character_, proteger = FALSE
    ))
  }
  # `NombreCliente` y `nroDoc` son `nombre_cliente` y `nro_doc` (ronda 37), y
  # `C.I.` es `ci` (ronda 39, `.juntar_siglas_de_documento()`).
  normalizado <- .normalizar_nombre_fecha(.separar_camel(
    .juntar_siglas_de_documento(.nombre_para_clasificar(nombre))
  ))
  # Ronda 37 (P-A1): las abreviaturas comunes de cada palabra -`tel`, `cel`,
  # `telef`, `fono`, `dir`, `domic`, `nom`, `ape`, `doc`, `ced`-, sus plurales y
  # un numero pegado -`telefono2`, `apellido1`-. `tel`, `nro_tel` o `cel` con
  # celulares se clasificaban documento `debil` y publicaban. No `mov`, que es
  # tambien un movimiento.
  # Ronda 38 (B1): el plural de la abreviatura -`tels`, `cels`, `fonos`,
  # `noms`, `dirs`- y las formas pegadas de los sistemas heredados -`nrotel`,
  # `TELCEL`, `nomape`, `NOMBRECOMPLETO`, `nrodoc`, `dirpart`-, enumeradas como
  # `fechanac`: palabras enteras, no una regla por forma.
  reglas_nombre <- c(
    documento_identidad = paste0(
      "(^|_)(cedula|cedulas|ced|ceds|",
      paste(.SIGLAS_DE_DOCUMENTO, collapse = "|"), "|documento|documentos|doc|",
      "docs|documento_identidad|numero_documento|nro_documento|",
      "nrodoc|numdoc|nrodocumento|numdocumento|nrocedula|numcedula|nroci|",
      "identificacion|id_nacional|pasaporte|passport)[0-9]*($|_)"
    ),
    correo = "(^|_)(correo|correos|email|emails|mail|mails|casilla)[0-9]*($|_)",
    telefono = paste0(
      "(^|_)(telefono|telefonos|tel|tels|telf|telfs|tlf|tfno|telef|fono|",
      "fonos|celular|celulares|cel|cels|movil|moviles|contacto|",
      "nrotel|numtel|nrotelefono|numtelefono|nrocel|numcel|nrocelular|",
      "telcel|telcelular|telefonocelular|telefonomovil)[0-9]*($|_)"
    ),
    # El patron abreviaba la PRIMERA palabra (`f_nacimiento`) y no la segunda,
    # asi que `fecha_nac` -de las formas mas frecuentes en registros
    # administrativos de la region- no se clasificaba y la fecha de nacimiento
    # quedaba sin proteger. Se enumeraron las cuatro formas de abreviar en vez
    # de agregar la que aparecio: entera, primera palabra, segunda palabra, las
    # dos, y pegada. Con dos casos parecia una convencion; con cinco era un
    # hueco.
    #
    # Ronda 39 (H-17): y la fecha de la nomenclatura GIK de GeneXus -objeto,
    # categoria y calificador, con la categoria fecha abreviada `Fch`-, con la
    # que estan hechos buena parte de los sistemas del Estado uruguayo:
    # `PerFchNac`, `CliFchNac`, `FchNac` (`fch_nac` al separar el camelCase), y
    # `FNac` (`fnac`). Publicaban la moda y los extremos de la fecha de
    # nacimiento en memoria y en los tres motores. `fch_nacimiento` ya lo
    # reconocia `nacimiento`.
    fecha_nacimiento = paste0(
      "(^|_)(fecha_nacimiento|f_nacimiento|fec_nacimiento|nacimiento|",
      "fecha_nac|f_nac|fec_nac|fch_nac|fechanac|fnac)($|_)"
    ),
    # La hermana tenia el mismo hueco por el mismo lado. No se agrega
    # `fecha_fall`: `fall` es tambien "falla" en datos de mantenimiento, y una
    # clasificacion de mas enmascara una columna que no es personal.
    # Ronda 39 (H-17): en GIK, el calificador de la categoria `Fch` es el
    # hecho de la fecha, abreviado: `PerFchDef`, `PerFchFall` (`fch_def`,
    # `fch_fall`). El riesgo que dejo afuera a `fecha_fall` -la falla de un
    # equipo- queda acotado a la palabra entera tras `fch`: `FchFalla`,
    # `FchFal` y `FchDefinitiva` siguen siendo fechas que no son personales.
    # Limitacion (declarada en la ayuda): un `FchFall` que sea la fecha de una
    # falla, o un `FchDef` que sea una fecha definitiva, se protege de mas.
    fecha_fallecimiento = paste0(
      "(^|_)(fallecimiento|defuncion|deceso|obito|fecha_muerte|",
      "f_fallecimiento|fecha_defuncion|f_defuncion|fec_defuncion|",
      "fecha_deceso|fch_def|fch_fall)($|_)"
    ),
    # El lexico de nombres no puede ser completo: una columna se puede llamar
    # de cualquier forma. Cubre las mas frecuentes en registros administrativos
    # y `columnas_personales` esta para el resto, que lo declara quien conoce
    # el dato. Las palabras de PERSONA -`cliente`, `socio`- van aparte: ver
    # `.PATRON_PERSONA_EN_NOMBRE`.
    nombre = paste0(
      "(^|_)(nombre|nombres|apellido|apellidos|nombre_completo|nom|noms|ape|",
      "nomape|apenom|nombrecompleto|nombreapellido|nombreyapellido|",
      "nombresapellidos|apellidonombre|apellidoynombre|apellidosnombres)",
      "[0-9]*($|_)"
    ),
    domicilio = paste0(
      "(^|_)(direccion|direcciones|direc|dir|dirs|domicilio|domicilios|domic|",
      "dirpart|dirparticular|direccionparticular|direccionpostal|",
      "domicilioparticular|domiciliolegal|",
      "calle|address|residencia|lugar_residencia|barrio)[0-9]*($|_)"
    )
  )
  palabras <- lapply(names(reglas_nombre), function(tipo) {
    .palabra_de_tipo(normalizado, reglas_nombre[[tipo]], tipo, inferencia$tipo,
                     x = x)
  })
  names(palabras) <- names(reglas_nombre)
  estados <- vapply(palabras, `[[`, character(1L), "estado")
  por_nombre <- names(estados)[estados == "si"]
  persona <- .palabra_de_tipo(normalizado, .PATRON_PERSONA_EN_NOMBRE, "persona",
                              inferencia$tipo)
  if (!length(por_nombre) && identical(persona$estado, "si")) {
    por_nombre <- "nombre"
  }
  # Ronda 35 (H3): la palabra calificada por el resto del nombre se decide con
  # los valores. Ver `.tipo_por_valores_calificada()`.
  if (!length(por_nombre)) {
    por_nombre <- .tipo_por_valores_calificada(x, palabras, persona, normalizado)
  }
  textos <- .textos_para_clasificacion_personal(x)
  presentes <- !is.na(textos) & nzchar(textos)
  proporcion_correo <- if ("correo" %in% por_nombre ||
    any(grepl("@", textos[presentes], fixed = TRUE, useBytes = TRUE))) {
    if (any(presentes)) mean(validar_correo(textos[presentes])) else NA_real_
  } else NA_real_
  # Un correo escrito `usuario at dominio punto com` no es un correo valido y
  # `validar_correo()` tiene razon en decir que no lo es: eso es una medida de
  # formato. Pero la columna sigue trayendo direcciones de personas, y esa es
  # una pregunta distinta. Las dos respuestas conviven: el validador mide la
  # forma, el clasificador decide si hay dato personal.
  proporcion_correo_ofuscado <- if (any(presentes)) {
    mean(grepl(
      .regex_sin_caja(paste0(
        "^[A-Za-z0-9._%+-]+[[:space:]]*(?:\\(|\\[)?(?:at|arroba)(?:\\)|\\])?",
        "[[:space:]]*[A-Za-z0-9-]+",
        # El dominio tiene que traer su separador: sin el, `lunes at casa` es
        # una frase y no una direccion, y protegerla seria inventar el dato.
        "(?:[[:space:]]*(?:\\(|\\[)?(?:dot|punto)(?:\\)|\\])?[[:space:]]*|\\.)",
        "[A-Za-z]{2,}$"
      )),
      textos[presentes], perl = TRUE
    ))
  } else NA_real_
  textos_presentes <- textos[presentes]
  longitudes_crudas <- nchar(textos_presentes, type = "chars")
  composicion_documental <- if (length(textos_presentes)) {
    mean(grepl("^[0-9 .-]+$", textos_presentes, perl = TRUE)) >= 0.8
  } else FALSE
  cerca_de_forma_documento <- length(longitudes_crudas) &&
    mean(longitudes_crudas >= 7L & longitudes_crudas <= 20L) >= 0.8 &&
    composicion_documental
  longitudes <- if (cerca_de_forma_documento) {
    nchar(gsub("[^0-9]", "", textos_presentes, perl = TRUE), type = "chars")
  } else integer()
  parece_fecha <- inferencia$tipo %in% c("fecha", "fecha-hora")
  forma_documento_posible <- length(longitudes) &&
    mean(longitudes >= 7L & longitudes <= 12L) >= 0.8 &&
    (!parece_fecha || "documento_identidad" %in% por_nombre)
  proporcion_documento <- if (
      "documento_identidad" %in% por_nombre || forma_documento_posible) {
    .proporcion_compatible(
      x,
      # Los dos puntos de miles van juntos o no va ninguno: con un solo punto
      # opcional, un p-valor de siete decimales -"0.4649203"- tenia forma de
      # cedula, y la columna se clasificaba documento y perdia la evidencia de
      # sus hallazgos. Medido en una refutacion.
      paste0(
        "^(?:[0-9]{7,12}|",
        "[0-9]{1,2}(?:\\.[0-9]{3}\\.[0-9]{3}|[0-9]{6})-?[0-9Kk]|",
        "[0-9]{2}(?:[ .][0-9]{3}){2}[ .][0-9]{4})$"
      )
    )
  } else NA_real_
  # Un entero de ocho digitos y una cedula escrita con puntos y guion caen los
  # dos en la forma de documento, y no son la misma evidencia. Un importe, un
  # numero de factura o un identificador de transaccion son enteros de ese
  # largo; nadie escribe un importe como `5.836.595-5`. La separacion importa
  # porque decide si, ante un validador que no verifica, corresponde proteger.
  proporcion_documento_formateado <- if (is.finite(proporcion_documento)) {
    .proporcion_compatible(
      x,
      paste0(
        "^(?:[0-9]{1,2}\\.[0-9]{3}\\.[0-9]{3}-[0-9Kk]|",
        "[0-9]{2}(?:[ .][0-9]{3}){2}[ .][0-9]{4})$"
      )
    )
  } else NA_real_
  documentos_distintos <- if (is.finite(proporcion_documento)) {
    length(unique(gsub("[^0-9]", "", textos_presentes, perl = TRUE)))
  } else 0L
  proporciones_validadores <- if (is.finite(proporcion_documento) &&
      documentos_distintos >= 3L &&
      (!length(por_nombre) || "documento_identidad" %in% por_nombre)) {
    .proporcion_validadores(
      textos_presentes, validadores, umbral_verificado,
      muestra = muestra_validadores
    )
  } else numeric()
  indice_validador <- if (length(proporciones_validadores)) {
    which.max(proporciones_validadores)
  } else integer()
  proporcion_verificada <- if (length(indice_validador)) {
    proporciones_validadores[[indice_validador]]
  } else NA_real_
  documento_verificado <- documentos_distintos >= 3L &&
    is.finite(proporcion_verificada) &&
    proporcion_verificada >= umbral_verificado

  tipo <- NA_character_
  proporcion <- NA_real_
  fundamento <- ""
  poder <- NA_character_
  proteger <- FALSE
  if (is.finite(proporcion_correo) && proporcion_correo >= 0.8) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "forma de correo dominante"
    poder <- "alto"
    proteger <- TRUE
  } else if ("correo" %in% por_nombre) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "nombre de columna"
    poder <- "medio"
    proteger <- TRUE
  } else if (is.finite(proporcion_correo_ofuscado) &&
             proporcion_correo_ofuscado >= 0.8) {
    tipo <- "correo"
    proporcion <- proporcion_correo_ofuscado
    fundamento <- "forma de correo ofuscada dominante"
    poder <- "debil"
    proteger <- TRUE
  } else if (length(por_nombre)) {
    tipo <- por_nombre[[1L]]
    proporcion <- switch(
      tipo,
      documento_identidad = proporcion_documento,
      fecha_nacimiento = if (inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_,
      fecha_fallecimiento = if (inferencia$tipo %in% c("fecha", "fecha-hora")) 1 else NA_real_,
      NA_real_
    )
    fundamento <- if (tipo == "documento_identidad" &&
                      is.finite(proporcion_documento)) {
      "nombre de columna y forma compatible"
    } else "nombre de columna"
    poder <- if (tipo == "documento_identidad") "alto" else "medio"
    proteger <- TRUE
    if (tipo == "documento_identidad" && documento_verificado) {
      fundamento <- paste0(
        "nombre de columna y forma verificada por ",
        names(proporciones_validadores)[[indice_validador]],
        .alcance_validacion(proporciones_validadores, indice_validador)
      )
      poder <- "verificado"
    }
  } else if (is.finite(proporcion_documento) &&
             proporcion_documento >= 0.8) {
    tipo <- "documento_identidad"
    proporcion <- proporcion_documento
    if (documento_verificado) {
      fundamento <- paste0(
        "forma verificada por ",
        names(proporciones_validadores)[[indice_validador]],
        .alcance_validacion(proporciones_validadores, indice_validador)
      )
      poder <- "verificado"
      proteger <- TRUE
    } else if (isTRUE(proporcion_documento_formateado >= 0.8)) {
      # Escritos como documento y sin verificar. Es el caso de una base sucia
      # —documentos mal cargados, con digito verificador equivocado—, que es la
      # poblacion para la que existe el paquete. Ante la duda se protege, y sin
      # exigir una cantidad minima de valores distintos: una columna con un solo
      # documento repetido no identifica a nadie DENTRO de la tabla, pero si
      # identifica a una persona fuera de ella, y el hallazgo que la nombra
      # —que la columna es constante— no necesita mostrar cual es el valor.
      # La evidencia sigue declarandose debil, que es lo que es.
      fundamento <- "forma de documento con separadores, sin verificar"
      poder <- "debil"
      proteger <- TRUE
    } else {
      # Digitos pelados. Aca la forma no distingue un documento de un importe,
      # un numero de factura o un identificador de transaccion, y proteger por
      # esa sola coincidencia le sacaria los estadisticos a media tabla. La
      # sospecha se declara y no suprime nada.
      fundamento <- "forma de documento dominante"
      poder <- "debil"
      proteger <- FALSE
    }
  }
  # Un correo no necesita ser mayoria para ser un correo. La rama de arriba pide
  # que lo sean el 80 % de los valores -una columna de correos-, y una columna de
  # nombre neutro con 70 correos y 30 textos se perfilaba sin proteger: los
  # ejemplos de sus patrones publicaban las direcciones enteras. El paquete ya
  # dice por que un correo se protege por su forma y un numero de ocho digitos no:
  # "su forma no deja dudas". Eso vale para cada valor, no para la mayoria. Es un
  # piso que falla cerrado: se aplica solo si nada de lo anterior protegio.
  if (!proteger && is.finite(proporcion_correo) && proporcion_correo > 0) {
    tipo <- "correo"
    proporcion <- proporcion_correo
    fundamento <- "valores con forma de correo, aunque no dominen la columna"
    poder <- "medio"
    proteger <- TRUE
  }
  list(
    tipo = tipo, proporcion = proporcion,
    # Sobre cuantos valores se calculo esa proporcion. Un blanco no tiene forma
    # que comparar contra un patron y por eso no entra al denominador, pero eso
    # se dice en vez de suponerse: ocho documentos y treinta y dos blancos
    # publicaban "100 % compatible" sin decir que era sobre ocho de cuarenta.
    valores_evaluados = as.integer(
      attr(proporcion, "evaluados", exact = TRUE) %||% NA_integer_
    ),
    valores_totales = as.integer(
      attr(proporcion, "totales", exact = TRUE) %||% NA_integer_
    ),
    fundamento = fundamento,
    poder_discriminante = poder, proteger = proteger,
    # La proporcion que decide `verificado` viajaba solo en la decision: una
    # columna con el 90 % de los documentos validos -el umbral- publicaba
    # exactamente lo mismo que una con el 100 %. El paquete ya se escribio la
    # regla en otra guarda: "el motivo tiene que nombrar el numero que decidio,
    # y decir de donde sale".
    proporcion_verificada = if (is.finite(proporcion_verificada)) {
      as.numeric(proporcion_verificada)
    } else {
      NA_real_
    }
  )
}

# Los tipos que el paquete sabe nombrar. Declarar uno de estos hace que la
# columna se trate igual que si el lexico la hubiera reconocido; declarar otro
# nombre tambien vale, y viaja tal cual, porque el usuario puede conocer una
# categoria que el paquete no tiene.
# No hay lista cerrada a proposito: hubo una y no la miraba nadie, que es peor
# que no tenerla porque se lee como una lista blanca que no existe.

# El lexico de nombres de columna no puede ser completo: una columna con
# documentos se puede llamar `cod_benef` y ninguna lista de nombres la va a
# reconocer. `columnas_personales` es la salida correcta a ese limite, y es el
# mismo patron que `columnas_opcionales`: lo declara quien conoce el dato, y lo
# declarado gana sobre lo inferido.
.normalizar_columnas_personales <- function(columnas_personales, nombres) {
  if (is.null(columnas_personales)) return(character())
  if (!is.character(columnas_personales) || anyNA(columnas_personales)) {
    .detener(
      "`columnas_personales` debe ser un vector de texto: nombres de columna, ",
      "o un vector con nombre donde el nombre es la columna y el valor es el ",
      "tipo de dato personal.", call. = FALSE
    )
  }
  if (!length(columnas_personales)) return(character())
  etiquetas <- names(columnas_personales)
  declaradas <- if (is.null(etiquetas) || !all(nzchar(etiquetas))) {
    if (!is.null(etiquetas) && any(nzchar(etiquetas))) {
      .detener(
        "`columnas_personales` mezcla elementos con nombre y sin nombre. ",
        "Corresponde una sola forma: o todos nombres de columna, o todos ",
        "`columna = \"tipo\"`.", call. = FALSE
      )
    }
    stats::setNames(rep("declarado", length(columnas_personales)),
                    columnas_personales)
  } else {
    stats::setNames(as.character(columnas_personales), etiquetas)
  }
  if (any(!nzchar(names(declaradas)))) {
    .detener("`columnas_personales` nombra una columna vac\u00eda.", call. = FALSE)
  }
  if (anyDuplicated(.nombres_para_operar(names(declaradas)))) {
    .detener("`columnas_personales` repite una columna.", call. = FALSE)
  }
  # `nombres = NULL` significa "no se puede saber que columnas hay". Lo usa
  # medir(), donde la entrada puede ser una coleccion o una conexion y no hay
  # una lista de columnas que mirar. Con NULL se valida la forma y no la
  # existencia: es preferible a rechazar una declaracion valida.
  indices <- if (is.null(nombres)) integer() else {
    .indice_nombre(names(declaradas), nombres)
  }
  desconocidas <- if (is.null(nombres)) character() else {
    names(declaradas)[is.na(indices)]
  }
  if (length(desconocidas)) {
    .detener("`columnas_personales` nombra columnas inexistentes: ",
         paste(desconocidas, collapse = ", "), ".", call. = FALSE)
  }
  vacios <- names(declaradas)[!nzchar(declaradas)]
  if (length(vacios)) {
    .detener("`columnas_personales` declara un tipo vac\u00edo en: ",
         paste(vacios, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(nombres)) {
    names(declaradas) <- nombres[indices]
  }
  declaradas
}

.detectar_datos_personales <- function(datos, nombres, resultados,
                                       validadores = list(),
                                       umbral_verificado = 0.9,
                                       muestra_validadores = 1000L,
                                       declaradas = character()) {
  filas <- lapply(seq_along(datos), function(i) {
    indice_declarada <- .indice_nombre(nombres[[i]], names(declaradas))
    if (!is.na(indice_declarada)) {
      # Lo declarado no se vuelve a inferir. El paquete no tiene con que
      # contradecir a quien conoce el dato, y una columna declarada personal
      # que el lexico no reconoce es justamente el caso que esto resuelve.
      return(data.frame(
        columna = nombres[[i]],
        tipo = unname(declaradas[[indice_declarada]]),
        proporcion_compatible = NA_real_,
        valores_evaluados = NA_integer_,
        valores_totales = NA_integer_,
        fundamento = "declarado con `columnas_personales`",
        poder_discriminante = "declarado",
        proporcion_verificada = NA_real_,
        proteger = TRUE,
        stringsAsFactors = FALSE
      ))
    }
    clasificacion <- .clasificar_dato_personal(
      datos[[i]], nombres[[i]], resultados[[i]]$inferencia,
      validadores = validadores, umbral_verificado = umbral_verificado,
      muestra_validadores = muestra_validadores
    )
    if (is.na(clasificacion$tipo)) return(NULL)
    data.frame(
      columna = nombres[[i]],
      tipo = clasificacion$tipo,
      proporcion_compatible = as.numeric(clasificacion$proporcion),
      proporcion_verificada = as.numeric(
        clasificacion$proporcion_verificada %||% NA_real_
      ),
      valores_evaluados = clasificacion$valores_evaluados %||% NA_integer_,
      valores_totales = clasificacion$valores_totales %||% NA_integer_,
      fundamento = clasificacion$fundamento,
      poder_discriminante = clasificacion$poder_discriminante,
      proteger = clasificacion$proteger,
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1L))]
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo = character(),
    proporcion_compatible = numeric(), proporcion_verificada = numeric(),
    valores_evaluados = integer(), valores_totales = integer(),
    fundamento = character(),
    poder_discriminante = character(), proteger = logical(),
    stringsAsFactors = FALSE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("clasificacion_datos_personales", "data.frame")
  # Y la columna que copia los documentos protegidos sin su verificador: ver
  # `.copias_sin_verificador()`. Ronda 34.
  .reclasificar_copias_sin_verificador(resultado, datos, nombres,
                                       umbral = umbral_verificado)
}

# Ronda 34 (H3, por columna). Una columna de nombre neutro con la cedula
# protegida de cada fila SIN su verificador -el entero `4685467` frente a
# `4.685.467-9`- publicaba sus extremos, que son dos cedulas, en `perfilar()`,
# en `perfil_muestra` de `perfilar_dbi()` y en los cuantiles de `analizar()`.
# Se clasificaba `documento_identidad` debil: siete cifras pelados no se
# distinguen de un importe. Pero esta columna no es un importe: sus valores son
# los documentos de otra columna protegida, y eso se puede medir. Se protege
# ENTERA, como una columna personal, la que cumple las tres:
#
# - al menos tres de sus valores distintos son el documento sin su verificador
#   de un valor protegido -con la regla de los textos, `.prefijo_documento()`-;
# - son al menos `umbral` de sus valores distintos -el mismo 90 % que decide
#   `verificado`: tolera tipeos aislados-;
# - y no es azar: con la densidad de esos documentos en el rango de la columna
#   -la chance de que un entero cualquiera de ese rango coincida con uno-, que
#   tantos coincidan tiene probabilidad binomial menor que `cola`.
#
# La tercera es la que separa: con millones de cedulas protegidas, un importe
# de siete cifras coincide por azar con una de cada cinco, o con la mitad, y una
# columna de pocos importes llegaba al 90 % por azar. Medido en
# `data-raw/medir_tapado_de_mas.R`. Se probo antes la otra forma -el parcial
# valor por valor en los extremos de cualquier columna- y tapaba el minimo o el
# maximo de uno de cada cinco importes: es diagnostico de rango que se pierde.
# Con la regla por columna, una columna que coincide por azar publica sus
# extremos aunque uno de ellos coincida.

# Las cifras de cada celda de `x` -un numero entero, o un texto de digitos y
# separadores como los de un documento-, sin ceros iniciales; `NA` para la que
# no es eso. `NULL` si la columna no puede llevar documentos.
#
# Ronda 35 (H2). Un texto que el paquete lee como NUMERO da las cifras de ese
# numero, con la misma lectura que el resumen (`.leer_numeros_texto()`): la
# columna de cedulas sin verificador que paso por una planilla -`"4685467.0"`,
# `"4685467,00"`, `"4.685.467,00"`- se leia aca como `46854670` y en el resumen
# como el numero `4685467`, que publicaba como su minimo. Una sola lectura: el
# numero que el perfil publica es el que se compara. Un texto con decimales que
# no son cero no es un documento (`NA`); el que no se lee como numero -con
# guion, con espacios, `"4.685.467-9"`- da sus digitos, sin una parte decimal
# de ceros.
.cifras_de_columna <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "integer64")) {
    x <- tryCatch(as.character(x), error = function(e) NULL)
    if (is.null(x)) return(NULL)
  }
  if (is.list(x) || inherits(x, c("Date", "POSIXt", "difftime"))) return(NULL)
  if (is.numeric(x)) return(.cifras_de_numeros(x))
  if (!is.character(x)) return(NULL)
  t <- .recortar_bytes(x)
  salida <- rep(NA_character_, length(t))
  # Lo que la lectura numerica del paquete lee: con punto o coma, con signo, en
  # notacion cientifica (`"1019433e0"`, `"+2499716.0"`; segunda refutacion de la
  # ronda 35). Un numero con decimales que no son cero no es un documento.
  # Sin separador, signo ni exponente las cifras ya son el numero, y una cedula
  # con guion no se lee: leer solo estos le ahorra la lectura entera a una
  # columna protegida de un millon de documentos.
  numericos <- !is.na(t) & grepl(
    "^[+-]?[0-9]+(?:[.,][0-9]+)*(?:[eE][+-]?[0-9]+)?$", t, perl = TRUE,
    useBytes = TRUE
  ) & !grepl("^[0-9]+$", t, perl = TRUE, useBytes = TRUE)
  leidos <- rep(FALSE, length(t))
  if (any(numericos)) {
    numeros <- suppressWarnings(.leer_numeros_texto(t[numericos]))
    # La notacion cientifica con punto que la convencion de la columna no
    # decide (`"1019433.0e0"` junto a `"4.685.467,00"`) la lee `as.numeric()`,
    # como la inferencia.
    exponente <- is.na(numeros) &
      grepl("^[^,]*[eE]", t[numericos], perl = TRUE, useBytes = TRUE)
    numeros[exponente] <- suppressWarnings(as.numeric(t[numericos][exponente]))
    leidos[numericos] <- !is.na(numeros)
    salida[numericos][!is.na(numeros)] <-
      .cifras_de_numeros(numeros[!is.na(numeros)])
  }
  # El resto con forma de documento -digitos y separadores, `"4.685.467-9"`,
  # `"4 685 467"`- da sus digitos. Si termina en una parte decimal -una o dos
  # cifras tras el ultimo punto o coma-, la de ceros se quita (`"2 499 716.0"`,
  # que la lectura numerica no lee por los espacios) y la que no es cero dice
  # que no es un documento (`"24 997.16"` es un importe): pegar los centavos
  # inventaba una cifra que podia ser una cedula.
  escritos <- !is.na(t) & !leidos &
    grepl("^[0-9][0-9 .,/'_-]*[0-9]$", t, useBytes = TRUE)
  if (any(escritos)) {
    texto <- t[escritos]
    decimal <- grepl("[0-9][.,][0-9]{1,2}$", texto, perl = TRUE)
    ceros <- decimal & grepl("[.,]0{1,2}$", texto, perl = TRUE)
    texto[ceros] <- sub("[.,]0{1,2}$", "", texto[ceros], perl = TRUE)
    cifras <- .sin_ceros_iniciales(gsub("[^0-9]", "", texto, useBytes = TRUE))
    cifras[decimal & !ceros] <- NA_character_
    salida[escritos] <- cifras
  }
  salida
}

# Las cifras de cada numero entero, sin ceros iniciales ni signo; `NA` para el
# que no es entero. La comparten la columna numerica y el texto que se lee como
# numero (`.cifras_de_columna()`).
.cifras_de_numeros <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  enteros <- !is.na(v) & is.finite(v) & v == round(v) & abs(v) < 1e15
  salida <- rep(NA_character_, length(v))
  salida[enteros] <- .sin_ceros_iniciales(
    formatC(abs(v[enteros]), format = "f", digits = 0)
  )
  salida
}

# La prueba de una columna -sus valores distintos `distintos`, sus cifras
# `cifras`- contra los documentos sin verificador `prefijos` (ordenados, como
# numeros). Devuelve los conteos y si la columna es una copia.
.prueba_copia_sin_verificador <- function(distintos, cifras, prefijos,
                                          umbral = 0.9, cola = 1e-6) {
  n <- length(distintos)
  candidatas <- unique(cifras[!is.na(cifras) &
                                nchar(cifras, type = "bytes") >=
                                  .MIN_LARGO_COPIA_SIN_VERIFICADOR &
                                !grepl(.PATRON_NUMERO_REDONDO, cifras,
                                       perl = TRUE)])
  # Por busqueda en los ordenados, no con `%in%`: armar la tabla de millones de
  # documentos una vez por columna costaba mas que la columna.
  numeros <- as.numeric(candidatas)
  posicion <- findInterval(numeros, prefijos)
  k <- sum(posicion > 0L & prefijos[pmax(posicion, 1L)] == numeros)
  densidad <- NA_real_
  probabilidad <- NA_real_
  copia <- FALSE
  if (n > 0L && k >= 3L && k / n >= umbral) {
    desde <- min(numeros)
    hasta <- max(numeros)
    en_rango <- findInterval(hasta, prefijos) -
      findInterval(desde - 1, prefijos)
    densidad <- min(1, en_rango / (hasta - desde + 1))
    probabilidad <- stats::pbinom(k - 1L, n, densidad, lower.tail = FALSE)
    copia <- isTRUE(probabilidad <= cola)
  }
  list(n = n, k = k, proporcion = if (n > 0L) k / n else NA_real_,
       densidad = densidad, probabilidad = probabilidad, copia = copia)
}

# Las columnas de `candidatas` que copian los documentos de `protegidas` sin su
# verificador: un data.frame con la columna, la proporcion de sus valores
# distintos que coinciden, cuantos son y de que columna protegida salen.
.copias_sin_verificador <- function(datos, protegidas, candidatas,
                                    umbral = 0.9, cola = 1e-6) {
  vacio <- .copias_vacias()
  if (!is.data.frame(datos) || !length(protegidas) || !length(candidatas)) {
    return(vacio)
  }
  nombres <- names(datos)
  operar <- function(v) .nombres_para_operar(as.character(v))
  prefijos_de <- list()
  for (columna in protegidas) {
    j <- match(operar(columna), operar(nombres))
    if (is.na(j)) next
    prefijos <- .prefijos_de_valores(datos[[j]])
    if (length(prefijos)) prefijos_de[[columna]] <- prefijos
  }
  if (!length(prefijos_de)) return(vacio)
  todos <- .prefijos_ordenados(prefijos_de)
  filas <- list()
  for (columna in candidatas) {
    j <- match(operar(columna), operar(nombres))
    if (is.na(j)) next
    copia <- .copia_de_documentos(datos[[j]], prefijos_de, todos,
                                  umbral = umbral, cola = cola)
    if (is.null(copia)) next
    filas[[length(filas) + 1L]] <- data.frame(
      columna = nombres[[j]], proporcion = copia$proporcion,
      evaluados = copia$evaluados, de = copia$de, origen = "columna",
      stringsAsFactors = FALSE
    )
  }
  if (!length(filas)) return(vacio)
  do.call(rbind, filas)
}

.copias_vacias <- function() {
  data.frame(columna = character(), proporcion = numeric(),
             evaluados = integer(), de = character(), origen = character(),
             stringsAsFactors = FALSE)
}

# Los documentos sin su verificador de los valores protegidos `valores` -los de
# una columna, o los que la coleccion leyo de una tabla-, como texto de cifras.
.prefijos_de_valores <- function(valores) {
  cifras <- tryCatch(.cifras_de_columna(unique(valores)),
                     error = function(e) NULL)
  if (is.null(cifras)) return(character())
  .prefijos_documentos(unique(cifras[!is.na(cifras)]),
                       minimo = .MIN_LARGO_COPIA_SIN_VERIFICADOR)
}

.prefijos_ordenados <- function(prefijos_de) {
  .sort_por_bytes(unique(as.numeric(unlist(prefijos_de, use.names = FALSE))))
}

# La prueba de UNA columna -sus valores `valores`- contra los documentos sin
# verificador de cada fuente de `prefijos_de` (una lista con nombre: la columna
# o la tabla de donde salen): `NULL` si no es una copia; si lo es, la proporcion
# de sus distintos que coinciden, cuantos son y la fuente con mas
# coincidencias. Una regla para la tabla (`.copias_sin_verificador()`) y para
# la coleccion, que prueba las columnas de una tabla contra los documentos de
# las demas (`.copias_entre_tablas_coleccion()`). Ronda 35 (H1).
.copia_de_documentos <- function(valores, prefijos_de, todos = NULL,
                                 umbral = 0.9, cola = 1e-6) {
  # Seis cifras o mas: la cedula de seis cifras sin su verificador tambien
  # (ronda 37, P-A2).
  if (is.numeric(valores) &&
      !any(abs(suppressWarnings(as.numeric(valores))) >= 1e5, na.rm = TRUE)) {
    return(NULL)
  }
  distintos <- tryCatch(unique(valores[!is.na(valores)]),
                        error = function(e) NULL)
  if (is.null(distintos) || length(distintos) < 3L) return(NULL)
  cifras <- tryCatch(.cifras_de_columna(distintos), error = function(e) NULL)
  if (is.null(cifras) || !any(!is.na(cifras))) return(NULL)
  if (is.null(todos)) todos <- .prefijos_ordenados(prefijos_de)
  prueba <- .prueba_copia_sin_verificador(distintos, cifras, todos,
                                          umbral = umbral, cola = cola)
  if (!prueba$copia) return(NULL)
  de <- names(prefijos_de)[which.max(vapply(prefijos_de, function(p) {
    sum(cifras %in% p)
  }, numeric(1L)))]
  list(proporcion = prueba$proporcion, evaluados = as.integer(prueba$n),
       de = de)
}

# Las copias que la coleccion encontro en OTRA tabla, para la tabla que se
# vuelve a perfilar: ver `.copias_entre_tablas_coleccion()`. Fuera de esa
# relectura esta vacio. Un estado de la llamada y no un argumento, como los
# valores del motor de la coleccion (`.valores_motor()`): la clasificacion
# corre dentro de `perfilar()` sobre la muestra que `perfilar_dbi()` lee.
.CONTEXTO_COPIAS <- new.env(parent = emptyenv())
.CONTEXTO_COPIAS$externas <- NULL

.copias_externas <- function() .CONTEXTO_COPIAS$externas

.fijar_copias_externas <- function(copias) {
  previas <- .CONTEXTO_COPIAS$externas
  .CONTEXTO_COPIAS$externas <- copias
  invisible(previas)
}

# Cada copia encontrada pasa a ser una columna personal protegida en la
# clasificacion: `documento_identidad`, poder `alto`, con su fundamento. Las de
# la tabla y, en la relectura de una coleccion, las que se encontraron contra
# los documentos de otra tabla (`.copias_externas()`).
.reclasificar_copias_sin_verificador <- function(clasificacion, datos, nombres,
                                                 umbral = 0.9) {
  externas <- .copias_externas()
  hay_externas <- is.data.frame(externas) && nrow(externas) > 0L
  if (!is.data.frame(clasificacion) ||
      (!nrow(clasificacion) && !hay_externas)) {
    return(clasificacion)
  }
  operar <- function(v) .nombres_para_operar(as.character(v))
  protegidas <- as.character(clasificacion$columna[clasificacion$proteger %in% TRUE])
  candidatas <- nombres[!operar(nombres) %in% operar(protegidas)]
  copias <- tryCatch(
    .copias_sin_verificador(datos, protegidas, candidatas, umbral = umbral),
    error = function(e) NULL
  )
  if (!is.data.frame(copias)) copias <- .copias_vacias()
  if (hay_externas) {
    externas <- externas[
      operar(externas$columna) %in% operar(candidatas) &
        !operar(externas$columna) %in% operar(copias$columna), , drop = FALSE
    ]
    if (nrow(externas)) copias <- rbind(copias, externas[names(copias)])
  }
  if (!nrow(copias)) return(clasificacion)
  clase <- class(clasificacion)
  clasificacion <- as.data.frame(unclass(clasificacion), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(copias))) {
    de <- if (identical(copias$origen[[i]], "tabla")) {
      paste0("la tabla `", copias$de[[i]], "`")
    } else {
      paste0("`", copias$de[[i]], "`")
    }
    fila <- data.frame(
      columna = copias$columna[[i]], tipo = "documento_identidad",
      proporcion_compatible = copias$proporcion[[i]],
      proporcion_verificada = copias$proporcion[[i]],
      valores_evaluados = copias$evaluados[[i]],
      valores_totales = NA_integer_,
      fundamento = paste0(
        "documentos de ", de, " sin su verificador: ",
        formatC(copias$proporcion[[i]], format = "f", digits = 3),
        " de sus valores distintos"
      ),
      poder_discriminante = "alto", proteger = TRUE, stringsAsFactors = FALSE
    )
    k <- match(operar(fila$columna), operar(clasificacion$columna))
    if (is.na(k)) {
      clasificacion <- rbind(clasificacion, fila[names(clasificacion)])
    } else {
      clasificacion[k, names(fila)] <- fila
    }
  }
  clasificacion <- clasificacion[
    .orden_por_bytes(match(operar(clasificacion$columna), operar(nombres))), , drop = FALSE
  ]
  rownames(clasificacion) <- NULL
  class(clasificacion) <- clase
  clasificacion
}

.hallazgos_datos_personales <- function(clasificacion, permitidos) {
  if (!nrow(clasificacion)) return(list())
  lapply(seq_len(nrow(clasificacion)), function(i) {
    fila <- clasificacion[i, , drop = FALSE]
    .nuevo_hallazgo(
      fila$columna[[1L]], "dato_personal_posible",
      if (permitidos) "ok" else "error",
      if (permitidos) {
        paste0(
          "La columna parece contener datos personales; esta clasificaci\u00f3n ",
          "no implica un problema de calidad ni de cumplimiento."
        )
      } else {
        paste0(
          "La columna parece contener datos personales y la entrega fue ",
          "declarada como libre de ellos."
        )
      },
      paste0(
        "Tipo posible: ", fila$tipo[[1L]], "; fundamento: ",
        fila$fundamento[[1L]],
        "; poder discriminante: ", fila$poder_discriminante[[1L]],
        "; proteccion automatica: ", if (fila$proteger[[1L]]) "si" else "no",
        if (identical(fila$poder_discriminante[[1L]], "debil") &&
            isTRUE(fila$proteger[[1L]])) {
          " (por precaucion: la forma es compatible y no se pudo verificar)"
        } else "",
        if (is.finite(fila$proporcion_compatible[[1L]])) {
          paste0("; proporci\u00f3n compatible: ",
                 .formatear_decimal_publicado(
                   fila$proporcion_compatible[[1L]]
                 ))
        } else ""
      ),
      if (permitidos) {
        if (fila$proteger[[1L]]) {
          paste0(
            "Mantener protegidos la moda, los ejemplos, la evidencia y los ",
            "estadisticos de orden al compartir salidas."
          )
        } else {
          paste0(
        "Confirmar la semantica de la columna si se necesita decidir si ",
            "sus valores deben protegerse."
          )
        }
      } else {
        "Confirmar el contrato de la entrega antes de retirar o transformar datos."
      },
      1, 1, "columna"
    )
  })
}

# Toda columna que el clasificador reconocio como personal, aunque la evidencia
# sea debil y no corresponda suprimir sus estadisticos. Es el alcance que usa la
# evidencia de los hallazgos: entera si la columna se protege, sus valores si es
# debil.
.columnas_personales_clasificadas <- function(clasificacion) {
  if (inherits(clasificacion, "perfil")) {
    clasificacion <- clasificacion$datos_personales
  }
  if (!inherits(clasificacion, "data.frame") || !nrow(clasificacion)) {
    return(character())
  }
  unique(clasificacion$columna)
}

# Un plan, una medicion, una deriva o un historico armados con la proteccion
# APAGADA llevan la marca `datos_personales_sin_proteger`, y `reportar()` -que
# protege por omision- la lee para no publicarlos en claro. `[` con columnas
# nombradas -lo que hace `subset()`- devuelve la misma clase SIN la marca: el
# objeto filtrado se publicaba entero. Medido en la ronda 21.
#
# Y sin NINGUNO de sus atributos, no solo la marca: la ronda 21 repuso la marca y
# dejo afuera el modelo, la cobertura y la configuracion de cada corrida. Medido
# en la ronda 22: `evaluar(subset(medicion, TRUE))` abortaba con "la regla no
# coincide con ninguna metrica instanciada" -se habia perdido
# `cobertura_metricas`-, y la deriva de un historico filtrado leia un cambio de
# modelo como deterioro de datos. Se reponen todos mientras el resultado siga
# siendo de la clase, y las tablas POR CORRIDA -`por_corrida`- pierden las
# corridas que el recorte saco: si no, volver a acumular una corrida corregida
# chocaba con la configuracion vieja.
.conservar_atributos_objeto <- function(resultado, x, por_corrida = character()) {
  if (!inherits(resultado, "data.frame") || !inherits(resultado, class(x)[[1L]])) {
    return(resultado)
  }
  estructurales <- c("names", "row.names", "class")
  for (atributo in setdiff(names(attributes(x)), estructurales)) {
    if (is.null(attr(resultado, atributo, exact = TRUE))) {
      attr(resultado, atributo) <- attr(x, atributo, exact = TRUE)
    }
  }
  if (length(por_corrida) && "id_medicion" %in% names(x) &&
      "id_medicion" %in% names(resultado)) {
    quitadas <- setdiff(
      unique(as.character(x$id_medicion)),
      unique(as.character(resultado$id_medicion))
    )
    for (atributo in por_corrida) {
      tabla <- attr(resultado, atributo, exact = TRUE)
      if (length(quitadas) && inherits(tabla, "data.frame") &&
          "id_medicion" %in% names(tabla)) {
        tabla <- tabla[!as.character(tabla$id_medicion) %in% quitadas, , drop = FALSE]
        rownames(tabla) <- NULL
        attr(resultado, atributo) <- tabla
      }
    }
  }
  resultado
}

# Y `rbind()` conserva solo la del PRIMER objeto: un objeto protegido delante de
# uno sin proteger daba un resultado sin marca. Se unen las de todos.
.unir_con_marca_sin_proteger <- function(..., deparse.level = 1) {
  resultado <- rbind.data.frame(..., deparse.level = deparse.level)
  marca <- unique(unlist(lapply(list(...), function(objeto) {
    attr(objeto, "datos_personales_sin_proteger", exact = TRUE)
  }), use.names = FALSE))
  if (length(marca) && inherits(resultado, "data.frame")) {
    attr(resultado, "datos_personales_sin_proteger") <- marca
  }
  resultado
}

.columnas_personales_protegidas <- function(clasificacion) {
  if (inherits(clasificacion, "perfil")) {
    clasificacion <- clasificacion$datos_personales
  }
  if (!inherits(clasificacion, "data.frame") || !nrow(clasificacion)) {
    return(character())
  }
  if (!"proteger" %in% names(clasificacion)) {
    return(unique(clasificacion$columna))
  }
  unique(clasificacion$columna[!is.na(clasificacion$proteger) &
    clasificacion$proteger])
}

.fecha_resumida_personal <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(as.Date(NA))
  tryCatch(
    suppressWarnings(as.Date(substr(as.character(x), 1L, 10L))),
    error = function(e) as.Date(NA)
  )
}

.hallazgos_rango_fecha_personal <- function(columnas, clasificacion,
                                            fecha_referencia, tipo) {
  configuraciones <- list(
    fecha_nacimiento = list(
      hallazgo = "fecha_nacimiento_fuera_rango",
      descripcion = "fecha de nacimiento"
    ),
    fecha_fallecimiento = list(
      hallazgo = "fecha_fallecimiento_fuera_rango",
      descripcion = "fecha de fallecimiento"
    )
  )
  configuracion <- configuraciones[[tipo]]
  if (is.null(configuracion)) return(list())
  fechas <- clasificacion[clasificacion$tipo == tipo, , drop = FALSE]
  if (!nrow(fechas)) return(list())
  limite_inferior <- as.Date("1900-01-01")
  limite_superior <- as.Date(fecha_referencia, tz = "UTC")
  hallazgos <- list()
  for (i in seq_len(nrow(fechas))) {
    nombre <- fechas$columna[[i]]
    indice <- .indice_nombre(nombre, columnas$columna)
    if (is.na(indice)) next
    minimo <- .fecha_resumida_personal(columnas$minimo_fecha[[indice]])
    maximo <- .fecha_resumida_personal(columnas$maximo_fecha[[indice]])
    anterior <- !is.na(minimo) && minimo < limite_inferior
    futura <- !is.na(maximo) && maximo > limite_superior
    if (!anterior && !futura) next
    situaciones <- c(
      if (anterior) "al menos una fecha anterior a 1900",
      if (futura) "al menos una fecha posterior a la fecha del perfil"
    )
    hallazgos[[length(hallazgos) + 1L]] <- .nuevo_hallazgo(
      nombre, configuracion$hallazgo,
      if (futura) "error" else "sospechoso",
      paste0(
        "La columna clasificada como ", configuracion$descripcion,
        " contiene ",
        paste(situaciones, collapse = " y "), "."
      ),
      paste0(
        "Se aplicaron limites de plausibilidad sin publicar las fechas ",
        "observadas."
      ),
      "Revisar los registros se\u00f1alados contra la fuente antes de corregirlos.",
      columnas$n[[indice]], NA_real_, "fila"
    )
  }
  hallazgos
}

.hallazgos_rango_nacimiento <- function(columnas, clasificacion,
                                        fecha_referencia) {
  .hallazgos_rango_fecha_personal(
    columnas, clasificacion, fecha_referencia, "fecha_nacimiento"
  )
}

# La proteccion no puede depender de que cada diagnostico recuerde llamar a un
# enmascarador. Las reglas se escriben en archivos distintos y varias de ellas
# arman prosa con valores de filas; si una queda fuera, el dato llega igual a
# `hallazgos`, al HTML o al plan. Este es el inventario de representaciones que
# puede publicar una salida sin conservar la columna original.
# `.texto_valor()` sobre cada elemento, en una pasada por clase. Se llamaba una
# vez POR CELDA de cada columna protegida: sobre 200.000 filas, 13 de los 44
# segundos de la proteccion. Las clases que no se reconocen siguen por el camino
# de uno en uno.
.texto_valor_vector <- function(x) {
  if (!length(x)) return(character())
  if (inherits(x, "POSIXt")) {
    salida <- format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  if (inherits(x, "Date")) {
    salida <- format(x, "%Y-%m-%d")
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  if ((is.double(x) && !inherits(x, "integer64") && is.null(attr(x, "class"))) ||
      (is.integer(x) && is.null(attr(x, "class")))) {
    return(.formatear_numeros_uno_a_uno(x))
  }
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    salida <- as.character(x)
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  vapply(seq_along(x), function(i) .texto_valor(x[i]), character(1L))
}

# Si una COLUMNA es de fechas: `Date` o `POSIXt`, o la mitad o mas de sus
# valores distintos tienen forma de fecha -ver `.forma_de_fecha()`-. Es la UNICA
# regla y la usan todas las puertas: el piso en memoria, la cosecha de los
# estadisticos, las fechas que identifican y la lectura de la tabla entera en
# `perfilar_dbi()`. Hasta la ronda 24 eran dos: la columna AAAAMMDD tenia que
# serlo en TODAS sus celdas y la de fechas con separadores en la mitad, y el ano
# en dos cifras no contaba. Medido en la ronda 25: una columna de nacimientos
# con el 60 % en `dd/mm/aa`, o una AAAAMMDD con UNA celda mala, no era "de
# fechas", sus fechas entraban al piso y tapaban la moda y los ejemplos de las
# fechas de OTRA columna -con 20.000 personas, el 16 % de sus celdas-. Ante
# cualquier duda, no es de fechas: la columna sigue aportando sus valores.
.columna_de_fechas <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(TRUE)
  if (!(is.numeric(x) || is.character(x) || is.factor(x)) ||
      inherits(x, "integer64")) {
    return(FALSE)
  }
  tryCatch({
    texto <- unique(.caracter_por_omision(x))
    texto <- texto[!is.na(texto)]
    forma <- .forma_de_fecha(texto)
    vacios <- !nzchar(.recortar_bytes(texto))
    any(!vacios) && mean(forma[!vacios]) >= 0.5
  }, error = function(e) FALSE)
}

# `trimws()` aborta sobre texto que no es UTF-8 valido, y este paquete trabaja
# con esos: se recorta en bytes.
.recortar_bytes <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x, useBytes = TRUE)
}

# La forma de fecha que cuenta para decidir si una columna es de fechas y, en
# una que lo es, que valores salen del piso: la fecha sola de `.es_fecha_sola()`,
# la del ano en dos cifras -"16/01/95"- y la compacta AAAAMMDD -"19950116"-,
# tambien escrita como numero real: SQLite devuelve "19950116.0" al leer como
# texto una columna REAL.
.forma_de_fecha <- function(valores) {
  valores <- .recortar_bytes(.caracter_por_omision(valores))
  dia <- "(0?[1-9]|[12][0-9]|3[01])"
  mes <- "(0?[1-9]|1[0-2])"
  dos_cifras <- paste0(
    "^(", dia, "[-/.]", mes, "|", mes, "[-/.]", dia, ")[-/.][0-9]{2}$"
  )
  compacta <- paste0(
    "^(1[89]|20)[0-9]{2}(0[1-9]|1[0-2])(0[1-9]|[12][0-9]|3[01])([.]0+)?$"
  )
  salida <- .es_fecha_sola(valores) |
    grepl(dos_cifras, valores, perl = TRUE, useBytes = TRUE) |
    grepl(compacta, valores, perl = TRUE, useBytes = TRUE)
  salida[is.na(valores)] <- FALSE
  salida
}

# Los RELLENOS de una columna protegida que son un centinela: un digito repetido
# -"99999999", "0000-0000"- que aparece al menos `.MIN_REPETICIONES_CENTINELA`
# veces en una columna que, sin el, es casi una clave -nueve de cada diez de sus
# valores, distintos-. Ahi el dato de una persona aparece una vez y lo que se
# repite cuarenta veces ocupa el lugar de un dato. Medido en la ronda 25: el
# `99999999` de una cedula entraba al piso y tapaba la moda, el minimo, la
# mediana y el maximo de un `monto` que tenia su propio `99999999`, y el hallazgo
# decia "el valor [valor protegido] aparece 30 veces". En una tabla de
# movimientos, donde el documento de un cliente se repite, NO se aplica: la
# cedula 2.222.222-2 es valida y es de alguien. Ante cualquier duda, ninguno.
# El relleno sin letras ni digitos -"------"- no necesita la columna: lo saca
# `.valores_identificantes()`.
# La tasa de distintos que la columna sin sus rellenos tiene que alcanzar. La
# usan esta regla y su confirmacion sobre la tabla entera en `perfilar_dbi()`
# (`.rellenos_confirmados_tabla_dbi()`): un solo numero.
.TASA_CASI_CLAVE_RELLENO <- 0.9

.rellenos_de_columna <- function(x) {
  if (!(is.character(x) || is.factor(x) || is.numeric(x)) ||
      inherits(x, "integer64")) {
    return(character())
  }
  tryCatch({
    texto <- .caracter_por_omision(x)
    texto <- texto[!is.na(texto)]
    if (length(texto) < .MIN_REPETICIONES_CENTINELA) return(character())
    distintos <- unique(texto)
    cuenta <- tabulate(match(texto, distintos), nbins = length(distintos))
    .rellenos_de_frecuencias(distintos, cuenta)
  }, error = function(e) character())
}

# La misma regla sobre la tabla de frecuencias de la columna: sus valores
# distintos y cuantas veces aparece cada uno. La usa tambien `perfilar_coleccion()`,
# que la trae con un `GROUP BY` para proteger una tabla con los valores de otra.
.rellenos_de_frecuencias <- function(distintos, cuenta) {
  tryCatch({
    distintos <- .caracter_por_omision(distintos)
    cuenta <- as.numeric(cuenta)
    validos <- !is.na(distintos) & !is.na(cuenta)
    distintos <- distintos[validos]
    cuenta <- cuenta[validos]
    if (sum(cuenta) < .MIN_REPETICIONES_CENTINELA) return(character())
    digitos <- gsub("[^0-9]", "", distintos, useBytes = TRUE)
    candidatos <- which(
      cuenta >= .MIN_REPETICIONES_CENTINELA &
        grepl("^[0-9 .,/-]+$", distintos, useBytes = TRUE) &
        grepl("^([0-9])\\1{5,}$", digitos, useBytes = TRUE)
    )
    if (!length(candidatos)) return(character())
    resto <- sum(cuenta[-candidatos])
    if (!resto) return(character())
    casi_clave <- (length(distintos) - length(candidatos)) / resto >=
      .TASA_CASI_CLAVE_RELLENO
    if (!casi_clave) return(character())
    distintos[candidatos]
  }, error = function(e) character())
}

# Los rellenos de las columnas `sensibles` de `datos`, para quien protege POR
# COLUMNA: los parametros del plan sobre una columna protegida y las filas
# duplicadas citadas tapan sus celdas con todos sus valores, tambien estos. Solo
# el piso -lo que se busca en las demas columnas- los deja afuera.
.rellenos_de_columnas <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) return(character())
  indices <- .indice_nombre(sensibles, names(datos))
  unique(unlist(lapply(indices[!is.na(indices)], function(i) {
    .rellenos_de_columna(datos[[i]])
  }), use.names = FALSE))
}

.valores_publicables_protegidos <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices_sensibles <- .indice_nombre(sensibles, names(datos))
  valores <- unlist(lapply(unique(indices_sensibles[!is.na(indices_sensibles)]), function(indice) {
    x <- datos[[indice]]
    if (is.data.frame(x) || .es_columna_compuesta(x) || is.list(x)) {
      return(character())
    }
    # En una columna de fechas -tambien AAAAMMDD, entero o texto, y con el ano
    # en dos cifras- una fecha sola no entra en el piso (ver
    # `.valores_identificantes()`). Se decide por la COLUMNA y no por el valor:
    # una cedula de ocho digitos puede parecer una fecha, una columna entera de
    # cedulas no. Medido en una refutacion: tapaba los estadisticos de fecha de
    # las demas columnas. Lo que en ella NO tiene forma de fecha -un relleno, un
    # texto- sigue entrando.
    de_fechas <- .columna_de_fechas(x)
    # Con la escritura de la sesion por omision: con `scipen = -9` el `11` de
    # la columna protegida salia `1.1e+01`, pasaba el piso de seis caracteres y
    # tapaba el maximo 11 de otra columna (ronda 31-S). Lo publicado con la marca
    # de la sesion lo cubren los `formateados`.
    crudos <- tryCatch(.caracter_por_omision(x), error = function(e) character())
    formateados <- tryCatch(c(
      format(x, digits = 15L, trim = TRUE, scientific = FALSE),
      format(x, digits = 8L, trim = TRUE, scientific = FALSE),
      .texto_valor_vector(x)
    ), error = function(e) character())
    # Recortado: `format()` de un texto lo rellena con espacios hasta el ancho
    # de la columna, y "99999999   " no es "99999999". Y la aguja tampoco va
    # rellena: "Maria Lopez Fernandez " -relleno hasta el largo de "Carlos
    # Rodriguez Silva"- se tragaba el espacio de la prosa que la cita, "La
    # relacion [valor protegido] -> [valor protegido]es exacta". Medido en la
    # ronda 29. El valor recortado tapa lo mismo: esta adentro del relleno.
    salida <- .recortar_bytes(c(crudos, formateados))
    if (de_fechas) salida <- salida[!.forma_de_fecha(salida)]
    rellenos <- .rellenos_de_columna(x)
    if (length(rellenos)) {
      salida <- salida[!salida %in% rellenos]
    }
    salida
  }), use.names = FALSE)
  valores <- unique(valores[!is.na(valores) & nzchar(valores)])
  # Los valores largos van primero para que `12` no deje restos dentro de un
  # identificador `312`. La sustitucion es fija: estos textos ya son valores,
  # no expresiones regulares.
  valores[.orden_por_bytes(nchar(valores, type = "bytes"), decreasing = TRUE,
                method = "radix")]
}

.valores_perfil_protegidos <- function(columnas, patrones, clasificacion,
                                       meta = NULL, datos = NULL,
                                       excluir = character(),
                                       rellenos = character()) {
  sensibles <- .columnas_personales_protegidas(clasificacion)
  # `excluir`: las columnas de fechas que quien llama ya decidio sin pasar los
  # datos -`perfilar_dbi()`, con la muestra-. Mismo efecto que el de abajo.
  # `rellenos`: lo mismo para los centinelas de `.rellenos_de_columna()`.
  sensibles <- sensibles[
    !.nombres_para_operar(sensibles) %in% .nombres_para_operar(excluir)
  ]
  if (inherits(datos, "data.frame") && length(sensibles)) {
    rellenos <- unique(c(rellenos, .rellenos_de_columnas(datos, sensibles)))
  }
  # Con los datos a mano, una columna de fechas -tambien AAAAMMDD o con el ano en
  # dos cifras- no aporta agujas tampoco por sus estadisticos: su maximo
  # `20000111` tapaba el maximo `2000-01-11` de otra columna de fechas. Lo que
  # en ella no tiene forma de fecha llega igual, desde los datos: ver
  # `.valores_publicables_protegidos()`.
  if (inherits(datos, "data.frame") && length(sensibles)) {
    indices_datos <- .indice_nombre(sensibles, names(datos))
    de_fechas <- vapply(seq_along(sensibles), function(i) {
      !is.na(indices_datos[[i]]) &&
        .columna_de_fechas(datos[[indices_datos[[i]]]])
    }, logical(1L))
    sensibles <- sensibles[!de_fechas]
  }
  if (!inherits(columnas, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices <- which(.nombres_para_operar(columnas$columna) %in%
                    .nombres_para_operar(sensibles))
  campos <- intersect(
    c(
      "moda", "minimo", "maximo", "mediana", "centinela_valor",
      "minimo_exacto", "maximo_exacto", "minimo_fecha", "maximo_fecha",
      "media_fecha", "mediana_fecha"
    ),
    names(columnas)
  )
  valores <- unlist(lapply(campos, function(campo) {
    tryCatch(.caracter_por_omision(columnas[[campo]][indices]),
             error = function(e) character())
  }), use.names = FALSE)
  if (length(patrones) && length(indices)) {
    indices_patrones <- intersect(indices, seq_along(patrones))
    for (i in indices_patrones) {
      tabla <- patrones[[i]]
      if (inherits(tabla, "data.frame") && "ejemplos" %in% names(tabla)) {
        valores <- c(valores, as.character(tabla$ejemplos))
      }
      for (atributo in c("resumen_patrones", "desvios_patron_raro")) {
        resumen <- attr(tabla, atributo, exact = TRUE)
        if (inherits(resumen, "data.frame") && "ejemplos" %in% names(resumen)) {
          valores <- c(valores, as.character(resumen$ejemplos))
        }
      }
    }
  }
  # Si el perfil abierto conserva una declaracion de centinelas, se trata como
  # una posible representacion del dato. Cuando no hay `datos` para decidir a
  # que columna corresponde, ocultarlos todos es la opcion segura.
  # El catalogo de centinelas DEL PAQUETE -`.numeros_na_locales`- es vocabulario y
  # no entra: como aguja se tapaba a si mismo en `meta`, y el plan quedaba con la
  # accion que convierte `-999` en `NA` sin efecto en una columna de montos,
  # recomendada y activa. Medido en una refutacion.
  #
  # Y CON los datos, lo declarado entra solo si aparece en una columna
  # protegida -como su valor o como su relleno: el 99999999 que se repite en la
  # cedula sigue en el piso-. Entraba siempre, y se tapaba a si mismo en `meta`
  # -de donde el plan toma la lista-: con `sentinelas_numericos = c(-999,
  # 99999999)` la accion de `monto` salia `valores = -999, NA` y `aplicar()` la
  # registraba `fallida` sin un solo 99999999 en la columna protegida: el piso
  # no protegia nada. Es lo que la ronda 26 arreglo para el catalogo, en la
  # declaracion. Medido en la ronda 28.
  declarados <- character()
  if (is.list(meta) && length(meta$sentinelas_numericos)) {
    propios <- meta$sentinelas_numericos[
      !meta$sentinelas_numericos %in% .numeros_na_locales
    ]
    if (inherits(datos, "data.frame")) {
      propios <- propios[.aparece_en_columnas(
        propios, datos, .columnas_personales_protegidas(clasificacion)
      )]
    }
    declarados <- as.character(propios)
  }
  # Las celdas de `ejemplos` no traen UN valor: traen hasta tres UNIDOS con
  # `.SEPARADOR_EJEMPLOS`. Una aguja que es la cadena unida no existe en ningun
  # otro lado de la salida, asi que el reemplazo no encontraba nada y el piso se
  # quedaba sin agujas. Medido: con documentos de TEXTO -la moda y los extremos
  # en `NA`, de modo que la celda unida es el UNICO portador del valor- el
  # informe publicaba tres de cuatro cedulas CON su proteccion puesta. Con
  # documentos numericos el mismo camino funciona, porque las agujas llegan por
  # `minimo` y `maximo`; por eso las dos pruebas de la suite que reportan un
  # perfil abierto pasaban sin ver nada.
  #
  # Se agregan las partes ADEMAS de la cadena entera, que tambien aparece tal
  # cual en la celda que la origino. `useBytes = TRUE` porque este paquete
  # trabaja con codificaciones rotas y sin eso `strsplit()` avisa "input string
  # is invalid in this locale" por cada valor sin marca. El piso de seis
  # caracteres lo aplica `.valores_identificantes()` mas adelante, asi que una
  # parte corta no enmascara media tabla.
  partes <- unlist(
    strsplit(valores, .SEPARADOR_EJEMPLOS, fixed = TRUE, useBytes = TRUE),
    use.names = FALSE
  )
  valores <- c(valores, partes)
  valores <- unique(valores[!is.na(valores) & nzchar(valores)])
  # El centinela de una columna protegida -su moda, cuarenta `99999999`- no es
  # el dato de nadie: ver `.rellenos_de_columna()`. Si otra columna protegida
  # lo tiene como valor, entra por `.valores_publicables_protegidos()`, que
  # decide por columna. Lo que el usuario DECLARO centinela en `meta` entra
  # igual: es su declaracion, y sin la tabla no se sabe de que columna es.
  if (length(rellenos)) {
    valores <- valores[!.recortar_bytes(valores) %in% rellenos]
  }
  valores <- unique(c(valores, declarados[!is.na(declarados)]))
  valores[.orden_por_bytes(nchar(valores, type = "bytes"), decreasing = TRUE,
                method = "radix")]
}

# Si cada numero de `valores` -un centinela declarado- aparece en alguna de las
# columnas `columnas` de `datos`, escrito como numero o como texto. Ante una
# columna que no se puede leer, TRUE: es el lado cerrado.
.aparece_en_columnas <- function(valores, datos, columnas) {
  if (!length(valores)) return(logical())
  indices <- .indice_nombre(columnas, names(datos))
  presente <- rep(FALSE, length(valores))
  for (indice in unique(indices[!is.na(indices)])) {
    x <- datos[[indice]]
    numeros <- tryCatch({
      if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
        as.numeric(x)
      } else if (is.character(x) || is.factor(x)) {
        suppressWarnings(as.numeric(trimws(as.character(x))))
      } else {
        numeric()
      }
    }, error = function(e) NULL)
    if (is.null(numeros)) return(rep(TRUE, length(valores)))
    presente <- presente | (!is.na(valores) & valores %in% numeros)
  }
  presente
}

# Si la aguja -una forma ya plegada por `.plegar_para_comparar()`, sin nada que no
# sea letra o digito- aparece en el texto plegado sin una LETRA pegada antes ni
# despues, con cualquier separador entre sus caracteres. El limite es de letras y
# no de alfanumericos a proposito: lo que se quiere dejar de tapar es un tramo de
# letras que cruza palabras de la prosa, y un digito pegado no es eso. Con limite
# alfanumerico se escapaba "maria.nunez123@correo.uy" frente al nombre protegido
# "Maria Nunez", que antes se tapaba. La aguja se parte por CARACTERES y las
# clases son de Unicode: partida por bytes, una letra griega o cirilica quedaba
# cortada al medio y el separador opcional se metia entre sus bytes. Ante un texto
# que no se puede examinar, TRUE: es una funcion de privacidad y el valor por
# omision tiene que ser cerrado.
.variante_en_limites <- function(aguja, textos) {
  caracteres <- intToUtf8(utf8ToInt(aguja), multiple = TRUE)
  patron <- paste0(
    "(?<!\\p{L})",
    paste(caracteres, collapse = "[^\\p{L}\\p{N}]*"),
    "(?!\\p{L})"
  )
  tryCatch(
    grepl(patron, textos, perl = TRUE),
    error = function(e) rep(TRUE, length(textos))
  )
}

# Los espacios de Unicode como un espacio, y los signos ASCII de ancho
# completo (U+FF01 a U+FF5E) como su forma ASCII. Solo para LEER el correo: lo
# que sale es una forma mas de comparacion, no el texto publicado.
#
# Y las demas FORMAS DE COMPATIBILIDAD de un signo ASCII, las que la regla de
# digitos ya leia: los parentesis blancos anchos (U+FF5F, U+FF60) y sus no
# anchos (U+2985, U+2986), el punto ideografico (U+3002) y el de medio ancho
# (U+FF61), las variantes chicas (U+FE50 a U+FE6B: la coma, el punto, el punto
# y coma, los parentesis, la arroba...), la barra partida (U+00A6) y su ancha
# (U+FFE4) como barra vertical, y el salto de linea de C1 (U+0085), que es un
# espacio de Unicode y PCRE no lo casa con `\\p{Z}`. Medido en la ronda 27: el
# correo `juan.perez<U+FF5F>at<U+FF60>empresa.com.uy`, el dominio con el punto
# ideografico y la fecha con hora con la arroba chica se publicaban, mientras
# la cedula escrita con esos mismos puntos se tapaba.
#
# Y TODAS las demas: cada caracter cuya forma de compatibilidad de Unicode (NFKC)
# es UN signo ASCII -los parentesis superindice y subindice (U+207D, U+207E,
# U+208D, U+208E), las formas de presentacion vertical (U+FE10 a U+FE48), el
# punto de guia (U+2024), los guiones bajos de U+FE4D a U+FE4F, el signo de
# interrogacion griego-. La ronda 27 enumero las formas que conocia y el
# correo `juan.perez<U+207D>at<U+207E>empresa.com.uy` se publicaba frente al
# protegido: era la misma propiedad con otros cuatro caracteres. Medido en la
# ronda 28. La lista se genero recorriendo Unicode 15.1 con
# `unicodedata.normalize("NFKC", x)`: son 82, y estas 28 faltaban (la raya chica
# U+FE63 la lleva al guion la regla de digitos). Las letras encerradas -un
# circulo, un parentesis- no son un signo y no se leen.
.ancho_y_espacios_ascii <- local({
  # Sin las rayas chicas (U+FE58, U+FE63): `chartr()` lee un guion entre dos
  # caracteres como un rango, y la regla de digitos ya las lleva al guion.
  anchos <- intToUtf8(c(0xFF01:0xFF5E, 0xFF5F, 0xFF60, 0x2985, 0x2986, 0x3002,
                        0xFF61, 0x00A6, 0xFFE4, 0xFE50:0xFE52, 0xFE54:0xFE57,
                        0xFE59:0xFE5C, 0xFE5F:0xFE62, 0xFE64:0xFE66,
                        0xFE68:0xFE6B,
                        0x037E, 0x1FEF, 0x2024, 0x207A, 0x207C:0x207E,
                        0x208A, 0x208C:0x208E, 0xFB29, 0xFE10, 0xFE13:0xFE16,
                        0xFE33:0xFE38, 0xFE47, 0xFE48, 0xFE4D:0xFE4F))
  ascii <- intToUtf8(c(0x21:0x7E, utf8ToInt(paste0(
    "()()..||,,.;:?!(){}#&*+<>=\\$%@",
    ";`.+=()+=()+,:;!?__(){}[]___"
  ))))
  function(textos) {
    textos <- gsub("[\\p{Z}\\t\u0085]", " ", textos, perl = TRUE)
    chartr(anchos, ascii, textos)
  }
})

# El correo ofuscado con palabras, leido como correo: `(at)`, `[at]`, `{at}`,
# `_at_`, `(arroba)`, `(a)` y ` at ` o ` arroba ` entre dos palabras son la
# arroba; `(dot)`, `(punto)` y ` dot ` o ` punto ` entre dos palabras, el punto.
# Solo con forma de correo: la arroba, si la sigue un dominio con su punto
# -escrito o en palabras-, y el punto, solo en un texto que ya tiene arroba.
# Sin eso, "maria at lopez" o "maria punto lopez" se leian "maria.lopez" y se
# tapaban frente a la protegida "Maria Lopez". La `a` sola, solo entre
# parentesis: suelta es una preposicion. Ante un texto que no se puede
# examinar, se devuelve como esta.
#
# Ronda 26, tres puertas de la misma regla. (1) Los espacios de Unicode -el duro
# U+00A0, el ideografico U+3000- y los signos de ancho completo -los parentesis
# U+FF08 y U+FF09, la arroba U+FF20- son cosmeticos para la proteccion, pero
# aca se leian crudos: `\s` de PCRE no casa el espacio duro, y el correo con
# `(at)` escrito con los parentesis anchos se publicaba. Se pasan a su forma
# ASCII antes de leer. (2) La
# arroba LITERAL tambien abre la lectura del punto en palabras:
# `juan.perez@empresa punto com punto uy` salia, porque solo `at` y `arroba`
# abrian el paso. (3) Quien llama lo aplica tambien sobre el texto plegado, que
# es UTF-8 valido: un texto latin1 con una tilde no lo es en bytes y se saltaba.
.correo_desofuscado <- function(textos) {
  tryCatch({
    # Solo el texto que es UTF-8 valido se examina: `grepl()` con `perl = TRUE`
    # AVISA "input string is invalid UTF-8" sobre bytes rotos, y el aviso le
    # llegaba al usuario desde la proteccion. El que no se puede examinar se
    # devuelve como esta, que es lo que este ayudante promete.
    examinables <- !is.na(textos) & validUTF8(textos)
    leidos <- textos
    leidos[examinables] <- .ancho_y_espacios_ascii(textos[examinables])
    con <- examinables
    con[examinables] <- grepl(
      .regex_sin_caja(paste0("[(\\[{_]\\s*(?:at|arroba|a)\\s*[)\\]}_]|",
             "[\\p{L}\\p{N}]\\s+(?:at|arroba)\\s+[\\p{L}\\p{N}]")),
      leidos[examinables], perl = TRUE
    ) | grepl("@", leidos[examinables], fixed = TRUE, useBytes = TRUE)
    if (!any(con)) return(textos)
    t <- leidos[con]
    punto <- .regex_sin_caja("(?:dot|punto)")
    punto_escrito <- paste0(
      "(?:\\s*[(\\[{]\\s*", punto, "\\s*[)\\]}]\\s*|_", punto, "_|\\s+", punto,
      "\\s+)"
    )
    dominio <- paste0("(?=[\\p{L}\\p{N}][\\p{L}\\p{N}-]*(?:\\.|", punto_escrito,
                      ")[\\p{L}])")
    arroba <- .regex_sin_caja("(?:at|arroba)")
    t <- gsub(paste0("\\s*(?:[(\\[{]\\s*(?:", arroba, "|[aA])\\s*[)\\]}]|_",
                     arroba, "_)\\s*", dominio), "@", t, perl = TRUE)
    t <- gsub(paste0("(?<=[\\p{L}\\p{N}])\\s+", arroba, "\\s+", dominio), "@",
              t, perl = TRUE)
    con_arroba <- grepl("@", t, fixed = TRUE, useBytes = TRUE)
    t[con_arroba] <- gsub(
      paste0("(?<=[\\p{L}\\p{N}])", punto_escrito, "(?=[\\p{L}\\p{N}])"), ".",
      t[con_arroba], perl = TRUE
    )
    textos[con] <- t
    textos
  }, error = function(e) textos)
}

.reemplazar_variantes_separadas <- function(x, valores,
                                           exigir_limites = FALSE) {
  # El reemplazo de arriba busca la cadena EXACTA, asi que el mismo documento
  # escrito con separadores se le escapa: `"771.771-01"` no contiene
  # `"77177101"`. Lo encontro una refutacion externa.
  #
  # Aca se compara la forma SIN separadores. Si al sacarle todo lo que no es
  # alfanumerico una cadena contiene un valor protegido igualmente normalizado,
  # la celda entera se enmascara: la separacion es cosmetica y el valor es el
  # mismo. Se enmascara entera y no por tramos porque el separador puede estar
  # en cualquier lado y reconstruir el resto no aportaria nada.
  #
  # Solo entran los valores identificantes que ya trae el piso -seis caracteres
  # o mas-, y ademas se exige que su forma normalizada conserve ese largo: sin
  # eso, un valor corto tras normalizar enmascararia media tabla.
  if (!is.character(x) || !length(valores)) return(x)
  apartado <- .apartar_marcas_paquete(x)
  x <- apartado$x
  # La caja, la tilde y la codificacion tambien son cosmeticas. El mismo nombre
  # en minusculas dentro de un texto libre se publicaba mientras la forma
  # canonica quedaba enmascarada en todo el informe; "juan.perez" se publicaba
  # frente al protegido "Juan Perez" con tilde; y una refutacion lo midio despues
  # fuera del latin occidental -vietnamita sin marcas, griego y cirilico en
  # mayusculas, ancho completo- y con el protegido en latin1 sin marca frente a la
  # celda en UTF-8. Agujas y celdas pasan por `.plegar_para_comparar()`, que deja
  # todo en UTF-8 valido y no depende del locale, y despues se les saca lo que no
  # es letra ni digito con clases de Unicode. Antes eso se hacia por bytes y una
  # letra no latina perdia la mayoria de los suyos: la comparacion era por azar.
  plegar <- function(v, desescapar = FALSE, invalidos = "por_byte") {
    tryCatch(
      .plegar_para_comparar(v, desescapar = desescapar, invalidos = invalidos),
      error = function(e) .textos_para_plegar(as.character(v))
    )
  }
  sin_separadores <- function(v) {
    tryCatch(
      gsub("[^\\p{L}\\p{N}]", "", v, perl = TRUE),
      error = function(e) gsub("[^[:alnum:]]", "", v, useBytes = TRUE)
    )
  }
  normalizar <- function(v, ...) sin_separadores(plegar(v, ...))
  # Un texto sin marca que no es UTF-8 valido se lee de las dos maneras -byte por
  # byte y entero como CP1252-: en latin1, una E acentuada seguida de un espacio
  # duro es por azar una letra UTF-8 valida, y la lectura por byte daba otra
  # letra. Medido en una refutacion.
  ambiguos <- function(v) {
    !is.na(v) & Encoding(v) %in% c("unknown", "bytes", "UTF-8") & !validUTF8(v)
  }
  agujas <- unique(c(
    normalizar(valores),
    normalizar(valores[ambiguos(valores)], invalidos = "entero")
  ))
  # El piso se cuenta en CARACTERES, como en `.valores_identificantes()`: en bytes,
  # un nombre cirilico de cuatro letras pasaba el piso de seis.
  largos <- nchar(agujas, type = "chars", allowNA = TRUE)
  agujas <- agujas[
    !is.na(agujas) & (is.na(largos) | largos >= .MIN_LARGO_VALOR_IDENTIFICANTE)
  ]
  if (!length(agujas)) return(.reponer_marcas_paquete(x, apartado))
  candidatas <- !is.na(x) & x != "[valor protegido]"
  if (!any(candidatas)) return(.reponer_marcas_paquete(x, apartado))
  palabras_paquete <- .LEXICO_PAQUETE$palabras
  # El texto publicado se compara en hasta tres formas: como esta, con los
  # escapes del paquete deshechos -un espacio duro sale `<U+00A0>`- y, si no es
  # UTF-8 valido, leido entero como CP1252. Basta que una contenga el valor. El
  # valor protegido no se desescapa: un valor con una barra literal y su cita, que
  # dobla la barra, solo coinciden asi. Medido en una refutacion.
  elegidos <- x[candidatas]
  crudos <- list(plegar(elegidos), plegar(elegidos, desescapar = TRUE))
  rotos <- ambiguos(elegidos)
  if (any(rotos)) {
    entero <- crudos[[1L]]
    entero[rotos] <- plegar(elegidos[rotos], invalidos = "entero")
    crudos[[3L]] <- entero
  }
  # Y el correo escrito para que no lo lean las maquinas -`juan(at)gmail.com`,
  # `juan arroba gmail punto com`-, con esas palabras leidas como la arroba y el
  # punto. Sin esto se publicaba en diez de doce formas. Medido en la ronda 25:
  # sobre 3000 correos protegidos, el costo de leerlas asi en correos NO
  # protegidos fue el mismo que con la arroba comun -los separadores ya eran
  # cosmeticos-, y 2009 frases con esas palabras no se taparon. Solo las filas
  # que traen una; las demas quedan vacias en esta forma y no golpean.
  #
  # Sobre cada forma plegada, no sobre la cruda: el pliegue deja todo en UTF-8
  # valido, y un texto latin1 con una tilde -`contactar a Jose: juan(at)...`
  # escrito con la tilde, como lo deja `read.csv(encoding = "latin1")`- no lo es
  # en bytes y el lector lo saltaba. Medido en la ronda 26.
  # Cada forma solo donde difiere de la primera: lo mismo no se lee dos veces.
  bases <- crudos
  for (k in seq_along(bases)) {
    base <- bases[[k]]
    mirar <- !is.na(base)
    if (k > 1L) mirar <- mirar & base != bases[[1L]]
    if (!any(mirar)) next
    ofuscados <- base
    ofuscados[mirar] <- .correo_desofuscado(base[mirar])
    cambiados <- mirar & !is.na(ofuscados) & ofuscados != base
    if (any(cambiados)) {
      forma <- rep("", length(elegidos))
      forma[cambiados] <- plegar(ofuscados[cambiados])
      crudos[[length(crudos) + 1L]] <- forma
    }
  }
  pajares <- lapply(crudos, sin_separadores)
  pajar <- pajares[[1L]]
  # La regla de digitos de mas abajo compara en la direccion contraria -la corrida
  # de la celda DENTRO de la aguja, que es el documento sin su verificador-, y
  # ahi el comienzo de la aguja no tiene por que aparecer: usa todas.
  agujas_todas <- agujas
  agujas <- .valores_que_pueden_aparecer(
    agujas, unlist(pajares, use.names = FALSE), .MIN_LARGO_VALOR_IDENTIFICANTE
  )
  # Las agujas de PUROS DIGITOS no se buscan aca: sin separadores, dos numeros
  # distintos se pegan -"N/A (49710); - (49576)" contenia una cedula de ocho
  # digitos que no estaba en la celda-. Las busca la regla de digitos de abajo,
  # que respeta donde empieza y termina cada numero.
  agujas <- agujas[!grepl("^[0-9]+$", agujas, useBytes = TRUE)]
  limites <- rep_len(as.logical(exigir_limites), length(x))[candidatas]
  limites[is.na(limites)] <- FALSE
  # Lo que la prosa cita entre comillas dobles no es prosa: es un valor de celda
  # que el paquete incrusta, como los niveles de un determinante en la sugerencia
  # de `posible_ausencia_estructural` -`~ proveedor == "juanperezsrl"`-. Ahi vale
  # la regla fuerte. Medido en una refutacion: con el titular "Juan Perez"
  # protegido, ese nivel salia entero porque la aguja esta pegada a "srl". Se tapa
  # solo lo citado, no el texto entero, para que la sugerencia se siga leyendo.
  # Las citas se buscan en el texto ORIGINAL y no en el transliterado: la
  # transliteracion puede convertir comillas tipograficas en comillas rectas, y
  # entonces las citas de uno y otro no se corresponden. Una comilla suelta -en
  # el nombre de una columna- corre el apareo, y entonces se prueban todos los
  # apareos posibles: ver `.tramos_entre_delimitadores()`.
  textos_candidatos <- x[candidatas]
  citas <- vector("list", length(pajar))
  citado <- rep("", length(pajar))
  texto_tramos <- function(texto, tramos) {
    vapply(seq_len(NROW(tramos)), function(k) {
      .texto_de_tramo(texto, tramos[k, "inicio"], tramos[k, "fin"])
    }, character(1L))
  }
  for (i in which(limites)) {
    citas[[i]] <- .tramos_entre_delimitadores(textos_candidatos[[i]], "\"")
    if (NROW(citas[[i]])) {
      partes <- texto_tramos(textos_candidatos[[i]], citas[[i]])
      citado[[i]] <- paste(
        c(normalizar(partes), normalizar(partes, desescapar = TRUE)),
        collapse = " "
      )
    }
  }
  agujas_citadas <- vector("list", length(pajar))
  golpea <- rep(FALSE, length(pajar))
  # La forma sin separadores CONTIENE a la aguja. En la PROSA del paquete -los
  # campos de `.CAMPOS_DE_PROSA`, y solo esos- se exige ademas que la aguja caiga
  # sin letras pegadas en el texto: no puede empezar ni terminar a mitad de una
  # palabra. Sin eso, la prosa del paquete se tapaba entera
  # por azar: con los nombres de millones de personas como agujas, alguno de seis
  # letras aparece cruzando palabras -"...COLUMNACONTIENE..."-, y en una base real
  # quedaron tapadas 38 de 64 descripciones y 37 de 64 sugerencias, tambien de
  # columnas que no eran personales. Lo que la regla existe para atrapar sigue
  # cayendo en limites: "771.771-01" contra el documento 77177101, "Maria Nunez"
  # contra el nombre escrito junto. Fuera de la prosa no se exige: un valor del
  # usuario -un ejemplo, una moda, una evidencia- con el nombre pegado a otras
  # letras es justo lo que hay que tapar. Medido en una refutacion:
  # "juanperezsrl@correo.uy" frente al titular protegido se publicaba en los
  # ejemplos de los patrones cuando la regla de limites valia en todas partes.
  for (aguja in agujas) {
    pendientes <- which(!golpea)
    if (!length(pendientes)) break
    contiene <- pendientes[Reduce(`|`, lapply(pajares, function(p) {
      grepl(aguja, p[pendientes], fixed = TRUE, useBytes = TRUE)
    }))]
    if (length(contiene)) {
      con_limites <- contiene[limites[contiene]]
      golpea[setdiff(contiene, con_limites)] <- TRUE
      if (length(con_limites)) {
        # Una palabra del paquete en su propia prosa no es un valor: solo cuenta
        # si aparece citada, que se mira abajo.
        golpea[con_limites] <- if (aguja %in% palabras_paquete) {
          FALSE
        } else {
          Reduce(`|`, lapply(crudos, function(forma) {
            .variante_en_limites(aguja, forma[con_limites])
          }))
        }
        en_cita <- con_limites[!golpea[con_limites]]
        en_cita <- en_cita[
          grepl(aguja, citado[en_cita], fixed = TRUE, useBytes = TRUE)
        ]
        agujas_citadas[en_cita] <- lapply(agujas_citadas[en_cita], c, aguja)
      }
    }
  }
  # Lo que se tapa por tramos en la prosa: la cita o el numero, no el texto.
  tramos_tapar <- vector("list", length(pajar))
  reemplazos_tapar <- vector("list", length(pajar))
  agregar_tramo <- function(i, tramos, reemplazo) {
    tramos_tapar[[i]] <<- rbind(tramos_tapar[[i]], tramos)
    reemplazos_tapar[[i]] <<- c(reemplazos_tapar[[i]], rep(reemplazo, NROW(tramos)))
  }
  for (i in which(lengths(agujas_citadas) > 0L & !golpea)) {
    partes <- texto_tramos(textos_candidatos[[i]], citas[[i]])
    normalizadas <- paste(normalizar(partes), normalizar(partes, desescapar = TRUE))
    tapar <- vapply(normalizadas, function(parte) {
      any(vapply(agujas_citadas[[i]], function(aguja) {
        grepl(aguja, parte, fixed = TRUE, useBytes = TRUE)
      }, logical(1L)))
    }, logical(1L), USE.NAMES = FALSE)
    if (any(tapar)) {
      agregar_tramo(i, citas[[i]][tapar, , drop = FALSE], "\"[valor protegido]\"")
    }
  }
  # Y los DIGITOS aparte, que es donde la variante no es cosmetica sino PARCIAL:
  # la cedula sin su verificador -"4.123.456" frente a "4.123.456-7"- normaliza a
  # "4123456", que NO contiene a "41234567", asi que la regla de arriba no la ve.
  # La regla vive en `R/regla-digitos.R`; aca se le dan los textos y los
  # documentos.
  #
  # Limitado a DIGITOS a proposito. La misma regla sobre texto -tapar una celda
  # que comparte seis caracteres con un valor protegido- taparia una palabra
  # corriente por compartir un tramo con un apellido, y eso silencia contenido
  # real del informe en vez de proteger un dato.
  # Extraer las corridas con `gregexpr()` sobre todas las agujas costaba 6,6 s
  # con 600.000 valores protegidos -medido en `perfilar_por()`, que compara tres
  # etiquetas-. Casi todas son solo digitos -un documento, un telefono- o no
  # tienen ninguno -un nombre-: esas se resuelven sin expresiones, y solo las
  # mezcladas pasan por `gregexpr()`.
  corridas_largas <- function(v) {
    piezas <- vector("list", length(v))
    solo_digitos <- !is.na(v) & grepl("^[0-9]+$", v, useBytes = TRUE)
    piezas[solo_digitos] <- as.list(v[solo_digitos])
    mezcladas <- which(!solo_digitos & !is.na(v) & grepl("[0-9]", v, useBytes = TRUE))
    if (length(mezcladas)) {
      piezas[mezcladas] <- strsplit(
        gsub("[^0-9]+", " ", v[mezcladas], useBytes = TRUE), " ", fixed = TRUE
      )
    }
    lapply(piezas, function(p) {
      p[!is.na(p) & nchar(p, type = "bytes") >= .MIN_LARGO_VALOR_IDENTIFICANTE]
    })
  }
  # Los documentos -corridas de las agujas, que pueden ser millones- se arman
  # SOLO si algun texto tiene seis digitos que probar: sin esa pereza,
  # `perfilar_por()` los rearmaba en cada grupo y costaba 8 s de 65.
  documentos <- NULL
  obtener_documentos <- function() {
    if (is.null(documentos)) {
      # Y la cifra entera del valor protegido escrito como numero de planilla
      # -"59893897037.0"-: sin separadores es "598938970370", otra cifra. Ver
      # `.cifra_entera_escrita()`.
      enteras <- .cifra_entera_escrita(valores)
      documentos <<- .documentos_digitos(c(
        unlist(corridas_largas(agujas_todas), use.names = FALSE),
        enteras[!is.na(enteras)]
      ))
    }
    documentos
  }
  seis_digitos <- function(v) {
    !is.na(v) & nchar(gsub("[^0-9]", "", v, useBytes = TRUE), type = "bytes") >=
      .MIN_LARGO_VALOR_IDENTIFICANTE
  }
  # Fuera de la prosa, la celda entera, en cualquiera de sus formas.
  for (forma in crudos) {
    probar <- which(!golpea & !limites & seis_digitos(forma))
    if (!length(probar)) next
    golpea[probar] <- .golpes_digitos(forma[probar], obtener_documentos())
  }
  # En la PROSA del paquete los numeros son conteos suyos -"12.497.500 pares
  # posibles"- y por su forma no se distinguen de un documento: con un millon de
  # cedulas alguno coincidia y la frase se tapaba entera. Ahi la regla mira solo
  # lo que la prosa cita -entre comillas dobles- y el codigo que sugiere -entre
  # comillas invertidas-: el umbral de una sugerencia, `~ nro_tramite <
  # 48889997`, es un valor de la tabla sin comillas, y era la cedula protegida.
  # Medido en una refutacion. Y se tapa el tramo, no la sugerencia entera.
  prosa <- which(limites & !golpea)
  numeros_tapar <- vector("list", length(pajar))
  if (length(prosa)) {
    duenos <- integer()
    filas <- integer()
    tramos_prosa <- vector("list", length(pajar))
    partes_prosa <- character()
    for (i in prosa) {
      tramos <- rbind(
        citas[[i]], .tramos_entre_delimitadores(textos_candidatos[[i]], "`")
      )
      if (!NROW(tramos)) next
      tipos <- c(rep("cita", NROW(citas[[i]])),
                 rep("codigo", NROW(tramos) - NROW(citas[[i]])))
      partes <- texto_tramos(textos_candidatos[[i]], tramos)
      # Contados DESPUES de plegar: una cita con digitos de ancho completo o
      # arabigo-indicos no tenia seis digitos ASCII y no se probaba. Medido en
      # la ronda 22.
      con_digitos <- seis_digitos(plegar(partes))
      if (!any(con_digitos)) next
      tramos_prosa[[i]] <- list(tramos = tramos[con_digitos, , drop = FALSE],
                                tipos = tipos[con_digitos])
      partes_prosa <- c(partes_prosa, partes[con_digitos])
      duenos <- c(duenos, rep(i, sum(con_digitos)))
      filas <- c(filas, seq_len(sum(con_digitos)))
    }
    if (length(partes_prosa)) {
      golpe <- .golpes_digitos(plegar(partes_prosa), obtener_documentos()) |
        .golpes_digitos(plegar(partes_prosa, desescapar = TRUE), obtener_documentos())
      for (k in which(golpe)) {
        i <- duenos[[k]]
        tramo <- tramos_prosa[[i]]$tramos[filas[[k]], , drop = FALSE]
        if (identical(tramos_prosa[[i]]$tipos[[filas[[k]]]], "cita")) {
          agregar_tramo(i, tramo, "\"[valor protegido]\"")
          next
        }
        # En el codigo, solo el numero: la sugerencia se sigue leyendo.
        parte <- partes_prosa[[k]]
        numeros <- tryCatch(
          regmatches(parte, gregexpr(.REGLA_DIGITOS$numero, parte, perl = TRUE))[[1L]],
          error = function(e) character()
        )
        culpables <- numeros[.golpes_digitos(plegar(numeros), obtener_documentos())]
        if (length(culpables)) {
          numeros_tapar[[i]] <- unique(c(numeros_tapar[[i]], culpables))
        } else {
          agregar_tramo(i, tramo, "`[valor protegido]`")
        }
      }
    }
  }
  for (i in which(lengths(reemplazos_tapar) > 0L & !golpea)) {
    textos_candidatos[[i]] <- .reemplazar_tramos_bytes(
      textos_candidatos[[i]], tramos_tapar[[i]], reemplazos_tapar[[i]]
    )
  }
  # El numero del codigo se tapa DESPUES de las citas: si estaba citado, la cita
  # ya lo tapo y aca no queda nada que reemplazar.
  for (i in which(lengths(numeros_tapar) > 0L & !golpea)) {
    texto <- textos_candidatos[[i]]
    marca <- Encoding(texto)
    for (numero in numeros_tapar[[i]][.orden_por_bytes(-nchar(numeros_tapar[[i]], type = "bytes"))]) {
      texto <- gsub(numero, "[valor protegido]", texto, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(texto) <- marca
    textos_candidatos[[i]] <- texto
  }
  textos_candidatos[golpea] <- "[valor protegido]"
  x[candidatas] <- textos_candidatos
  .reponer_marcas_paquete(x, apartado)
}

# Prefiltro para las dos reglas de reemplazo. Su costo era el producto de la
# cantidad de valores protegidos por la cantidad de textos de la salida -cada
# valor recorria todos los textos-, y en una base real los dos crecen con las
# filas: sobre 200.000 filas, la proteccion era la mitad de `perfilar()`. Si un
# valor aparece dentro de un texto, sus primeros `k` bytes aparecen como un tramo
# de `k` bytes de ese texto: se arma una vez el conjunto de esos tramos y solo se
# buscan los valores cuyo comienzo esta en el. Nunca descarta una coincidencia
# verdadera. Se trabaja en BYTES, igual que los `gsub(useBytes = TRUE)` que
# reemplazan, para no depender de la marca de codificacion; el byte nulo separa
# los textos y no puede formar parte de un valor.
.tramos_bytes <- function(textos, k) {
  textos <- textos[!is.na(textos)]
  if (!length(textos)) return(numeric())
  bytes <- as.integer(unlist(
    lapply(textos, function(texto) c(charToRaw(texto), as.raw(0L))),
    use.names = FALSE
  ))
  n <- length(bytes) - k + 1L
  if (n < 1L) return(numeric())
  clave <- numeric(n)
  for (j in seq_len(k)) clave <- clave * 256 + bytes[j:(j + n - 1L)]
  unique(clave)
}

# Recibe valores de `k` bytes o mas. Los primeros `k` bytes de todos se toman con
# una sola expresion en modo bytes -`(?s)` para que el punto cruce un salto de
# linea- y se pegan: una llamada por valor costaba 2 s sobre 600.000 valores. Si
# algun valor no diera `k` bytes, el pegado se desalinearia; ahi se vuelve al
# camino de a uno.
.comienzo_bytes <- function(valores, k) {
  potencias <- 256^((k - 1L):0L)
  if (!length(valores)) return(numeric())
  prefijos <- sub(
    paste0("(?s)^(.{", k, "}).*$"), "\\1", valores,
    perl = TRUE, useBytes = TRUE
  )
  Encoding(prefijos) <- "bytes"
  bytes <- as.integer(charToRaw(paste(prefijos, collapse = "")))
  if (length(bytes) != k * length(valores)) {
    return(vapply(valores, function(valor) {
      sum(as.integer(charToRaw(valor)[seq_len(k)]) * potencias)
    }, numeric(1L), USE.NAMES = FALSE))
  }
  colSums(matrix(bytes, nrow = k) * potencias)
}

.valores_que_pueden_aparecer <- function(valores, textos, k) {
  largos <- nchar(valores, type = "bytes", allowNA = TRUE)
  largos_ok <- !is.na(largos) & largos >= k
  if (!any(largos_ok)) return(valores)
  presentes <- .tramos_bytes(textos, k)
  posibles <- !largos_ok
  posibles[largos_ok] <- .comienzo_bytes(valores[largos_ok], k) %in% presentes
  valores[posibles]
}

# Los nombres de columna que un texto escribe entre delimitadores -`nombre`,
# "nombre", [nombre]- se apartan antes de comparar y se reponen despues. Cada uno
# se reemplaza por un codigo propio de caracteres de control -su numero en base
# cuatro con \003 a \006, entre dos \002-: la normalizacion los borra, no forman
# digitos ni letras, y al reponer cada codigo dice que nombre era. Un orden de
# aparicion no alcanzaria: si la proteccion tapa una cita que llevaba un nombre,
# los que siguen se correrian uno.
#
# La comparacion de cada pieza con los nombres es por BYTES, con una clave
# propia, y no con `match()`. Las piezas salen de `regmatches()` sobre una
# coincidencia en modo bytes, y R marca `bytes` a las que no son ASCII; los
# nombres llegan en UTF-8. `match()` entre las dos marcas ABORTA -"no se permite
# traduccion de cadenas con bytes"- en cuanto hay dos piezas, y con una sola no
# aborta pero tampoco encuentra. Medido en la ronda 25: `perfilar()`,
# `analizar()`, `perfilar_por()` y `perfilar_dbi()`, con la proteccion por
# omision, abortaban sobre una planilla con `Anio Ingreso` -con enie- y una
# cedula -la evidencia del nombre no sintactico cita las dos formas- y sobre
# cualquier base con una columna acentuada -la SQL guardada cita todas-. El
# codigo de cada nombre es su orden entre las piezas ENCONTRADAS, y al reponer
# se escriben esos mismos bytes: lo que vuelve al texto es lo que estaba, con la
# marca que tenia.
.clave_bytes_nombre <- function(textos, marcas = Encoding(textos)) {
  marcas <- rep_len(marcas, length(textos))
  vapply(seq_along(textos), function(i) {
    texto <- textos[[i]]
    # Un texto declarado latin1 se compara por su forma UTF-8, que es la de los
    # nombres que el paquete marca; lo que no declara nada se toma como esta.
    if (identical(marcas[[i]], "latin1")) {
      convertido <- iconv(texto, from = "latin1", to = "UTF-8")
      if (!is.na(convertido)) texto <- convertido
    }
    paste(as.integer(charToRaw(texto)), collapse = ".")
  }, character(1L), USE.NAMES = FALSE)
}

.apartar_nombres_delimitados <- function(x, nombres) {
  sin_cambios <- list(x = x, nombres = character())
  nombres <- unique(nombres[!is.na(nombres) & nzchar(nombres)])
  if (!is.character(x) || !length(x) || !length(nombres)) return(sin_cambios)
  indices <- which(
    !is.na(x) & grepl("[`\"\\[]", x, perl = TRUE, useBytes = TRUE) &
      !grepl("\002", x, fixed = TRUE, useBytes = TRUE)
  )
  if (!length(indices)) return(sin_cambios)
  codigo <- function(i) {
    cifras <- integer()
    repeat {
      cifras <- c(i %% 4L, cifras)
      i <- i %/% 4L
      if (!i) break
    }
    paste0("\002", rawToChar(as.raw(3L + cifras)), "\002")
  }
  claves_nombres <- .clave_bytes_nombre(nombres)
  patron <- "`[^`\n]+`|\"[^\"\n]+\"|\\[[^]\n]+\\]"
  elegidos <- x[indices]
  marcas <- Encoding(elegidos)
  # Todo en bytes desde aca: las piezas, su interior y el reensamblado.
  Encoding(elegidos) <- "bytes"
  coincidencias <- gregexpr(patron, elegidos, perl = TRUE, useBytes = TRUE)
  piezas <- regmatches(elegidos, coincidencias)
  encontrados <- character()
  claves_encontradas <- character()
  for (k in seq_along(piezas)) {
    p <- piezas[[k]]
    if (!length(p)) next
    Encoding(p) <- "bytes"
    interior <- substr(p, 2L, nchar(p, type = "bytes") - 1L)
    Encoding(interior) <- "bytes"
    cambiar <- .clave_bytes_nombre(interior, marcas[[k]]) %in% claves_nombres
    if (!any(cambiar)) next
    crudas <- .clave_bytes_nombre(interior[cambiar], "bytes")
    nuevas <- !crudas %in% claves_encontradas
    encontrados <- c(encontrados, interior[cambiar][nuevas])
    claves_encontradas <- c(claves_encontradas, crudas[nuevas])
    posicion <- match(crudas, claves_encontradas)
    p[cambiar] <- paste0(
      substr(p[cambiar], 1L, 1L),
      vapply(posicion, codigo, character(1L)),
      substr(p[cambiar], nchar(p[cambiar], type = "bytes"),
             nchar(p[cambiar], type = "bytes"))
    )
    piezas[[k]] <- p
  }
  if (!length(encontrados)) return(sin_cambios)
  regmatches(elegidos, coincidencias) <- piezas
  Encoding(elegidos) <- marcas
  x[indices] <- elegidos
  Encoding(encontrados) <- "bytes"
  list(x = x, nombres = encontrados)
}

.reponer_nombres_delimitados <- function(x, apartado) {
  if (!length(apartado$nombres) || !is.character(x)) return(x)
  con_codigo <- which(!is.na(x) & grepl("\002", x, fixed = TRUE, useBytes = TRUE))
  for (i in con_codigo) {
    texto <- x[[i]]
    marca <- Encoding(texto)
    Encoding(texto) <- "bytes"
    coincidencias <- gregexpr("\002[\003-\006]+\002", texto, perl = TRUE,
                              useBytes = TRUE)
    codigos <- regmatches(texto, coincidencias)[[1L]]
    if (!length(codigos)) next
    nombres <- vapply(codigos, function(cod) {
      cifras <- as.integer(charToRaw(substr(cod, 2L, nchar(cod, type = "bytes") - 1L))) - 3L
      posicion <- sum(cifras * 4L^rev(seq_along(cifras) - 1L))
      if (posicion >= 1L && posicion <= length(apartado$nombres)) {
        apartado$nombres[[posicion]]
      } else ""
    }, character(1L), USE.NAMES = FALSE)
    Encoding(nombres) <- "bytes"
    regmatches(texto, coincidencias) <- list(nombres)
    Encoding(texto) <- marca
    x[[i]] <- texto
  }
  x
}

# Las marcas del paquete -`<blanco>`, `grupo_maximo`, ver `.LEXICO_PAQUETE`- se
# apartan antes de comparar y se reponen despues: son estructura, y un apellido
# `Blanco` protegido tapaba toda evidencia que dijera `<blanco>`. Se reemplazan
# por caracteres de control que la normalizacion borra; un texto que ya los
# trae no se toca.
#
# Cada aparicion lleva su propio codigo -su numero de orden en base cuatro con
# \003 a \006, entre dos \001-, como los nombres delimitados: con un \001 por
# marca y reposicion por ORDEN, si la proteccion tapaba un tramo que llevaba una
# marca las siguientes se corrian una, y el texto publicado nombraba otra
# columna -`escribir a [valor protegido], con copia a juan_perez` donde decia
# `maria_lopez`-. Medido en la ronda 24.
.codigo_de_orden <- function(i, borde) {
  cifras <- integer()
  repeat {
    cifras <- c(i %% 4L, cifras)
    i <- i %/% 4L
    if (!i) break
  }
  paste0(borde, rawToChar(as.raw(3L + cifras)), borde)
}

.orden_de_codigo <- function(codigos) {
  vapply(codigos, function(cod) {
    cifras <- as.integer(charToRaw(substr(cod, 2L, nchar(cod, type = "bytes") - 1L))) - 3L
    as.integer(sum(cifras * 4L^rev(seq_along(cifras) - 1L)))
  }, integer(1L), USE.NAMES = FALSE)
}

.apartar_marcas_paquete <- function(x, nombres = character(), lexico = TRUE) {
  sin_cambios <- list(x = x, indices = integer(), guardadas = list(),
                      codificaciones = character())
  marcas <- unique(c(if (lexico) .LEXICO_PAQUETE$marcas, nombres))
  if (!is.character(x) || !length(x) || !length(marcas)) return(sin_cambios)
  patron <- "<[a-z_]+>|[a-z0-9]+(_[a-z0-9]+)+"
  indices <- which(
    !is.na(x) & grepl("<[a-z_]+>|[a-z0-9]_[a-z0-9]", x, perl = TRUE,
                      useBytes = TRUE) &
      !grepl("\001", x, fixed = TRUE, useBytes = TRUE)
  )
  if (!length(indices)) return(sin_cambios)
  elegidos <- x[indices]
  codificaciones <- Encoding(elegidos)
  coincidencias <- gregexpr(patron, elegidos, perl = TRUE, useBytes = TRUE)
  tokens <- regmatches(elegidos, coincidencias)
  guardadas <- lapply(tokens, function(t) t[t %in% marcas])
  con_marca <- lengths(guardadas) > 0L
  if (!any(con_marca)) return(sin_cambios)
  regmatches(elegidos, coincidencias) <- lapply(tokens, function(t) {
    es_marca <- t %in% marcas
    t[es_marca] <- vapply(seq_len(sum(es_marca)), .codigo_de_orden,
                          character(1L), borde = "\001")
    t
  })
  Encoding(elegidos) <- codificaciones
  x[indices[con_marca]] <- elegidos[con_marca]
  list(x = x, indices = indices[con_marca], guardadas = guardadas[con_marca],
       codificaciones = codificaciones[con_marca])
}

.reponer_marcas_paquete <- function(x, apartado) {
  if (!length(apartado$indices)) return(x)
  for (k in seq_along(apartado$indices)) {
    i <- apartado$indices[[k]]
    texto <- x[[i]]
    if (is.na(texto) || !grepl("\001", texto, fixed = TRUE, useBytes = TRUE)) next
    marcas <- apartado$guardadas[[k]]
    coincidencias <- gregexpr("\001[\003-\006]+\001", texto, perl = TRUE,
                              useBytes = TRUE)
    codigos <- regmatches(texto, coincidencias)[[1L]]
    if (length(codigos)) {
      posicion <- .orden_de_codigo(codigos)
      repuestas <- ifelse(posicion >= 1L & posicion <= length(marcas),
                          marcas[pmax(1L, pmin(posicion, length(marcas)))], "")
      regmatches(texto, coincidencias) <- list(repuestas)
    }
    # Un codigo que la proteccion corto a medias no se publica.
    texto <- gsub("\001[\003-\006]*\001?", "", texto, perl = TRUE,
                  useBytes = TRUE)
    Encoding(texto) <- apartado$codificaciones[[k]]
    x[[i]] <- texto
  }
  x
}

# Los valores protegidos de una sola palabra que el paquete tambien escribe en
# su prosa -`Maximo`, `Patron`, `Constante`-, segun su forma plegada.
.valores_que_son_palabras_del_paquete <- function(valores) {
  palabras <- .LEXICO_PAQUETE$palabras
  if (!length(valores) || !length(palabras)) return(character())
  candidatos <- valores[
    !is.na(valores) &
      !grepl("[[:space:][:digit:][:punct:]]", valores, useBytes = TRUE) &
      nchar(valores, type = "bytes") <= 80L
  ]
  if (!length(candidatos)) return(character())
  plegados <- tryCatch(
    gsub("[^\\p{L}\\p{N}]", "", .plegar_para_comparar(candidatos), perl = TRUE),
    error = function(e) rep(NA_character_, length(candidatos))
  )
  candidatos[!is.na(plegados) & plegados %in% palabras]
}

# Los valores protegidos que CONTIENEN alguna de las marcas o nombres de columna
# `presentes` -en cualquier caja, con otros separadores-, de los que pueden
# aparecer en `textos`. Se comparan sin separadores y plegados, y la marca tiene
# que ser MAS CORTA que el valor: un valor que es la marca misma -el apellido
# `Blanco` frente a `<blanco>`- no la contiene, y la marca sigue a salvo.
.clave_estructura <- function(v) {
  v <- as.character(v)
  salida <- rep(NA_character_, length(v))
  ascii <- !is.na(v) & !grepl("[^\\001-\\177]", v, perl = TRUE, useBytes = TRUE)
  salida[ascii] <- gsub(
    "[^a-z0-9]+", "",
    chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", v[ascii]),
    perl = TRUE
  )
  otros <- !is.na(v) & !ascii
  if (any(otros)) {
    salida[otros] <- tryCatch(
      gsub("[^\\p{L}\\p{N}]", "", .plegar_para_comparar(v[otros]), perl = TRUE),
      error = function(e) NA_character_
    )
  }
  salida
}

.valores_con_estructura <- function(valores, presentes, textos) {
  presentes <- presentes[!is.na(presentes) & nzchar(presentes)]
  if (!length(valores) || !length(presentes)) return(character())
  con_letra <- which(
    !is.na(valores) &
      grepl("[A-Za-z]|[^\\001-\\177]", valores, perl = TRUE, useBytes = TRUE)
  )
  if (!length(con_letra)) return(character())
  claves_textos <- .clave_estructura(textos)
  claves_textos <- claves_textos[!is.na(claves_textos) & nzchar(claves_textos)]
  if (!length(claves_textos)) return(character())
  claves_valores <- .clave_estructura(valores[con_letra])
  candidatas <- !is.na(claves_valores) & claves_valores %in%
    .valores_que_pueden_aparecer(unique(claves_valores), claves_textos, 4L)
  if (!any(candidatas)) return(character())
  contienen <- .contiene_estructura(
    valores[con_letra][candidatas], claves_valores[candidatas], presentes
  )
  unique(valores[con_letra][candidatas][contienen])
}

# Si cada valor contiene alguna de las marcas o nombres `presentes`. Una marca
# de etiqueta -`<br>`- se busca tal cual, en cualquier caja; un nombre, por su
# clave plegada sin separadores, de cuatro caracteres o mas y mas corta que la
# del valor: una columna `d` esta dentro de casi cualquier valor y no dice nada,
# y con ella "segundo" pasaba a contener estructura. Medido en la ronda 24.
.contiene_estructura <- function(valores, claves_valores, presentes) {
  contienen <- logical(length(valores))
  if (!length(valores)) return(contienen)
  etiqueta <- grepl("^<[a-z_]+>$", presentes, perl = TRUE, useBytes = TRUE)
  if (any(etiqueta)) {
    minusculas <- tryCatch(
      chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", valores),
      error = function(e) valores
    )
    for (marca in unique(presentes[etiqueta])) {
      contienen <- contienen | grepl(marca, minusculas, fixed = TRUE, useBytes = TRUE)
    }
  }
  claves <- unique(.clave_estructura(presentes[!etiqueta]))
  claves <- claves[!is.na(claves) & nchar(claves, type = "bytes") >= 4L]
  for (marca in claves) {
    contienen <- contienen | (
      !is.na(claves_valores) &
        nchar(claves_valores, type = "bytes") > nchar(marca, type = "bytes") &
        grepl(marca, claves_valores, fixed = TRUE, useBytes = TRUE)
    )
  }
  contienen
}

.reemplazar_valores_protegidos <- function(x, valores, exigir_limites = FALSE,
                                           intocables = character(),
                                           apartar_iguales = FALSE,
                                           nombres_iguales = NULL) {
  if (!is.character(x) || !length(valores)) return(x)
  # DE MAS LARGO A MAS CORTO, y una sola vez cada uno. Las dos cosas arreglan una
  # fuga medida con el piso del propio paquete -seis caracteres identifican-:
  #
  # Sustituyendo valor por valor EN EL ORDEN DE LA LISTA, un valor protegido que
  # es prefijo de otro deja publicada la cola del largo. Medido con
  # `c("Maria Nunez", "Maria Nunez de Castro")` sobre
  # `"beneficiaria: Maria Nunez de Castro"`:
  #
  #   antes:  beneficiaria: [valor protegido] de Castro   <- 9 caracteres afuera
  #   ahora:  beneficiaria: [valor protegido]
  #
  # Y no era un caso de laboratorio: los valores salen de las celdas de las
  # columnas protegidas EN EL ORDEN DE LAS FILAS, asi que cual va primero depende
  # de como este ordenada la tabla.
  #
  # El `unique()` cierra otra: repetir un valor lo sustituye dos veces, y si ese
  # valor aparece dentro del marcador -`protegido`, por ejemplo- la segunda pasada
  # corrompe el marcador y anida `[valor [valor protegido]]`.
  #
  # El orden NO se toca en `.reemplazar_variantes_separadas()`: alli las agujas
  # solo se buscan para enmascarar el elemento entero, y cual case primero no
  # cambia el resultado.
  valores <- unique(valores)
  valores <- valores[.orden_por_bytes(-nchar(valores, type = "bytes"))]
  # La regla de variantes recibe TODOS los valores: el prefiltro de aca mira la
  # escritura exacta, y una variante no la comparte.
  #
  # Los NOMBRES DE COLUMNA tambien son estructura cuando el texto los nombra: con
  # "Segundo" -un nombre de pila corriente- protegido, la SQL guardada de
  # `perfilar_dbi()` decia `COUNT(\`[valor protegido]_nombre\`)` y la prosa
  # nombraba una columna que no existe. Medido a partir de la cuarta evaluacion
  # real. Se apartan solo los que van entre comillas -invertidas, dobles o
  # corchetes, como los escriben el paquete y la SQL- o con la forma de una marca
  # -`segundo_nombre`-: un nombre corto suelto podria estar adentro de un valor.
  # Un nombre que es el mismo un valor protegido no se aparta.
  #
  # Salvo en la SQL que el paquete escribe (`apartar_iguales`): ahi un nombre
  # citado es un identificador, y el nombre se publica igual en
  # `columnas$columna`. Con un nombre de columna igual a un valor protegido salvo
  # la tilde, `perfilar_dbi()` tapaba ENTERAS las 33 sentencias de su lote -que
  # citan tambien `id` y las demas columnas- mientras la SQL de la muestra lo
  # citaba igual. Medido en la ronda 26.
  nombres <- if (isTRUE(apartar_iguales)) {
    setdiff(unique(as.character(intocables)), "")
  } else setdiff(unique(as.character(intocables)), c(valores, ""))
  if (length(nombres) && !isTRUE(apartar_iguales) && !is.null(nombres_iguales)) {
    # Ya calculados por quien llama (`.nombres_iguales_a_valores()`).
    nombres <- setdiff(nombres, nombres_iguales)
  } else if (length(nombres) && !isTRUE(apartar_iguales)) {
    # Tampoco uno que es el valor con otra caja o con otros separadores: una
    # columna `juan_perez` frente al titular protegido "Juan Perez" dejaria
    # publicado "juan_perez" en una celda. `tolower()` aborta sobre bytes que no
    # son UTF-8 valido, y entonces se compara sin plegar la caja.
    # Con el mismo pliegue que la proteccion -tildes, escritura, codificacion-:
    # con `tolower()` y clases por byte, la columna `jose_perez` frente a "Jose
    # Perez" con tildes se exceptuaba y el valor salia. Medido en la ronda 22.
    clave <- function(v) {
      plegado <- tryCatch(.plegar_para_comparar(v), error = function(e) as.character(v))
      tryCatch(
        gsub("[^\\p{L}\\p{N}]", "", plegado, perl = TRUE),
        error = function(e) gsub("[^[:alnum:]]", "", plegado, useBytes = TRUE)
      )
    }
    nombres <- nombres[!clave(nombres) %in% clave(valores)]
  }
  exacta <- function(textos, valores) {
    valores <- .valores_que_pueden_aparecer(valores, textos, 3L)
    for (valor in valores) {
      largo <- nchar(valor, type = "bytes")
      if (is.na(largo) || !largo) next
      if (largo < 3L) {
        escapado <- gsub(
          "([][{}()+*^$|\\\\?.])", "\\\\\\1", valor,
          fixed = FALSE, useBytes = TRUE
        )
        patron <- paste0(
          "(?<![[:alnum:]_])", escapado, "(?![[:alnum:]_])"
        )
        textos <- tryCatch(
          gsub(patron, "[valor protegido]", textos, perl = TRUE, useBytes = TRUE),
          error = function(e) gsub(
            valor, "[valor protegido]", textos, fixed = TRUE, useBytes = TRUE
          )
        )
      } else {
        textos <- gsub(valor, "[valor protegido]", textos, fixed = TRUE,
                       useBytes = TRUE)
      }
    }
    textos
  }
  # En la prosa del paquete, un valor de una sola palabra que el paquete tambien
  # escribe no se busca tal cual: "Patron dominante" es su texto. Lo citado
  # entre comillas lo sigue tapando la regla de variantes, con la regla fuerte.
  limites <- rep_len(as.logical(exigir_limites), length(x))
  limites[is.na(limites)] <- FALSE
  pasada <- function(textos, valores, limites) {
    if (!length(textos) || !length(valores)) return(textos)
    del_paquete <- if (any(limites)) {
      .valores_que_son_palabras_del_paquete(valores)
    } else character()
    if (length(del_paquete)) {
      textos[limites] <- exacta(textos[limites], setdiff(valores, del_paquete))
      textos[!limites] <- exacta(textos[!limites], valores)
    } else {
      textos <- exacta(textos, valores)
    }
    .reemplazar_variantes_separadas(textos, valores, limites)
  }
  # Un valor protegido que CONTIENE una marca o un nombre de columna -el correo
  # `juan_perez@empresa.com.uy` con una columna `juan_perez`- se busca ANTES de
  # apartar, en los textos que tienen alguno: si no, al texto se le aparta el
  # nombre y el valor ya no esta. La ronda 23 apartaba el nombre tambien de la
  # aguja, y eso dejaba escapar el valor con otra caja -`JUAN_PEREZ@...`- y
  # tapaba de mas: la aguja recortada `@empresa.com.uy` casaba con el dominio y
  # con el correo de cualquier otra columna. Medido en la ronda 24. Antes de
  # apartar, lo que hay en el texto es el valor entero; despues, la estructura
  # sigue a salvo de los valores que no la contienen.
  previo <- .apartar_marcas_paquete(x, nombres)
  previo_delimitados <- .apartar_nombres_delimitados(x, nombres)
  tocados <- .sort_por_bytes(unique(c(previo$indices, which(
    !is.na(x) & !is.na(previo_delimitados$x) & previo_delimitados$x != x
  ))))
  if (length(tocados)) {
    presentes <- unique(c(
      unlist(previo$guardadas, use.names = FALSE),
      nombres[vapply(nombres, function(n) {
        any(grepl(n, x[tocados], fixed = TRUE, useBytes = TRUE))
      }, logical(1L), USE.NAMES = FALSE)]
    ))
    estructura <- .valores_con_estructura(valores, presentes, x[tocados])
    if (length(estructura)) {
      # Y en esta pasada siguen apartados los nombres y las marcas que ninguno de
      # esos valores contiene: sin eso, "segundo" -protegido, y dentro de un
      # valor con estructura- se buscaba dentro de `segundo_nombre` en la SQL.
      # Medido en la ronda 24.
      claves_estructura <- .clave_estructura(estructura)
      contenida <- vapply(presentes, function(marca) {
        any(.contiene_estructura(estructura, claves_estructura, marca))
      }, logical(1L), USE.NAMES = FALSE)
      resguardo <- presentes[!contenida]
      textos <- x[tocados]
      resguardo_nombres <- .apartar_nombres_delimitados(
        textos, intersect(nombres, resguardo)
      )
      resguardo_marcas <- .apartar_marcas_paquete(
        resguardo_nombres$x, resguardo, lexico = FALSE
      )
      textos <- pasada(resguardo_marcas$x, estructura, limites[tocados])
      textos <- .reponer_marcas_paquete(textos, resguardo_marcas)
      x[tocados] <- .reponer_nombres_delimitados(textos, resguardo_nombres)
    }
  }
  nombres_apartados <- .apartar_nombres_delimitados(x, nombres)
  x <- nombres_apartados$x
  apartado <- .apartar_marcas_paquete(x, nombres)
  x <- apartado$x
  x <- pasada(x, valores, limites)
  x <- .reponer_marcas_paquete(x, apartado)
  .reponer_nombres_delimitados(x, nombres_apartados)
}

# Recorre la parte textual de una salida sin convertir estadisticos numericos
# que no sean valores de celda. Los parametros del plan tienen un recorrido
# adicional para quitar tambien valores numericos que todavia no se volvieron
# texto.
# EL RECORRIDO, separado de lo que se hace con cada hoja. Lo usan las dos pasadas
# de `.proteger_textos_salida()`, asi que las dos visitan las mismas hojas en el
# mismo orden por construccion -y los atributos se siguen protegiendo igual-.
# ¿Esta columna de una tabla de salida guarda NOMBRES DE COLUMNA de la entrada?
#
# Se decide por el CONTENIDO y no por el nombre del campo: lo es si todos sus
# valores no vacios son nombres de la entrada, solos o unidos por los separadores
# que el paquete usa para las referencias a varias columnas -`, `, `+`, ` | `-.
# Una lista de campos escrita a mano -`columna`, `determinante`, `dependiente`...-
# se habria quedado corta con el primero que se agregara, que es la forma en que
# este paquete ya perdio varias guardas.
#
# Y no puede ser "proteger los nombres de columna en todas partes": medido sobre
# el caso que lo destapo, el VALOR `"documento"` es igual al NOMBRE de columna
# `documento`, asi que proteger el nombre en todos lados dejaba de enmascarar
# `columnas$moda = "documento"` y filtraba el valor. La proteccion va por campo.
.es_columna_de_nombres <- function(x, nombres) {
  if (!length(nombres) || !length(x)) return(FALSE)
  valores <- as.character(x)
  valores <- valores[!is.na(valores) & nzchar(valores)]
  if (!length(valores)) return(FALSE)
  claves <- .nombres_para_operar(nombres)
  # La tabla, como la nombra el paquete donde una medida no es de una columna
  # -`(tabla)`, `(tabla: t1)`-, tambien es estructura: `objeto` mezcla las dos
  # cosas. Ronda 28.
  tabla <- "^\\(tabla(: [^()]+)?\\)$"
  exactos <- .nombres_para_operar(valores) %in% claves |
    grepl(tabla, valores, perl = TRUE, useBytes = TRUE)
  if (all(exactos)) return(TRUE)
  # Los que no son un nombre exacto se prueban como referencia a varios: se
  # parten por los separadores del paquete y cada pieza tiene que ser un nombre.
  # Primero el valor entero, porque un nombre de columna puede llevar una coma.
  restantes <- valores[!exactos]
  # Por bytes, como el resto del barrido: sin `useBytes`, `strsplit()` AVISA
  # "input string 1 is invalid UTF-8" por cada texto `latin1` sin marca -el que
  # deja `read.csv()` sin declarar la codificacion- y, con `options(warn = 2)`,
  # `perfilar()` abortaba. Medido en la ronda 27. Los separadores son ASCII, asi
  # que cortar por bytes da las mismas piezas; se les devuelve la marca del texto,
  # que el corte por bytes no conserva.
  compuestos <- vapply(restantes, function(v) {
    piezas <- strsplit(v, "\\s*(,|\\+|\\||->)\\s*", perl = TRUE,
                       useBytes = TRUE)[[1L]]
    marca <- Encoding(v)
    if (marca %in% c("latin1", "UTF-8")) Encoding(piezas) <- marca
    piezas <- piezas[nzchar(piezas)]
    length(piezas) > 1L && all(.nombres_para_operar(piezas) %in% claves |
                                 grepl(tabla, piezas, perl = TRUE,
                                       useBytes = TRUE))
  }, logical(1L), USE.NAMES = FALSE)
  all(compuestos)
}

# Los NOMBRES QUE EL PAQUETE DERIVA de un nombre de columna son estructura como
# el nombre del que salen: el `make.names()` y el snake_case que el plan propone
# en `nombres_propuestos`, y las marcas `.ausente_<nombre>` y `.outlier_<nombre>`
# que sus acciones crean. Sin esto el barrido los trataba como texto de los
# datos: con "Segundo" -un nombre de pila- protegido en `Primer Nombre`, la
# evidencia decia `"Segundo Nombre" -> "[valor protegido].Nombre"`, el plan
# PROPONIA ese nombre y `aplicar()` se lo ponia a la columna en los datos del
# usuario. Medido en la ronda 26, en la llamada por omision.
#
# No abre nada que el nombre de columna no abra ya: el derivado se escribe con
# las letras del nombre, que se publica por decision. Y valen las mismas
# excepciones, porque recorren la misma lista: un derivado que, plegado, ES un
# valor protegido no se aparta en `.reemplazar_valores_protegidos()`.
#
# Se calcula una vez por lista de nombres: el barrido corre muchas veces por
# salida con los mismos nombres, y derivar mil nombres cuesta 0,2 s. Se
# recuerdan las ultimas ocho listas, porque una misma corrida alterna entre los
# nombres de la tabla y los del perfil.
.memo_nombres_derivados <- new.env(parent = emptyenv())
.memo_nombres_derivados$entradas <- list()

.nombres_derivados_de_columnas <- function(nombres) {
  nombres <- as.character(nombres)
  nombres <- nombres[!is.na(nombres) & nzchar(nombres)]
  if (!length(nombres)) return(character())
  for (entrada in .memo_nombres_derivados$entradas) {
    if (identical(entrada$nombres, nombres)) return(entrada$derivados)
  }
  # Si el nombre no se puede derivar -bytes que no son UTF-8 valido-, no hay
  # derivado que exceptuar: se barre como antes, que es el lado cerrado.
  derivados <- tryCatch({
    # El vector entero -`nombres_propuestos`, con sus sufijos de unicidad- y
    # cada nombre solo -la marca de una columna se deriva de ese nombre-.
    sintacticos <- unique(c(
      .nombres_make_names(nombres),
      vapply(nombres, .nombres_make_names, character(1L), USE.NAMES = FALSE)
    ))
    unique(c(
      sintacticos, .nombres_snake(nombres),
      paste0(".ausente_", sintacticos), paste0(".outlier_", sintacticos)
    ))
  }, error = function(e) character())
  derivados <- setdiff(derivados[!is.na(derivados)], nombres)
  .memo_nombres_derivados$entradas <- c(
    list(list(nombres = nombres, derivados = derivados)),
    utils::head(.memo_nombres_derivados$entradas, 7L)
  )
  derivados
}

# El VOCABULARIO del paquete es estructura, igual que los nombres de columna: un
# tipo de hallazgo, una severidad, una estrategia, un estado o el nombre de un
# diagnostico o de una metrica no son datos de nadie, los escribio el paquete. El
# barrido los trataba como cualquier texto, y como enmascara por CONTENIDO -una
# celda que, sin separadores y en mayusculas, contiene un valor protegido de seis
# caracteres o mas se tapa entera-, bastaba un nombre de persona contenido en
# `faltantes_disfrazados` para que el tipo saliera `[valor protegido]`. Medido en
# la tercera evaluacion real, sobre tres columnas de nombres de persona: seis
# hallazgos sin tipo; y reproducido aca, el plan quedaba ademas SIN NINGUNA
# accion para ellos, porque ninguna estrategia casa con un tipo que no se lee.
#
# La decision sigue siendo por campo y por contenido, la misma de los nombres
# (`.es_columna_de_nombres()`): se recolecta lo que el objeto trae en sus campos
# de vocabulario y un campo cuyos valores son TODOS de ahi queda afuera del
# barrido. Un campo que mezcla vocabulario y valores, no. La lista de nombres de
# campo solo decide DE DONDE se recolecta: un campo de vocabulario que falte aca
# se sigue barriendo, que es el lado seguro para la privacidad.
.CAMPOS_DE_VOCABULARIO <- c(
  "tipo_hallazgo", "hallazgo", "estrategia", "severidad", "severidad_origen",
  "estado", "estado_reparacion", "estado_tipo_inferido", "unidad_conteo",
  "decision_grupo", "diagnostico", "metrica", "metrica_especifica",
  "dimension", "factor", "orientacion", "granularidad", "tipo_resultado",
  "agregacion", "componente", "tipo", "cambio", "aspecto", "direccion", "nivel",
  # La unidad de una columna -`segundos` en las fechas, la de un objeto `units`-
  # no es el dato de nadie, y `Segundo` protegido tapaba la de toda columna de
  # fechas. Medido en una refutacion.
  "unidad"
)

# La SQL que el paquete escribe y guarda -`sql`, `sql_esquema`, `sql_muestra`,
# `sql_conteo_filas`...-: ahi los nombres citados son identificadores, tambien
# el que es igual a un valor protegido (ver `.reemplazar_valores_protegidos()`).
# Un campo de SQL con otro nombre se barre como cualquier otro, que es el lado
# seguro.
.PATRON_CAMPOS_DE_SQL <- "^sql(_[a-z_]+)?$"

# La PROSA del paquete: los campos que explican un diagnostico con frases. En
# ellos la regla de variantes exige limites de palabra -ver
# `.reemplazar_variantes_separadas()`-; en cualquier otro campo, no. Un campo de
# prosa que falte aca se barre con la regla fuerte, que es el lado seguro.
.CAMPOS_DE_PROSA <- c(
  "descripcion", "sugerencia", "motivo", "como_resolverlo", "justificacion",
  "recomendacion_grupo"
)

.vocabulario_de_objeto <- function(x, profundidad = 0L) {
  if (profundidad > 6L || is.null(x)) return(character())
  vocabulario <- character()
  adicionales <- setdiff(
    names(attributes(x)), c("names", "class", "row.names", "dim", "dimnames")
  )
  for (atributo in adicionales) {
    vocabulario <- c(vocabulario, .vocabulario_de_objeto(
      attr(x, atributo, exact = TRUE), profundidad + 1L
    ))
  }
  if (inherits(x, "data.frame")) {
    for (campo in intersect(names(x), .CAMPOS_DE_VOCABULARIO)) {
      columna <- x[[campo]]
      if (is.factor(columna)) {
        vocabulario <- c(vocabulario, levels(columna))
      } else if (is.character(columna)) {
        vocabulario <- c(vocabulario, columna)
      }
    }
    for (columna in x) {
      if (is.list(columna) && !is.data.frame(columna)) {
        for (elemento in columna) {
          if (is.list(elemento)) {
            vocabulario <- c(vocabulario,
                             .vocabulario_de_objeto(elemento, profundidad + 1L))
          }
        }
      }
    }
  } else if (is.list(x)) {
    for (elemento in x) {
      vocabulario <- c(vocabulario, .vocabulario_de_objeto(elemento, profundidad + 1L))
    }
  }
  vocabulario <- vocabulario[!is.na(vocabulario) & nzchar(vocabulario)]
  unique(vocabulario)
}

# El vocabulario que el objeto trae en sus campos de vocabulario no ampara un
# valor protegido: un campo de vocabulario puede llevar texto del usuario -la
# `unidad` es el atributo `units` de su columna-, y `attr(y, "units") <- "CI
# 3.006.514-3"` se publicaba entero en `columnas$unidad` por ser "vocabulario".
# Medido en la ronda 29. Sale del vocabulario lo que lleva un valor protegido
# ENTRE LIMITES de palabra -la regla de la prosa-, sin contar los nombres de
# columna que cita -un hallazgo `columnas_duplicadas@Juan Perez Gonzalez`
# nombra la columna-: "segundos" frente al nombre protegido "Segundo" sigue
# siendo la unidad de las fechas. Lo que el paquete escribe -una palabra en
# minusculas con guiones bajos- no se revisa: no puede llevar el dato de nadie
# entre limites, y revisarlo costaria un barrido mas por salida.
.vocabulario_sin_valores <- function(vocabulario, valores, nombres = character()) {
  if (!length(vocabulario) || !length(valores)) return(vocabulario)
  revisar <- !grepl("^[a-z0-9_]*$", vocabulario, useBytes = TRUE)
  if (!any(revisar)) return(vocabulario)
  # Declarados antes de la primera primitiva, y solo lo que se puede leer por
  # caracteres (ronda 30-L). Bajo `C` un nombre UTF-8 sin marca no se traducia
  # al armar el patron -avisaba "unable to translate"-, y bajo un locale UTF-8
  # un nombre con un byte latin1 suelto ABORTABA `analizar()` en el
  # `gsub(fixed = TRUE)` que escapa el literal, que estaba fuera del
  # `tryCatch`. Lo que no es UTF-8 valido no se puede leer por caracteres en
  # ninguna sesion: ese nombre no se quita y ese texto no se barre por nombres
  # -queda entero para la regla de valores, que es el lado cerrado-.
  legible <- function(x) Encoding(x) == "latin1" | validUTF8(x)
  textos <- .declarar_utf8_valido(vocabulario[revisar])
  barribles <- legible(textos)
  nombres <- .declarar_utf8_valido(
    unique(as.character(nombres[!is.na(nombres) & nzchar(nombres)]))
  )
  nombres <- nombres[legible(nombres)]
  # Entero y entre limites: `id` adentro de "Apellidoso" no es la columna, y
  # sacarlo partia el valor protegido que la regla tenia que encontrar.
  for (nombre in nombres[.orden_por_bytes(-nchar(nombres, type = "bytes"))]) {
    if (!any(barribles)) break
    textos[barribles] <- tryCatch({
      literal <- gsub("\\E", "\\E\\\\E\\Q", nombre, fixed = TRUE)
      patron <- paste0("(?<![\\p{L}\\p{N}_])\\Q", literal, "\\E(?![\\p{L}\\p{N}_])")
      gsub(patron, " ", textos[barribles], perl = TRUE)
    }, error = function(e) textos[barribles])
  }
  tapados <- tryCatch(
    .reemplazar_valores_protegidos(textos, valores, exigir_limites = TRUE),
    error = function(e) rep(NA_character_, length(textos))
  )
  con_valor <- is.na(tapados) | tapados != textos
  vocabulario[!vocabulario %in% vocabulario[revisar][con_valor]]
}

# Los campos que REFIEREN columnas: ahi un nombre de columna es estructura aunque
# sea igual a un valor protegido -una planilla pivoteada por persona tiene una
# columna `Florencia Rodriguez` y una docente protegida con ese nombre-. El plan
# lo necesita para ejecutar, y la columna se publica igual en `columnas$columna`.
# Fuera de estos campos (y de los de vocabulario), un campo hecho SOLO de nombres
# que son valores protegidos es un campo de VALORES: los ejemplos de `aprobo`,
# sus niveles observados, la configuracion propuesta. La lista falla para el
# lado cerrado: un campo de nombres que falte aca se tapa en ese caso -raro-,
# no se publica.
#
# Ronda 29: la lista y el patron se revisaron RECORRIENDO las salidas -perfil,
# plan, `analizar()`, `perfilar_por()`, `perfilar_dbi()`, la coleccion, las
# dependencias, los duplicados, la distribucion, las asociaciones,
# `guiar_limpieza()` y `comparar_perfiles()`- y anotando todo campo cuyas hojas
# son nombres de columna. Faltaban `columna_1` y `columna_2` -las columnas
# duplicadas, en el perfil y en los parametros de sus dos acciones-, `eliminar`
# y `datos_personales_sin_proteger`: con la columna que se llama como una
# persona protegida, las dos acciones de la columna duplicada salian
# `bloqueada`, sin parametros, cuando sin la proteccion la recomendada estaba
# `lista`. La prueba de la ronda 29 repite el recorrido.
.CAMPOS_DE_NOMBRES <- c(
  "determinante", "dependiente", "variable", "objeto", "objeto_medible",
  "atributo", "atributos", "atributos_ligados", "grupo", "por", "clave",
  "claves", "identificador", "origen", "eliminar",
  "datos_personales_sin_proteger"
)

# Lo que un campo de nombres NO es adentro de un contenedor de VALORES: el
# `mapa` de una imputacion por dependencia tiene columnas `determinante` y
# `dependiente`, y lleva los valores de las dos columnas, no sus nombres. Con
# la docente protegida y una columna de notas por docente, el mapa se publicaba
# en claro -los cuatro nombres- porque sus valores eran todos nombres de
# columna. Medido en la ronda 29. Adentro de un contenedor de estos, cada hoja
# hereda el nombre del contenedor, que no es de nombres: se barre como valores.
# Lo que se EJECUTA no cambia: un mapa tapado queda `mapa_enmascarado` y
# `aplicar()` lo resuelve sobre los datos que recibe.
.CONTENEDORES_DE_VALORES <- c("mapa")

# Un VALOR de un campo que refiere columnas, con un prefijo de vocabulario y
# columnas detras -`dependencia_funcional:curso->turno`, el `origen` de la
# propuesta de `analizar()`-, es estructura aunque la hoja mezcle otras cosas:
# esa forma la escribe el paquete, no sale de una celda. La hoja `origen` mezcla
# `perfil:n_faltantes_totales` con las dependencias, no era "toda nombres" y se
# barria: con la docente protegida y una columna por docente, decia
# `dependencia_funcional:[valor protegido]->[valor protegido]` donde nombraba
# dos COLUMNAS, que salen igual en `atributos`. Medido en la ronda 28. Sin el
# prefijo no: un nombre suelto se decide por la hoja entera
# (`.es_columna_de_nombres()`), porque `mapa$dependiente` tambien se llama como
# un campo de nombres y lleva VALORES -"Mercedes", la persona, igual a la columna
# `Mercedes`-.
.es_referencia_a_columnas <- function(x, nombres) {
  valores <- as.character(x)
  salida <- rep(FALSE, length(valores))
  if (!length(nombres) || !length(valores)) return(salida)
  prefijo <- "^(?:[a-z][a-z0-9_]*:)+"
  con_prefijo <- which(!is.na(valores) &
                         grepl(prefijo, valores, perl = TRUE, useBytes = TRUE))
  for (i in con_prefijo) {
    v <- valores[[i]]
    resto <- sub(prefijo, "", v, perl = TRUE, useBytes = TRUE)
    marca <- Encoding(v)
    if (marca %in% c("latin1", "UTF-8")) Encoding(resto) <- marca
    salida[[i]] <- nzchar(resto) && .es_columna_de_nombres(resto, nombres)
  }
  salida
}

# Con cifras en el sufijo: `columna_1`, `columna_2`.
.PATRON_CAMPOS_DE_NOMBRES <- "^(columna|columnas|nombres)(_[a-z0-9_]+)?$"

.es_campo_de_nombres <- function(campo) {
  .refiere_columnas(campo) ||
    (!is.null(campo) && length(campo) == 1L && !is.na(campo) &&
       campo %in% .CAMPOS_DE_VOCABULARIO)
}

# Un campo que REFIERE columnas -ver `.CAMPOS_DE_NOMBRES`-, sin los de
# vocabulario: es lo que la prueba de la ronda 29 exige de todo campo cuyas
# hojas son nombres de columna. El vocabulario se saltea por otra razon -una
# palabra del paquete, `faltantes`, igual a un nombre protegido, sigue siendo el
# tipo del hallazgo-, y el texto del usuario que llega a un campo de vocabulario
# lo saca `.vocabulario_sin_valores()`.
.refiere_columnas <- function(campo) {
  !is.null(campo) && length(campo) == 1L && !is.na(campo) &&
    (campo %in% .CAMPOS_DE_NOMBRES || grepl(.PATRON_CAMPOS_DE_NOMBRES, campo))
}

.es_contenedor_de_valores <- function(campo) {
  !is.null(campo) && length(campo) == 1L && !is.na(campo) &&
    campo %in% .CONTENEDORES_DE_VALORES
}

# Los nombres apartados que, plegados, SON un valor protegido -con la clave de
# `.reemplazar_valores_protegidos()`: pliegue de tildes, caja y escritura, sin
# separadores-. Se calcula una vez por barrido y se pasa a las dos puertas que
# la necesitan, el recorrido y el reemplazo: plegar los valores es lo caro, y el
# reemplazo lo hacia por su cuenta.
.nombres_iguales_a_valores <- function(nombres, valores) {
  nombres <- setdiff(unique(as.character(nombres)), "")
  nombres <- nombres[!is.na(nombres)]
  if (!length(nombres) || !length(valores)) return(character())
  clave <- function(v) {
    plegado <- tryCatch(.plegar_para_comparar(v), error = function(e) as.character(v))
    tryCatch(
      gsub("[^\\p{L}\\p{N}]", "", plegado, perl = TRUE),
      error = function(e) gsub("[^[:alnum:]]", "", plegado, useBytes = TRUE)
    )
  }
  nombres[nombres %in% valores | clave(nombres) %in% clave(valores)]
}

# Se saltea esta hoja por ser estructura? Si todos sus valores son nombres o
# vocabulario -como antes-, salvo que alguno sea un valor protegido y el campo no
# refiera columnas: ahi es un campo de valores y se barre, y el reemplazo no
# aparta esos nombres. Medido en la ronda 27: `patrones$aprobo$ejemplos` decia
# `Florencia Rodriguez | Martin Gomez` -dos docentes protegidas- porque la tabla
# tenia columnas con esos nombres, y lo mismo `niveles_observados`, la
# configuracion propuesta y el informe HTML.
# `sin_iguales`: los intocables sin esos nombres, calculados una vez por barrido;
# NULL si no hay ninguno, que es lo corriente.
.saltear_hoja_de_nombres <- function(x, intocables, sin_iguales, campo) {
  if (!.es_columna_de_nombres(x, intocables)) return(FALSE)
  if (is.null(sin_iguales) || .es_campo_de_nombres(campo)) return(TRUE)
  .es_columna_de_nombres(x, sin_iguales)
}

# `aplicar` recibe la hoja y el nombre del campo de donde viene -la columna, el
# elemento de lista o el atributo, heredado hacia adentro-, para que el barrido
# pueda tratar distinto la prosa del paquete y los valores.
.recorrer_textos_salida <- function(x, aplicar, intocables = character(),
                                    campo = NULL, sin_iguales = NULL) {
  atributos <- attributes(x)
  estructurales <- c("names", "class", "row.names", "dim", "dimnames")
  adicionales <- setdiff(names(atributos), estructurales)
  for (atributo in adicionales) {
    attr(x, atributo) <- .recorrer_textos_salida(
      attr(x, atributo, exact = TRUE), aplicar, intocables, campo = atributo,
      sin_iguales = sin_iguales
    )
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      columna <- x[[j]]
      # Los campos que guardan nombres de columna son ESTRUCTURA: el paquete los
      # necesita para cruzar el perfil con los datos, y un nombre no filtra el
      # valor que casualmente contiene -existia antes y aparte del dato-.
      # Enmascararlos rompia las dos guardas que comparan nombres: medido en una
      # base real de millones de filas, `fecha_nacimiento` salia publicada como
      # `fecha_[valor protegido]` y `planificar_limpieza()` y `analizar()`
      # abortaban. Se saltean en las DOS pasadas, asi que las hojas siguen
      # alineadas: la decision depende solo del contenido original.
      nombre_columna <- if (.es_contenedor_de_valores(campo)) {
        campo
      } else names(x)[[j]]
      if ((is.character(columna) || is.factor(columna)) &&
          .saltear_hoja_de_nombres(columna, intocables, sin_iguales,
                                   nombre_columna)) {
        next
      }
      if (is.character(columna)) {
        x[[j]] <- aplicar(columna, nombre_columna)
      } else if (is.factor(columna)) {
        levels(columna) <- aplicar(levels(columna), nombre_columna)
        x[[j]] <- columna
      } else if (is.list(columna)) {
        x[[j]] <- lapply(
          columna, .recorrer_textos_salida,
          aplicar = aplicar, intocables = intocables, campo = nombre_columna,
          sin_iguales = sin_iguales
        )
      }
    }
    return(x)
  }
  if (is.list(x)) {
    nombres <- names(x)
    for (i in seq_along(x)) {
      propio <- if (!.es_contenedor_de_valores(campo) && !is.null(nombres) &&
                    !is.na(nombres[[i]]) && nzchar(nombres[[i]])) {
        nombres[[i]]
      } else campo
      elemento <- .recorrer_textos_salida(
        x[[i]], aplicar = aplicar, intocables = intocables, campo = propio,
        sin_iguales = sin_iguales
      )
      if (is.null(elemento)) {
        x[i] <- list(NULL)
      } else {
        x[[i]] <- elemento
      }
    }
    return(x)
  }
  # Y un vector suelto hecho entero de nombres de columna -`columnas_analizadas`,
  # `columnas_datos_personales_protegidas`- tambien es estructura: con "Segundo"
  # protegido, `segundo_nombre` salia `[valor protegido]_nombre` en esos
  # atributos. Medido en la cuarta evaluacion real.
  if (is.character(x)) {
    if (.saltear_hoja_de_nombres(x, intocables, sin_iguales, campo)) return(x)
    return(aplicar(x, campo))
  }
  x
}

# DOS PASADAS: cosechar las hojas de texto, reemplazar UNA vez sobre el vector
# entero, repartir.
#
# Por que. El reemplazo recorre cada valor protegido con un `gsub`, asi que cuesta
# lo mismo sobre una cadena que sobre diez mil; pagarlo por hoja es lo que hacia
# que publicar 4.754 hallazgos costara 42.794 llamadas -el 99% de las del objeto-
# con una mediana de UN texto por llamada. La culpable medida es
# `hallazgos$trazabilidad`, una columna-lista con una entrada por hallazgo y nueve
# hojas en cada entrada.
#
# Localizarlo costo tres intentos, y los tres errores fueron del instrumento: las
# tres primeras pilas apuntaban al `perfilar()` interno -que cuesta 1 s de 176-,
# contar en un entorno con asignacion compleja dio cero, y recien una muestra
# uniforme al 2 % dijo que el 99 % venia de aca.
#
# Lo que NO se hace: tocar el motor de coincidencia. Cambiar el bucle por una
# alternancia de expresion regular es mas rapido y, medido, DEJA DE ENMASCARAR el
# texto marcado `latin1` -armar el patron traduce a UTF-8-. Aca no se cambia ni
# `fixed = TRUE` ni la codificacion: solo se agrupa.
.proteger_textos_salida <- function(x, valores, intocables = character()) {
  if (!length(valores)) return(x)
  nombres <- c(intocables, .nombres_derivados_de_columnas(intocables))
  intocables <- c(
    nombres, .vocabulario_sin_valores(.vocabulario_de_objeto(x), valores, nombres)
  )
  iguales <- .nombres_iguales_a_valores(intocables, valores)
  sin_iguales <- if (length(iguales)) setdiff(intocables, iguales)
  cofre <- new.env(parent = emptyenv())
  cofre$hojas <- list()
  cofre$prosa <- list()
  cofre$sql <- list()
  cofre$referencias <- list()
  invisible(.recorrer_textos_salida(x, function(hoja, campo) {
    cofre$hojas[[length(cofre$hojas) + 1L]] <- hoja
    cofre$referencias[[length(cofre$referencias) + 1L]] <- if (
      .es_campo_de_nombres(campo)
    ) {
      .es_referencia_a_columnas(hoja, intocables)
    } else rep(FALSE, length(hoja))
    cofre$prosa[[length(cofre$prosa) + 1L]] <- rep(
      !is.null(campo) && campo %in% .CAMPOS_DE_PROSA, length(hoja)
    )
    cofre$sql[[length(cofre$sql) + 1L]] <- rep(
      !is.null(campo) && length(campo) == 1L && !is.na(campo) &&
        grepl(.PATRON_CAMPOS_DE_SQL, campo), length(hoja)
    )
    hoja
  }, intocables, sin_iguales = sin_iguales))
  if (!length(cofre$hojas)) return(x)
  textos <- unlist(cofre$hojas, use.names = FALSE)
  prosa <- unlist(cofre$prosa, use.names = FALSE)
  sql <- unlist(cofre$sql, use.names = FALSE)
  protegidas <- textos
  if (any(!sql)) {
    protegidas[!sql] <- .reemplazar_valores_protegidos(
      textos[!sql], valores, exigir_limites = prosa[!sql],
      intocables = intocables, nombres_iguales = iguales
    )
  }
  if (any(sql)) {
    protegidas[sql] <- .reemplazar_valores_protegidos(
      textos[sql], valores, exigir_limites = prosa[sql],
      intocables = intocables, apartar_iguales = TRUE
    )
  }
  # Una referencia a columnas en un campo de nombres queda: ver
  # `.es_referencia_a_columnas()`.
  referencias <- unlist(cofre$referencias, use.names = FALSE)
  protegidas[referencias] <- textos[referencias]
  cofre$desde <- 0L
  .recorrer_textos_salida(x, function(hoja, campo) {
    tramo <- protegidas[cofre$desde + seq_along(hoja)]
    cofre$desde <- cofre$desde + length(hoja)
    # Los atributos de la hoja -nombres, sobre todo- los pierde el corte del
    # vector y hay que devolverlos.
    attributes(tramo) <- attributes(hoja)
    tramo
  }, intocables, sin_iguales = sin_iguales)
}

# Los campos numericos que son ESTRUCTURA -cuentan, ubican o configuran- y no un
# dato: un conteo, un indice de fila, un tamano, un tiempo, una proporcion, un
# umbral. El piso numerico tapaba cualquier numero cuya representacion fuera un
# valor protegido, y con eso rompia la estructura por coincidencia. Medido: los
# `indices_fila` de una traza que coincidian con un documento protegido quedaban
# en NA y lupa se acusaba a si misma -"total de traza no coincide con sus
# indices"-, en una base real de millones de filas; y en `perfilar_dbi()` una
# columna `id = 1..n` hacia que el conteo `n` saliera NA en todas las columnas.
# Es la misma propiedad que ya rompio los nombres de columna: la proteccion
# reemplaza por coincidencia de valor cosas que no son celdas.
#
# La lista es de ESTRUCTURA, no de valores, y eso decide para que lado falla: un
# campo nuevo que no este aca se sigue tapando. Salio de recorrer los 271 campos
# numericos que publican `perfilar()`, `perfilar_dbi()` y `analizar()`; lo que
# puede llevar un valor de la tabla -`minimo`, `maximo`, `media`, `mediana`,
# `valor`, `resultado`, `rango`, `desvio`, `centinela_valor`, las coordenadas- no
# esta, y tampoco lo que se deriva de valores, como `ordenes_magnitud`.
#
# `soporte_minimo` y `cumplimiento` -de la imputacion por dependencia- son la
# configuracion de la accion: el soporte lo declara el usuario y el cumplimiento
# es una proporcion. Con una columna `id_persona = 1..40` protegida, el 2 del
# soporte coincidia con un valor y salia NA, y `aplicar()` dejaba la imputacion
# `fallida` ("no conserva un soporte de dependencia valido"). Medido en la ronda
# 27 por la guarda que recorre todos los parametros del plan.
.PATRON_NUMERICO_DE_ESTRUCTURA <- paste0(
  "^(n|n_.+|indices_.+|fila|filas|filas_.+|total|.+_totales|valores_evaluados|",
  "pares_.+|celdas|celdas_.+|columnas|columnas_.+|lote|lotes_.+|tamano_.+|",
  ".+_bytes|bytes_.+|memoria_.+|.+_ms|duracion_estimada_.+|umbral|umbral_.+|",
  "max_.+|min_.+|minimo_filas|minimo_observaciones_utilizables|",
  "minima_proporcion_positivos|prop_.+|proporcion|proporcion_.+|fraccion|",
  "tasa_.+|version_.+|consulta_id|id_consulta|consultas_.+|emitidas|",
  "llamadas_.+|bloques_.+|muestra|muestra_.+|presupuesto|mostrados|frecuencia|",
  "frecuencia_.+|absoluta|relativa|longitud_.+|centinela_repeticiones|",
  "soporte_minimo|cumplimiento)$"
)

# Solo una HOJA numerica se saltea, nunca un contenedor: el perfil tiene un
# componente que se llama `columnas`, y saltearlo por el nombre dejaba sin tapar
# la tabla entera -el `minimo` de una columna copia volvia a publicar documentos-.
# Lo atrapo la prueba que ya cuidaba esa filtracion.
.es_numero_de_estructura <- function(nombre, valor) {
  !is.null(nombre) && length(nombre) == 1L && !is.na(nombre) &&
    is.numeric(valor) && !is.list(valor) &&
    grepl(.PATRON_NUMERICO_DE_ESTRUCTURA, nombre)
}

.proteger_numeros_parametros <- function(x, valores, ampliados = FALSE) {
  if (!length(valores)) return(x)
  # Un numero es el documento aunque el documento este ESCRITO: "5.432.198-6" en
  # la columna protegida y 54321986 como `maximo` de una copia numerica. Se
  # comparaba solo la escritura exacta, y la misma cifra como texto se tapaba en
  # otra hoja del mismo perfil: dos reglas para el mismo dato. Medido en una
  # refutacion. Se agrega la forma en digitos, sin ceros iniciales, de cada valor
  # que solo tiene digitos y separadores; una vez, arriba del recorrido.
  if (!ampliados) {
    escritos <- valores[!is.na(valores) &
                          grepl("^[0-9][0-9 .,/'_-]*[0-9]$", valores, useBytes = TRUE)]
    digitos <- .sin_ceros_iniciales(gsub("[^0-9]", "", escritos, useBytes = TRUE))
    # La cifra escrita con un decimal de ceros o en notacion cientifica es la
    # misma: ver `.cifra_entera_escrita()`. Ronda 29.
    enteras <- .sin_ceros_iniciales(.cifra_entera_escrita(valores))
    digitos <- c(digitos, enteras[!is.na(enteras)])
    valores <- unique(c(valores, digitos[
      nchar(digitos, type = "bytes") >= .MIN_LARGO_VALOR_IDENTIFICANTE
    ]))
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      if (.es_numero_de_estructura(names(x)[[j]], x[[j]])) next
      if (is.list(x[[j]])) {
        x[[j]] <- lapply(x[[j]], .proteger_numeros_parametros,
                         valores = valores, ampliados = TRUE)
      } else {
        x[[j]] <- .proteger_numeros_parametros(x[[j]], valores, TRUE)
      }
    }
    return(x)
  }
  if (is.list(x)) {
    nombres <- names(x)
    for (i in seq_along(x)) {
      if (!is.null(nombres) && .es_numero_de_estructura(nombres[[i]], x[[i]])) {
        next
      }
      x[i] <- list(.proteger_numeros_parametros(x[[i]], valores, TRUE))
    }
    return(x)
  }
  if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) return(x)
  textos <- tryCatch(
    .caracter_por_omision(x),
    error = function(e) rep(NA_character_, length(x))
  )
  # Y la forma entera sin notacion cientifica: `as.character()` escribe
  # 1e+15 para un numero de dieciseis cifras.
  enteros <- tryCatch({
    numeros <- suppressWarnings(as.numeric(x))
    salida <- rep(NA_character_, length(x))
    enteras <- !is.na(numeros) & is.finite(numeros) & numeros == round(numeros) &
      abs(numeros) < 1e18
    salida[enteras] <- formatC(abs(numeros[enteras]), format = "f", digits = 0)
    salida
  }, error = function(e) rep(NA_character_, length(x)))
  protegidos <- (!is.na(textos) & textos %in% valores) |
    (!is.na(enteros) & enteros %in% valores)
  if (any(protegidos)) x[protegidos] <- NA
  x
}

# Una fecha de un parametro de una accion sobre una columna protegida sigue
# siendo un valor de la tabla aunque no coincida con el piso de identificantes.
# Se la tapa por clase, no por una lista de escrituras, y se conserva la clase
# para que una accion que la pueda resolver no cambie de contrato sin avisar.
.proteger_fechas_parametros <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    salida <- tryCatch({
      x[] <- NA
      x
    }, error = function(e) rep(NA, length(x)))
    return(salida)
  }
  if (inherits(x, "data.frame")) {
    for (j in seq_along(x)) {
      x[[j]] <- .proteger_fechas_parametros(x[[j]])
    }
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[[i]] <- .proteger_fechas_parametros(x[[i]])
    }
    return(x)
  }
  x
}

# El mapa tiene dos lados con contratos distintos: una columna protegida se
# tapa completo, y la otra conserva el barrido general. Esto evita que el
# enmascarado por construccion afloje el piso por contenido del lado publico.
# En texto se conserva la convencion del paquete; en las demas clases una `NA`
# no publica la escritura ni la clase del valor.
.enmascarar_lado_mapa <- function(x) {
  if (is.character(x) || is.factor(x)) {
    salida <- as.character(x)
    salida[!is.na(x)] <- "[valor protegido]"
    salida[is.na(x)] <- NA_character_
    return(salida)
  }
  tryCatch({
    x[] <- NA
    x
  }, error = function(e) rep(NA, length(x)))
}

.enmascarar_mapa_lados_protegidos <- function(mapa, determinante = FALSE,
                                              dependiente = FALSE) {
  if (!inherits(mapa, "data.frame")) return(mapa)
  if (isTRUE(determinante) && "determinante" %in% names(mapa)) {
    mapa$determinante <- .enmascarar_lado_mapa(mapa$determinante)
  }
  if (isTRUE(dependiente) && "dependiente" %in% names(mapa)) {
    mapa$dependiente <- .enmascarar_lado_mapa(mapa$dependiente)
  }
  mapa
}

# Repara el mapa despues de todos los barridos genericos. Solo reemplaza el
# lado protegido: el otro ya trae el piso por contenido y debe conservarlo. El
# mapa se marca aunque una clase no necesite el piso: `aplicar()` debe
# reconstruirlo sobre los datos que recibe.
.proteger_lados_mapa_plan <- function(plan, sensibles) {
  if (!inherits(plan, "data.frame") || !is.list(plan$parametros) ||
      !length(plan$parametros)) {
    return(plan)
  }
  sensibles <- .nombres_para_operar(sensibles)
  estrategias <- as.character(plan$estrategia)
  for (i in which(startsWith(estrategias, "imputar_dependencia_funcional__"))) {
    parametros <- plan$parametros[[i]]
    if (!is.list(parametros) || !inherits(parametros$mapa, "data.frame")) next
    determinante <- .nombres_para_operar(parametros$determinante)
    dependiente <- .nombres_para_operar(parametros$dependiente)
    det_protegida <- length(determinante) == 1L && determinante %in% sensibles
    dep_protegida <- length(dependiente) == 1L && dependiente %in% sensibles
    if (!det_protegida && !dep_protegida) next
    mapa <- parametros$mapa
    if (!all(c("determinante", "dependiente") %in% names(mapa))) next
    if (det_protegida) {
      mapa$determinante <- .enmascarar_lado_mapa(mapa$determinante)
    }
    if (dep_protegida) {
      mapa$dependiente <- .enmascarar_lado_mapa(mapa$dependiente)
    }
    parametros$mapa <- mapa
    parametros$mapa_enmascarado <- TRUE
    plan$parametros[[i]] <- parametros
  }
  plan
}

# La ayuda y `aplicar()` sólo necesitan que se tapen las fechas de acciones
# cuyo objeto directo es protegido. El mapa se trata aparte para no tapar por
# accidente su lado no protegido.
.proteger_fechas_acciones_plan <- function(plan, sensibles) {
  if (!inherits(plan, "data.frame") || !is.list(plan$parametros)) return(plan)
  sensibles <- .nombres_para_operar(sensibles)
  propias <- !is.na(plan$columna) &
    .nombres_para_operar(as.character(plan$columna)) %in% sensibles
  plan$parametros <- I(lapply(seq_along(plan$parametros), function(i) {
    parametros <- plan$parametros[[i]]
    if (isTRUE(propias[[i]])) {
      if (!is.list(parametros)) return(parametros)
      mapa <- parametros$mapa
      parametros <- .proteger_fechas_parametros(parametros)
      # El mapa se enmascara por lado en la segunda pasada; aca no se lo toca
      # para conservar el lado no protegido despues del barrido.
      if (!is.null(mapa)) parametros$mapa <- mapa
      parametros
    } else {
      parametros
    }
  }))
  plan
}

# El piso tapa un extremo de una columna NO protegida porque es un documento, y
# los campos de la secuencia entera lo devolvian: `minimo` 8 y
# `n_posiciones_secuencia_entera` 54321979 dan el maximo, 54321986. Medido en una
# refutacion. Es lo que ya esta escrito para las columnas protegidas -proteger el
# minimo y el maximo y dejar publicado su rango no protege nada-, aplicado a la
# fila donde el piso tapo algo. Se compara la tabla antes y despues del piso.
.tapar_derivados_del_piso <- function(antes, despues) {
  if (!inherits(antes, "data.frame") || !inherits(despues, "data.frame") ||
      nrow(antes) != nrow(despues)) {
    return(despues)
  }
  extremos <- intersect(c("minimo", "maximo", "mediana", "media"), names(despues))
  if (!length(extremos)) return(despues)
  tocadas <- Reduce(`|`, lapply(extremos, function(campo) {
    is.numeric(antes[[campo]]) & !is.na(antes[[campo]]) & is.na(despues[[campo]])
  }))
  if (!any(tocadas)) return(despues)
  # `desvio` tambien: con el extremo dominando la columna, el desvio y `n` lo
  # reconstruyen -48.890.008 contra el documento 48.889.997-. Medido en la ronda
  # 22. Aca el extremo tapado ES un documento. (En las columnas protegidas el
  # desvio tambien se tapa desde la ronda 32: `.CAMPOS_MOMENTO_PROTEGIDOS`.)
  derivados <- intersect(c(
    "minimo", "maximo", "mediana", "media", "desvio", "n_posiciones_secuencia_entera",
    "n_huecos_secuencia_entera", "hueco_maximo_secuencia_entera",
    "densidad_secuencia_entera", "densidad_sin_centinela"
  ), names(despues))
  for (campo in derivados) {
    if (is.numeric(despues[[campo]])) despues[[campo]][tocadas] <- NA
  }
  despues
}

# La accion sigue siendo visible, pero sus argumentos no pueden llevarse un
# valor personal de vuelta a la tabla. Un plan con parametros enmascarados es
# deliberadamente informativo: quien necesite ejecutarlo debe confirmar esos
# valores en la fuente protegida. La imputacion es una excepcion operativa: el
# mapa privado no viaja, pero sus columnas y su soporte si; `aplicar()` vuelve a
# resolver la relacion sobre los datos que recibe.
.proteger_plan_limpieza <- function(plan, perfil, datos = NULL) {
  if (!inherits(plan, "data.frame")) return(plan)
  # Con la proteccion apagada en el perfil, el plan tampoco se protege: el
  # usuario declaro que quiere los valores, y el plan tapaba igual los
  # parametros de sus acciones. Medido en una refutacion.
  if (is.list(perfil$meta) && isFALSE(perfil$meta$proteger_datos_personales)) {
    attr(plan, "columnas_datos_personales_protegidas") <- character()
    # Queda escrito que lleva datos personales en claro, para que `reportar()`
    # -que protege por omision- no lo publique sin decirlo.
    sin_proteger <- .columnas_personales_protegidas(perfil)
    if (length(sin_proteger)) {
      attr(plan, "datos_personales_sin_proteger") <- sin_proteger
    }
    return(plan)
  }
  sensibles <- .columnas_personales_protegidas(perfil)
  parametros_originales <- if (is.list(plan$parametros)) plan$parametros
  indices_dependencias <- which(startsWith(
    as.character(plan$estrategia), "imputar_dependencia_funcional__"
  ))
  if (length(indices_dependencias) && is.list(plan$parametros)) {
    for (indice in indices_dependencias) {
      parametros <- plan$parametros[[indice]]
      if (!is.list(parametros)) next
      involucra_protegida <- any(.nombres_para_operar(c(
        parametros$determinante, parametros$dependiente
      )) %in% .nombres_para_operar(sensibles))
      if (isTRUE(involucra_protegida)) {
        parametros$mapa_enmascarado <- TRUE
        plan$parametros[[indice]] <- parametros
      }
    }
  }
  valores <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta,
    datos = datos
  )
  valores <- unique(c(
    valores, .valores_publicables_protegidos(datos, sensibles)
  ))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  valores <- valores[.orden_por_bytes(nchar(valores, type = "bytes"), decreasing = TRUE,
                           method = "radix")]
  if (!length(valores)) {
    plan <- .proteger_fechas_acciones_plan(plan, sensibles)
    plan <- .proteger_lados_mapa_plan(plan, sensibles)
    attr(plan, "columnas_datos_personales_protegidas") <- sensibles
    return(plan)
  }
  nombres_entrada <- if (!is.null(perfil$columnas$columna)) {
    as.character(perfil$columnas$columna)
  } else character()
  # El barrido de todo el plan pasa por el PISO, como el del perfil: sin el, un
  # valor corto o un centinela del catalogo -`-999`- se tapaba en los parametros
  # de cualquier columna, y la accion que lo convierte quedaba sin efecto. Los
  # parametros de las acciones sobre una columna protegida se barren ademas con
  # todos sus valores: es la proteccion por columna.
  identificantes <- .valores_identificantes(
    valores, .fechas_que_identifican(datos, sensibles)
  )
  plan <- .proteger_textos_salida(
    plan, identificantes, intocables = nombres_entrada
  )
  if ("parametros" %in% names(plan) && is.list(plan$parametros)) {
    propias <- !is.na(plan$columna) &
      .nombres_para_operar(as.character(plan$columna)) %in%
        .nombres_para_operar(sensibles)
    # Por columna, con TODOS sus valores: tambien sus rellenos, que el piso deja
    # afuera. La accion que convierte el `99999999` de la cedula en `NA` no
    # publica el `99999999`.
    valores_columna <- unique(c(
      valores, .rellenos_de_columnas(datos, sensibles)
    ))
    antes <- plan$parametros
    # Con los nombres de la entrada INTOCABLES, como el barrido de todo el plan
    # de arriba. Sin ellos, con "Segundo" -un nombre de pila- protegido en
    # `Segundo Nombre`, la marca `.ausente_Segundo.Nombre` de su accion salia
    # `[valor protegido]` y `aplicar()` creaba en los datos una columna con ese
    # nombre; y el `dependiente` de su imputacion salia tapado y la accion
    # quedaba `fallida`. Lo que el plan EJECUTA -los nombres de columna y los
    # que deriva de ellos- guarda su valor real. Medido en la ronda 27, en la
    # llamada por omision: era la puerta de al lado de la ronda 26.
    plan$parametros <- I(lapply(seq_along(plan$parametros), function(i) {
      parametros <- plan$parametros[[i]]
      if (isTRUE(propias[[i]])) {
        parametros <- .proteger_textos_salida(
          parametros, valores_columna, intocables = nombres_entrada
        )
        .proteger_numeros_parametros(parametros, valores_columna)
      } else {
        .proteger_numeros_parametros(parametros, identificantes)
      }
    }))
    plan <- .ejecutables_tras_barrido(
      plan, parametros_originales, antes, datos, perfil$meta$cadenas_ausencia
    )
    plan <- .proteger_fechas_acciones_plan(plan, sensibles)
    plan <- .proteger_lados_mapa_plan(plan, sensibles)
  }
  cobertura <- attr(plan, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(cobertura, "data.frame")) {
    attr(plan, "cobertura_diagnosticos") <- .proteger_textos_salida(
      cobertura, identificantes, intocables = nombres_entrada
    )
  }
  # Este atributo copia la sugerencia del hallazgo, que es texto del paquete
  # pero puede nombrar un valor de la columna. Enmascararlo aqui es lo que
  # impide que un atributo nuevo publique lo que las columnas del plan ya no
  # publican.
  #
  # Con los nombres de columna INTOCABLES, como en el plan. Sin eso, "Segundo"
  # -un nombre de pila corriente- protegido tapaba la columna `segundo_nombre`
  # del atributo, que salia `[valor protegido]_nombre`: el hallazgo declarado ya
  # no correspondia a su columna. Medido en la cuarta evaluacion real, sobre
  # tres columnas de nombres, como "hallazgo sin accion ni declaracion".
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  if (inherits(sin_accion, "data.frame")) {
    attr(plan, "hallazgos_sin_accion") <- .proteger_textos_salida(
      sin_accion, identificantes, intocables = nombres_entrada
    )
  }
  # `guiar_limpieza()` vuelve a consultar los datos de origen para construir
  # ejemplos. Llevar sólo los nombres de columnas permite enmascararlos en esa
  # salida sin serializar ni publicar los valores protegidos.
  attr(plan, "columnas_datos_personales_protegidas") <- sensibles
  plan
}

# El MAPA de una imputacion por dependencia que la proteccion toco -una clave o un
# valor que contiene un valor protegido- ya no es la relacion: se marca
# `mapa_enmascarado` y `aplicar()` la reconstruye sobre los datos que recibe, con
# el soporte declarado. Se marcaba solo cuando la determinante o la dependiente
# ERA una columna protegida, y el piso tapa despues por CONTENIDO cualquier celda
# del mapa: con "Rodriguez Perez" protegido en `titular`, el mapa de `cod_suc ->
# sucursal` decia `S01 -> Farmacia [valor protegido]`, la accion `lista` quedaba
# `fallida` ("los datos actuales contradicen la dependencia") y, al reves
# (`sucursal -> cod_suc`), `ejecutada` con 12 de 24 celdas y sin avisar. Medido
# en la ronda 27. Es el mismo mecanismo que el centinela tapado de una columna
# protegida (`valores_enmascarados`): lo que se ejecuta se resuelve sobre los
# datos y el plan no publica el valor.
#
# Se decide comparando el mapa antes y despues de TODOS los barridos, asi que no
# depende de que regla lo tapo: el piso de texto, el numerico o el por columna.
.marcar_mapas_tapados <- function(plan, originales) {
  if (!inherits(plan, "data.frame") || !is.list(plan$parametros) ||
      !is.list(originales) || length(originales) != length(plan$parametros)) {
    return(plan)
  }
  estrategias <- as.character(plan$estrategia)
  for (i in which(startsWith(estrategias, "imputar_dependencia_funcional__"))) {
    parametros <- plan$parametros[[i]]
    original <- originales[[i]]
    if (!is.list(parametros) || !is.list(original) ||
        isTRUE(parametros$mapa_enmascarado)) {
      next
    }
    if (!identical(parametros$mapa, original$mapa)) {
      parametros$mapa_enmascarado <- TRUE
      plan$parametros[[i]] <- parametros
    }
  }
  plan
}

# Lo que el plan EJECUTA, despues de un barrido de sus parametros: los
# marcadores de ausencia guardan su valor, un centinela tapado se declara y se
# resuelve sobre los datos, y un mapa tocado se marca para reconstruirse. Lo
# llaman `.proteger_plan_limpieza()` y el barrido de `analizar()` sobre su plan:
# alli solo se volvia a marcar el mapa, y el barrido de `analizar()` volvia a
# tapar los marcadores que el plan ya habia devuelto. Medido en la ronda 28 por
# la guarda de los parametros ejecutables. `originales`: los parametros antes
# del barrido; `antes`: antes del barrido por columna, si lo hubo.
.ejecutables_tras_barrido <- function(plan, originales, antes = originales,
                                      datos = NULL, declarados = NULL) {
  plan <- .marcadores_ausencia_ejecutables(plan, originales, declarados)
  plan <- .centinelas_tapados_en_plan(plan, antes, datos)
  .marcar_mapas_tapados(plan, originales)
}

# Lo que `convertir_ausencias_textuales` EJECUTA guarda su valor real: la lista
# `valores` y la declaracion `cadenas_ausencia`. Un marcador del catalogo del
# paquete -`sin dato`, `N/A`- o uno que el usuario declaro -`SIN REGISTRO`- no
# es el dato de nadie: es vocabulario, del paquete o del usuario, y la
# declaracion ya se publica en `meta$cadenas_ausencia`. Los barridos lo tapaban
# igual -el por columna con todos los valores de la protegida; el piso, en
# cualquier columna, si el marcador declarado aparecia tambien en la protegida-
# y la accion quedaba `lista`, activa, con `valores = "[valor protegido]"`, y
# `aplicar()` la registraba `fallida`. Medido en la ronda 28, en la llamada por
# omision. Es la ronda 26 y la 27 otra vez: lo que se ejecuta no pasa por el
# barrido; se tapa lo que solo se muestra. Lo que no es marcador -un texto que
# el detector leyo de una evidencia ya tapada- sigue como lo dejo el barrido.
.marcadores_ausencia_ejecutables <- function(plan, originales,
                                             declarados = NULL) {
  if (!inherits(plan, "data.frame") || !is.list(plan$parametros) ||
      !is.list(originales) || length(originales) != length(plan$parametros)) {
    return(plan)
  }
  # El pliegue de los detectores de ausencia (`.minusculas_para_comparar()`):
  # no aborta sobre bytes que no son UTF-8 valido y no depende del locale.
  normalizar <- function(x) {
    .minusculas_para_comparar(x)
  }
  vocabulario <- unique(c(
    normalizar(c(.cadenas_na_naniar_1_1_0, .cadenas_na_locales)),
    normalizar(declarados)
  ))
  indices <- which(
    as.character(plan$estrategia) == "convertir_ausencias_textuales"
  )
  for (i in indices) {
    parametros <- plan$parametros[[i]]
    original <- originales[[i]]
    if (!is.list(parametros) || !is.list(original)) next
    propios <- unique(c(vocabulario, normalizar(original$cadenas_ausencia)))
    valores <- parametros$valores
    if (is.character(valores) && is.character(original$valores) &&
        length(valores) == length(original$valores)) {
      marcadores <- !is.na(original$valores) &
        normalizar(original$valores) %in% propios
      valores[marcadores] <- original$valores[marcadores]
      parametros$valores <- valores
    }
    if (length(original$cadenas_ausencia)) {
      parametros$cadenas_ausencia <- original$cadenas_ausencia
    }
    plan$parametros[[i]] <- parametros
  }
  plan
}

# La accion de centinelas sobre una columna protegida tiene que poder ejecutar lo
# que el plan dice sin publicar el valor protegido. El barrido por columna tapaba
# en `valores` todo numero de la columna, y con eso los mismos numeros que la
# accion necesita: sobre una cedula con un -999 la lista salia
# `c(-9999, NA, -99, -9, 999)`, la accion `lista` con 1 afectada, y `aplicar()`
# la registraba `fallida` con el -999 en la tabla. Con `sentinelas_numericos =
# 99999999` -que el perfil protegido ya no conserva- `valores` era `NA` y las seis
# celdas seguian. Y el hueco no ocultaba nada: estaba en la posicion de -999 del
# catalogo fijo. Medido en la refutacion 26-R.
#
# Dos reglas, cada una con su precedente:
#
#   * el catalogo DEL PAQUETE -`.numeros_na_locales`- es vocabulario y no se
#     tapa, como ya no se tapa en `meta` (ver `.valores_perfil_protegidos()`): la
#     lista publicada es la misma tenga o no la columna esos valores;
#   * lo demas sigue tapado, y como el mapa de la imputacion
#     (`mapa_enmascarado`), `aplicar()` lo resuelve sobre los datos que recibe:
#     lo tapado de una columna protegida que es centinela es su RELLENO -el digito
#     repetido de `.rellenos_de_columna()`-. Solo se ofrece asi si sobre los datos
#     del plan esa resolucion convierte exactamente las celdas que el plan
#     estima; si no, la accion queda `bloqueada` y la justificacion dice como
#     ejecutarla, en vez de ofrecer como `lista` una accion imposible.
#
# En TODA columna, no solo en las protegidas: la lista de una columna que no es
# personal sale de `meta$sentinelas_numericos`, y ahi el piso tapa el centinela
# declarado que es el valor de una columna protegida -el 99999999 que se repite
# en una tabla de movimientos, donde puede ser una cedula-. La accion de `monto`
# salia `lista` con `valores = -999, NA`, sin marca, y `aplicar()` la registraba
# `fallida`. Ahora lo declara igual: lo resuelve con los rellenos de `monto`, si
# eso reproduce lo que el plan estima, o queda `bloqueada`. Medido en la ronda
# 28.
.centinelas_tapados_en_plan <- function(plan, antes, datos) {
  indices <- which(
    as.character(plan$estrategia) == "convertir_sentinelas_numericos"
  )
  for (i in indices) {
    # Una accion que un barrido anterior ya declaro se deja como esta: la
    # justificacion no se repite.
    if (isTRUE(antes[[i]]$valores_enmascarados)) next
    original <- antes[[i]]$valores
    parametros <- plan$parametros[[i]]
    valores <- parametros$valores
    if (!is.numeric(valores) || length(valores) != length(original)) next
    del_catalogo <- !is.na(original) &
      suppressWarnings(as.numeric(original)) %in% .numeros_na_locales
    valores[del_catalogo] <- original[del_catalogo]
    parametros$valores <- valores
    if (anyNA(valores)) {
      parametros$valores_enmascarados <- TRUE
      columna <- as.character(plan$columna[[i]])
      resoluble <- isTRUE(tryCatch(
        .centinelas_tapados_resolubles(
          datos, columna, parametros, plan$n_afectadas[[i]]
        ),
        error = function(e) FALSE
      ))
      if (!resoluble) {
        plan$estado[[i]] <- "bloqueada"
        plan$aplicar[[i]] <- FALSE
        plan$recomendada[[i]] <- FALSE
        plan$justificacion[[i]] <- paste0(
          plan$justificacion[[i]], " El centinela de esta columna es un valor ",
          "protegido: el plan no lo publica y no puede reconstruirlo sobre los ",
          "datos, as\u00ed que la acci\u00f3n no se puede ejecutar tal como est\u00e1. ",
          "Para convertirlo, escribir el valor en `valores` y pasar `estado` a ",
          "`lista`, o planificar con `proteger_datos_personales = FALSE`."
        )
      }
    }
    plan$parametros[[i]] <- parametros
  }
  plan
}

# Si la resolucion de `aplicar()` -los rellenos de la columna en lugar de lo
# tapado- convierte sobre `datos` exactamente las celdas que el plan estima,
# dentro de su universo aplicable. Es la misma funcion que usa el ejecutor.
.centinelas_tapados_resolubles <- function(datos, columna, parametros,
                                           n_afectadas) {
  if (!inherits(datos, "data.frame") || is.na(columna)) return(FALSE)
  indice <- .indice_nombre(columna, names(datos))
  if (length(indice) != 1L || is.na(indice)) return(FALSE)
  mascara <- .mascara_sentinelas_numericos(datos[[indice]], parametros)
  if (!is.null(parametros$aplicabilidad)) {
    mascara <- mascara &
      .mascara_aplicabilidad_accion(datos, columna, parametros)
  }
  length(n_afectadas) == 1L && isTRUE(sum(mascara) == n_afectadas)
}

.proteger_ausencia_estructural <- function(hallazgos, sensibles) {
  if (!nrow(hallazgos) || !length(sensibles)) return(hallazgos)
  indices <- which(hallazgos$tipo_hallazgo ==
                     "posible_ausencia_estructural")
  if (!length(indices)) return(hallazgos)
  # El determinante se reconoce comparando el comienzo de la evidencia con cada
  # nombre protegido, y no extrayendolo con una expresion: un nombre con un
  # acento grave dentro cortaba la extraccion y la sugerencia salia entera, con
  # los niveles o el corte de la columna protegida.
  textos_sensibles <- unique(c(
    .marcar_utf8_textos(as.character(sensibles)),
    as.character(sensibles)
  ))
  for (i in indices) {
    evidencia <- as.character(hallazgos$evidencia[[i]])
    if (is.na(evidencia)) next
    prefijos <- paste0("`", textos_sensibles, "` predice ")
    coinciden <- which(startsWith(evidencia, prefijos))
    if (!length(coinciden)) next
    candidatos <- textos_sensibles[coinciden]
    determinante <- candidatos[[which.max(nchar(candidatos, type = "bytes"))]]
    inicio <- sub("\\. La columna corresponde.*$", "", evidencia)
    if (identical(inicio, evidencia)) inicio <- NA_character_
    tipo_criterio <- if (grepl("por un umbral", evidencia, fixed = TRUE, useBytes = TRUE)) {
      "un umbral"
    } else {
      "niveles"
    }
    hallazgos$evidencia[[i]] <- if (is.na(inicio)) {
      "[evidencia protegida]"
    } else paste0(
      inicio,
      ". La columna corresponde segun ", tipo_criterio,
      " de `", determinante,
      "` que no se publica porque esa columna esta protegida."
    )
    columna <- as.character(hallazgos$columna[[i]])
    hallazgos$sugerencia[[i]] <- paste0(
      "Si es asi, declararlo y volver a perfilar con `aplicabilidad` usando en `",
      determinante,
      "` el criterio confirmado en la fuente. El criterio no se reproduce aqui",
      " porque esa columna esta protegida. Con la regla declarada, la ausencia",
      " fuera de ese universo deja de contarse como defecto y el alcance queda",
      " escrito en `cobertura_diagnosticos` para `", columna, "`."
    )
  }
  hallazgos
}

# Los MOMENTOS que se tapan en una columna protegida, en todas las vias -la de
# memoria, `perfilar_dbi()` con o sin bloques, `perfilar_por()`-: la media, en
# sus dos formas, y el desvio. Ronda 32: el desvio se publicaba por una decision
# declarada (F-6, "una dispersion no identifica a nadie") que se revirtio.
.CAMPOS_MOMENTO_PROTEGIDOS <- c("media", "media_fecha", "desvio")

# El detalle que declara lo que se tapo de una columna protegida: los
# estadisticos de orden, la media, el desvio. Solo lo que de verdad se tapo
# -decir "momentos" cuando la media ya era NA, o mientras se publicaba el
# desvio, declaraba una proteccion que no se aplico-. Lo comparten las dos
# vias, para que la misma columna diga lo mismo por `perfilar()` y por
# `perfilar_dbi()`. `NA` donde no se tapo nada.
.detalle_proteccion_estadisticos <- function(orden, media, desvio) {
  momentos <- ifelse(
    media & desvio, "momentos",
    ifelse(media, "la media", ifelse(desvio, "el desvio", NA_character_))
  )
  ifelse(
    orden & !is.na(momentos), paste0("[estadisticos de orden y ", momentos,
                                     " protegidos]"),
    ifelse(orden, "[estadisticos de orden protegidos]",
           ifelse(!is.na(momentos), "[momentos protegidos]", NA_character_))
  )
}

.proteger_componentes_perfil <- function(columnas, patrones, dependencias,
                                          hallazgos, clasificacion,
                                          valores_debiles = NULL) {
  sensibles <- .columnas_personales_protegidas(clasificacion)
  # Dos alcances, y la diferencia es deliberada. `sensibles` gobierna los
  # estadisticos de la tabla de columnas —minimo, maximo, moda—, donde suprimir
  # de mas tiene un costo real: una columna de importes con forma de documento
  # perderia su resumen cuantitativo. `clasificadas` gobierna la evidencia de
  # los hallazgos, donde el valor casi nunca hace falta: el hallazgo `constante`
  # dice que la columna tiene un unico valor, y para actuar sobre eso no se
  # necesita saber cual. Ahi conviene ocultar aunque la clasificacion sea debil
  # -sus VALORES, no la evidencia entera: ver `indices_debiles` abajo-.
  clasificadas <- .columnas_personales_clasificadas(clasificacion)
  if (!length(sensibles) && !length(clasificadas)) {
    return(list(
      columnas = columnas, patrones = patrones, dependencias = dependencias,
      hallazgos = hallazgos
    ))
  }
  reemplazo <- "[valor protegido]"
  indices_columnas <- .nombres_para_operar(columnas$columna) %in%
    .nombres_para_operar(sensibles)
  ocultar_moda <- indices_columnas & !is.na(columnas$moda) &
    columnas$moda != reemplazo
  columnas$moda[ocultar_moda] <- reemplazo
  # `media` entra por lo mismo que la via DBI ya la tapaba, y con el mismo
  # argumento que esta escrito alla: la media de las cedulas de una tabla chica
  # reconstruye demasiado. Que una puerta la ocultara y la otra la publicara
  # significaba que la proteccion dependia de por donde entraras: la misma
  # columna de documentos salia con `media = 5108024` por `perfilar()` y con
  # `media = NA` por `perfilar_dbi()`.
  # `centinela_valor` publica un valor de celda de la columna, igual que el
  # minimo o la moda, asi que tiene que taparse por el mismo motivo. Quedaba
  # afuera y sobre una columna de documentos protegida el perfil mostraba
  # `moda = "[valor protegido]"`, `minimo = NA`... y `centinela_valor = 9999`.
  # Que el valor sea casi seguro un centinela y no un documento no cambia la
  # regla: la proteccion no adivina cuales valores son inocentes.
  # Los campos de la secuencia entera codifican el rango aunque no lo muestren:
  # `n_posiciones` es `maximo - minimo + 1`, `n_huecos` es eso menos los
  # distintos, y la densidad es los distintos sobre eso. Con cualquiera de los
  # tres y un segundo dato del mismo perfil se despeja el par.
  #
  # Medido sobre una columna de cedulas protegida: `n_posiciones` = 599.891 junto
  # con los ordenes de magnitud que publica Benford -log10(maximo/minimo)- son
  # dos ecuaciones con dos incognitas y devuelven **27 y 599.917 exactos**,
  # mientras `minimo` y `maximo` salian en NA como corresponde. Proteger el
  # minimo y el maximo y dejar publicado su rango no protege nada.
  # Y `desvio`, como la media, desde la ronda 32. Era la unica excepcion
  # declarada -la decision F-6: "una dispersion no identifica a nadie"-, y la
  # decision cambio: el desvio de una columna protegida es una magnitud de sus
  # valores, y junto con lo que el resto del perfil publica de la columna
  # acerca el valor. Se tapa en todas las vias, con la misma lista: ver
  # `.CAMPOS_MOMENTO_PROTEGIDOS`.
  #
  # Una lista de campos a ocultar falla ABIERTA: el campo que se agregue manana
  # se publica salvo que alguien se acuerde de anotarlo aca. Por eso viaja una
  # prueba que no lee esta lista: perfila una columna protegida de magnitudes
  # conocidas y exige que ningun campo publicado traiga una magnitud de la
  # columna, sin excepciones.
  campos_numericos <- intersect(
    c(
      "minimo", "maximo", "mediana", "media", "desvio", "centinela_valor",
      "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
      "hueco_maximo_secuencia_entera", "densidad_secuencia_entera",
      "densidad_sin_centinela"
    ),
    names(columnas)
  )
  campos_momento <- .CAMPOS_MOMENTO_PROTEGIDOS
  campos_texto <- intersect(
    c(
      "minimo_exacto", "maximo_exacto", "minimo_fecha", "maximo_fecha",
      "media_fecha", "mediana_fecha"
    ),
    names(columnas)
  )
  # La moda es un valor observado, igual que los extremos. Si se reemplazo,
  # el detalle tiene que declararlo aunque la columna no tenga otros campos de
  # orden para tapar.
  tenia_orden <- ocultar_moda
  tenia_media <- rep(FALSE, nrow(columnas))
  tenia_desvio <- rep(FALSE, nrow(columnas))
  for (campo in campos_numericos) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]])
    if (identical(campo, "desvio")) {
      tenia_desvio <- tenia_desvio | ocultar
    } else if (campo %in% campos_momento) {
      tenia_media <- tenia_media | ocultar
    } else {
      tenia_orden <- tenia_orden | ocultar
    }
    columnas[[campo]][ocultar] <- NA_real_
  }
  for (campo in campos_texto) {
    ocultar <- indices_columnas & !is.na(columnas[[campo]]) &
      nzchar(columnas[[campo]])
    if (campo %in% campos_momento) {
      tenia_media <- tenia_media | ocultar
    } else {
      tenia_orden <- tenia_orden | ocultar
    }
    columnas[[campo]][ocultar] <- reemplazo
  }
  # La caja envolvente es una estadistica espacial distinta de los extremos
  # numericos, pero sobre domicilios publica coordenadas de personas. Se
  # conserva el bloque geometrico y su alcance, ocultando sólo los cuatro
  # valores de coordenadas.
  campos_bbox <- intersect(
    c("bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax"),
    names(columnas)
  )
  geometria <- rep(FALSE, nrow(columnas))
  if ("representacion_geometria" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$representacion_geometria)
  }
  if ("tipo_geometria" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$tipo_geometria)
  }
  if ("bbox_alcance" %in% names(columnas)) {
    geometria <- geometria | !is.na(columnas$bbox_alcance)
  }
  geometria <- indices_columnas & geometria
  if (length(campos_bbox)) {
    for (campo in campos_bbox) {
      ocultar <- geometria & !is.na(columnas[[campo]])
      columnas[[campo]][ocultar] <- NA_real_
    }
  }
  # Solo donde habia un alcance que declarar: una geometria que no se pudo
  # convertir no tiene bbox, y decir que no se publico "por geometria
  # protegida" declararia un tapado que no ocurrio. Con el nombre de campo mal
  # escrito -`representacion_geometrica`- esas filas nunca llegaban aca; al
  # corregirlo, la sonda mostro que llegaban a esta linea con `bbox_alcance` NA.
  if ("bbox_alcance" %in% names(columnas)) {
    con_alcance <- geometria & !is.na(columnas$bbox_alcance)
    columnas$bbox_alcance[con_alcance] <-
      "no_publicado_por_geometria_protegida"
  }
  if (!"detalle_proteccion_personal" %in% names(columnas)) {
    columnas$detalle_proteccion_personal <- NA_character_
  }
  # El texto dice lo que de verdad se tapo. Decir "y momentos" cuando la media
  # ya era NA seria declarar una proteccion que no se aplico.
  detalle <- .detalle_proteccion_estadisticos(tenia_orden, tenia_media,
                                              tenia_desvio)
  columnas$detalle_proteccion_personal[!is.na(detalle)] <- detalle[!is.na(detalle)]
  for (i in intersect(which(indices_columnas), seq_along(patrones))) {
    if (.nombres_para_operar(columnas$columna[[i]]) %in%
        .nombres_para_operar(sensibles) &&
        "ejemplos" %in% names(patrones[[i]])) {
      patrones[[i]]$ejemplos[nzchar(patrones[[i]]$ejemplos)] <- reemplazo
      resumen <- attr(patrones[[i]], "resumen_patrones", exact = TRUE)
      if (!is.null(resumen) && "ejemplos" %in% names(resumen)) {
        resumen$ejemplos[nzchar(resumen$ejemplos)] <- reemplazo
        attr(patrones[[i]], "resumen_patrones") <- resumen
      }
      desvios <- attr(patrones[[i]], "desvios_patron_raro", exact = TRUE)
      if (!is.null(desvios) && "ejemplos" %in% names(desvios)) {
        desvios$ejemplos[nzchar(desvios$ejemplos)] <- reemplazo
        attr(patrones[[i]], "desvios_patron_raro") <- desvios
      }
    }
  }
  # `columna` puede ser una columna simple o una lista de columnas
  # separadas por comas. La decisión se toma por el contenido, no por el tipo
  # de hallazgo, para que los nuevos hallazgos compuestos queden protegidos
  # automáticamente.
  columna_completa <- as.character(hallazgos$columna)
  coincide_con <- function(lista) {
    claves <- .nombres_para_operar(lista)
    .nombres_para_operar(columna_completa) %in% claves |
      vapply(.partir_conservando_marca(columna_completa, ","),
             function(columnas) any(.nombres_para_operar(columnas) %in% claves),
             logical(1L))
  }
  candidatos <- !is.na(hallazgos$columna) &
    hallazgos$tipo_hallazgo != "dato_personal_posible"
  # La evidencia de la columna con clasificacion DEBIL -que no se protege- no
  # se tapa entera: se le tapan sus valores, con el piso. Tapada entera, sus
  # hallazgos perdian tambien la evidencia que no tiene ningun valor de celda
  # -"4939 valores distintos de 5000 (0.988)", "3 valores", "N/D (30)"-, y la
  # ayuda dice que lo debil se informa sin ocultar valores. Medido en la ronda
  # 26: nueve de nueve hallazgos de columnas debiles sin evidencia. Sin los
  # datos -un perfil que se vuelve a proteger sin su tabla- no hay valores con
  # que barrer, y se tapa entera como antes: el lado cerrado.
  if (is.null(valores_debiles)) {
    indices_hallazgos <- candidatos & coincide_con(clasificadas)
    indices_debiles <- rep(FALSE, length(candidatos))
  } else {
    indices_hallazgos <- candidatos & coincide_con(sensibles)
    indices_debiles <- candidatos & !indices_hallazgos &
      coincide_con(clasificadas)
  }
  # La ausencia estructural es una señal válida aunque el determinante sea
  # personal. Se conserva la predicción y su precisión, pero no el corte ni los
  # niveles que permitirían reconstruir valores de la columna protegida.
  # Corre antes de tapar la evidencia, porque reconoce el determinante en ella:
  # si la dependiente tambien era personal, la evidencia ya estaba tapada y la
  # sugerencia salia con los niveles o el corte del determinante.
  hallazgos <- .proteger_ausencia_estructural(hallazgos, sensibles)
  hallazgos$evidencia[indices_hallazgos] <- "[evidencia protegida]"
  if (any(indices_debiles) && length(valores_debiles)) {
    campos <- intersect(c("evidencia", "descripcion"), names(hallazgos))
    barridos <- .proteger_textos_salida(
      as.data.frame(lapply(hallazgos[indices_debiles, campos, drop = FALSE],
                           as.character), stringsAsFactors = FALSE),
      valores_debiles, intocables = as.character(columnas$columna)
    )
    for (campo in campos) {
      hallazgos[[campo]][indices_debiles] <- barridos[[campo]]
    }
  }
  # La evidencia se tapaba y la descripcion no, y hay hallazgos que nombran un
  # valor de celda ahi adentro: `posible_centinela_numerico` decia "El valor
  # 9999 aparece 5 veces". Sobre una columna de documentos protegida eso es una
  # fuga por la puerta de al lado, y llegaba hasta el informe HTML.
  #
  # No se tapa la descripcion entera, que explica el diagnostico y hay que poder
  # leerla: se saca el valor y se dice que se saco.
  con_valor <- indices_hallazgos &
    grepl("^El valor ", as.character(hallazgos$descripcion))
  if (any(con_valor)) {
    hallazgos$descripcion[con_valor] <- sub(
      "^El valor [^ ]+ aparece", "Un valor aparece",
      as.character(hallazgos$descripcion[con_valor])
    )
    hallazgos$descripcion[con_valor] <- paste(
      hallazgos$descripcion[con_valor],
      "El valor no se nombra porque la columna esta protegida."
    )
  }
  # La clave declarada es, por definicion, lo que identifica una fila: es la
  # que permite ir a verificar el caso y tambien la que identifica a una
  # persona. Si alguna de sus columnas quedo clasificada como personal, sus
  # valores salen enmascarados igual que la evidencia, en TODOS los hallazgos y
  # no solo en los de las columnas sensibles: la clave no pertenece a la
  # columna del hallazgo, viaja con la fila.
  hallazgos <- .proteger_claves_trazabilidad(hallazgos, sensibles)
  if (nrow(dependencias)) {
    indices_dependencias <-
      .nombres_para_operar(dependencias$determinante) %in%
        .nombres_para_operar(sensibles) |
      .nombres_para_operar(dependencias$dependiente) %in%
        .nombres_para_operar(sensibles)
    dependencias$evidencia[indices_dependencias & nzchar(dependencias$evidencia)] <-
      "[evidencia protegida]"
  }
  list(
    columnas = columnas, patrones = patrones, dependencias = dependencias,
    hallazgos = hallazgos
  )
}

.proteger_claves_trazabilidad <- function(hallazgos, sensibles) {
  if (!nrow(hallazgos) || !length(sensibles)) return(hallazgos)
  if (!"trazabilidad" %in% names(hallazgos)) return(hallazgos)
  hallazgos$trazabilidad <- I(lapply(hallazgos$trazabilidad, function(traza) {
    claves <- traza$claves
    if (is.null(claves) || !is.data.frame(claves) || !nrow(claves)) {
      return(traza)
    }
    indices_protegidas <- .indice_nombre(sensibles, names(claves))
    protegidas <- names(claves)[
      unique(indices_protegidas[!is.na(indices_protegidas)])
    ]
    if (!length(protegidas)) return(traza)
    for (columna in protegidas) {
      claves[[columna]] <- rep("[clave protegida]", nrow(claves))
    }
    traza$claves <- claves
    traza$claves_protegidas <- protegidas
    traza
  }))
  hallazgos
}

# `meta` y `cobertura_diagnosticos` quedaban fuera de la proteccion, y las dos
# publican cantidades derivadas de los extremos: Benford guarda
# `ordenes_magnitud`, que es log10(maximo/minimo), y el motivo de la cobertura lo
# imprime en el texto. Ninguno muestra un valor de celda, y por eso se pasaron
# por alto: lo que se filtra no es el dato sino la relacion entre dos datos, que
# con otra del mismo perfil se despeja.
.proteger_meta_y_cobertura <- function(perfil, sensibles,
                                       valores = character()) {
  if (!length(sensibles)) return(perfil)
  if (length(perfil$meta$sentinelas_numericos)) {
    sentinelas <- perfil$meta$sentinelas_numericos
    del_catalogo <- sentinelas %in% .numeros_na_locales
    if (length(valores)) {
      # Con el piso, como el resto de la salida: un centinela corto no identifica.
      perfil$meta$sentinelas_numericos <- .proteger_numeros_parametros(
        sentinelas, .valores_identificantes(valores)
      )
    } else {
      # Un perfil abierto puede llegar a `reportar()` sin conservar `datos`.
      # En ese caso no se puede saber que centinela pertenece a que columna;
      # conservar la lista completa publicaria uno si coincide con una columna
      # protegida, asi que se oculta de forma conservadora. El catalogo del
      # paquete no es de nadie y queda.
      sentinelas[!del_catalogo] <- NA_real_
      perfil$meta$sentinelas_numericos <- sentinelas
    }
  }
  resultados <- perfil$meta$benford$resultados
  if (length(resultados)) {
    nombres_resultados <- vapply(resultados, `[[`, character(1L), "columna")
    indices_resultados <- which(.nombres_para_operar(nombres_resultados) %in%
                                .nombres_para_operar(sensibles))
    for (indice in indices_resultados) {
      perfil$meta$benford$resultados[[indice]]$ordenes_magnitud <- NA_real_
    }
  }
  cobertura <- perfil$cobertura_diagnosticos
  if (inherits(cobertura, "data.frame") && nrow(cobertura) &&
        "columna" %in% names(cobertura) && "motivo" %in% names(cobertura)) {
    afectadas <- .nombres_para_operar(cobertura$columna) %in%
      .nombres_para_operar(sensibles)
    if (any(afectadas)) {
      cobertura$motivo[afectadas] <- gsub(
        "log10\\(max/min\\)[[:space:]]*[-+0-9.eE]+",
        "log10(max/min) [valor protegido]",
        as.character(cobertura$motivo[afectadas])
      )
      perfil$cobertura_diagnosticos <- cobertura
    }
  }
  perfil
}

.MIN_LARGO_VALOR_IDENTIFICANTE <- 6L

# Si cada valor tiene la forma de una fecha de calendario sola -con un ano
# plausible, o a medianoche-. La comparten el piso y la decision por columna.
.es_fecha_sola <- function(valores) {
  dia <- "(0?[1-9]|[12][0-9]|3[01])"
  mes <- "(0?[1-9]|1[0-2])"
  # Tambien la fecha escrita como fecha-hora a medianoche -ISO con `T`, como la
  # escriben muchos sistemas una fecha sin hora-: es la misma fecha.
  medianoche <- "([T ]00:00(:00(\\.0+)?)?(Z| ?UTC)?)?"
  # Con un ANO plausible, el mismo rango que la regla de digitos: con cualquiera,
  # el telefono guardado "2901-12-12" se trataba como fecha, salia del piso y se
  # publicaba exacto. La misma regla estaba escrita dos veces y la ronda 22 la
  # arreglo en una. Medido en la ronda 23.
  anio <- .ANIO_PLAUSIBLE
  # Y con espacios y el ano al final -"16 01 2002"-, como la regla de digitos.
  fecha <- paste0(
    "^(", anio, "[-/.]", mes, "[-/.]", dia, "|", dia, "[-/.]", mes, "[-/.]", anio, "|",
    mes, "[-/.]", dia, "[-/.]", anio, "|", dia, " ", mes, " ", anio, "|",
    mes, " ", dia, " ", anio, ")", medianoche, "$"
  )
  salida <- grepl(fecha, valores, perl = TRUE, useBytes = TRUE)
  salida[is.na(valores)] <- FALSE
  salida
}

# Los valores con forma de fecha de las columnas protegidas que NO son de
# fechas: en ellas la forma es casualidad -un telefono fijo "2012-11-05", que
# tiene mes y dia validos- y el valor es el dato de alguien. Se decide por la
# COLUMNA, con `.columna_de_fechas()`, la misma regla que saca del piso las
# fechas de una columna de fechas en `.valores_publicables_protegidos()`: es de
# fechas si es `Date` o `POSIXt`, o si la mitad o mas de sus valores distintos
# tienen forma de fecha. La ronda 23 estrecho el rango de anos y el piso seguia
# decidiendo por el valor: un fijo de Montevideo escrito 4-2-2 que empieza con
# `20` y tiene mes y dia validos -uno de cada trescientos- se publicaba exacto.
# Medido en la ronda 24.
.fechas_que_identifican <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) {
    return(character())
  }
  indices <- .indice_nombre(sensibles, names(datos))
  unique(unlist(lapply(unique(indices[!is.na(indices)]), function(indice) {
    x <- datos[[indice]]
    if (!is.atomic(x) || inherits(x, c("Date", "POSIXt")) || is.numeric(x) ||
        is.logical(x)) {
      return(character())
    }
    .fechas_de_columna_no_fecha(tryCatch(as.character(x),
                                         error = function(e) character()))
  }), use.names = FALSE))
}

.fechas_de_columna_no_fecha <- function(textos) {
  textos <- unique(textos[!is.na(textos) & nzchar(textos)])
  if (!length(textos)) return(character())
  fechas <- .es_fecha_sola(textos)
  if (!any(fechas) || .columna_de_fechas(textos)) return(character())
  textos[fechas]
}

.valores_identificantes <- function(valores,
                                    fechas_que_identifican = character()) {
  # El piso de la proteccion, en un solo lugar. Un valor identifica si tiene
  # seis caracteres o mas: es el mismo corte con el que la bateria de fugas
  # decide que cuenta como filtracion, y tener dos definiciones distintas en la
  # regla y en su prueba es por donde se cuela un defecto.
  #
  # `allowNA = TRUE`, y el NA cuenta como identificante: `nchar()` con
  # `type = "chars"` ABORTA sobre una cadena que no es UTF-8 valido, y este
  # paquete trabaja con codificaciones rotas. Ante un valor cuyo largo no se
  # puede medir, protegerlo es el lado seguro.
  if (!length(valores)) return(character())
  # El marcador nunca es una aguja. Si el perfil del que se cosechan los valores
  # ya paso por proteccion, `"[valor protegido]"` entra en la lista, y su forma
  # sin separadores -`valorprotegido`- coincide con la del estado
  # `valor_protegido`: el marcador terminaba enmascarandose a si mismo y un
  # campo que declara la proteccion pasaba a declarar un valor.
  valores <- valores[
    !is.na(valores) & nzchar(valores) & valores != "[valor protegido]"
  ]
  if (!length(valores)) return(character())
  # Se mide y se compara sobre la forma DECLARADA -los bytes UTF-8 validos sin
  # marca son UTF-8, `.declarar_utf8_valido()`- y se devuelve el valor como llego.
  # Bajo `LC_CTYPE=C`, `nchar()` de un texto sin marca cuenta bytes -`Angel` con
  # tilde daba 6 y pasaba el piso, que en una sesion UTF-8 da 5- y `trimws()`
  # sobre un vector con marcas mezcladas avisaba "unable to translate" o
  # abortaba: el filtro del catalogo de ausencias de abajo caia en su `tryCatch`
  # y no se aplicaba. La proteccion decidia distinto segun el locale.
  largos <- nchar(.declarar_utf8_valido(valores), type = "chars", allowNA = TRUE)
  valores <- valores[is.na(largos) | largos >= .MIN_LARGO_VALOR_IDENTIFICANTE]
  # Un relleno sin una letra ni un digito -"------", "......"- no es el dato de
  # nadie y no puede identificar a nadie. Medido en la ronda 25: el "------" de
  # un telefono protegido tapaba `sin observaciones ------ revisar` en otra
  # columna. Solo ASCII: un signo de otra escritura puede ser parte de un nombre.
  valores <- valores[
    grepl("[A-Za-z0-9]|[^\\x01-\\x7f]", valores, perl = TRUE, useBytes = TRUE)
  ]
  if (!length(valores)) return(character())
  # Una fecha de calendario sola no identifica, aunque tenga diez caracteres: en
  # una tabla de miles de personas casi todo dia es el cumpleanos de alguien. Con
  # la fecha de nacimiento protegida, el piso tapaba la media, la mediana, el
  # minimo y la moda de TODAS las demas columnas de fechas -doce estadisticos con
  # 50.000 personas, medido en una refutacion- y el umbral de fecha de una
  # sugerencia, que no son la fecha de nadie. La columna de fechas se sigue
  # protegiendo entera, por columna; lo que sale del piso es la busqueda de sus
  # valores en el resto de la salida. Una fecha con hora si queda: con segundos
  # vuelve a ser casi unica.
  #
  # Salvo las de `fechas_que_identifican`: un valor con forma de fecha de una
  # columna protegida que NO es de fechas -el telefono "2012-11-05" entre
  # cuatrocientos telefonos- es el dato de alguien, y salia del piso. Se decide
  # por la columna: ver `.fechas_que_identifican()`. Medido en la ronda 24.
  es_fecha <- .es_fecha_sola(valores)
  valores <- valores[!es_fecha | valores %in% fechas_que_identifican]
  # Y un marcador de ausencia del catalogo del paquete -`sin dato`, `N/A`- no es
  # el dato de nadie aunque aparezca en una columna de nombres: es vocabulario
  # compartido, como dice `?perfilar`. Como aguja tapaba el marcador en las
  # demas columnas, y en el plan dejaba sin efecto la accion que lo convierte en
  # `NA` en una columna vecina, recomendada y activa. Medido en una refutacion.
  # La columna protegida lo sigue tapando, por columna.
  # El pliegue de los detectores de ausencia (`.minusculas_para_comparar()`):
  # el `trimws()` antes de declarar avisaba bajo `C` con un nombre latin1, y
  # bajo un locale UTF-8 un solo valor con un byte suelto hacia caer el filtro
  # entero en el `tryCatch` (ronda 30-L).
  catalogo <- .minusculas_para_comparar(c(
    .cadenas_na_naniar_1_1_0, .cadenas_na_locales
  ))
  normalizados <- tryCatch(
    .minusculas_para_comparar(valores),
    error = function(e) rep(NA_character_, length(valores))
  )
  valores[is.na(normalizados) | !normalizados %in% catalogo]
}


.proteger_perfil <- function(perfil, datos = NULL) {
  valores_perfil <- .valores_perfil_protegidos(
    perfil$columnas, perfil$patrones, perfil$datos_personales, perfil$meta,
    datos = datos
  )
  # Los valores de las columnas con clasificacion debil, para tapar solo eso en
  # la evidencia de sus hallazgos. Sin los datos, `NULL`: se tapa entera.
  debiles <- setdiff(
    .columnas_personales_clasificadas(perfil$datos_personales),
    .columnas_personales_protegidas(perfil$datos_personales)
  )
  valores_debiles <- if (length(debiles) && inherits(datos, "data.frame")) {
    tryCatch(.valores_identificantes(
      .valores_publicables_protegidos(datos, debiles),
      .fechas_que_identifican(datos, debiles)
    ), error = function(e) NULL)
  } else NULL
  protegidos <- .proteger_componentes_perfil(
    perfil$columnas, perfil$patrones, perfil$dependencias,
    perfil$hallazgos, perfil$datos_personales,
    valores_debiles = valores_debiles
  )
  perfil$columnas <- protegidos$columnas
  # `dato_personal_protegido` dice si el valor **quedo** protegido -asi esta
  # escrito donde se fija al perfilar-, y esta funcion es la que lo protege.
  # Cuando corre sobre un perfil hecho SIN proteccion -por ejemplo al escribir
  # con `guardar_analisis()`, que vuelve a aplicarla antes de guardar- los
  # valores salian enmascarados y la marca seguia en `FALSE`: el objeto decia
  # "no hay columnas protegidas" al lado de una moda `[valor protegido]`, y
  # quien filtra por la marca no encontraba nada que el archivo si ocultaba.
  if ("dato_personal_protegido" %in% names(perfil$columnas) &&
      inherits(perfil$datos_personales, "data.frame")) {
    protegidas <- .nombres_para_operar(perfil$columnas$columna) %in%
      .nombres_para_operar(.columnas_personales_protegidas(perfil))
    perfil$columnas$dato_personal_protegido <-
      as.logical(perfil$columnas$dato_personal_protegido) | protegidas
  }
  perfil$patrones <- protegidos$patrones
  perfil$dependencias <- protegidos$dependencias
  perfil$hallazgos <- protegidos$hallazgos
  # La magnitud de Benford y el `log10(max/min)` de la cobertura, solo de las
  # columnas PROTEGIDAS: de una con clasificacion debil se publican el minimo y
  # el maximo, y su cociente no oculta nada. Se tapaban las de toda columna con
  # tipo de dato personal. Medido en la ronda 26.
  sensibles <- .columnas_personales_protegidas(perfil)
  valores <- .valores_publicables_protegidos(
    datos, .columnas_personales_protegidas(perfil)
  )
  valores <- unique(c(valores_perfil, valores))
  valores <- valores[!is.na(valores) & nzchar(valores)]
  valores <- valores[.orden_por_bytes(nchar(valores, type = "bytes"), decreasing = TRUE,
                           method = "radix")]
  perfil <- .proteger_meta_y_cobertura(perfil, sensibles, valores)
  # `meta` describe la CORRIDA, no los datos: la version del paquete, los
  # umbrales, la configuracion. Ya tiene su propio paso dirigido y por columna
  # -centinelas, Benford, cobertura- justo arriba, y el barrido general lo
  # volvia a pisar reemplazando cualquier coincidencia de texto. Medido: con
  # una columna protegida que contiene el valor "1", `meta$version` salia como
  # "0.[valor protegido].0" y la fuente de una medicion decia
  # "[valor protegido].000 columnas".
  meta_declarada <- perfil$meta
  # El campo `patron` es un VOCABULARIO DE FORMA, no un valor: `9+-9+-9+`
  # significa "digitos, guion, digitos, guion, digitos", y ese `9` es el simbolo
  # de "digito". Enmascararlo destruye la unica informacion que el campo existe
  # para dar -quedaba `[valor protegido]+[valor protegido]+[valor protegido]+`-
  # y no protege nada, porque el patron esta construido sobre un alfabeto fijo y
  # nunca lleva un valor de la tabla. Vale para toda columna, protegida o no.
  #
  # `ejemplos` es lo contrario: son valores de la tabla y siguen enmascarados.
  patrones_forma <- NULL
  if (is.list(perfil$patrones) && length(perfil$patrones)) {
    patrones_forma <- lapply(perfil$patrones, function(x) {
      if (is.list(x) && !is.null(x$patron)) x$patron else NULL
    })
  }
  # Enmascarado POR COLUMNA. `.proteger_componentes_perfil()` ya tapo, columna
  # por columna, lo que describe a una columna protegida: su `moda`, sus
  # estadisticos, sus `ejemplos` y la evidencia de sus hallazgos. El barrido
  # general que viene despues busca los valores en TODA la salida, y ahi tapaba
  # de mas: `"S/D"` es un centinela de `cedula` y a la vez el "sin dato" de
  # `sexo`, asi que `sexo` publicaba `[valor protegido]` sin tener un solo dato
  # personal, y el lector no puede distinguir eso de una columna que si los
  # tiene. Publicar `"S/D"` en `sexo` no dice nada de la cedula.
  #
  # Se devuelve el estado por columna SOLO en los componentes donde ese paso ya
  # corrio. `formatos_fecha` y `cobertura_diagnosticos` no lo tienen, asi que
  # ahi el barrido general se queda: es la unica proteccion que tienen.
  # `general`, `datos_personales` y los atributos tampoco tienen columna
  # atribuible.
  # Enmascarado POR COLUMNA, CON UN PISO.
  #
  # `.proteger_componentes_perfil()` ya tapo, columna por columna, lo que
  # describe a una columna protegida: su `moda`, sus estadisticos, sus
  # `ejemplos` y la evidencia de sus hallazgos. Ahi entra TODO valor, largo o
  # corto. El barrido general que viene ahora busca los valores en toda la
  # salida, y lleva SOLO los que identifican.
  #
  # Por que el piso. Sin el, una columna que el clasificador no marco publicaba
  # los valores de la protegida con solo repetirlos: medido, una copia llamada
  # `codigo_operacion` y hasta un texto libre con el documento adentro
  # publicaban tres de cuatro documentos de ocho digitos. Con el piso, un valor
  # que identifica no sale por ninguna puerta, tenga o no columna atribuible.
  #
  # Por que el corte en seis. No es un numero elegido aca: es el que la bateria
  # de fugas ya usa para decidir que cuenta como filtracion, y usar dos
  # definiciones distintas de "identificante" en la regla y en su prueba es
  # exactamente como se cuela un defecto entre las dos.
  #
  # Y por que alcanza con filtrar. El vocabulario corto -`"S/D"`, que es
  # centinela de una cedula y a la vez el "sin dato" de `sexo`- ya quedo tapado
  # donde describe a la columna protegida y no lo toca nadie mas, asi que `sexo`
  # lo publica. No hace falta restituir nada despues del barrido: lo que el
  # barrido no lleva, no lo pisa.
  # `allowNA = TRUE`, y el NA cuenta como identificante. Sin eso, `nchar()` con
  # `type = "chars"` ABORTA sobre una cadena que no es UTF-8 valido -"invalid
  # multibyte string"-, y este paquete trabaja justamente con codificaciones
  # rotas: la suite lo rompio en `detectar_duplicados_aproximados()`. Ante un
  # valor cuyo largo no se puede medir, protegerlo es el lado seguro: es una
  # funcion de privacidad y el valor por omision tiene que ser cerrado.
  identificantes <- .valores_identificantes(
    valores, .fechas_que_identifican(datos, .columnas_personales_protegidas(perfil))
  )
  # Los nombres se toman ANTES de enmascarar: son los de la entrada.
  perfil <- .proteger_textos_salida(
    perfil, identificantes,
    intocables = as.character(perfil$columnas$columna)
  )
  # Y lo mismo sobre los campos NUMERICOS, que el barrido de texto no toca.
  # Sin esto el piso quedaba a medias: medido, una columna copia clasificada
  # `documento_identidad` con poder discriminante debil -asi que no se protege-
  # publicaba en `minimo`, `maximo`, `media` y `mediana` los documentos de la
  # columna que si esta protegida, en el mismo objeto donde esa columna sale
  # enmascarada. Sobre doce filas, `minimo` y `maximo` son dos documentos
  # exactos. Se reusa el ayudante que ya tapaba los parametros de una accion del
  # plan, con el mismo criterio: un numero cuya representacion es un valor
  # identificante protegido no se publica.
  columnas_antes <- perfil$columnas
  perfil <- .proteger_numeros_parametros(perfil, identificantes)
  perfil$columnas <- .tapar_derivados_del_piso(columnas_antes, perfil$columnas)
  perfil$meta <- meta_declarada
  if (!is.null(patrones_forma)) {
    for (i in seq_along(patrones_forma)) {
      if (!is.null(patrones_forma[[i]]) && !is.null(perfil$patrones[[i]])) {
        perfil$patrones[[i]]$patron <- patrones_forma[[i]]
      }
    }
  }
  perfil
}

.proteger_duplicados_aproximados <- function(resultado, sensibles) {
  if (!inherits(resultado, "duplicados_aproximados") || !length(sensibles) ||
      !inherits(resultado$pares, "data.frame") || !nrow(resultado$pares)) {
    return(resultado)
  }
  resultado$pares$evidencia_1 <- "[valor protegido]"
  resultado$pares$evidencia_2 <- "[valor protegido]"
  resultado$pares$proteccion_evidencia <- "[valores personales protegidos]"
  resultado$proteccion_aplicada <- TRUE
  resultado
}
