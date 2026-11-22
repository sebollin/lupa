# Sello de cada metrica instanciada en una medicion: la version de lupa que la
# midio y una huella de su definicion.
#
# Dos corridas de la misma metrica son comparables si miden lo mismo, y hasta
# aca la medicion no lo decia: el perfil guardaba la version del paquete y la
# medicion no, asi que la serie de una metrica cuya regla, referencial u orden
# de texto cambio se leia como un cambio de los datos. La huella resume la
# definicion; la version dice con que implementacion se midio. Comparar dos
# mediciones -`rbind()`, `historico_calidad()`, `detectar_deriva_calidad()`-
# lee el sello y avisa cuando una metrica cambio de definicion.

# Que parte de la declaracion de la metrica es su definicion. La lista es
# explicita a proposito: un campo que se agregue a la declaracion no entra solo,
# porque cambiaria la huella de todas las metricas existentes y la serie de cada
# usuario acusaria un cambio de definicion que no hubo.
#
# `semantica` queda afuera: es la descripcion en prosa de la metrica, y corregir
# su redaccion no cambia lo que se mide. El mismo criterio que deja el metodo por
# omision fuera de `configuracion_modelo`: lo que es del paquete y no cambia el
# numero no se lee como un cambio del modelo de quien mide.
.CAMPOS_DEFINICION_METRICA <- c(
  "nombre", "granularidad", "tipo_resultado", "orientacion", "propiedades",
  "dimension", "factor"
)

# Version de la forma canonica de la definicion. Va dentro del texto que se
# resume: si un dia la forma cambia, las huellas nuevas no se confunden con las
# viejas.
.ESQUEMA_HUELLA_DEFINICION <- 1L

# Lo que identifica al sello dentro del historico, donde viaja como texto. La
# segunda forma lleva el nivel de certificacion de cada metrica; la primera se
# sigue leyendo, y su nivel se toma como parcial: no se sabe si era completo.
.MARCA_SELLO_HISTORICO <- "sello-2"
.MARCAS_SELLO_LEGIBLES <- c("sello-1", "sello-2")

# Los niveles de `certificacion`.
.CERTIFICACIONES <- c("completa", "parcial", "no_certificable")

# El md5 de cada texto, sobre sus bytes. `tools::md5sum(bytes = )` existe desde
# R 4.5.0; antes se escribe cada texto en un temporal, que da la misma suma. Se
# llama con `do.call()` para que el control de codigo de un R anterior no lea un
# argumento que no conoce.
.md5_textos <- function(textos,
                        via_archivo = !"bytes" %in% names(formals(tools::md5sum))) {
  if (!length(textos)) return(character())
  .md5_crudos(lapply(as.character(textos), charToRaw), via_archivo)
}

.md5_crudos <- function(crudos,
                        via_archivo = !"bytes" %in% names(formals(tools::md5sum))) {
  if (!length(crudos)) return(character())
  if (!isTRUE(via_archivo)) {
    return(vapply(crudos, function(b) {
      unname(do.call(tools::md5sum, list(bytes = b)))
    }, character(1L)))
  }
  archivos <- vapply(seq_along(crudos), function(i) {
    tempfile("lupa-huella-")
  }, character(1L))
  on.exit(unlink(archivos), add = TRUE)
  for (i in seq_along(crudos)) writeBin(crudos[[i]], archivos[[i]])
  unname(tools::md5sum(archivos))
}

# El referencial es parte de la definicion: medir contra otro padron es medir
# otra cosa. Entra lo que decide la medida -la clave, los valores, si es
# completo, su alcance, la normalizacion y las filas de esas columnas- y no su
# `nombre`, que es una etiqueta y por omision sale del simbolo con que se lo
# llamo. Las filas se ordenan por la clave: el referencial es un conjunto de
# pares clave-valor y el orden en que llegaron no cambia ninguno. Y una columna
# factor entra como su texto: las metricas del referencial comparan el texto de
# cada valor (`.texto_identidad()`), asi que `factor(c("a", "z"))` y
# `c("a", "z")` son el mismo padron. Las demas columnas entran con su clase.
#
# Sin despachar nada: las columnas se leen con `.subset2()`, se ordenan por su
# valor sin clase y se reordenan sin `[`. Una columna de mas de
# `.TOPE_VECTOR_REFERENCIAL` elementos no se lee: el referencial queda descrito
# por su tamano y la huella es parcial (`.vector_sin_recorrer()`).
.referencial_para_huella <- function(referencial, contexto) {
  if (!inherits(referencial, "referencial")) return(referencial)
  campo <- function(nombre) .subset2(referencial, nombre)
  columnas <- c(campo("clave"), campo("valor"))
  datos <- campo("datos")
  # La metrica lee el referencial y su tabla con `$` y `[`: su clase cuenta.
  .revisar_clases(c(oldClass(referencial), oldClass(datos)), contexto)
  nombres_datos <- names(unclass(datos))
  indices <- .indice_nombre(columnas, nombres_datos)
  tabla <- lapply(indices, function(i) {
    if (is.na(i)) return(NULL)
    columna <- .subset2(datos, i)
    if (!is.atomic(columna)) return(columna)
    n <- .largo_sin_despacho(columna, contexto)
    if (is.na(n) || n > .TOPE_VECTOR_REFERENCIAL) {
      return(.vector_sin_recorrer(columna, n, contexto, .TOPE_VECTOR_REFERENCIAL))
    }
    if (inherits(columna, "factor")) {
      .revisar_clases(oldClass(columna), contexto)
      return(.texto_de_factor(columna))
    }
    .copia_propia(columna)
  })
  names(tabla) <- columnas
  recorridas <- vapply(tabla, function(columna) {
    is.atomic(columna)
  }, logical(1L))
  n_clave <- length(campo("clave"))
  if (n_clave && all(recorridas) && length(tabla[[1L]])) {
    claves <- lapply(tabla[seq_len(n_clave)], function(columna) {
      # Un `integer64` se ordena por su valor, que `.orden_por_bytes()` sabe
      # leer; lo demas, por su valor sin clase.
      if (inherits(columna, "integer64")) columna else .valores_planos(columna)
    })
    orden <- do.call(.orden_por_bytes, unname(claves))
    tabla <- lapply(tabla, function(columna) {
      atributos <- attributes(columna)
      atributos$names <- NULL
      valores <- unclass(columna)
      attributes(valores) <- NULL
      valores <- valores[orden]
      attributes(valores) <- atributos
      valores
    })
  }
  huella <- list(
    clave = campo("clave"), valor = campo("valor"),
    completo = campo("completo"), alcance = campo("alcance"),
    normalizar = campo("normalizar"), datos = tabla
  )
  # El referencial del motor no trae filas: lo que decide la medida es la tabla
  # que nombra. Solo entonces entra el campo, asi la huella del referencial en R
  # es la de siempre.
  # Y la fuente donde se midio: el mismo nombre de tabla en otra base es otro
  # padron, y la serie tiene que decirlo (`.fuente_referenciales_dbi()`).
  fuente <- campo("fuente_motor")
  if (!is.null(fuente)) huella$fuente_motor <- as.character(fuente)
  tabla_motor <- campo("tabla_motor")
  if (!is.null(tabla_motor)) {
    huella$tabla_motor <- if (is.null(names(tabla_motor))) {
      as.character(tabla_motor)
    } else {
      paste0(names(tabla_motor), "=", unname(tabla_motor))
    }
  }
  huella
}

# Los valores de un vector sin su clase ni sus atributos, leidos sin despachar:
# una fecha da sus dias, un instante sus segundos, un `integer64` sus bytes. Lo
# demas, tal cual sin atributos. Los atributos -el huso, las unidades- los
# describe quien llama.
.valores_planos <- function(x) {
  if (!is.atomic(x)) return(x)
  if (inherits(x, "integer64")) {
    valores <- unclass(x)
    attributes(valores) <- NULL
    return(writeBin(valores, raw(), size = 8L, endian = "little"))
  }
  valores <- unclass(x)
  attributes(valores) <- NULL
  valores
}

# El texto de un vector sin despachar: sin clase y sin atributos. Para los
# niveles de un factor, que pueden traer una clase de quien llama.
.texto_plano <- function(x) {
  if (is.object(x)) x <- unclass(x)
  attributes(x) <- NULL
  as.character(x)
}

.texto_de_factor <- function(x) {
  codigos <- unclass(x)
  niveles <- .texto_plano(attr(codigos, "levels", exact = TRUE))
  attributes(codigos) <- NULL
  niveles[codigos]
}

# Las reglas de `aplicabilidad` que recortan esta metrica: las de sus atributos.
# Una regla sobre otra columna no cambia lo que mide, y no puede cambiar su
# huella. Se nombran por el atributo de la instancia y se ordenan por bytes: la
# lista de `aplicabilidad` es un conjunto de reglas por columna.
.aplicabilidad_de_instancia <- function(aplicabilidad, instancia) {
  if (!is.list(aplicabilidad)) return(NULL)
  aplicabilidad <- unclass(aplicabilidad)
  if (!length(aplicabilidad)) return(NULL)
  if (is.null(names(aplicabilidad))) return(NULL)
  atributos <- as.character(unlist(instancia[["atributos"]]))
  atributos <- unique(atributos[!is.na(atributos) & nzchar(atributos)])
  if (!length(atributos)) return(NULL)
  indices <- .indice_nombre(atributos, names(aplicabilidad))
  presentes <- !is.na(indices)
  if (!any(presentes)) return(NULL)
  reglas <- aplicabilidad[indices[presentes]]
  names(reglas) <- atributos[presentes]
  reglas[.orden_por_bytes(names(reglas))]
}

# --- Que ve una medicion de sus tablas ---------------------------------------
#
# La huella necesita dos datos de lo que se mide: la clase de una columna -para
# saber si una regla ordena texto- y que columnas tiene una tabla -una formula de
# `aplicabilidad` busca sus nombres primero en la tabla y despues en su entorno-.
# En memoria salen de la tabla; en una base, del esquema que R lee.
.lector_tablas <- function(tablas) {
  # Una tabla se busca una vez por entidad: el lector se consulta por cada
  # metrica, y un modelo de sesenta metricas sobre una tabla la buscaba sesenta
  # veces.
  vistas <- new.env(parent = emptyenv())
  tabla_de <- function(entidad) {
    clave <- .clave_bytes(as.character(entidad))
    if (!exists(clave, envir = vistas, inherits = FALSE)) {
      tabla <- tryCatch(.obtener_tabla_modelo(tablas, entidad), error = function(e) NULL)
      assign(clave, if (inherits(tabla, "data.frame")) tabla else NULL, envir = vistas)
    }
    get(clave, envir = vistas, inherits = FALSE)
  }
  list(
    clase = function(entidad, columnas) {
      tabla <- tabla_de(entidad)
      if (is.null(tabla)) return(rep(NA_character_, length(columnas)))
      indices <- .indice_nombre(columnas, names(unclass(tabla)))
      vapply(indices, function(i) {
        if (is.na(i)) return(NA_character_)
        x <- .subset2(tabla, i)
        if (inherits(x, "factor")) "factor" else if (is.character(x)) "texto" else "otro"
      }, character(1L))
    },
    columnas = function(entidad) {
      tabla <- tabla_de(entidad)
      if (is.null(tabla)) character() else names(unclass(tabla))
    }
  )
}

.lector_esquemas <- function(esquemas) {
  list(
    clase = function(entidad, columnas) {
      esq <- esquemas[[entidad]]
      if (is.null(esq) || is.null(esq$clases)) {
        return(rep(NA_character_, length(columnas)))
      }
      clases <- esq$clases
      vapply(columnas, function(columna) {
        i <- .indice_nombre(columna, names(clases))
        if (is.na(i)) NA_character_ else
          if (identical(unname(clases[[i]]), "texto")) "texto" else "otro"
      }, character(1L), USE.NAMES = FALSE)
    },
    columnas = function(entidad) {
      esq <- esquemas[[entidad]]
      if (is.null(esq)) character() else as.character(esq$campos)
    }
  )
}

# --- Si la metrica ordena texto ----------------------------------------------
#
# `orden_texto = "bytes"` entra en la huella solo si cambia lo que se mide: una
# regla que el paquete interpreta con un operador de orden -el rango de
# `ValoresPosiblesPorComprension`, `<`, `<=`, `>` y `>=` en la formula de
# `ReglaIntegridadIntraEntidad`- y donde algun operando es texto EN ESTA
# medicion. Antes alcanzaba con un literal de texto en cualquier rama -`t == "a"`
# junto a `n > 1`- o con un atributo de texto que la formula no usaba, y la
# deriva declaraba `no_comparable` una serie que no habia cambiado. Medido.

# Una llamada con clase -armada con `structure()` o metida con `bquote()`- se
# evalua como cualquier llamada: la clase no cambia lo que hace, y se quita antes
# de recorrerla, porque `[[` y `as.list()` sobre ella despacharian sus metodos.
.llamada_plana <- function(expresion) {
  if (is.object(expresion) && (is.call(expresion) || is.expression(expresion))) {
    oldClass(expresion) <- NULL
  }
  expresion
}

# Si una expresion de la formula da texto. Es la misma regla de
# `.compara_como_texto()`: un literal de texto, una columna `character` -un
# factor no, R lo compara por sus propios metodos- o lo que arma texto con los
# constructores que la formula admite.
.expresion_de_texto <- function(expresion, columna_de_texto) {
  if (is.character(expresion)) return(TRUE)
  if (is.symbol(expresion)) {
    return(isTRUE(columna_de_texto(as.character(expresion))))
  }
  if (!is.call(expresion)) return(FALSE)
  expresion <- .llamada_plana(expresion)
  if (!is.symbol(expresion[[1L]])) return(FALSE)
  operador <- as.character(expresion[[1L]])
  argumentos <- as.list(expresion)[-1L]
  if (!length(argumentos)) return(identical(operador, "character"))
  switch(
    operador,
    "(" = , "[" = .expresion_de_texto(argumentos[[1L]], columna_de_texto),
    "c" = any(vapply(argumentos, .expresion_de_texto, logical(1L),
                     columna_de_texto = columna_de_texto)),
    "character" = TRUE,
    FALSE
  )
}

.ordena_texto <- function(expresion, columna_de_texto) {
  if (!is.call(expresion)) return(FALSE)
  expresion <- .llamada_plana(expresion)
  argumentos <- as.list(expresion)[-1L]
  if (is.symbol(expresion[[1L]]) &&
      as.character(expresion[[1L]]) %in% c("<", "<=", ">", ">=") &&
      length(argumentos) == 2L &&
      (.expresion_de_texto(argumentos[[1L]], columna_de_texto) ||
         .expresion_de_texto(argumentos[[2L]], columna_de_texto))) {
    return(TRUE)
  }
  any(vapply(argumentos, function(a) {
    is.call(a) && .ordena_texto(a, columna_de_texto)
  }, logical(1L)))
}

# Que regla interpretada mide la instancia, leido sin despachar. Es lo mismo que
# decide `.especificacion_metrica()` para estos dos nodos, que son los unicos
# que la huella necesita; aquella mira tambien `length()` de los limites de
# otras metricas, y sobre un valor de una clase de quien llama eso ejecutaba su
# metodo al sellar.
.nodo_para_huella <- function(instancia) {
  metodo <- .subset2(instancia, "metodo")
  if (!.metodo_del_paquete(metodo)) return(NA_character_)
  nombre <- as.character(.subset2(.subset2(instancia, "declaracion"), "nombre"))
  configuracion <- unclass(.subset2(instancia, "configuracion"))
  if (identical(nombre, "ValoresPosiblesPorComprension") &&
      identical(metodo, .metodo_valores_comprension) &&
      is.null(configuracion[["predicado"]])) {
    return("en_rango")
  }
  if (identical(nombre, "ReglaIntegridadIntraEntidad") &&
      identical(metodo, .metodo_regla_intra) &&
      inherits(configuracion[["regla"]], "formula")) {
    return("predicado_fila")
  }
  NA_character_
}

.depende_orden_texto <- function(instancia, lector) {
  nodo <- .nodo_para_huella(instancia)
  if (is.na(nodo)) return(FALSE)
  configuracion <- unclass(.subset2(instancia, "configuracion"))
  entidad <- as.character(unlist(.subset2(instancia, "entidad")))[1L]
  atributos <- as.character(unlist(.subset2(instancia, "atributos")))
  if (identical(nodo, "en_rango")) {
    # El rango pasa la columna por `.texto_analizable()`, que lleva un factor a
    # su texto: aca un factor si ordena texto.
    clase <- lector$clase(entidad, atributos[1L])
    texto <- function(x) is.character(x) || inherits(x, "factor")
    return(texto(configuracion[["minimo"]]) || texto(configuracion[["maximo"]]) ||
             any(clase %in% c("texto", "factor")))
  }
  columna_de_texto <- function(nombre) {
    if (is.na(.indice_nombre(nombre, atributos))) return(FALSE)
    identical(lector$clase(entidad, nombre), "texto")
  }
  .ordena_texto(.subset2(configuracion[["regla"]], 2L), columna_de_texto)
}

# --- Forma canonica de una definicion ----------------------------------------
#
# Lo que se resume es la definicion, no su escritura. Tres cosas que no cambian
# el significado y movian la huella:
#
#   - `(` y un `{` de una sola expresion son la identidad: `~ (x > 1)` mide lo
#     mismo que `~ x > 1`;
#   - un entero y el doble del mismo valor: `0L` y `0` son el mismo limite para
#     cualquier comparacion de R;
#   - el orden de `proteger` en `normalizacion()`: es un conjunto de grafemas,
#     y el paquete ya lo trata asi en la deriva de perfiles
#     (`.configuracion_normalizacion_perfil()`).
#
# Y una que si lo cambia y la huella no veia: lo que una funcion del usuario lee
# de su entorno. `function(d) d$x > u` con `u = 1` y con `u = 2` tenia una sola
# huella y median distinto; la deriva publicaba el cambio como un cambio de los
# datos. Ahora entra el valor de cada variable libre que el cuerpo nombra y que se
# resuelve antes de llegar a una biblioteca (la base, lupa, cualquier paquete):
# lo que es de R lo identifica su nombre; lo de un paquete externo, su nombre,
# su version y el cuerpo de la funcion que se usa.
#
# Y sellar no ejecuta nada de quien llama. No evalua una promesa ni dispara un
# enlace activo, y no despacha: un objeto con clase se describe por `unclass()`,
# con su clase como dato, porque un `length()`, `[[`, `$`, `as.character()` o
# `format()` propios corrian al sellar -y uno que fallaba hacia abortar
# `medir()`-. Lo que no se puede describir asi deja la metrica
# `no_certificable`: comparar dice que no se sabe, no calla.
#
# Y la huella dice cuanto ve (`certificacion`). Lo que es dato -la declaracion,
# los parametros, la formula de `ReglaIntegridadIntraEntidad`, el referencial,
# una formula de `aplicabilidad` que solo usa funciones que no leen nada mas que
# sus argumentos- se describe entero: la huella es `completa`. Una funcion de
# quien llama no: certificar todo lo que un codigo de R puede leer por analisis
# estatico no se puede completar -`attr(f, "u")`, `body("fn")`, `local()`,
# `assign()`, `...elt()`, una funcion por su nombre armado como texto-. La huella
# ve su texto y los valores que nombra, y es `parcial`: si cambia, la definicion
# cambio; si no cambia, no se afirma que no cambio. Lo mismo un argumento sin
# evaluar (la huella ve su codigo, no su valor) y un valor demasiado grande para
# recorrerlo (ve su tipo, su largo y su clase).

.TOPE_PROFUNDIDAD_HUELLA <- 8L

# Lo mas grande que la huella recorre: un vector de mas elementos -o una lista de
# mas elementos- se describe por su tipo, su largo y su clase, sin leer sus
# datos, y la huella es parcial. Resumir un millon de textos cuesta 0,44 s y diez
# millones 3,75 s; y `1:1e9` en el entorno de una regla ocupa 680 bytes, pero
# leerlo para resumirlo pedia 8 GB y el sistema mataba a R. Medido. El
# referencial se mide entero en cada corrida, y su tope es mas alto.
.TOPE_VECTOR_HUELLA <- 1000000L
.TOPE_LISTA_HUELLA <- 100000L
.TOPE_VECTOR_REFERENCIAL <- 10000000L

.es_argumento_vacio <- function(x) identical(x, quote(expr = ))

# Una constante en su forma canonica: el entero como doble (`0L` y `0` son el
# mismo limite), pero el cero con su signo -`1 / u` distingue `0` de `-0`- y un
# complejo por sus dos partes -su escritura junta todo complejo con un `NaN`-.
.constante_canonica <- function(x) {
  if (is.complex(x) && !is.object(x)) {
    return(list(complejo = list(
      real = .constante_canonica(Re(x)), imaginaria = .constante_canonica(Im(x))
    )))
  }
  if (is.integer(x) && !is.object(x)) {
    atributos <- attributes(x)
    x <- as.double(x)
    attributes(x) <- atributos
  }
  if (is.double(x) && !is.object(x)) {
    negativos <- which(!is.na(x) & x == 0 & 1 / x < 0)
    if (length(negativos)) {
      return(list(valores = x, ceros_negativos = as.double(negativos)))
    }
  }
  x
}

.contexto_huella <- function(memoria = .memoria_huella()) {
  contexto <- new.env(parent = emptyenv())
  contexto$motivos <- character()
  contexto$parciales <- character()
  contexto$notas <- character()
  contexto$visitados <- list()
  contexto$columnas <- character()
  contexto$memoria <- memoria
  contexto
}

# Lo que la huella no ve entero: la deja `parcial`, con el motivo.
.parcial <- function(contexto, motivo) {
  contexto$parciales <- c(contexto$parciales, motivo)
  invisible(NULL)
}

# Las clases que la huella describe como dato: las de base que el paquete lee
# como valores -fechas, instantes, duraciones, factores, tablas-, `integer64`
# (el paquete lo ordena y lo compara como dato: `.clave_de_orden()`) y los
# objetos del paquete que viajan en una definicion. Un valor de cualquier otra
# clase, en cualquier nivel, deja la huella parcial: la metrica lo lee con sus
# metodos -`as.character()`, `$`, `[`-, y redefinir uno cambia lo que mide sin
# cambiar el valor que la huella ve. Medido: `as.character()` de una clase
# propia en `valores_nulos` cambiaba el resultado de `NoNulo` con la misma huella
# completa.
.CLASES_DE_DATO <- c(
  "factor", "ordered", "Date", "POSIXct", "POSIXlt", "POSIXt", "difftime",
  "data.frame", "list", "integer64", "formula", "referencial",
  "normalizacion_lupa", "normalizacion_resuelta_lupa", "vigencia_datos",
  "escala_medicion"
)

# Y las clases de dato, o las del paquete -una metrica, un modelo-, pueden tener
# un metodo escrito por quien llama: un `$.referencial` en el entorno global
# cambiaba la tabla que se compara. Un metodo escrito por quien llama es una
# funcion `generica.clase` en el camino de busqueda fuera de los paquetes, o una
# registrada con `registerS3method()` cuyo entorno no es una biblioteca.
.revisar_clases <- function(clases, contexto, de_dato = TRUE) {
  clases <- unique(as.character(clases))
  if (!length(clases)) return(invisible(NULL))
  if (isTRUE(de_dato)) {
    propias <- setdiff(clases, .CLASES_DE_DATO)
    if (length(propias)) {
      .parcial(contexto, paste0(
        "valor con clase propia: su m\u00e9todo puede cambiar lo que mide (",
        paste0("`", propias, "`", collapse = ", "), ")"
      ))
    }
  }
  con_metodo <- clases[vapply(clases, .clase_con_metodo_de_quien_llama,
                               logical(1L), contexto = contexto)]
  if (length(con_metodo)) {
    .parcial(contexto, paste0(
      "valor de clase ", paste0("`", con_metodo, "`", collapse = ", "),
      " con un m\u00e9todo escrito por quien llama: puede cambiar lo que mide"
    ))
  }
  invisible(NULL)
}

# Si quien llama escribio un metodo S3 para la clase, una vez por clase y
# medicion. Se buscan los nombres que terminan en `.clase`, y no se ejecuta ni
# se fuerza nada: en el camino de busqueda fuera de los paquetes -el entorno
# global, lo adjuntado con `attach()`- y, para las clases del paquete, tambien
# en la tabla de metodos de cada espacio de nombres cargado, donde cada entrada
# se lee con `substitute()`, que no evalua una promesa (una registracion
# diferida de un paquete). En el entorno global `substitute()` no lee, y alli se
# usa `get()`. Las tablas no se miran para las clases de base ni `integer64`:
# sus metodos son de R y de los paquetes, y reemplazar uno en su tabla es
# cambiar el paquete -como simula `test-integer64-bit64-anterior.R`-, no
# escribir la definicion.
.CLASES_DEL_PAQUETE_SELLO <- c(
  "referencial", "normalizacion_lupa", "normalizacion_resuelta_lupa",
  "vigencia_datos", "escala_medicion", "metrica_instanciada",
  "metrica_especifica", "metrica_generica", "modelo_calidad"
)

.clase_con_metodo_de_quien_llama <- function(clase, contexto = NULL) {
  memoria <- if (!is.null(contexto)) contexto$memoria
  if (!is.null(memoria) && !is.null(memoria$metodos[[clase]])) {
    return(memoria$metodos[[clase]])
  }
  sufijo <- paste0(".", clase)
  es_de_quien_llama <- function(nombre, entorno) {
    if (bindingIsActive(nombre, entorno)) return(FALSE)
    valor <- if (identical(entorno, globalenv())) {
      get(nombre, envir = entorno, inherits = FALSE)
    } else {
      do.call(substitute, list(as.name(nombre), entorno))
    }
    is.function(valor) && !is.primitive(valor) &&
      is.environment(environment(valor)) &&
      !.entorno_de_biblioteca(environment(valor))
  }
  entornos <- Filter(
    Negate(.entorno_de_biblioteca), lapply(seq_along(search()), pos.to.env)
  )
  tablas <- if (clase %in% .CLASES_DEL_PAQUETE_SELLO) {
    lapply(loadedNamespaces(), function(paquete) {
      tryCatch(
        get0(".__S3MethodsTable__.", envir = asNamespace(paquete), inherits = FALSE),
        error = function(e) NULL
      )
    })
  }
  propio <- FALSE
  for (entorno in c(entornos, Filter(is.environment, tablas))) {
    nombres <- ls(entorno, all.names = TRUE)
    for (nombre in nombres[endsWith(nombres, sufijo)]) {
      if (es_de_quien_llama(nombre, entorno)) {
        propio <- TRUE
        break
      }
    }
    if (propio) break
  }
  if (!is.null(memoria)) memoria$metodos[[clase]] <- propio
  propio
}

# Una copia propia de un vector sin clase, hecha con una sola lectura. Un vector
# ALTREP corre su propio codigo en cada lectura: describirlo elemento por
# elemento -ordenar un conjunto, quitar repetidos, resumirlo- le pedia 196.976
# lecturas a uno de 20.000 elementos; copiarlo antes le pide una. Medido. Un
# vector comun cuesta una copia, por debajo del tope.
.copia_propia <- function(x) {
  if (!is.atomic(x) || is.object(x) || !length(x)) return(x)
  copia <- x
  copia[1L] <- copia[1L]
  copia
}

.MOTIVO_FUNCION_USUARIO <- paste(
  "la definici\u00f3n incluye una funci\u00f3n de quien llama: la huella ve su texto",
  "y los valores que nombra, no lo que lee de otra forma"
)

# Una expresion de R como texto, en notacion prefija: `>`(`x`,double[1]). No es
# la de `deparse()`: aca cada arbol tiene un texto y cada texto un arbol, sin
# depender de como `deparse()` ponga parentesis o corte lineas. Una constante
# con clase que el codigo trae adentro se describe como cualquier valor, sin
# despachar.
.texto_lenguaje <- function(expresion, contexto = NULL) {
  if (.es_argumento_vacio(expresion)) return("<vacio>")
  expresion <- .llamada_plana(expresion)
  if (is.symbol(expresion)) {
    return(paste0("`", .clave_bytes(as.character(expresion)), "`"))
  }
  if (is.pairlist(expresion) && !is.null(expresion)) {
    return(.texto_formales(expresion, contexto))
  }
  if (is.call(expresion)) {
    cabeza <- expresion[[1L]]
    argumentos <- as.list(expresion)[-1L]
    if (is.symbol(cabeza)) {
      nombre <- as.character(cabeza)
      if (nombre %in% c("(", "{") && length(argumentos) == 1L) {
        return(.texto_lenguaje(argumentos[[1L]], contexto))
      }
      # Una funcion escrita adentro de otra trae su `srcref` como cuarto
      # elemento: es la posicion en el archivo, no la definicion.
      if (identical(nombre, "function")) {
        argumentos <- argumentos[seq_len(min(2L, length(argumentos)))]
      }
    }
    etiquetas <- names(argumentos)
    if (is.null(etiquetas)) etiquetas <- rep("", length(argumentos))
    piezas <- vapply(seq_along(argumentos), function(i) {
      valor <- .texto_lenguaje(argumentos[[i]], contexto)
      if (nzchar(etiquetas[[i]])) {
        paste0(.clave_bytes(etiquetas[[i]]), "=", valor)
      } else {
        valor
      }
    }, character(1L))
    return(paste0(
      .texto_lenguaje(cabeza, contexto), "(", paste(piezas, collapse = ","), ")"
    ))
  }
  if (is.expression(expresion)) {
    return(paste0("expression(", paste(vapply(
      seq_along(expresion), function(i) .texto_lenguaje(expresion[[i]], contexto),
      character(1L)
    ), collapse = ","), ")"))
  }
  if (is.null(contexto)) contexto <- .contexto_huella()
  .texto_configuracion_calidad(.canonica_definicion(expresion, contexto))
}

.texto_formales <- function(formales, contexto = NULL) {
  if (!length(formales)) return("formales()")
  nombres <- names(formales)
  piezas <- vapply(seq_along(formales), function(i) {
    paste0(
      .clave_bytes(nombres[[i]]),
      if (.es_argumento_vacio(formales[[i]])) "" else
        paste0("=", .texto_lenguaje(formales[[i]], contexto))
    )
  }, character(1L))
  paste0("formales(", paste(piezas, collapse = ","), ")")
}

# Los nombres que se asignan adentro de un cuerpo -y no en una funcion anidada,
# que tiene los suyos-: son locales, no se buscan en el entorno.
.asignados_localmente <- function(expresion) {
  if (!is.call(expresion)) return(character())
  expresion <- .llamada_plana(expresion)
  cabeza <- expresion[[1L]]
  argumentos <- as.list(expresion)[-1L]
  propios <- character()
  if (is.symbol(cabeza)) {
    nombre <- as.character(cabeza)
    if (identical(nombre, "function")) return(character())
    if (nombre %in% c("<-", "=") && length(argumentos) &&
        is.symbol(argumentos[[1L]])) {
      propios <- as.character(argumentos[[1L]])
    }
    if (identical(nombre, "for") && length(argumentos) &&
        is.symbol(argumentos[[1L]])) {
      propios <- as.character(argumentos[[1L]])
    }
  }
  c(propios, unlist(lapply(argumentos, function(a) {
    if (is.call(a)) .asignados_localmente(a) else character()
  }), use.names = FALSE))
}

.es_punto_numerado <- function(nombre) {
  grepl("^[.][.][0-9]+$", nombre, useBytes = TRUE)
}

# Los simbolos que una expresion lee de afuera: no los ligados (formales y
# locales), no el campo de `$` y `@`, no lo calificado con `::` y `:::` -es de
# una biblioteca, y lo cuenta el inventario-, ni lo que va adentro de una
# formula o de `quote()`, que no se evalua ahi sino donde se use. `..1` es de
# los `...` propios si la funcion los tiene.
.simbolos_usados <- function(expresion, ligados) {
  if (.es_argumento_vacio(expresion)) return(character())
  if (is.symbol(expresion)) {
    nombre <- as.character(expresion)
    if (!nzchar(nombre) || nombre %in% ligados) return(character())
    if (.es_punto_numerado(nombre) && "..." %in% ligados) return(character())
    return(nombre)
  }
  if (!is.call(expresion)) return(character())
  expresion <- .llamada_plana(expresion)
  cabeza <- expresion[[1L]]
  argumentos <- as.list(expresion)[-1L]
  if (is.symbol(cabeza)) {
    nombre <- as.character(cabeza)
    if (nombre %in% c("::", ":::", "~", "quote")) return(character())
    if (nombre %in% c("$", "@")) {
      if (!length(argumentos)) return(character())
      return(.simbolos_usados(argumentos[[1L]], ligados))
    }
    if (identical(nombre, "function")) {
      formales <- if (length(argumentos)) argumentos[[1L]] else NULL
      propios <- c(ligados, names(formales))
      defectos <- unlist(lapply(seq_along(formales), function(i) {
        .simbolos_usados(formales[[i]], propios)
      }), use.names = FALSE)
      cuerpo <- if (length(argumentos) >= 2L) {
        .simbolos_libres_cuerpo(argumentos[[2L]], propios)
      }
      return(unique(c(defectos, cuerpo)))
    }
  }
  unique(c(
    .simbolos_usados(cabeza, ligados),
    unlist(lapply(seq_along(argumentos), function(i) {
      .simbolos_usados(argumentos[[i]], ligados)
    }), use.names = FALSE)
  ))
}

.simbolos_libres_cuerpo <- function(cuerpo, ligados) {
  .simbolos_usados(cuerpo, unique(c(ligados, .asignados_localmente(cuerpo))))
}

# Un entorno de biblioteca: la base, un espacio de nombres -el de lupa o el de
# cualquier paquete- o un paquete adjunto. Lo que se resuelve ahi es codigo de R
# o de un paquete.
.entorno_de_biblioteca <- function(entorno) {
  identical(entorno, emptyenv()) || identical(entorno, baseenv()) ||
    identical(entorno, .BaseNamespaceEnv) || isNamespace(entorno) ||
    startsWith(environmentName(entorno), "package:") ||
    startsWith(environmentName(entorno), "tools:")
}

# Los paquetes de R mismo: su version es la de R. Lo que se resuelve en ellos, o
# en lupa, se identifica por su nombre; lo de cualquier otro paquete, por su
# nombre, su version y el cuerpo de la funcion que se usa.
.PAQUETES_DE_R <- c(
  "base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"
)

.paquete_de_entorno <- function(entorno) {
  if (identical(entorno, baseenv()) || identical(entorno, .BaseNamespaceEnv)) {
    return("base")
  }
  if (isNamespace(entorno)) return(getNamespaceName(entorno)[[1L]])
  nombre <- environmentName(entorno)
  if (startsWith(nombre, "package:")) return(substring(nombre, 9L))
  NA_character_
}

# La version de un paquete externo, sin cargarlo: la del espacio de nombres si
# ya esta cargado -es la que corre-, y si no la de su DESCRIPTION.
.version_de_paquete <- function(paquete) {
  if (paquete %in% loadedNamespaces()) {
    return(as.character(getNamespaceVersion(paquete))[[1L]])
  }
  tryCatch(
    as.character(utils::packageVersion(paquete)),
    error = function(e) "no instalado"
  )
}

# El paquete, su version y el cuerpo de cada funcion que la regla usa de el. El
# numero de version no alcanza: un paquete reinstalado o cargado con `pkgload`
# cambia su codigo sin cambiarlo. El cuerpo es el de esa funcion -no se recorre
# el paquete-, y se lee solo si el paquete ya esta cargado: sellar no lo carga.
.paquetes_externos <- function(usos, contexto) {
  if (!length(usos)) return(NULL)
  paquetes <- vapply(strsplit(usos, "\034", fixed = TRUE), `[[`, character(1L), 1L)
  funciones <- vapply(strsplit(usos, "\034", fixed = TRUE), function(p) {
    if (length(p) >= 2L) p[[2L]] else NA_character_
  }, character(1L))
  externos <- !is.na(paquetes) & nzchar(paquetes) &
    !paquetes %in% c(.PAQUETES_DE_R, .nombre_paquete())
  if (!any(externos)) return(NULL)
  paquetes <- paquetes[externos]
  funciones <- funciones[externos]
  memoria <- contexto$memoria
  resultado <- list()
  for (paquete in .sort_por_bytes(unique(paquetes))) {
    if (is.null(memoria$versiones[[paquete]])) {
      memoria$versiones[[paquete]] <- .version_de_paquete(paquete)
    }
    propias <- funciones[paquetes == paquete]
    propias <- .sort_por_bytes(unique(propias[!is.na(propias)]))
    cuerpos <- NULL
    if (paquete %in% loadedNamespaces()) {
      espacio <- asNamespace(paquete)
      if (exists(".__DEVTOOLS__", envir = espacio, inherits = FALSE)) {
        contexto$notas <- c(contexto$notas, paste0(
          "el paquete `", paquete, "` est\u00e1 cargado con pkgload: su ",
          "versi\u00f3n no fija su c\u00f3digo, y la huella lleva el cuerpo de las ",
          "funciones que la regla usa de \u00e9l"
        ))
      }
      cuerpos <- lapply(propias, function(nombre) {
        clave <- paste(paquete, nombre, sep = "::")
        if (is.null(memoria$cuerpos[[clave]])) {
          funcion <- tryCatch(
            get0(nombre, envir = espacio, inherits = FALSE),
            error = function(e) NULL
          )
          memoria$cuerpos[[clave]] <- if (is.function(funcion) && !is.primitive(funcion)) {
            .md5_textos(paste0(
              .texto_formales(formals(funcion)), ",", .texto_lenguaje(body(funcion))
            ))[[1L]]
          } else {
            "sin cuerpo"
          }
        }
        memoria$cuerpos[[clave]]
      })
      names(cuerpos) <- propias
    }
    resultado[[paquete]] <- list(
      version = memoria$versiones[[paquete]], funciones = cuerpos
    )
  }
  resultado
}

# Lo que una regla lee sin nombrarlo en su texto: una variable por su nombre
# escrito, un entorno entero, el marco de quien llama, una opcion o una variable
# del sistema, codigo armado como texto o leido de un archivo, o el metodo que
# elige el despacho -puede ser uno de quien llama, para cualquier clase-. La
# huella no puede certificar lo que leen, porque no lo ve: una regla que las usa
# -llamandolas, pasandolas a otra funcion o con otro nombre- queda
# `no_certificable`, y comparar lo dice. Es la lista entera; una funcion nueva de
# esta clase se agrega aca.
.LECTURAS_DINAMICAS <- c(
  "get", "get0", "mget", "dynGet", "exists", "eval", "evalq", "eval.parent",
  "parse", "str2lang", "str2expression", "getOption", "options",
  "Sys.getenv", "sys.function", "sys.call", "sys.frame", "sys.frames",
  "parent.frame", "globalenv", "environment", "as.environment", "parent.env",
  "topenv", "ls", "objects", "source", "sys.source", "attach", "UseMethod",
  "NextMethod", "standardGeneric", "getAnywhere", "getFromNamespace"
)

# Los simbolos que, nombrados, ya son una lectura del entorno.
.ENTORNOS_NOMBRADOS <- c(".GlobalEnv", ".BaseNamespaceEnv")

# Las funciones de la lista como objetos: una variable libre cuyo valor es una
# de ellas -`f <- get`- es la misma lectura con otro nombre.
.funciones_dinamicas <- function() {
  if (is.null(.ESTADO_SELLO$dinamicas)) {
    funciones <- lapply(.LECTURAS_DINAMICAS, function(nombre) {
      if (exists(nombre, envir = baseenv(), inherits = FALSE)) {
        return(get(nombre, envir = baseenv(), inherits = FALSE))
      }
      tryCatch(getExportedValue("utils", nombre), error = function(e) NULL)
    })
    .ESTADO_SELLO$dinamicas <- Filter(Negate(is.null), funciones)
  }
  .ESTADO_SELLO$dinamicas
}

.es_funcion_dinamica <- function(valor) {
  is.function(valor) &&
    any(vapply(.funciones_dinamicas(), identical, logical(1L), valor))
}

# El argumento con que cada funcion de orden superior recibe la funcion que
# aplica. `sapply(x, "fn")` nombra `fn` como texto, que es como
# `do.call("fn")`. Se empareja con la definicion de la base (`match.call()`
# sobre la llamada escrita: no ejecuta nada), asi que vale escrito por
# posicion, por nombre o con el nombre abreviado.
.FUNCIONALES <- c(
  lapply = "FUN", sapply = "FUN", vapply = "FUN", mapply = "FUN", Map = "f",
  Reduce = "f", Filter = "f", Position = "f", Find = "f", rapply = "f",
  apply = "FUN", tapply = "FUN", outer = "FUN", do.call = "what",
  match.fun = "FUN"
)

.argumento_funcional <- function(nombre, expresion) {
  if (!nombre %in% names(.FUNCIONALES)) return(NULL)
  definicion <- get0(nombre, envir = baseenv(), inherits = FALSE)
  if (!is.function(definicion) || is.primitive(definicion)) return(NULL)
  # Un `...` escrito en la llamada pasa lo que llega a quien la contiene: no
  # cambia cual es la funcion, y `match.call()` no lo empareja sin un marco.
  piezas <- as.list(expresion)
  piezas <- piezas[!vapply(piezas, identical, logical(1L), as.name("..."))]
  emparejada <- tryCatch(
    match.call(definicion, as.call(piezas), expand.dots = FALSE,
               envir = emptyenv()),
    error = function(e) NULL
  )
  if (is.null(emparejada)) return(NULL)
  as.list(emparejada)[[.FUNCIONALES[[nombre]]]]
}

# El inventario de las llamadas de un cuerpo: las lecturas dinamicas, los
# paquetes que nombra con `::` o `:::` (y que funcion de cada uno), y las
# funciones que una funcion de orden superior recibe por su nombre escrito
# (`sapply(x, "f")`, `do.call("f", ...)`: se resuelven como si el cuerpo
# nombrara a `f`) o en una variable (si esa variable guarda un texto, la funcion
# se elige al correr: es una lectura dinamica). Lo que va adentro de `quote()` o
# de una formula no corre aca.
.inventario_llamadas <- function(expresion, acumulado) {
  if (!is.call(expresion)) return(invisible(acumulado))
  expresion <- .llamada_plana(expresion)
  cabeza <- expresion[[1L]]
  argumentos <- as.list(expresion)[-1L]
  calificada <- function(llamada) {
    if (!is.call(llamada)) return(FALSE)
    llamada <- .llamada_plana(llamada)
    length(llamada) == 3L && is.symbol(llamada[[1L]]) &&
      as.character(llamada[[1L]]) %in% c("::", ":::")
  }
  anotar_calificada <- function(llamada) {
    llamada <- .llamada_plana(llamada)
    paquete <- as.character(llamada[[2L]])
    funcion <- as.character(llamada[[3L]])
    acumulado$paquetes <- c(acumulado$paquetes, paste(paquete, funcion, sep = "\034"))
    if (paquete %in% .PAQUETES_DE_R && funcion %in% .LECTURAS_DINAMICAS) {
      acumulado$dinamicas <- c(acumulado$dinamicas, funcion)
    }
  }
  if (calificada(cabeza)) {
    anotar_calificada(cabeza)
  } else if (is.symbol(cabeza)) {
    nombre <- as.character(cabeza)
    if (nombre %in% c("::", ":::")) {
      if (calificada(expresion)) anotar_calificada(expresion)
      return(invisible(acumulado))
    }
    if (nombre %in% c("quote", "~")) return(invisible(acumulado))
    if (nombre %in% .LECTURAS_DINAMICAS) {
      acumulado$dinamicas <- c(acumulado$dinamicas, nombre)
    }
    objetivo <- .argumento_funcional(nombre, expresion)
    if (!is.null(objetivo) || nombre %in% c("do.call", "match.fun")) {
      if (is.character(objetivo) && length(objetivo) == 1L && !is.na(objetivo)) {
        acumulado$por_texto <- c(acumulado$por_texto, objetivo)
        if (objetivo %in% .LECTURAS_DINAMICAS) {
          acumulado$dinamicas <- c(acumulado$dinamicas, objetivo)
        }
      } else if (is.symbol(objetivo)) {
        acumulado$objetivos <- c(acumulado$objetivos, as.character(objetivo))
      } else if (nombre %in% c("do.call", "match.fun") && !(is.call(objetivo) && (
        identical(.llamada_plana(objetivo)[[1L]], as.name("function")) ||
          calificada(objetivo)
      ))) {
        acumulado$dinamicas <- c(acumulado$dinamicas, nombre)
      }
    }
  } else {
    .inventario_llamadas(cabeza, acumulado)
  }
  for (i in seq_along(argumentos)) {
    if (is.call(argumentos[[i]])) .inventario_llamadas(argumentos[[i]], acumulado)
  }
  invisible(acumulado)
}

.inventario_de <- function(expresiones) {
  acumulado <- new.env(parent = emptyenv())
  acumulado$dinamicas <- character()
  acumulado$paquetes <- character()
  acumulado$por_texto <- character()
  acumulado$objetivos <- character()
  for (i in seq_along(expresiones)) {
    if (is.call(expresiones[[i]])) .inventario_llamadas(expresiones[[i]], acumulado)
  }
  acumulado
}

# Si lo que `substitute()` devuelve de una variable puede ser una promesa sin
# evaluar. `substitute()` mira el entorno sin evaluar nada: de una variable
# comun da su valor, y de una promesa -el argumento de una fabrica de funciones,
# un `delayedAssign()`- da su expresion, este evaluada o no. Si lo que da no es
# codigo, es el valor (o una promesa de una constante, que vale esa constante);
# si es codigo, no se puede saber sin evaluar si es una promesa: medido sobre
# `function(u) { force(u); function() u }` llamada con `2` (da `2`), con `1 + 1`
# (da `1 + 1`, ya evaluada) y con `w` (da `w`, sin evaluar). Entonces la huella
# describe ese codigo, y es parcial: `crear(c("a", "b"))` y `crear(c("a", "c"))`
# tienen huellas distintas, pero `crear(umbral)` tiene la misma con cualquier
# `umbral`. Evaluarlo, aun siendo solo constantes, daba otro valor si quien
# llama habia redefinido `letters` o `[` en un `local()`.
.parece_codigo <- function(x) {
  is.symbol(x) || is.expression(x) || (is.call(x) && !inherits(x, "formula"))
}

# Lee una variable libre sin ejecutar nada. No dispara un enlace activo
# (`bindingIsActive()`) ni evalua una promesa: fuera del entorno global lee con
# `substitute()`, y de una promesa devuelve su codigo. En el entorno global
# `substitute()` no lee -devuelve el simbolo-, y alli se lee con `get()`; una
# promesa ahi solo la crea `delayedAssign()` a mano, y se evaluaria (es el
# limite escrito en `?medir`). Lo que se resuelve en una biblioteca no se lee: se dice de que
# paquete es y en que entorno esta.
.resolver_libre <- function(nombre, entorno, memoria = NULL) {
  while (is.environment(entorno) && !identical(entorno, emptyenv())) {
    # Despues del entorno global viene el camino de busqueda, que es el mismo
    # para todas las reglas: lo que se resuelve ahi se busca una vez por
    # medicion. Doscientas reglas que nombran `[[`, `>` e `is.na` recorrian
    # cada una todos los paquetes adjuntos.
    if (!is.null(memoria) && identical(entorno, parent.env(globalenv()))) {
      if (is.null(memoria$camino[[nombre]])) {
        memoria$camino[[nombre]] <- .resolver_libre(nombre, entorno)
      }
      return(memoria$camino[[nombre]])
    }
    if (.entorno_de_biblioteca(entorno)) {
      if (exists(nombre, envir = entorno, inherits = FALSE)) {
        return(list(estado = "biblioteca", paquete = .paquete_de_entorno(entorno)))
      }
      entorno <- parent.env(entorno)
      next
    }
    if (exists(nombre, envir = entorno, inherits = FALSE)) {
      if (bindingIsActive(nombre, entorno)) {
        return(list(estado = "no_certificable", motivo = "un enlace activo"))
      }
      if (identical(entorno, globalenv())) {
        leido <- tryCatch(
          list(valor = get(nombre, envir = entorno, inherits = FALSE)),
          error = function(e) NULL
        )
        if (is.null(leido)) {
          return(list(
            estado = "no_certificable", motivo = "un valor que no se pudo leer"
          ))
        }
        return(list(estado = "valor", valor = leido$valor))
      }
      codigo <- do.call(substitute, list(as.name(nombre), entorno))
      if (.parece_codigo(codigo)) return(list(estado = "codigo", codigo = codigo))
      return(list(estado = "valor", valor = codigo))
    }
    entorno <- parent.env(entorno)
  }
  list(estado = "biblioteca", paquete = NA_character_)
}

.no_certificable <- function(contexto, motivo) {
  contexto$motivos <- c(contexto$motivos, motivo)
  "<no certificable>"
}

.valores_libres <- function(nombres, entorno, contexto, profundidad,
                            excluir = character(), objetivos = character(),
                            declaradas = character()) {
  nombres <- unique(nombres[!nombres %in% excluir])
  if (!length(nombres)) return(NULL)
  nombres <- nombres[.orden_por_bytes(nombres)]
  valores <- list()
  for (nombre in nombres) {
    if (nombre %in% .ENTORNOS_NOMBRADOS) {
      .no_certificable(contexto, paste0("lectura din\u00e1mica: `", nombre, "`"))
      next
    }
    # `..1` y `...` sin `...` propios leen los de una fabrica: son promesas.
    if (identical(nombre, "...") || .es_punto_numerado(nombre)) {
      .no_certificable(contexto, paste0(
        "`", nombre, "` lee los argumentos `...` de una f\u00e1brica de funciones, ",
        "que son promesas"
      ))
      next
    }
    resuelto <- .resolver_libre(nombre, entorno, contexto$memoria)
    if (identical(resuelto$estado, "biblioteca")) {
      if (nombre %in% .LECTURAS_DINAMICAS &&
          resuelto$paquete %in% c(.PAQUETES_DE_R, NA_character_)) {
        # Llamada, ya la declaro el inventario; nombrada como valor
        # (`sapply(x, get)`), se declara aca.
        if (!nombre %in% declaradas) {
          .no_certificable(contexto, paste0("lectura din\u00e1mica: ", nombre, "()"))
        }
        next
      }
      if (resuelto$paquete %in% c(.PAQUETES_DE_R, .nombre_paquete(), NA_character_)) {
        next
      }
      externo <- .paquetes_externos(
        paste(resuelto$paquete, nombre, sep = "\034"), contexto
      )
      if (length(externo)) {
        valores[nombre] <- list(list(
          paquete = names(externo)[[1L]], uso = externo[[1L]]
        ))
      }
      next
    }
    if (identical(resuelto$estado, "codigo")) {
      .parcial(contexto, paste0(
        "`", nombre, "` es un argumento sin evaluar o un valor que es c\u00f3digo ",
        "(el de una f\u00e1brica de funciones, por ejemplo): la huella ve su ",
        "c\u00f3digo, no su valor"
      ))
      valores[nombre] <- list(list(
        codigo = .texto_lenguaje(resuelto$codigo, contexto)
      ))
      next
    }
    if (identical(resuelto$estado, "valor")) {
      if (.es_funcion_dinamica(resuelto$valor)) {
        .no_certificable(contexto, paste0(
          "lectura din\u00e1mica: `", nombre, "` es otro nombre de una funci\u00f3n ",
          "que lee el entorno"
        ))
        next
      }
      if (nombre %in% objetivos && is.character(resuelto$valor)) {
        .no_certificable(contexto, paste0(
          "lectura din\u00e1mica: la funci\u00f3n que se aplica viene por su nombre, ",
          "guardado en `", nombre, "`"
        ))
      }
    }
    valores[nombre] <- list(if (identical(resuelto$estado, "valor")) {
      .canonica_definicion(resuelto$valor, contexto, profundidad)
    } else {
      .no_certificable(contexto, paste0("`", nombre, "` es ", resuelto$motivo))
    })
  }
  if (length(valores)) valores else NULL
}

# Lo que el inventario de un cuerpo agrega a su definicion: las lecturas
# dinamicas lo hacen no certificable, y los paquetes externos que nombra con
# `::` entran con su version y el cuerpo de lo que usa.
.declarar_inventario <- function(inventario, contexto) {
  dinamicas <- unique(inventario$dinamicas)
  if (length(dinamicas)) {
    .no_certificable(contexto, paste0(
      "lectura din\u00e1mica: ", paste0(dinamicas, "()", collapse = ", ")
    ))
  }
  .paquetes_externos(inventario$paquetes, contexto)
}

.canonica_funcion <- function(f, contexto, profundidad, con_entorno = TRUE) {
  if (is.primitive(f)) {
    return(list(primitiva = paste(.deparse_por_omision(f), collapse = " ")))
  }
  texto <- paste0(
    "function(", .texto_formales(formals(f), contexto), ",",
    .texto_lenguaje(body(f), contexto), ")"
  )
  # Una funcion cuyo entorno es una biblioteca -una funcion de R, de lupa o de
  # un paquete- vale por su texto y, si el paquete es externo, por su version.
  # Las demas leen de su entorno, y se recorre hasta llegar a una biblioteca: no
  # alcanza con mirar `topenv()`, que da el espacio de nombres de lupa para una
  # funcion escrita en un entorno hijo suyo, y la tomaba por del paquete.
  # Los atributos de una funcion son datos que su cuerpo puede leer
  # (`attr(f, "u")`); el `srcref` es la posicion en el archivo.
  atributos <- attributes(f)
  atributos[c("srcref", "class")] <- NULL
  clase <- oldClass(f)
  propios_de_f <- c(
    if (length(clase)) list(clase = as.character(clase)),
    if (length(atributos)) {
      list(atributos = .canonica_definicion(atributos, contexto, profundidad))
    }
  )
  entorno <- environment(f)
  if (!is.environment(entorno) || .entorno_de_biblioteca(entorno)) {
    externo <- if (is.environment(entorno)) {
      .paquetes_externos(.paquete_de_entorno(entorno), contexto)
    }
    if (length(externo)) {
      .parcial(contexto, paste0(
        "una funci\u00f3n del paquete `", names(externo)[[1L]], "`: la huella ve ",
        "su versi\u00f3n y su cuerpo, no lo que lee"
      ))
    }
    return(c(
      list(funcion = texto), propios_de_f,
      if (length(externo)) list(paquete = externo)
    ))
  }
  .parcial(contexto, .MOTIVO_FUNCION_USUARIO)
  for (visitada in contexto$visitados) {
    if (identical(visitada, f)) {
      return(c(list(funcion = texto), propios_de_f, list(recursiva = TRUE)))
    }
  }
  if (profundidad >= .TOPE_PROFUNDIDAD_HUELLA) {
    .parcial(contexto, paste0(
      "una cadena de m\u00e1s de ", .TOPE_PROFUNDIDAD_HUELLA,
      " funciones que se llaman entre s\u00ed: las de m\u00e1s abajo entran por ",
      "su texto, sin lo que leen"
    ))
    return(c(list(funcion = texto), propios_de_f))
  }
  formales <- formals(f)
  inventario <- .inventario_de(c(
    lapply(seq_along(formales), function(i) {
      if (.es_argumento_vacio(formales[[i]])) NULL else formales[[i]]
    }),
    list(body(f))
  ))
  paquetes <- .declarar_inventario(inventario, contexto)
  if (!isTRUE(con_entorno)) {
    return(c(
      list(funcion = texto), propios_de_f,
      if (length(paquetes)) list(paquetes = paquetes)
    ))
  }
  contexto$visitados <- c(contexto$visitados, list(f))
  on.exit(contexto$visitados <- contexto$visitados[-length(contexto$visitados)])
  propios <- names(formales)
  libres <- c(
    unlist(lapply(seq_along(formales), function(i) {
      .simbolos_usados(formales[[i]], propios)
    }), use.names = FALSE),
    .simbolos_libres_cuerpo(body(f), propios),
    # Una funcion nombrada por texto que el cuerpo define es la local.
    setdiff(inventario$por_texto, c(propios, .asignados_localmente(body(f))))
  )
  c(
    list(funcion = texto), propios_de_f,
    if (length(paquetes)) list(paquetes = paquetes),
    list(libres = .valores_libres(
      libres, entorno, contexto, profundidad + 1L,
      objetivos = inventario$objetivos, declaradas = inventario$dinamicas
    ))
  )
}

# Una formula que se evalua con `eval()` -`aplicabilidad`, `aplicable`- busca
# cada nombre primero en la tabla y despues en su entorno: los nombres de
# columna no se buscan. La de `ReglaIntegridadIntraEntidad` no mira su entorno
# (solo atributos y literales: ver `.validar_ast_formula()`), y entra por su
# texto.
#
# Una formula evaluada con `eval()` es codigo: si llama a algo mas que a las
# funciones de `.FUNCIONES_DE_FORMULA`, la huella es parcial, como la de una
# funcion de quien llama. Entran las de base que leen solo sus argumentos y el
# entorno de la sesion que el sello registra (`.entorno_medicion()`): con la
# misma huella y el mismo entorno, miden lo mismo. `paste()` de un numero lee
# `scipen` y `OutDec`, `gsub()` las clases de caracteres, `sort()` el orden del
# locale, `weekdays()` los nombres del locale: todo eso se registra. Quedan
# afuera `format()` -lee `digits`, que no se registra-, `Sys.time()` y
# `Sys.Date()` -el reloj- y todo lo que aplica otra funcion (`sapply()`).
.FUNCIONES_DE_FORMULA <- c(
  "(", "{", "!", "&", "|", "&&", "||", "xor", "==", "!=", "<", "<=", ">", ">=",
  "+", "-", "*", "/", "^", "%%", "%/%", "%in%", ":", "c", "[", "[[", "$",
  "is.na", "is.null", "is.finite", "is.infinite", "is.nan", "isTRUE",
  "isFALSE", "nchar", "nzchar", "grepl", "startsWith", "endsWith", "toupper",
  "tolower", "trimws", "substr", "substring", "abs", "round", "floor",
  "ceiling", "trunc", "sqrt", "exp", "log", "as.numeric", "as.integer",
  "as.double", "as.character", "as.logical", "ifelse", "any", "all", "sum",
  "min", "max", "pmin", "pmax", "length", "seq_len", "seq_along", "rep",
  "match", "duplicated", "unique", "identical", "is.element",
  "paste", "paste0", "sprintf", "gsub", "sub", "regexpr", "gregexpr",
  "strsplit", "mean", "median", "sd", "var", "quantile", "range", "prod",
  "cumsum", "cumprod", "is.numeric", "is.character", "is.logical", "which",
  "sort", "order", "rev", "table", "as.Date", "as.POSIXlt", "months", "weekdays"
)

# Las funciones que una expresion llama y no estan en `.FUNCIONES_DE_FORMULA`.
# Lo que va adentro de `quote()` o de otra formula no se llama ahi.
.llamadas_fuera_de_formula <- function(expresion) {
  if (!is.call(expresion)) return(character())
  expresion <- .llamada_plana(expresion)
  cabeza <- expresion[[1L]]
  argumentos <- as.list(expresion)[-1L]
  propia <- if (is.symbol(cabeza)) {
    nombre <- as.character(cabeza)
    if (nombre %in% c("quote", "~")) return(character())
    if (nombre %in% .FUNCIONES_DE_FORMULA) character() else nombre
  } else {
    paste(.deparse_por_omision(cabeza), collapse = "")
  }
  c(propia, unlist(lapply(argumentos, function(a) {
    if (is.call(a)) .llamadas_fuera_de_formula(a) else character()
  }), use.names = FALSE))
}
.canonica_formula <- function(f, contexto, profundidad, con_entorno = TRUE) {
  .revisar_clases(oldClass(f), contexto)
  partes <- as.list(unclass(f))[-1L]
  texto <- paste0("~(", paste(vapply(seq_along(partes), function(i) {
    .texto_lenguaje(partes[[i]], contexto)
  }, character(1L)), collapse = ","), ")")
  entorno <- attr(f, ".Environment", exact = TRUE)
  if (!isTRUE(con_entorno) || !is.environment(entorno)) {
    return(list(formula = texto))
  }
  inventario <- .inventario_de(partes)
  paquetes <- .declarar_inventario(inventario, contexto)
  fuera <- unique(unlist(lapply(partes, .llamadas_fuera_de_formula), use.names = FALSE))
  if (length(fuera)) {
    .parcial(contexto, paste0(
      "una f\u00f3rmula que se eval\u00faa llama a ",
      paste0("`", fuera, "()`", collapse = ", "),
      ": la huella no ve lo que esas funciones leen"
    ))
  }
  libres <- c(
    unlist(lapply(seq_along(partes), function(i) {
      .simbolos_usados(partes[[i]], character())
    }), use.names = FALSE),
    inventario$por_texto
  )
  c(
    list(formula = texto),
    if (length(paquetes)) list(paquetes = paquetes),
    list(libres = .valores_libres(
      libres, entorno, contexto, profundidad + 1L, excluir = contexto$columnas,
      objetivos = inventario$objetivos, declaradas = inventario$dinamicas
    ))
  )
}

# Sin despachar: la lista sin clase, `proteger` como texto plano y `por_columna`
# sin la clase que traiga. Una subclase de quien llama con su `[[<-`, o un
# `proteger` con su `as.character()`, corrian al sellar.
.normalizacion_para_huella <- function(x) {
  clase <- oldClass(x)
  y <- unclass(x)
  if ("normalizacion_lupa" %in% clase && !is.null(y[["proteger"]])) {
    proteger <- .texto_plano(y[["proteger"]])
    proteger <- proteger[!duplicated(.clave_bytes(proteger))]
    y[["proteger"]] <- proteger[.orden_por_bytes(proteger)]
  }
  if ("normalizacion_resuelta_lupa" %in% clase && !is.null(y[["por_columna"]])) {
    por_columna <- unclass(y[["por_columna"]])
    nombres <- names(por_columna)
    if (length(por_columna) && !is.null(nombres)) {
      orden <- .orden_por_bytes(nombres)
      plano <- por_columna
      attributes(plano) <- NULL
      plano <- plano[orden]
      names(plano) <- nombres[orden]
      y[["por_columna"]] <- plano
    }
  }
  oldClass(y) <- clase
  y
}

# El largo de un vector sin leer sus datos ni despachar el `length()` de quien
# llama: un vector ALTREP -`1:1e9`- da su largo sin materializarse. El de un
# objeto cuya clase tiene un `length()` escrito fuera de un paquete no se pide,
# y da `NA`; los de la base y de los paquetes se usan.
.largo_sin_despacho <- function(x, contexto = NULL) {
  if (is.list(x)) return(length(unclass(x)))
  if (!is.object(x)) return(length(x))
  for (clase in as.character(oldClass(x))) {
    if (.length_de_quien_llama(clase, contexto)) return(NA_integer_)
  }
  length(x)
}

.length_de_quien_llama <- function(clase, contexto = NULL) {
  memoria <- if (!is.null(contexto)) contexto$memoria
  if (!is.null(memoria) && !is.null(memoria$largos[[clase]])) {
    return(memoria$largos[[clase]])
  }
  metodo <- tryCatch(
    utils::getS3method("length", clase, optional = TRUE),
    error = function(e) NULL
  )
  propio <- is.function(metodo) && !(is.environment(environment(metodo)) &&
                                      .entorno_de_biblioteca(environment(metodo)))
  if (!is.null(memoria)) memoria$largos[[clase]] <- propio
  propio
}

# Un valor que la huella no recorre: queda descrito por su clase, su tipo y su
# largo, y la huella es parcial.
.vector_sin_recorrer <- function(x, n, contexto, tope = .TOPE_VECTOR_HUELLA) {
  clase <- as.character(oldClass(x))
  .parcial(contexto, if (is.na(n)) {
    paste0(
      "un valor de clase `", paste(clase, collapse = "/"), "` con un `length()` ",
      "propio: la huella ve su clase y su tipo, no sus valores"
    )
  } else {
    paste0(
      "un valor de ", sprintf("%.0f", n), " elementos, m\u00e1s que los ",
      sprintf("%.0f", tope), " que la huella recorre: ve su tipo, su largo y su ",
      "clase, no sus valores"
    )
  })
  list(
    sin_recorrer = TRUE, clase = clase, tipo = typeof(x),
    largo = if (is.na(n)) NA_real_ else as.double(n)
  )
}

# Un vector atomico grande -una tabla de referencia que una regla lee de su
# entorno, un padron- se resume por bloques: el md5 de su serializacion version
# 2 sin la cabecera (que lleva la version de R que escribe), en bloques de un
# millon de elementos. Cada bloque se lleva a su forma canonica antes de
# serializarlo -el texto con la marca de `.clave_bytes()`, el entero como
# doble-, y no el vector entero: la copia nunca pasa de un bloque. El cero
# conserva su signo. Describirlo valor por valor costaba 5,2 s por medicion
# para un `data.frame` de un millon de filas en el entorno de una regla; medido.
.UMBRAL_VECTOR_GRANDE <- 10000L
.BLOQUE_VECTOR_GRANDE <- 1000000L

.huella_vector_grande <- function(valores) {
  tipo <- typeof(valores)
  n <- length(valores)
  inicios <- seq.int(1, n, by = .BLOQUE_VECTOR_GRANDE)
  crudos <- lapply(inicios, function(i) {
    bloque <- valores[i:min(n, i + .BLOQUE_VECTOR_GRANDE - 1)]
    if (is.character(bloque)) bloque <- .clave_bytes(bloque)
    if (is.integer(bloque)) bloque <- as.double(bloque)
    serializado <- serialize(bloque, NULL, version = 2L, xdr = TRUE)
    serializado[-seq_len(14L)]
  })
  list(
    vector = if (identical(tipo, "integer")) "double" else tipo, n = n,
    md5 = .md5_textos(paste(.md5_crudos(crudos), collapse = ""))[[1L]]
  )
}

# Un vector atomico sin clase ni atributos, cuyo largo ya se miro.
.atomico_canonico <- function(valores) {
  if (length(valores) >= .UMBRAL_VECTOR_GRANDE) {
    return(.huella_vector_grande(valores))
  }
  .constante_canonica(valores)
}

# Un objeto con clase, sin despachar: su clase y sus atributos como datos, y
# sus valores sin clase. Una fecha, un instante, un factor y un `integer64` se
# leen por lo que valen (`.valores_planos()`; un factor por su texto, con sus
# niveles); una tabla, por sus columnas; un contrato de vigencia, con los campos
# que quien llama no declaro marcados -como en `configuracion_modelo`-.
.objeto_para_huella <- function(x, contexto, profundidad, con_entorno) {
  clase <- as.character(oldClass(x))
  .revisar_clases(clase, contexto)
  if (inherits(x, c("normalizacion_lupa", "normalizacion_resuelta_lupa"))) {
    x <- .normalizacion_para_huella(x)
  }
  sin_declarar <- if (inherits(x, "vigencia_datos")) {
    attr(x, "campos_sin_declarar", exact = TRUE)
  }
  valores <- unclass(x)
  atributos <- attributes(valores)
  # Todo atributo entra -el huso de un instante, las unidades, `comment`, las
  # dimensiones-, porque una regla puede leerlo. Los nombres de una lista van
  # con cada elemento; los niveles de un factor y los nombres de fila de una
  # tabla, aparte.
  excluidos <- c("class", "levels", "row.names")
  if (is.list(valores)) excluidos <- c(excluidos, "names")
  atributos <- atributos[setdiff(names(atributos), excluidos)]
  extra <- list()
  if (is.list(valores)) {
    nombres <- names(valores)
    plano <- valores
    attributes(plano) <- NULL
    contenido <- lapply(seq_along(plano), function(i) {
      if (!is.null(nombres) && nombres[[i]] %in% sin_declarar) return("<sin declarar>")
      .canonica_definicion(plano[[i]], contexto, profundidad, con_entorno)
    })
    names(contenido) <- nombres
    if (inherits(x, "data.frame")) {
      # Los nombres de fila automaticos se guardan compactos, `c(NA, -n)`, y se
      # leen asi sin expandirlos.
      filas <- .row_names_info(x, 0L)
      extra$filas <- if (is.integer(filas) && length(filas) == 2L &&
                         is.na(filas[[1L]])) {
        list(automaticas = abs(filas[[2L]]))
      } else {
        .canonica_definicion(filas, contexto)
      }
    }
  } else if (inherits(x, "factor")) {
    contenido <- .atomico_canonico(.texto_de_factor(x))
    extra$niveles <- .texto_plano(attr(valores, "levels", exact = TRUE))
  } else {
    contenido <- .atomico_canonico(.valores_planos(x))
  }
  c(
    list(clase = clase, valores = contenido), extra,
    if (length(atributos)) {
      list(atributos = .canonica_definicion(atributos, contexto, profundidad))
    }
  )
}

.canonica_definicion <- function(x, contexto, profundidad = 0L,
                                 con_entorno = TRUE) {
  if (is.null(x)) return(NULL)
  # El largo se mira antes que nada, sin despachar ni leer los datos: lo que
  # pasa del tope no se recorre. Y un objeto grande que varias metricas
  # comparten -un vector, un factor, un instante, una tabla- se resume una vez
  # por medicion: `identical()` sobre el mismo objeto no lo recorre.
  grande <- FALSE
  if (!isS4(x) && is.atomic(x)) {
    n <- .largo_sin_despacho(x, contexto)
    if (is.na(n) || n > .TOPE_VECTOR_HUELLA) {
      return(.vector_sin_recorrer(x, n, contexto))
    }
    grande <- n >= .UMBRAL_VECTOR_GRANDE
  } else if (!isS4(x) && inherits(x, "data.frame")) {
    grande <- abs(.row_names_info(x, 2L)) >= .UMBRAL_VECTOR_GRANDE
  } else if (!isS4(x) && is.list(x)) {
    n <- .largo_sin_despacho(x, contexto)
    if (n > .TOPE_LISTA_HUELLA) {
      return(.vector_sin_recorrer(x, n, contexto, .TOPE_LISTA_HUELLA))
    }
  }
  if (grande && !is.null(contexto$memoria)) {
    for (visto in contexto$memoria$objetos) {
      if (identical(visto$objeto, x)) {
        contexto$motivos <- c(contexto$motivos, visto$motivos)
        contexto$parciales <- c(contexto$parciales, visto$parciales)
        return(visto$canonico)
      }
    }
  }
  antes <- length(contexto$motivos)
  antes_parciales <- length(contexto$parciales)
  canonico <- .canonica_definicion_sin_memoria(
    x, contexto, profundidad, con_entorno
  )
  if (grande && !is.null(contexto$memoria)) {
    contexto$memoria$objetos <- c(contexto$memoria$objetos, list(list(
      objeto = x, canonico = canonico,
      motivos = contexto$motivos[seq_len(length(contexto$motivos) - antes) + antes],
      parciales = contexto$parciales[
        seq_len(length(contexto$parciales) - antes_parciales) + antes_parciales
      ]
    )))
  }
  canonico
}

.canonica_definicion_sin_memoria <- function(x, contexto, profundidad,
                                             con_entorno) {
  # El orden importa: lo que no se puede describir sin ejecutar se mira antes
  # que nada que despache.
  if (isS4(x)) return(.no_certificable(contexto, "un objeto S4"))
  if (is.environment(x)) return(.no_certificable(contexto, "un entorno"))
  if (typeof(x) %in% c("externalptr", "weakref", "bytecode")) {
    return(.no_certificable(contexto, "un puntero externo"))
  }
  if (is.function(x)) {
    return(.canonica_funcion(x, contexto, profundidad, con_entorno))
  }
  if (inherits(x, "formula")) {
    return(.canonica_formula(x, contexto, profundidad, con_entorno))
  }
  if (is.language(x)) {
    # Como dato -no como codigo que se evalua-, la clase de una expresion si
    # es parte de lo que vale.
    clase <- oldClass(x)
    .revisar_clases(clase, contexto)
    lenguaje <- list(lenguaje = .texto_lenguaje(x, contexto))
    if (is.null(clase)) return(lenguaje)
    return(c(lenguaje, list(clase = as.character(clase))))
  }
  if (inherits(x, "connection")) {
    return(.no_certificable(contexto, "una conexi\u00f3n"))
  }
  if (is.object(x)) return(.objeto_para_huella(x, contexto, profundidad, con_entorno))
  if (is.list(x)) {
    # Los nombres van con cada elemento; cualquier otro atributo -las
    # dimensiones de una lista, uno propio- entra aparte, como en un vector.
    nombres <- names(x)
    atributos <- attributes(x)
    atributos$names <- NULL
    plano <- x
    attributes(plano) <- NULL
    contenido <- lapply(plano, .canonica_definicion, contexto = contexto,
                        profundidad = profundidad, con_entorno = con_entorno)
    names(contenido) <- nombres
    if (!length(atributos)) return(contenido)
    return(list(
      valores = contenido,
      atributos = .canonica_definicion(atributos, contexto, profundidad)
    ))
  }
  if (is.atomic(x)) {
    # Los nombres, las dimensiones y cualquier otro atributo son parte del
    # valor: `u[["edad"]]` lee otra cosa si `u` cambia de nombres. La
    # escritura de `configuracion_modelo` no los ve, y aca entran aparte.
    atributos <- attributes(x)
    valores <- .copia_propia(x)
    attributes(valores) <- NULL
    canonico <- .atomico_canonico(valores)
    if (is.null(atributos)) return(canonico)
    return(list(
      valores = canonico,
      atributos = .canonica_definicion(atributos, contexto, profundidad)
    ))
  }
  list(tipo = typeof(x))
}

# Las propiedades que se usan como conjunto (`.configuracion_como_conjunto()`)
# se ordenan antes de describirlas -un conjunto grande se resume por bloques, y
# el resumen depende del orden-, y sin despachar: un valor con clase se ordena
# por sus valores sin clase y se le devuelven sus atributos. Ordenar el valor de
# quien llama con `[` y `as.character()` ejecutaba sus metodos.
.ordenar_conjunto <- function(v) {
  if (!is.atomic(v)) return(v)
  n <- .largo_sin_despacho(v)
  if (is.na(n) || n > .TOPE_VECTOR_HUELLA) return(v)
  if (!is.object(v)) {
    if (!length(v)) return(v)
    # Un conjunto vale por sus valores: los nombres no entran, como no
    # entraban en `configuracion_modelo`.
    v <- .copia_propia(v)
    attributes(v) <- NULL
    v <- v[!duplicated(v)]
    return(v[.orden_por_bytes(.clave_bytes(as.character(v)), method = "radix")])
  }
  # Con clase: sus valores sin clase, ordenados, y despues sus atributos. Un
  # conjunto no tiene forma: las dimensiones y los nombres no se devuelven
  # (quitar repetidos las rompia).
  # Los atributos se toman antes de `unclass()`, para que la clase vuelva.
  atributos <- attributes(v)
  atributos[c("names", "dim", "dimnames")] <- NULL
  valores <- unclass(v)
  attributes(valores) <- NULL
  valores <- valores[!duplicated(valores)]
  clave <- if (is.character(valores)) .clave_bytes(valores) else valores
  orden <- tryCatch(.orden_por_bytes(clave), error = function(e) seq_along(valores))
  valores <- valores[orden]
  attributes(valores) <- atributos
  valores
}

.memoria_huella <- function() {
  memoria <- new.env(parent = emptyenv())
  memoria$versiones <- list()
  memoria$cuerpos <- list()
  memoria$camino <- list()
  memoria$largos <- list()
  memoria$metodos <- list()
  memoria$objetos <- list()
  memoria$partes <- list()
  memoria
}

# El texto de una parte de la definicion que varias metricas comparten -la
# declaracion de `NoNulo`, la misma configuracion- se calcula una vez por
# medicion, con lo que hubiera declarado no certificable. Describir cada parte
# en cada metrica era casi todo el costo del sello: 0,8 ms por metrica, 20 s en
# un historico de 400 corridas de 60 metricas. Medido.
.parte_memorizada <- function(contexto, tipo, clave, calcular) {
  if (is.null(tipo)) return(calcular())
  memoria <- contexto$memoria
  for (vista in memoria$partes[[tipo]]) {
    if (identical(vista$clave, clave)) {
      contexto$motivos <- c(contexto$motivos, vista$motivos)
      contexto$parciales <- c(contexto$parciales, vista$parciales)
      contexto$notas <- c(contexto$notas, vista$notas)
      return(vista$texto)
    }
  }
  delta <- function(campo, antes) {
    actual <- get(campo, envir = contexto)
    actual[seq_len(length(actual) - antes) + antes]
  }
  antes <- length(contexto$motivos)
  antes_parciales <- length(contexto$parciales)
  antes_notas <- length(contexto$notas)
  texto <- calcular()
  memoria$partes[[tipo]] <- c(
    memoria$partes[[tipo]],
    list(list(
      clave = clave, texto = texto, motivos = delta("motivos", antes),
      parciales = delta("parciales", antes_parciales),
      notas = delta("notas", antes_notas)
    ))
  )
  texto
}

# La definicion canonica de una metrica instanciada. Entra lo que decide que se
# mide y como se lee: la declaracion (`.CAMPOS_DEFINICION_METRICA`), los
# parametros con que se especializo, la entidad y los atributos -en su orden,
# porque una regla escrita como funcion y las metricas de varias columnas los
# reciben en ese orden; la formula de `ReglaIntegridadIntraEntidad` los nombra,
# y ahi el orden no cuenta (`.r_predicado_fila()` y la via SQL los toman por
# nombre)-, el referencial, el metodo cuando no es el del paquete, las reglas de
# `aplicabilidad` de sus atributos y, si la metrica ordena texto en esta
# medicion, `orden_texto = "bytes"`.
#
# Y no entra lo que no lo decide. El NOMBRE de la instancia y de la
# especializacion son etiquetas -y el sello se aparea por el nombre de la
# instancia-. El orden de los argumentos de `especializar()` no es parte de la
# definicion, ni el de los valores de una propiedad que se usa como conjunto
# (`.configuracion_como_conjunto()`). El metodo por omision de una metrica del
# paquete tampoco: su identidad viaja en el nombre de la metrica, y lo que la
# implementacion haga en cada version lo dice `version_lupa`. Un metodo propio
# -declarado al instanciar, o el de una `metrica()` escrita por quien llama- si
# entra, con lo que lee de su entorno.
#
# Devuelve el texto, el motivo si algo no se pudo certificar, y las notas.
.texto_definicion_metrica <- function(instancia, aplicabilidad = NULL,
                                      orden_texto = "idioma",
                                      lector = .lector_tablas(list()),
                                      memoria = .memoria_huella()) {
  contexto <- .contexto_huella(memoria)
  entidades <- as.character(unlist(.subset2(instancia, "entidad")))
  contexto$columnas <- unique(unlist(
    lapply(entidades, lector$columnas), use.names = FALSE
  ))
  regla_formula <- identical(.nodo_para_huella(instancia), "predicado_fila")
  declaracion <- .subset2(instancia, "declaracion")
  configuracion_original <- .subset2(instancia, "configuracion")
  # La metrica, su declaracion y su configuracion se leen con `$`: un metodo de
  # quien llama para sus clases cambia lo que se mide.
  .revisar_clases(
    c(oldClass(instancia), oldClass(declaracion), oldClass(configuracion_original)),
    contexto, de_dato = FALSE
  )
  texto_metrica <- .parte_memorizada(contexto, "metrica", declaracion, function() {
    metrica <- lapply(.CAMPOS_DEFINICION_METRICA, function(campo) {
      .subset2(declaracion, campo)
    })
    names(metrica) <- .CAMPOS_DEFINICION_METRICA
    metrica$propiedades <- .sort_por_bytes(as.character(metrica$propiedades))
    .texto_configuracion_calidad(metrica)
  })
  # Una configuracion con funciones o formulas no se memoriza: cada regla suele
  # ser otra funcion, y compararlas todas contra todas con `identical()` costaba
  # mas que describirlas.
  con_codigo <- any(vapply(unclass(configuracion_original), function(v) {
    is.function(v) || inherits(v, "formula")
  }, logical(1L)))
  texto_configuracion <- .parte_memorizada(
    contexto, if (con_codigo) NULL else "configuracion",
    list(configuracion_original, regla_formula, contexto$columnas),
    function() {
      configuracion <- unclass(configuracion_original)
      nombres_configuracion <- names(configuracion)
      if (length(configuracion) && !is.null(nombres_configuracion)) {
        orden <- .orden_por_bytes(nombres_configuracion)
        configuracion <- configuracion[orden]
        nombres_configuracion <- nombres_configuracion[orden]
      }
      configuracion <- lapply(seq_along(configuracion), function(i) {
        nombre <- if (is.null(nombres_configuracion)) "" else nombres_configuracion[[i]]
        valor <- configuracion[[i]]
        if (nombre %in% c("diccionario", "valores", "valores_nulos")) {
          valor <- .ordenar_conjunto(valor)
        }
        .canonica_definicion(
          valor, contexto,
          con_entorno = !(regla_formula && identical(nombre, "regla"))
        )
      })
      names(configuracion) <- nombres_configuracion
      .texto_configuracion_calidad(configuracion)
    }
  )
  atributos <- as.character(unlist(.subset2(instancia, "atributos")))
  if (regla_formula) atributos <- .sort_por_bytes(atributos)
  # El texto es el de `.texto_configuracion_calidad()` sobre la lista de las
  # partes, armado parte por parte para no describir dos veces lo compartido.
  textos <- c(
    esquema = .texto_configuracion_calidad(.ESQUEMA_HUELLA_DEFINICION),
    metrica = texto_metrica,
    configuracion = texto_configuracion,
    entidad = .parte_memorizada(
      contexto, "entidad", entidades,
      function() .texto_configuracion_calidad(entidades)
    ),
    atributos = .texto_configuracion_calidad(atributos)
  )
  partes <- list()
  # El referencial que comparten varias metricas se describe una vez: ordenar
  # un padron de cien mil filas en cada una de doscientas metricas era casi
  # todo el costo del sello. Medido.
  referencial <- .subset2(instancia, "referencial")
  texto_referencial <- if (!is.null(referencial)) {
    .parte_memorizada(contexto, "referencial", referencial, function() {
      .texto_configuracion_calidad(.canonica_definicion(
        .referencial_para_huella(referencial, contexto), contexto
      ))
    })
  }
  # El metodo declarado al instanciar entra con lo que lee de su entorno. El
  # metodo por omision de una metrica escrita con `metrica()` entra por su
  # texto: como el de una metrica del paquete, es la implementacion de la
  # metrica, que se identifica por su nombre (el criterio de
  # `configuracion_modelo`, ver `.configuracion_modelo_calidad()`). Leer su
  # entorno haria de un valor que esa implementacion arrastra un cambio de
  # definicion de toda la serie, y la comparacion de dos corridas de la misma
  # metrica dejaria de publicarse.
  metodo <- .subset2(instancia, "metodo")
  if (isTRUE(.subset2(instancia, "metodo_declarado"))) {
    partes$metodo <- .canonica_definicion(metodo, contexto)
  } else if (!.metodo_del_paquete(metodo)) {
    partes$metodo <- .canonica_definicion(metodo, contexto, con_entorno = FALSE)
  }
  reglas <- .aplicabilidad_de_instancia(aplicabilidad, instancia)
  if (length(reglas)) {
    partes$aplicabilidad <- .canonica_definicion(reglas, contexto)
  }
  if (identical(orden_texto, "bytes") && .depende_orden_texto(instancia, lector)) {
    partes$orden_texto <- "bytes"
  }
  if (length(partes)) {
    textos <- c(textos, vapply(partes, .texto_configuracion_calidad, character(1L)))
  }
  if (!is.null(texto_referencial)) {
    # En el lugar que tenia: despues del metodo y la aplicabilidad no, antes.
    textos <- c(
      textos[setdiff(names(textos), c("metodo", "aplicabilidad", "orden_texto"))],
      referencial = texto_referencial,
      textos[intersect(c("metodo", "aplicabilidad", "orden_texto"), names(textos))]
    )
  }
  # Cada parte ya trae su texto canonico -cada hoja paso por `.clave_bytes()`-:
  # unirlas no necesita otra pasada, que sobre el texto entero costaba un tercio
  # del sello.
  unir <- function(x) if (length(x)) paste(unique(x), collapse = "; ") else NA_character_
  list(
    texto = paste0(
      "list{", paste0(names(textos), "=", textos, collapse = ";"), "}"
    ),
    motivo = unir(contexto$motivos),
    parcial = unir(contexto$parciales),
    nota = unir(contexto$notas)
  )
}

# La version de lupa se lee una vez por sesion: `packageVersion()` lee el
# DESCRIPTION cada vez, y era un octavo del costo del sello de una medicion.
.ESTADO_SELLO <- new.env(parent = emptyenv())

.version_paquete_sello <- function() {
  if (is.null(.ESTADO_SELLO$version)) .ESTADO_SELLO$version <- .version_paquete()
  .ESTADO_SELLO$version
}

# El entorno de la sesion que puede cambiar lo que se mide sin cambiar la
# definicion: `scipen` y `OutDec` (un numero comparado con un texto pasa por
# `as.character()`, que los lee), `LC_COLLATE` (el orden del texto con
# `orden_texto = "idioma"`, y siempre en `aplicable`), `LC_CTYPE` (las clases de
# caracteres de una expresion regular), `LC_TIME` (los nombres de meses y dias
# de `months()`, `weekdays()` y `as.Date()` con formato), el huso (un instante
# comparado con un texto) y la version de R. No es la definicion -la misma regla en otra sesion
# es la misma regla-, y por eso va aparte de la huella: comparar dos corridas
# con la misma definicion y otro entorno se avisa, no se retira. Medido: con
# `scipen = 0` y `scipen = 100`, `NoNulo(valores_nulos = 100000)` sobre el texto
# `"100000"` daba otro resultado con la misma huella.
.entorno_medicion <- function() {
  huso <- Sys.getenv("TZ")
  if (!nzchar(huso)) {
    huso <- tryCatch(Sys.timezone(), error = function(e) NA_character_)
  }
  if (!length(huso) || is.na(huso[[1L]]) || !nzchar(huso[[1L]])) huso <- "desconocido"
  scipen <- suppressWarnings(as.numeric(getOption("scipen", 0))[1L])
  if (is.na(scipen)) scipen <- 0
  separador <- as.character(getOption("OutDec", "."))[1L]
  partes <- c(
    R = paste(R.version$major, R.version$minor, sep = "."),
    scipen = sprintf("%.0f", scipen),
    OutDec = separador,
    LC_COLLATE = Sys.getlocale("LC_COLLATE"),
    LC_CTYPE = Sys.getlocale("LC_CTYPE"),
    LC_TIME = Sys.getlocale("LC_TIME"),
    TZ = as.character(huso)[[1L]]
  )
  paste0(names(partes), "=", .clave_bytes(partes), collapse = ";")
}

# El entorno de cada corrida de un sello: un vector con nombre, `id_medicion` ->
# entorno, en el atributo `entorno` de la tabla. Un sello sin el -de una version
# que no lo registraba- da `NA`: entorno desconocido.
.entorno_de_sello <- function(sello) {
  mapa <- attr(sello, "entorno", exact = TRUE)
  if (is.null(mapa) || is.null(names(mapa))) return(character())
  mapa
}

.entorno_por_fila <- function(sello) {
  mapa <- .entorno_de_sello(sello)
  if (!length(mapa) || !nrow(sello)) return(rep(NA_character_, nrow(sello)))
  unname(mapa[match(.clave_bytes(as.character(sello$id_medicion)),
                    .clave_bytes(names(mapa)))])
}

# Lo que cambio entre dos entornos, parte por parte: `scipen: 0 -> 100`.
.diferencias_de_entorno <- function(anterior, actual) {
  partes <- function(texto) {
    campos <- strsplit(texto, ";", fixed = TRUE)[[1L]]
    claves <- sub("=.*$", "", campos)
    valores <- substring(campos, nchar(claves, type = "bytes") + 2L)
    stats::setNames(valores, claves)
  }
  vapply(seq_along(anterior), function(i) {
    if (is.na(anterior[[i]]) || is.na(actual[[i]])) return(NA_character_)
    a <- partes(anterior[[i]])
    b <- partes(actual[[i]])
    claves <- unique(c(names(a), names(b)))
    distintas <- claves[vapply(claves, function(k) !identical(a[k][[1L]], b[k][[1L]]), logical(1L))]
    if (!length(distintas)) return(NA_character_)
    paste0(distintas, ": ", a[distintas], " -> ", b[distintas], collapse = ", ")
  }, character(1L))
}

# Lo que lleva `huella_definicion` cuando la definicion no se pudo certificar.
.HUELLA_NO_CERTIFICABLE <- "no_certificable"

# El sello de una corrida: una fila por metrica instanciada del modelo -tambien
# las que no se pudieron medir, porque su definicion es la misma-.
#
# Sellar nunca hace fallar una medicion: un error al describir una metrica la
# deja `no_certificable` con el error como motivo, un aviso no sale, y si algo
# fallara afuera de eso el sello entero queda `no_certificable` y la medicion
# sigue. `main` media; un `length()` propio que fallaba al sellar abortaba
# `medir()`. Medido.
.sello_metricas <- function(modelo, id_medicion, aplicabilidad, orden_texto,
                            lector) {
  instancias <- unname(.subset2(modelo, "metricas"))
  nombres <- vapply(instancias, function(i) {
    as.character(.subset2(i, "nombre"))[[1L]]
  }, character(1L))
  especificas <- vapply(instancias, function(i) {
    as.character(.subset2(i, "nombre_especifico"))[[1L]]
  }, character(1L))
  mensaje <- function(e) {
    texto <- tryCatch(conditionMessage(e), error = function(e2) "error")
    paste0("no se pudo describir la definici\u00f3n: ", texto)
  }
  sin_avisos <- function(expresion) {
    withCallingHandlers(expresion, warning = function(w) invokeRestart("muffleWarning"))
  }
  sellar <- function() {
    # Una memoria por llamada: la version de cada paquete se pregunta una vez,
    # y un objeto grande que comparten varias metricas se resume una vez.
    memoria <- .memoria_huella()
    definiciones <- lapply(instancias, function(instancia) {
      tryCatch(
        sin_avisos(.texto_definicion_metrica(
          instancia, aplicabilidad = aplicabilidad, orden_texto = orden_texto,
          lector = lector, memoria = memoria
        )),
        error = function(e) {
          list(texto = NA_character_, motivo = mensaje(e),
               parcial = NA_character_, nota = NA_character_)
        }
      )
    })
    campo <- function(nombre) {
      vapply(definiciones, function(d) {
        valor <- d[[nombre]]
        if (is.null(valor)) NA_character_ else valor
      }, character(1L))
    }
    textos <- campo("texto")
    motivos <- campo("motivo")
    parciales <- campo("parcial")
    notas <- campo("nota")
    huellas <- rep(.HUELLA_NO_CERTIFICABLE, length(textos))
    certificadas <- is.na(motivos) & !is.na(textos)
    if (any(certificadas)) huellas[certificadas] <- .md5_textos(textos[certificadas])
    certificacion <- ifelse(
      !certificadas, "no_certificable",
      ifelse(is.na(parciales), "completa", "parcial")
    )
    explicacion <- ifelse(
      is.na(parciales), notas,
      ifelse(is.na(notas), parciales, paste(parciales, notas, sep = "; "))
    )
    list(
      huellas = huellas, certificacion = certificacion,
      motivos = ifelse(certificadas, explicacion, motivos)
    )
  }
  sellado <- tryCatch(sin_avisos(sellar()), error = function(e) {
    list(
      huellas = rep(.HUELLA_NO_CERTIFICABLE, length(instancias)),
      certificacion = rep("no_certificable", length(instancias)),
      motivos = rep(mensaje(e), length(instancias))
    )
  })
  resultado <- data.frame(
    id_medicion = rep(as.character(id_medicion), length(instancias)),
    metrica_instanciada = nombres,
    metrica_especifica = especificas,
    version_lupa = rep(.version_paquete_sello(), length(instancias)),
    huella_definicion = sellado$huellas,
    certificacion = sellado$certificacion,
    motivo_huella = sellado$motivos,
    stringsAsFactors = FALSE
  )
  attr(resultado, "entorno") <- stats::setNames(
    tryCatch(.entorno_medicion(), error = function(e) NA_character_),
    as.character(id_medicion)
  )
  resultado
}

.sello_vacio <- function() {
  data.frame(
    id_medicion = character(), metrica_instanciada = character(),
    metrica_especifica = character(), version_lupa = character(),
    huella_definicion = character(), certificacion = character(),
    motivo_huella = character(), stringsAsFactors = FALSE
  )
}

# El nivel de cada fila de un sello. Uno sin la columna -guardado por una
# version que no la tenia- es parcial donde tiene huella: no se sabe si era
# completo, y no se afirma.
.certificacion_de <- function(sello) {
  if ("certificacion" %in% names(sello)) {
    nivel <- as.character(sello$certificacion)
  } else {
    nivel <- rep(NA_character_, nrow(sello))
  }
  huella <- as.character(sello$huella_definicion)
  desconocido <- is.na(nivel) | !nivel %in% .CERTIFICACIONES
  nivel[desconocido] <- ifelse(
    huella[desconocido] %in% .HUELLA_NO_CERTIFICABLE, "no_certificable", "parcial"
  )
  nivel
}

# Las metricas instanciadas del sello que alimentan las medidas que una
# evaluacion evaluo. Una medida cruda se llama como su metrica; una agregada
# (`agregar()`) se llama `agregada:<funcion>:<base>`, donde la base es la
# metrica especifica o la lista de instancias que junto. `NULL` si alguna no se
# puede resolver: entonces se toman todas las de la corrida, que es lo prudente
# -una metrica de mas compara de mas, una de menos calla un cambio-.
.metricas_de_origen_sello <- function(evaluadas, sello) {
  if (!inherits(sello, "data.frame") || !nrow(sello)) return(NULL)
  evaluadas <- .identificadores_unicos(as.character(evaluadas))
  if (!length(evaluadas)) return(character())
  nombres <- as.character(sello$metrica_instanciada)
  especificas <- if ("metrica_especifica" %in% names(sello)) {
    as.character(sello$metrica_especifica)
  } else {
    rep(NA_character_, nrow(sello))
  }
  origen <- character()
  for (nombre in evaluadas) {
    if (.identificadores_en(nombre, nombres)) {
      origen <- c(origen, nombre)
      next
    }
    # Posiciones de caracter, no de bytes: el nombre es texto valido, y si no
    # lo fuera la lectura cae al lado prudente.
    base <- tryCatch({
      base <- nombre
      while (startsWith(base, "agregada:")) {
        resto <- substring(base, 10L)
        corte <- regexpr(":", resto, fixed = TRUE)
        if (corte < 1L) break
        base <- substring(resto, corte + 1L)
      }
      base
    }, error = function(e) nombre)
    if (identical(base, nombre)) return(NULL)
    por_especifica <- nombres[.identificadores_en(especificas, base)]
    piezas <- strsplit(base, ", ", fixed = TRUE)[[1L]]
    por_instancia <- nombres[.identificadores_en(nombres, piezas)]
    if (!length(por_especifica) &&
        length(por_instancia) != length(unique(piezas))) {
      return(NULL)
    }
    origen <- c(origen, por_especifica, por_instancia)
  }
  .identificadores_unicos(origen)
}

.sello_de_objeto <- function(x) {
  sello <- attr(x, "sello_metricas", exact = TRUE)
  if (!inherits(sello, "data.frame")) return(NULL)
  sello
}

# La huella se publica corta: doce cifras alcanzan para que una persona vea que
# cambio, y la comparacion usa la entera.
.huella_corta <- function(huella) {
  ifelse(is.na(huella), "sin huella", substr(huella, 1L, 12L))
}

# Los cambios de definicion entre corridas CONSECUTIVAS: por cada metrica, las
# corridas que la sellaron ordenadas por fecha -los empates por los bytes de
# `id_medicion`, como en la deriva- y cada par de vecinas con otra huella. Una
# serie que cambia una vez da un cambio, no uno por cada corrida posterior.
#
# `tipo` separa lo que se sabe de lo que no: `cambio` es una huella distinta;
# `incierto`, un par donde alguna de las dos no se pudo certificar; `parcial`,
# la misma huella con alguna de las dos parcial: no se vio que cambiara, pero
# la huella no ve todo, y no se afirma que no cambio.
.cambios_de_definicion <- function(sellos, fechas) {
  vacio <- data.frame(
    metrica_instanciada = character(), id_anterior = character(),
    id_actual = character(), version_anterior = character(),
    version_actual = character(), huella_anterior = character(),
    huella_actual = character(), tipo = character(), stringsAsFactors = FALSE
  )
  pares <- .pares_de_sellos(sellos, fechas)
  if (is.null(pares)) return(vacio)
  sellos <- pares$sellos
  a <- pares$a
  b <- pares$b
  huella <- sellos$huella_definicion
  nivel <- .certificacion_de(sellos)
  incierto <- huella[a] %in% .HUELLA_NO_CERTIFICABLE |
    huella[b] %in% .HUELLA_NO_CERTIFICABLE
  parcial <- !incierto & huella[a] == huella[b] &
    (nivel[a] == "parcial" | nivel[b] == "parcial")
  distintas <- incierto | huella[a] != huella[b] | parcial
  if (!any(distintas)) return(vacio)
  tipo <- ifelse(incierto, "incierto", ifelse(parcial, "parcial", "cambio"))[distintas]
  a <- a[distintas]
  b <- b[distintas]
  resultado <- data.frame(
    metrica_instanciada = sellos$metrica_instanciada[a],
    id_anterior = sellos$id_medicion[a], id_actual = sellos$id_medicion[b],
    version_anterior = sellos$version_lupa[a],
    version_actual = sellos$version_lupa[b],
    huella_anterior = huella[a], huella_actual = huella[b],
    tipo = tipo, stringsAsFactors = FALSE
  )
  rownames(resultado) <- NULL
  resultado
}

# Los pares de corridas CONSECUTIVAS de cada metrica: las filas ordenadas por
# metrica, fecha e id -los empates por los bytes de `id_medicion`, como en la
# deriva- y cada fila con la anterior si es de la misma metrica. Una sola
# pasada; antes era un recorrido por metrica y un `data.frame` por grupo.
.pares_de_sellos <- function(sellos, fechas) {
  if (!inherits(sellos, "data.frame") || nrow(sellos) < 2L) return(NULL)
  sellos <- sellos[!is.na(sellos$huella_definicion), , drop = FALSE]
  clave_metrica <- .clave_bytes(sellos$metrica_instanciada)
  clave_id <- .clave_bytes(sellos$id_medicion)
  unicas <- !duplicated(paste(clave_id, clave_metrica, sep = "\034"))
  sellos <- sellos[unicas, , drop = FALSE]
  clave_metrica <- clave_metrica[unicas]
  clave_id <- clave_id[unicas]
  n <- nrow(sellos)
  if (n < 2L) return(NULL)
  instante <- as.numeric(fechas$fecha)[
    .indice_identificador(sellos$id_medicion, fechas$id_medicion)
  ]
  orden <- .orden_por_bytes(clave_metrica, instante, clave_id)
  sellos <- sellos[orden, , drop = FALSE]
  clave_metrica <- clave_metrica[orden]
  b <- which(clave_metrica[-1L] == clave_metrica[-n]) + 1L
  list(sellos = sellos, a = b - 1L, b = b)
}

# Los pares consecutivos de una metrica con la MISMA definicion y otro entorno
# de sesion (`tipo = "entorno"`, con lo que cambio), o donde una de las dos
# corridas no registra su entorno (`"entorno_desconocido"`). Una huella no
# certificable no es "la misma definicion": ese par ya se declara.
.cambios_de_entorno <- function(sellos, fechas) {
  vacio <- data.frame(
    metrica_instanciada = character(), id_anterior = character(),
    id_actual = character(), certificacion = character(), tipo = character(),
    cambio = character(), stringsAsFactors = FALSE
  )
  pares <- .pares_de_sellos(sellos, fechas)
  if (is.null(pares) || !length(pares$a)) return(vacio)
  sellos <- pares$sellos
  a <- pares$a
  b <- pares$b
  huella <- sellos$huella_definicion
  entorno <- .entorno_por_fila(sellos)
  nivel <- .certificacion_de(sellos)
  misma <- huella[a] == huella[b] & !huella[a] %in% .HUELLA_NO_CERTIFICABLE
  conocido <- !is.na(entorno[a]) & !is.na(entorno[b])
  otro <- misma & conocido & entorno[a] != entorno[b]
  desconocido <- misma & !conocido
  quedan <- otro | desconocido
  if (!any(quedan)) return(vacio)
  a <- a[quedan]
  b <- b[quedan]
  otro <- otro[quedan]
  resultado <- data.frame(
    metrica_instanciada = sellos$metrica_instanciada[a],
    id_anterior = sellos$id_medicion[a], id_actual = sellos$id_medicion[b],
    certificacion = ifelse(nivel[a] == "parcial" | nivel[b] == "parcial",
                           "parcial", "completa"),
    tipo = ifelse(otro, "entorno", "entorno_desconocido"),
    cambio = ifelse(otro, .diferencias_de_entorno(entorno[a], entorno[b]), NA_character_),
    stringsAsFactors = FALSE
  )
  rownames(resultado) <- NULL
  resultado
}

# Un aviso por par de corridas, con lo que cambio y cuantas metricas mide con la
# misma definicion. No es `no_comparable`: la definicion es la misma.
.avisar_cambios_entorno <- function(cambios, donde) {
  if (!inherits(cambios, "data.frame") || !nrow(cambios)) return(invisible(NULL))
  cambios <- cambios[cambios$tipo %in% "entorno", , drop = FALSE]
  if (!nrow(cambios)) return(invisible(NULL))
  par <- paste(.clave_bytes(cambios$id_anterior), .clave_bytes(cambios$id_actual),
               sep = "\034")
  primeros <- !duplicated(par)
  cuantas <- as.vector(table(par)[par[primeros]])
  .informar_sello(
    donde, ": misma definici\u00f3n, otro entorno: ",
    paste0(
      "entre `", cambios$id_anterior[primeros], "` y `", cambios$id_actual[primeros],
      "` (", cuantas, ifelse(cuantas == 1L, " m\u00e9trica", " m\u00e9tricas"),
      ") cambi\u00f3 ", cambios$cambio[primeros], collapse = "; "
    ),
    ". Una m\u00e9trica puede medir distinto por las opciones, el locale o el huso de ",
    "la sesi\u00f3n; el sello de cada corrida registra el suyo."
  )
}

.clave_cambios_entorno <- function(cambios) {
  paste(
    .clave_bytes(cambios$metrica_instanciada), .clave_bytes(cambios$id_anterior),
    .clave_bytes(cambios$id_actual), cambios$tipo, sep = "\034"
  )
}

.clave_cambios_definicion <- function(cambios) {
  paste(
    .clave_bytes(cambios$metrica_instanciada), .clave_bytes(cambios$id_anterior),
    .clave_bytes(cambios$id_actual), cambios$tipo, sep = "\034"
  )
}

.describir_cambio_definicion <- function(cambios) {
  paste0(
    "`", cambios$metrica_instanciada, "` entre `", cambios$id_anterior,
    "` y `", cambios$id_actual, "` (huella ",
    .huella_corta(cambios$huella_anterior), ", lupa ", cambios$version_anterior,
    " -> huella ", .huella_corta(cambios$huella_actual), ", lupa ",
    cambios$version_actual, ")"
  )
}

# Aviso de R base y no de cli: el mensaje trae nombres de metricas y corridas,
# que son texto de quien llama, y cli lo evaluaria como plantilla. Con clase
# propia, para que una funcion que compara por medio de otra
# -`comparar_evaluaciones()` arma un historico- lo diga con su nombre.
# Que la misma definicion se mida con otro entorno es una posibilidad, no un
# cambio: se informa (mensaje) y se declara en la cobertura, sin advertir en cada
# sesion con otro locale. El cambio de definicion si es una advertencia.
.informar_sello <- function(...) {
  message(structure(
    class = c("lupa_aviso_sello_entorno", "lupa_aviso_sello", "message", "condition"),
    list(message = paste0(.mensaje_condicion(...), "\n"), call = NULL)
  ))
}

.aviso_sello <- function(...) {
  .avisar(structure(
    class = c("lupa_aviso_sello", "warning", "condition"),
    list(message = .mensaje_condicion(...), call = NULL)
  ))
}

.avisar_cambios_definicion <- function(cambios, donde, donde_sello) {
  if (!inherits(cambios, "data.frame") || !nrow(cambios)) return(invisible(NULL))
  if ("tipo" %in% names(cambios)) {
    .avisar_definicion_incierta(
      cambios[cambios$tipo %in% "incierto", , drop = FALSE], donde
    )
    cambios <- cambios[cambios$tipo %in% "cambio", , drop = FALSE]
    if (!nrow(cambios)) return(invisible(NULL))
  }
  n <- length(.identificadores_unicos(cambios$metrica_instanciada))
  .aviso_sello(
    donde, ": ",
    if (n == 1L) {
      "una m\u00e9trica cambi\u00f3 de definici\u00f3n"
    } else {
      paste0(n, " m\u00e9tricas cambiaron de definici\u00f3n")
    },
    " de una corrida a la siguiente, y sus resultados no miden lo mismo: ",
    paste(.describir_cambio_definicion(cambios), collapse = "; "),
    ". El sello de cada corrida est\u00e1 en ", donde_sello, "."
  )
}

# Una huella que no se pudo certificar no compara: no se sabe si la definicion
# cambio, y el aviso lo dice en vez de callar.
.avisar_definicion_incierta <- function(cambios, donde) {
  if (!nrow(cambios)) return(invisible(NULL))
  .aviso_sello(
    donde, ": no se puede saber si cambi\u00f3 la definici\u00f3n de ",
    paste0(
      "`", cambios$metrica_instanciada, "` entre `", cambios$id_anterior,
      "` y `", cambios$id_actual, "`", collapse = "; "
    ),
    ": su definici\u00f3n lee de su entorno un valor que no se puede certificar ",
    "(un entorno, un puntero externo, una conexi\u00f3n o una lectura ",
    "din\u00e1mica). El motivo est\u00e1 en la columna `motivo_huella` del ",
    "sello de la medici\u00f3n."
  )
}

.avisar_sin_sello <- function(ids, donde) {
  ids <- .identificadores_unicos(ids)
  if (!length(ids)) return(invisible(NULL))
  .aviso_sello(
    donde, ": ",
    if (length(ids) == 1L) {
      "una corrida no trae sello de definici\u00f3n"
    } else {
      paste0(length(ids), " corridas no traen sello de definici\u00f3n")
    },
    " (se midi\u00f3 con una versi\u00f3n anterior de lupa): ",
    paste0("`", ids, "`", collapse = ", "),
    ". Se compara como antes, sin poder comprobar si alguna m\u00e9trica ",
    "cambi\u00f3 de definici\u00f3n."
  )
}

# El sello en el historico: un texto por corrida, para que sobreviva a
# `write.csv()` como las columnas `configuracion_*`. Cada metrica es
# `nombre|version|huella|certificacion`, con el nombre y la version escapados
# como las claves del historico (`|` y `;` no aparecen dentro de ninguno),
# ordenadas por bytes.
# Una corrida sellada sin metricas evaluadas es solo la marca; una sin sello,
# `NA`. El entorno de la corrida va como una entrada `entorno=...` despues de
# la marca, escapada como las claves; un lector que no la conoce la descarta,
# porque no tiene los campos de una metrica.
.texto_sello_corrida <- function(sello, id = NULL) {
  if (!inherits(sello, "data.frame")) return(NA_character_)
  mapa <- .entorno_de_sello(sello)
  if (is.null(id) && nrow(sello)) id <- as.character(sello$id_medicion[[1L]])
  entorno <- if (length(mapa) && !is.null(id)) {
    mapa[.clave_bytes(names(mapa)) == .clave_bytes(as.character(id))]
  }
  entrada_entorno <- if (length(entorno) && !is.na(entorno[[1L]])) {
    paste0("entorno", .escapar_clave(entorno[[1L]]))
  }
  if (!nrow(sello)) {
    return(paste(c(.MARCA_SELLO_HISTORICO, entrada_entorno), collapse = ";"))
  }
  claves <- .escapar_clave(sello$metrica_instanciada)
  entradas <- paste(
    claves, .escapar_clave(sello$version_lupa), sello$huella_definicion,
    .certificacion_de(sello), sep = "|"
  )
  entradas <- unique(entradas[.orden_por_bytes(claves)])
  paste(c(.MARCA_SELLO_HISTORICO, entrada_entorno, entradas), collapse = ";")
}

.desescapar_clave <- function(x) {
  vapply(x, function(z) {
    if (is.na(z) || identical(z, "~")) return(NA_character_)
    texto <- utils::URLdecode(sub("^=", "", z))
    if (validUTF8(texto)) Encoding(texto) <- "UTF-8"
    texto
  }, character(1L), USE.NAMES = FALSE)
}

.es_texto_sello <- function(textos) {
  textos <- as.character(textos)
  marca <- sub(";.*$", "", textos)
  !is.na(textos) & marca %in% .MARCAS_SELLO_LEGIBLES
}

# Los sellos de las corridas de una tabla de configuraciones del historico, en
# la forma de `attr(medicion, "sello_metricas")` pero con el nombre y la version
# escapados (`.sello_largo()`). Una corrida aparece en una fila por perfil,
# cada una con las metricas que ese perfil evaluo: se unen.
# El texto de corrida sin la entrada de su entorno.
.sello_sin_entorno <- function(textos) {
  gsub(";entorno=[^;]*", "", as.character(textos), useBytes = TRUE)
}

# El entorno de cada texto de corrida, o `NA` si no lo trae.
.entornos_de_textos <- function(textos) {
  textos <- as.character(textos)
  vapply(textos, function(texto) {
    if (is.na(texto)) return(NA_character_)
    entradas <- strsplit(texto, ";", fixed = TRUE)[[1L]]
    entrada <- entradas[startsWith(entradas, "entorno=")]
    if (!length(entrada)) return(NA_character_)
    .desescapar_clave(substring(entrada[[1L]], 8L))
  }, character(1L), USE.NAMES = FALSE)
}

.sellos_de_configuraciones <- function(configuraciones) {
  if (!inherits(configuraciones, "data.frame") || !nrow(configuraciones) ||
      !"sello_metricas" %in% names(configuraciones)) {
    return(.sello_vacio())
  }
  largo <- .sello_largo(configuraciones$sello_metricas)
  if (!nrow(largo)) return(.sello_vacio())
  tabla <- data.frame(
    id_medicion = as.character(configuraciones$id_medicion)[largo$fila],
    metrica_instanciada = largo$clave,
    metrica_especifica = rep(NA_character_, nrow(largo)),
    version_lupa = largo$version,
    huella_definicion = largo$huella,
    certificacion = largo$certificacion,
    motivo_huella = rep(NA_character_, nrow(largo)),
    stringsAsFactors = FALSE
  )
  # El entorno por corrida, el primero que se conozca de cada una.
  entornos <- .entornos_de_textos(configuraciones$sello_metricas)
  ids <- as.character(configuraciones$id_medicion)
  conocidos <- !is.na(entornos)
  mapa <- entornos[conocidos]
  names(mapa) <- ids[conocidos]
  mapa <- mapa[!duplicated(.clave_bytes(names(mapa)))]
  if (length(mapa)) attr(tabla, "entorno") <- mapa
  tabla
}

# Todas las entradas de los sellos de un vector de textos de corrida, en una
# tabla larga -la fila de su texto, la clave escapada de la metrica, la version
# escapada y la huella-, de una vez y sin decodificar: alcanza para comparar.
.sello_largo <- function(textos) {
  textos <- as.character(textos)
  sellados <- .es_texto_sello(textos)
  partes <- strsplit(ifelse(sellados, textos, ""), ";", fixed = TRUE)
  cantidad <- pmax(lengths(partes) - 1L, 0L)
  entradas <- unlist(lapply(partes, function(x) x[-1L]), use.names = FALSE)
  fila <- rep(seq_along(textos), cantidad)
  campos <- strsplit(entradas, "|", fixed = TRUE)
  # Tres campos en la primera forma, cuatro -con la certificacion- en la segunda.
  bien <- lengths(campos) %in% c(3L, 4L)
  if (!any(bien)) {
    return(data.frame(
      fila = integer(), clave = character(), version = character(),
      huella = character(), certificacion = character(),
      stringsAsFactors = FALSE
    ))
  }
  campos <- lapply(campos[bien], function(x) if (length(x) == 3L) c(x, NA_character_) else x)
  valores <- matrix(unlist(campos, use.names = FALSE), ncol = 4L, byrow = TRUE)
  largo <- data.frame(
    fila = fila[bien], clave = valores[, 1L], version = valores[, 2L],
    huella = valores[, 3L], certificacion = valores[, 4L],
    stringsAsFactors = FALSE
  )
  largo$certificacion <- .certificacion_de(data.frame(
    certificacion = largo$certificacion, huella_definicion = largo$huella,
    stringsAsFactors = FALSE
  ))
  largo
}

# Las corridas de una tabla de configuraciones que no traen sello, y los pares
# de corridas consecutivas -por fecha- donde una lo trae y la otra no.
.corridas_sin_sello <- function(configuraciones) {
  if (!inherits(configuraciones, "data.frame") || !nrow(configuraciones)) {
    return(character())
  }
  textos <- if ("sello_metricas" %in% names(configuraciones)) {
    as.character(configuraciones$sello_metricas)
  } else {
    rep(NA_character_, nrow(configuraciones))
  }
  sellada <- .es_texto_sello(textos)
  ids <- as.character(configuraciones$id_medicion)
  con <- .identificadores_unicos(ids[sellada])
  sin <- .identificadores_unicos(ids[!sellada])
  sin[!.identificadores_en(sin, con)]
}

.pares_sin_sello <- function(configuraciones) {
  sin <- .corridas_sin_sello(configuraciones)
  if (!length(sin)) return(character())
  ids <- as.character(configuraciones$id_medicion)
  primera <- !duplicated(.clave_bytes(ids))
  ids <- ids[primera]
  fechas <- as.numeric(configuraciones$fecha)[primera]
  if (length(ids) < 2L) return(character())
  orden <- .orden_por_bytes(fechas, .clave_bytes(ids))
  ids <- ids[orden]
  sin_sello <- .identificadores_en(ids, sin)
  a <- seq_len(length(ids) - 1L)
  b <- a + 1L
  mixtos <- sin_sello[a] != sin_sello[b]
  if (!any(mixtos)) return(character())
  paste(.clave_bytes(ids[a][mixtos]), .clave_bytes(ids[b][mixtos]), sep = "\034")
}

# Lo nuevo que una union de historicos trae, para avisar UNA vez: un cambio de
# definicion entre dos corridas que ya estaban juntas se aviso cuando se
# juntaron, y acumular una corrida mas no lo vuelve a decir.
.avisar_sellos_historico <- function(resultado, previos = list(), donde) {
  configuracion <- attr(resultado, "configuracion_evaluacion", exact = TRUE)
  cambios <- .cambios_de_definicion(
    .sellos_de_configuraciones(configuracion), configuracion
  )
  pares_sin <- .pares_sin_sello(configuracion)
  conocidos <- character()
  conocidos_sin <- character()
  for (previo in previos) {
    configuracion_previa <- attr(previo, "configuracion_evaluacion", exact = TRUE)
    anteriores <- .cambios_de_definicion(
      .sellos_de_configuraciones(configuracion_previa), configuracion_previa
    )
    conocidos <- c(conocidos, .clave_cambios_definicion(anteriores))
    conocidos_sin <- c(conocidos_sin, .pares_sin_sello(configuracion_previa))
  }
  if (nrow(cambios)) {
    cambios <- cambios[
      !.clave_cambios_definicion(cambios) %in% conocidos, , drop = FALSE
    ]
    for (campo in c("metrica_instanciada", "version_anterior", "version_actual")) {
      cambios[[campo]] <- .desescapar_clave(cambios[[campo]])
    }
  }
  .avisar_cambios_definicion(
    cambios, donde, "`attr(historico, \"configuracion_evaluacion\")$sello_metricas`"
  )
  # El entorno: un par nuevo con la misma definicion y otro entorno se avisa
  # una vez, como un cambio de definicion.
  entornos <- .cambios_de_entorno(
    .sellos_de_configuraciones(configuracion), configuracion
  )
  conocidos_entorno <- unlist(lapply(previos, function(previo) {
    configuracion_previa <- attr(previo, "configuracion_evaluacion", exact = TRUE)
    .clave_cambios_entorno(.cambios_de_entorno(
      .sellos_de_configuraciones(configuracion_previa), configuracion_previa
    ))
  }), use.names = FALSE)
  if (nrow(entornos)) {
    entornos <- entornos[!.clave_cambios_entorno(entornos) %in% conocidos_entorno, ,
                         drop = FALSE]
    entornos$metrica_instanciada <- .desescapar_clave(entornos$metrica_instanciada)
  }
  .avisar_cambios_entorno(entornos, donde)
  nuevos_sin <- setdiff(pares_sin, conocidos_sin)
  if (length(nuevos_sin)) {
    sin <- .corridas_sin_sello(configuracion)
    en_pares <- unique(unlist(strsplit(nuevos_sin, "\034", fixed = TRUE)))
    .avisar_sin_sello(sin[.clave_bytes(sin) %in% en_pares], donde)
  }
  invisible(NULL)
}

# Lo que un historico declara de su sello: cada par de corridas consecutivas de
# una metrica donde la misma huella no alcanza para decir que la definicion no
# cambio -una de las dos es parcial- o donde no se sabe -una no es
# certificable-. Un cambio de huella se avisa al juntar; esto no es un aviso,
# es la cobertura de la comparacion, y viaja en `attr(historico,
# "cobertura_sello")`. Sin nada que declarar, el atributo no esta.
.cobertura_sello_historico <- function(historico) {
  configuracion <- attr(historico, "configuracion_evaluacion", exact = TRUE)
  cambios <- .cambios_de_definicion(
    .sellos_de_configuraciones(configuracion), configuracion
  )
  dudosos <- cambios[cambios$tipo %in% c("parcial", "incierto"), , drop = FALSE]
  entornos <- .cambios_de_entorno(
    .sellos_de_configuraciones(configuracion), configuracion
  )
  if (!nrow(dudosos) && !nrow(entornos)) return(NULL)
  parcial <- dudosos$tipo == "parcial"
  tabla <- data.frame(
    id_medicion = dudosos$id_actual,
    id_medicion_anterior = dudosos$id_anterior,
    metrica_instanciada = .desescapar_clave(dudosos$metrica_instanciada),
    aspecto = ifelse(parcial, "huella_parcial", "no_certificable"),
    certificacion = ifelse(parcial, "parcial", "no_certificable"),
    descripcion = ifelse(
      parcial,
      paste(
        "huella parcial: una funci\u00f3n del usuario puede haber cambiado lo que",
        "lee sin que la huella lo vea"
      ),
      paste(
        "huella no certificable: la definici\u00f3n lee un valor que la huella no",
        "puede describir, y no se sabe si cambi\u00f3"
      )
    ),
    stringsAsFactors = FALSE
  )
  if (nrow(entornos)) {
    otro <- entornos$tipo == "entorno"
    tabla <- rbind(tabla, data.frame(
      id_medicion = entornos$id_actual,
      id_medicion_anterior = entornos$id_anterior,
      metrica_instanciada = .desescapar_clave(entornos$metrica_instanciada),
      aspecto = entornos$tipo,
      certificacion = entornos$certificacion,
      descripcion = ifelse(
        otro,
        paste0("misma definici\u00f3n, otro entorno: ", entornos$cambio),
        paste(
          "entorno desconocido: una de las dos corridas no registra el entorno",
          "de su sesi\u00f3n"
        )
      ),
      stringsAsFactors = FALSE
    ))
  }
  rownames(tabla) <- NULL
  tabla
}

.con_cobertura_sello <- function(historico) {
  attr(historico, "cobertura_sello") <- .cobertura_sello_historico(historico)
  historico
}

.ids_de_medicion <- function(x) {
  ids <- if ("id_medicion" %in% names(x)) as.character(x$id_medicion) else character()
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura, "data.frame") && "id_medicion" %in% names(cobertura)) {
    ids <- c(ids, as.character(cobertura$id_medicion))
  }
  .identificadores_unicos(ids)
}

.fechas_de_mediciones <- function(partes) {
  filas <- lapply(partes, function(x) {
    tablas <- list(
      if (all(c("id_medicion", "fecha") %in% names(x))) {
        data.frame(id_medicion = as.character(x$id_medicion),
                   fecha = as.numeric(x$fecha), stringsAsFactors = FALSE)
      },
      {
        cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
        if (inherits(cobertura, "data.frame") &&
            all(c("id_medicion", "fecha") %in% names(cobertura))) {
          data.frame(id_medicion = as.character(cobertura$id_medicion),
                     fecha = as.numeric(cobertura$fecha),
                     stringsAsFactors = FALSE)
        }
      }
    )
    do.call(rbind, Filter(Negate(is.null), tablas))
  })
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) {
    return(data.frame(id_medicion = character(), fecha = numeric(),
                      stringsAsFactors = FALSE))
  }
  fechas <- do.call(rbind, filas)
  fechas[!duplicated(.clave_bytes(fechas$id_medicion)), , drop = FALSE]
}

# Une los sellos de las mediciones que recibe `rbind()` y avisa lo que esa union
# compara: una metrica que cambio de definicion entre dos corridas, o partes que
# no traen sello. `NULL` si ninguna parte trae sello: lo unido se queda, como
# antes, sin el.
.unir_sellos_mediciones <- function(partes, donde) {
  sellos <- lapply(partes, function(parte) {
    sello <- .sello_de_objeto(parte)
    # Un sello sin la columna del nivel -de una version que no la tenia- se une
    # con el nivel que se le puede atribuir.
    if (!is.null(sello) && !"certificacion" %in% names(sello)) {
      sello$certificacion <- .certificacion_de(sello)
    }
    sello
  })
  con_sello <- !vapply(sellos, is.null, logical(1L))
  if (!any(con_sello)) return(NULL)
  if (!all(con_sello)) {
    .avisar_sin_sello(
      unlist(lapply(partes[!con_sello], .ids_de_medicion), use.names = FALSE),
      donde
    )
  }
  tabla <- unique(do.call(rbind, sellos[con_sello]))
  rownames(tabla) <- NULL
  # El entorno de cada corrida, de todas las partes.
  mapa <- unlist(lapply(sellos[con_sello], .entorno_de_sello))
  if (length(mapa)) {
    mapa <- mapa[!duplicated(.clave_bytes(names(mapa)))]
    attr(tabla, "entorno") <- mapa
  } else {
    attr(tabla, "entorno") <- NULL
  }
  # Dos sellos distintos de la misma m\u00e9trica en la misma corrida no son una
  # corrida armada por partes: son dos entregas con un id. Quedarse con uno
  # ser\u00eda elegir al azar cu\u00e1l definici\u00f3n midi\u00f3; se rechaza, como el resto de
  # `rbind()` rechaza dos modelos en dos corridas y como el hist\u00f3rico rechaza
  # una corrida ya acumulada que no coincide.
  clave <- paste(
    .clave_bytes(tabla$id_medicion), .clave_bytes(tabla$metrica_instanciada),
    sep = "\034"
  )
  repetidas <- duplicated(clave)
  if (any(repetidas)) {
    i <- which(clave == clave[repetidas][[1L]])
    .detener(
      donde, ": la corrida `", tabla$id_medicion[[i[[1L]]]], "` trae dos sellos ",
      "distintos de la m\u00e9trica `", tabla$metrica_instanciada[[i[[1L]]]], "` (",
      paste0("huella ", .huella_corta(tabla$huella_definicion[i]), ", lupa ",
             tabla$version_lupa[i], collapse = "; "),
      "): no es la misma medici\u00f3n, y unirlas dejar\u00eda una sola definici\u00f3n sin ",
      "saber cu\u00e1l. Dar a cada entrega su `id_medicion`.",
      call. = FALSE
    )
  }
  fechas <- .fechas_de_mediciones(partes)
  .avisar_cambios_definicion(
    .cambios_de_definicion(tabla, fechas), donde,
    "`attr(x, \"sello_metricas\")`"
  )
  .avisar_cambios_entorno(.cambios_de_entorno(tabla, fechas), donde)
  tabla
}

# Lo que el sello dice de cada par de corridas de una serie de la deriva:
# `anterior` y `actual` son las filas de `configuraciones` de cada lado (`NA`
# cuando el historico no declara la configuracion de esa corrida, que ya se
# declara aparte). Se comparan las metricas que las dos corridas sellaron.
.definiciones_deriva <- function(configuraciones, anterior, actual) {
  n <- length(anterior)
  textos <- if ("sello_metricas" %in% names(configuraciones)) {
    as.character(configuraciones$sello_metricas)
  } else {
    rep(NA_character_, nrow(configuraciones))
  }
  cambiada <- rep(FALSE, n)
  evidencia <- rep(NA_character_, n)
  metricas <- vector("list", n)
  inciertas <- vector("list", n)
  parciales <- vector("list", n)
  entorno_cambio <- rep(NA_character_, n)
  entorno_desconocido <- rep(FALSE, n)
  validos <- !is.na(anterior) & !is.na(actual)
  sellado <- .es_texto_sello(textos)
  sin_sello <- validos
  sin_sello[validos] <- !sellado[anterior[validos]] | !sellado[actual[validos]]
  pares <- which(validos & !sin_sello)
  if (length(pares)) {
    entornos <- .entornos_de_textos(textos)
    ea <- entornos[anterior[pares]]
    eb <- entornos[actual[pares]]
    entorno_desconocido[pares] <- is.na(ea) | is.na(eb)
    entorno_cambio[pares] <- .diferencias_de_entorno(ea, eb)
  }
  resultado <- function() {
    list(
      cambiada = cambiada, sin_sello = sin_sello, evidencia = evidencia,
      metricas = metricas, inciertas = inciertas, parciales = parciales,
      entorno_cambio = entorno_cambio, entorno_desconocido = entorno_desconocido
    )
  }
  if (!length(pares)) return(resultado())
  # Las entradas de cada lado de cada par, apareadas por (par, metrica) con un
  # solo `match()`: una corrida es el lado anterior de un par a lo sumo, y el
  # actual de otro, dentro de una serie.
  largo <- .sello_largo(textos)
  lado <- function(filas) {
    en <- largo[largo$fila %in% filas[pares], , drop = FALSE]
    par <- pares[match(en$fila, filas[pares])]
    en$clave_par <- paste(par, en$clave, sep = "\034")
    en$par <- par
    en
  }
  lado_a <- lado(anterior)
  lado_b <- lado(actual)
  ib <- match(lado_a$clave_par, lado_b$clave_par)
  comunes <- !is.na(ib)
  lado_a <- lado_a[comunes, , drop = FALSE]
  ib <- ib[comunes]
  huella_a <- lado_a$huella
  huella_b <- lado_b$huella[ib]
  incierta <- huella_a %in% .HUELLA_NO_CERTIFICABLE |
    huella_b %in% .HUELLA_NO_CERTIFICABLE
  distinta <- !incierta & huella_a != huella_b
  parcial <- !incierta & !distinta &
    (lado_a$certificacion == "parcial" | lado_b$certificacion[ib] == "parcial")
  if (any(incierta)) {
    por_par <- split(lado_a$clave[incierta], lado_a$par[incierta])
    for (p in names(por_par)) inciertas[[as.integer(p)]] <- .desescapar_clave(por_par[[p]])
  }
  if (any(parcial)) {
    por_par <- split(lado_a$clave[parcial], lado_a$par[parcial])
    for (p in names(por_par)) parciales[[as.integer(p)]] <- .desescapar_clave(por_par[[p]])
  }
  if (any(distinta)) {
    i <- which(distinta)
    nombres <- .desescapar_clave(lado_a$clave[i])
    textos_evidencia <- paste0(
      "`", nombres, "`: huella ", .huella_corta(huella_a[i]), " (lupa ",
      .desescapar_clave(lado_a$version[i]), ") -> ", .huella_corta(huella_b[i]),
      " (lupa ", .desescapar_clave(lado_b$version[ib[i]]), ")"
    )
    por_par <- split(seq_along(i), lado_a$par[i])
    for (p in names(por_par)) {
      k <- as.integer(p)
      cambiada[[k]] <- TRUE
      metricas[[k]] <- nombres[por_par[[p]]]
      evidencia[[k]] <- paste(textos_evidencia[por_par[[p]]], collapse = "; ")
    }
  }
  resultado()
}
