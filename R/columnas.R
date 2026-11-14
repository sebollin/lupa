# Una columna con MAS DE UNA DIMENSION -una matriz o un arreglo guardados en un
# `data.frame` con `I()`- no es una columna de valores por fila, y se resume
# aparte. La regla estaba escrita dos veces y las dos versiones NO coincidian:
# aca se preguntaba `is.matrix(x)`, que es FALSE para un arreglo de tres
# dimensiones, mientras `perfilar()` si contaba los arreglos. Consecuencia
# medida: sobre una tabla de CUATRO filas, la columna de un arreglo 4x2x2
# publicaba `n = 16` -los elementos del arreglo-, `n_distintos = 16`,
# `minimo 1`, `maximo 16`, y de ahi salia un hallazgo `posible_identificador`.
# Una cifra cuya unidad no es la fila, publicada al lado del conteo de filas de
# la tabla.
#
# Una columna `data.frame` anidada -la que deja `jsonlite::fromJSON()` o un
# tibble empaquetado con `tidyr::pack()`- es el caso hermano que la regla no
# enumeraba: no es matriz ni arreglo, caia por el camino comun, y `is.na()`
# sobre ella devuelve una matriz de CELDAS. Medido: doce filas con 27 celdas
# ausentes publicaban `n_faltantes = 27`, `prop_faltantes = 2.25` y una traza
# con indices hasta 34, filas que la tabla no tiene. Es compuesta por la misma
# razon que la matriz: varios valores por fila, sin una semantica de celda que
# el paquete pueda inventar.
.es_arreglo_compuesto <- function(x) {
  is.matrix(x) || (is.array(x) && length(dim(x)) > 1L)
}
.es_columna_compuesta <- function(x) {
  .es_arreglo_compuesto(x) || is.data.frame(x) || .es_columna_sin_escritura(x)
}

# Una columna cuyos valores no se pueden escribir como texto entra por la misma
# puerta que la matriz y la tabla anidada: el paquete no tiene un valor por fila
# que leer, y cualquier escritura que eligiera inventaria el dato. El caso medido
# es la `vctrs_rcrd` -varios campos por fila, como una tabla
# anidada sin serlo-, sobre la que `as.character()` y `format()` abortan, igual
# que sobre un `vctrs_vctr` sin conversion declarada. Abortaba 22 de las 29
# llamadas del catalogo desde tres sitios distintos -los formatos de fecha, el
# clasificador de datos personales y la identidad de las relaciones-, porque el
# paquete solo trataba como lista lo que sobrevivia a `as.character()`. Tampoco
# se lee como numero (`as.double()` aborta igual); y aunque una clase se dejara
# leer como numero, las etapas de texto abortarian sobre ella: lo que decide es
# la escritura.
#
# Se pregunta por la PROPIEDAD y no por la clase, una vez por columna, y con eso
# las etapas que ya preguntan `.es_columna_compuesta()` la declaran
# `tipo_compuesto_no_analizado` en vez de tocarla. La prueba es sobre el prefijo
# vacio: la conversion de estas clases se decide por el tipo y no por los
# valores, asi que cuesta lo mismo sobre diez filas que sobre diez millones.
# Las clases de base que siempre se escriben no pagan ni eso.
#
# Una clase sin metodo de `[` lo pierde al recortar -`x[0L]` es un `double`
# comun-, y la pregunta es sobre la clase: el prefijo vacio se arma con sus
# atributos. Si ni eso se puede armar, no se decide: la columna sigue por el
# camino de siempre.
.es_columna_sin_escritura <- function(x) {
  if (!is.object(x) || is.data.frame(x)) return(FALSE)
  if (is.factor(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
    return(FALSE)
  }
  vacia <- tryCatch(x[0L], error = function(e) NULL)
  if (is.null(vacia) || !identical(oldClass(vacia), oldClass(x))) {
    vacia <- tryCatch({
      atributos <- attributes(x)
      atributos[c("names", "dim", "dimnames", "row.names")] <- NULL
      prefijo <- unclass(x)[0L]
      attributes(prefijo) <- atributos
      prefijo
    }, error = function(e) NULL)
    if (is.null(vacia)) return(FALSE)
  }
  inherits(
    tryCatch(as.character(vacia), error = function(e) e), "error"
  )
}
.moda_columna <- function(x, claves = NULL) {
  validos <- !is.na(x)
  # Dos ausencias distintas, y la diferencia importa: sin valores validos la
  # frecuencia es cero porque se conto y no hay ninguno; sobre una columna de
  # listas no se pudo contar, y eso es NA. Confundirlas hacia que un hallazgo
  # informara cero afectados sobre datos validos.
  if (!any(validos)) {
    return(list(valor = NA_character_, frecuencia = 0L))
  }
  if (is.list(x)) {
    return(list(valor = NA_character_, frecuencia = NA_integer_))
  }
  # `integer64` tiene rama propia, y hace falta.
  #
  # `bit64` NO registra sus metodos S3 cuando solo esta cargado -medido:
  # `getS3method("as.character", "integer64", optional = TRUE)` devuelve `NULL`
  # dentro del espacio de nombres-. Ahi cualquier operacion de base sobre una
  # columna `integer64` la degrada a `double` y REINTERPRETA SUS BITS: `x[1]`,
  # `c()`, `unique()` y `unlist()` lo hacen. Medido sobre una columna con
  # 9007199254740992 y 9007199254740993, la moda publicada era
  # `4.4501477170144e-308`, que son los bits del primero leidos como `double`.
  # Con `bit64` adjunto en la sesion del usuario salia bien: el numero publicado
  # dependia de lo que el usuario tuviera cargado.
  #
  # Se trabaja con el texto exacto -que conserva el valor- y el desempate se
  # calcula aparte sobre el orden numerico, para no cambiar el criterio respecto
  # de una columna `double` equivalente.
  if (inherits(x, "integer64")) {
    if (!.bit64_disponible()) {
      return(list(valor = NA_character_, frecuencia = NA_integer_))
    }
    etiquetas <- bit64::as.character.integer64(x)
    etiquetas <- etiquetas[validos]
    if (!length(etiquetas)) {
      return(list(valor = NA_character_, frecuencia = 0L))
    }
    distintos <- unique(etiquetas)
    frecuencias <- tabulate(match(etiquetas, distintos), nbins = length(distintos))
    maximas <- which(frecuencias == max(frecuencias))
    if (length(maximas) > 1L) {
      numericos <- bit64::as.integer64(distintos[maximas])
      maximas <- maximas[.orden_por_bytes(bit64::rank.integer64(numericos))]
    }
    posicion <- maximas[[1L]]
    return(list(valor = distintos[[posicion]],
                frecuencia = as.integer(frecuencias[[posicion]])))
  }
  # Los valores invalidos no se pueden leer como texto, pero si se pueden
  # comparar por su representacion de bytes. La clave es solo para igualdad y
  # desempate: la moda publicada sigue siendo el valor almacenado original.
  if (is.character(x) || is.factor(x)) {
    if (is.null(claves)) claves <- .clave_bytes(x)
    validos_clave <- !is.na(x) & !is.na(claves)
    if (!any(validos_clave)) {
      return(list(valor = NA_character_, frecuencia = 0L))
    }
    sobre <- .mapa_distintos_bloques(claves)
    por_bloques <- .moda_mapa_distintos(sobre)
    if (!is.null(por_bloques)) {
      indice <- match(por_bloques$valor, claves, nomatch = 0L)
      if (length(indice) && indice[[1L]] > 0L) {
        return(list(
          valor = .texto_valor(x[indice[[1L]]]),
          frecuencia = por_bloques$frecuencia
        ))
      }
    }
    valores <- claves[validos_clave]
    unicos <- unique(valores)
    indices <- match(valores, unicos)
    frecuencias <- tabulate(indices, nbins = length(unicos))
    maximas <- which(frecuencias == max(frecuencias))
    if (length(maximas) > 1L) {
      orden <- .orden_seguro(unicos[maximas])
      if (length(orden) == length(maximas)) maximas <- maximas[orden]
    }
    posicion <- maximas[[1L]]
    indice <- which(validos_clave)[match(unicos[[posicion]], valores)]
    return(list(
      valor = .texto_valor(x[indice[[1L]]]),
      frecuencia = as.integer(frecuencias[[posicion]])
    ))
  }
  por_bloques <- .moda_mapa_distintos(.mapa_distintos_bloques(x))
  if (!is.null(por_bloques)) return(por_bloques)
  valores <- x[validos]
  unicos <- unique(valores)
  # `unique()` devuelve en orden de aparicion y `which.max()` toma el primer
  # maximo, asi que ante un empate la moda salia por el orden de las filas: la
  # columna 1,2,3,0,-4 daba moda 1, y la misma columna invertida daba -4. El
  # veredicto no puede depender de la forma fisica de la tabla.
  #
  # El desempate es por valor ascendente, que es el mismo criterio que ya usaba
  # la via DBI -`ORDER BY frecuencia DESC, columna ASC`- y estaba documentado
  # solo de aquel lado. Ahora las dos puertas dan la misma moda sobre los mismos
  # datos.
  #
  # `method = "radix"` en texto: el orden por omision depende de la
  # configuracion regional, y con eso el desempate cambiaria de una maquina a
  # otra. Es el mismo cuidado que en el recorte del vocabulario.
  # `order()` no sabe ordenar todos los tipos: sobre una columna de blobs
  # (`raw`) tira "unimplemented type 'raw' in 'orderVector1'". Se ordena por una
  # clave equivalente donde hace falta, y si aun asi no se puede, se deja el
  # orden de aparicion: es peor que ordenar, pero mucho mejor que romper el
  # perfil entero por una columna exotica.
  orden <- .orden_seguro(unicos)
  if (length(orden) == length(unicos)) unicos <- unicos[orden]
  indices <- match(valores, unicos)
  frecuencias <- tabulate(indices, nbins = length(unicos))
  posicion <- which.max(frecuencias)
  list(
    valor = .texto_moda(unicos[posicion]),
    frecuencia = as.integer(frecuencias[[posicion]])
  )
}

.resumen_longitud <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    return(c(minimo = NA_real_, maximo = NA_real_, media = NA_real_,
             n_resumidas = NA_real_))
  }
  .resumen_longitudes_bloques(x)
}

# Un entero escrito sin nada mas, partido como lo compara el paquete: sus cifras
# sin `+` ni ceros a la izquierda, si es negativo, y el texto que lo identifica,
# el de `.texto_identidad()` y de `integer64`. "-0" es el cero: no es negativo,
# y el extremo que lo tiene es "0". Contado como negativo, la columna
# `c("-0", "5", "9007199254740993")` publicaba `minimo_exacto = "-0"` y
# `n_negativos = 1`.
.partes_entero_texto <- function(texto) {
  sin_mas <- sub("^\\+", "", texto)
  digitos <- sub("^0+(?=[0-9])", "", sub("^-", "", sin_mas), perl = TRUE)
  negativos <- startsWith(sin_mas, "-") & digitos != "0"
  identidad <- digitos
  identidad[negativos] <- paste0("-", digitos[negativos])
  list(digitos = digitos, negativos = negativos, identidad = identidad)
}

# Un texto numerico que identifica un centinela declarado conserva ese valor y
# no el doble que resulte de leerlo. `R_strtod()` acumula las cifras en
# `long double`, y donde ese tipo es un doble -macOS sobre arm64- un entero de
# mas de 2^53 llega a otro doble: "1152921504606846976", que es 2^60, se leia
# 2^60 - 128. La identidad comun tambien cubre las formas regionales y la
# escritura corta de fracciones.
.centinelas_enteros_por_texto <- function(texto, valores,
                                          sentinelas_numericos,
                                          convencion = NULL,
                                          identidades = NULL) {
  if (!length(sentinelas_numericos) || length(texto) != length(valores)) {
    return(valores)
  }
  posicion <- identidades
  if (is.null(posicion)) {
    posicion <- .identidades_sentinelas_texto(
      texto, sentinelas_numericos, convencion, valores_leidos = valores
    )
  }
  cuales <- !is.na(posicion)
  valores[cuales] <- as.double(sentinelas_numericos[posicion[cuales]])
  valores
}

# Un entero escrito como TEXTO por encima de 2^53 no sobrevive al doble, y el
# resumen publicaba el redondeo como si fuera el dato: sobre la columna
# `c("9007199254740993", "9007199254740994", "9007199254740995")` -tres impares-
# se publicaba `minimo = 9007199254740992` y `maximo = 9007199254740996`, dos
# valores que NINGUNA fila contiene, con `estado_resumen_cuantitativo =
# "calculados"`. La decision ya estaba tomada para `integer64` -medidas en `NA`,
# extremos exactos como texto y `omitidos_precision`- y este camino la esquivaba
# por llegar como texto. Es la misma columna y ahora recibe la misma respuesta.
#
# Las comparaciones se hacen por longitud y con `method = "radix"`: comparar
# decimales con `<` depende del locale, y este paquete ya se quemo con eso.
#
# Los centinelas declarados salen antes de todo, con la regla de la rama
# numerica y de `integer64`: no entran en los extremos, los conteos ni la
# mediana, y se cuentan como excluidos. Se reconocen por el texto que
# identifica al numero (`.texto_identidad()`, la comparacion de `integer64`).
# Esta rama no los sacaba: con `-999` declarado publicaba `minimo_exacto =
# "-999"` y `n_negativos = 1`, mientras `integer64` con los mismos valores
# publicaba el menor de los otros. Si sin ellos ningun valor pasa de 2^53, la
# columna es la de un resumen comun y se devuelve `NULL`.
.extremos_enteros_texto_fuera_de_precision <- function(texto, tipo,
                                                       sentinelas_numericos =
                                                         numeric()) {
  if (!identical(as.character(tipo)[1L], "entero")) return(NULL)
  presentes <- texto[!is.na(texto) & nzchar(texto)]
  if (!length(presentes)) return(NULL)
  # Solo enteros escritos sin nada mas: si la columna trae decimales, notacion
  # cientifica o separadores, no es este caso y no se toca.
  if (!all(grepl("^[+-]?[0-9]+$", presentes))) return(NULL)
  partes <- .partes_entero_texto(presentes)
  negativos <- partes$negativos
  digitos <- partes$digitos
  n_sentinelas <- 0L
  if (length(sentinelas_numericos)) {
    centinela <- !is.na(.identidades_sentinelas_texto(
      presentes, sentinelas_numericos
    ))
    n_sentinelas <- as.integer(sum(centinela))
    negativos <- negativos[!centinela]
    digitos <- digitos[!centinela]
    if (!length(digitos)) return(NULL)
  }
  limite <- "9007199254740991"
  largo <- nchar(digitos)
  excede <- any(largo > nchar(limite))
  if (!excede) {
    iguales <- digitos[largo == nchar(limite)]
    if (length(iguales)) {
      mayor <- .sort_por_bytes(c(iguales, limite), method = "radix",
                    decreasing = TRUE)[[1L]]
      excede <- !identical(mayor, limite)
    }
  }
  if (!excede) return(NULL)
  mayor_magnitud <- function(v) {
    v[.orden_por_bytes(nchar(v), v, method = "radix", decreasing = TRUE)][[1L]]
  }
  menor_magnitud <- function(v) {
    v[.orden_por_bytes(nchar(v), v, method = "radix")][[1L]]
  }
  positivos <- digitos[!negativos]
  magnitudes_negativas <- digitos[negativos]
  maximo <- if (length(positivos)) {
    mayor_magnitud(positivos)
  } else {
    paste0("-", menor_magnitud(magnitudes_negativas))
  }
  minimo <- if (length(magnitudes_negativas)) {
    paste0("-", mayor_magnitud(magnitudes_negativas))
  } else {
    menor_magnitud(positivos)
  }
  # Las centrales por valor, con el mismo orden: los negativos de mayor a menor
  # magnitud y despues los demas de menor a mayor. La mediana la decide
  # `.mediana_centrales_exactas()`, como la de un `integer64`. Un "-0" ya es el
  # cero y va con los demas.
  debajo <- digitos[negativos]
  encima <- digitos[!negativos]
  # `paste0("-", character())` da "-": sin negativos no se pega nada.
  ordenados <- c(
    if (length(debajo)) paste0("-", debajo[.orden_por_bytes(
      nchar(debajo), debajo, method = "radix", decreasing = TRUE
    )]),
    encima[.orden_por_bytes(nchar(encima), encima, method = "radix")]
  )
  n <- length(ordenados)
  list(
    minimo_exacto = minimo, maximo_exacto = maximo,
    n_ceros = as.integer(sum(digitos == "0")),
    n_negativos = as.integer(sum(negativos)),
    centrales = ordenados[unique(c((n + 1L) %/% 2L, (n + 2L) %/% 2L))],
    n_sentinelas = n_sentinelas
  )
}

.valores_cuantitativos <- function(x, inferencia, formatos,
                                   meses_texto = NULL, vocabulario = NULL,
                                   valores_preparados = NULL,
                                   sentinelas_numericos = numeric()) {
  if (inherits(x, "POSIXt")) {
    return(list(valores = as.numeric(x), clase = "fecha-hora",
                n_fechas_resumidas = sum(is.finite(x)),
                n_fechas_excluidas_granularidad = 0L))
  }
  if (inherits(x, "Date")) {
    # `is.finite()` y no `!is.na()`: una fecha puede ser `Inf` -`f[2] <- Inf` sobre
    # un `Date` alcanza- y el resumen la deja afuera, asi que contarla como
    # resumida publica un universo que no se midio. La rama `POSIXt` de arriba ya
    # usaba `is.finite()`: era la misma pregunta contestada con dos expresiones, y
    # sobre el mismo dato una decia 2 y la otra 3.
    return(list(valores = as.numeric(x) * 86400, clase = "fecha",
                n_fechas_resumidas = sum(is.finite(x)),
                n_fechas_excluidas_granularidad = 0L))
  }
  if (inherits(x, "integer64")) {
    return(list(valores = x, clase = "integer64",
                n_fechas_resumidas = NA_integer_,
                n_fechas_excluidas_granularidad = NA_integer_))
  }
  # `difftime` se abstenia del resumen entero -todo en `NA` y
  # `estado_resumen_cuantitativo = "no_aplica"`-, y el motivo escrito era que sin
  # una unidad publicada no se publica el numero. Con el campo `unidad` ese motivo
  # ya no vale: la fila publica `days`, `hours` o lo que la columna declare al lado
  # de sus cifras.
  #
  # Era ademas el unico que se abstenia: `Date` y `POSIXt` publican `desvio` en
  # segundos desde antes. Seguir absteniendose era la inconsistencia, no la
  # prudencia. `is.numeric()` da FALSE sobre un `difftime`, asi que sin esta rama
  # caia hasta `clase = "ninguna"`.
  #
  # Los valores van en la unidad DECLARADA por la columna, sin convertir a
  # segundos: convertirlos publicaria un numero que no esta en la columna, y la
  # unidad que viaja es la que `.unidad_del_resumen()` lee del mismo atributo.
  if (inherits(x, "difftime")) {
    return(list(valores = as.numeric(x), clase = "numero",
                n_fechas_resumidas = NA_integer_,
                n_fechas_excluidas_granularidad = NA_integer_,
                n_valores_excluidos_resumen = 0L,
                estado = "calculados"))
  }
  if (is.numeric(x)) {
    return(list(valores = as.numeric(x), clase = "numero",
                n_fechas_resumidas = NA_integer_,
                n_fechas_excluidas_granularidad = NA_integer_,
                n_valores_excluidos_resumen = 0L,
                estado = "calculados"))
  }
  if (is.character(x) || is.factor(x)) {
    if (inferencia$tipo %in% c("entero", "doble")) {
      texto <- trimws(as.character(x))
      # Con la convencion decimal que el perfil publica para la columna, no con
      # un `sub(",", ".")` propio: ver `.leer_numeros_texto()`. Lo que esa
      # convencion no puede leer se cuenta abajo como excluido.
      valores <- .leer_numeros_texto(
        texto, inferencia[["convencion_numero_texto"]]
      )
      identidades_sentinelas <- .identidades_sentinelas_texto(
        texto, sentinelas_numericos,
        inferencia[["convencion_numero_texto"]], valores_leidos = valores
      )
      valores <- .centinelas_enteros_por_texto(
        texto, valores, sentinelas_numericos,
        inferencia[["convencion_numero_texto"]], identidades_sentinelas
      )
      mascara_sentinelas_texto <- !is.na(identidades_sentinelas)
      if (any(mascara_sentinelas_texto)) {
        valores[mascara_sentinelas_texto] <- as.double(
          sentinelas_numericos[identidades_sentinelas[mascara_sentinelas_texto]]
        )
      }
      # Los textos presentes que no llegan a numero no son NA de origen y no
      # pueden desaparecer sin dejar alcance.
      #
      # Esto NO depende del muestreo, y estaba escrito como si dependiera: la
      # rama tenia un `else 0L` literal, heredado de que el pendiente que la
      # origino hablaba de `muestra`. La conversion descarta lo que no puede
      # leer haya muestra o no. Medido sobre mil valores -900 que dicen "100",
      # sesenta "1.234,5" y cuarenta "no-numero"-, la MISMA columna informaba
      # `n_valores_excluidos_resumen = 0` con estado `calculados` sin muestrear,
      # y 100 con `calculados_sobre_valores` con `muestra = 50`. Los cien
      # valores quedaban afuera en los dos casos; lo unico que cambiaba era si
      # se decia.
      # `!is.na(x)` y no `nzchar(texto)`: `trimws()` convierte `" "` en `""`, y el
      # `nzchar()` descartaba los blancos de la cuenta. Un blanco NO es una
      # ausencia declarada -el paquete lo cuenta en `n_blancos`, lo sospecha en
      # `n_faltantes_disfrazados` y deja `n_faltantes` en cero-, asi que su
      # exclusion se declara como cualquier otra. Medido sobre mil filas -900
      # que dicen "10" y 100 en blanco-: la media publicada era 10 sobre 900
      # valores, con `n_valores_excluidos_resumen = 0` y estado `calculados`,
      # mientras los mismos 100 como texto ilegible si se declaraban.
      #
      # `NA` sigue afuera de la cuenta, y debe: ahi la ausencia esta declarada y
      # se informa en `n_faltantes`, que es su lugar.
      n_excluidas <- as.integer(sum(
        !is.na(x) & is.na(valores) & !is.nan(valores)
      ))
      return(list(valores = valores, clase = "numero",
                  n_fechas_resumidas = NA_integer_,
                  n_fechas_excluidas_granularidad = NA_integer_,
                  n_valores_excluidos_resumen = n_excluidas,
                  mascara_sentinelas_texto = mascara_sentinelas_texto,
                  fuera_de_precision =
                    .extremos_enteros_texto_fuera_de_precision(
                      texto, inferencia$tipo, sentinelas_numericos
                    ),
                  estado = if (n_excluidas > 0L) {
                    "calculados_sobre_valores"
                  } else "calculados"))
    }
    if (inferencia$tipo %in% c("fecha", "fecha-hora")) {
      muestreado <- isTRUE(inferencia$muestreado)
      granularidades <- if (is.data.frame(formatos) &&
          "granularidad" %in% names(formatos)) {
        formatos$granularidad[formatos$estado == "confirmado"]
      } else character()
      tiene_mes <- any(granularidades == "mes")
      tiene_dia <- any(granularidades == "dia")
      # `granularidad_incompleta` esta definido para "una columna de periodos
      # expresados SOLO como mes y ano", y se aplicaba mirando unicamente los
      # formatos CONFIRMADOS: una columna con tres fechas `03/04/2023` -dia,
      # ambiguas, candidatas- y un `01/2023` caia aca y publicaba ese estado,
      # culpando a la granularidad de un bloqueo que era de la ambiguedad
      # dia/mes. Si hay formatos de dia aunque sean candidatos, la columna no es
      # de periodos mensuales y sigue el camino general, donde lo que no se pudo
      # leer se cuenta y se declara.
      granularidades_candidatas <- if (is.data.frame(formatos) &&
          all(c("granularidad", "estado") %in% names(formatos))) {
        formatos$granularidad[formatos$estado == "candidato"]
      } else character()
      tiene_dia_candidato <- any(granularidades_candidatas == "dia")
      if (tiene_mes && !tiene_dia && !tiene_dia_candidato) {
        valores <- trimws(as.character(x))
        presentes <- !is.na(valores) & nzchar(valores)
        return(list(
          valores = numeric(), clase = "fecha_granularidad_incompleta",
          n_fechas_resumidas = 0L,
          n_fechas_excluidas_granularidad = if (muestreado) {
            as.integer(sum(presentes))
          } else {
            sum(formatos$n[formatos$estado == "confirmado" &
              formatos$granularidad == "mes"], na.rm = TRUE)
          },
          # El campo general cuenta TODO valor presente que no sostiene el
          # resumen -la documentacion lo dice con esas palabras, "sea un periodo
          # de mes o un texto que ningun formato pudo leer"- y aca quedaba en
          # `NA`: una columna de periodos mensuales publicaba `NA` excluidos
          # mientras `n_fechas_excluidas_granularidad` contaba tres. No depende
          # del muestreo: en esta rama ningun valor presente entra al resumen.
          n_valores_excluidos_resumen = as.integer(sum(presentes))
        ))
      }
      fechas <- .parsear_fechas(
        x, formatos, meses_texto = meses_texto, vocabulario = vocabulario,
        valores_preparados = valores_preparados
      )
      excluidas <- if (tiene_mes) {
        sum(formatos$n[formatos$estado == "confirmado" &
          formatos$granularidad == "mes"], na.rm = TRUE)
      } else 0L
      valores <- trimws(as.character(x))
      # Un blanco es un valor PRESENTE que no llego a fecha, igual que un texto
      # ilegible: `trimws()` lo deja en `""` y el `nzchar()` lo descartaba de la
      # cuenta. No es una ausencia declarada -eso es `NA`, y se informa en
      # `n_faltantes`-. Mismo arreglo que en la conversion a numero.
      presentes <- !is.na(x)
      # Todo lo que esta presente y no llego a fecha: los periodos de mes, que
      # no se convierten a proposito, y lo que ningun formato pudo leer. No
      # depende del muestreo -la conversion descarta igual- y por eso ya no se
      # calcula solo cuando `muestreado`.
      no_entraron <- as.integer(sum(presentes & is.na(fechas)))
      return(list(
        valores = as.numeric(fechas), clase = inferencia$tipo,
        # El estado nombra la RAZON, y `calculados_sobre_dias` nombra una:
        # quedaron afuera periodos de mes. Cuando no hay ninguno y lo que quedo
        # afuera son valores que ningun formato pudo leer -blancos, texto
        # ilegible-, la razon es otra y se usa el mismo vocabulario que la
        # conversion a numero. Antes las dos decian `calculados_sobre_dias`, y
        # quien lo leyera buscaba una granularidad que no existia.
        estado = if (tiene_mes) {
          "calculados_sobre_dias"
        } else if (no_entraron > 0L) {
          "calculados_sobre_valores"
        } else "calculados",
        n_fechas_resumidas = sum(is.finite(fechas)),
        # Los dos campos cuentan cosas distintas, y hubo una tanda en que
        # contaron la misma: con muestra, los fallos de PARSEO se sumaban al
        # campo que se llama `granularidad`, asi que una columna con noventa
        # fechas solo-mes y diez ilegibles publicaba cien "excluidas por
        # granularidad" y quien lo leyera buscaba una granularidad que explicaba
        # noventa de esos cien.
        #
        # `n_fechas_excluidas_granularidad` cuenta lo que su nombre dice: las
        # fechas que quedaron fuera por ser de granularidad mes.
        #
        # Con muestra no se puede: ese numero sale de `formatos$n`, que esta
        # contado SOBRE LA MUESTRA, mientras el resumen corre sobre la columna
        # entera. Publicarlo daria 4 donde son 90. Atribuir la causa exigiria
        # rehacer la deteccion sobre todo, que es justo el costo que el muestreo
        # existe para evitar. Entonces se informa `NA`, siguiendo la regla que el
        # paquete ya declara en su `\value`: cuando el camino no puede conocer un
        # conteo, informa NA y nunca cero. El total sigue estando, sin atribuir,
        # en `n_valores_excluidos_resumen`.
        n_fechas_excluidas_granularidad = if (muestreado) {
          NA_integer_
        } else {
          excluidas
        },
        # `n_valores_excluidos_resumen` es el campo general del paquete para
        # "valores presentes que no sostienen el resumen", cualquiera sea la
        # causa; la granularidad es un subconjunto suyo.
        n_valores_excluidos_resumen = no_entraron
      ))
    }
  }
  list(valores = numeric(), clase = "ninguna",
       n_fechas_resumidas = NA_integer_,
       n_fechas_excluidas_granularidad = NA_integer_,
       n_valores_excluidos_resumen = NA_integer_)
}

# Cuantas veces el hueco mas grande tiene que superar al tipico para que el
# valor que lo abre deje de ser cola y pase a ser anomalia.
#
# El numero no se eligio: se barrio. Sobre un banco de quince columnas con la
# respuesta conocida, contando por separado los dos errores -callar un dato malo
# cuesta mas que hablar de mas-, la meseta sin errores va de 10 a 50:
#
#     6, 8    ->  habla de mas 2 veces
#     10 a 50 ->  sin errores
#     75 o mas -> calla un dato malo
#
# Estaba en 10, pegado al borde inferior. 20 queda dentro de la meseta con
# margen de los dos lados, y del lado que importa -callar- el margen es mayor.
# Los otros dos umbrales se barrieron igual: la densidad no da errores entre 0,2
# y 0,6 y esta en 0,5; el minimo de distintos no da errores entre 3 y 15 y esta
# en 5. Los tres estan en el centro de su meseta, no en un borde.
.FACTOR_SALTO_ESCALA <- 20

# La otra via: el hueco mas grande como proporcion del rango. Las numeraciones
# reales del banco no pasan de 0,067 y los casos a informar arrancan en 0,409.
.PROPORCION_SALTO_ESCALA <- 0.25

.resumen_secuencia_entera <- function(x, inferencia, formatos,
                                      umbral_densidad = 0.8,
                                      min_distintos = 20L,
                                      cuantitativos = NULL) {
  vacio <- list(
    densa = FALSE, densidad = NA_real_, n_posiciones = NA_real_,
    n_huecos = NA_real_, hueco_maximo = NA_real_, salto_de_escala = FALSE,
    moda_sobresale = FALSE, no_evaluada = FALSE,
    motivo_no_evaluada = NA_character_,
    umbral_densidad = umbral_densidad,
    min_distintos = as.integer(min_distintos)
  )
  # Se miraba el tipo inferido y solo se aceptaba `entero`, pero unas lineas mas
  # abajo se comprueba que todos los valores sean enteros, que es la condicion
  # real. La del tipo dejaba afuera a las columnas guardadas como `double` con
  # valores enteros -que es como llegan casi todas por la puerta DBI, y como las
  # devuelven varios lectores de CSV-, asi que el mismo codigo se describia
  # distinto segun como estuviera almacenado. Medido: un codigo 1..284 con 179
  # valores fuera de los limites de Tukey se callaba como `integer` y se
  # senalaba como `double`.
  # `integer64` es la TERCERA forma de guardar lo mismo, y quedo afuera cuando
  # se agrego `doble` a esta lista. Medido: la columna `1:1000` se publica como
  # `posible_identificador` guardada en `entero` o en `double`, y como
  # `faltantes_disfrazados` guardada en `integer64` -porque sin secuencia densa
  # detectada, el `999` que trae la lista por omision de centinelas deja de estar
  # protegido por "esto es una numeracion"-. El mismo dato, dos hallazgos
  # distintos, decididos por el almacenamiento.
  #
  # Se creia que esto no podia depender de `bit64` porque no esta en Imports:
  # alcanzaria con el texto de `inferencia$tipo`. La medicion demostro lo
  # contrario: `as.numeric()` sobre un `integer64` sin sus metodos S3
  # registrados reinterpreta los bits y convierte 1, 2, 3... en denormales.
  # `requireNamespace()` registra esos metodos aunque el paquete no este
  # adjunto. Si no esta instalado, la secuencia queda declarada como no
  # evaluada en vez de publicar un `FALSE` inventado.
  # Los valores se llevan a `double` para medir la secuencia; con
  # identificadores por encima de 2^53 eso perderia precision, y por eso se
  # comprueba antes -si no entran, no se mide y se declara vacio, que es lo que
  # corresponde: no se inventa una densidad sobre numeros redondeados-.
  if (!as.character(inferencia$tipo) %in% c("entero", "doble", "integer64")) {
    return(vacio)
  }
  if (is.null(cuantitativos)) {
    cuantitativos <- .valores_cuantitativos(x, inferencia, formatos)
  }
  if (!identical(cuantitativos$clase, "numero") &&
      !identical(cuantitativos$clase, "integer64")) {
    return(vacio)
  }
  if (identical(cuantitativos$clase, "integer64") &&
      !.bit64_disponible()) {
    vacio$densa <- NA
    vacio$salto_de_escala <- NA
    vacio$moda_sobresale <- NA
    vacio$no_evaluada <- TRUE
    vacio$motivo_no_evaluada <-
      "No se pudo evaluar la secuencia entera porque falta el paquete opcional 'bit64'."
    return(vacio)
  }
  presentes <- !is.na(x)
  valores <- cuantitativos$valores
  if (identical(cuantitativos$clase, "integer64")) {
    finitos <- valores[!is.na(valores)]
    # El aviso de `bit64` -"integer precision lost while converting to double"-
    # se silencia a proposito: la conversion es la MEDICION, no un descuido.
    # Se esta preguntando si estos valores entran en un `double`, y si no
    # entran no se mide. Dejarlo salir haria que la comprobacion avise en el
    # unico caso donde se porta bien.
    if (length(finitos) &&
        any(suppressWarnings(abs(as.numeric(finitos))) > 2^53)) {
      # No se mide, y eso se DECLARA, igual que cuando falta `bit64`: devolvia
      # `vacio` y publicaba tres veredictos `FALSE` -"no es densa", "sin salto",
      # "sin moda sobresaliente"- sobre una numeracion que podia ser densa de
      # verdad, sin fila en la cobertura. Medido con 9e18 + 1..1000.
      vacio$densa <- NA
      vacio$salto_de_escala <- NA
      vacio$moda_sobresale <- NA
      vacio$no_evaluada <- TRUE
      vacio$motivo_no_evaluada <- paste(
        "No se evalu\u00f3 la secuencia entera: hay valores por encima de 2^53, que",
        "en un doble se redondean, y no se mide una densidad sobre n\u00fameros",
        "redondeados."
      )
      return(vacio)
    }
    valores <- suppressWarnings(as.numeric(valores))
  }
  if (!any(presentes) || length(valores) != length(x) ||
      any(!is.finite(valores[presentes])) ||
      any(valores[presentes] != floor(valores[presentes]))) {
    return(vacio)
  }
  distintos <- .sort_por_bytes(unique(valores[presentes]))
  if (!length(distintos)) return(vacio)
  n_posiciones <- max(distintos) - min(distintos) + 1
  if (!is.finite(n_posiciones) || n_posiciones < 1) return(vacio)
  densidad <- length(distintos) / n_posiciones
  # Un valor separado del resto por un hueco desproporcionado no es la cola de
  # la numeracion: es una anomalia, y es justo lo que hay que mirar. Un `120`
  # entre edades de 18 a 70 deja un hueco de 50 donde el tipico es 1; un `2000`
  # detras de 1..1000 deja uno de 1.000. Sin esta medida, la densidad sola
  # tapaba los dos casos, porque un valor fuera de escala de hasta el doble del
  # maximo no baja la densidad lo suficiente.
  # La densidad cuenta la COBERTURA del rango e ignora cuantas veces aparece
  # cada valor: mil valores distintos sobre mil posiciones dan densidad 1
  # aunque uno de ellos aparezca ciento una veces, que es exactamente la firma
  # de un centinela escondido en una numeracion. Lo que separa un centinela de
  # una clave foranea legitima no es el TAMANO de la moda -las claves foraneas
  # tienen modas grandes y legitimas- sino su FORMA: si sobresale del resto de
  # la distribucion. Se usa el criterio que el paquete ya tiene decidido para
  # el caso hermano en `concentracion.R`, en vez de inventar otro umbral.
  # Medido sobre las diez columnas numericas reales del banco, el mayor
  # cociente es 1,71 -`zip`-, con `brewery_id` en 1,63; el caso que hay que
  # atrapar da 101. El factor 5 queda en el medio con margen a los dos lados.
  frecuencias <- tabulate(
    match(valores[presentes], distintos), nbins = length(distintos)
  )
  # Que un valor SOBRESALGA de la distribucion se mide por el SALTO entre
  # frecuencias consecutivas, ordenadas de mayor a menor, mirando solo las
  # primeras posiciones. Es el mismo idioma que `salto_de_escala` usa unas
  # lineas abajo con los huecos: lo que delata a un centinela no es que su
  # frecuencia sea grande sino que haya un ACANTILADO entre el grupo que
  # sobresale y el resto.
  #
  # Tres reglas alternativas no sirven, y cada una tiene un caso que la rompe.
  # Contra el segundo valor: derrotada por un senuelo legitimo que
  # infla el segundo puesto -{6, 5, 1, ...} da 1,2-. Contra la frecuencia
  # tipica exigiendo forma de numeracion: derrotada por un centinela masivo
  # sobre una clave foranea -{100, 4, 4, ...}, mediana 4-. Y la disyuncion de
  # las dos: derrotada por VARIOS centinelas empatados, que se cubren entre si
  # en el segundo puesto -{499, 499, 499, 2, ...} deja `max == segundo` y la
  # comparacion vacua, callando mil cuatrocientos noventa y siete valores-.
  #
  # El acantilado ve los tres, porque no le importa quien es el segundo sino
  # donde se corta la distribucion. Medido sobre los ataques conocidos y las
  # diez columnas numericas del banco real mas dos formas de clave foranea:
  # los ataques dan entre 4,75 y 249,50 y todo lo legitimo entre 1,00 y 1,71.
  #
  # Se miran solo las primeras posiciones porque el grupo que sobresale esta
  # arriba; en la cola una caida de 2 a 1 da un salto de 2 que no dice nada, y
  # sin ese tope `beers$abv` daba 3,00 por una caida a mitad de su cola.
  ordenadas <- .sort_por_bytes(frecuencias, decreasing = TRUE)
  salto_frecuencias <- 1
  if (length(ordenadas) >= 2L) {
    razones <- ordenadas[-length(ordenadas)] /
      pmax(1, ordenadas[-1L])
    razones <- razones[seq_len(min(length(razones), .TOPE_SALTO_FRECUENCIAS))]
    if (length(razones)) salto_frecuencias <- max(razones)
  }
  moda_sobresale <- length(distintos) >= 3L &&
    isTRUE(salto_frecuencias >= .FACTOR_VALOR_CONCENTRADO)
  huecos <- if (length(distintos) > 1L) diff(distintos) else numeric()
  hueco_tipico <- if (length(huecos)) stats::median(huecos) else NA_real_
  if (!is.finite(hueco_tipico) || hueco_tipico <= 0) hueco_tipico <- 1
  hueco_maximo <- if (length(huecos)) max(huecos) else 0
  list(
    # `densa` responde si los distintos cubren suficientemente su rango Y casi
    # no se repiten: cubrir el rango sola la cumple tambien una magnitud entera
    # que se cargo con filas suficientes (`.numeracion_sin_repetir_conteos()`,
    # en `hallazgos.R`). La forma de la frecuencia es otra señal: la guarda de
    # centinelas la consulta, pero los escudos de numeración no deben depender
    # de que una moda legítima sobresalga, y por eso la regla descuenta el
    # excedente de la moda.
    densa = length(distintos) >= min_distintos &&
      densidad >= umbral_densidad &&
      .numeracion_sin_repetir_conteos(
        length(distintos), sum(frecuencias), max(frecuencias)
      ),
    densidad = as.numeric(densidad),
    n_posiciones = as.numeric(n_posiciones),
    n_huecos = as.numeric(n_posiciones - length(distintos)),
    hueco_maximo = as.numeric(hueco_maximo),
    hueco_tipico = as.numeric(hueco_tipico),
    # Hay salto de escala por dos vias, y hacen falta las dos. La primera
    # compara el hueco con el tipico, y se le escapa una columna de pocos
    # valores: puntajes del 1 al 13 con un 22 repetido veinte veces abren un
    # hueco de 9 sobre un tipico de 1, que no llega al factor y deja la columna
    # clasificada como numeracion, callando veinte valores extremos.
    #
    # La segunda mira el hueco contra el RANGO, y ahi la separacion es limpia:
    # las numeraciones reales llegan a 0,067 -el mayor es un codigo de 1 a 15- y
    # los casos que hay que informar empiezan en 0,409. Entre medio no hay
    # ninguno, asi que cualquier umbral entre 0,10 y 0,40 da cero errores sobre
    # el banco; 0,25 queda en el centro de esa meseta.
    salto_de_escala = length(distintos) >= 3L && hueco_maximo > 1 &&
      (hueco_maximo >= .FACTOR_SALTO_ESCALA * hueco_tipico ||
         hueco_maximo / n_posiciones >= .PROPORCION_SALTO_ESCALA),
    moda_sobresale = moda_sobresale,
    umbral_densidad = umbral_densidad,
    min_distintos = as.integer(min_distintos)
  )
}

.valores_numericos_secuencia <- function(x, inferencia, formatos,
                                         cuantitativos = NULL) {
  if (!as.character(inferencia$tipo) %in%
      c("entero", "doble", "integer64")) {
    return(numeric())
  }
  if (is.null(cuantitativos)) {
    cuantitativos <- tryCatch(
      .valores_cuantitativos(x, inferencia, formatos),
      error = function(e) NULL
    )
  }
  if (is.null(cuantitativos) ||
      !(cuantitativos$clase %in% c("numero", "integer64"))) {
    return(numeric())
  }
  if (identical(cuantitativos$clase, "integer64") &&
      !.bit64_disponible()) {
    return(numeric())
  }
  suppressWarnings(as.numeric(cuantitativos$valores))
}

.candidatos_sentinelas_presentes <- function(valores, sentinelas_numericos) {
  candidatos <- .numeros_na(sentinelas_numericos)
  presentes <- !is.na(valores) & is.finite(valores)
  if (!length(candidatos) || !any(presentes)) return(numeric())
  candidatos[vapply(candidatos, function(candidato) {
    any(presentes & valores == candidato)
  }, logical(1L))]
}

.rango_numeracion_sin_candidatos <- function(valores, candidatos) {
  presentes <- !is.na(valores) & is.finite(valores)
  if (!length(candidatos) || !any(presentes)) return(NULL)
  restantes <- valores[presentes & !(valores %in% candidatos)]
  if (!length(restantes)) return(NULL)
  range(restantes)
}

.sentinela_numerico_fuera_rango <- function(candidatos, rango) {
  if (is.null(rango) || !length(candidatos)) return(FALSE)
  any(candidatos < rango[[1L]] | candidatos > rango[[2L]])
}

# La guarda se decide POR CANDIDATO, no por columna. Decidirla por columna fue
# el defecto siguiente: en `c(1:1500, rep(1501:1505, each = 200), rep(-9, 200))`
# el `-9` cae fuera del rango y abre la guarda con razon, pero una vez abierta se
# marcaba tambien el `999` que la propia secuencia trae UNA vez, en su lugar. Dos
# candidatos con evidencia opuesta compartian un solo interruptor.
#
# Un candidato entra si muestra alguna senal propia: cae **fuera del rango** de la
# numeracion -un `-9` en una numeracion de 1 a 1505- o su frecuencia **sobresale**
# del resto. Un valor de la lista que aparece dentro del rango y sin destacarse no
# es sospechoso de nada: es un numero mas de la numeracion.
.sentinelas_que_abren_guarda <- function(valores, candidatos, rango,
                                         secuencia_entera) {
  if (!length(candidatos)) return(numeric())
  presentes <- !is.na(valores) & is.finite(valores)
  if (!any(presentes)) return(numeric())
  fuera <- if (is.null(rango)) {
    rep(FALSE, length(candidatos))
  } else {
    candidatos < rango[[1L]] | candidatos > rango[[2L]]
  }
  sobresale <- rep(FALSE, length(candidatos))
  if (isTRUE(secuencia_entera$moda_sobresale)) {
    distintos <- .sort_por_bytes(unique(valores[presentes]))
    frecuencias <- tabulate(match(valores[presentes], distintos),
                            nbins = length(distintos))
    tope <- max(frecuencias)
    sobresale <- vapply(candidatos, function(candidato) {
      sum(presentes & valores == candidato) == tope
    }, logical(1L))
  }
  candidatos[fuera | sobresale]
}

# Una sola decision para las dos puertas que miran una numeracion: el resumen
# de la columna y la cobertura de `perfilar_por()`. El grupo no debe volver a
# inventar si la guarda de la columna entera estaba cerrada.
.decision_guarda_numeracion <- function(valores, secuencia_entera,
                                        sentinelas_numericos,
                                        sentinelas_declarados =
                                          .sentinelas_numericos_declarados(
                                            sentinelas_numericos
                                          )) {
  candidatos <- .candidatos_sentinelas_presentes(
    valores, sentinelas_numericos
  )
  rango <- .rango_numeracion_sin_candidatos(valores, candidatos)
  efectivos <- if (isTRUE(secuencia_entera$densa) &&
                   !length(sentinelas_declarados)) {
    .sentinelas_que_abren_guarda(
      valores, candidatos, rango, secuencia_entera
    )
  } else if (!length(sentinelas_declarados)) {
    .sentinelas_sin_apariciones_unicas(
      valores, candidatos, rango, sentinelas_numericos
    )
  } else {
    sentinelas_numericos
  }
  list(
    candidatos = candidatos,
    rango = rango,
    sentinelas_efectivos = efectivos,
    detectar_sentinelas = length(sentinelas_declarados) > 0L ||
      !isTRUE(secuencia_entera$densa) || length(efectivos) > 0L,
    guarda_cerrada = isTRUE(secuencia_entera$densa) &&
      !length(.sentinelas_que_abren_guarda(
        valores, candidatos, rango, secuencia_entera
      ))
  )
}

# Las tres decisiones que `perfilar()` toma mirando la columna entera y que
# `perfilar_por()` comparte con los grupos: la secuencia y su guarda de
# numeracion, el centinela conjeturado y el identificador para Benford. Se
# arman con las mismas piezas que `.perfilar_columna()`, no con una copia. Los
# valores que se leen como numero pueden venir guardados como texto o factor;
# la conversion pasa por la misma inferencia y la misma convencion decimal que
# el perfil. No devuelve nada mas: lo que no es de esas tres decisiones lo
# decide cada grupo con su rebanada.
.contexto_decisiones_columna <- function(
    x, sentinelas_numericos, aplicable = NULL, muestra = 1e5,
    clave_declarada = FALSE) {
  vacio <- list(
    fila = NULL, secuencia = NULL, cuantitativo = NULL, guarda = NULL,
    es_identificador = FALSE
  )
  if (.es_columna_compuesta(x)) return(vacio)
  aplicable <- if (is.null(aplicable)) {
    rep(TRUE, NROW(x))
  } else {
    as.logical(aplicable)
  }
  if (length(aplicable) != NROW(x)) return(vacio)

  preparacion <- .texto_analizable(x)
  x_analisis <- preparacion$valores
  x_identidad <- preparacion$valores_identidad
  if (is.null(x_identidad)) x_identidad <- x_analisis
  if (!all(aplicable)) {
    x_analisis[!aplicable] <- NA
    if (inherits(x_identidad, "sfc")) {
      x_identidad <- x_identidad[aplicable]
    } else {
      x_identidad[!aplicable] <- NA
    }
  }

  inferencia <- .inferir_tipo_interno(
    if (is.character(x_analisis) || is.factor(x_analisis)) {
      x_analisis
    } else {
      x_identidad
    }, muestra = muestra, conservar_cache = TRUE
  )
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) formatos <- detectar_formatos_fecha(
    x_analisis, muestra = muestra
  )
  claves_identidad <- if (is.character(x_identidad) ||
                          is.factor(x_identidad)) {
    .clave_bytes(x_identidad)
  } else NULL
  n_distintos <- .n_distintos_columna(
    x_identidad, claves = claves_identidad, rapido = TRUE
  )
  vocabulario <- if (
    (is.character(x_analisis) || is.factor(x_analisis)) &&
      NROW(x_analisis) > 1L && is.finite(n_distintos) &&
      n_distintos / NROW(x_analisis) <= .umbral_vocabulario_codificacion
  ) {
    unique(x_analisis[!is.na(x_analisis)])
  } else NULL
  numeros_texto <- .analizar_numeros_texto(
    x_analisis, vocabulario = vocabulario, n_distintos = n_distintos,
    directo = is.null(vocabulario) &&
      (is.character(x_analisis) || is.factor(x_analisis))
  )
  inferencia$convencion_numero_texto <- numeros_texto$convencion
  sentinelas_declarados <- .sentinelas_numericos_declarados(
    sentinelas_numericos
  )
  cuantitativos_secuencia <- tryCatch(
    .valores_cuantitativos(x_analisis, inferencia, formatos),
    error = function(e) NULL
  )
  secuencia <- tryCatch(
    .resumen_secuencia_entera(
      x_analisis, inferencia, formatos,
      cuantitativos = cuantitativos_secuencia
    ),
    error = function(e) NULL
  )
  if (is.null(secuencia)) return(vacio)
  valores_numericos <- tryCatch(
    .valores_numericos_secuencia(
      x_analisis, inferencia, formatos,
      cuantitativos = cuantitativos_secuencia
    ),
    error = function(e) NULL
  )
  cuantitativo <- if (length(sentinelas_declarados)) tryCatch(
    .valores_cuantitativos(
      x_analisis, inferencia, formatos,
      sentinelas_numericos = sentinelas_declarados
    ),
    error = function(e) NULL
  ) else cuantitativos_secuencia
  cuantitativo <- tryCatch(
    .resumen_cuantitativo(
      x_analisis, inferencia, formatos,
      sentinelas_numericos = sentinelas_numericos,
      valores_preparados = x_analisis,
      cuantitativos = cuantitativo
    ),
    error = function(e) NULL
  )
  if (is.null(cuantitativo)) return(vacio)
  moda <- .moda_columna(x_identidad, claves = claves_identidad)
  n_aplicables <- sum(aplicable)
  n_faltantes <- sum(is.na(x_identidad) & aplicable)
  n_validos <- n_aplicables - n_faltantes
  tasa_distintos <- if (n_validos) n_distintos / n_validos else NA_real_
  fila <- list(
    tipo_declarado = .tipo_declarado(x),
    tipo_inferido = inferencia$tipo,
    n_distintos = n_distintos,
    n_aplicables = n_aplicables,
    n_faltantes = n_faltantes,
    frecuencia_moda = moda$frecuencia,
    densidad_sin_centinela = cuantitativo$densidad_sin_centinela,
    salto_de_escala_secuencia_entera = secuencia$salto_de_escala,
    moda_sobresale_secuencia_entera = secuencia$moda_sobresale,
    densidad_secuencia_entera = secuencia$densidad,
    secuencia_entera_densa = secuencia$densa,
    tasa_distintos = tasa_distintos
  )
  # `posible_identificador` no se comparte con los grupos, pero es una de las
  # tres razones por las que Benford excluye una columna: se calcula para eso.
  posible <- isTRUE(tryCatch(
    .decision_identificador_numerico(fila)$posible,
    error = function(e) FALSE
  ))
  guarda <- if (is.null(valores_numericos)) NULL else tryCatch(
    .decision_guarda_numeracion(
      valores_numericos, secuencia, sentinelas_numericos,
      sentinelas_declarados
    ),
    error = function(e) NULL
  )
  # Benford decide sobre el universo del resumen, que es el que `perfilar()` le
  # pasa: lo que el usuario declaro centinela no es un valor de la columna.
  # Decidir sobre los valores crudos daba otra respuesta que la tabla entera.
  # Medido con `c(1:2000, rep(8888, 20))` y `sentinelas_numericos = 8888`:
  # `perfilar()` excluye la columna por correlativa -sin los 8888 es `1:2000`-,
  # y aca los veinte 8888 rompian la numeracion, la columna no salia
  # identificadora y un grupo publicaba `desviacion_benford`. Solo un numero
  # sin clase es candidato a Benford; a las demas clases no les cambia nada.
  universo_benford <- if (.es_numerico_pelado(x)) {
    .universo_del_resumen(list(x), sentinelas_numericos, list(aplicable))[[1L]]
  } else valores_numericos
  motivos <- .motivos_identificador_benford(
    universo_benford, inferencia$tipo, posible, clave_declarada
  )
  list(
    fila = fila, secuencia = secuencia, cuantitativo = cuantitativo,
    guarda = guarda, es_identificador = length(motivos) > 0L
  )
}

# Un `999` que aparece UNA vez, en su lugar, dentro de un catalogo `1:1000 +
# 2001:3000` salia acusado de ser una ausencia codificada. Una sola aparicion no
# es una codificacion de ausencia: es un numero.
#
# La lista de por omision **pertenece al paquete** -lo dice su propio comentario
# mas abajo: "sirve para sospechar de codigos comunes, pero no alcanza para
# afirmar que son ausencia en una columna concreta"-. Exigirle que el candidato
# se repita es el paquete afinando su propia sospecha, y por eso la regla se
# aplica SOLO a esa lista.
#
# Cuidado con el criterio de las "tres cosas a la vez" que se lee mas abajo
# -extremo, que se repita, forma de centinela-: pertenece a OTRO mecanismo, el
# que sospecha de valores que NO estan en la lista, y su propio texto aclara que
# "no cambia `faltantes_disfrazados`". Tomarlo prestado para justificar esto
# seria sacarlo de su alcance.
#
# Sale de la lista solo el candidato que ademas cae **dentro** del rango de los
# demas valores. Uno que aparece una vez y esta fuera del rango -un `-9` suelto
# en una numeracion de 1 a 60- sigue entrando: ahi la rareza esta en el valor, no
# en su frecuencia.
.sentinelas_sin_apariciones_unicas <- function(valores, candidatos, rango,
                                              sentinelas_numericos) {
  # SOLO sobre la lista de por omision, que es la unica que el paquete aplica sin
  # que nadie la pida. Pedir `sentinelas_naniar` es -en palabras del propio
  # paquete- "optar por una heuristica MAS ANCHA", y angostarla aca contradice
  # esa eleccion: sobre `c(66, 77, 88, 45, 30, ...)` sacaba el `66` porque cae
  # dentro del rango de las edades y aparece una vez. Lo vigila
  # `test-declarado-contra-sospechado.R`.
  #
  # La comparacion es de CONJUNTO y no con `identical()`: con `identical()`, la
  # misma lista escrita al reves daba el resultado contrario.
  if (!identical(.numeros_na(sentinelas_numericos),
                 .numeros_na(.numeros_na_locales))) {
    return(sentinelas_numericos)
  }
  if (!length(candidatos) || is.null(rango)) return(sentinelas_numericos)
  presentes <- !is.na(valores) & is.finite(valores)
  if (!any(presentes)) return(sentinelas_numericos)
  unicos_en_rango <- vapply(candidatos, function(candidato) {
    dentro <- candidato >= rango[[1L]] && candidato <= rango[[2L]]
    dentro && sum(presentes & valores == candidato) == 1L
  }, logical(1L))
  if (!any(unicos_en_rango)) return(sentinelas_numericos)
  descartados <- candidatos[unicos_en_rango]
  # Se filtra en el espacio NORMALIZADO, no indexando la lista original. La
  # primera version calculaba la mascara sobre `.numeros_na(...)` -que hace
  # `sort(unique(...))`- y la aplicaba sobre `sentinelas_numericos`, que viene en
  # el orden del usuario. Con la lista por omision los dos ordenes son
  # `-9, -99, -999, -9999, 999` y `-9999, -999, -99, -9, 999`: descartar `-9`
  # sacaba `-9999`. O sea que dejaba pasar el falso positivo que venia a matar Y
  # **silenciaba ausencias reales**, que es el error peor de los dos. Solo `999`
  # funcionaba, porque es la posicion 5 en los dos ordenes, y los casos de prueba
  # usaban justo ese.
  #
  # Devolver la lista normalizada no cambia nada aguas abajo:
  # `.detectar_faltantes_disfrazados()` la normaliza igual antes de usarla.
  normalizados <- .numeros_na(sentinelas_numericos)
  normalizados[!(normalizados %in% descartados)]
}

.sentinela_numerico_es_moda_sobresaliente <- function(
    valores, candidatos, secuencia_entera) {
  if (!isTRUE(secuencia_entera$moda_sobresale)) return(FALSE)
  presentes <- !is.na(valores) & is.finite(valores)
  if (!length(candidatos) || !any(presentes)) return(FALSE)
  frecuencias_candidatos <- vapply(candidatos, function(candidato) {
    sum(presentes & valores == candidato)
  }, numeric(1L))
  distintos <- .sort_por_bytes(unique(valores[presentes]))
  frecuencias <- tabulate(match(valores[presentes], distintos),
                          nbins = length(distintos))
  any(frecuencias_candidatos == max(frecuencias))
}

# Un valor centinela -el `9999` que quiere decir "no sabemos"- no se puede
# reconocer por su forma sola: `9999` es una edad imposible y un codigo postal
# perfectamente valido. Por eso la lista de `sentinelas_numericos` no lo trae
# por omision, y hace bien: marcarlo siempre produciria falsos en cualquier
# columna donde ese numero es un dato.
#
# Lo que si lo distingue es que cumpla las **tres** cosas a la vez: que sea un
# valor extremo, que se repita, y que tenga forma de centinela. Un codigo postal
# 9999 repetido treinta veces no es extremo dentro de su columna; un monto real
# de 9999 no se repite; un ano 1999 no tiene forma de centinela. Medido sobre
# ocho columnas con la respuesta conocida -tres con centinela y cinco donde el
# mismo numero es un dato real-, las tres senales juntas aciertan las ocho.
#
# Esto no cambia `faltantes_disfrazados`, que sigue contando solo lo que la
# lista declarada dice. Es una senal aparte, que se informa para que el usuario
# decida si agrega ese valor a su lista.
.MIN_REPETICIONES_CENTINELA <- 5L

# El vector predeterminado pertenece al paquete: sirve para sospechar de
# codigos comunes, pero no alcanza para afirmar que son ausencia en una
# columna concreta. Si el usuario reemplaza la politica completa, en cambio,
# cada valor que entrega es una declaracion. Comparar el vector completo evita
# que el default no vacio empiece a alterar estadisticas publicadas sin una
# decision explicita.
# Que valores dijo el USUARIO que son ausencia, y cuales trae el paquete.
#
# La distincion gobierna los resumenes: lo declarado sale de la media, lo
# supuesto no. Las listas que el paquete publica -la de por omision y
# `sentinelas_naniar`- son heuristicas suyas, no afirmaciones de nadie sobre una
# columna concreta. Pedir `sentinelas_naniar` es optar por una heuristica MAS
# ANCHA, y la documentacion del paquete avisa por que no viene por omision:
# `66`, `77` y `88` tambien pueden ser edades, codigos o anos legitimos. Si eso
# contara como declaracion, elegir esa lista convertiria en `error` una columna
# de edades y sacaria esos valores del promedio, que es justo lo contrario de lo
# que el aviso promete.
#
# El limite es el que se puede sostener: un vector que el usuario compone es una
# declaracion. Si compone el mismo conjunto que alguna de las listas publicadas,
# se lo trata como heuristica; el orden, la clase y los duplicados no cambian esa
# respuesta.
.sentinelas_numericos_declarados <- function(sentinelas_numericos) {
  if (is.null(sentinelas_numericos)) {
    return(numeric())
  }
  normalizados <- .normalizar_sentinelas_numericos(sentinelas_numericos)
  if (setequal(normalizados, .normalizar_sentinelas_numericos(
    .numeros_na_locales
  )) || setequal(normalizados, .normalizar_sentinelas_numericos(
    sentinelas_naniar
  ))) {
    return(numeric())
  }
  normalizados
}

.mascara_sentinelas_resumen <- function(valores, sentinelas_numericos) {
  if (!length(valores) || !length(sentinelas_numericos)) {
    return(rep(FALSE, length(valores)))
  }
  coincide <- tryCatch(
    valores %in% sentinelas_numericos,
    error = function(e) NULL
  )
  if (is.null(coincide) || length(coincide) != length(valores)) {
    return(rep(FALSE, length(valores)))
  }
  !is.na(valores) & coincide
}

# **Se probo descartar al candidato que tiene vecinos inmediatos y se retiro.**
# La idea era que un centinela esta solo mientras que un codigo de catalogo vive
# en un tramo, y sobre el caso que la motivo funcionaba: `222` entre `221` y
# `223` daba razon de vecinos 1,00 y los centinelas sueltos 0,00.
#
# Lo que la tumbo es la codificacion mas comun que existe en microdatos de
# encuesta: `9999` = "no sabe" junto a `9998` = "no contesta". Medido sobre una
# columna de horas trabajadas con treinta `9999` y veinticinco `9998`, el `9998`
# contaba como vecino -razon 0,833-, descartaba al `9999`, y como el `9998` no
# tiene forma de digito repetido tampoco entraba por su cuenta: los dos
# centinelas quedaban invisibles Y la columna dejaba de verse como numeracion,
# asi que salian **cincuenta y cinco codigos de ausencia informados como valores
# extremos de una magnitud**.
#
# No hay senal de forma que separe `221/222/223` de `9998/9999`: en los dos
# casos son valores contiguos, extremos y repetidos. La diferencia es semantica.
# Y la regla del paquete es que una guarda solo entra si no calla nada real, asi
# que se prefiere el falso positivo del `222` -un aviso que dice que lo decida
# quien conoce la columna- antes que callar un centinela verdadero.

.centinela_por_tres_senales <- function(valores, iqr,
                                       sentinelas_numericos = NULL,
                                       q1 = NULL, q3 = NULL) {
  vacio <- list(valor = NA_real_, n = NA_integer_,
                densidad_sin_centinela = NA_real_)
  if (!length(valores) || !is.finite(iqr) || iqr <= 0) return(vacio)
  if (length(valores) < 20L) return(vacio)
  if (is.null(q1) || is.null(q3)) {
    q <- stats::quantile(
      valores, probs = c(0.25, 0.75), names = FALSE, type = 7
    )
    q1 <- q[[1L]]
    q3 <- q[[2L]]
  }
  extremos <- valores[valores < q1 - 1.5 * iqr |
                        valores > q3 + 1.5 * iqr]
  if (!length(extremos)) return(vacio)
  frecuencias <- table(extremos)
  repetidos <- suppressWarnings(
    as.numeric(names(frecuencias)[frecuencias >= .MIN_REPETICIONES_CENTINELA])
  )
  if (!length(repetidos)) return(vacio)
  # Un digito repetido tres veces o mas, con o sin signo: `999`, `-999`, `9999`,
  # `8888`, `7777`. Empezo siendo solo nueves y se amplio midiendo: sobre doce
  # columnas con la respuesta conocida, solo-nueves acertaba diez y se perdia
  # dos centinelas reales -`8888` y `7777` en columnas de edad-, y el patron
  # ancho acierta las doce sin inventar ninguno.
  #
  # Los controles que se le pasaron: un codigo `111` en una columna de 100 a
  # 200, un `0` repetido veinticinco veces entre cantidades, y un `222` entre
  # mediciones de 200 a 260. Ninguno es extremo dentro de su columna.
  #
  # **Y ahi estaba la trampa**, que se descubrio despues: los tres controles
  # eligen valores que no son extremos, o sea que ninguno ejercita la unica
  # situacion en que esta senal puede hacer dano. Se probaba el caso comodo y se
  # leia el resultado como si cubriera el dificil. El `222` de un tramo de
  # codigos altos dentro de una columna concentrada abajo SI es extremo, y ahi
  # esta forma decidia sola. Lo frena la guarda de vecinos, mas abajo.
  con_forma <- repetidos[grepl("^-?([0-9])\\1{2,}$", as.character(repetidos))]
  # Los valores que la lista de sentinelas YA cuenta como ausencia no son asunto
  # de este diagnostico: `faltantes_disfrazados` los informa. Sin este descarte
  # los dos hallazgos se contradecian sobre la misma columna -uno decia que el
  # `999` era una ausencia y el otro que "no se cuenta como ausencia porque no
  # esta declarado", cuando si lo estaba-.
  #
  # Con que severidad los informa depende de QUIEN lo dijo, y este comentario
  # afirmaba `error` a secas, que era falso para la mitad de los casos:
  # `error` si el usuario reemplazo `sentinelas_numericos` -es su declaracion-,
  # y `sospechoso` si el valor viene de la lista por omision, que es una
  # corazonada del paquete. La regla vive en `R/hallazgos.R`, en la rama
  # `solo_numericos`.
  declarados <- suppressWarnings(as.numeric(sentinelas_numericos))
  declarados <- declarados[is.finite(declarados)]
  if (length(declarados)) con_forma <- setdiff(con_forma, declarados)
  if (!length(con_forma)) return(vacio)
  elegido <- con_forma[[which.max(abs(con_forma))]]
  # Una numeracion con un centinela adentro deja de parecer numeracion: el
  # `9999` abre un hueco enorme y hunde la densidad, asi que sobre un
  # identificador de 1 a 4000 con quince `9999` se volvian a correr Benford y
  # los limites de Tukey. Medido: la densidad cae de 0,625 a 0,250 por culpa del
  # centinela.
  #
  # La columna sin el centinela es lo que hay que mirar para decidir si es una
  # numeracion. El centinela se informa igual, por su cuenta.
  restantes <- valores[valores != elegido]
  distintos <- unique(restantes)
  densidad_limpia <- if (length(distintos) > 1L) {
    rango <- max(distintos) - min(distintos) + 1
    if (is.finite(rango) && rango > 0) length(distintos) / rango else NA_real_
  } else NA_real_
  list(
    valor = as.numeric(elegido),
    n = as.integer(sum(valores == elegido)),
    densidad_sin_centinela = as.numeric(densidad_limpia)
  )
}

# Los estados que significan "esta clase NO produce resumen cuantitativo". Es la
# condicion con la que el campo `unidad` se calla: una unidad de cifras que no
# existen no contesta la pregunta del campo.
#
# Son dos y no uno, y eso costo un defecto: la rama principal miraba solo
# `no_aplica`, y la de columnas compuestas escribe su propio estado
# -`tipo_compuesto_no_analizado`- y publicaba la unidad sin condicion. Medido: una
# matriz con `attr(m, "units") <- "kg"` publicaba `unidad = "kg"` junto a
# `media = NA`, que es la forma exacta que el arreglo del `factor` habia venido a
# cerrar; la columna de listas, cuyo estado SI es `no_aplica`, salia callada. El
# comentario que lo justificaba decia "como en `difftime`", y esa analogia se
# vencio el mismo dia: `difftime` publica cifras desde que dejo de abstenerse.
#
# Es un vocabulario CERRADO del paquete -no una lista de clases-, asi que
# enumerarlo aca es la forma correcta; lo que no puede volver a pasar es que la
# condicion se escriba dos veces y una se quede corta.
.ESTADOS_SIN_RESUMEN_CUANTITATIVO <- c(
  "no_aplica", "tipo_compuesto_no_analizado"
)

.resumen_vacio_cuantitativo <- function(estado = "no_aplica") {
  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_,
    mediana = NA_real_, desvio = NA_real_, minimo_exacto = NA_character_,
    maximo_exacto = NA_character_, minimo_fecha = NA_character_,
    maximo_fecha = NA_character_, media_fecha = NA_character_,
    mediana_fecha = NA_character_, n_ceros = NA_integer_,
    n_negativos = NA_integer_, n_outliers = NA_integer_,
    centinela_valor = NA_real_, centinela_repeticiones = NA_integer_,
    densidad_sin_centinela = NA_real_, n_nan = 0L,
    n_infinito_positivo = 0L, n_infinito_negativo = 0L,
    n_fechas_resumidas = NA_integer_,
    n_fechas_excluidas_granularidad = NA_integer_,
    n_valores_excluidos_resumen = NA_integer_,
    estado_outliers = "no_aplica", motivo_outliers = NA_character_,
    estado_resumen_cuantitativo = estado
  )
}

# `columnas_codigo`. La regla de numeracion de
# `hallazgos.R` dejo de tratar como identificador a un codigo que se repite
# -un codigo de catalogo no se distingue por su forma de una magnitud que lleno
# su rango-, y un `EstCod` de 1 a 284 recibia 181 outliers falsos. Pasarlo como
# texto o factor no lo evitaba: el paquete infiere numero y calcula igual. Lo
# que el paquete no puede saber lo declara quien conoce el dato, como
# `columnas_opcionales` y `columnas_personales`.
#
# Una regla, en un solo lugar: TODAS las vias que aceptan las declaraciones de
# `perfilar()` -`perfilar_dbi()` para su `perfil_muestra`, `perfilar_por()` en
# cada grupo, `analizar()`, `perfilar_coleccion()`- perfilan por aca, y lo que
# no pasa por `perfilar()` -el `resumen_tabla` de `perfilar_dbi()`- llama a
# esta misma validacion.
.normalizar_columnas_codigo <- function(columnas_codigo, nombres) {
  if (is.null(columnas_codigo)) return(character())
  if (!is.character(columnas_codigo) || anyNA(columnas_codigo)) {
    .detener("`columnas_codigo` debe ser un vector de nombres de columna.",
         call. = FALSE)
  }
  if (!length(columnas_codigo)) return(character())
  indices <- .indice_nombre(columnas_codigo, nombres)
  if (anyNA(indices)) {
    .detener("`columnas_codigo` nombra columnas inexistentes: ",
         .pegar_texto(columnas_codigo[is.na(indices)], collapse = ", "), ".",
         call. = FALSE)
  }
  .identificadores_unicos(nombres[indices])
}

# Lo que una columna declarada como codigo NO publica: las cifras que solo
# tienen sentido sobre una magnitud. `media`, `mediana` y `desvio` describen
# una distribucion, y el promedio de unos codigos no es un dato; los limites
# de Tukey suponen esa distribucion -que un codigo quede lejos de la mediana no
# dice nada-, y el centinela de tres senales se mide contra esos limites. Lo
# demas se conserva, porque describe los VALORES y no una magnitud: `minimo` y
# `maximo` -el tramo de codigos observado, donde aparece un codigo fuera de
# catalogo-, los conteos, la moda, los distintos y los faltantes disfrazados.
# El estado lo declara, como `omitidos_precision` declara lo suyo.
#
# Solo actua sobre un resumen numerico calculado: una columna de texto o de
# fechas declarada como codigo no tiene esas cifras y no cambia.
.resumen_codigo_declarado <- function(cuantitativo) {
  # Un entero que pasa de 2^53 no publica media ni desvio, pero si la mediana
  # cuando sus centrales caben en un doble (`.mediana_centrales_exactas()`). La
  # de un codigo no es un dato: se saca, y el estado sigue diciendo la
  # precision, que es lo que retuvo el resto.
  if (identical(cuantitativo$estado_resumen_cuantitativo, "omitidos_precision")) {
    cuantitativo$mediana <- NA_real_
    return(cuantitativo)
  }
  numerico <- cuantitativo$estado_resumen_cuantitativo %in%
    c("calculados", "calculados_sobre_valores") &&
    is.na(cuantitativo$minimo_fecha) && is.na(cuantitativo$maximo_fecha)
  if (!isTRUE(numerico)) return(cuantitativo)
  cuantitativo$media <- NA_real_
  cuantitativo$mediana <- NA_real_
  cuantitativo$desvio <- NA_real_
  cuantitativo$n_outliers <- NA_integer_
  cuantitativo$centinela_valor <- NA_real_
  cuantitativo$centinela_repeticiones <- NA_integer_
  cuantitativo$densidad_sin_centinela <- NA_real_
  cuantitativo$estado_outliers <- "codigo_declarado"
  cuantitativo$motivo_outliers <- paste(
    "No se evaluaron los l\u00edmites de Tukey: la columna se declar\u00f3 en",
    "`columnas_codigo`, as\u00ed que es un c\u00f3digo y no una magnitud. Que un c\u00f3digo",
    "quede lejos de la mediana no dice nada de su calidad."
  )
  cuantitativo$estado_resumen_cuantitativo <- "omitidos_codigo"
  cuantitativo
}

# La mediana de enteros que se tienen exactos -un `integer64`, o enteros
# escritos como texto- en una columna con algun valor que el doble no
# representa. Es la regla de `perfilar_dbi()` (`.causa_mediana_inexacta_dbi()`)
# con las centrales a la vista: la mediana depende solo de ellas, y se publica si
# cada una cabe exacta en un doble -valor absoluto menor que 2^53-. Entonces es
# la de `median()` sobre esos dobles, la misma que da una columna que no pasa de
# 2^53 con esas centrales. Si alguna no cabe, `NA`: su doble ya no es el valor.
# La columna entera se negaba, y la mediana de `{1, 2, 2^63 - 1}` es 2.
#
# Una regla en un solo lugar: la usan `perfilar()` -y con el `perfil_muestra` y
# el spool de `muestra_motor`, que perfilan lo leido-, para `integer64` y para el
# texto, y la via por bloques sobre el mapa de distintos.
.mediana_centrales_exactas <- function(centrales) {
  if (!length(centrales) || length(centrales) > 2L) return(NA_real_)
  if (inherits(centrales, "integer64")) {
    if (!.bit64_disponible()) return(NA_real_)
    exactas <- as.logical(.integer64_exacto_en_doble(centrales))
    if (!isTRUE(all(exactas))) return(NA_real_)
    return(stats::median(as.numeric(centrales)))
  }
  # Texto de un entero: el doble que lee R es el valor si este queda por debajo
  # de 2^53 en valor absoluto, y desde ahi el redondeo es monotono -nunca cae
  # por debajo de 2^53-, asi que el mismo umbral sobre lo leido decide.
  dobles <- suppressWarnings(as.numeric(as.character(centrales)))
  if (!all(is.finite(dobles) & abs(dobles) < .MAX_ENTERO_EXACTO_DBI)) {
    return(NA_real_)
  }
  stats::median(dobles)
}

# Las una o dos centrales de un `integer64` sin `NA`, en su clase: las
# posiciones `(n + 1) %/% 2` y `(n + 2) %/% 2` del orden por valor, que son las
# que promedia `median()`. `.sort_por_bytes()` ordena un `integer64` por su
# rango, no por sus bits.
.centrales_enteros_exactos <- function(valores) {
  n <- length(valores)
  if (!n) return(valores)
  ordenados <- .sort_por_bytes(valores)
  ordenados[unique(c((n + 1L) %/% 2L, (n + 2L) %/% 2L))]
}

.resumen_integer64 <- function(x, sentinelas_numericos = NULL) {
  vacio <- .resumen_vacio_cuantitativo("sin_valores")
  sentinelas_completos <- sentinelas_numericos
  sentinelas_numericos <- .sentinelas_numericos_declarados(
    sentinelas_numericos
  )
  mascara_sentinelas <- if (length(sentinelas_numericos)) {
    !is.na(x) & .texto_identidad(x) %in% .texto_identidad(sentinelas_numericos)
  } else {
    rep(FALSE, length(x))
  }
  n_excluidos <- as.integer(sum(mascara_sentinelas))
  n_presentes_integer64 <- as.integer(sum(!is.na(x)))
  vacio$n_valores_excluidos_resumen <- n_excluidos
  if (n_excluidos > 0L) {
    x <- x[!mascara_sentinelas]
    vacio$estado_resumen_cuantitativo <- "calculados_sobre_valores"
  }
  validos <- !is.na(x)
  if (!any(validos)) {
    # La misma distincion que en la rama `double`: si la columna TENIA valores y
    # los centinelas se los llevaron todos, `sin_valores` es falso.
    # El estado se escribia arriba, apenas habia excluidos, ANTES de saber si
    # quedaba algo: con seis centinelas de seis valores publicaba
    # `calculados_sobre_valores` junto a `minimo`, `media` y `desvio` en `NA`. El
    # README fija el enunciado -"si no sobrevive ningun valor utilizable el estado
    # dice `sin_valores` en vez de afirmar que calculo algo"- y la rama `double`
    # sobre el mismo dato ya lo decia: era la misma pregunta contestada dos veces.
    vacio$estado_resumen_cuantitativo <- if (n_presentes_integer64 > 0L) {
      "sin_valores_utilizables"
    } else {
      "sin_valores"
    }
    return(vacio)
  }
  if (!.bit64_disponible()) {
    vacio$estado_resumen_cuantitativo <- "requiere_bit64"
    return(vacio)
  }
  valores <- x[validos]
  minimo_exacto <- as.character(min(valores))
  maximo_exacto <- as.character(max(valores))
  # La frontera es la de toda la via DBI, escrita una vez.
  seguros <- .integer64_exacto_en_doble(valores)
  if (!all(seguros)) {
    vacio$minimo_exacto <- minimo_exacto
    vacio$maximo_exacto <- maximo_exacto
    vacio$n_ceros <- as.integer(sum(valores == bit64::as.integer64(0)))
    vacio$n_negativos <- as.integer(sum(valores < bit64::as.integer64(0)))
    # La mediana no depende de toda la columna sino de sus centrales.
    vacio$mediana <- .mediana_centrales_exactas(
      .centrales_enteros_exactos(valores)
    )
    vacio$estado_resumen_cuantitativo <- "omitidos_precision"
    return(vacio)
  }
  resultado <- .resumen_cuantitativo(
    as.numeric(valores),
    list(tipo = "doble"),
    data.frame(stringsAsFactors = FALSE),
    sentinelas_numericos = sentinelas_completos
  )
  resultado$minimo_exacto <- minimo_exacto
  resultado$maximo_exacto <- maximo_exacto
  resultado$n_valores_excluidos_resumen <- n_excluidos
  if (n_excluidos > 0L) {
    resultado$estado_resumen_cuantitativo <- "calculados_sobre_valores"
  }
  resultado
}

.fecha_resumida <- function(valor, clase) {
  if (!is.finite(valor)) {
    return(NA_character_)
  }
  if (identical(clase, "fecha")) {
    return(format(
      as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d", tz = "UTC"
    ))
  }
  format(
    as.POSIXct(valor, origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"
  )
}

.resumen_cuantitativo <- function(x, inferencia, formatos,
                                  meses_texto = NULL,
                                  sentinelas_numericos = NULL,
                                  vocabulario = NULL,
                                  valores_preparados = NULL,
                                  cuantitativos = NULL) {
  sentinelas_declarados <- .sentinelas_numericos_declarados(
    sentinelas_numericos
  )
  if (is.null(cuantitativos)) {
    cuantitativos <- .valores_cuantitativos(
      x, inferencia, formatos, meses_texto = meses_texto,
      vocabulario = vocabulario, valores_preparados = valores_preparados,
      sentinelas_numericos = sentinelas_declarados
    )
  }
  if (identical(cuantitativos$clase, "fecha_granularidad_incompleta")) {
    salida <- .resumen_vacio_cuantitativo("granularidad_incompleta")
    salida$n_fechas_resumidas <- cuantitativos$n_fechas_resumidas
    salida$n_fechas_excluidas_granularidad <-
      cuantitativos$n_fechas_excluidas_granularidad
    salida$n_valores_excluidos_resumen <-
      cuantitativos$n_valores_excluidos_resumen
    return(salida)
  }
  if (identical(cuantitativos$clase, "integer64")) {
    return(.resumen_integer64(
      cuantitativos$valores,
      sentinelas_numericos = sentinelas_numericos
    ))
  }
  # La misma respuesta que `integer64` para el mismo dato escrito como texto:
  # las medidas quedan en `NA` -porque el doble no las puede sostener-, los
  # extremos salen exactos y el estado lo declara.
  if (!is.null(cuantitativos$fuera_de_precision)) {
    fuera <- cuantitativos$fuera_de_precision
    salida <- .resumen_vacio_cuantitativo("omitidos_precision")
    salida$minimo_exacto <- fuera$minimo_exacto
    salida$maximo_exacto <- fuera$maximo_exacto
    salida$n_ceros <- fuera$n_ceros
    salida$n_negativos <- fuera$n_negativos
    # La mediana, por sus centrales, como en `integer64`. Los centinelas
    # declarados ya salieron (`.extremos_enteros_texto_fuera_de_precision()`):
    # los extremos, los conteos y la mediana son los de los mismos valores que
    # en las otras ramas, y los excluidos se cuentan.
    salida$mediana <- .mediana_centrales_exactas(fuera$centrales)
    salida$n_valores_excluidos_resumen <- as.integer(
      if (is.null(cuantitativos$n_valores_excluidos_resumen)) {
        0L
      } else {
        cuantitativos$n_valores_excluidos_resumen
      }
    ) + as.integer(fuera$n_sentinelas %||% 0L)
    return(salida)
  }
  valores_originales <- cuantitativos$valores
  # Cuantos valores PRESENTES tenia la columna antes de excluir nada, **en el
  # universo que el resumen mide**. "Presente" se define igual que en
  # `n_faltantes`, que es la nocion del paquete: medido, una columna de cinco
  # `NaN` publica `n_faltantes = 5` y `n_distintos = 0` -el `NaN` cuenta como
  # ausente- y una de cinco `Inf` publica `n_faltantes = 0` y `n_distintos = 1`.
  # Inventar otra definicion aca haria que dos campos de la misma fila contaran
  # cosas distintas.
  #
  # El universo importa y la primera version de este comentario lo omitia: `x` ya
  # llega con las filas fuera de la aplicabilidad declarada marcadas como
  # ausentes, asi que la relacion que se cumple es
  # `n_aplicables - n_faltantes`, NO `n - n_faltantes`. Medido con
  # `aplicabilidad = list(v = ~ FALSE)` sobre cinco valores: `n_aplicables = 0`,
  # `n_faltantes = 0`, y el estado sale `sin_valores` -correcto, porque en el
  # universo que se resume no hay ningun valor presente-, mientras la fila
  # declara los cinco en `n_presentes_fuera_de_aplicabilidad`. No es una
  # contradiccion, y un estado como `sin_valores_utilizables` seria lo falso.
  n_presentes_resumen <- as.integer(sum(!is.na(valores_originales)))
  mascara_sentinelas <- if (!is.null(cuantitativos$mascara_sentinelas_texto)) {
    cuantitativos$mascara_sentinelas_texto
  } else if (identical(cuantitativos$clase, "numero")) {
    .mascara_sentinelas_resumen(
      valores_originales, sentinelas_declarados
    )
  } else {
    rep(FALSE, length(valores_originales))
  }
  n_sentinelas_excluidos <- as.integer(sum(mascara_sentinelas))
  n_previos_excluidos <- suppressWarnings(as.numeric(
    cuantitativos$n_valores_excluidos_resumen
  ))
  if (length(n_previos_excluidos) != 1L ||
      !is.finite(n_previos_excluidos)) {
    n_previos_excluidos <- 0
  }
  n_excluidos_resumen <- as.integer(
    n_previos_excluidos + n_sentinelas_excluidos
  )
  cuantitativos$n_valores_excluidos_resumen <- n_excluidos_resumen
  if (identical(cuantitativos$clase, "numero") &&
      n_excluidos_resumen > 0L) {
    cuantitativos$estado <- "calculados_sobre_valores"
  }
  valores <- valores_originales[!mascara_sentinelas]
  n_nan <- if (is.numeric(valores)) sum(is.nan(valores)) else 0L
  n_infinito_positivo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores > 0, na.rm = TRUE)
  } else 0L
  n_infinito_negativo <- if (is.numeric(valores)) {
    sum(is.infinite(valores) & valores < 0, na.rm = TRUE)
  } else 0L
  # Los presentes que no son finitos tampoco entraron al resumen, y hasta aca
  # no se contaban como excluidos. Los dos README prometen lo contrario con
  # todas las letras -"un resumen cuantitativo sigue dejando afuera lo que no
  # vale como numero -`NaN`, `Inf`, ...- y cuando lo hace lo dice:
  # `n_valores_excluidos_resumen` los cuenta, `estado_resumen_cuantitativo`
  # deja de decir `calculados`"-. Medido sobre 28 finitos con un `Inf` y un
  # `-Inf`: `n_valores_excluidos_resumen = 0`, estado `calculados`, y el minimo
  # y la media ya calculados sin ellos.
  #
  # `n_nan` y `n_infinito_*` siguen publicando el desglose; esto agrega el
  # total, que es lo que el resto del paquete lee para declarar el alcance.
  n_no_finitos <- as.integer(n_nan + n_infinito_positivo + n_infinito_negativo)
  # La condicion miraba `clase == "numero"`, y el descarte de la linea siguiente
  # -`valores[is.finite(valores)]`- NO mira la clase: pasa para todas. Con una
  # fecha `Inf`, el resumen se calculaba sobre los finitos y la fila publicaba
  # `n_valores_excluidos_resumen = 0` con estado `calculados`, en las DOS clases
  # temporales. Lo que decide es si quedo algo afuera, no de que clase es la
  # columna; para `integer64` y para las columnas sin resumen el total de no
  # finitos es cero, asi que ahi no cambia nada.
  if (n_no_finitos > 0L) {
    n_excluidos_resumen <- as.integer(n_excluidos_resumen + n_no_finitos)
    cuantitativos$n_valores_excluidos_resumen <- n_excluidos_resumen
    cuantitativos$estado <- "calculados_sobre_valores"
  }
  valores <- valores[is.finite(valores)]
  vacio <- .resumen_vacio_cuantitativo(if (
    identical(cuantitativos$clase, "ninguna")) "no_aplica" else "sin_valores"
  )
  vacio$n_nan <- as.integer(n_nan)
  vacio$n_infinito_positivo <- as.integer(n_infinito_positivo)
  vacio$n_infinito_negativo <- as.integer(n_infinito_negativo)
  vacio$n_fechas_resumidas <- if (!is.null(cuantitativos$n_fechas_resumidas)) {
    cuantitativos$n_fechas_resumidas
  } else NA_integer_
  vacio$n_fechas_excluidas_granularidad <- if (
    !is.null(cuantitativos$n_fechas_excluidas_granularidad)
  ) cuantitativos$n_fechas_excluidas_granularidad else NA_integer_
  vacio$n_valores_excluidos_resumen <- if (
    !is.null(cuantitativos$n_valores_excluidos_resumen)
  ) cuantitativos$n_valores_excluidos_resumen else NA_integer_
  vacio$estado_resumen_cuantitativo <- if (!is.null(cuantitativos$estado)) {
    cuantitativos$estado
  } else vacio$estado_resumen_cuantitativo
  if (!length(valores)) {
    # Sin un solo valor utilizable no se calculo nada, y el estado que venia
    # describe la columna entera: `calculados`, o `calculados_sobre_valores` si
    # algo quedo afuera. `sin_valores` ya existia en
    # `.resumen_vacio_cuantitativo()` y esta rama lo pisaba dos lineas antes,
    # asi que por este camino no se publicaba nunca: una columna de treinta
    # `Inf` salia `calculados` con el minimo, el maximo y la media en `NA`.
    # Dos afirmaciones distintas, que compartian un nombre. `sin_valores` era
    # cierto de una columna vacia y de una de puros `NA`, y FALSO de una de
    # treinta `Inf` -hay treinta valores, ninguno sirve- y de una donde los
    # centinelas se llevaron todo. Medido: cinco situaciones publicaban
    # `sin_valores`, y en tres el nombre mentia.
    vacio$estado_resumen_cuantitativo <- if (
      identical(cuantitativos$clase, "ninguna")
    ) {
      "no_aplica"
    } else if (n_presentes_resumen > 0L) {
      "sin_valores_utilizables"
    } else {
      "sin_valores"
    }
    if (cuantitativos$clase %in% c("numero", "integer64")) {
      vacio$estado_outliers <- "no_evaluado"
      vacio$motivo_outliers <- paste(
        "No se evaluaron los l\u00edmites de Tukey porque no hay valores finitos;",
        "se requieren al menos 20 valores finitos y un IQR positivo."
      )
    }
    return(vacio)
  }

  valores_basicos <- if (!is.null(cuantitativos$mascara_sentinelas_texto)) {
    valores_originales[!mascara_sentinelas]
  } else {
    valores_originales
  }
  basicos <- .resumen_cuantitativo_bloques(
    valores_basicos,
    contar_signos = identical(cuantitativos$clase, "numero"),
    excluir = if (is.null(cuantitativos$mascara_sentinelas_texto) &&
                 identical(cuantitativos$clase, "numero")) {
      sentinelas_declarados
    } else {
      numeric()
    }
  )
  minimo <- basicos$minimo
  maximo <- basicos$maximo
  media <- basicos$media
  cuartiles <- stats::quantile(
    valores, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7
  )
  # La mediana sigue saliendo de `median()` y no del cuantil 0,5. No son lo
  # mismo hasta el ultimo bit: `median()` promedia los dos centrales con
  # `(a + b) / 2` y `quantile(type = 7)` interpola con `a + 0,5 * (b - a)`, que
  # redondean distinto cuando los centrales son de magnitudes muy dispares.
  # Medido con `c(-1000, 0.000111, 0.25, 1000)`: 0,12505549999999999 contra
  # 0,12505550000000001, y la diferencia llegaba a la mediana informada.
  # Ahorrar un recorrido no vale cambiar un numero que se publica.
  mediana <- stats::median(valores)
  desvio <- basicos$desvio
  # `valores` ya fue filtrado con `is.finite()`: `na.rm = TRUE` conserva la
  # semántica del IQR anterior y también deja la llamada segura si este bloque
  # vuelve a recibir valores ausentes en el futuro.
  iqr <- cuartiles[[2L]] - cuartiles[[1L]]
  centinela <- .centinela_por_tres_senales(
    valores, iqr, sentinelas_numericos,
    q1 = cuartiles[[1L]], q3 = cuartiles[[2L]]
  )
  if (length(valores) < 20L) {
    n_outliers <- NA_integer_
    estado_outliers <- "no_evaluado"
    motivo_outliers <- paste0(
      "No se evaluaron los l\u00edmites de Tukey: hay ", length(valores),
      " valores finitos y se requieren al menos 20. Reunir m\u00e1s observaciones",
      " o declarar otro criterio antes de interpretar outliers."
    )
  } else if (!is.finite(iqr) || iqr <= 0) {
    n_outliers <- NA_integer_
    estado_outliers <- "no_evaluado"
    motivo_outliers <- paste(
      "No se evaluaron los l\u00edmites de Tukey porque el recorrido",
      "intercuart\u00edlico es cero; no hay una distancia distributiva que medir.",
      "Reunir valores con variaci\u00f3n o declarar otro criterio antes de",
      "interpretar outliers."
    )
  } else {
    n_outliers <- sum(valores < cuartiles[[1L]] - 1.5 * iqr |
      valores > cuartiles[[2L]] + 1.5 * iqr)
    estado_outliers <- "evaluado"
    motivo_outliers <- NA_character_
  }

  if (identical(cuantitativos$clase, "numero")) {
    return(list(
      minimo = minimo, maximo = maximo, media = media, mediana = mediana,
      desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
      minimo_fecha = NA_character_, maximo_fecha = NA_character_,
      media_fecha = NA_character_, mediana_fecha = NA_character_,
      n_ceros = basicos$n_ceros, n_negativos = basicos$n_negativos,
      n_outliers = n_outliers, centinela_valor = centinela$valor,
      centinela_repeticiones = centinela$n,
      densidad_sin_centinela = centinela$densidad_sin_centinela,
      estado_outliers = estado_outliers, motivo_outliers = motivo_outliers,
      n_nan = as.integer(n_nan),
      n_infinito_positivo = as.integer(n_infinito_positivo),
      n_infinito_negativo = as.integer(n_infinito_negativo),
      n_fechas_resumidas = NA_integer_,
      n_fechas_excluidas_granularidad = NA_integer_,
      n_valores_excluidos_resumen = cuantitativos$n_valores_excluidos_resumen,
      estado_resumen_cuantitativo = cuantitativos$estado,
      # Ver el comentario de `n_resumidos` en la lista del final de esta misma
      # funcion. ESTA lista y esa son dos copias de casi el mismo conjunto de
      # campos -la de aca es la via numerica, la otra la temporal-, y agregar el
      # campo a una sola fue lo que hizo que la primera version de este arreglo
      # publicara "un numero que no se pudo establecer" en los seis casos
      # medidos. Si se toca una, hay que tocar la otra.
      n_resumidos = length(valores)
    ))
  }

  list(
    minimo = NA_real_, maximo = NA_real_, media = NA_real_, mediana = NA_real_,
    desvio = desvio, minimo_exacto = NA_character_, maximo_exacto = NA_character_,
    minimo_fecha = .fecha_resumida(minimo, cuantitativos$clase),
    maximo_fecha = .fecha_resumida(maximo, cuantitativos$clase),
    media_fecha = .fecha_resumida(media, cuantitativos$clase),
    mediana_fecha = .fecha_resumida(mediana, cuantitativos$clase),
    n_ceros = 0L, n_negativos = 0L, n_outliers = n_outliers,
    centinela_valor = centinela$valor, centinela_repeticiones = centinela$n,
    densidad_sin_centinela = centinela$densidad_sin_centinela,
    estado_outliers = estado_outliers, motivo_outliers = motivo_outliers,
    n_nan = as.integer(n_nan),
    n_infinito_positivo = as.integer(n_infinito_positivo),
    n_infinito_negativo = as.integer(n_infinito_negativo),
    n_fechas_resumidas = if (startsWith(cuantitativos$clase, "fecha") &&
        !is.null(cuantitativos$n_fechas_resumidas)) {
      cuantitativos$n_fechas_resumidas
    } else NA_integer_,
    n_fechas_excluidas_granularidad = if (
      startsWith(cuantitativos$clase, "fecha") &&
        !is.null(cuantitativos$n_fechas_excluidas_granularidad)
    ) cuantitativos$n_fechas_excluidas_granularidad else NA_integer_,
    n_valores_excluidos_resumen = if (
      !is.null(cuantitativos$n_valores_excluidos_resumen)
    ) cuantitativos$n_valores_excluidos_resumen else NA_integer_,
    estado_resumen_cuantitativo = if (!is.null(cuantitativos$estado)) {
      cuantitativos$estado
    } else "calculados",
    # Cuantos valores entraron al resumen, CONTADOS aca, donde se calculan.
    #
    # La fila de cobertura publicaba esta cifra derivandola con aritmetica sobre
    # otros tres campos -`aplicables - faltantes - excluidos`- y por eso se
    # rompio DOS veces: primero porque el `NaN` estaba en dos de los tres
    # sumandos y se restaba dos veces, y despues porque el arreglo de eso supuso
    # que todo `NaN` esta en `n_faltantes`, que es falso para un `"NaN"` de
    # TEXTO -`is.na("NaN")` es FALSE, asi que la columna lo cuenta como presente-
    # y la cifra salia sobrestimada. Medido: `c("10","20","NaN","30","40")`
    # publicaba "se calculo sobre 5" con `media = 25`, que es el promedio de
    # cuatro valores.
    #
    # Una cifra derivada de otras tres hereda todas sus definiciones de
    # presencia. Contarla en el unico lugar que sabe cuantos valores uso -este-
    # la vuelve una medicion. No se publica en la fila: viaja en el resultado de
    # la columna para que la cobertura la lea.
    n_resumidos = length(valores)
  )
}

.umbral_vocabulario_barato <- 0.5
.umbral_vocabulario_codificacion <- 0.8

# La deduplicacion se activa segun la cardinalidad. En 240.000 filas, cuatro
# valores fueron claramente favorables y 132.610 valores ya hicieron mas caro
# el recorrido para predicados baratos; la reparacion de codificacion siguio
# siendo favorable hasta una relacion de 0,8.
.vocabulario_texto <- function(textos, umbral, valores = NULL) {
  n <- length(textos)
  presentes <- !is.na(textos)
  if (is.null(valores)) valores <- unique(textos[presentes])
  distintos <- length(valores)
  usar <- n > 1L && distintos > 0L && distintos / n <= umbral
  if (usar) {
    indices <- match(textos, valores)
  } else {
    valores <- textos
    indices <- seq_len(n)
  }
  list(
    valores = valores,
    indices = indices,
    usar = usar,
    n_distintos = distintos
  )
}

.componentes_numero_texto_optimizado <- function(textos, valores = NULL) {
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_barato, valores = valores
  )
  partes <- .componentes_numero_texto(vocabulario$valores)
  if (isTRUE(vocabulario$usar)) {
    partes <- lapply(partes, function(valores) valores[vocabulario$indices])
  }
  partes
}

.analizar_codificacion_vocabulario <- function(textos, valores = NULL,
                                              max_iteraciones = 20L,
                                              con_cache = FALSE) {
  if (!is.null(valores) && length(textos) > 1L &&
      length(valores) / length(textos) > .umbral_vocabulario_codificacion) {
    return(.analizar_codificacion(
      textos, max_iteraciones = max_iteraciones
    ))
  }
  vocabulario <- .vocabulario_texto(
    textos, .umbral_vocabulario_codificacion, valores = valores
  )
  if (!isTRUE(vocabulario$usar)) {
    return(.analizar_codificacion(
      textos, max_iteraciones = max_iteraciones
    ))
  }
  unico <- .analizar_codificacion(
    vocabulario$valores, max_iteraciones = max_iteraciones
  )
  reparados <- rep(NA_character_, length(textos))
  estados <- rep(NA_character_, length(textos))
  pasos <- vector("list", length(textos))
  consumidas <- vector("list", length(textos))
  presentes <- !is.na(vocabulario$indices)
  reparados[presentes] <- unico$reparados[vocabulario$indices[presentes]]
  estados[presentes] <- unico$estados[vocabulario$indices[presentes]]
  pasos[presentes] <- unico$pasos[vocabulario$indices[presentes]]
  consumidas[presentes] <- unico$consumidas[vocabulario$indices[presentes]]
  clasificacion <- .ftfy_mascaras_codificacion(textos, reparados, estados)
  reparables <- clasificacion$reparables
  parciales <- clasificacion$parciales
  irreparables <- clasificacion$irreparables
  afectados <- clasificacion$afectados
  evidencia <- .ftfy_evidencia_codificacion(
    textos, reparados, estados, afectados, irreparables
  )
  candidatos_expandidos <- rep(FALSE, length(textos))
  candidatos_expandidos[presentes] <-
    unico$candidatos[vocabulario$indices[presentes]]
  salida <- list(
    n = sum(afectados), n_reparables = sum(reparables),
    n_reparables_parcialmente = sum(parciales),
    n_irreparables = sum(irreparables),
    n_no_se_pudo = sum(clasificacion$no_se_pudo),
    evidencia = evidencia, reparados = reparados, estados = estados,
    pasos = pasos, afectados = afectados,
    candidatos = candidatos_expandidos, valores = as.character(textos),
    consumidas = consumidas,
    estado = .ftfy_estado_agregado(estados)
  )
  if (isTRUE(con_cache)) {
    # Estos campos conservan la salida sin expandir para que la mascara de
    # invisibles la alinee con el mismo vocabulario sin volver a reparar. El
    # resto del resultado sigue alineado a `textos` para los conteos publicados.
    salida$valores <- vocabulario$valores
    salida$candidatos_valores <- unico$candidatos
    salida$reparados_valores <- unico$reparados
    salida$estados_valores <- unico$estados
    salida$consumidas_valores <- unico$consumidas
  }
  salida
}

# El texto de una celda en la forma con que se lo lee como numero: declarado
# (`.declarar_utf8_valido()`), sin lo que no es UTF-8 valido -que no es un
# numero en ninguna sesion- y recortado. La comparten la descomposicion del
# numero regional y la conversion de tipo de `aplicar()`.
.texto_legible_recortado <- function(x) {
  texto <- .texto_para_operar(as.character(x))
  invalidos <- !is.na(texto) & Encoding(texto) != "latin1" & !validUTF8(texto)
  texto[invalidos] <- NA_character_
  trimws(texto)
}

# La moneda que acompana a un numero escrito como texto. La comparten la forma
# completa de `.componentes_numero_texto()` y la pregunta barata de
# `.inicio_numero_texto()`, que tienen que reconocer lo mismo.
#
# Un simbolo -`U$S` o un simbolo monetario de Unicode (`\p{Sc}`: el peso, el
# euro, la libra)- es moneda delante o detras del numero, pegado o separado. La
# forma europea lo escribe detras, y se contaba solo delante: `1.234,56 $`
# quedaba como texto, sin hallazgo y sin conversion, mientras `$ 1.234,56` se
# convertia; y lo mismo con el euro. `U$S` se escribe pegado igual que `$`.
#
# El espacio junto a un simbolo es cualquier espacio, como en el recorte del
# borde de los validadores: el duro (U+00A0) o el fino (U+202F) con que una
# planilla separa el simbolo es la misma escritura que el espacio ASCII. Se
# escribe con clases de PCRE y no con los caracteres, para que el patron siga
# siendo ASCII y no obligue a leer en UTF-8 una columna que es toda ASCII.
#
# Un codigo de tres mayusculas solo es moneda separado del numero: pegado
# (`35USD`) es un codigo como `12A`.
.simbolo_moneda_numero <- "(?:U\\$S|\\p{Sc})"
.espacio_moneda_numero <- "[[:space:]\\p{Z}]*"
.moneda_delante_numero <- paste0(
  "(?:[[:upper:]]{3}[[:space:]]+|", .simbolo_moneda_numero,
  .espacio_moneda_numero, ")"
)

# La forma completa de un numero escrito como texto, con su moneda o su unidad:
#
# - Moneda: un codigo de tres mayusculas separado del numero por espacios,
#   delante o detras, que ademas tiene que ser ISO 4217 -se valida en
#   `.componentes_numero_texto()`: `LEY 18331` no es un importe-; o un simbolo
#   delante o detras, pegado o separado. Una sola: con moneda delante y detras
#   (`$ 35 $`, `USD 35 EUR`) el valor no es un numero, porque convertirlo
#   obligaria a elegir cual de las dos vale.
# - Unidad: `%` o una abreviatura alfabetica que termina en minuscula. Las
#   mayusculas pegadas al numero (`12A`, `13B`, `500GB`) se reservan para
#   codigos: ese valor no es un numero, traiga o no separador decimal. La
#   unidad puede seguir a una moneda escrita delante (`$ 35 kg`), no a una
#   escrita detras.
#
# El separador decimal puede abrir el numero: `,5` es `0,5` y `.5` es `0.5`,
# con la misma evidencia de convencion que su gemelo con el cero escrito.
#
# Los patrones se arman una vez y no en cada llamada: el lector se llama varias
# veces por columna en cada perfil. Quien lo usa le pasa el vector entero y no
# valor por valor: `.detectar_multivaluados()` lo llamaba una vez por cada celda
# con coma, y en una columna de 200.000 decimales eso era casi todo el perfil.
.unidad_detras_numero <- "[[:space:]]*(?:%|[[:alpha:]]*[[:lower:]])"
.moneda_detras_numero <- paste0(
  "[[:space:]]+[[:upper:]]{3}|", .espacio_moneda_numero, .simbolo_moneda_numero
)
.forma_numero_texto <- function(numero) {
  paste0(
    "^(?:", .moneda_delante_numero, "[+-]?", numero, "(?:",
    .unidad_detras_numero, ")?|[+-]?", numero, "(?:", .unidad_detras_numero,
    "|", .moneda_detras_numero, ")?)$"
  )
}
.patron_coma_numero_texto <- .forma_numero_texto(
  "(?:(?:[0-9]{1,3}(?:\\.[0-9]{3})+|[0-9]+)(?:,[0-9]+)?|,[0-9]+)"
)
.patron_punto_numero_texto <- .forma_numero_texto(
  "(?:(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?|\\.[0-9]+)"
)
.patron_prefijo_moneda_numero <- paste0("^[[:space:]]*", .moneda_delante_numero)
.patron_simbolo_prefijo_numero <- paste0(
  "^[[:space:]]*(", .simbolo_moneda_numero, ")"
)
.patron_simbolo_sufijo_numero <- paste0(
  .espacio_moneda_numero, "(", .simbolo_moneda_numero, ")$"
)

.componentes_numero_texto <- function(x) {
  # La declaracion va antes de la primera primitiva. Sin ella, un
  # importe con el simbolo del euro en UTF-8 sin marca -un CSV leido sin
  # `encoding`- no casaba con `\\p{Sc}` bajo `C`: el perfil proponia
  # `convertir_numero_regional`, la comprobacion del plan la daba por no
  # ejecutable y `aplicar()` no convertia, mientras en una sesion UTF-8 si.
  # Lo que no es UTF-8 valido no es un numero en ninguna sesion: bajo un locale
  # UTF-8 `trimws()` abortaba sobre el y bajo `C` lo leia por bytes.
  texto <- .texto_legible_recortado(x)
  # Lo que acompana a un numero se reconoce como moneda o como unidad, o el
  # valor no es un numero. La forma completa -`compatible`- y la extraccion de
  # mas abajo usan las mismas definiciones. Antes la forma aceptaba cualquier
  # tramo de letras detras del numero y la unidad solo el que termina en
  # minuscula: `12 KG` no se contaba, pero `1.5TB` si -por su punto-, sin
  # unidad que publicar, y la conversion, que lee con esta misma funcion, le
  # quitaba el `TB` a toda la columna sin decirlo. La forma esta escrita en
  # `.forma_numero_texto()`.
  patron_coma <- .patron_coma_numero_texto
  patron_punto <- .patron_punto_numero_texto
  # La notacion cientifica (`1e3`, `1,5E+06`) no entra en estas formas: es un
  # numero para la inferencia y el resumen, que la leen con
  # `.leer_numeros_texto()`, pero no un numero escrito con separador regional,
  # unidad o moneda. Aca no se cuenta, y la conversion regional, que lee con
  # esta misma funcion, no la convierte.
  compatible_coma <- !is.na(texto) & grepl(patron_coma, texto, perl = TRUE)
  compatible_punto <- !is.na(texto) & grepl(patron_punto, texto, perl = TRUE)
  compatible <- compatible_coma | compatible_punto
  patron_prefijo <- .patron_prefijo_moneda_numero
  patron_codigo_prefijo <- "^[[:space:]]*([[:upper:]]{3})[[:space:]]+"
  codigo_prefijo <- compatible & grepl(
    patron_codigo_prefijo, texto, perl = TRUE
  )
  codigo_delante <- rep("", length(texto))
  codigo_delante[codigo_prefijo] <- sub(
    paste0(patron_codigo_prefijo, ".*$"), "\\1", texto[codigo_prefijo],
    perl = TRUE
  )
  patron_codigo_sufijo <- "[[:space:]]+([[:upper:]]{3})$"
  codigo_sufijo <- compatible & grepl(patron_codigo_sufijo, texto, perl = TRUE)
  codigo_detras <- rep("", length(texto))
  codigo_detras[codigo_sufijo] <- sub(
    paste0("^.*", patron_codigo_sufijo), "\\1", texto[codigo_sufijo],
    perl = TRUE
  )
  # Tres mayusculas son una moneda solo si son un codigo ISO 4217, con la lista
  # de `validar_iso4217()`. Las que no lo son no acompanan a un numero: el
  # valor deja de ser compatible, y con eso ni se cuenta ni se convierte.
  codigos <- unique(c(
    codigo_delante[codigo_prefijo], codigo_detras[codigo_sufijo]
  ))
  if (length(codigos)) {
    falsos <- codigos[!(validar_iso4217(codigos) %in% TRUE)]
    if (length(falsos)) {
      sin_moneda <- (codigo_prefijo & codigo_delante %in% falsos) |
        (codigo_sufijo & codigo_detras %in% falsos)
      compatible_coma[sin_moneda] <- FALSE
      compatible_punto[sin_moneda] <- FALSE
      compatible[sin_moneda] <- FALSE
      codigo_prefijo[sin_moneda] <- FALSE
      codigo_sufijo[sin_moneda] <- FALSE
      codigo_delante[sin_moneda] <- ""
    }
  }
  moneda <- codigo_delante
  # El simbolo se publica tal como esta escrito, delante o detras: `$ 10` y
  # `20 $` son la misma moneda, y `$` y `U$S` son dos. `(?s)` deja que el resto
  # cruce un salto de linea interno (`$ 35\nkg` responde a la forma), que sin
  # el dejaba el texto entero como moneda. Las extracciones corren solo si hay
  # a quien extraerle: cada patron se compila en cada llamada.
  simbolo_prefijo <- compatible & grepl(
    .patron_simbolo_prefijo_numero, texto, perl = TRUE
  )
  if (any(simbolo_prefijo)) {
    moneda[simbolo_prefijo] <- sub(
      paste0("(?s)", .patron_simbolo_prefijo_numero, ".*$"), "\\1",
      texto[simbolo_prefijo], perl = TRUE
    )
  }
  simbolo_sufijo <- compatible & grepl(
    .patron_simbolo_sufijo_numero, texto, perl = TRUE
  )
  if (any(simbolo_sufijo)) {
    moneda[simbolo_sufijo] <- sub(
      paste0("(?s)^.*?", .patron_simbolo_sufijo_numero), "\\1",
      texto[simbolo_sufijo], perl = TRUE
    )
  }
  sin_prefijo <- codigo_sufijo & !nzchar(moneda)
  moneda[sin_prefijo] <- codigo_detras[sin_prefijo]
  tiene_moneda <- nzchar(moneda)
  cuerpo <- texto
  cuerpo <- sub(patron_prefijo, "", cuerpo, perl = TRUE)
  # El simbolo escrito detras se quita antes de buscar la unidad: lo que queda
  # es el numero, como cuando la moneda va delante.
  if (any(simbolo_sufijo)) {
    cuerpo[simbolo_sufijo] <- sub(
      .patron_simbolo_sufijo_numero, "", cuerpo[simbolo_sufijo], perl = TRUE
    )
  }
  # La unidad es el tramo alfabetico entero cuando termina en minuscula, o `%`:
  # lo mismo que `unidad_detras` acepta arriba. Lo otro que la forma admite
  # detras del numero es la moneda: el codigo, que no es una unidad, y el
  # simbolo, ya quitado.
  tiene_unidad <- compatible & grepl("(?:%|[[:lower:]])$", cuerpo, perl = TRUE)
  unidad <- rep("", length(texto))
  unidad[tiene_unidad] <- sub(
    "^.*?[[:space:]]*(%|[[:alpha:]]+)$", "\\1", cuerpo[tiene_unidad],
    perl = TRUE
  )
  sin_unidad <- sub("[[:space:]]*(?:%|[[:alpha:]]+)$", "", cuerpo, perl = TRUE)
  tiene_coma <- grepl(",", sin_unidad, fixed = TRUE, useBytes = TRUE)
  tiene_punto <- grepl(".", sin_unidad, fixed = TRUE, useBytes = TRUE)
  punto_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:\\.[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  coma_tres <- grepl(
    "^[+-]?[0-9]{1,3}(?:,[0-9]{3})+$", sin_unidad, perl = TRUE
  )
  # Un separador seguido de tres digitos es evidencia cuando no puede agrupar
  # miles: detras de cuatro digitos o mas, o abriendo el numero. `,500` es
  # medio, igual que `0,500`; con uno, dos o mas de tres digitos detras el
  # separador inicial ya es evidencia por la expresion anterior.
  evidencia_coma <- compatible_coma & (
    grepl("\\.[0-9]{3}(?:\\.[0-9]{3})*,[0-9]+$", sin_unidad, perl = TRUE) |
      grepl(",[0-9]{1,2}$|,[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?(?:[0-9]{4,})?,[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  evidencia_punto <- compatible_punto & (
    grepl(",[0-9]{3}(?:,[0-9]{3})*\\.[0-9]+$", sin_unidad, perl = TRUE) |
      grepl("\\.[0-9]{1,2}$|\\.[0-9]{4,}$", sin_unidad, perl = TRUE) |
      grepl("^[+-]?(?:[0-9]{4,})?\\.[0-9]{3}$", sin_unidad, perl = TRUE)
  )
  especial <- tiene_coma | tiene_punto | tiene_moneda | nzchar(unidad)
  list(
    texto = texto, compatible = compatible, especial = especial,
    cuerpo = sin_unidad, unidad = unidad, tiene_coma = tiene_coma,
    tiene_punto = tiene_punto, punto_tres = punto_tres, coma_tres = coma_tres,
    compatible_coma = compatible_coma, compatible_punto = compatible_punto,
    evidencia_coma = evidencia_coma, evidencia_punto = evidencia_punto,
    moneda = moneda
  )
}

# La notacion cientifica: una mantisa con a lo sumo un separador y su
# exponente, con signo o sin el (`1e5`, `1.2E-05`, `1,5e+3`). La inferencia la
# cuenta como numero antes de conjeturar un identificador -tiene letras y
# digitos, y con valores distintos salia `identificador` o `doble` segun el
# signo del exponente-, y el lector resuelve su separador sin la convencion.
.forma_cientifica_numero_texto <- function(x) {
  posicion <- regexpr(
    "^[+-]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)[eE][+-]?[0-9]+$",
    x, perl = TRUE
  )
  !is.na(x) & posicion > 0L
}

# Un numero que empieza con el separador decimal: `.5`, `,5`, `-.25`.
.separador_inicial_numero_texto <- function(x) {
  !is.na(x) & grepl("^[+-]?[.,][0-9]+$", x, perl = TRUE)
}

# El separador que abre un numero y el de la mantisa de la notacion cientifica
# solo pueden ser decimales: ninguna convencion los lee como miles. Con la
# regla general, `.5` en una columna de coma decimal perdia el punto -quedaba
# 5- y `3.5e2` quedaba sin lectura, mientras la inferencia contaba los dos.
#
# `con_miles` son los valores que traen el separador de miles de la convencion:
# los unicos cuya lectura cambia, y los unicos sobre los que se pregunta. El
# lector pasa por aca en cada lectura de la columna, y en una columna de punto
# decimal sin ninguna coma no cuesta nada.
.separador_solo_decimal <- function(x, con_miles) {
  if (any(con_miles)) {
    candidatos <- x[con_miles]
    con_miles[con_miles] <- .separador_inicial_numero_texto(candidatos) |
      .forma_cientifica_numero_texto(candidatos)
  }
  con_miles
}

# Si el texto empieza como un numero escrito como texto: la moneda delante, el
# signo, y un digito o el separador decimal que abre el numero. Es la pregunta
# barata con que el analisis decide si mira la columna (al menos el 80 % de los
# valores presentes), y la hacen igual sus dos caminos. La moneda delante es la
# misma que acepta la forma completa (`.moneda_delante_numero`): si no, un
# valor que la forma cuenta podia quedar fuera antes de preguntarle.
.patron_inicio_numero_texto <- paste0(
  "^[[:space:]]*", .moneda_delante_numero, "?[+-]?(?:[0-9]|[.,][0-9])"
)

.inicio_numero_texto <- function(x) {
  grepl(.patron_inicio_numero_texto, x, perl = TRUE)
}

# Resuelve los separadores de un numero escrito como texto segun la convencion
# decimal: con `decimal_coma` el punto agrupa miles y la coma es decimal; con
# `decimal_punto`, al reves. Es el texto que la remediacion le pasa a
# `as.numeric()` -`.texto_regional_normalizado()`- y el que el resumen
# cuantitativo lee de la misma columna. Escrita una vez: el perfil publicaba
# `numero_texto_convencion = "decimal_coma"` y resumia `1.500` como 1,5 con un
# `sub(",", ".")` propio, mientras la limpieza del mismo paquete, con la
# convencion publicada, convertia el mismo texto en 1500.
.normalizar_separadores_numero <- function(texto, convencion,
                                           separador_libre = NULL) {
  coma <- identical(convencion, "decimal_coma")
  if (!coma && !identical(convencion, "decimal_punto")) return(texto)
  # Quien ya lo calculo sobre el mismo vector lo pasa, para no repetir la
  # pasada en cada lectura de la columna.
  if (is.null(separador_libre)) {
    separador_libre <- .separador_solo_decimal(
      texto,
      !is.na(texto) & grepl(
        if (coma) "." else ",", texto, fixed = TRUE, useBytes = TRUE
      )
    )
  }
  normalizado <- if (coma) {
    sub(",", ".", gsub(".", "", texto, fixed = TRUE), fixed = TRUE)
  } else {
    gsub(",", "", texto, fixed = TRUE)
  }
  if (any(separador_libre)) {
    normalizado[separador_libre] <- sub(
      ",", ".", texto[separador_libre], fixed = TRUE
    )
  }
  normalizado
}

# El numero que el perfil lee en cada texto de una columna inferida entera o
# doble, con la convencion decimal que el MISMO perfil publica en
# `numero_texto_convencion` -si no llega, se calcula con la misma funcion que la
# publica-.
#
# - `decimal_coma` y `decimal_punto`: la normalizacion compartida. El separador
#   de miles solo vale agrupando de a tres; un punto que no agrupa en una columna
#   de coma decimal no tiene lectura con esa convencion y queda afuera. El
#   separador que abre el numero y el de una mantisa con exponente no agrupan
#   nunca: ver `.separador_solo_decimal()`.
# - `ambigua` y `mixta`: la columna no decide. Cada valor se lee con su PROPIA
#   evidencia -`2,5` y `1.234,5` solo admiten una lectura- y el que no la tiene
#   -`1.500`, `1,500`- queda afuera: elegir seria inventar, igual que con una
#   fecha que dos formatos leen distinto.
# - sin separadores, o sin convencion evaluada: la lectura de siempre.
#
# Y en todas, dos reglas que valen para cualquier convencion:
#
# - un valor cuyo primer grupo de miles empieza con cero -`099.123.456`, o
#   `0.500` con coma decimal- no es un importe en ninguna: es un telefono o un
#   codigo, y quitarle los separadores lo volvia `99123456`;
# - solo se lee la FORMA que la inferencia cuenta como numero
#   (`.formas_numero_texto()`): el resumen recorre la columna entera y la
#   inferencia puede haber mirado una muestra, y `as.numeric()` lee mas de lo
#   que la inferencia valido -`0x1A` es 26, `5.` es 5-. Lo que entraba asi al
#   maximo y a la media lo hacia sin declararse.
#
# Lo que queda afuera vuelve `NA`, y quien llama lo cuenta y lo declara
# (`n_valores_excluidos_resumen`), como cualquier texto que no llega a numero.
# `NaN` e `Inf` escritos siguen siendo `NaN` e `Inf`: el resumen los cuenta en
# sus propios campos y no en el maximo.
#
# `recortado` lo pasa quien ya recorto el texto: `trimws()` es la operacion mas
# cara de esta lectura, y la columna se lee varias veces por perfil.
.normalizar_numeros_texto <- function(texto, convencion = NULL,
                                      recortado = FALSE) {
  if (!recortado) texto <- trimws(as.character(texto))
  con_separador <- !is.na(texto) & grepl("[.,]", texto, perl = TRUE)
  if (!any(con_separador)) {
    return(texto)
  }
  if (is.null(convencion)) {
    convencion <- .analizar_numeros_texto(texto)$convencion
  }
  convencion <- if (length(convencion) == 1L && !is.na(convencion)) {
    as.character(convencion)
  } else ""
  if (identical(convencion, "es-UY")) convencion <- "decimal_coma"
  normalizado <- texto
  if (convencion %in% c("decimal_coma", "decimal_punto")) {
    coma <- identical(convencion, "decimal_coma")
    miles <- if (coma) "." else ","
    agrupado <- if (coma) .patron_miles_coma else .patron_miles_punto
    con_miles <- con_separador & grepl(miles, texto, fixed = TRUE, useBytes = TRUE)
    separador_libre <- .separador_solo_decimal(texto, con_miles)
    sin_lectura <- con_miles & !separador_libre &
      !grepl(agrupado, texto, perl = TRUE)
    normalizado[sin_lectura] <- NA_character_
    normalizado <- .normalizar_separadores_numero(
      normalizado, convencion, separador_libre
    )
  } else if (convencion %in% c("ambigua", "mixta")) {
    partes <- .componentes_numero_texto(texto[con_separador])
    propia <- rep(NA_character_, sum(con_separador))
    propia[partes$evidencia_coma & !partes$evidencia_punto] <- "decimal_coma"
    propia[partes$evidencia_punto & !partes$evidencia_coma] <- "decimal_punto"
    leidos <- texto[con_separador]
    # La misma regla que con la convencion de la columna, con la del valor:
    # los miles agrupan de a tres y sin cero adelante. `099.123,45` trae la
    # coma y su primer grupo empieza con cero; `1.23,5` no agrupa.
    cero <- (propia %in% "decimal_coma" &
               grepl(".", leidos, fixed = TRUE, useBytes = TRUE) &
               !grepl(.patron_miles_coma, leidos, perl = TRUE)) |
      (propia %in% "decimal_punto" &
         grepl(",", leidos, fixed = TRUE, useBytes = TRUE) &
         !grepl(.patron_miles_punto, leidos, perl = TRUE))
    propia[cero] <- NA_character_
    for (lectura in c("decimal_coma", "decimal_punto")) {
      cuales <- !is.na(propia) & propia == lectura
      leidos[cuales] <- .normalizar_separadores_numero(leidos[cuales], lectura)
    }
    leidos[is.na(propia)] <- NA_character_
    normalizado[con_separador] <- leidos
  } else {
    normalizado <- sub(",", ".", texto, fixed = TRUE)
  }
  normalizado
}

# El lector comun: lo usan la inferencia del tipo, el resumen, la frase de
# `tipo_declarado_distinto` y la conversion de tipo, y por eso cuentan y leen lo
# mismo. No atraviesa una unidad ni una moneda: `12 kg` y `USD 35` no son
# numeros para el, sino numeros escritos como texto, que cuenta y convierte
# `.componentes_numero_texto()`.
.leer_numeros_texto <- function(texto, convencion = NULL) {
  texto <- trimws(as.character(texto))
  normalizado <- .normalizar_numeros_texto(texto, convencion, recortado = TRUE)
  .solo_formas_numero(suppressWarnings(as.numeric(normalizado)), texto)
}

# Los miles agrupados de a tres, con el primer grupo sin cero adelante, y la
# parte decimal con el otro separador. Los comparten la inferencia y el lector.
.patron_miles_coma <- "^[+-]?[1-9][0-9]{0,2}(?:\\.[0-9]{3})+(?:,[0-9]+)?$"
.patron_miles_punto <- "^[+-]?[1-9][0-9]{0,2}(?:,[0-9]{3})+(?:\\.[0-9]+)?$"

# Las formas que la inferencia cuenta como numero, antes de la convencion: un
# solo separador con su exponente (`1234,56`, `1.5e3`, `.5`), o los miles
# agrupados de a tres sin cero adelante (`1.234,56`, `1,234.56`, `1.234.567`).
# Que una forma este aca no dice que se lea: la convencion decide. Dice que lo
# que no esta no se lee nunca.
.formas_numero_texto <- function(texto) {
  !is.na(texto) & (
    grepl("^[+-]?(?:[0-9]+(?:[.,][0-9]+)?|[.,][0-9]+)(?:[eE][+-]?[0-9]+)?$",
          texto, perl = TRUE) |
      grepl(.patron_miles_coma, texto, perl = TRUE) |
      grepl(.patron_miles_punto, texto, perl = TRUE)
  )
}

.solo_formas_numero <- function(valores, texto) {
  leidos <- is.finite(valores)
  if (any(leidos)) {
    fuera <- leidos
    fuera[leidos] <- !.formas_numero_texto(texto[leidos])
    valores[fuera] <- NA_real_
  }
  valores
}

.frecuencias_unidades_numero <- function(partes, presentes) {
  unidades <- partes$unidad[presentes & partes$compatible & nzchar(partes$unidad)]
  if (!length(unidades)) return(stats::setNames(integer(), character()))
  niveles <- unique(unidades)
  salida <- tabulate(match(unidades, niveles), nbins = length(niveles))
  stats::setNames(as.integer(salida), niveles)
}

.frecuencias_monedas_numero <- function(partes, presentes) {
  monedas <- partes$moneda[presentes & partes$compatible & nzchar(partes$moneda)]
  if (!length(monedas)) return(stats::setNames(integer(), character()))
  niveles <- unique(monedas)
  salida <- tabulate(match(monedas, niveles), nbins = length(niveles))
  stats::setNames(as.integer(salida), niveles)
}

# El tipo de cada parte, para un vector de partes: la deteccion pregunta por
# todas las partes de la columna de una vez. Cada pregunta se hace solo sobre lo
# que las anteriores no resolvieron, en el orden de siempre: entero,
# alfanumerico (solo letras y digitos, con al menos uno de cada uno),
# identificador con separador y, si no, texto.
.tipo_parte_multivaluada <- function(valores) {
  tipos <- rep("texto", length(valores))
  if (!length(valores)) return(tipos)
  numerico <- grepl("^[+]?[0-9]+$", valores, perl = TRUE)
  tipos[numerico] <- "numerico"
  pendientes <- which(!numerico)
  alfanumerico <- pendientes[
    grepl("^[[:alnum:]]+$", valores[pendientes], perl = TRUE)
  ]
  alfanumerico <- alfanumerico[
    grepl("[[:alpha:]]", valores[alfanumerico], perl = TRUE)
  ]
  alfanumerico <- alfanumerico[
    grepl("[0-9]", valores[alfanumerico], perl = TRUE)
  ]
  tipos[alfanumerico] <- "alfanumerico"
  pendientes <- pendientes[tipos[pendientes] == "texto"]
  identificador <- pendientes[grepl(
    "^[+]?[0-9]{4,8}[-./][0-9]+$", valores[pendientes], perl = TRUE
  )]
  tipos[identificador] <- "identificador"
  tipos
}

# Las partes recortadas de cada celda, en una pasada sobre todas las celdas.
# Devuelve cuales valen como lista -al menos dos partes y ninguna vacia-, sus
# partes en el orden de las celdas y cuantas partes tiene cada una.
.partir_celdas_multivaluadas <- function(textos, delimitador) {
  piezas <- strsplit(textos, delimitador, fixed = TRUE)
  largos <- lengths(piezas)
  partes <- trimws(unlist(piezas, use.names = FALSE))
  celda <- rep.int(seq_along(textos), largos)
  vacias <- tabulate(celda[!nzchar(partes)], nbins = length(textos))
  validas <- largos >= 2L & vacias == 0L
  list(
    validas = validas,
    partes = partes[validas[celda]],
    largos = largos[validas]
  )
}

.patron_partes_multivaluada <- function(valores, expandir = FALSE) {
  # El patron de cada parte, con la misma generalizacion que publica
  # `descubrir_patrones()`: solo se compara la homogeneidad de las partes, asi
  # que no hace falta su tabla. Se la armaba y se la descartaba, y en una
  # columna de listas era la mitad del costo de esta comparacion.
  .generalizar_a_patron(
    valores, distinguir_mayusculas = TRUE, expandir = expandir
  )
}

.detectar_multivaluados <- function(x) {
  vacio <- list()
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  presentes <- !is.na(textos) & nzchar(trimws(textos))
  n_presentes <- sum(presentes)
  if (n_presentes < 2L) return(vacio)

  # La columna se mira entera en cada pasada y no celda por celda: cada llamada
  # chica a una expresion regular tiene un costo fijo -en Windows, decenas de
  # microsegundos-, y una columna de 200.000 celdas con coma las multiplicaba
  # por cientos de miles. No cambia ninguna respuesta. R elige como lee un
  # vector mirando las marcas de codificacion de todos sus valores, pero las
  # clases de estas expresiones son ASCII y la columna llega de
  # `.texto_analizable()`, con todo lo que no es ASCII marcado UTF-8 y valido.
  # Cada pregunta descarta un delimitador sin dejar rastro, asi que van de la
  # mas barata a la mas cara.
  maximo_celdas <- max(1L, floor(n_presentes / 2L))
  # Las celdas sin ningun delimitador no dependen del delimitador que se mira:
  # se leen una sola vez, la primera vez que hacen falta.
  resto <- NULL
  candidatos <- list()
  for (delimitador in c(",", ";", "|")) {
    indices <- which(presentes & grepl(delimitador, textos, fixed = TRUE))
    if (!length(indices)) next
    if (delimitador == ",") {
      # No convertir un decimal o un separador de miles en una lista de
      # valores: si la celda completa ya es un numero regional, se excluye. Va
      # antes de partir las celdas: en una columna de decimales con coma no
      # queda ninguna, y no hay nada que partir.
      numericas <- .componentes_numero_texto(textos[indices])$compatible
      indices <- indices[!numericas]
      if (!length(indices)) next
    }
    division <- .partir_celdas_multivaluadas(textos[indices], delimitador)
    if (!any(division$validas)) next
    indices <- indices[division$validas]
    # Si mas de la mitad de las celdas presentes traen el delimitador, es la
    # forma de la columna y no una lista dentro de ella.
    if (length(indices) > maximo_celdas) next
    partes <- division$partes
    tipo_dominante <- unique(.tipo_parte_multivaluada(partes))
    if (length(tipo_dominante) != 1L ||
        !tipo_dominante %in% c("numerico", "alfanumerico", "identificador")) {
      next
    }
    # Un entero se compara por su patron comprimido (`9+`), que admite largos
    # parecidos; lo demas, por el expandido, que exige el mismo largo.
    expandir <- tipo_dominante != "numerico"
    patron_partes <- .patron_partes_multivaluada(partes, expandir = expandir)
    homogeneas <- if (tipo_dominante == "numerico") {
      length(unique(patron_partes)) == 1L &&
        (max(nchar(partes)) - min(nchar(partes)) <= 2L)
    } else {
      length(unique(patron_partes)) == 1L &&
        length(unique(nchar(partes))) == 1L
    }
    if (!homogeneas) next

    if (is.null(resto)) {
      indices_resto <- which(presentes & !grepl(
        "[,;|]", textos, perl = TRUE
      ))
      resto <- trimws(textos[indices_resto])
      tipos_resto <- .tipo_parte_multivaluada(resto)
    }
    if (length(resto)) {
      resto_dominante <- resto[tipos_resto == tipo_dominante]
      if (!length(resto_dominante)) next
      patrones_resto <- .patron_partes_multivaluada(
        resto_dominante, expandir = expandir
      )
      if (!all(patrones_resto %in% unique(patron_partes))) next
    }
    candidatos[[length(candidatos) + 1L]] <- list(
      delimitador = delimitador,
      indices = indices,
      n_celdas = length(indices),
      valores_por_celda = table(division$largos),
      n_valores = length(partes)
    )
  }
  if (!length(candidatos)) return(vacio)
  indices <- .sort_por_bytes(unique(unlist(lapply(candidatos, `[[`, "indices"))))
  distribuciones <- lapply(candidatos, `[[`, "valores_por_celda")
  niveles <- .sort_por_bytes(unique(as.integer(unlist(lapply(
    distribuciones, names
  )))))
  frecuencias_valores <- stats::setNames(vapply(niveles, function(n) {
    sum(vapply(distribuciones, function(distribucion) {
      # Una `table` aborta cuando se le pide un nombre que no tiene, en lugar
      # de devolver `NULL`. Con listas de tres valores separadas por `,` y de
      # dos separadas por `;`, la de `,` no tiene `2`, y `perfilar()` abortaba
      # la tabla entera.
      nombre <- as.character(n)
      if (nombre %in% names(distribucion)) {
        as.integer(distribucion[[nombre]])
      } else {
        0L
      }
    }, integer(1L)))
  }, integer(1L)), as.character(niveles))
  list(list(
    delimitador = paste(vapply(candidatos, `[[`, character(1L), "delimitador"),
                        collapse = " / "),
    indices = indices,
    n_celdas = length(indices),
    valores_por_celda = frecuencias_valores,
    n_valores = sum(vapply(candidatos, `[[`, numeric(1L), "n_valores"))
  ))
}

# Las dos formas de analizar numeros escritos como texto -la directa y la que
# pasa por el vocabulario- comparten toda la interpretacion: convencion decimal,
# unidad y moneda. Lo unico que cambia entre ellas es COMO se obtienen `partes`,
# `presentes` y `especiales`; una vez que estan, la lectura es la misma.
#
# Estaba escrita dos veces, cuarenta y ocho lineas cada una. No es solo
# repeticion: las dos tienen que dar el mismo veredicto sobre los mismos
# valores, o la misma columna se describiria distinto segun por que camino se
# la miro.
.interpretar_numeros_texto <- function(partes, presentes, especiales,
                                      n_presentes) {
  hay_evidencia_coma <- any(partes$evidencia_coma[presentes])
  hay_evidencia_punto <- any(partes$evidencia_punto[presentes])
  convencion <- if (hay_evidencia_coma && hay_evidencia_punto) {
    "mixta"
  } else if (hay_evidencia_coma) {
    "decimal_coma"
  } else if (hay_evidencia_punto) {
    "decimal_punto"
  } else if (any((partes$punto_tres | partes$coma_tres) & especiales)) {
    "ambigua"
  } else {
    "sin_separadores"
  }
  compatibles_convencion <- switch(
    convencion,
    decimal_coma = partes$compatible_coma,
    decimal_punto = partes$compatible_punto,
    sin_separadores = partes$compatible,
    partes$compatible
  )
  ambiguos <- convencion %in% c("ambigua", "mixta") |
    any(presentes & !compatibles_convencion)
  unidades <- unique(partes$unidad[presentes & partes$compatible])
  unidades_no_vacias <- unidades[nzchar(unidades)]
  unidad_consistente <- length(unidades_no_vacias) <= 1L &&
    !(length(unidades_no_vacias) && any(!nzchar(unidades)))
  monedas <- unique(partes$moneda[presentes & partes$compatible])
  monedas_no_vacias <- monedas[nzchar(monedas)]
  moneda_consistente <- length(monedas_no_vacias) <= 1L &&
    !(length(monedas_no_vacias) && any(!nzchar(monedas)))
  compatibles <- sum(presentes & partes$compatible)
  list(
    n = sum(especiales),
    proporcion = if (n_presentes) compatibles / n_presentes else NA_real_,
    ambiguo = isTRUE(ambiguos),
    seguro = compatibles == n_presentes && !isTRUE(ambiguos) &&
      unidad_consistente && moneda_consistente,
    evidencia = paste(
      .comillas_sin_locale(utils::head(unique(partes$texto[especiales]), 6L), quote = '"'),
      collapse = "; "
    ),
    unidad = if (length(unidades_no_vacias) == 1L) unidades_no_vacias else "",
    moneda = if (length(monedas_no_vacias) == 1L) monedas_no_vacias else "",
    convencion = convencion,
    unidades = .frecuencias_unidades_numero(partes, presentes),
    monedas = .frecuencias_monedas_numero(partes, presentes),
    n_presentes = n_presentes
  )
}

# Si el perfil lee la columna como numeros escritos como texto: hay alguno con
# separador regional, unidad o moneda, y al menos el 80 % de los valores
# presentes responde a esa forma. Es la condicion con que se publica
# `numero_como_texto`, y por eso la que manda la conversion del plan por la via
# regional. La preguntan el hallazgo, su cobertura y la frase de
# `tipo_declarado_distinto` (`.riesgos_conversion_tipo()`), que tiene que decir
# lo mismo que ese plan.
.fila_con_numeros_como_texto <- function(fila) {
  n <- fila[["n_numeros_texto"]]
  proporcion <- fila[["proporcion_numeros_texto"]]
  length(n) == 1L && isTRUE(n > 0L) && length(proporcion) == 1L &&
    is.finite(proporcion) && proporcion >= 0.8
}

.analizar_numeros_texto_directo <- function(x, umbral_compatibilidad = 0.8) {
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    unidades = stats::setNames(integer(), character()),
    monedas = stats::setNames(integer(), character()), n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  # Una celda de solo espacios no es un valor presente -el perfil la cuenta
  # como ausencia-. Contada, bajaba la proporcion de la columna: con mas de un
  # 20 % de celdas asi la convencion no se evaluaba y `1.500` se leia 1,5. Es
  # presente lo que `trimws()` no deja vacio, preguntado en una sola pasada.
  presentes <- !is.na(textos) &
    grepl("[^ \t\r\n]", textos, perl = TRUE, useBytes = TRUE)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (!n_presentes || !any(grepl("[0-9]", textos[presentes], perl = TRUE))) {
    return(vacio)
  }
  inicio_numerico <- .inicio_numero_texto(textos[presentes])
  # Misma distincion que en `.analizar_numeros_texto()`: `n = 0` afirma "no hay
  # ninguno", y es falso cuando SI los vio y decidio no contarlos. La regla vive
  # en dos funciones -esta es el camino directo- y las dos tienen que decir lo
  # mismo; que estuviera escrita dos veces es como se desincronizan.
  proporcion_inicio <- mean(inicio_numerico)
  if (proporcion_inicio < umbral_compatibilidad) {
    if (proporcion_inicio > 0) vacio$n <- NA_integer_
    return(vacio)
  }
  partes <- .componentes_numero_texto(textos)
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) return(vacio)
  .interpretar_numeros_texto(partes, presentes, especiales, n_presentes)
}

.analizar_numeros_texto <- function(x, umbral_compatibilidad = 0.8,
                                    vocabulario = NULL, n_distintos = NULL,
                                    directo = FALSE) {
  if (isTRUE(directo)) {
    return(.analizar_numeros_texto_directo(x, umbral_compatibilidad))
  }
  vacio <- list(
    n = 0L, proporcion = NA_real_, ambiguo = FALSE, seguro = FALSE,
    evidencia = "", unidad = "", moneda = "", convencion = "",
    unidades = stats::setNames(integer(), character()),
    monedas = stats::setNames(integer(), character()), n_presentes = 0L
  )
  if (!is.character(x) && !is.factor(x)) return(vacio)
  textos <- as.character(x)
  # Una celda de solo espacios no es un valor presente -el perfil la cuenta
  # como ausencia-. Contada, bajaba la proporcion de la columna: con mas de un
  # 20 % de celdas asi la convencion no se evaluaba y `1.500` se leia 1,5. Es
  # presente lo que `trimws()` no deja vacio, preguntado en una sola pasada.
  presentes <- !is.na(textos) &
    grepl("[^ \t\r\n]", textos, perl = TRUE, useBytes = TRUE)
  n_presentes <- sum(presentes)
  vacio$n_presentes <- n_presentes
  if (is.null(vocabulario) && !is.null(n_distintos) &&
      length(textos) > 1L && is.finite(n_distintos) &&
      n_distintos / length(textos) > .umbral_vocabulario_barato) {
    vocabulario_numeros <- list(
      valores = textos, indices = seq_len(length(textos)), usar = FALSE
    )
  } else {
    vocabulario_numeros <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = vocabulario
    )
  }
  if (isTRUE(vocabulario_numeros$usar)) {
    valores_vocabulario <- vocabulario_numeros$valores
    indices_vocabulario <- vocabulario_numeros$indices
    tiene_digitos_vocabulario <- !is.na(valores_vocabulario) &
      grepl("[0-9]", valores_vocabulario, perl = TRUE)
    tiene_digitos <- tiene_digitos_vocabulario[indices_vocabulario]
  } else {
    valores_vocabulario <- textos
    indices_vocabulario <- seq_len(length(textos))
    tiene_digitos <- !is.na(textos) &
      grepl("[0-9]", textos, perl = TRUE)
  }
  if (!n_presentes || !any(tiene_digitos[presentes])) {
    return(vacio)
  }
  if (isTRUE(vocabulario_numeros$usar)) {
    inicio_vocabulario <- .inicio_numero_texto(valores_vocabulario)
    inicio_numerico <- inicio_vocabulario[indices_vocabulario]
    partes <- .componentes_numero_texto_optimizado(
      textos, valores = valores_vocabulario
    )
  } else {
    inicio_numerico <- rep(FALSE, length(textos))
    inicio_numerico[presentes] <- .inicio_numero_texto(textos[presentes])
    partes <- .componentes_numero_texto(textos)
  }
  proporcion_inicio <- mean(inicio_numerico[presentes])
  if (proporcion_inicio < umbral_compatibilidad) {
    # `n = 0` afirma "no hay ninguno", y eso es falso cuando SI los vio y decidio
    # no contarlos: una columna con tres de cuatro valores numericos-como-texto
    # publicaba `n_numeros_texto = 0` al lado de `proporcion_numeros_texto = NA`.
    # Los dos campos decian cosas distintas del mismo hecho -uno "cero", el otro
    # "no lo se"- y el paquete tiene la convencion contraria: lo que no se midio
    # va `NA`, nunca `0`, porque el cero afirma.
    #
    # Se distingue por eso: si no vio NINGUNO, cero es un hecho y se publica; si
    # vio algunos y no llego al umbral, no los midio como tales y va `NA`.
    if (proporcion_inicio > 0) {
      vacio$n <- NA_integer_
    }
    return(vacio)
  }
  especiales <- presentes & partes$compatible & partes$especial
  if (!any(especiales)) {
    return(vacio)
  }
  .interpretar_numeros_texto(partes, presentes, especiales, n_presentes)
}

.codigos_espacios_invisibles <- c(
  0x00A0, 0x1680, 0x2000:0x200A, 0x2028, 0x2029, 0x202F,
  0x205F, 0x3000
)

.codigos_invisibles_eliminables <- c(
  0x00AD, 0x061C, 0x180E, 0x200B, 0x200E, 0x200F, 0x202A:0x202E,
  0x2060, 0x2066:0x2069, 0xFEFF
)

.codigos_invisibles_significativos <- c(0x200C, 0x200D)
.codigos_c0_no_separadores <- c(0:8, 14L:31L)
.codigos_c1 <- 127L:159L
.codigos_salto_linea_set <- 9L:13L
.codigos_control_eliminable_set <- c(
  .codigos_c0_no_separadores, .codigos_c1,
  .codigos_invisibles_eliminables
)
.codigos_control_invisible_set <- c(
  .codigos_c0_no_separadores, .codigos_c1,
  .codigos_espacios_invisibles, .codigos_invisibles_eliminables,
  .codigos_invisibles_significativos
)

.codigos_control_eliminable <- function(codigos) {
  # Los separadores C0 (tabulacion, LF, VT, FF y CR) tienen un diagnostico y
  # una estrategia propios. Los restantes C0/C1 son controles de transporte.
  codigos %in% .codigos_control_eliminable_set
}

.codigos_control_invisible <- function(codigos) {
  # La deteccion informa todos los invisibles, incluidos los espacios Unicode
  # y ZWJ/ZWNJ; la remediacion separa los grupos por neutralidad semantica.
  codigos %in% .codigos_control_invisible_set
}

.predicados_invisibles <- function(textos, codificacion = NULL) {
  n <- length(textos)
  salida <- list(
    control = rep(FALSE, n), eliminable = rep(FALSE, n),
    espacio = rep(FALSE, n), significativo = rep(FALSE, n),
    separador = rep(FALSE, n)
  )
  if (!n) return(salida)
  # En ASCII imprimible no hay puntos Unicode ni controles que detectar. La
  # comprobacion vectorizada conserva el contrato de esta funcion y evita una
  # llamada a `utf8ToInt()` por valor en el camino caliente.
  if (.es_ascii(textos) && !any(grepl(
    "[\\x00-\\x1F\\x7F]", textos, useBytes = TRUE, perl = TRUE
  ))) {
    return(salida)
  }
  consumidos_por_celda <- .ftfy_posiciones_consumidas_vector(
    textos, codificacion = codificacion
  )
  for (i in seq_len(n)) {
    texto <- textos[[i]]
    if (is.na(texto)) next
    # .texto_analizable() sanea los bytes UTF-8 inválidos antes de llegar
    # aquí; por eso esta única pasada no necesita un tryCatch por predicado.
    codigos <- utf8ToInt(texto)
    consumidos <- consumidos_por_celda[[i]]
    if (is.null(consumidos) || length(consumidos) != length(codigos)) {
      consumidos <- rep(FALSE, length(codigos))
    }
    no_ascii <- codigos > 0x7fL
    solo_c1 <- any(codigos %in% 0x80L:0x9fL) &&
      all(codigos[no_ascii] %in% 0x80L:0x9fL) &&
      identical(.ftfy_reparar_uno(texto)$estado, "no_se_pudo")
    salida$control[[i]] <- any(
      .codigos_control_invisible(codigos) & (!consumidos | solo_c1)
    )
    proteccion <- list(
      candidatos = .ftfy_mascara_candidatos(texto),
      mascaras = list(consumidos)
    )
    candidato_inicial <- proteccion$candidatos
    texto_desde_codigos <- function(valores) {
      paste0(intToUtf8(valores, multiple = TRUE), collapse = "")
    }
    decidir_accion <- function(afectados, transformar, valores_nuevos) {
      if (!any(afectados)) return(FALSE)
      candidato_nuevo <- .ftfy_mascara_candidatos(
        texto_desde_codigos(valores_nuevos)
      )
      if (!isTRUE(candidato_inicial) && !isTRUE(candidato_nuevo)) {
        return(TRUE)
      }
      transformar(texto, proteccion)$n > 0L
    }
    eliminable <- .codigos_control_eliminable(codigos) & !consumidos
    if (any(eliminable)) {
      salida$eliminable[[i]] <- decidir_accion(
        eliminable, .quitar_controles_invisibles,
        codigos[!.codigos_control_eliminable(codigos) | consumidos]
      )
    }
    espacios <- codigos %in% .codigos_espacios_invisibles & !consumidos
    if (any(espacios)) {
      nuevos_espacios <- codigos
      nuevos_espacios[espacios] <- 32L
      salida$espacio[[i]] <- decidir_accion(
        espacios, .normalizar_espacios_invisibles, nuevos_espacios
      )
    }
    salida$significativo[[i]] <- any(
      codigos %in% .codigos_invisibles_significativos & !consumidos
    )
    separador <- codigos %in% .codigos_salto_linea_set & !consumidos
    if (any(separador)) {
      nuevos_separadores <- codigos
      nuevos_separadores[separador] <- 32L
      salida$separador[[i]] <- decidir_accion(
        separador, .reemplazar_separadores, nuevos_separadores
      )
    }
  }
  salida
}

# El paquete cubre las entidades HTML habituales en datos en espanol y todas
# las referencias numericas Unicode. No se interpreta cualquier texto entre
# '&' y ';': el nombre debe pertenecer a este mapa.
.entidades_html_comunes <- c(
  quot = "\"", amp = "&", apos = "'", lt = "<", gt = ">", nbsp = "\u00a0",
  iexcl = "\u00a1", cent = "\u00a2", pound = "\u00a3", curren = "\u00a4",
  yen = "\u00a5", sect = "\u00a7", copy = "\u00a9", reg = "\u00ae",
  deg = "\u00b0", plusmn = "\u00b1", para = "\u00b6", middot = "\u00b7",
  laquo = "\u00ab", raquo = "\u00bb", iquest = "\u00bf", euro = "\u20ac",
  Aacute = "\u00c1", Acirc = "\u00c2", Atilde = "\u00c3", Auml = "\u00c4",
  Agrave = "\u00c0", Aring = "\u00c5", Ccedil = "\u00c7",
  Eacute = "\u00c9", Ecirc = "\u00ca", Euml = "\u00cb", Egrave = "\u00c8",
  Iacute = "\u00cd", Icirc = "\u00ce", Iuml = "\u00cf", Igrave = "\u00cc",
  Ntilde = "\u00d1", Oacute = "\u00d3", Ocirc = "\u00d4", Otilde = "\u00d5",
  Ouml = "\u00d6", Ograve = "\u00d2", Uacute = "\u00da", Ucirc = "\u00db",
  Uuml = "\u00dc", Ugrave = "\u00d9", Yacute = "\u00dd",
  aacute = "\u00e1", acirc = "\u00e2", atilde = "\u00e3", auml = "\u00e4",
  agrave = "\u00e0", aring = "\u00e5", ccedil = "\u00e7",
  eacute = "\u00e9", ecirc = "\u00ea", euml = "\u00eb", egrave = "\u00e8",
  iacute = "\u00ed", icirc = "\u00ee", iuml = "\u00ef", igrave = "\u00ec",
  ntilde = "\u00f1", oacute = "\u00f3", ocirc = "\u00f4", otilde = "\u00f5",
  ouml = "\u00f6", ograve = "\u00f2", uacute = "\u00fa", ucirc = "\u00fb",
  uuml = "\u00fc", ugrave = "\u00f9", yacute = "\u00fd", yuml = "\u00ff"
)

# La misma gramatica la usan dos funciones que tienen que coincidir: esta la usa
# para **detectar** entidades y `.decodificar_entidades_html()` para
# **repararlas**. Escritas por separado, si una cambiaba se detectaba lo que no
# se reparaba, o al reves, y ninguna de las dos avisaba. Es el mismo
# acoplamiento que tenian las tres copias de la generalizacion de patrones.
.PATRON_ENTIDAD_HTML <-
  "&(?:#(?:[xX][0-9A-Fa-f]{1,6}|[0-9]{1,7})|[A-Za-z][A-Za-z0-9]+);"

.entidades_html_en_texto <- function(textos) {
  patron <- .PATRON_ENTIDAD_HTML
  vapply(textos, function(texto) {
    if (is.na(texto)) return(FALSE)
    texto <- .texto_para_operar(texto)
    if (!validUTF8(texto)) return(FALSE)
    posiciones <- gregexpr(patron, texto, perl = TRUE)[[1L]]
    if (identical(posiciones[[1L]], -1L)) return(FALSE)
    encontrados <- regmatches(texto, list(posiciones))[[1L]]
    any(vapply(encontrados, .entidad_html_valida, logical(1L)))
  }, logical(1L), USE.NAMES = FALSE)
}

.entidad_html_valida <- function(entidad) {
  cuerpo <- substring(entidad, 2L, nchar(entidad) - 1L)
  if (startsWith(cuerpo, "#x") || startsWith(cuerpo, "#X")) {
    punto <- suppressWarnings(strtoi(substring(cuerpo, 3L), base = 16L))
    return(is.finite(punto) && punto > 0L && punto <= 0x10FFFF &&
      !(punto >= 0xD800 && punto <= 0xDFFF))
  }
  if (startsWith(cuerpo, "#")) {
    punto <- suppressWarnings(as.numeric(substring(cuerpo, 2L)))
    return(is.finite(punto) && punto > 0 && punto <= 0x10FFFF &&
      !(punto >= 0xD800 && punto <= 0xDFFF))
  }
  cuerpo %in% names(.entidades_html_comunes)
}

.escapar_texto_visible <- function(texto) {
  if (is.na(texto)) return(NA_character_)
  # `Encoding(x) == "bytes"` es una declaracion de que el contenido NO debe
  # interpretarse como texto, y esto es publicacion. Sin este paso el mismo
  # valor salia de dos formas distintas EN EL MISMO OBJETO: la tabla del perfil
  # lo publicaba `ni\xc3\xb1o` -marca intacta- y la evidencia del hallazgo lo
  # publicaba `nino` con la enie armada, sin marca. Quien lee el informe no
  # podia saber que eran el mismo valor, que es exactamente lo que el hallazgo
  # le pide mirar antes de unificar.
  texto <- .texto_publicable(texto)
  # `latin1` NO es `bytes`: se convierte antes de mirar `validUTF8()`, porque
  # sus bytes no son UTF-8 validos. La conversion comun conserva tambien los
  # cinco puntos C1 que no tienen nombre en Windows-1252.
  texto <- .latin1_a_utf8(texto)
  # Y lo que aun no decodifica no se puede describir como caracteres. Antes se
  # intentaba igual: `utf8ToInt()` devuelve `NA` -no vacio, no error- y ese
  # `NA` llegaba al `if` de mas abajo y ABORTABA la corrida. Una columna
  # `latin1` de un CSV viejo con valores repetidos tumbaba `detectar_claves()`
  # entero. Se publica como lo imprime R, que es lo que ya hacen `moda` y la
  # tabla de patrones para el mismo caso.
  if (!validUTF8(texto)) {
    Encoding(texto) <- "bytes"
    texto <- .texto_publicable(texto)
  }
  codigos <- tryCatch(utf8ToInt(texto), error = function(e) integer())
  # Ultima red: si despues de todo lo anterior sigue sin decodificar, se
  # devuelve el texto tal como se lo publica en vez de entrar al bucle con un
  # `NA`. Devolver "" seria peor que abortar: afirmaria que el valor es la
  # cadena vacia.
  if (anyNA(codigos)) return(texto)
  if (!length(codigos)) return("")
  partes <- vapply(codigos, function(codigo) {
    if (codigo == 9L) return("\\t")
    if (codigo == 10L) return("\\n")
    if (codigo == 11L) return("\\v")
    if (codigo == 12L) return("\\f")
    if (codigo == 13L) return("\\r")
    if (.codigos_control_invisible(codigo)) {
      return(sprintf("<U+%04X>", codigo))
    }
    intToUtf8(codigo)
  }, character(1L), USE.NAMES = FALSE)
  paste0(partes, collapse = "")
}

.evidencia_texto_visible <- function(textos, mascara) {
  ejemplos <- utils::head(unique(textos[which(mascara)]), 6L)
  if (!length(ejemplos)) return("")
  # La tabla de patrones rinde los valores declarados `bytes` antes de
  # generalizarlos. La evidencia debe pasar por la misma vista; si se la
  # interpreta como UTF-8 aqui, las dos superficies publican cadenas distintas.
  ejemplos <- .texto_publicable(ejemplos)
  paste0(
    paste(vapply(ejemplos, .escapar_texto_visible, character(1L)), collapse = "; "),
    .nota_escapes(ejemplos)
  )
}

.stringi_disponible <- function() {
  requireNamespace("stringi", quietly = TRUE)
}

.bit64_disponible <- function() {
  requireNamespace("bit64", quietly = TRUE)
}

.texto_largo <- function(x, max_largo) {
  vacio <- list(
    mascara = NULL,
    n_valores = 0L,
    max_bytes = 0,
    max_caracteres = 0
  )
  if (!(is.character(x) || is.factor(x)) || !length(x) ||
      is.infinite(max_largo)) {
    return(vacio)
  }
  textos <- suppressWarnings(as.character(x))
  bytes <- suppressWarnings(nchar(textos, type = "bytes", allowNA = TRUE))
  observados_bytes <- bytes[!is.na(bytes)]
  maximo_bytes <- if (length(observados_bytes)) {
    max(observados_bytes)
  } else 0
  hay_largo <- is.finite(maximo_bytes) && maximo_bytes > max_largo
  if (!hay_largo) return(vacio)
  caracteres <- .nchar_texto_seguro(textos)
  mascara <- !is.na(textos) & bytes > max_largo
  if (!is.null(caracteres)) {
    mascara <- mascara | (!is.na(caracteres) & caracteres > max_largo)
  }
  vacio$mascara <- mascara
  vacio$n_valores <- as.integer(sum(mascara))
  observados_caracteres <- if (is.null(caracteres)) {
    observados_bytes
  } else caracteres[!is.na(caracteres)]
  vacio$max_bytes <- if (length(observados_bytes)) {
    max(observados_bytes)
  } else 0
  vacio$max_caracteres <- if (length(observados_caracteres)) {
    max(observados_caracteres)
  } else 0
  vacio
}

.marca_texto_largo <- function(x, mascara) {
  if (!(is.character(x) || is.factor(x))) return(x)
  if (!any(mascara)) return(x)
  salida <- if (is.factor(x)) as.character(x) else x
  salida[mascara] <- "texto_largo_no_evaluado"
  salida
}

.diagnosticar_texto <- function(x, vocabulario = NULL) {
  vacio <- list(
    n_espacios_borde = 0L,
    evidencia_espacios = "",
    n_variantes_mayusculas = 0L,
    evidencia_mayusculas = "",
    n_variantes_unicode = NA_integer_,
    evidencia_unicode = "",
    # Una columna vacía o compuesta sólo por NA no contiene texto no ASCII que
    # evaluar: la ausencia de `stringi` no limita ese caso.
    unicode_evaluado = TRUE,
    n_codificacion_rota = 0L,
    n_codificacion_reparable = 0L,
    n_codificacion_reparable_parcialmente = 0L,
    n_codificacion_irreparable = 0L,
    n_codificacion_no_se_pudo = 0L,
    estado_codificacion_reparacion = "no_parece_roto",
    evidencia_codificacion = "",
    n_codificacion_invalida = 0L,
    evidencia_codificacion_invalida = "",
    n_controles_invisibles = 0L,
    evidencia_controles_invisibles = "",
    n_invisibles_eliminables = 0L,
    evidencia_invisibles_eliminables = "",
    n_espacios_invisibles = 0L,
    evidencia_espacios_invisibles = "",
    n_invisibles_significativos = 0L,
    evidencia_invisibles_significativos = "",
    n_entidades_html = 0L,
    evidencia_entidades_html = "",
    n_separadores_en_campo = 0L,
    evidencia_separadores_en_campo = ""
  )
  if (!is.character(x) && !is.factor(x)) {
    return(vacio)
  }
  preparacion <- .texto_analizable(x)
  textos <- preparacion$valores
  # Los seis tipos de evidencia de mas abajo PUBLICAN valores, y publicar no es
  # analizar. Pasandoles `textos` -la forma interpretada- un mismo valor salia
  # de dos formas en el mismo perfil: la tabla de patrones publicaba
  # `ca\xc3\xb1\xc3\xb3n ` y el hallazgo `espacios_sobrantes` publicaba
  # `"canon "` con los acentos armados. Quien lee el hallazgo y va a verificar
  # contra la tabla no encuentra el valor que le mostraron. La regla es la
  # misma que usan las variantes del vocabulario: lo declarado `bytes` se
  # publica desde el original; para el resto, la forma analizada ES la
  # publicable, porque en `latin1` R convierte sin perder nada.
  publicables <- .vista_publicable(preparacion$valores_identidad, textos)
  vacio$n_codificacion_invalida <- length(preparacion$posiciones)
  if (length(preparacion$posiciones)) {
    mostradas <- utils::head(preparacion$posiciones, 8L)
    vacio$evidencia_codificacion_invalida <- paste0(
      length(preparacion$posiciones),
      if (length(preparacion$posiciones) == 1L) " valor; fila: " else " valores; filas: ",
      paste(mostradas, collapse = ", "),
      if (length(preparacion$posiciones) > length(mostradas)) ", ..." else ""
    )
  }
  validos <- !is.na(textos)
  if (!any(validos)) {
    if (length(preparacion$posiciones)) {
      campos_codificacion <- c(
        "n_codificacion_rota", "n_codificacion_reparable",
        "n_codificacion_reparable_parcialmente", "n_codificacion_irreparable",
        "n_codificacion_no_se_pudo"
      )
      vacio[campos_codificacion] <- lapply(
        campos_codificacion, function(.) NA_integer_
      )
      vacio$estado_codificacion_reparacion <- NA_character_
      # `n_codificacion_invalida` sigue contando los valores y el hallazgo de
      # error sigue explicando su exclusion; estas banderas declaran que las
      # pasadas que dependen de texto valido no llegaron a ejecutarse.
      vacio$unicode_evaluado <- FALSE
    }
    return(vacio)
  }

  unicos <- if (is.null(vocabulario)) {
    unique(textos[validos])
  } else {
    vocabulario
  }
  codificacion <- .analizar_codificacion_vocabulario(
    textos, valores = unicos, con_cache = TRUE
  )
  if (length(textos) > 1L &&
      length(unicos) / length(textos) <= .umbral_vocabulario_barato) {
    vocabulario_barato <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = unicos
    )
    espacios_unicos <- .celdas_texto_protegidas(
      unicos, "recortar_espacios"
    )
    espacios <- validos &
      espacios_unicos[vocabulario_barato$indices]
  } else {
    espacios_unicos <- .celdas_texto_protegidas(
      unicos, "recortar_espacios"
    )
    espacios <- validos & espacios_unicos[match(textos, unicos)]
  }
  # `unique()` sobre una cadena marcada `bytes` ABORTA en el minimo declarado -R
  # 4.1- con "translating strings with bytes encoding is not allowed", y estos
  # ejemplos salen de la columna del usuario, que puede traer esa marca. Medido:
  # `perfilar()` se caia ahi sobre una columna `latin1` valida. Se deduplica por
  # la clave de bytes, que no traduce.
  ejemplos_espacios <- utils::head(
    .identificadores_unicos(publicables[espacios]), 6L
  )
  # Los valores ASCII sin espacios, controles ni ampersands no pueden contener
  # invisibles Unicode, codificacion rota ni entidades HTML. En ese caso basta
  # con resolver la unica señal ASCII restante -variantes de mayusculas- y se
  # evita una llamada a `utf8ToInt()` por valor.
  if (.es_ascii(unicos) && !any(grepl(
    "[[:space:]&\\x00-\\x1F\\x7F]", unicos,
    useBytes = TRUE, perl = TRUE
  ))) {
    vocabulario_predicados <- .vocabulario_texto(
      textos, .umbral_vocabulario_barato, valores = unicos
    )
    .predicados_invisibles(vocabulario_predicados$valores)
    minusculas <- .normalizacion_minusculas_vector(unicos)
    colision <- duplicated(minusculas) | duplicated(minusculas, fromLast = TRUE)
    variantes <- .publicables_por_forma(unicos[colision], textos, publicables)
    vacio$n_variantes_mayusculas <- length(variantes)
    vacio$evidencia_mayusculas <- paste(
      .citar_publicable(utils::head(variantes, 6L)), collapse = "; "
    )
    vacio$n_variantes_unicode <- 0L
    return(vacio)
  }
  minusculas <- .normalizacion_minusculas_vector(unicos)
  colision <- duplicated(minusculas) | duplicated(minusculas, fromLast = TRUE)
  variantes <- .publicables_por_forma(unicos[colision], textos, publicables)
  solo_ascii <- !any(grepl("[^\\x01-\\x7F]", unicos, perl = TRUE))
  if (solo_ascii) {
    evidencia_unicode <- ""
    n_variantes_unicode <- 0L
    unicode_evaluado <- TRUE
  } else if (.stringi_disponible()) {
    normalizados_unicode <- stringi::stri_trans_nfc(unicos)
    colision_unicode <- duplicated(normalizados_unicode) |
      duplicated(normalizados_unicode, fromLast = TRUE)
    variantes_unicode <- unicos[colision_unicode]
    evidencia_unicode <- paste(
      stringi::stri_escape_unicode(utils::head(variantes_unicode, 6L)),
      collapse = "; "
    )
    n_variantes_unicode <- length(variantes_unicode)
    unicode_evaluado <- TRUE
  } else {
    evidencia_unicode <- "Se necesita el paquete opcional 'stringi'."
    n_variantes_unicode <- NA_integer_
    unicode_evaluado <- FALSE
  }
  vocabulario_predicados <- .vocabulario_texto(
    textos, .umbral_vocabulario_barato, valores = unicos
  )
  invisibles <- .predicados_invisibles(
    vocabulario_predicados$valores, codificacion = codificacion
  )
  mapear_resultado <- function(evaluados) {
    if (isTRUE(vocabulario_predicados$usar)) {
      evaluados[vocabulario_predicados$indices]
    } else {
      evaluados
    }
  }
  controles <- mapear_resultado(invisibles$control)
  eliminables <- mapear_resultado(invisibles$eliminable)
  espacios_invisibles <- mapear_resultado(invisibles$espacio)
  significativos <- mapear_resultado(invisibles$significativo)
  saltos <- mapear_resultado(invisibles$separador)
  entidades_unicas <- .celdas_texto_protegidas(
    vocabulario_predicados$valores, "decodificar_entidades_html"
  )
  entidades <- mapear_resultado(entidades_unicas)
  invisibles_cache <- list(
    control = controles, eliminable = eliminables,
    espacio = espacios_invisibles, significativo = significativos,
    separador = saltos
  )

  list(
    n_espacios_borde = sum(espacios),
    evidencia_espacios = paste0(
      paste(.citar_publicable(ejemplos_espacios), collapse = "; "),
      .nota_escapes(ejemplos_espacios)
    ),
    n_variantes_mayusculas = length(variantes),
    evidencia_mayusculas = paste(
      .citar_publicable(utils::head(variantes, 6L)), collapse = "; "
    ),
    n_variantes_unicode = n_variantes_unicode,
    evidencia_unicode = evidencia_unicode,
    unicode_evaluado = unicode_evaluado,
    n_codificacion_rota = codificacion$n,
    n_codificacion_reparable = codificacion$n_reparables,
    n_codificacion_reparable_parcialmente = codificacion$n_reparables_parcialmente,
    n_codificacion_irreparable = codificacion$n_irreparables,
    n_codificacion_no_se_pudo = codificacion$n_no_se_pudo,
    estado_codificacion_reparacion = codificacion$estado,
    evidencia_codificacion = codificacion$evidencia,
    n_codificacion_invalida = length(preparacion$posiciones),
    evidencia_codificacion_invalida = vacio$evidencia_codificacion_invalida,
    n_controles_invisibles = sum(controles, na.rm = TRUE),
    evidencia_controles_invisibles = .evidencia_texto_visible(publicables, controles),
    n_invisibles_eliminables = sum(eliminables, na.rm = TRUE),
    evidencia_invisibles_eliminables = .evidencia_texto_visible(publicables, eliminables),
    n_espacios_invisibles = sum(espacios_invisibles, na.rm = TRUE),
    evidencia_espacios_invisibles = .evidencia_texto_visible(publicables, espacios_invisibles),
    n_invisibles_significativos = sum(significativos, na.rm = TRUE),
    evidencia_invisibles_significativos = .evidencia_texto_visible(publicables, significativos),
    n_entidades_html = sum(entidades, na.rm = TRUE),
    evidencia_entidades_html = .evidencia_texto_visible(publicables, entidades),
    n_separadores_en_campo = sum(saltos, na.rm = TRUE),
    evidencia_separadores_en_campo = .evidencia_texto_visible(publicables, saltos),
    # `.indices_hallazgo_columna()` se ejecuta despues, durante la construccion
    # de hallazgos. Estas mascaras son el resultado de la misma pasada: volver a
    # descubrirlas alli repetia el filtro y la alineacion sobre la columna.
    cache_invisibles = invisibles_cache,
    cache_codificacion_afectados = codificacion$afectados
  )
}

.n_distintos_columna <- function(x, claves = NULL, rapido = FALSE) {
  validos <- tryCatch(!is.na(x), error = function(e) NULL)
  if (is.null(validos) || length(validos) != length(x)) return(NA_integer_)
  if (!any(validos)) return(0L)
  if (is.character(x) || is.factor(x)) {
    if (is.null(claves)) claves <- .clave_bytes(x)
    if (any(validos & is.na(claves))) return(NA_integer_)
    return(as.integer(length(unique(claves[validos]))))
  }
  if (isTRUE(rapido) && (is.numeric(x) || inherits(x, "integer64"))) {
    return(as.integer(length(unique(x[validos]))))
  }
  # El acumulador de distintos conserva valores atomicos. Las columnas de
  # listas (incluidas `sfc` y `POSIXlt`) siguen necesitando la igualdad de la
  # pasada unica: intentar guardarlas como representantes del data.frame del
  # mapa puede expandirlas o hacerlas no construibles. La migracion solo toma
  # la via por bloques para las clases que esa representacion puede conservar.
  sobre <- if (is.atomic(x) && is.null(dim(x))) {
    .mapa_distintos_bloques(x)
  } else {
    NULL
  }
  if (!is.null(sobre) && identical(sobre$estado, "calculado")) {
    return(as.integer(nrow(sobre$resultado)))
  }
  tryCatch(as.integer(length(unique(x[validos]))),
           error = function(e) NA_integer_)
}

# Recorta un resultado de faltantes disfrazados al universo aplicable. La
# mascara elemento a elemento es lo que permite hacerlo sin volver a detectar.
.restringir_disfrazados <- function(disfrazados, aplicable) {
  if (is.null(aplicable) || all(aplicable)) return(disfrazados)
  mascara <- disfrazados$mascara & aplicable
  n_aplicables <- sum(aplicable)
  disfrazados$mascara <- mascara
  disfrazados$n <- sum(mascara)
  disfrazados$proporcion <- if (n_aplicables) sum(mascara) / n_aplicables else NA_real_
  if (!is.null(disfrazados$mascara_textual)) {
    disfrazados$mascara_textual <- disfrazados$mascara_textual & aplicable
    disfrazados$n_textuales <- sum(disfrazados$mascara_textual)
  }
  if (!is.null(disfrazados$mascara_numerica)) {
    disfrazados$mascara_numerica <- disfrazados$mascara_numerica & aplicable
    disfrazados$n_numericos <- sum(disfrazados$mascara_numerica)
  }
  disfrazados
}

# Los centinelas numericos que el detector CONTO, despues de recortar al
# universo aplicable: los de las celdas de `mascara_numerica`, que es la mascara
# de `n_faltantes_disfrazados_numericos`. Salen de la misma comparacion que el
# conteo y por eso no pueden discrepar de el. Es lo que el hallazgo publica en
# su traza y lo que el plan de limpieza convierte; la posicion por celda -un
# vector del largo de la columna- se descarta aca y no viaja con el resultado.
.con_sentinelas_contados <- function(disfrazados) {
  posicion <- disfrazados$posicion_sentinela
  sentinelas <- disfrazados$sentinelas
  disfrazados$posicion_sentinela <- NULL
  disfrazados$sentinelas <- NULL
  contados <- numeric()
  mascara <- disfrazados$mascara_numerica
  if (length(posicion) && length(mascara) == length(posicion)) {
    presentes <- unique(posicion[mascara])
    presentes <- presentes[!is.na(presentes)]
    if (length(presentes)) {
      contados <- as.numeric(sentinelas[.sort_por_bytes(presentes)])
    }
  }
  disfrazados$sentinelas_contados <- contados
  disfrazados
}

# Los faltantes disfrazados de una columna -sentinelas numericos y cadenas que
# dicen ausencia-, con la politica entera de `perfilar()`. Estaba escrita dentro
# de `.perfilar_columna()`, y `detectar_claves()` llamaba a la casi-clave SIN
# mascara: medido, 190 documentos distintos mas diez "SIN DATO"
# salian casi-clave con concentracion 1 y colision "SIN DATO (10)", mientras
# `perfilar()` sobre la misma tabla contaba diez faltantes disfrazados y ningun
# hallazgo de clave. Una regla escrita dos veces contestaba distinto sobre la
# misma columna; ahora hay una sola.
.faltantes_disfrazados_columna <- function(x_analisis, inferencia, formatos,
                                           secuencia_entera,
                                           sentinelas_numericos,
                                           sentinelas_declarados,
                                           cadenas_ausencia, aplicable,
                                           nombre = NULL) {
  valores_numericos <- .valores_numericos_secuencia(
    x_analisis, inferencia, formatos
  )
  # Una duracion (`difftime`) o una medida con unidad (`units`) es un numero con
  # una clase encima, y el detector ya la compara como numero. La guarda de la
  # numeracion, en cambio, solo conoce los tipos `entero`, `doble` e
  # `integer64`: para estas dos clases no habia ni valores ni secuencia, y
  # corria como si no hubiera numeracion que proteger. Medido: `1:1000`
  # guardado como `integer` publicaba cero faltantes disfrazados, y guardado
  # como `difftime` o como `units` publicaba uno, el 999 que la numeracion trae
  # una vez en su lugar. El mismo dato con dos respuestas, decididas por la
  # clase. Aca se mide la numeracion de esos numeros con las mismas dos
  # funciones; el resumen de secuencia que la fila publica no cambia.
  if (inherits(x_analisis, c("difftime", "units"))) {
    numeros <- suppressWarnings(as.numeric(x_analisis))
    como_numero <- list(tipo = "doble")
    secuencia_entera <- .resumen_secuencia_entera(numeros, como_numero, NULL)
    valores_numericos <- .valores_numericos_secuencia(
      numeros, como_numero, NULL
    )
  }
  guarda <- .decision_guarda_numeracion(
    valores_numericos, secuencia_entera, sentinelas_numericos,
    sentinelas_declarados
  )
  # Sobre una numeracion limpia la guarda no corre entera: corre SOLO para los
  # candidatos que muestran una senal propia. Sobre cualquier otra columna corre
  # completa, como siempre. Una declaracion explicita la atraviesa igual.
  # `!length(sentinelas_declarados)` no sobra: **una declaracion explicita
  # atraviesa la guarda entera**, y filtrar por candidato tambien es la guarda.
  # Sin esa condicion, declarar `sentinelas_numericos = c(999)` sobre una
  # numeracion de 501 a 1000 no marcaba nada -999 esta dentro del rango y no
  # sobresale-, o sea que el paquete ignoraba lo que el usuario acababa de
  # decirle. Lo atrapo `test-grupos-guiado.R`.
  sentinelas_efectivos <- guarda$sentinelas_efectivos
  detectar_sentinelas <- guarda$detectar_sentinelas
  # La convencion decimal que la fila publica: un centinela escrito con ella
  # -`"-999,00"` en una columna `decimal_coma`- se lee como lo lee el resumen.
  convencion <- inferencia[["convencion_numero_texto"]]
  detectar <- function(efectivos, detectar_numericos) {
    .restringir_disfrazados(.detectar_faltantes_disfrazados(
      x_analisis, sentinelas_numericos = efectivos,
      cadenas_ausencia = cadenas_ausencia, convencion = convencion,
      # Una numeracion compacta no apaga por si sola la corazonada: un candidato
      # fuera de su rango o cuya frecuencia es la moda sobresaliente vuelve a
      # abrirla. Una declaracion explicita siempre atraviesa la guarda.
      detectar_sentinelas_numericos = detectar_numericos
    ), aplicable)
  }
  faltantes_disfrazados <- detectar(sentinelas_efectivos, detectar_sentinelas)
  # Dentro de `perfilar_por()` la guarda de la numeracion es la de la columna
  # entera, no la de la rebanada: ver `.disfrazados_con_la_columna_entera()`.
  # Fuera de esa llamada no cambia nada.
  faltantes_disfrazados <- .disfrazados_con_la_columna_entera(
    faltantes_disfrazados, nombre, guarda, sentinelas_declarados, detectar
  )
  # Los centinelas que la traza publica salen de la mascara ya decidida, la
  # misma que cuenta: tambien dentro de `perfilar_por()`.
  faltantes_disfrazados <- .con_sentinelas_contados(faltantes_disfrazados)
  faltantes_declarados <- .detectar_faltantes_disfrazados(
    x_analisis,
    sentinelas_numericos = .sentinelas_numericos_declarados(
      sentinelas_numericos
    ),
    detectar_sentinelas_numericos = TRUE, convencion = convencion
  )
  faltantes_declarados <- .restringir_disfrazados(
    faltantes_declarados, aplicable
  )
  # El total conserva la señal del paquete y la política completa, pero la
  # distinción entre declarado y adivinado queda disponible para la guarda de
  # severidad de `hallazgos.R`.
  faltantes_disfrazados$n_numericos_declarados <-
    as.integer(faltantes_declarados$n_numericos)
  faltantes_disfrazados
}

.perfilar_columna <- function(x, nombre, muestra, max_patrones,
                              distinguir_mayusculas, expandir,
                              umbral_patron_raro,
                              sentinelas_numericos,
                              aplicable = NULL,
                              cadenas_ausencia = NULL,
                              codigo = FALSE,
                              max_largo_valor_vocabulario =
                                .MAX_LARGO_VALOR_CASI_DUPLICADOS) {
  if (.es_columna_compuesta(x)) {
    return(.perfilar_columna_matriz(
      x, nombre, muestra, max_patrones, distinguir_mayusculas, expandir,
      umbral_patron_raro, sentinelas_numericos,
      max_largo_valor_vocabulario
    ))
  }
  # El universo aplicable se resuelve primero porque gobierna todos los conteos
  # de ausencia de abajo. Sin declaracion, toda la columna aplica y el resto se
  # reduce al comportamiento de siempre.
  sentinelas_declarados <- .sentinelas_numericos_declarados(
    sentinelas_numericos
  )
  aplicable <- if (is.null(aplicable)) {
    # `NROW()`, por la misma razon que `n`: una columna compuesta tiene tantas filas
    # como la tabla, no tantas como campos.
    rep(TRUE, NROW(x))
  } else {
    # `as.logical()` borraria el conteo de indeterminados que viaja como
    # atributo, y la fila volveria a confundir "no corresponde" con "no se sabe".
    # La mascara POR FILA viaja por el mismo motivo y se perdia en el mismo
    # lugar: se reponen las dos, o el arreglo de mas abajo no ve nada.
    indeterminados <- attr(aplicable, "n_indeterminados", exact = TRUE)
    por_fila <- attr(aplicable, "indeterminados_por_fila", exact = TRUE)
    convertida <- as.logical(aplicable)
    attr(convertida, "n_indeterminados") <- indeterminados
    if (!is.null(por_fila) && length(por_fila) == length(convertida)) {
      attr(convertida, "indeterminados_por_fila") <- por_fila
    }
    convertida
  }
  # La mascara entra solo para decidir en que se gasta el presupuesto geometrico:
  # primero en el universo.
  geometria <- .perfilar_geometria(
    x, aplicable = if (isTRUE(all(aplicable))) NULL else aplicable
  )
  # El analisis de geometria corre sobre la columna cruda -no puede ser de otro
  # modo: necesita la `sfc` entera para leer su CRS y sus tipos- asi que el universo
  # aplicable se aplica despues, sobre sus conteos y sus indices. Ver
  # `.restringir_geometria_al_universo()`.
  if (!all(aplicable)) {
    geometria <- .restringir_geometria_al_universo(geometria, aplicable)
  }
  preparacion_texto <- .texto_analizable(x)
  x_analisis <- preparacion_texto$valores
  # Esta vista conserva los valores para los conteos y las lecturas baratas.
  # El tope se calcula despues de aplicar el universo y solo modifica la vista
  # de las operaciones textuales costosas.
  x_analisis_diagnostico <- x_analisis
  # La columna tal cual, para lo que compara por igualdad en vez de leer texto.
  # Sobre una columna no analizable como texto, `valores` son ausentes
  # declarados: inferir el tipo ahi diria "desconocido" sobre una columna cuyo
  # tipo se conoce perfectamente.
  x_identidad <- preparacion_texto$valores_identidad
  if (is.null(x_identidad)) x_identidad <- x_analisis
  # Un byte no es un texto escrito por una persona. `.texto_analizable()` da a
  # una columna `raw` su escritura hexadecimal -`0a`, `1f`-, que sirve para
  # compararla y publicarla, pero no es una convencion que el dato tenga: leida
  # como texto escrito, se median `numero_como_texto` y
  # `unidades_mixtas` sobre los doce bytes con una letra hexadecimal -"0a" era
  # un cero con la unidad "a"- y `patron_raro` sobre `9a` entre `99`. Las
  # etapas que interpretan la escritura reciben ausentes, como una lista de
  # objetos; lo que compara por igualdad -distintos, moda- sigue con los bytes.
  escrito <- !is.raw(x)
  if (!escrito) x_analisis <- rep(NA_character_, length(x))
  # Las filas fuera del universo salen del analisis, no solo de los conteos.
  # Dejarlas dentro hacia que `n_distintos` contara valores que no aplican
  # mientras `n_validos` ya solo contaba los aplicables, y `tasa_distintos`
  # podia pasar de 1, que es imposible. Se marcan como ausentes en vez de
  # recortarse para que los indices de fila sigan alineados con la tabla.
  if (!all(aplicable)) {
    x_analisis[!aplicable] <- NA
    if (inherits(x_identidad, "sfc")) {
      # Una geometria no admite `NA`: `sf` convierte la asignacion en una
      # geometria VACIA -`POINT EMPTY`, con `is.na()` en FALSE-, que cuenta como
      # un valor mas (`tasa_distintos = 1.5` sobre dos geometrias aplicables), y
      # sobre la primera fila aborta -"object(s) should be of class 'sfg'"-. Aca
      # `x_identidad` solo alimenta la inferencia, que sale de la clase, y
      # agregados que no miran posiciones; los conteos por fila de la geometria
      # ya se restringieron arriba. Se recorta a las filas aplicables.
      x_identidad <- x_identidad[aplicable]
    } else {
      x_identidad[!aplicable] <- NA
    }
  }
  texto_largo <- .texto_largo(
    x_analisis, max_largo_valor_vocabulario
  )
  # La vista completa conserva los valores largos para identidad, conteos,
  # inferencia y longitudes. Las operaciones textuales costosas reciben una
  # vista aparte con esas filas enmascaradas mas abajo.
  x_analisis_diagnostico <- x_analisis
  x_texto_diagnostico <- if (is.character(x) || is.factor(x)) {
    salida <- x
    if (!all(aplicable)) salida[!aplicable] <- NA_character_
    if (any(texto_largo$mascara)) {
      salida[texto_largo$mascara] <- NA_character_
    }
    salida
  } else {
    .marca_texto_largo(x_analisis, texto_largo$mascara)
  }
  n <- NROW(x)
  claves_identidad <- if (is.character(x_identidad) || is.factor(x_identidad)) {
    .clave_bytes(x_identidad)
  } else NULL
  n_distintos <- .n_distintos_columna(x_identidad, claves = claves_identidad)
  vocabulario_texto <- if (
    (is.character(x_analisis_diagnostico) ||
       is.factor(x_analisis_diagnostico)) &&
      n > 1L && is.finite(n_distintos) &&
      n_distintos / n <= .umbral_vocabulario_codificacion
  ) {
    unique(x_analisis_diagnostico[!is.na(x_analisis_diagnostico)])
  } else {
    NULL
  }
  vocabulario_texto_caro <- if (
    is.null(vocabulario_texto) || !any(texto_largo$mascara)
  ) {
    vocabulario_texto
  } else {
    unique(x_analisis_diagnostico[
      !is.na(x_analisis_diagnostico) & !texto_largo$mascara
    ])
  }
  vocabulario_numeros <- if (
    is.null(vocabulario_texto) &&
      (is.character(x_analisis_diagnostico) ||
         is.factor(x_analisis_diagnostico))
  ) x_analisis_diagnostico else vocabulario_texto
  numeros_texto <- .analizar_numeros_texto(
    x_analisis_diagnostico, vocabulario = vocabulario_numeros,
    n_distintos = n_distintos,
    directo = is.null(vocabulario_texto) &&
      (is.character(x_analisis_diagnostico) ||
         is.factor(x_analisis_diagnostico))
  )
  # La convencion decimal de la columna se decide antes de inferir el tipo: la
  # inferencia cuenta como numero lo que el lector comun lee con ella, que es
  # lo que despues lee el resumen. Aunque infiera sobre una muestra, la
  # convencion es la de la columna entera.
  # El perfilado conserva el resultado intermedio sólo durante esta llamada;
  # la función exportada `inferir_tipo()` nunca devuelve ese caché.
  inferencia <- .inferir_tipo_interno(
    if (is.character(x_analisis_diagnostico) ||
        is.factor(x_analisis_diagnostico)) {
      x_analisis_diagnostico
    } else {
      x_identidad
    }, muestra = muestra, conservar_cache = TRUE,
    convencion_numero_texto = numeros_texto$convencion
  )
  formatos <- inferencia$formatos_fecha
  if (is.null(formatos)) {
    formatos <- detectar_formatos_fecha(
      x_analisis_diagnostico, muestra = muestra
    )
  }
  # El tipo y su estado deben salir de la misma tabla de formatos. En texto,
  # esta es la tabla que ya uso `.inferir_tipo_interno` para calcular
  # `compatibles`; para fechas nativas es la deteccion que acaba de completar
  # el mismo canal por columna.
  inferencia$estado_tipo_inferido <- .estado_tipo_inferido_desde_formatos(
    inferencia$tipo, formatos
  )
  meses_texto <- attr(formatos, "meses_texto", exact = TRUE)
  zona_horaria_origen <- .zona_horaria_origen(x)
  n_filas_fecha_civil_distinta_utc <- .fechas_civiles_distintas_utc(x)
  attr(formatos, "zona_horaria_origen") <- zona_horaria_origen
  attr(formatos, "n_filas_fecha_civil_distinta_utc") <-
    n_filas_fecha_civil_distinta_utc
  # The month parser is an internal hand-off between type inference and the
  # column summary. It must not remain attached to the public profile table.
  attr(formatos, "meses_texto") <- NULL
  # Tampoco la asignacion de formas a formatos, que es del mismo orden: un
  # intermedio de la deteccion. Hoy nadie la pide por este camino; si alguna
  # vez se reusa aca la deteccion para las filas de un hallazgo, la tabla que
  # se publica sigue sin llevarla.
  attr(formatos, "asignacion") <- NULL
  patrones <- if (is.character(x_analisis) || is.factor(x_analisis)) {
    # `proteger_datos_personales = FALSE` no es un descuido: `perfilar()` protege
    # el perfil ENTERO despues, y ahi respeta la declaracion del usuario. Si se
    # enmascarara tambien aca, `proteger_datos_personales = FALSE` dejaria de
    # obedecerse y el paquete ignoraria lo que el usuario acaba de pedirle. La
    # proteccion de `descubrir_patrones()` es para quien la llama suelta.
    # Se pasa la columna ORIGINAL rendida para publicar, no `x_analisis`.
    # `x_analisis` ya paso por `.texto_analizable()`, que marca UTF-8 lo
    # declarado `bytes` -a proposito, para que las operaciones de texto
    # trabajen-, asi que al llegar aca la declaracion ya no esta y el perfil
    # publicaba el caracter en `patron` y en `ejemplos` mientras `print()` del
    # mismo dato mostraba la forma escapada.
    #
    # `descubrir_patrones()` vuelve a analizar por dentro, asi que sobre todo lo
    # que no lleva esa marca el resultado es el mismo.
    # Solo cuando la columna ES texto. `x_analisis` puede ser character aunque
    # `x` no lo sea -una columna-lista que `.texto_analizable()` aplano-, y ahi
    # pasar el original hace que `descubrir_patrones()` lo rechace: "`x` debe
    # ser un vector atomico". Lo atrapo la suite.
    descubrir_patrones(
      x_texto_diagnostico,
      distinguir_mayusculas = distinguir_mayusculas,
      expandir = expandir,
      max_patrones = max_patrones,
      muestra = muestra,
      umbral_raro = umbral_patron_raro,
      proteger_datos_personales = FALSE
    )
  } else {
    estructura <- .data_frame_simple(
      patron = character(), n = integer(), proporcion = numeric(),
      ejemplos = character(), stringsAsFactors = FALSE
    )
    class(estructura) <- c("patrones", "data.frame")
    estructura
  }
  # `NROW()` y no `length()`. Para un vector son lo mismo; para una columna cuyo
  # valor es un `data.frame` -lo que devuelve un driver al leer un `STRUCT`- o una
  # matriz, `length()` cuenta sus CAMPOS y no sus filas: medido, una columna
  # `STRUCT(a, b)` sobre una tabla de tres filas publicaba `n = 2` mientras
  # `general$filas` decia 3, y por DBI el bloque de muestra discrepaba del resumen
  # SQL sin que nada declarara la diferencia. La columna tiene tres valores; lo que
  # no se puede es analizarlos como texto, y eso ya se declara aparte.
  #
  # Ademas alinea `n` con la mascara del universo, que llega con el largo de la
  # TABLA: con `aplicabilidad` declarada sobre una columna compuesta, `n` y
  # `sum(aplicable)` describian universos de distinto tamano.
  # El universo aplicable. Sin declaracion, toda la columna aplica y todo lo de
  # abajo se reduce al comportamiento de siempre. Con declaracion, las filas
  # donde la columna no corresponde salen del denominador: un ausente por
  # diseno no es un ausente por error, y contarlos juntos era informar como
  # defecto lo que es la forma del dato.
  n_aplicables <- sum(aplicable)
  n_indeterminados <- attr(aplicable, "n_indeterminados", exact = TRUE)
  if (is.null(n_indeterminados)) n_indeterminados <- 0L
  n_no_aplica <- n - n_aplicables - n_indeterminados
  n_faltantes <- sum(is.na(x) & aplicable)
  # Un valor presente donde la regla dice que no corresponde es un hallazgo por
  # derecho propio, no algo para descartar en silencio. Las filas donde el
  # predicado NO SE PUDO DECIDIR no entran: ahi la regla no dice que no
  # corresponda, dice que no sabe, y contarlas aca las publicaba como error
  # ajeno a lo que la propia fila declara una columna antes.
  indeterminado_por_fila <- attr(aplicable, "indeterminados_por_fila", exact = TRUE)
  if (is.null(indeterminado_por_fila) ||
      length(indeterminado_por_fila) != length(aplicable)) {
    indeterminado_por_fila <- rep(FALSE, length(aplicable))
  }
  n_presentes_fuera <- sum(!is.na(x) & !aplicable & !indeterminado_por_fila)
  n_presentes_indeterminadas <- sum(!is.na(x) & indeterminado_por_fila)
  n_codificacion_invalida <- length(preparacion_texto$posiciones)
  # La identidad describe la representacion almacenada: los bytes invalidos
  # se pueden distinguir sin decodificarlos y no son faltantes. Los analisis
  # textuales tienen su propio alcance, declarado por
  # `n_codificacion_invalida` y el hallazgo correspondiente.
  n_validos <- n_aplicables - n_faltantes
  valor_concentrado <- .estadisticos_valor_concentrado(
    x_identidad, sentinelas_declarados
  )
  # La convencion decimal que la fila publica viaja con la inferencia a TODO lo
  # que lee la columna como numero -la secuencia entera, los faltantes
  # disfrazados, el resumen cuantitativo-, y por eso se calcula antes que ellos:
  # leian `1.500` como 1,5 al lado de `numero_texto_convencion = "decimal_coma"`.
  # Ver `.leer_numeros_texto()`.
  inferencia$convencion_numero_texto <- numeros_texto$convencion
  secuencia_entera <- .resumen_secuencia_entera(
    x_analisis_diagnostico, inferencia, formatos
  )
  # Los desvios de `patron_raro` se deciden cuando la fila ya esta armada, mas
  # abajo: la numeracion se lee de ella.
  # La mascara se calcula en UNA funcion, que tambien usa `detectar_claves()`.
  faltantes_disfrazados <- .faltantes_disfrazados_columna(
    x_analisis_diagnostico, inferencia, formatos, secuencia_entera,
    sentinelas_numericos,
    sentinelas_declarados, cadenas_ausencia, aplicable, nombre = nombre
  )
  rol_propuesto <- .propuesta_escala(x, inferencia$tipo)$rol
  casi_clave <- .resumen_casi_clave(
    x_analisis_diagnostico, rol = rol_propuesto, tipo_implicito = inferencia$tipo,
    # Dentro de `perfilar_por()`, la que el grupo decide solo: ver
    # `.disfrazados_con_la_columna_entera()`. Fuera, la misma mascara.
    disfrazados = faltantes_disfrazados$mascara_grupo %||%
      faltantes_disfrazados$mascara
  )
  vocabulario_fecha <- if (
    (is.character(x_analisis_diagnostico) ||
       is.factor(x_analisis_diagnostico)) &&
      inferencia$tipo %in% c("fecha", "fecha-hora")
  ) {
    textos_fecha <- trimws(x_analisis_diagnostico)
    presentes_fecha <- !is.na(textos_fecha) & nzchar(textos_fecha)
    valores_fecha <- if (is.null(vocabulario_texto_caro)) NULL else {
      unicos <- trimws(vocabulario_texto_caro)
      unique(unicos[!is.na(unicos) & nzchar(unicos)])
    }
    .vocabulario_texto(
      textos_fecha[presentes_fecha], .umbral_vocabulario_barato,
      valores = valores_fecha
    )
  } else NULL
  moda <- .moda_columna(x_identidad, claves = claves_identidad)
  longitudes <- .resumen_longitud(x_analisis)
  cuantitativo <- .resumen_cuantitativo(
    x_analisis_diagnostico, inferencia, formatos, meses_texto = meses_texto,
    sentinelas_numericos = sentinelas_numericos,
    vocabulario = vocabulario_fecha,
    valores_preparados = x_analisis_diagnostico
  )
  # Declarada en `columnas_codigo`: ver `.resumen_codigo_declarado()`.
  if (isTRUE(codigo)) cuantitativo <- .resumen_codigo_declarado(cuantitativo)
  # Los diagnosticos de texto miran solo el universo aplicable, como ya lo
  # hacen el analisis y la identidad de mas arriba. Recibian la columna CRUDA
  # mientras su `vocabulario` venia armado sobre `x_analisis`, ya enmascarado:
  # dos vistas distintas en la misma llamada. Con `aplicabilidad` declarada,
  # `espacios_sobrantes` contaba la columna entera y trazaba solo lo aplicable,
  # y el paquete disparaba su propia alarma -"cuenta 6 y traza 4"-. Se marca
  # como ausente en vez de recortar, por el mismo motivo que arriba: para que
  # los indices de fila sigan alineados con la tabla.
  x_texto <- if (escrito) x_texto_diagnostico else {
    rep(NA_character_, length(x))
  }
  if (!all(aplicable)) {
    # Una geometria no admite `NA` -ver arriba, `x_identidad`- y los
    # diagnosticos de texto la tratan igual que a una columna de ausentes
    # (medido: mismo resultado), asi que se le pasa eso.
    if (inherits(x_texto, "sfc")) {
      x_texto <- rep(NA_character_, length(x_texto))
    } else {
      x_texto[!aplicable] <- NA
    }
  }
  diagnostico_texto <- .diagnosticar_texto(
    x_texto, vocabulario = vocabulario_texto_caro
  )
  if (is.character(x_analisis_diagnostico) ||
      is.factor(x_analisis_diagnostico)) {
    espacios_completos <- !is.na(x_analisis_diagnostico) &
      .celdas_texto_protegidas(
        as.character(x_analisis_diagnostico), "recortar_espacios"
      )
    diagnostico_texto$n_espacios_borde <- as.integer(sum(espacios_completos))
    if (any(espacios_completos)) {
      publicables_completos <- .vista_publicable(
        preparacion_texto$valores_identidad, x_analisis_diagnostico
      )
      diagnostico_texto$evidencia_espacios <- paste(
        .citar_publicable(utils::head(
          .identificadores_unicos(publicables_completos[espacios_completos]),
          6L
        )), collapse = "; "
      )
    }
  }
  multivaluados <- if (is.character(x_analisis_diagnostico) ||
                       is.factor(x_analisis_diagnostico)) {
    .detectar_multivaluados(x_analisis_diagnostico)
  } else list()
  n_blancos <- if (is.character(x_analisis_diagnostico) ||
                   is.factor(x_analisis_diagnostico)) {
    sum(!is.na(x_analisis_diagnostico) &
          !nzchar(trimws(as.character(x_analisis_diagnostico))))
  } else {
    0L
  }

  fila <- .data_frame_simple(
    columna = nombre,
    tipo_declarado = .tipo_declarado(x),
    tipo_inferido = inferencia$tipo,
    estado_tipo_inferido = inferencia$estado_tipo_inferido,
    proporcion_tipo_inferido = inferencia$proporcion,
    n_filas_analizadas_tipo = inferencia$n_analizados,
    muestreado_tipo_inferido = inferencia$muestreado,
    n = n,
    # El denominador de toda proporcion de ausencia es el universo aplicable,
    # no el total de filas. Sin declaracion son el mismo numero.
    n_aplicables = n_aplicables,
    n_no_aplica = n_no_aplica,
    n_aplicabilidad_indeterminada = n_indeterminados,
    n_presentes_en_aplicabilidad_indeterminada = n_presentes_indeterminadas,
    n_presentes_fuera_de_aplicabilidad = n_presentes_fuera,
    n_faltantes = n_faltantes,
    prop_faltantes = if (n_aplicables) n_faltantes / n_aplicables else NA_real_,
    n_faltantes_disfrazados = faltantes_disfrazados$n,
    n_faltantes_disfrazados_textuales = faltantes_disfrazados$n_textuales,
    n_faltantes_disfrazados_numericos = faltantes_disfrazados$n_numericos,
    prop_faltantes_disfrazados = if (n_aplicables) {
      faltantes_disfrazados$n / n_aplicables
    } else NA_real_,
    n_faltantes_totales = n_faltantes + faltantes_disfrazados$n,
    prop_faltantes_totales = if (n_aplicables) {
      (n_faltantes + faltantes_disfrazados$n) / n_aplicables
    } else {
      NA_real_
    },
    n_distintos = n_distintos,
    tasa_distintos = if (n_validos && !is.na(n_distintos)) {
      n_distintos / n_validos
    } else NA_real_,
    secuencia_entera_densa = secuencia_entera$densa,
    densidad_secuencia_entera = secuencia_entera$densidad,
    n_posiciones_secuencia_entera = secuencia_entera$n_posiciones,
    n_huecos_secuencia_entera = secuencia_entera$n_huecos,
    hueco_maximo_secuencia_entera = secuencia_entera$hueco_maximo,
    salto_de_escala_secuencia_entera = secuencia_entera$salto_de_escala,
    moda_sobresale_secuencia_entera = secuencia_entera$moda_sobresale,
    umbral_densidad_secuencia_entera = secuencia_entera$umbral_densidad,
    min_distintos_secuencia_entera = secuencia_entera$min_distintos,
    moda = moda$valor,
    frecuencia_moda = moda$frecuencia,
    longitud_minima = unname(longitudes[["minimo"]]),
    longitud_maxima = unname(longitudes[["maximo"]]),
    longitud_media = unname(longitudes[["media"]]),
    # El universo de las tres de arriba: sobre cuantos valores se calcularon.
    # `nchar()` no puede medir un valor cuyos bytes no son UTF-8 valido, y esos
    # quedaban afuera sin que nada lo dijera. Se declara igual que
    # `n_fechas_resumidas` declara el del resumen de fecha; la CAUSA viaja aparte,
    # en `n_codificacion_invalida`.
    n_longitudes_resumidas = as.integer(unname(longitudes[["n_resumidas"]])),
    minimo = cuantitativo$minimo,
    maximo = cuantitativo$maximo,
    media = cuantitativo$media,
    mediana = cuantitativo$mediana,
    desvio = cuantitativo$desvio,
    minimo_exacto = cuantitativo$minimo_exacto,
    maximo_exacto = cuantitativo$maximo_exacto,
    minimo_fecha = cuantitativo$minimo_fecha,
    maximo_fecha = cuantitativo$maximo_fecha,
    media_fecha = cuantitativo$media_fecha,
    mediana_fecha = cuantitativo$mediana_fecha,
    n_fechas_resumidas = cuantitativo$n_fechas_resumidas,
    n_fechas_excluidas_granularidad =
      cuantitativo$n_fechas_excluidas_granularidad,
    n_valores_excluidos_resumen = cuantitativo$n_valores_excluidos_resumen,
    n_ceros = cuantitativo$n_ceros,
    n_negativos = cuantitativo$n_negativos,
    n_outliers = cuantitativo$n_outliers,
    centinela_valor = cuantitativo$centinela_valor,
    centinela_repeticiones = cuantitativo$centinela_repeticiones,
    densidad_sin_centinela = cuantitativo$densidad_sin_centinela,
    n_nan = cuantitativo$n_nan,
    n_infinito_positivo = cuantitativo$n_infinito_positivo,
    n_infinito_negativo = cuantitativo$n_infinito_negativo,
    estado_resumen_cuantitativo = cuantitativo$estado_resumen_cuantitativo,
    # La unidad viaja al lado de las cifras que califica. Sin ella, cuatro
    # columnas `units` distintas -`kg`, `km/h`, `m^2`, `kg*m/s^2`- publicaban la
    # MISMA fila, y un `Period` de dos dias publicaba `172800` sin decir que eran
    # segundos. Ver `.unidad_del_resumen()`.
    #
    # Y se calla cuando la fila no publica NINGUNA cifra cuantitativa, que es lo
    # que el campo promete calificar. El atributo `units` circula como metadato en
    # datos importados y lo lleva cualquier clase: medido, un `factor` con
    # `attr(x, "units") <- "kg"` publicaba `unidad = "kg"` junto a `media = NA` y
    # `estado_resumen_cuantitativo = "no_aplica"`. Una unidad de cifras que no
    # existen no contesta la pregunta del campo.
    #
    # El discriminador es `no_aplica` -"esta clase no se resume"- y no una lista
    # de clases: una columna `units` de puros `NA` sigue declarando su unidad,
    # porque su clase SI se resume y la unidad dice en que estarian sus cifras.
    unidad = if (
      cuantitativo$estado_resumen_cuantitativo %in%
        .ESTADOS_SIN_RESUMEN_CUANTITATIVO
    ) {
      NA_character_
    } else {
      .unidad_del_resumen(x)
    },
    zona_horaria_origen = zona_horaria_origen,
    n_filas_fecha_civil_distinta_utc = n_filas_fecha_civil_distinta_utc,
    fecha_civil_distinta_utc = if (is.na(n_filas_fecha_civil_distinta_utc)) {
      NA
    } else n_filas_fecha_civil_distinta_utc > 0L,
    # Como llego la geometria y si se pudo interpretar. Sin esto, una columna
    # WKT o WKB queda indistinguible de un texto cualquiera aguas abajo, y la
    # cobertura conceptual llega a afirmar que la geometria no aplica sobre
    # datos que si son geometricos.
    representacion_geometria = if (is.null(geometria$representacion_geometria)) {
      NA_character_
    } else {
      geometria$representacion_geometria
    },
    motivo_representacion = if (is.null(geometria$motivo_representacion)) {
      NA_character_
    } else {
      geometria$motivo_representacion
    },
    crs_declarado = geometria$crs_declarado,
    tipo_geometria = geometria$tipo_geometria,
    dimension_geometria = geometria$dimension_geometria,
    dimensiones_no_evaluadas = geometria$dimensiones_no_evaluadas,
    n_geometrias_vacias = geometria$n_geometrias_vacias,
    n_geometrias_invalidas = geometria$n_geometrias_invalidas,
    n_validez_evaluados = geometria$n_validez_evaluados,
    validez_criterio = geometria$validez_criterio,
    validez_preprocesamiento = geometria$validez_preprocesamiento,
    n_fuera_de_dominio = geometria$n_fuera_de_dominio,
    n_dominio_evaluados = geometria$n_dominio_evaluados,
    n_bbox_evaluados = geometria$n_bbox_evaluados,
    # El trabajo que hizo el presupuesto geometrico. `?perfilar` los promete
    # publicados y solo existian dentro del analisis: la unica traza era el texto
    # de la cobertura, y solo cuando habia recorte.
    n_geometrias_analizadas = geometria$n_geometrias_analizadas,
    n_vertices_analizados = geometria$n_vertices_analizados,
    bbox_alcance = geometria$bbox_alcance,
    bbox_xmin = geometria$bbox_xmin,
    bbox_xmax = geometria$bbox_xmax,
    bbox_ymin = geometria$bbox_ymin,
    bbox_ymax = geometria$bbox_ymax,
    detalle_proteccion_personal = NA_character_,
    n_blancos = n_blancos,
    n_espacios_borde = diagnostico_texto$n_espacios_borde,
    n_variantes_mayusculas = diagnostico_texto$n_variantes_mayusculas,
    n_variantes_unicode = diagnostico_texto$n_variantes_unicode,
    unicode_evaluado = if (isTRUE(preparacion_texto$analizable) && escrito &&
                           (is.character(x_analisis) || is.factor(x_analisis))) {
      diagnostico_texto$unicode_evaluado
    } else {
      NA
    },
    n_codificacion_rota = diagnostico_texto$n_codificacion_rota,
    n_codificacion_reparable = diagnostico_texto$n_codificacion_reparable,
    n_codificacion_reparable_parcialmente = diagnostico_texto$n_codificacion_reparable_parcialmente,
    n_codificacion_irreparable = diagnostico_texto$n_codificacion_irreparable,
    n_codificacion_no_se_pudo = diagnostico_texto$n_codificacion_no_se_pudo,
    estado_codificacion_reparacion = diagnostico_texto$estado_codificacion_reparacion,
    n_codificacion_invalida = diagnostico_texto$n_codificacion_invalida,
    n_controles_invisibles = diagnostico_texto$n_controles_invisibles,
    n_invisibles_eliminables = diagnostico_texto$n_invisibles_eliminables,
    n_espacios_invisibles = diagnostico_texto$n_espacios_invisibles,
    n_invisibles_significativos = diagnostico_texto$n_invisibles_significativos,
    n_entidades_html = diagnostico_texto$n_entidades_html,
    n_separadores_en_campo = diagnostico_texto$n_separadores_en_campo,
    n_numeros_texto = numeros_texto$n,
    proporcion_numeros_texto = numeros_texto$proporcion,
    numero_texto_ambiguo = numeros_texto$ambiguo,
    numero_texto_seguro = numeros_texto$seguro,
    numero_texto_unidad = numeros_texto$unidad,
    numero_texto_moneda = numeros_texto$moneda,
    numero_texto_convencion = numeros_texto$convencion,
    stringsAsFactors = FALSE
  )
  # En una numeracion que arranca en 1 los valores de una cifra son su comienzo:
  # que difieran del dominante solo en el largo de la corrida de digitos no es un
  # desvio. Lo decidia solo `secuencia_entera_densa`, y un identificador con
  # huecos -`sample(1:4557, 3159)`, densidad 0,693- recibia `patron_raro` al
  # lado de `posible_identificador`. Ahora lo decide tambien la numeracion, la
  # misma respuesta de `.decision_identificador_numerico()` que publica
  # `posible_identificador`. La cobertura densa sigue alcanzando por si sola: un
  # tramo lleno sin repetir trae todos los largos por construccion, sea o no un
  # identificador -`c(1:2000, rep(500, 500))` no es numeracion por la moda que
  # sobresale y sus `1` a `9` tampoco son desvios-.
  attr(patrones, "desvios_patron_raro") <- .desvios_patron_raro_detectados(
    patrones,
    isTRUE(secuencia_entera$densa) ||
      isTRUE(.decision_identificador_numerico(fila)$parece),
    umbral_patron_raro
  )

  list(
    fila = fila,
    inferencia = inferencia,
    formatos = formatos,
    patrones = patrones,
    secuencia_entera = secuencia_entera,
    casi_clave = casi_clave,
    faltantes_disfrazados = faltantes_disfrazados,
    diagnostico_texto = diagnostico_texto,
    numeros_texto = numeros_texto,
    multivaluados = multivaluados,
    valor_concentrado = valor_concentrado,
    geometria = geometria,
    cuantitativo = cuantitativo,
    sentinelas_numericos_declarados = sentinelas_declarados,
    # La mascara viaja con el resultado para que la trazabilidad pueda nombrar
    # las filas del hallazgo de valor fuera de aplicabilidad.
    aplicable = aplicable,
    # La declaracion `columnas_codigo`, para los hallazgos: la marca del resumen
    # numerico no existe en un codigo guardado como texto que no se lee numero.
    codigo = isTRUE(codigo),
    texto_largo = texto_largo
  )
}

.perfilar_columna_matriz <- function(x, nombre, muestra, max_patrones,
                                     distinguir_mayusculas, expandir,
                                     umbral_patron_raro,
                                     sentinelas_numericos,
                                     max_largo_valor_vocabulario =
                                       .MAX_LARGO_VALOR_CASI_DUPLICADOS) {
  resultado <- .perfilar_columna(
    rep(NA_character_, NROW(x)), nombre, muestra, max_patrones,
    distinguir_mayusculas, expandir, umbral_patron_raro,
    sentinelas_numericos,
    max_largo_valor_vocabulario = max_largo_valor_vocabulario
  )
  fila <- resultado$fila
  # `.tipo_declarado()` y no `"matriz"` fijo: una tabla anidada es compuesta y
  # declara su propia clase.
  fila$tipo_declarado <- .tipo_declarado(x)
  fila$tipo_inferido <- "desconocido"
  fila$estado_tipo_inferido <- NA_character_
  fila$proporcion_tipo_inferido <- NA_real_
  fila$n_filas_analizadas_tipo <- NA_integer_
  fila$muestreado_tipo_inferido <- NA
  fila$n <- NROW(x)
  enteros_na <- c(
    "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
    "n_presentes_en_aplicabilidad_indeterminada",
    "n_presentes_fuera_de_aplicabilidad",
    "n_faltantes", "n_faltantes_disfrazados",
    "n_faltantes_disfrazados_textuales",
    "n_faltantes_disfrazados_numericos", "n_faltantes_totales",
    "n_distintos", "frecuencia_moda", "n_ceros", "n_negativos",
    "n_outliers", "n_nan", "n_infinito_positivo", "n_infinito_negativo",
    "n_valores_excluidos_resumen",
    "n_blancos", "n_espacios_borde", "n_variantes_mayusculas",
    "representacion_geometria", "motivo_representacion",
    "n_variantes_unicode", "n_codificacion_rota",
    "n_codificacion_reparable", "n_codificacion_reparable_parcialmente",
    "n_codificacion_irreparable", "n_codificacion_no_se_pudo",
    "n_codificacion_invalida", "n_controles_invisibles",
    "n_invisibles_eliminables", "n_espacios_invisibles",
    "n_invisibles_significativos", "n_entidades_html",
    "n_separadores_en_campo", "n_numeros_texto"
  )
  fila[enteros_na] <- NA_integer_
  reales_na <- c(
    "prop_faltantes", "prop_faltantes_disfrazados",
    "prop_faltantes_totales", "tasa_distintos", "longitud_minima",
    "longitud_maxima", "longitud_media", "minimo", "maximo", "media",
    "mediana", "desvio", "proporcion_numeros_texto"
  )
  fila[reales_na] <- NA_real_
  fila$moda <- NA_character_
  fila$minimo_exacto <- NA_character_
  fila$maximo_exacto <- NA_character_
  fila$minimo_fecha <- NA_character_
  fila$maximo_fecha <- NA_character_
  fila$media_fecha <- NA_character_
  fila$mediana_fecha <- NA_character_
  fila$estado_resumen_cuantitativo <- "tipo_compuesto_no_analizado"
  # Una columna compuesta no publica ninguna cifra cuantitativa, asi que su unidad
  # se calla, igual que en la rama principal. La version anterior la publicaba
  # "como en `difftime`", y esa analogia dejo de valer el dia que `difftime`
  # empezo a publicar cifras.
  fila$unidad <- if (
    fila$estado_resumen_cuantitativo %in% .ESTADOS_SIN_RESUMEN_CUANTITATIVO
  ) {
    NA_character_
  } else {
    .unidad_del_resumen(x)
  }
  fila$zona_horaria_origen <- NA_character_
  fila$n_filas_fecha_civil_distinta_utc <- NA_integer_
  fila$fecha_civil_distinta_utc <- NA
  fila$unicode_evaluado <- NA
  fila$numero_texto_ambiguo <- FALSE
  fila$numero_texto_seguro <- FALSE
  fila$numero_texto_unidad <- ""
  fila$numero_texto_moneda <- ""
  fila$numero_texto_convencion <- ""
  resultado$fila <- fila
  resultado$inferencia$tipo <- "desconocido"
  resultado$inferencia$proporcion <- NA_real_
  resultado$inferencia$estado_tipo_inferido <- NA_character_
  resultado$inferencia$compatibles <- 0L
  resultado$inferencia$n_analizados <- 0L
  # Una columna sin escritura no tiene dimensiones: sus componentes son los
  # campos de su registro, si los tiene, o el valor entero.
  sin_escritura <- !is.data.frame(x) && !.es_arreglo_compuesto(x)
  resultado$estructura_no_analizada <- list(
    tipo = if (is.data.frame(x)) {
      "tabla"
    } else if (sin_escritura) {
      "sin_escritura"
    } else {
      "matriz"
    },
    filas = NROW(x),
    # Un registro guarda sus campos con nombre y del largo de la tabla; una
    # lista de objetos guarda uno por fila.
    componentes = if (sin_escritura) {
      campos <- unclass(x)
      if (is.list(campos) && length(campos) && !is.null(names(campos)) &&
          all(lengths(campos) == NROW(x))) length(campos) else 1L
    } else {
      NCOL(x)
    },
    clase = class(x)[[1L]],
    dimensiones = dim(x)
  )
  resultado
}
