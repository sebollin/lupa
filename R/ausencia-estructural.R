# Una senal que sugiere declarar el universo, sin adivinarlo.
#
# `aplicabilidad` y `columnas_opcionales` resuelven el vacio por diseno, y
# funcionan. Pero exigen que el usuario los use: quien perfila una tabla con
# columnas condicionadas sin declarar nada recibe exactamente el mismo informe
# enganoso que antes -dos columnas al 50 % de ausencia informadas como error-.
# La vineta lo documenta, y eso no es lo mismo que ponerlo delante de quien lo
# necesita.
#
# La deteccion automatica del modelo esta descartada: el paquete no adivina si
# una tabla es entidad-atributo-valor ni reescribe el universo por su cuenta.
# Lo que si puede hacer es medir la evidencia y ofrecerla. Cuando la presencia
# de una columna esta determinada por el valor de otra, o cuando dos columnas se
# reparten las filas sin pisarse, eso es un hecho medible sobre los datos, no
# una interpretacion. Se informa con severidad `ok`, se dice que es una
# sospecha, y se entrega la linea exacta que habria que escribir. Quien decide
# sigue siendo el usuario.

# Cuantas filas hacen falta para que el patron signifique algo. Con menos, dos
# columnas se reparten las filas por casualidad con demasiada frecuencia.
.min_filas_ausencia_estructural <- 30L
# Columnas candidatas que se comparan de a pares. El tope existe porque el costo
# es cuadratico; cuando recorta, se declara.
.max_columnas_ausencia_estructural <- 40L
# Maximo de niveles para que una columna sirva de determinante. Por encima deja
# de ser un discriminador y pasa a ser un identificador.
.max_niveles_determinante <- 20L
# Cuanto puede pisarse un grupo de columnas excluyentes y seguir siendo un
# reparto. No es cero porque un dato real tiene ruido, y el solapamiento
# medido viaja en la evidencia.
.max_solapamiento_ausencia <- 0.01
# Cuanto de la tabla tiene que cubrir el grupo para que el reparto sea un
# reparto y no dos columnas flojas que casualmente no se pisan.
.min_cobertura_ausencia <- 0.95
# Cuan bien tiene que predecir el determinante. Por debajo de esto la relacion
# existe pero no es una regla, y ofrecer una formula seria afirmar de mas.
.min_determinacion_ausencia <- 0.99
# Cuanta ausencia tiene que haber para que valga la pena explicarla. Una columna
# con tres celdas vacias no necesita que le declaren un universo, y ajustar una
# regla a tres filas produce reglas que no significan nada.
.min_ausentes_ausencia <- 10L
.min_prop_ausencia_ausencia <- 0.05
# Un nivel del determinante con dos filas no es una regla: es una anecdota. Y
# una particion donde un lado ocupa el 0,5 % tampoco parte nada.
.min_filas_nivel_determinante <- 5L
# Cuantas columnas se prueban como determinantes. Igual que el tope de
# candidatas, existe porque el examen es de a pares, y cuando recorta se dice.
.max_determinantes_ausencia <- 30L
# Cuantos cortes se prueban al buscar un umbral. Con mas, el costo crece sin que
# la regla mejore: un corte cada medio por ciento de la columna ya la encuentra.
.max_cortes_umbral <- 200L
.min_prop_lado_determinante <- 0.02

# La ausencia que cuenta aca es la total -vacio y disfrazado-, porque una
# columna que no corresponde se llena tanto con `NA` como con cadena vacia o
# con "no corresponde", y las tres significan lo mismo para este patron.
#
# Y es la MISMA ausencia que cuenta el perfil: `is.na()`, tambien sobre una
# lista -el `NA` suelto, y el `NULL` solo donde la clase lo declara ausente,
# como `blob`-. Un `NULL` o un `integer(0)` en una lista sin clase no es un
# faltante para el perfil (decision escrita en `.posiciones_presentes()`, en
# `R/geometrias.R`). Aca la lista tenia su propia regla -largo cero o `NULL`
# era ausencia- y el objeto se contradecia: una lista
# con `integer(0)` en diez de treinta filas publicaba `n_faltantes = 0` y un
# `posible_ausencia_estructural` que "explicaba 10 de las 10 ausencias"; y
# seguir su sugerencia al pie de la letra fabricaba un
# `valor_fuera_de_aplicabilidad` sospechoso sobre las mismas diez celdas.
.mascara_ausencia_columna <- function(x, disfrazados, n) {
  if (is.data.frame(x) || .es_columna_compuesta(x)) return(NULL)
  ausente <- is.na(x)
  if (length(ausente) != n) return(NULL)
  mascara <- disfrazados$mascara
  if (!is.null(mascara) && length(mascara) == n) ausente <- ausente | mascara
  ausente
}

# Tres numeros que separan mascaras distintas sin construir una clave del largo
# de la tabla. Las colisiones se resuelven comparando, asi que la huella puede
# ser barata sin costar exactitud.
.huella_mascara <- function(m) {
  idx <- which(m)
  # `idx * idx` desbordaba el entero a partir de la fila 46.341, y el desborde
  # no se notaba por su aviso sino por lo que hacia: el tercer numero salia `NA`
  # y la huella quedaba con dos, o sea mas debil justo en las tablas grandes,
  # que es donde comparar cuesta. Medido, dos mascaras distintas de una tabla de
  # 60.000 filas daban las dos `2-105000-NA`.
  #
  # Se reduce antes de multiplicar, que es la misma cuenta -`(a*a) mod p` es
  # `((a mod p) * (a mod p)) mod p`- pero sin salirse del rango: el resto es
  # menor que 1.000.003, asi que su cuadrado entra holgado en un doble exacto.
  restos <- as.numeric(idx %% 1000003L)
  paste(
    length(idx),
    sum(restos),
    sum((restos * restos) %% 1000003),
    sep = "-"
  )
}

# El corte es codigo para pegar, no prosa: tiene que ser exacto, ejecutable y no
# depender de la sesion que lo escribio ni de la que lo pega.
.texto_corte_umbral <- function(ajuste) {
  clase <- ajuste$clase_umbral
  if (!is.null(clase) && clase %in% c("Date", "POSIXct", "POSIXt") &&
      !is.na(ajuste$etiqueta_corte)) {
    etiqueta <- .comillas_sin_locale(ajuste$etiqueta_corte, quote = "\"")
    if (identical(clase, "Date")) return(paste0("as.Date(", etiqueta, ")"))
    # Un instante sin zona se lee en la zona de la sesion que lo pega. Medido:
    # `as.POSIXct("2024-03-01")` sobre marcas en UTC ubicaba bien todas las
    # filas con la sesion en UTC y dejaba seis presentes fuera del universo en
    # America/Montevideo. La etiqueta se escribe en la zona del dato -o en UTC si
    # el dato no declara ninguna- y esa zona viaja en la llamada.
    zona <- ajuste$zona_corte
    if (is.null(zona) || is.na(zona) || !nzchar(zona)) zona <- "UTC"
    return(paste0(
      "as.POSIXct(", etiqueta, ", tz = ", .comillas_sin_locale(zona, quote = "\""), ")"
    ))
  }
  # La misma escritura que la etiqueta de un grupo, por la misma razon: tiene que
  # volver al valor del que salio. Fija `OutDec` y `scipen` -con `OutDec = ","`
  # la sugerencia decia `ingreso >= 30122,33`, que no es codigo R- y agrega
  # cifras hasta que el texto vuelve al doble exacto, que las 15 de `format()`
  # no garantizan.
  # `LC_NUMERIC` -la otra perilla- la cubre la misma escritura
  # (`.marca_decimal_c()`): el arreglo que vivia aca con `Sys.localeconv()` paso
  # a `utils.R`, donde escribe para todos los numeros que tienen que volver.
  .etiqueta_numero_reversible(ajuste$corte)
}

.texto_valores_nivel <- function(valores) {
  # Un nivel con comillas o barras daba una sugerencia que no es codigo de R
  # valido, y ademas la proteccion de datos personales reconoce lo citado en la
  # prosa por esas comillas. Se escapan en bytes -el nivel puede no ser UTF-8
  # valido- y se repone la marca, que `useBytes` borra al reemplazar: lo agregado
  # son barras ASCII y no cambia la codificacion.
  marcas <- Encoding(valores)
  valores <- gsub("([\"\\\\])", "\\\\\\1", valores, useBytes = TRUE)
  Encoding(valores) <- marcas
  entrecomillados <- paste0("\"", valores, "\"")
  if (length(valores) == 1L) return(entrecomillados)
  paste0("c(", paste(entrecomillados, collapse = ", "), ")")
}

# Un umbral tambien es una regla, y es la mas frecuente. `edad >= 65` decide si
# hay jubilacion; `fecha_alta >= X` decide si hay un campo que se empezo a pedir
# ese dia; un ingreso por encima de un corte decide si hay un tramo declarado.
# El camino por niveles no la ve: una edad tiene sesenta y tres valores
# distintos y queda descartada como determinante porque parece identificador.
#
# El corte se busca sobre el vector ordenado una sola vez, con sumas
# acumuladas, asi que cada par cuesta O(n) y no O(n log n): el orden se prepara
# junto con el determinante y se reutiliza para todas las columnas candidatas.
.determinacion_umbral <- function(presente, det) {
  orden <- det$orden
  if (is.null(orden)) return(NULL)
  presente_ordenado <- presente[det$usable][orden]
  n <- length(presente_ordenado)
  if (n < .min_filas_ausencia_estructural) return(NULL)
  acumulado <- cumsum(presente_ordenado)
  total_presentes <- acumulado[[n]]
  # Cortes posibles: solo donde el valor cambia, porque partir dentro de un
  # empate produciria una regla que no se puede escribir.
  cortes <- det$cortes
  if (!length(cortes)) return(NULL)
  # Debajo del corte hay `i` filas con `acumulado[i]` presentes; encima hay
  # `n - i` con `total_presentes - acumulado[i]`.
  aciertos_en <- function(posiciones, mayor) {
    presentes_abajo <- acumulado[posiciones]
    if (mayor) {
      (posiciones - presentes_abajo) + (total_presentes - presentes_abajo)
    } else {
      presentes_abajo + ((n - posiciones) - (total_presentes - presentes_abajo))
    }
  }
  # **El corte muestreado se refina al limite exacto.** Con mas de 200 cambios
  # de valor se prueba uno cada medio por ciento, y el corte publicado era el
  # valor siguiente al corte muestreado: sobre un ingreso continuo con la regla
  # exacta `>= 50000` -ausente mas alto 49.989,31, presente mas bajo 50.001,97-
  # se sugeria `>= 49849.27`, que cumple 0,9988, y pegada tal cual dejaba seis
  # faltantes dentro del universo. La sugerencia promete "la linea exacta". El
  # muestreo sigue eligiendo la zona, y despues se prueban TODOS los cambios
  # entre los dos cortes muestreados vecinos: el costo es de un tramo, no de la
  # columna. Los cambios del tramo se recalculan sobre los valores ordenados en
  # vez de guardarlos, que costaria un vector del largo de la tabla por cada
  # determinante.
  mejor_en <- function(mayor) {
    aciertos <- aciertos_en(cortes, mayor)
    j <- which.max(aciertos)
    if (!isTRUE(det$cortes_muestreados)) {
      return(list(aciertos = aciertos[[j]], indice = cortes[[j]]))
    }
    tramo <- seq.int(cortes[[max(1L, j - 1L)]],
                     cortes[[min(length(cortes), j + 1L)]])
    ordenados <- det$valores_ordenados
    vecinos <- tramo[ordenados[tramo] != ordenados[tramo + 1L]]
    aciertos_vecinos <- aciertos_en(vecinos, mayor)
    k <- which.max(aciertos_vecinos)
    list(aciertos = aciertos_vecinos[[k]], indice = vecinos[[k]])
  }
  mayor <- mejor_en(TRUE)
  menor <- mejor_en(FALSE)
  if (mayor$aciertos >= menor$aciertos) {
    aciertos <- mayor$aciertos
    indice <- mayor$indice
    sentido <- ">="
    # Las ausentes que la regla explica son las que quedan del lado donde la
    # columna no corresponde: debajo del corte.
    explicados <- indice - acumulado[[indice]]
  } else {
    aciertos <- menor$aciertos
    indice <- menor$indice
    sentido <- "<"
    explicados <- (n - indice) - (total_presentes - acumulado[[indice]])
  }
  cumplimiento <- aciertos / n
  if (cumplimiento < .min_determinacion_ausencia) return(NULL)
  lado_menor <- min(indice, n - indice)
  if (lado_menor < n * .min_prop_lado_determinante) return(NULL)
  list(
    cumplimiento = cumplimiento,
    n_usables = det$n_usables,
    n_niveles = det$n_niveles,
    corte = det$valores_ordenados[[indice + 1L]],
    clase_umbral = det$clase_umbral,
    zona_corte = det$zona_umbral,
    etiqueta_corte = if (!is.null(det$etiquetas_ordenadas)) {
      det$etiquetas_ordenadas[[indice + 1L]]
    } else NA_character_,
    sentido = sentido,
    n_indeterminados = det$n_indeterminados,
    n_ausentes_explicados = as.integer(explicados),
    # Donde cae el corte en el vector ordenado: es lo que separa el lado donde
    # la columna no corresponde, y con eso se nombran las filas que
    # `n_ausentes_explicados` cuenta. Ver `.filas_ausencia_explicada()`.
    posicion_corte = indice
  )
}

# Un determinante se prepara una sola vez y se contrasta contra todas las
# columnas candidatas. Construir el factor dentro del par lo reconstruye una vez
# por comparacion, y con doscientas columnas eso es la diferencia entre un
# segundo y cuarenta.
.preparar_determinante <- function(x, nombre, n) {
  if (is.data.frame(x) || .es_columna_compuesta(x) || is.list(x)) return(NULL)
  if (length(x) != n) return(NULL)
  usable <- !is.na(x)
  n_usables <- sum(usable)
  if (n_usables < .min_filas_ausencia_estructural) return(NULL)
  # El camino por umbral sirve para lo ordenable, y no le importa cuantos
  # niveles haya: `edad >= 65` es una regla aunque la edad tenga sesenta y tres
  # valores. Se prepara aparte, y el de niveles sigue su propio camino.
  ordenable <- (is.numeric(x) || inherits(x, c("Date", "POSIXt"))) &&
    !is.object(x) || inherits(x, c("Date", "POSIXt"))
  umbral <- NULL
  if (isTRUE(ordenable)) {
    valores <- suppressWarnings(as.numeric(x[usable]))
    if (!anyNA(valores) && length(unique(valores)) >= 2L) {
      orden <- .orden_por_bytes(valores)
      ordenados <- valores[orden]
      cambios <- which(ordenados[-length(ordenados)] != ordenados[-1L])
      muestreados <- length(cambios) > .max_cortes_umbral
      if (muestreados) {
        cambios <- cambios[unique(round(seq(
          1, length(cambios), length.out = .max_cortes_umbral
        )))]
      }
      # La clase viaja con el corte: `as.numeric()` sobre una fecha da los dias
      # desde 1970, y una sugerencia que diga `>= 18628` no se puede pegar.
      # La zona tambien: ver `.texto_corte_umbral()`.
      zona <- NA_character_
      etiquetable <- x[usable]
      if (inherits(x, "POSIXct")) {
        zona <- attr(x, "tzone", exact = TRUE)
        zona <- if (length(zona) && !is.na(zona[[1L]]) && nzchar(zona[[1L]])) {
          zona[[1L]]
        } else "UTC"
        attr(etiquetable, "tzone") <- zona
      }
      umbral <- list(
        orden = orden, valores_ordenados = ordenados, cortes = cambios,
        cortes_muestreados = muestreados,
        clase_umbral = class(x)[[1L]], zona_umbral = zona,
        etiquetas_ordenadas = as.character(etiquetable)[orden]
      )
    }
  }
  # Un vistazo barato antes de construir el factor: si los primeros valores ya
  # traen mas niveles de los admitidos, no hace falta mirar el resto.
  asomo_crudo <- x[usable][seq_len(min(n_usables, 5000L))]
  asomo <- if (is.character(asomo_crudo) || is.factor(asomo_crudo)) {
    unique(.nombres_para_operar(as.character(asomo_crudo)))
  } else unique(asomo_crudo)
  if (length(asomo) > .max_niveles_determinante) {
    if (is.null(umbral)) return(NULL)
    return(c(list(
      nombre = nombre, usable = usable, n_usables = n_usables,
      codigos = integer(), etiquetas = character(), n_niveles = NA_integer_,
      n_por_nivel = integer(), n_indeterminados = n - n_usables,
      solo_umbral = TRUE
    ), umbral))
  }
  niveles <- .factor_por_bytes(.nombres_para_operar(as.character(x[usable])))
  if (nlevels(niveles) < 2L || nlevels(niveles) > .max_niveles_determinante) {
    return(NULL)
  }
  n_por_nivel <- tabulate(as.integer(niveles), nlevels(niveles))
  if (min(n_por_nivel) < .min_filas_nivel_determinante) {
    if (is.null(umbral)) return(NULL)
    return(c(list(
      nombre = nombre, usable = usable, n_usables = n_usables,
      codigos = integer(), etiquetas = character(), n_niveles = NA_integer_,
      n_por_nivel = integer(), n_indeterminados = n - n_usables,
      solo_umbral = TRUE
    ), umbral))
  }
  c(list(
    nombre = nombre, usable = usable, n_usables = n_usables,
    codigos = as.integer(niveles), etiquetas = levels(niveles),
    n_niveles = nlevels(niveles), n_por_nivel = n_por_nivel,
    n_indeterminados = n - n_usables, solo_umbral = FALSE
  ), if (is.null(umbral)) list() else umbral)
}

# Mide si el valor del determinante decide la presencia de la columna. No
# alcanza con que el cumplimiento sea alto: hace falta que el determinante
# **parta** -que haya niveles donde la columna esta y niveles donde no-, porque
# si no, una columna sin ausencias da cumplimiento 1 contra cualquier cosa.
.determinacion_ausencia <- function(presente, det) {
  if (isTRUE(det$solo_umbral)) return(NULL)
  presentes_por_nivel <- tabulate(
    det$codigos[presente[det$usable]], det$n_niveles
  )
  aciertos <- sum(pmax(presentes_por_nivel,
                       det$n_por_nivel - presentes_por_nivel))
  cumplimiento <- aciertos / det$n_usables
  if (cumplimiento < .min_determinacion_ausencia) return(NULL)
  aplica <- presentes_por_nivel * 2L > det$n_por_nivel
  # Sin niveles de los dos lados no hay regla: hay una columna que siempre esta
  # o que nunca esta, que es otro diagnostico.
  if (!any(aplica) || all(aplica)) return(NULL)
  lado_menor <- min(sum(det$n_por_nivel[aplica]),
                    sum(det$n_por_nivel[!aplica]))
  if (lado_menor < det$n_usables * .min_prop_lado_determinante) return(NULL)
  list(
    cumplimiento = cumplimiento,
    n_usables = det$n_usables,
    n_niveles = det$n_niveles,
    niveles_aplica = det$etiquetas[aplica],
    n_indeterminados = det$n_indeterminados,
    # Las ausentes que caen en los niveles donde la columna no corresponde.
    n_ausentes_explicados = as.integer(sum(
      (det$n_por_nivel - presentes_por_nivel)[!aplica]
    )),
    # Los mismos niveles, por codigo: con ellos se nombran esas filas. Ver
    # `.filas_ausencia_explicada()`.
    por_nivel_aplica = aplica
  )
}

# Las filas que cuenta `n_ausentes_explicados`: las ausentes que caen donde la
# regla dice que la columna no corresponde. Salen de las mismas cantidades con
# que se contaron -los codigos de nivel, o la posicion del corte en el vector
# ordenado- y no de la regla publicada: reconstruirlas desde la etiqueta del
# corte o desde los nombres de los niveles seria contar por segunda vez, con
# otra escritura del mismo valor.
#
# Las ausencias que la regla NO explica -las que caen donde la columna si
# corresponde- no van: el hallazgo no las cuenta. Se llega a ellas declarando
# la regla, que deja un `faltantes` con esas filas.
.filas_ausencia_explicada <- function(presente, det, ajuste) {
  usables <- which(det$usable)
  if (is.null(ajuste$posicion_corte)) {
    no_corresponde <- !ajuste$por_nivel_aplica[det$codigos]
  } else {
    # Debajo del corte estan las primeras `posicion_corte` filas del vector
    # ordenado. Con `>=` la columna no corresponde ahi; con `<`, en el resto.
    debajo <- logical(length(usables))
    debajo[det$orden[seq_len(ajuste$posicion_corte)]] <- TRUE
    no_corresponde <- if (identical(ajuste$sentido, ">=")) debajo else !debajo
  }
  usables[no_corresponde & !presente[usables]]
}

# La traza de un hallazgo de ausencia, armada por quien lo conto. Sin tope: lo
# aplica `.trazabilidad_hallazgo()` al publicar, que ademas pone las claves.
# `columnas` viaja en la traza cuando el hallazgo es de varias columnas: el
# campo `columna` no alcanza para separar nombres que llevan comas.
.con_filas_de_ausencia <- function(hallazgo, filas, columnas = NULL) {
  traza <- .trazabilidad_indices(filas, "completo", limite = Inf)
  if (!is.null(columnas)) traza$columnas <- as.character(columnas)
  hallazgo$trazabilidad <- I(list(traza))
  hallazgo
}

.hallazgo_ausencia_determinada <- function(columna, determinante, ajuste,
                                           n_ausentes,
                                           columna_texto = columna,
                                           determinante_texto = determinante) {
  por_umbral <- !is.null(ajuste$corte)
  formula <- if (por_umbral) {
    paste0("~ ", determinante_texto, " ", ajuste$sentido, " ",
           .texto_corte_umbral(ajuste))
  } else {
    paste0(
      "~ ", determinante_texto,
      if (length(ajuste$niveles_aplica) == 1L) " == " else " %in% ",
      .texto_valores_nivel(ajuste$niveles_aplica)
    )
  }
  # `n_afectados` cuenta las ausencias que la regla explica: las que caen donde
  # la columna no corresponde. Era `round(cumplimiento * n_ausentes)`, que mezcla
  # el acierto sobre TODAS las filas con un conteo de ausentes y no es ningun
  # conteo verificable: con nueve ausencias al azar dentro del universo daba 108
  # donde la regla explica 100, y con el determinante faltante en 200 filas
  # vacias daba 300 donde explica 100. Las filas con el determinante faltante no
  # cuentan como explicadas, igual que `aplicabilidad` no las cuenta ni como
  # aplicables ni como no aplicables.
  explicados <- ajuste$n_ausentes_explicados
  .nuevo_hallazgo(
    columna, "posible_ausencia_estructural", "ok",
    paste0(
      "La ausencia de esta columna parece por dise\u00f1o y no por error: qu\u00e9 ",
      "filas la tienen queda decidido por el valor de otra columna. Es una ",
      "sospecha medida, no una conclusi\u00f3n; `lupa` no cambia el universo por ",
      "su cuenta."
    ),
    paste0(
      "`", determinante_texto, "` predice la presencia de `", columna_texto,
      "` en ", .formatear_decimal_publicado(100 * ajuste$cumplimiento, 1L),
      " % de ",
      ajuste$n_usables, if (ajuste$n_usables == 1L) " fila" else " filas",
      if (por_umbral) {
        ", por un umbral"
      } else {
        paste0(", con ", ajuste$n_niveles,
               if (ajuste$n_niveles == 1L) " valor distinto" else " valores distintos")
      },
      if (ajuste$n_indeterminados > 0L) {
        paste0(" y ", ajuste$n_indeterminados,
               if (ajuste$n_indeterminados == 1L) " fila" else " filas",
               " donde el determinante falta")
      } else {
        ""
      },
      ". La regla explica ", explicados,
      if (n_ausentes == 1L) " ausencia de la columna" else
        paste0(" de las ", n_ausentes, " ausencias de la columna"),
      ". La columna corresponde cuando ", determinante_texto,
      if (por_umbral) {
        paste0(" ", ajuste$sentido, " ", .texto_corte_umbral(ajuste))
      } else if (length(ajuste$niveles_aplica) == 1L) {
        paste0(" es ", .texto_valores_nivel(ajuste$niveles_aplica))
      } else {
        paste0(" est\u00e1 en ", .texto_valores_nivel(ajuste$niveles_aplica))
      },
      "."
    ),
    paste0(
      "Si es as\u00ed, declararlo y volver a perfilar: ",
      "`perfilar(datos, aplicabilidad = list(", columna_texto, " = ", formula,
      "))`. Con la regla declarada, la ausencia fuera de ese universo deja de ",
      "contarse como defecto y el alcance queda escrito en ",
      "`cobertura_diagnosticos`."
    ),
    n_ausentes, explicados, "fila"
  )
}

.hallazgo_ausencia_excluyente <- function(columna, grupo, solapamiento,
                                          cobertura, n, n_ausentes,
                                          columna_texto = columna,
                                          grupo_texto = grupo) {
  otras_texto <- grupo_texto[
    !(.nombres_para_operar(grupo) %in% .nombres_para_operar(columna))
  ]
  .nuevo_hallazgo(
    columna, "posible_ausencia_estructural", "ok",
    paste0(
      "La ausencia de esta columna parece por dise\u00f1o y no por error: se ",
      "reparte las filas con otras columnas sin pisarse, que es la forma de ",
      "una tabla con atributos excluyentes. Es una sospecha medida, no una ",
      "conclusi\u00f3n."
    ),
    paste0(
      "`", columna_texto, "` no coincide con ",
      paste0("`", otras_texto, "`", collapse = ", "),
      if (length(grupo) == 1L) ": la columna cubre " else
        paste0(": las ", length(grupo),
               if (length(grupo) == 1L) " columna cubre " else " columnas cubren "),
      .formatear_decimal_publicado(100 * cobertura, 1L),
      if (n == 1L) " % de la fila y se pisa en " else
        " % de las filas y se pisan en ",
      solapamiento, if (solapamiento == 1L) " fila." else " filas."
    ),
    paste0(
      "Si hay una columna que dice cu\u00e1l de las ", length(grupo),
      " corresponde en cada fila, declararla con `aplicabilidad`; si la ",
      "ausencia es por dise\u00f1o y no depende de ninguna otra columna, ",
      "`columnas_opcionales = c(",
      paste0("\"", grupo_texto, "\"", collapse = ", "), ")`."
    ),
    n_ausentes, n_ausentes, "fila"
  )
}

# Las filas que cuenta `regla_silencia_ausencia` salen de la misma mascara que
# su conteo, que no es la misma en los dos origenes. En una columna opcional son
# las de fuera del universo, y un faltante disfrazado NO es una de ellas: ahi el
# universo son las celdas con valor, y `s/d` es un valor. Medido: con 150 `NA` y
# 10 `s/d` el aviso cuenta 150, y la ausencia del perfil -`NA` o disfrazado-
# daria 160. Con `aplicabilidad` son las ausentes de dentro del universo, las
# mismas del `faltantes` que sale al lado.
.filas_regla_silenciosa <- function(origen, x, resultado) {
  aplicable <- resultado$aplicable
  if (!is.logical(aplicable)) return(NULL)
  # Las filas indeterminadas de una regla tambien estan fuera de `aplicable`.
  if (identical(origen, "columnas_opcionales")) return(which(!aplicable))
  ausente <- .mascara_ausencia_columna(
    x, resultado$faltantes_disfrazados, length(aplicable)
  )
  if (is.null(ausente)) return(NULL)
  which(ausente & aplicable)
}

# Un aviso, no un defecto: la regla declarada esta haciendo su trabajo, y por
# eso mismo el perfil sale limpio. Quien no lea `cobertura_diagnosticos` no se
# entera de que una columna casi vacia dejo de contar. El aviso lo pone en la
# misma tabla que todo lo demas.
.hallazgos_regla_silenciosa <- function(nombres, resultados, reglas, umbral,
                                        datos = NULL) {
  if (!nrow(reglas)) return(list())
  salida <- list()
  for (k in seq_len(nrow(reglas))) {
    i <- .indice_nombre(reglas$columna[[k]], nombres)
    if (is.na(i)) next
    fila <- resultados[[i]]$fila
    origen <- reglas$origen[[k]]
    # Una columna declarada opcional tiene, por construccion, cero ausencias
    # dentro de su universo: el universo son las celdas con valor. Medir ahi
    # daria siempre cero y el aviso no existiria nunca. Lo que interesa es
    # cuanto de la columna quedo fuera.
    if (identical(origen, "columnas_opcionales")) {
      n_ausentes <- reglas$n_no_aplica[[k]] + reglas$n_indeterminados[[k]]
      n_aplicables <- n_ausentes + reglas$n_aplicables[[k]]
    } else {
      n_ausentes <- fila$n_faltantes_totales[[1L]]
      n_aplicables <- fila$n_aplicables[[1L]]
    }
    proporcion <- if (n_aplicables > 0L) n_ausentes / n_aplicables else NA_real_
    if (!isTRUE(is.finite(proporcion)) || proporcion < umbral) next
    filas <- if (is.null(datos)) NULL else {
      .filas_regla_silenciosa(origen, datos[[i]], resultados[[i]])
    }
    aviso <- .nuevo_hallazgo(
      reglas$columna[[k]], "regla_silencia_ausencia", "ok",
      paste0(
        "La declaraci\u00f3n del universo evit\u00f3 que esta columna apareciera como ",
        "incompleta, y dentro del universo declarado sigue faltando la mayor ",
        "parte. Puede estar bien; el aviso existe para que la decisi\u00f3n sea ",
        "expl\u00edcita y no un efecto de la declaraci\u00f3n."
      ),
      paste0(
        if (n_ausentes == 1L) "Falta " else "Faltan ", n_ausentes,
        if (n_aplicables == 1L) " de 1 " else paste0(" de ", n_aplicables, " "),
        if (identical(origen, "columnas_opcionales")) {
          if (n_aplicables == 1L) "celda de la columna (" else
            "celdas de la columna ("
        } else {
          if (n_aplicables == 1L) "celda del universo declarado (" else
            "celdas del universo declarado ("
        },
      .formatear_decimal_publicado(100 * proporcion, 1L),
      " %), y la columna se declar\u00f3 con `",
        origen, "`: ", reglas$regla[[k]], "."
      ),
      if (identical(origen, "columnas_opcionales")) {
        paste0(
          "Comprobar que la ausencia sea realmente esperada. Si la columna ",
          "deber\u00eda tener valor en algunas filas, `aplicabilidad` permite ",
          "decir en cu\u00e1les en vez de eximirla entera."
        )
      } else {
        paste0(
          "Comprobar la regla declarada: dentro del universo que define, la ",
          "columna sigue casi vac\u00eda, as\u00ed que o la regla es m\u00e1s ancha que el ",
          "universo real o falta el dato."
        )
      },
      n_aplicables, n_ausentes, "fila"
    )
    if (!is.null(filas)) aviso <- .con_filas_de_ausencia(aviso, filas)
    salida[[length(salida) + 1L]] <- aviso
  }
  salida
}

# El mismo perfil puede informar, sobre la misma ausencia, un `faltantes` de
# severidad `error` y un `posible_ausencia_estructural` de severidad `ok`. Los
# dos son ciertos -uno cuenta celdas vacias, el otro mide que la vacancia sigue
# un patron- pero quien lee el primero sin ver el segundo se lleva un defecto
# que probablemente no lo sea.
#
# La salida NO es degradar la severidad del `faltantes`. Se considero y se
# descarto con un caso concreto: en una tabla pivoteada -meses por anios- la
# correlacion entre el mes y la columna del anio es real, y sin embargo un mes
# sin dato puede ser un hueco genuino. Degradar ahi esconderia el problema, que
# es justo lo que este paquete no hace. Ademas `posible_ausencia_estructural`
# tiene severidad `ok` porque el paquete **no sabe** que la ausencia sea
# estructural: sabe que lo parece. Bajar una severidad real apoyandose en una
# sospecha es decidir por el usuario.
#
# Lo que si corresponde es que el `faltantes` **nombre la senal**: quien lo lee
# se entera de que hay una lectura alternativa y donde encontrarla. Ver y
# decidir sigue siendo suyo.
.cruzar_faltantes_con_estructural <- function(hallazgos, senales) {
  if (!length(senales) || !nrow(hallazgos)) return(hallazgos)
  frases <- c(
    posible_ausencia_estructural = paste0(
      ". Hay adem\u00e1s un hallazgo `posible_ausencia_estructural` sobre esta ",
      "columna: parte de esta ausencia podr\u00eda ser por dise\u00f1o. La ",
      "severidad no se baja por esa sospecha; leer las dos y decidir."
    ),
    faltantes_conjuntos = paste0(
      ". Hay adem\u00e1s un hallazgo `faltantes_conjuntos`: esta columna falta ",
      "junto con otras, lo que puede ser un bloque que no se carg\u00f3 o ",
      "columnas que s\u00f3lo se completan juntas."
    ),
    ausencia_dependiente = paste0(
      ". Hay adem\u00e1s un hallazgo `ausencia_dependiente`: la proporci\u00f3n ",
      "de ausentes cambia seg\u00fan el valor de otra columna, lo que puede ser ",
      "un bloque que no se carg\u00f3 o una columna que s\u00f3lo se completa en ",
      "ciertos grupos."
    )
  )
  for (tipo in names(frases)) {
    senales_tipo <- senales[vapply(senales, function(h) {
      identical(as.character(h$tipo_hallazgo[[1L]]), tipo)
    }, logical(1L))]
    if (!length(senales_tipo)) next
    columnas <- unique(unlist(lapply(senales_tipo, function(h) {
      traza <- h$trazabilidad[[1L]]
      if (is.list(traza) && length(traza$columnas)) {
        as.character(traza$columnas)
      } else if (identical(tipo, "faltantes_conjuntos")) {
        .separar_nombres_columnas_historico(
          as.character(h$columna[[1L]]), sep = ","
        )
      } else {
        as.character(h$columna[[1L]])
      }
    }), use.names = FALSE))
    if (!length(columnas)) next
    objetivo <- hallazgos$tipo_hallazgo == "faltantes" &
      .nombres_para_operar(as.character(hallazgos$columna)) %in%
        .nombres_para_operar(columnas)
    if (!any(objetivo)) next
    hallazgos$evidencia[objetivo] <- paste0(
      sub("[.[:space:]]*$", "", hallazgos$evidencia[objetivo]),
      frases[[tipo]]
    )
  }
  hallazgos
}

.cobertura_ausencia_estructural <- function(columna, motivo, como) {
  data.frame(
    diagnostico = "posible_ausencia_estructural",
    columna = columna, motivo = motivo, como_resolverlo = como,
    dependencia = NA_character_, columnas = I(list(as.character(columna))),
    stringsAsFactors = FALSE
  )
}

.diagnosticar_ausencia_estructural <- function(datos, nombres, resultados,
                                               aplicabilidad_resuelta,
                                               umbral_faltantes_error,
                                               nombres_texto = NULL,
                                               marcas = NULL,
                                               excluidas = character()) {
  if (is.null(nombres_texto)) nombres_texto <- .marcar_utf8_textos(nombres)
  vacio <- list(
    hallazgos = list(), cobertura = .cobertura_diagnosticos_vacia()
  )
  avisos <- .hallazgos_regla_silenciosa(
    nombres, resultados, aplicabilidad_resuelta$reglas, umbral_faltantes_error,
    datos = datos
  )
  vacio$hallazgos <- avisos
  n <- nrow(datos)
  if (!length(nombres)) return(vacio)
  pocas_filas <- n < .min_filas_ausencia_estructural

  declaradas <- aplicabilidad_resuelta$reglas$columna
  nombres_operativos <- .nombres_para_operar(nombres)
  declaradas_operativas <- .nombres_para_operar(declaradas)
  excluidas_operativas <- .nombres_para_operar(excluidas)
  ausentes <- vector("list", length(nombres))
  no_atomicas <- character()
  for (i in seq_along(nombres)) {
    mascara <- .mascara_ausencia_columna(
      datos[[i]], resultados[[i]]$faltantes_disfrazados, n
    )
    if (is.null(mascara)) {
      no_atomicas <- c(no_atomicas, nombres[[i]])
      next
    }
    ausentes[[i]] <- mascara
  }

  cobertura <- .cobertura_diagnosticos_vacia()

  # Candidata es la que tiene algo presente y algo ausente. Una columna entera o
  # entera vacia no puede repartirse con nadie, y ya tiene su propio hallazgo.
  n_ausentes <- vapply(
    ausentes, function(m) if (is.null(m)) NA_integer_ else sum(m), integer(1L)
  )
  candidata <- !is.na(n_ausentes) & n_ausentes >= .min_ausentes_ausencia &
    n_ausentes < n & n_ausentes >= n * .min_prop_ausencia_ausencia &
    !(nombres_operativos %in% c(declaradas_operativas, excluidas_operativas))
  indices <- which(candidata)
  # El piso de filas se declara solo cuando habia algo que examinar. Anunciar en
  # cada tabla de diez filas que no se busco un patron que no tenia candidatos
  # llenaria la tabla de cobertura de avisos vacios, y esa tabla existe para
  # decir que se dejo de medir, no para inventariar lo que no venia al caso.
  if (pocas_filas) {
    if (length(indices)) {
      cobertura <- rbind(cobertura, .cobertura_ausencia_estructural(
        paste(nombres[indices], collapse = ", "),
        paste0(
          "No se busc\u00f3 ausencia estructural: la tabla tiene ", n,
          if (nrow(datos) == 1L) " fila y se necesita al menos " else
            " filas y se necesitan al menos ", .min_filas_ausencia_estructural,
          if (.min_filas_ausencia_estructural == 1L) " para que un reparto entre columnas signifique algo." else
            " para que un reparto entre columnas signifique algo."
        ),
        "Perfilar una tabla con m\u00e1s filas si interesa esta se\u00f1al."
      ))
    }
    return(list(hallazgos = avisos, cobertura = cobertura))
  }
  if (length(indices) > .max_columnas_ausencia_estructural) {
    recortadas <- nombres[indices[-seq_len(.max_columnas_ausencia_estructural)]]
    indices <- indices[seq_len(.max_columnas_ausencia_estructural)]
    cobertura <- rbind(cobertura, .cobertura_ausencia_estructural(
      paste(recortadas, collapse = ", "),
      paste0(
        "No se busc\u00f3 ausencia estructural en ", length(recortadas),
        if (length(recortadas) == 1L) " columna: el examen compara columnas de a pares y se detiene en " else
          " columnas: el examen compara columnas de a pares y se detiene en ",
        .max_columnas_ausencia_estructural,
        " para que el costo no crezca al cuadrado."
      ),
      "Perfilar por subconjuntos de columnas si interesa examinarlas todas."
    ))
  }
  # El reparto entre columnas necesita dos; la determinacion por otra columna
  # funciona con una sola, asi que el corte va aca y no antes.
  if (!length(indices)) {
    return(list(hallazgos = avisos, cobertura = cobertura))
  }
  if (length(no_atomicas) && length(indices)) {
    cobertura <- rbind(cobertura, .cobertura_ausencia_estructural(
      paste(no_atomicas, collapse = ", "),
      paste0(
        "No se busc\u00f3 ausencia estructural en ", length(no_atomicas),
        if (length(no_atomicas) == 1L) " columna que no es vector at\u00f3mico: el patr\u00f3n se mide sobre " else
          " columnas que no son vectores at\u00f3micos: el patr\u00f3n se mide sobre ",
        "presencia y ausencia celda a celda."
      ),
      "No corresponde resolverlo; queda declarado."
    ))
  }

  presentes <- lapply(indices, function(i) !ausentes[[i]])
  names(presentes) <- nombres[indices]

  # Primero la determinacion, que es la senal accionable: entrega la formula.
  determinantes <- list()
  for (j in seq_along(nombres)) {
    if (length(determinantes) >= .max_determinantes_ausencia) {
      cobertura <- rbind(cobertura, .cobertura_ausencia_estructural(
        NA_character_,
        paste0(
          "Se examinaron ", .max_determinantes_ausencia,
          if (.max_determinantes_ausencia == 1L) " columna como posible determinante de la ausencia y el resto no " else
            " columnas como posibles determinantes de la ausencia y el resto no ",
          "se prob\u00f3: el examen es de a pares y el tope acota el costo."
        ),
        "Perfilar por subconjuntos de columnas si interesa probarlas todas."
      ))
      break
    }
    preparado <- .preparar_determinante(datos[[j]], nombres[[j]], n)
    if (!is.null(preparado)) {
      preparado$indice <- j
      determinantes[[length(determinantes) + 1L]] <- preparado
    }
  }

  hallazgos <- list()
  explicadas <- character()
  for (k in seq_along(indices)) {
    columna <- nombres[[indices[[k]]]]
    mejor <- NULL
    mejor_det <- NULL
    mejor_nombre <- NA_character_
    mejor_indice <- NA_integer_
    # La marca que `aplicar()` agrego sobre la columna -`.ausente_<x>`- es su
    # ausencia escrita en otra columna: medido, despues de `aplicar()`
    # publicaba que `.ausente_Ozone` "predice la presencia de `Ozone` en 100 %"
    # y "explica 37 de las 37 ausencias". No es una estructura del dato. Se
    # reconoce por el atributo que deja `aplicar()`, no por el nombre: una
    # columna del usuario llamada asi si es una estructura del dato, y callarla
    # era silenciar un hallazgo.
    propias <- if (is.data.frame(marcas) && nrow(marcas)) {
      .nombres_para_operar(marcas$marca[
        !is.na(marcas$columna) &
          .nombres_para_operar(marcas$columna) %in% .nombres_para_operar(columna)
      ])
    } else character()
    for (det in determinantes) {
      if (det$indice == indices[[k]]) next
      if (.nombres_para_operar(nombres[[det$indice]]) %in% propias) next
      ajuste <- .determinacion_ausencia(presentes[[k]], det)
      if (is.null(ajuste)) ajuste <- .determinacion_umbral(presentes[[k]], det)
      if (is.null(ajuste)) next
      # Entre dos determinantes que explican lo mismo gana el de menos niveles:
      # la regla mas corta es la que el usuario puede leer y confirmar.
      niveles_de <- function(x) {
        if (isTRUE(is.finite(x$n_niveles))) x$n_niveles else 2L
      }
      if (is.null(mejor) || ajuste$cumplimiento > mejor$cumplimiento ||
          (ajuste$cumplimiento == mejor$cumplimiento &&
             niveles_de(ajuste) < niveles_de(mejor))) {
        mejor <- ajuste
        mejor_det <- det
        mejor_nombre <- det$nombre
        mejor_indice <- det$indice
      }
    }
    if (!is.null(mejor)) {
      hallazgos[[length(hallazgos) + 1L]] <- .con_filas_de_ausencia(
        .hallazgo_ausencia_determinada(
          columna, mejor_nombre, mejor, n_ausentes[[indices[[k]]]],
          columna_texto = nombres_texto[[indices[[k]]]],
          determinante_texto = nombres_texto[[mejor_indice]]
        ),
        .filas_ausencia_explicada(presentes[[k]], mejor_det, mejor)
      )
      explicadas <- c(explicadas, columna)
    }
  }

  # Despues el reparto entre columnas, para las que quedaron sin determinante.
  restantes <- which(!(nombres_operativos[indices] %in%
                         .nombres_para_operar(explicadas)))
  if (length(restantes) >= 2L) {
    huellas <- vapply(restantes, function(k) .huella_mascara(presentes[[k]]),
                      character(1L))
    orden <- restantes[.orden_por_bytes(
      vapply(restantes, function(k) sum(presentes[[k]]), integer(1L)),
      decreasing = TRUE
    )]
    tope_solape <- floor(.max_solapamiento_ausencia * n)
    usadas_operativas <- character()
    for (semilla in orden) {
      if (nombres_operativos[indices[[semilla]]] %in% usadas_operativas) next
      grupo <- semilla
      union <- presentes[[semilla]]
      solapamiento <- 0L
      for (otro in orden) {
        if (otro %in% grupo) next
        if (nombres_operativos[indices[[otro]]] %in% usadas_operativas) next
        # Mascaras identicas no son un reparto: son la misma condicion repetida.
        if (huellas[[match(otro, restantes)]] ==
              huellas[[match(semilla, restantes)]] &&
            identical(presentes[[otro]], presentes[[semilla]])) next
        pisada <- sum(union & presentes[[otro]])
        if (pisada > tope_solape) next
        grupo <- c(grupo, otro)
        solapamiento <- solapamiento + pisada
        union <- union | presentes[[otro]]
      }
      if (length(grupo) < 2L) next
      cubierto <- sum(union) / n
      if (cubierto < .min_cobertura_ausencia) next
      nombres_grupo <- nombres[indices[grupo]]
      nombres_grupo_texto <- nombres_texto[indices[grupo]]
      usadas_operativas <- c(usadas_operativas,
                             nombres_operativos[indices[grupo]])
      for (k in grupo) {
        # Aca el hallazgo cuenta todas las ausencias de la columna.
        hallazgos[[length(hallazgos) + 1L]] <- .con_filas_de_ausencia(
          .hallazgo_ausencia_excluyente(
            nombres[[indices[[k]]]], nombres_grupo, solapamiento, cubierto, n,
            n_ausentes[[indices[[k]]]],
            columna_texto = nombres_texto[[indices[[k]]]],
            grupo_texto = nombres_grupo_texto
          ),
          which(!presentes[[k]])
        )
      }
    }
  }

  list(hallazgos = c(avisos, hallazgos), cobertura = cobertura)
}

# ---------------------------------------------------------------------------
# Patrones de ausencia que no son una regla casi determinista.
#
# `posible_ausencia_estructural` responde a "esta columna falta porque el valor
# de otra decide donde corresponde". Los tres diagnosticos de abajo responden a
# preguntas distintas y por eso tienen tipo propio: dos columnas que faltan
# juntas, una columna que falta mas en un grupo que en otro, y filas sin ningun
# valor. Los tres comparten la lectura de ausencia del perfil -`NA` y lo ya
# detectado como disfrazado-, porque contar dos veces cosas distintas seria
# publicar un patron que los datos no tienen.
# ---------------------------------------------------------------------------

# Una columna casi siempre vacia hace trivial cualquier coincidencia: con 95 %
# de ausencias, dos columnas independientes comparten 90 % de las filas. El
# techo y el `lift` contra la independencia separan la coincidencia medida de la
# que produce una columna vacia.
.max_prop_ausencia_conjuntos <- 0.90
.min_lift_conjuntos <- 1.5
# Una columna es candidata si tiene algo presente y algo ausente. El piso es
# doble a proposito: el absoluto evita que en una tabla chica una ausencia de
# tres filas entre en la tabla de patrones, y el relativo evita que en una tabla
# enorme veinte filas sueltas lo hagan. Con 5 % una columna que falta en 40 de
# 1.000 quedaba fuera aunque su bloque comun fuera medible; con 20 filas y 2 %
# entra, y el patron conjunto sigue exigiendo sus propios minimos.
.min_ausentes_conjuntos <- 20L
.min_prop_ausencia_conjuntos <- 0.02
.max_columnas_conjuntos <- 40L
# Un patron de ausencia -la combinacion exacta de columnas que faltan en una
# fila- agrupa las filas que lo comparten, como `mice::md.pattern()`. Se exige
# un bloque minimo de filas y una proporcion minima de la tabla: un patron con
# dos filas en una tabla de mil es una casualidad, y uno con diez filas en una
# tabla de un millon no mueve ninguna conclusion. La proporcion es menor que la
# de cada columna porque el bloque comun puede ser una parte chica de las
# ausencias de cada una, que es justo el caso que los pares no ven.
.min_filas_patron_conjuntos <- 10L
.min_prop_patron_conjuntos <- 0.01

# Para la dependencia, el efecto se mide como diferencia de proporciones entre
# grupos. Un p-valor solo no sirve: con muchas filas, una diferencia de dos
# puntos porcentuales es significativa y no es un hallazgo util.
.min_diferencia_ausencia_dependiente <- 0.20
.min_grupo_ausencia_dependiente <- 20L
.max_niveles_ausencia_dependiente <- 20L
.min_ausentes_grupo_dependiente <- 10L
.max_columnas_dependiente <- 40L
.max_determinantes_dependiente <- 40L

.cobertura_patron_ausencia <- function(diagnostico, columna, motivo, como,
                                       columnas = NULL) {
  if (is.null(columnas)) columnas <- as.character(columna)
  data.frame(
    diagnostico = diagnostico,
    columna = if (length(columna)) as.character(columna) else NA_character_,
    motivo = motivo, como_resolverlo = como,
    dependencia = NA_character_, columnas = I(list(as.character(columnas))),
    stringsAsFactors = FALSE
  )
}

# La tabla de patrones de ausencia, en la misma idea que `mice::md.pattern()`:
# cada fila distinta de la matriz de indicadores es un patron, y se cuenta
# cuantas filas lo comparten. No se usa `mice` ni se agrega a Suggests: con el
# tope de columnas, cada patron cabe en los 53 bits exactos de un double y la
# clave se arma con una multiplicacion de la matriz por potencias de dos.
.patrones_ausencia <- function(mascaras) {
  k <- length(mascaras)
  if (k < 2L) return(NULL)
  n <- length(mascaras[[1L]])
  if (!n) return(NULL)
  matriz <- matrix(FALSE, nrow = n, ncol = k)
  for (i in seq_len(k)) matriz[, i] <- mascaras[[i]]
  claves <- drop(matriz %*% 2 ^ (seq_len(k) - 1L))
  unicas <- unique(claves)
  codigos <- match(claves, unicas)
  list(
    matriz = matriz,
    cuentas = tabulate(codigos, length(unicas)),
    codigos = codigos,
    primeras = which(!duplicated(codigos)),
    n = n, k = k
  )
}

# Las filas donde no hay ningun valor. Se exige ausencia real -`is.na()`-, no
# una ausencia disfrazada: un codigo 999 o un "S/D" es un valor presente, y una
# fila llena de ellos no es una fila vacia aunque el perfil los cuente como
# faltantes para otras medidas. En una columna compuesta se exige que todas sus
# celdas esten ausentes. Si una columna no se puede leer, la fila no se acusa
# vacia: callar una fila dudosa es preferible a inventar una fila vacia.
.filas_vacias_perfil <- function(datos, n) {
  if (!n || !ncol(datos)) return(integer())
  vacia <- rep(TRUE, n)
  for (i in seq_along(datos)) {
    x <- datos[[i]]
    mascara <- tryCatch({
      if (is.data.frame(x)) {
        !.filas_completas(x)
      } else if (is.matrix(x) || (is.array(x) && length(dim(x)) > 1L)) {
        dimensiones <- dim(x)
        if (is.null(dimensiones) || dimensiones[[1L]] != n) {
          NULL
        } else {
          celdas <- is.na(x)
          dim(celdas) <- c(n, length(celdas) / n)
          rowSums(celdas) == ncol(celdas)
        }
      } else {
        is.na(x)
      }
    }, error = function(e) NULL)
    if (is.null(mascara) || length(mascara) != n) {
      mascara <- rep(FALSE, n)
    }
    vacia <- vacia & as.logical(mascara)
  }
  which(vacia)
}

# Un determinante se prepara una vez y se contrasta contra todas las columnas
# objetivo. Los niveles se tabulan por codigo; el tope de un numero se compara
# contra el resto. Las dos formas son grupos medidos, no una categoria inventada
# para toda columna numerica.
.preparar_dependencia_ausencia <- function(x, nombre, n, resultado = NULL) {
  if (is.data.frame(x) || .es_columna_compuesta(x) || is.list(x)) return(NULL)
  if (length(x) != n) return(NULL)
  # Un identificador no sirve para agrupar: sus valores casi no se repiten y el
  # tope de un casi unico junta unas pocas filas que no son un universo. La
  # decision no es un umbral propio: `casi_clave` es la clave con colisiones que
  # ya publica el perfil, y `.decision_identificador_numerico()` es su
  # numeracion -densa y casi sin repetir- con las guardas de tipo y secuencia.
  # Una fecha con un tope repetido queda afuera de las dos y sigue agrupando.
  if (!is.null(resultado)) {
    es_identificador <- isTRUE(resultado$casi_clave$es_casi_clave) ||
      isTRUE(.decision_identificador_numerico(resultado$fila)$posible)
    if (es_identificador) return(NULL)
  }
  usable <- !is.na(x)
  n_usables <- sum(usable)
  if (n_usables < 2L * .min_grupo_ausencia_dependiente) return(NULL)
  valores <- x[usable]
  etiquetas <- NULL
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    etiquetas <- as.character(valores)
    distintos <- unique(etiquetas[!is.na(etiquetas)])
  } else {
    distintos <- unique(valores)
  }
  candidatos <- list()

  if (is.factor(x) || is.character(x) || is.logical(x) ||
      is.numeric(x) || inherits(x, c("Date", "POSIXt"))) {
    if (length(distintos) >= 2L &&
        length(distintos) <= .max_niveles_ausencia_dependiente) {
      if (is.null(etiquetas)) etiquetas <- .etiqueta_numero_reversible(valores)
      niveles <- .factor_por_bytes(etiquetas)
      codigos <- as.integer(niveles)
      n_por <- tabulate(codigos, nlevels(niveles))
      if (min(n_por) >= .min_grupo_ausencia_dependiente) {
        candidatos[[length(candidatos) + 1L]] <- list(
          tipo = "niveles", codigos = codigos, n_por = n_por,
          etiquetas = levels(niveles)
        )
      }
    }
  }

  if ((is.numeric(valores) || inherits(valores, c("Date", "POSIXt"))) &&
      !inherits(valores, c("difftime", "units"))) {
    maximo <- suppressWarnings(max(valores))
    numerico <- suppressWarnings(as.numeric(maximo))
    if (length(numerico) == 1L && is.finite(numerico)) {
      en_tope <- valores == maximo
      n_tope <- sum(en_tope)
      n_resto <- n_usables - n_tope
      if (n_tope >= .min_grupo_ausencia_dependiente &&
          n_resto >= .min_grupo_ausencia_dependiente) {
        candidatos[[length(candidatos) + 1L]] <- list(
          tipo = "tope",
          codigos = ifelse(en_tope, 1L, 2L),
          n_por = c(n_tope, n_resto),
          etiquetas = c(.etiqueta_numero_reversible(maximo), "resto")
        )
      }
    }
  }

  if (!length(candidatos)) return(NULL)
  list(
    nombre = nombre, usable = usable, n_usables = n_usables,
    candidatos = candidatos
  )
}

.hallazgo_faltantes_conjuntos <- function(nombres_grupo, n_ausentes_grupo,
                                          interseccion, n, jaccard, lift,
                                          columnas_texto,
                                          columnas_extra = character()) {
  conteos <- paste0(
    "`", columnas_texto, "`: ", n_ausentes_grupo,
    ifelse(n_ausentes_grupo == 1L, " ausente", " ausentes"),
    collapse = "; "
  )
  tope <- if (length(columnas_extra)) {
    paste0(
      " Hay ", length(columnas_extra),
      if (length(columnas_extra) == 1L) {
        " columna m\u00e1s que falta en esas mismas filas"
      } else {
        " columnas m\u00e1s que faltan en esas mismas filas"
      },
      "; el tope de ", .max_columnas_conjuntos, " no las examin\u00f3."
    )
  } else {
    ""
  }
  .nuevo_hallazgo(
    paste(nombres_grupo, collapse = ","),
    "faltantes_conjuntos", "ok",
    paste0(
      "La ausencia de estas columnas coincide en las mismas filas. Puede ser ",
      "un bloque que no se carg\u00f3 o columnas que s\u00f3lo se completan ",
      "juntas: la coincidencia describe la estructura de la ausencia y no ",
      "prueba un defecto."
    ),
    paste0(
      conteos, ". Faltan todas en ", interseccion,
      if (interseccion == 1L) " fila" else " filas",
      " (", .formatear_decimal_publicado(100 * interseccion / n, 1L),
      " % de ", n, "); coincidencia ",
      .formatear_decimal_publicado(jaccard, 2L),
      " y ", .formatear_decimal_publicado(lift, 2L),
      " veces lo esperado si las ausencias fueran independientes.", tope
    ),
    paste0(
      "Si la ausencia conjunta es por dise\u00f1o, declarar `aplicabilidad` o ",
      "`columnas_opcionales`; si es un bloque que no se carg\u00f3, completar ",
      "la carga antes de analizar."
    ),
    n, interseccion, "fila"
  )
}

.hallazgo_ausencia_dependiente <- function(columna, determinante, ajuste,
                                           columna_texto, determinante_texto) {
  if (identical(ajuste$tipo, "tope")) {
    grupo_alto <- paste0(
      "el m\u00e1ximo de `", determinante_texto, "` (",
      .texto_valores_nivel(ajuste$etiqueta_alta), ")"
    )
    grupo_bajo <- "el resto"
  } else {
    grupo_alto <- paste0(
      "`", determinante_texto, "` = ",
      .texto_valores_nivel(ajuste$etiqueta_alta)
    )
    grupo_bajo <- paste0(
      "`", determinante_texto, "` = ",
      .texto_valores_nivel(ajuste$etiqueta_baja)
    )
  }
  .nuevo_hallazgo(
    columna, "ausencia_dependiente", "ok",
    paste0(
      "La ausencia de esta columna no se reparte igual entre los valores de ",
      "otra: falta m\u00e1s en un grupo que en otro. Puede ser un bloque que no ",
      "se carg\u00f3 o una columna que s\u00f3lo se completa en ciertos grupos; ",
      "la asociaci\u00f3n describe la estructura de la ausencia y no prueba un ",
      "defecto."
    ),
    paste0(
      "En ", grupo_alto, " faltan ", ajuste$n_ausentes_alto, " de ",
      ajuste$n_alto, " (",
      .formatear_decimal_publicado(100 * ajuste$proporcion_alta, 1L),
      " %); en ", grupo_bajo, " faltan ", ajuste$n_ausentes_bajo, " de ",
      ajuste$n_bajo, " (",
      .formatear_decimal_publicado(100 * ajuste$proporcion_baja, 1L),
      " %). Diferencia: ",
      .formatear_decimal_publicado(100 * ajuste$diferencia, 1L),
      " puntos porcentuales; cociente de riesgos: ",
      if (is.finite(ajuste$razon)) {
        .formatear_decimal_publicado(ajuste$razon, 2L)
      } else {
        "infinito"
      },
      ". Medido sobre ", ajuste$n_evaluados,
      if (ajuste$n_evaluados == 1L) " fila con el determinante presente." else
        " filas con el determinante presente."
    ),
    paste0(
      "Revisar si el grupo con m\u00e1s ausencia es un universo distinto; si lo ",
      "es, declararlo con `aplicabilidad` o `columnas_opcionales`. Si la ",
      "ausencia depende del valor -por ejemplo, un registro que se corta al ",
      "llegar a un tope-, documentarlo antes de imputar o de comparar grupos."
    ),
    ajuste$n_evaluados, ajuste$n_afectados, "fila"
  )
}

.diagnosticar_patrones_ausencia <- function(datos, nombres, resultados,
                                            aplicabilidad_resuelta,
                                            explicadas = character(),
                                            explicadas_determinante = NULL,
                                            nombres_texto = NULL,
                                            marcas = NULL,
                                            fase = c("todo", "conjuntos",
                                                     "dependencia")) {
  fase <- match.arg(fase)
  hacer_conjuntos <- !identical(fase, "dependencia")
  hacer_dependencia <- !identical(fase, "conjuntos")
  vacio <- list(
    hallazgos = list(), cobertura = .cobertura_diagnosticos_vacia(),
    conjuntas_cubiertas = character()
  )
  n <- nrow(datos)
  if (!n || !length(nombres)) return(vacio)
  if (is.null(nombres_texto)) nombres_texto <- .marcar_utf8_textos(nombres)
  nombres_operativos <- .nombres_para_operar(nombres)
  declaradas <- .nombres_para_operar(aplicabilidad_resuelta$reglas$columna)
  explicadas <- unique(c(
    .nombres_para_operar(explicadas),
    if (is.data.frame(marcas) && nrow(marcas)) {
      .nombres_para_operar(marcas$marca[!is.na(marcas$columna)])
    } else {
      character()
    }
  ))
  # Una columna que participa de un bloque conjunto se saca como objetivo para
  # no repetir el bloque como dependencia. Como determinante no se saca: si su
  # ausencia esta contenida en la de otra, el bloque es una implicacion y la
  # dependencia del otro extremo sigue siendo el hallazgo que explica el hecho.
  if (is.null(explicadas_determinante)) {
    explicadas_determinante <- explicadas
  } else {
    explicadas_determinante <- unique(c(
      .nombres_para_operar(explicadas_determinante),
      if (is.data.frame(marcas) && nrow(marcas)) {
        .nombres_para_operar(marcas$marca[!is.na(marcas$columna)])
      } else {
        character()
      }
    ))
  }

  ausentes <- vector("list", length(nombres))
  no_atomicas <- character()
  for (i in seq_along(nombres)) {
    mascara <- .mascara_ausencia_columna(
      datos[[i]], resultados[[i]]$faltantes_disfrazados, n
    )
    if (is.null(mascara)) {
      no_atomicas <- c(no_atomicas, nombres[[i]])
    } else {
      ausentes[[i]] <- mascara
    }
  }

  hallazgos <- list()
  cobertura <- .cobertura_diagnosticos_vacia()

  # Las filas vacias se miden siempre: no dependen de un minimo de columnas ni
  # de que la tabla sea ancha. Una sola fila sin valores es un hecho, no una
  # tendencia.
  vacias <- if (hacer_conjuntos) {
    .filas_vacias_perfil(datos, n)
  } else {
    integer()
  }
  # En `perfilar_por()` la rebanada no tiene la columna de agrupacion: una fila
  # puede estar vacia dentro del grupo y tener el grupo en la tabla entera. La
  # tabla entera ya decidio que filas estan vacias; aca solo se conservan esas.
  if (hacer_conjuntos && length(vacias)) {
    del_contexto <- .CONTEXTO_POR$filas_vacias_tabla
    if (!is.null(del_contexto) && length(del_contexto) == n) {
      vacias <- vacias[del_contexto[vacias]]
    }
  }
  if (hacer_conjuntos && length(vacias)) {
    aviso <- .nuevo_hallazgo(
      NA_character_, "filas_vacias", "error",
      paste0(
        "La tabla tiene filas sin ning\u00fan valor. No aportan a ning\u00fan ",
        "an\u00e1lisis y, si son un artefacto de carga, conviene quitarlas del ",
        "origen antes de perfilar."
      ),
      paste0(
        length(vacias), if (length(vacias) == 1L) " fila vac\u00eda de " else
          " filas vac\u00edas de ", n,
        " (", .formatear_decimal_publicado(100 * length(vacias) / n, 1L),
        " %)."
      ),
      paste0(
        "Revisar el origen de esas filas y corregir la carga. El plan de ",
        "limpieza no propone eliminarlas: conservarlas o marcarlas es una ",
        "decisi\u00f3n de quien conoce el dato."
      ),
      n, length(vacias), "fila"
    )
    hallazgos[[length(hallazgos) + 1L]] <-
      .con_filas_de_ausencia(aviso, vacias)
  }

  n_ausentes <- vapply(
    ausentes, function(m) if (is.null(m)) NA_integer_ else sum(m), integer(1L)
  )
  # Candidata: tiene algo presente y algo ausente, no es casi siempre vacia y
  # no fue declarada ni explicada por otro diagnostico. Las columnas declaradas
  # no se examinan: el usuario ya dijo que su ausencia no es un defecto.
  candidata <- !is.na(n_ausentes) &
    n_ausentes >= .min_ausentes_conjuntos &
    n_ausentes >= n * .min_prop_ausencia_conjuntos &
    n_ausentes <= n * .max_prop_ausencia_conjuntos &
    !(nombres_operativos %in% c(declaradas, explicadas))
  indices <- which(candidata)

  if (hacer_conjuntos && n < .min_filas_ausencia_estructural &&
      length(indices)) {
    cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
      "faltantes_conjuntos", paste(nombres[indices], collapse = ", "),
      paste0(
        "No se buscaron faltantes conjuntos: la tabla tiene ", n,
        if (n == 1L) " fila y se necesitan al menos " else
          " filas y se necesitan al menos ", .min_filas_ausencia_estructural,
        " para que una coincidencia de ausencias signifique algo."
      ),
      "Perfilar una tabla con m\u00e1s filas si interesa esta se\u00f1al.",
      columnas = nombres[indices]
    ))
  }
  if (hacer_dependencia && n < .min_filas_ausencia_estructural &&
      length(indices)) {
    cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
      "ausencia_dependiente", paste(nombres[indices], collapse = ", "),
      paste0(
        "No se busc\u00f3 dependencia de la ausencia: la tabla tiene ", n,
        if (n == 1L) " fila y se necesitan al menos " else
          " filas y se necesitan al menos ", .min_filas_ausencia_estructural,
        " para comparar grupos."
      ),
      "Perfilar una tabla con m\u00e1s filas si interesa esta se\u00f1al.",
      columnas = nombres[indices]
    ))
  }

  if (hacer_conjuntos && length(no_atomicas) && length(indices)) {
    cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
      "faltantes_conjuntos", paste(no_atomicas, collapse = ", "),
      paste0(
        "No se buscaron faltantes conjuntos en ", length(no_atomicas),
        if (length(no_atomicas) == 1L) {
          " columna que no es un vector at\u00f3mico: la coincidencia se mide celda a celda."
        } else {
          " columnas que no son vectores at\u00f3micos: la coincidencia se mide celda a celda."
        }
      ),
      "No corresponde resolverlo; queda declarado."
    ))
  }

  if (n < .min_filas_ausencia_estructural || !length(indices)) {
    return(list(
      hallazgos = hallazgos, cobertura = cobertura,
      conjuntas_cubiertas = character()
    ))
  }

  indices_extra <- integer()
  if (hacer_conjuntos && length(indices) > .max_columnas_conjuntos) {
    indices_extra <- indices[-seq_len(.max_columnas_conjuntos)]
    recortadas <- nombres[indices_extra]
    indices <- indices[seq_len(.max_columnas_conjuntos)]
    cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
      "faltantes_conjuntos", paste(recortadas, collapse = ", "),
      paste0(
        "No se buscaron faltantes conjuntos en ", length(recortadas),
        if (length(recortadas) == 1L) {
          " columna: la tabla de patrones se detiene en "
        } else {
          " columnas: la tabla de patrones se detiene en "
        },
        .max_columnas_conjuntos,
        " para acotar el costo, y pueden compartir el patr\u00f3n de las examinadas."
      ),
      "Perfilar por subconjuntos de columnas si interesa examinarlas todas."
    ))
  }

  # --- Faltantes conjuntos -------------------------------------------------

  columnas_conjuntas <- character()
  cobertura_conjunta <- vector("list", length(nombres))
  if (hacer_conjuntos) {
    patrones <- .patrones_ausencia(ausentes[indices])
    if (!is.null(patrones)) {
      umbral_filas <- max(
        .min_filas_patron_conjuntos,
        ceiling(n * .min_prop_patron_conjuntos)
      )
      publicables <- which(patrones$cuentas >= umbral_filas)
      if (length(publicables)) {
        filas_por_patron <- split(seq_len(n), patrones$codigos)
        es_vacia <- logical(n)
        if (length(vacias)) es_vacia[vacias] <- TRUE
        for (g in publicables) {
          columnas_grupo <- which(patrones$matriz[patrones$primeras[[g]], ])
          if (length(columnas_grupo) < 2L) next
          filas_patron <- filas_por_patron[[g]]
          interseccion <- patrones$cuentas[[g]]
          # Si el patron es exactamente el conjunto de filas vacias, ya lo
          # cuenta `filas_vacias`: publicar ademas un grupo de columnas que
          # faltan juntas por esa causa repetiria el mismo hecho con otra forma.
          if (all(es_vacia[filas_patron])) next
          indices_grupo <- indices[columnas_grupo]
          mascaras <- ausentes[indices_grupo]
          union <- sum(Reduce(`|`, mascaras))
          jaccard <- if (union > 0L) interseccion / union else 0
          n_ausentes_grupo <- n_ausentes[indices_grupo]
          log_esperada <- sum(log(n_ausentes_grupo)) -
            (length(columnas_grupo) - 1L) * log(n)
          esperada <- exp(log_esperada)
          lift <- if (is.finite(esperada) && esperada > 0) {
            interseccion / esperada
          } else {
            Inf
          }
          # La coincidencia de a pares no alcanza para distinguir un bloque
          # comun de una casualidad; el `lift` si: mide el bloque contra lo que
          # produciria la independencia entre las mismas columnas.
          if (lift < .min_lift_conjuntos) next
          columnas_extra <- character()
          if (length(indices_extra)) {
            comparten <- vapply(
              indices_extra,
              function(j) all(ausentes[[j]][filas_patron]),
              logical(1L)
            )
            columnas_extra <- nombres[indices_extra[comparten]]
          }
          nombres_grupo <- nombres[indices_grupo]
          columnas_conjuntas <- c(columnas_conjuntas, nombres_grupo)
          filas_logicas <- logical(n)
          filas_logicas[filas_patron] <- TRUE
          for (jj in indices_grupo) {
            previo <- cobertura_conjunta[[jj]]
            cobertura_conjunta[[jj]] <- if (is.null(previo)) {
              filas_logicas
            } else {
              previo | filas_logicas
            }
          }
          hallazgos[[length(hallazgos) + 1L]] <- .con_filas_de_ausencia(
            .hallazgo_faltantes_conjuntos(
              nombres_grupo, n_ausentes_grupo, interseccion, n,
              jaccard, lift, nombres_texto[indices_grupo], columnas_extra
            ),
            filas_patron,
            columnas = nombres_grupo
          )
        }
      }
    }
  }

  # Una columna queda cubierta por los bloques conjuntos cuando todas sus
  # filas ausentes estan en alguno de ellos. Solo esas se sacan como objetivo de
  # la dependencia: si el bloque explica solo una parte de su ausencia, la
  # dependencia sigue midiendo el resto.
  conjuntas_cubiertas <- character()
  if (hacer_conjuntos && length(columnas_conjuntas)) {
    cubierta <- vapply(seq_along(nombres), function(j) {
      cob <- cobertura_conjunta[[j]]
      ausente <- ausentes[[j]]
      !is.null(cob) && !is.null(ausente) && any(ausente) && all(cob[ausente])
    }, logical(1L))
    conjuntas_cubiertas <- nombres[cubierta]
  }

  if (!hacer_dependencia) {
    return(list(
      hallazgos = hallazgos, cobertura = cobertura,
      conjuntas_cubiertas = conjuntas_cubiertas
    ))
  }

  # --- Ausencia dependiente de otra columna --------------------------------

  excluidas_objetivo <- unique(c(
    .nombres_para_operar(explicadas),
    .nombres_para_operar(conjuntas_cubiertas)
  ))
  excluidas_determinante <- .nombres_para_operar(explicadas_determinante)
  indices_dependencia <- indices[
    !(nombres_operativos[indices] %in% excluidas_objetivo)
  ]
  if (length(indices_dependencia) > .max_columnas_dependiente) {
    recortadas <- nombres[
      indices_dependencia[-seq_len(.max_columnas_dependiente)]
    ]
    indices_dependencia <- indices_dependencia[
      seq_len(.max_columnas_dependiente)
    ]
    cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
      "ausencia_dependiente", paste(recortadas, collapse = ", "),
      paste0(
        "No se busc\u00f3 dependencia de la ausencia en ", length(recortadas),
        if (length(recortadas) == 1L) {
          " columna: el examen es de a pares y se detiene en "
        } else {
          " columnas: el examen es de a pares y se detiene en "
        },
        .max_columnas_dependiente,
        " para acotar el costo."
      ),
      "Perfilar por subconjuntos de columnas si interesa examinarlas todas."
    ))
  }

  determinantes <- list()
  for (j in seq_along(nombres)) {
    if (length(determinantes) >= .max_determinantes_dependiente) {
      cobertura <- rbind(cobertura, .cobertura_patron_ausencia(
        "ausencia_dependiente", NA_character_,
        paste0(
          "Se examinaron ", .max_determinantes_dependiente,
          if (.max_determinantes_dependiente == 1L) {
            " columna como posible determinante de la ausencia y el resto no se prob\u00f3."
          } else {
            " columnas como posibles determinantes de la ausencia y el resto no se prob\u00f3."
          }
        ),
        "Perfilar por subconjuntos de columnas si interesa probarlas todas."
      ))
      break
    }
    preparado <- .preparar_dependencia_ausencia(
      datos[[j]], nombres[[j]], n, resultados[[j]]
    )
    if (!is.null(preparado)) {
      preparado$indice <- j
      determinantes[[length(determinantes) + 1L]] <- preparado
    }
  }

  for (objetivo in indices_dependencia) {
    ausente_objetivo <- ausentes[[objetivo]]
    mejor <- NULL
    for (det in determinantes) {
      if (det$indice == objetivo) next
      if (nombres_operativos[[det$indice]] %in% excluidas_determinante) next
      for (candidato in det$candidatos) {
        n_ausentes_grupo <- tabulate(
          candidato$codigos[ausente_objetivo[det$usable]],
          length(candidato$n_por)
        )
        proporciones <- n_ausentes_grupo / candidato$n_por
        alto <- which.max(proporciones)
        bajo <- which.min(proporciones)
        if (alto == bajo) next
        diferencia <- proporciones[[alto]] - proporciones[[bajo]]
        if (diferencia < .min_diferencia_ausencia_dependiente) next
        if (n_ausentes_grupo[[alto]] < .min_ausentes_grupo_dependiente) next
        if (candidato$n_por[[alto]] < .min_grupo_ausencia_dependiente ||
            candidato$n_por[[bajo]] < .min_grupo_ausencia_dependiente) {
          next
        }
        razon <- if (proporciones[[bajo]] > 0) {
          proporciones[[alto]] / proporciones[[bajo]]
        } else {
          Inf
        }
                  ajuste <- list(
                    tipo = candidato$tipo,
                    etiqueta_alta = candidato$etiquetas[[alto]],
                    etiqueta_baja = candidato$etiquetas[[bajo]],
                    n_alto = candidato$n_por[[alto]],
                    n_bajo = candidato$n_por[[bajo]],
                    n_ausentes_alto = n_ausentes_grupo[[alto]],
                    n_ausentes_bajo = n_ausentes_grupo[[bajo]],
                    proporcion_alta = proporciones[[alto]],
                    proporcion_baja = proporciones[[bajo]],
                    diferencia = diferencia,
                    razon = razon,
                    n_grupos = length(candidato$n_por),
                    n_evaluados = det$n_usables,
                    n_afectados = sum(n_ausentes_grupo),
                    detalle = det
                  )
                  if (is.null(mejor) || diferencia > mejor$diferencia ||
                      (diferencia == mejor$diferencia &&
                         ajuste$n_grupos < mejor$n_grupos)) {
                    mejor <- ajuste
                  }
      }
    }
    if (is.null(mejor)) next
    filas <- which(ausente_objetivo & mejor$detalle$usable)
    hallazgos[[length(hallazgos) + 1L]] <- .con_filas_de_ausencia(
      .hallazgo_ausencia_dependiente(
        nombres[[objetivo]], mejor$detalle$nombre, mejor,
        nombres_texto[[objetivo]],
        nombres_texto[[mejor$detalle$indice]]
      ),
      filas
    )
  }

  list(
    hallazgos = hallazgos, cobertura = cobertura,
    conjuntas_cubiertas = conjuntas_cubiertas
  )
}
