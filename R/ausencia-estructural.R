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
.mascara_ausencia_columna <- function(x, disfrazados, n) {
  if (is.data.frame(x) || is.matrix(x)) return(NULL)
  if (is.list(x)) {
    return(vapply(
      x,
      function(v) {
        length(v) == 0L || is.null(v) || (length(v) == 1L && all(is.na(v)))
      },
      logical(1L)
    ))
  }
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

.texto_corte_umbral <- function(ajuste) {
  clase <- ajuste$clase_umbral
  if (!is.null(clase) && clase %in% c("Date", "POSIXct", "POSIXt") &&
      !is.na(ajuste$etiqueta_corte)) {
    envoltorio <- if (identical(clase, "Date")) "as.Date" else "as.POSIXct"
    return(paste0(envoltorio, "(\"", ajuste$etiqueta_corte, "\")"))
  }
  texto <- trimws(format(
    ajuste$corte, digits = 15, scientific = FALSE, trim = TRUE
  ))
  decimal <- Sys.localeconv()[["decimal_point"]]
  if (length(decimal) && !is.na(decimal) && nzchar(decimal) &&
      !identical(decimal, ".")) {
    texto <- sub(decimal, ".", texto, fixed = TRUE)
  }
  texto
}

.texto_valores_nivel <- function(valores) {
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
  presentes_abajo <- acumulado[cortes]
  aciertos_mayor <- (cortes - presentes_abajo) +
    (total_presentes - presentes_abajo)
  aciertos_menor <- presentes_abajo + ((n - cortes) -
    (total_presentes - presentes_abajo))
  mejor_mayor <- max(aciertos_mayor)
  mejor_menor <- max(aciertos_menor)
  if (mejor_mayor >= mejor_menor) {
    aciertos <- mejor_mayor
    indice <- cortes[[which.max(aciertos_mayor)]]
    sentido <- ">="
  } else {
    aciertos <- mejor_menor
    indice <- cortes[[which.max(aciertos_menor)]]
    sentido <- "<"
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
    etiqueta_corte = if (!is.null(det$etiquetas_ordenadas)) {
      det$etiquetas_ordenadas[[indice + 1L]]
    } else NA_character_,
    sentido = sentido,
    n_indeterminados = det$n_indeterminados
  )
}

# Un determinante se prepara una sola vez y se contrasta contra todas las
# columnas candidatas. Construir el factor dentro del par lo reconstruye una vez
# por comparacion, y con doscientas columnas eso es la diferencia entre un
# segundo y cuarenta.
.preparar_determinante <- function(x, nombre, n) {
  if (is.data.frame(x) || is.matrix(x) || is.list(x)) return(NULL)
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
      orden <- order(valores)
      ordenados <- valores[orden]
      cambios <- which(ordenados[-length(ordenados)] != ordenados[-1L])
      if (length(cambios) > .max_cortes_umbral) {
        cambios <- cambios[unique(round(seq(
          1, length(cambios), length.out = .max_cortes_umbral
        )))]
      }
      # La clase viaja con el corte: `as.numeric()` sobre una fecha da los dias
      # desde 1970, y una sugerencia que diga `>= 18628` no se puede pegar.
      umbral <- list(
        orden = orden, valores_ordenados = ordenados, cortes = cambios,
        clase_umbral = class(x)[[1L]],
        etiquetas_ordenadas = as.character(x[usable])[orden]
      )
    }
  }
  # Un vistazo barato antes de construir el factor: si los primeros valores ya
  # traen mas niveles de los admitidos, no hace falta mirar el resto.
  asomo <- unique(x[usable][seq_len(min(n_usables, 5000L))])
  if (length(asomo) > .max_niveles_determinante) {
    if (is.null(umbral)) return(NULL)
    return(c(list(
      nombre = nombre, usable = usable, n_usables = n_usables,
      codigos = integer(), etiquetas = character(), n_niveles = NA_integer_,
      n_por_nivel = integer(), n_indeterminados = n - n_usables,
      solo_umbral = TRUE
    ), umbral))
  }
  niveles <- factor(as.character(x[usable]))
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
    n_indeterminados = det$n_indeterminados
  )
}

.hallazgo_ausencia_determinada <- function(columna, determinante, ajuste,
                                           n_ausentes) {
  por_umbral <- !is.null(ajuste$corte)
  formula <- if (por_umbral) {
    paste0("~ ", determinante, " ", ajuste$sentido, " ",
           .texto_corte_umbral(ajuste))
  } else {
    paste0(
      "~ ", determinante,
      if (length(ajuste$niveles_aplica) == 1L) " == " else " %in% ",
      .texto_valores_nivel(ajuste$niveles_aplica)
    )
  }
  explicados <- round(ajuste$cumplimiento * n_ausentes)
  .nuevo_hallazgo(
    columna, "posible_ausencia_estructural", "ok",
    paste0(
      "La ausencia de esta columna parece por dise\u00f1o y no por error: qu\u00e9 ",
      "filas la tienen queda decidido por el valor de otra columna. Es una ",
      "sospecha medida, no una conclusi\u00f3n; `lupa` no cambia el universo por ",
      "su cuenta."
    ),
    paste0(
      "`", determinante, "` predice la presencia de `", columna,
      "` en ", sprintf("%.1f", 100 * ajuste$cumplimiento), " % de ",
      ajuste$n_usables, " filas",
      if (por_umbral) {
        ", por un umbral"
      } else {
        paste0(", con ", ajuste$n_niveles, " valores distintos")
      },
      if (ajuste$n_indeterminados > 0L) {
        paste0(" y ", ajuste$n_indeterminados,
               " filas donde el determinante falta")
      } else {
        ""
      },
      ". La columna corresponde cuando ", determinante,
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
      "`perfilar(datos, aplicabilidad = list(", columna, " = ", formula,
      "))`. Con la regla declarada, la ausencia fuera de ese universo deja de ",
      "contarse como defecto y el alcance queda escrito en ",
      "`cobertura_diagnosticos`."
    ),
    n_ausentes, explicados, "fila"
  )
}

.hallazgo_ausencia_excluyente <- function(columna, grupo, solapamiento,
                                          cobertura, n, n_ausentes) {
  otras <- setdiff(grupo, columna)
  .nuevo_hallazgo(
    columna, "posible_ausencia_estructural", "ok",
    paste0(
      "La ausencia de esta columna parece por dise\u00f1o y no por error: se ",
      "reparte las filas con otras columnas sin pisarse, que es la forma de ",
      "una tabla con atributos excluyentes. Es una sospecha medida, no una ",
      "conclusi\u00f3n."
    ),
    paste0(
      "`", columna, "` no coincide con ", paste0("`", otras, "`", collapse = ", "),
      ": entre las ", length(grupo), " columnas cubren ",
      sprintf("%.1f", 100 * cobertura), " % de las ", n, " filas y se pisan en ",
      solapamiento, if (solapamiento == 1L) " fila." else " filas."
    ),
    paste0(
      "Si hay una columna que dice cu\u00e1l de las ", length(grupo),
      " corresponde en cada fila, declararla con `aplicabilidad`; si la ",
      "ausencia es por dise\u00f1o y no depende de ninguna otra columna, ",
      "`columnas_opcionales = c(",
      paste0("\"", grupo, "\"", collapse = ", "), ")`."
    ),
    n_ausentes, n_ausentes, "fila"
  )
}

# Un aviso, no un defecto: la regla declarada esta haciendo su trabajo, y por
# eso mismo el perfil sale limpio. Quien no lea `cobertura_diagnosticos` no se
# entera de que una columna casi vacia dejo de contar. El aviso lo pone en la
# misma tabla que todo lo demas.
.hallazgos_regla_silenciosa <- function(nombres, resultados, reglas, umbral) {
  if (!nrow(reglas)) return(list())
  salida <- list()
  for (k in seq_len(nrow(reglas))) {
    i <- match(reglas$columna[[k]], nombres)
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
    salida[[length(salida) + 1L]] <- .nuevo_hallazgo(
      reglas$columna[[k]], "regla_silencia_ausencia", "ok",
      paste0(
        "La declaraci\u00f3n del universo evit\u00f3 que esta columna apareciera como ",
        "incompleta, y dentro del universo declarado sigue faltando la mayor ",
        "parte. Puede estar bien; el aviso existe para que la decisi\u00f3n sea ",
        "expl\u00edcita y no un efecto de la declaraci\u00f3n."
      ),
      paste0(
        "Faltan ", n_ausentes, " de ", n_aplicables,
        if (identical(origen, "columnas_opcionales")) {
          " celdas de la columna ("
        } else {
          " celdas del universo declarado ("
        },
        sprintf("%.1f", 100 * proporcion), " %), y la columna se declar\u00f3 con `",
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
  columnas <- vapply(senales, function(h) as.character(h$columna[[1L]]),
                     character(1L))
  tipos <- vapply(senales, function(h) as.character(h$tipo_hallazgo[[1L]]),
                  character(1L))
  columnas <- unique(columnas[tipos == "posible_ausencia_estructural"])
  if (!length(columnas)) return(hallazgos)
  objetivo <- hallazgos$tipo_hallazgo == "faltantes" &
    as.character(hallazgos$columna) %in% columnas
  if (!any(objetivo)) return(hallazgos)
  hallazgos$evidencia[objetivo] <- paste0(
    sub("[.[:space:]]*$", "", hallazgos$evidencia[objetivo]),
    ". Hay adem\u00e1s un hallazgo `posible_ausencia_estructural` sobre esta ",
    "columna: parte de esta ausencia podr\u00eda ser por dise\u00f1o. La ",
    "severidad no se baja por esa sospecha; leer las dos y decidir."
  )
  hallazgos
}

.cobertura_ausencia_estructural <- function(columna, motivo, como) {
  data.frame(
    diagnostico = "posible_ausencia_estructural",
    columna = columna, motivo = motivo, como_resolverlo = como,
    dependencia = NA_character_, stringsAsFactors = FALSE
  )
}

.diagnosticar_ausencia_estructural <- function(datos, nombres, resultados,
                                               aplicabilidad_resuelta,
                                               umbral_faltantes_error) {
  vacio <- list(
    hallazgos = list(), cobertura = .cobertura_diagnosticos_vacia()
  )
  avisos <- .hallazgos_regla_silenciosa(
    nombres, resultados, aplicabilidad_resuelta$reglas, umbral_faltantes_error
  )
  vacio$hallazgos <- avisos
  n <- nrow(datos)
  if (!length(nombres)) return(vacio)
  pocas_filas <- n < .min_filas_ausencia_estructural

  declaradas <- aplicabilidad_resuelta$reglas$columna
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
    !(nombres %in% declaradas)
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
          " filas y se necesitan al menos ", .min_filas_ausencia_estructural,
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
    mejor_nombre <- NA_character_
    for (det in determinantes) {
      if (det$indice == indices[[k]]) next
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
        mejor_nombre <- det$nombre
      }
    }
    if (!is.null(mejor)) {
      hallazgos[[length(hallazgos) + 1L]] <- .hallazgo_ausencia_determinada(
        columna, mejor_nombre, mejor, n_ausentes[[indices[[k]]]]
      )
      explicadas <- c(explicadas, columna)
    }
  }

  # Despues el reparto entre columnas, para las que quedaron sin determinante.
  restantes <- which(!(nombres[indices] %in% explicadas))
  if (length(restantes) >= 2L) {
    huellas <- vapply(restantes, function(k) .huella_mascara(presentes[[k]]),
                      character(1L))
    orden <- restantes[order(
      vapply(restantes, function(k) sum(presentes[[k]]), integer(1L)),
      decreasing = TRUE
    )]
    tope_solape <- floor(.max_solapamiento_ausencia * n)
    usadas <- character()
    for (semilla in orden) {
      if (nombres[indices[[semilla]]] %in% usadas) next
      grupo <- semilla
      union <- presentes[[semilla]]
      solapamiento <- 0L
      for (otro in orden) {
        if (otro %in% grupo) next
        if (nombres[indices[[otro]]] %in% usadas) next
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
      usadas <- c(usadas, nombres_grupo)
      for (k in grupo) {
        hallazgos[[length(hallazgos) + 1L]] <- .hallazgo_ausencia_excluyente(
          nombres[[indices[[k]]]], nombres_grupo, solapamiento, cubierto, n,
          n_ausentes[[indices[[k]]]]
        )
      }
    }
  }

  list(hallazgos = c(avisos, hallazgos), cobertura = cobertura)
}
