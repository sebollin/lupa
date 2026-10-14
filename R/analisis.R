.version_esquema_analisis <- 1L

.advertencias_analisis <- function(distribuciones, asociaciones, temporal,
                                   variables, propuesta, perfil = NULL) {
  filas <- list()
  agregar <- function(componente, tipo, descripcion, severidad = "sospechoso") {
    filas[[length(filas) + 1L]] <<- data.frame(
      componente = componente, tipo = tipo, severidad = severidad,
      descripcion = descripcion, stringsAsFactors = FALSE
    )
  }
  if (any(distribuciones$alcance$muestreado)) agregar(
    "distribuciones", "muestreo",
    "Al menos una distribucion o sus cuantiles se estimaron sobre una muestra."
  )
  if (any(distribuciones$alcance$truncado)) agregar(
    "distribuciones", "truncamiento",
    "Las tablas de frecuencia muestran solo los valores mas frecuentes."
  )
  if (isTRUE(attr(asociaciones, "muestreado", exact = TRUE))) agregar(
    "asociaciones", "muestreo",
    "Las asociaciones se calcularon sobre una muestra comun de filas."
  )
  if (length(attr(asociaciones, "columnas_omitidas_limite", exact = TRUE))) agregar(
    "asociaciones", "columnas_omitidas",
    "Algunas columnas analizables excedieron el limite configurado."
  )
  if (isTRUE(attr(asociaciones, "truncado", exact = TRUE))) agregar(
    "asociaciones", "truncamiento",
    "La salida de asociaciones se recorto despues de ordenarla."
  )
  if (nrow(temporal$propuestas) && any(!temporal$propuestas$confirmada)) agregar(
    "tiempo", "frecuencia_no_confirmada",
    "Las frecuencias temporales son propuestas observadas, no requisitos confirmados."
  )
  if (any(temporal$resumen$huecos_truncados)) agregar(
    "tiempo", "huecos_truncados",
    "Al menos una serie tiene mas grupos de huecos que los mostrados."
  )
  if (isTRUE(attr(temporal, "truncado", exact = TRUE))) agregar(
    "tiempo", "columnas_omitidas",
    "El analisis temporal alcanzo el maximo de columnas configurado."
  )
  if (nrow(variables) && any(!variables$confirmada)) agregar(
    "variables", "escalas_no_confirmadas",
    "Algunas escalas se propusieron desde los valores y requieren confirmacion."
  )
  if (nrow(variables) && any(variables$n_niveles_ausentes > 0L)) agregar(
    "variables", "niveles_ausentes",
    "Hay niveles declarados que no aparecen en la entrega."
  )
  if (isTRUE(attr(propuesta, "truncado", exact = TRUE))) agregar(
    "modelo", "propuesta_truncada",
    "La propuesta de modelo contiene menos filas que el total detectado."
  )
  if (!is.null(perfil) && inherits(perfil, "perfil") &&
      nrow(perfil$hallazgos)) {
    casi_claves <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
    ]
    if (nrow(casi_claves)) {
      for (i in seq_len(nrow(casi_claves))) agregar(
        "perfil", "casi_clave",
        paste0(
          "La columna '", casi_claves$columna[[i]],
          "' tiene colisiones concentradas y no es una clave valida. ",
          casi_claves$evidencia[[i]]
        )
      )
    }
  }
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    componente = character(), tipo = character(), severidad = character(),
    descripcion = character(), stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  rownames(resultado) <- NULL
  resultado
}

.decision_medicion_propuesta <- function(propuesta, seleccion, confirmada,
                                         medicion_automatica) {
  if (!nrow(propuesta)) {
    return(data.frame(
      metrica = character(), objeto = character(), estado = character(),
      medida = logical(), motivo = character(), justificacion = character(),
      confirmada = logical(), stringsAsFactors = FALSE
    ))
  }
  medida <- seq_len(nrow(propuesta)) %in% seleccion
  motivo <- ifelse(
    medida,
    if (medicion_automatica) {
      "lupa la eligi\u00f3 porque la propuesta qued\u00f3 en estado 'lista'."
    } else {
      "La propuesta confirmada incluy\u00f3 esta m\u00e9trica."
    },
    if (!medicion_automatica && !confirmada) {
      "La medici\u00f3n autom\u00e1tica fue desactivada."
    } else ifelse(
      propuesta$estado != "lista",
      paste0("Qued\u00f3 afuera por su estado: ", propuesta$estado, "."),
      "Qued\u00f3 afuera de la selecci\u00f3n confirmada."
    )
  )
  data.frame(
    metrica = propuesta$metrica,
    objeto = ifelse(
      nzchar(propuesta$atributos), propuesta$atributos, "(tabla)"
    ),
    estado = as.character(propuesta$estado),
    medida = medida,
    motivo = motivo,
    justificacion = propuesta$justificacion,
    confirmada = rep(confirmada, nrow(propuesta)),
    stringsAsFactors = FALSE
  )
}

.decision_medicion_modelo <- function(modelo_elegido) {
  if (is.null(modelo_elegido)) return(NULL)
  filas <- lapply(modelo_elegido$metricas, function(x) {
    data.frame(
      metrica = x$declaracion$nombre,
      objeto = if (length(x$atributos)) {
        .pegar_nombres(x$atributos)
      } else {
        "(tabla)"
      },
      estado = "confirmada", medida = TRUE,
      motivo = "La m\u00e9trica pertenece al modelo confirmado.",
      justificacion = "Modelo declarado por quien realiza el an\u00e1lisis.",
      confirmada = TRUE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

.marco_modelo_analisis <- function(modelo_elegido) {
  if (is.null(modelo_elegido)) return(marco_agesic())
  if (inherits(modelo_elegido$marco, "marco_calidad")) {
    return(modelo_elegido$marco)
  }
  declaraciones <- lapply(modelo_elegido$metricas, `[[`, "declaracion")
  factores <- unique(data.frame(
    dimension = vapply(declaraciones, `[[`, character(1L), "dimension"),
    factor = vapply(declaraciones, `[[`, character(1L), "factor"),
    stringsAsFactors = FALSE
  ))
  if (anyNA(factores) || any(!nzchar(factores$dimension)) ||
      any(!nzchar(factores$factor))) {
    return(marco_agesic())
  }
  agesic <- marco_agesic()
  claves <- .clave_par_identificador(
    factores$dimension, factores$factor, sep = "\r"
  )
  claves_agesic <- .clave_par_identificador(
    agesic$factores$dimension, agesic$factores$factor, sep = "\r"
  )
  if (all(.identificadores_en(claves, claves_agesic))) agesic else {
    marco_calidad("Marco del modelo confirmado", factores)
  }
}

#' Ejecutar el análisis descriptivo completo
#'
#' Es la puerta de entrada al recorrido de `lupa`. Construye el perfil, las
#' distribuciones, asociaciones, diagnóstico temporal, clasificación confirmable
#' de variables, propuesta de modelo, medición agregada, tablero, cobertura
#' conceptual y plan de limpieza. No modifica los datos.
#'
#' Por omisión mide todas las sugerencias de la propuesta cuyo estado es
#' `"lista"`, aunque no estuvieran activas, y declara que esa selección fue
#' realizada por `lupa` sin confirmación. Agrega inmediatamente el detalle y
#' conserva [tablero_calidad()]; las medidas fila a fila sólo quedan en el
#' resultado si `conservar_detalle_medicion = TRUE`. La evaluación, cuando se
#' solicita, usa la medición agregada.
#' Los hallazgos `casi_clave` del perfil se reiteran en `advertencias`, con su
#' columna, valores en colisión, frecuencias y criterio observado, para que no
#' queden ocultos dentro del recorrido integral.
#'
#' @param datos Tabla que se desea analizar.
#' @param nombre Nombre de la entrega.
#' @param fecha Fecha y hora reproducible de la corrida. Se lee una vez, como
#'   en [perfilar()] y [medir()]: en UTC, también un texto como `"2026-03-01"`.
#' @param argumentos_perfil Lista de argumentos adicionales para [perfilar()].
#' @param metadatos_variables Declaraciones para [clasificar_variables()].
#' @param modelo_confirmado Modelo creado por [modelo()] o `NULL`.
#' @param propuesta_confirmada Propuesta editada por el usuario o `NULL`.
#' @param marco Taxonomía opcional creada por [marco_calidad()]. Si se omite,
#'   usa la asociada a `modelo_confirmado` y, en último término,
#'   [marco_agesic()].
#' @param perfil_evaluacion Perfil explícito para [evaluar()] o `NULL`.
#' @param id_medicion Identificador opcional enviado a [medir()]. Si
#'   `argumentos_perfil` declara `aplicabilidad`, esa misma declaración viaja a
#'   la medición: el universo se declara una vez y gobierna todo el análisis,
#'   así que el perfil y el tablero cuentan el mismo dato de la misma manera.
#'   `distribuciones` recibe esa declaración y la de `sentinelas_numericos`: lo
#'   que queda fuera del universo no entra en sus frecuencias ni en sus
#'   cuantiles, y un centinela declarado sale de los cuantiles como sale de
#'   `minimo` y `maximo` del perfil.
#' @param medir_propuesta Si se mide automáticamente la propuesta en estado
#'   `"lista"` cuando no se recibe un modelo o una propuesta confirmada. Use
#'   `FALSE` para conservar el comportamiento descriptivo anterior.
#' @param conservar_detalle_medicion Si se retienen las medidas fila a fila.
#'   Es `FALSE` por omisión: el tablero y la medición agregada permanecen, y la
#'   medición se hace métrica por métrica, agregando cada una antes de pasar a
#'   la siguiente, así que el detalle entero no existe nunca a la vez. Con
#'   `TRUE` el detalle es parte del resultado y su memoria crece con las filas
#'   por las métricas: una tabla de 19 columnas producía 21 filas de medida por
#'   fila de datos. Si ninguna métrica del modelo produce medidas, el tablero
#'   sale vacío y lleva el motivo de cada una en `cobertura_metricas`, en lugar
#'   de abortar el análisis.
#' @param muestra Límite de filas para perfil, distribuciones y enumeración de
#'   niveles observados.
#' @param muestra_asociacion Límite común de filas para asociaciones.
#' @param max_valores Máximo de valores por tabla de frecuencias.
#' @param probabilidades Cuantiles solicitados.
#' @param umbral_asociacion Asociación mínima informada.
#' @param max_columnas_asociacion Máximo de columnas para asociaciones.
#' @param max_niveles_asociacion Máximo de niveles categóricos.
#' @param max_pares_asociacion Máximo de pares devueltos.
#' @param metodo_asociacion_numerica Medida entre columnas numéricas que usa
#'   [detectar_asociaciones()]: `"pearson"` por omisión, o `"spearman"`
#'   para asociación monótona sobre los rangos, que no supone linealidad.
#' @param calendario Días ISO usados por el análisis temporal.
#' @param frecuencia_dias Frecuencia temporal conocida o `NULL` para proponerla.
#' @param max_huecos Máximo de grupos de huecos por columna.
#' @param max_columnas_temporales Máximo de columnas temporales analizadas.
#' @param conservar_datos Si el objeto retiene una copia de la entrada. Es
#'   `FALSE` por omisión para limitar tamaño y exposición. Con protección activa,
#'   las columnas personales de esa copia también se enmascaran, y en las demás
#'   columnas se tapa el valor protegido que identifica —el piso de
#'   [perfilar()]—: la cédula citada en una observación, `"pago CI 3.738.538-0"`,
#'   o guardada como número en otra columna. En todo lugar donde un
#'   `data.frame` guarda un valor: las celdas de cualquier clase —una copia
#'   `POSIXct` de la fecha con hora protegida queda en `NA`, como la tapa
#'   [perfilar()]—, las columnas de lista elemento por elemento, los atributos
#'   de la tabla y de cada columna —también `levels` en lo que no es un factor
#'   y `units`, y el nombre de cada atributo, además de su valor—, los nombres
#'   de columna de una tabla anidada en una columna
#'   lista, y los nombres de fila, que vuelven a ser los
#'   números de fila si alguno lleva un valor protegido. Una columna `POSIXlt`
#'   se tapa como la `POSIXct`, con sus atributos —`comment`, `label`—, y
#'   cualquier lista con clase —un registro de
#'   `vctrs`, un `blob`— por sus valores; un BLOB (`raw`) se lee como el texto
#'   que guarda y, si lleva un valor protegido, la copia guarda los bytes del
#'   texto tapado; un entorno no se modifica —es del usuario—: si guarda un
#'   valor protegido, en su contenido o en sus atributos —valor y nombre,
#'   también `name`, que `print()` muestra—, la copia lleva
#'   `"[valor protegido]"` en su lugar, también si tiene un atributo `name`; y
#'   también si no se puede leer entero —una promesa o una ligadura activa que
#'   aborta al leerse—, porque lo que no se lee no se sabe limpio; sólo el
#'   global, el de un paquete y los de la ruta de búsqueda se dejan como están. Una llamada, una fórmula o una
#'   función se leen como el texto que `print()` escribe —con sus atributos y,
#'   si guarda su fuente (`srcref`), la fuente con sus comentarios—, y también
#'   por sus atributos y, la función, por su entorno; si algo de eso lleva un
#'   valor protegido, la copia lleva `"[valor protegido]"` en su lugar. Una
#'   columna protegida
#'   conserva sólo los atributos de su clase (`class`, `tzone`, `units`, `dim`),
#'   y `tzone` y `units` pasan por el piso:
#'   las etiquetas de valor de un `haven_labelled` se quitan. Para conservar
#'   sus valores debe usarse `proteger_datos_personales = FALSE` al crear el
#'   analisis y al guardarlo. La opcion del guardado no recupera valores que el
#'   analisis ya enmascaro.
#' @param proteger_datos_personales Si perfiles y resúmenes ocultan valores de
#'   columnas cuya clasificación activa protección automática, incluidos
#'   estadísticos de orden, cuantiles y rangos temporales.
#' @param ... Argumentos con nombre enviados a [perfilar()]. Es una alternativa
#'   concisa a `argumentos_perfil`.
#'
#' @return Objeto S3 `analisis` con todos los componentes y su cobertura.
#'
#'   Dos diferencias respecto de llamar a las funciones sueltas, y las dos son
#'   deliberadas. La entrada puede ser una matriz de dos dimensiones: se la pasa
#'   a [perfilar()] sin convertir, para que sea él quien la convierta y lo
#'   declare en `meta$entrada_convertida`. Y cuando una columna temporal
#'   pertenece a una columna protegida, `temporal$resumen` gana la columna
#'   `proteccion_temporal` con el texto `[rangos y huecos protegidos]`: la
#'   protección se aplica y se declara en la misma tabla donde faltan los
#'   valores. La llamada directa a [analizar_tiempo()] respeta la misma
#'   declaración cuando recibe ese `perfil`.
#' @export
#' @seealso [guardar_analisis()], [reportar()], [cobertura_analisis()]
#'
#' @examples
#' resultado <- analizar(
#'   datos_administrativos, analizar_dependencias = FALSE,
#'   fecha = as.POSIXct("2026-01-15", tz = "UTC")
#' )
#' resultado
analizar <- function(datos, nombre = .nombre_de_los_datos(substitute(datos)),
                     fecha = Sys.time(),
                     argumentos_perfil = list(), metadatos_variables = NULL,
                     modelo_confirmado = NULL, propuesta_confirmada = NULL,
                     marco = NULL,
                     perfil_evaluacion = NULL, id_medicion = NULL,
                     medir_propuesta = TRUE,
                     conservar_detalle_medicion = FALSE,
                     muestra = 1e5, muestra_asociacion = 1e4,
                     max_valores = 20L,
                     probabilidades = c(0, 0.25, 0.5, 0.75, 1),
                     umbral_asociacion = 0.3,
                     max_columnas_asociacion = 50L,
                     max_niveles_asociacion = 50L,
                     max_pares_asociacion = 500L,
                     metodo_asociacion_numerica = c("pearson", "spearman"),
                     calendario = 1:7, frecuencia_dias = NULL,
                     max_huecos = 20L, max_columnas_temporales = 50L,
                     conservar_datos = FALSE,
                     proteger_datos_personales = TRUE, ...) {
  # `perfilar()` documenta y acepta una matriz de dos dimensiones: la convierte
  # con `as.data.frame()` y **lo declara** en `meta$entrada_convertida`. La
  # puerta de entrada la rechazaba antes de delegar, asi que `analizar()` no
  # aceptaba lo que su propio motor documenta.
  #
  # La matriz se le pasa a `perfilar()` TAL CUAL, para que sea el quien convierta
  # y declare: convertirla aca dejaba `entrada_convertida = NA` y el perfil
  # aparentaba haber recibido lo que no recibio. El resto de `analizar()` usa la
  # copia convertida, porque necesita una tabla.
  entrada_para_perfilar <- datos
  if (!inherits(datos, "data.frame") && is.matrix(datos) &&
      length(dim(datos)) == 2L) {
    datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  }
  .validar_datos_tabla(datos)
  datos_originales <- datos
  extras <- list(...)
  if (!is.list(argumentos_perfil) || is.null(names(argumentos_perfil)) &&
      length(argumentos_perfil)) {
    .detener("`argumentos_perfil` debe ser una lista con nombres.", call. = FALSE)
  }
  if (length(argumentos_perfil) && any(!nzchar(names(argumentos_perfil))) ||
      length(extras) && (is.null(names(extras)) || any(!nzchar(names(extras))))) {
    .detener("Todos los argumentos de perfilar() deben tener nombre.", call. = FALSE)
  }
  argumentos_perfil <- c(argumentos_perfil, extras)
  if (anyDuplicated(.nombres_para_operar(names(argumentos_perfil)))) {
    .detener("Los argumentos de perfilar() no pueden repetirse.", call. = FALSE)
  }
  reservados <- c("datos", "nombre", "fecha", "muestra",
                  "proteger_datos_personales")
  if (any(names(argumentos_perfil) %in% reservados)) {
    .detener("`argumentos_perfil` no puede reemplazar argumentos coordinados por analizar().",
         call. = FALSE)
  }
  .validar_bandera_proteccion(proteger_datos_personales)
  logicos <- c(
    conservar_datos, medir_propuesta, conservar_detalle_medicion
  )
  if (!is.logical(conservar_datos) || length(conservar_datos) != 1L ||
      !is.logical(medir_propuesta) || length(medir_propuesta) != 1L ||
      !is.logical(conservar_detalle_medicion) ||
      length(conservar_detalle_medicion) != 1L || anyNA(logicos)) {
    .detener("Los controles de medicion, conservacion y proteccion deben ser logicos.",
         call. = FALSE)
  }
  if (!is.null(modelo_confirmado) && !is.null(propuesta_confirmada)) {
    .detener("Use `modelo_confirmado` o `propuesta_confirmada`, no ambos.",
         call. = FALSE)
  }
  if (!is.null(modelo_confirmado) && !inherits(modelo_confirmado, "modelo_calidad")) {
    .detener("`modelo_confirmado` debe provenir de modelo().", call. = FALSE)
  }
  if (!is.null(propuesta_confirmada) &&
      !inherits(propuesta_confirmada, "propuesta_modelo")) {
    .detener("`propuesta_confirmada` debe provenir de proponer_modelo().",
         call. = FALSE)
  }
  if (!is.null(marco) && !inherits(marco, "marco_calidad")) {
    .detener("`marco` debe provenir de marco_calidad().", call. = FALSE)
  }
  # La fecha se lee UNA vez, en la entrada, como la leen `perfilar()` y
  # `medir()`: el texto crudo llegaba a `.nuevo_id_medicion()` y abortaba con un
  # error de R lo que las dos piezas aceptan. Medido en la ronda 32.
  # Si la fecha la declaro quien llama se pregunta ANTES de leerla: despues de
  # asignarla, `missing()` ya no lo dice. La medicion lo declara como `medir()`.
  fecha_declarada <- !missing(fecha)
  fecha <- .fecha_argumento_utc(fecha)
  argumentos <- c(list(
    # La entrada SIN convertir: si era una matriz, `perfilar()` la convierte y lo
    # declara en `meta$entrada_convertida`. Pasarle la copia ya convertida hacia
    # que el perfil aparentara haber recibido una tabla.
    datos = entrada_para_perfilar, nombre = nombre, fecha = fecha,
    muestra = muestra,
    proteger_datos_personales = proteger_datos_personales
  ), argumentos_perfil)
  perfil <- do.call(perfilar, argumentos)
  datos <- .tabla_base(datos_originales)
  # El universo declarado en `argumentos_perfil` gobierna tambien las
  # distribuciones. Ver `.universo_declarado_analisis()`.
  distribuciones <- distribucion_valores(
    datos, perfil, max_valores, probabilidades, muestra,
    proteger_datos_personales,
    .universo = .universo_declarado_analisis(datos, argumentos_perfil)
  )
  variables <- clasificar_variables(
    datos, perfil, metadatos_variables,
    muestra = muestra,
    proteger_datos_personales = proteger_datos_personales
  )
  metodo_asociacion_numerica <- .elegir_opcion(metodo_asociacion_numerica)
  # Una columna declarada en `columnas_codigo` es un codigo: se asocia como
  # categoria, no con una correlacion que supone una magnitud. Sin esto el mismo
  # objeto publicaba `cod` como codigo en el perfil y su Pearson con `monto` en
  # `asociaciones` (refutacion 33-D). Se convierte una copia; los datos del
  # usuario no se tocan.
  datos_asociaciones <- .codigos_como_categoria(
    datos, perfil$meta[["columnas_codigo"]]
  )
  asociaciones <- detectar_asociaciones(
    datos_asociaciones, perfil$dependencias, umbral_asociacion, muestra_asociacion,
    max_columnas_asociacion, max_niveles_asociacion, max_pares_asociacion,
    metodo_numerico = metodo_asociacion_numerica
  )
  cobertura_asociaciones <- attr(
    asociaciones, "cobertura_diagnosticos", exact = TRUE
  )
  if (inherits(cobertura_asociaciones, "data.frame") &&
      nrow(cobertura_asociaciones)) {
    perfil$cobertura_diagnosticos <- rbind(
      perfil$cobertura_diagnosticos, cobertura_asociaciones
    )
  }
  temporal <- analizar_tiempo(
    datos, perfil, calendario = calendario, frecuencia_dias = frecuencia_dias,
    max_huecos = max_huecos, max_columnas = max_columnas_temporales
  )
  propuesta <- proponer_modelo(perfil, datos)
  plan <- planificar_limpieza(perfil, datos)
  seleccion_automatica <- if (is.null(modelo_confirmado) &&
                              is.null(propuesta_confirmada) &&
                              medir_propuesta) {
    which(as.character(propuesta$estado) == "lista")
  } else integer()
  propuesta_medida <- if (length(seleccion_automatica)) {
    propuesta_automatica <- propuesta
    propuesta_automatica$incluir[] <- FALSE
    propuesta_automatica$incluir[seleccion_automatica] <- TRUE
    propuesta_automatica
  } else NULL
  modelo_elegido <- if (!is.null(propuesta_confirmada)) {
    modelo_desde_propuesta(propuesta_confirmada)
  } else if (!is.null(modelo_confirmado)) {
    modelo_confirmado
  } else if (!is.null(propuesta_medida)) {
    modelo_desde_propuesta(propuesta_medida)
  } else NULL
  marco_elegido <- if (!is.null(marco)) {
    marco
  } else {
    .marco_modelo_analisis(modelo_elegido)
  }
  if (!is.null(modelo_elegido)) {
    dimensiones_modelo <- vapply(modelo_elegido$metricas, function(x) {
      x$declaracion$dimension
    }, character(1L))
    factores_modelo <- vapply(modelo_elegido$metricas, function(x) {
      x$declaracion$factor
    }, character(1L))
    claves_modelo <- vapply(modelo_elegido$metricas, function(x) {
      .clave_par_identificador(
        x$declaracion$dimension, x$declaracion$factor, sep = "\r"
      )
    }, character(1L))
    claves_marco <- .clave_par_identificador(
      marco_elegido$factores$dimension, marco_elegido$factores$factor,
      sep = "\r"
    )
    if (!anyNA(dimensiones_modelo) && !anyNA(factores_modelo) &&
        all(nzchar(dimensiones_modelo)) && all(nzchar(factores_modelo)) &&
        all(.identificadores_en(claves_modelo, claves_marco))) {
      modelo_elegido <- modelo(modelo_elegido$metricas, marco = marco_elegido)
    }
  }
  # El universo aplicable se declara una sola vez, en `argumentos_perfil`, y
  # tiene que gobernar todo el analisis. Sin esta linea el perfil lo respetaba
  # -la proporcion de faltantes salia sobre las filas del universo- y la
  # medicion no: sobre una columna condicionada con universo de 300 filas de
  # 1.000, el perfil informaba 0,100 de faltantes y el tablero 0,270 de
  # completitud, que es el mismo dato contado de dos maneras dentro del mismo
  # objeto.
  aplicabilidad_declarada <- if (is.list(argumentos_perfil)) {
    argumentos_perfil[["aplicabilidad"]]
  } else NULL
  # Sin `conservar_detalle_medicion`, el detalle no se devuelve, y tampoco se
  # arma entero: se mide y se agrega METRICA POR METRICA. Medirlo todo junto
  # materializaba una fila por celda y por metrica antes de agregar -21 filas de
  # medida por fila de datos en una tabla de 19 columnas-, y sobre 4,24 millones
  # de filas eso mato el proceso con 103 GB en una evaluacion real. Medido en
  # 100.000 filas: `analizar()` 2,41 GB, y 0,95 GB sin medir. Con el detalle
  # pedido se sigue el camino de siempre, porque ahi el detalle ES el resultado.
  detalle_medicion <- NULL
  agregado <- NULL
  if (!is.null(modelo_elegido)) {
    if (conservar_detalle_medicion) {
      detalle_medicion <- medir(
        modelo_elegido, datos, id_medicion = id_medicion, fecha = fecha,
        aplicabilidad = aplicabilidad_declarada
      )
    } else {
      agregado <- .medir_y_agregar_por_metrica(
        modelo_elegido, datos, id_medicion = id_medicion, fecha = fecha,
        aplicabilidad = aplicabilidad_declarada
      )
      if (is.null(agregado$agregada)) detalle_medicion <- agregado$vacia
    }
  }
  if (!is.null(perfil_evaluacion) && is.null(detalle_medicion) &&
      is.null(agregado)) {
    .detener("`perfil_evaluacion` requiere una medici\u00f3n activa.",
         call. = FALSE)
  }
  cobertura <- cobertura_analisis(
    perfil,
    if (!is.null(agregado$agregada)) agregado$agregada else detalle_medicion,
    modelo = marco_elegido
  )
  preparado <- if (!is.null(agregado$agregada)) {
    .armar_tablero(
      agregado$agregada, agregado$agregada, marco = marco_elegido,
      cobertura = cobertura,
      cobertura_metricas = agregado$cobertura_metricas,
      alcance_medidas = agregado$alcance_medidas
    )
  } else if (!is.null(detalle_medicion) && nrow(detalle_medicion)) {
    .preparar_tablero(
      detalle_medicion, marco = marco_elegido, cobertura = cobertura
    )
  } else {
    # Un modelo donde NINGUNA metrica produjo medidas -todas sin valores o con su
    # metodo fallido- hacia abortar `analizar()` entero en el tablero, y con el se
    # perdian el perfil, el plan y todo lo demas ya calculado. Es la misma puerta
    # que `medir()` cerro para una metrica: lo que no se pudo medir se declara.
    # El tablero sale vacio y lleva el motivo de cada metrica.
    vacio <- .tablero_vacio(cobertura = cobertura, marco = marco_elegido)
    motivos <- attr(detalle_medicion, "cobertura_metricas", exact = TRUE)
    if (inherits(motivos, "data.frame") && nrow(motivos)) {
      attr(vacio, "cobertura_metricas") <- motivos
    }
    list(tablero = vacio, medicion = NULL)
  }
  medicion <- preparado$medicion
  tablero <- preparado$tablero
  # `medir()` registra si la fecha la declaro quien llama; aca quien llama es el
  # usuario de `analizar()`, que siempre le pasa una fecha a `medir()`. Las dos
  # mediciones del objeto lo dicen igual.
  if (inherits(medicion, "data.frame")) {
    attr(medicion, "fecha_declarada") <- fecha_declarada
  }
  if (inherits(detalle_medicion, "data.frame")) {
    attr(detalle_medicion, "fecha_declarada") <- fecha_declarada
  }
  evaluacion <- if (!is.null(perfil_evaluacion) && is.null(medicion)) {
    .avisar(
      "No se evalu\u00f3 `perfil_evaluacion`: ninguna m\u00e9trica del modelo ",
      "produjo medidas. El motivo de cada una est\u00e1 en ",
      "`attr(resultado$tablero, \"cobertura_metricas\")`.", call. = FALSE
    )
    NULL
  } else if (!is.null(perfil_evaluacion)) {
    evaluar(medicion, perfil_evaluacion)
  } else NULL
  desenlaces <- .desenlaces_de_objeto(evaluacion)
  if (!is.null(desenlaces)) {
    if (inherits(medicion, "data.frame")) {
      attr(medicion, "desenlaces") <- desenlaces
      medicion <- .proteger_medicion_desenlaces(
        medicion, desenlaces, reemplazo = NA_real_, marcar_objeto = TRUE
      )
    }
    if (inherits(detalle_medicion, "data.frame")) {
      attr(detalle_medicion, "desenlaces") <- desenlaces
      detalle_medicion <- .proteger_medicion_desenlaces(
        detalle_medicion, desenlaces, reemplazo = NA_real_,
        marcar_objeto = TRUE
      )
    }
    if (inherits(tablero, "data.frame")) {
      attr(tablero, "desenlaces") <- desenlaces
      tablero <- .proteger_tablero_desenlaces(tablero, desenlaces)
    }
    evaluacion <- .proteger_evaluacion_desenlaces(evaluacion, incluir_medidas = FALSE)
  }
  seleccion_confirmada <- if (!is.null(propuesta_confirmada)) {
    which(propuesta_confirmada$incluir &
            as.character(propuesta_confirmada$estado) == "lista")
  } else integer()
  decision_medicion <- if (!is.null(modelo_confirmado)) {
    .decision_medicion_modelo(modelo_elegido)
  } else if (!is.null(propuesta_confirmada)) {
    .decision_medicion_propuesta(
      propuesta_confirmada, seleccion_confirmada,
      confirmada = TRUE, medicion_automatica = FALSE
    )
  } else {
    .decision_medicion_propuesta(
      propuesta, seleccion_automatica,
      confirmada = FALSE, medicion_automatica = medir_propuesta
    )
  }
  advertencias <- .advertencias_analisis(
    distribuciones, asociaciones, temporal, variables, propuesta, perfil
  )
  fecha_utc <- .fecha_utc(fecha)
  estructura <- list(
    perfil = perfil, distribuciones = distribuciones,
    asociaciones = asociaciones, temporal = temporal, variables = variables,
    propuesta_modelo = propuesta, decision_medicion = decision_medicion,
    tablero = tablero, medicion = medicion,
    detalle_medicion = if (conservar_detalle_medicion) detalle_medicion else NULL,
    evaluacion = evaluacion,
    plan_limpieza = plan, cobertura = cobertura, advertencias = advertencias,
    datos = if (conservar_datos) as.data.frame(datos) else NULL,
    meta = list(
      nombre = nombre, fecha = fecha_utc,
      version_esquema = .version_esquema_analisis,
      version_paquete = .version_paquete(), datos_conservados = conservar_datos,
      modelo_medido = !is.null(medicion), evaluado = !is.null(evaluacion),
      propuesta_medida_automaticamente = length(seleccion_automatica) > 0L,
      propuesta_confirmada = !is.null(propuesta_confirmada) ||
        !is.null(modelo_confirmado),
      detalle_medicion_conservado = conservar_detalle_medicion &&
        !is.null(detalle_medicion),
      marco_calidad = marco_elegido$nombre
    )
  )
  class(estructura) <- "analisis"
  if (proteger_datos_personales) {
    .proteger_analisis(estructura, datos)
  } else {
    estructura
  }
}

#' @export
print.analisis <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1(.cli_literal(paste("Analisis de datos:", x$meta$nombre)))
  cli::cli_dl(.cli_literal(c(
    "Filas" = x$perfil$general$filas,
    "Columnas" = x$perfil$general$columnas,
    "Hallazgos del perfil" = nrow(x$perfil$hallazgos),
    "Advertencias de alcance" = nrow(x$advertencias),
    "Asociaciones informadas" = nrow(x$asociaciones),
    "Series temporales" = nrow(x$temporal$resumen),
    "Modelo medido" = if (isTRUE(x$meta$modelo_medido)) "si" else "no",
    "Detalle fila a fila" = if (isTRUE(x$meta$detalle_medicion_conservado)) {
      "conservado"
    } else {
      "no conservado"
    }
  )))
  cli::cli_h2("Tablero de calidad")
  print(tablero_calidad(x))
  cli::cli_h2("Cobertura conceptual")
  conteos <- table(.factor_por_bytes(x$cobertura$estado))
  vista <- data.frame(
    estado = names(conteos), factores = as.integer(conteos),
    stringsAsFactors = FALSE
  )
  .print_data_frame_bytes(vista, row.names = FALSE)
  if (nrow(x$advertencias)) {
    cli::cli_h2("Advertencias de alcance")
    .print_data_frame_bytes(
      x$advertencias[c("componente", "tipo", "descripcion")],
      row.names = FALSE
    )
  }
  invisible(original)
}

.descriptor_funcion <- function(propuesta, i, nombre) {
  atributos <- propuesta$atributos_ligados[[i]]
  if (identical(nombre, "regla") &&
      startsWith(propuesta$origen[[i]], "dependencia_funcional:") &&
      length(atributos) == 2L) {
    return(list(
      tipo = "dependencia_funcional", determinante = atributos[[1L]],
      dependiente = atributos[[2L]], incluir_original = propuesta$incluir[[i]],
      estado_original = propuesta$estado[[i]]
    ))
  }
  list(
    tipo = "funcion_no_reconstruible", propiedad = nombre,
    incluir_original = propuesta$incluir[[i]],
    estado_original = propuesta$estado[[i]]
  )
}

.deshidratar_propuesta <- function(propuesta) {
  if (!inherits(propuesta, "propuesta_modelo")) {
    return(list(propuesta = propuesta, n_funciones = 0L))
  }
  n_funciones <- 0L
  for (i in seq_len(nrow(propuesta))) {
    configuracion <- propuesta$configuracion[[i]]
    funciones <- names(configuracion)[vapply(configuracion, is.function, logical(1L))]
    if (!length(funciones)) next
    for (nombre in funciones) {
      configuracion[[nombre]] <- structure(
        .descriptor_funcion(propuesta, i, nombre),
        class = "descriptor_funcion_analisis"
      )
      n_funciones <- n_funciones + 1L
    }
    propuesta$configuracion[[i]] <- configuracion
    propuesta$incluir[[i]] <- FALSE
    propuesta$estado[[i]] <- "requiere_reconstruccion"
  }
  list(propuesta = propuesta, n_funciones = n_funciones)
}

.advertencia_persistencia_propuesta <- function(x) {
  persistencia <- x$meta$persistencia
  if (!is.list(persistencia)) return(NULL)
  sustituidas <- suppressWarnings(as.numeric(persistencia$funciones_sustituidas))
  if (length(sustituidas) != 1L || !is.finite(sustituidas) || sustituidas <= 0) {
    return(NULL)
  }
  if (inherits(x$advertencias, "data.frame") &&
      "tipo" %in% names(x$advertencias) &&
      any(as.character(x$advertencias$tipo) ==
          "persistencia_funciones_sustituidas")) {
    return(NULL)
  }
  data.frame(
    componente = "modelo",
    tipo = "persistencia_funciones_sustituidas",
    severidad = factor(
      "sospechoso", levels = c("ok", "sospechoso", "error"), ordered = TRUE
    ),
    descripcion = paste0(
      "El analisis fue releido despues de sustituir ", sustituidas,
      " funcion(es) de reglas por descriptores. La propuesta se volvio a ",
      "derivar sin esas funciones y puede no coincidir con la decision de ",
      "medicion registrada en la corrida original; la decision original se ",
      "conserva."
    ),
    stringsAsFactors = FALSE
  )
}

.hidratar_propuesta <- function(propuesta, datos) {
  if (!inherits(propuesta, "propuesta_modelo")) return(propuesta)
  for (i in seq_len(nrow(propuesta))) {
    configuracion <- propuesta$configuracion[[i]]
    descriptores <- names(configuracion)[vapply(
      configuracion, inherits, logical(1L), "descriptor_funcion_analisis"
    )]
    if (!length(descriptores)) next
    reconstruida <- TRUE
    requiere_funcion <- FALSE
    for (nombre in descriptores) {
      descriptor <- configuracion[[nombre]]
      if (identical(descriptor$tipo, "dependencia_funcional") &&
          inherits(datos, "data.frame") &&
          !anyNA(.indice_nombre(
            c(descriptor$determinante, descriptor$dependiente), names(datos)
          ))) {
        ligados <- names(datos)[.indice_nombre(
          c(descriptor$determinante, descriptor$dependiente), names(datos)
        )]
        configuracion[[nombre]] <- .regla_desde_dependencia(
          datos, ligados[[1L]], ligados[[2L]]
        )
      } else {
        reconstruida <- FALSE
        requiere_funcion <- requiere_funcion ||
          identical(descriptor$tipo, "funcion_no_reconstruible")
      }
    }
    propuesta$configuracion[[i]] <- configuracion
    if (reconstruida) {
      propuesta$incluir[[i]] <- isTRUE(descriptor$incluir_original)
      propuesta$estado[[i]] <- descriptor$estado_original
    } else {
      propuesta$incluir[[i]] <- FALSE
      propuesta$estado[[i]] <- if (requiere_funcion) {
        "requiere_funcion"
      } else "requiere_datos"
    }
  }
  propuesta
}

.proteger_propuesta_analisis <- function(propuesta, sensibles) {
  if (!inherits(propuesta, "propuesta_modelo") || !length(sensibles)) return(propuesta)
  for (i in seq_len(nrow(propuesta))) {
    if (!any(.nombres_para_operar(propuesta$atributos_ligados[[i]]) %in%
             .nombres_para_operar(sensibles))) next
    configuracion <- propuesta$configuracion[[i]]
    campos <- intersect(names(configuracion), c("valores", "diccionario"))
    for (campo in campos) configuracion[[campo]] <- "[valor protegido]"
    if (length(campos)) {
      propuesta$configuracion[[i]] <- configuracion
      propuesta$incluir[[i]] <- FALSE
      propuesta$estado[[i]] <- "requiere_valores_protegidos"
    }
  }
  propuesta
}

# Los atributos que hacen a la CLASE de una columna -como se lee- y no llevan
# valores: son lo unico que conserva una columna protegida de la copia. Todo lo
# demas puede llevar sus valores -las etiquetas de un `haven_labelled`, los
# nombres de un vector, un atributo propio- y se quita: `x[] <- NA` vaciaba las
# celdas y dejaba los nombres de las docentes en `labels`. Ronda 28.
.ATRIBUTOS_DE_CLASE_COLUMNA <- c("class", "tzone", "units", "dim")

.proteger_datos_conservados <- function(datos, sensibles) {
  if (!inherits(datos, "data.frame") || !length(sensibles)) return(datos)
  reemplazo <- "[valor protegido]"
  indices <- .indice_nombre(sensibles, names(datos))
  for (indice in unique(indices[!is.na(indices)])) {
    nombre <- names(datos)[[indice]]
    x <- datos[[indice]]
    if (is.character(x) || is.factor(x)) {
      datos[[indice]] <- rep(reemplazo, NROW(x))
    } else if (is.atomic(x)) {
      x[] <- NA
      for (atributo in setdiff(names(attributes(x)),
                               .ATRIBUTOS_DE_CLASE_COLUMNA)) {
        attr(x, atributo) <- NULL
      }
      datos[[indice]] <- x
    } else {
      datos[[indice]] <- rep(list(NULL), NROW(x))
    }
  }
  datos
}

# El PISO sobre todo lo demas de la copia conservada: un valor de una columna
# protegida que identifica no se publica en ninguna parte, tampoco en la columna
# de al lado de la copia. Medido en la ronda 27: `analizar(conservar_datos =
# TRUE)` tapaba `cedula` y dejaba las sesenta cedulas en `obs`, "pago CI
# 3.738.538-0". Celda por celda, con la regla de los valores -la misma que tapa
# la moda de `obs`-, y en los numeros, la cedula guardada como entero en otra
# columna. Sin el recorrido de las salidas: aca no hay prosa, ni nombres, ni
# vocabulario del paquete, y una columna del usuario llamada `estado` no es
# vocabulario.
#
# Y en TODOS los lugares donde un data.frame guarda un valor, no solo en las
# celdas de texto y de numeros: los nombres de fila, los atributos de la tabla y
# de cada columna -las etiquetas de valor de un `haven_labelled`-, las columnas
# de cualquier clase -una copia `POSIXct` de la fecha con hora protegida, que
# `perfilar()` si tapa- y las listas, elemento por elemento. Se recorrian solo el
# texto, el factor y `is.numeric()`, y la copia publicaba las ciento veinte
# cedulas por cada una de esas cuatro puertas. Medido en la ronda 28. Es una
# sola regla, recursiva: cada valor, donde este, pasa por el piso.
.proteger_piso_datos_conservados <- function(datos, identificantes, sensibles) {
  if (!inherits(datos, "data.frame") || !length(identificantes)) return(datos)
  propias <- .indice_nombre(sensibles, names(datos))
  propias <- unique(propias[!is.na(propias)])
  for (j in setdiff(seq_along(datos), propias)) {
    datos[[j]] <- .piso_valor_conservado(datos[[j]], identificantes)
  }
  # La columna protegida ya no tiene valores, pero conserva los atributos de su
  # clase -`.ATRIBUTOS_DE_CLASE_COLUMNA`-, y uno de ellos puede llevar uno:
  # `units = "Cliente Apellidoso 009"` en la cedula guardada como numero o en
  # la fecha de nacimiento se publicaba, mientras en `monto` se tapaba. Medido en
  # la ronda 30. Pasan por el piso como los de las demas, salvo la clase y la
  # forma.
  for (j in propias) {
    datos[[j]] <- .piso_atributos_conservados(datos[[j]], identificantes,
                                              estructurales = c("class", "dim"))
  }
  .piso_marco_conservado(datos, identificantes)
}

# Los nombres de fila y los atributos de la tabla. Los nombres de fila tienen que
# ser unicos y el marcador no lo es: si alguno lleva un valor protegido, vuelven
# a ser los numeros de fila. Los nombres de columna de la copia son estructura y
# quedan; los de una tabla ANIDADA en una columna lista (`nombres = TRUE`) son
# valores del usuario y pasan por el piso: un data.frame cuyos nombres de columna
# eran tres cedulas quedaba entero en la copia. Medido en la ronda 30.
.piso_marco_conservado <- function(datos, identificantes, vistos = list(),
                                   nombres = FALSE) {
  filas <- attr(datos, "row.names", exact = TRUE)
  if (is.character(filas)) {
    tapadas <- .reemplazar_valores_protegidos(filas, identificantes)
    if (!identical(tapadas, filas)) {
      attr(datos, "row.names") <- .set_row_names(length(filas))
    }
  }
  if (nombres && length(names(datos))) {
    tapados <- .reemplazar_valores_protegidos(names(datos), identificantes)
    if (!identical(tapados, names(datos))) names(datos) <- tapados
  }
  .piso_atributos_conservados(datos, identificantes, vistos,
                              c("names", "row.names", "class"))
}

# `vistos`: los entornos que ya se estan recorriendo, para no entrar dos veces
# en el mismo -un entorno puede guardarse a si mismo-.
.piso_valor_conservado <- function(x, identificantes, vistos = list()) {
  if (is.null(x)) return(x)
  if (is.environment(x)) {
    return(.piso_entorno_conservado(x, identificantes, vistos))
  }
  # Una llamada, una formula, un simbolo o una funcion no se recorren como un
  # vector: se leen como el texto que los escribe -`deparse()`- y, la funcion,
  # tambien por su entorno. `quote(buscar(doc == "4.685.467-9"))` y
  # `function() "4.685.467-9"` en una columna lista quedaban enteros en la
  # copia. Medido en la ronda 30. Como con un entorno, si llevan un valor
  # protegido la copia lleva el marcador en su lugar.
  if (is.function(x) || is.language(x)) {
    return(.piso_lenguaje_conservado(x, identificantes, vistos))
  }
  if (is.data.frame(x)) {
    for (j in seq_along(x)) {
      x[[j]] <- .piso_valor_conservado(x[[j]], identificantes, vistos)
    }
    return(.piso_marco_conservado(x, identificantes, vistos, nombres = TRUE))
  }
  # Una fecha con hora `POSIXlt` -lo que deja `d$x <- strptime(...)`- es una
  # LISTA por dentro, y `x[[i]]` de una `POSIXlt` es otra `POSIXlt`: el
  # recorrido de las listas no terminaba nunca y `analizar(conservar_datos =
  # TRUE)` abortaba con "stack desbordado" sobre una columna cualquiera, con la
  # proteccion puesta y sin ella no. Medido en la ronda 29. Es una fecha con
  # hora y se tapa como la `POSIXct`, celda por celda. Y sus atributos, como los
  # de cualquier objeto, abajo: devolverla aca dejaba `comment` y `label` en
  # claro. Medido en la ronda 30.
  es_posixlt <- inherits(x, "POSIXlt")
  if (es_posixlt) {
    tapadas <- tryCatch(
      .celdas_conservadas_tapadas(as.POSIXct(x), identificantes),
      error = function(e) rep(FALSE, length(x))
    )
    if (any(tapadas)) x[tapadas] <- NA
  } else if (is.list(x) && is.object(x)) {
    # Y por lo mismo cualquier otra lista CON CLASE -un registro de `vctrs`, un
    # `blob`, una lista con `I()`-: su `[[` puede devolver el mismo objeto. Se
    # recorre sin la clase, que es donde estan sus valores, y se le devuelve.
    clase <- oldClass(x)
    x <- .piso_valor_conservado(unclass(x), identificantes, vistos)
    oldClass(x) <- clase
    return(x)
  }
  if (!es_posixlt && is.list(x)) {
    textos <- vapply(x, function(y) {
      is.null(y) || (is.character(y) && is.null(attributes(y)))
    }, logical(1L))
    # Una lista de puros `NULL` no tiene nada que tapar: `unlist()` daba `NULL`
    # y `split()` abortaba `analizar(conservar_datos = TRUE)` sobre una columna
    # lista vacia. Medido en la ronda 31.
    if (length(x) && all(textos) && any(lengths(x) > 0L)) {
      # Lo comun -una lista de textos, como `I(list(c("pago", "CI ...")))`- de
      # una vez y no elemento por elemento.
      largos <- lengths(x)
      tapados <- .reemplazar_valores_protegidos(
        unlist(x, use.names = FALSE), identificantes
      )
      partes <- split(tapados, factor(rep(seq_along(x), largos),
                                      levels = seq_along(x)))
      for (i in which(largos > 0L)) x[[i]] <- unname(partes[[i]])
    } else {
      for (i in seq_along(x)) {
        if (!is.null(x[[i]])) {
          x[i] <- list(.piso_valor_conservado(x[[i]], identificantes, vistos))
        }
      }
    }
  } else if (is.factor(x)) {
    niveles <- .reemplazar_valores_protegidos(levels(x), identificantes)
    if (!identical(niveles, levels(x))) {
      atributos <- attributes(x)
      x <- factor(niveles[as.integer(x)], levels = unique(niveles))
      for (atributo in setdiff(names(atributos), c("levels", "class"))) {
        attr(x, atributo) <- atributos[[atributo]]
      }
    }
  } else if (is.character(x)) {
    tapados <- .reemplazar_valores_protegidos(as.vector(x), identificantes)
    cambiadas <- !is.na(tapados) & (is.na(x) | tapados != x)
    if (any(cambiadas)) x[cambiadas] <- tapados[cambiadas]
  } else if (is.raw(x)) {
    x <- .piso_bytes_conservados(x, identificantes)
  } else if (is.numeric(x) && !is.object(x)) {
    x <- .proteger_numeros_parametros(x, identificantes)
  } else if (is.atomic(x)) {
    if (is.numeric(x)) x <- .proteger_numeros_parametros(x, identificantes)
    tapadas <- .celdas_conservadas_tapadas(x, identificantes)
    if (any(tapadas)) x[tapadas] <- NA
  }
  # Todo atributo que no hace a la forma del objeto pasa por el piso, tambien
  # `levels` en lo que no es un factor y `units`: un entero con `levels` y una
  # columna con `units` guardaban cedulas en la copia. Medido en la ronda 29.
  # La unidad y la zona horaria de una fecha -`secs`, `UTC`- no llevan un valor
  # protegido y quedan como estan.
  estructurales <- c("class", "dim", "row.names", if (is.factor(x)) "levels")
  .piso_atributos_conservados(x, identificantes, vistos, estructurales)
}

# Los atributos de un objeto de la copia, cada uno por el piso: su VALOR, con
# el recorrido de arriba, y su NOMBRE, que tambien es texto del usuario:
# `attr(x, "2.037.187-5") <- TRUE` quedaba en la copia y `print()` lo
# mostraba. Medido en la ronda 31. Un nombre que lleva un valor protegido se
# cambia por el marcador -sin repetir otro nombre-; los que R usa para la forma
# de un objeto (`.ATRIBUTOS_DE_FORMA_R`) no son texto de nadie y no se miran.
# `estructurales`: los que no se recorren.
.ATRIBUTOS_DE_FORMA_R <- c(
  "class", "dim", "dimnames", "names", "row.names", "levels", "tsp",
  "comment", "srcref", "srcfile", "wholeSrcref", ".Environment", "tzone",
  "units"
)

.piso_atributos_conservados <- function(x, identificantes, vistos = list(),
                                        estructurales = character()) {
  for (atributo in setdiff(names(attributes(x)), estructurales)) {
    valor <- .piso_valor_conservado(
      attr(x, atributo, exact = TRUE), identificantes, vistos
    )
    nombre <- if (atributo %in% .ATRIBUTOS_DE_FORMA_R) atributo else {
      tryCatch(.reemplazar_valores_protegidos(atributo, identificantes),
               error = function(e) "[valor protegido]")
    }
    if (identical(nombre, atributo)) {
      attr(x, atributo) <- valor
    } else {
      attr(x, atributo) <- NULL
      libres <- make.unique(c(names(attributes(x)), nombre))
      nombre <- libres[[length(libres)]]
      attr(x, nombre) <- valor
    }
  }
  x
}

# Fechas, fechas con hora, duraciones, numeros con clase: la celda se tapa si
# alguna de sus escrituras -la del perfil, `as.character()` y `format()`- lleva
# un valor protegido. Es como `perfilar()` publica su moda o sus extremos, y
# como ahi los tapa el piso.
.celdas_conservadas_tapadas <- function(x, identificantes) {
  tapadas <- rep(FALSE, length(x))
  escrituras <- list(as.character, format)
  if (inherits(x, c("Date", "POSIXt"))) {
    escrituras <- c(escrituras, .texto_valor_vector)
  }
  for (escribir in escrituras) {
    texto <- tryCatch(as.character(escribir(x)), error = function(e) NULL)
    if (length(texto) != length(x)) next
    tapado <- .reemplazar_valores_protegidos(texto, identificantes)
    tapadas <- tapadas | (!is.na(texto) & !is.na(tapado) & tapado != texto)
  }
  tapadas
}

# Un BLOB -un vector `raw`, como devuelve DBI un BYTEA o un BLOB- con texto
# adentro es ese texto: los sesenta nombres protegidos guardados en bytes
# quedaban enteros en la copia, porque el piso solo miraba sus cifras
# hexadecimales. Medido en la ronda 29. Se lee como texto -un byte nulo, como un
# salto de linea; lo que no es UTF-8 valido, como latin1- y, si lleva un valor
# protegido, la copia guarda los bytes del texto tapado.
.piso_bytes_conservados <- function(x, identificantes) {
  if (!length(x)) return(x)
  texto <- tryCatch({
    bytes <- as.vector(x)
    bytes[bytes == as.raw(0L)] <- as.raw(10L)
    leido <- rawToChar(bytes)
    if (validUTF8(leido)) {
      Encoding(leido) <- "UTF-8"
      leido
    } else {
      iconv(leido, "latin1", "UTF-8")
    }
  }, error = function(e) NA_character_)
  if (length(texto) != 1L || is.na(texto)) return(x)
  tapado <- tryCatch(
    .reemplazar_valores_protegidos(texto, identificantes),
    error = function(e) "[valor protegido]"
  )
  if (identical(tapado, texto)) return(x)
  salida <- charToRaw(tapado)
  atributos <- attributes(x)
  for (atributo in setdiff(names(atributos), c("names", "dim", "dimnames"))) {
    attr(salida, atributo) <- atributos[[atributo]]
  }
  salida
}

# Un ENTORNO es una referencia: tapar adentro cambiaria el objeto del usuario y
# no la copia. Se recorre su contenido con el mismo piso -sin entrar dos veces en
# el mismo, ni en los entornos con nombre: el global, el de un paquete- y, si
# guarda un valor protegido, la copia lleva el marcador en su lugar. Una cedula
# en un entorno de una columna lista quedaba en la copia. Medido en la ronda 29.
.piso_entorno_conservado <- function(e, identificantes, vistos = list()) {
  # Los del sistema se reconocen por lo que son, no por su nombre:
  # `environmentName()` lee el atributo `name`, y un entorno del usuario con
  # ese atributo y una cedula adentro quedaba entero en la copia. Medido en la
  # ronda 30.
  if (identical(e, emptyenv()) || identical(e, globalenv()) ||
      identical(e, baseenv()) || isNamespace(e) ||
      .es_entorno_de_la_busqueda(e)) {
    return(e)
  }
  for (visto in vistos) if (identical(visto, e)) return(e)
  # Una cadena de entornos sin fin a la vista: el lado cerrado.
  if (length(vistos) >= 8L) return("[valor protegido]")
  # Ronda 32, dos puertas del mismo recorrido. (1) Los ATRIBUTOS del entorno,
  # valor y nombre, como los de cualquier objeto: un entorno con la cedula en
  # su atributo `name` -que `print()` muestra- o en otro, y el entorno de una
  # funcion con ese atributo, quedaban enteros en la copia, porque se recorria
  # solo su contenido. Se leen sin cambiarlos -el entorno es del usuario- y, si
  # llevan un valor protegido, la copia lleva el marcador. (2) Lo que no se
  # puede leer -una promesa o una ligadura activa que aborta al leerse- es el
  # lado cerrado: se devolvia el entorno entero, con la cedula en otra
  # ligadura. Ahora es como lo que se puede leer y lleva un valor protegido,
  # igual que `.piso_lenguaje_conservado()` con lo que no puede escribir.
  atributos <- attributes(e)
  if (length(atributos)) {
    recorridos <- tryCatch(
      .piso_valor_conservado(atributos, identificantes, c(vistos, list(e))),
      error = function(err) NULL
    )
    if (!identical(recorridos, atributos)) return("[valor protegido]")
  }
  contenido <- tryCatch(
    as.list.environment(e, all.names = TRUE, sorted = TRUE),
    error = function(err) err
  )
  if (inherits(contenido, "error")) return("[valor protegido]")
  if (!length(contenido)) return(e)
  tapado <- .piso_valor_conservado(contenido, identificantes, c(vistos, list(e)))
  if (identical(tapado, contenido)) e else "[valor protegido]"
}

# Un entorno de la ruta de busqueda: el global, el de un paquete adjunto, el de
# un `attach()`.
.es_entorno_de_la_busqueda <- function(e) {
  for (i in seq_along(search())) {
    if (identical(e, as.environment(i))) return(TRUE)
  }
  FALSE
}

# Una llamada, una formula, un simbolo o una funcion, por el texto que los
# escribe; la funcion, tambien por su entorno -con el mismo recorrido que un
# entorno de la copia-. Si algo lleva un valor protegido, el marcador en su
# lugar; si no se puede escribir, tambien: es el lado cerrado.
#
# El texto es el que `print()` escribe: con sus atributos (`showAttributes`) y,
# si la funcion guarda su fuente -`srcref`, lo que deja una sesion interactiva o
# `source()`-, la fuente con sus comentarios (`useSource`); y tambien sin ella,
# por si la fuente ya no es el codigo. Y sus atributos, valor y nombre, por el
# mismo recorrido que los de cualquier objeto: `deparse()` no escribe los de
# una llamada ni los de una formula, y el `srcref` guarda el archivo entero en
# su entorno. Se leia solo el codigo, sin atributos ni fuente, y se devolvia
# antes de recorrerlos: `structure(function() NULL, nota = <cedula>)`, la
# formula y la llamada con ese atributo, el comentario `# titular <cedula>` de
# una funcion y el de una expresion quedaban en la copia. Medido en la ronda 31.
.piso_lenguaje_conservado <- function(x, identificantes, vistos = list()) {
  escribir <- function(control) {
    tryCatch(
      paste(.deparse_por_omision(x, width.cutoff = 500L, control = control),
            collapse = "\n"),
      error = function(e) NA_character_
    )
  }
  textos <- c(
    escribir(c("keepInteger", "showAttributes", "useSource")),
    escribir(c("keepInteger", "showAttributes"))
  )
  if (anyNA(textos)) return("[valor protegido]")
  tapados <- tryCatch(
    .reemplazar_valores_protegidos(textos, identificantes),
    error = function(e) rep("[valor protegido]", length(textos))
  )
  if (!identical(tapados, textos)) return("[valor protegido]")
  entorno <- if (is.function(x)) environment(x) else attr(x, ".Environment")
  if (is.environment(entorno) &&
      !identical(.piso_entorno_conservado(entorno, identificantes, vistos),
                 entorno)) {
    return("[valor protegido]")
  }
  atributos <- attributes(x)
  if (length(atributos)) {
    recorridos <- tryCatch(
      attributes(.piso_atributos_conservados(x, identificantes, vistos)),
      error = function(e) NULL
    )
    if (!identical(recorridos, atributos)) return("[valor protegido]")
  }
  x
}

.proteger_analisis <- function(x, datos = NULL) {
  sensibles <- .columnas_personales_protegidas(x$perfil)
  # `datos` es la tabla original y llega por argumento porque `analizar()` por
  # omision NO la conserva: `x$datos` viene NULL, y sin ella los valores
  # identificantes no se pueden recuperar despues. La cosecha del perfil sola no
  # alcanza -trae estadisticos, no los valores de la columna, y con treinta
  # documentos distintos la moda ademas es NA-.
  if (is.null(datos)) datos <- x$datos
  identificantes <- .valores_identificantes(unique(c(
    .valores_perfil_protegidos(
      x$perfil$columnas, x$perfil$patrones, x$perfil$datos_personales,
      x$perfil$meta, datos = datos
    ),
    .valores_publicables_protegidos(datos, sensibles)
  )), .fechas_que_identifican(datos, sensibles))
  x$perfil <- .proteger_perfil(x$perfil, x$datos)
  # Hay que limpiar el plan antes de anonimizar `x$datos`: un analisis abierto
  # puede conservar parametros que vienen de las filas originales.
  x$plan_limpieza <- .proteger_plan_limpieza(
    x$plan_limpieza, x$perfil, x$datos
  )
  if (inherits(x$perfil, "perfil") &&
      !is.null(x$perfil$duplicados_aproximados)) {
    x$perfil$duplicados_aproximados <- .proteger_duplicados_aproximados(
      x$perfil$duplicados_aproximados, sensibles
    )
  }
  if (length(sensibles)) {
    indices <- .nombres_para_operar(x$distribuciones$frecuencias$columna) %in%
      .nombres_para_operar(sensibles)
    x$distribuciones$frecuencias$valor[indices] <- "[valor protegido]"
    if (!"estado" %in% names(x$distribuciones$cuantiles)) {
      x$distribuciones$cuantiles$estado <- rep(
        "calculado", nrow(x$distribuciones$cuantiles)
      )
    }
    indices_cuantiles <- .nombres_para_operar(x$distribuciones$cuantiles$columna) %in%
      .nombres_para_operar(sensibles)
    x$distribuciones$cuantiles$valor[indices_cuantiles] <- NA_real_
    x$distribuciones$cuantiles$estado[indices_cuantiles] <- "valor_protegido"
    indices_variables <- .nombres_para_operar(x$variables$columna) %in%
      .nombres_para_operar(sensibles)
    for (campo in c("niveles_declarados", "niveles_observados", "niveles_ausentes")) {
      x$variables[[campo]][indices_variables] <- lapply(
        x$variables[[campo]][indices_variables],
        function(y) if (length(y)) "[valor protegido]" else character()
      )
    }
    indices_tiempo <- .nombres_para_operar(x$temporal$resumen$columna) %in%
      .nombres_para_operar(sensibles)
    x$temporal$resumen$fecha_minima[indices_tiempo] <- as.Date(NA)
    x$temporal$resumen$fecha_maxima[indices_tiempo] <- as.Date(NA)
    if (!"proteccion_temporal" %in% names(x$temporal$resumen)) {
      x$temporal$resumen$proteccion_temporal <- rep(
        NA_character_, nrow(x$temporal$resumen)
      )
    }
    x$temporal$resumen$proteccion_temporal[indices_tiempo] <-
      "[rangos y huecos protegidos]"
    x$temporal$huecos <- x$temporal$huecos[
      !(.nombres_para_operar(x$temporal$huecos$columna) %in%
          .nombres_para_operar(sensibles)), , drop = FALSE
    ]
    if (inherits(x$plan_limpieza, "data.frame") &&
        all(c("columna", "evidencia") %in% names(x$plan_limpieza))) {
      indices_plan <- !is.na(x$plan_limpieza$columna) &
        .nombres_para_operar(x$plan_limpieza$columna) %in%
          .nombres_para_operar(sensibles)
      x$plan_limpieza$evidencia[indices_plan] <- "[evidencia protegida]"
    }
    x$propuesta_modelo <- .proteger_propuesta_analisis(
      x$propuesta_modelo, sensibles
    )
    x$datos <- .proteger_datos_conservados(x$datos, sensibles)
    x$datos <- .proteger_piso_datos_conservados(x$datos, identificantes, sensibles)
    nombres_datos <- if (inherits(x$datos, "data.frame")) names(x$datos) else character()
    x$meta$columnas_datos_protegidas <- nombres_datos[
      !is.na(.indice_nombre(nombres_datos, sensibles))
    ]
  }
  # El mismo piso que aplican `perfilar()` y `distribucion_valores()`. Todo lo
  # de arriba protege POR COLUMNA, y eso deja pasar el valor identificante que
  # vive en una columna que el clasificador no marco: medido, `analizar()`
  # publicaba `"cliente 77177101"` en `variables$niveles_observados` -un
  # documento adentro de texto libre- mientras `perfilar()`, sobre la misma
  # tabla, lo tapaba. Se barren los resumenes DERIVADOS: `datos` es la tabla que
  # el usuario pidio conservar y tiene su propio tratamiento -sus columnas
  # protegidas enteras y el piso en las demas, arriba-, `perfil` ya paso por
  # `.proteger_perfil()` y `meta` describe la corrida, no los datos.
  if (length(identificantes)) {
    derivados <- setdiff(names(x), c("datos", "perfil", "meta"))
    for (parte in derivados) {
      parametros_originales <- if (identical(parte, "plan_limpieza") &&
                                   is.list(x[[parte]]$parametros)) {
        x[[parte]]$parametros
      }
      x[[parte]] <- .proteger_textos_salida(
        x[[parte]], identificantes,
        intocables = if (!is.null(x$perfil$columnas$columna)) {
          as.character(x$perfil$columnas$columna)
        } else names(x$datos)
      )
      x[[parte]] <- .proteger_numeros_parametros(x[[parte]], identificantes)
      # Lo que el plan ejecuta, como despues del barrido del plan: los
      # marcadores de ausencia guardan su valor, un centinela tapado se declara
      # y un mapa tocado se reconstruye (ver `.ejecutables_tras_barrido()`).
      if (!is.null(parametros_originales)) {
        x[[parte]] <- .ejecutables_tras_barrido(
          x[[parte]], parametros_originales, datos = datos,
          declarados = x$perfil$meta$cadenas_ausencia
        )
      }
    }
  }
  x
}

#' Guardar y recuperar un análisis
#'
#' Persiste un objeto [analizar()] en RDS con número de esquema. Los datos de
#' entrada no se guardan por omisión. Las funciones de reglas se sustituyen por
#' declaraciones pequeñas: una dependencia funcional se reconstruye al leer
#' sólo si los datos fueron incluidos; una función arbitraria queda desactivada.
#' Así el archivo no serializa entornos de ejecución completos.
#'
#' La protección de datos personales con evidencia suficiente se vuelve a
#' aplicar antes de escribir. Incluir datos que contienen columnas protegidas
#' exige desactivar expresamente esa protección; una clasificación débil se
#' conserva como información pero no activa esa restricción.
#'
#' @param x Objeto creado por [analizar()].
#' @param archivo Ruta del archivo RDS. Una ruta que ya es un directorio se
#'   rechaza: el archivo va dentro, con su nombre.
#' @param incluir_datos Si se persiste la copia de los datos conservada por
#'   `analizar(conservar_datos = TRUE)`.
#' @param proteger_datos_personales Si se protege toda evidencia derivada.
#' @param sobrescribir Si se permite reemplazar un archivo existente.
#' @details
#' El archivo se escribe con el **formato RDS 2**, no con el 3 que `saveRDS()`
#' usa por omisión. El 3 anota la codificación nativa de quien escribe y al leer
#' *traduce* desde ella el texto que no declara la suya; en Windows esa
#' traducción no falla y cambia los bytes en silencio, así que un análisis
#' guardado bajo un locale y leído bajo otro podía volver con el texto alterado.
#'
#' Ese formato tiene un costo y conviene saberlo: **no conserva la
#' representación compacta** de los vectores que R guarda así. Medido, una
#' columna `1:1e6` ocupa 103 bytes con el formato 3 y 2.127.903 con el 2; con
#' `incluir_datos = TRUE` sobre una tabla con una columna `id = seq_len(n)`, eso
#' son unos 2 MB por cada millón de filas, y a escala la escritura puede exigir
#' materializar en memoria lo que el formato 3 no materializa. Se eligió así
#' porque un archivo más grande es visible y recuperable, y un texto corrompido
#' en silencio no lo es. Quien prefiera la otra relación puede serializar el
#' objeto por su cuenta.
#'
#' @param comprimir Compresión admitida por [saveRDS()]: un lógico, `"gzip"`,
#'   `"bzip2"` o `"xz"`.
#'
#' @return `guardar_analisis()` devuelve la ruta de forma invisible;
#'   `leer_analisis()` devuelve un objeto `analisis`.
#'
#'   El objeto guardado gana `meta$persistencia`, que declara qué se guardó y
#'   qué no: `version_esquema`, si los datos viajan (`datos_incluidos`), si la
#'   evidencia salió protegida (`evidencia_protegida`) y cuántas funciones se
#'   sustituyeron por su descripción (`funciones_sustituidas`). Es el único
#'   campo que un objeto leído tiene y el original no: lo que se guarda no es
#'   idéntico a lo que se analizó, y esa diferencia se declara en vez de
#'   suponerse. Al leer, si hubo funciones sustituidas, `advertencias` agrega
#'   una fila que declara que la propuesta se volvio a derivar sin ellas y que
#'   puede no coincidir con la decision original, que se conserva.
#' @seealso [analizar()], [reportar()], [guardar_historico()]
#'
#' @examples
#' a <- analizar(datos_administrativos, argumentos_perfil = list(
#'   analizar_dependencias = FALSE
#' ))
#' ruta <- tempfile(fileext = ".rds")
#' guardar_analisis(a, ruta)
#' b <- leer_analisis(ruta)
#' unlink(ruta)
#' @name persistir_analisis
NULL

#' @rdname persistir_analisis
#' @export
guardar_analisis <- function(x, archivo, incluir_datos = FALSE,
                             proteger_datos_personales = TRUE,
                             sobrescribir = FALSE,
                             comprimir = "xz") {
  if (!inherits(x, "analisis")) {
    .detener("`x` debe provenir de analizar().", call. = FALSE)
  }
  if (!.es_texto_escalar(archivo)) .detener("`archivo` debe ser una ruta.", call. = FALSE)
  .validar_bandera_proteccion(proteger_datos_personales)
  logicos <- c(incluir_datos, sobrescribir)
  if (!is.logical(incluir_datos) || length(incluir_datos) != 1L ||
      !is.logical(sobrescribir) || length(sobrescribir) != 1L ||
      anyNA(logicos)) {
    .detener("Los controles de persistencia deben ser logicos.", call. = FALSE)
  }
  # La lista incluia las CADENAS "TRUE" y "FALSE" y la comparacion se hacia
  # sobre `as.character(comprimir)`, asi que `comprimir = "TRUE"` -la cadena-
  # pasaba la validacion y `saveRDS()` la rechazaba despues con
  # `invalid 'compress' argument: TRUE`, un error crudo de R. Una validacion
  # existe para que ese error no llegue al usuario; esta lo dejaba pasar
  # justamente en el caso que parece correcto.
  #
  # `saveRDS()` admite un logico o uno de los tres metodos por nombre. Se pide
  # eso y no su impresion.
  # La lista depende de la VERSION de R y no puede escribirse fija: `saveRDS()`
  # documenta `"zstd"` desde R 4.5, y este paquete declara `R (>= 4.1.0)`. Con
  # la lista fija pasaba una de dos cosas segun donde se corriera: o se rechaza
  # un metodo que R admite -cerrar de mas- o se admite uno que R no conoce y el
  # error crudo vuelve a llegar al usuario, que es el defecto que esta
  # validacion vino a cerrar.
  #
  # Un binario puede no traer `zstd` compilado aunque la version lo documente.
  # Ese caso NO se filtra aca a proposito: `saveRDS()` responde
  # "Zstd compression support was not included in this R binary", que dice
  # exactamente que pasa y que hacer. Lo que no debe llegar es el error por
  # confusion de tipo -`invalid 'compress' argument: TRUE` ante la CADENA
  # "TRUE"-, que no le dice nada a nadie.
  metodos <- c("gzip", "bzip2", "xz")
  if (getRversion() >= "4.5.0") metodos <- c(metodos, "zstd")
  valido <- (is.logical(comprimir) && length(comprimir) == 1L &&
               !is.na(comprimir)) ||
    (is.character(comprimir) && length(comprimir) == 1L &&
       !is.na(comprimir) && comprimir %in% metodos)
  if (!valido) {
    .detener(
      "`comprimir` debe ser TRUE, FALSE o uno de ",
      paste(sprintf("\"%s\"", metodos), collapse = ", "), ".",
      call. = FALSE
    )
  }
  # Usaba su propia copia, que no nombraba el directorio que falta: el usuario
  # leia "No existe el directorio de destino." sin saber cual.
  .validar_destino_archivo(archivo, sobrescribir)
  if (incluir_datos && is.null(x$datos)) {
    .detener("El analisis no conservo datos; use `conservar_datos = TRUE`.",
         call. = FALSE)
  }
  sensibles <- .columnas_personales_protegidas(x$perfil)
  if (incluir_datos && proteger_datos_personales && length(sensibles)) {
    .detener(
      paste(
        "Incluir datos personales exige crear el analisis con",
        "`proteger_datos_personales = FALSE` en `analizar()` y usar la misma",
        "opcion al guardar; el guardado no recupera valores ya protegidos."
      ),
      call. = FALSE
    )
  }
  copia <- if (proteger_datos_personales) .proteger_analisis(x) else x
  copia$medicion <- .proteger_medicion_desenlaces(
    copia$medicion, .desenlaces_de_objeto(copia), reemplazo = NA_real_,
    marcar_objeto = TRUE
  )
  # `copia[["x"]] <- NULL` BORRA el elemento en vez de dejarlo en `NULL`, y
  # estas tres asignaciones devuelven `NULL` cuando el componente no existe. El
  # resultado: un analisis guardado perdia `detalle_medicion`, `evaluacion` y
  # `datos` de sus NOMBRES -medido: 16 componentes al analizar, 13 en el
  # archivo-, y la documentacion afirma, con esas palabras, que
  # `meta$persistencia` es "el unico campo que un objeto leido tiene y el
  # original no". No era cierto.
  #
  # `copia["x"] <- list(valor)` conserva el nombre con su `NULL`. El objeto
  # leido tiene entonces la misma forma que el guardado, y la ausencia de datos
  # se declara donde corresponde: en `meta$persistencia$datos_incluidos`.
  copia["detalle_medicion"] <- list(.proteger_medicion_desenlaces(
    copia$detalle_medicion, .desenlaces_de_objeto(copia),
    reemplazo = NA_real_, marcar_objeto = TRUE
  ))
  copia["tablero"] <- list(.proteger_tablero_desenlaces(
    copia$tablero, .desenlaces_de_objeto(copia)
  ))
  copia["evaluacion"] <- list(
    .proteger_evaluacion_desenlaces(copia$evaluacion, incluir_medidas = FALSE)
  )
  if (!incluir_datos) {
    copia["datos"] <- list(NULL)
    # `datos_conservados` describe al OBJETO, y el objeto que se guarda sin
    # datos no los conserva. Quedaba en TRUE -el valor de cuando se creo el
    # analisis- y el informe HTML del objeto releido publicaba "datos
    # conservados: TRUE" sobre un objeto cuyo `datos` es NULL y cuya propia
    # persistencia declara `datos_incluidos = FALSE`.
    copia$meta$datos_conservados <- FALSE
  }
  propuesta <- .deshidratar_propuesta(copia$propuesta_modelo)
  copia$propuesta_modelo <- propuesta$propuesta
  proteccion_analisis <- is.list(x$perfil$meta) && isTRUE(
    x$perfil$meta$proteger_datos_personales
  )
  copia$meta$persistencia <- list(
    version_esquema = .version_esquema_analisis,
    datos_incluidos = incluir_datos,
    evidencia_protegida = isTRUE(proteger_datos_personales) ||
      proteccion_analisis,
    funciones_sustituidas = propuesta$n_funciones
  )
  temporal <- tempfile(".lupa-analisis-", tmpdir = dirname(archivo), fileext = ".rds")
  on.exit(unlink(temporal), add = TRUE)
  # Version 2 por la misma razon que en `guardar_historico()`: el formato 3
  # anota la codificacion nativa de quien escribe y al leer traduce desde
  # ella las cadenas sin marca. En Windows esa traduccion no falla, apaga el
  # bit alto y corrompe el texto en silencio.
  saveRDS(copia, temporal, version = 2L, compress = comprimir)
  if (!file.copy(temporal, archivo, overwrite = sobrescribir)) {
    .detener("No se pudo escribir el analisis.", call. = FALSE)
  }
  invisible(normalizePath(archivo, winslash = "/", mustWork = TRUE))
}

#' @rdname persistir_analisis
#' @export
leer_analisis <- function(archivo) {
  if (!.es_texto_escalar(archivo) || !file.exists(archivo)) {
    .detener("`archivo` debe ser un RDS existente.", call. = FALSE)
  }
  x <- readRDS(archivo)
  if (!inherits(x, "analisis") || is.null(x$meta$version_esquema)) {
    .detener("El archivo no contiene un analisis versionado.", call. = FALSE)
  }
  if (!identical(as.integer(x$meta$version_esquema), .version_esquema_analisis)) {
    .detener("La version del esquema de analisis no es compatible.", call. = FALSE)
  }
  x$propuesta_modelo <- .hidratar_propuesta(x$propuesta_modelo, x$datos)
  advertencia <- .advertencia_persistencia_propuesta(x)
  if (!is.null(advertencia)) {
    x$advertencias <- rbind(x$advertencias, advertencia)
  }
  x
}

# Las columnas declaradas en `columnas_codigo`, como texto: asi `detectar_asociaciones()`
# las trata como categorias. Solo las numericas cambian; un codigo ya guardado
# como texto o factor ya es una categoria.
.codigos_como_categoria <- function(datos, columnas_codigo) {
  columnas_codigo <- as.character(columnas_codigo)
  if (!length(columnas_codigo)) return(datos)
  indices <- .indice_nombre(columnas_codigo, names(datos))
  for (j in unique(indices[!is.na(indices)])) {
    x <- datos[[j]]
    if ((is.numeric(x) || inherits(x, "integer64")) &&
        !inherits(x, c("Date", "POSIXt", "difftime"))) {
      texto <- as.character(x)
      texto[is.na(x)] <- NA_character_
      datos[[j]] <- texto
    }
  }
  datos
}

# Mide y agrega una metrica por vez, para que el pico de memoria sea el de UNA
# metrica y no el de todas. Devuelve lo mismo que agregar el detalle entero con
# `.preparar_tablero()`: la agregacion agrupa por `metrica_instanciada`, asi que
# hacerla por partes no cambia ningun grupo. Dos cosas dependen de la medicion
# ENTERA y se resuelven al final: si hay varias entidades -cambia la etiqueta del
# objeto-, y el aviso de las metricas cuyo metodo fallo, que `medir()` emite una
# vez sobre el modelo y aca se emitiria una vez por metrica.
#
# Si ninguna metrica produjo medidas, devuelve la medicion vacia -con la
# cobertura de todas- para que `analizar()` siga el camino de siempre.
.medir_y_agregar_por_metrica <- function(modelo_completo, datos, id_medicion, fecha,
                                         aplicabilidad = NULL) {
  if (is.null(id_medicion)) id_medicion <- .nuevo_id_medicion(fecha)
  partes <- list()
  coberturas <- list()
  alcances <- list()
  entidades <- character()
  vacia <- NULL
  # Lo que `medir()` declara sobre la corrida -la fecha declarada, la
  # aplicabilidad, el marco- es igual en cada metrica y se toma de la primera.
  # La configuracion del modelo no: la de cada medicion parcial es la de UNA
  # metrica, y se declara la del modelo entero, como la da `medir()` sobre el.
  declarado <- NULL
  for (instancia in modelo_completo$metricas) {
    sola <- modelo(list(instancia), marco = modelo_completo$marco)
    medidas <- withCallingHandlers(
      medir(sola, datos, id_medicion = id_medicion, fecha = fecha,
            aplicabilidad = aplicabilidad),
      warning = function(w) {
        if (grepl("no se midieron porque su m", conditionMessage(w),
                  fixed = TRUE, useBytes = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    if (is.null(declarado)) {
      declarado <- .trasladar_atributos_medicion(list(), medidas)
    }
    cobertura <- attr(medidas, "cobertura_metricas", exact = TRUE)
    if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
      coberturas[[length(coberturas) + 1L]] <- cobertura
    }
    alcance <- attr(medidas, "alcance_medidas", exact = TRUE)
    if (inherits(alcance, "data.frame") && nrow(alcance)) {
      alcances[[length(alcances) + 1L]] <- alcance
    }
    if (!nrow(medidas)) {
      vacia <- medidas
      next
    }
    validadas <- .validar_medidas_tablero(medidas)
    configuracion <- .configuracion_agregaciones(validadas, NULL, NULL)
    entidades <- .identificadores_unicos(c(entidades, validadas$entidad))
    partes[[length(partes) + 1L]] <- list(
      una = .agregar_medidas_tablero(validadas, configuracion,
                                     varias_entidades = FALSE),
      varias = .agregar_medidas_tablero(validadas, configuracion,
                                        varias_entidades = TRUE)
    )
    rm(medidas, validadas)
  }
  cobertura_metricas <- if (length(coberturas)) do.call(rbind, coberturas)
  alcance_medidas <- if (length(alcances)) {
    unidos <- do.call(rbind, alcances)
    rownames(unidos) <- NULL
    unidos
  }
  .avisar_no_medibles(cobertura_metricas, length(modelo_completo$metricas))
  configuracion_modelo <- .configuracion_modelo_calidad(modelo_completo)
  if (!length(partes)) {
    attr(vacia, "cobertura_metricas") <- cobertura_metricas
    attr(vacia, "alcance_medidas") <- alcance_medidas
    attr(vacia, "configuracion_modelo") <- configuracion_modelo
    return(list(agregada = NULL, vacia = vacia))
  }
  cual <- if (length(entidades) > 1L) "varias" else "una"
  agregada <- do.call(rbind, lapply(partes, `[[`, cual))
  rownames(agregada) <- NULL
  agregada$id_medida <- .ids_medida(
    agregada$id_medicion, agregada$metrica_instanciada, "tablero-"
  )
  class(agregada) <- c("medicion", "data.frame")
  agregada <- .trasladar_atributos_medicion(agregada, declarado)
  attr(agregada, "configuracion_modelo") <- configuracion_modelo
  # Las dos coberturas de la primera metrica no son las de la corrida: se
  # declaran las de todas, que son las que `.armar_tablero()` publica.
  attr(agregada, "cobertura_metricas") <- cobertura_metricas
  attr(agregada, "alcance_medidas") <- alcance_medidas
  list(
    agregada = agregada, cobertura_metricas = cobertura_metricas,
    alcance_medidas = alcance_medidas
  )
}
