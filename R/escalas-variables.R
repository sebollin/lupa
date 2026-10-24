.escalas_variables <- c(
  "nominal", "ordinal", "discreta", "continua", "binaria", "temporal",
  "desconocida"
)

.normalizar_metadatos_variables <- function(metadatos, nombres) {
  if (is.null(metadatos)) {
    return(data.frame(columna = character(), stringsAsFactors = FALSE))
  }
  indices_columnas <- if (inherits(metadatos, "data.frame") &&
                          "columna" %in% names(metadatos) &&
                          is.character(metadatos$columna) &&
                          !anyNA(metadatos$columna)) {
    .indice_nombre(metadatos$columna, nombres)
  } else integer()
  if (!inherits(metadatos, "data.frame") || !"columna" %in% names(metadatos) ||
      anyNA(metadatos$columna) || length(indices_columnas) !=
      length(metadatos$columna) || anyNA(indices_columnas) ||
      anyDuplicated(indices_columnas)) {
    .detener("`metadatos` debe tener una fila unica por columna existente.",
         call. = FALSE)
  }
  permitidas <- c("columna", "escala", "rol", "confianza", "confirmada",
                  "unidad", "niveles")
  if (any(!names(metadatos) %in% permitidas)) {
    .detener("`metadatos` contiene campos no reconocidos.", call. = FALSE)
  }
  if ("escala" %in% names(metadatos) &&
      any(!is.na(metadatos$escala) & !metadatos$escala %in% .escalas_variables)) {
    .detener("`escala` contiene una escala no reconocida.", call. = FALSE)
  }
  if ("confirmada" %in% names(metadatos) &&
      (!is.logical(metadatos$confirmada) || anyNA(metadatos$confirmada))) {
    .detener("`confirmada` debe ser logica y no contener NA.", call. = FALSE)
  }
  if ("confianza" %in% names(metadatos) &&
      any(!is.na(metadatos$confianza) &
          (!is.finite(metadatos$confianza) |
             metadatos$confianza < 0 | metadatos$confianza > 1))) {
    .detener("`confianza` debe estar en [0, 1].", call. = FALSE)
  }
  metadatos$columna <- nombres[indices_columnas]
  metadatos
}

.propuesta_escala <- function(x, tipo_implicito) {
  if (.es_columna_compuesta(x)) return(list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE,
    evidencia = paste0(
      "La columna es compuesta; sus componentes deben separarse antes de ",
      "declarar una escala de medici\u00f3n."
    )
  ))
  # `measure` es una declaracion y se publica como tal. `"scale"` se lee como
  # `continua`, que es como SPSS presenta ese nivel; para una cuenta declarada
  # asi, `metadatos` permite corregir la escala.
  medida <- attr(x, "measure", exact = TRUE)
  if (is.character(medida) && length(medida) == 1L &&
      medida %in% c("nominal", "ordinal", "scale")) {
    escala <- c(nominal = "nominal", ordinal = "ordinal", scale = "continua")[[medida]]
    return(list(
      escala = escala, rol = if (escala %in% c("nominal", "ordinal")) {
        "categoria"
      } else "medida",
      confianza = 1, confirmada = TRUE,
      evidencia = paste0("Metadato de escala declarado: ", medida, ".")
    ))
  }
  if (is.ordered(x)) return(list(
    escala = "ordinal", rol = "categoria", confianza = 1, confirmada = TRUE,
    evidencia = "La clase ordered declara niveles y orden."
  ))
  if (is.factor(x)) return(list(
    escala = "nominal", rol = "categoria", confianza = 1, confirmada = TRUE,
    evidencia = "La clase factor declara un dominio sin orden."
  ))
  if (is.logical(x)) return(list(
    escala = "binaria", rol = "indicador", confianza = 1, confirmada = TRUE,
    evidencia = "La clase logical declara dos estados."
  ))
  if (inherits(x, c("Date", "POSIXt"))) return(list(
    escala = "temporal", rol = "fecha", confianza = 1, confirmada = TRUE,
    evidencia = "La clase de fecha declara semantica temporal."
  ))
  # Una unidad declara una MAGNITUD, no si es discreta o continua: `kg` y
  # `personas` son las dos unidades. Lo que decide entre las dos es el valor, y
  # aca se confirmaba igual: un peso en kilos con valores enteros salia
  # `discreta`, confirmada y con confianza 1, cuando la ayuda promete que una
  # propuesta basada solo en valores nunca queda confirmada. La unidad confirma
  # la escala solo cuando los valores no dejan alternativa: un decimal descarta
  # la cuenta, y una magnitud declarada con decimales es continua. Con valores
  # enteros -o sin ninguno- quedan las dos lecturas, una cuenta o una medida
  # redondeada, y la propuesta sale sin confirmar.
  unidad <- attr(x, "units", exact = TRUE)
  if (is.numeric(x) && !is.null(unidad)) {
    presentes <- x[!is.na(x) & is.finite(x)]
    if (any(presentes != floor(presentes))) {
      return(list(
        escala = "continua", rol = "medida", confianza = 1, confirmada = TRUE,
        evidencia = paste0(
          "La unidad declarada confirma una magnitud cuantitativa, y los ",
          "decimales observados descartan una cuenta."
        )
      ))
    }
    return(list(
      escala = if (length(presentes)) "discreta" else "desconocida",
      rol = "medida", confianza = if (length(presentes)) 0.85 else NA_real_,
      confirmada = FALSE,
      evidencia = paste0(
        "La unidad declarada confirma una magnitud cuantitativa, pero ",
        if (length(presentes)) {
          "con valores enteros puede ser una cuenta o una medida redondeada: "
        } else {
          "sin valores finitos no se ve si es una cuenta o una medida: "
        },
        "que sea discreta o continua no esta declarado."
      )
    ))
  }
  if (!is.null(attr(x, "labels", exact = TRUE))) return(list(
    escala = "nominal", rol = "categoria", confianza = 0.85,
    confirmada = FALSE,
    evidencia = "Las etiquetas sugieren categorias, pero no declaran orden."
  ))
  presentes <- tryCatch(x[!is.na(x)], error = function(e) NULL)
  distintos <- if (is.null(presentes)) {
    NA_integer_
  } else {
    tryCatch(length(unique(presentes)), error = function(e) NA_integer_)
  }
  # Algunas listas tipadas por controladores DBI -en particular un BLOB que
  # llega como `blob`/`vctrs_list_of` de `raw`- tienen un metodo `unique()` que
  # no puede comparar sus elementos en versiones viejas de vctrs. La escala no
  # se puede inferir sin esa cardinalidad, pero eso no debe abortar el perfil:
  # `is.na()` ya permitio contar y trazar los faltantes, y el tipo compuesto
  # queda declarado como desconocido.
  if (is.na(distintos)) return(list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE,
    evidencia = paste0(
      "El tipo de la columna no permite comparar sus valores para inferir una ",
      "escala de medicion."
    )
  ))
  # Una columna sin un solo valor observado no exhibe ninguna escala. Antes se
  # publicaba "continua" con confianza 0.65 -la misma cifra que sobre mil
  # observaciones- y una evidencia que describia el ALMACENAMIENTO como si
  # describiera los datos. Es la regla del paquete, no publicar lo que no se
  # pudo medir, aplicada a esta senal; y el vocabulario ya trae `desconocida`
  # para decirlo.
  if (!length(presentes)) return(list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE,
    evidencia = paste0(
      "La columna no tiene ningun valor observado: no hay con que inferir una ",
      "escala de medicion."
    )
  ))
  # `Inf == floor(Inf)` es TRUE por la aritmetica de los dobles, no porque `Inf`
  # sea un entero: una columna de solo `Inf` salia `discreta` con "Los valores son
  # enteros", y una con `-Inf` e `Inf`, `binaria` con "dos estados". Sin un valor
  # finito no hay escala que inferir.
  if (is.double(x) && !inherits(x, "integer64") &&
      !any(is.finite(presentes))) return(list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE,
    evidencia = paste0(
      "La columna no tiene ningun valor finito -solo `Inf` o `-Inf`-: no hay ",
      "con que inferir una escala de medicion."
    )
  ))
  if (tipo_implicito == "identificador") return(list(
    escala = "nominal", rol = "identificador", confianza = 0.9,
    confirmada = FALSE,
    evidencia = "La forma y cardinalidad sugieren un identificador; requiere confirmacion."
  ))
  if (tipo_implicito %in% c("fecha", "fecha-hora")) return(list(
    escala = "temporal", rol = "fecha", confianza = 0.85,
    confirmada = FALSE,
    evidencia = "Los valores son compatibles con fecha; el almacenamiento no lo declara."
  ))
  if (tipo_implicito == "logico" || distintos == 2L) return(list(
    escala = "binaria", rol = "indicador", confianza = 0.8,
    confirmada = FALSE,
    evidencia = "Se observaron dos estados; pueden ser codigos y requieren confirmacion."
  ))
  finitos <- if (is.double(x) && !inherits(x, "integer64")) {
    presentes[is.finite(presentes)]
  } else presentes
  if (is.integer(x) || (is.numeric(x) && length(finitos) &&
      all(finitos == floor(finitos)))) return(list(
    escala = "discreta", rol = "medida", confianza = 0.65,
    confirmada = FALSE,
    evidencia = "Los valores son enteros; tambien podrian ser codigos nominales."
  ))
  if (is.numeric(x)) return(list(
    escala = "continua", rol = "medida", confianza = 0.65,
    confirmada = FALSE,
    evidencia = "El almacenamiento admite decimales; la semantica no esta declarada."
  ))
  if (is.character(x)) return(list(
    escala = "nominal", rol = "categoria", confianza = 0.55,
    confirmada = FALSE,
    evidencia = "El texto sugiere categorias, pero puede ser texto libre."
  ))
  list(
    escala = "desconocida", rol = "desconocido", confianza = NA_real_,
    confirmada = FALSE, evidencia = "El tipo no aporta una escala de medicion."
  )
}

.metricas_por_escala <- function(escala, rol) {
  if (rol == "identificador") return("Unicidad; integridad referencial")
  switch(
    escala,
    nominal = "ValoresPosiblesPorExtension; Formato",
    ordinal = "ValoresPosiblesPorExtension; orden y niveles",
    discreta = "ValoresPosiblesPorComprension; distribucion",
    continua = "Distribucion; ErrorEstandar; Escala",
    binaria = "ValoresPosiblesPorExtension; NoNulo",
    temporal = "Formato; vigencia; oportunidad",
    "Requiere declaracion"
  )
}

#' Proponer escalas y roles de las variables
#'
#' Separa el tipo de almacenamiento, el tipo implícito y la escala de medición.
#' Una propuesta basada sólo en valores nunca queda confirmada: `1, 2, 3` puede
#' ser conteo, código o nivel. Las clases `ordered`, `factor`, `logical`, las
#' clases temporales y el metadato `measure` sí constituyen declaraciones.
#' `measure = "scale"` se publica como `continua`, la lectura que SPSS da a ese
#' nivel. Una unidad declarada —el atributo `units`— establece una magnitud,
#' pero no si es discreta o continua: confirma `continua` cuando los valores
#' traen decimales, y con valores enteros la propuesta queda sin confirmar,
#' porque pueden ser una cuenta o una medida redondeada.
#'
#' Con `perfil`, una columna declarada en su `columnas_codigo` es un código y
#' no una magnitud: en lugar de `discreta` o `continua` con rol `medida`, sale
#' `nominal`, con rol `categoria` y confirmada, porque la declaración es del
#' usuario. `metadatos` la puede corregir igual.
#'
#' `metadatos` permite confirmar o corregir la propuesta con una tabla editable.
#' Debe contener `columna` y puede incluir `escala`, `rol`, `confianza`,
#' `confirmada`, `unidad` y una columna de lista `niveles`. Las escalas válidas
#' son nominal, ordinal, discreta, continua, binaria, temporal y desconocida.
#'
#' Los niveles declarados y observados se conservan como columnas de lista. Los
#' niveles ausentes son una observación, no prueba de error. Si la evidencia de
#' dato personal activa la protección, los niveles concretos se protegen, y los
#' valores de esas columnas escritos en los niveles de OTRA columna —una
#' observación que cita la cédula— se tapan con el mismo piso que aplica
#' [perfilar()]. Sólo
#' las escalas nominal, ordinal y binaria guardan niveles observados; en las
#' demás, `n_niveles_observados` es `NA`, porque la cifra no aplica.
#'
#' Una columna numérica sin ningún valor finito —sólo `Inf` o `-Inf`— queda
#' `desconocida`: `Inf` no es un entero ni un estado.
#'
#' @param datos Tabla que se desea clasificar.
#' @param perfil Perfil opcional de los mismos datos.
#' @param metadatos Declaraciones opcionales por columna.
#' @param max_niveles Máximo de niveles guardados en cada lista.
#' @param muestra Máximo de valores usados para enumerar niveles no declarados;
#'   los niveles ausentes de factores y metadatos se verifican sobre toda la
#'   columna.
#' @param proteger_datos_personales Si se ocultan niveles concretos de columnas
#'   cuya clasificación activa protección automática. Véase [perfilar()]. Con
#'   `perfil`, se respeta la clasificación de ese perfil; sin `perfil`, se
#'   rehace aquí por léxico y por forma, y lo declarado entra por
#'   `columnas_personales`.
#' @param columnas_personales Columnas que traen datos personales, declaradas
#'   con la misma forma que acepta [perfilar()]: nombres de columna, o un vector
#'   con nombre donde el nombre es la columna y el valor es el tipo. **Es el
#'   único camino cuando no se pasa `perfil`**: sin perfil, la clasificación
#'   corre por léxico y por forma, así que una columna que sólo quien la conoce
#'   sabe personal —una edad, un legajo interno— se publicaría entera. Sólo
#'   tiene efecto con `proteger_datos_personales = TRUE`.
#'
#' @return Data frame S3 `clasificacion_variables`, editable y filtrable.
#' @export
#' @seealso [analizar()], [distribucion_valores()], [proponer_modelo()]
#'
#' @examples
#' d <- data.frame(
#'   prioridad = ordered(c("baja", "alta"), levels = c("baja", "media", "alta")),
#'   cantidad = c(1L, 2L)
#' )
#' clasificar_variables(d)
clasificar_variables <- function(datos, perfil = NULL, metadatos = NULL,
                                  max_niveles = 100L,
                                  muestra = 1e5,
                                  proteger_datos_personales = TRUE,
                                  columnas_personales = character()) {
  .validar_datos_tabla(datos)
  .validar_perfil_de(perfil, datos)
  datos <- .tabla_base(datos)
  if (!is.null(metadatos)) metadatos <- .tabla_base(metadatos)
  max_niveles <- .validar_entero_positivo(max_niveles, "max_niveles")
  limite <- .validar_muestra(muestra)
  .validar_bandera_proteccion(proteger_datos_personales)
  metadatos <- .normalizar_metadatos_variables(metadatos, names(datos))
  nombres_metadatos <- .nombres_para_operar(metadatos$columna)
  # Igual que en `distribucion_valores()`: sin perfil, la declaracion del
  # usuario es el unico camino para proteger una columna que el lexico no
  # reconoce, y esta puerta no la aceptaba.
  columnas_personales <- .normalizar_columnas_personales(
    columnas_personales, names(datos)
  )
  personales <- if (proteger_datos_personales) {
    .columnas_personales_rapidas(datos, perfil,
                                 declaradas = columnas_personales)
  } else character()
  filas <- vector("list", ncol(datos))
  niveles_declarados <- niveles_observados <- niveles_ausentes <-
    vector("list", ncol(datos))
  for (i in seq_along(datos)) {
    x <- datos[[i]]
    nombre <- names(datos)[[i]]
    tipo_implicito <- if (!is.null(perfil)) {
      perfil$columnas$tipo_inferido[[i]]
    } else if (is.character(x) || is.factor(x)) {
      inferir_tipo(x)$tipo
    } else .tipo_declarado(x)
    propuesta <- .propuesta_escala(x, tipo_implicito)
    # Declarada en `columnas_codigo` del perfil: es un codigo, no una magnitud,
    # asi que no se propone una escala de magnitud ni el rol de medida. La
    # declaracion es del usuario, como `metadatos`, y `metadatos` la sigue
    # pudiendo corregir.
    if (!is.null(perfil) && propuesta$escala %in% c("discreta", "continua") &&
        .nombres_para_operar(nombre) %in%
          .nombres_para_operar(as.character(perfil$meta[["columnas_codigo"]]))) {
      propuesta <- list(
        escala = "nominal", rol = "categoria", confianza = 1,
        confirmada = TRUE,
        evidencia = paste0(
          "Declarada en `columnas_codigo` del perfil: es un c\u00f3digo, ",
          "no una magnitud."
        )
      )
    }
    meta <- metadatos[
      nombres_metadatos == .nombres_para_operar(nombre), , drop = FALSE
    ]
    if (nrow(meta)) {
      declaracion <- FALSE
      if ("escala" %in% names(meta) && !is.na(meta$escala[[1L]])) {
        propuesta$escala <- meta$escala[[1L]]
        declaracion <- TRUE
      }
      if ("rol" %in% names(meta) && !is.na(meta$rol[[1L]])) {
        propuesta$rol <- as.character(meta$rol[[1L]])
        declaracion <- TRUE
      }
      if ("confirmada" %in% names(meta)) {
        propuesta$confirmada <- meta$confirmada[[1L]]
        declaracion <- TRUE
      } else if ("escala" %in% names(meta) && !is.na(meta$escala[[1L]])) {
        propuesta$confirmada <- TRUE
      }
      if ("confianza" %in% names(meta) && !is.na(meta$confianza[[1L]])) {
        propuesta$confianza <- meta$confianza[[1L]]
        declaracion <- TRUE
      } else if (declaracion && propuesta$confirmada) {
        propuesta$confianza <- 1
      }
      if (declaracion) {
        propuesta$evidencia <- "Escala o rol declarados en metadatos provistos."
      }
    }
    declarados <- if (nrow(meta) && "niveles" %in% names(meta) &&
      length(meta$niveles[[1L]])) {
      as.character(meta$niveles[[1L]])
    } else if (is.factor(x)) levels(x) else character()
    guardar_niveles <- propuesta$escala %in% c("nominal", "ordinal", "binaria")
    muestreo <- .muestrear_vector(x, limite)
    # Rendido ANTES de analizar, no despues: `.texto_analizable()` marca UTF-8
    # lo declarado `bytes` -a proposito- y entonces la declaracion ya no esta
    # cuando estos niveles se publican. Rendirlos al sellar la tabla no alcanza
    # porque para entonces el valor ya perdio la marca.
    muestra_segura <- .texto_analizable(
      .texto_publicable(muestreo$valores)
    )$valores
    muestra_texto <- as.character(muestra_segura[!is.na(muestra_segura)])
    observados <- if (!guardar_niveles) {
      character()
    } else if (is.factor(x)) {
      niveles_x <- levels(x)
      niveles_x[tabulate(as.integer(x), nbins = length(niveles_x)) > 0L]
    } else {
      unique(muestra_texto)
    }
    presentes_declarados <- if (length(declarados)) {
      valores_seguros <- .texto_analizable(x)$valores
      # Con la clave estable (`.identificadores_en()`): los valores ya estan
      # declarados UTF-8 y los niveles llegan como el usuario los dejo, y bajo
      # `C` `%in%` entre un `unknown` y un `UTF-8` traduce el primero desde
      # ASCII y no casa -un nivel observado salia ausente-.
      .identificadores_en(
        declarados, as.character(valores_seguros[!is.na(valores_seguros)])
      )
    } else logical()
    ausentes <- declarados[!presentes_declarados]
    observados <- unique(c(declarados[presentes_declarados], observados))
    n_declarados <- length(declarados)
    # Una escala que no guarda niveles no los cuenta: publicaba 0 para una columna
    # `1, 2, 3`, que se lee como "ningun nivel observado". `NA` dice que la cifra
    # no aplica.
    n_observados <- if (!guardar_niveles) {
      NA_integer_
    } else if (!is.null(perfil)) {
      perfil$columnas$n_distintos[[i]]
    } else if (is.factor(x)) {
      sum(tabulate(as.integer(x), nbins = length(levels(x))) > 0L)
    } else length(unique(muestra_texto))
    n_ausentes <- length(ausentes)
    truncado <- any(c(length(declarados), length(observados), length(ausentes)) >
                     max_niveles)
    declarados <- utils::head(declarados, max_niveles)
    observados <- utils::head(observados, max_niveles)
    ausentes <- utils::head(ausentes, max_niveles)
    if (.nombres_para_operar(nombre) %in% .nombres_para_operar(personales)) {
      proteger <- function(x) if (length(x)) "[valor protegido]" else character()
      declarados <- proteger(declarados)
      observados <- proteger(observados)
      ausentes <- proteger(ausentes)
    }
    niveles_declarados[[i]] <- declarados
    niveles_observados[[i]] <- observados
    niveles_ausentes[[i]] <- ausentes
    unidad <- if (nrow(meta) && "unidad" %in% names(meta)) {
      as.character(meta$unidad[[1L]])
    } else {
      # `.unidad_declarada()` y no `attr(...)[[1L]]`: el numerador de un
      # `symbolic_units` es un vector, y leer su primer elemento publicaba `km`
      # para una columna en `km/h` y hacia abortar la funcion entera con `m^2`.
      .unidad_declarada(x)
    }
    filas[[i]] <- data.frame(
      columna = nombre, tipo_almacenamiento = .tipo_declarado(x),
      tipo_implicito = tipo_implicito, escala_propuesta = propuesta$escala,
      rol = propuesta$rol, confianza = propuesta$confianza,
      evidencia = propuesta$evidencia, confirmada = propuesta$confirmada,
      unidad = unidad, n_niveles_declarados = n_declarados,
      n_niveles_observados = n_observados,
      n_niveles_ausentes = n_ausentes,
      niveles_muestreados = muestreo$muestreado,
      niveles_truncados = truncado,
      metricas_sugeridas = .metricas_por_escala(propuesta$escala, propuesta$rol),
      stringsAsFactors = FALSE
    )
  }
  resultado <- if (length(filas)) do.call(rbind, filas) else data.frame(
    columna = character(), tipo_almacenamiento = character(),
    tipo_implicito = character(), escala_propuesta = character(), rol = character(),
    confianza = numeric(), evidencia = character(), confirmada = logical(),
    unidad = character(), n_niveles_declarados = integer(),
    n_niveles_observados = integer(), n_niveles_ausentes = integer(),
    niveles_muestreados = logical(), niveles_truncados = logical(),
    metricas_sugeridas = character(),
    stringsAsFactors = FALSE
  )
  # Los tres son listas de valores del usuario que se PUBLICAN: viajan en el
  # objeto, se imprimen y llegan al HTML de `reportar()`. Lo declarado `bytes`
  # se rinde a la forma que muestra la consola. Sin esto, la celda del reporte
  # decia `a\u00f1o` para un valor que `print()` publica como `a\xc3\xb1o`, y
  # los bytes crudos quedaban incrustados en un documento UTF-8 -donde el
  # navegador los renderiza como el caracter-.
  #
  # Aparecio despues de creer que `clasificar_variables()` estaba limpio: la
  # comprobacion habia mirado solo las columnas de texto de primer nivel y estos
  # son columnas-lista.
  #
  # Y declarados: los niveles de un factor llegan como el usuario los dejo, y
  # bajo `C` un nivel UTF-8 sin marca hacia abortar `print()` de la tabla
  # ("invalid multibyte string" en `format.AsIs()`), que en una sesion UTF-8
  # se imprime; y al reves, un nivel con un byte latin1 suelto la hacia abortar
  # en una sesion UTF-8 y no bajo `C`. Lo legible se marca -no
  # cambia los bytes- y lo que no es UTF-8 valido se rinde como lo muestra la
  # consola, igual que lo declarado `bytes`.
  publicar <- function(x) {
    .declarar_utf8_valido(
      .publicar_sin_marca_ilegible(.texto_publicable(x)), marcas = "unknown"
    )
  }
  resultado$niveles_declarados <- I(lapply(niveles_declarados, publicar))
  resultado$niveles_observados <- I(lapply(niveles_observados, publicar))
  resultado$niveles_ausentes <- I(lapply(niveles_ausentes, publicar))
  rownames(resultado) <- NULL
  # El mismo piso que aplican `perfilar()`, `analizar()` y
  # `distribucion_valores()`. Proteger solo por columna dejaba publicar el
  # documento protegido escrito en OTRA columna: medido, los
  # niveles de `obs` traian `reclamo CI 5.311.832-3` -y en mayusculas en `obs2`-
  # mientras las tres hermanas lo tapaban sobre la misma tabla.
  identificantes <- .valores_identificantes(
    .valores_publicables_protegidos(datos, personales),
    .fechas_que_identifican(datos, personales)
  )
  if (length(identificantes)) {
    resultado <- .proteger_textos_salida(
      resultado, identificantes, intocables = names(datos)
    )
  }
  class(resultado) <- c("clasificacion_variables", "data.frame")
  resultado
}
