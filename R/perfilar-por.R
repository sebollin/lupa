#' Perfilar una tabla por grupos de filas
#'
#' Aplica [perfilar()] a cada grupo de filas por separado y devuelve los
#' hallazgos de todos los grupos en una sola tabla. Es la respuesta al formato
#' largo: una tabla donde cada fila describe un atributo distinto no es una
#' tabla, son muchas apiladas, y perfilarla como si fuera una sola mezcla
#' dominios que no tienen nada que ver entre sí.
#'
#' Dentro de cada grupo se descartan las columnas enteramente ausentes antes de
#' perfilar. En un modelo entidad-atributo-valor bien formado eso deja viva
#' exactamente la columna de valor que corresponde al atributo del grupo, y es
#' lo que evita informar como falta lo que es la forma del dato. Las
#' declaraciones que nombran columnas retiradas se recortan para ese grupo y el
#' nombre de cada recorte, con su motivo, se declara en
#' `cobertura_grupos$columnas_descartadas`. En `aplicabilidad` también se
#' recorta una regla si `all.vars()` de su fórmula menciona una columna original
#' que no está en la rebanada; una variable que no es columna de la tabla se
#' conserva porque puede venir legítimamente del entorno de la fórmula.
#'
#' La función no adivina cuál es la columna de agrupación: la declara quien
#' conoce el dato, igual que [perfilar()] no adivina claves ni jerarquías.
#'
#' @param datos Data frame a perfilar.
#' @param por Nombre de una columna atómica cuyos valores definen los grupos.
#'   Los ausentes forman un grupo propio, con la etiqueta `"(ausente)"`. Si la
#'   columna trae ese mismo texto como valor real, los dos caen en un solo grupo
#'   —no se pierde ninguna fila— y la colisión se declara en
#'   `cobertura_grupos`, con cuántas filas aporta cada una.
#' @param clave Nombres de columnas de identidad que se conservan en cada grupo
#'   aunque estén enteramente ausentes. Importa: sin la clave de entidad, el
#'   diagnóstico de filas duplicadas informa como duplicada cada repetición del
#'   valor del atributo.
#'
#'   **La declaración viaja al perfilado de cada grupo**, igual que si se pasara
#'   `clave` a [perfilar()]: la ley de Benford no corre sobre esas columnas y la
#'   cobertura publica que la clave fue declarada, en vez de deducir que «parece
#'   un identificador». Si la clave nombra la columna de agrupación, ese nombre
#'   se recorta antes de reenviarlo, porque en la rebanada esa columna ya no
#'   está.
#' @param min_filas Grupos con menos filas que este número no se perfilan y se
#'   declaran en la cobertura. El valor por omisión evita conclusiones sobre
#'   grupos donde ningún diagnóstico tiene soporte.
#' @param ... Argumentos enviados a [perfilar()] para cada grupo. Los que
#'   acotan el trabajo —`muestra` entre ellos— se aplican **dentro de cada
#'   grupo**, no sobre la tabla entera: con `muestra = 100` y grupos de 500
#'   filas, los diagnósticos que muestrean miran 100 de cada grupo. El alcance
#'   viaja en la evidencia de cada hallazgo, que declara sobre cuántos valores
#'   se midió; `n_filas_grupo` sigue siendo el tamaño del grupo, no el de la
#'   muestra.
#'
#' @return Data frame de clase `hallazgos_por_grupo` con las columnas de
#'   `hallazgos` de [perfilar()] precedidas por `grupo` y `n_filas_grupo`. El
#'   atributo `cobertura_grupos` declara los grupos no perfilados, las columnas
#'   enteramente ausentes y las declaraciones recortadas por grupo.
#'
#'   El atributo `etiquetas_personales` declara si la columna de agrupación
#'   lleva datos personales. Las etiquetas de grupo **son** valores de esa
#'   columna, así que la salida los publica —en los hallazgos y en las dos
#'   tablas de cobertura— aunque [perfilar()] enmascare esa misma columna. No se
#'   enmascaran aquí porque la etiqueta es el eje del resultado y sin ella los
#'   grupos no se distinguen; pero tampoco ocurre en silencio: se avisa al
#'   ejecutar y queda declarado en el objeto. Para que no se publiquen, agrupe
#'   por una columna seudonimizada. El atributo queda vacío cuando la columna no
#'   lleva datos personales, y también cuando `proteger_datos_personales` es
#'   `FALSE`, porque entonces ya está declarado que se quieren los valores. Si el
#'   léxico no reconoce el nombre de la columna, decláresela con
#'   `columnas_personales`: ese argumento —como `columnas_opcionales`, `clave`,
#'   `columnas_sin_ceros`, `columnas_no_negativas` y `aplicabilidad`— puede
#'   nombrar la columna de agrupación, y se recorta de lo que se envía a
#'   [perfilar()] para cada grupo, donde esa columna ya no está. El recorte se
#'   publica por grupo en `cobertura_grupos$columnas_descartadas`, junto con el
#'   nombre de la declaración y el motivo. Para `aplicabilidad`, también se
#'   recorta una regla cuando alguna variable de su fórmula —según
#'   `all.vars()`— es una columna original ausente de la rebanada; las
#'   referencias que no son columnas originales se conservan como referencias
#'   posibles al entorno de la fórmula.
#'
#'   El atributo `cobertura_diagnosticos` declara, **por grupo**, los
#'   diagnósticos que no se evaluaron y por qué. Cada grupo se perfila por
#'   separado, así que cada uno declina los suyos: una columna puede tener
#'   bastantes filas en un grupo y muy pocas en otro. Sin esa tabla, un grupo sin
#'   hallazgos se lee como un grupo sano, cuando puede ser un grupo sobre el que
#'   no se miró.
#'
#'   Las senales de numeracion densa y de identificacion se miden tambien sobre
#'   columnas `integer64` con comparaciones exactas. Si un grupo vuelve a
#'   conjeturar un centinela por la particion, el hallazgo se mueve a esta
#'   cobertura y no se publica como defecto del grupo.
#' @export
#' @seealso [perfilar()], [cobertura_analisis()]
#'
#' @examples
#' largo <- data.frame(
#'   entidad = rep(1:40, each = 2),
#'   atributo = rep(c("pais", "edad"), 40),
#'   valor = c(rbind(sample(c("UY", "AR"), 40, TRUE), as.character(20:59)))
#' )
#' hallazgos <- perfilar_por(largo, "atributo", clave = "entidad", min_filas = 10)
#' head(hallazgos[, c("grupo", "columna", "tipo_hallazgo")])
perfilar_por <- function(datos, por, clave = NULL, min_filas = 30L, ...) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data frame.", call. = FALSE)
  }
  datos <- .tabla_base(datos)
  if (!is.character(por) || length(por) != 1L || is.na(por)) {
    stop("`por` debe ser el nombre de una sola columna.", call. = FALSE)
  }
  indice_por <- .indice_nombre(por, names(datos))
  if (is.na(indice_por)) {
    stop("`por` nombra una columna inexistente: ", por, ".", call. = FALSE)
  }
  por <- names(datos)[[indice_por]]
  if (!is.atomic(datos[[por]])) {
    stop("`por` debe nombrar una columna atomica.", call. = FALSE)
  }
  if (!is.null(clave)) {
    if (!is.character(clave)) {
      stop("`clave` debe ser un vector de nombres de columnas.", call. = FALSE)
    }
    indices_clave <- .indice_nombre(clave, names(datos))
    desconocidas <- clave[is.na(indices_clave)]
    if (length(desconocidas)) {
      stop("`clave` nombra columnas inexistentes: ",
           paste(desconocidas, collapse = ", "), ".", call. = FALSE)
    }
    clave <- names(datos)[indices_clave]
  }
  min_filas <- as.integer(min_filas)
  if (length(min_filas) != 1L || is.na(min_filas) || min_filas < 1L) {
    stop("`min_filas` debe ser un entero positivo.", call. = FALSE)
  }

  etiquetas <- as.character(datos[[por]])
  ausentes <- is.na(datos[[por]])
  # Los ausentes forman un grupo propio con esta etiqueta. Si la columna trae el
  # literal `"(ausente)"` como valor real, los dos caen en el mismo grupo: no se
  # pierde ninguna fila, pero se publica un grupo que junta dos cosas distintas
  # sin decirlo. No se cambia la etiqueta -es la que documenta la funcion y la
  # que la gente lee- sino que se declara la colision, que es lo que faltaba.
  colision <- sum(!ausentes & etiquetas == "(ausente)")
  etiquetas[ausentes] <- "(ausente)"
  etiquetas_operativas <- .nombres_para_operar(etiquetas)
  niveles_operativos <- unique(etiquetas_operativas)
  grupos <- split(
    seq_len(nrow(datos)),
    factor(etiquetas_operativas, levels = niveles_operativos)
  )
  # `split()` nombra los grupos con los niveles del factor. Esos niveles son
  # claves de trabajo; la etiqueta publicada sigue siendo el valor original
  # de la primera fila del grupo.
  names(grupos) <- vapply(niveles_operativos, function(clave) {
    etiquetas[[match(clave, etiquetas_operativas)]]
  }, character(1L))

  # La etiqueta de cada grupo ES un valor de la columna `por`. Si esa columna
  # lleva datos personales, las etiquetas los publican -en los hallazgos y en
  # las dos tablas de cobertura- aunque `perfilar()` sobre la misma columna
  # enmascare su moda. La columna se recorta de cada rebanada antes de perfilar,
  # asi que la capa de proteccion nunca la ve y no puede taparlo.
  #
  # No se enmascara: agrupar por una columna es pedir explicitamente que la
  # salida se organice por sus valores, y una etiqueta ilegible dejaria el
  # resultado sin eje. Pero tampoco se hace en silencio, que es lo que pasaba.
  # Se avisa y se declara en el objeto, para que quien publique la salida sepa
  # que lleva.
  #
  # La clasificacion la decide `perfilar()` sobre esa sola columna, no una copia
  # de la regla aca: una regla duplicada se arregla en un lado y no en el otro,
  # que es como aparecio el defecto de `analizar_tiempo()`.
  extras <- list(...)
  proteger <- if ("proteger_datos_personales" %in% names(extras)) {
    isTRUE(extras$proteger_datos_personales)
  } else {
    isTRUE(eval(formals(perfilar)$proteger_datos_personales))
  }
  etiquetas_personales <- NULL
  if (proteger) {
    # El clasificador RAPIDO, no `perfilar()`. La primera version sondeaba con
    # `perfilar()` para no duplicar la regla, y medida sobre una columna de
    # documentos de 200.000 filas costaba **55,7 s**: casi un minuto agregado a
    # una funcion publica, y justo en el caso que el sondeo existe para cubrir.
    # `.columnas_personales_rapidas()` da el mismo veredicto en **0,52 s** -107
    # veces menos- y es la primitiva que ya usan los caminos de referencial y de
    # analisis, asi que la regla sigue viviendo en un solo lugar.
    clasificada <- tryCatch(
      .columnas_personales_rapidas(.seleccionar_columnas(datos, por),
                                   detalle = TRUE),
      error = function(e) NULL
    )
    # Y lo que el usuario DECLARA vale aunque el lexico no lo reconozca: una
    # columna nombrada en `columnas_personales` es personal porque el usuario lo
    # dice, que es la regla del paquete.
    declaradas <- if ("columnas_personales" %in% names(extras)) {
      as.character(extras$columnas_personales)
    } else character()
    if (.nombres_presentes(por, declaradas) &&
        (is.null(clasificada) || !nrow(clasificada))) {
      clasificada <- data.frame(columna = por, tipo = "declarada_por_el_usuario",
                                stringsAsFactors = FALSE)
    }
    if (!is.null(clasificada) && nrow(clasificada)) {
      etiquetas_personales <- data.frame(
        columna = por,
        tipo = as.character(clasificada$tipo[1L]),
        n_grupos = length(grupos),
        motivo = paste0(
          "Las etiquetas de grupo son valores de `", por, "`, clasificada como ",
          clasificada$tipo[1L], ". `perfilar()` enmascara esa columna; aca no, ",
          "porque la etiqueta es el eje del resultado. La salida lleva esos ",
          "valores: agrupe por una columna seudonimizada si no deben publicarse."
        ),
        stringsAsFactors = FALSE
      )
      cli::cli_alert_warning(.cli_literal(.marcar_para_exhibir(etiquetas_personales$motivo)))
    }
  }

  # La declaracion se recorta dentro de cada grupo, despues de retirar las
  # columnas enteramente ausentes de su rebanada. La `clave` declarada tiene que
  # VIAJAR al perfilado de cada grupo. Era un
  # argumento formal de esta funcion, asi que nunca llegaba a `extras` -que se
  # arma desde `...`- y el `perfilar()` de cada rebanada no sabia que esa columna
  # identifica filas. La unica guarda que quedaba en el camino agrupado era la de
  # la INFERENCIA, que solo reconoce claves densas: sobre una clave repartida en
  # un rango ancho -`sample(1:6000, 2000)`, lo normal en un padron- el paquete
  # publicaba `desviacion_benford` sobre la columna que el usuario declaro como
  # identidad, mientras que por la puerta de `perfilar()` los mismos datos
  # publican cero. Declarada no hay nada que adivinar, y ademas el motivo pasa a
  # ser el hecho -"la clave fue declarada"- en vez de una deduccion que el
  # paquete no hizo.
  clave_recortada <- character()
  if (!is.null(clave)) {
    claves_por <- clave[
      .nombres_para_operar(clave) %in% .nombres_para_operar(por)
    ]
    if (length(claves_por)) {
      clave_recortada <- paste0(
        claves_por,
        " [clave: la columna de agrupacion no esta en la rebanada]"
      )
    }
  }
  if (!is.null(clave) && !("clave" %in% names(extras))) {
    clave_grupo <- clave[
      !(.nombres_para_operar(clave) %in% .nombres_para_operar(por))
    ]
    if (length(clave_grupo)) extras$clave <- clave_grupo
  }

  indices_sin_por <- setdiff(seq_along(datos), indice_por)
  nombres_sin_por <- names(datos)[indices_sin_por]
  hallazgos <- list()
  cobertura <- list()
  if (colision > 0L) {
    cobertura[[length(cobertura) + 1L]] <- data.frame(
      grupo = "(ausente)",
      n_filas_grupo = length(grupos[["(ausente)"]]),
      motivo = paste0(
        "El grupo `(ausente)` junta ", sum(ausentes), " fila(s) con la columna ",
        "de agrupacion ausente y ", colision, " fila(s) cuyo valor real es el ",
        "texto `(ausente)`. Son dos cosas distintas bajo una sola etiqueta: ",
        "distingalas antes de leer este grupo."
      ),
      columnas_descartadas = NA_character_, stringsAsFactors = FALSE
    )
  }
  cobertura_diagnosticos <- list()

  # Se recorre por POSICION y no por nombre.
  #
  # `x[[""]]` devuelve `NULL` aunque el elemento exista: R no resuelve la cadena
  # vacia como nombre. Con `for (nombre in names(grupos))` el grupo de los
  # blancos `""` recibia `filas <- NULL`, quedaba con cero filas, caia en la
  # rama de `min_filas` y se publicaba como "El grupo tiene 0 filas". Medido
  # sobre 200 filas con un grupo de 30 blancos: **la suma de `n_filas_grupo`
  # daba 170**, y esas 30 filas no aparecian en ningun lado -ni perfiladas ni
  # declaradas-.
  #
  # Es la unica forma de indexar que no depende del contenido de los nombres.
  # Lo que una columna ES no depende de que filas se miren, y partir la tabla
  # crea huecos que son artefacto de la particion. Dos guardas de `perfilar()`
  # se apoyan en propiedades de la columna entera y se volvian a deducir desde
  # cada rebanada, donde ya no valen:
  #
  # - Benford excluye las columnas que parecen un identificador. Medido: 2.000
  #   identificadores permutados quedan excluidos por la puerta de tabla
  #   completa -"parece un identificador"- y recibian `desviacion_benford`
  #   `sospechoso` en dos de tres grupos, porque dentro de 800 filas la serie
  #   tiene huecos.
  # - La conjetura de centinelas se apaga sobre una secuencia entera densa,
  #   "para no llamar faltante a un codigo valido". Medido: `id = 1:2000` no
  #   produce `faltantes_disfrazados` en la tabla completa y el MISMO 999
  #   produce uno en un grupo, porque la rebanada ya no es densa.
  #
  # Las dos senales se calculan una vez sobre la tabla entera, cada una con el
  # criterio de su propia guarda, y lo que en el grupo salga de ellas se mueve
  # a la cobertura con el motivo verdadero: ni se publica ni se calla.
  columnas_identificadoras <- character()
  columnas_densas <- character()
  columnas_centinales_no_evaluables <- character()
  # El centinela que conjetura la COLUMNA ENTERA, por columna. Sin esto no se
  # puede distinguir el centinela que un grupo conjetura POR LA PARTICION -el
  # que la documentacion promete mover a la cobertura- del que la columna ya
  # tenia. Medido: sobre una columna donde la tabla entera conjetura `-9999`,
  # un grupo conjeturaba `-999` y el hallazgo salia publicado como defecto del
  # grupo, con la cobertura vacia.
  centinelas_de_la_columna <- list()
  hay_centinelas_declarados <- length(
    .sentinelas_numericos_declarados(extras$sentinelas_numericos)
  ) > 0L
  for (nombre_columna in nombres_sin_por) {
    valores <- datos[[nombre_columna]]
    es_integer64 <- inherits(valores, "integer64")
    if (!is.numeric(valores) && !es_integer64) next
    presentes <- valores[!is.na(valores)]
    if (!length(presentes)) next
    if (es_integer64 && !.bit64_disponible()) {
      columnas_centinales_no_evaluables <- c(
        columnas_centinales_no_evaluables, nombre_columna
      )
      next
    }
    # Se usa el mismo conjeturador que el perfil, sobre la columna entera: no
    # se reimplementa la regla, se la llama con otro universo.
    finitos <- suppressWarnings(as.numeric(presentes))
    finitos <- finitos[is.finite(finitos)]
    if (length(finitos) >= 20L) {
      cuartiles <- stats::quantile(
        finitos, probs = c(0.25, 0.75), names = FALSE, type = 7
      )
      centinela_entero <- .centinela_por_tres_senales(
        finitos, cuartiles[[2L]] - cuartiles[[1L]],
        extras$sentinelas_numericos,
        q1 = cuartiles[[1L]], q3 = cuartiles[[2L]]
      )
      centinelas_de_la_columna[[nombre_columna]] <-
        suppressWarnings(as.numeric(centinela_entero$valor))
    }
    # La senal es la MISMA que usa la guarda de Benford, calculada sobre la
    # columna entera en vez de sobre la rebanada. Un primer intento uso
    # `tasa_distintos >= 0.9`, y el control lo tumbo: los importes reales son
    # casi unicos, asi que esa regla le suprimia Benford justo a las magnitudes
    # que Benford describe. La condicion del paquete es una conjuncion -alta
    # unicidad Y forma de numeracion-, no una de las dos.
    if (isTRUE(.parece_correlativo_benford(valores))) {
      columnas_identificadoras <- c(columnas_identificadoras, nombre_columna)
    }
    secuencia <- if (es_integer64) {
      distintos <- sort(unique(presentes))
      rango <- max(distintos) - min(distintos) + bit64::as.integer64(1)
      limite_doble <- bit64::as.integer64("9007199254740991")
      densidad <- if (rango > limite_doble) 0 else {
        length(distintos) / as.numeric(rango)
      }
      list(
        densa = length(distintos) >= 20L && densidad >= 0.8,
        no_evaluada = FALSE
      )
    } else {
      tryCatch(
        .resumen_secuencia_entera(valores, list(tipo = "entero"), NULL),
        error = function(e) NULL
      )
    }
    if (isTRUE(secuencia$densa)) {
      columnas_densas <- c(columnas_densas, nombre_columna)
    }
  }

  # Los indices de la traza salen de perfilar la REBANADA, asi que son
  # posiciones dentro del grupo. Publicados tal cual, con `localizador =
  # "indice_fila"` y `alcance = "completo"`, apuntan contra la tabla que el
  # usuario tiene -la unica que tiene- a filas inocentes: en un grupo de 102
  # filas, el hallazgo de las filas 199 a 202 salia como 99 a 102. `filas`
  # tiene las posiciones originales de cada grupo, que es justo lo que falta.
  .traza_en_la_tabla_original <- function(trazas, filas_originales) {
    if (!is.list(trazas) || !length(trazas)) return(trazas)
    lapply(trazas, function(traza) {
      if (!is.list(traza) || !length(traza$indices_fila)) return(traza)
      indices <- traza$indices_fila
      validos <- !is.na(indices) & indices >= 1L &
        indices <= length(filas_originales)
      traducidos <- rep(NA_integer_, length(indices))
      traducidos[validos] <- as.integer(filas_originales[indices[validos]])
      traza$indices_fila <- traducidos
      traza
    })
  }

  for (indice_grupo in seq_along(grupos)) {
    nombre_grupo <- names(grupos)[[indice_grupo]]
    filas <- grupos[[indice_grupo]]
    if (length(filas) < min_filas) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = paste0(
          "El grupo tiene ", length(filas), " filas y `min_filas` es ",
          min_filas, ": no se perfilo."
        ),
        columnas_descartadas = NA_character_, stringsAsFactors = FALSE
      )
      next
    }
    rebanada <- .seleccionar_columnas(
      datos, nombres_sin_por, filas = filas
    )
    # Las columnas enteramente ausentes dentro del grupo se descartan: son las
    # que no corresponden a este atributo, y contarlas como falta era el defecto.
    # La clave declarada nunca se descarta, porque de ella dependen los
    # diagnosticos de unicidad.
    vacias <- vapply(rebanada, function(x) all(is.na(x)), logical(1L))
    indices_clave_rebanada <- if (length(clave)) {
      .indice_nombre(clave, names(rebanada))
    } else integer()
    conservadas <- seq_along(rebanada) %in%
      indices_clave_rebanada[!is.na(indices_clave_rebanada)]
    descartables <- names(rebanada)[vacias & !conservadas]
    if (length(descartables)) {
      rebanada <- .seleccionar_columnas(
        rebanada, which(!vacias | conservadas)
      )
    }
    extras_grupo <- extras
    recortes_declaracion <- clave_recortada
    registrar_recorte <- function(texto) {
      recortes_declaracion <<- c(recortes_declaracion, texto)
    }

    # Las declaraciones de columnas se validan contra la tabla original y se
    # recortan contra la rebanada. Así un nombre que nunca existió sigue
    # llegando a `perfilar()` para producir su error habitual, pero uno que sí
    # existe y fue retirado por ausencia estructural no aborta la corrida.
    for (arg in c("columnas_personales", "columnas_opcionales", "clave",
                  "columnas_sin_ceros", "columnas_no_negativas")) {
      if (!(arg %in% names(extras_grupo)) ||
          !is.character(extras_grupo[[arg]])) next
      declaradas <- extras_grupo[[arg]]
      indices_original <- .indice_nombre(declaradas, names(datos))
      indices_rebanada <- .indice_nombre(declaradas, names(rebanada))
      recortar <- !is.na(indices_original) & is.na(indices_rebanada)
      if (any(recortar)) {
        for (indice in which(recortar)) {
          registrar_recorte(paste0(
            declaradas[[indice]], " [", arg,
            ": la columna no esta en la rebanada]"
          ))
        }
        extras_grupo[[arg]] <- declaradas[!recortar]
      }
    }

    # El nombre del elemento de `aplicabilidad` no alcanza para saber si la
    # regla puede viajar: la formula tambien puede mencionar la columna de
    # agrupacion, que no esta en ninguna rebanada. `all.vars()` enumera esas
    # referencias, pero los nombres que no son columnas de la tabla original
    # se dejan intactos porque pueden vivir legitimamente en el entorno de la
    # formula.
    if ("aplicabilidad" %in% names(extras_grupo) &&
        is.list(extras_grupo$aplicabilidad) &&
        !is.null(names(extras_grupo$aplicabilidad))) {
      reglas <- extras_grupo$aplicabilidad
      nombres_reglas <- names(reglas)
      indices_destino_original <- .indice_nombre(nombres_reglas, names(datos))
      indices_destino_rebanada <- .indice_nombre(nombres_reglas, names(rebanada))
      conservar <- rep(TRUE, length(reglas))
      for (indice in seq_along(reglas)) {
        # Un destino que nunca fue columna de la tabla original debe seguir
        # llegando a `perfilar()` para que su validacion publique el error
        # contractual; no se lo puede silenciar por otra variable ausente de
        # la formula.
        if (is.na(indices_destino_original[[indice]])) next
        destino_falta <- !is.na(indices_destino_original[[indice]]) &&
          is.na(indices_destino_rebanada[[indice]])
        variables <- if (inherits(reglas[[indice]], "formula")) {
          tryCatch(all.vars(reglas[[indice]]), error = function(e) character())
        } else character()
        indices_variables_original <- .indice_nombre(variables, names(datos))
        indices_variables_rebanada <- .indice_nombre(variables, names(rebanada))
        variables_faltantes <- variables[
          !is.na(indices_variables_original) &
            is.na(indices_variables_rebanada)
        ]
        if (!destino_falta && !length(variables_faltantes)) next
        conservar[[indice]] <- FALSE
        detalles <- character()
        if (destino_falta) {
          detalles <- c(detalles, "la columna declarada no esta en la rebanada")
        }
        if (length(variables_faltantes)) {
          detalles <- c(
            detalles,
            paste0(
              "la formula referencia ",
              paste(unique(variables_faltantes), collapse = ", "),
              ", ausente(s) de la rebanada"
            )
          )
        }
        registrar_recorte(paste0(
          "aplicabilidad de `", nombres_reglas[[indice]], "` [",
          paste(detalles, collapse = "; "), "]"
        ))
      }
      extras_grupo$aplicabilidad <- reglas[conservar]
    }

    descartadas_publicadas <- c(descartables, recortes_declaracion)
    if (!ncol(rebanada)) {
      # El motivo dice cual de las dos cosas paso, porque no son la misma y
      # piden respuestas distintas. Con una tabla cuya unica columna es la de
      # agrupacion, la rebanada queda vacia sin que ninguna columna este
      # ausente, y el texto unico afirmaba que todas lo estaban: falso, y con
      # `columnas_descartadas` vacio, que lo desmentia en la misma fila.
      motivo_grupo <- if (length(descartables)) {
        "Todas las columnas del grupo estan enteramente ausentes."
      } else {
        paste(
          "No queda ninguna columna para perfilar en este grupo: la tabla no",
          "tiene mas columnas que la de agrupacion."
        )
      }
      if (length(recortes_declaracion)) {
        motivo_grupo <- paste(
          motivo_grupo,
          paste0(
            "Se recortaron ", length(recortes_declaracion),
            " declaraciones porque no aplican a la rebanada."
          )
        )
      }
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = motivo_grupo,
        columnas_descartadas = paste(descartadas_publicadas, collapse = ", "),
        stringsAsFactors = FALSE
      )
      next
    }
    perfil <- do.call(perfilar, c(list(rebanada), extras_grupo))
    # Cada grupo se perfila por separado, asi que cada uno declina sus propios
    # diagnosticos: una columna puede tener bastantes filas en un grupo y muy
    # pocas en otro. Sin juntar esas declaraciones, quien mira los hallazgos por
    # grupo no tiene forma de saber que sobre tal grupo no se miro, y leeria un
    # grupo sin hallazgos como un grupo sano.
    cb_grupo <- perfil$cobertura_diagnosticos
    if (inherits(cb_grupo, "data.frame") && nrow(cb_grupo)) {
      cobertura_diagnosticos[[length(cobertura_diagnosticos) + 1L]] <- cbind(
        data.frame(
          grupo = nombre_grupo, n_filas_grupo = length(filas),
          stringsAsFactors = FALSE
        ),
        cb_grupo
      )
    }
    # Lo que la tabla entera ya decidio sobre la columna manda sobre lo que la
    # rebanada deduce. El hallazgo no se borra: se mueve a la cobertura con el
    # motivo, que es lo que este mismo objeto hace con todo lo que no se midio.
    mover_a_cobertura <- function(marcados, diagnostico, motivo) {
      if (!any(marcados)) return(invisible(NULL))
      for (columna_afectada in unique(
        as.character(perfil$hallazgos$columna[marcados])
      )) {
        cobertura_diagnosticos[[length(cobertura_diagnosticos) + 1L]] <<-
          data.frame(
            grupo = nombre_grupo, n_filas_grupo = length(filas),
            diagnostico = diagnostico, columna = columna_afectada,
            motivo = motivo,
            como_resolverlo = paste(
              "Si esa columna es una magnitud y no una numeraci\u00f3n, perfilar",
              "el grupo por separado con `perfilar()`."
            ),
            dependencia = NA_character_, stringsAsFactors = FALSE
          )
      }
      perfil$hallazgos <<- perfil$hallazgos[!marcados, , drop = FALSE]
      invisible(NULL)
    }
    if (nrow(perfil$hallazgos) && length(columnas_identificadoras)) {
      mover_a_cobertura(
        grepl("benford", perfil$hallazgos$tipo_hallazgo) &
          .nombres_para_operar(perfil$hallazgos$columna) %in%
            .nombres_para_operar(columnas_identificadoras),
        "ley_benford",
        paste(
          "No aplica la ley de Benford: la columna parece un identificador en",
          "la tabla completa. Dentro de un grupo la serie tiene huecos y esa",
          "forma no se reconoce, pero ser un identificador no depende de qu\u00e9",
          "filas se miren."
        )
      )
    }
    if (nrow(perfil$hallazgos) && length(columnas_densas) &&
          !hay_centinelas_declarados) {
      # `faltantes` entra tambien, pero SOLO cuando su contenido es enteramente
      # disfrazado. Lo encontro una refutacion externa: sobre una columna densa
      # en la tabla completa, un grupo cuya densidad cae bajo el umbral emitia
      # `faltantes` -"0 ausentes reales y 4 disfrazados"- y ese tipo no estaba en
      # la lista, asi que las dos puertas discrepaban sobre las mismas cuatro
      # filas. Se exige que el grupo no tenga NINGUNA ausencia real: un faltante
      # de verdad no es de la particion y no se reubica.
      sin_ausencias_reales <- vapply(
        as.character(perfil$hallazgos$columna),
        function(columna_faltantes) {
          indice <- .indice_nombre(
            columna_faltantes, perfil$columnas$columna
          )
          if (is.na(indice)) return(FALSE)
          reales <- suppressWarnings(
            as.numeric(perfil$columnas$n_faltantes[[indice]])
          )
          disfrazados <- suppressWarnings(
            as.numeric(perfil$columnas$n_faltantes_disfrazados[[indice]])
          )
          isTRUE(is.finite(reales) && reales == 0 &&
                   is.finite(disfrazados) && disfrazados > 0)
        },
        logical(1L)
      )
      mover_a_cobertura(
        (perfil$hallazgos$tipo_hallazgo %in%
           c("faltantes_disfrazados", "posible_centinela_numerico") |
           (perfil$hallazgos$tipo_hallazgo == "faltantes" &
              sin_ausencias_reales)) &
          .nombres_para_operar(perfil$hallazgos$columna) %in%
            .nombres_para_operar(columnas_densas),
        "centinelas_numericos",
        paste(
          "La columna es una secuencia entera densa en la tabla completa, y",
          "sobre una numeraci\u00f3n el paquete no conjetura centinelas para no",
          "llamar faltante a un c\u00f3digo v\u00e1lido. Los huecos del grupo son de la",
          "partici\u00f3n, no de la columna."
        )
      )
    }
    if (nrow(perfil$hallazgos) && length(centinelas_de_la_columna)) {
      # Un grupo que conjetura OTRO centinela que la columna entera lo esta
      # conjeturando por la particion: es exactamente el caso que la
      # documentacion promete mover a la cobertura. El que coincide con el de
      # la columna no se toca: ese no es de la particion.
      por_particion <- vapply(seq_len(nrow(perfil$hallazgos)), function(i) {
        if (!identical(
          as.character(perfil$hallazgos$tipo_hallazgo[[i]]),
          "posible_centinela_numerico"
        )) {
          return(FALSE)
        }
        nombre <- as.character(perfil$hallazgos$columna[[i]])
        de_la_columna <- centinelas_de_la_columna[[nombre]]
        if (is.null(de_la_columna)) return(FALSE)
        indice <- .indice_nombre(nombre, perfil$columnas$columna)
        if (is.na(indice)) return(FALSE)
        del_grupo <- suppressWarnings(
          as.numeric(perfil$columnas$centinela_valor[[indice]])
        )
        if (!isTRUE(is.finite(del_grupo))) return(FALSE)
        !isTRUE(is.finite(de_la_columna)) || !isTRUE(del_grupo == de_la_columna)
      }, logical(1L))
      mover_a_cobertura(
        por_particion,
        "centinelas_numericos",
        paste(
          "El grupo conjetura un centinela distinto del que conjetura la",
          "columna entera: la diferencia es de la partici\u00f3n, no de la",
          "columna, y lo que una columna ES no depende de qu\u00e9 filas se miren."
        )
      )
    }
    if (nrow(perfil$hallazgos) &&
        length(columnas_centinales_no_evaluables)) {
      mover_a_cobertura(
        perfil$hallazgos$tipo_hallazgo %in% c(
          "faltantes_disfrazados", "posible_centinela_numerico"
        ) &
          .nombres_para_operar(perfil$hallazgos$columna) %in%
            .nombres_para_operar(columnas_centinales_no_evaluables),
        "centinelas_numericos",
        paste(
          "No se pudo medir la numeracion de la columna `integer64` sin su",
          "soporte exacto; el hallazgo del grupo no se publica para no",
          "conjeturar sobre una medida incompleta."
        )
      )
    }
    if (nrow(perfil$hallazgos)) {
      perfil$hallazgos$trazabilidad <- .traza_en_la_tabla_original(
        perfil$hallazgos$trazabilidad, filas
      )
      fila <- cbind(
        data.frame(
          grupo = nombre_grupo, n_filas_grupo = length(filas),
          stringsAsFactors = FALSE
        ),
        perfil$hallazgos
      )
      hallazgos[[length(hallazgos) + 1L]] <- fila
    }
    if (length(descartadas_publicadas)) {
      motivo_descartes <- character()
      if (length(descartables)) {
        motivo_descartes <- c(
          motivo_descartes,
          paste0(
            "Se descartaron ", length(descartables),
            " columnas enteramente ausentes en este grupo antes de perfilar."
          )
        )
      }
      if (length(recortes_declaracion)) {
        motivo_descartes <- c(
          motivo_descartes,
          paste0(
            "Se recortaron ", length(recortes_declaracion),
            " declaraciones porque nombraban columnas que no estan en la",
            " rebanada."
          )
        )
      }
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, n_filas_grupo = length(filas),
        motivo = paste(motivo_descartes, collapse = " "),
        columnas_descartadas = paste(descartadas_publicadas, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }

  salida <- if (length(hallazgos)) {
    do.call(rbind, hallazgos)
  } else {
    # El esquema NO depende del contenido. Sin hallazgos, la salida se quedaba
    # en `grupo` y `n_filas_grupo`: dos columnas en vez de trece, con la misma
    # clase. Quien lo consume no podia distinguir "no se miro" de "salio
    # limpio" por la forma del objeto -`x$tipo_hallazgo` devolvia NULL y
    # `x[, "columna"]` rompia-, y la documentacion promete las columnas de
    # `hallazgos()` precedidas por las dos del grupo, sin condicionarlo.
    cbind(
      data.frame(
        grupo = character(), n_filas_grupo = integer(),
        stringsAsFactors = FALSE
      ),
      .hallazgos_vacios()
    )
  }
  rownames(salida) <- NULL
  attr(salida, "cobertura_grupos") <- if (length(cobertura)) {
    do.call(rbind, cobertura)
  } else {
    data.frame(
      grupo = character(), n_filas_grupo = integer(), motivo = character(),
      columnas_descartadas = character(), stringsAsFactors = FALSE
    )
  }
  attr(salida, "cobertura_diagnosticos") <- if (length(cobertura_diagnosticos)) {
    do.call(rbind, cobertura_diagnosticos)
  } else {
    cbind(
      data.frame(
        grupo = character(), n_filas_grupo = integer(),
        stringsAsFactors = FALSE
      ),
      .cobertura_diagnosticos_vacia()
    )
  }
  attr(salida, "n_grupos") <- length(grupos)
  attr(salida, "columna_grupo") <- por
  # Vacia cuando la columna de agrupacion no lleva datos personales, o cuando la
  # proteccion se desactivo: en ese caso el usuario ya declaro que los quiere.
  attr(salida, "etiquetas_personales") <- if (is.null(etiquetas_personales)) {
    data.frame(columna = character(), tipo = character(),
               n_grupos = integer(), motivo = character(),
               stringsAsFactors = FALSE)
  } else etiquetas_personales
  class(salida) <- c("hallazgos_por_grupo", "data.frame")
  salida
}
