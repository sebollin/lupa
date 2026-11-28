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
#' lo que evita informar como falta lo que es la forma del dato. Lo mismo
#' vale para una columna enteramente ausente en toda la tabla, como la columna
#' de valor que ningún atributo del extracto usa: se descarta en cada grupo y
#' `cobertura_grupos$columnas_descartadas` la nombra, en vez de publicarse como
#' `faltantes` en cada uno. Como esa columna tampoco se pierde sin decirlo, el
#' atributo `hallazgos_tabla` la informa **una** vez, con el esquema de los
#' hallazgos de [perfilar()] y producida por [perfilar()] sobre esas columnas:
#' los umbrales, `aplicabilidad` y `columnas_opcionales` valen igual que allí, y
#' una columna de `NaN` conserva `valores_no_finitos`. La sugerencia dice las dos
#' lecturas posibles —un defecto de carga, o la columna de valor de un atributo
#' que el extracto no trae— y al ejecutar se avisa que hay filas ahí. Es un
#' atributo y no filas de la tabla de grupos para que quien suma `n_filas_grupo`,
#' cuenta grupos o separa por `grupo` no encuentre una fila que no es un grupo;
#' quien filtra por severidad usa `attr(x, "hallazgos_tabla")`, que se puede unir
#' con `rbind()` a los hallazgos por grupo. Como `cobertura_grupos`, es un
#' atributo del objeto: `subset()`, `[`, `merge()`, `dplyr::select()`, la
#' escritura a CSV y `rbind()` (que conserva sólo el del primero) no lo llevan
#' consigo. No incluye las columnas compuestas, y
#' la `clave` declarada sólo si ningún grupo llegó a perfilarse (si alguno lo
#' hizo, cada grupo ya la informa). Las
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
#' @param datos Data frame a perfilar. Sus nombres de columna tienen que ser
#'   distintos: cada grupo se arma por nombre, y con dos columnas iguales se
#'   perfilaría una en lugar de la otra. Si los hay, la función se detiene;
#'   renómbrelas, por ejemplo con `names(datos) <- make.unique(names(datos))`.
#' @param por Nombre de una columna atómica cuyos valores definen los grupos.
#'   Los ausentes forman un grupo propio, con la etiqueta `"(ausente)"`. Si la
#'   columna trae ese mismo texto como valor real **y además hay ausentes**, los
#'   dos caen en un solo grupo —no se pierde ninguna fila— y la colisión se
#'   declara en `cobertura_grupos`, con cuántas filas aporta cada una. Sin
#'   ausentes no hay colisión y no se declara ninguna.
#'
#'   La etiqueta de cada grupo **vuelve a su valor**. En una columna de dobles,
#'   `as.character()` usa 15 cifras significativas y dos valores distintos pueden
#'   escribirse igual —`1e17` y `1e17 + 32` dan los dos `"1e+17"`—; en ese caso la
#'   etiqueta lleva las cifras que hacen falta para distinguirlos
#'   (`"1.0000000000000003e+17"`), de modo que cada grupo corresponde a un valor y
#'   `as.numeric()` de la etiqueta recupera ese valor exacto. Para la enorme
#'   mayoría de los números la etiqueta es la de siempre.
#'
#'   Lo mismo vale para las otras clases: un `integer64` se etiqueta con sus
#'   cifras exactas, aun por encima de 2^53; un complejo, con sus dos partes;
#'   una fecha, con su escritura `AAAA-MM-DD`, y una fecha-hora, con su
#'   instante en UTC y el rótulo `UTC` —al segundo, con la fracción exacta si
#'   la tiene—, la misma escritura en cualquier sesión; la zona declarada de la
#'   columna la publica [perfilar()] en `zona_horaria_origen`. No con su número
#'   de días o de segundos.
#'
#'   Si la columna es un **factor**, un nivel declarado sin ninguna fila es un
#'   grupo de cero filas: no se perfila y se declara en `cobertura_grupos`, igual
#'   que cualquier grupo por debajo de `min_filas`. Eso incluye un nivel
#'   llamado `"(ausente)"` sin filas cuando la columna también tiene `NA`: los
#'   `NA` se perfilan como su propio grupo, y el nivel vacío se declara aparte.
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
#'   declaran en la cobertura. Tiene que ser un entero positivo: un valor
#'   fraccionario se rechaza, en vez de truncarlo y perfilar un grupo que
#'   estaba por debajo del umbral. El valor por omisión evita conclusiones sobre
#'   grupos donde ningún diagnóstico tiene soporte.
#' @param ... Argumentos enviados a [perfilar()] para cada grupo. Los que
#'   acotan el trabajo —`muestra` entre ellos— se aplican **dentro de cada
#'   grupo**, no sobre la tabla entera: con `muestra = 100` y grupos de 500
#'   filas, los diagnósticos que muestrean miran 100 de cada grupo. El alcance
#'   viaja en la evidencia de cada hallazgo, que declara sobre cuántos valores
#'   se midió; `n_filas_grupo` sigue siendo el tamaño del grupo, no el de la
#'   muestra. Un nombre abreviado vale lo mismo que el entero, como lo completa
#'   R al llamar a [perfilar()]: `proteger = FALSE` es
#'   `proteger_datos_personales = FALSE` también para las etiquetas de grupo.
#'   La bandera, `fecha` y los nombres se validan al entrar, aunque ningún grupo
#'   llegue a `min_filas`. Los demás valores los valida [perfilar()] al perfilar
#'   el primer grupo: si ninguno llega a `min_filas`, uno inválido no da error.
#'
#' @return Data frame de clase `hallazgos_por_grupo` con las columnas de
#'   `hallazgos` de [perfilar()] precedidas por `grupo` y `n_filas_grupo`. El
#'   atributo `cobertura_grupos` declara los grupos no perfilados, las columnas
#'   enteramente ausentes y las declaraciones recortadas por grupo. Su columna
#'   lógica `grupo_perfilado` dice de qué clase de grupo habla cada fila: hay
#'   filas sobre grupos que **sí** se perfilaron —la colisión de `"(ausente)"`,
#'   las columnas descartadas— y filas sobre grupos que no. Sin esa distinción
#'   `n_filas_grupo` no se podría sumar, porque un grupo perfilado aparece en
#'   los hallazgos y otra vez en la cobertura.
#'
#'   **Cómo reconciliar las filas.** Hay una fila por grupo y motivo, así que un
#'   grupo puede aparecer más de una vez; la cuenta se hace sobre grupos
#'   distintos. La suma de `n_filas_grupo` sobre los grupos distintos —los
#'   perfilados, que están en los hallazgos y en las filas con
#'   `grupo_perfilado = TRUE`, más los no perfilados— da las filas de la tabla.
#'   Para que la cuenta cierre, un grupo perfilado que no produjo ningún
#'   hallazgo también tiene su fila en `cobertura_grupos`, con
#'   `grupo_perfilado = TRUE` y ese motivo: si no, no estaría en ninguna de las
#'   dos tablas.
#'   El atributo `hallazgos_tabla` tiene las columnas de `hallazgos` de
#'   [perfilar()] sin las del grupo: son los hallazgos de la tabla entera, hoy
#'   los de una columna enteramente ausente. No entra en esa suma.
#'
#'   El atributo `etiquetas_personales` declara si la columna de agrupación
#'   lleva datos personales. Las etiquetas de grupo **son** valores de esa
#'   columna, así que la salida los publica —en los hallazgos y en las dos
#'   tablas de cobertura— aunque [perfilar()] enmascare esa misma columna. No se
#'   enmascaran aquí porque la etiqueta es el eje del resultado y sin ella los
#'   grupos no se distinguen; pero tampoco ocurre en silencio: se avisa al
#'   ejecutar y queda declarado en el objeto. Para que no se publiquen, agrupe
#'   por una columna seudonimizada. Lo mismo, con tipo
#'   `contiene_valor_protegido`, cuando la columna de agrupación no es personal
#'   pero alguna etiqueta contiene un valor protegido de otra columna —un
#'   proveedor `juanperezsrl` junto a un titular protegido «Juan Perez»—: la
#'   etiqueta se publica igual, y `n_grupos` cuenta cuántas lo contienen. Cada
#'   grupo se perfila sobre sus filas, pero la salida entera se barre además con
#'   los valores protegidos de toda la tabla: el titular que en la columna
#'   protegida sólo aparece en un grupo no se publica en la evidencia de otro. El
#'   atributo queda vacío cuando ninguna etiqueta lleva datos personales, y
#'   también cuando `proteger_datos_personales` es
#'   `FALSE`, porque entonces ya está declarado que se quieren los valores. Si el
#'   léxico no reconoce el nombre de la columna, decláresela con
#'   `columnas_personales`: ese argumento —como `columnas_opcionales`, `clave`,
#'   `columnas_sin_ceros`, `columnas_no_negativas`, `columnas_codigo` y
#'   `aplicabilidad`— puede
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
#'   Las señales de numeración densa y de identificación, y el centinela que la
#'   columna conjetura, se miden una sola vez sobre la columna entera con el
#'   mismo código que [perfilar()]: en columnas numéricas, en `integer64` con
#'   comparaciones exactas, y en texto y `factor` que se leen como números. Si
#'   un grupo vuelve a conjeturar un centinela por la partición, el hallazgo se
#'   mueve a esta cobertura y no se publica como defecto del grupo. "La columna
#'   entera" es la que [perfilar()] mide con la misma declaración: las filas que
#'   `aplicabilidad` deja fuera no cuentan para el centinela, la numeración ni
#'   la guarda de Benford. Y lo que la columna entera sí conjetura —una
#'   numeración cuya moda sobresale, o un candidato fuera de su rango— no es
#'   de la partición: el grupo lo publica.
#'
#'   Sobre una numeración densa, cada grupo cuenta los faltantes disfrazados
#'   con la guarda de la columna entera, como [perfilar()] sobre la tabla
#'   completa: un `999` dentro de `1:2000` es un código de la numeración y no
#'   se cuenta ni en `faltantes_disfrazados` ni en `faltantes`, aunque dentro
#'   del grupo la numeración tenga huecos. Lo que así se deja de contar se
#'   declara en esta cobertura, con el diagnóstico `centinelas_numericos`, y el
#'   motivo dice si contado habría hecho cruzar el umbral de `faltantes`. Las
#'   ausencias escritas con texto —un `"0"` declarado en `cadenas_ausencia`,
#'   por ejemplo— son del grupo y se siguen contando.
#'
#'   Las marcas de [aplicar()] —el atributo `marcas_limpieza`— se verifican una
#'   vez sobre la tabla entera y llegan a cada grupo, donde [perfilar()] vuelve
#'   a decidir su estado sobre las filas del grupo: el `faltantes` de la marca
#'   de grupo sale `ok` en un grupo cuyos duplicados están enteros. La salida
#'   no publica ese estado —no tiene `general`— ni lo nombra al imprimirse: una
#'   marca de grupo `desactualizada` sin ningún `NA` no tiene `faltantes` que
#'   lo diga, y solo se ve en `general$marcas_limpieza$estado` y en `print()`
#'   de [perfilar()] sobre la tabla entera.
#' @export
#' @seealso [perfilar()], [cobertura_analisis()]
#'
#' @examples
#' set.seed(1)
#' largo <- data.frame(
#'   entidad = rep(1:40, each = 2),
#'   atributo = rep(c("pais", "edad"), 40),
#'   valor = c(rbind(sample(c("UY", "AR"), 40, TRUE), as.character(20:59)))
#' )
#' hallazgos <- perfilar_por(largo, "atributo", clave = "entidad", min_filas = 10)
#' head(hallazgos[, c("grupo", "columna", "tipo_hallazgo")])
perfilar_por <- function(datos, por, clave = NULL, min_filas = 30L, ...) {
  if (!inherits(datos, "data.frame")) {
    .detener("`datos` debe ser un data frame.", call. = FALSE)
  }
  datos <- .tabla_base(datos)
  # Con dos columnas del MISMO nombre -lo que da `fread()` sobre un CSV con un
  # encabezado repetido- la rebanada de cada grupo se arma por nombre, asi que la
  # primera se perfilaba dos veces y la segunda nunca: un `columnas_duplicadas`
  # falso y un 75 % de faltantes callado. Se dice en la entrada.
  repetidos <- unique(names(datos)[duplicated(.nombres_para_operar(names(datos)))])
  if (length(repetidos)) {
    .detener(
      "`datos` tiene nombres de columna repetidos: ",
      paste(repetidos, collapse = ", "), ". `perfilar_por()` arma cada grupo ",
      "por nombre y no puede distinguirlas; renombrarlas antes.", call. = FALSE
    )
  }
  if (!is.character(por) || length(por) != 1L || is.na(por)) {
    .detener("`por` debe ser el nombre de una sola columna.", call. = FALSE)
  }
  indice_por <- .indice_nombre(por, names(datos))
  if (is.na(indice_por)) {
    .detener("`por` nombra una columna inexistente: ", por, ".", call. = FALSE)
  }
  por <- names(datos)[[indice_por]]
  if (!is.atomic(datos[[por]])) {
    .detener("`por` debe nombrar una columna at\u00f3mica.", call. = FALSE)
  }
  if (!is.null(clave)) {
    if (!is.character(clave)) {
      .detener("`clave` debe ser un vector de nombres de columnas.", call. = FALSE)
    }
    indices_clave <- .indice_nombre(clave, names(datos))
    desconocidas <- clave[is.na(indices_clave)]
    if (length(desconocidas)) {
      .detener("`clave` nombra columnas inexistentes: ",
           paste(desconocidas, collapse = ", "), ".", call. = FALSE)
    }
    clave <- names(datos)[indices_clave]
  }
  # Un `min_filas` fraccionario se truncaba sin decirlo: con 10,7 se perfilaba un
  # grupo de 10 filas. Los otros parametros enteros del paquete rechazan la
  # fraccion; este tambien.
  # Y `Inf` es un entero para `trunc()`, pero no para `as.integer()`: daba NA,
  # un aviso de R y "valor ausente donde TRUE/FALSE es necesario".
  min_filas <- .validar_entero_positivo(min_filas, "min_filas")

  # La etiqueta sale del VALOR y tiene que volver a el: con `as.character()` dos
  # dobles distintos que se escriben igual caian en un solo grupo, y el perfil
  # publicado no correspondia a ninguno de los dos. Ver
  # `.etiqueta_numero_reversible()`.
  etiquetas <- .etiqueta_numero_reversible(datos[[por]])
  # Un factor con `NA` como NIVEL (`addNA()`, `exclude = NULL`) da `is.na()`
  # FALSE en esas filas y `as.character()` NA: la etiqueta quedaba NA y
  # `perfilar_por()` abortaba con un error interno de nombres. Son ausentes.
  ausentes <- is.na(datos[[por]]) | is.na(etiquetas)
  # Los ausentes forman un grupo propio con esta etiqueta. Si la columna trae el
  # literal `"(ausente)"` como valor real, los dos caen en el mismo grupo: no se
  # pierde ninguna fila, pero se publica un grupo que junta dos cosas distintas
  # sin decirlo. No se cambia la etiqueta -es la que documenta la funcion y la
  # que la gente lee- sino que se declara la colision, que es lo que faltaba.
  # Una colision necesita LAS DOS COSAS. `colision` cuenta las filas cuyo valor real
  # es el literal; sin ausentes no hay dos cosas bajo una etiqueta, y la declaracion
  # salía igual afirmando "junta 0 fila(s) con la columna de agrupacion ausente y 40
  # fila(s) cuyo valor real es el texto `(ausente)`. Son dos cosas distintas". Una
  # declaracion que se dispara sin el hecho que declara es una afirmacion falsa, y
  # justo dentro del libro de cobertura, que existe para poder confiar en el.
  colision <- sum(!ausentes & etiquetas == "(ausente)")
  hay_colision <- colision > 0L && any(ausentes)
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
  extras <- .argumentos_para_perfilar(list(...))
  .validar_argumentos_para_perfilar(extras)
  proteger <- if ("proteger_datos_personales" %in% names(extras)) {
    isTRUE(extras$proteger_datos_personales)
  } else {
    isTRUE(eval(formals(perfilar)$proteger_datos_personales))
  }
  etiquetas_personales <- NULL
  valores_otras <- character()
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
    # Los valores protegidos de las DEMAS columnas, sobre la tabla entera. Sirven
    # para dos cosas: avisar si una etiqueta de grupo lleva uno, y barrer la
    # salida al final. Cada grupo se perfila sobre su rebanada y solo conoce los
    # protegidos de sus filas, asi que el titular que en la columna protegida esta
    # solo en el grupo A se publicaba exacto en la evidencia del grupo B,
    # mientras `perfilar()` sobre la misma tabla lo tapa.
    indices_otras <- setdiff(seq_along(datos), indice_por)
    otras <- .seleccionar_columnas(datos, names(datos)[indices_otras])
    sensibles_otras <- tryCatch(
      .columnas_personales_rapidas(
        otras, declaradas = declaradas,
        validadores = extras$validadores_personales
      ),
      error = function(e) character()
    )
    valores_otras <- .valores_identificantes(
      .valores_publicables_protegidos(otras, sensibles_otras),
      .fechas_que_identifican(otras, sensibles_otras)
    )
    if (!is.null(clasificada) && nrow(clasificada)) {
      etiquetas_personales <- data.frame(
        columna = por,
        tipo = as.character(clasificada$tipo[1L]),
        n_grupos = length(grupos),
        motivo = paste0(
          "Las etiquetas de grupo son valores de `", por, "`, clasificada como ",
          clasificada$tipo[1L], ". `perfilar()` enmascara esa columna; ac\u00e1 no, ",
          "porque la etiqueta es el eje del resultado. La salida lleva esos ",
          "valores: agrupe por una columna seudonimizada si no deben publicarse."
        ),
        stringsAsFactors = FALSE
      )
      cli::cli_alert_warning(.cli_literal(.marcar_para_exhibir(etiquetas_personales$motivo)))
    } else {
      # La columna de agrupacion no es personal, pero una etiqueta puede llevar
      # adentro el valor de otra que si lo es: con el titular "Juan Perez"
      # protegido, agrupar por un proveedor "juanperezsrl" publicaba esa etiqueta
      # sin aviso, mientras `perfilar()` sobre la misma tabla la tapa en la moda y
      # en los ejemplos del proveedor. Se decide igual
      # que arriba -la etiqueta se publica porque es el eje- y por eso tampoco
      # aca en silencio: se avisa y se declara. Las etiquetas son valores, asi
      # que se comparan con la regla fuerte, la de los valores y no la de la
      # prosa.
      etiquetas_grupo <- names(grupos)
      if (length(valores_otras) && length(etiquetas_grupo)) {
        tapadas <- .reemplazar_valores_protegidos(etiquetas_grupo, valores_otras)
        cambiadas <- !vapply(seq_along(etiquetas_grupo), function(i) {
          identical(charToRaw(etiquetas_grupo[[i]]), charToRaw(tapadas[[i]]))
        }, logical(1L))
        if (any(cambiadas)) {
          etiquetas_personales <- data.frame(
            columna = por,
            tipo = "contiene_valor_protegido",
            n_grupos = sum(cambiadas),
            motivo = paste0(
              sum(cambiadas),
              if (sum(cambiadas) == 1L) {
                " etiqueta de grupo de `"
              } else {
                " etiquetas de grupo de `"
              }, por, "` contienen ",
              "un valor de una columna protegida. `perfilar()` lo enmascara; ",
              "ac\u00e1 no, porque la etiqueta es el eje del resultado. La salida ",
              "lleva esas etiquetas: agrupe por una columna seudonimizada si no ",
              "deben publicarse."
            ),
            stringsAsFactors = FALSE
          )
          cli::cli_alert_warning(.cli_literal(.marcar_para_exhibir(etiquetas_personales$motivo)))
        }
      }
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
        " [clave: la columna de agrupaci\u00f3n no est\u00e1 en la rebanada]"
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
  # Que filas estan vacias se decide sobre la tabla entera. En la rebanada la
  # columna de agrupacion no esta, asi que una fila con solo el grupo se veria
  # vacia y no lo es.
  vacias_enteras <- logical(nrow(datos))
  vacias_enteras[.filas_vacias_perfil(datos, nrow(datos))] <- TRUE
  hallazgos <- list()
  cobertura <- list()
  grupos_perfilados <- character()
  filas_perfilados <- integer()
  indice_colision <- 0L
  if (hay_colision) {
    indice_colision <- length(cobertura) + 1L
    cobertura[[indice_colision]] <- data.frame(
      grupo = "(ausente)",
      # `grupo_perfilado` distingue las filas que hablan de un grupo que SI se
      # perfilo -esta, y la de columnas descartadas- de las que hablan de uno que no.
      # Sin esa distincion, `n_filas_grupo` no se puede sumar: el grupo aparecia en
      # `hallazgos` y aca, las dos veces con sus 40 filas, y el libro daba 120 sobre
      # 80 filas reales. Quien use la tabla para verificar que no se pierde ninguna
      # fila -el uso que esta funcion promete- suma las filas de los grupos NO
      # perfilados y eso tiene que cerrar contra las filas de la tabla.
      # Se pone al final, cuando se sabe: ver mas abajo. Aca no se puede afirmar,
      # porque el grupo `(ausente)` puede quedar por debajo de `min_filas` -o sin
      # ninguna columna que perfilar- y entonces NO se perfila. La primera version
      # la dejaba fija en TRUE y era falsa justo en ese caso.
      grupo_perfilado = NA,
      n_filas_grupo = length(grupos[["(ausente)"]]),
      motivo = paste0(
        "El grupo `(ausente)` junta ", sum(ausentes),
        if (sum(ausentes) == 1L) " fila con la columna " else " filas con la columna ",
        "de agrupaci\u00f3n ausente y ", colision,
        if (colision == 1L) " fila cuyo valor real es el " else " filas cuyo valor real es el ",
        "texto `(ausente)`. Son dos cosas distintas bajo una sola etiqueta: ",
        "conviene distinguirlas antes de leer este grupo."
      ),
      columnas_descartadas = NA_character_, stringsAsFactors = FALSE
    )
  }
  # Un nivel DECLARADO sin filas es un grupo de cero filas, o sea por debajo de
  # cualquier `min_filas` -que ni acepta 0-, y `min_filas` promete que lo que queda
  # por debajo se declara y no se omite sin decirlo. Los grupos se arman sobre los
  # valores OBSERVADOS, asi que un nivel sin observaciones no llegaba nunca a esa
  # rama: medido, con niveles `A, B, C, D` y filas solo en `A` y `B`, la salida decia
  # `n_grupos = 2` y las DOS tablas de cobertura quedaban vacias, de modo que nada
  # explicaba la diferencia entre los cuatro niveles que la columna declara y los dos
  # grupos que publica. El disparador es cualquier factor rebanado o leido con
  # niveles de mas.
  #
  # `n_grupos` sigue contando los grupos con filas, que es lo que la funcion perfila;
  # los niveles sin filas quedan en la cobertura, que es donde se leen las ausencias.
  if (is.factor(datos[[por]])) {
    declarados <- levels(datos[[por]])
    declarados <- declarados[!is.na(declarados)]
    # Contados sobre los VALORES, no sobre las etiquetas de grupo: un nivel
    # `"(ausente)"` sin filas quedaba tapado por el grupo de los ausentes, que
    # lleva esa misma etiqueta.
    observados <- unique(as.character(datos[[por]][!ausentes]))
    sin_filas <- declarados[
      !(.nombres_para_operar(declarados) %in% .nombres_para_operar(observados))
    ]
    for (nivel in sin_filas) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nivel, grupo_perfilado = FALSE, n_filas_grupo = 0L,
        motivo = paste0(
          "El nivel `", nivel, "` est\u00e1 declarado en la columna de agrupaci\u00f3n y no ",
          "tiene ninguna fila: no se perfil\u00f3."
        ),
        columnas_descartadas = NA_character_, stringsAsFactors = FALSE
      )
    }
  }
  cobertura_diagnosticos <- list()

  # Los UNICOS tipos de hallazgo que un grupo puede perder a manos de la columna
  # entera, agrupados por el diagnostico con que se declaran en la cobertura.
  # Son la salida de las tres decisiones que `perfilar()` toma mirando la
  # columna completa y que una rebanada no puede volver a tomar:
  #
  # - el identificador para la ley de Benford -> `desviacion_benford`;
  # - el centinela numerico que la columna conjetura ->
  #   `posible_centinela_numerico`;
  # - la numeracion densa, que es la guarda de los faltantes disfrazados
  #   NUMERICOS -> el `posible_centinela_numerico` que la rebanada conjetura
  #   sobre la numeracion. Los faltantes disfrazados no se mueven: el perfil del
  #   grupo los cuenta ya con la guarda de la columna entera, y lo que esa
  #   guarda protege no llega a `faltantes_disfrazados` ni a `faltantes` (ver
  #   `.disfrazados_con_la_columna_entera()`). `faltantes_disfrazados` queda en
  #   la lista solo para la columna `integer64` que no se puede medir sin
  #   `bit64`.
  #
  # Ningun otro tipo entra, y la lista es la que autoriza: `mover_a_cobertura()`
  # no saca del grupo un tipo que no este aca, lo marque quien lo marque.
  # `perfilar_por()` existe para cuando los grupos son poblaciones distintas, y
  # entonces lo que la columna entera es no dice nada sobre lo que un grupo
  # tiene. Medido con un grupo de importes `c(1:100, 1000)` y otro de codigos
  # `101:2000`: la columna entera es una numeracion, y mover por eso los
  # `outliers` sacaba de los hallazgos el `1000`, que es un extremo verdadero
  # de los importes. Lo mismo `posible_identificador` y las ausencias escritas
  # con texto (`SD`, `N/A`): cada grupo las reconoce con su vocabulario, y
  # nunca fueron una decision de numeracion. Callar un defecto verdadero
  # cuesta mas que informar de mas.
  tipos_de_columna_entera <- list(
    ley_benford = "desviacion_benford",
    centinelas_numericos = c(
      "posible_centinela_numerico", "faltantes_disfrazados"
    )
  )

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
  # Las dos senales, y el centinela que la columna conjetura, se calculan una
  # vez sobre la tabla entera con el codigo de `perfilar()`
  # (`.contexto_decisiones_columna()`), y lo que en el grupo salga de ellas se
  # mueve a la cobertura con el motivo verdadero: ni se publica ni se calla.
  # Vale igual para una columna numerica, una `integer64` y un texto o un
  # `factor` que se leen como numeros: la misma numeracion escrita como texto
  # no puede decidir distinto.
  columnas_identificadoras <- character()
  columnas_contexto <- character()
  columnas_densas <- character()
  columnas_numeracion <- character()
  efectivos_numeracion <- list()
  columnas_centinales_no_evaluables <- character()
  # El centinela que conjetura la COLUMNA ENTERA, por columna. Sin esto no se
  # puede distinguir el centinela que un grupo conjetura POR LA PARTICION -el
  # que la documentacion promete mover a la cobertura- del que la columna ya
  # tenia. Medido: sobre una columna donde la tabla entera conjetura `-9999`,
  # un grupo conjeturaba `-999` y el hallazgo salia publicado como defecto del
  # grupo, con la cobertura vacia.
  centinelas_de_la_columna <- list()
  sentinelas_columna <- if ("sentinelas_numericos" %in% names(extras)) {
    extras$sentinelas_numericos
  } else {
    eval(formals(perfilar)$sentinelas_numericos)
  }
  muestra_columna <- if ("muestra" %in% names(extras)) {
    extras$muestra
  } else {
    eval(formals(perfilar)$muestra)
  }
  # El umbral con que el perfil del grupo decide `faltantes`, para que la
  # cobertura pueda decir si lo protegido por la numeracion lo habria cruzado.
  umbral_faltantes <- if ("umbral_faltantes_sospechoso" %in% names(extras)) {
    extras$umbral_faltantes_sospechoso
  } else {
    eval(formals(perfilar)$umbral_faltantes_sospechoso)
  }
  # Por columna: con la forma por columna, una columna declarada no hace
  # declaradas a las demas.
  centinelas_declarados_en <- function(nombre_columna) {
    length(.sentinelas_numericos_declarados(
      .sentinelas_de_columna(sentinelas_columna, nombre_columna)
    )) > 0L
  }
  # "La columna entera" es la que `perfilar()` mide con la MISMA declaracion:
  # las filas que `aplicabilidad` deja fuera no son de la columna. Las tres
  # senales se calculaban sobre todos los valores presentes. Medido:
  # con `aplicabilidad = list(ingreso = ~ edad >= 18)`, el relleno 999999 de los
  # menores ganaba la conjetura de la columna entera, el 88888 del universo
  # -el que `perfilar()` publica, con 18 afectados- se declaraba "de la
  # particion" y desaparecia de `hallazgos` en los tres grupos. La mascara es la
  # de `.resolver_aplicabilidad()`, la misma funcion del perfil; para la guarda
  # de Benford, el universo es el de `.universo_del_resumen()`, que es el que
  # `perfilar()` le pasa a Benford. Una declaracion invalida no se resuelve aca:
  # la rechaza `perfilar()` en cada grupo, con su error.
  mascaras_universo <- tryCatch(
    .resolver_aplicabilidad(
      datos, names(datos), extras$columnas_opcionales %||% character(),
      extras$aplicabilidad
    )$mascaras,
    error = function(e) NULL
  )
  # El contexto se calcula de manera diferida: una columna que no produce en
  # ningun grupo un hallazgo condicionado no necesita una decision global para
  # reubicarlo. Cada columna que si la necesita se calcula una sola vez y se
  # conserva en estas listas para todos los grupos siguientes.
  contextos_calculados <- character()
  asegurar_contexto <- function(nombre_columna) {
    if (nombre_columna %in% contextos_calculados) return(invisible(NULL))
    contextos_calculados <<- c(contextos_calculados, nombre_columna)
    indice_columna <- .indice_nombre(nombre_columna, names(datos))
    valores <- datos[[nombre_columna]]
    mascara <- if (!is.na(indice_columna) &&
                   length(mascaras_universo) >= indice_columna) {
      mascaras_universo[[indice_columna]]
    } else NULL
    if (!is.null(mascara) && length(mascara) == length(valores) &&
        !all(mascara)) {
      valores[!as.logical(mascara)] <- NA
    }
    contexto <- .contexto_decisiones_columna(
      valores, .sentinelas_de_columna(sentinelas_columna, nombre_columna),
      muestra = muestra_columna,
      clave_declarada = "clave" %in% names(extras) &&
        .nombres_para_operar(nombre_columna) %in%
          .nombres_para_operar(extras$clave)
    )
    if (is.null(contexto$fila)) return(invisible(NULL))
    columnas_contexto <<- c(columnas_contexto, nombre_columna)
    if (isTRUE(contexto$es_identificador)) {
      columnas_identificadoras <<- c(
        columnas_identificadoras, nombre_columna
      )
    }
    secuencia <- contexto$secuencia
    cuantitativo <- contexto$cuantitativo
    centinela_entero <- if (is.null(cuantitativo)) NULL else {
      suppressWarnings(as.numeric(cuantitativo$centinela_valor))
    }
    if (isTRUE(is.finite(centinela_entero))) {
      centinelas_de_la_columna[[nombre_columna]] <<- centinela_entero
    } else if (inherits(valores, "integer64") && !.bit64_disponible()) {
      # Solo sin `bit64`, que es cuando la columna no se puede medir. Con
      # valores por encima de 2^53 la numeracion tampoco se evalua, pero ahi
      # `perfilar()` no cierra la guarda y cuenta los faltantes disfrazados:
      # el grupo los decide igual. Tratarlo como falta de soporte mandaba a la
      # cobertura, con un motivo falso, el `-9999` de un grupo de valores
      # chicos porque otro grupo traia identificadores de 64 bits.
      columnas_centinales_no_evaluables <<- c(
        columnas_centinales_no_evaluables, nombre_columna
      )
    }
    if (!is.null(contexto$guarda) &&
        isTRUE(contexto$guarda$guarda_cerrada)) {
      columnas_densas <<- c(columnas_densas, nombre_columna)
    }
    # Lo que la guarda de la columna entera deja contar, si la columna es una
    # numeracion densa: ver `.disfrazados_con_la_columna_entera()`. Con la guarda
    # cerrada no deja contar ninguno; abierta, solo los candidatos que la abren.
    if (!is.null(contexto$guarda) && isTRUE(secuencia$densa)) {
      columnas_numeracion <<- c(columnas_numeracion, nombre_columna)
      efectivos_numeracion[[length(efectivos_numeracion) + 1L]] <<-
        .numeros_na(contexto$guarda$sentinelas_efectivos)
    }
    invisible(NULL)
  }
  # La consulta que el perfil de cada grupo le hace a la columna entera, por
  # nombre. La columna se mide la primera vez que un grupo la pide.
  guarda_de_la_columna <- function(nombre_columna) {
    indice_columna <- .indice_nombre(nombre_columna, names(datos))
    if (is.na(indice_columna) || indice_columna == indice_por) return(NULL)
    nombre_columna <- names(datos)[[indice_columna]]
    asegurar_contexto(nombre_columna)
    posicion <- match(
      .nombres_para_operar(nombre_columna),
      .nombres_para_operar(columnas_numeracion)
    )
    if (is.na(posicion)) return(NULL)
    efectivos_numeracion[[posicion]]
  }

  # Los codigos de ceros o nueves que se repiten en la columna ENTERA y que,
  # por lo tanto, son marcas de ausencia tambien cuando un grupo los ve una sola
  # vez. Se calcula una vez por columna, cuando un grupo la pide.
  centinelas_fecha_calculadas <- list()
  centinelas_fecha_de_la_columna <- function(nombre_columna) {
    indice_columna <- .indice_nombre(nombre_columna, names(datos))
    if (is.na(indice_columna) || indice_columna == indice_por) return(NULL)
    nombre_columna <- names(datos)[[indice_columna]]
    if (!is.null(centinelas_fecha_calculadas[[nombre_columna]])) {
      return(centinelas_fecha_calculadas[[nombre_columna]])
    }
    valores <- datos[[indice_columna]]
    if (!(is.character(valores) || is.factor(valores))) return(NULL)
    mascara <- if (length(mascaras_universo) >= indice_columna) {
      mascaras_universo[[indice_columna]]
    } else NULL
    if (!is.null(mascara) && length(mascara) == length(valores) &&
        !all(mascara)) {
      valores[!as.logical(mascara)] <- NA
    }
    marca <- .centinelas_fecha_texto(valores)
    salida <- if (any(marca)) {
      unique(trimws(as.character(valores)[marca]))
    } else {
      character()
    }
    centinelas_fecha_calculadas[[nombre_columna]] <<- salida
    salida
  }

  # El sostenimiento de la regla de dominio de cada columna, medido con la tabla
  # ENTERA y no con la rebanada: la regla vive en `perfilar()`, que decide con
  # el nombre, el contenido y las demas columnas (`lat` solo sabe que es una
  # latitud si hay una `lng`). Se perfila la tabla una vez, la primera vez que un
  # grupo lo pide, capturando lo que `perfilar()` decide, y se conserva.
  decision_dominio <- NULL
  dominio_de_la_columna <- function(nombre_columna) {
    if (is.null(decision_dominio)) {
      decision_dominio <<- tryCatch({
        previo <- .fijar_contexto_por(NULL, NULL)
        on.exit(.restaurar_contexto_por(previo), add = TRUE)
        captura <- new.env(parent = emptyenv())
        .CONTEXTO_POR$captura_dominio <- captura
        # Los mismos argumentos que reciben los grupos (todos los de
        # `perfilar()` que el usuario paso: `muestra`, `aplicabilidad`,
        # `fecha`...), para que la tabla se mida tal como se perfila.
        argumentos <- extras[intersect(names(extras), names(formals(perfilar)))]
        argumentos$analizar_dependencias <- NULL
        suppressWarnings(suppressMessages(do.call(
          perfilar,
          c(list(datos), argumentos, list(analizar_dependencias = FALSE))
        )))
        as.list(captura)
      }, error = function(e) list())
    }
    decision_dominio[[.nombres_para_operar(nombre_columna)]]
  }

  # Los indices de la traza salen de perfilar la REBANADA, asi que son
  # posiciones dentro del grupo. Publicados tal cual, con `localizador =
  # "indice_fila"` y `alcance = "completo"`, apuntan contra la tabla que el
  # usuario tiene -la unica que tiene- a filas inocentes: en un grupo de 102
  # filas, el hallazgo de las filas 199 a 202 salia como 99 a 102. `filas`
  # tiene las posiciones originales de cada grupo, que es justo lo que falta.
  #
  # `indices_unidades` -la primera fila de cada forma del grupo, en
  # `casi_duplicados_vocabulario` y sus hermanos- es otro campo de indices de la
  # MISMA traza y sale de la misma rebanada: se traducia solo `indices_fila`, y
  # con grupos intercalados `indices_unidades = 39,37,1` nombraba, en la tabla
  # del usuario, filas de OTRO grupo. Se traducen todos los campos de indices
  # de fila por la misma regla.
  .traza_en_la_tabla_original <- function(trazas, filas_originales) {
    if (!is.list(trazas) || !length(trazas)) return(trazas)
    traducir <- function(indices) {
      validos <- !is.na(indices) & indices >= 1L &
        indices <= length(filas_originales)
      traducidos <- rep(NA_integer_, length(indices))
      traducidos[validos] <- as.integer(filas_originales[indices[validos]])
      traducidos
    }
    lapply(trazas, function(traza) {
      if (!is.list(traza)) return(traza)
      for (campo in c("indices_fila", "indices_unidades")) {
        if (length(traza[[campo]])) traza[[campo]] <- traducir(traza[[campo]])
      }
      traza
    })
  }

  # Las marcas de `aplicar()` se verifican una vez, sobre la tabla entera: la
  # huella es la de la columna completa.
  marcas_tabla <- .marcas_limpieza_con_estado(datos)
  for (indice_grupo in seq_along(grupos)) {
    nombre_grupo <- names(grupos)[[indice_grupo]]
    filas <- grupos[[indice_grupo]]
    if (length(filas) < min_filas) {
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, grupo_perfilado = FALSE,
        n_filas_grupo = length(filas),
        motivo = paste0(
          "El grupo tiene ", length(filas),
          if (length(filas) == 1L) " fila y `min_filas` es " else " filas y `min_filas` es ",
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
    # El atributo de `aplicar()` no sobrevive a la seleccion: sin el, cada grupo
    # diagnosticaba las marcas como columnas del usuario. Ver
    # `.marcas_limpieza_rebanada()`.
    rebanada <- .marcas_limpieza_rebanada(rebanada, marcas_tabla)
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
                  "columnas_sin_ceros", "columnas_no_negativas",
                  "columnas_codigo")) {
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
            ": la columna no est\u00e1 en la rebanada]"
          ))
        }
        extras_grupo[[arg]] <- declaradas[!recortar]
      }
    }
    if ("dominios" %in% names(extras_grupo) &&
        is.list(extras_grupo$dominios) &&
        !is.null(names(extras_grupo$dominios))) {
      declaradas <- extras_grupo$dominios
      indices_original <- .indice_nombre(names(declaradas), names(datos))
      indices_rebanada <- .indice_nombre(names(declaradas), names(rebanada))
      recortar <- !is.na(indices_original) & is.na(indices_rebanada)
      if (any(recortar)) {
        for (indice in which(recortar)) {
          registrar_recorte(paste0(
            names(declaradas)[[indice]],
            " [dominios: la columna no esta en la rebanada]"
          ))
        }
        extras_grupo$dominios <- declaradas[!recortar]
      }
    }
    # La declaracion de ausencias por columna, como `dominios`: una entrada de
    # una columna que la rebanada no tiene se recorta y se declara.
    for (arg in c("sentinelas_numericos", "cadenas_ausencia")) {
      declaradas <- extras_grupo[[arg]]
      if (!.es_ausencias_por_columna(declaradas) || !length(declaradas)) next
      indices_original <- .indice_nombre(names(declaradas), names(datos))
      indices_rebanada <- .indice_nombre(names(declaradas), names(rebanada))
      recortar <- !is.na(indices_original) & is.na(indices_rebanada)
      if (any(recortar)) {
        for (indice in which(recortar)) {
          registrar_recorte(paste0(
            names(declaradas)[[indice]], " [", arg,
            ": la columna no est\u00e1 en la rebanada]"
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
          detalles <- c(detalles, "la columna declarada no est\u00e1 en la rebanada")
        }
        if (length(variables_faltantes)) {
          detalles <- c(
            detalles,
            paste0(
              "la f\u00f3rmula referencia ",
              paste(unique(variables_faltantes), collapse = ", "),
              if (length(unique(variables_faltantes)) == 1L) {
                ", ausente de la rebanada"
              } else {
                ", ausentes de la rebanada"
              }
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
          "Todas las columnas del grupo est\u00e1n enteramente ausentes."
      } else {
        paste(
          "No queda ninguna columna para perfilar en este grupo: la tabla no",
          "tiene m\u00e1s columnas que la de agrupaci\u00f3n."
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
        grupo = nombre_grupo, grupo_perfilado = FALSE,
        n_filas_grupo = length(filas),
        motivo = motivo_grupo,
        columnas_descartadas = paste(descartadas_publicadas, collapse = ", "),
        stringsAsFactors = FALSE
      )
      next
    }
    grupos_perfilados <- c(grupos_perfilados, nombre_grupo)
    filas_perfilados <- c(filas_perfilados, length(filas))
    # El perfil del grupo cuenta los faltantes disfrazados con la guarda de la
    # numeracion de la columna entera: ver `.disfrazados_con_la_columna_entera()`.
    protegidos_grupo <- list()
    previo_contexto <- .fijar_contexto_por(
      guarda_de_la_columna, dominio_de_la_columna,
      centinelas_fecha_de_la_columna
    )
    .CONTEXTO_POR$filas_vacias_tabla <- vacias_enteras[filas]
    perfil <- tryCatch({
      perfil_grupo <- do.call(perfilar, c(list(rebanada), extras_grupo))
      protegidos_grupo <- .CONTEXTO_POR$protegidos
      perfil_grupo
    }, finally = .restaurar_contexto_por(previo_contexto))
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
    declarar_en_cobertura <- function(diagnostico, columna_afectada, motivo) {
      cobertura_diagnosticos[[length(cobertura_diagnosticos) + 1L]] <<-
        data.frame(
          grupo = nombre_grupo, n_filas_grupo = length(filas),
          diagnostico = diagnostico, columna = columna_afectada,
          motivo = motivo,
          como_resolverlo = paste(
            "Si esa columna es una magnitud y no una numeraci\u00f3n, perfilar",
            "el grupo por separado con `perfilar()`."
          ),
          dependencia = NA_character_,
          columnas = I(list(as.character(columna_afectada))),
          stringsAsFactors = FALSE
        )
      invisible(NULL)
    }
    mover_a_cobertura <- function(marcados, diagnostico, motivo) {
      # La lista de arriba es la que autoriza: un tipo que no pertenece a las
      # decisiones de columna entera no sale del grupo aunque venga marcado.
      marcados <- marcados & perfil$hallazgos$tipo_hallazgo %in%
        tipos_de_columna_entera[[diagnostico]]
      if (!any(marcados)) return(invisible(NULL))
      for (columna_afectada in unique(
        as.character(perfil$hallazgos$columna[marcados])
      )) {
        declarar_en_cobertura(diagnostico, columna_afectada, motivo)
      }
      perfil$hallazgos <<- perfil$hallazgos[!marcados, , drop = FALSE]
      invisible(NULL)
    }
    # Lo que la guarda de la numeracion de la columna entera dejo sin contar en
    # el perfil del grupo no es un hallazgo que se mueva -nunca se conto-, pero
    # se declara con lo que habria cambiado, en el mismo diagnostico que la
    # conjetura de centinelas. Una fila por columna: si algo del perfil midio la
    # misma columna dos veces, es la misma medida.
    if (length(protegidos_grupo)) {
      columnas_protegidas <- vapply(protegidos_grupo, function(registro) {
        as.character(registro$columna)
      }, character(1L))
      for (indice_registro in which(
        !duplicated(.nombres_para_operar(columnas_protegidas))
      )) {
        registro <- protegidos_grupo[[indice_registro]]
        declarar_en_cobertura(
          "centinelas_numericos", registro$columna,
          .motivo_numeracion_protege(registro, perfil, umbral_faltantes)
        )
      }
    }
    columnas_a_contextualizar <- unique(as.character(
      perfil$hallazgos$columna[perfil$hallazgos$tipo_hallazgo %in%
                                 unlist(tipos_de_columna_entera)]
    ))
    for (columna_contexto in columnas_a_contextualizar) {
      asegurar_contexto(columna_contexto)
    }
    if (nrow(perfil$hallazgos) && length(columnas_identificadoras)) {
      mover_a_cobertura(
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
    densas_sin_declarar <- columnas_densas[!vapply(
      columnas_densas, centinelas_declarados_en, logical(1L)
    )]
    if (nrow(perfil$hallazgos) && length(densas_sin_declarar)) {
      # Los faltantes disfrazados ya se contaron con la guarda de la columna
      # entera, dentro del perfil del grupo, y lo que esa guarda protege no
      # llego a ningun hallazgo: ni a `faltantes_disfrazados` ni a `faltantes`
      # (ver `.disfrazados_con_la_columna_entera()`). Antes se movian despues
      # esos dos hallazgos, y `faltantes` solo cuando era enteramente
      # disfrazado: con 4 `NA` y dos `999` en el grupo se quedaba sumando los
      # dos. Lo que queda de la numeracion es la conjetura de un centinela sobre
      # los huecos de la rebanada.
      mover_a_cobertura(
        perfil$hallazgos$tipo_hallazgo == "posible_centinela_numerico" &
          .nombres_para_operar(perfil$hallazgos$columna) %in%
            .nombres_para_operar(densas_sin_declarar),
        "centinelas_numericos",
        paste(
          "La columna es una secuencia entera densa en la tabla completa, y",
          "sobre una numeraci\u00f3n el paquete no conjetura centinelas para no",
          "llamar faltante a un c\u00f3digo v\u00e1lido. Los huecos del grupo son de la",
          "partici\u00f3n, no de la columna."
        )
      )
    }
    if (nrow(perfil$hallazgos) && length(columnas_contexto)) {
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
        if (!nombre %in% columnas_contexto) return(FALSE)
        # Sin medida de la columna entera no hay con que comparar: ese caso
        # tiene su propio motivo, mas abajo.
        if (nombre %in% columnas_centinales_no_evaluables) return(FALSE)
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
          "No se pudo medir la numeraci\u00f3n de la columna `integer64` sin su",
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
            if (length(descartables) == 1L) {
              " columna enteramente ausente en este grupo antes de perfilar."
            } else " columnas enteramente ausentes en este grupo antes de perfilar."
          )
        )
      }
      if (length(recortes_declaracion)) {
        motivo_descartes <- c(
          motivo_descartes,
          paste0(
            "Se recortaron ", length(recortes_declaracion),
            if (length(recortes_declaracion) == 1L) {
              " declaraci\u00f3n porque nombraba columnas que no est\u00e1n en la"
            } else " declaraciones porque nombraban columnas que no est\u00e1n en la",
            " rebanada."
          )
        )
      }
      cobertura[[length(cobertura) + 1L]] <- data.frame(
        grupo = nombre_grupo, grupo_perfilado = TRUE,
        n_filas_grupo = length(filas),
        motivo = paste(motivo_descartes, collapse = " "),
        columnas_descartadas = paste(descartadas_publicadas, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }

  # Una columna enteramente ausente en toda la tabla se descarta en cada grupo
  # (en un formato largo estrecho es la columna de valor de un atributo que el
  # extracto no trae), y entonces ningun hallazgo la nombraba mientras
  # `perfilar()` la informa. Se informa una vez, a nivel de tabla, en un atributo
  # aparte: una fila que no es un grupo no puede vivir en la tabla de grupos sin
  # romper a quien suma `n_filas_grupo`, cuenta grupos o los separa por `grupo`.
  hallazgos_tabla <- .hallazgos_columnas_vacias_en_la_tabla(
    datos, indices_sin_por, clave, extras,
    hay_perfilados = length(grupos_perfilados) > 0L
  )
  # Solo se anuncia lo que no es informativo: una columna declarada opcional
  # deja un `regla_silencia_ausencia` de severidad `ok` que no pide mirar nada.
  a_anunciar <- hallazgos_tabla[
    as.character(hallazgos_tabla$severidad) != "ok", , drop = FALSE
  ]
  if (nrow(a_anunciar)) {
    nombres_aviso <- unique(as.character(a_anunciar$columna))
    nombres_aviso[is.na(nombres_aviso)] <- "NA"
    mostrados <- utils::head(nombres_aviso, 5L)
    cli::cli_alert_warning(.cli_literal(.marcar_para_exhibir(paste0(
      length(nombres_aviso),
      if (length(nombres_aviso) == 1L) {
        " columna est\u00e1 enteramente ausente en toda la tabla y ning\u00fan grupo la perfila: "
      } else {
        " columnas est\u00e1n enteramente ausentes en toda la tabla y ning\u00fan grupo las perfila: "
      },
      paste0("`", .marcar_para_exhibir(mostrados), "`", collapse = ", "),
      if (length(nombres_aviso) > length(mostrados)) {
        paste0(" y ", length(nombres_aviso) - length(mostrados), " m\u00e1s")
      },
      ". Los hallazgos est\u00e1n en `attr(, \"hallazgos_tabla\")`."
    ))))
  }

  if (indice_colision > 0L) {
    cobertura[[indice_colision]]$grupo_perfilado <-
      "(ausente)" %in% grupos_perfilados
  }
  # Un grupo perfilado SIN hallazgos no aparecia en ninguna de las dos tablas, y
  # la reconciliacion que la documentacion promete -la suma de `n_filas_grupo`
  # sobre los grupos distintos da las filas de la tabla- no cerraba: daba 0 o 40
  # contra 80. Se publica con una fila que lo dice.
  con_hallazgos <- unique(unlist(lapply(hallazgos, function(h) {
    as.character(h$grupo)
  })))
  con_fila <- unique(unlist(lapply(cobertura, function(fila) {
    as.character(fila$grupo[fila$grupo_perfilado %in% TRUE])
  })))
  for (k in seq_along(grupos_perfilados)) {
    nombre <- grupos_perfilados[[k]]
    if (nombre %in% c(con_hallazgos, con_fila)) next
    cobertura[[length(cobertura) + 1L]] <- data.frame(
      grupo = nombre, grupo_perfilado = TRUE,
      n_filas_grupo = filas_perfilados[[k]],
      motivo = "El grupo se perfilo y no produjo hallazgos.",
      columnas_descartadas = NA_character_, stringsAsFactors = FALSE
    )
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
      grupo = character(), grupo_perfilado = logical(),
      n_filas_grupo = integer(), motivo = character(),
      columnas_descartadas = character(), stringsAsFactors = FALSE
    )
  }
  attr(salida, "hallazgos_tabla") <- hallazgos_tabla
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
  # Vacia cuando ninguna etiqueta lleva datos personales -ni la columna de
  # agrupacion es personal ni sus etiquetas contienen un valor protegido de otra-,
  # o cuando la proteccion se desactivo: en ese caso el usuario ya declaro que los
  # quiere.
  attr(salida, "etiquetas_personales") <- if (is.null(etiquetas_personales)) {
    data.frame(columna = character(), tipo = character(),
               n_grupos = integer(), motivo = character(),
               stringsAsFactors = FALSE)
  } else etiquetas_personales
  # El barrido con los protegidos de la tabla entera. Las etiquetas de grupo y los
  # nombres de columna quedan afuera: las etiquetas se publican por decision
  # -son el eje, y lo declara `etiquetas_personales`- y los nombres son
  # estructura.
  if (length(valores_otras)) {
    salida <- .proteger_textos_salida(
      salida, valores_otras,
      intocables = c(names(grupos), as.character(names(datos)))
    )
  }
  class(salida) <- c("hallazgos_por_grupo", "data.frame")
  salida
}

# Hallazgos de las columnas enteramente ausentes en toda la tabla, con el
# esquema de los de `perfilar()`. Salen de `perfilar()` aplicado a esas columnas
# (con los umbrales y las declaraciones del usuario) y no de un texto propio: la
# severidad, `valores_no_finitos` de una columna de `NaN`, la unidad y la traza
# son las que `perfilar()` publica sobre la tabla entera. Una columna compuesta
# (matriz, data frame anidado) queda fuera: `perfilar()` la informa con otro
# tipo. La clave declarada se nombra solo si ningun grupo se perfilo, porque si
# alguno lo hizo ya la informa. Para evaluar una regla de `aplicabilidad` sobre
# la tabla entera se incluyen las columnas que la formula menciona.
.hallazgos_columnas_vacias_en_la_tabla <- function(datos, indices, clave, extras,
                                                    hay_perfilados) {
  vacio <- .hallazgos_vacios()
  if (!nrow(datos) || !length(indices)) return(vacio)
  columnas <- .seleccionar_columnas(datos, indices)
  vacia <- vapply(columnas, function(x) {
    !is.data.frame(x) && is.null(dim(x)) && all(is.na(x))
  }, logical(1L))
  candidatas <- names(datos)[indices][vacia]
  if (hay_perfilados && length(clave)) {
    candidatas <- candidatas[
      !(.nombres_para_operar(candidatas) %in% .nombres_para_operar(clave))
    ]
  }
  if (!length(candidatas)) return(vacio)

  reglas <- extras$aplicabilidad
  if (!is.list(reglas) || is.null(names(reglas))) reglas <- list()
  reglas <- reglas[
    .nombres_para_operar(names(reglas)) %in% .nombres_para_operar(candidatas)
  ]
  mencionadas <- unique(unlist(lapply(reglas, function(f) {
    if (inherits(f, "formula")) all.vars(f) else character()
  })))
  mencionadas <- intersect(mencionadas, names(datos))
  incluidas <- unique(c(candidatas, mencionadas))
  rebanada <- .seleccionar_columnas(
    datos, .indice_nombre(incluidas, names(datos))
  )

  # Los mismos pasos que `perfilar()` da a estas columnas -perfilado por columna,
  # hallazgos de columna, ausencia estructural y traza-, sin el trabajo de tabla
  # (filas duplicadas, aritmetica entre columnas, huella de muestra) que no toca
  # a estos hallazgos y que con muchas columnas vacias costaba varias veces lo
  # que cuesta perfilar los grupos.
  arg <- function(nombre) {
    valor <- if (nombre %in% names(extras)) extras[[nombre]] else {
      eval(formals(perfilar)[[nombre]], environment(perfilar))
    }
    # Las declaraciones que nombran columnas se recortan a las de esta rebanada.
    if (nombre %in% c("columnas_opcionales", "columnas_sin_ceros",
                      "columnas_no_negativas", "columnas_codigo") &&
        is.character(valor)) {
      valor <- valor[
        .nombres_para_operar(valor) %in% .nombres_para_operar(incluidas)
      ]
    }
    valor
  }
  nombres <- names(rebanada)
  nombres_texto <- .marcar_utf8_textos(nombres)
  con_ausencias_propias <- function(nombre) {
    any(vapply(c("sentinelas_numericos", "cadenas_ausencia"), function(a) {
      declaracion <- arg(a)
      .es_ausencias_por_columna(declaracion) &&
        !is.na(.indice_nombre(nombre, names(declaracion)))
    }, logical(1L)))
  }
  opcionales <- arg("columnas_opcionales")
  aplicabilidad_resuelta <- .resolver_aplicabilidad(
    rebanada, nombres, opcionales, if (length(reglas)) reglas else NULL
  )
  codigo <- .normalizar_columnas_codigo(arg("columnas_codigo"), nombres)
  es_codigo <- .nombres_para_operar(nombres) %in% .nombres_para_operar(codigo)
  # Las columnas enteramente ausentes del mismo tipo, sin regla de aplicabilidad
  # ni declaracion de codigo, dan el mismo perfil salvo el nombre: se perfila una
  # y las demas la reutilizan. Con decenas de columnas de valor sin usar, perfilar
  # cada una costaba mas que perfilar los grupos.
  vistos <- list()
  resultados <- lapply(seq_along(rebanada), function(i) {
    x <- rebanada[[i]]
    # Una columna con declaracion propia de ausencias no comparte perfil con
    # otra del mismo tipo: la declaracion cambia lo que se cuenta.
    reutilizable <- is.null(aplicabilidad_resuelta$mascaras[[i]]) &&
      !es_codigo[[i]] && !con_ausencias_propias(nombres[[i]])
    clave_tipo <- paste(
      c(typeof(x), oldClass(x), if (is.factor(x)) levels(x)), collapse = "\r"
    )
    if (reutilizable && !is.null(vistos[[clave_tipo]])) {
      resultado <- vistos[[clave_tipo]]
      resultado$fila$columna <- nombres[[i]]
      return(resultado)
    }
    resultado <- .perfilar_columna(
      x, nombres[[i]], nrow(rebanada), arg("max_patrones"),
      arg("distinguir_mayusculas"), arg("expandir"), arg("umbral_patron_raro"),
      .sentinelas_de_columna(arg("sentinelas_numericos"), nombres[[i]]),
      aplicable = aplicabilidad_resuelta$mascaras[[i]],
      cadenas_ausencia = .cadenas_de_columna(arg("cadenas_ausencia"), nombres[[i]]),
      codigo = es_codigo[[i]],
      max_largo_valor_vocabulario = arg("max_largo_valor_vocabulario")
    )
    if (reutilizable) vistos[[clave_tipo]] <<- resultado
    resultado
  })
  columnas_perfil <- .apilar_filas(lapply(resultados, `[[`, "fila"))
  rownames(columnas_perfil) <- NULL
  h <- .hallazgos_columnas(
    resultados, nombres, arg("umbral_alta_cardinalidad"),
    arg("umbral_faltantes_sospechoso"), arg("umbral_faltantes_error"),
    arg("umbral_patron_raro"), arg("umbral_patron_dominante"),
    arg("columnas_sin_ceros"), arg("columnas_no_negativas"), datos = rebanada
  )
  h <- if (length(h)) do.call(rbind, h) else .hallazgos_vacios()
  h$severidad <- factor(
    as.character(h$severidad),
    levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  rownames(h) <- NULL
  if (isTRUE(arg("ausencia_estructural"))) {
    estructural <- .diagnosticar_ausencia_estructural(
      rebanada, nombres, resultados, aplicabilidad_resuelta,
      arg("umbral_faltantes_error"), nombres_texto = nombres_texto,
      marcas = .marcas_limpieza_reconocidas(.marcas_limpieza_con_estado(rebanada))
    )
    if (length(estructural$hallazgos)) {
      h <- .cruzar_faltantes_con_estructural(h, estructural$hallazgos)
      h <- do.call(rbind, c(list(h), estructural$hallazgos))
      h$severidad <- factor(
        as.character(h$severidad),
        levels = c("ok", "sospechoso", "error"), ordered = TRUE
      )
      rownames(h) <- NULL
    }
  }
  h <- .agregar_trazabilidad_hallazgos(
    h, rebanada, nombres, resultados, expandir = arg("expandir"),
    aproximados = NULL, limite = arg("max_filas_hallazgo"),
    distinguir_mayusculas = arg("distinguir_mayusculas"), clave = NULL
  )
  mantener <- h$tipo_hallazgo %in% c(
    "faltantes", "valores_no_finitos", "regla_silencia_ausencia"
  ) & (.nombres_para_operar(h$columna) %in% .nombres_para_operar(candidatas))
  h <- h[mantener, , drop = FALSE]
  if (!nrow(h)) return(vacio)
  es_faltante <- h$tipo_hallazgo == "faltantes"
  h$sugerencia[es_faltante] <- paste0(
    h$sugerencia[es_faltante],
    " Si la tabla est\u00e1 en formato largo, puede ser la columna de valor de ",
    "un atributo que este extracto no trae; `cobertura_grupos` la declara ",
    "descartada en cada grupo."
  )
  rownames(h) <- NULL
  h
}

# La guarda de la numeracion que `perfilar_por()` decide sobre la columna
# entera, para el perfil de cada grupo. Un estado de la llamada y no un
# argumento, como `.CONTEXTO_COPIAS`: la deteccion corre dentro de `perfilar()`,
# sobre la rebanada, y `perfilar()` no tiene -ni debe tener- un argumento para
# esto. Fuera de `perfilar_por()` esta vacio y nada cambia.
#
# - `guarda_columna`: funcion que recibe el nombre de una columna y devuelve los
#   centinelas que la guarda de la columna entera deja contar, o `NULL` si la
#   columna entera no es una numeracion densa (entonces decide la rebanada).
# - `protegidos`: lo que la guarda de la columna entera dejo sin contar en el
#   grupo que se esta perfilando, para declararlo en la cobertura.
.CONTEXTO_POR <- new.env(parent = emptyenv())
.CONTEXTO_POR$guarda_columna <- NULL
.CONTEXTO_POR$protegidos <- NULL
# - `centinelas_fecha_columna`: funcion que recibe el nombre de una columna y
#   devuelve los valores con forma de fecha hechos solo de ceros o de nueves
#   que se repiten en la columna entera, o `NULL` si no es una columna de texto.
#   La repeticion que convierte el codigo en marca de ausencia se mide sobre la
#   tabla, no sobre la rebanada de cada grupo.
.CONTEXTO_POR$centinelas_fecha_columna <- NULL
# - `filas_vacias_tabla`: mascara logica de la rebanada que dice cuales de sus
#   filas estan vacias en la tabla entera. Una fila con el grupo presente no es
#   vacia aunque falten todas las demas columnas.
.CONTEXTO_POR$filas_vacias_tabla <- NULL
# - `dominio_columna`: funcion que recibe el nombre de una columna y devuelve,
#   por tipo de hallazgo de dominio, el modo con que la tabla entera lo publica
#   (`fila` o `columna`), o `NULL` si no se pudo medir. El sosten de una regla
#   inferida se decide con la tabla entera, no con la proporcion de cada grupo.
.CONTEXTO_POR$dominio_columna <- NULL
# - `captura_dominio`: entorno donde `.evaluar_dominios()` deja lo que decide por
#   columna, mientras `perfilar_por()` mide la tabla entera.
.CONTEXTO_POR$captura_dominio <- NULL

.fijar_contexto_por <- function(guarda_columna, dominio_columna = NULL,
                                centinelas_fecha_columna = NULL) {
  previo <- list(
    guarda_columna = .CONTEXTO_POR$guarda_columna,
    protegidos = .CONTEXTO_POR$protegidos,
    dominio_columna = .CONTEXTO_POR$dominio_columna,
    captura_dominio = .CONTEXTO_POR$captura_dominio,
    centinelas_fecha_columna = .CONTEXTO_POR$centinelas_fecha_columna,
    filas_vacias_tabla = .CONTEXTO_POR$filas_vacias_tabla
  )
  .CONTEXTO_POR$guarda_columna <- guarda_columna
  .CONTEXTO_POR$dominio_columna <- dominio_columna
  .CONTEXTO_POR$captura_dominio <- NULL
  .CONTEXTO_POR$centinelas_fecha_columna <- centinelas_fecha_columna
  .CONTEXTO_POR$protegidos <- list()
  .CONTEXTO_POR$filas_vacias_tabla <- NULL
  invisible(previo)
}

.restaurar_contexto_por <- function(previo) {
  .CONTEXTO_POR$guarda_columna <- previo$guarda_columna
  .CONTEXTO_POR$protegidos <- previo$protegidos
  .CONTEXTO_POR$dominio_columna <- previo$dominio_columna
  .CONTEXTO_POR$captura_dominio <- previo$captura_dominio
  .CONTEXTO_POR$centinelas_fecha_columna <- previo$centinelas_fecha_columna
  .CONTEXTO_POR$filas_vacias_tabla <- previo$filas_vacias_tabla
  invisible(NULL)
}

# Sobre la tabla entera, una numeracion densa protege a sus valores: un `999`
# dentro de `1:2000` no se cuenta como faltante disfrazado, ni en
# `faltantes_disfrazados` ni en `faltantes`. Partida en grupos, la rebanada deja
# de ser densa, la guarda se abre y el grupo los contaba. Mover despues el
# hallazgo `faltantes_disfrazados` a la cobertura no alcanzaba: el hallazgo
# `faltantes` seguia sumandolos. Medido: un grupo de 50 filas con 4 `NA` y dos
# `999` publicaba "4 ausentes reales y 2 disfrazados (0.120 del total)" y
# cruzaba el umbral del 10 %, con la cobertura diciendo al lado que esos dos no
# eran faltantes.
#
# Por eso la decision entra al perfil del grupo, antes de contar: de los
# centinelas que el grupo contaria, solo quedan los que la guarda de la columna
# entera tambien deja contar -los que caen fuera de su rango o son su moda
# sobresaliente-, y todo lo que se cuenta a partir de ahi -los dos hallazgos,
# `n_faltantes_disfrazados`, la proporcion- sale de la misma mascara. Solo
# quita: un centinela que el grupo no cuenta no se agrega por la columna. Las
# ausencias escritas con texto no son de la numeracion y no se tocan.
#
# `detectar` vuelve a contar la columna con otra lista de centinelas; se pide
# solo cuando la lista cambia. Lo que se dejo de contar queda en
# `.CONTEXTO_POR$protegidos` para que `perfilar_por()` lo declare: nada se pierde
# en silencio.
.disfrazados_con_la_columna_entera <- function(disfrazados, nombre, guarda,
                                               sentinelas_declarados,
                                               detectar) {
  consultar <- .CONTEXTO_POR$guarda_columna
  # Una declaracion explicita atraviesa la guarda, en el grupo y en la columna
  # entera. Y si el grupo no cuenta ningun centinela, no hay nada que proteger:
  # la columna entera no se mide.
  if (is.null(nombre) || !is.function(consultar) ||
      length(sentinelas_declarados) || !isTRUE(guarda$detectar_sentinelas) ||
      !isTRUE(disfrazados$n_numericos > 0L)) {
    return(disfrazados)
  }
  de_la_columna <- consultar(nombre)
  if (is.null(de_la_columna)) return(disfrazados)
  efectivos <- .numeros_na(guarda$sentinelas_efectivos)
  conservados <- efectivos[efectivos %in% .numeros_na(de_la_columna)]
  if (length(conservados) == length(efectivos)) return(disfrazados)
  # Si ninguno de los que se quitan esta en el grupo, contar de nuevo da lo
  # mismo: la lista del grupo trae toda la de por omision aunque solo aparezca
  # el 999. Sin candidatos medidos no se sabe y se cuenta.
  quitados <- efectivos[!(efectivos %in% conservados)]
  candidatos <- .numeros_na(guarda$candidatos)
  if (length(candidatos) && !any(quitados %in% candidatos)) {
    return(disfrazados)
  }
  sin_protegidos <- detectar(conservados, length(conservados) > 0L)
  n_protegidos <- as.integer(
    disfrazados$n_numericos - sin_protegidos$n_numericos
  )
  if (!isTRUE(n_protegidos > 0L)) return(disfrazados)
  # La mascara que el grupo decide solo, para lo que no es un conteo de
  # faltantes: la casi-clave es un diagnostico propio del grupo y sale igual que
  # perfilando el grupo por separado (lo vigila
  # `test-perfilar-por-columna-entera.R`). Lo que cuenta faltantes -los dos
  # hallazgos, sus columnas y su traza- lee `mascara`.
  sin_protegidos$mascara_grupo <- disfrazados$mascara
  .CONTEXTO_POR$protegidos[[length(.CONTEXTO_POR$protegidos) + 1L]] <- list(
    columna = nombre, n = n_protegidos,
    valores = quitados[quitados %in% candidatos]
  )
  sin_protegidos
}

# El motivo con que `perfilar_por()` declara lo que la numeracion de la columna
# entera protegio en un grupo. Si sin esos valores el grupo queda debajo del
# umbral de `faltantes` y contados lo habria cruzado, lo dice: es la diferencia
# que el usuario ve en la tabla de hallazgos, y no puede quedar sin explicar.
.motivo_numeracion_protege <- function(registro, perfil, umbral) {
  n <- as.integer(registro$n)
  valores <- if (length(registro$valores)) {
    paste0(" (", paste(.etiqueta_numero_reversible(registro$valores),
                       collapse = ", "), ")")
  } else ""
  motivo <- paste0(
    "La columna es una secuencia entera densa en la tabla completa, y ah\u00ed ",
    "sus valores son c\u00f3digos de la numeraci\u00f3n, no faltantes disfrazados: ",
    n, if (n == 1L) " valor del grupo" else " valores del grupo", valores,
    if (n == 1L) " no se cuenta" else " no se cuentan",
    " ni en `faltantes_disfrazados` ni en `faltantes`."
  )
  indice <- .indice_nombre(registro$columna, perfil$columnas$columna)
  if (is.na(indice) || !is.numeric(umbral) || length(umbral) != 1L ||
      !is.finite(umbral)) {
    return(motivo)
  }
  n_aplicables <- suppressWarnings(
    as.numeric(perfil$columnas$n_aplicables[[indice]])
  )
  sin <- suppressWarnings(
    as.numeric(perfil$columnas$prop_faltantes_totales[[indice]])
  )
  publicado <- any(
    perfil$hallazgos$tipo_hallazgo == "faltantes" &
      .nombres_para_operar(perfil$hallazgos$columna) ==
        .nombres_para_operar(registro$columna),
    na.rm = TRUE
  )
  if (publicado || !isTRUE(n_aplicables > 0) || !isTRUE(is.finite(sin))) {
    return(motivo)
  }
  con <- sin + n / n_aplicables
  if (con > umbral && !(sin > umbral)) {
    motivo <- paste0(
      motivo, " Contados, los faltantes del grupo ser\u00edan ",
      .formatear_decimal_publicado(con), " del total; sin ellos son ",
      .formatear_decimal_publicado(sin), " y no superan el umbral (",
      .formatear_decimal_publicado(umbral),
      "), as\u00ed que `faltantes` no se publica."
    )
  }
  motivo
}
