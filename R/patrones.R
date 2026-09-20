## Tope de valores que se miran para decidir si los ejemplos se publican.
## Es `muestra_validadores`, el mismo que el paquete ya usa para clasificar.
.MUESTRA_CLASIFICACION_PATRONES <- 1000L

# El limite de trazabilidad conserva nombres, no la distribucion de frecuencias.
.limite_patrones_raros_trazabilidad <- 5000L

#' Descubrir patrones de formato
#'
#' Generaliza un vector de texto mediante la convención del *Pattern Finder*
#' de DataCleaner: `9` representa un dígito, `a` una letra minúscula y `A` una
#' letra mayúscula. Los símbolos y espacios se conservan literalmente.
#'
#' El cálculo aplica reemplazos vectorizados sobre el vector completo. Si el
#' vector supera `muestra`, usa una muestra sistemática reproducible y registra
#' esa decisión en los atributos del resultado.
#'
#' @param x Vector que se convertirá a texto.
#' @param proteger_datos_personales Si los ejemplos se enmascaran cuando la
#'   forma de los valores alcanza para clasificarlos como dato personal. `TRUE`
#'   por omisión. `perfilar()` lo pasa en `FALSE` porque protege el perfil
#'   entero después, respetando ahí la declaración del usuario.
#' @param distinguir_mayusculas Si es `TRUE`, distingue `a` de `A`.
#' @param expandir Si es `FALSE`, colapsa tokens repetidos (`9999` a `9+`).
#' @param max_patrones Número máximo de patrones que se muestran.
#' @param na.rm Si es `TRUE`, excluye los valores ausentes.
#' @param muestra Máximo de valores que se analizan.
#' @param umbral_raro Umbral usado para conservar un resumen acotado de
#'   patrones raros para los hallazgos.
#'
#' @return Un data frame de clase `patrones` con patrón, frecuencia, proporción
#'   y ejemplos. Los **ejemplos no se publican** cuando la forma de los valores
#'   alcanza por sí sola para clasificarlos como dato personal —un correo, por
#'   ejemplo—: salen como `[valor protegido]`. Acá llega un vector suelto, sin
#'   nombre de columna, así que la vía por nombre no está disponible; un número
#'   de ocho dígitos **sí** se publica, porque su forma sola no alcanza para
#'   afirmar que es un documento. Los atributos `total`, `analizados`, `filas_analizadas` y
#'   `muestreado` describen el posible muestreo; `filas_analizadas` es un alias
#'   explícito de `analizados` para mantener el alcance visible junto a otros
#'   diagnósticos. `resumen_patrones` conserva sólo el patrón dominante
#'   y hasta seis patrones raros para presentacion; nunca guarda la distribucion
#'   completa. `patrones_raros_trazabilidad` conserva solo los nombres de los
#'   patrones raros, hasta 5.000, para que la trazabilidad pueda enumerar filas
#'   sin retener frecuencias ni ejemplos. Las proporciones siempre estan en
#'   `[0, 1]`. `n_patrones_distintos` registra el total antes de truncar la tabla
#'   para informar omisiones sin retenerla. `n_patrones_raros` y
#'   `n_patrones_raros_trazabilidad` registran cuantos patrones raros habia antes
#'   de sus respectivos limites. `n_filas_patrones_no_dominantes_excluidos`
#'   registra cuantas filas pertenecen a patrones no dominantes cuya proporcion
#'   no es rara y que por eso quedan fuera del hallazgo.
#' @export
#' @seealso [perfilar()], [inferir_tipo()], [detectar_formatos_fecha()]
#'
#' @examples
#' descubrir_patrones(
#'   c("2020-01-31", "2021-12-01", "31/01/2020"),
#'   expandir = TRUE
#' )
descubrir_patrones <- function(x,
                               distinguir_mayusculas = TRUE,
                               expandir = FALSE,
                               max_patrones = 20,
                               na.rm = TRUE,
                               muestra = 1e5,
                               umbral_raro = 0.05,
                               proteger_datos_personales = TRUE) {
  if (length(max_patrones) != 1L || is.na(max_patrones) || max_patrones < 1) {
    stop("`max_patrones` debe ser un entero positivo.", call. = FALSE)
  }
  if (!is.atomic(x) && !is.factor(x)) {
    stop("`x` debe ser un vector at\u00f3mico.", call. = FALSE)
  }
  if (length(umbral_raro) != 1L || is.na(umbral_raro) ||
      umbral_raro < 0 || umbral_raro > 1) {
    stop("`umbral_raro` debe estar entre 0 y 1.", call. = FALSE)
  }

  muestra_x <- .muestrear_vector(x, muestra)
  # Lo declarado `bytes` se rinde a su forma imprimible ANTES de analizar, y no
  # despues. `.texto_analizable()` lo marcaria UTF-8 -a proposito, para que
  # `tolower()` y las expresiones regulares trabajen- y entonces la marca ya no
  # estaria al publicar: el perfil mostraba `a\u00f1o` en `patron` y en
  # `ejemplos` mientras `print()` del mismo dato mostraba `a\xc3\xb1o`. Dos
  # salidas del paquete sobre el mismo valor.
  #
  # Rendido antes, el patron, los ejemplos y la consola dicen lo mismo, que es
  # lo que corresponde para algo que se PUBLICA. Para un valor que su duenio
  # declaro que no es texto, su forma imprimible es la unica lectura honesta.
  # Sobre todo lo demas no cambia nada.
  valores <- .texto_analizable(.texto_publicable(muestra_x$valores))$valores
  es_na <- is.na(valores)
  if (na.rm) {
    valores <- valores[!es_na]
    es_na <- rep(FALSE, length(valores))
  }

  textos <- as.character(valores)
  indices_validos <- which(!es_na)
  patrones <- rep(NA_character_, length(textos))

  if (length(indices_validos)) {
    generalizados <- textos[indices_validos]
    generalizados <- .generalizar_a_patron(
      generalizados, distinguir_mayusculas, expandir
    )
    patrones[indices_validos] <- generalizados
  }

  marcador_na <- "\001valor_ausente\001"
  para_tabla <- patrones
  para_tabla[is.na(para_tabla)] <- marcador_na
  # `sort(table(...))` desempata con el orden de los niveles, y `table()` los
  # ordena con la intercalacion del locale: con `max_patrones = 2` y un empate
  # por el segundo puesto, la MISMA tabla publicaba `A+ a+` en una maquina y
  # `A+ Aa` en otra. No es orden de filas: son patrones distintos, y con ellos
  # cambian los ejemplos publicados y el orden en que `patron_raro` lista los
  # desvios.
  #
  # El paquete ya arreglo esta familia una vez -el recorte de pares de
  # duplicados, que usa `.ordenar_por_bytes()`- y ya usa `method = "radix"` para
  # el desempate de la moda en `R/columnas.R:30`. El arreglo no habia llegado
  # aca. El desempate va por bytes: no depende de la maquina y es total.
  conteo <- table(para_tabla, useNA = "no")
  orden <- order(-as.integer(conteo),
                 .clave_bytes(names(conteo)), method = "radix")
  frecuencias <- conteo[orden]
  denominador <- length(para_tabla)
  proporciones <- if (denominador) {
    as.numeric(frecuencias) / denominador
  } else {
    numeric()
  }
  limite <- min(length(frecuencias), floor(max_patrones))
  indices_salida <- seq_len(limite)
  indices_raros <- which(
    seq_along(frecuencias) > 1L & proporciones < umbral_raro
  )
  indices_no_dominantes_excluidos <- which(
    seq_along(frecuencias) > 1L & proporciones >= umbral_raro
  )
  n_filas_patrones_no_dominantes_excluidos <- if (
    length(indices_no_dominantes_excluidos)
  ) {
    as.integer(sum(as.numeric(frecuencias[indices_no_dominantes_excluidos])))
  } else {
    0L
  }
  n_patrones_raros <- length(indices_raros)
  nombres_raros <- names(frecuencias)[indices_raros]
  nombres_raros_trazabilidad <- nombres_raros[
    seq_len(min(length(nombres_raros), .limite_patrones_raros_trazabilidad))
  ]
  indices_raros_presentacion <- utils::head(indices_raros, 6L)
  indices_resumen <- unique(c(
    if (length(frecuencias)) 1L else integer(),
    indices_raros_presentacion
  ))
  indices_objetivo <- sort(unique(c(indices_salida, indices_resumen)))
  nombres_objetivo <- names(frecuencias)[indices_objetivo]
  ejemplos_objetivo <- rep("", length(indices_objetivo))

  if (length(indices_objetivo)) {
    grupos <- match(para_tabla, nombres_objetivo, nomatch = 0L)
    posiciones <- which(grupos > 0L & !is.na(textos))
    if (length(posiciones)) {
      textos_por_patron <- split(textos[posiciones], grupos[posiciones])
      indices_grupo <- as.integer(names(textos_por_patron))
      ejemplos_objetivo[indices_grupo] <- vapply(
        textos_por_patron,
        function(valores_grupo) {
          paste(utils::head(unique(valores_grupo), 3L), collapse = " | ")
        },
        character(1L)
      )
    }
  }

  # Los ejemplos son valores del usuario, y por esta puerta salian crudos. La
  # misma columna por dentro de `perfilar()` sale enmascarada -medido: 0 de 5
  # correos en el perfil contra 3 de 5 aca-, asi que dos caminos hacian cosas
  # opuestas con el mismo dato.
  #
  # Aca no hay nombre de columna: llega un vector suelto. Se clasifica **por
  # forma**, que es lo unico disponible, y el paquete es deliberadamente
  # conservador con eso: un numero de ocho digitos NO se protege por su forma
  # -puede ser cualquier cosa, y conjeturar por debajo de lo declarado es lo que
  # su regla prohibe-, pero un correo si, porque su forma no deja dudas. O sea
  # que esto enmascara exactamente lo que el propio clasificador manda proteger
  # sin ayuda del nombre, ni un valor mas.
  #
  # Sobre una MUESTRA, no sobre la columna entera: `descubrir_patrones()` tambien
  # se llama dos veces por columna desde `perfilar()`. Medido sobre 20.000
  # valores, clasificar entero cuesta 21,2 ms -59% de lo que cuesta la funcion- y
  # con tope 1.000 cuesta 0,8 ms, con la misma respuesta. El tope no es un numero
  # nuevo: es `muestra_validadores`, que el paquete ya usa para esto mismo.
  if (isTRUE(proteger_datos_personales) &&
      length(ejemplos_objetivo) && any(nzchar(ejemplos_objetivo))) {
    presentes <- textos[!is.na(textos)]
    if (length(presentes)) {
      cata <- utils::head(presentes, .MUESTRA_CLASIFICACION_PATRONES)
      clasificacion <- .clasificar_dato_personal(
        cata, "", list(tipo = .tipo_declarado(cata))
      )
      if (isTRUE(clasificacion$proteger)) {
        ejemplos_objetivo[nzchar(ejemplos_objetivo)] <- "[valor protegido]"
      }
    }
  }

  crear_tabla <- function(indices) {
    nombres <- names(frecuencias)[indices]
    nombres[nombres == marcador_na] <- NA_character_
    data.frame(
      # Los patrones y sus ejemplos se PUBLICAN, asi que un valor declarado
      # `bytes` se rinde a la forma que muestra la consola en vez de
      # interpretarse: el perfil publicaba el caracter mientras `print()` del
      # mismo dato mostraba la forma escapada.
      patron = .texto_publicable(nombres),
      n = as.integer(frecuencias[indices]),
      proporcion = proporciones[indices],
      ejemplos = .texto_publicable(
        ejemplos_objetivo[match(indices, indices_objetivo)]
      ),
      stringsAsFactors = FALSE
    )
  }

  resultado <- crear_tabla(indices_salida)
  resumen <- crear_tabla(indices_resumen)
  rownames(resultado) <- NULL
  rownames(resumen) <- NULL
  class(resultado) <- c("patrones", "data.frame")
  attr(resultado, "total") <- muestra_x$total
  attr(resultado, "analizados") <- muestra_x$analizados
  attr(resultado, "filas_analizadas") <- muestra_x$analizados
  attr(resultado, "muestreado") <- muestra_x$muestreado
  attr(resultado, "n_patrones_distintos") <- length(frecuencias)
  attr(resultado, "n_patrones_raros") <- n_patrones_raros
  attr(resultado, "patrones_raros_trazabilidad") <-
    nombres_raros_trazabilidad
  attr(resultado, "n_patrones_raros_trazabilidad") <- n_patrones_raros
  attr(resultado, "limite_patrones_raros_trazabilidad") <-
    .limite_patrones_raros_trazabilidad
  attr(resultado, "n_filas_patrones_no_dominantes_excluidos") <-
    n_filas_patrones_no_dominantes_excluidos
  attr(resultado, "resumen_patrones") <- resumen
  resultado
}

# La misma generalizacion se escribia en tres lugares -aqui, en el detector de
# patrones raros y en el resumen de columnas- y no era solo repeticion: las tres
# TIENEN que coincidir o el paquete miente. `.indices_patron_raro()` cierra
# comparando sus patrones contra los que produjo `descubrir_patrones()`, asi que
# si una de las copias cambiaba, el `%in%` no encontraba nada, el hallazgo se
# publicaba igual y su trazabilidad salia vacia **sin ningun error**.
.generalizar_a_patron <- function(x, distinguir_mayusculas = TRUE,
                                  expandir = FALSE) {
  x <- gsub("[[:digit:]]", "9", x, perl = TRUE)
  if (isTRUE(distinguir_mayusculas)) {
    x <- gsub("[[:lower:]]", "a", x, perl = TRUE)
    x <- gsub("[[:upper:]]", "A", x, perl = TRUE)
  } else {
    x <- gsub("[[:alpha:]]", "a", x, perl = TRUE)
  }
  if (!isTRUE(expandir)) {
    x <- gsub("9{2,}", "9+", x, perl = TRUE)
    x <- gsub("a{2,}", "a+", x, perl = TRUE)
    x <- gsub("A{2,}", "A+", x, perl = TRUE)
  }
  x
}
