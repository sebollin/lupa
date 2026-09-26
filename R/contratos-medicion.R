.validar_fecha_contrato <- function(x, nombre, permitir_null = TRUE) {
  if (is.null(x) && permitir_null) return(NULL)
  if (!.es_fecha_modelo(x) || !length(x) || anyNA(x) ||
      any(!is.finite(.fecha_numerica(x)))) {
    stop("`", nombre, "` debe contener fechas v\u00e1lidas no ausentes.",
         call. = FALSE)
  }
  x
}

#' Declarar vigencia y escala de medición
#'
#' `vigencia()` reúne el contrato temporal que las métricas de actualidad y
#' oportunidad no pueden inferir de los datos: columna de actualización,
#' fecha de acceso, último cambio conocido, fecha límite, intervalo y frecuencia
#' esperada. Cada métrica valida los campos que necesita y se abstiene si faltan:
#' declara en `cobertura_metricas` cuál falta y cómo declararlo, en lugar de
#' abortar la medición de las demás métricas.
#'
#' Las dos puntas de cada comparación tienen que ser de la **misma clase**. Un
#' `Date` se ancla a la medianoche UTC y un `POSIXct` vale por su instante, así
#' que mezclarlos puede hacer contar como tarde una actualización del día del
#' límite, y si el `POSIXct` se leyó sin huso el resultado depende de la sesión.
#' Cuando una métrica compara clases distintas lo avisa. Como `fecha_acceso` vale
#' `Sys.time()` por omisión, con una columna `Date` conviene declararlo también
#' como `Date`: `fecha_acceso = Sys.Date()`.
#'
#' `escala()` declara el error de un instrumento o de otra escala experta. Con
#' error absoluto, `Escala` calcula `1 - error / abs(valor)` y acota el resultado
#' a `[0, 1]`; con error relativo calcula `1 - error`. No se aprende el error de
#' la distribución observada.
#'
#' @param columna_actualizacion Nombre de la columna Date o POSIXt que registra
#'   la última actualización de cada fila.
#' @param fecha_acceso Momento de acceso usado para estimar actualidad. De la
#'   misma clase que la columna de actualización: con una columna `Date`,
#'   `Sys.Date()` en lugar del `Sys.time()` por omisión.
#' @param fecha_ultimo_cambio Fecha conocida del último cambio en el mundo real;
#'   puede ser escalar o tener una entrada por fila.
#' @param fecha_limite Fecha límite escalar o por fila para oportunidad.
#' @param inicio_intervalo,fin_intervalo Extremos del intervalo de vigencia.
#' @param frecuencia_cambio Frecuencia esperada como `difftime` o número de días.
#' @param error Error no negativo escalar, vectorial o función del valor.
#' @param tipo Interpretación `"absoluto"` o `"relativo"` del error.
#'
#' @return `vigencia()` devuelve un objeto `vigencia_datos`; `escala()` devuelve
#'   un objeto `escala_medicion`. Ambos son contratos de configuración y no
#'   examinan datos.
#' @seealso [metricas_nucleo()], [especializar()], [cobertura_analisis()]
#'
#' @examples
#' contrato <- vigencia(
#'   "actualizado", fecha_limite = as.Date("2026-02-01"),
#'   frecuencia_cambio = 30, fecha_acceso = as.Date("2026-02-15")
#' )
#' instrumento <- escala(error = 0.5, tipo = "absoluto")
#' @name contratos_medicion
NULL

#' @rdname contratos_medicion
#' @export
vigencia <- function(columna_actualizacion, fecha_acceso = Sys.time(),
                     fecha_ultimo_cambio = NULL, fecha_limite = NULL,
                     inicio_intervalo = NULL, fin_intervalo = NULL,
                     frecuencia_cambio = NULL) {
  if (!.es_texto_escalar(columna_actualizacion)) {
    stop("`columna_actualizacion` debe ser un nombre no vac\u00edo.", call. = FALSE)
  }
  fecha_acceso <- .validar_fecha_contrato(
    fecha_acceso, "fecha_acceso", permitir_null = FALSE
  )
  fecha_ultimo_cambio <- .validar_fecha_contrato(
    fecha_ultimo_cambio, "fecha_ultimo_cambio"
  )
  fecha_limite <- .validar_fecha_contrato(fecha_limite, "fecha_limite")
  inicio_intervalo <- .validar_fecha_contrato(
    inicio_intervalo, "inicio_intervalo"
  )
  fin_intervalo <- .validar_fecha_contrato(fin_intervalo, "fin_intervalo")
  if (xor(is.null(inicio_intervalo), is.null(fin_intervalo))) {
    stop("El intervalo exige `inicio_intervalo` y `fin_intervalo`.",
         call. = FALSE)
  }
  if (!is.null(inicio_intervalo)) {
    .validar_intervalo_fechas(
      inicio_intervalo, fin_intervalo, "inicio_intervalo", "fin_intervalo"
    )
  }
  frecuencia_segundos <- NULL
  if (!is.null(frecuencia_cambio)) {
    frecuencia_segundos <- if (inherits(frecuencia_cambio, "difftime")) {
      as.numeric(frecuencia_cambio, units = "secs")
    } else if (is.numeric(frecuencia_cambio)) {
      as.numeric(frecuencia_cambio) * 86400
    } else {
      NA_real_
    }
    if (!length(frecuencia_segundos) || anyNA(frecuencia_segundos) ||
        any(!is.finite(frecuencia_segundos)) || any(frecuencia_segundos <= 0)) {
      stop("`frecuencia_cambio` debe ser una duraci\u00f3n positiva.", call. = FALSE)
    }
  }
  estructura <- list(
    columna_actualizacion = columna_actualizacion,
    fecha_acceso = fecha_acceso,
    fecha_ultimo_cambio = fecha_ultimo_cambio,
    fecha_limite = fecha_limite,
    inicio_intervalo = inicio_intervalo,
    fin_intervalo = fin_intervalo,
    frecuencia_cambio_segundos = frecuencia_segundos
  )
  class(estructura) <- "vigencia_datos"
  estructura
}

#' @rdname contratos_medicion
#' @export
escala <- function(error, tipo = c("absoluto", "relativo")) {
  tipo <- match.arg(tipo)
  if (!is.function(error) &&
      (!is.numeric(error) || !length(error) || anyNA(error) ||
       any(!is.finite(error)) || any(error < 0))) {
    stop("`error` debe ser num\u00e9rico no negativo o una funci\u00f3n.", call. = FALSE)
  }
  if (tipo == "relativo" && !is.function(error) && any(error > 1)) {
    stop("El error relativo debe estar en [0, 1].", call. = FALSE)
  }
  estructura <- list(error = error, tipo = tipo)
  class(estructura) <- "escala_medicion"
  estructura
}

.validar_config_contrato <- function(configuracion, clase, nombre) {
  configuracion <- .validar_propiedades_base(
    configuracion, tolower(nombre)
  )
  objeto <- configuracion[[tolower(nombre)]]
  if (!inherits(objeto, clase)) {
    stop("`", tolower(nombre), "` debe provenir de ", tolower(nombre), "().",
         call. = FALSE)
  }
  configuracion
}

.validar_config_escala <- function(configuracion) {
  .validar_config_contrato(configuracion, "escala_medicion", "Escala")
}

.validar_config_vigencia <- function(configuracion) {
  .validar_config_contrato(configuracion, "vigencia_datos", "Vigencia")
}

.rep_error_escala <- function(error, valores, filas, n_total) {
  resultado <- if (is.function(error)) error(valores) else error
  if (length(resultado) == n_total) resultado <- resultado[filas]
  if (length(resultado) == 1L) resultado <- rep(resultado, length(valores))
  if (!is.numeric(resultado) || length(resultado) != length(valores) ||
      anyNA(resultado) || any(!is.finite(resultado)) || any(resultado < 0)) {
    stop("El error de escala no devolvi\u00f3 un valor finito no negativo por fila.",
         call. = FALSE)
  }
  resultado
}

.metodo_escala <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  if (!is.numeric(x) || inherits(x, c("Date", "POSIXt", "integer64"))) {
    stop("Escala requiere un atributo num\u00e9rico ordinario.", call. = FALSE)
  }
  filas <- which(!is.na(x))
  valores <- x[filas]
  if (any(!is.finite(valores))) {
    stop("Escala no puede medir valores no finitos.", call. = FALSE)
  }
  contrato <- instancia$configuracion$escala
  error <- .rep_error_escala(contrato$error, valores, filas, length(x))
  if (contrato$tipo == "relativo") {
    if (any(error > 1)) stop("El error relativo debe estar en [0, 1].", call. = FALSE)
    resultado <- 1 - error
  } else {
    cociente <- ifelse(abs(valores) == 0, ifelse(error == 0, 0, Inf),
                       error / abs(valores))
    resultado <- pmax(0, pmin(1, 1 - cociente))
  }
  .salida_metodo(
    resultado, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

# `Date` y `POSIXct` no se comparan en la misma escala: `.fecha_numerica()` ancla
# un `Date` a la medianoche UTC y un `POSIXct` a su instante. Mezclarlos cambia el
# veredicto de una forma que no es obvia: una actualizacion hecha EL DIA del
# limite pero despues de las 00:00 UTC cuenta como tarde, y si la columna se
# parseo sin huso el resultado depende ademas de la sesion. Medido, la misma
# corrida sobre los mismos datos y el mismo contrato: con `TZ=UTC` los resultados
# son `1 0 0`, con `TZ=America/Montevideo` `0 0 0` y con `TZ=Asia/Tokyo` `1 0 0`.
#
# El paquete no puede adivinar lo que el usuario quiso -la decision de anclaje es
# suya y la ambiguedad del parseo es del dato-, pero si puede ver la mezcla y
# declararla. Es la misma senal que `perfilar()` publica en
# `fecha_civil_distinta_utc`, en la puerta donde se mide contra un contrato.
#
# Se mira SOLO lo que la metrica compara: el contrato trae `fecha_acceso` con la
# hora de la corrida aunque la metrica no la use, y mirarla entera hacia avisar
# sobre una comparacion Date-Date que no mezcla nada.
#
# La consecuencia se pasa como texto porque no es la misma en todas las metricas:
# en las de frescura una entrega del dia del limite puede contar como tarde; en un
# dominio por extension, un valor que SI esta en el dominio no coincide con
# ninguno y la metrica publica un incumplimiento que no existe. Medido sobre tres
# fechas: `0 0 0` con el dominio declarado como instantes y `1 1 0` con el mismo
# dominio como fechas de calendario.
.declarar_mezcla_de_husos <- function(nombre_metrica, columna, x, fechas,
                                      consecuencia = NULL) {
  if (is.null(consecuencia)) {
    consecuencia <- paste0(
      "as\u00ed que una actualizaci\u00f3n del d\u00eda del l\u00edmite puede contar como tarde, ",
      "y si el `POSIXct` se ley\u00f3 sin huso el resultado depende de la sesi\u00f3n"
    )
  }
  clase_temporal <- function(v) {
    if (inherits(v, "Date")) "Date" else if (inherits(v, "POSIXt")) "POSIXt" else
      NA_character_
  }
  clases <- c(clase_temporal(x),
              vapply(fechas, clase_temporal, character(1L), USE.NAMES = FALSE))
  clases <- clases[!is.na(clases)]
  if (length(unique(clases)) < 2L) return(invisible(FALSE))
  warning(
    nombre_metrica, " compara una fecha de calendario con un instante: la ",
    "columna `", columna, "` y el contrato mezclan `Date` -que se ancla a la ",
    "medianoche UTC- con `POSIXct`, ", consecuencia,
    ". Declarar las dos puntas en la misma clase ",
    "-o con `tz` expl\u00edcito en las dos- deja el n\u00famero estable.",
    call. = FALSE
  )
  invisible(TRUE)
}

.actualizaciones_vigencia <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L) {
    stop(instancia$declaracion$nombre, " requiere una entidad.", call. = FALSE)
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  contrato <- instancia$configuracion$vigencia
  columna <- contrato$columna_actualizacion
  x <- .obtener_columna_modelo(tabla, columna, entidad)
  if (!.es_fecha_modelo(x)) {
    stop("La columna de actualizaci\u00f3n debe ser Date o POSIXt.", call. = FALSE)
  }
  filas <- which(!is.na(x))
  list(
    entidad = entidad, tabla = tabla, contrato = contrato, columna = columna,
    filas = filas, actualizacion = .fecha_numerica(x[filas]), n = length(x)
  )
}

.metodo_desactualizacion_fecha <- function(tablas, instancia) {
  datos <- .actualizaciones_vigencia(tablas, instancia)
  contrato <- datos$contrato
  if (!is.null(contrato$fecha_ultimo_cambio)) {
    .declarar_mezcla_de_husos(
      "DesactualizacionPorFecha", datos$columna,
      datos$tabla[[datos$columna]], list(contrato$fecha_ultimo_cambio)
    )
    cambio <- .fecha_para_filas(
      contrato$fecha_ultimo_cambio, datos$filas, datos$n,
      "fecha_ultimo_cambio"
    )
    atraso <- pmax(0, cambio - datos$actualizacion)
  } else if (!is.null(contrato$frecuencia_cambio_segundos)) {
    .declarar_mezcla_de_husos(
      "DesactualizacionPorFecha", datos$columna,
      datos$tabla[[datos$columna]], list(contrato$fecha_acceso)
    )
    acceso <- .fecha_para_filas(
      contrato$fecha_acceso, datos$filas, datos$n, "fecha_acceso"
    )
    frecuencia <- rep(contrato$frecuencia_cambio_segundos,
                      length.out = length(datos$filas))
    atraso <- pmax(0, acceso - datos$actualizacion - frecuencia)
  } else {
    return(.abstener_metodo(
      instancia,
      paste0(
        "DesactualizacionPorFecha no se midi\u00f3: el contrato de vigencia no ",
        "trae `fecha_ultimo_cambio` ni `frecuencia_cambio`, y sin uno de los dos ",
        "no hay con qu\u00e9 calcular el atraso."
      ),
      paste0(
        "Declarar `fecha_ultimo_cambio` o `frecuencia_cambio` en `vigencia()`. ",
        "Una m\u00e9trica que se abstiene no se interpreta como cero."
      )
    ))
  }
  .salida_metodo(
    atraso / 86400, datos$entidad, datos$columna, datos$filas,
    paste0(datos$entidad, "[", datos$filas, ",]")
  )
}

.metodo_desactualizacion_cambios <- function(tablas, instancia) {
  datos <- .actualizaciones_vigencia(tablas, instancia)
  frecuencia <- datos$contrato$frecuencia_cambio_segundos
  if (is.null(frecuencia)) {
    return(.abstener_metodo(
      instancia,
      paste0(
        "DesactualizacionPorCambios no se midi\u00f3: el contrato de vigencia no ",
        "trae `frecuencia_cambio`, que es la unidad en la que esta m\u00e9trica ",
        "cuenta."
      ),
      paste0(
        "Declarar `frecuencia_cambio` en `vigencia()`. Una m\u00e9trica que se ",
        "abstiene no se interpreta como cero."
      )
    ))
  }
  .declarar_mezcla_de_husos(
    "DesactualizacionPorCambios", datos$columna,
    datos$tabla[[datos$columna]], list(datos$contrato$fecha_acceso)
  )
  acceso <- .fecha_para_filas(
    datos$contrato$fecha_acceso, datos$filas, datos$n, "fecha_acceso"
  )
  frecuencia <- rep(frecuencia, length.out = length(datos$filas))
  cambios <- floor(pmax(0, acceso - datos$actualizacion) / frecuencia)
  .salida_metodo(
    cambios, datos$entidad, datos$columna, datos$filas,
    paste0(datos$entidad, "[", datos$filas, ",]")
  )
}

.metodo_oportunidad_entidad <- function(tablas, instancia, intervalo) {
  datos <- .actualizaciones_vigencia(tablas, instancia)
  contrato <- datos$contrato
  if (intervalo) {
    if (is.null(contrato$inicio_intervalo) || is.null(contrato$fin_intervalo)) {
      return(.abstener_metodo(
        instancia,
        paste0(
          "OportunidadEntPorIntervalo no se midi\u00f3: el contrato de vigencia ",
          "no trae el intervalo -`inicio_intervalo` y `fin_intervalo`- contra el ",
          "que se compara la actualizaci\u00f3n."
        ),
        paste0(
          "Declarar el intervalo en `vigencia()`. Una m\u00e9trica que se ",
          "abstiene no se interpreta como cero."
        )
      ))
    }
    .declarar_mezcla_de_husos(
      "OportunidadEntPorIntervalo", datos$columna,
      datos$tabla[[datos$columna]],
      list(contrato$inicio_intervalo, contrato$fin_intervalo)
    )
    inicio <- .fecha_para_filas(
      contrato$inicio_intervalo, datos$filas, datos$n, "inicio_intervalo"
    )
    fin <- .fecha_para_filas(
      contrato$fin_intervalo, datos$filas, datos$n, "fin_intervalo"
    )
    resultado <- datos$actualizacion >= inicio & datos$actualizacion <= fin
  } else {
    if (is.null(contrato$fecha_limite)) {
      return(.abstener_metodo(
        instancia,
        paste0(
          "OportunidadEntPorFecha no se midi\u00f3: el contrato de vigencia no ",
          "trae `fecha_limite`, que es la fecha contra la que se compara la ",
          "actualizaci\u00f3n."
        ),
        paste0(
          "Declarar `fecha_limite` en `vigencia()`. Una m\u00e9trica que se ",
          "abstiene no se interpreta como cero."
        )
      ))
    }
    .declarar_mezcla_de_husos(
      "OportunidadEntPorFecha", datos$columna,
      datos$tabla[[datos$columna]], list(contrato$fecha_limite)
    )
    limite <- .fecha_para_filas(
      contrato$fecha_limite, datos$filas, datos$n, "fecha_limite"
    )
    resultado <- datos$actualizacion <= limite
  }
  .salida_metodo(
    resultado, datos$entidad, datos$columna, datos$filas,
    paste0(datos$entidad, "[", datos$filas, ",]")
  )
}

.metodo_oportunidad_entidad_fecha <- function(tablas, instancia) {
  .metodo_oportunidad_entidad(tablas, instancia, FALSE)
}

.metodo_oportunidad_entidad_intervalo <- function(tablas, instancia) {
  .metodo_oportunidad_entidad(tablas, instancia, TRUE)
}
