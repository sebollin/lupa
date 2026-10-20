#' @export
`[.deriva_perfil` <- function(x, ...) {
  resultado <- NextMethod("[")
  .conservar_atributos_objeto(resultado, x)
}

#' @export
rbind.deriva_perfil <- function(..., deparse.level = 1) {
  resultado <- .unir_con_marca_sin_proteger(..., deparse.level = deparse.level)
  # Lo que no se comparo por la proteccion se declara en un atributo, y
  # `rbind()` conserva solo el del primer objeto: unir dos derivas callaba lo que
  # declaraba la segunda. Se unen los de todas, como la marca de proteccion.
  declarados <- Filter(
    function(x) inherits(x, "data.frame"),
    lapply(list(...), function(objeto) {
      attr(objeto, "campos_protegidos", exact = TRUE)
    })
  )
  if (length(declarados) && inherits(resultado, "data.frame")) {
    unidos <- do.call(rbind, declarados)
    unidos <- unidos[!duplicated(unidos), , drop = FALSE]
    rownames(unidos) <- NULL
    attr(resultado, "campos_protegidos") <- unidos
  }
  resultado
}

.deriva_perfil_vacia <- function() {
  resultado <- data.frame(
    columna = character(), aspecto = character(), cambio = character(),
    severidad = character(), valor_anterior = character(),
    valor_actual = character(), delta = numeric(),
    cambio_relativo = numeric(), significativo = logical(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    descripcion = character(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

.validar_perfil_deriva <- function(x, nombre) {
  if (!inherits(x, "perfil") || !inherits(x$columnas, "data.frame") ||
      !is.list(x$patrones) || !inherits(x$hallazgos, "data.frame") ||
      is.null(x$meta$fecha_hora)) {
    if (inherits(x, "perfil_dbi")) {
      .detener(
        "`", nombre, "` es un objeto de `perfilar_dbi()`: para comparar dos ",
        "entregas pase su perfil de muestra, `", nombre, "$perfil_muestra`. ",
        "El resumen SQL de la tabla completa no es un perfil.",
        call. = FALSE
      )
    }
    .detener("`", nombre, "` debe ser un objeto producido por perfilar().",
         call. = FALSE)
  }
  x
}

.mapa_columnas_perfil <- function(perfil) {
  nombres <- as.character(perfil$columnas$columna)
  nombres[is.na(nombres) | !nzchar(nombres)] <- "<sin_nombre>"
  data.frame(
    clave = .nombres_unicos(.nombres_para_operar(nombres)), original = nombres,
    indice = seq_along(nombres), stringsAsFactors = FALSE
  )
}

.distinto_deriva <- function(a, b) {
  if (length(a) != 1L || length(b) != 1L) return(TRUE)
  if (is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) || is.na(b)) return(TRUE)
  !isTRUE(all.equal(a, b, check.attributes = FALSE))
}

# Un numero publicado en la deriva no puede depender de como imprime la sesion
# ni juntar dos valores distintos, y `format(x, digits = 8)` hacia las dos cosas.
# Con `scipen = 0` el rango [210000000011, 210000001000] salia `[2.1e+11,
# 2.1e+11]` -dos extremos distintos impresos iguales-, otro salia `[1e+06,
# 1040000]` -dos notaciones en el mismo rango-, y con `scipen = 100` los mismos
# perfiles se publicaban con todas sus cifras. Se escribe con la forma mas corta
# que vuelve al valor, la de `.texto_identidad()`: un entero con todas sus
# cifras, una fraccion con 15, 16 o 17. No mira `scipen` ni `digits`.
# La marca decimal SI sigue a `OutDec`: mostrar `3,5` a quien pidio coma es una
# decision del paquete (RESUELTOS 2.245 y 2.314), y la evidencia de la deriva ya
# la respeta con `.formatear_decimal_publicado()`. Un objeto con las dos marcas
# es peor que cualquiera de las dos.
.texto_numero_deriva <- function(x) {
  texto <- .texto_identidad(x)
  marca <- getOption("OutDec", ".")
  if (!identical(marca, ".")) texto <- gsub(".", marca, texto, fixed = TRUE)
  texto
}

# El separador de los extremos no puede chocar con la marca decimal: con
# `OutDec = ","` el rango de 1,5 a 3 se publicaba `[1,5, 3]`, que se lee de dos
# maneras. Con coma decimal los extremos se separan con punto y coma, que es como
# se escriben los intervalos en esa notacion.
#
# La coma no viene solo de `OutDec`: con `LC_NUMERIC` en coma la escritura
# publicada -`format()`, el `sprintf()` de C- ya la trae aunque `OutDec` sea
# punto, y `detectar_relaciones()` publicaba `[0,125, 0,75]` (ronda 32-S). Se
# mira tambien el texto de los extremos: el separador es "; " si alguno lleva
# coma. Quien publica varios rangos juntos pasa el separador de todos.
.separador_rango_deriva <- function(textos) {
  coma <- !identical(getOption("OutDec", "."), ".") ||
    any(grepl(",", textos, fixed = TRUE, useBytes = TRUE))
  if (coma) "; " else ", "
}

.texto_rango_deriva <- function(minimo, maximo,
                                separador = .separador_rango_deriva(
                                  c(minimo, maximo)
                                )) {
  paste0("[", minimo, separador, maximo, "]")
}

.texto_deriva <- function(x) {
  if (!length(x) || is.na(x)) return(NA_character_)
  if (is.numeric(x)) return(.texto_numero_deriva(x))
  texto <- as.character(x)
  # Una diferencia que el canal no muestra es una diferencia que el lector no
  # puede ver. Recortar espacios cambia el patron de `a+\a+9\a+9a ` a
  # `a+\a+9\a+9a`, y la deriva informaba seis cambios con severidad `error`
  # cuyos `valor_anterior` y `valor_actual` se imprimian IDENTICOS: el espacio
  # final no se ve. Se cita el valor solo cuando hace falta -espacio al borde,
  # cadena vacia, o algo que haya que escapar-, para que la diferencia se vea
  # sin agregar ruido al resto.
  # Los dos motivos para citar son distintos y no se pueden mezclar. Si hay algo
  # que escapar, se rinde con el renderizador del paquete. Si lo unico que pasa
  # es que el valor tiene un espacio al borde o esta vacio, se lo encierra entre
  # comillas TAL CUAL: pasarlo por `encodeString()` duplicaria las barras del
  # propio patron y la fila citada dejaria de casar con la que no lo esta.
  if (.hay_escapes_al_publicar(texto)) return(.citar_publicable(texto))
  if (!nzchar(texto) || !identical(texto, trimws(texto))) {
    return(paste0('"', texto, '"'))
  }
  texto
}

# Como entra cada argumento de `perfilar()` en `comparar_perfiles()`. La lista
# es de TODOS los argumentos, y una prueba exige que coincida con
# `formals(perfilar)`: un argumento nuevo que habilite un hallazgo o mueva una
# vara no puede quedar afuera en silencio, que es como quedaron
# `casi_duplicados_vocabulario` y `ausencia_estructural` -un hallazgo
# `resuelto, ok` con los mismos datos- y los umbrales de las relaciones entre
# columnas -hallazgos que aparecian sin fila de configuracion- (refutacion
# 35-D, M4 y B2). Las clases:
#
# * `habilita`: el argumento decide SI corre un diagnostico. Si el perfil nuevo
#   lo apago o retiro la declaracion, sus hallazgos del anterior se publican
#   `no_evaluado` y no `resuelto`; `forma` y `meta` dicen como leerlo (ver
#   `.declinados_por_declaracion_retirada()`).
# * `vara`: cambia si un hallazgo se emite o con que severidad; un cambio se
#   declara en la fila `configuracion_umbrales` (`.configuracion_umbrales_perfil()`).
#   Una declaracion puede ser las dos cosas (`columnas_sin_ceros`).
# * `politica`: tiene su propia fila, `fila`.
# * `limite`: acota el trabajo de un diagnostico; si lo corta, el perfil lo
#   declara en `cobertura_diagnosticos` y el hallazgo sale `no_evaluado` por esa
#   via (`.diagnosticos_declinados_deriva()`).
# * `sin_hallazgo`: no cambia `perfil$hallazgos` -sus resultados viven en otra
#   tabla, acotan la evidencia o solo avisan-.
# * `identidad`: los datos, su nombre y la fecha.
#
# Que la lista coincida con los argumentos no alcanza: `duplicados_aproximados`
# estaba en ella con la clase equivocada. Una prueba perfila un dato fijo con
# cada argumento movido y exige que la clase explique lo que cambia: un
# `sin_hallazgo` no cambia ningun hallazgo, y ningun `resuelto` sale sin una
# fila de configuracion que lo explique (test-O164).
.ARGUMENTOS_PERFIL_DERIVA <- list(
  datos = list(clase = "identidad"),
  nombre = list(clase = "identidad"),
  fecha = list(clase = "identidad"),
  muestra = list(clase = "politica", fila = "configuracion_patrones"),
  max_patrones = list(clase = "politica", fila = "configuracion_patrones"),
  distinguir_mayusculas = list(clase = "politica", fila = "configuracion_patrones"),
  expandir = list(clase = "politica", fila = "configuracion_patrones"),
  umbral_faltantes_sospechoso = list(clase = "vara", meta = "umbral_faltantes_sospechoso"),
  umbral_faltantes_error = list(clase = "vara", meta = "umbral_faltantes_error"),
  umbral_alta_cardinalidad = list(clase = "vara", meta = "umbral_alta_cardinalidad"),
  umbral_patron_dominante = list(clase = "vara", meta = "umbral_patron_dominante"),
  umbral_patron_raro = list(clase = "vara", meta = "umbral_patron_raro"),
  columnas_sin_ceros = list(
    clase = c("vara", "habilita"), meta = "columnas_sin_ceros",
    forma = "columnas", hallazgos = "ceros_no_permitidos"
  ),
  columnas_no_negativas = list(
    clase = c("vara", "habilita"), meta = "columnas_no_negativas",
    forma = "columnas", hallazgos = "negativos_no_permitidos"
  ),
  columnas_codigo = list(clase = "vara", meta = "columnas_codigo"),
  clave = list(
    clase = "habilita", meta = "declaracion_clave", forma = "tabla",
    hallazgos = c("clave_no_unica", "clave_con_ausentes")
  ),
  columnas_opcionales = list(clase = "politica", fila = "configuracion_columnas_opcionales"),
  # Tambien habilita: un valor presente fuera del universo declarado es el
  # hallazgo `valor_fuera_de_aplicabilidad`, que sin la declaracion no se busca.
  aplicabilidad = list(
    clase = c("politica", "habilita"), fila = "configuracion_aplicabilidad",
    meta = "declaracion_aplicabilidad", forma = "columnas",
    hallazgos = "valor_fuera_de_aplicabilidad"
  ),
  ausencia_estructural = list(
    clase = "habilita", meta = "ausencia_estructural", forma = "interruptor",
    hallazgos = "posible_ausencia_estructural"
  ),
  sentinelas_numericos = list(clase = "politica", fila = "configuracion_sentinelas_numericos"),
  cadenas_ausencia = list(clase = "politica", fila = "configuracion_cadenas_ausencia"),
  analizar_dependencias = list(clase = "sin_hallazgo"),
  umbral_dependencia = list(clase = "sin_hallazgo"),
  umbral_casi_clave_dependencia = list(clase = "sin_hallazgo"),
  max_columnas_dependencias = list(clase = "sin_hallazgo"),
  datos_personales_permitidos = list(clase = "vara", meta = "datos_personales_permitidos"),
  proteger_datos_personales = list(clase = "politica", fila = "configuracion_proteccion"),
  columnas_personales = list(
    clase = c("politica", "habilita"), fila = "proteccion",
    meta = "columnas_personales_declaradas", forma = "columnas",
    hallazgos = "dato_personal_posible"
  ),
  validadores_personales = list(clase = "vara", meta = "validadores_personales"),
  umbral_documento_verificado = list(clase = "vara", meta = "umbral_documento_verificado"),
  muestra_validadores = list(clase = "vara", meta = "muestra_validadores"),
  # Se clasificaba `sin_hallazgo` y produce hallazgos: apagarlo publicaba un
  # par de columnas duplicado como `resuelto, ok` con los mismos datos
  # (refutacion 35-D, segunda vuelta). No viaja en `meta`: lo que corrio queda
  # en `perfil$duplicados_aproximados` (ver `.duplicados_evaluados_perfil()`).
  duplicados_aproximados = list(
    clase = c("habilita", "politica"), forma = "duplicados",
    fila = "configuracion_duplicados_aproximados",
    hallazgos = c(
      "duplicados_exactos_columnas", "duplicados_exactos_normalizados",
      "duplicados_aproximados"
    )
  ),
  normalizar = list(clase = "politica", fila = "configuracion_normalizacion"),
  max_filas_hallazgo = list(clase = "sin_hallazgo"),
  umbral_orden_columnas = list(clase = "vara", meta = "umbral_orden_columnas"),
  max_columnas_orden = list(clase = "vara", meta = "max_columnas_orden"),
  umbral_solapamiento_orden = list(clase = "vara", meta = "umbral_solapamiento_orden"),
  umbral_aritmetica = list(clase = "vara", meta = "umbral_aritmetica"),
  min_filas_aritmetica = list(clase = "vara", meta = "min_filas_aritmetica"),
  tolerancia_aritmetica = list(clase = "vara", meta = "tolerancia_aritmetica"),
  max_columnas_aritmetica = list(clase = "vara", meta = "max_columnas_aritmetica"),
  casi_duplicados_vocabulario = list(
    clase = "habilita", meta = "casi_duplicados_vocabulario",
    forma = "interruptor", hallazgos = "casi_duplicados_vocabulario"
  ),
  max_proporcion_grupo_vocabulario = list(clase = "limite"),
  umbral_variante_rara_vocabulario = list(clase = "vara", meta = "umbral_variante_rara_vocabulario"),
  min_asimetria_vocabulario_corto = list(clase = "vara", meta = "min_asimetria_vocabulario_corto"),
  min_asimetria_vocabulario = list(clase = "vara", meta = "min_asimetria_vocabulario"),
  min_participacion_dominante_vocabulario_corto = list(
    clase = "vara", meta = "min_participacion_dominante_vocabulario_corto"
  ),
  variantes_equifrecuentes_vocabulario = list(
    clase = "habilita", meta = "variantes_equifrecuentes_vocabulario",
    forma = "interruptor", hallazgos = "variantes_equifrecuentes_vocabulario"
  ),
  max_asimetria_equifrecuente_vocabulario = list(
    clase = "vara", meta = "max_asimetria_equifrecuente_vocabulario"
  ),
  max_comparaciones_dependencias = list(clase = "sin_hallazgo"),
  max_trabajo_vocabulario = list(clase = "limite"),
  max_trabajo_dependencias = list(clase = "sin_hallazgo"),
  max_largo_valor_vocabulario = list(clase = "limite"),
  max_celdas_muestra = list(clase = "limite"),
  max_bytes_muestra = list(clase = "limite"),
  avisar_costo_tabla_ancha = list(clase = "sin_hallazgo"),
  umbral_celdas_aviso_tabla_ancha = list(clase = "sin_hallazgo")
)

# Los campos de `meta` de los argumentos de una clase, en el orden de la lista.
.argumentos_perfil_deriva_de_clase <- function(clase) {
  elegidos <- Filter(
    function(x) clase %in% x$clase && !is.null(x$meta),
    .ARGUMENTOS_PERFIL_DERIVA
  )
  unname(vapply(elegidos, function(x) x$meta, character(1L)))
}

.configuracion_patrones_perfil <- function(perfil) {
  campos <- c(
    "muestra", "max_patrones", "distinguir_mayusculas", "expandir",
    "umbral_patron_raro"
  )
  if (!all(campos %in% names(perfil$meta))) return(NULL)
  perfil$meta[campos]
}

# La normalizacion es la cuarta politica que redefine lo que se mide, y la
# unica que no se declaraba. Medido: con los MISMOS datos y `normalizar = FALSE`
# desaparece `casi_duplicados_vocabulario`, y la deriva informaba "Un hallazgo
# del perfil anterior ya no esta presente" con severidad `ok`. En monitoreo,
# apagar la normalizacion se leia como "los datos mejoraron". El `meta` guardaba
# la politica y la comparacion no la miraba.
# Las varas que deciden si un hallazgo se emite y con que severidad. Se comparan
# como CONJUNTO y en una sola fila: seis filas separadas serian ruido, y lo que
# el lector necesita saber es que la vara cambio, no cual de los seis umbrales.
.proteccion_declarada_perfil <- function(perfil) {
  # `perfilar()` declara su politica en `meta$proteger_datos_personales`. La via
  # DBI usa `meta$proteccion_personal$aplicada`, asi que se miran las dos y se
  # devuelve NA cuando ninguna esta: sin declaracion no se afirma nada.
  if (!is.list(perfil)) return(NA)
  meta <- perfil$meta
  if (!is.null(meta$proteger_datos_personales)) {
    valor <- meta$proteger_datos_personales[[1L]]
    if (is.logical(valor) && !is.na(valor)) return(valor)
  }
  aplicada <- meta$proteccion_personal$aplicada
  if (length(aplicada) == 1L && is.logical(aplicada) && !is.na(aplicada)) {
    return(aplicada)
  }
  NA
}

.configuracion_umbrales_perfil <- function(perfil) {
  # Las varas son las de `.ARGUMENTOS_PERFIL_DERIVA` con clase `vara`, en su
  # orden: las seis del principio y `columnas_codigo`, y desde la ronda 35 las
  # de las relaciones entre columnas, el vocabulario y los datos personales, que
  # cambiaban hallazgos y severidades sin esta fila (refutacion 35-D, B2).
  campos <- .argumentos_perfil_deriva_de_clase("vara")
  presentes <- intersect(campos, names(perfil$meta))
  if (!length(presentes)) return(NULL)
  # `columnas_codigo` llego en la ronda 32 (decision D2): un perfil guardado
  # antes no la trae, y no declaraba ningun codigo. Se la cuenta como "ninguna",
  # en su lugar de la lista, para que comparar contra un perfil viejo no
  # publique un cambio de vara que no hubo.
  presentes <- intersect(campos, c(presentes, "columnas_codigo"))
  valores <- lapply(presentes, function(campo) {
    valor <- perfil$meta[[campo]]
    if (is.null(valor) || !length(valor)) "ninguna"
    else {
      texto <- as.character(valor)
      texto <- texto[!duplicated(.clave_bytes(texto))]
      paste(.clave_bytes(.ordenar_por_bytes(texto)), collapse = "/")
    }
  })
  stats::setNames(unlist(valores, use.names = FALSE), presentes)
}

.configuracion_normalizacion_perfil <- function(perfil) {
  if (!"normalizacion" %in% names(perfil$meta)) return(NULL)
  general <- perfil$meta$normalizacion$general
  # Las politicas son CONJUNTOS: el orden en que se escriben es un artefacto de
  # la declaracion. Sin ordenar, `normalizacion(proteger = c("n", "u"))` contra
  # `normalizacion(proteger = c("u", "n"))` producia una fila
  # "modificado / error" cuyo valor anterior y actual eran IDENTICOS: la fila se
  # contradecia sola. Es la misma regla que el paquete ya fija para los pesos de
  # `agregar()`: la misma declaracion escrita en otro orden da el mismo numero.
  if (is.list(general) && !is.null(general$proteger)) {
    proteger <- as.character(general$proteger)
    proteger <- proteger[!duplicated(.clave_bytes(proteger))]
    general$proteger <- .clave_bytes(.ordenar_por_bytes(proteger))
  }
  general
}

.configuracion_aplicabilidad_perfil <- function(perfil) {
  if (!"declaracion_aplicabilidad" %in% names(perfil$meta)) return(NULL)
  valores <- as.character(perfil$meta$declaracion_aplicabilidad)
  valores <- valores[!duplicated(.clave_bytes(valores))]
  .clave_bytes(.ordenar_por_bytes(valores))
}

# `columnas_opcionales` redefine el universo de completitud de una columna -sus
# celdas presentes- igual que `aplicabilidad` redefine el aplicable, y el perfil
# la registra en `meta$columnas_opcionales` desde la ronda 33. La comparacion no
# la leia: el mismo dato perfilado sin y con la declaracion -la que el paquete
# recomienda en `posible_ausencia_estructural`- salia `faltantes 0,54 -> 0 ok`, y
# al retirarla `0 -> 0,54 error` con `hallazgo faltantes aparecido`, un deterioro
# que no ocurrio, sin la fila de configuracion que si tenia `aplicabilidad`
# (refutacion 34-D). Un perfil anterior a la ronda 33 no la registraba aunque la
# hubiera declarado: ahi no se sabe, y no se compara, como con `aplicabilidad`.
.configuracion_opcionales_perfil <- function(perfil) {
  if (!"columnas_opcionales" %in% names(perfil$meta)) return(NULL)
  valores <- as.character(perfil$meta$columnas_opcionales)
  valores <- valores[!is.na(valores)]
  valores <- valores[!duplicated(.clave_bytes(valores))]
  .clave_bytes(.ordenar_por_bytes(valores))
}

.configuracion_sentinelas_perfil <- function(perfil) {
  if (!"sentinelas_numericos" %in% names(perfil$meta)) return(NULL)
  # Un conjunto, no una secuencia: `c(999, -9)` y `c(-9, 999)` son la misma
  # politica y se declaraban como un cambio de vara con severidad `error`, que
  # es justo la fila que una serie de calidad lee como transicion de politica.
  # Los NA del enmascarado se conservan para que la guarda de politica oculta
  # los siga viendo.
  valores <- perfil$meta$sentinelas_numericos
  if (is.null(valores) || all(is.na(valores))) return(valores)
  .sort_por_bytes(unique(valores), na.last = TRUE)
}

# Un extremo exacto es un entero escrito en decimal y nada mas: es lo que el
# perfil conserva en `minimo_exacto` y `maximo_exacto` cuando el valor no entra
# en un doble. Se normaliza -sin `+` ni ceros a la izquierda- para que la
# igualdad sea la de los valores y no la de su escritura.
.extremo_exacto_deriva <- function(fila, campo) {
  if (!campo %in% names(fila)) return(NA_character_)
  valor <- fila[[campo]][[1L]]
  if (length(valor) != 1L || is.na(valor)) return(NA_character_)
  texto <- trimws(as.character(valor))
  if (!grepl("^[+-]?[0-9]+$", texto)) return(NA_character_)
  negativo <- startsWith(texto, "-")
  digitos <- sub("^0+(?=[0-9])", "", sub("^[+-]", "", texto), perl = TRUE)
  if (negativo && !identical(digitos, "0")) paste0("-", digitos) else digitos
}

# La diferencia EXACTA `x - y` entre dos enteros escritos en decimal, devuelta
# como doble. Hace falta para los extremos que no entran en un doble:
# `as.numeric()` los redondea a multiplos de 256 cerca de 2^60, y entre
# `1152921504606846996` y `1152921504606847976` la diferencia salia cero. Se
# restan las cifras, que no necesita `bit64` -esta en Suggests- y vale para
# cualquier largo; lo que se convierte a doble es la diferencia, que si entra.
.diferencia_entera_exacta <- function(x, y) {
  partes <- function(s) {
    list(
      signo = if (startsWith(s, "-")) -1L else 1L,
      cifras = rev(as.integer(strsplit(sub("^-", "", s), "", fixed = TRUE)[[1L]]))
    )
  }
  a <- partes(x)
  b <- partes(y)
  b$signo <- -b$signo
  largo <- max(length(a$cifras), length(b$cifras))
  ca <- c(a$cifras, integer(largo - length(a$cifras)))
  cb <- c(b$cifras, integer(largo - length(b$cifras)))
  if (a$signo == b$signo) {
    cifras <- ca + cb
    signo <- a$signo
  } else {
    distintas <- which(rev(ca) != rev(cb))
    if (!length(distintas)) return(0)
    primera <- distintas[[1L]]
    if (rev(ca)[[primera]] > rev(cb)[[primera]]) {
      cifras <- ca - cb
      signo <- a$signo
    } else {
      cifras <- cb - ca
      signo <- b$signo
    }
  }
  acarreo <- 0L
  for (i in seq_along(cifras)) {
    valor <- cifras[[i]] + acarreo
    acarreo <- valor %/% 10L
    cifras[[i]] <- valor %% 10L
  }
  if (acarreo > 0L) cifras <- c(cifras, acarreo)
  signo * as.numeric(paste(rev(cifras), collapse = ""))
}

# El rango de una columna, con sus extremos como VALORES. Tiene tres formas, y la
# segunda faltaba: con un `integer64` por encima de 2^53 -o un entero escrito
# como texto que no entra en un doble- el perfil publica `minimo` y `maximo` en
# NA y conserva los extremos exactos como texto, y esta funcion solo miraba los
# dobles. `2^60 + c(0, 10, 20)` contra `2^60 + c(0, 10, 1000)` -el ancho se
# multiplica por cuarenta y nueve- daba cero filas, mientras
# `comparar_equivalencia()` veia el `maximo_exacto` distinto.
.rango_perfil <- function(fila) {
  exacto_minimo <- .extremo_exacto_deriva(fila, "minimo_exacto")
  exacto_maximo <- .extremo_exacto_deriva(fila, "maximo_exacto")
  hay_exactos <- !is.na(exacto_minimo) && !is.na(exacto_maximo)
  if (!hay_exactos) {
    exacto_minimo <- NA_character_
    exacto_maximo <- NA_character_
  }
  if (is.finite(fila$minimo) && is.finite(fila$maximo)) {
    return(list(
      tipo = "numerico", minimo = fila$minimo, maximo = fila$maximo,
      minimo_exacto = exacto_minimo, maximo_exacto = exacto_maximo,
      texto = .texto_rango_deriva(
        .texto_numero_deriva(fila$minimo), .texto_numero_deriva(fila$maximo)
      )
    ))
  }
  if (hay_exactos) {
    return(list(
      tipo = "numerico",
      minimo = suppressWarnings(as.numeric(exacto_minimo)),
      maximo = suppressWarnings(as.numeric(exacto_maximo)),
      minimo_exacto = exacto_minimo, maximo_exacto = exacto_maximo,
      texto = .texto_rango_deriva(exacto_minimo, exacto_maximo)
    ))
  }
  if (!is.na(fila$minimo_fecha) && !is.na(fila$maximo_fecha)) {
    limites <- tryCatch(
      suppressWarnings(as.POSIXct(
        c(fila$minimo_fecha, fila$maximo_fecha), tz = "UTC"
      )),
      error = function(e) as.POSIXct(c(NA, NA), tz = "UTC")
    )
    if (length(limites) != 2L || anyNA(limites)) return(NULL)
    a <- as.numeric(limites[[1L]])
    b <- as.numeric(limites[[2L]])
    return(list(
      tipo = "fecha", minimo = a, maximo = b,
      minimo_exacto = NA_character_, maximo_exacto = NA_character_,
      texto = .texto_rango_deriva(fila$minimo_fecha, fila$maximo_fecha)
    ))
  }
  NULL
}

.rango_con_exactos <- function(rango) {
  !is.null(rango$minimo_exacto) && !is.na(rango$minimo_exacto) &&
    !is.null(rango$maximo_exacto) && !is.na(rango$maximo_exacto)
}

# Dos rangos son iguales si sus extremos son los mismos VALORES. Se comparaba su
# texto impreso, que redondea a ocho cifras y sigue a `scipen`:
# `1234567890000000 + c(0, 50000, 123456)` contra `+ c(0, 50000, 9999999)` -un
# ancho que se multiplica por ochenta- daba cero filas con `scipen = 0` y una fila
# `sospechoso` con `scipen = 100`, sobre los mismos perfiles; y `1.000000030`
# contra `1.000000044` no salia con ninguna. La magnitud se calculaba bien, pero
# nunca llegaba a calcularse si los dos textos coincidian. Los extremos son
# valores de la columna, no resultados de una cuenta, asi que la igualdad es
# exacta: un cambio chico sale como fila `ok`, que es lo que promete la ayuda.
.rangos_iguales_deriva <- function(anterior, actual) {
  if (!identical(anterior$tipo, actual$tipo)) return(FALSE)
  if (.rango_con_exactos(anterior) && .rango_con_exactos(actual)) {
    return(identical(anterior$minimo_exacto, actual$minimo_exacto) &&
             identical(anterior$maximo_exacto, actual$maximo_exacto))
  }
  isTRUE(anterior$minimo == actual$minimo) &&
    isTRUE(anterior$maximo == actual$maximo)
}

.magnitud_rango <- function(anterior, actual) {
  if (is.null(anterior) || is.null(actual) ||
      anterior$tipo != actual$tipo) return(Inf)
  if (.rango_con_exactos(anterior) && .rango_con_exactos(actual)) {
    ancho <- abs(.diferencia_entera_exacta(
      anterior$maximo_exacto, anterior$minimo_exacto
    ))
    cambio_minimo <- abs(.diferencia_entera_exacta(
      actual$minimo_exacto, anterior$minimo_exacto
    ))
    cambio_maximo <- abs(.diferencia_entera_exacta(
      actual$maximo_exacto, anterior$maximo_exacto
    ))
  } else {
    ancho <- abs(anterior$maximo - anterior$minimo)
    cambio_minimo <- abs(actual$minimo - anterior$minimo)
    cambio_maximo <- abs(actual$maximo - anterior$maximo)
  }
  escala <- if (is.finite(ancho) && ancho > 0) ancho else {
    max(abs(c(anterior$minimo, anterior$maximo)), 1)
  }
  max(cambio_minimo, cambio_maximo) / escala
}

# Si el rango de la columna esta tapado por la proteccion en este lado. Basta
# UNO de sus campos, con la misma regla que ya usa `comparar_equivalencia()` -la
# marca `dato_personal_protegido`, el marcador `[valor protegido]` o el detalle
# de proteccion de orden-, para que las dos hermanas no puedan contestar distinto
# sobre la misma columna.
.proteccion_conocida_deriva <- function(fila) {
  any(c("dato_personal_protegido", "detalle_proteccion_personal") %in%
        names(fila)) || !is.null(.rango_perfil(fila))
}

.rango_protegido_deriva <- function(fila) {
  campos <- intersect(
    c("minimo", "maximo", "minimo_fecha", "maximo_fecha",
      "minimo_exacto", "maximo_exacto"),
    names(fila)
  )
  any(vapply(campos, function(campo) {
    isTRUE(.campo_protegido_equivalencia(fila, campo, 1L))
  }, logical(1L)))
}

.patrones_perfil <- function(perfil, indice) {
  x <- perfil$patrones[[indice]]
  if (!inherits(x, "data.frame") || !all(c("patron", "proporcion") %in% names(x))) {
    return(data.frame(
      patron = character(), proporcion = numeric(), stringsAsFactors = FALSE
    ))
  }
  x[c("patron", "proporcion")]
}

# Un resumen de patrones guarda los `max_patrones` mas frecuentes. Si la columna
# tenia mas formas, las que no entraron siguen en los datos con una proporcion
# que no supera la menor del resumen. Devuelve esa cota, o NA si el resumen esta
# completo y un patron ausente de verdad no esta. El perfil publica cuantas
# formas habia en `n_patrones_distintos`; un perfil sin ese dato y con
# `max_patrones` filas se trata como lleno, que es la lectura que no afirma de
# mas.
.techo_fuera_del_resumen <- function(perfil, indice, tabla) {
  filas <- nrow(tabla)
  if (!filas) return(NA_real_)
  total <- attr(perfil$patrones[[indice]], "n_patrones_distintos", exact = TRUE)
  lleno <- if (is.numeric(total) && length(total) == 1L && !is.na(total)) {
    total > filas
  } else {
    maximo <- perfil$meta[["max_patrones", exact = TRUE]]
    is.numeric(maximo) && length(maximo) == 1L && !is.na(maximo) &&
      filas >= maximo
  }
  proporciones <- tabla$proporcion[!is.na(tabla$proporcion)]
  if (!lleno || !length(proporciones)) return(NA_real_)
  min(proporciones)
}

.clave_patron <- function(x) {
  valores <- ifelse(is.na(x), "<NA>", as.character(x))
  .clave_bytes(valores)
}

# Los diagnosticos que el perfil declino, como `columna tipo`, para poder
# distinguir un hallazgo resuelto de uno que no se volvio a evaluar.
# Un diagnostico tambien deja de evaluarse cuando desaparece la DECLARACION que
# lo habilitaba, y eso no queda en `cobertura_diagnosticos`: no es que el
# diagnostico se haya declinado sobre una columna, es que nunca se le pidio.
#
# Medido el 2026-09-05: la misma tabla perfilada con `clave = "id"` y despues sin
# `clave` informaba `clave_no_unica` como **resuelto, severidad ok**. La clave
# seguia estando duplicada; lo unico que cambio fue el argumento. El propio
# `man/comparar_perfiles.Rd` lo declara al reves de lo que pasaba: "dejar de
# mirar no es lo mismo que arreglar".
#
# Se mira `meta$declaracion_clave`, que se agrego para esto: `meta$clave` guarda el
# RESULTADO de comprobar la clave y queda `NULL` cuando sale limpia, asi que no
# distingue "no se declaro" de "se declaro y estaba bien".
#
# `clave` no era la unica, como decia este comentario: apagar
# `casi_duplicados_vocabulario` -lo que `?perfilar` recomienda para tablas
# grandes- o `ausencia_estructural` publicaba el hallazgo como **resuelto,
# severidad ok** con los mismos datos, y retirar `columnas_sin_ceros`,
# `columnas_no_negativas` o `columnas_personales`, igual (refutacion 35-D, M4).
# El dato estaba en `meta` y la comparacion no lo leia. Ahora la lista sale de
# `.ARGUMENTOS_PERFIL_DERIVA`, que clasifica TODOS los argumentos de
# `perfilar()`, y una prueba exige que no falte ninguno.
#
# Cada declaracion que habilita hallazgos tiene una forma:
# * `tabla`: habilita sobre lo declarado (`clave`). Retirarla o declararla sobre
#   otras columnas deja sin evaluar la anterior: con `clave = "id"` y despues
#   `clave = "ci"`, `clave_no_unica` de `id` salia `resuelto, ok` con `id`
#   todavia duplicado (refutacion 35-D, segunda vuelta).
# * `interruptor`: un logico; habilita con `TRUE`.
# * `columnas`: un conjunto de columnas; habilita en las declaradas, asi que
#   retirarla de una columna deja sin evaluar los hallazgos de esa columna.
# * `duplicados`: `duplicados_aproximados` compara filas sobre un conjunto de
#   columnas, y sus hallazgos llevan ese conjunto como columna (`nombre,
#   ciudad`): si el perfil nuevo no lo corrio, o lo corrio sobre otras
#   columnas, los del anterior no se volvieron a evaluar.
# Un perfil de otra version que no registra el campo no se declina: ahi no se
# sabe.
.hallazgos_por_declaracion <- function() {
  habilitan <- Filter(
    function(x) "habilita" %in% x$clase, .ARGUMENTOS_PERFIL_DERIVA
  )
  stats::setNames(lapply(habilitan, function(x) {
    x[c("meta", "forma", "hallazgos")]
  }), names(habilitan))
}

# Lo que `duplicados_aproximados` evaluo en un perfil: si corrio y pudo correr,
# sobre que columnas y con que parametros. `NULL` si no corrio.
.duplicados_evaluados_perfil <- function(perfil) {
  x <- if (is.list(perfil)) perfil[["duplicados_aproximados", exact = TRUE]] else NULL
  if (!is.list(x) || !isTRUE(x[["disponible", exact = TRUE]])) return(NULL)
  columnas <- as.character(x[["columnas", exact = TRUE]])
  parametro <- function(nombre) {
    valor <- x[[nombre, exact = TRUE]]
    if (is.null(valor) || !length(valor)) "ninguno" else {
      paste(as.character(valor), collapse = "/")
    }
  }
  list(
    columnas = .clave_bytes(.ordenar_por_bytes(unique(.nombres_para_operar(columnas)))),
    texto_columnas = paste(.ordenar_por_bytes(unique(columnas)), collapse = "/"),
    parametros = c(
      metodo = parametro("metodo"), umbral = parametro("umbral"),
      p = parametro("p"), max_largo_valor = parametro("max_largo_valor")
    )
  )
}

# Diagnosticos que comparan valores ENTRE SI y que solo corren sobre texto.
#
# Cuando una columna pasa de texto a numero, los diagnosticos de texto dejan de
# correr sobre ella. Para la mayoria eso es una resolucion de verdad: un numero
# no puede tener espacios sobrantes, mayusculas inconsistentes ni controles
# invisibles, asi que el problema dejo de existir. Pero un diagnostico que
# mira la RELACION entre valores no desaparece con el tipo: `1200` y `1201`
# siguen ahi despues de convertirlos, solo que nadie los volvio a comparar.
#
# Medido el 2026-09-21: el plan recomendado convertia `"1200%"`/`"1201%"` a
# numero -ninguna accion atendia los casi duplicados- y la deriva informaba
# `casi_duplicados_vocabulario` como **resuelto, severidad ok**.
#
# La direccion opuesta no tiene este problema, y se midio: una columna que pasa
# de numero a texto se vuelve a inferir como numerica y `outliers` sigue
# corriendo.
# Tiene que coincidir con lo que `.hallazgos_por_diagnostico` cuelga de
# `proximidad_vocabulario`: son los hallazgos de la misma familia, y una lista
# que nombra a uno y deja al hermano afuera es una regla escrita dos veces. Una
# prueba exige que coincidan.
.diagnosticos_relacion_textual <- c(
  "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
)

# Los hallazgos de esos diagnosticos, en columnas que en el perfil nuevo ya no
# son texto, como `columna tipo`.
.declinados_por_cambio_de_tipo <- function(anterior, actual) {
  columnas_a <- anterior$columnas
  columnas_b <- actual$columnas
  if (!inherits(columnas_a, "data.frame") || !inherits(columnas_b, "data.frame") ||
      !all(c("columna", "tipo_declarado") %in% names(columnas_a)) ||
      !all(c("columna", "tipo_declarado") %in% names(columnas_b))) {
    return(character())
  }
  es_texto <- function(tipo) {
    .minusculas_ascii(as.character(tipo)) %in% c("texto", "character", "factor", "categoria")
  }
  nombres_a <- .nombres_para_operar(columnas_a$columna)
  nombres_b <- .nombres_para_operar(columnas_b$columna)
  comunes <- intersect(nombres_a, nombres_b)
  dejaron <- comunes[
    es_texto(columnas_a$tipo_declarado[match(comunes, nombres_a)]) &
      !es_texto(columnas_b$tipo_declarado[match(comunes, nombres_b)])
  ]
  if (!length(dejaron)) return(character())
  as.vector(outer(dejaron, .diagnosticos_relacion_textual, paste))
}

# Las claves `columna tipo` que el perfil nuevo no evaluo porque se retiro o se
# apago la declaracion que las habilita. El nombre de cada clave es el argumento
# de `perfilar()`, que la descripcion de la fila nombra.
.declinados_por_declaracion_retirada <- function(anterior, actual) {
  declinados <- character()
  meta_a <- if (is.list(anterior$meta)) anterior$meta else list()
  meta_b <- if (is.list(actual$meta)) actual$meta else list()
  hallazgos_a <- anterior$hallazgos
  columnas_hallazgo <- if (inherits(hallazgos_a, "data.frame") &&
                           all(c("columna", "tipo_hallazgo") %in% names(hallazgos_a))) {
    ifelse(is.na(hallazgos_a$columna), "<tabla>", as.character(hallazgos_a$columna))
  } else character()
  tipos_hallazgo <- if (length(columnas_hallazgo)) {
    as.character(hallazgos_a$tipo_hallazgo)
  } else character()
  declaraciones <- .hallazgos_por_declaracion()
  for (argumento in names(declaraciones)) {
    declaracion <- declaraciones[[argumento]]
    if (identical(declaracion$forma, "duplicados")) {
      evaluado_a <- .duplicados_evaluados_perfil(anterior)
      evaluado_b <- .duplicados_evaluados_perfil(actual)
      if (is.null(evaluado_a) ||
          (!is.null(evaluado_b) &&
             identical(evaluado_a$columnas, evaluado_b$columnas))) {
        next
      }
      columnas <- unique(columnas_hallazgo[tipos_hallazgo %in% declaracion$hallazgos])
      if (!length(columnas)) next
      claves <- as.vector(outer(
        columnas, declaracion$hallazgos, .clave_declinacion_deriva
      ))
      declinados <- c(
        declinados, stats::setNames(claves, rep(argumento, length(claves)))
      )
      next
    }
    campo <- declaracion$meta
    if (!campo %in% names(meta_a) || !campo %in% names(meta_b)) {
      # `declaracion_clave` es anterior a esta regla y su ausencia decia
      # "no se declaro": se conserva esa lectura.
      if (!identical(declaracion$forma, "tabla")) next
    }
    valor_a <- meta_a[[campo, exact = TRUE]]
    valor_b <- meta_b[[campo, exact = TRUE]]
    columnas <- switch(
      declaracion$forma,
      tabla = {
        declarada_a <- .clave_bytes(.ordenar_por_bytes(unique(
          .nombres_para_operar(as.character(valor_a))
        )))
        declarada_b <- .clave_bytes(.ordenar_por_bytes(unique(
          .nombres_para_operar(as.character(valor_b))
        )))
        if (length(declarada_a) > 0L && !identical(declarada_a, declarada_b)) {
          "<tabla>"
        } else character()
      },
      interruptor = if (isTRUE(as.logical(valor_a)[1L]) &&
                        identical(as.logical(valor_b)[1L], FALSE)) {
        unique(columnas_hallazgo[tipos_hallazgo %in% declaracion$hallazgos])
      } else character(),
      columnas = {
        retiradas <- setdiff(
          .nombres_para_operar(as.character(valor_a)),
          .nombres_para_operar(as.character(valor_b))
        )
        retiradas[!is.na(retiradas)]
      },
      character()
    )
    if (!length(columnas)) next
    claves <- as.vector(outer(
      columnas, declaracion$hallazgos, .clave_declinacion_deriva
    ))
    declinados <- c(declinados, stats::setNames(claves, rep(argumento, length(claves))))
  }
  declinados
}

.clave_declinacion_deriva <- function(columna, tipo) {
  paste(.nombres_para_operar(columna), as.character(tipo))
}

# Si un hallazgo que desaparecio nombra una columna que el perfil nuevo ya no
# tiene: ahi no se presenta como resuelto, porque la columna misma se retiro.
# Un hallazgo de varias columnas las nombra juntas -`nombre, ciudad` en los
# duplicados entre columnas, `doble,monto` y `monto,doble,total` en las
# relaciones-, y ese texto no es ninguna columna: la guarda preguntaba si el
# texto era una columna del perfil nuevo, y lo descartaba siempre. Un par que se
# resolvio de verdad, con el diagnostico encendido en las dos corridas, o uno
# que no se volvio a evaluar, no publicaba ninguna fila: 100 pares de
# `duplicados_exactos_columnas` y una `relacion_aritmetica_columnas`
# desaparecian sin rastro (refutacion 35-D, segunda vuelta). Ahora se descarta
# solo si el texto nombra una columna retirada, entera o como una de sus partes.
.hallazgo_nombra_columna_retirada <- function(columna, retiradas) {
  retiradas <- .nombres_para_operar(as.character(retiradas))
  retiradas <- retiradas[!is.na(retiradas)]
  if (!length(retiradas)) return(FALSE)
  texto <- .nombres_para_operar(as.character(columna))
  if (texto %in% retiradas) return(TRUE)
  partes <- trimws(strsplit(texto, ",", fixed = TRUE)[[1L]])
  any(partes %in% retiradas)
}


# Lee un campo numerico de la fila de una columna, tolerando que no exista -un
# perfil de otra version puede no traerlo-.
.valor_columna_deriva <- function(fila, campo) {
  if (is.null(fila) || !campo %in% names(fila)) return(NA_real_)
  suppressWarnings(as.numeric(fila[[campo]][[1L]]))
}

# Un perfil guardado por otra version de lupa puede no traer un campo que esta
# version compara. Medido antes de arreglarlo: sin `tasa_distintos`, sin
# `prop_faltantes_totales`, sin `minimo` o sin `maximo`, comparar_perfiles()
# abortaba con el mensaje interno de R -"valor ausente donde TRUE/FALSE es
# necesario"-, que no le dice a nadie que el problema es la version del perfil.
# En otros dos campos era peor que abortar: sin `tipo_inferido` publicaba una
# fila de cambio "modificado" atribuida a los datos, cuando lo que habia
# cambiado era la forma del perfil; sin `n_distintos` la evidencia salia con el
# valor anterior en blanco.
#
# Se compara la interseccion y lo ausente se declara, que es el mismo mecanismo
# que ya usan la politica de patrones, la de centinelas y la de aplicabilidad.
# Los nombres de los grupos son los `aspecto` que ya publica la comparacion, asi
# que la fila de no comparabilidad cae junto a las filas que reemplaza.
.campos_comparables_deriva <- function(anterior, actual) {
  requeridos <- list(
    tipo_declarado = "tipo_declarado",
    tipo_inferido = "tipo_inferido",
    faltantes = "prop_faltantes_totales",
    cardinalidad = c("tasa_distintos", "n_distintos"),
    rango = c("minimo", "maximo", "minimo_fecha", "maximo_fecha"),
    # La unidad es la otra mitad del arreglo de la unidad que no viajaba: el
    # campo en la tabla de columnas, sin un aspecto que lo compare, dejaba el
    # defecto en pie. Medido antes de escribirlo: con `unidad` puesto a mano en
    # las dos entregas -`m` contra `km`-, la deriva solo informaba el rango, que
    # es exactamente "una entrega donde cambio la unidad no muestra cambio".
    unidad = "unidad"
  )
  nombres_a <- names(anterior$columnas)
  nombres_b <- names(actual$columnas)
  ausentes <- lapply(requeridos, function(campos) {
    list(
      anterior = setdiff(campos, nombres_a),
      actual = setdiff(campos, nombres_b)
    )
  })
  falta_alguno <- vapply(
    ausentes,
    function(x) length(x$anterior) > 0L || length(x$actual) > 0L,
    logical(1)
  )
  list(
    comparables = names(requeridos)[!falta_alguno],
    ausentes = ausentes[falta_alguno]
  )
}

# El nombre con que la cobertura declina un diagnostico no siempre es el del
# hallazgo que ese diagnostico produce. La cobertura dice
# `proximidad_vocabulario` y el hallazgo se llama `casi_duplicados_vocabulario`,
# asi que la clave de declinacion nunca casaba: un diagnostico que el perfil
# nuevo declinaba POR ESCRITO -"no se evaluo la proximidad del vocabulario"- se
# informaba **resuelto, severidad ok**, con `Norte` y `norte` todavia en la
# columna. El primer motivo de `no_evaluado` no disparaba nunca para esta
# familia.
#
# Los nombres que ya coinciden no necesitan entrada, y los que no producen
# hallazgo propio tampoco. Una prueba recorre TODOS los nombres que la
# cobertura escribe y exige que cada uno caiga en alguno de los tres casos,
# para que un diagnostico nuevo no se desencuentre en silencio.
.hallazgos_por_diagnostico <- list(
  proximidad_vocabulario = c(
    "casi_duplicados_vocabulario", "variantes_equifrecuentes_vocabulario"
  ),
  ley_benford = "desviacion_benford",
  # La geometria se declina con `agregar_cobertura()`, un constructor que la
  # primera version de la guarda no miraba: `validez_geometria` contra
  # `geometria_invalida` salio "resuelto" aun con la validez sin evaluar.
  validez_geometria = "geometria_invalida",
  dominio_geometria = "coordenada_fuera_dominio",
  # Sin perfil geometrico no se evalua NINGUNO de los hallazgos de geometria.
  perfil_geometria = c(
    "coordenada_fuera_dominio", "crs_no_declarado", "geometria_invalida",
    "geometria_vacia", "tipos_geometria_mixtos"
  )
)

.diagnosticos_declinados_deriva <- function(perfil) {
  cobertura <- perfil$cobertura_diagnosticos
  if (!inherits(cobertura, "data.frame") || !nrow(cobertura) ||
        !all(c("columna", "diagnostico") %in% names(cobertura))) {
    return(character())
  }
  diagnosticos <- as.character(cobertura$diagnostico)
  columnas <- as.character(cobertura$columna)
  unlist(lapply(seq_along(diagnosticos), function(i) {
    tipos <- .hallazgos_por_diagnostico[[diagnosticos[[i]]]]
    if (is.null(tipos)) tipos <- diagnosticos[[i]]
    # Una fila que declina un diagnostico sobre varias columnas las nombra como
    # una lista -`a,b,c`-: declina tambien cada una, para que un hallazgo de
    # varias columnas que nombra alguna no se publique resuelto.
    nombres <- columnas[[i]]
    if (!is.na(nombres) && grepl(",", nombres, fixed = TRUE, useBytes = TRUE)) {
      nombres <- unique(c(nombres, trimws(strsplit(nombres, ",", fixed = TRUE)[[1L]])))
    }
    as.vector(outer(nombres, tipos, .clave_declinacion_deriva))
  }), use.names = FALSE)
}

.hallazgo_cuenta_filas_tabla <- function(fila) {
  if (!all(c("columna", "unidad_conteo") %in% names(fila))) return(FALSE)
  unidad <- as.character(fila$unidad_conteo[[1L]])
  tabla <- is.na(fila$columna[[1L]]) ||
    identical(as.character(fila$columna[[1L]]), "<tabla>")
  isTRUE(identical(unidad, "fila")) && isTRUE(tabla)
}

.resumir_hallazgos_deriva <- function(perfil) {
  x <- perfil$hallazgos
  if (!nrow(x)) {
    return(data.frame(
      clave = character(), columna = character(), tipo_hallazgo = character(),
      severidad = character(), evidencia = character(),
      unidad_conteo = character(), stringsAsFactors = FALSE
    ))
  }
  columna <- ifelse(is.na(x$columna), "<tabla>", as.character(x$columna))
  clave <- .clave_bytes(paste(
    .clave_bytes(columna), .clave_bytes(x$tipo_hallazgo), sep = "\034"
  ))
  grupos <- .dividir_por_bytes(seq_len(nrow(x)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    nivel <- match(as.character(x$severidad[indices]),
                   c("ok", "sospechoso", "error"))
    elegido <- indices[which.max(nivel)]
    data.frame(
      clave = clave[[elegido]], columna = columna[[elegido]],
      tipo_hallazgo = x$tipo_hallazgo[[elegido]],
      severidad = as.character(x$severidad[[elegido]]),
      evidencia = x$evidencia[[elegido]],
      unidad_conteo = if ("unidad_conteo" %in% names(x)) {
        as.character(x$unidad_conteo[[elegido]])
      } else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, unname(partes))
  rownames(resultado) <- NULL
  resultado
}

# La regla de "alcanza el umbral", escrita UNA vez. Estaba escrita de dos
# maneras en este mismo archivo: los faltantes y la cardinalidad comparaban con
# tolerancia y el rango y los patrones sin ella, asi que dos pares que publican
# la misma magnitud recibian veredictos distintos segun el aspecto. La
# documentacion publica un unico umbral para todos.
#
# Dos condiciones, y la primera importa tanto como la segunda:
#   - una magnitud por debajo del ruido de la coma flotante NO es un cambio,
#     por chico que sea el umbral. Sin esto, un umbral menor que la tolerancia
#     hacia que un delta CERO entrara por la rama de "significativo".
#   - y el umbral se compara con tolerancia, porque 0,70 - 0,65 da
#     0,049999999999999933 y 0,75 - 0,70 da 0,050000000000000044.
.alcanza_umbral_deriva <- function(magnitud, umbral) {
  # La tolerancia es RELATIVA al umbral: absorbe el error de la resta -0,70
  # menos 0,65 da 0,049999999999999933- sin mover el corte cuando el umbral es
  # chico o cero.
  # Escala con las MAGNITUDES en juego, no con un piso de 1: el error de la
  # resta es del orden de `eps * max(|a|, |b|)`, y un piso fijo se traga
  # cambios reales cuando el umbral es diminuto.
  tolerancia <- sqrt(.Machine$double.eps) * max(abs(magnitud), abs(umbral))
  if (length(magnitud) != 1L || is.na(magnitud)) return(FALSE)
  if (is.infinite(magnitud)) return(TRUE)
  # Una diferencia EXACTAMENTE nula no es un cambio, con ningun umbral. Eso es
  # lo unico que hay que excluir: la primera version usaba un piso ABSOLUTO
  # -`abs(magnitud) > sqrt(.Machine$double.eps)`- y con eso borraba cambios
  # legitimamente diminutos, como un 1e-10 con `umbral = 0`, que la API deja
  # pedir. La tolerancia esta para estabilizar la comparacion CONTRA EL CORTE,
  # no para redefinir como cero toda magnitud pequena.
  if (magnitud == 0) return(FALSE)
  abs(magnitud) >= umbral - tolerancia
}

# Los dos objetos llevan su medicion fechada y la comparacion publica esas
# fechas en cada fila. Con los argumentos cambiados, la direccion publicada es
# la contraria de la serie medida -un hallazgo que se agravo sale `atenuado`, un
# patron que aparecio sale `desaparecido`- y la afirmacion contradice a las
# fechas de su propia fila. No se puede adivinar cual quiso poner quien llama:
# se para temprano y se dice como arreglarlo. Si alguna fecha falta o las dos
# son iguales, no hay nada que verificar y sigue como siempre.
.exigir_orden_temporal <- function(fecha_anterior, fecha_actual, funcion,
                                   declaradas = TRUE) {
  # Solo cuando las dos fechas las declaro quien llama: la de omision es la hora
  # de la corrida, y con dos perfiles hechos en la misma sesion el orden de los
  # relojes no dice nada. Lo atrapo la suite, sobre pruebas que perfilan dos
  # veces seguidas sin declarar fecha.
  if (!isTRUE(declaradas)) return(invisible(NULL))
  validas <- function(x) length(x) == 1L && !is.na(x) && is.finite(as.numeric(x))
  if (!validas(fecha_anterior) || !validas(fecha_actual)) return(invisible(NULL))
  # Se compara a la resolucion que el paquete PUBLICA -el segundo-, no a la del
  # reloj: dos perfiles hechos en el mismo segundo no son una serie, y la guarda
  # los rechazaba con un mensaje que imprimia dos veces la misma fecha. Lo
  # atrapo la suite, sobre una prueba que perfila dos veces seguidas.
  anterior_seg <- trunc(as.numeric(fecha_anterior))
  actual_seg <- trunc(as.numeric(fecha_actual))
  if (anterior_seg <= actual_seg) return(invisible(NULL))
  .detener(
    "`anterior` esta fechado despues que `actual` (",
    format(fecha_anterior, "%Y-%m-%d %H:%M:%S"), " contra ",
    format(fecha_actual, "%Y-%m-%d %H:%M:%S"), "), asi que ", funcion,
    " publicaria la direccion del cambio al reves. Invierta los argumentos.",
    call. = FALSE
  )
}

#' Comparar dos perfiles y detectar deriva estructural
#'
#' Compara entregas sin exigir que tengan las mismas columnas. Devuelve cambios
#' de esquema, tipos, faltantes, cardinalidad, rango, patrones y hallazgos como
#' un objeto de datos filtrable.
#'
#' @param anterior,actual Objetos producidos por [perfilar()].
#' @param umbral_cambio Diferencia mínima para considerar significativo un
#'   cambio de proporción, cardinalidad o rango relativo. Cinco puntos
#'   porcentuales evita elevar variaciones pequeñas a hallazgo.
#' @param umbral_error Diferencia a partir de la cual un aumento de faltantes o
#'   un cambio de patrón —uno que aparece, desaparece o cambia de proporción— se
#'   clasifica como `error`.
#'
#' @return Data frame `deriva_perfil`. `severidad` usa el factor ordenado
#'   `ok < sospechoso < error`; los cambios menores permanecen como filas `ok`
#'   para que la serie sea exportable sin ocultar diferencias.
#'   El atributo `campos_protegidos` declara lo que no se comparó porque la
#'   protección de datos personales lo tapa: un data frame con `columna`,
#'   `campo` y `lado` —`anterior` o `actual`—, la misma forma que usa
#'   [comparar_equivalencia()]. Hoy el único campo que puede figurar es
#'   `rango`. Está siempre, vacío si no hay nada que declarar, y `rbind()` une
#'   los de todas las derivas.
#'
#' @details
#' Los patrones se comparan sobre el resumen acotado que conserva cada perfil,
#' no sobre los valores originales ni una distribución completa. Si las dos
#' corridas usaron configuraciones de patrones diferentes, se informa un error
#' de comparabilidad y esa parte de la comparación se omite.
#'
#' Los nombres de columnas y las configuraciones que contienen texto del usuario
#' se comparan por sus bytes, no por la marca de codificación ni por la
#' intercalación del locale.
#'
#' Eso alcanza mientras los bytes lleguen intactos, y hay un caso en que no
#' llegan y **no depende de este paquete**: el formato RDS 3 —el de [saveRDS()]
#' por omisión— anota la codificación nativa de quien escribe, y al leer
#' *traduce* desde ella el texto que no declara la suya. Un perfil guardado bajo
#' `LC_CTYPE=C` y releído bajo otro locale puede volver con el texto cambiado;
#' en Windows el cambio es silencioso, porque ahí la conversión no falla.
#'
#' Cuando eso pasa, el perfil releído **está** alterado, y conviene saber qué
#' alcanza a ver esta función. Compara las **propiedades medidas** de cada
#' columna, los hallazgos, el alcance del resumen y las configuraciones
#' declaradas —no los valores uno por uno—: la columna `aspecto` del resultado
#' nombra, fila por fila, qué se comparó, y es la enumeración viva. **Un cambio de texto que no
#' altera ninguna propiedad medida no aparece**: por ejemplo, si la moda de una
#' columna pasa de `"Basico"` a `"Casico"` —mismo largo, misma forma de patrón,
#' mismos conteos—, la comparación devuelve cero filas. Por eso el remedio ante
#' una relectura sospechosa no es confiar en la comparación para detectarla.
#'
#' El remedio es una línea, `saveRDS(perfil, archivo, version = 2)`, porque ese
#' formato no anota codificación nativa y por lo tanto no traduce nada. La
#' persistencia propia del paquete —[guardar_historico()] y
#' [guardar_analisis()]— ya lo usa.
#' Las claves internas que agrupan hallazgos también se normalizan por bytes;
#' por eso dos perfiles idénticos no emiten avisos espurios bajo C.
#'
#' Que la **moda** no se compare es una **decisión**, no un pendiente: la moda es
#' un valor, no una propiedad medida, y cambia con cualquier corrimiento normal de
#' los datos, así que compararla llenaría la deriva de filas que no son deterioro.
#' Quien necesite detectar una relectura corrompida tiene el remedio de una línea
#' que está dos párrafos más abajo, que la evita en lugar de detectarla.
#'
#' Entre las propiedades comparadas está la **unidad** en la que la columna
#' publica sus cifras, con la misma severidad que un cambio de tipo: dos entregas
#' con los mismos valores y la unidad cambiada de `m` a `km` son un cambio real
#' —la cifra dejó de significar lo mismo— y antes devolvían cero filas. Un perfil
#' guardado antes de que ese campo existiera no hace mentir la comparación: esa
#' parte se declara **no comparable**, como el resto de los campos ausentes.
#'
#' El **rango** se compara por los valores de sus extremos, no por su texto
#' impreso: un cambio chico sale como fila `ok` y uno que alcanza
#' `umbral_cambio` —medido contra el ancho del rango anterior— como
#' `sospechoso`, sin que `options(scipen)` ni `digits` muevan el veredicto.
#' Cuando el perfil conserva los extremos exactos de un entero que no entra en
#' un doble —un `integer64` por encima de 2^53, o un entero escrito como
#' texto—, esos extremos se comparan con aritmética exacta. Los números se
#' publican con la escritura más corta que vuelve al valor, así que dos
#' extremos distintos nunca se imprimen iguales; la marca decimal sigue a
#' `OutDec`, y con coma decimal los extremos se separan con punto y coma
#' (`[1,5; 3]`).
#'
#' Una columna **protegida** no publica su rango, y la comparación tampoco lo
#' compara: lo declara en el atributo `campos_protegidos`, con la misma regla
#' que [comparar_equivalencia()]. Si la protección de una columna cambia entre
#' las corridas con la misma política —por ejemplo, porque la proporción de
#' documentos válidos cruzó el umbral de verificación—, el cambio se informa
#' con una fila propia, `aspecto = "proteccion"` y severidad `sospechoso`, en
#' lugar de un cambio de rango que no ocurrió en los datos. Si lo que cambió es
#' la política entera, ya lo dice la fila `configuracion_proteccion` y no se
#' repite por columna.
#'
#' Además de los patrones que aparecen y desaparecen, se informa el **cambio de
#' proporción** de un patrón que sigue en el resumen cuando alcanza
#' `umbral_cambio`: `sospechoso`, o `error` desde `umbral_error`, en las dos
#' direcciones. Que los cambios menores no generen fila es una **decisión**, a
#' diferencia de los faltantes: serían una fila por patrón en cada entrega. Si
#' el resumen de un lado estaba **lleno** —la columna tenía más formas que
#' `max_patrones`—, un patrón que falta en él pudo quedar por debajo del corte,
#' y no se afirma que apareció o desapareció: la fila dice `no_comparable`, con
#' severidad `ok`, salvo que el cambio que sí se puede garantizar —contra la
#' menor proporción que guardaba ese resumen— alcance el umbral; entonces dice
#' `modificado` y `delta` es esa cota.
#'
#' La **cardinalidad** no tiene una dirección buena: que caiga la tasa de
#' valores distintos de un identificador es un deterioro, y que suba la de una
#' columna de categorías también. Por **decisión**, un cambio significativo de
#' cardinalidad es `sospechoso` en los dos sentidos. Los faltantes sí la
#' tienen: que bajen es una mejora y queda `ok`.
#'
#' Las columnas que aparecen o desaparecen generan cambios estructurales de
#' severidad `error`, pero no impiden comparar las columnas compartidas. Un
#' hallazgo de una columna retirada no se presenta como resuelto.
#'
#' Un hallazgo que ya no aparece se informa como `resuelto` sólo si el
#' diagnóstico volvió a evaluarse. Si el perfil nuevo lo declinó —y lo dice en
#' su `cobertura_diagnosticos`—, el cambio se informa como `no_evaluado` con
#' severidad `sospechoso`, porque no se sabe si el hallazgo sigue: dejar de
#' mirar no es lo mismo que arreglar. Lo mismo si el perfil nuevo apagó el
#' diagnóstico con su argumento o retiró la declaración que lo habilita, cosa
#' que se lee en `meta`: `casi_duplicados_vocabulario = FALSE`,
#' `ausencia_estructural = FALSE`, `variantes_equifrecuentes_vocabulario =
#' FALSE`, una `clave` retirada o declarada sobre otras columnas, una columna
#' retirada de `columnas_sin_ceros`, `columnas_no_negativas`,
#' `columnas_personales` o `aplicabilidad`, y `duplicados_aproximados` apagado o
#' corrido sobre otras columnas —lo que corrió queda en
#' `perfil$duplicados_aproximados`; si corrió sobre las mismas columnas con
#' otros parámetros, la fila `configuracion_duplicados_aproximados` lo
#' declara—; la descripción nombra el argumento. Al revés, un hallazgo que
#' aparece porque el perfil nuevo encendió o declaró lo que lo habilita sigue
#' saliendo `aparecido`, y su descripción dice que el anterior no lo evaluaba.
#' Un hallazgo de varias columnas —un par de columnas duplicado, una relación
#' entre columnas— se compara como cualquier otro: sólo no se presenta como
#' resuelto si alguna de sus columnas desapareció. Cada argumento de
#' [perfilar()] tiene su lugar en la comparación:
#' habilita un diagnóstico, es una vara —su cambio se declara en la fila
#' `configuracion_umbrales`, que desde esta versión incluye los umbrales de las
#' relaciones de orden y aritméticas entre columnas, los del vocabulario y
#' `datos_personales_permitidos`, `validadores_personales`,
#' `umbral_documento_verificado` y `muestra_validadores`—, tiene su propia fila
#' de configuración, acota el trabajo y lo declara en `cobertura_diagnosticos`,
#' o no cambia los hallazgos. Una vara que uno de los dos perfiles no registra
#' —un perfil de una versión anterior— no se compara.
#'
#' Los hallazgos que cuentan filas de la tabla y no pertenecen a una columna
#' concreta tampoco se declaran resueltos si aparecieron o desaparecieron
#' columnas entre las corridas: cambio el universo que define esas filas. En
#' ese caso se publica `no_comparable`, con severidad `error`, y se nombran las
#' columnas cambiadas. Con las mismas columnas, el veredicto `resuelto` se
#' conserva.
#'
#' @export
#' @seealso [perfilar()], [detectar_deriva_calidad()], [reportar()],
#'   [comparar_equivalencia()]
#'
#' @examples
#' anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
#'                      fecha = as.POSIXct("2026-01-01", tz = "UTC"))
#' actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
#'                    fecha = as.POSIXct("2026-02-01", tz = "UTC"))
#' comparar_perfiles(anterior, actual)
comparar_perfiles <- function(anterior, actual, umbral_cambio = 0.05,
                              umbral_error = 0.20) {
  anterior <- .validar_perfil_deriva(anterior, "anterior")
  actual <- .validar_perfil_deriva(actual, "actual")
  .exigir_orden_temporal(
    anterior$meta$fecha_hora, actual$meta$fecha_hora, "comparar_perfiles()",
    declaradas = isTRUE(anterior$meta$fecha_declarada) &&
      isTRUE(actual$meta$fecha_declarada)
  )
  if (!.son_numeros_escalares(list(umbral_cambio, umbral_error))) {
    .detener(
      "Los umbrales deben estar en [0, 1] y `umbral_error` no puede ser menor.",
      call. = FALSE
    )
  }
  umbrales <- c(umbral_cambio, umbral_error)
  if (!is.numeric(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      umbral_error < umbral_cambio) {
    .detener(
      "Los umbrales deben estar en [0, 1] y `umbral_error` no puede ser menor.",
      call. = FALSE
    )
  }
  fecha_anterior <- .fecha_utc(anterior$meta$fecha_hora)
  fecha_actual <- .fecha_utc(actual$meta$fecha_hora)
  mapa_a <- .mapa_columnas_perfil(anterior)
  mapa_b <- .mapa_columnas_perfil(actual)
  cambios <- list()
  k <- 0L
  agregar <- function(columna, aspecto, cambio, severidad, valor_anterior,
                      valor_actual, delta = NA_real_, cambio_relativo = NA_real_,
                      significativo = TRUE, descripcion, evidencia = "") {
    k <<- k + 1L
    original <- if (length(columna) && !is.na(columna)) {
      indice <- match(columna, c(mapa_a$clave, mapa_b$clave))
      if (!is.na(indice)) {
        fuente <- c(mapa_a$original, mapa_b$original)
        fuente[[indice]]
      } else columna
    } else columna
    cambios[[k]] <<- data.frame(
      columna = original, aspecto = aspecto, cambio = cambio,
      severidad = severidad, valor_anterior = .texto_deriva(valor_anterior),
      valor_actual = .texto_deriva(valor_actual), delta = delta,
      cambio_relativo = cambio_relativo, significativo = significativo,
      fecha_anterior = fecha_anterior, fecha_actual = fecha_actual,
      descripcion = descripcion, evidencia = evidencia,
      stringsAsFactors = FALSE
    )
  }

  campos <- .campos_comparables_deriva(anterior, actual)
  for (aspecto in names(campos$ausentes)) {
    lados <- campos$ausentes[[aspecto]]
    texto_lado <- function(x) {
      if (!length(x)) "los trae todos" else paste(x, collapse = ", ")
    }
    agregar(
      NA_character_, aspecto, "no_comparable", "error",
      texto_lado(lados$anterior), texto_lado(lados$actual),
      descripcion = paste0(
        "Uno de los dos perfiles no trae los campos que esta versi\u00f3n necesita ",
        "para comparar ", gsub("_", " ", aspecto, fixed = TRUE),
        "; esa parte no se compara."
      ),
      evidencia = paste0(
        "Campos ausentes en el perfil anterior: ", texto_lado(lados$anterior),
        "; en el actual: ", texto_lado(lados$actual),
        ". Suele pasar al comparar contra un perfil guardado por otra ",
        "versi\u00f3n de lupa."
      )
    )
  }

  desaparecidas <- setdiff(mapa_a$clave, mapa_b$clave)
  aparecidas <- setdiff(mapa_b$clave, mapa_a$clave)
  for (columna in desaparecidas) {
    agregar(
      columna, "columna", "desaparecida", "error", columna, NA_character_,
      descripcion = "La columna desapareci\u00f3 de la entrega actual.",
      evidencia = "No se comparan sus m\u00e9tricas, patrones ni rango."
    )
  }
  for (columna in aparecidas) {
    agregar(
      columna, "columna", "aparecida", "error", NA_character_, columna,
      descripcion = "Apareci\u00f3 una columna que no estaba en la entrega anterior.",
      evidencia = "No existe una base anterior para sus m\u00e9tricas."
    )
  }

  comunes <- intersect(mapa_a$clave, mapa_b$clave)
  # La politica se mira antes de recorrer las columnas: si cambio la politica
  # entera, su fila `configuracion_proteccion` ya explica cada columna y la fila
  # por columna no se repite.
  proteccion_a <- .proteccion_declarada_perfil(anterior)
  proteccion_b <- .proteccion_declarada_perfil(actual)
  politica_proteccion_cambiada <- !is.na(proteccion_a) && !is.na(proteccion_b) &&
    !identical(proteccion_a, proteccion_b)
  campos_protegidos <- .campos_protegidos_equivalencia_vacios()
  for (columna in comunes) {
    indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
    indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
    a <- anterior$columnas[indice_a, , drop = FALSE]
    b <- actual$columnas[indice_b, , drop = FALSE]
    # `unidad` va con los tipos y no con los conteos: es texto y se compara por
    # igualdad, no por delta. Y va con severidad `error` por el mismo motivo que
    # `tipo_declarado` -es el precedente del aspecto hermano-: un cambio de unidad
    # hace que la misma cifra signifique otra cosa, que es exactamente lo que
    # `set_units(c(1,2,3), "m")` y `"km"` demostraban al publicar filas identicas.
    tipos_comparables <- intersect(
      c("tipo_declarado", "tipo_inferido", "unidad"), campos$comparables
    )
    for (campo in tipos_comparables) {
      if (!.distinto_deriva(a[[campo]], b[[campo]])) next
      # La unidad tiene DOS casos y no uno, y tratarlos igual publicaba una
      # afirmacion falsa. Con unidad en los dos lados -`m` contra `km`-, el mismo
      # numero significa otra cosa y eso es `error`. Con `NA` en uno de los dos,
      # lo que cambio es si la columna DECLARA su unidad, y ahi la frase "el
      # mismo numero significa otra cosa" puede ser falsa: si la entrega anterior
      # no publicaba ninguna cifra -como una columna que se abstenia del resumen-,
      # no hay ningun numero que haya cambiado de significado.
      unidad_en_los_dos <- identical(campo, "unidad") &&
        !is.na(a[[campo]][[1L]]) && !is.na(b[[campo]][[1L]])
      severidad <- if (identical(campo, "unidad") && !unidad_en_los_dos) {
        "sospechoso"
      } else {
        "error"
      }
      descripcion <- if (identical(campo, "unidad")) {
        if (unidad_en_los_dos) {
          paste0(
            "Cambi\u00f3 la unidad en la que la columna publica sus cifras: el ",
            "mismo n\u00famero significa otra cosa."
          )
        } else if (is.na(a[[campo]][[1L]])) {
          paste0(
            "La columna pas\u00f3 a declarar su unidad; antes no declaraba ninguna. ",
            "No es un cambio de las cifras sino de lo que la entrega declara ",
            "sobre ellas."
          )
        } else {
          paste0(
            "La columna dej\u00f3 de declarar su unidad. No es un cambio de las ",
            "cifras sino de lo que la entrega declara sobre ellas."
          )
        }
      } else {
        paste0(
          "Cambi\u00f3 el ", gsub("_", " ", campo, fixed = TRUE),
          " de la columna."
        )
      }
      agregar(
        columna, campo, "modificado", severidad, a[[campo]], b[[campo]],
        descripcion = descripcion
      )
    }
    for (especificacion in list(
      c(campo = "prop_faltantes_totales", aspecto = "faltantes"),
      c(campo = "tasa_distintos", aspecto = "cardinalidad")
    )) {
      campo <- especificacion[["campo"]]
      aspecto <- especificacion[["aspecto"]]
      if (!aspecto %in% campos$comparables) next
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        delta <- b[[campo]] - a[[campo]]
        # Con tolerancia, como `detectar_deriva_calidad()` en `historico.R` y
        # `tablero-calidad.R` con los pesos. Sin ella la resta en coma flotante
        # decide el veredicto: 0,65 -> 0,70 da 0,049999999999999933 y
        # 0,70 -> 0,75 da 0,050000000000000044, asi que dos pares que publican
        # el mismo `delta = 0,05` recibian veredictos distintos. El comentario
        # de `historico.R` ya decia que compararlo con tolerancia en un archivo
        # y sin ella en otro no es una decision sino una inconsistencia, y este
        # archivo era el otro.
        significativo <- .alcanza_umbral_deriva(delta, umbral_cambio)
        # Que bajen los faltantes es una mejora, y por eso queda `ok`. La
        # cardinalidad no tiene una direccion buena: la regla del signo valia
        # igual para las dos, y un `id_tramite` unico que pasaba a tener la mitad
        # repetida -tasa de 1 a 0,5, diez umbrales- salia `ok` con
        # `significativo = TRUE`, mientras el arreglo inverso salia
        # `sospechoso`. Un cambio significativo de cardinalidad es sospechoso en
        # los dos sentidos, como el del rango, que tampoco tiene direccion.
        severidad <- if (!significativo) {
          "ok"
        } else if (aspecto == "cardinalidad") {
          "sospechoso"
        } else if (delta < 0) {
          "ok"
        } else if (.alcanza_umbral_deriva(delta, umbral_error)) {
          "error"
        } else {
          "sospechoso"
        }
        evidencia <- if (aspecto == "cardinalidad") {
          paste0(
            "Valores distintos: ", a$n_distintos, " -> ", b$n_distintos,
            "; tasas: ", .formatear_decimal_publicado(a[[campo]], 4L),
            " -> ", .formatear_decimal_publicado(b[[campo]], 4L)
          )
        } else {
          paste0(
            "Cambio de ", .formatear_decimal_publicado(delta, 4L, signo = TRUE),
            " en escala [0, 1]."
          )
        }
        descripcion <- if (aspecto == "faltantes") {
          "Cambi\u00f3 la proporci\u00f3n de faltantes de la columna."
        } else {
          "Cambi\u00f3 la tasa de valores distintos de la columna."
        }
        agregar(
          columna, aspecto, "modificado", severidad, a[[campo]], b[[campo]],
          delta = delta, significativo = significativo,
          descripcion = descripcion,
          evidencia = evidencia
        )
      }
    }
    # Dos resumenes calculados sobre subconjuntos distintos no son comparables
    # sin decirlo. Medido: la misma columna, una vez numerica y otra como texto
    # con un valor que no convierte, producia una fila "Cambio el rango
    # observado de la columna" -[10, 100] contra [10, 90]- atribuida a los
    # datos, cuando el 100 seguia ahi y lo que cambio fue que quedo fuera del
    # resumen.
    #
    # Es el mismo mecanismo que ya declara la comparabilidad de la politica de
    # centinelas y de la de aplicabilidad, una escala mas abajo: por columna, y
    # no por corrida.
    excluidos_a <- .valor_columna_deriva(a, "n_valores_excluidos_resumen")
    excluidos_b <- .valor_columna_deriva(b, "n_valores_excluidos_resumen")
    if (isTRUE(is.finite(excluidos_a)) && isTRUE(is.finite(excluidos_b)) &&
        !identical(excluidos_a, excluidos_b)) {
      agregar(
        columna, "alcance_resumen", "modificado", "sospechoso",
        .texto_numero_deriva(excluidos_a), .texto_numero_deriva(excluidos_b),
        descripcion = paste(
          "Los dos resumenes se calcularon sobre distinta cantidad de valores,",
          "asi que las diferencias de media, rango o cardinalidad pueden venir",
          "del alcance y no de los datos."
        ),
        evidencia = paste0(
          "Valores excluidos del resumen: ", .texto_numero_deriva(excluidos_a),
          " antes, ", .texto_numero_deriva(excluidos_b), " ahora."
        )
      )
    }
    if ("rango" %in% campos$comparables) {
      # Una columna protegida no publica su rango, y comparar un rango tapado
      # mentia de las dos maneras posibles. Protegida en los dos lados, los dos
      # rangos eran NULL y no salia nada: `telefono` pasaba de [20001066,
      # 20997537] a [90002001, 90997570] -setenta veces su ancho- y la
      # comparacion no decia ni eso ni que no lo habia mirado. Protegida en un
      # solo lado -la columna cruzo el umbral de verificacion de cedulas, con la
      # misma politica en las dos corridas-, el rango de un lado contra NULL daba
      # magnitud `Inf` y "Cambio el rango observado" con `significativo = TRUE`,
      # sobre dos entregas con los MISMOS extremos.
      #
      # Las dos respuestas ya estaban escritas en el paquete. `comparar_
      # equivalencia()` no compara un campo protegido: lo declara por columna,
      # campo y lado en `campos_protegidos`. Y un cambio de proteccion es un
      # cambio de lo que la entrega publica, no del dato, asi que lleva su propia
      # fila, como `configuracion_proteccion` una escala mas arriba.
      protegido_a <- .rango_protegido_deriva(a)
      protegido_b <- .rango_protegido_deriva(b)
      if (protegido_a || protegido_b) {
        lados <- c(if (protegido_a) "anterior", if (protegido_b) "actual")
        campos_protegidos <- rbind(campos_protegidos, data.frame(
          columna = rep(mapa_a$original[match(columna, mapa_a$clave)],
                        length(lados)),
          campo = "rango", lado = lados, stringsAsFactors = FALSE
        ))
        # La fila afirma "sin proteger" del otro lado, y eso hay que saberlo: un
        # perfil de otra version sin los campos de proteccion y con el rango en
        # NA puede haberlo tenido tapado. Ahi no se afirma nada; lo protegido
        # queda declarado igual.
        conocido <- if (protegido_a) b else a
        if (xor(protegido_a, protegido_b) && !politica_proteccion_cambiada &&
            .proteccion_conocida_deriva(conocido)) {
          agregar(
            columna, "proteccion", "modificado", "sospechoso",
            if (protegido_a) "protegido" else "sin proteger",
            if (protegido_b) "protegido" else "sin proteger",
            descripcion = paste(
              "Cambi\u00f3 la protecci\u00f3n de datos personales de la columna: un",
              "lado oculta lo que el otro publica, as\u00ed que su rango no se compara."
            ),
            evidencia = paste(
              "La pol\u00edtica de protecci\u00f3n es la misma en las dos corridas;",
              "lo que cambi\u00f3 es la clasificaci\u00f3n de la columna. El rango del",
              "lado protegido queda declarado en el atributo `campos_protegidos`."
            )
          )
        }
      } else {
        rango_a <- .rango_perfil(a)
        rango_b <- .rango_perfil(b)
        if ((!is.null(rango_a) || !is.null(rango_b)) &&
            (is.null(rango_a) || is.null(rango_b) ||
             !.rangos_iguales_deriva(rango_a, rango_b))) {
          magnitud <- .magnitud_rango(rango_a, rango_b)
          significativo <- .alcanza_umbral_deriva(magnitud, umbral_cambio)
          agregar(
            columna, "rango", "modificado",
            if (significativo) "sospechoso" else "ok",
            if (is.null(rango_a)) NA_character_ else rango_a$texto,
            if (is.null(rango_b)) NA_character_ else rango_b$texto,
            cambio_relativo = magnitud, significativo = significativo,
            descripcion = "Cambi\u00f3 el rango observado de la columna."
          )
        }
      }
    }
  }

  config_a <- .configuracion_patrones_perfil(anterior)
  config_b <- .configuracion_patrones_perfil(actual)
  patrones_comparables <- !is.null(config_a) && !is.null(config_b) &&
    isTRUE(all.equal(config_a, config_b, check.attributes = FALSE))
  if (!patrones_comparables) {
    agregar(
      NA_character_, "configuracion_patrones", "no_comparable", "error",
      paste(unlist(config_a), collapse = "; "),
      paste(unlist(config_b), collapse = "; "),
      descripcion = paste0(
        "Las corridas no declaran la misma configuraci\u00f3n de patrones; ",
        "sus patrones no se comparan."
      )
    )
  } else {
    for (columna in comunes) {
      indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
      indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
      pa <- .patrones_perfil(anterior, indice_a)
      pb <- .patrones_perfil(actual, indice_b)
      claves_a <- .clave_patron(pa$patron)
      claves_b <- .clave_patron(pb$patron)
      # Un patron que falta en un resumen LLENO no falta en los datos: pudo
      # quedar por debajo del corte. Con 22 formas y `max_patrones = 20`, mover
      # UN valor entre dos formas raras publicaba "Aparecio un patron de formato
      # nuevo" y "Desaparecio un patron de formato anterior", los dos
      # `sospechoso`, sobre patrones que seguian en las dos entregas. El techo
      # es la mayor proporcion que pudo tener alli; NA si el resumen esta
      # completo y la ausencia es cierta.
      techo_a <- .techo_fuera_del_resumen(anterior, indice_a, pa)
      techo_b <- .techo_fuera_del_resumen(actual, indice_b, pb)
      # La severidad de un patron que cambia se gradua igual que la de uno que
      # aparece o desaparece: desde `umbral_error`, `error`.
      grado <- function(cambio) {
        if (.alcanza_umbral_deriva(cambio, umbral_error)) "error" else "sospechoso"
      }
      fmt <- function(x) .formatear_decimal_publicado(x, 4L)
      # La clave APAREA; lo que se publica es el patron tal como lo publica el
      # perfil. Publicando la clave, la fila de deriva sacaba el patron con la
      # barra invertida duplicada -el escape que hace inyectiva a la clave- y
      # no coincidia con el mismo patron en `perfil$patrones`. Contar y mostrar
      # son dos preguntas distintas, tambien aca.
      publicable <- function(clave, claves, tabla) {
        posicion <- match(clave, claves)
        if (is.na(posicion)) return(clave)
        as.character(tabla$patron[[posicion]])
      }
      for (patron in setdiff(claves_b, claves_a)) {
        proporcion <- pb$proporcion[match(patron, claves_b)]
        patron <- publicable(patron, claves_b, pb)
        if (is.na(techo_a)) {
          agregar(
            columna, "patron", "aparecido",
            if (.alcanza_umbral_deriva(proporcion, umbral_error)) {
              "error"
            } else {
              "sospechoso"
            },
            NA_character_, patron, delta = proporcion, significativo = TRUE,
            descripcion = "Apareci\u00f3 un patr\u00f3n de formato nuevo.",
            evidencia = paste0(
              "Proporci\u00f3n actual: ",
              .formatear_decimal_publicado(proporcion, 4L)
            )
          )
          next
        }
        evidencia <- paste0(
          "Proporci\u00f3n actual: ", fmt(proporcion), "; en el perfil anterior, ",
          "cuyo resumen estaba lleno, a lo sumo ", fmt(techo_a), "."
        )
        cota <- proporcion - techo_a
        if (cota > 0 && .alcanza_umbral_deriva(cota, umbral_cambio)) {
          agregar(
            columna, "patron", "modificado", grado(cota), NA_character_, patron,
            delta = cota, significativo = TRUE,
            descripcion = paste(
              "Aument\u00f3 la proporci\u00f3n de un patr\u00f3n de formato, por lo menos",
              "en lo que dice `delta`: en el perfil anterior estaba por debajo del",
              "corte del resumen acotado."
            ),
            evidencia = evidencia
          )
        } else {
          agregar(
            columna, "patron", "no_comparable", "ok", NA_character_, patron,
            significativo = FALSE,
            descripcion = paste(
              "Un patr\u00f3n de formato entr\u00f3 al resumen acotado, pero no se sabe",
              "si es nuevo: el resumen anterior estaba lleno y pudo estar por",
              "debajo del corte."
            ),
            evidencia = evidencia
          )
        }
      }
      for (patron in setdiff(claves_a, claves_b)) {
        proporcion <- pa$proporcion[match(patron, claves_a)]
        patron <- publicable(patron, claves_a, pa)
        if (is.na(techo_b)) {
          agregar(
            columna, "patron", "desaparecido",
            if (.alcanza_umbral_deriva(proporcion, umbral_error)) {
              "error"
            } else {
              "sospechoso"
            },
            patron, NA_character_, delta = -proporcion, significativo = TRUE,
            descripcion = "Desapareci\u00f3 un patr\u00f3n de formato anterior.",
            evidencia = paste0(
              "Proporci\u00f3n anterior: ",
              .formatear_decimal_publicado(proporcion, 4L)
            )
          )
          next
        }
        evidencia <- paste0(
          "Proporci\u00f3n anterior: ", fmt(proporcion), "; en el perfil actual, ",
          "cuyo resumen estaba lleno, a lo sumo ", fmt(techo_b), "."
        )
        cota <- proporcion - techo_b
        if (cota > 0 && .alcanza_umbral_deriva(cota, umbral_cambio)) {
          agregar(
            columna, "patron", "modificado", grado(cota), patron, NA_character_,
            delta = -cota, significativo = TRUE,
            descripcion = paste(
              "Baj\u00f3 la proporci\u00f3n de un patr\u00f3n de formato, por lo menos en",
              "lo que dice `delta`: en el perfil actual qued\u00f3 por debajo del corte",
              "del resumen acotado."
            ),
            evidencia = evidencia
          )
        } else {
          agregar(
            columna, "patron", "no_comparable", "ok", patron, NA_character_,
            significativo = FALSE,
            descripcion = paste(
              "Un patr\u00f3n de formato sali\u00f3 del resumen acotado, pero no se sabe",
              "si desapareci\u00f3: el resumen actual estaba lleno y puede seguir por",
              "debajo del corte."
            ),
            evidencia = evidencia
          )
        }
      }
      # Un patron que sigue en los dos resumenes tiene su proporcion medida en
      # los dos, y no se miraba: `Montevideo` x95 y `MVD-01` x5 contra la
      # inversa -el formato dominante paso del nombre al codigo- daba cero filas.
      # Se informa cuando el cambio alcanza `umbral_cambio`; los menores no dan
      # fila, a diferencia de los faltantes, porque serian una fila por patron
      # en cada entrega. Es una decision, y esta escrita en la ayuda.
      for (patron in intersect(claves_a, claves_b)) {
        proporcion_a <- pa$proporcion[match(patron, claves_a)]
        proporcion_b <- pb$proporcion[match(patron, claves_b)]
        delta <- proporcion_b - proporcion_a
        if (!.alcanza_umbral_deriva(delta, umbral_cambio)) next
        patron <- publicable(patron, claves_b, pb)
        agregar(
          columna, "patron", "modificado", grado(delta), patron, patron,
          delta = delta, significativo = TRUE,
          descripcion = paste(
            "Cambi\u00f3 la proporci\u00f3n de un patr\u00f3n de formato que sigue",
            "presente."
          ),
          evidencia = paste0(
            "Proporci\u00f3n: ", fmt(proporcion_a), " -> ", fmt(proporcion_b), "."
          )
        )
      }
    }
  }

  sentinelas_a <- .configuracion_sentinelas_perfil(anterior)
  sentinelas_b <- .configuracion_sentinelas_perfil(actual)
  # Una lista enteramente ausente no es una politica distinta: es una politica
  # OCULTA. La proteccion de datos personales reemplaza `meta$sentinelas_
  # numericos` por NA cuando no puede decidir que centinela pertenece a que
  # columna, y comparar un perfil protegido contra uno sin proteger daba
  # "Cambio la politica de centinelas numericos" con severidad `error` sobre
  # dos corridas que usaban exactamente la misma politica por omision.
  # Basta UN centinela enmascarado: la proteccion tapa solo los que coinciden con
  # un valor protegido, y una politica tapada en parte se comparaba como
  # "modificada" con severidad `error`. Es la regla de las cadenas de ausencia.
  oculta <- function(x) !is.null(x) && length(x) && anyNA(x)
  sentinelas_comparables <- !is.null(sentinelas_a) &&
    !is.null(sentinelas_b) &&
    isTRUE(all.equal(sentinelas_a, sentinelas_b, check.attributes = FALSE))
  # Dos listas enmascaradas del mismo modo comparan iguales y no hay nada que
  # declarar: la guarda es para cuando DIFIEREN y esa diferencia se explica por
  # el enmascarado, no por la politica.
  sentinelas_ocultos <- !sentinelas_comparables &&
    (oculta(sentinelas_a) || oculta(sentinelas_b))
  texto_configuracion <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (oculta(x)) return("[pol\u00edtica protegida]")
    paste(unlist(x), collapse = "; ")
  }
  if (sentinelas_ocultos) {
    agregar(
      NA_character_, "configuracion_sentinelas_numericos", "no_comparable",
      "sospechoso", texto_configuracion(sentinelas_a),
      texto_configuracion(sentinelas_b),
      descripcion = paste(
        "Una de las dos corridas public\u00f3 su pol\u00edtica de centinelas",
        "enmascarada por la protecci\u00f3n de datos personales: no se puede",
        "saber si la pol\u00edtica cambi\u00f3."
      ),
      evidencia = paste(
        "No se afirma que haya cambiado. Para comparar la pol\u00edtica,",
        "correr las dos con la misma configuraci\u00f3n de protecci\u00f3n."
      )
    )
  } else if (!sentinelas_comparables) {
    agregar(
      NA_character_, "configuracion_sentinelas_numericos", "modificado",
      "error", texto_configuracion(sentinelas_a),
      texto_configuracion(sentinelas_b),
      descripcion = paste(
        "Cambi\u00f3 la pol\u00edtica de centinelas num\u00e9ricos; se mantienen las",
        "comparaciones para que las corridas siguientes sigan detectando",
        "deriva con la pol\u00edtica vigente."
      ),
      evidencia = paste(
        "Las diferencias de m\u00e9tricas o hallazgos pueden atribuirse a esta",
        "pol\u00edtica en la transici\u00f3n; revisar la configuraci\u00f3n publicada."
      )
    )
  }

  # `cadenas_ausencia` es la gemela textual de los centinelas numericos: las dos
  # redefinen que cuenta como ausente. No se comparaba, y con los MISMOS datos y
  # la lista declarada en una sola corrida la deriva publicaba los faltantes
  # bajando de 0,305 a 0 y el hallazgo "resuelto" con severidad `ok` -una mejora
  # que no ocurrio-, o en la otra direccion un deterioro con severidad `error`.
  # Se declara con su propia fila, igual que la politica gemela.
  cadenas <- function(x) {
    x <- x$meta$cadenas_ausencia
    if (is.null(x) || !length(x)) return(character())
    .sort_por_bytes(unique(as.character(x)))
  }
  cadenas_a <- cadenas(anterior)
  cadenas_b <- cadenas(actual)
  if (!identical(cadenas_a, cadenas_b)) {
    ocultas <- anyNA(cadenas_a) || anyNA(cadenas_b)
    agregar(
      NA_character_, "configuracion_cadenas_ausencia",
      if (ocultas) "no_comparable" else "modificado",
      if (ocultas) "sospechoso" else "error",
      if (length(cadenas_a)) paste(cadenas_a, collapse = "; ") else "ninguna",
      if (length(cadenas_b)) paste(cadenas_b, collapse = "; ") else "ninguna",
      descripcion = paste(
        "Cambi\u00f3 la lista de cadenas que se declaran como ausencia; se",
        "mantienen las comparaciones, pero las diferencias de faltantes y de",
        "hallazgos de ausencia pueden venir de esta pol\u00edtica y no de los datos."
      ),
      evidencia = paste(
        "Para comparar los datos, correr las dos con la misma",
        "`cadenas_ausencia`."
      )
    )
  }

  # Y el tamano de la tabla. La cardinalidad se compara por TASA, asi que una
  # tabla que duplica sus filas con valores nuevos deja la tasa igual y la deriva
  # no publicaba nada del crecimiento, aunque el perfil lo trae y la propia
  # evidencia de otras filas lo imprimia. Perder filas es sospechoso; ganarlas se
  # declara sin alarma.
  filas_a <- anterior$meta$filas_totales
  filas_b <- actual$meta$filas_totales
  if (is.numeric(filas_a) && is.numeric(filas_b) && length(filas_a) == 1L &&
      length(filas_b) == 1L && !is.na(filas_a) && !is.na(filas_b) &&
      filas_a != filas_b) {
    agregar(
      NA_character_, "filas", "modificado",
      if (filas_b < filas_a) "sospechoso" else "ok",
      filas_a, filas_b, delta = filas_b - filas_a,
      cambio_relativo = if (filas_a > 0) (filas_b - filas_a) / filas_a else NA_real_,
      significativo = TRUE,
      descripcion = if (filas_b < filas_a) {
        "La tabla tiene menos filas que en la corrida anterior."
      } else {
        "La tabla tiene m\u00e1s filas que en la corrida anterior."
      },
      evidencia = paste0("Filas: ", filas_a, " -> ", filas_b, ".")
    )
  }

  # Comparar un perfil protegido contra uno sin proteger publicaba los valores
  # que el lado protegido oculta, y la diferencia se leia como deriva del dato
  # cuando lo que cambio es la politica. No es una fuga -quien corre esa
  # comparacion ya tiene el perfil sin proteger- pero es la misma forma que ya
  # tienen `configuracion_umbrales` y `configuracion_normalizacion`: un cambio de
  # politica se declara con su propia fila en vez de disfrazarse de deriva.
  if (politica_proteccion_cambiada) {
    agregar(
      NA_character_, "configuracion_proteccion", "modificado", "error",
      if (proteccion_a) "protegido" else "sin proteger",
      if (proteccion_b) "protegido" else "sin proteger",
      descripcion = paste(
        "Un lado oculta los valores de las columnas personales y el otro no."
      ),
      evidencia = paste(
        "Los valores que aparecen de un solo lado pueden venir de esta",
        "diferencia de pol\u00edtica y no de los datos."
      )
    )
  }

  # `duplicados_aproximados` corrio en las dos corridas con otra configuracion:
  # sus pares no se comparan sobre lo mismo. Si corrio en una sola, sus
  # hallazgos lo dicen uno por uno (`no_evaluado`, o `aparecido` con la causa).
  duplicados_a <- .duplicados_evaluados_perfil(anterior)
  duplicados_b <- .duplicados_evaluados_perfil(actual)
  if (!is.null(duplicados_a) && !is.null(duplicados_b)) {
    configuracion_a <- c(columnas = duplicados_a$texto_columnas, duplicados_a$parametros)
    configuracion_b <- c(columnas = duplicados_b$texto_columnas, duplicados_b$parametros)
    parametros <- names(duplicados_a$parametros)
    difieren <- c(
      if (!identical(duplicados_a$columnas, duplicados_b$columnas)) "columnas",
      parametros[duplicados_a$parametros[parametros] !=
                   duplicados_b$parametros[parametros]]
    )
    if (length(difieren)) {
      texto_duplicados <- function(x) {
        paste(paste0(difieren, "=", x[difieren]), collapse = "; ")
      }
      agregar(
        NA_character_, "configuracion_duplicados_aproximados", "modificado",
        "error", texto_duplicados(configuracion_a),
        texto_duplicados(configuracion_b),
        descripcion = paste(
          "Cambi\u00f3 la configuraci\u00f3n de `duplicados_aproximados`: las",
          "columnas que compara o sus par\u00e1metros."
        ),
        evidencia = paste(
          "Un par de filas que aparece o desaparece entre las dos corridas",
          "puede venir de esta configuraci\u00f3n y no de los datos."
        )
      )
    }
  }

  umbrales_a <- .configuracion_umbrales_perfil(anterior)
  umbrales_b <- .configuracion_umbrales_perfil(actual)
  # Solo las varas que los dos perfiles registran: un perfil de otra version
  # que no registraba una no la declaraba, y ahi no se sabe -como con
  # `aplicabilidad`-. Compararla contra su ausencia publicaba un cambio de vara
  # que no hubo.
  if (!is.null(umbrales_a) && !is.null(umbrales_b)) {
    comunes <- intersect(names(umbrales_a), names(umbrales_b))
    umbrales_a <- umbrales_a[comunes]
    umbrales_b <- umbrales_b[comunes]
  }
  if (!is.null(umbrales_a) && !is.null(umbrales_b) &&
      !identical(umbrales_a, umbrales_b)) {
    difieren <- names(umbrales_a)[
      !names(umbrales_a) %in% names(umbrales_b) |
        umbrales_a[names(umbrales_a)] !=
          umbrales_b[match(names(umbrales_a), names(umbrales_b))]
    ]
    difieren <- difieren[!is.na(difieren)]
    texto_umbral <- function(x, cuales) {
      paste(paste0(cuales, "=", x[cuales]), collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_umbrales", "modificado", "error",
      texto_umbral(umbrales_a, difieren), texto_umbral(umbrales_b, difieren),
      descripcion = paste(
        "Cambiaron los umbrales que deciden si un hallazgo se emite y con",
        "qu\u00e9 severidad."
      ),
      evidencia = paste(
        "Un hallazgo que aparece, desaparece o cambia de severidad entre las",
        "dos corridas puede venir de esta configuraci\u00f3n y no de los datos."
      )
    )
  }

  normalizacion_a <- .configuracion_normalizacion_perfil(anterior)
  normalizacion_b <- .configuracion_normalizacion_perfil(actual)
  if (!is.null(normalizacion_a) && !is.null(normalizacion_b) &&
      !isTRUE(all.equal(normalizacion_a, normalizacion_b,
                        check.attributes = FALSE))) {
    texto_normalizacion <- function(x) {
      activos <- names(x)[vapply(x, function(z) isTRUE(z[[1L]]), logical(1))]
      if (!length(activos)) "ninguno" else paste(activos, collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_normalizacion", "modificado", "error",
      texto_normalizacion(normalizacion_a),
      texto_normalizacion(normalizacion_b),
      descripcion = paste(
        "Cambi\u00f3 la normalizaci\u00f3n del texto, que decide qu\u00e9 valores se",
        "consideran el mismo."
      ),
      evidencia = paste(
        "Las diferencias de cardinalidad, variantes y duplicados pueden venir",
        "de esta pol\u00edtica y no de los datos; un hallazgo que desaparece al",
        "apagarla no es una mejora de la entrega."
      )
    )
  }

  # La misma declaracion de comparabilidad que ya existia para los centinelas,
  # para la tercera politica que redefine lo que se mide.
  aplicabilidad_a <- .configuracion_aplicabilidad_perfil(anterior)
  aplicabilidad_b <- .configuracion_aplicabilidad_perfil(actual)
  if (!is.null(aplicabilidad_a) && !is.null(aplicabilidad_b) &&
      !identical(aplicabilidad_a, aplicabilidad_b)) {
    texto_aplicabilidad <- function(x) {
      if (!length(x)) "ninguna" else paste(x, collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_aplicabilidad", "modificado",
      "error", texto_aplicabilidad(aplicabilidad_a),
      texto_aplicabilidad(aplicabilidad_b),
      descripcion = paste(
        "Cambiaron las columnas con regla de aplicabilidad; esa regla redefine",
        "el universo aplicable, asi que las diferencias de faltantes,",
        "cardinalidad y rango pueden ser del metodo y no de los datos."
      ),
      evidencia = paste(
        "Se mantienen las comparaciones para que las corridas siguientes sigan",
        "detectando deriva con la politica vigente."
      )
    )
  }

  # La misma fila para la otra declaracion que redefine un universo.
  opcionales_a <- .configuracion_opcionales_perfil(anterior)
  opcionales_b <- .configuracion_opcionales_perfil(actual)
  if (!is.null(opcionales_a) && !is.null(opcionales_b) &&
      !identical(opcionales_a, opcionales_b)) {
    texto_opcionales <- function(x) {
      if (!length(x)) "ninguna" else paste(x, collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_columnas_opcionales", "modificado",
      "error", texto_opcionales(opcionales_a),
      texto_opcionales(opcionales_b),
      descripcion = paste(
        "Cambiaron las columnas declaradas en `columnas_opcionales`; su",
        "universo de completitud son las celdas presentes, asi que las",
        "diferencias de faltantes pueden ser del metodo y no de los datos."
      ),
      evidencia = paste(
        "Se mantienen las comparaciones para que las corridas siguientes sigan",
        "detectando deriva con la politica vigente."
      )
    )
  }

  hallazgos_a <- .resumir_hallazgos_deriva(anterior)
  hallazgos_b <- .resumir_hallazgos_deriva(actual)
  nuevos <- setdiff(hallazgos_b$clave, hallazgos_a$clave)
  resueltos <- setdiff(hallazgos_a$clave, hallazgos_b$clave)
  nombres_actuales <- .nombres_para_operar(unique(mapa_b$original))
  # Al reves de la declinacion: un hallazgo que aparece porque el perfil nuevo
  # encendio el diagnostico o declaro lo que lo habilita no era un hallazgo que
  # faltara en los datos de antes. Sigue siendo `aparecido` -esta en el perfil
  # nuevo y no en el anterior-, pero la descripcion dice que el anterior no lo
  # evaluaba, como la de `no_evaluado`.
  habilitados_ahora <- .declinados_por_declaracion_retirada(actual, anterior)
  for (clave in nuevos) {
    x <- hallazgos_b[match(clave, hallazgos_b$clave), , drop = FALSE]
    clave_habilitada <- .clave_declinacion_deriva(x$columna, x$tipo_hallazgo)
    argumento_nuevo <- if (clave_habilitada %in% habilitados_ahora) {
      names(habilitados_ahora)[match(clave_habilitada, habilitados_ahora)]
    } else NA_character_
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo", "aparecido", x$severidad, NA_character_, x$tipo_hallazgo,
      descripcion = if (is.na(argumento_nuevo)) {
        "Apareci\u00f3 un hallazgo que no estaba en el perfil anterior."
      } else {
        paste0(
          "Apareci\u00f3 un hallazgo que el perfil anterior no evaluaba: no ",
          "declaraba lo que lo habilita (`", argumento_nuevo, "`), as\u00ed que ",
          "no es necesariamente un cambio de los datos."
        )
      },
      evidencia = x$evidencia
    )
  }
  # Un hallazgo que ya no esta puede haberse resuelto o puede no haberse
  # evaluado, y son cosas distintas: la primera es una mejora y la segunda es
  # una medicion que falta. Se distinguen mirando la cobertura del perfil nuevo,
  # que es donde el paquete declara lo que declino. Sin esta consulta se
  # informaba "resuelto" con severidad `ok` sobre un diagnostico que decia, en
  # esa misma tabla, "no se evaluaron los limites de Tukey".
  declinados_ahora <- c(
    .diagnosticos_declinados_deriva(actual),
    .declinados_por_declaracion_retirada(anterior, actual),
    .declinados_por_cambio_de_tipo(anterior, actual)
  )
  nombres_columnas_cambiadas <- c(
    if (length(aparecidas)) {
      paste0("aparecieron: ", paste(
        mapa_b$original[match(aparecidas, mapa_b$clave)], collapse = ", "
      ))
    },
    if (length(desaparecidas)) {
      paste0("desaparecieron: ", paste(
        mapa_a$original[match(desaparecidas, mapa_a$clave)], collapse = ", "
      ))
    }
  )
  motivo_columnas_cambiadas <- if (length(nombres_columnas_cambiadas)) {
    paste(nombres_columnas_cambiadas, collapse = "; ")
  } else ""
  columnas_retiradas <- mapa_a$original[match(desaparecidas, mapa_a$clave)]
  for (clave in resueltos) {
    x <- hallazgos_a[match(clave, hallazgos_a$clave), , drop = FALSE]
    if (x$columna != "<tabla>" &&
        !.nombres_para_operar(x$columna) %in% nombres_actuales &&
        .hallazgo_nombra_columna_retirada(x$columna, columnas_retiradas)) next
    if (.hallazgo_cuenta_filas_tabla(x) &&
        nzchar(motivo_columnas_cambiadas)) {
      agregar(
        NA_character_, "hallazgo", "no_comparable", "error",
        x$tipo_hallazgo, NA_character_,
        descripcion = paste0(
          "El hallazgo cuenta filas de la tabla, pero no se puede decidir si",
          " se resolvio porque cambio el universo de columnas: ",
          motivo_columnas_cambiadas, "."
        ),
        evidencia = paste0(
          x$evidencia, " No se compara como resuelto: ",
          motivo_columnas_cambiadas, "."
        )
      )
      next
    }
    clave_declinacion <- .clave_declinacion_deriva(
      x$columna, x$tipo_hallazgo
    )
    # Un hallazgo de varias columnas tampoco se volvio a evaluar si la
    # cobertura del perfil nuevo declina su diagnostico sobre alguna de ellas:
    # las relaciones entre columnas declaran las que omitieron como una lista
    # (`a,b,c`), y la guarda las parte en `.diagnosticos_declinados_deriva()`.
    componentes <- if (!identical(x$columna, "<tabla>") &&
                       grepl(",", x$columna, fixed = TRUE, useBytes = TRUE)) {
      trimws(strsplit(x$columna, ",", fixed = TRUE)[[1L]])
    } else character()
    declinado <- clave_declinacion %in% declinados_ahora ||
      any(.clave_declinacion_deriva(
        componentes, rep(x$tipo_hallazgo, length(componentes))
      ) %in% declinados_ahora)
    # Los dos motivos de no evaluacion mandan a lugares distintos, y decir el
    # equivocado es peor que no decir ninguno: quien busque en
    # `cobertura_diagnosticos` un diagnostico que nunca se pidio no va a
    # encontrar nada y va a concluir que el aviso esta de mas.
    retiradas <- .declinados_por_declaracion_retirada(anterior, actual)
    por_declaracion <- clave_declinacion %in% retiradas
    argumento_retirado <- if (por_declaracion) {
      names(retiradas)[match(clave_declinacion, retiradas)]
    } else NA_character_
    por_tipo <- clave_declinacion %in%
      .declinados_por_cambio_de_tipo(anterior, actual)
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo",
      if (declinado) "no_evaluado" else "resuelto",
      if (declinado) "sospechoso" else "ok",
      x$tipo_hallazgo, NA_character_,
      descripcion = if (por_tipo) {
        paste(
          "La columna dej\u00f3 de ser texto y este diagn\u00f3stico compara valores",
          "de texto entre s\u00ed, as\u00ed que no se volvi\u00f3 a evaluar: los valores",
          "que lo dispararon pueden seguir ah\u00ed. Dejar de mirar no es lo mismo",
          "que arreglar."
        )
      } else if (por_declaracion) {
        paste0(
          "El diagn\u00f3stico no se evalu\u00f3 en el perfil nuevo porque se apag\u00f3, ",
          "se retir\u00f3 o se declar\u00f3 sobre otras columnas lo que lo habilita (`",
          argumento_retirado, "`), as\u00ed que no se sabe si el hallazgo sigue: ",
          "dejar de mirar no es lo mismo que arreglar."
        )
      } else if (declinado) {
        paste(
          "El diagn\u00f3stico no se evalu\u00f3 en el perfil nuevo, as\u00ed que no se sabe",
          "si el hallazgo sigue: el motivo est\u00e1 en `cobertura_diagnosticos`."
        )
      } else {
        "Un hallazgo del perfil anterior ya no est\u00e1 presente."
      },
      evidencia = x$evidencia
    )
  }
  compartidos <- intersect(hallazgos_a$clave, hallazgos_b$clave)
  for (clave in compartidos) {
    indice_a <- match(clave, hallazgos_a$clave)
    indice_b <- match(clave, hallazgos_b$clave)
    sa <- hallazgos_a$severidad[[indice_a]]
    sb <- hallazgos_b$severidad[[indice_b]]
    if (sa != sb) {
      columna <- hallazgos_b$columna[[indice_b]]
      agregar(
        if (columna == "<tabla>") NA_character_ else columna,
        "severidad_hallazgo",
        if (match(sb, c("ok", "sospechoso", "error")) >
            match(sa, c("ok", "sospechoso", "error"))) "agravado" else "atenuado",
        sb, sa, sb,
        descripcion = "Cambi\u00f3 la severidad de un hallazgo persistente.",
        evidencia = hallazgos_b$tipo_hallazgo[[indice_b]]
      )
    }
  }

  rownames(campos_protegidos) <- NULL
  if (!length(cambios)) {
    resultado <- .deriva_perfil_vacia()
    attr(resultado, "campos_protegidos") <- campos_protegidos
    return(resultado)
  }
  resultado <- do.call(rbind, cambios)
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  # Una comparacion de perfiles armados SIN proteccion publica sus valores -los
  # rangos de una cedula-; queda escrito para que `reportar()` no la publique en
  # claro dentro de un informe que protege.
  sin_proteger <- unique(c(
    if (is.list(anterior$meta) && isFALSE(anterior$meta$proteger_datos_personales)) {
      .columnas_personales_protegidas(anterior)
    },
    if (is.list(actual$meta) && isFALSE(actual$meta$proteger_datos_personales)) {
      .columnas_personales_protegidas(actual)
    }
  ))
  if (length(sin_proteger)) {
    attr(resultado, "datos_personales_sin_proteger") <- sin_proteger
  }
  attr(resultado, "campos_protegidos") <- campos_protegidos
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

# Registro fijo de los campos que el comparador puede interpretar. La clase
# almacenada de una columna no decide el eje: memoria y DBI publican algunos
# conteos con clases distintas, y el mismo campo debe conservar la misma regla.
# Si aparece una métrica nueva, se agrega explícitamente acá antes de compararla.
.registro_campos_equivalencia <- function() {
  list(
    flotante = c("media", "mediana", "desvio", "longitud_media"),
    exacto = c(
      "proporcion_tipo_inferido", "n_filas_analizadas_tipo", "n",
      "n_validos",
      "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
      "n_presentes_fuera_de_aplicabilidad", "n_faltantes",
      "prop_faltantes", "n_faltantes_disfrazados",
      "n_faltantes_disfrazados_textuales", "n_faltantes_disfrazados_numericos",
      "prop_faltantes_disfrazados", "n_faltantes_totales",
      "prop_faltantes_totales", "n_distintos", "tasa_distintos",
      "secuencia_entera_densa", "densidad_secuencia_entera",
      "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
      "hueco_maximo_secuencia_entera", "salto_de_escala_secuencia_entera",
      "moda_sobresale_secuencia_entera",
      "umbral_densidad_secuencia_entera", "min_distintos_secuencia_entera",
      "frecuencia_moda", "longitud_minima", "longitud_maxima",
      "minimo", "maximo", "minimo_exacto", "maximo_exacto",
      "n_fechas_resumidas", "n_fechas_excluidas_granularidad",
      "n_valores_excluidos_resumen", "n_ceros",
      "n_negativos", "n_outliers", "centinela_repeticiones",
      "densidad_sin_centinela", "n_nan", "n_infinito_positivo",
      "n_infinito_negativo", "n_filas_fecha_civil_distinta_utc", "n_blancos",
      "n_espacios_borde", "n_variantes_mayusculas", "n_variantes_unicode",
      "n_codificacion_rota", "n_codificacion_reparable",
      "n_codificacion_reparable_parcialmente", "n_codificacion_irreparable",
      "n_codificacion_no_se_pudo", "n_codificacion_invalida",
      "n_controles_invisibles", "n_invisibles_eliminables",
      "n_espacios_invisibles", "n_invisibles_significativos",
      "n_entidades_html", "n_separadores_en_campo", "n_numeros_texto",
      "proporcion_numeros_texto"
    ),
    fecha = c(
      "minimo_fecha", "maximo_fecha", "media_fecha", "mediana_fecha"
    ),
    valor = c("moda", "centinela_valor")
  )
}

.campos_magnitud_equivalencia <- function(registro) {
  # Estos son los campos cuyo numero representa una magnitud de la columna.
  # Los conteos y proporciones quedan fuera: que la columna haya pasado de
  # fecha a numero no cambia la unidad de contar filas o valores distintos.
  unique(intersect(
    c(registro$flotante, "minimo", "maximo", "centinela_valor"),
    unlist(registro, use.names = FALSE)
  ))
}

.tipo_declarado_equivalencia <- function(columnas, indice) {
  # Los guardas de tipo consultan `.tipo_columna_equivalencia()`, que prefiere
  # el tipo *inferido*. Si el texto contiene numeros o fechas, esa inferencia
  # borra justo la diferencia de almacenamiento que el guarda existe para ver.
  # Aca se lee el tipo declarado, el unico que dice como estan guardados los
  # valores. Devuelve NA cuando el vocabulario no es el de memoria -- un tipo
  # SQL, una clase `sfc` -- y entonces ningun guarda de almacenamiento afirma
  # nada. El vocabulario es el que produce `.tipo_declarado()`, y vive aca una
  # sola vez para que los dos clasificadores de abajo no puedan divergir.
  if (!"tipo_declarado" %in% names(columnas)) return(NA_character_)
  valor <- as.character(columnas[["tipo_declarado"]][[indice]])
  if (length(valor) != 1L || is.na(valor) || !nzchar(trimws(valor))) {
    return(NA_character_)
  }
  tipo <- .minusculas_ascii(trimws(valor))
  conocidos <- c(
    "texto", "factor", "factor-ordenado", "doble", "entero", "logico",
    "fecha", "fecha-hora", "integer64", "lista", "matriz"
  )
  if (!tipo %in% conocidos) return(NA_character_)
  tipo
}

.nombre_tipo_motivo <- function(declarado, inferido) {
  # El motivo describe un cambio de TIPO, y quien lo lee entiende el tipo
  # declarado. `.tipo_columna_equivalencia()` devuelve el INFERIDO, asi que un
  # texto de "1","2","3" se nombraba `entero`: la pareja texto contra doble se
  # publicaba como `tipo_cambiado:entero_vs_doble`, donde ningun lado es entero.
  # Se prefiere el declarado y se cae al inferido solo cuando no se conoce -por
  # ejemplo con un tipo SQL-, que es mejor que no decir nada.
  if (!is.na(declarado)) return(declarado)
  if (!is.na(inferido)) return(inferido)
  "desconocido"
}

.almacenamiento_caracter_equivalencia <- function(columnas, indice) {
  tipo <- .tipo_declarado_equivalencia(columnas, indice)
  if (is.na(tipo)) return(NA)
  tipo %in% c("texto", "factor", "factor-ordenado")
}

.almacenamiento_con_zona_equivalencia <- function(columnas, indice) {
  # Solo `fecha-hora` lleva zona horaria. `fecha` no la tiene, y una fecha
  # escrita como texto tampoco: por eso el desfasaje se pierde al guardarla.
  tipo <- .tipo_declarado_equivalencia(columnas, indice)
  if (is.na(tipo)) return(NA)
  identical(tipo, "fecha-hora")
}

.campos_representacion_equivalencia <- function(registro) {
  # Estos son los campos cuyo numero se calcula sobre la *representacion* de
  # los valores y no sobre los valores. Cambian con solo cambiar el
  # almacenamiento, aunque el dato sea identico: medidos sobre pares del mismo
  # dato guardado de las dos formas -- doble, entero, fecha, logico, negativos,
  # notacion cientifica, factor y fecha-hora -- son los unicos que difieren
  # siempre. Los conteos de codificacion y de invisibles quedan fuera a
  # proposito: ahi una diferencia habla del dato, no de como se guarda.
  unique(intersect(
    c("longitud_minima", "longitud_maxima", "longitud_media",
      "n_variantes_unicode", "n_numeros_texto", "proporcion_numeros_texto"),
    unlist(registro, use.names = FALSE)
  ))
}

.campos_zona_horaria_equivalencia <- function(registro) {
  # Campos que solo puede medir un almacenamiento que lleva zona horaria. Del
  # otro lado no dan un valor distinto: no dan ninguno, porque no hay zona que
  # comparar contra UTC. Los extremos de fecha quedan FUERA a proposito: ahi la
  # diferencia es real -- al guardarse como texto la columna pierde la zona y
  # los instantes que denota son otros -- y es el hallazgo mas importante de
  # ese escenario.
  unique(intersect(
    c("n_filas_fecha_civil_distinta_utc", "fecha_civil_distinta_utc"),
    unlist(registro, use.names = FALSE)
  ))
}

.tipo_columna_equivalencia <- function(columnas, indice) {
  nombres <- c(
    "tipo_inferido", "clase_temporal", "tipo_temporal", "clase",
    "tipo_declarado"
  )
  nombres <- intersect(nombres, names(columnas))
  for (nombre in nombres) {
    valor <- as.character(columnas[[nombre]][[indice]])
    if (length(valor) == 1L && !is.na(valor) && nzchar(trimws(valor))) {
      return(.minusculas_ascii(trimws(valor)))
    }
  }
  NA_character_
}

.es_temporal_equivalencia <- function(columnas, indice) {
  tipo <- .tipo_columna_equivalencia(columnas, indice)
  # Sin tipo no se sabe, y no saber no es "no temporal". El frame de
  # `perfilar_dbi()` no publica ningun campo de tipo, y devolver FALSE ahi hacia
  # que el `xor` afirmara `tipo_cambiado:temporal_vs_no_temporal` sobre columnas
  # que el motor devolvia como `Date` y `POSIXct`. Es la regla de la guarda
  # hermana de texto, `.almacenamiento_caracter_equivalencia()`: ante lo
  # desconocido, NA.
  if (is.na(tipo)) return(NA)
  tipo %in% c(
    "fecha", "fecha-hora", "fecha_hora", "date", "datetime", "timestamp",
    "posixct", "posixlt", "temporal"
  ) || grepl("^(fecha|date|datetime|timestamp|posix)", tipo)
}

.campos_protegidos_equivalencia_vacios <- function() {
  data.frame(
    columna = character(), campo = character(), lado = character(),
    stringsAsFactors = FALSE
  )
}

.detalle_proteccion_campo_equivalencia <- function(detalle, campo) {
  if (length(detalle) != 1L || is.na(detalle) || !nzchar(detalle)) {
    return(FALSE)
  }
  if (identical(campo, "moda") && grepl("moda", detalle, fixed = TRUE, useBytes = TRUE)) {
    return(TRUE)
  }
  if (campo %in% c("media", "media_fecha") &&
      grepl("momentos", detalle, fixed = TRUE, useBytes = TRUE)) {
    return(TRUE)
  }
  campos_orden <- c(
    "minimo", "maximo", "mediana", "minimo_exacto", "maximo_exacto",
    "minimo_fecha", "maximo_fecha", "mediana_fecha", "centinela_valor",
    "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
    "hueco_maximo_secuencia_entera", "densidad_secuencia_entera",
    "densidad_sin_centinela"
  )
  campo %in% campos_orden && grepl("orden", detalle, fixed = TRUE, useBytes = TRUE)
}

.campo_protegido_equivalencia <- function(columnas, campo, indice) {
  protegido <- if ("dato_personal_protegido" %in% names(columnas)) {
    isTRUE(columnas$dato_personal_protegido[[indice]])
  } else {
    FALSE
  }
  valor <- .valor_equivalencia(columnas, campo, indice)
  if (protegido || (is.character(valor) && length(valor) == 1L &&
                    identical(valor, "[valor protegido]"))) {
    return(TRUE)
  }
  if (!.faltante_equivalencia(valor) ||
      !"detalle_proteccion_personal" %in% names(columnas)) {
    return(FALSE)
  }
  .detalle_proteccion_campo_equivalencia(
    columnas$detalle_proteccion_personal[[indice]], campo
  )
}

.columnas_equivalencia <- function(x, nombre) {
  if (inherits(x, "perfil")) {
    columnas <- x$columnas
  } else if (inherits(x, "perfil_dbi")) {
    columnas <- x$resumen_tabla$columnas
  } else if (is.data.frame(x)) {
    columnas <- x
  } else {
    .detener(
      "`", nombre, "` debe ser un `perfil`, un `perfil_dbi` o un frame `columnas`.",
      call. = FALSE
    )
  }
  if (!is.data.frame(columnas) || !"columna" %in% names(columnas)) {
    .detener(
      "`", nombre, "` no contiene un frame `columnas` con el campo `columna`.",
      call. = FALSE
    )
  }
  nombres <- as.character(columnas$columna)
  if (anyNA(nombres) || anyDuplicated(.nombres_para_operar(nombres))) {
    .detener(
      "El frame `columnas` de `", nombre,
      "` debe tener nombres de columna presentes y sin duplicados.", call. = FALSE
    )
  }
  columnas
}

.valor_equivalencia <- function(columnas, campo, indice) {
  columnas[[campo]][[indice]]
}

.igualdad_equivalencia <- function(a, b) {
  resultado <- tryCatch(a == b, error = function(e) FALSE)
  isTRUE(resultado)
}

.faltante_equivalencia <- function(x) {
  resultado <- tryCatch(is.na(x), error = function(e) FALSE)
  isTRUE(resultado)
}

.infinito_equivalencia <- function(x) {
  if (length(x) != 1L || !is.numeric(x)) return(FALSE)
  isTRUE(is.infinite(x))
}

.comparar_valor_equivalencia <- function(a, b, tipo_eje, tolerancia) {
  if (length(a) != 1L || length(b) != 1L) {
    .detener(
      "Cada valor de un campo comparable debe ser escalar.", call. = FALSE
    )
  }
  faltante_a <- .faltante_equivalencia(a)
  faltante_b <- .faltante_equivalencia(b)
  if (faltante_a && faltante_b) {
    return(list(
      veredicto = "no_comparable",
      motivo = "sin_valor_medible_en_ambos",
      diferencia_normalizada = NA_real_
    ))
  }
  if (xor(faltante_a, faltante_b)) {
    return(list(
      veredicto = "no_comparable", motivo = "valor_medible_en_un_solo_lado",
      diferencia_normalizada = NA_real_
    ))
  }
  if (.infinito_equivalencia(a) || .infinito_equivalencia(b)) {
    igual <- .igualdad_equivalencia(a, b)
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else "infinito",
      diferencia_normalizada = NA_real_
    ))
  }

  igual <- .igualdad_equivalencia(a, b)
  if (tipo_eje != "flotante") {
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else paste0("eje_", tipo_eje),
      diferencia_normalizada = NA_real_
    ))
  }
  if (!is.numeric(a) || !is.numeric(b) ||
      !isTRUE(is.finite(a)) || !isTRUE(is.finite(b))) {
    .detener(
      "Los campos del eje `flotante` deben contener numeros finitos o faltantes.",
      call. = FALSE
    )
  }
  # La tolerancia es MIXTA -la misma que usa la deteccion de relaciones
  # aritmeticas-: la diferencia se divide por el mayor de 1, |a| y |b|. Por
  # debajo de magnitud 1 el divisor es 1 y la comparacion es ABSOLUTA, que es lo
  # correcto cerca de cero -dividir por casi nada convierte el ruido en un
  # cambio del 100%- y lo que hay que DECIR: una media de 0,001 que pasa a 0,4
  # queda dentro de una tolerancia de 0,5, y el veredicto `equivalente` viajaba
  # solo, con una columna llamada `diferencia_relativa` que ahi no era relativa.
  # Ahora la columna se llama por lo que es y el motivo declara la escala.
  escala <- pmax(1, abs(a), abs(b))
  escala_unidad <- isTRUE(escala == 1)
  diferencia_normalizada <- suppressWarnings(abs(a - b) / escala)
  if (igual) {
    return(list(
      veredicto = "identico", motivo = "igualdad_exacta",
      diferencia_normalizada = as.numeric(diferencia_normalizada)
    ))
  }
  # El veredicto sale de la MISMA cifra que se publica. Calculado como
  # `d <= tol * s` y publicado como `d / s`, en el borde la fila decia
  # `equivalente` al lado de una `diferencia_normalizada` mayor que la tolerancia
  # -y la aritmetica exacta confirma que estaba fuera-.
  dentro <- isTRUE(diferencia_normalizada <= tolerancia)
  motivo <- if (isTRUE(dentro)) "dentro_de_tolerancia" else "fuera_de_tolerancia"
  if (escala_unidad) motivo <- paste0(motivo, "_escala_unidad")
  list(
    veredicto = if (isTRUE(dentro)) "equivalente" else
      "materialmente_distinto",
    motivo = motivo,
    diferencia_normalizada = as.numeric(diferencia_normalizada)
  )
}

#' Comparar la equivalencia de dos resúmenes de perfiles
#'
#' Compara por intersección los campos registrados de dos perfiles y devuelve
#' una fila por cada par de columna y campo. Los campos que no tienen un eje
#' registrado no se comparan y quedan declarados en `campos_no_comparables`, y
#' lo mismo pasa con un campo registrado que uno de los dos lados no mide: se
#' declara ahi, con el motivo `campo_solo_en_anterior` o `campo_solo_en_actual`
#' en `detalle_campos_no_comparables` y una fila en `cobertura_diagnosticos`.
#' Los campos bajo protección tampoco se comparan: se declaran por columna,
#' campo y lado en `campos_protegidos`.
#'
#' @param anterior,actual Un objeto `perfil` de [perfilar()], un objeto
#'   `perfil_dbi` de [perfilar_dbi()] o directamente un frame `columnas`.
#' @param tolerancia Número escalar no negativo y finito, declarado por quien
#'   llama. No tiene valor por omisión y se publica en cada fila.
#'
#'   **La tolerancia es mixta, no relativa.** La diferencia se divide por el
#'   mayor de `1`, `|anterior|` y `|actual|`, que es la misma regla que usa la
#'   detección de relaciones aritméticas: por debajo de magnitud 1 el divisor es
#'   1 y la comparación pasa a ser absoluta, porque dividir por casi cero
#'   convierte el ruido en un cambio del cien por ciento. Eso tiene una
#'   consecuencia que conviene tener presente: una media que pasa de `0,001` a
#'   `0,4` queda dentro de una tolerancia de `0,5`. Cuando la escala usada fue
#'   la unidad y no el valor, el `motivo` de esa fila lo dice
#'   (`dentro_de_tolerancia_escala_unidad`, `fuera_de_tolerancia_escala_unidad`)
#'   y la columna publicada se llama `diferencia_normalizada`, no «relativa».
#'
#' @return Un frame de clase `equivalencia_perfiles` con `columna`, `campo`,
#'   `valor_anterior`, `valor_actual`, `diferencia_normalizada`, `veredicto`,
#'   `motivo`, `tipo_eje` y `tolerancia`. `veredicto` es un factor ordenado con
#'   niveles `identico < equivalente < materialmente_distinto`. Los atributos
#'   `campos_no_comparables`, `detalle_campos_no_comparables`,
#'   `columnas_no_comparables`, `cobertura_diagnosticos`, `campos_protegidos`
#'   y `resumen` declaran, respectivamente, los campos omitidos, los motivos
#'   estructurales de esos campos, las columnas presentes en un solo lado o
#'   con tipos incompatibles, los diagnósticos que no se pudieron evaluar, los
#'   campos omitidos por protección y el conteo de cada veredicto.
#'   Un campo sin valor medible en ambos lados, o con valor en un solo lado,
#'   queda en la tabla con veredicto `no_comparable` y no suma como acuerdo.
#'   `campos_protegidos` es un data frame con las columnas `columna`, `campo` y
#'   `lado`; este último toma los valores `anterior` y `actual`.
#'
#' @details
#' El registro fijo asigna tolerancia sólo a `media`, `mediana`, `desvio` y
#' `longitud_media`. Los conteos, proporciones de conteos y extremos por
#' selección se comparan en el eje `exacto`; las fechas canónicas en `fecha` y
#' `moda` y `centinela_valor` en `valor`. Los ejes `exacto`, `fecha` y `valor`
#' son binarios por construcción.
#'
#' El comparador devuelve datos, no decisiones: no alimenta hallazgos,
#' severidades ni puntajes. La tolerancia es del llamador y jamás entra en una
#' regla del paquete; sólo se aplica al eje flotante finito y queda publicada
#' para que el llamador decida cómo usarla.
#'
#' Si una columna es temporal en exactamente uno de los perfiles, los campos
#' de magnitud numérica se omiten por el cambio de esquema y su motivo queda en
#' `detalle_campos_no_comparables` como
#' `tipo_cambiado:temporal_vs_no_temporal`. Así no se comparan duraciones en
#' segundos contra magnitudes numéricas sin unidad común. Cuando ambos lados
#' son temporales, `desvio` sí se compara en flotante porque ambas puertas lo
#' expresan en segundos.
#'
#' Si un lado es temporal y el otro **no publica su tipo** —el frame de
#' [perfilar_dbi()] no trae ningún campo de tipo—, no se afirma un cambio de
#' tipo: los mismos campos se omiten con el motivo
#' `tipo_desconocido:temporal_vs_sin_tipo`, porque tampoco se puede garantizar
#' una unidad común. Una fecha guardada como número en el motor tiene su
#' `desvio` en días de ese lado y en segundos del otro, y compararlos
#' publicaría como diferencia del dato una diferencia de unidad.
#'
#' Si una columna guarda caracteres en exactamente uno de los perfiles, los
#' campos calculados sobre la representación —longitudes, variantes unicode y
#' números escritos como texto— se omiten y su motivo queda en
#' `detalle_campos_no_comparables` como `tipo_cambiado:texto_vs_no_texto`. Esos
#' campos cambian con sólo cambiar el almacenamiento, aunque el dato sea el
#' mismo, de modo que compararlos publicaría el síntoma y callaría la causa.
#' `factor` cuenta como almacenamiento de caracteres. Este guarda lee el tipo
#' *declarado* y no el inferido: si el texto contiene números o fechas, la
#' inferencia borra justo la diferencia que hay que ver. Cuando el tipo
#' declarado viene de un vocabulario ajeno al de memoria —un tipo SQL, por
#' ejemplo— no se afirma nada y la comparación sigue como siempre.
#'
#' Por la misma razón, si una columna lleva zona horaria en exactamente uno de
#' los perfiles, los campos que sólo ese almacenamiento puede medir se omiten
#' con el motivo `tipo_cambiado:con_zona_vs_sin_zona`. Del otro lado no dan un
#' valor distinto: no dan ninguno, porque no hay zona que comparar contra UTC.
#' Los extremos de fecha **sí** se siguen comparando: al guardarse como texto
#' la columna pierde la zona y los instantes que denota son realmente otros,
#' que es lo más importante que hay para informar en ese caso.
#'
#' @name comparar_equivalencia
#' @usage comparar_equivalencia(anterior, actual, tolerancia)
#' @export
#' @seealso [comparar_perfiles()], [perfilar()], [perfilar_dbi()]
#'
#' @examples
#' anterior <- data.frame(
#'   columna = "monto", media = 10, minimo = 1, moda = "a",
#'   stringsAsFactors = FALSE
#' )
#' actual <- data.frame(
#'   columna = "monto", media = 10.00000000001, minimo = 2, moda = "b",
#'   stringsAsFactors = FALSE
#' )
#' comparar_equivalencia(anterior, actual, tolerancia = 1e-9)
comparar_equivalencia <- function(anterior, actual, tolerancia) {
  if (!is.numeric(tolerancia) || length(tolerancia) != 1L ||
      is.na(tolerancia) || !is.finite(tolerancia) || tolerancia < 0) {
    .detener(
      "`tolerancia` debe ser un escalar numerico finito, sin NA y mayor o igual a 0.",
      call. = FALSE
    )
  }
  tolerancia <- as.numeric(tolerancia)
  anterior <- .columnas_equivalencia(anterior, "anterior")
  actual <- .columnas_equivalencia(actual, "actual")
  mapa_anterior <- data.frame(
    clave = .nombres_unicos(.nombres_para_operar(anterior$columna)),
    original = as.character(anterior$columna),
    stringsAsFactors = FALSE
  )
  mapa_actual <- data.frame(
    clave = .nombres_unicos(.nombres_para_operar(actual$columna)),
    original = as.character(actual$columna),
    stringsAsFactors = FALSE
  )
  registro <- .registro_campos_equivalencia()
  campos_registrados <- unlist(registro, use.names = FALSE)
  campos_presentes <- unique(c(names(anterior), names(actual)))
  campos_no_comparables <- setdiff(
    setdiff(campos_presentes, "columna"), campos_registrados
  )
  campos <- intersect(
    setdiff(names(anterior), "columna"), setdiff(names(actual), "columna")
  )
  campos <- campos[campos %in% campos_registrados]
  columnas <- intersect(mapa_anterior$clave, mapa_actual$clave)
  campos_magnitud <- .campos_magnitud_equivalencia(registro)
  campos_representacion <- .campos_representacion_equivalencia(registro)
  campos_zona <- .campos_zona_horaria_equivalencia(registro)
  columnas_no_comparables <- data.frame(
    columna = character(), lado = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  detalle_campos_no_comparables <- data.frame(
    columna = character(), campo = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  registrar_no_comparable <- function(columna, lado, motivo, campo = NA_character_) {
    columnas_no_comparables <<- rbind(
      columnas_no_comparables,
      data.frame(columna = columna, lado = lado, motivo = motivo,
                 stringsAsFactors = FALSE)
    )
    detalle_campos_no_comparables <<- rbind(
      detalle_campos_no_comparables,
      data.frame(columna = columna, campo = campo, motivo = motivo,
                 stringsAsFactors = FALSE)
    )
    cobertura_diagnosticos <<- rbind(
      cobertura_diagnosticos,
      .nuevo_diagnostico_no_evaluado(
        "comparar_equivalencia", columna,
        paste0("no_comparable: ", motivo),
        "Asegurar que la columna exista en ambas corridas y conserve un tipo comparable."
      )
    )
  }
  solo_anterior <- setdiff(mapa_anterior$clave, mapa_actual$clave)
  solo_actual <- setdiff(mapa_actual$clave, mapa_anterior$clave)
  for (clave in solo_anterior) {
    columna <- mapa_anterior$original[match(clave, mapa_anterior$clave)]
    registrar_no_comparable(
      columna, "anterior", "columna_solo_en_anterior"
    )
  }
  for (clave in solo_actual) {
    columna <- mapa_actual$original[match(clave, mapa_actual$clave)]
    registrar_no_comparable(
      columna, "actual", "columna_solo_en_actual"
    )
  }
  # Lo mismo que las dos vueltas de arriba hacen con una columna presente en un
  # solo lado, pero para un CAMPO. Un campo registrado que existe en un lado y no
  # en el otro se caia de las dos listas: no entraba en `campos` -que es la
  # interseccion- ni en `campos_no_comparables` -que solo recogia los campos sin
  # eje registrado-, asi que no aparecia en ninguna parte de la salida y el
  # `resumen` contaba el universo comparado como si fuera el universo entero.
  # Es el invariante central: el silencio se declara.
  declarar_campo_de_un_lado <- function(campo, lado) {
    campos_no_comparables <<- unique(c(campos_no_comparables, campo))
    motivo <- paste0("campo_solo_en_", lado)
    # `columna = NA_character_` porque no falta en una columna: falta en el
    # resumen entero, y atribuirselo a una columna seria inventar un alcance.
    detalle_campos_no_comparables <<- rbind(
      detalle_campos_no_comparables,
      data.frame(columna = NA_character_, campo = campo, motivo = motivo,
                 stringsAsFactors = FALSE)
    )
    cobertura_diagnosticos <<- rbind(
      cobertura_diagnosticos,
      .nuevo_diagnostico_no_evaluado(
        "comparar_equivalencia", NA_character_,
        paste0("no_comparable: ", motivo, ": ", campo),
        paste0(
          "Medir el campo '", campo, "' en las dos corridas para que se pueda ",
          "comparar."
        )
      )
    )
  }
  campos_solo_anterior <- setdiff(
    setdiff(names(anterior), "columna"), names(actual)
  )
  campos_solo_actual <- setdiff(
    setdiff(names(actual), "columna"), names(anterior)
  )
  for (campo in intersect(campos_solo_anterior, campos_registrados)) {
    declarar_campo_de_un_lado(campo, "anterior")
  }
  for (campo in intersect(campos_solo_actual, campos_registrados)) {
    declarar_campo_de_un_lado(campo, "actual")
  }
  campos_protegidos <- .campos_protegidos_equivalencia_vacios()
  niveles <- c(
    "identico", "equivalente", "materialmente_distinto", "no_comparable"
  )
  salida <- list()
  k <- 0L
  for (clave in columnas) {
    indice_a <- match(clave, mapa_anterior$clave)
    indice_b <- match(clave, mapa_actual$clave)
    columna <- mapa_anterior$original[[indice_a]]
    temporal_a <- .es_temporal_equivalencia(anterior, indice_a)
    temporal_b <- .es_temporal_equivalencia(actual, indice_b)
    # Solo con los dos tipos conocidos se puede afirmar que cambio. Con un lado
    # temporal y el otro sin tipo tampoco se puede garantizar una unidad comun,
    # y abstenerse del todo -comparar como siempre- publicaba como diferencia
    # del dato una diferencia de unidad: la misma columna `Date` guardada como
    # REAL en SQLite tiene `desvio` en dias del lado DBI y en segundos del lado
    # de memoria. Se omiten los mismos campos, con un motivo que no afirma un
    # cambio que nadie midio.
    temporal_cambiado <- !is.na(temporal_a) && !is.na(temporal_b) &&
      xor(temporal_a, temporal_b)
    temporal_sin_tipo <- (isTRUE(temporal_a) && is.na(temporal_b)) ||
      (is.na(temporal_a) && isTRUE(temporal_b))
    tipo_a <- .tipo_columna_equivalencia(anterior, indice_a)
    tipo_b <- .tipo_columna_equivalencia(actual, indice_b)
    declarado_a <- .tipo_declarado_equivalencia(anterior, indice_a)
    declarado_b <- .tipo_declarado_equivalencia(actual, indice_b)
    caracter_a <- .almacenamiento_caracter_equivalencia(anterior, indice_a)
    caracter_b <- .almacenamiento_caracter_equivalencia(actual, indice_b)
    caracter_cambiado <- !is.na(caracter_a) && !is.na(caracter_b) &&
      xor(caracter_a, caracter_b)
    zona_a <- .almacenamiento_con_zona_equivalencia(anterior, indice_a)
    zona_b <- .almacenamiento_con_zona_equivalencia(actual, indice_b)
    zona_cambiada <- !is.na(zona_a) && !is.na(zona_b) && xor(zona_a, zona_b)
    tipo_cambiado <- !is.na(tipo_a) && !is.na(tipo_b) &&
      !identical(tipo_a, tipo_b)
    for (campo in campos) {
      protegido_a <- .campo_protegido_equivalencia(anterior, campo, indice_a)
      protegido_b <- .campo_protegido_equivalencia(actual, campo, indice_b)
      if (protegido_a || protegido_b) {
        registros <- list()
        if (protegido_a) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "anterior",
            stringsAsFactors = FALSE
          )
        }
        if (protegido_b) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "actual",
            stringsAsFactors = FALSE
          )
        }
        campos_protegidos <- rbind(campos_protegidos, do.call(rbind, registros))
        next
      }
      if ((temporal_cambiado || temporal_sin_tipo) &&
          campo %in% campos_magnitud) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos",
          if (temporal_cambiado) {
            "tipo_cambiado:temporal_vs_no_temporal"
          } else {
            "tipo_desconocido:temporal_vs_sin_tipo"
          },
          campo
        )
        next
      }
      if (caracter_cambiado && campo %in% campos_representacion) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos", "tipo_cambiado:texto_vs_no_texto", campo
        )
        next
      }
      if (zona_cambiada && campo %in% campos_zona) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos", "tipo_cambiado:con_zona_vs_sin_zona", campo
        )
        next
      }
      tipo_eje <- names(registro)[vapply(
        registro, function(campos_eje) campo %in% campos_eje, logical(1L)
      )]
      a <- .valor_equivalencia(anterior, campo, indice_a)
      b <- .valor_equivalencia(actual, campo, indice_b)
      if (tipo_cambiado && campo %in% campos_magnitud &&
          .faltante_equivalencia(a) && .faltante_equivalencia(b)) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos",
          paste0("tipo_cambiado:", .nombre_tipo_motivo(declarado_a, tipo_a),
                 "_vs_", .nombre_tipo_motivo(declarado_b, tipo_b)),
          campo
        )
        next
      }
      comparacion <- .comparar_valor_equivalencia(
        a, b, tipo_eje[[1L]], tolerancia
      )
      k <- k + 1L
      salida[[k]] <- list(
        columna = columna, campo = campo,
        valor_anterior = a, valor_actual = b,
        diferencia_normalizada = comparacion$diferencia_normalizada,
        veredicto = comparacion$veredicto, motivo = comparacion$motivo,
        tipo_eje = tipo_eje[[1L]], tolerancia = tolerancia
      )
    }
  }
  if (length(salida)) {
    resultado <- data.frame(
      columna = vapply(salida, `[[`, character(1L), "columna"),
      campo = vapply(salida, `[[`, character(1L), "campo"),
      # Se rinden para publicar: un valor declarado `bytes` dentro de una
      # columna-lista hacia que `print()` del resultado abortara con "width is
      # not computable in bytes encoding". La comparacion corria bien y el
      # objeto no se podia mostrar ni exportar; bastaba con que UNO de los dos
      # lados llevara la marca, aunque los bytes fueran identicos.
      valor_anterior = I(lapply(
        lapply(salida, `[[`, "valor_anterior"), .texto_publicable
      )),
      valor_actual = I(lapply(
        lapply(salida, `[[`, "valor_actual"), .texto_publicable
      )),
      diferencia_normalizada = vapply(
        salida, `[[`, numeric(1L), "diferencia_normalizada"
      ),
      veredicto = vapply(salida, `[[`, character(1L), "veredicto"),
      motivo = vapply(salida, `[[`, character(1L), "motivo"),
      tipo_eje = vapply(salida, `[[`, character(1L), "tipo_eje"),
      tolerancia = vapply(salida, `[[`, numeric(1L), "tolerancia"),
      stringsAsFactors = FALSE
    )
  } else {
    resultado <- data.frame(
      columna = character(), campo = character(),
      valor_anterior = I(list()), valor_actual = I(list()),
      diferencia_normalizada = numeric(), veredicto = character(),
      motivo = character(), tipo_eje = character(), tolerancia = numeric(),
      stringsAsFactors = FALSE
    )
  }
  resultado$veredicto <- factor(
    resultado$veredicto, levels = niveles, ordered = TRUE
  )
  resumen <- stats::setNames(
    as.integer(table(factor(resultado$veredicto, levels = niveles))), niveles
  )
  attr(resultado, "campos_no_comparables") <- campos_no_comparables
  attr(resultado, "detalle_campos_no_comparables") <-
    detalle_campos_no_comparables
  columnas_no_comparables <- columnas_no_comparables[
    !duplicated(columnas_no_comparables), , drop = FALSE
  ]
  cobertura_diagnosticos <- cobertura_diagnosticos[
    !duplicated(cobertura_diagnosticos[c("diagnostico", "columna", "motivo")]),
    , drop = FALSE
  ]
  attr(resultado, "columnas_no_comparables") <- columnas_no_comparables
  attr(resultado, "cobertura_diagnosticos") <- cobertura_diagnosticos
  attr(resultado, "campos_protegidos") <- campos_protegidos
  attr(resultado, "resumen") <- resumen
  rownames(resultado) <- NULL
  class(resultado) <- c("equivalencia_perfiles", "data.frame")
  resultado
}
