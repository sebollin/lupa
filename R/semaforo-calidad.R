#' Semáforo de calidad por tipo de indicador
#'
#' Clasifica cada celda del tablero —una métrica sobre un objeto— en niveles,
#' con cortes que declara quien mide: por dimensión del marco y, si hace falta,
#' por métrica. **Sólo pinta una cifra cuyas cuentas cierran**: lo que el resumen
#' de [medir()] o de [medir_dbi()] declara en `estado_cuentas`. Una cifra que no
#' cierra —lo no evaluado no son los vacíos que la métrica declara no evaluar, o
#' la tabla cambió entre dos lecturas de la corrida—, una calculada sobre una
#' parte (un vocabulario truncado por `max_valores`) y una sin cuentas de su
#' universo quedan sin color, con su motivo.
#'
#' No hay cortes de fábrica ni una cifra que combine dimensiones: no hay «peor
#' color» de una tabla ni total. `conteo_niveles` es una distribución por
#' dimensión.
#'
#' @section Cuándo una celda tiene color:
#' Todas a la vez:
#'
#' - sus cuentas cierran (`estado_cuentas == "cierran"`). Una métrica que da una
#'   cifra por objeto sin cuentas de su universo —un estadístico, una métrica
#'   propia de granularidad `entidad`— no se pinta aunque su resultado esté en
#'   `[0, 1]`: sin universo no hay cómo saber qué representa;
#' - el resultado es finito (un denominador cero queda `sin_denominador`, sin
#'   abortar);
#' - es una proporción: la orientación operativa del tablero no es `no_aplica`
#'   (por tipo, unidad o valores fuera de `[0, 1]`);
#' - hay un corte para ella.
#'
#' La métrica evalúa **su** universo: una de validez evalúa los valores
#' presentes y los vacíos son de la completitud, que tiene su métrica. Por eso
#' una columna de validez con vacíos tiene color si evaluó todos sus valores, y
#' `fraccion_evaluada` se calcula sobre ese universo propio.
#'
#' Las métricas `no_resumible` (días o cambios de desactualización por celda) no
#' tienen celda: ningún corte en `[0, 1]` las juzga. Quedan en
#' `fuera_del_semaforo` con su motivo, y el semáforo de las demás no aborta.
#'
#' @section Los cortes:
#' `cortes` en dos formas:
#'
#' - una lista con nombres de dimensión, cada elemento un vector de
#'   `length(niveles) - 1` cortes estrictamente crecientes en `[0, 1]`:
#'   `list(Completitud = c(0.80, 0.95), Unicidad = c(0.99, 1))`;
#' - un `data.frame` con `por` (`"dimension"`, `"metrica"`,
#'   `"metrica_especifica"` o `"metrica_instanciada"`), `nombre` y una columna por
#'   nivel que abre, con el nombre del nivel (con los niveles por omisión,
#'   `amarillo` y `verde`).
#'
#' Manda la regla más específica: instanciada, específica, genérica, dimensión.
#' Una clave sin celdas en esta medición no es un error —una política de cortes
#' se reusa entre muchas tablas— y se declara en `cortes_sin_celdas`; sí lo es
#' una que coincide con un nombre existente salvo por la caja o la forma
#' Unicode.
#'
#' La comparación es `valor_comparado >= corte` con la tolerancia de redondeo de
#' `ratio_umbral` en [agregar()]; `valor_comparado` es `valor` o `1 - valor` en
#' las métricas de defecto, la convención de [tablero_calidad()] y
#' [indice_calidad()]. Las bandas se publican en la escala de `valor` (`desde`,
#' `hasta`) y la escala comparada en `escala_comparada`.
#'
#' @param medidas Un resumen de medición: `medir(detalle = "resumen")`,
#'   [medir_dbi()] o la consolidación `rbind(..., prioridad = )`. Una medición
#'   completa, un agregado o un tablero no traen las cuentas que el semáforo
#'   necesita y se rechazan con clase `lupa_semaforo_sin_cuentas`. Un resumen
#'   recortado a algunas columnas se rechaza con clase `lupa_resumen_incompleto`
#'   y las columnas que faltan; sin `entidad_referida` el semáforo trabaja y el
#'   control de lecturas es `desconocido`. Las filas vacías que agrega
#'   `x[cond, ]` cuando `cond` tiene `NA` se rechazan igual.
#' @param cortes Lista con nombres de dimensión o `data.frame`, como arriba. Sin
#'   valor por omisión.
#' @param niveles Nombres de los niveles, de peor a mejor.
#' @param marco,cobertura Como en [tablero_calidad()].
#'
#' @return Data frame de clase `semaforo_calidad`, una fila por celda del
#'   tablero, con las columnas del tablero más `metrica_especifica`,
#'   `valor_comparado`, `escala_comparada`, `regla`, `desde`, `hasta`, `nivel`,
#'   `motivo_sin_color`, `fraccion_evaluada`, `estado_cuentas`,
#'   `motivo_cuentas`, `diferencia_cuentas`, `control_lecturas` y, si el
#'   resumen es una consolidación, `fuente`. `control_lecturas` dice cuánto
#'   respalda el control de lecturas a la cifra: `una_lectura` (la tabla se leyó
#'   en una sola sentencia, o en memoria), `completo` (varias lecturas con la
#'   instantánea probada por el contador del motor o la huella de los datos),
#'   `limitado` (varias lecturas y alguna sin instantánea: sólo se comparó el
#'   número de filas, que no ve un `UPDATE` que no lo cambia) o `desconocido`:
#'   no se puede saber qué lecturas respaldan la cifra. Tiene cuatro causas. El
#'   resumen no conserva `id_medicion`, `entidad` o `entidad_referida`. El motor
#'   no resolvió la tabla de alguna lectura: en SQLite y DuckDB se resuelve, en
#'   los demás motores todavía no, y tampoco la trae un resumen medido antes de
#'   que las lecturas registraran su tabla. El nombre de la entidad da más de
#'   una tabla en las partes unidas: una parte la leyó en memoria y otra en el
#'   motor, o dos partes leyeron bases distintas adjuntas con el mismo alias. O
#'   un referencial en R comparte su nombre con otro de las mismas filas y otros
#'   datos, respaldado de otra forma, o con una tabla de otra parte que no
#'   tiene sus mismos datos. Una celda con color y
#'   control `limitado` o `desconocido` lo dice también la impresión.
#'   Atributos: `cortes`, `niveles`, `comparacion`, `conteo_niveles`,
#'   `cortes_sin_celdas`, `fuera_del_semaforo` y los del tablero.
#' @export
#' @seealso [tablero_calidad()], [indice_calidad()], [medir()], [medir_dbi()]
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' modelo_ejemplo <- modelo(
#'   instanciar(especializar(nucleo$NoNulo), "personas", "edad"),
#'   instanciar(especializar(nucleo$ValoresPosiblesPorComprension,
#'                           minimo = 0, maximo = 120), "personas", "edad")
#' )
#' resumen <- medir(modelo_ejemplo,
#'                  list(personas = data.frame(edad = c(20, NA, 35, 150))),
#'                  detalle = "resumen")
#' # Cortes de este ejemplo, no del paquete:
#' semaforo_calidad(resumen, cortes = list(
#'   Completitud = c(0.80, 0.95),
#'   Consistencia = c(0.70, 0.90)
#' ))
semaforo_calidad <- function(medidas, cortes,
                             niveles = c("rojo", "amarillo", "verde"),
                             marco = NULL, cobertura = NULL) {
  if (!.es_medicion_resumen(medidas)) {
    .detener(structure(
      class = c("lupa_semaforo_sin_cuentas", "error", "condition"),
      list(
        message = paste0(
          "`semaforo_calidad()` necesita un resumen de medici\u00f3n: s\u00f3lo pinta ",
          "cifras cuyas cuentas cierran, y ", .clase_entrada_semaforo(medidas),
          " no trae esas cuentas. Medir con `detalle = \"resumen\"` o con ",
          "`medir_dbi()`; el tablero es el mismo."
        ),
        call = NULL
      )
    ))
  }
  .exigir_resumen_entero(medidas, "semaforo_calidad", .COLUMNAS_SEMAFORO_RESUMEN)
  if (missing(cortes)) {
    .detener(
      "`cortes` es obligatorio: no hay cortes de f\u00e1brica.", call. = FALSE
    )
  }
  niveles <- .validar_niveles_semaforo(niveles)
  reglas <- .normalizar_cortes_semaforo(cortes, niveles)
  resumen <- medidas
  fuera <- .fuera_del_semaforo(resumen)
  finito <- is.finite(suppressWarnings(as.numeric(resumen$resultado)))
  sin_den <- resumen[!finito, , drop = FALSE]
  con_valor <- resumen[finito, , drop = FALSE]
  tablero <- if (nrow(con_valor)) {
    .tablero_desde_resumen(con_valor, marco = marco, cobertura = cobertura,
                           declarar_fuera = TRUE)
  } else NULL
  celdas <- .celdas_semaforo(tablero, con_valor, reglas, niveles)
  if (nrow(sin_den)) {
    celdas <- .unir_celdas_semaforo(celdas, .celdas_sin_denominador(sin_den, niveles))
  }
  .validar_claves_cortes(reglas, resumen, marco = if (!is.null(tablero)) {
    attr(tablero, "marco_calidad", exact = TRUE)
  } else marco)
  usadas <- unique(celdas$regla[!is.na(celdas$regla)])
  sin_celdas <- reglas[!paste0(reglas$por, ":", reglas$nombre) %in% usadas, , drop = FALSE]
  rownames(sin_celdas) <- NULL
  if (!is.null(tablero)) {
    for (a in setdiff(names(attributes(tablero)),
                      c("names", "row.names", "class"))) {
      attr(celdas, a) <- attr(tablero, a, exact = TRUE)
    }
  }
  attr(celdas, "cortes") <- reglas
  attr(celdas, "niveles") <- niveles
  attr(celdas, "comparacion") <- paste0(
    "valor_comparado >= corte (tolerancia 64 * .Machine$double.eps); ",
    "valor_comparado = 1 - valor en m\u00e9tricas de defecto"
  )
  attr(celdas, "cortes_sin_celdas") <- sin_celdas
  attr(celdas, "fuera_del_semaforo") <- fuera
  attr(celdas, "conteo_niveles") <- .conteo_niveles_semaforo(
    celdas, niveles, attr(resumen, "cobertura_metricas", exact = TRUE), fuera
  )
  class(celdas) <- c("semaforo_calidad", "data.frame")
  celdas
}

.clase_entrada_semaforo <- function(x) {
  if (inherits(x, "tablero_calidad")) return("un tablero")
  if (inherits(x, "analisis")) return("un an\u00e1lisis")
  if (inherits(x, "medicion")) return("una medici\u00f3n completa o agregada")
  paste0("un objeto de clase ", paste(class(x), collapse = "/"))
}

.validar_niveles_semaforo <- function(niveles) {
  if (!is.character(niveles) || length(niveles) < 2L || anyNA(niveles) ||
      any(!nzchar(niveles)) || anyDuplicated(.clave_bytes(niveles))) {
    .detener(
      "`niveles` debe tener al menos dos nombres distintos, de peor a mejor.",
      call. = FALSE
    )
  }
  niveles
}

# Las reglas de corte como una tabla: `por`, `nombre` y un corte por nivel que
# se abre.
.normalizar_cortes_semaforo <- function(cortes, niveles) {
  abre <- niveles[-1L]
  if (is.list(cortes) && !is.data.frame(cortes)) {
    if (!length(cortes) || is.null(names(cortes)) || anyNA(names(cortes)) ||
        any(!nzchar(names(cortes)))) {
      .detener(
        "`cortes` debe ser una lista con nombres de dimensi\u00f3n o un data.frame, ",
        "con al menos una regla.", call. = FALSE
      )
    }
    filas <- lapply(names(cortes), function(n) {
      v <- cortes[[n]]
      if (!is.numeric(v) || length(v) != length(abre)) {
        .detener(
          "Los cortes de `", n, "` deben ser ", length(abre),
          " n\u00fameros, uno por nivel que se abre (", paste(abre, collapse = ", "), ").",
          call. = FALSE
        )
      }
      fila <- data.frame(por = "dimension", nombre = n, stringsAsFactors = FALSE)
      for (k in seq_along(abre)) fila[[abre[[k]]]] <- as.numeric(v[[k]])
      fila
    })
    cortes <- do.call(rbind, filas)
  }
  if (!is.data.frame(cortes) || !nrow(cortes) ||
      !all(c("por", "nombre", abre) %in% names(cortes))) {
    .detener(
      "`cortes` debe tener al menos una regla, con `por`, `nombre` y una columna ",
      "por nivel que se abre (", paste(abre, collapse = ", "), ").", call. = FALSE
    )
  }
  cortes <- as.data.frame(cortes, stringsAsFactors = FALSE)
  extra <- setdiff(names(cortes), c("por", "nombre", abre))
  if (length(extra)) {
    .detener(
      "`cortes` trae columnas que no son niveles: ", paste(extra, collapse = ", "),
      ". Los niveles son: ", paste(niveles, collapse = ", "), ".", call. = FALSE
    )
  }
  cortes$por <- as.character(cortes$por)
  cortes$nombre <- as.character(cortes$nombre)
  validos <- c("dimension", "metrica", "metrica_especifica", "metrica_instanciada")
  if (anyNA(cortes$por) || any(!cortes$por %in% validos) || anyNA(cortes$nombre) ||
      any(!nzchar(cortes$nombre))) {
    .detener(
      "`por` debe ser uno de ", paste(validos, collapse = ", "),
      ", y `nombre` un texto no vac\u00edo.", call. = FALSE
    )
  }
  if (anyDuplicated(.clave_bytes(paste(cortes$por, cortes$nombre, sep = "\034")))) {
    .detener("Cada regla de `cortes` (`por`, `nombre`) va una sola vez.", call. = FALSE)
  }
  m <- as.matrix(cortes[abre])
  if (!is.numeric(m) || anyNA(m) || any(!is.finite(m)) || any(m < 0 | m > 1) ||
      (ncol(m) > 1L && any(t(apply(m, 1L, diff)) <= 0))) {
    .detener(
      "Los cortes de cada regla deben ser n\u00fameros estrictamente crecientes en [0, 1].",
      call. = FALSE
    )
  }
  cortes[abre] <- lapply(cortes[abre], as.numeric)
  rownames(cortes) <- NULL
  cortes
}

.fuera_del_semaforo <- function(resumen) {
  cob <- attr(resumen, "cobertura_metricas", exact = TRUE)
  vacio <- data.frame(
    metrica_instanciada = character(), entidad = character(), motivo = character(),
    como_resolverlo = character(), stringsAsFactors = FALSE
  )
  if (!inherits(cob, "data.frame") || !nrow(cob)) return(vacio)
  nr <- cob[cob$estado == "no_resumible", , drop = FALSE]
  if (!nrow(nr)) return(vacio)
  data.frame(
    metrica_instanciada = nr$metrica_instanciada,
    entidad = nr$entidad,
    motivo = paste0(
      "La m\u00e9trica da un valor no acotado por celda (d\u00edas o cambios) que el ",
      "resumen no promedia, y ning\u00fan corte en [0, 1] lo juzga."
    ),
    como_resolverlo = paste(
      "Medirla con `detalle = \"completo\"` y leerla en su escala; el sem\u00e1foro",
      "de las dem\u00e1s no cambia."
    ),
    stringsAsFactors = FALSE
  )
}

# El universo propio de una fila: todo el universo aplicable si la metrica
# evalua tambien los vacios, o solo lo que tiene valor.
.universo_propio_resumen <- function(resumen) {
  vacios <- .RUTAS_CATALOGO_METRICAS$universo_vacios[
    match(as.character(resumen$metrica), .RUTAS_CATALOGO_METRICAS$metrica)
  ]
  sv <- resumen$n_sin_valor
  sv[is.na(sv)] <- 0
  inferido <- is.na(vacios)
  vacios[inferido] <- ifelse(
    !is.na(resumen$n_no_evaluados[inferido]) &
      resumen$n_no_evaluados[inferido] == sv[inferido] & sv[inferido] > 0,
    "excluye", "incluye"
  )
  ifelse(vacios %in% c("excluye", "excluye_sin_texto"), resumen$n_universo - sv,
         resumen$n_universo)
}

.regla_para_celda <- function(reglas, celda) {
  for (por in c("metrica_instanciada", "metrica_especifica", "metrica", "dimension")) {
    valor <- celda[[por]]
    if (is.null(valor) || is.na(valor)) next
    i <- which(reglas$por == por &
                 .clave_bytes(reglas$nombre) == .clave_bytes(valor))
    if (length(i)) return(i[[1L]])
  }
  NA_integer_
}

.celdas_semaforo <- function(tablero, resumen, reglas, niveles) {
  if (is.null(tablero) || !nrow(tablero)) return(.celdas_semaforo_vacias(niveles))
  abre <- niveles[-1L]
  n <- nrow(tablero)
  base <- as.data.frame(unclass(tablero), stringsAsFactors = FALSE)
  base$metrica_especifica <- resumen$metrica_especifica
  orientacion <- base$orientacion
  valor <- as.numeric(base$valor)
  defecto <- !is.na(orientacion) & orientacion == "defecto"
  comparado <- ifelse(defecto, 1 - valor, valor)
  comparado[!is.na(orientacion) & orientacion == "no_aplica"] <- NA_real_
  propio <- .universo_propio_resumen(resumen)
  control <- .alcance_lecturas_filas(resumen)
  salida <- data.frame(
    base[c("componente", "dimension", "factor", "metrica")],
    metrica_especifica = resumen$metrica_especifica,
    base[c("metrica_instanciada", "entidad", "objeto", "valor", "orientacion",
           "agregacion", "universo")],
    valor_comparado = comparado,
    escala_comparada = ifelse(defecto, "1 - valor", "valor"),
    regla = NA_character_, desde = NA_real_, hasta = NA_real_,
    nivel = NA_character_, motivo_sin_color = NA_character_,
    fraccion_evaluada = as.numeric(resumen$denominador) / propio,
    estado_cuentas = resumen$estado_cuentas,
    motivo_cuentas = resumen$motivo_cuentas,
    diferencia_cuentas = resumen$diferencia_cuentas,
    control_lecturas = control,
    stringsAsFactors = FALSE, row.names = NULL
  )
  if ("fuente" %in% names(base)) salida$fuente <- base$fuente
  tolerancia <- 64 * .Machine$double.eps
  for (i in seq_len(n)) {
    k <- .regla_para_celda(reglas, salida[i, , drop = FALSE])
    if (!is.na(k)) salida$regla[[i]] <- paste0(reglas$por[[k]], ":", reglas$nombre[[k]])
    estado <- salida$estado_cuentas[[i]]
    motivo <- if (!identical(estado, "cierran")) {
      if (is.na(estado)) "sin_cuentas" else estado
    } else if (is.na(comparado[[i]])) {
      "no_acotada"
    } else if (is.na(k)) {
      if (is.na(salida$dimension[[i]]) || !nzchar(salida$dimension[[i]])) {
        "sin_dimension"
      } else "sin_corte"
    } else NA_character_
    if (!is.na(motivo)) {
      salida$motivo_sin_color[[i]] <- motivo
      next
    }
    c_i <- as.numeric(reglas[k, abre])
    alcanzados <- sum(comparado[[i]] >= c_i - tolerancia)
    desde_c <- c(0, c_i)[[alcanzados + 1L]]
    hasta_c <- c(c_i, 1)[[alcanzados + 1L]]
    salida$nivel[[i]] <- niveles[[alcanzados + 1L]]
    if (defecto[[i]]) {
      salida$desde[[i]] <- 1 - hasta_c
      salida$hasta[[i]] <- 1 - desde_c
    } else {
      salida$desde[[i]] <- desde_c
      salida$hasta[[i]] <- hasta_c
    }
  }
  salida
}

.celdas_semaforo_vacias <- function(niveles) {
  data.frame(
    componente = character(), dimension = character(), factor = character(),
    metrica = character(), metrica_especifica = character(),
    metrica_instanciada = character(), entidad = character(), objeto = character(),
    valor = numeric(), orientacion = character(), agregacion = character(),
    universo = character(), valor_comparado = numeric(),
    escala_comparada = character(), regla = character(), desde = numeric(),
    hasta = numeric(), nivel = character(), motivo_sin_color = character(),
    fraccion_evaluada = numeric(), estado_cuentas = character(),
    motivo_cuentas = character(), diferencia_cuentas = numeric(),
    control_lecturas = character(), stringsAsFactors = FALSE
  )
}

# Las filas sin resultado finito: un denominador cero no tumba el semaforo, y la
# fila queda a la vista, sin color.
.celdas_sin_denominador <- function(filas, niveles) {
  vacias <- .celdas_semaforo_vacias(niveles)
  salida <- vacias[rep(NA_integer_, nrow(filas)), , drop = FALSE]
  rownames(salida) <- NULL
  salida$componente <- NA_character_
  salida$dimension <- filas$dimension
  salida$factor <- filas$factor
  salida$metrica <- filas$metrica
  salida$metrica_especifica <- filas$metrica_especifica
  salida$metrica_instanciada <- filas$metrica_instanciada
  salida$entidad <- filas$entidad
  salida$objeto <- filas$objeto_medible
  salida$valor <- suppressWarnings(as.numeric(filas$resultado))
  salida$orientacion <- filas$orientacion
  salida$agregacion <- filas$agregacion
  salida$motivo_sin_color <- "sin_denominador"
  salida$estado_cuentas <- filas$estado_cuentas
  salida$motivo_cuentas <- filas$motivo_cuentas
  salida$diferencia_cuentas <- filas$diferencia_cuentas
  salida$control_lecturas <- NA_character_
  if ("fuente" %in% names(filas)) salida$fuente <- filas$fuente
  salida
}

.unir_celdas_semaforo <- function(a, b) {
  for (col in setdiff(names(a), names(b))) b[[col]] <- rep(NA, nrow(b))
  for (col in setdiff(names(b), names(a))) a[[col]] <- rep(NA, nrow(a))
  salida <- rbind(a, b[names(a)])
  rownames(salida) <- NULL
  salida
}

# Una clave que no esta en esta medicion se acepta y se declara. Se rechaza la
# que coincide con un nombre existente salvo por la caja o la forma Unicode:
# ahi lo mas probable es un error de tipeo, y aceptarla dejaria la celda sin
# corte sin que nadie lo note.
.validar_claves_cortes <- function(reglas, resumen, marco = NULL) {
  cob <- attr(resumen, "cobertura_metricas", exact = TRUE)
  sello <- attr(resumen, "sello_metricas", exact = TRUE)
  vocab <- list(
    dimension = unique(c(
      as.character(resumen$dimension),
      if (inherits(marco, "marco_calidad")) marco$factores$dimension,
      if (inherits(cob, "data.frame")) as.character(cob$dimension)
    )),
    metrica = unique(c(as.character(resumen$metrica),
                       if (inherits(cob, "data.frame")) as.character(cob$metrica))),
    metrica_especifica = unique(c(
      as.character(resumen$metrica_especifica),
      if (inherits(cob, "data.frame")) as.character(cob$metrica_especifica),
      if (inherits(sello, "data.frame")) as.character(sello$metrica_especifica)
    )),
    metrica_instanciada = unique(c(
      as.character(resumen$metrica_instanciada),
      if (inherits(cob, "data.frame")) as.character(cob$metrica_instanciada),
      if (inherits(sello, "data.frame")) as.character(sello$metrica_instanciada)
    ))
  )
  plegar <- function(v) tryCatch(.plegar_para_comparar(v), error = function(e) v)
  for (i in seq_len(nrow(reglas))) {
    v <- vocab[[reglas$por[[i]]]]
    v <- v[!is.na(v)]
    nombre <- reglas$nombre[[i]]
    if (.identificadores_en(nombre, v)) next
    parecido <- v[plegar(v) %in% plegar(nombre)]
    if (length(parecido)) {
      .detener(
        "La regla de `cortes` `", nombre, "` (por ", reglas$por[[i]], ") no ",
        "coincide con ning\u00fan nombre de la medici\u00f3n, pero se parece a `",
        parecido[[1L]], "`.", .pista_forma_distinta(nombre, parecido),
        " Escribirla igual que en la medici\u00f3n.", call. = FALSE
      )
    }
  }
  invisible(NULL)
}

.conteo_niveles_semaforo <- function(celdas, niveles, cobertura, fuera) {
  dim_de <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "(sin dimensi\u00f3n)"
    x
  }
  dims <- unique(c(dim_de(celdas$dimension),
                   if (inherits(cobertura, "data.frame") && nrow(cobertura) &&
                       "dimension" %in% names(cobertura)) dim_de(cobertura$dimension)))
  if (!length(dims)) {
    return(data.frame(dimension = character(), stringsAsFactors = FALSE))
  }
  d_celda <- dim_de(celdas$dimension)
  salida <- data.frame(dimension = dims, stringsAsFactors = FALSE)
  for (nivel in niveles) {
    salida[[nivel]] <- vapply(dims, function(d) {
      sum(d_celda == d & !is.na(celdas$nivel) & celdas$nivel == nivel)
    }, integer(1L))
  }
  salida$sin_color <- vapply(dims, function(d) {
    sum(d_celda == d & is.na(celdas$nivel))
  }, integer(1L))
  salida$sin_medir <- vapply(dims, function(d) {
    if (!inherits(cobertura, "data.frame") || !nrow(cobertura) ||
        !"dimension" %in% names(cobertura)) return(0L)
    sum(dim_de(cobertura$dimension) == d & cobertura$estado != "no_resumible")
  }, integer(1L))
  salida$fuera_del_semaforo <- vapply(dims, function(d) {
    if (!inherits(cobertura, "data.frame") || !nrow(cobertura) ||
        !"dimension" %in% names(cobertura)) return(0L)
    sum(dim_de(cobertura$dimension) == d & cobertura$estado == "no_resumible")
  }, integer(1L))
  rownames(salida) <- NULL
  salida
}

#' @export
`[.semaforo_calidad` <- function(x, ...) {
  resultado <- NextMethod("[")
  if (inherits(resultado, "data.frame")) {
    for (a in setdiff(names(attributes(x)), c("names", "row.names", "class"))) {
      if (is.null(attr(resultado, a, exact = TRUE))) {
        attr(resultado, a) <- attr(x, a, exact = TRUE)
      }
    }
  }
  resultado
}

#' @export
print.semaforo_calidad <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Sem\u00e1foro de calidad")
  tabla <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  cols <- intersect(c(
    "dimension", "metrica_instanciada", "objeto", "valor", "escala_comparada",
    "regla", "nivel", "motivo_sin_color", "control_lecturas", "fuente"
  ), names(tabla))
  .print_data_frame_bytes(tabla[cols], row.names = FALSE)
  conteo <- attr(x, "conteo_niveles", exact = TRUE)
  if (inherits(conteo, "data.frame") && nrow(conteo)) {
    cli::cli_h2("Por dimensi\u00f3n")
    .print_data_frame_bytes(conteo, row.names = FALSE)
  }
  sin <- tabla[!is.na(tabla$motivo_sin_color), , drop = FALSE]
  if (nrow(sin)) {
    cli::cli_h2("Sin color")
    motivos <- table(sin$motivo_sin_color)
    cli::cli_alert_info(.cli_literal(paste0(
      paste0(names(motivos), ": ", as.integer(motivos), collapse = "; "), "."
    )))
  }
  limitadas <- !is.na(tabla$nivel) & tabla$control_lecturas %in% "limitado"
  if (any(limitadas)) {
    cli::cli_alert_warning(.cli_literal(paste0(
      sum(limitadas),
      if (sum(limitadas) == 1L) " celda con color tiene" else " celdas con color tienen",
      " un control de lecturas limitado (`control_lecturas`): su tabla se ley\u00f3 en ",
      "varias consultas sin un contador de modificaciones del motor, y s\u00f3lo se ",
      "compar\u00f3 el n\u00famero de filas. Una fila agregada o quitada entre lecturas ",
      "se ve; un `UPDATE` que no cambia ese n\u00famero, no."
    )))
  }
  desconocidas <- !is.na(tabla$nivel) & tabla$control_lecturas %in% "desconocido"
  if (any(desconocidas)) {
    cli::cli_alert_warning(.cli_literal(paste0(
      sum(desconocidas),
      if (sum(desconocidas) == 1L) " celda con color tiene" else " celdas con color tienen",
      " un control de lecturas desconocido (`control_lecturas`): no se puede ",
      "saber qu\u00e9 lecturas respaldan la cifra. Pasa si el resumen no conserva ",
      "`id_medicion`, `entidad` o `entidad_referida`; si el motor no resolvi\u00f3 ",
      "la tabla de alguna lectura (fuera de SQLite y DuckDB, o en un resumen ",
      "medido con una versi\u00f3n anterior); si el nombre de la entidad da m\u00e1s ",
      "de una tabla en las partes unidas (una parte en memoria y otra en el ",
      "motor, u otra base adjunta con el mismo alias); o si un referencial en R ",
      "comparte su nombre con otro de las mismas filas y otros datos, o con una ",
      "tabla de otra parte. `attr(x, \"lecturas\")` muestra las de cada nombre."
    )))
  }
  sin_celdas <- attr(x, "cortes_sin_celdas", exact = TRUE)
  if (inherits(sin_celdas, "data.frame") && nrow(sin_celdas)) {
    cli::cli_alert_info(.cli_literal(paste0(
      "Reglas de corte sin celdas en esta medici\u00f3n: ",
      paste(paste0(sin_celdas$por, ":", sin_celdas$nombre), collapse = ", "), "."
    )))
  }
  fuera <- attr(x, "fuera_del_semaforo", exact = TRUE)
  if (inherits(fuera, "data.frame") && nrow(fuera)) {
    cli::cli_h2("Fuera del sem\u00e1foro")
    .print_data_frame_bytes(fuera[c("metrica_instanciada", "motivo")], row.names = FALSE)
  }
  .imprimir_coberturas_frontera(x)
  .imprimir_cuentas_y_consolidacion(x)
  invisible(original)
}
