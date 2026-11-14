.plan_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    grupo = character(), decision_grupo = character(),
    recomendacion_grupo = character(),
    estrategia = character(), recomendada = logical(),
    severidad_origen = character(), evidencia = character(),
    justificacion = character(), n_afectadas = numeric(),
    unidad_conteo = character(),
    reversible = logical(), destructiva = logical(),
    estado = character(), estado_reparacion = character(), aplicar = logical(),
    orden = integer(), stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

.nueva_accion <- function(columna, hallazgo, estrategia, recomendada,
                          justificacion, n_afectadas, reversible,
                          estado = "lista", aplicar = recomendada,
                          parametros = list(), orden = 500L,
                          grupo = NA_character_,
                          decision_grupo = NA_character_,
                          recomendacion_grupo = NA_character_,
                          destructiva = FALSE,
                          estado_reparacion = NA_character_,
                          unidad_conteo = NA_character_) {
  estructura <- .data_frame_simple(
    id_accion = "", columna = columna, hallazgo = hallazgo,
    grupo = grupo, decision_grupo = decision_grupo,
    recomendacion_grupo = recomendacion_grupo,
    estrategia = estrategia, recomendada = recomendada,
    severidad_origen = NA_character_, evidencia = "",
    justificacion = justificacion,
    n_afectadas = as.numeric(n_afectadas),
    unidad_conteo = as.character(unidad_conteo), reversible = reversible,
    destructiva = destructiva,
    estado = estado, estado_reparacion = estado_reparacion,
    aplicar = aplicar, orden = as.integer(orden),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list(parametros))
  estructura
}

.id_grupo <- function(indice) sprintf("grupo-%04d", indice)

# El numero que se publica tiene que ser el de la UNIDAD que se publica. Una
# accion que no declara `unidad_conteo` hereda la del hallazgo que la origina,
# y entonces su `n_afectadas` es el alcance que midio ese hallazgo -no las
# filas de la columna-. Estaba escrito una vez bien -las acciones informativas,
# donde `alta_cardinalidad` publicaba 80 "columnas" sobre 1- y otra mal:
# `desambiguar_fecha_en_origen` publicaba `n_afectadas = 40` con la unidad
# "formato" de un hallazgo que declara 2 formatos. La regla es una.
.n_afectadas_del_hallazgo <- function(hallazgo, fila) {
  if (!is.null(hallazgo) && "n_afectados" %in% names(hallazgo)) {
    return(suppressWarnings(as.numeric(hallazgo$n_afectados[[1L]])))
  }
  if (is.null(fila)) NA_real_ else fila$n[[1L]]
}

# Los valores presentes de la COLUMNA ENTERA, contados con las cifras del
# perfil. Una conversion de tipo cambia la columna entera -una columna tiene un
# solo tipo-, asi que su alcance son todos los presentes: los del universo
# aplicable, que el perfil cuenta como `n_aplicables - n_faltantes`, y los que
# publica aparte, fuera del universo y donde el predicado no se pudo decidir.
# Es la misma suma con que la corroboracion por DBI separa lo de fuera.
#
# Se contaba `n - n_faltantes`, y con un universo declarado eso no cuenta
# presentes: `n_faltantes` solo cuenta las ausencias DENTRO del universo, asi
# que las celdas vacias de fuera -vacias por diseno- quedaban como presentes.
# Con `aplicabilidad = list(x = ~ g == "si")`, 30 vacias fuera y 60 presentes
# dentro, `convertir_numero_regional` prometia 90 y `aplicar()` cambiaba 60. Con
# un solo presente fuera prometia 61, lo cierto, porque ese caso se recontaba
# en otro lugar con la regla del perfil: dos caminos que contaban distinto.
# Una columna de `columnas_opcionales` tenia lo mismo, porque su universo son
# las celdas con valor.
#
# Un perfil sin estas cifras -de una version anterior- conserva la cuenta de
# antes, la unica que se puede hacer con lo que trae.
.n_presentes_columna <- function(fila) {
  partes <- c(
    "n_aplicables", "n_faltantes", "n_presentes_fuera_de_aplicabilidad",
    "n_presentes_en_aplicabilidad_indeterminada"
  )
  valores <- if (all(partes %in% names(fila))) {
    vapply(partes, function(parte) as.numeric(fila[[parte]][[1L]]), numeric(1L))
  }
  if (is.null(valores) || anyNA(valores)) {
    return(fila$n[[1L]] - fila$n_faltantes[[1L]])
  }
  valores[["n_aplicables"]] - valores[["n_faltantes"]] +
    valores[["n_presentes_fuera_de_aplicabilidad"]] +
    valores[["n_presentes_en_aplicabilidad_indeterminada"]]
}

.nombres_snake <- function(nombres) {
  originales <- as.character(nombres)
  originales[is.na(originales)] <- ""
  transliterados <- .transliterar_ascii(.nombres_para_operar(originales))
  # Escribe el nombre: el mapa del paquete sin plegar la sigma final.
  salida <- .normalizacion_minusculas_vector(trimws(transliterados))
  salida <- gsub("[^[:alnum:]]+", "_", salida, perl = TRUE)
  salida <- gsub("^_+|_+$", "", salida, perl = TRUE)
  salida[!nzchar(salida)] <- "x"
  salida[grepl("^[0-9]", salida)] <- paste0(
    "x_", salida[grepl("^[0-9]", salida)]
  )
  .nombres_unicos(salida, sep = "_")
}

.fila_perfil <- function(perfil, columna) {
  indices <- which(.nombres_para_operar(perfil$columnas$columna) %in%
                    .nombres_para_operar(columna))
  if (length(indices) == 1L) .fila_de_tabla(perfil$columnas, indices) else NULL
}

# `tabla[i, , drop = FALSE]` para una sola fila. `[.data.frame` pasa por
# `[[.data.frame` en cada columna, y la tabla de columnas del perfil tiene mas de
# cien: medido, 0,49 ms por fila contra 0,04, y `planificar_limpieza()` la pide
# unas treinta veces sobre la tabla de ejemplo de diez columnas. La via corta
# solo toma lo que arma igual que `[`: un `data.frame` sin subclase ni otros
# atributos (en cualquier orden: `[` los devuelve siempre como `names`,
# `row.names`, `class`), con nombres de fila automaticos, nombres de columna sin
# repetir y columnas atomicas sin atributos. Lo demas sigue por `[`.
.fila_de_tabla <- function(tabla, i) {
  if (!.fila_por_via_corta(tabla, i)) return(tabla[i, , drop = FALSE])
  fila <- lapply(unclass(tabla), `[`, i)
  attributes(fila) <- list(names = names(tabla), row.names = i,
                           class = "data.frame")
  fila
}

.fila_por_via_corta <- function(tabla, i) {
  if (!is.integer(i) || length(i) != 1L || is.na(i) || i < 1L) return(FALSE)
  if (!identical(class(tabla), "data.frame")) return(FALSE)
  atributos <- names(attributes(tabla))
  length(atributos) == 3L &&
    all(c("names", "row.names", "class") %in% atributos) &&
    i <= .row_names_info(tabla, 2L) && .row_names_info(tabla) < 0L &&
    !anyDuplicated(names(tabla)) &&
    all(vapply(tabla, function(columna) {
      is.atomic(columna) && is.null(attributes(columna))
    }, logical(1L)))
}

.formatos_perfil <- function(perfil, columna) {
  indices <- which(.nombres_para_operar(perfil$columnas$columna) %in%
                    .nombres_para_operar(columna))
  if (length(indices) == 1L) perfil$formatos_fecha[[indices]] else NULL
}

.accion_columna_ambigua <- function(perfil, columna) {
  sum(.nombres_para_operar(perfil$columnas$columna) %in%
        .nombres_para_operar(columna)) != 1L
}

.estado_columna <- function(perfil, columna, estado = "lista") {
  if (.accion_columna_ambigua(perfil, columna)) "bloqueada" else estado
}

# Las columnas de una tabla que son marcas que agrega el propio plan:
# `.fila_duplicada`, `.grupo_duplicado`, y `.ausente_<x>` y `.outlier_<x>` de una
# columna `x` de la misma tabla. Volver a planificar sobre lo aplicado -el idioma
# documentado `plan$aplicar <- plan$recomendada`, una y otra vez- trataba la
# marca como dato: recomendaba `marcar_filas_ausentes` sobre `.grupo_duplicado`,
# cuyo `NA` significa "no participa de ningun grupo", y publicaba cinco filas
# "ausentes" que no lo eran; proponia ademas borrarla por constante. Medido.
.marcas_del_plan <- function(nombres) {
  nombres <- as.character(nombres)
  if (!length(nombres)) return(logical())
  base <- .nombres_make_names(nombres)
  candidatas <- c(
    ".fila_duplicada", ".grupo_duplicado",
    paste0(".ausente_", base), paste0(".outlier_", base)
  )
  !is.na(nombres) &
    .nombres_para_operar(nombres) %in% .nombres_para_operar(candidatas)
}

# Una accion que agrega una marca cuya columna YA esta en la tabla no puede
# ejecutarse: `.agregar_marca()` aborta. El plan la recomendaba y activaba igual
# cada vez, y `aplicar()` la registraba `fallida` -"La columna de marca ya
# existe"- sin que el ciclo llegara nunca a un plan sin fallas. Medido. Se
# bloquea y se dice por que, la misma pregunta que hace el ejecutor.
.bloquear_marcas_existentes <- function(plan, nombres_tabla) {
  if (!nrow(plan) || !length(nombres_tabla)) return(plan)
  existentes <- .nombres_para_operar(as.character(nombres_tabla))
  for (j in seq_len(nrow(plan))) {
    parametros <- plan$parametros[[j]]
    if (!is.list(parametros)) next
    marcas <- unlist(
      parametros[intersect(names(parametros), .parametros_nombre_de_columna)],
      use.names = FALSE
    )
    marcas <- as.character(marcas)
    repetidas <- marcas[!is.na(marcas) &
                          .nombres_para_operar(marcas) %in% existentes]
    if (!length(repetidas)) next
    plan$estado[[j]] <- "bloqueada"
    plan$aplicar[[j]] <- FALSE
    plan$recomendada[[j]] <- FALSE
    plan$justificacion[[j]] <- paste0(
      plan$justificacion[[j]], " La columna de marca ",
      paste0("`", repetidas, "`", collapse = ", "),
      " ya existe en los datos -la agreg\u00f3 un plan anterior- y la acci\u00f3n ",
      "no puede volver a crearla. Para marcar de nuevo, quitarla antes."
    )
  }
  plan
}

# Por que se bloquean las acciones de un grupo de columnas iguales que no se
# puede comprobar entero. Vacio si el grupo esta completo.
.motivo_grupo_duplicadas_incompleto <- function(miembros, faltan_perfil) {
  nombrar <- function(columnas) paste0("`", columnas, "`", collapse = ", ")
  donde <- function(columnas, lugar) {
    paste0(
      if (length(columnas) == 1L) " La columna " else " Las columnas ",
      nombrar(columnas), " del grupo no ",
      if (length(columnas) == 1L) "est\u00e1" else "est\u00e1n", " en ", lugar
    )
  }
  if (is.null(miembros)) {
    return(paste0(
      " El perfil no guarda el grupo de columnas iguales de este hallazgo, y ",
      "la acci\u00f3n no sabe sobre qu\u00e9 columnas actuar\u00eda: volver a ",
      "perfilar los datos que se van a limpiar."
    ))
  }
  if (length(faltan_perfil)) {
    return(paste0(
      donde(faltan_perfil, "el perfil"), ": el hallazgo no corresponde a las ",
      "columnas que el perfil describe, y la acci\u00f3n queda bloqueada. ",
      "Volver a perfilar los datos que se van a limpiar."
    ))
  }
  ""
}

# **Una accion sobre una columna que no esta en los datos no sale `lista`.**
# Con un perfil de otra version de la tabla -una columna renombrada o
# quitada-, el plan proponia y activaba acciones sobre ella, y `aplicar()` las
# registraba `fallida` ("requiere una unica columna llamada 'x'; se
# encontraron 0"). El plan tenia los datos y podia saberlo, como sabe que una
# columna de marca ya existe (`.bloquear_marcas_existentes()`). Es la misma
# regla para toda accion: la columna de la accion y las que nombran sus
# parametros -los miembros de un grupo de columnas iguales, la clave, el
# determinante y el dependiente-. Una informativa no se ejecuta y queda como
# esta. Sin datos no se sabe, y no se afirma.
.parametros_columna_existente <- c(
  "columna_1", "columna_2", "eliminar", "clave", "determinante", "dependiente"
)

.bloquear_columnas_ausentes <- function(plan, datos) {
  if (is.null(datos) || !nrow(plan)) return(plan)
  existentes <- .nombres_para_operar(names(datos))
  for (j in seq_len(nrow(plan))) {
    if (identical(as.character(plan$estado[[j]]), "informativa")) next
    parametros <- plan$parametros[[j]]
    referidas <- c(
      as.character(plan$columna[[j]]),
      if (is.list(parametros)) {
        as.character(unlist(
          parametros[intersect(names(parametros), .parametros_columna_existente)],
          use.names = FALSE
        ))
      }
    )
    # Tambien el nombre vacio: una columna `""` del perfil que no esta en los
    # datos dejaba `lista` las acciones de su grupo de columnas iguales.
    referidas <- .identificadores_unicos(referidas[!is.na(referidas)])
    faltan <- referidas[!.nombres_para_operar(referidas) %in% existentes]
    # Una accion de nombres renombra por posicion los nombres que el perfil
    # vio: con otros nombres en los datos no se puede aplicar, y el plan lo
    # sabe.
    if (!length(faltan) &&
        !is.null(.motivo_nombres_distintos(plan[j, , drop = FALSE], names(datos)))) {
      plan$estado[[j]] <- "bloqueada"
      plan$aplicar[[j]] <- FALSE
      plan$recomendada[[j]] <- FALSE
      plan$justificacion[[j]] <- paste0(
        plan$justificacion[[j]],
        " Los nombres de los datos no son los que el perfil vio: el perfil es de ",
        "otra versi\u00f3n de la tabla, y la acci\u00f3n queda bloqueada. Volver a ",
        "perfilar los datos que se van a limpiar."
      )
      next
    }
    if (!length(faltan)) next
    plan$estado[[j]] <- "bloqueada"
    plan$aplicar[[j]] <- FALSE
    plan$recomendada[[j]] <- FALSE
    plan$justificacion[[j]] <- paste0(
      plan$justificacion[[j]],
      if (length(faltan) == 1L) " La columna " else " Las columnas ",
      paste0("`", faltan, "`", collapse = ", "),
      if (length(faltan) == 1L) " no est\u00e1" else " no est\u00e1n",
      " en los datos: el perfil es de otra versi\u00f3n de la tabla, y la ",
      "acci\u00f3n queda bloqueada. Volver a perfilar los datos que se van a ",
      "limpiar."
    )
  }
  plan
}

# Si algun miembro de un grupo de columnas iguales se llama como otra columna
# de la tabla: entonces ninguna accion sobre el grupo puede decir sobre cual
# actua. `claves_columnas` son los nombres del perfil ya normalizados.
.grupo_con_nombre_repetido <- function(miembros, claves_columnas) {
  if (!length(miembros)) return(FALSE)
  any(tabulate(
    match(claves_columnas, .nombres_para_operar(miembros)), length(miembros)
  ) > 1L)
}

.hallazgos_sin_accion_por_columna_ambigua <- function(perfil, grupos = NULL) {
  # El plan se arma desde `perfil$hallazgos`, y una columna cuyo nombre aparece
  # mas de una vez no se puede identificar: `.fila_perfil()` devuelve NULL y
  # NINGUNA rama del planificador llega a proponer su accion. Medido sobre una
  # tabla con dos columnas llamadas igual y espacios al borde en las dos: el
  # perfil publica los dos hallazgos `espacios_sobrantes`, el plan publica solo
  # las dos estrategias de nombres, y la cobertura sale vacia. Con los nombres
  # distintos, la misma tabla produce las dos acciones de recorte.
  #
  # Es el reverso exacto de lo que ya esta escrito donde la cobertura se cuelga
  # del plan: ahi el silencio era un diagnostico que no se evaluo, aca es un
  # hallazgo medido que no encontro accion. Se declara en la misma puerta donde
  # se decide que limpiar, porque leer el plan y no ver la accion se lee como
  # "no hay nada que hacer".
  hallazgos <- perfil$hallazgos
  vacio <- data.frame(
    hallazgo = character(), columna = character(), motivo = character(),
    como_resolverlo = character(), stringsAsFactors = FALSE
  )
  if (!inherits(hallazgos, "data.frame") || !nrow(hallazgos)) return(vacio)
  # La misma pregunta que `.accion_columna_ambigua()` -cuantas columnas del
  # perfil tienen ese nombre-, con los nombres normalizados y contados una sola
  # vez. Preguntarla por hallazgo normalizaba todos los nombres del perfil en
  # cada uno: con un perfil guardado por pares de 1.000 columnas iguales
  # (499.500 hallazgos) eran 81 de los 87 s de `planificar_limpieza()`.
  # `.nombres_para_operar()` es elemento a elemento, asi que normalizar el
  # vector entero da las mismas claves que normalizar cada nombre.
  # Un hallazgo `ok` no espera accion, como en `.hallazgos_sin_accion_declarados()`.
  # Uno con `columna` `NA` es de la tabla, salvo que su tipo sea de una columna:
  # entonces es de una columna sin nombre (ver `.TIPOS_HALLAZGO_DE_LA_TABLA`).
  sin_nombre <- which(is.na(hallazgos$columna) &
                        as.character(hallazgos$severidad) != "ok" &
                        !as.character(hallazgos$tipo_hallazgo) %in%
                          .TIPOS_HALLAZGO_DE_LA_TABLA)
  candidatos <- which(!is.na(hallazgos$columna) &
                        as.character(hallazgos$severidad) != "ok")
  nombres <- .nombres_para_operar(perfil$columnas$columna)
  distintos <- unique(nombres)
  conteo <- tabulate(match(nombres, distintos), nbins = length(distintos))
  veces <- conteo[
    match(.nombres_para_operar(hallazgos$columna[candidatos]), distintos)
  ]
  veces[is.na(veces)] <- 0L
  # **Un hallazgo de varias columnas no es una columna ambigua.** La regla es la
  # de `.accion_columna_ambigua()` -la columna no se identifica si su nombre no
  # aparece exactamente una vez-, pero un hallazgo de varias columnas
  # -`relacion_aritmetica_columnas` en `neto,iva,total`,
  # `relacion_orden_columnas` en `inicio,fin`- no nombra UNA columna: nombra
  # varias del perfil, y caia aca con el motivo "el nombre de columna se repite
  # en la tabla", falso; tambien los `ok`. Uno asi es ambiguo solo si alguna de
  # SUS columnas se repite o no esta; si no, va a `hallazgos_sin_accion` con su
  # razon. Y el motivo dice cual de los dos casos es: repetida, o ausente del
  # perfil.
  ambiguo <- veces != 1L
  ausente <- veces == 0L
  # La trazabilidad manda sobre el texto: una relacion entre `a` y `b` se
  # publica con `columna = "a,b"`, y si la tabla tiene ademas una columna
  # llamada literalmente `a,b`, el texto la nombraba a ella -"sobre la misma
  # columna si propone recortar_espacios"- o, con dos asi, la declaraba
  # repetida.
  traza <- if ("trazabilidad" %in% names(hallazgos)) hallazgos$trazabilidad[candidatos]
  de_varias <- vapply(seq_along(candidatos), function(j) {
    t <- if (is.null(traza)) NULL else traza[[j]]
    is.list(t) && length(t$columnas) > 1L
  }, logical(1L))
  # Un hallazgo de varias columnas con una sin nombre (`NA` en su
  # trazabilidad) tampoco se puede identificar: el plan no puede nombrarla.
  falta_nombre <- logical(length(candidatos))
  for (j in which(veces == 0L | de_varias)) {
    i <- candidatos[[j]]
    miembros <- .miembros_hallazgo_varias_columnas(
      hallazgos$columna[[i]],
      if (!is.null(traza)) traza[[j]],
      perfil$columnas$columna
    )
    if (length(miembros) > 1L && anyNA(miembros)) {
      ambiguo[[j]] <- TRUE
      falta_nombre[[j]] <- TRUE
    } else if (length(miembros) > 1L) {
      repeticiones <- conteo[match(.nombres_para_operar(miembros), distintos)]
      repeticiones[is.na(repeticiones)] <- 0L
      ambiguo[[j]] <- any(repeticiones != 1L)
      ausente[[j]] <- any(repeticiones == 0L) && !any(repeticiones > 1L)
    }
  }
  motivo_sin_nombre <- paste0(
    "La columna del hallazgo no tiene nombre, as\u00ed que el plan no puede ",
    "nombrarla para actuar sobre ella: no propone acci\u00f3n para este hallazgo."
  )
  # Un grupo de columnas iguales puede nombrarse por una columna unica y tener
  # otra repetida (`Y, X, X`): el plan tampoco propone su accion (ver
  # `.grupo_con_nombre_repetido()`), y el motivo es el mismo.
  duplicadas <- as.character(hallazgos$tipo_hallazgo[candidatos]) ==
    "columnas_duplicadas"
  if (any(duplicadas & !ambiguo) && is.list(grupos) &&
      length(grupos$grupos)) {
    repetido <- vapply(grupos$grupos, .grupo_con_nombre_repetido, logical(1L),
                       claves_columnas = nombres)
    g <- grupos$grupo[match(
      .nombres_para_operar(hallazgos$columna[candidatos]), grupos$claves
    )]
    ambiguo <- ambiguo | (duplicadas & !is.na(g) & repetido[g] %in% TRUE)
  }
  # Por fila del perfil, para que la lista salga en su orden, como los demas
  # atributos del plan.
  razon <- rep(NA_character_, nrow(hallazgos))
  razon[candidatos[ambiguo]] <- ifelse(
    falta_nombre[ambiguo], "sin_nombre_varias",
    ifelse(ausente[ambiguo], "ausente", "repetida")
  )
  razon[sin_nombre] <- "sin_nombre"
  ambiguos <- which(!is.na(razon))
  if (!length(ambiguos)) return(vacio)
  razon <- razon[ambiguos]
  motivos <- c(
    ausente = paste0(
      "La columna del hallazgo no est\u00e1 entre las del perfil: el hallazgo no ",
      "corresponde a las columnas que el perfil describe, y el plan no propone ",
      "acci\u00f3n para \u00e9l."
    ),
    repetida = paste0(
      "El nombre de columna se repite en la tabla, as\u00ed que no hay forma de ",
      "saber sobre cu\u00e1l de ellas act\u00faa la limpieza: el plan no propone acci\u00f3n ",
      "para este hallazgo."
    ),
    sin_nombre = motivo_sin_nombre,
    sin_nombre_varias = paste0(
      "Una de las columnas del hallazgo no tiene nombre, as\u00ed que el plan ",
      "no puede nombrarla para actuar sobre ella: no propone acci\u00f3n para ",
      "este hallazgo."
    )
  )
  data.frame(
    hallazgo = as.character(hallazgos$tipo_hallazgo[ambiguos]),
    columna = as.character(hallazgos$columna[ambiguos]),
    motivo = unname(motivos[razon]),
    como_resolverlo = paste0(
      "Aplicar `normalizar_nombres` -o renombrar las columnas a mano- y volver ",
      "a perfilar: con nombres distintos el plan propone las acciones de este ",
      "hallazgo."
    ),
    stringsAsFactors = FALSE
  )
}

# Las columnas que nombra un hallazgo de varias, cuando su `columna` no es una
# columna del perfil: las de su trazabilidad, que las guarda como lista, o la
# `columna` partida por los nombres del perfil (`a,b,c`), con el mismo lector
# que la deriva. Solo si son dos o mas; si no, ninguna. No normaliza la
# `columna` -quien llama ya la comparo- y solo parte un texto con coma: con
# 499.500 hallazgos de un perfil viejo, partir cada uno contra todos los
# nombres era otra vez cuadratico.
.miembros_hallazgo_varias_columnas <- function(columna, trazabilidad,
                                               nombres_perfil) {
  columna <- as.character(columna)
  if (length(columna) != 1L || is.na(columna)) return(character())
  miembros <- if (is.list(trazabilidad) && length(trazabilidad$columnas)) {
    as.character(trazabilidad$columnas)
  } else if (grepl(",", columna, fixed = TRUE, useBytes = TRUE)) {
    as.character(.separar_nombres_columnas(columna, nombres_perfil))
  } else character()
  if (length(miembros) > 1L) miembros else character()
}

# Para el motivo de `hallazgos_sin_accion`: la columna del hallazgo si es una
# del perfil, sus columnas si es de varias, ninguna si es de la tabla.
.columnas_de_hallazgo_plan <- function(hallazgos, i, nombres_perfil,
                                       claves_perfil) {
  columna <- as.character(hallazgos$columna[[i]])
  if (is.na(columna)) return(character())
  traza <- if ("trazabilidad" %in% names(hallazgos)) hallazgos$trazabilidad[[i]]
  # La trazabilidad primero: ver `.hallazgos_sin_accion_por_columna_ambigua()`.
  if (is.list(traza) && length(traza$columnas) > 1L) {
    return(as.character(traza$columnas))
  }
  if (.nombres_para_operar(columna) %in% claves_perfil) return(columna)
  miembros <- .miembros_hallazgo_varias_columnas(columna, traza, nombres_perfil)
  if (length(miembros)) miembros else columna
}

# Los tipos de hallazgo de la tabla y no de una columna: los unicos que se
# publican con `columna = NA` (`.nuevo_hallazgo(NA_character_, ...)` en
# `R/hallazgos.R`; una prueba recorre el codigo y los compara con esta lista).
# Cualquier otro con `columna` `NA` es de una columna sin nombre.
.TIPOS_HALLAZGO_DE_LA_TABLA <- c(
  "clave_con_ausentes", "clave_no_unica", "filas_duplicadas",
  "nombres_columnas_problematicos", "fecha_partida_columnas"
)

.hallazgos_sin_accion_declarados <- function(perfil, plan, ambiguos) {
  # El plan promete declarar lo que no cubre, y declaraba dos huecos: el
  # diagnostico que no se evaluo y el hallazgo cuya columna es ambigua. Falta el
  # tercero, que es el mas silencioso: el hallazgo que el perfil SI midio, con
  # su columna identificada, y que ninguna rama del planificador convierte en
  # accion. Medido sobre cuatro tablas -las dos del paquete y dos fixtures-:
  # `patron_raro`, `casi_duplicados_vocabulario` y `relacion_orden_columnas` no
  # tienen rama, y `faltantes` y `tipo_declarado_distinto` tienen una que puede
  # no proponer nada. En todos los casos el plan se leia como "no hay nada que
  # hacer ahi".
  #
  # El motivo no adivina la causa: mide si la misma columna recibio otras
  # acciones y lo dice, para que quien lee decida si eso cubre el hallazgo.
  vacio <- data.frame(
    hallazgo = character(), columna = character(), severidad = character(),
    motivo = character(), como_resolverlo = character(),
    stringsAsFactors = FALSE
  )
  hallazgos <- perfil$hallazgos
  if (!inherits(hallazgos, "data.frame") || !nrow(hallazgos)) return(vacio)
  # Un hallazgo `ok` no espera accion: describe una propiedad medida, no un
  # problema.
  candidatos <- which(as.character(hallazgos$severidad) != "ok")
  # Un perfil de una version anterior trae un hallazgo `columnas_duplicadas`
  # por par, y el plan cubre el grupo con las acciones del primero: los demas
  # pares del grupo estan cubiertos, aunque nombren otra columna.
  duplicadas_plan <- as.character(plan$hallazgo) == "columnas_duplicadas"
  duplicadas_h <- candidatos[
    as.character(hallazgos$tipo_hallazgo[candidatos]) == "columnas_duplicadas"
  ]
  if (any(duplicadas_plan) && length(duplicadas_h)) {
    grupos <- .grupos_columnas_duplicadas_perfil(perfil)
    if (length(grupos$grupos)) {
      grupo_de <- function(columnas) {
        grupos$grupo[match(.nombres_para_operar(as.character(columnas)), grupos$claves)]
      }
      planificados <- grupo_de(plan$columna[duplicadas_plan])
      cubiertos <- duplicadas_h[grupo_de(hallazgos$columna[duplicadas_h]) %in%
                                  planificados[!is.na(planificados)]]
      candidatos <- setdiff(candidatos, cubiertos)
    }
  }
  if (!length(candidatos)) return(vacio)
  claves_plan <- .clave_par_identificador(
    as.character(plan$columna), as.character(plan$hallazgo)
  )
  claves <- .clave_par_identificador(
    as.character(hallazgos$columna[candidatos]),
    as.character(hallazgos$tipo_hallazgo[candidatos])
  )
  sin_accion <- candidatos[!.identificadores_en(claves, claves_plan)]
  if (length(sin_accion) && inherits(ambiguos, "data.frame") &&
      nrow(ambiguos)) {
    ya_declarados <- .clave_par_identificador(
      as.character(ambiguos$columna), as.character(ambiguos$hallazgo)
    )
    claves_sin <- .clave_par_identificador(
      as.character(hallazgos$columna[sin_accion]),
      as.character(hallazgos$tipo_hallazgo[sin_accion])
    )
    sin_accion <- sin_accion[!.identificadores_en(claves_sin, ya_declarados)]
  }
  if (!length(sin_accion)) return(vacio)
  marcas_plan <- as.character(perfil$columnas$columna)
  marcas_plan <- marcas_plan[.marcas_del_plan(marcas_plan)]
  claves_perfil <- .nombres_para_operar(as.character(perfil$columnas$columna))
  motivos <- vapply(sin_accion, function(i) {
    columna <- as.character(hallazgos$columna[[i]])
    if (!is.na(columna) &&
        .nombres_para_operar(columna) %in% .nombres_para_operar(marcas_plan)) {
      return(paste0(
        "La columna es una marca que agrega el propio plan de limpieza, no un ",
        "dato que limpiar: el plan no propone acciones sobre ella. En ",
        "`.grupo_duplicado`, `NA` significa que la fila no participa de ",
        "ning\u00fan grupo."
      ))
    }
    # Un hallazgo de la tabla (`NA`) o de varias columnas no tiene "la misma
    # columna": el motivo decia "ni para el resto de esta columna" sobre una
    # clave compuesta o una fecha partida en tres columnas.
    miembros <- .columnas_de_hallazgo_plan(hallazgos, i, perfil$columnas$columna,
                                           claves_perfil)
    if (length(miembros) != 1L) {
      estrategias <- if (!length(miembros)) character() else {
        unicas <- .identificadores_unicos(as.character(
          plan$estrategia[.identificadores_en(as.character(plan$columna), miembros)]
        ))
        unicas[!is.na(unicas)]
      }
      return(paste0(
        if (length(miembros)) {
          paste0(
            "El hallazgo es de varias columnas (",
            paste0("`", miembros, "`", collapse = ", "), ")"
          )
        } else "El hallazgo es de la tabla y no de una columna",
        ", y el plan no tiene una estrategia para este tipo de hallazgo: no ",
        "propone acci\u00f3n.",
        if (length(estrategias)) {
          paste0(
            " Sobre esas columnas s\u00ed propone: ", paste(estrategias, collapse = ", "),
            "; si alguna lo cubre, es una decisi\u00f3n del dominio, no una ",
            "correspondencia que el paquete haya medido."
          )
        } else ""
      ))
    }
    estrategias <- {
      unicas <- .identificadores_unicos(as.character(
        plan$estrategia[.identificadores_en(as.character(plan$columna), columna)]
      ))
      unicas[!is.na(unicas)]
    }
    if (length(estrategias)) {
      paste0(
        "El plan no propone una acci\u00f3n para este hallazgo. Sobre la misma ",
        "columna s\u00ed propone: ", paste(estrategias, collapse = ", "),
        "; si alguna lo cubre, es una decisi\u00f3n del dominio, no una ",
        "correspondencia que el paquete haya medido."
      )
    } else {
      paste0(
        "El plan no propone ninguna acci\u00f3n para este hallazgo ni para el ",
        "resto de esta columna."
      )
    }
  }, character(1L))
  .data_frame_simple(
    hallazgo = as.character(hallazgos$tipo_hallazgo[sin_accion]),
    columna = as.character(hallazgos$columna[sin_accion]),
    severidad = as.character(hallazgos$severidad[sin_accion]),
    motivo = motivos,
    como_resolverlo = as.character(hallazgos$sugerencia[sin_accion]),
    stringsAsFactors = FALSE
  )
}

.es_fecha_ambigua <- function(perfil, columna) {
  any(
    .nombres_para_operar(perfil$hallazgos$columna) %in%
      .nombres_para_operar(columna) &
      perfil$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo",
    na.rm = TRUE
  )
}

.es_fecha_mixta <- function(perfil, columna) {
  any(
    .nombres_para_operar(perfil$hallazgos$columna) %in%
      .nombres_para_operar(columna) &
      perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos",
    na.rm = TRUE
  )
}

.agregar_accion <- function(acciones, accion) {
  acciones[[length(acciones) + 1L]] <- accion
  acciones
}

# Si la conversion regional no se puede ejecutar sobre los datos con NINGUNA
# configuracion. El plan la prueba con sus parametros, y una falla puede ser
# solo una ambiguedad que el usuario resuelve -`punto_sin_coma`,
# `coma_sin_punto`-: esa accion sigue `lista`, se configura y se ejecuta. Pero
# si falla con todas las configuraciones no hay nada que decidir, y es
# `bloqueada`. Resolverlo para UNA de esas fallas -la columna que no es texto,
# mirando su clase- deja la regla escrita como un caso: sobre
# `c("1.234,56", "100.000", "1e5", ...)` -`1e5` no responde al formato
# regional- la accion salia `lista` con un `motivo_no_reversible` que decia "no
# es ejecutable sobre los datos completos". Por eso se pregunta al conversor,
# que es quien decide. Sin datos no se sabe, y no se afirma.
.regional_no_ejecutable_plan <- function(datos, columna, parametros,
                                         comprobacion) {
  if (!isTRUE(comprobacion$verificable) || isTRUE(comprobacion$ejecutable)) {
    return(FALSE)
  }
  indice <- .indice_nombre(columna, names(datos))
  if (is.na(indice)) return(FALSE)
  x <- datos[[indice]]
  for (punto in c("miles", "decimal")) {
    for (coma in c("miles", "decimal")) {
      configurados <- parametros
      configurados$punto_sin_coma <- punto
      configurados$coma_sin_punto <- coma
      prueba <- tryCatch(
        .convertir_numero_regional(x, configurados),
        error = function(e) e
      )
      if (!inherits(prueba, "error")) return(FALSE)
    }
  }
  TRUE
}

.comprobar_reversibilidad_accion <- function(datos, columna, estrategia,
                                             parametros) {
  sin_datos <- list(
    verificable = FALSE, ejecutable = FALSE, reversible = FALSE,
    n_no_reversibles = 0L,
    justificacion = paste0(
      "La reversibilidad no se puede comprobar sin los datos completos de `",
      columna, "`; pasar `datos` a `planificar_limpieza()` para habilitar esta acci\u00f3n."
    ), error = NULL
  )
  if (is.null(datos)) return(sin_datos)
  indice_columna <- if (is.data.frame(datos)) {
    .indice_nombre(columna, names(datos))
  } else NA_integer_
  if (!is.data.frame(datos) || is.na(indice_columna)) {
    return(list(
      verificable = FALSE, ejecutable = FALSE, reversible = FALSE,
      n_no_reversibles = 0L,
      justificacion = paste0(
        "La columna `", columna,
        "` no est\u00e1 disponible para comprobar la reversibilidad."
      ), error = NULL
    ))
  }
  x <- datos[[indice_columna]]
  convertido <- tryCatch({
    valor <- switch(
      estrategia,
      convertir_tipo = .convertir_tipo(x, parametros),
      convertir_numero_regional = .convertir_numero_regional(x, parametros),
      convertir_fecha_confirmada = .convertir_fecha(x, parametros),
      .detener("Estrategia de conversi\u00f3n no reconocida.", call. = FALSE)
    )
    if (is.list(valor) && !is.null(valor$valor)) valor$valor else valor
  }, error = function(e) e)
  if (inherits(convertido, "error")) {
    # Lo que no se ejecuta igual dice cuanto no se lee: el perfil ya lo cuenta
    # -"convertir dejaria 1 de 10 valores en NA"- y el plan publicaba
    # `n_no_reversibles = 0` al lado. La frase que sigue la pone quien decide
    # el estado (ver `.frase_conversion_no_ejecutable()`): aca se decia "se
    # conserva como accion destructiva no recomendada" tambien sobre una accion
    # `bloqueada` y sin marca de destructiva.
    no_leidos <- .valores_no_convertibles(x, estrategia, parametros)
    presentes <- sum(!is.na(x))
    return(list(
      verificable = TRUE, ejecutable = FALSE, reversible = FALSE,
      n_no_reversibles = as.integer(no_leidos),
      justificacion = paste0(
        "La conversi\u00f3n no es ejecutable sobre los datos completos: ",
        conditionMessage(convertido), " ",
        if (no_leidos == 0L && identical(estrategia, "convertir_numero_regional") &&
            identical(parametros$convencion, "ambigua")) {
          "Con la configuraci\u00f3n que la resuelve, se leen todos los valores presentes."
        } else if (no_leidos == 0L) {
          paste0(
            "Cada valor presente se lee por separado: lo que impide la ",
            "conversi\u00f3n es la columna entera."
          )
        } else {
          paste0(
            no_leidos, " de ", presentes,
            if (presentes == 1L) " valor presente" else " valores presentes",
            if (no_leidos == 1L) " no se lee." else " no se leen."
          )
        }
      ), error = conditionMessage(convertido)
    ))
  }
  evaluacion <- .evaluar_conversion(x, convertido, estrategia, parametros)
  riesgos <- character()
  if (evaluacion$n_ceros_iniciales) {
    riesgos <- c(
      riesgos,
      paste0(evaluacion$n_ceros_iniciales,
             if (evaluacion$n_ceros_iniciales == 1L) {
               " valor tiene cero inicial y se conserva su identidad textual"
             } else " valores tienen cero inicial y se conserva su identidad textual")
    )
  }
  if (!evaluacion$inyectiva) {
    riesgos <- c(
      riesgos,
      paste0(evaluacion$n_colisionados,
             if (evaluacion$n_colisionados == 1L) {
               " valor participa en una conversi\u00f3n no inyectiva"
             } else " valores participan en una conversi\u00f3n no inyectiva")
    )
  }
  # Sin esto la justificacion salia con un punto suelto adelante -"`. Se declara
  # destructiva`"- porque el unico riesgo presente no tenia texto. Declarar algo
  # destructivo sin decir por que deja al usuario sin nada que decidir.
  if (isTRUE(evaluacion$n_redondeados > 0L)) {
    riesgos <- c(
      riesgos,
      paste0(
        evaluacion$n_redondeados,
        if (evaluacion$n_redondeados == 1L) {
          " valor tiene m\u00e1s cifras de las que guarda un n\u00famero de "
        } else " valores tienen m\u00e1s cifras de las que guarda un n\u00famero de ",
        "doble precisi\u00f3n y se redondear\u00edan sin poder recuperarse"
      )
    )
  }
  justificacion <- if (evaluacion$reversible) {
    "La conversi\u00f3n es ejecutable e inyectiva sobre todos los valores presentes."
  } else {
    paste0(
      paste(riesgos, collapse = "; "),
      ". Se declara destructiva y no se recomienda autom\u00e1ticamente."
    )
  }
  list(
    verificable = TRUE, ejecutable = TRUE,
    reversible = evaluacion$reversible,
    n_no_reversibles = evaluacion$n_no_reversibles,
    justificacion = justificacion, error = NULL
  )
}

# Cuantos valores presentes no convierte el conversor de la accion, con su
# multiplicidad. El conversor decide sobre la columna entera y aborta al primer
# valor que no lee, asi que se le pregunta por partes: se divide el conjunto de
# valores distintos hasta aislar los que fallan, y cuesta del orden del numero de
# fallas por el logaritmo de los distintos, no una llamada por valor. Una
# conversion regional ambigua que solo espera configuracion se cuenta con la
# configuracion que menos deja afuera: son los valores que ninguna lee.
.valores_no_convertibles <- function(x, estrategia, parametros) {
  convertir <- switch(
    estrategia,
    convertir_tipo = function(v, p) .convertir_tipo(v, p),
    convertir_numero_regional = function(v, p) .convertir_numero_regional(v, p),
    convertir_fecha_confirmada = function(v, p) .convertir_fecha(v, p),
    return(NA_integer_)
  )
  configuraciones <- list(parametros)
  if (identical(estrategia, "convertir_numero_regional") &&
      identical(parametros$convencion, "ambigua")) {
    configuraciones <- list()
    for (punto in c("miles", "decimal")) {
      for (coma in c("miles", "decimal")) {
        configurados <- parametros
        configurados$punto_sin_coma <- punto
        configurados$coma_sin_punto <- coma
        configuraciones[[length(configuraciones) + 1L]] <- configurados
      }
    }
  }
  distintos <- unique(x)
  falla <- function(v, p) {
    inherits(tryCatch(convertir(v, p), error = function(e) e), "error")
  }
  aislar <- function(indices, p) {
    if (!length(indices) || !falla(distintos[indices], p)) return(integer())
    if (length(indices) == 1L) return(indices)
    mitad <- length(indices) %/% 2L
    c(aislar(indices[seq_len(mitad)], p),
      aislar(indices[-seq_len(mitad)], p))
  }
  conteos <- vapply(configuraciones, function(p) {
    fallan <- aislar(seq_along(distintos), p)
    if (!length(fallan)) return(0)
    as.numeric(sum(x %in% distintos[fallan]))
  }, numeric(1L))
  as.integer(min(conteos))
}

# La frase final de una conversion que no se ejecuta, segun el estado con que
# queda en el plan: `bloqueada` no se puede activar, y `lista` -una regional
# ambigua- se ejecuta una vez configurada. No es "destructiva no recomendada":
# no se ejecuta, y su marca de destructiva lo decia al reves.
.frase_conversion_no_ejecutable <- function(comprobacion, estado) {
  if (!isTRUE(comprobacion$verificable) || isTRUE(comprobacion$ejecutable)) {
    return("")
  }
  if (identical(estado, "bloqueada")) {
    return(paste0(
      " La acci\u00f3n queda bloqueada: no se puede activar sobre estos datos ",
      "hasta resolver esos valores."
    ))
  }
  if (identical(estado, "lista")) {
    return(paste0(
      " La acci\u00f3n queda lista sin activar: se ejecuta una vez configurada."
    ))
  }
  ""
}

.evaluar_conversion <- function(original, convertido, estrategia, parametros) {
  antes <- .texto_representacion_conversion(original)
  despues <- .texto_representacion_conversion(convertido)
  # Recortado por bytes: `trimws()` sobre el texto crudo avisaba bajo `C` con
  # marcas mezcladas y abortaba bajo un locale UTF-8 con un byte suelto.
  presentes <- !is.na(antes) & nzchar(.recortar_bytes(antes)) & !is.na(despues)
  colisionados <- rep(FALSE, length(antes))
  if (any(presentes)) {
    pares <- unique(data.frame(
      original = .nombres_para_operar(antes[presentes]),
      convertido = .nombres_para_operar(despues[presentes]),
      stringsAsFactors = FALSE
    ))
    por_valor <- split(pares$original, pares$convertido)
    repetidos <- unique(unlist(por_valor[lengths(por_valor) > 1L],
                               use.names = FALSE))
    colisionados <- presentes & .nombres_para_operar(antes) %in% repetidos
  }
  destino <- if (identical(estrategia, "convertir_numero_regional")) {
    "numerico"
  } else {
    parametros$tipo
  }
  numerico <- destino %in% c("numerico", "entero", "doble")
  ceros <- rep(FALSE, length(antes))
  if (numerico) {
    ceros <- !is.na(antes) &
      grepl("^0[0-9]", .recortar_bytes(antes), perl = TRUE, useBytes = TRUE)
  }
  # Un entero por encima de 2^53 NO entra en un `double` sin redondear, y esa
  # perdida no produce colision: `9007199254740993` se vuelve
  # `9007199254740992` y ningun otro valor termina ahi, asi que la comprobacion
  # de arriba -que compara representaciones textuales- no la ve. El plan
  # publicaba entonces `reversible = TRUE`, `destructiva = FALSE` y
  # `n_no_reversibles = 0` sobre un valor que habia destruido: ningun formato
  # recupera el 3 perdido.
  #
  # Es la misma regla que ya aplica el perfil, que por encima de 2^53 se niega
  # a medir la secuencia entera en vez de publicar un numero redondeado. Se
  # mira el valor CONVERTIDO y no el texto de entrada, para que no dependa de
  # como venia escrito -con puntos de miles, con signo o en notacion regional-.
  #
  # La comprobacion NO puede ser de magnitud. `9007199254740993` redondea a
  # `9007199254740992`, que es 2^53 exacto, asi que `> 2^53` da FALSE sobre el
  # unico valor que hay que atrapar: la guarda se prueba con el valor donde
  # cambia. Se comparan los DIGITOS del original contra los del convertido, que
  # es exacto y ademas no depende de como viniera escrito.
  redondeados <- rep(FALSE, length(antes))
  if (numerico) {
    # La perdida ocurre al parsear, y se mide sobre el texto EXACTO que el
    # conversor le pasa a `as.numeric()`: el que devuelve su propia
    # normalizacion, antes de cualquier unidad. Esta guarda rearmaba antes ese
    # texto por su cuenta, con una rama por formato, y cada formato nuevo fue
    # un agujero -entero plano, con `%`, cientifica, mantisa con coma, entero
    # es-UY con `,0`-. Con la normalizacion compartida no hay
    # formato que el conversor lea y la guarda no vea.
    texto <- tryCatch(
      if (identical(estrategia, "convertir_numero_regional")) {
        .texto_regional_normalizado(original, parametros)$texto
      } else {
        .texto_tipo_normalizado(original)
      },
      error = function(e) NULL
    )
    # La regla es la de `.cifras_que_redondea_un_doble()` (en `R/tipos.R`), con
    # la que el perfil declara las mismas perdidas: un entero se compara contra
    # su valor exacto -por encima de 2^53 el `double` guarda otro entero-, y un
    # decimal a la precision con que VINO ESCRITO -a su propia precision todo
    # decimal de hasta 15 cifras vuelve exacto, y `33.333333333333333333%`, de
    # 20 cifras, no-. Escrita una sola vez, el plan y el perfil no pueden
    # contar distinto.
    if (!is.null(texto) && length(texto) == length(antes)) {
      # El numero es el que el conversor obtiene de ese texto: `.convertir_tipo()`
      # lee el texto con el lector comun y la convencion de la columna, y la
      # conversion regional le pasa a `as.numeric()` su texto ya normalizado.
      valores_precision <- if (identical(estrategia, "convertir_tipo") &&
                               (is.character(original) || is.factor(original))) {
        suppressWarnings(.leer_numeros_texto(texto))
      } else {
        suppressWarnings(as.numeric(texto))
      }
      redondeados <- presentes & .cifras_que_redondea_un_doble(
        texto, valores_precision
      )
    }
  }
  riesgos <- colisionados | ceros | redondeados
  list(
    inyectiva = !any(colisionados),
    reversible = !any(riesgos),
    n_no_reversibles = as.integer(sum(riesgos)),
    n_ceros_iniciales = as.integer(sum(ceros)),
    n_colisionados = as.integer(sum(colisionados)),
    n_redondeados = as.integer(sum(redondeados))
  )
}

# Las acciones que transforman CELDA por celda. Son las unicas que pueden
# respetar el universo aplicable: se aplican y despues se devuelven a su valor
# las celdas de fuera.
#
# Este comentario decia que conservan la clase de la columna, y sobre un FACTOR
# no es cierto: todas devuelven texto -`.resultado_texto()` lo decide en un solo
# lugar, a proposito, para no dejar un factor incompleto-. La afirmacion falsa
# se quedo escrita mientras el plan recomendaba y ACTIVABA `recortar_espacios`
# sobre un factor ordenado, publicando `destructiva = FALSE`; la columna volvia
# `character`, sin orden y sin los niveles sin observaciones. Lo que el usuario
# lee ahora lo dice: ver `.declarar_texto_en_columna_factor()`. Las conversiones de
# tipo no estan aca porque no se pueden aplicar a medias -una columna tiene un
# solo tipo-, y para ellas el plan declara el alcance real.
.estrategias_por_celda <- c(
  "convertir_ausencias_textuales", "recortar_espacios",
  "eliminar_controles_invisibles", "normalizar_espacios_invisibles",
  "decodificar_entidades_html", "reemplazar_separadores",
  "reparar_codificacion", "convertir_minusculas", "convertir_mayusculas",
  "convertir_titulo", "convertir_segun_diccionario",
  "convertir_sentinelas_numericos"
)

# Si una estrategia transforma celda por celda y por eso respeta el universo
# aplicable. La imputacion por dependencia tambien lo hace -llena celdas `NA` de
# una columna- pero su nombre lleva el determinante, asi que no podia estar en la
# lista y quedaba fuera de las dos puertas: el plan no le ponia la regla y
# `aplicar()` no recortaba. Con `descripcion ~ flag == "si"`, el perfil contaba 1
# faltante y la accion imputaba 3, incluidas las 2 filas que el usuario declaro
# vacias por diseno; el re-perfil acusaba `valor_fuera_de_aplicabilidad`.
# Medido. Una sola pregunta para las dos puertas.
.estrategia_por_celda <- function(estrategia) {
  estrategia <- as.character(estrategia)
  estrategia %in% .estrategias_por_celda |
    startsWith(estrategia, "imputar_dependencia_funcional__")
}

# Las acciones de outliers reciben la regla de `aplicabilidad` y la aplican ELLAS
# MISMAS: los limites de Tukey dependen de que valores entran en el calculo, asi
# que no alcanza con restaurar despues las celdas de afuera. Medido:
# `marcar_outliers` marcaba una fila fuera del universo y
# `winsorizar_outliers` recortaba 400 a 242,5 -limite de la columna entera- en vez
# de 106,5, el del universo que el perfil midio. Y restaurar despues confundia la
# winsorizacion con una fusion al recontar las irreversibles.
.estrategias_outliers <- c("marcar_outliers", "winsorizar_outliers")
.declarar_texto_en_columna_factor <- function(plan, perfil) {
  # Una sola vez para todas las acciones por celda, y no una por estrategia: lo
  # que cambia la clase no es la estrategia sino la columna, y el contrato de
  # devolver texto esta escrito en un solo lugar. Enumerar las nueve ramas del
  # planificador que aceptan un factor era enumerar sitios; la pregunta es si la
  # columna declara un factor.
  #
  # Donde se declara: en la justificacion, que es lo que se lee para decidir. Y
  # si el factor declara un ORDEN, la accion queda recomendada pero sin activar,
  # con el mismo idioma que ya usan las acciones contextuales del plan: el orden
  # es algo que el usuario declaro y que el texto no puede llevar, asi que la
  # decision es suya.
  if (!nrow(plan)) return(plan)
  for (j in seq_len(nrow(plan))) {
    if (!plan$estrategia[[j]] %in% .estrategias_por_celda) next
    columna <- plan$columna[[j]]
    if (is.na(columna)) next
    fila <- .fila_perfil(perfil, columna)
    if (is.null(fila)) next
    if (!as.character(fila$tipo_declarado[[1L]]) %in%
          c("factor", "factor-ordenado")) {
      next
    }
    ordenado <- identical(as.character(fila$tipo_declarado[[1L]]),
                          "factor-ordenado")
    plan$justificacion[[j]] <- paste0(
      plan$justificacion[[j]],
      " La columna es un factor", if (ordenado) " ordenado" else "",
      " y la acci\u00f3n devuelve texto: ",
      if (ordenado) "el orden declarado y " else "",
      "los niveles sin observaciones no se conservan.",
      if (ordenado) {
        paste0(
          " Queda recomendada pero sin activar: conservar el orden es una ",
          "decisi\u00f3n del dominio."
        )
      } else {
        ""
      }
    )
    if (ordenado) plan$aplicar[[j]] <- FALSE
  }
  plan
}

.estrategias_cambio_de_tipo <- c(
  "convertir_tipo", "convertir_numero_regional", "convertir_fecha_confirmada"
)
.estrategias_por_fila <- c(
  "marcar_filas_ausentes", "eliminar_filas_ausentes"
)

# Una conversion que se ejecuto sobre los datos y perdio algo es destructiva,
# sea o no segura la columna. La marca se calculaba solo cuando la columna era
# segura: sobre `10%` junto a `0.1` -que no lo es- el plan media cinco valores
# irrecuperables, su propio motivo decia "Se declara destructiva", y la marca
# quedaba en FALSE. Activada a mano, el aviso de acciones destructivas no
# aparecia y el registro repetia FALSE. Lo que no se pudo ejecutar no cuenta:
# una columna ambigua que solo espera configuracion no destruye nada.
.perdida_medida <- function(comprobacion) {
  isTRUE(comprobacion$ejecutable) && isFALSE(comprobacion$reversible)
}

# Lo que la conversion regional hace ademas de cambiar el tipo. Un `%` se
# divide por 100: sin decirlo, "10%" pasaba a 0.1 con una justificacion que
# aseguraba que la columna se convertia "sin elegir entre interpretaciones".
.nota_unidad_numero <- function(parametros) {
  unidad <- parametros$unidad
  moneda <- parametros$moneda
  nota <- character()
  if (length(unidad) == 1L && !is.na(unidad) && nzchar(unidad)) {
    nota <- c(nota, if (identical(unidad, "%")) {
      paste0(
        " Los valores con \"%\" se dividen por 100; un porcentaje que no ",
        "est\u00e1 entre 0 y 100 queda fuera de [0, 1]."
      )
    } else {
      paste0(
        " La unidad ", .comillas_sin_locale(unidad, quote = "\""), " deja de formar ",
        "parte del valor; queda en los par\u00e1metros."
      )
    })
  }
  if (length(moneda) == 1L && !is.na(moneda) && nzchar(moneda)) {
    nota <- c(nota, paste0(
      " El s\u00edmbolo ", .comillas_sin_locale(moneda, quote = "\""), " deja de formar ",
      "parte del valor; queda en los par\u00e1metros."
    ))
  }
  paste(nota, collapse = "")
}

# Por que una columna de numeros como texto no se convierte. Se decia siempre
# que faltaba evidencia para distinguir el separador decimal del de miles,
# tambien en `5` junto a `5 %`, que no tiene separadores: el motivo publicado
# no era el motivo.
.motivo_numero_no_seguro <- function(fila) {
  # Un perfil de base de datos o guardado por una version anterior puede no
  # traer estos campos, o traerlos vacios: se leen sin suponer que estan, y lo
  # que no se sabe no se afirma.
  convencion <- fila[["numero_texto_convencion"]]
  proporcion <- fila[["proporcion_numeros_texto"]]
  convencion <- if (length(convencion)) as.character(convencion[[1L]]) else NA_character_
  proporcion <- if (length(proporcion)) as.numeric(proporcion[[1L]]) else NA_real_
  if (isTRUE(convencion %in% c("ambigua", "mixta"))) {
    paste0(
      "La columna no aporta evidencia suficiente para distinguir el ",
      "separador decimal del separador de miles."
    )
  } else if (isTRUE(proporcion < 1)) {
    paste0(
      "Hay valores presentes que no responden al formato num\u00e9rico de ",
      "la columna; convertirlos exigir\u00eda decidir qu\u00e9 hacer con ellos."
    )
  } else if (isTRUE(proporcion == 1)) {
    paste0(
      "La columna mezcla unidades o monedas, o valores con y sin ellas; ",
      "convertirla exigir\u00eda decidir si un n\u00famero sin unidad est\u00e1 en ",
      "la misma escala que los dem\u00e1s."
    )
  } else {
    paste0(
      "La conversi\u00f3n exige que todos los valores presentes compartan ",
      "convenci\u00f3n decimal, unidad y moneda, y el perfil no lo acredita."
    )
  }
}

# Los centinelas numericos que el perfil CONTO en la columna, leidos de la
# evidencia del hallazgo. El detector decide por columna cuales cuentan -un
# `999` dentro de una numeracion de 1 a 2000 no es un centinela, diez `-9`
# si-, y la accion llevaba la lista entera: al marcarla, `aplicar()` convertia
# tambien el `999` legitimo, once celdas donde el plan anunciaba diez. Medido.
# La evidencia nombra cada valor con su conteo, y es lo que ya lee la accion
# hermana (`.marcadores_de_ausencia()`): si los conteos de sus etiquetas
# numericas suman lo que el perfil conto, la lista esta completa y es la del
# detector. Si no -la evidencia muestra solo las marcas mas frecuentes, o una
# etiqueta no se puede releer- devuelve `NULL`, y quien llama conserva la lista
# entera, como antes: nunca una lista parcial, que dejaria celdas sin convertir.
.sentinelas_contados_en_evidencia <- function(evidencia, n_numericos,
                                              convencion = NULL,
                                              textuales = character()) {
  if (is.null(evidencia) || !length(evidencia) || is.na(evidencia[[1L]]) ||
      !nzchar(evidencia[[1L]]) || !isTRUE(n_numericos > 0)) {
    return(NULL)
  }
  # La evidencia separa las marcas con "; ". Una marca declarada que lleva ese
  # separador adentro -`cadenas_ausencia = "9; x"`- se parte en dos, y un trozo
  # se leia como un numero que nadie conto: no se puede confiar en el corte.
  if (any(grepl("; ", textuales, fixed = TRUE, useBytes = TRUE))) {
    return(NULL)
  }
  partes <- strsplit(
    .declarar_utf8_valido(as.character(evidencia)[[1L]]), "; ", fixed = TRUE
  )[[1L]]
  # Cada parte tiene que ser una marca con su conteo; si a alguna le falta, la
  # evidencia no tiene la forma que se sabe leer.
  if (!all(grepl("\\([0-9]+\\)$", partes))) return(NULL)
  etiquetas <- sub("\\s*\\([0-9]+\\)$", "", partes)
  conteos <- suppressWarnings(
    as.numeric(sub("^.*\\(([0-9]+)\\)$", "\\1", partes))
  )
  numeros <- .leer_numeros_texto_centinela(etiquetas, convencion)
  # Una marca textual declarada que se escribe como numero -`cadenas_ausencia =
  # "99"`- se conto entre las textuales: no entra en esta cuenta.
  numericas <- !is.na(numeros) & !(
    .minusculas_para_comparar(etiquetas) %in%
      .minusculas_para_comparar(textuales)
  )
  if (!any(numericas) || anyNA(conteos[numericas]) ||
      !isTRUE(sum(conteos[numericas]) == as.numeric(n_numericos))) {
    return(NULL)
  }
  .normalizar_sentinelas_numericos(numeros[numericas])
}

# Los centinelas numericos que el perfil conto, como los publica la traza del
# hallazgo. `NULL` si la traza no los trae -un perfil guardado por una version
# anterior- o los trae tapados -la proteccion toco la evidencia del hallazgo y
# el perfil no dice que valor conto-. Nunca una lista parcial: con un `NA`
# adentro no se sabe que falta, y convertir el resto dejaria celdas contadas
# sin convertir.
.sentinelas_contados_de_traza <- function(hallazgo) {
  traza <- tryCatch(hallazgo$trazabilidad[[1L]], error = function(e) NULL)
  if (!is.list(traza)) return(NULL)
  contados <- traza[["sentinelas_contados"]]
  if (!is.numeric(contados) || !length(contados) || anyNA(contados)) {
    return(NULL)
  }
  .normalizar_sentinelas_numericos(contados)
}

# Lo que `aplicar()` necesita para acotar la lista de centinelas cuando el plan
# no sabe cuales conto el perfil: el contexto con que el detector decide, tal
# como el perfil ya lo publica en la fila de la columna -el tipo inferido, la
# convencion decimal y las dos banderas de la numeracion- y en `meta` -la lista
# de centinelas-. Con eso `aplicar()` corre el mismo detector sobre la columna
# que recibe (`.sentinelas_contados_sobre_datos()`) y convierte solo los
# centinelas de la lista que el detector cuenta. La guarda nunca agrega: lo que
# no esta en `valores` no se convierte.
#
# `sentinelas` es la lista de `meta`, que con proteccion trae un `NA` en el
# lugar de cada centinela declarado que es un valor protegido. Ese hueco solo
# lo deja una declaracion -el catalogo del paquete no se tapa-, y por eso
# alcanza para saber que la lista fue declarada aunque lo que queda a la vista
# coincida con el catalogo.
#
# `universo_recortado` dice si el perfil conto dentro de un universo aplicable
# mas chico que la columna. Las banderas de la numeracion describen ese
# universo, no la columna entera, asi que sin la regla `aplicar()` no puede
# reproducir el conteo: ver `.mascara_universo_guarda()`. Una columna declarada
# en `columnas_opcionales` no cuenta: su universo deja afuera solo las celdas
# ausentes, que el detector no mira.
.guarda_sentinelas_del_perfil <- function(perfil, columna, fila, sentinelas,
                                          convencion) {
  campo <- function(nombre) {
    if (!is.null(fila) && nombre %in% names(fila)) fila[[nombre]][[1L]] else NA
  }
  visibles <- .normalizar_sentinelas_numericos(sentinelas)
  opcional <- !is.na(columna) &&
    .nombres_para_operar(columna) %in%
      .nombres_para_operar(as.character(perfil$meta$columnas_opcionales))
  n <- suppressWarnings(as.numeric(campo("n")))
  n_aplicables <- suppressWarnings(as.numeric(campo("n_aplicables")))
  list(
    tipo = as.character(campo("tipo_inferido")),
    convencion = if (length(convencion) == 1L && !is.na(convencion)) {
      as.character(convencion)
    } else "",
    densa = isTRUE(as.logical(campo("secuencia_entera_densa"))),
    moda_sobresale = isTRUE(
      as.logical(campo("moda_sobresale_secuencia_entera"))
    ),
    sentinelas = visibles,
    declarada = anyNA(sentinelas) ||
      length(.sentinelas_numericos_declarados(visibles)) > 0L,
    universo_recortado = !opcional && isTRUE(n_aplicables < n)
  )
}

# Lleva la regla de aplicabilidad del perfil a las acciones del plan. Sin esto
# la limpieza operaba sobre la columna entera aunque el usuario hubiera
# declarado que una parte de las filas no le corresponde: una `S/D` en una fila
# declarada fuera se volvia `NA`, y el plan que prometia 2 cambios hacia 4.
.aplicabilidad_en_plan <- function(resultado, perfil) {
  reglas <- perfil$meta$reglas_aplicabilidad
  if (!length(reglas) || !nrow(resultado)) return(resultado)
  for (i in seq_len(nrow(resultado))) {
    columna <- resultado$columna[[i]]
    if (is.na(columna) || !columna %in% names(reglas)) next
    estrategia <- as.character(resultado$estrategia[[i]])
    if (.estrategia_por_celda(estrategia)) {
      parametros <- resultado$parametros[[i]]
      if (is.null(parametros)) parametros <- list()
      parametros$aplicabilidad <- reglas[[columna]]
      resultado$parametros[[i]] <- parametros
    } else if (estrategia %in% c(.estrategias_por_fila, .estrategias_outliers)) {
      parametros <- resultado$parametros[[i]]
      if (is.null(parametros)) parametros <- list()
      parametros$aplicabilidad <- reglas[[columna]]
      resultado$parametros[[i]] <- parametros
    } else if (estrategia %in% .estrategias_cambio_de_tipo) {
      # No se puede aplicar a medias, y el alcance real -todos los presentes de
      # la columna- ya lo cuenta la accion al nacer (`.n_presentes_columna()`).
      # Aca solo se explica. Recontar aca era un segundo camino, y contaba
      # distinto que el primero: este solo corria con presentes fuera.
      fila <- .fila_perfil(perfil, columna)
      if (!is.null(fila) &&
          "n_presentes_fuera_de_aplicabilidad" %in% names(fila)) {
        fuera <- fila$n_presentes_fuera_de_aplicabilidad[[1L]]
        if (isTRUE(fuera > 0L)) {
          resultado$justificacion[[i]] <- paste0(
            resultado$justificacion[[i]], " La conversi\u00f3n cambia la ",
            "columna entera, incluidas ", fuera,
            if (fuera == 1L) " celda fuera del universo " else " celdas fuera del universo ",
            "aplicable: una columna tiene un solo tipo."
          )
        }
      }
    }
  }
  resultado
}

# Devuelve a su valor las celdas fuera del universo aplicable despues de que la
# accion las transformo, y recuenta. Es un solo lugar para todos los ejecutores
# por celda, en vez de una copia de esta logica en cada uno.
.restringir_a_aplicabilidad <- function(ejecutada, anterior, accion,
                                        original = anterior) {
  estrategia <- as.character(accion$estrategia[[1L]])
  regla <- accion$parametros[[1L]]$aplicabilidad
  columna <- accion$columna[[1L]]
  if (is.null(regla) || !.estrategia_por_celda(estrategia) ||
      is.na(columna) || !columna %in% names(anterior)) {
    return(ejecutada)
  }
  # La regla se evalua sobre los datos ORIGINALES, no sobre `anterior`. El
  # universo lo define el perfil, que se calculo sobre los originales. Evaluada
  # sobre el estado ya modificado, una accion previa podia cambiar la columna
  # de la que depende la regla -quitarle un invisible a `cat` y volverla "x"- y
  # meter en el universo una fila que el perfil habia declarado fuera: el
  # recorte tocaba justo esa celda y el registro lo daba por ejecutado.
  if (nrow(original) != nrow(anterior)) {
    # Una accion previa elimino filas y la mascara de los originales ya no se
    # alinea con las que quedan. Rastrear cuales sobrevivieron seria fragil;
    # tocar la columna entera seria el defecto. Se falla y se dice.
    ejecutada$error <- paste(
      "No se puede respetar la aplicabilidad despu\u00e9s de eliminar filas:",
      "aplicar las acciones por celda antes de las que eliminan filas."
    )
    return(ejecutada)
  }
  mascara <- tryCatch(
    .evaluar_predicado_aplicabilidad(original, columna, regla),
    error = function(e) e
  )
  if (inherits(mascara, "error")) {
    # Sin poder evaluar la regla sobre estos datos no se puede respetar el
    # universo, y tocar la columna entera seria justo el defecto.
    ejecutada$error <- paste(
      "No se pudo evaluar la regla de aplicabilidad sobre estos datos:",
      conditionMessage(mascara)
    )
    return(ejecutada)
  }
  # Indeterminado es "no se sabe si corresponde": no se toca.
  fuera <- is.na(mascara) | !mascara
  if (!any(fuera)) return(ejecutada)
  viejo <- anterior[[columna]]
  nuevo <- ejecutada$datos[[columna]]
  sin_restaurar <- nuevo
  # Los ejecutores de texto devuelven `character` aunque la entrada sea factor;
  # asignar el factor directo lo convertiria en sus codigos numericos.
  restaurar <- viejo[fuera]
  if (is.factor(restaurar) && !is.factor(nuevo)) restaurar <- as.character(restaurar)
  nuevo[fuera] <- restaurar
  ejecutada$datos[[columna]] <- nuevo
  # Tambien cuenta la celda ausente que paso a tener valor: es el cambio de la
  # imputacion, y `.celdas_cambiadas()` solo mira las que tenian valor. Sin esto
  # la imputacion recortada a su universo contaba 0 y quedaba `fallida` con la
  # celda imputada.
  cambiadas <- sum(
    .celdas_cambiadas(viejo, nuevo) | (is.na(viejo) & !is.na(nuevo))
  )
  n_anterior <- ejecutada$n
  ejecutada$n <- cambiadas
  # Las irreversibles se recuentan con la MISMA regla con que la accion las conto,
  # y la regla se reconoce por su resultado, no por el nombre de la accion. Si el
  # conteo coincidia con medir la perdida sobre el resultado -recortar espacios,
  # quitar invisibles: pierden valor solo cuando dos valores colapsan-, se vuelve
  # a medir sobre el resultado restaurado: una celda devuelta a su valor ya no
  # colapsa con nadie. Medido: `" y"` fuera del universo colisionaba con `"y"`,
  # se restauraba, y el registro seguia contando 1 irreversible sobre un
  # resultado sin ninguna perdida. Si contaba como irreversible CADA cambio -una
  # imputacion no se distingue del dato-, sigue contando cada cambio.
  if (!is.null(ejecutada$n_no_reversibles) &&
      isTRUE(ejecutada$n_no_reversibles > 0L)) {
    por_resultado <- sum(.celdas_que_pierden_valor(viejo, sin_restaurar))
    ejecutada$n_no_reversibles <- if (
      isTRUE(ejecutada$n_no_reversibles == por_resultado)
    ) {
      sum(.celdas_que_pierden_valor(viejo, nuevo))
    } else if (isTRUE(ejecutada$n_no_reversibles == n_anterior)) {
      cambiadas
    } else {
      min(ejecutada$n_no_reversibles, cambiadas)
    }
  }
  ejecutada
}

#' Construir y aplicar un plan de limpieza auditable
#'
#' `planificar_limpieza()` transforma los hallazgos de un objeto `perfil` en un
#' objeto de datos editable, sin modificar los datos examinados. Cada fila
#' representa una acción propuesta. Sólo se marcan como recomendadas las
#' estrategias correctas con independencia del dominio; algunas, como marcar
#' valores extremos, permanecen inactivas hasta que se decida actuar. Las
#' decisiones contextuales quedan desactivadas y los formatos de fecha ambiguos
#' quedan bloqueados.
#'
#' `aplicar()` ejecuta exclusivamente las filas con `aplicar == TRUE`, sobre una
#' copia de `datos`. Verifica que cada columna siga siendo identificable y que
#' las conversiones sean completas antes de sustituirla. Devuelve los datos
#' nuevos junto con un registro de las acciones y sus parámetros. El mismo
#' registro queda en el atributo `registro_limpieza` de los datos resultantes.
#'
#' Las alternativas para un mismo hallazgo comparten `grupo`; las acciones
#' independientes usan `NA`. Como máximo una alternativa de cada grupo puede
#' tener `aplicar == TRUE`, invariante que `aplicar()` vuelve a validar. No se
#' agrega una fila ficticia para "no hacer nada": `decision_grupo` distingue
#' `pendiente`, `recomendada`, `desactivada`, `elegida` y `omitida`, mientras
#' `recomendacion_grupo = "no_hacer_nada"` representa una recomendación
#' explícita de conservar los datos. Esto permite separar un grupo aún no
#' revisado de una omisión deliberada.
#'
#' `estado` distingue acciones `lista`, `bloqueada` e `informativa`; `orden`
#' fija la secuencia reproducible. La acción `reparar_codificacion`, cuando está
#' seleccionada, tiene precedencia sobre las demás acciones de su misma columna:
#' corre primero en esa columna para que las restantes reciban texto ya
#' decodificado, aunque su `orden` editado sea mayor. No reordena acciones de
#' otras columnas. Del mismo modo, `decodificar_entidades_html` corre antes que
#' `convertir_minusculas`, `convertir_titulo` y `convertir_mayusculas` de su
#' columna aunque un `orden` editado la ponga detrás: una entidad con nombre
#' distingue mayúsculas —`&eacute;` es «é» y `&Eacute;` es «É»—, y cambiar la
#' caja antes de decodificar cambiaba la letra o dejaba la entidad sin leer
#' (`&Amp;`). Con el `orden` del plan ya corre antes y nada se mueve.
#' Entre las acciones restantes, y entre empates, el desempate lo resuelve
#' `id_accion`, no la posición de la fila: reordenar el plan no cambia el
#' resultado. Y un grupo marcado como `elegida` sin ninguna acción activa se
#' rechaza, porque no es ni una elección ni una omisión. `n_afectadas` es la estimación del perfil
#' sobre **lo que esta acción tocaría**, que puede ser menos que el conteo del
#' hallazgo que la originó cuando la acción sólo cubre parte del caso: una
#' columna con tres valores de codificación rota, de los cuales uno es
#' reparable, produce un hallazgo con `n_afectados = 3` y una acción
#' `reparar_codificacion` con `n_afectadas = 1`. Las dos cifras son ciertas y
#' cuentan cosas distintas. `unidad_conteo` dice si cuenta filas, columnas o
#' valores distintos —lo
#' declara la acción cuando cuenta en una unidad propia, y sólo si no lo hace se
#' hereda del hallazgo—. El registro informa `n_cambiadas` sobre los datos
#' recibidos.
#'
#' Con un universo declarado, `n_afectadas` también cuenta lo que la acción va
#' a cambiar. Las acciones por celda no tocan las filas que `aplicabilidad`
#' deja fuera, y tampoco las cuentan. Una conversión de tipo
#' cambia la columna entera —una columna tiene un solo tipo—, así que cuenta
#' todos sus valores presentes: los del universo, los de fuera y los de las
#' filas donde la regla no se pudo decidir. Las celdas vacías no se cuentan:
#' fuera del universo están vacías por diseño, y en una columna de
#' `columnas_opcionales` el universo son justamente las celdas con valor.
#' Aplicada sola sobre los datos del perfil, la conversión registra en
#' `n_cambiadas` lo que anunció.
#'
#' **`n_afectadas` y `n_cambiadas` pueden no coincidir, y las dos son ciertas.**
#' La estimación se calcula sobre los datos que se perfilaron; el registro
#' cuenta lo que pasó al aplicar. Si una acción anterior del mismo plan ya tocó
#' esa columna, la posterior encuentra menos —o más— de lo estimado. Las
#' excepciones de precedencia son dos: `reparar_codificacion`, que se adelanta a
#' las demás acciones de su columna para que ninguna opere sobre mojibake, y
#' `decodificar_entidades_html`, que se adelanta a las conversiones de caja. Por
#' ejemplo, con
#' `convertir_sentinelas_numericos` (orden 110) convirtiendo tres `-999` en
#' ausentes, `winsorizar_outliers` (orden 520) recorta dos valores donde el plan
#' estimaba siete. No es un desvío que ocultar: `orden`, `n_afectadas` y
#' `n_cambiadas` se publican los tres, y compararlos es la forma de ver el
#' efecto de la composición. Sólo el caso extremo —la acción no produce **ningún**
#' efecto, también cuando el plan no trae una estimación válida— se registra
#' como `fallida` con su motivo, salvo que una acción anterior del mismo plan ya
#' haya hecho su trabajo: si la misma detección con que el plan contó la acción
#' encontraba el problema en los datos que recibió `aplicar()` y ya no lo
#' encuentra cuando a la acción le toca correr, la acción queda `ejecutada` con
#' `n_cambiadas = 0` y sin `error`, y el resumen impreso no la cuenta entre las
#' fallidas. Por ejemplo, `convertir_ausencias_textuales` (orden 100) vuelve
#' ausentes las celdas en blanco y a `recortar_espacios` (orden 200) no le
#' queda nada que recortar en ellas. Esa detección existe para las acciones de
#' texto que el perfil cuenta con `n_espacios_borde`, `n_invisibles_eliminables`,
#' `n_espacios_invisibles`, `n_entidades_html`, `n_separadores_en_campo` y
#' `n_codificacion_reparable`: `recortar_espacios`,
#' `eliminar_controles_invisibles`, `normalizar_espacios_invisibles`,
#' `decodificar_entidades_html`, `reemplazar_separadores` y
#' `reparar_codificacion`. Si el problema sigue en los datos y la acción no lo
#' tocó, si nunca estuvo en los datos que recibió `aplicar()` —un plan aplicado
#' a otra entrega— o si la acción no tiene esa detección, sigue `fallida`.
#'
#' **Esa comparación sólo lee composición cuando las dos cifras cuentan en la
#' misma unidad.** `n_afectadas` cuenta en la `unidad_conteo` que el plan
#' declara y `n_cambiadas` cuenta lo que la acción tocó al aplicarse, que es
#' su unidad natural. Cuando la acción declara `unidad_conteo =
#' "valor_distinto"` —las conversiones de mayúsculas y minúsculas—, las dos
#' cifras miden poblaciones distintas y su diferencia no dice nada sobre la
#' composición: medido sobre `c("Ana", "ana", "ANA", "Beto", " Ana ")`,
#' `convertir_minusculas` estima `n_afectadas = 3` valores distintos y el
#' registro publica `n_cambiadas = 4` celdas. Las dos son ciertas. La unidad
#' del plan se recupera uniendo el registro con el plan por `id_accion`, que
#' los dos publican. `reversible`
#' indica si la conversión conserva la identidad de cada valor. Las
#' conversiones se comprueban sobre todos los valores de `datos`: las numéricas
#' bloquean ceros iniciales, colisiones no inyectivas y valores con más cifras
#' significativas de las que guarda un número de doble precisión —un decimal se
#' compara con la precisión con que vino escrito—, mientras que fechas,
#' fechas-hora y lógicos sólo bloquean conversiones no ejecutables o no
#' inyectivas. Las fechas pueden cambiar a la representación canónica del tipo
#' sin que eso sea una pérdida. Sin `datos` no se puede hacer la comprobación y
#' la acción queda bloqueada. Cuando **una conversión de tipo** se ejecuta y no
#' es reversible se marca `destructiva` —también si la columna no era segura y
#' se activa a mano—, no se activa por defecto y el registro conserva
#' `n_no_reversibles` y la justificación de la decisión.
#'
#' `destructiva` no es sinónimo de "pierde algo": marca las acciones que el
#' usuario tiene que activar a mano —las que retiran filas o columnas y las
#' conversiones que pierden representación, como `winsorizar_outliers` sobre una
#' columna **guardada como entera**, donde los límites de Tukey son cuartiles y
#' la columna queda en doble precisión: la justificación lo dice y `parametros`
#' publica `tipo_original` y `tipo_resultante`. Sobre cualquier otra clase
#' —texto con números, factor, fecha— la acción conserva la clase y reescribe
#' sólo las celdas recortadas; `Inf`, `NaN` y `NA` quedan como estaban— y por
#' eso ninguna acción
#' `destructiva` puede estar `recomendada`. Hay acciones recomendadas que sí
#' pierden el valor de una celda: `convertir_ausencias_textuales` cambia un
#' marcador por `NA` y `eliminar_controles_invisibles` quita un carácter. Esas
#' lo dicen en su justificación y el registro las cuantifica en
#' `n_no_reversibles`; leer `destructiva = FALSE` no significa que no se haya
#' perdido nada, sino que el paquete pudo recomendar la acción sin conocer el
#' dominio.
#' Sobre una columna **factor** las acciones por celda devuelven texto: el
#' resultado no puede ser un factor incompleto, así que el orden declarado y los
#' niveles sin observaciones no se conservan. La justificación de la acción lo
#' dice, y si el factor es ordenado la acción queda recomendada pero **sin
#' activar**, porque conservar el orden es una decisión del dominio.
#'
#' Tres atributos del plan declaran lo que el plan no cubre.
#' `cobertura_diagnosticos` trae los diagnósticos que el perfil no pudo
#' evaluar, para que leer tres acciones no se confunda con "lo demás está
#' bien". `hallazgos_sin_accion_por_columna_ambigua` trae los hallazgos medidos
#' que no produjeron acción porque su columna no se puede identificar: comparte
#' nombre con otra y no hay forma de saber sobre cuál actuaría la limpieza —el
#' remedio es normalizar los nombres y volver a perfilar—, o no está entre las
#' columnas del perfil, o no tiene nombre —su nombre es `NA`; una columna
#' llamada con el texto `"NA"` sí lo tiene, y el plan actúa sobre ella—; el
#' `motivo` dice cuál. Un hallazgo de varias columnas —una relación aritmética en
#' `neto,iva,total`, una de orden— no es una columna ambigua: lo es sólo si
#' alguna de sus columnas lo es. Sus columnas se leen de la trazabilidad del
#' hallazgo y no del texto de `columna`: una columna llamada `a,b` no se confunde
#' con la relación entre `a` y `b`. `hallazgos_sin_accion`
#' trae el resto: los hallazgos que el perfil midió, de una columna
#' identificada, de varias o de la tabla, y que ninguna acción del plan atiende
#' —porque el plan no tiene una estrategia para ese tipo, o porque la que tiene
#' necesitaba una condición que no se cumplió—. Su `motivo` dice si la misma
#' columna (o esas columnas) recibió otras acciones, sin afirmar que alguna
#' cubra el hallazgo: eso es una decisión del dominio. Un hallazgo `ok` no
#' espera acción y no entra a ninguno de los dos. Los tres se imprimen con el
#' plan.
#'
#' `convertir_numero_regional` sólo se recomienda si todos los valores
#' presentes comparten convención decimal, unidad y moneda. En una columna que
#' el perfil infiere como `identificador` (`101a`, `102a`) la acción queda
#' `lista` sin activar: lo que acompaña al número puede ser parte del código.
#' Un valor con `%` se divide por 100, la escala con que el paquete publica
#' toda proporción; queda en `[0, 1]` si el porcentaje estaba entre 0 y 100, y
#' el plan no comprueba ese rango. Una unidad, o una moneda delante o detrás
#' del número (`$ 35`, `1.234,56 $`), dejan de formar parte del valor y quedan
#' en `parametros`. La justificación lo dice
#' cuando ocurre. Una columna que mezcla `5` con `5 %` no se convierte: habría
#' que decidir si el número sin `%` está en la misma escala. Esta conversión no
#' lee la notación científica (`1e5`): si la columna la trae junto a números
#' con formato regional, la acción queda `bloqueada`. `convertir_tipo` lee cada
#' texto con la convención decimal de la columna, como el resumen del perfil:
#' con coma decimal, `12.500` se convierte en doce mil quinientos.
#' La acción de codificación prueba las tablas congeladas de varias
#' codificaciones y deja en `estado_reparacion` uno de `reparado`,
#' `reparado_parcialmente` o `no_se_pudo`: el de lo que la acción, tal como sale
#' del plan, va a cambiar. Una reparación parcial —una celda que el motor repara
#' sólo en parte— no se aplica automáticamente, pero tampoco frena la de las
#' celdas que se reparan enteras. En una columna sin parciales, la acción sale
#' activa —si nada más lo impide— y repara todas las celdas que el motor
#' repara. En una columna con parciales y con celdas que se reparan enteras, la
#' acción también sale activa, con `aplicar_parciales = FALSE` en
#' `parametros`: repara las enteras, deja las
#' parciales como están, `n_afectadas` cuenta sólo las enteras y la
#' justificación dice cuántas parciales quedan; para aplicarlas también, se pone
#' `aplicar_parciales = TRUE` en `parametros`. En una columna que sólo tiene
#' parciales, la acción queda `lista` con `aplicar = FALSE` y
#' `aplicar_parciales = TRUE`, y se selecciona de forma explícita marcando
#' `aplicar = TRUE`; entonces se aplican las parciales. Una celda sin
#' reparación local —con el carácter de reemplazo U+FFFD, o que el motor no
#' pudo reparar— no frena la de las demás: queda como está y la justificación
#' de la acción la cuenta. La estrategia se llama `reparar_codificacion` y no
#' limita el motor a latin-1. Un texto legítimo que se escribe con los mismos
#' bytes que un mojibake —por ejemplo, uno que cita literalmente la `Ã` seguida
#' de `©`— no se distingue de uno roto: se acusa, la evidencia del plan lo
#' muestra antes de aplicar, y si se aplica la acción se reescribe como la letra
#' que esos bytes codifican. Si la columna contiene texto así, no active la
#' acción.
#' Unos mismos bytes pueden parecer un control: el segundo byte de una «Á» mal
#' leída es un C1, el de una «í» un guion blando, y `c2 80` es la huella de un
#' euro de Windows-1252 leído como latin-1. Lo que la reparación reemplaza no
#' cuenta como carácter invisible y ninguna otra acción del plan lo toca:
#' aunque se desmarque `reparar_codificacion`, la secuencia queda entera y el
#' perfil siguiente la sigue encontrando. Lo mismo si el motor no puede reparar
#' la celda: sus letras rotas quedan como están. Los controles que la
#' reparación deja como están, como un U+0081 suelto o un guion blando fuera de
#' una letra rota, se diagnostican y se quitan, también en una celda con
#' mojibake, salvo que quitarlos vuelva reparable la celda: entonces la acción
#' la deja como está, para que el resultado no dependa de si corre antes o
#' después de `reparar_codificacion`. El perfil cuenta un invisible entre los
#' que se pueden eliminar (`n_invisibles_eliminables`) sólo donde la acción
#' puede quitarlo; el control que la acción deja así sigue contando en
#' `n_controles_invisibles`, y si es el único el hallazgo dice que ninguna
#' acción del plan lo quita, en vez de recomendar quitarlo. La reparación
#' devuelve el texto que codificaban los bytes, y si ese texto trae un
#' invisible —el mojibake de un U+200B se repara a un U+200B— el plan no lo
#' quita en la misma pasada: el perfil describe los datos que recibió. El perfil
#' del resultado lo señala, y su plan lo quita.
#' Una celda cuyo texto no se puede leer —declarada `bytes` con bytes que no
#' son UTF-8, o sin marca en una sesión UTF-8, que es lo que deja [read.csv()]
#' sin `fileEncoding` sobre un archivo latin1— no se transforma: el perfil no la
#' midió y la informa como `codificacion_invalida`, cuyo remedio es volver a leer
#' la fuente declarando su codificación. Las acciones de texto trabajan sobre las
#' demás celdas de la columna y cuentan sólo lo que cambiaron. Una celda sin
#' marca cuyos bytes son UTF-8 válido se trata como UTF-8 en cualquier locale,
#' igual que en el perfil: con `LC_CTYPE=C` las acciones hacen lo mismo que en
#' una sesión UTF-8.
#'
#' La protección de las acciones de texto se decide por una propiedad común,
#' no por una lista de estrategias: en cada celda que el perfil señala como
#' rota y reparable se comparan los dos órdenes de reparación y acción. Si no
#' conmutan, la celda conserva su valor original y no cuenta entre las
#' cambiadas. La comprobación se ejecuta sólo sobre las celdas que la acción
#' iba a cambiar. El perfil cuenta con la misma regla: `n_afectadas` es lo que
#' la acción sola puede cambiar, y el plan no la ofrece por las celdas que la
#' protección le reserva (el espacio del borde que deja reconocer una letra
#' rota no cuenta como `espacios_sobrantes`). La excepción es `entidades_html`:
#' la entidad se informa aunque esté junto a una letra rota;
#' `decodificar_entidades_html`, que no viene marcada, la decodifica después de
#' la reparación y, sola, deja esa celda como está.
#'
#' Una celda `latin1` **sí** se transforma —ahí R conoce la codificación y la
#' convierte sin pérdida—, y el resultado queda marcado `UTF-8`. Como la acción
#' toca sólo las celdas que cambia, una columna `latin1` puede quedar con marcas
#' **mixtas**: las celdas transformadas en `UTF-8` y las intactas en `latin1`. El
#' valor es el mismo, pero los bytes no, así que el registro lo cuenta en
#' `n_codificacion_normalizada` en vez de dejarlo implícito. Ese conteo es de
#' recodificaciones, no de marcas: una celda sin marca cuyos bytes son UTF-8
#' válido ya es UTF-8 y no cambia de codificación aunque la acción la devuelva
#' marcada, y un texto ASCII, el de entrada o el que queda, se escribe igual en
#' toda codificación —decodificar `Jos&eacute;` o recortar un espacio duro del
#' borde cambia el valor, no la codificación—. No se convierte la
#' columna entera a propósito: eso tocaría celdas que el plan no declaró como
#' cambiadas y haría mentir a `n_cambiadas` por el otro lado.
#' Si se marca una acción que no está `lista`, `aplicar()` aborta antes de
#' modificar la copia y enumera las filas problemáticas. Una acción que sí está
#' lista pero falla se registra con su error y no impide aplicar las siguientes:
#' cada una conserva atomicidad sobre su propia columna o tabla. Una acción de
#' nombres preparada para otros nombres —un perfil de otra versión de la tabla,
#' con dos `x` donde los datos tienen una— es una de esas: queda `fallida` con el
#' motivo, y en un plan armado con `datos` ya sale `bloqueada`. Las acciones
#' que efectivamente eliminan filas o columnas requieren además
#' `permitir_eliminacion = TRUE`; una conversión `destructiva` requiere selección
#' explícita y deja la pérdida cuantificada. Por defecto, el resultado conserva
#' lo retirado en `eliminados`; use `conservar_eliminados = FALSE` para evitar
#' ese costo de memoria.
#'
#' Los hallazgos `controles_invisibles`, `entidades_html` y `separadores_en_campo`
#' tienen acciones separadas. La detección de invisibles informa tanto los
#' caracteres que se pueden normalizar como los que pueden ser parte del texto
#' —ZWJ/ZWNJ y las etiquetas Unicode que arman banderas—; la normalización actúa
#' sobre un conjunto más pequeño que la detección. La acción
#' `eliminar_controles_invisibles` quita controles C0/C1 que no son separadores
#' y los invisibles Unicode de transporte —también los operadores invisibles,
#' los rellenos del hangul y la etiqueta de idioma U+E0001—, y se recomienda por
#' defecto; conserva ZWJ/ZWNJ y las etiquetas de bandera.
#' `normalizar_espacios_invisibles` colapsa espacios Unicode (incluido
#' NBSP) a un espacio ASCII y, como su hermana, se recomienda y se activa por
#' defecto: queda un espacio, así que el valor sigue ahí; si al colapsarlo dos
#' valores que eran distintos quedan iguales, el registro lo cuenta en
#' `n_no_reversibles`. Corre antes que `recortar_espacios`, así que el espacio
#' común que deja en el borde lo quita el recorte. Para `recortar_espacios` y
#' para `n_espacios_borde`, un espacio al borde es todo blanco ASCII —espacio,
#' tabulación, salto de línea, tabulación vertical, avance de página y retorno—
#' y todo separador de Unicode, como el espacio duro (U+00A0) o el fino
#' (U+202F): los mismos que recortan los validadores. El espacio duro que es la
#' mitad de una letra mal convertida no se cuenta ni se recorta.
#' `decodificar_entidades_html` cubre las entidades
#' con nombre comunes en español y referencias numéricas válidas, pero no se
#' activa sola porque un ampersand puede ser contenido legítimo. Decodifica
#' **una capa**: un texto codificado dos veces —`&amp;amp;`— queda en `&amp;`,
#' y el perfil del resultado lo vuelve a señalar. No se itera hasta el fondo
#' porque no se puede distinguir una doble codificación de un texto que quería
#' decir `&amp;`: la segunda capa se decide viéndola.
#' `reemplazar_separadores` convierte tabulaciones, saltos de línea, avances de
#' página y tabulaciones verticales (`\\t`, `\\n`, `\\r`, `\\r\\n`, `\\f` y `\\v`)
#' en un espacio y también requiere una decisión explícita. Son exactamente los
#' códigos 9 a 13 que cuenta el hallazgo: los separadores Unicode U+2028 y
#' U+2029 son de `normalizar_espacios_invisibles` y el control U+0085 de
#' `eliminar_controles_invisibles`, y sobre celdas `bytes` la acción no toca
#' ningún byte de un carácter multibyte. Las tres acciones
#' registran el número de valores cambiados. Una comparación aproximada con
#' `normalizar = TRUE` usa estas mismas clases: colapsa espacios y omite basura
#' de transporte, pero conserva ZWJ/ZWNJ y las etiquetas.
#'
#' Las imputaciones por dependencia funcional se ofrecen desactivadas. Aunque
#' una dependencia exacta permite deducir un valor sin usar media, moda o un
#' modelo externo, sigue siendo una regularidad aprendida de una sola entrega y
#' puede reflejar un error sistemático en vez de una regla de negocio. El plan
#' conserva el mapa y su soporte para que el usuario la confirme; sólo entonces
#' se aplica y se vuelve a validar contra los datos recibidos. Si la protección
#' enmascaró alguna celda del mapa —una clave o un valor, también cuando ninguna
#' de las dos columnas es personal pero una celda contiene un valor protegido,
#' como la sucursal que se llama igual que un titular—, queda marcado
#' `mapa_enmascarado = TRUE` y se valida igual que sin protección: las filas
#' que el plan publica en claro son el mapa tal cual, y una fila tapada se
#' resuelve con el par de los datos recibidos que tiene su forma y que, tapado
#' con los valores protegidos de esos mismos datos, da lo que el plan publicó;
#' la relación de los datos se toma con el soporte declarado, sin publicar sus
#' valores. Si los datos contradicen una fila —la publicada en claro o la
#' tapada—, la acción queda `fallida`, y una clave que no está en el plan no
#' se imputa: reconstruir el mapa sobre los datos recibidos escribiría, en otra
#' entrega, valores que el plan nunca mostró. Una fila tapada no se
#' puede validar si los datos recibidos no traen el valor protegido que la
#' tapó —un subconjunto sin ese titular—: también queda `fallida`, con el
#' motivo, y se aplica planificando sobre esos datos. Y si los datos piden
#' imputar una clave cuya fila el plan publicó tapada pero no traen ningún par
#' de esa clave que la resuelva —la entrega que trae sólo las filas a
#' completar—, la acción tampoco escribe a medias: queda `fallida`, con el
#' motivo, porque sin la protección el plan escribiría esas celdas y con la
#' fila tapada no se sabe con qué. Con una regla de `aplicabilidad` sobre la columna
#' dependiente, la imputación respeta su universo como las demás acciones por
#' celda: no llena las filas declaradas fuera, que son vacío por diseño, y
#' `n_afectadas` las excluye.
#'
#' Cuando el determinante o el dependiente es una columna protegida, se tapa por
#' construcción todo ese lado del mapa: el texto usa `[valor protegido]` y las
#' columnas numéricas, `Date` y `POSIXct` usan `NA`. El lado no protegido conserva
#' el resultado de los barridos generales, incluido el piso por contenido. Los
#' demás parámetros de una acción sobre una columna protegida tampoco publican
#' objetos de clase `Date` o `POSIXct`.
#'
#' Con un lado entero tapado, todas sus filas quedan tapadas y `aplicar()` sólo
#' puede resolverlas con datos que traigan la relación y el soporte declarado.
#' Una entrega parcial —las filas a completar y un par de cada clave— queda
#' `fallida`, con el motivo de que los datos no traen un par de esa clave que la
#' resuelva. Una
#' contradicción en una clave que no hay que completar —una fila con el
#' dependiente presente que cambió de valor— no se detecta: el plan no publica
#' esa clave para compararla, la acción sale `ejecutada` y lo que imputa es
#' correcto sobre los datos recibidos. Es una limitación de esta representación
#' tapada.
#'
#' Sobre una columna protegida, `convertir_sentinelas_numericos` publica el
#' catálogo de centinelas del paquete tal cual —es vocabulario, no un dato— y
#' tapa el resto. Lo tapado se marca con `valores_enmascarados = TRUE` y
#' `aplicar()` lo resuelve sobre los datos que recibe: son los rellenos de la
#' columna, el dígito repetido que ocupa el lugar de un documento. El plan
#' ofrece la acción así sólo si esa resolución convierte, sobre los datos del
#' plan, exactamente las celdas que estima; si no, la acción queda `bloqueada`
#' y la justificación dice cómo ejecutarla. Lo mismo en una columna que no es
#' personal cuando el centinela declarado aparece en una protegida —el
#' `99999999` que se repite en la cédula de una tabla de movimientos—: no se
#' publica, se declara y se resuelve con los rellenos de esa columna, o la
#' acción queda `bloqueada`. Un centinela declarado que no aparece en ninguna
#' protegida se publica: es la declaración del usuario.
#'
#' `marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada` reproduce la
#' semántica de [duplicated()] y marca sólo las apariciones posteriores;
#' `.grupo_duplicado` identifica a **todas** las filas que participan en cada
#' grupo de contenido idéntico. Las acciones de duplicados comparan las filas
#' con la misma igualdad que el perfil, también con columnas `integer64`,
#' matrices, `data.frame` anidados o `POSIXlt`; una tabla con columnas de lista
#' deja las acciones bloqueadas. Las de columnas duplicadas comparan el
#' contenido como el perfil: un factor con nivel `NA` y el mismo factor sin ese
#' nivel, un `POSIXlt` y su `POSIXct`, o dos factores sin orden con los mismos
#' niveles en otro orden, son la misma columna también al aplicar. Si alguna
#' columna del grupo se llama como otra de la tabla, no se propone acción: el
#' hallazgo queda en `hallazgos_sin_accion_por_columna_ambigua`, con el
#' remedio. Hay una decisión por grupo de columnas iguales, no por par:
#' marcarlo, o eliminar las repetidas y conservar la primera del grupo.
#' `columna_1` nombra la que se conserva, y `columna_2` y `eliminar`, todas las
#' repetidas; el atributo `columnas_duplicadas_marcadas` lleva una fila por
#' repetida, junto a la primera. Marcar no elimina filas. Con las dos columnas
#' incluidas, las marcas distinguen la primera aparición de las repetidas, pero
#' no a las repetidas entre sí: en un grupo de tres o más filas un perfil
#' posterior sigue contando duplicadas. Para saber si los duplicados siguen en
#' la tabla hay que quitar las columnas de marca antes de perfilarla.
#'
#' Al volver a planificar sobre lo aplicado, el plan no propone acciones sobre
#' las marcas que agrega —`.fila_duplicada`, `.grupo_duplicado`,
#' `.ausente_<columna>` y `.outlier_<columna>`—: sus hallazgos quedan en
#' `hallazgos_sin_accion` con ese motivo, porque el `NA` de `.grupo_duplicado`
#' significa que la fila no participa de ningún grupo y no una ausencia. Una
#' acción cuya columna de marca ya existe en los datos queda `bloqueada`:
#' `aplicar()` no puede volver a crearla.
#'
#' El orden operativo se aparta deliberadamente de la secuencia dimensional
#' frescura–completitud–exactitud–consistencia–unicidad sugerida por el marco.
#' Primero marca duplicados sin borrar, luego normaliza ausencias y texto, y
#' deja los cambios de esquema para el final. Esto evita perder la evidencia
#' original, permite imputar antes de convertir tipos y mantiene identificables
#' las columnas durante todo el plan. Las eliminaciones nunca se activan por
#' defecto, por lo que deduplicar temprano no puede hacer desaparecer registros.
#' `destructiva` también marca una conversión que pierde representación, aunque
#' no elimine filas o columnas. El consentimiento `permitir_eliminacion` sólo
#' se exige para las estrategias que efectivamente retiran filas o columnas;
#' una conversión destructiva requiere que el usuario la active explícitamente
#' y deja su pérdida cuantificada en el registro.
#'
#' **Marcar o eliminar ausentes va después de normalizarlos**, no antes. Una
#' columna con `"N/A"` y `"sin dato"` tiene ausentes disfrazados que
#' `convertir_ausencias_textuales` convierte en `NA` reales; si la marca
#' corriera primero, quedaría una columna `.ausente_x` que dice `FALSE` en
#' filas que terminan en `NA` —una afirmación falsa sobre los datos, publicada
#' con las dos acciones en `ejecutada` y sin error—. El plan ordena la marca
#' después de la conversión, así que el idioma documentado
#' `plan$aplicar <- plan$recomendada` no produce esa marca falsa. Por la misma
#' razón, `eliminar_filas_ausentes` también va después: eliminar antes deja sin
#' eliminar las filas cuyo ausente todavía estaba disfrazado.
#'
#' `convertir_ausencias_textuales` convierte exactamente los textos de
#' `parametros$valores`, comparados sin mayúsculas ni espacios al borde. El
#' plan arma la lista con los marcadores que el perfil detectó y los que se
#' declararon en `cadenas_ausencia`; editarla es una decisión del usuario:
#' quitar un texto lo conserva y agregar uno lo convierte, como si se hubiera
#' declarado. Con protección, la lista y la declaración guardan su valor
#' también sobre una columna protegida: un marcador del catálogo —`sin dato`—
#' o uno declarado —`SIN REGISTRO`— no es el dato de nadie, y la declaración ya
#' se publica en `meta$cadenas_ausencia`. Tapados, saldrían `[valor protegido]`
#' y `aplicar()` registraría `fallida` la acción recomendada.
#'
#' `convertir_sentinelas_numericos` convierte los números de
#' `parametros$valores`. El plan arma la lista con los centinelas que el perfil
#' contó en esa columna —los que la traza del hallazgo publica en
#' `sentinelas_contados`— y con los que se declararon en `sentinelas_numericos`:
#' un `999` dentro de una numeración de 1 a 2000 no es un centinela para
#' [perfilar()], y la acción no lo convierte. Así, aplicada a los datos del
#' perfil, cambia exactamente las celdas que `n_afectadas` anuncia. Una celda
#' que el perfil contó como marca textual declarada en `cadenas_ausencia` —un
#' `"999"` con `cadenas_ausencia = "999"`— es de `convertir_ausencias_textuales`,
#' y esta acción no la toca aunque se lea como uno de sus números. La acción
#' trabaja sobre números, texto, factores, duraciones (`difftime`) y medidas con
#' unidad (`units`), y conserva la clase y la unidad de la columna. Editar la
#' lista es una decisión del usuario, como en la acción de las ausencias
#' textuales.
#'
#' Sobre otros datos —otra entrega de la misma tabla— la lista manda: la acción
#' convierte esos valores y ningún otro. Si la otra entrega usa otro código, la
#' acción termina `fallida` sin cambiar celdas, y hay que volver a perfilar o
#' agregar el código a `valores`.
#'
#' En dos casos el plan no sabe qué centinelas contó el perfil: un perfil
#' guardado por una versión anterior, cuya traza no trae el campo, y un
#' hallazgo cuya evidencia tapó la protección de datos personales, porque el
#' perfil no publica ahí qué valor contó. Entonces `valores` es la lista entera
#' de centinelas del perfil, la justificación lo dice, y la acción lleva en
#' `parametros$guarda` el contexto con que el perfil decidió —el tipo inferido,
#' la convención decimal y las banderas de la numeración—. Al aplicar, convierte
#' sólo los centinelas de esa lista que el mismo detector cuenta en los datos
#' que recibe, que pueden ser menos, y nunca un valor que no esté en `valores`;
#' lo que el usuario agregue a `valores` se convierte sin acotar. Sobre los
#' datos del perfil convierte lo contado. Sobre otra entrega decide con esos
#' datos, y puede convertir un centinela de la lista que el perfil de origen no
#' había contado: es el límite de estos dos casos. Si el perfil contó dentro de
#' un universo declarado en `aplicabilidad`, la acción necesita la regla que el
#' plan le copia para acotar: sin ella termina `fallida` y dice por qué.
#'
#' @param perfil Objeto de clase `perfil` creado por [perfilar()].
#' @param datos Datos opcionales que originaron el perfil. Son necesarios para
#'   comprobar la reversibilidad de conversiones sobre todos los valores y para
#'   proponer imputaciones deducidas de dependencias funcionales.
#' @param soporte_minimo_dependencia Cantidad mínima de observaciones
#'   concordantes por valor determinante para proponer una imputación.
#' @param plan Objeto de clase `plan_limpieza` o data frame con el mismo
#'   contrato. Puede filtrarse y editarse antes de aplicarlo; una acción cuyos
#'   parámetros no son la lista con nombres que propone el plan —un vector,
#'   `NULL`— queda `fallida` con ese motivo.
#' @param datos `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el
#'   plan. El objeto recibido no se modifica.
#'
#' @return `planificar_limpieza()` devuelve un data frame de clase
#'   `plan_limpieza`. `aplicar()` devuelve una lista de clase
#'   `resultado_limpieza` con `datos`, `registro`, `plan_aplicado`, el `plan`
#'   sincronizado y `eliminados`. El `registro` conserva `estado` (`ejecutada`
#'   o `fallida`), `error`, `n_no_reversibles` y la `justificacion` de cada
#'   acción seleccionada, incluso cuando una falla y las siguientes continúan.
#'   `n_codificacion_normalizada` cuenta, por acción, las celdas cuya **marca de
#'   codificación** cambió: es el caso de una acción de texto sobre celdas
#'   `latin1`, cuyo valor se conserva y cuyos bytes no. No cuenta una celda sin
#'   marca cuyos bytes ya eran UTF-8 válido ni una celda que es o queda ASCII,
#'   así que no depende del locale.
#'   `n_no_reversibles` cuenta las celdas cuyo VALOR se perdió y no se puede
#'   recuperar desde el resultado: un centinela `-999` que pasa a ausencia, un
#'   extremo recortado a su límite, un marcador de ausencia convertido, o un
#'   número que se redondeó al convertirlo. Las acciones que **normalizan la
#'   escritura** —recortar espacios, quitar un invisible de transporte,
#'   reemplazar separadores o cambiar mayúsculas— dejan el valor en su lugar y
#'   no cuentan, **salvo cuando la normalización fusiona valores que eran
#'   distintos**: ahí lo que los separaba no queda en ningún lado y esas celdas
#'   sí se cuentan. El criterio se mide sobre el resultado, no por el nombre de
#'   la acción: quitar un guion suave de `PRO<U+00AD>DUCTO-A` no pierde nada,
#'   y quitar un espacio de ancho cero que distinguía dos claves fusiona las
#'   dos.
#'   Si una acción seleccionada no produce ningún efecto, se registra como
#'   `fallida` con el motivo y su copia no se incorpora al resultado. La
#'   comprobación del efecto observa `n_cambiadas` aunque `n_afectadas` sea
#'   `NA` o haya sido editado: una acción seleccionada que no cambia nada queda
#'   `fallida`, porque la ausencia de efecto es observable sin depender de la
#'   estimación.
#'   Si una columna de entrada es un factor, las acciones que transforman su
#'   texto devuelven una columna `character`: no se reconstruyen los niveles
#'   originales, porque una limpieza puede introducir valores nuevos.
#'   Los atributos de cada columna —la etiqueta de variable que deja `haven`—
#'   sobreviven a las acciones que la reescriben, a las conversiones de tipo y a
#'   las que quitan filas; quitar filas de un `data.table` conserva su clave.
#' @export
#' @seealso [perfilar()], [guiar_limpieza()], [detectar_dependencias()]
#'
#' @examples
#' datos <- data.frame(categoria = c(" A", "S/D", "B"))
#' perfil <- perfilar(datos)
#' plan <- planificar_limpieza(perfil, datos)
#' plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
#' resultado <- aplicar(plan, datos)
#' resultado$datos
planificar_limpieza <- function(perfil, datos = NULL,
                                soporte_minimo_dependencia = 2L) {
  if (!inherits(perfil, "perfil")) {
    .detener("`perfil` debe ser un objeto de clase perfil.", call. = FALSE)
  }
  if (!is.null(datos) && !inherits(datos, "data.frame")) {
    .detener("`datos` debe ser NULL o heredar de data.frame.", call. = FALSE)
  }
  if (!is.null(datos)) datos <- .tabla_base(datos)
  if (length(soporte_minimo_dependencia) != 1L ||
      is.na(soporte_minimo_dependencia) ||
      !is.finite(soporte_minimo_dependencia) ||
      soporte_minimo_dependencia < 1L ||
      soporte_minimo_dependencia != floor(soporte_minimo_dependencia)) {
    .detener("`soporte_minimo_dependencia` debe ser un entero positivo.",
         call. = FALSE)
  }
  acciones <- list()
  hallazgos <- perfil$hallazgos
  nombres_tabla <- if (!is.null(datos)) {
    names(datos)
  } else {
    as.character(perfil$columnas$columna)
  }
  marcas_plan <- nombres_tabla[.marcas_del_plan(nombres_tabla)]
  # Una columna declarada en `columnas_codigo` es un codigo y no una cantidad: no
  # recibe acciones que la conviertan en numero. El perfil ya no publica esos
  # hallazgos sobre un codigo declarado; la guarda vale tambien para un perfil
  # calculado antes, que si los traia.
  codigos_declarados <- .nombres_para_operar(as.character(
    perfil$meta[["columnas_codigo"]]
  ))
  convierte_a_numero <- function(tipo, columna) {
    if (is.na(columna) || !length(codigos_declarados) ||
        !.nombres_para_operar(columna) %in% codigos_declarados) {
      return(FALSE)
    }
    if (tipo %in% c("numero_como_texto", "unidades_mixtas", "monedas_mixtas")) {
      return(TRUE)
    }
    if (!identical(tipo, "tipo_declarado_distinto")) return(FALSE)
    fila <- .fila_perfil(perfil, columna)
    !is.null(fila) &&
      as.character(fila$tipo_inferido[[1L]]) %in% c("entero", "doble", "integer64")
  }

  # Los grupos de columnas iguales se leen una vez: buscarlos por hallazgo
  # recorria todos los pares en cada uno, y con 1.000 columnas iguales
  # planificar se estimaba en un dia.
  # Un perfil de una version anterior trae un hallazgo por par: todos los de un
  # grupo caen en el mismo, que se planifica en el primero. Los demas se saltean
  # antes de extraer la fila, porque con 1.000 columnas iguales son 499.500.
  grupos_duplicadas <- .grupos_columnas_duplicadas_perfil(perfil)
  claves_columnas_perfil <- .nombres_para_operar(as.character(perfil$columnas$columna))
  grupo_de_hallazgo <- rep(NA_integer_, nrow(hallazgos))
  filas_duplicadas_h <- which(
    as.character(hallazgos$tipo_hallazgo) == "columnas_duplicadas"
  )
  if (length(filas_duplicadas_h) && length(grupos_duplicadas$grupos)) {
    grupo_de_hallazgo[filas_duplicadas_h] <- grupos_duplicadas$grupo[match(
      .nombres_para_operar(as.character(hallazgos$columna[filas_duplicadas_h])),
      grupos_duplicadas$claves
    )]
  }
  grupo_ya_planificado <- !is.na(grupo_de_hallazgo) &
    duplicated(grupo_de_hallazgo)

  for (i in seq_len(nrow(hallazgos))) {
    if (grupo_ya_planificado[[i]]) next
    hallazgo <- hallazgos[i, , drop = FALSE]
    tipo <- hallazgo$tipo_hallazgo[[1L]]
    columna <- hallazgo$columna[[1L]]
    # Las marcas que agrega el propio plan no son datos que limpiar: ver
    # `.marcas_del_plan()`. El hallazgo queda declarado en `hallazgos_sin_accion`.
    if (!is.na(columna) &&
        .nombres_para_operar(columna) %in% .nombres_para_operar(marcas_plan)) {
      next
    }
    if (convierte_a_numero(tipo, columna)) next
    grupo_hallazgo <- .id_grupo(i)
    fila <- if (!is.na(columna)) .fila_perfil(perfil, columna) else NULL
    estado_columna <- if (!is.na(columna)) {
      .estado_columna(perfil, columna)
    } else {
      "lista"
    }

    if (identical(tipo, "faltantes") && !is.null(fila) &&
        fila$n_faltantes[[1L]] > 0L) {
      nombre_marca <- paste0(".ausente_", .nombres_make_names(columna))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_filas_ausentes", TRUE,
        paste0(
          "La marca conserva los registros y permite revisar los ausentes ",
          "antes de decidir si corresponde excluirlos."
        ), fila$n_faltantes[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        # Despues de las conversiones que CREAN ausentes -textuales en 100,
        # centinelas numericos en 110-. Marcar antes producia una columna que
        # afirma algo falso: medido sobre siete formas de tabla, en tres de
        # ellas la marca decia FALSE en filas que quedaban NA, y las dos
        # acciones se registraban como ejecutadas sin error. Con el idioma
        # documentado -`plan$aplicar <- plan$recomendada`- alcanzaba para que
        # `.ausente_t` negara dos de los tres ausentes.
        parametros = list(columna_marca = nombre_marca), orden = 115L,
        grupo = grupo_hallazgo, decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_filas_ausentes", FALSE,
        paste0(
          "Eliminar registros puede excluir personas o hechos del an\u00e1lisis; ",
          "s\u00f3lo corresponde cuando el dominio confirma que el ausente invalida la fila."
        ), fila$n_faltantes[[1L]], FALSE, estado = estado_columna,
        # Por la misma razon que la marca: eliminar antes de normalizar deja
        # sin eliminar las filas cuyo ausente todavia estaba disfrazado.
        aplicar = FALSE, orden = 120L, grupo = grupo_hallazgo,
        decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_filas_ausentes",
        destructiva = TRUE
      ))
    } else if (identical(tipo, "faltantes_disfrazados") && !is.null(fila)) {
      n_textuales <- fila$n_faltantes_disfrazados_textuales[[1L]]
      n_numericos <- fila$n_faltantes_disfrazados_numericos[[1L]]
      if (n_textuales > 0L) {
        # La frase decia "pueden normalizarse SIN INFERIR EL DOMINIO", y eso
        # es falso justo donde importa: decidir que `NA` es una ausencia y no
        # el codigo de Namibia ES inferir el dominio. El catalogo reconoce el
        # token, no el significado que tiene en esta columna, y el cambio no
        # se puede deshacer. Se dice lo que el paquete sabe y se nombra la
        # comprobacion que solo puede hacer quien conoce los datos.
        justificacion <- paste0(
          "Las representaciones textuales del cat\u00e1logo son marcadores ",
          "habituales de ausencia. El cambio no es reversible: confirmar que ",
          "ninguno de esos textos sea un valor leg\u00edtimo de la columna ",
          "-`NA` es el c\u00f3digo de Namibia, `NULL` puede ser un apellido-."
        )
        declarados <- perfil$meta$cadenas_ausencia
        leidos <- .marcadores_de_ausencia(
          if (!is.null(datos) && columna %in% names(datos)) datos[[columna]] else NULL,
          hallazgo$evidencia[[1L]], n_textuales, declarados
        )
        ambiguos <- .marcadores_ambiguos(leidos$marcadores, declarados)
        # Sin los datos y con la evidencia recortada, no se puede saber si hay
        # un marcador ambiguo fuera de la vista: se pide confirmar.
        confirmar <- length(ambiguos) > 0L || !isTRUE(leidos$completa)
        if (confirmar) {
          justificacion <- if (length(ambiguos)) {
            paste0(
              "Uno de los textos detectados (", paste(ambiguos, collapse = ", "),
              ") podr\u00eda ser un valor leg\u00edtimo de la columna: `NA` es el ",
              "c\u00f3digo de Namibia, `NULL` puede ser un apellido. El cambio no ",
              "es reversible y el paquete no puede decidirlo, as\u00ed que la ",
              "acci\u00f3n queda para activar a mano."
            )
          } else {
            paste0(
              "La evidencia muestra s\u00f3lo los marcadores m\u00e1s frecuentes y ",
              "no se recibieron los datos para ver el resto: puede haber uno que ",
              "sea un valor leg\u00edtimo de la columna. El cambio no es ",
              "reversible, as\u00ed que la acci\u00f3n queda para activar a mano."
            )
          }
        }
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_ausencias_textuales", !confirmar,
          justificacion, n_textuales, FALSE,
          estado = estado_columna,
          # `aplicar` se calcula aparte de `recomendada`, asi que no alcanza
          # con no recomendarla: sin esto la accion se autoaplicaba igual.
          aplicar = !confirmar && identical(estado_columna, "lista"),
          # La documentacion del paquete dice que una accion destructiva
          # requiere que el usuario la active explicitamente. Esto es
          # exactamente eso.
          destructiva = confirmar,
          # La lista acota a lo OBSERVADO, no al catalogo. Con el catalogo
          # completo, un plan hecho sobre una entrega y aplicado a otra
          # convertia marcadores que nunca aparecieron -un `NULL` que puede ser
          # un apellido- sin pasar por la guarda de ambiguedad, que corre al
          # planificar. Lo declarado se suma porque el usuario lo afirmo.
          parametros = list(
            valores = unique(c(leidos$marcadores,
                               .minusculas_para_comparar(declarados))),
            cadenas_ausencia = declarados
          ), orden = 100L
        ))
      }
      if (n_numericos > 0L) {
        sentinelas <- perfil$meta$sentinelas_numericos
        if (is.null(sentinelas)) sentinelas <- .numeros_na()
        justificacion <- paste0(
          "Un sentinela num\u00e9rico tambi\u00e9n puede ser un valor leg\u00edtimo; ",
          "requiere confirmar el diccionario del campo."
        )
        # La convencion decimal de la columna viaja con la accion: el detector
        # conto `"-999,00"` como -999 en una columna `decimal_coma`, y sin ella
        # la accion no lo convertia.
        convencion <- if ("numero_texto_convencion" %in% names(fila)) {
          as.character(fila$numero_texto_convencion[[1L]])
        } else ""
        # La lista acota a lo que el perfil conto en ESTA columna, no al
        # catalogo. Lo dice la traza del hallazgo (`sentinelas_contados`), que
        # sale de la misma mascara que el conteo: con la lista entera,
        # `aplicar()` convertia tambien el `999` legitimo de una numeracion.
        # Lo que el usuario declaro se suma, porque atraviesa la guarda del
        # detector entera.
        #
        # La traza no lo trae en dos casos: un perfil guardado por una version
        # anterior, y un hallazgo cuya evidencia la proteccion tapo -ahi el
        # perfil no publica que valor conto-. En el primero todavia se puede
        # leer de la evidencia, si nombra todo lo contado
        # (`.sentinelas_contados_en_evidencia()`); con un perfil protegido no,
        # porque la lista lleva tapados los centinelas declarados que son
        # valores de una columna protegida -un `NA` en su lugar- y achicarla
        # los perdia. La proteccion se lee de `meta`, no de la clasificacion:
        # una columna clasificada como personal figura para proteger aunque la
        # proteccion este apagada. Si tampoco se puede, la lista queda entera
        # y la accion lleva la guarda con que `aplicar()` la acota sobre los
        # datos que recibe: ver `.guarda_sentinelas_del_perfil()`.
        contados <- .sentinelas_contados_de_traza(hallazgo)
        if (is.null(contados) &&
            identical(perfil$meta$proteger_datos_personales, FALSE) &&
            !anyNA(sentinelas)) {
          contados <- .sentinelas_contados_en_evidencia(
            hallazgo$evidencia[[1L]], n_numericos, convencion,
            textuales = perfil$meta$cadenas_ausencia
          )
        }
        guarda_sentinelas <- NULL
        if (!is.null(contados)) {
          sentinelas <- .normalizar_sentinelas_numericos(c(
            contados, .sentinelas_numericos_declarados(sentinelas)
          ))
        } else {
          guarda_sentinelas <- .guarda_sentinelas_del_perfil(
            perfil, columna, fila, sentinelas, convencion
          )
          justificacion <- paste0(
            justificacion, " La lista `valores` es la lista entera de ",
            "sentinelas del perfil, que no dice cu\u00e1les cont\u00f3 en esta ",
            "columna; al aplicar, la acci\u00f3n convierte s\u00f3lo los que el ",
            "detector cuenta en los datos que recibe, que pueden ser menos."
          )
        }
        parametros_sentinelas <- list(valores = sentinelas)
        if (length(convencion) == 1L && !is.na(convencion) &&
            nzchar(convencion)) {
          parametros_sentinelas$convencion <- convencion
        }
        # Las marcas textuales declaradas viajan con la accion: una celda que
        # el perfil conto como marca textual -`cadenas_ausencia = "999"`- no es
        # de esta accion aunque se lea como uno de sus numeros.
        if (length(perfil$meta$cadenas_ausencia)) {
          parametros_sentinelas$cadenas_ausencia <- perfil$meta$cadenas_ausencia
        }
        if (!is.null(guarda_sentinelas)) {
          parametros_sentinelas$guarda <- guarda_sentinelas
        }
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "convertir_sentinelas_numericos", FALSE,
          justificacion, n_numericos, FALSE,
          estado = estado_columna, aplicar = FALSE,
          parametros = parametros_sentinelas, orden = 110L,
          grupo = grupo_hallazgo, decision_grupo = "pendiente"
        ))
      }
    } else if (identical(tipo, "espacios_sobrantes") && !is.null(fila)) {
      justificacion <- paste0(
        "Los espacios al borde no aportan contenido y separan categor\u00edas ",
        "que visualmente son iguales."
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "recortar_espacios", TRUE, justificacion,
        fila$n_espacios_borde[[1L]], FALSE, estado = estado_columna,
        aplicar = identical(estado_columna, "lista"), orden = 200L
      ))
    } else if (identical(tipo, "controles_invisibles") && !is.null(fila)) {
      n_eliminables <- if ("n_invisibles_eliminables" %in% names(fila)) {
        fila$n_invisibles_eliminables[[1L]]
      } else fila$n_controles_invisibles[[1L]]
      n_espacios <- if ("n_espacios_invisibles" %in% names(fila)) {
        fila$n_espacios_invisibles[[1L]]
      } else 0L
      if (isTRUE(n_eliminables > 0L)) {
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "eliminar_controles_invisibles", TRUE,
          paste0(
            "Los controles C0/C1 y los invisibles Unicode de transporte no ",
            "aportan contenido de negocio y pueden romper cruces, ",
            "comparaciones y exportes. Los ZWJ/ZWNJ se conservan. Quitar el ",
            "car\u00e1cter no se puede deshacer: si al quitarlo dos valores que ",
            "eran distintos quedan iguales, el registro cuenta esas celdas en ",
            "`n_no_reversibles`."
          ), n_eliminables, FALSE,
          estado = estado_columna,
          aplicar = identical(estado_columna, "lista"), orden = 195L
        ))
      }
      if (isTRUE(n_espacios > 0L)) {
        # Esta accion y su hermana `eliminar_controles_invisibles` estan en la
        # MISMA rama y se trataban al reves: una se recomendaba y se activaba
        # sola, y esta era destructiva y exigia confirmacion. La asimetria no
        # tenia sostén: convertir un espacio Unicode en un espacio comun deja un
        # espacio -es un cambio de forma, no de valor-, y el paquete recomienda y
        # activa `recortar_espacios`, que QUITA espacios enteros. La regla del
        # 2026-09-22 es que solo cuenta la perdida de VALOR.
        #
        # La confirmacion tendria sentido si la accion pudiera tocar un invisible
        # SIGNIFICATIVO -un ZWJ o un ZWNJ, que cambian la forma de una palabra-,
        # y no puede: `.codigos_espacios_invisibles` no los contiene, igual que
        # `.codigos_invisibles_eliminables`. Eso lo sostiene una prueba, no este
        # comentario.
        #
        # Lo que se pierde de verdad -dos valores distintos que quedan iguales-
        # lo sigue contando `n_no_reversibles`, y medido sin colision publica 0.
        #
        # Corre ANTES que `recortar_espacios` (200) y despues de
        # `eliminar_controles_invisibles` (195). Corria despues del recorte, y
        # el espacio duro del borde que esta accion vuelve un espacio comun
        # quedaba al borde: aplicar el plan que el paquete propone dejaba
        # espacios al borde, y el perfil de lo aplicado volvia a publicar
        # `espacios_sobrantes`. En este orden el recorte los encuentra.
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, "normalizar_espacios_invisibles", TRUE,
          paste0(
            "Los espacios Unicode se convierten a un espacio com\u00fan para ",
            "comparar y exportar. Queda un espacio, as\u00ed que el valor sigue ah\u00ed ",
            "con otra forma; lo que se pierde es la distinci\u00f3n del espacio ",
            "original. No toca los invisibles significativos -ZWJ y ZWNJ-, que ",
            "no est\u00e1n en su conjunto. Si al normalizar dos valores que eran ",
            "distintos quedan iguales, el registro cuenta esas celdas en ",
            "`n_no_reversibles`."
          ), n_espacios, FALSE,
          estado = estado_columna,
          aplicar = identical(estado_columna, "lista"), orden = 198L
        ))
      }
    } else if (identical(tipo, "entidades_html") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "decodificar_entidades_html", FALSE,
        paste0(
          "Decodificar una entidad puede ser correcto para una fuente web, ",
          "pero un ampersand tambi\u00e9n puede ser contenido leg\u00edtimo."
        ), fila$n_entidades_html[[1L]], FALSE,
        estado = estado_columna, aplicar = FALSE, orden = 207L
      ))
    } else if (identical(tipo, "separadores_en_campo") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "reemplazar_separadores", FALSE,
        paste0(
          "Un salto dentro de un campo puede romper un CSV, pero tambi\u00e9n ",
          "puede ser parte leg\u00edtima de una observaci\u00f3n."
        ), fila$n_separadores_en_campo[[1L]], FALSE,
        estado = estado_columna, aplicar = FALSE, orden = 209L
      ))
    } else if (identical(tipo, "codificacion_rota") && !is.null(fila)) {
      n_reparables <- fila$n_codificacion_reparable[[1L]]
      # PARCIAL es una celda que el motor repara solo en parte, no una columna
      # que ademas de las reparables trae una celda sin reparacion local. Se
      # decidia con el estado AGREGADO de la columna, y `.ftfy_estado_agregado()`
      # da `reparado_parcialmente` en cuanto una celda con el caracter de
      # reemplazo convive con las reparadas: una sola celda ajena dejaba
      # `informativa` -y sin via para aplicarla- la reparacion de tres celdas que
      # se reparan enteras, aunque la accion ni siquiera toca la irreparable.
      # Con el conteo por celda la accion repara lo reparable y declara lo
      # irreparable en su justificacion. El estado agregado queda solo para un
      # perfil anterior que no traiga el conteo.
      n_parciales <- if ("n_codificacion_reparable_parcialmente" %in% names(fila)) {
        fila$n_codificacion_reparable_parcialmente[[1L]]
      } else NA_integer_
      parcial <- if (!is.na(n_parciales)) {
        n_parciales > 0L
      } else {
        isTRUE(fila$estado_codificacion_reparacion[[1L]] == "reparado_parcialmente")
      }
      n_parciales <- if (parcial && !is.na(n_parciales)) n_parciales else 0L
      reparable <- n_reparables > 0L || n_parciales > 0L
      # Una celda parcial no apaga la reparacion de las que el motor repara
      # enteras. Bastaba UNA parcial en la columna para que la accion saliera sin
      # marcar, y con ella ninguna celda se reparaba con el plan por omision:
      # medido sobre tablas generadas, una celda que paso de `no_se_pudo` a
      # `reparado_parcialmente` dejo sin reparar otra de la misma columna que se
      # repara entera. Una entera es igual de segura con o sin parciales al lado,
      # asi que con alguna entera la accion se recomienda y se marca como en una
      # columna sin parciales, con `aplicar_parciales = FALSE`: las parciales
      # quedan como estan y la justificacion dice como pedirlas. Solo una columna
      # sin ninguna entera deja la accion sin marcar, y marcarla aplica las
      # parciales: es la seleccion explicita que describe la ayuda.
      aplicar_parciales <- n_parciales > 0L && !(n_reparables > 0L)
      n_irreparables <- if ("n_codificacion_irreparable" %in% names(fila)) {
        fila$n_codificacion_irreparable[[1L]]
      } else 0L
      n_no_se_pudo <- if ("n_codificacion_no_se_pudo" %in% names(fila)) {
        fila$n_codificacion_no_se_pudo[[1L]]
      } else 0L
      # El estado y el conteo son los de lo que la accion, tal como sale del
      # plan, va a cambiar: `.reparar_codificacion()` publica en el registro el
      # estado de lo aplicado, y `n_afectadas` tiene que ser lo que `aplicar()`
      # cuenta en `n_cambiadas`.
      estado_reparacion <- if (reparable) {
        if (aplicar_parciales) "reparado_parcialmente" else "reparado"
      } else if ("estado_codificacion_reparacion" %in% names(fila)) {
        as.character(fila$estado_codificacion_reparacion[[1L]])
      } else "no_se_pudo"
      n_afectadas_reparacion <- n_reparables +
        if (aplicar_parciales) n_parciales else 0L
      # Lo que la accion no toca: el hallazgo cuenta cada valor afectado una vez.
      n_sin_reparacion <- max(0L, fila$n_codificacion_rota[[1L]] -
        n_reparables - n_parciales)
      recomendada <- reparable && !aplicar_parciales
      puede_aplicar <- recomendada && identical(estado_columna, "lista")
      detalle_parciales <- if (parcial) {
        conteo_parciales <- if (n_parciales > 0L) {
          paste0(" (", n_parciales, ")")
        } else ""
        if (aplicar_parciales) {
          paste0(
            " Hay reparaciones parciales", conteo_parciales,
            ": la acci\u00f3n queda sin activar y, si se activa, las aplica ",
            "tambi\u00e9n."
          )
        } else {
          paste0(
            " Hay reparaciones parciales", conteo_parciales,
            ": quedan como est\u00e1n y la acci\u00f3n repara s\u00f3lo los valores ",
            "que se reparan enteros. Para aplicarlas tambi\u00e9n, poner ",
            "`aplicar_parciales = TRUE` en `parametros`."
          )
        }
      } else ""
      detalle_sin_reparacion <- if (n_sin_reparacion > 0L) {
        motivos <- character()
        if (n_irreparables > 0L) {
          motivos <- c(motivos,
                       "los valores con el car\u00e1cter de reemplazo indican que se perdieron bytes")
        }
        if (n_no_se_pudo > 0L || !length(motivos)) {
          motivos <- c(motivos,
                       "el motor reconoce los valores mal convertidos pero no pudo repararlos")
        }
        paste0(" Los valores sin reparaci\u00f3n local quedan como est\u00e1n: ",
               paste(motivos, collapse = "; "), ".")
      } else ""
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, if (reparable) {
          "reparar_codificacion"
        } else {
          "recuperar_codificacion_en_origen"
        }, recomendada,
        if (reparable) {
          paste0(
            "Prueba las codificaciones conocidas y se detiene cuando el texto ",
            "deja de parecer mojibake.",
            detalle_parciales,
            detalle_sin_reparacion
          )
        } else {
          if (n_irreparables > 0L) {
            paste0(
              "El car\u00e1cter de reemplazo indica que se perdieron bytes; ninguna ",
              "transformaci\u00f3n local puede recuperar el contenido original."
            )
          } else {
            paste0(
              "El motor reconoce el texto mal convertido y no pudo repararlo; ",
              "ninguna transformaci\u00f3n local puede recuperar el contenido original."
            )
          }
        },
        if (reparable) n_afectadas_reparacion else {
          fila$n_codificacion_irreparable[[1L]]
        }, FALSE,
        # Una reparacion solo de parciales queda `lista` y SIN activar:
        # `aplicar()` solo ejecuta acciones `lista`, y quedaba `informativa`, asi
        # que la "seleccion explicita" que pide la ayuda -marcar `aplicar = TRUE`-
        # abortaba. Activarla a mano aplica tambien las parciales: antes nunca se
        # aplicaban, y el registro publicaba `reparado_parcialmente` sobre una
        # celda que quedo intacta.
        estado = if (reparable) estado_columna else "informativa",
        aplicar = puede_aplicar,
        parametros = list(codificacion_intermedia = "ftfy",
                          codificaciones = names(.ftfy_tablas_bytes),
                          max_iteraciones = 20L,
                          aplicar_parciales = aplicar_parciales),
        orden = 180L, estado_reparacion = estado_reparacion
      ))
    } else if (identical(tipo, "numero_como_texto") && !is.null(fila)) {
      seguro <- isTRUE(fila$numero_texto_seguro[[1L]])
      parametros_numero <- list(
        convencion = fila$numero_texto_convencion[[1L]],
        moneda = fila$numero_texto_moneda[[1L]],
        unidad = fila$numero_texto_unidad[[1L]],
        punto_sin_coma = NA_character_, coma_sin_punto = NA_character_
      )
      comprobacion_numero <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_numero_regional", parametros_numero
      )
      # Una columna que el perfil infiere `identificador` (`101a`, `102a`) se
      # lee tambien como un numero con la unidad `a`, y por ser una sola unidad
      # la conversion salia marcada: `aplicar()` dejaba 101, 102 y la letra del
      # codigo se perdia sin que nadie la pidiera. La evidencia que hace de la
      # letra una unidad es la misma que la hace parte del codigo, asi que el
      # plan no elige: ofrece la conversion `lista` y sin marcar. Es lo unico
      # que cambia: si la comprobacion encuentra una perdida, el motivo y la
      # marca de destructiva siguen siendo los de esa perdida.
      identificador <- identical(
        as.character(fila$tipo_inferido[[1L]]), "identificador"
      )
      conversion_numero_segura <- seguro && !identificador &&
        isTRUE(comprobacion_numero$reversible)
      # Una conversion que el plan sabe que no se puede ejecutar sobre estos
      # datos no sale `lista`: sobre una columna que no es texto su propio
      # `motivo_no_reversible` decia "requiere una columna de texto", el
      # estado invitaba a activarla y `aplicar()` la registraba `fallida`
      # (medido sobre una columna `raw`). Ninguna
      # configuracion la cambia. Una conversion que solo espera que se
      # resuelva una ambiguedad -`punto_sin_coma`, `coma_sin_punto`- sigue
      # `lista`: se configura y se ejecuta. La pregunta es una, y se le hace
      # al conversor: ver `.regional_no_ejecutable_plan()`.
      estado_numero <- if (.regional_no_ejecutable_plan(
        datos, columna, parametros_numero, comprobacion_numero
      )) {
        "bloqueada"
      } else if (seguro && comprobacion_numero$verificable) {
        estado_columna
      } else if (seguro) "bloqueada" else estado_columna
      motivo_numero <- paste0(
        comprobacion_numero$justificacion,
        .frase_conversion_no_ejecutable(comprobacion_numero, estado_numero)
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_numero_regional", conversion_numero_segura,
        if (conversion_numero_segura) {
          paste0(
            "La columna usa una convenci\u00f3n decimal coherente (",
            fila$numero_texto_convencion[[1L]],
            ") y puede convertirse sin elegir entre interpretaciones.",
            .nota_unidad_numero(parametros_numero)
          )
        } else if (seguro && identificador &&
                   isTRUE(comprobacion_numero$reversible)) {
          paste0(
            "La columna se infiri\u00f3 como identificador: lo que acompa\u00f1a ",
            "al n\u00famero puede ser parte del c\u00f3digo y la conversi\u00f3n ",
            "lo descartar\u00eda. La acci\u00f3n queda lista sin activar.",
            .nota_unidad_numero(parametros_numero)
          )
        } else if (seguro) {
          motivo_numero
        } else {
          .motivo_numero_no_seguro(fila)
        },
        # Cuenta valores presentes, como las otras dos conversiones de tipo:
        # heredaba `n_numeros_texto`, que solo cuenta los que tienen
        # separadores, y el plan prometia 11 donde la conversion cambiaba los
        # 20 -una columna cambia de tipo entera, tambien el "7"-.
        .n_presentes_columna(fila), comprobacion_numero$reversible,
        estado = estado_numero,
        aplicar = conversion_numero_segura &&
          identical(estado_columna, "lista"),
        parametros = c(parametros_numero, list(
          reversibilidad_comprobada = comprobacion_numero$verificable,
          n_no_reversibles = comprobacion_numero$n_no_reversibles,
          motivo_no_reversible = motivo_numero
        )),
        unidad_conteo = "valor",
        destructiva = (seguro && !isTRUE(comprobacion_numero$reversible)) ||
          .perdida_medida(comprobacion_numero),
        orden = 320L
      ))
    } else if (identical(tipo, "formato_fecha_ambiguo")) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "desambiguar_fecha_en_origen", FALSE,
        paste0(
          "Los datos no permiten elegir entre d\u00eda/mes y mes/d\u00eda; convertirlos ",
          "inventar\u00eda una interpretaci\u00f3n."
        ), .n_afectadas_del_hallazgo(hallazgo, fila), FALSE,
        estado = "bloqueada", aplicar = FALSE,
        parametros = list(candidatos = hallazgo$evidencia[[1L]]), orden = 300L
      ))
    } else if (identical(tipo, "formatos_fecha_mixtos") && !is.null(fila)) {
      formatos <- .formatos_perfil(perfil, columna)
      confirmados <- if (is.null(formatos)) character() else {
        formatos$formato[formatos$estado == "confirmado"]
      }
      # Con dos formatos confirmados que leen distinto el mismo valor -`01/12`
      # como 1 de diciembre y como 12 de enero-, convertir es elegir. La accion
      # se marcaba `lista` con la justificacion "sin elegir entre candidatos
      # ambiguos", y la conversion resolvia el empate por la fila mas numerosa.
      ambiguos <- if ("n_ambiguos" %in% names(formatos)) {
        sum(formatos$n_ambiguos[formatos$estado == "confirmado"], na.rm = TRUE)
      } else 0
      seguro <- length(confirmados) >= 2L &&
        !any(formatos$estado != "confirmado") &&
        isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1) &&
        !.accion_columna_ambigua(perfil, columna) &&
        ambiguos == 0
      estado <- if (seguro) "lista" else "bloqueada"
      justificacion <- if (seguro) {
        paste0(
          "Todos los formatos presentes est\u00e1n confirmados por los datos y ",
          "pueden convertirse sin elegir entre candidatos ambiguos."
        )
      } else {
        paste0(
          "La conversi\u00f3n no es segura porque queda alg\u00fan formato candidato, ",
          "hay valores incompatibles, hay valores que los formatos confirmados leen ",
          "como fechas distintas, o la columna no se identifica de manera \u00fanica."
        )
      }
      destino <- if (any(grepl("%H", confirmados, fixed = TRUE, useBytes = TRUE))) {
        "fecha-hora"
      } else {
        "fecha"
      }
      parametros_fecha <- list(formatos = confirmados, tipo = destino)
      comprobacion_fecha <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_fecha_confirmada", parametros_fecha
      )
      conversion_fecha_segura <- seguro &&
        isTRUE(comprobacion_fecha$reversible)
      estado_fecha <- if (conversion_fecha_segura) estado else "bloqueada"
      motivo_fecha <- paste0(
        comprobacion_fecha$justificacion,
        .frase_conversion_no_ejecutable(comprobacion_fecha, estado_fecha)
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_fecha_confirmada", conversion_fecha_segura,
        if (conversion_fecha_segura) justificacion else motivo_fecha,
        .n_presentes_columna(fila),
        comprobacion_fecha$reversible,
        estado = estado_fecha,
        aplicar = conversion_fecha_segura && identical(estado, "lista"),
        parametros = c(parametros_fecha, list(
          reversibilidad_comprobada = comprobacion_fecha$verificable,
          n_no_reversibles = comprobacion_fecha$n_no_reversibles,
          motivo_no_reversible = motivo_fecha
        )),
        # Cuenta valores presentes, no columnas ni filas: se declara aca en
        # vez de heredar la unidad del hallazgo, que habla de otra cosa.
        unidad_conteo = "valor",
        destructiva = (seguro && !conversion_fecha_segura) ||
          .perdida_medida(comprobacion_fecha),
        orden = 300L
      ))
    } else if (identical(tipo, "tipo_declarado_distinto") && !is.null(fila) &&
               !.es_fecha_ambigua(perfil, columna) &&
               !.es_fecha_mixta(perfil, columna) &&
                 !any(
                 .nombres_para_operar(perfil$hallazgos$columna) %in%
                   .nombres_para_operar(columna) &
                   perfil$hallazgos$tipo_hallazgo == "numero_como_texto",
                 na.rm = TRUE
               )) {
      destino <- fila$tipo_inferido[[1L]]
      soportado <- destino %in% c("entero", "doble", "logico", "fecha", "fecha-hora")
      compatible <- isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1)
      formatos <- .formatos_perfil(perfil, columna)
      confirmados <- if (is.null(formatos)) character() else {
        formatos$formato[formatos$estado == "confirmado"]
      }
      fecha_segura <- !destino %in% c("fecha", "fecha-hora") ||
        (length(confirmados) > 0L && !any(formatos$estado != "confirmado"))
      parametros_tipo <- list(tipo = destino, formatos = confirmados)
      comprobacion_tipo <- .comprobar_reversibilidad_accion(
        datos, columna, "convertir_tipo", parametros_tipo
      )
      base_tipo_seguro <- soportado && compatible && fecha_segura &&
        !.accion_columna_ambigua(perfil, columna)
      recomendar <- base_tipo_seguro &&
        isTRUE(comprobacion_tipo$reversible)
      estado <- if (recomendar) estado_columna else "bloqueada"
      motivo_tipo <- paste0(
        comprobacion_tipo$justificacion,
        .frase_conversion_no_ejecutable(comprobacion_tipo, estado)
      )
      justificacion <- if (recomendar) {
        paste0(
          "Todos los valores presentes son compatibles con el tipo inferido y ",
          "la conversi\u00f3n es ejecutable e inyectiva."
        )
      } else if (base_tipo_seguro) {
        motivo_tipo
      } else {
        paste0(
          "La conversi\u00f3n requiere compatibilidad total, un tipo con conversi\u00f3n ",
          "definida y, para fechas, un formato confirmado."
        )
      }
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "convertir_tipo", recomendar, justificacion,
        .n_presentes_columna(fila), comprobacion_tipo$reversible,
        estado = estado, aplicar = recomendar,
        parametros = c(parametros_tipo, list(
          reversibilidad_comprobada = comprobacion_tipo$verificable,
          n_no_reversibles = comprobacion_tipo$n_no_reversibles,
          motivo_no_reversible = motivo_tipo
        )), orden = 310L, unidad_conteo = "valor",
        grupo = grupo_hallazgo,
        decision_grupo = if (recomendar) "recomendada" else "pendiente",
        recomendacion_grupo = if (recomendar) "convertir_tipo" else NA_character_,
        destructiva = (base_tipo_seguro && !recomendar) ||
          .perdida_medida(comprobacion_tipo)
      ))
    } else if (identical(tipo, "filas_duplicadas")) {
      n_participantes <- perfil$general$filas_en_grupos_duplicados
      if (is.null(n_participantes)) {
        n_participantes <- perfil$general$filas_duplicadas
      }
      # La deteccion cuenta duplicados con `duplicated.data.frame()`, que
      # tolera columnas-lista, pero el ejecutor agrupa por los codigos de
      # `factor()`, que no las admite, y aborta. El plan recomendaba y activaba
      # una accion que sobre esa tabla no podia ejecutarse nunca: la registraba
      # `fallida` en cada corrida. Se bloquea y se dice por que, que es lo que
      # el paquete ya hace con las conversiones que no puede comprobar.
      #
      # Con `datos`, la guarda hace la MISMA pregunta que el ejecutor sobre la
      # misma tabla aplanada: `tipo_declarado == "lista"` no ve una columna de
      # lista dentro de un `data.frame` anidado ni una geometria `sfc`, que son
      # listas y el ejecutor no puede comparar. Sin `datos`, queda la lectura
      # del perfil.
      con_lista <- if (!is.null(datos)) {
        .identidad_filas_con_listas(.tabla_identidad_filas(datos))
      } else {
        inherits(perfil$columnas, "data.frame") &&
          "tipo_declarado" %in% names(perfil$columnas) &&
          any(as.character(perfil$columnas$tipo_declarado) == "lista")
      }
      motivo_lista <- paste0(
        "La tabla tiene columnas de lista y el agrupamiento de duplicados no ",
        "puede compararlas; la acci\u00f3n no se puede ejecutar sobre estos datos. ",
        "Convertir o quitar las columnas de lista para agrupar las filas."
      )
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "marcar_filas_duplicadas", !con_lista,
        if (con_lista) motivo_lista else paste0(
          "Marcar conserva todas las filas, identifica las repeticiones y ",
          "asigna un grupo a todos los registros que participan. No elimina ",
          "filas; mientras las marcas esten incluidas, un perfil posterior no ",
          "vuelve a contar esas filas como duplicadas exactas."
        ), n_participantes, TRUE,
        estado = if (con_lista) "bloqueada" else "lista", aplicar = !con_lista,
        parametros = list(
          columna_marca = ".fila_duplicada",
          columna_grupo = ".grupo_duplicado"
        ), orden = 30L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "conservar_primera_duplicada", FALSE,
        if (con_lista) motivo_lista else paste0(
          "Conserva la primera aparici\u00f3n exacta y elimina las siguientes; ",
          "el orden de entrada pasa a determinar qu\u00e9 registro sobrevive."
        ), perfil$general$filas_duplicadas, FALSE,
        estado = if (con_lista) "bloqueada" else "lista",
        aplicar = FALSE, orden = 35L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas",
        destructiva = TRUE
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "conservar_mas_completa", FALSE,
        paste0(
          "Requiere configurar una clave: entre duplicados exactos todas las ",
          "filas tienen la misma completitud y esta opci\u00f3n ser\u00eda equivalente a la primera."
        ), perfil$general$filas_duplicadas, FALSE, estado = "bloqueada",
        aplicar = FALSE, parametros = list(clave = character()), orden = 36L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_filas_duplicadas",
        destructiva = TRUE
      ))
    } else if (identical(tipo, "outliers") && !is.null(fila)) {
      winsor_disponible <- fila$tipo_inferido[[1L]] %in% c("entero", "doble") &&
        isTRUE(fila$proporcion_tipo_inferido[[1L]] == 1)
      # Los limites de Tukey son CUARTILES, no enteros: sobre una columna entera
      # la sustitucion devuelve dobles y la columna deja de ser entera. Medido:
      # `integer` entraba y salia `numeric` con la fila diciendo
      # `destructiva = FALSE`, la justificacion hablando solo de valores y
      # ninguno de los diecisiete campos del registro nombrando el tipo -mientras
      # la MISMA accion sobre `Date` sale `bloqueada`, o sea que el plan si mira
      # el tipo cuando le importa-. La definicion de `destructiva` de este mismo
      # archivo incluye "una conversion que pierde representacion, aunque no
      # elimine filas o columnas", asi que esto es una de ellas: se marca y se
      # dice, igual que la de factor -> texto. No cambia lo que hay que consentir:
      # `permitir_eliminacion` se exige solo a lo que retira filas o columnas, y
      # esta accion ya requeria activacion explicita.
      # Solo si la columna esta GUARDADA como entera: una de texto con numeros
      # se infiere `entero` pero sigue siendo texto despues de la accion -solo
      # se reescriben las celdas recortadas-, y el plan publicaba
      # `tipo_original = "entero"` sobre una columna de texto.
      winsor_pierde_entero <- identical(fila$tipo_inferido[[1L]], "entero") &&
        identical(as.character(fila$tipo_declarado[[1L]]), "entero")
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_outliers", TRUE,
        paste0(
          "Un valor extremo puede ser correcto; la marca conserva el dato ",
          "para que el dominio decida c\u00f3mo tratarlo."
        ), fila$n_outliers[[1L]], TRUE, estado = estado_columna,
        aplicar = FALSE,
        parametros = list(
          columna_marca = paste0(".outlier_", .nombres_make_names(columna)),
          regla = "Tukey 1,5 x IQR"
        ), orden = 510L, grupo = grupo_hallazgo,
        decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_outliers"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "winsorizar_outliers", FALSE,
        paste0(
          "Sustituye los extremos por los l\u00edmites de Tukey y altera valores ",
          "observados; s\u00f3lo debe elegirse con justificaci\u00f3n anal\u00edtica.",
          if (winsor_pierde_entero) {
            paste0(
              " Los l\u00edmites de Tukey son cuartiles, as\u00ed que la columna deja ",
              "de ser entera y queda en doble precisi\u00f3n."
            )
          } else {
            ""
          }
        ), fila$n_outliers[[1L]], FALSE,
        estado = if (winsor_disponible) estado_columna else "bloqueada",
        aplicar = FALSE,
        parametros = c(
          list(regla = "Tukey 1,5 x IQR"),
          if (winsor_pierde_entero) {
            list(tipo_resultante = "doble", tipo_original = "entero")
          } else {
            list()
          }
        ), orden = 520L,
        grupo = grupo_hallazgo, decision_grupo = "pendiente",
        recomendacion_grupo = "marcar_outliers",
        destructiva = winsor_pierde_entero
      ))
    } else if (identical(tipo, "nombres_columnas_problematicos")) {
      nombres <- perfil$columnas$columna
      problema <- .nombres_columnas_problematicos(nombres)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "normalizar_nombres", TRUE,
        paste0(
          "Los nombres sint\u00e1cticos y \u00fanicos evitan referencias ambiguas sin ",
          "alterar el contenido de las columnas."
        ), nrow(problema), FALSE, estado = "lista", aplicar = TRUE,
        parametros = list(
          nombres_esperados = nombres,
          nombres_propuestos = .nombres_make_names(nombres)
        ), orden = 900L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "normalizar_nombres"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        NA_character_, tipo, "normalizar_nombres_snake_case", FALSE,
        paste0(
          "snake_case es legible y estable, pero cambia may\u00fasculas y signos; ",
          "se ofrece como alternativa expl\u00edcita a make.names()."
        ), nrow(problema), FALSE, estado = "lista", aplicar = FALSE,
        parametros = list(
          nombres_esperados = nombres,
          nombres_propuestos = .nombres_snake(nombres)
        ), orden = 900L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "normalizar_nombres"
      ))
    } else if (identical(tipo, "mayusculas_inconsistentes") && !is.null(fila)) {
      opciones <- list(
        list(
          estrategia = "convertir_minusculas",
          justificacion = "Unifica la columna en min\u00fasculas; puede alterar nombres propios."
        ),
        list(
          estrategia = "convertir_titulo",
          justificacion = paste0(
            "Capitaliza cada palabra; no conoce excepciones ling\u00fc\u00edsticas ni ",
            "convenciones de nombres propios."
          )
        ),
        list(
          estrategia = "convertir_mayusculas",
          justificacion = "Unifica la columna en may\u00fasculas; puede perder matices del texto."
        ),
        list(
          estrategia = "convertir_segun_diccionario",
          justificacion = paste0(
            "Aplica un vector con nombres donde cada nombre es el valor original ",
            "y su contenido es el valor normalizado."
          ),
          parametros = list(diccionario = NULL)
        )
      )
      for (opcion in opciones) {
        requiere_diccionario <- identical(
          opcion$estrategia, "convertir_segun_diccionario"
        )
        acciones <- .agregar_accion(acciones, .nueva_accion(
          columna, tipo, opcion$estrategia, FALSE, opcion$justificacion,
          fila$n_variantes_mayusculas[[1L]], FALSE,
          estado = if (requiere_diccionario) "bloqueada" else estado_columna,
          parametros = if (is.null(opcion$parametros)) list() else opcion$parametros,
          orden = 600L, grupo = grupo_hallazgo,
          decision_grupo = "pendiente"
        ))
      }
    } else if (identical(tipo, "constante") && !is.null(fila)) {
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_columna_constante", FALSE,
        paste0(
          "Eliminarla pierde contexto potencial; dejarla es la recomendaci\u00f3n ",
          "hasta confirmar que no aporta significado administrativo."
        ), 1, FALSE, estado = estado_columna, aplicar = FALSE,
        orden = 710L, grupo = grupo_hallazgo,
        decision_grupo = "recomendada",
        recomendacion_grupo = "no_hacer_nada", destructiva = TRUE
      ))
    } else if (identical(tipo, "columnas_duplicadas")) {
      # Una decision por GRUPO de columnas iguales: marcar la redundancia, o
      # eliminar las repetidas y conservar la primera, como
      # `conservar_primera_duplicada` con las filas. Con una decision por par,
      # `A = B = C` proponia eliminar B, C y otra vez C, y la tercera quedaba
      # `fallida`.
      g <- grupo_de_hallazgo[[i]]
      miembros <- if (is.na(g)) NULL else grupos_duplicadas$grupos[[g]]
      # Con un nombre que se repite en la tabla, la accion no puede decir sobre
      # cual de las columnas que se llaman igual actua: con `X, X` proponia dos
      # acciones `bloqueada` con `columna_2` vacia. Como en las demas ramas, no
      # se propone, y el hallazgo queda en
      # `hallazgos_sin_accion_por_columna_ambigua`, con el remedio.
      if (.grupo_con_nombre_repetido(miembros, claves_columnas_perfil)) next
      # Un miembro del grupo que no esta en el perfil -un hallazgo que no
      # corresponde a sus columnas-: la accion salia `bloqueada` sin decir por
      # que. La decision sobre el grupo no se toma con una parte del grupo: se
      # bloquea y se dice cual falta. Un miembro que falta en los DATOS es la
      # regla de todas las acciones: ver `.bloquear_columnas_ausentes()`.
      faltan_perfil <- if (is.null(miembros)) character() else {
        miembros[!.nombres_para_operar(miembros) %in% claves_columnas_perfil]
      }
      estado_par <- if (
        is.null(miembros) || !all(tabulate(
          match(claves_columnas_perfil, .nombres_para_operar(miembros)),
          length(miembros)
        ) == 1L)
      ) "bloqueada" else "lista"
      motivo_bloqueo_par <- .motivo_grupo_duplicadas_incompleto(
        miembros, faltan_perfil
      )
      parametros_par <- if (is.null(miembros)) list() else list(
        columna_1 = miembros[[1L]], columna_2 = miembros[-1L],
        eliminar = miembros[-1L]
      )
      n_repetidas <- if (is.null(miembros)) 1 else length(miembros) - 1L
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "marcar_columnas_duplicadas", TRUE,
        paste0(if (n_repetidas == 1L) {
          "La anotaci\u00f3n conserva ambas columnas y registra expl\u00edcitamente la redundancia."
        } else {
          "La anotaci\u00f3n conserva todas las columnas y registra expl\u00edcitamente la redundancia."
        }, motivo_bloqueo_par), n_repetidas, TRUE, estado = estado_par,
        aplicar = identical(estado_par, "lista"),
        parametros = parametros_par, orden = 700L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_columnas_duplicadas"
      ))
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, "eliminar_columna_duplicada", FALSE,
        paste0(if (n_repetidas == 1L) {
          paste0(
            "Eliminar una columna puede romper consumidores que dependan de su ",
            "nombre aunque el contenido sea redundante."
          )
        } else {
          paste0(
            "Elimina las ", .formatear_numero_publicado(n_repetidas),
            " columnas repetidas y conserva la primera del grupo. Puede ",
            "romper consumidores que dependan de sus nombres aunque el ",
            "contenido sea redundante."
          )
        }, motivo_bloqueo_par), n_repetidas, FALSE, estado = estado_par, aplicar = FALSE,
        parametros = parametros_par, orden = 705L,
        grupo = grupo_hallazgo, decision_grupo = "recomendada",
        recomendacion_grupo = "marcar_columnas_duplicadas",
        destructiva = TRUE
      ))
    } else if (tipo %in% c(
      "alta_cardinalidad", "ceros_no_permitidos", "negativos_no_permitidos"
    )) {
      estrategias <- c(
        alta_cardinalidad = "revisar_cardinalidad",
        ceros_no_permitidos = "revisar_ceros",
        negativos_no_permitidos = "revisar_negativos"
      )
      # El numero que se publica tiene que ser el de la UNIDAD que se publica.
      # Aca se pasaba `fila$n` -las filas de la columna- y la unidad se hereda
      # del hallazgo: sobre una tabla de 7 columnas, la accion de
      # `alta_cardinalidad` publicaba `n_afectadas = 80` con
      # `unidad_conteo = "columna"` mientras su hallazgo declaraba 1 columna.
      # Ninguna lectura salva ese par: ni 80 columnas existen, ni la unidad es
      # de filas. Estas tres acciones son informativas -no tocan nada-, asi que
      # lo que corresponde es el alcance que midio el hallazgo que las origino.
      # Ver `.n_afectadas_del_hallazgo()`.
      n <- .n_afectadas_del_hallazgo(hallazgo, fila)
      acciones <- .agregar_accion(acciones, .nueva_accion(
        columna, tipo, estrategias[[tipo]], FALSE,
        paste0(
          "El perfil se\u00f1ala el problema, pero no contiene conocimiento ",
          "suficiente del dominio para elegir una transformaci\u00f3n."
        ), n, NA, estado = "informativa", aplicar = FALSE, orden = 800L
      ))
    }
  }

  dependencias <- perfil$dependencias
  if (!is.null(datos) && inherits(dependencias, "data.frame") &&
      nrow(dependencias)) {
    if (!identical(
      .nombres_para_operar(names(datos)),
      .nombres_para_operar(perfil$columnas$columna)
    )) {
      .detener("Los nombres de `datos` no coinciden con los usados por el perfil.",
           call. = FALSE)
    }
    exactas <- dependencias[dependencias$exacta, , drop = FALSE]
    candidatas <- list()
    for (i in seq_len(nrow(exactas))) {
      determinante <- exactas$determinante[[i]]
      dependiente <- exactas$dependiente[[i]]
      indices_dependencia <- .indice_nombre(
        c(determinante, dependiente), names(datos)
      )
      if (anyNA(indices_dependencia)) next
      determinante <- names(datos)[indices_dependencia[[1L]]]
      dependiente <- names(datos)[indices_dependencia[[2L]]]
      if (any(.nombres_para_operar(c(determinante, dependiente)) %in%
                .nombres_para_operar(marcas_plan))) {
        next
      }
      mapa <- .mapa_dependencia(
        datos, determinante, dependiente,
        soporte_minimo = soporte_minimo_dependencia
      )
      if (!nrow(mapa)) next
      # Por indice: un nombre marcado `bytes` no se traduce (ver
      # `.mapa_dependencia()`).
      indices <- match(
        .valores_relacion(datos[[indices_dependencia[[1L]]]]),
        .valores_relacion(mapa$determinante)
      )
      imputables <- is.na(datos[[indices_dependencia[[2L]]]]) & !is.na(indices)
      # Solo dentro del universo aplicable del dependiente: una fila declarada
      # fuera es vacio por diseno, el perfil no la cuenta como faltante y
      # `aplicar()` no la toca (ver `.estrategia_por_celda()`). Si la regla no se
      # puede evaluar aca, tampoco se podra al aplicar, que lo dira; el conteo
      # queda sin recortar.
      reglas <- perfil$meta$reglas_aplicabilidad
      i_regla <- if (length(reglas)) .indice_nombre(dependiente, names(reglas))
      regla_dependiente <- if (length(i_regla) && !is.na(i_regla)) {
        reglas[[i_regla]]
      }
      if (!is.null(regla_dependiente)) {
        universo <- tryCatch(
          .mascara_aplicabilidad_accion(
            datos, dependiente, list(aplicabilidad = regla_dependiente)
          ),
          error = function(e) NULL
        )
        if (!is.null(universo)) imputables <- imputables & universo
      }
      if (!any(imputables)) next
      candidatas[[length(candidatas) + 1L]] <- list(
        determinante = determinante, dependiente = dependiente,
        mapa = mapa, n = sum(imputables), soporte = exactas$n_evaluados[[i]]
      )
    }
    if (length(candidatas)) {
      por_dependiente <- .dividir_por_bytes(
        seq_along(candidatas),
        .nombres_para_operar(vapply(
          candidatas, `[[`, character(1L), "dependiente"
        ))
      )
      numero_grupo <- nrow(hallazgos)
      for (indices in por_dependiente) {
        numero_grupo <- numero_grupo + 1L
        grupo <- .id_grupo(numero_grupo)
        for (indice in indices) {
          candidata <- candidatas[[indice]]
          estrategia_imputacion <- paste0(
            "imputar_dependencia_funcional__",
            .nombres_make_names(candidata$determinante)
          )
          acciones <- .agregar_accion(acciones, .nueva_accion(
            candidata$dependiente, "faltantes",
            estrategia_imputacion, FALSE,
            paste0(
              "La relaci\u00f3n ", candidata$determinante, " -> ",
              candidata$dependiente, " es exacta en ", candidata$soporte,
              if (candidata$soporte == 1L) " fila presente y cada valor usado tiene al menos " else
                " filas presentes y cada valor usado tiene al menos ",
              soporte_minimo_dependencia,
              if (soporte_minimo_dependencia == 1L) " observaci\u00f3n de soporte. " else " observaciones de soporte. ",
              "Debe confirmarse como regla antes de imputar."
            ),
            candidata$n, FALSE, estado = "lista", aplicar = FALSE,
            parametros = list(
              determinante = candidata$determinante,
              dependiente = candidata$dependiente,
              mapa = candidata$mapa,
              soporte_minimo = soporte_minimo_dependencia,
              cumplimiento = 1
            ),
            orden = 150L, grupo = grupo,
            decision_grupo = "pendiente",
            recomendacion_grupo = NA_character_
          ))
        }
      }
    }
  }

  if (!length(acciones)) {
    resultado <- .plan_vacio()
  } else {
    resultado <- do.call(rbind, acciones)
    rownames(resultado) <- NULL
    resultado$id_accion <- sprintf("accion-%04d", seq_len(nrow(resultado)))
    for (j in seq_len(nrow(resultado))) {
      indice_hallazgo <- if (!is.na(resultado$grupo[[j]])) {
        suppressWarnings(as.integer(sub("^grupo-", "", resultado$grupo[[j]])))
      } else {
        candidatos <- which(
          hallazgos$tipo_hallazgo == resultado$hallazgo[[j]] &
            ((is.na(hallazgos$columna) & is.na(resultado$columna[[j]])) |
               .nombres_para_operar(hallazgos$columna) %in%
                 .nombres_para_operar(resultado$columna[[j]]))
        )
        if (length(candidatos)) candidatos[[1L]] else NA_integer_
      }
      if (!is.na(indice_hallazgo) && indice_hallazgo <= nrow(hallazgos)) {
        resultado$evidencia[[j]] <- hallazgos$evidencia[[indice_hallazgo]]
        resultado$severidad_origen[[j]] <- as.character(
          hallazgos$severidad[[indice_hallazgo]]
        )
        # Solo se hereda la unidad del hallazgo si la accion NO declaro la
        # suya. Antes se pisaba siempre, y una accion que cuenta en otra unidad
        # quedaba con el par mal: `convertir_tipo` declaraba `n_afectadas = 5`
        # -los valores presentes- con `unidad_conteo = "columna"` heredado del
        # hallazgo, sobre una tabla de UNA columna. Ese par no es cierto en
        # ninguna lectura, y `guiar_limpieza()` lo repetia en pantalla.
        if (is.na(resultado$unidad_conteo[[j]])) {
          resultado$unidad_conteo[[j]] <- as.character(
            hallazgos$unidad_conteo[[indice_hallazgo]]
          )
        }
      } else if (startsWith(resultado$estrategia[[j]],
                            "imputar_dependencia_funcional__")) {
        resultado$unidad_conteo[[j]] <- "fila"
      }
    }
  }
  resultado <- .declarar_texto_en_columna_factor(resultado, perfil)
  resultado$estado <- factor(
    resultado$estado,
    levels = c("lista", "bloqueada", "informativa")
  )
  resultado$decision_grupo <- factor(
    resultado$decision_grupo,
    levels = c(
      "pendiente", "recomendada", "desactivada", "elegida", "omitida"
    )
  )
  resultado <- .aplicabilidad_en_plan(resultado, perfil)
  resultado <- .bloquear_marcas_existentes(resultado, nombres_tabla)
  resultado <- .bloquear_columnas_ausentes(resultado, datos)
  class(resultado) <- c("plan_limpieza", "data.frame")
  # El plan se arma desde `perfil$hallazgos`, asi que por construccion no puede
  # tener una accion para un diagnostico que no se evaluo. Quien trabaja desde
  # el plan no tenia forma de enterarse de que uno se declino: leia tres
  # acciones y concluia que lo demas estaba bien, cuando lo que habia pasado es
  # que sobre esa columna no se miro. La cobertura viaja con el plan para que
  # ese silencio sea visible desde la misma puerta donde se decide que limpiar.
  attr(resultado, "cobertura_diagnosticos") <- if (
    inherits(perfil$cobertura_diagnosticos, "data.frame")
  ) {
    perfil$cobertura_diagnosticos
  } else {
    .cobertura_diagnosticos_vacia()
  }
  ambiguos <- .hallazgos_sin_accion_por_columna_ambigua(
    perfil, grupos = grupos_duplicadas
  )
  attr(resultado, "hallazgos_sin_accion_por_columna_ambigua") <- ambiguos
  attr(resultado, "hallazgos_sin_accion") <-
    .hallazgos_sin_accion_declarados(perfil, resultado, ambiguos)
  .proteger_plan_limpieza(resultado, perfil, datos)
}

.columnas_plan <- function() {
  c(
    "id_accion", "columna", "hallazgo", "grupo", "decision_grupo",
    "recomendacion_grupo", "estrategia", "recomendada",
    "severidad_origen", "evidencia", "justificacion", "n_afectadas",
    "unidad_conteo",
    "reversible", "destructiva", "estado", "aplicar", "orden", "parametros"
  )
}

.validar_plan_limpieza <- function(plan) {
  requeridas <- .columnas_plan()
  if (!inherits(plan, "data.frame") || !all(requeridas %in% names(plan))) {
    .detener("`plan` no cumple el contrato de un plan de limpieza.", call. = FALSE)
  }
  if (!is.logical(plan$aplicar) || anyNA(plan$aplicar)) {
    .detener("`plan$aplicar` debe ser un vector l\u00f3gico sin NA.", call. = FALSE)
  }
  if (!is.character(plan$id_accion) || anyNA(plan$id_accion) ||
      length(plan$id_accion) != nrow(plan)) {
    # Se comprobaba que fueran unicos y no que fueran texto: vaciando la
    # columna, el usuario recibia "los argumentos implican un numero diferente
    # de filas: 0, 1" -un error interno de R al armar el registro- en vez del
    # motivo del plan que el paquete promete.
    .detener(
      "`plan$id_accion` debe ser un vector de texto sin NA, con un ",
      "identificador por fila.",
      call. = FALSE
    )
  }
  if (anyDuplicated(plan$id_accion)) {
    .detener("`plan$id_accion` debe contener identificadores \u00fanicos.", call. = FALSE)
  }
  if (!is.list(plan$parametros)) {
    .detener("`plan$parametros` debe ser una columna de listas.", call. = FALSE)
  }
  if (!is.character(plan$unidad_conteo)) {
    .detener("`plan$unidad_conteo` debe ser texto.", call. = FALSE)
  }
  if (!is.logical(plan$recomendada) || anyNA(plan$recomendada) ||
      !is.logical(plan$reversible)) {
    .detener(
      "`recomendada` debe ser l\u00f3gica sin NA y `reversible` debe ser l\u00f3gica.",
      call. = FALSE
    )
  }
  if (!is.character(plan$grupo) || !is.logical(plan$destructiva) ||
      anyNA(plan$destructiva)) {
    .detener("`grupo` debe ser texto y `destructiva` un l\u00f3gico sin NA.", call. = FALSE)
  }
  # `orden` fija la secuencia, y como texto se ordena alfabeticamente: `"35"` va
  # despues de `"200"`. Una asignacion de R sobre cero filas basta para volverlo
  # texto, y el plan editado eliminaba tres filas en vez de una sin decir nada.
  # Medido.
  if (!is.numeric(plan$orden) || anyNA(plan$orden)) {
    .detener(
      "`orden` debe ser num\u00e9rico sin NA: fija la secuencia en que se ",
      "aplican las acciones, y como texto se ordenar\u00eda alfab\u00e9ticamente.",
      call. = FALSE
    )
  }
  if (any(plan$destructiva & plan$recomendada, na.rm = TRUE)) {
    .detener("Una acci\u00f3n destructiva nunca puede ser recomendada.", call. = FALSE)
  }
  if (any(plan$destructiva & (is.na(plan$reversible) | plan$reversible))) {
    .detener("Toda acci\u00f3n destructiva debe declarar `reversible = FALSE`.", call. = FALSE)
  }
  estados <- as.character(plan$estado)
  if (anyNA(estados) || any(!estados %in% c("lista", "bloqueada", "informativa"))) {
    .detener("`estado` contiene un valor no reconocido.", call. = FALSE)
  }
  if (any(plan$aplicar & estados != "lista")) {
    invalidas <- which(plan$aplicar & estados != "lista")
    detalle <- paste(vapply(invalidas, function(i) {
      paste0("fila ", i, " (columna '", as.character(plan$columna[[i]]),
             "', estrategia '", as.character(plan$estrategia[[i]]),
             "', estado '", estados[[i]], "')")
    }, character(1L)), collapse = "; ")
    .detener(
      "S\u00f3lo se pueden aplicar acciones con estado 'lista'. ",
      "Acciones no listas: ", detalle, ".",
      call. = FALSE
    )
  }
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    activas <- indices[plan$aplicar[indices]]
    if (length(activas) > 1L) {
      .detener(
        "El grupo '", grupo, "' tiene acciones incompatibles activas: ",
        paste(plan$estrategia[activas], collapse = ", "), ".",
        call. = FALSE
      )
    }
    decision <- unique(as.character(plan$decision_grupo[indices]))
    decision <- decision[!is.na(decision)]
    if (!length(activas) && identical(decision, "elegida")) {
      # `decision_grupo` es la distincion entre una eleccion y una omision. Un
      # grupo marcado como elegido y sin ninguna accion activa no es ninguna de
      # las dos: es una edicion contradictoria, y el paquete ya rechaza las
      # otras dos de esta familia en vez de adivinar cual gana.
      .detener(
        "El grupo '", grupo, "' est\u00e1 marcado como elegido y no tiene ninguna ",
        "acci\u00f3n activa. Activar la acci\u00f3n elegida o cambiar la decisi\u00f3n.",
        call. = FALSE
      )
    }
    if (anyDuplicated(plan$estrategia[indices])) {
      .detener("El grupo '", grupo, "' repite una estrategia.", call. = FALSE)
    }
    decisiones <- unique(as.character(plan$decision_grupo[indices]))
    if (length(decisiones) != 1L || is.na(decisiones)) {
      .detener("El grupo '", grupo, "' debe compartir una sola decisi\u00f3n.", call. = FALSE)
    }
    recomendaciones <- unique(plan$recomendacion_grupo[indices])
    recomendaciones <- recomendaciones[!is.na(recomendaciones)]
    if (length(recomendaciones) > 1L) {
      .detener("El grupo '", grupo, "' declara recomendaciones incompatibles.", call. = FALSE)
    }
  }
  invisible(plan)
}

.sincronizar_decisiones <- function(plan) {
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    activas <- indices[plan$aplicar[indices]]
    recomendacion <- unique(plan$recomendacion_grupo[indices])
    recomendacion <- recomendacion[!is.na(recomendacion)]
    if (length(activas) == 1L &&
        (!length(recomendacion) ||
         plan$estrategia[[activas]] != recomendacion[[1L]])) {
      plan$decision_grupo[indices] <- "elegida"
    } else if (length(activas) == 1L && length(recomendacion) &&
               plan$estrategia[[activas]] == recomendacion[[1L]] &&
               as.character(plan$decision_grupo[[indices[[1L]]]]) %in%
                 c("pendiente", "omitida", "desactivada")) {
      # La activa es la recomendada: el grupo no esta sin revisar ni omitido, y el
      # registro publicaba `pendiente` u `omitida` junto a `ejecutada`. Medido.
      plan$decision_grupo[indices] <- "recomendada"
    } else if (!length(activas) && length(recomendacion) &&
               recomendacion[[1L]] != "no_hacer_nada" &&
               as.character(plan$decision_grupo[[indices[[1L]]]]) == "recomendada") {
      plan$decision_grupo[indices] <- "desactivada"
    }
  }
  plan
}

#' @export
rbind.plan_limpieza <- function(..., deparse.level = 1) {
  .unir_con_marca_sin_proteger(..., deparse.level = deparse.level)
}

#' @export
`[.plan_limpieza` <- function(x, ...) {
  resultado <- NextMethod("[")
  if (inherits(resultado, "data.frame") &&
      all(.columnas_plan() %in% names(resultado))) {
    class(resultado) <- unique(c("plan_limpieza", class(resultado)))
  } else if (inherits(resultado, "data.frame")) {
    class(resultado) <- setdiff(class(resultado), "plan_limpieza")
  }
  .conservar_atributos_objeto(resultado, x)
}

.copiar_datos <- function(datos) {
  if (inherits(datos, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::copy(datos))
  }
  datos
}

.indice_columna <- function(datos, columna) {
  indices <- which(.nombres_para_operar(names(datos)) %in%
                   .nombres_para_operar(columna))
  if (length(indices) != 1L) {
    .detener(
      "La acci\u00f3n requiere una \u00fanica columna llamada '", columna,
      "'; se encontraron ", length(indices), ".", call. = FALSE
    )
  }
  indices[[1L]]
}

.reemplazar_ausencias_textuales <- function(x, parametros, proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La normalizaci\u00f3n textual requiere una columna de texto.", call. = FALSE)
  }
  # Se convierte EXACTAMENTE lo que el detector conto, y no lo que diga un
  # catalogo propio. Antes este ejecutor tenia su propia logica y se habia
  # separado de la deteccion por dos lados:
  #
  #   * el detector saca `sd`, `nc` y `nd` del catalogo cuando la columna tiene
  #     diez o mas valores distintos -para no confundirlos con codigos-, y el
  #     ejecutor usaba el catalogo COMPLETO. Sobre una columna de estados de
  #     EE.UU. el hallazgo declaraba 4 celdas y la accion borraba 7, entre
  #     ellas Dakota del Sur, Carolina del Norte y Dakota del Norte;
  #   * el detector normaliza con `.texto_analizable()`, que tolera valores
  #     marcados `bytes`, y el ejecutor hacia `tolower()` sobre la columna
  #     cruda, que ABORTA con uno solo: la accion se recomendaba y era
  #     imposible de ejecutar.
  #
  # Una regla escrita dos veces. Ahora hay una sola, la del detector.
  #
  # La lista `valores` del plan es la que manda: lo que nombra se convierte y lo
  # que no nombra no. Los textos de la lista atraviesan la guarda del
  # vocabulario como lo declarado en `cadenas_ausencia`, porque nombrarlos en
  # el plan es declararlos. Antes la lista solo podia ACOTAR -decision escrita
  # en este comentario y en ninguna ayuda-: agregar `"b"` a `valores` dejaba
  # las `"B"` en la tabla con el registro `ejecutada` publicando
  # `valores = c("s/d", "n/a", "b")`, parametros que no aplico; la accion
  # hermana `convertir_sentinelas_numericos` si honra la lista ampliada.
  # Medido. El plan arma la lista con lo que el detector
  # encontro, asi que sin editar convierte lo mismo que antes.
  # Acotar con la lista SIEMPRE, tambien vacia. Con `if (length(valores))` una
  # lista vacia dejaba de acotar: sacar el unico marcador para conservarlo lo
  # convertia igual, y aplicado a otra entrega convertia `NULL` o `sin dato`, que
  # el plan nunca nombro, mientras el registro publicaba `valores = character(0)`
  # junto a cuatro celdas cambiadas. Medido. El plan siempre
  # arma la lista con lo observado, asi que vacia o ausente es lo mismo: nada
  # autorizado.
  if (!length(parametros$valores)) {
    .detener(
      "La lista `valores` de la acci\u00f3n est\u00e1 vac\u00eda: no hay ",
      "marcadores autorizados para convertir. Para conservarlos todos, ",
      "desactivar la acci\u00f3n.", call. = FALSE
    )
  }
  transformar <- function(v) {
    deteccion <- .detectar_faltantes_disfrazados(
      v, detectar_sentinelas_numericos = FALSE,
      cadenas_ausencia = unique(c(
        as.character(parametros$cadenas_ausencia),
        as.character(parametros$valores)
      ))
    )
    normalizados <- .minusculas_para_comparar(.texto_analizable(v)$valores)
    mascara <- deteccion$mascara_textual & !is.na(normalizados) &
      normalizados %in% .minusculas_para_comparar(parametros$valores)
    salida <- as.character(v)
    salida[mascara] <- NA_character_
    salida
  }
  protegido <- .proteger_accion_texto(
    as.character(x), transformar, proteccion = proteccion
  )
  list(valor = .resultado_texto(x, protegido$valor),
       n = sum(protegido$cambiadas))
}

# Los valores que convierte la accion. Con `valores_enmascarados` -la proteccion
# tapo en el plan un centinela de una columna protegida- lo tapado se resuelve
# sobre los datos que recibe `aplicar()`, sin que el plan lo publique: son los
# rellenos de la columna (`.rellenos_de_columna()`). El plan solo lo ofrece asi
# cuando esa resolucion convierte lo que estima; ver
# `.centinelas_tapados_en_plan()`.
#
# Con `guarda` -el plan no sabe cuales centinelas conto el perfil: la traza no
# los trae o la proteccion los tapo- la lista entera se acota antes sobre los
# datos: de los centinelas de la guarda quedan los que el detector cuenta en la
# columna recibida. Lo que el usuario agrego a `valores` no es de la guarda y
# queda; lo que quito ya no esta. La guarda nunca agrega un valor a la lista.
#
# `x_guarda` es la columna sobre la que decide la guarda: la que `aplicar()`
# RECIBIO, no la que dejaron las acciones anteriores del mismo plan. El perfil
# decidio mirando la columna entera, tambien las celdas que son marca textual:
# con `cadenas_ausencia = "999"`, tres `"999"` y un `"999,00"`, el 999 aparece
# cuatro veces y el detector lo cuenta. Si `convertir_ausencias_textuales` corre
# antes y la guarda mira lo que queda, el 999 aparece una sola vez, dentro del
# rango, y deja de contarse: la accion convertia una celda menos que el perfil.
.valores_sentinelas_accion <- function(x, parametros, aplicable = NULL,
                                       x_guarda = x) {
  valores <- parametros$valores
  guarda <- parametros$guarda
  if (is.list(guarda) && is.numeric(valores) &&
      is.numeric(guarda$sentinelas)) {
    contados <- .sentinelas_contados_sobre_datos(
      x_guarda, guarda, parametros$cadenas_ausencia, aplicable
    )
    de_la_guarda <- !is.na(valores) & valores %in% guarda$sentinelas
    valores <- valores[!de_la_guarda | valores %in% contados]
  }
  if (!isTRUE(parametros$valores_enmascarados)) return(valores)
  visibles <- valores[!is.na(valores)]
  rellenos <- suppressWarnings(as.numeric(.rellenos_de_columna(x)))
  unique(c(visibles, rellenos[!is.na(rellenos)]))
}

# Los centinelas de la guarda que el detector cuenta en la columna que recibe
# `aplicar()`. Es `.faltantes_disfrazados_columna()` -la unica funcion que
# decide el conteo del perfil- con el contexto que el perfil publico, en lugar
# de volver a inferirlo: el tipo, la convencion decimal y las banderas de la
# numeracion salen de la guarda. Las filas fuera del universo aplicable salen
# antes, como en el perfil: las banderas describen ese universo, y un centinela
# de fuera no se cuenta ni puede volver sobresaliente a una moda.
.sentinelas_contados_sobre_datos <- function(x, guarda, cadenas_ausencia = NULL,
                                             aplicable = NULL) {
  x_analisis <- .texto_analizable(x)$valores
  if (is.null(aplicable)) aplicable <- rep(TRUE, length(x_analisis))
  if (length(aplicable) != length(x_analisis)) return(numeric())
  if (!all(aplicable)) x_analisis[!aplicable] <- NA
  lista <- .normalizar_sentinelas_numericos(guarda$sentinelas)
  disfrazados <- .faltantes_disfrazados_columna(
    x_analisis,
    inferencia = list(
      tipo = as.character(guarda$tipo)[[1L]],
      convencion_numero_texto = guarda$convencion
    ),
    formatos = NULL,
    secuencia_entera = list(
      densa = isTRUE(guarda$densa),
      moda_sobresale = isTRUE(guarda$moda_sobresale)
    ),
    sentinelas_numericos = lista,
    sentinelas_declarados = if (isTRUE(guarda$declarada)) lista else numeric(),
    cadenas_ausencia = cadenas_ausencia,
    aplicable = aplicable
  )
  disfrazados$sentinelas_contados
}

# El universo aplicable para la guarda. Sin guarda no hace falta: la lista ya es
# la que el perfil conto dentro de su universo, y las celdas de fuera se
# devuelven despues (`.restringir_a_aplicabilidad()`). Con guarda es obligatorio:
# sin la mascara el detector contaria los centinelas de las filas de fuera, y
# las banderas de la numeracion describen las filas aplicables. Si el perfil
# recorto el universo y la accion no trae la regla, no se puede acotar, y se
# dice en lugar de convertir la lista entera.
.mascara_universo_guarda <- function(datos, original, columna, parametros) {
  guarda <- parametros$guarda
  if (!is.list(guarda)) return(NULL)
  if (is.null(parametros$aplicabilidad)) {
    if (isTRUE(guarda$universo_recortado)) {
      .detener(
        "La acci\u00f3n no se puede acotar a lo que el perfil cont\u00f3: el perfil ",
        "cont\u00f3 los sentinelas de `", columna, "` dentro de un universo ",
        "aplicable y la acci\u00f3n no trae esa regla en `aplicabilidad`. Volver ",
        "a planificar desde el perfil, o reponer la regla.", call. = FALSE
      )
    }
    return(NULL)
  }
  if (nrow(original) != nrow(datos)) {
    .detener(
      "No se puede respetar la aplicabilidad despu\u00e9s de eliminar filas: ",
      "aplicar las acciones por celda antes de las que eliminan filas.",
      call. = FALSE
    )
  }
  .mascara_aplicabilidad_accion(original, columna, parametros)
}

# La columna sobre la que decide la guarda: la de los datos que `aplicar()`
# recibio (ver `.valores_sentinelas_accion()`). Si no se la puede ubicar ahi
# con el mismo largo -una accion anterior elimino filas-, la del estado actual.
.columna_para_guarda <- function(original, columna, x, parametros) {
  if (!is.list(parametros$guarda) || !inherits(original, "data.frame") ||
      is.na(columna)) {
    return(x)
  }
  indice <- .indice_nombre(columna, names(original))
  if (length(indice) != 1L || is.na(indice)) return(x)
  recibida <- original[[indice]]
  if (NROW(recibida) != NROW(x)) return(x)
  recibida
}

# Las celdas que el detector cuenta como MARCA TEXTUAL de ausencia. No son de
# esta accion aunque se lean como uno de sus numeros: con `cadenas_ausencia =
# "999"` y la misma cifra escrita `"999"` seis veces y `"999.0"` cinco, el
# perfil cuenta seis textuales y cinco numericos, y la accion numerica sola
# convertia las once. Es la mascara que usa `convertir_ausencias_textuales`,
# calculada por el detector.
.mascara_marcas_textuales <- function(x, parametros, aplicable = NULL) {
  texto <- .texto_analizable(x)$valores
  if (!is.null(aplicable) && length(aplicable) == length(texto) &&
      !all(aplicable)) {
    texto[!aplicable] <- NA
  }
  .detectar_faltantes_disfrazados(
    texto, sentinelas_numericos = numeric(),
    detectar_sentinelas_numericos = FALSE,
    cadenas_ausencia = parametros$cadenas_ausencia
  )$mascara_textual
}

.mascara_sentinelas_numericos <- function(x, parametros, aplicable = NULL,
                                          x_guarda = x) {
  valores <- .valores_sentinelas_accion(x, parametros, aplicable, x_guarda)
  if (is.character(x) || is.factor(x)) {
    # Con la convencion decimal que el plan registro, la lectura del detector:
    # ver `.leer_numeros_texto_centinela()`.
    texto <- trimws(.texto_para_operar(x))
    numeros <- .leer_numeros_texto_centinela(
      texto, parametros$convencion
    )
    identidad <- .identidades_sentinelas_texto(
      texto, valores, parametros$convencion, valores_leidos = numeros
    )
    mascara <- !is.na(x) & !is.na(identidad)
    if (any(mascara)) {
      mascara <- mascara & !.mascara_marcas_textuales(x, parametros, aplicable)
    }
    return(mascara)
  } else if (
    (is.numeric(x) || inherits(x, c("difftime", "units"))) &&
      !inherits(x, c("Date", "POSIXt"))
  ) {
    # Las mismas clases que el detector: `is.numeric()` da FALSE sobre una
    # duracion y sobre una medida con unidad, y la accion que el perfil
    # proponia para ellas quedaba `fallida`. Se compara el numero, como el
    # detector; al asignar `NA` la columna conserva su clase y su unidad.
    numeros <- as.numeric(x)
  } else {
    .detener("Los sentinelas num\u00e9ricos requieren texto o n\u00fameros.", call. = FALSE)
  }
  !is.na(x) & !is.na(numeros) & numeros %in% valores
}

.reemplazar_sentinelas_numericos <- function(x, parametros, aplicable = NULL,
                                             x_guarda = x) {
  mascara <- .mascara_sentinelas_numericos(x, parametros, aplicable, x_guarda)
  if (is.factor(x)) x <- as.character(x)
  x[mascara] <- NA
  list(valor = x, n = sum(mascara))
}

# El mapa ENMASCARADO sobre los datos que recibe `aplicar()`. El plan publico el
# mapa con algunas celdas tapadas -una sucursal que se llama como un titular
# protegido-, y la relacion se reconstruia entera sobre los datos recibidos, sin
# la validacion que la ayuda promete: sobre otra entrega, la accion que sin
# proteccion queda `fallida` -una fila PUBLICADA en claro que los datos
# contradicen, o la fila tapada con otro valor- salia `ejecutada` y escribia
# valores que el plan nunca mostro, e imputaba una clave nueva que no estaba en
# el plan. Medido. Ahora hace lo mismo que sin proteccion:
#
#   * las filas que el plan publica EN CLARO son el mapa, tal cual, y se validan
#     como sin proteccion;
#   * una fila tapada se resuelve con el par de los datos recibidos que tiene su
#     forma -lo visible, igual; lo tapado, cualquier texto- y que, tapado con los
#     valores protegidos de esos mismos datos, da exactamente lo que el plan
#     publico. Si un par tiene la clave de la fila y su valor no la reproduce,
#     los datos contradicen la dependencia, como sin proteccion -o no traen el
#     valor protegido que la tapo: tampoco se puede validar-. Si la reproducen
#     mas pares que filas tapadas con esa forma, no se puede saber cual esta en
#     el plan, y tambien se detiene;
#   * una clave que no esta en el plan -ni en claro ni con la forma de una fila
#     tapada- no se imputa, como sin proteccion.
#
# Sin el mapa publicado -un plan de otra version- se reconstruye como antes.
.resolver_mapa_enmascarado <- function(datos, parametros, i_determinante,
                                       i_dependiente, protegidas = character()) {
  publicado <- parametros$mapa
  if (!inherits(publicado, "data.frame") ||
      !all(c("determinante", "dependiente") %in% names(publicado))) {
    return(.mapa_dependencia(
      datos, names(datos)[[i_determinante]], names(datos)[[i_dependiente]],
      soporte_minimo = parametros$soporte_minimo
    ))
  }
  marca <- "[valor protegido]"
  texto <- function(x) {
    salida <- tryCatch(as.character(x), error = function(e) {
      rep(NA_character_, length(x))
    })
    salida[is.na(x)] <- NA_character_
    salida
  }
  # Una celda tapada lleva el marcador; una numerica tapada, `NA` -un par del
  # mapa nunca tiene un lado ausente-.
  tapada_celda <- function(x) is.na(x) | grepl(marca, x, fixed = TRUE)
  igual <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  texto_det <- texto(publicado$determinante)
  texto_dep <- texto(publicado$dependiente)
  tapada <- tapada_celda(texto_det) | tapada_celda(texto_dep)
  claras <- publicado[!tapada, c("determinante", "dependiente"), drop = FALSE]
  if (!any(tapada)) return(claras)
  x <- datos[[i_determinante]]
  y <- datos[[i_dependiente]]
  conocidos <- !is.na(x) & !is.na(y)
  pares <- data.frame(determinante = x[conocidos], dependiente = y[conocidos],
                      stringsAsFactors = FALSE)
  pares <- pares[!duplicated(data.frame(
    a = .valores_relacion(pares$determinante),
    b = .valores_relacion(pares$dependiente)
  )), , drop = FALSE]
  # Las claves publicadas en claro ya son mapa: sus pares los valida el ejecutor.
  pares <- pares[!.valores_relacion(pares$determinante) %in%
                   .valores_relacion(claras$determinante), , drop = FALSE]
  # Lo visible, literal; lo tapado, cualquier texto. Un `NA` publicado es un
  # numero tapado: cualquier valor.
  forma <- function(publicada) {
    if (is.na(publicada)) return("^[\\s\\S]*$")
    # Tantas partes como marcadores mas uno: `strsplit()` no devuelve la parte
    # vacia del final.
    n_marcas <- lengths(regmatches(
      publicada, gregexpr(marca, publicada, fixed = TRUE)
    ))
    partes <- strsplit(publicada, marca, fixed = TRUE)[[1L]]
    partes <- c(partes, rep("", n_marcas + 1L - length(partes)))
    literal <- gsub("(\\W)", "\\\\\\1", partes, perl = TRUE)
    paste0("^", paste(literal, collapse = "[\\s\\S]+"), "$")
  }
  # Lo que el plan publicaria de cada par, con los valores protegidos de los
  # datos recibidos: el piso, y si la accion es sobre una columna protegida,
  # todos sus valores, como el barrido por columna del plan.
  protegidas <- as.character(protegidas)
  determinante_protegida <- .nombres_para_operar(
    names(datos)[[i_determinante]]
  ) %in% .nombres_para_operar(protegidas)
  dependiente_protegida <- .nombres_para_operar(
    names(datos)[[i_dependiente]]
  ) %in% .nombres_para_operar(protegidas)
  valores <- .valores_publicables_protegidos(datos, protegidas)
  identificantes <- .valores_identificantes(
    valores, .fechas_que_identifican(datos, protegidas)
  )
  ver <- function(tabla) {
    vistos <- list(mapa = tabla)
    vistos <- .proteger_textos_salida(vistos, identificantes,
                                      intocables = names(datos))
    vistos <- .proteger_numeros_parametros(vistos, identificantes)
    if (.nombres_para_operar(names(datos)[[i_dependiente]]) %in%
        .nombres_para_operar(protegidas)) {
      todos <- unique(c(valores, .rellenos_de_columnas(datos, protegidas)))
      vistos <- .proteger_textos_salida(vistos, todos, intocables = names(datos))
      vistos <- .proteger_numeros_parametros(vistos, todos)
    }
    if (determinante_protegida || dependiente_protegida) {
      vistos$mapa <- .enmascarar_mapa_lados_protegidos(
        vistos$mapa, determinante_protegida, dependiente_protegida
      )
    }
    vistos$mapa
  }
  grupos <- unique(data.frame(determinante = texto_det[tapada],
                              dependiente = texto_dep[tapada],
                              stringsAsFactors = FALSE))
  filas_grupo <- vapply(seq_len(nrow(grupos)), function(g) {
    sum(igual(texto_det[tapada], grupos$determinante[[g]]) &
          igual(texto_dep[tapada], grupos$dependiente[[g]]))
  }, integer(1L))
  # Una fila tapada que los datos recibidos NO resuelven -no traen ningun par de
  # su clave, o no traen el valor protegido que la tapo- no se imputa, y se
  # salteaba en silencio: con la entrega que trae solo las filas a completar, la
  # accion salia `ejecutada` con 12 celdas de 24 -las de esa clave quedaban NA,
  # sin motivo-; y sin el par ni el titular, `ejecutada` con 11 de 22 donde la
  # ayuda promete `fallida`. Medido. Si los datos piden imputar
  # una clave -dependiente ausente- que no cubren ni el mapa en claro ni una
  # fila tapada resuelta, y que tiene la forma de una fila tapada sin resolver,
  # la accion no puede hacer lo que el plan dice: queda `fallida`, con el
  # motivo. Una clave que no tiene la forma de ninguna fila del plan no se
  # imputa, como sin proteccion.
  detener_si_falta <- function(resueltas, sin_resolver) {
    if (!any(sin_resolver)) return(invisible())
    pedidas <- !is.na(x) & is.na(y)
    if (!any(pedidas)) return(invisible())
    claves <- x[pedidas]
    claves <- claves[!duplicated(.valores_relacion(claves))]
    cubiertas <- .valores_relacion(claves) %in%
      .valores_relacion(claras$determinante) |
      .valores_relacion(claves) %in%
        .valores_relacion(pares$determinante[resueltas])
    claves <- claves[!cubiertas]
    if (!length(claves)) return(invisible())
    texto_claves <- texto(claves)
    vistas <- texto(ver(data.frame(
      determinante = claves, dependiente = rep(NA, length(claves)),
      stringsAsFactors = FALSE
    ))$determinante)
    for (g in which(sin_resolver)) {
      det_g <- grupos$determinante[[g]]
      piden <- grepl(forma(det_g), texto_claves, perl = TRUE) &
        igual(vistas, det_g)
      if (any(piden)) {
        n_celdas <- sum(.valores_relacion(x[pedidas]) %in%
                          .valores_relacion(claves[piden]))
        .detener(
          "La dependencia funcional del plan no se puede aplicar sobre los ",
          "datos recibidos: ", n_celdas,
          if (n_celdas == 1L) " celda para imputar tiene una " else
            " celdas para imputar tienen una ",
          "clave cuya fila el plan public\u00f3 tapada, y los datos no traen ",
          "un par de esa clave que la resuelva -ni, quiz\u00e1, el valor ",
          "protegido que la tap\u00f3-. Sin proteger, el plan las ",
          "escribir\u00eda. Para aplicarla, planificar sobre datos que traigan ",
          "esa relaci\u00f3n.", call. = FALSE
        )
      }
    }
    invisible()
  }
  if (!nrow(pares)) {
    detener_si_falta(integer(), filas_grupo > 0L)
    return(claras)
  }
  # Lo que resuelve una fila tapada es la relacion de los datos con el soporte
  # declarado -como se armo el mapa del plan-; todos los pares, en cambio,
  # cuentan para la contradiccion.
  reconstruido <- .mapa_dependencia(
    datos, names(datos)[[i_determinante]], names(datos)[[i_dependiente]],
    soporte_minimo = parametros$soporte_minimo
  )
  en_relacion <- paste(.valores_relacion(pares$determinante),
                       .valores_relacion(pares$dependiente), sep = "\001") %in%
    paste(.valores_relacion(reconstruido$determinante),
          .valores_relacion(reconstruido$dependiente), sep = "\001")
  vistos <- ver(pares)
  visto_det <- texto(vistos$determinante)
  visto_dep <- texto(vistos$dependiente)
  par_det <- texto(pares$determinante)
  par_dep <- texto(pares$dependiente)
  resueltas <- integer()
  sin_resolver <- rep(FALSE, nrow(grupos))
  for (g in seq_len(nrow(grupos))) {
    det_g <- grupos$determinante[[g]]
    dep_g <- grupos$dependiente[[g]]
    con_forma <- which(
      grepl(forma(det_g), par_det, perl = TRUE) &
        grepl(forma(dep_g), par_dep, perl = TRUE)
    )
    clave <- con_forma[igual(visto_det[con_forma], det_g)]
    reproducen <- clave[igual(visto_dep[clave], dep_g)]
    reproducen <- reproducen[en_relacion[reproducen]]
    if (length(clave[!igual(visto_dep[clave], dep_g)])) {
      .detener(
        "Los datos actuales contradicen la dependencia funcional del plan: ",
        "una fila que el plan public\u00f3 tapada no se reproduce sobre los ",
        "datos recibidos -la relaci\u00f3n cambi\u00f3, o los datos no traen el ",
        "valor protegido que la tap\u00f3-. Para aplicarla, planificar sobre ",
        "estos datos.", call. = FALSE
      )
    }
    if (length(reproducen) > filas_grupo[[g]]) {
      .detener(
        "La dependencia funcional del plan no se puede resolver sobre los ",
        "datos recibidos: m\u00e1s claves de las que el plan public\u00f3 tapadas ",
        "tienen la misma forma, y no se puede saber cu\u00e1l est\u00e1 en el plan.",
        call. = FALSE
      )
    }
    sin_resolver[[g]] <- length(reproducen) < filas_grupo[[g]]
    resueltas <- c(resueltas, reproducen)
  }
  detener_si_falta(unique(resueltas), sin_resolver)
  rbind(claras, pares[unique(resueltas), , drop = FALSE])
}

.imputar_dependencia <- function(datos, parametros, protegidas = character()) {
  determinante <- parametros$determinante
  dependiente <- parametros$dependiente
  indices_dependencia <- .indice_nombre(
    c(determinante, dependiente), names(datos)
  )
  if (anyNA(indices_dependencia)) {
    .detener("La imputaci\u00f3n no conserva un contrato de dependencia v\u00e1lido.",
         call. = FALSE)
  }
  determinante <- names(datos)[indices_dependencia[[1L]]]
  dependiente <- names(datos)[indices_dependencia[[2L]]]
  mapa <- if (isTRUE(parametros$mapa_enmascarado)) {
    soporte <- parametros$soporte_minimo
    if (length(soporte) != 1L || is.na(soporte) ||
        !is.finite(soporte) || soporte < 1L) {
      .detener("La imputaci\u00f3n no conserva un soporte de dependencia v\u00e1lido.",
           call. = FALSE)
    }
    .resolver_mapa_enmascarado(
      datos, parametros, indices_dependencia[[1L]], indices_dependencia[[2L]],
      protegidas
    )
  } else {
    parametros$mapa
  }
  if (!inherits(mapa, "data.frame") ||
      !all(c("determinante", "dependiente") %in% names(mapa))) {
    .detener("La imputaci\u00f3n no conserva un contrato de dependencia v\u00e1lido.",
         call. = FALSE)
  }
  if (isTRUE(parametros$mapa_enmascarado) && !nrow(mapa)) {
    .detener(
      "La dependencia funcional del plan no se puede resolver sobre los datos recibidos.",
      call. = FALSE
    )
  }
  # Por indice: un nombre marcado `bytes` no se traduce (ver
  # `.mapa_dependencia()`).
  i_determinante <- indices_dependencia[[1L]]
  i_dependiente <- indices_dependencia[[2L]]
  indices <- match(
    .valores_relacion(datos[[i_determinante]]),
    .valores_relacion(mapa$determinante)
  )
  conocidos <- !is.na(datos[[i_dependiente]]) & !is.na(indices)
  esperado <- mapa$dependiente[indices[conocidos]]
  if (any(.valores_relacion(datos[[i_dependiente]][conocidos]) !=
          .valores_relacion(esperado))) {
    .detener("Los datos actuales contradicen la dependencia funcional del plan.",
         call. = FALSE)
  }
  imputar <- is.na(datos[[i_dependiente]]) & !is.na(indices)
  salida <- datos[[i_dependiente]]
  if (is.factor(salida)) {
    texto <- as.character(salida)
    texto[imputar] <- as.character(mapa$dependiente[indices[imputar]])
    salida <- texto
  } else {
    salida[imputar] <- mapa$dependiente[indices[imputar]]
  }
  datos[[i_dependiente]] <- .con_atributos_de(datos[[i_dependiente]], salida)
  list(datos = datos, n = sum(imputar))
}

# Cuenta las celdas que REALMENTE cambiaron, comparando bytes.
#
# `anterior != nuevo` compara TEXTO, y el texto distingue la marca de
# codificacion: una celda declarada `bytes` que la accion no toco quedaba
# contada como cambiada porque su marca difiere de la del resultado, aunque los
# bytes fueran identicos. Medido: `eliminar_controles_invisibles` y
# `normalizar_espacios_invisibles` informaban `n_cambiadas = 2` donde el plan
# habia anunciado 1, sobre un dato donde solo una celda cambio.
#
# Que el paquete diga haber cambiado una celda que no cambio es informar como
# hecho lo que no hizo, un piso mas arriba de lo que mide.
#
# Se confirma por bytes SOLO donde `!=` acuso, que es un conjunto chico: la
# comprobacion cara no corre sobre la columna entera.
.celdas_cambiadas <- function(anterior, nuevo) {
  anterior <- as.character(anterior)
  nuevo <- as.character(nuevo)
  # `!=` sobre una cadena marcada `bytes` ABORTA en R 4.1
  # con "translating strings with bytes encoding is not allowed", y en 4.6 no.
  # Mientras esta funcion solo se llamaba desde los tres ejecutores de texto el
  # camino con marca no llegaba aca; desde que la perdida se mide en la puerta
  # comun, si. Se compara por la clave de bytes, que es lo que el paquete ya usa
  # para esto y no traduce nada. La comparacion por `charToRaw` de mas abajo se
  # conserva: distingue dos cadenas de los mismos bytes con marcas distintas.
  clave_anterior <- .clave_bytes(anterior)
  clave_nueva <- .clave_bytes(nuevo)
  cambio <- !is.na(anterior) &
    (is.na(nuevo) | clave_anterior != clave_nueva)
  sospechosas <- which(cambio & !is.na(nuevo))
  if (length(sospechosas)) {
    iguales <- vapply(
      sospechosas,
      function(i) identical(charToRaw(anterior[[i]]), charToRaw(nuevo[[i]])),
      logical(1L)
    )
    cambio[sospechosas[iguales]] <- FALSE
  }
  cambio
}

.transformar_con_posiciones_protegidas <- function(texto, consumidas,
                                                   transformar) {
  codigos <- .codigos_decodificables(texto)
  if (is.null(codigos) || length(consumidas) != length(codigos) ||
      !any(consumidas)) {
    return(transformar(texto)[[1L]])
  }
  protegidas <- consumidas
  continuaciones <- .ftfy_representaciones_continuacion
  for (indice in which(consumidas)) {
    anterior <- indice - 1L
    while (anterior >= 1L && codigos[[anterior]] %in% continuaciones) {
      protegidas[[anterior]] <- TRUE
      anterior <- anterior - 1L
    }
    if (anterior >= 1L && codigos[[anterior]] %in% 0xC2L:0xF4L &&
        codigos[[anterior + 1L]] %in% continuaciones) {
      protegidas[[anterior]] <- TRUE
    }
  }
  indices <- which(protegidas)
  base <- if (length(indices) <= 0xF00L) 0xE000L else 0xF0000L
  marcas <- base + seq_along(indices) - 1L
  enmascarado <- codigos
  enmascarado[indices] <- marcas
  texto_enmascarado <- paste0(
    intToUtf8(enmascarado, multiple = TRUE), collapse = ""
  )
  transformado <- transformar(texto_enmascarado)[[1L]]
  salida <- .codigos_decodificables(transformado)
  if (is.null(salida)) return(texto)
  posiciones <- match(marcas, salida)
  if (anyNA(posiciones)) return(texto)
  salida[posiciones] <- codigos[indices]
  paste0(intToUtf8(salida, multiple = TRUE), collapse = "")
}

# Repara solamente las celdas que una accion ya sabe que va a tocar. La
# deteccion vectorizada evita llamar al reparador sobre texto limpio.
.ftfy_mascara_para_texto <- function(textos, trabajo = NULL) {
  if (is.null(trabajo)) trabajo <- .texto_para_operar(textos)
  .ftfy_mascara_candidatos(trabajo)
}

.reparar_textos_seleccionados <- function(textos, seleccion) {
  salida <- textos
  if (!length(textos) || length(seleccion) != length(textos) ||
      !any(seleccion, na.rm = TRUE)) {
    return(salida)
  }
  indices <- which(!is.na(seleccion) & seleccion & !is.na(textos))
  if (!length(indices)) return(salida)
  trabajo <- .texto_para_operar(textos)
  candidatos <- .ftfy_mascara_para_texto(
    textos[indices], trabajo = trabajo[indices]
  )
  if (!any(candidatos)) return(salida)
  posiciones <- indices[candidatos]
  reparados <- .analizar_codificacion(trabajo[posiciones])$reparados
  validos <- !is.na(reparados)
  if (any(validos)) salida[posiciones[validos]] <- reparados[validos]
  salida
}

.proteccion_texto_valores <- function(valores, codificacion = NULL) {
  trabajo <- .texto_para_operar(valores)
  candidatos <- .ftfy_mascara_para_texto(valores, trabajo = trabajo)
  mascaras <- vector("list", length(valores))
  indices <- which(candidatos)
  if (length(indices)) {
    candidatos_cod <- if (!is.null(codificacion) &&
                          is.logical(codificacion$candidatos_valores) &&
                          length(codificacion$candidatos_valores) == length(valores)) {
      codificacion$candidatos_valores
    } else if (!is.null(codificacion) &&
               is.logical(codificacion$candidatos) &&
               length(codificacion$candidatos) == length(valores)) {
      codificacion$candidatos
    } else NULL
    reparados_cod <- if (!is.null(codificacion) &&
                         length(codificacion$reparados_valores) == length(valores)) {
      codificacion$reparados_valores
    } else if (!is.null(codificacion) &&
               length(codificacion$reparados) == length(valores)) {
      codificacion$reparados
    } else NULL
    estados_cod <- if (!is.null(codificacion) &&
                       length(codificacion$estados_valores) == length(valores)) {
      codificacion$estados_valores
    } else if (!is.null(codificacion) &&
               length(codificacion$estados) == length(valores)) {
      codificacion$estados
    } else NULL
    consumidas_cod <- if (!is.null(codificacion) &&
                          length(codificacion$consumidas_valores) == length(valores)) {
      codificacion$consumidas_valores
    } else if (!is.null(codificacion) &&
               length(codificacion$consumidas) == length(valores)) {
      codificacion$consumidas
    } else NULL
    if (!is.null(candidatos_cod) && !is.null(reparados_cod) &&
        !is.null(estados_cod) && !is.null(consumidas_cod)) {
      calculadas <- lapply(indices, function(i) {
        .ftfy_posiciones_consumidas(
          trabajo[[i]], reparado = reparados_cod[[i]],
          consumidas = consumidas_cod[[i]], estado = estados_cod[[i]]
        )
      })
    } else {
      analisis <- .analizar_codificacion(trabajo[indices])
      calculadas <- .ftfy_posiciones_consumidas_vector(
        trabajo[indices], codificacion = analisis
      )
    }
    mascaras[indices] <- calculadas
  }
  list(candidatos = candidatos, mascaras = mascaras)
}

# El perfil usa la misma puerta que la accion para contar una celda: la misma
# funcion, con la misma mascara y los mismos argumentos que `aplicar()`. Asi una
# guarda que protege un tramo de mojibake no puede dejar un hallazgo que luego
# la accion no tiene permitido cambiar, y `n_afectadas` del plan es lo que la
# accion sola va a cambiar. Con una variante para contar y otra para aplicar,
# el plan ofrecia marcada una accion que despues terminaba `fallida`.
#
# La excepcion es `entidades_html`, y es a proposito: la entidad se informa
# aunque decodificarla junto a una letra rota no conmute con la reparacion (la
# letra decodificada al lado impide reconocer la rota). El hallazgo no es de
# codificacion y no tiene que desaparecer por ella; la accion no viene marcada,
# y con la reparacion marcada -que corre primero- si decodifica la celda. Sola,
# sobre esa celda, no la toca.
.celdas_texto_protegidas <- function(textos, estrategia, proteccion = NULL) {
  if (!length(textos)) return(logical())
  candidatos <- switch(
    estrategia,
    recortar_espacios = .celdas_cambiadas(
      textos, .recortar_borde_texto(textos)
    ),
    decodificar_entidades_html = .entidades_html_en_texto(textos),
    .detener("Estrategia de texto desconocida.", call. = FALSE)
  )
  salida <- rep(FALSE, length(textos))
  indices <- which(candidatos)
  if (!length(indices)) return(salida)
  if (is.null(proteccion)) {
    proteccion <- .proteccion_texto_valores(textos[indices])
  } else if (length(proteccion$candidatos) == length(textos)) {
    proteccion <- list(
      candidatos = proteccion$candidatos[indices],
      mascaras = proteccion$mascaras[indices]
    )
  }
  textos_candidatos <- textos[indices]
  cambio <- switch(
    estrategia,
    recortar_espacios = .recortar_texto(textos_candidatos, proteccion),
    decodificar_entidades_html = .decodificar_entidades_html(
      textos_candidatos, proteccion, proteger_conmutacion = FALSE
    ),
    .detener("Estrategia de texto desconocida.", call. = FALSE)
  )
  salida[indices] <- .celdas_cambiadas(textos_candidatos, cambio$valor)
  salida
}

.accion_fabrica_secuencia <- function(anterior, nuevo) {
  anterior_trabajo <- .texto_para_operar(anterior)
  nuevo_trabajo <- .texto_para_operar(nuevo)
  entrada <- .codigos_decodificables(anterior_trabajo)
  salida <- .codigos_decodificables(nuevo_trabajo)
  if (is.null(entrada) || is.null(salida) ||
      identical(anterior_trabajo, nuevo_trabajo)) {
    return(FALSE)
  }
  antes <- .ftfy_reparar_uno(anterior_trabajo)
  despues <- .ftfy_reparar_uno(nuevo_trabajo)
  !identical(antes$estado, "reparado") &&
    identical(despues$estado, "reparado") &&
    !identical(despues$texto, nuevo_trabajo)
}

# Protege la propiedad de que una accion de texto no cambie la lectura de la
# reparacion. `transformar` es la accion sin mascara y `enmascarar` reconstruye
# solo las celdas donde la accion tocaria un punto consumido por la reparacion.
# Si la accion crea una secuencia reparable, la celda queda intacta: no se
# puede convertir una accion elegida por el usuario en una reparacion implicita.
#
# `proteger_conmutacion = FALSE` deja solo la proteccion por posicion, sin la
# comparacion de los dos ordenes. Lo usa unicamente el conteo de las entidades
# HTML del perfil (ver `.celdas_texto_protegidas()`); ninguna accion se aplica
# asi.
.proteger_accion_texto <- function(anterior, transformar, enmascarar = NULL,
                                  proteccion = NULL,
                                  revisar_nuevas = "siempre",
                                  proteger_conmutacion = TRUE) {
  nuevo <- transformar(anterior)
  tocar <- .celdas_cambiadas(anterior, nuevo)
  resultado <- function(valor, cambiadas) {
    list(valor = valor, cambiadas = cambiadas)
  }
  if (!any(tocar)) return(resultado(nuevo, tocar))
  indices <- which(tocar & !is.na(anterior))
  if (!length(indices)) return(resultado(nuevo, tocar))
  candidatos <- if (!is.null(proteccion) &&
                    length(proteccion$candidatos) == length(anterior)) {
    proteccion$candidatos[indices]
  } else {
    .ftfy_mascara_para_texto(anterior[indices])
  }
  nuevas <- logical(length(indices))
  indices_nuevas <- which(!candidatos)
  if (length(indices_nuevas) && identical(revisar_nuevas, "siempre")) {
    nuevas[indices_nuevas] <- .ftfy_mascara_para_texto(
      nuevo[indices[indices_nuevas]]
    )
  } else if (length(indices_nuevas) && identical(revisar_nuevas, "si_hay_A")) {
    posibles <- grepl("[\u00c2\u00c3]", nuevo[indices[indices_nuevas]], perl = TRUE)
    if (any(posibles)) {
      # `posibles` recorre `indices_nuevas`, no `indices`: indexar `nuevas`
      # con el dejaba la respuesta en otra celda cuando la columna traia
      # tambien candidatas, y una celda limpia quedaba sin proteger.
      ubicaciones <- indices_nuevas[posibles]
      nuevas[ubicaciones] <- .ftfy_mascara_para_texto(nuevo[indices[ubicaciones]])
    }
  }
  if (!any(candidatos | nuevas)) {
    return(resultado(nuevo, tocar))
  }
  reparado <- .reparar_textos_seleccionados(anterior, tocar)
  consumidos <- if (!is.null(proteccion) &&
                    is.list(proteccion$mascaras) &&
                    length(proteccion$mascaras) == length(anterior)) {
    proteccion$mascaras
  } else {
    .ftfy_posiciones_consumidas_vector(
      .texto_para_operar(anterior),
      solo = tocar
    )
  }
  protegido <- nuevo
  proteger_entera <- rep(FALSE, length(indices))
  # Una celda que el perfil ya senalo como codificacion rota se conserva entera
  # solo si la comparacion de ordenes demuestra que la accion no conmuta con
  # la reparacion. La decision se toma mas abajo, despues de calcular ambos
  # ordenes; una accion que si conmuta puede seguir limpiando un control o un
  # separador genuino fuera de la parte rota.
  for (j in seq_along(indices)) {
    i <- indices[[j]]
    mascara <- consumidos[[i]]
    if (isTRUE(nuevas[[j]]) &&
               (!length(mascara) || !any(mascara)) &&
               .accion_fabrica_secuencia(anterior[[i]], nuevo[[i]])) {
      # Si quitar el invisible vuelve reparable la celda, no hay una posicion
      # previa que pueda enmascararse: se conserva la celda completa.
      protegido[[i]] <- anterior[[i]]
      proteger_entera[[j]] <- TRUE
    }
  }
  if (!is.null(enmascarar)) {
    for (j in which(!proteger_entera)) {
      i <- indices[[j]]
      mascara <- consumidos[[i]]
      if (length(mascara) && any(mascara)) {
        protegido[[i]] <- enmascarar(anterior[[i]], mascara)
      }
    }
  } else {
    for (j in which(!proteger_entera)) {
      i <- indices[[j]]
      mascara <- consumidos[[i]]
      if (length(mascara) && any(mascara)) {
        protegido[[i]] <- .transformar_con_posiciones_protegidas(
          anterior[[i]], mascara, transformar
        )
      }
    }
  }
  accion_reparada <- transformar(reparado)
  for (j in which(!proteger_entera)) {
    i <- indices[[j]]
    mascara <- consumidos[[i]]
    codigos <- .codigos_decodificables(reparado[[i]])
    if (is.null(enmascarar) || !length(mascara) || is.null(codigos) ||
        length(mascara) != length(codigos)) next
    accion_reparada[[i]] <- transformar(
      enmascarar(reparado[[i]], mascara)
    )[[1L]]
  }
  reparado_protegido <- .reparar_textos_seleccionados(protegido, tocar)
  diferencias <- .celdas_cambiadas(reparado_protegido, accion_reparada)
  if (any(diferencias)) {
    # Una celda que la reparacion no puede tocar se deja a la accion: su
    # control genuino se quita. Salvo que quitarlo la vuelva reparable: es la
    # misma regla que para una celda nueva (`.accion_fabrica_secuencia()`), y
    # sin ella "Mar" + U+0081 + "\u00c3\u00ada" daba otra cosa segun la accion
    # corriera antes o despues de la reparacion.
    revertir <- vapply(which(diferencias), function(i) {
      j <- match(i, indices)
      if (isTRUE(proteger_conmutacion)) {
        !is.na(j) && (isTRUE(candidatos[[j]]) || isTRUE(nuevas[[j]])) &&
          (!identical(
            .texto_para_operar(reparado[[i]]),
            .texto_para_operar(anterior[[i]])
          ) || isTRUE(nuevas[[j]]) ||
            .accion_fabrica_secuencia(anterior[[i]], protegido[[i]]))
      } else {
        FALSE
      }
    }, logical(1L), USE.NAMES = FALSE)
    if (any(revertir)) {
      protegidos <- which(diferencias)[revertir]
      protegido[protegidos] <- anterior[protegidos]
    }
  }
  resultado(protegido, .celdas_cambiadas(anterior, protegido))
}

# Las protecciones de `aplicar()` son una cache por columna, y no se calcula
# nada por adelantado: la mascara de una columna se arma la primera vez que una
# accion de texto la pide, sobre los valores que la columna tiene en ese
# momento, y se descarta cuando una accion la cambia. Prepararla al empezar para
# toda columna con una accion marcada le hacia pagar la mascara de cada fila a
# un plan que solo reparaba la codificacion o marcaba duplicados: `aplicar()`
# tardaba casi cuatro veces mas sobre 200.000 filas sin que ninguna accion la
# usara. Tampoco hay una lista de acciones que decida a cual se le prepara: la
# paga la que la usa.
.preparar_protecciones_texto <- function() {
  protecciones <- new.env(parent = emptyenv())
  protecciones$entradas <- list()
  protecciones
}

.indices_proteccion_texto <- function(entradas, columna) {
  if (!length(entradas)) return(integer())
  which(.nombres_para_operar(names(entradas)) %in%
          .nombres_para_operar(columna))
}

.proteccion_texto_columna <- function(protecciones, columna, valores) {
  if (is.null(protecciones) || length(columna) != 1L || is.na(columna) ||
      !is.character(valores) && !is.factor(valores)) {
    return(NULL)
  }
  entradas <- protecciones$entradas
  indices <- .indices_proteccion_texto(entradas, columna)
  if (length(indices) > 1L) return(NULL)
  if (length(indices) == 1L &&
      length(entradas[[indices]]$candidatos) == length(valores)) {
    return(entradas[[indices]])
  }
  entrada <- c(
    list(columna = columna), .proteccion_texto_valores(as.character(valores))
  )
  if (length(indices) == 1L) {
    protecciones$entradas[[indices]] <- entrada
  } else {
    protecciones$entradas <- c(
      entradas, stats::setNames(list(entrada), columna)
    )
  }
  entrada
}

# Despues de una accion que cambio la columna su mascara ya no vale: se
# descarta, y la proxima accion de texto que la pida la calcula de nuevo. Sin
# este paso la accion siguiente trabaja con posiciones del texto anterior.
.actualizar_proteccion_texto <- function(protecciones, datos, columna) {
  if (is.null(protecciones) || length(columna) != 1L || is.na(columna)) {
    return(protecciones)
  }
  indices <- .indices_proteccion_texto(protecciones$entradas, columna)
  if (length(indices)) protecciones$entradas[indices] <- NULL
  protecciones
}

.celdas_que_pierden_valor <- function(anterior, nuevo, cambiadas = NULL) {
  # Cuando una accion NORMALIZA la escritura -recortar espacios, quitar un
  # control invisible, unificar mayusculas-, el valor sigue en su lugar y la
  # documentacion la exime del conteo por nombre. Pero la misma accion pierde
  # informacion cuando **colapsa valores que eran distintos**: ahi lo que los
  # separaba no esta en ningun lado.
  #
  # Medido, las dos mitades: quitar un guion suave de `PRO<U+00AD>DUCTO-A` deja
  # las celdas identicas a su forma canonica y el cruce contra el catalogo
  # encuentra los cinco valores -no se perdio nada, y el registro contaba 4-; y
  # con claves marcadas por un espacio de ancho cero, la misma accion colapsa
  # cinco claves distintas en tres y fabrica dos duplicados que no existian
  # -ahi si se perdio, y contaba igual-. Una regla por accion no puede acertar
  # en los dos casos. Esta se mide sobre el resultado.
  #
  # `recortar_espacios` tenia el defecto simetrico: sobre `" ana "` y `"ana"`
  # los dos quedan `"ana"` -de cuatro distintos a tres- y el registro publicaba
  # cero irreversibles.
  if (is.null(cambiadas) || length(cambiadas) != length(anterior)) {
    cambiadas <- .celdas_cambiadas(anterior, nuevo)
  }
  if (!any(cambiadas)) return(cambiadas)
  # Se compara sobre la CLAVE por bytes, no sobre las cadenas. En R 4.1
  # `duplicated()` y `split()` sobre una cadena marcada
  # `bytes` abortan con "translating strings with bytes encoding is not
  # allowed", y esta regla corre sobre columnas de texto que pueden traer esa
  # marca. La clave ademas separa dos cadenas de los mismos bytes con marcas
  # distintas, que es lo que el paquete ya decidio en otro lado.
  antes <- .clave_bytes(as.character(anterior))
  despues <- .clave_bytes(as.character(nuevo))
  # Volverse ausente es perder el valor aunque no colapse con nadie.
  perdidas <- cambiadas & is.na(despues) & !is.na(antes)
  # `.clave_bytes()` conserva los `NA`, asi que la cuenta de ausencias sigue
  # saliendo de la clave sin mirar el original.
  # Solo pueden colapsar los valores nuevos que se REPITEN, asi que la particion
  # se limita a esos: sobre una columna de valores distintos -el caso comun- no
  # se arma ningun grupo. Partir la columna entera costaba por columna y por
  # accion, y este paquete ya se cuido de eso en el resumen cuantitativo.
  comparables <- !is.na(despues)
  repetidos <- comparables &
    (duplicated(despues) | duplicated(despues, fromLast = TRUE))
  candidatos <- which(repetidos)
  if (length(candidatos)) {
    grupos <- split(candidatos, despues[candidatos])
    for (grupo in grupos) {
      if (length(grupo) < 2L) next
      # Colapsan si los valores ORIGINALES del grupo no eran todos iguales.
      #
      # Se marca solo la celda que TENIA valor: si una celda ausente pasa a
      # valer lo mismo que otra -una imputacion-, no perdio nada, gano un
      # valor, y la accion que lo hizo ya declara lo suyo.
      if (length(unique(antes[grupo])) > 1L) {
        perdidas[grupo] <- perdidas[grupo] |
          (cambiadas[grupo] & !is.na(antes[grupo]))
      }
    }
  }
  perdidas
}

# El criterio de `n_no_reversibles` esta escrito en la documentacion: "se mide
# sobre el RESULTADO, no por el nombre de la accion". Los ejecutores que no
# devuelven la cuenta la dejaban en cero y `aplicar()` publicaba "0
# irreversibles" sobre columnas donde tres valores distintos habian quedado en
# uno. Medido sobre `ana|ANA|beto`: `convertir_mayusculas` funde dos celdas y
# publicaba 0, mientras `eliminar_controles_invisibles` -que pasa por
# `.celdas_que_pierden_valor()`- publicaba 1 con la misma fusion. Comparten la
# clase de operacion y contestaban distinto.
#
# Se mide aca, en la puerta comun, y no en cada ejecutor: asi la cuenta cubre
# tambien a la proxima estrategia que se agregue sin devolver el campo. Los
# ejecutores que SI la devuelven -las conversiones, los centinelas, la
# winsorizacion- conservan la suya, porque miden perdidas que no son fusion.
# Una accion de texto sobre celdas `latin1` devuelve UTF-8, y SOLO en las celdas
# que toco: la columna queda con marcas mixtas -medido, `UTF-8, latin1, UTF-8`- y
# el destino de la marca de cada celda lo decide su contenido, no una politica.
# El valor semantico se conserva -este paquete trata `latin1` como convertible sin
# perdida, y eso esta escrito y decidido-, pero el registro publicaba
# `n_cambiadas = 2`, `n_no_reversibles = 0` y NADA sobre la codificacion, mientras
# los bytes de una celda pasaban de `e9` a `c3 a9`.
#
# Se declara en vez de convertir la columna entera: convertir tocaria celdas que
# el plan no declaro como cambiadas, y entonces `n_cambiadas` mentiria por el otro
# lado. La eleccion es deliberada y queda escrita para que no se lea como olvido.
#
# Se mide en la MISMA puerta comun que las perdidas, no en cada ejecutor, asi la
# cuenta cubre tambien a la proxima estrategia de texto que se agregue.
.codificaciones_normalizadas <- function(anterior, nuevo, columna) {
  if (length(columna) != 1L || is.na(columna)) return(0L)
  if (!is.data.frame(anterior) || !is.data.frame(nuevo)) return(0L)
  indice_anterior <- .indice_nombre(columna, names(anterior))
  indice_nuevo <- .indice_nombre(columna, names(nuevo))
  if (is.na(indice_anterior) || is.na(indice_nuevo)) return(0L)
  x <- anterior[[indice_anterior]]
  y <- nuevo[[indice_nuevo]]
  if (!is.character(x) || !is.character(y) || length(x) != length(y)) {
    return(0L)
  }
  # Cuenta RECODIFICACIONES: celdas cuyo contenido quedo escrito en otra
  # codificacion -`latin1` que pasa a UTF-8-, no celdas cuya marca cambio. La
  # marca de un texto sin declarar la pone el locale: `trimws()` sobre bytes
  # UTF-8 `unknown` devuelve `UTF-8` en una sesion UTF-8 y `unknown` bajo `C`, y
  # el campo publicaba 1 o 0 para la misma salida, con los mismos bytes. Por eso
  # las dos puntas pasan por la declaracion de la normalizacion
  # -`.declarar_utf8_valido()`-: un `unknown` valido ya ES UTF-8 y no cambio de
  # codificacion. Y un texto ASCII se escribe igual en todas, asi que tampoco:
  # `Jos&eacute;` decodificado a `Jose` con tilde cambio de valor, no de codificacion.
  # Lo mismo del otro lado: una celda que queda ASCII -`a` con un espacio de ancho
  # cero que se quita, un espacio duro del borde que se recorta- cambio de valor y
  # ya no tiene codificacion que cambiar. Se contaba por mirar solo la entrada.
  x <- .declarar_utf8_valido(x)
  y <- .declarar_utf8_valido(y)
  no_ascii <- grepl("[^\\001-\\177]", x, useBytes = TRUE, perl = TRUE) &
    grepl("[^\\001-\\177]", y, useBytes = TRUE, perl = TRUE)
  as.integer(sum(!is.na(x) & !is.na(y) & no_ascii & Encoding(x) != Encoding(y)))
}

.perdidas_al_aplicar <- function(anterior, nuevo, columna) {
  if (length(columna) != 1L || is.na(columna)) return(0L)
  if (!is.data.frame(anterior) || !is.data.frame(nuevo)) return(0L)
  indice_anterior <- .indice_nombre(columna, names(anterior))
  indice_nuevo <- .indice_nombre(columna, names(nuevo))
  if (is.na(indice_anterior) || is.na(indice_nuevo)) return(0L)
  x <- anterior[[indice_anterior]]
  y <- nuevo[[indice_nuevo]]
  # Una accion que quita filas cambia el largo: su perdida se cuenta en
  # `n_filas_eliminadas`, no celda por celda.
  if (length(x) != length(y)) return(0L)
  as.integer(sum(.celdas_que_pierden_valor(x, y)))
}

# El recorte del borde sobre una columna, con la definicion de
# `.recortar_espacios_borde()`. Lo usan la accion y el conteo del perfil, que
# tienen que ver lo mismo: con dos copias de la transformacion, una podia
# cambiar sin la otra y el plan ofrecia una accion que despues no cambiaba nada.
#
# `trimws()` sobre un vector que contiene UNA cadena marcada `bytes` devuelve
# marcadas `bytes` tambien a las `latin1` que viajaban al lado, y ahi su
# contenido deja de ser recuperable: un `ca<f1>o ` valido volvia como
# `ca<f1>o` declarado `bytes`, que ya no es UTF-8 valido. El destino de una
# fila lo decidia lo que hubiera en el resto de la tanda. Se recorta cada
# grupo por separado para que ninguna marca contamine a la vecina. Lo
# declarado `bytes` que llega a recortarse es UTF-8 valido
# (`.aplicar_por_marca()` deja intacto lo demas), asi que se lee como UTF-8 para
# reconocer sus espacios Unicode -como ya lo lee
# `normalizar_espacios_invisibles`- y vuelve declarado `bytes`.
.recortar_borde_texto <- function(v) .aplicar_por_marca(
  v, .recortar_espacios_borde,
  function(z) {
    Encoding(z) <- "UTF-8"
    .recortar_espacios_borde(z)
  }
)

.recortar_texto <- function(x, proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("El recorte de espacios requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  transformar <- .recortar_borde_texto
  enmascarar <- function(texto, consumidos) {
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    if (is.null(consumidos) || length(consumidos) != length(codigos)) {
      consumidos <- rep(FALSE, length(codigos))
    }
    # Un caracter es espacio si el recorte lo deja vacio: la misma definicion
    # que la transformacion, y no una lista de codigos aparte. Un espacio que la
    # reparacion consume -el U+00A0 de `A` con tilde seguida de U+00A0, que es
    # una `a` con grave leida como latin-1- es parte de una letra y no se
    # recorta; detiene el recorte en ese borde.
    espacio <- !nzchar(.recortar_espacios_borde(
      intToUtf8(codigos, multiple = TRUE)
    ))
    quitar <- espacio & !consumidos
    conservar <- which(!quitar)
    if (!length(conservar)) return("")
    paste0(intToUtf8(codigos[min(conservar):max(conservar)], multiple = TRUE),
           collapse = "")
  }
  protegido <- .proteger_accion_texto(
    anterior, transformar, enmascarar, proteccion, revisar_nuevas = "nunca"
  )
  nuevo <- protegido$valor
  mascara <- protegido$cambiadas
  list(valor = nuevo, n = sum(mascara),
       n_no_reversibles = sum(.celdas_que_pierden_valor(
         anterior, nuevo, mascara
       )))
}

.quitar_controles_invisibles <- function(x, proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La eliminaci\u00f3n de controles invisibles requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  # Lo que no se puede decodificar se DEJA COMO ESTA. Antes se lo recorria
  # igual y `paste0(intToUtf8(NA), collapse = "")` devolvia la cadena `"NA"`,
  # que reemplazaba el valor del usuario: sobre una columna que mezcla `latin1`
  # con UTF-8, tres de siete valores se volvian `"NA"` al aplicar una accion
  # marcada como recomendada. No tocarlo tambien es una respuesta, y es la
  # unica honesta cuando no se puede leer el contenido.
  transformar <- function(v) vapply(v, function(texto) {
    if (is.na(texto)) return(NA_character_)
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    conservar <- !.codigos_control_eliminable(codigos)
    if (all(conservar)) return(texto)
    paste0(intToUtf8(codigos[conservar], multiple = TRUE), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
  enmascarar <- function(texto, consumidos) {
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    if (is.null(consumidos) || length(consumidos) != length(codigos)) {
      consumidos <- rep(FALSE, length(codigos))
    }
    conservar <- !(.codigos_control_eliminable(codigos) & !consumidos)
    if (all(conservar)) return(texto)
    paste0(intToUtf8(codigos[conservar], multiple = TRUE), collapse = "")
  }
  protegido <- .proteger_accion_texto(
    anterior, transformar, enmascarar, proteccion
  )
  nuevo <- protegido$valor
  cambio <- protegido$cambiadas
  list(valor = .resultado_texto(x, nuevo), n = sum(cambio),
       n_no_reversibles = sum(.celdas_que_pierden_valor(
         anterior, nuevo, cambio
       )))
}

.normalizar_espacios_invisibles <- function(x, proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La normalizaci\u00f3n de espacios Unicode requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  transformar <- function(v) vapply(v, function(texto) {
    if (is.na(texto)) return(NA_character_)
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    afectados <- codigos %in% .codigos_espacios_invisibles
    if (!any(afectados)) return(texto)
    codigos[afectados] <- 32L
    paste0(intToUtf8(codigos, multiple = TRUE), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
  enmascarar <- function(texto, consumidos) {
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    if (is.null(consumidos) || length(consumidos) != length(codigos)) {
      consumidos <- rep(FALSE, length(codigos))
    }
    afectados <- codigos %in% .codigos_espacios_invisibles & !consumidos
    if (!any(afectados)) return(texto)
    codigos[afectados] <- 32L
    paste0(intToUtf8(codigos, multiple = TRUE), collapse = "")
  }
  protegido <- .proteger_accion_texto(
    anterior, transformar, enmascarar, proteccion, revisar_nuevas = "si_hay_A"
  )
  nuevo <- protegido$valor
  cambio <- protegido$cambiadas
  list(valor = .resultado_texto(x, nuevo), n = sum(cambio),
       n_no_reversibles = sum(.celdas_que_pierden_valor(
         anterior, nuevo, cambio
       )))
}

# HTML5 lee las referencias numericas entre 0x80 y 0x9F como Windows-1252, no como
# los controles C1 que esos numeros son en Unicode: `&#146;` es el apostrofo
# tipografico de los textos que vienen de Word. Decodificarlas como controles
# dejaba un caracter invisible donde iba una letra o una comilla. Medido. Tabla
# del estandar, escrita aca para no depender de `iconv`; los
# cinco numeros que Windows-1252 no define quedan como estan.
.REFERENCIAS_HTML_WINDOWS_1252 <- c(
  `128` = 0x20AC, `130` = 0x201A, `131` = 0x0192, `132` = 0x201E,
  `133` = 0x2026, `134` = 0x2020, `135` = 0x2021, `136` = 0x02C6,
  `137` = 0x2030, `138` = 0x0160, `139` = 0x2039, `140` = 0x0152,
  `142` = 0x017D, `145` = 0x2018, `146` = 0x2019, `147` = 0x201C,
  `148` = 0x201D, `149` = 0x2022, `150` = 0x2013, `151` = 0x2014,
  `152` = 0x02DC, `153` = 0x2122, `154` = 0x0161, `155` = 0x203A,
  `156` = 0x0153, `158` = 0x017E, `159` = 0x0178
)

.entidad_html_reemplazo <- function(entidad) {
  if (!.entidad_html_valida(entidad)) return(entidad)
  cuerpo <- substring(entidad, 2L, nchar(entidad) - 1L)
  punto <- NULL
  if (startsWith(cuerpo, "#x") || startsWith(cuerpo, "#X")) {
    punto <- suppressWarnings(strtoi(substring(cuerpo, 3L), base = 16L))
  } else if (startsWith(cuerpo, "#")) {
    punto <- suppressWarnings(as.integer(substring(cuerpo, 2L)))
  }
  if (!is.null(punto)) {
    windows <- .REFERENCIAS_HTML_WINDOWS_1252[as.character(punto)]
    if (!is.na(windows)) punto <- unname(windows)
    return(intToUtf8(punto))
  }
  unname(.entidades_html_comunes[[cuerpo]])
}

.decodificar_entidades_html <- function(x, proteccion = NULL,
                                        proteger_conmutacion = TRUE) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La decodificaci\u00f3n HTML requiere una columna de texto.", call. = FALSE)
  }
  anterior <- as.character(x)
  # La misma que usa la deteccion: las dos tienen que coincidir.
  patron <- .PATRON_ENTIDAD_HTML
  transformar <- function(v) vapply(v, function(original) {
    if (is.na(original)) return(NA_character_)
    # Declarado antes de la expresion regular, como en `.aplicar_por_marca()`:
    # bajo `LC_CTYPE=C` un `unknown` UTF-8 valido se leia como bytes de la
    # sesion. Lo `bytes` sigue como esta. Sin entidades, vuelve como llego.
    texto <- .texto_para_operar(original)
    if (!validUTF8(texto)) return(original)
    coincidencias <- gregexpr(patron, texto, perl = TRUE)[[1L]]
    if (identical(coincidencias[[1L]], -1L)) return(original)
    encontrados <- regmatches(texto, list(coincidencias))[[1L]]
    reemplazos <- vapply(encontrados, .entidad_html_reemplazo, character(1L))
    regmatches(texto, list(coincidencias)) <- list(reemplazos)
    texto
  }, character(1L), USE.NAMES = FALSE)
  protegido <- .proteger_accion_texto(
    anterior, transformar, proteccion = proteccion,
    proteger_conmutacion = proteger_conmutacion
  )
  list(valor = .resultado_texto(x, protegido$valor),
       n = sum(protegido$cambiadas))
}

.reemplazar_separadores <- function(x, proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("El reemplazo de saltos de l\u00ednea requiere una columna de texto.",
         call. = FALSE)
  }
  anterior <- as.character(x)
  # Los MISMOS cinco codigos que cuenta el detector (`.codigos_salto_linea_set`,
  # 9 a 13), escritos por su numero. En PCRE `\v` no es la tabulacion
  # vertical sino la clase "espacio vertical": casa tambien U+0085, U+2028 y
  # U+2029, que son del dominio de `eliminar_controles_invisibles` y
  # `normalizar_espacios_invisibles`, y el plan que estimaba 1 celda cambiaba 4.
  # En la rama por bytes casaba el BYTE 0x85, que es de continuacion en UTF-8:
  # `c3 85` (A con anillo) y `c4 85` (a con ogonek) quedaban `c3 20` y `c4 20`,
  # dos celdas UTF-8 invalido con el registro en `ejecutada`. Medido.
  # `\x09` a `\x0D` son ASCII: no aparecen dentro de ningun
  # caracter multibyte, asi que la rama por bytes no puede partir una letra.
  patron <- "\\x0D\\x0A|[\\x09-\\x0D]"
  transformar <- function(v) .aplicar_por_marca(
    v,
    function(v) gsub(patron, " ", v, perl = TRUE),
    function(v) gsub(patron, " ", v, perl = TRUE, useBytes = TRUE)
  )
  enmascarar <- function(texto, consumidos) {
    codigos <- .codigos_decodificables(texto)
    if (is.null(codigos)) return(texto)
    if (is.null(consumidos) || length(consumidos) != length(codigos)) {
      consumidos <- rep(FALSE, length(codigos))
    }
    salida <- integer()
    i <- 1L
    while (i <= length(codigos)) {
      if (i < length(codigos) && codigos[[i]] == 13L &&
          codigos[[i + 1L]] == 10L) {
        if (any(consumidos[i:(i + 1L)])) {
          salida <- c(salida, codigos[i:(i + 1L)])
        } else {
          salida <- c(salida, 32L)
        }
        i <- i + 2L
      } else if (codigos[[i]] %in% 9L:13L && !consumidos[[i]]) {
        salida <- c(salida, 32L)
        i <- i + 1L
      } else {
        salida <- c(salida, codigos[[i]])
        i <- i + 1L
      }
    }
    paste0(intToUtf8(salida, multiple = TRUE), collapse = "")
  }
  protegido <- .proteger_accion_texto(
    anterior, transformar, enmascarar, proteccion, revisar_nuevas = "si_hay_A"
  )
  list(valor = .resultado_texto(x, protegido$valor),
       n = sum(protegido$cambiadas))
}

.reparar_codificacion <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La reparaci\u00f3n de codificaci\u00f3n requiere una columna de texto.",
         call. = FALSE)
  }
  iteraciones <- parametros$max_iteraciones
  if (is.null(iteraciones)) iteraciones <- 20L
  anterior <- as.character(x)
  # La reparacion tiene que ver LO MISMO que vio el perfil, o el paquete
  # recomienda una accion que despues no puede ejecutar.
  #
  # `perfilar()` analiza sobre la forma saneada -`.texto_analizable()` marca
  # UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito- y
  # sobre esa forma publica `n_codificacion_reparable` y el estado. La
  # aplicacion trabajaba sobre la columna CRUDA, todavia declarada `bytes`, y
  # ahi `.ftfy_decode_inconsistent_utf8()` muere en `nchar()` -que sobre esa
  # marca no puede contar caracteres-.
  #
  # Medido: con mojibake declarado `bytes`, el perfil publicaba
  # `reparable = 2`, estado `reparado`, y el plan una accion `recomendada` y
  # activa con `n_afectadas = 2`; `aplicar()` la daba por `fallida` con
  # `n_cambiadas = 0` y el dato quedaba intacto.
  #
  # Se marca, no se convierte: los bytes no cambian. Lo que no es UTF-8 valido
  # queda como esta -no hay nada que interpretar ahi- y sigue contandose en
  # `n_codificacion_invalida`.
  anterior <- .texto_para_transformar(anterior)
  aplicables <- if (isTRUE(parametros$aplicar_parciales)) {
    c("reparado", "reparado_parcialmente")
  } else "reparado"
  unicos <- unique(anterior[!is.na(anterior)])
  analisis <- .analizar_codificacion(
    unicos, max_iteraciones = iteraciones
  )
  indice <- match(anterior, unicos)
  candidatos <- rep(NA_character_, length(anterior))
  estados <- rep(NA_character_, length(anterior))
  if (length(unicos)) {
    presentes <- !is.na(indice)
    candidatos[presentes] <- analisis$reparados[indice[presentes]]
    estados[presentes] <- analisis$estados[indice[presentes]]
  }
  mascara <- !is.na(candidatos) & !is.na(anterior) & candidatos != anterior &
    estados %in% aplicables
  nuevo <- anterior
  nuevo[mascara] <- candidatos[mascara]
  parciales <- !is.na(candidatos) & !is.na(anterior) & candidatos != anterior &
    estados == "reparado_parcialmente"
  # El estado que se publica es el de lo APLICADO, no el de la columna: con las
  # parciales afuera, decir `reparado_parcialmente` describia una celda que no se
  # toco. Lo mismo una celda `no_se_pudo` -el caracter de reemplazo-: la accion
  # no la toca, y con ella el registro publicaba `reparado_parcialmente` sobre
  # tres celdas reparadas enteras, contra el `reparado` del plan.
  estados_aplicados <- estados
  estados_aplicados[!(estados %in% aplicables)] <- NA_character_
  estado <- .ftfy_estado_agregado(estados_aplicados)
  list(valor = .resultado_texto(x, nuevo), n = sum(mascara),
       n_parciales = sum(parciales), estado_reparacion = estado,
       estados = estados)
}

# El texto que `.convertir_numero_regional()` le pasa a `as.numeric()`, con
# moneda y unidad sacadas y los separadores ya resueltos segun la convencion.
#
# Vive aparte porque la guarda de precision de `.evaluar_conversion()` necesita
# exactamente este texto. Antes lo rearmaba por su cuenta a partir del original,
# y cada formato que el conversor aceptaba era un agujero posible en la guarda:
# aparecieron cinco -entero plano, con `%`, notacion
# cientifica, mantisa con coma, y entero es-UY con `,0` final-. Con una sola
# normalizacion para los dos, lo que el conversor sabe leer la guarda lo ve.
# El texto que `.convertir_tipo()` lee como numero. Aparte por el mismo motivo
# que `.texto_regional_normalizado()`. Ya no reemplaza la coma por el punto: el
# texto se lee con `.leer_numeros_texto()` y la convencion decimal de la
# columna, como lo lee el resumen del perfil. Con el reemplazo propio, `12.500`
# en una columna de coma decimal -doce mil quinientos para el resumen- se
# convertia en 12,5.
.texto_tipo_normalizado <- function(x) {
  .texto_legible_recortado(x)
}

# Lo que una conversion a numero tiene que leer: lo que no es `NA` ni queda
# vacio recortado por bytes -`trimws()` sobre el texto crudo abortaba bajo un
# locale UTF-8 con un byte suelto, y avisaba bajo `C` con marcas mezcladas-. Se
# mira la celda CRUDA, no su texto legible: el texto legible pone `NA` en lo que
# no es UTF-8 valido, y eso no es una ausencia sino un valor presente que no se
# lee. `convertir_tipo` contaba presentes sobre el texto legible y daba por
# ausente una celda `14` seguida de un byte suelto: la accion salia `lista`, no
# destructiva, y `aplicar()` escribia `NA` en su lugar. La regional ya la
# contaba presente y quedaba bloqueada.
.presentes_para_convertir <- function(x) {
  !is.na(x) & nzchar(.recortar_bytes(as.character(x)))
}

.texto_regional_normalizado <- function(x, parametros) {
  partes <- .componentes_numero_texto(x)
  presentes <- .presentes_para_convertir(x)
  if (any(presentes & !partes$compatible)) {
    .detener("Hay valores presentes que no responden al formato num\u00e9rico regional.",
         call. = FALSE)
  }
  convencion <- parametros$convencion
  if (is.null(convencion) || !length(convencion)) convencion <- "ambigua"
  if (identical(convencion, "es-UY")) convencion <- "decimal_coma"
  texto <- partes$cuerpo
  # La normalizacion es la misma con la que el perfil resume la columna: ver
  # `.normalizar_separadores_numero()`.
  if (length(convencion) == 1L &&
      convencion %in% c("decimal_coma", "decimal_punto")) {
    texto <- .normalizar_separadores_numero(texto, convencion)
  } else if (identical(convencion, "ambigua")) {
    ambiguos_punto <- presentes & partes$punto_tres
    ambiguos_coma <- presentes & partes$coma_tres
    interpretacion_punto <- parametros$punto_sin_coma
    interpretacion_coma <- parametros$coma_sin_punto
    if (any(ambiguos_punto) &&
        (length(interpretacion_punto) != 1L || is.na(interpretacion_punto) ||
         !interpretacion_punto %in% c("miles", "decimal"))) {
      .detener(
        "La columna es ambigua; configure `punto_sin_coma` como 'miles' o 'decimal'.",
        call. = FALSE
      )
    }
    if (any(ambiguos_coma) &&
        (length(interpretacion_coma) != 1L || is.na(interpretacion_coma) ||
         !interpretacion_coma %in% c("miles", "decimal"))) {
      .detener(
        "La columna es ambigua; configure `coma_sin_punto` como 'miles' o 'decimal'.",
        call. = FALSE
      )
    }
    if (any(ambiguos_punto) && identical(interpretacion_punto, "miles")) {
      texto[ambiguos_punto] <- gsub(".", "", texto[ambiguos_punto], fixed = TRUE)
    }
    if (any(ambiguos_coma)) {
      if (identical(interpretacion_coma, "miles")) {
        texto[ambiguos_coma] <- gsub(",", "", texto[ambiguos_coma], fixed = TRUE)
      } else {
        texto[ambiguos_coma] <- sub(",", ".", texto[ambiguos_coma], fixed = TRUE)
      }
    }
  } else if (!identical(convencion, "sin_separadores")) {
    .detener("La convenci\u00f3n num\u00e9rica no est\u00e1 confirmada.", call. = FALSE)
  }
  # Lo que el lector del resumen no lee como numero tampoco lo convierte el
  # plan (ver `.leer_numeros_texto()`). `099.123.456` junto a importes con coma
  # decimal es un telefono: el perfil lo deja fuera del resumen y dice que
  # convertir lo dejaria en `NA`, y aca se le quitaban los puntos y `aplicar()`
  # escribia 99123456. Se trata como cualquier valor que no responde al
  # formato, igual que `0x1A`: la conversion no se ejecuta. Con una convencion
  # decimal la pregunta se le hace al lector; con la ambigua, que el lector no
  # decide, se le aplican sus dos reglas: la forma de numero, y los miles
  # agrupados de a tres sin cero adelante cuando el separador se lee como miles.
  candidatos <- which(presentes & partes$compatible)
  if (length(candidatos)) {
    cuerpo <- trimws(as.character(partes$cuerpo[candidatos]))
    sin_lectura <- if (length(convencion) == 1L &&
                       convencion %in% c("decimal_coma", "decimal_punto")) {
      leidos <- .leer_numeros_texto(cuerpo, convencion)
      is.na(leidos) & !is.nan(leidos)
    } else {
      fuera <- !.formas_numero_texto(cuerpo)
      if (identical(convencion, "ambigua")) {
        miles_punto <- partes$punto_tres[candidatos] %in% TRUE &
          identical(parametros$punto_sin_coma, "miles")
        miles_coma <- partes$coma_tres[candidatos] %in% TRUE &
          identical(parametros$coma_sin_punto, "miles")
        fuera <- fuera |
          (miles_punto & !grepl(.patron_miles_coma, cuerpo, perl = TRUE)) |
          (miles_coma & !grepl(.patron_miles_punto, cuerpo, perl = TRUE))
      }
      fuera
    }
    if (any(sin_lectura)) {
      .detener("Hay valores presentes que no responden al formato num\u00e9rico regional.",
           call. = FALSE)
    }
  }
  list(texto = texto, presentes = presentes, partes = partes)
}

.convertir_numero_regional <- function(x, parametros) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La conversi\u00f3n regional requiere una columna de texto.", call. = FALSE)
  }
  normalizado <- .texto_regional_normalizado(x, parametros)
  texto <- normalizado$texto
  presentes <- normalizado$presentes
  partes <- normalizado$partes
  numero <- suppressWarnings(as.numeric(texto))
  if (any(presentes & (!is.finite(numero) | is.na(numero)))) {
    .detener("No fue posible convertir todos los valores regionales.", call. = FALSE)
  }
  porcentajes <- presentes & partes$unidad == "%"
  numero[porcentajes] <- numero[porcentajes] / 100
  list(valor = numero, n = sum(presentes))
}

.resultado_texto <- function(original, nuevo) {
  if (!is.factor(original)) return(nuevo)
  # Las acciones pueden introducir valores fuera de los niveles originales;
  # el contrato devuelve texto y no un factor incompleto.
  as.character(nuevo)
}

# Deja el texto en la forma que el PERFIL analizo, para que lo que el plan
# propone se pueda ejecutar sobre lo mismo que se midio.
#
# `perfilar()` analiza sobre la forma saneada -`.texto_analizable()` marca
# UTF-8 lo declarado `bytes` cuando sus bytes son validos, a proposito, para que
# `tolower()` y las expresiones regulares puedan trabajar- y sobre esa forma
# publica sus cifras y arma el plan. Las acciones trabajaban sobre la columna
# CRUDA, todavia declarada `bytes`, y ahi las mismas primitivas abortan: el
# paquete recomendaba acciones que no podia ejecutar.
#
# Medido sobre una columna con mayusculas inconsistentes y un valor declarado:
# `convertir_minusculas`, `convertir_titulo` y `convertir_mayusculas` quedaban
# en `fallida` con `n_cambiadas = 0`, y `convertir_segun_diccionario` abortaba.
#
# Se MARCA, no se convierte: los bytes no cambian. Lo que no es UTF-8 valido
# queda como esta -no hay nada que interpretar ahi- y sigue contandose en
# `n_codificacion_invalida`.
#
# Lo `unknown` tambien: el perfil lo marca igual, y aca solo se marcaba lo
# `bytes`. Bajo `LC_CTYPE=C` la reparacion de codificacion leia un `unknown`
# valido como ASCII escapado y lo daba por sano: la accion que el plan publicaba
# `lista` quedaba `fallida` segun el locale. La declaracion es la misma de la
# normalizacion, `.declarar_utf8_valido()`.
.texto_para_transformar <- function(x) {
  .texto_para_operar(as.character(x))
}

.transformar_capitalizacion <- function(x, estrategia, parametros,
                                       proteccion = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    .detener("La capitalizaci\u00f3n requiere una columna de texto.", call. = FALSE)
  }
  anterior <- .texto_para_transformar(x)
  # La caja se cambia con los mapas del paquete y no con `tolower()`/`toupper()`,
  # que dependen del locale, ni con `\\b`, que sin `(*UCP)` no ve una letra
  # acentuada como letra (ver `.a_titulo_vector()`).
  transformar <- function(v) {
    if (identical(estrategia, "convertir_minusculas")) {
      .aplicar_por_marca(v, .normalizacion_minusculas_vector)
    } else if (identical(estrategia, "convertir_mayusculas")) {
      .aplicar_por_marca(v, .a_mayusculas_vector)
    } else if (identical(estrategia, "convertir_titulo")) {
      .aplicar_por_marca(v, .a_titulo_vector)
    } else {
      diccionario <- parametros$diccionario
      if (is.null(diccionario) || !is.atomic(diccionario) ||
          is.null(names(diccionario)) || any(!nzchar(names(diccionario)))) {
        .detener(
          "La capitalizaci\u00f3n por diccionario requiere un vector at\u00f3mico con nombres.",
          call. = FALSE
        )
      }
      salida <- v
      indices <- match(v, names(diccionario))
      reemplazar <- !is.na(indices) & !is.na(v)
      salida[reemplazar] <- as.character(diccionario[indices[reemplazar]])
      salida
    }
  }
  protegido <- .proteger_accion_texto(
    anterior, transformar, proteccion = proteccion
  )
  list(valor = .resultado_texto(x, protegido$valor),
       n = sum(protegido$cambiadas))
}

.convertir_logico <- function(x) {
  # El mismo pliegue con que `inferir_tipo()` reconocio el logico: con
  # `tolower()`, un plan armado en una sesion UTF-8 sobre `SI` con tilde quedaba
  # `fallida` en un `Rscript` bajo `C`.
  texto <- .minusculas_para_comparar(x)
  verdaderos <- c("true", "t", "si", "s\u00ed", "s", "1")
  falsos <- c("false", "f", "no", "n", "0")
  presentes <- !is.na(x)
  validos <- texto %in% c(verdaderos, falsos)
  if (any(presentes & !validos)) {
    .detener("Hay valores presentes que no pueden convertirse a l\u00f3gico.", call. = FALSE)
  }
  salida <- rep(NA, length(x))
  salida[presentes] <- texto[presentes] %in% verdaderos
  salida
}

.convertir_fecha <- function(x, parametros) {
  formatos <- parametros$formatos
  if (!length(formatos)) {
    .detener("La conversi\u00f3n de fecha requiere formatos confirmados.", call. = FALSE)
  }
  # Declarado antes del recorte: lo mismo que la conversion a
  # numero, `.texto_legible_recortado()`.
  valores <- .texto_legible_recortado(x)
  presentes <- !is.na(x)
  especificaciones <- .especificaciones_fecha()
  indices <- match(formatos, especificaciones$formato)
  if (anyNA(indices)) {
    .detener("La conversi\u00f3n de fecha recibi\u00f3 un formato no reconocido.", call. = FALSE)
  }
  compatibles <- rep(FALSE, length(x))
  for (indice in indices) {
    compatibles <- compatibles | .es_fecha_valida(
      valores, especificaciones$formato[[indice]],
      especificaciones$expresion[[indice]]
    )
  }
  if (any(presentes & !compatibles)) {
    .detener("Hay valores presentes que no responden a los formatos confirmados.", call. = FALSE)
  }
  tabla_formatos <- data.frame(
    formato = formatos, estado = rep("confirmado", length(formatos)),
    stringsAsFactors = FALSE
  )
  salida <- .parsear_fechas(x, tabla_formatos)
  if (any(presentes & is.na(salida))) {
    .detener("Hay valores presentes que no responden a los formatos confirmados.", call. = FALSE)
  }
  if (identical(parametros$tipo, "fecha")) as.Date(salida) else salida
}

.convertir_tipo <- function(x, parametros) {
  tipo <- parametros$tipo
  presentes <- !is.na(x)
  if (tipo %in% c("fecha", "fecha-hora")) {
    return(.convertir_fecha(x, parametros))
  }
  if (identical(tipo, "logico")) {
    return(.convertir_logico(x))
  }
  texto <- .texto_tipo_normalizado(x)
  if (is.character(x) || is.factor(x)) {
    # Una celda vacia o de solo espacios no es un valor presente: el perfil la
    # cuenta como ausencia y la conversion la deja en `NA`, como hace
    # `convertir_numero_regional`. Contada como presente bloqueaba la
    # conversion de una columna que el hallazgo declaraba sin perdida. Una
    # celda que no es UTF-8 valido si lo es, y no se lee: bloquea.
    presentes <- .presentes_para_convertir(x)
    # El texto se lee con el lector comun y la convencion decimal de la
    # columna, como lo leen la inferencia y el resumen.
    numero <- suppressWarnings(.leer_numeros_texto(texto))
  } else {
    numero <- suppressWarnings(as.numeric(texto))
  }
  if (any(presentes & (!is.finite(numero) | is.na(numero)))) {
    .detener("Hay valores presentes que no pueden convertirse a n\u00famero.", call. = FALSE)
  }
  if (identical(tipo, "doble")) {
    return(numero)
  }
  if (identical(tipo, "entero")) {
    # El rango de `integer` en R es simetrico: -2147483648 es el patron de bits
    # de `NA_integer_`. El limite inferior estaba en `-.Machine$integer.max - 1`
    # y ese valor pasaba la guarda, se volvia `NA` con un aviso de R, y el plan
    # -que comprueba con este mismo conversor- lo publicaba recomendado, activo
    # e inyectivo, con `n_no_reversibles = 0` sobre un valor perdido. Medido.
    # Ademas de corregir el limite se mira el resultado: un
    # presente que sale `NA` es un valor que el conversor no pudo representar,
    # cualquiera sea la razon.
    limites <- c(-.Machine$integer.max, .Machine$integer.max)
    validos <- !presentes |
      (abs(numero - round(numero)) < sqrt(.Machine$double.eps) &
         numero >= limites[[1L]] & numero <= limites[[2L]])
    if (!all(validos)) {
      .detener("Hay valores presentes que no pueden representarse como enteros.", call. = FALSE)
    }
    entero <- suppressWarnings(as.integer(numero))
    if (any(presentes & is.na(entero))) {
      .detener("Hay valores presentes que no pueden representarse como enteros.", call. = FALSE)
    }
    return(entero)
  }
  .detener("No hay una conversi\u00f3n definida para el tipo '", tipo, "'.", call. = FALSE)
}

# `dentro` es el universo de la accion -la regla de `aplicabilidad` del perfil-:
# los limites se calculan solo con sus valores, y solo ahi se marca o se recorta.
.limites_outliers <- function(x, dentro = NULL) {
  if (inherits(x, c("Date", "POSIXt")) || is.numeric(x)) {
    valores <- as.numeric(x)
  } else {
    # El texto se lee con su convencion decimal, como lo lee el perfil que
    # propuso la accion: con `sub(",", ".")` los limites salian de `1.500` leido
    # como 1,5 en una columna de coma decimal. Ver `.leer_numeros_texto()`.
    valores <- .leer_numeros_texto(x)
  }
  validos <- is.finite(valores)
  if (!is.null(dentro)) validos <- validos & dentro
  if (!any(validos)) {
    return(list(valores = valores, validos = validos, inferior = NA_real_,
                superior = NA_real_))
  }
  iqr <- stats::IQR(valores[validos], type = 7)
  cuartiles <- stats::quantile(
    valores[validos], c(0.25, 0.75), names = FALSE, type = 7
  )
  list(
    valores = valores, validos = validos,
    inferior = cuartiles[[1L]] - 1.5 * iqr,
    superior = cuartiles[[2L]] + 1.5 * iqr
  )
}

.marca_outliers <- function(x, dentro = NULL) {
  limites <- .limites_outliers(x, dentro)
  mascara <- rep(FALSE, length(x))
  if (!any(limites$validos)) return(mascara)
  mascara[limites$validos] <-
    limites$valores[limites$validos] < limites$inferior |
    limites$valores[limites$validos] > limites$superior
  mascara
}

.winsorizar_outliers <- function(x, dentro = NULL) {
  limites <- .limites_outliers(x, dentro)
  salida <- limites$valores
  salida[limites$validos] <- pmin(
    pmax(salida[limites$validos], limites$inferior), limites$superior
  )
  # Lo que no es finito no entra en los limites ni en la cuenta de outliers del
  # perfil, asi que tampoco se toca: `Inf` y `NaN` pasaban a `NA` sin contarse.
  salida[!limites$validos] <- limites$valores[!limites$validos]
  # Un solo recorte, a los limites de Tukey de los datos recibidos: es lo que
  # dice la justificacion del plan y la ayuda -"reescribe solo las celdas
  # recortadas"-. Habia una iteracion que, si tras recortar quedaban extremos,
  # los reemplazaba por la mediana de los centrales y, si aun quedaban, llevaba
  # la columna ENTERA a un solo valor. Cuando los extremos mueven los cuartiles
  # -seis de veinticuatro sobre `41 ... 59, 100, 100, 150, 1000, 1000, 5000`-
  # las 24 celdas quedaban en 52,5 y el re-perfil acusaba `constante`, con el
  # plan diciendo 6 afectadas. Medido. Que el perfil del
  # resultado pueda volver a ver extremos con los cuartiles nuevos es lo que hace
  # Tukey, y no se corrige reescribiendo valores que no eran extremos.
  cambio <- limites$validos & limites$valores != salida
  cambio[is.na(cambio)] <- FALSE
  list(valor = .columna_winsorizada(x, salida, cambio), n = sum(cambio))
}

# La columna winsorizada, en la clase de la original y con solo las celdas
# recortadas cambiadas. Se devolvia siempre el vector numerico de los limites:
# una columna de TEXTO con numeros pasaba entera a `double` -perdia los ceros
# iniciales que el propio plan protege al bloquear `convertir_tipo`, y el
# registro contaba una celda de veintiuna-, un factor tambien, y una fecha forzada
# volvia como numero. Medido. Una columna entera si queda en
# doble precision: los limites son cuartiles, y el plan lo dice.
.columna_winsorizada <- function(x, salida, cambio) {
  if (inherits(x, "Date")) {
    resultado <- x
    resultado[cambio] <- structure(salida[cambio], class = "Date")
    return(resultado)
  }
  if (inherits(x, "POSIXct")) {
    resultado <- x
    resultado[cambio] <- .POSIXct(salida[cambio], tz = attr(x, "tzone"))
    return(resultado)
  }
  if (is.character(x) || is.factor(x)) {
    texto <- as.character(x)
    nuevos <- .formatear_numeros_uno_a_uno(salida[cambio])
    con_coma <- grepl(",", texto[cambio], fixed = TRUE, useBytes = TRUE)
    nuevos[con_coma] <- sub(".", ",", nuevos[con_coma], fixed = TRUE)
    texto[cambio] <- nuevos
    if (is.factor(x)) {
      return(factor(texto, levels = unique(c(levels(x), nuevos))))
    }
    return(texto)
  }
  salida
}

# Un codigo entero por fila que iguala dos filas solo si `duplicated()` de base
# las iguala, que es lo que cuenta el perfil y lo que usa
# `conservar_primera_duplicada`. Antes se pasaba cada columna por `factor()`, que
# arma sus niveles con `as.character()`: un doble se escribe con quince cifras y
# `0.1 + 0.2` quedaba igual a `0.3`, y `.nombres_para_operar()` igualaba un texto
# `latin1` con el mismo en UTF-8. Las dos acciones del grupo de duplicados
# contestaban distinto a "es esta fila un duplicado", y `conservar_mas_completa`
# borraba una fila con clave distinta. Medido. `match()` sobre
# cada columna es exacto en los dobles, distingue `NaN` de `NA` y no ordena, asi
# que tampoco aborta con bytes invalidos. `integer64` va por su texto exacto: su
# `NA` tiene el patron de bits de `-0` como doble, y `match()` lo igualaria al 0.
#
# Un factor va por sus CODIGOS, que es lo que compara `duplicated.data.frame()`
# del perfil. Iba por `as.character()`, que lleva a `NA` tanto el codigo ausente
# como el nivel `NA`: sobre codigos `1, 2, NA, NA` con niveles `c("a", NA)` el
# perfil contaba 1 duplicada y 2 filas en grupos, `marcar_filas_duplicadas`
# metia la fila 2 en el grupo de las filas 3 y 4 -3 filas, 2 repetidas- y
# `conservar_primera_duplicada` quitaba 1. Medido. Un
# `POSIXlt` -lo que deja `strptime()`- es una lista por dentro: va por su
# instante, como lo iguala el perfil.
#
# El nivel `NA` es un faltante en el perfil -ver
# `.factor_sin_nivel_na()`-, y el perfil cuenta los duplicados sobre su copia
# sin ese nivel: el nivel `NA` y el codigo ausente son el mismo valor, ausente.
# Los codigos se toman de la misma copia, para que el ejecutor siga agrupando
# como el perfil.
.codigos_filas_exactos <- function(tabla) {
  codigos <- lapply(tabla, function(x) {
    valores <- if (inherits(x, "integer64")) {
      as.character(x)
    } else if (inherits(x, "POSIXlt")) {
      unclass(as.POSIXct(x))
    } else if (is.factor(x)) {
      unclass(.leer_latin1_sin_windows_1252(.factor_sin_nivel_na(x)))
    } else if (is.character(x)) {
      .leer_latin1_sin_windows_1252(x)
    } else {
      unclass(x)
    }
    attributes(valores) <- NULL
    match(valores, unique(valores))
  })
  if (!length(codigos)) return(integer())
  # Sin los nombres de las columnas: `do.call()` los vuelve nombres de argumento
  # y bajo `C` una columna con acento avisaba "unable to translate" en
  # `aplicar()`. Es la construccion de `medir()`.
  .codigos_filas_combinados(codigos)
}

# La tabla que comparan las acciones de duplicados, con la MISMA forma que la
# que cuenta el perfil (`.filas_duplicadas_base()`): las columnas compuestas
# -matriz, arreglo, `data.frame` anidado- aplanadas por `.expandir_datos_identidad()`.
# El marcador no aplanaba: el perfil contaba 1 duplicada en una tabla con una
# columna matriz, una `data.frame` anidada o un `POSIXlt`, el plan recomendaba y
# ACTIVABA `marcar_filas_duplicadas`, y `aplicar()` la registraba `fallida`
# ("replacement has 6 rows, data has 3" / "No se pueden agrupar duplicados con
# columnas de lista") mientras `conservar_primera_duplicada` -que iba por otro
# camino- si quitaba la fila. Medido.
.tabla_identidad_filas <- function(datos) {
  .expandir_datos_identidad(.tabla_base(datos))
}

# Si alguna columna de esa tabla es una LISTA que los codigos no pueden comparar
# exactamente. `POSIXlt` es lista por dentro y se compara por su instante. Es la
# pregunta que hace el ejecutor y la misma que hace el plan para bloquear las
# acciones de duplicados: una sola regla.
.identidad_filas_con_listas <- function(tabla) {
  any(vapply(tabla, function(x) is.list(x) && !inherits(x, "POSIXlt"),
             logical(1L)))
}

.grupos_filas_duplicadas <- function(datos) {
  datos_base <- .tabla_identidad_filas(datos)
  if (!length(datos_base)) {
    repetidas <- seq_len(nrow(datos_base)) > 1L
    grupos <- if (nrow(datos_base) > 1L) {
      rep.int(1L, nrow(datos_base))
    } else {
      rep.int(NA_integer_, nrow(datos_base))
    }
    return(list(repetidas = repetidas, grupos = grupos))
  }
  if (.identidad_filas_con_listas(datos_base)) {
    .detener("No se pueden agrupar duplicados con columnas de lista.", call. = FALSE)
  }
  # Todo sale de los MISMOS codigos, y no de `duplicated.data.frame`.
  #
  # Sobre una columna `integer64`, `duplicated(x, fromLast = TRUE)` devuelve lo
  # mismo que sin `fromLast`: el metodo de `bit64` ignora el argumento. Con eso,
  # `participantes` se quedaba con la ultima fila de cada grupo y no con las
  # otras. Medido sobre `as.integer64(c(1, 2, -999, 4, -999))`: el plan declaraba
  # 2 filas, la accion marcaba 1, `.grupo_duplicado` dejaba la fila 3 sin grupo
  # -participa, con contenido identico- y el hallazgo sobrevivia al re-perfilar.
  # La misma tabla en `double` marcaba las dos, que es como se supo que el
  # defecto era del camino `integer64` y no de la regla.
  #
  # `factor()` lleva cualquier tipo a niveles por su representacion, y sobre los
  # codigos enteros que salen de ahi `duplicated()` es el de base y si honra
  # `fromLast`. Ademas es una sola definicion de "misma fila" para las tres
  # cosas que antes usaban dos.
  codigos <- .codigos_filas_exactos(datos_base)
  repetidas <- duplicated(codigos)
  participantes <- repetidas | duplicated(codigos, fromLast = TRUE)
  grupos <- rep(NA_integer_, nrow(datos_base))
  if (!any(participantes)) {
    return(list(repetidas = repetidas, grupos = grupos))
  }
  grupos[participantes] <- match(
    codigos[participantes], unique(codigos[participantes])
  )
  list(repetidas = repetidas, grupos = grupos)
}

# Quitar filas no cambia las columnas. `[` sobre un `data.frame` le saca a cada
# vector los atributos que no son de estructura, y con ellos la etiqueta de
# variable que deja `haven`: `eliminar_filas_ausentes` y
# `conservar_primera_duplicada` la borraban en las tres columnas de la tabla,
# tambien en las que ninguna accion tocaba -el defecto que `.con_atributos_de()`
# ya habia corregido para las acciones de texto-. Y sobre un `data.table` con
# clave, rearmar la tabla con `as.data.table()` perdia la clave: quitar filas de
# una tabla ordenada la deja ordenada, y `aplicar()` solo saca la clave si la
# accion toca su columna. Medido.
.filtrar_filas <- function(datos, conservar) {
  if (inherits(datos, "data.table")) {
    clave <- data.table::key(datos)
    base <- as.data.frame(data.table::copy(datos), stringsAsFactors = FALSE)
    salida <- data.table::as.data.table(
      .atributos_de_columnas(base, base[which(conservar), , drop = FALSE])
    )
    if (length(clave) && all(clave %in% names(salida))) {
      data.table::setkeyv(salida, clave)
    }
    salida
  } else {
    .atributos_de_columnas(datos, datos[conservar, , drop = FALSE])
  }
}

# Devuelve a cada columna de `filtrada` los atributos de la misma columna de
# `original` que el subconjunto de filas perdio. Los de estructura no se tocan:
# los pone `[` y dependen del largo.
.atributos_de_columnas <- function(original, filtrada) {
  if (!identical(length(original), length(filtrada))) return(filtrada)
  estructura <- c("names", "dim", "dimnames", "row.names")
  for (j in seq_along(filtrada)) {
    atributos <- attributes(original[[j]])
    if (is.null(atributos)) next
    columna <- filtrada[[j]]
    faltan <- setdiff(names(atributos), c(estructura, names(attributes(columna))))
    if (!length(faltan)) next
    for (nombre in faltan) attr(columna, nombre) <- atributos[[nombre]]
    filtrada[[j]] <- columna
  }
  filtrada
}

.conservar_mas_completa <- function(datos, clave) {
  datos_base <- .tabla_base(datos)
  if (!length(clave) || anyNA(.indice_nombre(clave, names(datos_base)))) {
    .detener(
      "`conservar_mas_completa` requiere configurar nombres de clave existentes.",
      call. = FALSE
    )
  }
  claves <- .seleccionar_columnas(datos_base, clave)
  if (any(vapply(claves, is.list, logical(1L)))) {
    .detener("La clave no puede contener columnas de lista.", call. = FALSE)
  }
  codigos <- .codigos_filas_exactos(claves)
  # Una clave AUSENTE no identifica a nadie: las filas sin documento formaban un
  # solo grupo y sobrevivia una -personas distintas, eliminadas por no tener
  # clave-. Medido. Una fila con la clave incompleta no se
  # agrupa con ninguna.
  incompletas <- !.filas_completas(as.data.frame(claves))
  grupos <- split(which(!incompletas), codigos[!incompletas])
  # La completitud de la fila con la misma lectura de la ausencia que la clave
  # de arriba: el nivel `NA` de un factor es un faltante. Ver
  # `.normalizar_columnas_entrada()`.
  completitud <- rowSums(!is.na(.normalizar_columnas_entrada(datos_base)))
  conservar <- rep(TRUE, nrow(datos_base))
  for (indices in grupos) {
    if (length(indices) > 1L) {
      elegido <- indices[[which.max(completitud[indices])]]
      conservar[setdiff(indices, elegido)] <- FALSE
    }
  }
  conservar
}

.contenido_igual <- function(x, y) {
  .columnas_identicas(x, y)
}

# Si una accion de nombres no puede aplicarse a estos datos: renombra por el mapa
# `nombres_esperados` -> `nombres_propuestos`, y los nombres de los datos tienen
# que ser los esperados. `NULL` si puede; si no, el motivo.
.motivo_nombres_distintos <- function(accion, nombres) {
  if (!accion$estrategia[[1L]] %in%
      c("normalizar_nombres", "normalizar_nombres_snake_case")) {
    return(NULL)
  }
  esperados <- accion$parametros[[1L]]$nombres_esperados
  if (is.null(esperados) ||
      identical(.nombres_para_operar(nombres), .nombres_para_operar(esperados))) {
    return(NULL)
  }
  "Los nombres de los datos no coinciden con los usados por el perfil."
}

.agregar_marca <- function(datos, nombre, valor) {
  if (.nombres_para_operar(nombre) %in% .nombres_para_operar(names(datos))) {
    .detener("La columna de marca ya existe: ", nombre, ".", call. = FALSE)
  }
  datos[[nombre]] <- valor
  datos
}

# Cuantos valores CAMBIARON de verdad, que es lo que `n_cambiadas` promete.
#
# Las dos conversiones devolvian `sum(!is.na(x))` -los presentes-, y catorce
# acciones de este mismo archivo devuelven `sum(cambio)`. La misma idea escrita
# de dos maneras. Se ve cuando la conversion es una IDENTIDAD: planificar sobre
# una columna de texto y aplicar sobre la misma tabla ya convertida a entero
# dejaba la columna bit a bit igual, con `estado = ejecutada` y
# `n_cambiadas = 4`. La documentacion promete lo contrario: "si una accion no
# produce ningun efecto cuando el plan estimaba alguno, se registra como
# fallida". Con el conteo correcto eso sale solo, porque
# `.motivo_efecto_accion()` ya mira el caso `actual == 0`.
#
# Si cambia la clase, todo valor presente se convirtio de verdad. Si no cambia,
# se comparan las representaciones: un presente que quedo `NA` tambien cambio
# -se perdio-, y cuenta.
.n_valores_cambiados <- function(antes, despues) {
  presentes <- !is.na(antes)
  if (!any(presentes)) return(0L)
  if (!identical(class(antes), class(despues))) {
    return(as.integer(sum(presentes)))
  }
  a <- as.character(antes)[presentes]
  b <- as.character(despues)[presentes]
  as.integer(sum(is.na(b) | a != b))
}

.mascara_aplicabilidad_accion <- function(datos, columna, parametros) {
  regla <- parametros$aplicabilidad
  if (is.null(regla)) return(rep(TRUE, nrow(datos)))
  mascara <- tryCatch(
    .evaluar_predicado_aplicabilidad(datos, columna, regla),
    error = function(e) e
  )
  if (inherits(mascara, "error")) {
    .detener(
      "No se pudo respetar la aplicabilidad de `", columna, "`: ",
      conditionMessage(mascara), call. = FALSE
    )
  }
  if (length(mascara) != nrow(datos)) {
    .detener(
      "No se pudo respetar la aplicabilidad de `", columna,
      "`: la regla no devolvi\u00f3 una marca por fila.", call. = FALSE
    )
  }
  !is.na(mascara) & mascara
}

# Los nombres que aplica una accion de nombres son los del PLAN: el usuario puede
# editar `nombres_propuestos`, y el ejecutor los recalculaba desde cero -se
# aplicaban otros y el registro publicaba los editados como aplicados; medido-.
# Se aplica el mapa `nombres_esperados` -> `nombres_propuestos`;
# una columna que no estaba en el plan conserva su nombre, y eso incluye las
# marcas que el propio plan agrega -`.fila_duplicada`-, que snake_case renombraba.
# Sin el mapa -un plan armado a mano- se calcula como antes.
.nombres_segun_plan <- function(anteriores, estrategia, parametros) {
  esperados <- parametros$nombres_esperados
  propuestos <- parametros$nombres_propuestos
  if (is.null(esperados) || is.null(propuestos)) {
    return(if (identical(estrategia, "normalizar_nombres")) {
      .nombres_make_names(anteriores)
    } else {
      .nombres_snake(anteriores)
    })
  }
  propuestos <- as.character(propuestos)
  if (length(propuestos) != length(esperados) || anyNA(propuestos) ||
      any(!nzchar(propuestos))) {
    .detener(
      "`nombres_propuestos` tiene que traer un nombre no vac\u00edo por cada ",
      "uno de `nombres_esperados` (", length(esperados), ").", call. = FALSE
    )
  }
  # Si los nombres son los del plan en el mismo orden, el mapa es POR POSICION:
  # con nombres repetidos -`" x"`, `" x"`- el mapa por nombre mandaba los dos al
  # primer propuesto.
  indices <- if (identical(.nombres_para_operar(anteriores),
                           .nombres_para_operar(as.character(esperados)))) {
    seq_along(anteriores)
  } else {
    match(.nombres_para_operar(anteriores),
          .nombres_para_operar(as.character(esperados)))
  }
  nuevos <- anteriores
  nuevos[!is.na(indices)] <- propuestos[indices[!is.na(indices)]]
  if (anyDuplicated(.nombres_para_operar(nuevos))) {
    repetidos <- unique(nuevos[duplicated(.nombres_para_operar(nuevos))])
    .detener(
      "Los nombres propuestos repiten ", paste0("`", repetidos, "`", collapse = ", "),
      ": una tabla no puede tener dos columnas con el mismo nombre.", call. = FALSE
    )
  }
  nuevos
}

# La columna que devuelve una accion de texto conserva los atributos de la
# original. Los ejecutores empiezan con `as.character()`, que los descarta, y
# `recortar_espacios` -recomendada y activa por omision- borraba la etiqueta de
# variable que deja `haven` en las seis filas, tambien en las que no toco;
# medido. La clase y los niveles se copian solo si el tipo no
# cambio y la columna no era un factor: un factor vuelve como texto, que es el
# contrato escrito.
.con_atributos_de <- function(original, nuevo) {
  atributos <- attributes(original)
  if (is.null(atributos) || length(nuevo) != length(original)) return(nuevo)
  quitar <- c("names", "dim", "dimnames")
  if (is.factor(original) || !identical(typeof(original), typeof(nuevo))) {
    quitar <- c(quitar, "class", "levels")
  }
  for (nombre in setdiff(names(atributos), quitar)) {
    attr(nuevo, nombre) <- atributos[[nombre]]
  }
  nuevo
}

.ejecutar_accion <- function(datos, accion, original = datos,
                             protegidas = character(), protecciones = NULL) {
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  columna <- accion$columna[[1L]]

  if (estrategia %in% c(
    "normalizar_nombres", "normalizar_nombres_snake_case"
  )) {
    anteriores <- names(datos)
    nuevos <- .nombres_segun_plan(anteriores, estrategia, parametros)
    names(datos) <- nuevos
    # Un `sf` guarda el nombre de su geometria en `sf_column` y el de los
    # atributos en `agr`: renombrar las columnas sin esos dos dejaba un objeto
    # roto -la geometria `SHAPE` pasaba a `shape` y `sf` ya no la encontraba-
    # mientras el registro decia `ejecutada`. Medido.
    if (inherits(datos, "sf")) {
      geometria <- attr(datos, "sf_column", exact = TRUE)
      if (length(geometria) == 1L && geometria %in% anteriores) {
        attr(datos, "sf_column") <- nuevos[[match(geometria, anteriores)]]
      }
      agr <- attr(datos, "agr", exact = TRUE)
      if (!is.null(agr) && !is.null(names(agr))) {
        indices <- match(names(agr), anteriores)
        names(agr)[!is.na(indices)] <- nuevos[indices[!is.na(indices)]]
        attr(datos, "agr") <- agr
      }
    }
    return(list(
      datos = datos,
      n = sum(.nombres_para_operar(anteriores) !=
                .nombres_para_operar(names(datos)))
    ))
  }
  if (identical(estrategia, "marcar_filas_duplicadas")) {
    marcas <- .grupos_filas_duplicadas(datos)
    datos <- .agregar_marca(datos, parametros$columna_marca, marcas$repetidas)
    datos <- .agregar_marca(datos, parametros$columna_grupo, marcas$grupos)
    return(list(datos = datos, n = sum(!is.na(marcas$grupos))))
  }
  if (identical(estrategia, "conservar_primera_duplicada")) {
    # La MISMA definicion de "misma fila" que el marcador del grupo y que el
    # perfil. `duplicated.data.frame()` crudo arma la fila con `Map()`, que le
    # saca la clase a un `integer64`: dos negativos distintos -patron NaN como
    # doble- eran la misma fila. Sobre `id = -1, -2, 3, -1` el perfil contaba 1
    # duplicada, el marcador marcaba la fila 4 y esta accion borraba ademas la
    # fila 2, con `n_cambiadas = 2`. Medido.
    eliminar <- .grupos_filas_duplicadas(datos)$repetidas
    retiradas <- .filtrar_filas(datos, eliminar)
    datos <- .filtrar_filas(datos, !eliminar)
    return(list(
      datos = datos, n = sum(eliminar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(eliminar), n_columnas_eliminadas = 0
    ))
  }
  if (identical(estrategia, "conservar_mas_completa")) {
    conservar <- .conservar_mas_completa(datos, parametros$clave)
    retiradas <- .filtrar_filas(datos, !conservar)
    datos <- .filtrar_filas(datos, conservar)
    return(list(
      datos = datos, n = sum(!conservar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(!conservar), n_columnas_eliminadas = 0
    ))
  }
  if (estrategia %in% c(
    "marcar_columnas_duplicadas", "eliminar_columna_duplicada"
  )) {
    # `columna_1` es la primera del grupo y `columna_2` las repetidas: una con
    # dos columnas iguales, todas las demas con tres o mas. Se comprueba cada
    # una contra la primera, que es contra la que el perfil las agrupo.
    indice_1 <- .indice_columna(datos, parametros$columna_1)
    repetidas <- as.character(parametros$columna_2)
    if (!length(repetidas)) .indice_columna(datos, parametros$columna_2)
    for (repetida in repetidas) {
      indice_2 <- .indice_columna(datos, repetida)
      if (!.contenido_igual(datos[[indice_1]], datos[[indice_2]])) {
        .detener("Las columnas dejaron de tener contenido duplicado.", call. = FALSE)
      }
    }
    if (identical(estrategia, "marcar_columnas_duplicadas")) {
      marcas <- attr(datos, "columnas_duplicadas_marcadas", exact = TRUE)
      nueva <- data.frame(
        columna_1 = rep(parametros$columna_1, length(repetidas)),
        columna_2 = repetidas,
        stringsAsFactors = FALSE
      )
      combinadas <- if (is.null(marcas)) {
        nueva
      } else {
        unique(rbind(marcas, nueva))
      }
      attr(datos, "columnas_duplicadas_marcadas") <- combinadas
      # El conteo es el cambio REAL, no un numero fijo: las marcas que se
      # agregaron. Con la marca ya puesta -dos acciones iguales en el mismo
      # plan- `unique()` la descarta y no cambia nada, y contarla informaba un
      # efecto que no ocurrio. Con 0, el registro la marca `fallida`, que es lo
      # que promete la documentacion para una accion seleccionada sin efecto.
      return(list(
        datos = datos,
        n = if (identical(combinadas, marcas)) 0 else
          nrow(combinadas) - if (is.null(marcas)) 0L else nrow(marcas)
      ))
    }
    eliminar <- as.character(parametros$eliminar)
    indices <- if (length(eliminar) <= 1L) {
      .indice_columna(datos, parametros$eliminar)
    } else {
      unname(vapply(eliminar, function(nombre) {
        .indice_columna(datos, nombre)
      }, integer(1L)))
    }
    if (anyDuplicated(indices)) {
      .detener(
        "La acci\u00f3n nombra m\u00e1s de una vez la misma columna a eliminar.",
        call. = FALSE
      )
    }
    # Con una sola columna, lo retirado se guarda como siempre; con varias, los
    # mismos tres campos con una entrada por columna, `valores` como lista.
    retirada <- if (length(indices) == 1L) {
      list(
        nombre = names(datos)[[indices]], posicion = indices,
        valores = datos[[indices]]
      )
    } else {
      valores <- lapply(indices, function(j) datos[[j]])
      names(valores) <- names(datos)[indices]
      list(nombre = names(datos)[indices], posicion = indices, valores = valores)
    }
    for (j in .sort_por_bytes(indices, decreasing = TRUE)) datos[[j]] <- NULL
    return(list(
      datos = datos, n = length(indices), columna_eliminada = retirada,
      n_filas_eliminadas = 0, n_columnas_eliminadas = length(indices)
    ))
  }
  if (identical(estrategia, "eliminar_columna_constante")) {
    indice <- .indice_columna(datos, columna)
    x <- datos[[indice]]
    if (length(unique(x[!is.na(x)])) > 1L) {
      .detener("La columna dej\u00f3 de ser constante.", call. = FALSE)
    }
    retirada <- list(nombre = columna, posicion = indice, valores = x)
    datos[[indice]] <- NULL
    return(list(
      datos = datos, n = 1, columna_eliminada = retirada,
      n_filas_eliminadas = 0, n_columnas_eliminadas = 1
    ))
  }

  indice <- .indice_columna(datos, columna)
  x <- datos[[indice]]
  # Se pide dentro de cada accion de texto y no aca: el argumento no se evalua
  # hasta que la proteccion lo mira, y solo lo mira si la accion cambia alguna
  # celda. Una accion que no es de texto no paga la mascara de la columna.
  proteccion <- function() .proteccion_texto_columna(protecciones, columna, x)
  if (identical(estrategia, "convertir_ausencias_textuales")) {
    cambio <- .reemplazar_ausencias_textuales(x, parametros, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    # Cada celda que se convierte en ausencia pierde el texto que tenia: `S/D`
    # o `N/A` no quedan en ningun lado. El plan ya declara la accion
    # `reversible = FALSE`, pero el registro informaba `n_no_reversibles = 0`
    # porque este ejecutor no lo devolvia y `aplicar()` completa con cero: dos
    # perdidas reales y ninguna cuantificada. Aca todo cambio es irreversible.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (identical(estrategia, "convertir_sentinelas_numericos")) {
    cambio <- .reemplazar_sentinelas_numericos(
      x, parametros,
      aplicable = .mascara_universo_guarda(datos, original, columna, parametros),
      x_guarda = .columna_para_guarda(original, columna, x, parametros)
    )
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    # Cada celda cambiada pierde su valor original y ningun formato lo
    # recupera. El registro informaba `n_no_reversibles = 0` porque este
    # ejecutor no lo devolvia y `aplicar()` completa con cero: las mismas
    # perdidas que ya se cuentan en `convertir_ausencias_textuales`.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (startsWith(estrategia, "imputar_dependencia_funcional__")) {
    return(.imputar_dependencia(datos, parametros, protegidas))
  }
  if (identical(estrategia, "recortar_espacios")) {
    cambio <- .recortar_texto(x, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "eliminar_controles_invisibles")) {
    cambio <- .quitar_controles_invisibles(x, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "normalizar_espacios_invisibles")) {
    cambio <- .normalizar_espacios_invisibles(x, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n,
                n_no_reversibles = cambio$n_no_reversibles))
  }
  if (identical(estrategia, "decodificar_entidades_html")) {
    cambio <- .decodificar_entidades_html(x, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "reemplazar_separadores")) {
    cambio <- .reemplazar_separadores(x, proteccion())
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n))
  }
  if (identical(estrategia, "reparar_codificacion")) {
    cambio <- .reparar_codificacion(x, parametros)
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n,
                estado_reparacion = cambio$estado_reparacion,
                n_parciales = cambio$n_parciales))
  }
  if (identical(estrategia, "convertir_numero_regional")) {
    cambio <- .convertir_numero_regional(x, parametros)
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    evaluacion <- .evaluar_conversion(
      x, cambio$valor, estrategia, parametros
    )
    # Mismo conteo que las otras dos conversiones de tipo: contaba solo los
    # valores no vacios, asi que un `""` que la conversion vuelve `NA` no
    # figuraba entre los cambios.
    return(list(datos = datos, n = .n_valores_cambiados(x, cambio$valor),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  if (identical(estrategia, "marcar_filas_ausentes")) {
    # Sin regla de aplicabilidad no hay nada que alinear: todas las filas
    # actuales aplican. La mascara se pedia igual sobre los ORIGINALES, y despues
    # de que otra accion eliminara filas -conservar la primera duplicada, que el
    # propio plan ordena antes- el largo no coincidia y la accion fallaba
    # culpando a una aplicabilidad que el plan no declaraba.
    aplicable <- if (is.null(parametros$aplicabilidad)) {
      rep(TRUE, length(x))
    } else {
      .mascara_aplicabilidad_accion(original, columna, parametros)
    }
    if (length(aplicable) != length(x)) {
      .detener(
        "No se pudo respetar la aplicabilidad despu\u00e9s de eliminar filas.",
        call. = FALSE
      )
    }
    # La ausencia es la del perfil: un nivel `NA` de un factor cuenta. Ver
    # `.factor_sin_nivel_na()`; la columna no se modifica.
    marca <- is.na(.factor_sin_nivel_na(x)) & aplicable
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "eliminar_filas_ausentes")) {
    # Sin regla de aplicabilidad no hay nada que alinear: todas las filas
    # actuales aplican. La mascara se pedia igual sobre los ORIGINALES, y despues
    # de que otra accion eliminara filas -conservar la primera duplicada, que el
    # propio plan ordena antes- el largo no coincidia y la accion fallaba
    # culpando a una aplicabilidad que el plan no declaraba.
    aplicable <- if (is.null(parametros$aplicabilidad)) {
      rep(TRUE, length(x))
    } else {
      .mascara_aplicabilidad_accion(original, columna, parametros)
    }
    if (length(aplicable) != length(x)) {
      .detener(
        "No se pudo respetar la aplicabilidad despu\u00e9s de eliminar filas.",
        call. = FALSE
      )
    }
    eliminar <- is.na(.factor_sin_nivel_na(x)) & aplicable
    retiradas <- .filtrar_filas(datos, eliminar)
    datos <- .filtrar_filas(datos, !eliminar)
    return(list(
      datos = datos, n = sum(eliminar), filas_eliminadas = retiradas,
      n_filas_eliminadas = sum(eliminar), n_columnas_eliminadas = 0
    ))
  }
  if (identical(estrategia, "convertir_fecha_confirmada")) {
    convertido <- .convertir_fecha(x, parametros)
    datos[[indice]] <- .con_atributos_de(x, convertido)
    evaluacion <- .evaluar_conversion(
      x, convertido, estrategia, parametros
    )
    return(list(datos = datos, n = .n_valores_cambiados(x, convertido),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  if (identical(estrategia, "convertir_tipo")) {
    convertido <- .convertir_tipo(x, parametros)
    # La etiqueta de variable sobrevive a la conversion como a las acciones de
    # texto; la clase y los niveles no, porque el tipo cambio. Ver
    # `.con_atributos_de()`. Medido.
    datos[[indice]] <- .con_atributos_de(x, convertido)
    evaluacion <- .evaluar_conversion(
      x, convertido, estrategia, parametros
    )
    return(list(datos = datos, n = .n_valores_cambiados(x, convertido),
                n_no_reversibles = evaluacion$n_no_reversibles))
  }
  dentro_outliers <- NULL
  if (estrategia %in% .estrategias_outliers && !is.null(parametros$aplicabilidad)) {
    if (nrow(original) != nrow(datos)) {
      .detener(
        "No se puede respetar la aplicabilidad despu\u00e9s de eliminar filas: ",
        "aplicar las acciones de outliers antes de las que eliminan filas.",
        call. = FALSE
      )
    }
    dentro_outliers <- .mascara_aplicabilidad_accion(original, columna, parametros)
  }
  if (identical(estrategia, "marcar_outliers")) {
    marca <- .marca_outliers(x, dentro_outliers)
    datos <- .agregar_marca(datos, parametros$columna_marca, marca)
    return(list(datos = datos, n = sum(marca)))
  }
  if (identical(estrategia, "winsorizar_outliers")) {
    cambio <- .winsorizar_outliers(x, dentro_outliers)
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    # Cada celda cambiada pierde su valor original y ningun formato lo
    # recupera. El registro informaba `n_no_reversibles = 0` porque este
    # ejecutor no lo devolvia y `aplicar()` completa con cero: las mismas
    # perdidas que ya se cuentan en `convertir_ausencias_textuales`.
    return(list(datos = datos, n = cambio$n, n_no_reversibles = cambio$n))
  }
  if (estrategia %in% c(
    "convertir_minusculas", "convertir_titulo", "convertir_mayusculas",
    "convertir_segun_diccionario"
  )) {
    cambio <- .transformar_capitalizacion(
      x, estrategia, parametros, proteccion()
    )
    datos[[indice]] <- .con_atributos_de(x, cambio$valor)
    return(list(datos = datos, n = cambio$n))
  }
  .detener("Estrategia de limpieza no implementada: ", estrategia, ".", call. = FALSE)
}

.registro_vacio <- function() {
  estructura <- data.frame(
    id_accion = character(), columna = character(), hallazgo = character(),
    grupo = character(), decision_grupo = character(), estrategia = character(),
    destructiva = logical(), n_cambiadas = numeric(),
    n_no_reversibles = numeric(), justificacion = character(),
    estado = character(), error = character(),
    estado_reparacion = character(),
    # El registro VACIO tiene que tener la misma forma que el lleno: si le falta
    # una columna, `rbind()` de una corrida sin acciones contra una con acciones
    # falla o -peor- deja la tabla con una columna que aparece y desaparece
    # segun si hubo algo que aplicar.
    n_codificacion_normalizada = numeric(),
    n_filas_eliminadas = numeric(), n_columnas_eliminadas = numeric(),
    fecha_hora = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  estructura$parametros <- I(list())
  estructura
}

.estrategias_eliminatorias <- function() {
  c(
    "conservar_primera_duplicada", "conservar_mas_completa",
    "eliminar_filas_ausentes", "eliminar_columna_duplicada",
    "eliminar_columna_constante"
  )
}

# Quien edita un plan no conoce las funciones internas, y ese es justamente el
# caso de uso que la capa de remediacion declara: un plan editable. Medido
# mutando los veinte nombres de parametro que produce un plan, en seis casos de
# datos: solo los dos que nombran una columna a crear fallaban con el mensaje
# interno de R -"argumento tiene longitud cero" con largo cero, "valor ausente
# donde TRUE/FALSE es necesario" con NA-. Con la cadena vacia era peor que
# fallar: creaba una columna llamada "V3", que no es la que el plan decia.
#
# No se valida mas que eso a proposito. Seis de los veintiocho parametros que
# produce un plan legitimo son de largo cero o todo NA -`clave = character(0)`
# significa "sin clave declarada", y `punto_sin_coma = NA` significa "sin
# determinar"-, asi que una regla general sobre todos los parametros
# rechazaria planes que funcionan.
.parametros_nombre_de_columna <- c("columna_marca", "columna_grupo")

.motivo_parametros_accion <- function(accion) {
  parametros <- accion$parametros[[1L]]
  # Un plan con `parametros[i] <- list(NULL)` dejaba en el registro el error
  # interno de R, "argumento tiene longitud cero". Una lista vacia, en cambio,
  # es legitima: es la de las acciones que no llevan parametros.
  if (is.null(parametros)) {
    return(paste0(
      "Los par\u00e1metros de la acci\u00f3n `", accion$estrategia[[1L]],
      "` no tienen la forma que la acci\u00f3n espera: una lista con nombres, ",
      "como la que propone el plan; recibi\u00f3 `NULL`."
    ))
  }
  if (!length(parametros)) return(NULL)
  # Los parametros de una accion son una lista con nombres. Un plan editado con
  # `parametros[[i]] <- c(-9999)` fallaba con el mensaje interno de R, "$
  # operator is invalid for atomic vectors".
  if (!is.list(parametros) || is.null(names(parametros))) {
    return(paste0(
      "Los par\u00e1metros de la acci\u00f3n `", accion$estrategia[[1L]],
      "` no tienen la forma que la acci\u00f3n espera: una lista con nombres, ",
      "como la que propone el plan; recibi\u00f3 ",
      if (is.list(parametros)) "una lista sin nombres" else
        paste0("un vector de clase `", class(parametros)[[1L]], "`"),
      "."
    ))
  }
  nombres <- intersect(names(parametros), .parametros_nombre_de_columna)
  for (nombre in nombres) {
    valor <- parametros[[nombre]]
    problema <- if (length(valor) != 1L) {
      paste0("recibi\u00f3 ", length(valor),
             if (length(valor) == 1L) " valor" else " valores")
    } else if (is.na(valor)) {
      "recibi\u00f3 NA"
    } else if (!nzchar(trimws(as.character(valor)))) {
      "recibi\u00f3 una cadena vac\u00eda"
    } else {
      NULL
    }
    if (!is.null(problema)) {
      return(paste0(
        "El par\u00e1metro `", nombre, "` de la acci\u00f3n `",
        accion$estrategia[[1L]], "` debe ser el nombre de la columna a crear: ",
        "un \u00fanico texto no vac\u00edo; ", problema, "."
      ))
    }
  }
  NULL
}

.motivo_efecto_accion <- function(accion, ejecutada) {
  actual <- as.numeric(ejecutada$n)
  if (length(actual) != 1L || !is.finite(actual) || actual != 0L) {
    return(NULL)
  }
  esperado <- as.numeric(accion$n_afectadas[[1L]])
  unidad <- as.character(accion$unidad_conteo[[1L]])
  detalle_estimacion <- if (length(esperado) == 1L && is.finite(esperado)) {
    paste0(
      " El plan estimaba ", esperado,
      if (length(unidad) && !is.na(unidad) && nzchar(unidad)) {
        paste0(" ", unidad)
      } else "", "."
    )
  } else {
    " El plan no contiene una estimaci\u00f3n v\u00e1lida; la comprobaci\u00f3n se basa en el efecto observado."
  }
  paste0(
    "La acci\u00f3n `", accion$estrategia[[1L]],
    "` qued\u00f3 sin efecto: no cambi\u00f3 ning\u00fan valor.",
    detalle_estimacion
  )
}

# Los campos del perfil con que el plan cuenta cada accion de texto
# (`n_afectadas`): los publica `.diagnosticar_texto()`, y `aplicar()` vuelve a
# correr esa misma deteccion para saber si el problema sigue en los datos.
.CAMPOS_DETECCION_ACCION_TEXTO <- list(
  recortar_espacios = "n_espacios_borde",
  eliminar_controles_invisibles = "n_invisibles_eliminables",
  normalizar_espacios_invisibles = "n_espacios_invisibles",
  decodificar_entidades_html = "n_entidades_html",
  reemplazar_separadores = "n_separadores_en_campo",
  reparar_codificacion = c(
    "n_codificacion_reparable", "n_codificacion_reparable_parcialmente"
  )
)

# Cuantas celdas de `columna` cuenta, en `datos`, la deteccion con que el plan
# conto la accion. `NA` si la estrategia no tiene esa deteccion o no se puede
# medir: quien llama lo lee como "no se sabe".
.conteo_deteccion_accion <- function(estrategia, datos, columna) {
  campos <- .CAMPOS_DETECCION_ACCION_TEXTO[[estrategia]]
  if (is.null(campos) || length(columna) != 1L || is.na(columna)) {
    return(NA_real_)
  }
  indice <- tryCatch(.indice_columna(datos, columna), error = function(e) NULL)
  if (is.null(indice)) return(NA_real_)
  x <- datos[[indice]]
  if (!is.character(x) && !is.factor(x)) return(NA_real_)
  deteccion <- tryCatch(.diagnosticar_texto(x), error = function(e) NULL)
  if (is.null(deteccion)) return(NA_real_)
  conteos <- suppressWarnings(as.numeric(unlist(deteccion[campos])))
  if (length(conteos) != length(campos) || anyNA(conteos)) return(NA_real_)
  sum(conteos)
}

# **Sin efecto porque otra accion del plan ya hizo el trabajo.** Una accion que
# no cambia nada se registraba `fallida` aunque el problema que la motivo ya no
# estuviera: con 200 tablas de texto, el plan por omision dejaba
# `normalizar_espacios_invisibles` fallida porque `eliminar_controles_invisibles`
# ya habia dejado sus dos celdas sin espacio invisible que contar, y
# `recortar_espacios` fallida porque `convertir_ausencias_textuales` ya habia
# vuelto ausentes sus cinco celdas en blanco. Despues de aplicar el plan que el
# paquete propuso, "fallida" decia que algo habia salido mal sobre datos
# limpios.
#
# Cuenta como trabajo hecho solo si la misma deteccion con que el plan conto la
# accion encontraba el problema en los datos que recibio `aplicar()` y ya no lo
# encuentra cuando a la accion le toca correr. Si el problema sigue -la accion
# no pudo con el- o nunca estuvo en estos datos -un plan aplicado a otra
# entrega-, la accion sigue `fallida`. Una estrategia sin esa deteccion tambien:
# no se afirma un trabajo hecho que no se puede medir.
.problema_resuelto_por_el_plan <- function(accion, recibidos, actuales) {
  estrategia <- as.character(accion$estrategia[[1L]])
  columna <- as.character(accion$columna[[1L]])
  if (is.null(.CAMPOS_DETECCION_ACCION_TEXTO[[estrategia]])) return(FALSE)
  antes <- .conteo_deteccion_accion(estrategia, recibidos, columna)
  if (!isTRUE(antes > 0)) return(FALSE)
  isTRUE(.conteo_deteccion_accion(estrategia, actuales, columna) == 0)
}

.accion_modifica_clave <- function(accion, clave) {
  if (!length(clave)) return(FALSE)
  estrategia <- accion$estrategia[[1L]]
  parametros <- accion$parametros[[1L]]
  if (estrategia %in% c("normalizar_nombres", "normalizar_nombres_snake_case")) {
    esperados <- parametros$nombres_esperados
    propuestos <- parametros$nombres_propuestos
    if (length(esperados) && length(propuestos) &&
        length(esperados) == length(propuestos)) {
      cambiadas <- .nombres_para_operar(esperados) !=
        .nombres_para_operar(propuestos)
      return(any(.nombres_para_operar(clave) %in%
                 .nombres_para_operar(esperados[cambiadas])))
    }
    return(TRUE)
  }
  if (estrategia %in% c(
    "marcar_filas_duplicadas", "marcar_columnas_duplicadas",
    "marcar_outliers"
  )) return(FALSE)
  if (estrategia %in% c(
    "eliminar_columna_duplicada", "eliminar_columna_constante"
  )) {
    # `eliminar` nombra todas las repetidas de un grupo de columnas iguales:
    # basta con que una sea de la clave.
    return(any(.nombres_para_operar(parametros$eliminar) %in%
                 .nombres_para_operar(clave)) ||
      isTRUE(.nombres_para_operar(accion$columna[[1L]]) %in%
             .nombres_para_operar(clave)))
  }
  columna <- accion$columna[[1L]]
  !is.na(columna) && .nombres_para_operar(columna) %in%
    .nombres_para_operar(clave)
}

#' @rdname planificar_limpieza
#' @param permitir_eliminacion Segundo consentimiento obligatorio para ejecutar
#'   acciones que eliminan filas o columnas. Sin él, una acción eliminatoria
#'   seleccionada hace que `aplicar()` **se niegue antes de tocar nada**: no se
#'   ejecuta ninguna acción del plan —tampoco las que no eliminan— y no hay
#'   registro, porque no hubo ejecución. El error nombra las acciones y los dos
#'   caminos: dar el consentimiento, o desactivarlas y aplicar el resto.
#' @param conservar_eliminados Si se conservan en el resultado las filas y
#'   columnas retiradas. Es `TRUE` de forma predeterminada.
#' @export
aplicar <- function(plan, datos, permitir_eliminacion = FALSE,
                    conservar_eliminados = TRUE) {
  era_plan <- inherits(plan, "plan_limpieza")
  # Las columnas que la proteccion tapo al planificar: con ellas se valida un
  # mapa enmascarado sobre los datos recibidos (`.resolver_mapa_enmascarado()`).
  protegidas <- attr(plan, "columnas_datos_personales_protegidas", exact = TRUE)
  .validar_plan_limpieza(plan)
  plan <- .tabla_base(plan)
  if (era_plan) class(plan) <- unique(c("plan_limpieza", class(plan)))
  plan <- .sincronizar_decisiones(plan)
  if (!inherits(datos, "data.frame")) {
    .detener("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  if (!is.logical(permitir_eliminacion) || length(permitir_eliminacion) != 1L ||
      is.na(permitir_eliminacion) || !is.logical(conservar_eliminados) ||
      length(conservar_eliminados) != 1L || is.na(conservar_eliminados)) {
    .detener(
      "Los permisos de eliminaci\u00f3n deben ser l\u00f3gicos escalares sin NA.",
      call. = FALSE
    )
  }
  seleccion <- which(plan$aplicar)
  if (length(seleccion)) {
    # El desempate va por `id_accion`, que identifica a la accion y no cambia
    # si el usuario reordena las filas del plan. Desempatar por el indice de
    # fila hacia que el `orden` dejara de fijar una secuencia reproducible:
    # con dos acciones empatadas sobre la misma columna, el mismo plan con las
    # filas invertidas devolvia datos distintos -` A` contra `A`-, que es lo
    # contrario de lo que promete `planificar_limpieza()`.
    seleccion <- seleccion[.orden_por_bytes(
      plan$orden[seleccion],
      .clave_bytes(as.character(plan$id_accion[seleccion]))
    )]
    # La reparacion es una barrera de lectura por columna: si esta seleccionada,
    # se adelanta a las otras acciones de esa columna, pero no altera el orden
    # entre columnas independientes.
    columnas <- unique(plan$columna[seleccion])
    for (columna in columnas) {
      if (is.na(columna)) next
      mismas <- !is.na(plan$columna[seleccion]) &
        plan$columna[seleccion] == columna
      reparaciones <- seleccion[mismas &
        plan$estrategia[seleccion] == "reparar_codificacion"]
      otras <- seleccion[mismas &
        plan$estrategia[seleccion] != "reparar_codificacion"]
      if (!length(reparaciones) || !length(otras)) next
      sin_reparaciones <- seleccion[!seleccion %in% reparaciones]
      primera <- which(sin_reparaciones %in% otras)[[1L]]
      seleccion <- c(
        sin_reparaciones[seq_len(primera - 1L)], reparaciones,
        sin_reparaciones[primera:length(sin_reparaciones)]
      )
    }
    # Una entidad con nombre distingue la caja -`&eacute;` es la e minuscula y
    # `&Eacute;` la mayuscula-, asi que cambiar la caja antes de decodificar
    # cambia la letra: con el orden editado, `jos&eacute;` pasaba a
    # `Jos&Eacute;` y se decodificaba con la E mayuscula sin aviso, y `&amp;` a
    # `&Amp;`, que ya no es una entidad. Es la misma barrera de lectura que la
    # reparacion, mas angosta: la decodificacion se adelanta solo a las
    # conversiones de caja de su columna y solo si quedaba detras de alguna. Con
    # el orden del plan ya corre antes (207 contra 600) y nada se mueve.
    cajas <- c("convertir_minusculas", "convertir_titulo", "convertir_mayusculas")
    for (columna in columnas) {
      if (is.na(columna)) next
      mismas <- !is.na(plan$columna[seleccion]) &
        plan$columna[seleccion] == columna
      decodificaciones <- seleccion[mismas &
        plan$estrategia[seleccion] == "decodificar_entidades_html"]
      de_caja <- seleccion[mismas & plan$estrategia[seleccion] %in% cajas]
      if (!length(decodificaciones) || !length(de_caja)) next
      if (max(match(decodificaciones, seleccion)) <
          min(match(de_caja, seleccion))) next
      sin_decodificar <- seleccion[!seleccion %in% decodificaciones]
      primera <- which(sin_decodificar %in% de_caja)[[1L]]
      seleccion <- c(
        sin_decodificar[seq_len(primera - 1L)], decodificaciones,
        sin_decodificar[primera:length(sin_decodificar)]
      )
    }
  }
  # La puerta del consentimiento deriva de la ESTRATEGIA, no de la celda
  # `destructiva`. Combinarlas la volvia evitable con una edicion: poner
  # `destructiva = FALSE` en una fila de `eliminar_filas_ausentes` borraba la
  # mitad de las filas sin `permitir_eliminacion`. Que hace una accion lo sabe
  # el paquete; el plan editado dice que se aplica, no que hace.
  #
  # Se probo tambien sellar las columnas de diagnostico y rechazar su edicion,
  # y se RETIRO: editar `estado` es la palanca sancionada para correr una
  # accion bloqueada, y editar `estrategia` es como se ejercita la rama no
  # implementada. La huella cerraba un agujero que este cambio ya cierra en la
  # raiz, y de paso silenciaba dos usos legitimos.
  destructivas <- seleccion[
    plan$estrategia[seleccion] %in% .estrategias_eliminatorias()
  ]
  if (length(destructivas) && !permitir_eliminacion) {
    # La negativa se explica entera: que no corrio NADA -ni las acciones
    # benignas que estaban seleccionadas- y cuales son los dos caminos. Decir
    # solo "requiere permitir_eliminacion" deja abierta la lectura de que lo
    # demas si se aplico, y el objeto que lo desmentiria -el registro- no
    # existe, porque esta negativa ocurre antes de tocar nada.
    .detener(
      "El plan contiene acciones destructivas y requiere ",
      "`permitir_eliminacion = TRUE`: ",
      paste(plan$estrategia[destructivas], collapse = ", "), ". ",
      "No se aplic\u00f3 ninguna acci\u00f3n y `datos` qued\u00f3 intacto; ",
      "volver a llamar con el consentimiento, o desactivar esas acciones ",
      "-`plan$aplicar[...] <- FALSE`- para aplicar el resto.",
      call. = FALSE
    )
  }
  # Una accion de nombres que no corresponde a estos datos -un perfil con dos
  # `x` y datos con una- se registra `fallida` y las demas siguen, como toda
  # accion `lista` que falla. Antes `aplicar()` abortaba entero, y con el se
  # perdian las acciones validas del mismo plan.
  nombres_iniciales <- names(datos)
  salida <- .copiar_datos(datos)
  protecciones <- .preparar_protecciones_texto()
  registros <- vector("list", length(seleccion))
  eliminados <- list(filas = list(), columnas = list())
  for (j in seq_along(seleccion)) {
    accion <- plan[seleccion[[j]], , drop = FALSE]
    # El estado de ESTE momento, para medir despues lo que la accion perdio.
    estado_previo <- salida
    clave <- if (inherits(salida, "data.table") &&
                 requireNamespace("data.table", quietly = TRUE)) {
      data.table::key(salida)
    } else {
      character()
    }
    # Cada acción trabaja sobre una copia del estado anterior. Si falla, la
    # columna (o tabla) queda intacta y el resto del plan puede continuar.
    motivo_parametros <- .motivo_parametros_accion(accion)
    if (is.null(motivo_parametros)) {
      motivo_parametros <- .motivo_nombres_distintos(accion, nombres_iniciales)
    }
    ejecutada <- if (!is.null(motivo_parametros)) {
      list(error = motivo_parametros, n = 0, n_no_reversibles = 0)
    } else {
      tryCatch(
        .ejecutar_accion(.copiar_datos(salida), accion, original = datos,
                         protegidas = protegidas, protecciones = protecciones),
        error = function(e) list(
          error = conditionMessage(e), n = 0, n_no_reversibles = 0
        )
      )
    }
    if (is.null(ejecutada$error)) {
      ejecutada <- .restringir_a_aplicabilidad(ejecutada, salida, accion, datos)
    }
    if (is.null(ejecutada$error)) {
      motivo_efecto <- .motivo_efecto_accion(accion, ejecutada)
      # Sin efecto porque una accion anterior ya resolvio el problema: queda
      # `ejecutada` con cero cambios. Ver `.problema_resuelto_por_el_plan()`.
      if (!is.null(motivo_efecto) &&
          !.problema_resuelto_por_el_plan(accion, datos, estado_previo)) {
        ejecutada$error <- motivo_efecto
      }
    }
    fallo <- !is.null(ejecutada$error)
    if (!fallo) {
      salida <- ejecutada$datos
      if (isTRUE(ejecutada$n > 0)) {
        # Cada accion de texto puede acortar o reescribir la columna. La
        # proteccion se alinea de nuevo con el estado que queda, no con el
        # largo del perfil inicial; la siguiente accion recibe posiciones
        # validas aunque una anterior haya quitado caracteres.
        protecciones <- .actualizar_proteccion_texto(
          protecciones, salida, accion$columna[[1L]]
        )
      }
      if (length(clave) && .accion_modifica_clave(accion, clave)) {
        data.table::setkey(salida, NULL)
      }
    }
    # La accion de quitar una columna duplicada va en el plan bajo la columna que
    # se CONSERVA -es la del hallazgo-, y el registro la publicaba como la tocada:
    # nombraba la que quedo y no la que se elimino. Medido.
    # Con un grupo de tres o mas columnas iguales se eliminan varias, y se
    # nombran todas separadas por coma, como un hallazgo de varias columnas.
    columna_registro <- accion$columna[[1L]]
    if (identical(accion$estrategia[[1L]], "eliminar_columna_duplicada") &&
        length(accion$parametros[[1L]]$eliminar) >= 1L) {
      eliminadas <- as.character(accion$parametros[[1L]]$eliminar)
      columna_registro <- if (length(eliminadas) == 1L) {
        eliminadas
      } else {
        .pegar_texto(eliminadas, collapse = ",")
      }
    }
    registro <- data.frame(
      id_accion = accion$id_accion[[1L]],
      columna = columna_registro,
      hallazgo = accion$hallazgo[[1L]],
      grupo = accion$grupo[[1L]],
      decision_grupo = as.character(accion$decision_grupo[[1L]]),
      estrategia = accion$estrategia[[1L]],
      destructiva = accion$destructiva[[1L]],
      n_cambiadas = as.numeric(if (fallo) 0 else ejecutada$n),
      n_no_reversibles = as.numeric(if (fallo) 0 else {
        if (is.null(ejecutada$n_no_reversibles)) {
          .perdidas_al_aplicar(estado_previo, salida, accion$columna[[1L]])
        } else {
          ejecutada$n_no_reversibles
        }
      }),
      justificacion = as.character(accion$justificacion[[1L]]),
      estado = if (fallo) "fallida" else "ejecutada",
      error = if (fallo) as.character(ejecutada$error) else NA_character_,
      estado_reparacion = if (is.null(ejecutada$estado_reparacion)) {
        if ("estado_reparacion" %in% names(accion)) {
          as.character(accion$estado_reparacion[[1L]])
        } else NA_character_
      } else as.character(ejecutada$estado_reparacion),
      n_codificacion_normalizada = as.numeric(
        if (fallo) {
          0
        } else {
          .codificaciones_normalizadas(
            estado_previo, salida, accion$columna[[1L]]
          )
        }
      ),
      n_filas_eliminadas = as.numeric(
        if (is.null(ejecutada$n_filas_eliminadas)) 0 else ejecutada$n_filas_eliminadas
      ),
      n_columnas_eliminadas = as.numeric(
        if (is.null(ejecutada$n_columnas_eliminadas)) 0 else ejecutada$n_columnas_eliminadas
      ),
      fecha_hora = as.POSIXct(Sys.time(), tz = "UTC"),
      stringsAsFactors = FALSE
    )
    registro$parametros <- I(list(accion$parametros[[1L]]))
    registros[[j]] <- registro
    if (conservar_eliminados && !is.null(ejecutada$filas_eliminadas)) {
      eliminados$filas[[accion$id_accion[[1L]]]] <- ejecutada$filas_eliminadas
    }
    if (conservar_eliminados && !is.null(ejecutada$columna_eliminada)) {
      eliminados$columnas[[accion$id_accion[[1L]]]] <- ejecutada$columna_eliminada
    }
  }
  registro <- if (length(registros)) do.call(rbind, registros) else .registro_vacio()
  rownames(registro) <- NULL
  attr(salida, "registro_limpieza") <- registro
  # Una accion asigna con `datos[[indice]] <- ...`, que es `[[<-.data.frame`: R
  # copia la tabla y la data.table vuelve sin espacio reservado -`truelength` 0-
  # y con la autorreferencia rota, asi que el primer `:=` del usuario avisaba
  # "Se tomo una copia superficial". La clase volvia, pero degradada. Medido.
  # Se le devuelve el espacio, como lo haria `data.table` misma.
  if (inherits(salida, "data.table") &&
      requireNamespace("data.table", quietly = TRUE)) {
    salida <- data.table::setalloccol(salida)
  }
  estructura <- list(
    datos = salida,
    registro = registro,
    plan_aplicado = plan[seleccion, , drop = FALSE],
    plan = plan,
    eliminados = eliminados
  )
  class(estructura) <- "resultado_limpieza"
  estructura
}

# La evidencia viaja EN EL PLAN, que el usuario edita y vuelve a entregar, asi
# que no puede depender del locale del proceso que lo genero. `encodeString()`
# si depende: el mismo valor salia `"\u00c1"` bajo un locale UTF-8 y
# `"<U+00C1>"` bajo `C`, y dos personas obtenian planes distintos de los mismos
# datos. Se arma el mismo texto sin consultar el locale: los bytes validos se
# declaran, los rotos se escapan en octal, y se escapan a mano la barra, la
# comilla y los controles que `encodeString()` escapaba.
.texto_ejemplo <- function(x) {
  if (length(x) == 0L || is.na(x)) return("<NA>")
  texto <- .clave_bytes(as.character(x))
  texto <- gsub("\\", "\\\\", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub('"', '\\"', texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\n", "\\n", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\r", "\\r", texto, fixed = TRUE, useBytes = TRUE)
  texto <- gsub("\t", "\\t", texto, fixed = TRUE, useBytes = TRUE)
  paste0('"', texto, '"')
}

.ejemplos_grupo <- function(acciones, datos, max_ejemplos = 5L,
                            columnas_protegidas = character(),
                            identificantes = character()) {
  ejemplos <- .ejemplos_grupo_crudos(
    acciones, datos, max_ejemplos, columnas_protegidas
  )
  # Los ejemplos van a la consola y al `selector`, y solo la rama de filas
  # duplicadas miraba las columnas protegidas: para todo grupo POR COLUMNA se
  # publicaban sus valores tal cual -nombres, correos y cedulas como "Ejemplos
  # reales"- mientras el mismo plan tapaba su evidencia. Medido. Una columna
  # protegida no da ejemplos, y los de cualquier otra se
  # barren con el piso, como el perfil.
  columna <- acciones$columna[[1L]]
  if (!is.na(columna) && length(columnas_protegidas) &&
      .nombres_para_operar(columna) %in% .nombres_para_operar(columnas_protegidas)) {
    return("[valores protegidos: la columna lleva datos personales]")
  }
  if (length(identificantes) && length(ejemplos)) {
    ejemplos <- .reemplazar_valores_protegidos(ejemplos, identificantes)
  }
  ejemplos
}

.ejemplos_grupo_crudos <- function(acciones, datos, max_ejemplos = 5L,
                                   columnas_protegidas = character()) {
  tipo <- acciones$hallazgo[[1L]]
  columna <- acciones$columna[[1L]]
  parametros <- acciones$parametros[[1L]]
  valores <- NULL

  if (identical(tipo, "filas_duplicadas")) {
    grupos <- .grupos_filas_duplicadas(datos)$grupos
    indices <- utils::head(which(!is.na(grupos)), max_ejemplos)
    indices_protegidas <- .indice_nombre(columnas_protegidas, names(datos))
    columnas_protegidas <- names(datos)[
      unique(indices_protegidas[!is.na(indices_protegidas)])
    ]
    # Por columna: tambien sus rellenos, que el piso deja afuera.
    valores_protegidos <- c(
      .valores_publicables_protegidos(datos, columnas_protegidas),
      .rellenos_de_columnas(datos, columnas_protegidas)
    )
    return(vapply(indices, function(i) {
      contenido <- vapply(seq_along(datos), function(j) {
        texto <- .texto_ejemplo(datos[[j]][[i]])
        if (.nombres_para_operar(names(datos)[[j]]) %in%
            .nombres_para_operar(columnas_protegidas)) {
          texto <- .reemplazar_valores_protegidos(
            texto, valores_protegidos
          )
        }
        texto
      }, character(1L))
      paste0("fila ", i, " [grupo ", grupos[[i]], "]: ",
             paste(names(datos), contenido, sep = "=", collapse = ", "))
    }, character(1L)))
  }
  if (identical(tipo, "columnas_duplicadas")) {
    columna <- parametros$columna_1
  }
  if (identical(tipo, "nombres_columnas_problematicos")) {
    problema <- .nombres_columnas_problematicos(names(datos))
    originales <- vapply(problema$original, .texto_ejemplo, character(1L))
    propuestos <- vapply(problema$propuesto, .texto_ejemplo, character(1L))
    return(.recortar_ejemplos(paste(originales, "->", propuestos),
                              max_ejemplos))
  }
  if (is.na(columna)) {
    evidencia <- unique(acciones$evidencia[nzchar(acciones$evidencia)])
    return(.recortar_ejemplos(evidencia, max_ejemplos))
  }
  indice <- .indice_columna(datos, columna)
  # Estos ejemplos se le MUESTRAN al usuario para que decida, asi que lo
  # declarado `bytes` se rinde a la forma que publica la consola. Sin esto,
  # `guiar_limpieza()` -que es exportada- moria con el error crudo de R "no se
  # permite traduccion de cadenas con bytes de codificacion" al hacer
  # `tolower()` sobre la columna cruda, y la corrida entera se perdia SIN
  # declarar nada: a diferencia de `aplicar()`, que anota la falla en su
  # registro, aca no quedaba rastro.
  x <- .texto_publicable(datos[[indice]])
  if (identical(tipo, "mayusculas_inconsistentes")) {
    unicos <- unique(as.character(x[!is.na(x)]))
    # El pliegue del detector (`mayusculas_inconsistentes`), no `tolower()`.
    base <- .normalizacion_minusculas_vector(unicos)
    colision <- duplicated(base) | duplicated(base, fromLast = TRUE)
    valores <- unicos[colision]
  } else if (identical(tipo, "faltantes_disfrazados")) {
    texto <- trimws(as.character(x))
    numericos <- .leer_numeros_texto_centinela(
      texto, parametros$convencion
    )
    identidad <- .identidades_sentinelas_texto(
      texto, parametros$valores, parametros$convencion,
      valores_leidos = numericos
    )
    valores <- x[!is.na(identidad)]
  } else if (identical(tipo, "outliers")) {
    valores <- x[.marca_outliers(x)]
  } else if (identical(tipo, "faltantes")) {
    valores <- paste0("<NA> en fila ", which(is.na(x)))
  } else if (identical(tipo, "constante")) {
    valores <- unique(x[!is.na(x)])
  } else {
    valores <- x[!is.na(x)]
  }
  .recortar_ejemplos(
    unique(vapply(valores, .texto_ejemplo, character(1L))), max_ejemplos
  )
}

# El recorte viaja con su total. `guiar_limpieza()` mostraba cinco ejemplos de
# cuarenta sin decirlo, mientras el paquete declara cada recorte en sus
# informes y en sus impresiones. Una lista recortada
# que no dice que lo es se lee como la lista entera, y aca se lee justo antes
# de decidir si aplicar una accion.
.recortar_ejemplos <- function(valores, max_ejemplos) {
  total <- length(valores)
  recortados <- utils::head(valores, max_ejemplos)
  attr(recortados, "total_ejemplos") <- total
  recortados
}

.grupos_para_guiar <- function(plan) {
  grupos <- unique(plan$grupo[!is.na(plan$grupo)])
  grupos[vapply(grupos, function(grupo) {
    indices <- which(plan$grupo == grupo)
    any(as.character(plan$decision_grupo[indices]) == "pendiente") ||
      any(plan$destructiva[indices])
  }, logical(1L))]
}

.resolver_seleccion_guiada <- function(respuesta, acciones, elegibles) {
  # `NA` NO es "no hacer nada". Lo era, y con eso un selector cuya lectura
  # fallara -`as.integer(entrada)` sobre algo que no es un numero devuelve NA-
  # dejaba TODOS los grupos como `omitida`, en silencio: el plan quedaba
  # registrado como revisado y omitido a proposito cuando no se reviso nada, y
  # el `.Rd` reserva `pendiente` para lo no revisado. Un 99, igual de invalido,
  # ya daba un error claro.
  #
  # El camino interactivo no produce NA: `utils::menu()` devuelve 0 al
  # cancelar, que sigue siendo "no hacer nada".
  if (!length(respuesta) ||
      identical(respuesta[[1L]], "no_hacer_nada") ||
      identical(respuesta[[1L]], 0L) || identical(respuesta[[1L]], 0)) {
    return(NA_integer_)
  }
  if (is.na(respuesta[[1L]])) {
    .detener(
      "La selecci\u00f3n guiada devolvi\u00f3 NA, que no identifica ninguna opci\u00f3n. ",
      "Para no hacer nada, devolver 0.", call. = FALSE
    )
  }
  if (is.numeric(respuesta) && length(respuesta) == 1L &&
      respuesta == length(elegibles) + 1L) {
    return(NA_integer_)
  }
  if (is.numeric(respuesta) && length(respuesta) == 1L &&
      respuesta >= 1L && respuesta <= length(elegibles)) {
    return(elegibles[[as.integer(respuesta)]])
  }
  respuesta <- as.character(respuesta[[1L]])
  candidatos <- which(
    acciones$id_accion == respuesta | acciones$estrategia == respuesta
  )
  candidatos <- intersect(candidatos, elegibles)
  if (length(candidatos) != 1L) {
    .detener("La selecci\u00f3n guiada no identifica una opci\u00f3n disponible.", call. = FALSE)
  }
  candidatos[[1L]]
}

#' Revisar decisiones de limpieza paso a paso
#'
#' Recorre los grupos pendientes y aquellos que contienen alternativas
#' destructivas. Muestra evidencia calculada sobre `datos`, las estrategias y
#' sus justificaciones, y devuelve el plan editado sin aplicarlo. En una sesión
#' no interactiva retorna inmediatamente el plan sin cambios, salvo que se
#' proporcione un `selector` explícito.
#'
#' No se representa "no hacer nada" como una acción ficticia. Elegirlo cambia
#' `decision_grupo` a `"omitida"`; por contraste, un grupo aún no revisado
#' conserva `"pendiente"`. Cuando conservar los datos es la recomendación, esa
#' opción también se muestra con la marca "(Recomendado)" y su justificación.
#' Los diccionarios de capitalización se suministran como una lista con nombre
#' de vectores con nombre.
#'
#' @param plan Objeto `plan_limpieza`.
#' @param datos Datos correspondientes al perfil que originó el plan.
#' @param selector Función opcional que recibe una lista con `grupo`,
#'   `acciones`, `elegibles`, `ejemplos` y `opciones`. Debe devolver la posición
#'   —el número que imprime la lista, que cuenta sólo las opciones elegibles—,
#'   el identificador o el nombre de una estrategia, o `0` para no hacer nada. `NA` no es un valor
#'   válido: se rechaza con un error, para que una lectura fallida no quede
#'   registrada como una decisión de omitir.
#' @param diccionarios Lista opcional con nombre de diccionarios por columna.
#' @param max_ejemplos Máximo de ejemplos reales mostrados por grupo.
#'
#' @return El mismo `plan_limpieza` recibido, con la columna de decisión
#'   sincronizada según lo elegido, y sin ejecutar ninguna acción sobre los
#'   datos: cambia qué acciones quedan marcadas para `aplicar()`, no la
#'   tabla. En una sesión no interactiva y sin `selector`, devuelve el plan
#'   sin cambios: **también un plan ya editado**, cuyo `decision_grupo` se
#'   conserva tal como lo recibió. La sincronización de decisiones corre en el
#'   camino interactivo y en [aplicar()], no en la puerta de salida.
#' @export
#' @seealso [planificar_limpieza()], [aplicar()]
#'
#' @examples
#' datos <- data.frame(zona = c("Norte", "NORTE", "sur"))
#' plan <- planificar_limpieza(perfilar(datos), datos)
#' guiado <- guiar_limpieza(plan, datos)
#' identical(plan, guiado) # TRUE en una sesión no interactiva
guiar_limpieza <- function(plan, datos, selector = NULL,
                           diccionarios = list(), max_ejemplos = 5L) {
  era_plan <- inherits(plan, "plan_limpieza")
  .validar_plan_limpieza(plan)
  plan <- .tabla_base(plan)
  if (era_plan) class(plan) <- unique(c("plan_limpieza", class(plan)))
  # La sincronizacion va DESPUES de la puerta de salida. La documentacion promete
  # dos veces -y el ejemplo del roxygen lo ilustra con `identical()`- que en una
  # sesion no interactiva y sin `selector` se devuelve el plan sin cambios, y con
  # un plan EDITADO -la edicion que esta misma capa invita a hacer- no era cierto:
  # `.sincronizar_decisiones()` reescribia `decision_grupo` de `recomendada` a
  # `desactivada`, que en la taxonomia del paquete es otro estado -dice que el
  # usuario desactivo la recomendacion cuando solo la desmarco- y esa afirmacion
  # viaja con el plan si se guarda o se aplica. Medido: la unica columna que
  # cambiaba era `decision_grupo`, y con el plan sin editar `identical()` daba
  # TRUE, que es por lo que el ejemplo pasaba.
  if (is.null(selector) && !interactive()) return(plan)
  plan <- .sincronizar_decisiones(plan)
  if (!is.null(selector) && !is.function(selector)) {
    .detener("`selector` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!inherits(datos, "data.frame")) {
    .detener("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  datos <- .tabla_base(datos)
  if (!is.list(diccionarios) ||
      (length(diccionarios) &&
       (is.null(names(diccionarios)) || any(!nzchar(names(diccionarios)))))) {
    .detener("`diccionarios` debe ser una lista con nombres de columna.", call. = FALSE)
  }
  if (!is.numeric(max_ejemplos) || length(max_ejemplos) != 1L ||
      is.na(max_ejemplos) || max_ejemplos < 1) {
    .detener("`max_ejemplos` debe ser un n\u00famero positivo.", call. = FALSE)
  }
  max_ejemplos <- floor(max_ejemplos)

  grupos <- .grupos_para_guiar(plan)
  columnas_protegidas <- attr(
    plan, "columnas_datos_personales_protegidas", exact = TRUE
  )
  if (is.null(columnas_protegidas)) columnas_protegidas <- character()
  identificantes <- if (length(columnas_protegidas)) {
    .valores_identificantes(
      .valores_publicables_protegidos(datos, columnas_protegidas),
      .fechas_que_identifican(datos, columnas_protegidas)
    )
  } else character()
  for (grupo in grupos) {
    indices <- which(plan$grupo == grupo)
    acciones <- plan[indices, , drop = FALSE]
    nombre_columna <- acciones$columna[[1L]]
    diccionario <- if (!is.na(nombre_columna)) {
      indice_diccionario <- .indice_nombre(nombre_columna, names(diccionarios))
      if (is.na(indice_diccionario)) NULL else diccionarios[[indice_diccionario]]
    } else {
      NULL
    }
    indice_diccionario <- which(
      acciones$estrategia == "convertir_segun_diccionario"
    )
    if (length(indice_diccionario) && !is.null(diccionario)) {
      # Se AGREGA el diccionario a los parametros que la accion ya traia, no se
      # los reemplaza: reemplazarlos tiraba la regla de `aplicabilidad`, y el plan
      # guiado convertia la columna entera, incluidas las filas que el plan
      # declaraba fuera del universo.
      for (i in indices[indice_diccionario]) {
        parametros <- plan$parametros[[i]]
        if (!is.list(parametros)) parametros <- list()
        parametros$diccionario <- diccionario
        plan$parametros[[i]] <- parametros
      }
      plan$estado[indices[indice_diccionario]] <- "lista"
      acciones <- plan[indices, , drop = FALSE]
    }
    elegibles <- which(as.character(acciones$estado) == "lista")
    ejemplos <- .ejemplos_grupo(
      acciones, datos, max_ejemplos, columnas_protegidas, identificantes
    )
    etiquetas <- paste0(
      acciones$estrategia[elegibles],
      ifelse(acciones$recomendada[elegibles], " (Recomendado)", "")
    )
    no_hacer_recomendado <- any(
      acciones$recomendacion_grupo == "no_hacer_nada", na.rm = TRUE
    )
    etiqueta_no_hacer <- paste0(
      "No hacer nada",
      if (no_hacer_recomendado) " (Recomendado)" else ""
    )

    cli::cli_h2(.cli_literal(.marcar_para_exhibir(paste("Decisi\u00f3n", grupo))))
    cli::cli_text(.cli_literal(.marcar_para_exhibir(
      paste("Hallazgo:", acciones$hallazgo[[1L]])
    )))
    cli::cli_text(.cli_literal(.marcar_para_exhibir(paste(
      "Objeto afectado:",
      if (is.na(acciones$columna[[1L]])) "tabla" else acciones$columna[[1L]]
    ))))
    cantidades <- acciones$n_afectadas[is.finite(acciones$n_afectadas)]
    cantidad <- if (length(cantidades)) max(cantidades) else NA_real_
    unidades <- unique(as.character(acciones$unidad_conteo))
    unidades <- unidades[!is.na(unidades) & nzchar(unidades)]
    unidad <- if (length(unidades)) {
      paste0(" (unidad: ", paste(unidades, collapse = ", "), ")")
    } else {
      ""
    }
    cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0("Cantidad estimada: ", cantidad, unidad))))
    if (length(ejemplos)) {
      total_ejemplos <- attr(ejemplos, "total_ejemplos", exact = TRUE)
      cuantos <- if (length(total_ejemplos) == 1L &&
                     isTRUE(total_ejemplos > length(ejemplos))) {
        paste0(" (", length(ejemplos), " de ", total_ejemplos, ")")
      } else {
        ""
      }
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0(
        "Ejemplos reales", cuantos, ": ", paste(ejemplos, collapse = "; ")
      ))))
    }
    # Se imprime la POSICION entre las elegibles, que es lo que numera
    # `utils::menu()` y lo que resuelve `.resolver_seleccion_guiada()`. Se
    # imprimia el indice dentro del grupo: con una bloqueada antes de una
    # elegible -un plan reordenado, edicion que la ayuda declara inocua- la
    # lista decia "1., 2., 4." y elegir el 4 que se veia dejaba el grupo
    # `omitida`. Medido.
    for (posicion in seq_along(elegibles)) {
      k <- elegibles[[posicion]]
      marca <- if (acciones$recomendada[[k]]) " (Recomendado)" else ""
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0(
        posicion, ". ", acciones$estrategia[[k]], marca, " -- ",
        acciones$justificacion[[k]]
      ))))
    }
    bloqueadas <- which(as.character(acciones$estado) == "bloqueada")
    for (k in bloqueadas) {
      cli::cli_text(.cli_literal(.marcar_para_exhibir(paste0(
        "[bloqueada] ", acciones$estrategia[[k]], " -- ",
        acciones$justificacion[[k]]
      ))))
    }
    explicacion_no_hacer <- if (no_hacer_recomendado) {
      paste0(
        "No hacer nada conserva los datos y es lo recomendado porque el ",
        "hallazgo no justifica por s\u00ed solo una eliminaci\u00f3n."
      )
    } else {
      "No hacer nada conserva los datos y registra la omisi\u00f3n."
    }
    cli::cli_text(.cli_literal(.marcar_para_exhibir(
      paste0(etiqueta_no_hacer, " -- ", explicacion_no_hacer)
    )))

    decision <- list(
      grupo = grupo, acciones = acciones, elegibles = elegibles,
      ejemplos = ejemplos,
      opciones = c(etiquetas, etiqueta_no_hacer)
    )
    respuesta <- if (is.null(selector)) {
      utils::menu(decision$opciones, title = paste("Seleccionar para", grupo))
    } else {
      selector(decision)
    }
    elegida_local <- .resolver_seleccion_guiada(respuesta, acciones, elegibles)
    plan$aplicar[indices] <- FALSE
    if (is.na(elegida_local)) {
      plan$decision_grupo[indices] <- "omitida"
    } else {
      plan$aplicar[indices[[elegida_local]]] <- TRUE
      plan$decision_grupo[indices] <- "elegida"
    }
  }
  plan <- .proteger_fechas_acciones_plan(plan, columnas_protegidas)
  plan
}

#' @export
print.plan_limpieza <- function(x, ...) {
  .validar_objeto_lupa(x, "plan_limpieza", character(), "planificar_limpieza()")
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Plan de limpieza")
  cli::cli_alert_success(.cli_literal(paste0(
    sum(x$aplicar), if (sum(x$aplicar) == 1L) " acci\u00f3n activada" else " acciones activadas"
  )))
  cli::cli_alert_info(.cli_literal(paste0(
    sum(!x$aplicar), if (sum(!x$aplicar) == 1L) " acci\u00f3n desactivada" else " acciones desactivadas"
  )))
  n_destructivas <- sum(x$aplicar & x$destructiva)
  n_eliminatorias <- sum(
    x$aplicar & x$destructiva &
      x$estrategia %in% .estrategias_eliminatorias()
  )
  if (n_destructivas) {
    cli::cli_alert_danger(.cli_literal(paste0(
      n_destructivas,
      if (n_destructivas == 1L) " acci\u00f3n destructiva activa; revisar la p\u00e9rdida declarada" else
        " acciones destructivas activas; revisar la p\u00e9rdida declarada"
    )))
  }
  if (n_eliminatorias) {
    cli::cli_alert_danger(.cli_literal(paste0(
      n_eliminatorias,
      if (n_eliminatorias == 1L) " acci\u00f3n eliminatoria activa; requiere un segundo consentimiento" else
        " acciones eliminatorias activas; requieren un segundo consentimiento"
    )))
  }
  # Lo que no se evaluo se dice aca, no solo en el perfil. Un plan que enumera
  # acciones sin avisar que sobre tal columna no se miro invita a leerlo como si
  # lo demas estuviera bien.
  cobertura <- attr(x, "cobertura_diagnosticos", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    columnas <- unique(as.character(cobertura$columna))
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(cobertura),
      if (nrow(cobertura) == 1L) {
        " diagn\u00f3stico no se evalu\u00f3"
      } else {
        " diagn\u00f3sticos no se evaluaron"
      },
      " y por eso no hay acci\u00f3n para ellos, en: ",
      # Los nombres se rinden ANTES de pegarlos: `paste()` marca `bytes` la
      # frase entera si un nombre lo esta, y la frase del paquete salia
      # `diagn\xc3\xb3sticos`, escapada junto con el nombre.
      paste(.texto_publicable(columnas), collapse = ", "),
      ". El motivo medido de cada uno est\u00e1 en ",
      "`attr(plan, \"cobertura_diagnosticos\")`."
    )))
  }
  sin_accion <- attr(x, "hallazgos_sin_accion_por_columna_ambigua",
                     exact = TRUE)
  if (inherits(sin_accion, "data.frame") && nrow(sin_accion)) {
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(sin_accion),
      if (nrow(sin_accion) == 1L) {
        " hallazgo medido no tiene acci\u00f3n"
      } else {
        " hallazgos medidos no tienen acci\u00f3n"
      },
      " porque su columna comparte nombre con otra, en: ",
      paste(.texto_publicable(unique(as.character(sin_accion$columna))),
            collapse = ", "),
      ". El detalle est\u00e1 en ",
      "`attr(plan, \"hallazgos_sin_accion_por_columna_ambigua\")`."
    )))
  }
  sin_rama <- attr(x, "hallazgos_sin_accion", exact = TRUE)
  if (inherits(sin_rama, "data.frame") && nrow(sin_rama)) {
    cli::cli_alert_warning(.cli_literal(paste0(
      nrow(sin_rama),
      if (nrow(sin_rama) == 1L) {
        " hallazgo medido no tiene acci\u00f3n en el plan"
      } else {
        " hallazgos medidos no tienen acci\u00f3n en el plan"
      },
      ", en: ",
      paste(.texto_publicable(unique(as.character(sin_rama$columna))),
            collapse = ", "),
      ". El motivo de cada uno est\u00e1 en ",
      "`attr(plan, \"hallazgos_sin_accion\")`."
    )))
  }
  vista <- x[c(
    "id_accion", "grupo", "columna", "estrategia", "decision_grupo",
    "n_afectadas", "unidad_conteo", "orden", "estado", "recomendada",
    "destructiva", "aplicar"
  )]
  .print_data_frame_bytes(vista, row.names = FALSE)
  # Lo que esta impresion oculta es justamente `evidencia` y `justificacion`,
  # que son lo que se lee para decidir si aplicar una accion. Un recorte que no
  # se declara se lee como la tabla entera.
  .avisar_campos_no_mostrados(vista, x, "as.data.frame(plan)")
  invisible(original)
}

#' @export
print.resultado_limpieza <- function(x, ...) {
  original <- x
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Resultado de limpieza")
  ejecutadas <- if ("estado" %in% names(x$registro)) {
    sum(x$registro$estado == "ejecutada")
  } else {
    nrow(x$registro)
  }
  fallidas <- if ("estado" %in% names(x$registro)) {
    sum(x$registro$estado == "fallida")
  } else {
    0L
  }
  cli::cli_alert_success(.cli_literal(paste0(
    ejecutadas, if (ejecutadas == 1L) " acci\u00f3n ejecutada" else " acciones ejecutadas"
  )))
  if (fallidas) {
    cli::cli_alert_danger(.cli_literal(paste0(
      fallidas,
      if (fallidas == 1L) " acci\u00f3n fallida; revisar `registro$error`" else
        " acciones fallidas; revisar `registro$error`"
    )))
  }
  # Es una suma de lo que conto cada accion, no de celdas distintas: una celda
  # que tocan dos acciones cuenta dos veces, y una fila o un nombre cuentan uno.
  # Decia "celdas o marcas afectadas", y el usuario lee primero esa cifra.
  cli::cli_text(.cli_literal(paste0(
    sum(x$registro$n_cambiadas),
    if (sum(x$registro$n_cambiadas) == 1L) " cambio contado por las acciones (una celda " else
      " cambios contados por las acciones (una celda ",
    "que tocan dos acciones cuenta dos veces; el detalle est\u00e1 en `registro`)"
  )))
  n_filas <- sum(x$registro$n_filas_eliminadas)
  n_columnas <- sum(x$registro$n_columnas_eliminadas)
  if (n_filas || n_columnas) {
    cli::cli_alert_warning(.cli_literal(paste(
      n_filas, if (n_filas == 1L) " fila y " else " filas y ", n_columnas,
      if (n_columnas == 1L) " columna eliminada" else " columnas eliminadas"
    )))
  }
  invisible(original)
}
