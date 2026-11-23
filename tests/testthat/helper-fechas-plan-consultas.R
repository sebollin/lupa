# Lo que comparten las pruebas del plan de las consultas de fechas
# (`test-fechas-plan-consultas.R`, `test-fechas-plan-consultas-motores.R`,
# `test-local-fechas-plan-consultas-mysql.R`): las tablas, el plan y la corrida
# con los mismos argumentos, y en que no coinciden.

.fpc_fecha <- as.POSIXct("2026-03-01 12:00:00", tz = "UTC")

.fpc_clases <- c(
  resumen = "resumen de fechas (cuentas contra literales)",
  periodos = "periodos (particion contra literales)"
)

# Una tabla donde ninguna consulta de fechas depende de los datos: sin fechas
# centinela, sin fechas futuras, sin texto que parezca fecha.
.fpc_limpia <- function() {
  data.frame(
    lupa_id = 1:8, lupa_aplica = c(1, 1, 0, 1, NA, 1, 0, 1),
    lupa_desde = as.Date("2020-01-01") + (0:7) * 31,
    lupa_hasta = as.Date("2020-06-01") + (0:7) * 29,
    lupa_alta = as.Date("2019-01-01") + (0:7) * 17,
    lupa_nota = letters[1:8], stringsAsFactors = FALSE
  )
}

# La misma con lo que activa las consultas condicionales: una centinela que se
# repite, fechas futuras y un texto que la muestra lee como fecha.
.fpc_sucia <- function() {
  datos <- .fpc_limpia()
  datos$lupa_hasta[c(2, 5, 6)] <- as.Date("9999-12-31")
  datos$lupa_alta[c(3, 4)] <- as.Date("2031-01-01") + c(0, 7)
  datos$lupa_nota <- format(as.Date("2021-01-01") + 0:7)
  datos
}

.fpc_reglas <- list(
  ninguna = NULL,
  algunas = list(lupa_hasta = ~ lupa_aplica == 1, lupa_alta = ~ lupa_aplica == 1),
  todas = list(lupa_desde = ~ lupa_aplica == 1, lupa_hasta = ~ lupa_aplica == 1,
               lupa_alta = ~ lupa_aplica == 1),
  no_logica = list(lupa_hasta = ~ lupa_aplica)
)

.fpc_filas_fechas <- function(plan) {
  filas <- as.data.frame(plan)
  filas <- filas[filas$clase_consulta %in% .fpc_clases,
                 c("clase_consulta", "n_consultas", "n_consultas_max"), drop = FALSE]
  rownames(filas) <- NULL
  filas
}

# El plan y la corrida con los mismos argumentos.
.fpc_par <- function(con, tabla, argumentos) {
  list(
    plan = suppressWarnings(do.call(plan_perfilado_dbi, c(list(con, tabla), argumentos))),
    corrida = suppressWarnings(do.call(
      perfilar_dbi, c(list(con, tabla, fecha = .fpc_fecha), argumentos)
    ))
  )
}

# En que no coinciden el plan y la corrida. `exacto`: nada depende de los
# datos, y lo emitido tiene que ser el total.
.fpc_desacuerdos <- function(par, exacto) {
  plan <- par$plan
  consultas <- par$corrida$resumen_tabla$meta$consultas
  conciliacion <- consultas$conciliacion
  total <- attr(plan, "total")
  c(
    if (!isTRUE(consultas$emitidas >= attr(plan, "total_minimo") &&
                consultas$emitidas <= attr(plan, "total_maximo"))) {
      paste0("emitidas ", consultas$emitidas, " fuera de [",
             attr(plan, "total_minimo"), ", ", attr(plan, "total_maximo"), "]")
    },
    if (!identical(conciliacion$estado, "conciliado")) {
      paste0("conciliacion ", conciliacion$estado)
    },
    if (!isTRUE(all.equal(conciliacion$emitidas, consultas$emitidas))) {
      paste0("la conciliacion suma ", conciliacion$emitidas, " y se emitieron ",
             consultas$emitidas)
    },
    if (!isTRUE(all.equal(conciliacion$anunciadas, total))) {
      paste0("la conciliacion anuncia ", conciliacion$anunciadas,
             " y el plan ", total)
    },
    if (!isTRUE(all.equal(.fpc_filas_fechas(plan),
                          .fpc_filas_fechas(par$corrida$resumen_tabla$meta$plan)))) {
      "meta$plan no trae las filas de fechas del plan"
    },
    if (!isTRUE(all.equal(attr(par$corrida$resumen_tabla$meta$plan, "total"), total))) {
      paste0("el total de meta$plan es ",
             attr(par$corrida$resumen_tabla$meta$plan, "total"), " y el del plan ", total)
    },
    # La conciliacion lleva los totales del plan contra el que concilia, y la
    # copia que guarda de la corrida suma lo mismo.
    if (!is.null(attr(conciliacion, "total")) &&
        !isTRUE(all.equal(attr(conciliacion, "total"), total))) {
      paste0("el total de la conciliacion es ", attr(conciliacion, "total"))
    },
    if (!is.null(attr(conciliacion, "total_maximo")) &&
        !isTRUE(consultas$emitidas <= attr(conciliacion, "total_maximo"))) {
      paste0("el maximo de la conciliacion es ", attr(conciliacion, "total_maximo"),
             " y se emitieron ", consultas$emitidas)
    },
    if (is.numeric(conciliacion$plan_corrida$emitidas) &&
        !isTRUE(all.equal(conciliacion$plan_corrida$emitidas, consultas$emitidas))) {
      paste0("la conciliacion de la corrida suma ", conciliacion$plan_corrida$emitidas)
    },
    if (isTRUE(exacto) && !isTRUE(all.equal(consultas$emitidas, total))) {
      paste0("nada depende de los datos: emitidas ", consultas$emitidas,
             ", total ", total)
    }
  )
}

# Lo seguro y el maximo que el plan anuncia para una clase, y lo que la corrida
# emitio de ella segun su conciliacion.
.fpc_clase <- function(par, clase) {
  fila <- par$plan[par$plan$clase_consulta == .fpc_clases[[clase]], , drop = FALSE]
  detalle <- par$corrida$resumen_tabla$meta$consultas$conciliacion$detalle
  detalle <- detalle[detalle$clase_consulta == .fpc_clases[[clase]], , drop = FALSE]
  list(
    filas = nrow(fila), seguras = fila$n_consultas, maximas = fila$n_consultas_max,
    emitidas = if (nrow(detalle)) detalle$n_emitidas else fila$n_consultas,
    direccion = if (nrow(detalle)) detalle$direccion else NA_character_
  )
}

# Las mismas tablas en un motor de servidor, con CREATE TABLE e INSERT: los
# tipos son los mismos en todos, y MySQL puede tener apagada la carga local que
# usa `dbWriteTable()`.
.fpc_borrar_tabla <- function(con, tabla) {
  suppressWarnings(try(DBI::dbExecute(con, paste("DROP TABLE", tabla)), silent = TRUE))
  invisible(NULL)
}

.fpc_crear_tabla <- function(con, tabla, datos) {
  .fpc_borrar_tabla(con, tabla)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (lupa_id INTEGER, lupa_aplica INTEGER, ",
    "lupa_desde DATE, lupa_hasta DATE, lupa_alta DATE, lupa_nota VARCHAR(30))"
  ))
  literal <- function(v) {
    if (inherits(v, "Date")) v <- format(v)
    ifelse(is.na(v), "NULL", if (is.character(v)) paste0("'", v, "'") else as.character(v))
  }
  filas <- do.call(paste, c(lapply(datos, literal), sep = ", "))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste0("(", filas, ")", collapse = ", ")
  ))
  invisible(tabla)
}

# Una muestra de la grilla para un motor de servidor: sobre la tabla sucia,
# todas las combinaciones de `periodos`, `resumen_fechas`, `bloque_filas` y
# `estrategia_no_sql` sin regla y con reglas sobre algunas fechas; sobre la
# limpia, las de `periodos`, `resumen_fechas` y `estrategia_no_sql`, donde lo
# emitido tiene que ser el total. Devuelve cuantas recorrio y en cuantas los
# datos activaron una consulta condicional. `fechas_como_texto`: el controlador
# entrega una columna `DATE` como texto (RMySQL), y entonces que sea una fecha
# lo dice la muestra: con `resumen_fechas` y `"bloques"` su lectura es
# condicional tambien en la tabla limpia.
.fpc_muestra_grilla <- function(con, motor, prefijo, fechas_como_texto = FALSE) {
  sucia <- .fpc_crear_tabla(con, paste0(prefijo, "_sucia"), .fpc_sucia())
  limpia <- .fpc_crear_tabla(con, paste0(prefijo, "_limpia"), .fpc_limpia())
  recorridas <- 0L
  condicionales <- 0L
  comprobar <- function(tabla, hay_periodo, hay_resumen, regla, bloque, estrategia) {
    argumentos <- list(metricas = "basicos", estrategia_no_sql = estrategia)
    if (hay_periodo) argumentos$periodos <- c(inicio = "lupa_desde", fin = "lupa_hasta")
    if (hay_resumen) argumentos$resumen_fechas <- TRUE
    if (!is.null(.fpc_reglas[[regla]])) argumentos$aplicabilidad <- .fpc_reglas[[regla]]
    if (!is.null(bloque)) argumentos$bloque_filas <- bloque
    par <- .fpc_par(con, tabla, argumentos)
    exacto <- identical(tabla, limpia) && !(
      isTRUE(fechas_como_texto) && hay_resumen && identical(estrategia, "bloques")
    )
    expect_identical(
      .fpc_desacuerdos(par, exacto = exacto), NULL,
      label = paste0(
        motor, ", ", tabla, ", periodos ", hay_periodo, ", resumen ", hay_resumen,
        ", regla ", regla, ", bloque_filas ", if (is.null(bloque)) "sin" else bloque,
        ", ", estrategia
      )
    )
    recorridas <<- recorridas + 1L
    if (par$corrida$resumen_tabla$meta$consultas$emitidas > attr(par$plan, "total")) {
      condicionales <<- condicionales + 1L
    }
  }
  for (hay_periodo in c(FALSE, TRUE)) for (hay_resumen in c(FALSE, TRUE)) {
    for (estrategia in c("no_medible", "bloques")) {
      for (regla in c("ninguna", "algunas")) for (bloque in list(NULL, 2L)) {
        comprobar(sucia, hay_periodo, hay_resumen, regla, bloque, estrategia)
      }
      comprobar(limpia, hay_periodo, hay_resumen, "ninguna", NULL, estrategia)
    }
  }
  .fpc_borrar_tabla(con, sucia)
  .fpc_borrar_tabla(con, limpia)
  list(recorridas = recorridas, condicionales = condicionales)
}
