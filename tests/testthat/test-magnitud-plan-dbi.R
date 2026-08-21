skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# Un plan que solo cuenta consultas no responde la pregunta que trae el usuario:
# si la corrida tarda segundos, minutos u horas. Catorce consultas sobre dos
# millones de filas son mucho mas trabajo que doscientas sobre mil.

.plan_de <- function(alcance, n_consultas = 1) {
  data.frame(
    clase_consulta = paste0("clase ", seq_along(alcance)),
    n_consultas = as.numeric(n_consultas),
    alcance = alcance,
    stringsAsFactors = FALSE
  )
}

test_that("el peso de cada clase sale de su alcance y no de su nombre", {
  filas <- 1e6
  escanea <- .trabajo_plan_dbi(.plan_de("escanea la tabla completa", 3), filas, 0)
  expect_equal(escanea$filas_leidas, 3 * filas)
  expect_equal(escanea$ordenaciones, 0)

  # Escanear dos veces cuesta el doble: la sonda del desvio lo hace.
  doble <- .trabajo_plan_dbi(
    .plan_de("escanea la tabla completa dos veces", 3), filas, 0
  )
  expect_equal(doble$filas_leidas, 6 * filas)

  # Una muestra no toca la tabla entera, y esa es justamente la palanca.
  muestreado <- .trabajo_plan_dbi(
    .plan_de("lee una muestra del motor", 40), filas, muestra = 1000
  )
  expect_equal(muestreado$filas_leidas, 40 * 1000)
  expect_lt(muestreado$filas_leidas, escanea$filas_leidas)

  ordena <- .trabajo_plan_dbi(.plan_de("ordena la tabla completa", 5), filas, 0)
  expect_equal(ordena$ordenaciones, 5)
  # Las lecturas siguen siendo lecturas: el sobrecosto de ordenar va aparte,
  # para que los dos numeros publicados no dependan del supuesto.
  expect_equal(ordena$filas_leidas, 5 * filas)
  expect_gt(ordena$equivalente, ordena$filas_leidas)
})

test_that("la magnitud cambia en el umbral y no cerca del umbral", {
  # Un plan de una sola consulta que escanea: el trabajo equivalente es
  # exactamente el numero de filas, asi que el borde se puede pisar justo.
  magnitud_con <- function(filas) {
    .trabajo_plan_dbi(.plan_de("escanea la tabla completa", 1), filas, 0)$magnitud
  }
  expect_equal(magnitud_con(.UMBRAL_TRABAJO_MEDIO_DBI - 1), "baja")
  expect_equal(magnitud_con(.UMBRAL_TRABAJO_MEDIO_DBI), "media")
  expect_equal(magnitud_con(.UMBRAL_TRABAJO_ALTO_DBI - 1), "media")
  expect_equal(magnitud_con(.UMBRAL_TRABAJO_ALTO_DBI), "alta")
})

test_that("sin conteo de filas la magnitud es desconocida y no cero", {
  # `0` seria "no cuesta nada", que es justo lo contrario de lo que se sabe.
  for (sin_dato in list(NA, NA_real_, NULL)) {
    trabajo <- .trabajo_plan_dbi(
      .plan_de("escanea la tabla completa", 1), sin_dato, 0
    )
    expect_equal(trabajo$magnitud, "desconocida")
    expect_true(is.na(trabajo$filas_leidas))
  }
})

test_that("un conteo de filas que no es un conteo no produce una estimacion", {
  # Cada uno de estos rompia algo antes de tener guarda: `Inf` multiplicado por
  # cero ordenaciones da NaN y reventaba el `if` de la magnitud; un conteo
  # negativo daba "baja" con lecturas negativas; y un conteo no numerico
  # emitia un aviso de coercion antes de rendirse.
  imposibles <- list(-5, Inf, -Inf, NaN, NA, NA_real_, c(10, 20), "mil",
                     character(), numeric())
  for (valor in imposibles) {
    trabajo <- expect_no_warning(
      .trabajo_plan_dbi(.plan_de("escanea la tabla completa", 1), valor, 1000)
    )
    expect_equal(trabajo$magnitud, "desconocida")
    expect_true(is.na(trabajo$filas_leidas))
  }
  # Cero filas si es un conteo: la tabla esta vacia y no cuesta nada leerla.
  vacia <- .trabajo_plan_dbi(.plan_de("escanea la tabla completa", 1), 0, 1000)
  expect_equal(vacia$magnitud, "baja")
  expect_equal(vacia$filas_leidas, 0)
})

test_that("un conteo `integer64` se estima igual, que es donde mas importa", {
  skip_if_not_installed("bit64")
  # `n_total` llega como `integer64` justamente sobre las tablas grandes, y
  # `is.numeric()` da FALSE para esa clase: exigirlo habria dejado sin
  # estimacion el unico caso donde la estimacion hace falta.
  trabajo <- .trabajo_plan_dbi(
    .plan_de("escanea la tabla completa", 1), bit64::as.integer64(2e9), 1000
  )
  expect_equal(trabajo$magnitud, "alta")
  expect_equal(trabajo$filas_leidas, 2e9)
})

test_that("un alcance sin peso declarado no se estima en silencio", {
  # Si manana alguien agrega una clase de consulta y olvida pesarla, el plan
  # tiene que decir que no sabe, no inventar un numero bajo.
  trabajo <- .trabajo_plan_dbi(.plan_de("hace algo nuevo", 1), 1e6, 0)
  expect_equal(trabajo$magnitud, "desconocida")
  expect_true(is.na(trabajo$filas_leidas))
})

test_that("el plan real trae la magnitud y sigue siendo un data.frame", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:50, valor = as.numeric(1:50)))
  plan <- plan_perfilado_dbi(con, "t")

  expect_s3_class(plan, "data.frame")
  expect_s3_class(plan, "plan_perfilado_dbi")
  expect_equal(attr(plan, "magnitud"), "baja")
  expect_true(is.finite(attr(plan, "filas_leidas")))
  expect_match(attr(plan, "supuesto_costo"), "estimaci")
})

test_that("la impresion avisa cuando el trabajo es alto y nombra las palancas", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:50, valor = as.numeric(1:50)))
  plan <- plan_perfilado_dbi(con, "t")

  # cli escribe por el flujo de mensajes, no por la salida estandar.
  bajo <- capture.output(print(plan), type = "message")
  expect_true(any(grepl("bajo", bajo)))
  expect_false(any(grepl("muestreado", bajo)))
  # Sobre una tabla chica los dos parrafos de supuestos son ruido: tapan la
  # respuesta en vez de matizarla. Pero la palabra "techo" viaja igual con el
  # conteo, y los supuestos siguen accesibles.
  expect_false(any(grepl("ning\u00fan \u00edndice", bajo)))
  expect_true(any(grepl("techo", bajo)))
  expect_true(any(grepl("supuesto_costo", bajo)))

  attr(plan, "magnitud") <- "alta"
  attr(plan, "filas_leidas") <- 5e9
  alto <- capture.output(print(plan), type = "message")
  expect_true(any(grepl("alto", alto)))
  # Avisar que es grande sin decir que hacer no le sirve a nadie.
  expect_true(any(grepl("muestreado", alto)))
  expect_true(any(grepl("max_consultas", alto)))
  # Y el supuesto viaja con el numero, siempre.
  expect_true(any(grepl("estimaci", alto)))
})

test_that("declarar la magnitud no cambia el conteo de consultas", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:50, valor = as.numeric(1:50)))
  plan <- plan_perfilado_dbi(con, "t")
  expect_equal(attr(plan, "total"), sum(plan$n_consultas))
  expect_true(all(c("clase_consulta", "n_consultas", "alcance") %in% names(plan)))
})
