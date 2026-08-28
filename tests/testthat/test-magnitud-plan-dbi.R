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

test_that("el plan real conserva la magnitud desconocida sin escanear", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:50, valor = as.numeric(1:50)))
  plan <- plan_perfilado_dbi(con, "t")

  expect_s3_class(plan, "data.frame")
  expect_s3_class(plan, "plan_perfilado_dbi")
  expect_equal(attr(plan, "magnitud"), "desconocida")
  expect_true(is.na(attr(plan, "filas_leidas")))
  expect_match(attr(plan, "supuesto_costo"), "estimaci")
})

test_that("la impresion declara la incertidumbre del trabajo", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:50, valor = as.numeric(1:50)))
  plan <- plan_perfilado_dbi(con, "t")

  # cli escribe por el flujo de mensajes, no por la salida estandar.
  bajo <- capture.output(print(plan), type = "message")
  expect_true(any(grepl("desconocida|No se pudo estimar", bajo)))
  expect_true(any(grepl("No se pudo estimar", bajo)))
  expect_true(any(grepl("no escanea datos", bajo)))
  # El rango y los supuestos siguen accesibles aunque falte el total.
  expect_false(any(grepl("techo", bajo)))
  expect_true(any(grepl("entre [0-9]", bajo)))
  expect_true(any(grepl("El trabajo es una estimación", bajo)))

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

# ---- La mitad que se hacia en R y no se contaba --------------------------

.plan_con_muestra <- function(alcance_muestra = "lee las filas pedidas") {
  data.frame(
    clase_consulta = c("conteos", "muestra"),
    n_consultas = c(1, 1),
    alcance = c("escanea la tabla completa", alcance_muestra),
    stringsAsFactors = FALSE
  )
}

test_that("el plan cuenta el trabajo por valor, no solo el del motor", {
  plan <- .plan_con_muestra()
  # El caso que lo destapo: 3.912 filas, una columna de geometria en texto.
  # El motor pone poquisimo y el reloj marca 35 segundos.
  sin_texto <- .trabajo_plan_dbi(plan, 3912, 3912, columnas_texto = 0)
  expect_equal(sin_texto$magnitud_motor, "baja")
  expect_equal(sin_texto$pares_texto, 0)
  expect_equal(sin_texto$magnitud, "baja")

  con_texto <- .trabajo_plan_dbi(plan, 3912, 3912, columnas_texto = 1)
  # El motor sigue poniendo lo mismo: lo que cambia es lo que ahora se cuenta.
  expect_equal(con_texto$magnitud_motor, "baja")
  expect_equal(con_texto$filas_leidas, sin_texto$filas_leidas)
  expect_gt(con_texto$pares_texto, 0)
  expect_false(identical(con_texto$magnitud, "baja"))
  # Y el titular es la mayor de las dos mitades, no la del motor.
  expect_equal(
    con_texto$magnitud,
    .mayor_magnitud_dbi(con_texto$magnitud_motor, con_texto$magnitud_texto)
  )
})

test_that("los pares se acotan por la muestra y por el tope del detector", {
  plan <- .plan_con_muestra()
  # Con una muestra chica el techo lo pone la muestra: m*(m-1)/2.
  chica <- .trabajo_plan_dbi(plan, 1e6, 100, columnas_texto = 1)
  expect_equal(chica$pares_texto, 100 * 99 / 2)
  expect_equal(chica$magnitud_texto, "baja")

  # Con una muestra grande el techo lo pone `max_pares`, que se lee de la firma
  # del detector para que no se pueda ir por su lado.
  grande <- .trabajo_plan_dbi(plan, 1e9, 1e6, columnas_texto = 1)
  expect_equal(grande$pares_texto, .max_pares_vocabulario_dbi())

  # Y escala con las columnas de texto, que es lo que multiplica el trabajo.
  diez <- .trabajo_plan_dbi(plan, 1e9, 1e6, columnas_texto = 10)
  expect_equal(diez$pares_texto, 10 * .max_pares_vocabulario_dbi())
  expect_equal(diez$columnas_texto, 10)
})

test_that("sin muestra no hay trabajo por valor que contar", {
  # En `modo = "conteos"` no se trae ninguna fila a R: el detector de
  # vocabulario no corre, y contar pares ahi seria inventar trabajo.
  plan <- data.frame(
    clase_consulta = "conteos", n_consultas = 1,
    alcance = "escanea la tabla completa", stringsAsFactors = FALSE
  )
  sin_muestra <- .trabajo_plan_dbi(plan, 1e9, 1e6, columnas_texto = 40)
  expect_equal(sin_muestra$pares_texto, 0)
  expect_equal(sin_muestra$magnitud_texto, "baja")
  expect_equal(sin_muestra$magnitud, sin_muestra$magnitud_motor)
})

test_that("la magnitud combinada nunca baja la del motor", {
  expect_equal(.mayor_magnitud_dbi("baja", "alta"), "alta")
  expect_equal(.mayor_magnitud_dbi("alta", "baja"), "alta")
  expect_equal(.mayor_magnitud_dbi("media", "media"), "media")
  # Un valor fuera del vocabulario cerrado no se cuela como magnitud valida.
  expect_equal(.mayor_magnitud_dbi("baja", "enorme"), "desconocida")
  expect_equal(.mayor_magnitud_dbi("desconocida", "alta"), "desconocida")
})

test_that("el plan real declara las dos mitades y la impresion las muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = 1:300, texto = paste0("valor ", 1:300), stringsAsFactors = FALSE
  ))
  plan <- plan_perfilado_dbi(con, "t", muestra = 300)
  expect_equal(attr(plan, "columnas_texto", exact = TRUE), 1)
  expect_equal(attr(plan, "pares_texto", exact = TRUE), 300 * 299 / 2)
  expect_equal(attr(plan, "magnitud_motor", exact = TRUE), "desconocida")
  expect_equal(attr(plan, "magnitud", exact = TRUE), "desconocida")
  expect_true(attr(plan, "magnitud_texto", exact = TRUE) %in% .ORDEN_MAGNITUD_DBI)
  salida <- paste(
    capture.output(print(plan), type = "message"), collapse = " "
  )
  expect_match(salida, "columna de texto")
  expect_match(salida, "pares de formas")
})

test_that("una magnitud desconocida no inventa palancas", {
  skip_if_not_installed("RSQLite")
  # Salio de una corrida contra motores reales: una tabla de millones de filas
  # tardaba minutos con las opciones por omision y su plan la clasificaba
  # **media**, donde el aviso avisaba pero no nombraba `modo = 'muestreado'`.
  # Un plan que dice "va a costar" sin decir "y asi se acota" deja la decision a
  # medias justo donde importa.
  palancas_de <- function(datos) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "t", datos)
    plan <- plan_perfilado_dbi(con, "t")
    salida <- c(
      capture.output(print(plan)),
      capture.output(print(plan), type = "message")
    )
    list(
      magnitud = attr(plan, "magnitud", exact = TRUE),
      nombra = any(grepl("modo = .muestreado", salida))
    )
  }

  set.seed(4)
  chica <- palancas_de(data.frame(
    id = 1:500, v = letters[sample(26L, 500L, TRUE)], stringsAsFactors = FALSE
  ))
  expect_equal(chica$magnitud, "desconocida")
  expect_false(chica$nombra)

  vocabulario <- replicate(
    500L, paste(sample(LETTERS, 60L, TRUE), collapse = "")
  )
  grande <- palancas_de(data.frame(
    id = 1:400000L,
    monto = round(rlnorm(400000L, 9, 1), 2),
    t1 = vocabulario[sample(500L, 400000L, TRUE)],
    t2 = vocabulario[sample(500L, 400000L, TRUE)],
    stringsAsFactors = FALSE
  ))
  expect_equal(grande$magnitud, "desconocida")
  expect_false(grande$nombra)
})
