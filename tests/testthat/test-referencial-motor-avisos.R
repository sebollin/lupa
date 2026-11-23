# El aviso de las metricas que no se midieron dice por que: "porque su metodo
# fallo" solo de las que fallaron. La guarda del pliegue de un referencial del
# motor no es un metodo que falla, y su motivo no traia punto final: el aviso
# pegaba "segun el perfil) El detalle". Y `medir()` con un referencial del motor
# repetia la misma frase una vez por metrica, y avisaba una vez por cada una
# ademas del aviso de todas.

.avisos_ref_capturar <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

.avisos_ref_contar <- function(texto, patron) {
  posiciones <- gregexpr(patron, texto, fixed = TRUE, useBytes = TRUE)[[1L]]
  sum(posiciones > 0L)
}

.avisos_ref_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "sucursales", data.frame(
    id_sucursal = 1:2, zona = c("Norte", "Sur")
  ))
  # Un blanco al final de "Sur ": la guarda del pliegue aparta la consulta.
  DBI::dbWriteTable(con, "ventas", data.frame(
    id_sucursal = c(1, 2, 2), zona = c("Norte", "Sur ", "Sur")
  ))
  con
}

.avisos_ref_modelo <- function() {
  ref <- metricas_referencial()
  padron <- referencial(DBI::Id(table = "sucursales"), clave = "id_sucursal",
                        valor = "zona", completo = TRUE, alcance = "ejemplo")
  modelo(
    instanciar(especializar(ref$CorrectitudSemFuerte, "F"), "ventas",
               "id_sucursal", referencial = padron),
    instanciar(especializar(ref$CorrectitudSemDebil, "D"), "ventas",
               c("id_sucursal", "zona"), referencial = padron),
    instanciar(especializar(ref$RatioCobertura, "C"), "ventas", "id_sucursal",
               referencial = padron)
  )
}

.avisos_ref_metodo <- "porque su m\u00e9todo fall\u00f3"
.avisos_ref_detalle <- "El detalle est\u00e1 en `attr(medicion, \"cobertura_metricas\")`."

test_that("la guarda del pliegue no se avisa como un metodo que fallo", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .avisos_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  r <- .avisos_ref_capturar(medir_dbi(.avisos_ref_modelo(), con))
  expect_length(r$avisos, 1L)
  aviso <- r$avisos[[1L]]
  expect_false(grepl(.avisos_ref_metodo, aviso, fixed = TRUE, useBytes = TRUE))
  expect_true(startsWith(aviso, "1 m\u00e9trica no se midi\u00f3; la corrida"))
  expect_true(grepl(paste0("seg\u00fan el perfil). ", .avisos_ref_detalle),
                    aviso, fixed = TRUE, useBytes = TRUE))
  cobertura <- attr(r$valor, "cobertura_metricas", exact = TRUE)
  expect_true(endsWith(cobertura$motivo[cobertura$estado == "no_medible"],
                       "seg\u00fan el perfil)."))
})

test_that("medir() con un referencial del motor avisa una vez, con la frase una vez", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .avisos_ref_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ventas <- DBI::dbReadTable(con, "ventas")
  r <- .avisos_ref_capturar(
    medir(.avisos_ref_modelo(), list(ventas = ventas), detalle = "resumen")
  )
  expect_length(r$avisos, 1L)
  aviso <- r$avisos[[1L]]
  expect_true(startsWith(aviso, "3 m\u00e9tricas no se midieron. El referencial"))
  expect_identical(.avisos_ref_contar(aviso, "vive en la tabla"), 1L)
  expect_false(grepl(.avisos_ref_metodo, aviso, fixed = TRUE, useBytes = TRUE))
  # La cobertura conserva una fila por metrica.
  cobertura <- attr(r$valor, "cobertura_metricas", exact = TRUE)
  expect_identical(sum(cobertura$estado == "no_medible"), 3L)
  # Lo mismo con el detalle completo.
  r <- .avisos_ref_capturar(medir(.avisos_ref_modelo(), list(ventas = ventas)))
  expect_length(r$avisos, 1L)
  expect_identical(.avisos_ref_contar(r$avisos[[1L]], "vive en la tabla"), 1L)
})

test_that("con una falla y un limite, el aviso cuenta las que fallaron", {
  skip_on_cran()
  nucleo <- metricas_nucleo()
  ref <- metricas_referencial()
  padron <- referencial(DBI::Id(table = "sucursales"), clave = "id_sucursal",
                        valor = "zona", completo = TRUE, alcance = "ejemplo")
  mixto <- modelo(
    instanciar(especializar(nucleo$ErrorEstandar), "ventas", "monto"),
    instanciar(especializar(ref$CorrectitudSemFuerte, "F"), "ventas",
               "id_sucursal", referencial = padron),
    instanciar(especializar(nucleo$NoNulo), "ventas", "monto")
  )
  ventas <- data.frame(id_sucursal = c(1, 2), monto = c(5, NA))
  r <- .avisos_ref_capturar(medir(mixto, list(ventas = ventas)))
  expect_length(r$avisos, 1L)
  expect_true(startsWith(
    r$avisos[[1L]],
    paste0("2 m\u00e9tricas no se midieron (1 ", .avisos_ref_metodo,
           "); la corrida sigui\u00f3 con las dem\u00e1s del modelo. ")
  ))
})

test_that("el control: una falla sola sigue diciendo que su metodo fallo", {
  skip_on_cran()
  nucleo <- metricas_nucleo()
  r <- .avisos_ref_capturar(medir(
    modelo(instanciar(especializar(nucleo$ErrorEstandar), "t", "edad")),
    list(t = data.frame(edad = c(5, NA)))
  ))
  expect_length(r$avisos, 1L)
  expect_true(startsWith(
    r$avisos[[1L]], paste0("1 m\u00e9trica no se midi\u00f3 ", .avisos_ref_metodo, ". ")
  ))
})

test_that("un motivo sin punto final lo recibe antes de la frase siguiente", {
  skip_on_cran()
  propia <- metrica("Propia", "Una metrica que siempre falla.", "atributo", "real")
  falla <- instanciar(especializar(propia), "t", "edad",
                      metodo = function(tablas, instancia) stop("sin punto"))
  r <- .avisos_ref_capturar(
    medir(modelo(falla), list(t = data.frame(edad = c(1, 2))))
  )
  expect_length(r$avisos, 1L)
  expect_true(grepl(paste0("sin punto. ", .avisos_ref_detalle), r$avisos[[1L]],
                    fixed = TRUE, useBytes = TRUE))
  # Un motivo que ya trae su punto no recibe otro.
  falla <- instanciar(especializar(propia), "t", "edad",
                      metodo = function(tablas, instancia) stop("con punto."))
  r <- .avisos_ref_capturar(
    medir(modelo(falla), list(t = data.frame(edad = c(1, 2))))
  )
  expect_true(grepl(paste0("con punto. ", .avisos_ref_detalle), r$avisos[[1L]],
                    fixed = TRUE, useBytes = TRUE))
})

test_that("medir por metrica avisa una vez por todas, no una por cada parte", {
  skip_on_cran()
  nucleo <- metricas_nucleo()
  r <- .avisos_ref_capturar(.medir_y_agregar_por_metrica(
    modelo(instanciar(especializar(nucleo$ErrorEstandar), "t", "edad"),
           instanciar(especializar(nucleo$NoNulo), "t", "edad")),
    list(t = data.frame(edad = c(5, NA, NA))), "c", as.POSIXct("2026-01-15")
  ))
  expect_length(r$avisos, 1L)
  expect_true(grepl(.avisos_ref_metodo, r$avisos[[1L]], fixed = TRUE,
                    useBytes = TRUE))
})
