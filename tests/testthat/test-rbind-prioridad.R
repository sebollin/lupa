# `rbind(..., prioridad = )`: junta partes de una corrida solo si son
# comparables -la misma corrida, la misma definicion y la misma instantanea de
# los datos- y conserva lo que no eligio.

.rp_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.rp_datos <- function() {
  data.frame(n = c(1, 2, NA, 4, 2, 100, 0, NA, 5, 6),
             t = c("x", "y", NA, "x", "z", NA, "x", "y", "x", "x"),
             stringsAsFactors = FALSE)
}

.rp_partes <- function(d = .rp_datos(), d2 = d, id = "e", fecha2 = .rp_fecha) {
  nuc <- metricas_nucleo()
  a <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
                    instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y")), "d", "t")),
             list(d = d), id_medicion = id, fecha = .rp_fecha, detalle = "resumen")
  b <- medir(modelo(instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y")), "d", "t"),
                    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t")),
             list(d = d2), id_medicion = id, fecha = fecha2, detalle = "resumen")
  list(a = a, b = b)
}

.rp_espec_contador <- function() {
  list(
    motor = "sqlite", mecanismo = "prueba", requiere_tabla_base = FALSE,
    requiere_filas_no_vacias = FALSE, solo_contador_cierre = FALSE,
    campos = list(list(clave = "suma", etiqueta_dif = "suma", alias = "lupa_est_suma",
                       sql = "CAST(SUM(n) AS TEXT)", obligatorio = TRUE))
  )
}

test_that("sin prioridad la misma celda en dos partes se sigue rechazando", {
  skip_on_cran()
  p <- .rp_partes()
  expect_error(rbind(p$a, p$b), "aparece dos veces", class = "lupa_resumen_no_admitido")
})

test_that("con prioridad junta partes de la misma tabla y conserva lo descartado", {
  skip_on_cran()
  p <- .rp_partes()
  k <- rbind(primera = p$a, segunda = p$b, prioridad = c("segunda", "primera"))
  expect_s3_class(k, "medicion_resumen")
  expect_identical(sort(k$metrica_instanciada), c("Dic@d.t", "Dup@d.t", "NN@d.n"))
  expect_identical(k$fuente[k$metrica_instanciada == "Dic@d.t"], "segunda")
  expect_identical(k$fuente[k$metrica_instanciada == "NN@d.n"], "primera")
  d <- attr(k, "descartadas")
  expect_identical(d$metrica_instanciada, "Dic@d.t")
  expect_identical(d$fuente, "primera")
  expect_identical(d$fuente_elegida, "segunda")
  expect_true(d$coinciden)
  expect_identical(attr(k, "consolidacion")$prioridad, c("segunda", "primera"))
  expect_true(all(k$estado_cuentas == "cierran"))
  # El tablero, el indice y el semaforo publican la fuente y la consolidacion.
  tb <- tablero_calidad(k)
  expect_identical(tb$fuente, k$fuente)
  expect_false(is.null(attr(tb, "consolidacion")))
  expect_false(is.null(attr(tb, "descartadas")))
  ic <- indice_calidad(k, pesos = c(Completitud = 0.4, Exactitud = 0.3, Unicidad = 0.3))
  expect_false(is.null(attr(ic$tablero, "consolidacion")))
  s <- semaforo_calidad(k, cortes = list(Completitud = c(0.5, 0.9)))
  expect_identical(s$fuente, k$fuente)
  # Con `do.call()`, desde una lista.
  k2 <- do.call(rbind, c(list(primera = p$a, segunda = p$b),
                         list(prioridad = c("segunda", "primera"))))
  expect_identical(k2, k)
})

test_that("el orden de las partes no cambia nada: decide la prioridad", {
  skip_on_cran()
  p <- .rp_partes()
  k1 <- rbind(primera = p$a, segunda = p$b, prioridad = c("primera", "segunda"))
  k2 <- rbind(segunda = p$b, primera = p$a, prioridad = c("primera", "segunda"))
  expect_identical(k1, k2)
})

test_that("otra tabla con las mismas filas no se junta: el conteo no prueba la instantanea", {
  skip_on_cran()
  d2 <- .rp_datos()
  d2$t[[1L]] <- "q"
  p <- .rp_partes(d2 = d2)
  expect_error(rbind(a = p$a, b = p$b, prioridad = c("a", "b")),
               "otra instant", class = "lupa_consolidacion_no_admitida")
})

test_that("el motor sin contador y la memoria no se juntan, y se dice por que", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- .rp_datos()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  md <- medir_dbi(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n")), con,
                  id_medicion = "e", fecha = .rp_fecha)
  p <- .rp_partes()
  expect_error(rbind(motor = md, memoria = p$a, prioridad = c("motor", "memoria")),
               "no hay c\u00f3mo probar", class = "lupa_consolidacion_no_admitida")
  md2 <- medir_dbi(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n")), con,
                   id_medicion = "e", fecha = .rp_fecha)
  expect_error(rbind(a = md, b = md2, prioridad = c("a", "b")),
               "no hay c\u00f3mo probar", class = "lupa_consolidacion_no_admitida")
})

test_that("dos partes del motor con el mismo contador se juntan; lo que una no tradujo lo mide la otra", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- .rp_datos()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  local_mocked_bindings(.especificacion_contador_estabilidad_dbi = function(...) {
    .rp_espec_contador()
  })
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
                instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^[a-z]$"), "d", "t"))
  sin_bloques <- suppressWarnings(medir_dbi(mod, con, id_medicion = "e", fecha = .rp_fecha))
  con_bloques <- suppressWarnings(medir_dbi(mod, con, id_medicion = "e", fecha = .rp_fecha,
                                            estrategia_no_sql = "bloques"))
  expect_identical(attr(sin_bloques, "cobertura_metricas")$metrica_instanciada, "Rx@d.t")
  # Con el contador, cada lectura lleva su instantanea y el control lo dice.
  lecturas <- attr(sin_bloques, "lecturas", exact = TRUE)
  expect_identical(unique(lecturas$control), "contador_del_motor")
  expect_false(anyNA(lecturas$instantanea))
  k <- rbind(sql = sin_bloques, bloques = con_bloques, prioridad = c("sql", "bloques"))
  expect_identical(k$fuente[k$metrica_instanciada == "Rx@d.t"], "bloques")
  expect_identical(attr(k, "cobertura_resuelta")$metrica_instanciada, "Rx@d.t")
  expect_null(attr(k, "cobertura_metricas"))
  # Una falla del metodo no es una limitacion del instrumento: no se completa con
  # la cifra de la otra parte.
  vias <- attr(sin_bloques, "ejecucion_dbi")
  vias$vias[["Rx@d.t"]] <- "no_medible_metodo"
  attr(sin_bloques, "ejecucion_dbi") <- vias
  expect_error(rbind(sql = sin_bloques, bloques = con_bloques, prioridad = c("sql", "bloques")),
               "se contradicen", class = "lupa_consolidacion_no_admitida")
  # Con la tabla cambiada entre las dos, el contador no coincide.
  DBI::dbExecute(con, "UPDATE d SET n = 50 WHERE n = 100")
  otra <- suppressWarnings(medir_dbi(mod, con, id_medicion = "e", fecha = .rp_fecha,
                                     estrategia_no_sql = "bloques"))
  vias$vias[["Rx@d.t"]] <- "no_medible"
  attr(sin_bloques, "ejecucion_dbi") <- vias
  expect_error(rbind(sql = sin_bloques, bloques = otra, prioridad = c("sql", "bloques")),
               "otra instant", class = "lupa_consolidacion_no_admitida")
})

test_that("sin sello completo, con otra definicion, otra fecha u otro id no se junta", {
  skip_on_cran()
  p <- .rp_partes()
  sin_sello <- p$b
  attr(sin_sello, "sello_metricas") <- NULL
  expect_error(rbind(a = p$a, b = sin_sello, prioridad = c("a", "b")),
               "sello", class = "lupa_consolidacion_no_admitida")
  propia <- metrica(
    "Propia", "Metodo propio.", "instanciaAtributo", "booleano",
    dimension = "Exactitud", factor = "Exactitud sintactica",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad[[1L]]]][[instancia$atributos[[1L]]]]
      f <- which(!is.na(x))
      data.frame(resultado = rep(TRUE, length(f)), entidad = instancia$entidad[[1L]],
                 atributo = instancia$atributos[[1L]], fila = f,
                 objeto = paste0("d$t[", f, "]"), stringsAsFactors = FALSE)
    }
  )
  parcial <- medir(modelo(instanciar(especializar(propia, "P"), "d", "t")),
                   list(d = .rp_datos()), id_medicion = "e", fecha = .rp_fecha, detalle = "resumen")
  expect_identical(attr(parcial, "sello_metricas")$certificacion, "parcial")
  expect_error(rbind(a = p$a, b = parcial, prioridad = c("a", "b")),
               "certificada", class = "lupa_consolidacion_no_admitida")
  nuc <- metricas_nucleo()
  otra_def <- medir(modelo(instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "z")),
                                      "d", "t")),
                    list(d = .rp_datos()), id_medicion = "e", fecha = .rp_fecha, detalle = "resumen")
  expect_error(rbind(a = p$a, b = otra_def, prioridad = c("a", "b")),
               "misma definici", class = "lupa_consolidacion_no_admitida")
  otra_fecha <- .rp_partes(fecha2 = .rp_fecha + 60)
  expect_error(rbind(a = otra_fecha$a, b = otra_fecha$b, prioridad = c("a", "b")),
               "fecha", class = "lupa_consolidacion_no_admitida")
  otro_id <- medir(modelo(instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t")),
                   list(d = .rp_datos()), id_medicion = "otra", fecha = .rp_fecha, detalle = "resumen")
  expect_error(rbind(a = p$a, b = otro_id, prioridad = c("a", "b")),
               "una corrida", class = "lupa_consolidacion_no_admitida")
  sin_declarar <- medir(modelo(instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t")),
                        list(d = .rp_datos()), id_medicion = "e", detalle = "resumen")
  expect_error(rbind(a = p$a, b = sin_declarar, prioridad = c("a", "b")),
               class = "lupa_resumen_no_admitido")
})

test_that("la prioridad tiene que nombrar todas las partes", {
  skip_on_cran()
  p <- .rp_partes()
  expect_error(rbind(a = p$a, b = p$b, prioridad = "a"), "todas las partes",
               class = "lupa_consolidacion_no_admitida")
  expect_error(rbind(a = p$a, b = p$b, prioridad = c("a", "a")), "todas las partes",
               class = "lupa_consolidacion_no_admitida")
  expect_error(rbind(p$a, p$b, prioridad = c("a", "b")), "nombre propio",
               class = "lupa_consolidacion_no_admitida")
})

test_that("una consolidacion no se vuelve a unir, y `[` filtra lo descartado por celda", {
  skip_on_cran()
  p <- .rp_partes()
  k <- rbind(a = p$a, b = p$b, prioridad = c("a", "b"))
  expect_error(rbind(k, p$a), "consolidaci", class = "lupa_resumen_no_admitido")
  expect_identical(nrow(attr(k, "descartadas")), 1L)
  sin_dic <- k[k$metrica_instanciada != "Dic@d.t", ]
  expect_identical(nrow(attr(sin_dic, "descartadas")), 0L)
  con_dic <- k[k$metrica_instanciada == "Dic@d.t", ]
  expect_identical(attr(con_dic, "descartadas")$metrica_instanciada, "Dic@d.t")
})
