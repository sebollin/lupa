test_that("el spool verifica trailer, checksum y orden de sus chunks", {
  skip_if_not_installed("DBI")
  chunks <- list(
    data.frame(id = 1:2, texto = c("a", "b"), stringsAsFactors = FALSE),
    data.frame(id = 3:4, texto = c("c", "d"), stringsAsFactors = FALSE)
  )
  spool <- .escribir_spool_muestra_dbi(chunks, max_bytes = Inf)
  on.exit(unlink(spool$path), add = TRUE)
  expect_true(spool$ok)
  leido <- .leer_spool_muestra_dbi(spool$path)
  expect_true(leido$ok)
  expect_equal(leido$n_filas, 4)
  expect_equal(leido$n_chunks, 2)
  expect_identical(leido$trailer$magic_fin, .MAGIC_FIN_SPOOL_MUESTRA_DBI)
  expect_equal(leido$trailer$n_chunks, 2)
  expect_identical(leido$trailer$checksum_payload, leido$checksum)
  expect_identical(leido$datos, do.call(rbind, chunks))

  con <- file(spool$path, "rb")
  objetos <- list()
  repeat {
    objeto <- tryCatch(unserialize(con), error = function(e) NULL)
    if (is.null(objeto)) break
    objetos[[length(objetos) + 1L]] <- objeto
    if (identical(objeto$tipo, "trailer")) break
  }
  close(con)
  objetos[[length(objetos)]]$checksum <- "lupa16:corrupto"
  corrupto <- tempfile("lupa-spool-corrupto-", fileext = ".bin")
  on.exit(unlink(corrupto), add = TRUE)
  con <- file(corrupto, "wb")
  for (objeto in objetos) writeBin(.serializar_spool_dbi(objeto), con)
  close(con)
  expect_identical(
    .leer_spool_muestra_dbi(corrupto, devolver_chunks = FALSE)$motivo,
    "spool_checksum_invalido"
  )

  incompleto <- tempfile("lupa-spool-incompleto-", fileext = ".bin")
  on.exit(unlink(incompleto), add = TRUE)
  bytes <- readBin(spool$path, "raw", n = file.info(spool$path)$size[[1L]] - 1L)
  writeBin(bytes, incompleto)
  expect_identical(
    .leer_spool_muestra_dbi(incompleto, devolver_chunks = FALSE)$motivo,
    "spool_incompleto:trailer_ausente"
  )

  ids <- objetos
  ids[[length(ids)]]$muestra_id <- "otra-muestra"
  ids_corruptos <- tempfile("lupa-spool-ids-", fileext = ".bin")
  on.exit(unlink(ids_corruptos), add = TRUE)
  con <- file(ids_corruptos, "wb")
  for (objeto in ids) writeBin(.serializar_spool_dbi(objeto), con)
  close(con)
  expect_identical(
    .leer_spool_muestra_dbi(ids_corruptos, devolver_chunks = FALSE)$motivo,
    "spool_checksum_invalido"
  )
})

test_that("el presupuesto del spool se comprueba antes de escribir el chunk", {
  chunks <- list(data.frame(id = seq_len(5L)))
  spool <- .escribir_spool_muestra_dbi(chunks, max_bytes = 100L)
  on.exit(unlink(spool$path), add = TRUE)
  expect_false(spool$ok)
  expect_identical(spool$motivo,
                   "muestra_inestable:presupuesto_materializacion")
  expect_true(any(spool$eventos$tipo_evento == "spool_presupuesto_excedido"))
  expect_equal(spool$n_filas, 0)
})

test_that("muestra_motor materializa una vez y todas las métricas leen el spool", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = seq_len(40L), valor = seq_len(40L) / 2,
    etiqueta = rep(c("a", "b"), 20L)
  ))
  resultado <- perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 10L,
    muestra = 6L, estrategia_mediana = "exacta",
    proteger_datos_personales = FALSE
  )
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_identical(materializacion$backend, "spool_sesion_cliente")
  expect_true(isTRUE(materializacion$validado_relectura))
  expect_equal(materializacion$n_filas, 10)
  expect_equal(resultado$perfil_muestra$meta$filas_analizadas, 6)
  expect_equal(resultado$resumen_tabla$meta$bloques$filas_vistas, 10)
  expect_equal(resultado$resumen_tabla$meta$bloques$recorridos, 1)
  expect_identical(resultado$resumen_tabla$meta$pasadas$valor, "spool")
  expect_equal(
    resultado$perfil_muestra$meta$origen_dbi$muestreo$muestra_id,
    materializacion$muestra_id
  )
  metricas <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica != "n" &
      !is.na(resultado$resumen_tabla$sql$sql), , drop = FALSE
  ]
  expect_true(nrow(metricas) > 0L)
  expect_true(all(metricas$metodo == "spool_sesion_cliente"))
  expect_true(all(metricas$id_consulta == metricas$consulta_id))
  expect_true("id_consulta" %in% names(resultado$resumen_tabla$sql))
  expect_false("muestra_id" %in% names(resultado$resumen_tabla$sql))
  expect_true(all(vapply(
    resultado$perfil_muestra$hallazgos$trazabilidad, is.null, logical(1L)
  )))
  expect_true(all(c("trazabilidad", "ejemplos", "muestra") %in%
                  resultado$perfil_muestra$cobertura_diagnosticos$diagnostico))
})

test_that("un presupuesto excedido no publica una muestra hibrida", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(valor = seq_len(40L)))
  resultado <- perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 40L,
    muestra = 40L, max_bytes_materializacion = 1000L,
    proteger_datos_personales = FALSE
  )
  materializacion <- resultado$resumen_tabla$meta$materializacion
  expect_identical(materializacion$estado, "no_disponible")
  expect_identical(materializacion$motivo,
                   "muestra_inestable:presupuesto_materializacion")
  expect_false(isTRUE(materializacion$validado_relectura))
  expect_true(any(
    resultado$resumen_tabla$meta$vigilante$eventos$tipo_evento ==
      "spool_presupuesto_excedido"
  ))
  expect_true(is.null(resultado$perfil_muestra))
  metricas <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica != "n", , drop = FALSE
  ]
  expect_true(all(metricas$estado == "no_disponible"))
  expect_true(all(metricas$motivo ==
                  "muestra_inestable:presupuesto_materializacion"))
})
