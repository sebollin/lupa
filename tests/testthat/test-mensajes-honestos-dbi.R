# Dos correcciones de la misma familia, salidas de correr el paquete contra
# bases reales: una donde afirmaba de menos y otra donde callaba algo que el
# consumidor del objeto necesita.

test_that("cuando el motor dijo que es permiso, el mensaje no repite la disyuncion", {
  # En una corrida real fueron veintitres tablas descritas como "no existe o no
  # hay permiso" cuando PostgreSQL habia respondido "permiso denegado a la
  # relacion". Repetir la duda con la respuesta en la mano es informar como
  # incierto algo ya resuelto.
  permisos <- c(
    "ERROR:  permiso denegado a la relación Disuelto",
    "The SELECT permission was denied on the object 'x'",
    "ERROR: permission denied for table y",
    "ORA-00942: table or view does not exist; ORA-01031: insufficient privileges"
  )
  for (motivo in permisos) {
    mensaje <- .mensaje_tabla_inaccesible_dbi(motivo)
    expect_match(mensaje, "no tiene permiso para verla")
    # Cuidado con la subcadena: "pero la credencial" contiene "o la credencial".
    # Lo que no puede aparecer es la disyuncion completa.
    expect_false(grepl("no existe en la conexion DBI", mensaje, fixed = TRUE))
    # El texto del motor se conserva: el diagnostico no reemplaza la evidencia.
    expect_true(grepl(substr(motivo, 1, 20), mensaje, fixed = TRUE))
  }
})

test_that("sin evidencia del motor, la disyuncion se mantiene", {
  for (motivo in c(NA_character_, "connection timed out", "syntax error")) {
    mensaje <- .mensaje_tabla_inaccesible_dbi(motivo)
    expect_match(mensaje, "no existe en la conexion DBI, o la credencial")
    expect_false(grepl("lo dijo explicitamente", mensaje, fixed = TRUE))
  }
})

test_that("el objeto declara la unica materializacion muestreada", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:200, b = seq_len(200) / 200))

  # Un consumidor automatico lee el objeto, no la vineta.
  for (caso in c("muestreado")) {
    argumentos <- .argumentos_caso_dbi(caso, muestra = 20L)
    meta <- do.call(
      perfilar_dbi, c(list(con, "t"), argumentos)
    )$resumen_tabla$meta
    expect_true(is.list(meta$materializacion))
    expect_true(isTRUE(meta$materializacion$externo))
    expect_true(isTRUE(meta$materializacion$validado_relectura))
    expect_true(nzchar(meta$materializacion$muestra_id))
    expect_equal(meta$materializacion$muestra_id, meta$muestreo$muestra_id)
    expect_equal(meta$materializacion$checksum, meta$muestreo$checksum)
  }
  # La aproximacion de medianas es ahora una estrategia del motor sobre la
  # tabla completa, no un sinonimo de muestreo; por eso tampoco activa este
  # metadato. La comprobacion vieja para `modo = "aproximado"` no aplicaba a
  # su significado nuevo y queda reemplazada por esta separacion explicita.
  for (caso in c("exacto", "seguro", "conteos", "aproximado")) {
    argumentos <- .argumentos_caso_dbi(caso, muestra = 20L)
    meta <- do.call(
      perfilar_dbi, c(list(con, "t"), argumentos)
    )$resumen_tabla$meta
    expect_true(is.null(meta$materializacion))
  }
})
