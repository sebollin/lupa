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

test_that("el objeto declara que las metricas muestreadas no comparten filas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:200, b = seq_len(200) / 200))

  # Un consumidor automatico lee el objeto, no la vineta.
  for (modo in c("muestreado", "aproximado")) {
    meta <- perfilar_dbi(con, "t", modo = modo)$resumen_tabla$meta
    expect_false(is.na(meta$muestras_independientes))
    # El alcance del muestreo es por consulta, no por perfilado: las columnas
    # que comparten una consulta consolidada se miden sobre las MISMAS filas, y
    # las de consultas distintas no. Las dos mitades tienen que estar dichas.
    expect_match(meta$muestras_independientes, "MISMAS filas")
    expect_match(meta$muestras_independientes, "muestras\\s+distintas")
    expect_match(meta$muestras_independientes, "columnas_compartidas")
    expect_match(meta$muestras_independientes, "perfil_muestra")
  }
  # Y no aparece donde no corresponde: en los modos que miden sobre la tabla
  # entera no hay nada que advertir.
  for (modo in c("exacto", "seguro", "conteos")) {
    meta <- perfilar_dbi(con, "t", modo = modo)$resumen_tabla$meta
    expect_true(is.na(meta$muestras_independientes))
  }
})
