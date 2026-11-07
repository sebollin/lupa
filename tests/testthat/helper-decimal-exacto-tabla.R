# Casos y comprobaciones compartidos por las pruebas de la media y el desvio
# de un decimal por la tabla completa, por bloques y por el spool (la de MySQL
# y MariaDB vive aparte, porque sus controladores no estan en Suggests).

.dext_casos <- list(
  centavos = list(
    escala = 2L,
    valores = c("500000000000006.60", "500000000000009.60",
                "500000000000005.60", "500000000000007.60"),
    media = 0x1.c6bf526340076p+48, desvio = 0x1.b534070e9620cp+0
  ),
  negativos = list(
    escala = 2L,
    valores = c("-500000000000006.60", "-500000000000009.60",
                "-500000000000005.60", "-500000000000007.60"),
    media = -0x1.c6bf526340076p+48, desvio = 0x1.b534070e9620cp+0
  ),
  cancela = list(
    escala = 2L,
    valores = c("999999999999999.99", "-999999999999999.98", "0.01", "0.02",
                "0.03", "-0.04"),
    media = 0x1.47ae147ae147bp-8, desvio = 0x1.1f9b83a95b45fp+49
  ),
  chicos = list(
    escala = 18L,
    valores = paste0("0.", strrep("0", 15), 1:4),
    media = 0x1.203af9ee75616p-52, desvio = 0x1.29aedc86942b4p-53
  ),
  treinta = list(
    escala = 30L,
    valores = c("90000000.123456789012345678901234567890",
                "90000000.123456789012345678901234567891",
                "89999999.876543210987654321098765432109"),
    media = 0x1.5752a002a23d2p+26, desvio = 0x1.23f433877bfc1p-3
  ),
  simples = list(
    escala = 2L, valores = c("1.10", "2.20", "3.30"),
    media = 0x1.199999999999ap+1, desvio = 0x1.199999999999ap+0
  )
)

.dext_comprobar <- function(con, tabla, caso, nombre, ...) {
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-07 12:00:00", tz = "UTC"), ...
  )))
  columnas <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  via <- paste(names(list(...)), collapse = ",")
  for (metrica in c("media", "desvio")) {
    info <- paste(nombre, metrica, via)
    estado <- sql$estado[sql$columna == "x" & sql$metrica == metrica]
    expect_true(all(estado %in% c("calculado", "estimado")), info = info)
    expect_identical(
      as.numeric(columnas[[metrica]][columnas$columna == "x"]),
      caso[[metrica]], info = info
    )
  }
  if (identical(list(...)$incluir_valores, FALSE)) {
    expect_false(any(grepl("50000000000000", sql$sql, fixed = TRUE)),
                 info = paste(nombre, via))
  }
}

.dext_probar <- function(con, prefijo, tipo, bloques = TRUE) {
  for (nombre in names(.dext_casos)) {
    caso <- .dext_casos[[nombre]]
    tabla <- paste0(prefijo, "_", nombre)
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (x ", sprintf(tipo, caso$escala), ")"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ",
      paste0("(", caso$valores, ")", collapse = ", ")
    ))
    .dext_comprobar(con, tabla, caso, nombre)
    if (bloques) {
      .dext_comprobar(con, tabla, caso, nombre, bloque_filas = 2L,
                      bloque_muestra = "solo_agregados")
    }
    .dext_comprobar(con, tabla, caso, nombre, universo = "muestra_motor",
                    muestra_motor = length(caso$valores))
    if (nombre == "centavos") {
      .dext_comprobar(con, tabla, caso, nombre, incluir_valores = FALSE)
      if (bloques) {
        .dext_comprobar(con, tabla, caso, nombre, bloque_filas = 2L,
                        incluir_valores = FALSE)
      }
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  }
}
