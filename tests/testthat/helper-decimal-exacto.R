# Casos y comprobaciones compartidos por las pruebas de la media, la mediana y
# el desvio de un decimal por bloques y por el spool (la de MySQL y MariaDB
# vive aparte, porque sus controladores no estan en Suggests).

.dexa_casos <- list(
  dos = list(
    filas = 2L,
    columnas = list(
      p4 = list(tipo = c(18L, 4L), valores = c("0.0001", "0.0002"),
                media = 0x1.3a92a30553261p-13,
                mediana = 0x1.3a92a30553261p-13),
      p30 = list(tipo = c(38L, 30L),
                 valores = c(paste0("0.", strrep("0", 29), "1"),
                             paste0("0.", strrep("0", 29), "2")),
                 media = 0x1.e6c71fe61a3efp-100,
                 mediana = 0x1.e6c71fe61a3efp-100)
    )
  ),
  nueve = list(
    filas = 9L,
    columnas = list(
      m2 = list(tipo = c(18L, 2L),
                valores = c("71845.61", "84221.55", "-55676.60", "82368.44",
                            "2409.15", "-50979.09", "7041.19", "85773.16",
                            "35538.90"),
                media = 0x1.c7cd789abcdf0p+14,
                mediana = 0x1.15a5ccccccccdp+15),
      g2 = list(tipo = c(18L, 2L),
                valores = paste0("50000000000000", c(
                  "9.15", "6.19", "5.59", "0.60", "3.67", "7.04", "4.41",
                  "3.49", "1.53"
                )),
                media = 0x1.c6bf52634004ap+48,
                mediana = 0x1.c6bf526340047p+48,
                desvio = 0x1.57cce3d3c21ecp+1)
    )
  )
)

.dexa_crear <- function(con, tabla, caso, decimal = "DECIMAL") {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  columnas <- vapply(names(caso$columnas), function(k) {
    t <- caso$columnas[[k]]$tipo
    paste0(k, " ", decimal, "(", t[[1L]], ",", t[[2L]], ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (", paste(columnas, collapse = ", "), ")"
  ))
  filas <- vapply(seq_len(caso$filas), function(i) {
    paste0("(", paste(vapply(caso$columnas, function(p) p$valores[[i]],
                             character(1L)), collapse = ", "), ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
}

.dexa_comprobar <- function(con, tabla, caso, ...) {
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    fecha = as.POSIXct("2026-11-07 12:00:00", tz = "UTC"), ...
  )))
  columnas <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  via <- paste(names(list(...)), collapse = ",")
  for (k in names(caso$columnas)) {
    esperado <- caso$columnas[[k]]
    for (metrica in intersect(c("media", "mediana", "desvio"),
                              names(esperado))) {
      info <- paste(k, metrica, via)
      estado <- sql$estado[sql$columna == k & sql$metrica == metrica]
      expect_true(all(estado %in% c("calculado", "estimado")), info = info)
      valor <- as.numeric(columnas[[metrica]][columnas$columna == k])
      if (identical(metrica, "desvio")) {
        expect_equal(valor, esperado$desvio, tolerance = 1e-12, info = info)
      } else {
        expect_identical(valor, esperado[[metrica]], info = info)
      }
    }
  }
  invisible(perfil)
}

.dexa_probar <- function(con, prefijo, decimal = "DECIMAL", bloques = TRUE) {
  for (nombre in names(.dexa_casos)) {
    caso <- .dexa_casos[[nombre]]
    tabla <- paste0(prefijo, "_", nombre)
    .dexa_crear(con, tabla, caso, decimal = decimal)
    if (bloques) {
      .dexa_comprobar(con, tabla, caso, bloque_filas = 2L,
                      bloque_muestra = "solo_agregados")
    }
    .dexa_comprobar(con, tabla, caso, universo = "muestra_motor",
                    muestra_motor = caso$filas)
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  }
}
