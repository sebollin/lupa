# La mediana de un decimal en MySQL y MariaDB promediaba las centrales con
# `AVG(valor * 1.0)`, que necesita una cifra de escala mas que la columna: en la
# escala maxima del motor -30 en MySQL, 38 en MariaDB- se recortaba, y la
# mediana de 1e-30 y 2e-30 salia 2e-30. Ahora la consulta suma las centrales en
# el tipo de la columna y R divide por cuantas son: el doble mas cercano de la
# exacta. Las esperadas estan escritas en hexadecimal: son la mediana exacta
# -(a + b) / 2 en fracciones sobre el texto de cada valor- redondeada al doble
# mas cercano, calculada fuera de R.
#
# Usa RMySQL y RMariaDB, que el paquete no declara: ver `test-local-mysql.R`.

.mds_con <- function(maria) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD")
  if (!nzchar(clave)) skip("falta la clave del motor")
  tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) skip("motor local no disponible"))
}

.mds_probar <- function(maria) {
  con <- .mds_con(maria)
  tabla <- if (maria) "lupa_mds_maria" else "lupa_mds_mysql"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  escala <- if (maria) 38L else 30L
  chico <- function(k) paste0("0.", strrep("0", escala - 1L), k)
  try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (escala_max DECIMAL(65,", escala, "), ",
    "centavos DECIMAL(18,2), largo DECIMAL(38,20))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES (", chico(1), ", 0.10, ",
    "8253.90651511007901151702), (", chico(2), ", 0.20, ",
    "3870.25973563673977909110)"
  ))
  esperado <- c(
    # 1,5e-30 y 1,5e-38: la mitad de la ultima cifra de la escala.
    escala_max = if (maria) 0x1.46abcb63fdaabp-126 else 0x1.e6c71fe61a3efp-100,
    # 0,15. El promedio de los dobles de 0,10 y 0,20 es 0,15000000000000002.
    centavos = 0x1.3333333333333p-3,
    # El promedio de los dobles de cada central da 0x1.7ae1547b45842p+12.
    largo = 0x1.7ae1547b45843p+12
  )
  fecha <- as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  for (via in list(
    list(),
    list(universo = "muestra_motor", muestra_motor = 2L)
  )) {
    p <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(list(
      con, tabla, proteger_datos_personales = FALSE, fecha = fecha,
      bloque_muestra = "solo_agregados"
    ), via))))
    columnas <- p$resumen_tabla$columnas
    sql <- p$resumen_tabla$sql
    for (columna in names(esperado)) {
      fila <- sql[sql$columna == columna & sql$metrica == "mediana", , drop = FALSE]
      expect_true(fila$estado %in% c("calculado", "estimado"), info = columna)
      expect_identical(
        as.numeric(columnas$mediana[columnas$columna == columna]),
        esperado[[columna]], info = paste(columna, paste(names(via), collapse = ","))
      )
    }
  }
}

test_that("la mediana de un decimal en MySQL no recorta la mitad de la ultima cifra", {
  skip_on_cran()
  .mds_probar(maria = FALSE)
})

test_that("la mediana de un decimal en MariaDB sigue la misma regla", {
  skip_on_cran()
  .mds_probar(maria = TRUE)
})
