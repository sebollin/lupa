# Ronda 27-V: lo que la refutacion de validadores y fechas encontro. Una prueba
# por hallazgo, barata: tablas de decenas de filas. Las cifras esperadas se
# rehacen a mano en cada prueba, no se copian de la salida del paquete.

test_that("H1: el resumen lee los numeros con la convencion que el perfil publica", {
  b <- c("1.500", "2.000", "12.345", "3.250", "4.100", "7.800", "9.999",
         "2.000", "5.500", "2,5")
  # A mano, con coma decimal: el punto agrupa miles.
  leidos <- c(1500, 2000, 12345, 3250, 4100, 7800, 9999, 2000, 5500, 2.5)
  p <- perfilar(data.frame(x = b), analizar_dependencias = FALSE)
  fila <- as.data.frame(p$columnas)
  expect_identical(fila$numero_texto_convencion, "decimal_coma")
  expect_identical(fila$tipo_inferido, "doble")
  expect_equal(fila$media, mean(leidos))
  expect_equal(fila$media, 4849.65)
  expect_equal(fila$maximo, 12345)
  expect_equal(fila$minimo, 2.5)
  expect_identical(fila$estado_resumen_cuantitativo, "calculados")

  # La remediacion lee la misma columna igual: un solo numero por texto.
  expect_equal(
    lupa:::.convertir_numero_regional(b, list(convencion = "decimal_coma"))$valor,
    leidos
  )
  # Y los limites de Tukey del plan salen de la misma lectura.
  expect_equal(lupa:::.limites_outliers(b)$valores, leidos)

  # Importes ordinarios a la uruguaya: ninguno de tres cifras con punto de
  # miles puede ser un minimo de 1,01 ni un atipico.
  importes <- c("850", "870", "880", "890", "900", "910", "920", "930",
                "1.050", "1.120", "940", "950", "960", "970", "1.010",
                "980", "985", "990", "899,5", "875")
  leidos_importes <- c(850, 870, 880, 890, 900, 910, 920, 930, 1050, 1120,
                       940, 950, 960, 970, 1010, 980, 985, 990, 899.5, 875)
  q <- perfilar(data.frame(x = importes), analizar_dependencias = FALSE)
  fila_q <- as.data.frame(q$columnas)
  expect_identical(fila_q$numero_texto_convencion, "decimal_coma")
  expect_equal(fila_q$minimo, 850)
  expect_equal(fila_q$maximo, 1120)
  expect_equal(fila_q$media, mean(leidos_importes))
  cuartiles <- stats::quantile(leidos_importes, c(0.25, 0.75), names = FALSE)
  rango <- diff(cuartiles)
  fuera <- which(leidos_importes < cuartiles[[1L]] - 1.5 * rango |
                   leidos_importes > cuartiles[[2L]] + 1.5 * rango)
  h <- as.data.frame(q$hallazgos)
  outliers <- h[h$tipo_hallazgo == "outliers", , drop = FALSE]
  expect_equal(nrow(outliers), as.integer(length(fuera) > 0L))
  if (nrow(outliers)) {
    expect_equal(outliers$n_afectados[[1L]], length(fuera))
  }
})

test_that("H1: perfilar_dbi() resume la muestra con la misma convencion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  b <- c("1.500", "2.000", "12.345", "3.250", "4.100", "7.800", "9.999",
         "2.000", "5.500", "2,5")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(x = b, stringsAsFactors = FALSE))
  r <- suppressWarnings(perfilar_dbi(con, "t", muestra = 100))
  fila <- as.data.frame(r$perfil_muestra$columnas)
  expect_identical(fila$numero_texto_convencion, "decimal_coma")
  expect_equal(fila$media, mean(c(1500, 2000, 12345, 3250, 4100, 7800, 9999,
                                  2000, 5500, 2.5)))
  expect_equal(fila$maximo, 12345)
})

test_that("H2: la convencion que no se decide no se resume, con punto o con coma", {
  punto <- c("1.500", "2.000", "12.345", "3.250", "4.100", "7.800", "9.999",
             "2.000", "5.500")
  coma <- chartr(".", ",", punto)
  p <- perfilar(data.frame(punto, coma, stringsAsFactors = FALSE),
                analizar_dependencias = FALSE)
  filas <- as.data.frame(p$columnas)
  expect_identical(filas$numero_texto_convencion, c("ambigua", "ambigua"))
  expect_identical(filas$numero_texto_ambiguo, c(TRUE, TRUE))
  # La misma respuesta para los dos separadores: no hay numero que publicar.
  expect_identical(filas$tipo_inferido, c("texto", "texto"))
  expect_identical(filas$estado_resumen_cuantitativo,
                   c("no_aplica", "no_aplica"))
  expect_true(all(is.na(filas$media)))
  expect_identical(inferir_tipo(coma)$tipo, inferir_tipo(punto)$tipo)

  # Un separador de tres cifras no decide al otro: `2,000` es tan ambiguo como
  # `1.500`.
  cruzada <- c("1.500", "2,000", "3.250", "4,100")
  expect_identical(inferir_tipo(cruzada)$tipo, "texto")

  # Si la columna no decide, lo que no tiene lectura propia queda afuera y se
  # declara; no se lee como decimal. A mano: 17 valores de 850.
  casi <- c(rep("850", 17), "1.050", "1.120", "1.010")
  q <- perfilar(data.frame(x = casi), analizar_dependencias = FALSE)
  fila <- as.data.frame(q$columnas)
  expect_identical(fila$numero_texto_convencion, "ambigua")
  expect_equal(fila$minimo, 850)
  expect_equal(fila$maximo, 850)
  expect_identical(fila$n_valores_excluidos_resumen, 3L)
  expect_identical(fila$estado_resumen_cuantitativo, "calculados_sobre_valores")
})

test_that("H5: `%y-%m-%d` es otra lectura de la misma escritura, no otro formato", {
  # A mano: una sola escritura `dd-dd-dd`, y cada valor tiene las tres lecturas
  # validas. Ninguno es inequivoco; hay una representacion, no tres.
  f <- detectar_formatos_fecha(rep("01-02-03", 5))
  expect_setequal(f$formato, c("%d-%m-%y", "%m-%d-%y", "%y-%m-%d"))
  expect_identical(f$n_inequivocos, c(0L, 0L, 0L))
  expect_identical(f$n_ambiguos, c(5L, 5L, 5L))
  expect_false(attr(f, "formatos_mixtos"))

  # `25-12-31` es dia-mes-ano o ano-mes-dia (no mes-dia-ano: no hay mes 25).
  g <- detectar_formatos_fecha(c("25-12-31", "20-11-30", "21-10-29"))
  expect_setequal(g$formato, c("%d-%m-%y", "%y-%m-%d"))
  expect_identical(g$n_inequivocos, c(0L, 0L))
  expect_false(attr(g, "formatos_mixtos"))

  p <- perfilar(
    data.frame(
      a = rep(c("01-02-03", "04-05-06", "07-08-09"), 4),
      b = rep(c("25-12-31", "20-11-30", "21-10-29"), 4),
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE
  )
  h <- as.data.frame(p$hallazgos)
  expect_false(any(h$tipo_hallazgo == "formatos_fecha_mixtos"))
  ambiguos <- h[h$tipo_hallazgo == "formato_fecha_ambiguo", , drop = FALSE]
  expect_setequal(ambiguos$columna, c("a", "b"))
  expect_true(grepl("%y-%m-%d", ambiguos$evidencia[ambiguos$columna == "a"],
                    fixed = TRUE))
  expect_true(grepl("%y-%m-%d", ambiguos$evidencia[ambiguos$columna == "b"],
                    fixed = TRUE))

  # Dos ordenes probados cada uno por un valor propio siguen siendo mixtos.
  expect_true(attr(detectar_formatos_fecha(c("25-12-95", "95-12-25")),
                   "formatos_mixtos"))
})

test_that("H6: `May` en una columna de meses completos en ingles no es otro formato", {
  x <- c("January 5, 2020", "May 7, 2020", "June 1, 2020", "March 3, 2020")
  f <- detectar_formatos_fecha(x)
  expect_identical(f$formato, "%B %d, %Y")
  expect_identical(f$n, 4L)
  expect_false(attr(f, "formatos_mixtos"))
  p <- perfilar(data.frame(f = rep(x[1:3], 4)), analizar_dependencias = FALSE)
  expect_false(any(p$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos"))
  expect_identical(p$columnas$minimo_fecha, "2020-01-05")
  expect_identical(p$columnas$maximo_fecha, "2020-06-01")
  # Entre abreviaturas espanolas `May` sigue siendo abreviatura.
  g <- detectar_formatos_fecha(c("5-Ene-2020", "7-May-2020", "1-Jun-2020"))
  expect_identical(g$formato, "%d-%b-%Y")
})

test_that("H7: la hora 24:00 no se publica como el dia siguiente", {
  f <- detectar_formatos_fecha(c("2024-01-31 24:00", "2024-01-30 10:00"))
  expect_identical(attr(f, "compatibles"), 1L)
  x <- c(rep("2024-01-30 10:00", 11), "2024-01-31 24:00")
  p <- perfilar(data.frame(f = x), analizar_dependencias = FALSE)
  # A mano: el unico instante que se puede publicar sin inventar un dia.
  expect_identical(p$columnas$maximo_fecha, "2024-01-30 10:00:00 UTC")
  expect_identical(p$columnas$n_valores_excluidos_resumen, 1L)
  expect_identical(p$columnas$estado_resumen_cuantitativo,
                   "calculados_sobre_valores")
  # 23:59 y 00:00 siguen siendo horas.
  g <- detectar_formatos_fecha(c("2024-01-31 23:59", "2024-01-30 00:00"))
  expect_identical(attr(g, "compatibles"), 2L)
})

test_that("H8: el texto con desplazamiento horario declara que no se evaluo la fecha civil", {
  x <- c("2020-01-31T23:30:00-03:00", "2020-01-15T10:00:00-03:00")
  p <- perfilar(data.frame(f = x), analizar_dependencias = FALSE)
  expect_identical(p$columnas$tipo_inferido, "fecha-hora")
  expect_true(is.na(p$columnas$n_filas_fecha_civil_distinta_utc))
  cobertura <- as.data.frame(p$cobertura_diagnosticos)
  fila <- cobertura[cobertura$diagnostico == "zona_horaria_fecha_hora" &
                      cobertura$columna == "f", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  # Sin desplazamiento la fecha publicada es la escrita: nada que declarar.
  q <- perfilar(data.frame(f = c("2020-01-31 23:30", "2020-01-15 10:00")),
                analizar_dependencias = FALSE)
  cobertura_q <- as.data.frame(q$cobertura_diagnosticos)
  expect_false(any(cobertura_q$diagnostico == "zona_horaria_fecha_hora"))
})

test_that("H9: el RUT guardado como numero se valida como el texto", {
  # A mano (modulo 11 de python-stdnum): `080121500010` es valido; como numero
  # pierde el cero inicial y queda con once digitos.
  digitos <- c(0L, 8L, 0L, 1L, 2L, 1L, 5L, 0L, 0L, 0L, 1L)
  verificador <- (-sum(digitos * c(4L, 3L, 2L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L))) %% 11L
  expect_identical(verificador, 0L)
  expect_true(validar_rut_uy("080121500010"))
  expect_true(validar_rut_uy(80121500010))
  # La cedula de siete ya se completaba: mismo criterio.
  expect_true(validar_ci_uy(1234561))
  # Once digitos que no completan un RUT siguen siendo FALSE.
  expect_false(validar_rut_uy(59899123456))
  # Lo que `read.csv()` deja de una columna de RUT: los de prefijo 01 a 09
  # llegan con once digitos. Digito verificador rehecho a mano.
  dv <- function(once) {
    (-sum(as.integer(strsplit(once, "", fixed = TRUE)[[1L]]) *
            c(4L, 3L, 2L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L))) %% 11L
  }
  cuerpos <- c("05123456001", "07654321001", "21100342001", "08012150001",
               "03111222001", "02555666001")
  verificadores <- vapply(cuerpos, dv, integer(1L), USE.NAMES = FALSE)
  expect_true(all(verificadores <= 9L))
  ruts <- paste0(cuerpos, verificadores)
  expect_true(all(validar_rut_uy(ruts)))
  como_numero <- as.numeric(ruts)
  expect_identical(sum(nchar(sprintf("%.0f", como_numero)) == 11L), 5L)
  expect_true(all(validar_rut_uy(como_numero)))
  p <- perfilar(data.frame(rut = como_numero), analizar_dependencias = FALSE)
  dp <- as.data.frame(p$datos_personales)
  expect_identical(dp$poder_discriminante[dp$columna == "rut"], "verificado")
})

test_that("H9: integer64 de once digitos tambien", {
  skip_if_not_installed("bit64")
  expect_true(validar_rut_uy(bit64::as.integer64("80121500010")))
})

test_that("H3: un doble desde 2^53 no se valida por la impresion de R", {
  # 2^53 - 1 tiene sus digitos; 2^53 ya es el mismo doble que 2^53 + 1.
  menor <- 2^53 - 1
  texto_menor <- sprintf("%.0f", menor)
  expect_identical(validar_luhn(menor), validar_luhn(texto_menor))
  expect_false(validar_luhn(2^53))
  expect_false(validar_luhn(9.1e15))
  # El mismo numero como texto si se valida: sus digitos estan escritos.
  expect_true(validar_luhn("9100000000000000"))
  # `4111111111111111111` como doble es `...168`: no se valida lo redondeado.
  expect_false(validar_luhn(as.numeric("1253896908084157938")))
})

test_that("H4: una magnitud con clase devuelve FALSE o NA, no aborta", {
  fechas <- as.Date(c("2020-01-01", NA))
  expect_identical(validar_luhn(fechas), c(FALSE, NA))
  expect_identical(validar_ci_uy(fechas), c(FALSE, NA))
  expect_identical(validar_correo(fechas), c(FALSE, NA))
  expect_identical(validar_iso3166(fechas), c(FALSE, NA))
  expect_identical(validar_ci_uy(as.POSIXct("2023-11-14 22:13:20", tz = "UTC")),
                   FALSE)
  # 12345672 es una cedula valida como numero; como segundos no lo es.
  expect_true(validar_ci_uy(12345672))
  expect_false(validar_ci_uy(as.difftime(12345672, units = "secs")))
  # El factor y el integer64 siguen validando su valor.
  expect_true(validar_ci_uy(factor("1.234.567-2")))
})

test_that("H4: hms y units tampoco se validan como documento", {
  skip_if_not_installed("hms")
  expect_false(validar_ci_uy(hms::as_hms(12345672)))
  skip_if_not_installed("units")
  expect_false(validar_ci_uy(units::set_units(12345672, "m")))
})

test_that("H10: el espacio duro al borde se recorta como el tabulador", {
  nbsp <- "\u00a0"
  fino <- "\u2009"
  expect_true(validar_ci_uy("1.234.567-2"))
  expect_true(validar_ci_uy(paste0("1.234.567-2", nbsp)))
  expect_true(validar_ci_uy(paste0(nbsp, "1.234.567-2")))
  expect_true(validar_ci_uy("1.234.567-2\t"))
  expect_true(validar_luhn(paste0("79927398713", nbsp)))
  expect_true(validar_iso3166(paste0("UY", nbsp)))
  expect_true(validar_correo(paste0("persona@example.org", nbsp)))
  # Como separador, cualquier espacio: el fino entre grupos de la cedula.
  expect_true(validar_ci_uy(paste0("1", fino, "234", fino, "567-2")))
  # Solo espacios sigue siendo un valor presente invalido.
  expect_false(validar_ci_uy(paste0(nbsp, nbsp)))
  # Y la URL no recorta: un espacio literal la invalida.
  expect_false(validar_url(paste0("https://ejemplo.uy", nbsp)))
})

test_that("H11: la IPv4 embebida va al final de la IPv6 y con octetos decimales", {
  expect_false(validar_url("http://[1.2.3.4::]/"))
  expect_false(validar_url("http://[::1.2.3.4.]/"))
  expect_false(validar_url("http://[::01.2.3.4]/"))
  expect_true(validar_url("http://[::1.2.3.4]/"))
  expect_true(validar_url("http://[::ffff:1.2.3.4]/"))
  expect_true(validar_url("http://[1:2:3:4:5:6:1.2.3.4]/"))
  # Como nombre de host fuera de corchetes siguen siendo validos (reg-name).
  expect_true(validar_url("http://1.2.3.4./"))
})

test_that("H12: una letra suelta sin su opuesto no es un literal logico", {
  sexo <- c(rep("F", 16), rep("M", 4))
  expect_identical(inferir_tipo(sexo)$tipo, "texto")
  expect_identical(inferir_tipo(rep("F", 5))$tipo, "texto")
  # Con su opuesto si: 16 F y 4 T son 20 de 20 literales.
  expect_identical(inferir_tipo(c(rep("F", 16), rep("T", 4)))$tipo, "logico")
  expect_identical(inferir_tipo(c("S", "N", "S", "S"))$tipo, "logico")
  expect_identical(inferir_tipo(c("S", "S", "S", "no"))$tipo, "logico")
  p <- perfilar(data.frame(sexo = sexo), analizar_dependencias = FALSE)
  expect_false(any(p$hallazgos$tipo_hallazgo == "tipo_declarado_distinto"))
})
