# Ronda 26: lo que la refutacion de la proteccion encontro. Una prueba por
# hallazgo, barata: tablas de decenas o pocos cientos de filas.

test_that("T1: el nombre que el plan propone no pasa por el barrido", {
  n <- 60L
  d <- data.frame(
    id = seq_len(n),
    pn = rep(c("Segundo", "Florencia", "Martina", "Joaquin", "Rosario",
               "Valentina"), length.out = n),
    sn = rep(c("Maria", "Jose", "Ines", "Pablo"), length.out = n),
    pa = rep(c("Gonzalez", "Rodriguez", "Fernandez", "Martinez"),
             length.out = n),
    monto = seq_len(n) + 0.5,
    stringsAsFactors = FALSE
  )
  names(d) <- c("id", "Primer Nombre", "Segundo Nombre", "Primer Apellido",
                "monto")
  p <- suppressWarnings(perfilar(d))
  expect_true("Primer Nombre" %in% .columnas_personales_protegidas(p))
  h <- as.data.frame(p$hallazgos)
  evidencia <- h$evidencia[h$tipo == "nombres_columnas_problematicos"]
  expect_length(evidencia, 1L)
  expect_true(grepl('"Segundo Nombre" -> "Segundo.Nombre"', evidencia,
                    fixed = TRUE))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  renombres <- which(pl$hallazgo == "nombres_columnas_problematicos")
  expect_length(renombres, 2L)
  propuestos <- lapply(pl$parametros[renombres], `[[`, "nombres_propuestos")
  expect_identical(propuestos[[1L]], c("id", "Primer.Nombre", "Segundo.Nombre",
                                       "Primer.Apellido", "monto"))
  expect_identical(propuestos[[2L]], c("id", "primer_nombre", "segundo_nombre",
                                       "primer_apellido", "monto"))
  ap <- suppressWarnings(aplicar(pl, d))
  expect_identical(names(ap$datos), propuestos[[1L]])
  # El valor sigue tapado donde es un valor: la moda de la columna protegida.
  expect_false(any(grepl("Segundo", unlist(p$columnas$moda), fixed = TRUE)))
})

test_that("T1: el nombre que contiene un valor protegido se propone entero", {
  d <- data.frame(
    id = seq_len(60L),
    titular = c(rep("Rodriguez Perez", 20L),
                sprintf("Persona %03d Apellido%03d", 1:40, 1:40)),
    x = rep(c("a", "b"), 30L),
    stringsAsFactors = FALSE
  )
  names(d)[3L] <- "pago Rodriguez Perez"
  p <- suppressWarnings(perfilar(d, columnas_personales = "titular"))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  renombres <- which(pl$hallazgo == "nombres_columnas_problematicos")
  propuestos <- lapply(pl$parametros[renombres], `[[`, "nombres_propuestos")
  expect_identical(propuestos[[1L]], c("id", "titular", "pago.Rodriguez.Perez"))
  expect_identical(propuestos[[2L]], c("id", "titular", "pago_rodriguez_perez"))
  ap <- suppressWarnings(aplicar(pl, d))
  expect_identical(names(ap$datos), c("id", "titular", "pago.Rodriguez.Perez"))
  # Y el valor, escrito en una celda que no es un nombre, se sigue tapando.
  d$nota <- rep(c("pago de Rodriguez Perez", "sin nota"), 30L)
  p2 <- suppressWarnings(perfilar(d, columnas_personales = "titular"))
  expect_false(any(grepl("Rodriguez Perez", unlist(p2$columnas$moda),
                         fixed = TRUE)))
})

test_that("T1: las marcas que crea el plan llevan el nombre de su columna", {
  n <- 120L
  d <- data.frame(
    id = seq_len(n),
    pn = rep(c("Segundo", "Florencia", "Martina", "Joaquin"), length.out = n),
    monto = 100 + (seq_len(n) %% 9),
    stringsAsFactors = FALSE
  )
  d$monto[c(3L, 9L)] <- c(9000, 12000)
  d$monto[20:60] <- NA
  names(d) <- c("id", "Primer Nombre", "monto Segundo")
  p <- suppressWarnings(perfilar(d))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  marcas <- unlist(lapply(pl$parametros, `[[`, "columna_marca"))
  expect_true(all(c(".ausente_monto.Segundo", ".outlier_monto.Segundo") %in%
                    marcas))
  pl$aplicar[] <- pl$estrategia %in% c("marcar_outliers",
                                       "marcar_filas_ausentes")
  ap <- suppressWarnings(aplicar(pl, d))
  expect_true(all(c(".ausente_monto.Segundo", ".outlier_monto.Segundo") %in%
                    names(ap$datos)))
  expect_false(any(grepl("protegido", names(ap$datos), fixed = TRUE)))
})

test_that("F1: detectar_dependencias() no publica valores personales", {
  ced <- sprintf("%d.%03d.%03d-%d", 1 + (1:40) %% 6, 100 + 1:40, 500 + 1:40,
                 (1:40) %% 10)
  k <- rep(1:40, length.out = 200L)
  d <- data.frame(
    cedula = ced[k], titular = sprintf("Titular Apellidoso %02d", k),
    obs = paste("pago CI", ced[k]), monto = k %% 7,
    stringsAsFactors = FALSE
  )
  d$titular[7L] <- "Rosario Inchausti Larrabide"
  d$monto[9L] <- 99
  dep <- detectar_dependencias(d, umbral = 0.9)
  texto <- c(dep$evidencia, utils::capture.output(print(dep)))
  expect_false(any(grepl("2.107.507-7", texto, fixed = TRUE)))
  expect_false(any(grepl("Rosario Inchausti", texto, fixed = TRUE)))
  expect_false(any(grepl("Titular Apellidoso", texto, fixed = TRUE)))
  par <- dep$determinante == "cedula" & dep$dependiente == "titular"
  expect_identical(dep$evidencia[par], "[evidencia protegida]")
  # La columna que no es personal se cita, con el documento tapado.
  obs_monto <- dep$determinante == "obs" & dep$dependiente == "monto"
  expect_true(any(obs_monto))
  expect_true(grepl("pago CI [valor protegido]", dep$evidencia[obs_monto],
                    fixed = TRUE))
  # Con la proteccion apagada, la evidencia vuelve, y es lo que publica
  # `perfilar(proteger_datos_personales = FALSE)`.
  abierta <- detectar_dependencias(d, umbral = 0.9,
                                   proteger_datos_personales = FALSE)
  expect_true(any(grepl("2.107.507-7", abierta$evidencia, fixed = TRUE)))
  p0 <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_true(any(grepl("2.107.507-7", p0$dependencias$evidencia,
                        fixed = TRUE)))
  # Lo declarado se protege aunque el lexico no lo reconozca.
  e <- data.frame(legajo = sprintf("LG%05d", 37L * k), area = k %% 5,
                  stringsAsFactors = FALSE)
  e$area[7L] <- 9
  sin_declarar <- detectar_dependencias(e, umbral = 0.9)
  expect_true(any(grepl("LG00259", sin_declarar$evidencia, fixed = TRUE)))
  declarada <- detectar_dependencias(e, umbral = 0.9,
                                     columnas_personales = "legajo")
  expect_false(any(grepl("LG00259", declarada$evidencia, fixed = TRUE)))
})

test_that("F1: analizar_tiempo() sin perfil protege las fechas personales", {
  d <- data.frame(
    alta = as.Date("2024-01-01") + rep(0:29, 2L),
    fecha_nacimiento = as.Date("1960-01-01") + seq(0, 590, by = 10)
  )
  t0 <- analizar_tiempo(d)
  resumen <- t0$resumen
  fila <- resumen$columna == "fecha_nacimiento"
  expect_true(any(fila))
  expect_true(is.na(resumen$fecha_minima[fila]))
  expect_true(is.na(resumen$fecha_maxima[fila]))
  expect_false(any(grepl("1960-01-01", utils::capture.output(print(t0)),
                         fixed = TRUE)))
  # La columna que no es personal conserva sus fechas.
  expect_identical(resumen$fecha_minima[resumen$columna == "alta"],
                   as.Date("2024-01-01"))
  # Igual que con un perfil protegido.
  p <- suppressWarnings(perfilar(d))
  con_perfil <- analizar_tiempo(d, perfil = p)$resumen
  expect_identical(is.na(con_perfil$fecha_minima), is.na(resumen$fecha_minima))
  # Y con la proteccion apagada, se publican.
  abierto <- analizar_tiempo(d, proteger_datos_personales = FALSE)$resumen
  expect_identical(abierto$fecha_minima[abierto$columna == "fecha_nacimiento"],
                   as.Date("1960-01-01"))
})

test_that("F2: en perfilar_dbi() con muestra, el relleno lo confirma la tabla", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  verificador <- function(num) {
    d <- as.integer(strsplit(sprintf("%07d", num), "")[[1L]])
    (10 - sum(d * c(2, 9, 8, 7, 6, 3, 4)) %% 10) %% 10
  }
  cedula_uy <- function(num) {
    s <- sprintf("%07d", num)
    sprintf("%s.%s.%s-%d", substr(s, 1, 1), substr(s, 2, 4), substr(s, 5, 7),
            verificador(num))
  }
  v <- "2.222.222-2"
  expect_true(validar_ci_uy(v))
  clientes <- vapply(1000000L + 7919L * (1:120), cedula_uy, character(1L))
  otros <- vapply(2000000L + 7919L * (1:240), cedula_uy, character(1L))
  # Las primeras 40 filas -la muestra- parecen casi una clave con `v` cinco
  # veces; en la tabla entera cada cliente tiene dos movimientos y `v`, una
  # cedula valida, quince. `documento` si es casi una clave con su relleno.
  d <- data.frame(
    cedula = c(clientes[1:35], rep(v, 5L), clientes[1:35],
               rep(clientes[36:120], 2L), rep(v, 10L)),
    documento = c(otros[1:35], rep("99999999", 5L), otros[36:240],
                  rep("99999999", 10L)),
    stringsAsFactors = FALSE
  )
  d$obs <- paste("pago CI", d$cedula)
  d$monto <- c(rep(99999999, 30L), seq_len(nrow(d) - 30L))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  pd <- suppressWarnings(perfilar_dbi(
    con, "t", muestra = 40, columnas_personales = c("cedula", "documento")
  ))
  for (columnas in list(pd$resumen_tabla$columnas, pd$perfil_muestra$columnas)) {
    expect_identical(columnas$moda[columnas$columna == "obs"],
                     "pago CI [valor protegido]")
    # El relleno confirmado sale del piso, como en memoria.
    expect_identical(as.character(columnas$moda[columnas$columna == "monto"]),
                     "99999999")
  }
  texto <- unlist(lapply(pd, function(x) unlist(x, use.names = FALSE)))
  expect_false(any(grepl(v, texto, fixed = TRUE)))
})

test_that("F3, F4, F5: el correo escrito con palabras se lee en sus formas", {
  v <- "juan.perez@empresa.com.uy"
  formas <- c(
    # F3: la arroba literal con el punto en palabras.
    "ver juan.perez@empresa punto com punto uy",
    "ver juan.perez@empresa(dot)com(dot)uy",
    "ver juan.perez @ empresa dot com dot uy",
    # F5: espacios de Unicode y parentesis de ancho completo.
    "ver juan.perez\u00a0at\u00a0empresa.com.uy",
    "ver juan.perez\u3000at\u3000empresa.com.uy",
    "ver juan.perez\uff08at\uff09empresa.com.uy"
  )
  expect_identical(.reemplazar_valores_protegidos(formas, v),
                   rep("[valor protegido]", length(formas)))
  # F4: el mismo texto en latin1, marcado y sin marca.
  latin1 <- iconv(c(
    "contactar a Jos\u00e9: juan.perez(at)empresa.com.uy",
    "Jos\u00e9 juan.perez arroba empresa punto com punto uy"
  ), "UTF-8", "latin1")
  Encoding(latin1) <- "latin1"
  sin_marca <- latin1
  Encoding(sin_marca) <- "unknown"
  textos <- c(latin1, sin_marca)
  expect_true(all(.reemplazar_valores_protegidos(textos, v) != textos))
  # Controles: el punto en palabras sin arroba no es un punto, y una frase con
  # otro correo no se tapa.
  controles <- c("maria punto lopez", "punto de venta",
                 "escribir a soporte@otra.com.uy punto final")
  expect_identical(.reemplazar_valores_protegidos(controles, v), controles)
  # Punta a punta: la moda de una columna que no es personal.
  d <- data.frame(
    correo = c(rep(v, 30L), sprintf("cliente%02d@otra.com.uy", 1:90)),
    nota = c(rep(formas[[1L]], 60L), sprintf("nota %02d", 1:60)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(d))
  expect_identical(p$columnas$moda[p$columnas$columna == "nota"],
                   "[valor protegido]")
})

test_that("F6: la fecha con hora se reconoce con los separadores anchos", {
  v <- "2024-06-07 09:04:05"
  textos <- paste0("alta 2024-06-07", c("\uff20", "\uff1b", "\uff5c", "\uff0c"),
                   "09:04:05")
  expect_identical(.reemplazar_valores_protegidos(textos, v),
                   rep("[valor protegido]", length(textos)))
  # La fecha sola, o con otra hora, no.
  controles <- c("alta 2024-06-07", "alta 2024-06-07\uff2010:04:05")
  expect_identical(.reemplazar_valores_protegidos(controles, v), controles)
})

test_that("F7: las letras modificadoras como tilde se pliegan", {
  v <- "Josefina Gonzalez"
  marcas <- c("\u02bc", "\ua788", "\u02b9", "\u2e2f", "\u02be", "\u02ee")
  textos <- paste0("titular Jose", marcas, "fina Gonza", marcas, "lez")
  expect_true(all(.reemplazar_valores_protegidos(textos, v) != textos))
  controles <- c("titular Josefa Gonzalvez", "titular Jose\u02bcfa Gonzalo")
  expect_identical(.reemplazar_valores_protegidos(controles, v), controles)
  # Entre cifras la prima no une: no es un acento.
  expect_identical(.reemplazar_valores_protegidos("ref 4123\u02b9456", "4123456"),
                   "ref 4123\u02b9456")
})

test_that("F8: el celular con su prefijo agrupado de a tres se reconoce", {
  v <- "099 123 456"
  textos <- c("tel 598.099.123.456", "tel +598.099.123.456",
              "tel 598,099,123,456", "tel 598'099'123'456")
  expect_identical(.reemplazar_valores_protegidos(textos, v),
                   rep("[valor protegido]", length(textos)))
  # El importe de cuatro grupos sigue probandose entero: su cola no empieza
  # con un cero y no es el celular.
  controles <- c("$ 3.919.024.166", "total 5.099.123.457")
  expect_identical(.reemplazar_valores_protegidos(controles, v), controles)
})

test_that("F9: perfilar_coleccion() protege una tabla con los valores de otra", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  cedulas <- sprintf("%d.%03d.%03d-%d", 1L + (1:60) %% 6L, 100L + 1:60,
                     300L + 1:60, (1:60) %% 10L)
  clientes <- data.frame(
    cedula = c(cedulas, rep("99999999", 6L)),
    nombre = c(sprintf("Cliente Apellidoso Numero %03d", 1:60),
               sprintf("Sin Documento %02d", 1:6)),
    stringsAsFactors = FALSE
  )
  k <- c(rep(7L, 40L), rep(1:60, 2L))
  movimientos <- data.frame(
    obs = paste("pago de", clientes$nombre[k], "CI", clientes$cedula[k]),
    monto = c(rep(99999999, 50L), seq_len(length(k) - 50L)),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "clientes", clientes)
  DBI::dbWriteTable(con, "movimientos", movimientos)
  pc <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("clientes", "movimientos")), conservar_perfiles = TRUE
  ))
  pm <- pc$perfiles$movimientos
  texto <- unlist(lapply(pm, function(x) unlist(x, use.names = FALSE)))
  expect_false(any(grepl(cedulas[[7L]], texto, fixed = TRUE)))
  expect_false(any(grepl("Apellidoso Numero 007", texto, fixed = TRUE)))
  # El relleno de la cedula, casi clave sin el, no tapa el `monto` de la otra
  # tabla: la misma regla que en memoria.
  columnas <- pm$resumen_tabla$columnas
  expect_identical(as.character(columnas$moda[columnas$columna == "monto"]),
                   "99999999")
})

test_that("T2: la columna con clasificacion debil tapa sus valores, no su evidencia", {
  n <- 400L
  d <- data.frame(id = seq_len(n))
  d$codigo_producto <- 10000000L + 7919L * seq_len(n)
  d$codigo_producto[2L] <- d$codigo_producto[1L]
  d$cantidad <- 1000000L + 2003L * seq_len(n)
  p <- suppressWarnings(perfilar(d))
  dp <- p$datos_personales
  expect_identical(
    dp$poder_discriminante[dp$columna == "codigo_producto"], "debil"
  )
  expect_false("codigo_producto" %in% .columnas_personales_protegidas(p))
  h <- p$hallazgos
  evidencia <- h$evidencia[h$columna == "codigo_producto" &
                             h$tipo_hallazgo == "casi_clave"]
  expect_length(evidencia, 1L)
  expect_true(grepl("valores distintos de 400", evidencia, fixed = TRUE))
  expect_true(grepl("Colisiones: [valor protegido] (2)", evidencia,
                    fixed = TRUE))
  expect_false(grepl(as.character(d$codigo_producto[1L]), evidencia,
                     fixed = TRUE))
  # Su magnitud de Benford y su cobertura no tapan nada: el minimo y el
  # maximo de la columna se publican.
  benford <- p$meta$benford$resultados
  magnitudes <- vapply(benford, function(x) x$ordenes_magnitud, numeric(1L))
  nombres <- vapply(benford, `[[`, character(1L), "columna")
  expect_false(anyNA(magnitudes[nombres %in% c("codigo_producto", "cantidad")]))
  motivos <- p$cobertura_diagnosticos$motivo
  expect_false(any(grepl("log10(max/min) [valor protegido]", motivos,
                         fixed = TRUE)))
  # La misma columna declarada personal se tapa entera.
  q <- suppressWarnings(perfilar(d, columnas_personales = "codigo_producto"))
  hq <- q$hallazgos
  expect_identical(hq$evidencia[hq$columna == "codigo_producto" &
                                  hq$tipo_hallazgo == "casi_clave"],
                   "[evidencia protegida]")
})

test_that("T4: la SQL guardada cita el nombre igual a un valor protegido", {
  v <- "Rodriguez Perez"
  nombre <- "Rodr\u00edguez P\u00e9rez"
  sql <- paste0("SELECT COUNT(`id`), COUNT(DISTINCT `", nombre, "`) FROM `t`")
  objeto <- list(
    columnas = data.frame(columna = c("id", nombre),
                          moda = c("pago de Rodriguez Perez", "x"),
                          stringsAsFactors = FALSE),
    sql = data.frame(columna = c("id", nombre), sql = c(sql, sql),
                     stringsAsFactors = FALSE),
    meta = list(sql_esquema = sql)
  )
  protegido <- .proteger_textos_salida(objeto, v,
                                       intocables = c("id", nombre))
  expect_identical(protegido$sql$sql, c(sql, sql))
  expect_identical(protegido$meta$sql_esquema, sql)
  # Fuera de la SQL el valor se sigue tapando.
  expect_identical(protegido$columnas$moda[[1L]], "pago de [valor protegido]")
  # Y un valor que NO es un nombre citado se tapa tambien dentro de la SQL.
  otra <- "SELECT 1 FROM `t` -- Rodriguez Perez"
  expect_true(grepl("[valor protegido]",
                    .proteger_textos_salida(list(sql = otra), v,
                                            intocables = c("id", nombre))$sql,
                    fixed = TRUE))
})

test_that("T3: la mascara de un rango de redes no se pega a la IP siguiente", {
  v <- "2.429.235-3"
  rango <- "red 10.4.1.0/24-29.235.20.0/24"
  expect_identical(.reemplazar_valores_protegidos(rango, v), rango)
  # El documento parcial sigue tapandose, y la IP con mascara que es un
  # documento entero tambien.
  expect_false(identical(.reemplazar_valores_protegidos("CI 2.429.235", v),
                         "CI 2.429.235"))
  expect_false(identical(
    .reemplazar_valores_protegidos("CI 4.123.456.7/24", "4.123.456-7"),
    "CI 4.123.456.7/24"
  ))
})
