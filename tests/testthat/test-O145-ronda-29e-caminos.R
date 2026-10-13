# Ronda 29-E: lo que la refutacion de "la misma tabla por todos los caminos"
# encontro. Una prueba por hallazgo, con la cifra rehecha a mano y tablas
# chicas en SQLite en memoria.

conexion_sqlite_r29e <- function() {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

resumen_r29e <- function(con, tabla, bloque_filas = NULL, ...) {
  suppressWarnings(perfilar_dbi(
    con, tabla, bloque_filas = bloque_filas,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE, ...
  ))$resumen_tabla
}

fila_r29e <- function(resumen, columna) {
  resumen$columnas[resumen$columnas$columna == columna, , drop = FALSE]
}

estados_r29e <- function(resumen, columna, metricas) {
  sql <- resumen$sql
  sql <- sql[sql$columna == columna & sql$metrica %in% metricas, , drop = FALSE]
  stats::setNames(sql$estado, sql$metrica)[metricas]
}

magnitudes_r29e <- c("minimo", "maximo", "media", "mediana", "desvio")

test_that("H1: con bloque_filas un integer64 da las cifras del agregado y no cambia la clase de la tabla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # `marca` (milisegundos) y `neg` no entran en 32 bits: RSQLite las devuelve
  # como integer64. La primera columna es doble: la de la refutacion era una
  # clave bigserial primero, que contagiaba a las demas.
  DBI::dbExecute(con, "CREATE TABLE t (marca INTEGER, x REAL, neg INTEGER)")
  DBI::dbExecute(con, paste(
    "INSERT INTO t VALUES (1700000000000, 0.25, -3000000001),",
    "(1700000000005, 0.5, -3000000002), (1700000000010, 0.75, -3000000003),",
    "(1700000000020, 1.5, -3000000004), (1700000000020, 2.125, -3000000005)"
  ))
  expect_s3_class(DBI::dbGetQuery(con, "SELECT marca FROM t")$marca, "integer64")
  # A mano, sobre dobles: todos los valores estan por debajo de 2^53.
  a_mano <- list(
    marca = 1700000000000 + c(0, 5, 10, 20, 20),
    x = c(0.25, 0.5, 0.75, 1.5, 2.125),
    neg = -3000000000 - 1:5
  )
  for (bf in list(NULL, 2L, 3L)) {
    rt <- resumen_r29e(con, "t", bf)
    for (campo in magnitudes_r29e) {
      expect_false(inherits(rt$columnas[[campo]], "integer64"))
      expect_type(rt$columnas[[campo]], "double")
    }
    for (columna in names(a_mano)) {
      v <- a_mano[[columna]]
      f <- fila_r29e(rt, columna)
      expect_identical(f$minimo, min(v))
      expect_identical(f$maximo, max(v))
      expect_equal(f$media, mean(v), tolerance = 1e-12)
      expect_identical(f$mediana, stats::median(v))
      expect_equal(f$desvio, stats::sd(v), tolerance = 1e-9)
      expect_true(all(estados_r29e(rt, columna, magnitudes_r29e) == "calculado"))
    }
  }
})

test_that("H1: con bloque_filas un integer64 desde 2^53 retiene las siete magnitudes, como el agregado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("bit64")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE g (g INTEGER, d REAL)")
  # 9007199254740993 = 2^53 + 1: ningun doble lo representa.
  DBI::dbExecute(con, paste(
    "INSERT INTO g VALUES (9007199254740993, 1.5), (9007199254740993, 2.5),",
    "(5, 4), (-7, 5), (0, 6)"
  ))
  siete <- c("minimo", "maximo", "media", "n_ceros", "n_negativos",
             "mediana", "desvio")
  for (bf in list(NULL, 2L)) {
    rt <- resumen_r29e(con, "g", bf)
    expect_true(all(estados_r29e(rt, "g", siete) == "no_disponible"))
    f <- fila_r29e(rt, "g")
    expect_true(all(is.na(unlist(f[siete]))))
    # Lo que se cuenta con exactitud se sigue publicando.
    expect_identical(as.numeric(f$n_distintos), 4)
    expect_identical(f$moda, "9007199254740993")
    # Y la columna doble de al lado no se entera.
    expect_equal(fila_r29e(rt, "d")$media, mean(c(1.5, 2.5, 4, 5, 6)))
  }
})

hallazgos_columna_r29e <- function(h, columna, grupo = NULL) {
  h <- as.data.frame(h)
  filas <- which(h$columna == columna)
  if (!is.null(grupo)) filas <- intersect(filas, which(h$grupo == grupo))
  h[filas, , drop = FALSE]
}

test_that("H2: perfilar_por() mide el centinela de la columna entera dentro del universo declarado", {
  set.seed(19)
  n <- 240
  region <- rep(c("norte", "sur", "este"), each = n / 3)
  edad <- sample(c(rep(10, 8), 20:70), n, TRUE)
  ingreso <- round(stats::runif(n, 1000, 3000))
  menor <- edad < 18
  # Los menores llevan un relleno de "no aplica"; el centinela real esta en el
  # universo de los adultos.
  ingreso[menor] <- 999999
  adultos <- which(!menor)
  ingreso[adultos[seq(1, length(adultos), length.out = 18)]] <- 88888
  datos <- data.frame(region = region, edad = edad, ingreso = ingreso)
  ap <- list(ingreso = ~ edad >= 18)
  # A mano: valores repetidos 5 o mas veces fuera de los limites de Tukey.
  repetidos_fuera <- function(v) {
    q <- stats::quantile(v, c(0.25, 0.75), names = FALSE)
    lim <- c(q[[1]] - 1.5 * diff(q), q[[2]] + 1.5 * diff(q))
    t <- table(v[v < lim[[1]] | v > lim[[2]]])
    t[t >= 5]
  }
  expect_identical(names(repetidos_fuera(ingreso)), c("88888", "999999"))
  expect_identical(names(repetidos_fuera(ingreso[!menor])), "88888")
  entero <- perfilar(datos, aplicabilidad = ap, proteger_datos_personales = FALSE)
  expect_identical(
    entero$columnas$centinela_valor[entero$columnas$columna == "ingreso"], 88888
  )
  por <- perfilar_por(datos, "region", aplicabilidad = ap,
                      proteger_datos_personales = FALSE)
  cobertura <- as.data.frame(attr(por, "cobertura_diagnosticos"))
  for (g in unique(region)) {
    h <- hallazgos_columna_r29e(por, "ingreso", g)
    centinela <- h[h$tipo_hallazgo == "posible_centinela_numerico", ]
    expect_equal(nrow(centinela), 1L)
    expect_equal(centinela$n_afectados,
                 sum(ingreso[region == g] == 88888))
    expect_false(any(cobertura$grupo == g & cobertura$columna == "ingreso" &
                       grepl("columna entera", cobertura$motivo)))
  }
})

test_that("H2: las guardas de numeracion e identificador de perfilar_por() miran el mismo universo", {
  # Una numeracion 1..2000 con el 999 dos veces, y 200 filas fuera del universo
  # con relleno 999999. Dentro del universo la columna es densa e
  # identificadora y no acusa nada; cada grupo, intercalado, ya no es denso.
  id <- seq_len(2000L)
  id[c(997L, 1001L)] <- 999L
  datos <- rbind(
    data.frame(g = rep(c("A", "B"), 1000L), tipo = "persona", id = id,
               stringsAsFactors = FALSE),
    data.frame(g = rep(c("A", "B"), 100L), tipo = "empresa", id = 999999L,
               stringsAsFactors = FALSE)
  )
  ap <- list(id = ~ tipo == "persona")
  tipos <- c("faltantes_disfrazados", "desviacion_benford")
  entero <- perfilar(datos, aplicabilidad = ap, proteger_datos_personales = FALSE)
  expect_false(any(hallazgos_columna_r29e(entero$hallazgos, "id")$tipo_hallazgo %in%
                     tipos))
  # El grupo A solo, sin el puente, si acusa: es el hueco de la particion.
  solo_a <- perfilar(datos[datos$g == "A", c("tipo", "id")], aplicabilidad = ap,
                     proteger_datos_personales = FALSE)
  expect_true(all(tipos %in%
                    hallazgos_columna_r29e(solo_a$hallazgos, "id")$tipo_hallazgo))
  por <- perfilar_por(datos, por = "g", min_filas = 1, aplicabilidad = ap,
                      proteger_datos_personales = FALSE)
  expect_false(any(hallazgos_columna_r29e(por, "id")$tipo_hallazgo %in% tipos))
  cobertura <- as.data.frame(attr(por, "cobertura_diagnosticos"))
  movidas <- cobertura[cobertura$grupo == "A" & cobertura$columna == "id", ]
  expect_true(all(c("centinelas_numericos", "ley_benford") %in%
                    movidas$diagnostico))
})

test_that("H2: perfilar_por() no calla como de la particion lo que la columna entera acusa", {
  # La numeracion es densa, pero el 999 repetido 50 veces es su moda
  # sobresaliente: la guarda de la tabla entera se reabre y `perfilar()` lo
  # publica.
  denso <- data.frame(
    egreso = rep(c("Si", "No"), each = 1000L),
    id = c(seq_len(950L), rep(999L, 50L), 1000L + seq_len(1000L)),
    stringsAsFactors = FALSE
  )
  entero <- perfilar(denso["id"])
  expect_equal(entero$columnas$n_faltantes_disfrazados, sum(denso$id == 999L))
  por <- perfilar_por(denso, por = "egreso", min_filas = 1)
  h <- hallazgos_columna_r29e(por, "id", "Si")
  disfrazados <- h[h$tipo_hallazgo == "faltantes_disfrazados", ]
  expect_equal(nrow(disfrazados), 1L)
  expect_equal(disfrazados$n_afectados, 50)
  # Y un candidato fuera del rango reabre la guarda igual: 1..94 con ocho -9.
  # La tabla entera acusa los 8; los grupos los publican, 4 y 4.
  menos9 <- data.frame(
    x = c(1:60, rep(-9L, 4L), 61:94, rep(-9L, 4L)),
    g = rep(c("A", "B"), c(64L, 38L))
  )
  entero <- perfilar(menos9["x"], proteger_datos_personales = FALSE)
  expect_equal(entero$columnas$n_faltantes_disfrazados, sum(menos9$x == -9L))
  por <- as.data.frame(perfilar_por(menos9, "g",
                                    proteger_datos_personales = FALSE))
  disfrazados <- por[por$tipo_hallazgo == "faltantes_disfrazados", ]
  expect_equal(sum(disfrazados$n_afectados), 8)
})

divergencias_r29e <- function(resumen) {
  cb <- resumen$cobertura
  cb <- cb[cb$bloque == "corroboracion", , drop = FALSE]
  cb$elemento[cb$estado == "divergencia"]
}

test_that("H3: la corroboracion no declara divergencia entre dos textos iguales", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Texto con coma decimal: el perfil de muestra lo infiere doble, y la moda es
  # el texto "3,75" en los dos bloques.
  coma <- c(rep("3,75", 4), "1,5", "2,25", "8", "9,5")
  DBI::dbWriteTable(con, "t", data.frame(coma = coma, stringsAsFactors = FALSE))
  for (bf in list(NULL, 3L)) {
    p <- suppressWarnings(perfilar_dbi(con, "t", bloque_filas = bf,
                                       proteger_datos_personales = FALSE))
    expect_identical(p$resumen_tabla$columnas$moda, "3,75")
    expect_identical(p$perfil_muestra$columnas$moda, "3,75")
    expect_length(divergencias_r29e(p$resumen_tabla), 0L)
  }
})

test_that("H3: con OutDec = \",\" la moda decimal no abre una divergencia nueva", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(v = c(rep(2.5, 5), 1:10 + 0.25)))
  for (marca in c(".", ",")) {
    op <- options(OutDec = marca)
    for (bf in list(NULL, 4L)) {
      p <- suppressWarnings(perfilar_dbi(con, "t", bloque_filas = bf,
                                         proteger_datos_personales = FALSE))
      expect_identical(p$resumen_tabla$columnas$moda, paste0("2", marca, "5"))
      expect_length(divergencias_r29e(p$resumen_tabla), 0L)
    }
    options(op)
  }
})

test_that("H4: la moda por bloques se escribe como en memoria y en el agregado", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- data.frame(
    chico = c(1e-20, 1e-20, 1, 2),
    micro = c(0.00001234, 0.00001234, 1, 2),
    grande = c(1e22, 1e22, 1, 2)
  )
  DBI::dbWriteTable(con, "t", datos)
  # A mano: notacion fija, sin exponente.
  esperadas <- c(chico = "0.00000000000000000001", micro = "0.00001234",
                 grande = "10000000000000000000000")
  memoria <- perfilar(datos, proteger_datos_personales = FALSE)
  agregado <- resumen_r29e(con, "t")
  bloques <- resumen_r29e(con, "t", 2L)
  for (columna in names(esperadas)) {
    expect_identical(memoria$columnas$moda[memoria$columnas$columna == columna],
                     esperadas[[columna]])
    expect_identical(fila_r29e(agregado, columna)$moda, esperadas[[columna]])
    expect_identical(fila_r29e(bloques, columna)$moda, esperadas[[columna]])
  }
  e <- as.data.frame(comparar_equivalencia(
    agregado$columnas, bloques$columnas, tolerancia = 1e-9
  ))
  modas <- e[e$campo == "moda", ]
  expect_true(all(modas$veredicto == "identico"))
})

test_that("H5: la moda se publica con las cifras que vuelven a su valor", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  suma <- 0.1 + 0.2
  datos <- data.frame(suma = c(suma, suma, 1, 2), tercio = c(1/3, 1/3, 1, 2),
                      tres = c(0.3, 0.3, 1, 2))
  DBI::dbWriteTable(con, "t", datos)
  # A mano: 0.1 + 0.2 necesita 17 cifras y 1/3 16; 0.3 vuelve con 15.
  expect_identical(sprintf("%.17g", suma), "0.30000000000000004")
  expect_false(as.numeric(sprintf("%.16g", suma)) == suma)
  expect_true(as.numeric(sprintf("%.16g", 1/3)) == 1/3)
  esperadas <- c(suma = "0.30000000000000004", tercio = "0.3333333333333333",
                 tres = "0.3")
  caminos <- list(
    memoria = perfilar(datos, proteger_datos_personales = FALSE)$columnas,
    agregado = resumen_r29e(con, "t")$columnas,
    bloques = resumen_r29e(con, "t", 2L)$columnas
  )
  for (camino in caminos) {
    for (columna in names(esperadas)) {
      moda <- camino$moda[camino$columna == columna]
      expect_identical(moda, esperadas[[columna]])
      expect_identical(as.numeric(moda), datos[[columna]][[1L]])
    }
  }
  # Y el comparador ya no da por identicas dos modas distintas.
  a <- perfilar(data.frame(v = c(0.3, 0.3, 5)), proteger_datos_personales = FALSE)
  b <- perfilar(data.frame(v = c(suma, suma, 5)), proteger_datos_personales = FALSE)
  e <- as.data.frame(comparar_equivalencia(a, b, tolerancia = 0))
  expect_false(e$veredicto[e$campo == "moda"] == "identico")
  # Con coma decimal, la misma precision con la marca de la sesion.
  op <- options(OutDec = ",")
  on.exit(options(op), add = TRUE)
  expect_identical(
    perfilar(data.frame(v = c(suma, suma, 5)),
             proteger_datos_personales = FALSE)$columnas$moda,
    "0,30000000000000004"
  )
})

test_that("H6: el camino por bloques declara su criterio y la corroboracion no llama divergencia a lo esperable", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ciudad <- c("Montevideo", "montevideo", "MONTEVIDEO", "Rivera", "Rivera",
              "salto", "Salto", "Paysandu", "paysandu", "Colonia")
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER, ciudad TEXT COLLATE NOCASE)")
  DBI::dbAppendTable(con, "t", data.frame(id = seq_along(ciudad), ciudad = ciudad))
  # A mano: por bytes 9 distintos y moda "Rivera" (2); sin distinguir caja 5 y
  # "montevideo" (3).
  expect_equal(length(unique(ciudad)), 9L)
  expect_equal(length(unique(tolower(ciudad))), 5L)
  agregado <- suppressWarnings(perfilar_dbi(con, "t",
                                            proteger_datos_personales = FALSE))
  rt <- agregado$resumen_tabla
  expect_equal(fila_r29e(rt, "ciudad")$n_distintos, 5)
  expect_equal(agregado$perfil_muestra$columnas$n_distintos[
    agregado$perfil_muestra$columnas$columna == "ciudad"], 9)
  # Lo que el criterio declara esperable no es divergencia, y no se calla.
  expect_length(divergencias_r29e(rt), 0L)
  cb <- rt$cobertura
  nc <- cb$elemento[cb$bloque == "corroboracion" & cb$estado == "no_corroborada"]
  expect_true(all(c("ciudad::n_distintos", "ciudad::moda",
                    "ciudad::frecuencia_moda") %in%
                    trimws(unlist(strsplit(nc, ",")))))
  bloques <- suppressWarnings(perfilar_dbi(con, "t", bloque_filas = 3L,
                                           proteger_datos_personales = FALSE))
  rb <- bloques$resumen_tabla
  expect_equal(fila_r29e(rb, "ciudad")$n_distintos, 9)
  expect_identical(fila_r29e(rb, "ciudad")$moda, "Rivera")
  expect_type(rb$meta$criterio_comparacion, "character")
  expect_type(rb$meta$criterio_moda, "character")
  expect_match(rb$meta$criterio_comparacion, "byte a byte")
  # La mediana de una columna de texto no aplica, como en el agregado.
  expect_identical(unname(estados_r29e(rb, "ciudad", "mediana")), "no_aplica")
  expect_identical(unname(estados_r29e(rt, "ciudad", "mediana")), "no_aplica")
  expect_length(divergencias_r29e(rb), 0L)
})

catalogo_pgss_r29e <- function(queryid, llamadas, escritos = 0, texto = NULL) {
  n <- length(queryid)
  if (is.null(texto)) {
    texto <- paste0("SELECT COUNT(DISTINCT c", seq_len(n), ") AS n FROM t")
  }
  data.frame(queryid = queryid, query = texto, calls = llamadas,
             temp_blks_read = 0, temp_blks_written = escritos,
             stringsAsFactors = FALSE)
}

# `pg_stat_statements` se simula en la consulta misma: lo que se mide es cuanto
# trabajo hace el paquete con lo que el motor devuelve.
instrumentar_con_r29e <- function(antes, despues) {
  lectura <- 0L
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection",
    .package = "lupa"
  )
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      lectura <<- lectura + 1L
      if (lectura == 1L) antes else despues
    },
    .package = "DBI"
  )
  conexion <- structure(list(), class = "PqConnection")
  presupuesto <- .presupuesto_dbi(Inf, instrumentar = TRUE)
  .iniciar_instrumentacion_derrame_dbi(conexion, presupuesto, exacto = TRUE)
  .finalizar_instrumentacion_derrame_dbi(conexion, presupuesto)
  presupuesto
}

test_that("H7: instrumentar normaliza solo lo que esta corrida movio, y una vez", {
  skip_if_not_installed("DBI")
  # Un catalogo de 300 consultas viejas y dos de esta corrida.
  viejas <- catalogo_pgss_r29e(1:300, rep(50, 300))
  nuevas <- catalogo_pgss_r29e(
    c(901, 902), c(1, 1), escritos = c(0, 4),
    texto = c("SELECT COUNT(DISTINCT \"a\") AS n FROM \"t\"",
              "SELECT \"b\" AS valor, COUNT(*) AS frecuencia FROM \"t\" LIMIT 1")
  )
  normalizar <- .normalizar_sql_derrame_dbi
  llamadas <- 0L
  testthat::local_mocked_bindings(
    .normalizar_sql_derrame_dbi = function(sql) {
      llamadas <<- llamadas + 1L
      normalizar(sql)
    },
    .package = "lupa"
  )
  presupuesto <- instrumentar_con_r29e(viejas, rbind(viejas, nuevas))
  publicado <- .publicar_derrame_dbi(presupuesto)
  expect_identical(publicado$estado, "medido")
  expect_identical(publicado$consultas_observadas, 2L)
  expect_identical(publicado$llamadas_en_ventana, 2)
  expect_identical(publicado$bloques_temporales_escritos, 4)
  # A mano: las dos consultas nuevas, no las 300 + 302 de las dos lecturas del
  # catalogo.
  expect_identical(llamadas, 2L)
  # Y el SQL de la corrida se normaliza una vez por fila, no una vez por
  # consulta medida.
  llamadas <- 0L
  textos <- c(nuevas$query, "SELECT 1")
  sql <- do.call(rbind, lapply(seq_along(textos), function(i) {
    .registro_sql_dbi(c("a", "b", "c")[[i]], "n_distintos", "calculado",
                      NA_character_, textos[[i]])
  }))
  sql <- .adjuntar_derrame_sql_dbi(sql, presupuesto$derrame)
  expect_identical(llamadas, 3L)
  expect_identical(sql$bloques_temporales_escritos, c(0, 4, NA))
})

test_that("H7: un queryid repetido en el catalogo no convierte su historia en la ventana", {
  skip_if_not_installed("DBI")
  # 77 es la misma consulta de otro usuario: aparece dos veces. 901 es de esta
  # corrida.
  antes <- catalogo_pgss_r29e(c(77, 77, 5), c(40, 30, 9))
  despues <- rbind(
    catalogo_pgss_r29e(c(77, 77, 5), c(40, 30, 9)),
    catalogo_pgss_r29e(901, 1, texto = "SELECT COUNT(DISTINCT \"z\") AS n FROM \"t\"")
  )
  publicado <- .publicar_derrame_dbi(instrumentar_con_r29e(antes, despues))
  # A mano: en la ventana hubo una llamada, la de 901.
  expect_identical(publicado$estado, "medido")
  expect_identical(publicado$consultas_observadas, 1L)
  expect_identical(publicado$llamadas_en_ventana, 1)
})

test_that("H7: la normalizacion del SQL no cambia", {
  # A mano: literales y numeros a $n en orden, identificadores entre comillas
  # intactos, espacios colapsados, sin el punto y coma final.
  expect_identical(
    .normalizar_sql_derrame_dbi(
      "SELECT  COUNT(DISTINCT \"x1\") AS n FROM \"t\" WHERE a = 'b''c' AND b > 1.5e3 AND c1 = -2;"
    ),
    "SELECT COUNT(DISTINCT \"x1\") AS n FROM \"t\" WHERE a = $1 AND b > $2 AND c1 = $3"
  )
  # Un numero mas largo que la ventana de busqueda sigue siendo uno solo.
  largo <- paste0("SELECT ", strrep("9", 70), "e+12, x", strrep("7", 3), " FROM t")
  expect_identical(.normalizar_sql_derrame_dbi(largo), "SELECT $1, x777 FROM t")
  expect_identical(.normalizar_sql_derrame_dbi(paste0(strrep("1", 62), "e+9")), "$1")
  expect_identical(.normalizar_sql_derrame_dbi("f(1,2) - .5"), "f($1,$2) - $3")
})

test_that("H8: lo que la proteccion tapa en resumen_tabla se declara, y el desvio sigue a perfilar()", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- conexion_sqlite_r29e()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  v <- c(10.5, 12, 15.25, 18, 11, 12)
  DBI::dbWriteTable(con, "t", data.frame(v = v))
  # El precedente: la misma columna protegida en memoria tapa moda, extremos,
  # media y mediana, y desde la ronda 32 (D9) tambien el desvio.
  memoria <- perfilar(data.frame(v = v), columnas_personales = "v")$columnas
  expect_identical(memoria$moda, "[valor protegido]")
  expect_true(all(is.na(unlist(memoria[c("minimo", "maximo", "media", "mediana")]))))
  expect_true(is.na(memoria$desvio))
  tapadas <- c("moda", "minimo", "maximo", "media", "mediana", "desvio")
  # Sin muestra no hay clasificacion y la proteccion por omision cierra la
  # columna: lo que no se publica no puede decir `calculado`.
  for (bf in list(NULL, 2L)) {
    rt <- suppressWarnings(perfilar_dbi(con, "t", bloque_filas = bf,
                                        bloque_muestra = "solo_agregados"))$resumen_tabla
    f <- fila_r29e(rt, "v")
    expect_identical(f$moda, "[valor protegido]")
    expect_true(all(is.na(unlist(f[c("minimo", "maximo", "media", "mediana")]))))
    expect_true(all(estados_r29e(rt, "v", tapadas) == "omitido_por_privacidad"))
    sql <- rt$sql[rt$sql$columna == "v" & rt$sql$metrica %in% tapadas, ]
    expect_true(all(!is.na(sql$motivo) & nzchar(sql$motivo)))
    expect_true(is.na(f$desvio))
    expect_identical(unname(estados_r29e(rt, "v", "desvio")),
                     "omitido_por_privacidad")
  }
})
