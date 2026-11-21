testthat::skip_on_cran()

# Fechas compactas, meses escritos, centinelas de ceros o nueves y numeracion
# desplazada: lo que el perfil ya sabe leer y lo que no debe confundir con un
# codigo.

fccn_hallazgos <- function(d, tipo, ...) {
  h <- as.data.frame(hallazgos(perfilar(d, ...)))
  h[h$tipo_hallazgo == tipo, , drop = FALSE]
}

fccn_filas <- function(h) h$trazabilidad[[1L]]$indices_fila

fccn_fechas <- function(n, formato = "%Y-%m-%d") {
  format(as.Date("2015-01-01") + (seq_len(n) * 37L) %% 3000L, formato)
}

fccn_compactas <- function(n = 300) {
  x <- as.integer(format(
    as.Date("2015-01-01") + (seq_len(n) * 37L) %% 3000L, "%Y%m%d"
  ))
  x[1:6] <- c(20220230L, 20221301L, 20220431L, 20230229L, 20220000L,
              20229999L)
  x
}

test_that("fecha_invalida ve las compactas imposibles de texto y de enteros", {
  compactas <- fccn_compactas()
  d <- data.frame(id = 1:300, f = sprintf("%08d", compactas),
                  stringsAsFactors = FALSE)
  h <- fccn_hallazgos(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 6)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(fccn_filas(h)), 1:6)

  e <- data.frame(id = 1:300, f = compactas)
  he <- fccn_hallazgos(e, "fecha_invalida")
  expect_equal(nrow(he), 1L)
  expect_equal(he$n_afectados, 6)
  expect_equal(he$n_evaluados, 300)
  expect_equal(sort(fccn_filas(he)), 1:6)
})

test_that("un codigo, un telefono o un monto de ocho cifras no son fechas", {
  set.seed(20261124)
  casos <- list(
    codigo = sample(10000000:99999999, 300),
    telefono = sample(20000000:29999999, 300),
    monto = sample(10000000:99999999, 300),
    id = 20220001:20220300
  )
  for (nombre in names(casos)) {
    d <- data.frame(x = casos[[nombre]])
    names(d) <- nombre
    expect_equal(
      nrow(fccn_hallazgos(d, "fecha_invalida")), 0L, info = nombre
    )
  }
})

test_that("fecha_invalida ve los meses escritos imposibles", {
  fechas <- as.Date("2015-01-01") + (seq_len(300) * 37L) %% 3000L
  meses <- month.name[as.integer(format(fechas, "%m"))]
  dias <- as.integer(format(fechas, "%d"))
  anios <- as.integer(format(fechas, "%Y"))

  es <- sprintf("%d de %s de %d", dias, meses, anios)
  es[1:3] <- c("30 de febrero de 2022", "31 de abril de 2022",
               "32 de enero de 2022")
  h <- fccn_hallazgos(data.frame(f = es, stringsAsFactors = FALSE),
                      "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 3)
  expect_equal(sort(fccn_filas(h)), 1:3)

  en <- sprintf("%s %d, %d", meses, dias, anios)
  en[1:3] <- c("February 30, 2022", "April 31, 2022", "January 32, 2022")
  he <- fccn_hallazgos(data.frame(f = en, stringsAsFactors = FALSE),
                       "fecha_invalida")
  expect_equal(nrow(he), 1L)
  expect_equal(sort(fccn_filas(he)), 1:3)

  abrev <- sprintf("%d-%s-%d", dias, substr(meses, 1L, 3L), anios)
  abrev[1:3] <- c("30-feb-2022", "31-abr-2022", "32-ene-2022")
  ha <- fccn_hallazgos(data.frame(f = abrev, stringsAsFactors = FALSE),
                       "fecha_invalida")
  expect_equal(nrow(ha), 1L)
  expect_equal(sort(fccn_filas(ha)), 1:3)

  espacio <- sprintf("%d %s %d", dias, substr(meses, 1L, 3L), anios)
  espacio[1:3] <- c("30 Feb 2022", "31 Apr 2022", "32 Jan 2022")
  hs <- fccn_hallazgos(data.frame(f = espacio, stringsAsFactors = FALSE),
                       "fecha_invalida")
  expect_equal(nrow(hs), 1L)
  expect_equal(sort(fccn_filas(hs)), 1:3)

  mayus <- toupper(es)
  mayus[1:3] <- c("30 FEBRERO 2022", "31 ABRIL 2022", "32 ENERO 2022")
  hm <- fccn_hallazgos(data.frame(f = mayus, stringsAsFactors = FALSE),
                       "fecha_invalida")
  expect_equal(nrow(hm), 1L)
  expect_equal(sort(fccn_filas(hm)), 1:3)
})

test_that("la abreviatura inglesa sept tambien se lee como septiembre", {
  x <- sprintf(
    "%d Sept %d", rep(1:28, length.out = 300), rep(2020:2024, length.out = 300)
  )
  x[1] <- "31 Sept 2022"
  h <- fccn_hallazgos(data.frame(f = x, stringsAsFactors = FALSE),
                      "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(fccn_filas(h), 1L)
})

test_that("los meses escritos validos, con setiembre y sept, no se acusan", {
  d <- data.frame(
    id = 1:12,
    es = c(
      "15 de enero de 2024", "15 de febrero de 2024", "15 de marzo de 2024",
      "15 de abril de 2024", "15 de mayo de 2024", "15 de junio de 2024",
      "15 de julio de 2024", "15 de agosto de 2024",
      "15 de setiembre de 2024", "15 de septiembre de 2024",
      "15 de octubre de 2024", "15 de diciembre de 2024"
    ),
    en = c(
      "15 Jan 2024", "15 Feb 2024", "15 Mar 2024", "15 Apr 2024",
      "15 May 2024", "15 Jun 2024", "15 Jul 2024", "15 Aug 2024",
      "15 Sep 2024", "15 Sept 2024", "15 Oct 2024", "15 Dec 2024"
    ),
    punto = c(
      "15. ene. 2024", "15. feb. 2024", "15. mar. 2024", "15. abr. 2024",
      "15. may. 2024", "15. jun. 2024", "15. jul. 2024", "15. ago. 2024",
      "15. sep. 2024", "15. sept. 2024", "15. oct. 2024", "15. dic. 2024"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(fccn_hallazgos(d, "fecha_invalida")), 0L)
})

test_that("un centinela de ceros o nueves repetido es faltante, no fecha mala", {
  f <- fccn_fechas(300)
  f[1:4] <- c("00-00-00", "00-00-00", "99-99-99", "99-99-99")
  d <- data.frame(id = 1:300, f = f, stringsAsFactors = FALSE)
  h <- fccn_hallazgos(d, "faltantes_disfrazados")
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 4)
  expect_equal(as.character(h$severidad), "error")
  expect_equal(sort(fccn_filas(h)), 1:4)
  expect_equal(nrow(fccn_hallazgos(d, "fecha_invalida")), 0L)

  g <- fccn_fechas(300)
  g[1:6] <- c(rep("0000-00-00", 3), rep("9999-99-99", 3))
  e <- data.frame(id = 1:300, f = g, stringsAsFactors = FALSE)
  he <- fccn_hallazgos(e, "faltantes_disfrazados")
  expect_equal(he$n_afectados, 6)
  expect_equal(nrow(fccn_hallazgos(e, "fecha_invalida")), 0L)

  compactas <- sprintf(
    "%08d", as.integer(format(
      as.Date("2015-01-01") + (seq_len(300) * 37L) %% 3000L, "%Y%m%d"
    ))
  )
  compactas[1:2] <- "00000000"
  cd <- data.frame(id = 1:300, f = compactas, stringsAsFactors = FALSE)
  hc <- fccn_hallazgos(cd, "faltantes_disfrazados")
  expect_equal(hc$n_afectados, 2)
  expect_equal(nrow(fccn_hallazgos(cd, "fecha_invalida")), 0L)
})

test_that("un centinela suelto y una fecha imposible suelta siguen acusados", {
  f <- fccn_fechas(300)
  f[1:3] <- c("0000-00-00", "9999-99-99", "2022-02-30")
  d <- data.frame(id = 1:300, f = f, stringsAsFactors = FALSE)
  expect_equal(nrow(fccn_hallazgos(d, "faltantes_disfrazados")), 0L)
  h <- fccn_hallazgos(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(sort(fccn_filas(h)), 1:3)
})

test_that("un centinela de fecha en una columna que no es fecha no es faltante", {
  d <- data.frame(
    id = 1:300,
    codigo = rep(c("00-00-00", "99-99-99", "A1", "B2", "C3"), 60),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(fccn_hallazgos(d, "faltantes_disfrazados")), 0L)
})

test_that("una clave con ausentes se acusa aunque sus valores sean una numeracion", {
  n <- 10000
  casos <- list(
    desplazada = 2:(n + 1),
    escalada = as.numeric(seq_len(n)) * 2,
    paso_tres = as.numeric(seq_len(n)) * 3
  )
  for (nombre in names(casos)) {
    d <- data.frame(id = replace(casos[[nombre]], 1:5, NA_real_))
    expect_equal(
      nrow(fccn_hallazgos(d, "identificador_con_ausentes")), 1L,
      info = nombre
    )
  }
  # El nombre es el que decide: un conteo 1..n con ausentes sigue afuera.
  conteo <- data.frame(conteo = replace(seq_len(n), 1:5, NA_integer_))
  expect_equal(
    nrow(fccn_hallazgos(conteo, "identificador_con_ausentes")), 0L
  )
})

test_that("una clave desplazada y una progresion con un valor fuera de escala se acusan", {
  d <- data.frame(id = replace(1001:1500, 1:5, NA_integer_))
  expect_equal(nrow(fccn_hallazgos(d, "identificador_con_ausentes")), 1L)

  e <- data.frame(id = replace(c(1:99, 1000), 1:5, NA_integer_))
  expect_equal(nrow(fccn_hallazgos(e, "identificador_con_ausentes")), 1L)
})

test_that("perfilar_por mide el centinela repetido en la columna entera", {
  f <- fccn_fechas(300)
  f[1] <- "00-00-00"
  f[151] <- "00-00-00"
  d <- data.frame(
    g = rep(c("a", "b"), each = 150), f = f, stringsAsFactors = FALSE
  )
  pp <- as.data.frame(perfilar_por(
    d, por = "g", fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  ))
  h <- pp[pp$tipo_hallazgo == "faltantes_disfrazados", , drop = FALSE]
  expect_equal(nrow(h), 2L)
  expect_equal(sum(h$n_afectados), 2)
  expect_equal(
    nrow(pp[pp$tipo_hallazgo == "fecha_invalida", , drop = FALSE]), 0L
  )
})

test_that("perfilar_por aplica las cuatro reglas por grupo", {
  fechas <- fccn_fechas(600)
  compactas <- fccn_compactas(600)
  fechas[1:2] <- c("00-00-00", "00-00-00")
  fechas[3:4] <- c("2022-02-30", "2022-13-01")
  compactas[1:2] <- c(20220230L, 20221301L)
  d <- data.frame(
    g = rep(c("a", "b"), each = 300),
    id = c(replace(2:301, 1:3, NA_integer_), 1:300),
    f = fechas,
    compacta = compactas,
    stringsAsFactors = FALSE
  )
  pp <- as.data.frame(perfilar_por(d, por = "g", fecha = as.POSIXct(
    "2026-01-01", tz = "UTC"
  ), analizar_dependencias = FALSE))
  expect_equal(nrow(pp[pp$grupo == "a" & pp$tipo_hallazgo ==
                         "faltantes_disfrazados", , drop = FALSE]), 1L)
  expect_equal(nrow(pp[pp$grupo == "a" & pp$tipo_hallazgo ==
                         "fecha_invalida", , drop = FALSE]), 2L)
  expect_equal(nrow(pp[pp$grupo == "b" & pp$tipo_hallazgo %in%
                         c("faltantes_disfrazados", "fecha_invalida"), ,
                       drop = FALSE]), 0L)
})

test_that("analizar incluye las cuatro reglas en el perfil", {
  compactas <- fccn_compactas(300)
  f <- fccn_fechas(300)
  f[1:2] <- c("00-00-00", "00-00-00")
  f[3] <- "2022-02-30"
  d <- data.frame(
    id = replace(2:301, 1:3, NA_integer_),
    compacta = compactas,
    f = f,
    stringsAsFactors = FALSE
  )
  r <- analizar(
    d, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  h <- as.data.frame(r$perfil$hallazgos)
  expect_true("fecha_invalida" %in% h$tipo_hallazgo)
  expect_true("faltantes_disfrazados" %in% h$tipo_hallazgo)
  # La clave `id` con una numeracion desplazada y ausentes ahora se informa:
  # el nombre manda, no el maximo.
  expect_true("identificador_con_ausentes" %in% h$tipo_hallazgo)
})

test_that("perfilar_dbi publica las cuatro reglas en SQLite y DuckDB", {
  skip_if_not_installed("DBI")
  compactas <- fccn_compactas(300)
  f <- fccn_fechas(300)
  f[1:2] <- c("00-00-00", "00-00-00")
  f[3] <- "2022-02-30"
  d <- data.frame(
    id = replace(2:301, 1:3, NA_integer_),
    compacta = compactas,
    f = f,
    stringsAsFactors = FALSE
  )

  revisar <- function(con) {
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "t", d)
    h <- as.data.frame(hallazgos(perfilar_dbi(con, "t")))
    expect_true("fecha_invalida" %in% h$tipo_hallazgo)
    expect_true("faltantes_disfrazados" %in% h$tipo_hallazgo)
    expect_true("identificador_con_ausentes" %in% h$tipo_hallazgo)
    fi <- h[h$tipo_hallazgo == "fecha_invalida" & h$columna == "f", ,
            drop = FALSE]
    expect_equal(sort(fi$trazabilidad[[1L]]$indices_fila), 3L)
  }

  revisiones <- 0L
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    revisar(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
    revisiones <- revisiones + 1L
  }
  if (requireNamespace("duckdb", quietly = TRUE) &&
      exists(".duckdb_prueba", mode = "function")) {
    con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
    on.exit(try(duckdb::duckdb_shutdown(con), silent = TRUE), add = TRUE)
    revisar(con)
    revisiones <- revisiones + 1L
  }
  expect_gte(revisiones, 1L)
})
