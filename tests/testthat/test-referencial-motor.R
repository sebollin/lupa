# Un referencial que vive en el motor: `referencial(DBI::Id(...))` no copia la
# tabla a R, y `medir_dbi()` mide las tres metricas referenciales contando lo
# mismo que `medir()` sobre las dos tablas leidas con `dbReadTable()`.

testthat::skip_on_cran()

fecha_ref <- as.POSIXct("2026-01-01", tz = "UTC")
campos_ref <- c(
  "metrica", "resultado", "numerador", "denominador", "n_universo",
  "n_no_evaluados", "motivo_no_evaluados", "objeto_medible", "agregacion",
  "granularidad"
)

conexiones_ref <- function() {
  motores <- list()
  if (requireNamespace("RSQLite", quietly = TRUE)) motores$sqlite <- function() {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  if (requireNamespace("duckdb", quietly = TRUE)) motores$duckdb <- function() {
    DBI::dbConnect(duckdb::duckdb(), ":memory:")
  }
  motores
}

# Lo que da el motor y lo que da R con las tablas leidas.
par_ref <- function(con, ent, atributos, ref, columnas, metrica,
                    normalizar = TRUE, estrategia = "no_medible") {
  m <- metricas_referencial()
  debil <- identical(metrica, "CorrectitudSemDebil")
  clave <- if (debil) columnas[[1L]] else columnas
  valor <- if (debil) columnas[-1L] else character()
  en_motor <- referencial(DBI::Id(table = ref), clave, valor, completo = TRUE,
                          alcance = "prueba", normalizar = normalizar,
                          nombre = "r")
  en_r <- referencial(DBI::dbReadTable(con, ref), clave, valor, completo = TRUE,
                      alcance = "prueba", normalizar = normalizar, nombre = "r")
  i_motor <- instanciar(especializar(m[[metrica]], "X"), ent, atributos,
                        referencial = en_motor)
  i_r <- instanciar(especializar(m[[metrica]], "X"), ent, atributos,
                    referencial = en_r)
  motor <- suppressWarnings(medir_dbi(
    modelo(i_motor), con, fecha = fecha_ref, id_medicion = "m",
    estrategia_no_sql = estrategia
  ))
  memoria <- suppressWarnings(medir(
    modelo(i_r), stats::setNames(list(DBI::dbReadTable(con, ent)), ent),
    detalle = "resumen", fecha = fecha_ref, id_medicion = "m"
  ))
  list(motor = motor, memoria = memoria,
       via = unname(attr(motor, "ejecucion_dbi")$vias[[1L]]))
}

filas_ref <- function(x) {
  x <- as.data.frame(x)[, campos_ref]
  rownames(x) <- NULL
  x
}

escribir_casos_ref <- function(con, motor) {
  w <- function(n, d) DBI::dbWriteTable(con, n, d, overwrite = TRUE)
  w("ref_txt", data.frame(k = c("NC", "PC", "SC", "AR"), v = c("n", "p", "s", NA)))
  w("ent_txt", data.frame(k = c("NC", "nc", "PC", "XX", NA, "AR", "AR"),
                          v = c("n", "n", "q", "x", "p", NA, "z")))
  w("ref_int", data.frame(k = c(1L, 2L, 3L)))
  w("ent_num", data.frame(k = c(1, 2, 2, 9, NA)))
  w("ent_t2i", data.frame(k = c("1", "01", "2", "2", NA)))
  w("ref_comp", data.frame(a = c(1L, 1L, 2L), b = c("x", "y", "x")))
  w("ent_comp", data.frame(a = c(1L, 1L, NA, 2L, 3L), b = c("x", "z", "x", NA, "x")))
  w("ref_Aa", data.frame(k = c("A", "a", "b")))
  w("ent_Aa", data.frame(k = "a"))
  w("ent_esp", data.frame(k = c("NC", " NC", "N C", "PC")))
  w("ref_etq", data.frame(k = c("1", "2"), v = c("Sin dato", "No sabe")))
  w("ent_etq", data.frame(k = c("1", "2", "1"), v = c("SIN DATO", "No sabe", "otro")))
  jose <- paste0("Jos", intToUtf8(0xE9))
  w("ref_acento", data.frame(k = c(jose, "Ana")))
  w("ent_acento", data.frame(k = c("JOSE", jose, "ana", "zz")))
  invisible(DBI::dbExecute(con, "CREATE TABLE ent_base (k TEXT)"))
  invisible(DBI::dbExecute(con, "INSERT INTO ent_base VALUES ('A'), ('a'), ('b'), ('c')"))
  invisible(DBI::dbExecute(con, "CREATE VIEW vista_ent AS SELECT k AS k FROM ent_base"))
}

test_that("las tres metricas cuentan en el motor lo que cuenta R", {
  for (nombre in names(conexiones_ref())) {
    con <- conexiones_ref()[[nombre]]()
    escribir_casos_ref(con, nombre)
    casos <- list(
      list("ent_txt", "k", "ref_txt", "k"),
      list("ent_num", "k", "ref_int", "k"),
      list("ent_t2i", "k", "ref_int", "k"),
      list("ent_comp", c("a", "b"), "ref_comp", c("a", "b")),
      list("ent_Aa", "k", "ref_Aa", "k"),
      list("vista_ent", "k", "ref_txt", "k")
    )
    for (normalizar in list(TRUE, FALSE, "amplio")) {
      for (metrica in c("CorrectitudSemFuerte", "RatioCobertura")) {
        for (caso in casos) {
          r <- par_ref(con, caso[[1]], caso[[2]], caso[[3]], caso[[4]], metrica,
                       normalizar = normalizar)
          etiqueta <- paste(nombre, caso[[1]], metrica, format(normalizar))
          expect_identical(r$via, "sql_dedicado_referencial", label = etiqueta)
          expect_equal(filas_ref(r$motor), filas_ref(r$memoria), label = etiqueta)
        }
      }
      for (caso in list(list("ent_txt", c("k", "v"), "ref_txt", c("k", "v")),
                        list("ent_etq", c("k", "v"), "ref_etq", c("k", "v")))) {
        r <- par_ref(con, caso[[1]], caso[[2]], caso[[3]], caso[[4]],
                     "CorrectitudSemDebil", normalizar = normalizar)
        etiqueta <- paste(nombre, caso[[1]], "debil", format(normalizar))
        expect_identical(r$via, "sql_dedicado_referencial", label = etiqueta)
        expect_equal(filas_ref(r$motor), filas_ref(r$memoria), label = etiqueta)
      }
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("'01' no casa con 1 ni en R ni en el motor; '1' si", {
  for (nombre in names(conexiones_ref())) {
    con <- conexiones_ref()[[nombre]]()
    escribir_casos_ref(con, nombre)
    r <- par_ref(con, "ent_t2i", "k", "ref_int", "k", "CorrectitudSemFuerte",
                 normalizar = FALSE)
    # "1", "2", "2" casan; "01" no.
    expect_identical(r$motor$numerador, 3)
    expect_identical(r$motor$denominador, 4)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("lo que la guarda aparta se declara, y con bloques se compara en R", {
  for (nombre in names(conexiones_ref())) {
    con <- conexiones_ref()[[nombre]]()
    escribir_casos_ref(con, nombre)
    for (caso in list(list("ent_esp", "ref_txt"), list("ent_acento", "ref_acento"))) {
      for (metrica in c("CorrectitudSemFuerte", "RatioCobertura")) {
        sin <- par_ref(con, caso[[1]], "k", caso[[2]], "k", metrica)
        expect_identical(nrow(sin$motor), 0L)
        cob <- attr(sin$motor, "cobertura_metricas")
        expect_identical(cob$estado, "no_medible")
        expect_match(cob$motivo, "normalizaci", fixed = TRUE)
        con_bloques <- par_ref(con, caso[[1]], "k", caso[[2]], "k", metrica,
                               estrategia = "bloques")
        expect_identical(con_bloques$via, "distintos_r")
        expect_equal(filas_ref(con_bloques$motor), filas_ref(con_bloques$memoria))
      }
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("un NaN de la foranea es ausente, como para R", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE ent_nan AS SELECT * FROM",
    "(VALUES (1.0), (3.0), ('NaN'::DOUBLE), (NULL)) t(k)"
  ))
  DBI::dbWriteTable(con, "ref_dbl", data.frame(k = c(1, 2)))
  for (metrica in c("CorrectitudSemFuerte", "RatioCobertura")) {
    r <- par_ref(con, "ent_nan", "k", "ref_dbl", "k", metrica)
    expect_equal(filas_ref(r$motor), filas_ref(r$memoria))
  }
  r <- par_ref(con, "ent_nan", "k", "ref_dbl", "k", "CorrectitudSemFuerte")
  expect_identical(r$motor$denominador, 2)
})

test_that("la clave del referencial se verifica en cada corrida con las frases de R", {
  for (nombre in names(conexiones_ref())) {
    con <- conexiones_ref()[[nombre]]()
    DBI::dbWriteTable(con, "ent", data.frame(k = c("a", "b")))
    DBI::dbWriteTable(con, "dup", data.frame(k = c("a", "a", "b")))
    DBI::dbWriteTable(con, "nulos", data.frame(k = c("a", NA)))
    DBI::dbWriteTable(con, "mayus", data.frame(k = c("A", "a")))
    DBI::dbWriteTable(con, "vacio", data.frame(k = character()))
    m <- metricas_referencial()
    medir_con <- function(tabla, metrica = "CorrectitudSemFuerte") {
      ref <- referencial(tabla, "k", completo = TRUE, alcance = "p")
      suppressWarnings(medir_dbi(modelo(instanciar(
        especializar(m[[metrica]], "X"), "ent", "k", referencial = ref
      )), con))
    }
    expect_match(
      attr(medir_con("dup"), "cobertura_metricas")$motivo,
      "identificar un\u00edvocamente", fixed = TRUE
    )
    expect_match(
      attr(medir_con("nulos"), "cobertura_metricas")$motivo,
      "no puede contener valores ausentes", fixed = TRUE
    )
    # `A` y `a` son dos claves para `referencial()`: se aceptan.
    expect_identical(nrow(medir_con("mayus")), 1L)
    expect_identical(
      attr(medir_con("vacio", "RatioCobertura"), "cobertura_metricas")$estado,
      "sin_valores"
    )
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("el plan cuenta las consultas que se emiten y compila las referenciales", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "dep", data.frame(
    codigo = c(1L, 2L, 99L), nombre = c("Norte", "Sur", "Sin dato")
  ))
  DBI::dbWriteTable(con, "personas", data.frame(
    dep = c(1L, 2L, 2L, 7L, NA), nom = c("Norte", "Sur", "x", "y", NA)
  ))
  ref <- referencial(DBI::Id(table = "dep"), "codigo", "nombre",
                     completo = TRUE, alcance = "departamentos")
  m <- metricas_referencial()
  mod <- modelo(
    instanciar(especializar(m$CorrectitudSemFuerte, "F"), "personas", "dep", referencial = ref),
    instanciar(especializar(m$CorrectitudSemDebil, "D"), "personas", c("dep", "nom"), referencial = ref),
    instanciar(especializar(m$RatioCobertura, "R"), "personas", "dep", referencial = ref)
  )
  plan <- plan_medicion_dbi(mod, con)
  medicion <- medir_dbi(mod, con)
  expect_identical(
    sum(attr(plan, "consultas")), attr(medicion, "ejecucion_dbi")$consultas
  )
  expect_identical(nrow(medicion), 3L)
  compilado <- plan_medicion_dbi(mod, con, compilar = TRUE)
  etapas <- attr(compilado, "compilacion")$etapa
  # Una validacion compartida y una consulta por metrica.
  expect_identical(sum(etapas == "referencial"), 4L)
})

test_that("en memoria se abstiene, se imprime sin leer y la huella nombra la tabla", {
  ref <- referencial(DBI::Id(schema = "main", table = "dep"), "k",
                     completo = TRUE, alcance = "p")
  expect_null(ref$datos)
  expect_identical(ref$nombre, "dep")
  salida <- utils::capture.output(print(ref))
  expect_true(any(grepl("main.dep", salida, fixed = TRUE)))
  m <- metricas_referencial()
  medicion <- suppressWarnings(medir(
    modelo(instanciar(especializar(m$CorrectitudSemFuerte), "d", "k", referencial = ref)),
    list(d = data.frame(k = "a")), detalle = "resumen"
  ))
  expect_identical(nrow(medicion), 0L)
  expect_identical(attr(medicion, "cobertura_metricas")$estado, "no_medible")
  otra <- referencial(DBI::Id(schema = "main", table = "otra"), "k",
                      completo = TRUE, alcance = "p")
  huella <- function(r) lupa:::.referencial_para_huella(r, lupa:::.contexto_huella())
  expect_false(identical(huella(ref), huella(otra)))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(ref, archivo, version = 2)
  expect_identical(readRDS(archivo), ref)
  expect_error(referencial(c("a", "b"), "k"), "nombre de una tabla")
})

test_that("RatioCobertura publica numerador y denominador en memoria", {
  ref <- referencial(data.frame(k = c("A", "a", "b")), "k", completo = TRUE,
                     alcance = "p")
  m <- metricas_referencial()
  r <- medir(modelo(instanciar(especializar(m$RatioCobertura), "d", "k",
                               referencial = ref)),
             list(d = data.frame(k = "a")), detalle = "resumen")
  # Con el perfil por omision `A` y `a` son una clave: 1 de 2.
  expect_identical(r$numerador, 1)
  expect_identical(r$denominador, 2)
  expect_identical(r$resultado, r$numerador / r$denominador)
})
