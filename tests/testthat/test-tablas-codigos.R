# `detectar_tablas_codigos()` encuentra las tablas de codigos de una coleccion,
# lee sus etiquetas y propone -sin aplicar nada- que codigos declarar como
# ausencia en cada columna que los usa, con cuantas filas lo usan.

testthat::skip_on_cran()

coleccion_codigos <- function(motor = "sqlite") {
  con <- if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else DBI::dbConnect(duckdb::duckdb(), ":memory:")
  x <- function(s) invisible(DBI::dbExecute(con, s))
  if (identical(motor, "sqlite")) x("PRAGMA foreign_keys = ON")
  x("CREATE TABLE departamentos (codigo INTEGER PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO departamentos VALUES (1, 'Norte'), (2, 'Sur'), (99, 'Sin dato')")
  x("CREATE TABLE sexos (id INTEGER PRIMARY KEY, codigo TEXT UNIQUE, nombre TEXT)")
  x("INSERT INTO sexos VALUES (1, 'M', 'Mujer'), (2, 'V', 'Varon'), (3, 'I', 'Ignorado')")
  x("CREATE TABLE condicion (cod INTEGER, etiqueta TEXT, descripcion TEXT)")
  x(paste0(
    "INSERT INTO condicion VALUES (1, 'Ocupado', 'a'), (2, 'Ninguno', 'b'), ",
    "(8, 'No corresponde', 'c'), (9, 'No sabe / no responde', 'd')"
  ))
  x("CREATE TABLE nivel (cod TEXT PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO nivel VALUES ('NC', 'Nivel completo'), ('PC', 'Primaria'), ('SD', 'Sin dato')")
  x("CREATE TABLE suelta (cod INTEGER PRIMARY KEY, nombre TEXT)")
  x("INSERT INTO suelta VALUES (1, 'Sin dato'), (2, 'Otro')")
  x(paste(
    "CREATE TABLE personas (id INTEGER PRIMARY KEY,",
    "dep INTEGER REFERENCES departamentos(codigo), sexo TEXT, cond INTEGER,",
    "cond_txt TEXT, niv TEXT REFERENCES nivel(cod))"
  ))
  set.seed(5)
  n <- 1500
  p <- data.frame(
    id = seq_len(n), dep = sample(c(1, 2, 99), n, TRUE),
    sexo = sample(c("M", "V", "I"), n, TRUE),
    cond = sample(c(1, 2, 8, 9), n, TRUE),
    cond_txt = sample(c("1", "2", "8", "9", "09"), n, TRUE),
    niv = sample(c("NC", "PC", "SD"), n, TRUE), stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, "personas", p)
  col <- coleccion(con, c("departamentos", "sexos", "condicion", "nivel", "suelta", "personas"))
  list(con = con, col = col, datos = p)
}

test_that("las tablas de codigos salen con su uso y la propuesta con su evidencia", {
  for (motor in c("sqlite", if (requireNamespace("duckdb", quietly = TRUE)) "duckdb")) {
    caso <- coleccion_codigos(motor)
    pc <- suppressWarnings(perfilar_coleccion(caso$col))
    rel <- relaciones_coleccion(
      caso$col,
      pares = data.frame(tabla_1 = c("sexos", "condicion"), tabla_2 = "personas"),
      columnas_candidatas = list(
        sexos = c("id", "codigo"), condicion = "cod",
        personas = c("sexo", "cond", "cond_txt")
      ),
      # `cond_txt` tiene huerfanos (`09`): una cobertura de 0,8.
      umbral_cobertura = 0.75
    )
    # La misma cobertura minima que la relacion medida: `cond_txt` tiene 0,8.
    tc <- detectar_tablas_codigos(caso$col, pc, relaciones = rel, umbral_cobertura = 0.75)
    expect_s3_class(tc, "tablas_codigos")
    estado <- stats::setNames(tc$tablas$estado, tc$tablas$tabla)
    expect_identical(unname(estado[c("departamentos", "sexos", "condicion", "nivel")]),
                     rep("candidata", 4L))
    expect_identical(unname(estado["suelta"]), "posible")
    # La tabla grande no es de codigos.
    expect_false("personas" %in% tc$tablas$tabla)
    # Con dos claves (`id` y `codigo`), el codigo es la que el uso apunta.
    expect_identical(tc$tablas$columna_codigo[tc$tablas$tabla == "sexos"], "codigo")
    pr <- as.data.frame(tc$propuesta)
    fila <- function(codigo, columna) pr[pr$codigo == codigo & pr$columna %in% columna, , drop = FALSE]
    p <- caso$datos
    expect_identical(fila("99", "dep")$n_filas, as.numeric(sum(p$dep == 99)))
    expect_identical(fila("99", "dep")$argumento, "sentinelas_numericos")
    expect_identical(fila("I", "sexo")$n_filas, as.numeric(sum(p$sexo == "I")))
    expect_identical(fila("I", "sexo")$argumento, "cadenas_ausencia")
    # La misma tabla de codigos usada por una columna entera y una de texto: una
    # fila para cada una, con su argumento. El texto "09" no es el codigo 9.
    expect_identical(fila("9", "cond")$argumento, "sentinelas_numericos")
    expect_identical(fila("9", "cond_txt")$argumento, "cadenas_ausencia")
    expect_identical(fila("9", "cond_txt")$n_filas, as.numeric(sum(p$cond_txt == "9")))
    expect_identical(fila("9", "cond")$coincidencia, "compuesta")
    expect_identical(fila("8", "cond")$semantica, "no_corresponde")
    # `Ninguno` y `Ocupado` son categorias: no se proponen.
    expect_identical(nrow(fila("2", c("cond", "cond_txt"))), 0L)
    # `NC` = "Nivel completo" es valido y la lista incorporada lo cuenta.
    nc <- fila("NC", "niv")
    expect_identical(nc$semantica, "codigo_valido_contado")
    expect_true(is.na(nc$incluir))
    # Ninguna propuesta nace incluida.
    expect_false(any(pr$incluir %in% TRUE))
    expect_true(nrow(tc$meta$pares_sugeridos) > 0L)
    DBI::dbDisconnect(caso$con, shutdown = TRUE)
  }
})

test_that("la declaracion confirmada cuenta en el perfil lo que la propuesta dijo", {
  caso <- coleccion_codigos()
  on.exit(DBI::dbDisconnect(caso$con), add = TRUE)
  pc <- suppressWarnings(perfilar_coleccion(caso$col))
  tc <- detectar_tablas_codigos(caso$col, pc)
  pr <- tc$propuesta
  pr$incluir[pr$incluir %in% FALSE & pr$columna == "dep"] <- TRUE
  decl <- ausencias_desde_propuesta(pr)
  expect_identical(decl, list(sentinelas_numericos = list(personas = list(dep = 99))))
  antes <- suppressWarnings(perfilar_coleccion(caso$col, conservar_perfiles = TRUE))
  despues <- suppressWarnings(do.call(
    perfilar_coleccion, c(list(caso$col, conservar_perfiles = TRUE), decl)
  ))
  disfrazados <- function(x, columna) {
    m <- x$perfiles$personas$perfil_muestra$columnas
    m$n_faltantes_disfrazados[m$columna == columna]
  }
  expect_identical(
    as.numeric(disfrazados(despues, "dep") - disfrazados(antes, "dep")),
    pr$n_filas[pr$columna %in% "dep"]
  )
  expect_identical(disfrazados(despues, "sexo"), disfrazados(antes, "sexo"))
  # Para una tabla, las listas por columna.
  expect_identical(
    ausencias_desde_propuesta(pr, tabla = "personas"),
    list(sentinelas_numericos = list(dep = 99))
  )
})

test_that("ausencias_desde_propuesta() no declara lo que no se incluyo ni lo que no tiene destino", {
  caso <- coleccion_codigos()
  on.exit(DBI::dbDisconnect(caso$con), add = TRUE)
  tc <- detectar_tablas_codigos(caso$col, suppressWarnings(perfilar_coleccion(caso$col)))
  expect_warning(vacia <- ausencias_desde_propuesta(tc), "incluir")
  expect_identical(vacia, stats::setNames(list(), character()))
  pr <- tc$propuesta
  pr$incluir[pr$codigo == "NC"] <- TRUE
  pr$argumento[pr$codigo == "NC"] <- NA_character_
  expect_error(ausencias_desde_propuesta(pr), "no tiene d")
})

test_that("las etiquetas se clasifican sin mayusculas, acentos ni prefijos de codigo", {
  lexico <- lupa:::.lexico_etiquetas_ausencia("rechazado")
  etiquetas <- c(
    "Sin dato", paste0("SIN INFORMACI", intToUtf8(0xD3), "N"), "99 - Ignorado",
    "No sabe / No responde", "Unknown", "Not applicable", "Otro / sin dato",
    "N/A", "Ninguno", "None", "Otro", "No", "Rechazado"
  )
  clase <- lupa:::.clasificar_etiqueta_ausencia(etiquetas, lexico)
  expect_identical(clase$semantica, c(
    "sin_dato", "sin_dato", "sin_dato", "no_responde", "sin_dato",
    "no_corresponde", "sin_dato", "ambigua", NA, NA, NA, NA, "declarada"
  ))
  expect_identical(clase$coincidencia[c(4L, 7L)], c("compuesta", "parcial"))
})

test_that("los argumentos se validan", {
  caso <- coleccion_codigos()
  on.exit(DBI::dbDisconnect(caso$con), add = TRUE)
  pc <- suppressWarnings(perfilar_coleccion(caso$col))
  expect_error(detectar_tablas_codigos(caso$col, list()), "perfilar_coleccion",
               class = "lupa_error_argumento_dbi")
  expect_error(detectar_tablas_codigos(caso$col, pc, max_filas = 0),
               class = "lupa_error_argumento_dbi")
  expect_error(detectar_tablas_codigos(caso$col, pc, etiquetas_ausencia = NA_character_),
               class = "lupa_error_argumento_dbi")
})
