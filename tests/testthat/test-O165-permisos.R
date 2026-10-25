# Permisos por columna, enmascaramiento y conteo de faltantes en el motor. Cada
# bloque es un defecto, con la cifra rehecha a mano. SQL Server y PostgreSQL no
# estan en la maquina de CRAN: lo que dicen sus catalogos y lo que rechazan se
# midio en los motores reales y aca se finge sobre RSQLite, con las mismas
# respuestas y los mismos rechazos, para ejercer la decision que el paquete toma
# con ellas. El archivo es ASCII.

# El motor fingido. RSQLite contesta los datos; las consultas al catalogo se
# contestan como las contesta el motor medido, y lo que ese motor rechaza se
# rechaza aca con su mensaje:
# * `negadas`: columnas que la credencial no puede leer; toda consulta que las
#   nombre se rechaza (SQL Server y PostgreSQL, medido).
# * `rechaza_tabla`: tablas sobre las que el motor rechaza tambien `COUNT(*)`,
#   `SELECT 1` y `SELECT *` cuando hay una columna sin permiso (SQL Server,
#   medido); una tabla derivada con las legibles se lee.
# * `cuenta_enmascarada`: columnas cuyo `COUNT(col)` sale enmascarado, 0 (DDM
#   de SQL Server, medido); la tabla guarda lo que la credencial lee, la mascara.
# * `rechaza_count`: si TRUE, `COUNT(col)` es invalido sobre cualquier columna
#   (SQL Server lo rechaza sobre `text`, `ntext` e `image`, medido).
# * `listados`: lo que devuelve `dbListFields()` por tabla, cuando el motor la
#   filtra (RPostgres lee `information_schema.columns`, medido).
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.emular_motor_r35x1 <- function(senas = NULL, catalogo = list(),
                                mascaras = list(), negadas = character(),
                                rechaza_tabla = character(),
                                cuenta_enmascarada = character(),
                                rechaza_count = FALSE, listados = list(),
                                env = parent.frame()) {
  ns <- asNamespace("lupa")
  traer <- get(".traer_consulta_dbi", envir = ns)
  enviar <- get(".enviar_consulta_dbi", envir = ns)
  listar <- get(".listar_campos_dbi", envir = ns)
  registro <- new.env()
  registro$sql <- character()
  ident <- function(x) paste0("[`\"]", x, "[`\"]")
  nombra <- function(sql, tabla) grepl(ident(tabla), sql, perl = TRUE)
  motor <- function(conexion, sql, original) {
    registro$sql <- c(registro$sql, sql)
    if (grepl("sys.masked_columns", sql, fixed = TRUE)) {
      for (tabla in names(mascaras)) if (nombra(sql, tabla)) return(mascaras[[tabla]])
      return(data.frame(columna = character(), mascara = character(),
                        sin_mascara_columna = integer(),
                        sin_mascara_base = integer()))
    }
    if (grepl("sys.columns|pg_attribute", sql, perl = TRUE)) {
      for (tabla in names(catalogo)) if (nombra(sql, tabla)) return(catalogo[[tabla]])
      return(data.frame(posicion = integer(), columna = character(),
                        legible = integer()))
    }
    for (columna in negadas) {
      if (grepl(ident(columna), sql, perl = TRUE)) {
        stop("The SELECT permission was denied on the column '", columna, "'")
      }
    }
    for (tabla in rechaza_tabla) {
      if (grepl(paste0("FROM ", ident(tabla), "($|[^)])"), sql, perl = TRUE) &&
          grepl("COUNT\\(\\*\\)|SELECT 1 |SELECT \\*", sql, perl = TRUE)) {
        stop("The SELECT permission was denied on the object '", tabla, "'")
      }
    }
    if (isTRUE(rechaza_count) &&
        grepl("COUNT\\((?!\\*|DISTINCT |CASE WHEN )", sql, perl = TRUE)) {
      stop("Operand data type text is invalid for count operator.")
    }
    for (columna in cuenta_enmascarada) {
      sql <- gsub(paste0("COUNT\\(", ident(columna), "\\)"), "0", sql, perl = TRUE)
    }
    original(conexion, sql)
  }
  ligaduras <- list(
    .traer_consulta_dbi = function(conexion, sql) motor(conexion, sql, traer),
    .enviar_consulta_dbi = function(conexion, sql) motor(conexion, sql, enviar),
    .listar_campos_dbi = function(conexion, nombre) {
      texto <- if (is.character(nombre)) nombre else
        tryCatch(nombre@name[["table"]], error = function(e) "")
      for (tabla in names(listados)) {
        if (identical(texto, tabla)) return(listados[[tabla]])
      }
      listar(conexion, nombre)
    }
  )
  if (!is.null(senas)) ligaduras$.senas_conexion_dbi <- function(conexion) senas
  do.call(testthat::local_mocked_bindings,
          c(ligaduras, list(.package = "lupa", .env = env)))
  registro
}

# Dos tablas: `persona` (id, cedula, nombre, depto) con 40 filas, `depto` nulo
# cuando id %% 4 == 0 -> 10 nulos-; `tramite` (nro, persona_id, depto) con 60.
# A mano: `persona_id` toma 1 + (1:60 %% 40), 40 valores, todos en `id`.
.tablas_persona_r35x1 <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  n <- 40L
  DBI::dbWriteTable(con, "persona", data.frame(
    id = seq_len(n), cedula = as.character(1000000L + seq_len(n) * 37L),
    nombre = paste0("n", seq_len(n)),
    depto = ifelse(seq_len(n) %% 4L == 0L, NA_integer_, seq_len(n) %% 7L)
  ))
  DBI::dbWriteTable(con, "tramite", data.frame(
    nro = 500L + seq_len(60L), persona_id = 1L + (seq_len(60L) %% n),
    depto = seq_len(60L) %% 7L
  ))
  con
}

.catalogo_r35x1 <- function(columnas, legible) {
  data.frame(posicion = seq_along(columnas), columna = columnas,
             legible = as.integer(legible))
}

.cobertura_columna_r35x1 <- function(r, columna) {
  cb <- r$resumen_tabla$cobertura
  cb[cb$bloque == "resumen_tabla" & cb$elemento == columna, , drop = FALSE]
}

test_that("A1: con una columna sin permiso en SQL Server se perfilan las demas y ninguna consulta la nombra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .tablas_persona_r35x1()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  registro <- .emular_motor_r35x1(
    senas = "microsoft sql server odbc",
    catalogo = list(
      persona = .catalogo_r35x1(c("id", "cedula", "nombre", "depto"),
                                c(1, 0, 1, 1)),
      tramite = .catalogo_r35x1(c("nro", "persona_id", "depto"), c(1, 1, 1))
    ),
    negadas = "cedula", rechaza_tabla = "persona"
  )
  r <- suppressWarnings(suppressMessages(perfilar_dbi(con, "persona", proteger_datos_personales = TRUE)))
  columnas <- r$resumen_tabla$columnas
  # Las cuatro columnas estan; `cedula` sin medir y declarada.
  expect_setequal(columnas$columna, c("id", "cedula", "nombre", "depto"))
  expect_true(is.na(columnas$n_validos[columnas$columna == "cedula"]))
  fila <- .cobertura_columna_r35x1(r, "cedula")
  expect_identical(fila$estado, "no_disponible")
  expect_true(grepl("permiso SELECT", fila$motivo, fixed = TRUE))
  expect_true(grepl("GRANT SELECT", fila$como_resolverlo, fixed = TRUE))
  # Las demas, enteras: 40 filas, 10 `depto` nulos, media de id = 20,5.
  expect_equal(r$resumen_tabla$meta$filas, 40)
  expect_equal(columnas$n_faltantes[columnas$columna == "depto"], 10)
  expect_equal(columnas$media[columnas$columna == "id"], mean(1:40))
  expect_setequal(r$perfil_muestra$columnas$columna, c("id", "nombre", "depto"))
  # Las cuatro puertas terminan.
  expect_s3_class(suppressWarnings(suppressMessages(
    plan_perfilado_dbi(con, "persona")
  )), "plan_perfilado_dbi")
  col <- suppressMessages(coleccion(con, c("persona", "tramite")))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(col, proteger_datos_personales = TRUE)))
  expect_equal(pc$resumen_coleccion$n_columnas[pc$resumen_coleccion$tabla == "persona"], 4)
  rc <- suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "persona", tabla_2 = "tramite")
  )))
  # `id -> persona_id`: los 40 valores, a mano.
  fila_rel <- rc$relaciones[rc$relaciones$columna_tabla1 == "id" &
                              rc$relaciones$columna_tabla2 == "persona_id", ]
  expect_equal(nrow(fila_rel), 1L)
  expect_equal(fila_rel$n_valores_comunes, 40)
  podas <- rc$cobertura_podas[rc$cobertura_podas$motivo == "columna_sin_permiso", ]
  expect_setequal(podas$columna_tabla2, c("nro", "persona_id", "depto"))
  expect_true(all(podas$columna_tabla1 == "cedula"))
  # Con `universo = "muestra_motor"` (la via del spool) la
  # columna negada desaparecia del perfil y de la coleccion sin una sola fila
  # de cobertura. Se declara igual que sobre la tabla completa.
  rm <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "persona", universo = "muestra_motor", muestra_motor = 20, proteger_datos_personales = TRUE
  )))
  columnas_m <- rm$resumen_tabla$columnas
  expect_setequal(columnas_m$columna, c("id", "cedula", "nombre", "depto"))
  expect_true(is.na(columnas_m$n_validos[columnas_m$columna == "cedula"]))
  fila_m <- .cobertura_columna_r35x1(rm, "cedula")
  expect_identical(fila_m$estado, "no_disponible")
  expect_true(grepl("permiso SELECT", fila_m$motivo, fixed = TRUE))
  sql_m <- rm$resumen_tabla$sql
  expect_true(all(sql_m$estado[sql_m$columna == "cedula" & sql_m$metrica != "n"] ==
                    "no_disponible"))
  pcm <- suppressWarnings(suppressMessages(
    perfilar_coleccion(col, universo = "muestra_motor", muestra_motor = 20, proteger_datos_personales = TRUE)
  ))
  expect_equal(pcm$resumen_coleccion$n_columnas[
    pcm$resumen_coleccion$tabla == "persona"], 4)
  expect_true(any(pcm$cobertura_metricas$columna == "cedula" &
                    pcm$cobertura_metricas$estado == "no_disponible"))
  # Y ninguna consulta mandada nombra la columna negada.
  expect_gt(length(registro$sql), 0L)
  expect_false(any(grepl("[`\"]cedula[`\"]", registro$sql, perl = TRUE)))
})

test_that("M4 y B2: con GRANT por columnas en PostgreSQL la tabla tiene cuatro columnas y el par se compara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .tablas_persona_r35x1()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  registro <- .emular_motor_r35x1(
    senas = "pqconnection rpostgres postgresql",
    catalogo = list(
      persona = .catalogo_r35x1(c("id", "cedula", "nombre", "depto"),
                                c(TRUE, FALSE, TRUE, TRUE)),
      tramite = .catalogo_r35x1(c("nro", "persona_id", "depto"), rep(TRUE, 3))
    ),
    negadas = "cedula",
    # `dbListFields()` de RPostgres no lista la columna sin privilegio.
    listados = list(persona = c("id", "nombre", "depto"))
  )
  r <- suppressWarnings(suppressMessages(perfilar_dbi(con, "persona", proteger_datos_personales = TRUE)))
  columnas <- r$resumen_tabla$columnas
  expect_setequal(columnas$columna, c("id", "cedula", "nombre", "depto"))
  fila <- .cobertura_columna_r35x1(r, "cedula")
  expect_identical(fila$estado, "no_disponible")
  expect_true(grepl("permiso SELECT", fila$motivo, fixed = TRUE))
  expect_equal(columnas$n_faltantes[columnas$columna == "depto"], 10)
  col <- suppressMessages(coleccion(con, c("persona", "tramite")))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(col, proteger_datos_personales = TRUE)))
  expect_equal(pc$resumen_coleccion$n_columnas[pc$resumen_coleccion$tabla == "persona"], 4)
  # B2: el par se compara (antes `SELECT *` se rechazaba y se perdia entero).
  rc <- suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "persona", tabla_2 = "tramite")
  )))
  expect_equal(nrow(rc$cobertura_pares), 0L)
  fila_rel <- rc$relaciones[rc$relaciones$columna_tabla1 == "id" &
                              rc$relaciones$columna_tabla2 == "persona_id", ]
  expect_equal(fila_rel$n_valores_comunes, 40)
  expect_true(any(rc$cobertura_podas$motivo == "columna_sin_permiso"))
  # Una candidata que existe y no se puede leer no es "inexistente".
  rc2 <- suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "persona", tabla_2 = "tramite"),
    columnas_candidatas = list(persona = c("id", "cedula"),
                               tramite = "persona_id")
  )))
  expect_equal(rc2$relaciones$n_valores_comunes[
    rc2$relaciones$columna_tabla1 == "id"], 40)
  expect_false(any(grepl("[`\"]cedula[`\"]", registro$sql, perl = TRUE)))
})

test_that("M2: la completitud y el desvio se miden aunque el motor rechace COUNT(col)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # A mano: `tx` nulo en 1 de cada 10 filas de 50 -> 5; `x` nulo en 7.
  x <- c(seq(1.5, 43.5, by = 1), rep(NA, 7))
  d <- data.frame(id = 1:50, tx = ifelse(1:50 %% 10 == 0, NA, paste0("t", 1:50)),
                  x = x)
  DBI::dbWriteTable(con, "tipos", d)
  .emular_motor_r35x1(rechaza_count = TRUE)
  r <- suppressWarnings(suppressMessages(perfilar_dbi(con, "tipos", proteger_datos_personales = TRUE)))
  columnas <- r$resumen_tabla$columnas
  expect_equal(columnas$n_faltantes[columnas$columna == "tx"], 5)
  expect_equal(columnas$n_faltantes[columnas$columna == "x"], 7)
  expect_equal(columnas$prop_faltantes[columnas$columna == "tx"], 0.1)
  expect_equal(columnas$desvio[columnas$columna == "x"], stats::sd(x, na.rm = TRUE))
  # Ninguna metrica quedo sin medir por el rechazo: el conteo de no nulos es
  # uno solo en todas las consultas.
  sql <- r$resumen_tabla$sql
  expect_false(any(grepl("invalid for count operator", sql$motivo, fixed = TRUE)))
})

test_that("A3: una columna enmascarada publica su completitud y nada de lo que describe la mascara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n <- 40L
  # Lo que lee una credencial sin UNMASK: la mascara donde hay dato, el nulo
  # donde no. A mano: `cedula` 0 nulos, `ingreso` 4 (id %% 10 == 0). `abierta`
  # tambien esta enmascarada pero la credencial tiene UNMASK sobre ella: se lee
  # el dato, 40 valores distintos.
  DBI::dbWriteTable(con, "cliente", data.frame(
    id = seq_len(n), cedula = rep("xxxx", n),
    ingreso = ifelse(seq_len(n) %% 10L == 0L, NA_integer_, 0L),
    abierta = paste0("a", seq_len(n)), depto = seq_len(n) %% 7L
  ))
  # Otra tabla con una cedula enmascarada que no comparte ningun valor real.
  DBI::dbWriteTable(con, "contacto", data.frame(
    nro = 900L + seq_len(10L), cedula = rep("xxxx", 10L)
  ))
  mascara <- function(columnas, abiertas = character()) data.frame(
    columna = columnas, mascara = "default()",
    sin_mascara_columna = as.integer(columnas %in% abiertas),
    sin_mascara_base = 0L
  )
  .emular_motor_r35x1(
    senas = "microsoft sql server odbc",
    catalogo = list(
      cliente = .catalogo_r35x1(c("id", "cedula", "ingreso", "abierta", "depto"),
                                rep(1, 5)),
      contacto = .catalogo_r35x1(c("nro", "cedula"), c(1, 1))
    ),
    mascaras = list(
      cliente = mascara(c("cedula", "ingreso", "abierta"), abiertas = "abierta"),
      contacto = mascara("cedula")
    ),
    cuenta_enmascarada = c("cedula", "ingreso")
  )
  for (bloque in c("con_muestra", "solo_agregados")) {
    r <- suppressWarnings(suppressMessages(
      perfilar_dbi(con, "cliente", bloque_muestra = bloque, proteger_datos_personales = TRUE)
    ))
    columnas <- r$resumen_tabla$columnas
    fila <- function(columna) columnas[columnas$columna == columna, , drop = FALSE]
    expect_equal(fila("cedula")$n_faltantes, 0)
    expect_equal(fila("ingreso")$n_faltantes, 4)
    expect_equal(fila("ingreso")$prop_faltantes, 0.1)
    for (columna in c("cedula", "ingreso")) {
      expect_true(is.na(fila(columna)$n_distintos))
      expect_true(is.na(fila(columna)$moda))
      sql <- r$resumen_tabla$sql
      sql <- sql[sql$columna == columna & sql$metrica %in%
                   c("n_distintos", "moda", "minimo", "maximo", "media"), ]
      expect_true(all(sql$estado %in% c("no_disponible", "no_aplica")))
      expect_true(any(sql$estado == "no_disponible"))
      cb <- .cobertura_columna_r35x1(r, columna)
      expect_true(grepl("enmascarada", cb$motivo, fixed = TRUE))
      expect_true(grepl("UNMASK", cb$como_resolverlo, fixed = TRUE))
    }
    # Con UNMASK se perfila entera.
    expect_equal(fila("abierta")$n_distintos, 40)
    if (identical(bloque, "con_muestra")) {
      expect_setequal(r$perfil_muestra$columnas$columna, c("id", "abierta", "depto"))
      corroboracion <- r$resumen_tabla$cobertura
      corroboracion <- corroboracion[corroboracion$bloque == "corroboracion" &
                                       corroboracion$estado == "divergencia", ]
      expect_false(any(corroboracion$elemento %in% c("cedula", "ingreso")))
    }
  }
  # La via del spool (`universo = "muestra_motor"`): el resumen sale del perfil
  # de la muestra, y la columna enmascarada no entra en el perfil. Las filas se
  # aparean por nombre -por posicion se reciclaban las de las otras columnas-
  # y la completitud de la enmascarada sale de lo leido: `cedula` no tiene
  # ningun nulo en la tabla, asi que tampoco en la muestra.
  r <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "cliente", universo = "muestra_motor", muestra_motor = 20, proteger_datos_personales = TRUE
  )))
  columnas <- r$resumen_tabla$columnas
  expect_identical(columnas$columna, c("id", "cedula", "ingreso", "abierta", "depto"))
  expect_equal(columnas$n_faltantes[columnas$columna == "cedula"], 0)
  expect_true(is.na(columnas$n_distintos[columnas$columna == "cedula"]))
  expect_true(is.na(columnas$n_distintos[columnas$columna == "ingreso"]))
  expect_false(any(c("cedula", "ingreso") %in% r$perfil_muestra$columnas$columna))
  expect_equal(columnas$n_distintos[columnas$columna == "depto"],
               r$perfil_muestra$columnas$n_distintos[
                 r$perfil_muestra$columnas$columna == "depto"])
  # La coleccion: completitud del dato, no de la mascara.
  col <- suppressMessages(coleccion(con, c("cliente", "contacto")))
  pc <- suppressWarnings(suppressMessages(
    perfilar_coleccion(col, bloque_muestra = "solo_agregados", proteger_datos_personales = TRUE)
  ))
  fila_pc <- pc$resumen_coleccion[pc$resumen_coleccion$tabla == "cliente", ]
  expect_equal(fila_pc$prop_faltantes_maxima, 0.1)
  expect_equal(fila_pc$n_columnas_sin_faltantes, 4)
  # Y dos cedulas enmascaradas sin un valor comun no se relacionan.
  rc <- suppressWarnings(suppressMessages(relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "contacto", tabla_2 = "cliente")
  )))
  expect_false(any(rc$relaciones$columna_tabla1 == "cedula" &
                     rc$relaciones$columna_tabla2 == "cedula"))
  podas <- rc$cobertura_podas[rc$cobertura_podas$motivo == "columna_enmascarada", ]
  expect_true(any(podas$columna_tabla1 == "cedula" & podas$columna_tabla2 == "cedula"))
})

test_that("M5: la columna que falla en una fila que no es la primera se aisla sobre las filas de la muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "PRAGMA threads=1")
  # 300 filas; `codigo` no es un entero en la fila 240; `depto` nulo cuando
  # g %% 9 == 0 -> 33. duckdb lee la primera fila de la vista y falla al leerla
  # entera, como PostgreSQL y SQL Server (medido).
  DBI::dbExecute(con, paste(
    "CREATE TABLE legado AS SELECT g AS id,",
    "CASE WHEN g = 240 THEN 'A-12' ELSE CAST(1000 + g AS VARCHAR) END AS codigo,",
    "g * 2.5 AS monto,",
    "CASE WHEN g % 9 = 0 THEN NULL ELSE 'd' || CAST(g % 5 AS VARCHAR) END AS depto",
    "FROM range(1, 301) t(g)"
  ))
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_codigo AS SELECT id, CAST(codigo AS INTEGER) AS codigo_num,",
    "monto, depto FROM legado"
  ))
  r <- suppressWarnings(suppressMessages(perfilar_dbi(con, "v_codigo", proteger_datos_personales = TRUE)))
  expect_false(is.null(r$perfil_muestra))
  expect_setequal(r$perfil_muestra$columnas$columna, c("id", "monto", "depto"))
  pm <- r$perfil_muestra$columnas
  expect_equal(pm$n_faltantes[pm$columna == "depto"], sum(1:300 %% 9 == 0))
  cb <- r$resumen_tabla$cobertura
  fila <- cb[cb$bloque == "perfil_muestra", , drop = FALSE]
  expect_identical(fila$estado, "alcance_distinto")
  expect_true(grepl("codigo_num", fila$motivo, fixed = TRUE))
  expect_false(grepl("ninguna fallo por si sola", fila$motivo, fixed = TRUE))
})

# El motivo de la columna aislada lleva el mensaje del motor, y sigue la
# bandera como el de la muestra perdida (test-O157): entero sin proteccion, sin
# el literal con ella.
test_that("M5: el motivo de la columna aislada sigue la bandera de proteccion", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  d <- data.frame(id = 1:30, codigo_txt = as.character(1:30),
                  stringsAsFactors = FALSE)
  d$codigo_txt[25] <- "N/D"
  DBI::dbWriteTable(con, "codigos", d)
  DBI::dbExecute(con, paste(
    "CREATE VIEW codigos_v AS",
    "SELECT id, CAST(codigo_txt AS INTEGER) AS codigo FROM codigos"
  ))
  fila_muestra <- function(r) {
    cb <- r$resumen_tabla$cobertura
    cb[cb$bloque == "perfil_muestra", , drop = FALSE]
  }
  sin <- suppressWarnings(perfilar_dbi(
    con, "codigos_v", proteger_datos_personales = FALSE
  ))
  expect_identical(sin$perfil_muestra$columnas$columna, "id")
  expect_identical(fila_muestra(sin)$estado, "alcance_distinto")
  expect_true(grepl("N/D", fila_muestra(sin)$motivo, fixed = TRUE))
  con_proteccion <- suppressWarnings(perfilar_dbi(con, "codigos_v", proteger_datos_personales = TRUE))
  expect_identical(fila_muestra(con_proteccion)$estado, "alcance_distinto")
  expect_false(grepl("N/D", fila_muestra(con_proteccion)$motivo, fixed = TRUE))
  expect_true(grepl("[literal omitido]", fila_muestra(con_proteccion)$motivo,
                    fixed = TRUE))
})

test_that("Guarda: las columnas de una tabla y lo que la credencial lee salen de una sola funcion", {
  ns <- asNamespace("lupa")
  sitios <- character()
  recorrer <- function(e, nombre) {
    if (is.call(e)) {
      cabeza <- e[[1L]]
      if (is.name(cabeza) && identical(as.character(cabeza), ".listar_campos_dbi")) {
        sitios <<- c(sitios, nombre)
      }
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]], nombre)
      }
    }
    invisible()
  }
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto)) recorrer(body(objeto), nombre)
  }
  expect_identical(sort(unique(sitios)), ".columnas_tabla_dbi")
})
