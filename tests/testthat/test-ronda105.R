datos_r105 <- function(n = 5000L) {
  i <- seq_len(n)
  data.frame(
    id = i,
    entero = ifelse(i %% 17L == 0L, NA_integer_,
                    ifelse(i %% 10L == 0L, 0L, (i %% 31L) - 15L)),
    decimal = ifelse(i %% 23L == 0L, NA_real_,
                     ifelse(i %% 10L == 0L, 1.25, (i %% 19L) / 4)),
    texto = ifelse(i %% 13L == 0L, NA_character_,
                   ifelse(i %% 5L == 0L, "B", "A")),
    fecha = ifelse(
      i %% 29L == 0L, NA_character_,
      ifelse(i %% 4L == 0L, "2020-06-01",
             as.character(as.Date("2020-01-01") + (i %% 366L)))
    ),
    stringsAsFactors = FALSE
  )
}

con_r105 <- function(datos = datos_r105()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "datos_r105", datos)
  con
}

argumentos_perfil_r105 <- list(
  analizar_dependencias = FALSE,
  proteger_datos_personales = FALSE,
  casi_duplicados_vocabulario = FALSE
)

resumen_r_columna_r105 <- function(x) {
  validos <- !is.na(x)
  valores <- x[validos]
  es_numerico <- is.numeric(x)
  unicos <- unique(valores)
  frecuencias <- if (length(valores)) {
    tabulate(match(valores, unicos), nbins = length(unicos))
  } else integer()
  posicion_moda <- if (length(frecuencias)) which.max(frecuencias) else NA_integer_
  data.frame(
    n = length(x),
    n_validos = sum(validos),
    n_faltantes = sum(!validos),
    prop_faltantes = if (length(x)) mean(!validos) else NA_real_,
    n_distintos = length(unicos),
    tasa_distintos = if (length(valores)) length(unicos) / length(valores) else NA_real_,
    moda = if (length(valores)) lupa:::.texto_valor(unicos[[posicion_moda]]) else NA_character_,
    frecuencia_moda = if (length(valores)) frecuencias[[posicion_moda]] else NA_real_,
    minimo = if (es_numerico && length(valores)) min(valores) else NA_real_,
    maximo = if (es_numerico && length(valores)) max(valores) else NA_real_,
    media = if (es_numerico && length(valores)) mean(valores) else NA_real_,
    mediana = if (es_numerico && length(valores)) stats::median(valores) else NA_real_,
    desvio = if (es_numerico && length(valores) > 1L) stats::sd(valores) else NA_real_,
    n_ceros = if (es_numerico && length(valores)) sum(valores == 0) else NA_real_,
    n_negativos = if (es_numerico && length(valores)) sum(valores < 0) else NA_real_,
    stringsAsFactors = FALSE
  )
}

test_that("el resumen SQL coincide con R sobre las 5000 filas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- datos_r105()
  con <- con_r105(datos)
  on.exit(DBI::dbDisconnect(con))
  tablas_antes <- DBI::dbListTables(con)
  DBI::dbExecute(con, "PRAGMA query_only = ON")

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "datos_r105", muestra = 1000L,
           orden_muestra = "id"), argumentos_perfil_r105)
  )
  sql <- resultado$resumen_tabla$columnas
  esperado <- do.call(rbind, lapply(datos, resumen_r_columna_r105))
  esperado$columna <- names(datos)
  esperado <- esperado[c("columna", setdiff(names(esperado), "columna"))]
  rownames(esperado) <- NULL

  expect_equal(sql, esperado, tolerance = 1e-12)
  expect_identical(resultado$resumen_tabla$meta$alcance, "tabla_completa")
  expect_equal(resultado$resumen_tabla$meta$filas, 5000)
  expect_true(length(resultado$resumen_tabla$meta$motor$informacion_dbi) > 0L)
  expect_setequal(
    unique(resultado$resumen_tabla$sql$metrica),
    setdiff(names(resultado$resumen_tabla$columnas), "columna")
  )
  expect_true(all(nzchar(resultado$resumen_tabla$sql$sql[
    resultado$resumen_tabla$sql$estado == "calculado"
  ])))
  expect_identical(DBI::dbListTables(con), tablas_antes)
  expect_false(resultado$resumen_tabla$meta$objetos_temporales)
})

test_that("el perfil completo declara el alcance de la muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- con_r105()
  on.exit(DBI::dbDisconnect(con))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "datos_r105", muestra = 1000L,
           orden_muestra = "id"), argumentos_perfil_r105)
  )
  perfil <- resultado$perfil_muestra
  alcance <- perfil$meta$origen_dbi$muestreo

  expect_named(resultado, c("resumen_tabla", "perfil_muestra"))
  expect_s3_class(perfil, "perfil")
  ## Canario de esquema: si este numero cambia sin querer, es que una columna
  ## nueva del perfil se colo sin documentarse. Subio a 105 al declarar el
  ## universo aplicable (4 campos) y la representacion geometrica (2), y a 107
  ## al medir el salto de escala de una secuencia entera -el hueco mas grande y
  ## si es desproporcionado-, que es la segunda senal para reconocer una
  ## numeracion, a 109 al medir el valor centinela por tres senales -cual es y
  ## cuantas veces aparece- y a 110 al medir la densidad sin ese centinela, que
  ## es lo que decide si la columna es una numeracion con un centinela adentro.
  ## La columna de estado del tipo inferido se suma al esquema publicado.
  expect_equal(ncol(perfil$columnas), 111L)
  expect_equal(ncol(perfil$columnas) - 1L, 110L)
  expect_true(all(perfil$columnas$n == 1000L))
  expect_equal(alcance$filas_solicitadas, 1000)
  expect_equal(alcance$filas_obtenidas, 1000)
  expect_equal(alcance$filas_totales_fuente, 5000)
  expect_false(alcance$tabla_completa)
  expect_true(alcance$orden_unico_verificado)
  expect_true(alcance$reproducible)
  expect_match(alcance$sql_muestra, "ORDER BY")
})

test_that("sin orden no se finge reproducibilidad", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- con_r105(datos_r105(40L))
  on.exit(DBI::dbDisconnect(con))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "datos_r105", muestra = 10L),
      argumentos_perfil_r105)
  )
  alcance <- resultado$perfil_muestra$meta$origen_dbi$muestreo

  expect_false(alcance$reproducible)
  expect_match(alcance$motivo_reproducibilidad, "no garantiza")
  expect_identical(alcance$metodo, "primeras_filas_sin_orden_garantizado")
})

test_that("una muestra mayor trae la tabla entera y lo declara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- con_r105(datos_r105(75L))
  on.exit(DBI::dbDisconnect(con))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "datos_r105", muestra = 100L,
           orden_muestra = "id"), argumentos_perfil_r105)
  )
  alcance <- resultado$perfil_muestra$meta$origen_dbi$muestreo

  expect_equal(alcance$filas_solicitadas, 100)
  expect_equal(alcance$filas_obtenidas, 75)
  expect_equal(alcance$filas_totales_fuente, 75)
  expect_true(alcance$tabla_completa)
  expect_equal(resultado$perfil_muestra$general$filas, 75L)
  expect_false(grepl("LIMIT", alcance$sql_muestra, fixed = TRUE))
})

test_that("una tabla vacia conserva alcances y NA honestos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- con_r105(datos_r105(0L))
  on.exit(DBI::dbDisconnect(con))

  resultado <- do.call(
    perfilar_dbi,
    c(list(conexion = con, tabla = "datos_r105", muestra = 10L,
           orden_muestra = "id"), argumentos_perfil_r105)
  )
  resumen <- resultado$resumen_tabla$columnas
  alcance <- resultado$perfil_muestra$meta$origen_dbi$muestreo

  expect_true(all(resumen$n == 0))
  expect_true(all(resumen$n_faltantes == 0))
  expect_true(all(resumen$n_distintos == 0))
  expect_true(all(is.na(resumen$prop_faltantes)))
  expect_true(all(is.na(resumen$moda)))
  expect_equal(alcance$filas_obtenidas, 0)
  expect_true(alcance$tabla_completa)
  expect_equal(resultado$perfil_muestra$general$filas, 0L)
})

test_that("un BLOB no se convierte en un agregado cuantitativo cero", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE blobs (id INTEGER, contenido BLOB)")
  DBI::dbExecute(
    con,
    "INSERT INTO blobs VALUES (1, X'0102'), (2, X'0102'), (3, NULL)"
  )

  resultado <- NULL
  ## La ausencia de un BLOB es conocible con `is.na()`, asi que se nombra y no
  ## queda incoherencia que advertir. Antes se contaba sin nombrar y la guarda
  ## avisaba; esta prueba fija que el hueco quedo cerrado.
  expect_no_warning(
    resultado <- do.call(
      perfilar_dbi,
      c(list(conexion = con, tabla = "blobs", muestra = 10L,
             orden_muestra = "id"), argumentos_perfil_r105)
    ),
    class = "lupa_trazabilidad_incoherente"
  )
  faltantes_blob <- resultado$perfil_muestra$hallazgos[
    resultado$perfil_muestra$hallazgos$columna == "contenido" &
      resultado$perfil_muestra$hallazgos$tipo_hallazgo == "faltantes", ,
    drop = FALSE
  ]
  expect_equal(nrow(faltantes_blob), 1L)
  expect_equal(faltantes_blob$trazabilidad[[1L]]$indices_fila, 3L)
  fila <- resultado$resumen_tabla$columnas[
    resultado$resumen_tabla$columnas$columna == "contenido", , drop = FALSE
  ]
  diagnostico <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$columna == "contenido" &
      resultado$resumen_tabla$sql$metrica == "media", , drop = FALSE
  ]

  expect_true(is.na(fila$media))
  expect_identical(diagnostico$estado, "no_aplica")
  expect_match(diagnostico$motivo, "DBI expuso")
})

test_that("la ausencia de DBI falla con un mensaje accionable", {
  local_mocked_bindings(.dbi_disponible = function() FALSE, .package = "lupa")
  expect_error(
    perfilar_dbi(NULL, "tabla"),
    "instalar el paquete opcional 'DBI'", fixed = TRUE
  )
})

test_that("el camino de data.frame conserva su contrato", {
  datos <- datos_r105(40L)
  fecha <- as.POSIXct("2026-08-16 12:00:00", tz = "UTC")
  argumentos <- c(
    list(datos = datos, nombre = "memoria-r105", fecha = fecha),
    argumentos_perfil_r105
  )

  antes <- do.call(perfilar, argumentos)
  despues <- do.call(perfilar, argumentos)
  expect_identical(despues, antes)
  expect_identical(names(despues), c(
    "general", "columnas", "patrones", "formatos_fecha", "dependencias",
    "hallazgos", "cobertura_diagnosticos", "datos_personales", "meta"
  ))
})
