# Ayudas de las pruebas de la mediana de `muestra_motor` con
# `bloque_muestra = "solo_agregados"` y de la mediana sin conteo de la tabla
# completa, que comparten `test-mediana-cte-muestra-dbi.R`,
# `test-mediana-sin-conteo-dbi.R` y `test-local-mysql.R`.
#
# La tabla tiene un doble comun (`x`), un entero (`y`), un doble con NULL
# (`z`) y un doble con extremos (`w`, del orden de 1e200), que no puede usar la
# mediana consolidada. Con `filas` menor que la muestra pedida, la muestra
# cubre la tabla y las dos vias leen las mismas filas: los valores se pueden
# comparar con `median()`.

.mcm_datos <- function(filas) {
  set.seed(20261105)
  data.frame(
    x = stats::rnorm(filas),
    y = sample(1:50, filas, replace = TRUE),
    z = ifelse(stats::runif(filas) < 0.3, NA_real_,
               round(stats::rnorm(filas) * 100, 2)),
    w = stats::rnorm(filas) * 1e200
  )
}

# Centrales de +-1,7e308, cuya suma desborda. Con `metricas = c("basicos",
# "mediana")` no se conoce el conteo de validos y la guarda de la via de dos
# consultas no pide la escalada: la CTE tiene que pedirla por los extremos.
.mcm_datos_extremos <- function(filas) {
  set.seed(20261105)
  mitad <- filas %/% 2L
  data.frame(
    x = stats::rnorm(filas),
    w = stats::rnorm(filas) * 1e200,
    g = c(rep(-1.7e308, mitad - 1L), rep(1.7e308, filas - mitad + 1L))
  )
}

# MySQL y MariaDB no reciben `dbWriteTable()` sin la carga local de archivos:
# la tabla se escribe con INSERT, como en `.arrm_tabla_normal()`.
.mcm_escribir <- function(conexion, tabla, datos) {
  try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
  senas <- .senas_conexion_dbi(conexion)
  if (!grepl("mysql|maria", senas, perl = TRUE, useBytes = TRUE)) {
    DBI::dbWriteTable(conexion, tabla, datos)
    return(invisible(NULL))
  }
  tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
  tipos <- ifelse(vapply(datos, is.integer, logical(1L)), "INTEGER", "DOUBLE")
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE ", tabla_sql, " (",
    paste(names(datos), tipos, collapse = ", "), ")"
  ))
  literal <- function(v) {
    ifelse(is.na(v), "NULL", if (is.integer(v)) as.character(v) else
      sprintf("%.17g", v))
  }
  celdas <- vapply(datos, literal, character(nrow(datos)))
  filas <- paste0("(", apply(celdas, 1L, paste, collapse = ","), ")")
  for (lote in split(filas, ceiling(seq_along(filas) / 1000))) {
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla_sql, " (", paste(names(datos), collapse = ", "),
      ") VALUES ", paste(lote, collapse = ",")
    ))
  }
  invisible(NULL)
}

# La mediana de cada columna por las dos vias: estado, error, metodo y valor.
.mcm_medianas <- function(conexion, tabla, muestra_motor, ...) {
  vias <- c("solo_agregados", "con_muestra")
  stats::setNames(lapply(vias, function(via) {
    perfil <- suppressWarnings(perfilar_dbi(
      conexion, tabla, universo = "muestra_motor",
      muestra_motor = muestra_motor, bloque_muestra = via,
      proteger_datos_personales = FALSE, ...
    ))
    sql <- perfil$resumen_tabla$sql
    columnas <- perfil$resumen_tabla$columnas
    salida <- sql[sql$metrica == "mediana",
                  c("columna", "estado", "error_esperado", "metodo", "motivo")]
    salida$valor <- as.numeric(
      columnas$mediana[match(salida$columna, columnas$columna)]
    )
    rownames(salida) <- salida$columna
    salida
  }), vias)
}

# Con `solo_agregados` la mediana sale con el mismo par estado/error que con
# `con_muestra`, que es el de `.clasificar_metrica_muestreo_dbi()`. Las
# columnas de `metodo_cte` salen de la CTE de ventanas y, si la muestra cubre la
# tabla, con el valor de `median()`. El valor de la consolidada no se compara
# aca: es otra forma, con su propio redondeo.
.mcm_comprobar <- function(medianas, columnas, metodo_cte, datos = NULL) {
  solo <- medianas$solo_agregados
  con <- medianas$con_muestra
  # `TABLESAMPLE SYSTEM` muestrea por paginas, y en `solo_agregados` cada
  # consulta saca la suya: alguna vez una vuelve con menos de la mitad de lo
  # pedido (1 de 30 corridas con 20.000 filas) y el paquete lo declara con este
  # motivo en todas las metricas de esa columna. Esa columna no se compara; las
  # demas si, y tiene que quedar al menos una.
  declarada <- "muestra_inestable:tablesample_system_tamano_insuficiente"
  corta <- vapply(columnas, function(columna) {
    identical(solo[columna, "motivo"], declarada) ||
      identical(con[columna, "motivo"], declarada)
  }, logical(1L))
  expect_true(any(!corta))
  metodo_cte <- setdiff(metodo_cte, columnas[corta])
  columnas <- columnas[!corta]
  for (columna in columnas) {
    expect_identical(solo[columna, "estado"], "estimado", info = columna)
    expect_identical(
      solo[columna, c("estado", "error_esperado")],
      con[columna, c("estado", "error_esperado")], info = columna
    )
  }
  for (columna in metodo_cte) {
    expect_match(solo[columna, "metodo"], "^cte_ventana", info = columna)
    if (!is.null(datos)) {
      referencia <- as.numeric(stats::median(datos[[columna]], na.rm = TRUE))
      expect_identical(solo[columna, "valor"], referencia, info = columna)
      expect_identical(solo[columna, "valor"], con[columna, "valor"],
                       info = columna)
    }
  }
  invisible(NULL)
}

# PostgreSQL de prueba, con el patron de salto de las demas pruebas: si no
# conecta, se saltea.
.mcm_con_pg <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  con
}

# La mediana con `metricas = c("basicos", "mediana")` -sin `validos`, que da
# el conteo- tiene que ser la misma, registro por registro, que con todas las
# metricas: estado, error esperado, metodo, SQL y valor.
.mcm_comprobar_sin_conteo <- function(conexion, tabla, esperado,
                                      sin_valores = character()) {
  perfil <- function(...) suppressWarnings(suppressMessages(perfilar_dbi(
    conexion, tabla, proteger_datos_personales = FALSE, ...
  )))
  completo <- perfil()
  parcial <- perfil(metricas = c("basicos", "mediana"))
  campos <- c("columna", "estado", "error_esperado", "motivo", "metodo", "sql")
  mediana <- function(p) {
    s <- p$resumen_tabla$sql
    s <- s[s$metrica == "mediana", campos, drop = FALSE]
    rownames(s) <- NULL
    s
  }
  expect_identical(mediana(parcial), mediana(completo))
  columnas <- parcial$resumen_tabla$columnas
  registro <- mediana(parcial)
  for (columna in names(esperado)) {
    expect_identical(
      as.numeric(columnas$mediana[columnas$columna == columna]),
      esperado[[columna]], info = columna
    )
    expect_identical(
      registro$estado[registro$columna == columna], "calculado", info = columna
    )
  }
  for (columna in sin_valores) {
    expect_identical(
      registro$estado[registro$columna == columna], "sin_valores",
      info = columna
    )
  }
  invisible(parcial)
}

# Los registros que afirman un valor -`calculado`, `estimado`,
# `observado_muestra`- y lo publican ausente. El valor puede venir en el campo
# exacto o en el de fecha.
.mcm_afirmados_sin_valor <- function(perfil) {
  sql <- perfil$resumen_tabla$sql
  columnas <- perfil$resumen_tabla$columnas
  alternos <- list(
    minimo = c("minimo", "minimo_exacto", "minimo_fecha"),
    maximo = c("maximo", "maximo_exacto", "maximo_fecha"),
    media = c("media", "media_fecha"), mediana = c("mediana", "mediana_fecha")
  )
  afirma <- sql$estado %in% c("calculado", "estimado", "observado_muestra") &
    sql$metrica %in% names(columnas)
  ausente <- vapply(seq_len(nrow(sql)), function(i) {
    if (!afirma[[i]]) return(FALSE)
    campos <- if (is.null(alternos[[sql$metrica[[i]]]])) sql$metrica[[i]] else
      alternos[[sql$metrica[[i]]]]
    campos <- intersect(campos, names(columnas))
    fila <- columnas$columna == sql$columna[[i]]
    all(vapply(campos, function(campo) all(is.na(columnas[[campo]][fila])),
               logical(1L)))
  }, logical(1L))
  sql[ausente, c("columna", "metrica", "estado", "metodo"), drop = FALSE]
}

# Columnas vacias de varios tipos, una de un solo valor, una sin repetidos y una
# de texto. Las fechas van como texto ISO, que todos los motores leen.
.mcm_tabla_vacias <- function(conexion, tabla, filas = 60L) {
  set.seed(20261106)
  try(DBI::dbExecute(conexion, paste0("DROP TABLE ", tabla)), silent = TRUE)
  senas <- .senas_conexion_dbi(conexion)
  doble <- if (grepl("sql server|mssql", senas)) "FLOAT" else if (
    grepl("postgres|pqconnection", senas)) "DOUBLE PRECISION" else "DOUBLE"
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE ", tabla, " (vacia ", doble, ", entera_vacia INTEGER,",
    " decimal_vacio DECIMAL(10,2), texto_vacio VARCHAR(20), fecha_vacia DATE,",
    " uno ", doble, ", unicos ", doble, ", texto VARCHAR(20))"
  ))
  filas_sql <- sprintf(
    "(NULL,NULL,NULL,NULL,NULL,%s,%s,'v%d')",
    c("2.5", rep("NULL", filas - 1L)), sprintf("%.17e", stats::rnorm(filas)),
    sample(1:5, filas, replace = TRUE)
  )
  sql <- paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas_sql, collapse = ",")
  )
  if (inherits(conexion, "OdbcConnection")) {
    DBI::dbExecute(conexion, sql, immediate = TRUE)
  } else {
    DBI::dbExecute(conexion, sql)
  }
  invisible(NULL)
}

# El mismo recorrido para cualquier conexion: caminos y juegos de metricas.
.mcm_recorrer_afirmados <- function(conexion, tabla) {
  caminos <- list(
    list(universo = "tabla_completa"),
    list(universo = "tabla_completa", estrategia_mediana = "aproximada_motor"),
    list(universo = "muestra_motor", muestra_motor = 1000L,
         bloque_muestra = "solo_agregados"),
    list(universo = "muestra_motor", muestra_motor = 1000L,
         bloque_muestra = "con_muestra")
  )
  juegos <- list(NULL, "basicos", "mediana", c("basicos", "mediana"),
                 "desvio", "moda", "distintos")
  hallados <- list()
  for (camino in caminos) for (metricas in juegos) {
    argumentos <- c(
      list(conexion, tabla, proteger_datos_personales = FALSE), camino
    )
    if (!is.null(metricas)) argumentos$metricas <- metricas
    perfil <- suppressWarnings(suppressMessages(
      do.call(perfilar_dbi, argumentos)
    ))
    malos <- .mcm_afirmados_sin_valor(perfil)
    if (nrow(malos)) hallados[[length(hallados) + 1L]] <- malos
  }
  if (length(hallados)) do.call(rbind, hallados) else
    data.frame(columna = character(), metrica = character())
}
