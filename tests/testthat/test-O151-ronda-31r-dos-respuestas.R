# Ronda 31-R: dos funciones, dos respuestas sobre el mismo hecho. Cada prueba
# rehace su cifra a mano y falla contra el codigo anterior a la ronda.

.fecha_O151 <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("H1: con bloque_filas la mediana se calcula solo con valores finitos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  casos <- list(
    menos = c(-Inf, -Inf, -Inf, 10, 20, 30, 40),
    mas = c(1, 2, 3, Inf, Inf, Inf)
  )
  for (nombre in names(casos)) {
    v <- casos[[nombre]]
    DBI::dbWriteTable(con, "t", data.frame(v = v), overwrite = TRUE)
    p <- suppressWarnings(perfilar_dbi(
      con, "t", bloque_filas = 2L, proteger_datos_personales = FALSE
    ))
    fila <- p$resumen_tabla$columnas
    # A mano: la mediana de los finitos, como minimo, maximo y media de la
    # misma fila. Con los infinitos daria 10 e Inf.
    finitos <- v[is.finite(v)]
    expect_equal(fila$mediana[[1L]], stats::median(finitos))
    expect_equal(fila$media[[1L]], mean(finitos))
    expect_equal(fila$minimo[[1L]], min(finitos))
  }
})

test_that("H4: sin bloques, una columna con infinitos publica sus magnitudes finitas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  v <- c(-Inf, -Inf, 5, 10, 20, 30, 40, NA)
  finitos <- v[is.finite(v)]
  conexiones <- list(
    sqlite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = function() DBI::dbConnect(duckdb::duckdb())
  )
  for (motor in names(conexiones)) {
    con <- conexiones[[motor]]()
    DBI::dbWriteTable(con, "t", data.frame(v = v))
    # A mano: el motor guarda dos -Inf y ningun texto.
    if (identical(motor, "sqlite")) {
      tipos <- DBI::dbGetQuery(
        con, "SELECT SUM(typeof(v) = 'text') AS n FROM t"
      )
      expect_equal(tipos$n, 0)
    }
    p <- suppressWarnings(perfilar_dbi(con, "t", proteger_datos_personales = FALSE))
    fila <- p$resumen_tabla$columnas
    expect_equal(fila$minimo[[1L]], min(finitos))
    expect_equal(fila$maximo[[1L]], max(finitos))
    expect_equal(fila$media[[1L]], mean(finitos))
    expect_equal(fila$mediana[[1L]], stats::median(finitos))
    expect_equal(fila$desvio[[1L]], stats::sd(finitos))
    expect_equal(fila$n_negativos[[1L]], sum(finitos < 0))
    sql <- p$resumen_tabla$sql
    magnitudes <- sql[sql$metrica %in% c("minimo", "media", "mediana"), ]
    expect_true(all(magnitudes$estado == "calculado"))
    expect_false(any(grepl("no se pudo leer como numero", sql$motivo, fixed = TRUE)))
    cobertura <- p$resumen_tabla$cobertura
    expect_false(any(grepl("Causa medida", cobertura$motivo, fixed = TRUE)))
    # Por la sonda de magnitud -sin basicos-: el `-Inf` queda al principio del
    # orden y la mediana salia 10.
    p <- suppressWarnings(perfilar_dbi(
      con, "t", metricas = c("validos", "mediana", "desvio"),
      proteger_datos_personales = FALSE
    ))
    expect_equal(p$resumen_tabla$columnas$mediana[[1L]], stats::median(finitos))
    # Sin `MIN` ni `MAX`, un `Inf` y un `-Inf` solo se ven en `AVG`: `NaN` en
    # duckdb y nulo en SQLite. A mano, los finitos: media 25/3, un negativo.
    w <- c(-Inf, Inf, -5, 10, 20, NA)
    DBI::dbWriteTable(con, "u", data.frame(v = w))
    p <- suppressWarnings(perfilar_dbi(
      con, "u", incluir_valores = FALSE, proteger_datos_personales = FALSE
    ))
    fila <- p$resumen_tabla$columnas
    expect_equal(fila$media[[1L]], mean(w[is.finite(w)]))
    expect_equal(fila$n_negativos[[1L]], sum(w[is.finite(w)] < 0))
    DBI::dbDisconnect(con)
  }
})

test_that("H4: la causa 'guarda texto' se mide con typeof() y no se deduce", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE m (v INTEGER)")
  DBI::dbExecute(con, "INSERT INTO m VALUES (10), (20), ('abc'), (40), ('abc'), (60)")
  # A mano: dos valores guardados como texto.
  expect_equal(DBI::dbGetQuery(con, "SELECT SUM(typeof(v) = 'text') AS n FROM m")$n, 2)
  p <- suppressWarnings(perfilar_dbi(con, "m", proteger_datos_personales = FALSE))
  sql <- p$resumen_tabla$sql
  media <- sql[sql$metrica == "media", ]
  expect_equal(media$estado[[1L]], "no_disponible")
  expect_match(media$motivo[[1L]], "2 valores guardados como texto o blob", fixed = TRUE)
  cobertura <- p$resumen_tabla$cobertura
  divergencias <- cobertura[cobertura$estado == "divergencia", ]
  expect_true(nrow(divergencias) > 0L)
  expect_true(all(grepl("typeof()", divergencias$motivo, fixed = TRUE)))
})

test_that("H2: un factor con nivel NA no es clave en detectar_claves(), sugerir_clave() ni referencial()", {
  cod <- sprintf("K%02d", 1:40)
  cod[7] <- NA
  d <- data.frame(k = addNA(factor(cod)), v = c(1:20, 1:20))
  # A mano: 39 presentes distintos y un ausente; una clave exige no tener
  # ausentes.
  expect_equal(sum(is.na(as.character(d$k))), 1L)
  expect_equal(length(unique(cod[!is.na(cod)])), 39L)
  fila <- perfilar(d, fecha = .fecha_O151, analizar_dependencias = FALSE)$columnas
  expect_equal(fila$n_faltantes[fila$columna == "k"], 1)
  expect_false("k" %in% detectar_claves(d)$columnas)
  s <- sugerir_clave(d)
  expect_false(s$identifica[s$columna == "k"])
  expect_false(s$sin_faltantes[s$columna == "k"])
  expect_error(referencial(d, clave = "k"), "valores ausentes", fixed = TRUE)
  # La misma columna como factor comun da las mismas respuestas.
  comun <- data.frame(k = factor(cod), v = c(1:20, 1:20))
  expect_identical(sugerir_clave(comun), s)
  expect_identical(detectar_claves(comun)$columnas, detectar_claves(d)$columnas)
})

test_that("H3: aplicar() elimina las columnas que el perfil y el plan llaman duplicadas", {
  f <- factor(c("x", "y", NA, "x", "y", NA, "z", "x"))
  ct <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") +
    3600 * c(0, 1, 2, 3, 1, 2, 0, 5)
  tablas <- list(
    nivel_na = data.frame(a = addNA(f), b = f),
    posixlt = local({
      x <- data.frame(b = ct)
      x$a <- as.POSIXlt(ct, tz = "UTC")
      x[, c("a", "b")]
    })
  )
  for (nombre in names(tablas)) {
    d <- tablas[[nombre]]
    # A mano: el mismo contenido con la regla del paquete -el nivel NA es un
    # faltante, un POSIXlt vale por su instante-.
    expect_identical(as.character(d$a), as.character(d$b))
    p <- perfilar(d, fecha = .fecha_O151, analizar_dependencias = FALSE)
    expect_true("columnas_duplicadas" %in% p$hallazgos$tipo_hallazgo)
    plan <- planificar_limpieza(p, datos = d)
    plan$aplicar <- plan$estrategia == "eliminar_columna_duplicada"
    expect_equal(sum(plan$aplicar), 1L)
    r <- aplicar(plan, d, permitir_eliminacion = TRUE)
    expect_identical(r$registro$estado, "ejecutada")
    expect_equal(r$registro$n_cambiadas, 1)
    expect_equal(ncol(r$datos), 1L)
  }
})

test_that("H5: analizar()$distribuciones recibe los centinelas y el universo declarados", {
  n <- 40
  d <- data.frame(tiene_auto = rep(c("Si", "No"), length.out = n),
                  stringsAsFactors = FALSE)
  d$edad <- c(rep(c(20, 30, 40, 50, 60), length.out = n - 3), -999, -999, 999)
  d$marca <- ifelse(d$tiene_auto == "Si", rep(c("fiat", "ford", "kia"), length.out = n), NA)
  d$marca[c(2, 4, 6)] <- "tesla"
  d$km <- ifelse(d$tiene_auto == "Si", seq(1000, by = 500, length.out = n), NA)
  d$km[c(8, 10)] <- 999999
  aplica <- d$tiene_auto == "Si"
  a <- suppressWarnings(analizar(d, fecha = .fecha_O151, argumentos_perfil = list(
    sentinelas_numericos = c(-999, 999),
    aplicabilidad = list(marca = ~ tiene_auto == "Si", km = ~ tiene_auto == "Si"),
    analizar_dependencias = FALSE
  )))
  q <- a$distribuciones$cuantiles
  # A mano: la edad sin los centinelas declarados; el km y la marca dentro del
  # universo.
  edad <- d$edad[!d$edad %in% c(-999, 999)]
  expect_equal(q$valor[q$columna == "edad" & q$probabilidad == 0], min(edad))
  expect_equal(q$valor[q$columna == "edad" & q$probabilidad == 1], max(edad))
  expect_equal(q$valor[q$columna == "km" & q$probabilidad == 1],
               max(d$km[aplica], na.rm = TRUE))
  alcance <- a$distribuciones$alcance
  expect_equal(alcance$n_distintos_muestra[alcance$columna == "marca"],
               length(unique(stats::na.omit(d$marca[aplica]))))
  expect_false("tesla" %in% a$distribuciones$frecuencias$valor)
  # El perfil dice lo mismo.
  fila <- a$perfil$columnas
  expect_equal(fila$maximo[fila$columna == "edad"], max(edad))
  expect_equal(fila$n_distintos[fila$columna == "marca"], 3)
})

test_that("H6: analizar() no mide la NoNulo que la propuesta deja inactiva por disfrazados", {
  n <- 40
  d <- data.frame(
    edad = c(rep(c(20, 30, 40, 50, 60), length.out = n - 4), -999, -999, -999, NA),
    estado = c(rep(c("activo", "baja", "suspendido"), length.out = n - 6),
               "N/A", "N/A", "sin dato", "-", "-", NA),
    stringsAsFactors = FALSE
  )
  a <- suppressWarnings(analizar(d, fecha = .fecha_O151,
    argumentos_perfil = list(analizar_dependencias = FALSE)))
  fila <- a$perfil$columnas
  # A mano: 4 y 6 ausentes contando los disfrazados; 39/40 = 0.975 cuenta solo
  # el NA, que es la completitud que la propuesta llama sobrestimada.
  expect_equal(fila$n_faltantes_totales[fila$columna == "edad"], 4)
  expect_equal(fila$n_faltantes_totales[fila$columna == "estado"], 6)
  decision <- a$decision_medicion
  no_nulo <- decision[decision$metrica == "NoNulo", ]
  expect_equal(nrow(no_nulo), 2L)
  expect_true(all(no_nulo$estado == "faltantes_disfrazados"))
  expect_false(any(no_nulo$medida))
  medicion <- as.data.frame(a$medicion)
  expect_false(any(medicion$metrica == "NoNulo"))
  # La misma regla que `analizar()` ya aplicaba: medida si y solo si lista.
  expect_identical(decision$medida, decision$estado == "lista")
})

test_that("H8: distribucion_valores() cuenta un POSIXlt como su POSIXct", {
  ct <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC") +
    3600 * c(0, 1, 2, 3, 1, 2, 0, 5)
  ct[4] <- NA
  d <- data.frame(ct = ct)
  d$lt <- as.POSIXlt(ct)
  # A mano: 7 presentes y 4 instantes distintos.
  expect_equal(sum(!is.na(ct)), 7L)
  expect_equal(length(unique(ct[!is.na(ct)])), 4L)
  dv <- distribucion_valores(d)
  alcance <- dv$alcance
  expect_equal(alcance$estado[alcance$columna == "lt"], "calculada")
  expect_equal(alcance$n_analizados[alcance$columna == "lt"], 7)
  expect_equal(alcance$n_distintos_muestra[alcance$columna == "lt"], 4)
  fr <- dv$frecuencias
  expect_identical(fr$valor[fr$columna == "lt"], fr$valor[fr$columna == "ct"])
  expect_identical(fr$frecuencia[fr$columna == "lt"], fr$frecuencia[fr$columna == "ct"])
})

test_that("H9: desambiguar_fecha_en_origen cuenta en la unidad de su hallazgo", {
  amb <- format(as.Date(sprintf("2023-%02d-%02d", rep(1:10, 4), rep(1:4, each = 10))),
                "%d/%m/%Y")
  amb[c(5, 6, 7)] <- NA
  d <- data.frame(amb = amb, stringsAsFactors = FALSE)
  p <- perfilar(d, fecha = .fecha_O151, analizar_dependencias = FALSE)
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "formato_fecha_ambiguo", ]
  # A mano: dos formatos candidatos, d/m y m/d.
  expect_equal(h$n_afectados, 2)
  expect_identical(h$unidad_conteo, "formato")
  plan <- planificar_limpieza(p, datos = d)
  accion <- plan[plan$estrategia == "desambiguar_fecha_en_origen", ]
  expect_identical(accion$unidad_conteo, "formato")
  expect_equal(accion$n_afectadas, 2)
})

test_that("H10: sin moda la moda es NA tambien con la proteccion activa", {
  d <- data.frame(
    correo = sprintf("persona%02d@ejemplo.com.uy", 1:12),
    correo_rep = sprintf("persona%02d@ejemplo.com.uy", c(1:11, 1)),
    stringsAsFactors = FALSE
  )
  # A mano: el primero no repite ningun valor, el segundo repite uno.
  expect_equal(max(table(d$correo)), 1L)
  expect_equal(max(table(d$correo_rep)), 2L)
  fila <- perfilar(d, fecha = .fecha_O151, analizar_dependencias = FALSE)$columnas
  expect_true(all(fila$dato_personal_protegido))
  expect_true(is.na(fila$moda[fila$columna == "correo"]))
  expect_equal(fila$frecuencia_moda[fila$columna == "correo"], 1)
  expect_identical(fila$moda[fila$columna == "correo_rep"], "[valor protegido]")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  p <- suppressWarnings(perfilar_dbi(con, "t"))
  for (columnas in list(p$resumen_tabla$columnas, p$perfil_muestra$columnas)) {
    expect_true(is.na(columnas$moda[columnas$columna == "correo"]))
    expect_identical(columnas$moda[columnas$columna == "correo_rep"], "[valor protegido]")
  }
})

test_that("A-1: una conversion regional que no se ejecuta con ninguna configuracion sale bloqueada", {
  txt <- c("1.234,56", "1,5", "100.000", "1e5", "2,5", "3,25", "0,1",
           "1.000.000,00", "12,00", "7", "8,5", "9,75")
  d <- data.frame(ID = seq_along(txt), txt = txt, stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(d, fecha = .fecha_O151), d)
  accion <- plan[plan$estrategia == "convertir_numero_regional", ]
  expect_equal(nrow(accion), 1L)
  # A mano: `1e5` no responde al formato regional con ninguna lectura del
  # punto ni de la coma.
  for (punto in c("miles", "decimal")) for (coma in c("miles", "decimal")) {
    parametros <- accion$parametros[[1L]]
    parametros$punto_sin_coma <- punto
    parametros$coma_sin_punto <- coma
    expect_error(.convertir_numero_regional(d$txt, parametros), "formato")
  }
  expect_identical(as.character(accion$estado), "bloqueada")
  expect_false(accion$aplicar)
  # Control: la ambiguedad que se resuelve configurando sigue `lista`.
  ambigua <- data.frame(
    txt = c("1.234", "2.500", "3.750", "10.000", "12.345", "7.001", "8.002", "9.003"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(ambigua, fecha = .fecha_O151), ambigua)
  accion <- plan[plan$estrategia == "convertir_numero_regional", ]
  expect_match(accion$parametros[[1L]]$motivo_no_reversible, "punto_sin_coma", fixed = TRUE)
  expect_identical(as.character(accion$estado), "lista")
})
