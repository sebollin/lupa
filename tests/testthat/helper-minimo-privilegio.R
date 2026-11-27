# Piezas comunes de las pruebas con una credencial de minimo privilegio (solo
# `SELECT` sobre las tablas de la prueba): las mismas reglas y las mismas
# tablas en cada motor, y lo que se compara entre el dueno y esa credencial.

.minp_tablas <- c("lupa_mdi_min_a", "lupa_mdi_min_b")

.minp_clave <- function() {
  paste0(paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = ""), "Aa1")
}

.minp_crear_tablas <- function(con) {
  for (t in .minp_tablas) {
    try(DBI::dbExecute(con, paste0("DROP TABLE ", t)), silent = TRUE)
  }
  DBI::dbExecute(con, paste0("CREATE TABLE ", .minp_tablas[[1L]], " (id INT, x INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", .minp_tablas[[1L]],
                             " VALUES (1, 1), (2, 2), (3, NULL), (4, 5), (5, 3)"))
  DBI::dbExecute(con, paste0("CREATE TABLE ", .minp_tablas[[2L]], " (id INT, y INT)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", .minp_tablas[[2L]],
                             " VALUES (1, 10), (2, NULL), (3, 30), (3, 90)"))
}

.minp_borrar_tablas <- function(con) {
  for (t in .minp_tablas) {
    try(DBI::dbExecute(con, paste0("DROP TABLE ", t)), silent = TRUE)
  }
}

.minp_modelo <- function() {
  nuc <- metricas_nucleo()
  a <- .minp_tablas[[1L]]
  b <- .minp_tablas[[2L]]
  inst <- function(met, nombre, ent, atr, ...) {
    instanciar(especializar(met, nombre, ...), ent, atr, nombre_instancia = nombre)
  }
  modelo(
    inst(nuc$NoNulo, "NN", a, "x"),
    inst(nuc$ValoresPosiblesPorComprension, "R", a, "x", minimo = 0, maximo = 3),
    inst(nuc$ReglaIntegridadIntraEntidad, "P", a, "x", regla = ~ is.na(x) | x > 1),
    inst(nuc$NoNulo, "NB", b, "y"),
    inst(nuc$ReglaIntegridadInterEntidad, "FK", c(a, b), c("id", "id"))
  )
}

# Mide con las dos credenciales y compara: las mismas cifras y ninguna metrica
# sin medir; el plan anuncia las sentencias que la corrida manda; y el resto de
# las funciones de la conexion da lo mismo. Devuelve las dos mediciones.
.minp_comparar <- function(duenio, minimo, motor) {
  mod <- .minp_modelo()
  medir <- function(con) {
    suppressWarnings(medir_dbi(mod, con, id_medicion = "m",
                               fecha = as.Date("2026-01-01")))
  }
  m_d <- medir(duenio)
  m_m <- medir(minimo)
  cifras <- function(m) paste(m$metrica_instanciada, m$numerador, m$denominador)
  expect_identical(cifras(m_m), cifras(m_d), info = motor)
  expect_identical(nrow(m_m), 5L, info = motor)
  cob <- attr(m_m, "cobertura_metricas", exact = TRUE)
  expect_false(any(cob$estado %in% "no_medible"), info = motor)
  plan <- plan_medicion_dbi(mod, minimo)
  expect_identical(attr(m_m, "ejecucion_dbi", exact = TRUE)$consultas,
                   sum(attr(plan, "consultas", exact = TRUE)), info = motor)
  p_d <- suppressWarnings(perfilar_dbi(duenio, .minp_tablas[[1L]]))
  p_m <- suppressWarnings(perfilar_dbi(minimo, .minp_tablas[[1L]]))
  columnas <- c("columna", "n", "n_faltantes", "n_distintos", "minimo", "maximo", "media")
  expect_identical(p_m$resumen_tabla$columnas[, columnas],
                   p_d$resumen_tabla$columnas[, columnas], info = motor)
  b_d <- conformidad_dbi(duenio, mod)
  b_m <- conformidad_dbi(minimo, mod)
  expect_identical(b_m$conformidad, b_d$conformidad, info = motor)
  rel <- function(con) {
    r <- suppressWarnings(suppressMessages(relaciones_coleccion(
      coleccion(con, .minp_tablas),
      pares = data.frame(tabla_1 = .minp_tablas[[1L]], tabla_2 = .minp_tablas[[2L]])
    )))
    paste(r$relaciones$columna_tabla1, r$relaciones$columna_tabla2, r$relaciones$cardinalidad)
  }
  expect_identical(rel(minimo), rel(duenio), info = motor)
  invisible(list(duenio = m_d, minimo = m_m))
}
