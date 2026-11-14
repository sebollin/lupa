# Una dependencia funcional que casi se cumple es el hallazgo
# `relacion_funcional_columnas`, con las filas que la rompen en su traza.
#
# Antes quedaba solo en `perfil$dependencias` -`"MO" -> "Montevideo" | "Salto"`,
# cumplimiento 0,995- sin hallazgo y sin filas, aunque la ayuda de
# `detectar_dependencias()` promete que una dependencia aproximada senala las
# filas minoritarias que pueden ser errores de carga. Las filas se comparan con
# un calculo independiente, escrito aca sin usar el del paquete.

# Corre en la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.rf_fecha <- as.POSIXct("2026-01-01", tz = "UTC")

# 1000 filas, cinco codigos de 200 filas con su departamento; `n_rotas` filas
# de `MO` dicen `Salto`, y las posiciones vacias se dejan en `NA`.
.rf_tabla <- function(n_rotas, vacio_dependiente = integer(),
                      vacio_determinante = integer()) {
  codigo <- rep(c("MO", "SA", "PA", "RI", "CA"), each = 200L)
  departamento <- unname(c(
    MO = "Montevideo", SA = "Salto", PA = "Paysandu", RI = "Rivera",
    CA = "Canelones"
  )[codigo])
  departamento[c(3L, 50L, 120L, 180L, 199L, 7L, 77L)[seq_len(n_rotas)]] <- "Salto"
  departamento[vacio_dependiente] <- NA
  codigo[vacio_determinante] <- NA
  data.frame(codigo = codigo, departamento = departamento,
             stringsAsFactors = FALSE)
}

# Las filas con las dos columnas presentes cuyo dependiente no es el mas
# frecuente de su grupo; con empate, el que aparece primero.
.rf_a_mano <- function(x, y) {
  validos <- which(!is.na(x) & !is.na(y))
  rompen <- logical(length(x))
  for (grupo in unique(x[validos])) {
    filas <- validos[x[validos] == grupo]
    valores <- unique(y[filas])
    modal <- valores[which.max(tabulate(match(y[filas], valores)))]
    rompen[filas[y[filas] != modal]] <- TRUE
  }
  which(rompen)
}

.rf_hallazgos <- function(perfil) {
  h <- perfil$hallazgos
  h[as.character(h$tipo_hallazgo) == "relacion_funcional_columnas", , drop = FALSE]
}

# Los hallazgos son exactamente las dependencias publicadas que no son
# exactas, en su orden, con sus cifras y con las filas del calculo de arriba.
.rf_esperar_coherente <- function(perfil, datos) {
  dependencias <- perfil$dependencias
  no_exactas <- dependencias[!dependencias$exacta, , drop = FALSE]
  h <- .rf_hallazgos(perfil)
  expect_identical(
    as.character(h$columna),
    paste(no_exactas$determinante, no_exactas$dependiente, sep = ",")
  )
  for (i in seq_len(nrow(h))) {
    traza <- h$trazabilidad[[i]]
    expect_identical(
      traza$columnas,
      c(no_exactas$determinante[[i]], no_exactas$dependiente[[i]])
    )
    expect_identical(as.character(h$severidad[[i]]), "sospechoso")
    expect_identical(h$unidad_conteo[[i]], "fila")
    expect_equal(h$n_evaluados[[i]], no_exactas$n_evaluados[[i]])
    expect_equal(h$n_afectados[[i]], no_exactas$n_violaciones[[i]])
    expect_equal(traza$total, h$n_afectados[[i]])
    expect_identical(
      traza$indices_fila,
      .rf_a_mano(datos[[traza$columnas[[1L]]]], datos[[traza$columnas[[2L]]]])
    )
  }
  invisible(h)
}

test_that("el hallazgo sale si y solo si la dependencia se publica sin ser exacta, en los bordes del umbral", {
  casos <- list(
    list(datos = .rf_tabla(5L), umbral = 0.995, hallazgos = 2L),
    list(datos = .rf_tabla(6L), umbral = 0.995, hallazgos = 0L),
    list(datos = .rf_tabla(5L, vacio_dependiente = 500L), umbral = 0.995,
         hallazgos = 0L),
    list(datos = .rf_tabla(5L, vacio_determinante = c(600L, 601L)),
         umbral = 0.995, hallazgos = 0L),
    list(datos = .rf_tabla(4L, vacio_dependiente = c(500L, 501L)),
         umbral = 0.995, hallazgos = 2L),
    list(datos = .rf_tabla(6L), umbral = 0.99, hallazgos = 2L),
    # Control: una dependencia exacta se publica y no es un hallazgo.
    list(datos = .rf_tabla(0L), umbral = 0.995, hallazgos = 0L)
  )
  for (caso in casos) {
    perfil <- perfilar(caso$datos, fecha = .rf_fecha,
                       umbral_dependencia = caso$umbral)
    h <- .rf_esperar_coherente(perfil, caso$datos)
    expect_identical(nrow(h), caso$hallazgos)
  }
  # El ejemplo, a mano: las cinco filas plantadas, en los dos sentidos.
  perfil <- perfilar(.rf_tabla(5L), fecha = .rf_fecha)
  h <- .rf_hallazgos(perfil)
  expect_identical(as.character(h$columna),
                   c("codigo,departamento", "departamento,codigo"))
  for (traza in h$trazabilidad) {
    expect_identical(traza$indices_fila, c(3L, 50L, 120L, 180L, 199L))
    expect_identical(traza$estado, "disponible")
    expect_identical(traza$alcance, "completo")
  }
  # Con vacios, las filas incompletas no entran en la traza.
  con_vacios <- perfilar(.rf_tabla(4L, vacio_dependiente = c(500L, 501L)),
                         fecha = .rf_fecha)
  traza <- .rf_hallazgos(con_vacios)$trazabilidad[[1L]]
  expect_identical(traza$indices_fila, c(3L, 50L, 120L, 180L))
  expect_equal(.rf_hallazgos(con_vacios)$n_evaluados[[1L]], 998)
})

test_that("los dos sentidos son dos hallazgos, cada uno con las filas que lo rompen", {
  datos <- .rf_tabla(0L)
  # `Durazno` no esta en otro codigo: rompe codigo -> departamento y no al reves.
  datos$departamento[c(3L, 50L)] <- "Durazno"
  # `ZZ` no esta en otra fila: rompe departamento -> codigo y no al reves.
  datos$codigo[120L] <- "ZZ"
  perfil <- perfilar(datos, fecha = .rf_fecha)
  h <- .rf_esperar_coherente(perfil, datos)
  expect_identical(nrow(h), 2L)
  filas <- stats::setNames(
    lapply(h$trazabilidad, `[[`, "indices_fila"), h$columna
  )
  expect_identical(filas[["codigo,departamento"]], c(3L, 50L))
  expect_identical(filas[["departamento,codigo"]], 120L)
})

test_that("con empate en la moda de un grupo rompe la pareja que aparece despues", {
  base <- rep(c("AA", "BB", "CC", "DD"), each = 300L)
  dependiente <- unname(c(AA = "x1", BB = "x2", CC = "x3", DD = "x4")[base])
  for (zz in list(c("a", "a", "a", "b", "b", "b"),
                  c("b", "b", "b", "a", "a", "a"),
                  c("a", "b", "b", "a", "a", "b"))) {
    datos <- data.frame(det = c(base, rep("ZZ", 6L)), dep = c(dependiente, zz),
                        stringsAsFactors = FALSE)
    perfil <- perfilar(datos, fecha = .rf_fecha)
    h <- .rf_esperar_coherente(perfil, datos)
    expect_identical(as.character(h$columna), "det,dep")
    # Tres y tres: cuenta tres, y son las de la pareja que aparece despues.
    perdedora <- setdiff(c("a", "b"), zz[[1L]])
    expect_identical(h$trazabilidad[[1L]]$indices_fila,
                     1200L + which(zz == perdedora))
  }
})

test_that("con tres valores en un grupo rompen las dos minorias", {
  datos <- .rf_tabla(0L)
  datos$departamento[c(10L, 20L)] <- "Salto"
  datos$departamento[30L] <- "Rivera"
  perfil <- perfilar(datos, fecha = .rf_fecha)
  h <- .rf_esperar_coherente(perfil, datos)
  directa <- h[h$columna == "codigo,departamento", , drop = FALSE]
  expect_identical(directa$trazabilidad[[1L]]$indices_fila, c(10L, 20L, 30L))
  expect_match(directa$evidencia, '"MO" -> "Montevideo" | "Salto" | "Rivera"',
               fixed = TRUE)
})

test_that("con muestra, las filas son de la tabla si se verifico y de la muestra si no", {
  n <- 4000L
  codigo <- rep(c("MO", "SA", "PA", "RI", "CA"), length.out = n)
  departamento <- unname(c(MO = "Montevideo", SA = "Salto", PA = "Paysandu",
                           RI = "Rivera", CA = "Canelones")[codigo])
  departamento[c(11L, 401L, 1201L, 2001L, 2801L, 3601L, 3991L)] <- "Durazno"
  datos <- data.frame(codigo = codigo, departamento = departamento,
                      stringsAsFactors = FALSE)
  # Verificada sobre la tabla: las siete filas y alcance completo.
  verificada <- perfilar(datos, fecha = .rf_fecha, muestra = 1000)
  expect_true(isTRUE(attr(verificada$dependencias, "muestreado")))
  h <- .rf_esperar_coherente(verificada, datos)
  expect_identical(nrow(h), 1L)
  expect_identical(h$trazabilidad[[1L]]$alcance, "completo")
  expect_equal(h$n_evaluados[[1L]], n)
  # Sin presupuesto para recalcular: cifras y filas de la muestra, en su
  # posicion de la tabla, y el alcance lo dice.
  de_muestra <- perfilar(datos, fecha = .rf_fecha, muestra = 1000,
                         max_trabajo_dependencias = 2000)
  expect_false(any(attr(de_muestra$dependencias, "verificadas_tabla_completa")))
  h <- .rf_hallazgos(de_muestra)
  expect_identical(nrow(h), 1L)
  traza <- h$trazabilidad[[1L]]
  expect_identical(traza$alcance, "muestra_dependencias")
  filas_muestra <- lupa:::.muestrear_filas_dispersas(
    n, attr(de_muestra$dependencias, "filas_analizadas")
  )$valores
  esperadas <- filas_muestra[.rf_a_mano(
    datos$codigo[filas_muestra], datos$departamento[filas_muestra]
  )]
  # La guarda de la prueba no es vacia: la muestra vio alguna fila rota.
  expect_gt(length(esperadas), 0L)
  expect_identical(traza$indices_fila, esperadas)
  expect_equal(traza$total, h$n_afectados[[1L]])
  expect_equal(h$n_evaluados[[1L]], length(filas_muestra))
  expect_match(h$evidencia[[1L]], "de la muestra de 1000 filas", fixed = TRUE)
})

test_that("el tope de filas trunca la traza y la clave declarada localiza las filas", {
  datos <- .rf_tabla(5L)
  datos$id <- sprintf("k%04d", seq_len(nrow(datos)))
  perfil <- perfilar(datos, fecha = .rf_fecha, max_filas_hallazgo = 2L,
                     clave = "id")
  traza <- .rf_hallazgos(perfil)$trazabilidad[[1L]]
  expect_identical(traza$estado, "truncada")
  expect_equal(traza$total, 5)
  expect_identical(traza$indices_fila, c(3L, 50L))
  expect_identical(traza$localizador, "clave_declarada")
  expect_identical(traza$claves$id, c("k0003", "k0050"))
  sin_tope <- perfilar(.rf_tabla(5L), fecha = .rf_fecha,
                       max_filas_hallazgo = Inf)
  traza <- .rf_hallazgos(sin_tope)$trazabilidad[[1L]]
  expect_identical(traza$estado, "disponible")
  expect_identical(traza$mostrados, 5L)
})

test_that("la tabla de dependencias y detectar_dependencias() no cambian", {
  datos <- .rf_tabla(5L)
  perfil <- perfilar(datos, fecha = .rf_fecha)
  exportada <- detectar_dependencias(datos)
  expect_identical(perfil$dependencias, exportada)
  expect_null(attr(exportada, "filas_rompen", exact = TRUE))
})

test_that("apagar o recortar la busqueda no publica el hallazgo como resuelto", {
  datos <- .rf_tabla(5L)
  datos$monto <- round(100 + (seq_len(nrow(datos)) * 37) %% 101, 2)
  perfil <- perfilar(datos, fecha = .rf_fecha)
  expect_identical(nrow(.rf_hallazgos(perfil)), 2L)
  apagado <- perfilar(datos, fecha = .rf_fecha + 60, analizar_dependencias = FALSE)
  expect_identical(nrow(.rf_hallazgos(apagado)), 0L)
  for (nuevo in list(
    apagado,
    perfilar(datos, fecha = .rf_fecha + 60, max_columnas_dependencias = 1L),
    perfilar(datos, fecha = .rf_fecha + 60, max_comparaciones_dependencias = 1L)
  )) {
    # Los que el perfil nuevo no trae, y solo esos, salen `no_evaluado`.
    faltan <- setdiff(.rf_hallazgos(perfil)$columna, .rf_hallazgos(nuevo)$columna)
    expect_gt(length(faltan), 0L)
    r <- as.data.frame(comparar_perfiles(perfil, nuevo))
    fila <- r[r$aspecto == "hallazgo" &
                r$valor_anterior %in% "relacion_funcional_columnas", , drop = FALSE]
    expect_setequal(fila$columna, faltan)
    expect_identical(unique(as.character(fila$cambio)), "no_evaluado")
  }
  r <- as.data.frame(comparar_perfiles(perfil, apagado))
  fila <- r[r$aspecto == "hallazgo", , drop = FALSE]
  expect_match(fila$descripcion[[1L]], "`analizar_dependencias`", fixed = TRUE)
  # Mover el umbral es una vara: la comparacion declara el cambio.
  movido <- perfilar(datos, fecha = .rf_fecha + 60, umbral_dependencia = 1)
  r <- as.data.frame(comparar_perfiles(perfil, movido))
  umbrales <- r[r$aspecto == "configuracion_umbrales", , drop = FALSE]
  expect_identical(nrow(umbrales), 1L)
  expect_match(umbrales$valor_anterior, "umbral_dependencia=0.995", fixed = TRUE)
  # Control: con los datos arreglados, el hallazgo si se resolvio.
  arreglados <- datos
  arreglados$departamento[c(3L, 50L, 120L, 180L, 199L)] <- "Montevideo"
  r <- as.data.frame(comparar_perfiles(
    perfil, perfilar(arreglados, fecha = .rf_fecha + 60)
  ))
  fila <- r[r$aspecto == "hallazgo", , drop = FALSE]
  expect_identical(unique(as.character(fila$cambio)), "resuelto")
})

test_that("un nombre de columna con coma no confunde las dos columnas", {
  datos <- .rf_tabla(5L)
  names(datos) <- c("cod,igo", "departamento")
  perfil <- perfilar(datos, fecha = .rf_fecha)
  h <- .rf_esperar_coherente(perfil, datos)
  expect_identical(nrow(h), 2L)
  expect_identical(h$trazabilidad[[1L]]$columnas, c("cod,igo", "departamento"))
  apagado <- perfilar(datos, fecha = .rf_fecha + 60, analizar_dependencias = FALSE)
  r <- as.data.frame(comparar_perfiles(perfil, apagado))
  fila <- r[r$aspecto == "hallazgo", , drop = FALSE]
  expect_identical(unique(as.character(fila$cambio)), "no_evaluado")
})

test_that("el plan no propone accion: el hallazgo queda en hallazgos_sin_accion", {
  datos <- .rf_tabla(5L)
  perfil <- perfilar(datos, fecha = .rf_fecha)
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(plan$hallazgo %in% "relacion_funcional_columnas"))
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_identical(
    sum(sin_accion$hallazgo == "relacion_funcional_columnas"), 2L
  )
})

test_that("la proteccion tapa la evidencia de una columna personal y conserva las filas", {
  datos <- .rf_tabla(5L)
  datos$id <- sprintf("k%04d", seq_len(nrow(datos)))
  protegido <- perfilar(datos, fecha = .rf_fecha, clave = "id",
                        proteger_datos_personales = TRUE,
                        columnas_personales = c("codigo", "id"))
  h <- .rf_hallazgos(protegido)
  expect_identical(nrow(h), 2L)
  expect_identical(unique(h$evidencia), "[evidencia protegida]")
  traza <- h$trazabilidad[[1L]]
  expect_identical(traza$indices_fila, c(3L, 50L, 120L, 180L, 199L))
  expect_identical(traza$columnas, c("codigo", "departamento"))
  expect_identical(traza$claves_protegidas, "id")
  expect_false(any(grepl("k0", unlist(traza$claves), fixed = TRUE)))
})

test_that("la guarda de la trazabilidad no avisa al construir ni al publicar", {
  datos <- .rf_tabla(5L)
  expect_no_warning(
    perfil <- perfilar(datos, fecha = .rf_fecha),
    class = "lupa_trazabilidad_incoherente"
  )
  expect_identical(nrow(.rf_hallazgos(perfil)), 2L)
  expect_no_warning(hallazgos(perfil), class = "lupa_trazabilidad_incoherente")
})
