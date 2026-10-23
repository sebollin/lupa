# Ronda 32-D: tres decisiones de Sebastian del 2026-10-13 (D2, D3 y D6). No son
# defectos refutados sino cambios de promesa; cada prueba rehace su cifra a mano
# y falla contra el codigo anterior a la ronda.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.fecha_O156 <- as.POSIXct("2026-01-01", tz = "UTC")

# Un codigo de catalogo que se repite: 1..284 con mucho peso en los primeros, como
# `EstCod` en un banco real. La regla de numeracion no lo reconoce (repite) y los
# limites de Tukey lo acusan.
.datos_codigo_O156 <- function(n = 300L) {
  set.seed(3256)
  data.frame(
    estcod = sample(1:284, n, replace = TRUE, prob = exp(-(1:284) / 25)),
    actividad = sample(1:9999, n, replace = TRUE, prob = 1 / (1:9999)^0.3),
    edad = sample(18:90, n, replace = TRUE),
    grupo = rep(c("a", "b"), length.out = n)
  )
}

.tukey_O156 <- function(x) {
  q <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
  iqr <- q[[2L]] - q[[1L]]
  sum(x < q[[1L]] - 1.5 * iqr | x > q[[2L]] + 1.5 * iqr)
}

test_that("D2: una columna declarada en columnas_codigo no recibe Tukey, Benford ni media", {
  d <- .datos_codigo_O156()
  # A mano: el codigo tiene outliers de Tukey que no significan nada.
  esperados <- .tukey_O156(d$estcod)
  expect_gt(esperados, 0L)
  sin <- perfilar(d, fecha = .fecha_O156, analizar_dependencias = FALSE,
                  proteger_datos_personales = FALSE)
  expect_equal(sin$columnas$n_outliers[sin$columnas$columna == "estcod"], esperados)

  for (forma in c("entero", "texto", "factor")) {
    dd <- d
    if (forma == "texto") dd$estcod <- as.character(dd$estcod)
    if (forma == "factor") dd$estcod <- factor(dd$estcod)
    p <- perfilar(dd, fecha = .fecha_O156, analizar_dependencias = FALSE,
                  proteger_datos_personales = FALSE,
                  columnas_codigo = c("estcod", "actividad"))
    fila <- p$columnas[p$columnas$columna == "estcod", ]
    expect_true(is.na(fila$n_outliers), info = forma)
    expect_true(is.na(fila$media) && is.na(fila$mediana) && is.na(fila$desvio),
                info = forma)
    expect_identical(fila$estado_resumen_cuantitativo, "omitidos_codigo")
    # Lo que describe los valores se conserva: el tramo de codigos, a mano.
    expect_equal(fila$minimo, min(d$estcod), info = forma)
    expect_equal(fila$maximo, max(d$estcod), info = forma)
    expect_equal(fila$n_distintos, length(unique(d$estcod)), info = forma)
    h <- p$hallazgos
    expect_false(any(h$columna == "estcod" &
                       h$tipo_hallazgo %in% c("outliers", "posible_centinela_numerico")),
                 info = forma)
    cb <- p$cobertura_diagnosticos
    fila_cb <- cb[cb$columna == "estcod" & cb$diagnostico == "outliers", ]
    expect_equal(nrow(fila_cb), 1L, info = forma)
    expect_match(fila_cb$motivo, "columnas_codigo", fixed = TRUE)
    expect_match(fila_cb$como_resolverlo, "columnas_codigo", fixed = TRUE)
    # La columna no declarada sigue midiendose como antes.
    expect_equal(p$columnas$media[p$columnas$columna == "edad"], mean(d$edad))
  }

  # Benford: `actividad` entera tiene 3 ordenes de magnitud y 300 positivos, y
  # declarada no se evalua; el motivo nombra la declaracion y no otra cosa.
  expect_gte(log10(max(d$actividad) / min(d$actividad)), 3)
  p <- perfilar(d, fecha = .fecha_O156, analizar_dependencias = FALSE,
                proteger_datos_personales = FALSE, columnas_codigo = "actividad")
  cb <- p$cobertura_diagnosticos
  fila_b <- cb[cb$columna == "actividad" & cb$diagnostico == "ley_benford", ]
  expect_equal(nrow(fila_b), 1L)
  expect_match(fila_b$motivo, "columnas_codigo", fixed = TRUE)
  expect_identical(p$meta$benford$resultados$actividad$precondiciones_fallidas,
                   "codigo_declarado")
  expect_identical(p$meta$columnas_codigo, "actividad")

  expect_error(
    perfilar(d, analizar_dependencias = FALSE, columnas_codigo = "estcodd"),
    "columnas_codigo"
  )
})

test_that("D2: la declaracion vale igual en perfilar_por(), analizar() y la deriva", {
  d <- .datos_codigo_O156(120L)
  por <- perfilar_por(d, "grupo", columnas_codigo = "estcod",
                      analizar_dependencias = FALSE, fecha = .fecha_O156)
  h <- as.data.frame(por)
  expect_false(any(h$columna == "estcod" & h$tipo_hallazgo == "outliers"))
  # Agrupar POR la columna declarada la recorta de cada rebanada sin abortar.
  expect_no_error(suppressWarnings(perfilar_por(
    d[d$estcod <= 3, ], "estcod", columnas_codigo = "estcod",
    analizar_dependencias = FALSE, fecha = .fecha_O156
  )))

  a <- suppressWarnings(analizar(
    d, fecha = .fecha_O156,
    argumentos_perfil = list(analizar_dependencias = FALSE,
                             columnas_codigo = "estcod")
  ))
  expect_identical(a$perfil$columnas$estado_resumen_cuantitativo[
    a$perfil$columnas$columna == "estcod"], "omitidos_codigo")
  cq <- a$distribuciones$cuantiles
  cq <- cq[cq$columna == "estcod", ]
  # A mano: los extremos son el tramo de codigos; los intermedios no se publican.
  expect_equal(cq$valor[cq$probabilidad == 0], min(d$estcod))
  expect_equal(cq$valor[cq$probabilidad == 1], max(d$estcod))
  intermedios <- cq$probabilidad > 0 & cq$probabilidad < 1
  expect_true(all(is.na(cq$valor[intermedios])))
  expect_true(all(cq$estado[intermedios] == "codigo_declarado"))

  antes <- perfilar(d, nombre = "t", fecha = .fecha_O156,
                    analizar_dependencias = FALSE)
  despues <- perfilar(d, nombre = "t", fecha = .fecha_O156,
                      analizar_dependencias = FALSE, columnas_codigo = "estcod")
  deriva <- comparar_perfiles(antes, despues)
  fila <- deriva[deriva$aspecto == "configuracion_umbrales", ]
  expect_equal(nrow(fila), 1L)
  expect_match(as.character(fila$valor_actual), "columnas_codigo=estcod",
               fixed = TRUE)
  # La misma declaracion de los dos lados no es un cambio de vara.
  expect_equal(sum(comparar_perfiles(despues, despues)$aspecto ==
                     "configuracion_umbrales"), 0L)
})

test_that("D2: perfilar_dbi() la aplica tambien en resumen_tabla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  d <- .datos_codigo_O156(200L)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  p <- suppressWarnings(perfilar_dbi(
    con, "t", columnas_codigo = "estcod", proteger_datos_personales = FALSE
  ))
  fila <- p$resumen_tabla$columnas[p$resumen_tabla$columnas$columna == "estcod", ]
  expect_true(is.na(fila$media) && is.na(fila$mediana) && is.na(fila$desvio))
  expect_equal(fila$minimo, min(d$estcod))
  expect_equal(fila$maximo, max(d$estcod))
  # La columna no declarada conserva su media SQL, a mano.
  expect_equal(p$resumen_tabla$columnas$media[
    p$resumen_tabla$columnas$columna == "edad"], mean(d$edad))
  cb <- p$resumen_tabla$cobertura
  expect_true(any(cb$elemento == "estcod" & cb$estado == "no_aplica" &
                    grepl("columnas_codigo", cb$motivo, fixed = TRUE)))
  m <- p$perfil_muestra
  expect_true(is.na(m$columnas$n_outliers[m$columnas$columna == "estcod"]))
  expect_false(any(m$hallazgos$columna == "estcod" &
                     m$hallazgos$tipo_hallazgo == "outliers"))
  # Sin muestra no hay `perfilar()` que valide: el nombre se valida igual.
  expect_error(suppressWarnings(perfilar_dbi(
    con, "t", bloque_muestra = "solo_agregados", columnas_codigo = "estcodd"
  )), "columnas_codigo")
})

.tablas_O156 <- function() {
  n <- 30L
  t1 <- data.frame(
    depto = rep(c("Salto", "Rivera", "Paysandu"), length.out = n),
    edad = c(rep(c(20, 30, 40), length.out = n - 2L), NA, NA),
    nombre = c(rep(c("ana", "beto", "carla", "dani", "eva"),
                   length.out = n - 1L), NA),
    stringsAsFactors = FALSE
  )
  t1 <- rbind(t1, t1[1:2, ])
  t2 <- t1
  # Una columna nueva que ordena ANTES que las demas, mitad ausente.
  t2$apellido <- rep(c(NA, "perez", NA, "gomez"), length.out = nrow(t2))
  t2 <- t2[, c("apellido", "depto", "edad", "nombre")]
  list(t1 = t1, t2 = t2)
}

test_that("D3: el id de la sugerencia sale de su contenido y sobrevive a una columna nueva", {
  tablas <- .tablas_O156()
  proponer <- function(t) proponer_modelo(perfilar(
    t, nombre = "personas", fecha = .fecha_O156,
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ), t)
  p1 <- proponer(tablas$t1)
  p2 <- proponer(tablas$t2)
  # A mano: metrica y atributo.
  esperado <- ifelse(nzchar(p1$atributos),
                     paste0(p1$metrica, ":", p1$atributos), p1$metrica)
  expect_identical(p1$id_sugerencia, esperado)
  expect_true(all(p1$id_sugerencia %in% p2$id_sugerencia))
  clave <- function(p) paste(p$metrica, p$atributos)
  expect_identical(p2$id_sugerencia[match(clave(p1), clave(p2))],
                   p1$id_sugerencia)

  # Hasta el ultimo consumidor: la corrida 2 vuelve a proponer, elige por id el
  # modelo de la corrida 1, y una regla nombra la NoNulo de `edad`.
  ids <- p1$id_sugerencia[p1$estado == "lista"]
  regla <- "NoNulo:edad"
  correr <- function(p, datos, id, fecha) {
    p$incluir <- p$estado == "lista" & p$id_sugerencia %in% ids
    m <- medir(modelo_desde_propuesta(p), datos, id_medicion = id,
               fecha = as.POSIXct(fecha, tz = "UTC"))
    list(m = m, ev = evaluar(m, perfil_evaluacion(
      "P", regla_evaluacion("R", function(x) x > 0.8, metricas = regla)
    )))
  }
  r1 <- correr(p1, tablas$t1, "c1", "2026-01-31")
  r2 <- correr(p2, tablas$t2, "c2", "2026-02-28")
  for (r in list(r1, r2)) {
    medidas <- as.data.frame(r$m)
    medidas <- medidas[medidas$metrica_instanciada == regla, ]
    expect_identical(unique(medidas$atributo), "edad")
    # A mano: 30 de 32 filas tienen edad.
    expect_equal(mean(medidas$resultado), 30 / 32)
  }
  expect_true(regla %in% as.data.frame(tablero_calidad(r2$m))$metrica_instanciada)
  archivo <- tempfile(fileext = ".csv")
  on.exit(unlink(archivo), add = TRUE)
  guardar_historico(historico_calidad(r1$m, r1$ev, r2$m, r2$ev), archivo)
  deriva <- as.data.frame(detectar_deriva_calidad(leer_historico(archivo),
                                                  nivel = "regla"))
  expect_false(any(deriva$aspecto == "configuracion_modelo"))
  resultado <- deriva[deriva$aspecto == "resultado", ]
  expect_equal(nrow(resultado), 1L)
  expect_equal(resultado$delta, 0)
})

test_that("D3: el id escapa su sintaxis y desambigua sin la posicion", {
  d <- data.frame(a = c(1, NA, 3, 4), b = c(NA, "x", "y", "x"),
                  c = c(1, 1, NA, 2), stringsAsFactors = FALSE)
  names(d) <- c("x", "x", "a,b%#")
  p <- proponer_modelo(perfilar(d, fecha = .fecha_O156,
                                analizar_dependencias = FALSE,
                                proteger_datos_personales = FALSE), d)
  no_nulo <- p$id_sugerencia[p$metrica == "NoNulo"]
  # A mano: `,` `%` `#` escritos como %2C %25 %23; la segunda `x` lleva #2.
  expect_setequal(no_nulo, c("NoNulo:a%2Cb%25%23", "NoNulo:x", "NoNulo:x#2"))
  expect_false(anyDuplicated(p$id_sugerencia) > 0L)

  # Un nombre que no es UTF-8 valido, declarado `bytes`: el id no hereda la
  # marca -con ella `medir()` abortaba en su `rbind()`- y escribe el byte alto.
  nombre <- rawToChar(as.raw(c(0x6e, 0x6f, 0x74, 0x61, 0xe9)))
  Encoding(nombre) <- "bytes"
  b <- data.frame(id = 1:4, v = c("a", NA, "b", "a"), stringsAsFactors = FALSE)
  names(b)[2L] <- nombre
  pb <- proponer_modelo(perfilar(b, fecha = .fecha_O156,
                                 analizar_dependencias = FALSE,
                                 proteger_datos_personales = FALSE), b)
  id <- pb$id_sugerencia[pb$metrica == "NoNulo"]
  expect_identical(id, "NoNulo:nota%E9")
  expect_false(any(Encoding(pb$id_sugerencia) == "bytes"))
  pb$incluir <- pb$estado == "lista"
  medidas <- as.data.frame(medir(modelo_desde_propuesta(pb), b, id_medicion = "m"))
  # A mano: 3 de 4 presentes.
  expect_equal(mean(medidas$resultado[medidas$metrica_instanciada == id]), 3 / 4)

  rel <- data.frame(columna_tabla1 = "id", columna_tabla2 = "persona_id",
                    cobertura_tabla2_en_tabla1 = 1,
                    cobertura_tabla1_en_tabla2 = 0.9)
  t <- data.frame(persona_id = c(1, 2, 2, 4))
  pr <- proponer_modelo(perfilar(t, fecha = .fecha_O156,
                                 analizar_dependencias = FALSE,
                                 proteger_datos_personales = FALSE),
                        t, relaciones = rel,
                        entidades_relacion = c("per.sonas", "tramites"))
  inter <- pr$id_sugerencia[pr$metrica == "ReglaIntegridadInterEntidad"]
  expect_setequal(inter, c(
    "ReglaIntegridadInterEntidad:per%2Esonas.id,tramites.persona_id",
    "ReglaIntegridadInterEntidad:tramites.persona_id,per%2Esonas.id"
  ))
})

.datos_disfrazados_O156 <- function() {
  n <- 40
  data.frame(
    edad = c(rep(c(20, 30, 40, 50, 60), length.out = n - 4), -999, -999, -999, NA),
    estado = c(rep(c("activo", "baja", "suspendido"), length.out = n - 6),
               "N/A", "N/A", "sin dato", "-", "-", NA),
    stringsAsFactors = FALSE
  )
}

test_that("D6: la NoNulo con faltantes disfrazados se mide contandolos", {
  d <- .datos_disfrazados_O156()
  a <- suppressWarnings(analizar(d, fecha = .fecha_O156,
    argumentos_perfil = list(analizar_dependencias = FALSE)))
  propuesta <- a$propuesta_modelo
  no_nulo <- propuesta[propuesta$metrica == "NoNulo", , drop = FALSE]
  expect_true(all(no_nulo$incluir))
  expect_true(all(as.character(no_nulo$estado) == "lista"))
  expect_match(no_nulo$justificacion, "normalizan")
  nulos <- no_nulo$configuracion[no_nulo$atributos == "estado"][[1L]]$valores_nulos
  expect_setequal(nulos, c("N/A", "sin dato", "-"))
  expect_identical(
    no_nulo$configuracion[no_nulo$atributos == "edad"][[1L]]$valores_nulos, -999
  )
  tablero <- as.data.frame(a$tablero)
  tablero <- tablero[tablero$metrica == "NoNulo", ]
  # A mano: 1 NA y 3 -999 de 40; 1 NA y 5 disfrazados de 40.
  expect_equal(tablero$valor[tablero$objeto == "edad"], (40 - 4) / 40)
  expect_equal(tablero$valor[tablero$objeto == "estado"], (40 - 6) / 40)
  decision <- a$decision_medicion
  expect_true(all(decision$medida[decision$metrica == "NoNulo"]))

  # Sin los datos no se pueden identificar: se mide igual, con los crudos.
  crudo <- proponer_modelo(a$perfil)
  no_nulo <- crudo[crudo$metrica == "NoNulo", , drop = FALSE]
  expect_true(all(no_nulo$incluir))
  expect_true(all(as.character(no_nulo$estado) == "lista"))
  expect_match(no_nulo$justificacion, "valores crudos")
  expect_true(all(vapply(no_nulo$configuracion, function(x) {
    is.null(x$valores_nulos)
  }, logical(1L))))
  m <- as.data.frame(medir(modelo_desde_propuesta(no_nulo), d, id_medicion = "m"))
  # A mano: solo el NA, 39 de 40 en las dos columnas.
  expect_equal(mean(m$resultado[m$atributo == "edad"]), 39 / 40)
  expect_equal(mean(m$resultado[m$atributo == "estado"]), 39 / 40)
})

test_that("D3: un id con acento es el mismo texto bajo C y medir() no avisa", {
  anterior <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", anterior), add = TRUE)
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")))) {
    skip("No se pudo fijar el locale C.")
  }
  d <- data.frame(id = 1:4, v = c("x", NA, "y", "x"), stringsAsFactors = FALSE)
  names(d)[2L] <- "a\u00f1o"
  p <- proponer_modelo(perfilar(d, fecha = .fecha_O156,
                                analizar_dependencias = FALSE,
                                proteger_datos_personales = FALSE), d)
  id <- p$id_sugerencia[p$metrica == "NoNulo"]
  # A mano: `NoNulo:` y la enie en UTF-8 (c3 b1), no su escritura escapada.
  expect_identical(charToRaw(id), c(charToRaw("NoNulo:a"),
                                    as.raw(c(0xc3, 0xb1)), charToRaw("o")))
  p$incluir <- p$estado == "lista"
  # `medir()` nombraba las filas de su `rbind()` con los nombres de las
  # instancias y bajo C avisaba "unable to translate".
  expect_no_warning(
    m <- medir(modelo_desde_propuesta(p), d, id_medicion = "m")
  )
  m <- as.data.frame(m)
  expect_equal(mean(m$resultado[m$metrica_instanciada == id]), 3 / 4)
})
