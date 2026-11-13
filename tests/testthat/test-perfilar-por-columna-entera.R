# La particion comparte con la columna entera tres decisiones -el centinela
# conjeturado, la numeracion densa y el identificador para Benford- y ninguna
# otra: lo demas lo publica cada grupo con sus filas.
testthat::skip_on_cran()

.firma_por_entera <- function(x, por = "grupo") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if ("columna" %in% names(x)) {
    x <- x[.nombres_para_operar(x$columna) !=
             .nombres_para_operar(por), , drop = FALSE]
  }
  if (!nrow(x)) return(character())
  columnas <- intersect(
    c("columna", "tipo_hallazgo", "severidad", "n_afectados", "evidencia",
      "como_resolverlo", "dependencia", "columnas"),
    names(x)
  )
  filas <- vapply(seq_len(nrow(x)), function(i) {
    paste(vapply(columnas, function(nombre) {
      valor <- x[[nombre]][i]
      paste0(nombre, "=", paste(capture.output(dput(valor)), collapse = ""))
    }, character(1L)), collapse = "|")
  }, character(1L))
  .sort_por_bytes(filas)
}

.firma_cobertura_entera <- function(x, por = "grupo") {
  if (!is.data.frame(x) || !nrow(x)) return(character())
  if ("columna" %in% names(x)) {
    x <- x[.nombres_para_operar(x$columna) !=
             .nombres_para_operar(por), , drop = FALSE]
  }
  if (!nrow(x)) return(character())
  columnas <- intersect(
    c("diagnostico", "columna", "motivo", "como_resolverlo", "columnas"),
    names(x)
  )
  filas <- vapply(seq_len(nrow(x)), function(i) {
    paste(vapply(columnas, function(nombre) {
      valor <- x[[nombre]][i]
      paste0(nombre, "=", paste(capture.output(dput(valor)), collapse = ""))
    }, character(1L)), collapse = "|")
  }, character(1L))
  .sort_por_bytes(filas)
}

.comparar_por_entera <- function(datos, argumentos = list()) {
  entero <- do.call(perfilar, c(list(datos), argumentos))
  por <- do.call(perfilar_por, c(
    list(datos = datos, por = "grupo", min_filas = 1L), argumentos
  ))
  expect_identical(
    .firma_por_entera(entero$hallazgos),
    .firma_por_entera(por)
  )
  expect_identical(
    .firma_cobertura_entera(entero$cobertura_diagnosticos),
    .firma_cobertura_entera(attr(por, "cobertura_diagnosticos", exact = TRUE))
  )
  invisible(list(entero = entero, por = por))
}

.opciones_por_entera <- function() {
  list(
    fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE,
    normalizar = FALSE
  )
}

# Dos poblaciones distintas en una misma tabla: unos importes y unos codigos.
# Cada columna pone a los importes un defecto propio al lado de una decision
# que la columna entera toma por los codigos.
.banco_dos_poblaciones <- function() {
  n_importes <- 101L
  n_codigos <- 1900L
  numeracion <- as.character(1:2001)
  numeracion[1:2] <- "999"
  data.frame(
    grupo = c(rep("importes", n_importes), rep("codigos", n_codigos)),
    # El 1000 es un extremo de los importes; la columna entera es `1:2000`.
    extremo = c(c(1:100, 1000), 101:2000),
    # `SD` es una ausencia en el vocabulario chico de los importes.
    vocabulario = c(
      c(rep(c("A", "B", "C"), length.out = n_importes - 1L), "SD"),
      sprintf("C%04d", seq_len(n_codigos))
    ),
    # Una numeracion escrita como texto: el 999 es un codigo mas.
    numeracion_texto = numeracion,
    # La columna entera conjetura 8888; los importes, solos, 7777.
    centinela_factor = factor(as.character(c(
      rep(40:59, length.out = n_importes - 10L), rep(7777, 10L),
      rep(40:59, length.out = n_codigos - 30L), rep(8888, 30L)
    ))),
    stringsAsFactors = FALSE
  )
}

# Los tipos que salen de las tres decisiones de columna entera, con el
# diagnostico de la cobertura que los declara cuando el grupo no los publica.
.diagnostico_de_columna_entera <- c(
  desviacion_benford = "ley_benford",
  posible_centinela_numerico = "centinelas_numericos",
  faltantes_disfrazados = "centinelas_numericos",
  faltantes = "centinelas_numericos"
)

test_that("un solo grupo conserva hallazgos y cobertura de la columna entera", {
  n <- 240L
  datos <- data.frame(
    grupo = rep("todo", n),
    entero = rep(1:12, length.out = n),
    doble = round(stats::rnorm(n, 30, 4), 3),
    texto_num = as.character(rep(1:20, length.out = n)),
    factor_num = factor(rep(c("1", "2", "10"), length.out = n)),
    fecha = as.Date("2020-01-01") + rep(0:29, length.out = n),
    logica = rep(c(TRUE, FALSE, NA), length.out = n),
    secuencia = seq_len(n),
    importe = 10 ^ stats::runif(n, 0, 5),
    stringsAsFactors = FALSE
  )
  .comparar_por_entera(datos, list(
    fecha = as.POSIXct("2026-01-01", tz = "UTC"), muestra = Inf,
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE,
    normalizar = FALSE
  ))
})

test_that("la conjetura usa la lista de centinelas por omision", {
  n <- 300L
  x <- c(rep(-9999, 40L), rep(8888, 40L), round(stats::rnorm(n - 80L, 100, 5)))
  datos <- data.frame(grupo = rep("todo", n), x = x)
  salida <- .comparar_por_entera(datos, list(
    fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  ))
  entero <- salida$entero
  por <- salida$por
  expect_true(any(entero$hallazgos$tipo_hallazgo ==
                    "posible_centinela_numerico"))
  expect_true(any(por$tipo_hallazgo == "posible_centinela_numerico"))
  expect_false(any(
    attr(por, "cobertura_diagnosticos", exact = TRUE)$diagnostico ==
      "centinelas_numericos"
  ))
})

test_that("double e integer64 deciden igual la secuencia concentrada", {
  skip_if_not_installed("bit64")
  id <- c(1:950, rep(999, 50), 1001:2000)
  opciones <- list(
    fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE
  )
  doble <- .comparar_por_entera(
    data.frame(grupo = rep("todo", length(id)), x = as.numeric(id)), opciones
  )
  entero64 <- .comparar_por_entera(
    data.frame(grupo = rep("todo", length(id)),
               x = bit64::as.integer64(id)), opciones
  )
  expect_identical(
    .firma_por_entera(doble$entero$hallazgos),
    .firma_por_entera(entero64$entero$hallazgos)
  )
})

test_that("Benford usa las tres razones de identificador de la columna entera", {
  set.seed(9)
  x <- c(1:2000, rep(8888, 20L))
  datos <- data.frame(
    grupo = sample(rep(letters[1:10], length.out = length(x))), x = x
  )
  resultado <- perfilar_por(
    datos, "grupo", min_filas = 1L, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ausencia_estructural = FALSE,
    proteger_datos_personales = FALSE
  )
  expect_false(any(resultado$columna == "x" &
                    resultado$tipo_hallazgo == "desviacion_benford"))
  cobertura <- attr(resultado, "cobertura_diagnosticos", exact = TRUE)
  expect_equal(sum(cobertura$diagnostico == "ley_benford" &
                     cobertura$columna == "x"), 10L)
})

test_that("sobre una numeracion el grupo publica sus extremos y Benford sigue a la columna", {
  x <- c(1:2000, rep(9999, 20L))
  datos <- data.frame(
    grupo = rep(c("A", "B", "C"), length.out = length(x)), x = x
  )
  entero <- perfilar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  por <- perfilar_por(
    datos, "grupo", min_filas = 1L,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )
  cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
  # La columna entera calla sus extremos porque tiene forma de numeracion. Eso
  # no se comparte: los veinte 9999 son extremos de cada grupo y cada grupo
  # los publica, sin fila de cobertura que los declare no evaluados.
  expect_false(any(entero$hallazgos$tipo_hallazgo == "outliers" &
                     entero$hallazgos$columna == "x"))
  extremos <- por[por$tipo_hallazgo == "outliers" & por$columna == "x", ,
                  drop = FALSE]
  expect_setequal(extremos$grupo, c("A", "B", "C"))
  expect_equal(sum(extremos$n_afectados), 20)
  expect_false(any(cobertura$diagnostico == "outliers" &
                     cobertura$columna == "x"))
  # El identificador para Benford si es de la columna entera.
  expect_false(any(por$tipo_hallazgo == "desviacion_benford" &
                     por$columna == "x"))
  expect_true(any(cobertura$diagnostico == "ley_benford" &
                    cobertura$columna == "x"))
})

test_that("el identificador para Benford se decide sin los centinelas declarados", {
  # Declarado el 8888, la columna entera es `1:2000`: correlativa, y Benford no
  # le aplica. Un grupo, solo, ya no es correlativo y se desvia de Benford.
  x <- c(1:2000, rep(8888, 20L))
  datos <- data.frame(
    grupo = rep(c("A", "B", "C"), length.out = length(x)), x = x
  )
  opciones <- c(.opciones_por_entera(), list(sentinelas_numericos = 8888))
  entero <- do.call(perfilar, c(list(datos), opciones))
  por <- do.call(perfilar_por, c(
    list(datos, "grupo", min_filas = 1L), opciones
  ))
  expect_false(any(entero$hallazgos$columna == "x" &
                     entero$hallazgos$tipo_hallazgo == "desviacion_benford"))
  expect_true(any(
    entero$cobertura_diagnosticos$columna == "x" &
      entero$cobertura_diagnosticos$diagnostico == "ley_benford" &
      grepl("identificador", entero$cobertura_diagnosticos$motivo,
            fixed = TRUE, useBytes = TRUE)
  ))
  se_desvian <- vapply(c("A", "B", "C"), function(grupo) {
    solo <- do.call(perfilar, c(
      list(datos[datos$grupo == grupo, "x", drop = FALSE]), opciones
    ))
    any(solo$hallazgos$tipo_hallazgo == "desviacion_benford")
  }, logical(1L))
  expect_true(any(se_desvian))
  expect_false(any(por$columna == "x" &
                     por$tipo_hallazgo == "desviacion_benford"))
  cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
  for (grupo in names(se_desvian)[se_desvian]) {
    expect_true(any(
      cobertura$grupo == grupo & cobertura$columna == "x" &
        cobertura$diagnostico == "ley_benford" &
        grepl("identificador", cobertura$motivo, fixed = TRUE,
              useBytes = TRUE)
    ), info = grupo)
  }
})

test_that("la conjetura de centinela de texto y factor es de la columna entera", {
  base <- as.character(1:2000)
  grupos <- rep(c("A", "B", "C"), length.out = length(base))
  base[which(grupos == "A")[1:10]] <- "7777"
  base[which(grupos == "B")[1:10]] <- "8888"
  for (x in list(
    base,
    factor(base, levels = unique(base))
  )) {
    datos <- data.frame(grupo = grupos, x = x)
    entero <- perfilar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
    por <- perfilar_por(
      datos, "grupo", min_filas = 1L,
      fecha = as.POSIXct("2026-01-01", tz = "UTC")
    )
    expect_true(any(entero$hallazgos$columna == "x" &
                      entero$hallazgos$tipo_hallazgo ==
                        "posible_centinela_numerico"))
    expect_false(any(por$grupo == "A" & por$columna == "x" &
                       por$tipo_hallazgo == "posible_centinela_numerico" &
                       grepl("7777", por$evidencia, fixed = TRUE,
                             useBytes = TRUE)))
    cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
    expect_true(any(cobertura$grupo == "A" & cobertura$columna == "x" &
                      cobertura$diagnostico == "centinelas_numericos"))
  }
})

test_that("la numeracion densa de texto y factor no inventa faltantes por grupo", {
  base <- as.character(1:2000)
  grupos <- rep(c("A", "B", "C"), length.out = length(base))
  base[which(grupos == "A")[1:2]] <- "999"
  base[which(grupos == "B")[1]] <- "999"
  for (x in list(
    base,
    factor(base, levels = unique(base))
  )) {
    datos <- data.frame(grupo = grupos, x = x)
    entero <- perfilar(datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
    por <- perfilar_por(
      datos, "grupo", min_filas = 1L,
      fecha = as.POSIXct("2026-01-01", tz = "UTC")
    )
    expect_false(any(entero$hallazgos$columna == "x" &
                       entero$hallazgos$tipo_hallazgo ==
                         "faltantes_disfrazados"))
    expect_false(any(por$columna == "x" &
                       por$tipo_hallazgo == "faltantes_disfrazados"))
    cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
    expect_true(any(cobertura$grupo == "A" & cobertura$columna == "x" &
                      cobertura$diagnostico == "centinelas_numericos"))
  }
})

test_that("una cadena de ausencia declarada no es de la numeracion", {
  # El mismo banco, pero el usuario declara "999" como cadena de ausencia: ya
  # no es un candidato numerico que la numeracion protege, es una ausencia
  # escrita con texto. La columna entera la publica, y cada grupo tambien.
  base <- as.character(1:2000)
  grupos <- rep(c("A", "B", "C"), length.out = length(base))
  base[which(grupos == "A")[1:2]] <- "999"
  base[which(grupos == "B")[1]] <- "999"
  opciones <- c(.opciones_por_entera(), list(cadenas_ausencia = "999"))
  for (x in list(
    base,
    factor(base, levels = unique(base))
  )) {
    datos <- data.frame(grupo = grupos, x = x)
    entero <- do.call(perfilar, c(list(datos), opciones))
    por <- do.call(perfilar_por, c(
      list(datos, "grupo", min_filas = 1L), opciones
    ))
    de_la_columna <- entero$hallazgos[
      entero$hallazgos$columna == "x" &
        entero$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
      drop = FALSE
    ]
    expect_equal(de_la_columna$n_afectados, 4)
    de_los_grupos <- por[por$columna == "x" &
                           por$tipo_hallazgo == "faltantes_disfrazados", ,
                         drop = FALSE]
    expect_identical(
      de_los_grupos$n_afectados[match(c("A", "B", "C"), de_los_grupos$grupo)],
      c(2, 1, 1)
    )
    cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
    expect_false(any(cobertura$columna == "x" &
                       cobertura$diagnostico == "centinelas_numericos"))
    # Y con un solo grupo, lo mismo que la tabla entera.
    datos$grupo <- "todo"
    .comparar_por_entera(datos, opciones)
  }
})

test_that("integer64 no agrega un centinela que double no conjetura", {
  skip_if_not_installed("bit64")
  set.seed(17)
  normales <- round(stats::rnorm(940L, 10, 1) * 1000)
  x <- c(rep(-9999, 60L), normales)
  doble <- perfilar(data.frame(x = x), analizar_dependencias = FALSE)
  entero64 <- perfilar(
    data.frame(x = bit64::as.integer64(x)), analizar_dependencias = FALSE
  )
  expect_identical(
    .firma_por_entera(doble$hallazgos, por = "__ninguna__"),
    .firma_por_entera(entero64$hallazgos, por = "__ninguna__")
  )
  expect_false(any(entero64$hallazgos$tipo_hallazgo ==
                     "posible_centinela_numerico"))
})

test_that("integer64 y double publican lo mismo de un centinela de la lista", {
  skip_if_not_installed("bit64")
  # Un valor de la lista por omision ya sale como `faltantes_disfrazados`:
  # conjeturarlo ademas como centinela era propio de `integer64`, y con el
  # hallazgo se llenaban tres campos de la tabla de columnas que en `double`
  # quedan vacios.
  set.seed(3)
  for (centinela in c(-9999, -999, 999)) {
    base <- if (centinela == 999) {
      sample(9900:10100, 170L, replace = TRUE)
    } else {
      sample(1:300, 170L, replace = TRUE)
    }
    x <- c(rep(centinela, 30L), base)
    doble <- perfilar(data.frame(x = as.numeric(x)),
                      analizar_dependencias = FALSE)
    entero64 <- perfilar(data.frame(x = bit64::as.integer64(x)),
                         analizar_dependencias = FALSE)
    expect_identical(
      .firma_por_entera(doble$hallazgos, por = "__ninguna__"),
      .firma_por_entera(entero64$hallazgos, por = "__ninguna__")
    )
    expect_true(any(entero64$hallazgos$tipo_hallazgo ==
                      "faltantes_disfrazados"))
    for (campo in c("centinela_valor", "centinela_repeticiones",
                    "densidad_sin_centinela")) {
      expect_true(is.na(doble$columnas[[campo]]), info = campo)
      expect_true(is.na(entero64$columnas[[campo]]), info = campo)
    }
  }
})

test_that("los outliers propios del grupo no se trasladan a cobertura", {
  datos <- data.frame(
    grupo = c(rep("importes", 101L), rep("codigos", 1900L)),
    x = c(c(1:100, 1000), 101:2000),
    stringsAsFactors = FALSE
  )
  resultado <- perfilar_por(
    datos, "grupo", min_filas = 1L,
    fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE,
    normalizar = FALSE
  )
  grupo <- resultado[resultado$grupo == "importes" &
                       resultado$columna == "x" &
                       resultado$tipo_hallazgo == "outliers", , drop = FALSE]
  expect_equal(nrow(grupo), 1L)
  expect_equal(grupo$n_afectados[[1L]], 1)
  cobertura <- attr(resultado, "cobertura_diagnosticos", exact = TRUE)
  expect_false(any(cobertura$grupo == "importes" &
                     cobertura$columna == "x" &
                     cobertura$diagnostico == "outliers"))
})

test_that("las ausencias textuales siguen siendo propias del grupo", {
  datos <- data.frame(
    grupo = c(rep("grande", 400L), rep("chico", 20L)),
    x = c(sprintf("C%04d", 1:400),
          c(rep(c("A", "B", "C"), length.out = 19L), "SD")),
    stringsAsFactors = FALSE
  )
  resultado <- perfilar_por(
    datos, "grupo", min_filas = 1L,
    fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE,
    ausencia_estructural = FALSE, proteger_datos_personales = FALSE,
    normalizar = FALSE
  )
  grupo <- resultado[resultado$grupo == "chico" &
                       resultado$columna == "x" &
                       resultado$tipo_hallazgo == "faltantes_disfrazados", ,
                     drop = FALSE]
  expect_equal(nrow(grupo), 1L)
  expect_equal(grupo$n_afectados[[1L]], 1)
  expect_match(grupo$evidencia[[1L]], "SD", fixed = TRUE)
  cobertura <- attr(resultado, "cobertura_diagnosticos", exact = TRUE)
  expect_false(any(cobertura$grupo == "chico" &
                     cobertura$columna == "x" &
                     cobertura$diagnostico == "centinelas_numericos"))
})

test_that("cada grupo publica lo propio y solo cede las tres decisiones de columna entera", {
  datos <- .banco_dos_poblaciones()
  opciones <- .opciones_por_entera()
  por <- do.call(perfilar_por, c(
    list(datos, "grupo", min_filas = 1L), opciones
  ))
  cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
  publicados <- as.data.frame(por, stringsAsFactors = FALSE)
  de_columna_entera <- function(tabla) {
    tabla$tipo_hallazgo %in% names(.diagnostico_de_columna_entera)
  }
  cedidos <- character()
  for (grupo in c("importes", "codigos")) {
    # La referencia de lo que el grupo tiene es perfilarlo solo.
    solo <- do.call(perfilar, c(
      list(datos[datos$grupo == grupo, names(datos) != "grupo",
                 drop = FALSE]),
      opciones
    ))$hallazgos
    del_grupo <- publicados[publicados$grupo == grupo, , drop = FALSE]
    # Lo propio sale igual: mismo tipo, columna, afectados y evidencia.
    expect_identical(
      .firma_por_entera(del_grupo[!de_columna_entera(del_grupo), ,
                                  drop = FALSE], por = "__ninguna__"),
      .firma_por_entera(solo[!de_columna_entera(solo), , drop = FALSE],
                        por = "__ninguna__")
    )
    # Lo que depende de la columna entera se publica igual o se declara en la
    # cobertura con su diagnostico: ni se inventa ni se calla.
    firmas_del_grupo <- .firma_por_entera(del_grupo, por = "__ninguna__")
    for (i in which(de_columna_entera(solo))) {
      fila <- solo[i, , drop = FALSE]
      tipo <- as.character(fila$tipo_hallazgo)
      publicado <- .firma_por_entera(fila, por = "__ninguna__") %in%
        firmas_del_grupo
      declarado <- any(
        cobertura$grupo == grupo &
          cobertura$columna == as.character(fila$columna) &
          cobertura$diagnostico == .diagnostico_de_columna_entera[[tipo]]
      )
      expect_true(publicado || declarado,
                  info = paste(grupo, fila$columna, tipo))
      expect_false(publicado && declarado,
                   info = paste(grupo, fila$columna, tipo))
      if (!publicado) {
        cedidos <- c(cedidos, paste(grupo, fila$columna, tipo))
      }
    }
    expect_true(all(
      .firma_por_entera(del_grupo[de_columna_entera(del_grupo), ,
                                  drop = FALSE], por = "__ninguna__") %in%
        .firma_por_entera(solo, por = "__ninguna__")
    ))
  }
  # El banco ejerce las tres decisiones: sin esto la prueba pasaria tambien si
  # ningun grupo cediera nada.
  expect_setequal(cedidos, c(
    "importes extremo desviacion_benford",
    "importes numeracion_texto faltantes_disfrazados",
    "importes centinela_factor posible_centinela_numerico"
  ))
  # Y los dos defectos propios de los importes siguen a la vista.
  propios <- publicados[publicados$grupo == "importes", , drop = FALSE]
  expect_true(any(propios$columna == "extremo" &
                    propios$tipo_hallazgo == "outliers" &
                    propios$n_afectados == 1))
  expect_true(any(propios$columna == "vocabulario" &
                    propios$tipo_hallazgo == "faltantes_disfrazados" &
                    propios$n_afectados == 1))
})

test_that("los extremos de un grupo se publican aunque Benford siga a la columna", {
  # Unos importes con el primer digito siempre 1 dentro de una tabla dominada
  # por codigos correlativos: los extremos son de los importes; la exclusion de
  # Benford por identificador es de la columna.
  candidatos <- c(1, 10:19, 100:199, 1000:1999, 10000:19999)
  importes <- candidatos[seq(1L, length(candidatos), length.out = 200L)]
  datos <- data.frame(
    grupo = c(rep("codigos", 20000L), rep("importes", 200L),
              rep("codigos", 30L)),
    x = c(1:20000, importes, rep(99999, 30L)),
    stringsAsFactors = FALSE
  )
  opciones <- .opciones_por_entera()
  por <- do.call(perfilar_por, c(
    list(datos, "grupo", min_filas = 1L), opciones
  ))
  solo <- do.call(perfilar, c(
    list(datos[datos$grupo == "importes", "x", drop = FALSE]), opciones
  ))$hallazgos
  extremos_solo <- solo[solo$tipo_hallazgo == "outliers", , drop = FALSE]
  extremos <- por[por$grupo == "importes" & por$columna == "x" &
                    por$tipo_hallazgo == "outliers", , drop = FALSE]
  expect_equal(nrow(extremos_solo), 1L)
  expect_identical(extremos$n_afectados, extremos_solo$n_afectados)
  expect_identical(extremos$evidencia, extremos_solo$evidencia)
  cobertura <- attr(por, "cobertura_diagnosticos", exact = TRUE)
  expect_false(any(cobertura$grupo == "importes" & cobertura$columna == "x" &
                     cobertura$diagnostico == "outliers"))
  # El grupo solo tiene una desviacion de Benford; la columna entera esta
  # excluida por identificador, y eso manda.
  expect_true(any(solo$tipo_hallazgo == "desviacion_benford"))
  expect_false(any(por$grupo == "importes" & por$columna == "x" &
                     por$tipo_hallazgo == "desviacion_benford"))
  expect_true(any(cobertura$grupo == "importes" & cobertura$columna == "x" &
                    cobertura$diagnostico == "ley_benford"))
})

# Una columna `integer64` con valores por encima de 2^53 no deja evaluar la
# numeracion (en un doble se redondean), y eso no es lo mismo que no tener
# `bit64`: `perfilar()` no cierra la guarda y cuenta los faltantes disfrazados.
# Un grupo de valores chicos con dos `-9999` los tiene que seguir publicando
# aunque otro grupo de la misma columna traiga identificadores de 64 bits.
test_that("valores de mas de 2^53 en otro grupo no callan el faltante disfrazado de este", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  datos <- data.frame(
    grupo = rep(c("g1", "g2"), each = 40L),
    id = c(
      bit64::as.integer64("9007199254740999") + seq_len(40L),
      bit64::as.integer64(c(rep(-9999, 2L), 100 + seq_len(38L)))
    ),
    stringsAsFactors = FALSE
  )
  por_grupo <- perfilar_por(datos, "grupo", min_filas = 10L,
                            analizar_dependencias = FALSE,
                            proteger_datos_personales = FALSE)
  tabla <- as.data.frame(por_grupo)
  disfrazados <- tabla[
    tabla$grupo == "g2" & tabla$columna %in% "id" &
      tabla$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_identical(nrow(disfrazados), 1L)
  expect_equal(disfrazados$n_afectados, 2)
  cobertura <- as.data.frame(attr(por_grupo, "cobertura_diagnosticos"))
  expect_false(any(
    cobertura$grupo == "g2" & cobertura$columna %in% "id" &
      cobertura$diagnostico == "centinelas_numericos"
  ))
})
