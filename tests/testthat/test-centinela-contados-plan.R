# `convertir_sentinelas_numericos` convierte exactamente las celdas que el perfil
# conto en `n_faltantes_disfrazados_numericos`. El plan armaba su lista con el
# catalogo entero de centinelas -o con lo que lograba leer de la evidencia, que
# muestra seis marcas- y `aplicar()` convertia de mas: el `999` legitimo de una
# numeracion, las celdas que son marca textual, las de fuera del universo
# aplicable. Ahora la lista sale de la traza del hallazgo
# (`sentinelas_contados`) y, donde la traza no la trae o la proteccion la tapo,
# `aplicar()` la acota con el mismo detector sobre los datos que recibe.

# Varias pruebas perfilan tablas de dos mil filas: corre en la suite completa y
# en la integracion continua, no en el tiempo de CRAN.
testthat::skip_on_cran()

.ccp_perfil <- function(datos, ...) {
  suppressWarnings(perfilar(
    datos, fecha = "2026-01-15", analizar_dependencias = FALSE, ...
  ))
}

.ccp_contados <- function(perfil, columna = "x") {
  fila <- perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
  as.numeric(fila$n_faltantes_disfrazados_numericos)
}

.ccp_traza <- function(perfil, columna = "x") {
  i <- which(
    perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados" &
      perfil$hallazgos$columna %in% columna
  )
  perfil$hallazgos$trazabilidad[[i[[1L]]]]
}

# Un perfil guardado por la version anterior: la traza no trae el campo.
.ccp_sin_campo <- function(perfil) {
  filas <- which(perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados")
  for (i in filas) {
    traza <- perfil$hallazgos$trazabilidad[[i]]
    traza$sentinelas_contados <- NULL
    perfil$hallazgos$trazabilidad[[i]] <- traza
  }
  perfil
}

.ccp_plan <- function(perfil, datos, con_datos = TRUE) {
  if (con_datos) planificar_limpieza(perfil, datos) else planificar_limpieza(perfil)
}

.ccp_fila <- function(plan, columna = "x") {
  which(
    plan$estrategia == "convertir_sentinelas_numericos" &
      plan$columna %in% columna
  )
}

# Aplica SOLO la accion de centinelas de `columna` y devuelve lo que hizo.
.ccp_aplicar <- function(plan, datos, columna = "x") {
  k <- .ccp_fila(plan, columna)
  plan$aplicar <- seq_len(nrow(plan)) == k
  aplicado <- suppressWarnings(aplicar(plan, datos))
  registro <- aplicado$registro[
    aplicado$registro$estrategia == "convertir_sentinelas_numericos", ,
    drop = FALSE
  ]
  list(
    datos = aplicado$datos, estado = as.character(registro$estado),
    n = as.numeric(registro$n_cambiadas), error = as.character(registro$error),
    convertidas = which(!is.na(datos[[columna]]) &
                          is.na(aplicado$datos[[columna]])),
    registro = aplicado$registro
  )
}

.ccp_marcas <- c("S/D", "N/A", "NULL", "-", "sin dato", "NA", "ninguno")

.ccp_texto_con_marcas <- function() {
  c(as.character(1:2000), rep(.ccp_marcas, each = 12), rep("-9", 9))
}

test_that("con mas marcas que las que muestra la evidencia la lista es la contada", {
  texto <- .ccp_texto_con_marcas()
  formas <- list(
    texto = data.frame(x = texto, stringsAsFactors = FALSE),
    factor = data.frame(x = factor(texto))
  )
  for (datos in formas) {
    perfil <- .ccp_perfil(datos)
    expect_identical(.ccp_contados(perfil), 9)
    expect_identical(.ccp_traza(perfil)$sentinelas_contados, -9)
    for (con_datos in c(TRUE, FALSE)) {
      plan <- .ccp_plan(perfil, datos, con_datos)
      k <- .ccp_fila(plan)
      expect_identical(as.numeric(plan$parametros[[k]]$valores), -9)
      hecho <- .ccp_aplicar(plan, datos)
      expect_identical(hecho$estado, "ejecutada")
      expect_identical(hecho$n, 9)
      expect_identical(hecho$convertidas, which(as.character(datos$x) == "-9"))
      expect_true("999" %in% as.character(hecho$datos$x))
    }
  }
})

test_that("la lista contada se lee con la coma decimal de la columna", {
  texto <- c(
    sub(".", ",", sprintf("%.2f", seq(1000.5, 4000.5, by = 3)), fixed = TRUE),
    rep(.ccp_marcas, each = 12), rep("-9,00", 9)
  )
  datos <- data.frame(x = texto, stringsAsFactors = FALSE)
  perfil <- .ccp_perfil(datos)
  expect_identical(.ccp_contados(perfil), 9)
  plan <- .ccp_plan(perfil, datos)
  expect_identical(as.numeric(plan$parametros[[.ccp_fila(plan)]]$valores), -9)
  hecho <- .ccp_aplicar(plan, datos)
  expect_identical(hecho$n, 9)
  expect_identical(hecho$convertidas, which(datos$x == "-9,00"))
})

test_that("con la proteccion encendida una columna no personal conserva su 999", {
  documentos <- sprintf("%d.%03d.%03d-%d", rep(1:4, 100), 100:499, 500:899, rep(0:9, 40))
  monto <- c(seq(100, 4969, by = 13)[1:388], rep(-99, 12))
  monto[[5L]] <- 999
  datos <- data.frame(doc = documentos, x = monto, stringsAsFactors = FALSE)
  perfil <- .ccp_perfil(
    datos, proteger_datos_personales = TRUE, columnas_personales = "doc"
  )
  expect_identical(.ccp_contados(perfil), 12)
  for (con_datos in c(TRUE, FALSE)) {
    plan <- .ccp_plan(perfil, datos, con_datos)
    expect_identical(as.numeric(plan$parametros[[.ccp_fila(plan)]]$valores), -99)
    hecho <- .ccp_aplicar(plan, datos)
    expect_identical(hecho$n, 12)
    expect_true(999 %in% hecho$datos$x)
  }
})

test_that("una columna protegida no publica que centinela se conto y convierte lo contado", {
  datos <- data.frame(id_persona = c(1:2000, rep(-9, 10)))
  perfil <- .ccp_perfil(
    datos, proteger_datos_personales = TRUE, columnas_personales = "id_persona"
  )
  expect_identical(.ccp_contados(perfil, "id_persona"), 10)
  # El perfil tapa la evidencia: la traza tampoco dice que se conto.
  expect_identical(
    .ccp_traza(perfil, "id_persona")$sentinelas_contados, NA_real_
  )
  for (con_datos in c(TRUE, FALSE)) {
    plan <- .ccp_plan(perfil, datos, con_datos)
    k <- .ccp_fila(plan, "id_persona")
    # La lista sigue siendo la del perfil entera: no senala al contado.
    expect_setequal(
      as.numeric(plan$parametros[[k]]$valores), c(-9999, -999, -99, -9, 999)
    )
    expect_identical(as.numeric(plan$n_afectadas[[k]]), 10)
    hecho <- .ccp_aplicar(plan, datos, "id_persona")
    expect_identical(hecho$estado, "ejecutada")
    expect_identical(hecho$n, 10)
    expect_identical(hecho$convertidas, 2001:2010)
    expect_true(999 %in% hecho$datos$id_persona)
  }
})

test_that("un perfil sin el campo en la traza convierte lo contado", {
  datos <- data.frame(x = .ccp_texto_con_marcas(), stringsAsFactors = FALSE)
  perfil <- .ccp_sin_campo(.ccp_perfil(datos))
  for (con_datos in c(TRUE, FALSE)) {
    plan <- .ccp_plan(perfil, datos, con_datos)
    hecho <- .ccp_aplicar(plan, datos)
    expect_identical(hecho$estado, "ejecutada")
    expect_identical(hecho$n, 9)
    expect_identical(hecho$convertidas, which(datos$x == "-9"))
    expect_true("999" %in% hecho$datos$x)
  }
})

test_that("la lista editada manda: lo agregado se convierte y lo quitado se conserva", {
  datos <- data.frame(x = .ccp_texto_con_marcas(), stringsAsFactors = FALSE)
  perfil <- .ccp_perfil(datos)
  # Con el campo en la traza y sin el: en el segundo caso la accion acota la
  # lista del perfil sobre los datos, y lo que el usuario agrego no se acota.
  for (p in list(perfil, .ccp_sin_campo(perfil))) {
    plan <- .ccp_plan(p, datos)
    k <- .ccp_fila(plan)
    plan$parametros[[k]]$valores <- c(
      setdiff(plan$parametros[[k]]$valores, -9), 1234
    )
    hecho <- .ccp_aplicar(plan, datos)
    expect_identical(hecho$convertidas, which(datos$x == "1234"))
    expect_identical(sum(hecho$datos$x %in% "-9"), 9L)
    expect_true("999" %in% hecho$datos$x)
  }
})

test_that("sobre otra entrega se convierte lo que el plan lista y nada mas", {
  origen <- data.frame(
    x = c(as.character(1:2000), rep(.ccp_marcas, each = 12), rep("-9", 10)),
    stringsAsFactors = FALSE
  )
  otra <- data.frame(
    x = c(as.character(1:900), rep(.ccp_marcas, each = 3), rep("999", 40),
          rep("-9", 3)),
    stringsAsFactors = FALSE
  )
  plan <- .ccp_plan(.ccp_perfil(origen), origen)
  hecho <- .ccp_aplicar(plan, otra)
  expect_identical(hecho$convertidas, which(otra$x == "-9"))
  expect_identical(sum(hecho$datos$x %in% "999"), 40L)
  # El codigo de la otra entrega se convierte si se lo agrega a la lista.
  k <- .ccp_fila(plan)
  plan$parametros[[k]]$valores <- c(plan$parametros[[k]]$valores, 999)
  ampliado <- .ccp_aplicar(plan, otra)
  expect_identical(ampliado$n, 43)
})

test_that("una celda que es marca textual declarada no es de la accion numerica", {
  datos <- data.frame(
    x = c(as.character(1:2000), rep("999", 6), rep("999.0", 5)),
    stringsAsFactors = FALSE
  )
  perfil <- .ccp_perfil(datos, cadenas_ausencia = "999")
  expect_identical(.ccp_contados(perfil), 5)
  for (p in list(perfil, .ccp_sin_campo(perfil))) {
    hecho <- .ccp_aplicar(.ccp_plan(p, datos), datos)
    expect_identical(hecho$n, 5)
    expect_identical(hecho$convertidas, which(datos$x == "999.0"))
    # Las siete celdas `"999"` -las seis agregadas y la de la numeracion- son
    # de `convertir_ausencias_textuales`.
    expect_identical(sum(hecho$datos$x %in% "999"), 7L)
  }
  # El mismo numero declarado como marca y como centinela, escrito de dos formas.
  datos <- data.frame(x = c(1:100, "99", "099"), stringsAsFactors = FALSE)
  perfil <- .ccp_perfil(datos, cadenas_ausencia = "99", sentinelas_numericos = 99)
  expect_identical(.ccp_contados(perfil), 1)
  hecho <- .ccp_aplicar(.ccp_plan(perfil, datos), datos)
  expect_identical(hecho$convertidas, 102L)
})

test_that("la accion que acota sobre los datos respeta el universo aplicable", {
  # Dentro del universo, cinco -9; fuera, diez.
  chica <- data.frame(
    g = rep(c("si", "no"), c(20, 20)),
    x = c(1:15, rep(-9, 5), 1:10, rep(-9, 10))
  )
  # Dentro, una numeracion con diez -9; fuera, cincuenta 999, que sin el
  # universo vuelven al 999 una moda que sobresale.
  densa <- data.frame(
    g = c(rep("si", 1010), rep("no", 50)),
    x = c(1:1000, rep(-9, 10), rep(999, 50))
  )
  for (datos in list(chica, densa)) {
    perfil <- .ccp_perfil(
      datos, aplicabilidad = list(x = ~ g == "si"),
      proteger_datos_personales = TRUE, columnas_personales = "x"
    )
    esperadas <- which(datos$g == "si" & datos$x == -9)
    expect_identical(.ccp_contados(perfil), as.numeric(length(esperadas)))
    plan <- .ccp_plan(perfil, datos)
    hecho <- .ccp_aplicar(plan, datos)
    expect_identical(hecho$estado, "ejecutada")
    expect_identical(hecho$convertidas, esperadas)
    # Sin la regla la accion no puede acotarse a lo que el perfil conto: no
    # convierte la lista entera, falla y lo dice.
    k <- .ccp_fila(plan)
    plan$parametros[[k]]$aplicabilidad <- NULL
    sin_regla <- .ccp_aplicar(plan, datos)
    expect_identical(sin_regla$estado, "fallida")
    expect_identical(sin_regla$convertidas, integer())
    expect_match(sin_regla$error, "aplicabilidad", fixed = TRUE)
  }
})

test_that("una columna opcional cuenta lo mismo y no necesita regla para acotar", {
  datos <- data.frame(id = 1:1012, x = c(1:1000, rep(-9, 10), NA, NA))
  expect_identical(
    .ccp_contados(.ccp_perfil(datos, columnas_opcionales = "x")),
    .ccp_contados(.ccp_perfil(datos))
  )
  perfil <- .ccp_perfil(
    datos, columnas_opcionales = "x",
    proteger_datos_personales = TRUE, columnas_personales = "x"
  )
  expect_identical(.ccp_contados(perfil), 10)
  hecho <- .ccp_aplicar(.ccp_plan(perfil, datos), datos)
  expect_identical(hecho$estado, "ejecutada")
  expect_identical(hecho$convertidas, 1001:1010)
  expect_true(999 %in% hecho$datos$x)
})

test_that("la accion que acota sobre los datos decide con la entrega que recibe", {
  origen <- data.frame(x = c(1:2000, rep(-9, 10)))
  perfil <- .ccp_perfil(
    origen, proteger_datos_personales = TRUE, columnas_personales = "x"
  )
  plan <- .ccp_plan(perfil, origen)
  # En la otra entrega el -99 cae fuera de la numeracion y el 999 esta adentro,
  # una vez: se convierte el primero, que el perfil de origen no habia contado,
  # y no el segundo. Nunca un valor que la lista no tenga.
  otra <- data.frame(x = c(1:1500, rep(-99, 4), rep(7777, 3)))
  hecho <- .ccp_aplicar(plan, otra)
  expect_identical(hecho$convertidas, 1501:1504)
  expect_true(all(c(999, 7777) %in% hecho$datos$x))
})

test_that("la misma numeracion cuenta lo mismo como entero, duracion o medida", {
  series <- list(
    1:1000, c(1:1000, rep(-9, 10)), c(501:1500, 999, 999),
    c(1:1000, rep(999, 60))
  )
  for (serie in series) {
    entero <- .ccp_contados(.ccp_perfil(data.frame(x = as.integer(serie))))
    duracion <- .ccp_contados(.ccp_perfil(
      data.frame(x = as.difftime(as.numeric(serie), units = "mins"))
    ))
    expect_identical(duracion, entero)
    if (requireNamespace("units", quietly = TRUE)) {
      medida <- .ccp_contados(.ccp_perfil(
        data.frame(x = units::set_units(as.numeric(serie), "m"))
      ))
      expect_identical(medida, entero)
    }
  }
})

test_that("la accion convierte los centinelas de una duracion y conserva su unidad", {
  minutos <- c(round(seq(5, 500, length.out = 200)), rep(-9999, 6))
  datos <- data.frame(x = as.difftime(minutos, units = "mins"))
  perfil <- .ccp_perfil(datos, sentinelas_numericos = -9999)
  expect_identical(.ccp_contados(perfil), 6)
  hecho <- .ccp_aplicar(.ccp_plan(perfil, datos), datos)
  expect_identical(hecho$estado, "ejecutada")
  expect_identical(hecho$convertidas, 201:206)
  expect_s3_class(hecho$datos$x, "difftime")
  expect_identical(units(hecho$datos$x), "mins")
  expect_identical(as.numeric(hecho$datos$x)[1:200], minutos[1:200])
})

test_that("la accion sobre una medida con unidad no toca el valor de la numeracion", {
  skip_if_not_installed("units")
  datos <- data.frame(x = units::set_units(as.numeric(c(1:1000, rep(-9, 10))), "m"))
  perfil <- .ccp_perfil(datos)
  expect_identical(.ccp_contados(perfil), 10)
  hecho <- .ccp_aplicar(.ccp_plan(perfil, datos), datos)
  expect_identical(hecho$convertidas, 1001:1010)
  expect_s3_class(hecho$datos$x, "units")
  expect_identical(as.character(units(hecho$datos$x)), "m")
  expect_true(999 %in% as.numeric(hecho$datos$x))
})

test_that("la traza no dice que centinela se conto donde la evidencia esta tapada", {
  # Columna personal que NO se protege (poder discriminante debil): su
  # evidencia se tapa por valor.
  datos <- data.frame(
    x = as.numeric(c(rep(10000000 + seq_len(100), each = 19), rep(123456789, 10)))
  )
  perfil <- .ccp_perfil(
    datos, proteger_datos_personales = TRUE, sentinelas_numericos = 123456789
  )
  skip_if(
    !identical(as.logical(perfil$datos_personales$proteger), FALSE),
    "la columna no quedo clasificada como personal sin proteger en este armado"
  )
  i <- which(perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados")
  expect_match(perfil$hallazgos$evidencia[[i]], "[valor protegido]", fixed = TRUE)
  expect_identical(.ccp_traza(perfil)$sentinelas_contados, NA_real_)
  # La tabla de hallazgos, entera y con su traza, no dice el valor. `meta` si lo
  # dice -es la declaracion del usuario, y la columna no se protege-, y por eso
  # no se mira el perfil entero.
  contiene <- function(objeto) {
    any(grepl("123456789", utils::capture.output(dput(objeto)), fixed = TRUE))
  }
  expect_false(contiene(perfil$hallazgos))
  expect_false(contiene(as.data.frame(hallazgos(perfil))))
  # La accion va por los datos y convierte lo contado.
  hecho <- .ccp_aplicar(.ccp_plan(perfil, datos), datos)
  expect_identical(hecho$n, 10)
})

test_that("el centinela de una columna protegida no sale por el perfil, el plan ni el analisis", {
  documentos <- as.numeric(sprintf(
    "%d%03d%03d%d", rep(1:4, 100), 100:499, 500:899, rep(0:9, 40)
  ))
  datos <- data.frame(x = c(documentos, rep(99999999, 12)))
  contiene <- function(objeto) {
    any(grepl(
      "99999999", c(format(objeto), utils::capture.output(dput(objeto))),
      fixed = TRUE
    ))
  }
  perfil <- .ccp_perfil(
    datos, proteger_datos_personales = TRUE, columnas_personales = "x",
    sentinelas_numericos = 99999999
  )
  expect_identical(.ccp_contados(perfil), 12)
  expect_identical(.ccp_traza(perfil)$sentinelas_contados, NA_real_)
  expect_false(contiene(unclass(perfil)))
  expect_false(contiene(as.data.frame(hallazgos(perfil))))
  plan <- .ccp_plan(perfil, datos)
  expect_false(contiene(plan))
  hecho <- .ccp_aplicar(plan, datos)
  expect_identical(hecho$n, 12)
  expect_false(contiene(hecho$registro))
  # Un analisis hecho sin proteccion lleva el campo; guardado con proteccion, no.
  analisis <- suppressWarnings(analizar(
    datos, argumentos_perfil = list(
      sentinelas_numericos = 99999999, columnas_personales = "x"
    )
  ))
  expect_identical(.ccp_traza(analisis$perfil)$sentinelas_contados, 99999999)
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  suppressMessages(guardar_analisis(
    analisis, archivo, proteger_datos_personales = TRUE
  ))
  leido <- leer_analisis(archivo)
  expect_identical(.ccp_traza(leido$perfil)$sentinelas_contados, NA_real_)
  expect_false(contiene(unclass(leido)))
})

test_that("con proteccion el campo nunca trae un numero que el perfil no publique", {
  documentos <- sprintf("%d.%03d.%03d-%d", rep(1:4, 100), 100:499, 500:899, rep(0:9, 40))
  datos <- data.frame(
    doc = documentos,
    monto = c(seq(150, 4000, length.out = 380), rep(-999, 12), rep(99999999, 8)),
    edad = c(rep(18:80, length.out = 390), rep(-9, 10)),
    stringsAsFactors = FALSE
  )
  perfil <- .ccp_perfil(
    datos, proteger_datos_personales = TRUE, columnas_personales = "doc",
    sentinelas_numericos = c(-999, -9, 99999999)
  )
  publicados <- perfil$meta$sentinelas_numericos
  filas <- which(perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados")
  expect_true(length(filas) > 0L)
  for (i in filas) {
    campo <- perfil$hallazgos$trazabilidad[[i]]$sentinelas_contados
    expect_true(is.numeric(campo))
    expect_true(all(is.na(campo) | campo %in% publicados))
    tapada <- grepl("protegid", perfil$hallazgos$evidencia[[i]], fixed = TRUE)
    if (tapada) expect_identical(campo, NA_real_)
  }
})

test_that("el campo vive en la traza y no cambia la forma del perfil", {
  datos <- data.frame(x = c(1:500, rep(-9, 4)), y = c(letters[1:24], rep("S/D", 480)))
  perfil <- .ccp_perfil(datos)
  expect_identical(.ccp_traza(perfil, "x")$sentinelas_contados, -9)
  # Un hallazgo que solo cuenta marcas textuales lleva el campo vacio.
  expect_identical(.ccp_traza(perfil, "y")$sentinelas_contados, numeric())
  expect_false("sentinelas_contados" %in% names(perfil))
  expect_false("sentinelas_contados" %in% names(perfil$columnas))
  expect_false("sentinelas_contados" %in% names(perfil$hallazgos))
  expect_false("sentinelas_contados" %in% names(perfil$meta))
  # Un perfil de la version anterior contra uno nuevo de la misma tabla.
  comparacion <- comparar_perfiles(.ccp_sin_campo(perfil), perfil)
  expect_identical(nrow(comparacion), 0L)
})

test_that("la traza que retiene una muestra de orden inestable conserva lo contado", {
  datos <- data.frame(x = .ccp_texto_con_marcas(), stringsAsFactors = FALSE)
  perfil <- .cobertura_orden_inestable_spool_dbi(.ccp_perfil(datos))
  traza <- .ccp_traza(perfil)
  expect_identical(traza$estado, "no_disponible")
  expect_identical(traza$sentinelas_contados, -9)
  plan <- planificar_limpieza(perfil)
  expect_identical(as.numeric(plan$parametros[[.ccp_fila(plan)]]$valores), -9)
})

test_that("lo convertido es lo contado sobre tablas al azar, con el campo y sin el", {
  set.seed(20261113)
  catalogo <- c(-9999, -999, -99, -9, 999)
  for (i in seq_len(60L)) {
    forma <- sample(c("numeracion", "importes", "edades", "pocos"), 1L)
    n <- sample(c(40L, 300L), 1L)
    base <- switch(
      forma,
      numeracion = sample(c(1L, 500L, 900L), 1L) + seq_len(n) - 1L,
      importes = round(stats::runif(n, 50, 5000)),
      edades = sample(0:105, n, TRUE),
      pocos = sample(c(1, 2, 3, 99, 999), n, TRUE)
    )
    centinelas <- sample(catalogo, sample(0:3, 1L))
    veces <- sample(c(1L, 2L, 5L, 30L), length(centinelas), TRUE)
    x <- c(base, rep(centinelas, veces))
    clase <- sample(c("numero", "texto", "factor"), 1L)
    columna <- if (clase == "numero") as.numeric(x) else as.character(x)
    if (clase != "numero" && stats::runif(1L) < 0.6) {
      columna <- c(columna, rep(.ccp_marcas, each = sample(c(2L, 12L), 1L)))
    }
    if (clase == "factor") columna <- factor(columna)
    datos <- data.frame(x = columna, stringsAsFactors = FALSE)
    perfil <- .ccp_perfil(datos)
    contados <- .ccp_contados(perfil)
    if (!isTRUE(contados > 0)) next
    campo <- .ccp_traza(perfil)$sentinelas_contados
    numeros <- suppressWarnings(as.numeric(as.character(datos$x)))
    # Las celdas cuyo numero esta en el campo son las que el perfil conto.
    expect_identical(sum(numeros %in% campo), as.integer(contados))
    esperadas <- which(numeros %in% campo)
    # Y la accion las convierte: con la lista de la traza y, sin el campo,
    # acotando sobre los datos.
    for (p in list(perfil, .ccp_sin_campo(perfil))) {
      for (con_datos in c(TRUE, FALSE)) {
        hecho <- .ccp_aplicar(.ccp_plan(p, datos, con_datos), datos)
        expect_identical(hecho$convertidas, esperadas)
        expect_identical(hecho$n, contados)
      }
    }
  }
})
