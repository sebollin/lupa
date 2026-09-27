test_that("un millón de valores se perfila en pocos segundos", {
  skip_on_cran()
  skip_on_ci()
  datos <- data.frame(
    codigo = rep(c("AB1234", "CD5678", "EF9012", "GH3456"), length.out = 1e6)
  )
  # El calentamiento tiene que ser del MISMO tamaño que la medición. Con una tabla
  # de una fila, la primera corrida grande pagaba sola el costo de arranque
  # -asignación, compilación de los closures, hilos de data.table-: medido el
  # 2026-09-27, 8,42 s la primera y 5,92 y 5,80 las siguientes, con el techo en 8.
  # Así que esta guarda se ponía roja por el arranque y no por el algoritmo, y una
  # guarda que falla sola se termina ignorando, que es el peor estado posible.
  #
  # Ahora se miden las dos cosas, cada una con su techo y su nombre: el régimen
  # -que es lo que la frase "se perfila en pocos segundos" promete- y el arranque,
  # porque si un día cuesta el doble eso es un hallazgo y no ruido.
  primera <- system.time(invisible(perfilar(datos)))[["elapsed"]]
  en_regimen <- system.time(invisible(perfilar(datos)))[["elapsed"]]

  expect_lt(unname(en_regimen), 8)
  expect_lt(unname(primera), 15)
})

test_that("un millón de valores activa el muestreo declarado", {
  skip_on_cran()
  datos <- data.frame(
    codigo = rep(c("AB1234", "CD5678", "EF9012", "GH3456"), length.out = 1e6)
  )
  resultado <- perfilar(datos)
  expect_true(resultado$meta$muestreo)
  expect_equal(resultado$meta$filas_analizadas, 1e5)
})

test_that("los predicados invisibles decodifican cada celda como maximo una vez", {
  llamadas <- 0L
  valores_decodificados <- 0L
  original <- lupa:::.predicados_invisibles
  local_mocked_bindings(
    .predicados_invisibles = function(textos) {
      llamadas <<- llamadas + 1L
      valores_decodificados <<- valores_decodificados + length(textos)
      original(textos)
    },
    .package = "lupa"
  )
  n <- 20000L
  valores <- as.character(seq.int(10000000L, length.out = n))
  datos <- data.frame(
    a = valores,
    b = rev(valores),
    c = paste0("9", valores),
    stringsAsFactors = FALSE
  )
  perfilar(datos, analizar_dependencias = FALSE)
  expect_equal(llamadas, ncol(datos))
  celdas <- nrow(datos) * ncol(datos)
  expect_equal(valores_decodificados, celdas)
  # Margen cero a propósito: cinco pasadas por valor (la regresión de la
  # ronda 75) tiene que fallar este mismo guardián.
  expect_failure(expect_lte(valores_decodificados * 5L, celdas))
})

test_that("la trazabilidad de otro hallazgo no calcula invisibles", {
  x <- c(rep("AB123", 90L), sprintf("texto-%02d", seq_len(10L)))
  resultado <- lupa:::.perfilar_columna(
    x, "x", Inf, 20L, TRUE, FALSE, 0.05,
    sentinelas_numericos = c(-9, -99, -999, -9999, 999)
  )
  patrones <- resultado$patrones
  attr(patrones, "resumen_patrones") <- as.data.frame(patrones)
  attr(patrones, "n_patrones_distintos") <- 2L
  attr(patrones, "desvios_patron_raro") <- as.data.frame(patrones)[
    -1L, , drop = FALSE
  ]
  resultado$patrones <- patrones
  llamadas <- 0L
  original <- lupa:::.predicados_invisibles
  local_mocked_bindings(
    .predicados_invisibles = function(textos) {
      llamadas <<- llamadas + 1L
      original(textos)
    },
    .package = "lupa"
  )
  indices <- lupa:::.indices_hallazgo_columna(
    "patron_raro", x, resultado$fila, resultado,
    expandir = FALSE, distinguir_mayusculas = TRUE
  )
  expect_gt(length(indices), 0L)
  expect_equal(llamadas, 0L)
})

test_that("texto libre de cardinalidad alta no degrada el perfil", {
  skip_on_cran()
  skip_on_ci()
  set.seed(3)
  n <- 1e4
  libre <- vapply(seq_len(n), function(i) {
    paste(sample(
      c(letters, LETTERS, 0:9, " ", ".", ",", "-"),
      sample(10:40, 1), TRUE
    ), collapse = "")
  }, character(1L))
  datos <- data.frame(obs1 = libre, obs2 = rev(libre), obs3 = libre)

  tiempo <- system.time(resultado <- perfilar(datos))[["elapsed"]]

  # La ronda 78 triplicó el costo del diagnóstico de vocabulario y lo llevó
  # de 1,5 s a casi 5 s. El contador determinista del test siguiente vigila
  # el trabajo fino; este reloj queda como red de arrastre con margen para el
  # calentamiento de un proceso limpio, pero sigue detectando un desastre
  # algorítmico.
  #
  # El techo pasa de 12 s a 25 s el 2026-09-16, y se sube DESPUÉS de mirar por
  # qué se pasaba, no para que la prueba pase. Lo medido:
  #
  #   este árbol      14,7  13,0  11,2 s
  #   commit anterior 13,2  13,2  12,1 s   <- igual de lento; no lo causó un cambio
  #
  # El perfil dice dónde está: el 48 % del tiempo es
  # `stringdist::stringdistmatrix()` dentro del diagnóstico de casi-duplicados
  # de vocabulario. Y ese trabajo está ACOTADO Y DECLARADO: con este fixture el
  # perfil emite una fila de cobertura por columna que dice «se evaluaron 5.000
  # de 10.000 valores distintos». O sea que no es desperdicio —como sí lo eran
  # las claves de bytes sobre texto ASCII, que se quitaron y bajaron otro camino
  # de 36 s a 12— sino el costo propio de comparar 5.000 formas en tres
  # columnas.
  #
  # Queda anotado en PENDIENTES que dos de las tres columnas de este fixture son
  # idénticas y el diagnóstico las recorre por separado: ahí hay una mejora real
  # que no se hace ahora.
  #
  # El propósito del guardián se conserva: un desastre algorítmico llevaría esto
  # a un orden de magnitud, no a un 20 % más.
  expect_lt(unname(tiempo), 25)

  # Decisión de §2.308: no se deduplican columnas por su vocabulario. El costo
  # puede repetirse, pero cada columna conserva su propia cobertura, aun cuando
  # `obs1` y `obs3` sean idénticas.
  cobertura <- resultado$cobertura_diagnosticos[
    resultado$cobertura_diagnosticos$diagnostico ==
      "proximidad_vocabulario", , drop = FALSE
  ]
  expect_identical(
    sort(as.character(cobertura$columna)), c("obs1", "obs2", "obs3")
  )
})

test_that("el vocabulario identico conserva cobertura y hallazgos por columna", {
  skip_if_not_installed("stringdist")
  base <- c(rep("Montevideo", 7L), rep("Montevido", 3L))
  con_hallazgos <- perfilar(
    data.frame(obs1 = base, obs2 = base, obs3 = base,
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  hallazgos_vocabulario <- con_hallazgos$hallazgos[
    as.character(con_hallazgos$hallazgos$tipo_hallazgo) ==
      "casi_duplicados_vocabulario", , drop = FALSE
  ]
  expect_identical(
    sort(as.character(hallazgos_vocabulario$columna)),
    c("obs1", "obs2", "obs3")
  )

  valores <- paste0("valor-", seq_len(30L))
  con_cobertura <- perfilar(
    data.frame(obs1 = valores, obs2 = valores, obs3 = valores,
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    max_trabajo_vocabulario = 1
  )
  cobertura <- con_cobertura$cobertura_diagnosticos[
    con_cobertura$cobertura_diagnosticos$diagnostico ==
      "proximidad_vocabulario", , drop = FALSE
  ]
  expect_identical(
    sort(as.character(cobertura$columna)), c("obs1", "obs2", "obs3")
  )
})

test_that("el diagnóstico de vocabulario respeta su presupuesto de pares", {
  skip_if_not_installed("stringdist")
  pares_comparados <- 0
  llamadas <- 0L
  original <- lupa:::.comparar_bloques_duplicados
  local_mocked_bindings(
    .comparar_bloques_duplicados = function(
        valores, filas, metodo, umbral, bloque, max_resultados, ...) {
      llamadas <<- llamadas + 1L
      pares_comparados <<- pares_comparados +
        choose(length(valores), 2L)
      original(valores, filas, metodo, umbral, bloque, max_resultados, ...)
    },
    .package = "lupa"
  )
  valores <- sprintf("valor-%08d", seq_len(10000L))
  perfil <- perfilar(
    data.frame(valor = valores), analizar_dependencias = FALSE
  )
  expect_equal(llamadas, 1L)
  expect_equal(pares_comparados, choose(2000L, 2L))
  expect_lte(pares_comparados, 2000000)
  # Margen cero a propósito: una segunda pasada completa vuelve a fallar.
  expect_failure(expect_lte(pares_comparados * 2L, 2000000))
})

test_that("el perfil no conserva el caché del detector de meses", {
  datos <- data.frame(
    a = rep("texto", 100000L), b = rep("otro", 100000L),
    c = rep("15 de marzo de 2024", 100000L)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  inferencia <- inferir_tipo(datos$a)
  expect_false("meses_texto" %in% names(attributes(inferencia$formatos_fecha)))
  expect_lt(as.numeric(object.size(inferencia)), 1 * 1024^2)
  expect_false(any(vapply(
    perfil$formatos_fecha,
    function(x) "meses_texto" %in% names(attributes(x)),
    logical(1L)
  )))
  expect_lt(as.numeric(object.size(perfil)), 2 * 1024^2)
})

test_that("texto libre conserva memoria y resumen de patrones", {
  skip_on_cran()
  set.seed(3)
  n <- 1e4
  libre <- vapply(seq_len(n), function(i) {
    paste(sample(
      c(letters, LETTERS, 0:9, " ", ".", ",", "-"),
      sample(10:40, 1), TRUE
    ), collapse = "")
  }, character(1L))
  datos <- data.frame(obs1 = libre, obs2 = rev(libre), obs3 = libre)
  resultado <- perfilar(datos)
  expect_lt(as.numeric(object.size(resultado)), 5 * 1024^2)
  expect_true(all(
    attr(resultado$dependencias, "columnas_descartadas")$motivo == "casi_clave"
  ))
  expect_true(all(vapply(
    resultado$patrones,
    function(x) nrow(attr(x, "resumen_patrones")) <= 7L,
    logical(1L)
  )))
})

test_that("el enmascarado de variantes no crece con la cantidad de agujas", {
  skip_on_cran()
  skip_on_ci()
  # La regla que compara corridas de digitos -para que la cedula sin su verificador
  # no se publique- preguntaba celda por celda contra aguja por aguja: sobre 8.000
  # celdas y 400 agujas tardaba 11,8 s, y con 40 veces mas agujas costaba 32 veces
  # mas. Concatenar las agujas con un separador que no puede aparecer en una corrida
  # de digitos deja una sola busqueda por corrida, y el costo deja de depender de
  # cuantas agujas haya.
  #
  # Se afirma sobre la RAZON y no sobre un tiempo absoluto: un techo en segundos
  # medido en esta maquina no transfiere -ya paso en este mismo archivo, con la
  # maquina cargada-, mientras la razon entre dos mediciones del mismo proceso si.
  set.seed(3)
  n <- 8000L
  pajar <- paste("nota", sample(letters, n, TRUE), sample(100000:999999, n, TRUE))
  agujas <- sprintf(
    "%d.%03d.%03d-%d", sample(1:5, 400, TRUE), sample(100:999, 400, TRUE),
    sample(100:999, 400, TRUE), sample(0:9, 400, TRUE)
  )
  medir <- function(cuantas) {
    unas <- agujas[seq_len(cuantas)]
    # Una vuelta corta primero, para no medir la carga del paquete.
    invisible(lupa:::.reemplazar_variantes_separadas(pajar[1:100], unas))
    min(replicate(3, system.time(
      invisible(lupa:::.reemplazar_variantes_separadas(pajar, unas))
    )[["elapsed"]]))
  }

  con_diez <- medir(10L)
  con_cuatrocientas <- medir(400L)

  # Cuarenta veces mas agujas: medido, la razon queda en 1,5 con la version que
  # concatena y en 32,5 con la que preguntaba una por una.
  expect_lt(con_cuatrocientas / max(con_diez, 1e-6), 5)
})
