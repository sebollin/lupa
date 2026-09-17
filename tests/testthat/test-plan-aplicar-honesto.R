capturar_cli_plan <- function(expr) {
  # `cli` emite su salida como condiciones de mensaje y dentro de `testthat`
  # quedan atrapadas antes de llegar a ningun flujo, asi que el `sink()` recogia
  # un archivo vacio y la prueba fallaba corrida sola. `salida_cli()` las recoge
  # como condiciones -ver `helper-salida-cli.R`-.
  salida_cli(expr)
}

activar_una_accion_honesta <- function(plan, estrategia) {
  indice <- which(plan$estrategia == estrategia)
  stopifnot(length(indice) == 1L)
  grupo <- plan$grupo[[indice]]
  if (!is.na(grupo)) plan$aplicar[plan$grupo == grupo] <- FALSE
  plan$aplicar[[indice]] <- TRUE
  plan$decision_grupo[plan$grupo == grupo] <- "elegida"
  plan
}

test_that("la unidad del hallazgo viaja al plan y al modo guiado", {
  datos <- data.frame(
    zona = c(rep("Activo", 60L), rep("ACTIVO", 30L), rep("activo", 10L)),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  plan <- plan[plan$hallazgo == "mayusculas_inconsistentes", , drop = FALSE]
  indice <- which(plan$estrategia == "convertir_minusculas")

  expect_equal(plan$n_afectadas[[indice]], 3)
  expect_equal(plan$unidad_conteo[[indice]], "valor_distinto")
  expect_match(capturar_cli_plan(print(plan)), "unidad_conteo")

  guiado <- capturar_cli_plan(guiar_limpieza(
    plan, datos, selector = function(decision) "convertir_minusculas"
  ))
  expect_match(guiado, "Cantidad estimada: 3.*valor_distinto")

  plan <- activar_una_accion_honesta(plan, "convertir_minusculas")
  resultado <- aplicar(plan, datos)
  expect_equal(resultado$registro$n_cambiadas, 90)
  expect_equal(resultado$registro$estado, "ejecutada")
})

test_that("un efecto nulo no se registra como ejecutado", {
  datos <- data.frame(x = c(" NA ", NA, "x"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)
  resultado <- aplicar(plan, datos)
  recorte <- resultado$registro$estrategia == "recortar_espacios"

  expect_true(any(recorte))
  expect_equal(resultado$registro$n_cambiadas[recorte], 0)
  expect_equal(resultado$registro$estado[recorte], "fallida")
  expect_match(resultado$registro$error[recorte], "sin efecto")
  expect_true(all(is.na(resultado$datos$x[1:2])))
})

test_that("un plan editado conserva el control de efecto sin estimacion", {
  sucios <- data.frame(
    x = paste0(letters[1:6], " "), stringsAsFactors = FALSE
  )
  limpios <- data.frame(x = letters[1:6], stringsAsFactors = FALSE)
  plan_base <- planificar_limpieza(
    perfilar(sucios, analizar_dependencias = FALSE), sucios
  )
  indice <- which(plan_base$estrategia == "recortar_espacios")
  expect_length(indice, 1L)

  activar <- plan_base
  activar$aplicar[] <- FALSE
  activar$aplicar[[indice]] <- TRUE

  # La prueba positiva fija que la acción sí corre y publica las seis celdas
  # que efectivamente cambia.
  positivo <- aplicar(activar, sucios)
  fila_positiva <- positivo$registro[
    positivo$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]
  expect_equal(fila_positiva$estado, "ejecutada")
  expect_equal(fila_positiva$n_cambiadas, 6L)

  # El plan editado puede quitar la acción: no se confunde ausencia de acción
  # con una acción ejecutada sin efecto.
  sin_accion <- activar[-indice, , drop = FALSE]
  sin_accion$aplicar[] <- FALSE
  resultado_sin_accion <- aplicar(sin_accion, limpios)
  expect_false(any(resultado_sin_accion$registro$estrategia ==
                    "recortar_espacios"))

  # Con el mismo plan intacto, datos ya limpios dejan n_cambiadas = 0 y fallan.
  sin_efecto <- aplicar(activar, limpios)
  fila_sin_efecto <- sin_efecto$registro[
    sin_efecto$registro$estrategia == "recortar_espacios", , drop = FALSE
  ]
  expect_equal(fila_sin_efecto$estado, "fallida")
  expect_equal(fila_sin_efecto$n_cambiadas, 0L)
  expect_match(fila_sin_efecto$error, "sin efecto", fixed = TRUE)

  # La ausencia del estimado no apaga el control y el motivo deja constancia
  # de que se decidió por el efecto observado.
  estimado_ausente <- activar
  estimado_ausente$n_afectadas[[indice]] <- NA_real_
  fallo_ausente <- aplicar(estimado_ausente, limpios)$registro
  expect_equal(fallo_ausente$estado, "fallida")
  expect_equal(fallo_ausente$n_cambiadas, 0L)
  expect_match(fallo_ausente$error, "efecto observado", fixed = TRUE)

  # Un número editado que no coincide con el perfil tampoco convierte la
  # comparación en una igualdad esperada/real: se registra lo que ocurrió.
  estimado_editado <- activar
  estimado_editado$n_afectadas[[indice]] <- 999
  resultado_editado <- aplicar(estimado_editado, sucios)
  expect_equal(resultado_editado$registro$estado, "ejecutada")
  expect_equal(resultado_editado$registro$n_cambiadas, 6L)

  # Incluso un estimado editado a cero no puede convertir un efecto nulo en
  # éxito: una acción seleccionada que no cambia nada sigue siendo fallida.
  estimado_cero <- activar
  estimado_cero$n_afectadas[[indice]] <- 0
  fallo_cero <- aplicar(estimado_cero, limpios)$registro
  expect_equal(fallo_cero$estado, "fallida")
  expect_equal(fallo_cero$n_cambiadas, 0L)

  # Un parámetro inválido del plan editado se registra en la misma bitácora,
  # sin dejar que la acción escriba una columna inventada.
  con_ausentes <- data.frame(x = c(1, NA_real_), y = 2:3)
  plan_ausentes <- planificar_limpieza(
    perfilar(con_ausentes, analizar_dependencias = FALSE), con_ausentes
  )
  indice_marca <- which(plan_ausentes$estrategia == "marcar_filas_ausentes")
  expect_length(indice_marca, 1L)
  plan_ausentes$aplicar[] <- FALSE
  plan_ausentes$aplicar[[indice_marca]] <- TRUE
  plan_ausentes$parametros[[indice_marca]]$columna_marca <- NA_character_
  fallo_parametro <- aplicar(plan_ausentes, con_ausentes)
  expect_equal(fallo_parametro$registro$estado, "fallida")
  expect_match(fallo_parametro$registro$error, "columna_marca", fixed = TRUE)
  expect_identical(names(fallo_parametro$datos), names(con_ausentes))
})

test_that("la composicion de acciones de texto tiene un orden efectivo", {
  datos <- data.frame(x = c("\u200B A ", "B"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  expect_true(all(plan$aplicar[plan$columna == "x"]))
  expect_lt(
    plan$orden[plan$estrategia == "eliminar_controles_invisibles"],
    plan$orden[plan$estrategia == "recortar_espacios"]
  )

  resultado <- aplicar(plan, datos)
  expect_equal(resultado$datos$x, c("A", "B"))
  expect_true(all(resultado$registro$estado == "ejecutada"))
})

test_that("la imputacion usa los datos cuando el mapa fue protegido", {
  datos <- data.frame(
    documento = rep(c("12345678", "23456789"), each = 6L),
    provincia = rep(c("Norte", "Sur"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$provincia[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__documento")
  expect_length(indice, 1L)
  mapa <- plan$parametros[[indice]]$mapa
  expect_false(any(mapa$determinante %in% datos$documento))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$registro$n_cambiadas, 1)
  expect_equal(resultado$datos$provincia[[12L]], "Sur")
})

test_that("la imputacion con determinante no protegido conserva el mapa", {
  datos <- data.frame(
    codigo = rep(c("AA-001", "BB-002"), each = 6L),
    provincia = rep(c("Norte", "Sur"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$provincia[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__codigo")
  expect_length(indice, 1L)
  expect_true(any(plan$parametros[[indice]]$mapa$determinante %in% datos$codigo))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$registro$n_cambiadas, 1)
  expect_equal(resultado$datos$provincia[[12L]], "Sur")
})

test_that("la imputacion conserva el dependiente protegido sin publicar el mapa", {
  datos <- data.frame(
    codigo = rep(c("AA-001", "BB-002"), each = 6L),
    nombre = rep(c("Ana", "Beto"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$nombre[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__codigo")
  expect_length(indice, 1L)
  expect_false(any(plan$parametros[[indice]]$mapa$dependiente %in% datos$nombre))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$datos$nombre[[12L]], "Beto")
})

test_that("winsorizar deja sin outliers el perfil de una columna chica", {
  datos <- data.frame(x = c(1, 2, 3, 100))
  perfil <- perfilar(datos, muestra = Inf, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  elegido <- activar_una_accion_honesta(plan, "winsorizar_outliers")
  resultado <- aplicar(elegido, datos)
  indice <- resultado$registro$estrategia == "winsorizar_outliers"

  expect_equal(resultado$registro$estado[indice], "ejecutada")
  expect_equal(resultado$registro$n_cambiadas[indice], 1)
  reprofilado <- perfilar(
    resultado$datos, muestra = Inf, analizar_dependencias = FALSE
  )
  expect_false(any(reprofilado$hallazgos$tipo_hallazgo == "outliers"))
})

test_that("al transformar una columna clave se descarta la clave data.table", {
  skip_if_not_installed("data.table")
  datos <- data.table::data.table(x = c("ZETA", "Zeta", "alfa"))
  data.table::setkey(datos, x)
  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE))
  plan <- activar_una_accion_honesta(plan, "convertir_minusculas")
  resultado <- aplicar(plan, datos)

  expect_null(data.table::key(resultado$datos))
  expect_equal(resultado$datos$x, c("zeta", "zeta", "alfa"))
})

# Tres cosas que el plan decia y `aplicar()` desmentia, encontradas el 2026-09-05
# con el encargo de construir el caso donde el plan promete algo que no cumple.

test_that("n_cambiadas cuenta cambios y no valores presentes", {
  # Planificar sobre texto y aplicar sobre la MISMA tabla ya convertida a
  # entero: la conversion es una identidad y no cambia nada. Catorce acciones de
  # `R/remediacion.R` cuentan `sum(cambio)`; las dos conversiones contaban
  # `sum(!is.na(x))`, o sea los presentes, y por eso declaraban `ejecutada` con
  # `n_cambiadas = 4` sobre una columna que quedaba bit a bit igual.
  datos <- data.frame(x = c("1", "2", "3", "4"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  skip_if_not(any(plan$estrategia == "convertir_tipo"),
              "el plan no propuso `convertir_tipo` en este entorno")

  # Primera mitad: sobre los datos originales la accion SI convierte. Sin esto,
  # un `n_cambiadas = 0` podria venir de que la accion no corrio.
  real <- aplicar(plan, datos)
  fila_real <- real$registro[real$registro$estrategia == "convertir_tipo", ]
  expect_equal(fila_real$estado, "ejecutada")
  expect_equal(fila_real$n_cambiadas, 4L)
  expect_false(identical(class(real$datos$x), class(datos$x)))

  ya_convertidos <- datos
  ya_convertidos$x <- as.integer(ya_convertidos$x)
  identidad <- aplicar(plan, ya_convertidos)
  fila <- identidad$registro[identidad$registro$estrategia == "convertir_tipo", ]

  # La columna no cambio, asi que el conteo es cero y el estado es `fallida`
  # con su motivo: es lo que `man/planificar_limpieza.Rd` promete para una
  # accion sin efecto cuando el plan estimaba alguno.
  expect_true(identical(identidad$datos$x, ya_convertidos$x))
  expect_equal(fila$n_cambiadas, 0L)
  expect_equal(fila$estado, "fallida")
  expect_match(paste(fila$error, collapse = " "), "sin efecto", fixed = TRUE)
})

test_that("los duplicados se agrupan igual en integer64 que en double", {
  skip_if_not_installed("bit64")
  # `duplicated(x, fromLast = TRUE)` sobre `integer64` devuelve lo mismo que sin
  # `fromLast`: el metodo de bit64 ignora el argumento. Con eso, de un grupo de
  # dos filas identicas se marcaba una sola.
  valores <- c(1, 2, -999, 4, -999)
  con_64 <- data.frame(v = bit64::as.integer64(valores))
  con_dbl <- data.frame(v = valores)

  aplicar_marca <- function(datos) {
    plan <- planificar_limpieza(
      perfilar(datos, analizar_dependencias = FALSE), datos
    )
    aplicar(plan, datos)
  }
  r64 <- aplicar_marca(con_64)
  rdbl <- aplicar_marca(con_dbl)

  # Primera mitad: la accion corrio en los dos casos.
  fila64 <- r64$registro[r64$registro$estrategia == "marcar_filas_duplicadas", ]
  expect_equal(nrow(fila64), 1L)
  expect_equal(fila64$estado, "ejecutada")

  # Las dos filas del grupo quedan identificadas, como declara la documentacion,
  # y el tipo no cambia el resultado.
  expect_equal(r64$datos$.grupo_duplicado, rdbl$datos$.grupo_duplicado)
  expect_equal(sum(!is.na(r64$datos$.grupo_duplicado)), 2L)
  expect_equal(fila64$n_cambiadas, 2L)
})

test_that("el numero del plan y su unidad de conteo hablan de lo mismo", {
  # `unidad_conteo` se heredaba del hallazgo pisando lo que la accion declaraba:
  # `convertir_tipo` cuenta valores presentes y quedaba con `columna`, asi que
  # el plan decia "5 columna" sobre una tabla de UNA columna.
  datos <- data.frame(x = c("1", "1", "2", "2", "3"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  fila <- plan[plan$estrategia == "convertir_tipo", ]
  skip_if_not(nrow(fila) == 1L, "el plan no propuso `convertir_tipo`")

  expect_equal(fila$unidad_conteo, "valor")
  expect_equal(fila$n_afectadas, sum(!is.na(datos$x)))

  # Control, y es el que hace que esto pruebe algo: una accion que SI cuenta en
  # la unidad del hallazgo tiene que seguir heredandola. Sin esta mitad, poner
  # "valor" en todas pasaria el test.
  con_duplicados <- data.frame(a = c(1, 1, 2, 2, 3), b = c(1, 1, 2, 2, 3))
  plan2 <- planificar_limpieza(
    perfilar(con_duplicados, analizar_dependencias = FALSE), con_duplicados
  )
  columnas <- plan2[plan2$estrategia == "marcar_columnas_duplicadas", ]
  filas <- plan2[plan2$estrategia == "marcar_filas_duplicadas", ]
  if (nrow(columnas)) expect_equal(columnas$unidad_conteo, "columna")
  if (nrow(filas)) expect_equal(filas$unidad_conteo, "fila")
})

# Quien edita un plan no conoce las funciones internas. Antes de arreglarlo, un
# nombre de columna vacio o NA hacia fallar la accion con el mensaje interno de
# R, que no dice que corregir; y con la cadena vacia no fallaba: creaba una
# columna llamada "V3".
test_that("un nombre de columna invalido falla en los terminos del plan", {
  datos <- data.frame(
    x = c(1, 2, NA, 4, 1000), t = c("a", " b ", "a", "", "b"),
    stringsAsFactors = FALSE
  )
  base <- planificar_limpieza(perfilar(datos))
  fila <- which(base$estrategia == "marcar_filas_ausentes")[[1L]]

  invalidos <- list(
    `largo cero` = character(0),
    ausente = NA_character_,
    vacio = "",
    espacios = "   ",
    `dos valores` = c("a", "b")
  )
  for (etiqueta in names(invalidos)) {
    plan <- base
    plan$aplicar <- FALSE
    plan$aplicar[fila] <- TRUE
    plan$parametros[[fila]]$columna_marca <- invalidos[[etiqueta]]
    resultado <- aplicar(plan, datos)

    motivo <- as.character(resultado$registro$error)[[1L]]
    expect_false(is.na(motivo), info = etiqueta)
    # El motivo nombra el parametro y la accion, que es lo que el usuario edito.
    expect_match(motivo, "columna_marca", info = etiqueta)
    expect_match(motivo, "marcar_filas_ausentes", info = etiqueta)
    # Y no deja rastro en los datos: ni la columna pedida ni una inventada.
    expect_identical(names(resultado$datos), names(datos), info = etiqueta)
  }
})

# La mitad de control, y no es decorativa: seis de los veintiocho parametros que
# produce un plan legitimo son de largo cero o todo NA -`clave = character(0)`
# significa "sin clave declarada"-, asi que una validacion general sobre todos
# los parametros rechazaria planes que funcionan.
test_that("los parametros legitimamente vacios siguen aplicandose", {
  datos <- data.frame(
    x = c(1, 2, NA, 4, 1000), t = c("a", " b ", "a", "", "b"),
    stringsAsFactors = FALSE
  )
  base <- planificar_limpieza(perfilar(datos))
  fila <- which(base$estrategia == "marcar_filas_ausentes")[[1L]]

  plan <- base
  plan$aplicar <- FALSE
  plan$aplicar[fila] <- TRUE
  resultado <- aplicar(plan, datos)
  expect_true(is.na(as.character(resultado$registro$error)[[1L]]))
  expect_true(".ausente_x" %in% names(resultado$datos))

  # Un nombre elegido a mano por quien edita el plan es valido y se respeta.
  plan$parametros[[fila]]$columna_marca <- "revisar_x"
  resultado <- aplicar(plan, datos)
  expect_true(is.na(as.character(resultado$registro$error)[[1L]]))
  expect_true("revisar_x" %in% names(resultado$datos))

  # Y un plan cuya clave es character(0) -"sin clave declarada"- aplica entero.
  duplicados <- data.frame(
    id = c(1, 1, 2, 2, 3), v = c("a", "a", "b", "b", "c"),
    stringsAsFactors = FALSE
  )
  plan_dup <- planificar_limpieza(perfilar(duplicados))
  plan_dup$aplicar <- plan_dup$recomendada
  claves <- vapply(
    plan_dup$parametros, function(p) length(p$clave %||% NULL) == 0L, logical(1)
  )
  expect_true(any(claves))
  resultado <- aplicar(plan_dup, duplicados, permitir_eliminacion = TRUE)
  expect_equal(sum(!is.na(resultado$registro$error)), 0L)
})

# `marcar_filas_ausentes` corria ANTES de las conversiones que crean ausentes
# -textuales y centinelas numericos-, asi que la columna de marca terminaba
# diciendo FALSE en filas que quedaban NA. No era un dato que faltara: era una
# afirmacion falsa sobre los datos, publicada con las dos acciones en
# `ejecutada` y sin error, y alcanzable con el idioma documentado
# `plan$aplicar <- plan$recomendada`.
test_that("la marca de ausentes cubre los que crea el propio plan", {
  casos <- list(
    textuales = data.frame(
      t = c("a", "N/A", NA, "b", "sin dato", "c"), stringsAsFactors = FALSE
    ),
    centinelas = data.frame(x = c(1, 2, -999, 4, -999, 6, NA)),
    mixto = data.frame(
      t = c("a", "N/D", NA, " b ", "S/D", "c"),
      x = c(1, -999, NA, 4, 5, 6),
      stringsAsFactors = FALSE
    )
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    plan <- planificar_limpieza(perfilar(datos))
    plan$aplicar <- plan$recomendada
    resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)

    marcas <- grep("^\\.ausente_", names(resultado$datos), value = TRUE)
    # Primero: el mecanismo se activo. Sin marca no se prueba nada.
    expect_true(length(marcas) > 0L, info = nombre)
    for (marca in marcas) {
      columna <- sub("^\\.ausente_", "", marca)
      if (!columna %in% names(resultado$datos)) next
      ausentes <- is.na(resultado$datos[[columna]])
      expect_equal(
        sum(ausentes & !resultado$datos[[marca]]), 0L,
        info = paste(nombre, marca, "niega un ausente")
      )
      # Y no marca de mas: la marca dice exactamente lo que es.
      expect_equal(
        sum(!ausentes & resultado$datos[[marca]]), 0L,
        info = paste(nombre, marca, "marca lo que no es ausente")
      )
    }
  }
})

# El control: el orden nuevo tiene que ser el que lo consigue, no una
# casualidad del fixture. Si la marca vuelve a correr antes, el test de arriba
# falla; esto fija que la secuencia sea la que corresponde.
test_that("las acciones sobre ausentes van despues de normalizarlos", {
  datos <- data.frame(
    t = c("a", "N/A", NA, "b", "sin dato", "c"), stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos))
  orden_de <- function(estrategia) {
    plan$orden[plan$estrategia == estrategia][[1L]]
  }
  expect_lt(
    orden_de("convertir_ausencias_textuales"),
    orden_de("marcar_filas_ausentes")
  )
  expect_lt(
    orden_de("convertir_ausencias_textuales"),
    orden_de("eliminar_filas_ausentes")
  )
  # Y la marca sigue antes que los cambios de esquema, que van al final.
  expect_lt(orden_de("marcar_filas_ausentes"), 500L)
})
