# Dos cosas que una corrida contra bases reales dejo abiertas, las dos sobre que
# le llega al usuario y ninguna sobre el calculo.

test_that("el aviso de trazabilidad dice de quien es el problema", {
  # Las seis condiciones que lo disparan comparan dos salidas del propio `lupa`
  # -lo que el hallazgo afirma contra las filas que se le adjuntaron- y ninguna
  # mira los datos del usuario. El mensaje decia "la trazabilidad contiene
  # incoherencias", que manda a buscar en la tabla algo que esta en el codigo.
  datos <- data.frame(
    categoria = c(rep("alfa", 8L), rep("ALFA", 4L), rep("beta", 8L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  indice <- which(perfil$hallazgos$tipo_hallazgo == "mayusculas_inconsistentes")
  skip_if(length(indice) != 1L, "el fixture no produjo el hallazgo esperado")

  roto <- perfil$hallazgos
  roto$n_afectados[[indice]] <- 1
  aviso <- tryCatch(
    lupa:::.advertir_incoherencias_trazabilidad(roto, datos, "categoria"),
    lupa_trazabilidad_incoherente = function(w) conditionMessage(w)
  )

  expect_true(is.character(aviso))
  # Lo que el usuario necesita saber, en este orden.
  expect_match(aviso, "problema de `lupa`, no de sus datos", fixed = TRUE)
  expect_match(aviso, "nada que corregir en la tabla", fixed = TRUE)
  # La consecuencia para lo que esta leyendo, que si le importa.
  expect_match(aviso, "no corresponderse con lo que el hallazgo cuenta", fixed = TRUE)
  expect_match(aviso, "se conserva para no ocultar la evidencia", fixed = TRUE)
  # Y el detalle interno sigue estando, con la etiqueta del hallazgo, que es lo
  # que lo hace reportable. Cual de las seis condiciones aparece depende del
  # fixture -este produce la de unidades y filas-, asi que se comprueba la
  # etiqueta y no una frase concreta.
  expect_match(aviso, "mayusculas_inconsistentes en categoria", fixed = TRUE)
  expect_match(aviso, "conviene reportarlo con este detalle", fixed = TRUE)
})

test_that("un perfil intacto no emite ese aviso", {
  # La contraparte que hace valer la prueba de arriba: si el aviso saliera
  # tambien en una corrida normal, el texto nuevo estaria mintiendo al decir que
  # es un problema del paquete.
  datos <- data.frame(
    categoria = c(rep("alfa", 8L), rep("ALFA", 4L), rep("beta", 8L)),
    stringsAsFactors = FALSE
  )
  expect_no_warning(
    perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
    ),
    class = "lupa_trazabilidad_incoherente"
  )
})

test_that("el alcance LSH declara que sus candidatos dependen del orden", {
  skip_if_not_installed("stringdist")
  set.seed(21)
  base <- replicate(200L, paste(sample(LETTERS, 10L, TRUE), collapse = ""))
  datos <- data.frame(
    nombre = c(paste0(base, " SOCIEDAD ANONIMA"), paste0(base, " SOCIEDAD ANONMA")),
    stringsAsFactors = FALSE
  )
  con_lsh <- detectar_duplicados_aproximados(
    datos, umbral = 0.20, estrategia = "lsh", max_resultados = 50L,
    proteger_datos_personales = FALSE
  )$alcance
  sin_lsh <- detectar_duplicados_aproximados(
    datos, umbral = 0.20, estrategia = "teselas", max_resultados = 50L,
    proteger_datos_personales = FALSE
  )$alcance

  expect_true(con_lsh$lsh_candidatos_dependen_orden_filas)
  # No es un booleano decorativo: nombra el mecanismo, asi que si la numeracion
  # se canoniza alguna vez, este valor cambia y la dependencia deja de
  # declararse sola.
  expect_equal(con_lsh$lsh_orden_vocabulario, "primera_aparicion")
  # Y no aparece donde no corresponde: el camino exhaustivo no tiene esa
  # dependencia y no debe declarar una propiedad que no tiene.
  expect_false("lsh_candidatos_dependen_orden_filas" %in% names(sin_lsh))
  expect_false("lsh_orden_vocabulario" %in% names(sin_lsh))
})

test_that("el muestreo por cantidad fija de filas declina una muestra infinita", {
  # `muestra = Inf` -la tabla entera- es el valor por omision desde hoy, y el
  # muestreo EN EL MOTOR no puede acotar con eso: quedarse con "todas" las filas
  # no es muestrear.
  #
  # Esto salio de una refutacion externa: de las tres ramas de
  # `.forma_muestreo_dbi()`, dos se guardaron al aceptar `Inf` y la tercera se
  # paso por alto. Contra un motor real producia
  # `TABLESAMPLE RESERVOIR (Inf ROWS)` y un error de sintaxis; el paquete lo
  # declaraba honestamente, pero el usuario se quedaba sin perfil de muestra sin
  # haber pedido nada raro. Guardar caso por caso, en vez de en la entrada, es
  # exactamente como se olvida uno.
  candidato <- list(
    nombre = "tablesample_reservoir",
    descripcion = "TABLESAMPLE RESERVOIR (n ROWS)",
    tipo = "tablesample_filas",
    constructor = function(tabla, filas) paste0(
      tabla, " TABLESAMPLE RESERVOIR (", filas, " ROWS)"
    )
  )
  dialecto <- list(limitar = function(sql, n, salto) paste0(sql, " LIMIT ", n))

  expect_null(lupa:::.forma_muestreo_dbi(
    candidato, "`t`", "`a`", 10, Inf, dialecto
  ))
})

test_that("con una muestra finita ese mismo candidato sigue aplicando", {
  # El control que hace valer la prueba de arriba: si la guarda nueva hiciera
  # declinar tambien el caso finito, habria apagado el muestreo en el motor
  # entero y la prueba anterior pasaria igual, sin haber probado nada.
  candidato <- list(
    nombre = "tablesample_reservoir",
    descripcion = "TABLESAMPLE RESERVOIR (n ROWS)",
    tipo = "tablesample_filas",
    constructor = function(tabla, filas) paste0(
      tabla, " TABLESAMPLE RESERVOIR (", filas, " ROWS)"
    )
  )
  dialecto <- list(limitar = function(sql, n, salto) paste0(sql, " LIMIT ", n))

  forma <- lupa:::.forma_muestreo_dbi(
    candidato, "`t`", "`a`", 10, 2000L, dialecto
  )
  expect_false(is.null(forma))
  expect_match(forma$sql, "2000", fixed = TRUE)
  expect_false(grepl("Inf", forma$sql, fixed = TRUE))
})
