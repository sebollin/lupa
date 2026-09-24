# El plan promete declarar lo que no cubre y declaraba dos huecos: el
# diagnostico que no se evaluo y el hallazgo cuya columna es ambigua. Habia un
# tercero sin declarar y era el mas silencioso: el hallazgo medido, con su
# columna identificada, que ninguna rama del planificador convierte en accion.
# Quien leia el plan no tenia forma de distinguirlo de "no hay nada que hacer".

tabla_o48 <- function() {
  set.seed(48)
  n <- 40
  data.frame(
    id = seq_len(n),
    nombre = c(rep("Ana", 3), rep("AN A", 2), paste0("Persona", 1:(n - 5))),
    ciudad = rep(c("MVD", "MVD ", "mvd", "SLGO"), length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("un hallazgo medido sin accion se declara y dice su motivo", {
  datos <- tabla_o48()
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)

  hallazgos <- as.data.frame(perfil$hallazgos)
  llaves_plan <- paste(as.data.frame(plan)$columna,
                       as.data.frame(plan)$hallazgo)
  medidos <- hallazgos[as.character(hallazgos$severidad) != "ok", ,
                       drop = FALSE]
  sin_accion_medido <- medidos[
    !paste(medidos$columna, medidos$tipo_hallazgo) %in% llaves_plan, ,
    drop = FALSE
  ]
  expect_gt(nrow(sin_accion_medido), 0L)

  declarados <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_s3_class(declarados, "data.frame")
  expect_true(all(
    c("hallazgo", "columna", "severidad", "motivo", "como_resolverlo") %in%
      names(declarados)
  ))
  expect_setequal(
    paste(declarados$columna, declarados$hallazgo),
    paste(sin_accion_medido$columna, sin_accion_medido$tipo_hallazgo)
  )
  expect_true(all(nzchar(declarados$motivo)))
  expect_true(all(nzchar(declarados$como_resolverlo)))
})

test_that("todo hallazgo medido produce accion o queda declarado", {
  # La promesa completa: leer el plan no puede dejar un hallazgo del perfil sin
  # accion y sin mencion. Se mide sobre las dos tablas del paquete, que no se
  # armaron para esta prueba.
  for (datos in list(datos_administrativos, datos_operativos, tabla_o48())) {
    perfil <- perfilar(datos)
    plan <- planificar_limpieza(perfil, datos = datos)
    hallazgos <- as.data.frame(perfil$hallazgos)
    medidos <- hallazgos[as.character(hallazgos$severidad) != "ok", ,
                         drop = FALSE]
    expect_gt(nrow(medidos), 0L)

    con_accion <- paste(as.data.frame(plan)$columna,
                        as.data.frame(plan)$hallazgo)
    declarados <- rbind(
      attr(plan, "hallazgos_sin_accion", exact = TRUE)[, c("hallazgo", "columna")],
      attr(plan, "hallazgos_sin_accion_por_columna_ambigua",
           exact = TRUE)[, c("hallazgo", "columna")]
    )
    cubiertos <- paste(medidos$columna, medidos$tipo_hallazgo) %in% con_accion |
      paste(medidos$columna, medidos$tipo_hallazgo) %in%
        paste(declarados$columna, declarados$hallazgo)
    expect_true(
      all(cubiertos),
      info = paste(
        "sin accion y sin declarar:",
        paste(medidos$tipo_hallazgo[!cubiertos], collapse = ", ")
      )
    )
  }
})

test_that("el hallazgo que si produce accion no se declara como hueco", {
  # El control tiene que poder fallar: si el atributo listara todo, esta
  # comprobacion no distinguiria nada.
  datos <- tabla_o48()
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)
  declarados <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  con_accion <- unique(paste(as.data.frame(plan)$columna,
                             as.data.frame(plan)$hallazgo))

  expect_gt(length(con_accion), 0L)
  expect_gt(nrow(declarados), 0L)
  expect_false(any(
    paste(declarados$columna, declarados$hallazgo) %in% con_accion
  ))
})

test_that("el hueco de la columna ambigua se declara una sola vez", {
  datos <- data.frame(
    a = c(" x ", " y "), a = c(" z ", " w "),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)

  expect_gt(
    nrow(attr(plan, "hallazgos_sin_accion_por_columna_ambigua",
              exact = TRUE)),
    0L
  )
  expect_equal(
    nrow(attr(plan, "hallazgos_sin_accion", exact = TRUE)), 0L
  )
})

test_that("la impresion del plan menciona el hueco", {
  datos <- tabla_o48()
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)
  salida <- NULL
  invisible(capture.output(salida <- cli::cli_fmt(print(plan))))

  expect_true(any(grepl("no tienen? acci", salida)))
  expect_true(any(grepl("hallazgos_sin_accion", salida, fixed = TRUE)))
})

test_that("el hueco declarado no publica un valor protegido", {
  # El atributo copia la sugerencia del hallazgo. Si esa sugerencia nombrara un
  # valor de una columna personal, el plan lo publicaria por una puerta que la
  # proteccion no miraba.
  datos <- data.frame(
    cedula = c("1.112.222-3", "4.556.677-8", "1.112.222-3", "9.998.887-6"),
    ciudad = c("MVD", "MVD ", "mvd", "SLGO"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)
  declarados <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  protegidas <- attr(plan, "columnas_datos_personales_protegidas",
                     exact = TRUE)

  expect_true("cedula" %in% protegidas)
  if (nrow(declarados)) {
    texto <- paste(unlist(declarados), collapse = " ")
    expect_false(grepl("1.112.222-3", texto, fixed = TRUE))
    expect_false(grepl("9.998.887-6", texto, fixed = TRUE))
  }
})

test_that("el analisis conserva el hueco declarado en su plan", {
  datos <- tabla_o48()
  analisis <- analizar(
    datos, fecha = as.POSIXct("2026-09-24 10:00:00", tz = "UTC")
  )
  declarados <- attr(analisis$plan_limpieza, "hallazgos_sin_accion",
                     exact = TRUE)

  expect_s3_class(declarados, "data.frame")
  expect_gt(nrow(declarados), 0L)
})

test_that("el informe publica el hueco, no solo la consola", {
  # Una declaracion vale en todas las salidas: el informe es el que se manda a
  # otra persona, y publicaba "Acciones propuestas: 5" sin decir que habia un
  # hallazgo medido sin accion.
  datos <- tabla_o48()
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos = datos)
  declarados <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_gt(nrow(declarados), 0L)

  archivo <- file.path(tempdir(), "plan-o48.html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(plan, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE), collapse = "\n")

  expect_true(grepl("Hallazgos medidos sin acci", html))
  expect_true(all(vapply(
    declarados$hallazgo, function(tipo) grepl(tipo, html, fixed = TRUE),
    logical(1L)
  )))
})
