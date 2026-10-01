# Ronda 15-B: estimaciones externas, senales redundantes, clases de variable,
# patrones y proteccion de correos.

test_that("medicion_desde_estimaciones lee un factor por su texto", {
  est <- data.frame(
    celda = c("Montevideo", "Interior"),
    stat = factor(c("0.42", "0.38")), cv = factor(c("0.08", "0.34")),
    n = factor(c("1200", "90"))
  )
  m <- medicion_desde_estimaciones(est, entidad = "e", atributo = "celda",
                                   fuente = "s")
  # Antes: los codigos de nivel, 2 y 1.
  expect_equal(m$resultado[m$metrica == "Estimacion"], c(0.42, 0.38))
  expect_equal(m$resultado[m$metrica == "TamanoMuestra"], c(1200, 90))
  # Control: como texto ya salia bien.
  est[] <- lapply(est, as.character)
  m <- medicion_desde_estimaciones(est, entidad = "e", atributo = "celda",
                                   fuente = "s")
  expect_equal(m$resultado[m$metrica == "Estimacion"], c(0.42, 0.38))
})

test_that("una estimacion fuera de [0, 1] se evalua, y una celda invalida se declara", {
  regla <- regla_evaluacion(
    "cv", function(x, maximo) x <= maximo,
    metricas = "CoeficienteVariacion@e", umbrales = list(maximo = 1)
  )
  validas <- data.frame(celda = c("A", "B"), stat = c(35000, -2.5),
                        n = c(10, 20), se = c(1200, 0.2), cv = c(1.5, 0.9),
                        df = c(30, 12))
  m <- medicion_desde_estimaciones(validas, entidad = "e", atributo = "celda",
                                   fuente = "s")
  # Antes: `real` es [0, 1] y `evaluar()` rechazaba la medicion entera.
  evaluada <- evaluar(m, perfil_evaluacion("p", regla))
  expect_equal(evaluada$reglas$resultado, 0.5)

  invalidas <- data.frame(celda = c("A", "B"), stat = c(0.5, 0.6),
                          n = c(0.5, -3), se = c(-0.1, 0.2), cv = c(1.5, 0.9))
  expect_warning(
    m <- medicion_desde_estimaciones(invalidas, entidad = "e",
                                     atributo = "celda", fuente = "s"),
    "fuera del dominio"
  )
  expect_false(any(m$metrica == "TamanoMuestra"))
  expect_equal(nrow(attr(m, "celdas_descartadas")), 3L)
  expect_true(all(attr(m, "celdas_descartadas")$motivo == "fuera_de_dominio"))
  # Y lo valido que queda se evalua.
  expect_s3_class(evaluar(m, perfil_evaluacion("p", regla)), "evaluacion_calidad")
})

test_that("la evidencia de una discordancia no se contradice ni se imprime igual", {
  d <- data.frame(a = c(1, 1, 2), b = c(1, 2, 1))
  r <- detectar_discordancias(d, senal_redundante(c("a", "b")), max_ejemplos = 0)
  # Antes: "sin filas discordantes" al lado de `n_discordantes = 2`.
  expect_match(r$evidencia, "2 filas discordantes, ninguna citada", fixed = TRUE)
  iguales <- data.frame(a = c(1, 1), b = c(1, 1))
  r <- detectar_discordancias(iguales, senal_redundante(c("a", "b")),
                              max_ejemplos = 0)
  expect_match(r$evidencia, "sin filas discordantes", fixed = TRUE)

  r <- detectar_discordancias(data.frame(a = c(0.1 + 0.2, 1), b = c(0.3, 1)),
                              senal_redundante(c("a", "b")))
  # Antes: "a=0.3; b=0.3".
  expect_match(r$evidencia, "a=0.30000000000000004; b=0.3", fixed = TRUE)
  r <- detectar_discordancias(data.frame(a = c(20, 5), b = c(21, 5)),
                              senal_redundante(c("a", "b")))
  expect_match(r$evidencia, "a=20; b=21", fixed = TRUE)
})

test_that("clasificar_variables no llama entero a Inf ni cuenta niveles que no guarda", {
  r <- clasificar_variables(data.frame(
    a = c(Inf, Inf, Inf), b = c(-Inf, Inf, NaN), c = c(1, 2, Inf),
    d = c("x", "y", "x")
  ))
  # Antes: `a` discreta ("Los valores son enteros") y `b` binaria.
  expect_identical(r$escala_propuesta[r$columna == "a"], "desconocida")
  expect_identical(r$escala_propuesta[r$columna == "b"], "desconocida")
  expect_identical(r$escala_propuesta[r$columna == "c"], "discreta")
  # Antes: 0 niveles observados en una columna 1, 2, 3.
  expect_true(is.na(r$n_niveles_observados[r$columna == "c"]))
  expect_equal(r$n_niveles_observados[r$columna == "d"], 2L)
})

test_that("un patron no publica digitos ni letras no ASCII", {
  p <- descubrir_patrones(c("123", "\u0661\u0662\u0663"), expandir = FALSE)
  # Antes: "\u0661\u0662\u0663" salia como su propio patron.
  expect_identical(p$patron, "9+")
  p <- descubrir_patrones(c("Jos\u00e9", "\u00c9XITO"), expandir = FALSE)
  expect_setequal(p$patron, c("Aa+", "A+"))
  # Y lo mismo bajo C, con el texto sin marca.
  sin_marca <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xc3, 0xa9)))
  original <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", original)), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  expect_identical(lupa:::.generalizar_a_patron(sin_marca), "Aa+")
})

test_that("una columna con correos se protege aunque no sean mayoria", {
  d <- data.frame(
    dato = c(paste0("persona", 1:70, "@correo.uy"), paste0("x", 1:30)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(d))
  salida <- unlist(lapply(p, function(x) utils::capture.output(print(x))))
  # Antes: seis direcciones enteras en los ejemplos de los patrones.
  expect_false(any(grepl("@correo.uy", salida, fixed = TRUE)))
  expect_true(p$datos_personales$proteger[p$datos_personales$columna == "dato"])
  ejemplos <- descubrir_patrones(c("juan@mail.com", "ana@mail.com", "pedro"),
                                 expandir = FALSE)$ejemplos
  expect_false(any(grepl("@", ejemplos, fixed = TRUE)))
  # Control: sin correos, los ejemplos se publican.
  ejemplos <- descubrir_patrones(c("lunes", "martes"), expandir = FALSE)$ejemplos
  expect_match(ejemplos, "lunes", fixed = TRUE)
})
