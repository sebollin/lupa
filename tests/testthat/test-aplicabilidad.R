test_that("sin declarar nada, el universo es la tabla entera", {
  datos <- data.frame(
    a = c("Si", "No", "No", "Si"),
    b = c(1, NA, NA, 4)
  )
  p <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- p$columnas[p$columnas$columna == "b", ]
  expect_equal(fila$n_aplicables, 4)
  expect_equal(fila$n_no_aplica, 0)
  expect_equal(fila$n_faltantes, 2)
  expect_equal(fila$prop_faltantes, 0.5)
})

test_that("una regla de aplicabilidad saca del universo lo que no corresponde", {
  set.seed(3)
  n <- 2000
  tiene <- rep(c("Si", "No"), c(600, 1400))
  datos <- data.frame(
    tiene = tiene,
    marca = ifelse(tiene == "Si", "Ford", NA),
    stringsAsFactors = FALSE
  )

  sin_declarar <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(
    sin_declarar$hallazgos$tipo_hallazgo == "faltantes" &
      sin_declarar$hallazgos$columna == "marca"
  ))

  declarado <- perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(marca = ~ tiene == "Si")
  )
  expect_false(any(
    declarado$hallazgos$tipo_hallazgo == "faltantes" &
      declarado$hallazgos$columna == "marca"
  ))

  fila <- declarado$columnas[declarado$columnas$columna == "marca", ]
  expect_equal(fila$n, 2000)
  expect_equal(fila$n_aplicables, 600)
  expect_equal(fila$n_no_aplica, 1400)
  expect_equal(fila$n_faltantes, 0)
  expect_equal(fila$prop_faltantes, 0)

  cobertura <- declarado$cobertura_diagnosticos
  fila_cob <- cobertura[cobertura$columna == "marca" &
                          cobertura$diagnostico == "faltantes", ]
  expect_equal(nrow(fila_cob), 1L)
  expect_match(fila_cob$motivo, "600 filas aplicables de 2000")
  expect_match(fila_cob$motivo, 'tiene == "Si"', fixed = TRUE)
})

test_that("una columna opcional no informa la ausencia como defecto", {
  datos <- data.frame(
    id = 1:100,
    valido_hasta = c(rep(NA, 40), 41:100)
  )
  sin_declarar <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(sin_declarar$hallazgos$columna == "valido_hasta" &
                    sin_declarar$hallazgos$tipo_hallazgo == "faltantes"))

  declarado <- perfilar(
    datos, analizar_dependencias = FALSE,
    columnas_opcionales = "valido_hasta"
  )
  expect_false(any(declarado$hallazgos$columna == "valido_hasta" &
                     declarado$hallazgos$tipo_hallazgo == "faltantes"))
  fila <- declarado$columnas[declarado$columnas$columna == "valido_hasta", ]
  expect_equal(fila$n_aplicables, 60)
  expect_equal(fila$n_no_aplica, 40)
  expect_true(any(declarado$cobertura_diagnosticos$columna == "valido_hasta"))
})

test_that("un valor fuera del universo declarado es un hallazgo con su traza", {
  datos <- data.frame(
    tiene = c("Si", "No", "No", "Si"),
    marca = c("Ford", NA, "Fiat", "VW"),
    stringsAsFactors = FALSE
  )
  expect_no_warning(
    p <- perfilar(
      datos, analizar_dependencias = FALSE,
      aplicabilidad = list(marca = ~ tiene == "Si")
    )
  )
  h <- p$hallazgos[p$hallazgos$tipo_hallazgo == "valor_fuera_de_aplicabilidad", ]
  expect_equal(nrow(h), 1L)
  expect_equal(h$n_afectados, 1)
  expect_equal(h$trazabilidad[[1L]]$indices_fila, 3L)
})

test_that("una regla indeterminada se declara y no se cuenta como aplicable", {
  datos <- data.frame(
    tiene = c("Si", "No", NA, "Si"),
    marca = c("Ford", NA, NA, "VW"),
    stringsAsFactors = FALSE
  )
  p <- perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(marca = ~ tiene == "Si")
  )
  fila <- p$columnas[p$columnas$columna == "marca", ]
  expect_equal(fila$n_aplicables, 2)
  expect_equal(fila$n_no_aplica, 1)
  cobertura <- p$cobertura_diagnosticos
  motivo <- cobertura$motivo[cobertura$columna == "marca" &
                               cobertura$diagnostico == "faltantes"]
  expect_match(motivo, "1 filas la regla no se pudo determinar")
})

test_that("la aplicabilidad se rechaza con mensaje claro cuando esta mal escrita", {
  datos <- data.frame(a = 1:3, b = 1:3)
  expect_error(
    perfilar(datos, aplicabilidad = list(inexistente = ~ a > 1)),
    "columnas inexistentes"
  )
  expect_error(
    perfilar(datos, aplicabilidad = list(b = "a > 1")),
    "formula de un solo lado"
  )
  expect_error(
    perfilar(datos, aplicabilidad = list(b = ~ a)),
    "logico"
  )
  expect_error(
    perfilar(datos, columnas_opcionales = "b", aplicabilidad = list(b = ~ a > 1)),
    "a la vez"
  )
  expect_error(
    perfilar(datos, columnas_opcionales = "nada"),
    "columnas inexistentes"
  )
})

test_that("la metrica NoNulo acepta el mismo universo declarado", {
  tiene <- rep(c("Si", "No"), c(300, 700))
  datos <- data.frame(
    tiene = tiene,
    marca = ifelse(tiene == "Si", "Ford", NA),
    stringsAsFactors = FALSE
  )
  nucleo <- metricas_nucleo()
  sin_declarar <- instanciar(especializar(nucleo$NoNulo), "datos", "marca")
  declarado <- instanciar(
    especializar(nucleo$NoNulo, aplicable = ~ tiene == "Si"), "datos", "marca"
  )
  m1 <- medir(modelo(sin_declarar), list(datos = datos), id_medicion = "a")
  m2 <- medir(modelo(declarado), list(datos = datos), id_medicion = "b")

  expect_equal(nrow(m1), 1000L)
  expect_equal(mean(m1$resultado), 0.3)
  expect_equal(nrow(m2), 300L)
  expect_equal(mean(m2$resultado), 1)
})

test_that("la medicion conserva filas originales despues de aplicar una regla", {
  nucleo <- metricas_nucleo()
  metrica <- instanciar(especializar(nucleo$NoNulo), "tabla", "dato")
  medicion <- medir(
    modelo(metrica),
    data.frame(tipo = c("Si", "No", "Si"), dato = c("A", NA, "B")),
    aplicabilidad = list(dato = ~ tipo == "Si"), id_medicion = "traza"
  )
  expect_equal(medicion$fila, c(1L, 3L))
  expect_equal(medicion$objeto_medible, c("tabla$dato[1]", "tabla$dato[3]"))
})

test_that("perfilar_por perfila cada grupo y declara lo que descarta", {
  set.seed(9)
  n <- 900
  atributo <- rep(c("pais", "edad", "correo"), each = n / 3)
  largo <- data.frame(
    entidad = rep(seq_len(n / 3), 3),
    atributo = atributo,
    valor_texto = ifelse(
      atributo == "edad", NA,
      ifelse(atributo == "pais", sample(c("UY", "uy", "AR"), n, TRUE),
             paste0("a", seq_len(n), "@x.uy"))
    ),
    valor_num = ifelse(atributo == "edad", sample(18:80, n, TRUE), NA),
    stringsAsFactors = FALSE
  )

  plano <- perfilar(largo, analizar_dependencias = FALSE)
  expect_true(any(plano$hallazgos$tipo_hallazgo == "faltantes"))

  por_grupo <- perfilar_por(
    largo, "atributo", clave = "entidad", min_filas = 30,
    analizar_dependencias = FALSE
  )
  expect_s3_class(por_grupo, "hallazgos_por_grupo")
  expect_equal(attr(por_grupo, "n_grupos"), 3L)
  expect_equal(sum(por_grupo$tipo_hallazgo == "faltantes"), 0L)
  expect_true(all(c("grupo", "n_filas_grupo") %in% names(por_grupo)))
  expect_setequal(unique(por_grupo$grupo), c("pais", "edad", "correo"))

  cobertura <- attr(por_grupo, "cobertura_grupos")
  expect_true(all(c("grupo", "motivo", "columnas_descartadas") %in% names(cobertura)))
  expect_true(any(grepl("enteramente ausentes", cobertura$motivo)))
})

test_that("perfilar_por declara los grupos que no alcanzan el minimo", {
  datos <- data.frame(
    g = c(rep("grande", 50), rep("chico", 3)),
    v = c(sample(1:100, 50, TRUE), 1:3)
  )
  h <- perfilar_por(datos, "g", min_filas = 10, analizar_dependencias = FALSE)
  cobertura <- attr(h, "cobertura_grupos")
  expect_true(any(cobertura$grupo == "chico"))
  expect_match(
    cobertura$motivo[cobertura$grupo == "chico"], "no se perfilo"
  )
  expect_false("chico" %in% h$grupo)
})

test_that("perfilar_por rechaza argumentos mal escritos", {
  datos <- data.frame(g = c("a", "b"), v = 1:2)
  expect_error(perfilar_por(datos, "inexistente"), "columna inexistente")
  expect_error(perfilar_por(datos, c("g", "v")), "una sola columna")
  expect_error(perfilar_por(datos, "g", clave = "nada"), "columnas inexistentes")
  expect_error(perfilar_por(datos, "g", min_filas = 0), "entero positivo")
})

test_that("el universo declarado es el denominador de los diagnosticos por fila", {
  set.seed(5)
  n <- 1000
  tiene <- rep(c("Si", "No"), c(300, 700))
  marca <- rep(NA_character_, n)
  marca[seq_len(300)] <- c(
    rep(c("Ford", "FORD", "Fiat", "fiat", "VW"), 59),
    "Ford ", "Chevrolet", "Chevrolet", "Ford", "Fiat"
  )
  datos <- data.frame(tiene = tiene, marca = marca, stringsAsFactors = FALSE)

  sin_declarar <- perfilar(datos, analizar_dependencias = FALSE)
  declarado <- perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(marca = ~ tiene == "Si")
  )
  de <- function(perfil, tipo) {
    perfil$hallazgos[
      perfil$hallazgos$columna == "marca" &
        perfil$hallazgos$tipo_hallazgo == tipo, , drop = FALSE
    ]
  }

  ## Unidad `fila`: el denominador sigue al universo declarado. Informar
  ## "1 de 1000" sobre una columna cuyo universo son 300 filas contradice la
  ## propia declaracion.
  expect_equal(de(sin_declarar, "espacios_sobrantes")$n_evaluados, 1000)
  expect_equal(de(declarado, "espacios_sobrantes")$n_evaluados, 300)
  expect_equal(de(declarado, "espacios_sobrantes")$n_afectados, 1)

  ## Unidad `valor_distinto`: no cambia, y esta bien que no cambie. Opera sobre
  ## los valores presentes, que son todos aplicables por construccion.
  for (tipo in c("mayusculas_inconsistentes", "casi_duplicados_vocabulario")) {
    expect_equal(
      de(declarado, tipo)$n_evaluados, de(sin_declarar, tipo)$n_evaluados
    )
    expect_equal(
      de(declarado, tipo)$n_afectados, de(sin_declarar, tipo)$n_afectados
    )
  }
})

test_that("el universo aplicable tambien saca las filas del analisis", {
  ## Lo encontro una refutacion adversarial. Con filas no aplicables que tienen
  ## VALOR (no ausente), el conteo de distintos las incluia mientras el de
  ## validos ya no, y `tasa_distintos` podia pasar de 1, que es imposible.
  datos <- data.frame(
    flag = c(rep("Si", 5), rep("No", 10)),
    valor = c(LETTERS[1:5], rep("XX", 10)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(valor = ~ flag == "Si")
  )
  fila <- perfil$columnas[perfil$columnas$columna == "valor", ]

  expect_equal(fila$n_aplicables, 5)
  expect_equal(fila$n_distintos, 5L)
  expect_false(identical(fila$moda, "XX"))
  expect_equal(fila$frecuencia_moda, 1)
  expect_lte(fila$tasa_distintos, 1)
})

test_that("la traza no nombra filas fuera del universo declarado", {
  ## La guarda de coherencia detectaba esto y tenia razon: el conteo excluia las
  ## filas no aplicables y la trazabilidad las nombraba igual.
  datos <- data.frame(
    tiene = c(rep("Si", 10), rep("No", 10)),
    cant = c(1:5, rep(NA_integer_, 5), rep(NA_integer_, 10))
  )
  expect_no_warning(
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      aplicabilidad = list(cant = ~ tiene == "Si")
    )
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "faltantes", , drop = FALSE
  ]
  expect_equal(hallazgo$n_afectados, 5)
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, 6:10)
})
