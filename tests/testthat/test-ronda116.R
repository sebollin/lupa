# Las once senales falsas de la corrida contra tres tablas administrativas
# reales. En las once el calculo estaba bien; lo que fallaba era el juicio de si
# la prueba corresponde. Cada prueba de aqui fija las dos mitades: que la senal
# falsa deje de emitirse Y que quede declarada, porque bajar el ruido callando
# seria mejorar el numero sin mejorar el paquete.

.perfil_r116 <- function(datos) {
  perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
}

.tipos_r116 <- function(perfil, columna) {
  as.character(perfil$hallazgos$tipo_hallazgo[
    perfil$hallazgos$columna == columna &
      as.character(perfil$hallazgos$severidad) != "ok"
  ])
}

.motivo_r116 <- function(perfil, columna, diagnostico) {
  cobertura <- perfil$cobertura_diagnosticos
  as.character(cobertura$motivo[
    cobertura$columna == columna & cobertura$diagnostico == diagnostico
  ])
}

test_that("Benford no se corre sobre una numeracion con huecos", {
  # El caso real: 3.159 filas numeradas entre 1 y 4557, con los huecos de las
  # bajas. La guarda anterior exigia una corrida sin huecos y este `MotId` no la
  # pasaba, asi que Benford se corria sobre un identificador.
  set.seed(4)
  identificadores <- sort(sample(seq_len(4557L), 3159L))
  perfil <- .perfil_r116(data.frame(MotId = identificadores))

  expect_false("desviacion_benford" %in% .tipos_r116(perfil, "MotId"))
  motivo <- .motivo_r116(perfil, "MotId", "ley_benford")
  expect_length(motivo, 1L)
  expect_match(motivo, "identificador")
})

test_that("Benford sigue corriendo sobre magnitudes casi unicas", {
  # La unicidad no distingue: un monto tambien es casi unico. Si la guarda
  # mirara la unicidad en vez de la densidad, este caso se apagaria.
  set.seed(4)
  montos <- c(
    sample(c(9:99, 900:999, 90000:99999, 9000000:9999999), 500L, TRUE),
    sample(c(10:19, 1000:1099), 30L, TRUE)
  )
  perfil <- .perfil_r116(data.frame(monto = as.numeric(montos)))

  expect_gt(perfil$columnas$tasa_distintos, 0.9)
  # La densidad se mide sobre el vector, no sobre el campo del perfil, que solo
  # se llena para columnas enteras. Es la comparacion que importa: casi unico
  # como `MotId`, y a la vez a cuatro ordenes de distancia de su densidad.
  expect_false(.parece_correlativo_benford(as.numeric(montos)))
  expect_true("desviacion_benford" %in% .tipos_r116(perfil, "monto"))
})

test_that("los limites de Tukey no se evaluan sobre un codigo, y se declara", {
  # Un codigo 1..284 cuyas filas se concentran en los bajos: Tukey marca la
  # cola entera. Son 179 valores "extremos" que no dicen nada de la calidad.
  set.seed(9)
  codigos <- c(sample(seq_len(60L), 1900L, TRUE), sample(61:284, 224L, TRUE))
  perfil <- .perfil_r116(data.frame(EstCod = codigos))

  expect_gt(perfil$columnas$n_outliers, 0L)
  expect_false("outliers" %in% .tipos_r116(perfil, "EstCod"))

  motivo <- .motivo_r116(perfil, "EstCod", "outliers")
  expect_length(motivo, 1L)
  # El motivo declara el criterio que se aplico y cuanto se dejo de decir. Un
  # motivo que describe otro criterio es la misma falta que no declarar nada.
  expect_match(motivo, "de los enteros entre su minimo y su maximo")
  expect_match(motivo, as.character(perfil$columnas$n_outliers))
})

test_that("un valor fuera de escala rompe la densidad y se sigue senalando", {
  # Es el caso que hay que ver: un 10000 entre identificadores de 1 a 100. La
  # guarda no puede taparlo, y no lo tapa porque ese valor destruye la
  # compacidad que la define.
  perfil <- perfilar(
    data.frame(id = as.character(c(seq_len(100L), 10000L))),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  expect_lt(perfil$columnas$densidad_secuencia_entera, 0.5)
  expect_true("outliers" %in% .tipos_r116(perfil, "id"))
})

test_that("un anio centinela sigue senalandose entre anios validos", {
  set.seed(11)
  anios <- c(sample(2000:2030, 200L, TRUE), 1900L, 1900L)
  perfil <- .perfil_r116(data.frame(ProAnio = anios))

  expect_true("outliers" %in% .tipos_r116(perfil, "ProAnio"))
})

test_that("la cardinalidad de un texto libre no es un defecto", {
  # Un nombre, una descripcion o un objetivo tienen cardinalidad alta por
  # definicion. Eran tres de los once falsos.
  descripciones <- paste(
    "Descripcion extensa del expediente numero", seq_len(1500L),
    "con redaccion administrativa que no se normaliza"
  )
  set.seed(3)
  columna <- c(descripciones, sample(descripciones, 500L, TRUE))
  perfil <- .perfil_r116(data.frame(descripcion = columna))

  expect_gt(perfil$columnas$longitud_media, 40)
  expect_lt(perfil$columnas$tasa_distintos, 1)
  expect_false("alta_cardinalidad" %in% .tipos_r116(perfil, "descripcion"))

  motivo <- .motivo_r116(perfil, "descripcion", "alta_cardinalidad")
  expect_length(motivo, 1L)
  expect_match(motivo, "texto libre")
})

test_that("una categoria de valores cortos sigue midiendo su cardinalidad", {
  # 60 categorias sobre 100 filas: cardinalidad alta para una categoria, y
  # lejos de la unicidad que la volveria un identificador.
  set.seed(5)
  catalogo <- paste0("C", sprintf("%03d", seq_len(60L)))
  codigos <- c(catalogo, sample(catalogo, 40L, replace = TRUE))
  perfil <- .perfil_r116(data.frame(codigo = codigos))

  expect_lt(perfil$columnas$longitud_media, 40)
  expect_true("alta_cardinalidad" %in% .tipos_r116(perfil, "codigo"))
})

test_that("el orden entre dos numeraciones no se propone como regla", {
  # `MotId <= MEsId` se cumplia en el 99,1 % de las filas porque los dos
  # contadores avanzan juntos. El 0,9 % restante violaba una regla que no existe.
  set.seed(4)
  perfil <- .perfil_r116(data.frame(
    MotId = sort(sample(seq_len(4557L), 3159L)),
    MEsId = sort(sample(seq_len(4600L), 3159L))
  ))

  expect_false(
    "relacion_orden_columnas" %in% as.character(perfil$hallazgos$tipo_hallazgo)
  )
  alcance <- perfil$meta$orden_columnas
  expect_equal(alcance$pares_descartados_identificador, 1)
  expect_equal(alcance$pares_identificador_descartados, "MotId,MEsId")
})

test_that("una brecha estable conserva la relacion aunque ambas sean densas", {
  # `inicio`/`fin` son dos columnas enteras, unicas y densas, igual que el par
  # de identificadores. Lo que las separa es que la brecha es constante.
  inicio <- seq_len(100L)
  fin <- inicio + 10L
  fin[seq_len(3L)] <- inicio[seq_len(3L)] - 1L
  perfil <- .perfil_r116(data.frame(inicio = inicio, fin = fin))

  expect_true(
    "relacion_orden_columnas" %in% as.character(perfil$hallazgos$tipo_hallazgo)
  )
  expect_equal(perfil$meta$orden_columnas$pares_descartados_identificador, 0)
})

test_that("`constante` no se afirma desde una muestra", {
  # Una tabla de 200 filas con tres valores, muestreada en 50 que traen uno
  # solo, informaba "la columna contiene un unico valor". Una proporcion
  # estimada sobre una muestra sigue siendo honesta; una cuantificacion
  # universal no: basta una fila no leida para desmentirla.
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = seq_len(200L),
    estado = c(rep("A", 50L), rep(c("B", "C"), each = 75L)),
    stringsAsFactors = FALSE
  ))

  resultado <- perfilar_dbi(conexion, "t", muestra = 50, orden_muestra = "id")
  muestreo <- resultado$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(muestreo$tabla_completa)

  hallazgos <- resultado$perfil_muestra$hallazgos
  expect_false("constante" %in% as.character(hallazgos$tipo_hallazgo))

  cobertura <- resultado$perfil_muestra$cobertura_diagnosticos
  fila <- cobertura$diagnostico == "constante" & cobertura$columna == "estado"
  expect_true(any(fila))
  # El motivo trae las dos cifras: no alcanza con decir que no se evaluo.
  expect_match(as.character(cobertura$motivo[fila])[[1L]], "50 filas de las 200")
})

test_that("`constante` se sigue afirmando cuando la muestra es la tabla", {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    id = seq_len(200L), estado = rep("A", 200L), stringsAsFactors = FALSE
  ))

  resultado <- perfilar_dbi(conexion, "t", muestra = 500, orden_muestra = "id")
  expect_true(resultado$perfil_muestra$meta$origen_dbi$muestreo$tabla_completa)
  expect_true(
    "constante" %in%
      as.character(resultado$perfil_muestra$hallazgos$tipo_hallazgo)
  )
})

test_that("el plan se imprime como rango y el subconjunto avisa", {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    x = as.numeric(seq_len(50L)), y = letters[seq_len(50L) %% 26L + 1L],
    stringsAsFactors = FALSE
  ))
  plan <- plan_perfilado_dbi(conexion, "t", modo = "exacto")

  # cli escribe por el flujo de mensajes, no por la salida estandar: capturar
  # el flujo equivocado haria pasar la prueba sin mirar nada.
  encabezado <- capture.output(print(plan), type = "message")
  expect_true(length(encabezado) > 0L)
  # El atributo `supuesto` declaraba un rango y el encabezado decia "techo".
  expect_false(any(grepl("techo", encabezado, fixed = TRUE)))
  expect_true(any(grepl("consultas", encabezado, fixed = TRUE)))

  # Subconjuntar conserva la clase y pierde los atributos: el metodo imprimia
  # "sin dato consultas como techo sobre sin dato filas".
  recorte <- plan[, c("clase_consulta", "n_consultas")]
  aviso <- capture.output(print(recorte), type = "message")
  tabla <- capture.output(print(recorte))
  expect_false(any(grepl("sin dato", c(aviso, tabla), fixed = TRUE)))
  expect_true(any(grepl("subconjunto", aviso, fixed = TRUE)))
  # Y la tabla se imprime igual: el aviso no puede costarle el contenido.
  expect_true(any(grepl("clase_consulta", tabla, fixed = TRUE)))
})
