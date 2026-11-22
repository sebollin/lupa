# Cinco lecturas que el perfil hacia mal en los bordes: un desvio que no cabe en
# un doble, un centinela entero por encima de 2^53, un codigo de moneda separado
# del numero por un espacio duro, un numero con la unidad pegada leido como
# identificador y el espacio duro de un importe acusado como invisible.
testthat::skip_on_cran()

.hallazgos_de <- function(perfil, columna, tipo) {
  h <- perfil$hallazgos
  h[h$columna %in% columna & h$tipo_hallazgo == tipo, , drop = FALSE]
}

.importes_con_espacio <- function(espacio) {
  set.seed(11)
  v <- sample(c(35.5, 12.25, 99, 1500.75, 42), 80, TRUE)
  data.frame(
    delante = paste0("USD", espacio, v),
    detras = paste0(v, espacio, "USD"),
    simbolo = paste0("$", espacio, v),
    stringsAsFactors = FALSE
  )
}

test_that("un desvio que no cabe en un doble se publica como NA, no como Inf", {
  datos <- data.frame(v = c(1.7e308, -1.7e308))
  perfil <- perfilar(datos)
  expect_true(is.na(perfil$columnas$desvio))
  expect_false(is.infinite(perfil$columnas$desvio))
  # Con mas valores el desvio si cabe y se sigue publicando.
  mas <- perfilar(data.frame(v = rep(c(1.7e308, -1.7e308), 11L)))
  expect_true(is.finite(mas$columnas$desvio))
  # Un infinito de verdad sigue declarado aparte y no cambia el desvio.
  con_inf <- perfilar(data.frame(v = c(1, 2, 3, Inf)))
  expect_equal(con_inf$columnas$desvio, 1)
  expect_equal(con_inf$columnas$n_infinito_positivo, 1L)

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "extremos", datos)
  dbi <- perfilar_dbi(con, "extremos")
  expect_true(is.na(dbi$resumen_tabla$columnas$desvio))
  expect_true(is.na(dbi$perfil_muestra$columnas$desvio))
})

test_that("un centinela entero declarado no arrastra al vecino de una unidad en integer64", {
  skip_if_not_installed("bit64")
  set.seed(2)
  ordinarios <- as.character(sample(1:1e6, 100L))
  centinela <- "9007199254740992"
  vecino <- "9007199254740993"
  perfil <- perfilar(
    data.frame(v = bit64::as.integer64(
      c(rep(centinela, 20L), rep(vecino, 15L), ordinarios)
    )),
    sentinelas_numericos = 9007199254740992
  )
  disfrazados <- .hallazgos_de(perfil, "v", "faltantes_disfrazados")
  expect_equal(nrow(disfrazados), 1L)
  expect_equal(disfrazados$n_afectados, 20)
  expect_match(disfrazados$evidencia, "9007199254740992 (20)", fixed = TRUE)
  expect_false(grepl("(35)", disfrazados$evidencia, fixed = TRUE))
  # Un centinela comun sigue contandose entero.
  comun <- perfilar(
    data.frame(v = bit64::as.integer64(c(rep("-999", 20L), ordinarios))),
    sentinelas_numericos = -999
  )
  expect_equal(.hallazgos_de(comun, "v", "faltantes_disfrazados")$n_afectados, 20)
})

test_that("el codigo de moneda acepta el espacio duro y el fino como el simbolo", {
  for (espacio in c(" ", intToUtf8(0x00a0), intToUtf8(0x202f))) {
    datos <- .importes_con_espacio(espacio)
    perfil <- perfilar(datos)
    columnas <- perfil$columnas
    expect_equal(columnas$n_numeros_texto, c(80L, 80L, 80L), info = espacio)
    expect_equal(columnas$numero_texto_moneda, c("USD", "USD", "$"), info = espacio)
    expect_true(all(perfil$hallazgos$tipo_hallazgo[
      perfil$hallazgos$columna %in% c("delante", "detras", "simbolo")
    ] != "controles_invisibles"), info = espacio)
  }
  # Un codigo pegado al numero sigue siendo un codigo, no una moneda.
  pegado <- perfilar(data.frame(v = rep(c("35.50USD", "12.25USD"), 40L)))
  expect_equal(pegado$columnas$n_numeros_texto, 0L)
})

test_that("un numero con la unidad pegada no es un identificador", {
  set.seed(1)
  distintos <- round(runif(80L, 10, 5000), 2)
  datos <- data.frame(
    pegada = paste0(sprintf("%.2f", distintos), "ml"),
    separada = paste0(sprintf("%.2f", distintos), " ml"),
    codigo = paste0("AB", 1000L + 1:80),
    ceros = sprintf("%06d", 1:80),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  tipos <- stats::setNames(perfil$columnas$tipo_inferido, perfil$columnas$columna)
  expect_identical(tipos[["pegada"]], "texto")
  expect_identical(tipos[["separada"]], "texto")
  expect_identical(tipos[["codigo"]], "identificador")
  expect_identical(tipos[["ceros"]], "identificador")
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[plan$columna == "pegada" &
    plan$estrategia == "convertir_numero_regional", ]
  # Pegada, la conversion se ofrece pero no se marca: puede ser un codigo.
  expect_identical(nrow(conversion), 1L)
  expect_false(conversion$aplicar)
})

test_that("el espacio duro de un importe no es un invisible, el de un texto libre si", {
  duro <- intToUtf8(0x00a0)
  datos <- .importes_con_espacio(duro)
  datos$libre <- rep(c(paste0("Ana", duro, "Perez"), "Ana Perez"), 40L)
  datos$borde <- paste0("$ 35", duro, rep("", 80L))
  datos$mezcla <- paste0("$", duro, "35", intToUtf8(0x200b))
  perfil <- perfilar(datos)
  invisibles <- function(columna) nrow(.hallazgos_de(perfil, columna, "controles_invisibles"))
  expect_identical(invisibles("delante"), 0L)
  expect_identical(invisibles("detras"), 0L)
  expect_identical(invisibles("simbolo"), 0L)
  expect_identical(invisibles("libre"), 1L)
  expect_identical(invisibles("borde"), 1L)
  expect_identical(invisibles("mezcla"), 1L)
  expect_identical(nrow(.hallazgos_de(perfil, "borde", "espacios_sobrantes")), 1L)
  expect_identical(
    perfil$columnas$n_controles_invisibles[perfil$columnas$columna == "simbolo"], 0L
  )
})

# Lo que el plan anuncia y lo que `aplicar()` escribe tienen que coincidir.
.aplicar_accion <- function(perfil, datos, columna, estrategia) {
  plan <- planificar_limpieza(perfil, datos)
  fila <- plan$columna == columna & plan$estrategia == estrategia
  expect_equal(sum(fila), 1L)
  anunciadas <- plan$n_afectadas[fila]
  plan$aplicar[fila] <- TRUE
  # Sobre una columna integer64, aplicar() avisa de una conversion a doble que
  # no cambia lo escrito (los vecinos conservan su valor exacto, que se mide).
  resultado <- suppressWarnings(aplicar(plan, datos))
  registro <- resultado$registro
  list(
    datos = resultado$datos, anunciadas = anunciadas,
    cambiadas = registro$n_cambiadas[registro$columna == columna &
      registro$estrategia == estrategia]
  )
}

test_that("el centinela integer64 que cuenta el perfil es el que borra aplicar()", {
  skip_if_not_installed("bit64")
  set.seed(2)
  ordinarios <- as.character(sample(1:1e6, 100L))
  datos <- data.frame(v = bit64::as.integer64(
    c(rep("9007199254740992", 20L), rep("9007199254740993", 15L), ordinarios)
  ))
  perfil <- perfilar(datos, sentinelas_numericos = 9007199254740992)
  hecho <- .aplicar_accion(perfil, datos, "v", "convertir_sentinelas_numericos")
  expect_equal(hecho$anunciadas, 20)
  expect_equal(hecho$cambiadas, 20)
  restantes <- bit64::as.character.integer64(hecho$datos$v)
  expect_equal(sum(is.na(restantes)), 20L)
  expect_equal(sum(restantes %in% "9007199254740993", na.rm = TRUE), 15L)
})

test_that("las versiones y secciones con letra no se convierten ni se leen como unidad", {
  n <- 80L
  datos <- data.frame(
    version = sprintf("%d.0beta", 1:n), seccion = sprintf("%d.01a", 1:n),
    espaciada = sprintf("%d.0 beta", 1:n), kilos = sprintf("%d kg", 1:n),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  tipos <- stats::setNames(perfil$columnas$tipo_inferido, perfil$columnas$columna)
  expect_identical(tipos[["version"]], "identificador")
  expect_identical(tipos[["seccion"]], "identificador")
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
  marcada <- stats::setNames(conversion$aplicar, conversion$columna)
  expect_false(marcada[["version"]])
  expect_false(marcada[["seccion"]])
  expect_false(marcada[["espaciada"]])
  expect_true(marcada[["kilos"]])
  # Por omision, `aplicar()` deja el dato como estaba.
  aplicado <- aplicar(plan, datos)$datos
  expect_identical(aplicado$version, datos$version)
  expect_identical(aplicado$espaciada, datos$espaciada)
})

test_that("una unidad pegada al numero se lee como numero pero no se convierte por omision", {
  n <- 80L
  datos <- data.frame(
    metros = sprintf("%d.5m", 1:n), litros = sprintf("%d.0l", 1:n),
    vatios = sprintf("%d.5w", 1:n), gramos = sprintf("%d.5g", 1:n),
    kilos = sprintf("%d.25kg", 1:n), coordenada = sprintf("%d.5s", 1:n),
    entera = sprintf("%dm", 1:n), porcentaje = sprintf("%d.5%%", 1:n),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  fila <- function(columna) perfil$columnas[perfil$columnas$columna == columna, ]
  tipos <- stats::setNames(perfil$columnas$tipo_inferido, perfil$columnas$columna)
  for (columna in c("metros", "litros", "vatios", "gramos", "kilos", "coordenada")) {
    expect_identical(tipos[[columna]], "texto", info = columna)
    expect_identical(fila(columna)$n_numeros_texto, n, info = columna)
  }
  expect_identical(tipos[["entera"]], "identificador")
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
  marcada <- stats::setNames(conversion$aplicar, conversion$columna)
  for (columna in c("metros", "litros", "vatios", "gramos", "kilos", "coordenada")) {
    expect_true(columna %in% names(marcada), info = columna)
    expect_false(marcada[[columna]], info = columna)
  }
  expect_false(marcada[["entera"]])
  # El porcentaje pegado es un porcentaje: se sigue ofreciendo marcado.
  expect_true(marcada[["porcentaje"]])
  aplicado <- aplicar(plan, datos)$datos
  for (columna in c("metros", "litros", "vatios", "gramos", "kilos", "coordenada", "entera")) {
    expect_identical(aplicado[[columna]], datos[[columna]], info = columna)
  }
  nota <- conversion$justificacion[conversion$columna == "metros"]
  expect_match(nota, "c\u00f3digo o una versi\u00f3n", fixed = TRUE)
})

test_that("con la unidad separada, las unidades comunes se siguen convirtiendo por omision", {
  n <- 60L
  unidades <- c("kcal", "cc", "oz", "lb", "mAh", "ha", "s", "h", "kg", "ml", "Kg", "%")
  datos <- as.data.frame(
    stats::setNames(lapply(unidades, function(u) sprintf("%d.25 %s", 1:n, u)),
                    paste0("u_", seq_along(unidades))),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
  expect_identical(nrow(conversion), length(unidades))
  expect_true(all(conversion$aplicar))
  # Una palabra que no es una unidad no se convierte sola.
  beta <- data.frame(v = sprintf("%d.25 puntos", 1:n), stringsAsFactors = FALSE)
  plan_beta <- planificar_limpieza(perfilar(beta), beta)
  expect_false(any(plan_beta$aplicar[plan_beta$estrategia == "convertir_numero_regional"]))
})

test_that("dos espacios invisibles dentro de un importe se siguen acusando", {
  duro <- intToUtf8(0x00a0)
  base <- rep("N/A", 60L)
  for (celda in c(
    paste0("$", duro, duro, "35"), paste0("$", duro, intToUtf8(0x202f), "35"),
    paste0("$", duro, " 35"), paste0("USD", duro, duro, "35")
  )) {
    perfil <- perfilar(data.frame(v = c(base, celda), stringsAsFactors = FALSE))
    expect_identical(perfil$columnas$n_espacios_invisibles, 1L, info = celda)
    expect_identical(
      nrow(.hallazgos_de(perfil, "v", "controles_invisibles")), 1L, info = celda
    )
  }
  uno <- perfilar(data.frame(v = c(base, paste0("$", duro, "35")), stringsAsFactors = FALSE))
  expect_identical(uno$columnas$n_espacios_invisibles, 0L)
})

test_that("en una columna mezclada el plan cuenta lo que aplicar() cambia", {
  duro <- intToUtf8(0x00a0)
  datos <- data.frame(
    v = c(paste0("$", duro, 1:40), rep(paste0("Ana", duro, "Perez"), 40L)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  hecho <- .aplicar_accion(perfil, datos, "v", "normalizar_espacios_invisibles")
  expect_equal(hecho$anunciadas, 40)
  expect_equal(hecho$cambiadas, 40)
  expect_equal(sum(hecho$datos$v != datos$v), 40L)
  expect_identical(hecho$datos$v[1:40], datos$v[1:40])
})

test_that("un importe con codigo de moneda y ceros a la izquierda se ofrece pero no se convierte solo", {
  duro <- intToUtf8(0x00a0)
  for (espacio in c(" ", duro)) {
    for (modelo in c("PEN%s%06d", "USD%s%04d", "UYU%s%06d")) {
      datos <- data.frame(v = sprintf(modelo, espacio, 1:80), stringsAsFactors = FALSE)
      perfil <- perfilar(datos)
      info <- paste(modelo, nchar(espacio))
      expect_identical(perfil$columnas$n_numeros_texto, 80L, info = info)
      plan <- planificar_limpieza(perfil, datos)
      conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
      expect_identical(nrow(conversion), 1L, info = info)
      expect_false(conversion$aplicar, info = info)
      expect_match(conversion$justificacion, "c\u00f3digo de documento", fixed = TRUE, info = info)
      sin_espacio_duro <- gsub(duro, " ", datos$v, fixed = TRUE)
      expect_identical(aplicar(plan, datos)$datos$v, sin_espacio_duro, info = info)
      # El espacio duro de ese codigo se sigue acusando y normalizando.
      if (identical(espacio, duro)) {
        expect_identical(nrow(.hallazgos_de(perfil, "v", "controles_invisibles")), 1L, info = info)
        expect_true(any(plan$estrategia == "normalizar_espacios_invisibles" & plan$aplicar), info = info)
      }
    }
  }
  # Con decimales ceros a la izquierda tambien son ambiguos; sin ceros es un importe.
  importes <- data.frame(v = sprintf("USD%s%d", duro, 1:80), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(importes), importes)
  expect_true(plan$aplicar[plan$estrategia == "convertir_numero_regional"])
})

test_that("un importe con dos espacios duros anuncia cada accion sobre sus celdas", {
  duro <- intToUtf8(0x00a0)
  datos <- data.frame(v = sprintf("USD%s%s%d.25", duro, duro, 1:20), stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  expect_equal(plan$n_afectadas[plan$estrategia == "normalizar_espacios_invisibles"], 20)
  expect_equal(plan$n_afectadas[plan$estrategia == "convertir_numero_regional"], 20)
  resultado <- aplicar(plan, datos)
  registro <- resultado$registro
  # Una celda que tocan dos acciones cuenta en cada una; el registro lo detalla.
  expect_equal(registro$n_cambiadas[registro$estrategia == "convertir_numero_regional"], 20)
  expect_equal(sum(resultado$datos$v != datos$v), 20L)
})

test_that("un centinela de texto de mas de 15 cifras no cuenta a su vecino", {
  set.seed(2)
  ordinarios <- as.character(sample(1:1e6, 100L))
  datos <- data.frame(
    v = c(rep("9007199254740992", 20L), rep("9007199254740993", 15L), ordinarios),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, sentinelas_numericos = 9007199254740992)
  disfrazados <- .hallazgos_de(perfil, "v", "faltantes_disfrazados")
  expect_equal(disfrazados$n_afectados, 20)
  hecho <- .aplicar_accion(perfil, datos, "v", "convertir_sentinelas_numericos")
  expect_equal(hecho$anunciadas, 20)
  expect_equal(hecho$cambiadas, 20)
  expect_equal(sum(hecho$datos$v %in% "9007199254740993"), 15L)
  # Los centinelas comunes escritos de varias formas se cuentan igual que antes.
  comun <- perfilar(data.frame(v = c(rep(c("-999", "-999.0", "-9.99e2"), 5L), ordinarios)),
                    sentinelas_numericos = -999)
  expect_equal(.hallazgos_de(comun, "v", "faltantes_disfrazados")$n_afectados, 15)
})

test_that("el desvio que no cabe en un doble se declara en la cobertura", {
  del_desvio <- function(perfil) {
    cobertura <- perfil$cobertura_diagnosticos
    cobertura[cobertura$diagnostico == "resumen_cuantitativo" &
      grepl("desv", cobertura$motivo), , drop = FALSE]
  }
  fila <- del_desvio(perfilar(data.frame(v = c(1.7e308, -1.7e308))))
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "no cabe", fixed = TRUE)
  # Un solo valor no tiene desvio, y eso no es una magnitud que no cabe.
  expect_equal(nrow(del_desvio(perfilar(data.frame(v = 5)))), 0L)
  expect_equal(nrow(del_desvio(perfilar(data.frame(v = c(1, 2, 3, 4))))), 0L)
})

test_that("una celda ambigua no desmarca la columna cuando la forma dominante no lo es", {
  n <- 119L
  conversion_de <- function(valores) {
    datos <- data.frame(v = valores, stringsAsFactors = FALSE)
    plan <- planificar_limpieza(perfilar(datos), datos)
    list(
      plan = plan[plan$estrategia == "convertir_numero_regional", ],
      aplicado = aplicar(plan, datos)$datos$v
    )
  }
  # 119 kg separados y uno pegado de la misma unidad: la unidad esta establecida.
  kilos <- conversion_de(c(sprintf("%d.25 kg", 1:n), "60.0kg"))
  expect_true(kilos$plan$aplicar)
  expect_identical(kilos$aplicado, c(1:n + 0.25, 60))
  # 119 importes y uno con ceros a la izquierda: se convierte como los demas.
  usd <- conversion_de(c(sprintf("USD %d.25", 1:n), "USD 000101"))
  expect_true(usd$plan$aplicar)
  expect_identical(usd$aplicado, c(1:n + 0.25, 101))
  simbolo <- conversion_de(c(sprintf("$ %d.25", 1:n), "$ 000101"))
  expect_true(simbolo$plan$aplicar)
  # Cuando lo ambiguo es la forma dominante, la columna queda sin marcar.
  pegadas <- conversion_de(c(sprintf("%d.25kg", 1:n), "60.0 kg"))
  expect_false(pegadas$plan$aplicar)
  expect_identical(pegadas$aplicado, c(sprintf("%d.25kg", 1:n), "60.0 kg"))
  mitad <- conversion_de(c(sprintf("%d.5 kg", 1:40), sprintf("%d.5kg", 41:80)))
  expect_true(mitad$plan$aplicar)
  expect_identical(mitad$aplicado, 1:80 + 0.5)
  ceros <- conversion_de(c(sprintf("USD %06d", 1:n), "USD 5.25"))
  expect_false(ceros$plan$aplicar)
  # Otra unidad en la columna: no se resuelve y la conversion queda sin marcar.
  otra <- conversion_de(c(sprintf("%d.25 kg", 1:n), "60.0 puntos"))
  expect_false(otra$plan$aplicar)
  otra_unidad <- conversion_de(c(sprintf("%d.25 kg", 1:n), "60.0m"))
  expect_false(otra_unidad$plan$aplicar)
})

test_that("los ceros a la izquierda con simbolo de moneda se ofrecen pero no se convierten solos", {
  euro <- intToUtf8(0x20ac)
  for (modelo in c("$ %06d", "U$S %06d", paste0(euro, " %06d"), "$%06d", paste0("%06d ", euro))) {
    datos <- data.frame(v = sprintf(modelo, 1:80), stringsAsFactors = FALSE)
    perfil <- perfilar(datos)
    plan <- planificar_limpieza(perfil, datos)
    conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
    expect_identical(nrow(conversion), 1L, info = modelo)
    expect_false(conversion$aplicar, info = modelo)
    expect_identical(aplicar(plan, datos)$datos$v, datos$v, info = modelo)
  }
  # Sin ceros, el importe con simbolo se sigue convirtiendo.
  importes <- data.frame(v = sprintf("$ %d.25", 1:80), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(importes), importes)
  expect_true(plan$aplicar[plan$estrategia == "convertir_numero_regional"])
})

test_that("un centinela de texto de mas de 15 cifras es exacto en cualquier notacion", {
  set.seed(2)
  ordinarios <- as.character(sample(1:1e6, 100L))
  for (vecino in c("9.007199254740993e15", "9007199254740993e0", "9007199254740991.5",
                   "-9.007199254740993e15", "+9.007199254740993e+15")) {
    datos <- data.frame(
      v = c(rep("9007199254740992", 20L), rep(vecino, 15L), ordinarios),
      stringsAsFactors = FALSE
    )
    perfil <- perfilar(datos, sentinelas_numericos = 9007199254740992)
    expect_equal(.hallazgos_de(perfil, "v", "faltantes_disfrazados")$n_afectados, 20, info = vecino)
    hecho <- .aplicar_accion(perfil, datos, "v", "convertir_sentinelas_numericos")
    expect_equal(hecho$anunciadas, 20, info = vecino)
    expect_equal(hecho$cambiadas, 20, info = vecino)
    expect_equal(sum(hecho$datos$v %in% vecino), 15L, info = vecino)
    por <- perfilar_por(
      cbind(datos, g = "a"), "g", sentinelas_numericos = 9007199254740992, min_filas = 10
    )
    expect_equal(por$n_afectados[por$tipo_hallazgo == "faltantes_disfrazados"], 20, info = vecino)
  }
  # La misma cifra escrita con ceros de relleno o en notacion es el centinela.
  mismas <- perfilar(
    data.frame(v = c("9007199254740992.0", "9.007199254740992e15", "9007199254740992", ordinarios)),
    sentinelas_numericos = 9007199254740992
  )
  expect_equal(.hallazgos_de(mismas, "v", "faltantes_disfrazados")$n_afectados, 3)
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    v = c(rep("9007199254740992", 20L), rep("9.007199254740993e15", 15L), ordinarios)
  ))
  dbi <- suppressWarnings(perfilar_dbi(con, "t", sentinelas_numericos = 9007199254740992))
  h <- dbi$perfil_muestra$hallazgos
  expect_equal(h$n_afectados[h$tipo_hallazgo == "faltantes_disfrazados"], 20)
})

test_that("las unidades comunes con espacio se convierten por omision y una letra ambigua no", {
  n <- 40L
  unidades <- c("in", "ft", "yd", "mi", "t", "Pa", "kPa", "j", "kj", "v", "ohm", "qt",
                "cup", "gal", "pt", "tbsp", "mph")
  datos <- as.data.frame(
    stats::setNames(lapply(unidades, function(u) sprintf("%d.5 %s", 1:n, u)), unidades),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  plan <- planificar_limpieza(perfilar(datos), datos)
  conversion <- plan[plan$estrategia == "convertir_numero_regional", ]
  expect_identical(nrow(conversion), length(unidades))
  expect_true(all(conversion$aplicar))
  # Una letra suelta muy ambigua no es unidad conocida.
  ambigua <- data.frame(v = sprintf("%d.5 a", 1:n), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(ambigua), ambigua)
  expect_false(any(plan$aplicar[plan$estrategia == "convertir_numero_regional"]))
})
