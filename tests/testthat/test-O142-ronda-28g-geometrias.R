# Ronda 28-G: lo que la refutacion de geometrias encontro. Una prueba por
# hallazgo; cada cifra se rehace a mano en la propia prueba.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

avisos_r28g <- function(expr) {
  recogidos <- character()
  valor <- withCallingHandlers(
    expr,
    warning = function(w) {
      recogidos <<- c(recogidos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, avisos = unique(recogidos))
}

fila_geom_r28g <- function(perfil, columna = "geom") {
  perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
}

hallazgo_r28g <- function(perfil, tipo, columna = "geom") {
  todos <- hallazgos(perfil)
  todos[todos$tipo_hallazgo == tipo & todos$columna %in% columna, , drop = FALSE]
}

cobertura_r28g <- function(perfil, diagnostico, columna = "geom") {
  cobertura <- perfil$cobertura_diagnosticos
  cobertura[cobertura$diagnostico == diagnostico &
              cobertura$columna %in% columna, , drop = FALSE]
}

mono_r28g <- function() {
  sf::st_polygon(list(rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0))))
}

cuadrado_r28g <- function() {
  sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
}

test_that("C: con universo, los evaluados cuentan solo lo que se evaluo", {
  skip_if_not_installed("sf")
  # Universo: filas 1 a 6, mas que el tope de 3 geometrias. El presupuesto
  # muestrea dentro del universo las posiciones
  # unique(round(seq(1, 6, length.out = 3))) = 1, 4, 6. Monos en 4 (analizada) y
  # en 2 (no); lon 250 en 5 (no analizada).
  geoms <- list(
    cuadrado_r28g(), mono_r28g(), cuadrado_r28g(), mono_r28g(),
    sf::st_point(c(250, 1)), cuadrado_r28g(), cuadrado_r28g(), cuadrado_r28g()
  )
  datos <- data.frame(tipo = c(rep("a", 6L), "b", "b"))
  datos$geom <- sf::st_sfc(geoms, crs = 4326)
  anteriores <- options(lupa.max_geometrias = 3L)
  on.exit(options(anteriores), add = TRUE)

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  fila <- fila_geom_r28g(perfil)

  universo <- which(datos$tipo == "a")
  analizadas <- universo[unique(as.integer(round(seq.int(1, 6, length.out = 3L))))]
  expect_identical(analizadas, c(1L, 4L, 6L))
  # Antes: 1 invalida "de 6 evaluadas" y dominio "de 6".
  expect_identical(fila$n_validez_evaluados, length(analizadas))
  expect_identical(fila$n_geometrias_invalidas, 1L)
  expect_identical(fila$n_dominio_evaluados, length(analizadas))
  expect_identical(fila$n_fuera_de_dominio, 0L)
  invalida <- hallazgo_r28g(perfil, "geometria_invalida")
  expect_identical(as.numeric(invalida$n_evaluados), 3)
  recorte <- cobertura_r28g(perfil, "perfil_geometria")$motivo
  expect_true(any(grepl(
    "se analizaron 3 de sus 6 valores presentes; los 3 restantes",
    recorte, fixed = TRUE
  )))

  # Los `NA` de una columna WKT no son geometrias evaluadas. Universo: filas 1 a
  # 6, dos de ellas `NA`; presentes en el universo: 4. Sin el tope, que aca
  # recortaria tambien la conversion.
  options(anteriores)
  wkt <- c(
    "POINT (1 1)", NA, "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA,
    "POINT EMPTY", "POINT (2 2)", NA, "POINT (3 3)", "POINT (4 4)",
    "POINT (5 5)"
  )
  datos_wkt <- data.frame(
    tipo = c(rep("a", 6L), rep("b", 4L)), geom = wkt,
    stringsAsFactors = FALSE
  )
  perfil_wkt <- suppressWarnings(perfilar(
    datos_wkt, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  presentes_universo <- sum(!is.na(wkt[1:6]))
  expect_identical(presentes_universo, 4L)
  expect_identical(
    fila_geom_r28g(perfil_wkt)$n_validez_evaluados, presentes_universo
  )
})

test_that("A: con universo, el dominio no cuenta las vacias como evaluadas", {
  skip_if_not_installed("sf")
  geoms <- list(
    sf::st_point(c(1, 1)), sf::st_point(), sf::st_point(c(200, 1)),
    sf::st_point(c(2, 2)), sf::st_point(), sf::st_point(c(3, 3)),
    sf::st_point(c(4, 4)), sf::st_point(), sf::st_point(c(5, 5)),
    sf::st_point(c(6, 6))
  )
  datos <- data.frame(tipo = c(rep("a", 6L), rep("b", 4L)))
  datos$geom <- sf::st_sfc(geoms, crs = 4326)
  vacias <- vapply(geoms, function(g) all(is.na(unclass(g))), logical(1L))

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  fila <- fila_geom_r28g(perfil)

  # A mano: universo de 6 filas, 2 vacias -> 4 evaluadas, 1 fuera (lon 200).
  expect_identical(fila$n_dominio_evaluados, as.integer(6L - sum(vacias[1:6])))
  expect_identical(fila$n_fuera_de_dominio, 1L)
  fuera <- hallazgo_r28g(perfil, "coordenada_fuera_dominio")
  expect_identical(as.numeric(fuera$n_evaluados), 4)

  # Control: sin universo, 10 - 3 vacias = 7, como ya publicaba.
  sin_universo <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  expect_identical(sin_universo$n_dominio_evaluados, as.integer(10L - sum(vacias)))
})

test_that("B: geometria_vacia cuenta sobre las geometrias del universo", {
  skip_if_not_installed("sf")
  geoms <- list(
    sf::st_point(c(1, 1)), sf::st_point(), sf::st_point(c(2, 2)),
    sf::st_point(c(3, 3)), sf::st_point(), sf::st_point(c(4, 4)),
    sf::st_point(c(5, 5)), sf::st_point(), sf::st_point(c(6, 6)),
    sf::st_point(c(7, 7))
  )
  datos <- data.frame(tipo = c(rep("a", 6L), rep("b", 4L)))
  datos$geom <- sf::st_sfc(geoms, crs = 4326)

  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  vacia <- hallazgo_r28g(perfil, "geometria_vacia")
  # A mano: universo de 6 filas con 2 vacias. Antes: 2 "de 10".
  expect_identical(as.numeric(vacia$n_afectados), 2)
  expect_identical(as.numeric(vacia$n_evaluados), 6)

  # Una columna WKT con 3 `NA`: 7 geometrias presentes, una vacia. Antes:
  # 1 "de 10" mientras `geometria_invalida` publicaba 7.
  wkt <- c(
    "POINT (1 1)", NA, "POINT EMPTY", "POINT (2 2)", NA, "POINT (3 3)",
    NA, "POINT (4 4)", "POINT (5 5)", "POINT (6 6)"
  )
  perfil_wkt <- suppressWarnings(perfilar(
    data.frame(geom = wkt, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  vacia_wkt <- hallazgo_r28g(perfil_wkt, "geometria_vacia")
  expect_identical(as.numeric(vacia_wkt$n_evaluados), as.numeric(sum(!is.na(wkt))))
  expect_identical(as.numeric(vacia_wkt$n_afectados), 1)
})

test_that("K: tipos mixtos no cuenta las filas de regla indeterminada", {
  skip_if_not_installed("sf")
  geoms <- list(
    sf::st_point(c(1, 1)), cuadrado_r28g(), sf::st_point(c(2, 2)),
    sf::st_point(c(3, 3)), cuadrado_r28g(), sf::st_point(c(4, 4))
  )
  datos <- data.frame(edad = c(30, 40, NA, 50, NA, 10))
  datos$geom <- sf::st_sfc(geoms, crs = 4326)

  corrida <- avisos_r28g(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ edad > 18)
  ))
  mixtos <- hallazgo_r28g(corrida$valor, "tipos_geometria_mixtos")
  # A mano: universo {1, 2, 4} (edad > 18 y no NA) = POINT, POLYGON, POINT.
  universo <- which(!is.na(datos$edad) & datos$edad > 18)
  expect_identical(universo, c(1L, 2L, 4L))
  expect_identical(as.numeric(mixtos$n_afectados), as.numeric(length(universo)))
  expect_identical(as.numeric(mixtos$n_evaluados), as.numeric(length(universo)))
  expect_identical(
    sort(as.integer(mixtos$trazabilidad[[1L]]$indices_fila)), universo
  )
  # La incoherencia desaparece porque las dos cifras coinciden.
  expect_false(any(grepl("trazabilidad", corrida$avisos, fixed = TRUE)))

  # Universo vacio: no hay geometrias que mezclar. Antes: 2 afectadas.
  vacio <- avisos_r28g(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ edad > 100)
  ))
  expect_identical(nrow(hallazgo_r28g(vacio$valor, "tipos_geometria_mixtos")), 0L)
  expect_false(any(grepl("trazabilidad", vacio$avisos, fixed = TRUE)))
})

test_that("H: un valor que no convierte no borra una columna WKB", {
  skip_if_not_installed("sf")
  textos <- rep(c(
    "POINT (-56.16 -34.9)", "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))",
    "POINT (200 1)"
  ), length.out = 9L)
  geometria <- sf::st_as_sfc(textos, crs = 4326)
  invalidas <- sum(!sf::st_is_valid(sf::st_set_crs(geometria, NA)))
  fuera <- sum(grepl("200", textos, fixed = TRUE))
  expect_identical(c(invalidas, fuera), c(3L, 3L))
  hexadecimal <- as.character(unlist(
    sf::st_as_binary(geometria, EWKB = TRUE, hex = TRUE)
  ))

  for (basura in c("", "zz", substr(hexadecimal[[1L]], 1L, 26L))) {
    perfil <- suppressWarnings(perfilar(
      data.frame(id = 1:10, geom = c(hexadecimal, basura),
                 stringsAsFactors = FALSE),
      analizar_dependencias = FALSE
    ))
    fila <- fila_geom_r28g(perfil)
    # Antes: representacion NA, motivo NA y ningun campo geometrico.
    expect_identical(fila$representacion_geometria, "WKB hexadecimal")
    expect_identical(fila$n_validez_evaluados, 9L)
    expect_identical(fila$n_geometrias_invalidas, invalidas)
    expect_identical(fila$n_fuera_de_dominio, fuera)
    expect_match(fila$motivo_representacion, "1 de 10 valores presentes",
                 fixed = TRUE)
    declarada <- cobertura_r28g(perfil, "perfil_geometria")
    expect_true(any(grepl("1 de 10 valores presentes", declarada$motivo,
                          fixed = TRUE)))
  }

  # WKB crudo: el valor perdido se declara en vez de callarse.
  crudo <- lapply(sf::st_as_binary(geometria, EWKB = TRUE), as.raw)
  datos <- data.frame(id = 1:10)
  datos$geom <- c(crudo, list(as.raw(c(1, 2, 3))))
  fila <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  expect_identical(fila$n_validez_evaluados, 9L)
  expect_match(fila$motivo_representacion, "1 de 10 valores presentes",
               fixed = TRUE)

  # Control: un identificador hexadecimal sigue sin ser geometria.
  identificador <- lupa:::.perfilar_geometria(
    c("00ff00ff00ff00ff00", "01ab01ab01ab01ab01")
  )
  expect_false(identificador$aplica)

  # La biseccion que aisla lo que sf no lee tiene techo: con cuerpos corruptos
  # de encabezado plausible -un poligono que anuncia 65.535 anillos en 13
  # bytes-, pasado el tope de intentos fallidos el resto se cuenta por tramo, y
  # nada queda sin contar.
  corrupto <- as.raw(c(1, 3, 0, 0, 0, 255, 255, 0, 0, 1, 2, 3, 4))
  expect_true(lupa:::.wkb_plausible(corrupto))
  punto <- crudo[[1L]]
  mezcla <- c(rep(list(corrupto), 20L), list(punto), rep(list(corrupto), 20L))
  conversion <- lupa:::.convertir_a_sfc(
    mezcla, "WKB", seq_along(mezcla), max_intentos_fallidos = 4L
  )
  expect_identical(
    length(conversion$indices) + length(conversion$fallidas), length(mezcla)
  )
  expect_match(conversion$error, "se dejo de aislar valor por valor",
               fixed = TRUE)
  sin_techo <- lupa:::.convertir_a_sfc(mezcla, "WKB", seq_along(mezcla))
  expect_identical(sin_techo$indices, 21L)
})

test_that("I: el WKT que escribe PostGIS se mide y una curva no borra la columna", {
  skip_if_not_installed("sf")
  postgis <- c(
    "GEOMETRYCOLLECTION Z (POINT Z (1 2 3))",
    "GEOMETRYCOLLECTION M (POINT M (1 2 3))",
    "GEOMETRYCOLLECTION(GEOMETRYCOLLECTION(POINT(1 2)))"
  )
  columna <- c("POINT (1 1)", "POINT (2 2)", "POINT (3 3)", "POINT (4 4)", postgis)
  expect_length(sf::st_as_sfc(columna), length(columna))
  fila <- fila_geom_r28g(suppressWarnings(perfilar(
    data.frame(geom = columna, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )))
  # Antes: la columna entera sin medir, "WKT estructuralmente invalido".
  expect_identical(fila$n_validez_evaluados, length(columna))
  expect_true(is.na(fila$motivo_representacion))

  con_curva <- c(columna, "CIRCULARSTRING (0 0, 1 1, 2 0)")
  fila_curva <- fila_geom_r28g(suppressWarnings(perfilar(
    data.frame(geom = con_curva, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )))
  expect_identical(fila_curva$n_validez_evaluados, length(columna))
  expect_match(fila_curva$motivo_representacion, "1 de 8 valores presentes",
               fixed = TRUE)
  expect_false(grepl("estructuralmente", fila_curva$motivo_representacion,
                     fixed = TRUE))

  expect_identical(lupa:::.wkt_plausible(postgis), c(TRUE, TRUE, TRUE))
  # Control: el texto corrupto sigue sin pasar la guarda.
  expect_identical(
    lupa:::.wkt_plausible(c("POINT (1 2", "POINT )1 2(", "hola", "")),
    rep(FALSE, 4L)
  )
})

test_that("G: si los valores declaran su SRID, no se acusa crs_no_declarado", {
  skip_if_not_installed("sf")
  # Montevideo en 4326 y un punto en el centro de la zona UTM 21S (meridiano
  # 57 O, a unos 34.8 grados de latitud sur): los dos dentro de su area de uso.
  mixta <- c(
    "SRID=4326;POINT (-56.16 -34.9)", "SRID=32721;POINT (500000 6140000)"
  )
  centro_utm <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(500000, 6140000)), crs = 32721), 4326
  ))
  expect_true(abs(centro_utm[1L, "X"] + 57) < 0.01)
  perfil <- suppressWarnings(perfilar(
    data.frame(geom = mixta, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  fila <- fila_geom_r28g(perfil)
  # Antes: columna sin medir y `crs_no_declarado` [error].
  expect_identical(fila$crs_declarado, "4326, 32721")
  expect_identical(fila$n_dominio_evaluados, 2L)
  expect_identical(fila$n_fuera_de_dominio, 0L)
  expect_identical(nrow(hallazgo_r28g(perfil, "crs_no_declarado")), 0L)

  # Treinta EWKT en 4326 y un vacio: se mide en 4326.
  ewkt <- c(paste0("SRID=4326;POINT (", 1:30, " 1)"), "")
  perfil_vacio <- suppressWarnings(perfilar(
    data.frame(geom = ewkt, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  expect_identical(fila_geom_r28g(perfil_vacio)$crs_declarado, "4326")
  expect_identical(nrow(hallazgo_r28g(perfil_vacio, "crs_no_declarado")), 0L)

  # Nada convierte, pero cada valor dice SRID=4326: la columna declara su CRS.
  corruptos <- c("SRID=4326;POINT (1 2", "SRID=4326;POINT (3 4")
  perfil_corrupto <- suppressWarnings(perfilar(
    data.frame(geom = corruptos, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  fila_corrupta <- fila_geom_r28g(perfil_corrupto)
  expect_identical(fila_corrupta$representacion_geometria, "WKT")
  expect_true(is.na(fila_corrupta$n_validez_evaluados))
  expect_identical(fila_corrupta$crs_declarado, "4326")
  expect_identical(nrow(hallazgo_r28g(perfil_corrupto, "crs_no_declarado")), 0L)
})

test_that("E: un componente vacio no hace salir del dominio a una geometria", {
  skip_if_not_installed("sf")
  textos <- c(
    "GEOMETRYCOLLECTION (POINT EMPTY, POINT (10 10))",
    "MULTIPOINT (EMPTY, (10 10))", "POINT (20 20)"
  )
  for (crs in c(4326, 3857)) {
    geometria <- sf::st_as_sfc(textos, crs = crs)
    expect_false(any(sf::st_is_empty(geometria)))
    datos <- data.frame(id = 1:3)
    datos$geom <- geometria
    fila <- fila_geom_r28g(
      suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
    )
    # A mano: las unicas coordenadas reales son (10, 10) y (20, 20), dentro.
    expect_identical(fila$n_fuera_de_dominio, 0L, info = as.character(crs))
    expect_identical(fila$n_dominio_evaluados, 3L, info = as.character(crs))
  }
  # Control: un componente real fuera de rango sigue contando.
  datos <- data.frame(id = 1)
  datos$geom <- sf::st_as_sfc(
    "GEOMETRYCOLLECTION (POINT EMPTY, POINT (200 10))", crs = 4326
  )
  fila <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  expect_identical(fila$n_fuera_de_dominio, 1L)
})

test_that("F: un CRS geografico de otro meridiano o en gradianes se compara en su marco", {
  skip_if_not_installed("sf")
  # EPSG:4805 (Ferro) y EPSG:4807 (Paris, gradianes): el centro de su area de
  # uso, expresado en el propio CRS. La ida y vuelta a 4326 lo devuelve.
  centros <- list(`4805` = c(16.285, 44.935), `4807` = c(2.38, 46.225))
  for (codigo in names(centros)) {
    wgs84 <- sf::st_sfc(sf::st_point(centros[[codigo]]), crs = 4326)
    propio <- sf::st_transform(wgs84, as.integer(codigo))
    vuelta <- sf::st_coordinates(sf::st_transform(propio, 4326))
    expect_true(all(abs(vuelta[1L, 1:2] - centros[[codigo]]) < 1e-6))
    expect_false(isTRUE(all.equal(
      as.numeric(sf::st_coordinates(propio)[1L, 1:2]), centros[[codigo]]
    )))
    datos <- data.frame(id = 1)
    datos$geom <- propio
    fila <- fila_geom_r28g(
      suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
    )
    # Antes: fuera 1 de 1, comparando los numeros crudos con la BBOX Greenwich.
    expect_identical(fila$n_fuera_de_dominio, 0L, info = codigo)
  }
  # Control: 250 gradianes esta fuera del rango de la NTF (+-200), aunque PROJ
  # lo envolveria a -132.66 grados.
  datos <- data.frame(id = 1)
  datos$geom <- sf::st_sfc(sf::st_point(c(250, 50)), crs = 4807)
  fila <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  expect_identical(fila$n_fuera_de_dominio, 1L)
})

test_that("D: el trabajo del presupuesto geometrico se publica", {
  skip_if_not_installed("sf")
  geoms <- list(
    sf::st_point(c(1, 1)), cuadrado_r28g(),
    sf::st_linestring(rbind(c(0, 0), c(1, 1), c(2, 2)))
  )
  vertices <- vapply(geoms, function(g) {
    nrow(sf::st_coordinates(g))
  }, integer(1L))
  expect_identical(vertices, c(1L, 5L, 3L))
  datos <- data.frame(id = 1:3)
  datos$geom <- sf::st_sfc(geoms, crs = 4326)

  fila <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  # Antes: ni la columna existia.
  expect_identical(fila$n_geometrias_analizadas, 3L)
  expect_identical(fila$n_vertices_analizados, as.numeric(sum(vertices)))

  anteriores <- options(lupa.max_geometrias = 2L)
  on.exit(options(anteriores), add = TRUE)
  recortada <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  # unique(round(seq(1, 3, length.out = 2))) = 1 y 3.
  expect_identical(recortada$n_geometrias_analizadas, 2L)
  expect_identical(recortada$n_vertices_analizados, as.numeric(sum(vertices[c(1, 3)])))
})

test_that("J: los diagnosticos de palabras no aplican sobre una geometria de texto", {
  # Sesenta WKT distintos con un tope de trabajo bajo dejan la fila "excede el
  # alcance" que 2.500 valores dejaban con el tope por omision; tres de tipos
  # distintos, la de `patron_raro` por patron dominante bajo. Que no apliquen no
  # puede depender de cuantos valores haya.
  wkt <- sprintf("POINT (%d %d)", 1:60, 61:120)
  perfil <- suppressWarnings(perfilar(
    data.frame(geom = wkt, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, max_trabajo_vocabulario = 1e4
  ))
  vocabulario <- cobertura_r28g(perfil, "proximidad_vocabulario")
  expect_identical(nrow(vocabulario), 1L)
  expect_match(vocabulario$motivo, "no aplica", fixed = TRUE)

  tres <- c(
    "POINT (1 2)", "LINESTRING (0 0, 1 1)", "POLYGON ((0 0, 1 0, 1 1, 0 0))"
  )
  perfil_tres <- suppressWarnings(perfilar(
    data.frame(geom = tres, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  patron <- cobertura_r28g(perfil_tres, "patron_raro")
  expect_identical(nrow(patron), 1L)
  expect_match(patron$motivo, "no aplica", fixed = TRUE)

  # Control: un texto comun conserva su fila de patron_raro con la medicion.
  texto <- suppressWarnings(perfilar(
    data.frame(nombre = c("ana", "Beto 1", "c-3"), stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  propia <- cobertura_r28g(texto, "patron_raro", "nombre")
  expect_false(any(grepl("no aplica", propia$motivo, fixed = TRUE)))
})

test_that("L: con SRID mezclados todos geograficos, la invalidez es sospechosa", {
  skip_if_not_installed("sf")
  hex_de <- function(geometria, crs) {
    as.character(unlist(sf::st_as_binary(
      sf::st_sfc(geometria, crs = crs), EWKB = TRUE, hex = TRUE
    )))
  }
  columna <- c(
    hex_de(mono_r28g(), 4326), hex_de(sf::st_point(c(10, 50)), 4258),
    hex_de(sf::st_point(c(11, 51)), 4258)
  )
  expect_true(sf::st_is_longlat(sf::st_crs(4326)))
  expect_true(sf::st_is_longlat(sf::st_crs(4258)))
  perfil <- suppressWarnings(perfilar(
    data.frame(geom = columna, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  ))
  invalida <- hallazgo_r28g(perfil, "geometria_invalida")
  expect_identical(nrow(invalida), 1L)
  # Antes: `error`; la misma columna toda en 4326 publicaba `sospechoso`.
  expect_identical(as.character(invalida$severidad), "sospechoso")

  # Control: un SRID proyectado en la mezcla vuelve a `error`.
  proyectada <- c(columna[1:2], hex_de(sf::st_point(c(500000, 6140000)), 32721))
  invalida_proyectada <- hallazgo_r28g(suppressWarnings(perfilar(
    data.frame(geom = proyectada, stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )), "geometria_invalida")
  expect_identical(as.character(invalida_proyectada$severidad), "error")
})

test_that("C2: con universo, el presupuesto se gasta primero en el universo", {
  skip_if_not_installed("sf")
  # Universo {2, 4, 5, 7, 9}: monos en 2, 5 y 7; lon 250 y 270 en 4 y 9. Con un
  # tope de 5 geometrias, repartir entre todas las filas analizaba las posiciones
  # 1, 3, 6, 8 y 10 -ninguna del universo-; el universo entra entero en el tope.
  geoms <- list(
    cuadrado_r28g(), mono_r28g(), cuadrado_r28g(), sf::st_point(c(250, 1)),
    mono_r28g(), cuadrado_r28g(), mono_r28g(), cuadrado_r28g(),
    sf::st_point(c(270, 5)), cuadrado_r28g()
  )
  tipo <- c("b", "a", "b", "a", "a", "b", "a", "b", "a", "b")
  datos <- data.frame(tipo = tipo)
  datos$geom <- sf::st_sfc(geoms, crs = 4326)
  universo <- which(tipo == "a")
  invalidas <- sum(!sf::st_is_valid(sf::st_set_crs(datos$geom[universo], NA)))
  fuera <- sum(vapply(geoms[universo], function(g) {
    any(sf::st_coordinates(g)[, "X"] > 180)
  }, logical(1L)))
  expect_identical(c(invalidas, fuera), c(3L, 2L))

  anteriores <- options(lupa.max_geometrias = 5L)
  on.exit(options(anteriores), add = TRUE)
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  fila <- fila_geom_r28g(perfil)
  # Antes: 0 evaluadas (o "0 de 5" en la base) y ningun hallazgo.
  expect_identical(fila$n_validez_evaluados, length(universo))
  expect_identical(fila$n_geometrias_invalidas, invalidas)
  expect_identical(fila$n_dominio_evaluados, length(universo))
  expect_identical(fila$n_fuera_de_dominio, fuera)
  expect_identical(nrow(hallazgo_r28g(perfil, "geometria_invalida")), 1L)
  # El trabajo publicado sigue siendo el de la columna: el tope entero.
  expect_identical(fila$n_geometrias_analizadas, 5L)
  recorte <- cobertura_r28g(perfil, "perfil_geometria")$motivo
  expect_true(any(grepl("el recorte cayo fuera de el", recorte, fixed = TRUE)))
  expect_false(any(grepl("quedaron sin evaluar", recorte, fixed = TRUE)))

  # La misma columna en WKT: el tope de la conversion sigue la misma regla.
  datos_wkt <- data.frame(
    tipo = tipo, geom = sf::st_as_text(datos$geom), stringsAsFactors = FALSE
  )
  fila_wkt <- fila_geom_r28g(suppressWarnings(perfilar(
    datos_wkt, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  )))
  expect_identical(fila_wkt$n_validez_evaluados, length(universo))
  expect_identical(fila_wkt$n_geometrias_invalidas, invalidas)

  # Si el universo solo excede el tope, se muestrea de forma uniforme dentro de
  # el: con tope 3, unique(round(seq(1, 5, length.out = 3))) = 1, 3, 5 del
  # universo -las filas 2, 5 y 9-: dos monos y un punto fuera.
  options(lupa.max_geometrias = 3L)
  parcial <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE,
    aplicabilidad = list(geom = ~ tipo == "a")
  ))
  fila_parcial <- fila_geom_r28g(parcial)
  elegidas <- universo[unique(as.integer(round(seq.int(1, 5, length.out = 3L))))]
  expect_identical(elegidas, c(2L, 5L, 9L))
  expect_identical(fila_parcial$n_validez_evaluados, 3L)
  expect_identical(fila_parcial$n_geometrias_invalidas, 2L)
  expect_identical(fila_parcial$n_fuera_de_dominio, 1L)
  expect_true(any(grepl(
    "se analizaron 3 de sus 5 valores presentes",
    cobertura_r28g(parcial, "perfil_geometria")$motivo, fixed = TRUE
  )))

  # Control: sin universo, el presupuesto hace lo de siempre -1, 3, 6, 8, 10-.
  options(lupa.max_geometrias = 5L)
  sin_universo <- fila_geom_r28g(
    suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  )
  siempre <- unique(as.integer(round(seq.int(1, 10, length.out = 5L))))
  expect_identical(sin_universo$n_validez_evaluados, length(siempre))
  expect_identical(
    sin_universo$n_geometrias_invalidas,
    sum(!sf::st_is_valid(sf::st_set_crs(datos$geom[siempre], NA)))
  )
})

test_that("la proteccion no declara tapada una bbox que nunca se midio", {
  # `R/datos-personales.R` buscaba `representacion_geometrica`, un campo que no
  # existe. Corregido el nombre, una geometria de texto que no se pudo convertir
  # -sin bbox- pasaba a declarar `no_publicado_por_geometria_protegida`: un
  # tapado que no ocurrio. Su alcance sigue en NA.
  corrupta <- data.frame(id = 1:3, stringsAsFactors = FALSE)
  corrupta$domicilio <- c("POINT (1 2", "POINT (3 4", "POINT (5 6")
  fila <- suppressWarnings(perfilar(
    corrupta, proteger_datos_personales = TRUE, analizar_dependencias = FALSE
  ))$columnas
  fila <- fila[fila$columna == "domicilio", ]
  expect_identical(fila$representacion_geometria, "WKT")
  expect_true(is.na(fila$bbox_alcance))

  # Control: una geometria medida si declara su bbox tapada.
  skip_if_not_installed("sf")
  medida <- data.frame(id = 1:3)
  medida$domicilio <- sf::st_as_sfc(
    c("POINT (-56.11 -34.81)", "POINT (-56.12 -34.82)", "POINT (-56.13 -34.83)"),
    crs = 4326
  )
  fila_medida <- suppressWarnings(perfilar(
    medida, proteger_datos_personales = TRUE, analizar_dependencias = FALSE
  ))$columnas
  fila_medida <- fila_medida[fila_medida$columna == "domicilio", ]
  expect_identical(fila_medida$bbox_alcance, "no_publicado_por_geometria_protegida")
  expect_true(is.na(fila_medida$bbox_xmin))
})
