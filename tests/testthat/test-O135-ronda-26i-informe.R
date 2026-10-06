# Ronda 26-I: el informe HTML, refutado.

.fecha_O135 <- as.POSIXct("2026-03-01", tz = "UTC")

.medicion_O135 <- function(datos, id = "M", fecha = .fecha_O135) {
  especie <- especializar(metricas_nucleo()$NoNulo)
  instancias <- lapply(names(datos), function(t) instanciar(especie, t, "x"))
  suppressWarnings(medir(modelo(instancias), datos, id_medicion = id, fecha = fecha))
}

.html_O135 <- function(..., max_filas = 100L) {
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(..., archivo = archivo, fecha = .fecha_O135, max_filas = max_filas)
  paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("H1: la cobertura de la organizacion y de los conjuntos llega al informe y a print()", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  datos <- list(
    a1 = data.frame(x = c(1, NA)), b1 = data.frame(x = c(1, 2)),
    z1 = data.frame(x = 1)
  )
  for (n in names(datos)) DBI::dbWriteTable(con, n, datos[[n]])
  colA <- coleccion(con, "a1", nombre = "A")
  colB <- coleccion(con, "b1", nombre = "B")
  colZ <- coleccion(con, "z1", nombre = "ZETA")
  ent <- agregar(
    agregar(.medicion_O135(datos[c("a1", "b1")]), "atributo", "ratio"),
    "entidad", "promedio"
  )
  cA <- agregar(ent[ent$entidad == "a1", ], "coleccion", "promedio_ponderado",
                pesos = c(a1 = 1), coleccion = colA)
  cB <- agregar(ent[ent$entidad == "b1", ], "coleccion", "promedio_ponderado",
                pesos = c(b1 = 1), coleccion = colB)
  org <- organizacion("Org", list(A = colA, B = colB, ZETA = colZ))
  o <- agregar(rbind(cA, cB), "organizacion", "promedio_ponderado",
               pesos = c(A = 0.5, B = 0.5), organizacion = org)
  # A mano: la organizacion declara 3 colecciones y el numero lleva 2 (A y B);
  # falta ZETA. Antes el informe publicaba solo la union de las colecciones que
  # SI entraron -2 tablas de 2, ninguna sin medir- y nada de ZETA.
  expect_equal(attr(o, "cobertura_organizacion")$cobertura, 2 / 3)
  ev <- evaluar(o, perfil_evaluacion("P", regla_evaluacion("R", function(x) x > 0.5)))
  for (objeto in list(o, ev)) {
    html <- .html_O135(objeto)
    expect_match(html, "Cobertura de la organizaci\u00f3n", fixed = TRUE)
    expect_match(html, "<td>3</td><td>2</td><td>ZETA</td>", fixed = TRUE)
  }
  impresos <- list(
    salida_cli(print(o)), salida_cli(print(ev)),
    salida_cli(print(tablero_calidad(o))),
    salida_cli(print(indice_calidad(o, pesos = c(Completitud = 1))))
  )
  for (impreso in impresos) {
    expect_match(impreso, "Cobertura de la organizaci\u00f3n", fixed = TRUE)
    expect_match(impreso, "ZETA", fixed = TRUE)
  }

  # El conjunto de colecciones: 3 declaradas, 2 en el numero.
  cc <- agregar(rbind(cA, cB), "conjuntoColecciones", "promedio_ponderado",
                pesos = c(A = 0.5, B = 0.5),
                colecciones = list(A = colA, B = colB, ZETA = colZ))
  html <- .html_O135(cc)
  expect_match(html, "Cobertura del conjunto de colecciones", fixed = TRUE)
  expect_match(html, "<td>3</td><td>2</td><td>ZETA</td>", fixed = TRUE)
  expect_match(salida_cli(print(cc)), "ZETA", fixed = TRUE)

  # El conjunto de organizaciones: 2 declaradas, 1 en el numero; falta OZ.
  org1 <- organizacion("Org1", list(A = colA, B = colB))
  o1 <- agregar(rbind(cA, cB), "organizacion", "promedio_ponderado",
                pesos = c(A = 0.5, B = 0.5), organizacion = org1)
  co <- agregar(o1, "conjuntoOrganizaciones", "promedio_ponderado",
                pesos = c(Org1 = 1),
                organizaciones = list(org1, organizacion("OZ", list(ZETA = colZ))))
  html <- .html_O135(co)
  expect_match(html, "Cobertura del conjunto de organizaciones", fixed = TRUE)
  expect_match(html, "<td>2</td><td>1</td><td>OZ</td>", fixed = TRUE)
  expect_match(salida_cli(print(co)), "OZ", fixed = TRUE)

  # Y la parte con peso cero, que solo declaraba la advertencia de la
  # organizacion, se nombra.
  o0 <- agregar(rbind(cA, cB), "organizacion", "promedio_ponderado",
                pesos = c(A = 1, B = 0), organizacion = org1)
  html <- .html_O135(o0)
  expect_match(html, "<th>partes_con_peso_cero</th>", fixed = TRUE)
  expect_match(html, "<td>2</td><td>2</td><td>ninguna</td><td>B</td>", fixed = TRUE)
})

test_that("H7: la medida que una evaluacion suprime no la publica otra evaluacion del informe", {
  inst <- instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "edad")
  med <- medir(modelo(inst), data.frame(edad = c(20, NA, 35)),
               id_medicion = "enero", fecha = .fecha_O135)
  ev1 <- evaluar(med, perfil_evaluacion("Publicable", regla_evaluacion(
    "Presente", function(x) x > 0, desenlace = "suprimir"
  )))
  ev2 <- evaluar(med, perfil_evaluacion("Interno", regla_evaluacion(
    "Completo", function(x) x > 0.5
  )))
  # A mano: la celda 2 es NA, NoNulo = 0, `0 > 0` FALSE -> ev1 la suprime; en
  # ev2, `0 > 0.5` FALSE -> "no", que en una medida booleana es el 0 suprimido.
  suprimida <- as.character(ev1$desenlaces$id_medida)
  expect_identical(suprimida, "enero-NoNulo@personas.edad-000002")
  fila_ev2 <- function(html, medida) {
    filas <- strsplit(html, "</tr>", fixed = TRUE)[[1L]]
    filas[grepl(lupa:::.html_escapar(medida), filas, fixed = TRUE) &
            grepl("<td>Interno</td>", filas, fixed = TRUE)]
  }
  for (objetos in list(list(ev1, ev2), list(ev2, ev1), list(ev2, med, ev1))) {
    html <- .html_O135(objetos)
    fila <- fila_ev2(html, suprimida)
    expect_length(fila, 1L)
    expect_match(fila, "<td>[valor suprimido]</td>", fixed = TRUE)
    expect_no_match(fila, "<td>no</td>", fixed = TRUE)
    # Las que nadie suprimio siguen publicadas, y con la escritura del informe
    # para un logico: 20 y 35 cumplen, "si" -no el "TRUE" de `as.character()`
    # que dejaba el enmascarado en las demas celdas de la columna-.
    for (otra in c("000001", "000003")) {
      expect_match(
        fila_ev2(html, paste0("enero-NoNulo@personas.edad-", otra)),
        "<td>s\u00ed</td>", fixed = TRUE
      )
    }
  }
  # Y la supresion que trae un historico vale igual para los demas objetos del
  # informe: `historico_calidad(ev1)` la tapa, y la medicion y ev2 al lado la
  # publicaban -el 0 y el "no"-.
  h <- historico_calidad(ev1)
  html <- .html_O135(h, ev2)
  fila <- fila_ev2(html, suprimida)
  expect_match(fila, "<td>[valor suprimido]</td>", fixed = TRUE)
  expect_no_match(fila, "<td>no</td>", fixed = TRUE)
  filas <- strsplit(.html_O135(h, med), "</tr>", fixed = TRUE)[[1L]]
  fila_med <- filas[grepl(lupa:::.html_escapar(suprimida), filas, fixed = TRUE) &
                      grepl("<td>personas$edad[2]</td>", filas, fixed = TRUE)]
  expect_length(fila_med, 1L)
  expect_match(fila_med, "<td>[valor suprimido]</td>", fixed = TRUE)
  expect_no_match(fila_med, "<td>0</td>", fixed = TRUE)
})

# Las celdas de una tabla del informe, por el titulo que la precede.
.celdas_O135 <- function(html, titulo) {
  resto <- strsplit(html, paste0("<h3>", titulo, "</h3>"), fixed = TRUE)[[1L]][[2L]]
  tabla <- strsplit(resto, "</table>", fixed = TRUE)[[1L]][[1L]]
  celdas <- regmatches(tabla, gregexpr("<td[^>]*>.*?</td>", tabla, perl = TRUE))[[1L]]
  gsub("<[^>]+>", "", celdas)
}

test_that("H2: una cifra que no cabe en 240 caracteres no se corta como un texto", {
  # El ausente de Stata leido como numero: 8.98846567431158e307.
  centinela <- 8.98846567431158e307
  perfil <- perfilar(data.frame(peso = c(10, 20, 30, centinela)))
  html <- .html_O135(perfil)
  celdas <- .celdas_O135(html, "Resumen por columna")
  # A mano: sin exponente tiene 308 cifras enteras; cortado a 239 y "...", vale
  # 8.988e238 -69 ordenes menos-. Ahora vuelve al valor del objeto.
  expect_true(any(celdas == "8.98846567431158e+307"))
  expect_identical(as.numeric("8.98846567431158e+307"), centinela)
  expect_false(any(grepl("^8988465674", celdas)))
  # La media, un cuarto del centinela mas 15: otra cifra, con su propia magnitud
  # -antes las dos salian con las mismas 239 cifras-, y vuelve a su valor.
  media <- mean(c(10, 20, 30, centinela))
  vuelven <- suppressWarnings(as.numeric(celdas)) == media
  expect_true(any(vuelven, na.rm = TRUE))
  expect_match(celdas[which(vuelven)[[1L]]], "e\\+307$")
  # Las chicas: 3e-301 escrito sin exponente es "0.000..." y se leia 0.
  expect_identical(lupa:::.resumir_valor_reporte(3e-301), "3e-301")
  # Una que cabe sigue sin exponente: 1e239 tiene 240 cifras.
  expect_identical(nchar(lupa:::.resumir_valor_reporte(1e239)), 240L)
  # Y varias en una celda se abrevian por cifras enteras, no por la mitad.
  varias <- lupa:::.resumir_valor_reporte(rep(123456789, 30))
  expect_match(varias, "^(123456789, )+\u2026$")
})

test_that("H8: la cifra publicada vuelve al valor del objeto, al lado de su veredicto", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  med <- medir(
    modelo(list(instanciar(especie, "t", "x"), instanciar(especie, "t", "y"))),
    data.frame(x = c(1, 2, NA), y = c(1, NA, NA)), id_medicion = "M",
    fecha = .fecha_O135
  )
  agregada <- agregar(med, "atributo", "ratio")
  # A mano: x tiene 2 de 3 celdas y y 1 de 3 -> 2/3 y 1/3, que con quince
  # cifras se escribian 0.666666666666667 y 0.333333333333333: otros dobles.
  expect_identical(sort(agregada$resultado), c(1 / 3, 2 / 3))
  expect_identical(as.numeric("0.6666666666666666"), 2 / 3)
  expect_identical(as.numeric("0.3333333333333333"), 1 / 3)
  ev <- evaluar(agregada, perfil_evaluacion("P", regla_evaluacion(
    "Dos tercios", function(x) x >= 2 / 3
  )))
  html <- .html_O135(ev, agregada)
  expect_match(html, "<td>0.6666666666666666</td>", fixed = TRUE)
  expect_match(html, "<td>0.3333333333333333</td>", fixed = TRUE)
  expect_no_match(html, "0.666666666666667<", fixed = TRUE)
  # La de la refutacion: 0.1 + 0.2, al lado de "no cumple x <= 0.3".
  expect_identical(lupa:::.resumir_valor_reporte(0.1 + 0.2), "0.30000000000000004")
  expect_identical(lupa:::.resumir_valor_reporte(0.3), "0.3")
  # Y la medicion enmascarada por una supresion escribe sus otras cifras igual:
  # el enmascarado volvia texto la columna con `as.character()`, quince cifras.
  ev_sup <- evaluar(agregada, perfil_evaluacion("S", regla_evaluacion(
    "Mayoria", function(x) x > 0.5, desenlace = "suprimir"
  )))
  html <- .html_O135(ev_sup, agregada)
  expect_match(html, "[valor suprimido]", fixed = TRUE)
  expect_match(html, "<td>0.6666666666666666</td>", fixed = TRUE)
  expect_no_match(html, "0.333333333333333", fixed = TRUE)
})

test_that("H6: las tablas de indicadores escriben cada valor con su tipo", {
  datos <- as.data.frame(matrix(1L, nrow = 1000, ncol = 10))
  perfil <- perfilar(datos)
  # A mano: 1000 x 10 = 10000 celdas, guardadas como doble.
  expect_identical(perfil$general$celdas, 10000)
  celdas <- .celdas_O135(.html_O135(perfil), "Resumen general")
  expect_identical(celdas[[which(celdas == "Celdas") + 1L]], "10000")
  expect_false(any(celdas == "1e+04"))
  memoria <- celdas[[which(celdas == "Memoria de los datos") + 1L]]
  expect_match(memoria, paste0("^", perfil$general$memoria_bytes, " bytes \\("))
  # Los logicos del alcance de las asociaciones, como en el resto del informe.
  an <- analizar(data.frame(a = c(1, 2, 3, 4), b = c(2, 4, 6, 9)))
  celdas <- .celdas_O135(.html_O135(an), "Alcance y recortes")
  expect_identical(celdas[[which(celdas == "Muestreado") + 1L]], "no")
  expect_identical(celdas[[which(celdas == "Salida truncada") + 1L]], "no")
})

test_that("H3: la evolucion dice por que falta el delta cuando la deriva no arma el par", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  pe <- perfil_evaluacion("P", regla_evaluacion("R", function(x) x > 0.5))
  eA <- evaluar(medir(modelo(instanciar(especie, "clientes", "x")),
                      data.frame(x = c(1, NA, 3, 4)), id_medicion = "ene",
                      fecha = as.POSIXct("2026-01-31", tz = "UTC")), pe)
  eB <- evaluar(medir(modelo(instanciar(especie, "proveedores", "x")),
                      data.frame(x = c(1, 2, 3, 4)), id_medicion = "feb",
                      fecha = as.POSIXct("2026-02-28", tz = "UTC")), pe)
  # A mano: ene 3/4 = 0.75 sobre clientes, feb 4/4 = 1 sobre proveedores; puestas
  # en serie bajo P se leian como una mejora. `comparar_evaluaciones()` dice que
  # no se comparan, y el informe tiene que decir lo mismo.
  motivo <- comparar_evaluaciones(eA, eB)$comparacion
  expect_identical(
    motivo,
    "No se puede comparar: las dos corridas no miden la misma tabla (clientes contra proveedores)."
  )
  evolucion <- lupa:::.evolucion_historico(historico_calidad(eA, eB))
  feb <- evolucion[evolucion$id_medicion == "feb", ]
  expect_true(is.na(feb$delta))
  expect_identical(feb$comparado_con, "ene")
  expect_identical(feb$comparacion, motivo)
  # La primera corrida no tiene con que compararse y no inventa un motivo.
  expect_true(is.na(evolucion$comparacion[evolucion$id_medicion == "ene"]))
  html <- .html_O135(historico_calidad(eA, eB))
  expect_match(html, "no miden la misma tabla (clientes contra proveedores)", fixed = TRUE)
})

test_that("H4: el informe del plan publica los diagnosticos que el perfil no evaluo", {
  datos <- data.frame(
    monto = c(12.5, 30, 41, 18, 22, 1000),
    cod = c("A1", "a1", "A1", "B2", "B2", "B2")
  )
  perfil <- suppressWarnings(perfilar(datos))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  no_evaluados <- attr(plan, "cobertura_diagnosticos")
  # A mano: con seis valores, los atipicos de `monto` no se evaluan; el plan lo
  # declara y la consola lo imprime. Sin `stringdist` -un Suggests- tampoco se
  # evaluan los casi duplicados: la cuenta depende de lo instalado, la promesa no.
  diagnosticos <- as.character(no_evaluados$diagnostico)
  expect_true("outliers" %in% diagnosticos)
  expect_match(salida_cli(print(plan)), paste0(nrow(no_evaluados), " diagn"),
               fixed = TRUE)
  # El informe del plan solo -sin el perfil al lado- tambien, con cada uno.
  html <- .html_O135(plan)
  expect_match(html, "<h3>Diagn\u00f3sticos no evaluados</h3>", fixed = TRUE)
  celdas <- .celdas_O135(html, "Diagn\u00f3sticos no evaluados")
  expect_true(all(c(diagnosticos, "monto") %in% celdas))
  expect_true(all(lupa:::.html_escapar(as.character(no_evaluados$motivo)) %in% celdas))
})

test_that("H10: lo que no se midio no queda detras del corte de la tabla del historico", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  med <- suppressWarnings(medir(
    modelo(list(instanciar(especie, "t", "x"), instanciar(especie, "t", "no_existe"))),
    data.frame(x = c(1, 2, NA, 4, 5)), id_medicion = "R1", fecha = .fecha_O135
  ))
  h <- historico_calidad(med)
  # A mano: 5 celdas de `x` -> 5 filas `medida`, y `no_existe` -> 1 fila
  # `metrica_no_evaluada`. Ordenada por nivel, "medida" < "metrica_no_evaluada":
  # con un corte de 3 filas la declaracion quedaba sexta, afuera.
  expect_identical(as.vector(table(h$nivel)[c("medida", "metrica_no_evaluada")]), c(5L, 1L))
  html <- .html_O135(h, max_filas = 3L)
  expect_match(html, "Se muestran 3 de 6 filas.", fixed = TRUE)
  expect_match(html, "<h3>Lo que no se midi\u00f3</h3>", fixed = TRUE)
  celdas <- .celdas_O135(html, "Lo que no se midi\u00f3")
  expect_true("metrica_no_evaluada" %in% celdas)
  expect_true(any(grepl("NoNulo&#64;t.no_existe", celdas, fixed = TRUE)))
  # La misma medicion reportada directamente ya lo publicaba, sin tope.
  expect_match(.html_O135(med, max_filas = 3L), "no_existe", fixed = TRUE)
})

test_that("H9: el informe nombra los pares medidos fuera del marco, no solo los cuenta", {
  propio <- marco_calidad("Marco operativo", list(Trazabilidad = "Origen documentado"))
  an <- suppressWarnings(suppressMessages(analizar(
    data.frame(a = c(1, 2, NA, 2), b = c("x", "y", "y", NA)), marco = propio
  )))
  fuera <- attr(an$tablero, "pares_fuera_del_marco")
  # A mano: el marco declara solo Trazabilidad; lo que el perfil mide -Completitud
  # y Exactitud- queda afuera, y el alcance lo cuenta.
  expect_true(length(fuera) >= 1L)
  expect_identical(
    attr(an$tablero, "alcance")$medidos_fuera_del_marco, length(fuera)
  )
  html <- .html_O135(an)
  celdas <- .celdas_O135(html, "Pares medidos fuera del marco")
  expect_setequal(celdas, lupa:::.html_escapar(fuera))
})

test_that("H5: los caracteres de control se escriben con su codigo en todo el informe", {
  ctl <- paste0("ctl", rawToChar(as.raw(0x01)), "x")
  bel <- paste0("bel", rawToChar(as.raw(0x07)), "y")
  perfil <- suppressWarnings(perfilar(data.frame(
    t = rep(c(ctl, bel, "comun", "otro"), 3), stringsAsFactors = FALSE
  )))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo, fecha = .fecha_O135)
  bytes <- readBin(archivo, "raw", file.info(archivo)$size)
  # A mano: ningun byte de control fuera de tabulador y saltos de linea. Antes
  # salian crudos -63 74 6c 01 78- en la moda, los ejemplos y los patrones, y un
  # navegador no los dibuja: `ctl\x01x` se veia `ctlx`.
  controles <- as.integer(bytes)
  expect_false(any(controles < 32L & !controles %in% c(9L, 10L, 13L)))
  expect_false(any(controles == 127L))
  html <- rawToChar(bytes)
  expect_match(html, "ctl&lt;U+0001&gt;x", fixed = TRUE)
  expect_match(html, "bel&lt;U+0007&gt;y", fixed = TRUE)
  # Y un C1 -U+0085, el NEL- tambien.
  expect_identical(
    lupa:::.html_escapar(paste0("a", intToUtf8(0x85L), "b")), "a&lt;U+0085&gt;b"
  )
  # El texto corriente, con tildes, no cambia.
  expect_identical(lupa:::.html_escapar("a\u00f1o"), "a\u00f1o")
})
