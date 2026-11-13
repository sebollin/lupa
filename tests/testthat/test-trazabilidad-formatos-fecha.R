# Los tres hallazgos que cuentan formatos de fecha -`formatos_fecha_mixtos`,
# `formato_fecha_ambiguo` y `anio_de_dos_digitos`- nombran las celdas escritas en
# esos formatos. Antes decian `no_aplica`: contaban formatos y no habia manera de
# llegar a las filas que habia que corregir.
#
# Pesada para CRAN (perfila unas cuarenta tablas chicas): corre con
# NOT_CRAN=true, en la suite completa y en la integracion continua.
testthat::skip_on_cran()

perfilar_fechas <- function(datos, ...) {
  perfilar(
    datos, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE, ...
  )
}

fechas <- function(...) data.frame(fecha = c(...), stringsAsFactors = FALSE)

hallazgos_de <- function(perfil, tipo, columna = "fecha") {
  h <- perfil$hallazgos
  h[as.character(h$tipo_hallazgo) == tipo &
      as.character(h$columna) %in% columna, , drop = FALSE]
}

traza_de <- function(perfil, tipo, columna = "fecha") {
  hallazgo <- hallazgos_de(perfil, tipo, columna)
  expect_equal(nrow(hallazgo), 1L, info = tipo)
  hallazgo$trazabilidad[[1L]]
}

tipos_formato_fecha <- c(
  "formatos_fecha_mixtos", "formato_fecha_ambiguo", "anio_de_dos_digitos"
)

sin_hallazgos_de_formato <- function(perfil) {
  !any(as.character(perfil$hallazgos$tipo_hallazgo) %in% tipos_formato_fecha)
}

detalle_de_la_guarda <- function(expr) {
  detalle <- character()
  withCallingHandlers(expr, lupa_trazabilidad_incoherente = function(w) {
    detalle <<- c(detalle, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  detalle
}

# El caso de la guia: 150 fechas ISO, 30 con barra y anio de cuatro cifras
# (filas 151 a 180) y 20 con anio de dos cifras (181 a 200).
tres_escrituras <- function() {
  fechas(rep("2024-01-15", 150), rep("15/01/2024", 30), rep("01/02/24", 20))
}

test_that("los tres hallazgos de formato de fecha nombran sus filas", {
  expect_no_warning(
    perfil <- perfilar_fechas(tres_escrituras()),
    class = "lupa_trazabilidad_incoherente"
  )

  mixtos <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(mixtos$estado, "disponible")
  expect_identical(mixtos$alcance, "completo")
  expect_identical(mixtos$localizador, "indice_fila")
  # Todas las filas con fecha: primero las 50 de las dos escrituras
  # minoritarias, en orden de fila, y despues las 150 de la dominante.
  expect_identical(mixtos$indices_fila, c(151:200, 1:150))
  expect_equal(mixtos$total, 200)
  expect_identical(mixtos$mostrados, 200L)
  expect_false(mixtos$truncado)
  expect_equal(mixtos$n_filas_formas_variantes, 50)
  expect_equal(mixtos$n_filas_formas_dominantes, 150)
  expect_equal(mixtos$mostrados_formas_variantes, 50)
  expect_equal(mixtos$mostrados_formas_dominantes, 150)

  for (tipo in c("formato_fecha_ambiguo", "anio_de_dos_digitos")) {
    traza <- traza_de(perfil, tipo)
    expect_identical(traza$estado, "disponible", info = tipo)
    expect_identical(traza$alcance, "completo", info = tipo)
    # Las 20 celdas `01/02/24`, que son las que no se pueden leer.
    expect_identical(traza$indices_fila, 181:200, info = tipo)
    expect_equal(traza$total, 20, info = tipo)
    # Sin reparto: en estos dos no hay una escritura dominante.
    expect_null(traza$n_filas_formas_variantes, info = tipo)
  }
})

test_that("la traza no cambia lo que el hallazgo cuenta", {
  perfil <- perfilar_fechas(tres_escrituras())
  # Siguen contando formatos: `total` cuenta filas y no se compara con esto.
  esperado <- c(
    formatos_fecha_mixtos = 4, formato_fecha_ambiguo = 2,
    anio_de_dos_digitos = 2
  )
  for (tipo in names(esperado)) {
    hallazgo <- hallazgos_de(perfil, tipo)
    expect_identical(as.character(hallazgo$unidad_conteo), "formato", info = tipo)
    expect_equal(hallazgo$n_afectados, esperado[[tipo]], info = tipo)
    expect_equal(hallazgo$n_evaluados, 4, info = tipo)
  }
  expect_identical(
    hallazgos_de(perfil, "formatos_fecha_mixtos")$evidencia,
    "%Y-%m-%d (150); %d/%m/%Y (30); %d/%m/%y (20); %m/%d/%y (20)"
  )
})

test_that("con tres formatos las variantes son los dos minoritarios juntos", {
  perfil <- perfilar_fechas(fechas(
    rep("2020-01-31", 100), rep("31/01/2020", 60), rep("31.01.2020", 40)
  ))
  traza <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(traza$indices_fila, c(101:200, 1:100))
  expect_equal(traza$n_filas_formas_variantes, 100)
  expect_equal(traza$n_filas_formas_dominantes, 100)
})

test_that("con empate en el primer lugar no hay dominante", {
  # Ninguna evidencia dice cual de las dos escrituras es la correcta: elegir
  # una seria decidir en silencio. Todas las filas son variantes.
  casos <- list(
    en_bloque = rep(c("2020-01-31", "31/01/2020"), each = 100),
    intercalado = rep(c("2020-01-31", "31/01/2020"), 100),
    dos_filas = c("2024-01-15", "15/01/2024"),
    con_un_tercero = c(
      rep("2020-01-31", 100), rep("31/01/2020", 100), rep("31.01.2020", 50)
    )
  )
  for (nombre in names(casos)) {
    traza <- traza_de(perfilar_fechas(fechas(casos[[nombre]])),
                      "formatos_fecha_mixtos")
    n <- length(casos[[nombre]])
    expect_identical(traza$indices_fila, seq_len(n), info = nombre)
    expect_equal(traza$n_filas_formas_variantes, n, info = nombre)
    expect_equal(traza$n_filas_formas_dominantes, 0, info = nombre)
  }
  # La otra mitad: con una fila de diferencia si hay dominante.
  traza <- traza_de(
    perfilar_fechas(fechas(rep("2020-01-31", 101), rep("31/01/2020", 99))),
    "formatos_fecha_mixtos"
  )
  expect_identical(traza$indices_fila, c(102:200, 1:101))
  expect_equal(traza$n_filas_formas_variantes, 99)
  expect_equal(traza$n_filas_formas_dominantes, 101)
})

test_that("una celda que dos lecturas probadas leen distinto va con las variantes", {
  # Diez `13/06/2020` prueban dia/mes, cinco `06/30/2020` prueban mes/dia, y
  # `01/12/2020` (16 a 18) lo leen las dos: no es de ninguna de las dos.
  perfil <- perfilar_fechas(fechas(
    rep("13/06/2020", 10), rep("06/30/2020", 5), rep("01/12/2020", 3)
  ))
  traza <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(traza$indices_fila, c(11:18, 1:10))
  expect_equal(traza$total, 18)
  expect_equal(traza$n_filas_formas_variantes, 8)
  expect_equal(traza$n_filas_formas_dominantes, 10)
})

test_that("una fecha que dos lecturas aceptan entra en la traza del anio corto", {
  # Lo mismo con el anio de dos cifras: `01/12/20` (16 a 18) no lo cuenta el
  # `n` de ninguna de las dos lecturas, pero esta escrito en las dos que el
  # hallazgo nombra y tiene el anio corto como las otras quince.
  datos <- fechas(rep("13/06/20", 10), rep("06/30/20", 5), rep("01/12/20", 3))
  expect_no_warning(
    perfil <- perfilar_fechas(datos), class = "lupa_trazabilidad_incoherente"
  )
  traza <- traza_de(perfil, "anio_de_dos_digitos")
  expect_identical(traza$indices_fila, 1:18)
  expect_equal(traza$total, 18)
  # La tabla de formatos cuenta 10 y 5: la traza nombra mas filas que esa suma.
  formatos <- perfil$formatos_fecha$fecha
  expect_equal(sum(formatos$n[formatos$anio_dos_digitos]), 15)
  # Los otros dos no cambian: ninguna lectura queda sin resolver, y mixtos
  # sigue poniendo esas tres celdas con las variantes.
  expect_equal(nrow(hallazgos_de(perfil, "formato_fecha_ambiguo")), 0L)
  mixtos <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(mixtos$indices_fila, c(11:18, 1:10))
  expect_equal(mixtos$n_filas_formas_variantes, 8)
  expect_equal(mixtos$n_filas_formas_dominantes, 10)
})

# Con `muestra = 20` sobre 300 filas, la muestra toma las filas 1, 17, 32, 48,
# 64, 80, 95, 111, 127, 143, 158, 174, 190, 206, 221, 237, 253, 269, 284 y 300:
# lo que se escribe en las filas 2 a 10 no lo ve.
fuera_de_la_muestra <- 2:10

test_that("el anio corto se traza sobre la columna entera, no sobre lo que vio la muestra", {
  # 380 `01/12/20`, 10 `13/06/20` y 10 `06/30/20`, con una muestra de 20: la
  # muestra solo publica `%m/%d/%y`, pero las 400 filas tienen el anio con
  # dos cifras y ninguna admite otra lectura.
  datos <- fechas(rep("01/12/20", 380), rep("13/06/20", 10), rep("06/30/20", 10))
  expect_no_warning(
    perfil <- perfilar_fechas(datos, muestra = 20),
    class = "lupa_trazabilidad_incoherente"
  )
  formatos <- perfil$formatos_fecha$fecha
  expect_false("%d/%m/%y" %in% formatos$formato)
  corto <- hallazgos_de(perfil, "anio_de_dos_digitos")
  expect_equal(nrow(corto), 1L)
  expect_identical(corto$trazabilidad[[1L]]$indices_fila, 1:400)
  expect_equal(corto$trazabilidad[[1L]]$total, 400)
  # Lo que el hallazgo cuenta y cita sigue siendo de la muestra.
  expect_equal(corto$n_afectados, 1)
  expect_match(corto$evidencia, "%m/%d/%y", fixed = TRUE)
  expect_false(grepl("%d/%m/%y", corto$evidencia, fixed = TRUE))
})

test_that("la lectura ambigua y los formatos mixtos nombran filas que la muestra no vio", {
  valores <- rep("2024-01-15", 300)
  valores[101:160] <- "03/04/2021"
  # Otra escritura ambigua, solo en filas que la muestra no toma.
  valores[fuera_de_la_muestra] <- "05-06-2021"
  expect_no_warning(
    perfil <- perfilar_fechas(fechas(valores), muestra = 20),
    class = "lupa_trazabilidad_incoherente"
  )
  formatos <- perfil$formatos_fecha$fecha
  expect_false(any(grepl("-%Y", formatos$formato, fixed = TRUE)))
  ambiguo <- hallazgos_de(perfil, "formato_fecha_ambiguo")
  expect_equal(nrow(ambiguo), 1L)
  expect_identical(
    ambiguo$trazabilidad[[1L]]$indices_fila, c(fuera_de_la_muestra, 101:160)
  )
  expect_false(grepl("-%m-", ambiguo$evidencia, fixed = TRUE))
  # Mixtos trae todas las filas con fecha; las de la escritura que la muestra
  # no vio van con las variantes, y la dominante es la ISO de la columna entera.
  mixtos <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_equal(mixtos$total, 300)
  expect_equal(mixtos$n_filas_formas_variantes, 69)
  expect_equal(mixtos$n_filas_formas_dominantes, 231)
  expect_setequal(mixtos$indices_fila[1:69], c(fuera_de_la_muestra, 101:160))
})

test_that("si la columna entera resuelve lo que la muestra no pudo, se nombran las celdas que admiten las lecturas citadas", {
  # La muestra solo ve `01/02/24`: dia/mes o mes/dia, sin resolver. Cuatro
  # `13/02/24` fuera de la muestra resuelven la columna entera como dia/mes:
  # ninguna fila queda ambigua de ese modo, y la traza nombra las 60 celdas
  # que admiten las dos lecturas que el hallazgo cita, no las cuatro resueltas.
  valores <- rep("2024-01-15", 300)
  valores[101:160] <- "01/02/24"
  valores[2:5] <- "13/02/24"
  expect_no_warning(
    perfil <- perfilar_fechas(fechas(valores), muestra = 20),
    class = "lupa_trazabilidad_incoherente"
  )
  ambiguo <- hallazgos_de(perfil, "formato_fecha_ambiguo")
  expect_equal(nrow(ambiguo), 1L)
  expect_identical(ambiguo$trazabilidad[[1L]]$indices_fila, 101:160)
  expect_identical(
    traza_de(perfil, "anio_de_dos_digitos")$indices_fila, c(2:5, 101:160)
  )
})

test_that("una columna entera ambigua nombra todas sus fechas", {
  perfil <- perfilar_fechas(fechas(rep(c("03/04/2021", "05/06/2021"), 10)))
  traza <- traza_de(perfil, "formato_fecha_ambiguo")
  expect_identical(traza$indices_fila, 1:20)
  expect_equal(traza$total, 20)
  # Las dos lecturas son una sola escritura: no hay formatos mixtos.
  expect_equal(nrow(hallazgos_de(perfil, "formatos_fecha_mixtos")), 0L)
})

test_that("el anio corto y la lectura ambigua no siempre son las mismas filas", {
  # `25-12-31` (1 a 10) se lee como dia-mes-anio o anio-mes-dia; `01-25-95`
  # (11 a 16) solo como mes-dia-anio. Las 16 tienen el anio con dos cifras y
  # solo las 10 primeras son ambiguas.
  perfil <- perfilar_fechas(fechas(rep("25-12-31", 10), rep("01-25-95", 6)))
  expect_identical(traza_de(perfil, "formato_fecha_ambiguo")$indices_fila, 1:10)
  expect_identical(traza_de(perfil, "anio_de_dos_digitos")$indices_fila, 1:16)
  mixtos <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(mixtos$indices_fila, c(11:16, 1:10))

  # Un anio corto que el dia resuelve: 20 `25/12/99` y 5 `01/02/99`.
  perfil <- perfilar_fechas(fechas(rep("25/12/99", 20), rep("01/02/99", 5)))
  expect_identical(traza_de(perfil, "anio_de_dos_digitos")$indices_fila, 1:25)

  # Un mes escrito con el anio corto, junto a fechas ISO.
  perfil <- perfilar_fechas(fechas(
    rep("5 de marzo de 21", 20), rep("2024-01-15", 10)
  ))
  expect_identical(traza_de(perfil, "anio_de_dos_digitos")$indices_fila, 1:20)
  expect_identical(
    traza_de(perfil, "formatos_fecha_mixtos")$indices_fila, c(21:30, 1:20)
  )
})

test_that("lo que no es una fecha no entra en la traza", {
  valores <- c(rep("2024-01-15", 30), rep("15/01/2024", 10))
  valores[c(5, 9)] <- NA
  valores[[12L]] <- ""
  valores[[13L]] <- "  "
  valores[[20L]] <- "pendiente"
  traza <- traza_de(perfilar_fechas(fechas(valores)), "formatos_fecha_mixtos")
  fuera <- c(5L, 9L, 12L, 13L, 20L)
  expect_identical(traza$indices_fila, c(31:40, setdiff(1:30, fuera)))
  expect_equal(traza$total, 35)
  expect_false(any(fuera %in% traza$indices_fila))
})

test_that("un factor se traza igual que el texto", {
  valores <- c(rep("2024-01-15", 30), rep("15/01/2024", 10))
  de_texto <- traza_de(perfilar_fechas(fechas(valores)), "formatos_fecha_mixtos")
  de_factor <- traza_de(
    perfilar_fechas(data.frame(fecha = factor(valores))),
    "formatos_fecha_mixtos"
  )
  expect_identical(de_factor, de_texto)
  expect_identical(de_texto$indices_fila, c(31:40, 1:30))
})

test_that("una sola fila: sin hallazgo si se lee, con su fila si es ambigua", {
  expect_true(sin_hallazgos_de_formato(perfilar_fechas(fechas("15/01/2024"))))
  traza <- traza_de(perfilar_fechas(fechas("03/04/2021")),
                    "formato_fecha_ambiguo")
  expect_identical(traza$indices_fila, 1L)
  expect_equal(traza$total, 1)
})

test_that("lo que no emite el hallazgo no tiene traza que dar", {
  # Las mitades que no cambian: una fecha ya tipada, y una columna que el dia
  # resuelve (17 filas con dia mayor que 12 y 3 que solas serian ambiguas).
  expect_true(sin_hallazgos_de_formato(perfilar_fechas(
    data.frame(fecha = as.Date("2024-01-01") + 0:39)
  )))
  expect_true(sin_hallazgos_de_formato(perfilar_fechas(data.frame(
    fecha = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 3600 * 0:39
  ))))
  expect_true(sin_hallazgos_de_formato(perfilar_fechas(fechas(
    rep("25/04/2021", 17), "03/04/2021", "05/06/2021", "01/02/2021"
  ))))
})

test_that("cada traza sale de su columna, con cualquier nombre", {
  valores <- c(rep("2024-01-15", 30), rep("15/01/2024", 10))
  nombres <- c("fecha de alta", "a\u00f1o{1+1}", "x,y", "`", "")
  for (nombre in nombres) {
    datos <- data.frame(valores, stringsAsFactors = FALSE)
    names(datos) <- nombre
    expect_no_warning(
      perfil <- perfilar_fechas(datos), class = "lupa_trazabilidad_incoherente"
    )
    traza <- traza_de(perfil, "formatos_fecha_mixtos", nombre)
    expect_identical(traza$indices_fila, c(31:40, 1:30), info = nombre)
  }
  # Dos columnas con el mismo nombre: el nombre no las distingue, la posicion
  # si. La segunda tiene la minoria en las filas 36 a 40.
  repetidas <- data.frame(
    valores, c(rep("2024-01-15", 35), rep("15/01/2024", 5)),
    stringsAsFactors = FALSE
  )
  names(repetidas) <- c("f", "f")
  perfil <- perfilar_fechas(repetidas)
  hallazgos <- hallazgos_de(perfil, "formatos_fecha_mixtos", "f")
  expect_equal(nrow(hallazgos), 2L)
  expect_identical(
    hallazgos$trazabilidad[[1L]]$indices_fila, c(31:40, 1:30)
  )
  expect_identical(
    hallazgos$trazabilidad[[2L]]$indices_fila, c(36:40, 1:35)
  )
})

test_that("la traza no nombra filas de fuera del universo declarado", {
  # El universo son las 40 primeras filas; las otras 40 estan escritas con
  # punto, un tercer formato que el perfil no cuenta.
  datos <- data.frame(
    tipo = rep(c("A", "B"), each = 40),
    fecha = c(
      rep("2024-01-15", 30), rep("15/01/2024", 10), rep("15.01.2024", 40)
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar_fechas(datos, aplicabilidad = list(fecha = ~ tipo == "A"))
  traza <- traza_de(perfil, "formatos_fecha_mixtos")
  expect_identical(traza$indices_fila, c(31:40, 1:30))
  expect_equal(traza$total, 40)
  expect_equal(traza$n_filas_formas_variantes, 10)
})

test_that("el tope corta primero la escritura dominante", {
  pocas <- fechas(rep("2024-01-15", 5000), rep("15/01/2024", 300))
  traza <- traza_de(perfilar_fechas(pocas), "formatos_fecha_mixtos")
  expect_identical(traza$estado, "truncada")
  expect_true(traza$truncado)
  expect_equal(traza$total, 5300)
  expect_identical(traza$limite, 1000L)
  # Entran las 300 variantes enteras y 700 de las 5.000 dominantes.
  expect_identical(traza$indices_fila, c(5001:5300, 1:700))
  expect_equal(traza$mostrados_formas_variantes, 300)
  expect_equal(traza$mostrados_formas_dominantes, 700)

  muchas <- fechas(rep("2024-01-15", 5000), rep("15/01/2024", 1500))
  traza <- traza_de(perfilar_fechas(muchas), "formatos_fecha_mixtos")
  expect_identical(traza$indices_fila, 5001:6000)
  expect_equal(traza$n_filas_formas_variantes, 1500)
  expect_equal(traza$mostrados_formas_variantes, 1000)
  expect_equal(traza$mostrados_formas_dominantes, 0)

  traza <- traza_de(
    perfilar_fechas(muchas, max_filas_hallazgo = Inf), "formatos_fecha_mixtos"
  )
  expect_identical(traza$estado, "disponible")
  expect_identical(traza$limite, Inf)
  expect_identical(traza$indices_fila, c(5001:6500, 1:5000))
  expect_equal(traza$mostrados_formas_variantes, 1500)
  expect_equal(traza$mostrados_formas_dominantes, 5000)

  traza <- traza_de(
    perfilar_fechas(muchas, max_filas_hallazgo = 1), "formatos_fecha_mixtos"
  )
  expect_identical(traza$estado, "truncada")
  expect_identical(traza$indices_fila, 5001L)
  expect_equal(traza$total, 6500)
})

test_that("con clave declarada cada indice mostrado lleva su clave", {
  valores <- c(rep("2024-01-15", 30), rep("15/01/2024", 10))
  simple <- data.frame(
    id = sprintf("K%03d", 1:40), fecha = valores, stringsAsFactors = FALSE
  )
  traza <- traza_de(perfilar_fechas(simple, clave = "id"),
                    "formatos_fecha_mixtos")
  expect_identical(traza$localizador, "clave_declarada")
  expect_identical(names(traza$claves), "id")
  # Una fila por indice, en el mismo orden: la primera es la fila 31.
  expect_identical(traza$claves$id, simple$id[traza$indices_fila])
  expect_identical(traza$claves$id[[1L]], "K031")

  compuesta <- data.frame(
    a = rep(1:20, 2), b = rep(c("x", "y"), each = 20), fecha = valores,
    stringsAsFactors = FALSE
  )
  traza <- traza_de(perfilar_fechas(compuesta, clave = c("a", "b")),
                    "formatos_fecha_mixtos")
  expect_identical(names(traza$claves), c("a", "b"))
  expect_identical(traza$claves$a, compuesta$a[traza$indices_fila])
  expect_identical(traza$claves$b, compuesta$b[traza$indices_fila])
})

test_that("con mas filas que la muestra la traza es de la columna entera", {
  # La muestra de 100 sobre 1.000 filas ve una sola de las diez minoritarias
  # (991 a 1000). La tabla de formatos y la evidencia son de la muestra, y lo
  # dicen; las filas son de la tabla que se perfilo, como en cualquier traza.
  datos <- fechas(rep("2024-01-15", 990), rep("15/01/2024", 10))
  perfil <- perfilar_fechas(datos, muestra = 100)
  tabla <- perfil$formatos_fecha$fecha
  expect_true(attr(tabla, "muestreado"))
  expect_identical(tabla$n, c(99L, 1L))
  hallazgo <- hallazgos_de(perfil, "formatos_fecha_mixtos")
  expect_match(hallazgo$evidencia, "muestra de 100", fixed = TRUE)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_identical(traza$alcance, "completo")
  expect_equal(traza$total, 1000)
  expect_identical(traza$indices_fila, c(991:1000, 1:990))
  expect_equal(traza$n_filas_formas_variantes, 10)
  expect_equal(traza$n_filas_formas_dominantes, 990)
})

test_that("las filas recogidas coinciden con lo que la deteccion cuenta", {
  # El reparto por fila sale de la misma deteccion que cuenta: por formato,
  # las filas recogidas son su `n` sobre la columna entera, y las filas con
  # fecha son las `compatibles`. Un banco con todas las ramas: confirmados,
  # grupos ambiguos, lecturas probadas, anio corto, meses escritos y ruido.
  set.seed(4821)
  piezas <- list(
    iso = "2024-03-17", barra_dia = "25/04/2021", barra_mes = "04/25/2021",
    ambigua = "03/04/2021", corta_dia = "25/12/99", corta_ambigua = "01/02/99",
    guion = "01-02-03", guion_anio = "95-12-25", mes = "5 de marzo de 2021",
    mes_corto = "5 de marzo de 21", punto = "17.03.2024",
    hora = "2024-03-17 10:30:00", ruido = "pendiente", vacio = ""
  )
  for (sorteo in seq_len(25L)) {
    elegidas <- sample(names(piezas), sample(2:5, 1L))
    valores <- sample(
      unlist(piezas[elegidas], use.names = FALSE), 120L, replace = TRUE
    )
    valores[sample(120L, 6L)] <- NA
    etiqueta <- paste(elegidas, collapse = "+")
    tabla <- detectar_formatos_fecha(valores, muestra = Inf)
    asignacion <- .filas_formatos_fecha(.texto_en_el_universo(valores))
    if (is.null(asignacion)) {
      expect_equal(nrow(tabla), 0L, info = etiqueta)
      next
    }
    recogidas <- vapply(tabla$formato, function(formato) {
      length(.filas_de_formatos_fecha(asignacion, formato))
    }, integer(1L), USE.NAMES = FALSE)
    expect_identical(recogidas, tabla$n, info = etiqueta)
    # Con varios formatos se suman las celdas que solo ellos leen: nunca menos
    # que lo que cuenta cada uno, y nunca mas que las filas con fecha.
    corto <- tabla$formato[tabla$anio_dos_digitos]
    if (length(corto)) {
      filas_corto <- .filas_de_formatos_fecha(asignacion, corto)
      por_formato <- unique(unlist(lapply(corto, function(formato) {
        .filas_de_formatos_fecha(asignacion, formato)
      })))
      expect_true(all(por_formato %in% filas_corto), info = etiqueta)
      expect_lte(length(filas_corto), attr(tabla, "compatibles"))
    }
    reparto <- .reparto_filas_formatos_fecha(asignacion)
    con_fecha <- c(reparto$variantes, reparto$dominantes)
    expect_equal(length(con_fecha), attr(tabla, "compatibles"), info = etiqueta)
    expect_false(anyDuplicated(con_fecha) > 0L, info = etiqueta)
    # Y lo mismo por la salida publica, cuando el hallazgo sale.
    perfil <- perfilar_fechas(fechas(valores))
    mixtos <- hallazgos_de(perfil, "formatos_fecha_mixtos")
    if (nrow(mixtos)) {
      traza <- mixtos$trazabilidad[[1L]]
      expect_equal(traza$total, attr(tabla, "compatibles"), info = etiqueta)
      expect_setequal(traza$indices_fila, con_fecha)
      expect_equal(
        traza$n_filas_formas_variantes + traza$n_filas_formas_dominantes,
        traza$total, info = etiqueta
      )
    }
  }
})

test_that("la asignacion de formas no se publica", {
  # Es un intermedio de la deteccion, como el lector de meses escritos.
  valores <- tres_escrituras()$fecha
  expect_null(attr(detectar_formatos_fecha(valores), "asignacion"))
  expect_null(attr(inferir_tipo(valores)$formatos_fecha, "asignacion"))
  perfil <- perfilar_fechas(tres_escrituras())
  expect_null(attr(perfil$formatos_fecha$fecha, "asignacion"))
  expect_setequal(
    names(attributes(perfil$formatos_fecha$fecha)),
    c("names", "row.names", "class", "total", "analizados", "muestreado",
      "compatibles", "formatos_mixtos", "zona_horaria_origen",
      "n_filas_fecha_civil_distinta_utc")
  )
})

test_that("la segunda deteccion se paga solo si el hallazgo sale, una vez por columna", {
  llamadas <- 0L
  original <- .filas_formatos_fecha
  local_mocked_bindings(
    .filas_formatos_fecha = function(texto) {
      llamadas <<- llamadas + 1L
      original(texto)
    },
    .package = "lupa"
  )
  # Sin fechas, con fechas tipadas y con fechas de texto en un solo formato:
  # ningun hallazgo de formato, ninguna deteccion de mas.
  perfilar_fechas(data.frame(
    n = 1:60, texto = rep(c("a", "b", "c"), 20),
    tipada = as.Date("2024-01-01") + 0:59,
    un_formato = format(as.Date("2024-01-01") + 0:59),
    stringsAsFactors = FALSE
  ))
  expect_identical(llamadas, 0L)
  # Tres hallazgos sobre la misma columna: una deteccion, no tres.
  perfilar_fechas(tres_escrituras())
  expect_identical(llamadas, 1L)
  # Dos columnas con hallazgo: una por columna.
  dos <- tres_escrituras()
  dos$otra <- rev(dos$fecha)
  perfilar_fechas(dos)
  expect_identical(llamadas, 3L)
})

test_that("la proteccion conserva los indices y tapa las claves personales", {
  set.seed(7)
  n <- 120L
  cedula <- make.unique(sprintf(
    "%d.%03d.%03d-%d", sample(1:5, n, TRUE), sample(0:999, n, TRUE),
    sample(0:999, n, TRUE), sample(0:9, n, TRUE)
  ))
  datos <- data.frame(
    cedula = cedula,
    fecha = c(rep("1980-05-17", 80), rep("17/05/1980", 30), rep("03/04/80", 10)),
    stringsAsFactors = FALSE
  )
  perfilar_con <- function(proteger) {
    perfilar_fechas(
      datos, clave = "cedula", proteger_datos_personales = proteger,
      columnas_personales = c(cedula = "documento", fecha = "fecha_nacimiento")
    )
  }
  abierto <- perfilar_con(FALSE)
  protegido <- perfilar_con(TRUE)
  for (tipo in tipos_formato_fecha) {
    a <- traza_de(abierto, tipo)
    p <- traza_de(protegido, tipo)
    expect_identical(p$indices_fila, a$indices_fila, info = tipo)
    expect_identical(p$total, a$total, info = tipo)
    expect_identical(p$estado, "disponible", info = tipo)
    # En el perfil abierto la clave es la cedula de cada fila; en el
    # protegido no queda ninguna.
    expect_identical(a$claves$cedula, datos$cedula[a$indices_fila], info = tipo)
    expect_false(any(p$claves$cedula %in% datos$cedula), info = tipo)
    expect_identical(p$claves_protegidas, "cedula", info = tipo)
  }
  expect_identical(
    traza_de(protegido, "formatos_fecha_mixtos")$n_filas_formas_variantes,
    traza_de(abierto, "formatos_fecha_mixtos")$n_filas_formas_variantes
  )
  # Ningun valor de celda en la traza protegida.
  texto <- unlist(lapply(
    protegido$hallazgos$trazabilidad, function(traza) {
      as.character(unlist(traza, use.names = FALSE))
    }
  ), use.names = FALSE)
  for (valor in c(datos$cedula[c(1L, 81L, 111L)],
                  "1980-05-17", "17/05/1980", "03/04/80")) {
    expect_false(any(grepl(valor, texto, fixed = TRUE, useBytes = TRUE)),
                 info = valor)
  }
})

test_that("por grupo los indices son de la tabla original", {
  base <- tres_escrituras()
  datos <- rbind(base, base)
  datos$grupo <- rep(c("norte", "sur"), each = 200L)
  # Intercalados: el grupo norte queda en las filas impares.
  datos <- datos[rep(1:200, each = 2L) + rep(c(0L, 200L), 200L), ]
  rownames(datos) <- NULL
  datos$id <- sprintf("K%03d", seq_len(nrow(datos)))
  por_grupo <- perfilar_por(
    datos, por = "grupo", clave = "id", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  for (grupo in c("norte", "sur")) {
    fila <- which(por_grupo$grupo == grupo &
                    por_grupo$tipo_hallazgo == "formatos_fecha_mixtos")
    expect_length(fila, 1L)
    traza <- por_grupo$trazabilidad[[fila]]
    del_grupo <- which(datos$grupo == grupo)
    minoritarias <- which(datos$grupo == grupo & datos$fecha != "2024-01-15")
    expect_identical(
      traza$indices_fila,
      c(minoritarias, setdiff(del_grupo, minoritarias)), info = grupo
    )
    expect_identical(traza$claves$id, datos$id[traza$indices_fila], info = grupo)
    expect_equal(traza$n_filas_formas_variantes, 50, info = grupo)
    ambiguas <- which(por_grupo$grupo == grupo &
                        por_grupo$tipo_hallazgo == "formato_fecha_ambiguo")
    expect_identical(
      por_grupo$trazabilidad[[ambiguas]]$indices_fila,
      which(datos$grupo == grupo & datos$fecha == "01/02/24"), info = grupo
    )
  }
})

test_that("por DBI la tabla completa trae filas y la muestra del motor las retiene", {
  skip_if_not_installed("DBI")
  datos <- data.frame(
    id = 1:600,
    fecha = c(rep("2024-01-15", 450), rep("15/01/2024", 100), rep("01/02/24", 50)),
    stringsAsFactors = FALSE
  )
  motores <- list()
  if (requireNamespace("duckdb", quietly = TRUE)) {
    motores$duckdb <- function() DBI::dbConnect(duckdb::duckdb(), ":memory:")
  }
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    motores$sqlite <- function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  skip_if(!length(motores), "sin duckdb ni RSQLite")
  for (motor in names(motores)) {
    con <- motores[[motor]]()
    DBI::dbWriteTable(con, "t", datos)
    completa <- suppressMessages(perfilar_dbi(con, "t"))
    expect_length(detalle_de_la_guarda(h <- hallazgos(completa)), 0L)
    mixtos <- h$trazabilidad[[which(h$tipo_hallazgo == "formatos_fecha_mixtos")]]
    expect_identical(mixtos$estado, "disponible", info = motor)
    expect_identical(mixtos$indices_fila, c(451:600, 1:450), info = motor)
    ambiguo <- h$trazabilidad[[which(h$tipo_hallazgo == "formato_fecha_ambiguo")]]
    expect_identical(ambiguo$indices_fila, 551:600, info = motor)

    # Con la muestra elegida por el motor el orden no es estable: la traza se
    # retiene, y conserva lo que no nombra filas.
    muestra <- suppressMessages(perfilar_dbi(
      con, "t", universo = "muestra_motor", muestra_motor = 300
    ))
    expect_length(detalle_de_la_guarda(h <- hallazgos(muestra)), 0L)
    # Con 450 / 100 / 50, una muestra de 300 que traiga un solo formato es
    # imposible en la practica: el hallazgo sale siempre.
    i <- which(h$tipo_hallazgo == "formatos_fecha_mixtos")
    expect_length(i, 1L)
    retenida <- h$trazabilidad[[i]]
    expect_identical(retenida$estado, "no_disponible", info = motor)
    expect_identical(retenida$alcance, "orden_muestra_no_estable", info = motor)
    expect_length(retenida$indices_fila, 0L)
    expect_equal(retenida$total, 300, info = motor)
    expect_equal(
      retenida$n_filas_formas_variantes + retenida$n_filas_formas_dominantes,
      300, info = motor
    )
    if (identical(motor, "duckdb")) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }
})

# Un perfil como lo guardaba la version anterior: los mismos hallazgos, con la
# traza `no_aplica` en los tres de formato de fecha.
como_la_version_anterior <- function(perfil) {
  de_formato <- which(
    as.character(perfil$hallazgos$tipo_hallazgo) %in% tipos_formato_fecha
  )
  for (i in de_formato) {
    perfil$hallazgos$trazabilidad[[i]] <- .trazabilidad_vacia(
      "no_aplica", alcance = "no_aplica"
    )
  }
  perfil
}

test_that("un perfil de la version anterior se publica y se compara sin avisos", {
  datos <- tres_escrituras()
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  nuevo <- perfilar_fechas(datos, fecha = fecha, nombre = "t")
  viejo <- como_la_version_anterior(nuevo)
  expect_length(detalle_de_la_guarda(hallazgos(viejo)), 0L)
  expect_length(detalle_de_la_guarda(salida_cli(print(viejo))), 0L)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_length(detalle_de_la_guarda(suppressMessages(
    reportar(viejo, archivo = archivo, sobrescribir = TRUE, fecha = fecha)
  )), 0L)
  expect_true(file.exists(archivo))
  # La deriva identifica un hallazgo por columna y tipo: que ahora traiga filas
  # no es un cambio en los datos.
  expect_equal(nrow(comparar_perfiles(viejo, nuevo)), 0L)
  expect_equal(nrow(comparar_perfiles(nuevo, viejo)), 0L)
  # Y el plan no depende de la traza.
  expect_identical(
    planificar_limpieza(viejo, datos = datos),
    planificar_limpieza(nuevo, datos = datos)
  )
})

test_that("el informe muestra las filas de un hallazgo de formato como las de cualquier otro", {
  perfil <- perfilar_fechas(tres_escrituras())
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  expect_length(detalle_de_la_guarda(suppressMessages(
    reportar(perfil, archivo = archivo, sobrescribir = TRUE)
  )), 0L)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  filas <- regmatches(html, gregexpr("<tr>.*?</tr>", html, perl = TRUE))[[1L]]
  fila <- filas[grepl(">formatos_fecha_mixtos<", filas, fixed = TRUE,
                      useBytes = TRUE)]
  expect_length(fila, 1L)
  expect_match(fila, "filas mostradas&#61;200 de 200", fixed = TRUE)
})

test_that("la guarda acusa un hallazgo de formato que se construye sin filas", {
  datos <- tres_escrituras()
  perfil <- perfilar_fechas(datos)
  guarda <- function(hallazgos, con_datos) {
    detalle_de_la_guarda(if (con_datos) {
      .advertir_incoherencias_trazabilidad(hallazgos, datos, names(datos))
    } else {
      .advertir_incoherencias_trazabilidad(hallazgos)
    })
  }
  # El perfil intacto calla en los dos momentos.
  expect_length(guarda(perfil$hallazgos, con_datos = TRUE), 0L)
  expect_length(guarda(perfil$hallazgos, con_datos = FALSE), 0L)

  # `no_aplica` en un hallazgo de formato: asi lo guardaba la version
  # anterior, y al publicar se tolera. Al construir -con los datos delante- es
  # un hallazgo que cuenta celdas y no las nombra.
  sin_filas <- como_la_version_anterior(perfil)$hallazgos
  expect_length(guarda(sin_filas, con_datos = FALSE), 0L)
  expect_match(
    guarda(sin_filas, con_datos = TRUE), "hallazgo de formato sin filas",
    fixed = TRUE
  )

  i <- which(perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos")
  # La traza vacia con la que nace un hallazgo, sin reemplazar: se acusa
  # siempre.
  vacia <- perfil$hallazgos
  vacia$trazabilidad[[i]] <- .trazabilidad_vacia()
  expect_match(guarda(vacia, con_datos = TRUE), "traza no disponible",
               fixed = TRUE)
  expect_match(guarda(vacia, con_datos = FALSE), "traza no disponible",
               fixed = TRUE)

  # La retenida por la muestra del motor esta declarada en la cobertura: calla.
  retenida <- perfil$hallazgos
  retenida$trazabilidad[[i]] <- .traza_retenida_orden_inestable(
    perfil$hallazgos$trazabilidad[[i]]
  )
  expect_length(guarda(retenida, con_datos = FALSE), 0L)
  expect_length(guarda(retenida, con_datos = TRUE), 0L)

  # Un indice quitado a mano: 199 indices, total 200, sin truncar.
  recortada <- perfil$hallazgos
  recortada$trazabilidad[[i]]$indices_fila <-
    recortada$trazabilidad[[i]]$indices_fila[-1L]
  expect_match(
    guarda(recortada, con_datos = FALSE),
    "formatos_fecha_mixtos en fecha: total de traza no coincide", fixed = TRUE
  )

  # Truncada por el tope: 10 de 200 no es una incoherencia.
  truncada <- perfilar_fechas(datos, max_filas_hallazgo = 10)
  expect_identical(
    traza_de(truncada, "formatos_fecha_mixtos")$estado, "truncada"
  )
  expect_length(guarda(truncada$hallazgos, con_datos = TRUE), 0L)
  expect_length(guarda(truncada$hallazgos, con_datos = FALSE), 0L)
})

test_that("la guarda acusa un reparto que no suma el total de la traza", {
  datos <- tres_escrituras()
  perfil <- perfilar_fechas(datos)
  i <- which(perfil$hallazgos$tipo_hallazgo == "formatos_fecha_mixtos")
  roto <- perfil$hallazgos
  # 49 + 150 no son las 200 filas de la traza.
  roto$trazabilidad[[i]]$n_filas_formas_variantes <- 49L
  expect_match(
    detalle_de_la_guarda(.advertir_incoherencias_trazabilidad(roto)),
    "el reparto de la traza no suma su total", fixed = TRUE
  )
  # La mitad que no cambia: el reparto intacto calla.
  expect_length(
    detalle_de_la_guarda(.advertir_incoherencias_trazabilidad(perfil$hallazgos)),
    0L
  )

  # Vale para toda traza con reparto, no solo para las de fecha: la de un
  # hallazgo de vocabulario con las mismas dos cuentas.
  skip_if_not_installed("stringdist")
  ciudad <- data.frame(
    ciudad = c(rep("Montevideo", 280), rep("Montevido", 12)),
    stringsAsFactors = FALSE
  )
  vocabulario <- perfilar(
    ciudad, analizar_dependencias = FALSE, casi_duplicados_vocabulario = TRUE
  )
  j <- which(vocabulario$hallazgos$tipo_hallazgo == "casi_duplicados_vocabulario")
  expect_length(j, 1L)
  expect_equal(
    vocabulario$hallazgos$trazabilidad[[j]]$n_filas_formas_variantes, 12
  )
  expect_length(detalle_de_la_guarda(
    .advertir_incoherencias_trazabilidad(vocabulario$hallazgos, ciudad, "ciudad")
  ), 0L)
  roto <- vocabulario$hallazgos
  roto$trazabilidad[[j]]$n_filas_formas_dominantes <- 279L
  expect_match(
    detalle_de_la_guarda(
      .advertir_incoherencias_trazabilidad(roto, ciudad, "ciudad")
    ),
    "el reparto de la traza no suma su total", fixed = TRUE
  )
})

test_that("la guarda sigue exigiendo no_aplica a lo contado en columnas", {
  # `tipo_declarado_distinto` habla de la columna entera -texto que se lee
  # como fecha-, no de unas celdas.
  perfil <- perfilar_fechas(tres_escrituras())
  i <- which(perfil$hallazgos$tipo_hallazgo == "tipo_declarado_distinto")
  expect_length(i, 1L)
  expect_identical(
    as.character(perfil$hallazgos$unidad_conteo[[i]]), "columna"
  )
  expect_identical(perfil$hallazgos$trazabilidad[[i]]$estado, "no_aplica")
  alterado <- perfil$hallazgos
  alterado$trazabilidad[[i]] <- .trazabilidad_vacia()
  expect_match(
    detalle_de_la_guarda(.advertir_incoherencias_trazabilidad(alterado)),
    "falta no_aplica", fixed = TRUE
  )
})
