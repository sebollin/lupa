# El soporte de una proporcionalidad, la resolucion de cada columna medida en
# la respuesta, y las relaciones reales chicas que se descartaban sin decirlo.

prop_de <- function(d) {
  r <- lupa:::.detectar_aritmetica_columnas(d)
  h <- Filter(function(x) grepl("proporcional", x$descripcion[[1L]], fixed = TRUE),
              r$hallazgos)
  list(h = h, cobertura = r$cobertura, alcance = r$alcance)
}

test_that("los ceros que coinciden no sostienen una proporcionalidad", {
  x <- c(rep(0, 180), 1:20)
  y <- c(rep(0, 180), 101:120)
  expect_length(prop_de(data.frame(x = x, y = y))$h, 0L)
  x2 <- c(rep(0, 380), 1:20)
  y2 <- c(rep(0, 380), 101:120)
  expect_length(prop_de(data.frame(x = x2, y = y2))$h, 0L)
  # Ni un par que se repite: 180 filas en (1, 2) y 20 codigos sin relacion.
  set.seed(4)
  x3 <- c(rep(1, 180), sample(3:60, 20))
  y3 <- c(rep(2, 180), sample(61:200, 20))
  expect_length(prop_de(data.frame(x = x3, y = y3))$h, 0L)
  # Con codigos consecutivos, 3 de los 20 caen por azar dentro de la tolerancia.
  x4 <- c(rep(1, 180), 2:21)
  y4 <- c(rep(2, 180), 3:22)
  expect_length(prop_de(data.frame(x = x4, y = y4))$h, 0L)
})

test_that("un par sano repetido no calla una relacion real en el criterio", {
  # 300 filas exactas como decimales escritos: neto en pasos de 0,50 e iva en
  # 0,11 por paso; el par (50, 11) se repite 7 veces. Con 30 filas corridas un
  # centavo la relacion cumple justo 0.9: sin el par repetido quedaria en 0.898.
  j <- c(rep(100L, 7L), 101:393)
  neto <- as.numeric(sprintf("%d.%02d", j %/% 2L, (j %% 2L) * 50L))
  iva_cent <- j * 11L
  rotas <- 7L + as.integer(round(seq(5, 290, length.out = 30L)))
  iva_cent[rotas] <- iva_cent[rotas] + 1L
  iva <- as.numeric(sprintf("%d.%02d", iva_cent %/% 100L, iva_cent %% 100L))
  r <- prop_de(data.frame(neto = neto, iva = iva))
  expect_length(r$h, 1L)
  expect_equal(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), sort(rotas))
})

test_that("una relacion real con muchos ceros se sigue publicando", {
  set.seed(5)
  neto <- round(stats::runif(300, 1e7, 1e8), 2)
  neto[sample(300, 180)] <- 0
  iva <- round(neto * 0.22, 2)
  r <- prop_de(data.frame(neto = neto, iva = iva))
  expect_length(r$h, 1L)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  expect_match(r$h[[1L]]$descripcion, "iva ~= neto * 0.22", fixed = TRUE)
  # Una con ceros rota en dos filas con neto distinto de cero.
  iva[which(neto > 0)[c(3, 30)]] <- iva[which(neto > 0)[c(3, 30)]] + 0.01
  r <- prop_de(data.frame(neto = neto, iva = iva))
  expect_length(r$h, 1L)
  expect_equal(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), sort(which(neto > 0)[c(3, 30)]))
})

base_gruesa <- function(paso) {
  # 36 valores en pasos de `paso` y 4 a centavos; y = round(1.22 * x, 2).
  x <- c(1e6 + paso * (1:36), 1e6 + c(0.37, 12.41, 55.03, 81.99))
  y <- round(1.22 * x, 2)
  list(x = x, y = y)
}

test_that("con la base en una reticula mas gruesa, la respuesta a centavos se lee como respuesta", {
  sana <- base_gruesa(2.5)
  r <- prop_de(data.frame(x = sana$x, y = sana$y))
  expect_length(r$h, 1L)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 1.22", fixed = TRUE)
  rota <- base_gruesa(25)
  y <- rota$y
  y[c(5, 25, 35)] <- y[c(5, 25, 35)] + 0.01
  r <- prop_de(data.frame(x = rota$x, y = y))
  expect_length(r$h, 1L)
  expect_equal(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), c(5L, 25L, 35L))
})

test_that("las relaciones reales chicas con cuantos que no son potencia de diez se publican", {
  for (caso in list(c(22, 0.05, 1e7), c(24, 0.05, 1e7), c(20, 0.25, 1e7),
                    c(36, 0.25, 1e7), c(28, 0.5, 1e7), c(25, 5, 1e8))) {
    n <- caso[[1L]]; q <- caso[[2L]]
    x <- caso[[3L]] + round(seq(1, 100, length.out = n), 2)
    y <- round(round(x * 1.22 / q) * q, 2)
    r <- prop_de(data.frame(x = x, y = y))
    info <- paste(caso, collapse = " ")
    expect_length(r$h, 1L)
    if (length(r$h)) expect_equal(r$h[[1L]]$n_afectados, 0, info = info)
  }
})

test_that("un descarte por el cuanto deja su fila de cobertura", {
  set.seed(11)
  neto <- as.numeric(sprintf("%.2f", stats::runif(300, 1e9, 1e10)))
  iva <- as.numeric(sprintf("%.2f", neto * 0.22))
  j <- sample(300, 120)
  iva[j] <- as.numeric(sprintf("%.2f", iva[j] + 0.01))
  r <- prop_de(data.frame(neto = neto, iva = iva))
  expect_length(r$h, 0L)
  expect_equal(r$alcance$proporcionalidades_descartadas_cuanto, 1)
  cb <- r$cobertura[r$cobertura$diagnostico == "relacion_aritmetica_columnas", ]
  expect_equal(nrow(cb), 1L)
  expect_match(cb$motivo, "La tolerancia relativa reconoc", fixed = TRUE)
  expect_match(cb$motivo, "iva ~= neto *", fixed = TRUE)
  expect_setequal(cb$columnas[[1L]], c("neto", "iva"))
})

test_that("un par repetido no invierte la lectura ni alarga la constante", {
  # 34 de 50 filas en el par (1000, 1105) y 16 en y = round(1.105 * x, 2),
  # todas sanas (comprobado en centavos enteros). Las copias del par hacian
  # salir la mitad chica de `y` en pasos de 0,02: la columna no pasaba por de
  # decimales fijos y la relacion se leia al reves, `x ~= y * 0.90497737`, con
  # la fila 30 acusada.
  x_cent <- c(7160, 83724388, 44029, 35031862, 3189599, 145377, 9578923, 633774,
              676508, 20550682, 50082956, 3552371, 1182, 131990, 791361, 9035049)
  y_cent <- c(7912, 92515449, 48652, 38710208, 3524507, 160642, 10584710, 700320,
              747541, 22708504, 55341666, 3925370, 1306, 145849, 874454, 9983729)
  expect_true(all(abs(y_cent * 1000 - 1105 * x_cent) <= 500))
  filas <- c(3, 5, 7, 10, 12, 15, 21, 23, 26, 28, 30, 33, 35, 38, 39, 50)
  x <- rep(1000, 50)
  y <- rep(1105, 50)
  x[filas] <- x_cent / 100
  y[filas] <- y_cent / 100
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 1.105 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  # Con las 16 duplicadas la lectura al reves tambien salia, `ok` y larga.
  r <- prop_de(data.frame(x = c(x, x[filas]), y = c(y, y[filas])))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 1.105 ", fixed = TRUE)
})

test_that("una proporcionalidad sin soporte se cuenta en meta, sin fila de cobertura", {
  fecha <- as.POSIXct("2026-01-01", tz = "UTC")
  p <- perfilar(data.frame(x = c(rep(0, 180), 1:20), y = c(rep(0, 180), 101:120)),
                analizar_dependencias = FALSE, fecha = fecha)
  expect_false(any(p$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"))
  expect_equal(p$meta$aritmetica_columnas$proporcionalidades_descartadas_soporte, 1)
  expect_false(any(p$cobertura_diagnosticos$diagnostico == "relacion_aritmetica_columnas"))
  # Con soporte no cuenta: la misma tabla con la relacion en las 20 filas.
  p <- perfilar(data.frame(x = c(rep(0, 180), 1:20), y = c(rep(0, 180), 1:20 * 5)),
                analizar_dependencias = FALSE, fecha = fecha)
  expect_true(any(p$hallazgos$tipo_hallazgo == "relacion_aritmetica_columnas"))
  expect_equal(p$meta$aritmetica_columnas$proporcionalidades_descartadas_soporte, 0)
})

test_that("con la base en medios la respuesta se lee como respuesta, sin acusar sanas", {
  # 30 filas con x en medios e y = round(0.18 * x, 2), comprobadas en centavos;
  # la 24 rota (35,96 en vez de 35,91). `0.18 * x` cae en pasos de 0,09, y la
  # mitad de `y` sin la rota salia en 0,09: `y` no pasaba por de decimales fijos
  # y se leia `x ~= y * 5.55556`, que acusaba tambien la 30, sana.
  x_cent <- c(3768450, 3861150, 142650, 12050, 188750, 29707300, 1352550, 3850,
              1561650, 96200, 137450, 7789550, 3600, 1802150, 23799150, 121000,
              286700, 5732550, 35650, 6800, 16250, 6305700, 525250, 19950,
              10113950, 135400, 3280550, 7614200, 418900, 46331250)
  y_cent <- c(678321, 695007, 25677, 2169, 33975, 5347314, 243459, 693, 281097,
              17316, 24741, 1402119, 648, 324387, 4283847, 21780, 51606, 1031859,
              6417, 1224, 2925, 1135026, 94545, 3596, 1820511, 24372, 590499,
              1370556, 75402, 8339625)
  expect_true(all(x_cent %% 50 == 0))
  sana <- abs(100 * y_cent - 18 * x_cent) <= 50
  expect_identical(which(!sana), 24L)
  r <- prop_de(data.frame(x = x_cent / 100, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 0.18 ", fixed = TRUE)
  expect_identical(r$h[[1L]]$trazabilidad[[1L]]$indices_fila, 24L)
})

test_that("una constante larga no se traga las filas rotas de importes grandes", {
  # 15 filas en (1000, 1105) y 35 en y = round(1.105 * x, 2), cinco rotas por
  # un centavo. `1.10500001` dejaba adentro 47 filas contra 45 de `1.105` -las
  # rotas 32 y 37, de importes grandes- y se publicaba acusando tres de cinco.
  x_cent <- c(100000, 12141, 5991, 1082186, 466537, 3049, 1576839, 3887232,
              2475600, 110764, 47734, 100000, 4561, 3603, 100000, 46965, 100000,
              58867, 100000, 3039849, 1367822, 100000, 2325659, 100000, 100000,
              95670, 73774, 100000, 2213, 100000, 54544959, 93703880, 19916,
              364803, 218685, 1113, 44870014, 5800, 3257, 7736, 678244, 100000,
              100000, 60011769, 7382, 40215072, 100000, 39131971, 643562, 100000)
  y_cent <- c(110500, 13416, 6620, 1195816, 515523, 3370, 1742407, 4295391,
              2735538, 122394, 52747, 110500, 5040, 3981, 110500, 51896, 110500,
              65048, 110500, 3359033, 1511443, 110500, 2569854, 110500, 110500,
              105715, 81520, 110500, 2445, 110500, 60272180, 103542788, 22007,
              403107, 241647, 1230, 49581366, 6409, 3599, 8548, 749460, 110500,
              110500, 66313005, 8157, 44437655, 110500, 43240828, 711136, 110500)
  rotas <- which(abs(1000 * y_cent - 1105 * x_cent) > 500)
  expect_identical(rotas, c(6L, 11L, 23L, 32L, 37L))
  x <- x_cent / 100
  y <- y_cent / 100
  # La constante de las filas: la corta, que deja afuera las cinco.
  k <- lupa:::.constante_por_intervalos_aritmetica(x, y, 0.005, 1.105)
  expect_identical(as.vector(k), 1.105)
  # Publicada o no, una relacion no calla filas rotas.
  r <- prop_de(data.frame(x = x, y = y))
  for (h in r$h) expect_true(all(rotas %in% h$trazabilidad[[1L]]$indices_fila))
})

test_that("una constante real larga no se trunca para acusar los importes grandes", {
  # Un tipo de cambio de seis cifras sobre dolares con cuatro decimales, casi
  # todos chicos: `39.457` explica las 50 filas chicas (comprobado) y solo
  # falla la grande, y la holgura de una fila la elegia acusando la grande sana.
  u <- c(setdiff(seq(1000L, 9650L, by = 173L), 8266L), 25000037L)
  pesos_cent <- (394567 * u + 500000) %/% 1e6
  expect_true(all(abs(1e6 * pesos_cent - 394567 * u) < 500000))
  truncada <- abs(1e5 * pesos_cent - 39457 * u) <= 50000
  expect_identical(which(!truncada), length(u))
  r <- prop_de(data.frame(usd = u / 1e4, pesos = pesos_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "pesos ~= usd * 39.4567 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  # Con dolares a centavos la lectura de los dolares leia `0.0253`, tres cifras,
  # y acusaba las dos grandes.
  b_cent <- c(seq(11, 207, by = 4), 250037, 131071)
  pesos_cent <- (394567 * b_cent + 5000) %/% 10000
  expect_true(all(abs(10000 * pesos_cent - 394567 * b_cent) < 5000))
  r <- prop_de(data.frame(usd = b_cent / 100, pesos = pesos_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "pesos ~= usd * 39.4567 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("dos rotas grandes parecidas no compran una constante larga", {
  # 30 filas, y = round(k * x, dy) salvo 3 rotas por 3 cuantos de y. Con las
  # rotas entre las filas mayores, dos del mismo lado caian en el intervalo de
  # una constante larga (`0.21999992` contra `0.22`), y una excepcion por filas
  # lejanas la elegia: callaba dos rotas y acusaba una sana. Con `x` en medios,
  # la lectura de `x` -medio 0,5 de tolerancia- se tragaba las tres. `x` va en
  # centavos e `y` en unidades de su ultimo decimal; la sanidad, en enteros.
  casos <- list(
    # sesgada, x a 0,01, las 3 rotas son las mayores.
    list(k = "0.22", num = 11, den = 50, dy = 2L, rotas = c(11L, 13L, 25L),
         x = c(2935, 3643, 9381, 3646, 9959, 1261, 3619, 4298, 7402, 8333,
        74235316, 7979, 36027839, 1118, 9313, 9186, 5277, 8832, 9430,
        29026159, 6017, 2526, 210, 7412, 36289382, 7307, 3386, 320, 660,
        2820),
         y = c(646, 801, 2064, 802, 2191, 277, 796, 946, 1628, 1833, 16331773,
        1755, 7926122, 246, 2049, 2021, 1161, 1943, 2075, 6385755, 1324,
        556, 46, 1631, 7983661, 1608, 745, 70, 145, 620)),
    # sesgada, x entero, las 3 mayores.
    list(k = "0.0253", num = 253, den = 10000, dy = 2L, rotas = c(23L, 25L, 26L),
         x = c(9900, 3700, 4500, 6600, 5200, 7600, 4500, 4500, 5500, 16828300,
        9000, 5100, 9900, 4000, 1800, 5700, 1900, 1500, 4300, 9800, 8300,
        9600, 55810800, 5100, 75094200, 60924400, 4600, 6900, 6600, 200),
         y = c(250, 94, 114, 167, 132, 192, 114, 114, 139, 425756, 228, 129, 250,
        101, 46, 144, 48, 38, 109, 248, 210, 243, 1412010, 129, 1899880,
        1541390, 116, 175, 167, 5)),
    # sesgada, x entero, las 3 mayores.
    list(k = "0.22", num = 11, den = 50, dy = 2L, rotas = c(1L, 3L, 11L),
         x = c(90749500, 9800, 90811500, 9700, 65840000, 700, 5000, 1800, 9800,
        8000, 77853800, 8600, 400, 6600, 900, 6100, 7200, 7900, 200, 9000,
        3100, 9700, 3400, 4200, 9400, 5500, 3800, 7900, 2800, 4100),
         y = c(19964887, 2156, 19978527, 2134, 14484800, 154, 1100, 396, 2156,
        1760, 17127839, 1892, 88, 1452, 198, 1342, 1584, 1738, 44, 1980,
        682, 2134, 748, 924, 2068, 1210, 836, 1738, 616, 902)),
    # log, x a 0,01, las 3 mayores.
    list(k = "0.012345", num = 2469, den = 200000, dy = 2L, rotas = c(3L, 10L, 12L),
         x = c(15094565, 669859, 95827169, 37047, 8639912, 5767, 128817, 385606,
        134619, 22812771, 34213, 74670337, 364729, 3531, 1621702, 4550428,
        2280534, 1884, 101230, 10553, 63628, 1196, 7621, 8507, 6326, 1281,
        911876, 388924, 10940565, 499336),
         y = c(186342, 8269, 1182983, 457, 106660, 71, 1590, 4760, 1662, 281627,
        422, 921802, 4503, 44, 20020, 56175, 28153, 23, 1250, 130, 785, 15,
        94, 105, 78, 16, 11257, 4801, 135061, 6164)),
    # sesgada, x a 0,01, y a 4 decimales, 3 al azar.
    list(k = "0.22", num = 11, den = 50, dy = 4L, rotas = c(9L, 17L, 24L),
         x = c(9608, 4749, 9894, 8047, 7123, 5149, 52666078, 9857, 99112794, 7667,
        5606, 9288, 5059, 8045, 8656, 8627, 80913655, 5642, 6321, 5589,
        9068, 5181, 4523, 9596, 2770144, 7242, 9285, 8053, 3515, 2843003),
         y = c(211376, 104478, 217668, 177034, 156706, 113278, 1158653716, 216854,
        2180481471, 168674, 123332, 204336, 111298, 176990, 190432, 189794,
        1780100413, 124124, 139062, 122958, 199496, 113982, 99506, 211115,
        60943168, 159324, 204270, 177166, 77330, 62546066)),
    # log, x en medios, las 3 mayores.
    list(k = "39.4567", num = 394567, den = 10000, dy = 2L, rotas = c(4L, 18L, 29L),
         x = c(4780850, 1850450, 1800, 89448200, 4150, 424650, 2150, 9600, 18650,
        5150, 21670400, 570700, 574400, 2746550, 615800, 1532450, 515100,
        27641700, 10300, 982400, 12650, 264700, 7653100, 7650, 3200,
        133100, 284950, 2497150, 66763900, 12114250),
         y = c(188636564, 73012651, 71022, 3529330796, 163745, 16755288, 84832,
        378784, 735867, 203202, 855042472, 22517939, 22663928, 108369799,
        24297436, 60465420, 20324146, 1090650261, 406404, 38762262, 499127,
        10444188, 301966071, 301844, 126261, 5251687, 11243187, 98529298,
        2634283176, 477988328))
,
    # sesgada, x en medios, las 3 mayores, del mismo lado.
    list(k = "1.0825", num = 433, den = 400, dy = 2L, rotas = c(13L, 29L, 30L),
         x = c(5650, 8100, 2400, 1350, 2200, 9600, 3900, 6700, 3100, 1350, 6750,
        9150, 66420000, 2700, 3200, 1750, 2850, 650, 5300, 5450, 65388450,
        8900, 1150, 6900, 1600, 8900, 3500, 8150, 78961950, 78332500),
         y = c(6116, 8768, 2598, 1461, 2382, 10392, 4222, 7253, 3356, 1461, 7307,
        9905, 71899653, 2923, 3464, 1894, 3085, 704, 5737, 5900, 70782997,
        9634, 1245, 7469, 1732, 9634, 3789, 8822, 85476314, 84794934)),
    # angosta, x entero, y a 4 decimales, las 3 mayores.
    list(k = "3.75", num = 15, den = 4, dy = 4L, rotas = c(36L, 158L, 168L),
         x = c(62300, 72400, 18700, 78100, 32000, 11300, 11400, 19200, 64800,
        13800, 78000, 26600, 89500, 14600, 93100, 30800, 13100, 38900,
        37000, 18300, 48900, 77800, 83000, 98400, 14600, 44500, 26800,
        41000, 12200, 10700, 21700, 76700, 73000, 47700, 70100, 99000,
        70700, 64900, 56500, 91200, 47900, 42800, 38600, 51400, 54900,
        72700, 49100, 49200, 93400, 25300, 54100, 18000, 21400, 93000,
        42000, 54300, 24300, 81700, 85600, 62000, 89500, 17400, 42100,
        73800, 58100, 96200, 63500, 35800, 30500, 42600, 57800, 23100,
        41700, 65900, 81700, 77200, 81500, 39700, 20300, 91800, 79400,
        34100, 90200, 33300, 46000, 86400, 49300, 57100, 79600, 75900,
        90700, 45900, 38700, 41200, 71500, 67600, 75200, 85600, 32100,
        56800, 66700, 17500, 66200, 95300, 17300, 54100, 51800, 62000,
        85400, 75700, 98900, 33900, 73500, 64600, 32100, 74100, 53600,
        46900, 30500, 17100, 61400, 56600, 34700, 13000, 16500, 66700,
        76000, 84500, 91600, 85600, 55400, 74900, 70300, 21400, 64100,
        82200, 38300, 66800, 37400, 98500, 32500, 60200, 50500, 47700,
        90700, 22300, 79300, 16700, 38900, 29900, 27900, 17100, 67600,
        85700, 55900, 28900, 46400, 99700, 60800, 95800, 72500, 28600,
        81200, 59300, 21400, 42200, 42400, 99600, 59900, 81600, 11800,
        47600, 30500, 81300, 77600, 88200, 74900, 97800, 47100, 40400,
        87100, 13200, 68000, 56100, 89600, 47300, 59200, 23100, 37300,
        47700, 52000, 78000, 72200, 61500, 70400, 69600, 45500, 97700,
        89200, 73800, 78600, 41200, 78600, 33400, 58500, 29600, 62000,
        57700, 70600, 69400, 23400, 50000, 31300, 11900, 61100, 30000,
        74400, 75300, 18300, 44000, 81200, 41400, 78000, 87800, 21200,
        66200, 51300, 92500, 93800, 46500, 73300, 71500, 71700, 51900,
        43600, 75200, 86600, 71700, 27000, 72600, 76700, 28500, 92000,
        19000, 37500, 53900, 83400, 84500, 73300, 78200, 73000, 36900,
        35200, 45300, 66900, 74600, 55400, 27000, 81100, 21400, 58500,
        38700, 64300, 73000, 25900, 68500, 35900, 37600, 96800, 68000,
        47400, 15100, 68300, 21200, 55500, 54900, 37900, 27400, 99000,
        22300, 52200, 58200, 87800, 25500, 43600, 63900, 42900, 58500,
        35600, 32000, 13200, 56500, 22900, 17100, 36400, 64600, 18600,
        43200, 95900, 27400),
         y = c(23362500, 27150000, 7012500, 29287500, 12000000, 4237500, 4275000,
        7200000, 24300000, 5175000, 29250000, 9975000, 33562500, 5475000,
        34912500, 11550000, 4912500, 14587500, 13875000, 6862500, 18337500,
        29175000, 31125000, 36900000, 5475000, 16687500, 10050000,
        15375000, 4575000, 4012500, 8137500, 28762500, 27375000, 17887500,
        26287500, 37124997, 26512500, 24337500, 21187500, 34200000,
        17962500, 16050000, 14475000, 19275000, 20587500, 27262500,
        18412500, 18450000, 35025000, 9487500, 20287500, 6750000, 8025000,
        34875000, 15750000, 20362500, 9112500, 30637500, 32100000,
        23250000, 33562500, 6525000, 15787500, 27675000, 21787500,
        36075000, 23812500, 13425000, 11437500, 15975000, 21675000,
        8662500, 15637500, 24712500, 30637500, 28950000, 30562500,
        14887500, 7612500, 34425000, 29775000, 12787500, 33825000,
        12487500, 17250000, 32400000, 18487500, 21412500, 29850000,
        28462500, 34012500, 17212500, 14512500, 15450000, 26812500,
        25350000, 28200000, 32100000, 12037500, 21300000, 25012500,
        6562500, 24825000, 35737500, 6487500, 20287500, 19425000, 23250000,
        32025000, 28387500, 37087500, 12712500, 27562500, 24225000,
        12037500, 27787500, 20100000, 17587500, 11437500, 6412500,
        23025000, 21225000, 13012500, 4875000, 6187500, 25012500, 28500000,
        31687500, 34350000, 32100000, 20775000, 28087500, 26362500,
        8025000, 24037500, 30825000, 14362500, 25050000, 14025000,
        36937500, 12187500, 22575000, 18937500, 17887500, 34012500,
        8362500, 29737500, 6262500, 14587500, 11212500, 10462500, 6412500,
        25350000, 32137500, 20962500, 10837500, 17400000, 37387497,
        22800000, 35925000, 27187500, 10725000, 30450000, 22237500,
        8025000, 15825000, 15900000, 37349997, 22462500, 30600000, 4425000,
        17850000, 11437500, 30487500, 29100000, 33075000, 28087500,
        36675000, 17662500, 15150000, 32662500, 4950000, 25500000,
        21037500, 33600000, 17737500, 22200000, 8662500, 13987500,
        17887500, 19500000, 29250000, 27075000, 23062500, 26400000,
        26100000, 17062500, 36637500, 33450000, 27675000, 29475000,
        15450000, 29475000, 12525000, 21937500, 11100000, 23250000,
        21637500, 26475000, 26025000, 8775000, 18750000, 11737500, 4462500,
        22912500, 11250000, 27900000, 28237500, 6862500, 16500000,
        30450000, 15525000, 29250000, 32925000, 7950000, 24825000,
        19237500, 34687500, 35175000, 17437500, 27487500, 26812500,
        26887500, 19462500, 16350000, 28200000, 32475000, 26887500,
        10125000, 27225000, 28762500, 10687500, 34500000, 7125000,
        14062500, 20212500, 31275000, 31687500, 27487500, 29325000,
        27375000, 13837500, 13200000, 16987500, 25087500, 27975000,
        20775000, 10125000, 30412500, 8025000, 21937500, 14512500,
        24112500, 27375000, 9712500, 25687500, 13462500, 14100000,
        36300000, 25500000, 17775000, 5662500, 25612500, 7950000, 20812500,
        20587500, 14212500, 10275000, 37125000, 8362500, 19575000,
        21825000, 32925000, 9562500, 16350000, 23962500, 16087500,
        21937500, 13350000, 12000000, 4950000, 21187500, 8587500, 6412500,
        13650000, 24225000, 6975000, 16200000, 35962500, 10275000))
  )
  for (cs in casos) {
    # |y - k x| <= medio cuanto de y, en enteros: y * den * 100 contra num * x * 10^dy.
    desvio <- abs(cs$y * cs$den * 100 - cs$num * cs$x * 10^cs$dy)
    expect_identical(which(desvio > cs$den * 50), cs$rotas, info = cs$k)
    r <- prop_de(data.frame(x = cs$x / 100, y = cs$y / 10^cs$dy))
    expect_length(r$h, 1L)
    expect_match(r$h[[1L]]$descripcion, paste0("y ~= x * ", cs$k, " "), fixed = TRUE,
                 info = cs$k)
    expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), cs$rotas,
                     info = cs$k)
  }
})

test_that("con cotas parecidas gana la lectura que explica mas filas", {
  # 30 filas sanas: x a centavos, casi todas chicas y tres grandes, e
  # y = 3.75 * x exacto a cuatro decimales. La cota de las dos lecturas es la
  # misma en la misma columna; `x ~= y * 0.26666` dejaba afuera las tres
  # grandes y ganaba por ser la directa.
  x <- c(799, 4230, 69909390, 4676, 710, 9839, 9694, 8236, 5804, 5858, 5739,
         16962173, 5181, 8106, 4476, 8334, 1133, 7537, 8464, 237, 8300,
         2869, 5221, 9461, 7245, 9505253, 520, 2850, 6477, 4206)
  y <- c(299625, 1586250, 26216021250, 1753500, 266250, 3689625, 3635250,
         3088500, 2176500, 2196750, 2152125, 6360814875, 1942875, 3039750,
         1678500, 3125250, 424875, 2826375, 3174000, 88875, 3112500,
         1075875, 1957875, 3547875, 2716875, 3564469875, 195000, 1068750,
         2428875, 1577250)
  expect_true(all(y == 375 * x))
  r <- prop_de(data.frame(x = x / 100, y = y / 1e4))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 3.75 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("las cifras de la lectura gruesa se cuentan en la fina", {
  # x entero, y = round(0.012345 * x, 2) y las tres mayores rotas por 3
  # centavos. La lectura de x, con medio entero de tolerancia, leia `81.004`:
  # cinco cifras, como `0.012345`, pero en la lectura de y es `0.01234507`,
  # siete. Contada con cinco ganaba por una fila -una rota grande que se
  # tragaba- y acusaba una sana. `x` va en centavos e `y` en unidades de su
  # ultimo decimal; la sanidad, en enteros.
  casos <- list(
    # log, x entero, las 3 mayores.
    list(k = "0.012345", num = 2469, den = 200000, dy = 2L, rotas = c(10L, 20L, 27L),
         x = c(1811800, 11200, 2282400, 2300, 3873200, 404400, 171600, 50100,
        193800, 22037300, 118800, 317800, 2700, 174800, 2769300, 399800,
        4400, 20607600, 460000, 33586500, 29300, 64000, 3200, 10900, 16000,
        290300, 40260700, 2400, 67100, 2372800),
         y = c(22367, 138, 28176, 28, 47815, 4992, 2118, 618, 2392, 272047, 1467,
        3923, 33, 2158, 34187, 4936, 54, 254401, 5679, 414628, 362, 790,
        40, 135, 198, 3584, 497021, 30, 828, 29292))
  )
  for (cs in casos) {
    desvio <- abs(cs$y * cs$den * 100 - cs$num * cs$x * 10^cs$dy)
    expect_identical(which(desvio > cs$den * 50), cs$rotas, info = cs$k)
    r <- prop_de(data.frame(x = cs$x / 100, y = cs$y / 10^cs$dy))
    expect_length(r$h, 1L)
    expect_match(r$h[[1L]]$descripcion, paste0("y ~= x * ", cs$k, " "), fixed = TRUE,
                 info = cs$k)
    expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), cs$rotas,
                     info = cs$k)
  }
})

test_that("unas pocas rotas no tumban los decimales fijos de la respuesta", {
  # y a cuatro decimales cuyos valores sanos usan dos -x en medios o entero-, y
  # tres rotas por 3 diezmilesimos: las unicas filas con cuatro decimales. La
  # mitad con las rotas salia en 0,0003 o 0,0001 contra 0,36 o 0,11 de la otra,
  # y la resolucion de casi todos contaba tres de 27 valores distintos. La
  # respuesta no pasaba por de decimales fijos y quedaba la lectura gruesa de
  # x, que se tragaba las rotas o acusaba una sana.
  casos <- list(
    # sesgada, x en medios, 3 al azar.
    list(k = "0.22", num = 11, den = 50, dy = 4L, rotas = c(1L, 11L, 21L),
         x = c(2700, 4100, 5400, 5250, 2250, 8300, 6200, 64140700, 6100, 5750,
        3250, 8600, 5950, 4500, 5500, 9650, 9350, 5400, 250, 950, 4350,
        4400, 9800, 4500, 2350, 7350, 7000, 5250, 5000, 6850),
         y = c(59403, 90200, 118800, 115500, 49500, 182600, 136400, 1411095400,
        134200, 126500, 71497, 189200, 130900, 99000, 121000, 212300,
        205700, 118800, 5500, 20900, 95703, 96800, 215600, 99000, 51700,
        161700, 154000, 115500, 110000, 150700)),
    # x entero, las 3 mayores.
    list(k = "0.36", num = 9, den = 25, dy = 4L, rotas = c(4L, 5L, 9L),
         x = c(29000, 2800, 334500, 50398200, 79677300, 1100, 5560800, 92800,
        7416000, 1555600, 5870200, 4781700, 875700, 27500, 12000, 569800,
        6600, 6835300, 99100, 8100, 1558800, 235100, 432800, 2304200,
        48100, 8400, 6174700, 44100, 6400, 110700),
         y = c(1044000, 100800, 12042000, 1814335203, 2868382803, 39600,
        200188800, 3340800, 266975997, 56001600, 211327200, 172141200,
        31525200, 990000, 432000, 20512800, 237600, 246070800, 3567600,
        291600, 56116800, 8463600, 15580800, 82951200, 1731600, 302400,
        222289200, 1587600, 230400, 3985200))
  )
  for (cs in casos) {
    desvio <- abs(cs$y * cs$den * 100 - cs$num * cs$x * 10^cs$dy)
    expect_identical(which(desvio > cs$den * 50), cs$rotas, info = cs$k)
    r <- prop_de(data.frame(x = cs$x / 100, y = cs$y / 10^cs$dy))
    expect_length(r$h, 1L)
    expect_match(r$h[[1L]]$descripcion, paste0("y ~= x * ", cs$k, " "), fixed = TRUE,
                 info = cs$k)
    expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), cs$rotas,
                     info = cs$k)
  }
})

test_that("una rota suelta no paga la cifra de mas", {
  # pesos = 100 * cant con una fila corrida 51: `0.0099` explicaba las 20 filas
  # contra 19 de `0.01` -una cifra por una fila, empate- y se publicaba sin
  # filas acusadas. Una constante truncada falla primero las filas de mayor
  # magnitud; una rota suelta en el medio no compra cifras.
  for (caso in list(c(20, 5), c(50, 25), c(20, 1))) {
    cant <- seq_len(caso[[1L]])
    pesos <- 100 * cant
    pesos[caso[[2L]]] <- pesos[caso[[2L]]] + 51
    r <- prop_de(data.frame(cant = cant, pesos = pesos))
    expect_length(r$h, 1L)
    expect_match(r$h[[1L]]$descripcion, "cant ~= pesos * 0.01 ", fixed = TRUE)
    expect_identical(r$h[[1L]]$trazabilidad[[1L]]$indices_fila, as.integer(caso[[2L]]))
  }
  # Con la rota en la fila de mayor magnitud tambien se acusa: la corta explica
  # exactas todas las demas, y un truncado no deja residuos cero.
  cant <- 1:20
  pesos <- 100 * cant
  pesos[20] <- pesos[20] + 51
  r <- prop_de(data.frame(cant = cant, pesos = pesos))
  expect_length(r$h, 1L)
  expect_identical(r$h[[1L]]$trazabilidad[[1L]]$indices_fila, 20L)
  # Y la lectura reciproca de `y = round(0.18 * x, 2)` con x en medios explica
  # todas las filas, la mas grande tambien, con `1/0.18`, la forma corta.
  j <- c(75369, 77223, 2853, 241, 3775, 594146, 27051, 77, 31233, 1924, 2749,
         155791, 72, 36043, 475983, 2420, 5734, 114651, 713, 136, 325, 126114,
         10505, 399, 202279, 2708, 65611, 152284, 8378, 926625)
  x_cent <- 50 * j
  y_cent <- (18 * x_cent + 50) %/% 100
  expect_true(all(abs(100 * y_cent - 18 * x_cent) <= 50))
  k <- lupa:::.constante_por_intervalos_aritmetica(y_cent / 100, x_cent / 100,
                                                    0.25, 1 / 0.18)
  expect_identical(attr(k, "reciproca"), 0.18)
  expect_true(all(abs(x_cent / 100 - (y_cent / 100) * as.vector(k)) <= 0.25))
})

test_that("una grilla de 5 sobre importes grandes se lee aunque junte pocos valores", {
  # y = round(x * 0.22 / 5) * 5 sobre x = 1e9 + 1:100: cinco valores distintos de
  # y, y contados como cinco la reticula de 5 "se explicaba por azar". Son cien
  # registros distintos; las 34 copias de un mismo par siguen contando una.
  x <- 1e9 + 1:100
  y_q <- (22 * x + 250) %/% 500
  y <- 5 * y_q
  expect_true(all(abs(100 * y - 22 * x) <= 250))
  expect_length(unique(y), 5L)
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 0.22 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("entre los decimales del mismo largo, el del producto decimal", {
  # resp = round(cant * 0.0725, 2): el tramo contiene a `0.0724` y a `0.0725`, y
  # el redondeo de su centro elegia `0.0724`. Con `0.0725` cinco filas caen
  # justo en el empate del redondeo, como cae un producto decimal verdadero.
  cant <- 1:20
  resp_cent <- c(7, 14, 22, 29, 36, 43, 51, 58, 65, 72, 80, 87, 94, 101, 109,
                 116, 123, 130, 138, 145)
  expect_true(all(abs(4 * resp_cent - 29 * cant) <= 2))
  r <- prop_de(data.frame(cant = cant, resp = resp_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "resp ~= cant * 0.0725 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("las proporcionalidades redundantes sin soporte se cuentan", {
  # Una identidad y + z ~= x con 180 ceros: las dos proporcionalidades que
  # incluyen su total se omitian sin evaluarse y el contador quedaba en 0.
  x <- c(rep(0, 180), 1:20)
  y <- c(rep(0, 180), 2 * (1:20) + rep(c(0, 3), 10))
  p <- perfilar(data.frame(x = x, y = y, z = 2 * y), analizar_dependencias = FALSE,
                fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_equal(p$meta$aritmetica_columnas$proporcionalidades_descartadas_soporte, 2)
})

test_that("el empate paga una fila grande aunque no sea la mayor", {
  # y = round(0.012345 * x, 2) con x en medios, casi todas chicas, y las tres
  # mayores rotas. `0.01235` empataba con `0.012347` dejando afuera la tercera
  # fila mas grande de las que esta explica: un truncado no siempre deja afuera
  # la mayor, depende de su residuo. Con la regla de la fila mas grande la
  # relacion no alcanzaba el criterio y se callaban las tres rotas.
  x_cent <- c(4800, 1000, 4400, 1300, 2150, 1250, 69828450, 3900, 8250, 9650,
              8800, 1450, 2800, 4600, 41236900, 6150, 300, 7750, 3700, 8300,
              5200, 3200, 5950, 3400, 1400, 4600, 8750, 1350, 450, 1750)
  y_cent <- c(59, 12, 54, 16, 27, 15, 862029, 48, 102, 122, 109, 18, 35, 57,
              509073, 76, 4, 96, 46, 102, 64, 40, 73, 42, 17, 57, 108, 17, 6, 22)
  rotas <- which(abs(200000 * y_cent - 2469 * x_cent) > 100000)
  expect_identical(rotas, c(7L, 10L, 15L))
  r <- prop_de(data.frame(x = x_cent / 100, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("la reciproca corta y exacta gana al decimal truncado", {
  # y = round(x / 7, 2) con x = 1:20: `1/7` explica las 20 filas con medio
  # centavo y `0.143` -tres cifras- dejaba afuera la 12 y la 19, sanas. En
  # centavos, |7 * y_cent - 100 * x| <= 3.5 es |y - x / 7| <= 0.005.
  x <- 1:20
  y_cent <- (100 * x + 3) %/% 7
  y_cent <- ifelse(abs(7 * (y_cent + 1) - 100 * x) < abs(7 * y_cent - 100 * x),
                   y_cent + 1, y_cent)
  expect_true(all(abs(7 * y_cent - 100 * x) <= 3.5))
  r <- prop_de(data.frame(x = x, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 7 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  # Un tercio con una sola fila grande: `1/3` y no `0.3334`, que acusaba la
  # grande sana; y con la grande corrida, se la acusa.
  x <- c(1:19, 1e6)
  y_cent <- (100 * x + 1) %/% 3
  expect_true(all(abs(3 * y_cent - 100 * x) <= 1.5))
  r <- prop_de(data.frame(x = x, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 3 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  y_cent[20] <- 33340000
  r <- prop_de(data.frame(x = x, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_identical(r$h[[1L]]$trazabilidad[[1L]]$indices_fila, 20L)
})

test_that("un redondeo al par en el medio cuanto no se acusa", {
  # y = round(0.9 * x) en enteros: R redondea al par, y las filas 5, 15, 25,
  # 35 y 45 quedan a 0,5 exacto de 0.9 * x, en el borde inclusivo. La lectura
  # de x, apenas mas fina, las dejaba a 0,56 y `x ~= y * 1.11` las acusaba.
  x <- 1:50
  y <- round(0.9 * x)
  expect_true(all(abs(10 * y - 9 * x) <= 5))
  expect_identical(which(abs(10 * y - 9 * x) == 5), c(5L, 15L, 25L, 35L, 45L))
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 0.9 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("una reciproca corta que no alcanza el criterio no compite", {
  # y = round(0.012345 * x, 2) con x de 100 a 1000 y tres rotas: `1/81` -dos
  # cifras y la division- explicaba 25 de 30 y le ganaba por cifras a
  # `0.012345`, que explica las 27 sanas, y la relacion se callaba.
  x_cent <- c(68140, 40056, 70453, 41450, 40984, 16081, 20666, 20036, 93120,
              88706, 21520, 14035, 52223, 11182, 98709, 88285, 59523, 89797,
              28458, 43822, 73222, 61617, 36627, 74087, 97931, 40595, 58702,
              23566, 63492, 12749)
  y_cent <- c(841, 494, 870, 512, 506, 199, 255, 247, 1150, 1095, 266, 173, 645,
              141, 1219, 1090, 735, 1109, 351, 541, 904, 764, 452, 915, 1212,
              501, 725, 291, 784, 157)
  rotas <- which(abs(200000 * y_cent - 2469 * x_cent) > 100000)
  expect_identical(rotas, c(14L, 22L, 25L))
  r <- prop_de(data.frame(x = x_cent / 100, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), rotas)
})

test_that("con la redondeada en la lectura gruesa, la banda no se acusa", {
  # y = round(x / 1.105) en enteros: la tolerancia de `y` -0,5- es un 10 % mas
  # ancha que la de `x` llevada a `y`, y uno de cada diez residuos cae entre
  # las dos. Contadas con la de `x`, `x ~= y * 1.105` acusaba las cinco filas
  # sanas de esa banda. En enteros: |221 * y - 200 * x| <= 110,5, y la banda
  # es |221 * y - 200 * x| > 100.
  x <- c(834, 1899, 697, 207, 715, 889, 1833, 1749, 1246, 1095, 1527, 1427,
         387, 1830, 826, 943, 821, 844, 1552, 1946, 997, 128, 1563, 457, 982,
         732, 1866, 1109, 1146, 474, 1843, 1649, 950, 1208, 92, 269, 1487, 1431,
         235, 1078, 942, 146, 1488, 1216, 665, 394, 810, 512, 628, 248)
  y <- (400 * x + 221) %/% 442
  expect_true(all(2 * abs(221 * y - 200 * x) <= 221))
  expect_identical(which(abs(221 * y - 200 * x) > 100),
                   c(6L, 15L, 19L, 23L, 41L))
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 1.105 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  # Lo mismo con 1,22 y la base de 11 a casi un millon: la banda es
  # |61 * y - 50 * x| > 25, y `x ~= y * 1.22` la acusaba.
  x <- c(8070, 191, 245708, 257936, 864318, 32, 12413, 33394, 55, 284481, 65,
         41, 772297, 13663, 12, 139901, 987160, 219017, 965, 479964, 421,
         728178, 6501, 7882, 132878, 98, 12444, 133, 13, 60607, 15516, 19,
         23388, 28690, 53229, 42, 57, 77, 11, 14755, 731, 245, 73, 104, 129,
         185, 71, 405867, 533, 27)
  y <- (100 * x + 61) %/% 122
  expect_true(all(2 * abs(61 * y - 50 * x) <= 61))
  expect_identical(which(abs(61 * y - 50 * x) > 25), c(2L, 17L, 32L, 33L, 36L))
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 1.22 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
  # Y con 1,18 y `x` de 1 a 100: la lectura de `x` leia `1/0.8466`, una cifra
  # mas que `1/1.18`, y con esa constante la banda parecia de filas rotas.
  x <- c(63, 84, 65, 52, 64, 94, 33, 1, 36, 1, 48, 67, 26, 100, 85, 49,
         50, 45, 10, 53, 72, 28, 24, 56, 71, 83, 49, 5, 12, 19, 65, 84,
         48, 85, 37, 5, 56, 98, 8, 22, 47, 19, 5, 90, 57, 73, 55, 7,
         82, 34)
  y <- (100 * x + 59) %/% 118
  expect_true(all(2 * abs(59 * y - 50 * x) <= 59))
  expect_identical(which(abs(59 * y - 50 * x) > 25),
                   c(9L, 16L, 19L, 24L, 27L, 37L, 49L))
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 1.18 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("una banda rala sin rotas cerca es redondeo y no se acusa", {
  # y = round(x / 1.09) sobre 300 enteros de 10 a un millon. La banda
  # -|109 * y - 100 * x| > 50- junta 10 filas contra unas 25 esperadas, y
  # ninguna fila cae justo afuera de la ventana de `y`: son residuos del
  # redondeo, no rotas. Pedir la mitad de las esperadas acusaba las 10.
  set.seed(161)
  x <- round(exp(stats::runif(300, log(10), log(1e6))))
  y <- (200 * x + 109) %/% 218
  expect_true(all(2 * abs(109 * y - 100 * x) <= 109))
  expect_identical(sum(abs(109 * y - 100 * x) > 50), 10L)
  r <- prop_de(data.frame(x = x, y = y))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x / 1.09 ", fixed = TRUE)
  expect_equal(r$h[[1L]]$n_afectados, 0)
})

test_that("una corta exacta no la gana una reciproca que traga rotas", {
  # x en medios e y = 0.18 * x exacto a centavos, con las tres mayores corridas
  # 3 centavos. `1/5.555` -cuatro cifras- se tragaba las tres y le ganaba por
  # un punto a `0.18`, que las acusa: una corta exacta no es un truncado.
  x_cent <- c(99950, 54950, 76850, 69250, 24300, 44800, 76500, 69750, 27600,
              19350, 65750, 73400, 51950, 84500, 62950, 18800, 92550, 43750,
              42700, 63650, 43600, 81050, 71850, 73100, 44100, 45850, 60900,
              61800, 10450, 30450)
  y_cent <- c(17994, 9891, 13833, 12465, 4374, 8064, 13770, 12555, 4968, 3483,
              11835, 13212, 9351, 15213, 11331, 3384, 16662, 7875, 7686, 11457,
              7848, 14589, 12933, 13158, 7938, 8253, 10962, 11124, 1881, 5481)
  expect_true(all(x_cent %% 50 == 0))
  rotas <- which(100 * y_cent != 18 * x_cent)
  expect_identical(rotas, c(1L, 14L, 17L))
  expect_identical(sort(order(-x_cent)[1:3]), rotas)
  r <- prop_de(data.frame(x = x_cent / 100, y = y_cent / 100))
  expect_length(r$h, 1L)
  expect_match(r$h[[1L]]$descripcion, "y ~= x * 0.18 ", fixed = TRUE)
  expect_identical(sort(r$h[[1L]]$trazabilidad[[1L]]$indices_fila), rotas)
})
