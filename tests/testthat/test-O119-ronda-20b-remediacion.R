# Ronda 20-B: el plan de limpieza, su aplicacion y la guia. Cada bloque es un
# hallazgo de la refutacion, con el caso que lo reproducia.

.solo_O119 <- function(plan, estrategia, columna = NULL) {
  cuales <- plan$estrategia == estrategia
  if (!is.null(columna)) cuales <- cuales & plan$columna %in% columna
  plan$aplicar[] <- FALSE
  plan$estado[cuales] <- "lista"
  plan$aplicar[cuales] <- TRUE
  plan
}

.aplicar_O119 <- function(datos, estrategia, columna = NULL, ...) {
  perfil <- suppressWarnings(perfilar(datos, ...))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  plan <- .solo_O119(plan, estrategia, columna)
  suppressWarnings(aplicar(plan, datos, permitir_eliminacion = TRUE))
}

test_that("convertir_titulo pone la mayuscula en la inicial, tambien acentuada", {
  datos <- data.frame(
    lugar = c("\u00e1ngel l\u00f3pez", "\u00c1NGEL L\u00d3PEZ", "pe\u00f1a", "Pe\u00f1a",
              "\u041c\u041e\u0421\u041a\u0412\u0410", "o'neil"),
    stringsAsFactors = FALSE
  )
  r <- .aplicar_O119(datos, "convertir_titulo")
  # Antes: "\u00e1Ngel L\u00f3Pez", "Pe\u00f1A" y "\u043c\u043e\u0441\u043a\u0432\u0430".
  expect_identical(r$datos$lugar, c(
    "\u00c1ngel L\u00f3pez", "\u00c1ngel L\u00f3pez", "Pe\u00f1a", "Pe\u00f1a",
    "\u041c\u043e\u0441\u043a\u0432\u0430", "O'Neil"
  ))
})

test_that("winsorizar no convierte el texto en numero ni toca lo no finito", {
  codigos <- c("0010", "0011", "0012", "0013", "0011", "0012", "0010", "0013",
               "0012", "0011", "0010", "0012", "0011", "0012", "0013", "0010",
               "0011", "0012", "0011", "0012", "0500")
  r <- .aplicar_O119(data.frame(codigo = codigos, stringsAsFactors = FALSE),
                     "winsorizar_outliers")
  # Antes: la columna entera pasaba a double y perdia los ceros iniciales.
  expect_type(r$datos$codigo, "character")
  expect_identical(r$datos$codigo[1:20], codigos[1:20])
  expect_identical(r$registro$n_cambiadas, 1)
  base <- c(10.5, 11.2, 12.1, 13.3, 11.8, 12.4, 10.9, 13.0, 12.2, 11.1, 10.2, 12.9,
            11.6, 12.7, 13.4, 10.7, 11.9, 12.5, 11.3, 12.0)
  r <- .aplicar_O119(data.frame(x = c(base, 100.5, Inf, -Inf, NaN)),
                     "winsorizar_outliers")
  # Antes: Inf, -Inf y NaN pasaban a NA sin contarse.
  expect_identical(r$datos$x[22:24], c(Inf, -Inf, NaN))
  expect_identical(r$registro$n_cambiadas, 1)
})

test_that("la lista vacia de ausencias no convierte nada y lo dice", {
  datos <- data.frame(estado = c("activo", "S/D", "baja", "activo", "S/D", "baja"),
                      stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos))
  plan <- .solo_O119(planificar_limpieza(perfil, datos),
                     "convertir_ausencias_textuales")
  i <- which(plan$estrategia == "convertir_ausencias_textuales")
  plan$parametros[[i]]$valores <- character()
  r <- suppressWarnings(aplicar(plan, datos))
  # Antes: con la lista vacia se convertia todo lo detectado.
  expect_identical(r$datos$estado, datos$estado)
  expect_identical(r$registro$estado, "fallida")
  expect_match(r$registro$error, "vac")
})

test_that("guiar_limpieza no muestra valores de columnas protegidas", {
  datos <- data.frame(
    nombre = c("Ana Perez Ruiz", "ANA PEREZ RUIZ", "Bruno Diaz Sosa", "bruno diaz sosa"),
    monto = c(10, 11, 12, 300), stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "nombre"))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  vistos <- character()
  salida <- capture.output(suppressMessages(suppressWarnings(guiar_limpieza(
    plan, datos, selector = function(decision) {
      vistos <<- c(vistos, unlist(decision$ejemplos))
      0L
    }
  ))), type = "message")
  # Antes: "Ana Perez Ruiz" y sus variantes como "Ejemplos reales".
  expect_false(any(grepl("perez", tolower(c(vistos, salida)), fixed = TRUE)))
})

test_that("las acciones de duplicados usan la misma igualdad que el perfil", {
  datos <- data.frame(id = c(1, 1, 2, 2), importe = c(5, 5, 0.3, 0.1 + 0.2))
  r <- .aplicar_O119(datos, "marcar_filas_duplicadas")
  # Antes: la fila 4 (0.1 + 0.2) se marcaba duplicada de la 3 (0.3).
  expect_identical(r$datos$.fila_duplicada, c(FALSE, TRUE, FALSE, FALSE))
  clave <- data.frame(id = c(10, 10, 0.3, 0.1 + 0.2), x = c(1, NA, 2, 3))
  perfil <- suppressWarnings(perfilar(clave))
  plan <- suppressWarnings(planificar_limpieza(perfil, clave))
  i <- which(plan$estrategia == "conservar_mas_completa")
  if (length(i)) {
    plan <- .solo_O119(plan, "conservar_mas_completa")
    plan$parametros[[i]]$clave <- "id"
    r <- suppressWarnings(aplicar(plan, clave, permitir_eliminacion = TRUE))
    expect_identical(nrow(r$datos), 3L)
  }
})

test_that("conservar_mas_completa no junta las claves ausentes", {
  datos <- data.frame(
    doc = c("111", "111", NA, NA, NA, "222", "333", "333"),
    nombre = c("Ana", "Ana", "Bruno", "Carla", "Dario", "Eva", "Fede", "Fede"),
    tel = c(NA, "099", "098", NA, "097", "096", "095", "095"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  skip_if_not(any(plan$estrategia == "conservar_mas_completa"))
  plan <- .solo_O119(plan, "conservar_mas_completa")
  i <- which(plan$estrategia == "conservar_mas_completa")
  plan$parametros[[i]]$clave <- "doc"
  r <- suppressWarnings(aplicar(plan, datos, permitir_eliminacion = TRUE))
  # Antes: Carla y Dario, sin documento, se eliminaban como duplicados de Bruno.
  expect_identical(sort(r$datos$nombre),
                   c("Ana", "Bruno", "Carla", "Dario", "Eva", "Fede"))
})

test_that("normalizar_nombres aplica los nombres editados y no toca las marcas", {
  datos <- data.frame(`Monto Total` = 1:3, `fecha alta` = 4:6, check.names = FALSE)
  perfil <- suppressWarnings(perfilar(datos))
  plan <- .solo_O119(suppressWarnings(planificar_limpieza(perfil, datos)),
                     "normalizar_nombres")
  i <- which(plan$estrategia == "normalizar_nombres")
  plan$parametros[[i]]$nombres_propuestos <- c("monto_total", "fecha_alta")
  r <- suppressWarnings(aplicar(plan, datos))
  # Antes: se aplicaban "Monto.Total" y "fecha.alta".
  expect_identical(names(r$datos), c("monto_total", "fecha_alta"))
})

test_that("el registro de eliminar una columna duplicada nombra la eliminada", {
  datos <- data.frame(depto = c("a", "b", "c"), depto_copia = c("a", "b", "c"),
                      n = 1:3, stringsAsFactors = FALSE)
  r <- .aplicar_O119(datos, "eliminar_columna_duplicada")
  skip_if_not(nrow(r$registro) > 0L)
  expect_identical(r$registro$columna, setdiff(names(datos), names(r$datos)))
})

test_that("con aplicabilidad, los outliers se miden y se tratan en su universo", {
  gasto <- c(c(100, 101, 99, 102, 98, 100, 103, 97, 101, 99, 100, 102, 98, 101,
               99, 100, 103, 97, 101, 100, 400),
             c(5, 6, 4, 5, 7, 5, 6, 4, 5, 6, 5, 4, 6, 5, 7, 5, 4, 6, 5, 6, 2000))
  datos <- data.frame(tiene_auto = rep(c("Si", "No"), each = 21), gasto_auto = gasto,
                      stringsAsFactors = FALSE)
  aplic <- list(gasto_auto = ~ tiene_auto == "Si")
  r <- .aplicar_O119(datos, "marcar_outliers", aplicabilidad = aplic)
  marca <- r$datos[[grep("^\\.outlier", names(r$datos), value = TRUE)]]
  # Antes: tambien la fila 42, fuera del universo.
  expect_identical(which(marca), 21L)
  r <- .aplicar_O119(datos, "winsorizar_outliers", aplicabilidad = aplic)
  # Antes: 400 se recortaba a 242,5, el limite de la columna entera.
  expect_lt(r$datos$gasto_auto[[21L]], 110)
  expect_identical(r$datos$gasto_auto[[42L]], 2000)
  expect_identical(r$registro$n_no_reversibles, 1)
})

test_that("el plan no tapa el catalogo de centinelas ni el vocabulario compartido", {
  datos <- data.frame(
    nombre = c("Ana Maria Ruiz", "Juan Perez Gomez", "sin dato", "Luis Sosa Diaz"),
    monto = c(10, -999, 12, -999), estado = c("activo", "sin dato", "baja", "sin dato"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "nombre"))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  j <- which(plan$estrategia == "convertir_ausencias_textuales" & plan$columna == "estado")
  expect_identical(plan$parametros[[j]]$valores, "sin dato")
  i <- which(plan$estrategia == "convertir_sentinelas_numericos" & plan$columna == "monto")
  skip_if_not(length(i) == 1L)
  # Antes: el catalogo del paquete salia como NA y la accion quedaba sin efecto.
  expect_false(anyNA(plan$parametros[[i]]$valores))
})

test_that("snake_case sobre un sf conserva su geometria", {
  skip_if_not_installed("sf")
  datos <- sf::st_as_sf(data.frame(`Nombre Depto` = c("a", "b"), x = 1:2, y = 3:4,
                                   check.names = FALSE), coords = c("x", "y"))
  names(datos)[names(datos) == "geometry"] <- "SHAPE"
  sf::st_geometry(datos) <- "SHAPE"
  r <- .aplicar_O119(datos, "normalizar_nombres_snake_case")
  expect_true(attr(r$datos, "sf_column") %in% names(r$datos))
})

test_that("una referencia numerica 0x80-0x9F se lee como Windows-1252", {
  datos <- data.frame(t = c("O&#146;Neil", "2010 &#150; 2015", "otro"),
                      stringsAsFactors = FALSE)
  r <- .aplicar_O119(datos, "decodificar_entidades_html")
  # Antes: un control C1 invisible en lugar del apostrofo y la raya.
  expect_identical(r$datos$t[1:2], c("O\u2019Neil", "2010 \u2013 2015"))
})

test_that("un orden de texto se rechaza", {
  datos <- data.frame(nombre = c("Ana", "Ana", " Ana"), stringsAsFactors = FALSE)
  plan <- suppressWarnings(planificar_limpieza(suppressWarnings(perfilar(datos)), datos))
  plan$orden <- as.character(plan$orden)
  expect_error(aplicar(plan, datos), "orden")
})

test_that("el resumen impreso no llama celdas a la suma de las acciones", {
  datos <- data.frame(n = c(" Ana", "Ana ", "Beto"), stringsAsFactors = FALSE)
  r <- .aplicar_O119(datos, "recortar_espacios")
  impreso <- paste(capture.output(print(r), type = "message"),
                   capture.output(print(r)), collapse = " ")
  expect_false(grepl("celdas o marcas afectadas", impreso, fixed = TRUE))
})

test_that("activar la reparacion parcial aplica la parte parcial", {
  datos <- data.frame(t = c("Direcci\u00c3\u00b3n\ufffd", "Paysand\u00c3\u00ba", "ok"),
                      stringsAsFactors = FALSE)
  r <- .aplicar_O119(datos, "reparar_codificacion")
  skip_if_not(nrow(r$registro) > 0L)
  # Antes: la parcial quedaba intacta y el registro decia reparado_parcialmente.
  expect_identical(r$datos$t[[1L]], "Direcci\u00f3n\ufffd")
})

test_that("una accion ejecutada que es la recomendada no figura pendiente", {
  x <- c(10, 11, 12, 11, 12, 13, 11, 12, 10, 11, 12, 13, 12, 11, 12, 11, 12, 13, 12, 11, 100)
  datos <- data.frame(x = x)
  perfil <- suppressWarnings(perfilar(datos))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  plan$aplicar <- plan$recomendada
  r <- suppressWarnings(aplicar(plan, datos))
  ejecutadas <- r$registro[r$registro$estado == "ejecutada", ]
  expect_false(any(ejecutadas$decision_grupo %in% c("pendiente", "omitida")))
})

test_that("las acciones de texto conservan los atributos de la columna", {
  depto <- c(" Montevideo", "Rocha ", "Salto")
  attr(depto, "label") <- "Departamento de residencia"
  datos <- data.frame(id = 1:3)
  datos$depto <- depto
  r <- .aplicar_O119(datos, "recortar_espacios")
  # Antes: la etiqueta de variable desaparecia.
  expect_identical(attr(r$datos$depto, "label"), "Departamento de residencia")
  expect_identical(as.vector(r$datos$depto), c("Montevideo", "Rocha", "Salto"))
})
