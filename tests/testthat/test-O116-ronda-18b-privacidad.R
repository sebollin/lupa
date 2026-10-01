# Ronda 18-B: lo que la proteccion de datos personales dejaba salir por la
# prosa del paquete, por las etiquetas de grupo y por las variantes de un nombre
# fuera del latin occidental.

.hojas_O116 <- function(x) {
  hojas <- character()
  recorrer <- function(y) {
    for (a in setdiff(names(attributes(y)), c("names", "class", "row.names"))) {
      recorrer(attr(y, a, exact = TRUE))
    }
    if (is.factor(y)) hojas <<- c(hojas, levels(y))
    else if (is.character(y)) hojas <<- c(hojas, y)
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(x)
  hojas[!is.na(hojas)]
}

.sugerencia_ausencia_O116 <- function(perfil) {
  h <- perfil$hallazgos
  as.character(h$sugerencia[h$tipo_hallazgo == "posible_ausencia_estructural"])
}

test_that("la sugerencia no publica el criterio de un determinante protegido", {
  n <- 60L
  pila <- rep(c("Ana", "Eva"), length.out = n)
  # La dependiente es un documento, que el clasificador marca personal: la
  # evidencia se tapaba antes de que la proteccion de la ausencia la leyera.
  datos <- data.frame(
    id = seq_len(n), pila = pila,
    rut = ifelse(pila == "Ana", sprintf("21%07d0018", seq_len(n)), NA),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = c(pila = "nombre")))
  sugerencia <- .sugerencia_ausencia_O116(perfil)
  expect_length(sugerencia, 1L)
  # Antes: `aplicabilidad = list(rut = ~ pila == "Ana")`.
  expect_false(grepl("\"Ana\"", sugerencia, fixed = TRUE))
  expect_true(grepl("esta protegida", sugerencia, fixed = TRUE))

  # Con un umbral: antes `nro_jubilacion = ~ edad >= 66`.
  set.seed(4)
  edad <- sample(20:90, n, TRUE)
  datos <- data.frame(
    id = seq_len(n), edad = edad,
    nro_jubilacion = ifelse(edad >= 65, sprintf("21%07d0018", seq_len(n)), NA),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, columnas_personales = c(edad = "edad", nro_jubilacion = "documento_identidad")
  ))
  sugerencia <- .sugerencia_ausencia_O116(perfil)
  expect_length(sugerencia, 1L)
  expect_false(grepl(">=", sugerencia, fixed = TRUE))
  expect_false(grepl("~ edad", sugerencia, fixed = TRUE))

  # Un nombre con acento grave cortaba la extraccion del determinante.
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos[["nom`bre"]] <- pila
  datos$rut <- ifelse(pila == "Ana", sprintf("21%07d0018", seq_len(n)), NA)
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "nom`bre"))
  sugerencia <- .sugerencia_ausencia_O116(perfil)
  expect_length(sugerencia, 1L)
  expect_false(grepl("\"Ana\"", sugerencia, fixed = TRUE))
})

test_that("el nivel citado en la prosa no publica un nombre protegido", {
  n <- 90L
  proveedor <- rep(c("juanperezsrl", "otra \"cosa\"", "particular"), length.out = n)
  datos <- data.frame(
    id = seq_len(n),
    titular = rep(c("Juan Perez", "Maria Nunez", "Pedro Gomez", "Lucia Fernandez"),
                  length.out = n),
    proveedor = proveedor,
    rut = ifelse(proveedor != "particular", sprintf("21%07d0018", seq_len(n)), NA),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  sugerencia <- .sugerencia_ausencia_O116(perfil)
  expect_length(sugerencia, 1L)
  # Antes salia `proveedor %in% c("juanperezsrl", ...)`: el titular pegado a
  # "srl", y en la prosa valia la regla de limites de palabra.
  expect_false(grepl("juanperez", sugerencia, fixed = TRUE))
  # Se tapa la cita, no la sugerencia: sigue leyendose, y el otro nivel -con
  # comillas escapadas, como codigo de R valido- se publica.
  expect_true(grepl("\"[valor protegido]\"", sugerencia, fixed = TRUE))
  expect_true(grepl("otra \\\"cosa\\\"", sugerencia, fixed = TRUE))
  expect_true(grepl("aplicabilidad", sugerencia, fixed = TRUE))
  todo <- paste(.hojas_O116(perfil), collapse = " || ")
  expect_false(grepl("juanperezsrl", todo, fixed = TRUE))
})

test_that("los niveles citados en la sugerencia son codigo de R que se puede leer", {
  niveles <- c("a\"b", "c\\d", "Jos\u00e9 \"x\"")
  texto <- lupa:::.texto_valores_nivel(niveles)
  expect_identical(eval(parse(text = texto)), niveles)
  expect_identical(Encoding(lupa:::.texto_valores_nivel("Jos\u00e9 \"x\"")), "UTF-8")
})

test_that("perfilar_por avisa si una etiqueta de grupo lleva un valor protegido", {
  datos <- data.frame(
    id = 1:120,
    titular = rep(c("Juan Perez", "Maria Nunez", "Pedro Gomez", "Lucia Fernandez"), 30),
    proveedor = rep(c("juanperezsrl", "particular"), 60),
    monto = 100 + 1:120 %% 7,
    stringsAsFactors = FALSE
  )
  expect_message(
    agrupado <- perfilar_por(datos, por = "proveedor", min_filas = 10L,
                             columnas_personales = "titular"),
    "valor de una columna protegida"
  )
  declaradas <- attr(agrupado, "etiquetas_personales")
  expect_identical(nrow(declaradas), 1L)
  expect_identical(declaradas$tipo, "contiene_valor_protegido")
  expect_identical(declaradas$n_grupos, 1L)

  # Controles: etiquetas ajenas a todo nombre, y la proteccion desactivada.
  ajenos <- datos
  ajenos$proveedor <- rep(c("distribuidora", "particular"), 60)
  agrupado <- suppressMessages(perfilar_por(
    ajenos, por = "proveedor", min_filas = 10L, columnas_personales = "titular"
  ))
  expect_identical(nrow(attr(agrupado, "etiquetas_personales")), 0L)
  agrupado <- suppressMessages(perfilar_por(
    datos, por = "proveedor", min_filas = 10L, columnas_personales = "titular",
    proteger_datos_personales = FALSE
  ))
  expect_identical(nrow(attr(agrupado, "etiquetas_personales")), 0L)
})

test_that("el protegido en latin1 sin marca se reconoce en la celda en UTF-8", {
  # "Jose Perez" con tildes, en latin1 sin marca y en UTF-8, armado por bytes.
  latin1 <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9, 0x20, 0x50, 0xe9, 0x72, 0x65, 0x7a)))
  Encoding(latin1) <- "unknown"
  utf8 <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xc3, 0xa9, 0x20, 0x50, 0xc3, 0xa9, 0x72, 0x65, 0x7a)))
  Encoding(utf8) <- "UTF-8"
  datos <- data.frame(id = 1:12, stringsAsFactors = FALSE)
  datos$titular <- rep(c(latin1, "Ana Maria Ruiz"), 6)
  datos$nota <- rep(c(paste("pago", utf8), "otra cosa larga"), 6)
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  hojas <- .hojas_O116(perfil)
  # Antes: "pago Jose Perez" en los ejemplos de los patrones de `nota`.
  expect_false(any(grepl("P\u00e9rez", hojas, fixed = TRUE, useBytes = TRUE)))
  expect_true(any(hojas == "otra cosa larga"))
})

test_that("las variantes de un nombre fuera del latin occidental no salen", {
  protegidos <- c(
    "Nguyen Van Thanh",
    "Tr\u01b0\u01a1ng H\u01b0\u01a1ng",
    "\u039d\u03af\u03ba\u03bf\u03c2 \u03a0\u03b1\u03c0\u03b1\u03c2",
    "\u0418\u0432\u0430\u043d \u041f\u0435\u0442\u0440\u043e\u0432",
    "\u0531\u0580\u0561\u0574 \u054d\u0561\u0580\u0563\u057d\u0575\u0561\u0576",
    "Juan Perez"
  )
  variantes <- c(
    "pago Nguy\u1ec5n V\u0103n Th\u00e0nh",
    "pago truong.huong",
    "pago \u039d\u0399\u039a\u039f\u03a3 \u03a0\u0391\u03a0\u0391\u03a3",
    "pago \u0418\u0412\u0410\u041d \u041f\u0415\u0422\u0420\u041e\u0412",
    "pago \u0531\u0550\u0531\u0544 \u054d\u0531\u0550\u0533\u054d\u0545\u0531\u0546",
    "pago \uff2a\uff35\uff21\uff2e\u3000\uff30\uff25\uff32\uff25\uff3a"
  )
  n <- 36L
  datos <- data.frame(
    id = seq_len(n),
    titular = rep(protegidos, length.out = n),
    nota = rep(c(variantes, "sin novedad"), length.out = n),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(datos, columnas_personales = "titular"))
  hojas <- .hojas_O116(perfil)
  # Antes, las seis en los ejemplos de `nota`: la comparacion iba por bytes y el
  # pliegue de caja era solo ASCII.
  for (v in variantes) expect_false(v %in% hojas, label = v)
  # Control: lo que no nombra a nadie se publica.
  expect_true("sin novedad" %in% hojas)
})

test_that("el pliegue para comparar no depende del locale", {
  original <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", original)), add = TRUE)
  x <- c("Nguy\u1ec5n", "\u039d\u0399\u039a\u039f\u03a3", "\u039d\u03af\u03ba\u03bf\u03c2",
         "\u0418\u0412\u0410\u041d", "\uff2a\uff35\uff21\uff2e")
  latin1 <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9)))
  esperado <- c("nguyen", "\u03bd\u03b9\u03ba\u03bf\u03c3", "\u03bd\u03b9\u03ba\u03bf\u03c3",
                "\u0438\u0432\u0430\u043d", "juan", "jose")
  resultados <- lapply(c("C", "es_UY.UTF-8", "en_US.UTF-8"), function(locale) {
    puesto <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (!nzchar(puesto)) return(NULL)
    lupa:::.plegar_para_comparar(c(x, latin1))
  })
  resultados <- Filter(Negate(is.null), resultados)
  expect_gte(length(resultados), 1L)
  for (r in resultados) expect_identical(r, esperado)
})

test_that("el piso de seis caracteres se cuenta en caracteres y no en bytes", {
  # Cuatro letras cirilicas son ocho bytes. Con el pliegue por caracteres de
  # esta ronda, contar el piso en bytes -como se contaba- tapaba esta celda por
  # un valor de cinco caracteres: medido cambiando solo el tipo de `nchar()`.
  # Con la comparacion por bytes de antes no se tapaba, pero por azar.
  x <- "cliente \u0418\u0432\u0430\u043d\u043e\u0432\u0430 del rubro"
  expect_identical(lupa:::.reemplazar_variantes_separadas(x, "\u0418\u0432 \u0430\u043d"), x)
  # Control: con seis letras si se tapa.
  expect_identical(
    lupa:::.reemplazar_variantes_separadas(x, "\u0418\u0432\u0430\u043d\u043e\u0432"),
    "[valor protegido]"
  )
})
