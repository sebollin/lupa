test_that("un centinela compartido se publica en la columna que no es personal", {
  # `S/D` es un centinela de `cedula` y a la vez el "sin dato" de `sexo`. Con
  # enmascarado global por valor, `sexo` publicaba `[valor protegido]` sin tener
  # un solo dato personal, y el lector no podia distinguir eso de una columna
  # que si los tiene.
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    monto = c(100, 200, 100, 300, 100),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)

  expect_true("S/D" %in% perfil$patrones$sexo$ejemplos)
  expect_identical(
    as.character(perfil$columnas$moda[perfil$columnas$columna == "sexo"]), "F"
  )

  # Y la columna protegida sigue tapada, que es lo que no cambia.
  expect_true(all(perfil$patrones$cedula$ejemplos == "[valor protegido]"))
  expect_identical(
    as.character(perfil$columnas$moda[perfil$columnas$columna == "cedula"]),
    "[valor protegido]"
  )
})

test_that("la evidencia de un hallazgo sobre una columna no personal se publica", {
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  sobre_sexo <- perfil$hallazgos[
    !is.na(perfil$hallazgos$columna) & perfil$hallazgos$columna == "sexo",
  ]

  expect_gt(nrow(sobre_sexo), 0L)
  expect_false(any(grepl(
    "[valor protegido]", as.character(sobre_sexo$evidencia), fixed = TRUE
  )))
})

test_that("un hallazgo sin columna atribuible sigue enmascarado", {
  # Una fila duplicada muestra filas ENTERAS de la tabla, que incluyen la
  # columna protegida. No hay a quien atribuirle esa fila, asi que el barrido
  # general la sigue gobernando: es el borde que la politica por columna deja
  # explicitamente afuera.
  datos <- data.frame(
    cedula = c("48123456", "48123456", "51987654"),
    monto = c(100, 100, 200),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  sin_columna <- perfil$hallazgos[is.na(perfil$hallazgos$columna), ]

  skip_if(nrow(sin_columna) == 0L, "esta tabla no produjo hallazgos de fila")
  expect_false(any(grepl(
    "48123456", as.character(sin_columna$evidencia), fixed = TRUE
  )))
})

test_that("ningun valor identificante se publica en otra columna", {
  # El limite de la politica por columna: si un valor de una columna protegida
  # aparece tambien en una que no lo esta, por columna se publicaria. Aca se
  # comprueba que eso no ocurre con los valores que identifican, porque el paso
  # por columna no es lo unico que protege: los componentes sin columna
  # atribuible siguen bajo el barrido general.
  datos <- data.frame(
    correo = c("juan.perez@x.uy", "ana@y.uy", "b@z.uy", "c@w.uy"),
    nota = c("sin novedad", "sin novedad", "revisar", "revisar"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  protegidas <- perfil$datos_personales$columna[perfil$datos_personales$proteger]
  skip_if(length(protegidas) == 0L, "el correo no se clasifico como protegido")

  textos <- unlist(lapply(perfil, function(x) {
    if (is.list(x)) unlist(lapply(x, as.character)) else as.character(x)
  }))
  expect_false(any(grepl("juan.perez@x.uy", textos, fixed = TRUE)))
})

test_that("un valor identificante no se publica aunque viva en otra columna", {
  # El piso de la politica por columna. Sin el, una columna que el clasificador
  # no marco publicaba los valores de la protegida con solo repetirlos: medido,
  # tres de cuatro documentos de ocho digitos salian por una copia llamada
  # `codigo_operacion`, por un texto libre que los contenia y por una copia
  # guardada como numero.
  documentos <- c("48123456", "51987654", "39112233", "12345678", "48123456")
  textos_de <- function(x, acumulado = character()) {
    if (is.list(x)) {
      for (parte in x) acumulado <- textos_de(parte, acumulado)
      return(acumulado)
    }
    if (is.character(x) || is.factor(x)) {
      acumulado <- c(acumulado, as.character(x))
    }
    acumulado
  }
  copias <- list(
    codigo_operacion = documentos,
    nota = paste0("ref ", documentos),
    clave_externa = as.numeric(documentos)
  )
  for (nombre in names(copias)) {
    datos <- data.frame(documento = documentos, stringsAsFactors = FALSE)
    datos[[nombre]] <- copias[[nombre]]
    datos$monto <- c(100, 200, 100, 300, 100)
    perfil <- perfilar(datos)

    publicado <- textos_de(perfil)
    publicado <- publicado[!is.na(publicado)]
    for (documento in unique(documentos)) {
      expect_false(
        any(grepl(documento, publicado, fixed = TRUE)),
        info = paste(nombre, documento)
      )
    }
  }
})

test_that("el piso no tapa el vocabulario corto que se comparte", {
  # Control: el piso protege lo que identifica y NO deshace la politica por
  # columna. `"S/D"` tiene tres caracteres, no identifica a nadie y sigue
  # publicandose donde describe a una columna que no es personal.
  datos <- data.frame(
    cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
    sexo = c("F", "M", "S/D", "F", "M"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)

  expect_true("S/D" %in% perfil$patrones$sexo$ejemplos)
  expect_true(all(perfil$patrones$cedula$ejemplos == "[valor protegido]"))
})

test_that("el piso alcanza a los campos numericos, no solo al texto", {
  # Lo encontro una refutacion externa y el piso no lo cubria: `copia` se
  # clasifica `documento_identidad` con poder discriminante debil, asi que no se
  # protege, y publicaba en `minimo`, `maximo`, `media` y `mediana` los
  # documentos de `documento`, que en el mismo objeto sale enmascarada. Sobre
  # doce filas, `minimo` y `maximo` son dos documentos exactos.
  datos <- data.frame(
    documento = sprintf("707771%02d", 1:12), stringsAsFactors = FALSE
  )
  datos$copia <- datos$documento
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  i <- match("copia", perfil$columnas$columna)
  expect_true(is.na(perfil$columnas$minimo[[i]]))
  expect_true(is.na(perfil$columnas$maximo[[i]]))
  expect_true(is.na(perfil$columnas$media[[i]]))
  expect_true(is.na(perfil$columnas$mediana[[i]]))
})

test_that("el piso numerico no toca un numero que no es un valor protegido", {
  # Control: el piso tapa un numero cuya representacion ES un valor
  # identificante de una columna protegida, y ningun otro. Una columna de
  # importes conserva sus estadisticos enteros.
  datos <- data.frame(
    documento = sprintf("707771%02d", 1:12),
    monto = c(1500, 2300, 1500, 4800, 2300, 990, 1500, 7200, 3100, 990, 4800, 2300),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  i <- match("monto", perfil$columnas$columna)
  expect_equal(perfil$columnas$minimo[[i]], 990)
  expect_equal(perfil$columnas$maximo[[i]], 7200)
  expect_false(is.na(perfil$columnas$media[[i]]))
})

test_that("distribucion_valores aplica el mismo piso que perfilar", {
  # Lo encontro una refutacion externa: `distribucion_valores()` protege POR
  # COLUMNA -si la columna es personal, sus cuantiles van NA- pero no aplicaba
  # el piso, asi que una copia que el clasificador no marca publicaba los
  # documentos de la protegida. Medido: los cinco cuantiles y las sesenta
  # frecuencias los traian enteros.
  documentos <- sprintf("771771%02d", 1:30)
  datos <- data.frame(
    documento = documentos,
    codigo_operacion = as.numeric(documentos),
    stringsAsFactors = FALSE
  )
  dv <- distribucion_valores(datos, proteger_datos_personales = TRUE)

  # Ninguna frecuencia publica un documento.
  expect_false(any(grepl(
    "771771", as.character(dv$frecuencias$valor), fixed = TRUE
  )))

  # Los cuantiles que SON valores exactos de la columna quedan tapados; los
  # interpolados no lo son y se publican, que es el trato que el `.Rd` declara
  # para una clasificacion de poder discriminante debil.
  q <- dv$cuantiles[dv$cuantiles$columna == "codigo_operacion", ]
  exactos <- q$probabilidad %in% c(0, 1)
  expect_true(all(is.na(q$valor[exactos])))
  expect_false(any(
    q$valor[!exactos] %in% as.numeric(documentos)
  ))
})

test_that("el mismo documento con separadores no escapa al piso", {
  # El reemplazo busca la cadena exacta, asi que `"771.771-01"` no contiene
  # `"77177101"` y se publicaba. Lo encontro una refutacion externa. La
  # separacion es cosmetica: el valor es el mismo.
  documentos <- sprintf("771771%02d", 1:30)
  con_puntos <- sub("^(...)(...)(..)$", "\\1.\\2-\\3", documentos)
  datos <- data.frame(
    documento = documentos, sep = con_puntos, stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_true(all(perfil$patrones$sep$ejemplos == "[valor protegido]"))
})

test_that("la pasada de separadores no toca lo que no es un documento", {
  # Control: se exige que la forma normalizada conserve el largo del piso, asi
  # que un codigo corto o un importe no arrastran media tabla.
  datos <- data.frame(
    documento = sprintf("771771%02d", 1:30),
    codigo = rep(c("A-1", "B-2", "C-3"), 10),
    monto = rep(c(1500, 2300, 990), 10),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  i <- match("codigo", perfil$columnas$columna)
  expect_false(is.na(perfil$columnas$moda[[i]]))
  expect_false(identical(
    as.character(perfil$columnas$moda[[i]]), "[valor protegido]"
  ))
  j <- match("monto", perfil$columnas$columna)
  expect_equal(perfil$columnas$minimo[[j]], 990)
})

test_that("analizar() aplica el piso sobre sus resumenes derivados", {
  # `analizar()` protege por columna, asi que publicaba el documento adentro de
  # texto libre en `variables$niveles_observados` mientras `perfilar()`, sobre
  # la misma tabla, lo tapaba. Y por omision NO conserva los datos, asi que los
  # valores hay que cosecharlos antes de proteger el perfil.
  documentos <- sprintf("771771%02d", 1:30)
  datos <- data.frame(
    documento = documentos,
    notas = paste("cliente", documentos),
    stringsAsFactors = FALSE
  )
  analisis <- analizar(datos)

  textos <- unlist(lapply(analisis, function(x) {
    if (is.list(x)) unlist(lapply(x, as.character)) else as.character(x)
  }))
  expect_false(any(grepl("771771", textos, fixed = TRUE)))
})

test_that("el marcador de proteccion no se enmascara a si mismo", {
  # Cuando los valores se cosechan de un perfil que YA paso por proteccion,
  # `"[valor protegido]"` entra en la lista de valores a tapar. Su forma sin
  # separadores es `valorprotegido`, identica a la del estado `valor_protegido`
  # que publican los cuantiles: el marcador terminaba enmascarandose a si mismo
  # y un campo que declara la proteccion pasaba a declarar un valor.
  expect_length(lupa:::.valores_identificantes("[valor protegido]"), 0L)
  expect_length(
    lupa:::.valores_identificantes(c("[valor protegido]", "77177101")), 1L
  )
})

test_that("comparar_perfiles declara que un lado esta protegido y el otro no", {
  # Salio de una refutacion externa. Comparando un perfil protegido contra uno
  # sin proteger de la MISMA tabla, la salida publicaba el rango que el lado
  # protegido oculta y la diferencia se leia como deriva del dato, cuando lo que
  # cambio es la politica. No es una fuga -quien corre esa comparacion ya tiene
  # el perfil sin proteger- pero es la misma forma que ya tienen las varas y la
  # normalizacion: un cambio de politica se declara con su propia fila.
  datos <- data.frame(
    documento = sprintf("707771%02d", 1:40), stringsAsFactors = FALSE
  )
  protegido <- perfilar(datos, analizar_dependencias = FALSE)
  abierto <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  fila <- function(x, y) {
    r <- as.data.frame(comparar_perfiles(x, y))
    r[r$aspecto == "configuracion_proteccion", ]
  }
  # En las dos direcciones, porque la asimetria no tiene lado preferido.
  for (par in list(list(protegido, abierto), list(abierto, protegido))) {
    f <- fila(par[[1L]], par[[2L]])
    expect_equal(nrow(f), 1L)
    expect_identical(as.character(f$severidad[[1L]]), "error")
  }

  # Control: dos corridas con la misma politica no declaran nada.
  expect_equal(
    nrow(fila(protegido, perfilar(datos, analizar_dependencias = FALSE))), 0L
  )
})

test_that("detectar_duplicados_aproximados aplica el piso", {
  # Lo encontro una refutacion externa: la proteccion de esa puerta es por
  # columna, asi que publicaba el documento en la MISMA cadena donde la columna
  # protegida iba enmascarada -- `documento=[valor protegido]; codigo=77177101`.
  datos <- data.frame(
    documento = c("77177101", "77177101", "77177102"),
    codigo = c("77177101", "77177101", "77177102"),
    stringsAsFactors = FALSE
  )
  resultado <- detectar_duplicados_aproximados(datos)
  skip_if(is.null(resultado$pares) || !nrow(resultado$pares), "sin pares")

  evidencia <- c(
    as.character(resultado$pares$evidencia_1),
    as.character(resultado$pares$evidencia_2)
  )
  expect_false(any(grepl("77177101", evidencia, fixed = TRUE)))

  # Y la forma con separadores, que es el mismo documento.
  con_puntos <- data.frame(
    documento = c("77177101", "77177101", "77177102"),
    sep = c("771.771-01", "771.771-01", "771.771-02"),
    stringsAsFactors = FALSE
  )
  r2 <- detectar_duplicados_aproximados(con_puntos)
  skip_if(is.null(r2$pares) || !nrow(r2$pares), "sin pares")
  expect_false(any(grepl(
    "771.771-01", as.character(r2$pares$evidencia_1), fixed = TRUE
  )))
})

test_that("perfilar_dbi tapa lo mismo que perfilar sobre la misma tabla", {
  # Divergencia entre puertas encontrada por una refutacion externa:
  # `resumen_tabla$columnas` traia `minimo`, `maximo`, `media` y `mediana` de una
  # copia numerica con los treinta documentos enteros, mientras `perfilar()` los
  # enmascaraba. Por esta puerta el piso por valor es imposible -el resumen se
  # calcula con SQL sobre la tabla entera y no se ven todos los valores-, asi que
  # se tapan los estadisticos de orden de toda columna que COMPARTE tipo personal
  # con una protegida: no es conjetura sobre los valores, es lo que la
  # clasificacion ya afirmo de las dos.
  skip_if_not_installed("RSQLite")
  documentos <- sprintf("771771%02d", 1:30)
  datos <- data.frame(
    documento = documentos,
    copia_num = as.numeric(documentos),
    monto = round(seq(100, 4000, length.out = 30)),
    stringsAsFactors = FALSE
  )
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", datos)

  memoria <- perfilar(datos, analizar_dependencias = FALSE)$columnas
  motor <- perfilar_dbi(conexion, "t")$resumen_tabla$columnas

  for (campo in c("minimo", "maximo", "media")) {
    i <- match("copia_num", motor$columna)
    expect_true(is.na(motor[[campo]][[i]]), info = campo)
  }
  # Control: la columna legitima conserva sus estadisticos en las DOS puertas.
  j <- match("monto", motor$columna)
  k <- match("monto", memoria$columna)
  expect_equal(motor$minimo[[j]], memoria$minimo[[k]])
})
