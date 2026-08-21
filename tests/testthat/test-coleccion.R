# Nivel 7 del marco: la colección, una base entera. Estaba declarada y no se
# medía, y el mensaje decía que faltaba implementarla. Lo que faltaba era el
# objeto: nadie le decía a `lupa` qué tablas la componen.
#
# Las pruebas siguen los criterios que salieron de refutar el diseño contra
# bases reales de más de mil tablas: frontera declarada, esquema como parte de
# la identidad, permisos parciales como caso normal, muestreo por tabla, y
# ninguna lectura instantánea.

.con_de_prueba <- function() {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(
    id = 1:50, nombre = rep(letters[1:5], 10L), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "hogares", data.frame(
    id = 1:20, depto = c(rep("MVD", 15L), rep(NA_character_, 5L)),
    stringsAsFactors = FALSE
  ))
  con
}

test_that("la colección declara la frontera y no consulta nada", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Se puede declarar una tabla que no existe: declarar no es medir.
  col <- coleccion(con, c("personas", "hogares", "no_existe"), nombre = "padron")
  expect_s3_class(col, "coleccion_lupa")
  expect_equal(col$n_declaradas, 3L)
  expect_equal(col$nombre, "padron")
  expect_setequal(col$tablas$tabla, c("personas", "hogares", "no_existe"))
})

test_that("el esquema es parte de la identidad de la tabla", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  por_texto <- coleccion(con, c("dbo.personas", "ods.personas"))
  expect_equal(por_texto$tablas$esquema, c("dbo", "ods"))
  expect_equal(por_texto$tablas$tabla, c("personas", "personas"))
  # El mismo nombre en dos esquemas son dos tablas, no una repetida.
  expect_equal(por_texto$n_declaradas, 2L)

  por_tabla <- coleccion(con, data.frame(
    esquema = c("dbo", "ods"), tabla = c("personas", "hogares"),
    stringsAsFactors = FALSE
  ))
  expect_equal(por_tabla$tablas$identificador, c("dbo.personas", "ods.hogares"))
})

test_that("lo que no se pudo leer se declara, y nunca queda en cero", {
  # Es el caso normal en bases institucionales: una credencial lee unas pocas
  # tablas de un esquema con cientos de objetos.
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares", "sin_permiso"))
  )
  expect_s3_class(perfil, "perfil_coleccion")
  expect_equal(perfil$meta$n_declaradas, 3L)
  expect_equal(perfil$meta$n_perfiladas, 2L)
  expect_equal(perfil$meta$n_sin_perfilar, 1L)

  falta <- perfil$cobertura_coleccion
  expect_equal(nrow(falta), 1L)
  expect_equal(falta$tabla, "sin_permiso")
  expect_true(nzchar(falta$motivo))
  expect_true(nzchar(falta$como_resolverlo))
  # Y no aparece en el resumen con ceros.
  expect_false("sin_permiso" %in% perfil$resumen_coleccion$tabla)
})

test_that("un objeto que no es tabla base se declara en vez de perfilarse", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  col <- coleccion(con, data.frame(
    esquema = c(NA, NA), tabla = c("personas", "hogares"),
    tipo = c("tabla", "vista"), stringsAsFactors = FALSE
  ))
  perfil <- perfilar_coleccion(col)

  expect_equal(perfil$meta$n_perfiladas, 1L)
  declarado <- perfil$cobertura_coleccion
  expect_equal(declarado$tabla, "hogares")
  expect_equal(declarado$tipo, "vista")
  expect_true(grepl("vista", declarado$motivo, fixed = TRUE))
})

test_that("cada tabla declara su propio muestreo, sin promediar alcances", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares")), muestra = 30L
  )
  resumen <- perfil$resumen_coleccion
  expect_equal(nrow(resumen), 2L)
  expect_true(all(resumen$muestra_solicitada == 30))
  # Las tablas tienen 50 y 20 filas: los alcances analizados difieren y se
  # declaran por separado, no se promedian.
  expect_equal(sort(resumen$n_filas), c(20, 50))
  expect_equal(length(unique(resumen$muestra_analizada)), 2L)
})

test_that("los agregados exactos vienen del resumen en SQL", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  resumen <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares"))
  )$resumen_coleccion
  hogares <- resumen[resumen$tabla == "hogares", ]
  # 5 de 20 ausentes en una columna, sobre la tabla entera y no sobre la muestra.
  expect_equal(hogares$prop_faltantes_maxima, 0.25)
  expect_equal(hogares$n_filas, 20)
})

test_that("no hay lectura instantánea, y el objeto lo declara", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(coleccion(con, c("personas", "hogares")))
  expect_false(perfil$meta$snapshot)
  expect_true(grepl("instantanea", perfil$meta$nota_snapshot, fixed = TRUE))
  # Cada tabla trae el momento en que se midió.
  expect_true(inherits(perfil$resumen_coleccion$momento, "POSIXct"))
  expect_false(anyNA(perfil$resumen_coleccion$momento))
})

test_that("los perfiles completos no se retienen salvo que se pidan", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, c("personas", "hogares"))

  liviano <- perfilar_coleccion(col)
  expect_null(liviano$perfiles)

  pesado <- perfilar_coleccion(col, conservar_perfiles = TRUE)
  expect_equal(length(pesado$perfiles), 2L)
  expect_s3_class(pesado$perfiles[["personas"]], "perfil_dbi")
})

test_that("una colección mal declarada se rechaza", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_error(coleccion(con, character()), "vector de nombres")
  expect_error(coleccion(con, c("a", "a")), "repite")
  expect_error(coleccion(con, data.frame(x = 1)), "columna `tabla`")
  expect_error(perfilar_coleccion(list()), "coleccion\\(\\)")
})

# --- Agregar al nivel de colección ---------------------------------------
#
# El hallazgo central de refutar este diseño: un número sobre «la colección»
# calculado sólo con las tablas que se pudieron medir informa como medido lo que
# no se midió. El peso de la tabla ausente desaparece en vez de manifestar la
# falta de cobertura. Por eso la cobertura viaja pegada al número.

.medicion_de_dos_tablas <- function() {
  nucleo <- metricas_nucleo()
  modelo_dos <- modelo(list(
    instanciar(especializar(nucleo$NoNulo), "personas", "nombre"),
    instanciar(especializar(nucleo$NoNulo), "hogares", "depto")
  ))
  medicion <- medir(modelo_dos, list(
    personas = data.frame(nombre = c("a", "b", NA), stringsAsFactors = FALSE),
    hogares = data.frame(depto = c("MVD", NA, "CAN"), stringsAsFactors = FALSE)
  ))
  agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")
}

test_that("agregar a colección exige declarar la frontera", {
  medidas <- .medicion_de_dos_tablas()
  expect_error(
    agregar(medidas, "coleccion", "promedio_ponderado", pesos = c(0.5, 0.5)),
    "declarar la frontera"
  )
  expect_error(
    agregar(medidas, "coleccion", "promedio_ponderado", pesos = c(0.5, 0.5),
            coleccion = list()),
    "coleccion\\(\\)"
  )
})

test_that("la política de pesos no se puede esquivar con un promedio simple", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  perfil <- perfilar_coleccion(coleccion(con, c("personas", "hogares")))
  medidas <- .medicion_de_dos_tablas()

  # Sin esta restricción bastaba pedir `promedio` para obtener un número entre
  # tablas sin declarar nada.
  for (funcion in c("promedio", "ratio", "ratio_umbral")) {
    expect_error(
      agregar(medidas, "coleccion", funcion, coleccion = perfil, umbral = 0.5),
      "solo se admite 'promedio_ponderado'"
    )
  }
})

test_that("el número de la colección viaja con su cobertura", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Tres tablas declaradas, una ilegible: el caso normal en una base real.
  perfil <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares", "sin_permiso"), nombre = "padron")
  )
  medidas <- .medicion_de_dos_tablas()

  agregado <- agregar(
    medidas, "coleccion", "promedio_ponderado",
    pesos = c(0.7, 0.3), coleccion = perfil
  )
  expect_equal(nrow(agregado), 1L)
  expect_equal(agregado$granularidad, "coleccion")
  expect_equal(agregado$entidad, "padron")

  cobertura <- attr(agregado, "cobertura_coleccion")
  expect_equal(cobertura$tablas_declaradas, 3L)
  expect_equal(cobertura$tablas_en_el_numero, 2L)
  expect_equal(cobertura$cobertura, 2 / 3)
  expect_equal(cobertura$tablas_sin_medir, "sin_permiso")
  expect_true(nzchar(cobertura$motivo_sin_medir))
  # Y dice por qué leerlo sin la cobertura sería un error.
  expect_true(grepl("no se midio", cobertura$advertencia, fixed = TRUE))
})

test_that("la granularidad coleccion ya figura como implementada", {
  catalogo <- granularidades()
  expect_true(
    catalogo$implementada[catalogo$granularidad == "coleccion"]
  )
  expect_true(
    catalogo$implementada[catalogo$granularidad == "conjuntoColecciones"]
  )
  # Las diez se miden: las cuatro de arriba, sólo con la frontera declarada.
  expect_true(all(catalogo$implementada))
})

test_that("las granularidades por encima de la colección dicen qué falta", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  perfil <- perfilar_coleccion(coleccion(con, c("personas", "hogares")))
  medidas <- .medicion_de_dos_tablas()

  # Se llega hasta `coleccion`, que ahora sí se mide.
  agregado <- agregar(
    medidas, "coleccion", "promedio_ponderado",
    pesos = c(0.5, 0.5), coleccion = perfil
  )
  # El conjunto de colecciones ya se mide, y exige que la frontera se declare:
  # qué bases lo componen no está en ningún dato.
  expect_error(
    agregar(agregado, "conjuntoColecciones", "promedio"),
    "`colecciones` debe ser"
  )
  expect_error(
    agregar(agregado, "organizacion", "promedio"),
    "qu\u00e9 bases le pertenecen"
  )
})

# --- Relaciones entre tablas declaradas ----------------------------------
#
# Es el punto donde el costo explota: con 1.730 tablas hay casi tres millones de
# direcciones, porque una clave foránea es dirigida. Los pares se declaran igual
# que la frontera.

.con_con_relacion <- function() {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(
    id = 1:20, nombre = letters[1:20], stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "visitas", data.frame(
    persona_id = c(1:15, 1:5), fecha = rep("2023-01-01", 20L),
    stringsAsFactors = FALSE
  ))
  con
}

test_that("el costo se estima en pares dirigidos y comparaciones de columnas", {
  con <- .con_con_relacion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  costo <- estimar_costo_coleccion(coleccion(con, c("personas", "visitas")))

  expect_equal(costo$n_tablas, 2L)
  # Una clave foránea es dirigida: n*(n-1), no n*(n-1)/2.
  expect_equal(costo$n_pares_dirigidos, 2L)
  # Dos tablas de dos columnas son cuatro comparaciones por dirección.
  expect_equal(costo$n_comparaciones_columnas, 8)
  expect_true(grepl("dirigida", costo$nota, fixed = TRUE))
})

test_that("los pares se declaran: explorar todos no es una opción", {
  con <- .con_con_relacion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, c("personas", "visitas"))

  expect_error(relaciones_coleccion(col, pares = NULL), "no es viable")
  expect_error(
    relaciones_coleccion(col, pares = data.frame(
      tabla_1 = "personas", tabla_2 = "inexistente"
    )),
    "no estan declaradas"
  )
})

test_that("una clave foránea candidata aparece con su cardinalidad y alcance", {
  con <- .con_con_relacion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, c("personas", "visitas"), nombre = "padron")

  resultado <- relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "personas", tabla_2 = "visitas")
  )
  expect_s3_class(resultado, "relaciones_coleccion")
  expect_equal(nrow(resultado$relaciones), 1L)

  relacion <- resultado$relaciones
  expect_equal(relacion$columna_tabla1, "id")
  expect_equal(relacion$columna_tabla2, "persona_id")
  expect_equal(relacion$cardinalidad, "1:m")
  expect_equal(relacion$cobertura_tabla2_en_tabla1, 1)
  # El alcance de la lectura se declara.
  expect_equal(relacion$filas_leidas_1, 20)
  expect_equal(resultado$meta$pares_comparados, 1L)

  # Y el objeto dice que un indicio sobre una muestra no es una clave
  # comprobada.
  expect_true(grepl("no es una clave foranea", resultado$meta$alcance,
                    fixed = TRUE))
})

test_that("un par que no se pudo leer se declara en vez de desaparecer", {
  con <- .con_con_relacion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, c("personas", "visitas", "sin_permiso"))

  resultado <- relaciones_coleccion(col, pares = data.frame(
    tabla_1 = c("personas", "personas"),
    tabla_2 = c("visitas", "sin_permiso"),
    stringsAsFactors = FALSE
  ))
  expect_equal(resultado$meta$pares_declarados, 2L)
  expect_equal(resultado$meta$pares_comparados, 1L)
  expect_equal(nrow(resultado$cobertura_pares), 1L)
  expect_equal(resultado$cobertura_pares$tabla_2, "sin_permiso")
  expect_true(nzchar(resultado$cobertura_pares$motivo))
})

# --- Los tres defectos que encontró la auditoría cruzada ------------------

test_that("un número de colección no puede mezclar entidades ajenas", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, "personas", nombre = "padron")

  nucleo <- metricas_nucleo()
  modelo_mixto <- modelo(list(
    instanciar(especializar(nucleo$NoNulo), "personas", "nombre"),
    instanciar(especializar(nucleo$NoNulo), "ajena", "campo")
  ))
  medicion <- medir(modelo_mixto, list(
    personas = data.frame(nombre = c("a", "b", NA), stringsAsFactors = FALSE),
    ajena = data.frame(campo = c(NA, NA, NA), stringsAsFactors = FALSE)
  ))
  medidas <- agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")

  # Antes devolvía 0,3333 con cobertura 1 de 1, aunque la mitad del número
  # venía de una tabla que no pertenece a la colección: el mismo invariante
  # roto en la dirección contraria a la que se había protegido.
  expect_error(
    agregar(medidas, "coleccion", "promedio_ponderado",
            pesos = c(0.5, 0.5), coleccion = col),
    "no estan declaradas en la coleccion"
  )
})

test_that("dos tablas con el mismo nombre en esquemas distintos no colapsan", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, data.frame(
    esquema = c("public", "auditoria"), tabla = c("personas", "personas"),
    stringsAsFactors = FALSE
  ))

  frontera <- lupa:::.validar_coleccion_destino(col)
  expect_setequal(
    frontera$declaradas, c("public.personas", "auditoria.personas")
  )
  # Medir una de las dos es cobertura 1 de 2, no 1 de 1.
  cobertura <- lupa:::.cobertura_agregacion_coleccion(
    frontera, "public.personas"
  )
  expect_equal(cobertura$tablas_declaradas, 2L)
  expect_equal(cobertura$tablas_en_el_numero, 1L)
  expect_equal(cobertura$cobertura, 0.5)
  expect_equal(cobertura$tablas_sin_medir, "auditoria.personas")
})

test_that("una tabla con esquema se cita como identificador compuesto", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, data.frame(
    esquema = c(NA, "public"), tabla = c("personas", "hogares"),
    stringsAsFactors = FALSE
  ))

  # Sin esquema, el nombre simple. Con esquema, un `DBI::Id`, porque
  # `dbQuoteIdentifier("public.hogares")` produce un identificador unico con un
  # punto adentro y consulta una tabla que no existe.
  expect_type(col$tablas$referencia[[1L]], "character")
  expect_s4_class(col$tablas$referencia[[2L]], "Id")
  expect_equal(
    as.character(DBI::dbQuoteIdentifier(con, col$tablas$referencia[[2L]])),
    "`public`.`hogares`"
  )
})

test_that("un resultado malformado no se lleva puesta toda la colección", {
  # Lo encontró la refutación externa. `perfilar_dbi()` puede no fallar y aun
  # así devolver algo con otra forma —otra versión del paquete, un objeto a
  # medio construir—. Si el error al leer sus piezas escapa del `tryCatch`,
  # rompe el bucle y se pierden **todas** las tablas ya perfiladas: el usuario
  # se queda sin resumen y sin cobertura. Un silencio total es el peor
  # resultado posible para este paquete.
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tercera", data.frame(z = 1:5))
  col <- coleccion(con, c("personas", "hogares", "tercera"))

  llamada <- 0L
  testthat::local_mocked_bindings(
    perfilar_dbi = function(conexion, tabla, ...) {
      llamada <<- llamada + 1L
      if (llamada == 2L) return(list())
      list(
        resumen_tabla = list(
          columnas = data.frame(prop_faltantes = 0),
          meta = list(filas = 5)
        ),
        perfil_muestra = list(meta = list(filas_analizadas = 5))
      )
    },
    .package = "lupa"
  )
  perfil <- perfilar_coleccion(col)

  # Las otras dos sobreviven, y la malformada se declara.
  expect_equal(perfil$meta$n_perfiladas, 2L)
  expect_equal(nrow(perfil$cobertura_coleccion), 1L)
  expect_true(grepl(
    "no tiene la forma esperada", perfil$cobertura_coleccion$motivo,
    fixed = TRUE
  ))
})
