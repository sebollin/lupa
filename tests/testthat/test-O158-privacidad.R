# O158: la proteccion de datos personales. H1, la via por bloques sin el piso
# por valor; H2 y H3, el piso final de la coleccion; H4, las secuencias de color
# de cli en el mensaje del motor. Cada bloque rehace la cifra a mano y falla
# contra el codigo anterior al arreglo.

# Las hojas de un objeto, por tipo: los numeros y los textos, recorriendo
# listas y marcos de datos.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r33p_hojas <- function(x) {
  numeros <- numeric()
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.numeric(y)) {
      numeros <<- c(numeros, as.numeric(y))
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(x)
  list(numeros = numeros, textos = textos)
}

# Cuantos de los valores `agujas` (textos) y `numeros` aparecen en el objeto:
# un texto, dentro de cualquier hoja de texto; un numero, como hoja numerica o
# escrito entero en un texto.
.r33p_publicados <- function(objeto, agujas = character(), numeros = numeric()) {
  hojas <- .r33p_hojas(unclass(objeto))
  textos <- hojas$textos[!is.na(hojas$textos)]
  en_texto <- function(v) any(grepl(v, textos, fixed = TRUE))
  escritos <- formatC(numeros, format = "f", digits = 0)
  sum(vapply(agujas, en_texto, logical(1L))) +
    sum(numeros %in% hojas$numeros | vapply(escritos, en_texto, logical(1L)))
}

test_that("H1: por bloques el piso por valor corre sobre todas las columnas, como sin bloques", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # 24 filas: `cedula` (un numero de ocho cifras) y `nombre` protegidas;
  # `codigo`, que no es personal, repite dos cedulas -su maximo es una-, y
  # `obs`, texto libre, cita el nombre en una de cada cuatro filas.
  set.seed(3301)
  n <- 24L
  cedulas <- as.numeric(sample(10000000:59999999, n))
  nombres <- sprintf("Persona Apellidaza %03d", seq_len(n))
  codigo <- as.numeric(sample(100:999, n, TRUE))
  # Dos cedulas del medio: ni el minimo ni el maximo de `cedula`, que la
  # proteccion de la columna ya conoce.
  copiadas <- order(cedulas)[c(5L, 12L)]
  codigo[copiadas] <- cedulas[copiadas]
  citan <- seq_len(n) %% 4L == 0L
  d <- data.frame(
    id = seq_len(n), cedula = cedulas, nombre = nombres, codigo = codigo,
    obs = ifelse(citan, paste("llamar a", nombres), "sin novedad"),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  protegidas <- c("cedula", "nombre")
  perfil <- function(..., proteger_datos_personales = TRUE) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, "t", columnas_personales = protegidas,
      proteger_datos_personales = proteger_datos_personales, ...
    )))
  }
  fila <- function(p, columna) {
    columnas <- p$resumen_tabla$columnas
    columnas[columnas$columna == columna, , drop = FALSE]
  }
  # La cifra a mano: el maximo de `codigo` es la cedula mayor de las dos
  # copiadas, y la via por bloques la calcula -sin proteccion la publica-.
  maximo <- max(cedulas[copiadas])
  expect_identical(max(codigo), maximo)
  abierto <- perfil(bloque_filas = 10L, proteger_datos_personales = FALSE)
  expect_identical(as.numeric(fila(abierto, "codigo")$maximo), maximo)
  # Y el instrumento ve los valores cuando estan: los seis nombres citados y
  # las cedulas.
  expect_gt(.r33p_publicados(abierto, nombres[citan], cedulas), 6L)

  sin_bloques <- perfil()
  expect_identical(.r33p_publicados(sin_bloques, nombres[citan], cedulas), 0L)
  # En SQLite la cedula numerica se relee entera como texto (la lectura fuera
  # de la muestra): por bloques corre la misma.
  for (muestra in list(NULL)) {
    por_bloques <- perfil(bloque_filas = 10L)
    info <- "por bloques"
    # Ni en `columnas`, ni en la corroboracion, ni en `meta$familias`.
    expect_identical(.r33p_publicados(por_bloques, nombres[citan], cedulas), 0L,
                     info = info)
    # Las dos vias tapan los mismos campos de las mismas columnas.
    for (columna in c("codigo", "obs", "id")) {
      for (campo in c("moda", "minimo", "maximo", "media", "mediana", "desvio")) {
        expect_identical(
          is.na(fila(por_bloques, columna)[[campo]]),
          is.na(fila(sin_bloques, columna)[[campo]]),
          info = paste(info, columna, campo)
        )
      }
    }
    expect_true(is.na(fila(por_bloques, "codigo")$maximo), info = info)
    # La proteccion no inventa una divergencia: los dos lados callan el maximo.
    cobertura <- por_bloques$resumen_tabla$cobertura
    expect_false(any(as.character(cobertura$estado) == "divergencia"),
                 info = info)
    # `meta$familias` no publica lo que la tabla de columnas tapa: con el
    # maximo tapado, la media y el desvio reconstruyen las dos cedulas
    # (sum(codigo) / 24 esta dominada por ellas).
    familias <- por_bloques$resumen_tabla$meta$familias
    cuantitativos <- familias$codigo$cuantitativos$resultado
    expect_true(all(is.na(unlist(cuantitativos[c("minimo", "maximo", "media",
                                                  "desvio")]))), info = info)
    # Lo que no es un valor protegido queda: la frase sin nombre de `obs` y los
    # valores de `id`.
    expect_true("sin novedad" %in% familias$obs$distintos$resultado$representante,
                info = info)
    expect_true(all(familias$id$distintos$resultado$representante %in% d$id),
                info = info)
  }

  # Y cuando los valores de la tabla entera no se pueden leer, la via por
  # bloques cierra como la otra: cada valor distinto de `meta$familias` de las
  # columnas no personales tambien se tapa, y se dice.
  local_mocked_bindings(
    .valores_protegidos_fuera_de_muestra_dbi = function(...) {
      list(valores = character(), completo = FALSE, motivo = "simulado",
           fechas = character())
    }
  )
  cerrado <- perfil(bloque_filas = 10L, muestra = 15, orden_muestra = "id")
  expect_identical(.r33p_publicados(cerrado, nombres[citan], cedulas), 0L)
  expect_match(cerrado$resumen_tabla$meta$proteccion_personal$fuera_de_muestra,
               "No verificada: simulado", fixed = TRUE)
})

# SQL Server con la sesion en sueco cita la celda sin comillas.
.r33p_sueco <- function(x) {
  paste0("[FreeTDS][SQL Server]Konvertering av nvarchar vardet av typen ", x,
         " till datatypen int misslyckades.")
}

# Una coleccion de SQLite con la vista ANTES que su tabla base: `perfilar_dbi()`
# de la vista se envuelve para que su lectura de la muestra falle con el error
# del motor que cita la celda -como la vista que castea el celular donde
# alguien cargo el nombre-, en los mismos tres lugares que la via real: la
# cobertura de la muestra, el motivo de una metrica y el aviso de consola.
.r33p_coleccion_vista <- function(valor, conservar, incompleta = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nombres <- sprintf("Cliente Apellidoso Numero %03d", seq_len(12L))
  nombres[[5L]] <- valor
  DBI::dbWriteTable(con, "clientes", data.frame(
    id = seq_len(12L), nombre = nombres, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "vista", data.frame(id = seq_len(6L),
                                             monto = seq_len(6L) * 10))
  original <- perfilar_dbi
  testthat::local_mocked_bindings(perfilar_dbi = function(conexion, tabla, ...) {
    perfil <- original(conexion, tabla, ...)
    if (!identical(as.character(tabla), "vista")) return(perfil)
    motivo <- paste("No se pudo leer la muestra:",
                    .mensaje_motor(simpleError(.r33p_sueco(valor))))
    # Y el control: una celda que la coleccion no leyo -un faltante
    # disfrazado- es el diagnostico, y se publica como antes.
    control <- paste("No se pudo leer la columna:",
                     .mensaje_motor(simpleError(.r33p_sueco("N/D"))))
    resumen <- perfil$resumen_tabla
    resumen$cobertura <- rbind(resumen$cobertura, .registro_cobertura_dbi(
      "perfil_muestra", "vista", "no_disponible", c(motivo, control),
      "Revisar la conversion."
    ))
    resumen$sql$estado[1:2] <- "no_disponible"
    resumen$sql$motivo[1:2] <- c(motivo, control)
    perfil$resumen_tabla <- resumen
    .avisar_dbi("lupa_muestra_dbi_no_disponible", paste(
      "El resumen SQL se calculo y se devuelve, pero la muestra no:", motivo
    ))
    perfil
  })
  if (incompleta) {
    # La base queda incompleta: en SQL Server, una columna protegida `ntext`
    # no admite el `GROUP BY` que lee sus valores.
    testthat::local_mocked_bindings(
      .valores_protegidos_tabla_coleccion = function(conexion, referencia,
                                                     perfil) {
        list(valores = character(), fechas = character(), completo = FALSE,
             motivo = "no se pudieron leer los valores de `nombre`: ntext")
      }
    )
  }
  avisos <- character()
  pc <- withCallingHandlers(
    perfilar_coleccion(coleccion(con, c("vista", "clientes")),
                       conservar_perfiles = conservar,
                       columnas_personales = "nombre", proteger_datos_personales = TRUE),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # `print()` habla por condiciones de cli: `salida_cli()` las recoge.
  list(pc = pc, avisos = avisos, impreso = salida_cli(print(pc)))
}

test_that("H2 y H3: el piso final de la coleccion lee cada mensaje del motor entero, en cualquier orden", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  casos <- list(
    # H2: el resto del nombre son palabras permitidas -una particula y un
    # apellido del vocabulario-; la segunda pasada omitia "Maria" y dejaba
    # "De Los Campos".
    list(valor = "Maria De Los Campos", resto = "Campos", incompleta = FALSE),
    # H3: el valor ENTERO es de palabras permitidas, y la tabla base queda
    # incompleta: con perfiles retenidos el piso final no usaba las huellas.
    list(valor = "Campos Cadena", resto = "Campos", incompleta = TRUE)
  )
  for (caso in casos) {
    # H3 es con perfiles retenidos: sin ellos las huellas ya lo tapaban.
    for (conservar in if (caso$incompleta) TRUE else c(TRUE, FALSE)) {
      info <- paste(caso$valor, "| conservar_perfiles =", conservar)
      r <- .r33p_coleccion_vista(caso$valor, conservar, caso$incompleta)
      textos <- c(.r33p_hojas(unclass(r$pc))$textos, r$avisos, r$impreso)
      # La cifra a mano: el motivo de la vista salio en los tres campos de
      # motivos, en el aviso y, con perfiles, en el perfil de la vista.
      motivos <- c(r$pc$cobertura_coleccion$motivo, r$pc$meta$lecturas$motivo,
                   r$pc$cobertura_metricas$motivo)
      del_motor <- grepl("nvarchar", motivos, fixed = TRUE)
      expect_true(sum(del_motor) >= 2L, info = info)
      expect_true(any(grepl("nvarchar", r$avisos, fixed = TRUE)), info = info)
      if (conservar) {
        expect_true(any(grepl(
          "nvarchar", r$pc$perfiles$vista$resumen_tabla$sql$motivo,
          fixed = TRUE
        )), info = info)
      }
      # Ni el valor ni su resto, en ninguna parte.
      expect_identical(sum(grepl(caso$resto, textos, fixed = TRUE)), 0L,
                       info = info)
      # Y el diagnostico queda: el tipo de la celda y el de destino.
      expect_true(all(grepl(" int ", motivos[del_motor], fixed = TRUE)),
                  info = info)
      expect_true(any(grepl("No se pudo leer la columna: [FreeTDS][SQL Server][literal omitido] nvarchar [literal omitido] N/D [literal omitido] int",
                            r$pc$meta$lecturas$motivo, fixed = TRUE)),
                  info = info)
    }
  }
})

test_that("H4: el mensaje del motor se guarda sin los colores de cli", {
  skip_if_not_installed("rlang")
  # Una consola a color con enlaces, como RStudio: odbc arma su error con cli,
  # que formatea las partes al senalarlo.
  viejas <- options(cli.num_colors = 256L, cli.hyperlink = TRUE)
  on.exit(options(viejas), add = TRUE)
  condicion <- tryCatch(cli::cli_abort(c(
    "!" = "ODBC failed with error 00000 from {.field [FreeTDS][SQL Server]}.",
    x = "Conversion failed when converting the nvarchar value 'N/D' to data type int.",
    i = "From {.file nanodbc/nanodbc.cpp:1802}."
  ), class = "odbc_database_error", call = NULL), error = identity)
  # La cifra a mano: las partes de la condicion traen el byte ESC.
  partes <- unlist(condicion[c("message", "body", "footer")], use.names = FALSE)
  expect_true(any(grepl("\033", partes, fixed = TRUE)))
  leer <- function(proteger) {
    .fijar_proteccion_motor(proteger)
    .mensaje_motor(condicion)
  }
  for (proteger in c(TRUE, FALSE)) {
    salida <- leer(proteger)
    info <- paste("proteger =", proteger)
    expect_false(grepl("\033", salida, fixed = TRUE), info = info)
    expect_false(grepl("file://", salida, fixed = TRUE), info = info)
    # Lo del motor queda como lo dijo: el controlador entero, sin un marcador
    # pegado a sus corchetes.
    expect_true(grepl("from [FreeTDS][SQL Server].", salida, fixed = TRUE),
                info = info)
    expect_false(grepl("[[", salida, fixed = TRUE), info = info)
  }
  # Sin proteccion, el faltante disfrazado se publica: es el diagnostico.
  expect_true(grepl("'N/D'", leer(FALSE), fixed = TRUE))
})
