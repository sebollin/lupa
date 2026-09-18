.n63_locale_utf8 <- function() {
  candidatos <- c(
    "es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "es_ES.utf8",
    "en_US.UTF-8", "en_US.utf8", "C.UTF-8", "C.utf8"
  )
  originales <- stats::setNames(
    vapply(c("LC_CTYPE", "LC_COLLATE"), Sys.getlocale, character(1L)),
    c("LC_CTYPE", "LC_COLLATE")
  )
  on.exit(for (categoria in names(originales)) {
    suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
  }, add = TRUE)
  for (candidato in candidatos) {
    if (tryCatch({
      .n63_set_locale(candidato)
      TRUE
    }, error = function(e) FALSE)) return(candidato)
  }
  NULL
}

.n63_run <- function(modo, locale, ...) {
  expresion <- paste(
    "source('helper-n63-historico.R');",
    ".n63_child(commandArgs(trailingOnly = TRUE))"
  )
  # `Rscript` a secas esta prohibido bajo `R CMD check` -Writing R Extensions,
  # par. 1.6- y la suite local no lo ve: 56 fallos aparecieron solo en el check.
  salida <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(expresion), "--args", modo, locale, ...),
    stdout = TRUE, stderr = TRUE
  )
  estado <- attr(salida, "status")
  expect_true(
    is.null(estado) || identical(estado, 0L),
    info = paste(salida, collapse = "\n")
  )
  paste(salida, collapse = "\n")
}

test_that("historico cruza locales en dos procesos y conserva el veredicto", {
  locale_utf8 <- .n63_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 disponible")
  # Solo el directorio temporal de la sesion. Escribir en el arbol -aunque sea
  # `../../notas-desarrollo`- deja restos que `R CMD check` denuncia como
  # "non-standard things in the check directory", y ademas crea la carpeta
  # dentro del repositorio del paquete.
  dir <- tempfile("n63-historico-")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  etiqueta <- paste0("n63-historico-", Sys.getpid())
  archivo_u8 <- file.path(dir, paste0(etiqueta, "-u8.rds"))
  archivo_c <- file.path(dir, paste0(etiqueta, "-c.rds"))
  on.exit(unlink(c(archivo_u8, archivo_c)), add = TRUE)

  expect_match(.n63_run("guardar", locale_utf8, archivo_u8), "ok=TRUE")
  u8_u8 <- .n63_run("deriva", locale_utf8, archivo_u8)
  c_u8 <- .n63_run("deriva", "C", archivo_u8)
  expect_match(u8_u8, "delta=0\\.25;direccion=mejora;severidad=ok;avisos=0")
  expect_match(c_u8, "delta=0\\.25;direccion=mejora;severidad=ok;avisos=0")

  expect_match(.n63_run("guardar", "C", archivo_c), "ok=TRUE")
  c_c <- .n63_run("deriva", "C", archivo_c)
  expect_match(c_c, "delta=0\\.25;direccion=mejora;severidad=ok;avisos=0")
  expect_identical(
    sub(";avisos=.*", "", u8_u8), sub(";avisos=.*", "", c_u8)
  )
  expect_identical(
    sub(";avisos=.*", "", c_u8), sub(";avisos=.*", "", c_c)
  )
})

test_that("acumular acepta la propia corrida guardada en ambas direcciones", {
  locale_utf8 <- .n63_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 disponible")
  # Solo el directorio temporal de la sesion. Escribir en el arbol -aunque sea
  # `../../notas-desarrollo`- deja restos que `R CMD check` denuncia como
  # "non-standard things in the check directory", y ademas crea la carpeta
  # dentro del repositorio del paquete.
  dir <- tempfile("n63-historico-")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  etiqueta <- paste0("n63-acumular-", Sys.getpid())
  guardados <- file.path(dir, paste0(etiqueta, c("-u8.rds", "-c.rds")))
  propios <- file.path(dir, paste0(etiqueta, c("-propio-c.rds", "-propio-u8.rds")))
  on.exit(unlink(c(guardados, propios)), add = TRUE)

  expect_match(.n63_run("guardar", locale_utf8, guardados[[1L]]), "ok=TRUE")
  expect_match(
    .n63_run("acumular", "C", guardados[[1L]], propios[[1L]]),
    "ok=TRUE;filas=4;avisos=0"
  )
  expect_match(.n63_run("guardar", "C", guardados[[2L]]), "ok=TRUE")
  expect_match(
    .n63_run("acumular", locale_utf8, guardados[[2L]], propios[[2L]]),
    "ok=TRUE;filas=4;avisos=0"
  )
})

test_that("comparar_perfiles conserva cero filas y cero avisos en ocho cruces", {
  locale_utf8 <- .n63_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 disponible")
  # Solo el directorio temporal de la sesion. Escribir en el arbol -aunque sea
  # `../../notas-desarrollo`- deja restos que `R CMD check` denuncia como
  # "non-standard things in the check directory", y ademas crea la carpeta
  # dentro del repositorio del paquete.
  dir <- tempfile("n63-historico-")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  etiqueta <- paste0("n63-perfiles-", Sys.getpid())
  archivos <- file.path(dir, paste0(etiqueta, c("-u8.rds", "-c.rds")))
  on.exit(unlink(archivos), add = TRUE)
  expect_match(.n63_run("perfil", locale_utf8, archivos[[1L]]), "ok=TRUE")
  expect_match(.n63_run("perfil", "C", archivos[[2L]]), "ok=TRUE")

  for (locale in c(locale_utf8, "C")) {
    for (anterior in archivos) for (actual in archivos) {
      expect_match(
        .n63_run("comparar_perfiles", locale, anterior, actual),
        "filas=0;avisos=0"
      )
    }
  }
})

test_that("la fila de configuracion se compara por bytes, como la de datos", {
  # `8926292` le dio normalizacion por bytes a la comparacion de filas de DATOS
  # y dejo la de filas de CONFIGURACION con `all.equal()` plano. Dos filas con
  # los mismos bytes y distinta marca -lo que pasa cuando una viene de un RDS y
  # la otra se acaba de calcular- se comparaban distintas bajo un locale no
  # UTF-8, y eso aborta toda la acumulacion.
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  originales <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  on.exit(
    for (categoria in categorias) {
      suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
    },
    add = TRUE
  )
  for (categoria in categorias) suppressWarnings(Sys.setlocale(categoria, "C"))
  if (!identical(Sys.getlocale("LC_CTYPE"), "C")) {
    skip("no se pudo fijar LC_CTYPE = C en esta maquina")
  }

  sin_marca <- rawToChar(charToRaw("B\u00e1sico"))
  con_marca <- sin_marca
  Encoding(con_marca) <- "UTF-8"
  expect_identical(charToRaw(sin_marca), charToRaw(con_marca))

  fila <- function(perfil) {
    data.frame(
      id_medicion = "Zebra_2", perfil = perfil,
      configuracion_modelo = rawToChar(charToRaw("modelo_a\u00f1o")),
      stringsAsFactors = FALSE
    )
  }
  cruda <- fila(sin_marca)
  marcada <- fila(con_marca)

  # El caso que hacia fallar la comparacion vieja.
  expect_false(isTRUE(all.equal(cruda, marcada, check.attributes = FALSE)))

  clave_bytes <- getFromNamespace(".clave_bytes", "lupa")
  for (nombre in names(cruda)) {
    cruda[[nombre]] <- clave_bytes(cruda[[nombre]])
    marcada[[nombre]] <- clave_bytes(marcada[[nombre]])
  }
  expect_true(isTRUE(all.equal(cruda, marcada, check.attributes = FALSE)))
})
