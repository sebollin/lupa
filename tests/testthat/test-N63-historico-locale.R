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
  salida <- system2(
    "Rscript",
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
  dir <- file.path("..", "..", "notas-desarrollo", ".trabajo-agente")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  dir <- file.path("..", "..", "notas-desarrollo", ".trabajo-agente")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  dir <- file.path("..", "..", "notas-desarrollo", ".trabajo-agente")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
