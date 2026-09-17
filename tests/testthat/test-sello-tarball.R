# El nombre de este archivo dice QUE prueba, no de que pendiente salio.
# Se llamaba `test-cola-pendientes.R` y la comprobacion de higiene lo marcaba
# como archivo de notas versionado -busca `PENDIENTES` en los nombres de lo que
# viaja-. La guarda tenia razon en desconfiar: un nombre que evoca la
# contabilidad interna del proyecto no describe una conducta del paquete, y
# ademas viaja en el tarball a CRAN.
test_that("el sello ata el log al tarball medido y rechaza otro", {
  script_disponible <- testthat::test_path(
    "..", "..", "..", "notas-desarrollo", "sellar-log.sh"
  )
  if (!file.exists(script_disponible)) {
    skip("las herramientas de notas no viajan con el paquete instalado")
  }
  script <- file.path(
    "tests", "testthat", "..", "..", "..", "notas-desarrollo",
    "sellar-log.sh"
  )
  if (.Platform$OS.type == "windows") {
    skip("sellar-log.sh requiere un shell POSIX")
  }
  raiz <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  directorio_anterior <- getwd()
  on.exit(setwd(directorio_anterior), add = TRUE)
  setwd(raiz)
  if (!nzchar(Sys.which("git")) || !nzchar(Sys.which("tar")) ||
      !nzchar(Sys.which("bash"))) {
    skip("faltan git, tar o bash para probar el sello")
  }

  temporal <- tempfile("sello-")
  dir.create(temporal)
  tarball_legitimo <- file.path(temporal, "legitimo.tar.gz")
  tarball_distinto <- file.path(temporal, "distinto.tar.gz")
  log <- file.path(temporal, "medicion.log")
  estado <- system2(
    "git", c("archive", "--format=tar.gz", "--prefix=lupa/", "-o",
             tarball_legitimo, "HEAD"), stdout = TRUE, stderr = TRUE
  )
  expect_equal(attr(estado, "status") %||% 0L, 0L)
  writeLines("Status: OK", log)

  sellar <- function(tarball) {
    salida <- system2(
      "bash", c(script, log, tarball, "HEAD"), stdout = TRUE, stderr = TRUE
    )
    list(salida = salida, estado = attr(salida, "status") %||% 0L)
  }
  primero <- sellar(tarball_legitimo)
  expect_equal(primero$estado, 0L)
  expect_length(grep(
    "^# lupa tarball-sha256 [0-9a-f]{64}$", readLines(log)
  ), 1L)
  expect_length(grep(
    "^# lupa tarball-source-sha256 [0-9a-f]{64}$", readLines(log)
  ), 1L)

  extraer <- file.path(temporal, "extraer")
  dir.create(extraer)
  estado_extraer <- system2(
    "tar", c("-xzf", tarball_legitimo, "-C", extraer),
    stdout = TRUE, stderr = TRUE
  )
  expect_equal(attr(estado_extraer, "status") %||% 0L, 0L)
  writeLines(
    c(readLines(file.path(extraer, "lupa", "R", "normalizacion.R")),
      "# tarball distinto para la guarda"),
    file.path(extraer, "lupa", "R", "normalizacion.R")
  )
  estado_empaquetar <- system2(
    "tar", c("-czf", tarball_distinto, "-C", extraer, "lupa"),
    stdout = TRUE, stderr = TRUE
  )
  expect_equal(attr(estado_empaquetar, "status") %||% 0L, 0L)

  segundo <- suppressWarnings(sellar(tarball_distinto))
  expect_equal(segundo$estado, 3L)
  expect_true(any(grepl("RECHAZADO", segundo$salida, fixed = TRUE)))

  matriz <- file.path(
    "tests", "testthat", "..", "..", "..", "notas-desarrollo",
    "matriz-carta.sh"
  )
  if (!file.exists(matriz)) {
    skip("no esta disponible la matriz de las notas")
  }
  directorio_logs <- file.path(temporal, "verificacion")
  dir.create(directorio_logs)
  log_matriz <- file.path(directorio_logs, "check-depends-only.log")
  expect_true(file.copy(log, log_matriz))
  fecha_sonda <- "2099-01-01"
  anterior_dir <- Sys.getenv("LUPA_VERIFICACION_DIR", unset = NA_character_)
  Sys.setenv(LUPA_VERIFICACION_DIR = directorio_logs)
  on.exit({
    if (is.na(anterior_dir)) Sys.unsetenv("LUPA_VERIFICACION_DIR") else
      Sys.setenv(LUPA_VERIFICACION_DIR = anterior_dir)
  }, add = TRUE)
  matriz_legitima <- system2(
    "bash", c(matriz, "HEAD", fecha_sonda), stdout = TRUE, stderr = TRUE
  )
  fila_legitima <- grep("check-depends-only.log", matriz_legitima, value = TRUE)
  expect_true(any(grepl("Status: OK", fila_legitima, fixed = TRUE)))
  expect_false(any(grepl("DOES NOT MATCH COMMIT", fila_legitima, fixed = TRUE)))

  sellado <- readLines(log_matriz)
  sellado[grep("tarball-source-sha256", sellado)] <- paste0(
    "# lupa tarball-source-sha256 ", paste(rep("0", 64L), collapse = "")
  )
  writeLines(sellado, log_matriz)
  matriz_invalida <- system2(
    "bash", c(matriz, "HEAD", fecha_sonda), stdout = TRUE, stderr = TRUE
  )
  fila_invalida <- grep("check-depends-only.log", matriz_invalida, value = TRUE)
  expect_true(any(grepl("DOES NOT MATCH COMMIT", fila_invalida, fixed = TRUE)))
})
