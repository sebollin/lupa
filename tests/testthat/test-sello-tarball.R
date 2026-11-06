# El sello de un log lo ata al tarball que se midio: un log sellado sobre otro
# tarball se rechaza. Usa una herramienta de desarrollo que no viaja con el
# paquete; sin ella, la prueba se saltea.
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
  # La salida trae dos tablas, la matriz y la de TIEMPOS, y la fila de
  # `check-depends-only.log` aparece en las dos. Mirar la salida entera dejaba
  # pasar una matriz que no comparaba la huella de fuentes: la que la comparaba
  # era la de TIEMPOS. Se corta antes de esa tabla, y el corte tiene que existir.
  solo_matriz <- function(salida) {
    corte <- which(salida == "--- tabla de TIEMPOS ---")
    expect_length(corte, 1L)
    salida[seq_len(corte[1L] - 1L)]
  }
  matriz_legitima <- solo_matriz(system2(
    "bash", c(matriz, "HEAD", fecha_sonda), stdout = TRUE, stderr = TRUE
  ))
  fila_legitima <- grep("check-depends-only.log", matriz_legitima, value = TRUE)
  expect_length(fila_legitima, 1L)
  expect_true(any(grepl("Status: OK", fila_legitima, fixed = TRUE)))
  expect_false(any(grepl("DOES NOT MATCH COMMIT", fila_legitima, fixed = TRUE)))

  sellado <- readLines(log_matriz)
  sellado_original <- sellado
  sellado[grep("tarball-source-sha256", sellado)] <- paste0(
    "# lupa tarball-source-sha256 ", paste(rep("0", 64L), collapse = "")
  )
  writeLines(sellado, log_matriz)
  matriz_invalida <- solo_matriz(system2(
    "bash", c(matriz, "HEAD", fecha_sonda), stdout = TRUE, stderr = TRUE
  ))
  fila_invalida <- grep("check-depends-only.log", matriz_invalida, value = TRUE)
  expect_length(fila_invalida, 1L)
  expect_true(any(grepl("DOES NOT MATCH COMMIT", fila_invalida, fixed = TRUE)))

  # Las filas de una misma corrida tienen que haber medido el mismo tarball.
  # Un tarball rearmado con los mismos archivos tiene las mismas fuentes (el
  # sello lo acepta) y otro checksum: la segunda fila de la corrida cae.
  writeLines(sellado_original, log_matriz)
  rearmar <- file.path(temporal, "rearmar")
  dir.create(rearmar)
  expect_equal(attr(system2(
    "tar", c("-xzf", tarball_legitimo, "-C", rearmar),
    stdout = TRUE, stderr = TRUE
  ), "status") %||% 0L, 0L)
  tarball_rearmado <- file.path(temporal, "rearmado.tar.gz")
  expect_equal(attr(system2(
    "tar", c("-czf", tarball_rearmado, "-C", rearmar, "lupa"),
    stdout = TRUE, stderr = TRUE
  ), "status") %||% 0L, 0L)
  expect_false(identical(
    unname(tools::md5sum(tarball_rearmado)),
    unname(tools::md5sum(tarball_legitimo))
  ))
  log_motores <- file.path(temporal, "motores.log")
  writeLines("motores medidos: 7 de 7", log_motores)
  sello_motores <- system2(
    "bash", c(script, log_motores, tarball_rearmado, "HEAD"),
    stdout = TRUE, stderr = TRUE
  )
  expect_equal(attr(sello_motores, "status") %||% 0L, 0L)
  expect_true(file.copy(
    log_motores, file.path(directorio_logs, "matriz-motores.log")
  ))
  matriz_dos_tarballs <- solo_matriz(system2(
    "bash", c(matriz, "HEAD", fecha_sonda), stdout = TRUE, stderr = TRUE
  ))
  fila_primera <- grep("check-depends-only.log", matriz_dos_tarballs, value = TRUE)
  fila_segunda <- grep("matriz-motores.log", matriz_dos_tarballs, value = TRUE)
  expect_length(fila_segunda, 1L)
  expect_false(any(grepl("DIFFERENT TARBALL", fila_primera, fixed = TRUE)))
  expect_true(any(grepl("MEASURED A DIFFERENT TARBALL", fila_segunda, fixed = TRUE)))
})
