# `cli::cli_warn()`, `cli_abort()` y `cli_inform()` llaman a rlang, que cli solo
# sugiere y el paquete no declara. Instalado lupa con sus dependencias y nada
# mas, una clave repetida en `perfilar()` abortaba con "no hay paquete llamado
# 'rlang'" en vez de avisar, y `requisitos_motor()` con un motor desconocido
# tambien. La guarda es por construccion: ninguna funcion del paquete llama a
# esas tres ni a rlang. Los avisos y errores salen por `.avisar()` y
# `.detener()`, con el texto armado por `cli::format_warning()` y
# `cli::format_error()`, que no necesitan rlang.

.llamadas_que_piden_rlang <- function(funcion) {
  piden <- c("cli_abort", "cli_warn", "cli_inform")
  encontradas <- character()
  recorrer <- function(e) {
    if (is.call(e)) {
      cabeza <- e[[1L]]
      if (is.call(cabeza) && length(cabeza) == 3L &&
          as.character(cabeza[[1L]]) %in% c("::", ":::")) {
        paquete <- as.character(cabeza[[2L]])
        nombre <- as.character(cabeza[[3L]])
        if (identical(paquete, "rlang") ||
            (identical(paquete, "cli") && nombre %in% piden)) {
          encontradas <<- c(encontradas, paste0(paquete, "::", nombre))
        }
      } else if (is.name(cabeza) && as.character(cabeza) %in% piden) {
        encontradas <<- c(encontradas, as.character(cabeza))
      }
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) recorrer(e[[i]])
      }
    }
    invisible()
  }
  recorrer(body(funcion))
  encontradas
}

test_that("la guarda reconoce las llamadas que piden rlang", {
  # Primera mitad: la guarda encuentra lo que busca. Sin esto, un recorrido que
  # no mirara nada pasaria la prueba de abajo.
  expect_identical(
    .llamadas_que_piden_rlang(function() cli::cli_warn("x")), "cli::cli_warn"
  )
  # La llamada a rlang se arma sin escribir `rlang::` en la fuente: el paquete no
  # declara rlang, y R CMD check lee los `::` de las pruebas como dependencias.
  con_rlang <- function() NULL
  body(con_rlang) <- call("if", TRUE, as.call(list(
    call("::", as.name("rlang"), as.name("abort")), "x"
  )))
  expect_identical(.llamadas_que_piden_rlang(con_rlang), "rlang::abort")
  expect_identical(.llamadas_que_piden_rlang(function() cli::format_warning("x")),
                   character())
})

test_that("ninguna funcion del paquete llama a cli_abort, cli_warn, cli_inform ni a rlang", {
  ns <- asNamespace("lupa")
  usan <- character()
  for (nombre in ls(ns, all.names = TRUE)) {
    objeto <- get(nombre, envir = ns)
    if (is.function(objeto) && !is.primitive(objeto) &&
        length(.llamadas_que_piden_rlang(objeto))) {
      usan <- c(usan, nombre)
    }
  }
  expect_identical(usan, character())
})
