# Dos puertas publicaban valores crudos porque nunca recibieron la capa de
# proteccion, no porque la capa fallara. Las dos se prueban en las DOS
# direcciones: que dejen de publicar lo que identifica, y que SIGAN publicando lo
# que no. Una guarda que enmascara todo no se distingue de una que no mide.

test_that("detectar_discordancias no publica valores de una columna personal", {
  d <- data.frame(cedula = sprintf("%08d", 77310001:77310025),
                  stringsAsFactors = FALSE)
  d$cedula_bis <- d$cedula
  d$cedula_bis[c(5L, 11L)] <- c("77310999", "77310888")

  dis <- detectar_discordancias(d, senal_redundante(c("cedula", "cedula_bis")))

  # Ni el valor de referencia ni el discordante, y en NINGUNA parte del objeto.
  texto <- paste(
    utils::capture.output(utils::str(dis, max.level = 6, list.len = 400)),
    collapse = " "
  )
  for (valor in c("77310005", "77310011", "77310999", "77310888")) {
    expect_false(grepl(valor, texto, fixed = TRUE))
  }
  # Pero la fila sigue localizada: para eso existe la funcion.
  expect_true(grepl("fila 5", as.character(dis$evidencia), fixed = TRUE))
  expect_true(grepl("fila 11", as.character(dis$evidencia), fixed = TRUE))
  # Y los conteos no se tocan.
  expect_equal(as.numeric(dis$n_discordantes), 2)
})

test_that("detectar_discordancias sigue publicando lo que no es personal", {
  e <- data.frame(
    sucursal = rep(c("norte", "sur", "este", "oeste", "centro"), 5L),
    stringsAsFactors = FALSE
  )
  e$sucursal_bis <- e$sucursal
  e$sucursal_bis[c(3L, 9L)] <- c("nortex", "surx")

  evidencia <- as.character(
    detectar_discordancias(e, senal_redundante(c("sucursal", "sucursal_bis")))$evidencia
  )
  expect_false(grepl("valor protegido", evidencia, fixed = TRUE))
  expect_true(grepl("sucursal=este", evidencia, fixed = TRUE))
  expect_true(grepl("sucursal_bis=nortex", evidencia, fixed = TRUE))
})

test_that("descubrir_patrones suelto enmascara lo que la forma alcanza a clasificar", {
  # Un correo se clasifica por su sola forma, sin nombre de columna que ayude.
  correos <- paste0("persona", sprintf("%02d", 1:25), "@ejemplo.org")
  expect_equal(
    as.character(descubrir_patrones(correos)$ejemplos)[[1L]],
    "[valor protegido]"
  )

  # Un codigo no es personal: se sigue publicando.
  codigos <- sprintf("SKU-%04d", 1:25)
  expect_true(
    grepl("SKU-0001", as.character(descubrir_patrones(codigos)$ejemplos)[[1L]],
          fixed = TRUE)
  )

  # Y un numero de ocho digitos SIN nombre de columna tampoco: el paquete se
  # abstiene a proposito, porque la forma sola no alcanza para un documento y
  # conjeturar por debajo de lo declarado es lo que su regla prohibe.
  numeros <- sprintf("%08d", 77310001:77310025)
  expect_true(
    grepl("77310001", as.character(descubrir_patrones(numeros)$ejemplos)[[1L]],
          fixed = TRUE)
  )
})

test_that("perfilar sigue sin publicar esos valores por su propia via", {
  correos <- paste0("persona", sprintf("%02d", 1:25), "@ejemplo.org")
  # Nombre de columna NEUTRO a proposito: la clasificacion sale de la forma.
  perfil <- perfilar(data.frame(x = correos, stringsAsFactors = FALSE))
  expect_equal(as.character(perfil$columnas$tipo_dato_personal), "correo")
  texto <- paste(
    utils::capture.output(utils::str(perfil, max.level = 8, list.len = 400)),
    collapse = " "
  )
  for (valor in correos[1:5]) expect_false(grepl(valor, texto, fixed = TRUE))
})

test_that("la declaracion del usuario atraviesa las dos puertas", {
  # La regla del paquete: excluye lo que el usuario declara. Si pide no
  # proteger, no se protege — y sin este control una guarda nueva deja de
  # obedecer esa declaracion sin que nadie lo note.
  d <- data.frame(cedula = sprintf("%08d", 77310001:77310025),
                  stringsAsFactors = FALSE)
  d$cedula_bis <- d$cedula
  d$cedula_bis[c(5L, 11L)] <- c("77310999", "77310888")

  abierta <- detectar_discordancias(
    d, senal_redundante(c("cedula", "cedula_bis")),
    proteger_datos_personales = FALSE
  )
  expect_true(grepl("77310005", as.character(abierta$evidencia), fixed = TRUE))

  correos <- paste0("persona", sprintf("%02d", 1:25), "@ejemplo.org")
  ejemplos <- as.character(
    descubrir_patrones(correos, proteger_datos_personales = FALSE)$ejemplos
  )[[1L]]
  expect_true(grepl("persona01@ejemplo.org", ejemplos, fixed = TRUE))
})
