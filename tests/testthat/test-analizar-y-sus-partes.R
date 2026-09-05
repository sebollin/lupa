# `analizar()` junta el perfil, el modelo, el tablero y el analisis temporal.
# Cada pieza tiene que decir lo mismo que la funcion suelta que la produce, y
# donde difiera a proposito tiene que estar declarado.

test_that("analizar() no publica el codigo deparseado como nombre", {
  # `analizar()` tomaba `nombre = deparse(substitute(datos))`, que sobre una
  # expresion larga devuelve VARIAS lineas. `perfilar()` tiene un saneador para
  # eso, pero vivia DENTRO de su propio cuerpo -era local- y `analizar()` no
  # podia usarlo: publicaba un nombre de tres elementos, y con
  # `[valor protegido]` incrustado, porque la capa de proteccion enmascaraba los
  # numeros del propio nombre.
  resultado <- analizar(data.frame(
    columna_con_nombre_muy_largo_uno = 1:5,
    columna_con_nombre_muy_largo_dos = 6:10,
    columna_con_nombre_muy_largo_tres = 11:15,
    columna_con_nombre_muy_largo_cuatro = 16:20,
    columna_con_nombre_muy_largo_cinco = 21:25
  ))
  expect_length(resultado$perfil$meta$nombre, 1L)
  expect_equal(resultado$perfil$meta$nombre, "datos")

  # Control: un nombre corto y escrito por alguien se conserva tal cual.
  tabla <- data.frame(a = 1:6, b = letters[1:6], stringsAsFactors = FALSE)
  expect_equal(analizar(tabla, nombre = "mi_tabla")$perfil$meta$nombre,
               "mi_tabla")
  expect_equal(analizar(tabla)$perfil$meta$nombre, "tabla")
})

test_that("analizar() acepta una matriz y declara la conversion igual que perfilar()", {
  # `man/perfilar.Rd` declara que una matriz de dos dimensiones se convierte y
  # que la conversion queda en `meta$entrada_convertida`. La puerta de entrada
  # de `analizar()` la rechazaba antes de delegar en su propio motor.
  matriz <- matrix(1:10, ncol = 2)
  colnames(matriz) <- c("a", "b")

  por_analizar <- analizar(matriz, nombre = "m")
  por_perfilar <- perfilar(matriz, analizar_dependencias = FALSE)

  # Primera mitad: la conversion ocurrio de verdad y las dos vias la declaran.
  expect_match(por_perfilar$meta$entrada_convertida, "matriz", fixed = TRUE)
  expect_equal(por_analizar$perfil$meta$entrada_convertida,
               por_perfilar$meta$entrada_convertida)

  # Control: una tabla normal no declara conversion alguna.
  expect_true(is.na(
    analizar(data.frame(a = 1:6, b = letters[1:6]), nombre = "d")$perfil$meta$entrada_convertida
  ))
})

test_that("guardar y volver a leer devuelve lo mismo, salvo lo que se declara", {
  analisis <- analizar(
    data.frame(x = c(1, NA, 3), t = format(as.Date("2024-01-01") + 0:2),
               stringsAsFactors = FALSE),
    nombre = "t"
  )
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  guardar_analisis(analisis, archivo)
  leido <- leer_analisis(archivo)

  # Primera mitad: el objeto tiene piezas que valen la pena comparar.
  expect_true(nrow(analisis$perfil$columnas) >= 2L)

  # Todo lo sustantivo sobrevive la ida y vuelta.
  expect_identical(leido$perfil, analisis$perfil)
  for (componente in setdiff(names(analisis), c("meta", "datos"))) {
    expect_identical(leido[[componente]], analisis[[componente]],
                     info = componente)
  }

  # Y la UNICA diferencia es la que el guardado declara: lo que se guardo y lo
  # que no. Lo que se persiste no es identico a lo que se analizo, y esa
  # diferencia se declara en vez de suponerse.
  difieren <- names(analisis$meta)[
    !vapply(names(analisis$meta),
            function(k) identical(analisis$meta[[k]], leido$meta[[k]]),
            logical(1L))
  ]
  agregados <- setdiff(names(leido$meta), names(analisis$meta))
  expect_equal(union(difieren, agregados), "persistencia")
  expect_named(
    leido$meta$persistencia,
    c("version_esquema", "datos_incluidos", "evidencia_protegida",
      "funciones_sustituidas")
  )
})
