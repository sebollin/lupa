# Los nombres de columna son DATOS DEL USUARIO y no pueden chocar con los
# argumentos de una funcion interna. `do.call(paste, c(<lista con nombres>,
# sep = "..."))` pasa cada columna como argumento con nombre, asi que una
# columna llamada `sep` o `recycle0` abortaba la corrida entera con un mensaje
# que no nombraba ni la columna ni la causa.
#
# Aparecio de casualidad, escribiendo el fixture de otra prueba con una columna
# llamada `sep`. El patron estaba en cinco lugares y **uno solo** tenia el
# `unname()` que lo evita: la firma de una regla escrita varias veces y
# arreglada en una.

test_that("una columna con nombre de argumento de paste no rompe nada", {
  reservados <- c("sep", "collapse", "recycle0")
  for (nombre in reservados) {
    datos <- data.frame(a = c("x", "x", "y"), stringsAsFactors = FALSE)
    datos[[nombre]] <- c("p", "p", "q")

    expect_error(detectar_duplicados_aproximados(datos), NA, info = nombre)
    expect_error(detectar_claves(datos), NA, info = nombre)
    expect_error(
      perfilar(datos, analizar_dependencias = FALSE), NA, info = nombre
    )
  }
})

test_that("las columnas reservadas se describen como cualquier otra", {
  datos <- data.frame(
    a = c("x", "x", "y"), sep = c("p", "p", "q"), stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  expect_true("sep" %in% perfil$columnas$columna)
  i <- match("sep", perfil$columnas$columna)
  expect_equal(as.numeric(perfil$columnas$n_distintos[[i]]), 2)
})
