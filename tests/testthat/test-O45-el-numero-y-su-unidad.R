# O45: las tres acciones informativas -revisar cardinalidad, ceros y
# negativos- publicaban como `n_afectadas` el conteo de FILAS de la columna y
# heredaban la unidad de su hallazgo. Sobre una tabla de siete columnas, la
# accion de `alta_cardinalidad` decia "80" con unidad "columna" mientras su
# hallazgo declaraba 1 columna: un par que se contradice dentro de la fila.

.tabla_para_informativas <- function() {
  set.seed(45)
  n <- 80L
  data.frame(
    id = seq_len(n),
    documento = c(rep("1.112.222-3", 4L),
                  sprintf("1.%03d.%03d-%d", 200 + (seq_len(n - 4L)) %/% 1000,
                          (200 + seq_len(n - 4L)) %% 1000,
                          seq_len(n - 4L) %% 10)),
    monto = c(0, 0, -5, stats::rnorm(n - 3L, 500, 80)),
    stringsAsFactors = FALSE
  )
}

test_that("la accion informativa cuenta en la unidad que publica", {
  datos <- .tabla_para_informativas()
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- as.data.frame(planificar_limpieza(perfil, datos = datos))
  hallazgos_tabla <- as.data.frame(hallazgos(perfil))

  informativas <- plan[plan$estrategia %in% c(
    "revisar_cardinalidad", "revisar_ceros", "revisar_negativos"
  ), , drop = FALSE]
  skip_if(nrow(informativas) == 0L,
          "esta tabla no produjo acciones informativas")

  for (i in seq_len(nrow(informativas))) {
    fila <- informativas[i, , drop = FALSE]
    origen <- hallazgos_tabla[
      as.character(hallazgos_tabla$tipo_hallazgo) == fila$hallazgo[[1L]] &
        as.character(hallazgos_tabla$columna) == fila$columna[[1L]], ,
      drop = FALSE
    ]
    expect_equal(nrow(origen), 1L, info = fila$estrategia[[1L]])
    expect_equal(as.character(fila$unidad_conteo[[1L]]),
                 as.character(origen$unidad_conteo[[1L]]),
                 info = fila$estrategia[[1L]])
    expect_equal(fila$n_afectadas[[1L]], origen$n_afectados[[1L]],
                 info = fila$estrategia[[1L]])
  }
})

test_that("una accion que declara su propia unidad conserva su numero", {
  # Control: la regla es sobre las informativas, que no tocan nada. Una accion
  # por celda sigue contando lo que ella tocaria, que puede ser menos que el
  # hallazgo.
  datos <- data.frame(v = c(" a ", " b ", "c"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  plan <- as.data.frame(planificar_limpieza(perfil, datos = datos))
  recorte <- plan[plan$estrategia == "recortar_espacios", , drop = FALSE]

  expect_equal(nrow(recorte), 1L)
  expect_equal(as.character(recorte$unidad_conteo[[1L]]), "fila")
  expect_equal(recorte$n_afectadas[[1L]], 2)
})
