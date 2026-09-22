# O03. Dejar de mirar no es lo mismo que arreglar, tampoco cuando cambia el tipo.
#
# `comparar_perfiles()` ya distinguia dos motivos por los que un hallazgo deja
# de aparecer sin haberse resuelto: el diagnostico se declino, o dejo de
# declararse lo que lo habilitaba. Faltaba un tercero. Cuando el plan
# recomendado convierte una columna de texto a numero, los diagnosticos de
# texto dejan de correr sobre ella, y la deriva los declaraba **resueltos, con
# severidad ok**.
#
# Para casi todos eso es cierto: un numero no puede tener espacios sobrantes ni
# mayusculas inconsistentes, el problema dejo de existir. Pero un diagnostico
# que compara valores entre si no desaparece con el tipo: `1200` y `1201`
# siguen ahi despues de convertirlos, solo que nadie los volvio a comparar.

.o03_fechas <- list(
  antes = as.POSIXct("2026-01-01", tz = "UTC"),
  despues = as.POSIXct("2026-01-02", tz = "UTC")
)

.o03_ciclo <- function(datos) {
  antes <- perfilar(datos, analizar_dependencias = FALSE,
                    casi_duplicados_vocabulario = TRUE, fecha = .o03_fechas$antes)
  plan <- planificar_limpieza(antes, datos)
  despues <- perfilar(aplicar(plan, datos)$datos, analizar_dependencias = FALSE,
                      casi_duplicados_vocabulario = TRUE,
                      fecha = .o03_fechas$despues)
  deriva <- as.data.frame(comparar_perfiles(antes, despues))
  deriva[as.character(deriva$aspecto) == "hallazgo", , drop = FALSE]
}

test_that("un diagnostico de relacion que dejo de correr por el tipo no se declara resuelto", {
  # `1200` contra `1201` es un casi duplicado por distancia de edicion, que
  # necesita `stringdist`. Sin el, el hallazgo no aparece en el primer perfil y
  # no hay nada que pueda volverse `no_evaluado`: la premisa no se cumple. Esta
  # prueba lo asumia y fallaba en `R CMD check` con los Suggests ausentes.
  skip_if_not(lupa:::.stringdist_disponible(), "sin 'stringdist' no hay casi duplicados por distancia")
  datos <- data.frame(
    v = c(rep("1200%", 8L), "1201%", rep("3400%", 8L), "3401%", "10%", "20%"),
    id = seq_len(20L), stringsAsFactors = FALSE
  )
  deriva <- .o03_ciclo(datos)
  fila <- deriva[as.character(deriva$valor_anterior) == "casi_duplicados_vocabulario", ,
                 drop = FALSE]
  # Si el hallazgo no aparece en la deriva, la prueba no mide nada: que falle.
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "no_evaluado")
  expect_identical(as.character(fila$severidad), "sospechoso")
  expect_true(grepl("dej\u00f3 de ser texto", as.character(fila$descripcion), fixed = TRUE))
})

test_that("lo que la conversion si resolvio se sigue informando resuelto", {
  # La salvedad no puede comerse las resoluciones reales. `numero_como_texto`
  # desaparece PORQUE se convirtio, y `espacios_sobrantes` porque se recortaron.
  datos <- data.frame(
    v = c(" 1,5", "2,5 ", "3,5", " 4,5 ", "5,5", "6,5"),
    id = seq_len(6L), stringsAsFactors = FALSE
  )
  deriva <- .o03_ciclo(datos)
  for (tipo in c("numero_como_texto", "espacios_sobrantes")) {
    fila <- deriva[as.character(deriva$valor_anterior) == tipo, , drop = FALSE]
    expect_equal(nrow(fila), 1L, info = tipo)
    expect_identical(as.character(fila$cambio), "resuelto", info = tipo)
  }
})

test_that("la direccion opuesta no necesita la salvedad, y se midio", {
  # Una columna que pasa de numero a texto se vuelve a inferir como numerica y
  # `outliers` sigue corriendo: no hay nada que dejo de mirarse.
  antes <- perfilar(data.frame(v = c(10, 11, 12, 10, 11, 12, 10, 11, 9999)),
                    analizar_dependencias = FALSE, fecha = .o03_fechas$antes)
  despues <- perfilar(
    data.frame(v = as.character(c(10, 11, 12, 10, 11, 12, 10, 11, 9999)),
               stringsAsFactors = FALSE),
    analizar_dependencias = FALSE, fecha = .o03_fechas$despues
  )
  deriva <- as.data.frame(comparar_perfiles(antes, despues))
  resueltos <- deriva[as.character(deriva$aspecto) == "hallazgo" &
                        as.character(deriva$cambio) %in% c("resuelto", "no_evaluado"), ,
                      drop = FALSE]
  expect_false("outliers" %in% as.character(resueltos$valor_anterior))
})
