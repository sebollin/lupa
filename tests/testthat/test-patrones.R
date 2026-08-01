test_that("los patrones expandidos siguen la convención documentada", {
  valores <- c(
    "2020-01-31", "31/01/2020", "4.123.456-3", "ABC1234",
    "juan.perez@x.uy"
  )
  esperado <- c(
    "9999-99-99", "99/99/9999", "9.999.999-9", "AAA9999",
    "aaaa.aaaaa@a.aa"
  )
  resultado <- descubrir_patrones(valores, expandir = TRUE)

  expect_setequal(resultado$patron, esperado)
  expect_equal(sum(resultado$n), length(valores))
  expect_equal(sum(resultado$proporcion), 1)
})

test_that("los patrones colapsan repeticiones y pueden ignorar mayúsculas", {
  resultado <- descubrir_patrones(c("Juan", "JUAN"), expandir = FALSE)
  expect_true(all(c("Aa+", "A+") %in% resultado$patron))

  sin_caso <- descubrir_patrones(
    c("Juan", "JUAN"), distinguir_mayusculas = FALSE, expandir = FALSE
  )
  expect_equal(sin_caso$patron, "a+")
  expect_equal(sin_caso$n, 2L)
})

test_that("el muestreo y los ausentes quedan documentados", {
  resultado <- descubrir_patrones(
    c(rep("AB12", 20), NA_character_), muestra = 10, na.rm = FALSE
  )
  expect_true(attr(resultado, "muestreado"))
  expect_equal(attr(resultado, "total"), 21L)
  expect_equal(attr(resultado, "analizados"), 10L)

  sin_na <- descubrir_patrones(c("A1", NA_character_), na.rm = TRUE)
  con_na <- descubrir_patrones(c("A1", NA_character_), na.rm = FALSE)
  expect_equal(sum(sin_na$n), 1L)
  expect_equal(sum(con_na$n), 2L)
  expect_true(anyNA(con_na$patron))
})

test_that("se validan los argumentos de patrones", {
  expect_error(descubrir_patrones(list(1, 2)), "vector atómico")
  expect_error(descubrir_patrones("a", max_patrones = 0), "positivo")
  expect_error(descubrir_patrones("a", muestra = 0), "positivo")
})
