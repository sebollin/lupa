# La contrabarra no esta en la gramatica de RFC 3986 y un navegador la lee como
# barra: `https://evil.com\@x.com` va a `evil.com`, y el validador la aceptaba
# con `x.com` de host. Los invisibles de formato pasaban en la ruta y la
# consulta, aunque el host ya los rechazaba.

test_that("la contrabarra hace invalida la URL en cualquier componente", {
  barra <- rawToChar(as.raw(0x5c))
  urls <- c(
    paste0("https://evil.example", barra, "@x.example"),
    paste0("https://x.example/a", barra, "b"),
    paste0("https://x.example?q=a", barra, "b"),
    paste0("https://x.example/#a", barra, "b")
  )
  expect_identical(validar_url(urls), rep(FALSE, 4L))
  expect_false(validar_url(paste0("x.example/a", barra, "b"),
                           esquema_obligatorio = FALSE))
  # Codificada, la contrabarra es un dato de la ruta y la URL es valida.
  expect_true(validar_url("https://x.example/a%5Cb"))
  expect_true(validar_url("https://usuario@x.example/a"))
})

test_that("un invisible de formato en la ruta o la consulta hace invalida la URL", {
  # Con `intToUtf8()`: R no admite una marca de direccion escrita en un literal.
  invisibles <- vapply(c(0x200B, 0xFEFF, 0x200D, 0x200E, 0x202E, 0x2060),
                       intToUtf8, character(1L))
  en_ruta <- paste0("https://x.example/a", invisibles, "b")
  en_consulta <- paste0("https://x.example/?q=a", invisibles, "b")
  expect_identical(validar_url(en_ruta), rep(FALSE, length(invisibles)))
  expect_identical(validar_url(en_consulta), rep(FALSE, length(invisibles)))
  # El control: una letra no ASCII en la ruta sigue siendo valida, y el host
  # Unicode tambien.
  expect_true(validar_url("https://x.example/a\u00f1o"))
  expect_true(validar_url("https://ni\u00f1o.example/"))
})
