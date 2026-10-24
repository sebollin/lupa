datos_administrativos <- data.frame(
  id_persona = c(1:10, 10, 1, 11),
  cedula = c(
    "0.000.001-1", "0.000.002-2", "00000033", "0.000.004-4",
    "MAL-5", "0.000.006-6", "0.000.007-7", "0.000.008-8",
    "0.000.009-9", "0.000.010-0", "0.000.010-0", "0.000.001-1",
    "S/D"
  ),
  fecha_nacimiento = c(
    "1980-01-31", "31/01/1981", "1982/02/15", "16-03-1983",
    "1984.04.17", "19850518", "19/06/1986", "1987-07-20",
    "21/08/1988", "1989-09-22", "23/10/1989", "1980-01-31", "NULL"
  ),
  sexo = c("F", "M", "F", "M", "S/D", "F", "M", "F", "M", "F", "M", "F", ""),
  ingreso = c(
    25000, 31000, 28500, -99, 0, 33000, 34500, 29900, 31500,
    32000, -50, 25000, 9999999
  ),
  departamento = c(
    "Montevideo", "Canelones", "Maldonado", "Rocha", "Salto", "Paysandú",
    "Colonia", "Rivera", "Artigas", "Florida", "Florida", "Montevideo",
    "Durazno"
  ),
  pais = rep("UY", 13),
  correo = c(
    "persona01@example.invalid", "persona02@example.invalid",
    "persona03@example.invalid", "persona04@example.invalid",
    "persona05@example.invalid", "persona06@example.invalid",
    "persona07@example.invalid", "persona08@example.invalid",
    "persona09@example.invalid", "persona10@example.invalid",
    "persona10b@example.invalid", "persona01@example.invalid", "correo-mal"
  ),
  id_copia = c(1:10, 10, 1, 11),
  id_tramite = c(
    sprintf("TR%03d", 1:10), "TR012", "TR001", "TR011"
  ),
  stringsAsFactors = FALSE
)

save(
  datos_administrativos,
  file = "data/datos_administrativos.rda",
  compress = "xz",
  version = 2
)
