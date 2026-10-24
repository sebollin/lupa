datos_operativos <- data.frame(
  id_registro = c(1:10, 10, 1, 11),
  codigo_usuario = c(
    "USR-001", "USR-002", "USR0003", "USR-004", "MAL-5", "USR-006",
    "USR-007", "USR-008", "USR-009", "USR-010", "USR-010", "USR-001",
    "S/D"
  ),
  fecha_evento = c(
    "2024-01-31", "31/01/2024", "2024/02/15", "16-03-2024",
    "2024.04.17", "20240518", "19/06/2024", "2024-07-20",
    "21/08/2024", "2024-09-22", "23/10/2024", "2024-01-31", "NULL"
  ),
  canal = c(
    "web", "telefono", "Web", "presencial", "S/D", "web", "telefono",
    "web ", "APP", "web", "web", "web", ""
  ),
  monto = c(
    1250, 1600, 1425, -99, 0, 1750, 1810, 1490, 1660, 1700, -50, 1250,
    999999
  ),
  zona = c(
    "Norte", "Centro", "Sur", "Este", "Oeste", "Norte", "Centro", "Sur",
    "Este", "Oeste", "Oeste", "Norte", "Centro"
  ),
  sistema = rep("principal", 13),
  contacto = c(
    "usuario01@example.invalid", "usuario02@example.invalid",
    "usuario03@example.invalid", "usuario04@example.invalid",
    "usuario05@example.invalid", "usuario06@example.invalid",
    "usuario07@example.invalid", "usuario08@example.invalid",
    "usuario09@example.invalid", "usuario10@example.invalid",
    "usuario10b@example.invalid", "usuario01@example.invalid", "contacto-mal"
  ),
  id_copia = c(1:10, 10, 1, 11),
  id_evento = c(sprintf("EVT%03d", 1:10), "EVT012", "EVT001", "EVT011"),
  stringsAsFactors = FALSE
)

save(
  datos_operativos,
  file = "data/datos_operativos.rda",
  compress = "xz",
  version = 2
)
