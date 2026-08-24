## Verifica la superficie DBI de `lupa` contra un motor REAL.
##
## Existe para que la fila "probado contra el motor real" de la tabla de motores
## de los README no dependa de que alguien se acuerde de correrlo a mano. Se
## media una vez, quedaba en notas, y la tabla envejecia sin avisar.
##
## Se configura por variables de entorno, asi que el mismo guion sirve en una
## maquina y en integracion continua:
##
##   LUPA_MOTOR      postgres | mariadb | sqlite
##   LUPA_DB_HOST    servidor          (no aplica a sqlite)
##   LUPA_DB_PORT    puerto
##   LUPA_DB_NAME    base
##   LUPA_DB_USER    usuario
##   LUPA_DB_PASS    contrasena
##
## Sale con codigo distinto de cero si alguna comprobacion falla, para que la
## integracion lo marque en rojo.

motor <- Sys.getenv("LUPA_MOTOR", unset = "sqlite")
cat("motor:", motor, "\n")

.paquete <- switch(motor,
  postgres = "RPostgres", mariadb = "RMariaDB", sqlite = "RSQLite",
  stop("LUPA_MOTOR no reconocido: ", motor, call. = FALSE)
)
if (!requireNamespace(.paquete, quietly = TRUE)) {
  cat("SALTEADO:", .paquete, "no esta instalado.\n")
  quit(status = 0)
}
library(lupa)

conectar <- function() {
  switch(motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    postgres = DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
      port = as.integer(Sys.getenv("LUPA_DB_PORT", "5432")),
      dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
      user = Sys.getenv("LUPA_DB_USER", "lupa"),
      password = Sys.getenv("LUPA_DB_PASS", "")
    ),
    mariadb = DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
      port = as.integer(Sys.getenv("LUPA_DB_PORT", "3306")),
      dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
      username = Sys.getenv("LUPA_DB_USER", "lupa"),
      password = Sys.getenv("LUPA_DB_PASS", "")
    )
  )
}

con <- conectar()
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
cat("conectado. clase:", paste(class(con), collapse = ", "), "\n\n")

## ---- La tabla de prueba --------------------------------------------------
##
## Realista a proposito: un identificador, un documento, una categoria, un monto
## que se reparte por ordenes de magnitud, una edad con ausentes y duplicados de
## fila. Cada columna existe para ejercitar un diagnostico distinto.
set.seed(11)
n <- 5000L
datos <- data.frame(
  id_tramite = seq_len(n),
  documento = sample(10000000:49999999, n, replace = TRUE),
  categoria = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  monto = round(stats::rlnorm(n, 9, 1.1), 2),
  edad = c(sample(18:85, n - 20L, replace = TRUE), rep(NA_integer_, 20L)),
  stringsAsFactors = FALSE
)
try(DBI::dbRemoveTable(con, "tramites"), silent = TRUE)
DBI::dbWriteTable(con, "tramites", datos)

fallos <- character()
comprobar <- function(etiqueta, condicion, detalle = "") {
  ok <- isTRUE(condicion)
  cat(sprintf("%-46s %s%s\n", etiqueta, if (ok) "si" else "NO",
              if (nzchar(detalle)) paste0("  (", detalle, ")") else ""))
  if (!ok) fallos <<- c(fallos, etiqueta)
  invisible(ok)
}

perfil <- perfilar_dbi(con, "tramites", muestra = 500L)
resumen <- perfil$resumen_tabla

comprobar("perfila las cinco columnas", nrow(resumen$columnas) == 5L,
          paste(nrow(resumen$columnas), "columnas"))
comprobar("el dialecto se resuelve por sonda",
          !is.null(resumen$meta$dialecto$nombre),
          as.character(resumen$meta$dialecto$nombre))

## Que corra no alcanza: los estadisticos del motor tienen que coincidir con los
## de R sobre la MISMA columna, traida de la propia tabla.
real <- DBI::dbGetQuery(con, "SELECT monto FROM tramites")[[1L]]
fila <- resumen$columnas[resumen$columnas$columna == "monto", , drop = FALSE]
if (nrow(fila) && !is.na(fila$media)) {
  comprobar("la media del motor coincide con la de R",
            isTRUE(all.equal(as.numeric(fila$media), mean(real), tolerance = 1e-8)),
            sprintf("%.6f contra %.6f", as.numeric(fila$media), mean(real)))
} else {
  cat("la media no se calculo en este modo; no se compara\n")
}

## La clave primaria se lee del catalogo cuando el motor la declara.
try(DBI::dbExecute(con, "DROP TABLE padron"), silent = TRUE)
DBI::dbExecute(con, paste(
  "CREATE TABLE padron (id_persona INTEGER PRIMARY KEY,",
  "nombre VARCHAR(60))"
))
## Se pide por nombre en vez de con `:::` directo para que el guion no muera
## cuando corre contra una version instalada que todavia no la trae: eso diria
## "fallo el motor" cuando lo que falta es una funcion.
if (exists(".clave_primaria_dbi", asNamespace("lupa"), inherits = FALSE)) {
  leer_clave <- get(".clave_primaria_dbi", asNamespace("lupa"))
  clave <- leer_clave(con, "padron")
  comprobar("la clave primaria se lee del catalogo",
            identical(tolower(clave$columnas), "id_persona"),
            paste(clave$columnas, collapse = ", "))
} else {
  cat("la lectura de clave primaria no esta en esta version; no se comprueba\n")
}

## Lo que no se pudo medir queda declarado, nunca en cero.
cobertura <- resumen$cobertura
comprobar("la cobertura existe y es una tabla",
          inherits(cobertura, "data.frame"),
          paste(if (is.null(cobertura)) 0 else nrow(cobertura), "filas"))

cat("\n")
if (length(fallos)) {
  cat("FALLARON:", paste(fallos, collapse = "; "), "\n")
  quit(status = 1)
}
cat("Todas las comprobaciones pasaron contra", motor, "\n")
