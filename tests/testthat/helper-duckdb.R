# El controlador de DuckDB para las pruebas, sin el directorio de extensiones
# que comparte con el usuario (`~/.duckdb`). Sin `shared_home = FALSE`, duckdb
# usa ese directorio si existe y avisa en la consola; en CRAN no existe y usa
# uno temporal, asi que la prueba local corria en otro entorno que la de CRAN.
# `shared_home` es de las versiones recientes de duckdb: una anterior, como la
# del contenedor del minimo de R declarado, no lo acepta (aborta con "... must
# be empty"), y ahi no hay directorio compartido que evitar.
.duckdb_prueba <- function(...) {
  if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    duckdb::duckdb(..., shared_home = FALSE)
  } else {
    duckdb::duckdb(...)
  }
}
