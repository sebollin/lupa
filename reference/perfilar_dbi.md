# Perfilar una muestra leida mediante DBI

Calcula en SQL un resumen acotado sobre toda una tabla y, en un bloque
separado, ejecuta
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sobre una muestra traida a memoria. El resumen completo de 99 campos no
se presenta como calculado por la base: esos campos pertenecen
exclusivamente a `perfil_muestra` y su universo es la muestra.

## Usage

``` r
perfilar_dbi(conexion, tabla, muestra = 1000L, orden_muestra = NULL, ...)
```

## Arguments

- conexion:

  Conexion abierta compatible con DBI.

- tabla:

  Nombre de tabla o un objeto aceptado por
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).

- muestra:

  Cantidad positiva y finita de filas solicitadas.

- orden_muestra:

  Columnas para `ORDER BY`. La salida solo declara orden reproducible
  cuando la combinacion es unica en toda la tabla. Sin este argumento,
  DBI no garantiza el orden ni la pertenencia de una muestra limitada, y
  `meta` lo declara expresamente.

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para analizar la muestra.

## Value

Objeto de clase `perfil_dbi` con exactamente dos bloques:
`resumen_tabla`, de alcance completo, y `perfil_muestra`, un objeto
`perfil` cuyo `meta$origen_dbi` declara tabla, conexion, SQL y alcance.

## Details

Esta funcion no escribe en la conexion ni crea objetos temporales. `DBI`
es una dependencia opcional. Cada agregado no disponible queda en `NA` y
su consulta, estado y motivo se conservan en `resumen_tabla$sql`. Las
expresiones se ejecutan como capacidades a comprobar, no como un
dialecto SQL universal: si el motor rechaza una, se registra como no
disponible y las demas metricas siguen siendo independientes.

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
  resultado <- perfilar_dbi(con, "ejemplo", muestra = 5, orden_muestra = "id")
  DBI::dbDisconnect(con)
}
```
