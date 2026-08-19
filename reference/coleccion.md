# Declarar la frontera de una colección

Una colección es una base de datos entera: el séptimo nivel de
granularidad del marco. `lupa` no la descubre recorriendo el catálogo —
**la frontera la declara quien conoce la base**, igual que los pesos de
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
o el marco de
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).
Explorar el catálogo convertiría un error de permisos en un resultado, y
en bases reales de más de mil tablas recorrerlas todas no es viable.

## Usage

``` r
coleccion(conexion, tablas, nombre = NULL)
```

## Arguments

- conexion:

  Conexión abierta compatible con DBI.

- tablas:

  Nombres de las tablas que componen la colección, como vector
  `"esquema.tabla"` o como data frame con columnas `esquema`, `tabla` y
  opcionalmente `tipo`.

- nombre:

  Etiqueta de la colección.

## Value

Objeto de clase `coleccion_lupa`.

## Details

Esta función no consulta nada: sólo declara. Lo que se mide viene
después, con
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).

El **esquema es parte de la identidad de la tabla**. Se puede declarar
como `"esquema.tabla"` o con un data frame de columnas `esquema` y
`tabla`. Una tercera columna `tipo` permite declarar qué es cada objeto
—`"tabla"`, `"vista"`, `"temporal"`—, porque el conteo bruto de un
catálogo mezcla tablas base con vistas, índices y secuencias, y no todas
se perfilan igual.

## See also

[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  coleccion(con, "personas", nombre = "padron")
  DBI::dbDisconnect(con)
}
```
