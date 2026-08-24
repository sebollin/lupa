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

  Nombres de las tablas que componen la colección: un data frame con
  columnas `esquema`, `tabla` y opcionalmente `tipo`; un
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html) o una lista de
  [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html); o un vector de
  texto `"esquema.tabla"`.

- nombre:

  Etiqueta de la colección.

## Value

Objeto de clase `coleccion_lupa`.

## Details

Esta función no consulta nada: sólo declara. Lo que se mide viene
después, con
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).

El **esquema es parte de la identidad de la tabla**, y el **catálogo
también** cuando existe: `catalogo.esquema.tabla` es el nombre de tres
partes que usan SQL Server y otros motores. Se declara con la columna
`catalogo`, y donde no hay catálogo se omite —o va `NA`— y la identidad
queda igual que siempre. Dos tablas con el mismo nombre y esquema en
catálogos distintos son **dos tablas distintas**, y se cuentan como dos.
Una tercera columna `tipo` permite declarar qué es cada objeto
—`"tabla"`, `"vista"`, `"temporal"`—, porque el conteo bruto de un
catálogo mezcla tablas base con vistas, índices y secuencias, y no todas
se perfilan igual.

## Cómo declarar el nombre

Hay tres formas, y **no son equivalentes**:

- `data.frame(esquema =, tabla =)`:

  **La forma recomendada para cualquier nombre no trivial.** Los nombres
  viajan literales: puntos, espacios y comillas incluidos. No hay parseo
  y por lo tanto no hay nada que se pueda parsear mal.

- [`DBI::Id`](https://dbi.r-dbi.org/reference/Id.html), suelto o en una
  lista:

  La forma canónica de DBI. Tampoco se parsea. Se admiten hasta tres
  componentes —catálogo, esquema y tabla—. Con cuatro o más se rechaza
  **nombrando la causa**: por encima del catálogo no hay un nivel que la
  colección sepa declarar, y devolver ese error como si fuera un
  problema de permisos mandaría a pedir un acceso que ya se tiene.

- Texto `"esquema.tabla"`:

  Atajo cómodo para el caso simple. El texto **se parte en el punto**,
  respetando el entrecomillado del motor: un nombre entrecomillado con
  un punto adentro queda entero, como una sola tabla. Un nombre de tres
  o más partes, un punto al principio o al final, o una comilla sin
  cerrar se **rechazan acá**, con el motivo real. No se aceptan para
  fallar más tarde como si fueran un problema de permisos.

El atajo de texto no puede resolver una ambigüedad genuina:
`"informe.2024"` puede ser la tabla `2024` del esquema `informe` o una
tabla llamada `informe.2024`. `lupa` elige la primera lectura y lo deja
anotado, de modo que si después no encuentra la tabla dice que el nombre
se partió. Para el caso ambiguo use el data frame.

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
  # La forma literal, sin parseo, para nombres con puntos:
  coleccion(con, data.frame(esquema = NA, tabla = "personas"))
  DBI::dbDisconnect(con)
}
```
