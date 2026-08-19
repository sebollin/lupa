# Perfilar una colección declarada

Recorre las tablas declaradas en
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
y devuelve **una fila por tabla** con su resumen exacto, más la
cobertura de lo que no se pudo medir.

## Usage

``` r
perfilar_coleccion(coleccion, muestra = 1000L, conservar_perfiles = FALSE, ...)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- muestra:

  Filas solicitadas por tabla para el bloque en memoria.

- conservar_perfiles:

  Si se retienen los objetos `perfil_dbi` completos. Por omisión
  `FALSE`.

- ...:

  Argumentos enviados a
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).

## Value

Objeto de clase `perfil_coleccion` con `resumen_coleccion`,
`cobertura_coleccion`, `meta` y, si se pidió, `perfiles`.

## Details

Conserva el resumen calculado en SQL sobre toda la tabla, que es
acotado, y **no** retiene el perfil de la muestra de cada tabla, que es
el objeto pesado: con cientos de tablas eso no entra en memoria.
`conservar_perfiles` permite retenerlos cuando la colección es chica y
se los necesita.

**Lo que no se pudo medir se declara.** Una tabla que la credencial no
puede leer, un objeto que no es una tabla base, un motor que rechaza un
agregado: cada caso va a `cobertura_coleccion` con su motivo y su
`como_resolverlo`, y **nunca a cero**. En bases institucionales los
permisos parciales son el caso normal, no el borde.

**No hay lectura instantánea.** Perfilar una colección son muchas
consultas y la base puede cambiar entre ellas: una tabla puede truncarse
después de que se contaron sus filas. Por eso cada fila declara el
`momento` en que se midió y `meta$snapshot` declara que no lo hubo.

## See also

[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  perfilar_coleccion(coleccion(con, "personas", nombre = "padron"))
  DBI::dbDisconnect(con)
}
```
