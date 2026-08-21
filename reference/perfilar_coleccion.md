# Perfilar una colección declarada

Recorre las tablas declaradas en
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
y devuelve **una fila por tabla** con su resumen exacto, más la
cobertura de lo que no se pudo medir.

## Usage

``` r
perfilar_coleccion(
  coleccion,
  muestra = 1000L,
  conservar_perfiles = FALSE,
  cobertura_metricas = c("no_medidas", "completa", "ninguna"),
  tope_cobertura_metricas = 20000L,
  ...
)
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

- cobertura_metricas:

  Qué se sube a `cobertura_metricas`: `"no_medidas"` (por omisión) sólo
  las métricas que no se calcularon, `"completa"` todas con su estado,
  `"ninguna"` para omitirla. Los conteos por tabla de
  `resumen_coleccion` no dependen de esta elección.

- tope_cobertura_metricas:

  Máximo de filas de `cobertura_metricas`. El total real queda siempre
  en `meta$n_metricas_no_medidas`, aunque la tabla se haya recortado.

- ...:

  Argumentos enviados a
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).

## Value

Objeto de clase `perfil_coleccion` con `resumen_coleccion`,
`cobertura_coleccion`, `cobertura_metricas`, `meta` y, si se pidió,
`perfiles`.

## Details

Conserva el resumen calculado en SQL sobre toda la tabla, que es
acotado, y **no** retiene el perfil de la muestra de cada tabla, que es
el objeto pesado: con cientos de tablas eso no entra en memoria.
`conservar_perfiles` permite retenerlos cuando la colección es chica y
se los necesita.

**Lo que no se pudo medir se declara, y nunca a cero.** Una tabla que la
credencial no puede leer, un objeto que no es una tabla base y una tabla
vacía van a `cobertura_coleccion` con su motivo y su `como_resolverlo`.
Un motor que rechaza un agregado —o una columna cuyo tipo no lo admite—
va a `cobertura_metricas`, con una fila por columna y métrica, y deja
además una línea de resumen en `cobertura_coleccion`. Esa cobertura por
métrica **se calcula antes de descartar el perfil**, así que existe
también con `conservar_perfiles = FALSE`. En bases institucionales los
permisos parciales son el caso normal, no el borde.

**Donde no hubo medición va `NA`, no `0`.** Una tabla vacía deja
`prop_faltantes` en `NA` en todas sus columnas: antes eso producía
`prop_faltantes_maxima = -Inf` —fuera del `[0, 1]` que el paquete
promete, y ordenada como la tabla de mejor calidad de la base— y
`n_columnas_sin_faltantes = 0`, que afirmaba tres columnas sin ausencias
sobre una tabla de la que no se sabía nada. `n_columnas_medidas` declara
sobre cuántas columnas se conoce la proporción de ausentes, para que los
dos números anteriores se lean con su alcance.

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
