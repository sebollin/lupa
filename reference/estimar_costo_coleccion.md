# Estimar el costo de buscar relaciones en una colección

Cuenta cuántas comparaciones de columnas implicaría buscar claves
foráneas entre los pares indicados, **antes** de hacerlas. El número que
importa no es la cantidad de tablas sino la de pares de columnas: dos
tablas de cincuenta columnas son dos mil quinientas comparaciones.

## Usage

``` r
estimar_costo_coleccion(coleccion, pares = NULL, columnas_candidatas = NULL)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- pares:

  Data frame con columnas `tabla_1` y `tabla_2`. Un data frame con cero
  filas es una declaración válida de «ningún par». Sin `pares` —`NULL`—,
  todos los pares dirigidos.

- columnas_candidatas:

  Lista nombrada por identificador de tabla. Cada entrada declara las
  columnas que pueden participar de una relación; las tablas no
  nombradas conservan todas sus columnas. La declaración reduce tanto la
  lectura como el número de comparaciones.

## Value

Lista con `n_tablas`, `n_pares_dirigidos`, `n_pares_declarados`,
`n_pares_estimados`, `n_pares_sin_estimar`, `n_comparaciones_columnas`,
`n_tablas_sin_esquema`, `tablas_sin_esquema` y `alcance`. También
publica `columnas_candidatas` y el alcance real de las comparaciones.

## Details

Sin `pares` estima sobre todos los pares dirigidos de la colección, que
es justamente lo que suele mostrar por qué hay que declararlos. Ese caso
**no materializa los pares**: la suma de `c_i * c_j` sobre los pares
dirigidos es `sum(c)^2 - sum(c^2)`, así que el trabajo es lineal en el
número de tablas y no cuadrático. Estimar el costo no puede costar más
que medirlo.

Sólo se consulta el ancho de las tablas que los pares nombran, por
metadatos
([`DBI::dbListFields()`](https://dbi.r-dbi.org/reference/dbListFields.html)),
no leyendo datos.

**Una tabla cuyo esquema no se puede leer no se suma como cero.** Queda
declarada en `tablas_sin_esquema` y los pares que la involucran se
cuentan en `n_pares_sin_estimar`: el número devuelto cubre
`n_pares_estimados`, no todos los pares.

## See also

[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md),
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = 1:3, dia = 1:3))
  estimar_costo_coleccion(
    coleccion(con, c("personas", "visitas")),
    columnas_candidatas = list(personas = "id", visitas = "persona_id")
  )
  DBI::dbDisconnect(con)
}
```
