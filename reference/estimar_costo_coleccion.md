# Estimar el costo de buscar relaciones en una colección

Cuenta cuántas comparaciones de columnas implicaría buscar claves
foráneas entre los pares indicados, **antes** de hacerlas. El número que
importa no es la cantidad de tablas sino la de pares de columnas: dos
tablas de cincuenta columnas son dos mil quinientas comparaciones.

## Usage

``` r
estimar_costo_coleccion(coleccion, pares = NULL)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- pares:

  Data frame con columnas `tabla_1` y `tabla_2`. Sin él, todos los pares
  dirigidos.

## Value

Lista con `n_tablas`, `n_pares_dirigidos`, `n_pares_declarados` y,
cuando se pueden leer los esquemas, `n_comparaciones_columnas`.

## Details

Sin `pares` estima sobre todos los pares dirigidos de la colección, que
es justamente lo que suele mostrar por qué hay que declararlos.

## See also

[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md),
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
