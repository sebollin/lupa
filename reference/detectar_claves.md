# Detectar claves candidatas

Busca primero claves simples y luego combinaciones mínimas de dos o tres
columnas. No prueba una combinación si ya contiene una clave candidata
más pequeña. Una clave exige ausencia de `NA` y unicidad en todas las
filas.

## Usage

``` r
detectar_claves(datos, max_combinacion = 3, normalizar = NULL, perfil = NULL)
```

## Arguments

- datos:

  Objeto que hereda de `data.frame`.

- max_combinacion:

  Máximo de columnas por combinación, entre 1 y 3.

- normalizar:

  Perfil de comparación. `NULL` hereda el perfil de `perfil`, pero las
  claves se siguen descubriendo por identidad exacta.

- perfil:

  Perfil producido por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para heredar la comparación.

## Value

Data frame de claves candidatas con las columnas combinadas, cantidad de
columnas, marcas de redundancia y las columnas `unicidad_exacta` y
`unicidad_normalizada`. La búsqueda de candidatas usa identidad exacta;
la segunda columna muestra cuántas candidatas también siguen siendo
únicas bajo el perfil de comparación.

## Details

Dos claves simples se marcan como redundantes cuando sus contenidos son
idénticos —incluidas clase, atributos, ausencias y representación
exacta—, aunque tengan nombres distintos. Las columnas matriciales o de
lista no se interpretan como claves. Los pares también quedan en el
atributo `claves_redundantes`.

## See also

[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)

## Examples

``` r
detectar_claves(data.frame(id = 1:4, grupo = c("a", "a", "b", "b")))
#>   columnas n_columnas n_filas redundante equivalente_a unicidad_exacta
#> 1       id          1       4      FALSE                          TRUE
#>   unicidad_normalizada n_distintos_exactos n_distintos_normalizados
#> 1                 TRUE                   4                        4
```
