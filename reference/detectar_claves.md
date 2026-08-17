# Detectar claves candidatas

Busca primero claves simples y luego combinaciones mínimas de dos o tres
columnas. No prueba una combinación si ya contiene una clave candidata
más pequeña. Una clave exige ausencia de `NA` y unicidad en todas las
filas. Además informa columnas simples casi-clave cuando al menos el 90
% de sus valores son distintos y un único valor concentra al menos la
mitad de los duplicados excedentes. La concentración evita confundir
texto libre de alta cardinalidad, con muchas colisiones dispersas, con
una clave dañada. Los vectores `double` sólo son candidatos si ninguno
de sus valores finitos tiene parte fraccionaria. Esto conserva
identificadores enteros importados desde archivos de texto y excluye
importes, coordenadas y otras medidas. Los vectores `integer64` se
tratan como enteros semánticos.

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

Data frame de claves candidatas y casi-claves con las columnas
combinadas, cantidad de columnas, marcas de redundancia, `casi_clave`,
`unicidad_exacta` y `unicidad_normalizada`. Las columnas de colisiones
publican sus valores, frecuencias y la concentración observada. Las
claves exactas conservan `casi_clave = FALSE`; una fila con
`casi_clave = TRUE` es un diagnóstico que requiere corregir o confirmar,
no una clave válida.

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
#>   columnas n_columnas n_filas redundante equivalente_a casi_clave
#> 1       id          1       4      FALSE                    FALSE
#>   unicidad_exacta unicidad_normalizada n_distintos_exactos
#> 1            TRUE                 TRUE                   4
#>   n_distintos_normalizados n_valores_colisionados n_filas_en_colision
#> 1                        4                      0                   0
#>   n_duplicados_excedentes concentracion_colisiones colisiones
#> 1                       0                       NA           
```
