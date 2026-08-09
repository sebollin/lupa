# Detectar claves candidatas

Busca primero claves simples y luego combinaciones mínimas de dos o tres
columnas. No prueba una combinación si ya contiene una clave candidata
más pequeña. Una clave exige ausencia de `NA` y unicidad en todas las
filas.

## Uso

``` r
detectar_claves(datos, max_combinacion = 3)
```

## Argumentos

  - datos:
    
    Objeto que hereda de `data.frame`.

  - max\_combinacion:
    
    Máximo de columnas por combinación, entre 1 y 3.

## Valor

Data frame de claves candidatas con las columnas combinadas, cantidad de
columnas y marcas de redundancia.

## Detalles

Dos claves simples se marcan como redundantes cuando sus contenidos son
idénticos —incluidas clase, atributos, ausencias y representación
exacta—, aunque tengan nombres distintos. Las columnas matriciales o
de lista no se interpretan como claves. Los pares también quedan en el
atributo `claves_redundantes`.

## Ver también

`detectar_dependencias()`, `detectar_relaciones()`

## Ejemplos

``` r
detectar_claves(data.frame(id = 1:4, grupo = c("a", "a", "b", "b")))
#>   columnas n_columnas n_filas redundante equivalente_a
#> 1       id          1       4      FALSE              
```
