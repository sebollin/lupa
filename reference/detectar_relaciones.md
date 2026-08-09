# Detectar relaciones entre dos tablas

Examina todos los pares de columnas y describe su cardinalidad a partir
de la unicidad completa de cada lado. La cobertura `tabla1_en_tabla2` es
la proporción de valores no ausentes de la primera columna que existe en
la segunda; la cobertura inversa se informa de forma simétrica. Así se
puede escoger la dirección PK/FK sin imponerla de antemano.

## Uso

``` r
detectar_relaciones(tabla1, tabla2, muestra = 1e+05)
```

## Argumentos

  - tabla1, tabla2:
    
    Objetos que heredan de `data.frame`.

  - muestra:
    
    Máximo de filas del lado verificado que se usan para estimar cada
    cobertura. El muestreo es sistemático y reproducible; el lado de
    referencia no se muestrea. Use `Inf` para calcular todo sin
    muestreo.

## Valor

Data frame con columnas comparadas, cardinalidad, coincidencias y
coberturas de integridad referencial en ambas direcciones. Los atributos
`filas_totales`, `filas_analizadas` y `muestreado` documentan el
muestreo.

## Detalles

Cuando una tabla supera `muestra`, la función estima cada cobertura con
una muestra sistemática del lado que se verifica y conserva completo el
conjunto de referencia. La cardinalidad y la cantidad de valores comunes
siempre se calculan con ambas columnas completas.

## Ver también

`detectar_claves()`, `referencial()`, `proponer_modelo()`

## Ejemplos

``` r
personas <- data.frame(id = 1:3)
tramites <- data.frame(persona_id = c(1, 1, 3, 4))
detectar_relaciones(personas, tramites)
#>   columna_tabla1 columna_tabla2 cardinalidad n_valores_comunes
#> 1             id     persona_id          1:m                 2
#>   cobertura_tabla1_en_tabla2 cobertura_tabla2_en_tabla1
#> 1                  0.6666667                       0.75
```
