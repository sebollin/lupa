# Materializar una propuesta de modelo de calidad

Convierte las filas con `incluir == TRUE` en métricas instanciadas y las
reúne mediante `modelo()`. No vuelve a examinar los datos ni ejecuta una
medición.

## Uso

``` r
modelo_desde_propuesta(propuesta)
```

## Argumentos

  - propuesta:
    
    Objeto creado por `proponer_modelo()`.

## Valor

Objeto `modelo_calidad` listo para `medir()`.

## Ver también

`proponer_modelo()`, `modelo()`, `medir()`

## Ejemplos

``` r
datos <- data.frame(x = c(1, NA, 3))
propuesta <- proponer_modelo(perfilar(datos), datos)
modelo_propuesto <- modelo_desde_propuesta(propuesta)
medir(modelo_propuesto, datos)
#>                                       id_medida
#> 1 medicion-20260808T211924.800452-303753-000001
#> 2 medicion-20260808T211924.800452-303753-000002
#> 3 medicion-20260808T211924.800452-303753-000003
#>                              id_medicion               fecha metrica
#> 1 medicion-20260808T211924.800452-303753 2026-08-08 21:19:24  NoNulo
#> 2 medicion-20260808T211924.800452-303753 2026-08-08 21:19:24  NoNulo
#> 3 medicion-20260808T211924.800452-303753 2026-08-08 21:19:24  NoNulo
#>   metrica_especifica metrica_instanciada   dimension   factor      granularidad
#> 1    NoNuloPropuesto     sugerencia-0001 Completitud Densidad instanciaAtributo
#> 2    NoNuloPropuesto     sugerencia-0001 Completitud Densidad instanciaAtributo
#> 3    NoNuloPropuesto     sugerencia-0001 Completitud Densidad instanciaAtributo
#>   tipo_resultado entidad atributo fila objeto_medible resultado agregacion
#> 1       booleano   datos        x    1     datos$x[1]         1       <NA>
#> 2       booleano   datos        x    2     datos$x[2]         0       <NA>
#> 3       booleano   datos        x    3     datos$x[3]         1       <NA>
```
