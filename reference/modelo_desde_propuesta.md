# Materializar una propuesta de modelo de calidad

Convierte las filas con `incluir == TRUE` en métricas instanciadas y las
reúne mediante
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
No vuelve a examinar los datos ni ejecuta una medición.

## Usage

``` r
modelo_desde_propuesta(propuesta)
```

## Arguments

- propuesta:

  Objeto creado por
  [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md).

## Value

Objeto `modelo_calidad` con las métricas que la propuesta declaró, ya
instanciadas y reunidas por
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
listo para
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md). No
vuelve a examinar los datos ni ejecuta ninguna medición.

## See also

[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md),
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md)

## Examples

``` r
datos <- data.frame(x = c(1, NA, 3))
propuesta <- proponer_modelo(perfilar(datos), datos)
modelo_propuesto <- modelo_desde_propuesta(propuesta)
medir(modelo_propuesto, datos)
#>                                     id_medida
#> 1 medicion-20260903T191848.800512-7367-000001
#> 2 medicion-20260903T191848.800512-7367-000002
#> 3 medicion-20260903T191848.800512-7367-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260903T191848.800512-7367 2026-09-03 19:18:48  NoNulo
#> 2 medicion-20260903T191848.800512-7367 2026-09-03 19:18:48  NoNulo
#> 3 medicion-20260903T191848.800512-7367 2026-09-03 19:18:48  NoNulo
#>   metrica_especifica metrica_instanciada   dimension   factor orientacion
#> 1    NoNuloPropuesto     sugerencia-0001 Completitud Densidad conformidad
#> 2    NoNuloPropuesto     sugerencia-0001 Completitud Densidad conformidad
#> 3    NoNuloPropuesto     sugerencia-0001 Completitud Densidad conformidad
#>        granularidad tipo_resultado entidad atributo fila objeto_medible
#> 1 instanciaAtributo       booleano   datos        x    1     datos$x[1]
#> 2 instanciaAtributo       booleano   datos        x    2     datos$x[2]
#> 3 instanciaAtributo       booleano   datos        x    3     datos$x[3]
#>   resultado agregacion
#> 1         1       <NA>
#> 2         0       <NA>
#> 3         1       <NA>
```
