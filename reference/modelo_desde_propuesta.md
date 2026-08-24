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

Objeto `modelo_calidad` listo para
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

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
#> 1 medicion-20260824T034615.212608-7445-000001
#> 2 medicion-20260824T034615.212608-7445-000002
#> 3 medicion-20260824T034615.212608-7445-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260824T034615.212608-7445 2026-08-24 03:46:15  NoNulo
#> 2 medicion-20260824T034615.212608-7445 2026-08-24 03:46:15  NoNulo
#> 3 medicion-20260824T034615.212608-7445 2026-08-24 03:46:15  NoNulo
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
