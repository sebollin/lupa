# Catálogo de estadísticos de estimación reconocidos

Los siete estadísticos que
[`medicion_desde_estimaciones()`](https://sebollin.github.io/lupa/reference/medicion_desde_estimaciones.md)
sabe llevar al contrato de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md), con la
métrica a la que corresponden, su tipo, su unidad y su **orientación**:
si un valor alto es conformidad o defecto. Sin la orientación el número
no se puede evaluar, porque un coeficiente de variación de `0,30` y un
tamaño de muestra de `0,30` no se leen igual.

## Usage

``` r
estadisticos_estimacion()
```

## Value

Data frame con `estadistico`, `metrica`, `tipo_resultado`, `orientacion`
y `unidad`.

## See also

[`medicion_desde_estimaciones()`](https://sebollin.github.io/lupa/reference/medicion_desde_estimaciones.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)

## Examples

``` r
estadisticos_estimacion()
#>   estadistico               metrica tipo_resultado orientacion
#> 1        stat            Estimacion           real   no_aplica
#> 2          se         ErrorEstandar           real     defecto
#> 3          cv  CoeficienteVariacion           real     defecto
#> 4           n         TamanoMuestra         entero conformidad
#> 5          df        GradosLibertad           real conformidad
#> 6        deff          EfectoDiseno           real     defecto
#> 7         ess TamanoMuestraEfectivo           real conformidad
#>                    unidad
#> 1 unidad de la estimacion
#> 2 unidad de la estimacion
#> 3              proporcion
#> 4                   casos
#> 5                  grados
#> 6                   razon
#> 7                   casos
```
