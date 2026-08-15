# Comparar dos perfiles y detectar deriva estructural

Compara entregas sin exigir que tengan las mismas columnas. Devuelve
cambios de esquema, tipos, faltantes, cardinalidad, rango, patrones y
hallazgos como un objeto de datos filtrable.

## Usage

``` r
comparar_perfiles(anterior, actual, umbral_cambio = 0.05, umbral_error = 0.2)
```

## Arguments

- anterior, actual:

  Objetos producidos por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- umbral_cambio:

  Diferencia mínima para considerar significativo un cambio de
  proporción, cardinalidad o rango relativo. Cinco puntos porcentuales
  evita elevar variaciones pequeñas a hallazgo.

- umbral_error:

  Diferencia a partir de la cual un aumento de faltantes o un patrón
  nuevo se clasifica como `error`.

## Value

Data frame `deriva_perfil`. `severidad` usa el factor ordenado
`ok < sospechoso < error`; los cambios menores permanecen como filas
`ok` para que la serie sea exportable sin ocultar diferencias.

## Details

Los patrones se comparan sobre el resumen acotado que conserva cada
perfil, no sobre los valores originales ni una distribución completa. Si
las dos corridas usaron configuraciones de patrones diferentes, se
informa un error de comparabilidad y esa parte de la comparación se
omite.

Las columnas que aparecen o desaparecen generan cambios estructurales de
severidad `error`, pero no impiden comparar las columnas compartidas. Un
hallazgo de una columna retirada no se presenta como resuelto.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)

## Examples

``` r
anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
                     fecha = as.POSIXct("2026-01-01", tz = "UTC"))
actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
                   fecha = as.POSIXct("2026-02-01", tz = "UTC"))
comparar_perfiles(anterior, actual)
#>   columna aspecto    cambio severidad valor_anterior valor_actual delta
#> 1   nueva columna aparecida     error           <NA>        nueva    NA
#> 2  codigo  patron aparecido     error           <NA>          A-9   0.5
#>   cambio_relativo significativo fecha_anterior fecha_actual
#> 1              NA          TRUE     2026-01-01   2026-02-01
#> 2              NA          TRUE     2026-01-01   2026-02-01
#>                                                  descripcion
#> 1 Apareció una columna que no estaba en la entrega anterior.
#> 2                       Apareció un patrón de formato nuevo.
#>                                        evidencia
#> 1 No existe una base anterior para sus métricas.
#> 2                      Proporción actual: 0.5000
```
