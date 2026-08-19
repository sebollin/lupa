# Propiedades declaradas de una regla de evaluación

Devuelve, en una tabla, lo que una regla declara: a qué métricas se
engancha, en qué nivel evalúa, qué desenlace produce y **qué umbrales
usa**. Los umbrales viajan aparte de la condición justamente para esto:
encerrados en el *closure* quedaban invisibles y obligaban a escribir
otra regla para mover un número.

## Usage

``` r
propiedades_regla(regla)
```

## Arguments

- regla:

  Objeto creado por
  [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md).

## Value

Data frame con una fila por propiedad: `propiedad`, `valor`.

## Details

Es la contraparte de
[`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
que describe métricas. Un umbral pertenece a una regla, no a una
métrica, así que no cabía allí.

## See also

[`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md),
[`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)

## Examples

``` r
regla <- regla_evaluacion(
  "cobertura minima",
  function(x, minimo) x >= minimo,
  umbrales = list(minimo = 0.9)
)
propiedades_regla(regla)
#>           propiedad            valor
#> 1            nombre cobertura minima
#> 2          metricas             <NA>
#> 3             nivel           medida
#> 4 proporcion_minima             <NA>
#> 5         desenlace             <NA>
#> 6     umbral:minimo              0.9
```
