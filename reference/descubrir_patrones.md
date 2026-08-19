# Descubrir patrones de formato

Generaliza un vector de texto mediante la convención del *Pattern
Finder* de DataCleaner: `9` representa un dígito, `a` una letra
minúscula y `A` una letra mayúscula. Los símbolos y espacios se
conservan literalmente.

## Usage

``` r
descubrir_patrones(
  x,
  distinguir_mayusculas = TRUE,
  expandir = FALSE,
  max_patrones = 20,
  na.rm = TRUE,
  muestra = 1e+05,
  umbral_raro = 0.05
)
```

## Arguments

- x:

  Vector que se convertirá a texto.

- distinguir_mayusculas:

  Si es `TRUE`, distingue `a` de `A`.

- expandir:

  Si es `FALSE`, colapsa tokens repetidos (`9999` a `9+`).

- max_patrones:

  Número máximo de patrones que se muestran.

- na.rm:

  Si es `TRUE`, excluye los valores ausentes.

- muestra:

  Máximo de valores que se analizan.

- umbral_raro:

  Umbral usado para conservar un resumen acotado de patrones raros para
  los hallazgos.

## Value

Un data frame de clase `patrones` con patrón, frecuencia, proporción y
ejemplos. Los atributos `total`, `analizados`, `filas_analizadas` y
`muestreado` describen el posible muestreo; `filas_analizadas` es un
alias explícito de `analizados` para mantener el alcance visible junto a
otros diagnósticos. `resumen_patrones` conserva sólo el patrón dominante
y hasta seis patrones raros para presentacion; nunca guarda la
distribucion completa. `patrones_raros_trazabilidad` conserva solo los
nombres de los patrones raros, hasta 5.000, para que la trazabilidad
pueda enumerar filas sin retener frecuencias ni ejemplos. Las
proporciones siempre estan en `[0, 1]`. `n_patrones_distintos` registra
el total antes de truncar la tabla para informar omisiones sin
retenerla. `n_patrones_raros` y `n_patrones_raros_trazabilidad`
registran cuantos patrones raros habia antes de sus respectivos limites.
`n_filas_patrones_no_dominantes_excluidos` registra cuantas filas
pertenecen a patrones no dominantes cuya proporcion no es rara y que por
eso quedan fuera del hallazgo.

## Details

El cálculo aplica reemplazos vectorizados sobre el vector completo. Si
el vector supera `muestra`, usa una muestra sistemática reproducible y
registra esa decisión en los atributos del resultado.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md),
[`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md)

## Examples

``` r
descubrir_patrones(
  c("2020-01-31", "2021-12-01", "31/01/2020"),
  expandir = TRUE
)
#>       patron n proporcion                ejemplos
#> 1 9999-99-99 2  0.6666667 2020-01-31 | 2021-12-01
#> 2 99/99/9999 1  0.3333333              31/01/2020
```
