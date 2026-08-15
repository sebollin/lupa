# Comparar evaluaciones de perfil

Calcula el cambio de `EvaluacionPerfil` entre dos corridas. Cada objeto
debe contener una sola `id_medicion`; no persiste los resultados. Para
una serie de N corridas use
[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
y
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md).

## Usage

``` r
comparar_evaluaciones(anterior, actual)
```

## Arguments

- anterior, actual:

  Objetos creados por
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md).

## Value

Data frame con resultados anterior y actual, y `delta`.

## Examples

``` r
# Ver ejemplos de evaluar().
```
