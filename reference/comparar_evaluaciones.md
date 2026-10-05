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

Data frame con resultados anterior y actual, `delta` y `comparacion`. El
delta es el que publica
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
sobre el histórico de las dos evaluaciones, con la misma regla: queda en
`NA` cuando las corridas miden tablas distintas, cambia el marco o los
tipos de resultado, cambia la parte medida de la frontera o un resultado
no se evaluó, y `comparacion` dice por qué con la palabra de la deriva.
El orden lo dan los argumentos, no la fecha: `anterior` es siempre el
primero.

## Examples

``` r
# Ver ejemplos de evaluar().
```
