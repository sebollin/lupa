# Comparar evaluaciones de perfil

Calcula el cambio de `EvaluacionPerfil` entre dos corridas. Cada objeto
debe contener una sola `id_medicion`; no persiste los resultados. Para
una serie de N corridas use `historico_calidad()` y
`detectar_deriva_calidad()`.

## Uso

``` r
comparar_evaluaciones(anterior, actual)
```

## Argumentos

  - anterior, actual:
    
    Objetos creados por `evaluar()`.

## Valor

Data frame con resultados anterior y actual, y `delta`.

## Ejemplos

``` r
# Ver ejemplos de evaluar().
```
