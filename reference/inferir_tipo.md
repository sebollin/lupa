# Inferir el tipo implícito de un vector

Para columnas de texto distingue valores lógicos, enteros, dobles,
fechas, fechas-hora, horas, identificadores y texto. Además de la
etiqueta devuelve la proporción de valores compatibles, siempre en la
escala `[0, 1]`. Los textos compuestos únicamente por `0` y `1` se
consideran enteros; para inferir un lógico debe aparecer al menos un
literal alfabético como `sí`, `no`, `true` o `false`.

## Uso

``` r
inferir_tipo(x, umbral = 0.8, muestra = 1e+05)
```

## Argumentos

  - x:
    
    Vector que se desea examinar.

  - umbral:
    
    Proporción mínima para asignar un tipo implícito.

  - muestra:
    
    Máximo de valores que se analizan.

## Valor

Lista de clase `inferencia_tipo` con `tipo`, `proporcion`, conteos,
candidatos evaluados y, cuando corresponde, formatos de fecha.

## Ver también

`detectar_formatos_fecha()`, `descubrir_patrones()`, `perfilar()`

## Ejemplos

``` r
inferir_tipo(c("1", "2", "3"))
#> entero (100.0%; 3 de 3 valores compatibles)
inferir_tipo(c("2020-01-01", "31/12/2020"))
#> fecha (100.0%; 2 de 2 valores compatibles)
```
