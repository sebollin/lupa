# Inferir el tipo implícito de un vector

Para columnas de texto distingue valores lógicos, enteros, dobles,
fechas, fechas-hora, horas, identificadores y texto. Además de la
etiqueta devuelve la proporción de valores compatibles, siempre en la
escala `[0, 1]`. Los textos compuestos únicamente por `0` y `1` se
consideran enteros; para inferir un lógico debe aparecer al menos un
literal alfabético como `sí`, `no`, `true` o `false`. Una letra suelta
(`T`, `F`, `S`, `N`) cuenta como literal sólo si aparece también su
opuesto —`T` con `F` o `false`, `S` con `N` o `no`, y al revés—: sola es
un código, y una columna de `F` y `M` no es lógica. Un separador seguido
de exactamente tres dígitos (`1.500`, `1,500`) cuenta como número sólo
si la columna aporta evidencia de una sola convención decimal, con punto
o con coma por igual.

## Usage

``` r
inferir_tipo(x, umbral = 0.8, muestra = 1e+05)
```

## Arguments

- x:

  Vector que se desea examinar.

- umbral:

  Proporción mínima para asignar un tipo implícito.

- muestra:

  Máximo de valores que se analizan.

## Value

Lista de clase `inferencia_tipo` con `tipo`, `proporcion`, conteos,
candidatos evaluados y, cuando corresponde, formatos de fecha.
`n_analizados` y `muestreado` declaran cuántos valores sostienen la
proporción. El resultado público no conserva cachés internos del
perfilado.

## See also

[`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md),
[`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## Examples

``` r
inferir_tipo(c("1", "2", "3"))
#> entero (100.0%; 3 de 3 valores compatibles)
inferir_tipo(c("2020-01-01", "31/12/2020"))
#> fecha (100.0%; 2 de 2 valores compatibles)
```
