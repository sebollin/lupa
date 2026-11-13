# Inferir el tipo implícito de un vector

Para columnas de texto distingue valores lógicos, enteros, dobles,
fechas, fechas-hora, horas, identificadores y texto. Además de la
etiqueta devuelve la proporción de valores compatibles, siempre en la
escala `[0, 1]`. Los textos compuestos únicamente por `0` y `1` se
consideran enteros; para inferir un lógico debe aparecer al menos un
literal alfabético como `sí`, `no`, `true` o `false`. Una letra suelta
(`T`, `F`, `S`, `N`) cuenta como literal sólo si aparece también su
opuesto —`T` con `F` o `false`, `S` con `N` o `no`, y al revés—: sola es
un código, y una columna de `F` y `M` no es lógica. Los literales se
reconocen sin distinguir mayúsculas —`SÍ` es `sí`— con el pliegue de
caja de
[`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md),
que no depende del locale. Un separador seguido de exactamente tres
dígitos (`1.500`, `1,500`) cuenta como número sólo si la columna aporta
evidencia de una sola convención decimal, con punto o con coma por
igual. Un número con los miles agrupados —`1.234,56`, `1,234.56`,
`12.500.000`— cuenta como número si agrupa los miles de a tres con la
convención de la columna o, cuando la columna aporta evidencia de las
dos, con la que trae el propio valor (`1.234,56` sólo admite la coma
decimal): es como lo lee el resumen de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
Sin evidencia de convención no cuenta, y un primer grupo de miles que
empieza con cero (`099.123.456`, la forma de un teléfono) no es un
número en ninguna. Un separador decimal que abre el número (`.5`, `,5`)
se lee igual que `0.5` o `0,5` y aporta la misma evidencia de
convención; mezclado con la convención contraria, la columna queda
`mixta`. La notación científica (`1e5`, `1.2E-05`, `1,5e3`) es un
número, y no la forma de un identificador, cualquiera sea el signo del
exponente; un exponente que desborda el doble (`1e400`) no lo es. Si la
proporción de números alcanza el umbral y alguno trae decimales, el tipo
inferido es `doble` aunque la mayoría sean enteros: es el tipo que los
cubre a todos. Un número con unidad o moneda (`12 kg`, `USD 35`) no
cuenta como entero ni como doble:
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) lo
informa como número escrito como texto.

## Usage

``` r
inferir_tipo(x, umbral = 0.8, muestra = 1e+05)
```

## Arguments

- x:

  Vector que se desea examinar. Una tabla —también la de una columna que
  da `tibble[, "f"]`— se rechaza: se pasa la columna, `tabla[["f"]]`.

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
