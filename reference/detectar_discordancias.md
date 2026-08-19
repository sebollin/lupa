# Detectar filas donde señales redundantes se contradicen

Recorre las señales declaradas con
[`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
e informa las filas donde las columnas que deberían codificar el mismo
hecho no concuerdan dentro de la ventana declarada.

## Usage

``` r
detectar_discordancias(datos, senales, max_ejemplos = 5L)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- senales:

  Una señal creada por
  [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  o una lista de señales.

- max_ejemplos:

  Máximo de filas concretas que se citan como evidencia.

## Value

Data frame con una fila por señal: `senal`, `columnas`, `n_filas`,
`n_evaluadas`, `n_discordantes`, `proporcion`, `ventana` y `evidencia`.

## Details

El valor de este diagnóstico es que **ninguna columna por separado lo
muestra**: los tres años de una fila pueden ser todos plausibles y aun
así contradecirse entre sí.

Una fila donde alguna de las columnas comparadas está ausente **no se
cuenta como discordante ni como concordante**: se excluye del universo
evaluado, y `n_evaluadas` lo declara. Ausencia no es desacuerdo.

## See also

[`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)

## Examples

``` r
d <- data.frame(
  anio_fiscal = c(2023L, 2023L, 2022L),
  anio_archivo = c(2023L, 2023L, 2023L)
)
detectar_discordancias(d, senal_redundante(c("anio_fiscal", "anio_archivo")))
#>                        senal                  columnas n_filas n_evaluadas
#> 1 anio_fiscal = anio_archivo anio_fiscal, anio_archivo       3           3
#>   n_discordantes proporcion ventana
#> 1              1  0.3333333       0
#>                                                                                              evidencia
#> 1 fila 3: anio_fiscal=2022; anio_archivo=2023; universo: 3 de 3 filas con todas las columnas presentes
```
