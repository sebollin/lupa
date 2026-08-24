# Sugerir qué columnas podrían ser la clave

Ordena las columnas que **podrían** identificar una fila, para que quien
conoce la tabla elija. No decide: una columna única puede ser una clave
o puede ser una magnitud que no repite, y esa diferencia no está en los
datos.

## Usage

``` r
sugerir_clave(datos, maximo = 5L, umbral_casi = 0.95)
```

## Arguments

- datos:

  Tabla a examinar.

- maximo:

  Cuántas sugerencias devolver como máximo.

- umbral_casi:

  Proporción de valores distintos a partir de la cual una columna que
  repite se informa igual, como candidata con duplicados.

## Value

Un `data.frame` con una fila por columna candidata, ordenado de más a
menos probable, con las tres señales por separado y el motivo en texto.
Cero filas si ninguna columna se acerca.

## Details

El orden combina tres señales, y las tres se publican para que se pueda
discutir el orden en vez de aceptarlo: si la columna **identifica** cada
fila sin repetir, si **no tiene ausentes** —una clave con ausentes no
identifica—, y cuánto se **parece su nombre** al de una clave
(`id_persona`, `documento`, `expediente`). El nombre se compara sin
acentos ni separadores, así que `ID_Persona` e `idpersona` pesan igual.

Las columnas que repiten valores no se ofrecen como clave de una sola
columna, pero sí se informan cuando estuvieron cerca: una columna que
identifica al 99 % suele ser una clave con duplicados de carga, que es
exactamente lo que conviene mirar.

## See also

[`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
para las combinaciones de varias columnas, y el argumento `clave` de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
para declarar la que se elija.

## Examples

``` r
personas <- data.frame(
  id_persona = 1:4,
  documento = c(11111111, 22222222, 33333333, 33333333),
  edad = c(30, 41, 25, 25)
)
sugerir_clave(personas)
#>      columna identifica sin_faltantes tasa_distintos parecido_nombre
#> 1 id_persona       TRUE          TRUE              1               2
#>                                                           motivo
#> 1 identifica cada fila sin repetir; su nombre es el de una clave
```
