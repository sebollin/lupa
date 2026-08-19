# Declarar una señal redundante entre columnas

Declara que varias columnas de la misma tabla codifican **el mismo
hecho**, de modo que
[`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md)
pueda informar las filas donde no concuerdan. El grupo lo declara quien
conoce los datos: `lupa` no lo adivina, porque dos columnas de año
pueden ser el año de nacimiento y el año de ingreso y no tienen por qué
coincidir.

## Usage

``` r
senal_redundante(columnas, ventana = 0, transformacion = NULL, nombre = NULL)
```

## Arguments

- columnas:

  Nombres de al menos dos columnas que codifican el mismo hecho.

- ventana:

  Tolerancia máxima admitida entre los valores, en las unidades del
  valor comparado. Por omisión `0`: coincidencia exacta.

- transformacion:

  Lista opcional con nombres de columna y funciones que las llevan a una
  escala comparable.

- nombre:

  Etiqueta de la señal. Por omisión, las columnas unidas.

## Value

Objeto de clase `senal_redundante`.

## Details

`transformacion` permite comparar columnas que guardan el hecho de
formas distintas —por ejemplo extraer el año de una fecha para
compararlo con una columna de año—. Cada función recibe la columna
entera y devuelve un vector de la misma longitud.

`ventana` es la tolerancia, **en las unidades del valor transformado**:
con años, `ventana = 1` acepta un año de diferencia. La unidad no se
adivina; es la del resultado de la transformación.

## See also

[`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)

## Examples

``` r
senal_redundante(c("anio_fiscal", "anio_archivo"))
#> Señal redundante: anio_fiscal = anio_archivo
#> Columnas: anio_fiscal, anio_archivo
#> Ventana: 0
senal_redundante(
  c("fecha", "anio_fiscal"),
  transformacion = list(fecha = function(x) as.integer(format(x, "%Y")))
)
#> Señal redundante: fecha = anio_fiscal
#> Columnas: fecha, anio_fiscal
#> Ventana: 0
#> Transformadas: fecha
```
