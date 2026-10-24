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
  valor comparado. Por omisión `0`: coincidencia exacta. Sólo se aplica
  si todas las columnas comparadas son numéricas; con fechas o texto la
  comparación es de igualdad exacta y se avisa.

- transformacion:

  Lista opcional con nombres de columna y funciones que las llevan a una
  escala comparable.

- nombre:

  Etiqueta de la señal. Por omisión, las columnas unidas.

## Value

Objeto de clase `senal_redundante`: una lista con `nombre` -el
declarado, o las columnas unidas por `=`-, el vector `columnas` que la
señal relaciona, la `ventana` numérica y la `transformacion` validada.
Describe la relación esperada; no la evalúa.

## Details

`transformacion` permite comparar columnas que guardan el hecho de
formas distintas —por ejemplo extraer el año de una fecha para
compararlo con una columna de año—. Cada función recibe la columna
entera y devuelve un vector de la misma longitud.

`ventana` es la tolerancia, **en las unidades del valor transformado**:
con años, `ventana = 1` acepta un año de diferencia. La unidad no se
adivina; es la del resultado de la transformación.

La ventana **sólo gobierna una comparación numérica**. Si alguna de las
columnas comparadas no es numérica —fechas, texto, factores—, la
comparación es de igualdad exacta sobre el texto y la ventana no se
aplica: eso queda dicho en la evidencia (`comparacion textual exacta`)
y, si se declaró una ventana mayor que cero, se avisa al medir. Para
comparar fechas con tolerancia hay que llevarlas con `transformacion` a
un número en la unidad de la ventana.

Esa igualdad exacta es **de valores, no de su impresión**. Una
fecha-hora se compara por su instante: el mismo instante guardado en dos
zonas horarias concuerda. Se escribe en UTC, al segundo y con su
fracción exacta si la tiene —la misma escritura que usa
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)—,
así que comparada con una columna de texto, el texto tiene que estar en
UTC; para otra zona, se la lleva con `transformacion`. Un número se
compara con todas sus cifras —`1e5` y el texto `"100000"` concuerdan;
`0.1 + 0.2` y `0.3` no—. Si todas las columnas son duraciones
(`difftime`), se comparan en segundos: una hora y sesenta minutos
concuerdan. Un texto se compara por sus bytes, así que una palabra con
la tilde compuesta y la misma con la tilde descompuesta son dos textos
distintos.

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
