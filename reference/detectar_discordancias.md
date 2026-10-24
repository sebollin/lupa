# Detectar filas donde señales redundantes se contradicen

Recorre las señales declaradas con
[`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
e informa las filas donde las columnas que deberían codificar el mismo
hecho no concuerdan dentro de la ventana declarada.

## Usage

``` r
detectar_discordancias(
  datos,
  senales,
  max_ejemplos = 5L,
  proteger_datos_personales = TRUE,
  columnas_personales = character()
)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- senales:

  Una señal creada por
  [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  o una lista de señales.

- max_ejemplos:

  Máximo de filas concretas que se citan como evidencia: un entero no
  negativo; `0` no cita ninguna.

- proteger_datos_personales:

  Si los valores de las columnas clasificadas como dato personal se
  enmascaran en `evidencia`. `TRUE` por omisión; el número de fila y el
  nombre de la columna se conservan igual. La clasificación se hace aquí
  por léxico y por forma —esta función no recibe un perfil—, así que lo
  que sólo quien conoce los datos sabe personal entra por
  `columnas_personales`.

- columnas_personales:

  Columnas que traen datos personales, declaradas con la misma forma que
  acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  nombres de columna, o un vector con nombre donde el nombre es la
  columna y el valor es el tipo. Sin ella, una columna que el léxico no
  reconoce —un legajo interno— publica sus valores en `evidencia`. Sólo
  tiene efecto con `proteger_datos_personales = TRUE`.

## Value

Data frame con una fila por señal: `senal`, `columnas`, `n_filas`,
`n_evaluadas`, `n_discordantes`, `proporcion`, `ventana` y `evidencia`.

`evidencia` cita filas concretas, y **no publica los valores de una
columna clasificada como dato personal** —salvo que se pida lo contrario
con `proteger_datos_personales = FALSE`—: esos salen como
`[valor protegido]`. El número de fila y el nombre de la columna se
conservan, que es lo que hace falta para ir a corregirla. Las columnas
que no son personales se citan enteras, salvo lo que en ellas repite un
valor de una columna personal de la tabla —una observación que cita la
cédula—: eso se tapa con el mismo piso que aplica
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
aunque la columna personal no esté en la señal.

## Details

El valor de este diagnóstico es que **ninguna columna por separado lo
muestra**: los tres años de una fila pueden ser todos plausibles y aun
así contradecirse entre sí.

Una fila donde alguna de las columnas comparadas está ausente **no se
cuenta como discordante ni como concordante**: se excluye del universo
evaluado, y `n_evaluadas` lo declara. Ausencia no es desacuerdo.

Una columna de la señal que no tiene un valor comparable por fila —una
compuesta (matriz, tabla anidada, valores que no se escriben como
texto), una lista de objetos o de geometrías, o una magnitud con unidad
(`units`), cuyo número sin la unidad no es su valor— no entra: la fila
de la señal sale con `n_evaluadas = 0`, `n_discordantes = NA` y la
evidencia dice qué columna y por qué. Con una `transformacion` que la
lleve a un valor por fila —por ejemplo
[`units::drop_units()`](https://r-quantities.github.io/units/reference/drop_units.html)
después de llevarla a una unidad—, se compara.

Una fila con el mismo valor en todas las columnas concuerda, también si
ese valor es infinito: `Inf` contra `Inf` es coincidencia exacta.

Una fila citada no muestra iguales dos valores que el veredicto separó.
Si la escritura corriente los junta —dos dobles que coinciden en 15
cifras, dos instantes del mismo segundo, dos textos que se ven iguales
con distintos puntos de código—, esa fila los cita con la escritura que
los distingue: el doble con las cifras que lo devuelven a su valor, el
instante con su fracción y el texto con cada carácter fuera del ASCII
imprimible escrito `<U+XXXX>`, con una nota que lo dice.

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
